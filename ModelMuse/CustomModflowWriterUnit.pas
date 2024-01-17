{@name is used to define @link(TCustomModflowWriter) which is an abstract
 base class for writing MODFLOW input files.

@author(Richard B. Winston <rbwinst@usgs.gov>)}
unit CustomModflowWriterUnit;

interface

uses ModflowFmpFarmUnit, ModflowFmpClimateUnit, ModflowFmpCropUnit,
  ModflowFmpSoilUnit,
  Mt3dmsChemSpeciesUnit, Vcl.Forms,
  System.SysUtils,
  PestPropertiesUnit,
  Windows, Types,
  LayerStructureUnit,
  ModflowTimeUnit,
  FluxObservationUnit,
  Classes, Contnrs, RbwParser, AbstractGridUnit,
  SubscriptionUnit, GoPhastTypes, DataSetUnit, PhastModelUnit, ModflowCellUnit,
  ModflowOutputControlUnit, ScreenObjectUnit, ModflowBoundaryUnit,
  ModflowPackageSelectionUnit, ModflowTransientListParameterUnit,
  OrderedCollectionUnit, ModflowBoundaryDisplayUnit, ModflowParameterUnit,
  System.Generics.Collections, SparseDataSets, Modflow6ObsUnit,
  Modflow6TimeSeriesCollectionsUnit;

type
  // @name indicates whether the file in the name file is an input file
  // and output file or undetermined.
  TFileOption = (foNone, foInput, foOutput, foInputAlreadyExists);

  TModflowArrayType = (matStructured, matUnstructured, mat1D);

  {@name is used in @link(TCustomModflowWriter.CheckArray)
   @value(cvmGreater If the value in the array is greater than
     CheckValue in @link(TCustomModflowWriter.CheckArray),
     the value in the array is OK.)
   @value(cvmGreaterEqual If the value in the array is greater than or equal to
     CheckValue in @link(TCustomModflowWriter.CheckArray),
     the value in the array is OK.)
   @value(cvmEqual If the value in the array is equal to
     CheckValue in @link(TCustomModflowWriter.CheckArray),
     the value in the array is OK.)
   @value(cvmNotEqual If the value in the array is not equal to
     CheckValue in @link(TCustomModflowWriter.CheckArray),
     the value in the array is OK.)
   @value(cvmLessThan If the value in the array is less than
     CheckValue in @link(TCustomModflowWriter.CheckArray),
     the value in the array is OK.)
   @value(cvmLessThanEqual If the value in the array is less than or equal to
     CheckValue in @link(TCustomModflowWriter.CheckArray),
     the value in the array is OK.)
  }
  TCheckValueMethod = (cvmGreater, cvmGreaterEqual, cvmEqual,
    cvmNotEqual, cvmLessThan, cvmLessThanEqual, cvmGradient);

  {@name is used in @link(TCustomModflowWriter.CheckArray) to specify
    whether cases that don't pass the check should be treated as errors or
    warnings.
    @value(etError If something doesn't pass the check, it should be treated
      as an error.)
    @value(etWarning If something doesn't pass the check, it should be treated
      as a warning.)
      }
  TErrorType = (etError, etWarning);
  TArrayWritingFormat = (awfModflow, awfMt3dms, awfModflow_6);

  TTestRealValueOkProcedure = reference to procedure (Value: double);
  TTestIntValueOkProcedure = reference to procedure (Value: Integer);

  TBoundaryFlowObservationLocation = record
//    FCell: TCellLocation;
    FName: string;
    FBoundName: string;
    FMf6Obs: TModflow6Obs;
  end;

  TBoundaryFlowObservationLocationList = TList<TBoundaryFlowObservationLocation>;

  TCustomFileWriter = class(TObject)
  strict protected
    // name is the file that is created by @classname.
    FFileStream: TFileStream;
    FPestDataArrays: TDictionary<string, TDataArray>;
    // @name is the name of the file being created.
    // It is different from @link(FInputFileName)
    FNameOfFile: string;
    FFileStreamList: TList<TFileStream>;
  private
    // See @link(Model).
    FModel: TCustomModel;
    FWritingTemplate: boolean;
  protected
    FEvaluationType: TEvaluationType;
    FPestParamUsed: Boolean;
    // @name is the name of the file that will eventually be read by the
    // model. It differs from @link(FNameOfFile) in that is the same
    // regardless or whether the file being created is the actual input file
    // or a template file being created for PEST.
    FInputFileName: string;
    FTimeSeriesNames: TStringList;
    // @name closes the file that is being exported.
    // @seealso(OpenFile)
    procedure CloseFile;
    // @name opens the input file to be exported.
    // @seealso(CloseFile)
    procedure OpenFile(const FileName: string);
    // @name generates a comment line for a MODFLOW input file indentifying
    // the file type
    function File_Comment(const FileID: string): string;
    // @name writes a comment line to the output file.
    procedure WriteCommentLine(const Comment: string);
    procedure WriteExitSpec(ExitSpec: TSutraExitSpecificationMethod);
    procedure WriteLimit(Limit: TSutraLimitType);
    // @name returns the extension (including the initial period) for the
    // MODFLOW input file to be exported.
    class function Extension: string; virtual; abstract;
    procedure WriteTemplateFormula(ParameterName: string;
      ModifierValue: double; Method: TPestParamMethod);
    procedure WriteModflowParamFormula(ModflowParameterName: string;
      PestParValue: string; Value: double; ACell: TValueCell);

    procedure WritePestTemplateLine(AFileName: string);
    function GetPestTemplateFormula(Value: double; PestParValue: string;
      PestSeriesValue: string; Method: TPestParamMethod;
      ACell: PCellLocation; const AScreenObject: TObject; var ModifiedValue: double): string;
    function GetPestTemplateFormulaOrValue(Value: double; PestParValue: string;
      PestSeriesValue: string; Method: TPestParamMethod;
      ACell: PCellLocation; const AScreenObject: TObject): string;
    function WritePestTemplateFormula(Value: double; PestParValue: string;
      PestSeriesValue: string; Method: TPestParamMethod;
      ACell: TValueCell; FixedLength: Integer = 0;
      ChangeSign: Boolean = False): double ; overload;
    function WritePestTemplateFormula(Value: double; PestParValue: string;
      PestSeriesValue: string; Method: TPestParamMethod;
      ACell: PCellLocation; AScreenObject: TObject; FixedLength: Integer = 0;
      ChangeSign: Boolean = False): double; overload;
    procedure WritePestTemplateFormulaOrValue(Value: double; PestParValue: string;
      PestSeriesValue: string; Method: TPestParamMethod;
      ACell: PCellLocation; AScreenObject: TObject; FixedLength: Integer = 0;
      ChangeSign: Boolean = False); overload;
    procedure WritePestZones(DataArray: TDataArray; InputFileName: string;
      const DataArrayID, Prefix: string);
    // @name creates a new file whose name is FileName and writes all values
    // written to file to that file until @link(CloseTempFile) is called.
    procedure OpenTempFile(const FileName: string);
    // @name closes the last file created with @link(OpenTempFile) and reverts
    // to writing data to the prior location.
    procedure CloseTempFile;
    property NameOfFile: string read FNameOfFile;
    function GetPestNonTransientTemplateFormula(DataArray: TDataArray;
      Layer, Row, Col: Integer): string;
    function GetPestParamFormula(Value: double; PestParName: string): string;
    procedure WriteTemplateHeader;
    procedure ExtendedTemplateFormula(var Formula: string);
    procedure AssignPestFormula(var Formula: string;
      const PestSeriesName: string; SeriesMethod: TPestParamMethod;
      PestNames: TStringList);
    // Write either the data array value at the specified location
    // or a formula to be evaluated by EnhancedTemplateProcessor
    procedure WriteDataArrayValueOrFormula(DataArray: TDataArray;
      Layer, Row, Col: Integer);
    // Write a formula for EnhancedTemplateProcessor or write a value
    // based on an identified parameter or PEST-modified data set.
    // If Layer < 0, only parameters will be used, not data sets.
    procedure WriteFormulaOrValueBasedOnAPestName(const PestName: string;
      Value: double; Layer, Row, Column: Integer);
    procedure WriteBeginPackageData;
  public
    function GwtFileName(const AFileName: string; SpeciesIndex: Integer): string;
    // @name converts AFileName to use the correct extension for the file.
    class function FileName(const AFileName: string): string;
    {@name is the model to be exported.}
    property Model: TCustomModel read FModel;
    // @name should be set to True when an input file template is being written.
    property WritingTemplate: boolean read FWritingTemplate write FWritingTemplate;
    // the period as the decimal separator.
    class Function FortranDecimal(NumberString : string) : string;
    // @name converts "Value" to an string padded at the beginning with blank
    // characters so that the resulting string as a length of "Width".
    class function FixedFormattedInteger(const Value, Width: integer): string;
    // @name converts "Value" to an string padded at the beginning with blank
    // characters so that the resulting string as a length of "Width".
    class function FixedFormattedReal(const Value : double;
      const Width : integer) : string;
    // @name converts "Value" to a string.  The decimal separator will
    // always be a period.
    class function FreeFormattedReal(const Value : double) : string;
    // @name creates and instance of @classname.
    // @param(Model is the @link(TCustomModel) to be exported.)
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); virtual;
    destructor Destroy; override;
    // @name writes an end of line to the output file.
    procedure NewLine; virtual;
    // @name writes Value to the output with a leading blank space.
    procedure WriteFloat(const Value: double);
    procedure WriteFloatCondensed(const Value: double);
    procedure WriteF10Float(const Value: double);
    procedure WriteF15Float(const Value: double);
    // @name writes Value to the output with a single blank space.
    procedure WriteFreeInteger(Const Value: integer);
    // @name writes Value to the output with a leading blank space(s).
    procedure WriteInteger(Const Value: integer);
    // @name writes Value to the output in I10 format.
    procedure WriteI10Integer(Const Value: integer; const ErrorID: string);
    // @name writes Value to the output in I5 format.
    procedure WriteI5Integer(Const Value: integer; const ErrorID: string);
    // @name writes Value to the output in I2 format.
    procedure WriteI2Integer(Const Value: integer; const ErrorID: string);
    // @name writes Value to the output with NO leading blank space.
    procedure WriteString(const Value: String); overload;
    procedure WriteString(const Value: AnsiString); overload; virtual;
    class function PestUtilityProgramPath(UtilityProgramName,
      AFileName: string): string;
    procedure AddUsedPestDataArray(ADataArray: TDataArray);
    procedure ClearUsedPestDataArrays;
    function GetUsedPestDataArrays: TArray<TDataArray>;
    // Name returns an empty string if no file needs to be created.
    // otherwise, it returns the name of the arrays file to be used
    // in the PEST template for a boundary condition.
    function WriteArraysFile(TemplateFileName: string): string;
    { @name writes one layer of DataArray to the output file.
      @param(DataArray is the TDataArray to be written.)
      @param(LayerIndex is the layer in DataArray to be written.)
      @param(ArrayName is written as a comment
        identifying the data set being written.)
    }
    procedure WriteArray(const DataArray: TDataArray; const LayerIndex: integer;
      const Comment: string;  NoValueAssignedAnnotation: string;
      const MF6_Arrayname: string; CacheArray: boolean = True;
      ShouldWriteHeader: Boolean = True; ForceFullArray: Boolean = False); virtual;
    {
    @name returns whether or not PEST parameters are used with DataArray in the
    layer designated by LayerIndex.
    If the result is @True, @link(WriteArray) will handle the data set
    differently
    }
    function DataArrayUsesPestParameters(const DataArray: TDataArray): boolean;
    property TimeSeriesNames: TStringList read FTimeSeriesNames;
end;

  { @name is an abstract base class used as an ancestor for classes that
    write MODFLOW input files.
  }
  TCustomModflowWriter = class(TCustomFileWriter)
  private
    // @name writes a header for DataArray using either
    // @link(WriteConstantU2DINT) or @link(WriteU2DRELHeader).
    procedure WriteHeader(const DataArray: TDataArray;
      const Comment: string; const MF6_ArrayName: string); overload;
    procedure WriteHeader(const DataType: TRbwDataType;
      const Comment: string; const MF6_ArrayName: string); overload;
    function CheckArrayUniform(const LayerIndex: Integer;
      const DataArray: TDataArray): boolean;  overload;
    function CheckArrayUniform(const LayerIndex: Integer;
      const DataArray: TDataArray; var IntValue: Integer;
      var BoolValue: Boolean; var RealValue: Double): boolean; overload;
    procedure WriteArrayValues(const LayerIndex: Integer;
      const DataArray: TDataArray);
  protected
    FArrayWritingFormat: TArrayWritingFormat;
    // @name generates a comment line for a MODFLOW input file indentifying
    // the package.
    function PackageID_Comment(APackage: TModflowPackageSelection): string; virtual;
    // @name writes Lines as comments.
    procedure WriteCommentLines(const Lines: TStrings);
    // @name returns the proper IPRN for specifying whether to print an
    // integer input array
    function IPRN_Integer: integer;
    function IPRN_Mt3dms_Integer: integer;
    // @name returns the proper IPRN for specifying whether to print an
    // real number input array
    function IPRN_Real: integer;
    function IPRN_Mt3dms_Real: integer;
    // @name retrieves the proper unit number for the cell-by-cell flow
    // file.
    procedure GetFlowUnitNumber(var UnitNumber: Integer); virtual;
    // @name writes a U2DREL array based on the contents of List.
    //  List is sometimes a @link(TValueCellList)
    // or a list of list of @link(TValueCellList)
    // or a TList.
    // DataTypeIndex indicates which parameter of the @link(TValueCell)
    // to use.
    // DataType indicates what sort of information in the TList.
    // DefaultValue indicates the proper default parameter for the gridded data.
    // If FreeArray is @true, TransientArray will be set to nil.
    // If FreeArray is @false, TransientArray will be set
    // to the @link(TDataArray) that was exported and it will be the caller's
    // responsibility to free TransientArray
    procedure WriteTransient2DArray(const Comment: string;
      DataTypeIndex: Integer; DataType: TRbwDataType; DefaultValue: Double;
      List: TList; AssignmentMethod: TUpdateMethod; AdjustForLGR: boolean;
      var TransientArray: TDataArray; const MF6_ArrayName: string;
      FreeArray: boolean = True; ShouldWriteHeader: Boolean = True;
      ForceFullArray: Boolean = False);
    procedure WriteMf6_DataSet(DataArray: TDataArray; const ID: string);
    // SAVE_FLOWS
    procedure WriteSaveFlowsOption;
    procedure WriteBeginOptions;
    procedure WriteEndOptions;
    procedure PrintListInputOption;
    function ShouldCheckCell(LayerIndex, RowIndex, ColIndex: integer)
      : boolean; virtual;
    procedure WriteBeginDimensions;
    procedure WriteEndDimensions;
    // @name is used to warn the user that an array has not been specified.
    // It is overriden in the RCH, EVT, and ETS packages.
    procedure HandleMissingArrayData; virtual;
    procedure WriteBeginPeriod(StressPeriodIndex: integer);
    procedure WriteEndPeriod;
    procedure WriteBeginGridData;
    procedure WriteEndGridData;

    // Write a value or a formula for EnhancedTemplateProcessor for a
    // boundary condition cell. Index is used to access the appropiate
    // value, PEST name, PEST Series name and PEST series method.
    // The result is the value that will be written to the file as
    // modified by applicable the PEST variables.
    // the result will be zero if a MODFLOW 6 time series name will be
    // written.
    function WriteValueOrFormula(Cell: TValueCell; Index: integer;
      FixedLength: Integer = 0; ChangeSign: Boolean = False): double;

    procedure WritePestFormulaOrValue(const PestName, PestSeriesName: string;
      PestMethod: TPestParamMethod; Value: double);
    procedure WriteEndPackageData;
  public
    // @name converts a real number represented as a string to always use
    property ArrayWritingFormat: TArrayWritingFormat read FArrayWritingFormat;
    {@name checks that the values stored in DataArray are valid.
    @name returns @true if no error is found.
    @param(DataArray is the @link(TDataArray) to be checked.)
    @param(LayerIndex is the layer (first index) in DataArray to be checked.)
    @param(ErrorOrWarningMessage is the message to be displayed to the
      user if the check fails.)
    @param(CheckMethod indicates how the data in DataArray will be checked.)
    @param(CheckValue is the value against which the data in DataArray
      will be checked.)
    @param(ErrorType indicates whether problems discovered in @name should be
      considered errors or warnings.)
    }
    function CheckArray(const DataArray: TDataArray; const LayerIndex: integer;
      const ErrorOrWarningMessage: string; CheckMethod: TCheckValueMethod;
      CheckValue: double; ErrorType: TErrorType): boolean;
    // @name creates and instance of @classname.
    // @param(Model is the @link(TCustomModel) to be exported.)
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    { @name writes one layer of DataArray to the output file.
      @param(DataArray is the TDataArray to be written.)
      @param(LayerIndex is the layer in DataArray to be written.)
      @param(ArrayName is written as a comment
        identifying the data set being written.)
    }
    procedure WriteArray(const DataArray: TDataArray; const LayerIndex: integer;
      const Comment: string;  NoValueAssignedAnnotation: string;
      const MF6_Arrayname: string; CacheArray: Boolean = True;
      ShouldWriteHeader: Boolean = True; ForceFullArray: Boolean = False); override;
    // @name writes value to the output file using the U2DINT format in MODFLOW
    // or the IARRAY array reader in MT3DMS depending on the value of
    // @link(FArrayWritingFormat)
    // for cases when the array is a constant.
    procedure WriteConstantU2DINT(const Comment: string;
      const Value: integer; ArrayType: TModflowArrayType;
      const MF6_ArrayName: string); virtual;
    // @name writes value to the output file using the U2DREL format in MODFLOW
    // or the RARRAY array reader in MT3DMS depending on the value of
    // @link(FArrayWritingFormat)
    // for cases when the array is a constant.
    procedure WriteConstantU2DREL(const Comment: string;
      const Value: double; ArrayType: TModflowArrayType;
      const MF6_ArrayName: string); virtual;
    // @name writes DataArray in cross section format.
    procedure WriteCrossSectionArray(const DataArray: TDataArray;
      const Comment: string; const MF6_ArrayName: string);
    // @name writes the IFACE parameter in MODFLOW.
    procedure WriteIface(const Value: TIface);
    // @name writes the header for the U2DINT array reader in MODFLOW
    // or the IARRAY array reader in MT3DMS depending on the value of
    // @link(FArrayWritingFormat).
    // @param(Comment is used to identify the array being written.)
    Procedure WriteU2DINTHeader(const Comment: string;
      ArrayType: TModflowArrayType; const MF6_ArrayName: string); virtual;
    // @name writes the header for the U2DINT array reader in MODFLOW
    // or the RARRAY array reader in MT3DMS depending on the value of
    // @link(FArrayWritingFormat).
    // @param(Comment is used to identify the array being written.)
    Procedure WriteU2DRELHeader(const Comment: string;
      ArrayType: TModflowArrayType; const MF6_ArrayName: string); virtual;
    // @name writes a line to the name file.
    class procedure WriteToNameFile(const Ftype: string;
      const UnitNumber: integer; FileName: string;
      const Option: TFileOption; AModel: TCustomModel;
      RelativeFileName: boolean = False; PackageName: String  = ''); overload;
    class procedure WriteToNameFile(const Ftype: string;
      UnitNumber: integer; FileName: string;
      const Option: TFileOption; OutputSuppression: TOutputSuppression;
      AModel: TCustomModel;
      RelativeFileName: boolean = False; PackageName: String = ''); overload;
    procedure WriteToGwtNameFile(const Ftype: string; FileName: string;
      SpeciesIndex: Integer; PackageName: string = '');
    class procedure WriteToMt3dMsNameFile(const Ftype: string;
      const UnitNumber: integer; FileName: string; FileOption: TFileOption;
      AModel: TCustomModel; RelativeFileName: boolean = False; Option: String = '');
    // @name adds a comment line to the name file.
    class procedure AddNameFileComment(const Comment: string);
  end;

  // @name is an abstract base class used to generate input for
  // MODFLOW for a specific package.
  TCustomPackageWriter = class(TCustomModflowWriter)
  private
    FWarningRoot: string;
    // @name must be nil or TModflowMvrWriter
    FMvrWriter: TObject;
    procedure SetMvrWriter(const Value: TObject);
  protected
    FTimeSeriesFileNames: TStringList;
    procedure SetTimeListUpToDate(List: TModflowBoundaryDisplayTimeList);
    procedure SetTimeListsUpToDate(TimeLists: TModflowBoundListOfTimeLists); overload;
    procedure SetTimeListsUpToDate(TimeLists: TList<TModflowBoundListOfTimeLists>); overload;
    // @name identifies the package that is being exported.
    function Package: TModflowPackageSelection; virtual; abstract;
    // @name writes the comments for the current package.
    // @name also writes a comment identifying the package.
    procedure WriteDataSet0;
    // @name prepares the lists in TimeLists for display when the
    // lists in question are not used.
    procedure UpdateNotUsedDisplay(TimeLists: TModflowBoundListOfTimeLists); overload;
    procedure UpdateNotUsedDisplay(TimeLists: TList<TModflowBoundListOfTimeLists>); overload;
    // @name updates the @link(TDataArray)s in DataArrayList
    // using the cells @link(TValueCellList CellList).
    // @param(ParameterIndicies indicates which properties of the cells are
    // affected by MODFLOW parameters.)
    // @param(Param is a MODFLOW parameter that might affect the value of
    // some cells.)
    // @Seealso(TCustomTransientWriter.UpdateTransient2DArray)
    procedure UpdateCellDisplay(CellList: TValueCellList; DataArrayList: TList;
      ParameterIndicies: TByteSet; Param: TModflowTransientListParameter = nil;
      UsedIndicies: TByteSet = []);
    // @name adds spaces to Source until the result is at least as long
    // as ALength.
    function ExpandString(Source: string; ALength: integer): string;
    // @name is used to define parameter clusters for Param.
    //
    // When @name is called, Param must be a parameter for which the clusters
    // need to be identified and LayerCount must be greater than or equal
    // to the maximum number of clusters that could be defined for the
    // parameter.
    //
    // After @name is called:
    // NCLU will be equal to the number of layer
    // clusters needed to be associated with Param.
    //
    // Clusters will be an
    // array with length equal to the LayerCount.  The first NCLU
    // items in Clusters will be the MODFLOW layers for which a cluster
    // will need to be defined.
    //
    // UniformLayers will be an
    // array with length equal to the LayerCount.  The first NCLU
    // items in UniformLayers will indicate whether or not the cluster
    // associated with that layer will be uniform.
    //
    // The remaining items in Clusters and UniformLayers are undefined.
    procedure IdentifyZoneClusters(var NCLU: Integer;
      var Clusters: TOneDIntegerArray; var UniformLayers: TBooleanDynArray;
      LayerCount: Integer; Param: TModflowSteadyParameter);
    Function UcodeObsNameOK(Const AName: string): boolean;
    Function PestObsNameOK(Const AName: string): boolean;
    procedure CheckCell(ValueCell: TValueCell;
      const PackageName: string); virtual;
    procedure RemoveNoDefinedError(var NoDefinedErrorRoot: string); virtual;
    procedure ShowNoBoundaryError(const NoDefinedErrorRoot: string); virtual;
    function AquiferConductance(Layer, Row, Column: Integer): double;
    function AquiferKx(Layer, Row, Column: integer): double;
    // PRINT_INPUT, PRINT_FLOWS, and SAVE_FLOWS
    procedure PrintOutputOptions;
    procedure PrintFlowsOption;
    procedure WriteBeginConnectionData;
    procedure WriteEndConnectionData;
    procedure WriteBoundNamesOption;
    procedure WriteTimeSeriesFiles(InputFileName: string);
    procedure PrintConcentrationOption;
//    procedure WriteGwtlAuxVariables;
  public
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    property MvrWriter: TObject read FMvrWriter write SetMvrWriter;
  end;

  TCustomSolverWriter = class(TCustomPackageWriter)
  public
    class function SolverFileGeneratedExternally(Model: TCustomModel): boolean;
  end;

  TCustomFlowPackageWriter = class(TCustomPackageWriter)
  protected
    // return Kx or transmissivity in X direction
    function XConnection(LayerIndex: Integer): TDataArray; virtual;
    // return Ky or transmissivity in Y direction
    function YConnection(LayerIndex: Integer): TDataArray; virtual;
    // return Kz or conductance in Z direction
    function ZConnection(LayerIndex: Integer): TDataArray; virtual;
    procedure CheckSpecifiedHeadsConnected;
  public
    class function FlowPackageFileGeneratedExternally(Model: TCustomModel): boolean;
  end;

  // @name is used to export input files for MODFLOW for packages with
  // time-varying data.
  TCustomTransientWriter = class(TCustomPackageWriter)
  private
    // After @link(Evaluate) is called,
    // @name contains a series of @link(TValueCellList)s;
    // one for each stress period.
    // Each such list contains series of @link(TValueCell)s. Each
    // @link(TValueCell) defines one boundary cell for one stress period.
    // @name is a TObjectList.
    FValues: TList;
    FFlowObsLocations: TBoundaryFlowObservationLocationList;
    FObsLocationCheck: T3DSparseStringArray;
//    FMf6ObsArray: T3DSparsePointerArray;
    FToMvrFlowObsLocations: TBoundaryFlowObservationLocationList;
    FWellReductionFlowObsLocations: TBoundaryFlowObservationLocationList;
    FDirectObsLines: TStrings;
    FFileNameLines: TStrings;
    FCalculatedObsLines: TStrings;
    function GetOwnsValueContents: Boolean;
    procedure SetOwnsValueContents(const Value: Boolean);
  protected
    MAXBOUND: integer;
    // @name is used for recording the locations of observations for
    // MODFLOW-6 flow observations.
    property FlowObsLocations: TBoundaryFlowObservationLocationList
      read FFlowObsLocations;
    property ToMvrFlowObsLocations: TBoundaryFlowObservationLocationList
      read FToMvrFlowObsLocations;
    property WellReductionFlowObsLocations: TBoundaryFlowObservationLocationList
      read FWellReductionFlowObsLocations;
    // @name is used for recording the locations of observations for
    // MODFLOW-6 flow observations.
    property ObsLocationCheck: T3DSparseStringArray read FObsLocationCheck;
//    property Mf6ObsArray: T3DSparsePointerArray read FMf6ObsArray;
    // @name reads the data from the model and processes it into a form
    // where it can be readily exported to the MODFLOW input file.
    procedure Evaluate; virtual; abstract;
    // @name stores the values at a list of cells.
    property Values: TList read FValues;
    property OwnsValueContents: Boolean read GetOwnsValueContents
      write SetOwnsValueContents;

    {@name updates @link(TModflowBoundaryDisplayDataArray DisplayArray)
    with the contents of List.
    List can either contain @link(TDataArray)s,
    lists of @link(TDataArray)s, or lists of lists of @link(TDataArray)s.
    @Seealso(UpdateCellDisplay)}
    procedure UpdateTransient2DArray(
      DisplayArray: TModflowBoundaryDisplayDataArray; List: TList);

    {@name
    @unorderedList(
      @item(assigns DefaultValue to every location in
      @link(TModflowBoundaryDisplayDataArray DisplayArray),)
      @item(assigns @link(StrNoValueAssigned) to the corresponding annotation,)
      @item(assigns @link(TModflowBoundaryDisplayDataArray.CellCount)
        a value of zero.)
    )}
    procedure InitializeDisplayArray(
      DisplayArray: TModflowBoundaryDisplayDataArray; DefaultValue: Double);

    {@name uses the contents of  List to assign values to
    @link(TModflowBoundaryDisplayDataArray DisplayArray).
     @param(List can either contain @link(TValueCellList)s or
     lists of @link(TValueCellList)s.)
     DefaultValue is the value to be assigned if no member of List has
     a value.
     @param(DataType indicates whether @link(TValueCell.IntegerValue)
     or @link(TValueCell.RealValue) should be accessed.)
     @param(DataTypeIndex indicates which @link(TValueCell.IntegerValue)
     or @link(TValueCell.RealValue) should be accessed.)
     @param(UpdateMethod indicates whether successive values from @link(TValueCell)
     should replace or be added to existing values.)}
    procedure AssignTransient2DArray(
      DisplayArray: TModflowBoundaryDisplayDataArray; DataTypeIndex: Integer;
      List: TList; DefaultValue: double; DataType: TRbwDataType;
      UpdateMethod: TUpdateMethod);
    // @name counts the maximum number of cells used in any stress period. This
    // value is returned in MaximumNumberOfCells.
    procedure CountCells(var MaximumNumberOfCells: integer); virtual;
    procedure GetITMP(var ITMP: integer; TimeIndex: integer;
      var List: TValueCellList); virtual;
    procedure WriteDimensionsMF6;
    // @name indicates that a particular object defines a flow observation
    // in a descendant class.
    function IsMf6Observation(AScreenObject: TScreenObject): Boolean; virtual;
    function IsMf6ToMvrObservation(AScreenObject: TScreenObject): Boolean; virtual;
    function IsMf6ToWellReductionObservation(AScreenObject: TScreenObject): Boolean; virtual;
    function IsFlowObs(AScreenObject: TScreenObject): Boolean; virtual;
    procedure EnsureMf6CalibObservations(AScreenObject: TScreenObject); virtual;
//    function ObsFactors: TObservationFactors; virtual;
    // @name indicates that flow observations are used in a descendant class.
    function Mf6ObservationsUsed: Boolean; virtual;
    // @name is the file extension used for the observation input file.
    class function ObservationExtension: string; virtual;
    // @name returns the name of the Observation input file.
    function ObservationFileName(AFileName: string): string;
    procedure WriteModflow6FlowObs(FileName: string;
      EvaluationType: TEvaluationType);
    procedure WriteModflow6GwtFlowObs(FileName: string;
      EvaluationType: TEvaluationType; SpeciesIndex: integer);
    function ObsType: string; virtual;
    procedure WriteMF6ObsOption(InputFileName: string);
    procedure WriteBoundName(ACell: TValueCell);
    Class function Mf6ObType: TObGeneral; virtual;
    Class function Mf6GwtObType: TObGwt; virtual;
  public
    // @name creates and instance of @classname.
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    // @name destroys the current instance of @classname.
    Destructor Destroy; override;
    property DirectObsLines: TStrings read FDirectObsLines write FDirectObsLines;
    property CalculatedObsLines: TStrings read FCalculatedObsLines write FCalculatedObsLines;
    property FileNameLines: TStrings read FFileNameLines write FFileNameLines;
    // @name is the file extension used for the observation output file.
    class function ObservationOutputExtension: string; virtual;
  end;

  {@name is used as a base class for writing typical boundary condition
   files that allow for both parameter and non-parameter cells.}
  TCustomParameterTransientWriter = class(TCustomTransientWriter)
  private
    FNumInstList: TGenericIntegerList;
    procedure StorePotentialObservationLocationsAndNames;
  protected
    FParameterNames: TStringList;
    // @name is used to maintain a list of parameter instance names so that
    // duplicate instance names are not generated. @name is filled once
    // when exporting the parameters and again when exporting the
    // stress periods.  Thus it must be cleared at the beginning of the process
    // of exporting the stress periods.
    FUsedInstanceNames: TStringList;
    // After @link(Evaluate) is called,
    // @name contains a series of parameter names.  Associated with each
    // parameter name in the Objects property is a TList.  Each TList
    // contains a series of @link(TValueCellList)s; one for each stress period.
    // Each such list contains series of @link(TValueCell)s. Each
    // @link(TValueCell) defines one boundary cell for one stress period.
    // See @link(ParamValues).
    FParamValues: TStringList;
    function BoundariesPresent: boolean; virtual;
    // @name is used to create a @link(TValueCellList) for a particular type
    // of @link(TValueCell).
    function CellType: TValueCellType; virtual; abstract;
    // @name returns the number of cells defined by parameters and the number
    // of parameters instances for the parameter.
    procedure GetNumCellsAndNumInstances(ParamValues: TList;
      var NUMINST, NLST: Integer);
    // @name is an abstract function used to return the proper
    // @link(TModflowBoundary) for the package that is being exported.
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      virtual; abstract;
    // @name writes the non-parameter cells and the names of the parameter
    // instance for each stress period.
    // @param(DataSetIdentifier DataSetIdentifier is passed to
    //   @link(TCustomListWriter.WriteCell).)
    // @param(VariableIdentifiers VariableIdentifiers is passed to
    //   @link(TCustomListWriter.WriteCell).)
    procedure WriteStressPeriods(const VariableIdentifiers, DataSetIdentifier,
      DS5, D7PNameIname, D7PName: string); virtual; abstract;
    // @name returns the proper name for an instance of a parameter in
    // InstanceName. InstanceRoot should have been obtained from
    // @link(GetInstanceRoot). @name needs to return the same value of the
    // instance name for each unique combination of TimeIndex and InstanceRoot;
    procedure GetInstanceName(var InstanceName: string; TimeIndex: Integer;
      InstanceRoot: string);
    // @name uses PARNAM to determine InstanceRoot which is then used in
    // @link(GetInstanceName). InstanceRoot will be short enough that
    // the name of the parameter instance will always be less than or equal
    // to 10 characters.
    procedure GetInstanceRoot(const PARNAM: string; ParamValues: TList;
      var InstanceRoot: string);
    // @name determines the appropriate value or values to assign to each cell
    // for each stress period.
    procedure Evaluate; override;
    // @name adds lines to ParametersUsed that specify the parameters
    // that are used in the current stress period.
    // @name fills ParameterValues with lists of @link(TValueCellList)s.
    procedure RetrieveParametersForStressPeriod(const D7PNameIname,
      D7PName: string; TimeIndex: Integer; ParametersUsed: TStringList;
      ParameterValues: TList; IncludePrintCode: boolean);
    // @name is the number of parameters used by the package.
    function ParameterCount: integer; virtual;
    // @name is an abstract method that returns the package being exported.
    function ParameterType: TParameterType; virtual; abstract;
    // @name is an abstract method used to write the cells for a parameter.
    // If CellList.Count < NLST, dummy, inactive cells should be written so
    // that all instances of the parameter have the same number of cells.
    // @param(CellList CellList contains a list of @link(TValueCell)s to be
    //   exported.)
    // @param(NLST NLST is the required number of cells in each parameter
    //   instance.)
    // @param(DataSetIdentifier DataSetIdentifier is passed to
    //   @link(TCustomListWriter.WriteCell).)
    // @param(VariableIdentifiers VariableIdentifiers is passed to
    //   @link(TCustomListWriter.WriteCell).)
    procedure WriteParameterCells(CellList: TValueCellList; NLST: Integer;
      const VariableIdentifiers, DataSetIdentifier: string;
      AssignmentMethod: TUpdateMethod;
      MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection); virtual; abstract;
    // @name writes the definitions of parameters including the lists of cells.
    // @param(DS3 DS3 identifies the data set containing
    //   PARNAM PARTYP Parval NLST  and those variable names.)
    // @param(DS3Instances DS3Instances identifies the variables
    //    associated with parameters instances.)
    // @param(DS4A DS4A identifies the data set containing INSTNAM. when
    //   parameter instances are being defined.)
    // @param(DataSetIdentifier DataSetIdentifier is passed to
    //   @link(TCustomListWriter.WriteCell).)
    // @param(VariableIdentifiers VariableIdentifiers is passed to
    //   @link(TCustomListWriter.WriteCell).)
    // @param(ErrorRoot ErrorRoot is used in a format statement to create
    //   an error message when a parameter has no associated cells.
    //   It should have %s in it somewhere which will be filled in with the
    //   type of parameter being exported.)
    // @param(ParameterType ParameterType is the type of parameter associated
    //   with the package being exported.)
    procedure WriteParameterDefinitions(const DS3, DS3Instances, DS4A,
      DataSetIdentifier, VariableIdentifiers, ErrorRoot: string;
      AssignmentMethod: TUpdateMethod;
      MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection); virtual; abstract;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure ClearTimeLists(AModel: TBaseModel);
    procedure WriteMf6ParamListOption;
    // @name counts the number of parameters and the maximum number of cells
    // defined by parameters. If a parameter has no cells associated with it,
    // its name will be erased from FParamValues which will cause a warning
    // message to be displayed later on.
    // @seealso(ParameterCount)
    procedure CountParametersAndParameterCells(var ParamCount,
      ParamCellCount: integer);
    procedure WriteBoundaryArrayParams;
  public
    // @name creates and instance of @classname.
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    Destructor Destroy; override;
    // After @link(Evaluate) is called,
    // @name contains a series of parameter names.  Associated with each
    // parameter name in the Objects property is a TList.  Each TList
    // contains a series of @link(TValueCellList)s; one for each stress period.
    // Each such list contains series of @link(TValueCell)s. Each
    // @link(TValueCell) defines one boundary cell for one stress period.
    property ParamValues: TStringList read FParamValues;
  end;

  TCustomListWriter = class(TCustomParameterTransientWriter)
  private
    FObjectNames: TStringList;
  strict protected
    FStressPeriod: Integer;
    FBoundaryIndex: Integer;
  protected
    FStartTime: Double;
    procedure WriteMF6_ListParm(DataSetIdentifier, VariableIdentifiers,
      ErrorRoot: string; const TimeIndex: integer); virtual;
    // @name counts the maximum number of cells used in any stress period. This
    // value is returned in MaximumNumberOfCells.
    procedure CountCells(var MaximumNumberOfCells: Integer); override;

    // @name returns the 'Option" used in many packages for suppressing
    // the printing of lists of cells.
    procedure GetOption(var Option: string); virtual;
    // @name is an abstract method for writes a cell.
    // @param(DataSetIdentifier DataSetIdentifier identifies the data set
    // that is being exported.)
    // @param(VariableIdentifiers VariableIdentifiers identifies the variables
    // being exported when the names of some variables vary.)
    procedure WriteCell(Cell: TValueCell;
      const DataSetIdentifier, VariableIdentifiers: string); virtual; abstract;
    // @name writes the definitions of parameters including the lists of cells.
    // @param(DS3 DS3 identifies the data set containing
    //   PARNAM PARTYP Parval NLST  and those variable names.)
    // @param(DS3Instances DS3Instances identifies the variables
    //    associated with parameters instances.)
    // @param(DS4A DS4A identifies the data set containing INSTNAM. when
    //   parameter instances are being defined.)
    // @param(DataSetIdentifier DataSetIdentifier is passed to
    //   @link(WriteCell).)
    // @param(VariableIdentifiers VariableIdentifiers is passed to
    //   @link(WriteCell).)
    // @param(ErrorRoot ErrorRoot is used in a format statement to create
    //   an error message when a parameter has no associated cells.
    //   It should have %s in it somewhere which will be filled in with the
    //   type of parameter being exported.)
    // @param(ParameterType ParameterType is the type of parameter associated
    //   with the package being exported.)
    procedure WriteParameterDefinitions(const DS3, DS3Instances, DS4A,
      DataSetIdentifier, VariableIdentifiers, ErrorRoot: string;
      AssignmentMethod: TUpdateMethod;
      MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection); override;
    // @name writes the non-parameter cells and the names of the parameter
    // instance for each stress period.
    // @param(DataSetIdentifier DataSetIdentifier is passed to
    //   @link(WriteCell).)
    // @param(VariableIdentifiers VariableIdentifiers is passed to
    //   @link(WriteCell).)
    procedure WriteStressPeriods(const VariableIdentifiers, DataSetIdentifier,
      DS5, D7PNameIname, D7PName: string); override;
    // Check that the cell is in an active cell.

    // @name can be overridden to write additional information for
    // a stress period.
    procedure WriteCustomStressPeriod(TimeIndex: Integer); virtual;
    procedure DoBeforeWriteCells; virtual;
    procedure DoBeforeWriteParamCells; virtual;
    procedure WriteListOptions(InputFileName: string); virtual;
    procedure WriteOptionsMF6(InputFileName: string);
    procedure WriteAndCheckCells(const VariableIdentifiers: string;
      const DataSetIdentifier: string; List: TValueCellList;
      TimeIndex: integer); virtual;
    procedure WriteMoverOption; virtual;
    // @name is used to write additional auxillary variables such as
    // chemical species names with GWT.
    procedure WriteAdditionalAuxVariables; virtual;
    function GetObjectString(ErrorObject: TObject): string;
    function EvaluateValueFromGlobalFormula(Formula: string;
      ErrorObject: TObject; const DataSetErrorString: string;
      const OKTypes: TRbwDataTypes): TExpression;
    procedure WriteFloatValueFromGlobalFormula(Formula: string;
      ErrorObject: TObject; const DataSetErrorString: string;
      TestProc: TTestRealValueOkProcedure = nil);
    procedure WriteBooleanValueFromGlobalFormula(Formula: string;
      ErrorObject: TObject; const DataSetErrorString: string);
    procedure WriteIntegerValueFromGlobalFormula(Formula: string;
      ErrorObject: TObject; const DataSetErrorString: string;
      TestValue: TTestIntValueOkProcedure = nil);
    // @name begins at 0. That is if @name is zero, that is the first stress
    // period.
    property StressPeriod: integer read FStressPeriod;
    function ITMPUsed: Boolean; virtual;
  public
    // @name is used to update the display of transient data used to color the
    // grid.
    // @param(TimeLists TimeLists is a list of
    //   @link(TModflowBoundaryDisplayTimeList)s that are to be updated.
    //   The order of the @link(TModflowBoundaryDisplayTimeList)s in TimeLists
    //   is important. The position in the list must be same as the index
    //   value used to access @link(TValueCell.RealValue TValueCell.RealValue)
    //   and @link(TValueCell.RealAnnotation TValueCell.RealAnnotation).)
    // @param(ParameterIndicies The values included in ParameterIndicies
    //   indicate which @link(TModflowBoundaryDisplayTimeList)s in TimeLists
    //   are affected by MODFLOW parameters.)
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists;
      ParameterIndicies: TByteSet);
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    Destructor Destroy; override;
  end;

  TTransientArrayWriter = class(TCustomModflowWriter)
  protected
    class function Extension: string; override;
  public
    function WriteFile(const AFileName, ArrayType: string;
      ADataArray: TDataArray): string;
  end;

  TLayerArrayWriter = class(TCustomModflowWriter)
  protected
    class function Extension: string; override;
  public
    function WriteFile(const AFileName: string;
      ADataArray: TDataArray; ALayer: Integer): string;
  end;


  // @name is used for packages that have an associated flux observation
  // package. @name writes the input for both the boundary condition
  // and the observations.
  TFluxObsWriter = class(TCustomListWriter)
  private
    FPestInstructionFile: TStringList;
    function GetFluxType(ObservationGroup: TFluxObservationGroup): string;
    procedure RemoveWarningGroups(ObservationGroup: TFluxObservationGroup);
  protected
    // @name must be set to the name of the input file for the package.
//    FFileName: string;
    function ObservationPackage: TModflowPackageSelection; virtual; abstract;

    // @name returns the name of the Observation output file.
    function ObservationOutputFileName(AFileName: string): string;
    // @name writes OBSNAM IREFSP TOFFSET FLWOBS for the flux observations.
    procedure WriteObservationDataSet4(ObservationGroup: TFluxObservationGroup;
      DataSet4: TStringList);
    // @name writes the observation cells to be included in the observation to
    // DataSet5 for a @link(TFluxObservationGroup).
    // @param(AllCells is a list of the cells from the
    // corresponding flux package.)
    procedure WriteObservationDataSet5(DataSet5: TStringList;
      ObservationGroup: TFluxObservationGroup; AllCells: TList); virtual;
    // @name writes a flux observation file.
    procedure WriteFluxObsFile(const AFileName, OutputUnitId,
      PackageAbbreviation, DataSet1Comment, DataSet2Comment,
      DataSet3Comment: string; Observations: TFluxObservationGroups;
      Purpose: TObservationPurpose);
    // @name writes a cell with a factor of zero in order to ensure that
    // MODFLOW skips the cell in the observation.
    procedure WriteZeroConductanceCell(ACell: TValueCell;
      DataSet5: TStringList); virtual;
    // @name writes the layer, row, column and factor for the
    // @link(TValueCell ACell) to DataSet5.
    // @param(Expression determines what "Factor" should be.)
    // @param(Variables is a list of @link(TCustomVariable)s used by
    //  Expression.)
    // @param(DataSets is a list of @link(TDataArray)s corresponding to
    // the @link(TCustomVariable)s in Variables.)
    procedure WriteObservationCell(ACell: TValueCell; DataSet5: TStringList;
      var Expression: TExpression; DataSets, Variables: TList;
      ObsFactor: TObservationFactor); virtual;
    // @name writes the cells to be include in the observation to DataSet5.
    // @param(Variables is a list of @link(TCustomVariable)s used by
    //  Expression.)
    // @param(DataSets is a list of @link(TDataArray)s corresponding to
    // the @link(TCustomVariable)s in Variables.)
    // @param(Expression determines what "Factor" should be for each cell.)
    // @param(AllCells is a list of @link(TValueCell)s. from the corresponding
    // flux package.)
    // @param(ScreenObject is the @link(TScreenObject) that defines the
    // cells to be exported.)
    procedure WriteObservationCells(Variables, DataSets: TList;
      var Expression: TExpression; DataSet5: TStringList; AllCells: TList;
      ScreenObject: TScreenObject; ObsFactor: TObservationFactor); virtual;
    function ObsNameWarningString: string; virtual; abstract;
    procedure WriteDataSet5ForOneScreenObject(ObsFactor: TObservationFactor;
      DataSet5: TStringList; AllCells: TList);
    procedure EvaluateFactor(var Factor: Extended; var Expression: TExpression;
      ACell: TValueCell; DataSets, Variables: TList;
      ObsFactor: TObservationFactor);
    procedure SavePestInstructionFile(OutputName: string);
    function IsFlowObs(AScreenObject: TScreenObject): Boolean; override;
    function ObsFactors: TFluxObservationGroups; virtual;
    procedure EnsureMf6CalibObservations(AScreenObject: TScreenObject); override;
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    Destructor Destroy; override;
  end;

  TCustomTransientArrayWriter = class(TCustomParameterTransientWriter)
  private
    {@name assigns values to DataArray based on Param and
     the contents of CellList.}
    procedure EvaluateParameterCells(CellList: TValueCellList;
      DataArray: TModflowBoundaryDisplayDataArray;
      Param: TModflowTransientListParameter; AssignmentMethod: TUpdateMethod);
    {@name assigns the layer where the array boundary condition will be applied
    based on the contents of List.
    List can be a @link(TValueCellList) or contain @link(TValueCellList)s
    or contain lists of @link(TValueCellList)s.}
    procedure UpdateLayerDataSet(List: TList;
      DisplayArray: TModflowBoundaryDisplayDataArray);
  protected
//    FNameOfFile: string;
    // @name stores information about which layer is used to assign
    // an array boundary condition for each stress period.
    // @name is actually a TObjectList.
    // @name is only used if the layers are specified and time-varying.
    FLayers: TList;
    // @name updates DataArray with the layer used to assign the
    // array boundary condition.
    procedure UpdateLayerDisplay(List: TValueCellList; ParameterValues: TList;
      TimeIndex: Integer; DataArray: TModflowBoundaryDisplayDataArray);
    // On exit, List contains a @link(TModflowBoundaryDisplayDataArray)
    // for each active parameter for each stress period.
    procedure EvaluateParameterDefinitions(List: TList;
      const ErrorRoot: string; AssignmentMethod: TUpdateMethod);
    // Name is used in assigning the names of Multiplier array
    // and Zone array data sets.
    function Prefix: string; virtual; abstract;
    // @name creates transient multiplier and zone arrays to define where
    // the array parameter should be applied and the  for the cells in CellList.
    // NLST is not used in this version of @name.
    // DataSetIdentifier is not used in this version of @name.
    procedure WriteParameterCells(CellList: TValueCellList; NLST: integer;
      const VariableIdentifiers, DataSetIdentifier: string;
      AssignmentMethod: TUpdateMethod;
      MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection); override;
    procedure WriteParameterDefinitions(const DS3, DS3Instances, DS4A,
      DataSetIdentifier, VariableIdentifiers, ErrorRoot: string;
      AssignmentMethod: TUpdateMethod;
      MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection); override;
    // @name is used to write the layer on which the array boundary
    // condition is applied for each cell in Lists.
    procedure WriteLayerArray(Lists: TList; const Comment: string;
      const MF6_ArrayName: string);
    // @name writes the layer where the array boundary should be applied.
    // If the layer is time-varying, FLayers is used to define the layers.
    // If parameters are used, ParameterValues is used to define the layers.
    // Otherwise, List is used to define the layers.
    procedure WriteLayerSelection(List: TValueCellList;
      ParameterValues: TList; TimeIndex: Integer; const Comment: string;
      const MF6_ArrayName: string);
    // @name clears @link(FUsedInstanceNames) so that the same parameter
    //instance names will be used in the stress periods as used in the
    // paramater definitions.
    procedure WriteStressPeriods(const VariableIdentifiers, DataSetIdentifier,
      DS5, D7PNameIname, D7PName: string); override;
    function ParameterCount: integer; override;
    procedure WriteEndPeriod;
    function OkLocationMF6(IDomain: TDataArray;
      UsedLocations: T2DSparseBooleanArray;
      var Layer: integer; Row, Column: integer; Option: TLayerOption): boolean;
    function CountCellsMF6(RateList: TValueCellList; Option: TLayerOption): integer;
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    // @name destroys the current instance of @classname.
    Destructor Destroy; override;
  end;

  TCustomSubWriter = class(TCustomPackageWriter)
  protected
    procedure GetStartAndEndTimeSteps(var ITS2, ISP2, ITS1, ISP1: Integer;
      PrintChoice: TCustomPrintItem);
    function StartTimeOK(Time: double; PrintChoice: TCustomPrintItem): boolean;
    function EndTimeOK(Time: double; PrintChoice: TCustomPrintItem): boolean;
  end;

  TCustomNameFileWriter = class(TCustomModflowWriter)
  private
    FNameFile: TStringList;
    FInputFiles: TStringList;
    FOutputFiles: TStringList;
    FListFileName: string;
    FFullOutputFiles: TStringList;
    FFullInputFiles: TStringList;
//    FInputOptions: TStringList;
  protected
    // @name clears the name file.
    procedure ClearNameFile;
    procedure InitilizeNameFile(Const FileName: string;
      out OutputListFileName: string); virtual; abstract;
    procedure HandleMF6Files; virtual;
    function ArchiveExtension: string; virtual;
    function GetModelName(AFileName: string): string; virtual;
  public
    Constructor Create(AModel: TCustomModel; const FileName: string;
      EvaluationType: TEvaluationType); reintroduce;
    Destructor Destroy; override;
    // Name saves the name file to a file.
    procedure SaveNameFile(AFileName: string); virtual;
  public
    property ListFileName: string read FListFileName;
    property NameFile: TStringList read FNameFile;
    property InputFiles: TStringList read FInputFiles;
    property OutputFiles: TStringList read FOutputFiles;
  end;

  TNameFileWriter = class(TCustomNameFileWriter)
  private
    procedure CheckExternalFiles(const FileName: string);
  protected
    // @name adds lines to the name file to define the listing file,
    // the cell-by-cell flow file, and the files generated outside of
    // ModelMuse.
    procedure InitilizeNameFile(Const FileName: string;
      out OutputListFileName: string); override;
    procedure HandleMF6Files; override;
  public
    procedure SaveNameFile(AFileName: string); override;
    class function Extension: string; override;
  end;

  TMt3dmsNameWriter = class(TCustomNameFileWriter)
  protected
    procedure InitilizeNameFile(Const FileName: string;
      out OutputListFileName: string); override;
    function ArchiveExtension: string; override;
//    function GetModelName(AFileName: string): string; override;
  public
    procedure SaveNameFile(AFileName: string); override;
    class function Extension: string; override;
  end;

  TMf6GwtNameWriter = class(TCustomModflowWriter)
  private
    FPackageLines: TStringList;
    FFileName: string;
    FSpeciesName: string;
    FSpeciesIndex: Integer;
    procedure WriteOptions;
    procedure WritePackages;
  protected
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel; const FileName: string; SpeciesIndex: Integer;
      EvaluationType: TEvaluationType); reintroduce;
    destructor Destroy; override;
    procedure AddPackageFile(FileType: string; FileName: string;
      PackageName: string = '');
    procedure WriteFile;
  end;

  TMf6GwtNameWriters = TObjectList<TMf6GwtNameWriter>;

  TModelDataList = TList<TModelData>;

  TMf6_SimNameFileWriter = class(TCustomModflowWriter, IMf6_SimNameFileWriter)
  private
    FTDisFileName: string;
    FModelDataList: TModelDataList;
    FExchanges: TStringList;
    FGwtTDisFileNames: TStringList;
    FSpeciesIndex: Integer;
    FSeparateGwt: Boolean;
    FSimFileNames: TStringList;
    FWritingFlowModel: Boolean;
    procedure WriteOptions;
    procedure WriteTiming;
    procedure WriteModels;
    procedure WriteExchanges;
    procedure WriteSolutionGroups;
    function GetTDisFileName: string;
    procedure SetTDisFileName(const Value: string);
    function GetGwtTDisFileNames(SpeciesIndex: Integer): string;
    procedure SetGwtTDisFileNames(SpeciesIndex: Integer; const Value: string);
    procedure WriteFileInternal(FileName: string; BackupFileName: WideString);
    function GetShouldWriteLine(GwtUsed: Boolean; SeparateGwtUsed: Boolean;
      ModelIndex: Integer): Boolean;
    function GetSimFileName(Index: Integer): string;
    function GetSimFileNameCount: Integer;
  protected
    FRefCount: Integer;
    class function Extension: string; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    Constructor Create(AModel: TCustomModel); reintroduce;
    destructor Destroy; override;
    property TDisFileName: string read GetTDisFileName write SetTDisFileName;
    procedure AddModel(ModelData: TModelData);
    procedure AddExchange(ExchangeLine: string);
    procedure WriteFile(FileName: string);
    property GwtTDisFileNames[SpeciesIndex: Integer]: string
      read GetGwtTDisFileNames write SetGwtTDisFileNames;
    property SimFileNameCount: Integer read GetSimFileNameCount;
    property SimFileNames[Index: Integer]: string read GetSimFileName;
  end;

procedure MoveAppToDirectory(const AppFullPath, Directory: string);

// name writes a batch-file used to run MODFLOW.
function WriteModflowBatchFile(ProgramLocations: TProgramLocations;
  const FileName: string; ListFiles: TStringList; OpenListFile: boolean;
  Before, After: TStrings; ExportModpath, ExportZoneBudget: boolean;
  Model: TCustomModel): string;

function WriteMt3dmsBatchFile(ProgramLocations: TProgramLocations;
  FileName: string; ListFiles: TStringList; OpenListFile: boolean;
  Model: TCustomModel): string;

// name writes a batch-file used to run MODPATH.
function WriteModPathBatchFile(ProgramLocations: TProgramLocations;
  FileName: string; ListFile: string; OpenListFile: boolean;
  const LargeBudgetFileResponse: string; EmbeddedExport: boolean; Model: TCustomModel): string;

function WriteModPathBatchFileVersion6(ProgramLocations: TProgramLocations;
  FileName: string; ListFile: string; OpenListFile: boolean;
  EmbeddedExport: boolean; Model: TCustomModel): string;

function WriteModPathBatchFileVersion7(ProgramLocations: TProgramLocations;
  FileName: string; ListFile: string; OpenListFile: boolean;
  EmbeddedExport: boolean; Model: TCustomModel): string;

function WriteZoneBudgetBatchFile(Model: TCustomModel;
  FileName: string; OpenListFile, EmbeddedExport: boolean): string;

procedure ResetMaxUnitNumber;
function GetMaxUnitNumber: integer;

procedure SetCurrentNameFileWriter(NameFileWriter: TCustomNameFileWriter);

procedure AddOpenListFileLine(ListFile: string; OpenListFile: Boolean;
  BatchFile: TStringList; ProgramLocations: TProgramLocations);

function GetPLPROC_Location(const FileName: string; Model: TCustomModel): string;

resourcestring
  StrObservationFactor = '(Observation factor for the ' + sLineBreak + '%s)';
  StrNoBoundaryConditio = 'No boundary conditions assigned to the %s because' +
  ' the object does not set the values of either enclosed or intersected cel' +
  'ls.';
  StrTheSPackageHasB = 'The %s package has been activated but no boundaries ' +
  'for it have been defined.';
  StrNoDefinedBoundarie = 'No defined boundaries in %s.';
  StrInSNoFlowObser = 'In %s, no flow observations of the correct type have ' +
  'been defined.';
  StrSelectModelObserv = 'Select "Model|Observation Type" and make sure you ' +
  'have the correct observation type selected.';
  StrLayerRowCol = 'Layer, Row, Col = [%0:d, %1:d, %2:d]';
  StrLayerRowColObject = 'Layer, Row, Col = [%0:d, %1:d, %2:d] defined by the object %3:s';
  StrLayerRowColObjectAmount = 'Layer, Row, Col = [%0:d, %1:d, %2:d] defined by the object %3:s. Amount: %4:g.';
  StrEvaluatingS = '  Evaluating %s';
  StrEvaluatingSData = 'Evaluating %s data.';
  StrWritingStressPer = '  Writing Stress Period %d';
  StrNoBoundaryConditio1 = 'No boundary conditions for the %0:s in one or mor' +
  'e stress periods.';
  StrStressPeriod0d = 'Stress Period %0:d';
  StrOneOrMoreSParam = 'One or more %s parameters have been eliminated becau' +
  'se there are no cells associated with them.';
  StrRunModelBat = 'RunModel.Bat';

const
  // @name is the comment assigned to locations in a @link(TDataArray) where
  // no value has been assigned but a value is still needed.
  StrDATABINARY = 'DATA(BINARY)';
  StrDATA = 'DATA';
  StrArrays = 'arrays';
  StrKrigfactorsscript = '.krig_factors_script';
  StrIfExist0sDel = 'if exist "%0:s" del "%0:s"';

implementation

uses frmErrorsAndWarningsUnit, ModflowUnitNumbers, frmGoPhastUnit,
  frmProgressUnit, GlobalVariablesUnit, frmFormulaErrorsUnit, GIS_Functions,
  ZoneBudgetWriterUnit, ModelMuseUtilities, SparseArrayUnit,
  RealListUnit, ModflowMultiplierZoneWriterUnit, IOUtils,
  ModpathResponseFileWriterUnit, ModflowPackagesUnit, Math,
  System.Generics.Defaults, ArchiveNodeInterface, ModflowOptionsUnit,
  Modflow6ObsWriterUnit, ModflowTimeSeriesWriterUnit,
  ModflowTimeSeriesUnit, ModflowMvrWriterUnit, PlProcUnit,
  PestObsExtractorInputWriterUnit,
  System.AnsiStrings, ModflowMf6TimeSeriesWriterUnit, ModflowFmpWriterUnit,
  DataSetNamesUnit, CellLocationUnit, ScreenObjectInterfaceUnit;

resourcestring
  StrTheFollowingParameSkip = 'The following %s parameters are being skipped ' +
  'because they have no cells associated with them.';
  StrValueTooLong = 'Value too long';
  StrSIsTooLong10 = '%s is too long to be displayed with 10 characters';
  StrSIsTooLong2 = '%s is too long to be displayed with 2 characters';
  StrTheSInputFileCa = 'The %s input file can not be created.';
  StrInTheBoundaryPack = 'In the boundary package related to the %s package,' +
  ' no boundaries were defined.';
  StrErrorFlowObservatEarly = 'Error; Flow Observation = %0:s Early Times = ' +
  '%1:s';
  StrErrorFlowObservatLate = 'Error; Flow Observation = %0:s Late Times = ' +
  '%1:s';
  EarlyTimeWarning = '%s flow observation times earlier than the beginning of the first stress period will be ignored.';
  LateTimeWarning = '%s flow observation times later than the end of the last stress period will be ignored.';
  MissingFile = 'One or more files that you specified in the MODFLOW Name '
    + 'File dialog box are missing. If these are input files, MODFLOW will '
    + 'not be able to run.';
  StrNameFileForMODFLO = 'Name File for MODFLOW created on %0:s by %1:s';
  StrNameFileForMT3DMS = 'Name File for MT3DMS created on %0:s by %1:s';
  StrTheFlowtransportL = 'The flow-transport link file, %s, does not exist. ' +
  'Run MODFLOW again to correct the problem.';
  StrMissingFtlFile = 'Missing *.ftl file.';
  StrWritingS = '    Writing %s';
  StrWriting0sFor = '    Writing %0:s for Layer %1:d';
  StrWritingArray = '    Writing array';
  StrWritingParamter = '    Writing paramter: %s';
  StrLayer0dRow1 = 'Layer: %0:d; Row: %1:d; Column: %2:d';
  StrInputFileDoesNot = 'Input file does not exist.';
  StrTheRequiredInputF = 'The required input file "%s" does not exist.';
  StrNoParametersAreBe = 'No parameters are being used in one or more stress' +
  ' periods in the %s package.';
  StrNoValuesAssignedT = 'No values assigned to the following data sets';
  StrMissingObservationObjects = 'In the %s package, no objects are part of ' +
  'the following observations.';
  StrOneOrMoreBoundari = 'One or more boundaries in the %s are in inactive c' +
  'ells';
  StrFileNameTooLong = 'FileName too long';
  StrTheMaximumAllowed = 'The maximum allowed file name length in MODFLOW 6 ' +
  'is 300. This file (%0:s) has a length of %1:d.';
  StrDirectoryDoesNotE = 'Directory does not exist.';
  StrTheDirectoryFor = 'The directory for "%s" does not exist.';
  StrInvalidSpecifiedHe = 'Invalid specified head cell properties';
  StrTheSpecifiedHeadC = 'The specified head cell at (Layer,Row,Col) = (%0:d' +
  ',%1:d,%2:d) is invalid because the %3:s, %4:s, and %5:s data sets all hav' +
  'e values of zero at that cell.';
  Str0sMultipliedByParam = '%0:s (multiplied by %1:s = %2:g)';
  StrPestFormulaFormat = ' %0:s                    %1:s                    %2:s%1:s ' +
  '%4:s %3:g%0:s ';
  StrNoAlternativeSolve = 'No alternative solver specified';
  StrInTheMODFLOWName = 'In the MODFLOW Name file dialog box, the option to ' +
  'specify an alternative solver was selected but none was specified.';
  StrNoAlternativeFlow = 'No alternative flow package specified';
  StrInTheMODFLOWNameFlow = 'In the MODFLOW Name file dialog box, the option' +
  ' to specify an alternative flow package was selected but none was specifi' +
  'ed.';
  StrPLPROCNotFound = 'PLPROC not found';
  StrPLPROCWasNotFound = 'PLPROC was not found in %s.';
  StrArrayFormulaFormat = ' %0:s                    %1:s                    %2' +
  ':s[%5:d,%6:d,%7:d]%1:s %4:s %3:g%0:s ';
  StrExtendedTemplateFormat = ' %0:s                    %1:s%0:s ';
  StrErrorHandlingTheF = 'Error handling the following name file lines';
  StrMODFLOWTimeSeries = 'MODFLOW time series Interpolated value in series %' +
  '0:s at time %1:g.';
  StrTheFormulaShouldInt = 'The formula should result in an integer';

const
  StrMf6ObsExtractorexe = 'Mf6ObsExtractor.exe';
  StrMf2005ObsExtractor = 'Mf2005ObsExtractor.exe';
  StrPlprocexe = 'plproc.exe';

var
//  NameFile: TStringList;
  MaxUnitNumber: integer = 0;
  CurrentNameFileWriter: TCustomNameFileWriter;

const
  ExtFileString = '..\externalfiles\';
  ExtFileLength = Length(ExtFileString);

procedure SetCurrentNameFileWriter(NameFileWriter: TCustomNameFileWriter);
begin
  CurrentNameFileWriter := NameFileWriter;
end;

procedure ResetMaxUnitNumber;
begin
  MaxUnitNumber := 0;
end;

function GetMaxUnitNumber: integer;
begin
  result := MaxUnitNumber;
end;

procedure AddOpenListFileLine(ListFile: string; OpenListFile: Boolean;
  BatchFile: TStringList; ProgramLocations: TProgramLocations);
var
  TextEditor: string;
begin
  if OpenListFile then
  begin
    TextEditor := ProgramLocations.TextEditorLocation;
    if TextEditor <> StrNotepadexe then
    begin
      if not FileExists(TextEditor) then
      begin
        TextEditor := StrNotepadexe;
      end;
    end;
    if TextEditor <> StrNotepadexe then
    begin
      TextEditor := ExpandFileName(TextEditor);
    end;
    TextEditor := QuoteFileName(TextEditor);
    ListFile := QuoteFileName(ExtractFileName(ListFile));
    BatchFile.Add('Start ' + TextEditor + ' ' + ListFile);
  end;
end;

function WriteMt3dmsBatchFile(ProgramLocations: TProgramLocations;
  FileName: string; ListFiles: TStringList; OpenListFile: boolean;
  Model: TCustomModel): string;
var
  ADirectory: string;
  Mt3dmsLocation: string;
  BatchFile: TStringList;
  AFileName: string;
  ListFileIndex: Integer;
  Modelname: string;
  ModelDirectory: string;
  NetworkDrive: Boolean;
begin
  ADirectory:= GetCurrentDir;
  try
    result := ExtractFileDir(FileName);
    ModelDirectory := result;
    NetworkDrive := IsNetworkDrive(FileName);
    SetCurrentDir(result);
    if Model.ModflowPackages.Mt3dBasic.Mt3dVersion = mvUSGS then
    begin
      result := IncludeTrailingPathDelimiter(result)
        + 'RunMt3d-USGS.Bat';
    end
    else
    begin
      result := IncludeTrailingPathDelimiter(result)
        + 'RunMt3dms.Bat';
    end;

    if Model.ModflowPackages.Mt3dBasic.Mt3dVersion = mvMS then
    begin
      Mt3dmsLocation := ProgramLocations.Mt3dmsLocation;
    end
    else
    begin
      Mt3dmsLocation := ProgramLocations.Mt3dUsgsLocation;
    end;
    Model.AddBinaryFile(Mt3dmsLocation);

    BatchFile := TStringList.Create;
    try
      if NetworkDrive then
      begin
        BatchFile.Add('pushd ' + ModelDirectory);
      end;

      AFileName :=  QuoteFileName(ExpandFileName(Mt3dmsLocation));
      BatchFile.Add(AFileName + ' ' + ExtractFileName(FileName) {+ ' /wait'});

      for ListFileIndex := 0 to ListFiles.Count - 1 do
      begin
        AddOpenListFileLine(ListFiles[ListFileIndex], OpenListFile,
          BatchFile, ProgramLocations);
      end;

      if NetworkDrive then
      begin
        BatchFile.Add('popd');
      end;
      BatchFile.Add('pause');
      BatchFile.SaveToFile(result);
    finally
      BatchFile.Free;
    end;

    BatchFile := TStringList.Create;
    try
      BatchFile.Add('if not exist "..\..\output\NUL" mkdir "..\..\output"');
      Modelname := ChangeFileExt(ExtractFileName(FileName), '');
      BatchFile.Add(Format('if not exist "..\..\output\%0:s_MT3DMS\NUL" mkdir "..\..\output\%0:s_MT3DMS"', ['output.' + Modelname]));

      AFileName := '..\..\bin\' + ExtractFileName(Mt3dmsLocation);
      BatchFile.Add(AFileName + ' ' + ExtractFileName(FileName) {+ ' /wait'});
      BatchFile.Add('pause');
      BatchFile.SaveToFile(result + ArchiveExt);
    finally
      BatchFile.Free;
    end;

  finally
    SetCurrentDir(ADirectory);
  end;
end;

var
  StoredPLPROC_Location: string = '';

function GetPLPROC_Location(const FileName: string; Model: TCustomModel): string;
var
  TestedLocations: TStringList;
begin
  if (StoredPLPROC_Location <> '')
    and (StoredPLPROC_Location <> StrPlprocexe) then
  begin
    result := StoredPLPROC_Location;
    Exit;
  end;
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrPLPROCNotFound);
  TestedLocations := TStringList.Create;
  try
    result := IncludeTrailingPathDelimiter
      (Model.ProgramLocations.PestDirectory) + StrPlprocexe;
    if not FileExists(result) then
    begin
      TestedLocations.Add(result);
      result := IncludeTrailingPathDelimiter(
        ExtractFileDir(Application.ExeName)) + StrPlprocexe;
      if not FileExists(result) then
      begin
        TestedLocations.Add(result);
        result := IncludeTrailingPathDelimiter(ExtractFileDir(FileName))
          + StrPlprocexe;
        if not FileExists(result) then
        begin
          TestedLocations.Add(result);
          if Model.PestUsed then
          begin
            frmErrorsAndWarnings.AddWarning(Model, StrPLPROCNotFound,
              Format(StrPLPROCWasNotFound, [TestedLocations.DelimitedText]));
          end;
          result := StrPlprocexe;
        end;
      end;
    end;
  finally
    TestedLocations.Free;
  end;
  StoredPLPROC_Location := result;
end;

procedure MoveAppToDirectory(const AppFullPath, Directory: string);
var
  AppName: string;
  NewFileName: string;
begin
  if Directory = '' then
  begin
    Exit;
  end;

  if not TFile.Exists(AppFullPath) then
  begin
    Exit;
  end;
  AppName := ExtractFileName(AppFullPath);
  NewFileName := IncludeTrailingPathDelimiter(Directory) + AppName;
  if SameText(AppFullPath, AppName) then
  begin
    Exit;
  end;
  if TFile.Exists(NewFileName) then
  begin
    if TFile.GetCreationTime(AppFullPath)
      = TFile.GetCreationTime(NewFileName) then
    begin
      Exit;
    end;
  end;
  if (frmGoPhast.PhastModel.AppsMoved.IndexOf(NewFileName) >= 0)
    and (frmGoPhast.PhastModel.AppsMoved.IndexOf(AppFullPath) >= 0) then
  begin
    Exit;
  end;
  frmGoPhast.PhastModel.AppsMoved.Add(NewFileName);
  frmGoPhast.PhastModel.AppsMoved.Add(AppFullPath);
  TFile.Copy(AppFullPath, NewFileName, True);
end;

function WriteModflowBatchFile(ProgramLocations: TProgramLocations;
  const FileName: string; ListFiles: TStringList; OpenListFile: boolean;
  Before, After: TStrings; ExportModpath, ExportZoneBudget: boolean;
  Model: TCustomModel): string;
var
  BatchFile: TStringList;
  ArchiveBatchFile: TStringList;
  AFileName: string;
  ADirectory: string;
  ModflowLocation: string;
  ListFileIndex: Integer;
  SwiObsExtractorPath: string;
  ParamEstBatFileName: string;
  ParamEstBatchFile: TStringList;
  ALine: string;
  ModelName: string;
  ChildIndex: Integer;
  AChildModel: TChildModel;
  ChildModelName: string;
  ChildModel: TChildModel;
  NetworkDrive: Boolean;
  ModelDirectory: string;
  SwiObsExtInputFile: string;
  MfsimName: string;
  WriteInstructionBatFileName: string;
  WriteInstuctionsBatchFile: TStringList;
  InsFileName: string;
  DSIndex: Integer;
  ADataArray: TDataArray;
  INFLE: string;
  PLPROC_Location: string;
  KrigFactorsFileName: string;
  FileRoot: string;
  BackupParamEstBatFileName: string;
  BackupRunModflow: string;
  SimIndex: Integer;
  SimFileName: string;
  ListFileName: string;
  MfListName: string;
  SimCount: Integer;
  ChemIndex: Integer;
  ChemExt: string;
//  BackupMfsimName: string;
//  CopyLine: WideString;
begin

  ADirectory:= GetCurrentDir;
  try
    FileRoot := ChangeFileExt(ExtractFileName(FileName), '.');
    NetworkDrive := IsNetworkDrive(FileName);
    result := ExtractFileDir(FileName);
    ModelDirectory := result;
    SetCurrentDir(result);
    BackupParamEstBatFileName := IncludeTrailingPathDelimiter(result)
      + FileRoot + StrRunModelBat;
    ParamEstBatFileName := IncludeTrailingPathDelimiter(result)
      + StrRunModelBat;

    WriteInstructionBatFileName := IncludeTrailingPathDelimiter(result)
      + 'WriteInstructions.Bat';
    BackupRunModflow := IncludeTrailingPathDelimiter(result)
      + FileRoot + 'RunModflow.Bat';
    result := IncludeTrailingPathDelimiter(result)
      + 'RunModflow.Bat';

    case frmGoPhast.PhastModel.ModelSelection of
      msModflow:
        begin
          ModflowLocation := ProgramLocations.ModflowLocation;
        end;
      msModflowLGR:
        begin
          ModflowLocation := ProgramLocations.ModflowLgrLocation;
        end;
      msModflowLGR2:
        begin
          ModflowLocation := ProgramLocations.ModflowLgr2Location;
        end;
      msModflowNWT:
        begin
          ModflowLocation := ProgramLocations.ModflowNwtLocation;
        end;
      msModflowFmp:
        begin
          ModflowLocation := ProgramLocations.ModflowOwhmLocation;
        end;
      msModflowCFP:
        begin
          ModflowLocation := ProgramLocations.ModflowCfpLocation;
        end;
      msModflow2015:
        begin
          ModflowLocation := ProgramLocations.Modflow6Location;
        end;
      msModflowOwhm2:
        begin
          ModflowLocation := ProgramLocations.ModflowOwhmV2Location;
        end;
      else Assert(False);
    end;

    Model.AddBinaryFile(ModflowLocation);

    ParamEstBatchFile := TStringList.Create;
    BatchFile := TStringList.Create;
    ArchiveBatchFile := TStringList.Create;
    WriteInstuctionsBatchFile := TStringList.Create;
    try
      ParamEstBatchFile.AddStrings(StartTimeCmdLines);
      Model.AddFilesToDeleteToBatchFile(ParamEstBatchFile, ParamEstBatFileName);

      PLPROC_Location := GetPLPROC_Location(FileName, Model);
      if Model.PestUsed then
      begin
        MoveAppToDirectory(PLPROC_Location, ModelDirectory);
        PLPROC_Location := ExtractFileName(PLPROC_Location);
      end;
      PLPROC_Location := Format('"%s" ', [PLPROC_Location]);
      for DSIndex := 0 to Model.DataArrayManager.DataSetCount - 1 do
      begin
        ADataArray := Model.DataArrayManager[DSIndex];
        if ADataArray.PestParametersUsed then
        begin
          INFLE := ExtractFileName(ChangeFileExt(FileName,
            '.' + ADataArray.Name + '.script' ));
          ParamEstBatchFile.Add(PLPROC_Location + INFLE);
        end;
      end;
      ParamEstBatchFile.AddStrings(Model.PestTemplateLines);

      ArchiveBatchFile.Add('if not exist "..\..\output\NUL" mkdir "..\..\output"');
      ModelName := ExtractFileName(ChangeFileExt(FileName, ''));
      ArchiveBatchFile.Add(Format('if not exist "..\..\output\%0:s\NUL" mkdir "..\..\output\%0:s"', ['output.' + ModelName]));

      if frmGoPhast.PhastModel.LgrUsed then
      begin
        for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
        begin
          AChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
          if AChildModel <> nil then
          begin
            ChildModelName := AChildModel.ModelNameForDos;
            ArchiveBatchFile.Add(Format('if not exist "..\..\output\%0:s_%1:s\NUL" mkdir "..\..\output\%0:s_%1:s"', ['output.' + ModelName, ChildModelName]));
          end;
        end;
      end;

      if NetworkDrive then
      begin
        BatchFile.Add('pushd ' + ModelDirectory);
      end;

      BatchFile.AddStrings(Before);
      ArchiveBatchFile.AddStrings(Before);
      ParamEstBatchFile.AddStrings(Before);

      if (Model.ModelSelection <> msModflow2015) then
      begin
        if FileExists(ProgramLocations.ModelMonitorLocation) then
        begin
          BatchFile.Add('call '
            + QuoteFileName(ExpandFileName(ProgramLocations.ModelMonitorLocation))
            + ' -m ' + QuoteFileName(ExpandFileName(ModflowLocation))
            + ' -n ' + QuoteFileName(ExtractFileName(FileName)));
        end
        else
        begin
          AFileName :=  QuoteFileName(ExpandFileName(ModflowLocation));
          BatchFile.Add(AFileName + ' '
            + QuoteFileName(ExtractFileName(FileName)) + ' /wait');
        end;
      end
      else
      begin
//        BackupMfsimName := FileRoot + 'mfsim.nam';
        MfsimName := 'mfsim.nam';
//        CopyLine := Format('if exist "%0:s" copy /Y "%0:s" "%1:s"', [BackupMfsimName, MfsimName]);
        MfListName := 'mfsim.lst';
        SimCount := frmGoPhast.PhastModel.SimNameWriter.SimFileNameCount;
        for SimIndex := 0 to SimCount -1 do
        begin
          SimFileName := ExtractFileName(frmGoPhast.PhastModel.SimNameWriter.
            SimFileNames[SimIndex]);
          ListFileName := ChangeFileExt(SimFileName, '.lst');
//          if SimCount > 1 then
          begin
            BatchFile.Add(Format('Copy /Y /A "%0:s" "%1:s"',
              [SimFileName, MfsimName]));
          end;

          if FileExists(ProgramLocations.ModelMonitorLocation) then
          begin

            BatchFile.Add('call '
              + QuoteFileName(ExpandFileName(ProgramLocations.ModelMonitorLocation))
              + ' -m ' + QuoteFileName(ExpandFileName(ModflowLocation))
              + ' -n ' + QuoteFileName(ExtractFileName(MfsimName))
              + ' -mv 6');
          end
          else
          begin
            AFileName :=  QuoteFileName(ExpandFileName(ModflowLocation));
            if (Model.ModelSelection = msModflow2015) then
            begin
              BatchFile.Add(AFileName {+ ' /wait'});
            end
            else
            begin
              BatchFile.Add(AFileName + ' '
                + QuoteFileName(ExtractFileName(FileName)) + ' /wait');
            end;
          end;
          if SimCount > 1 then
          begin
            BatchFile.Add(Format('Copy /Y /A "%0:s" "%1:s"',
              [MfListName, ListFileName]));
            ListFiles.Add(ListFileName);
            if SimIndex > 0 then
            begin
              ListFileName := ChangeFileExt(ListFileName, '');
              ListFileName := ChangeFileExt(ListFileName, '.lst');
              ListFiles.Add(ListFileName);
            end;
          end
          else
          begin
            ListFiles.Add(MfListName);
          end;
        end;
        if (SimCount = 1) and Model.GwtUsed then
        begin
          SimFileName := ExtractFileName(frmGoPhast.PhastModel.SimNameWriter.
            SimFileNames[0]);
          SimFileName := ChangeFileExt(SimFileName, '');
          for ChemIndex := 0 to Model.MobileComponents.Count - 1 do
          begin
            if Model.MobileComponents[ChemIndex].UsedForGWT then
            begin
              ChemExt := '.' + Model.MobileComponents[ChemIndex].Name + '.lst';
              ListFileName := ChangeFileExt(SimFileName, ChemExt);
              ListFiles.Add(ListFileName);
            end;
          end;
        end;
      end;

      AFileName := '..\..\bin\'+ ExtractFileName(ModflowLocation);
      if (Model.ModelSelection = msModflow2015) then
      begin
        SimCount := frmGoPhast.PhastModel.SimNameWriter.SimFileNameCount;
        for SimIndex := 0 to SimCount -1 do
        begin
          SimFileName := ExtractFileName(frmGoPhast.PhastModel.SimNameWriter.
            SimFileNames[SimIndex]);
          ListFileName := ChangeFileExt(SimFileName, '.lst');
          if SimCount > 1 then
          begin
            ArchiveBatchFile.Add(Format('Copy /Y /A "%0:s" "%1:s"',
              [SimFileName, MfsimName]));
          end;
          ArchiveBatchFile.Add(AFileName);
          if SimCount > 1 then
          begin
            ArchiveBatchFile.Add(Format('Copy /Y /A "%0:s" "%1:s"',
              [MfListName, ListFileName]));
          end;
        end;
      end
      else
      begin
        ArchiveBatchFile.Add(AFileName + ' ' +
          ArchiveQuoteFileName(ExtractFileName(FileName)) + ' /wait');
      end;

      AFileName :=  QuoteFileName(ExpandFileName(ModflowLocation));
      if Model.PestUsed then
      begin
        MoveAppToDirectory(ExpandFileName(ModflowLocation), ModelDirectory);
        AFileName := ExtractFileName(ModflowLocation);
      end;

      if (Model.ModelSelection <> msModflow2015) then
      begin
        ParamEstBatchFile.Add(AFileName + ' ' + QuoteFileName(ExtractFileName(FileName)) + ' /wait');
        WriteInstuctionsBatchFile.Add(AFileName + ' ' + QuoteFileName(ExtractFileName(FileName)) + ' /wait');
        InsFileName := ExtractFileName(ChangeFileExt(FileName, StrMf2005WriteIns));
        WriteInstuctionsBatchFile.Add(TCustomFileWriter.PestUtilityProgramPath(
          StrMf2005ObsExtractor, FileName)
          + ' ' + InsFileName);
        WriteInstuctionsBatchFile.Add('pause');
      end
      else
      begin
        SimCount := frmGoPhast.PhastModel.SimNameWriter.SimFileNameCount;
        for SimIndex := 0 to SimCount -1 do
        begin
          SimFileName := ExtractFileName(frmGoPhast.PhastModel.SimNameWriter.SimFileNames[SimIndex]);
          ListFileName := ChangeFileExt(SimFileName, '.lst');
          if SimCount > 1 then
          begin
            ParamEstBatchFile.Add(Format('Copy /Y /A "%0:s" "%1:s"', [SimFileName, MfsimName]));
          end;
          ParamEstBatchFile.Add(AFileName {+ ' /wait'});
          if SimCount > 1 then
          begin
            ParamEstBatchFile.Add(Format('Copy /Y /A "%0:s" "%1:s"', [MfListName, ListFileName]));
          end;
        end;
        WriteInstuctionsBatchFile.Add(AFileName {+ ' /wait'});
        InsFileName := ExtractFileName(ChangeFileExt(FileName, StrMf6WriteIns));
        WriteInstuctionsBatchFile.Add(TCustomFileWriter.PestUtilityProgramPath(
          StrMf6ObsExtractorexe, ExtractFileName(FileName))
          + ' ' + InsFileName);
        WriteInstuctionsBatchFile.Add('pause');
      end;

      if frmGoPhast.PhastModel.InterpSwiObsDefined
        and not frmGoPhast.PhastModel.PestUsed then
      begin
        SwiObsExtractorPath := ExtractFileDir(Application.ExeName);
        SwiObsExtractorPath := IncludeTrailingPathDelimiter(SwiObsExtractorPath);
        SwiObsExtractorPath := SwiObsExtractorPath + 'SwiObsExtractor.exe';
        Model.AddBinaryFile(SwiObsExtractorPath);
        SwiObsExtractorPath := '"'+ SwiObsExtractorPath + '" ';
        SwiObsExtInputFile := ChangeFileExt(FileName, '.swi_obsi');
//        Model.AddSwiObsExtInputFile(SwiObsExtInputFile);
        ALine := SwiObsExtractorPath + ExtractFileName(SwiObsExtInputFile);
        BatchFile.Add(ALine);
        ParamEstBatchFile.Add(ALine);
//        ALine := '..\..\bin\SwiObsExtractor.exe ' + ExtractFileName(SwiObsExtInputFile);
//        ArchiveBatchFile.Add(ALine);
      end;

      BatchFile.AddStrings(After);
      ParamEstBatchFile.AddStrings(After);
      ArchiveBatchFile.AddStrings(After);

      if ExportModpath then
      begin
        if Model is TPhastModel then
        begin
          if Model.ModflowPackages.ModPath.IsSelected then
          begin
            ALine := 'call RunModpath.bat';
            BatchFile.Add(ALine);
            ParamEstBatchFile.Add(ALine);
          end;
          if TPhastModel(Model).LgrUsed then
          begin
            for ChildIndex := 0 to TPhastModel(Model).ChildModels.Count - 1 do
            begin
              ChildModel := TPhastModel(Model).ChildModels[ChildIndex].ChildModel;
              if ChildModel <> nil then
              begin
                if ChildModel.ModflowPackages.ModPath.IsSelected then
                begin
                  ALine := 'call '+
                    ChildModel.ModelNameForDos + '_'
                    + 'RunModpath.bat';
                  BatchFile.Add(ALine);
                  ParamEstBatchFile.Add(ALine);
                end;
              end;
            end;
          end;
        end
        else
        begin
          ChildModel := Model as TChildModel;
          if ChildModel.ModflowPackages.ModPath.IsSelected then
          begin
            ALine := 'call '+
              ChildModel.ModelNameForDos + '_'
              + 'RunModpath.bat';
            BatchFile.Add(ALine);
            ParamEstBatchFile.Add(ALine);
          end;
        end;
      end;
      if ExportZoneBudget then
      begin
        if Model is TPhastModel then
        begin
          if Model.ModflowPackages.ZoneBudget.IsSelected then
          begin
            ALine := 'call RunZoneBudget.bat';
            BatchFile.Add(ALine);
            ParamEstBatchFile.Add(ALine);
          end;
          if TPhastModel(Model).LgrUsed then
          begin
            for ChildIndex := 0 to TPhastModel(Model).ChildModels.Count - 1 do
            begin
              ChildModel := TPhastModel(Model).ChildModels[ChildIndex].ChildModel;
              if ChildModel <> nil then
              begin
                if ChildModel.ModflowPackages.ZoneBudget.IsSelected then
                begin
                  ALine := 'call '+
                    ChildModel.ModelNameForDos + '_'
                    + 'RunZoneBudget.bat';
                  BatchFile.Add(ALine);
                  ParamEstBatchFile.Add(ALine);
                end;
              end;
            end;
          end;
        end
        else
        begin
          ChildModel := Model as TChildModel;
          if ChildModel.ModflowPackages.ZoneBudget.IsSelected then
          begin
            ALine := 'call '+
              ChildModel.ModelNameForDos + '_'
              + 'RunZoneBudget.bat';
            BatchFile.Add(ALine);
            ParamEstBatchFile.Add(ALine);
          end;
        end;
      end;

      if Model.PestStatus in [psObservations, psActive] then
      begin
        if Model.ModelSelection = msModflow2015 then
        begin
          ParamEstBatchFile.Add(TCustomFileWriter.PestUtilityProgramPath(
            StrMf6ObsExtractorexe, ExtractFileName(FileName))
            + ' ' + ChangeFileExt(ExtractFileName(FileName), '.Mf6ExtractValues'));
        end
        else
        begin
          ParamEstBatchFile.Add(TCustomFileWriter.PestUtilityProgramPath(
            StrMf2005ObsExtractor, ExtractFileName(FileName)) + ' '
            + ChangeFileExt(ExtractFileName(FileName), '.Mf2005ExtractValues'));
        end;
      end;
      if Model.PestStatus in [psObservations, psActive] then
      begin
        if (Model.ModelSelection <> msModflow2015) then
        begin
          InsFileName := ExtractFileName(ChangeFileExt(FileName, StrMf2005WriteIns));
          BatchFile.Add(TCustomFileWriter.PestUtilityProgramPath(
            StrMf2005ObsExtractor, FileName) + ' ' + InsFileName);
        end
        else
        begin
          InsFileName := ExtractFileName(ChangeFileExt(FileName, StrMf6WriteIns));
          BatchFile.Add(TCustomFileWriter.PestUtilityProgramPath(
            StrMf6ObsExtractorexe, FileName)
            + ' ' + InsFileName);
        end;
      end;
      if Model.PestUsed then
      begin
        PLPROC_Location := GetPLPROC_Location(FileName, Model);
        if Model.PestUsed then
        begin
          MoveAppToDirectory(PLPROC_Location, ModelDirectory);
          PLPROC_Location := ExtractFileName(PLPROC_Location);
        end;
        PLPROC_Location := Format('"%s" ', [PLPROC_Location]);
        for DSIndex := 0 to Model.DataArrayManager.DataSetCount - 1 do
        begin
          ADataArray := Model.DataArrayManager[DSIndex];
          if ADataArray.PilotPointsUsed then
          begin
            KrigFactorsFileName := ExtractFileName(ChangeFileExt(
              FileName, '.' + ADataArray.Name) + StrKrigfactorsscript);
            BatchFile.Add(PLPROC_Location + KrigFactorsFileName);
          end;
        end;
      end;
      // displaying the list file should be the last command before
      // exiting from the network drive.
      for ListFileIndex := 0 to ListFiles.Count - 1 do
      begin
        AddOpenListFileLine(ListFiles[ListFileIndex], OpenListFile,
          BatchFile, ProgramLocations);
      end;

      if NetworkDrive then
      begin
        BatchFile.Add('popd');
      end;
      BatchFile.Add('pause');
      ArchiveBatchFile.Add('pause');

      try
        BatchFile.SaveToFile(result);
      except
        begin
          Sleep(1000);
          BatchFile.SaveToFile(result);
        end;
      end;
      TFile.Copy(result, BackupRunModflow, True);
      ParamEstBatchFile.AddStrings(ShowElapsedTimeCmdLines);
      try
        ParamEstBatchFile.SaveToFile(ParamEstBatFileName);
      except
        begin
          Sleep(1000);
          ParamEstBatchFile.SaveToFile(ParamEstBatFileName);
        end;
      end;
      TFile.Copy(ParamEstBatFileName, BackupParamEstBatFileName, True);
      ArchiveBatchFile.SaveToFile(result + ArchiveExt);
      Model.AddModelInputFile(result + ArchiveExt);
      if Model.PestStatus in [psObservations, psActive] then
      begin
        WriteInstuctionsBatchFile.SaveToFile(WriteInstructionBatFileName);
      end;
    finally
      BatchFile.Free;
      ParamEstBatchFile.Free;
      ArchiveBatchFile.Free;
      WriteInstuctionsBatchFile.Free;
    end;
  finally
    SetCurrentDir(ADirectory);
  end;
end;

function WriteModPathBatchFileVersion6(ProgramLocations: TProgramLocations;
  FileName: string; ListFile: string; OpenListFile: boolean;
  EmbeddedExport: boolean; Model: TCustomModel): string;
var
  BatchFile: TStringList;
  ArchiveBatchFile: TStringList;
  ModPathLocation: string;
  AFileName: string;
  MpBatName: string;
  Modelname: string;
  OutputPrefix: string;
  ModelNameForDos: string;
  NetworkDrive: Boolean;
  ModelDirectory: string;
begin
  NetworkDrive := IsNetworkDrive(FileName);
  result := ExtractFileDir(FileName);
  ModelDirectory := result;
  if Model is TChildModel then
  begin
    ModelNameForDos := TChildModel(Model).ModelNameForDos + '_';
  end
  else
  begin
    ModelNameForDos := ''
  end;
  MpBatName := IncludeTrailingPathDelimiter(result)
    + ModelNameForDos + 'Mp.bat';

  BatchFile := TStringList.Create;
  ArchiveBatchFile := TStringList.Create;
  try
    if NetworkDrive then
    begin
      BatchFile.Add('pushd ' + ModelDirectory);
    end;
    AFileName := ExtractFileName(
      ChangeFileExt(FileName, TModpathSimFileWriter.Extension));

    ModPathLocation :=  QuoteFileName(ProgramLocations.ModPathLocationVersion6);
    Model.AddBinaryFile(ProgramLocations.ModPathLocationVersion6);
    BatchFile.Add(ModPathLocation + ' ' + AFileName);

    ModPathLocation := '..\..\bin\'
      + ExtractFileName(ProgramLocations.ModPathLocationVersion6);
    ArchiveBatchFile.Add(ModPathLocation + ' ' + AFileName);

    if NetworkDrive then
    begin
      BatchFile.Add('popd');
    end;
    BatchFile.SaveToFile(MpBatName);
    ArchiveBatchFile.SaveToFile(MpBatName + ArchiveExt);

    Model.ModpathInputFiles.Add(MpBatName + ArchiveExt)
  finally
    BatchFile.Free;
    ArchiveBatchFile.Free;
  end;

  result := IncludeTrailingPathDelimiter(result)
    + ModelNameForDos + 'RunModpath.Bat';

  FileName := ExtractFileName(FileName);
  BatchFile := TStringList.Create;
  try
    if NetworkDrive then
    begin
      BatchFile.Add('pushd ' + ModelDirectory);
    end;
    BatchFile.Add('call ' + QuoteFileName(ModelNameForDos + 'mp.bat') + ' /wait');
    AddOpenListFileLine(ListFile, OpenListFile, BatchFile, ProgramLocations);
    if NetworkDrive then
    begin
      BatchFile.Add('popd');
    end;
    if not EmbeddedExport then
    begin
      BatchFile.Add('pause');
    end;
    BatchFile.SaveToFile(result);

    BatchFile.Insert(0, 'if not exist "..\..\output\NUL" mkdir "..\..\output"');
    Modelname := ChangeFileExt(ExtractFileName(FileName), '');
    BatchFile.Insert(1, Format('if not exist "..\..\output\%0:s_Modpath\NUL" mkdir "..\..\output\%0:s_Modpath"', ['output.' + Modelname]));

    OutputPrefix := '..\..\output\output.' + Modelname + '_Modpath\';
    BatchFile.Add('move /Y MPATH6.LOG ' + OutputPrefix + 'MPATH6.LOG');

    if EmbeddedExport then
    begin
      BatchFile.Add('pause');
    end;
    BatchFile.SaveToFile(result + ArchiveExt);
    Model.ModpathInputFiles.Add(result + ArchiveExt)
  finally
    BatchFile.Free;
  end;

end;

function WriteModPathBatchFileVersion7(ProgramLocations: TProgramLocations;
  FileName: string; ListFile: string; OpenListFile: boolean;
  EmbeddedExport: boolean; Model: TCustomModel): string;
var
  BatchFile: TStringList;
  ArchiveBatchFile: TStringList;
  ModPathLocation: string;
  AFileName: string;
  MpBatName: string;
  Modelname: string;
  OutputPrefix: string;
  ModelNameForDos: string;
  NetworkDrive: Boolean;
  ModelDirectory: string;
begin
  NetworkDrive := IsNetworkDrive(FileName);
  result := ExtractFileDir(FileName);
  ModelDirectory := result;
  if Model is TChildModel then
  begin
    ModelNameForDos := TChildModel(Model).ModelNameForDos + '_';
  end
  else
  begin
    ModelNameForDos := ''
  end;
  MpBatName := IncludeTrailingPathDelimiter(result)
    + ModelNameForDos + 'Mp.bat';

  BatchFile := TStringList.Create;
  ArchiveBatchFile := TStringList.Create;
  try
    if NetworkDrive then
    begin
      BatchFile.Add('pushd ' + ModelDirectory);
    end;
    AFileName := ExtractFileName(
      ChangeFileExt(FileName, TModpathSimFileWriter.Extension));

    ModPathLocation :=  QuoteFileName(ProgramLocations.ModPathLocationVersion7);
    Model.AddBinaryFile(ProgramLocations.ModPathLocationVersion7);
    BatchFile.Add(ModPathLocation + ' ' + AFileName);

    ModPathLocation := '..\..\bin\'
      + ExtractFileName(ProgramLocations.ModPathLocationVersion7);
    ArchiveBatchFile.Add(ModPathLocation + ' ' + AFileName);

    if NetworkDrive then
    begin
      BatchFile.Add('popd');
    end;
    BatchFile.SaveToFile(MpBatName);
    ArchiveBatchFile.SaveToFile(MpBatName + ArchiveExt);

    Model.ModpathInputFiles.Add(MpBatName + ArchiveExt)
  finally
    BatchFile.Free;
    ArchiveBatchFile.Free;
  end;

  result := IncludeTrailingPathDelimiter(result)
    + ModelNameForDos + 'RunModpath.Bat';

  FileName := ExtractFileName(FileName);
  BatchFile := TStringList.Create;
  try
    if NetworkDrive then
    begin
      BatchFile.Add('pushd ' + ModelDirectory);
    end;
    BatchFile.Add('call ' + QuoteFileName(ModelNameForDos + 'mp.bat') + ' /wait');
    AddOpenListFileLine(ListFile, OpenListFile, BatchFile, ProgramLocations);
    if NetworkDrive then
    begin
      BatchFile.Add('popd');
    end;
    if not EmbeddedExport then
    begin
      BatchFile.Add('pause');
    end;
    BatchFile.SaveToFile(result);

    BatchFile.Insert(0, 'if not exist "..\..\output\NUL" mkdir "..\..\output"');
    Modelname := ChangeFileExt(ExtractFileName(FileName), '');
    BatchFile.Insert(1, Format('if not exist "..\..\output\%0:s_Modpath\NUL" mkdir "..\..\output\%0:s_Modpath"', ['output.' + Modelname]));

    OutputPrefix := '..\..\output\output.' + Modelname + '_Modpath\';
    BatchFile.Add('move /Y MPATH6.LOG ' + OutputPrefix + 'MPATH6.LOG');

    if EmbeddedExport then
    begin
      BatchFile.Add('pause');
    end;
    BatchFile.SaveToFile(result + ArchiveExt);
    Model.ModpathInputFiles.Add(result + ArchiveExt)
  finally
    BatchFile.Free;
  end;

end;

function WriteModPathBatchFile(ProgramLocations: TProgramLocations;
  FileName: string; ListFile: string; OpenListFile: boolean;
  const LargeBudgetFileResponse: string; EmbeddedExport: boolean;
  Model: TCustomModel): string;
var
  BatchFile: TStringList;
  AFileName: string;
  ResponseFile: string;
  MpBatName: string;
  Modelname: string;
  ModelNameForDos: string;
  NetworkDrive: Boolean;
  ModelDirectory: string;
begin
  NetworkDrive := IsNetworkDrive(FileName);
  result := ExtractFileDir(FileName);
  ModelDirectory := result;
  MpBatName := IncludeTrailingPathDelimiter(result);
  if Model is TChildModel then
  begin
    ModelNameForDos := TChildModel(Model).ModelNameForDos + '_';
  end
  else
  begin
    ModelNameForDos := ''
  end;
  MpBatName := MpBatName + ModelNameForDos + 'MpIn.txt';
  Model.AddModpathInputFile(MpBatName);

  BatchFile := TStringList.Create;
  try
    ResponseFile := ChangeFileExt(FileName, '.mprsp');
//    Model.AddModpathInputFile(ResponseFile);
    ResponseFile := ExtractFileName(ResponseFile);
    BatchFile.Add(ResponseFile);
    if LargeBudgetFileResponse <> '' then
    begin
      BatchFile.Add(LargeBudgetFileResponse);
    end;
    BatchFile.SaveToFile(MpBatName);
  finally
    BatchFile.Free;
  end;

  MpBatName := IncludeTrailingPathDelimiter(result)
    + ModelNameForDos + 'Mp.bat';

  BatchFile := TStringList.Create;
  try
    if NetworkDrive then
    begin
      BatchFile.Add('pushd ' + ModelDirectory);
    end;
    AFileName :=  QuoteFileName(ProgramLocations.ModPathLocation);
    BatchFile.Add(AFileName + ' <' + ModelNameForDos + 'MpIn.txt');
    if NetworkDrive then
    begin
      BatchFile.Add('popd');
    end;
    BatchFile.SaveToFile(MpBatName);
  finally
    BatchFile.Free;
  end;

  Model.AddModpathInputFile(MpBatName + ArchiveExt);
  BatchFile := TStringList.Create;
  try
    AFileName :=  ExtractFileName(ProgramLocations.ModPathLocation);
    Model.AddBinaryFile(ProgramLocations.ModPathLocation);
    AFileName := '..\..\bin\' + AFileName;
    BatchFile.Add(AFileName + ' <' + ModelNameForDos + 'MpIn.txt');
    BatchFile.SaveToFile(MpBatName + ArchiveExt);
  finally
    BatchFile.Free;
  end;

  result := IncludeTrailingPathDelimiter(result)
    + ModelNameForDos + 'RunModpath.Bat';
  Model.AddModpathInputFile(result + ArchiveExt);

  FileName := ExtractFileName(FileName);
  BatchFile := TStringList.Create;
  try
    BatchFile.Add('call ' + QuoteFileName(ModelNameForDos + 'mp.bat') + ' /wait');
    AddOpenListFileLine(ListFile, OpenListFile, BatchFile, ProgramLocations);
    if not EmbeddedExport then
    begin
      BatchFile.Add('pause');
    end;
    BatchFile.SaveToFile(result);
    BatchFile.Insert(0, 'if not exist "..\..\output\NUL" mkdir "..\..\output"');
    Modelname := ChangeFileExt(ExtractFileName(FileName), '');
    BatchFile.Insert(1, Format('if not exist "..\..\output\%0:s_Modpath\NUL" mkdir "..\..\output\%0:s_Modpath"', ['output.' + Modelname]));
    if EmbeddedExport then
    begin
      BatchFile.Add('pause');
    end;
    BatchFile.SaveToFile(result + ArchiveExt);
  finally
    BatchFile.Free;
  end;
end;

function WriteZoneBudgetBatchFile(Model: TCustomModel;
  FileName: string; OpenListFile, EmbeddedExport: boolean): string;
var
  BatchFile: TStringList;
  AFileName: string;
  InputFileName: string;
  ZoneBudBatName: string;
  ProgramLocations: TProgramLocations;
  ListFile: string;
  ZoneBudget: TZoneBudgetSelect;
  ZoneBudgetLocation: string;
  ADirectory: string;
  Modelname: string;
  ZBName: string;
  NetworkDrive: Boolean;
  ModelDirectory: string;
begin
  NetworkDrive := IsNetworkDrive(FileName);
  ModelDirectory := ExtractFileDir(FileName);
  ZoneBudget := Model.ModflowPackages.ZoneBudget;
  if ZoneBudget.ExportCSV2 then
  begin
    ListFile := ChangeFileExt(FileName, '.2.csv');
    Model.AddZoneBudgetOutputFile(ListFile);
  end;
  if ZoneBudget.ExportCSV then
  begin
    ListFile := ChangeFileExt(FileName, '.csv');
    Model.AddZoneBudgetOutputFile(ListFile);
  end;
  if ZoneBudget.ExportZBLST then
  begin
    if Model.ModelSelection = msModflow2015 then
    begin
      ListFile := ChangeFileExt(FileName, '.zb.lst');
    end
    else
    begin
      ListFile := ChangeFileExt(FileName, '.zblst');
    end;
    Model.AddZoneBudgetOutputFile(ListFile);
  end;

  ProgramLocations := Model.ProgramLocations;

  if Model.ModelSelection = msModflow2015 then
  begin
    ZoneBudgetLocation := ProgramLocations.ZoneBudgetLocationMf6;
  end
  else
  begin
    ZoneBudgetLocation := ProgramLocations.ZoneBudgetLocation;
  end;
  Model.AddBinaryFile(ZoneBudgetLocation);
//  Model.AddZoneBudgetInputFile(ZoneBudgetLocation);
  ZoneBudgetLocation := QuoteFileName(ZoneBudgetLocation);

  ADirectory := IncludeTrailingPathDelimiter(ExtractFileDir(FileName));

  if Model.ModelSelection = msModflow2015 then
  begin
    InputFileName := ChangeFileExt(FileName, TZoneBudgetNameFileWriter.Extension);
  end
  else
  begin
    InputFileName := ChangeFileExt(FileName, TZoneBudgetResponseFileWriter.Extension);
  end;
  InputFileName := ExtractFileName(InputFileName);

  if Model is TChildModel then
  begin
    ZoneBudBatName := ADirectory
      + TChildModel(Model).ModelNameForDos + '_'
      + 'ZB.bat';
  end
  else
  begin
    ZoneBudBatName := ADirectory + 'ZB.bat';
  end;
  BatchFile := TStringList.Create;
  try
    if NetworkDrive then
    begin
      BatchFile.Add('pushd ' + ModelDirectory);
    end;
    AFileName :=  ZoneBudgetLocation;
    Model.AddZoneBudgetInputFile(AFileName);
    if Model.ModelSelection = msModflow2015 then
    begin
      BatchFile.Add(AFileName + ' ' + InputFileName);
    end
    else
    begin
      BatchFile.Add(AFileName + ' <' + InputFileName);
    end;
    if NetworkDrive then
    begin
      BatchFile.Add('popd');
    end;
    BatchFile.SaveToFile(ZoneBudBatName);
  finally
    BatchFile.Free;
  end;

  if Model is TChildModel then
  begin
    ZoneBudBatName := ADirectory
      + TChildModel(Model).ModelNameForDos + '_'
      + 'RunZoneBudget.bat' + ArchiveExt;
    ZBName := TChildModel(Model).ModelNameForDos + '_ZB.bat';
  end
  else
  begin
    ZoneBudBatName := ADirectory + 'RunZoneBudget.bat' + ArchiveExt;
    ZBName := 'ZB.bat';
  end;
  Model.AddZoneBudgetInputFile(ZoneBudBatName);
  BatchFile := TStringList.Create;
  try
    BatchFile.Add('if not exist "..\..\output\NUL" mkdir "..\..\output"');
    Modelname := ChangeFileExt(ExtractFileName(FileName), '');
    BatchFile.Add(Format('if not exist "..\..\output\%0:s_Zonebudget\NUL" mkdir "..\..\output\%0:s_Zonebudget"', ['output.' + Modelname]));

    AFileName :=  '..\..\bin\' + ExtractFileName(ZoneBudgetLocation);
//    Model.AddZoneBudgetInputFile(AFileName);

    if Model.ModelSelection = msModflow2015 then
    begin
      BatchFile.Add(AFileName + ' ' + InputFileName);
    end
    else
    begin
      BatchFile.Add(AFileName + ' <' + InputFileName);
    end;

    BatchFile.Add('pause');
    BatchFile.SaveToFile(ZoneBudBatName);
  finally
    BatchFile.Free;
  end;

  // strip off archive extension.
  result := ChangeFileExt(ZoneBudBatName, '');
//  Model.AddZoneBudgetInputFile(result);

  FileName := ExtractFileName(FileName);
  BatchFile := TStringList.Create;
  try
    if NetworkDrive then
    begin
      BatchFile.Add('pushd ' + ModelDirectory);
    end;
    BatchFile.Add('call ' + ZBName + ' /wait');
    AddOpenListFileLine(ListFile, OpenListFile, BatchFile, ProgramLocations);
    if NetworkDrive then
    begin
      BatchFile.Add('popd');
    end;
    if not EmbeddedExport then
    begin
      BatchFile.Add('pause');
    end;
    BatchFile.SaveToFile(result);
  finally
    BatchFile.Free;
  end;
end;

{ TCustomModflowWriter }

class procedure TCustomModflowWriter.AddNameFileComment(const Comment: string);
begin
  Assert(CurrentNameFileWriter <> nil);
  CurrentNameFileWriter.NameFile.Add('# ' + Comment);
end;

var
  LimitValue: double;
  NegLimitValue: double;

procedure TCustomModflowWriter.WriteArrayValues(const LayerIndex: Integer; const DataArray: TDataArray);
var
  ColIndex: Integer;
  RowIndex: Integer;
  NeedNewLine: Boolean;
  UseGsflowFormat: Boolean;
  AValue: double;
begin
  UseGsflowFormat := (Model as TCustomModel).UseGsflowFormat;
  for RowIndex := 0 to DataArray.RowCount - 1 do
  begin
    NeedNewLine := False;
    if (Model.ModelSelection = msModflow2015) then
    begin
      WriteString('      ');
    end;
    for ColIndex := 0 to DataArray.ColumnCount - 1 do
    begin
      case DataArray.DataType of
        rdtDouble:
          begin
            // For very small numbers, limit values to the range of
            // single precision values.
            if DataArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              AValue := DataArray.RealData[LayerIndex, RowIndex, ColIndex];
            end
            else
            begin
              AValue := 0;
            end;

            if (NegLimitValue < AValue) and (AValue < LimitValue) then
            begin
              AValue := 0;
            end;
            WriteFloat(AValue);
          end;
        rdtInteger:
          begin
            WriteInteger(DataArray.IntegerData[LayerIndex, RowIndex, ColIndex]);
          end;
        rdtBoolean:
          begin
            if DataArray.BooleanData[LayerIndex, RowIndex, ColIndex] then
            begin
              WriteInteger(1);
            end
            else
            begin
              WriteInteger(0);
            end;
          end;
      else
        Assert(False);
      end;
      NeedNewLine := ((ColIndex + 1) mod 10) <> 0;
      if not NeedNewLine then
      begin
        if not UseGsflowFormat then
        begin
          NewLine;
        end;
        if (Model.ModelSelection = msModflow2015)
          and (ColIndex <> DataArray.ColumnCount - 1) then
        begin
          WriteString('      ');
        end;
      end;
    end;
    if NeedNewLine or UseGsflowFormat then
    begin
      NewLine;
    end;
  end;
end;

function TCustomModflowWriter.CheckArrayUniform(const LayerIndex: Integer;
  const DataArray: TDataArray): boolean;
var
//  ColIndex: Integer;
//  RowIndex: Integer;
  IntValue: Integer;
  BoolValue: Boolean;
  RealValue: Double;
//  FirstValueFound: Boolean;
begin
  result := CheckArrayUniform(LayerIndex, DataArray, IntValue, BoolValue,
    RealValue)
//  DataArray.Initialize;
//  RealValue := 0;
//  IntValue := 0;
//  BoolValue := False;
//  case DataArray.DataType of
//    rdtDouble:
//      begin
//        RealValue := DataArray.RealData[LayerIndex, 0, 0];
//      end;
//    rdtInteger:
//      begin
//        IntValue := DataArray.IntegerData[LayerIndex, 0, 0];
//      end;
//    rdtBoolean:
//      begin
//        BoolValue := DataArray.BooleanData[LayerIndex, 0, 0];
//      end;
//  else
//    Assert(False);
//  end;
//  FirstValueFound := False;
//  result := True;
//  for RowIndex := 0 to DataArray.RowCount - 1 do
//  begin
//    for ColIndex := 0 to DataArray.ColumnCount - 1 do
//    begin
//      if DataArray.IsValue[LayerIndex, RowIndex, ColIndex] then
//      begin
//        if FirstValueFound then
//        begin
//          case DataArray.DataType of
//            rdtDouble:
//              begin
//                result := DataArray.RealData[LayerIndex, RowIndex, ColIndex] = RealValue;
//              end;
//            rdtInteger:
//              begin
//                result := DataArray.IntegerData[LayerIndex, RowIndex, ColIndex] = IntValue;
//              end;
//            rdtBoolean:
//              begin
//                result := DataArray.BooleanData[LayerIndex, RowIndex, ColIndex] = BoolValue;
//              end;
//          else
//            Assert(False);
//          end;
//        end
//        else
//        begin
//          case DataArray.DataType of
//            rdtDouble:
//              begin
//                RealValue := DataArray.RealData[LayerIndex, RowIndex, ColIndex];
//              end;
//            rdtInteger:
//              begin
//                IntValue := DataArray.IntegerData[LayerIndex, RowIndex, ColIndex];
//              end;
//            rdtBoolean:
//              begin
//                BoolValue := DataArray.BooleanData[LayerIndex, RowIndex, ColIndex]
//              end;
//          else
//            Assert(False);
//          end;
//          FirstValueFound := True;
//        end;
//      end;
//      if not result then
//        break;
//    end;
//    if not result then
//      break;
//  end;
end;

function TCustomModflowWriter.ShouldCheckCell(LayerIndex, RowIndex,
  ColIndex: Integer): Boolean;
begin
  Result := True;
end;

function TCustomModflowWriter.CheckArray(const DataArray: TDataArray;
  const LayerIndex: integer; const ErrorOrWarningMessage: string;
  CheckMethod: TCheckValueMethod; CheckValue: double; ErrorType: TErrorType): boolean;
var
  RowIndex: Integer;
  ColIndex: Integer;
  Value: double;
  OtherValue1: double;
  OtherValue2: double;
  OkValue: boolean;
  Error: string;
  ActiveDataArray: TDataArray;
  Active: Boolean;
  OtherRow: Integer;
  OtherCol: Integer;
  function GetActive(Layer, Row, Column: integer): Boolean;
  begin
    if Model.ModelSelection = msModflow2015 then
    begin
      result := ActiveDataArray.IntegerData[Layer, Row, Column] > 0;
    end
    else
    begin
      result := ActiveDataArray.BooleanData[Layer, Row, Column];
    end;
  end;
begin

  case ErrorType of
    etError: frmErrorsAndWarnings.RemoveErrorGroup(Model, ErrorOrWarningMessage);
    etWarning: frmErrorsAndWarnings.RemoveWarningGroup(Model, ErrorOrWarningMessage);
  end;
  result := True;
  if Model.ModelSelection = msModflow2015 then
  begin
    ActiveDataArray := FModel.DataArrayManager.GetDataSetByName(K_IDOMAIN);
  end
  else
  begin
    ActiveDataArray := FModel.DataArrayManager.GetDataSetByName(rsActive);
  end;
  Assert(ActiveDataArray <> nil);
  ActiveDataArray.Initialize;
  DataArray.Initialize;
  for RowIndex := 0 to DataArray.RowCount -1 do
  begin
    for ColIndex := 0 to DataArray.ColumnCount - 1 do
    begin
      Value := 0;
      case DataArray.DataType of
        rdtDouble:
          begin
            Value := DataArray.RealData[LayerIndex, RowIndex, ColIndex];
          end;
        rdtInteger:
          begin
            Value := DataArray.IntegerData[LayerIndex, RowIndex, ColIndex];
          end;
        else Assert(False);
      end;
      Active := GetActive(LayerIndex, RowIndex, ColIndex);
      OkValue := True;
      OtherRow := 0;
      OtherCol := 0;
      if Active and ShouldCheckCell(LayerIndex, RowIndex, ColIndex) then
      begin
        case CheckMethod of
          cvmGreater: OkValue := Value > CheckValue;
          cvmGreaterEqual: OkValue := Value >= CheckValue;
          cvmEqual: OkValue := Value = CheckValue;
          cvmNotEqual: OkValue := Value <> CheckValue;
          cvmLessThan: OkValue := Value < CheckValue;
          cvmLessThanEqual: OkValue := Value <= CheckValue;
          cvmGradient:
            begin
              Assert(DataArray.DataType = rdtDouble);
              if Value <> 0 then
              begin
                if (RowIndex > 0)
                  and GetActive(LayerIndex, RowIndex-1, ColIndex) then
                begin
                  OtherValue1 := DataArray.RealData[LayerIndex, RowIndex-1, ColIndex];
                end
                else
                begin
                  OtherValue1 := Value
                end;
                if (ColIndex > 0)
                  and GetActive(LayerIndex, RowIndex, ColIndex-1) then
                begin
                  OtherValue2 := DataArray.RealData[LayerIndex, RowIndex, ColIndex-1];
                end
                else
                begin
                  OtherValue2 := Value
                end;
                OtherRow := RowIndex;
                OtherCol := ColIndex;
                if Abs(OtherValue2-Value) > Abs(OtherValue1-Value)  then
                begin
                  OtherValue1 := OtherValue2;
                  OtherCol := ColIndex-1
                end
                else
                begin
                  OtherRow := RowIndex-1;
                end;
                if OtherValue1 <> 0 then
                begin
                  if OtherValue1 > Value then
                  begin
                    OtherValue1 := OtherValue1/Value;
                  end
                  else
                  begin
                    OtherValue1 := Value/OtherValue1;
                  end;
                  OkValue := OtherValue1 <= CheckValue;
                end
                else
                begin
                  OkValue := True;
                end;
              end
              else
              begin
                OkValue := True;
              end;
            end;
          else Assert(False);
        end;
      end;
      if not OkValue then
      begin
        result := False;
        Error := Format(StrLayer0dRow1, [LayerIndex+1, RowIndex+1,ColIndex+1]);
        if CheckMethod = cvmGradient then
        begin
          Error := 'Between ' + Error + ' and '
            + Format(StrLayer0dRow1, [LayerIndex+1, OtherRow+1,OtherCol+1]);
        end;
        case ErrorType of
          etError: frmErrorsAndWarnings.AddError(Model, ErrorOrWarningMessage, Error);
          etWarning: frmErrorsAndWarnings.
            AddWarning(Model, ErrorOrWarningMessage, Error);
        end;
      end;
    end;
  end;
  Model.DataArrayManager.AddDataSetToCache(DataArray);
end;

function TCustomModflowWriter.CheckArrayUniform(const LayerIndex: Integer;
  const DataArray: TDataArray; var IntValue: Integer; var BoolValue: Boolean;
  var RealValue: Double): boolean;
var
  ColIndex: Integer;
  RowIndex: Integer;
  FirstValueFound: Boolean;
begin
  DataArray.Initialize;
  RealValue := 0;
  IntValue := 0;
  BoolValue := False;
//  case DataArray.DataType of
//    rdtDouble:
//      begin
//        RealValue := DataArray.RealData[LayerIndex, 0, 0];
//      end;
//    rdtInteger:
//      begin
//        IntValue := DataArray.IntegerData[LayerIndex, 0, 0];
//      end;
//    rdtBoolean:
//      begin
//        BoolValue := DataArray.BooleanData[LayerIndex, 0, 0];
//      end;
//  else
//    Assert(False);
//  end;
  FirstValueFound := False;
  result := True;
  for RowIndex := 0 to DataArray.RowCount - 1 do
  begin
    for ColIndex := 0 to DataArray.ColumnCount - 1 do
    begin
      if DataArray.IsValue[LayerIndex, RowIndex, ColIndex] then
      begin
        if FirstValueFound then
        begin
          case DataArray.DataType of
            rdtDouble:
              begin
                result := DataArray.RealData[LayerIndex, RowIndex, ColIndex] = RealValue;
              end;
            rdtInteger:
              begin
                result := DataArray.IntegerData[LayerIndex, RowIndex, ColIndex] = IntValue;
              end;
            rdtBoolean:
              begin
                result := DataArray.BooleanData[LayerIndex, RowIndex, ColIndex] = BoolValue;
              end;
          else
            Assert(False);
          end;
        end
        else
        begin
          case DataArray.DataType of
            rdtDouble:
              begin
                RealValue := DataArray.RealData[LayerIndex, RowIndex, ColIndex];
              end;
            rdtInteger:
              begin
                IntValue := DataArray.IntegerData[LayerIndex, RowIndex, ColIndex];
              end;
            rdtBoolean:
              begin
                BoolValue := DataArray.BooleanData[LayerIndex, RowIndex, ColIndex]
              end;
          else
            Assert(False);
          end;
          FirstValueFound := True;
        end;
      end;
      if not result then
        break;
    end;
    if not result then
      break;
  end;
end;

procedure TCustomFileWriter.AddUsedPestDataArray(ADataArray: TDataArray);
begin
  if not FPestDataArrays.ContainsKey(UpperCase(ADataArray.Name)) then
  begin
    FPestDataArrays.Add(UpperCase(ADataArray.Name), ADataArray);
  end;
end;

procedure TCustomFileWriter.ClearUsedPestDataArrays;
begin
  FPestDataArrays.Clear;
end;

procedure TCustomFileWriter.CloseFile;
begin
//  if FFileStream = FMainFileStream then
//  begin
//    FMainFileStream := nil;
//  end;
  FreeAndNil(FFileStream);
end;

procedure TCustomFileWriter.WriteLimit(Limit: TSutraLimitType);
begin
  case Limit of
    sltNone: WriteString(' "N"');
    sltFlow: WriteString(' "Q"');
    sltPressure: WriteString(' "P"');
    else Assert(False);
  end;
end;

function TCustomFileWriter.WritePestTemplateFormula(Value: double;
  PestParValue, PestSeriesValue: string; Method: TPestParamMethod;
  ACell: TValueCell; FixedLength: Integer; ChangeSign: Boolean): double;
var
  ACellLocation: TCellLocation;
  CellLocAddr: PCellLocation;
  ScreenObject: TObject;
begin
  if ACell <> nil then
  begin
    ACellLocation := ACell.CellLocation;
    CellLocAddr := Addr(ACellLocation);
    ScreenObject := ACell.ScreenObject as TScreenObject;
  end
  else
  begin
    CellLocAddr := nil;
    ScreenObject := nil;
  end;

  result := WritePestTemplateFormula(Value, PestParValue, PestSeriesValue, Method,
    CellLocAddr, ScreenObject, FixedLength, ChangeSign);

end;

function TCustomFileWriter.WritePestTemplateFormula(Value: double;
  PestParValue, PestSeriesValue: string; Method: TPestParamMethod;
  ACell: PCellLocation; AScreenObject: TObject; FixedLength: Integer;
  ChangeSign: Boolean): double;
var
  Formula: string;
  OldDecimalSeparator: Char;
begin
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  try
    Formula := GetPestTemplateFormula(Value, PestParValue, PestSeriesValue,
      Method, ACell, AScreenObject as TScreenObject, result);
    if ChangeSign then
    begin
      Formula := '-1*(' + Formula + ')';
      result := -result;
    end;
    if Formula <> '' then
    begin
      ExtendedTemplateFormula(Formula);
      WriteString(Formula);
    end
    else
    begin
      if FixedLength = 0 then
      begin
        WriteFloat(Value);
      end
      else if FixedLength = 10 then
      begin
        WriteF10Float(Value);
      end
      else if FixedLength = 15 then
      begin
        WriteF15Float(Value);
      end
      else
      begin
        Assert(False);
      end;
      Result := Value
    end;
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;

procedure TCustomFileWriter.WritePestTemplateFormulaOrValue(Value: double;
  PestParValue, PestSeriesValue: string; Method: TPestParamMethod;
  ACell: PCellLocation; AScreenObject: TObject; FixedLength: Integer;
  ChangeSign: Boolean);
begin
  if (PestParValue <> '') or (PestSeriesValue <> '') then
  begin
    FPestParamUsed := True;
  end;
  if (not WritingTemplate) or ((PestParValue = '')
    and (PestSeriesValue = '')) then
  begin
    if ChangeSign then
    begin
      Value := -Value
    end;
    if FixedLength = 0 then
    begin
      WriteFloat(Value);
    end
    else if FixedLength = 10 then
    begin
      WriteF10Float(Value);
    end
    else if FixedLength = 15 then
    begin
      WriteF15Float(Value);
    end
    else
    begin
      Assert(False);
    end;
  end
  else
  begin
    WritePestTemplateFormula(Value, PestParValue, PestSeriesValue, Method,
      ACell, AScreenObject, FixedLength, ChangeSign)
  end;
end;

procedure TCustomFileWriter.WritePestTemplateLine(AFileName: string);
var
  PValFileName: string;
  ALine: string;
begin
  // strip off .tpl extension to get input file name.
  PValFileName := ChangeFileExt(ExtractFileName(AFileName), '');
  Model.FilesToDelete.Add(PValFileName);
  PValFileName := ChangeFileExt(PValFileName, '');
  PValFileName := ChangeFileExt(PValFileName, '');
  PValFileName := ChangeFileExt(PValFileName, StrPvalExt);
  ALine := PestUtilityProgramPath(
    StrEnhancedTemplateProc, AFileName)
    + ' ' + ExtractFileName(AFileName) + ' ' + PValFileName;
  Model.PestTemplateLines.Add(ALine);

end;

procedure TCustomFileWriter.WriteExitSpec(ExitSpec: TSutraExitSpecificationMethod);
begin
  case ExitSpec of
    sexmRelative: WriteString(' "REL"');
    sexmDirect: WriteString(' "DIR"');
    else Assert(False);
  end;
end;



//procedure TCustomModflowWriter.UpdateExportTime;
//const
//  TenthSecond = 1/24/3600/10;
//begin
//  if (Now - FExportTime) > TenthSecond then
//  begin
//    FExportTime := Now;
//    Application.ProcessMessages;
//  end;
//end;

constructor TCustomModflowWriter.Create(AModel: TCustomModel; EvaluationType: TEvaluationType);
begin
  inherited Create(AModel, EvaluationType);
  if Model.ModelSelection = msModflow2015 then
  begin
    FArrayWritingFormat := awfModflow_6;
  end
  else
  begin
    FArrayWritingFormat := awfModflow;
  end;
end;

class function TCustomFileWriter.FileName(const AFileName: string): string;
begin
  result := ChangeFileExt(AFileName, Extension);
end;

constructor TCustomFileWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited Create;
  FTimeSeriesNames := TStringList.Create;
  FTimeSeriesNames.Sorted := True;
  FTimeSeriesNames.Duplicates := dupIgnore;
  FFileStreamList := TList<TFileStream>.Create;
  FPestDataArrays := TDictionary<string, TDataArray>.Create;
  FEvaluationType := EvaluationType;
  FModel := AModel;
end;

destructor TCustomFileWriter.Destroy;
begin
  FPestDataArrays.Free;
  FFileStreamList.Free;
  FTimeSeriesNames.Free;
  inherited;
end;

//class function TCustomFileWriter.Extension: string;
//begin
//  Result := '';
//end;
//
class function TCustomFileWriter.FixedFormattedInteger(const Value,
  Width: integer): string;
var
  Index : integer;
  PadIndex : integer;
begin
  for Index := Width downto 1 do
  begin
    result := Format(' %.*d', [Index, Value]);
    if Value < 0 then
    begin
      while (Length(result) > 3) and (result[3] = '0') do
      begin
        Delete(result, 3, 1);
      end;
    end
    else
    begin
      while (Length(result) > 2) and (result[2] = '0') do
      begin
        Delete(result, 2, 1);
      end;
    end;

    if Length(result) <= Width then
    begin
      for PadIndex := 0 to Width - Length(result) -1 do
      begin
        result := ' ' + result;
      end;
      break;
    end;
  end;
end;

class function TCustomFileWriter.FixedFormattedReal(const Value: double;
  const Width: integer): string;
var
  Index : integer;
  PadIndex : integer;
  OldDecimalSeparator: Char;
begin
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  try
    for Index := Width downto 1 do
    begin
      result := Format(' %.*g', [Index, Value]);
      if Length(result) <= Width then
      begin
        for PadIndex := 0 to Width - Length(result) -1 do
        begin
          result := ' ' + result;
        end;
        break;
      end;
    end;
    result := FortranDecimal(result);
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;

class function TCustomFileWriter.FortranDecimal(
  NumberString: string): string;
begin
  if FormatSettings.DecimalSeparator = '.' then
  begin
    result := NumberString;
  end
  else
  begin
    result := StringReplace(NumberString, FormatSettings.DecimalSeparator, '.',
      [rfReplaceAll]);
  end;
end;

class function TCustomFileWriter.FreeFormattedReal(
  const Value: double): string;
begin
  result := FortranDecimal(Format('%.13e ', [Value]));
  while Length(result) < 21 do
  begin
    result := ' ' + result
  end;
end;

function TCustomFileWriter.GetPestNonTransientTemplateFormula(
  DataArray: TDataArray; Layer, Row, Col: Integer): string;
var
  TemplateCharacter: string;
  ArrayTemplateCharacter: string;
  Value: Double;
  CellValueReplacement: string;
  OldDecimalSeparator: Char;
  procedure GetCellData(out CellValueReplacement: string; out Value: Double);
  var
    ModifierValue: Double;
    LocalLayer: Integer;
  begin
    ModifierValue := 0;
    if DataArray.Orientation = dsoTop then
    begin
      Layer := 0;
    end;
    Value := DataArray.RealData[Layer, Row, Col];
    if DataArray.PestParametersUsed then
    begin
      ModifierValue := DataArray.RealData[Layer, Row, Col];

      if Model.ModelSelection in ModflowSelection then
      begin
        LocalLayer := Model.DataSetLayerToModflowLayer(Layer);
      end
      else
      begin
        LocalLayer := Layer + 1;
      end;
      CellValueReplacement := Format(' %0:s                    %1:s[%2:d, %3:d, %4:d]%0:s',
        [ArrayTemplateCharacter, DataArray.Name,
        LocalLayer, Row+1, Col+1]);
    end
    else
    begin
      CellValueReplacement := ''
    end;

    if CellValueReplacement <> '' then
    begin
      if ModifierValue = 0 then
      begin
        Value := 0;
      end
      else
      begin
        Value := Value/ModifierValue;
      end;
    end;
  end;
begin
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  try
    TemplateCharacter := Model.PestProperties.TemplateCharacter;
    ArrayTemplateCharacter := Model.PestProperties.ArrayTemplateCharacter;

    result := '';
    begin
      GetCellData(CellValueReplacement, Value);

      if (CellValueReplacement <> '') then
      begin
        result := Format('%0:g * %1:s',
          [Value, CellValueReplacement]);
      end;
    end
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;

function TCustomFileWriter.GetPestParamFormula(Value: double;
  PestParName: string): string;
var
  Param: TModflowSteadyParameter;
  TemplateCharacter: string;
begin
  if PestParName = '' then
  begin
    result := FortranFloatToStr(Value)
  end
  else
  begin
    Param := Model.GetPestParameterByName(PestParName);
    if Param <> nil then
    begin
      Param.IsUsedInTemplate := True;
      TemplateCharacter := Model.PestProperties.TemplateCharacter;
      result := Format(' %0:s                    %1:s%0:s',
        [TemplateCharacter, Param.ParameterName])
    end
    else
    begin
      result := FortranFloatToStr(Value)
    end;
  end;
end;

function TCustomFileWriter.GetPestTemplateFormula(Value: double; PestParValue,
  PestSeriesValue: string; Method: TPestParamMethod; ACell: PCellLocation;
  const AScreenObject: TObject; var ModifiedValue: double): string;
var
  TemplateCharacter: string;
  ArrayTemplateCharacter: string;
  SeriesReplacement: String;
  CellValueReplacement: string;
  Operation: String;
  OldDecimalSeparator: Char;
  procedure GetSeriesData(out SeriesReplacement: string; var Value: double);
  var
    LocalLayer: Integer;
    SeriesValue: double;
    SeriesParam: TModflowSteadyParameter;
    SeriesDataArray: TDataArray;
    DataArrayLayer: Integer;
  begin
    SeriesValue := 0;
    SeriesParam := Model.GetPestParameterByName(PestSeriesValue);
    if SeriesParam <> nil then
    begin
      SeriesParam.UsedDirectly := True;
      SeriesParam.IsUsedInTemplate := True;
	    SeriesValue := SeriesParam.Value;
      Model.WritePValAndTemplate(SeriesParam.ParameterName, SeriesParam.Value,
        SeriesParam, True);

      SeriesReplacement := Format(' %0:s                    %1:s%0:s',
        [TemplateCharacter, PestSeriesValue])
    end
    else
    begin
      Assert(ACell <> nil);
      SeriesDataArray := Model.DataArrayManager.
        GetDataSetByName(PestSeriesValue);
      if SeriesDataArray <> nil then
      begin
        if SeriesDataArray.PestParametersUsed then
        begin
          if SeriesDataArray.Orientation = dsoTop then
          begin
            DataArrayLayer := 0;
          end
          else
          begin
            DataArrayLayer := ACell.Layer;
          end;
          SeriesValue := SeriesDataArray.RealData[
            DataArrayLayer, ACell.Row, ACell.Column];
          if Model.ModelSelection in ModflowSelection then
          begin
            LocalLayer := Model.DataSetLayerToModflowLayer(DataArrayLayer);
          end
          else
          begin
            LocalLayer := DataArrayLayer + 1;
          end;

          SeriesReplacement := Format(' %0:s                    %1:s[%2:d, %3:d, %4:d]%0:s',
            [ArrayTemplateCharacter, SeriesDataArray.Name,
            LocalLayer, ACell.Row+1, ACell.Column+1]);
        end
        else
        begin
          SeriesReplacement := '';
          SeriesValue := 0;
        end;
      end
      else
      begin
        if AScreenObject <> nil then
        begin
//          Assert(AScreenObject <> nil);
  //        AScreenObject := ACell.ScreenObject as TScreenObject;
          frmErrorsAndWarnings.AddError(Model, 'Unrecognized PEST parameter or data set',
            Format('%0:s was not recognized in %1:s',
            [PestSeriesValue, (AScreenObject as TScreenObject).Name]), AScreenObject);
        end
        else
        begin
          frmErrorsAndWarnings.AddError(Model, 'Unrecognized PEST parameter or data set',
            Format('%0:s was not recognized in %1:s',
            [PestSeriesValue, '(Unknown)']));
        end;
      end;
	  end;

    if SeriesReplacement <> '' then
    begin
      Case Method of
        ppmAdd:
          begin
            Value := Value - SeriesValue;
          end;
        ppmMultiply:
          begin
            if SeriesValue = 0 then
            begin
              Value := 0;
            end
            else
            begin
              Value := Value/SeriesValue;
            end;
          end;
        else
          Assert(False);
      end;
    end;
  end;
  procedure GetCellData(out CellValueReplacement: string; var Value: double);
  var
    ModifierValue: Double;
    LocalLayer: Integer;
    Param: TModflowSteadyParameter;
    DataArray: TDataArray;
    DataArrayLayer: Integer;
  begin
    ModifierValue := 0;
    Param := Model.GetPestParameterByName(PestParValue);
    if Param <> nil then
    begin
      Param.UsedDirectly := True;
      Param.IsUsedInTemplate := True;
	    ModifierValue := Param.Value;
      Model.WritePValAndTemplate(Param.ParameterName, Param.Value, Param, True);
      CellValueReplacement := Format(' %0:s                    %1:s%0:s',
        [TemplateCharacter, PestParValue]);
    end
    else
    begin
      Assert(ACell <> nil);
      DataArray := Model.DataArrayManager.GetDataSetByName(PestParValue);
      if DataArray <> nil then
      begin
        if DataArray.PestParametersUsed then
        begin
          if DataArray.Orientation = dsoTop then
          begin
            DataArrayLayer := 0;
          end
          else
          begin
            DataArrayLayer := ACell.Layer;
          end;
          ModifierValue := DataArray.RealData[DataArrayLayer, ACell.Row, ACell.Column];

          if Model.ModelSelection in ModflowSelection then
          begin
            LocalLayer := Model.DataSetLayerToModflowLayer(DataArrayLayer);
          end
          else
          begin
            LocalLayer := DataArrayLayer + 1;
          end;

          CellValueReplacement := Format(' %0:s                    %1:s[%2:d, %3:d, %4:d]%0:s',
            [ArrayTemplateCharacter, DataArray.Name,
            LocalLayer, ACell.Row+1, ACell.Column+1]);
        end
        else
        begin
          CellValueReplacement := ''
        end;
      end
      else
      begin
        if AScreenObject <> nil then
        begin
          frmErrorsAndWarnings.AddError(Model, 'Unrecognized PEST parameter or data set',
            Format('%0:s was not recognized in %1:s',
            [PestParValue, (AScreenObject as TScreenObject).Name]), AScreenObject);
        end
        else
        begin
          frmErrorsAndWarnings.AddError(Model, 'Unrecognized PEST parameter or data set',
            Format('%0:s was not recognized in %1:s',
            [PestParValue, '(Unknown)']));
        end;
      end;
	  end;

    if CellValueReplacement <> '' then
    begin
      if ModifierValue = 0 then
      begin
        Value := 0;
      end
      else
      begin
        Value := Value/ModifierValue;
      end;
    end;
  end;
begin
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  try
    TemplateCharacter := Model.PestProperties.TemplateCharacter;
    ArrayTemplateCharacter := Model.PestProperties.ArrayTemplateCharacter;
    Case Method of
      ppmAdd:
        begin
          Operation := '+';
        end;
      ppmMultiply:
        begin
          Operation := '*';
        end;
      else
        Assert(False);
    end;

    result := '';
    if (PestParValue <> '') and (PestSeriesValue <> '') then
    begin
      GetSeriesData(SeriesReplacement, Value);
      GetCellData(CellValueReplacement, Value);
      ModifiedValue := Value;

      if (CellValueReplacement <> '') and (SeriesReplacement <> '') then
      begin
        result := Format('(%0:g * %1:s) %2:s %3:s',
          [Value, CellValueReplacement, Operation, SeriesReplacement]);
      end
      else if (CellValueReplacement <> '') then
      begin
        result := Format('%0:g * %1:s',
          [Value, CellValueReplacement]);
      end
      else if (SeriesReplacement <> '') then
      begin
        result := Format('%0:g  %1:s %2:s',
          [Value, Operation, SeriesReplacement]);
      end;
    end
    else if (PestParValue <> '') then
    begin
      GetCellData(CellValueReplacement, Value);
      ModifiedValue := Value;
      if (CellValueReplacement <> '') then
      begin
        result := Format('%0:g * %1:s',
          [Value, CellValueReplacement]);
      end;
    end
    else
    begin
      Assert(PestSeriesValue <> '');
      GetSeriesData(SeriesReplacement, Value);
      ModifiedValue := Value;
      if SeriesReplacement <> '' then
      begin
        result := Format('%0:g  %1:s %2:s',
          [Value, Operation, SeriesReplacement]);
      end;
    end;
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;

function TCustomFileWriter.GetPestTemplateFormulaOrValue(Value: double;
  PestParValue, PestSeriesValue: string; Method: TPestParamMethod;
  ACell: PCellLocation; const AScreenObject: TObject): string;
var
  ModifiedValue: double;
begin
  if (PestParValue = '') and (PestSeriesValue = '') then
  begin
    result := FortranFloatToStr(Value);
  end
  else
  begin
    result := GetPestTemplateFormula(Value, PestParValue, PestSeriesValue,
      Method, ACell, AScreenObject, ModifiedValue);
  end;
end;

function TCustomFileWriter.GetUsedPestDataArrays: TArray<TDataArray>;
var
  Index: Integer;
  Value: TDataArray;
begin
  SetLength(result, FPestDataArrays.Count);
  Index := 0;
  for Value in FPestDataArrays.Values do
  begin
    result[Index] := Value;
    Inc(Index);
  end;
end;

function TCustomFileWriter.GwtFileName(const AFileName: string;
  SpeciesIndex: Integer): string;
var
  ASpeciesName: String;
begin
  ASpeciesName := '.' + Model.MobileComponents[SpeciesIndex].Name;
  result := ChangeFileExt(AFileName, ASpeciesName) + Extension;
end;

procedure TCustomFileWriter.NewLine;
begin
  WriteString(sLineBreak);
end;

procedure TCustomFileWriter.OpenFile(const FileName: string);
begin
  FFileStream:= TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
end;

procedure TCustomModflowWriter.WriteArray(const DataArray: TDataArray;
  const LayerIndex: integer; const Comment: string;
  NoValueAssignedAnnotation: string; const MF6_Arrayname: string;
  CacheArray: Boolean = True; ShouldWriteHeader: Boolean = True;
  ForceFullArray: Boolean = False);
var
  RealValue: double;
  IntValue: integer;
  BoolValue: Boolean;
  Uniform: boolean;
  UnassignedAnnotation: Boolean;
  RowIndex: integer;
  ColIndex: integer;
  LayerArrayWriter: TLayerArrayWriter;
  ArrayFileName: string;
  ArraysPos: Integer;
begin
  Assert(DataArray <> nil);
  if Comment <> '' then
  begin
    frmProgressMM.AddMessage(Format(StrWritingS, [Comment]));
  end
  else if DataArray.Name <> '' then
  begin
    frmProgressMM.AddMessage(Format(StrWriting0sFor,
      [DataArray.Name, LayerIndex+1]));
  end
  else
  begin
    frmProgressMM.AddMessage(StrWritingArray);
  end;

  if DataArrayUsesPestParameters(DataArray) then
  begin
    LayerArrayWriter := TLayerArrayWriter.Create(Model, FEvaluationType);
    try
      Assert(FInputFileName <> '');
      ArrayFileName := LayerArrayWriter.WriteFile(FInputFileName,
        DataArray, LayerIndex);
    finally
      LayerArrayWriter.Free;
    end;
    Model.AddModelInputFile(ArrayFileName);
    ArraysPos := Pos(StrArrays, ArrayFileName);
    if ArraysPos > 0 then
    begin
      ArrayFileName := Copy(ArrayFileName, ArraysPos, MAXINT);
    end;
    DataArray.PestArrayFileNames.Add(ArrayFileName);

    if (Model.ModelSelection = msModflow2015) and (MF6_ArrayName <> '') then
    begin
      WriteString('  ');
      WriteString(MF6_ArrayName);
      begin
        WriteString(' LAYERED');
      end;
      WriteString(' IPRN');
      WriteInteger(IPRN_Real);
      NewLine;
    end;

    WriteString('  OPEN/CLOSE ');
    WriteString(ArrayFileName);
    if Model.ModelSelection <> msModflow2015 then
    begin
      WriteString(' 1.0 (Free) ');
      WriteInteger(IPRN_Real)
    end
    else
    begin
      if Model.ModflowOutputControl.PrintInputArrays then
      begin
        WriteString(' IPRN');
        if DataArray.DataType = rdtDouble then
        begin
          WriteInteger(IPRN_Real)
        end
        else
        begin
          WriteInteger(IPRN_Integer)
        end;
      end;
    end;
    NewLine;
  end
  else
  begin
    Uniform := CheckArrayUniform(LayerIndex, DataArray, IntValue, BoolValue, RealValue);
    if Uniform and not ForceFullArray then
    begin
      case DataArray.DataType of
        rdtDouble:
          begin
//            RealValue := DataArray.RealData[LayerIndex, 0, 0];
            WriteConstantU2DREL(Comment, RealValue, matStructured, MF6_Arrayname);
          end;
        rdtInteger:
          begin
//            IntValue := DataArray.IntegerData[LayerIndex, 0, 0];
            WriteConstantU2DINT(Comment, IntValue, matStructured, MF6_Arrayname);
          end;
        rdtBoolean:
          begin
//            BoolValue := DataArray.BooleanData[LayerIndex, 0, 0];
            IntValue := Ord(BoolValue);
            WriteConstantU2DINT(Comment, IntValue, matStructured, MF6_Arrayname);
          end;
        else Assert(False);
      end;
      UnassignedAnnotation := True;
      for RowIndex := 0 to DataArray.RowCount - 1 do
      begin
        if not UnassignedAnnotation then
        begin
          break;
        end;
        for ColIndex := 0 to DataArray.ColumnCount - 1 do
        begin
          if DataArray.IsValue[LayerIndex, RowIndex, ColIndex] then
          begin
            if DataArray.Annotation[LayerIndex, RowIndex, ColIndex]
              <> NoValueAssignedAnnotation then
            begin
              UnassignedAnnotation := False;
              Break;
            end;
          end;
        end;
      end;
      if UnassignedAnnotation then
      begin
        if DataArray.Name <> '' then
        begin
          frmErrorsAndWarnings.AddWarning(Model, StrNoValuesAssignedT,
            DataArray.Name);
        end
        else
        begin
          frmErrorsAndWarnings.AddWarning(Model, StrNoValuesAssignedT,
            Comment);
        end;
      end;
    end
    else
    begin
      if ShouldWriteHeader then
      begin
        WriteHeader(DataArray, Comment, MF6_Arrayname);
      end;
      WriteArrayValues(LayerIndex, DataArray);
    end;
  end;
  if CacheArray then
  begin
    FModel.DataArrayManager.AddDataSetToCache(DataArray);
  end;
end;

procedure TCustomFileWriter.WriteArray(const DataArray: TDataArray;
  const LayerIndex: integer; const Comment: string;
  NoValueAssignedAnnotation: string; const MF6_Arrayname: string;
  CacheArray: Boolean = True; ShouldWriteHeader: Boolean = True;
  ForceFullArray: Boolean = False);
var
  LayerArrayWriter: TLayerArrayWriter;
  ArrayFileName: string;
  ArraysPos: Integer;
begin
  LayerArrayWriter := TLayerArrayWriter.Create(Model, FEvaluationType);
  try
    Assert(FInputFileName <> '');
    ArrayFileName := LayerArrayWriter.WriteFile(FInputFileName,
      DataArray, LayerIndex);
  finally
    LayerArrayWriter.Free;
  end;
  Model.AddModelInputFile(ArrayFileName);
  ArraysPos := Pos(StrArrays, ArrayFileName);
  if ArraysPos > 0 then
  begin
    ArrayFileName := Copy(ArrayFileName, ArraysPos, MAXINT);
  end;
  DataArray.PestArrayFileNames.Add(ArrayFileName);
end;

function TCustomFileWriter.WriteArraysFile(TemplateFileName: string): string;
var
  ArraysFile: TStringList;
  DataArrays: TArray<TDataArray>;
  DataArrayIndex: Integer;
  ADataArray: TDataArray;
  ALine: WideString;
  TempFile: string;
  LayerIndex: Integer;
//  ModflowWriter: TCustomModflowWriter;
  ParameterZoneWriter: TParameterZoneWriter;
begin
  if FPestDataArrays.Count = 0 then
  begin
    result := '';
  end
  else
  begin
//    ModflowWriter := nil;
//    TemplateFileName :=   ChangeFileExt(TemplateFileName, '');
    result := ExpandFileName(ChangeFileExt(TemplateFileName, '.txt'));
    ArraysFile := TStringList.Create;
    try
      ArraysFile.Add(Model.PestProperties.ArrayTemplateCharacter);
      DataArrays := GetUsedPestDataArrays;
      for DataArrayIndex := 0 to Length(DataArrays) - 1 do
      begin
        ADataArray := DataArrays[DataArrayIndex];
        if ADataArray.PestArrayFileNames.Count = 0 then
        begin
          ParameterZoneWriter := TParameterZoneWriter.Create(Model, etExport);
          try
            TempFile := ChangeFileExt(FNameOfFile, '');
            ParameterZoneWriter.WriteFile(TempFile, ADataArray, ADataArray.Name);
          finally
            ParameterZoneWriter.Free;
          end;

          TempFile := ChangeFileExt(FNameOfFile, '');
          TempFile := ChangeFileExt(TempFile, '') + '.' + ADataArray.Name;
          OpenTempFile(TempFile);
          try
//            if ModflowWriter = nil then
//            begin
//              ModflowWriter := self as TCustomModflowWriter;
//            end;
            for LayerIndex := 0 to ADataArray.LayerCount - 1 do
            begin
              {ModflowWriter.}WriteArray(ADataArray,
                LayerIndex, '', '', ADataArray.Name);
            end;
          finally
            CloseTempFile;
          end;
        end;
        ALine := Format('%0:s[%1:d, %2:d, %3:d] ', [ADataArray.Name,
          ADataArray.LayerCount, ADataArray.RowCount, ADataArray.ColumnCount])
          + ADataArray.PestArrayFileNames.DelimitedText;
        ArraysFile.Add(ALine);
      end;

      ArraysFile.SaveToFile(result)
    finally
      ArraysFile.Free;
    end;
  end;
end;

procedure TCustomFileWriter.ExtendedTemplateFormula(var Formula: string);
begin
  Formula := Format(StrExtendedTemplateFormat,
    [Model.PestProperties.ExtendedTemplateCharacter, Formula]);
end;

procedure TCustomFileWriter.WriteCommentLine(const Comment: string);
begin
  WriteString('# ' + Comment);
  NewLine;
end;

procedure TCustomModflowWriter.WriteCommentLines(const Lines: TStrings);
var
  LineIndex: Integer;
begin
  for LineIndex := 0 to Lines.Count - 1 do
  begin
    WriteCommentLine(Lines[LineIndex]);
  end;
end;

procedure TCustomModflowWriter.WriteCrossSectionArray(
  const DataArray: TDataArray; const Comment: string;
  const MF6_ArrayName: string);
var
  LayerIndex, ColIndex: Integer;
  NeedNewLine: boolean;
begin
  DataArray.Initialize;
  WriteHeader(DataArray, Comment, MF6_ArrayName);
  for LayerIndex := 0 to DataArray.LayerCount -1 do
  begin
    NeedNewLine := False;
    if Model.IsLayerSimulated(LayerIndex) then
    begin
      for ColIndex := 0 to DataArray.ColumnCount - 1 do
      begin
        case DataArray.DataType of
          rdtDouble:
            begin
              WriteFloat(DataArray.RealData[LayerIndex, 0, ColIndex]);
            end;
          rdtInteger:
            begin
              WriteInteger(DataArray.IntegerData[LayerIndex, 0, ColIndex]);
            end;
          rdtBoolean:
            begin
              if DataArray.BooleanData[LayerIndex, 0, ColIndex] then
              begin
                WriteInteger(1);
              end
              else
              begin
                WriteInteger(0);
              end;
            end;
          else Assert(False);
        end;
        NeedNewLine := ((ColIndex + 1) mod 10) <> 0;
        if not NeedNewLine then
        begin
          NewLine;
        end
      end;
    end;
    if NeedNewLine then
    begin
      NewLine;
    end;
  end;
  Model.DataArrayManager.AddDataSetToCache(DataArray);
end;

procedure TCustomModflowWriter.WriteHeader(const DataArray: TDataArray;
  const Comment: string; const MF6_ArrayName: string);
begin
  WriteHeader(DataArray.DataType, Comment, MF6_ArrayName);
//  case DataArray.DataType of
//    rdtDouble:
//      begin
//        WriteU2DRELHeader(Comment, matStructured, MF6_ArrayName);
//      end;
//    rdtInteger, rdtBoolean:
//      begin
//        WriteU2DINTHeader(Comment, matStructured, MF6_ArrayName);
//      end;
//  else
//    Assert(False);
//  end;
end;

procedure TCustomFileWriter.WriteF10Float(const Value: double);
begin
  WriteString(FixedFormattedReal(Value, 10));
end;

procedure TCustomFileWriter.WriteF15Float(const Value: double);
begin
  WriteString(FixedFormattedReal(Value, 15));
end;

procedure TCustomFileWriter.WriteFloat(const Value: double);
begin
  WriteString(' ' + FreeFormattedReal(Value));
end;

procedure TCustomFileWriter.WriteFloatCondensed(const Value: double);
var
  AValue: string;
  OldValueLength: Integer;
begin
  AValue := FreeFormattedReal(Value);
  repeat
    OldValueLength := Length(AValue);
    AValue := StringReplace(AValue, '00E', 'E', [rfReplaceAll, rfIgnoreCase]);
  until (OldValueLength = Length(AValue));
  WriteString(' ' + AValue);
end;

procedure TCustomFileWriter.WriteFreeInteger(const Value: integer);
var
  ValueAsString: string;
begin
  ValueAsString := ' ' + IntToStr(Value);
  WriteString(ValueAsString);
end;

procedure TCustomFileWriter.WriteI10Integer(const Value: integer;
  const ErrorID: string);
const
  MaxCharacters = 10;
var
  ValueAsString: string;
begin
  ValueAsString := IntToStr(Value);
  if Length(ValueAsString) > MaxCharacters then
  begin
    frmErrorsAndWarnings.AddError(Model, StrValueTooLong,
      Format(StrSIsTooLong10, [ErrorID]));
  end
  else
  begin
    while Length(ValueAsString) < MaxCharacters do
    begin
      ValueAsString := ' ' + ValueAsString;
    end;
  end;
  WriteString(ValueAsString);
end;

procedure TCustomFileWriter.WriteI2Integer(const Value: integer;
  const ErrorID: string);
const
  MaxCharacters = 2;
var
  ValueAsString: string;
begin
  ValueAsString := IntToStr(Value);
  if Length(ValueAsString) > MaxCharacters then
  begin
    frmErrorsAndWarnings.AddError(Model, StrValueTooLong,
      Format(StrSIsTooLong2, [ErrorID]));
  end
  else
  begin
    while Length(ValueAsString) < MaxCharacters do
    begin
      ValueAsString := ' ' + ValueAsString;
    end;
  end;
  WriteString(ValueAsString);
end;

procedure TCustomFileWriter.WriteI5Integer(const Value: integer;
  const ErrorID: string);
const
  MaxCharacters = 5;
var
  ValueAsString: string;
begin
  ValueAsString := IntToStr(Value);
  if Length(ValueAsString) > MaxCharacters then
  begin
    frmErrorsAndWarnings.AddError(Model, StrValueTooLong,
      Format(StrSIsTooLong10, [ErrorID]));
  end
  else
  begin
    while Length(ValueAsString) < MaxCharacters do
    begin
      ValueAsString := ' ' + ValueAsString;
    end;
  end;
  WriteString(ValueAsString);
end;

procedure TCustomModflowWriter.WriteIface(const Value: TIface);
var
  IFACE: integer;
begin
  IFACE := Ord(Value) - 2;
  WriteInteger(IFACE);
end;

procedure TCustomFileWriter.WriteInteger(const Value: integer);
var
  ValueAsString: string;
begin
  ValueAsString := ' ' + IntToStr(Value);
  while Length(ValueAsString) < 6 do
  begin
    ValueAsString := ' ' + ValueAsString;
  end;

  WriteString(ValueAsString);
end;

procedure TCustomFileWriter.WriteString(const Value: AnsiString);
begin
  if Length(Value) > 0 then
  begin
    FFileStream.Write(Value[1], Length(Value)*SizeOf(AnsiChar));
//    UpdateExportTime;
  end;
end;

procedure TCustomFileWriter.WriteString(const Value: String);
begin
  WriteString(AnsiString(Value));
end;

procedure TCustomModflowWriter.WriteToGwtNameFile(const Ftype: string;
  FileName: string; SpeciesIndex: Integer; PackageName: string = '');
var
  Mf6GwtNameWriters: TMf6GwtNameWriters;
  Mf6GwtNameWriter: TMf6GwtNameWriter;
  IgnoredNames: TStringList;
  FileIndex: Integer;
  SIndex: Integer;
  SpeciesName: string;
begin
  Mf6GwtNameWriters := Model.Mf6GwtNameWriters as TMf6GwtNameWriters;
  IgnoredNames := TStringList.Create;
  try
    Model.GetIgnoredSpeciesNames(IgnoredNames);
    SpeciesName := Model.MobileComponents[SpeciesIndex].Name;
    if IgnoredNames.IndexOf(SpeciesName) >= 0 then
    begin
      Exit;
    end;
    FileIndex := -1;
    for SIndex := 0 to SpeciesIndex do
    begin
      SpeciesName := Model.MobileComponents[SpeciesIndex].Name;
      if IgnoredNames.IndexOf(SpeciesName) < 0 then
      begin
        Inc(FileIndex);
      end;
    end;
    Mf6GwtNameWriter := Mf6GwtNameWriters[FileIndex];
    Mf6GwtNameWriter.AddPackageFile(Ftype, FileName, PackageName);
  finally
    IgnoredNames.Free;
  end;
end;

class procedure TCustomModflowWriter.WriteToMt3dMsNameFile(const Ftype: string;
  const UnitNumber: integer; FileName: string; FileOption: TFileOption;
  AModel: TCustomModel; RelativeFileName: boolean = False; Option: String = '');
var
  Line: string;
  Extension: string;
begin
  Assert(CurrentNameFileWriter <> nil);
  case FileOption of
    foInput, foInputAlreadyExists:
      begin
        Extension := ExtractFileExt(FileName);
        if Extension <> '.ftl' then
        begin
          AModel.AddMt3dmsInputFile(FileName);
        end;
        CurrentNameFileWriter.FFullInputFiles.Add(FileName);
      end;
    foOutput:
      begin
        AModel.AddMt3dmsOutputFile(FileName);
        CurrentNameFileWriter.FFullOutputFiles.Add(FileName);
      end;
    else
      Assert(False);
  end;
  if not RelativeFileName then
  begin
    FileName := ExtractFileName(FileName);
  end;
  Line := Ftype + ' ' + IntToStr(UnitNumber) + ' ' + FileName;
  if Option <> '' then
  begin
    Line := Line + ' ' + Option;
  end;
  CurrentNameFileWriter.NameFile.Add(Line);
end;

class procedure TCustomModflowWriter.WriteToNameFile(const Ftype: string;
  UnitNumber: integer; FileName: string; const Option: TFileOption;
  OutputSuppression: TOutputSuppression; AModel: TCustomModel;
  RelativeFileName: boolean = False; PackageName: String = '');
var
  Line: string;
  UnitNumberString: string;
  FullFileName: string;
begin
  Assert(CurrentNameFileWriter <> nil);
  if (Option = foInputAlreadyExists) and not FileExists(FileName) then
  begin
    frmErrorsAndWarnings.AddError(CurrentNameFileWriter.Model,
      StrInputFileDoesNot,
      Format(StrTheRequiredInputF, [FileName]));
  end;
  if UnitNumber > MaxUnitNumber then
  begin
    MaxUnitNumber := UnitNumber;
  end;
  if RelativeFileName then
  begin
    FullFileName := ExpandFileName(FileName);
  end
  else
  begin
    FullFileName := FileName;
  end;
  case Option of
    foInput, foInputAlreadyExists:
      begin
        AModel.AddModelInputFile(FullFileName);
        CurrentNameFileWriter.FFullInputFiles.Add(ExtractFileName(FileName))
      end;
    foOutput:
      begin
        AModel.AddModelOutputFile(FullFileName);
        CurrentNameFileWriter.FFullOutputFiles.Add(ExtractFileName(FileName))
      end;
    else
      Assert(False);
  end;
  if not RelativeFileName then
  begin
    FileName := ExtractFileName(FileName);
  end;
  if Pos(' ', FileName) > 0 then
  begin
    if not FileExists(FileName) and (Option <> foInputAlreadyExists) then
    begin
      try
        TFile.Create(FileName).Free;
      except on EDirectoryNotFoundException do
        begin
          frmErrorsAndWarnings.AddError(AModel, StrDirectoryDoesNotE,
            Format(StrTheDirectoryFor, [FileName]));
        end;
      end;
    end;
    if frmGoPhast.ModelSelection <> msModflow2015 then
    begin
      FileName := ExtractShortPathName(FileName);
    end
    else
    begin
      FileName := '''' + FileName + '''';
    end;
  end;

  if OutputSuppression <> osShowAll then
  begin
    UnitNumber := -UnitNumber;
  end;

  Line := Ftype;
  if Length(Line) = 3 then
  begin
    Line := Line + '          ';
  end;
  if Length(Line) = 4 then
  begin
    Line := Line + '         ';
  end;
  while Length(Line) < 13 do
  begin
    Line := Line + ' ';
  end;
  if Line[Length(Line)] <> ' ' then
  begin
    Line := Line + ' ';
  end;
  if frmGoPhast.ModelSelection <> msModflow2015 then
  begin
    UnitNumberString := IntToStr(UnitNumber);
    while Length(UnitNumberString) < 6 do
    begin
      UnitNumberString := ' ' + UnitNumberString;
    end;
    Line := Line + UnitNumberString + ' ';
  end;
  Line := Line + FileName;
  if OutputSuppression <> osShowAll then
  begin
    Line := Line + ' ' + IntToStr(Ord(OutputSuppression) + 1);
  end;
  if (OutputSuppression <> osNoOutput)
    and (frmGoPhast.ModelSelection <> msModflow2015) then
  begin
    case Option of
      foNone: ;// do nothing
      foInput, foInputAlreadyExists:
        begin
          Line := Line + ' ' + 'OLD';
        end;
      foOutput:
        begin
          Line := Line + ' ' + 'REPLACE';
        end;
      else
        Assert(False);
    end;
  end;
  if frmGoPhast.ModelSelection = msModflow2015 then
  begin
    Line := '  ' + Line;
    if PackageName <> '' then
    begin
      Line := Line + '  ' + PackageName;
    end;
    case Option of
      foNone: Assert(False);
      foInput, foInputAlreadyExists:
        begin
          CurrentNameFileWriter.InputFiles.Add(Line);
        end;
      foOutput:
        begin
          CurrentNameFileWriter.OutputFiles.Add(Line);
        end;
      else
        Assert(False);
    end;
  end
  else
  begin
    CurrentNameFileWriter.NameFile.Add(Line);
  end;
end;

class procedure TCustomModflowWriter.WriteToNameFile(const Ftype: string;
  const UnitNumber: integer; FileName: string; const Option: TFileOption;
  AModel: TCustomModel;
  RelativeFileName: boolean = False; PackageName: String = '');
begin
  WriteToNameFile(Ftype, UnitNumber, FileName, Option, osShowAll, AModel,
    RelativeFileName, PackageName);
end;

function TCustomModflowWriter.IPRN_Real: integer;
begin
  if Model.ModflowOutputControl.PrintInputArrays then
  begin
    result := 12;
  end
  else
  begin
    result := -1;
  end;
end;

function TCustomModflowWriter.IPRN_Integer: integer;
begin
  if Model.ModflowOutputControl.PrintInputArrays then
  begin
    result := 5;
  end
  else
  begin
    result := -1;
  end;
end;

function TCustomModflowWriter.IPRN_Mt3dms_Integer: integer;
begin
  if Model.ModflowOutputControl.PrintInputArrays then
  begin
    result := 5;
  end
  else
  begin
    result := 0;
  end;
end;

function TCustomModflowWriter.IPRN_Mt3dms_Real: integer;
begin
  if Model.ModflowOutputControl.PrintInputArrays then
  begin
    result := 12;
  end
  else
  begin
    result := 0;
  end;
end;

procedure TCustomModflowWriter.WriteU2DINTHeader(const Comment: string;
  ArrayType: TModflowArrayType; const MF6_ArrayName: string);
begin
  case FArrayWritingFormat of
    awfModflow:
      begin
        WriteString( 'INTERNAL 1 (FREE)   ');
        WriteInteger(IPRN_Integer);
      end;
    awfMt3dms:
      begin
        WriteString('       103         1                    ');
        WriteString(FixedFormattedInteger(IPRN_Integer, 10));
      end;
    awfModflow_6:
      begin
        if MF6_ArrayName <> '' then
        begin
          WriteString('  ');
          WriteString(MF6_ArrayName);
          if ArrayType = matStructured then
          begin
            WriteString(' LAYERED');
          end;
          NewLine;
        end;
        WriteString('    INTERNAL IPRN ');
        WriteInteger(IPRN_Integer);
        WriteString(' # ' + Comment);
        NewLine;
      end
    else
      Assert(False);
  end;
  if FArrayWritingFormat <> awfModflow_6 then
  begin
    WriteString( ' # ' + Comment);
    NewLine;
  end;
end;

procedure TCustomModflowWriter.WriteU2DRELHeader(const Comment: string;
  ArrayType: TModflowArrayType; const MF6_ArrayName: string);
begin
  case FArrayWritingFormat of
    awfModflow:
      begin
        WriteString( 'INTERNAL 1.0 (FREE)   ');
        WriteInteger(IPRN_Real);
      end;
    awfMt3dms:
      begin
        WriteString('       103        1.                    ');
        WriteString(FixedFormattedInteger(IPRN_Real, 10));
      end;
    awfModflow_6:
      begin
        if MF6_ArrayName <> '' then
        begin
          WriteString('  ');
          WriteString(MF6_ArrayName);
//          if ArrayType = matStructured then
          begin
            WriteString(' LAYERED');
          end;
          NewLine;
        end;
        WriteString('    INTERNAL IPRN ');
        WriteInteger(IPRN_Real);
        WriteString(' # ' + Comment);
        NewLine;
      end
    else
      Assert(False);
  end;
  if FArrayWritingFormat <> awfModflow_6 then
  begin
    WriteString( ' # ' + Comment);
    NewLine;
  end;
end;

procedure TCustomModflowWriter.WriteConstantU2DINT(const Comment: string;
  const Value: integer; ArrayType: TModflowArrayType;
  const MF6_ArrayName: string);
begin
  case FArrayWritingFormat of
    awfModflow:
      begin
        WriteString( 'CONSTANT   ');
        WriteInteger(Value);
      end;
    awfMt3dms:
      begin
        WriteString('         0');
        WriteString(FixedFormattedInteger(Value, 10));
        WriteString('                    ');
        WriteString(FixedFormattedInteger(IPRN_Integer, 10));
      end;
    awfModflow_6:
      begin
        if MF6_ArrayName <> '' then
        begin
          WriteString('  ');
          WriteString(MF6_ArrayName);
          if ArrayType = matStructured then
          begin
            WriteString(' LAYERED');
          end;
          WriteString(' IPRN');
          WriteInteger(IPRN_Integer);
          NewLine;
        end;

        WriteString('    CONSTANT ');
        WriteInteger(Value);

        WriteString(' # ');
        WriteString(Comment);

        NewLine;
      end
    else
      Assert(False);
  end;
  if FArrayWritingFormat <> awfModflow_6 then
  begin
    WriteString( ' # ' + Comment);
    NewLine;
  end;
end;

procedure TCustomModflowWriter.WriteConstantU2DREL(const Comment: string;
  const Value: double; ArrayType: TModflowArrayType; const MF6_ArrayName: string);
begin
  case FArrayWritingFormat of
    awfModflow:
      begin
        WriteString('CONSTANT   ');
        WriteFloat(Value);
      end;
    awfMt3dms:
      begin
        WriteString('         0');
        WriteF10Float(Value);
        WriteString('                    ');
        WriteI10Integer(IPRN_Real, 'IPRN');
      end;
    awfModflow_6:
      begin
        if MF6_ArrayName <> '' then
        begin
          WriteString('  ');
          WriteString(MF6_ArrayName);
          if ArrayType = matStructured then
          begin
            WriteString(' LAYERED');
          end;
          WriteString(' IPRN');
          WriteInteger(IPRN_Real);
          NewLine;
        end;

        WriteString('    CONSTANT ');
        WriteFloat(Value);

        WriteString(' # ');
        WriteString(Comment);

        NewLine;
      end
    else
      Assert(False);
  end;
  if Model.ModelSelection <> msModflow2015 then
  begin
    WriteString( ' # ' + Comment);
  end;
  NewLine;
end;

{ TNameFileWriter }

class function TNameFileWriter.Extension: string;
begin
  result := '.nam';
end;

procedure TNameFileWriter.CheckExternalFiles(const FileName: string);
var
  Directory: string;
  OldDir: string;
  Index: Integer;
  ALine: string;
  Splitter: TStringList;
  Fname: string;
  InputFile: Boolean;
  Option: string;
  IsRelativeFileName: Boolean;
  RelPath: string;
begin
  Directory := ExtractFileDir(FileName);
  Directory := IncludeTrailingPathDelimiter(Directory);
  OldDir := GetCurrentDir;
  try
    SetCurrentDir(Directory);
    Splitter := TStringList.Create;
    try
      Splitter.Delimiter := #9;
      for Index := 0 to Model.ModflowNameFileLines.Count - 1 do
      begin
        ALine := Model.ModflowNameFileLines[Index];
        if (Trim(ALine) <> '') and (ALine[1] <> '#') then
        begin
          Splitter.DelimitedText := ALine;
          if Splitter.Count >= 3 then
          begin
            Fname := Splitter[2];
            InputFile := True;
            if Splitter.Count >= 4 then
            begin
              Option := Splitter[3];
              if SameText(Option, 'REPLACE') then
              begin
                InputFile := False;
              end;
            end;
            if not FileExists(Fname) then
            begin
              if InputFile then
              begin
                frmErrorsAndWarnings.AddWarning(Model, MissingFile, Fname);
              end;
            end;
            if InputFile then
            begin
              IsRelativeFileName := AnsiSameText(Copy(Fname, 1, ExtFileLength), ExtFileString);
              if not IsRelativeFileName then
              begin
                RelPath := ExtractRelativePath(FileName, Fname);
                IsRelativeFileName := AnsiSameText(Copy(RelPath, 1, ExtFileLength), ExtFileString);
              end;
              if IsRelativeFileName then
              begin
                Model.AddExternalFile(ExpandFileName(Fname));
              end
              else
              begin
                Model.AddModelInputFile(ExpandFileName(Fname));
              end;
            end
            else
            begin
              Model.AddModelOutputFile(ExpandFileName(Fname));
            end;
          end;
        end;
      end;
    finally
      Splitter.Free;
    end;
  finally
    SetCurrentDir(OldDir);
  end;
end;

procedure TNameFileWriter.InitilizeNameFile(const FileName: string;
  out OutputListFileName: string);
var
  CellFlowsName: string;
begin
//  Model.AddModelInputFile(FileName);
  ClearNameFile;
  AddNameFileComment(Format(StrNameFileForMODFLO,
    [DateToStr(Now), Model.ProgramName + ' Version ' + IModelVersion]));
  OutputListFileName := ChangeFileExt(FileName, '.lst');
  if Model.ModelSelection in [msModflowFmp, msModflowOwhm2] then
  begin
    WriteToNameFile(StrLIST, Model.UnitNumbers.UnitNumber(StrLIST),
      OutputListFileName, foOutput,
      Model.ModflowOutputControl.OutputSuppression, Model);
  end
  else
  begin
    WriteToNameFile(StrLIST, Model.UnitNumbers.UnitNumber(StrLIST),
      OutputListFileName, foOutput, Model);
  end;

  { TODO -cMODFLOW 6-2015 : This needs to change for MODFLOW 6}
  case Model.ModflowOutputControl.SaveCellFlows of
    csfNone:
      begin
        // do nothing
      end;
    csfBinary:
      begin
        CellFlowsName := ChangeFileExt(FileName, StrCbcExt);
        WriteToNameFile(StrDATABINARY,
          Model.UnitNumbers.UnitNumber(StrCBC), CellFlowsName, foOutput, Model);
      end;
    csfListing:
      begin
        // do nothing
      end;
    else
      begin
        Assert(False);
      end;
  end;
  if Model.ModflowNameFileLines.Count > 0 then
  begin
    CheckExternalFiles(FileName);
    if FModel.ModelSelection = msModflow2015 then
    begin
      InputFiles.Add('');
      InputFiles.Add('#Files generated outside of ' + Model.ProgramName);
      InputFiles.AddStrings(Model.ModflowNameFileLines);
      InputFiles.Add('');
      InputFiles.Add('#Files generated by ' + Model.ProgramName);
    end
    else
    begin
      NameFile.Add('');
      NameFile.Add('#Files generated outside of ' + Model.ProgramName);
      NameFile.AddStrings(Model.ModflowNameFileLines);
      NameFile.Add('');
      NameFile.Add('#Files generated by ' + Model.ProgramName);
    end;
  end;
end;

procedure TNameFileWriter.SaveNameFile(AFileName: string);
const
  // MaxLineLength is specified in
  MaxLineLength = 300;
begin
  inherited;
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrFileNameTooLong);
  if (Model.ModelSelection = msModflow2015)
    and (Length(AFileName) > MaxLineLength) then
  begin
    frmErrorsAndWarnings.AddError(Model, StrFileNameTooLong,
      Format(StrTheMaximumAllowed, [AFileName, Length(AFileName)]));
  end;
  AFileName := ChangeFileExt(AFileName, '.nam') + ArchiveExt;
  NameFile.WriteBOM := False;
  NameFile.SaveToFile(AFileName);
  Model.AddModelInputFile(AFileName);
end;

procedure TCustomModflowWriter.GetFlowUnitNumber(var UnitNumber: Integer);
begin

  case Model.ModflowOutputControl.SaveCellFlows of
    csfNone:
      begin
        UnitNumber := 0;
      end;
    csfBinary:
      begin
        UnitNumber := Model.UnitNumbers.UnitNumber(StrCBC);
      end;
    csfListing:
      begin
        UnitNumber := -1;
      end;
  else
    begin
      Assert(False);
    end;
  end;
end;

{ TCustomTransientWriter }

procedure TCustomParameterTransientWriter.ClearTimeLists(AModel: TBaseModel);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TModflowBoundary;
begin
  for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    if not ScreenObject.UsedModels.UsesModel(Model) then
    begin
      Continue;
    end;
    Boundary := GetBoundary(ScreenObject);
    if Boundary <> nil then
    begin
      Boundary.ClearTimeLists(AModel);
    end;
  end;

end;

procedure TCustomTransientWriter.CountCells(var MaximumNumberOfCells: Integer);
var
  StressPeriodIndex: Integer;
  List: TValueCellList;
begin
  MaximumNumberOfCells := 0;
  for StressPeriodIndex := 0 to FValues.Count - 1 do
  begin
    List := FValues[StressPeriodIndex];
    if List.Count > MaximumNumberOfCells then
    begin
      MaximumNumberOfCells := List.Count;
    end;
    List.Cache;
  end;

end;

constructor TCustomTransientWriter.Create(Model: TCustomModel; EvaluationType: TEvaluationType);
begin
  inherited;
  Assert(Model <> nil);
//  FMf6ObsArray := nil;
//  FScreenObjectLists := TObjectScreenObjectLists.Create;
  FValues := TObjectList.Create;
  if Mf6ObservationsUsed then
  begin
    FFlowObsLocations := TBoundaryFlowObservationLocationList.Create;
    FObsLocationCheck := T3DSparseStringArray.Create(
      GetQuantum(Model.LayerCount), GetQuantum(Model.RowCount),
      GetQuantum(Model.ColumnCount));

    FToMvrFlowObsLocations := TBoundaryFlowObservationLocationList.Create;
    FWellReductionFlowObsLocations := TBoundaryFlowObservationLocationList.Create;
  end;
  DirectObsLines := Model.DirectObservationLines;
  CalculatedObsLines := Model.DerivedObservationLines;
  FileNameLines := Model.FileNameLines;
//  FTimeSeriesNames := TStringList.Create;
//  FTimeSeriesNames.Sorted := True;
//  FTimeSeriesNames.Duplicates := dupIgnore;
end;

destructor TCustomTransientWriter.Destroy;
begin
  FWellReductionFlowObsLocations.Free;
  FToMvrFlowObsLocations.Free;

  FObsLocationCheck.Free;
  FFlowObsLocations.Free;
  FValues.Free;
//  FScreenObjectLists.Free;
  inherited;
end;

procedure TCustomTransientWriter.EnsureMf6CalibObservations(
  AScreenObject: TScreenObject);
begin
  // do nothing
end;

function TCustomTransientWriter.GetOwnsValueContents: Boolean;
begin
  result := (FValues as TObjectList).OwnsObjects;
end;

function TCustomParameterTransientWriter.BoundariesPresent: boolean;
begin
  result := (FParamValues.Count > 0) or (FValues.Count > 0)
end;

procedure TCustomParameterTransientWriter.Evaluate;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ParamIndex: Integer;
  List: TList;
  Boundary: TModflowBoundary;
  NoAssignmentErrorRoot: string;
  NoDefinedErrorRoot: string;
  FlowObs: TBoundaryFlowObservationLocation;
  Mf6Obs: TModflow6Obs;
  ListIndex: Integer;
  ValueCellList: TValueCellList;
  AValueCell: TValueCell;
  ParamBoundary: TModflowParamBoundary;
  AParam: TModflowTransientListParameter;
  procedure AssignMf6ObsNames(ValueCellList: TValueCellList);
  var
    CellIndex: Integer;
  begin
    for CellIndex := 0 to ValueCellList.Count - 1 do
    begin
      AValueCell := ValueCellList[CellIndex];

      if (not IsMf6Observation(AValueCell.ScreenObject as TScreenObject))
        and FObsLocationCheck.IsValue[
        AValueCell.Layer, AValueCell.Row, AValueCell.Column] then
      begin
        AValueCell.Mf6ObsName :=
          FObsLocationCheck[AValueCell.Layer, AValueCell.Row, AValueCell.Column];
        ValueCellList.InvalidateCache;
      end
      else
      begin
        AValueCell.Mf6ObsName := '';
      end;
    end;
    ValueCellList.Cache;
  end;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    RemoveNoDefinedError(NoDefinedErrorRoot);
    NoAssignmentErrorRoot := Format(StrNoBoundaryConditio, [Package.PackageIdentifier]);
    frmProgressMM.AddMessage(Format(StrEvaluatingSData, [Package.PackageIdentifier]));

    StorePotentialObservationLocationsAndNames;

    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      if not ScreenObject.UsedModels.UsesModel(Model) then
      begin
        Continue;
      end;
      Boundary := GetBoundary(ScreenObject);
      if Boundary <> nil then
      begin
        if Boundary is TModflowParamBoundary then
        begin
          ParamBoundary := TModflowParamBoundary(Boundary);
          for ParamIndex := ParamBoundary.Parameters.Count - 1 downto 0 do
          begin
            AParam := ParamBoundary.Parameters[ParamIndex].Param.Param;
            if (AParam = nil) or (AParam.ParameterType <> ParameterType) then
            begin
              ParamBoundary.Parameters.Delete(ParamIndex);
            end;
          end;
        end;

        if not ScreenObject.SetValuesOfEnclosedCells
          and not ScreenObject.SetValuesOfIntersectedCells then
        begin
          frmErrorsAndWarnings.AddError(Model, NoAssignmentErrorRoot,
            ScreenObject.Name, ScreenObject);
        end;
        frmProgressMM.AddMessage(Format(StrEvaluatingS,
          [ScreenObject.Name]));

        Boundary.GetCellValues(FValues, FParamValues, Model, self);
        FTimeSeriesNames.AddStrings(Boundary.Mf6TimeSeriesNames);

        if Mf6ObservationsUsed then
        begin
          if IsMf6ToWellReductionObservation(ScreenObject) then
          begin
            Mf6Obs := ScreenObject.Modflow6Obs;

            FlowObs.FName := Mf6Obs.Name;
            FlowObs.FMf6Obs := Mf6Obs;
            if Length(FlowObs.FName) > 36 then
            begin
              SetLength(FlowObs.FName, 36);
            end;

            FlowObs.FBoundName := ScreenObject.Name;
            WellReductionFlowObsLocations.Add(FlowObs);
          end;
          if IsMf6ToMvrObservation(ScreenObject) then
          begin
            Mf6Obs := ScreenObject.Modflow6Obs;

            FlowObs.FName := Mf6Obs.Name;
            FlowObs.FMf6Obs := Mf6Obs;
            if Length(FlowObs.FName) > 36 then
            begin
              SetLength(FlowObs.FName, 36);
            end;

            FlowObs.FBoundName := ScreenObject.Name;
            ToMvrFlowObsLocations.Add(FlowObs);
          end;
          if IsMf6Observation(ScreenObject) then
          begin
            if IsFlowObs(ScreenObject) then
            begin
              EnsureMf6CalibObservations(ScreenObject)
            end;
            Mf6Obs := ScreenObject.Modflow6Obs;

            FlowObs.FName := Mf6Obs.Name;
            FlowObs.FMf6Obs := Mf6Obs;
            FlowObs.FBoundName := ScreenObject.Name;
            FlowObsLocations.Add(FlowObs);
          end;
        end;
      end
      else if Mf6ObservationsUsed and IsMf6Observation(ScreenObject) then
      begin
        Mf6Obs := ScreenObject.Modflow6Obs;
        FlowObs.FName := Mf6Obs.Name;
        FlowObs.FMf6Obs := Mf6Obs;
        FlowObs.FBoundName := Mf6Obs.Name;
        FlowObsLocations.Add(FlowObs);

        if IsMf6ToMvrObservation(ScreenObject) then
        begin
          ToMvrFlowObsLocations.Add(FlowObs);
        end;

        if IsMf6ToWellReductionObservation(ScreenObject) then
        begin
          WellReductionFlowObsLocations.Add(FlowObs);
        end;
      end;
    end;

    if Mf6ObservationsUsed then
    begin

      for ParamIndex := 0 to FParamValues.Count - 1 do
      begin
        List := FParamValues.Objects[ParamIndex] as TList;
        for ListIndex := 0 to List.Count - 1 do
        begin
          ValueCellList := List[ListIndex];
          AssignMf6ObsNames(ValueCellList);
        end;
      end;

      for ListIndex := 0 to FValues.Count - 1 do
      begin
        ValueCellList := FValues[ListIndex];
        AssignMf6ObsNames(ValueCellList);
      end;
    end;

    for ParamIndex := 0 to FParamValues.Count - 1 do
    begin
      List := FParamValues.Objects[ParamIndex] as TList;
      While List.Count > FValues.Count do
      begin
        FValues.Add(TValueCellList.Create(CellType))
      end;
    end;
    for ParamIndex := 0 to FParamValues.Count - 1 do
    begin
      List := FParamValues.Objects[ParamIndex] as TList;
      While List.Count < FValues.Count do
      begin
        List.Add(TValueCellList.Create(CellType))
      end;
    end;


    if not BoundariesPresent then
    begin
      ShowNoBoundaryError(NoDefinedErrorRoot);
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

function TCustomParameterTransientWriter.ParameterCount: integer;
begin
  result := FParamValues.Count;
end;

procedure TCustomParameterTransientWriter.GetInstanceRoot(const PARNAM: string;
  ParamValues: TList; var InstanceRoot: string);
var
  InstanceName: string;
  PCount: Integer;
  MaxStressPeriodString: string;
  TerminalString: string;
begin
  PCount := FParameterNames.IndexOf(PARNAM);
  if PCount < 0 then
  begin
    PCount := FParameterNames.Add(PARNAM);
  end;
  Inc(PCount);
  MaxStressPeriodString := IntToStr(FModel.ModflowFullStressPeriods.Count);
  TerminalString := IntToStr(PCount) + '_';

  // Make sure the maximum length of the name of instance is <= 10.
  InstanceRoot := PARNAM;
  InstanceName := InstanceRoot + TerminalString + MaxStressPeriodString;
  While(Length(InstanceName)) > 10 do
  begin
    SetLength(InstanceRoot, Length(InstanceRoot)-1);
    InstanceName := InstanceRoot + TerminalString + MaxStressPeriodString;
  end;
  InstanceRoot := InstanceRoot + TerminalString;
end;

procedure TCustomParameterTransientWriter.GetInstanceName(
  var InstanceName: string;
  TimeIndex: Integer; InstanceRoot: string);
begin
  InstanceName := InstanceRoot + IntToStr(TimeIndex + 1);
  While (Length(InstanceName) > 10)
    or (FUsedInstanceNames.IndexOf(InstanceName) >= 0) do
  begin
    SetLength(InstanceRoot, Length(InstanceRoot) -1);
    Assert(InstanceRoot <> '');
    InstanceName := InstanceRoot + IntToStr(TimeIndex + 1);
  end;
  FUsedInstanceNames.Add(InstanceName);
end;

procedure TCustomParameterTransientWriter.GetNumCellsAndNumInstances(
  ParamValues: TList; var NUMINST, NLST: Integer);
var
  CellList: TValueCellList;
  TimeIndex: Integer;
  AllSame: Boolean;
  CellIndex: Integer;
  FirstCell: TValueCell;
  ACell: TValueCell;
  CellCount: Integer;
  FirstCellList: TValueCellList;
begin
  NLST := 0;
  NUMINST := 0;
  AllSame := True;
  CellCount := 0;
  for TimeIndex := 0 to ParamValues.Count - 1 do
  begin
    CellList := ParamValues[TimeIndex];
    if CellList.Count > NLST then
    begin
      NLST := CellList.Count;
    end;
    if CellList.Count > 0 then
    begin
      Inc(NUMINST);
    end;
    if TimeIndex = 0 then
    begin
      CellCount := CellList.Count;
    end
    else
    begin
      if CellCount <> CellList.Count then
      begin
        AllSame := False;
      end;
    end;
    CellList.Cache;
  end;
  if AllSame and (ParamValues.Count > 1) then
  begin
    FirstCellList := ParamValues[0];
    for TimeIndex := 0 to ParamValues.Count - 1 do
    begin
      CellList := ParamValues[TimeIndex];
      Assert(FirstCellList.Count = CellList.Count);
      for CellIndex := 0 to CellList.Count - 1 do
      begin
        FirstCell := FirstCellList[CellIndex];
        ACell := CellList[CellIndex];
        AllSame := ACell.IsIdentical(FirstCell);
        if not AllSame then
        begin
          break;
        end;
      end;
      CellList.Cache;
      if not AllSame then
      begin
        break;
      end;
    end;
    FirstCellList.Cache;
  end;
  if AllSame then
  begin
    NUMINST := 1;
  end;
end;

{ TCustomListWriter }

constructor TCustomListWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
//  FBoundNames := TStringList.Create;

  FObjectNames := TStringList.Create;
  FObjectNames.Sorted := True;
  FObjectNames.CaseSensitive := False;
end;

procedure TCustomPackageWriter.CheckCell(ValueCell: TValueCell;
  const PackageName: string);
var
  ActiveDataArray: TDataArray;
  WarningRoot: string;
  ErrorMessage: string;
begin
  ActiveDataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
  Assert(ActiveDataArray <> nil);
  ActiveDataArray.Initialize;
  if not ActiveDataArray.BooleanData[ValueCell.Layer,
    ValueCell.Row, ValueCell.Column] then
  begin
    WarningRoot := Format(StrOneOrMoreBoundari, [PackageName]);
    if WarningRoot = FWarningRoot then
    begin
      WarningRoot := FWarningRoot;
    end
    else
    begin
      FWarningRoot := WarningRoot;
    end;
    ErrorMessage := Format(StrLayerRowCol,
      [ValueCell.Layer+1, ValueCell.Row+1, ValueCell.Column+1]);
    frmErrorsAndWarnings.AddWarning(Model, FWarningRoot, ErrorMessage);
  end;
end;

constructor TCustomPackageWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FTimeSeriesFileNames := TStringList.Create;
  FMvrWriter := nil;
end;

function TCustomPackageWriter.AquiferConductance(Layer, Row, Column: Integer): double;
var
  Packages: TModflowPackages;
  DataArray: TDataArray;
  Value: Double;
  Group: TLayerGroup;
  AquiferType: Integer;
begin
  result := 0;
  Packages := Model.ModflowPackages;
  if Packages.LpfPackage.IsSelected or Packages.UpwPackage.IsSelected
    or Packages.NpfPackage.IsSelected then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(rsKx);
    Assert(DataArray <> nil);
    DataArray.Initialize;
    Value := DataArray.RealData[Layer, Row, Column];
    if not Model.DisvUsed then
    begin
      result := Value * Model.LayerThickness[Layer, Row, Column]
        * Min(Model.Grid.RowWidth[Row], Model.Grid.ColumnWidth[Column]);
    end
    else
    begin
      result := Value * Model.LayerThickness[Layer, Row, Column]
        * Model.DisvGrid.TwoDGrid.MinWidth[Column];
    end;
  end
  else if Packages.BcfPackage.IsSelected then
  begin
    Group := Model.GetLayerGroupByLayer(Layer);
    AquiferType := Group.AquiferType;
    if (AquiferType = 1) and (Layer > 0) then
    begin
      AquiferType := 3;
    end;
    case AquiferType of
      0, 2:
        begin
          // Data set 5
          DataArray := Model.DataArrayManager.GetDataSetByName(StrTransmissivity);
          Assert(DataArray <> nil);
          DataArray.Initialize;
          Value := DataArray.RealData[Layer, Row, Column];
          if not Model.DisvUsed then
          begin
            result := Value
              * Min(Model.Grid.RowWidth[Row], Model.Grid.ColumnWidth[Column]);
          end
          else
          begin
            result := Value * Model.DisvGrid.TwoDGrid.MinWidth[Column];
//              * Min(Model.Grid.RowWidth[Row], Model.Grid.ColumnWidth[Column]);
          end;
        end;
      1, 3:
        begin
          // Data set 6
          DataArray := Model.DataArrayManager.GetDataSetByName(rsKx);
          Assert(DataArray <> nil);
          DataArray.Initialize;
          Value := DataArray.RealData[Layer, Row, Column];
          if not Model.DisvUsed then
          begin
            result := Value * Model.LayerThickness[Layer, Row, Column]
              * Min(Model.Grid.RowWidth[Row], Model.Grid.ColumnWidth[Column]);
          end
          else
          begin
            result := Value * Model.LayerThickness[Layer, Row, Column]
              * Model.DisvGrid.TwoDGrid.MinWidth[Column];
          end;
        end;
      else
        begin
          Assert(False);
        end;
    end;
  end
  else if Packages.HufPackage.IsSelected then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(StrHUFKxName);
    Assert(DataArray <> nil);
    DataArray.Initialize;
    Value := DataArray.RealData[Layer, Row, Column];
    if not Model.DisvUsed then
    begin
      result := Value * Model.LayerThickness[Layer, Row, Column]
        * Min(Model.Grid.RowWidth[Row], Model.Grid.ColumnWidth[Column]);
    end
    else
    begin
      result := Value * Model.LayerThickness[Layer, Row, Column]
        * Model.DisvGrid.TwoDGrid.MinWidth[Column];
    end;
  end
  else
  begin
    Assert(False);
  end;
end;

function TCustomPackageWriter.AquiferKx(Layer, Row, Column: Integer): double;
var
  Packages: TModflowPackages;
  DataArray: TDataArray;
  Value: Double;
  Group: TLayerGroup;
  AquiferType: Integer;
begin
  result := 0;
  Packages := Model.ModflowPackages;
  if Packages.LpfPackage.IsSelected or Packages.UpwPackage.IsSelected
    or Packages.NpfPackage.IsSelected then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(rsKx);
    Assert(DataArray <> nil);
    DataArray.Initialize;
    result := DataArray.RealData[Layer, Row, Column];
  end
  else if Packages.BcfPackage.IsSelected then
  begin
    Group := Model.GetLayerGroupByLayer(Layer);
    AquiferType := Group.AquiferType;
    if (AquiferType = 1) and (Layer > 0) then
    begin
      AquiferType := 3;
    end;
    case AquiferType of
      0, 2:
        begin
          // Data set 5
          DataArray := Model.DataArrayManager.GetDataSetByName(StrTransmissivity);
          Assert(DataArray <> nil);
          DataArray.Initialize;
          Value := DataArray.RealData[Layer, Row, Column];
          result := Value / Model.ModflowGrid.LayerThickness(Layer, Row, Column);
        end;
      1, 3:
        begin
          // Data set 6
          DataArray := Model.DataArrayManager.GetDataSetByName(rsKx);
          Assert(DataArray <> nil);
          DataArray.Initialize;
          result := DataArray.RealData[Layer, Row, Column];
        end;
      else
        begin
          Assert(False);
        end;
    end;
  end
  else if Packages.HufPackage.IsSelected then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(StrHUFKxName);
    Assert(DataArray <> nil);
    DataArray.Initialize;
    result := DataArray.RealData[Layer, Row, Column];
  end
  else
  begin
    Assert(False, 'No flow model has been selected');
  end;
end;

procedure TCustomListWriter.CountCells(var MaximumNumberOfCells: Integer);
var
  List: TValueCellList;
  NumberInStressPeriod: Integer;
  ParamIndex: Integer;
  ParamValues: TList;
  StressPeriodIndex: Integer;
  MaximumParamCells : integer;
begin
  inherited CountCells(MaximumNumberOfCells);

//  if Model.ModelSelection <> msModflow2015 then
  begin
    MaximumParamCells := 0;
    for StressPeriodIndex := 0 to FValues.Count - 1 do
    begin
      NumberInStressPeriod := 0;
      for ParamIndex := 0 to FParamValues.Count - 1 do
      begin
        ParamValues := FParamValues.Objects[ParamIndex] as TList;
        Assert(ParamValues.Count = FValues.Count);
        List := ParamValues[StressPeriodIndex];
        NumberInStressPeriod := NumberInStressPeriod + List.Count;
        List.Cache;
      end;
      if NumberInStressPeriod > MaximumParamCells then
      begin
        MaximumParamCells := NumberInStressPeriod;
      end;
    end;

    MaximumNumberOfCells := MaximumNumberOfCells + MaximumParamCells;
  end;
end;

procedure TCustomListWriter.GetOption(var Option: string);
begin
  Option := ' AUXILIARY IFACE';
  if not Model.ModflowOutputControl.PrintInputCellLists then
  begin
    Option := Option + ' NOPRINT';
  end;
end;

function TCustomListWriter.ITMPUsed: Boolean;
begin
  result := True;
end;

function TCustomTransientWriter.ObsType: string;
begin
  result := '';
  Assert(False);
end;

procedure TCustomListWriter.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists; ParameterIndicies: TByteSet);
var
  ParamIndex: Integer;
  Param: TModflowTransientListParameter;
  Position: integer;
  ParameterValues: TList;
  PARTYP: String;
  ErrorMessage: string;
  TimeIndex: Integer;
  CellList: TValueCellList;
  DataArrayList: TList;
  TimeListIndex: Integer;
  DisplayTimeList: TModflowBoundaryDisplayTimeList;
  DataArray: TModflowBoundaryDisplayDataArray;
  CheckParameters: Boolean;
begin
  // Quit if the package isn't used.
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;
  frmErrorsAndWarnings.BeginUpdate;
  try
    DataArrayList := TList.Create;
    try
      // evaluate all the data used in the package.
      Evaluate;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      ClearTimeLists(Model);

      // Set PARTYP.
      case ParameterType of
        ptUndefined..ptLPF_VKCB:
          begin
            Assert(False);
          end;
        ptRch, ptEvt, ptETS, ptCHD..ptDRT, ptSTR, ptQMAX:
          begin
            PARTYP := ' ' + ParmeterTypeToStr(ParameterType);
          end
        Else Assert(False);
      end;
      // Set the error message.
      ErrorMessage := Format(StrOneOrMoreSParam, [Trim(PARTYP)]);
      frmErrorsAndWarnings.RemoveWarningGroup(Model, ErrorMessage);


      CheckParameters := True;
      if (ParameterType = ptQMAX) and (Model.ModelSelection = msModflowOwhm2) then
      begin
        CheckParameters := False;
      end;
      // loop over the parameters
      if CheckParameters then
      begin
        for ParamIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
        begin
          Param := Model.ModflowTransientParameters[ParamIndex];
          // Consider only those parameters that match the parameter type for the
          // current package.
          if Param.ParameterType = ParameterType then
          begin
            Position := ParamValues.IndexOf(Param.ParameterName);
            // The parameter name is erased from FParamValues in
            // CountParametersAndParameterCells if there are no cells
            // associated with it.
            if Position < 0 then
            begin
              if frmErrorsAndWarnings <> nil then
              begin
                frmErrorsAndWarnings.AddWarning(Model,
                  ErrorMessage, Param.ParameterName);
              end;
              Continue;
            end;
            ParameterValues := ParamValues.Objects[Position] as TList;
            if ParameterValues.Count = 0 then
            begin
              Continue;
            end;
            // ParameterValues contains lists of cells for a parameter for
            // each stress period.
            for TimeListIndex := 0 to TimeLists.Count - 1 do
            begin
              DisplayTimeList := TimeLists[TimeListIndex];
              Assert(ParameterValues.Count = DisplayTimeList.Count);
            end;
            // For each stress period, transfer values from
            // the cells lists to the data arrays.
            for TimeIndex := 0 to ParameterValues.Count - 1 do
            begin
              CellList := ParameterValues[TimeIndex];
              if CellList.Count > 0 then
              begin
                DataArrayList.Clear;
                for TimeListIndex := 0 to TimeLists.Count - 1 do
                begin
                  DisplayTimeList := TimeLists[TimeListIndex];
                  DataArray := DisplayTimeList[TimeIndex]
                    as TModflowBoundaryDisplayDataArray;
                  DataArrayList.Add(DataArray);
                end;
                UpdateCellDisplay(CellList, DataArrayList,
                  ParameterIndicies, Param);
              end;
            end;
          end;
        end;
      end;
      // Values contains lists of cells not associated with any parameter for
      // each stress period.
      for TimeListIndex := 0 to TimeLists.Count - 1 do
      begin
        DisplayTimeList := TimeLists[TimeListIndex];
        // Values.Count can be zero if no objects define the boundary condition.
        if (Values.Count <> 0) or (DisplayTimeList.Count = 0) then
        begin
          Assert(Values.Count = DisplayTimeList.Count);
        end;
      end;

      // For each stress period, transfer values from
      // the cells lists to the data arrays.
      for TimeIndex := 0 to Values.Count - 1 do
      begin
        CellList := Values[TimeIndex];
        if CellList.Count > 0 then
        begin
          DataArrayList.Clear;
          for TimeListIndex := 0 to TimeLists.Count - 1 do
          begin
            DisplayTimeList := TimeLists[TimeListIndex];
            DataArray := DisplayTimeList[TimeIndex]
              as TModflowBoundaryDisplayDataArray;
            DataArrayList.Add(DataArray);
          end;
          UpdateCellDisplay(CellList, DataArrayList, ParameterIndicies);
        end;
      end;
      // Mark all the data arrays and time lists as up to date.
      for TimeListIndex := 0 to TimeLists.Count - 1 do
      begin
        DisplayTimeList := TimeLists[TimeListIndex];
        for TimeIndex := 0 to DisplayTimeList.Count - 1 do
        begin
          DataArray := DisplayTimeList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.UpToDate := True;
        end;
        DisplayTimeList.SetUpToDate(True);
      end;
    finally
      DataArrayList.Free;
    end;
    if frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.Show;
    end;
  finally
    if Model is TPhastModel then
    begin
      TPhastModel(Model).InvalidateAllDynamicLists;
    end;
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TCustomListWriter.WriteMF6_ListParm(DataSetIdentifier,
  VariableIdentifiers, ErrorRoot: string; const TimeIndex: integer);
var
  ErrorMessage: string;
  ParamIndex: Integer;
  Param: TModflowTransientListParameter;
  Position: Integer;
  ParamValues: TList;
//  TimeIndex: Integer;
  CellList: TValueCellList;
  PARTYP: string;
  NUMINST: Integer;
  NLST: Integer;
//  DataSetIdentifier: string;
//  MultiplierArrayNames: TTransientMultCollection;
//  ZoneArrayNames: TTransientZoneCollection;
begin
  case ParameterType of
    ptUndefined..ptLPF_VKCB: Assert(False);
    ptRch, ptEvt, ptETS, ptCHD..ptDRT, ptQMAX: PARTYP := ' '
      + ParmeterTypeToStr(ParameterType);
    Else Assert(False);
  end;
  ErrorMessage := Format(ErrorRoot, [Trim(PARTYP)]);
  for ParamIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    Param := Model.ModflowTransientParameters[ParamIndex];
    if Param.ParameterType = ParameterType then
    begin
      Position := FParamValues.IndexOf(Param.ParameterName);
      // The parameter name is erased from FParamValues in
      // CountParametersAndParameterCells if there are no cells
      // associated with it.
      if Position < 0 then
      begin
        if frmErrorsAndWarnings <> nil then
        begin
          frmErrorsAndWarnings.AddWarning(Model,
            ErrorMessage, Param.ParameterName);
        end;
        Continue;
      end;
      ParamValues := FParamValues.Objects[Position] as TList;
      GetNumCellsAndNumInstances(ParamValues, NUMINST, NLST);
      Assert(ParamValues.Count > 0);
//      for TimeIndex := 0 to ParamValues.Count - 1 do
//      begin
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        CellList := ParamValues[TimeIndex];
//        CellList.CheckRestore;
        if CellList.Count > 0 then
        begin
          WriteParameterCells(CellList, NLST, VariableIdentifiers,
            DataSetIdentifier, umAssign, nil, nil);
        end;
        CellList.Cache;
//      end;
    end;
  end;

end;

procedure TCustomListWriter.WriteMoverOption;
begin

end;

procedure TCustomTransientWriter.WriteModflow6FlowObs(FileName: string;
  EvaluationType: TEvaluationType);
var
  FlowObsWriter: TModflow6FlowObsWriter;
begin
  if ((FlowObsLocations <> nil) and (FlowObsLocations.Count > 0))
    or ((ToMvrFlowObsLocations <> nil) and (ToMvrFlowObsLocations.Count > 0))
    or ((WellReductionFlowObsLocations <> nil) and (WellReductionFlowObsLocations.Count > 0))
    then
  begin
    FileName := ChangeFileExt(FileName, ObservationExtension);
    FlowObsWriter := TModflow6FlowObsWriter.Create(Model, EvaluationType,
      FlowObsLocations, ObsType, ToMvrFlowObsLocations,
      WellReductionFlowObsLocations,
      ObservationOutputExtension, Mf6ObType);
    try
      FlowObsWriter.DirectObsLines := DirectObsLines;
      FlowObsWriter.CalculatedObsLines := CalculatedObsLines;
      FlowObsWriter.FileNameLines := FileNameLines;
      FlowObsWriter.WriteFile(FileName);
    finally
      FlowObsWriter.Free;
    end;
  end;
end;

procedure TCustomTransientWriter.WriteModflow6GwtFlowObs(FileName: string;
  EvaluationType: TEvaluationType; SpeciesIndex: integer);
var
  FlowObsWriter: TModflow6GwtFlowObsWriter;
begin
  if ((FlowObsLocations <> nil) and (FlowObsLocations.Count > 0))
    or ((ToMvrFlowObsLocations <> nil) and (ToMvrFlowObsLocations.Count > 0))
    or ((WellReductionFlowObsLocations <> nil) and (WellReductionFlowObsLocations.Count > 0)) then
  begin
    FileName := ChangeFileExt(FileName, ObservationExtension);
    FlowObsWriter := TModflow6GwtFlowObsWriter.Create(Model, EvaluationType,
      FlowObsLocations, ObsType, ToMvrFlowObsLocations, WellReductionFlowObsLocations,
      ObservationOutputExtension, Mf6GwtObType, SpeciesIndex);
    try
      FlowObsWriter.DirectObsLines := DirectObsLines;
      FlowObsWriter.CalculatedObsLines := CalculatedObsLines;
      FlowObsWriter.FileNameLines := FileNameLines;
      FlowObsWriter.WriteFile(FileName);
    finally
      FlowObsWriter.Free;
    end;
  end;
end;

procedure TCustomListWriter.WriteParameterDefinitions(const DS3, DS3Instances,
  DS4A, DataSetIdentifier, VariableIdentifiers, ErrorRoot: string;
  AssignmentMethod: TUpdateMethod;
  MultiplierArrayNames: TTransientMultCollection;
  ZoneArrayNames: TTransientZoneCollection);
var
  ParamIndex: Integer;
  Param: TModflowTransientListParameter;
  Position: Integer;
  ParamValues: TList;
  PARNAM: string;
  NLST: Integer;
  Parval: Double;
  InstanceRoot: string;
  TimeIndex: Integer;
  CellList: TValueCellList;
  InstanceName: string;
  PARTYP: string;
  ErrorMessage: string;
  NUMINST: Integer;
  FirstIndex: Boolean;
  List: TValueCellList;
  CellIndex: Integer;
begin
  case ParameterType of
    ptUndefined..ptLPF_VKCB: Assert(False);
    ptRch, ptEvt, ptETS, ptCHD..ptDRT, ptQMAX: PARTYP := ' '
      + ParmeterTypeToStr(ParameterType);
    Else Assert(False);
  end;
  ErrorMessage := Format(ErrorRoot, [Trim(PARTYP)]);
  for ParamIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    Param := Model.ModflowTransientParameters[ParamIndex];
    if Param.ParameterType = ParameterType then
    begin
      Position := FParamValues.IndexOf(Param.ParameterName);
      // The parameter name is erased from FParamValues in
      // CountParametersAndParameterCells if there are no cells
      // associated with it.
      if Position < 0 then
      begin
        if frmErrorsAndWarnings <> nil then
        begin
          frmErrorsAndWarnings.AddWarning(Model,
            ErrorMessage, Param.ParameterName);
        end;
        Continue;
      end;
      ParamValues := FParamValues.Objects[Position] as TList;
      Assert(ParamValues.Count > 0);
      // Data set 3
      PARNAM := Param.ParameterName;
      frmProgressMM.AddMessage(Format(StrWritingParamter, [PARNAM]));
      if Length(PARNAM) > 10 then
      begin
        SetLength(PARNAM, 10);
      end;
      GetNumCellsAndNumInstances(ParamValues, NUMINST, NLST);
      while FNumInstList.Count <= Position do
      begin
        FNumInstList.Add(0);
      end;
      FNumInstList[Position] := NUMINST;
      Parval := Param.Value;
      if (Model.ModelSelection <> msModflow2015) and (FEvaluationType <> etExportCsv) then
      begin
        WriteString(PARNAM);
        WriteString(PARTYP);
        WriteFloat(Parval);
        WriteInteger(NLST);
        if NUMINST > 1 then
        begin
          WriteString(' INSTANCES');
          WriteInteger(NUMINST);
        end;
        WriteString(DS3);
        if NUMINST > 1 then
        begin
          WriteString(DS3Instances);
        end;
        NewLine;
      end;

      if not WritingTemplate then
      begin
        Model.WritePValAndTemplate(PARNAM,PARVAL, Param);
      end;

      // Make sure the maximum length of the name of instance is <= 10.
      GetInstanceRoot(PARNAM, ParamValues, InstanceRoot);

      DoBeforeWriteParamCells;

      // Data sets 4a and 4b
      FirstIndex := True;
      for TimeIndex := 0 to ParamValues.Count - 1 do
      begin
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        CellList := ParamValues[TimeIndex];
//        CellList.CheckRestore;
        if CellList.Count > 0 then
        begin
          if (Model.ModelSelection  = msModflow2015) or (FEvaluationType = etExportCsv) then
          begin
            List := FValues[TimeIndex];
            for CellIndex := 0 to CellList.Count - 1 do
            begin
              List.Add(CellList[CellIndex]);
            end;
            CellList.OwnsObjects := False;
            CellList.Clear;
            List.Cache;
          end
          else
          begin
            // Data set 4a.
            if NUMINST > 1 then
            begin
              GetInstanceName(InstanceName, TimeIndex, InstanceRoot);
              WriteString(InstanceName);
              WriteString(DS4A + ' (Parameter instance for stress period '
                + IntToStr(TimeIndex+1) + ')');
              NewLine;
            end;
            // Data set 4b.
            if (FirstIndex) or (NUMINST > 1 ) then
            begin
              FirstIndex := False;
              WriteParameterCells(CellList, NLST, VariableIdentifiers,
                DataSetIdentifier, umAssign, MultiplierArrayNames, ZoneArrayNames);
            end;
          end;
        end;
        CellList.Cache;
      end;
    end;
  end;
end;

destructor TCustomListWriter.Destroy;
begin
  FObjectNames.Free;
//  FBoundNames.Free;
  inherited;
end;

procedure TCustomListWriter.WriteAndCheckCells(const VariableIdentifiers: string;
  const DataSetIdentifier: string; List: TValueCellList; TimeIndex: integer);
var
  CellIndex: Integer;
  Cell: TValueCell;
  ShouldWrite: Boolean;
  IDomainDataArray: TDataArray;
begin
  if Model.ModelSelection = msModflow2015 then
  begin
	  IDomainDataArray := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);
  end
  else
  begin
    IDomainDataArray := nil;
  end;
  for CellIndex := 0 to List.Count - 1 do
  begin
    Cell := List[CellIndex] as TValueCell;
    ShouldWrite := (Model.ModelSelection <> msModflow2015)
      or (IDomainDataArray.IntegerData[Cell.Layer,
      Cell.Row, Cell.Column] > 0);
    if ShouldWrite then
    begin
      WriteCell(Cell, DataSetIdentifier, VariableIdentifiers);
    end;
    CheckCell(Cell, Package.PackageIdentifier);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
  end;
end;

procedure TCustomListWriter.WriteBooleanValueFromGlobalFormula(Formula: string;
  ErrorObject: TObject; const DataSetErrorString: string);
var
  Expression: TExpression;
  Value: Boolean;
begin
  Expression := EvaluateValueFromGlobalFormula(Formula, ErrorObject,
    DataSetErrorString, [rdtBoolean]);

  if Expression.ResultType = rdtBoolean then
  begin
    Value := Expression.BooleanResult;
    if Value then
    begin
      WriteInteger(1);
    end
    else
    begin
      WriteInteger(0);
    end;
  end
  else
  begin
    WriteInteger(0);
    frmFormulaErrors.AddFormulaError(GetObjectString(ErrorObject), DataSetErrorString,
      Formula, StrTheFormulaShouldR);
  end;
end;

procedure TCustomListWriter.DoBeforeWriteCells;
begin
  // overriden in Farm process and DRN, RIV, GHB, DRT, and CHD.
end;

procedure TCustomListWriter.DoBeforeWriteParamCells;
begin
  // overriden in Farm process
end;

procedure TCustomListWriter.WriteCustomStressPeriod(TimeIndex: Integer);
begin

end;

procedure TCustomListWriter.WriteStressPeriods(const VariableIdentifiers,
  DataSetIdentifier, DS5, D7PNameIname, D7PName: string);
var
  ITMP: Integer;
  NP: Integer;
  List: TValueCellList;
  ParamValues: TList;
  ParamIndex: Integer;
  ParametersUsed: TStringList;
  TimeIndex: Integer;
  ShouldWriteITMP: Boolean;
  WriteCells: Boolean;
begin
  FUsedInstanceNames.Clear;
  ParamValues := TList.Create;
  try
    for TimeIndex := 0 to FValues.Count - 1 do
    begin
      FStartTime := Model.ModflowFullStressPeriods[TimeIndex].StartTime;
      FStressPeriod := TimeIndex;
      FBoundaryIndex := 0;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      frmProgressMM.AddMessage(Format(StrWritingStressPer, [TimeIndex+1]));
      ParametersUsed := TStringList.Create;
      try
        RetrieveParametersForStressPeriod(D7PNameIname, D7PName, TimeIndex,
          ParametersUsed, ParamValues, False);

        NP := ParametersUsed.Count;
        GetITMP(ITMP, TimeIndex, List);
        if (ITMP = 0) and (NP = 0) then
        begin
          frmErrorsAndWarnings.AddWarning(Model,
            Format(StrNoBoundaryConditio1, [Package.PackageIdentifier]),
            Format(StrStressPeriod0d, [TimeIndex+1]));
        end;

        ShouldWriteITMP := True;
        if (Model.ModelSelection = msModflow2015) and (FEvaluationType <> etExportCsv) then
        begin
          ShouldWriteITMP := False;
        end
        else
        begin
          if (Model.ModelSelection = msModflowOwhm2) then
          begin
            ShouldWriteITMP := ITMPUsed;
          end;
        end;

        if ShouldWriteITMP then
        begin
          // data set 5;
          WriteInteger(ITMP);
          WriteInteger(NP);
          if DS5 <> '' then
          begin
            WriteString(DS5 + ' Stress period ' + IntToStr(TimeIndex+1));
          end;
          NewLine;
        end;

        if Model.ModelSelection = msModflow2015 then
        begin
          if (ITMP < 0) and (NP = 0) and (FEvaluationType <> etExportCsv) then
          begin
            Continue;
          end;
          if FEvaluationType <> etExportCsv then
          begin
            WriteBeginPeriod(TimeIndex);
          end;
        end;
        // data set 6
        WriteCells := (ITMP > 0) or ((ITMP < 0) and (FEvaluationType = etExportCsv));
        if (Model.ModelSelection = msModflowOwhm2) and not ITMPUsed and (ITMP < 0) then
        begin
          WriteCells := True;
        end;

        if WriteCells then
        begin
          DoBeforeWriteCells;
          WriteAndCheckCells(VariableIdentifiers, DataSetIdentifier, List,
            TimeIndex);
          Application.ProcessMessages;
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;
        end;
        if (Model.ModelSelection = msModflow2015) or (FEvaluationType = etExportCsv) then
        begin
          WriteMF6_ListParm(DataSetIdentifier, VariableIdentifiers,
            StrOneOrMoreSParam, TimeIndex);
          if FEvaluationType <> etExportCsv then
          begin
            WriteEndPeriod;
          end;
        end;
        if TimeIndex = FValues.Count - 1 then
        begin
          if List <> nil then
          begin
            List.Cache;
          end;
        end;
        // data set 7
        if Model.ModelSelection <> msModflow2015 then
        begin
          for ParamIndex := 0 to ParametersUsed.Count - 1 do
          begin
            WriteString(ParametersUsed[ParamIndex]);
            NewLine;
            Application.ProcessMessages;
            if not frmProgressMM.ShouldContinue then
            begin
              Exit;
            end;
          end;
        end;
      finally
        ParametersUsed.Free;
      end;
      WriteCustomStressPeriod(TimeIndex);

    end;
  finally
    ParamValues.Free;
  end;
end;

{ TCustomTransientArrayWriter }
procedure TCustomTransientArrayWriter.EvaluateParameterCells(
  CellList: TValueCellList; DataArray: TModflowBoundaryDisplayDataArray;
  Param: TModflowTransientListParameter; AssignmentMethod: TUpdateMethod);
var
  CellIndex: Integer;
  Cell: TValueCell;
  Annotation: string;
  NewAnnotation: string;
  OldAnnotation: string;
  AnnotationList: TStringList;
  GroupedAnnotation: string;
  GroupIndex: Integer;
begin
  if CellList.Count = 0 then
  begin
    Exit;
  end;

  AnnotationList := TStringList.Create;
  try
    AnnotationList.Sorted := True;
    Annotation := '';
    for CellIndex := 0 to CellList.Count - 1 do
    begin
      Cell := CellList[CellIndex] as TValueCell;
      if (AssignmentMethod = umAdd)
        and DataArray.IsValue[0, Cell.Row, Cell.Column] then
      begin
        DataArray.RealData[0, Cell.Row, Cell.Column] :=
          DataArray.RealData[0, Cell.Row, Cell.Column]
          + Cell.RealValue[0, Model] * Param.Value;
      end
      else if Model.ModelSelection <> msModflow2015 then
      begin
        DataArray.RealData[0, Cell.Row, Cell.Column] :=
          Cell.RealValue[0, Model] * Param.Value;
      end
      else
      begin
        DataArray.RealData[0, Cell.Row, Cell.Column] :=
          Cell.RealValue[0, Model];
      end;
      if Model.ModelSelection <> msModflow2015 then
      begin
        NewAnnotation := Format(Str0sMultipliedByParam,
          [Cell.RealAnnotation[0, Model],
          Param.ParameterName, Param.Value]);
      end
      else
      begin
        NewAnnotation := Cell.RealAnnotation[0, Model];
      end;
//      NewAnnotation := Cell.RealAnnotation[0, Model]
//        + ' (multiplied by ' + Param.ParameterName + ' = '
//        + FloatToStr(Param.Value) + ')';
      // reduce memory usage by preventing multiple copies of the same
      // annotation from being saved.
      if Annotation <> NewAnnotation then
      begin
        Annotation := NewAnnotation;
      end;
      if (AssignmentMethod = umAdd)
        and DataArray.IsValue[0, Cell.Row, Cell.Column] then
      begin
        OldAnnotation := DataArray.Annotation[0, Cell.Row, Cell.Column];
        GroupedAnnotation := OldAnnotation
          + sLineBreak + 'Plus' + sLineBreak + Annotation;
        // reduce memory usage by preventing the number of strings from
        // getting out of hand.
        GroupIndex := AnnotationList.IndexOf(GroupedAnnotation);
        if GroupIndex >= 0 then
        begin
          GroupedAnnotation := AnnotationList[GroupIndex];
        end
        else
        begin
          AnnotationList.Add(GroupedAnnotation)
        end;
        DataArray.Annotation[0, Cell.Row, Cell.Column] := GroupedAnnotation;
      end
      else
      begin
        DataArray.Annotation[0, Cell.Row, Cell.Column] := Annotation;
      end;
      DataArray.CellCount[0, Cell.Row, Cell.Column] := 1;
    end;

    DataArray.UpToDate := True;
  finally
    AnnotationList.Free;
  end;

end;

procedure TCustomTransientArrayWriter.WriteParameterCells(
  CellList: TValueCellList; NLST: integer; const VariableIdentifiers,
  DataSetIdentifier: string; AssignmentMethod: TUpdateMethod;
  MultiplierArrayNames: TTransientMultCollection;
  ZoneArrayNames: TTransientZoneCollection);
var
  MultiplierArray: TDataArray;
  ZoneArray: TDataArray;
  CellIndex: Integer;
  Cell: TValueCell;
  RowIndex: Integer;
  ColIndex: Integer;
  IdenticalZoneArray: TDataArray;
  IdenticalMultiplierArray: TDataArray;
  NewMultName: string;
  MultPrefix: string;
  ZonePrefix: string;
  NewZoneName: string;
  MultItem: TTransientMultItem;
  TransMultWriter: TTransientArrayWriter;
  ZoneItem: TTransientZoneItem;
  DummyAnnotation: string;
begin
  if CellList.Count = 0 then
  begin
    Exit;
  end;
  DummyAnnotation := 'none';
  MultiplierArray := TDataArray.Create(Model);
  ZoneArray:= TDataArray.Create(Model);
  try
    MultPrefix := Prefix + '_Mult_';
    NewMultName := MultPrefix
      + IntToStr(Model.TransientMultiplierArrays.Count + 1);
    while (Length(NewMultName) > 10) and (Length(MultPrefix) > 0) do
    begin
      MultPrefix := Copy(MultPrefix, 1, Length(MultPrefix)-1);
      NewMultName := MultPrefix
        + IntToStr(Model.TransientMultiplierArrays.Count + 1);
    end;

    ZonePrefix := Prefix + '_Zone_';
    NewZoneName := ZonePrefix
      + IntToStr(Model.TransientZoneArrays.Count + 1);
    while (Length(NewZoneName) > 10) and (Length(ZonePrefix) > 0) do
    begin
      ZonePrefix := Copy(ZonePrefix, 1, Length(ZonePrefix)-1);
      NewZoneName := ZonePrefix
        + IntToStr(Model.TransientZoneArrays.Count + 1);
    end;

    MultiplierArray.Name := NewMultName;
    ZoneArray.Name := NewZoneName;

    MultiplierArray.Orientation := dsoTop;
    ZoneArray.Orientation := dsoTop;

    MultiplierArray.EvaluatedAt := eaBlocks;
    ZoneArray.EvaluatedAt := eaBlocks;

    MultiplierArray.DataType := rdtDouble;
    ZoneArray.DataType := rdtBoolean;

    MultiplierArray.UpdateDimensions(1, Model.ModflowGrid.RowCount,
      Model.ModflowGrid.ColumnCount, True);
    ZoneArray.UpdateDimensions(1, Model.ModflowGrid.RowCount,
      Model.ModflowGrid.ColumnCount, True);

    MultiplierArray.UpToDate := True;
    ZoneArray.UpToDate := True;

    // initialize multiplier and zone arrays.
    for RowIndex := 0 to Model.ModflowGrid.RowCount - 1 do
    begin
      for ColIndex := 0 to Model.ModflowGrid.ColumnCount - 1 do
      begin
        MultiplierArray.RealData[0,RowIndex,ColIndex] := 0;
        ZoneArray.BooleanData[0,RowIndex,ColIndex] := False;
        MultiplierArray.Annotation[0,RowIndex,ColIndex] := DummyAnnotation;
        ZoneArray.Annotation[0,RowIndex,ColIndex] := DummyAnnotation;
      end;
    end;

    for CellIndex := 0 to CellList.Count - 1 do
    begin
      Cell := CellList[CellIndex];
      case AssignmentMethod of
        umAssign:
          begin
            MultiplierArray.RealData[0, Cell.Row, Cell.Column] :=
              Cell.RealValue[0, Model];
          end;
        umAdd:
          begin
            MultiplierArray.RealData[0, Cell.Row, Cell.Column] :=
              MultiplierArray.RealData[0, Cell.Row, Cell.Column]
              + Cell.RealValue[0, Model];
          end;
        else Assert(False);
      end;
      ZoneArray.BooleanData[0, Cell.Row, Cell.Column] := True;
    end;

    MultiplierArray.UpToDate := True;
    ZoneArray.UpToDate := True;
    if ParameterType in [ptRCH, ptEVT, ptETS] then
    begin
      Model.AdjustDataArray(MultiplierArray);
      Model.AdjustDataArray(ZoneArray);
    end;
    MultiplierArray.CheckIfUniform;
    if (MultiplierArray.IsUniform = iuTrue) and
      (MultiplierArray.RealData[0,0,0] = 1) then
    begin
      WriteString('NONE ');
    end
    else
    begin
      IdenticalMultiplierArray := Model.
        IndenticalTransientMultiplierArray(MultiplierArray);
      if IdenticalMultiplierArray = nil then
      begin
        Assert(Length(MultiplierArray.Name) <= 10);
        WriteString(MultiplierArray.Name);

        MultItem := MultiplierArrayNames.Add;
        MultItem.ArrayName := MultiplierArray.Name;
        MultItem.Uniform := CheckArrayUniform(0, MultiplierArray);
        if MultItem.Uniform then
        begin
          MultItem.UniformValue := MultiplierArray.RealData[0,0,0];
        end
        else
        begin
          TransMultWriter := TTransientArrayWriter.Create(Model, FEvaluationType);
          try
            MultItem.FileName :=  TransMultWriter.WriteFile(FNameOfFile,
              TModflowMultiplierWriter.ArrayType, MultiplierArray)
          finally
            TransMultWriter.Free;
          end;
        end;

        WriteString(' ');
        MultiplierArray.CacheData;
        Model.TransientMultiplierArrays.Add(MultiplierArray);
        MultiplierArray := nil;
      end
      else
      begin
        WriteString(IdenticalMultiplierArray.Name);
        WriteString(' ');
      end;
    end;

    ZoneArray.UpToDate := True;
    ZoneArray.CheckIfUniform;
    if (ZoneArray.IsUniform = iuTrue) and
      (ZoneArray.BooleanData[0,0,0] = True) then
    begin
      WriteString('ALL');
    end
    else
    begin
      IdenticalZoneArray := Model.IndenticalTransientZoneArray(ZoneArray);
      if IdenticalZoneArray = nil then
      begin
        Assert(Length(ZoneArray.Name) <= 10);
        WriteString(ZoneArray.Name);
//        ZoneArrayNames.Add(ZoneArray.Name);

        ZoneItem := ZoneArrayNames.Add;
        ZoneItem.ArrayName := ZoneArray.Name;
        ZoneItem.Uniform := CheckArrayUniform(0, ZoneArray);
        if ZoneItem.Uniform then
        begin
          ZoneItem.UniformValue := ZoneArray.IntegerData[0,0,0];
        end
        else
        begin
          TransMultWriter := TTransientArrayWriter.Create(Model, FEvaluationType);
          try
            ZoneItem.FileName :=  TransMultWriter.WriteFile(FNameOfFile,
              TModflowZoneWriter.ArrayType, ZoneArray)
          finally
            TransMultWriter.Free;
          end;
        end;


        WriteString(' 1');
        ZoneArray.CacheData;
        Model.TransientZoneArrays.Add(ZoneArray);
        ZoneArray := nil;
      end
      else
      begin
        WriteString(IdenticalZoneArray.Name);
        WriteString(' 1');
      end;
    end;

    NewLine;
  finally
    MultiplierArray.Free;
    ZoneArray.Free
  end;
end;

procedure TCustomTransientArrayWriter.EvaluateParameterDefinitions(List: TList;
  const ErrorRoot: string; AssignmentMethod: TUpdateMethod);
var
  ParamIndex: Integer;
  Param: TModflowTransientListParameter;
  Position: Integer;
  ParameterValues: TList;
  NUMINST: Integer;
  NCLU: Integer;
  TimeIndex: Integer;
  CellList: TValueCellList;
  PARTYP: string;
  ErrorMessage: string;
  DataArray: TModflowBoundaryDisplayDataArray;
  DataArrayList: TObjectList;
  SkippedParamWarning: string;
  NoParametersError: string;
  ParameterActive: Boolean;
  ParametersUsed: Boolean;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    case ParameterType of
      ptUndefined..ptLPF_VKCB: Assert(False);
      ptRCH, ptEVT, ptETS, ptCHD..ptDRT, ptQMAX: PARTYP := ' '
        + ParmeterTypeToStr(ParameterType);
      Else Assert(False);
    end;
    ErrorMessage := Format(ErrorRoot, [Trim(PARTYP)]);
    SkippedParamWarning := Format(StrTheFollowingParameSkip, [Trim(PARTYP)]);
    NoParametersError := Format(StrNoParametersAreBe, [Trim(PARTYP)]);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, ErrorMessage);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, SkippedParamWarning);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, NoParametersError);

    ParameterActive := False;
    ParametersUsed := False;
    for ParamIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
    begin
      Param := Model.ModflowTransientParameters[ParamIndex];
      if Param.ParameterType = ParameterType then
      begin
        ParametersUsed := True;
        Position := ParamValues.IndexOf(Param.ParameterName);
        // The parameter name is erased from ParamValues in
        // CountParametersAndParameterCells if there are no cells
        // associated with it.
        if Position < 0 then
        begin
          frmErrorsAndWarnings.AddWarning(Model,
            ErrorMessage, Param.ParameterName);
          Continue;
        end;
        ParameterValues := ParamValues.Objects[Position] as TList;
        if ParameterValues.Count = 0 then
        begin
          Continue;
        end;
        ParameterActive := True;
        Assert(ParameterValues.Count > 0);
        // Data set 3
        GetNumCellsAndNumInstances(ParameterValues, NUMINST, NCLU);
        while FNumInstList.Count <= Position do
        begin
          FNumInstList.Add(0);
        end;
        FNumInstList[Position] := NUMINST;
//        FNumInstList.Add(NUMINST);
        if NCLU > 0 then
        begin
          NCLU := 1;
          // Make sure the maximum length of the name of instance is <= 10.
    //      GetInstanceRoot(PARNAM, ParameterValues, InstanceRoot);
          // Data sets 4a and 4b
          for TimeIndex := 0 to ParameterValues.Count - 1 do
          begin
            if List.Count > TimeIndex then
            begin
              DataArrayList := List[TimeIndex];
            end
            else
            begin
              DataArrayList:= TObjectList.Create;
              List.Add(DataArrayList);
            end;
            CellList := ParameterValues[TimeIndex];
            if CellList.Count > 0 then
            begin
              // Data set 4a
              DataArray:= TModflowBoundaryDisplayDataArray.Create(Model);
              DataArray.Orientation := dso3D;
              DataArray.EvaluatedAt := eaBlocks;
              DataArray.UpdateDimensions(Model.LayerCount,
                Model.RowCount,
                Model.ColumnCount);
              DataArrayList.Add(DataArray);
              EvaluateParameterCells(CellList, DataArray, Param, AssignmentMethod);
              DataArray.CacheData;
            end;
          end;
        end
        else
        begin
          frmErrorsAndWarnings.AddWarning(Model,
            SkippedParamWarning, Param.ParameterName);
        end;
      end;
    end;
    if ParametersUsed and not ParameterActive then
    begin
      frmErrorsAndWarnings.AddError(Model, NoParametersError,
        NoParametersError);
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

function TCustomTransientArrayWriter.ParameterCount: integer;
var
  ParamIndex: Integer;
  Param: TModflowTransientListParameter;
  Position: Integer;
  ParameterValues: TList;
  NUMINST: Integer;
  NCLU: Integer;
begin
  result := 0;
  for ParamIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
  begin
    Param := Model.ModflowTransientParameters[ParamIndex];
    if Param.ParameterType = ParameterType then
    begin
      Position := ParamValues.IndexOf(Param.ParameterName);
      // The parameter name is erased from ParamValues in
      // CountParametersAndParameterCells if there are no cells
      // associated with it.
      if Position < 0 then
      begin
//        if frmErrorsAndWarnings <> nil then
//        begin
//          frmErrorsAndWarnings.AddWarning(Model,
//            ErrorMessage, Param.ParameterName);
//        end;
        Continue;
      end;
      ParameterValues := ParamValues.Objects[Position] as TList;
      if ParameterValues.Count = 0 then
      begin
        Continue;
      end;
      Assert(ParameterValues.Count > 0);
      // Data set 3
      GetNumCellsAndNumInstances(ParameterValues, NUMINST, NCLU);
      while FNumInstList.Count <= Position do
      begin
        FNumInstList.Add(0);
      end;
      FNumInstList[Position] := NUMINST;
//      FNumInstList.Add(NUMINST);
      if NCLU > 0 then
      begin
        Inc(result);
      end;
    end;
  end;

end;

procedure TCustomTransientArrayWriter.WriteParameterDefinitions(
  const DS3, DS3Instances, DS4A, DataSetIdentifier,
  VariableIdentifiers, ErrorRoot: string; AssignmentMethod: TUpdateMethod;
  MultiplierArrayNames: TTransientMultCollection;
  ZoneArrayNames: TTransientZoneCollection);
var
  ParamIndex: Integer;
  Param: TModflowTransientListParameter;
  Position: Integer;
  ParameterValues: TList;
  PARNAM: string;
  NUMINST: Integer;
  NCLU: Integer;
  Parval: Double;
  InstanceRoot: string;
  TimeIndex: Integer;
  CellList: TValueCellList;
  InstanceName: string;
  PARTYP: string;
  ErrorMessage: string;
  SkippedParamWarning: string;
  First: Boolean;
begin
  case ParameterType of
    ptUndefined..ptLPF_VKCB: Assert(False);
    ptRCH, ptEVT, ptETS, ptCHD..ptDRT, ptQMAX: PARTYP := ' '
      + ParmeterTypeToStr(ParameterType);
    Else Assert(False);
  end;
  ErrorMessage := Format(ErrorRoot, [Trim(PARTYP)]);
  SkippedParamWarning := Format(StrTheFollowingParameSkip, [Trim(PARTYP)]);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, ErrorMessage);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, SkippedParamWarning);
  for ParamIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    Param := Model.ModflowTransientParameters[ParamIndex];
    if Param.ParameterType = ParameterType then
    begin
      Position := ParamValues.IndexOf(Param.ParameterName);
      // The parameter name is erased from ParamValues in
      // CountParametersAndParameterCells if there are no cells
      // associated with it.
      if Position < 0 then
      begin
        frmErrorsAndWarnings.AddWarning(Model,
          ErrorMessage, Param.ParameterName);
        Continue;
      end;
      ParameterValues := ParamValues.Objects[Position] as TList;
      if ParameterValues.Count = 0 then
      begin
        frmErrorsAndWarnings.AddWarning(Model,
          ErrorMessage, Param.ParameterName);
        Continue;
      end;
      Assert(ParameterValues.Count > 0);
      // Data set 3
      PARNAM := Param.ParameterName;
      if Length(PARNAM) > 10 then
      begin
        SetLength(PARNAM, 10);
      end;
      frmProgressMM.AddMessage(Format(StrWritingParamter, [PARNAM]));
      GetNumCellsAndNumInstances(ParameterValues, NUMINST, NCLU);
      if NCLU > 0 then
      begin
        NCLU := 1;
        Parval := Param.Value;
        WriteString(PARNAM);
        WriteString(PARTYP);
        WriteFloat(Parval);
        WriteInteger(NCLU);
        if NUMINST > 1 then
        begin
          WriteString(' INSTANCES');
          WriteInteger(NUMINST);
        end;
        WriteString(DS3);
        if NUMINST > 1 then
        begin
          WriteString(DS3Instances);
        end;
        NewLine;

        if not WritingTemplate then
        begin
          Model.WritePValAndTemplate(PARNAM,PARVAL,Param);
        end;

        // Make sure the maximum length of the name of instance is <= 10.
        GetInstanceRoot(PARNAM, ParameterValues, InstanceRoot);
        // Data sets 4a and 4b
        First := True;
        for TimeIndex := 0 to ParameterValues.Count - 1 do
        begin
          Application.ProcessMessages;
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;
          CellList := ParameterValues[TimeIndex];
          if CellList.Count > 0 then
          begin
            if First or (NUMINST > 1) then
            begin
              First := False;
              // Data set 4a
              if NUMINST > 1 then
              begin
                GetInstanceName(InstanceName, TimeIndex, InstanceRoot);
                WriteString(InstanceName);
                WriteString(DS4A + ' (Parameter instance for stress period '
                  + IntToStr(TimeIndex+1) + ')');
                NewLine;
              end;
              WriteParameterCells(CellList, NCLU, VariableIdentifiers,
                DataSetIdentifier, AssignmentMethod, MultiplierArrayNames, ZoneArrayNames);
            end;
          end;
          CellList.Cache;
        end;
      end
      else
      begin
        frmErrorsAndWarnings.AddWarning(Model,
          SkippedParamWarning, Param.ParameterName);
      end;
    end;
  end;
end;

procedure TCustomTransientArrayWriter.WriteStressPeriods(
  const VariableIdentifiers, DataSetIdentifier, DS5, D7PNameIname,
  D7PName: string);
begin
  FUsedInstanceNames.Clear;
end;

constructor TCustomTransientArrayWriter.Create(Model: TCustomModel; EvaluationType: TEvaluationType);
begin
  inherited;
  FLayers := TObjectList.Create;
end;

destructor TCustomTransientArrayWriter.Destroy;
begin
  FLayers.Free;
  inherited;
end;

procedure TCustomTransientArrayWriter.WriteLayerArray(Lists: TList;
  const Comment: string; const MF6_ArrayName: string);
var
  Dummy: TDataArray;
begin
  WriteTransient2DArray(Comment, 0, rdtInteger, 1, Lists, umAssign,
    False, Dummy, MF6_ArrayName);
end;

procedure TCustomTransientArrayWriter.UpdateLayerDataSet(List: TList;
  DisplayArray: TModflowBoundaryDisplayDataArray);
var
  DefaultValue: integer;
  DataTypeIndex: integer;
  RowIndex: Integer;
  ColIndex: Integer;
  LayerArray: TIntegerSparseDataSet;
  InderListIndex: Integer;
  InnerList: TValueCellList;
var
  DataArrayList: TList;
  AnObject: TObject;
  ListIndex: integer;
  Layer, LayerIndex: integer;
  procedure AssignCellValues(CellList: TValueCellList);
  var
    CellIndex: Integer;
    Cell: TValueCell;
  begin
    for CellIndex := 0 to CellList.Count - 1 do
    begin
      Cell := CellList[CellIndex] as TValueCell;
      LayerArray.IntegerData[0, Cell.Row, Cell.Column] :=
        Cell.IntegerValue[DataTypeIndex, Model];
      LayerArray.Annotation[0, Cell.Row, Cell.Column] :=
        Cell.IntegerAnnotation[DataTypeIndex, Model];
    end;
  end;
begin
  LayerArray := TIntegerSparseDataSet.Create(Model);
  try
    LayerArray.Orientation := dsoTop;
    LayerArray.EvaluatedAt := eaBlocks;
    LayerArray.UpdateDimensions(Model.LayerCount,
      Model.RowCount, Model.ColumnCount);

    DefaultValue := 1;
    DataTypeIndex := 0;
    if List.Count > 0 then
    begin
      AnObject := List[0];
      if AnObject is TList then
      begin
        for ListIndex := 0 to List.Count - 1 do
        begin
          DataArrayList := List[ListIndex];
          if DataArrayList.Count > 0 then
          begin
            AnObject := DataArrayList[0];
            if AnObject is TList then
            begin
              for InderListIndex := 0 to DataArrayList.Count - 1 do
              begin
                InnerList := DataArrayList[InderListIndex];
                AssignCellValues(InnerList);
              end;
            end
            else
            begin
              AssignCellValues(DataArrayList as TValueCellList);
            end;
          end;
        end;
      end
      else
      begin
        AssignCellValues(List as TValueCellList);
      end;
    end
    else if List is TValueCellList then
    begin
      AssignCellValues(TValueCellList(List));
    end;

    for RowIndex := 0 to Model.ModflowGrid.RowCount - 1 do
    begin
      for ColIndex := 0 to Model.ModflowGrid.ColumnCount - 1 do
      begin
        if not LayerArray.IsValue[0, RowIndex, ColIndex] then
        begin
          LayerArray.IntegerData[0, RowIndex, ColIndex] := DefaultValue;
          LayerArray.Annotation[0, RowIndex, ColIndex] := StrNoValueAssigned;
        end;
      end;
    end;
    for RowIndex := 0 to Model.ModflowGrid.RowCount - 1 do
    begin
      for ColIndex := 0 to Model.ModflowGrid.ColumnCount - 1 do
      begin
        Assert(LayerArray.IsValue[0, RowIndex, ColIndex]);
        Layer := LayerArray.IntegerData[0, RowIndex, ColIndex];
        if (Layer >= 1) and
          (Layer <= Model.ModflowLayerCount) then
        begin
          LayerIndex := Model.
            ModflowLayerToDataSetLayer(Layer);
          if (LayerIndex >= 0) and (LayerIndex < DisplayArray.LayerCount) then
          begin
            DisplayArray.RealData[LayerIndex, RowIndex, ColIndex] := Layer;
            DisplayArray.Annotation[LayerIndex, RowIndex, ColIndex]
              := LayerArray.Annotation[0, RowIndex, ColIndex];
            DisplayArray.CellCount[LayerIndex, RowIndex, ColIndex] := 1;
          end;
        end;
      end;
    end;
    DisplayArray.UpToDate := True;
  finally
    LayerArray.Free;
  end;
end;

procedure TCustomTransientArrayWriter.UpdateLayerDisplay(
  List: TValueCellList; ParameterValues: TList; TimeIndex: Integer;
  DataArray: TModflowBoundaryDisplayDataArray);
var
  CellList: TValueCellList;
  LocalPackage : TCustomTransientLayerPackageSelection;
begin
  LocalPackage := Package as TCustomTransientLayerPackageSelection;
  if LocalPackage.LayerOption = loSpecified then
  begin
    if LocalPackage.TimeVaryingLayers then
    begin
      if TimeIndex < FLayers.Count then
      begin
        CellList := FLayers[TimeIndex];
//        CellList.CheckRestore;
        UpdateLayerDataSet(CellList, DataArray);
      end
      else
      begin
        InitializeDisplayArray(DataArray, 1);
        DataArray.UpToDate := True;
      end;
    end
    else if ParameterCount > 0 then
    begin
      UpdateLayerDataSet(ParameterValues, DataArray);
    end
    else
    begin
      UpdateLayerDataSet(List, DataArray);
    end;
  end;
end;

procedure TCustomTransientArrayWriter.WriteLayerSelection(
  List: TValueCellList; ParameterValues: TList; TimeIndex: Integer;
  const Comment: string; const MF6_ArrayName: string);
var
  LayerList: TValueCellList;
  LocalPackage: TCustomTransientLayerPackageSelection;
begin
  LocalPackage := Package as TCustomTransientLayerPackageSelection;
  if LocalPackage.LayerOption = loSpecified then
  begin
    if LocalPackage.TimeVaryingLayers then
    begin
      if TimeIndex < FLayers.Count then
      begin
        LayerList := FLayers[TimeIndex];
        WriteLayerArray(LayerList, Comment, MF6_ArrayName);
      end
      else
      begin
        LayerList := TValueCellList.Create(CellType);
        try
          WriteLayerArray(LayerList, Comment, MF6_ArrayName);
        finally
          LayerList.Free;
        end;
      end;
    end
    else if ParameterCount > 0 then
    begin
      WriteLayerArray(ParameterValues, Comment, MF6_ArrayName);
    end
    else
    begin
      WriteLayerArray(List, Comment, MF6_ArrayName);
    end;
  end;
  if (LocalPackage.LayerOption = loTop)
    and (Model.ModelSelection = msModflow2015)
    and (TimeIndex = 0) then
  begin
    WriteString('  ');
    WriteString(MF6_ArrayName);
    NewLine;
    WriteString('    CONSTANT');
    WriteInteger(1);
    NewLine;
  end;
end;

procedure TCustomParameterTransientWriter.RetrieveParametersForStressPeriod(
  const D7PNameIname, D7PName: string; TimeIndex: Integer;
  ParametersUsed: TStringList; ParameterValues: TList;
  IncludePrintCode: boolean);
var
  PARNAM: string;
  InstanceRoot: string;
  NUMINST: Integer;
//  NLST: Integer;
  List: TValueCellList;
  InstanceName: string;
  ParamIndex: Integer;
  PValues : TList;
  PrintCode: String;
begin
  ParameterValues.Clear;
  if IncludePrintCode then
  begin
    PrintCode := ' ' + IntToStr(IPRN_Real);
  end
  else
  begin
    PrintCode := '';
  end;
  for ParamIndex := 0 to ParamValues.Count - 1 do
  begin
    PARNAM := ParamValues[ParamIndex];
    if PARNAM <> '' then
    begin
      if Length(PARNAM) > 10 then
      begin
        SetLength(PARNAM, 10);
      end;
      // The number of items in PValues is
      // equal to the number of stress periods.
      // Each item in PValues is a TValueCellList.
      PValues := ParamValues.Objects[ParamIndex] as TList;
      ParameterValues.Add(PValues);
      GetInstanceRoot(PARNAM, PValues, InstanceRoot);
//      GetNumCellsAndNumInstances(PValues, NUMINST, NLST);
      List := PValues[TimeIndex];
      if (List.Count > 0) then
      begin
        NUMINST := FNumInstList[ParamIndex];
        if NUMINST > 1 then
        begin
          GetInstanceName(InstanceName, TimeIndex, InstanceRoot);
          ParametersUsed.Add(PARNAM + ' ' + InstanceName
            + PrintCode + D7PNameIname);
        end
        else
        begin
          ParametersUsed.Add(PARNAM + PrintCode + D7PName);
        end;
      end;
    end;
  end;
end;

procedure TCustomPackageWriter.WriteDataSet0;
var
  Index: integer;
  APackage: TModflowPackageSelection;
begin
  APackage := Package;
  WriteCommentLine(PackageID_Comment(APackage));
  if WritingTemplate then
  begin
    WriteCommentLine('(and then modified by a parameter estimation program.)');
  end;
  for Index := 0 to APackage.Comments.Count - 1 do
  begin
    WriteCommentLine(APackage.Comments[Index]);
  end;
end;

procedure TCustomPackageWriter.UpdateNotUsedDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
begin
  if not Package.IsSelected then
  begin
    SetTimeListsUpToDate(TimeLists);
  end;
end;

procedure TCustomModflowWriter.HandleMissingArrayData;
begin
  // do nothing
end;

procedure TCustomModflowWriter.WriteTransient2DArray(const Comment: string;
  DataTypeIndex: Integer; DataType: TRbwDataType; DefaultValue: Double;
  List: TList; AssignmentMethod: TUpdateMethod; AdjustForLGR: boolean;
  var TransientArray: TDataArray; const MF6_ArrayName: string;
  FreeArray: boolean = True; ShouldWriteHeader: Boolean = True;
  ForceFullArray: Boolean = False);
var
  ColIndex: Integer;
  RowIndex: Integer;
  ExportArray: TDataArray;
  IntDefaultValue: Integer;
  CellList: TList;
  InnerCellList: TValueCellList;
  AnObject: TObject;
  ListIndex: integer;
  CellListIndex: Integer;
  DummyAnnotation: string;
  ValuesAssigned: Boolean;
  RowCount: Integer;
  ColumnCount: Integer;
  Formulas: TTwoDStringArray;
  FormulasApplied: T2DBoolArray;
  IsFormula: Boolean;
  Formula: string;
  FormulasUsed: Boolean;
  ExtendedTemplateCharacter: string;
  procedure AssignCellValues(CellList: TValueCellList);
  var
    CellIndex: Integer;
    Cell: TValueCell;
    Value: Double;
    PestName: string;
    PestSeriesName: string;
    PestSeriesMethod: TPestParamMethod;
    ACellLocation: TCellLocation;
    ModifiedValue: Double;
  begin
    for CellIndex := 0 to CellList.Count - 1 do
    begin
      Cell := CellList[CellIndex] as TValueCell;
      ValuesAssigned := True;
      case DataType of
        rdtDouble:
          begin
            case AssignmentMethod of
              umAssign:
                begin
                  ExportArray.RealData[0, Cell.Row, Cell.Column] :=
                    Cell.RealValue[DataTypeIndex, Model];
                  ExportArray.Annotation[0, Cell.Row, Cell.Column] :=
                    Cell.RealAnnotation[DataTypeIndex, Model];
                end;
              umAdd:
                begin
                  ExportArray.RealData[0, Cell.Row, Cell.Column] :=
                    ExportArray.RealData[0, Cell.Row, Cell.Column]
                    + Cell.RealValue[DataTypeIndex, Model];
                  ExportArray.Annotation[0, Cell.Row, Cell.Column] :=
                    ExportArray.Annotation[0, Cell.Row, Cell.Column]
                    + Cell.RealAnnotation[DataTypeIndex, Model];
                end;
              else Assert(False);
            end;
            Formula := FortranFloatToStr(Cell.RealValue[DataTypeIndex, Model]);
            PestName := Cell.PestName[DataTypeIndex];
            PestSeriesName := Cell.PestSeriesName[DataTypeIndex];

            if (PestName <> '') or (PestSeriesName <> '') then
            begin
              FPestParamUsed := True;
              FormulasUsed := True;
            end;
            if WritingTemplate then
            begin
              if (PestName <> '') or (PestSeriesName <> '') then
              begin
                Assert(Cell <> nil);
                Value := Cell.RealValue[DataTypeIndex, Model];
                PestSeriesMethod := Cell.PestSeriesMethod[DataTypeIndex];
                ACellLocation := Cell.CellLocation;
                Formula := GetPestTemplateFormula(Value, PestName, PestSeriesName,
                  PestSeriesMethod, PCellLocation(Addr(ACellLocation)),
                  Cell.ScreenObject as TScreenObject, ModifiedValue);
                if Formula = '' then
                begin
                  IsFormula := False;
                  Formula := FortranFloatToStr(Value)
                end
                else
                begin
                  IsFormula := True;
                end;

                if Formulas[Cell.Row, Cell.Column] = '' then
                begin
                  Formulas[Cell.Row, Cell.Column] := Formula;
                  FormulasApplied[Cell.Row, Cell.Column] := IsFormula;
                end
                else
                begin
                  case AssignmentMethod of
                    umAssign:
                      begin
                        Formulas[Cell.Row, Cell.Column] := Formula;
                        FormulasApplied[Cell.Row, Cell.Column] := IsFormula;
                      end;
                    umAdd:
                      begin
                        Formulas[Cell.Row, Cell.Column] := Format('%0:s + %1:s',
                          [Formulas[Cell.Row, Cell.Column], Formula]);
                        FormulasApplied[Cell.Row, Cell.Column] := True;
                      end;
                  end;
                end;
              end
              else
              begin
                if Formulas[Cell.Row, Cell.Column] = '' then
                begin
                  Formulas[Cell.Row, Cell.Column] := Formula;
                  FormulasApplied[Cell.Row, Cell.Column] := False;
                end
                else
                begin
                  case AssignmentMethod of
                    umAssign:
                      begin
                        Formulas[Cell.Row, Cell.Column] := Formula;
                        FormulasApplied[Cell.Row, Cell.Column] := False;
                      end;
                    umAdd:
                      begin
                        Formulas[Cell.Row, Cell.Column] := Format('%0:s + %1:s',
                          [Formulas[Cell.Row, Cell.Column], Formula]);
                        FormulasApplied[Cell.Row, Cell.Column] := True;
                      end;
                  end;
                end;
              end;

            end;
          end;
        rdtInteger:
          begin
            case AssignmentMethod of
              umAssign:
                begin
                  ExportArray.IntegerData[0, Cell.Row, Cell.Column] :=
                    Cell.IntegerValue[DataTypeIndex, Model];
                  ExportArray.Annotation[0, Cell.Row, Cell.Column] :=
                    Cell.IntegerAnnotation[DataTypeIndex, Model];
                end;
              umAdd:
                begin
                  ExportArray.IntegerData[0, Cell.Row, Cell.Column] :=
                    ExportArray.IntegerData[0, Cell.Row, Cell.Column]
                    + Cell.IntegerValue[DataTypeIndex, Model];
                  ExportArray.Annotation[0, Cell.Row, Cell.Column] :=
                    ExportArray.Annotation[0, Cell.Row, Cell.Column]
                    + Cell.IntegerAnnotation[DataTypeIndex, Model];
                end;
              else Assert(False);
            end;
          end;
        rdtBoolean:
          begin
            case AssignmentMethod of
              umAssign:
                begin
                  ExportArray.BooleanData[0, Cell.Row, Cell.Column] :=
                    Cell.BooleanValue[DataTypeIndex, Model];
                  ExportArray.Annotation[0, Cell.Row, Cell.Column] :=
                    Cell.BooleanAnnotation[DataTypeIndex, Model];
                end;
              umAdd:
                begin
                  ExportArray.BooleanData[0, Cell.Row, Cell.Column] :=
                    ExportArray.BooleanData[0, Cell.Row, Cell.Column]
                    or Cell.BooleanValue[DataTypeIndex, Model];
                  ExportArray.Annotation[0, Cell.Row, Cell.Column] :=
                    ExportArray.Annotation[0, Cell.Row, Cell.Column]
                    + Cell.BooleanAnnotation[DataTypeIndex, Model];
                end;
              else Assert(False);
            end;
          end;
      else
        Assert(False);
      end;
    end;
  end;
begin
  if Model.ModelSelection in [msModflowLGR2, msModflowFmp, msModflowOwhm2] then
  begin
    AdjustForLGR := False;
  end;
  FormulasUsed := False;
  DummyAnnotation := 'none';
  ExportArray := TDataArray.Create(Model);
  try
    IntDefaultValue := Round(DefaultValue);
    ExportArray.Orientation := dsoTop;
    ExportArray.EvaluatedAt := eaBlocks;
    ExportArray.DataType := DataType;
    RowCount := Model.RowCount;
    ColumnCount := Model.ColumnCount;
    if WritingTemplate then
    begin
      SetLength(Formulas, RowCount, ColumnCount);
      SetLength(FormulasApplied, RowCount, ColumnCount);
    end;

    ExportArray.UpdateDimensions(1, RowCount, ColumnCount, True);

    ExportArray.UpToDate := True;

    for RowIndex := 0 to RowCount - 1 do
    begin
      for ColIndex := 0 to ColumnCount - 1 do
      begin
        ExportArray.Annotation[0, RowIndex, ColIndex] := DummyAnnotation;
        case DataType of
          rdtDouble:
            begin
              ExportArray.RealData[0, RowIndex, ColIndex] := DefaultValue;
            end;
          rdtInteger:
            begin
              ExportArray.IntegerData[0, RowIndex, ColIndex] := IntDefaultValue;
            end;
          rdtBoolean:
            begin
              ExportArray.BooleanData[0, RowIndex, ColIndex] := IntDefaultValue <> 0;
            end;
        else
          Assert(False);
        end;
        if WritingTemplate then
        begin
          Formulas[RowIndex, ColIndex] := '';
          FormulasApplied[RowIndex, ColIndex] := False;
        end;
      end;
    end;

    // List may have been cast to a TValueCellList even when it wasn't one.
    if List.Count > 0 then
    begin
      AnObject := List[0];
    end
    else
    begin
      AnObject := nil;
    end;

    ValuesAssigned := False;
    if (AnObject = nil) or (AnObject is TValueCell) then
    begin
      AssignCellValues(List as TValueCellList);
      TValueCellList(List).Cache;
    end
    else if List.Count > 0 then
    begin
      AnObject := List[0];
      if AnObject is TValueCellList then
      begin
        for ListIndex := 0 to List.Count - 1 do
        begin
          CellList := List[ListIndex];
          AssignCellValues(CellList as TValueCellList);
          TValueCellList(CellList).Cache;
        end;
      end
      else if AnObject is TList then
      begin
        for ListIndex := 0 to List.Count - 1 do
        begin
          CellList := List[ListIndex];
          if CellList.Count > 0 then
          begin
            for CellListIndex := 0 to CellList.Count - 1 do
            begin
              InnerCellList := CellList[CellListIndex];
              AssignCellValues(InnerCellList);
              InnerCellList.Cache;
            end;
          end;
        end;
      end
    end;
    if not ValuesAssigned then
    begin
      HandleMissingArrayData;
    end;

    if AdjustForLGR then
    begin
      Model.AdjustDataArray(ExportArray);
    end;
    ExportArray.Name := MF6_ArrayName;
    ExportArray.UpToDate := True;
    if WritingTemplate and FormulasUsed then
    begin
      ExtendedTemplateCharacter := Model.PestProperties.ExtendedTemplateCharacter;
      if ShouldWriteHeader then
      begin
        WriteHeader(rdtDouble, Comment, MF6_ArrayName);
      end;
      for RowIndex := 0 to RowCount - 1 do
      begin
        for ColIndex := 0 to ColumnCount - 1 do
        begin
          if FormulasApplied[RowIndex, ColIndex] then
          begin
            WriteString( Format(' %0:s                    %1:s%0:s ',
              [ExtendedTemplateCharacter, Formulas[RowIndex, ColIndex]]));
          end
          else
          begin
            WriteFloat(ExportArray.RealData[0, RowIndex, ColIndex]);
          end;
          NewLine;
        end;
      end;
    end
    else
    begin
      WriteArray(ExportArray, 0, Comment, DummyAnnotation, MF6_ArrayName, False,
        ShouldWriteHeader, ForceFullArray);
    end;
  finally
    if FreeArray then
    begin
      TransientArray := nil;
      ExportArray.Free;
    end
    else
    begin
      TransientArray := ExportArray;
    end;
  end;
end;

{ TCustomParameterTransientWriter }


constructor TCustomParameterTransientWriter.Create(Model: TCustomModel; EvaluationType: TEvaluationType);
begin
  inherited;
  FParamValues := TStringList.Create;
  FParamValues.OwnsObjects := True;
  FUsedInstanceNames := TStringList.Create;
  FUsedInstanceNames.Sorted := True;
  // FParameterNames should not be sorted.
  FParameterNames := TStringList.Create;
  FNumInstList := TGenericIntegerList.Create;

end;

destructor TCustomParameterTransientWriter.Destroy;
begin
  FNumInstList.Free;
  FParameterNames.Free;
  FUsedInstanceNames.Free;
//  for Index := 0 to FParamValues.Count - 1 do
//  begin
//    FParamValues.Objects[Index].Free;
//  end;
  FParamValues.Free;
//  ScreenObjectLists.Free;

  inherited;
end;

class function TCustomTransientWriter.ObservationExtension: string;
begin
  result := '';
  Assert(False);
end;

function TCustomTransientWriter.ObservationFileName(AFileName: string): string;
begin
  if WritingTemplate then
  begin
    AFileName := ChangeFileExt(AFileName, '');
  end;
  result := ChangeFileExt(AFileName, ObservationExtension);
end;

class function TCustomTransientWriter.ObservationOutputExtension: string;
begin
  result := '';
  Assert(False);
end;

procedure TCustomParameterTransientWriter.StorePotentialObservationLocationsAndNames;
var
  ScreenObject: TScreenObject;
  Boundary: TModflowBoundary;
  CellList: TCellAssignmentList;
  ScreenObjectIndex: Integer;
  Mf6Obs: TModflow6Obs;
  CellIndex: Integer;
  ACell: TCellAssignment;
  IDomainArray: TDataArray;
begin
  if not Mf6ObservationsUsed then
  begin
    Exit;
  end;
  IDomainArray := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);
  for ScreenObjectIndex := Model.ScreenObjectCount - 1 downto 0 do
  begin
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    if not ScreenObject.UsedModels.UsesModel(Model) then
    begin
      Continue;
    end;
    Boundary := GetBoundary(ScreenObject);
    if (Boundary = nil) then
    begin
      if IsMf6Observation(ScreenObject) then
      begin
        if IsFlowObs(ScreenObject) then
        begin
          EnsureMf6CalibObservations(ScreenObject)
        end;
        CellList := TCellAssignmentList.Create;
        try
          ScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
          Mf6Obs := ScreenObject.Modflow6Obs;
          for CellIndex := 0 to CellList.Count - 1 do
          begin
            ACell := CellList[CellIndex];
            if (IDomainArray.IntegerData[ACell.Cell.Layer, ACell.Cell.Row,
              ACell.Cell.Column] > 0)
              then
            begin
              ObsLocationCheck[ACell.Cell.Layer, ACell.Cell.Row,
                ACell.Cell.Column] := Mf6Obs.Name;
            end;
          end;
        finally
          CellList.Free;
        end;
      end;
    end;
  end;
end;

function TCustomPackageWriter.UcodeObsNameOK(const AName: string): boolean;
const
  Letters = ['A'..'Z', 'a'..'z'];
  ValidCharacters = ['A'..'Z', 'a'..'z', '0'..'9',
    '_', '.', ':', '&', '#', '@'];
var
  Index: Integer;
begin
  result := (AName <> '') and not SameText(AName, 'dum')
    and (Length(AName) <= 20);
  if result then
  begin
    result := CharInSet(AName[1], Letters);
    if result then
    begin
      for Index := 2 to Length(AName) do
      begin
        result := CharInSet(AName[Index], ValidCharacters);
        if not result then
        begin
          break;
        end;
      end;
    end;
  end;
end;

procedure TCustomPackageWriter.UpdateCellDisplay(CellList: TValueCellList;
  DataArrayList: TList; ParameterIndicies: TByteSet;
  Param: TModflowTransientListParameter = nil; UsedIndicies: TByteSet = []);
var
  Cell: TValueCell;
  CellIndex: Integer;
  Value: double;
  Annotation: string;
  DataArrayIndex: Integer;
  DataArray: TModflowBoundaryDisplayDataArray;
  PriorAnnotation: string;
  TimeSeriesName: string;
begin
  // Data set 4b
  for DataArrayIndex := 0 to DataArrayList.Count - 1 do
  begin
    if UsedIndicies <> [] then
    begin
      if not (DataArrayIndex in UsedIndicies) then
      begin
        Continue;
      end;
    end;
    DataArray := DataArrayList[DataArrayIndex];
    for CellIndex := 0 to CellList.Count - 1 do
    begin
      Cell := CellList[CellIndex] as TValueCell;
      TimeSeriesName := Cell.Mf6TimeSeriesName[DataArrayIndex];
      if TimeSeriesName <> '' then
      begin
        Value := Model.Mf6TimesSeries.GetInterpolatedValue(Model,
          Model.ThreeDDisplayTime, TimeSeriesName);
        Annotation := Format(StrMODFLOWTimeSeries,
          [TimeSeriesName, Model.ThreeDDisplayTime]);
      end
      else if (Model.ModelSelection = msModflow2015) or
        ((Param = nil) or not (DataArrayIndex in ParameterIndicies)) then
      begin
        Value := Cell.RealValue[DataArrayIndex, Model];
        Annotation := Cell.RealAnnotation[DataArrayIndex, Model];
      end
      else
      begin
        Value := Cell.RealValue[DataArrayIndex, Model]*Param.Value;
        Annotation := Format(Str0sMultipliedByParam,
          [Cell.RealAnnotation[DataArrayIndex, Model],
          Param.ParameterName, Param.Value]);
//        Annotation := Cell.RealAnnotation[DataArrayIndex, Model]
//          + ' (multiplied by parameter '
//          + Param.ParameterName + ' = ' + FloatToStr(Param.Value) + ')';
      end;

      // Reduce the storage of duplicate strings.
      if PriorAnnotation <> Annotation then
      begin
        PriorAnnotation := Annotation;
      end;
      DataArray.AddDataValue(PriorAnnotation, Value,
        Cell.Column, Cell.Row, Cell.Layer);
    end;
  end;
end;

procedure TCustomPackageWriter.UpdateNotUsedDisplay(
  TimeLists: TList<TModflowBoundListOfTimeLists>);
var
  index: Integer;
begin
  for index := 0 to TimeLists.Count - 1 do
  begin
    UpdateNotUsedDisplay(TimeLists[index]);
  end;
end;

procedure TCustomPackageWriter.SetMvrWriter(const Value: TObject);
begin
  if Value <> nil then
  begin
    Assert(Value is TModflowMvrWriter);
  end;
  FMvrWriter := Value;
end;

procedure TCustomPackageWriter.SetTimeListsUpToDate(
  TimeLists: TList<TModflowBoundListOfTimeLists>);
var
  Index: Integer;
begin
  for Index := 0 to TimeLists.Count - 1 do
  begin
    SetTimeListsUpToDate(TimeLists[Index]);
  end;
end;

procedure TCustomPackageWriter.SetTimeListsUpToDate(TimeLists: TModflowBoundListOfTimeLists);
var
  List: TModflowBoundaryDisplayTimeList;
  ListIndex: Integer;
begin
  for ListIndex := 0 to TimeLists.Count - 1 do
  begin
    List := TimeLists[ListIndex];
    SetTimeListUpToDate(List);
  end;
end;

destructor TCustomPackageWriter.Destroy;
begin
  DSiTrimWorkingSet;
  FTimeSeriesFileNames.Free;
  inherited;
end;

procedure TCustomPackageWriter.SetTimeListUpToDate(
  List: TModflowBoundaryDisplayTimeList);
var
  TimeIndex: Integer;
  DataArray: TDataArray;
begin
  if List <> nil then
  begin
    for TimeIndex := 0 to List.Count - 1 do
    begin
      DataArray := List[TimeIndex];
      DataArray.UpToDate := True;
    end;
    List.SetUpToDate(True);
  end;
end;

function TCustomPackageWriter.ExpandString(Source: string;
  ALength: integer): string;
begin
  result := Source;
  while Length(result) < ALength do
  begin
    result := result + ' ';
  end;
end;

function TCustomFileWriter.File_Comment(const FileID: string): string;
begin
  result := FileID + ' file created on '
    + DateToStr(Now) + ' by ' + Model.ProgramName + ' version '
    + IModelVersion + '.';
end;

function TCustomModflowWriter.PackageID_Comment(
  APackage: TModflowPackageSelection): string;
begin
  result := File_Comment(APackage.PackageIdentifier);
end;

procedure TCustomModflowWriter.PrintListInputOption;
begin
  if Model.ModflowOutputControl.PrintInputCellLists then
  begin
    WriteString('    PRINT_INPUT');
    NewLine;
  end;
end;

procedure TCustomTransientWriter.UpdateTransient2DArray(
  DisplayArray: TModflowBoundaryDisplayDataArray; List: TList);
var
  DataArrayList: TList;
  AnObject: TObject;
  ListIndex: integer;
  DataArrayIndex: Integer;
  InnerDataArrayList: TList;
  procedure AssignCellValues(DataArrayList: TList);
  var
    DataArrayIndex: Integer;
    DataArray: TDataArray;
  begin
    for DataArrayIndex := 0 to DataArrayList.Count - 1 do
    begin
      DataArray := DataArrayList[DataArrayIndex];
      DisplayArray.AddDataArray(DataArray);
    end;
  end;
begin
//  if List is TValueCellList then
//  begin
//    TValueCellList(List).CheckRestore;
//  end;
  if List.Count > 0 then
  begin
    AnObject := List[0];
    if AnObject is TList then
    begin
//      if AnObject is TValueCellList then
//      begin
//        TValueCellList(AnObject).CheckRestore;
//      end;
      for ListIndex := 0 to List.Count - 1 do
      begin
        DataArrayList := List[ListIndex];
//        if DataArrayList is TValueCellList then
//        begin
//          TValueCellList(DataArrayList).CheckRestore;
//        end;
        if DataArrayList.Count > 0 then
        begin
          AnObject := DataArrayList[0];
          if AnObject is TList then
          begin
//            if AnObject is TValueCellList then
//            begin
//              TValueCellList(AnObject).CheckRestore;
//            end;
            for DataArrayIndex := 0 to DataArrayList.Count - 1 do
            begin
              InnerDataArrayList := DataArrayList[DataArrayIndex];
//              if InnerDataArrayList is TValueCellList then
//              begin
//                TValueCellList(InnerDataArrayList).CheckRestore;
//              end;
              AssignCellValues(InnerDataArrayList);
            end;
          end
          else
          begin
            AssignCellValues(DataArrayList);
          end;
        end;
      end;
    end
    else
    begin
      AssignCellValues(List);
    end;
  end;
  DisplayArray.UpToDate := True;
end;

procedure TCustomTransientWriter.InitializeDisplayArray(
  DisplayArray: TModflowBoundaryDisplayDataArray; DefaultValue: Double);
begin
  DisplayArray.InitializeDisplayArray(0);
end;

function TCustomTransientWriter.IsFlowObs(
  AScreenObject: TScreenObject): Boolean;
begin
  result := False;
end;

function TCustomTransientWriter.IsMf6Observation(
  AScreenObject: TScreenObject): Boolean;
begin
  result := False;
end;

function TCustomTransientWriter.IsMf6ToMvrObservation(
  AScreenObject: TScreenObject): Boolean;
begin
  result := False;
end;

function TCustomTransientWriter.IsMf6ToWellReductionObservation(
  AScreenObject: TScreenObject): Boolean;
begin
  result := False;
end;

class function TCustomTransientWriter.Mf6ObType: TObGeneral;
begin
  result := ogUndefined;
end;

class function TCustomTransientWriter.Mf6GwtObType: TObGwt;
begin
  result := ogwtUndefined;
end;

function TCustomTransientWriter.Mf6ObservationsUsed: Boolean;
begin
  result := False;
//  result := (Model.ModelSelection = msModflow2015)
//    and Model.ModflowPackages.Mf6ObservationUtility.IsSelected;
end;

//function TCustomTransientWriter.ObsFactors: TObservationFactors;
//begin
//  result := nil;
//end;
//
procedure TCustomTransientWriter.SetOwnsValueContents(const Value: Boolean);
begin
  (FValues as TObjectList).OwnsObjects := Value;
end;

procedure TCustomTransientWriter.AssignTransient2DArray(
      DisplayArray: TModflowBoundaryDisplayDataArray; DataTypeIndex: Integer;
      List: TList; DefaultValue: double; DataType: TRbwDataType;
      UpdateMethod: TUpdateMethod);
var
  CellList: TValueCellList;
  AnObject: TObject;
  ListIndex: integer;
  procedure AssignCellValues(CellList: TValueCellList);
  var
    CellIndex: Integer;
    Cell: TValueCell;
  begin
    for CellIndex := 0 to CellList.Count - 1 do
    begin
      Cell := CellList[CellIndex] as TValueCell;
      case UpdateMethod of
        umAssign:
          begin
            case DataType of
              rdtDouble:
                begin
                  DisplayArray.RealData[0, Cell.Row, Cell.Column]
                    := Cell.RealValue[DataTypeIndex, Model];
                  DisplayArray.Annotation[0, Cell.Row, Cell.Column]
                    := Cell.RealAnnotation[DataTypeIndex, Model];
                  DisplayArray.CellCount[0, Cell.Row, Cell.Column] := 1;
                end;
              rdtInteger:
                begin
                  DisplayArray.RealData[0, Cell.Row, Cell.Column]
                    := Cell.IntegerValue[DataTypeIndex, Model];
                  DisplayArray.Annotation[0, Cell.Row, Cell.Column]
                    := Cell.IntegerAnnotation[DataTypeIndex, Model];
                  DisplayArray.CellCount[0, Cell.Row, Cell.Column] := 1;
                end;
            else
              Assert(False);
            end;
          end;
        umAdd:
          begin
            case DataType of
              rdtDouble:
                begin
                  DisplayArray.AddDataValue(Cell.RealAnnotation[DataTypeIndex, Model],
                    Cell.RealValue[DataTypeIndex, Model], Cell.Column, Cell.Row, 0);
                end;
              rdtInteger:
                begin
                  DisplayArray.AddDataValue(Cell.IntegerAnnotation[DataTypeIndex, Model],
                    Cell.IntegerValue[DataTypeIndex, Model], Cell.Column, Cell.Row, 0);
                end;
            else
              Assert(False);
            end;
          end;
        else Assert(False);
      end;
    end;
  end;
begin
  InitializeDisplayArray(DisplayArray, DefaultValue);
  if (List.Count > 0)
    or ((List is TValueCellList) and (TValueCellList(List).Count > 0)) then
  begin
    if (List is TValueCellList) then
    begin
      AnObject := TValueCellList(List).Items[0];
    end
    else
    begin
      AnObject := List[0];
    end;
    if AnObject is TList then
    begin
      for ListIndex := 0 to List.Count - 1 do
      begin
        CellList := List[ListIndex];
        AssignCellValues(CellList);
      end;
    end
    else
    begin
      AssignCellValues(List as TValueCellList);
    end;
  end;
  DisplayArray.UpToDate := True;
end;

//function TFluxObsWriter.ObservationFileName(AFileName: string): string;
//begin
//  result := ChangeFileExt(AFileName, ObservationExtension);
//end;

function TFluxObsWriter.ObservationOutputFileName(AFileName: string): string;
begin
  result := ChangeFileExt(AFileName, ObservationOutputExtension);
end;

function TFluxObsWriter.ObsFactors: TFluxObservationGroups;
begin
  result := nil;
end;

procedure TFluxObsWriter.RemoveWarningGroups(ObservationGroup: TFluxObservationGroup);
begin
  frmErrorsAndWarnings.RemoveWarningGroup(Model,
    Format(EarlyTimeWarning, [GetFluxType(ObservationGroup)]));
  frmErrorsAndWarnings.RemoveWarningGroup(Model,
    Format(LateTimeWarning, [GetFluxType(ObservationGroup)]));
end;

procedure TFluxObsWriter.SavePestInstructionFile(OutputName: string);
begin
  if Model.PestUsed then
  begin
    FPestInstructionFile.WriteBOM := False;
    FPestInstructionFile.SaveToFile(OutputName + '.ins');
  end;
end;

function TFluxObsWriter.GetFluxType(ObservationGroup: TFluxObservationGroup): string;
begin
  case ObservationGroup.FluxObsType of
    fotHead: result := 'CHOB ';
    fotRiver: result := 'RVOB ';
    fotDrain: result := 'DROB ';
    fotGHB: result := 'GBOB ';
    fotSTR: result := 'STOB ';
  end;
end;

procedure TFluxObsWriter.EnsureMf6CalibObservations(AScreenObject: TScreenObject);
var
  Obs: TFluxObservationGroups;
  ObsIndex: Integer;
  ObsGroup: TFluxObservationGroup;
  ObserveIndex: Integer;
  CalibrationObservations: TMf6CalibrationObservations;
  CalibIndex: Integer;
  CalibObs: TFluxObservation;
  CalibItem: TMf6CalibrationObs;
  MyGuid: TGUID;
begin
  if ObservationPackage.IsSelected then
  begin
    Obs:= ObsFactors;
    if Obs <> nil then
    begin
      for ObsIndex := 0 to Obs.Count - 1 do
      begin
        ObsGroup := Obs[ObsIndex];
        ObserveIndex := ObsGroup.ObservationFactors.IndexOfScreenObject(AScreenObject);
        if ObserveIndex >= 0 then
        begin
          CalibrationObservations :=
            AScreenObject.Modflow6Obs.CalibrationObservations;

          for CalibIndex := 0 to ObsGroup.ObservationTimes.Count - 1 do
          begin
            CalibObs := ObsGroup.ObservationTimes[CalibIndex];
            if CalibrationObservations.IndexOfTimeAndType(CalibObs.Time, Mf6ObType) < 0 then
            begin
              CalibItem := CalibrationObservations.Add;
              CalibItem.ObSeries := osGeneral;
              CalibItem.ObGeneral := Mf6ObType;
              if CreateGUID(MyGuid) = 0 then
              begin
                CalibItem.GUID := GUIDToString(MyGuid);
              end;
              CalibItem.Time := CalibObs.Time;
              CalibItem.Name := Format('%0:s_%1:d_%2:s',
                [ObsGroup.ObservationName, ObserveIndex+1, CalibObs.ObsNameTimeString]);
            end;
          end;
//          break;
        end;
      end;
    end;
  end;

end;

function TFluxObsWriter.IsFlowObs(AScreenObject: TScreenObject): Boolean;
var
  Obs: TFluxObservationGroups;
  ObsIndex: Integer;
  ObsGroup: TFluxObservationGroup;
  ObserveIndex: Integer;
begin
  result := False;
  if ObservationPackage.IsSelected then
  begin
    Obs:= ObsFactors;
    if Obs <> nil then
    begin
      for ObsIndex := 0 to Obs.Count - 1 do
      begin
        ObsGroup := Obs[ObsIndex];
        ObserveIndex := ObsGroup.ObservationFactors.IndexOfScreenObject(
          AScreenObject);
        result := (ObserveIndex >= 0);
        if result then
        begin
          if AScreenObject.Modflow6Obs = nil then
          begin
            AScreenObject.CreateMf6Obs;
            AScreenObject.Modflow6Obs.General := [Mf6ObType];
            AScreenObject.Modflow6Obs.Name := Format('%0:s_%1:d',
              [ObsGroup.ObservationName, ObserveIndex + 1]);
          end
          else
          begin
            AScreenObject.Modflow6Obs.General :=
              AScreenObject.Modflow6Obs.General + [Mf6ObType];
          end;

          break;
        end;
      end;
    end;
  end;
end;

procedure TFluxObsWriter.WriteObservationDataSet4(ObservationGroup: TFluxObservationGroup;
      DataSet4: TStringList);
var
  ObsTime: TFluxObservation;
  OBSNAM: string;
  TOFFSET: Double;
  FLWOBS: Double;
  TimeIndex: Integer;
  StartTime: Double;
  EndTime: Double;
  TimeString: string;
  Comment: string;
  EarlyTimes: string;
  LateTimes: string;
  StartingTimes: TRealList;
  ReferenceStressPeriodIndex: Integer;
begin
  StartingTimes := TRealList.Create;
  try
    for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
    begin
      StartingTimes.Add(Model.ModflowFullStressPeriods[TimeIndex].StartTime);
    end;
    StartingTimes.Sort;
    EarlyTimes := '';
    LateTimes := '';
    StartTime := Model.ModflowStressPeriods[0].StartTime;
    EndTime := Model.ModflowFullStressPeriods[
      Model.ModflowFullStressPeriods.Count-1].EndTime;
    for TimeIndex := 0 to ObservationGroup.ObservationTimes.Count - 1 do
    begin
      ObsTime := ObservationGroup.ObservationTimes[TimeIndex];

      if ObsTime.Time < StartTime then
      begin
        EarlyTimes := EarlyTimes + ' ' + FloatToStr(ObsTime.Time);
        Continue;
      end;

      if ObsTime.Time > EndTime then
      begin
        LateTimes := LateTimes + ' ' + FloatToStr(ObsTime.Time);
        Continue;
      end;

      TimeString := ObsTime.ObsNameTimeString;
      OBSNAM := ObservationGroup.ObservationName + '_' + TimeString;
      if Model.PestStatus in [psObservations, psActive] then
      begin
        if not PestObsNameOK(OBSNAM) then
        begin
          frmErrorsAndWarnings.AddWarning(Model, ObsNameWarningString, OBSNAM);
        end;
      end
      else
      begin
        if not UcodeObsNameOK(OBSNAM) then
        begin
          frmErrorsAndWarnings.AddWarning(Model, ObsNameWarningString, OBSNAM);
        end;
      end;
      ReferenceStressPeriodIndex := StartingTimes.IndexOfClosest(ObsTime.Time);
      if (StartingTimes[ReferenceStressPeriodIndex] > ObsTime.Time) then
      begin
        Dec(ReferenceStressPeriodIndex);
      end;
      Assert(ReferenceStressPeriodIndex >= 0);
      TOFFSET := ObsTime.Time - StartingTimes[ReferenceStressPeriodIndex];
      FLWOBS := ObsTime.ObservedValue;
      if ObsTime.Comment = '' then
      begin
        Comment := '';
      end
      else
      begin
        Comment :=  ' Comment = ' + ObsTime.Comment;
      end;
      DataSet4.Add(OBSNAM + ' ' + IntToStr(ReferenceStressPeriodIndex+1)
        +  ' ' + FreeFormattedReal(TOFFSET)
        + FreeFormattedReal(FLWOBS)
        + ' # Data Set 4: OBSNAM IREFSP TOFFSET FLWOBS'
        + Comment);
      if Model.PestUsed then
      begin
        FPestInstructionFile.Add(Format('l1 !%s!', [OBSNAM]));
      end;
    end;
    if EarlyTimes <> '' then
    begin
      EarlyTimes := Format(StrErrorFlowObservatEarly,
        [ObservationGroup.ObservationName, EarlyTimes]);
      frmErrorsAndWarnings.AddWarning(Model,
        Format(EarlyTimeWarning, [GetFluxType(ObservationGroup)]), EarlyTimes);
    end;
    if LateTimes <> '' then
    begin
      LateTimes := Format(StrErrorFlowObservatLate,
        [ObservationGroup.ObservationName, LateTimes]);
      frmErrorsAndWarnings.AddWarning(Model,
        Format(LateTimeWarning, [GetFluxType(ObservationGroup)]), LateTimes);
    end;
  finally
    StartingTimes.Free;
  end;
end;

procedure TFluxObsWriter.WriteObservationDataSet5(DataSet5: TStringList;
      ObservationGroup: TFluxObservationGroup; AllCells: TList);
var
  ObsFactor: TObservationFactor;
  ObjectIndex: Integer;
begin
  for ObjectIndex := 0 to ObservationGroup.ObservationFactors.Count - 1 do
  begin
    ObsFactor := ObservationGroup.ObservationFactors[ObjectIndex];
    WriteDataSet5ForOneScreenObject(ObsFactor, DataSet5, AllCells);
  end;
end;

procedure TFluxObsWriter.WriteFluxObsFile(const AFileName, OutputUnitId,
      PackageAbbreviation, DataSet1Comment, DataSet2Comment,
      DataSet3Comment: string; Observations: TFluxObservationGroups;
      Purpose: TObservationPurpose);
var
  Index: Integer;
  NQCL_Pkg: Integer;
  DataSet5: TStringList;
  NQOB_Pkg: Integer;
  DataSet4: TStringList;
  ObservationGroup: TFluxObservationGroup;
  ObsIndex: Integer;
  CellList: TValueCellList;
  LocalParamValues: TList;
  ParamIndex: Integer;
  ACell: TValueCell;
  CellIndex: Integer;
  List: TValueCellList;
  ObsFile: TStringList;
  AllCells: TList;
  OutputName: string;
  IU_Pkg_OBSV: Integer;
  NameOfFile: string;
  NQT_Pkg: Integer;
  NQC_Pkg: Integer;
  NQ_Pkg: Integer;
  ErrorRoot: string;
  DetailedMessage: string;
  FluxObsCountWarning: string;
  DataSet1: string;
  PrintObservations: Boolean;
  MissingObservationObjectsErrorMessage: string;
begin
  // if the package is not selected, quit.
  if not ObservationPackage.IsSelected then
  begin
    Exit;
  end;
  // If the file has been generated externally, quit.
  if Model.PackageGeneratedExternally(PackageAbbreviation) then
  begin
    Exit;
  end;

  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveWarningGroup(Model, ObsNameWarningString);

    FluxObsCountWarning := Format(StrInSNoFlowObser,
      [ObservationPackage.PackageIdentifier]);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, FluxObsCountWarning);
    MissingObservationObjectsErrorMessage :=
      Format(StrMissingObservationObjects, [PackageAbbreviation]);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, MissingObservationObjectsErrorMessage);

    // count the number of cell groups for which flux observations are listed
    NQ_Pkg := 0;
    for ObsIndex := 0 to Observations.Count - 1 do
    begin
      ObservationGroup := Observations[ObsIndex];
      if Purpose = ObservationGroup.Purpose then
      begin
        Inc(NQ_Pkg);
      end;
    end;

    if (Observations.Count > 0) and (NQ_Pkg = 0) then
    begin
      frmErrorsAndWarnings.AddWarning(Model, FluxObsCountWarning,
        StrSelectModelObserv);
    end;

    NQC_Pkg := 0;
    NQT_Pkg := 0;

    NameOfFile := ObservationFileName(AFileName);
    WriteToNameFile(PackageAbbreviation, Model.UnitNumbers.
      UnitNumber(PackageAbbreviation), NameOfFile, foInput, Model);

    IU_Pkg_OBSV := Model.UnitNumbers.UnitNumber(OutputUnitId);
    OutputName := ObservationOutputFileName(AFileName);
    WriteToNameFile(StrDATA, IU_Pkg_OBSV, OutputName, foOutput, Model);
    if Model.PestUsed then
    begin
      Model.FileNameLines.Add(Format('  %0:s %1:s',
        [PackageAbbreviation, ExtractFileName(OutputName)]));
    end;

    AllCells := TList.Create;
    ObsFile := TStringList.Create;
    try
      // Values stores the values at a list of cells.
      if Values.Count = 0 then
      begin
        ErrorRoot := Format(StrTheSInputFileCa, [PackageAbbreviation]);
        DetailedMessage := Format(StrInTheBoundaryPack, [PackageAbbreviation]);
        frmErrorsAndWarnings.AddError(Model, ErrorRoot, DetailedMessage);
        Exit;
      end;
      // put the cells for the flux in AllCells
      List := Values[0];
      for CellIndex := 0 to List.Count - 1 do
      begin
        ACell := List[CellIndex];
        AllCells.Add(ACell);
      end;
      for ParamIndex := 0 to ParamValues.Count - 1 do
      begin
        LocalParamValues := ParamValues.Objects[ParamIndex] as TList;
        CellList := LocalParamValues[0];
        for CellIndex := 0 to CellList.Count - 1 do
        begin
          ACell := CellList[CellIndex];
          AllCells.Add(ACell);
        end;
      end;
      for ObsIndex := 0 to Observations.Count - 1 do
      begin
        ObservationGroup := Observations[ObsIndex];
        if ObsIndex = 0 then
        begin
          RemoveWarningGroups(ObservationGroup);
        end;
        if Purpose = ObservationGroup.Purpose then
        begin
          if ObservationGroup.ObservationFactors.Count = 0 then
          begin
            frmErrorsAndWarnings.AddError(Model,
              MissingObservationObjectsErrorMessage,
              ObservationGroup.ObservationName);
          end;
          DataSet4 := TStringList.Create;
          try
            WriteObservationDataSet4(ObservationGroup, DataSet4);
            NQOB_Pkg := DataSet4.Count;
            NQT_Pkg := NQT_Pkg + NQOB_Pkg;
            DataSet5 := TStringList.Create;
            try
              WriteObservationDataSet5(DataSet5, ObservationGroup, AllCells);
              NQCL_Pkg := DataSet5.Count;
              NQC_Pkg := NQC_Pkg + NQCL_Pkg;
              ObsFile.Add(IntToStr(NQOB_Pkg) + ' ' + IntToStr(NQCL_Pkg)
                + DataSet3Comment);
              ObsFile.AddStrings(DataSet4);
              ObsFile.AddStrings(DataSet5);
            finally
              DataSet5.Free;
            end;
          finally
            DataSet4.Free;
          end;
        end;
      end;
      DataSet1 := IntToStr(NQ_Pkg) + ' ' + IntToStr(NQC_Pkg) + ' '
        + IntToStr(NQT_Pkg) + ' ' + IntToStr(IU_Pkg_OBSV);

      PrintObservations := Model.ModflowOutputControl.PrintObservations;
      if not PrintObservations then
      begin
        DataSet1 := DataSet1 +' NOPRINT';
      end;
      DataSet1 := DataSet1 + DataSet1Comment;
      if not PrintObservations then
      begin
        DataSet1 := DataSet1 +' NOPRINT';
      end;
      ObsFile.Insert(0,DataSet1);
      ObsFile.Insert(1, '1' + DataSet2Comment);
      for Index := ObservationPackage.Comments.Count - 1 downto 0 do
      begin
        ObsFile.Insert(0, '# ' + ObservationPackage.Comments[Index]);
      end;
      ObsFile.Insert(0, '# ' + PackageID_Comment(ObservationPackage));
      ObsFile.WriteBOM := False;
      ObsFile.SaveToFile(NameOfFile);
      SavePestInstructionFile(OutputName);
    finally
      AllCells.Free;
      ObsFile.Free;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;


procedure TFluxObsWriter.WriteZeroConductanceCell(ACell: TValueCell;
  DataSet5: TStringList);
var
  Layer: Integer;
  Row: Integer;
  Column: Integer;
begin
  // write zero-conductance cell.
  Layer := Model.
    DataSetLayerToModflowLayer(ACell.Layer);
  Row := ACell.Row + 1;
  Column := ACell.Column + 1;
  DataSet5.Add(IntToStr(Layer) + ' ' + IntToStr(Row) + ' ' + IntToStr(Column)
    + ' 0' + ' # Data Set 5: Layer Row Column Factor');
end;

procedure TFluxObsWriter.WriteObservationCell(ACell: TValueCell;
  DataSet5: TStringList; var Expression: TExpression; DataSets, Variables: TList;
  ObsFactor: TObservationFactor);
var
  Layer: Integer;
  Row: Integer;
  Column: Integer;
  Factor: Extended;
begin
  // Write cell
  Layer := Model.DataSetLayerToModflowLayer(ACell.Layer);
  Row := ACell.Row + 1;
  Column := ACell.Column + 1;
  EvaluateFactor(Factor, Expression, ACell,
    DataSets, Variables, ObsFactor);
  DataSet5.Add(IntToStr(Layer) + ' ' + IntToStr(Row) + ' '
    + IntToStr(Column) + ' ' + FreeFormattedReal(Factor)
    + ' # Data Set 5: Layer Row Column Factor');
end;

procedure TFluxObsWriter.WriteObservationCells(Variables, DataSets: TList;
  var Expression: TExpression; DataSet5: TStringList; AllCells: TList;
  ScreenObject: TScreenObject; ObsFactor: TObservationFactor);
var
  AnotherCell: TValueCell;
  StartPoint: Integer;
  ACell: TValueCell;
  CellIndex: Integer;
  TempList: TList;
begin
  TempList := TList.Create;
  try
    for CellIndex := 0 to AllCells.Count - 1 do
    begin
      ACell := AllCells[CellIndex];
      if ACell.ScreenObject = ScreenObject as IScreenObject then
      begin
        TempList.Add(ACell);
      end;
    end;
    // Simulate the procedure MODFLOW uses to identify the cells to use as
    // part of the observation.
    StartPoint := 0;
    for CellIndex := 0 to TempList.Count - 1 do
    begin
      ACell := TempList[CellIndex];
      while True do
      begin
        AnotherCell := AllCells[StartPoint];
        Inc(StartPoint);
        if StartPoint = AllCells.Count then
        begin
          StartPoint := 0;
        end;
        if ACell = AnotherCell then
        begin
          WriteObservationCell(ACell, DataSet5, Expression,
            DataSets, Variables, ObsFactor);
          break;
        end
        else if (ACell.Layer = AnotherCell.Layer)
          and (ACell.Row = AnotherCell.Row)
          and (ACell.Column = AnotherCell.Column) then
        begin
          WriteZeroConductanceCell(ACell, DataSet5);
        end;
      end;
    end;
  finally
    TempList.Free;
  end;
end;

procedure TFluxObsWriter.WriteDataSet5ForOneScreenObject(
  ObsFactor: TObservationFactor; DataSet5: TStringList; AllCells: TList);
var
  Expression: TExpression;
  Observer: TObserver;
  VariableIndex: Integer;
  Variables: TList;
  FactorFormula: string;
  ScreenObject: TScreenObject;
  DataSets: TList;
  VariablesUsed: TStringList;
  DataArray: TDataArray;
begin
  FactorFormula := ObsFactor.Factor;
  Model.rpThreeDFormulaCompiler.Compile(FactorFormula);
  Expression := Model.rpThreeDFormulaCompiler.CurrentExpression;
  Assert(Expression.ResultType in [rdtDouble, rdtInteger]);
  DataSets := TList.Create;
  Variables := TList.Create;
  try
    VariablesUsed := Expression.VariablesUsed;
    Variables.Capacity := VariablesUsed.Count;
    DataSets.Capacity := VariablesUsed.Count;
    for VariableIndex := 0 to VariablesUsed.Count - 1 do
    begin
      Observer := Model.GetObserverByName(VariablesUsed[VariableIndex]);
      if Observer is TDataArray then
      begin
        DataArray := TDataArray(Observer);
        DataArray.Initialize;
        Variables.Add(VariablesUsed.Objects[VariableIndex]);
        DataSets.Add(DataArray);
        Model.DataArrayManager.AddDataSetToCache(DataArray);
      end
      else
      begin
        Assert(Observer is TGlobalVariable);
      end;
    end;
    ScreenObject := ObsFactor.ScreenObject as TScreenObject;
    WriteObservationCells(Variables, DataSets, Expression, DataSet5, AllCells, ScreenObject, ObsFactor);
  finally
    Variables.Free;
    DataSets.Free;
    Model.DataArrayManager.CacheDataArrays;
  end;
end;

constructor TFluxObsWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
var
  TemplateCharacter: Char;
begin
  inherited;
  TemplateCharacter := Model.PestProperties.TemplateCharacter;
  FPestInstructionFile:= TStringList.Create;
  FPestInstructionFile.Add('pif ' + TemplateCharacter);
end;

destructor TFluxObsWriter.Destroy;
begin
  FPestInstructionFile.Free;
  inherited;
end;

procedure TFluxObsWriter.EvaluateFactor(var Factor: Extended;
  var Expression: TExpression; ACell: TValueCell; DataSets, Variables: TList;
  ObsFactor: TObservationFactor);
var
  DA_Column: Integer;
  DA_Row: Integer;
  DA_Layer: Integer;
  Compiler: TRbwParser;
  ScreenObject: TScreenObject;
  RealVariable: TRealVariable;
  StringVariable: TStringVariable;
  TempFormula: string;
  IntegerVariable: TIntegerVariable;
  Local_VariableIndex: Integer;
  DataArray: TDataArray;
  BooleanVariable: TBooleanVariable;
begin
//  Layer := Model.DataSetLayerToModflowLayer(ACell.Layer);
//  Row := ACell.Row + 1;
//  Column := ACell.Column + 1;
  for Local_VariableIndex := 0 to Variables.Count - 1 do
  begin
    DataArray := DataSets[Local_VariableIndex];
    DA_Layer := ACell.Layer;
    DA_Row := ACell.Row;
    DA_Column := ACell.Column;
    case DataArray.Orientation of
      dsoTop:
        DA_Layer := 0;
      dsoFront:
        DA_Row := 0;
      dsoSide:
        DA_Column := 0;
      dso3D:
        ;
    else
      // do nothing
      Assert(False);
    end;
    //    DataArray.Initialize;
    case DataArray.DataType of
      rdtDouble:
        begin
          RealVariable := Variables[Local_VariableIndex];
          RealVariable.Value := DataArray.RealData[DA_Layer, DA_Row, DA_Column];
        end;
      rdtInteger:
        begin
          IntegerVariable := Variables[Local_VariableIndex];
          IntegerVariable.Value := DataArray.IntegerData[
            DA_Layer, DA_Row, DA_Column];
        end;
      rdtBoolean:
        begin
          BooleanVariable := Variables[Local_VariableIndex];
          BooleanVariable.Value := DataArray.BooleanData[
            DA_Layer, DA_Row, DA_Column];
        end;
      rdtString:
        begin
          StringVariable := Variables[Local_VariableIndex];
          StringVariable.Value := DataArray.StringData[
            DA_Layer, DA_Row, DA_Column];
        end;
    else
      Assert(False);
    end;
  end;
  UpdateGlobalLocations(ACell.Column, ACell.Row, ACell.Layer, eaBlocks, Model);
  UpdateCurrentScreenObject(ObsFactor.ScreenObject as TScreenObject);
  { TODO : Find a way to specify segment }
//  UpdateCurrentSegment(ACell.Segment);
  UpdateCurrentSection(ACell.Section);
  try
    Expression.Evaluate;
  except
    on E: ERbwParserError do
    begin
      ScreenObject := ObsFactor.ScreenObject as TScreenObject;
      frmFormulaErrors.AddFormulaError(ScreenObject.Name,
        Format(StrObservationFactor, [ObservationPackage.PackageIdentifier]),
        Expression.Decompile, E.Message);
      ObsFactor.Factor := '1.';
      Compiler := Model.rpThreeDFormulaCompiler;
      TempFormula := ObsFactor.Factor;
      Compiler.Compile(TempFormula);
      Expression := Compiler.CurrentExpression;
      Expression.Evaluate;
    end;
  end;
  Factor := Expression.DoubleResult;
  if Factor > 1 then
  begin
    Factor := 1;
  end
  else if Factor < 0 then
  begin
    Factor := 0;
  end;
end;

procedure TCustomPackageWriter.IdentifyZoneClusters(var NCLU: Integer;
      var Clusters: TOneDIntegerArray; var UniformLayers: TBooleanDynArray;
      LayerCount: Integer; Param: TModflowSteadyParameter);
var
  MF_LayerIndex: Integer;
//  Group: TLayerGroup;
  LayerUsed: Boolean;
  LayerIndex: Integer;
  ModelLayerIndex: Integer;
  ZoneDataSet: TDataArray;
  ColIndex: Integer;
  RowIndex: Integer;
  FirstValue: Boolean;
begin
  SetLength(Clusters, LayerCount);
  SetLength(UniformLayers, LayerCount);
  ZoneDataSet := Model.DataArrayManager.GetDataSetByName(Param.ZoneName);
  ZoneDataSet.Initialize;
  NCLU := 0;
  if Param.ParameterType = ptLPF_VKCB then
  begin
    MF_LayerIndex := 0;
    for ModelLayerIndex := 0 to Model.Grid.LayerCount - 1 do
    begin
//      Group := Model.LayerStructure.LayerGroups[ModelLayerIndex];
      if Model.IsLayerSimulated(ModelLayerIndex) then
      begin
//        MF_LayerIndex := MF_LayerIndex + Group.ModflowLayerCount;
        Inc(MF_LayerIndex);
      end
      else
      begin
        LayerUsed := False;
        LayerIndex := ModelLayerIndex;
//        Inc(LayerIndex);
        FirstValue := ZoneDataSet.BooleanData[LayerIndex, 0, 0];
        UniformLayers[NCLU] := True;
        for RowIndex := 0 to Model.ModflowGrid.RowCount - 1 do
        begin
          for ColIndex := 0 to Model.ModflowGrid.ColumnCount - 1 do
          begin
            if ZoneDataSet.BooleanData[LayerIndex, RowIndex, ColIndex] then
            begin
              if not LayerUsed then
              begin
                Clusters[NCLU] := MF_LayerIndex;
                Inc(NCLU);
                LayerUsed := True;
              end;
            end;
            if ZoneDataSet.BooleanData[LayerIndex, RowIndex, ColIndex] <>
              FirstValue then
            begin
              UniformLayers[NCLU-1] := False;
              break;
            end;
          end;
          if LayerUsed and not UniformLayers[NCLU-1] then
          begin
            break;
          end;
        end;
      end;
    end;
  end
  else
  begin
    for MF_LayerIndex := 1 to LayerCount do
    begin
      LayerUsed := False;
      LayerIndex := Model.
        ModflowLayerToDataSetLayer(MF_LayerIndex);
      FirstValue := ZoneDataSet.BooleanData[LayerIndex, 0, 0];
      UniformLayers[NCLU] := True;
      for RowIndex := 0 to Model.ModflowGrid.RowCount - 1 do
      begin
        for ColIndex := 0 to Model.ModflowGrid.ColumnCount - 1 do
        begin
          if ZoneDataSet.BooleanData[LayerIndex, RowIndex, ColIndex] then
          begin
            if not LayerUsed then
            begin
              Clusters[NCLU] := MF_LayerIndex;
              Inc(NCLU);
              LayerUsed := True;
            end;
          end;
          if ZoneDataSet.BooleanData[LayerIndex, RowIndex, ColIndex] <>
            FirstValue then
          begin
            UniformLayers[NCLU-1] := False;
            break;
          end;
        end;
        if LayerUsed and not UniformLayers[NCLU-1] then
        begin
          break;
        end;
      end;
    end;
  end;
  Model.DataArrayManager.AddDataSetToCache(ZoneDataSet);
end;

{ TCustomSolverWriter }

class function TCustomSolverWriter.SolverFileGeneratedExternally(Model: TCustomModel): boolean;
var
  Index: Integer;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoAlternativeSolve);
  if Model.AlternateSolver then
  begin
    result := True;
    if Model.ModflowNameFileLines.Count = 0 then
    begin
      frmErrorsAndWarnings.AddError(Model, StrNoAlternativeSolve,
        StrInTheMODFLOWName)
    end;
  end
  else
  begin
    for Index := 0 to Length(Solvers) - 1 do
    begin
      result := Model.PackageGeneratedExternally(Solvers[Index]);
      if result then
      begin
        Exit;
      end;
    end;
  end;
end;

{ TCustomFlowPackageWriter }

procedure TCustomFlowPackageWriter.CheckSpecifiedHeadsConnected;
var
  SpecifiedHead: TDataArray;
  LayerIndex: Integer;
  XdataSet: TDataArray;
  YdataSet: TDataArray;
  ZdataSet: TDataArray;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidSpecifiedHe);
  SpecifiedHead := Model.DataArrayManager.GetDataSetByName(rsModflowSpecifiedHead);
  for LayerIndex := 0 to Model.LayerCount - 1 do
  begin
    if Model.IsLayerSimulated(LayerIndex) then
    begin
      XdataSet := XConnection(LayerIndex);
      YdataSet := YConnection(LayerIndex);
      ZdataSet := ZConnection(LayerIndex);

      for RowIndex := 0 to Model.RowCount - 1 do
      begin
        for ColIndex := 0 to Model.ColumnCount - 1 do
        begin
          if SpecifiedHead.BooleanData[LayerIndex, RowIndex, ColIndex] then
          begin
            if (XdataSet.RealData[LayerIndex, RowIndex, ColIndex] = 0)
              and (YdataSet.RealData[LayerIndex, RowIndex, ColIndex] = 0)
              and ((ZdataSet = nil) or (ZdataSet.RealData[LayerIndex, RowIndex, ColIndex] = 0))
              then
            begin
              if (ZdataSet = nil) then
              begin
                frmErrorsAndWarnings.AddError(Model, StrInvalidSpecifiedHe,
                  Format('The specified head cell at (Layer,Row,Col) = (%0:d,%1:d,%2:d) is invalid because the %3:s and %4:s data sets both have values of zero at that cell.',
                  [LayerIndex+1, RowIndex+1, ColIndex+1,
                  XdataSet.Name, YdataSet.Name]),nil);
              end
              else
              begin
                frmErrorsAndWarnings.AddError(Model, StrInvalidSpecifiedHe,
                  Format(StrTheSpecifiedHeadC,
                  [LayerIndex+1, RowIndex+1, ColIndex+1,
                  XdataSet.Name, YdataSet.Name, ZdataSet.Name]),nil);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

class function TCustomFlowPackageWriter.FlowPackageFileGeneratedExternally(
  Model: TCustomModel): boolean;
var
  Index: Integer;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoAlternativeFlow);
  if Model.AlternateFlowPackage then
  begin
    result := True;
    if Model.ModflowNameFileLines.Count = 0 then
    begin
      frmErrorsAndWarnings.AddError(Model, StrNoAlternativeFlow,
        StrInTheMODFLOWNameFlow)
    end;
  end
  else
  begin
    for Index := 0 to Length(FlowPackages) - 1 do
    begin
      result := Model.PackageGeneratedExternally(FlowPackages[Index]);
      if result then
      begin
        Exit;
      end;
    end;
  end;
end;

function TCustomFlowPackageWriter.XConnection(LayerIndex: Integer): TDataArray;
begin
  result := Model.DataArrayManager.GetDataSetByName(rsKx);
end;

function TCustomFlowPackageWriter.YConnection(LayerIndex: Integer): TDataArray;
begin
  result := Model.DataArrayManager.GetDataSetByName(rsKy);
end;

function TCustomFlowPackageWriter.ZConnection(LayerIndex: Integer): TDataArray;
begin
  if Model.LayerCount > 1 then
  begin
    result := Model.DataArrayManager.GetDataSetByName(rsKz);
  end
  else
  begin
    result := nil;
  end;
end;

procedure TCustomSubWriter.GetStartAndEndTimeSteps(var ITS2, ISP2, ITS1, ISP1: Integer;
      PrintChoice: TCustomPrintItem);
var
  TimeStepIndex: Integer;
  LengthOfTimeStep: Double;
  Time: Double;
  StressPeriodIndex: Integer;
  StressPeriod: TModflowStressPeriod;
begin
  ISP1 := 0;
  ITS1 := 0;
  ISP2 := 0;
  ITS2 := 0;
  StressPeriod := Model.ModflowFullStressPeriods[0];
  if StartTimeOK(StressPeriod.StartTime, PrintChoice) then
  begin
    ISP1 := 1;
    ITS1 := 1;
  end
  else
  begin
    for StressPeriodIndex := 0 to
      Model.ModflowFullStressPeriods.Count - 1 do
    begin
      StressPeriod := Model.ModflowFullStressPeriods[StressPeriodIndex];
      if StressPeriod.EndTime > PrintChoice.StartTime then
      begin
        ISP1 := StressPeriodIndex + 1;
        if StartTimeOK(StressPeriod.StartTime, PrintChoice) then
        begin
          ITS1 := 1;
        end
        else
        begin
          Time := StressPeriod.StartTime;
          LengthOfTimeStep := StressPeriod.LengthOfFirstTimeStep;
          for TimeStepIndex := 0 to StressPeriod.NumberOfSteps - 1 do
          begin
            Time := Time + LengthOfTimeStep;
            if StartTimeOK(Time, PrintChoice) then
            begin
              ITS1 := TimeStepIndex;
              if ITS1 = 0 then
              begin
                ITS1 := 1;
              end;
              break;
            end;
            LengthOfTimeStep :=
              LengthOfTimeStep * StressPeriod.TimeStepMultiplier;
          end;
          if ITS1 = 0 then
          begin
            ITS1 := StressPeriod.NumberOfSteps;
          end;
        end;
        break;
      end;
    end;
  end;
  StressPeriod := Model.ModflowFullStressPeriods
    [Model.ModflowFullStressPeriods.Count - 1];
  if PrintChoice.EndTime >= StressPeriod.EndTime then
  begin
    ISP2 := Model.ModflowFullStressPeriods.Count;
    ITS2 := StressPeriod.NumberOfSteps;
  end
  else
  begin
    for StressPeriodIndex := ISP1 - 1 to
      Model.ModflowFullStressPeriods.Count - 1 do
    begin
      StressPeriod := Model.ModflowFullStressPeriods[StressPeriodIndex];
      if EndTimeOK(StressPeriod.EndTime, PrintChoice) then
      begin
        ISP2 := StressPeriodIndex + 1;
        if StressPeriod.EndTime = PrintChoice.EndTime then
        begin
          ITS2 := StressPeriod.NumberOfSteps;
        end
        else
        begin
          Time := StressPeriod.StartTime;
          LengthOfTimeStep := StressPeriod.LengthOfFirstTimeStep;
          for TimeStepIndex := 0 to StressPeriod.NumberOfSteps - 1 do
          begin
            Time := Time + LengthOfTimeStep;
            if EndTimeOK(Time, PrintChoice) then
            begin
              ITS2 := TimeStepIndex + 1;
              break;
            end;
            LengthOfTimeStep :=
              LengthOfTimeStep * StressPeriod.TimeStepMultiplier;
          end;
          if ITS2 = 0 then
          begin
            ITS2 := StressPeriod.NumberOfSteps;
          end;
        end;
        break;
      end;
    end;
  end;
end;

const
  Epsilon = 1e-6;

function TCustomSubWriter.StartTimeOK(Time: double; PrintChoice: TCustomPrintItem): boolean;
begin
  result := PrintChoice.StartTime + Abs(PrintChoice.StartTime)*Epsilon <= Time
end;

function TCustomSubWriter.EndTimeOK(Time: double; PrintChoice: TCustomPrintItem): boolean;
begin
  result := Time >= PrintChoice.EndTime - Abs(PrintChoice.EndTime)*Epsilon;
end;

{ TTransientArrayWriter }

class function TTransientArrayWriter.Extension: string;
begin
  result := '';
end;

function TTransientArrayWriter.WriteFile(const AFileName,
  ArrayType: string; ADataArray: TDataArray): string;
var
  ArrayName: string;
  OutputFileName: string;
  OutputDirectory: string;
begin
  ArrayName := ADataArray.Name;
  OutputFileName := AFileName + '.' + Trim(ArrayName);

  result := ExtractFileName(OutputFileName);
  OutputDirectory := ExtractFileDir(OutputFileName);
  OutputDirectory := IncludeTrailingPathDelimiter(OutputDirectory) + StrArrays;
  if not DirectoryExists(OutputDirectory) then
  begin
    CreateDir(OutputDirectory);
  end;
  result := IncludeTrailingPathDelimiter(OutputDirectory) + result;
  FInputFileName := result;

  OpenFile(result);
  try
    WriteArrayValues(0, ADataArray);
  finally
    CloseFile;
  end;
  ADataArray.CacheData;
end;

{ TCustomNameFileWriter }

constructor TCustomNameFileWriter.Create(AModel: TCustomModel;
  const FileName: string; EvaluationType: TEvaluationType);
begin
  inherited Create(AModel, EvaluationType);
  FFullOutputFiles := TStringList.Create;
  FFullInputFiles := TStringList.Create;
  FNameFile := TStringList.Create;
  FInputFiles := TStringList.Create;
  FOutputFiles := TStringList.Create;

  SetCurrentNameFileWriter(self);
  InitilizeNameFile(FileName, FListFileName);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInputFileDoesNot);
end;

destructor TCustomNameFileWriter.Destroy;
begin
  FFullOutputFiles.Free;
  FFullInputFiles.Free;
  FInputFiles.Free;
  FOutputFiles.Free;
  FNameFile.Free;
  inherited;
end;

function TCustomNameFileWriter.GetModelName(AFileName: string): string;
begin
  result := ExtractFileName(ChangeFileExt(AFileName, ''));
end;

procedure TCustomNameFileWriter.HandleMF6Files;
begin
  // do nothing
end;

function TCustomNameFileWriter.ArchiveExtension: string;
begin
  result := '';
end;

procedure TCustomNameFileWriter.ClearNameFile;
begin
  NameFile.Clear;
  OutputFiles.Clear;
  InputFiles.Clear;
end;

{ TMt3dmsNameWriter }

function TMt3dmsNameWriter.ArchiveExtension: string;
begin
  if Model.ModflowPackages.Mt3dBasic.Mt3dVersion = mvUSGS then
  begin
    Result := '_MT3D-USGS';
  end
  else
  begin
    Result := '_MT3DMS';
  end;
end;

class function TMt3dmsNameWriter.Extension: string;
begin
  result := StrMtName;
end;

//function TMt3dmsNameWriter.GetModelName(AFileName: string): string;
//begin
//  Result := inherited GetModelName(AFileName) + '_MT3DMS';
//end;

procedure TMt3dmsNameWriter.InitilizeNameFile(const FileName: string;
  out OutputListFileName: string);
var
  FtlFileName: string;
  CbcFileName: string;
  BhdFileName: string;
  DisGrbFileName: string;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrMissingFtlFile);
  ClearNameFile;
  AddNameFileComment(Format(StrNameFileForMT3DMS,
    [DateToStr(Now), Model.ProgramName]));
  OutputListFileName := ChangeFileExt(FileName, '.mls');
  WriteToMt3dMsNameFile(StrLIST, Mt3dList, OutputListFileName, foOutput, Model);
  FtlFileName := ChangeFileExt(OutputListFileName, '.ftl');
  if not FileExists(FtlFileName) and (Model.ModelSelection <> msModflow2015) then
  begin
    frmErrorsAndWarnings.AddError(Model, StrMissingFtlFile,
      Format(StrTheFlowtransportL, [FtlFileName]));
  end;
  if (muoTextFtl in Model.ModflowPackages.Mt3dBasic.Mt3dUsgsOptions)
    and (Model.ModflowPackages.Mt3dBasic.Mt3dVersion = mvUSGS) then
  begin
    if Model.ModelSelection = msModflow2015 then
    begin
      CbcFileName := ChangeFileExt(OutputListFileName, StrCbcExt);
      WriteToMt3dMsNameFile(StrFt6CBC, Mt3dFL6Cbc, CbcFileName, foInput, Model,
        False, 'FREE');
      BhdFileName := ChangeFileExt(OutputListFileName, StrBhd);
      WriteToMt3dMsNameFile(StrFt6BHD, Mt3dFl6Bhd, BhdFileName, foInput, Model,
        False, 'FREE');
      DisGrbFileName := ChangeFileExt(OutputListFileName, StrDisgrb);
      WriteToMt3dMsNameFile(StrFt6DisGrb, Mt3dDisGrb, DisGrbFileName, foInput,
        Model, False, 'FREE');
    end
    else
    begin
      WriteToMt3dMsNameFile(StrFTL, Mt3dFtl, FtlFileName, foInput, Model,
        False, 'FREE');
    end;
  end
  else
  begin
    if Model.ModelSelection = msModflow2015 then
    begin
      CbcFileName := ChangeFileExt(OutputListFileName, StrCbcExt);
      WriteToMt3dMsNameFile(StrFt6CBC, Mt3dFL6Cbc, CbcFileName, foInput, Model);
      BhdFileName := ChangeFileExt(OutputListFileName, StrBhd);
      WriteToMt3dMsNameFile(StrFt6BHD, Mt3dFl6Bhd, BhdFileName, foInput, Model);
      DisGrbFileName := ChangeFileExt(OutputListFileName, StrDisgrb);
      WriteToMt3dMsNameFile(StrFt6DisGrb, Mt3dDisGrb, DisGrbFileName, foInput,
        Model);
    end
    else
    begin
      WriteToMt3dMsNameFile(StrFTL, Mt3dFtl, FtlFileName, foInput, Model);
    end;
  end;
  WriteToMt3dMsNameFile(StrDATA, Mt3dCnf, ChangeFileExt(OutputListFileName, '.cnf'), foOutput, Model);
end;

procedure TMt3dmsNameWriter.SaveNameFile(AFileName: string);
var
  ModelName: string;
  NameFileIndex: Integer;
  ALine: string;
  FtlFileName: string;
begin
  inherited;
  ModelName := ExtractFileName(ChangeFileExt((Model as TCustomModel).
    ParentModel.ModelFileName, ''));
  FtlFileName := ModelName + '.ftl';
  for NameFileIndex := 0 to NameFile.Count - 1 do
  begin
    ALine := NameFile[NameFileIndex];
    if Pos(FtlFileName, ALine) > 0 then
    begin
      ALine := StringReplace(ALine, FtlFileName,
        '..\..\output\output.'+ ModelName + '\' + FtlFileName, []);
      NameFile[NameFileIndex] := ALine;
    end;
  end;


  AFileName := FileName(AFileName) + ArchiveExt;
  NameFile.WriteBOM := False;
  NameFile.SaveToFile(AFileName);
  Model.AddMt3dmsInputFile(AFileName);
end;

procedure TCustomNameFileWriter.SaveNameFile(AFileName: string);
var
  NameFileIndex: Integer;
  OutputFileIndex: Integer;
  OutputFile: string;
  ModelName: string;
  ALine: string;
  InputFileIndex: Integer;
  InputFile: string;
  Splitter: TStringList;
  LineIndex: Integer;
  ExternalFileName: string;
  Filetype: string;
  Input: boolean;
  ArchiveName: string;
  ExternalLine: Boolean;
  ItemIndex: Integer;
  NamePos: Integer;
  NextChar: string;
  AFile: string;
  IsRelativeFileName: Boolean;
  RelPath: string;
begin
  AFileName := FileName(AFileName);
  HandleMF6Files;
  NameFile.WriteBOM := False;
  NameFile.SaveToFile(AFileName);
//  Model.AddModelInputFile(AFileName);

  ModelName := GetModelName(AFileName);
  Splitter := TStringList.Create;
  try
    Splitter.Delimiter := ' ';
    ExternalLine := false;
    for LineIndex := 0 to NameFile.Count - 1 do
    begin
      ALine := NameFile[LineIndex];
      if Pos('#Files generated outside of', ALine) = 1 then
      begin
        ExternalLine := True;
        Continue;
      end;
      if ExternalLine then
      begin

        if Pos('#Files generated by', ALine) = 1 then
        begin
          break;
        end;
        if ALine = '' then
        begin
          Continue;
        end;
        if ALine[1] = '#' then
        begin
          Continue;
        end;
        Splitter.DelimitedText := ALine;
        if Splitter.Count >= 3 then
        begin
          IsRelativeFileName := AnsiSameText(Copy(Splitter[2], 1, ExtFileLength), ExtFileString);
          if not IsRelativeFileName then
          begin
            RelPath := ExtractRelativePath(AFileName, Splitter[2]);
            IsRelativeFileName := AnsiSameText(Copy(RelPath, 1, ExtFileLength), ExtFileString);
            if IsRelativeFileName then
            begin
              Splitter[2] := RelPath;
            end;
          end;
          ExternalFileName := ExpandFileName(Splitter[2]);
          ArchiveName := ExtractFileName(Splitter[2]);
          Filetype := UpperCase(Splitter[0]);
          Input := False;
          if (Filetype = StrDATA) or (Filetype = StrDATABINARY)  then
          begin
            if Splitter.Count >= 4 then
            begin
              Filetype := UpperCase(Splitter[3]);
              if Filetype = 'OLD' then
              begin
                Input := True;
              end;
            end;
          end
          else
          begin
            Input := True;
          end;
          if Input then
          begin
            if IsRelativeFileName then
            begin
              ArchiveName := '..\..\model\externalfiles\' + Copy(Splitter[2], ExtFileLength+1, MAXINT);
              Model.AddExternalFile(ExternalFileName);
            end
            else
            begin
              ArchiveName := '..\..\model\model.' + ModelName + ArchiveExtension + '\' + ArchiveName;
              Model.AddModelInputFile(ExternalFileName);
            end;
          end
          else
          begin
            Model.AddModelOutputFile(ExternalFileName);
            ArchiveName := '..\..\output\output.' + ModelName+ ArchiveExtension + '\' + ArchiveName;
          end;
          ALine := Splitter[0] + ' ' + Splitter[1] + ' ' + ArchiveName;
          for ItemIndex := 3 to Splitter.Count - 1 do
          begin
            ALine := ALine + ' ' + Splitter[ItemIndex];
          end;
          NameFile[LineIndex] := ALine;
        end;

      end;
    end;

    for OutputFileIndex := 0 to FFullOutputFiles.Count - 1 do
    begin
      OutputFile := ExtractFileName(FFullOutputFiles[OutputFileIndex]);

      for NameFileIndex := 0 to NameFile.Count - 1 do
      begin
        ALine := NameFile[NameFileIndex];
        if Copy(ALine, 1, 4) = 'FTL ' then
        begin
          Continue;
        end;
        NamePos := Pos(OutputFile, ALine);
        if (NamePos > 0) then
        begin
          NextChar := Copy(ALine, NamePos + Length(OutputFile), 1);
          if (NextChar = '') or (NextChar = ' ') then
          begin
            Splitter.DelimitedText := ALine;

            if Model.ModelSelection = msModflow2015 then
            begin
              if Splitter.Count < 2 then
              begin
                frmErrorsAndWarnings.AddError(Model, StrErrorHandlingTheF,
                  ALine);
                Continue;
              end;
              AFile := Splitter[1];
            end
            else
            begin
              if Splitter.Count < 3 then
              begin
                frmErrorsAndWarnings.AddError(Model, StrErrorHandlingTheF,
                  ALine);
                Continue;
              end;
              AFile := Splitter[2];
            end;

            if AFile = OutputFile then
            begin
              ALine := StringReplace(ALine, OutputFile,
                '..\..\output\output.'+ ModelName + ArchiveExtension + '\' + OutputFile, []);
            end
            else
            begin
              ArchiveName := '..\..\output\output.' + ModelName+ ArchiveExtension + '\' + OutputFile;
              ALine := Splitter[0] + ' ';
              while Length(ALine) + Length(Splitter[1])< 19 do
              begin
                ALine := ALine + ' ';
              end;
              ALine := ALine + Splitter[1] + ' ' + ArchiveName;
              for ItemIndex := 3 to Splitter.Count - 1 do
              begin
                ALine := ALine + ' ' + Splitter[ItemIndex];
              end;
            end;
            NameFile[NameFileIndex] := ALine;
          end;
        end;
      end;
    end;
    for InputFileIndex := 0 to FFullInputFiles.Count - 1 do
    begin
      InputFile := ExtractFileName(FFullInputFiles[InputFileIndex]);

      for NameFileIndex := 0 to NameFile.Count - 1 do
      begin
        ALine := NameFile[NameFileIndex];
        if Copy(ALine, 1, 4) = 'FTL ' then
        begin
          Continue;
        end;
        NamePos := Pos(InputFile, ALine);
        if NamePos > 0 then
        begin
          NextChar := Copy(ALine, NamePos + Length(InputFile), 1);
          if (NextChar = '') or (NextChar = ' ') then
          begin
            Splitter.DelimitedText := ALine;
            if Model.ModelSelection = msModflow2015 then
            begin
              AFile := Splitter[1];
            end
            else
            begin
              AFile := Splitter[2];
            end;
            if AFile = InputFile then
            begin
              ALine := StringReplace(ALine, InputFile,
                '..\..\model\model.'+ ModelName + ArchiveExtension + '\' + InputFile, []);
            end
            else
            begin
              ArchiveName := '..\..\model\model.' + ModelName+ ArchiveExtension + '\' + InputFile;
              ALine := Splitter[0] + ' ';
              while Length(ALine) + Length(Splitter[1])< 19 do
              begin
                ALine := ALine + ' ';
              end;
              ALine := ALine + Splitter[1] + ' ' + ArchiveName;
              for ItemIndex := 3 to Splitter.Count - 1 do
              begin
                ALine := ALine + ' ' + Splitter[ItemIndex];
              end;
            end;
            NameFile[NameFileIndex] := ALine;
          end;
        end;
      end;
    end;
  finally
    Splitter.Free;
  end;

end;

procedure TNameFileWriter.HandleMF6Files;
var
  ALine: string;
  Options: TModflowOptions;
  OC: TModflowOutputControl;
begin
  if FModel.ModelSelection = msModflow2015 then
  begin
    NameFile.Add('BEGIN OPTIONS');
    NameFile.Add('  LIST ' + ExtractFileName(FListFileName));

    Options := Model.ModflowOptions;
    if Options.NewtonMF6 then
    begin
      ALine := '  NEWTON';
      if Options.UnderRelaxationMF6 then
      begin
        ALine := ALine + ' UNDER_RELAXATION';
      end;
      NameFile.Add(ALine);
    end;

    OC := Model.ModflowOutputControl;
    if OC.PrintInputArrays or OC.PrintInputCellLists then
    begin
      ALine := '  PRINT_INPUT';
      NameFile.Add(ALine);
    end;

    case OC.SaveCellFlows of
      csfNone: ;
      csfBinary:
        begin
          ALine := '  SAVE_FLOWS';
          NameFile.Add(ALine);
        end;
      csfListing:
        begin
          ALine := '  PRINT_FLOWS';
          NameFile.Add(ALine);
        end;
      else
        Assert(False);
    end;


    NameFile.Add('END OPTIONS');

    NameFile.Add('');

    NameFile.Add('BEGIN PACKAGES');
    NameFile.AddStrings(InputFiles);
    NameFile.Add('END PACKAGES');
  end;
end;

procedure TCustomPackageWriter.RemoveNoDefinedError
  (var NoDefinedErrorRoot: string);
begin
  NoDefinedErrorRoot := Format(StrNoDefinedBoundarie,
    [Package.PackageIdentifier]);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, NoDefinedErrorRoot);
end;

procedure TCustomPackageWriter.ShowNoBoundaryError
  (const NoDefinedErrorRoot: string);
begin
  frmErrorsAndWarnings.AddError(Model, NoDefinedErrorRoot,
    Format(StrTheSPackageHasB, [Package.PackageIdentifier]));
end;

procedure TCustomTransientWriter.GetITMP(var ITMP: integer; TimeIndex: integer;
  var List: TValueCellList);
var
  PriorList: TValueCellList;
  IdenticalValues: boolean;
  Cell: TValueCell;
  CellIndex: integer;
  PriorCell: TValueCell;
begin
  List := FValues[TimeIndex];
  ITMP := List.Count;
  if TimeIndex > 0 then
  begin
    PriorList := FValues[TimeIndex - 1];
    if (List.Count = PriorList.Count) and (List.Count > 0) then
    begin
      IdenticalValues := True;
      for CellIndex := 0 to List.Count - 1 do
      begin
        Cell := List[CellIndex] as TValueCell;
        PriorCell := PriorList[CellIndex] as TValueCell;
        if not Cell.IsIdentical(PriorCell) then
        begin
          IdenticalValues := False;
          break;
        end;
      end;
      if IdenticalValues then
      begin
        ITMP := -1;
      end;
    end;
    PriorList.Cache;
  end;
end;

procedure TCustomModflowWriter.WriteMf6_DataSet(DataArray: TDataArray;
  const ID: string);
var
  LayerIndex: integer;
  MF6_ArrayName: string;
begin
  Assert(DataArray <> nil);
  for LayerIndex := 0 to DataArray.LayerCount - 1 do
  begin
    if LayerIndex = 0 then
    begin
      MF6_ArrayName := ID;
    end
    else
    begin
      MF6_ArrayName := '';
    end;
    WriteArray(DataArray, LayerIndex, ID + ' ' + IntToStr(LayerIndex + 1),
      StrNoValueAssigned, MF6_ArrayName, False);
  end;
end;

procedure TCustomModflowWriter.WritePestFormulaOrValue(const PestName,
  PestSeriesName: string; PestMethod: TPestParamMethod; Value: double);
begin
  if (PestName <> '') or (PestSeriesName <> '') then
  begin
    FPestParamUsed := True;
  end;
  if Model.PestUsed and WritingTemplate and
    ((PestName <> '') or (PestSeriesName <> '')) then
  begin
    WritePestTemplateFormula(Value, PestName, PestSeriesName, PestMethod, nil,
      0, False);
  end
  else
  begin
    WriteFloat(Value);
  end;
end;

procedure TCustomModflowWriter.WriteSaveFlowsOption;
begin
  { TODO -cMODFLOW-6 : Need to modify for MODFLOW-6 to allow printing and saving in the same model }
  if Model.ModflowOutputControl.SaveCellFlows = csfBinary then
  begin
    WriteString('    SAVE_FLOWS');
    NewLine;
  end;
end;

procedure TCustomModflowWriter.WriteBeginOptions;
begin
  WriteString('BEGIN OPTIONS');
  NewLine;
end;

procedure TCustomModflowWriter.WriteEndOptions;
begin
  WriteString('END OPTIONS');
  NewLine;
  NewLine;
end;

{ TMf6_SimNameFileWriter }

procedure TMf6_SimNameFileWriter.AddExchange(ExchangeLine: string);
begin
  FExchanges.Add(ExchangeLine);
end;

procedure TMf6_SimNameFileWriter.AddModel(ModelData: TModelData);
begin
  Assert(ModelData.ModelNameFile <> '');
  Assert(ModelData.ModelName <> '');
  Assert(ModelData.SolutionGroup <> '');
  Assert(ModelData.ImsFile <> '');
  Assert(ModelData.MaxIterations > 0);
  FModelDataList.Add(ModelData);
end;

constructor TMf6_SimNameFileWriter.Create(AModel: TCustomModel);
begin
  inherited Create(AModel, etExport);
  FModelDataList := TModelDataList.Create;
  FExchanges := TStringList.Create;
  FGwtTDisFileNames := TStringList.Create;
  FSimFileNames := TStringList.Create;
end;

destructor TMf6_SimNameFileWriter.Destroy;
begin
  FSimFileNames.Free;
  FGwtTDisFileNames.Free;
  FExchanges.Free;
  FModelDataList.Free;
  inherited;
end;

class function TMf6_SimNameFileWriter.Extension: string;
begin
  Assert(False);
end;

function TMf6_SimNameFileWriter.GetGwtTDisFileNames(
  SpeciesIndex: Integer): string;
begin
  result := FGwtTDisFileNames[SpeciesIndex];
end;

function TMf6_SimNameFileWriter.GetTDisFileName: string;
begin
  result := FTDisFileName;
end;

function TMf6_SimNameFileWriter.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TMf6_SimNameFileWriter.SetGwtTDisFileNames(SpeciesIndex: Integer;
  const Value: string);
begin
  Assert(SpeciesIndex >= 0);
  while SpeciesIndex >= FGwtTDisFileNames.Count do
  begin
    FGwtTDisFileNames.Add('');
  end;
  FGwtTDisFileNames[SpeciesIndex] := Value;
end;

procedure TMf6_SimNameFileWriter.SetTDisFileName(const Value: string);
begin
  FTDisFileName := Value;
end;

procedure TMf6_SimNameFileWriter.WriteExchanges;
var
  index: Integer;
begin
  WriteString('BEGIN EXCHANGES');
  NewLine;
  if Model.GwtUsed and not Model.SeparateGwtUsed then
  begin
    for index := 0 to FExchanges.Count - 1 do
    begin
      WriteString('  ');
      WriteString(FExchanges[index]);
      NewLine;
    end;
  end;
  WriteString('END EXCHANGES');
  NewLine;
  NewLine;
end;

procedure TMf6_SimNameFileWriter.WriteFile(FileName: string);
var
  FileRoot: WideString;
  BackupFileName: WideString;
  Limit: Integer;
//  Index: Integer;
  SpeciesIndex: Integer;
  SpeciesName: string;
begin
  if Model.ModelSelection <> msModflow2015 then
  begin
    Exit;
  end;
  FileRoot := ChangeFileExt(ExtractFileName(FileName), '.');
  BackupFileName := IncludeTrailingPathDelimiter(ExtractFileDir(FileName))
    + FileRoot + 'mfsim.nam';
  FileName := IncludeTrailingPathDelimiter(ExtractFileDir(FileName))
    + 'mfsim.nam';
  FInputFileName := FileName;
  FSpeciesIndex := -1;
  FWritingFlowModel := True;
  WriteFileInternal(FileName, BackupFileName);
  TFile.Copy(BackupFileName, FileName, True);

  FWritingFlowModel := False;
  if Model.GwtUsed and Model.SeparateGwtUsed then
  begin
    FSeparateGwt := Model.ModflowPackages.GwtProcess.SeparateGwt;
    Limit := Model.MobileComponents.Count;

    for SpeciesIndex := 0 to Limit - 1 do
    begin
      FSpeciesIndex := SpeciesIndex;
      if Model.MobileComponents[SpeciesIndex].UsedForGWT then
      begin
        SpeciesName := Model.MobileComponents[SpeciesIndex].Name + '.';
        BackupFileName := IncludeTrailingPathDelimiter(ExtractFileDir(FileName))
          + FileRoot + SpeciesName + 'mfsim.nam';
        WriteFileInternal(FileName, BackupFileName);
      end;
    end;
  end;

end;

function TMf6_SimNameFileWriter.GetShouldWriteLine(GwtUsed: Boolean;
  SeparateGwtUsed: Boolean; ModelIndex: Integer): Boolean;
begin
  result := True;
  if not GwtUsed then
  begin
    result := True;
  end
  else if FWritingFlowModel and (ModelIndex = 0) then
  begin
    result := True;
  end
  else if SeparateGwtUsed then
  begin
    result := (ModelIndex - 1 = FSpeciesIndex) and (ModelIndex > 0);
  end;
end;

function TMf6_SimNameFileWriter.GetSimFileName(Index: Integer): string;
begin
  result := FSimFileNames[Index];
end;

function TMf6_SimNameFileWriter.GetSimFileNameCount: Integer;
begin
  result := FSimFileNames.Count
end;

procedure TMf6_SimNameFileWriter.WriteFileInternal(FileName: string; BackupFileName: WideString);
begin
  FSimFileNames.Add(BackupFileName);
  OpenFile(BackupFileName);
  try
    WriteOptions;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteTiming;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteModels;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteExchanges;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    WriteSolutionGroups;
  finally
    CloseFile;
  end;
  Model.AddModelInputFile(FileName);
  Model.AddModelOutputFile(ChangeFileExt(FileName, '.lst'));
end;

procedure TMf6_SimNameFileWriter.WriteModels;
var
  ModelIndex: Integer;
  ShouldWriteLine: Boolean;
  GwtUsed: Boolean;
  SeparateGwtUsed: Boolean;
begin
  Assert(FModelDataList.Count > 0);
  WriteString('BEGIN MODELS');
  NewLine;

  GwtUsed := Model.GwtUsed;
  SeparateGwtUsed := Model.SeparateGwtUsed;

  for ModelIndex := 0 to FModelDataList.Count - 1 do
  begin
    ShouldWriteLine := GetShouldWriteLine(GwtUsed, SeparateGwtUsed, ModelIndex);
    if ShouldWriteLine then
    begin
      WriteString(FModelDataList[ModelIndex].ModelLine);
      NewLine;
    end;
  end;

  WriteString('END MODELS');
  NewLine;
  NewLine;
end;

procedure TMf6_SimNameFileWriter.WriteOptions;
var
  SmsPkg: TSmsPackageSelection;
  HasOptions: Boolean;
  OC: TModflowOutputControl;
  procedure WriteBegin;
  begin
    if not HasOptions then
    begin
      WriteBeginOptions;
      HasOptions := True;
    end;
  end;
begin
  SmsPkg := Model.ModflowPackages.SmsPackage;
  HasOptions := False;
  if SmsPkg.ContinueModel then
  begin
    WriteBegin;
    WriteString('  CONTINUE');
    NewLine;
  end;

  if SmsPkg.MaxErrors > -1 then
  begin
    WriteBegin;
    WriteString('  MAXERRORS');
    WriteInteger(SmsPkg.MaxErrors);
    NewLine;
  end;

  if SmsPkg.CheckInput = ciDontCheck then
  begin
    WriteBegin;
    WriteString('  NOCHECK');
    NewLine;
  end;

  case SmsPkg.MemoryPrint of
    mpNone:
      begin
        // do nothing
      end;
    mpSummary:
      begin
        WriteBegin;
        WriteString('  MEMORY_PRINT_OPTION SUMMARY');
        NewLine;
      end;
    mpAll:
      begin
        WriteBegin;
        WriteString('  MEMORY_PRINT_OPTION ALL');
        NewLine;
      end;
    else
      Assert(False);
  end;

  OC := Model.ModflowOutputControl;
  if OC.PrintInputArrays or OC.PrintInputCellLists then
  begin
    WriteBegin;
    WriteString('  PRINT_INPUT');
    NewLine;
  end;

  if HasOptions then
  begin
    WriteEndOptions;
  end;
end;

procedure TMf6_SimNameFileWriter.WriteSolutionGroups;
var
  ModelIndex: Integer;
  ModelData: TModelData;
  GwtUsed: Boolean;
  SeparateGwtUsed: Boolean;
  ShouldWriteLine: Boolean;
begin

  GwtUsed := Model.GwtUsed;
  SeparateGwtUsed := Model.SeparateGwtUsed;


  // If ModelMuse ever supports more than one solution group,
  // the following commented out text might be a starting point..
//  FModelDataList.Sort(TComparer<TModelData>.Construct(
//    function (const Left, Right: TModelData): Integer
//    begin
//      result := AnsiCompareText(Left.SolutionGroup, Right.SolutionGroup);
//      if result = 0 then
//      begin
//        result := AnsiCompareText(Left.ImsFile, Right.ImsFile);
//      end;
//    end));

  WriteString('BEGIN SOLUTIONGROUP');
  WriteInteger(1);
  NewLine;


//  isg := 0;
  for ModelIndex := 0 to FModelDataList.Count - 1 do
  begin
    ModelData := FModelDataList[ModelIndex];
//    NewGroup := False;
//    NewImsLine := False;
//    if (ModelIndex = 0) then
//    begin
//      NewGroup := True;
//    end
//    else if AnsiCompareText(ModelData.SolutionGroup,
//      PriorModelData.SolutionGroup) <> 0 then
//    begin
//      NewLine;
//      WriteString('END SOLUTIONGROUP');
//      NewLine;
//      NewLine;
//      NewGroup := True;
//    end;
//    if NewGroup then
//    begin
//      Inc(isg);
//      WriteString('BEGIN SOLUTIONGROUP');
//      WriteInteger(isg);
//      NewLine;

    if ModelIndex = 0 then
    begin
      WriteString('  MXITER');
      if FModelDataList.Count > 1 then
      begin
        WriteInteger(ModelData.MaxIterations);
      end
      else
      begin
        WriteInteger(1);
      end;
      NewLine;
    end;
//      WriteString('  MXITER');
//      if FModelDataList.Count > 1 then
//      begin
//        WriteInteger(ModelData.MaxIterations);
//      end
//      else
//      begin
//        WriteInteger(1);
//      end;
//      NewImsLine := True;
//    end;

//    if AnsiCompareText(ModelData.ImsFile,
//      PriorModelData.ImsFile) <> 0 then
//    begin
//      NewImsLine := True;
//    end;

//    if NewImsLine then
//    begin
//      NewLine;
    ShouldWriteLine := GetShouldWriteLine(GwtUsed, SeparateGwtUsed, ModelIndex);
    if ShouldWriteLine then
    begin
      WriteString('  IMS6 ');
      WriteString('''' +  ExtractFileName(ModelData.ImsFile) + ''' ');
  //      NewLine;
  //    end;
      WriteString('''' +  ModelData.ModelName + ''' ');
      NewLine;
    end;

//    PriorModelData := ModelData;
  end;


//  NewLine;
  WriteString('END SOLUTIONGROUP');
  NewLine;
  NewLine;
end;

procedure TMf6_SimNameFileWriter.WriteTiming;
var
  GwtUsed: Boolean;
  SeparateGwtUsed: Boolean;
  ShouldWriteLine: Boolean;
  ModelIndex: Integer;
begin
  Assert(TDisFileName <> '');
  WriteString('BEGIN TIMING');
  NewLine;

  GwtUsed := Model.GwtUsed;
  SeparateGwtUsed := Model.SeparateGwtUsed;

  if FSpeciesIndex = -1 then
  begin
    WriteString('  TDIS6 ');
    WriteString('''' + ExtractFileName(TDisFileName) + '''');
    NewLine;
  end;

  if SeparateGwtUsed then
  begin
    for ModelIndex := 1 to FModelDataList.Count - 1 do
    begin
      ShouldWriteLine := GetShouldWriteLine(GwtUsed, SeparateGwtUsed, ModelIndex);
      if ShouldWriteLine then
      begin
        WriteString('  TDIS6 ');
        WriteString('''' + ExtractFileName(GwtTDisFileNames[ModelIndex-1]) + '''');
        NewLine;
      end;
    end;
  end;

  WriteString('END TIMING');
  NewLine;
  NewLine;
end;

function TMf6_SimNameFileWriter._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TMf6_SimNameFileWriter._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

procedure TCustomTransientWriter.WriteDimensionsMF6;
begin
  CountCells(MAXBOUND);
  WriteBeginDimensions;
  try
    WriteString('  MAXBOUND');
    WriteInteger(MAXBOUND);
    NewLine;
  finally
    WriteEndDimensions;
  end;
end;

procedure TCustomTransientWriter.WriteMF6ObsOption(InputFileName: string);
var
  NameOfFile: string;
begin
  if (FlowObsLocations <> nil) and (FlowObsLocations.Count > 0)
    or (ToMvrFlowObsLocations <> nil) and (ToMvrFlowObsLocations.Count > 0)
    or (WellReductionFlowObsLocations <> nil) and (WellReductionFlowObsLocations.Count > 0)
  then
  begin
    WriteString('    OBS6 FILEIN ');
    NameOfFile := ObservationFileName(InputFileName);
    Model.AddModelInputFile(NameOfFile);
    NameOfFile := ExtractFileName(NameOfFile);
    WriteString(NameOfFile);
    NewLine;
  end;
end;

procedure TCustomPackageWriter.WriteTimeSeriesFiles(InputFileName: string);
var
  Groups: TTimesSeriesGroups;
  GroupIndex: Integer;
  AGroup: TTimesSeriesCollection;
  FileIndex: Integer;
  TimeSeriesWriter: TMf6TimeSeriesWriter;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
begin
  if FTimeSeriesNames.Count > 0 then
  begin
    if not WritingTemplate then
    begin
      Groups := TTimesSeriesGroups.Create;
      try
        Model.Mf6TimesSeries.GetTimesSeriesGroups(FTimeSeriesNames, Groups);
        for GroupIndex := 0 to Groups.Count - 1 do
        begin
          AGroup := Groups[GroupIndex];
          TimeSeriesWriter := TMf6TimeSeriesWriter.Create(Model, etExport);
          try
            FTimeSeriesFileNames.Add(TimeSeriesWriter.
              WriteFile(FInputFileName, AGroup));
          finally
            TimeSeriesWriter.Free;
          end;
        end;
        for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
        begin
          AScreenObject := Model.ScreenObjects[ScreenObjectIndex];
          AScreenObject.DynamicTimesSeriesCollections.ClearLocations;
        end;
      finally
        Groups.Free;
      end;
      Model.ModelInputFiles.AddStrings(FTimeSeriesFileNames);
    end;
    for FileIndex := 0 to FTimeSeriesFileNames.Count - 1 do
    begin
      WriteString('    TS6 FILEIN ');
      WriteString(ExtractFileName(FTimeSeriesFileNames[FileIndex]));
      NewLine;
    end;
//    FTimeSeriesFileNames.Clear;
  end;
end;

procedure TCustomListWriter.WriteAdditionalAuxVariables;
begin
  // do nothing
end;

procedure TCustomListWriter.WriteListOptions(InputFileName: string);
begin
  { TODO -cMODFLOW 6 : Support additional MODFLOW-6 options }
  // PACKAGENAME not currently supported.
  WriteString('    AUXILIARY IFACE');
  WriteAdditionalAuxVariables;
  NewLine;

  WriteString('    BOUNDNAMES');
  NewLine;

  PrintListInputOption;
  WriteSaveFlowsOption;
  WriteMoverOption;
  WriteTimeSeriesFiles(InputFileName);

  WriteMF6ObsOption(InputFileName)
end;

procedure TCustomListWriter.WriteOptionsMF6(InputFileName: string);
begin
  WriteBeginOptions;
  try
    WriteListOptions(InputFileName);
  finally
    WriteEndOptions;
  end;
end;

procedure TCustomTransientWriter.WriteBoundName(ACell: TValueCell);
var
  ScreenObject: TScreenObject;
  BoundName: string;
begin
  if (Model.ModelSelection = msModflow2015) then
  begin
    if ACell.Mf6ObsName <> '' then
    begin
      BoundName := ' ''' + ACell.Mf6ObsName + ''' ';
      WriteString(BoundName);
      if ACell.ScreenObject <> nil then
      begin
        ScreenObject := ACell.ScreenObject as TScreenObject;
        WriteString('# ' +ScreenObject.Name);
      end;
    end
    else if ACell.ScreenObject = nil then
    begin
      WriteString(' _ ');
    end
    else
    begin
      ScreenObject := ACell.ScreenObject as TScreenObject;
      BoundName := Copy(ScreenObject.Name, 1, MaxBoundNameLength);
      BoundName := ' ''' + BoundName + ''' ';
      WriteString(BoundName);
    end;
  end;
end;

procedure TCustomModflowWriter.WriteBeginDimensions;
begin
  WriteString('BEGIN DIMENSIONS');
  NewLine;
end;

procedure TCustomModflowWriter.WriteEndDimensions;
begin
  WriteString('END DIMENSIONS');
  NewLine;
  NewLine;
end;

//procedure TCustomParameterTransientWriter.WriteBeginPeriod(TimeIndex: integer);
//begin
//  if Model.ModelSelection = msModflow2015 then
//  begin
//    WriteString('BEGIN PERIOD ');
//    WriteInteger(TimeIndex + 1);
//    NewLine;
//  end;
//end;

procedure TCustomTransientArrayWriter.WriteEndPeriod;
begin
  if Model.ModelSelection = msModflow2015 then
  begin
    WriteString('END PERIOD ');
    NewLine;
    NewLine;
  end;
end;

function TCustomPackageWriter.PestObsNameOK(const AName: string): boolean;
begin
  result := (Length(AName) <= 20)
    and (Pos(' ', AName) <= 0)
    and (Pos('[', AName) <= 0)
    and (Pos(']', AName) <= 0)
    and (Pos('(', AName) <= 0)
    and (Pos(')', AName) <= 0)
    and (Pos('!', AName) <= 0)
    and (Pos('@', AName) <= 0)
end;

procedure TCustomPackageWriter.PrintConcentrationOption;
var
  ConcentrationOC: THeadDrawdownOutputControl;
begin
  ConcentrationOC := Model.ModflowOutputControl.ConcentrationOC;
  if ConcentrationOC.PrintInListing then
  begin
    WriteString('  PRINT_CONCENTRATION');
    NewLine;
  end;
end;

procedure TCustomPackageWriter.PrintFlowsOption;
var
  OutputControl: TModflowOutputControl;
begin
  OutputControl := Model.ModflowOutputControl;
  if OutputControl.SaveCellFlows = csfListing then
  begin
    WriteString('  PRINT_FLOWS');
    NewLine;
  end;
end;

procedure TCustomPackageWriter.PrintOutputOptions;
var
  OutputControl: TModflowOutputControl;
begin
  OutputControl := Model.ModflowOutputControl;
  if OutputControl.PrintInputArrays then
  begin
    WriteString('  PRINT_INPUT');
    NewLine;
  end;
  PrintFlowsOption;
  WriteSaveFlowsOption;
//  if OutputControl.SaveCellFlows = csfBinary then
//  begin
//    WriteString('  SAVE_FLOWS');
//    NewLine;
//  end;
end;

procedure TCustomPackageWriter.WriteBeginConnectionData;
begin
  WriteString('BEGIN CONNECTIONDATA');
  NewLine;
end;

procedure TCustomPackageWriter.WriteEndConnectionData;
begin
  WriteString('END CONNECTIONDATA');
  NewLine;
  NewLine;
end;

procedure TCustomModflowWriter.WriteBeginPeriod(StressPeriodIndex: integer);
begin
  if Model.ModelSelection = msModflow2015 then
  begin
    WriteString('BEGIN PERIOD ');
    WriteInteger(StressPeriodIndex + 1);
    NewLine;
  end;
end;

procedure TCustomModflowWriter.WriteEndPeriod;
begin
  if Model.ModelSelection = msModflow2015 then
  begin
    WriteString('END PERIOD ');
    NewLine;
    NewLine;
  end;
end;

procedure TCustomFileWriter.WriteFormulaOrValueBasedOnAPestName(
  const PestName: string; Value: double; Layer, Row, Column: Integer);
var
  Param: TModflowSteadyParameter;
  DataArray: TDataArray;
  TemplateCharacter: string;
  Formula: WideString;
  ExtendedTemplateCharacter: Char;
  OldDecimalSeparator: Char;
begin
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  try
    if not WritingTemplate then
    begin
      WriteFloat(Value);
      if PestName <> '' then
      begin
        Param := Model.GetPestParameterByName(PestName);
        if Param <> nil then
        begin
          Param.IsUsedInTemplate := True;
          Model.WritePValAndTemplate(Param.ParameterName, Param.Value, Param,
            Param.UsedDirectly);
          FPestParamUsed := True;
        end
        else if Layer >= 0 then
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(PestName);
          if DataArray <> nil then
          begin
            FPestParamUsed := True;
            AddUsedPestDataArray(DataArray);
          end;
        end;
      end;
    end
    else
    begin
      if PestName = '' then
      begin
        WriteFloat(Value);
      end
      else
      begin
        Param := Model.GetPestParameterByName(PestName);
        if Param <> nil then
        begin
          Param.IsUsedInTemplate := True;
          TemplateCharacter := Model.PestProperties.TemplateCharacter;
          Formula := Format(' %0:s                    %1:s%0:s ',
            [TemplateCharacter, Param.ParameterName]);
          WriteString(Formula);
        end
        else if Layer >= 0 then
        begin
          DataArray := Model.DataArrayManager.GetDataSetByName(PestName);
          Assert(DataArray <> nil);
          Assert (DataArray.PestParametersUsed);
          Formula := GetPestNonTransientTemplateFormula(DataArray,
            Layer, Row, Column);
          if Formula = '' then
          begin
            WriteFloat(Value);
  //          Exit;
          end
          else
          begin
            ExtendedTemplateCharacter := Model.PestProperties.ExtendedTemplateCharacter;
            Formula := Format(' %0:s                    %1:s%0:s ',
              [ExtendedTemplateCharacter, Formula]);
            WriteString(Formula);
          end;
        end
        else
        begin
          WriteFloat(Value);
        end;
      end;
    end;
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;

procedure TCustomModflowWriter.WriteHeader(const DataType: TRbwDataType;
  const Comment, MF6_ArrayName: string);
begin
  case DataType of
    rdtDouble:
      begin
        WriteU2DRELHeader(Comment, matStructured, MF6_ArrayName);
      end;
    rdtInteger, rdtBoolean:
      begin
        WriteU2DINTHeader(Comment, matStructured, MF6_ArrayName);
      end;
  else
    Assert(False);
  end;
end;

procedure TCustomParameterTransientWriter.WriteMf6ParamListOption;
var
  ParameterIndex: integer;
  AParam: TModflowTransientListParameter;
  SeriesIndex: integer;
  TimeSeries: TTimeSeries;
  TimeSeriesWriter: TTimeSeriesWriter;
  FileNameTS: string;
  SeriesStart: Integer;
  SIndex: Integer;
  procedure WriteTimeSeries;
  begin
    TimeSeriesWriter := TTimeSeriesWriter.Create(Model, etExport,
      TimeSeries, SeriesStart);
    try
      TimeSeriesWriter.WriteFile(FileNameTS)
    finally
      TimeSeriesWriter.Free;
    end;
    FileNameTS := ExtractFileName(FileNameTS);
    WriteString('    TS6 FILEIN ');
    WriteString(FileNameTS);
    NewLine;
  end;
begin
  for ParameterIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
  begin
    AParam := Model.ModflowTransientParameters[ParameterIndex];
    if (AParam.ParameterType = ParameterType) then
    begin
      for SeriesIndex := 0 to AParam.TimeSeriesList.Count - 1 do
      begin
        TimeSeries := AParam.TimeSeriesList[SeriesIndex];
        SeriesStart := 0;
        SIndex := 1;
        if TimeSeries.SeriesCount < MaxSeries then
        begin
          FileNameTS := ChangeFileExt(NameOfFile, '') + '.' +
            TimeSeries.ParameterName + '.' + TimeSeries.ObjectName +
            '.time_series';
          WriteTimeSeries;
        end
        else
        begin
          while SeriesStart < TimeSeries.SeriesCount do
          begin
            FileNameTS := ChangeFileExt(NameOfFile, '') + '.' +
              TimeSeries.ParameterName + '.' + TimeSeries.ObjectName
              + '__' + SIndex.ToString +
              '.time_series';
            WriteTimeSeries;
            Inc(SIndex);
            Inc(SeriesStart, MaxSeries);
          end;
        end;
      end;
    end;
  end;
end;

procedure TCustomParameterTransientWriter.CountParametersAndParameterCells
  (var ParamCount, ParamCellCount: integer);
var
  ParamIndex: integer;
  Times: TList;
  MaxCells: integer;
  TimeIndex: integer;
  Cells: TValueCellList;
begin
  ParamCount := 0;
  ParamCellCount := 0;
  for ParamIndex := 0 to FParamValues.Count - 1 do
  begin
    Times := FParamValues.Objects[ParamIndex] as TList;
    MaxCells := 0;
    for TimeIndex := 0 to Times.Count - 1 do
    begin
      Cells := Times[TimeIndex];
      if MaxCells < Cells.Count then
      begin
        MaxCells := Cells.Count;
      end;
    end;
    ParamCellCount := ParamCellCount + MaxCells * Times.Count;
    if MaxCells > 0 then
    begin
      Inc(ParamCount);
    end
    else
    begin
      // This is done to flag the parameter has having no cells
      // associated with it.  Later on, an error message will
      // be displayed about this problem.
      FParamValues[ParamIndex] := '';
    end;
  end;
end;

procedure TCustomModflowWriter.WriteBeginGridData;
begin
  WriteString('BEGIN GRIDDATA');
  NewLine;
end;

procedure TCustomModflowWriter.WriteEndGridData;
begin
  WriteString('END GRIDDATA');
  NewLine;
  NewLine;
end;

procedure TCustomPackageWriter.WriteBoundNamesOption;
begin
  WriteString('    BOUNDNAMES');
  NewLine;
end;

function TCustomTransientArrayWriter.OkLocationMF6(IDomain: TDataArray;
  UsedLocations: T2DSparseBooleanArray; var Layer: integer; Row, Column: integer;
  Option: TLayerOption): boolean;
var
  LayerIndex: Integer;
begin
  if Option = loTopActive then
  begin
    result := False;
    for LayerIndex := 0 to IDomain.LayerCount - 1 do
    begin
      result := (IDomain.IntegerData[LayerIndex, Row, Column] > 0);
      if result then
      begin
        Layer := LayerIndex;
        break;
      end;
    end;
  end
  else
  begin
    result := (IDomain.IntegerData[Layer, Row, Column] > 0)

//    if result and (Option <> loSpecified) then
//    begin
//      if (Layer > 0) and (IDomain.IntegerData[Layer-1, Row, Column] > 0) then
//      begin
//        result := False;
//      end;
//    end;
  end;
  if result then
  begin
    result := (not UsedLocations.IsValue[Row, Column]);
  end;
end;

function TCustomTransientArrayWriter.CountCellsMF6
  (RateList: TValueCellList; Option: TLayerOption): integer;
var
  CellIndex: integer;
  // ValueCell: TRch_Cell;
  IDomain: TDataArray;
  UsedLocations: T2DSparseBooleanArray;
  ValueCell: TValueCell;
  Layer: Integer;
begin
  result := 0;
  IDomain := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);
  // Assert(DepthSurfaceCellList.Count = RchRateList.Count);
  UsedLocations := T2DSparseBooleanArray.Create(GetQuantum(IDomain.RowCount),
    GetQuantum(IDomain.ColumnCount));
  try
    for CellIndex := RateList.Count - 1 downto 0 do
    begin
      ValueCell := RateList[CellIndex];
      if (Option = loTop) and (ValueCell.Layer <> 0) then
      begin
        ValueCell.Layer := 0;
      end;
      Layer := ValueCell.Layer;
      if OkLocationMF6(IDomain, UsedLocations, Layer, ValueCell.Row,
        ValueCell.Column, Option) then
      begin
        UsedLocations.Items[ValueCell.Row, ValueCell.Column] := True;
        Inc(result);
      end;
    end;
  finally
    UsedLocations.Free;
  end;
end;

//procedure TCustomFileWriter.WriteTemplateReplace(ParameterName: string);
//var
//  TemplateCharacter: string;
//begin
//  TemplateCharacter := Model.PestProperties.TemplateCharacter;
//  WriteString(Format(' %0:s                %1:s%0:s',
//    [TemplateCharacter, ParameterName]));
//end;

//procedure TCustomFileWriter.WriteArrayReplacementFormula(DataSetName: string;
//  ModifierValue: double; Method: TPestParamMethod; Layer, Row, Column: Integer);
//var
//  ArrayTemplateCharacter: string;
//  ExtendedTemplateCharacter: string;
//  Operation: string;
//begin
//  ArrayTemplateCharacter := Model.PestProperties.ArrayTemplateCharacter;
//  ExtendedTemplateCharacter := Model.PestProperties.ExtendedTemplateCharacter;
//  case Method of
//    ppmMultiply:
//      begin
//        Operation := '*';
//      end;
//    ppmAdd:
//      begin
//        Operation := '+';
//      end;
//  end;
//  WriteString
//    (Format(StrArrayFormulaFormat,
//    [ExtendedTemplateCharacter, ArrayTemplateCharacter, DataSetName,
//    ModifierValue, Operation, Layer, Row, Column]));
//end;

procedure TCustomFileWriter.WriteModflowParamFormula(ModflowParameterName: string;
  PestParValue: string; Value: double; ACell: TValueCell);
var
  CellValueReplacement: string;
  TemplateCharacter: string;
  ExtendedTemplateCharacter: string;
  ArrayTemplateCharacter: Char;
  OldDecimalSeparator: Char;
  procedure GetCellData;
  var
    ModifierValue: Double;
    LocalLayer: Integer;
    Param: TModflowSteadyParameter;
    DataArray: TDataArray;
    DataArrayLayer: Integer;
    AScreenObject: TScreenObject;
  begin
    ModifierValue := 0;
    Param := Model.GetPestParameterByName(PestParValue);
    if Param <> nil then
    begin
      Param.IsUsedInTemplate := True;
      Param.UsedDirectly := True;
	    ModifierValue := Param.Value;
      //WriteTemplateReplace(PestParValue);
      Model.WritePValAndTemplate(Param.ParameterName, Param.Value, Param, True);
      CellValueReplacement := Format(' %0:s                    %1:s%0:s',
        [TemplateCharacter, PestParValue])
    end
    else
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(PestParValue);
      if DataArray <> nil then
      begin
        if DataArray.PestParametersUsed then
        begin
          if DataArray.Orientation = dsoTop then
          begin
            DataArrayLayer := 0;
          end
          else
          begin
            DataArrayLayer := ACell.Layer;
          end;
          ModifierValue := DataArray.RealData[DataArrayLayer, ACell.Row, ACell.Column];

          if Model.ModelSelection in ModflowSelection then
          begin
            LocalLayer := Model.DataSetLayerToModflowLayer(ACell.Layer);
          end
          else
          begin
            LocalLayer := ACell.Layer + 1;
          end;
          CellValueReplacement := Format(' %0:s                    %1:s[%2:d, %3:d, %4:d]%0:s',
            [ArrayTemplateCharacter, DataArray.Name,
            LocalLayer, ACell.Row+1, ACell.Column+1]);

  //        WriteArrayReplace(DataArray.Name, DataArrayLayer, Row, Column);
        end
        else
        begin
          CellValueReplacement := ''
        end;
      end
      else
      begin
        CellValueReplacement := '';
        AScreenObject := ACell.ScreenObject as TScreenObject;
        frmErrorsAndWarnings.AddError(Model, 'Unrecognized PEST parameter or data set',
          Format('%0:s was not recognized in %1:s',
          [PestParValue, AScreenObject.Name]), AScreenObject);
      end;
	  end;

    if CellValueReplacement <> '' then
    begin
      if ModifierValue = 0 then
      begin
        Value := 0;
      end
      else
      begin
        Value := Value/ModifierValue;
      end;
    end;
  end;
begin
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  try
    if PestParValue <> '' then
    begin
      TemplateCharacter := Model.PestProperties.TemplateCharacter;
      ArrayTemplateCharacter :=Model.PestProperties.ArrayTemplateCharacter;
      ExtendedTemplateCharacter := Model.PestProperties.ExtendedTemplateCharacter;
      GetCellData;
      WriteString
        (Format(' %0:s                    %1:s                    %2:s%1:s * %3:g * %4:s%0:s ',
        [ExtendedTemplateCharacter, TemplateCharacter, ModflowParameterName,
        Value, CellValueReplacement]));

    end
    else
    begin
      WriteTemplateFormula(ModflowParameterName, Value, ppmMultiply)
    end;
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;


procedure TCustomFileWriter.WriteTemplateFormula(ParameterName: string;
  ModifierValue: double; Method: TPestParamMethod);
var
  TemplateCharacter: string;
  ExtendedTemplateCharacter: string;
  Operation: string;
  OldDecimalSeparator: Char;
begin
  TemplateCharacter := Model.PestProperties.TemplateCharacter;
  ExtendedTemplateCharacter := Model.PestProperties.ExtendedTemplateCharacter;
  case Method of
    ppmMultiply:
      begin
        Operation := '*';
      end;
    ppmAdd:
      begin
        Operation := '+';
      end;
  end;
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  try
    WriteString
      (Format(StrPestFormulaFormat,
      [ExtendedTemplateCharacter, TemplateCharacter, ParameterName,
      ModifierValue, Operation]));
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;

procedure TCustomParameterTransientWriter.WriteBoundaryArrayParams;
var
  ParamIndex: integer;
  Param: TModflowTransientListParameter;
begin
  if WritingTemplate then
  begin
    Exit;
  end;
  for ParamIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    Param := Model.ModflowTransientParameters[ParamIndex];
    if Param.ParameterType = ParameterType then
    begin
      Model.WritePValAndTemplate(Param.ParameterName, Param.Value, Param);
    end;
  end;
end;


//procedure TCustomPackageWriter.WriteNoNewtown;
//begin
//  if Package.NewtonFormulation = nfOff then
//  begin
//    WriteString('    NO_NEWTON');
//    NewLine;
//  end;
//end;
{ TLayerArrayWriter }

class function TLayerArrayWriter.Extension: string;
begin
  result := '';
end;

function TLayerArrayWriter.WriteFile(const AFileName: string;
  ADataArray: TDataArray; ALayer: Integer): string;
var
  ArrayName: string;
  OutputFileName: string;
  OutputDirectory: string;
begin
  ArrayName := ADataArray.Name;
  Assert(ArrayName <> '');
  OutputFileName := ChangeFileExt(AFileName, '');
  OutputFileName := OutputFileName + '.' + Trim(ArrayName) + '_'
    + IntToStr(ALayer+1) + StrArraysExt;

  result := ExtractFileName(OutputFileName);
  OutputDirectory := ExtractFileDir(OutputFileName);
  OutputDirectory := IncludeTrailingPathDelimiter(OutputDirectory) + StrArrays;
  if not DirectoryExists(OutputDirectory) then
  begin
    CreateDir(OutputDirectory);
  end;
  result := IncludeTrailingPathDelimiter(OutputDirectory) + result;
  FInputFileName := result;

  OpenFile(result);
  try
    WriteArrayValues(ALayer, ADataArray);
  finally
    CloseFile;
  end;
  ADataArray.CacheData;
  Model.FilesToDelete.Add(result);
end;

procedure TCustomFileWriter.WritePestZones(DataArray: TDataArray;
  InputFileName: string; const DataArrayID, Prefix: string);
var
  PestZoneWriter: TParameterZoneWriter;
begin
  if (Model.PestUsed) and DataArray.PestParametersUsed then
  begin
    PestZoneWriter := TParameterZoneWriter.Create(Model, etExport);
    try
      PestZoneWriter.WriteFile(InputFileName, DataArray, DataArrayID, Prefix);
    finally
      PestZoneWriter.Free;
    end;
  end;
end;

procedure TCustomFileWriter.OpenTempFile(const FileName: string);
begin
  Assert(FFileStream <> nil);
  FFileStreamList.Add(FFileStream);
  FFileStream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
end;

class function TCustomFileWriter.PestUtilityProgramPath(UtilityProgramName,
  AFileName: string): string;
begin
  try
    result := IncludeTrailingPathDelimiter
      (frmGoPhast.PhastModel.ProgramLocations.PestDirectory) + UtilityProgramName;
    if TFile.Exists(result) then
    begin
      Exit;
    end;
    result := IncludeTrailingPathDelimiter(ExtractFileDir(Application.ExeName))
       + UtilityProgramName;
    if TFile.Exists(result) then
    begin
      Exit;
    end;
    result := IncludeTrailingPathDelimiter(ExtractFileDir(AFileName))
       + UtilityProgramName;
    if TFile.Exists(result) then
    begin
      Exit;
    end;
    result := UtilityProgramName;
  finally
    if frmGoPhast.PhastModel.PestUsed then
    begin
      MoveAppToDirectory(ExpandFileName(result), ExtractFileDir(AFileName));
      result := UtilityProgramName;
    end;
    result := '"' + result + '"';
  end;
end;

procedure TCustomFileWriter.CloseTempFile;
begin
  Assert(FFileStreamList.Count > 0);
  FreeAndNil(FFileStream);
  FFileStream := FFileStreamList.Last;
  FFileStreamList.Delete(FFileStreamList.Count-1);
end;

function TCustomModflowWriter.WriteValueOrFormula(Cell: TValueCell;
  Index: integer; FixedLength: Integer; ChangeSign: Boolean): double;
var
  Value: double;
  PestItem: string;
  PestSeries: string;
  PestMethod: TPestParamMethod;
  DataArray: TDataArray;
  TimeSeriesName: string;
  OldDecimalSeparator: Char;
begin
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  try
    if (Model.ModelSelection = msModflow2015) then
    begin
      TimeSeriesName := Cell.Mf6TimeSeriesName[Index];
      if TimeSeriesName <> '' then
      begin
        WriteString(' ');
        WriteString(TimeSeriesName);
        result := 0;
        Exit;
      end;
    end;

    Value := Cell.RealValue[Index, Model];
    if ChangeSign then
    begin
      Value := -Value;
    end;
    PestItem := Cell.PestName[Index];
    PestSeries := Cell.PestSeriesName[Index];
    if (PestItem <> '') or (PestSeries <> '') then
    begin
      FPestParamUsed := True;
    end;
    if Model.PestUsed and WritingTemplate and
      ((PestItem <> '') or (PestSeries <> '')) then
    begin
      PestMethod := Cell.PestSeriesMethod[Index];
      result := WritePestTemplateFormula(Value, PestItem, PestSeries,
        PestMethod, Cell, FixedLength, ChangeSign);
    end
    else
    begin
      if FixedLength = 0 then
      begin
        WriteFloat(Value);
      end
      else if FixedLength = 10 then
      begin
        WriteF10Float(Value);
      end
      else if FixedLength = 15 then
      begin
        WriteF15Float(Value);
      end
      else
      begin
        Assert(False);
      end;
      result := Value;

      if PestItem <> '' then
      begin
        DataArray := Model.DataArrayManager.GetDataSetByName(PestItem);
        if DataArray <> nil then
        begin
          AddUsedPestDataArray(DataArray);
        end;
      end;
      if PestSeries <> '' then
      begin
        DataArray := Model.DataArrayManager.GetDataSetByName(PestSeries);
        if DataArray <> nil then
        begin
          AddUsedPestDataArray(DataArray);
        end;
      end;
    end;
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;

procedure TCustomFileWriter.WriteTemplateHeader;
var
  ArraysFileName: string;
begin
  if WritingTemplate then
  begin
    WriteString('ptf ');
    WriteString(Model.PestProperties.TemplateCharacter);
    NewLine;
    WriteString('etf ');
    WriteString(Model.PestProperties.ExtendedTemplateCharacter);
    NewLine;
  end;
  ArraysFileName := WriteArraysFile(FNameOfFile);
  if (ArraysFileName <> '') and WritingTemplate then
  begin
    WriteString(Model.PestProperties.ExtendedTemplateCharacter);
    WriteString('ReadArrays(');
    WriteString(ExtractFileName(ArraysFileName));
    WriteString(')');
    WriteString(Model.PestProperties.ExtendedTemplateCharacter);
    NewLine;
  end;
end;

procedure TCustomFileWriter.AssignPestFormula(var Formula: string;
const PestSeriesName: string; SeriesMethod: TPestParamMethod;
PestNames: TStringList);
var
  Param: TModflowSteadyParameter;
  PestDataArray: TDataArray;
  OldDecimalSeparator: Char;
begin
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  try
    Param := Model.GetPestParameterByName(Formula);
    if Param <> nil then
    begin
      Param.IsUsedInTemplate := True;
      PestNames.Add(Param.ParameterName);
      Formula := FortranFloatToStr(Param.Value);
      FPestParamUsed := True;
    end
    else
    begin
      PestDataArray := Model.DataArrayManager.GetDataSetByName(Formula);
      if (PestDataArray <> nil) and PestDataArray.PestParametersUsed then
      begin
        FPestParamUsed := True;
        PestNames.Add(PestDataArray.Name);
        AddUsedPestDataArray(PestDataArray);
      end
      else
      begin
        PestNames.Add('');
      end;
    end;
    if PestSeriesName <> '' then
    begin
      Param := Model.GetPestParameterByName(PestSeriesName);
      if Param <> nil then
      begin
        Param.IsUsedInTemplate := True;
        FPestParamUsed := True;
        case SeriesMethod of
          ppmMultiply:
            begin
              Formula := Format('(%0:s) * %1:g', [Formula, Param.Value]);
            end;
          ppmAdd:
            begin
              Formula := Format('(%0:s) + %1:g', [Formula, Param.Value]);
            end;
        end;
      end
      else
      begin
        PestDataArray := Model.DataArrayManager.GetDataSetByName(PestSeriesName);
        if (PestDataArray <> nil) and PestDataArray.PestParametersUsed then
        begin
          FPestParamUsed := True;
          case SeriesMethod of
            ppmMultiply:
              begin
                Formula := Format('(%0:s) * %1:s', [Formula, PestDataArray.Name]);
              end;
            ppmAdd:
              begin
                Formula := Format('(%0:s) + %1:s', [Formula, PestDataArray.Name]);
              end;
          end;
          AddUsedPestDataArray(PestDataArray);
        end;
      end;
    end;
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;

function TCustomFileWriter.DataArrayUsesPestParameters(const DataArray
  : TDataArray): boolean;
begin
  Assert(Model <> nil);
  result := Model.PestUsed and (DataArray.DataType = rdtDouble) and
    DataArray.PestParametersUsed;
end;

procedure TCustomFileWriter.WriteDataArrayValueOrFormula(DataArray: TDataArray;
Layer, Row, Col: Integer);
var
  Formula: string;
  ExtendedTemplateCharacter: string;
begin
  Formula := GetPestNonTransientTemplateFormula(DataArray, Layer, Row, Col);
  if not WritingTemplate or (Formula = '') then
  begin
    WriteFloat(DataArray.RealData[Layer, Row, Col]);
    if Formula <> '' then
    begin
      FPestParamUsed := True;
      AddUsedPestDataArray(DataArray);
    end;
  end
  else
  begin
    ExtendedTemplateCharacter := Model.PestProperties.ExtendedTemplateCharacter;
    Formula := Format(' %0:s                    %1:s%0:s ',
      [ExtendedTemplateCharacter, Formula]);
    WriteString(Formula);
  end;
end;


{ TMf6GwtNameWriter }

procedure TMf6GwtNameWriter.AddPackageFile(FileType, FileName,
  PackageName: string);
begin
  if PackageName = '' then
  begin
    FPackageLines.Add(Format('  %0:s %1:s', [FileType, ExtractFileName(FileName)]));
  end
  else
  begin
    FPackageLines.Add(Format('  %0:s %1:s %2:s',
      [FileType, ExtractFileName(FileName),PackageName]));
  end;
  Model.AddModelInputFile(FileName)
end;

constructor TMf6GwtNameWriter.Create(AModel: TCustomModel;
  const FileName: string; SpeciesIndex: Integer; EvaluationType: TEvaluationType);
begin
  inherited Create(AModel, EvaluationType);
  FPackageLines := TStringList.Create;
  FSpeciesIndex := SpeciesIndex;
  FSpeciesName := AModel.MobileComponents[SpeciesIndex].Name;
  FFileName := ChangeFileExt(FileName, '') + '.' + FSpeciesName + '.Gwt_nam';
end;

destructor TMf6GwtNameWriter.Destroy;
begin
  FPackageLines.Free;
  inherited;
end;

class function TMf6GwtNameWriter.Extension: string;
begin
  result := '.nam';
end;

procedure TMf6GwtNameWriter.WriteFile;
var
  SimNameWriter: IMf6_SimNameFileWriter;
  ModelData: TModelData;
begin

  ModelData.ModelType := mtGroundWaterTransport;
  ModelData.ModelName := FSpeciesName;
  ModelData.SolutionGroup := StrSolutionGroupName;
  ModelData.MaxIterations := Model.ModflowPackages.SmsPackage.SolutionGroupMaxIteration;
  ModelData.ImsFile := ChangeFileExt(FFileName, '.ims');
  Model.AddModelInputFile(ModelData.ImsFile);
  ModelData.ModelNameFile := FFileName;

  SimNameWriter := Model.SimNameWriter;
  SimNameWriter.AddModel(ModelData);
  // write GWT name file.

  FNameOfFile := FFileName;
  OpenFile(FNameOfFile);
  try
    WriteCommentLine(File_Comment('Transport name file for ' + FSpeciesName));
    WriteOptions;
    WritePackages;
  finally
    CloseFile;
  end;
end;

procedure TMf6GwtNameWriter.WriteOptions;
var
  ListFileName: string;
begin
  WriteBeginOptions;

  ListFileName := ChangeFileExt(FFileName, '.lst');
  Model.AddModelOutputFile(ListFileName);
  WriteString('  LIST ');
  WriteString(ExtractFileName(ListFileName));
  NewLine;

  PrintListInputOption;
  WriteSaveFlowsOption;

  WriteEndOptions;
end;

procedure TMf6GwtNameWriter.WritePackages;
var
  PackageIndex: Integer;
begin
  WriteString('BEGIN PACKAGES');
  NewLine;

  for PackageIndex := 0 to FPackageLines.Count - 1 do
  begin
    WriteString(FPackageLines[PackageIndex]);
    NewLine;
  end;

  WriteString('END PACKAGES');
  NewLine;
end;

procedure TCustomFileWriter.WriteBeginPackageData;
begin
  WriteString('BEGIN PACKAGEDATA');
  NewLine;
end;

procedure TCustomModflowWriter.WriteEndPackageData;
begin
  WriteString('END PACKAGEDATA');
  NewLine;
  NewLine;
end;

function TCustomListWriter.GetObjectString(ErrorObject: TObject): string;
var
  ACrop: TCropItem;
  AClimate: TClimateItem;
  Farm: TFarm;
  ASoil: TSoilItem;
  AScreenObject: TScreenObject;
begin
  if ErrorObject = nil then
  begin
    result := '';
  end
  else if ErrorObject is TSoilItem then
  begin
    ASoil := TSoilItem(ErrorObject);
    result := Format(StrSoilS, [ASoil.SoilName]);
  end
  else if ErrorObject is TCropItem then
  begin
    ACrop := TCropItem(ErrorObject);
    result := Format(StrCropS, [ACrop.CropName]);
  end
  else if ErrorObject is TClimateItem then
  begin
    AClimate := TClimateItem(ErrorObject);
    result := Format(StrClimateStartingTim, [AClimate.StartTime]);
  end
  else if ErrorObject is TFarm then
  begin
    Farm := TFarm(ErrorObject);
    result := Format(StrErrorInFarmD, [Farm.FarmID]);
  end
  else if ErrorObject is TScreenObject then
  begin
    AScreenObject := TScreenObject(ErrorObject);
    result := AScreenObject.Name;
  end
  else
  begin
    Assert(False);
  end;
end;

function TCustomListWriter.EvaluateValueFromGlobalFormula(Formula: string;
  ErrorObject: TObject; const DataSetErrorString: string;
  const OKTypes: TRbwDataTypes): TExpression;
var
  Compiler: TRbwParser;
  ErrorFormula: string;
  ObjectString: string;
begin
  Compiler := Model.ParentModel.rpThreeDFormulaCompiler;
  ErrorFormula := Formula;
  try
    Compiler.Compile(Formula)
  except
    on E: ERbwParserError do
    begin
      ObjectString := GetObjectString(ErrorObject);
      frmFormulaErrors.AddFormulaError(ObjectString, DataSetErrorString,
        ErrorFormula, E.Message);
      Formula := '0';
      Compiler.Compile(Formula);
      // send error message
    end;
  end;
  result := Compiler.CurrentExpression;
  if result = nil then
  begin
    Formula := '0';
    Compiler.Compile(Formula);
    result := Compiler.CurrentExpression;
  end;
  if not(result.ResultType in OKTypes) then
  begin
    ObjectString := GetObjectString(ErrorObject);
    frmFormulaErrors.AddFormulaError(ObjectString, DataSetErrorString,
      ErrorFormula, StrInvalidResultType);
    if rdtInteger in OKTypes then
    begin
      Formula := '0';
    end
    else if rdtBoolean in OKTypes then
    begin
      Formula := 'False';
    end
    else if rdtString in OKTypes then
    begin
      Formula := '""';
    end
    else
    begin
      Assert(False);
      Compiler.Compile(Formula);
      // send error message
      result := Compiler.CurrentExpression;
    end;
  end;
  result.Evaluate;
end;

procedure TCustomListWriter.WriteFloatValueFromGlobalFormula(Formula: string;
  ErrorObject: TObject; const DataSetErrorString: string;
  TestProc: TTestRealValueOkProcedure = nil);
var
  Value: double;
  Expression: TExpression;
begin
  Expression := EvaluateValueFromGlobalFormula(Formula, ErrorObject,
    DataSetErrorString, [rdtDouble, rdtInteger]);
  if Expression.ResultType in [rdtDouble, rdtInteger] then
  begin
    Value := Expression.DoubleResult;
    WriteFloatCondensed(Value);
    if Assigned(TestProc) then
    begin
      TestProc(Value);
    end;
  end
  else
  begin
    WriteFloatCondensed(0);
    frmFormulaErrors.AddFormulaError(GetObjectString(ErrorObject),
      DataSetErrorString, Formula, StrTheFormulaShouldReal);
  end;
end;

procedure TCustomListWriter.WriteIntegerValueFromGlobalFormula(Formula: string;
  ErrorObject: TObject; const DataSetErrorString: string;
  TestValue: TTestIntValueOkProcedure = nil);
var
  Value: Integer;
  Expression: TExpression;
begin
  Expression := EvaluateValueFromGlobalFormula(Formula, ErrorObject,
    DataSetErrorString, [rdtInteger]);
  if Expression.ResultType = rdtInteger then
  begin
    Value := Expression.IntegerResult;
    WriteFreeInteger(Value);
    if Assigned(TestValue) then
    begin
      TestValue(Value);
    end;
  end
  else
  begin
    WriteFreeInteger(0);
    frmFormulaErrors.AddFormulaError(GetObjectString(ErrorObject),
      DataSetErrorString, Formula, StrTheFormulaShouldInt);
  end;
end;

initialization

LimitValue := 1.18e-38;
  NegLimitValue := -LimitValue;


end.
