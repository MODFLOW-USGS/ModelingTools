{@abstract(The main purposes of @name is to define @link(TDataArray) which
  provides an interface to a 3D array of data and
  @link(TCustom2DInterpolater) which provides an interface
  for 2D interpolation.)
  @seealso(PhastDataSets)
  @seealso(ModflowConstantHeadBoundaryUnit)

  @author(Richard B. Winston <rbwinst@usgs.gov>)
  }
unit DataSetUnit;

interface

uses System.UITypes, Windows, Math, ZLib, GR32, TempFiles,
  RealListUnit, SysUtils,
  Classes, Forms, RbwParser, GoPhastTypes, SubscriptionUnit,
  SparseDataSets, ObserverIntfU, FormulaManagerUnit, Dialogs,
  Generics.Collections, Generics.Defaults, SparseArrayUnit,
  FastGEO, DataArrayInterfaceUnit;

{ TODO :
Consider making dual data sets that can be evaluated at both elements
and nodes. }

type
  TCheckUsageEvent = procedure(Sender: TObject; var ShouldUse: Boolean) of object;

  // @name is a 3D array of real numbers.
  T3DRealDataSet = array of array of array of real;

  // @name is a 3D array of integers.
  T3DIntegerDataSet = array of array of array of integer;

  // @name is a 3D array of booleans.
  T3DBooleanDataSet = array of array of array of boolean;

  // @name is a 3D array of strings.
  T3DStringDataSet = array of array of array of string;

  TBoundaryType = (btUndefined, btPhastSpecifiedHead, btPhastFlux, btPhastLeaky,
    btPhastRiver, btPhastWell, btMfWell, btMfGhb, btMfDrn, btMfDrt, btMfRiv,
    btMfChd, btMfEts, btMfEt, btMfRch, btMfSfr, btMfStr, btMfUzf, btMfObs,
    btMfMnw, btMt3dSsm, btMfHfb, btSutraSpecifiedPressure, btSutraSpecifiedHead,
    btSutraSpecConcTemp, btSutraFluidFlux, btMassEnergyFlux, btSutraGeneralFlow,
    btSutraGenTransp, btMfFhb, btCFP, btMfFarm, btSWR, btMnw1, btMtmsObs, btRIP,
    btMt3dRchConc, mt3dUnsatConc, mt3dSatConc, btSfr_MF6, btMAW, btUzfMf6,
    btSft, btCSub, btMvr, btCnc, btSrc, btTransK, btTransS);

  TBoundaryTypes = set of TBoundaryType;

  TContourAlg = (caSimple, caACM626);

  TAngleType = (atNone, atDegrees, atRadians);



  // @abstract(@name is raised if a formula is assigned that, when compiled
  // gives a type of data that is incompatible with the type of data
  // stored in the @link(TDataArray).)
  EInvalidDataType = class(Exception)
    constructor Create(AMessage, AFormula: string);
  private
    FFormula: string;
  public
    property Formula: string read FFormula;
  end;

  // @abstract(@name is raised if an error occurs during interpolation.)
  EInterpolationException = class(Exception);

  // @abstract(@name is raised if, when
  // assigning values to a data set, a circular
  // reference is encountered so that the @link(TDataArray) depends on itself.)
//  ECircularReference = class(Exception);

  ECorruptedData = class(Exception);

  EInvalidTime = class(Exception);

  EInvalidGridOrMesh = class(Exception);

  // @name is the type of the TDataArray.@link(TDataArray.OnDataSetUsed) event.
  // TDataArray.@link(TDataArray.OnDataSetUsed)
  // is called by TDataArray.@link(TDataArray.UsedByModel)
  TObjectUsedEvent = function(Sender: TObject): boolean of object;

  TCustom2DInterpolater = class;

  // @name is used in @link(TDataArray.ChangeAFormula)
  // where it is used to get the @link(TObserver)s that
  // will affect the @link(TDataArray) whose formula is
  // being changed.
  TUseListFunction = function: TStringList of Object;

  {@abstract(When the grid is colored by a @link(TDataArray), the
   colors can be restricted to lie within a range.
   @name is used to represent one end of the range.
   See @link(TColoringLimits).)}
  TColoringLimit = class(TPersistent)
  private
    // See @link(BooleanLimitValue).
    FBooleanLimitValue: boolean;
    // See @link(DataType).
    FDataType: TRbwDataType;
    // See @link(DefaultBooleanLimitValue).
    FDefaultBooleanLimitValue: boolean;
    // See @link(IntegerLimitValue).
    FIntegerLimitValue: integer;
    // See @link(RealLimitValue).
    FRealLimitValue: double;
    // See @link(StringLimitValue).
    FStringLimitValue: string;
    // See @link(UseLimit).
    FUseLimit: boolean;
    // See @link(OnChange).
    FOnChange: TNotifyEvent;
    // See @link(BooleanLimitValue).
    procedure SetBooleanLimitValue(const Value: boolean);
    // See @link(DataType).
    procedure SetDataType(const Value: TRbwDataType);
    // See @link(IntegerLimitValue).
    procedure SetIntegerLimitValue(const Value: integer);
    // See @link(RealLimitValue).
    procedure SetRealLimitValue(const Value: double);
    // See @link(StringLimitValue).
    procedure SetStringLimitValue(const Value: string);
    // See @link(UseLimit).
    procedure SetUseLimit(const Value: boolean);
  public
    // @name copies those parts of Value that are used
    // to the current instance of @classname (except for OnChange).
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    constructor Create;
    // @name is the value to used for @link(BooleanLimitValue) when
    // @link(UseLimit) is @false.
    property DefaultBooleanLimitValue: boolean read FDefaultBooleanLimitValue
      write FDefaultBooleanLimitValue;
    // If there is a change in @link(UseLimit),
    // @link(BooleanLimitValue), @link(IntegerLimitValue),
    // @link(RealLimitValue), or @link(StringLimitValue),
    // @name can be used to respond to that change.
    // Changing to @link(DataType) does not cause an @name event to occur.
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function OkLogLimit: boolean;
  published
    // If @link(DataType) is rdtBoolean and @link(UseLimit) is @True,
    // @name is the boolean value used to limit what values will
    // be used to color the grid.
    property BooleanLimitValue: boolean read FBooleanLimitValue write
      SetBooleanLimitValue;
    // @name indicates the data type (real, integer, boolean, or string)
    //  of the @link(TDataArray) or @link(TCustomModflowGridEdgeDisplay) to
    // which this limit applies.
    property DataType: TRbwDataType read FDataType write SetDataType;
    // If @link(DataType) is rdtInteger and @link(UseLimit) is @True,
    // @name is the integer value used to limit what values will
    // be used to color the grid.
    property IntegerLimitValue: integer read FIntegerLimitValue
      write SetIntegerLimitValue;
    // If @link(DataType) is rdtDouble and @link(UseLimit) is @True,
    // @name is the real value used to limit what values will
    // be used to color the grid.
    property RealLimitValue: double read FRealLimitValue write
      SetRealLimitValue;
    // If @link(DataType) is rdtString and @link(UseLimit) is @True,
    // @name is the string value used to limit what values will
    // be used to color the grid.
    property StringLimitValue: string read FStringLimitValue write
      SetStringLimitValue;
    // @name indicates whether this @classname should be used to limit
    // what values are used to color the grid.
    property UseLimit: boolean read FUseLimit write SetUseLimit;
  end;

  TSkipReal = class(TCollectionItem)
  private
    FRealValue: double;
    procedure SetRealValue(const Value: double);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property RealValue: double read FRealValue write SetRealValue;
  end;

  TCustomSkipCollection = class(TCollection)
  private var
    FOnChange: TNotifyEvent;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  protected
    procedure Update(Item: TCollectionItem);override;
  end;

  TSkipRealCollection = class(TCustomSkipCollection)
  public
    constructor Create;
    function IndexOf(AValue: double): integer;
  end;

  TSkipInteger = class(TCollectionItem)
  private
    FIntegerValue: integer;
    procedure SetIntegerValue(const Value: integer);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property IntegerValue: integer read FIntegerValue write SetIntegerValue;
  end;

  TSkipIntegerCollection = class(TCustomSkipCollection)
  public
    constructor Create;
  end;

  {@abstract(When the grid is colored by a @link(TDataArray), the
   colors can be restricted to lie within a range.
   @name is used to represent both ends of the range.
   See @link(TColoringLimit).)}
  TColoringLimits = class(TPersistent)
  private
    // See @link(LowerLimit).
    FLowerLimit: TColoringLimit;
    // See @link(UpperLimit).
    FUpperLimit: TColoringLimit;
    FActiveOnly: boolean;
    FIntegerValuesToSkip: TSkipIntegerCollection;
    FRealValuesToSkip: TSkipRealCollection;
    FStringValuesToSkip: TStrings;
    FLogTransform: boolean;
    FStoredEpsilon: TRealStorage;
    FShadeInactiveArea: boolean;
    FOnChange: TNotifyEvent;
    // See @link(LowerLimit).
    procedure SetLowerLimit(const Value: TColoringLimit);
    // See @link(UpperLimit).
    procedure SetUpperLimit(const Value: TColoringLimit);
    procedure SetActiveOnly(const Value: boolean);
    procedure SetIntegerValuesToSkip(const Value: TSkipIntegerCollection);
    procedure SetRealValuesToSkip(const Value: TSkipRealCollection);
    procedure SetStringValuesToSkip(const Value: TStrings);
    function StoreRealSkipValues: boolean;
    function StoreIntegerSkipValues: boolean;
    procedure SetLogTransform(const Value: boolean);
    function GetEpsilon: double;
    procedure SetEpsilon(const Value: double);
    procedure SetStoredEpsilon(const Value: TRealStorage);
    procedure Changed;
    procedure SetOnChange(const Value: TNotifyEvent);
  public
    // @name calls TColoringLimit.@link(TColoringLimit.Assign)
    // for @link(LowerLimit) and @link(UpperLimit) and then calls
    // @link(Update).
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    constructor Create;
    // @name destroys the the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
    // If both the @link(LowerLimit) and @link(UpperLimit) are used
    // (see TColoringLimit.@link(TColoringLimit.UseLimit)),
    // @name insures that the upper limit is greater than or
    // equal to the lower limit.
    procedure Update;
    function ValueOk(AValue: double): boolean; overload;
    function ValueOk(AValue: integer): boolean; overload;
    function ValueOk(const AValue: String): boolean; overload;
    property Epsilon: double read GetEpsilon write SetEpsilon;
    property ShadeInactiveArea: boolean read FShadeInactiveArea
      write FShadeInactiveArea;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  published
    property ActiveOnly: boolean read FActiveOnly write SetActiveOnly
      stored FActiveOnly;
    // @name is the lower limit on what values should be used to color
    // the grid.
    property LowerLimit: TColoringLimit read FLowerLimit write SetLowerLimit;
    // @name is the upper limit on what values should be used to color
    // the grid.
    property UpperLimit: TColoringLimit read FUpperLimit write SetUpperLimit;
    property RealValuesToSkip: TSkipRealCollection
      read FRealValuesToSkip write SetRealValuesToSkip
      stored StoreRealSkipValues;
    property IntegerValuesToSkip: TSkipIntegerCollection
      read FIntegerValuesToSkip write SetIntegerValuesToSkip
      stored StoreIntegerSkipValues;
    property StringValuesToSkip: TStrings read FStringValuesToSkip
      write SetStringValuesToSkip;
    property LogTransform: boolean read FLogTransform write SetLogTransform;
    property StoredEpsilon: TRealStorage read FStoredEpsilon
      write SetStoredEpsilon;
  end;

  // @name is the data type of @link(TDataArray.IsUniform).
  TIsUniform = (iuUnknown, iuFalse, iuTrue);

  TCustomTimeList = Class;

  TContours = class(TPersistent)
  private
    FSpecifyContours: boolean;
    FLineThicknesses: TOneDRealArray;
    FContourColors: TArrayOfColor32;
    FContourValues: TOneDRealArray;
    FAutomaticColors: boolean;
    FLogTransform: boolean;
    FContourStringValues: TStringList;
    function GetCount: integer;
    procedure SetCount(const Value: integer);
    procedure SetContourStringValues(const Value: TStringList);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadContourValues(Reader: TReader);
    procedure WriteContourValues(Writer: TWriter);
    procedure ReadLineThicknesses(Reader: TReader);
    procedure WriteLineThicknesses(Writer: TWriter);
    procedure ReadContourColors(Reader: TReader);
    procedure WriteContourColors(Writer: TWriter);

  public
    Constructor Create;
    Destructor Destroy; override;
    procedure Sort;
    procedure Assign(Source: TPersistent); override;
    property ContourValues: TOneDRealArray read FContourValues
      write FContourValues;
    property LineThicknesses: TOneDRealArray read FLineThicknesses
      write FLineThicknesses;
    property ContourColors: TArrayOfColor32 read FContourColors
      write FContourColors;
  published
    property AutomaticColors: boolean read FAutomaticColors
      write FAutomaticColors;
    property Count: integer read GetCount write SetCount;
    property SpecifyContours: boolean read FSpecifyContours
      write FSpecifyContours;
    property LogTransform: boolean read FLogTransform write FLogTransform;
    property ContourStringValues: TStringList read FContourStringValues
      write SetContourStringValues;
  end;

  TContourItem = class(TObject)
    Value: double;
    Color: TColor32;
    LineThickness: Double;
  end;

  TContourItemObjectList = TObjectList<TContourItem>;
  TContourComparer = TComparer<TContourItem>;



  {@abstract(@name provides an interface to a 3D array of data.)

  All descendants of @name that are instantiated must be registered
  with the system using RegisterClass in the initialization section
  of the unit in which they are declared.
  }
  TDataArray = class(TObserver, IDataArray)
  private
    // See @link(Annotation).
    FAnnotation: T3DStringDataSet;
    // See @link(CheckMax).
    FCheckMax: boolean;
    // See @link(CheckMin).
    FCheckMin: boolean;
    // See @link(ColumnCount).
    FColumnCount: integer;
    // Depending on @link(Datatype), @name will be either a
    // @link(T3DRealDataSet), @link(T3DIntegerDataSet),
    // @link(T3DBooleanDataSet), or @link(T3DStringDataSet).
    //
    // See @link(RealData), @link(IntegerData),
    // @link(BooleanData), and @link(StringData),
    FDataArray: pointer;
    // See @link(Datatype).
    FDataType: TRbwDataType;
    // See @link(EvaluatedAt).
    FEvaluatedAt: TEvaluatedAt;
    // @name is used as temporary storage when changing a formula.
    // See @link(Formula).
    FFormula: string;
    // See @link(LayerCount).
    FLayerCount: integer;
    // See @link(Limits).
    FLimits: TColoringLimits;
    // See @link(Lock).
    FLock: TDataLock;
    // See @link(Max).
    FMax: double;
    // See @link(Min).
    FMin: double;
    // See @link(Orientation).
    FOrientation: TDataSetOrientation;
    // See @link(RowCount).
    FRowCount: integer;
    // See @link(TwoDInterpolator).
    FTwoDInterpolator: TCustom2DInterpolater;
    // See @link(Units).
    FUnits: string;
    // See @link(UseList).
    FUseList: TStringList;
    // @name is used to indicate whether the values in @link(UseList)
    // need to be regenerated.  Changing @link(Formula) causes
    // @name to be set to @false. Calling @link(UpdateUseList) causes
    // @name to be set to @true.
    FUseListUpToDate: boolean;
    // See @link(Visible).
    FVisible: boolean;
    // See @link(OnDataSetUsed).
    FOnDataSetUsed: TObjectUsedEvent;
    // See @link(TCustomTimeList).
    FTimeList: TCustomTimeList;
    // See @link(Classification).
    FClassification: string;
    // See @link(OnPostInitialize).
    FOnPostInitialize: TNotifyEvent;
    // See @link(OnDestroy).
    FOnDestroy: TNotifyEvent;
    // See @link(ParameterUsed).
    FParameterUsed: boolean;
    // See @link(ParameterFormula).
    FParameterFormula: string;
    // See @link(UniformStringValue).
    FUniformStringValue: string;
    // See @link(UniformIntegerValue).
    FUniformIntegerValue: integer;
    // See @link(UniformAnnotation).
    FUniformAnnotation: string;
    // See @link(UniformBooleanValue).
    FUniformBooleanValue: boolean;
    // See @link(UniformRealValue).
    FUniformRealValue: double;
    // When the @Classname is cached, @name stores the number of layers.
    FCachedLayerCount: Integer;
    // When the @Classname is cached, @name stores the number of rows.
    FCachedRowCount: Integer;
    // When the @Classname is cached, @name stores the number of columns.
    FCachedColumnCount: Integer;
    // see @link(Comment).
    FComment: string;
    // see @link(AssociatedDataSets).
    FAssociatedDataSets: string;
    // See @link(MaxValue).
    FMaxValue: string;
    // See @link(MinValue).
    FMinValue: string;
    FMinReal: double;
    FMaxReal: double;
    FMinRealPositive: double;
    FMinInteger: Integer;
    FMaxInteger: Integer;
    FMinBoolean: Boolean;
    FMaxBoolean: Boolean;
    FMinString: string;
    FMaxString: string;
    FContourLimits: TColoringLimits;
    FContours: TContours;
    FFormulaObject: TFormulaObject;
    FHash: longint;
    FReadDataFromFile: Boolean;
    FUpdatingProgress: Boolean;
    FUseLgrEdgeCells: TLgrCellTreatment;
    FUnicodeSaved: Boolean;
    FDisplayName: string;
    FContourAlg: TContourAlg;
    FMinMaxUpToDate: Boolean;
    FAngleType: TAngleType;
    FContourInterval: TRealStorage;
    FOnInitialize: TNotifyEvent;
    FOnShouldUseOnInitialize: TCheckUsageEvent;
    FRefreshingOldUseList: boolean;
    FPestParametersUsed: Boolean;
    FUsedPestParameters: TStrings;
    FPestParametersAllowed: Boolean;
    FPilotPointsUsed: Boolean;
    FSuppressCache: Boolean;
    FPestArrayFileNames: TStringList;
    FTemplateNeeded: Boolean;
    // See @link(TwoDInterpolatorClass).
    function GetTwoDInterpolatorClass: string;
    // @name is called if an invalid formula has been specified.
    // @name changes the formula to something that is sure to work.
    procedure ResetFormula(const Compiler: TRbwParser;
      const ErrorMessage: string);
    // See @link(Formula).
    procedure SetFormula(const Value: string);
    // See @link(Limits).
    procedure SetLimits(const Value: TColoringLimits);
    // See @link(Lock).
    procedure SetLock(const Value: TDataLock);
    // See @link(TwoDInterpolator).
    procedure SetTwoDInterpolator(const Value: TCustom2DInterpolater);
    // See @link(TwoDInterpolatorClass).
    procedure SetTwoDInterpolatorClass(const Value: string);
    // See @link(Units).
    procedure SetUnits(const Value: string);
    // See @link(Visible).
    procedure SetVisible(const Value: boolean);
    // @name updates @link(UseList) with the variables
    // used by @link(Formula).
    procedure UpdateUseList;
    // See @link(Classification).
    procedure SetClassification(const Value: string);
    // See @link(Classification).
    function GetClassification: string;
    // See @link(ParameterFormula).
    procedure SetParameterFormula(const Value: string);
    // See @link(ParameterUsed).
    procedure SetParameterUsed(const Value: boolean);
    // @name gets the array used to store the boolean data.
    procedure GetBoolArray(var AnArray: T3DBooleanDataSet);
    // @name gets the array used to store the string data.
    procedure GetStringArray(var AnArray: T3DStringDataSet);
    // @name gets the array used to store the real-number data.
    procedure GetRealArray(var AnArray: T3DRealDataSet);
    // @name gets the array used to store the integer data.
    procedure GetIntegerArray(var AnArray: T3DIntegerDataSet);
    // See @link(LayerCount).
    function GetLayerCount: integer;
    // See @link(ColumnCount).
    function GetColumnCount: integer;
    // See @link(RowCount).
    function GetRowCount: integer;
    // See @link(Comment).
    procedure SetComment(const Value: string);
    // See @link(MaxValue).
    function GetMaxValue: string;
    // See @link(MinValue).
    function GetMinValue: string;
    procedure SetContourLimits(const Value: TColoringLimits);
    procedure ContourLimitsChanged(Sender: TObject);
    procedure SetContours(const Value: TContours);
    function GetFormula: string;
    function ValueOK(const Layer, Row, Col: Integer;
      LocalLimits: TColoringLimits): Boolean;
    function GetHash: longint;
    procedure UpdateSubscriptions(NewUseList: TStringList; OldUseList: TStringList);
    procedure ReadCompressedData(Stream: TStream);
    procedure WriteCompressedData(Stream: TStream);
    procedure SetUnicodeSaved(const Value: Boolean);
    procedure SetDisplayName(const Value: string);
    function GetDisplayName: string;
    function GetDisplayFormula: string;
    procedure SetContourAlg(const Value: TContourAlg);
    function GetMaxReal: double;
    function GetMinReal: double;
    function GetMinRealPositive: double;
    function GetMinInteger: integer;
    function GetMaxInteger: integer;
    function GetMinBoolean: boolean;
    function GetMaxBoolean: boolean;
    function GetMinString: string;
    function GetMaxString: string;
    procedure SetAngleType(const Value: TAngleType);
    procedure SetContourInterval(const Value: TRealStorage);
    procedure OnValueChanged(Sender: TObject);
    procedure SetOnShouldUseOnInitialize(const Value: TCheckUsageEvent);
    procedure SetPestParametersUsed(const Value: Boolean);
    procedure CreatePestParmNameDataSet;
    function GetParamDataSetName: string;
    procedure ApplyPestParameter;
    procedure SetUsedPestParameters(const Value: TStrings);
    procedure SetPestParametersAllowed(const Value: Boolean);
    procedure SetPilotPointsUsed(const Value: Boolean);
    function GetPestArrayFileNames: TStringList;
    procedure SetTemplateNeeded(const Value: Boolean);
  protected
    // See @link(DimensionsChanged).
    FDimensionsChanged: boolean;
    // See @link(IsUniform).
    FIsUniform: TIsUniform;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name is the @link(TPhastModel) that owns the @classname.
    FModel: IModelMuseModel;
    // @name is @true if the @classname has been cleared.
    FCleared: boolean;
    // @name is true if the @classname data has been stored in a temporary file.
    FDataCached: boolean;
    // @name is the name of the temporary file
    // used to store the @classname data.
    FTempFileName: string;

    // See @link(EvaluatedAt).
    procedure SetEvaluatedAt(const Value: TEvaluatedAt); virtual;
    procedure DefineProperties(Filer: TFiler); override;
    function GetCompiler: TRbwParser;
    // See @link(Orientation).
    procedure SetOrientation(const Value: TDataSetOrientation); virtual;
    // @name returns the dimensions of the @classname.
    procedure GetLimits(out ColLimit, RowLimit, LayerLimit: Integer);
    // @name reads the data that has been previously stored in a temporary file.
    // @seealso(StoreData)
    procedure ReadData(DecompressionStream: TDecompressionStream); virtual;
    // @name returns the dimensions of the @classname and the number of values
    // stored in the @classname.
    procedure CountValues(out LayerLimit, RowLimit, ColLimit, Count: Integer);
    // @name stores the data in @classname to a temporary file.
    // @seealso(ReadData)
    procedure StoreData(Stream: TStream); virtual;
    // @name restores the dimensions of the @classname to what they should be.
    procedure RestoreArraySize;
    // @name changes OldFormula to NewFormula and in the process updates
    // the list of @link(TObserver)s that affect the @classname.
    procedure ChangeAFormula(const NewFormula: string; var OldFormula: string;
      var UseListUpToDate: boolean; UseListFunction: TUseListFunction);
    // @name indicates that the dimensions of the data set have changed.
    // This can happen either by the number of rows, columns, or layers
    // in the grid has changed, because @link(EvaluatedAt) has changed,
    // or because the @link(Orientation) has changed.
    property DimensionsChanged: boolean read FDimensionsChanged;
    // See @link(Annotation).
    function GetAnnotation(const Layer, Row, Col: integer): string; virtual;
    // See @link(BooleanData).
    function GetBooleanData(const Layer, Row, Col: integer): boolean; virtual;
    // See @link(IntegerData).
    function GetIntegerData(const Layer, Row, Col: integer): integer; virtual;
    // See @link(IsValue).
    function GetIsValue(const Layer, Row, Col: Integer): boolean;
      virtual;
    // @name returns frmGoPhast.@link(TfrmGoPhast.PhastModel).
    function GetOwner: TPersistent; override;
    // See @link(RealData).
    function GetRealData(const Layer, Row, Col: integer): double; virtual;
    // @name gets the number of dimensions that @classname needs to have.
    procedure GetRequiredDimensions(out NumberOfLayers, NumberOfRows,
      NumberOfColumns: integer);
    // See @link(StringData).
    function GetStringData(const Layer, Row, Col: integer): string; virtual;
    // See @link(UseList).
    function GetUseList: TStringList; virtual;
    // @name is the event handler for
    // TColoringLimit.@link(TColoringLimit.OnChange).
    // If this @classname is being used to color the grid,
    // @name causes the colors displayed on the grid to be
    // recalculated and the display to be updated.
    procedure LimitsChanged(Sender: TObject);
    // See @link(Annotation).
    procedure SetAnnotation(const Layer, Row, Col: integer;
      const Value: string); virtual;
    // See @link(BooleanData).
    procedure SetBooleanData(const Layer, Row, Col: integer;
      const Value: boolean); virtual;
    // See @link(Datatype).
    procedure SetDataType(const Value: TRbwDataType); virtual;
    // @name sets the size of the array of data stored by
    // @classname to be set to the correct size.  If SetToZero
    // is true, the dimensions are all set to zero.
    procedure SetDimensions(const SetToZero: boolean = False); virtual;
    // See @link(IntegerData).
    procedure SetIntegerData(const Layer, Row, Col, Value: integer); virtual;
    // See @link(IsValue).
    procedure SetIsValue(const Layer, Row, Col: Integer;
      const Value: boolean); virtual;
    // @name calls inherited @name and then invalidates the model.
    procedure SetName(const Value: TComponentName); override;
    // See @link(RealData).
    procedure SetRealData(const Layer, Row, Col: integer;
      const Value: double); virtual;
    // See @link(StringData).
    procedure SetStringData(const Layer, Row, Col: integer;
      const Value: string); virtual;
    // @name calls inherited and also notifies the model that it has
    // changed.  If the @classname is newly set to be up-to-date, the
    // minimum and maximum values are calculated.
    procedure SetUpToDate(const Value: boolean); override;
    // @name is used to set values of some cells in a @classname
    // in some special way after it has been set in @link(Initialize).
    // @name calls @link(OnPostInitialize) if @link(OnPostInitialize)
    // is assigned.
    //
    // If a @classname overrides @link(Initialize), it should call @name
    // just before setting @link(TObserver.UpToDate TObserver.UpToDate)
    // to @true at the end of @link(Initialize).
    procedure PostInitialize;
    // @name sets the array size to (0,0,0).
    procedure Clear; virtual;
    // @name restores data from a temporary file.
    procedure RestoreData;
    // When appropriate, @name restores data from a temporary file.
    procedure CheckRestoreData;
    {@name sets @link(FIsUniform), @link(FUniformAnnotation),
     and depending on @link(DataType) it also
     sets one of the following:
     @unorderedlist(
       @item(@link(FUniformRealValue),)
       @item(@link(FUniformIntegerValue),)
       @item(@link(FUniformBooleanValue), or)
       @item(@link(FUniformStringValue).)
     )
    }
    procedure RemoveSubscription(Sender: TObject; const AName: string);
    procedure RestoreSubscription(Sender: TObject; const AName: string);
    procedure UpdateNotifiers; virtual;
    function IsSparseArray: boolean; virtual;
    function ShouldUseOnInitialize: Boolean;
    procedure HandleCircularReferenceError(ErrorMessage: string;
      ScreenObject: TObject);
//    procedure SetName(const Value: TComponentName); override;
    // LayerIndex should be between 0 and @link(LayerCount) -1
//    function IsLayerUsedWithParameters(const ALayer: Integer): Boolean;
  public
    property UnicodeSaved: Boolean read FUnicodeSaved write SetUnicodeSaved;
    procedure UpdateWithoutNotification(NewOrientation: TDataSetOrientation;
      NewEvaluatedAt: TEvaluatedAt; NewDataType: TRbwDataType;
      var NeedToInvalidate: boolean);
    procedure UpdateWithName(const AName: string); override;
    procedure RestoreUpToDateStatus;
    Procedure SetModelToNil;
    procedure RefreshUseList;
    function ColorGridValueOK(const Layer, Row, Col: integer): boolean;
    function ContourGridValueOK(const Layer, Row, Col: integer): boolean;
    procedure CheckIfUniform; virtual;
    procedure GetMinMaxStoredLimits(out LayerMin, RowMin, ColMin,
      LayerMax, RowMax, ColMax: integer); virtual;
    // @name updates @link(MinValue) and @link(MaxValue).
    procedure UpdateMinMaxValues;
    // @name does nothing if the Source is a @classname.
    // otherwise, it raises an exception.
    procedure Assign(Source: TPersistent); override;
    // @name stores the data in @classname in a temporary file.
    procedure CacheData;
    // @name adds to @link(Classification) information about whether a
    // @classname is required for the current model or not.
    function FullClassification: string;
    // When a value is assigned to a location in a @classname,
    // @name is used to specify how that value was assigned.
    // This can help the user understand why the value is what it is.
    property Annotation[const Layer, Row, Col: integer]: string read
      GetAnnotation write SetAnnotation;
    // @name gives the boolean value at the location specified by
    // Layer, Row, and Col.
    property BooleanData[const Layer, Row, Col: integer]: boolean read
      GetBooleanData write SetBooleanData;
    // @name gives the number of columns of data in the @classname.
    property ColumnCount: integer read GetColumnCount;
    // @name creates an instance of @classname.
    constructor Create(AnOwner: IModelMuseModel); reintroduce; virtual;
    // @name destroys the the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
    // @name is used when coloring the grid to specify that
    // the real number value should be
    // used when coloring the grid instead of the integer value.
    // In @classname, @name always returns false.
    // This is overridden in @link(PhastDataSets.TIntegerPhastDataSet)
    // and @link(PhastDataSets.TSparseIntegerPhastDataSet).
    function DisplayRealValue: boolean; virtual;
    // @name is the formula used to assign values to the @classname.
    property Formula: string read GetFormula write SetFormula;
    property DisplayFormula: string read GetDisplayFormula write SetFormula;
    // @name is the formula that is used to simulate how
    // MODFLOW parameters are used in MODFLOW.
    property ParameterFormula: string read FParameterFormula
      write SetParameterFormula;
    // @name indicates whether MODFLOW parameters
    // are used to set the @classname.
    property ParameterUsed: boolean read FParameterUsed write SetParameterUsed;
    // @name fills AStringList with the names of all the @classname
    // that depend on the current @classname.
    procedure FullUseList(const AStringList: TStringList);
    // @name assigns values to each location in
    // @link(RealData), @link(IntegerData),
    // @link(BooleanData), or @link(StringData) in @classname.
    // It first uses either @link(Formula) or @link(TwoDInterpolator)
    // to assign values and then assigns values using @link(TScreenObject)s.
    // See @link(TCustomPhastDataSet.Initialize
    // TCustomPhastDataSet.Initialize)
    // for a flow chart.
    // @seealso(PostInitialize)
    procedure Initialize; virtual;
    // @name gives the integer value at the location specified by
    // Layer, Row, and Col.
    property IntegerData[const Layer, Row, Col: integer]: integer read
      GetIntegerData write SetIntegerData;
    // @name is called when something has happened that would change
    // the values assigned to the @classname.
    procedure Invalidate; virtual;
    // @name indicates that a value is present at the location specified
    // by Layer, Row, Col. In @classname, @name is always true.
    // In some descendants of @classname, @name can be false.
    property IsValue[const Layer, Row, Col: Integer]: boolean read
      GetIsValue write SetIsValue;
    // @name indicates whether or not a @classname is uniform or not.
    property IsUniform: TIsUniform read FIsUniform;
    // If the @classname is uniform (see @link(IsUniform)
    // and @link(DataType) is rdtReal,
    // @name is the uniform value in the @classname.
    property UniformRealValue: double read FUniformRealValue;
    // If the @classname is uniform (see @link(IsUniform)
    // and @link(DataType) is rdtInteger,
    // @name is the uniform value in the @classname.
    property UniformIntegerValue: integer read FUniformIntegerValue;
    // If the @classname is uniform (see @link(IsUniform)
    // and @link(DataType) is rdtBoolean,
    // @name is the uniform value in the @classname.
    property UniformBooleanValue: boolean read FUniformBooleanValue;
    // If the @classname is uniform (see @link(IsUniform)
    // and @link(DataType) is rdtString,
    // @name is the uniform value in the @classname.
    property UniformStringValue: string read FUniformStringValue;
    // If the @classname is uniform (see @link(IsUniform),
    // @name is the annotation in the @classname.
    property UniformAnnotation: string read FUniformAnnotation;
    // @name gives the number of layers of data in the @classname.
    property LayerCount: integer read GetLayerCount;
    // @name gives the real number value at the location specified by
    // Layer, Row, and Col.
    property RealData[const Layer, Row, Col: integer]: double read GetRealData
      write SetRealData;
    // @name gives the number of rows of data in the @classname.
    property RowCount: integer read GetRowCount;
    // @name gives the string value at the location specified by
    // Layer, Row, and Col.
    property StringData[const Layer, Row, Col: integer]: string read
      GetStringData write SetStringData;
    // @name updates the number of dimensions that need to be in
    // the @classname.  It does not actually change the dimensions
    // of the array used to hold the data.  See @link(SetDimensions).
    procedure UpdateDimensions(NumberOfLayers, NumberOfRows,
      NumberOfColumns: integer; ForceResize: boolean = False); virtual;
    // @name returns true unless @link(OnDataSetUsed) is assigned.
    // in which case it calls @link(OnDataSetUsed) and returns its result.
    // See TPhastModel.@link(TCustomModel.ChemistryUsed).
    // See TPhastModel.@link(TCustomModel.EquilibriumPhasesUsed).
    // See TPhastModel.@link(TCustomModel.ExchangeUsed).
    // See TPhastModel.@link(TCustomModel.GasPhaseUsed).
    // See TPhastModel.@link(TCustomModel.InitialHeadUsed).
    // See TPhastModel.@link(TCustomModel.KineticsUsed).
    // See TPhastModel.@link(TCustomModel.SolidSolutionUsed).
    // See TPhastModel.@link(TCustomModel.SurfacesUsed).
    // See TPhastModel.@link(TCustomModel.InitialWaterTableUsed).
    function UsedByModel: boolean;
    // @name is a list of the variables used by the @link(Formula).
    property UseList: TStringList read GetUseList;
    // See @link(UsedByModel).
    property OnDataSetUsed: TObjectUsedEvent read FOnDataSetUsed write
      FOnDataSetUsed;

    // @name is the @link(TPhastTimeList) that controls this @classname.
    property ATimeList: TCustomTimeList read FTimeList write FTimeList;
    // @name is used to set values of some cells in a @classname
    // in some special way after it has been set in @link(Initialize).
    // @name is called in @link(PostInitialize) if @name
    // is assigned.
    //
    // If a @classname overrides @link(Initialize), it should call
    // @link(PostInitialize)
    // just before setting @link(TObserver.UpToDate TObserver.UpToDate)
    // to @true at the end of @link(Initialize).
    //
    // @name is assigned in @link(TCustomModel.UpdateOnPostInitialize).
    //
    // @seealso(PostInitialize)
    // @seealso(Initialize)
    property OnPostInitialize: TNotifyEvent read FOnPostInitialize
      write FOnPostInitialize;
    // @name is called when a @classname is being destroyed.
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    // @name indicates the names of the data sets in the
    // MODFLOW and/or PHAST model input files
    // that are associated with the @classname.
    property AssociatedDataSets: string read FAssociatedDataSets
      write FAssociatedDataSets;
    // @name is a string representation of the maximum value in the @classname.
    // See @link(UpdateMinMaxValues).
    property MaxValue: string read GetMaxValue;
    // @name is a string representation of the minimum value in the @classname.
    // See @link(UpdateMinMaxValues).
    property MinValue: string read GetMinValue;
    property MinReal: double read GetMinReal;
    property MaxReal: double read GetMaxReal;
    property MinRealPositive: double read GetMinRealPositive;
    property MinInteger: integer read GetMinInteger;
    property MaxInteger: integer read GetMaxInteger;
    property MinBoolean: boolean read GetMinBoolean;
    property MaxBoolean: boolean read GetMaxBoolean;
    property MinString: string read GetMinString;
    property MaxString: string read GetMaxString;
    // @name records the contouring data used to contour the data set.
    // @name may be nil.
    property Contours: TContours read FContours write SetContours;
    procedure RefreshFormula; virtual;
    procedure ComputeHash;
    property Hash: longint read GetHash;
    function IdenticalDataArrayContents(ADataArray: TDataArray): boolean;
    procedure AssignProperties(Source: TDataArray); virtual;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property Model: IModelMuseModel read FModel;
    property UseLgrEdgeCells: TLgrCellTreatment read FUseLgrEdgeCells write FUseLgrEdgeCells;
    property DisplayName: string read GetDisplayName write SetDisplayName;
    property ContourAlg: TContourAlg read FContourAlg write SetContourAlg;
    property OnInitialize: TNotifyEvent read FOnInitialize write FOnInitialize;
    property OnShouldUseOnInitialize: TCheckUsageEvent
      read FOnShouldUseOnInitialize write SetOnShouldUseOnInitialize;
    procedure AssignValuesWithScreenObjects;
    // The data set named @name will hold the names of parameters that apply
    // to a @classname
    property ParamDataSetName: string read GetParamDataSetName;
    property PilotPointsUsed: Boolean read FPilotPointsUsed
      write SetPilotPointsUsed;
    property PestArrayFileNames: TStringList read GetPestArrayFileNames;
    procedure Loaded; override;
    procedure AssignModel(AModel: IModelMuseModel);
  published
    // @name indicates the hierarchical position of this instance of
    // @classname when it is required by the model.
    property Classification: string read GetClassification
      write SetClassification;
    {If @name is @true, attempting to set
     @link(IntegerData) or @link(RealData) to a value
     greater than @link(Max) will cause
     it to be set to Trunc(@link(Max)) or @link(Max). }
    property CheckMax: boolean read FCheckMax write FCheckMax;
    {If @name is @true, attempting to set
     @link(IntegerData) or @link(RealData) to a value
     less than @link(Min) will cause
     it to be set to Trunc(@link(Min)) or @link(Min). }
    property CheckMin: boolean read FCheckMin write FCheckMin;
    // @name is a user-defined comment about the @classname.
    // When importing a @classname, @name may be assigned automatically.
    property Comment: string read FComment write SetComment;
    // @name indicates whether the data stored in @classname are
    // real numbers, integers, booleans, or strings.
    property DataType: TRbwDataType read FDataType write SetDataType;
    // @name indicates whether the data in @classname are evaluated at
    // element centers or nodes.
    property EvaluatedAt: TEvaluatedAt read FEvaluatedAt write SetEvaluatedAt;
    // @name specifies how (or if) the values used to color the grid should
    // be limited.  See @link(TColoringLimits).
    property Limits: TColoringLimits read FLimits write SetLimits;
    // @name specifies how (or if) the values used to color the grid should
    // be limited.  See @link(TColoringLimits).
    property ContourLimits: TColoringLimits read FContourLimits write SetContourLimits;
    // Members of @name are things the user ought not to be able to edit.
    property Lock: TDataLock read FLock write SetLock;
    {See @Link(CheckMax).}
    property Max: double read FMax write FMax;
    {See @Link(CheckMin).}
    property Min: double read FMin write FMin;
    // @name is used to
    // indicate whether the @classname is a 2D or 3D data set and,
    // if it is 2D, which face of the grid it is associated with.
    property Orientation: TDataSetOrientation read FOrientation
      write SetOrientation;
    // @name is used only for backwards compatibility.
    // @name must be before @link(TwoDInterpolator).
    // See @link(TwoDInterpolatorClass).
    property TwoInterpolatorClass: string read GetTwoDInterpolatorClass
      write SetTwoDInterpolatorClass stored False;
    // @name is the name of the Class of @link(TCustom2DInterpolater)
    // used with this @classname.  Assigning @name will cause an instance
    // of @link(TCustom2DInterpolater) to be created and assigned to
    // @link(TwoDInterpolator).
    //
    // Important: when reading a @classname from a stream
    // @name must be assigned
    // before @link(TwoDInterpolator) so that an instance of
    // @link(TCustom2DInterpolater) of the correct type will be created.
    // To achieve this, @name must appear before @link(TwoDInterpolator)
    // in the list of published properties.
    property TwoDInterpolatorClass: string read GetTwoDInterpolatorClass
      write SetTwoDInterpolatorClass;
    {@name is the @link(TCustom2DInterpolater) that is used to interpolate
     among @link(TScreenObject)s when assigning values to @classname.}
    property TwoDInterpolator: TCustom2DInterpolater read FTwoDInterpolator
      write SetTwoDInterpolator;
    // @name is used to indicate the units for the data in @classname.
    // @name is only a label; it has no significance for the model.
    property Units: string read FUnits write SetUnits;
    // @name indicates whether the @classname will be shown or not
    // when editing a @link(TScreenObject).
    property Visible: boolean read FVisible write SetVisible stored False;
    property AngleType: TAngleType read FAngleType write SetAngleType;
    property ContourInterval: TRealStorage read FContourInterval
      write SetContourInterval;
    property PestParametersUsed: Boolean read FPestParametersUsed
      write SetPestParametersUsed stored True;
    property UsedPestParameters: TStrings read FUsedPestParameters
      write SetUsedPestParameters;
    property PestParametersAllowed: Boolean read FPestParametersAllowed
      write SetPestParametersAllowed stored True;
    property TemplateNeeded: Boolean read FTemplateNeeded
      write SetTemplateNeeded stored True;
  end;

  TDataArrayList = TList<TDataArray>;
  TDataArrayObjectList = TObjectList<TDataArray>;

  TTempDataArrayStorage = class(TObject)
  private
    FModelList: TList;
    FDataArrayList: TList;
    FOwnsDataArrays: boolean;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function GetDataArray(Model: TBaseModel; Index: integer): TDataArray;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function GetDataArrayList(Model: TBaseModel): TDataArrayObjectList;
    procedure SetOwnsDataArrays(const Value: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property DataArrayList[Model: TBaseModel]: TDataArrayObjectList read GetDataArrayList;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property DataArray[Model: TBaseModel; Index: integer]: TDataArray read GetDataArray;
    property OwnsDataArrays: boolean read FOwnsDataArrays write SetOwnsDataArrays;
  end;

  {@name is used in creating descendants of @link(TDataArray) of the
   correct type when reading the model from a stream.
   See TDataSetItem.@link(PhastModelUnit.TDataSetItem.DataSetClass). }
  TDataArrayType = class of TDataArray;

  {@name is used in creating descendants of @link(TCustom2DInterpolater)
    of the
   correct type when reading a @link(TDataArray) from a stream.
   See TDataArray.@link(TDataArray.TwoDInterpolatorClass). }
  TInterpolatorType = class of TCustom2DInterpolater;

  // See TCustom2DInterpolater.@link(TCustom2DInterpolater.OnInitialize)
  // and TCustom2DInterpolater.@link(TCustom2DInterpolater.OnFinalize).
  TInitializeDataSetInterpolator = procedure(Sender: TObject;
    const DataSet: TDataArray) of object;

  {@abstract(@name provides an abstract interface for 2D interpolation.
   Descendants of @name provide concrete implementations.

   Call RegisterClass in the initialization section for any
   descendants that are instantiated.)

   Descendants of @name include @link(TCustomAnisotropicInterpolator),
   @link(TInvDistSq2DInterpolator), @link(TNearest2DInterpolator), and
   @link(TNearestPoint2DInterpolator)

   @seealso(InterpolationUnit).
   }
  TCustom2DInterpolater = class(TComponent)
  private
    // See @link(DataSet).
    FDataSet: TDataArray;
    // See @link(OnEdit).
    FOnEdit: TNotifyEvent;
    // See @link(OnFinalize).
    FOnFinalize: TInitializeDataSetInterpolator;
    // See @link(OnInitialize).
    FOnInitialize: TInitializeDataSetInterpolator;
    FEpsilonX: Double;
    FEpsilonY: Double;
  protected
    FReady: Boolean;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    FModel: TBaseModel;
    procedure SetEpsilonX(const Value: Double); virtual;
    procedure SetEpsilonY(const Value: Double); virtual;
    // @name fills ListOfScreenObjects with all the @link(TScreenObject)s
    // that should be used to assign values to cells with
    // this @classname.  The order of @link(TScreenObject)s
    // is the same as the order in
    // frmGoPhast.Model.@link(TCustomModel.ScreenObjects).
    procedure FillScreenObjectList(const ListOfScreenObjects: TList);
    // @name evaluates Expression and resets formula for the related data set
    // in AScreenObject in the event of an error.
    procedure EvaluateExpression(Compiler: TRbwParser;
      var Expression: TExpression; AScreenObject: TObject);
  public
    // @name copies the values from Source to the current @classname.
    procedure Assign(Source: TPersistent); override;
    // @name returns a boolean value at Location.
    function BooleanResult(const Location: TPoint2D): boolean; virtual;
    // @name creates an instance of @classname.
    // If Owner is a @link(TDataArray) @name
    // checks the @link(TDataArray.DataType).
    // @name makes itself a subcomponent of Owner.
    constructor Create(AOwner: TComponent); override;
    // @name is the @link(TDataArray) with which this
    // @classname will be used.  Normally @name is set
    // by passing a @link(TDataArray) as AOwner in @link(Create).
    property DataSet: TDataArray read FDataSet;
    // If @link(OnEdit) is assigned, @name calls @link(OnEdit).
    procedure Edit;
    // If @link(OnFinalize) is assigned, @name calls @link(OnFinalize).
    // @name is called just after the interpolation process ends.
    procedure Finalize(const DataSet: TDataArray); virtual;
    // If @link(OnInitialize) is assigned, @name calls @link(OnInitialize).
    // @name is called just before the interpolation process begins.
    procedure Initialize(const DataSet: TDataArray); virtual;
    // @name returns a integer value at Location.
    function IntegerResult(const Location: TPoint2D): integer; virtual;
    // @name is the name of the interpolator as displayed to the user.
    class function InterpolatorName: string; virtual; abstract;
    // @name returns a real-number value at Location.
    function RealResult(const Location: TPoint2D): real; virtual;
    // @name returns @true if AnotherInterpolator has the same parameters
    // as the @classname being called.
    function SameAs(AnotherInterpolator: TCustom2DInterpolater): boolean;
      virtual;
    // @name returns true if there are any @link(TScreenObject)s
    // that can be used for interpolation with this data set.
    function ShouldInterpolate: boolean; virtual;
    // @name returns a string value at Location.
    function StringResult(const Location: TPoint2D): string; virtual;
    // @name indicates the types of data that the current @classname
    // can be used with.
    class function ValidReturnTypes: TRbwDataTypes; virtual; abstract;
    // @name returns the orientations that are valid for interpolation
    // based on @link(TBaseModel.ModelSelection).
    class Function ValidOrientations: TDataSetOrientations; virtual;
    // @name is used to assign a comment to a particular cell in
    // a @link(TDataArray). It should be called immediately after a call to
    // @link(BooleanResult), @link(IntegerResult), @link(RealResult) or
    // @link(StringResult);
    function LastAnnotation: string; virtual;
    property EpsilonX: Double read FEpsilonX write SetEpsilonX;
    property EpsilonY: Double read FEpsilonY write SetEpsilonY;
    property Ready: Boolean read FReady;
  published
    // @name can be used to respond to a change in the interpolator.
    property OnEdit: TNotifyEvent read FOnEdit write FOnEdit;
    // If the @classname has to do something to finish up after the
    // interpolation process use OnFinalize to do it.
    property OnFinalize: TInitializeDataSetInterpolator read FOnFinalize
      write FOnFinalize;
    // If the @classname has to do something to get ready to start the
    // interpolation process use OnInitialize to do it.
    property OnInitialize: TInitializeDataSetInterpolator read FOnInitialize
      write FOnInitialize;
  end;

  // @abstract(@name is used to create
  // descendants of @link(TCustom2DInterpolater)
  // when reading a @link(TDataArray) from a file.)
  TInterpolatorClass = class of TCustom2DInterpolater;

  // @abstract(@name is the abstract ancestor of classes that use descendants
  // of @link(T3DSparsePointerArray) to store data.)
  // These data sets won't necessarily have a value defined at all locations.
  // Related data such as @link(TDataArray.Annotation) are affected too
  // so those are stored in a @link(T3DSparseStringArray).
  // The @link(TDataArray.Formula) is not used; only @link(TScreenObject)s
  // are used to assign values.
  TCustomSparseDataSet = class(TDataArray)
  private
    // See @link(BoundaryTypeDataSet).
    FBoundaryTypeDataSet: TDataArray;
    // @name is used to store the
    // @link(TDataArray.Annotation) at each location.
    FAnnotation: T3DSparseStringArray;
    FPriorLayer: Integer;
    FPriorRow: Integer;
    FPriorCol: Integer;
    FPriorResult: Boolean;
  protected
    procedure Clear; override;
    // See @link(TDataArray.Annotation).
    function GetAnnotation(const Layer, Row, Col: integer): string; override;
    // @name checks whether @link(TDataArray.Annotation) has been
    // assigned at Layer, Row, Col.
    // If @link(BoundaryTypeDataSet) has been assigned, it checks
    // it too.
    function GetIsValue(const Layer, Row, Col: Integer): boolean;override;
    // See @link(TDataArray.Annotation).
    procedure SetAnnotation(const Layer, Row, Col: integer;
      const Value: string); override;
    // @name clears @link(FAnnotation).
    procedure SetDimensions(const SetToZero: boolean); override;
    // If @link(BoundaryTypeDataSet) is assigned,
    // @name sets BoundaryTypeDataSet.IsValue to true at
    // Layer, Row, Col.
    procedure SetIsValue(const Layer, Row, Col: Integer;
      const Value: boolean);override;
    function IsSparseArray: boolean; override;
  public
    procedure Invalidate; override;
    // @name creates an instance of @classname and sets
    // @link(TDataArray.EvaluatedAt) to eaNodes.
    constructor Create(AnOwner: IModelMuseModel); override;
    // @name destroys the the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
    // @name assigns values to some location in
    // @link(TDataArray.RealData), @link(TDataArray.IntegerData),
    // @link(TDataArray.BooleanData), or @link(TDataArray.StringData)
    // in @classname.
    // Unlike @link(TDataArray) It doesn't uses either
    // @link(TDataArray.Formula) or @link(TDataArray.TwoDInterpolator)
    // to assign values. It only assigns values using @link(TScreenObject)s.
    procedure Initialize; override;

    // If this data set is used with a boundary condition
    // and several boundary conditions are mutually exclusive.
    // @name is used to check that the correct type of boundary
    // condition is stored at a
    // location when determining @link(TDataArray.IsValue).
    property BoundaryTypeDataSet: TDataArray read FBoundaryTypeDataSet
      write FBoundaryTypeDataSet;
    procedure UpdateDimensions(NumberOfLayers, NumberOfRows,
      NumberOfColumns: integer; ForceResize: boolean = False); override;
  end;

  {@abstract(@name is used to store real numbers in a sparse array.)}
  TRealSparseDataSet = class(TCustomSparseDataSet)
  private
    // @name is used to store the real numbers in @classname.
    FRealValues: T3DSparseRealArray;
    function GetMaxColumn: Integer;
    function GetMaxLayer: Integer;
    function GetMaxRow: Integer;
    function GetMinColumn: Integer;
    function GetMinLayer: Integer;
    function GetMinRow: Integer;
  protected
    procedure Clear; override;
    // @name checks whether @link(FRealValues) has been
    // assigned at Layer, Row, Col as well as inherited @name.
    function GetIsValue(const Layer, Row, Col: Integer): boolean;
      override;
    // @name retrieves the real number at Layer, Row, Col.
    function GetRealData(const Layer, Row, Col: integer): double; override;
    // @name checks that Value is rdtDouble.
    procedure SetDataType(const Value: TRbwDataType); override;
    // @name clears FRealValues and calls
    // inherited @Link(TCustomSparseDataSet.SetDimensions).
    procedure SetDimensions(const SetToZero: boolean); override;
    // @name stores a real number at Layer, Row, Col.
    procedure SetRealData(const Layer, Row, Col: integer;
      const Value: double); override;
  public
    procedure RemoveValue(const Layer, Row, Col: Integer);
    procedure GetMinMaxStoredLimits(out LayerMin, RowMin, ColMin,
      LayerMax, RowMax, ColMax: integer); override;
    // @name creates an instance of @classname and sets
    // @link(TDataArray.DataType) to rdtDouble.
    constructor Create(AnOwner: IModelMuseModel); override;
    // @name destroys the the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
    property MinLayer: Integer read GetMinLayer;
    property MaxLayer: Integer read GetMaxLayer;
    property MinRow: Integer read GetMinRow;
    property MaxRow: Integer read GetMaxRow;
    property MinColumn: Integer read GetMinColumn;
    property MaxColumn: Integer read GetMaxColumn;
    procedure UpdateDimensions(NumberOfLayers, NumberOfRows,
      NumberOfColumns: integer; ForceResize: boolean = False); override;
  end;

  TBooleanSparseDataSet = class(TCustomSparseDataSet)
  private
    // @name is used to store the real numbers in @classname.
    FBooleanValues: T3DSparseBooleanArray;
    function GetMaxColumn: Integer;
    function GetMaxLayer: Integer;
    function GetMaxRow: Integer;
    function GetMinColumn: Integer;
    function GetMinLayer: Integer;
    function GetMinRow: Integer;
  protected
    procedure Clear; override;
    // @name checks whether @link(FRealValues) has been
    // assigned at Layer, Row, Col as well as inherited @name.
    function GetIsValue(const Layer, Row, Col: Integer): boolean;
      override;
    // @name retrieves the real number at Layer, Row, Col.
    function GetBooleanData(const Layer, Row, Col: integer): Boolean; override;
    // @name checks that Value is rdtDouble.
    procedure SetDataType(const Value: TRbwDataType); override;
    // @name clears FRealValues and calls
    // inherited @Link(TCustomSparseDataSet.SetDimensions).
    procedure SetDimensions(const SetToZero: boolean); override;
    // @name stores a real number at Layer, Row, Col.
    procedure SetBooleanData(const Layer, Row, Col: integer;
      const Value: Boolean); override;
  public
    procedure RemoveValue(const Layer, Row, Col: Integer);
    procedure GetMinMaxStoredLimits(out LayerMin, RowMin, ColMin,
      LayerMax, RowMax, ColMax: integer); override;
    // @name creates an instance of @classname and sets
    // @link(TDataArray.DataType) to rdtDouble.
    constructor Create(AnOwner: IModelMuseModel); override;
    // @name destroys the the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
    property MinLayer: Integer read GetMinLayer;
    property MaxLayer: Integer read GetMaxLayer;
    property MinRow: Integer read GetMinRow;
    property MaxRow: Integer read GetMaxRow;
    property MinColumn: Integer read GetMinColumn;
    property MaxColumn: Integer read GetMaxColumn;
    procedure UpdateDimensions(NumberOfLayers, NumberOfRows,
      NumberOfColumns: integer; ForceResize: boolean = False); override;
  end;

  TValueAddMethod = (vamAdd, vamReplace, vamAveragedDelayed, vamAddDelayed);

  // @name can have multiple values assigned to the same cell. See
  // @link(ComputeAverage), @link(LabelAsSum), and @link(AddMethod).
  TCustomBoundaryRealSparseDataSet = class(TRealSparseDataSet)
  private
    FCount: T3DSparseIntegerArray;
    FAddMethod: TValueAddMethod;
    FTempValue: double;
    FRestoring: boolean;
    function GetCellCount(Layer, Row, Column: integer): integer;
    procedure SetCellCount(Layer, Row, Column: integer; const Value: integer);
    procedure SetAddMethod(const Value: TValueAddMethod);
  protected
    procedure SetDimensions(const SetToZero: boolean); override;
    procedure SetUpToDate(const Value: boolean); override;
    procedure StoreData(Stream: TStream); override;
    procedure ReadData(DecompressionStream: TDecompressionStream); override;
    procedure SetRealData(const Layer, Row, Col: integer;
      const Value: double); override;
    procedure SetAnnotation(const Layer, Row, Col: integer;
      const Value: string); override;
    function GetRealData(const Layer, Row, Col: integer): double; override;
    function GetIsValue(const Layer, Row, Col: Integer): boolean;
      override;
  public
    procedure Clear; override;
    procedure AddDataArray(DataArray: TDataArray);
    // @name adds a value at the designated location.
    procedure AddDataValue(const DataAnnotation: string; DataValue: Double;
      ColIndex, RowIndex, LayerIndex: Integer);
    constructor Create(AnOwner: IModelMuseModel); override;
    destructor Destroy; override;
    // for each cell, @name computes the average of all the values that have
    // been assigned to the cell when @AddMethod has been assigned a value of
    // vamAdd.
    procedure ComputeAverage;
    // for each cell, @name computes the sum of all the values that have
    // been assigned to the cell when @AddMethod has been assigned a value of
    // vamAdd.
    procedure LabelAsSum;
    property CellCount[Layer, Row, Column: integer]: integer read GetCellCount
      Write SetCellCount;
    procedure UpdateDimensions(NumberOfLayers, NumberOfRows,
      NumberOfColumns: integer; ForceResize: boolean = False); override;
    // @name determines how values are assigned to cells when
    // @link(AddDataValue) is called.
    property AddMethod: TValueAddMethod read FAddMethod write SetAddMethod;
    procedure InitializeDisplayArray(DefaultValue: Double); virtual; abstract;
  end;

  TTransientRealSparseDataSet = class(TRealSparseDataSet)
  protected
    procedure UpdateNotifiers; override;
  end;

  {@abstract(@name is used to store integers in a sparse array.)}
  TIntegerSparseDataSet = class(TCustomSparseDataSet)
  private
    // @name is used to store the integers in @classname.
    FIntegerValues: T3DSparseIntegerArray;
    // @name is used to indicate that this data set is being
    // used to define the type of boundary condition at various
    // locations.
    FIsBoundary: boolean;
  protected
    // @name removes all the data from @classname.
    procedure Clear; override;
    function GetIntegerData(const Layer, Row, Col: integer): integer; override;
    // @name checks whether @link(FIntegerValues) has been
    // assigned at Layer, Row, Col as well as inherited @name.
    function GetIsValue(const Layer, Row, Col: Integer): boolean;
      override;
    // @name checks that Value is rdtInteger.
    procedure SetDataType(const Value: TRbwDataType); override;
    // @name clears @link(FIntegerValues) and calls
    // inherited @Link(TCustomSparseDataSet.SetDimensions).
    procedure SetDimensions(const SetToZero: boolean); override;
    // @name stores an integer at Layer, Row, Col.
    procedure SetIntegerData(const Layer, Row, Col: integer;
      const Value: integer); override;
    // @name if BoundaryTypeDataSet = self then FIntegerValues.IsValue is set
    // to Value. Otherwise inherited IsValue is set.
    procedure SetIsValue(const Layer, Row, Col: Integer;
      const Value: boolean); override;
  public
    procedure RemoveValue(const Layer, Row, Col: Integer);
    // @name creates an instance of @classname and sets
    // @link(TDataArray.DataType) to rdtInteger.
    constructor Create(AnOwner: IModelMuseModel); override;
    // @name destroys the the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
    // @name is true if this data set is used to record the type of
    // boundary condition at a location.
    property IsBoundaryTypeDataSet: boolean read FIsBoundary write FIsBoundary;
    procedure GetMinMaxStoredLimits(out LayerMin, RowMin, ColMin,
      LayerMax, RowMax, ColMax: integer); override;
    procedure UpdateDimensions(NumberOfLayers, NumberOfRows,
      NumberOfColumns: integer; ForceResize: boolean = False); override;
  end;

  {@abstract(@name is used to store integers in a sparse array.)}
  TStringSparseDataSet = class(TCustomSparseDataSet)
  private
    // @name is used to store the integers in @classname.
    FStringValues: T3DSparseStringArray;
    // @name is used to indicate that this data set is being
    // used to define the type of boundary condition at various
    // locations.
    FIsBoundary: boolean;
  protected
    // @name removes all the data from @classname.
    procedure Clear; override;
    function GetStringData(const Layer, Row, Col: integer): string; override;
    // @name checks whether @link(FIntegerValues) has been
    // assigned at Layer, Row, Col as well as inherited @name.
    function GetIsValue(const Layer, Row, Col: Integer): boolean;
      override;
    // @name checks that Value is rdtInteger.
    procedure SetDataType(const Value: TRbwDataType); override;
    // @name clears @link(FIntegerValues) and calls
    // inherited @Link(TCustomSparseDataSet.SetDimensions).
    procedure SetDimensions(const SetToZero: boolean); override;
    // @name stores an integer at Layer, Row, Col.
    procedure SetStringData(const Layer, Row, Col: integer;
      const Value: string); override;
    // @name if BoundaryTypeDataSet = self then FIntegerValues.IsValue is set
    // to Value. Otherwise inherited IsValue is set.
    procedure SetIsValue(const Layer, Row, Col: Integer;
      const Value: boolean); override;
  public
    procedure RemoveValue(const Layer, Row, Col: Integer);
    // @name creates an instance of @classname and sets
    // @link(TDataArray.DataType) to rdtInteger.
    constructor Create(AnOwner: IModelMuseModel); override;
    // @name destroys the the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
    // @name is true if this data set is used to record the type of
    // boundary condition at a location.
    property IsBoundaryTypeDataSet: boolean read FIsBoundary write FIsBoundary;
    procedure GetMinMaxStoredLimits(out LayerMin, RowMin, ColMin,
      LayerMax, RowMax, ColMax: integer); override;
    procedure UpdateDimensions(NumberOfLayers, NumberOfRows,
      NumberOfColumns: integer; ForceResize: boolean = False); override;
  end;


  TTransientIntegerSparseDataSet = class(TIntegerSparseDataSet)
  protected
    procedure UpdateNotifiers; override;
  end;

  TTransientBooleanSparseDataSet = class(TBooleanSparseDataSet)
  protected
    procedure UpdateNotifiers; override;
  end;

  TOnGetUseList = procedure (Sender: TObject;
    NewUseList: TStringList) of object;

  // @name is used to store a series of @link(TDataArray)s. Each is associated
  // with a specific time.
  TCustomTimeList = Class(TObject)
  private
    // @name is either nil or a @link(TCustomModel).
    FModel: TBaseModel;
    // See @link(Direction).
    FDirection: TDataSetOrientation;
    // See @link(Limits).
    FLimits: TColoringLimits;
    // See @link(Name).
    FName: string;
    // See @link(Orientation).
    FOrientation: TDataSetOrientation;
    // See @link(UpToDate).
    FUpToDate: boolean;
    // See @link(Classification).
    FClassification: string;
    // See @link(CheckMax).
    FCheckMax: boolean;
    // See @link(Max).
    FMax: double;
    // See @link(CheckMin).
    FCheckMin: boolean;
    // See @link(Min).
    FMin: double;
    // See @link(DataType).
    FDataType: TRbwDataType;
    // See @link(OnTimeListUsed).
    FOnTimeListUsed: TObjectUsedEvent;
    // See @link(Count).
    function GetCount: integer;
    // See @link(Items).
    function GetItems(const Index: integer): TDataArray;
    // See @link(Times).
    function GetTimes(const Index: integer): double;
    // See @link(Items).
    procedure SetItems(const Index: integer; const Value: TDataArray);
    // See @link(Limits).
    procedure SetLimits(const Value: TColoringLimits);
    // See @link(Name).
    procedure SetName(const Value: string);
    // See @link(Orientation).
    procedure SetOrientation(const Value: TDataSetOrientation);
    // See @link(Classification).
    procedure SetClassification(const Value: string);
    // See @link(Classification).
    function GetClassification: string;
    // See @link(CheckMax).
    procedure SetCheckMax(const Value: boolean);
    // See @link(CheckMin).
    procedure SetCheckMin(const Value: boolean);
    // See @link(Max).
    procedure SetMax(const Value: double);
    // See @link(Min).
    procedure SetMin(const Value: double);
    function GetBoundaryType: TBoundaryType;
  protected
    // @name stores the @link(TDataArray)s.
    // @name is instantiated as a TObjectList.
    FData: TList;
    // See @link(Times).
    FTimes: TRealList;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property Model: TBaseModel read FModel;
    // See @link(UpToDate).
    function GetUpToDate: boolean; virtual;
    // See @link(UpToDate).
    procedure SetUpToDate(const Value: boolean); virtual;
    // @name checks that there is not a conflict between the
    // @link(TDataArray.Model) and @link(Model).
    procedure CheckSameModel(const Data: TDataArray); virtual;
  public
    // @name is used to indicate whether or not a particular @classname
    // is used by the model.
    // See @link(UsedByModel).
    property OnTimeListUsed: TObjectUsedEvent read FOnTimeListUsed write
      FOnTimeListUsed;
    // If @link(OnTimeListUsed) is assigned, @name calls @link(OnTimeListUsed)
    // and returns its result. Otherwise @name returns @true.
    function UsedByModel: boolean;
    // @name adds a @link(TDataArray) and its associated
    // time to the @classname.
    function Add(const ATime: double; const Data: TDataArray): integer;

    // @name removes all the @link(TDataArray)s from
    // the @classname.
    procedure Clear; virtual;

    // @name is the number of @link(TDataArray)s stored in
    // the @classname.
    property Count: integer read GetCount;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name creates an instance of @classname.
    constructor Create(AModel: TBaseModel);
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;

    // @name returns the position in @link(Times) of the first time that
    // is greater than ATime.
    function FirstTimeGreaterThan(const ATime: double): integer;

    // @name returns the position of ATime in @link(Times).
    function IndexOf(const ATime: double): integer;

    // @name returns the position of Data in @link(Items).
    function IndexOfDataSet(const Data: TDataArray): integer;

    // @name assigns values to all the @link(TDataArray)s in @classname.
    procedure Initialize(Times: TRealList = nil); virtual; abstract;
    // @name sets UpToDate to false;
    procedure Invalidate; virtual;

    // @name is the Name of the @classname.
    property Name: string read FName write SetName;

    // @name provides access to the time for each
    // @link(TSparseArrayPhastInterpolationDataSet) in @link(Items).
    property Times[const Index: integer]: double read GetTimes;

    // @name indicates whether all of the
    // @link(TSparseArrayPhastInterpolationDataSet)s are up-to-date.
    property UpToDate: boolean read GetUpToDate;

    { TODO : Document why Direction and Orientation are sometimes different. }
    // @name is the @link(TDataSetOrientation) of the
    // @link(TSparseArrayPhastInterpolationDataSet)s in the @classname.
    property Orientation: TDataSetOrientation read FOrientation write
      SetOrientation;

    // @name is an event handler. One place it is used is
    // TCustomPhastBoundaryCondition.
    // @link(TCustomPhastBoundaryCondition.AddMixtureSubscriptions).
    procedure Changed(Sender: TObject);

    // @name sets the item at Index to nil.
    // Because FData is a TObjectList, this also destroys the item at Index.
    procedure FreeItem(Index: integer);
    // @name provides access to the @link(TDataArray)s stored in
    // @classname.
    property Items[const Index: integer]: TDataArray read GetItems
      write SetItems; default;

    // @name represents the @link(TColoringLimits) to be applied
    // to the data sets in @classname when the grid is colored
    // by one of those data sets.
    property Limits: TColoringLimits read FLimits write SetLimits;

    // @name applies @link(Limits) to each of the
    // @link(TSparseArrayPhastInterpolationDataSet) in @classname.
    procedure UpDateLimits;

    { TODO : Document why Direction and Orientation are sometimes different. }
    // @name specifies the @link(TDataSetOrientation) of the
    // @link(TSparseArrayPhastInterpolationDataSet)s in the @classname.
    property Direction: TDataSetOrientation read FDirection write FDirection;
    {@name is used as as suggestion to a GUI on how to select a particular
    // @classname
    from a hierarchical list of @classname's}
    property Classification: string read GetClassification
      write SetClassification;
    property BoundaryType: TBoundaryType read GetBoundaryType;
    // @name is used to set the @link(TDataArray.Max) property of any
    // @link(TDataArray)s stored in @classname
    property Max: double read FMax write SetMax;
    // @name is used to set the @link(TDataArray.Min) property of any
    // @link(TDataArray)s stored in @classname
    property Min: double read FMin write SetMin;
    // @name is used to set the @link(TDataArray.CheckMax) property of any
    // @link(TDataArray)s stored in @classname
    property CheckMax: boolean read FCheckMax write SetCheckMax;
    // @name is used to set the @link(TDataArray.CheckMin) property of any
    // @link(TDataArray)s stored in @classname
    property CheckMin: boolean read FCheckMin write SetCheckMin;

    // @name specifies the type of data stored in the
    // @link(TDataArray)s stored in @classname.
    property DataType: TRbwDataType read FDataType write FDataType;
    // @name is a string representation of the maximum value
    // of the @link(TDataArray) ast Time.
    function MaxValue(Time: double): string;
    // @name is a string representation of the minimum value
    // of the @link(TDataArray) ast Time.
    function MinValue(Time: double): string;
  End;

procedure GlobalDataArrayRemoveSubscription(Sender: TObject; Subject: TObject;
  const AName: string);

procedure GlobalDataArrayRestoreSubscription(Sender: TObject; Subject: TObject;
  const AName: string);

var
  // @name Stack is used in @link(TDataArray.Initialize) to
  // detect circular references.  @link(TDataArray.HandleCircularReferenceError)
  // is called if one is found.
  Stack: TStringList = nil;

resourcestring
  // @name is used in the classification of data sets.
  StrDataSets = 'Data Sets';
  // @name is used in the classification of data sets.
  StrUserDefined = 'User Defined';
  StrRequiredPart = '|Required|';
  StrOptionalPart = '|Optional|';
var
  // @name is used in the classification of data sets.
  // @name is set in the initialization section and subsequently
  // should be treated as a constant.
  StrRequired: string;
  // @name is used in the classification of data sets.
  // @name is set in the initialization section and subsequently
  // should be treated as a constant.
  StrOptional: string;
  // @name is used in the classification of data sets.
  // @name is set in the initialization section and subsequently
  // should be treated as a constant.
  strDefaultClassification: string;

resourcestring
  StrCircularReferenceI = 'Circular reference in %s';
  StrCircularReferenceI2 = 'Circular reference in %0:s. Circle = %1:s';
  StrCircularReferenceScreen = 'Circular reference in %0:s. Circle = %1:s'
    + sLineBreak + 'Current object = %2:s';
  StrInvalidDataType = 'Invalid data type.';
  StrMODFLOWMultinodeWe = 'MODFLOW Multinode Well';
  StrEvaluatingDat = '      Evaluating data set: "%s."';
  StrCircularReferenceE = 'Circular Reference Error';

const
  MaxSmallArraySize = 1000000;
  StrParamNameSuffix = '_Parameter_Names';

//function GetQuantum(NewSize: Integer): TSPAQuantum;

implementation

uses Contnrs, frmGoPhastUnit, frmConvertChoiceUnit, GIS_Functions,
  ScreenObjectUnit, frmFormulaErrorsUnit, InterpolationUnit,
  PhastModelUnit, AbstractGridUnit, frmErrorsAndWarningsUnit, frmProgressUnit,
  frmDisplayDataUnit, SutraMeshUnit, PhastDataSets,
  ModflowParameterUnit, OrderedCollectionUnit, SfrProcedures,
  {$IFDEF MeasureTime}
  ModelMuseUtilities,
  {$ENDIF}
  DataArrayManagerUnit, DataSetNamesUnit
  ;

resourcestring
  StrUnassigned = 'Unassigned';
  StrThereWasAProblem = 'There was a problem reading the cached values for d' +
  'ata set %s. ModelMuse will attempt to skip the cached values and recalcul' +
  'ate them later.';
  StrSetUsingParameters = 'set using parameters via the formula: %s';
  StrTheNumberOfColumn = 'The number of columns in a data set must be greate' +
  'r than or equal to -1.';
  StrTheNumberOfRowsI = 'The number of rows in a data set must be greater th' +
  'an or equal to -1.';
  StrTheNumberOfLayers = 'The number of layers in a data set must be greater' +
  ' than or equal to -1.';
  StrErrorThisInterpol = 'Error: This interpolator can not return a boolean ' +
  'result.';
  StrErrorThisInterpolVar = 'Error: This interpolator can not return a %s resul' +
  't.';
  StrErrorThisInterpolDataArray = 'Error: This interpolator can not be used ' +
  'with %s because its orientation is wrong.';
  StrErrorThisInterpolInt = 'Error: This interpolator can not return an inte' +
  'ger result.';
  StrErrorThisInterpolFloat = 'Error: This interpolator can not return a rea' +
  'l-number result.';
  StrErrorThisInterpolString = 'Error: This interpolator can not return a st' +
  'ring result.';
  StrPHASTSpecifiedHead = 'PHAST Specified Head';
  StrPHASTFlux = 'PHAST Flux';
  StrPHASTLeaky = 'PHAST Leaky';
  StrPHASTRiver = 'PHAST River';
  StrPHASTWell = 'PHAST Well';
  StrMODFLOWWell = 'MODFLOW Well';
  StrMODFLOWGeneralHead = 'MODFLOW General Head Boundary';
  StrMODFLOWDrain = 'MODFLOW Drain';
  StrMODFLOWDrainReturn = 'MODFLOW Drain Return';
  StrMODFLOWRiver = 'MODFLOW River';
  StrMODFLOWCHD = 'MODFLOW CHD';
  StrMODFLOWFHB = 'MODFLOW Flow and Head Boundary';
  StrMODFLOWEvapoSegments = 'MODFLOW Evapotranspiration Segments';
  StrMODFLOWEvapotranspi = 'MODFLOW Evapotranspiration';
  StrMODFLOWRecharge = 'MODFLOW Recharge';
  StrMODFLOWStreamflowR = 'MODFLOW Streamflow Routing';
  StrMODFLOWStream = 'MODFLOW Stream';
  StrMODFLOWUnsaturated = 'MODFLOW Unsaturated Zone Flow';
  StrMODFLOWObservations = 'MODFLOW Observations';
  StrMt3dmsObservations = 'MT3DMS or MT3D-USGS Observations';
  StrMT3DMSSinkAndSour = 'MT3DMS or MT3D-USGS Sink and Source Mixing';
  StrMODFLOW_Farm = 'MODFLOW Farm Process';
  StrMODFLOW_CFP = 'MODFLOW Conduit Flow Process';
  StrUndefined = 'Undefined';
  DataSetInterpolatorExplanation = 'set via %s.';
  StrSetViaDefaultForm = 'set via default formula: %s';
  StrTheFormulaForThe = 'The formula for the "%0:s" data set is invalid beca' +
  'use the data set or global variable "%1:s" does not exist.';
  StrMODFLOWSWR = 'MODFLOW SWR';
  StrMODFLOWMNW1 = 'MODFLOW MNW1';
  StrMODFLOWRIP = 'MODFLOW RIP';
  StrInvalidTimeSpecifi = 'Invalid time specified. Check that duplicate time' +
  's were not specified.';
  StrMODFLOW6SFR = 'MODFLOW 6 SFR';
  StrMODFLOW6MAW = 'MODFLOW 6 MAW';
  StrMODFLOW6UZF = 'MODFLOW 6 UZF';
  StrThereWasAProblem2 = 'There was a problem reading the cached values for ' +
  'data set %s. ModelMuse will attempt to skip the cached values and recalcu' +
  'late them.';
  StrSFT = 'SFT_';
  StrMODFLOW6CSUB = 'MODFLOW 6 CSUB';
  StrThereWasACircular = 'There was a circular reference error in evaluating' +
  ' %0:s that appears be due to %1:s. %1:s is being converted to a 2D object' +
  '.';
  StrTheDefaultFormula = 'The default formula for %0:s returns a value of th' +
  'e wrong type. The formula is %1:s.';
  StrSParameterNames = '%s' + StrParamNameSuffix;
  StrSMultipliedByAP = '%s multiplied by a parameter value';
  StrMODFLOW6MVR = 'MODFLOW 6 MVR';
  StrNoPESTParameterAs = 'No PEST parameter assigned';
  StrNoPESTParametersA = 'No PEST parameters are assigned to any cell in the' +
  ' data set "%s."';
  StrSHasBeenAssigned = '%s has been assigned a default value because of a c' +
  'ircular reference error.';
  StrErrorAssigningValu = 'Error assigning values to Data set "%0:s" using t' +
  'he interpolation algorithm "%1:s." The error message was %2:s"';
  StrMODFLOW6CNC = 'MODFLOW 6 CNC';
  StrMODFLOW6SRC = 'MODFLOW 6 SRC';
  StrMODFLOWTVK = 'MODFLOW TVK';
  StrMODFLOWTVS = 'MODFLOW TVS';
//  StrMT3DUSGSSFT = 'MT3D-USGS SFT';

//function GetQuantum(NewSize: Integer): TSPAQuantum;
//begin
//  if NewSize > MaxSmallArraySize  then
//  begin
//    result := SPALarge;
//  end
//  else
//  begin
//    result := SPASmall;
//  end;
//end;

{ TDataArray }

procedure TDataArray.ResetFormula(const Compiler: TRbwParser;
  const ErrorMessage: string);
var
  TempFormula: string;
begin
  if ParameterUsed then
  begin
    TempFormula := ParameterFormula;
  end
  else
  begin
    TempFormula := Formula;
    if TempFormula = '' then
    begin
      TempFormula := FFormula;
    end;
  end;
  if (FModel <> nil)
    and not (csDestroying in (FModel as TComponent).ComponentState)
    and not (FModel as TCustomModel).Clearing
    and not ClearingDeletedDataSets then
  begin
    frmFormulaErrors.AddFormulaError('', Name, TempFormula, ErrorMessage);
  end;
  case DataType of
    rdtDouble, rdtInteger:
      begin
        TempFormula := '0';
      end;
    rdtBoolean:
      begin
        TempFormula := 'False';
      end;
    rdtString:
      begin
        TempFormula := '"0"';
      end;
  else
    Assert(False);
  end;
  if ParameterUsed then
  begin
    FParameterFormula := TempFormula;
  end
  else
  begin
    FFormula := TempFormula;
    frmGoPhast.PhastModel.FormulaManager.ChangeFormula(FFormulaObject, FFormula,
      GetCompiler, GlobalDataArrayRemoveSubscription,
      GlobalDataArrayRestoreSubscription, self);
  end;
  Compiler.Compile(TempFormula);
end;

destructor TDataArray.Destroy;
var
  LocalModel: TCustomModel;
begin
  FPestArrayFileNames.Free;
  LocalModel := nil;
  if (FModel <> nil)
    and (not (csDestroying in (FModel as TComponent).ComponentState))
    and not (FModel as TCustomModel).Clearing
    and not ClearingDeletedDataSets then
  begin
    if Assigned(OnDestroy) then
    begin
      OnDestroy(self);
    end;
    case DataType of
      rdtDouble: Formula := '0.';
      rdtInteger: Formula := '0';
      rdtBoolean: Formula := 'False';
      rdtString: Formula := '""';
    end;

    LocalModel := FModel as TCustomModel;
    LocalModel.ModelObserversStopTalkingTo(self);
    if LocalModel.PhastGrid <> nil then
    begin
      if LocalModel.PhastGrid.TopDataSet = self then
      begin
        LocalModel.PhastGrid.TopDataSet := nil
      end;
      if LocalModel.PhastGrid.FrontDataSet = self then
      begin
        LocalModel.PhastGrid.FrontDataSet := nil
      end;
      if LocalModel.PhastGrid.SideDataSet = self then
      begin
        LocalModel.PhastGrid.SideDataSet := nil
      end;
      if LocalModel.PhastGrid.ThreeDDataSet = self then
      begin
        LocalModel.PhastGrid.ThreeDDataSet := nil
      end;

      if LocalModel.PhastGrid.TopContourDataSet = self then
      begin
        LocalModel.PhastGrid.TopContourDataSet := nil
      end;
      if LocalModel.PhastGrid.FrontContourDataSet = self then
      begin
        LocalModel.PhastGrid.FrontContourDataSet := nil
      end;
      if LocalModel.PhastGrid.SideContourDataSet = self then
      begin
        LocalModel.PhastGrid.SideContourDataSet := nil
      end;
      if LocalModel.PhastGrid.ThreeDContourDataSet = self then
      begin
        LocalModel.PhastGrid.ThreeDContourDataSet := nil
      end;
    end;

    if LocalModel.ModflowGrid <> nil then
    begin
      if LocalModel.ModflowGrid.TopDataSet = self then
      begin
        LocalModel.ModflowGrid.TopDataSet := nil
      end;
      if LocalModel.ModflowGrid.FrontDataSet = self then
      begin
        LocalModel.ModflowGrid.FrontDataSet := nil
      end;
      if LocalModel.ModflowGrid.SideDataSet = self then
      begin
        LocalModel.ModflowGrid.SideDataSet := nil
      end;
      if LocalModel.ModflowGrid.ThreeDDataSet = self then
      begin
        LocalModel.ModflowGrid.ThreeDDataSet := nil
      end;

      if LocalModel.ModflowGrid.TopContourDataSet = self then
      begin
        LocalModel.ModflowGrid.TopContourDataSet := nil
      end;
      if LocalModel.ModflowGrid.FrontContourDataSet = self then
      begin
        LocalModel.ModflowGrid.FrontContourDataSet := nil
      end;
      if LocalModel.ModflowGrid.SideContourDataSet = self then
      begin
        LocalModel.ModflowGrid.SideContourDataSet := nil
      end;
      if LocalModel.ModflowGrid.ThreeDContourDataSet = self then
      begin
        LocalModel.ModflowGrid.ThreeDContourDataSet := nil
      end;
    end;

    if LocalModel.Mesh <> nil then
    begin
      if (LocalModel.Mesh as TSutraMesh3D).TopDataSet = self then
      begin
        (LocalModel.Mesh as TSutraMesh3D).TopDataSet := nil
      end;
//      if LocalModel.Mesh.FrontDataSet = self then
//      begin
//        LocalModel.Mesh.FrontDataSet := nil
//      end;
//      if LocalModel.Mesh.SideDataSet = self then
//      begin
//        LocalModel.Mesh.SideDataSet := nil
//      end;
      if (LocalModel.Mesh as TSutraMesh3D).ThreeDDataSet = self then
      begin
        (LocalModel.Mesh as TSutraMesh3D).ThreeDDataSet := nil
      end;

//      if LocalModel.Mesh.TopContourDataSet = self then
//      begin
//        LocalModel.Mesh.TopContourDataSet := nil
//      end;
//      if LocalModel.Mesh.FrontContourDataSet = self then
//      begin
//        LocalModel.Mesh.FrontContourDataSet := nil
//      end;
//      if LocalModel.Mesh.SideContourDataSet = self then
//      begin
//        LocalModel.Mesh.SideContourDataSet := nil
//      end;
//      if LocalModel.Mesh.ThreeDContourDataSet = self then
//      begin
//        LocalModel.Mesh.ThreeDContourDataSet := nil
//      end;
    end;
  end;

  FUsedPestParameters.Free;
  FContourInterval.Free;
  FLimits.Free;
  FContourLimits.Free;
  SetDimensions(True);
  FUseList.Free;
  FTwoDInterpolator.Free;

  if frmGoPhast.PhastModel <> nil then
  begin
    if frmGoPhast.PhastModel.TopGridObserver <> nil then
    begin
      frmGoPhast.PhastModel.TopGridObserver.StopsTalkingTo(self);
      StopsTalkingTo(frmGoPhast.PhastModel.TopGridObserver);
    end;
    if frmGoPhast.PhastModel.ThreeDGridObserver <> nil then
    begin
      frmGoPhast.PhastModel.ThreeDGridObserver.StopsTalkingTo(self);
      StopsTalkingTo(frmGoPhast.PhastModel.ThreeDGridObserver);
    end;
  end;

  if LocalModel <> nil then
  begin
    if LocalModel.TopGridObserver <> nil then
    begin
      LocalModel.TopGridObserver.StopsTalkingTo(self);
      StopsTalkingTo(LocalModel.TopGridObserver);
    end;
    if LocalModel.ThreeDGridObserver <> nil then
    begin
      LocalModel.ThreeDGridObserver.StopsTalkingTo(self);
      StopsTalkingTo(LocalModel.ThreeDGridObserver);
    end;
  end;
  FContours.Free;

  if FileExists(FTempFileName) then
  begin
    DeleteFile(FTempFileName);
  end;

  if (FModel <> nil) and not ClearingDeletedDataSets then
  begin
    (FModel as TCustomModel).FormulaManager.Remove(FFormulaObject,
      GlobalDataArrayRemoveSubscription,
      GlobalDataArrayRestoreSubscription, self);
  end;

  inherited;
end;

function TDataArray.FullClassification: string;
begin
  if Pos(strDefaultClassification, Classification) = 1 then
  begin
    result := Classification;
  end
  else if UsedByModel then
  begin
    result := StrRequired + Classification;
  end
  else
  begin
    result := StrOptional + Classification;
  end;
end;

procedure TDataArray.FullUseList(const AStringList: TStringList);
var
  Index: integer;
  DataSet: TDataArray;
  DataArrayManager: TDataArrayManager;
begin
  Assert(AStringList <> nil);
  AStringList.Clear;
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  for Index := 0 to DataArrayManager.DataSetCount - 1 do
  begin
    DataSet := DataArrayManager.DataSets[Index];
    DataSet.Observed := False;
  end;

  ObserverList.NotifyOnChange(self, ckCheckDependance);
  for Index := 0 to DataArrayManager.DataSetCount - 1 do
  begin
    DataSet := DataArrayManager.DataSets[Index];
    if DataSet.Observed then
    begin
      AStringList.Add(DataSet.Name)
    end;
  end;
end;

function TDataArray.GetUseList: TStringList;
begin
  // FUseListUpToDate is set to False in Invalidate
  // and to True in UpdateUseList.
  if not FUseListUpToDate then
  begin
    UpdateUseList;
  end;
  result := FUseList;
end;

function TDataArray.IdenticalDataArrayContents(ADataArray: TDataArray): boolean;
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  LocalIsValue: Boolean;
begin
  ADataArray.Initialize;
  Result := (DataType = ADataArray.DataType)
    and (Orientation = ADataArray.Orientation)
    and (EvaluatedAt = ADataArray.EvaluatedAt);
  if result then
  begin
    for LayerIndex := 0 to LayerCount - 1 do
    begin
      for RowIndex := 0 to RowCount - 1 do
      begin
        for ColIndex := 0 to ColumnCount - 1 do
        begin
          LocalIsValue := IsValue[LayerIndex,RowIndex,ColIndex];
          result := LocalIsValue
            = ADataArray.IsValue[LayerIndex,RowIndex,ColIndex];
          if not result then
          begin
            Exit;
          end;
          if LocalIsValue then
          begin
            case DataType of
              rdtDouble:
                begin
                  result := RealData[LayerIndex,RowIndex,ColIndex]
                    = ADataArray.RealData[LayerIndex,RowIndex,ColIndex];
                end;
              rdtInteger: 
                begin
                  result := IntegerData[LayerIndex,RowIndex,ColIndex]
                    = ADataArray.IntegerData[LayerIndex,RowIndex,ColIndex];
                end;
              rdtBoolean: 
                begin
                  result := BooleanData[LayerIndex,RowIndex,ColIndex]
                    = ADataArray.BooleanData[LayerIndex,RowIndex,ColIndex];
                end;
              rdtString: 
                begin
                  result := StringData[LayerIndex,RowIndex,ColIndex]
                    = ADataArray.StringData[LayerIndex,RowIndex,ColIndex];
                end;
            end;
            if not result then
            begin
              Exit;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TDataArray.UpdateNotifiers;
var
  LocalModel: TCustomModel;
begin
  case FOrientation of
    dsoTop:
      begin
        LocalModel := FModel as TCustomModel;
        LocalModel.ThreeDGridObserver.StopsTalkingTo(self);
        LocalModel.TopGridObserver.TalksTo(self);
      end;
    dsoFront, dsoSide, dso3D:
      begin
        LocalModel := FModel as TCustomModel;
        LocalModel.ThreeDGridObserver.TalksTo(self);
        LocalModel.TopGridObserver.StopsTalkingTo(self);
      end;
  else
    Assert(False);
  end;
end;

procedure TDataArray.UpdateSubscriptions(NewUseList: TStringList; OldUseList: TStringList);
var
  ObservedItem: TObserver;
  OtherIndex: Integer;
  Index: Integer;
  Model: TCustomModel;
begin
  Model := FModel as TCustomModel;
  for Index := OldUseList.Count - 1 downto 0 do
  begin
    OtherIndex := NewUseList.IndexOf(OldUseList[Index]);
    if OtherIndex >= 0 then
    begin
      OldUseList.Delete(Index);
//      NewUseList.Delete(OtherIndex);
    end;
  end;
  for Index := 0 to OldUseList.Count - 1 do
  begin
    ObservedItem := Model.GetObserverByName(OldUseList[Index]);
    if ObservedItem <> nil then
    begin
      // DS may be nil when the model is being destroyed.
      ObservedItem.StopsTalkingTo(self);
    end;
  end;
  for Index := 0 to NewUseList.Count - 1 do
  begin
    ObservedItem := Model.GetObserverByName(NewUseList[Index]);
    if ObservedItem = nil then
    begin
      frmFormulaErrors.AddFormulaError('', Name, Formula,
        Format(StrTheFormulaForThe, [Name, NewUseList[Index]]));
    end
    else
    begin
      if not Model.Clearing then
      begin
        ObservedItem.TalksTo(self);
      end;
    end;
  end;
end;

resourcestring
  ErrorMessageInterpUnNamed = 'An invalid value assigned by an interpolator '
    + 'has been replaced by the maximum single-precision real number in an unamed data set.';
  ErrorMessageInterpNamed = 'An invalid value assigned by an interpolator '
    + 'has been replaced by the maximum single-precision  real number in Data Set: %s.';
  ErrorMessageFormulaUnNamed = 'An invalid value assigned by a formula '
    + 'has been replaced by the maximum single-precision  real number in an unamed data set.';
  ErrorMessageFormulaNamed = 'An invalid value assigned by a formula '
    + 'has been replaced by the maximum single-precision  real number in Data Set: %s.';
  ErrorString ='Layer: %d; Row: %d; Column: %d.';
  ErrorMessageFormulaUnNamedInt = 'An invalid value assigned by a formula '
    + 'has been replaced by the maximum integer number in an unamed data set.';
  ErrorMessageFormulaNamedInt = 'An invalid value assigned by a formula '
    + 'has been replaced by the maximum integer number in Data Set: %s.';
  ErrorMessageFormulaUnNamedBool = 'An invalid value assigned by a formula '
    + 'has been replaced by "False" in an unamed data set.';
  ErrorMessageFormulaNamedBool = 'An invalid value assigned by a formula '
    + 'has been replaced by "False" in Data Set: %s.';
  ErrorMessageFormulaUnNamedString = 'An invalid value assigned by a formula '
    + 'has been replaced by "" in an unamed data set.';
  ErrorMessageFormulaNamedString = 'An invalid value assigned by a formula '
    + 'has been replaced by "" in Data Set: %s.';

procedure TDataArray.HandleCircularReferenceError(ErrorMessage: string; ScreenObject: TObject);
var
  LIndex: Integer;
  RIndex: Integer;
  CIndex: Integer;
begin
  ErrorMessage := Format(StrSHasBeenAssigned + sLineBreak, [Name]) + ErrorMessage;
  for LIndex := 0 to LayerCount - 1 do
  begin
    for RIndex := 0 to RowCount - 1 do
    begin
      for CIndex := 0 to ColumnCount - 1 do
      begin
        case DataType of
          rdtDouble:
            begin
              RealData[LIndex, RIndex, CIndex] := 0.0;
            end;
          rdtInteger:
            begin
              IntegerData[LIndex, RIndex, CIndex] := 0;
            end;
          rdtBoolean:
            begin
              BooleanData[LIndex, RIndex, CIndex] := False;
            end;
          rdtString:
            begin
              StringData[LIndex, RIndex, CIndex] := 'Error';
            end;
        end;
        Annotation[LIndex, RIndex, CIndex] := ErrorMessage;
      end;
    end;
  end;
  FCleared := False;
  UpToDate := True;
  frmErrorsAndWarnings.AddError(Model as TCustomModel, StrCircularReferenceE, ErrorMessage, ScreenObject);
  Beep;
  MessageDlg(ErrorMessage, mtError, [mbOK], 0);
end;

procedure TDataArray.Initialize;
const
  MaxReal = 3.4E38;
var
  ColIndex, RowIndex, LayerIndex: integer;
  Compiler: TRbwParser;
  Expression: TExpression;
  ResultTypeOK: boolean;
  TempFormula: string;
  VarIndex: integer;
  VarName: string;
  VarPosition: integer;
  Variable: TCustomValue;
  DataSetIndex: integer;
  AnotherDataSet: TDataArray;
  LayerToUse, RowToUse, ColToUse: integer;
  TempUseList: TStringList;
  CellCenter, CellCorner: TPoint2D;
  CellCenter3D, CellCorner3D: TPoint3D;
  LayerLimit, RowLimit, ColLimit: integer;
  FreeStack: boolean;
  AnnotationString: string;
  VariablePositions: array of integer;
  DataSetIndexes: array of integer;
  InterpAnnString: string;
  AValue: double;
  HideProgressForm: Boolean;
  DataArrayManager: TDataArrayManager;
  LocalModel: TCustomModel;
  ResultOK: Boolean;
  PriorInterpAnnString: string;
  DisLimits: TGridLimit;
  ErrorMessage: string;
  NameDataArray: TDataArray;
  OkAssignment: Boolean;
  StackIndex: Integer;
  AnObject: TObject;
  AnotherObject: TObject;
  {$IFDEF MeasureTime}
  StartTime: TDateTime;
  ElapsedTime: double;
  {$ENDIF}
  procedure GetLimits;
  begin
    if LocalModel.ModelSelection in SutraSelection then
    begin
      LayerLimit := LayerCount - 1;
      RowLimit := RowCount - 1;
      ColLimit := ColumnCount - 1;
    end
    else
    begin
      case EvaluatedAt of
        eaBlocks:
          begin
            LayerLimit := LayerCount - 1;
            RowLimit := RowCount - 1;
            ColLimit := ColumnCount - 1;
          end;
        eaNodes:
          begin
            case Orientation of
              dsoTop:
                begin
                  LayerLimit := LayerCount - 1;
                  RowLimit := RowCount;
                  ColLimit := ColumnCount;
                end;
              dsoFront:
                begin
                  LayerLimit := LayerCount;
                  RowLimit := RowCount - 1;
                  ColLimit := ColumnCount;
                end;
              dsoSide:
                begin
                  LayerLimit := LayerCount;
                  RowLimit := RowCount;
                  ColLimit := ColumnCount - 1;
                end;
              dso3D:
                begin
                  LayerLimit := LayerCount;
                  RowLimit := RowCount;
                  ColLimit := ColumnCount;
                end;
            else
              Assert(False);
            end;
          end;
      else
        Assert(False);
      end;
    end;
  end;
  procedure HandleError;
  begin
    ResultOK := False;
    case DataType of
      rdtDouble:
        begin
          if Name = '' then
          begin
            frmErrorsAndWarnings.AddError(Model as TCustomModel,
              ErrorMessageFormulaUnNamed, Format(ErrorString,
              [LayerIndex+1,RowIndex+1,ColIndex+1] ));
          end
          else
          begin
            frmErrorsAndWarnings.AddError(Model as TCustomModel,
              Format(ErrorMessageFormulaNamed, [Name]),
              Format(ErrorString,
              [LayerIndex+1,RowIndex+1,ColIndex+1]));
          end;
          AValue := MaxReal;
        end;
      rdtInteger:
        begin
          if Name = '' then
          begin
            frmErrorsAndWarnings.AddError(Model as TCustomModel,
              ErrorMessageFormulaUnNamedInt, Format(ErrorString,
              [LayerIndex+1,RowIndex+1,ColIndex+1] ));
          end
          else
          begin
            frmErrorsAndWarnings.AddError(Model as TCustomModel,
              Format(ErrorMessageFormulaNamedInt, [Name]),
              Format(ErrorString,
              [LayerIndex+1,RowIndex+1,ColIndex+1]));
          end;
          IntegerData[LayerIndex, RowIndex, ColIndex] := MaxInt;
        end;
      rdtBoolean:
        begin
          if Name = '' then
          begin
            frmErrorsAndWarnings.AddError(Model as TCustomModel,
              ErrorMessageFormulaUnNamedBool, Format(ErrorString,
              [LayerIndex+1,RowIndex+1,ColIndex+1] ));
          end
          else
          begin
            frmErrorsAndWarnings.AddError(Model as TCustomModel,
              Format(ErrorMessageFormulaNamedBool, [Name]),
              Format(ErrorString,
              [LayerIndex+1,RowIndex+1,ColIndex+1]));
          end;
          BooleanData[LayerIndex, RowIndex, ColIndex] := False;
        end;
      rdtString:
        begin
          if Name = '' then
          begin
            frmErrorsAndWarnings.AddError(Model as TCustomModel,
              ErrorMessageFormulaUnNamedString, Format(ErrorString,
              [LayerIndex+1,RowIndex+1,ColIndex+1] ));
          end
          else
          begin
            frmErrorsAndWarnings.AddError(Model as TCustomModel,
              Format(ErrorMessageFormulaNamedString, [Name]),
              Format(ErrorString,
              [LayerIndex+1,RowIndex+1,ColIndex+1]));
          end;
          StringData[0, RowIndex, ColIndex] := '';
        end;
    else
      Assert(False);
    end;
  end;
  procedure UpdateInterpAnnString;
  begin
    InterpAnnString := Format(DataSetInterpolatorExplanation,
            [TwoDInterpolator.LastAnnotation]);
    if InterpAnnString = PriorInterpAnnString then
    begin
      // update reference count of PriorInterpAnnString
      // and use PriorInterpAnnString
      InterpAnnString := PriorInterpAnnString;
    end
    else
    begin
      // replace PriorInterpAnnString
      PriorInterpAnnString := InterpAnnString;
    end;
  end;
begin
  if FUpdatingProgress then
  begin
    Exit;
  end;

//  OutputDebugString('SAMPLING ON');
  if UpToDate and not DimensionsChanged then
  begin
    try
      CheckRestoreData;
      Exit;
    except on ERangeError do
      begin
        FDataCached := False;
        FCleared := False;
        FIsUniform := iuFalse;
        UpToDate := False;
        FReadDataFromFile := False;
        Beep;
        MessageDlg(Format(StrThereWasAProblem2, [Name]), mtError, [mbOK], 0);
      end;
    end;
  end;

  {$IFDEF MeasureTime}
  StartTime := Now;
  {$ENDIF}

  LocalModel := FModel as TCustomModel;
  DataArrayManager := LocalModel.DataArrayManager;
  HideProgressForm := False;
  if (Name <> '') and (frmProgressMM <> nil) then
  begin
    FUpdatingProgress := True;
    try
      HideProgressForm := not frmProgressMM.Visible;
      if HideProgressForm then
      begin
        frmProgressMM.Caption := '';
        frmProgressMM.Show;
      end;
      frmProgressMM.AddMessage(Format(StrEvaluatingDat, [Name]), False);
    finally
      FUpdatingProgress := False;
    end;
  end;

  FDataCached := False;

  FreeStack := (Stack = nil);
  try
    InterpAnnString := '';
    try
      if FreeStack then
      begin
        Stack := TStringList.Create;
      end;
      if Stack.IndexOf(Name) >= 0 then
      begin
        Stack.Add(Name);
        UpToDate := True;
        if CurrentObject = nil then
        begin
          Stack[0] := Stack[0] + ' depends on';
          for StackIndex := 1 to Stack.Count - 2 do
          begin
            Stack[StackIndex] := Stack[StackIndex] + ' which depends on';
          end;
          HandleCircularReferenceError(Format(StrCircularReferenceI2, [Name, Stack.Text]), nil);
          Exit;
        end
        else
        begin
          if CurrentObject.ElevationCount <> ecZero then
          begin
            Beep;
            CurrentObject.ElevationCount := ecZero;
            ErrorMessage := Format(StrThereWasACircular, [Name, CurrentObject.Name]);
            frmErrorsAndWarnings.AddError(Model as TCustomModel, StrCircularReferenceE, ErrorMessage, CurrentObject);
            MessageDlg(ErrorMessage, mtError, [mbOK], 0);
            Exit;
          end
          else
          begin
            Stack[0] := Stack[0] + ' depends on';
            for StackIndex := 1 to Stack.Count - 2 do
            begin
              Stack[StackIndex] := Stack[StackIndex] + ' which depends on';
            end;
            HandleCircularReferenceError(Format(StrCircularReferenceScreen,
              [Name, Stack.Text, CurrentObject.Name]), CurrentObject);
            Exit;
          end;
        end;
      end;
      Stack.Add(Name);

      GlobalEvaluatedAt := EvaluatedAt;

      if DimensionsChanged then
      begin
        SetDimensions(False);
      end
      else
      begin
        RestoreArraySize;
      end;

      if PestParametersUsed then
      begin
        NameDataArray := LocalModel.DataArrayManager.
          GetDataSetByName(ParamDataSetName);
        NameDataArray.Initialize;
      end;

      GetLimits;
      if (LayerLimit >= 0) and (RowLimit >= 0) and (ColLimit >= 0) then
      begin
        if ShouldUseOnInitialize then
        begin
          OnInitialize(Self);
        end
        else
        begin
          repeat
            OkAssignment := True;
            UpdateCurrentScreenObject(nil);
            if (Orientation <> dso3D) and (TwoDInterpolator <> nil)
              and TwoDInterpolator.ShouldInterpolate then
            begin
              try
                DisLimits := (Model as TCustomModel).
                  DiscretizationLimits(OrientationToViewDirection(Orientation));
                TwoDInterpolator.EpsilonX := (DisLimits.MaxX - DisLimits.MinX)/1E8;
                TwoDInterpolator.EpsilonY := (DisLimits.MaxY - DisLimits.MinY)/1E8;
      //          FWaitingOnInterpolator := True;
                try
                  TwoDInterpolator.Initialize(self);
                  while not TwoDInterpolator.Ready do
                  begin

                  end;
                Except
                  on E: ESfrProcedureException do
                  begin
                    OkAssignment := False;
                    Exception.RaiseOuterException(EInterpolationException.Create(Format(
                      StrErrorAssigningValu,
                      [Name, TwoDInterpolator.InterpolatorName, E.Message])));
                  end;
                  on E: ERbwParserError do
                  begin
                    OkAssignment := False;
                    Exception.RaiseOuterException(EInterpolationException.Create(Format(
                      StrErrorAssigningValu,
                      [Name, TwoDInterpolator.InterpolatorName, E.Message])));
                  end;
                  on E: EPlProcException do
                  begin
                    OkAssignment := False;
                    Exception.RaiseOuterException(EInterpolationException.Create(Format(
                      StrErrorAssigningValu,
                      [Name, TwoDInterpolator.InterpolatorName, E.Message])));
                  end;
                end;

                if InterpAnnString = '' then
                begin
                  InterpAnnString := Format(DataSetInterpolatorExplanation,
                    [TwoDInterpolator.InterpolatorName]);
                  PriorInterpAnnString := InterpAnnString;
                end;
                case Orientation of
                  dsoTop:
                    begin
                      case EvaluatedAt of
                        eaBlocks:
                          begin
                            for RowIndex := 0 to RowLimit do
                            begin
                              for ColIndex := 0 to ColLimit do
                              begin
                                CellCenter :=
                                  LocalModel.TwoDElementCenter(ColIndex,
                                  RowIndex);

                                UpdateGlobalLocations(ColIndex,
                                  RowIndex, 0, EvaluatedAt, FModel as TCustomModel);

                                case Datatype of
                                  rdtDouble:
                                    begin
                                      AValue := TwoDInterpolator.RealResult(CellCenter);
                                      UpdateInterpAnnString;
                                      if IsInfinite(AValue) or IsNan(AValue) then
                                      begin
                                        if Name = '' then
                                        begin
                                          frmErrorsAndWarnings.AddError(Model as TCustomModel,
                                            ErrorMessageInterpUnNamed, Format(ErrorString,
                                            [1,RowIndex+1,ColIndex+1] ));
                                        end
                                        else
                                        begin
                                          frmErrorsAndWarnings.AddError(Model as TCustomModel,
                                            Format(ErrorMessageInterpNamed, [Name]),
                                            Format(ErrorString,
                                            [1,RowIndex+1,ColIndex+1]));
                                        end;
                                        RealData[0, RowIndex, ColIndex] := MaxReal
                                      end
                                      else
                                      begin
                                        RealData[0, RowIndex, ColIndex] := AValue;
                                      end;
                                    end;
                                  rdtInteger:
                                    begin
                                      IntegerData[0, RowIndex, ColIndex] :=
                                        TwoDInterpolator.IntegerResult(CellCenter);
                                      UpdateInterpAnnString;
                                    end;
                                  rdtBoolean:
                                    begin
                                      BooleanData[0, RowIndex, ColIndex] :=
                                        TwoDInterpolator.BooleanResult(CellCenter);
                                      UpdateInterpAnnString;
                                    end;
                                  rdtString:
                                    begin
                                      StringData[0, RowIndex, ColIndex] :=
                                        TwoDInterpolator.StringResult(CellCenter);
                                      UpdateInterpAnnString;
                                    end;
                                else
                                  Assert(False);
                                end;
                                Annotation[0, RowIndex, ColIndex] := InterpAnnString;
                              end
                            end;
                          end;
                        eaNodes:
                          begin
                            for RowIndex := 0 to RowLimit do
                            begin
                              for ColIndex := 0 to ColLimit do
                              begin
                                CellCorner :=
                                  LocalModel.TwoDElementCorner(ColIndex,
                                  RowIndex);
                                UpdateGlobalLocations(ColIndex,
                                  RowIndex, 0, EvaluatedAt, FModel as TCustomModel);

                                case Datatype of
                                  rdtDouble:
                                    begin
        //                              try
                                      AValue := TwoDInterpolator.RealResult(CellCorner);
        //                              except
        //                                Beep;
        //                                raise;
        //                              end;
                                      UpdateInterpAnnString;
                                      if IsInfinite(AValue) or IsNan(AValue) then
                                      begin
                                        if Name = '' then
                                        begin
                                          frmErrorsAndWarnings.AddError(Model as TCustomModel,
                                            ErrorMessageInterpUnNamed,
                                            Format(ErrorString, [1,RowIndex+1,ColIndex+1] ));
                                        end
                                        else
                                        begin
                                          frmErrorsAndWarnings.AddError(Model as TCustomModel,
                                            Format(ErrorMessageInterpNamed, [Name]),
                                            Format(ErrorString,
                                            [1,RowIndex+1,ColIndex+1]));
                                        end;
                                        RealData[0, RowIndex, ColIndex] := MaxReal
                                      end
                                      else
                                      begin
                                        RealData[0, RowIndex, ColIndex] := AValue;
                                      end;
                                    end;
                                  rdtInteger:
                                    begin
                                      IntegerData[0, RowIndex, ColIndex] :=
                                        TwoDInterpolator.IntegerResult(CellCorner);
                                      UpdateInterpAnnString;
                                    end;
                                  rdtBoolean:
                                    begin
                                      BooleanData[0, RowIndex, ColIndex] :=
                                        TwoDInterpolator.BooleanResult(CellCorner);
                                      UpdateInterpAnnString;
                                    end;
                                  rdtString:
                                    begin
                                      StringData[0, RowIndex, ColIndex] :=
                                        TwoDInterpolator.StringResult(CellCorner);
                                      UpdateInterpAnnString;
                                    end;
                                else
                                  Assert(False);
                                end;
                                Annotation[0, RowIndex, ColIndex] := InterpAnnString
                              end
                            end;
                          end;
                      else
                        begin
                          Assert(False);
                        end;
                      end;
                    end;
                  dsoFront:
                    begin
                      case EvaluatedAt of
                        eaBlocks:
                          begin
                            for LayerIndex := 0 to LayerLimit do
                            begin
                              for ColIndex := 0 to ColLimit do
                              begin
                                CellCenter3D := LocalModel.Grid.ThreeDElementCenter(
                                  ColIndex, 0, LayerIndex);

                                UpdateGlobalLocations(ColIndex, 0,
                                  LayerIndex, EvaluatedAt, FModel as TCustomModel);

                                CellCenter.X := CellCenter3D.X;
                                CellCenter.Y := CellCenter3D.Z;

                                case Datatype of
                                  rdtDouble:
                                    begin
                                      AValue := TwoDInterpolator.RealResult(CellCenter);
                                      UpdateInterpAnnString;
                                      if IsInfinite(AValue) or IsNan(AValue) then
                                      begin
                                        if Name = '' then
                                        begin
                                          frmErrorsAndWarnings.AddError(Model as TCustomModel,
                                            ErrorMessageInterpUnNamed, Format(ErrorString,
                                            [LayerIndex+1,1,ColIndex+1] ));
                                        end
                                        else
                                        begin
                                          frmErrorsAndWarnings.AddError(Model as TCustomModel,
                                            Format(ErrorMessageInterpNamed, [Name]),
                                            Format(ErrorString,
                                            [LayerIndex+1,1,ColIndex+1]));
                                        end;
                                        RealData[LayerIndex, 0, ColIndex] := MaxReal
                                      end
                                      else
                                      begin
                                        RealData[LayerIndex, 0, ColIndex] := AValue;
                                      end;
                                    end;
                                  rdtInteger:
                                    begin
                                      IntegerData[LayerIndex, 0, ColIndex] :=
                                        TwoDInterpolator.IntegerResult(CellCenter);
                                      UpdateInterpAnnString;
                                    end;
                                  rdtBoolean:
                                    begin
                                      BooleanData[LayerIndex, 0, ColIndex] :=
                                        TwoDInterpolator.BooleanResult(CellCenter);
                                      UpdateInterpAnnString;
                                    end;
                                  rdtString:
                                    begin
                                      StringData[LayerIndex, 0, ColIndex] :=
                                        TwoDInterpolator.StringResult(CellCenter);
                                      UpdateInterpAnnString;
                                    end;
                                else
                                  Assert(False);
                                end;
                                Annotation[LayerIndex, 0, ColIndex] := InterpAnnString;
                              end;
                            end;
                          end;
                        eaNodes:
                          begin
                            for LayerIndex := 0 to LayerLimit do
                            begin
                              for ColIndex := 0 to ColLimit do
                              begin
                                CellCorner3D := LocalModel.Grid.ThreeDElementCorner(
                                  ColIndex, 0, LayerIndex);

                                UpdateGlobalLocations(ColIndex, 0,
                                  LayerIndex, EvaluatedAt, FModel as TCustomModel);

                                CellCorner.X := CellCorner3D.X;
                                CellCorner.Y := CellCorner3D.Z;

                                case Datatype of
                                  rdtDouble:
                                    begin
                                      AValue := TwoDInterpolator.RealResult(CellCorner);
                                      UpdateInterpAnnString;
                                      if IsInfinite(AValue) or IsNan(AValue) then
                                      begin
                                        if Name = '' then
                                        begin
                                          frmErrorsAndWarnings.AddError(Model as TCustomModel,
                                            ErrorMessageInterpUnNamed, Format(ErrorString,
                                            [LayerIndex+1,1,ColIndex+1] ));
                                        end
                                        else
                                        begin
                                          frmErrorsAndWarnings.AddError(Model as TCustomModel,
                                            Format(ErrorMessageInterpNamed, [Name]),
                                            Format(ErrorString,
                                            [LayerIndex+1,1,ColIndex+1]));
                                        end;
                                        RealData[LayerIndex, 0, ColIndex] := MaxReal
                                      end
                                      else
                                      begin
                                        RealData[LayerIndex, 0, ColIndex] := AValue;
                                      end;
                                    end;
                                  rdtInteger:
                                    begin
                                      IntegerData[LayerIndex, 0, ColIndex] :=
                                        TwoDInterpolator.IntegerResult(CellCorner);
                                      UpdateInterpAnnString;
                                    end;
                                  rdtBoolean:
                                    begin
                                      BooleanData[LayerIndex, 0, ColIndex] :=
                                        TwoDInterpolator.BooleanResult(CellCorner);
                                      UpdateInterpAnnString;
                                    end;
                                  rdtString:
                                    begin
                                      StringData[LayerIndex, 0, ColIndex] :=
                                        TwoDInterpolator.StringResult(CellCorner);
                                      UpdateInterpAnnString;
                                    end;
                                else
                                  Assert(False);
                                end;
                                Annotation[LayerIndex, 0, ColIndex] := InterpAnnString;
                              end;
                            end;
                          end;
                      else
                        begin
                          Assert(False);
                        end;

                      end;
                    end;
                  dsoSide:
                    begin
                      case EvaluatedAt of
                        eaBlocks:
                          begin
                            for LayerIndex := 0 to LayerLimit do
                            begin
                              for RowIndex := 0 to RowLimit do
                              begin
                                CellCenter3D := LocalModel.Grid.ThreeDElementCenter(
                                  0, RowIndex, LayerIndex);

                                UpdateGlobalLocations(0,
                                  RowIndex, LayerIndex, EvaluatedAt, FModel as TCustomModel);

                                CellCenter.X := CellCenter3D.Y;
                                CellCenter.Y := CellCenter3D.Z;

                                case Datatype of
                                  rdtDouble:
                                    begin
                                      AValue := TwoDInterpolator.RealResult(CellCenter);
                                      UpdateInterpAnnString;
                                      if IsInfinite(AValue) or IsNan(AValue) then
                                      begin
                                        if Name = '' then
                                        begin
                                          frmErrorsAndWarnings.AddError(Model as TCustomModel,
                                            ErrorMessageInterpUnNamed, Format(ErrorString,
                                            [LayerIndex+1,RowIndex+1,1] ));
                                        end
                                        else
                                        begin
                                          frmErrorsAndWarnings.AddError(Model as TCustomModel,
                                            Format(ErrorMessageInterpNamed, [Name]),
                                            Format(ErrorString,
                                            [LayerIndex+1,RowIndex+1,1]));
                                        end;
                                        RealData[LayerIndex, RowIndex, 0] := MaxReal
                                      end
                                      else
                                      begin
                                        RealData[LayerIndex, RowIndex, 0] := AValue;
                                      end;
                                    end;
                                  rdtInteger:
                                    begin
                                      IntegerData[LayerIndex, RowIndex, 0] :=
                                        TwoDInterpolator.IntegerResult(CellCenter);
                                      UpdateInterpAnnString;
                                    end;
                                  rdtBoolean:
                                    begin
                                      BooleanData[LayerIndex, RowIndex, 0] :=
                                        TwoDInterpolator.BooleanResult(CellCenter);
                                      UpdateInterpAnnString;
                                    end;
                                  rdtString:
                                    begin
                                      StringData[LayerIndex, RowIndex, 0] :=
                                        TwoDInterpolator.StringResult(CellCenter);
                                      UpdateInterpAnnString;
                                    end;
                                else
                                  Assert(False);
                                end;
                                Annotation[LayerIndex, RowIndex, 0] := InterpAnnString;
                              end;
                            end;
                          end;
                        eaNodes:
                          begin
                            for LayerIndex := 0 to LayerLimit do
                            begin
                              for RowIndex := 0 to RowLimit do
                              begin
                                CellCorner3D :=
                                  LocalModel.Grid.ThreeDElementCorner(
                                  0, RowIndex, LayerIndex);

                                UpdateGlobalLocations(0,
                                  RowIndex, LayerIndex, EvaluatedAt, FModel as TCustomModel);

                                CellCorner.X := CellCorner3D.Y;
                                CellCorner.Y := CellCorner3D.Z;

                                case Datatype of
                                  rdtDouble:
                                    begin
                                      AValue := TwoDInterpolator.RealResult(CellCorner);
                                      UpdateInterpAnnString;
                                      if IsInfinite(AValue) or IsNan(AValue) then
                                      begin
                                        if Name = '' then
                                        begin
                                          frmErrorsAndWarnings.AddError(Model as TCustomModel,
                                            ErrorMessageInterpUnNamed, Format(ErrorString,
                                            [LayerIndex+1,RowIndex+1,1] ));
                                        end
                                        else
                                        begin
                                          frmErrorsAndWarnings.AddError(Model as TCustomModel,
                                            Format(ErrorMessageInterpNamed, [Name]),
                                            Format(ErrorString,
                                            [LayerIndex+1,RowIndex+1,1]));
                                        end;
                                        RealData[LayerIndex, RowIndex, 0] := MaxReal
                                      end
                                      else
                                      begin
                                        RealData[LayerIndex, RowIndex, 0] := AValue;
                                      end;
                                    end;
                                  rdtInteger:
                                    begin
                                      IntegerData[LayerIndex, RowIndex, 0] :=
                                        TwoDInterpolator.IntegerResult(CellCorner);
                                      UpdateInterpAnnString;
                                    end;
                                  rdtBoolean:
                                    begin
                                      BooleanData[LayerIndex, RowIndex, 0] :=
                                        TwoDInterpolator.BooleanResult(CellCorner);
                                      UpdateInterpAnnString;
                                    end;
                                  rdtString:
                                    begin
                                      StringData[LayerIndex, RowIndex, 0] :=
                                        TwoDInterpolator.StringResult(CellCorner);
                                      UpdateInterpAnnString;
                                    end;
                                else
                                  Assert(False);
                                end;
                                Annotation[LayerIndex, RowIndex, 0] := InterpAnnString;
                              end;
                            end;
                          end;
                      else
                        begin
                          Assert(False);
                        end;
                      end;
                    end;
                else
                  Assert(False);
                end;
                TwoDInterpolator.Finalize(self);
                break;
              except on E: EInterpolationException do
                begin
                  TwoDInterpolator := nil;
                  frmFormulaErrors.AddFormulaError('', Name, '', E.message);
                end;
              end;
            end
            else
            begin
              TempUseList := TStringList.Create;
              try
                TempUseList.Duplicates := dupIgnore;
                TempUseList.Sorted := True;
                TempUseList.Capacity := UseList.Count;
                TempUseList.Assign(UseList);

                for VarIndex := 0 to TempUseList.Count - 1 do
                begin
                  VarName := TempUseList[VarIndex];
                  AnotherDataSet := DataArrayManager.GetDataSetByName(VarName);
                  if AnotherDataSet <> nil then
                  begin
                    Assert(AnotherDataSet <> self);
                    AnotherDataSet.Initialize;
                    DataArrayManager.AddDataSetToCache(AnotherDataSet);
                  end;
                end;
                GlobalEvaluatedAt := EvaluatedAt;

                Compiler := GetCompiler;
                if ParameterUsed then
                begin
                  TempFormula := ParameterFormula;
                end
                else
                begin
                  TempFormula := Formula;
                end;

                try
                  Compiler.Compile(TempFormula);
                except on E: ERbwParserError do
                  begin
                    ResetFormula(Compiler, E.Message);
                  end;
                end;

                if ParameterUsed then
                begin
                  TempFormula := ParameterFormula;
                end
                else
                begin
                  TempFormula := Formula;
                end;
                Compiler.Compile(TempFormula);
                Expression := Compiler.CurrentExpression;
                ResultTypeOK := (Expression.ResultType = Datatype)
                  or ((Expression.ResultType = rdtInteger) and (Datatype = rdtDouble));
                if not ResultTypeOK then
                begin
                  ResetFormula(Compiler, StrInvalidDataType);
                  if ParameterUsed then
                  begin
                    TempFormula := ParameterFormula;
                  end
                  else
                  begin
                    TempFormula := Formula;
                  end;
                  Compiler.Compile(TempFormula);
                  Expression := Compiler.CurrentExpression;
                  ResultTypeOK := (Expression.ResultType = Datatype)
                    or ((Expression.ResultType = rdtInteger) and (Datatype = rdtDouble));
                  if not ResultTypeOK then
                  begin
  //                  Beep;
  //                  MessageDlg(Format(StrTheDefaultFormula, [Name, Expression.Decompile]), mtError, [mbOK], 0);
                    raise EInvalidDataType.Create(Format(StrTheDefaultFormula, [Name, Expression.Decompile]), Expression.Decompile);
                  end;
                end;

                if ParameterUsed then
                begin
                  AnnotationString := Format(StrSetUsingParameters, [TempFormula]);
                end
                else
                begin
                  AnnotationString := Format(
                    StrSetViaDefaultForm, [TempFormula]);
                end;

                SetLength(VariablePositions, TempUseList.Count);
                SetLength(DataSetIndexes, TempUseList.Count);
                for VarIndex := 0 to TempUseList.Count - 1 do
                begin
                  VarName := TempUseList[VarIndex];
                  VarPosition := Compiler.IndexOfVariable(VarName);
                  VariablePositions[VarIndex] := VarPosition;
                  if VarPosition >= 0 then
                  begin
                    DataSetIndex := DataArrayManager.IndexOfDataSet(VarName);
                    DataSetIndexes[VarIndex] := DataSetIndex;
                  end
                  else
                  begin
                    DataSetIndexes[VarIndex] := -1;
                  end;
                end;

                for LayerIndex := 0 to LayerLimit do
                begin
                  for RowIndex := 0 to RowLimit do
                  begin
                    for ColIndex := 0 to ColLimit do
                    begin

                      UpdateGlobalLocations(ColIndex, RowIndex, LayerIndex,
                        EvaluatedAt, FModel as TCustomModel);

                      for VarIndex := 0 to TempUseList.Count - 1 do
                      begin
                        VarName := TempUseList[VarIndex];
                        VarPosition := VariablePositions[VarIndex];
                        if VarPosition >= 0 then
                        begin
                          Variable := Compiler.Variables[VarPosition];
                          DataSetIndex := DataSetIndexes[VarIndex];
                          if DataSetIndex >= 0 then
                          begin
                            AnotherDataSet :=
                              DataArrayManager.DataSets[DataSetIndex];
                            Assert(AnotherDataSet <> self);

                            // Newly created data sets may have different values
                            // of the Model interface from ones created when
                            // the model was first created even though they
                            // represent the same TCustomModel. For that reason
                            // the following doesn't work
                            // Assert(Model = AnotherDataSet.Model);

                            // This ensures that the data sets all below to the
                            // same model without testing for equality of
                            // Model and AnotherDataSet.Model.
                            Model.QueryInterface(IInterface, AnObject);
                            AnotherDataSet.Model.QueryInterface(IInterface, AnotherObject);
                            Assert(AnObject = AnotherObject);


                            Assert(AnotherDataSet.DataType = Variable.ResultType);
                            if AnotherDataSet.Orientation = dsoTop then
                            begin
                              LayerToUse := 0;
                            end
                            else
                            begin
                              LayerToUse := LayerIndex;
                            end;
                            if AnotherDataSet.Orientation = dsoFront then
                            begin
                              RowToUse := 0;
                            end
                            else
                            begin
                              RowToUse := RowIndex;
                            end;
                            if AnotherDataSet.Orientation = dsoSide then
                            begin
                              ColToUse := 0;
                            end
                            else
                            begin
                              ColToUse := ColIndex;
                            end;

                            case Variable.ResultType of
                              rdtDouble:
                                begin
                                  TRealVariable(Variable).Value :=
                                    AnotherDataSet.RealData[LayerToUse, RowToUse,
                                    ColToUse];
                                end;
                              rdtInteger:
                                begin
                                  TIntegerVariable(Variable).Value :=
                                    AnotherDataSet.IntegerData[LayerToUse, RowToUse,
                                    ColToUse];
                                end;
                              rdtBoolean:
                                begin
                                  TBooleanVariable(Variable).Value :=
                                    AnotherDataSet.BooleanData[LayerToUse, RowToUse,
                                    ColToUse];
                                end;
                              rdtString:
                                begin
                                  TStringVariable(Variable).Value :=
                                    AnotherDataSet.StringData[LayerToUse, RowToUse,
                                    ColToUse];
                                end;
                            else
                              Assert(False);
                            end;
                          end;
                        end;
                      end;
                      ResultOK := True;
                      try
                        Expression.Evaluate;
                      except
                        on E: ERbwParserError do
                        begin
                          ResetFormula(Compiler, E.Message);
                          TempFormula := Formula;
                          Compiler.Compile(TempFormula);
                          Expression := Compiler.CurrentExpression;
                          Expression.Evaluate;
                        end;
                        on E: EInvalidOp do
                        begin
                          HandleError;
                        end;
                        on E: EDivByZero do
                        begin
                          HandleError;
                        end;
                        on E: EZeroDivide do
                        begin
                          HandleError;
                        end;
                      end;

                      case Datatype of
                        rdtDouble:
                          begin
                            if ResultOK then
                            begin
                              AValue := Expression.DoubleResult;
                            end;
                            if IsInfinite(AValue) or IsNan(AValue) then
                            begin
                              if Name = '' then
                              begin
                                frmErrorsAndWarnings.AddError(Model as TCustomModel,
                                  ErrorMessageFormulaUnNamed, Format(ErrorString,
                                  [LayerIndex+1,RowIndex+1,ColIndex+1] ));
                              end
                              else
                              begin
                                frmErrorsAndWarnings.AddError(Model as TCustomModel,
                                  Format(ErrorMessageFormulaNamed, [Name]),
                                  Format(ErrorString,
                                  [LayerIndex+1,RowIndex+1,ColIndex+1]));
                              end;
                              RealData[LayerIndex, RowIndex, ColIndex] := MaxReal
                            end
                            else
                            begin
                              RealData[LayerIndex, RowIndex, ColIndex] := AValue;
                            end;
                          end;
                        rdtInteger:
                          begin
                            if ResultOK then
                            begin
                              IntegerData[LayerIndex, RowIndex, ColIndex]
                                := Expression.IntegerResult;
                            end;
                          end;
                        rdtBoolean:
                          begin
                            if ResultOK then
                            begin
                              BooleanData[LayerIndex, RowIndex, ColIndex]
                                := Expression.BooleanResult;
                            end;
                          end;
                        rdtString:
                          begin
                            if ResultOK then
                            begin
                              StringData[LayerIndex, RowIndex, ColIndex]
                                := Expression.StringResult;
                            end;
                          end;
                      else
                        Assert(False);
                      end;
                      Annotation[LayerIndex, RowIndex, ColIndex] := AnnotationString;
                    end;
                  end;
                end;
              finally
                TempUseList.Free;
              end;
              break;
            end;
          until OkAssignment;

          AssignValuesWithScreenObjects;
        end;
      end;
    finally
      if FreeStack then
      begin
        FreeAndNil(Stack);
        DataArrayManager.DontCache(self);
        DataArrayManager.CacheDataArrays;
    //      OutputDebugString('SAMPLING OFF');
      end;
      if HideProgressForm then
      begin
        frmProgressMM.Hide;
      end;
    end;
    PostInitialize;
  except on E: EInvalidDataType do
    begin
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;

  FCleared := False;
  UpToDate := True;
//  FWaitingOnInterpolator := False;
  if FreeStack then
  begin
    if (frmDisplayData <> nil) and frmDisplayData.ShouldUpdate then
    begin
      frmDisplayData.UpdateLabelsAndLegend;
    end;

    if frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.Show;
    end;
  end;
  CheckIfUniform;

  {$IFDEF MeasureTime}
  ElapsedTime := Now;
  ElapsedTime := ElapsedTime - StartTime;
  ElapsedTime := ElapsedTime * 24 * 3600;
  ElapsedTime := RoundSigDigits(ElapsedTime, 3);
//  ElapsedTime := Round(ElapsedTime*1000)/1000;
  ShowMessage(ElapsedTime.ToString);
  {$ENDIF}

end;

procedure TDataArray.Invalidate;
begin
  UpToDate := False;
end;

//function TDataArray.IsLayerUsedWithParameters(const ALayer: Integer): Boolean;
//var
//  Splitter: TStringList;
//  UsedLayers: array of Boolean;
//  LineIndex: Integer;
//  ALine: string;
//  LayerNumber: Integer;
//  LayerIndex: Integer;
//  String1: string;
//  RangePosition: Integer;
////  LayerIndex: Integer;
//  String2: string;
//  StartLayer: Integer;
//  EndLayer: Integer;
//begin
//  if (ALayer < 0) or (ALayer >= LayerCount) then
//  begin
//    result := False;
//    Exit;
//  end;
//
//  if ParametersUsed then
//  begin
//    if CompareText(ParameterLayersUsed, 'All') = 0 then
//    begin
//      result := True;
//    end
//    else
//    begin
//      SetLength(UsedLayers, LayerCount);
//      for LayerIndex := 0 to LayerCount - 1 do
//      begin
//        UsedLayers[LayerIndex] := False;
//      end;
//      Splitter := TStringList.Create;
//      try
//        Splitter.CommaText := ParameterLayersUsed;
//        for LineIndex := 0 to Splitter.Count - 1 do
//        begin
//          ALine := Splitter[LineIndex];
//          if TryStrToInt(ALine, LayerNumber) then
//          begin
//            UsedLayers[LayerNumber-1] := True;
//          end
//          else
//          begin
//            RangePosition := Pos('..', ALine);
//            if RangePosition >= 2 then
//            begin
//              String1 := Trim(Copy(ALine, 1, RangePosition-1));
//              String2 := Trim(Copy(ALine, RangePosition + 2, MAXINT));
//              if TryStrToInt(String1, StartLayer)
//                and TryStrToInt(String2, EndLayer)
//                and (StartLayer <= LayerCount) then
//              begin
//                Dec(StartLayer);
//                Dec(EndLayer);
//                StartLayer := Math.Max(0, StartLayer);
//                EndLayer := Math.Min(EndLayer, LayerCount-1);
//                for LayerIndex := StartLayer to EndLayer do
//                begin
//                  UsedLayers[LayerIndex] := True;
//                end;
//              end;
//            end;
//          end;
//        end;
//      finally
//        Splitter.Free;
//      end;
//      result := UsedLayers[ALayer];
//    end;
//  end
//  else
//  begin
//    result := False;
//  end;
//end;

function TDataArray.IsSparseArray: boolean;
begin
  result := false;
end;

procedure TDataArray.SetDataType(const Value: TRbwDataType);
var
  NumberOfLayers, NumberOfRows, NumberOfColumns: integer;
  Index: integer;
  AScreenObject: TScreenObject;
  Position: integer;
  formula: string;
begin
  if FDataType <> Value then
  begin
    // Adjust the formulas of all the screen objects that
    // set the value of this data set directly.
    // The formulas of screen objects that set the value
    // of this data set indirectly are not changed.

    if (frmGoPhast.PhastModel <> nil) and (Formula <> '') then
    begin
      for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
      begin
        AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
        Position := AScreenObject.IndexOfDataSet(self);
        if Position >= 0 then
        begin
          formula := AScreenObject.DataSetFormulas[Position];
          formula := AdjustFormula(formula, FDataType, Value);
          AScreenObject.DataSetFormulas[Position] := Formula;
        end;
      end;
      frmGoPhast.InvalidateModel;
    end;

    // Store the current dimensions.
    NumberOfLayers := FLayerCount;
    NumberOfRows := FRowCount;
    NumberOfColumns := FColumnCount;
    // deallocate array
    SetDimensions(True);
    FDataType := Value;
    // reallocate array with new data type.
    UpdateDimensions(NumberOfLayers, NumberOfRows, NumberOfColumns);
    UpToDate := False;
  end;
end;

procedure TDataArray.GetRequiredDimensions(out NumberOfLayers, NumberOfRows,
  NumberOfColumns: integer);
var
  LocalModel: TCustomModel;
begin
  NumberOfLayers := FLayerCount;
  NumberOfRows := FRowCount;
  NumberOfColumns := FColumnCount;
  case EvaluatedAt of
    eaBlocks:
      begin
        // do nothing
      end;
    eaNodes:
      begin
        LocalModel := Model as TCustomModel;
        if LocalModel.ModelSelection = msPhast then
        begin
          case Orientation of
            dsoTop:
              begin
                Inc(NumberOfRows);
                Inc(NumberOfColumns);
              end;
            dsoFront:
              begin
                Inc(NumberOfLayers);
                Inc(NumberOfColumns);
              end;
            dsoSide:
              begin
                Inc(NumberOfLayers);
                Inc(NumberOfRows);
              end;
            dso3D:
              begin
                Inc(NumberOfLayers);
                Inc(NumberOfRows);
                Inc(NumberOfColumns);
              end;
          else
            Assert(False);
          end;
        end
//        else if LocalModel.ModelSelection = msSutra then
//        begin
//          NumberOfLayers := LocalModel.SutraLayerStructure.NodeLayerCount;
//          NumberOfRows := 1;
//          NumberOfColumns := LocalModel.SutraMesh.Mesh2D.Nodes.Count;
//          FLayerCount := NumberOfLayers;
//          FRowCount := NumberOfRows;
//          FColumnCount := NumberOfColumns;
//        end;
      end;
  else
    begin
      Assert(False);
    end;
  end;
end;

function TDataArray.GetRowCount: integer;
begin
  if (IsUniform = iuTrue) or FCleared then
  begin
    result := FCachedRowCount;
  end
  else
  begin
    result := FRowCount;
  end;
end;

procedure TDataArray.SetDimensions(const SetToZero: boolean);
var
  NumberOfLayers: integer;
  NumberOfRows: integer;
  NumberOfColumns: integer;
begin
  if SetToZero then
  begin
    NumberOfLayers := 0;
    NumberOfRows := 0;
    NumberOfColumns := 0;
    FLayerCount := NumberOfLayers;
    FRowCount := NumberOfRows;
    FColumnCount := NumberOfColumns;
  end
  else
  begin
    GetRequiredDimensions(NumberOfLayers, NumberOfRows, NumberOfColumns);
  end;

  NumberOfLayers := Math.Max(NumberOfLayers, 0);
  NumberOfRows := Math.Max(NumberOfRows, 0);
  NumberOfColumns := Math.Max(NumberOfColumns, 0);

  if not IsSparseArray then
  begin
    case FDataType of
      rdtDouble:
        begin
          SetLength(T3DRealDataSet(FDataArray), NumberOfLayers, NumberOfRows,
            NumberOfColumns);
        end;
      rdtInteger:
        begin
          SetLength(T3DIntegerDataSet(FDataArray), NumberOfLayers, NumberOfRows,
            NumberOfColumns);
        end;
      rdtBoolean:
        begin
          SetLength(T3DBooleanDataSet(FDataArray), NumberOfLayers, NumberOfRows,
            NumberOfColumns);
        end;
      rdtString:
        begin
          SetLength(T3DStringDataSet(FDataArray), NumberOfLayers, NumberOfRows,
            NumberOfColumns);
        end;
    else
      Assert(False);
    end;
    SetLength(FAnnotation, NumberOfLayers, NumberOfRows, NumberOfColumns);
  end;
  FDimensionsChanged := False;
end;

procedure TDataArray.SetDisplayName(const Value: string);
begin
  if FDisplayName <> Value then
  begin
    FDisplayName := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TDataArray.SetFormula(const Value: string);
var
  P: Pointer;
  ChildIndex: Integer;
  LocalModel: TPhastModel;
  ChildItem: TChildModelItem;
  ChildDataArray: TDataArray;
begin
  FFormula := Formula;
  ChangeAFormula(Value, FFormula, FUseListUpToDate, GetUseList);
  P := Addr(GlobalDataArrayRemoveSubscription);
  if Assigned(P) then
  begin
    TCustomModel(FModel).FormulaManager.ChangeFormula(FFormulaObject,
      FFormula, GetCompiler, GlobalDataArrayRemoveSubscription,
      GlobalDataArrayRestoreSubscription, self);
  end;
  if (Name <> '') and (FModel <> nil) and (FModel is TPhastModel) then
  begin
    LocalModel := TPhastModel(FModel);
    for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
    begin
      ChildItem := LocalModel.ChildModels[ChildIndex];
      if ChildItem.ChildModel <> nil then
      begin
        ChildDataArray := ChildItem.ChildModel.DataArrayManager.
          GetDataSetByName(Name);
        if ChildDataArray <> nil then
        begin
          ChildDataArray.Formula := Formula;
        end;
      end;
    end;
  end;
end;

procedure TDataArray.SetOnShouldUseOnInitialize(const Value: TCheckUsageEvent);
begin
  FOnShouldUseOnInitialize := Value;
end;

procedure TDataArray.SetOrientation(const Value: TDataSetOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    FDimensionsChanged := True;
    UpToDate := False;
    UpdateNotifiers;
    frmGoPhast.InvalidateModel;
  end;
  FFormulaObject.Parser := GetCompiler;
end;

procedure TDataArray.SetParameterFormula(const Value: string);
begin
  ChangeAFormula(Value, FParameterFormula, FUseListUpToDate, GetUseList)
end;

procedure TDataArray.SetPestParametersAllowed(const Value: Boolean);
begin
  FPestParametersAllowed := Value;
end;

procedure TDataArray.SetPestParametersUsed(const Value: Boolean);
begin
  if FPestParametersUsed <> Value then
  begin
    FPestParametersUsed := Value;
    frmGoPhast.InvalidateModel;
    CreatePestParmNameDataSet;
  end;
end;

procedure TDataArray.SetPilotPointsUsed(const Value: Boolean);
begin
  FPilotPointsUsed := Value;
end;

procedure TDataArray.SetParameterUsed(const Value: boolean);
begin
  if FParameterUsed <> Value then
  begin
    FParameterUsed := Value;
    FUseListUpToDate := False;
    UpToDate := False;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TDataArray.SetContours(const Value: TContours);
begin
  if (FContours = nil) and (Value <> nil) then
  begin
    FContours := TContours.Create;
  end;
  if Value = nil then
  begin
    FreeAndNil(FContours);
  end
  else
  begin
    FContours.Assign(Value);
//    if frmContourData = nil then
//    begin
//      frmContourData := TfrmContourData.Create(nil);
//    end;
//    frmContourData.UpdateLabelsAndLegend;
    if frmDisplayData = nil then
    begin
      Application.CreateForm(TfrmDisplayData, frmDisplayData);
    end;
    frmDisplayData.UpdateLabelsAndLegend;
  end;
end;

procedure TDataArray.SetTemplateNeeded(const Value: Boolean);
begin
  FTemplateNeeded := Value;
end;

procedure TDataArray.SetTwoDInterpolator(const Value: TCustom2DInterpolater);
begin
  if Value = nil then
  begin
    if FTwoDInterpolator <> nil then
    begin
      FTwoDInterpolator.Free;
      FTwoDInterpolator := nil;
      UpToDate := False;
      frmGoPhast.InvalidateModel;
    end;
  end
  else
  begin
    if (FTwoDInterpolator <> nil) and FTwoDInterpolator.SameAs(Value) then
    begin
      Exit;
    end;
    FTwoDInterpolator.Free;
    try
      FTwoDInterpolator := TInterpolatorType(Value.ClassType).Create(self);
      FTwoDInterpolator.SetSubComponent(True);
      FTwoDInterpolator.Assign(Value);
    except on E: EInterpolationException do
      begin
        FTwoDInterpolator := nil;
      end;
    end;
    UpToDate := False;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TDataArray.SetUnicodeSaved(const Value: Boolean);
begin
  if FUnicodeSaved <> Value then
  begin
    FUnicodeSaved := Value;
    if FDataCached then
    begin
      Invalidate;
//      Clear;
//      FCleared := True;
//      Invalidate;
    end;
  end;
end;

procedure TDataArray.SetUnits(const Value: string);
begin
  if FUnits <> Value then
  begin
    FUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TDataArray.SetUpToDate(const Value: boolean);
var
  Updated: boolean;
  LocalModel: TCustomModel;
begin
  if (frmGoPhast.PhastModel <> nil)
    and (csDestroying in frmGoPhast.PhastModel.ComponentState) then
  begin
    inherited;
    Exit;
  end;

  FIsUniform := iuUnknown;
  frmGoPhast.InvalidateModel;
  RefreshUseList;
  FUseListUpToDate := False;
  if not Value then
  begin
    FMinMaxUpToDate := False;
  end;

  Updated := Value and not UpToDate;
  inherited;
  // doing this causes too much thrashing when evaluating
  // transient data.
  // ScreenObjectsChanged is called every time a TDataArray
  // is created.
{  if not Value then
  begin
    (FPhastModel as TPhastModel).ScreenObjectsChanged(self);
  end; }
  if Updated then
  begin
//    UpdateMinMaxValues;
  end;
  LocalModel := FModel as TCustomModel;
  if (LocalModel <> nil) then
  begin
    if Updated and ((LocalModel.TopContourDataSet = self)
      or (LocalModel.FrontContourDataSet = self)
      or (LocalModel.SideContourDataSet = self)) then
    begin
      LocalModel.InvalidateContours;
	    frmGoPhast.InvalidateAllViews;
    end;

    if (LocalModel.Grid <> nil) then
    begin
      if ((LocalModel.Grid.TopDataSet <> nil)
        and (LocalModel.Grid.TopDataSet = self)
        and not LocalModel.Grid.TopDataSet.UpToDate)
        or ((LocalModel.Grid.TopContourDataSet <> nil)
        and (LocalModel.Grid.TopContourDataSet = self)
        and not LocalModel.Grid.TopContourDataSet.UpToDate) then
      begin
        LocalModel.Grid.NeedToRecalculateTopCellColors := True;
        frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
      end;
      if ((LocalModel.Grid.FrontDataSet <> nil)
        and (LocalModel.Grid.FrontDataSet = self)
        and not LocalModel.Grid.FrontDataSet.UpToDate)
        or ((LocalModel.Grid.FrontContourDataSet <> nil)
        and (LocalModel.Grid.FrontContourDataSet = self)
        and not LocalModel.Grid.FrontContourDataSet.UpToDate) then
      begin
        LocalModel.Grid.NeedToRecalculateFrontCellColors := True;
        frmGoPhast.frameFrontView.ZoomBox.InvalidateImage32;
      end;
      if ((LocalModel.Grid.SideDataSet <> nil)
        and (LocalModel.Grid.SideDataSet = self)
        and not LocalModel.Grid.SideDataSet.UpToDate)
        or ((LocalModel.Grid.SideContourDataSet <> nil)
        and (LocalModel.Grid.SideContourDataSet = self)
        and not LocalModel.Grid.SideContourDataSet.UpToDate) then
      begin
        LocalModel.Grid.NeedToRecalculateSideCellColors := True;
        frmGoPhast.frameSideView.ZoomBox.InvalidateImage32;
      end;
      if ((LocalModel.ThreeDDataSet <> nil)
        and (LocalModel.Grid.ThreeDDataSet = self)
        and not LocalModel.ThreeDDataSet.UpToDate)
        or ((LocalModel.Grid.ThreeDContourDataSet <> nil)
        and (LocalModel.Grid.ThreeDContourDataSet = self)
        and not LocalModel.Grid.ThreeDContourDataSet.UpToDate) then
      begin
        LocalModel.Grid.NeedToRecalculate3DCellColors := True;
        LocalModel.DiscretizationChanged;
      end;
    end
    else if LocalModel.Mesh3D <> nil then
    begin
      if ((LocalModel.Mesh3D.TopDataSet <> nil)
        and not LocalModel.Mesh3D.TopDataSet.UpToDate)
        {or ((LocalModel.Mesh3D.TopContourDataSet <> nil)
        and not LocalModel.Mesh3D.TopContourDataSet.UpToDate)} then
      begin
        LocalModel.Mesh3D.NeedToRecalculateTopColors := True;
        frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
      end;
      {if ((LocalModel.Mesh3D.FrontDataSet <> nil)
        and not LocalModel.Mesh3D.FrontDataSet.UpToDate)
        or ((LocalModel.Mesh3D.FrontContourDataSet <> nil)
        and not LocalModel.Mesh3D.FrontContourDataSet.UpToDate) then
      begin
        LocalModel.Mesh3D.NeedToRecalculateFrontCellColors := True;
        frmGoPhast.frameFrontView.ZoomBox.InvalidateImage32;
      end; }
      {if ((LocalModel.Mesh3D.SideDataSet <> nil)
        and not LocalModel.Mesh3D.SideDataSet.UpToDate)
        or ((LocalModel.Mesh3D.SideContourDataSet <> nil)
        and not LocalModel.Mesh3D.SideContourDataSet.UpToDate) then
      begin
//        LocalModel.Mesh3D.NeedToRecalculateSideCellColors := True;
        frmGoPhast.frameSideView.ZoomBox.InvalidateImage32;
      end;       }
      if ((LocalModel.ThreeDDataSet <> nil)
        and not LocalModel.ThreeDDataSet.UpToDate)
        {or ((LocalModel.Mesh3D.ThreeDContourDataSet <> nil)
        and not LocalModel.Mesh3D.ThreeDContourDataSet.UpToDate)} then
      begin
        LocalModel.Mesh3D.NeedToRecalculateFrontColors := True;
        LocalModel.DiscretizationChanged;
      end;

    end;
  end;
  
  if Updated and (frmDisplayData <> nil) and (Stack = nil) then
  begin
    if (LocalModel.ThreeDDataSet = self) or (LocalModel.TopDataSet = self)
	  or (LocalModel.FrontDataSet = self) or (LocalModel.SideDataSet = self)
	  or (LocalModel.ThreeDContourDataSet = self) or (LocalModel.TopContourDataSet = self)
	  or (LocalModel.FrontContourDataSet = self) or (LocalModel.SideContourDataSet = self) then
    begin
      frmDisplayData.UpdateLabelsAndLegend;
    end;
  end;  
end;

procedure TDataArray.SetUsedPestParameters(const Value: TStrings);
begin
  FUsedPestParameters.Assign(Value);
end;

procedure TDataArray.UpdateDimensions(NumberOfLayers, NumberOfRows,
  NumberOfColumns: integer; ForceResize: boolean = False);
begin
  case Orientation of
    dsoTop:
      begin
        NumberOfLayers := 1;
      end;
    dsoFront:
      begin
        NumberOfRows := 1;
      end;
    dsoSide:
      begin
        NumberOfColumns := 1;
      end;
    dso3D:
      begin
      end;
  else
    Assert(False);
  end;

  if NumberOfColumns < -1 then
  begin
    raise EInvalidGridOrMesh.Create(StrTheNumberOfColumn);
  end;
  if NumberOfRows < -1 then
  begin
    raise EInvalidGridOrMesh.Create(StrTheNumberOfRowsI);
  end;
  if NumberOfLayers < -1 then
  begin
    raise EInvalidGridOrMesh.Create(StrTheNumberOfLayers);
  end;
  if (FLayerCount <> NumberOfLayers)
    or (FRowCount <> NumberOfRows)
    or (FColumnCount <> NumberOfColumns)
    or ForceResize then
  begin
    if (FDataCached and FCleared) or (UpToDate and (IsUniform = iuTrue))
      and (FCachedLayerCount = NumberOfLayers)
      and (FCachedRowCount = NumberOfRows)
      and (FCachedColumnCount = NumberOfColumns) then
    begin
      // do nothing.
    end
    else
    begin
      FDimensionsChanged := True;
    end;
    FLayerCount := NumberOfLayers;
    FCachedLayerCount := NumberOfLayers;
    FRowCount := NumberOfRows;
    FCachedRowCount := NumberOfRows;
    FColumnCount := NumberOfColumns;
    FCachedColumnCount := NumberOfColumns;
    if ForceResize then
    begin
      SetDimensions(False);
    end;
  end;
end;

procedure TDataArray.UpdateUseList;
var
  Compiler: TRbwParser;
  TempFormula: string;
begin
  Compiler := GetCompiler;
  if ParameterUsed then
  begin
    TempFormula := ParameterFormula;
  end
  else
  begin
    TempFormula := FFormula;
  end;
  if TempFormula = '' then
  begin
    TempFormula := '0';
  end;
  try
    Compiler.Compile(TempFormula);
    FUseList.Assign(Compiler.CurrentExpression.VariablesUsed);
  except on E: ERbwParserError do
    begin
      if FRefreshingOldUseList then
      begin
        raise;
      end
      else
      begin
        ResetFormula(Compiler, E.Message);
        FUseList.Clear;
        Exit;
      end;
    end;
  end;
  FUseListUpToDate := True;
end;

procedure TDataArray.UpdateWithName(const AName: string);
var
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildItem: TChildModelItem;
  ChildDataArray: TDataArray;
begin
  if Name <> AName then
  begin
    DisplayName := AName;
  end;
  inherited;
  if (FModel <> nil) and (FModel is TPhastModel) then
  begin
    LocalModel := TPhastModel(FModel);
    for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
    begin
      ChildItem := LocalModel.ChildModels[ChildIndex];
      if ChildItem.ChildModel <> nil then
      begin
        ChildDataArray := ChildItem.ChildModel.DataArrayManager.
          GetDataSetByName(Name);
        if ChildDataArray <> nil then
        begin
          ChildDataArray.UpdateWithName(AName);
        end;
      end;
    end;
  end;
end;

procedure TDataArray.UpdateWithoutNotification(
  NewOrientation: TDataSetOrientation; NewEvaluatedAt: TEvaluatedAt;
  NewDataType: TRbwDataType; var NeedToInvalidate: boolean);
begin
  NeedToInvalidate := NeedToInvalidate
    or (FOrientation<> NewOrientation)
    or (FEvaluatedAt <> NewEvaluatedAt)
    or (FDataType <> NewDataType);
  FOrientation := NewOrientation;
  FEvaluatedAt := NewEvaluatedAt;
  FDataType := NewDataType;
end;

constructor TDataArray.Create(AnOwner: IModelMuseModel);
var
  LocalPhastModel : TPhastModel;
  LocalChildModel: TChildModel;
  LocalModel: TCustomModel;
begin
  FPestParametersAllowed := True;
  FVisible := True;
  FUseLgrEdgeCells := lctUse;
  Assert(AnOwner <> nil);
  FModel := AnOwner;
  LocalModel := FModel as TCustomModel;
  if LocalModel is TPhastModel then
  begin
    LocalPhastModel := TPhastModel(LocalModel);
  end
  else
  begin
    LocalChildModel := LocalModel as TChildModel;
    LocalPhastModel := LocalChildModel.ParentModel as TPhastModel;
  end;
  FUnicodeSaved := not LocalPhastModel.FileVersionEqualOrEarlier('2.9.1.2');
  inherited Create(nil);
  FFormulaObject := LocalModel.FormulaManager.Add;
  FFormulaObject.AddSubscriptionEvents(GlobalDataArrayRemoveSubscription,
      GlobalDataArrayRestoreSubscription, self);
  FLimits := TColoringLimits.Create;
  FLimits.OnChange := LimitsChanged;
  FContourLimits := TColoringLimits.Create;
  FContourLimits.OnChange := ContourLimitsChanged;
//  FContourLimits.LowerLimit.OnChange := ContourLimitsChanged;
  FUseList := TStringList.Create;
  FUseList.Sorted := True;
  FUseList.Duplicates := dupAccept;
  UpToDate := False;
  FVisible := True;
  FUseListUpToDate := False;
  FReadDataFromFile := False;
  FContourInterval := TRealStorage.Create;
  FContourInterval.OnChange := OnValueChanged;
  FUsedPestParameters := TStringList.Create;
  TStringList(FUsedPestParameters).Duplicates := dupIgnore;
  TStringList(FUsedPestParameters).Sorted := True;
end;

procedure TDataArray.CreatePestParmNameDataSet;
var
  DataSetName: string;
  LocalModel: TCustomModel;
  NameDataArray: TDataArray;
begin
  DataSetName := ParamDataSetName;
  LocalModel := FModel as TCustomModel;
  NameDataArray := LocalModel.DataArrayManager.
    GetDataSetByName(DataSetName);
  if PestParametersUsed then
  begin
    if NameDataArray = nil then
    begin
      if csLoading in LocalModel.ComponentState then
      begin
        Exit;
      end;
      NameDataArray := LocalModel.DataArrayManager.CreateNewDataArray(
        TDataArray, DataSetName, '""',
        DataSetName,
        Lock, rdtString, EvaluatedAt,
        Orientation, Classification);
      NameDataArray.OnDataSetUsed := LocalModel.ParamNamesDataSetUsed;
      NameDataArray.Lock := Lock - [dcFormula];
      NameDataArray.CheckMax := False;
      NameDataArray.CheckMin := False;
      NameDataArray.DisplayName := DataSetName;
      NameDataArray.Visible := True;
      NameDataArray.OnInitialize := nil;
      NameDataArray.OnShouldUseOnInitialize := nil;
      NameDataArray.SetDimensions(False);
    end
    else
    begin
      NameDataArray.OnDataSetUsed := LocalModel.ParamNamesDataSetUsed;
      NameDataArray.Classification := Classification;
    end;
    NameDataArray.TalksTo(self);
  end
  else
  begin
    if NameDataArray <> nil then
    begin
      NameDataArray.StopsTalkingTo(self);
    end;
  end;
end;

procedure TDataArray.OnValueChanged(Sender: TObject);
begin
  frmGoPhast.InvalidateModel;
end;

procedure TDataArray.SetEvaluatedAt(const Value: TEvaluatedAt);
begin
  if FEvaluatedAt <> Value then
  begin
    FEvaluatedAt := Value;
    UpToDate := False;
    FDimensionsChanged := True;
    frmGoPhast.InvalidateModel;
  end;
  FFormulaObject.Parser := GetCompiler;
end;

procedure TDataArray.SetLock(const Value: TDataLock);
begin
  FLock := Value;
end;

procedure TDataArray.SetModelToNil;
begin
  FModel := nil;
end;

procedure TDataArray.SetClassification(const Value: string);
begin
  FClassification := Value;
end;

procedure TDataArray.SetComment(const Value: string);
begin
  if FComment <> Value then
  begin
    FComment := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TDataArray.SetContourAlg(const Value: TContourAlg);
begin
  FContourAlg := Value;
end;

procedure TDataArray.SetContourInterval(const Value: TRealStorage);
begin
  FContourInterval.Assign(Value);
end;

procedure TDataArray.SetContourLimits(const Value: TColoringLimits);
begin
  FContourLimits.Assign(Value);
end;

function TDataArray.GetBooleanData(const Layer, Row, Col: integer): boolean;
var
  AnArray: T3DBooleanDataSet;
begin
  if FIsUniform = iuTrue then
  begin
    result := FUniformBooleanValue;
    Exit;
  end;
  CheckRestoreData;
  if not UpToDate then
  begin
    Initialize;
  end;
  GetBoolArray(AnArray);
  result := AnArray[Layer, Row, Col];
//  UpdateEvalTime;
end;

function TDataArray.GetClassification: string;
begin
  result := FClassification;
  if result = '' then
  begin
    if (Name = rsActive)
      or (Name = rsKx)
      or (Name = rsKy)
      or (Name = rsKz)
      or (Name = rsPorosity)
      or (Name = rsSpecific_Storage)
      or (Name = rsLong_Dispersivity)
      or (Name = rsHorizontal_Transv_Dispersivity)
      or (Name = rsVertical_Transv_Dispersivity)
      or (Name = rsInitial_Head)
      or (Name = rsInitial_Water_Table)
      or (Name = rsModflow_Initial_Head)
      or (Name = rsModflow_CBKz)
      or (Name = rsSpecificYield)
      or (Name = rsWetDryThreshold)
      or (Name = rsWetDryFlag)
      or (Name = rsWetDry)
      or (Name = rsHorizontalAnisotropy)
      or (Name = rsVerticalAnisotropy)
      then
    begin
      result := StrHydrology;
    end
    else if (Name = rsChemistry_Initial_Solution)
      or (Name = rsChemistry_Initial_Equilibrium_Phases)
      or (Name = rsChemistry_Initial_Surface)
      or (Name = rsChemistry_Initial_Exchange)
      or (Name = rsChemistry_Initial_Gas_Phase)
      or (Name = rsChemistry_Initial_Solid_Solutions)
      or (Name = rsChemistry_Initial_Kinetics)
      then
    begin
      result := StrChemistry;
    end else if (Name = rsPrint_Chemistry)
      or (Name = rsPrint_XYZ_Chemistry)
      then
    begin
      result := StrOutput;
    end
    else if (Name = rsTopLeakyHydraulicConductivity)
      or (Name = rsTopLeakyThickness)
      or (Name = rsFrontLeakyHydraulicConductivity)
      or (Name = rsFrontLeakyThickness)
      or (Name = rsSideLeakyHydraulicConductivity)
      or (Name = rsSideLeakyThickness)
      then
    begin
      result := StrPHASTLeaky;
    end
    else if (Name = rsRiverHydraulicConductivity)
      or (Name = rsRiverWidth)
      or (Name = rsRiverDepth)
      or (Name = rsRiverBedThickness)
      then
    begin
      result := StrPHASTRiver;
    end
    else if (Name = rsSolutionType)
      then
    begin
      result := StrPHASTSpecifiedHead;
    end
    else
    begin
      result := strDefaultClassification;
    end;
    if result <> strDefaultClassification then
    begin
      FClassification := result;
    end;
  end;
end;

function TDataArray.GetColumnCount: integer;
begin
  if (IsUniform = iuTrue) or FCleared {and FDataCached} then
  begin
    result := FCachedColumnCount;
  end
  else
  begin
    result := FColumnCount;
  end;
end;

function TDataArray.GetCompiler: TRbwParser;
var
  LocalModel: TCustomModel;
  ChildModel: TChildModel;
begin
  LocalModel := FModel as TCustomModel;
  if LocalModel is TChildModel then
  begin
    ChildModel := TChildModel(LocalModel);
    if ChildModel.ParentModel <> nil then
    begin
      LocalModel := ChildModel.ParentModel;
    end;
  end;
  result := LocalModel.GetCompiler(Orientation, EvaluatedAt);
end;

function TDataArray.GetDisplayFormula: string;
begin
  Assert(FFormulaObject <> nil);
  result := FFormulaObject.DisplayFormula;
end;

function TDataArray.GetDisplayName: string;
begin
  if FDisplayName = '' then
  begin
    result := Name;
  end
  else
  begin
    result := FDisplayName;
  end;
end;

function TDataArray.GetFormula: string;
begin
  Assert(FFormulaObject <> nil);
  result := FFormulaObject.Formula;
end;

function TDataArray.GetHash: longint;
begin
  result := FHash;
end;

function TDataArray.GetIntegerData(const Layer, Row, Col: integer): integer;
var
  AnArray: T3DIntegerDataSet;
begin
  if FIsUniform = iuTrue then
  begin
    result := FUniformIntegerValue;
    Exit;
  end;
  CheckRestoreData;
  if not UpToDate then
  begin
    Initialize;
  end;
  GetIntegerArray(AnArray);
  result := AnArray[Layer, Row, Col];
//  UpdateEvalTime;
end;

function TDataArray.GetRealData(const Layer, Row, Col: integer): double;
var
  AnArray: T3DRealDataSet;
begin
  if FIsUniform = iuTrue then
  begin
    result := FUniformRealValue;
    Exit;
  end;
  CheckRestoreData;
  if not UpToDate then
  begin
    Initialize;
  end;
  GetRealArray(AnArray);
  result := AnArray[Layer, Row, Col];
//  UpdateEvalTime;
end;

function TDataArray.GetStringData(const Layer, Row, Col: integer): string;
var
  AnArray: T3DStringDataSet;
begin
  if FIsUniform = iuTrue then
  begin
    result := FUniformStringValue;
    Exit;
  end;
  CheckRestoreData;
  if not UpToDate then
  begin
    Initialize;
  end;
  GetStringArray(AnArray);
  result := AnArray[Layer, Row, Col];
//  UpdateEvalTime;
end;

procedure TDataArray.SetBooleanData(const Layer, Row, Col: integer;
  const Value: boolean);
var
  AnArray: T3DBooleanDataSet;
begin
  GetBoolArray(AnArray);
  AnArray[Layer, Row, Col] := Value;
//  UpdateEvalTime;
end;

procedure TDataArray.SetIntegerData(const Layer, Row, Col, Value: integer);
var
  AnArray: T3DIntegerDataSet;
begin
  GetIntegerArray(AnArray);
  if CheckMin and (Value < Min) then
  begin
    AnArray[Layer, Row, Col] := Trunc(Min);
  end
  else if CheckMax and (Value > Max) then
  begin
    AnArray[Layer, Row, Col] := Trunc(Max);
  end
  else
  begin
    AnArray[Layer, Row, Col] := Value;
  end;
//  UpdateEvalTime;
end;

procedure TDataArray.SetRealData(const Layer, Row, Col: integer;
  const Value: double);
var
  AnArray: T3DRealDataSet;
begin
  GetRealArray(AnArray);
  if CheckMin and (Value < Min) then
  begin
    AnArray[Layer, Row, Col] := Min;
  end
  else if CheckMax and (Value > Max) then
  begin
    AnArray[Layer, Row, Col] := Max;
  end
  else
  begin
    AnArray[Layer, Row, Col] := Value;
  end;
//  UpdateEvalTime;
end;

procedure TDataArray.SetStringData(const Layer, Row, Col: integer;
  const Value: string);
var
  AnArray: T3DStringDataSet;
begin
  GetStringArray(AnArray);
  AnArray[Layer, Row, Col] := Value;
//  UpdateEvalTime;
end;

function TDataArray.GetIsValue(const Layer, Row, Col: Integer): boolean;
begin
  if IsUniform = iuTrue then
  begin
    result := True;
    Exit;
  end;
  CheckRestoreData;

  if not UpToDate then
  begin
    Initialize;
  end;
  result := True;
end;

function TDataArray.GetOwner: TPersistent;
begin
  result := frmGoPhast.PhastModel;
end;

function TDataArray.GetTwoDInterpolatorClass: string;
begin
  if TwoDInterpolator = nil then
  begin
    result := '';
  end
  else
  begin
    result := TwoDInterpolator.ClassName;
  end;
end;

procedure TDataArray.SetTwoDInterpolatorClass(const Value: string);
begin
  FreeAndNil(FTwoDInterpolator);
  if Value = '' then
  begin
    Exit;
  end;
  try
    FTwoDInterpolator := TInterpolatorClass(GetClass(Value)).Create(self);
    FTwoDInterpolator.SetSubComponent(True);
  except on E: EInterpolationException do
    begin
      FTwoDInterpolator := nil;
    end;
  end;
end;

function TDataArray.GetAnnotation(const Layer, Row, Col: integer): string;
begin
  if FIsUniform = iuTrue then
  begin
    result := FUniformAnnotation;
    Exit;
  end;
  CheckRestoreData;
  if not UpToDate then
  begin
    Initialize;
  end;
  result := FAnnotation[Layer, Row, Col];
end;

procedure TDataArray.SetAngleType(const Value: TAngleType);
begin
  if FAngleType <> Value then
  begin
    FAngleType := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TDataArray.SetAnnotation(const Layer, Row, Col: integer;
  const Value: string);
begin
  FAnnotation[Layer, Row, Col] := Value;
//  UpdateEvalTime;
end;

function TDataArray.DisplayRealValue: boolean;
begin
  Assert(Datatype = rdtInteger);
  result := False;
end;

procedure TDataArray.SetVisible(const Value: boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
//    frmGoPhast.InvalidateModel;
  end;
end;

function TDataArray.ShouldUseOnInitialize: Boolean;
begin
  result := Assigned(OnInitialize);
  if result and Assigned(OnShouldUseOnInitialize) then
  begin
    OnShouldUseOnInitialize(self, result);
  end;
end;

procedure TDataArray.SetName(const Value: TComponentName);
var
  LocalModel :  TCustomModel;
  MustAdd: boolean;
  NameChanged: Boolean;
  OldParamDataSetName: string;
  NewParamDataSetName: string;
  ParamDataSet: TDataArray;
begin
  NameChanged := Name <> Value;
  LocalModel := FModel as TCustomModel;
  MustAdd := False;
  if NameChanged and (Name <> '') then
  begin
    if LocalModel <> nil then
    begin
      LocalModel.ThreeDGridObserver.StopsTalkingTo(self);
      LocalModel.TopGridObserver.StopsTalkingTo(self);
    end;
  end;
  if NameChanged then
  begin
    if PestParametersUsed then
    begin
      OldParamDataSetName := Format(StrSParameterNames, [Name]);
      NewParamDataSetName := Format(StrSParameterNames, [Value]);
      ParamDataSet := LocalModel.DataArrayManager.
        GetDataSetByName(OldParamDataSetName);
      if ParamDataSet <> nil then
      begin
        ParamDataSet.Name := NewParamDataSetName;
        ParamDataSet.DisplayName := NewParamDataSetName;
      end;
    end;

    if LocalModel <> nil then
    begin
      if LocalModel.DataArrayManager.GetDataSetByName(Name) <> nil then
      begin
        LocalModel.DataArrayManager.RemoveDataSetFromLookUpList(self);
        MustAdd := True;
      end;
    end;
  end;
  inherited;
  if MustAdd then
  begin
    if LocalModel <> nil then
    begin
      LocalModel.DataArrayManager.AddDataSetToLookUpList(self);
    end;
  end;
  if NameChanged then
  begin
    if LocalModel <> nil then
    begin
      case FOrientation of
        dsoTop:
          begin
            LocalModel.TopGridObserver.TalksTo(self);
          end;
        dsoFront, dsoSide, dso3D:
          begin
            LocalModel.ThreeDGridObserver.TalksTo(self);
          end;
        else Assert(False);
      end;
    end;
  end;
  frmGoPhast.InvalidateModel;
end;

function TDataArray.UsedByModel: boolean;
begin
  if Assigned(OnDataSetUsed) then
  begin
    result := OnDataSetUsed(self);
  end
  else
  begin
    result := False;
  end;
end;

function TDataArray.ColorGridValueOK(const Layer, Row, Col: integer): boolean;
begin
  Result := ValueOK(Layer, Row, Col, Limits);
end;

{$OVERFLOWCHECKS OFF}

procedure TDataArray.ComputeHash;
const
  DoubleLimit = 7;
  IntLimit = 3;
Type
  TDoubleBytes = array[0..DoubleLimit] of Byte;
  TIntBytes = array[0..IntLimit] of Byte;
var
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  DoubleBytes: TDoubleBytes;
  ABool: ByteBool;
  IntBytes: TIntBytes;
  ByteIndex: Integer;
  G: longint;
begin
  // modified from PJW hash function in Bucknall, J. M. 2006.
  // The Tomes of Delphi: Algorithms and Data Structures. 
  Initialize;
  case DataType of
    rdtDouble:
      begin
        FHash := 0;
        for LayerIndex := 0 to LayerCount - 1 do
        begin
          for RowIndex := 0 to RowCount - 1 do
          begin
            for ColIndex := 0 to ColumnCount - 1 do
            begin
              if IsValue[LayerIndex, RowIndex, ColIndex] then
              begin
                DoubleBytes := TDoubleBytes(RealData[
                  LayerIndex, RowIndex, ColIndex]);
                for ByteIndex := 0 to DoubleLimit do
                begin
                  FHash := (FHash shl 4) + DoubleBytes[ByteIndex];
                  G := FHash and longint($F0000000);
                  if G <> 0 then
                  begin
                    FHash := FHash xor (G shr 24) xor G;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    rdtInteger:
      begin
        FHash := 0;
        for LayerIndex := 0 to LayerCount - 1 do
        begin
          for RowIndex := 0 to RowCount - 1 do
          begin
            for ColIndex := 0 to ColumnCount - 1 do
            begin
              if IsValue[LayerIndex, RowIndex, ColIndex] then
              begin
                IntBytes := TIntBytes(IntegerData[
                  LayerIndex, RowIndex, ColIndex]);
                for ByteIndex := 0 to IntLimit do
                begin
                  FHash := (FHash shl 4) + IntBytes[ByteIndex];
                  G := FHash and longint($F0000000);
                  if G <> 0 then
                  begin
                    FHash := FHash xor (G shr 24) xor G;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    rdtBoolean:
      begin
        FHash := 0;
        for LayerIndex := 0 to LayerCount - 1 do
        begin
          for RowIndex := 0 to RowCount - 1 do
          begin
            for ColIndex := 0 to ColumnCount - 1 do
            begin
              if IsValue[LayerIndex, RowIndex, ColIndex] then
              begin
                ABool := BooleanData[LayerIndex, RowIndex, ColIndex];
                FHash := (FHash shl 4) + Byte(ABool);
                G := FHash and longint($F0000000);
                if G <> 0 then
                begin
                  FHash := FHash xor (G shr 24) xor G;
                end;
              end;
            end;
          end;
        end;
      end
    else Assert(False)
  end;
end;

{$OVERFLOWCHECKS ON}


procedure TDataArray.UpdateMinMaxValues;
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  FirstValue: Boolean;
  MinimumBoolean: Boolean;
  MaximumBoolean: Boolean;
  StringValues: TStringList;
  MinimumInteger: Integer;
  MaximumInteger: Integer;
  MinimumReal: Double;
  MaximumReal: Double;
  ColLimit: Integer;
  RowLimit: Integer;
  LayerLimit: Integer;
  PhastModel: TCustomModel;
  Grid: TCustomModelGrid;
  LayerMin: Integer;
  RowMin: Integer;
  ColMin: Integer;
  ARealValue: double;
  AnIntValue: Integer;
  MinimumRealPositive: double;
  Mesh: TSutraMesh3D;
  OkLocation: Boolean;
begin
  if not UpToDate then
  begin
    Exit;
  end;
  if (FModel <> nil) then
  begin
    PhastModel := FModel as TCustomModel;
    Grid := PhastModel.Grid;
    Mesh := PhastModel.Mesh as TSutraMesh3D;
  end
  else
  begin
//    PhastModel := nil;
    Grid := nil;
    Mesh := nil;
  end;

  if FMinMaxUpToDate then
  begin
    Exit;
  end;

  GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
    LayerLimit, RowLimit, ColLimit);
  case Datatype of
    rdtDouble:
      begin
        MinimumReal := 0;
        MaximumReal := 0;
        MinimumRealPositive := 0;
        FirstValue := True;
        if LayerMin >= 0 then
        begin
          for LayerIndex := LayerMin to LayerLimit do
          begin
            for RowIndex := RowMin to RowLimit do
            begin
              for ColIndex := ColMin to ColLimit do
              begin
                if Grid <> nil then
                begin
                  OkLocation := Grid.OkLocation(self, LayerIndex, RowIndex, ColIndex);
                end
                else if Mesh <> nil then
                begin
                  OkLocation := Mesh.OkLocation(self, LayerIndex, RowIndex, ColIndex);
                end
                else
                begin
                  OkLocation := IsValue[LayerIndex, RowIndex, ColIndex]
                end;
                if OkLocation then
                begin
                  if FirstValue then
                  begin
                    MinimumReal := RealData[LayerIndex, RowIndex, ColIndex];
                    MaximumReal := MinimumReal;
                    if MinimumReal > 0 then
                    begin
                      MinimumRealPositive := MinimumReal;
                    end;
                    FirstValue := False;
                  end
                  else
                  begin
                    ARealValue := RealData[LayerIndex, RowIndex, ColIndex];
                    if MinimumReal > ARealValue then
                    begin
                      MinimumReal := ARealValue;
                    end;
                    if MaximumReal < ARealValue then
                    begin
                      MaximumReal := ARealValue;
                    end;
                    if (ARealValue > 0)
                      and ((MinimumRealPositive > ARealValue)
                      or (MinimumRealPositive = 0)) then
                    begin
                      MinimumRealPositive := ARealValue;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
        if FirstValue then
        begin
          FMaxValue := StrUnassigned;
          FMinValue := StrUnassigned;
          FMinReal := 0.0;
          FMaxReal := 0.0;
        end
        else
        begin
          FMaxValue := FloatToStrF(MaximumReal, ffGeneral, 7, 0);
          FMinValue := FloatToStrF(MinimumReal, ffGeneral, 7, 0);
          FMinReal := MinimumReal;
          FMaxReal := MaximumReal;
          FMinRealPositive := MinimumRealPositive;
        end;
      end;
    rdtInteger:
      begin
        MinimumInteger := 0;
        MaximumInteger := 0;
        FirstValue := True;
        if LayerMin >= 0 then
        begin
          for LayerIndex := LayerMin to LayerLimit do
          begin
            for RowIndex := RowMin to RowLimit do
            begin
              for ColIndex := ColMin to ColLimit do
              begin
                if Grid <> nil then
                begin
                  OkLocation := Grid.OkLocation(self, LayerIndex, RowIndex, ColIndex);
                end
                else if Mesh <> nil then
                begin
                  OkLocation := Mesh.OkLocation(self, LayerIndex, RowIndex, ColIndex);
                end
                else
                begin
                  OkLocation := IsValue[LayerIndex, RowIndex, ColIndex]
                end;
                if OkLocation then
                begin
                  if FirstValue then
                  begin
                    MinimumInteger := IntegerData[LayerIndex, RowIndex, ColIndex];
                    MaximumInteger := MinimumInteger;
                    FirstValue := False;
                  end
                  else
                  begin
                    AnIntValue := IntegerData[LayerIndex, RowIndex, ColIndex];
                    if MinimumInteger > AnIntValue then
                    begin
                      MinimumInteger := AnIntValue;
                    end;
                    if MaximumInteger < AnIntValue then
                    begin
                      MaximumInteger := AnIntValue;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
        if FirstValue then
        begin
          FMaxValue := StrUnassigned;
          FMinValue := StrUnassigned;
          FMinInteger := 0;
          FMaxInteger := 0;
        end
        else
        begin
          FMaxValue := IntToStr(MaximumInteger);
          FMinValue := IntToStr(MinimumInteger);
          FMinInteger := MinimumInteger;
          FMaxInteger := MaximumInteger;
        end;
      end;
    rdtBoolean:
      begin
        MinimumBoolean := False;
        MaximumBoolean := False;
        FirstValue := True;
        if LayerMin >= 0 then
        begin
          for LayerIndex := LayerMin to LayerLimit do
          begin
            for RowIndex := RowMin to RowLimit do
            begin
              for ColIndex := ColMin to ColLimit do
              begin
                if Grid <> nil then
                begin
                  OkLocation := Grid.OkLocation(self, LayerIndex, RowIndex, ColIndex);
                end
                else if Mesh <> nil then
                begin
                  OkLocation := Mesh.OkLocation(self, LayerIndex, RowIndex, ColIndex);
                end
                else
                begin
                  OkLocation := IsValue[LayerIndex, RowIndex, ColIndex]
                end;
                if OkLocation then
                begin
                  if FirstValue then
                  begin
                    MinimumBoolean := BooleanData[LayerIndex, RowIndex, ColIndex];
                    MaximumBoolean := MinimumBoolean;
                    FirstValue := False;
                  end
                  else
                  begin
                    if MinimumBoolean > BooleanData[LayerIndex, RowIndex, ColIndex] then
                    begin
                      MinimumBoolean := BooleanData[LayerIndex, RowIndex, ColIndex];
                    end;
                    if MaximumBoolean < BooleanData[LayerIndex, RowIndex, ColIndex] then
                    begin
                      MaximumBoolean := BooleanData[LayerIndex, RowIndex, ColIndex];
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
        if FirstValue then
        begin
          FMaxValue := StrUnassigned;
          FMinValue := StrUnassigned;
          FMinBoolean := False;
          FMaxBoolean := False;
        end
        else
        begin
          if MaximumBoolean then
          begin
            FMaxValue := 'True';
          end
          else
          begin
            FMaxValue := 'False';
          end;
          if MinimumBoolean then
          begin
            FMinValue := 'True';
          end
          else
          begin
            FMinValue := 'False';
          end;
          FMinBoolean := MinimumBoolean;
          FMaxBoolean := MaximumBoolean;
        end;
      end;
    rdtString:
      begin
        StringValues := TStringList.Create;
        try
          StringValues.Sorted := True;
          StringValues.Duplicates := dupIgnore;
          StringValues.CaseSensitive := True;
          if LayerMin >= 0 then
          begin
            for LayerIndex := LayerMin to LayerLimit do
            begin
              for RowIndex := RowMin to RowLimit do
              begin
                for ColIndex := ColMin to ColLimit do
                begin
                  if Grid <> nil then
                  begin
                    OkLocation := Grid.OkLocation(self, LayerIndex, RowIndex, ColIndex);
                  end
                  else if Mesh <> nil then
                  begin
                    OkLocation := Mesh.OkLocation(self, LayerIndex, RowIndex, ColIndex);
                  end
                  else
                  begin
                    OkLocation := IsValue[LayerIndex, RowIndex, ColIndex]
                  end;
                  if OkLocation then
                  begin
                    StringValues.Add(StringData[LayerIndex, RowIndex, ColIndex]);
                    if StringValues.Count > 2 then
                    begin
                      StringValues.Delete(1);
                    end;
                  end;
                end;
              end;
            end;
          end;
          if StringValues.Count > 0 then
          begin
            FMaxValue := StringValues[StringValues.Count - 1];
            FMinValue := StringValues[0];
            FMinString := FMinValue;
            FMaxString := FMaxValue;
          end
          else
          begin
            FMaxValue := StrUnassigned;
            FMinValue := StrUnassigned;
            FMinString := '';
            FMaxString := '';
          end;
        finally
          StringValues.Free;
        end;
      end;
  else
    Assert(False);
  end;
  if (FModel <> nil) then
  begin
    PhastModel := FModel as TCustomModel;
    Grid := PhastModel.Grid;
    Mesh := PhastModel.Mesh as TSutraMesh3D;
    if Grid <> nil then
    begin
      if (frmDisplayData <> nil) then
      begin
        if (PhastModel.TopDataSet = self)
          or (Grid.FrontDataSet = self)
          or (Grid.SideDataSet = self)
          or (PhastModel.ThreeDDataSet = self) then
        begin
          frmDisplayData.frameColorGrid.LegendDataSource := self;
          frmDisplayData.ShouldUpdate := True;
//          frmDisplayData.UpdateLabelsAndLegend;
        end;
        if (Grid.TopContourDataSet = self)
          or (Grid.FrontContourDataSet = self)
          or (Grid.SideContourDataSet = self)
          or (Grid.ThreeDContourDataSet = self) then
        begin
          frmDisplayData.frameContourData.LegendDataSource := self;
          frmDisplayData.ShouldUpdate := True;
//          frmDisplayData.UpdateLabelsAndLegend;
        end;
      end;
    end
    else if Mesh <> nil then
    begin
      if (frmDisplayData <> nil) then
      begin
        if (PhastModel.TopDataSet = self)
//          or (Mesh.FrontDataSet = self)
//          or (Mesh.SideDataSet = self)
          or (PhastModel.ThreeDDataSet = self) then
        begin
          frmDisplayData.frameColorGrid.LegendDataSource := self;
          frmDisplayData.ShouldUpdate := True;
//          frmDisplayData.UpdateLabelsAndLegend;
        end;
//        if (Mesh.TopContourDataSet = self)
//          or (Mesh.FrontContourDataSet = self)
//          or (Mesh.SideContourDataSet = self)
//          or (Mesh.ThreeDContourDataSet = self) then
//        begin
//          frmDisplayData.frameContourData.LegendDataSource := self;
//          frmDisplayData.ShouldUpdate := True;
////          frmDisplayData.UpdateLabelsAndLegend;
//        end;
      end;
    end;
  end;
  FMinMaxUpToDate := True;
end;

procedure TDataArray.ReadData(DecompressionStream: TDecompressionStream);
var
  AnnotationIndex: Integer;
  AnsiStringValue: Ansistring;
  StringValue: string;
  ValueLength: Integer;
  BooleanValue: Boolean;
  IntValue: Integer;
  RealValue: Double;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  AnsiAnnText: AnsiString;
  AnnText: string;
  AnnSize: Integer;
  Index: Integer;
  Count: Integer;
  Annotations: TStringList;
  AnnotationIndexArray: array of Integer;
  LayerArray: array of Integer;
  RowArray: array of Integer;
  ColumnArray: array of Integer;
  RealValues: array of double;
  IntegerValues: array of integer;
  BoooleanValues: array of Boolean;
  StringValues: TStringList;
begin
  StringValues := nil;
  Annotations := TStringList.Create;
  try
    DecompressionStream.Read(Count, SizeOf(Count));
    if Count < 0 then
    begin
      // ModelMuse now saves a -1 at the beginning of the data
      // to indicate that the current version is saving Unicode data.
      // Previously, Count was the first thing saved and it would always
      // be greater than or equal to zero.
      FUnicodeSaved := True;
      DecompressionStream.Read(Count, SizeOf(Count));
    end
    else
    begin
      FUnicodeSaved := False;
    end;
    if (Count = 0) and (csReading in (Model as TComponent).ComponentState) then
    begin
      // Data is corrupted
      raise ECorruptedData.Create(StrThereWasAProblem);
    end;
    Annotations.Capacity := Count;
    for Index := 0 to Count - 1 do
    begin
      DecompressionStream.Read(AnnSize, SizeOf(AnnSize));
      if FUnicodeSaved then
      begin
        SetString(AnnText, nil, AnnSize);
        DecompressionStream.Read(Pointer(AnnText)^, AnnSize * SizeOf(Char));
        Annotations.Add(AnnText);
        if (Count = 1) and (AnnText = '') and (csReading in (Model as TComponent).ComponentState) then
        begin
          // Data is corrupted
          raise ECorruptedData.Create(StrThereWasAProblem);
        end;
      end
      else
      begin
        SetString(AnsiAnnText, nil, AnnSize);
        DecompressionStream.Read(Pointer(AnsiAnnText)^, AnnSize * SizeOf(AnsiChar));
        Annotations.Add(string(AnsiAnnText));
        if (Count = 1) and (AnsiAnnText = '') and (csReading in (Model as TComponent).ComponentState) then
        begin
          // Data is corrupted
          raise ECorruptedData.Create(StrThereWasAProblem);
        end;
      end;
    end;
    DecompressionStream.Read(Count, SizeOf(Count));
    SetLength(AnnotationIndexArray, Count);
    SetLength(LayerArray, Count);
    SetLength(RowArray, Count);
    SetLength(ColumnArray, Count);
    case DataType of
      rdtDouble: SetLength(RealValues, Count);
      rdtInteger: SetLength(IntegerValues, Count);
      rdtBoolean: SetLength(BoooleanValues, Count);
      rdtString:
        begin
          StringValues := TStringList.Create;
          StringValues.Capacity := Count;
        end
      else Assert(False);
    end;

    if Count > 0 then
    begin

      DecompressionStream.Read(LayerArray[0], Count*SizeOf(integer));
      DecompressionStream.Read(RowArray[0], Count*SizeOf(integer));
      DecompressionStream.Read(ColumnArray[0], Count*SizeOf(integer));
      DecompressionStream.Read(AnnotationIndexArray[0], Count*SizeOf(integer));

      case DataType of
        rdtDouble: DecompressionStream.Read(RealValues[0], Count*SizeOf(double));
        rdtInteger: DecompressionStream.Read(IntegerValues[0], Count*SizeOf(integer));
        rdtBoolean: DecompressionStream.Read(BoooleanValues[0], Count*SizeOf(boolean));
        rdtString:
          begin
            for Index := 0 to Count - 1 do
            begin
              DecompressionStream.Read(ValueLength, SizeOf(ValueLength));
              if FUnicodeSaved then
              begin
                SetString(StringValue, nil, ValueLength);
                DecompressionStream.Read(Pointer(StringValue)^, ValueLength * SizeOf(Char));
                StringValues.Add(StringValue);
              end
              else
              begin
                SetString(AnsiStringValue, nil, ValueLength);
                DecompressionStream.Read(Pointer(AnsiStringValue)^, ValueLength * SizeOf(AnsiChar));
                StringValues.Add(string(AnsiStringValue));
              end;
            end;
          end;
        else Assert(False);
      end;

      for Index := 0 to Count - 1 do
      begin
        LayerIndex := LayerArray[Index];
        RowIndex := RowArray[Index];
        ColIndex := ColumnArray[Index];
        case DataType of
          rdtDouble:
            begin
              RealValue := RealValues[Index];
              RealData[LayerIndex, RowIndex, ColIndex] := RealValue;
            end;
          rdtInteger:
            begin
              IntValue := IntegerValues[Index];
              IntegerData[LayerIndex, RowIndex, ColIndex] := IntValue;
            end;
          rdtBoolean:
            begin
              BooleanValue := BoooleanValues[Index];
              BooleanData[LayerIndex, RowIndex, ColIndex] := BooleanValue;
            end;
          rdtString:
            begin
              StringValue := StringValues[Index];
              StringData[LayerIndex, RowIndex, ColIndex] := StringValue;
            end;
        else
          Assert(False);
        end;
        AnnotationIndex := AnnotationIndexArray[Index];
        Annotation[LayerIndex, RowIndex, ColIndex] := Annotations[AnnotationIndex];
      end;
    end;
  finally
    Annotations.Free;
    StringValues.Free;
  end;
end;

procedure TDataArray.RefreshFormula;
begin
  FFormula := Formula;
end;

function TDataArray.ValueOK(const Layer, Row, Col: Integer;
  LocalLimits: TColoringLimits): Boolean;
begin
  result := IsValue[Layer, Row, Col];
  if not result then
  begin
    Exit;
  end;
  case Datatype of
    rdtDouble:
      begin
        if not LocalLimits.ValueOk(RealData[Layer, Row, Col]) then
        begin
          result := False;
        end;
      end;
    rdtInteger:
      begin
        if not LocalLimits.ValueOk(IntegerData[Layer, Row, Col]) then
        begin
          result := False;
        end;
      end;
    rdtBoolean:
      begin
      end;
    rdtString:
      begin
        if not LocalLimits.ValueOk(StringData[Layer, Row, Col]) then
        begin
          result := False;
        end;
      end;
  else
    Assert(False);
  end;
end;

procedure GlobalDataArrayRemoveSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TDataArray).RemoveSubscription(Sender, AName);
end;

procedure GlobalDataArrayRestoreSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TDataArray).RestoreSubscription(Sender, AName);
end;

procedure TDataArray.RemoveSubscription(Sender: TObject; const AName: string);
var
  Model: TCustomModel;
  ObservedItem: TObserver;
begin
  Model := FModel as TCustomModel;
  ObservedItem := Model.GetObserverByName(AName);
  if ObservedItem <> nil then
  begin
    // DS may be nil when the model is being destroyed.
    ObservedItem.StopsTalkingTo(self);
    Invalidate;
  end;
end;

procedure TDataArray.CountValues(out LayerLimit, RowLimit, ColLimit,
  Count: Integer);
var
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  LayerMin, RowMin, ColMin: integer;
begin
  Count := 0;
  GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
    LayerLimit, RowLimit, ColLimit);
  if LayerLimit < 0 then
  begin
    Exit;
  end;
//  GetLimits(ColLimit, RowLimit, LayerLimit);
  for LayerIndex := LayerMin to LayerLimit do
  begin
    for RowIndex := RowMin to RowLimit do
    begin
      for ColIndex := ColMin to ColLimit do
      begin
        if IsValue[LayerIndex, RowIndex, ColIndex] then
        begin
          Inc(Count);
        end;
      end;
    end;
  end;
end;

procedure TDataArray.StoreData(Stream: TStream);
var
  ValueLength: Integer;
  StringValue: string;
  AnnSize: Integer;
  Index: Integer;
  AnnCount: Integer;
  AnnotationIndex: Integer;
  LocalAnnotation: string;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  LayerLimit: Integer;
  RowLimit: Integer;
  ColLimit: Integer;
  LocalAnnotatations: TStringList;
  Count: Integer;
  LayerMin, RowMin, ColMin: integer;
  AnnotationIndexArray: array of Integer;
  LayerArray: array of Integer;
  RowArray: array of Integer;
  ColumnArray: array of Integer;
  RealValues: array of double;
  IntegerValues: array of integer;
  BoooleanValues: array of Boolean;
  StringValues: TStringList;
  UnicodeKey: integer;
begin
  Count := 0;
  StringValues := nil;
  LocalAnnotatations := TStringList.Create;
  try
    CountValues(LayerLimit, RowLimit, ColLimit, Count);
    GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
      LayerLimit, RowLimit, ColLimit);
    if Count > 0 then
    begin
      SetLength(AnnotationIndexArray, Count);
      SetLength(LayerArray, Count);
      SetLength(RowArray, Count);
      SetLength(ColumnArray, Count);
      case DataType of
        rdtDouble: SetLength(RealValues, Count);
        rdtInteger: SetLength(IntegerValues, Count);
        rdtBoolean: SetLength(BoooleanValues, Count);
        rdtString:
          begin
            StringValues := TStringList.Create;
            StringValues.Capacity := Count;
          end;
        else Assert(False);
      end;
      Count := 0;
      for LayerIndex := LayerMin to LayerLimit do
      begin
        for RowIndex := RowMin to RowLimit do
        begin
          for ColIndex := ColMin to ColLimit do
          begin
            if IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              LocalAnnotation := Annotation[LayerIndex, RowIndex, ColIndex];
              if LocalAnnotatations.Count < 1000 then
              begin
                AnnotationIndex := LocalAnnotatations.IndexOf(LocalAnnotation);
              end
              else
              begin
                AnnotationIndex := -1;
              end;
              if AnnotationIndex < 0 then
              begin
                AnnotationIndex := LocalAnnotatations.Add(LocalAnnotation);
              end;
              AnnotationIndexArray[Count] := AnnotationIndex;
              LayerArray[Count] := LayerIndex;
              RowArray[Count] := RowIndex;
              ColumnArray[Count] := ColIndex;
              case DataType of
                rdtDouble: RealValues[Count] :=
                  RealData[LayerIndex, RowIndex, ColIndex];

                rdtInteger: IntegerValues[Count] :=
                  IntegerData[LayerIndex, RowIndex, ColIndex];

                rdtBoolean: BoooleanValues[Count] :=
                  BooleanData[LayerIndex, RowIndex, ColIndex];

                rdtString: StringValues.Add(
                  StringData[LayerIndex, RowIndex, ColIndex]);
                Else Assert(False);
              end;
              Inc(Count);
            end;
          end;
        end;
      end;
    end;
    AnnCount := LocalAnnotatations.Count;
    UnicodeKey := -1;
    Stream.Write(UnicodeKey, SizeOf(UnicodeKey));
    Stream.Write(AnnCount, SizeOf(AnnCount));
//    LocalAnnotatations.SaveToStream(Stream);
    for Index := 0 to LocalAnnotatations.Count - 1 do
    begin
      LocalAnnotation := LocalAnnotatations[Index];
      AnnSize := Length(LocalAnnotation);
      Stream.Write(AnnSize, SizeOf(AnnSize));
      Stream.WriteBuffer(Pointer(LocalAnnotation)^,
        ByteLength(LocalAnnotation));
    end;
    Stream.Write(Count, SizeOf(Count));
    if Count > 0 then
    begin
      Stream.Write(LayerArray[0], Count*SizeOf(integer));
      Stream.Write(RowArray[0], Count*SizeOf(integer));
      Stream.Write(ColumnArray[0], Count*SizeOf(integer));
      Stream.Write(AnnotationIndexArray[0], Count*SizeOf(integer));
      case DataType of
        rdtDouble: Stream.Write(RealValues[0], Count*SizeOf(double));
        rdtInteger: Stream.Write(IntegerValues[0], Count*SizeOf(integer));
        rdtBoolean: Stream.Write(BoooleanValues[0], Count*SizeOf(Boolean));
        rdtString:
          begin
//            StringValues.SaveToStream(Stream);
            for Index := 0 to StringValues.Count - 1 do
            begin
              StringValue := StringValues[Index];
              ValueLength := Length(StringValue);
              Stream.Write(ValueLength, SizeOf(ValueLength));
              Stream.WriteBuffer(Pointer(StringValue)^, ByteLength(StringValue));
            end;
          end;
        else Assert(False);
      end;
    end;
    FUnicodeSaved := True;
  finally
    LocalAnnotatations.Free;
    StringValues.Free;
  end;
end;

procedure TDataArray.GetIntegerArray(var AnArray: T3DIntegerDataSet);
begin
  if FDataType <> rdtInteger then
  begin
    raise EInvalidDataType.Create(StrInvalidDataType, '?');
  end
  else
  begin
    if DimensionsChanged then
    begin
      SetDimensions(False);
    end;
    AnArray := T3DIntegerDataSet(FDataArray);
  end;
end;

procedure TDataArray.GetRealArray(var AnArray: T3DRealDataSet);
begin
  if FDataType <> rdtDouble then
  begin
    raise EInvalidDataType.Create(StrInvalidDataType, '?');
  end
  else
  begin
    if DimensionsChanged then
    begin
      SetDimensions(False);
    end;
    AnArray := T3DRealDataSet(FDataArray);
  end;
end;

procedure TDataArray.GetStringArray(var AnArray: T3DStringDataSet);
begin
  if FDataType <> rdtString then
  begin
    raise EInvalidDataType.Create(StrInvalidDataType, '?');
  end
  else
  begin
    if DimensionsChanged then
    begin
      SetDimensions(False);
    end;
    AnArray := T3DStringDataSet(FDataArray);
  end;
end;

procedure TDataArray.GetBoolArray(var AnArray: T3DBooleanDataSet);
begin
  if FDataType <> rdtBoolean then
  begin
    raise EInvalidDataType.Create(StrInvalidDataType, '?');
  end
  else
  begin
    if DimensionsChanged then
    begin
      SetDimensions(False);
    end;
    AnArray := T3DBooleanDataSet(FDataArray);
  end;
end;

function TDataArray.GetLayerCount: integer;
begin
  if (IsUniform = iuTrue) or FCleared then
  begin
    result := FCachedLayerCount;
  end
  else
  begin
    result := FLayerCount;
  end;
end;

procedure TDataArray.GetLimits(out ColLimit, RowLimit, LayerLimit: Integer);
begin
  if (Model as TCustomModel).ModelSelection in SutraSelection then
  begin
    LayerLimit := LayerCount - 1;
    RowLimit := RowCount - 1;
    ColLimit := ColumnCount - 1;
  end
  else
  begin
    case EvaluatedAt of
      eaBlocks:
        begin
          LayerLimit := LayerCount - 1;
          RowLimit := RowCount - 1;
          ColLimit := ColumnCount - 1;
        end;
      eaNodes:
        begin
          LayerLimit := LayerCount;
          RowLimit := RowCount;
          ColLimit := ColumnCount;
        end;
    else
      Assert(False);
    end;
  end;
  case Orientation of
    dsoTop: LayerLimit := 0;
    dsoFront: RowLimit := 0;
    dsoSide: ColLimit := 0;
    dso3D: ; // do nothing
    else Assert(False);
  end;
end;


function TDataArray.GetMaxBoolean: boolean;
begin
  UpdateMinMaxValues;
  result := FMaxBoolean;
end;

function TDataArray.GetMaxInteger: integer;
begin
  UpdateMinMaxValues;
  result := FMaxInteger;
end;

function TDataArray.GetMaxReal: double;
begin
  UpdateMinMaxValues;
  result := FMaxReal
end;

function TDataArray.GetMaxString: string;
begin
  UpdateMinMaxValues;
  result := FMaxString;
end;

function TDataArray.GetMaxValue: string;
begin
  if UpToDate then
  begin
    UpdateMinMaxValues;
    result := FMaxValue
  end
  else
  begin
    result := '?'
  end;
end;

function TDataArray.GetMinBoolean: boolean;
begin
  UpdateMinMaxValues;
  result := FMinBoolean;
end;

function TDataArray.GetMinInteger: integer;
begin
  UpdateMinMaxValues;
  result := FMinInteger;
end;

procedure TDataArray.GetMinMaxStoredLimits(out LayerMin, RowMin, ColMin,
  LayerMax, RowMax, ColMax: integer);
begin
  GetLimits(ColMax, RowMax, LayerMax);
  ColMin := 0;
  RowMin := 0;
  LayerMin := 0;
end;

function TDataArray.GetMinReal: double;
begin
  UpdateMinMaxValues;
  result := FMinReal
end;

function TDataArray.GetMinRealPositive: double;
begin
  UpdateMinMaxValues;
  result := FMinRealPositive;
end;

function TDataArray.GetMinString: string;
begin
  UpdateMinMaxValues;
  result := FMinString;
end;

function TDataArray.GetMinValue: string;
begin
  if UpToDate then
  begin
    UpdateMinMaxValues;
    result := FMinValue
  end
  else
  begin
    result := '?'
  end;
end;

procedure TDataArray.RefreshUseList;
var
  OldUseList: TStringList;
  NewUseList: TStringList;
begin
  if csDestroying in ComponentState then
  begin
    Exit;
  end;
  if (FModel <> nil) then
  begin
    if (csDestroying in (Model as TComponent).ComponentState) then
    begin
      Exit;
    end;
    if (FModel as TCustomModel).Clearing then
    begin
      Exit;
    end;
  end;

  OldUseList := TStringList.Create;
  NewUseList := TStringList.Create;
  try
    FRefreshingOldUseList := True;
    try
      try
        OldUseList.Assign(GetUseList);
      except on ERbwParserError do
        begin
          OldUseList.Clear;
        end;
      end;
    finally
      FRefreshingOldUseList := False;
    end;
    FRefreshingOldUseList := True;
    try
      FUseListUpToDate := False;
      try
        NewUseList.Assign(GetUseList);
      except on ERbwParserError do
        begin
          NewUseList.Clear;
        end;
      end;
    finally
      FRefreshingOldUseList := False;
    end;

    if not (FModel as TCustomModel).Clearing then
    begin
      UpdateSubscriptions(NewUseList, OldUseList);
    end;
  finally
    OldUseList.Free;
    NewUseList.Free
  end;

end;

procedure TDataArray.ChangeAFormula(const NewFormula: string;
  var OldFormula: string; var UseListUpToDate: boolean;
  UseListFunction: TUseListFunction);
var
  NewUseList: TStringList;
  OldUseList: TStringList;
//  Model: TPhastModel;
begin
  if OldFormula <> NewFormula then
  begin
//    Model := FPhastModel as TPhastModel;
    frmGoPhast.InvalidateModel;
    OldUseList := TStringList.Create;
    NewUseList := TStringList.Create;
    try
      try
        OldUseList.Assign(UseListFunction);
      except on ERbwParserError do
        begin
          OldUseList.Clear;
        end;
      end;
      OldFormula := NewFormula;
      UseListUpToDate := False;
      NewUseList.Assign(UseListFunction);
      UpdateSubscriptions(NewUseList, OldUseList);
      Invalidate;
    finally
      OldUseList.Free;
      NewUseList.Free;
    end;
  end;
end;

procedure TDataArray.Clear;
begin
  SetDimensions(True);
end;

procedure TDataArray.SetIsValue(const Layer, Row, Col: Integer;
  const Value: boolean);
begin
  // do nothing
end;

procedure TDataArray.SetLimits(const Value: TColoringLimits);
begin
  FLimits.Assign(Value);
end;

function TDataArray.ContourGridValueOK(const Layer, Row, Col: integer): boolean;
begin
  Result := ValueOK(Layer, Row, Col, ContourLimits);
end;

procedure TDataArray.ContourLimitsChanged(Sender: TObject);
begin

end;

procedure TDataArray.LimitsChanged(Sender: TObject);
var
  Mesh: TSutraMesh3D;
begin
  if (frmGoPhast <> nil) and (frmGoPhast.PhastModel <> nil) then
  begin
    if (frmGoPhast.PhastModel.Grid <> nil) then
    begin
      if self = frmGoPhast.PhastModel.Grid.TopDataSet then
      begin
        frmGoPhast.PhastModel.Grid.NeedToRecalculateTopCellColors := True;
      end;
      if self = frmGoPhast.PhastModel.Grid.FrontDataSet then
      begin
        frmGoPhast.PhastModel.Grid.NeedToRecalculateFrontCellColors := True;
      end;
      if self = frmGoPhast.PhastModel.Grid.SideDataSet then
      begin
        frmGoPhast.PhastModel.Grid.NeedToRecalculateSideCellColors := True;
      end;
      if self = frmGoPhast.PhastModel.Grid.ThreeDDataSet then
      begin
        frmGoPhast.PhastModel.Grid.NeedToRecalculate3DCellColors := True;
        frmGoPhast.PhastModel.Grid.GridChanged;
      end;
    end
    else
    begin
      Mesh := frmGoPhast.PhastModel.Mesh as TSutraMesh3D;
      if Mesh <> nil then
      begin
        Mesh.NeedToRecalculateTopColors := True;
        Mesh.NeedToRecalculateFrontColors := True;
        frmGoPhast.InvalidateImage32AllViews;
      end;
    end;
  end;
end;

procedure TDataArray.Loaded;
begin
  CreatePestParmNameDataSet;
end;

procedure TDataArray.RestoreUpToDateStatus;
begin
  inherited;
  if FReadDataFromFile then
  begin
    UpToDate := True;
    FReadDataFromFile := False;
    CacheData;
  end;
end;

procedure TDataArray.ApplyPestParameter;
var
  PestParameters: TStringList;
  Annotations: TStringList;
  ModifiedAnnotations: TStringList;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  ParamDataArray: TDataArray;
  AnnotationIndex: Integer;
  PIndex: Integer;
  LocalModel: TCustomModel;
  AParam: TModflowSteadyParameter;
  AIndex: Integer;
  AParamName: string;
  ParameterAssigned: Boolean;
begin
  FSuppressCache := True;
  try
    UpToDate := True;
  finally
    FSuppressCache := False;
  end;
  UsedPestParameters.Clear;
  LocalModel := Model as TCustomModel;
  PestParameters := TStringList.Create;
  Annotations := TStringList.Create;
  ModifiedAnnotations := TStringList.Create;
  try
    PestParameters.CaseSensitive := False;
    for PIndex := 0 to LocalModel.ModflowSteadyParameters.Count - 1 do
    begin
      AParam := LocalModel.ModflowSteadyParameters[PIndex];
      if AParam.ParameterType = ptPEST then
      begin
        PestParameters.AddObject(LowerCase(AParam.ParameterName), AParam);
      end;
    end;
    PestParameters.Sorted := True;
    Annotations.Sorted := True;
    Annotations.Duplicates := dupIgnore;

    for LayerIndex := 0 to LayerCount - 1 do
    begin
      for RowIndex := 0 to RowCount - 1 do
      begin
        for ColIndex := 0 to ColumnCount - 1 do
        begin
          if IsValue[LayerIndex, RowIndex, ColIndex]  then
          begin
            Annotations.Add(Annotation[LayerIndex, RowIndex, ColIndex]);
          end;
        end;
      end;
    end;

    for AnnotationIndex := 0 to Annotations.Count - 1 do
    begin
      ModifiedAnnotations.Add(Format(StrSMultipliedByAP,
        [Annotations[AnnotationIndex]]));
    end;

    ParamDataArray := LocalModel.DataArrayManager.GetDataSetByName
      (ParamDataSetName);
    Assert(ParamDataArray <> nil);
    ParamDataArray.Initialize;

    ParameterAssigned := False;
    for LayerIndex := 0 to LayerCount - 1 do
    begin
      for RowIndex := 0 to RowCount - 1 do
      begin
        for ColIndex := 0 to ColumnCount - 1 do
        begin
          if IsValue[LayerIndex, RowIndex, ColIndex]  then
          begin
            AParamName := Trim(LowerCase(ParamDataArray.StringData[
              LayerIndex,RowIndex,ColIndex]));
            if AParamName = '' then
            begin
              Continue;
            end;
            PIndex := PestParameters.IndexOf(AParamName);
            if PIndex >= 0 then
            begin
              ParameterAssigned := True;
              AParam := PestParameters.Objects[PIndex] as TModflowSteadyParameter;
              AParam.IsUsedInTemplate := True;
              UsedPestParameters.Add(AParam.ParameterName);
              RealData[LayerIndex, RowIndex, ColIndex] := AParam.Value *
                RealData[LayerIndex, RowIndex, ColIndex];
              AIndex := Annotations.IndexOf(Annotation[LayerIndex, RowIndex, ColIndex]);
              Assert(AIndex >= 0);
              Annotation[LayerIndex, RowIndex, ColIndex] :=
                ModifiedAnnotations[AIndex];
//               :=
            end;
          end;
        end;
      end;
    end;

    if not ParameterAssigned then
    begin
      frmErrorsAndWarnings.AddError(Model as TCustomModel, StrNoPESTParameterAs,
        Format(StrNoPESTParametersA, [Name]))
    end;

  finally
    PestParameters.Free;
    Annotations.Free;
    ModifiedAnnotations.Free;
  end;
end;

procedure TDataArray.PostInitialize;
begin
  if Assigned(OnPostInitialize) then
  begin
    UpToDate := True;
    OnPostInitialize(self);
  end;
  if PestParametersUsed then
  begin
    ApplyPestParameter;
  end;

end;

{ TCustom2DInterpolater }

procedure TCustom2DInterpolater.Assign(Source: TPersistent);
begin
  if not (Source is TCustom2DInterpolater) then
  begin
    inherited Assign(Source);
  end;
end;

function TCustom2DInterpolater.BooleanResult(const Location: TPoint2D):
  boolean;
begin
  result := False;
  if not (rdtBoolean in ValidReturnTypes) then
  begin
    raise EInterpolationException.Create(StrErrorThisInterpol);
  end;
end;

constructor TCustom2DInterpolater.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner <> nil then
  begin
    SetSubComponent(True);
  end;
  if not (AOwner is TDataArray) then
  begin
    Exit;
  end;
  FDataSet := AOwner as TDataArray;
  FModel := FDataSet.FModel as TCustomModel;
  if not (FDataSet.DataType in ValidReturnTypes) then
  begin
    raise EInterpolationException.Create(Format(
      StrErrorThisInterpolVar, [DataTypeToString(DataSet.DataType)]));
  end;
  if not (FDataSet.Orientation in ValidOrientations) then
  begin
    raise EInterpolationException.Create(Format(
      StrErrorThisInterpolDataArray, [DataSet.Name]));
  end;
  FReady := True;
end;

procedure TCustom2DInterpolater.Edit;
begin
  if Assigned(FOnEdit) then
  begin
    FOnEdit(self);
  end
end;

procedure TCustom2DInterpolater.FillScreenObjectList(
  const ListOfScreenObjects: TList);
var
  Index: integer;
  AScreenObject: TScreenObject;
  DataSetIndex: integer;
begin
  ListOfScreenObjects.Clear;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if AScreenObject.Deleted
      or not AScreenObject.SetValuesByInterpolation
      or not AScreenObject.UsedModels.UsesModel(DataSet.Model as TCustomModel)
      or (AScreenObject.ElevationCount <> ecZero) then
    begin
      continue;
    end;

    case AScreenObject.ViewDirection of
      vdTop:
        begin
          if DataSet.Orientation <> dsoTop then
          begin
            Continue;
          end;
        end;
      vdFront:
        begin
          if DataSet.Orientation <> dsoFront then
          begin
            Continue;
          end;
        end;
      vdSide:
        begin
          if DataSet.Orientation <> dsoSide then
          begin
            Continue;
          end;
        end;
    else Assert(False);
    end;

    DataSetIndex := AScreenObject.IndexOfDataSet(DataSet);
    if DataSetIndex < 0 then
    begin
      continue;
    end;
    
    ListOfScreenObjects.Add(AScreenObject);
  end;
end;

procedure TCustom2DInterpolater.Finalize(const DataSet: TDataArray);
begin
  if Assigned(FOnFinalize) then
  begin
    FOnFinalize(self, DataSet);
  end;
end;

procedure TCustom2DInterpolater.Initialize(const DataSet: TDataArray);
begin
  if Assigned(FOnInitialize) then
  begin
    FOnInitialize(self, DataSet);
  end;
end;

function TCustom2DInterpolater.IntegerResult(const Location: TPoint2D):
  integer;
begin
  result := 0;
  if not (rdtInteger in ValidReturnTypes) then
  begin
    raise EInterpolationException.Create(StrErrorThisInterpolInt);
  end;
end;

function TCustom2DInterpolater.LastAnnotation: string;
begin
  result := InterpolatorName;
end;

function TCustom2DInterpolater.RealResult(const Location: TPoint2D): real;
begin
  result := 0;
  if not (rdtDouble in ValidReturnTypes) then
  begin
    raise EInterpolationException.Create(StrErrorThisInterpolFloat);
  end;
end;

function TCustom2DInterpolater.SameAs(
  AnotherInterpolator: TCustom2DInterpolater): boolean;
begin
  result := (AnotherInterpolator <> nil);
  if result then
  begin
    result := ClassType = AnotherInterpolator.ClassType;
    if result then
    begin
      result := (ValidReturnTypes = AnotherInterpolator.ValidReturnTypes)
        and (ValidOrientations = AnotherInterpolator.ValidOrientations);
    end;
  end;
end;

procedure TCustom2DInterpolater.SetEpsilonX(const Value: Double);
begin
  FEpsilonX := Value;
end;

procedure TCustom2DInterpolater.SetEpsilonY(const Value: Double);
begin
  FEpsilonY := Value;
end;

function TCustom2DInterpolater.ShouldInterpolate: boolean;
var
  Index: integer;
  AScreenObject: TScreenObject;
  DataSetIndex: integer;
begin
  result := False;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if AScreenObject.Deleted or not AScreenObject.SetValuesByInterpolation
      then
    begin
      continue;
    end;
    DataSetIndex := AScreenObject.IndexOfDataSet(FDataSet);
    if DataSetIndex < 0 then
    begin
      continue;
    end;

    if not AScreenObject.UsedModels.UsesModel(FDataSet.Model as TCustomModel) then
    begin
      Continue;
    end;

    if AScreenObject.Count = 0 then
    begin
      Continue;
    end;


    result := True;
    break;
  end;
end;

function TCustom2DInterpolater.StringResult(const Location: TPoint2D):
  string;
begin
  result := '';
  if not (rdtString in ValidReturnTypes) then
  begin
    raise EInterpolationException.Create(StrErrorThisInterpolString);
  end;
end;

class function TCustom2DInterpolater.ValidOrientations: TDataSetOrientations;
begin
  if frmGoPhast.ModelSelection = msUndefined then
  begin
    result := [dsoTop, dsoFront, dsoSide, dso3D];
  end
  else if frmGoPhast.ModelSelection = msPhast then
  begin
    result := [dsoTop, dsoFront, dsoSide];
  end
  else
  begin
    result := [dsoTop];
  end;
end;

{ TCustomSparseDataSet }

procedure TCustomSparseDataSet.Clear;
begin
  // don't call inherited Clear.
  FAnnotation.Clear;
end;

constructor TCustomSparseDataSet.Create(AnOwner: IModelMuseModel);
var
  LocalModel: TCustomModel;
  LayerCount, RowCount, ColumnCount: Integer;
begin
  inherited Create(AnOwner);
  if AnOwner <> nil then
  begin
    LocalModel := AnOwner as TCustomModel;
    LayerCount := LocalModel.LayerCount+1;
    RowCount := LocalModel.RowCount+1;
    ColumnCount := LocalModel.MaxColumnCount;
  end
  else
  begin
    LayerCount := 0;
    RowCount := 0;
    ColumnCount := 0;
  end;
  FPriorLayer := -1;
  FPriorRow := -1;
  FPriorCol := -1;
  FAnnotation := T3DSparseStringArray.Create(GetQuantum(LayerCount),
    GetQuantum(RowCount), GetQuantum(ColumnCount));
  // Sparase Array data sets are only used for boundary conditions in
  // PHAST and the boundary conditions all apply to nodes.
  EvaluatedAt := eaNodes;
end;

destructor TCustomSparseDataSet.Destroy;
begin
  FreeAndNil(FAnnotation);
  inherited;
end;

function TCustomSparseDataSet.GetAnnotation(const Layer, Row,
  Col: integer): string;
begin
  result := FAnnotation[Layer, Row, Col];
end;

procedure TCustomSparseDataSet.Initialize;
var
  ScreenObjectIndex: integer;
  AScreenObject: TScreenObject;
  FreeStack: boolean;
  StackPosition: Integer;
  StackIndex: Integer;
begin
  // Values are assigned only using screen objects. Neither iterpolation nor
  // default expressions are used.

  if UpToDate and not DimensionsChanged then
  begin
    if FDataCached and FCleared then
    begin
      RestoreData;
    end;
    Exit;
  end;
  FDataCached := False;

  FreeStack := (Stack = nil);
  try
    if FreeStack then
    begin
      Stack := TStringList.Create;
    end;
    if Stack.IndexOf(Name) >= 0 then
    begin
      UpToDate := True;
      Stack[0] := Stack[0] + ' depends on';
      for StackIndex := 1 to Stack.Count - 2 do
      begin
        Stack[StackIndex] := Stack[StackIndex] + ' which depends on';
      end;
      HandleCircularReferenceError(Format(StrCircularReferenceI2, [Name, Stack.Text]), nil);
      Exit;
//      raise ECircularReference.Create(Format(StrCircularReferenceI2, [Name, Stack.Text]));
    end;
    StackPosition := Stack.Add(Name);

    try
      GlobalEvaluatedAt := EvaluatedAt;

      SetDimensions(False);

      if ShouldUseOnInitialize then
      begin
        OnInitialize(Self);
      end
      else
      begin
        for ScreenObjectIndex := 0 to
          frmGoPhast.PhastModel.ScreenObjectCount - 1 do
        begin
          AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
          if not AScreenObject.Deleted
            and AScreenObject.UsedModels.UsesModel(Model as TCustomModel) then
          begin
            AScreenObject.AssignValuesToDataSet(self, FModel as TCustomModel,
              UseLgrEdgeCells);
          end;
        end;
      end;
    finally
      if not FreeStack then
      begin
        Stack.Delete(StackPosition);
      end;
    end;

  finally
    if FreeStack then
    begin
      FreeAndNil(Stack);
      frmGoPhast.PhastModel.DataArrayManager.CacheDataArrays;
    end;
  end;

  PostInitialize;

  if FreeStack then
  begin
    if (frmDisplayData <> nil) and frmDisplayData.ShouldUpdate then
    begin
      frmDisplayData.UpdateLabelsAndLegend;
    end;

    if frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.Show;
    end;
  end;

  UpToDate := True;
end;

procedure TCustomSparseDataSet.Invalidate;
begin
  inherited;
  FPriorLayer := -1;
  FPriorRow := -1;
  FPriorCol := -1;
end;

function TCustomSparseDataSet.IsSparseArray: boolean;
begin
  result := True;
end;

procedure TCustomSparseDataSet.SetAnnotation(const Layer, Row,
  Col: integer; const Value: string);
begin
  FAnnotation[Layer, Row, Col] := Value;
  FPriorLayer := -1;
  FPriorRow := -1;
  FPriorCol := -1;
end;

procedure TCustomSparseDataSet.SetDimensions(const SetToZero: boolean);
var
  NumberOfLayers: Integer;
  NumberOfRows: Integer;
  NumberOfColumns: Integer;
  LocalModel: TCustomModel;
  Grid: TCustomModelGrid;
  Mesh: TSutraMesh2D;
begin
  // don't call inherited.
  // Do this instead.

  if SetToZero then
  begin
    NumberOfLayers := 0;
    NumberOfRows := 0;
    NumberOfColumns := 0;
    FLayerCount := NumberOfLayers;
    FRowCount := NumberOfRows;
    FColumnCount := NumberOfColumns;
  end
  else
  begin
    LocalModel := Model as TCustomModel;

    if LocalModel <> nil then
    begin
      if LocalModel.ModelSelection in SutraSelection then
      begin
        Mesh := LocalModel.SutraMesh.Mesh2D;
        if Mesh <> nil then
        begin

          if (LocalModel.SutraMesh.MeshType = mt3D)
            and (EvaluatedAt = eaNodes)
            and (Orientation = dso3D) then
          begin
            NumberOfLayers := frmGoPhast.PhastModel.
              SutraLayerStructure.LayerCount+1;
          end
          else
          begin
            NumberOfLayers := frmGoPhast.PhastModel.
              SutraLayerStructure.LayerCount;
          end;
          NumberOfRows := 1;
          case EvaluatedAt of
            eaBlocks: NumberOfColumns := Mesh.Elements.Count;
            eaNodes: NumberOfColumns := Mesh.Nodes.Count;
            else Assert(False);
          end;
        end
        else
        begin
          GetRequiredDimensions(NumberOfLayers, NumberOfRows, NumberOfColumns);
        end;
      end
      else
      begin
        Grid := LocalModel.Grid;
        if Grid <> nil then
        begin
//          case EvaluatedAt of
//            eaBlocks:
//              begin
                NumberOfLayers := Grid.LayerCount;
                NumberOfRows := Grid.RowCount;
                NumberOfColumns := Grid.ColumnCount;
//              end;
//            eaNodes:
//              begin
//                NumberOfLayers := Grid.LayerCount+1;
//                NumberOfRows := Grid.RowCount+1;
//                NumberOfColumns := Grid.ColumnCount+1;
//              end;
//          end;
        end
        else
        begin
          GetRequiredDimensions(NumberOfLayers, NumberOfRows, NumberOfColumns);
        end;
      end;
    end
    else
    begin
      GetRequiredDimensions(NumberOfLayers, NumberOfRows, NumberOfColumns);
    end;
  end;
  NumberOfLayers := Math.Max(NumberOfLayers, 0);
  NumberOfRows := Math.Max(NumberOfRows, 0);
  NumberOfColumns := Math.Max(NumberOfColumns, 0);

  UpdateDimensions(NumberOfLayers, NumberOfRows, NumberOfColumns);

//  inherited;

  if FAnnotation <> nil then
  begin
    FAnnotation.Clear;
  end;
  FDimensionsChanged := False;
end;

{ TRealSparseDataSet }
procedure TRealSparseDataSet.Clear;
begin
  inherited;
  if FRealValues <> nil then
  begin
    FRealValues.Clear;
  end;
end;

constructor TRealSparseDataSet.Create(AnOwner: IModelMuseModel);
begin
  inherited;
  FRealValues := T3DSparseRealArray.Create(GetQuantum(LayerCount),
    GetQuantum(RowCount), GetQuantum(ColumnCount));
  DataType := rdtDouble;
end;

destructor TRealSparseDataSet.Destroy;
begin
  FreeAndNil(FRealValues);
  inherited;
end;

function TRealSparseDataSet.GetIsValue(
  const Layer, Row, Col: Integer): boolean;
begin
  result := inherited GetIsValue(Layer, Row, Col);
  if result then
  begin
    result := FRealValues.IsValue[Layer, Row, Col];
    FPriorResult := result;
  end;
end;

function TRealSparseDataSet.GetMaxColumn: Integer;
begin
  CheckRestoreData;
  result := FRealValues.MaxCol;
end;

function TRealSparseDataSet.GetMaxLayer: Integer;
begin
  CheckRestoreData;
  result := FRealValues.MaxLayer;
end;

function TRealSparseDataSet.GetMaxRow: Integer;
begin
  CheckRestoreData;
  result := FRealValues.MaxRow;
end;

function TRealSparseDataSet.GetMinColumn: Integer;
begin
  CheckRestoreData;
  result := FRealValues.MinCol;
end;

function TRealSparseDataSet.GetMinLayer: Integer;
begin
  CheckRestoreData;
  result := FRealValues.MinLayer
end;

procedure TRealSparseDataSet.GetMinMaxStoredLimits(out LayerMin, RowMin, ColMin,
  LayerMax, RowMax, ColMax: integer);
begin
  CheckRestoreData;
  LayerMin := FRealValues.MinLayer;
  RowMin := FRealValues.MinRow;
  ColMin := FRealValues.MinCol;
  LayerMax := FRealValues.MaxLayer;
  RowMax := FRealValues.MaxRow;
  ColMax := FRealValues.MaxCol;
end;

function TRealSparseDataSet.GetMinRow: Integer;
begin
  CheckRestoreData;
  result := FRealValues.MinRow
end;

function TRealSparseDataSet.GetRealData(const Layer, Row,
  Col: integer): double;
begin
  Assert(IsValue[Layer, Row, Col]);
  result := FRealValues[Layer, Row, Col];
end;

procedure TRealSparseDataSet.RemoveValue(const Layer, Row, Col: Integer);
begin
  if IsValue[Layer, Row, Col] then
  begin
    FRealValues.RemoveValue(Layer, Row, Col);
  end;
end;

procedure TRealSparseDataSet.SetDataType(const Value: TRbwDataType);
begin
  Assert(Value = rdtDouble);
  inherited;
end;

procedure TRealSparseDataSet.SetDimensions(const SetToZero: boolean);
begin
  inherited;
  if FRealValues <> nil then
  begin
    FRealValues.Clear;
  end;
end;

procedure TRealSparseDataSet.SetRealData(const Layer, Row, Col: integer;
  const Value: double);
begin
  // don't call inherited;
  if CheckMin and (Value < Min) then
  begin
    FRealValues[Layer, Row, Col] := Min;
  end
  else if CheckMax and (Value > Max) then
  begin
    FRealValues[Layer, Row, Col] := Max;
  end
  else
  begin
    FRealValues[Layer, Row, Col] := Value;
  end;
end;

procedure TRealSparseDataSet.UpdateDimensions(NumberOfLayers, NumberOfRows,
  NumberOfColumns: integer; ForceResize: boolean);
var
  OldLayerCount: integer;
  OldRowCount: integer;
  OldColumnCount: integer;
begin
  OldLayerCount := LayerCount;
  OldRowCount := RowCount;
  OldColumnCount := ColumnCount;
  inherited;
  if ((OldLayerCount > MaxSmallArraySize) <> (NumberOfLayers > MaxSmallArraySize))
    or ((OldRowCount > MaxSmallArraySize) <> (NumberOfRows > MaxSmallArraySize))
    or ((OldColumnCount > MaxSmallArraySize) <> (NumberOfColumns > MaxSmallArraySize))
    then
  begin
    FRealValues.Free;
    FRealValues := T3DSparseRealArray.Create(GetQuantum(NumberOfLayers),
      GetQuantum(NumberOfRows), GetQuantum(NumberOfColumns));
  end;
end;

{ TIntegerSparseDataSet }

procedure TIntegerSparseDataSet.Clear;
begin
  inherited;
  if FIntegerValues <> nil then
  begin
    FIntegerValues.Clear;
  end;
end;

constructor TIntegerSparseDataSet.Create(AnOwner: IModelMuseModel);
begin
  inherited;
  FIntegerValues := T3DSparseIntegerArray.Create(GetQuantum(LayerCount),
    GetQuantum(RowCount), GetQuantum(ColumnCount));
  DataType := rdtInteger;
end;

destructor TIntegerSparseDataSet.Destroy;
begin
  FreeAndNil(FIntegerValues);
  inherited;
end;

function TIntegerSparseDataSet.GetIsValue(
  const Layer, Row, Col: Integer): boolean;
begin
  result := inherited GetIsValue(Layer, Row, Col)
    and FIntegerValues.IsValue[Layer, Row, Col];
  FPriorResult := result;
end;

procedure TIntegerSparseDataSet.GetMinMaxStoredLimits(out LayerMin, RowMin,
  ColMin, LayerMax, RowMax, ColMax: integer);
begin
  CheckRestoreData;
  LayerMin := FIntegerValues.MinLayer;
  RowMin := FIntegerValues.MinRow;
  ColMin := FIntegerValues.MinCol;
  LayerMax := FIntegerValues.MaxLayer;
  RowMax := FIntegerValues.MaxRow;
  ColMax := FIntegerValues.MaxCol;
end;

procedure TIntegerSparseDataSet.RemoveValue(const Layer, Row, Col: Integer);
begin
  if IsValue[Layer, Row, Col] then
  begin
    FIntegerValues.RemoveValue(Layer, Row, Col);
  end;
end;

function TIntegerSparseDataSet.GetIntegerData(const Layer, Row,
  Col: integer): integer;
begin
  Assert(IsValue[Layer, Row, Col]);
  result := FIntegerValues[Layer, Row, Col];
end;

procedure TIntegerSparseDataSet.SetDataType(const Value: TRbwDataType);
begin
  Assert(Value = rdtInteger);
  inherited;
end;

procedure TIntegerSparseDataSet.SetDimensions(const SetToZero: boolean);
begin
  inherited;
  if FIntegerValues <> nil then
  begin
    FIntegerValues.Clear;
  end;
end;

procedure TIntegerSparseDataSet.SetIntegerData(const Layer, Row, Col: integer;
  const Value: integer);
var
  AValue: integer;
begin
  // don't call inherited;
  AValue := Value;
  if CheckMin and (Value < Min) then
  begin
    AValue := Trunc(Min);
    if AValue < Min then
    begin
      Inc(AValue);
    end
  end
  else if CheckMax and (Value > Max) then
  begin
    AValue := Trunc(Max);
    if AValue > Max then
    begin
      Dec(AValue);
    end
  end;
  FIntegerValues[Layer, Row, Col] := AValue;
end;

procedure TIntegerSparseDataSet.SetIsValue(const Layer, Row, Col: Integer;
  const Value: boolean);
begin
  if (BoundaryTypeDataSet = self) then
  begin
    FIntegerValues.IsValue[Layer, Row, Col] := Value;
  end
  else
  begin
    inherited SetIsValue(Layer, Row, Col, Value)
  end;
end;

procedure TIntegerSparseDataSet.UpdateDimensions(NumberOfLayers, NumberOfRows,
  NumberOfColumns: integer; ForceResize: boolean);
var
  OldLayerCount: integer;
  OldRowCount: integer;
  OldColumnCount: integer;
begin
  OldLayerCount := LayerCount;
  OldRowCount := RowCount;
  OldColumnCount := ColumnCount;
  inherited;
  if ((OldLayerCount > MaxSmallArraySize) <> (NumberOfLayers > MaxSmallArraySize))
    or ((OldRowCount > MaxSmallArraySize) <> (NumberOfRows > MaxSmallArraySize))
    or ((OldColumnCount > MaxSmallArraySize) <> (NumberOfColumns > MaxSmallArraySize))
    then
  begin
    FIntegerValues.Free;
    FIntegerValues := T3DSparseIntegerArray.Create(GetQuantum(NumberOfLayers),
      GetQuantum(NumberOfRows), GetQuantum(NumberOfColumns));
  end;
end;

{ TColoringLimit }

procedure TColoringLimit.Assign(Source: TPersistent);
var
  Value: TColoringLimit;
begin
  if Source is TColoringLimit then
  begin
    Value := TColoringLimit(Source);
    UseLimit := Value.UseLimit;
    if UseLimit then
    begin
      DataType := Value.DataType;
      case DataType of
        rdtDouble:
          begin
            RealLimitValue := Value.RealLimitValue;
          end;
        rdtInteger:
          begin
            IntegerLimitValue := Value.IntegerLimitValue;
          end;
        rdtBoolean:
          begin
            BooleanLimitValue := Value.BooleanLimitValue;
          end;
        rdtString:
          begin
            StringLimitValue := Value.StringLimitValue;
          end;
      else
        Assert(False);
      end;
    end
    else
    begin
      BooleanLimitValue := Value.DefaultBooleanLimitValue;
    end;
  end
  else
  begin
    inherited;
  end;
end;

constructor TColoringLimit.Create;
begin
  UseLimit := False;
  FBooleanLimitValue := False;
  FIntegerLimitValue := 0;
  FRealLimitValue := 0;
  FStringLimitValue := '';
end;

function TColoringLimit.OkLogLimit: boolean;
begin
  result := True;
  if UseLimit and (DataType = rdtDouble) then
  begin
    result := RealLimitValue > 0;
  end;
end;

procedure TColoringLimit.SetBooleanLimitValue(const Value: boolean);
begin
  if FBooleanLimitValue <> Value then
  begin
    FBooleanLimitValue := Value;
    if Assigned(OnChange) then
      OnChange(self);
  end;
end;

procedure TColoringLimit.SetDataType(const Value: TRbwDataType);
begin
  FDataType := Value;
end;

procedure TColoringLimit.SetIntegerLimitValue(const Value: integer);
begin
  if FIntegerLimitValue <> Value then
  begin
    FIntegerLimitValue := Value;
    if Assigned(OnChange) then
      OnChange(self);
  end;
end;

procedure TColoringLimit.SetRealLimitValue(const Value: double);
begin
  if FRealLimitValue <> Value then
  begin
    FRealLimitValue := Value;
    if Assigned(OnChange) then
      OnChange(self);
  end;
end;

procedure TColoringLimit.SetStringLimitValue(const Value: string);
begin
  if FStringLimitValue <> Value then
  begin
    FStringLimitValue := Value;
    if Assigned(OnChange) then
      OnChange(self);
  end;
end;

procedure TColoringLimit.SetUseLimit(const Value: boolean);
begin
  if FUseLimit <> Value then
  begin
    FUseLimit := Value;
    if Assigned(OnChange) then
      OnChange(self);
  end;
end;
{ TColoringLimits }

procedure TColoringLimits.Assign(Source: TPersistent);
var
  Value: TColoringLimits;
begin
  if Source is TColoringLimits then
  begin
    Value := TColoringLimits(Source);
    LowerLimit := Value.LowerLimit;
    UpperLimit := Value.UpperLimit;
    ActiveOnly := Value.ActiveOnly;
    RealValuesToSkip := Value.RealValuesToSkip;
    IntegerValuesToSkip := Value.IntegerValuesToSkip;
    StringValuesToSkip := Value.StringValuesToSkip;
    LogTransform := Value.LogTransform;
    Epsilon := Value.Epsilon;
    Update;
  end
  else
  begin
    inherited;
  end;
end;

procedure TColoringLimits.Changed;
begin
  if Assigned(FOnChange) then
  begin
    FOnChange(self)
  end;
end;

constructor TColoringLimits.Create;
begin
  FShadeInactiveArea := True;
  FStoredEpsilon:= TRealStorage.Create;
  Epsilon := 1e-6;
  FLowerLimit := TColoringLimit.Create;
  FUpperLimit := TColoringLimit.Create;
  FUpperLimit.DefaultBooleanLimitValue := True;
  FRealValuesToSkip := TSkipRealCollection.Create;
  FIntegerValuesToSkip := TSkipIntegerCollection.Create;
  FStringValuesToSkip := TStringList.Create;
  TStringList(FStringValuesToSkip).Sorted := true;
  TStringList(FStringValuesToSkip).Duplicates := dupIgnore;
end;

destructor TColoringLimits.Destroy;
begin
  FStringValuesToSkip.Free;
  FIntegerValuesToSkip.Free;
  FRealValuesToSkip.Free;
  FLowerLimit.Free;
  FUpperLimit.Free;
  FStoredEpsilon.Free;
  inherited;
end;

function TColoringLimits.GetEpsilon: double;
begin
  result := FStoredEpsilon.Value;
end;

procedure TColoringLimits.SetActiveOnly(const Value: boolean);
begin
  if FActiveOnly <> Value then
  begin
    FActiveOnly := Value;
    Changed
  end;
end;

procedure TColoringLimits.SetEpsilon(const Value: double);
begin
  if FStoredEpsilon.Value <> Value then
  begin
    FStoredEpsilon.Value := Value;
    Changed
  end;
end;

procedure TColoringLimits.SetIntegerValuesToSkip(
  const Value: TSkipIntegerCollection);
begin
  FIntegerValuesToSkip.Assign(Value);
end;

procedure TColoringLimits.SetLogTransform(const Value: boolean);
begin
  if FLogTransform <> Value then
  begin
    FLogTransform := Value;
    Changed
  end;
end;

procedure TColoringLimits.SetLowerLimit(const Value: TColoringLimit);
begin
  FLowerLimit.Assign(Value);
end;

procedure TColoringLimits.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
  LowerLimit.OnChange := Value;
  UpperLimit.OnChange := Value;
  IntegerValuesToSkip.OnChange := Value;
  RealValuesToSkip.OnChange := Value;
  TStringList(FStringValuesToSkip).OnChange := Value;
end;

procedure TColoringLimits.SetRealValuesToSkip(const Value: TSkipRealCollection);
begin
  FRealValuesToSkip.Assign(Value);
end;

procedure TColoringLimits.SetStoredEpsilon(const Value: TRealStorage);
begin
  FStoredEpsilon.Assign(Value);
end;

procedure TColoringLimits.SetStringValuesToSkip(const Value: TStrings);
begin
  FStringValuesToSkip.Assign(Value);
end;

procedure TColoringLimits.SetUpperLimit(const Value: TColoringLimit);
begin
  FUpperLimit.Assign(Value);
end;

function TColoringLimits.StoreIntegerSkipValues: boolean;
begin
  result := FIntegerValuesToSkip.Count > 0
end;

function TColoringLimits.StoreRealSkipValues: boolean;
begin
  result := FRealValuesToSkip.Count > 0
end;

procedure TColoringLimits.Update;
var
  TempReal: double;
  TempInteger: integer;
  TempBoolean: boolean;
  TempString: string;
begin
  if LowerLimit.UseLimit and UpperLimit.UseLimit then
  begin
    assert(LowerLimit.DataType = UpperLimit.DataType);
    case LowerLimit.DataType of
      rdtDouble:
        begin
          if LowerLimit.RealLimitValue > UpperLimit.RealLimitValue then
          begin
            TempReal := UpperLimit.RealLimitValue;
            UpperLimit.RealLimitValue := LowerLimit.RealLimitValue;
            LowerLimit.RealLimitValue := TempReal;
          end;
        end;
      rdtInteger:
        begin
          if LowerLimit.IntegerLimitValue > UpperLimit.IntegerLimitValue then
          begin
            TempInteger := UpperLimit.IntegerLimitValue;
            UpperLimit.IntegerLimitValue := LowerLimit.IntegerLimitValue;
            LowerLimit.IntegerLimitValue := TempInteger;
          end;
        end;
      rdtBoolean:
        begin
          if LowerLimit.BooleanLimitValue > UpperLimit.BooleanLimitValue then
          begin
            TempBoolean := UpperLimit.BooleanLimitValue;
            UpperLimit.BooleanLimitValue := LowerLimit.BooleanLimitValue;
            LowerLimit.BooleanLimitValue := TempBoolean;
          end;
        end;
      rdtString:
        begin
          if LowerLimit.StringLimitValue > UpperLimit.StringLimitValue then
          begin
            TempString := UpperLimit.StringLimitValue;
            UpperLimit.StringLimitValue := LowerLimit.StringLimitValue;
            LowerLimit.StringLimitValue := TempString;
          end;
        end;
    else
      Assert(False);
    end;
  end;
end;

function TColoringLimits.ValueOk(AValue: double): boolean;
var
  SkipIndex: Integer;
  SkipItem: TSkipReal;
  LocalEpsilon: Double;
begin
  result := True;
  if LogTransform and (AValue <= 0) then
  begin
    result := False;
    Exit;
  end;
  for SkipIndex := 0 to RealValuesToSkip.Count - 1 do
  begin
    SkipItem := RealValuesToSkip.Items[SkipIndex] as TSkipReal;
    if SkipItem.RealValue = AValue then
    begin
      result := False;
      Exit;
    end;
    if SkipItem.RealValue = 0 then
    begin
      LocalEpsilon := Epsilon;
    end
    else
    begin
      LocalEpsilon := Abs(SkipItem.RealValue)*Epsilon;
    end;
    if ((SkipItem.RealValue - LocalEpsilon) < AValue)
      and ((SkipItem.RealValue + LocalEpsilon) > AValue) then
    begin
      result := False;
      Exit;
    end;
  end;
end;

function TColoringLimits.ValueOk(AValue: integer): boolean;
var
  SkipIndex: Integer;
  SkipItem: TSkipInteger;
begin
  result := True;
  for SkipIndex := 0 to IntegerValuesToSkip.Count - 1 do
  begin
    SkipItem := IntegerValuesToSkip.Items[SkipIndex] as TSkipInteger;
    if SkipItem.IntegerValue = AValue then
    begin
      result := False;
      Exit;
    end;
  end;
end;

function TColoringLimits.ValueOk(const AValue: String): boolean;
begin
  result := StringValuesToSkip.IndexOf(AValue) < 0;
end;

{ TCustomTimeList }

function TCustomTimeList.GetUpToDate: boolean;
var
  Index: Integer;
begin
  result := FUpToDate;
  if result then
  begin
    for Index := 0 to Count - 1 do
    begin
      if Items[Index] <> nil then
      begin
        result := Items[Index].UpToDate;
        if not result then
        begin
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TCustomTimeList.SetUpToDate(const Value: boolean);
begin
  FUpToDate := Value;
  if not FUpToDate then
  begin
    if frmGoPhast.PhastModel <> nil then
    begin

      if frmGoPhast.PhastModel.TopTimeList = self then
      begin
        frmGoPhast.PhastGrid.NeedToRecalculateTopCellColors := True;
        frmGoPhast.ModflowGrid.NeedToRecalculateTopCellColors := True;
      end;
      if frmGoPhast.PhastModel.FrontTimeList = self then
      begin
        frmGoPhast.PhastGrid.NeedToRecalculateFrontCellColors := True;
        frmGoPhast.ModflowGrid.NeedToRecalculateFrontCellColors := True;
      end;
      if frmGoPhast.PhastModel.SideTimeList = self then
      begin
        frmGoPhast.PhastGrid.NeedToRecalculateSideCellColors := True;
        frmGoPhast.ModflowGrid.NeedToRecalculateSideCellColors := True;
      end;
    end;
  end;
end;

procedure TCustomTimeList.Invalidate;
var
  Model: TCustomModel;
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ATimeList: TCustomTimeList;
begin
  SetUpToDate(False);
  if FModel <> nil then
  begin
    Model := FModel as TCustomModel;
    if self = Model.ThreeDTimeList then
    begin
      if Model.Grid <> nil then
      begin
        Model.Grid.GridChanged;
      end
      else if Model.Mesh <> nil then
      begin
        (Model.Mesh as TSutraMesh3D).NeedToRecalculateFrontColors := True;
        (Model.Mesh as TSutraMesh3D).NeedToRecalculateTopColors := True;
        (Model.Mesh as TSutraMesh3D).MeshChanged;
      end;
    end;
    if (Name <> '') and (Model is TPhastModel) then
    begin
      LocalModel := TPhastModel(Model);
      if LocalModel.ChildModels <> nil then
      begin
        for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
        begin
          ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
          if ChildModel <> nil then
          begin
            ATimeList := ChildModel.GetTimeListByName(Name);
            if ATimeList <> nil then
            begin
              ATimeList.Invalidate;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TCustomTimeList.MaxValue(Time: double): string;
var
  TimeIndex: Integer;
  DataSet: TDataArray;
begin
  result := '?';
  if UpToDate then
  begin
    TimeIndex := FirstTimeGreaterThan(Time) - 1;
    if TimeIndex >= 0 then
    begin
      DataSet := Items[TimeIndex];
      if DataSet <> nil then
      begin
        result := DataSet.MaxValue;
      end;
    end;
  end;
end;

function TCustomTimeList.MinValue(Time: double): string;
var
  TimeIndex: Integer;
  DataSet: TDataArray;
begin
  result := '?';
  if UpToDate then
  begin
    TimeIndex := FirstTimeGreaterThan(Time) - 1;
    if TimeIndex >= 0 then
    begin
      DataSet := Items[TimeIndex];
      if DataSet <> nil then
      begin
        result := DataSet.MinValue;
      end;
    end;
  end;
end;

procedure TCustomTimeList.CheckSameModel(const Data: TDataArray);
begin
  Assert(Data <> nil);
  Assert(Model = Data.Model as TCustomModel);
end;

function TCustomTimeList.Add(const ATime: double;
  const Data: TDataArray): integer;
begin
  CheckSameModel(Data);
  result := IndexOf(ATime);
  if result >= 0 then
  begin
    if (FData[result] <> Data) then
    begin
      raise EInvalidTime.Create(StrInvalidTimeSpecifi);
    end;
    Exit;
  end;

  result := FTimes.Add(ATime);
  if result >= FData.Count then
  begin
    FData.Add(Data);
  end
  else
  begin
    FData.Insert(result, Data);
  end;

  if Data <> nil then
  begin
    Data.Limits := Limits;
    Data.Max := Max;
    Data.Min := Min;
    Data.CheckMax := CheckMax;
    Data.CheckMin := CheckMin;
  end;

  Invalidate;
end;

function TCustomTimeList.GetTimes(const Index: integer): double;
begin
  result := FTimes[Index];
end;

function TCustomTimeList.IndexOf(const ATime: double): integer;
begin
  result := FTimes.IndexOf(ATime);
end;

procedure TCustomTimeList.SetName(const Value: string);
begin
  FName := Value;
  Invalidate;
end;

function TCustomTimeList.GetBoundaryType: TBoundaryType;
begin
  if (Name = StrSpecifiedHead)
    or (Name = StrSpecifiedHeadSolution)
    then
  begin
    result := btPhastSpecifiedHead;
  end
  else if (Name = StrTopFluxBoundaryFlux)
    or (Name = StrFrontFluxBoundaryFlux)
    or (Name = StrSideFluxBoundaryFlux)
    or (Name = StrTopFluxBoundaryAssocSoln)
    or (Name = StrFrontFluxBoundaryAssocSoln)
    or (Name = StrSideFluxBoundaryAssocSoln)
    then
  begin
    Result := btPhastFlux;
  end
  else if (Name = StrTopLeakyBoundaryHead)
    or (Name = StrTopLeakyBoundaryAssocSoln)
    or (Name = StrFrontLeakyBoundaryHead)
    or (Name = StrFrontLeakyBoundaryAssocSoln)
    or (Name = StrSideLeakyBoundaryHead)
    or (Name = StrSideLeakyBoundaryAssocSoln)
    then
  begin
    Result := btPhastLeaky;
  end
  else if (Name = StrRiverHead)
    or (Name = StrRiverAssocSoln)
    then
  begin
    Result := btPhastRiver;
  end
  else if (Name = StrWellInjectionRate)
    or (Name = StrWellSolution)
    then
  begin
    Result := btPhastWell;
  end
  else if (Name = StrMODFLOWWellPumping) or (Pos('WEL ', Name) = 1)
    then
  begin
    Result := btMfWell;
  end
  else if (Name = StrMODFLOWGhbConductance)
    or (Name = StrMODFLOWGhbHead)
    or (Pos('GHB ', Name) = 1)
    then
  begin
    Result := btMfGhb;
  end
  else if (Name = StrMODFLOWDrainElevation)
    or (Name = StrMODFLOWDrainConductance)
    then
  begin
    Result := btMfDrn;
  end
  else if (Name = StrMODFLOWDrainReturnConductance)
    or (Name = StrMODFLOWDrainReturnElevation)
    or (Name = StrMODFLOWDrainReturnFraction)
    then
  begin
    Result := btMfDrt;
  end
  else if (Name = StrMODFLOWRiverConductance)
    or (Name = StrMODFLOWRiverStage)
    or (Name = StrMODFLOWRiverBottom)
    or (Pos('RIV ', Name) = 1)
    then
  begin
    Result := btMfRiv;
  end
  else if (Name = StrMODFLOWCHDStartingHead)
    or (Name = StrMODFLOWCHDEndingHead)
    or (Pos('CHD ', Name) = 1)
    then
  begin
    Result := btMfChd;
  end
  else if (Pos(StrMODFLOWEtsRateFraction, Name) = 1)
    or (Pos(StrMODFLOWEtsDepthFraction, Name) = 1)
    or (Name = StrMODFLOWEtsRate)
    or (Name = StrMODFLOWEtsDepth)
    or (Name = StrMODFLOWEtsSurface)
    or (Name = StrMODFLOWEtsLayer)
    then
  begin
    Result := btMfEts;
  end
  else if (Name = StrMODFLOWEvtRate)
    or (Name = StrMODFLOWEvtDepth)
    or (Name = StrMODFLOWEvtSurface)
    or (Name = StrMODFLOWEvtLayer)
    then
  begin
    Result := btMfEt;
  end
  else if (Name = StrMODFLOWRchRate)
    or (Name = StrMODFLOWRchLayer)
    or (Pos('RCH ', Name) = 1)
    then
  begin
    Result := btMfRch;
  end
  else if (Name = StrModflowSfrSegment)
    or (Name = StrModflowSfrReach)
    or (Name = StrModflowSfrIcalc)
    or (Name = StrModflowSfrReachLength)
    or (Name = StrModflowSfrStreamTop)
    or (Name = StrModflowSfrStreamSlope)
    or (Name = StrModflowSfrStreamThickness)
    or (Name = StrModflowSfrStreamK)
    or (Name = StrModflowSfrSatWatCont)
    or (Name = StrModflowSfrInitWatCont)
    or (Name = StrModflowSfrBrooksCorey)
    or (Name = StrModflowSfrVertK)
    or (Name = StrModflowSfrDownstreamSegment)
    or (Name = StrModflowSfrDiversionSegment)
    or (Name = StrModflowSfrIprior)
    or (Name = StrModflowSfrFlow)
    or (Name = StrModflowSfrRunoff)
    or (Name = StrModflowSfrPrecipitation)
    or (Name = StrModflowSfrEvapotranspiration)
    or (Name = StrModflowSfrChannelRoughness)
    or (Name = StrModflowSfrBankRoughness)
    or (Name = StrModflowSfrDepthCoefficient)
    or (Name = StrModflowSfrDepthExponent)
    or (Name = StrModflowSfrWidthCoefficient)
    or (Name = StrModflowSfrWidthExponent)
    or (Name = StrModflowSfrUpstreamHydraulicConductivity)
    or (Name = StrModflowSfrDownstreamHydraulicConductivity)
    or (Name = StrModflowSfrUpstreamWidth)
    or (Name = StrModflowSfrDownstreamWidth)
    or (Name = StrModflowSfrUpstreamThickness)
    or (Name = StrModflowSfrDownstreamThickness)
    or (Name = StrModflowSfrUpstreamElevation)
    or (Name = StrModflowSfrDownstreamElevation)
    or (Name = StrModflowSfrUpstreamDepth)
    or (Name = StrModflowSfrDownstreamDepth)
    or (Name = StrModflowSfrUpstreamSaturatedWaterContent)
    or (Name = StrModflowSfrDownstreamSaturatedWaterContent)
    or (Name = StrModflowSfrUpstreamInitialUnsaturatedWaterContent)
    or (Name = StrModflowSfrDownstreamInitialUnsaturatedWaterContent)
    or (Name = StrModflowSfrUpstreamBrooksCoreyExponent)
    or (Name = StrModflowSfrDownstreamBrooksCoreyExponent)
    or (Name = StrModflowSfrUpstreamMaxUnsaturatedKz)
    or (Name = StrModflowSfrDownstreamMaxUnsaturatedKz)
    then
  begin
    Result := btMfSfr;
  end
  else if (Name = StrModflowStrSegment)
    or (Name = StrModflowStrDownstreamSegment)
    or (Name = StrModflowStrDiversionSegment)
    or (Name = StrModflowStrReach)
    or (Name = StrSTRStreamTopElev)
    or (Name = StrSTRStreamBottomElev)
    or (Name = StrSTRStreamStage)
    or (Name = StrSTRStreamConductance)
    or (Name = StrSTRStreamFlow)
    or (Name = StrSTRStreamWidth)
    or (Name = StrSTRStreamSlope)
    or (Name = StrSTRStreamRoughness)
    then
  begin
    Result := btMfStr;
  end
  else if (Name = StrUzfInfiltrationRate)
    or (Name = StrUzfExtinctionDepth)
    or (Name = StrUzfWaterContent)
    or (Name = StrUzfEtDemand)
    then
  begin
    Result := btMfUzf;
  end
  else if (Name = StrMODFLOWHeadObservations)
    then
  begin
    Result := btMfObs;
  end
  else if (Name = StrMt3dTobConcObservations)
    then
  begin
    Result := btMtmsObs;
  end
  else if (Name = StrWellRadius)
    or (Name = StrSkinRadius)
    or (Name = StrSkinK)
    or (Name = StrB)
    or (Name = StrC)
    or (Name = StrP)
    or (Name = StrCellToWellConductance)
    or (Name = StrPartialPenetration)
    then
  begin
    Result := btMfMnw;
  end
  else if (Name = StrMT3DMSSSMConcentra)
    or (Name = StrMt3dSsmRechConcentrat)
    or (Name = StrMt3dSsmSinkConcentrat)
    then
  begin
    Result := btMt3dSsm;
  end
  else if (Name = StrSpecifiedPressure)
    or (Name = StrAssocPresConc)
    or (Name = StrAssocPresTemp)
    then
  begin
    Result := btSutraSpecifiedPressure;
  end
  else if (Name = StrSutraSpecifiedHead)
    or (Name = StrAssocHeadConc)
    then
  begin
    Result := btSutraSpecifiedHead;
  end
  else if (Name = StrSpecifiedTemp)
    or (Name = StrSpecifiedConc) then
  begin
    Result := btSutraSpecConcTemp;
  end
  else if (Name = StrFluidFlux)
   or (Name = StrFluxAssocPresConc)
   or (Name = StrAssocPresTemp) then
  begin
    Result := btSutraFluidFlux;
  end
  else if (Name = StrMassFlux)
    or (Name = StrEnergyFlux) then
  begin
    Result := btMassEnergyFlux;
  end
  else if (Name = StrLowerPressureValue)
    or (Name = StrLowerHeadValue)
    or (Name = StrHigherPressureValue)
    or (Name = StrHigherHeadValue)
    or (Name = StrLowerRateP)
    or (Name = StrHigherRateP)
    or (Name = StrLowerRateH)
    or (Name = StrHigherRateH)
    or (Name = StrLowerConcentrationP)
    or (Name = StrHigherConcentrationP)
    or (Name = StrLowerConcentrationH)
    or (Name = StrHigherConcentrationH)
    or (Name = StrLowerTemperature)
    or (Name = StrHigherTemperature)
    then
  begin
    Result := btSutraGeneralFlow;
  end
  else if (Name = StrLowerConcentrationValue)
    or (Name = StrHigherConcentrationValue)
    or (Name = StrLowerTemperatureValue)
    or (Name = StrHigherTemperatureValue)
    or (Name = StrMassFlowAtLowerConcentration)
    or (Name = StrMassFlowAtHigherConcentration)
    or (Name = StrEnergyFlowAtLowerTemperature)
    or (Name = StrEnergyFlowAtHigherTemperature)
    then
  begin
    Result := btSutraGenTransp;
  end
  else if (Name = StrMODFLOWFHBHeads)
    or (Name = StrMODFLOWFHBFlows) then
  begin
    Result := btMfFhb;
  end
  else if (Name = StrFarmID2)
    or (Name = StrFarmEvap)
    or (Name = StrFarmPrecip)
    or (Name = StrFarmCropID)
    or (Name = StrFarmMaxPumpRate)
    or (Name = StrFarmWellFarmID)
    or (Name = StrFarmWellPumpRequired)
    or (Name = StrLandUseID)
    or (Name = StrFMP4Efficiency)
    or (Name = StrFMP4EfficiencyImpr)
    or (Name = StrFMP4BareRunoffFra)
    or (Name = StrFMP4BarePrecipitat)
    or (Name = StrFMP4AddedDemandRu)
    or (Name = StrPotentialEvaporatio)
    or (Name = StrFMP4DirectRecharge)
    or (Name = StrFMP4PrecipitationP)
    or (Name = StrFMP4NonRoutedDeli)
    or (Name = StrFMP4LandUseAreaF)
    or (Name = StrFMP4CropCoefficien)
    or (Name = StrFMP4ConsumptiveUse)
    or (Name = StrFMP4Irrigation)
    or (Name = StrFMP4RootDepth)
    or (Name = StrFMP4TranspirationF)
    or (Name = StrFMP4EvaporationIrr)
    or (Name = StrFMP4FractionOfExc)
    or (Name = StrFMP4FractionOfExcIrrig)
    or (Name = StrFMP4AddedDemand)
    or (Pos('FMP4', Name) = 1)
    then
  begin
    Result := btMfFarm;
  end
  else if (Name = StrCfpRecharge) then
  begin
    Result := btCFP;
  end
  else if (Name = StrSWR_Rain)
    or (Name = StrSWR_Evap)
    or (Name = StrSWR_LatInflow)
    or (Name = StrSWR_Stage)
    or (Name = StrSWR_DirectRunoffReach)
    or (Name = StrSWR_DirectRunoffValue)
    or (Name = StrSWR_Vertical_Offset)
    or (Name = StrSWR_BoundaryType)
    or (Name = StrSWR_GeometryNumber)
    then
  begin
    Result := btSWR;
  end
  else if (Name = StrMNW1DesiredPumping)
    or (Name = StrMNW1WaterQuality)
    or (Name = StrMNW1WellRadius)
    or (Name = StrMNW1Conductance)
    or (Name = StrMNW1Skin)
    or (Name = StrMNW1LimitingWater)
    or (Name = StrMNW1ReferenceEleva)
    or (Name = StrMNW1WaterQualityG)
    or (Name = StrMNW1NonlinearLoss)
    or (Name = StrMNW1MinimumPumping)
    or (Name = StrMNW1ReactivationPu)
    then
  begin
    result := btMnw1;
  end
  else if Name = StrMt3dRechConcentrat then
  begin
    result := btMt3dRchConc;
  end
  else if Name = StrMt3dUnsatConcentrat then
  begin
    result := mt3dUnsatConc;
  end
  else if Name = StrMt3dSatConcentrat then
  begin
    result := mt3dSatConc;
  end
  else if (Name = StrRipGroundElevation)
    or (frmGoPhast.PhastModel.RipPlantGroups.IndexOfName(Name) >= 0) then
  begin
    result := btRip;
  end
  else if (Name = StrSFR6Inflow)
    or (Name = StrSFR6Rainfall)
    or (Name = StrSFR6Evaporation)
    or (Name = StrSFR6Runoff)
    or (Name = StrSFR6UpstreamFracti)
    or (Name = StrSFR6Stage)
    or (Name = StrSFR6Roughness)
    or (Name = StrSFR6StreamStatus)
    or (Name = StrSFR6ReachNumber)
    or (Pos('SFR MF6', Name) = 1)
    then
  begin
    result := btSfr_MF6;
  end
  else if (Name = StrMawWellElevation)
    or (Name = StrMAWWellConductance)
    or (Name = StrMAWWellRedLength)
    or (Name = StrMAWWellRate)
    or (Name = StrMAWWellHead)
    or (Name = StrMAWWellLimit)
    or (Name = StrMAWWellMinimumPum)
    or (Name = StrMAWWellMaximumPum)
    or (Name = StrMAWPumpElevation)
    or (Name = StrMAWScalingLength)
    or (Pos('MAW MF6', Name) = 1)
    then
  begin
    result := btMAW;
  end
  else if (Name = StrUzfMf6InfiltrationRate)
    or (Name = StrUzfMf6PotentialET)
    or (Name = StrUzfMf6ExtinctionDepth)
    or (Name = StrUzfMf6ExtinctionWaterContent)
    or (Name = StrUzfMf6AirEntryPotential)
    or (Name = StrUzfMf6RootPotential)
    or (Name = StrUzfMf6RootActivity)
    or (Pos('UZF MF6', Name) = 1)
    then
  begin
    result := btUzfMf6;
  end
  else if (Name = StrCSUBStressOffset) then
  begin
    result := btCSub;
  end
  else if Pos(StrSFT, Name) = 1 then
  begin
    result := btSft;
  end
  else if Name = StrMVRValue then
  begin
    result := btMvr;
  end
  else if (Name = StrTransientKx)
    or (Name = StrTransientKy)
    or (Name = StrTransientKz)
    or (Name = StrTransientKyKx)
    or (Name = StrTransientKzKx)
    or (Pos(StrTransientKy, Name) > 0)
    or (Pos(StrTransientKz, Name) > 0)
    then
  begin
    result := btTransK;
  end
  else if (Name = StrTransientSS)
    or (Name = StrTransientSY)
    then
  begin
    result := btTransS;
  end
  else if Pos('CNC', Name) = 1 then
  begin
    result := btCNC;
  end
  else if Pos('SRC', Name) = 1 then
  begin
    result := btSRC;
  end

  else
  begin
    result := btUndefined;
  end;
end;

function TCustomTimeList.GetClassification: string;
begin
  result := FClassification;
  if result = '' then
  begin
    case BoundaryType of
      btUndefined:
        begin
          result := StrUndefined;
        end;
      btPhastSpecifiedHead:
        begin
          result := StrPHASTSpecifiedHead;
        end;
      btPhastFlux:
        begin
          Result := StrPHASTFlux;
        end;
      btPhastLeaky:
        begin
          Result := StrPHASTLeaky;
        end;
      btPhastRiver:
        begin
          Result := StrPHASTRiver;
        end;
      btPhastWell:
        begin
          Result := StrPHASTWell;
        end;
      btMfWell:
        begin
          Result := StrMODFLOWWell;
        end;
      btMfGhb:
        begin
          Result := StrMODFLOWGeneralHead;
        end;
      btMfDrn:
        begin
          Result := StrMODFLOWDrain;
        end;
      btMfDrt:
        begin
          Result := StrMODFLOWDrainReturn;
        end;
      btMfRiv:
        begin
          Result := StrMODFLOWRiver;
        end;
      btMfChd:
        begin
          Result := StrMODFLOWCHD;
        end;
      btMfEts:
        begin
          Result := StrMODFLOWEvapoSegments;
        end;
      btMfEt:
        begin
          Result := StrMODFLOWEvapotranspi;
        end;
      btMfRch:
        begin
          Result := StrMODFLOWRecharge;
        end;
      btMfSfr:
        begin
          Result := StrMODFLOWStreamflowR;
        end;
      btMfStr:
        begin
          Result := StrMODFLOWStream;
        end;
      btMfUzf:
        begin
          Result := StrMODFLOWUnsaturated;
        end;
      btMfObs:
        begin
          Result := StrMODFLOWObservations;
        end;
      btMfMnw:
        begin
          Result := StrMODFLOWMultinodeWe;
        end;
      btMt3dSsm:
        begin
          Result := StrMT3DMSSinkAndSour;
        end;
      btSutraSpecifiedPressure:
        begin
          result := StrSUTRASpecifiedPres;
        end;
      btSutraSpecifiedHead:
        begin
          result := StrSUTRASpecifiedHead;
        end;
      btSutraSpecConcTemp:
        begin
          result := StrSUTRASpecifiedConcTemp;
        end;
      btSutraFluidFlux:
        begin
          result := StrSutraFluidFlux;
        end;
      btMassEnergyFlux:
        begin
          result := StrMassEnergyFlux;
        end;
      btSutraGeneralFlow:
        begin
          result := StrSutraGeneralizedFlow;
        end;
      btSutraGenTransp:
        begin
          result := StrSutraGeneralizedTransport;
        end;
      btMfFhb:
        begin
          result := StrMODFLOWFHB;
        end;
      btMfFarm:
        begin
          result := StrMODFLOW_Farm;
        end;
      btCFP:
        begin
          result := StrMODFLOW_CFP;
        end;
      btSWR:
        begin
          Result := StrMODFLOWSWR;
        end;
      btMnw1:
        begin
          Result := StrMODFLOWMNW1;
        end;
      btMtmsObs:
        begin
          Result := StrMt3dmsObservations;
        end;
      btRIP:
        begin
          Result := StrMODFLOWRIP;
        end;
      btMt3dRchConc:
        begin
          Result := StrMt3dRechConcentrat;
        end;
      mt3dUnsatConc:
        begin
          Result := StrMt3dUnsatConcentrat;
        end;
      mt3dSatConc:
        begin
          Result := StrMt3dSatConcentrat;
        end;
      btSfr_MF6:
        begin
          Result := StrMODFLOW6SFR;
        end;
      btMAW:
        begin
          Result := StrMODFLOW6MAW;
        end;
      btUzfMf6:
        begin
          Result := StrMODFLOW6UZF;
        end;
      btSft:
        begin
          Result := StrStreamTransport;
        end;
      btCSub:
        begin
          result := StrMODFLOW6CSUB;
        end;
      btMvr:
        begin
          result := StrMODFLOW6MVR;
        end;
      btCnc:
        begin
          result := StrMODFLOW6CNC;
        end;
      btSrc:
        begin
          result := StrMODFLOW6SRC;
        end;
      btTransK:
        begin
          result := StrMODFLOWTVK;
        end;
      btTransS:
        begin
          result := StrMODFLOWTVS;
        end;
      else
        Assert(False);
    end;
    if result <> StrUndefined then
    begin
      FClassification := result;
    end;
  end;
end;

function TCustomTimeList.GetCount: integer;
begin
  result := FTimes.Count;
end;

function TCustomTimeList.FirstTimeGreaterThan(const ATime: double): integer;
var
  Top, Bottom, Middle: integer;
begin
  if Count = 0 then
  begin
    result := -1
  end
  else if Times[0] > ATime then
  begin
    result := 0;
  end
  else if Times[Count - 1] <= ATime then
  begin
    result := Count;
  end
  else
  begin
    Top := Count - 1;
    Bottom := 0;
    while Top - Bottom > 1 do
    begin
      Middle := (Top + Bottom) div 2;
      if Times[Middle] <= ATime then
      begin
        Bottom := Middle;
      end
      else
      begin
        Top := Middle;
      end;
    end; // While Top - Bottom > 1 do
    result := Top;
  end;
end;

procedure TCustomTimeList.FreeItem(Index: integer);
begin
  FData[Index] := nil;
end;

function TCustomTimeList.IndexOfDataSet(const Data: TDataArray): integer;
begin
  result := FData.IndexOf(Data);
end;

procedure TCustomTimeList.SetOrientation(const Value: TDataSetOrientation);
begin
  FOrientation := Value;
  Invalidate;
end;

procedure TCustomTimeList.Changed(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    Invalidate;
  end;
  frmGoPhast.ScreenObjectsChanged(Sender);
end;

procedure TCustomTimeList.Clear;
begin
  FTimes.Clear;
  FData.Clear;
  Invalidate;
end;

constructor TCustomTimeList.Create(AModel: TBaseModel);
begin
  inherited Create;
  Assert((AModel = nil) or (AModel is TCustomModel));
  FModel := AModel;
  FLimits := TColoringLimits.Create;
  FTimes := TRealList.Create;
  FTimes.Sorted := True;
  FData := TObjectList.Create;
  FOrientation := dso3D;
  if FModel <> nil then
  begin
    Invalidate
  end;
end;

destructor TCustomTimeList.Destroy;
var
  Model: TCustomModel;
begin
  if (FModel <> nil) and not (csDestroying in Application.ComponentState) then
  begin
    Model := FModel as TCustomModel;
    if Model.TopTimeList = self then
    begin
      Model.TopTimeList := nil;
    end;
    if Model.FrontTimeList = self then
    begin
      Model.FrontTimeList := nil;
    end;
    if Model.SideTimeList = self then
    begin
      Model.SideTimeList := nil;
    end;
    if Model.ThreeDTimeList = self then
    begin
      Model.ThreeDTimeList := nil;
    end;
  end;
  FLimits.Free;
  FTimes.Free;
  FData.Free;
  inherited;
end;

function TCustomTimeList.GetItems(const Index: integer): TDataArray;
begin
  result := FData[Index];
  if result <> nil then
  begin
    result.ATimeList := self;
  end;
end;

procedure TCustomTimeList.SetCheckMax(const Value: boolean);
begin
  FCheckMax := Value;
end;

procedure TCustomTimeList.SetCheckMin(const Value: boolean);
begin
  FCheckMin := Value;
end;

procedure TCustomTimeList.SetClassification(const Value: string);
begin
  FClassification := Value;
end;

procedure TCustomTimeList.SetItems(const Index: integer;
  const Value: TDataArray);
begin
  if FData[Index] <> Value then
  begin
    FData[Index] := Value;
    Invalidate;
  end;
end;

procedure TCustomTimeList.SetLimits(const Value: TColoringLimits);
begin
  FLimits.Assign(Value);
  UpdateLimits;
end;

procedure TCustomTimeList.SetMax(const Value: double);
begin
  FMax := Value;
end;

procedure TCustomTimeList.SetMin(const Value: double);
begin
  FMin := Value;
end;

procedure TCustomTimeList.UpDateLimits;
var
  Index: integer;
  DataSet: TDataArray;
begin
  Limits.Update;
  for Index := 0 to Count - 1 do
  begin
    DataSet := Items[Index];
    if DataSet <> nil then
    begin
      DataSet.Limits := Limits;
    end;
  end;
end;

function TCustomTimeList.UsedByModel: boolean;
begin
  if Assigned(FOnTimeListUsed) then
  begin
    result := FOnTimeListUsed(self);
  end
  else
  begin
    result := True;
  end;
end;

function TCustomSparseDataSet.GetIsValue(const Layer, Row, Col: Integer): boolean;
begin
  result := True;
  CheckRestoreData;
  if (FPriorLayer = Layer)
    and (FPriorRow = Row)
    and (FPriorCol = Col) then
  begin
    result := FPriorResult;
    Exit;
  end;

  if (BoundaryTypeDataSet <> nil) and (BoundaryTypeDataSet <> self) then
  begin
    result := BoundaryTypeDataSet.IsValue[Layer, Row, Col];
  end;
  result := result and FAnnotation.IsValue[Layer, Row, Col];
  FPriorLayer := Layer;
  FPriorRow := Row;
  FPriorCol := Col;
  FPriorResult := result;

end;

procedure TCustomSparseDataSet.SetIsValue(const Layer, Row, Col: Integer;
      const Value: boolean);
begin
  if (BoundaryTypeDataSet <> nil) and (BoundaryTypeDataSet <> self) then
  begin
    BoundaryTypeDataSet.IsValue[Layer, Row, Col] := Value;
  end;
end;

procedure TCustomSparseDataSet.UpdateDimensions(NumberOfLayers, NumberOfRows,
  NumberOfColumns: integer; ForceResize: boolean);
var
  OldLayerCount: integer;
  OldRowCount: integer;
  OldColumnCount: integer;
begin
  OldLayerCount := LayerCount;
  OldRowCount := RowCount;
  OldColumnCount := ColumnCount;
  inherited;
  if ((OldLayerCount > MaxSmallArraySize) <> (NumberOfLayers > MaxSmallArraySize))
    or ((OldRowCount > MaxSmallArraySize) <> (NumberOfRows > MaxSmallArraySize))
    or ((OldColumnCount > MaxSmallArraySize) <> (NumberOfColumns > MaxSmallArraySize))
    then
  begin
    FAnnotation.Free;
    FAnnotation := T3DSparseStringArray.Create(GetQuantum(NumberOfLayers),
      GetQuantum(NumberOfRows), GetQuantum(NumberOfColumns));
  end;
end;

procedure TDataArray.Assign(Source: TPersistent);
var
  SourceDataArray: TDataArray;
begin
  if (Source is TDataArray) then
  begin
    SourceDataArray := TDataArray(Source);
    PestParametersUsed := SourceDataArray.PestParametersUsed;
    TemplateNeeded := SourceDataArray.TemplateNeeded;
    if SourceDataArray.FReadDataFromFile then
    begin
      FReadDataFromFile := True;
      FTempFileName := SourceDataArray.FTempFileName;
      UpToDate := True;
      FDataCached := True;
      FCleared := True;
    end;
  end
  else
  begin
    inherited;
  end;
end;

procedure TDataArray.AssignModel(AModel: IModelMuseModel);
begin
  FModel := AModel;
end;

procedure TDataArray.AssignProperties(Source: TDataArray);
var
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildItem: TChildModelItem;
  ChildDataArray: TDataArray;
begin
  Name := Source.Name;
  CheckMax := Source.CheckMax;
  CheckMin := Source.CheckMin;
  Lock := Source.Lock;
  Max := Source.Max;
  Min := Source.Min;
  DataType := Source.DataType;
  EvaluatedAt := Source.EvaluatedAt;
  Orientation := Source.Orientation;
  TwoDInterpolator := Source.TwoDInterpolator;
  Units := Source.Units;
  ContourInterval := Source.ContourInterval;
  if (FModel <> nil) and (FModel is TPhastModel) then
  begin
    LocalModel := TPhastModel(FModel);
    for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
    begin
      ChildItem := LocalModel.ChildModels[ChildIndex];
      if ChildItem.ChildModel <> nil then
      begin
        ChildDataArray := ChildItem.ChildModel.DataArrayManager.
          GetDataSetByName(Source.Name);
        ChildDataArray.AssignProperties(Source);
      end;
    end;
  end;
end;

procedure TDataArray.AssignValuesWithScreenObjects;
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
begin
  if not ParameterUsed then
  begin
    for ScreenObjectIndex := 0 to TCustomModel(FModel).ScreenObjectCount - 1 do
    begin
      AScreenObject := TCustomModel(FModel).ScreenObjects[ScreenObjectIndex];
      if not AScreenObject.Deleted
        and AScreenObject.UsedModels.UsesModel(Model as TCustomModel) then
      begin
        AScreenObject.AssignValuesToDataSet(self, FModel as TCustomModel, UseLgrEdgeCells);
      end;
    end;
  end;
end;

function TDataArray.GetParamDataSetName: string;
begin
  result := Format(StrSParameterNames, [Name]);
end;

function TDataArray.GetPestArrayFileNames: TStringList;
begin
  if FPestArrayFileNames = nil then
  begin
    FPestArrayFileNames := TStringList.Create;
  end;
  result := FPestArrayFileNames;
end;

procedure TDataArray.CacheData;
var
  MemStream: TMemoryStream;
  Compressor: TCompressionStream;
  TempStream: TMemoryStream;
begin
  if FSuppressCache then
  begin
    Exit;
  end;
  if UpToDate then
  begin
    if not FCleared then
    begin
      FCachedLayerCount := FLayerCount;
      FCachedRowCount := FRowCount;
      FCachedColumnCount := FColumnCount;
    end;
    if not FDataCached and (IsUniform <> iuTrue) then
    begin
      if FTempFileName = '' then
      begin
        FTempFileName := TempFileName;
      end;
      MemStream := TMemoryStream.Create;
      try
        Compressor := TCompressionStream.Create(clDefault, MemStream);
        TempStream := TMemoryStream.Create;
        try
          MemStream.Position := 0;
          StoreData(TempStream);
          TempStream.SaveToStream(Compressor);
        finally
          Compressor.Free;
          TempStream.Free;
        end;
        MemStream.Position := 0;
        ZipAFile(FTempFileName, MemStream);
      finally
        MemStream.Free;
      end;
      FDataCached := True;
    end;
    Clear;
    FCleared := True;
  end;
end;

procedure TDataArray.RestoreArraySize;
//var
//  Grid: TCustomModelGrid;
begin
  if FCleared then
  begin
    (Model as TCustomModel).UpdateDataArrayDimensions(self);
//    Grid := (Model as TCustomModel).Grid;
//    UpDateDimensions(Grid.LayerCount, Grid.RowCount, Grid.ColumnCount);
    SetDimensions(False);
  end;
end;

procedure TDataArray.CheckIfUniform;
var
  FirstValueFound: boolean;
  ColLimit: Integer;
  RowLimit: Integer;
  LayerLimit: Integer;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  if not UpToDate then
  begin
    FIsUniform := iuUnknown;
    Exit;
  end;
  if FCleared then
  begin
    Exit;
  end;
  FUniformAnnotation := '';
  FUniformStringValue := '';
  FUniformIntegerValue := 0;
  FUniformBooleanValue := False;
  FUniformRealValue := 0;

  FirstValueFound := False;
  GetLimits(ColLimit, RowLimit, LayerLimit);

  for LayerIndex := 0 to LayerLimit do
  begin
    for RowIndex := 0 to RowLimit do
    begin
      for ColIndex := 0 to ColLimit do
      begin
        if IsValue[LayerIndex, RowIndex, ColIndex] then
        begin
          if FirstValueFound then
          begin
            case DataType of
              rdtDouble:
                begin
                  if FUniformRealValue <>
                    RealData[LayerIndex, RowIndex, ColIndex] then
                  begin
                    FIsUniform := iuFalse;
                    Exit;
                  end;
                end;
              rdtInteger:
                begin
                  if FUniformIntegerValue <>
                    IntegerData[LayerIndex, RowIndex, ColIndex] then
                  begin
                    FIsUniform := iuFalse;
                    Exit;
                  end;
                end;
              rdtBoolean:
                begin
                  if FUniformBooleanValue <>
                    BooleanData[LayerIndex, RowIndex, ColIndex] then
                  begin
                    FIsUniform := iuFalse;
                    Exit;
                  end;
                end;
              rdtString:
                begin
                  if FUniformStringValue <>
                    StringData[LayerIndex, RowIndex, ColIndex] then
                  begin
                    FIsUniform := iuFalse;
                    Exit;
                  end;
                end;
            end;
            if FUniformAnnotation <>
              Annotation[LayerIndex, RowIndex, ColIndex] then
            begin
              FIsUniform := iuFalse;
              Exit;
            end;
          end
          else
          begin
            FirstValueFound := True;
            case DataType of
              rdtDouble:
                begin
                  FUniformRealValue :=
                    RealData[LayerIndex, RowIndex, ColIndex];
                end;
              rdtInteger:
                begin
                  FUniformIntegerValue :=
                    IntegerData[LayerIndex, RowIndex, ColIndex];
                end;
              rdtBoolean:
                begin
                  FUniformBooleanValue :=
                    BooleanData[LayerIndex, RowIndex, ColIndex];
                end;
              rdtString:
                begin
                  FUniformStringValue :=
                    StringData[LayerIndex, RowIndex, ColIndex];
                end;
            end;
            FUniformAnnotation := Annotation[LayerIndex, RowIndex, ColIndex];
          end;
        end
        else
        begin
          FIsUniform := iuFalse;
          Exit;
        end;
      end;
    end;
  end;
  FIsUniform := iuTrue;
end;

procedure TDataArray.CheckRestoreData;
begin
  if UpToDate and FDataCached and FCleared and (IsUniform <> iuTrue) then
  begin
    RestoreArraySize;
    RestoreData;
  end;
end;

procedure TDataArray.ReadCompressedData(Stream: TStream);
var
  MemStream: TMemoryStream;
  DecompressionStream: TDecompressionStream;
  Size: Int64;
//  Data: array of Byte;
  StoredLayerCount: integer;
  StoredRowCount: integer;
  StoredColumnCount: integer;
begin
  FCleared := True;
  MemStream := TMemoryStream.Create;
  try
    Stream.Read(StoredLayerCount, SizeOf(StoredLayerCount));
    Stream.Read(StoredRowCount, SizeOf(StoredRowCount));
    Stream.Read(StoredColumnCount, SizeOf(StoredColumnCount));
    UpDateDimensions(StoredLayerCount, StoredRowCount, StoredColumnCount);
    SetDimensions(False);

    Stream.Read(Size, SizeOf(Size));
//    SetLength(Data, Size);
//    Stream.ReadBuffer(Data[0], Size);
//    MemStream.WriteBuffer(Data[0], Size);
    MemStream.CopyFrom(Stream, Size);
    MemStream.Position := 0;

    DecompressionStream := nil;
    try
      try
        DecompressionStream := TDecompressionStream.Create(MemStream);
        ReadData(DecompressionStream);
      Except
          FDataCached := False;
          FCleared := False;
          FIsUniform := iuFalse;
          UpToDate := False;
          FReadDataFromFile := False;
          Beep;
          MessageDlg(Format(StrThereWasAProblem, [Name]), mtError, [mbOK], 0);
          Exit;
      end;
    finally
      DecompressionStream.Free;
    end;
  finally
    MemStream.Free;
  end;
  FDataCached := False;
  FCleared := False;
  FIsUniform := iuFalse;
  UpToDate := True;
  CacheData;
  FReadDataFromFile := True;

end;

procedure TDataArray.WriteCompressedData(Stream: TStream);
var
  MemStream: TMemoryStream;
  Size: Int64;
//  Data: array of Byte;
begin
  Assert(UpToDate);
  Assert(IsUniform <> iuTrue);

  CacheData;
  Assert(FTempFileName <> '');
  Stream.Write(FCachedLayerCount, SizeOf(FCachedLayerCount));
  Stream.Write(FCachedRowCount, SizeOf(FCachedRowCount));
  Stream.Write(FCachedColumnCount, SizeOf(FCachedColumnCount));

  MemStream := TMemoryStream.Create;
  try
    ExtractAFile(FTempFileName, MemStream);
    Size := MemStream.Size;
    Stream.Write(Size, SizeOf(Size));
    Stream.CopyFrom(MemStream, MemStream.Size)
//    SetLength(Data, Size);
//    MemStream.ReadBuffer(Data[0], Size);
//    Stream.WriteBuffer(Data[0], Size);
  finally
    MemStream.Free;
  end;
  CacheData;
end;

procedure TDataArray.DefineProperties(Filer: TFiler);
  function StoreCompressedData: boolean;
  var
    Model: TCustomModel;
    LayerLimit: Integer;
    RowLimit: Integer;
    ColLimit: Integer;
    Count: Integer;
  begin
    result := UpToDate and (IsUniform <> iuTrue)
      and (frmGoPhast.PhastModel.SaveDataSetValues = sdsvAlways);
    if result
      and ((self is (TCustomSparseDataSet))
      or (self is (TSparseArrayPhastInterpolationDataSet))) then
    begin
      CountValues(LayerLimit, RowLimit, ColLimit, Count);
      result := Count > 0;
    end;

    if result then
    begin
      Model := FModel as TCustomModel;
      result := Model.DataArrayManager.StoreCachedData;
    end;
  end;
begin
//  inherited;
  Filer.DefineBinaryProperty('CompressedData', ReadCompressedData,
    WriteCompressedData, StoreCompressedData);
end;

procedure TDataArray.RestoreData;
var
  MemStream: TMemoryStream;
  DecompressionStream: TDecompressionStream;
begin
  Assert(FDataCached);
  Assert(FCleared);
  MemStream := TMemoryStream.Create;
  try
    ExtractAFile(FTempFileName, MemStream);
    DecompressionStream := TDecompressionStream.Create(MemStream);
    try
      ReadData(DecompressionStream);
    finally
      DecompressionStream.Free;
    end;
  finally
    MemStream.Free;
  end;
  FCleared := False;
end;

procedure TDataArray.RestoreSubscription(Sender: TObject; const AName: string);
var
  Model: TCustomModel;
  ObservedItem: TObserver;
begin
  Model := FModel as TCustomModel;
  ObservedItem := Model.GetObserverByName(AName);
  Assert(ObservedItem <> nil);
  ObservedItem.TalksTo(self);
  Invalidate;
end;

{ TContours }

procedure TContours.Assign(Source: TPersistent);
var
  SourceContours: TContours;
begin
  if Source is TContours then
  begin
    SourceContours := TContours(Source);
    AutomaticColors := SourceContours.AutomaticColors;
    SpecifyContours := SourceContours.SpecifyContours;
    ContourValues := Copy(SourceContours.ContourValues);
    LineThicknesses := Copy(SourceContours.LineThicknesses);
    ContourColors := Copy(SourceContours.ContourColors);
    LogTransform := SourceContours.LogTransform;
    ContourStringValues := SourceContours.ContourStringValues;
  end
  else
  begin
    inherited;
  end;
end;

constructor TContours.Create;
begin
  FAutomaticColors := True;
  FContourStringValues := TStringList.Create;
end;

procedure TContours.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('ContourValues', ReadContourValues,
    WriteContourValues, Count > 0);
  Filer.DefineProperty('LineThicknesses', ReadLineThicknesses,
    WriteLineThicknesses, Count > 0);
  Filer.DefineProperty('ContourColors', ReadContourColors,
    WriteContourColors, Count > 0);

end;

destructor TContours.Destroy;
begin
  FContourStringValues.Free;
  inherited;
end;

function TContours.GetCount: integer;
begin
  result := Length(FContourValues);
end;

procedure TContours.ReadContourColors(Reader: TReader);
var
  Index : integer;
begin
  Reader.ReadListBegin;
  Index := 0;
  while not Reader.EndOfList do
  begin
    ContourColors[Index] := Reader.ReadInt64;
    Inc(Index);
  end;
  Reader.ReadListEnd;
end;

procedure TContours.ReadContourValues(Reader: TReader);
var
  Index : integer;
begin
  Reader.ReadListBegin;
  Index := 0;
  while not Reader.EndOfList do
  begin
    ContourValues[Index] := Reader.ReadFloat;
    Inc(Index);
  end;
  Reader.ReadListEnd;
end;

procedure TContours.ReadLineThicknesses(Reader: TReader);
var
  Index : integer;
begin
  Reader.ReadListBegin;
  Index := 0;
  while not Reader.EndOfList do
  begin
    LineThicknesses[Index] := Reader.ReadFloat;
    Inc(Index);
  end;
  Reader.ReadListEnd;
end;

procedure TContours.SetContourStringValues(const Value: TStringList);
begin
  FContourStringValues.Assign(Value);
end;

procedure TContours.SetCount(const Value: integer);
begin
  if Value <> Count then
  begin
    SetLength(FContourValues, Value);
    SetLength(FContourColors, Value);
    SetLength(FLineThicknesses, Value);
  end;
end;

procedure TContours.Sort;
var
  List: TContourItemObjectList;
  index: Integer;
  AnItem: TContourItem;
begin
  List := TContourItemObjectList.Create;
  try
    List.Capacity := Count;
    for index := 0 to Count - 1 do
    begin
      AnItem := TContourItem.Create;
      AnItem.Value := ContourValues[index];
      AnItem.Color := ContourColors[index];
      AnItem.LineThickness := LineThicknesses[index];
      List.Add(AnItem);
    end;
    List.Sort(TContourComparer.Construct(
        function (const L, R: TContourItem): Integer
        begin
          result := Sign(L.Value - R.Value);
        end));
    for index := 0 to Count - 1 do
    begin
      AnItem := List[index];
      ContourValues[index] := AnItem.Value;
      ContourColors[index] := AnItem.Color;
      LineThicknesses[index] := AnItem.LineThickness;
    end;
  finally
    List.Free;
  end;
end;

procedure TContours.WriteContourColors(Writer: TWriter);
var
  Index: Integer;
begin
  Writer.WriteListBegin;
  for Index := 0 to Count - 1 do
  begin
    Writer.WriteInteger(ContourColors[Index]);
  end;
  Writer.WriteListEnd;
end;

procedure TContours.WriteContourValues(Writer: TWriter);
var
  Index: Integer;
begin
  Writer.WriteListBegin;
  for Index := 0 to Count - 1 do
  begin
    Writer.WriteFloat(ContourValues[Index]);
  end;
  Writer.WriteListEnd;
end;

procedure TContours.WriteLineThicknesses(Writer: TWriter);
var
  Index: Integer;
begin
  Writer.WriteListBegin;
  for Index := 0 to Count - 1 do
  begin
    Writer.WriteFloat(LineThicknesses[Index]);
  end;
  Writer.WriteListEnd;
end;

procedure TCustom2DInterpolater.EvaluateExpression(Compiler: TRbwParser;
  var Expression: TExpression; AScreenObject: TObject);
var
  DI: Integer;
  IsBoundary: Boolean;
  ScreenObject: TScreenObject;
begin
  try
    Expression.Evaluate;
  except
    on E: ERbwParserError do
    begin
      ScreenObject := AScreenObject as TScreenObject;
      DI := ScreenObject.IndexOfDataSet(DataSet);
      if DI >= 0 then
      begin
        IsBoundary := False;
      end
      else
      begin
        IsBoundary := True;
        DI := ScreenObject.IndexOfBoundaryDataSet(DataSet);
      end;
      ResetScreenObjectFunction(DI, ScreenObject, Compiler, DataSet.DataType,
        E.Message, IsBoundary, Expression.Decompile);
      Expression := Compiler.CurrentExpression;
      Expression.Evaluate;
    end;
  end;
end;

{ TSkipReal }

procedure TSkipReal.Assign(Source: TPersistent);
begin
  if Source is TSkipReal then
  begin
    RealValue := TSkipReal(Source).RealValue
  end
  else
  begin
    inherited;
  end;
end;

procedure TSkipReal.SetRealValue(const Value: double);
begin
  if FRealValue <> Value then
  begin
    FRealValue := Value;
    Changed(False);
  end;
end;

{ TSkipInteger }

procedure TSkipInteger.Assign(Source: TPersistent);
begin
  if Source is TSkipInteger then
  begin
    IntegerValue := TSkipInteger(Source).IntegerValue
  end
  else
  begin
    inherited;
  end;
end;

procedure TSkipInteger.SetIntegerValue(const Value: integer);
begin
  if FIntegerValue <> Value then
  begin
    FIntegerValue := Value;
    Changed(False);
  end;
end;

{ TSkipRealCollection }

constructor TSkipRealCollection.Create;
begin
  inherited Create(TSkipReal);
end;

function TSkipRealCollection.IndexOf(AValue: double): integer;
var
  Index: Integer;
  AnItem: TSkipReal;
begin
  result := -1;
  for Index := 0 to Count - 1 do
  begin
    AnItem := Items[Index] as TSkipReal;
    if AnItem.RealValue = AValue then
    begin
      result := Index;
      Exit; 
    end;
  end;
end;

{ TSkipIntegerCollection }

constructor TSkipIntegerCollection.Create;
begin
  inherited Create(TSkipInteger);
end;

{ TTransientRealSparseDataSet }
procedure TTransientRealSparseDataSet.UpdateNotifiers;
begin
  // do nothing
//  inherited;
end;

{ TTransientIntegerSparseDataSet }

procedure TTransientIntegerSparseDataSet.UpdateNotifiers;
begin
  // do nothing
//  inherited;
end;

procedure TCustomSkipCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(OnChange) then
  begin
    OnChange(self);
  end;
end;

{ TTempDataArrayStorage }

constructor TTempDataArrayStorage.Create;
begin
  FModelList := TList.Create;
  FDataArrayList := TObjectList.Create;
end;

destructor TTempDataArrayStorage.Destroy;
begin
  FDataArrayList.Free;
  FModelList.Free;
  inherited;
end;

function TTempDataArrayStorage.GetDataArray(Model: TBaseModel;
  Index: integer): TDataArray;
begin
  result := DataArrayList[Model][Index];
end;

function TTempDataArrayStorage.GetDataArrayList(Model: TBaseModel): TDataArrayObjectList;
var
  Index: Integer;
begin
  Assert(Model <> nil);
  Index := FModelList.IndexOf(Model);
  if Index < 0 then
  begin
    Index := FModelList.Add(Model);
    FDataArrayList.Add(TDataArrayObjectList.Create);
  end;
  result := FDataArrayList[Index];
  result.OwnsObjects := OwnsDataArrays;
end;

procedure TTempDataArrayStorage.SetOwnsDataArrays(const Value: boolean);
var
  Index: Integer;
  List: TDataArrayObjectList;
begin
  FOwnsDataArrays := Value;
  for Index := 0 to FDataArrayList.Count - 1 do
  begin
    List := FDataArrayList[Index];
    List.OwnsObjects := FOwnsDataArrays;
  end;
end;

{ TCustomBoundaryRealSparseDataSet }

procedure TCustomBoundaryRealSparseDataSet.AddDataArray(DataArray: TDataArray);
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  DataValue: Double;
  DataAnnotation: string;
begin
  for LayerIndex := 0 to DataArray.LayerCount - 1 do
  begin
    for RowIndex := 0 to DataArray.RowCount - 1 do
    begin
      for ColIndex := 0 to DataArray.ColumnCount - 1 do
      begin
        if DataArray.IsValue[LayerIndex, RowIndex, ColIndex] then
        begin
          DataValue := DataArray.RealData[LayerIndex, RowIndex, ColIndex];
          DataAnnotation :=
            DataArray.Annotation[LayerIndex, RowIndex, ColIndex];
          AddDataValue(DataAnnotation, DataValue,
            ColIndex, RowIndex, LayerIndex);
        end;
      end;
    end;
  end;
end;

procedure TCustomBoundaryRealSparseDataSet.AddDataValue(
  const DataAnnotation: string; DataValue: Double; ColIndex, RowIndex,
  LayerIndex: Integer);
begin
  case Orientation of
    dsoTop: LayerIndex := 0;
    dsoFront: RowIndex := 0;
    dsoSide: ColIndex := 0;
    dso3D: ; // do nothing.
    else Assert(False);
  end;
  // The way the annotations are handled here is related to the
  // fact that strings are reference counted variables.
  // A new string will be allocated each time a string is changed
  // but if a string is just copied all that really happens is that
  // the reference count of the string is increased.  Thus, it
  // saves memory to change a string only when it really needs to be
  // changed and to just copy it whenever possible.
  if (AddMethod in [vamAdd, vamAveragedDelayed, vamAddDelayed])
    and IsValue[LayerIndex, RowIndex, ColIndex] then
  begin
    if Annotation[LayerIndex, RowIndex, ColIndex] = StrNoValueAssigned then
    begin
      // If the array has been previously set to a
      // default value of zero,
      // just replace the old annotaion with the new annotation.
      inherited SetAnnotation(LayerIndex, RowIndex, ColIndex, DataAnnotation);
    end
    else
    begin
      // One or more values has been previously assigned
      // to this cell.
      // Each value will need to be included as part of the
      // annotation.
      if FCount[LayerIndex, RowIndex, ColIndex] = 1 then
      begin
        // One value has been previously assigned to this cell.
        // The value associated with that annotation was not part
        // of the original annotation so the previous value
        // needs to be included along with the original annotation.
        inherited SetAnnotation(LayerIndex, RowIndex, ColIndex,
          FloatToStr(RealData[LayerIndex, RowIndex, ColIndex])  + ' '
          + Annotation[LayerIndex, RowIndex, ColIndex] + EndOfLine
          + FloatToStr(DataValue) + ' ' + DataAnnotation);
      end
      else
      begin
        // Include the new value and new annotation in
        // the annotation for the cell.
        inherited SetAnnotation(LayerIndex, RowIndex, ColIndex,
          Annotation[LayerIndex, RowIndex, ColIndex] + EndOfLine
          + FloatToStr(DataValue) + ' ' + DataAnnotation);
      end;
    end;
    inherited SetRealData(LayerIndex, RowIndex, ColIndex,
      RealData[LayerIndex, RowIndex, ColIndex] + DataValue);
    FCount[LayerIndex, RowIndex, ColIndex] :=
      FCount[LayerIndex, RowIndex, ColIndex] + 1;
  end
  else
  begin
    // No previous value has been assigned to this cell.
    // Just make a copy of the annotation.
    inherited SetRealData(LayerIndex, RowIndex, ColIndex, DataValue);
    inherited SetAnnotation(LayerIndex, RowIndex, ColIndex, DataAnnotation);
    FCount[LayerIndex, RowIndex, ColIndex] := 1;
  end;
end;

procedure TCustomBoundaryRealSparseDataSet.Clear;
begin
  inherited;
  FCount.Clear;
end;

procedure TCustomBoundaryRealSparseDataSet.ComputeAverage;
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  for LayerIndex := 0 to LayerCount - 1 do
  begin
    for RowIndex := 0 to RowCount - 1 do
    begin
      for ColIndex := 0 to ColumnCount - 1 do
      begin
        if IsValue[LayerIndex, RowIndex, ColIndex] then
        begin
          if FCount[LayerIndex, RowIndex, ColIndex] > 1 then
          begin
            inherited SetRealData(LayerIndex, RowIndex, ColIndex,
              RealData[LayerIndex, RowIndex, ColIndex]
              / FCount[LayerIndex, RowIndex, ColIndex]);
            inherited SetAnnotation(LayerIndex, RowIndex, ColIndex,
              'Average of:' + EndOfLine +
              Annotation[LayerIndex, RowIndex, ColIndex]);
            FDataCached := False;
          end;
        end;
      end;
    end;
  end;
end;

constructor TCustomBoundaryRealSparseDataSet.Create(AnOwner: IModelMuseModel);
begin
  inherited;
  FDataCached := False;
  FCount:= T3DSparseIntegerArray.Create(GetQuantum(LayerCount),
    GetQuantum(RowCount), GetQuantum(ColumnCount));
end;

destructor TCustomBoundaryRealSparseDataSet.Destroy;
begin
  inherited;
  FCount.Free;
end;

function TCustomBoundaryRealSparseDataSet.GetCellCount(Layer, Row,
  Column: integer): integer;
begin
  result := FCount[Layer, Row, Column];
end;

function TCustomBoundaryRealSparseDataSet.GetIsValue(const Layer, Row,
  Col: Integer): boolean;
begin
  if FRestoring then
  begin
    result := False;
  end
  else
  begin
    result := inherited and FCount.IsValue[Layer, Row, Col];
  end;
end;

function TCustomBoundaryRealSparseDataSet.GetRealData(const Layer, Row,
  Col: integer): double;
begin
  if IsValue[Layer, Row, Col] then
  begin
    result := inherited;
  end
  else
  begin
    if not TryStrToFloat(Formula, result) then
    begin
      result := 0;
    end;
  end;
end;

procedure TCustomBoundaryRealSparseDataSet.LabelAsSum;
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  for LayerIndex := 0 to LayerCount - 1 do
  begin
    for RowIndex := 0 to RowCount - 1 do
    begin
      for ColIndex := 0 to ColumnCount - 1 do
      begin
        if IsValue[LayerIndex, RowIndex, ColIndex] then
        begin
          if FCount[LayerIndex, RowIndex, ColIndex] > 1 then
          begin
            inherited SetAnnotation(LayerIndex, RowIndex, ColIndex,
              'Sum of:' + EndOfLine
              + Annotation[LayerIndex, RowIndex, ColIndex]);
            FDataCached := False;
          end;
        end;
      end;
    end;
  end;
end;

procedure TCustomBoundaryRealSparseDataSet.ReadData(
  DecompressionStream: TDecompressionStream);
var
  Count: Integer;
  LayerArray: array of Integer;
  RowArray: array of Integer;
  ColumnArray: array of Integer;
  IntegerValues: array of Integer;
  Index: Integer;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  IntValue: Integer;
begin
  FRestoring := True;
  try
    inherited ReadData(DecompressionStream);
    DecompressionStream.Read(Count, SizeOf(Count));
    SetLength(LayerArray, Count);
    SetLength(RowArray, Count);
    SetLength(ColumnArray, Count);
    SetLength(IntegerValues, Count);
    if Count > 0 then
    begin
      DecompressionStream.Read(LayerArray[0], Count*SizeOf(integer));
      DecompressionStream.Read(RowArray[0], Count*SizeOf(integer));
      DecompressionStream.Read(ColumnArray[0], Count*SizeOf(integer));
      DecompressionStream.Read(IntegerValues[0], Count*SizeOf(integer));

      for Index := 0 to Count - 1 do
      begin
        LayerIndex := LayerArray[Index];
        RowIndex := RowArray[Index];
        ColIndex := ColumnArray[Index];
        IntValue := IntegerValues[Index];
        CellCount[LayerIndex, RowIndex, ColIndex] := IntValue;
      end;
    end;
  finally
    FRestoring := False;
  end;
end;

procedure TCustomBoundaryRealSparseDataSet.SetAddMethod(
  const Value: TValueAddMethod);
begin
  FAddMethod := Value;
end;

procedure TCustomBoundaryRealSparseDataSet.SetAnnotation(const Layer, Row,
  Col: integer; const Value: string);
begin
  if not (AddMethod in [vamAveragedDelayed, vamAddDelayed]) then
  begin
    inherited;
  end
  else
  begin
    AddDataValue(Value, FTempValue, Col, Row, Layer);
  end;
end;

procedure TCustomBoundaryRealSparseDataSet.SetCellCount(Layer, Row,
  Column: integer; const Value: integer);
begin
  FCount[Layer, Row, Column] := Value;
end;

procedure TCustomBoundaryRealSparseDataSet.SetDimensions(
  const SetToZero: boolean);
begin
  inherited;
  FCount.Clear;
  if not (csDestroying in ComponentState) then
  begin
    if (AddMethod = vamAveragedDelayed) then
    begin
      InitializeDisplayArray(0);
    end;
  end;
end;

procedure TCustomBoundaryRealSparseDataSet.SetRealData(const Layer, Row,
  Col: integer; const Value: double);
begin
  if not (AddMethod in [vamAveragedDelayed, vamAddDelayed]) then
  begin
    inherited;
  end
  else
  begin
    FTempValue := Value;
  end;
end;

procedure TCustomBoundaryRealSparseDataSet.SetUpToDate(const Value: boolean);
begin
  if Value and not UpToDate then
  begin
//    FDataCached := False;
    FDimensionsChanged := False;
    if AddMethod = vamAveragedDelayed then
    begin
      ComputeAverage;
    end
    else if AddMethod = vamAddDelayed then
    begin
      LabelAsSum;
    end;
  end;
  inherited;
end;

procedure TCustomBoundaryRealSparseDataSet.StoreData(Stream: TStream);
var
  Count: Integer;
  LayerLimit: Integer;
  RowLimit: Integer;
  ColLimit: Integer;
  LayerMin: Integer;
  RowMin: Integer;
  ColMin: Integer;
  LayerArray: array of Integer;
  RowArray: array of Integer;
  ColumnArray: array of Integer;
  IntegerValues: array of Integer;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  inherited StoreData(Stream);
  Count := 0;
  CountValues(LayerLimit, RowLimit, ColLimit, Count);
  GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
    LayerLimit, RowLimit, ColLimit);
  if Count > 0 then
  begin
    SetLength(LayerArray, Count);
    SetLength(RowArray, Count);
    SetLength(ColumnArray, Count);
    SetLength(IntegerValues, Count);
    Count := 0;
    for LayerIndex := LayerMin to LayerLimit do
    begin
      for RowIndex := RowMin to RowLimit do
      begin
        for ColIndex := ColMin to ColLimit do
        begin
          if IsValue[LayerIndex, RowIndex, ColIndex] then
          begin
            LayerArray[Count] := LayerIndex;
            RowArray[Count] := RowIndex;
            ColumnArray[Count] := ColIndex;
            IntegerValues[Count] := CellCount[LayerIndex, RowIndex, ColIndex];
            Inc(Count);
          end;
        end;
      end;
    end;
    Stream.Write(Count, SizeOf(Count));
    Stream.Write(LayerArray[0], Count*SizeOf(integer));
    Stream.Write(RowArray[0], Count*SizeOf(integer));
    Stream.Write(ColumnArray[0], Count*SizeOf(integer));
    Stream.Write(IntegerValues[0], Count*SizeOf(integer));
  end
  else
  begin
    Stream.Write(Count, SizeOf(Count));
  end;
end;

procedure TCustomBoundaryRealSparseDataSet.UpdateDimensions(NumberOfLayers,
  NumberOfRows, NumberOfColumns: integer; ForceResize: boolean);
var
  OldLayerCount: integer;
  OldRowCount: integer;
  OldColumnCount: integer;
begin
  OldLayerCount := LayerCount;
  OldRowCount := RowCount;
  OldColumnCount := ColumnCount;
  inherited;
  if ((OldLayerCount > MaxSmallArraySize) <> (NumberOfLayers > MaxSmallArraySize))
    or ((OldRowCount > MaxSmallArraySize) <> (NumberOfRows > MaxSmallArraySize))
    or ((OldColumnCount > MaxSmallArraySize) <> (NumberOfColumns > MaxSmallArraySize))
    then
  begin
    FCount.Free;
    FCount := T3DSparseIntegerArray.Create(GetQuantum(NumberOfLayers),
      GetQuantum(NumberOfRows), GetQuantum(NumberOfColumns));
  end;
end;

{ EInvalidDataType }

constructor EInvalidDataType.Create(AMessage, AFormula: string);
begin
  inherited Create(AMessage);
  FFormula := AFormula;
end;

{ TStringSparseDataSet }

procedure TStringSparseDataSet.Clear;
begin
  inherited;
  if FStringValues <> nil then
  begin
    FStringValues.Clear;
  end;
end;

constructor TStringSparseDataSet.Create(AnOwner: IModelMuseModel);
begin
  inherited;
  FStringValues := T3DSparseStringArray.Create(GetQuantum(LayerCount),
    GetQuantum(RowCount), GetQuantum(ColumnCount));
  DataType := rdtString;

end;

destructor TStringSparseDataSet.Destroy;
begin
  FreeAndNil(FStringValues);
  inherited;
end;

function TStringSparseDataSet.GetIsValue(const Layer, Row,
  Col: Integer): boolean;
begin
  result := inherited GetIsValue(Layer, Row, Col)
    and FStringValues.IsValue[Layer, Row, Col];
  FPriorResult := result;
end;

procedure TStringSparseDataSet.GetMinMaxStoredLimits(out LayerMin, RowMin,
  ColMin, LayerMax, RowMax, ColMax: integer);
begin
  CheckRestoreData;
  LayerMin := FStringValues.MinLayer;
  RowMin := FStringValues.MinRow;
  ColMin := FStringValues.MinCol;
  LayerMax := FStringValues.MaxLayer;
  RowMax := FStringValues.MaxRow;
  ColMax := FStringValues.MaxCol;
end;

function TStringSparseDataSet.GetStringData(const Layer, Row,
  Col: integer): string;
begin
  Assert(IsValue[Layer, Row, Col]);
  result := FStringValues[Layer, Row, Col];
end;

procedure TStringSparseDataSet.RemoveValue(const Layer, Row, Col: Integer);
begin
  if IsValue[Layer, Row, Col] then
  begin
    FStringValues.RemoveValue(Layer, Row, Col);
  end;
end;

procedure TStringSparseDataSet.SetDataType(const Value: TRbwDataType);
begin
  Assert(Value = rdtString);
  inherited;

end;

procedure TStringSparseDataSet.SetDimensions(const SetToZero: boolean);
begin
  inherited;
  if FStringValues <> nil then
  begin
    FStringValues.Clear;
  end;
end;

procedure TStringSparseDataSet.SetIsValue(const Layer, Row, Col: Integer;
  const Value: boolean);
begin
  if (BoundaryTypeDataSet = self) then
  begin
    FStringValues.IsValue[Layer, Row, Col] := Value;
  end
  else
  begin
    inherited SetIsValue(Layer, Row, Col, Value)
  end;
end;

procedure TStringSparseDataSet.SetStringData(const Layer, Row, Col: Integer;
  const Value: string);
begin
  FStringValues[Layer, Row, Col] := Value;
end;

procedure TStringSparseDataSet.UpdateDimensions(NumberOfLayers, NumberOfRows,
  NumberOfColumns: integer; ForceResize: boolean);
var
  OldLayerCount: integer;
  OldRowCount: integer;
  OldColumnCount: integer;
begin
  OldLayerCount := LayerCount;
  OldRowCount := RowCount;
  OldColumnCount := ColumnCount;
  inherited;
  if ((OldLayerCount > MaxSmallArraySize) <> (NumberOfLayers > MaxSmallArraySize))
    or ((OldRowCount > MaxSmallArraySize) <> (NumberOfRows > MaxSmallArraySize))
    or ((OldColumnCount > MaxSmallArraySize) <> (NumberOfColumns > MaxSmallArraySize))
    then
  begin
    FStringValues.Free;
    FStringValues := T3DSparseStringArray.Create(GetQuantum(NumberOfLayers),
      GetQuantum(NumberOfRows), GetQuantum(NumberOfColumns));
  end;
end;

{ TBooleanSparseDataSet }

procedure TBooleanSparseDataSet.Clear;
begin
  inherited;
  if FBooleanValues <> nil then
  begin
    FBooleanValues.Clear;
  end;
end;

constructor TBooleanSparseDataSet.Create(AnOwner: IModelMuseModel);
begin
  inherited;
  FBooleanValues := T3DSparseBooleanArray.Create(GetQuantum(LayerCount),
    GetQuantum(RowCount), GetQuantum(ColumnCount));
  DataType := rdtBoolean;
end;

destructor TBooleanSparseDataSet.Destroy;
begin
  FreeAndNil(FBooleanValues);
  inherited;
end;

function TBooleanSparseDataSet.GetBooleanData(const Layer, Row,
  Col: integer): Boolean;
begin
  Assert(IsValue[Layer, Row, Col]);
  result := FBooleanValues[Layer, Row, Col];
end;

function TBooleanSparseDataSet.GetIsValue(const Layer, Row,
  Col: Integer): boolean;
begin
  result := inherited GetIsValue(Layer, Row, Col);
  if result then
  begin
    result := FBooleanValues.IsValue[Layer, Row, Col];
    FPriorResult := result;
  end;
end;

function TBooleanSparseDataSet.GetMaxColumn: Integer;
begin
  CheckRestoreData;
  result := FBooleanValues.MaxCol;
end;

function TBooleanSparseDataSet.GetMaxLayer: Integer;
begin
  CheckRestoreData;
  result := FBooleanValues.MaxLayer;
end;

function TBooleanSparseDataSet.GetMaxRow: Integer;
begin
  CheckRestoreData;
  result := FBooleanValues.MaxRow;
end;

function TBooleanSparseDataSet.GetMinColumn: Integer;
begin
  CheckRestoreData;
  result := FBooleanValues.MinCol;
end;

function TBooleanSparseDataSet.GetMinLayer: Integer;
begin
  CheckRestoreData;
  result := FBooleanValues.MinLayer
end;

procedure TBooleanSparseDataSet.GetMinMaxStoredLimits(out LayerMin, RowMin,
  ColMin, LayerMax, RowMax, ColMax: integer);
begin
  CheckRestoreData;
  LayerMin := FBooleanValues.MinLayer;
  RowMin := FBooleanValues.MinRow;
  ColMin := FBooleanValues.MinCol;
  LayerMax := FBooleanValues.MaxLayer;
  RowMax := FBooleanValues.MaxRow;
  ColMax := FBooleanValues.MaxCol;
end;

function TBooleanSparseDataSet.GetMinRow: Integer;
begin
  CheckRestoreData;
  result := FBooleanValues.MinRow
end;

procedure TBooleanSparseDataSet.RemoveValue(const Layer, Row, Col: Integer);
begin
  if IsValue[Layer, Row, Col] then
  begin
    FBooleanValues.RemoveValue(Layer, Row, Col);
  end;
end;

procedure TBooleanSparseDataSet.SetBooleanData(const Layer, Row, Col: integer;
  const Value: Boolean);
begin
  // don't call inherited;
  FBooleanValues[Layer, Row, Col] := Value;
end;

procedure TBooleanSparseDataSet.SetDataType(const Value: TRbwDataType);
begin
  Assert(Value = rdtBoolean);
  inherited;
end;

procedure TBooleanSparseDataSet.SetDimensions(const SetToZero: boolean);
begin
  inherited;
  if FBooleanValues <> nil then
  begin
    FBooleanValues.Clear;
  end;
end;

procedure TBooleanSparseDataSet.UpdateDimensions(NumberOfLayers, NumberOfRows,
  NumberOfColumns: integer; ForceResize: boolean);
var
  OldLayerCount: integer;
  OldRowCount: integer;
  OldColumnCount: integer;
begin
  OldLayerCount := LayerCount;
  OldRowCount := RowCount;
  OldColumnCount := ColumnCount;
  inherited;
  if ((OldLayerCount > MaxSmallArraySize) <> (NumberOfLayers > MaxSmallArraySize))
    or ((OldRowCount > MaxSmallArraySize) <> (NumberOfRows > MaxSmallArraySize))
    or ((OldColumnCount > MaxSmallArraySize) <> (NumberOfColumns > MaxSmallArraySize))
    then
  begin
    FBooleanValues.Free;
    FBooleanValues := T3DSparseBooleanArray.Create(GetQuantum(NumberOfLayers),
      GetQuantum(NumberOfRows), GetQuantum(NumberOfColumns));
  end;
end;

{ TTransientBooleanSparseDataSet }

procedure TTransientBooleanSparseDataSet.UpdateNotifiers;
begin
  // do nothing
//  inherited;
end;

initialization
  StrRequired := StrDataSets + StrRequiredPart;
  // @name is used in the classification of data sets.
  StrOptional := StrDataSets + StrOptionalPart;
  // @name is used in the classification of data sets.
  strDefaultClassification := StrDataSets + '|' + StrUserDefined;

  RegisterClasses([TDataArray, TCustom2DInterpolater, TRealSparseDataSet,
    TStringSparseDataSet, TIntegerSparseDataSet]);

end.

