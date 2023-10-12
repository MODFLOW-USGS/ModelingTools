{@abstract(@name is used to define types, constants, and small classes used
  in a variety of places in GoPhast.)}
unit GoPhastTypes;

interface

uses
  GR32, // defines TColor32.
  SysUtils, Types, Classes, FastGEO, Graphics, Generics.Collections, Character,
  System.ZLib, System.Generics.Defaults;

type
  PReal = ^Real;

  {@abstract(@name is a one-dimensional array of doubles.)
  }
  TOneDRealArray = array of Double;

  {@abstract(@name is a one-dimensional array of integers.)
  }
  TOneDIntegerArray = array of integer;

  {@abstract(@name is a two-dimensional array of integers.)
  }
  TTwoDIntegerArray = array of TOneDIntegerArray;
  TThreeDIntegerArray = array of TTwoDIntegerArray;

  {@abstract(@name is a two-dimensional array of doubles.)
  }
  TTwoDRealArray = array of TOneDRealArray;

  {@abstract(@name is a three-dimensional array of doubles.)
  }
  TThreeDRealArray = array of TTwoDRealArray;

  TTwoDStringArray = array of array of string;

  {@abstract(@name is a pointer to a TPoint2D.)
  }
  P2DRealPoint = TPoint2DPtr;

  {@abstract(@name is a one-dimensional array of T2DRealPoints.)
  }
  TRealPointArray = TPolygon2D;//array of TPoint2D;

  {@abstract(@name is a two-dimensional array of T2DRealPoints.)
  }
  T2DRealPointArray = array of TRealPointArray;

  {@abstract(@name is a one-dimensional array of booleans.)
  }
  TBoolArray = array of boolean;

  {@abstract(@name is a two-dimensional array of booleans.)
  }
  T2DBoolArray = array of array of boolean;

  {@abstract(@name is a three-dimensional array of booleans.)
  }
  T3DBoolArray = array of array of array of boolean;

  {@abstract(@name represents a 3D point with real-number coordinates.)
  }
  T3DRealPoint = TPoint3D;
  P3DRealPoint = ^T3DRealPoint;

  TRealArray = array of Real;

  // @name is a 1D array of @link(T3DRealPoint)s.
  T3DRealPointArray1 = array of TPoint3D;

  // @name is a 2D array of @link(T3DRealPoint)s.
  T3DRealPointArray2 = array of T3DRealPointArray1;

  // @name is a 3D array of @link(T3DRealPoint)s.
  T3DRealPointArray3 = array of array of array of TPoint3D;

  // @name records the minimum and maximum values assigned to a data set.
  TMinMax = record
    LogRMin, LogRMax: Double;
    RMinPositive, RMin, RMax: Real;
    IMin, IMax: Integer;
    BMin, BMax: Boolean;
    SMin, SMax: string;
  end;

  TDualLocation = record
    RotatedLocation: TPoint3D;
    UnRotatedLocation: TPoint3D;
  end;

  // @name is used to indicate which view of the model the cursor
  // is over.
  TCursorGrid = (cgNone, cgTop, cgFront, cgSide);

  // @name is used to describe the direction
  // @link(ScreenObjectUnit.TScreenObject)s are
  // viewed from.
  TViewDirection = (vdTop, vdFront, vdSide);
  TViewDirections = set of TViewDirection;

  // @name is used to specify the columns in the table on
  // @link(frmDataSetsUnits.TfrmDataSets).
  //
  // @value(dcName = name of the @link(DataSetUnit.TDataArray).)
  // @value(dcType = the type of data @link(DataSetUnit.TDataArray.DataType)
  //   (boolean, integer, real number, or string)
  //   stored by the @link(DataSetUnit.TDataArray).)
  // @value(dcOrientation = the @link(DataSetUnit.TDataArray.Orientation) of the
  //   @link(DataSetUnit.TDataArray).)
  // @value(dcEvaluatedAt = the @link(DataSetUnit.TDataArray.EvaluatedAt)
  //    of the @link(DataSetUnit.TDataArray).)
  // @value(dcUnits = the @link(DataSetUnit.TDataArray.Units) of the
  //   @link(DataSetUnit.TDataArray).)
  // @value(dcFormula = the @link(DataSetUnit.TDataArray.Formula) of the
  //    @link(DataSetUnit.TDataArray).)
  // @value(dcInterpolation = the @link(DataSetUnit.TDataArray.TwoDInterpolator)
  //    of the @link(DataSetUnit.TDataArray).)
  TDataColumns = (dcName, dcType, dcOrientation, dcEvaluatedAt,
    dcUnits, dcFormula, dcInterpolation);
  // @name specifies which values in the table on
  // @link(frmDataSetsUnits.TfrmDataSets) the user can edit.
  // only columns not included in @name can be edited.
  TDataLock = set of TDataColumns;

  // @name is used in TDataArray.@link(TDataArray.Orientation) to
  // indicate whether the @link(TDataArray) is a 2D or 3D data set and,
  // if it is 2D, which face of the grid it is associated with.
  //
  // @value(dsoTop 2D top face)
  // @value(dsoFront 2D front face)
  // @value(dsoSide 2D side face)
  // @value(dso3D 3D)
  TDataSetOrientation = (dsoTop, dsoFront, dsoSide, dso3D);

  TDataSetOrientations = set of TDataSetOrientation;

  // @name is used in specifying the number of elevations associated with
  // a @link(TScreenObject).
  TElevationCount = (ecZero, ecOne, ecTwo);

  // @name is used to specify whether a data set is evaluated at
  // element centers or at nodes.
  TEvaluatedAt = (eaBlocks, eaNodes);

  TLgrCellTreatment = (lctUse, lctIgnore, lctZero);

  // @name represents the frequencies with which data can be printed
  // in PHAST.
  TFrequencyUnits = (fuDefault, fuSeconds, fuMinutes, fuHours, fuDays,
    fuYears, fuStep, fuEnd);

  // @name represents the time units recognized by PHAST.
  TTimeUnits = (tuSeconds, tuMinutes, tuHours, tuDays, tuYears);

  // @name represents the length units recognized by PHAST.
  TLengthUnits = (luInches, luFeet, luMiles, luMillimeters,
    luCentimeters, luMeters, luKilometers);

  // @name represents the 1/length units recognized by PHAST.
  TInverseLengthUnits = (iluInches, iluFeet, iluMiles, iluMillimeters,
    iluCentimeters, iluMeters, iluKilometers);

  // @name represents the volume units recognized by PHAST.
  TVolumeUnits = (vuGallons, vuInches3, vuFeet3, vuMiles3,
    vuLiters, vuMillimeters3, vuCentimeters3, vuMeters3, vuKilometers3);

  // @name represents the solvers used by PHAST.
  TPhastSolver = (psDirect, psIterative);

  //TInterpolationDirection determines whether "PHAST" style interpolation
  // is used or "PHAST" style mixtures.  If "PHAST" style interpolation
  // is used, it also determines the coordinate direction.
  // @value(pidX = Interpolate in the X direction.)
  // @value(pidY = Interpolate in the Y direction.)
  // @value(pidZ = Interpolate in the Z direction.)
  // @value(pidMix = Use "PHAST" style mixtures.)
  // See @LINK(TPhastInterpolationValues).
  TInterpolationDirection = (pidX, pidY, pidZ, pidMix);

  {@abstract(@name is a pointer to a @link(TInterpolationDirection).)
  @longcode(#
  PInterpolationDirection = ^TInterpolationDirection;
  #)
  }
  PInterpolationDirection = ^TInterpolationDirection;

  // @name represent the items whose print frequencies
  // can be specified in PHAST.
  TPrintFrequencyRow = (pfrName, pfrTime, pfrFlowRate, pfrComponents,
    pfrConductance, pfrFlowBalance, pfrChemPrint, pfrHDFChem, pfrHDFHeads,
    pfrHDFVelocity, pfrHeads, pfrProgress, pfrRestart, pfrVelocities, pfrWells, pfrXYZChem,
    pfrXYZComponents, pfrXYZHeads, pfrXYZVelocities, pfrXYZWells,
    pfrBoundaryConditions, pfrDefault);

  TPrintFrequencyRows = set of TPrintFrequencyRow;

  // @name represents the types of boundary
  // @value(btNone = no boundary condition)
  // @value(btSpecifiedHead = Specified head boundary condition)
  // @value(btFlux = Flux boundary condition)
  // @value(btLeaky = Leaky boundary condition)
  // @value(btRiver = River boundary condition)
  // @value(btWell = Well boundary condition)
  TPhastBoundaryTypes = (btNone, btSpecifiedHead, btFlux, btLeaky, btRiver, btWell);
  TModflowBoundaryType = (mbtNone, mbtCHD);

  // @name specifies how elevations are specified in the well boundary
  // condition. See @link(TWellBoundary.WellElevationFormat).
  TWellElevationFormat = (wefElevation, wefDepth);

  {@abstract(The row and column of a cell)
  @longcode(#
  T2DTopCell = record
    Col: integer;
    Row: integer;
  end;
  #)
  @member(Col @name is the column number.)
  @member(Row @name is the row number.)
  }
  T2DTopCell = record
    Col: integer;
    Row: integer;
  end;

  {@abstract(The column and layer of a cell)
  @longcode(#
  T2DFrontCell = record
    Col: integer;
    Lay: integer;
  end;
  #)
  @member(Col @name is the column number.)
  @member(Lay @name is the layer number.)
  }
  T2DFrontCell = record
    Col: integer;
    Lay: integer;
  end;

  {@abstract(The row and layer of a cell)
  @longcode(#
  T2DSideCell = record
    Row: integer;
    Lay: integer;
  end;
  #)
  @member(Row @name is the row number.)
  @member(Lay @name is the layer number.)
  }
  T2DSideCell = record
    Row: integer;
    Lay: integer;
  end;

  {@abstract(The columns, row, and layer of a cell)
  @longcode(#
  T3DCell = record
    Col: integer;
    Row: integer;
    Lay: integer;
  end;
  #)
  @member(Col @name is the column number.)
  @member(Row @name is the row number.)
  @member(Lay @name is the layer number.)
  }
  T3DCell = record
    Col: integer;
    Row: integer;
    Lay: integer;
  end;

  // The format of several files in MODFLOW-NWT 1.1 changed in way that is
  // incompatible with the previous format. nf1_0 represents the old format.
  // nf1_1 represents the new format;
  TNwtFormat = (nf1_0, nf1_1);

  TDrawingChoice = (dcAll, dcEdge);

  // @name is used to indicate what type of model is active.
  // The type of model should never be set to msUndefined.
  TModelSelection = (msUndefined, msPhast, msModflow, msModflowLGR,
    msModflowLGR2, msModflowNWT, msModflowFmp, msModflowCfp, msSutra22,
    msSutra30, msSutra40, msFootPrint, msModflow2015, msModflowOwhm2);

  TPestParamMethod = (ppmMultiply, ppmAdd);

  TPestMethodList = TList<TPestParamMethod>;
  TStringListObjectList = TObjectList<TStringList>;

  // @name  is mainly used for concentrations in boundaries.
  TGwtCellData = record
  private
    FSpeciesCount: Integer;
    procedure SetSpeciesCount(const Value: Integer);
  public
    Values: array of double;
    ValueAnnotations: array of string;
    ValuePestNames: array of string;
    ValuePestSeriesNames: array of string;
    ValuePestSeriesMethods: array of TPestParamMethod;
    ValueTimeSeriesNames: array of string;
    procedure Assign(const Item: TGwtCellData);
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
    Property SpeciesCount: Integer read FSpeciesCount write SetSpeciesCount;
  end;

  // @name is used in FMP4
  TLandUseData = record
  private
    FLandUseCount: Integer;
    procedure SetLandUseCount(const Value: Integer);
  public
    Values: array of double;
    IntValues: array of Integer;
    ValueAnnotations: array of string;
    ValuePestNames: array of string;
    ValuePestSeriesNames: array of string;
    ValuePestSeriesMethods: array of TPestParamMethod;
    ValueTimeSeriesNames: array of string;
    procedure Assign(const Item: TLandUseData);
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
    Property LandUseCount: Integer read FLandUseCount write SetLandUseCount;
  end;

  TCellData = record
  public
    Value: double;
    ValueAnnotation: string;
    ValuePestName: string;
    ValuePestSeriesName: string;
    ValuePestSeriesMethod: TPestParamMethod;
    ValueTimeSeriesName: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TCellDataArray = record
  private
    procedure SetPropertyCount(const Value: Integer);
    function GetPropertyCount: Integer;
  public
    Values: array of TCellData;
    procedure Assign(const Item: TCellDataArray);
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
    property PropertyCount: Integer read GetPropertyCount write SetPropertyCount;
  end;

const
  ModflowSelection = [msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
    msModflowFmp, msModflowCfp, msModflow2015, msModflowOwhm2];
  Modflow2005Selection =  [msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
    msModflowFmp, msModflowCfp, msModflowOwhm2];
  SutraSelection = [msSutra22, msSutra30, msSutra40];
  ModelsWithGrid  = [msPhast, msModflow, msModflowLGR, msModflowLGR2,
    msModflowNWT, msModflowFmp, msModflowCfp, msFootPrint, msModflow2015,
    msModflowOwhm2];
  HighGradient = 0.01;

type
  TActiveCell = class(TObject)
    MFLayer: integer;
    Layer: integer;
    Row: integer;
    Column: integer;
  end;

  TActiveCellQueue = TObjectQueue<TActiveCell>;

  TModelType = (mtGroundWaterFlow, mtGroundwaterTransport);

  TModelData = record
    ModelType: TModelType;
    ModelNameFile: string;
    ModelName: string;
    SolutionGroup: string;
    ImsFile: string;
    MaxIterations: integer;
    function ModelLine: string;
  end;

  // @link(TMf6_SimNameFileWriter) in @link(CustomModflowWriterUnit)
  // implements @name
  IMf6_SimNameFileWriter = interface(IInterface) ['{9C59A1EE-3BB2-4E39-A92A-DA09791188BD}']
    function GetTDisFileName: string;
    procedure SetTDisFileName(const Value: string);
    property TDisFileName: string read GetTDisFileName write SetTDisFileName;
    procedure AddModel(ModelData: TModelData);
    procedure WriteFile(Directory: string);
    procedure AddExchange(ExchangeLine: string);
    function GetGwtTDisFileNames(SpeciesIndex: Integer): string;
    procedure SetGwtTDisFileNames(SpeciesIndex: Integer; const Value: string);
    property GwtTDisFileNames[SpeciesIndex: Integer]: string
      read GetGwtTDisFileNames write SetGwtTDisFileNames;
    function GetSimFileName(Index: Integer): string;
    function GetSimFileNameCount: Integer;
    property SimFileNameCount: Integer read GetSimFileNameCount;
    property SimFileNames[Index: Integer]: string read GetSimFileName;
  end;

  TBoundaryNode = record
    NodeNumber: Integer;
    PressureOrFlow: double;
    TempOrConc: double;
    UseBCTime: Boolean;
    PressureOrFlowFormula: string;
    TempOrConcFormula: string;
    class function Create(NodeNumber: Integer; PressureOrFlow,
      TempOrConc: Double; UseBCTime: Boolean;
      PressureOrFlowFormula, TempOrConcFormula: string): TBoundaryNode; static;
  end;

  IBoundaryNodes = interface(IInterface) ['{8DAD8491-1A06-4C52-99B6-C204C17CCF9A}']
    procedure AddUnique(Node: TBoundaryNode);
    procedure Clear;
    function ToArray: TArray<TPair<Integer,TBoundaryNode>>;
    function GetCount: Integer;
    property Count: Integer read GetCount;
  end;

  //  @name is used to indicate how the spacing of layers within a unit
  // is specified.
  // @value(gmUniform) The layers are spaced uniformly.
  // @value(gmUp) The layers increase in thickness upward.
  // @value(gmDown) The layers increase in thickness downward.
  // @value(gmMiddle) The layers increase in thickness toward the middle
  // of the unit from both the top and bottom.
  // @value(gmEdge) The layers increase in thickness toward both the top
  // and bottom of the unit from the middle.
  // @value(gmCustom) The thickness of each layer is specified individually.
  TGrowthMethod = (gmUniform, gmUp, gmDown, gmMiddle, gmEdge, gmCustom);

  // @name indicates the locations at which a @link(TScreenObject) should
  // assign values to cells in a @link(TDataArray)
  // @value(alAll Assign values to all locations.)
  // @value(alFirstVertex Assign values to cells at the location of the first
  //   vertex in the @link(TScreenObject).)
  // @value(alLastVertex Assign values to cells at the location of the last
  //   vertex in the @link(TScreenObject).)
  TAssignmentLocation = (alAll, alFirstVertex, alLastVertex);

  TByteSet = set of byte;

  TIface = (iIndeterminant, iHorizontal, iInternal,
    iLeft, iRight, iFront, iBack, iBottom, iTop);

  TStatFlag = (stVariance, stStandardDev, stCoefVar, stWeight, stSquaredWeight);

  TObservationPurpose = (ofObserved, ofPredicted, ofInacative);

  // @name is used in @link(TCustomTransientWriter.AssignTransient2DArray).
  // @value(umAssign Replace existing values with new values.)
  // @value(umAdd Add new values to existing values.)
  TUpdateMethod = (umAssign, umAdd);

  // In MT3DMS transport observations, observation results can be specified
  // either at a specific time or at a desired frequency using the same
  // variable. @name is used to indicate which form is to be used.
  TObservationType = (otTime, otFrequency);

  TDisplayChoice = (dcColor, dcContour, dcNone);

  TSwrSpecificationMethod = (smObject, smArray);

  TMf6InterpolationMethods = (mimStepwise, mimLinear, mimLinearEnd);

  TOwhmV2PumpSpreadChoice = (pscConductance, pscByNodeCount, pscTopNode, pscByWbs);

  TBaseModel = class;

  TElementOutline = record
  private
    FOutline: TPolygon2D;
    function GetCount: Integer;
    procedure SetCount(const Value: Integer);
    function GetSegment(Index: Integer): TSegment2D;
    function GetPoint(Index: Integer): TPoint2D;
    procedure SetPoint(Index: Integer; const Value: TPoint2D);
  public
    property Count: Integer read GetCount write SetCount;
    property Outline: TPolygon2D read FOutline write FOutline;
    property Segments[Index: Integer]: TSegment2D read GetSegment;
    property Points[Index: Integer]: TPoint2D read GetPoint write SetPoint;
  end;

  TGenericIntegerList = TList<integer>;
  TListOfTIntegerList = TObjectList<TGenericIntegerList>;

  TGridLimit = record
    MinX: double;
    MaxX: double;
    MinY: double;
    MaxY: double;
    MinZ: double;
    MaxZ: double;
  end;

  // @name is used to communicate the identifying information of a source to
  // @link(TModflowMvrWriter) via @link(TModflowMvrWriter.AddMvrSource);
  TMvrSourceKey = record
    // @name is the order in which the cell appears in the boundaries generated
    // from @link(TScreenObject ScreenObject);
    MvrIndex: Integer;
    ScreenObject: TObject;
  end;

  TMvrRegisterKey = record
    SourceKey: TMvrSourceKey;
    // @name is the number of the source in its respective package and stress
    // period starting with 1.
    Index: Integer;
    StressPeriod: Integer;
  end;

  // @abstract(@name invalidates the model when it is changed.)
  TPhastCollection = class(TCollection)
  strict private
    FOnInvalidateModel: TNotifyEvent;
  private
    FOnChange: TNotifyEvent;
    function GetFirst: TCollectionItem;
    function GetLast: TCollectionItem;
    procedure DoOnChange;
  protected
    function GetCount: Integer; virtual;
    procedure SetCount(const Value: Integer); virtual;
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function QueryInterface(const IID: TGUID; out Obj): HRESULT;
      virtual; stdcall;
    procedure Assign(Source: TPersistent); override;
    procedure InvalidateModel;
    // @name invalidates the model.
    procedure Notify(Item: TCollectionItem; Action: Classes.TCollectionNotification);
      override;
    constructor Create(ItemClass: TCollectionItemClass; InvalidateModelEvent: TNotifyEvent);
    property First: TCollectionItem read GetFirst;
    property Last: TCollectionItem read GetLast;
    property Count: Integer read GetCount write SetCount;
    property OnInvalidateModel: TNotifyEvent read FOnInvalidateModel;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function Add: TCollectionItem;
    procedure Clear;
    procedure Delete(Index: Integer);
    function Insert(Index: Integer): TCollectionItem;
    procedure Sort(const AComparer: IComparer<TCollectionItem>);
  end;

  TPhastCollectionItem = class(TCollectionItem)
  strict private
    function GetOnInvalidateModel: TNotifyEvent;
  strict Protected
    property OnInvalidateModel: TNotifyEvent read GetOnInvalidateModel;
  protected
    procedure SetRealProperty(var AField: double; const NewValue: double);
    procedure SetBooleanProperty(var AField: boolean; const NewValue: boolean);
    procedure SetIntegerProperty(var AField: integer; const NewValue: integer);
    procedure SetStringProperty(var AField: string; const NewValue: string);
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
  public
    procedure InvalidateModel;
  end;

  // @name adds the required functions of IInterface.
  TInterfacedPhastCollectionItem = class(TPhastCollectionItem, IInterface)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function QueryInterface(const IID: TGUID; out Obj): HRESULT;
      virtual; stdcall;
  end;

  TIntegerItem = class(TPhastCollectionItem)
  private
    FValue: integer;
    FOnChange: TNotifyEvent;
    procedure SetValue(const Value: integer);
  public
    procedure Assign(Source: TPersistent); override;
    function IsSame(Item: TIntegerItem): Boolean;
  published
    property Value: integer read FValue write SetValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TIntegerCollection = class(TPhastCollection)
  private
    FInitialValue: integer;
    function GetItems(Index: Integer): TIntegerItem;
    procedure SetItems(Index: Integer; const Value: TIntegerItem);
    procedure SetInitialValue(const Value: integer);
    function GetFirst: TIntegerItem;
    function GetLast: TIntegerItem;
  public
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    function IsSame(IntegerCollection: TIntegerCollection): Boolean;
    property  Items[Index: Integer]: TIntegerItem read GetItems
      write SetItems; default;
    function Add: TIntegerItem;
    property InitialValue: integer read FInitialValue write SetInitialValue;
    procedure Sort;
    function IndexOf(AValue: Integer): integer;
    property First: TIntegerItem read GetFirst;
    property Last: TIntegerItem read GetLast;
  end;

  TRealItem = class(TPhastCollectionItem)
  private
    FValue: double;
    FOnChange: TNotifyEvent;
    function GetValue: double;
    procedure SetValue(const Value: double);
  protected
    procedure ReadValue(Reader: TReader);
    procedure ReadStringValue(Reader: TReader);
    procedure WriteValue(Writer: TWriter);
    procedure WriteStringValue(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
    function IsSame(Item: TRealItem): Boolean; virtual;
  published
    property Value: double read GetValue write SetValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TRealCollection = class(TPhastCollection)
  private
    FInitialValue: Real;
    function GetItems(Index: Integer): TRealItem;
    procedure SetItems(Index: Integer; const Value: TRealItem);
    procedure SetInitialValue(const Value: Real);
  protected
    function GetValue(Index: Integer): Double;
    procedure SetValue(Index: Integer; const Value: Double);
  public
    constructor Create(InvalidateModelEvent: TNotifyEvent); overload; virtual;
    constructor Create(InvalidateModelEvent: TNotifyEvent; Values: TOneDRealArray); overload;
    function IsSame(RealCollection: TRealCollection): Boolean;
    property  Items[Index: Integer]: TRealItem read GetItems
      write SetItems; default;
    property Values[Index: Integer]: Double read GetValue write SetValue;
    function Add: TRealItem;
    property InitialValue: Real read FInitialValue write SetInitialValue;
    procedure Sort;
    function First: TRealItem;
    function Last: TRealItem;
  end;

  TPointArray = array of TPoint;

  TRealStorage = class(TPersistent)
  private
    FValue: real;
    FOnChange: TNotifyEvent;
    procedure SetValue(Value: real);
  protected
    procedure ReadValue(Reader: TReader);
    procedure ReadStringValue(Reader: TReader);
    procedure WriteValue(Writer: TWriter);
    procedure WriteStringValue(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(OnChangeEvent: TNotifyEvent); overload;
  published
    property Value: real read FValue write SetValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  // @name is used to ensure that a value for a string is saved even if it
  // is an empty string.
  TStringStorage = class(TPersistent)
  private
    FValue: string;
    FOnChange: TNotifyEvent;
    procedure SetValue(const Value: string);
  protected
    procedure ReadValue(Reader: TReader);
    procedure WriteValue(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Value: string read FValue write SetValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGoPhastPersistent = class(TPersistent)
  strict private
    FOnInvalidateModel: TNotifyEvent;
  protected
    procedure OnChangeEventHander(Sender: TObject); virtual;
    procedure InvalidateModel; virtual;
    procedure SetBooleanProperty(var AField: boolean; const NewValue: boolean);
    procedure SetIntegerProperty(var AField: integer; const NewValue: integer);
    procedure SetRealProperty(var AField: double; const NewValue: double);
    procedure SetAnsiStringProperty(var AField: AnsiString; const NewValue: AnsiString);
    procedure SetStringProperty(var AField: string; const NewValue: string);
    procedure SetPointProperty(var AField: TPoint; const NewValue: TPoint);
    procedure SetColorProperty(var AField: TColor; const NewValue: TColor);
    procedure SetDataTimeProperty(var AField: TDateTime; const NewValue: TDateTime);
    procedure SetCharacterProperty(var AField: Char; const NewValue: Char);
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function QueryInterface(const IID: TGUID; out Obj): HRESULT;
      virtual; stdcall;
    property OnInvalidateModel: TNotifyEvent read FOnInvalidateModel;
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
  end;

  IModelMuseModel = interface(IInterface)
    ['{84E508DD-8AC0-4FA4-9919-C81388A57403}']
    function GetModelSelection: TModelSelection;
    procedure SetModelSelection(const Value: TModelSelection);
    property ModelSelection: TModelSelection read GetModelSelection
      write SetModelSelection;
    procedure DoInvalidate(Sender: TObject);
    function GetGwtUsed: Boolean;
    property GwtUsed: Boolean read GetGwtUsed;
    function GetInvalidate: TNotifyEvent;
    property Invalidate: TNotifyEvent read GetInvalidate;
    function GetComponentState: TComponentState;
    property ComponentState: TComponentState read GetComponentState;
    function GetDisplayName: string;
    property DisplayName: string read GetDisplayName;
  end;

  TBaseModel = class abstract(TComponent, IModelMuseModel)
  private
    // See @link(UpToDate).
    FUpToDate: boolean;
    function GetComponentState: TComponentState;
  protected
    // See @link(UpToDate).
    procedure SetUpToDate(const Value : boolean); virtual;
    function GetDisplayName: string; virtual; abstract;
    function GetModelSelection: TModelSelection; virtual; abstract;
    procedure SetModelSelection(const Value: TModelSelection); virtual; abstract;
    function GetGwtUsed: Boolean; virtual; abstract;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    property ComponentState: TComponentState read GetComponentState;
  public
    // Call @name to indicate that the model has changed in some important
    // respect.  The user will be prompted to save the model when closing.
    procedure DoInvalidate(Sender: TObject); virtual;
    function GetInvalidate: TNotifyEvent;
    property Invalidate: TNotifyEvent read GetInvalidate;

    // @name indicates whether or not the model needs to be saved to file.
    // See @link(Invalidate).
    property UpToDate: boolean read FUpToDate write SetUpToDate;
    property DisplayName: string read GetDisplayName;
    property GwtUsed: Boolean read GetGwtUsed;
  published
    property ModelSelection: TModelSelection read GetModelSelection
      write SetModelSelection;
  end;

  TLayerSort = class(TObject)
    Layer: integer;
    ActiveCells: integer;
    Proportion: double;
  end;

  {TPhastModel.UpdateModelMateParameter should be updated
  if new parameters are added.
  }
  TParameterType = (ptUndefined, ptLPF_HK, ptLPF_HANI, ptLPF_VK,
    ptLPF_VANI, ptLPF_SS, ptLPF_SY, ptLPF_VKCB, ptRCH, ptEVT, ptETS,
    ptCHD, ptGHB, ptQ,
    ptRIV, ptDRN, ptDRT, ptSFR, ptHFB,
    ptHUF_HK, ptHUF_HANI, ptHUF_VK, ptHUF_VANI, ptHUF_SS, ptHUF_SY,
    ptHUF_SYTP, ptHUF_KDEP, ptHUF_LVDA, ptSTR, ptQMAX, ptPEST);
  //

  // @name is used to indicate groups of related MODFLOW parameters.
  TParameterTypes = set of TParameterType;



  TSutraLimitType = (sltNone, sltFlow, sltPressure);
  TSutraExitSpecificationMethod = (sexmRelative, sexmDirect);

  TSutraLimitTypeList = TList<TSutraLimitType>;
  TSutraExitSpecificationMethodList = TList<TSutraExitSpecificationMethod>;

  function EvalAtToString(const Eval: TEvaluatedAt;
    const Model: TModelSelection; const Plural, TitleCase: boolean): string;

  function ValidName(const OriginalName: string): string;
  function RightCopy(const Source: string; LengthToCopy: integer): string;

  function OrientationToViewDirection(Orientation: TDataSetOrientation): TViewDirection;

  function GetLFormatSettings: TFormatSettings;

procedure AssignModflowLengthUnitStringsToPicklist(LengthPickList: TStrings);
procedure AssignModflowTimeUnitStringsToPicklist(TimePickList: TStrings);
procedure AssignTypicalTimeUnitStringsToPicklist(TimePickList: TStrings);
procedure AssignTypicalLengthUnitStringsToPicklist(LengthPickList: TStrings);

procedure AssignLengthUnitStringsToPicklist(LengthPickList: TStrings;
  ModelType: TModelSelection);

procedure AssignTimeUnitStringsToPicklist(TimePickList: TStrings;
  ModelType: TModelSelection);

procedure WriteCompInt(Stream: TStream; Value: integer);
procedure WriteCompReal(Stream: TStream; Value: double);
procedure WriteCompBoolean(Stream: TStream; Value: Boolean);
procedure WriteCompString(Stream: TStream; Value: string);

function ReadCompInt(Stream: TStream): integer;
function ReadCompReal(Stream: TStream): double;
function ReadCompBoolean(Stream: TStream): Boolean;
function ReadCompStringSimple(Stream: TStream): string;
function ReadCompString(Stream: TStream; Annotations: TStringList): string;

resourcestring
  StrPestModifier = 'PEST Modifier';
  StrModificationMethod = 'Modification Method';

const
  PestRowOffset = 2;
//  PestRowOffset = 0;

  PestModifierRow = 1;
  PestMethodRow = 2;

  TimeEpsilon = 1e-6;

  kModelTop = 'Model_Top';


  {@name is used when writing the PHAST input file to insert a consistent
    number of blank spaces.

  @longcode(#
  BlankSpaces = '      ';
  #)
  }
  BlankSpaces = '      ';

  // @name is the maximum length of boundary names in MODFLOW 6.
  MaxBoundNameLength = 40;

  MaxTimeSeriesNameLength = 40;

  // @name is the section name in the ini file that holds the
  // names of the most recently opened files.
  MRU_Section = 'MostRecentlyUsed';
  StrParameterType = 'Parameter Type';
  StrEnhancedTemplateProc = 'EnhancedTemplateProcessor.exe';

resourcestring
  StrModelTop = kModelTop;
  StrWritingDataSet0 = '  Writing Data Set 0.';
  StrWritingDataSet1 = '  Writing Data Set 1.';
  StrWritingDataSet2 = '  Writing Data Set 2.';
  StrWritingDataSet3 = '  Writing Data Set 3.';
  StrWritingDataSet4 = '  Writing Data Set 4.';
  StrWritingDataSet5 = '  Writing Data Set 5.';
  StrWritingDataSet6 = '  Writing Data Set 6.';
  StrWritingDataSet7 = '  Writing Data Set 7.';
  StrWritingDataSet8 = '  Writing Data Set 8.';
  StrWritingDataSet9 = '  Writing Data Set 9.';
  StrWritingDataSet10 = '  Writing Data Set 10.';
  StrWritingDataSet11 = '  Writing Data Set 11.';
  StrWritingDataSet12 = '  Writing Data Set 12.';
  StrWritingDataSet13 = '  Writing Data Set 13.';
  StrWritingDataSet14 = '  Writing Data Set 14.';
  StrWritingDataSet15 = '  Writing Data Set 15.';
  StrWritingDataSet16 = '  Writing Data Set 16.';
  StrWritingDataSet17 = '  Writing Data Set 17.';
  StrWritingDataSet18 = '  Writing Data Set 18.';
  StrWritingDataSet19 = '  Writing Data Set 19.';
  StrWritingDataSet20 = '  Writing Data Set 20.';
  StrWritingDataSet21 = '  Writing Data Set 21.';
  StrWritingDataSet22 = '  Writing Data Set 22.';
  StrWritingDataSet23 = '  Writing Data Set 23.';
  StrWritingDataSet24 = '  Writing Data Set 24.';
  StrWritingDataSet25 = '  Writing Data Set 25.';
  StrWritingDataSet26 = '  Writing Data Set 26.';
  StrWritingDataSet27 = '  Writing Data Set 27.';
  StrWritingDataSet28 = '  Writing Data Set 28.';
  StrWritingDataSet29 = '  Writing Data Set 29.';
  StrWritingDataSet30 = '  Writing Data Set 30.';
  StrWritingDataSet31 = '  Writing Data Set 31.';
  StrWritingDataSet32 = '  Writing Data Set 32.';
  StrWritingDataSet33 = '  Writing Data Set 33.';
  StrWritingDataSet34 = '  Writing Data Set 34.';
  StrWritingDataSet35 = '  Writing Data Set 35.';
  StrWritingDataSet36 = '  Writing Data Set 36.';
  StrWritingDataSet37 = '  Writing Data Set 37.';
  StrWritingDataSet38 = '  Writing Data Set 38.';
  StrWritingDataSet39 = '  Writing Data Set 39.';
  StrWritingDataSets3and4 = '  Writing Data Sets 3 and 4.';
  StrWritingDataSets5to7 = '  Writing Data Sets 5 to 7.';
  StrWritingDataSets5to8 = '  Writing Data Sets 5 to 8.';
  StrWritingDataSets2and3 = '  Writing Data Sets 2 and 3.';
  StrWritingDataSets3to5 = '  Writing Data Sets 3 to 5.';
  StrWritingDataSets6to8 = '  Writing Data Sets 6 to 8.';

  StrWritingOptions = '  Writing Options';
  StrWritingDimensions = '  Writing Dimensions';
  StrWritingPackageData = 'Writing Package Data';
  StrWritingStressPerio = 'Writing Stress Periods';
  StrStreamTransport = 'SFT Stream Transport';
  StrWritingStressP = '    Writing Stress Period %d';
  StrWritingGridData = 'Writing Grid Data';
resourcestring
  StrNone = 'none';


  {@name is used as the default name for a new data set.

  @longcode(#
  rsNewDataSet = 'New data set';
  #)
  }
  rsNewDataSet = 'New data set';

  // @name is used to set captions for several radio buttons.
  // @Seealso(rsSetValueOfIntersected)
  // @Seealso(rsSetValueOf)
  // @Seealso(rsByInterpolation)
  // @Seealso(frmScreenObjectPropertiesUnit.TfrmScreenObjectProperties.SetCheckBoxCaptions)
  //  frmScreenObjectPropertiesUnit.TfrmScreenObjectProperties.SetCheckBoxCaptions)
  // @Seealso(frmCustomImportSimpleFileUnit.TfrmCustomImportSimpleFile.SetCheckBoxCaptions
  // frmCustomImportSimpleFileUnit.TfrmCustomImportSimpleFile.SetCheckBoxCaptions)
  // @Seealso(frmImportPointsUnits.TfrmImportPoints.SetCheckBoxCaptions
  // frmImportPointsUnits.TfrmImportPoints.SetCheckBoxCaptions)
  // @Seealso(frmImportShapefileUnit.TfrmImportShapefile.SetCheckBoxCaptions
  // frmImportShapefileUnit.TfrmImportShapefile.SetCheckBoxCaptions)
  rsSetValueOfEnclosed = 'Set values of enclosed ';
  // @name is used to set captions for several radio buttons.
  // @Seealso(rsSetValueOfEnclosed)
  // @Seealso(rsSetValueOf)
  // @Seealso(rsByInterpolation)
  // @Seealso(frmScreenObjectPropertiesUnit.TfrmScreenObjectProperties.SetCheckBoxCaptions)
  //  frmScreenObjectPropertiesUnit.TfrmScreenObjectProperties.SetCheckBoxCaptions)
  // @Seealso(frmCustomImportSimpleFileUnit.TfrmCustomImportSimpleFile.SetCheckBoxCaptions
  // frmCustomImportSimpleFileUnit.TfrmCustomImportSimpleFile.SetCheckBoxCaptions)
  // @Seealso(frmImportPointsUnits.TfrmImportPoints.SetCheckBoxCaptions
  // frmImportPointsUnits.TfrmImportPoints.SetCheckBoxCaptions)
  // @Seealso(frmImportShapefileUnit.TfrmImportShapefile.SetCheckBoxCaptions
  // frmImportShapefileUnit.TfrmImportShapefile.SetCheckBoxCaptions)
  rsSetValueOfIntersected = 'Set values of intersected ';
  // @name is used to set captions for several radio buttons.
  // @Seealso(rsSetValueOfEnclosed)
  // @Seealso(rsSetValueOfIntersected)
  // @Seealso(rsByInterpolation)
  // @Seealso(frmScreenObjectPropertiesUnit.TfrmScreenObjectProperties.SetCheckBoxCaptions)
  //  frmScreenObjectPropertiesUnit.TfrmScreenObjectProperties.SetCheckBoxCaptions)
  // @Seealso(frmCustomImportSimpleFileUnit.TfrmCustomImportSimpleFile.SetCheckBoxCaptions
  // frmCustomImportSimpleFileUnit.TfrmCustomImportSimpleFile.SetCheckBoxCaptions)
  // @Seealso(frmImportPointsUnits.TfrmImportPoints.SetCheckBoxCaptions
  // frmImportPointsUnits.TfrmImportPoints.SetCheckBoxCaptions)
  // @Seealso(frmImportShapefileUnit.TfrmImportShapefile.SetCheckBoxCaptions
  // frmImportShapefileUnit.TfrmImportShapefile.SetCheckBoxCaptions)
  rsSetValueOf = 'Set values of ';
  // @name is used to set captions for several radio buttons.
  // @Seealso(rsSetValueOfEnclosed)
  // @Seealso(rsSetValueOfIntersected)
  // @Seealso(rsSetValueOf)
  // @Seealso(frmScreenObjectPropertiesUnit.TfrmScreenObjectProperties.SetCheckBoxCaptions)
  //  frmScreenObjectPropertiesUnit.TfrmScreenObjectProperties.SetCheckBoxCaptions)
  // @Seealso(frmCustomImportSimpleFileUnit.TfrmCustomImportSimpleFile.SetCheckBoxCaptions
  // frmCustomImportSimpleFileUnit.TfrmCustomImportSimpleFile.SetCheckBoxCaptions)
  // @Seealso(frmImportPointsUnits.TfrmImportPoints.SetCheckBoxCaptions
  // frmImportPointsUnits.TfrmImportPoints.SetCheckBoxCaptions)
  // @Seealso(frmImportShapefileUnit.TfrmImportShapefile.SetCheckBoxCaptions
  // frmImportShapefileUnit.TfrmImportShapefile.SetCheckBoxCaptions)
  rsByInterpolation = ' by interpolation';

  StrLowerLimit = 'Lower limit';
  StrUpperLimit = 'Upper limit';
  StrX = 'X';
  StrY = 'Y';
  StrRow = 'Row';
  StrColumn = 'Column';
  StrLayer = 'Layer';
  StrF = 'F()';
  StrInvalidResultType = 'Invalid result type';
  StrFluidSourcesAndSi = 'Fluid Sources and Sinks';
  StrMassOrEnergySourc = 'Mass or Energy Sources and Sinks';
  StrInvalidBoundaryTim = 'Invalid boundary time';
  StrInSTheFirstSpe = 'In %s, the first specified time must be after the ini' +
  'tial time';
  StrSP_Lay_Row_Col = 'Stress Period: %0:d; Layer: %1:d; Row: %2:d; Column: ' +
  '%3:d.';

  StrSPerUnitLength = '%s  per unit length or area';
  StrTotalSPerLayer = 'Total %s (per layer)';
  StrTotalS = ' total %s (per layer)';
  StrMultiply = 'Multiply';
  StrAdd = 'Add';
  StrModelFeatures = '|Model Features';
  StrFrequency = 'Frequency';
  StrArrayOrList = 'Array or list';
  StrUnitConversionScal = 'Unit conversion scale factor (optional)';
  StrExternallyGenerated = 'Externally generated file (optional)';
  StrExternallyGeneratedSfac = 'Externally generated SFAC file (optional)';



const
  StrObjectIntersectLength = 'ObjectIntersectLength';
  StrObjectSectionIntersectLength = 'ObjectSectionIntersectLength';
  StrObjectIntersectArea = 'ObjectIntersectArea';
  StrObjectArea = 'ObjectArea';
  StrObjectLength = 'ObjectLength';
  StrLayerBoundaryPosition = 'LayerBoundaryPosition';

resourcestring
  StrStartingTime = 'Starting time';
  StrEndingTime = 'Ending time';
  StrStatus = 'Status';
  StrPumpingRate = 'Pumping rate';
  StrConductance = 'Conductance';
  StrConductanceMultipl = ' conductance multiplier';
  StrRiverStage = 'River stage';
  StrRiverBottom = 'River bottom';
  StrBoundaryHead = 'Boundary head';
  StrDrainElevation = 'Drain elevation';
  StrStartingHead = 'Starting head';
  StrEndingHead = 'Ending head';
  StrElevation = 'Elevation';
  StrMaxPumpingRate = 'Max pumping rate (QMAX)';
  StrPumpOnlyIfCropRequiresWater = 'Pump only if irrigation required in cell';
  StrFarmID =  'FarmID';
  StrStressOffset = 'Stress offset';
  StrOnlyMODPATH7CanB = 'Only MODPATH 7 can be used with MODFLOW 6.';


  StrVariance = 'Variance (0)';
  StrStdDev = 'Standard dev. (1)';
  StrCoefVar = 'Coef. of var. (2)';
  StrWt = 'Weight (3)';
  StrSqRtWt = 'Sq. rt. of weight (4)';

  // @name represents the characters used to define the end of a line.
  EndOfLine = sLineBreak;
  StrStressPeriodLabel = 'Stress Period: ';
  StrTimeStepLabel = 'Time Step: ';
  StrElapsedTimeLabel = 'Elapsed Time: ';
  StrTransportStep = 'Transport Step: ';
  StrParentModel = 'Parent model';
  StrYouNeedToSelectA = 'You need to select a row in the grid before clickin' +
  'g the Insert button.';

  StrObject0sLayerError = 'Object: %0:s, Layer: %1:d, Row: %2:d, Column: %3:d, Error: %4:s';
  StrFormulaButtonCaption = 'F()';

  StrTheFollowingObjectSutra = 'The following objects defines a transient ' +
  '%s. SUTRA will not use the transient data because transient data is not ' +
  'used in steady-state simulations. See the "Model|Sutra Options" dialog box.';
  StrNoValueAssigned = 'No value assigned';
  StrStressPeriodD = 'Stress Period %d';
  StrWhenTheGhostNode = 'When the Ghost Node Correction package is enabled w' +
  'ith the implicit option, the linear acceleration method in the Integrated' +
  ' Model Solution package must be set to BICGSTAB instead of Conjugate ' +
  'Gradient,';

  StrModelResults = 'Model Results';
  StrDensityExt = '.density';
  StrViscosityExt = '.viscosity';

const
  // On Linux, @name is used to control the access permissions of files.
  // @name has no effect in Windows.
{$IFDEF MSWINDOWS}
  ReadWritePermissions = 0;
{$ELSE}
  ReadWritePermissions = S_IREAD or S_IWRITE or S_IRGRP or S_IWGRP or S_IROTH;
{$ENDIF}

  clTransparent32 : TColor32 = 0;
  SelectEpsilon = 5;
  StrArraysExt = '.arrays';

var
  ObservationStatFlagLabels: TStringList = nil;
  PredictionStatFlagLabels: TStringList = nil;

function StrToStatFlag(Const AStatFlagLabel: string): TStatFlag;
function StatFlatToStr(AStatFlag: TStatFlag): string;
function SortLayerSorts(Item1, Item2: Pointer): Integer;
function IsNetworkDrive(const FileName: string): Boolean;
function FortranStrToFloat(AString: string): Extended;
function FortranFloatToStr(Value: Extended): string;

resourcestring
  StrNoBoundaryConditio = 'No boundary conditions assigned to the %s because' +
  ' the object does not set the values of either enclosed or intersected cel' +
  'ls.';
  StrErrorObjectDuplicateTimes = 'Error; Object = %0:s Duplicate Times = %1:s';
  StrErrorObjectEarlyTimes = 'Error; Object = %0:s Early Times = %1:s';
  StrErrorObjectLateTimes = 'Error; Object = %0:s Late Times = %1:s';
  StrObjectS = 'Object: %s';
  StrObjectSTimeG = 'Object: %0:s; Time: %1:g';
  StrAssignedBy0sWit = 'Assigned by %0:s with formula = "%1:s."';
  StrAllTheActiveCells = 'All the active cells in the following layers have ' +
  'thicknesses less than or equal to zero.';
const
  StrDensity = 'Density';
  StrViscosity = 'Viscosity';

var
  FxButton: TBitMap;

implementation


{$IFNDEF Testing}
uses Math, System.StrUtils;
{$ENDIF}

function SortLayerSorts(Item1, Item2: Pointer): Integer;
var
  LayerSort1: TLayerSort;
  LayerSort2: TLayerSort;
begin
  LayerSort1 := Item1;
  LayerSort2 := Item2;
  result := LayerSort1.ActiveCells - LayerSort2.ActiveCells;
  if result = 0 then
  begin
    result := LayerSort1.Layer - LayerSort2.Layer;
  end;
end;

function FortranStrToFloat(AString: string): Extended;
var
  OldDecimalSeparator: Char;
  SignPos: Integer;
begin
  AString := Trim(AString);
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := '.';
    AString := StringReplace(AString, ',', '.', [rfReplaceAll, rfIgnoreCase]);
    AString := StringReplace(AString, 'd', 'e', [rfReplaceAll, rfIgnoreCase]);
    SignPos := Max(PosEx('+', AString, 2), PosEx('-', AString, 2));
    if SignPos > 0 then
    begin
      if not CharInSet(AString[SignPos-1], ['e', 'E']) then
      begin
        Insert('E', AString, SignPos);
      end;
    end;
    result := StrToFloat(AString);
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;

function FortranFloatToStr(Value: Extended): string;
var
  OldDecimalSeparator: Char;
begin
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := '.';
    result := FloatToStr(Value);
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;

function StrToStatFlag(Const AStatFlagLabel: string): TStatFlag;
var
  Position: integer;
begin
  Position := ObservationStatFlagLabels.IndexOf(AStatFlagLabel);
  Assert(Position >= 0);
  result := TStatFlag(Position);
end;

function StatFlatToStr(AStatFlag: TStatFlag): string;
begin
  result := ObservationStatFlagLabels[Ord(AStatFlag)];
end;

{ TPhastCollection }

function TPhastCollection.Add: TCollectionItem;
begin
  result := inherited;
  DoOnChange;
end;

procedure TPhastCollection.Assign(Source: TPersistent);
begin
  if Source is TPhastCollection then
  begin
    Capacity := Max(Count, TPhastCollection(Source).Count);
  end;
  inherited;
  DoOnChange;
end;

procedure TPhastCollection.Clear;
begin
  inherited;
  DoOnChange;
end;

constructor TPhastCollection.Create(ItemClass: TCollectionItemClass;
  InvalidateModelEvent: TNotifyEvent);
begin
  FOnInvalidateModel := InvalidateModelEvent;
  inherited Create(ItemClass);
end;

procedure TPhastCollection.Delete(Index: Integer);
begin
  inherited;
  DoOnChange;
end;

procedure TPhastCollection.DoOnChange;
begin
  if Assigned (FOnChange) then
  begin
    FOnChange(self);
  end;
end;

function TPhastCollection.GetCount: Integer;
begin
  result := inherited Count;
end;

function TPhastCollection.GetFirst: TCollectionItem;
begin
  result := Items[0];
end;

function TPhastCollection.GetLast: TCollectionItem;
begin
  Result := Items[Count-1];
end;

function TPhastCollection.Insert(Index: Integer): TCollectionItem;
begin
  result := inherited;
  DoOnChange;
end;

procedure TPhastCollection.InvalidateModel;
begin
  if Assigned(FOnInvalidateModel) then
  begin
    FOnInvalidateModel(self);
  end;
end;

procedure TPhastCollection.Notify(Item: TCollectionItem;
  Action: Classes.TCollectionNotification);
begin
  inherited;
{$IFNDEF Testing}
  InvalidateModel;
{$ENDIF}
end;

function TPhastCollection.QueryInterface(const IID: TGUID; out Obj): HRESULT;
const
  E_NOINTERFACE = HRESULT($80004002);
begin
  if GetInterface(IID, Obj) then
    result := 0
  else
    result := E_NOINTERFACE;
end;

procedure TPhastCollection.SetCount(const Value: Integer);
var
  ExistingCount: integer;
begin
  Assert(Value >= 0);
  ExistingCount := inherited Count;
  if ExistingCount <> Value then
  begin
    while ExistingCount < Value do
    begin
      Add;
      Inc(ExistingCount);
    end;
    while ExistingCount > Value do
    begin
      Last.Free;
      Dec(ExistingCount);
    end;
    DoOnChange;
  end;
end;

procedure TPhastCollection.Sort(const AComparer: IComparer<TCollectionItem>);
begin
  inherited;
  DoOnChange;
end;

function TPhastCollection._AddRef: Integer;
begin
  result := -1;
end;

function TPhastCollection._Release: Integer;
begin
  result := -1;
end;

{ TRealStorage }

procedure TRealStorage.Assign(Source: TPersistent);
begin
  if Source is TRealStorage then
  begin
    Value := TRealStorage(Source).Value;
  end
  else
  begin
    inherited;
  end;
end;

constructor TRealStorage.Create(OnChangeEvent: TNotifyEvent);
begin
  Inherited Create;
  OnChange := OnChangeEvent;
end;

procedure TRealStorage.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Value', ReadValue, WriteValue, Value = 0);
  Filer.DefineProperty('StringValue', ReadStringValue, WriteStringValue,
   (Value <> 0) and (Abs(Value) < 1e-10));
end;

procedure TRealStorage.ReadStringValue(Reader: TReader);
begin
  Value := FortranStrToFloat(Reader.ReadString)
end;

procedure TRealStorage.ReadValue(Reader: TReader);
begin
  Value := Reader.ReadFloat;
end;

procedure TRealStorage.SetValue(Value: real);
begin
  // prevent rounding errors in saved files from being used.
  Value := FortranStrToFloat(FortranFloatToStr(Value));
  if FValue <> Value then
  begin
    FValue := Value;
    if Assigned(OnChange) then
    begin
      OnChange(self);
    end;
  end;
end;

var
  LFormatSettings: TFormatSettings;

function GetLFormatSettings: TFormatSettings;
begin
  result := LFormatSettings;
end;

procedure TRealStorage.WriteStringValue(Writer: TWriter);
begin
  Writer.WriteString(FloatToStrF(Value, ffGeneral, 16, 18, LFormatSettings));
end;

procedure TRealStorage.WriteValue(Writer: TWriter);
begin
  Writer.WriteFloat(Value);
end;

{ TGoPhastPersistent }

constructor TGoPhastPersistent.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create;
  FOnInvalidateModel := InvalidateModelEvent;
end;

procedure TGoPhastPersistent.InvalidateModel;
begin
  if Assigned(FOnInvalidateModel) then
  begin
    FOnInvalidateModel(self);
  end;
end;

procedure TGoPhastPersistent.OnChangeEventHander(Sender: TObject);
begin
  InvalidateModel;
end;

function TGoPhastPersistent.QueryInterface(const IID: TGUID; out Obj): HRESULT;
const
  E_NOINTERFACE = HRESULT($80004002);
begin
  if GetInterface(IID, Obj) then
    result := 0
  else
    result := E_NOINTERFACE;
end;

procedure TGoPhastPersistent.SetAnsiStringProperty(var AField: AnsiString;
  const NewValue: AnsiString);
begin
  if AField <> NewValue then
  begin
    AField := NewValue;
    InvalidateModel;
  end;
end;

procedure TGoPhastPersistent.SetBooleanProperty(var AField: boolean;
  const NewValue: boolean);
begin
  if AField <> NewValue then
  begin
    AField := NewValue;
    InvalidateModel;
  end;
end;

procedure TGoPhastPersistent.SetCharacterProperty(var AField: Char;
  const NewValue: Char);
begin
  if AField <> NewValue then
  begin
    AField := NewValue;
    InvalidateModel;
  end;
end;

procedure TGoPhastPersistent.SetColorProperty(var AField: TColor;
  const NewValue: TColor);
begin
  if AField <> NewValue then
  begin
    AField := NewValue;
    InvalidateModel;
  end;
end;

procedure TGoPhastPersistent.SetDataTimeProperty(var AField: TDateTime;
  const NewValue: TDateTime);
begin
  if AField <> NewValue then
  begin
    AField := NewValue;
    InvalidateModel;
  end;
end;

procedure TGoPhastPersistent.SetIntegerProperty(var AField: integer;
  const NewValue: integer);
begin
  if AField <> NewValue then
  begin
    AField := NewValue;
    InvalidateModel;
  end;
end;

procedure TGoPhastPersistent.SetPointProperty(var AField: TPoint;
  const NewValue: TPoint);
begin
  SetIntegerProperty(AField.X, NewValue.X);
  SetIntegerProperty(AField.Y, NewValue.Y);
end;

procedure TGoPhastPersistent.SetRealProperty(var AField: double;
  const NewValue: double);
begin
  if AField <> NewValue then
  begin
    AField := NewValue;
    InvalidateModel;
  end;
end;

procedure TGoPhastPersistent.SetStringProperty(var AField: string;
  const NewValue: string);
begin
  if AField <> NewValue then
  begin
    AField := NewValue;
    InvalidateModel;
  end;
end;

function TGoPhastPersistent._AddRef: Integer;
begin
  result := -1;
end;

function TGoPhastPersistent._Release: Integer;
begin
  result := -1;
end;

function EvalAtToString(const Eval: TEvaluatedAt; const Model: TModelSelection;
  const Plural, TitleCase: boolean): string;
begin
  result := '';
  case Model of
    msUndefined, msPhast, msSutra22, msSutra30, msSutra40:
      begin
        case Eval of
          eaBlocks:
            begin
              if TitleCase then
              begin
                result := 'Element';
              end
              else
              begin
                result := 'element';
              end;
            end;
          eaNodes:
            begin
              if TitleCase then
              begin
                result := 'Node';
              end
              else
              begin
                result := 'node';
              end;
            end;
          else
            Assert(False);
        end;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msFootPrint, msModflow2015, msModflowOwhm2:
      begin
        case Eval of
          eaBlocks:
            begin
              if TitleCase then
              begin
                result := 'Cell';
              end
              else
              begin
                result := 'cell';
              end;
            end;
          eaNodes:
            begin
              if TitleCase then
              begin
                result := 'Cell corner';
              end
              else
              begin
                result := 'cell corner';
              end;
            end;
          else
            Assert(False);
        end;
      end;
    else Assert(False);
  end;
  if Plural then
  begin
    result := result + 's';
  end;
end;

{ TPhastCollectionItem }

procedure TPhastCollectionItem.BeginUpdate;
begin
  // do nothing;
end;

procedure TPhastCollectionItem.EndUpdate;
begin
  // do nothing;
end;

function TPhastCollectionItem.GetOnInvalidateModel: TNotifyEvent;
begin
  if Collection <> nil then
  begin
    Result := (Collection as TPhastCollection).OnInvalidateModel;
  end
  else
  begin
    Result := nil;
  end;
end;

procedure TPhastCollectionItem.InvalidateModel;
var
  PhastCollection: TPhastCollection;
begin
  if Collection <> nil then
  begin
    PhastCollection := Collection as TPhastCollection;
    PhastCollection.Update(self);
    PhastCollection.InvalidateModel;
  end;
end;

function ValidName(const OriginalName: string): string;
  function Alpha(C: Char): Boolean; inline;
  begin
//    Result := TCharacter.IsLetter(C) or (C = '_');
    Result := C.IsLetter or (C = '_');
  end;

  function AlphaNumeric(C: Char): Boolean; inline;
  begin
//    Result := TCharacter.IsLetterOrDigit(C) or (C = '_');
    Result := C.IsLetterOrDigit or (C = '_');
  end;
var
  Index: integer;
  AChar: Char;
begin
  // See also PhastModelUnit.GenerateNewName

  result :=  Trim(OriginalName);
  result := StringReplace(result, ' ', '_', [rfReplaceAll]);
  result := StringReplace(result, '"', '', [rfReplaceAll]);
  result := StringReplace(result, '''', '', [rfReplaceAll]);
  result := StringReplace(result, '/', '_', [rfReplaceAll]);
  result := StringReplace(result, '\', '_', [rfReplaceAll]);
  try
    if IsValidIdent(Result, False) then
    begin
      Exit;
    end
    else
    begin
      if Length(result) >= 1 then
      begin
        AChar := result[1];
        if not Alpha(AChar) then
          begin
            result := '_' + result;
          end;
      end;
      for Index := 1 to Length(result) do
      begin
        AChar := result[Index];
        if Index = 1 then
        begin
          if not Alpha(AChar) then
          begin
            result[Index] := '_';
          end;
        end
        else
        begin
          if not AlphaNumeric(AChar) then
          begin
            result[Index] := '_';
          end;
        end;
      end;
    end;
  finally
    if result = '' then
    begin
      result := '_';
    end
  end;
end;

function RightCopy(const Source: string; LengthToCopy: integer): string;
var
  Start: Integer;
begin
  Start := Length(Source) - LengthToCopy + 1;
  if Start < 1 then
  begin
    Start := 1;
  end;
  result := Copy(Source, Start, LengthToCopy);
end;

procedure TPhastCollectionItem.SetBooleanProperty(var AField: boolean;
  const NewValue: boolean);
begin
  if AField <> NewValue then
  begin
    BeginUpdate;
    try
      AField := NewValue;
      InvalidateModel;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TPhastCollectionItem.SetIntegerProperty(var AField: integer;
  const NewValue: integer);
begin
  if AField <> NewValue then
  begin
    BeginUpdate;
    try
      AField := NewValue;
      InvalidateModel;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TPhastCollectionItem.SetRealProperty(var AField: double;
  const NewValue: double);
begin
  if AField <> NewValue then
  begin
    BeginUpdate;
    try
      AField := NewValue;
      InvalidateModel;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TPhastCollectionItem.SetStringProperty(var AField: string;
  const NewValue: string);
begin
  if AField <> NewValue then
  begin
    BeginUpdate;
    try
      AField := NewValue;
      InvalidateModel;
    finally
      EndUpdate;
    end;
  end;
end;

procedure InitializeStatTypeLabels;
begin
  ObservationStatFlagLabels := TStringList.Create;
  PredictionStatFlagLabels := TStringList.Create;

  ObservationStatFlagLabels.Add(StrVariance);
  ObservationStatFlagLabels.Add(StrStdDev);
  ObservationStatFlagLabels.Add(StrCoefVar);
  ObservationStatFlagLabels.Add(StrWt);
  ObservationStatFlagLabels.Add(StrSqRtWt);

  PredictionStatFlagLabels.Add(StrVariance);
  PredictionStatFlagLabels.Add(StrStdDev);
end;

function TBaseModel.GetComponentState: TComponentState;
begin
  result := inherited ComponentState
end;

function TBaseModel.GetInvalidate: TNotifyEvent;
begin
  result := DoInvalidate;
end;


procedure TBaseModel.SetUpToDate(const Value : boolean);
begin
  FUpToDate := Value;
end;

function TBaseModel._AddRef: Integer;
begin
  result := -1;
end;

function TBaseModel._Release: Integer;
begin
  result := -1;
end;

procedure TBaseModel.DoInvalidate(Sender: TObject);
begin
  UpToDate := False;
end;

function OrientationToViewDirection(Orientation: TDataSetOrientation): TViewDirection;
begin
  result := vdTop;
  case Orientation of
    dsoTop: result := vdTop;
    dsoFront: result := vdFront;
    dsoSide: result := vdSide;
    else Assert(False);
  end;
end;


{ TStringStorage }

procedure TStringStorage.Assign(Source: TPersistent);
begin
  if Source is TStringStorage then
  begin
    Value := TStringStorage(Source).Value;
  end
  else
  begin
    inherited;
  end;
end;

procedure TStringStorage.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Value', ReadValue, WriteValue, Value = '')
end;

procedure TStringStorage.ReadValue(Reader: TReader);
begin
  Value := Reader.ReadString;
end;

procedure TStringStorage.SetValue(const Value: string);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    if Assigned(OnChange) then
    begin
      OnChange(self);
    end;
  end;
end;

procedure TStringStorage.WriteValue(Writer: TWriter);
begin
  Writer.WriteString(Value);
end;

{ TRealItem }

procedure TRealItem.Assign(Source: TPersistent);
begin
  if Source is TRealItem then
  begin
    Value := TRealItem(Source).Value;
  end
  else
  begin
    inherited;
  end;
  // if Assign is updated, update IsSame too.
end;

procedure TRealItem.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Value', ReadValue, WriteValue, Value = 0);
  Filer.DefineProperty('StringValue', ReadStringValue, WriteStringValue,
   (Value <> 0) and (Abs(Value) < 1e-10));
end;

function TRealItem.GetValue: double;
begin
  result := Min(FValue,  1.78E308);
  result := Max(result, -1.78E308);
end;

function TRealItem.IsSame(Item: TRealItem): Boolean;
begin
  Result := Value = Item.Value;
end;

procedure TRealItem.ReadStringValue(Reader: TReader);
begin
  Value := StrToFloat(Reader.ReadString)
end;

procedure TRealItem.ReadValue(Reader: TReader);
begin
  Value := Reader.ReadFloat;
end;

procedure TRealItem.SetValue(const Value: double);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    InvalidateModel;
    if Assigned(OnChange) then
    begin
      OnChange(self);
    end;
  end;
end;

procedure TRealItem.WriteStringValue(Writer: TWriter);
begin
  Writer.WriteString(FloatToStrF(Value, ffGeneral, 16, 18, LFormatSettings));
end;

procedure TRealItem.WriteValue(Writer: TWriter);
begin
  Writer.WriteFloat(Value);
end;

{ TRealCollection }

function TRealCollection.Add: TRealItem;
begin
  result := inherited Add as TRealItem;
  result.FValue := InitialValue;
end;

constructor TRealCollection.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(TRealItem, InvalidateModelEvent);
end;

constructor TRealCollection.Create(InvalidateModelEvent: TNotifyEvent; Values: TOneDRealArray);
var
  index: Integer;
begin
  Create(InvalidateModelEvent);
  Capacity := Length(Values);
  for index := 0 to Length(Values) - 1 do
  begin
    Add.Value := Values[index];
  end;
end;

function TRealCollection.First: TRealItem;
begin
  if Count > 0 then
  begin
    result := Items[0];
  end
  else
  begin
    result := nil;
  end;
end;

function TRealCollection.GetItems(Index: Integer): TRealItem;
begin
  result := inherited Items[index] as TRealItem
end;

function TRealCollection.GetValue(Index: Integer): Double;
begin
  Result := Items[Index].Value;
end;

function TRealCollection.IsSame(RealCollection: TRealCollection): Boolean;
var
  index: Integer;
begin
  // if Assign is updated, update IsSame too.
  result := (Count = RealCollection.Count)
    and (InitialValue = RealCollection.InitialValue);
  if result then
  begin
    for index := 0 to Count - 1 do
    begin
      result := Items[index].IsSame(RealCollection[index]);
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

function TRealCollection.Last: TRealItem;
begin
  if Count > 0 then
  begin
    result := Items[Count-1];
  end
  else
  begin
    result := nil;
  end;
end;

procedure TRealCollection.SetInitialValue(const Value: Real);
begin
 if FInitialValue <> Value then
 begin
   FInitialValue := Value;
   InvalidateModel;
 end;

end;

procedure TRealCollection.SetItems(Index: Integer; const Value:TRealItem );
begin
  inherited Items[index] := Value;
end;

procedure TRealCollection.SetValue(Index: Integer; const Value: Double);
begin
  Items[Index].Value := Value;
end;

Function CompareRealItems(Item1, Item2: Pointer): integer;
var
  RealItem1: TRealItem;
  RealItem2: TRealItem;
begin
  RealItem1 := Item1;
  RealItem2 := Item2;
  result := Sign(RealItem1.Value - RealItem2.Value);
end;

procedure TRealCollection.Sort;
var
  AList: TList;
  Index: Integer;
  AnItem: TRealItem;
begin
  AList := TList.Create;
  try
    for Index := 0 to Count - 1 do
    begin
      AList.Add(Items[Index]);
    end;
    AList.Sort(CompareRealItems);
    for Index := 0 to AList.Count - 1 do
    begin
      AnItem := AList[Index];
      AnItem.Index := Index
    end;
  finally
    AList.Free;
  end;
end;

{ TIntegerItem }

procedure TIntegerItem.Assign(Source: TPersistent);
begin
  if Source is TIntegerItem then
  begin
    Value := TIntegerItem(Source).Value;
  end
  else
  begin
    inherited;
  end;
  // if Assign is updated, update IsSame too.
end;

function TIntegerItem.IsSame(Item: TIntegerItem): Boolean;
begin
  Result := Value = Item.Value;
end;

procedure TIntegerItem.SetValue(const Value: integer);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    InvalidateModel;
    if Assigned(OnChange) then
    begin
      OnChange(self);
    end;
  end;
end;

{ TIntegerCollection }

function TIntegerCollection.Add: TIntegerItem;
begin
  result := inherited Add as TIntegerItem;
  result.FValue := InitialValue;
end;

constructor TIntegerCollection.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(TIntegerItem, InvalidateModelEvent);
end;

function TIntegerCollection.GetFirst: TIntegerItem;
begin
  result := inherited First as TIntegerItem;
end;

function TIntegerCollection.GetItems(Index: Integer): TIntegerItem;
begin
  result := inherited Items[index] as TIntegerItem
end;

function TIntegerCollection.GetLast: TIntegerItem;
begin
  result := inherited Last as TIntegerItem;
end;

function TIntegerCollection.IndexOf(AValue: Integer): integer;
var
  index: Integer;
begin
  result := -1;
  for index := 0 to Count - 1 do
  begin
    if Items[index].Value = AValue then
    begin
      result := index;
      Exit;
    end;
  end;
end;

function TIntegerCollection.IsSame(IntegerCollection: TIntegerCollection): Boolean;
var
  index: Integer;
begin
  // if Assign is updated, update IsSame too.
  result := (Count = IntegerCollection.Count)
    and (InitialValue = IntegerCollection.InitialValue);
  if result then
  begin
    for index := 0 to Count - 1 do
    begin
      result := Items[index].IsSame(IntegerCollection[index]);
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TIntegerCollection.SetInitialValue(const Value: integer);
begin
  FInitialValue := Value;
end;

procedure TIntegerCollection.SetItems(Index: Integer;
  const Value: TIntegerItem);
begin
  inherited Items[index] := Value;
end;

Function CompareIntegerItems(Item1, Item2: Pointer): integer;
var
  IntItem1: TIntegerItem;
  IntItem2: TIntegerItem;
begin
  IntItem1 := Item1;
  IntItem2 := Item2;
  result := Sign(IntItem1.Value - IntItem2.Value);
end;


procedure TIntegerCollection.Sort;
var
  AList: TList;
  Index: Integer;
  AnItem: TIntegerItem;
begin
  AList := TList.Create;
  try
    for Index := 0 to Count - 1 do
    begin
      AList.Add(Items[Index]);
    end;
    AList.Sort(CompareIntegerItems);
    for Index := 0 to AList.Count - 1 do
    begin
      AnItem := AList[Index];
      AnItem.Index := Index
    end;
  finally
    AList.Free;
  end;
end;

{ TModelData }

function TModelData.ModelLine: string;
begin
  result := '';
  case ModelType of
    mtGroundWaterFlow: result := '  GWF6 ';
    mtGroundWaterTransport: result := '  GWT6 ';
    else Assert(False);
  end;
  result := result + '''' + ExtractFileName(ModelNameFile) + ''' ''' + ModelName + '''';
end;

{ TBoundaryNode }

class function TBoundaryNode.Create(NodeNumber: Integer; PressureOrFlow,
  TempOrConc: Double; UseBCTime: Boolean; PressureOrFlowFormula,
  TempOrConcFormula: string): TBoundaryNode;
begin
  Result.NodeNumber := NodeNumber;
  Result.PressureOrFlow := PressureOrFlow;
  Result.TempOrConc := TempOrConc;
  Result.UseBCTime := UseBCTime;
  Result.PressureOrFlowFormula := PressureOrFlowFormula;
  Result.TempOrConcFormula := TempOrConcFormula;
end;

function IsNetworkDrive(const FileName: string): Boolean;
var
  Drive: string;
begin
  Drive := ExtractFileDrive(FileName);
  result := Copy(Drive, 1, 2) = '\\';
end;

{ TInterfacedPhastCollectionItem }

function TInterfacedPhastCollectionItem.QueryInterface(const IID: TGUID;
  out Obj): HRESULT;
const
  E_NOINTERFACE = HRESULT($80004002);
begin
  if GetInterface(IID, Obj) then
    result := 0
  else
    result := E_NOINTERFACE;
end;

function TInterfacedPhastCollectionItem._AddRef: Integer;
begin
  result := -1;
end;

function TInterfacedPhastCollectionItem._Release: Integer;
begin
  result := -1;
end;

resourcestring
  StrInches = 'Inches';
  StrFeet = 'Feet';
  StrMiles = 'Miles';
  StrMillimeters = 'Millimeters';
  StrCentimeters = 'Centimeters';
  StrMeters = 'Meters';
  StrKilometers = 'Kilometers';
  StrSeconds = 'Seconds';
  StrMinutes = 'Minutes';
  StrHours = 'Hours';
  StrDays = 'Days';
  StrYears = 'Years';
  StrUndefined = 'Undefined';

procedure AssignModflowLengthUnitStringsToPicklist(LengthPickList: TStrings);
begin
  LengthPickList.Clear;
  LengthPickList.Add(StrUndefined);
  LengthPickList.Add(StrFeet);
  LengthPickList.Add(StrMeters);
  LengthPickList.Add(StrCentimeters);
end;

procedure AssignModflowTimeUnitStringsToPicklist(TimePickList: TStrings);
begin
  TimePickList.Clear;
  TimePickList.Add(StrUndefined);
  TimePickList.Add(StrSeconds);
  TimePickList.Add(StrMinutes);
  TimePickList.Add(StrHours);
  TimePickList.Add(StrDays);
  TimePickList.Add(StrYears);
end;

procedure AssignTypicalTimeUnitStringsToPicklist(TimePickList: TStrings);
begin
  TimePickList.Clear;
  TimePickList.Add(StrSeconds);
  TimePickList.Add(StrMinutes);
  TimePickList.Add(StrHours);
  TimePickList.Add(StrDays);
  TimePickList.Add(StrYears);
end;

procedure AssignTypicalLengthUnitStringsToPicklist(LengthPickList: TStrings);
begin
  LengthPickList.Clear;
  LengthPickList.Add(StrInches);
  LengthPickList.Add(StrFeet);
  LengthPickList.Add(StrMiles);
  LengthPickList.Add(StrMillimeters);
  LengthPickList.Add(StrCentimeters);
  LengthPickList.Add(StrMeters);
  LengthPickList.Add(StrKilometers);
end;

procedure AssignLengthUnitStringsToPicklist(LengthPickList: TStrings; ModelType: TModelSelection);
begin
  if ModelType in ModflowSelection then
  begin
    AssignModflowLengthUnitStringsToPicklist(LengthPickList);
  end
  else
  begin
    AssignTypicalLengthUnitStringsToPicklist(LengthPickList);
  end;
end;

procedure AssignTimeUnitStringsToPicklist(TimePickList: TStrings; ModelType: TModelSelection);
begin
  if ModelType in ModflowSelection then
  begin
    AssignModflowTimeUnitStringsToPicklist(TimePickList);
  end
  else
  begin
    AssignTypicalTimeUnitStringsToPicklist(TimePickList);
  end;
end;

{ TElementOutline }

function TElementOutline.GetCount: Integer;
begin
  result := Length(FOutline);
end;

function TElementOutline.GetPoint(Index: Integer): TPoint2D;
begin
  result := FOutline[Index];
end;

function TElementOutline.GetSegment(Index: Integer): TSegment2D;
begin
  result[1] := FOutline[Index];
  if Index < Count -1 then
  begin
    result[2] := FOutline[Index+1];
  end
  else
  begin
    result[2] := FOutline[0];
  end;
end;

procedure TElementOutline.SetCount(const Value: Integer);
begin
  SetLength(FOutline, Value)
end;

procedure TElementOutline.SetPoint(Index: Integer; const Value: TPoint2D);
begin
  FOutline[Index] := Value;
end;


{ TScreenObjectOwnerCollection }

//constructor TScreenObjectOwnerCollection.Create(ItemClass: TCollectionItemClass;
//  InvalidateModelEvent: TNotifyEvent; ScreenObject: TObject);
//begin
//  inherited Create(ItemClass, InvalidateModelEvent);
//  if ScreenObject <> nil then
//  begin
//    Assert(ScreenObject is TScreenObject);
//  end;
//  FScreenObject := ScreenObject;
//end;

{ TGwtCellData }

procedure TGwtCellData.Assign(const Item: TGwtCellData);
begin
  self := Item;
  SetLength(Values, Length(Values));
  SetLength(ValueAnnotations, Length(ValueAnnotations));
  SetLength(ValuePestNames, Length(ValuePestNames));
  SetLength(ValuePestSeriesNames, Length(ValuePestSeriesNames));
  SetLength(ValuePestSeriesMethods, Length(ValuePestSeriesMethods));
  SetLength(ValueTimeSeriesNames, Length(ValueTimeSeriesNames));
end;

procedure TGwtCellData.Cache(Comp: TCompressionStream; Strings: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  Count := Length(Values);
  WriteCompInt(Comp, Count);
  for Index := 0 to Count - 1 do
  begin
    WriteCompReal(Comp, Values[Index]);
  end;
  for Index := 0 to Count - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(ValueAnnotations[Index]));
  end;
  for Index := 0 to Count - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(ValuePestNames[Index]));
  end;
  for Index := 0 to Count - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(ValuePestSeriesNames[Index]));
  end;
  for Index := 0 to Count - 1 do
  begin
    WriteCompInt(Comp, Ord(ValuePestSeriesMethods[Index]));
  end;
  for Index := 0 to Count - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(ValueTimeSeriesNames[Index]));
  end;

end;

procedure TGwtCellData.RecordStrings(Strings: TStringList);
var
  Index: Integer;
begin
  for Index := 0 to Length(ValueAnnotations) - 1 do
  begin
    Strings.Add(ValueAnnotations[Index]);
  end;
  for Index := 0 to Length(ValuePestNames) - 1 do
  begin
    Strings.Add(ValuePestNames[Index]);
  end;
  for Index := 0 to Length(ValuePestSeriesNames) - 1 do
  begin
    Strings.Add(ValuePestSeriesNames[Index]);
  end;
  for Index := 0 to Length(ValueTimeSeriesNames) - 1 do
  begin
    Strings.Add(ValueTimeSeriesNames[Index]);
  end;
end;

procedure TGwtCellData.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
var
  Count: Integer;
  Index: Integer;
begin
  Count := ReadCompInt(Decomp);
  SetLength(Values, Count);
  for Index := 0 to Count - 1 do
  begin
    Values[Index] := ReadCompReal(Decomp);
  end;
  SetLength(ValueAnnotations, Count);
  for Index := 0 to Count - 1 do
  begin
    ValueAnnotations[Index] := Annotations[ReadCompInt(Decomp)];
  end;
  SetLength(ValuePestNames, Count);
  for Index := 0 to Count - 1 do
  begin
    ValuePestNames[Index] := Annotations[ReadCompInt(Decomp)];
  end;
  SetLength(ValuePestSeriesNames, Count);
  for Index := 0 to Count - 1 do
  begin
    ValuePestSeriesNames[Index] := Annotations[ReadCompInt(Decomp)];
  end;
  SetLength(ValuePestSeriesMethods, Count);
  for Index := 0 to Count - 1 do
  begin
    ValuePestSeriesMethods[Index] := TPestParamMethod(ReadCompInt(Decomp));
  end;
  SetLength(ValueTimeSeriesNames, Count);
  for Index := 0 to Count - 1 do
  begin
    ValueTimeSeriesNames[Index] := Annotations[ReadCompInt(Decomp)];
  end;
end;

procedure TGwtCellData.SetSpeciesCount(const Value: Integer);
begin
  if FSpeciesCount <> Value then
  begin
    FSpeciesCount := Value;
    SetLength(Values, FSpeciesCount);
    SetLength(ValueAnnotations, FSpeciesCount);
    SetLength(ValuePestNames, FSpeciesCount);
    SetLength(ValuePestSeriesNames, FSpeciesCount);
    SetLength(ValuePestSeriesMethods, FSpeciesCount);
    SetLength(ValueTimeSeriesNames, FSpeciesCount);
  end;
end;

procedure WriteCompInt(Stream: TStream; Value: integer);
begin
  Stream.Write(Value, SizeOf(Value));
end;

function ReadCompInt(Stream: TStream): integer;
begin
  Stream.Read(result, SizeOf(result));
end;

procedure WriteCompReal(Stream: TStream; Value: double);
begin
  Stream.Write(Value, SizeOf(Value));
end;

function ReadCompReal(Stream: TStream): double;
begin
  Stream.Read(result, SizeOf(result));
end;

procedure WriteCompBoolean(Stream: TStream; Value: Boolean);
begin
  Stream.Write(Value, SizeOf(Value));
end;

function ReadCompBoolean(Stream: TStream): Boolean;
begin
  Stream.Read(result, SizeOf(result));
end;

procedure WriteCompString(Stream: TStream; Value: string);
var
  StringLength: integer;
begin
  StringLength := Length(Value);
  WriteCompInt(Stream, StringLength);
  if StringLength > 0 then
  begin
    Stream.WriteBuffer(Pointer(Value)^, ByteLength(Value));
  end;
end;

function ReadCompStringSimple(Stream: TStream): string;
var
  StringLength: Integer;
begin
  Stream.Read(StringLength, SizeOf(StringLength));
  if StringLength > 0 then
  begin
    SetString(result, nil, StringLength);
    Stream.Read(Pointer(result)^, StringLength * SizeOf(Char));
  end
  else
  begin
    result := ''
  end;
end;

function ReadCompString(Stream: TStream; Annotations: TStringList): string;
var
  StringPostion: integer;
begin
  result := ReadCompStringSimple(Stream);
  StringPostion := Annotations.IndexOf(result);
  if StringPostion < 0 then
  begin
    Annotations.Add(result);
  end
  else
  begin
    result := Annotations[StringPostion]
  end;
end;

{ TLandUseData }

procedure TLandUseData.Assign(const Item: TLandUseData);
begin
  self := Item;
  SetLength(Values, Length(Values));
  SetLength(IntValues, Length(IntValues));
  SetLength(ValueAnnotations, Length(ValueAnnotations));
  SetLength(ValuePestNames, Length(ValuePestNames));
  SetLength(ValuePestSeriesNames, Length(ValuePestSeriesNames));
  SetLength(ValuePestSeriesMethods, Length(ValuePestSeriesMethods));
  SetLength(ValueTimeSeriesNames, Length(ValueTimeSeriesNames));
end;

procedure TLandUseData.Cache(Comp: TCompressionStream; Strings: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  Count := Length(Values);
  WriteCompInt(Comp, Count);
  for Index := 0 to Count - 1 do
  begin
    WriteCompReal(Comp, Values[Index]);
  end;
  for Index := 0 to Count - 1 do
  begin
    WriteCompInt(Comp, IntValues[Index]);
  end;
  for Index := 0 to Count - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(ValueAnnotations[Index]));
  end;
  for Index := 0 to Count - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(ValuePestNames[Index]));
  end;
  for Index := 0 to Count - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(ValuePestSeriesNames[Index]));
  end;
  for Index := 0 to Count - 1 do
  begin
    WriteCompInt(Comp, Ord(ValuePestSeriesMethods[Index]));
  end;
  for Index := 0 to Count - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(ValueTimeSeriesNames[Index]));
  end;
end;

procedure TLandUseData.RecordStrings(Strings: TStringList);
var
  Index: Integer;
begin
  for Index := 0 to Length(ValueAnnotations) - 1 do
  begin
    Strings.Add(ValueAnnotations[Index]);
  end;
  for Index := 0 to Length(ValuePestNames) - 1 do
  begin
    Strings.Add(ValuePestNames[Index]);
  end;
  for Index := 0 to Length(ValuePestSeriesNames) - 1 do
  begin
    Strings.Add(ValuePestSeriesNames[Index]);
  end;
  for Index := 0 to Length(ValueTimeSeriesNames) - 1 do
  begin
    Strings.Add(ValueTimeSeriesNames[Index]);
  end;
end;

procedure TLandUseData.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
var
  Count: Integer;
  Index: Integer;
begin
  Count := ReadCompInt(Decomp);
  SetLength(Values, Count);
  for Index := 0 to Count - 1 do
  begin
    Values[Index] := ReadCompReal(Decomp);
  end;
  SetLength(IntValues, Count);
  for Index := 0 to Count - 1 do
  begin
    IntValues[Index] := ReadCompInt(Decomp);
  end;
  SetLength(ValueAnnotations, Count);
  for Index := 0 to Count - 1 do
  begin
    ValueAnnotations[Index] := Annotations[ReadCompInt(Decomp)];
  end;
  SetLength(ValuePestNames, Count);
  for Index := 0 to Count - 1 do
  begin
    ValuePestNames[Index] := Annotations[ReadCompInt(Decomp)];
  end;
  SetLength(ValuePestSeriesNames, Count);
  for Index := 0 to Count - 1 do
  begin
    ValuePestSeriesNames[Index] := Annotations[ReadCompInt(Decomp)];
  end;
  SetLength(ValuePestSeriesMethods, Count);
  for Index := 0 to Count - 1 do
  begin
    ValuePestSeriesMethods[Index] := TPestParamMethod(ReadCompInt(Decomp));
  end;
  SetLength(ValueTimeSeriesNames, Count);
  for Index := 0 to Count - 1 do
  begin
    ValueTimeSeriesNames[Index] := Annotations[ReadCompInt(Decomp)];
  end;
end;

procedure TLandUseData.SetLandUseCount(const Value: Integer);
begin
  if FLandUseCount <> Value then
  begin
    FLandUseCount := Value;
    SetLength(Values, FLandUseCount);
    SetLength(IntValues, FLandUseCount);
    SetLength(ValueAnnotations, FLandUseCount);
    SetLength(ValuePestNames, FLandUseCount);
    SetLength(ValuePestSeriesNames, FLandUseCount);
    SetLength(ValuePestSeriesMethods, FLandUseCount);
    SetLength(ValueTimeSeriesNames, FLandUseCount);
  end;
end;

{ TCellData }

procedure TCellData.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompReal(Comp, Value);
  WriteCompInt(Comp, Strings.IndexOf(ValueAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(ValuePestName));
  WriteCompInt(Comp, Strings.IndexOf(ValuePestSeriesName));
  WriteCompInt(Comp, Ord(ValuePestSeriesMethod));
  WriteCompInt(Comp, Strings.IndexOf(ValueTimeSeriesName));
end;

procedure TCellData.RecordStrings(Strings: TStringList);
begin
  Strings.Add(ValueAnnotation);
  Strings.Add(ValuePestName);
  Strings.Add(ValuePestSeriesName);
  Strings.Add(ValueTimeSeriesName);
end;

procedure TCellData.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Value := ReadCompReal(Decomp);
  ValueAnnotation := Annotations[ReadCompInt(Decomp)];
  ValuePestName := Annotations[ReadCompInt(Decomp)];
  ValuePestSeriesName := Annotations[ReadCompInt(Decomp)];
  ValuePestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  ValueTimeSeriesName := Annotations[ReadCompInt(Decomp)];
end;

{ TCellDataArray }

procedure TCellDataArray.Assign(const Item: TCellDataArray);
begin
  Self := Item;
  PropertyCount := Item.PropertyCount;
end;

procedure TCellDataArray.Cache(Comp: TCompressionStream; Strings: TStringList);
var
  Index: Integer;
begin
  WriteCompInt(Comp, PropertyCount);
  for Index := 0 to Length(Values) - 1 do
  begin
    Values[Index].Cache(Comp, Strings)
  end;
end;

function TCellDataArray.GetPropertyCount: Integer;
begin
  Result := Length(Values);
end;

procedure TCellDataArray.RecordStrings(Strings: TStringList);
var
  Index: Integer;
begin
  for Index := 0 to Length(Values) - 1 do
  begin
    Values[Index].RecordStrings(Strings)
  end;
end;

procedure TCellDataArray.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
begin
  PropertyCount := ReadCompInt(Decomp);
  for Index := 0 to PropertyCount - 1 do
  begin
    Values[Index].Restore(Decomp, Annotations);
  end;
end;

procedure TCellDataArray.SetPropertyCount(const Value: Integer);
begin
  SetLength(Values, Value)
end;

initialization
  InitializeStatTypeLabels;
  LFormatSettings := TFormatSettings.Create('en-US'); // do not localize
  LFormatSettings.DecimalSeparator := AnsiChar('.');
  FxButton := TBitMap.Create;
  FxButton.Canvas.Font.Name := 'Arial';
  FxButton.Canvas.Font.Size := -14;
  FxButton.Width := 22;
  FxButton.Height := 22;
  FxButton.Canvas.TextOut(0,0, 'F()');

finalization
  ObservationStatFlagLabels.Free;
  PredictionStatFlagLabels.Free;
  FxButton.Free;

end.

