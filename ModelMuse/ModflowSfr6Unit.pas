unit ModflowSfr6Unit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, ModflowCellUnit,
  ModflowBoundaryUnit, FormulaManagerUnit, OrderedCollectionUnit, GoPhastTypes,
  System.Generics.Collections, SubscriptionUnit, RbwParser;

type
  TSfrOb = (soStage, soExtInflow, soInflow, soFromMvr, soRainfall, soRunoff, soSfr,
    soEvaporation, soOutflow, soExternalOutflow, soToMvr, soUpstreamFlow,
    soDownstreamFlow);
  TSfrObs = set of TSfrOb;

  TSfrObsLocation = (solAll, solFirst, solLast, solIndividual);

  // ssInactive = Stream is inactive.
  // ssActive = stage is calculated.
  // ssSimple = stage is specified.
  TStreamStatus = (ssInactive, ssActive, ssSimple);
  TGwtStreamStatus = (gssInactive, gssActive, gssConstant);

  TGwtStreamStatusArray = array of TGwtStreamStatus;

  TGwtStrStatusItem = class(TCollectionItem)
  private
    FGwtStreamStatus: TGwtStreamStatus;
    procedure SetGwtStreamStatus(const Value: TGwtStreamStatus);
  public
    procedure Assign(Source: TPersistent); override;
  published
    Property GwtStreamStatus: TGwtStreamStatus read FGwtStreamStatus write SetGwtStreamStatus;
  end;

  TGwtStrStatusCollection = class(TCollection)
  private
    function GetItems(Index: Integer): TGwtStrStatusItem;
    procedure SetItems(Index: Integer; const Value: TGwtStrStatusItem);
  public
    constructor Create;
    property Items[Index: Integer]: TGwtStrStatusItem read GetItems write SetItems; default;
  end;

  TSfrMF6Record = record
    Cell: TCellLocation;
    Inflow: double;
    Rainfall: double;
    Evaporation: double;
    Runoff: double;
    UpstreamFraction: double;
    Stage: Double;
    Roughness: Double;
    Diversions: array of double;

    InflowAnnotation: string;
    RainfallAnnotation: string;
    EvaporationAnnotation: string;
    RunoffAnnotation: string;
    UpstreamFractionAnnotation: string;
    StageAnnotation: string;
    RoughnessAnnotation: string;
    DiversionAnnotations: array of string;

    InflowPest: string;
    RainfallPest: string;
    EvaporationPest: string;
    RunoffPest: string;
    UpstreamFractionPest: string;
    StagePest: string;
    RoughnessPest: string;
    DiversionPests: array of string;

    InflowPestSeriesName: string;
    RainfallPestSeriesName: string;
    EvaporationPestSeriesName: string;
    RunoffPestSeriesName: string;
    UpstreamFractionPestSeriesName: string;
    StagePestSeriesName: string;
    RoughnessPestSeriesName: string;
    DiversionPestSeriesNames: array of string;

    InflowPestSeriesMethod: TPestParamMethod;
    RainfallPestSeriesMethod: TPestParamMethod;
    EvaporationPestSeriesMethod: TPestParamMethod;
    RunoffPestSeriesMethod: TPestParamMethod;
    UpstreamFractionPestSeriesMethod: TPestParamMethod;
    StagePestSeriesMethod: TPestParamMethod;
    RoughnessPestSeriesMethod: TPestParamMethod;
    DiversionPestSeriesMethods: array of TPestParamMethod;

    InflowTimeSeriesName: string;
    RainfallTimeSeriesName: string;
    EvaporationTimeSeriesName: string;
    RunoffTimeSeriesName: string;
    StageTimeSeriesName: string;
    RoughnessTimeSeriesName: string;
    DiversionTimeSeriesName: array of string;

    Status: TStreamStatus;
    ReachNumber: Integer;
    MvrUsed: Boolean;
    MvrIndex: Integer;

    // GWT
    GwtStatus: TGwtStreamStatusArray;
    SpecifiedConcentrations: TGwtCellData;
    RainfallConcentrations: TGwtCellData;
    EvapConcentrations: TGwtCellData;
    RunoffConcentrations: TGwtCellData;
    InflowConcentrations: TGwtCellData;

    procedure Assign(const Item: TSfrMF6Record);
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TSfrMF6Array = array of TSfrMF6Record;

  TSfrMf6Storage = class(TCustomBoundaryStorage)
  private
    FSfrMF6Array: TSfrMF6Array;
    FSpeciesCount: Integer;
    function GetStrMF6Array: TSfrMF6Array;
    procedure SetSpeciesCount(const Value: Integer);
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property SfrMF6Array: TSfrMF6Array read GetStrMF6Array;
    property SpeciesCount: Integer read FSpeciesCount write SetSpeciesCount;
  end;

  TSfrMf6Collection = class;

  TSftGwtConcCollection = class(TGwtConcStringCollection)
    constructor Create(Model: TBaseModel; AScreenObject: TObject;
      ParentCollection: TSfrMf6Collection);
  end;

  TSfrMf6Item = class(TCustomModflowBoundaryItem)
  private
    FDiversions: TStringList;
    FInflow: TFormulaObject;
    FEvaporation: TFormulaObject;
    FRunoff: TFormulaObject;
    FUpstreamFraction: TFormulaObject;
    FDiversionFormulas: TList<TFormulaObject>;
    FStatus: Boolean;
    FRainfall: TFormulaObject;
    FStage: TFormulaObject;
    FRoughness: TFormulaObject;
    FStreamStatus: TStreamStatus;
    // GWT
    FGwtStatus: TGwtStrStatusCollection;
    FSpecifiedConcentrations: TSftGwtConcCollection;
    FRainfallConcentrations: TSftGwtConcCollection;
    FEvapConcentrations: TSftGwtConcCollection;
    FRunoffConcentrations: TSftGwtConcCollection;
    FInflowConcentrations: TSftGwtConcCollection;
    function GetDiversions: TStrings;
    function GetEvaporation: string;
    function GetInflow: string;
    function GetRunoff: string;
    function GetUpstreamFraction: string;
    procedure SetDiversions(const Value: TStrings);
    procedure SetEvaporation(const Value: string);
    procedure SetInflow(const Value: string);
    procedure SetRunoff(const Value: string);
    procedure SetUpstreamFraction(const Value: string);
    function GetDiversionFormula(Index: Integer): string;
    procedure SetDiversionFormula(Index: Integer; const Value: string);
    procedure SetDiversionCount(const Value: Integer);
    function GetDiversionCount: Integer;
    procedure SetStatus(const Value: Boolean);
    function GetRainfall: string;
    procedure SetRainfall(const Value: string);
    procedure SetStreamStatus(const Value: TStreamStatus);
    function GetRoughness: string;
    function GetStage: string;
    procedure SetRoughness(const Value: string);
    procedure SetStage(const Value: string);
    procedure SetEvapConcentrations(const Value: TSftGwtConcCollection);
    procedure SetGwtStatus(const Value: TGwtStrStatusCollection);
    procedure SetInflowConcentrations(const Value: TSftGwtConcCollection);
    procedure SetRainfallConcentrations(const Value: TSftGwtConcCollection);
    procedure SetRunoffConcentrations(const Value: TSftGwtConcCollection);
    procedure SetSpecifiedConcentrations(const Value: TSftGwtConcCollection);
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    // @name checks whether AnotherItem is the same as the current @classname.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure InvalidateModel; override;
    function BoundaryFormulaCount: integer; override;
    procedure UpdateBoundaryObservers;
  public
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
    // @name defines formulas for diversion amounts.
    property DiversionFormulas[Index: Integer]: string read GetDiversionFormula
      write SetDiversionFormula;
    property DiversionCount: Integer read GetDiversionCount write SetDiversionCount;
    procedure Loaded;
  published
    property Inflow: string read GetInflow write SetInflow;
    property Rainfall: string read GetRainfall write SetRainfall;
    property Evaporation: string read GetEvaporation write SetEvaporation;
    property Runoff: string read GetRunoff write SetRunoff;
    property UpstreamFraction: string read GetUpstreamFraction
      write SetUpstreamFraction;
    property Stage: string read GetStage write SetStage;
    property Roughness: string read GetRoughness write SetRoughness;
    // for backwards compatibility
    property Status: Boolean read FStatus write SetStatus stored False;
    property StreamStatus: TStreamStatus read FStreamStatus write SetStreamStatus;
    // @name is used to store @link(DiversionFormulas) to file and to read
    // @link(DiversionFormulas) from a file but otherwise, use
    // @link(DiversionFormulas) instead of @name.
    property Diversions: TStrings read GetDiversions write SetDiversions;
    // GWT
    property GwtStatus: TGwtStrStatusCollection read FGwtStatus write SetGwtStatus
      {$IFNDEF GWT}
      stored False
      {$ENDIF}
      ;
    property SpecifiedConcentrations: TSftGwtConcCollection read FSpecifiedConcentrations
      write SetSpecifiedConcentrations
      {$IFNDEF GWT}
      stored False
      {$ENDIF}
      ;
    property RainfallConcentrations: TSftGwtConcCollection read FRainfallConcentrations
      write SetRainfallConcentrations
      {$IFNDEF GWT}
      stored False
      {$ENDIF}
      ;
    property EvapConcentrations: TSftGwtConcCollection read FEvapConcentrations
      write SetEvapConcentrations
      {$IFNDEF GWT}
      stored False
      {$ENDIF}
      ;
    property RunoffConcentrations: TSftGwtConcCollection read FRunoffConcentrations
      write SetRunoffConcentrations
      {$IFNDEF GWT}
      stored False
      {$ENDIF}
      ;
    property InflowConcentrations: TSftGwtConcCollection read FInflowConcentrations
      write SetInflowConcentrations
      {$IFNDEF GWT}
      stored False
      {$ENDIF}
      ;
  end;

  TSfrMf6TimeListLink = class(TTimeListsModelLink)
  private
    FInflow: TModflowTimeList;
    FRainfall: TModflowTimeList;
    FEvaporation: TModflowTimeList;
    FRunoff: TModflowTimeList;
    FUpstreamFraction: TModflowTimeList;
    FStage: TModflowTimeList;
    FRoughness: TModflowTimeList;
    FStreamStatus: TModflowTimeList;
    FReachNumber: TModflowTimeList;
    // GWT
    FGwtStatusList: TModflowTimeLists;
    FSpecifiedConcList: TModflowTimeLists;
    FRainfallConcList: TModflowTimeLists;
    FEvapConcList: TModflowTimeLists;
    FRunoffConcList: TModflowTimeLists;
    FInflowConcList: TModflowTimeLists;

  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  TSfrMf6Boundary = class;

  TSfrMf6Collection = class(TCustomMF_ListBoundColl)
  private
    FSfrMf6Boundary: TSfrMf6Boundary;
    procedure InvalidateInflowData(Sender: TObject);
    procedure InvalidateRainfallData(Sender: TObject);
    procedure InvalidateEvaporationData(Sender: TObject);
    procedure InvalidateRunoffData(Sender: TObject);
    procedure InvalidateUpstreamFractionData(Sender: TObject);
    procedure InvalidateDiversionsData(Sender: TObject);
    procedure InvalidateStageData(Sender: TObject);
    procedure InvalidateRoughnessData(Sender: TObject);
    // GWT
    procedure InvalidateGwtStatus(Sender: TObject);
    procedure InvalidateSpecifiedConcentrations(Sender: TObject);
    procedure InvalidateRainfallConcentrations(Sender: TObject);
    procedure InvalidateEvapConcentrations(Sender: TObject);
    procedure InvalidateRunoffConcentrations(Sender: TObject);
    procedure InvalidateInflowConcentrations(Sender: TObject);
  protected
    class function ItemClass: TBoundaryItemClass; override;
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure Loaded;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer;
      AModel: TBaseModel); override;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
      override;
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject;
      PestName: string; PestSeriesName: string;
      PestSeriesMethod: TPestParamMethod; TimeSeriesName: string); override;
    procedure AssignDirectlySpecifiedValues( AnItem: TCustomModflowBoundaryItem;
      BoundaryStorage: TCustomBoundaryStorage); override;
  public
    constructor Create(Boundary: TModflowScreenObjectProperty;
      Model: TBaseModel; ScreenObject: TObject); override;
  end;

  TSfrMf6_Cell = class(TValueCell)
  private
    FValues: TSfrMF6Record;
    StressPeriod: integer;
    function GetMvrIndex: Integer;
    function GetMvrUsed: Boolean;
    function GetEvapConcentrations: TGwtCellData;
    function GetGwtStatus: TGwtStreamStatusArray;
    function GetInflowConcentrations: TGwtCellData;
    function GetRainfallConcentrations: TGwtCellData;
    function GetRunoffConcentrations: TGwtCellData;
    function GetSpecifiedConcentrations: TGwtCellData;
  protected
    function GetColumn: integer; override;
    function GetLayer: integer; override;
    function GetRow: integer; override;
    procedure SetColumn(const Value: integer); override;
    procedure SetLayer(const Value: integer); override;
    procedure SetRow(const Value: integer); override;
    function GetIntegerValue(Index: integer; AModel: TBaseModel): integer; override;
    function GetRealValue(Index: integer; AModel: TBaseModel): double; override;
    function GetRealAnnotation(Index: integer; AModel: TBaseModel): string; override;
    function GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string; override;
    function GetSection: integer; override;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);  override;
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList); override;
    procedure RecordStrings(Strings: TStringList); override;
    function GetMf6TimeSeriesName(Index: Integer): string; override;
    procedure SetMf6TimeSeriesName(Index: Integer; const Value: string); override;
  public
    property MvrUsed: Boolean read GetMvrUsed;
    property MvrIndex: Integer read GetMvrIndex;
    property Values: TSfrMF6Record read FValues;
    function GetPestName(Index: Integer): string; override;
    function GetPestSeriesMethod(Index: Integer): TPestParamMethod; override;
    function GetPestSeriesName(Index: Integer): string; override;
    // GWT
    Property GwtStatus: TGwtStreamStatusArray read GetGwtStatus;
    Property SpecifiedConcentrations: TGwtCellData read GetSpecifiedConcentrations;
    Property RainfallConcentrations: TGwtCellData read GetRainfallConcentrations;
    Property EvapConcentrations: TGwtCellData read GetEvapConcentrations;
    Property RunoffConcentrations: TGwtCellData read GetRunoffConcentrations;
    Property InflowConcentrations: TGwtCellData read GetInflowConcentrations;

  end;

  TDivisionPriority = (cpFraction, cpExcess, cpThreshold, cpUpTo);

  TDivRecord = record
    ConnectedReach: Integer;
    Priority: TDivisionPriority;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
  end;

  TDiversionArray = array of TDivRecord;

  TSfrMF6ConstantRecord = record
  private
    FReachNumber: integer;
    function GetBoundaryAnnotation(Index: Integer): string;
    function GetBoundaryValue(Index: Integer): double;
    procedure SetBoundaryAnnotation(Index: Integer; const Value: string);
    procedure SetBoundaryValue(Index: Integer; const Value: double);
    procedure SetReachNumber(const Value: integer);
    function GetPestParamName(Index: Integer): string;
    procedure SetPestParamName(Index: Integer; const Value: string);
  public
    Cell: TCellLocation;
    ReachLength: Double;
    ReachWidth: Double;
    Gradient: Double;
    StreambedTop: Double;
    StreambedThickness: Double;
    HydraulicConductivity: Double;
    ConnectedReaches: array of integer;
    DownstreamDiversions: TDiversionArray;
    ReachLengthAnnotation: string;
    ReachWidthAnnotation: string;
    GradientAnnotation: string;
    StreambedTopAnnotation: string;
    StreambedThicknessAnnotation: string;
    HydraulicConductivityAnnotation: string;
    BoundName: string;
    ConnectedReacheAnnotations: array of string;

    PestReachLength: string;
    PestReachWidth: string;
    PestGradient: string;
    PestStreambedTop: string;
    PestStreambedThickness: string;
    PestHydraulicConductivity: string;

    property ReachNumber: integer read FReachNumber write SetReachNumber;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
    property BoundaryValue[Index: Integer]: double read GetBoundaryValue
      write SetBoundaryValue;
    property BoundaryAnnotation[Index: Integer]: string
      read GetBoundaryAnnotation write SetBoundaryAnnotation;
    property PestParamName[Index: Integer]: string read GetPestParamName
      write SetPestParamName;
    function IsConnected(Value: Integer): boolean;
  end;

  TSfrMF6ConstArray = array of TSfrMF6ConstantRecord;

  TSDiversionItem = class(TOrderedItem)
  private
    FDownstreamSegment: Integer;
    FPriority: TDivisionPriority;
//    FDiversionNumber: Integer;
    procedure SetPriority(const Value: TDivisionPriority);
    procedure SetDownstreamSegment(const Value: Integer);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
//    property DiversionNumber: Integer read FDiversionNumber write FDiversionNumber;
  published
    property DownstreamSegment: Integer read FDownstreamSegment
      write SetDownstreamSegment;
    property Priority: TDivisionPriority read FPriority write SetPriority;
  end;

  TDiversionCollection = class(TOrderedCollection)
  private
    function GetItem(Index: Integer): TSDiversionItem;
    procedure SetItem(Index: Integer; const Value: TSDiversionItem);
  public
    function IsSame(AnOrderedCollection: TOrderedCollection): boolean; override;
    constructor Create(Model: TBaseModel);
    property Items[Index: Integer]: TSDiversionItem read GetItem write SetItem; default;
    function Add: TSDiversionItem;
  end;

//  TSfrInflowLocation = (silAllCells, silFirstCell);

  TSfrMf6Boundary = class(TModflowBoundary)
  private
    FDiversions: TDiversionCollection;
    FDownstreamSegments: TIntegerCollection;
    FSegmentNumber: Integer;
    FReachLength: TFormulaObject;
    FGradient: TFormulaObject;
    FHydraulicConductivity: TFormulaObject;
    FReachWidth: TFormulaObject;
    FRoughness: string;
    FStreambedThickness: TFormulaObject;
    FStreambedTop: TFormulaObject;
    FUpstreamFraction: string;
    FReachLengthObserver: TObserver;
    FHydraulicConductivityObserver: TObserver;
    FReachWidthObserver: TObserver;
    FStreambedTopObserver: TObserver;
    FGradientObserver: TObserver;
    FStreambedThicknessObserver: TObserver;
    FPestRainfallMethod: TPestParamMethod;
    FPestRoughnessMethod: TPestParamMethod;
    FPestRunoffMethod: TPestParamMethod;
    FPestInflowMethod: TPestParamMethod;
    FPestEvaporationMethod: TPestParamMethod;
    FPestStageMethod: TPestParamMethod;
    FPestUpstreamFractionMethod: TPestParamMethod;
    FInflow: TFormulaObject;
    FRainfall: TFormulaObject;
    FEvaporation: TFormulaObject;
    FRunoff: TFormulaObject;
    FStage: TFormulaObject;
    FUpstreamFractionFormula: TFormulaObject;
    FRoughnessFormula: TFormulaObject;
    FPestEvaporationObserver: TObserver;
    FPestInflowObserver: TObserver;
    FPestRainfallObserver: TObserver;
    FPestRoughnessObserver: TObserver;
    FPestRunoffObserver: TObserver;
    FPestStageObserver: TObserver;
    FPestUpstreamFractionObserver: TObserver;
    FUsedObserver: TObserver;
    procedure SetDiversions(const Value: TDiversionCollection);
    procedure SetDownstreamSegments(const Value: TIntegerCollection);
    procedure SetSegmentNumber(const Value: Integer);
    function GetReachLength: string;
    procedure SetReachLength(const Value: string);
    function GetGradient: string;
    function GetHydraulicConductivity: string;
    function GetReachWidth: string;
    function GetStreambedThickness: string;
    function GetStreambedTop: string;
    procedure SetGradient(const Value: string);
    procedure SetHydraulicConductivity(const Value: string);
    procedure SetReachWidth(const Value: string);
    procedure SetStreambedThickness(const Value: string);
    procedure SetStreambedTop(const Value: string);
//    procedure CreateFormulaObjects;
    procedure RemoveFormulaObjects;
    function GetGradientObserver: TObserver;
    function GetHydraulicConductivityObserver: TObserver;
    function GetReachLengthObserver: TObserver;
    function GetReachWidthObserver: TObserver;
    function GetStreambedThicknessObserver: TObserver;
    function GetStreambedTopObserver: TObserver;
    procedure InvalidateDisplayTimeLists;
    procedure LinkReachLength;
    procedure LinkReachWidth;
    procedure LinkGradient;
    procedure LinkStreambedTop;
    procedure LinkStreambedThickness;
    procedure LinkHydraulicConductivity;
    procedure InvalidateInflowData(Sender: TObject);
    procedure InvalidateRainfallData(Sender: TObject);
    procedure InvalidateEvaporationData(Sender: TObject);
    procedure InvalidateRunoffData(Sender: TObject);
    procedure InvalidateUpstreamFractionData(Sender: TObject);
    procedure InvalidateStageData(Sender: TObject);
    procedure InvalidateRoughnessData(Sender: TObject);
    function GetPestEvaporationFormula: string;
    function GetPestEvaporationObserver: TObserver;
    function GetPestInflowFormula: string;
    function GetPestInflowObserver: TObserver;
    function GetPestRainfallFormula: string;
    function GetPestRainfallObserver: TObserver;
    function GetPestRoughnessFormula: string;
    function GetPestRoughnessObserver: TObserver;
    function GetPestRunoffFormula: string;
    function GetPestRunoffObserver: TObserver;
    function GetPestStageFormula: string;
    function GetPestStageObserver: TObserver;
    function GetPestUpstreamFractionFormula: string;
    function GetPestUpstreamFractionObserver: TObserver;
    procedure SetPestEvaporationFormula(const Value: string);
    procedure SetPestEvaporationMethod(const Value: TPestParamMethod);
    procedure SetPestInflowFormula(const Value: string);
    procedure SetPestInflowMethod(const Value: TPestParamMethod);
    procedure SetPestRainfallFormula(const Value: string);
    procedure SetPestRainfallMethod(const Value: TPestParamMethod);
    procedure SetPestRoughnessFormula(const Value: string);
    procedure SetPestRoughnessMethod(const Value: TPestParamMethod);
    procedure SetPestRunoffFormula(const Value: string);
    procedure SetPestRunoffMethod(const Value: TPestParamMethod);
    procedure SetPestStageFormula(const Value: string);
    procedure SetPestStageMethod(const Value: TPestParamMethod);
    procedure SetPestUpstreamFractionFormula(const Value: string);
    procedure SetPestUpstreamFractionMethod(const Value: TPestParamMethod);
  protected
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    property ReachLengthObserver: TObserver read GetReachLengthObserver;
    property ReachWidthObserver: TObserver read GetReachWidthObserver;
    property GradientObserver: TObserver read GetGradientObserver;
    property StreambedTopObserver: TObserver read GetStreambedTopObserver;
    property StreambedThicknessObserver: TObserver read GetStreambedThicknessObserver;
    property HydraulicConductivityObserver: TObserver read GetHydraulicConductivityObserver;
    function BoundaryObserverPrefix: string; override;

    procedure HandleChangedValue(Observer: TObserver); //override;
    function GetUsedObserver: TObserver; //override;
//    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure CreateFormulaObjects; //override;
//    function BoundaryObserverPrefix: string; override;
    procedure CreateObservers; //override;
    function GetPestBoundaryFormula(FormulaIndex: integer): string; override;
    procedure SetPestBoundaryFormula(FormulaIndex: integer;
      const Value: string); override;
    function GetPestBoundaryMethod(FormulaIndex: integer): TPestParamMethod; override;
    procedure SetPestBoundaryMethod(FormulaIndex: integer;
      const Value: TPestParamMethod); override;
    property PestInflowObserver: TObserver read GetPestInflowObserver;
    property PestRainfallObserver: TObserver read GetPestRainfallObserver;
    property PestEvaporationObserver: TObserver read GetPestEvaporationObserver;
    property PestRunoffObserver: TObserver read GetPestRunoffObserver;
    property PestUpstreamFractionObserver: TObserver read GetPestUpstreamFractionObserver;
    property PestStageObserver: TObserver read GetPestStageObserver;
    property PestRoughnessObserver: TObserver read GetPestRoughnessObserver;
  public
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    Destructor Destroy; override;
    Procedure Assign(Source: TPersistent); override;
    procedure Loaded;
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    procedure InvalidateDisplay; override;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
  published
    property SegmentNumber: Integer read FSegmentNumber write SetSegmentNumber;
    property DownstreamSegments: TIntegerCollection read FDownstreamSegments
      write SetDownstreamSegments;
    property Diversions: TDiversionCollection read FDiversions write SetDiversions;
    property ReachLength: string read GetReachLength Write SetReachLength;
    property ReachWidth: string read GetReachWidth write SetReachWidth;
    property Gradient: string read GetGradient write SetGradient;
    property StreambedTop: string read GetStreambedTop write SetStreambedTop;
    property StreambedThickness: string read GetStreambedThickness
      write SetStreambedThickness;
    property HydraulicConductivity: string read GetHydraulicConductivity
      write SetHydraulicConductivity;
    // @name is only for backwards compatibility. It is not used.
    property Roughness: string read FRoughness write FRoughness stored False;
    // @name is only for backwards compatibility. It is not used.
    property UpstreamFraction: string read FUpstreamFraction
      write FUpstreamFraction stored False;

    property PestInflowFormula: string read GetPestInflowFormula
      write SetPestInflowFormula
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestInflowMethod: TPestParamMethod read FPestInflowMethod
      write SetPestInflowMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestRainfallFormula: string read GetPestRainfallFormula
      write SetPestRainfallFormula
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestRainfallMethod: TPestParamMethod read FPestRainfallMethod
      write SetPestRainfallMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestEvaporationFormula: string read GetPestEvaporationFormula
      write SetPestEvaporationFormula
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestEvaporationMethod: TPestParamMethod read FPestEvaporationMethod
      write SetPestEvaporationMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestRunoffFormula: string read GetPestRunoffFormula
      write SetPestRunoffFormula
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestRunoffMethod: TPestParamMethod read FPestRunoffMethod
      write SetPestRunoffMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestUpstreamFractionFormula: string read GetPestUpstreamFractionFormula
      write SetPestUpstreamFractionFormula
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestUpstreamFractionMethod: TPestParamMethod read FPestUpstreamFractionMethod
      write SetPestUpstreamFractionMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestStageFormula: string read GetPestStageFormula
      write SetPestStageFormula
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestStageMethod: TPestParamMethod read FPestStageMethod
      write SetPestStageMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestRoughnessFormula: string read GetPestRoughnessFormula
      write SetPestRoughnessFormula
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestRoughnessMethod: TPestParamMethod read FPestRoughnessMethod
      write SetPestRoughnessMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
  end;

const
  SfrMf6InflowPosition = 0;
  SfrMf6RainfallPosition = 1;
  SfrMf6EvaporationPosition = 2;
  SfrMf6RunoffPosition = 3;
  SfrMf6UpstreamFractionPosition = 4;
  SfrMf6StagePosition = 5;
  SfrMf6RoughnessPosition = 6;
  SfrMf6DiversionStartPosition = 7;

  SfrMf6ReachLengthPosition = 0;
  SfrMf6ReachWidthPosition = 1;
  SfrMf6GradientPosition = 2;
  SfrMf6StreambedTopPosition = 3;
  SfrMf6StreambedThicknessPosition = 4;
  SfrMf6HydraulicConductivityPosition = 5;
  SfrMf6PestBoundaryOffset = 6;

function TryGetSfrOb(const SfrObName: string; var SfrOb: TSfrOb): Boolean;
function SfrObToString(const SfrOb: TSfrOb): string;
Procedure FillSfrSeriesNames(AList: TStrings);

implementation

uses
  frmGoPhastUnit, ModflowTimeUnit, PhastModelUnit,
  ScreenObjectUnit, GIS_Functions, ModflowSfrUnit, ModflowSfrReachUnit,
  ModflowSfrSegment, ModflowSfrChannelUnit, ModflowSfrParamIcalcUnit,
  ModflowSfrFlows, ModflowStrUnit, DataSetUnit, ModflowMvrUnit;

const
  SfrObName: array[TSfrOb] of string =
    ('Stage', 'ExtInflow', 'Inflow', 'FromMvr', 'Rainfall', 'Runoff', 'Sfr',
    'Evaporation', 'Outflow', 'ExternalOutflow', 'ToMvr', 'UpstreamFlow',
    'DownstreamFlow');

var
  SfrObNames: TStringList;

procedure InitializeSfrObNames;
var
  SfrOb: TSfrOb;
begin
  SfrObNames:= TStringList.Create;
  SfrObNames.CaseSensitive := False;
  for SfrOb := Low(TSfrOb) to High(TSfrOb) do
  begin
    SfrObNames.Add(SfrObName[SfrOb]);
  end;
end;

function TryGetSfrOb(const SfrObName: string; var SfrOb: TSfrOb): Boolean;
var
  Index: Integer;
begin
  Index := SfrObNames.IndexOf(SfrObName);
  result := Index >= 0;
  if result then
  begin
    SfrOb := TSfrOb(Index);
  end;
end;

Procedure FillSfrSeriesNames(AList: TStrings);
begin
  AList.Assign(SfrObNames);
end;

function SfrObToString(const SfrOb: TSfrOb): string;
begin
  result := SfrObName[SfrOb]
end;

resourcestring
  StrAllButTheFirstRe = 'All but the first reach in a segment is assigned a ' +
  'value of 1. Assigned by %s.';
  StrUpstreamFractionIs = 'Upstream fraction is automatically set to zero fo' +
  'r the first reach in an inactive segment.';
  StrInflowIsOnlySetT = 'Inflow is only set to a non-zero value in the first' +
  ' reach in the segment defined by %s';

{ TStrMF6Record }

procedure TSfrMF6Record.Assign(const Item: TSfrMF6Record);
begin
  self := Item;
  SpecifiedConcentrations.Assign(Item.SpecifiedConcentrations);
  RainfallConcentrations.Assign(Item.RainfallConcentrations);
  EvapConcentrations.Assign(Item.EvapConcentrations);
  RunoffConcentrations.Assign(Item.RunoffConcentrations);
  InflowConcentrations.Assign(Item.InflowConcentrations);
  SetLength(GwtStatus, Length(GwtStatus));
end;

procedure TSfrMF6Record.Cache(Comp: TCompressionStream; Strings: TStringList);
var
  index: Integer;
  GwtStatusCount: Integer;
  SpeciesIndex: Integer;
begin
  WriteCompCell(Comp, Cell);

  WriteCompReal(Comp, Inflow);
  WriteCompReal(Comp, Rainfall);
  WriteCompReal(Comp, Evaporation);
  WriteCompReal(Comp, Runoff);
  WriteCompReal(Comp, UpstreamFraction);
  WriteCompReal(Comp, Stage);
  WriteCompReal(Comp, Roughness);
  WriteCompInt(Comp, Ord(Status));

  WriteCompInt(Comp, Length(Diversions));
  for index := 0 to Length(Diversions) - 1 do
  begin
    WriteCompReal(Comp, Diversions[index]);
  end;

  WriteCompInt(Comp, Strings.IndexOf(InflowAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(RainfallAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(EvaporationAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(RunoffAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(UpstreamFractionAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(StageAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(RoughnessAnnotation));

  for index := 0 to Length(DiversionAnnotations) - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(DiversionAnnotations[index]));
  end;

  WriteCompInt(Comp, Strings.IndexOf(InflowPest));
  WriteCompInt(Comp, Strings.IndexOf(RainfallPest));
  WriteCompInt(Comp, Strings.IndexOf(EvaporationPest));
  WriteCompInt(Comp, Strings.IndexOf(RunoffPest));
  WriteCompInt(Comp, Strings.IndexOf(UpstreamFractionPest));
  WriteCompInt(Comp, Strings.IndexOf(StagePest));
  WriteCompInt(Comp, Strings.IndexOf(RoughnessPest));

  for index := 0 to Length(DiversionPests) - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(DiversionPests[index]));
  end;

  WriteCompInt(Comp, Strings.IndexOf(InflowPestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(RainfallPestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(EvaporationPestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(RunoffPestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(UpstreamFractionPestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(StagePestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(RoughnessPestSeriesName));

  for index := 0 to Length(DiversionPestSeriesNames) - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(DiversionPestSeriesNames[index]));
  end;

  WriteCompInt(Comp, Ord(InflowPestSeriesMethod));
  WriteCompInt(Comp, Ord(RainfallPestSeriesMethod));
  WriteCompInt(Comp, Ord(EvaporationPestSeriesMethod));
  WriteCompInt(Comp, Ord(RunoffPestSeriesMethod));
  WriteCompInt(Comp, Ord(UpstreamFractionPestSeriesMethod));
  WriteCompInt(Comp, Ord(StagePestSeriesMethod));
  WriteCompInt(Comp, Ord(RoughnessPestSeriesMethod));

  for index := 0 to Length(DiversionPestSeriesMethods) - 1 do
  begin
    WriteCompInt(Comp, Ord(DiversionPestSeriesMethods[index]));
  end;

  WriteCompInt(Comp, Strings.IndexOf(InflowTimeSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(RainfallTimeSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(EvaporationTimeSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(RunoffTimeSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(StageTimeSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(RoughnessTimeSeriesName));

  for index := 0 to Length(DiversionTimeSeriesName) - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(DiversionTimeSeriesName[index]));
  end;

  WriteCompInt(Comp, ReachNumber);

  WriteCompBoolean(Comp, MvrUsed);
  WriteCompInt(Comp, MvrIndex);
  SetLength(Diversions, 0);
  SetLength(DiversionAnnotations, 0);

  SpecifiedConcentrations.Cache(Comp, Strings);
  RainfallConcentrations.Cache(Comp, Strings);
  EvapConcentrations.Cache(Comp, Strings);
  RunoffConcentrations.Cache(Comp, Strings);
  InflowConcentrations.Cache(Comp, Strings);

  GwtStatusCount := Length(GwtStatus);
  WriteCompInt(Comp, GwtStatusCount);
  for SpeciesIndex := 0 to GwtStatusCount - 1 do
  begin
    WriteCompInt(Comp, Ord(GwtStatus[SpeciesIndex]));
  end;
end;

procedure TSfrMF6Record.RecordStrings(Strings: TStringList);
var
  index: Integer;
begin
  Strings.Add(InflowAnnotation);
  Strings.Add(RainfallAnnotation);
  Strings.Add(EvaporationAnnotation);
  Strings.Add(RunoffAnnotation);
  Strings.Add(UpstreamFractionAnnotation);
  Strings.Add(StageAnnotation);
  Strings.Add(RoughnessAnnotation);
  for index := 0 to Length(DiversionAnnotations) - 1 do
  begin
    Strings.Add(DiversionAnnotations[index]);
  end;

  Strings.Add(InflowPest);
  Strings.Add(RainfallPest);
  Strings.Add(EvaporationPest);
  Strings.Add(RunoffPest);
  Strings.Add(UpstreamFractionPest);
  Strings.Add(StagePest);
  Strings.Add(RoughnessPest);
  for index := 0 to Length(DiversionPests) - 1 do
  begin
    Strings.Add(DiversionPests[index]);
  end;

  Strings.Add(InflowPestSeriesName);
  Strings.Add(RainfallPestSeriesName);
  Strings.Add(EvaporationPestSeriesName);
  Strings.Add(RunoffPestSeriesName);
  Strings.Add(UpstreamFractionPestSeriesName);
  Strings.Add(StagePestSeriesName);
  Strings.Add(RoughnessPestSeriesName);
  for index := 0 to Length(DiversionPestSeriesNames) - 1 do
  begin
    Strings.Add(DiversionPestSeriesNames[index]);
  end;

  Strings.Add(InflowTimeSeriesName);
  Strings.Add(RainfallTimeSeriesName);
  Strings.Add(EvaporationTimeSeriesName);
  Strings.Add(RunoffTimeSeriesName);
  Strings.Add(StageTimeSeriesName);
  Strings.Add(RoughnessTimeSeriesName);
  for index := 0 to Length(DiversionTimeSeriesName) - 1 do
  begin
    Strings.Add(DiversionTimeSeriesName[index]);
  end;

  SpecifiedConcentrations.RecordStrings(Strings);
  RainfallConcentrations.RecordStrings(Strings);
  EvapConcentrations.RecordStrings(Strings);
  RunoffConcentrations.RecordStrings(Strings);
  InflowConcentrations.RecordStrings(Strings);
end;

procedure TSfrMF6Record.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
var
  ArraySize: Integer;
  index: Integer;
  GwtStatusCount: Integer;
  SpeciesIndex: Integer;
begin
  Cell := ReadCompCell(Decomp);

  Inflow := ReadCompReal(Decomp);
  Rainfall := ReadCompReal(Decomp);
  Evaporation := ReadCompReal(Decomp);
  Runoff := ReadCompReal(Decomp);
  UpstreamFraction := ReadCompReal(Decomp);
  Stage := ReadCompReal(Decomp);
  Roughness := ReadCompReal(Decomp);
  Status := TStreamStatus(ReadCompInt(Decomp));

  ArraySize := ReadCompInt(Decomp);
  SetLength(Diversions, ArraySize);
  for index := 0 to ArraySize - 1 do
  begin
    Diversions[index] := ReadCompReal(Decomp);
  end;

  InflowAnnotation := Annotations[ReadCompInt(Decomp)];
  RainfallAnnotation := Annotations[ReadCompInt(Decomp)];
  EvaporationAnnotation := Annotations[ReadCompInt(Decomp)];
  RunoffAnnotation := Annotations[ReadCompInt(Decomp)];
  UpstreamFractionAnnotation := Annotations[ReadCompInt(Decomp)];
  StageAnnotation := Annotations[ReadCompInt(Decomp)];
  RoughnessAnnotation := Annotations[ReadCompInt(Decomp)];

  SetLength(DiversionAnnotations, ArraySize);
  for index := 0 to ArraySize - 1 do
  begin
    DiversionAnnotations[index] := Annotations[ReadCompInt(Decomp)];
  end;

  InflowPest := Annotations[ReadCompInt(Decomp)];
  RainfallPest := Annotations[ReadCompInt(Decomp)];
  EvaporationPest := Annotations[ReadCompInt(Decomp)];
  RunoffPest := Annotations[ReadCompInt(Decomp)];
  UpstreamFractionPest := Annotations[ReadCompInt(Decomp)];
  StagePest := Annotations[ReadCompInt(Decomp)];
  RoughnessPest := Annotations[ReadCompInt(Decomp)];

  SetLength(DiversionPests, ArraySize);
  for index := 0 to ArraySize - 1 do
  begin
    DiversionPests[index] := Annotations[ReadCompInt(Decomp)];
  end;

  InflowPestSeriesName := Annotations[ReadCompInt(Decomp)];
  RainfallPestSeriesName := Annotations[ReadCompInt(Decomp)];
  EvaporationPestSeriesName := Annotations[ReadCompInt(Decomp)];
  RunoffPestSeriesName := Annotations[ReadCompInt(Decomp)];
  UpstreamFractionPestSeriesName := Annotations[ReadCompInt(Decomp)];
  StagePestSeriesName := Annotations[ReadCompInt(Decomp)];
  RoughnessPestSeriesName := Annotations[ReadCompInt(Decomp)];

  SetLength(DiversionPestSeriesNames, ArraySize);
  for index := 0 to ArraySize - 1 do
  begin
    DiversionPestSeriesNames[index] := Annotations[ReadCompInt(Decomp)];
  end;

  InflowPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  RainfallPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  EvaporationPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  RunoffPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  UpstreamFractionPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  StagePestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  RoughnessPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));

  SetLength(DiversionPestSeriesMethods, ArraySize);
  for index := 0 to ArraySize - 1 do
  begin
    DiversionPestSeriesMethods[index] := TPestParamMethod(ReadCompInt(Decomp));
  end;

  InflowTimeSeriesName := Annotations[ReadCompInt(Decomp)];
  RainfallTimeSeriesName := Annotations[ReadCompInt(Decomp)];
  EvaporationTimeSeriesName := Annotations[ReadCompInt(Decomp)];
  RunoffTimeSeriesName := Annotations[ReadCompInt(Decomp)];
  StageTimeSeriesName := Annotations[ReadCompInt(Decomp)];
  RoughnessTimeSeriesName := Annotations[ReadCompInt(Decomp)];

  SetLength(DiversionTimeSeriesName, ArraySize);
  for index := 0 to ArraySize - 1 do
  begin
    DiversionTimeSeriesName[index] := Annotations[ReadCompInt(Decomp)];
  end;

  ReachNumber := ReadCompInt(Decomp);
  MvrUsed := ReadCompBoolean(Decomp);
  MvrIndex := ReadCompInt(Decomp);

  SpecifiedConcentrations.Restore(Decomp, Annotations);
  RainfallConcentrations.Restore(Decomp, Annotations);
  EvapConcentrations.Restore(Decomp, Annotations);
  RunoffConcentrations.Restore(Decomp, Annotations);
  InflowConcentrations.Restore(Decomp, Annotations);

  GwtStatusCount := ReadCompInt(Decomp);
  SetLength(GwtStatus, GwtStatusCount);
  for SpeciesIndex := 0 to GwtStatusCount - 1 do
  begin
    GwtStatus[SpeciesIndex] := TGwtStreamStatus(ReadCompInt(Decomp));
  end;

end;

{ TStrMf6Storage }

procedure TSfrMf6Storage.Clear;
begin
  SetLength(FSfrMF6Array, 0);
  FCleared := True;
end;

function TSfrMf6Storage.GetStrMF6Array: TSfrMF6Array;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FSfrMF6Array;
end;

procedure TSfrMf6Storage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FSfrMF6Array, Count);
  for Index := 0 to Count - 1 do
  begin
    FSfrMF6Array[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TSfrMf6Storage.SetSpeciesCount(const Value: Integer);
var
  Index: Integer;
begin
  FSpeciesCount := Value;
  for Index := 0 to Length(FSfrMF6Array)-1 do
  begin
    SetLength(FSfrMF6Array[Index].GwtStatus, FSpeciesCount);
    FSfrMF6Array[Index].SpecifiedConcentrations.SpeciesCount := FSpeciesCount;
    FSfrMF6Array[Index].RainfallConcentrations.SpeciesCount := FSpeciesCount;
    FSfrMF6Array[Index].EvapConcentrations.SpeciesCount := FSpeciesCount;
    FSfrMF6Array[Index].RunoffConcentrations.SpeciesCount := FSpeciesCount;
    FSfrMF6Array[Index].InflowConcentrations.SpeciesCount := FSpeciesCount;
  end;
end;

procedure TSfrMf6Storage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FSfrMF6Array);
    for Index := 0 to Count - 1 do
    begin
      FSfrMF6Array[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FSfrMF6Array[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

{ TStrMf6Item }

procedure TSfrMf6Item.Assign(Source: TPersistent);
var
  StrSource: TSfrMf6Item;
  DivIndex: Integer;
  ParamItem: TSfrParamIcalcItem;
  StrItem: TStrItem;
  LocalModel: TCustomModel;
begin
  if Source is TSfrMf6Item then
  begin
    StrSource := TSfrMf6Item(Source);
    Inflow := StrSource.Inflow;
    Rainfall := StrSource.Rainfall;
    Evaporation := StrSource.Evaporation;
    Runoff := StrSource.Runoff;
    UpstreamFraction := StrSource.UpstreamFraction;
    Stage := StrSource.Stage;
    Roughness := StrSource.Roughness;
    StreamStatus := StrSource.StreamStatus;
    DiversionCount := StrSource.Diversions.Count;
    for DivIndex := 0 to DiversionCount - 1 do
    begin
      DiversionFormulas[DivIndex] := StrSource.Diversions[DivIndex];
    end;
    GwtStatus := StrSource.GwtStatus;
    SpecifiedConcentrations := StrSource.SpecifiedConcentrations;
    RainfallConcentrations := StrSource.RainfallConcentrations;
    EvapConcentrations := StrSource.EvapConcentrations;
    RunoffConcentrations := StrSource.RunoffConcentrations;
    InflowConcentrations := StrSource.InflowConcentrations;
  end
  else if Source is TSfrParamIcalcItem then
  begin
    ParamItem := TSfrParamIcalcItem(Source);
    if ParamItem.ICalc = 0 then
    begin
      StreamStatus := ssSimple
    end
    else
    begin
      StreamStatus := ssActive
    end;
  end
  else if Source is TStrItem then
  begin
    StrItem := TStrItem(Source);
    LocalModel := Model as TCustomModel;
    if LocalModel.ModflowPackages.StrPackage.CalculateStage then
    begin
      StreamStatus := ssActive
    end
    else
    begin
      StreamStatus := ssSimple
    end;

    Inflow := StrItem.Flow;
    Rainfall := '0';
    Evaporation := '0';
    Runoff := '0';
    UpstreamFraction := '1';
    Stage := StrItem.Stage;
    Roughness := StrItem.Roughness;
    if StrItem.DiversionSegment > 0 then
    begin
      Inflow := '0';
      Diversions.Add(StrItem.Flow)
    end
    else
    begin
      Inflow := StrItem.Flow;
    end;

  end;
  inherited;

end;

procedure TSfrMf6Item.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TSfrMf6Collection;
  Observer: TObserver;
  DivIndex: Integer;
  ConcIndex: Integer;
begin
  ParentCollection := Collection as TSfrMf6Collection;
  Observer := FObserverList[SfrMf6InflowPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateInflowData;

  Observer := FObserverList[SfrMf6RainfallPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateRainfallData;

  Observer := FObserverList[SfrMf6EvaporationPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateEvaporationData;

  Observer := FObserverList[SfrMf6RunoffPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateRunoffData;

  Observer := FObserverList[SfrMf6UpstreamFractionPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateUpstreamFractionData;

  Observer := FObserverList[SfrMf6StagePosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateStageData;

  Observer := FObserverList[SfrMf6RoughnessPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateRoughnessData;

  for DivIndex := 0 to DiversionCount - 1 do
  begin
    Observer := FObserverList[DivIndex + SfrMf6DiversionStartPosition];
    Observer.OnUpToDateSet := ParentCollection.InvalidateDiversionsData;
  end;

//  for ConcIndex := 0 to GwtStatus.Count - 1 do
//  begin
//    GwtStatus[ConcIndex].Observer.OnUpToDateSet
//      := ParentCollection.InvalidateSpecifiedConcentrations;
//  end;

  for ConcIndex := 0 to SpecifiedConcentrations.Count - 1 do
  begin
    SpecifiedConcentrations[ConcIndex].Observer.OnUpToDateSet
      := ParentCollection.InvalidateSpecifiedConcentrations;
  end;

  for ConcIndex := 0 to RainfallConcentrations.Count - 1 do
  begin
    RainfallConcentrations[ConcIndex].Observer.OnUpToDateSet
      := ParentCollection.InvalidateRainfallConcentrations;
  end;

  for ConcIndex := 0 to EvapConcentrations.Count - 1 do
  begin
    EvapConcentrations[ConcIndex].Observer.OnUpToDateSet
      := ParentCollection.InvalidateEvapConcentrations;
  end;

  for ConcIndex := 0 to RunoffConcentrations.Count - 1 do
  begin
    RunoffConcentrations[ConcIndex].Observer.OnUpToDateSet
      := ParentCollection.InvalidateRunoffConcentrations;
  end;

  for ConcIndex := 0 to InflowConcentrations.Count - 1 do
  begin
    InflowConcentrations[ConcIndex].Observer.OnUpToDateSet
      := ParentCollection.InvalidateInflowConcentrations;
  end;
end;

function TSfrMf6Item.BoundaryFormulaCount: integer;
begin
  result := DiversionCount + 7;
  if frmGoPhast.PhastModel.GwtUsed then
  begin
    result := result + frmGoPhast.PhastModel.MobileComponents.Count *5;
  end;
end;

constructor TSfrMf6Item.Create(Collection: TCollection);
var
  SftCollection: TSfrMf6Collection;
begin
  FDiversions := TStringList.Create;
  FDiversionFormulas := TList<TFormulaObject>.Create;

  SftCollection := Collection as TSfrMf6Collection;
  FGwtStatus := TGwtStrStatusCollection.Create;
  FSpecifiedConcentrations := TSftGwtConcCollection.Create(Model, ScreenObject,
    SftCollection);
  FRainfallConcentrations := TSftGwtConcCollection.Create(Model, ScreenObject,
    SftCollection);
  FEvapConcentrations := TSftGwtConcCollection.Create(Model, ScreenObject,
    SftCollection);
  FRunoffConcentrations := TSftGwtConcCollection.Create(Model, ScreenObject,
    SftCollection);
  FInflowConcentrations := TSftGwtConcCollection.Create(Model, ScreenObject,
    SftCollection);

  inherited;
  FStatus := True;

  Inflow := '0';
  Rainfall := '0';
  Runoff := '0';
  UpstreamFraction := '1';
  Stage := '0';
  Roughness := '0.03';
//  Status := True;
  StreamStatus := ssActive;

end;

procedure TSfrMf6Item.CreateFormulaObjects;
begin
  inherited;
  FInflow := CreateFormulaObject(dso3D);
  FRainfall := CreateFormulaObject(dso3D);
  FEvaporation := CreateFormulaObject(dso3D);
  FRunoff := CreateFormulaObject(dso3D);
  FUpstreamFraction := CreateFormulaObject(dso3D);
  FStage := CreateFormulaObject(dso3D);
  FRoughness := CreateFormulaObject(dso3D);
end;

destructor TSfrMf6Item.Destroy;
var
  Index: Integer;
begin
  FGwtStatus.Free;
  for Index := 0 to FSpecifiedConcentrations.Count - 1 do
  begin
    FSpecifiedConcentrations[Index].Value := '0';
  end;
  FSpecifiedConcentrations.Free;

  for Index := 0 to FRainfallConcentrations.Count - 1 do
  begin
    FRainfallConcentrations[Index].Value := '0';
  end;
  FRainfallConcentrations.Free;

  for Index := 0 to FEvapConcentrations.Count - 1 do
  begin
    FEvapConcentrations[Index].Value := '0';
  end;
  FEvapConcentrations.Free;

  for Index := 0 to FRunoffConcentrations.Count - 1 do
  begin
    FRunoffConcentrations[Index].Value := '0';
  end;
  FRunoffConcentrations.Free;

  for Index := 0 to FInflowConcentrations.Count - 1 do
  begin
    FInflowConcentrations[Index].Value := '0';
  end;
  FInflowConcentrations.Free;

  DiversionCount := 0;
  inherited;
  // FDiversionFormulas is accessed in RemoveFormulaObjects which is called
  // in inherited Destroy.
  FDiversionFormulas.Free;
  FDiversions.Free;
end;

function TSfrMf6Item.GetBoundaryFormula(Index: integer): string;
var
  ChemSpeciesCount: Integer;
begin
  case Index of
    SfrMf6InflowPosition: result := Inflow;
    SfrMf6RainfallPosition: result := Rainfall;
    SfrMf6EvaporationPosition: result := Evaporation;
    SfrMf6RunoffPosition: result := Runoff;
    SfrMf6UpstreamFractionPosition: result := UpstreamFraction;
    SfrMf6StagePosition: result := Stage;
    SfrMf6RoughnessPosition: result := Roughness;
    else 
      begin
        Index := Index-SfrMf6DiversionStartPosition; 
        if Index < FDiversionFormulas.Count then
        begin
          result := DiversionFormulas[Index];
          Exit;
        end;
        Index := Index - FDiversionFormulas.Count;
        // GWT
        if frmGoPhast.PhastModel.GwtUsed then
        begin
          ChemSpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;
          while SpecifiedConcentrations.Count < ChemSpeciesCount do
          begin
            SpecifiedConcentrations.Add;
          end;
          if Index < ChemSpeciesCount then
          begin
            result := SpecifiedConcentrations[Index].Value;
            Exit;
          end;
          Index := Index - ChemSpeciesCount;

          while RainfallConcentrations.Count < ChemSpeciesCount do
          begin
            RainfallConcentrations.Add;
          end;
          if Index < ChemSpeciesCount then
          begin
            result := RainfallConcentrations[Index].Value;
            Exit;
          end;
          Index := Index - ChemSpeciesCount;

          while EvapConcentrations.Count < ChemSpeciesCount do
          begin
            EvapConcentrations.Add;
          end;
          if Index < ChemSpeciesCount then
          begin
            result := EvapConcentrations[Index].Value;
            Exit;
          end;
          Index := Index - ChemSpeciesCount;

          while RunoffConcentrations.Count < ChemSpeciesCount do
          begin
            RunoffConcentrations.Add;
          end;
          if Index < ChemSpeciesCount then
          begin
            result := RunoffConcentrations[Index].Value;
            Exit;
          end;
          Index := Index - ChemSpeciesCount;

          while InflowConcentrations.Count < ChemSpeciesCount do
          begin
            InflowConcentrations.Add;
          end;
          if Index < ChemSpeciesCount then
          begin
            result := InflowConcentrations[Index].Value;
            Exit;
          end;
          Assert(False);
        end;
//        Index := Index - ChemSpeciesCount;
      end;
  end;
end;

function TSfrMf6Item.GetDiversionCount: Integer;
begin
  result := FDiversionFormulas.Count;
end;

function TSfrMf6Item.GetDiversionFormula(Index: Integer): string;
begin
  Result := FDiversionFormulas[Index].Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(SfrMf6DiversionStartPosition+Index);
  end;
end;

function TSfrMf6Item.GetDiversions: TStrings;
var
  DivIndex: Integer;
begin
  UpdateBoundaryObservers;

  FDiversions.Clear;
  FDiversions.Capacity := DiversionCount;
  for DivIndex := 0 to DiversionCount - 1 do
  begin
    FDiversions.Add(DiversionFormulas[DivIndex])
  end;
  result := FDiversions;
end;

function TSfrMf6Item.GetEvaporation: string;
begin
  Result := FEvaporation.Formula;
  ResetItemObserver(SfrMf6EvaporationPosition);
end;

function TSfrMf6Item.GetInflow: string;
begin
  Result := FInflow.Formula;
  ResetItemObserver(SfrMf6InflowPosition);
end;

procedure TSfrMf6Item.GetPropertyObserver(Sender: TObject; List: TList);
var
  ConcIndex: Integer;
  Item: TGwtConcStringValueItem;
begin
  if Sender = FInflow then
  begin
    List.Add(FObserverList[SfrMf6InflowPosition]);
  end;
  if Sender = FRainfall then
  begin
    List.Add(FObserverList[SfrMf6RainfallPosition]);
  end;
  if Sender = FEvaporation then
  begin
    List.Add(FObserverList[SfrMf6EvaporationPosition]);
  end;
  if Sender = FRunoff then
  begin
    List.Add(FObserverList[SfrMf6RunoffPosition]);
  end;
  if Sender = FUpstreamFraction then
  begin
    List.Add(FObserverList[SfrMf6UpstreamFractionPosition]);
  end;
  if Sender = FStage then
  begin
    List.Add(FObserverList[SfrMf6StagePosition]);
  end;
  if Sender = FRoughness then
  begin
    List.Add(FObserverList[SfrMf6RoughnessPosition]);
  end;
  if FDiversionFormulas.IndexOf(Sender as TFormulaObject) >= 0 then
  begin
    List.Add(FObserverList[SfrMf6DiversionStartPosition]);
  end;
  // GWT
  for ConcIndex := 0 to SpecifiedConcentrations.Count - 1 do
  begin
    Item := SpecifiedConcentrations.Items[ConcIndex];
    if Item.ValueObject = Sender then
    begin
      List.Add(Item.Observer);
    end;
  end;

  for ConcIndex := 0 to RainfallConcentrations.Count - 1 do
  begin
    Item := RainfallConcentrations.Items[ConcIndex];
    if Item.ValueObject = Sender then
    begin
      List.Add(Item.Observer);
    end;
  end;

  for ConcIndex := 0 to EvapConcentrations.Count - 1 do
  begin
    Item := EvapConcentrations.Items[ConcIndex];
    if Item.ValueObject = Sender then
    begin
      List.Add(Item.Observer);
    end;
  end;

  for ConcIndex := 0 to RunoffConcentrations.Count - 1 do
  begin
    Item := RunoffConcentrations.Items[ConcIndex];
    if Item.ValueObject = Sender then
    begin
      List.Add(Item.Observer);
    end;
  end;

  for ConcIndex := 0 to InflowConcentrations.Count - 1 do
  begin
    Item := InflowConcentrations.Items[ConcIndex];
    if Item.ValueObject = Sender then
    begin
      List.Add(Item.Observer);
    end;
  end;

//  SpecifiedConcentrations.Assign(Item.SpecifiedConcentrations);
//  RainfallConcentrations.Assign(Item.RainfallConcentrations);
//  EvapConcentrations.Assign(Item.EvapConcentrations);
//  RunoffConcentrations.Assign(Item.RunoffConcentrations);
//  InflowConcentrations.Assign(Item.InflowConcentrations);

end;

function TSfrMf6Item.GetRainfall: string;
begin
  Result := FRainfall.Formula;
  ResetItemObserver(SfrMf6RainfallPosition);
end;

function TSfrMf6Item.GetRoughness: string;
begin
  Result := FRoughness.Formula;
  ResetItemObserver(SfrMf6RoughnessPosition);
end;

function TSfrMf6Item.GetRunoff: string;
begin
  Result := FRunoff.Formula;
  ResetItemObserver(SfrMf6RunoffPosition);
end;

function TSfrMf6Item.GetStage: string;
begin
  Result := FStage.Formula;
  ResetItemObserver(SfrMf6StagePosition);
end;

function TSfrMf6Item.GetUpstreamFraction: string;
begin
  Result := FUpstreamFraction.Formula;
  ResetItemObserver(SfrMf6UpstreamFractionPosition);
end;

procedure TSfrMf6Item.InvalidateModel;
begin
  inherited;
  { TODO -cMODFLOW-6 : Invalidate displays here. }
end;

function TSfrMf6Item.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TSfrMf6Item;
  Index: Integer;
begin
  result := (AnotherItem is TSfrMf6Item) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TSfrMf6Item(AnotherItem);
    result := (Item.Inflow = Inflow)
      and (Item.Rainfall = Rainfall)
      and (Item.Evaporation = Evaporation)
      and (Item.Runoff = Runoff)
      and (Item.UpstreamFraction = UpstreamFraction)
      and (Item.Stage = Stage)
      and (Item.Roughness = Roughness)
      and (Item.StreamStatus = StreamStatus)
      and (Item.DiversionCount = DiversionCount)
      and (Item.SpecifiedConcentrations.Count = SpecifiedConcentrations.Count)
      and (Item.RainfallConcentrations.Count = RainfallConcentrations.Count)
      and (Item.EvapConcentrations.Count = EvapConcentrations.Count)
      and (Item.RunoffConcentrations.Count = RunoffConcentrations.Count)
      and (Item.InflowConcentrations.Count = InflowConcentrations.Count)
      and (Item.GwtStatus.Count = GwtStatus.Count);
      
    if result then
    begin
      for Index := 0 to DiversionCount - 1 do
      begin
        result := Item.Diversions[Index] = Diversions[Index];
        if not Result then
        begin
          Exit;
        end;
      end;
      for Index := 0 to GwtStatus.Count - 1 do
      begin
        result := Item.GwtStatus[Index].GwtStreamStatus = GwtStatus[Index].GwtStreamStatus;
        if not Result then
        begin
          Exit;
        end;
      end;
      for Index := 0 to SpecifiedConcentrations.Count - 1 do
      begin
        result := Item.SpecifiedConcentrations[Index].Value = SpecifiedConcentrations[Index].Value;
        if not Result then
        begin
          Exit;
        end;
      end;
      for Index := 0 to RainfallConcentrations.Count - 1 do
      begin
        result := Item.RainfallConcentrations[Index].Value = RainfallConcentrations[Index].Value;
        if not Result then
        begin
          Exit;
        end;
      end;
      for Index := 0 to EvapConcentrations.Count - 1 do
      begin
        result := Item.EvapConcentrations[Index].Value = EvapConcentrations[Index].Value;
        if not Result then
        begin
          Exit;
        end;
      end;
      for Index := 0 to RunoffConcentrations.Count - 1 do
      begin
        result := Item.RunoffConcentrations[Index].Value = RunoffConcentrations[Index].Value;
        if not Result then
        begin
          Exit;
        end;
      end;
      for Index := 0 to InflowConcentrations.Count - 1 do
      begin
        result := Item.InflowConcentrations[Index].Value = InflowConcentrations[Index].Value;
        if not Result then
        begin
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TSfrMf6Item.Loaded;
var
  Index: Integer;
begin
  DiversionCount := FDiversions.Count;
  for Index := 0 to FDiversions.Count -1 do
  begin
    DiversionFormulas[Index] := FDiversions[Index];
  end;
end;

procedure TSfrMf6Item.RemoveFormulaObjects;
var
  DivIndex: Integer;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FInflow,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRainfall,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FEvaporation,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRunoff,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FUpstreamFraction,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FStage,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRoughness,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  for DivIndex := 0 to FDiversionFormulas.Count - 1 do
  begin
    frmGoPhast.PhastModel.FormulaManager.Remove(FDiversionFormulas[DivIndex],
      GlobalRemoveModflowBoundaryItemSubscription,
      GlobalRestoreModflowBoundaryItemSubscription, self);
  end;

end;

procedure TSfrMf6Item.SetBoundaryFormula(Index: integer; const Value: string);
var
  ChemSpeciesCount: Integer;
begin
  case Index of
    SfrMf6InflowPosition:
      Inflow := Value;
    SfrMf6RainfallPosition:
      Rainfall := Value;
    SfrMf6EvaporationPosition:
      Evaporation := Value;
    SfrMf6RunoffPosition:
      Runoff := Value;
    SfrMf6UpstreamFractionPosition:
      UpstreamFraction := Value;
    SfrMf6StagePosition:
      Stage := Value;
    SfrMf6RoughnessPosition:
      Roughness := Value;

    else
      begin
        Index := Index-SfrMf6DiversionStartPosition;
        if Index < FDiversionFormulas.Count then
        begin
          DiversionFormulas[Index-SfrMf6DiversionStartPosition] := Value;
        end;
        Index := Index - FDiversionFormulas.Count;

        // GWT
        if frmGoPhast.PhastModel.GwtUsed then
        begin

          ChemSpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;
          while SpecifiedConcentrations.Count < ChemSpeciesCount do
          begin
            SpecifiedConcentrations.Add;
          end;
          if Index < ChemSpeciesCount then
          begin
            SpecifiedConcentrations[Index].Value := Value;
            Exit;
          end;
          Index := Index - ChemSpeciesCount;

          while RainfallConcentrations.Count < ChemSpeciesCount do
          begin
            RainfallConcentrations.Add;
          end;
          if Index < ChemSpeciesCount then
          begin
            RainfallConcentrations[Index].Value := Value;
            Exit;
          end;
          Index := Index - ChemSpeciesCount;

          while EvapConcentrations.Count < ChemSpeciesCount do
          begin
            EvapConcentrations.Add;
          end;
          if Index < ChemSpeciesCount then
          begin
            EvapConcentrations[Index].Value := Value;
            Exit;
          end;
          Index := Index - ChemSpeciesCount;

          while RunoffConcentrations.Count < ChemSpeciesCount do
          begin
            RunoffConcentrations.Add;
          end;
          if Index < ChemSpeciesCount then
          begin
            RunoffConcentrations[Index].Value := Value;
            Exit;
          end;
          Index := Index - ChemSpeciesCount;

          while InflowConcentrations.Count < ChemSpeciesCount do
          begin
            InflowConcentrations.Add;
          end;
          if Index < ChemSpeciesCount then
          begin
            InflowConcentrations[Index].Value := Value;
            Exit;
          end;
          Assert(False);
        end;
      end;
  end;
end;

procedure TSfrMf6Item.SetDiversionCount(const Value: Integer);
var
  FormulaObj: TFormulaObject;
begin
  While Value > FDiversionFormulas.Count do
  begin
    FDiversionFormulas.Add(CreateFormulaObject(dso3D));
  end;
  UpdateBoundaryObservers;

  While Value < FDiversionFormulas.Count do
  begin
    FormulaObj := FDiversionFormulas[FDiversionFormulas.Count-1];
    UpdateFormulaBlocks('0', SfrMf6DiversionStartPosition +FDiversionFormulas.Count-1, FormulaObj);
    FDiversionFormulas[FDiversionFormulas.Count-1] := FormulaObj;
    frmGoPhast.PhastModel.FormulaManager.Remove(FormulaObj,
      GlobalRemoveModflowBoundaryItemSubscription,
      GlobalRestoreModflowBoundaryItemSubscription, self);
    FDiversionFormulas.Delete(FDiversionFormulas.Count-1);
  end;
end;

procedure TSfrMf6Item.SetDiversionFormula(Index: Integer; const Value: string);
var
  FormulaObject: TFormulaObject;
begin
  FormulaObject := FDiversionFormulas[Index];
  UpdateFormulaBlocks(Value, SfrMf6DiversionStartPosition+Index, FormulaObject);
  FDiversionFormulas[Index] := FormulaObject;
end;

procedure TSfrMf6Item.SetDiversions(const Value: TStrings);
begin
  UpdateBoundaryObservers;
  FDiversions.Assign(Value);
end;

procedure TSfrMf6Item.SetEvapConcentrations(const Value: TSftGwtConcCollection);
begin
  FEvapConcentrations.Assign(Value);
end;

procedure TSfrMf6Item.SetEvaporation(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FEvaporation.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, SfrMf6EvaporationPosition, FEvaporation);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateSfr6Evaporation(self);
      end;
    end;
  end;
end;

procedure TSfrMf6Item.SetGwtStatus(const Value: TGwtStrStatusCollection);
begin
  FGwtStatus.Assign(Value);
end;

procedure TSfrMf6Item.SetInflow(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FInflow.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, SfrMf6InflowPosition, FInflow);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateSfr6Inflow(self);
      end;
    end;
  end;
end;

procedure TSfrMf6Item.SetInflowConcentrations(
  const Value: TSftGwtConcCollection);
begin
  FInflowConcentrations.Assign(Value);
end;

procedure TSfrMf6Item.SetRainfall(const Value: string);
begin
  UpdateFormulaBlocks(Value, SfrMf6RainfallPosition, FRainfall);
end;

procedure TSfrMf6Item.SetRainfallConcentrations(
  const Value: TSftGwtConcCollection);
begin
  FRainfallConcentrations.Assign(Value);
end;

procedure TSfrMf6Item.SetRoughness(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FRoughness.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, SfrMf6RoughnessPosition, FRoughness);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateSfr6Roughness(self);
      end;
    end;
  end;
end;

procedure TSfrMf6Item.SetRunoff(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FRunoff.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, SfrMf6RunoffPosition, FRunoff);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateSfr6Runoff(self);
      end;
    end;
  end;
end;

procedure TSfrMf6Item.SetRunoffConcentrations(
  const Value: TSftGwtConcCollection);
begin
  FRunoffConcentrations.Assign(Value);
end;

procedure TSfrMf6Item.SetSpecifiedConcentrations(
  const Value: TSftGwtConcCollection);
begin
  FSpecifiedConcentrations.Assign(Value);
end;

procedure TSfrMf6Item.SetStage(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FStage.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, SfrMf6StagePosition, FStage);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateSfr6Stage(self);
      end;
    end;
  end;
end;

procedure TSfrMf6Item.SetStatus(const Value: Boolean);
begin
  StreamStatus := TStreamStatus(Value);
end;

procedure TSfrMf6Item.SetStreamStatus(const Value: TStreamStatus);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FStreamStatus <> Value then
  begin
    FStreamStatus := Value;
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateSfr6StreamStatus(self);
      end;
    end;
    InvalidateModel;
  end;
end;

procedure TSfrMf6Item.SetUpstreamFraction(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FUpstreamFraction.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, SfrMf6UpstreamFractionPosition, FUpstreamFraction);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateSfr6UpstreamFraction(self);
      end;
    end;
  end;
end;

procedure TSfrMf6Item.UpdateBoundaryObservers;
var
  Observer: TObserver;
  LocalScreenObject: TScreenObject;
begin
  if (ScreenObject <> nil) and not frmGoPhast.PhastModel.Clearing then
  begin
    while FObserverList.Count < BoundaryFormulaCount do
    begin
      Observer := TObserver.Create(nil);
      FObserverList.Add(Observer);
      LocalScreenObject := ScreenObject as TScreenObject;
      if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
      begin
        LocalScreenObject.TalksTo(Observer);
      end;
    end;
  end;
end;

{ TStrMf6Collection }

procedure TSfrMf6Collection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TSfrMf6Storage.Create(AModel));
end;

function TSfrMf6Collection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
//  Boundary: TGhbBoundary;
//  ScreenObject: TScreenObject;
  Item: TSfrMf6Item;
begin
  Item := Items[ItemIndex] as TSfrMf6Item;
//  if FormulaIndex = ConductancePosition then
//  begin
//    Boundary := BoundaryGroup as TGhbBoundary;
//    ScreenObject := Boundary.ScreenObject as TScreenObject;
//    case Boundary.FormulaInterpretation of
//      fiSpecific:
//        begin
//          if ScreenObject.ScreenObjectLength = 0 then
//          begin
//            result := Item.Conductance;
//          end
//          else if ScreenObject.Closed then
//          begin
//            result := '(' + Item.Conductance
//              + ') * ' + StrObjectIntersectArea;
//          end
//          else
//          begin
//            result := '(' + Item.Conductance
//              + ') * ' + StrObjectSectionIntersectLength;
//          end;
//        end;
//      fiDirect:
//        begin
//          result := Item.Conductance;
//        end;
//      fiTotal:
//        begin
//          if ScreenObject.ScreenObjectLength = 0 then
//          begin
//            result := Item.Conductance;
//          end
//          else if ScreenObject.Closed then
//          begin
//            result := '((' + Item.Conductance
//              + ') * ' + StrObjectIntersectArea + ') / ' + StrObjectArea;
//          end
//          else
//          begin
//            result := '((' + Item.Conductance
//              + ') * ' + StrObjectSectionIntersectLength+ ') / ' + StrObjectLength;
//          end;
//        end;
//      else Assert(False);
//    end;
//  end
//  else
//  begin
    result := Item.BoundaryFormula[FormulaIndex];
//  end;
end;

procedure TSfrMf6Collection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList;
  AModel: TBaseModel; AScreenObject: TObject; PestName: string;
  PestSeriesName: string; PestSeriesMethod: TPestParamMethod;
  TimeSeriesName: string);
var
  Sfr6Storage: TSfrMf6Storage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
  RequiredLength: Integer;
  FractionAnnotation: string;
  FormulaIndex: Integer;
  DiversionCount: Integer;
  SpeciesCount: Integer;
begin
  BoundaryGroup.Mf6TimeSeriesNames.Add(TimeSeriesName);
  Assert(Expression <> nil);

  Sfr6Storage := BoundaryStorage as TSfrMf6Storage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);
    FractionAnnotation := Format(StrAllButTheFirstRe, [(AScreenObject as TScreenObject).Name]);

    Expression.Evaluate;
    with Sfr6Storage.SfrMF6Array[Index] do
    begin
      case BoundaryFunctionIndex of
        SfrMf6InflowPosition:
          begin
            Inflow := Expression.DoubleResult;
            InflowAnnotation := ACell.Annotation;
            InflowPest := PestName;
            InflowPestSeriesName := PestSeriesName;
            InflowPestSeriesMethod := PestSeriesMethod;
            InflowTimeSeriesName := TimeSeriesName;
          end;
        SfrMf6RainfallPosition:
          begin
            Rainfall := Expression.DoubleResult;
            RainfallAnnotation := ACell.Annotation;
            RainfallPest := PestName;
            RainfallPestSeriesName := PestSeriesName;
            RainfallPestSeriesMethod := PestSeriesMethod;
            RainfallTimeSeriesName := TimeSeriesName;
          end;
        SfrMf6EvaporationPosition:
          begin
            Evaporation := Expression.DoubleResult;
            EvaporationAnnotation := ACell.Annotation;
            EvaporationPest := PestName;
            EvaporationPestSeriesName := PestSeriesName;
            EvaporationPestSeriesMethod := PestSeriesMethod;
            EvaporationTimeSeriesName := TimeSeriesName;
          end;
        SfrMf6RunoffPosition:
          begin
            Runoff := Expression.DoubleResult;
            RunoffAnnotation := ACell.Annotation;
            RunoffPest := PestName;
            RunoffPestSeriesName := PestSeriesName;
            RunoffPestSeriesMethod := PestSeriesMethod;
            RunoffTimeSeriesName := TimeSeriesName;
          end;
        SfrMf6UpstreamFractionPosition:
          begin
            if Index = 0 then
            begin
              UpstreamFraction := Expression.DoubleResult;
              UpstreamFractionAnnotation := ACell.Annotation;
              UpstreamFractionPest := PestName;
              UpstreamFractionPestSeriesName := PestSeriesName;
              UpstreamFractionPestSeriesMethod := PestSeriesMethod;
            end
            else
            begin
              UpstreamFraction := 1;
              UpstreamFractionAnnotation := FractionAnnotation;
              UpstreamFractionPest := PestName;
              UpstreamFractionPestSeriesName := PestSeriesName;
              UpstreamFractionPestSeriesMethod := PestSeriesMethod;
            end;
          end;
        SfrMf6StagePosition:
          begin
            Stage := Expression.DoubleResult;
            StageAnnotation := ACell.Annotation;
            StagePest := PestName;
            StagePestSeriesName := PestSeriesName;
            StagePestSeriesMethod := PestSeriesMethod;
            StageTimeSeriesName := TimeSeriesName;
          end;
        SfrMf6RoughnessPosition:
          begin
            Roughness := Expression.DoubleResult;
            RoughnessAnnotation := ACell.Annotation;
            RoughnessPest := PestName;
            RoughnessPestSeriesName := PestSeriesName;
            RoughnessPestSeriesMethod := PestSeriesMethod;
            RoughnessTimeSeriesName := TimeSeriesName;
          end;
        else
          begin
            DiversionCount := FSfrMf6Boundary.Diversions.Count;
            FormulaIndex := BoundaryFunctionIndex - SfrMf6DiversionStartPosition;
            if FormulaIndex < DiversionCount then
            begin
              if Index = CellList.Count - 1 then
              begin
                RequiredLength := BoundaryFunctionIndex - SfrMf6DiversionStartPosition + 1;
                if Length(Diversions) <> RequiredLength then
                begin
                  SetLength(Diversions, RequiredLength);
                  SetLength(DiversionAnnotations, RequiredLength);
                  SetLength(DiversionPests, RequiredLength);
                  SetLength(DiversionPestSeriesNames, RequiredLength);
                  SetLength(DiversionPestSeriesMethods, RequiredLength);
                  SetLength(DiversionTimeSeriesName, RequiredLength);
                end;
                Diversions[FormulaIndex]
                  := Expression.DoubleResult;
                DiversionAnnotations[FormulaIndex]
                  := ACell.Annotation;
                DiversionPests[FormulaIndex]
                  := PestName;
                DiversionPestSeriesNames[FormulaIndex]
                  := PestSeriesName;
                DiversionPestSeriesMethods[FormulaIndex]
                  := PestSeriesMethod;
                DiversionTimeSeriesName[FormulaIndex]
                  := TimeSeriesName;
              end;
              Exit;
            end;
            // GWT
            SpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;
            FormulaIndex := FormulaIndex - DiversionCount;
            if FormulaIndex < SpeciesCount then
            begin
              with Sfr6Storage.SfrMF6Array[Index] do
              begin
                SpecifiedConcentrations.Concentrations[FormulaIndex] := Expression.DoubleResult;
                SpecifiedConcentrations.ConcentrationAnnotations[FormulaIndex] := ACell.Annotation;
                SpecifiedConcentrations.ConcentrationPestNames[FormulaIndex] := PestName;
                SpecifiedConcentrations.ConcentrationPestSeriesNames[FormulaIndex] := PestSeriesName;
                SpecifiedConcentrations.ConcentrationPestSeriesMethods[FormulaIndex] := PestSeriesMethod;
                SpecifiedConcentrations.ConcentrationTimeSeriesNames[FormulaIndex] := TimeSeriesName;
              end;
              Exit;
            end;

            FormulaIndex := FormulaIndex - SpeciesCount;
            if FormulaIndex < SpeciesCount then
            begin
              with Sfr6Storage.SfrMF6Array[Index] do
              begin
                RainfallConcentrations.Concentrations[FormulaIndex] := Expression.DoubleResult;
                RainfallConcentrations.ConcentrationAnnotations[FormulaIndex] := ACell.Annotation;
                RainfallConcentrations.ConcentrationPestNames[FormulaIndex] := PestName;
                RainfallConcentrations.ConcentrationPestSeriesNames[FormulaIndex] := PestSeriesName;
                RainfallConcentrations.ConcentrationPestSeriesMethods[FormulaIndex] := PestSeriesMethod;
                RainfallConcentrations.ConcentrationTimeSeriesNames[FormulaIndex] := TimeSeriesName;
              end;
              Exit;
            end;
            
            FormulaIndex := FormulaIndex - SpeciesCount;
            if FormulaIndex < SpeciesCount then
            begin
              with Sfr6Storage.SfrMF6Array[Index] do
              begin
                EvapConcentrations.Concentrations[FormulaIndex] := Expression.DoubleResult;
                EvapConcentrations.ConcentrationAnnotations[FormulaIndex] := ACell.Annotation;
                EvapConcentrations.ConcentrationPestNames[FormulaIndex] := PestName;
                EvapConcentrations.ConcentrationPestSeriesNames[FormulaIndex] := PestSeriesName;
                EvapConcentrations.ConcentrationPestSeriesMethods[FormulaIndex] := PestSeriesMethod;
                EvapConcentrations.ConcentrationTimeSeriesNames[FormulaIndex] := TimeSeriesName;
              end;
              Exit;
            end;
            
            FormulaIndex := FormulaIndex - SpeciesCount;
            if FormulaIndex < SpeciesCount then
            begin
              with Sfr6Storage.SfrMF6Array[Index] do
              begin
                RunoffConcentrations.Concentrations[FormulaIndex] := Expression.DoubleResult;
                RunoffConcentrations.ConcentrationAnnotations[FormulaIndex] := ACell.Annotation;
                RunoffConcentrations.ConcentrationPestNames[FormulaIndex] := PestName;
                RunoffConcentrations.ConcentrationPestSeriesNames[FormulaIndex] := PestSeriesName;
                RunoffConcentrations.ConcentrationPestSeriesMethods[FormulaIndex] := PestSeriesMethod;
                RunoffConcentrations.ConcentrationTimeSeriesNames[FormulaIndex] := TimeSeriesName;
              end;
              Exit;
            end;
            
            FormulaIndex := FormulaIndex - SpeciesCount;
            if FormulaIndex < SpeciesCount then
            begin
              with Sfr6Storage.SfrMF6Array[Index] do
              begin
                InflowConcentrations.Concentrations[FormulaIndex] := Expression.DoubleResult;
                InflowConcentrations.ConcentrationAnnotations[FormulaIndex] := ACell.Annotation;
                InflowConcentrations.ConcentrationPestNames[FormulaIndex] := PestName;
                InflowConcentrations.ConcentrationPestSeriesNames[FormulaIndex] := PestSeriesName;
                InflowConcentrations.ConcentrationPestSeriesMethods[FormulaIndex] := PestSeriesMethod;
                InflowConcentrations.ConcentrationTimeSeriesNames[FormulaIndex] := TimeSeriesName;
              end;
              Exit;
            end;
          end;
      end;
    end;
  end;
end;

procedure TSfrMf6Collection.AssignDirectlySpecifiedValues(
  AnItem: TCustomModflowBoundaryItem; BoundaryStorage: TCustomBoundaryStorage);
var
  Sfr6Storage: TSfrMf6Storage;
  index: integer;
  SfrMf6Item: TSfrMf6Item;
  SpeciesCount: Integer;
  SpeciesIndex: Integer;
begin
  inherited;
  // GWT
  if frmGoPhast.PhastModel.GwtUsed then
  begin
    SpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;
  end
  else
  begin
    SpeciesCount := 0;
  end;
  Sfr6Storage := BoundaryStorage as TSfrMf6Storage;
  SfrMf6Item := AnItem as TSfrMf6Item;

  for index := 0 to Length(Sfr6Storage.FSfrMF6Array) - 1 do
  begin
    Sfr6Storage.FSfrMF6Array[index].Status := SfrMf6Item.StreamStatus;
    Sfr6Storage.FSfrMF6Array[index].ReachNumber := index+1;
    if (not SfrMf6Item.Status) and (index = 0) then
    begin
      Sfr6Storage.FSfrMF6Array[index].UpstreamFraction := 0;
      Sfr6Storage.FSfrMF6Array[index].UpstreamFractionAnnotation
        := StrUpstreamFractionIs
    end;

    SetLength(Sfr6Storage.FSfrMF6Array[index].GwtStatus, SpeciesCount);
    while SfrMf6Item.GwtStatus.Count < SpeciesCount do
    begin
      SfrMf6Item.GwtStatus.Add;
    end;
    for SpeciesIndex := 0 to SpeciesCount - 1 do
    begin
      Sfr6Storage.FSfrMF6Array[index].GwtStatus[SpeciesIndex] := 
        SfrMf6Item.GwtStatus[SpeciesIndex].GwtStreamStatus
    end;
  end;
end;

procedure TSfrMf6Collection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  Sfr6Storage: TSfrMf6Storage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  Sfr6Storage := BoundaryStorage as TSfrMf6Storage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    if ACell.LgrEdge then
    begin
      Continue;
    end;
    with Sfr6Storage.SfrMF6Array[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

constructor TSfrMf6Collection.Create(Boundary: TModflowScreenObjectProperty;
  Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  FSfrMf6Boundary := Boundary as TSfrMf6Boundary;
  SectionDuplicatesAllowed := True;
end;

function TSfrMf6Collection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TSfrMf6TimeListLink;
end;

procedure TSfrMf6Collection.InvalidateDiversionsData(Sender: TObject);
//var
//  PhastModel: TPhastModel;
//  Link: TSfrMf6TimeListLink;
//  ChildIndex: Integer;
//  ChildModel: TChildModel;
begin
//  if not (Sender as TObserver).UpToDate then
//  begin
//    PhastModel := frmGoPhast.PhastModel;
//    Link := TimeListLink.GetLink(PhastModel) as TSfrMf6TimeListLink;
//    Link.FUpstreamFraction.Invalidate;
//    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
//    begin
//      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
//      Link := TimeListLink.GetLink(ChildModel) as TSfrMf6TimeListLink;
//      Link.FUpstreamFraction.Invalidate;
//    end;
//  end;
end;

procedure TSfrMf6Collection.InvalidateEvapConcentrations(Sender: TObject);
var
  Index: Integer;
  TimeList: TModflowTimeList;
  PhastModel: TPhastModel;
  Link: TSfrMf6TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TSfrMf6TimeListLink;
    for Index := 0 to Link.FEvapConcList.Count - 1 do
    begin
      TimeList := Link.FEvapConcList[Index];
      TimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrMf6TimeListLink;
      for Index := 0 to Link.FEvapConcList.Count - 1 do
      begin
        TimeList := Link.FEvapConcList[Index];
        TimeList.Invalidate;
      end;
    end;
  end;
end;

procedure TSfrMf6Collection.InvalidateEvaporationData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrMf6TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TSfrMf6TimeListLink;
    Link.FEvaporation.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrMf6TimeListLink;
      Link.FEvaporation.Invalidate;
    end;
  end;
end;

procedure TSfrMf6Collection.InvalidateGwtStatus(Sender: TObject);
begin

end;

procedure TSfrMf6Collection.InvalidateInflowConcentrations(Sender: TObject);
var
  Index: Integer;
  TimeList: TModflowTimeList;
  PhastModel: TPhastModel;
  Link: TSfrMf6TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TSfrMf6TimeListLink;
    for Index := 0 to Link.FInflowConcList.Count - 1 do
    begin
      TimeList := Link.FInflowConcList[Index];
      TimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrMf6TimeListLink;
      for Index := 0 to Link.FInflowConcList.Count - 1 do
      begin
        TimeList := Link.FInflowConcList[Index];
        TimeList.Invalidate;
      end;
    end;
  end;
end;

procedure TSfrMf6Collection.InvalidateInflowData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrMf6TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TSfrMf6TimeListLink;
    Link.FInflow.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrMf6TimeListLink;
      Link.FInflow.Invalidate;
    end;
  end;
end;

procedure TSfrMf6Collection.InvalidateRainfallConcentrations(Sender: TObject);
var
  Index: Integer;
  TimeList: TModflowTimeList;
  PhastModel: TPhastModel;
  Link: TSfrMf6TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TSfrMf6TimeListLink;
    for Index := 0 to Link.FRainfallConcList.Count - 1 do
    begin
      TimeList := Link.FRainfallConcList[Index];
      TimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrMf6TimeListLink;
      for Index := 0 to Link.FRainfallConcList.Count - 1 do
      begin
        TimeList := Link.FRainfallConcList[Index];
        TimeList.Invalidate;
      end;
    end;
  end;
end;

procedure TSfrMf6Collection.InvalidateRainfallData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrMf6TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TSfrMf6TimeListLink;
    Link.FRainfall.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrMf6TimeListLink;
      Link.FRainfall.Invalidate;
    end;
  end;
end;

procedure TSfrMf6Collection.InvalidateRoughnessData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrMf6TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TSfrMf6TimeListLink;
    Link.FRoughness.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrMf6TimeListLink;
      Link.FRoughness.Invalidate;
    end;
  end;
end;

procedure TSfrMf6Collection.InvalidateRunoffConcentrations(Sender: TObject);
var
  Index: Integer;
  TimeList: TModflowTimeList;
  PhastModel: TPhastModel;
  Link: TSfrMf6TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TSfrMf6TimeListLink;
    for Index := 0 to Link.FRunoffConcList.Count - 1 do
    begin
      TimeList := Link.FRunoffConcList[Index];
      TimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrMf6TimeListLink;
      for Index := 0 to Link.FRunoffConcList.Count - 1 do
      begin
        TimeList := Link.FRunoffConcList[Index];
        TimeList.Invalidate;
      end;
    end;
  end;
end;

procedure TSfrMf6Collection.InvalidateRunoffData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrMf6TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TSfrMf6TimeListLink;
    Link.FRunoff.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrMf6TimeListLink;
      Link.FRunoff.Invalidate;
    end;
  end;
end;

procedure TSfrMf6Collection.InvalidateSpecifiedConcentrations(Sender: TObject);
var
  Index: Integer;
  TimeList: TModflowTimeList;
  PhastModel: TPhastModel;
  Link: TSfrMf6TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TSfrMf6TimeListLink;
    for Index := 0 to Link.FSpecifiedConcList.Count - 1 do
    begin
      TimeList := Link.FSpecifiedConcList[Index];
      TimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrMf6TimeListLink;
      for Index := 0 to Link.FSpecifiedConcList.Count - 1 do
      begin
        TimeList := Link.FSpecifiedConcList[Index];
        TimeList.Invalidate;
      end;
    end;
  end;
end;

procedure TSfrMf6Collection.InvalidateStageData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrMf6TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TSfrMf6TimeListLink;
    Link.FStage.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrMf6TimeListLink;
      Link.FStage.Invalidate;
    end;
  end;
end;

procedure TSfrMf6Collection.InvalidateUpstreamFractionData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrMf6TimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TSfrMf6TimeListLink;
    Link.FUpstreamFraction.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrMf6TimeListLink;
      Link.FUpstreamFraction.Invalidate;
    end;
  end;
end;

class function TSfrMf6Collection.ItemClass: TBoundaryItemClass;
begin
  result := TSfrMf6Item
end;

procedure TSfrMf6Collection.Loaded;
var
  index: Integer;
  Item: TSfrMf6Item;
begin
  for index := 0 to Count - 1 do
  begin
    Item := Items[index] as TSfrMf6Item;
    Item.Loaded;
  end;
end;

procedure TSfrMf6Collection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TSfrMf6Storage).FSfrMF6Array, BoundaryCount);
  (Boundaries[ItemIndex, AModel] as TSfrMf6Storage).SpeciesCount
    := (AModel as TCustomModel).MobileComponents.Count;
  inherited;
end;

{ TSDiversionItem }

procedure TSDiversionItem.Assign(Source: TPersistent);
var
  Diversion: TSDiversionItem;
  SfrParamIcalc: TSfrParamIcalcItem;
  StrItem: TStrItem;
begin
  if Source is TSDiversionItem then
  begin
    Diversion := TSDiversionItem(Source);
    Priority := Diversion.Priority;
    DownstreamSegment := Diversion.DownstreamSegment;
  end
  else if Source is TSfrParamIcalcItem then
  begin
    SfrParamIcalc := TSfrParamIcalcItem(Source);
    DownstreamSegment := SfrParamIcalc.SegmentNumber;
    case Abs(SfrParamIcalc.IPRIOR) of
      0:
        begin
          Priority := cpUpTo;
        end;
      1:
        begin
          Priority := cpThreshold;
        end;
      2:
        begin
          Priority := cpFraction;
        end;
      3:
        begin
          Priority := cpExcess;
        end;
    end;
  end
  else if Source is TStrItem then
  begin
    StrItem := TStrItem(Source);
    DownstreamSegment := StrItem.SegmentNumber;
    Priority := cpUpTo;
  end;


  inherited;
end;

function TSDiversionItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Diversion: TSDiversionItem;
begin
  result := AnotherItem is TSDiversionItem;
  if result then
  begin
    Diversion := TSDiversionItem(AnotherItem);
    result := (Priority = Diversion.Priority)
      and (DownstreamSegment = Diversion.DownstreamSegment);
  end;
end;

procedure TSDiversionItem.SetPriority(const Value: TDivisionPriority);
begin
  if FPriority <> Value then
  begin
    FPriority := Value;
    InvalidateModel;
  end;
end;

procedure TSDiversionItem.SetDownstreamSegment(const Value: Integer);
begin
  if FDownstreamSegment <> Value then
  begin
    FDownstreamSegment := Value;
    InvalidateModel;
  end;
end;

{ TDiversionCollection }

function TDiversionCollection.Add: TSDiversionItem;
begin
  result := inherited Add as TSDiversionItem;
end;

constructor TDiversionCollection.Create(Model: TBaseModel);
begin
  inherited Create(TSDiversionItem, Model);
end;

function TDiversionCollection.GetItem(Index: Integer): TSDiversionItem;
begin
  result := inherited Items[Index] as TSDiversionItem
end;

function TDiversionCollection.IsSame(
  AnOrderedCollection: TOrderedCollection): boolean;
begin
  result := (AnOrderedCollection is TDiversionCollection)
    and inherited;
end;

procedure TDiversionCollection.SetItem(Index: Integer;
  const Value: TSDiversionItem);
begin
  inherited Items[Index] := Value;
end;

{ TStrMf6Boundary }

procedure TSfrMf6Boundary.Assign(Source: TPersistent);
var
  SourceSfr6: TSfrMf6Boundary;
  SourceSfrMf2005: TSfrBoundary;
  LocalModel: TPhastModel;
  DefineByReach: Boolean;
  SfrItem: TSfrItem;
  UpstreamSegment: TSfrSegmentItem;
  DownstreamSegment: TSfrSegmentItem;
  ChannelItem: TSfrChannelItem;
  ItemIndex: Integer;
  SfrMf6Item: TSfrMf6Item;
  FlowItem: TSfrSegmentFlowItem;
  ParamItem: TSfrParamIcalcItem;
//  DiversionItem: TSDiversionItem;
  SourceStr: TStrBoundary;
  StrItem: TStrItem;
//  DiversionSeg: TSDiversionItem;
begin
  if Source is TSfrMf6Boundary then
  begin
    SourceSfr6 := TSfrMf6Boundary(Source);
    if Used <> SourceSfr6.Used then
    begin
      if (ParentModel <> nil) and (ScreenObject <> nil)
        and (ScreenObject as TScreenObject).CanInvalidateModel  then
      begin
        InvalidateDisplayTimeLists;
      end;
    end;

    SegmentNumber := SourceSfr6.SegmentNumber;
    DownstreamSegments := SourceSfr6.DownstreamSegments;
    Diversions := SourceSfr6.Diversions;
    ReachLength := SourceSfr6.ReachLength;
    ReachWidth := SourceSfr6.ReachWidth;
    Gradient := SourceSfr6.Gradient;
    StreambedTop := SourceSfr6.StreambedTop;
    StreambedThickness := SourceSfr6.StreambedThickness;
    HydraulicConductivity := SourceSfr6.HydraulicConductivity;

    PestInflowFormula := SourceSfr6.PestInflowFormula;
    PestInflowMethod := SourceSfr6.PestInflowMethod;
    PestRainfallFormula := SourceSfr6.PestRainfallFormula;
    PestRainfallMethod := SourceSfr6.PestRainfallMethod;
    PestEvaporationFormula := SourceSfr6.PestEvaporationFormula;
    PestEvaporationMethod := SourceSfr6.PestEvaporationMethod;
    PestRunoffFormula := SourceSfr6.PestRunoffFormula;
    PestRunoffMethod := SourceSfr6.PestRunoffMethod;
    PestUpstreamFractionFormula := SourceSfr6.PestUpstreamFractionFormula;
    PestUpstreamFractionMethod := SourceSfr6.PestUpstreamFractionMethod;
    PestStageFormula := SourceSfr6.PestStageFormula;
    PestStageMethod := SourceSfr6.PestStageMethod;
    PestRoughnessFormula := SourceSfr6.PestRoughnessFormula;
    PestRoughnessMethod := SourceSfr6.PestRoughnessMethod;
  end
  else if Source is TSfrBoundary then
  begin
    SourceSfrMf2005 := TSfrBoundary(Source);
    SegmentNumber := SourceSfrMf2005.SegmentNumber;
    LocalModel := frmGoPhast.PhastModel;
    DefineByReach := LocalModel.ModflowPackages.SfrPackage.ISFROPT in [1,2,3];

    SfrItem := SourceSfrMf2005.Values[0] as TSfrItem;
    ReachLength := SfrItem.ReachLength;

    UpstreamSegment := SourceSfrMf2005.UpstreamSegmentValues[0] as TSfrSegmentItem;
    DownstreamSegment := SourceSfrMf2005.DownstreamSegmentValues[0] as TSfrSegmentItem;
    if UpstreamSegment.StreamWidth = DownstreamSegment.StreamWidth then
    begin
      ReachWidth := UpstreamSegment.StreamWidth
    end
    else
    begin
      ReachWidth := 'Interpolate(FractionOfObjectLength, '
        + UpstreamSegment.StreamWidth
        + ', 0, '
        + DownstreamSegment.StreamWidth
        + ', 1)'
    end;


    if DefineByReach then
    begin
      Gradient := SfrItem.StreamSlope;
      StreambedTop := SfrItem.StreambedElevation;
      StreambedThickness := SfrItem.StreamBedThickness;
      HydraulicConductivity := SfrItem.HydraulicConductivity;
    end
    else
    begin
      Gradient := '((' + UpstreamSegment.StreambedElevation
        + ') - (' + DownstreamSegment.StreambedElevation
        + '))/'
        + StrObjectLength;
        
      if UpstreamSegment.StreambedElevation <> DownstreamSegment.StreambedElevation then
      begin
        StreambedTop := 'Interpolate(FractionOfObjectLength, ('
          + UpstreamSegment.StreambedElevation
          + '), 0, ('
          + DownstreamSegment.StreambedElevation
          + '), 1)';
      end
      else
      begin
        StreambedTop := UpstreamSegment.StreambedElevation;
      end; 
                                       
      if UpstreamSegment.StreambedThickness <> DownstreamSegment.StreambedThickness then      
      begin      
        StreambedThickness := 'Interpolate(FractionOfObjectLength, ('
          + UpstreamSegment.StreambedThickness
          + '), 0, ('
          + DownstreamSegment.StreambedThickness
          + '), 1)';
      end          
      else      
      begin      
        StreambedThickness := UpstreamSegment.StreambedThickness;
      end;    
              
      if UpstreamSegment.HydraulicConductivity <> DownstreamSegment.HydraulicConductivity then      
      begin      
        HydraulicConductivity := 'Interpolate(FractionOfObjectLength, ('
          + UpstreamSegment.HydraulicConductivity
          + '), 0, ('
          + DownstreamSegment.HydraulicConductivity
          + '), 1)';
      end          
      else      
      begin      
        HydraulicConductivity := UpstreamSegment.HydraulicConductivity;
      end;            
    end;

//    ChannelItem := SourceSfrMf2005.ChannelValues[0];
//    Roughness := ChannelItem.ChannelRoughness;

//    UpstreamFraction := '1';

    ParamItem := SourceSfrMf2005.ParamIcalc[0] as TSfrParamIcalcItem;
    DownstreamSegments.Clear;
    if ParamItem.OutflowSegment <> 0 then
    begin
      DownstreamSegments.Add.Value := ParamItem.OutflowSegment;
    end;

//    Diversions.Clear;
//    if ParamItem.DiversionSegment <> 0 then
//    begin
//      DiversionItem := Diversions.Add;
//      DiversionItem.DownstreamSegment := ParamItem.DiversionSegment;
//      case Abs(ParamItem.IPRIOR) of
//        0:
//          begin
//            DiversionItem.Priority := cpUpTo;
//          end;
//        1:
//          begin
//            DiversionItem.Priority := cpThreshold;
//          end;
//        2:
//          begin
//            DiversionItem.Priority := cpFraction;
//          end;
//        3:
//          begin
//            DiversionItem.Priority := cpExcess;
//          end;
//      end;
//    end;

    Values.Assign(SourceSfrMf2005.ParamIcalc);
    Assert(Values.Count = SourceSfrMf2005.UpstreamSegmentValues.Count);
    Assert(Values.Count = SourceSfrMf2005.SegmentFlows.Count);
    Assert(Values.Count = SourceSfrMf2005.ChannelValues.Count);

    for ItemIndex := 0 to Values.Count - 1 do
    begin
      SfrMf6Item := Values[ItemIndex] as TSfrMf6Item;

      UpstreamSegment := SourceSfrMf2005.UpstreamSegmentValues[ItemIndex] as TSfrSegmentItem;
      DownstreamSegment := SourceSfrMf2005.DownstreamSegmentValues[ItemIndex] as TSfrSegmentItem;
      if (UpstreamSegment.StreambedElevation = DownstreamSegment.StreambedElevation)
        and (UpstreamSegment.StreamDepth = DownstreamSegment.StreamDepth)
        then
      begin
        SfrMf6Item.Stage := '(' + UpstreamSegment.StreambedElevation + ') + ('
          + UpstreamSegment.StreamDepth + ')';
      end
      else
      begin
        SfrMf6Item.Stage := 'Interpolate(FractionOfObjectLength, ('
          + '(' + UpstreamSegment.StreambedElevation + ') + ('
            + UpstreamSegment.StreamDepth + ')'
          + '), 0, ('
          + '(' + DownstreamSegment.StreambedElevation + ') + ('
            + DownstreamSegment.StreamDepth + ')'
          + '), 1)';
      end;

      FlowItem := SourceSfrMf2005.SegmentFlows[ItemIndex] as TSfrSegmentFlowItem;
      if Diversions.Count = 0 then
      begin
        SfrMf6Item.Inflow := FlowItem.Flow;
      end
      else
      begin
        SfrMf6Item.Inflow := '0';
        SfrMf6Item.DiversionCount := 1;
        SfrMf6Item.DiversionFormulas[0] := FlowItem.Flow;
      end;
      SfrMf6Item.Rainfall := FlowItem.Precipitation;
      SfrMf6Item.Evaporation := FlowItem.Evapotranspiration;
      SfrMf6Item.Runoff := FlowItem.Runnoff;
      SfrMf6Item.Rainfall := FlowItem.Precipitation;
      SfrMf6Item.UpstreamFraction := '1';


      ChannelItem := SourceSfrMf2005.ChannelValues[ItemIndex];
      SfrMf6Item.Roughness := ChannelItem.ChannelRoughness;
    end;

    Exit;
  end
  else if Source is TStrBoundary then
  begin
    SourceStr := TStrBoundary(Source);
    SegmentNumber := SourceStr.SegmentNumber;
    ReachLength := '1';
    ReachWidth := '1';
    StreambedThickness := '1';
    if SourceStr.Values.Count > 0 then
    begin
      StrItem := SourceStr.Values[0] as TStrItem;
      Gradient := StrItem.Slope;
      StreambedTop := StrItem.BedTop;
//      Roughness := StrItem.Roughness;
      HydraulicConductivity := StrItem.Conductance;
//      UpstreamFraction := '1';
      if StrItem.OutflowSegment > 0 then
      begin
        DownstreamSegments.Add.Value := StrItem.OutflowSegment
      end;
	  
//      if StrItem.DiversionSegment > 0 then
//      begin
//        DiversionSeg := Diversions.Add;
//        DiversionSeg.DownstreamSegment := StrItem.DiversionSegment;
//        DiversionSeg.Priority := cpUpTo;
//      end;

    end;
    Values := SourceStr.Values;

    Exit;
  end;
  inherited;


end;

procedure TSfrMf6Boundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TSfrMF6_Cell;
  BoundaryValues: TSfrMF6Record;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TSfrMF6Storage;
  LocalModel: TCustomModel;
  MvrUsed: Boolean;
  LastIndex: Integer;
  LocalScreenObject: TScreenObject;
  InflowAnnotation: string;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TSfrMF6Storage;
  Assert(ScreenObject <> nil);
  LocalScreenObject := ScreenObject as TScreenObject;
  InflowAnnotation := Format(StrInflowIsOnlySetT, [LocalScreenObject.Name]);
  MvrUsed := (LocalScreenObject.ModflowMvr <> nil)
    and LocalScreenObject.ModflowMvr.Used
    and (LocalScreenObject.ModflowMvr.SourcePackageChoice = spcSfr);
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TSfrMF6_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.SfrMF6Array) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.SfrMF6Array)
      end;
      LastIndex := Length(LocalBoundaryStorage.SfrMF6Array) - 1;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.SfrMF6Array) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.SfrMF6Array[BoundaryIndex];
        BoundaryValues.MvrUsed := MvrUsed and (BoundaryIndex = LastIndex);
        BoundaryValues.MvrIndex := BoundaryIndex;
        if (BoundaryIndex > 0) then
        begin
          BoundaryValues.Inflow := 0;
          BoundaryValues.InflowPest := '';
          BoundaryValues.InflowPestSeriesName := '';
          BoundaryValues.InflowAnnotation := InflowAnnotation;
        end;
        Cell := TSfrMF6_Cell.Create;
        Assert(ScreenObject <> nil);
        Cell.IFace := (ScreenObject as TScreenObject).IFace;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.FValues := BoundaryValues;
        SetLength(Cell.FValues.Diversions, Length(Cell.FValues.Diversions));
        SetLength(Cell.FValues.DiversionAnnotations, Length(Cell.FValues.DiversionAnnotations));
        Cell.ScreenObject := ScreenObject;
        LocalModel.AdjustCellPosition(Cell);
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TSfrMf6Boundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TSfrMF6Collection;
end;

function TSfrMf6Boundary.BoundaryObserverPrefix: string;
begin
  result := 'Sfr6';
end;

constructor TSfrMf6Boundary.Create(Model: TBaseModel; ScreenObject: TObject);
var
  InvalidateEvent: TNotifyEvent;
  Index: Integer;
begin
  inherited;
  if Model = nil then
  begin
    InvalidateEvent := nil;
  end
  else
  begin
    InvalidateEvent := Model.Invalidate;
  end;
  FDownstreamSegments := TIntegerCollection.Create(InvalidateEvent);
  FDiversions := TDiversionCollection.Create(Model);
  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

  LinkReachLength;
  LinkReachWidth;
  LinkGradient;
  LinkStreambedTop;
  LinkStreambedThickness;
  LinkHydraulicConductivity;

  ReachLength := StrObjectIntersectLength;
  ReachWidth := '1';
  Gradient := '0.001';
  StreambedTop := '0';
  StreambedThickness := '1';
  HydraulicConductivity := '0';

  PestInflowFormula := '';
  PestRainfallFormula := '';
  PestEvaporationFormula := '';
  PestRunoffFormula := '';
  PestUpstreamFractionFormula := '';
  PestStageFormula := '';
  PestRoughnessFormula := '';

  for Index := SfrMf6InflowPosition to SfrMf6RoughnessPosition do
  begin
    PestBoundaryMethod[Index] := DefaultBoundaryMethod(Index);
  end;
end;

procedure TSfrMf6Boundary.CreateFormulaObjects;
begin
  FReachLength := CreateFormulaObjectBlocks(dso3D);
  FReachWidth := CreateFormulaObjectBlocks(dso3D);
  FGradient := CreateFormulaObjectBlocks(dso3D);
  FStreambedTop := CreateFormulaObjectBlocks(dso3D);
  FStreambedThickness := CreateFormulaObjectBlocks(dso3D);
  FHydraulicConductivity := CreateFormulaObjectBlocks(dso3D);

  FInflow := CreateFormulaObjectBlocks(dso3D);
  FRainfall := CreateFormulaObjectBlocks(dso3D);
  FEvaporation := CreateFormulaObjectBlocks(dso3D);
  FRunoff := CreateFormulaObjectBlocks(dso3D);
  FUpstreamFractionFormula := CreateFormulaObjectBlocks(dso3D);
  FStage := CreateFormulaObjectBlocks(dso3D);
  FRoughnessFormula := CreateFormulaObjectBlocks(dso3D);

end;

procedure TSfrMf6Boundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(ReachLengthObserver);
    FObserverList.Add(ReachWidthObserver);
    FObserverList.Add(GradientObserver);
    FObserverList.Add(StreambedTopObserver);
    FObserverList.Add(StreambedThicknessObserver);
    FObserverList.Add(HydraulicConductivityObserver);

    FObserverList.Add(PestInflowObserver);
    FObserverList.Add(PestRainfallObserver);
    FObserverList.Add(PestEvaporationObserver);
    FObserverList.Add(PestRunoffObserver);
    FObserverList.Add(PestUpstreamFractionObserver);
    FObserverList.Add(PestStageObserver);
    FObserverList.Add(PestRoughnessObserver);

  end;
end;

class function TSfrMf6Boundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    SfrMf6InflowPosition:
      begin
        result := ppmMultiply;
      end;
    SfrMf6RainfallPosition:
      begin
        result := ppmMultiply;
      end;
    SfrMf6EvaporationPosition:
      begin
        result := ppmMultiply;
      end;
    SfrMf6RunoffPosition:
      begin
        result := ppmMultiply;
      end;
    SfrMf6UpstreamFractionPosition:
      begin
        result := ppmMultiply;
      end;
    SfrMf6StagePosition:
      begin
        result := ppmAdd;
      end;
    SfrMf6RoughnessPosition:
      begin
        result := ppmMultiply;
      end;
    else
      begin
        result := inherited;
        Assert(False);
      end;
  end;
end;

destructor TSfrMf6Boundary.Destroy;
begin
  PestInflowFormula := '';
  PestRainfallFormula := '';
  PestEvaporationFormula := '';
  PestRunoffFormula := '';
  PestUpstreamFractionFormula := '';
  PestStageFormula := '';
  PestRoughnessFormula := '';

  FDiversions.Free;
  FDownstreamSegments.Free;
  RemoveFormulaObjects;
  inherited;
end;

procedure TSfrMf6Boundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
var
  ValueIndex: Integer;
  BoundaryStorage: TSfrMf6Storage;
begin
//  inherited;
  EvaluateListBoundaries(AModel);
  for ValueIndex := 0 to Values.Count - 1 do
  begin
//    Item := Values[ValueIndex] as TCustomModflowBoundaryItem;
    BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TSfrMf6Storage;
    AssignCells(BoundaryStorage, ValueTimeList, AModel);
  end;
end;

function TSfrMf6Boundary.GetGradient: string;
begin
  Result := FGradient.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(SfrMf6GradientPosition);
  end;
end;

function TSfrMf6Boundary.GetGradientObserver: TObserver;
begin
  if FGradientObserver = nil then
  begin
    CreateObserver('SFR6_Gradient_', FGradientObserver, nil);
  end;
  result := FGradientObserver;
end;

function TSfrMf6Boundary.GetHydraulicConductivity: string;
begin
  Result := FHydraulicConductivity.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(SfrMf6HydraulicConductivityPosition);
  end;
end;

function TSfrMf6Boundary.GetHydraulicConductivityObserver: TObserver;
begin
  if FHydraulicConductivityObserver = nil then
  begin
    CreateObserver('SFR6_HydraulicConductivity_', FHydraulicConductivityObserver, nil);
  end;
  result := FHydraulicConductivityObserver;
end;

function TSfrMf6Boundary.GetPestBoundaryFormula(FormulaIndex: integer): string;
begin
  case FormulaIndex of
    SfrMf6InflowPosition:
      begin
        result := PestInflowFormula;
      end;
    SfrMf6RainfallPosition:
      begin
        result := PestRainfallFormula;
      end;
    SfrMf6EvaporationPosition:
      begin
        result := PestEvaporationFormula;
      end;
    SfrMf6RunoffPosition:
      begin
        result := PestRunoffFormula;
      end;
    SfrMf6UpstreamFractionPosition:
      begin
        result := PestUpstreamFractionFormula;
      end;
    SfrMf6StagePosition:
      begin
        result := PestStageFormula;
      end;
    SfrMf6RoughnessPosition:
      begin
        result := PestRoughnessFormula;
      end;
    else
      begin
        result := inherited;
//        Assert(False);
      end;
  end;
end;

function TSfrMf6Boundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    SfrMf6InflowPosition:
      begin
        result := PestInflowMethod;
      end;
    SfrMf6RainfallPosition:
      begin
        result := PestRainfallMethod;
      end;
    SfrMf6EvaporationPosition:
      begin
        result := PestEvaporationMethod;
      end;
    SfrMf6RunoffPosition:
      begin
        result := PestRunoffMethod;
      end;
    SfrMf6UpstreamFractionPosition:
      begin
        result := PestUpstreamFractionMethod;
      end;
    SfrMf6StagePosition:
      begin
        result := PestStageMethod;
      end;
    SfrMf6RoughnessPosition:
      begin
        result := PestRoughnessMethod;
      end;
    else
      begin
        result := inherited;
//        Assert(False);
      end;
  end;
end;

function TSfrMf6Boundary.GetPestEvaporationFormula: string;
begin
  Result := FEvaporation.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(SfrMf6EvaporationPosition + SfrMf6PestBoundaryOffset);
  end;
end;

function TSfrMf6Boundary.GetPestEvaporationObserver: TObserver;
begin
  if FPestEvaporationObserver = nil then
  begin
    CreateObserver('PestEvaporation_', FPestEvaporationObserver, nil);
    FPestEvaporationObserver.OnUpToDateSet := InvalidateEvaporationData;
  end;
  result := FPestEvaporationObserver;
end;

function TSfrMf6Boundary.GetPestInflowFormula: string;
begin
  Result := FInflow.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(SfrMf6InflowPosition + SfrMf6PestBoundaryOffset);
  end;
end;

function TSfrMf6Boundary.GetPestInflowObserver: TObserver;
begin
  if FPestInflowObserver = nil then
  begin
    CreateObserver('PestInflow_', FPestInflowObserver, nil);
    FPestInflowObserver.OnUpToDateSet := InvalidateInflowData;
  end;
  result := FPestInflowObserver;
end;

function TSfrMf6Boundary.GetPestRainfallFormula: string;
begin
  Result := FRainfall.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(SfrMf6RainfallPosition + SfrMf6PestBoundaryOffset);
  end;
end;

function TSfrMf6Boundary.GetPestRainfallObserver: TObserver;
begin
  if FPestRainfallObserver = nil then
  begin
    CreateObserver('PestRainfall_', FPestRainfallObserver, nil);
    FPestRainfallObserver.OnUpToDateSet := InvalidateRainfallData;
  end;
  result := FPestRainfallObserver;
end;

function TSfrMf6Boundary.GetPestRoughnessFormula: string;
begin
  Result := FRoughnessFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(SfrMf6RoughnessPosition + SfrMf6PestBoundaryOffset);
  end;
end;

function TSfrMf6Boundary.GetPestRoughnessObserver: TObserver;
begin
  if FPestRoughnessObserver = nil then
  begin
    CreateObserver('PestRoughness_', FPestRoughnessObserver, nil);
    FPestRoughnessObserver.OnUpToDateSet := InvalidateRoughnessData;
  end;
  result := FPestRoughnessObserver;
end;

function TSfrMf6Boundary.GetPestRunoffFormula: string;
begin
  Result := FRunoff.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(SfrMf6RunoffPosition + SfrMf6PestBoundaryOffset);
  end;
end;

function TSfrMf6Boundary.GetPestRunoffObserver: TObserver;
begin
  if FPestRunoffObserver = nil then
  begin
    CreateObserver('PestRunoff_', FPestRunoffObserver, nil);
    FPestRunoffObserver.OnUpToDateSet := InvalidateRunoffData;
  end;
  result := FPestRunoffObserver;
end;

function TSfrMf6Boundary.GetPestStageFormula: string;
begin
  Result := FStage.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(SfrMf6StagePosition + SfrMf6PestBoundaryOffset);
  end;
end;

function TSfrMf6Boundary.GetPestStageObserver: TObserver;
begin
  if FPestStageObserver = nil then
  begin
    CreateObserver('PestStage_', FPestStageObserver, nil);
    FPestStageObserver.OnUpToDateSet := InvalidateStageData;
  end;
  result := FPestStageObserver;
end;

function TSfrMf6Boundary.GetPestUpstreamFractionFormula: string;
begin
  Result := FUpstreamFractionFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(SfrMf6UpstreamFractionPosition + SfrMf6PestBoundaryOffset);
  end;
end;

function TSfrMf6Boundary.GetPestUpstreamFractionObserver: TObserver;
begin
  if FPestUpstreamFractionObserver = nil then
  begin
    CreateObserver('PestUpstreamFraction_', FPestUpstreamFractionObserver, nil);
    FPestUpstreamFractionObserver.OnUpToDateSet := InvalidateUpstreamFractionData;
  end;
  result := FPestUpstreamFractionObserver;
end;

procedure TSfrMf6Boundary.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FReachLength then
  begin
    List.Add(FObserverList[SfrMf6ReachLengthPosition]);
  end;
  if Sender = FReachWidth then
  begin
    List.Add(FObserverList[SfrMf6ReachWidthPosition]);
  end;
  if Sender = FGradient then
  begin
    List.Add(FObserverList[SfrMf6GradientPosition]);
  end;
  if Sender = FStreambedTop then
  begin
    List.Add(FObserverList[SfrMf6StreambedTopPosition]);
  end;
  if Sender = FStreambedThickness then
  begin
    List.Add(FObserverList[SfrMf6StreambedThicknessPosition]);
  end;
  if Sender = FHydraulicConductivity then
  begin
    List.Add(FObserverList[SfrMf6HydraulicConductivityPosition]);
  end;

  if Sender = FInflow then
  begin
    List.Add(FObserverList[SfrMf6InflowPosition + SfrMf6PestBoundaryOffset]);
  end;
  if Sender = FRainfall then
  begin
    List.Add(FObserverList[SfrMf6RainfallPosition + SfrMf6PestBoundaryOffset]);
  end;
  if Sender = FEvaporation then
  begin
    List.Add(FObserverList[SfrMf6EvaporationPosition + SfrMf6PestBoundaryOffset]);
  end;
  if Sender = FRunoff then
  begin
    List.Add(FObserverList[SfrMf6RunoffPosition + SfrMf6PestBoundaryOffset]);
  end;
  if Sender = FUpstreamFractionFormula then
  begin
    List.Add(FObserverList[SfrMf6UpstreamFractionPosition + SfrMf6PestBoundaryOffset]);
  end;
  if Sender = FStage then
  begin
    List.Add(FObserverList[SfrMf6StagePosition + SfrMf6PestBoundaryOffset]);
  end;
  if Sender = FRoughnessFormula then
  begin
    List.Add(FObserverList[SfrMf6RoughnessPosition + SfrMf6PestBoundaryOffset]);
  end;


end;

function TSfrMf6Boundary.GetReachLength: string;
begin
  Result := FReachLength.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(SfrMf6ReachLengthPosition);
  end;
end;

function TSfrMf6Boundary.GetReachLengthObserver: TObserver;
begin
  if FReachLengthObserver = nil then
  begin
    CreateObserver('SFR6_ReachLength_', FReachLengthObserver, nil);
  end;
  result := FReachLengthObserver;
end;

function TSfrMf6Boundary.GetReachWidth: string;
begin
  Result := FReachWidth.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(SfrMf6ReachWidthPosition);
  end;
end;

function TSfrMf6Boundary.GetReachWidthObserver: TObserver;
begin
  if FReachWidthObserver = nil then
  begin
    CreateObserver('SFR6_ReachWidth_', FReachWidthObserver, nil);
  end;
  result := FReachWidthObserver;
end;

//function TSfrMf6Boundary.GetRoughness: string;
//begin
//  Result := FRoughness.Formula;
//  if ScreenObject <> nil then
//  begin
//    ResetItemObserver(RoughnessPosition);
//  end;
//end;

//function TSfrMf6Boundary.GetRoughnessObserver: TObserver;
//begin
//  if FRoughnessObserver = nil then
//  begin
//    CreateObserver('SFR6_Roughness_', FRoughnessObserver, nil);
//  end;
//  result := FRoughnessObserver;
//end;

function TSfrMf6Boundary.GetStreambedThickness: string;
begin
  Result := FStreambedThickness.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(SfrMf6StreambedThicknessPosition);
  end;
end;

function TSfrMf6Boundary.GetStreambedThicknessObserver: TObserver;
begin
  if FStreambedThicknessObserver = nil then
  begin
    CreateObserver('SFR6_StreambedThickness_', FStreambedThicknessObserver, nil);
  end;
  result := FStreambedThicknessObserver;
end;

function TSfrMf6Boundary.GetStreambedTop: string;
begin
  Result := FStreambedTop.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(SfrMf6StreambedTopPosition);
  end;
end;

function TSfrMf6Boundary.GetStreambedTopObserver: TObserver;
begin
  if FStreambedTopObserver = nil then
  begin
    CreateObserver('SFR6_StreambedTop_', FStreambedTopObserver, nil);
  end;
  result := FStreambedTopObserver;
end;

function TSfrMf6Boundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('PestSfr6_Used_', FUsedObserver, nil);
  end;
  result := FUsedObserver;
end;

procedure TSfrMf6Boundary.HandleChangedValue(Observer: TObserver);
begin
//  inherited;
  InvalidateDisplay;
end;

procedure TSfrMf6Boundary.InvalidateDisplay;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    InvalidateDisplayTimeLists;
  end;
end;

procedure TSfrMf6Boundary.InvalidateDisplayTimeLists;
var
  LocaModel: TPhastModel;
begin
  LocaModel := ParentModel as TPhastModel;
  if LocaModel.Clearing then
  begin
    Exit;
  end;
  LocaModel.InvalidateSfr6Inflow(self);
  LocaModel.InvalidateSfr6Rainfall(self);
  LocaModel.InvalidateSfr6Evaporation(self);
  LocaModel.InvalidateSfr6Runoff(self);
  LocaModel.InvalidateSfr6UpstreamFraction(self);
  LocaModel.InvalidateSfr6Stage(self);
  LocaModel.InvalidateSfr6Roughness(self);
  LocaModel.InvalidateSfr6StreamStatus(self);
  LocaModel.InvalidateSfr6ReachNumber(self);
end;

procedure TSfrMf6Boundary.InvalidateEvaporationData(Sender: TObject);
begin
  if ParentModel <> nil then
  begin
    (ParentModel as TPhastModel).InvalidateSfr6Evaporation(self);
  end;
end;

procedure TSfrMf6Boundary.InvalidateInflowData(Sender: TObject);
begin
  if ParentModel <> nil then
  begin
    (ParentModel as TPhastModel).InvalidateSfr6Inflow(self);
  end;
end;

procedure TSfrMf6Boundary.InvalidateRainfallData(Sender: TObject);
begin
  if ParentModel <> nil then
  begin
    (ParentModel as TPhastModel).InvalidateSfr6Rainfall(self);
  end;
end;

procedure TSfrMf6Boundary.InvalidateRoughnessData(Sender: TObject);
begin
  if ParentModel <> nil then
  begin
    (ParentModel as TPhastModel).InvalidateSfr6Roughness(self);
  end;
end;

procedure TSfrMf6Boundary.InvalidateRunoffData(Sender: TObject);
begin
  if ParentModel <> nil then
  begin
    (ParentModel as TPhastModel).InvalidateSfr6Runoff(self);
  end;
end;

procedure TSfrMf6Boundary.InvalidateStageData(Sender: TObject);
begin
  if ParentModel <> nil then
  begin
    (ParentModel as TPhastModel).InvalidateSfr6Stage(self);
  end;
end;

procedure TSfrMf6Boundary.InvalidateUpstreamFractionData(Sender: TObject);
begin
  if ParentModel <> nil then
  begin
    (ParentModel as TPhastModel).InvalidateSfr6UpstreamFraction(self);
  end;
end;

procedure TSfrMf6Boundary.LinkGradient;
var
  LocalScreenObject: TScreenObject;
  Sfr6GradientArray: TDataArray;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.TalksTo(GradientObserver);
    if ParentModel <> nil then
    begin
      Sfr6GradientArray := (ParentModel as TCustomModel).DataArrayManager.
        GetDataSetByName(KGradientSFR6);
      if Sfr6GradientArray <> nil then
      begin
        GradientObserver.TalksTo(Sfr6GradientArray);
      end;
    end;
  end;
end;

procedure TSfrMf6Boundary.LinkHydraulicConductivity;
var
  LocalScreenObject: TScreenObject;
  Sfr6HydraulicConductivityArray: TDataArray;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.TalksTo(HydraulicConductivityObserver);
    if ParentModel <> nil then
    begin
      Sfr6HydraulicConductivityArray := (ParentModel as TCustomModel).DataArrayManager.
        GetDataSetByName(KHydraulicConductivitySFR6);
      if Sfr6HydraulicConductivityArray <> nil then
      begin
        HydraulicConductivityObserver.TalksTo(Sfr6HydraulicConductivityArray);
      end;
    end;
  end;
end;

procedure TSfrMf6Boundary.LinkReachLength;
var
  LocalScreenObject: TScreenObject;
  Sfr6ReachLengthArray: TDataArray;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.TalksTo(ReachLengthObserver);
    if ParentModel <> nil then
    begin
      Sfr6ReachLengthArray := (ParentModel as TCustomModel).DataArrayManager.
        GetDataSetByName(KReachLengthSFR);
      if Sfr6ReachLengthArray <> nil then
      begin
        ReachLengthObserver.TalksTo(Sfr6ReachLengthArray);
      end;
    end;
  end;
end;

procedure TSfrMf6Boundary.LinkReachWidth;
var
  LocalScreenObject: TScreenObject;
  Sfr6ReachWidthArray: TDataArray;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.TalksTo(ReachWidthObserver);
    if ParentModel <> nil then
    begin
      Sfr6ReachWidthArray := (ParentModel as TCustomModel).DataArrayManager.
        GetDataSetByName(KReachWidthSFR6);
      if Sfr6ReachWidthArray <> nil then
      begin
        ReachWidthObserver.TalksTo(Sfr6ReachWidthArray);
      end;
    end;
  end;
end;

procedure TSfrMf6Boundary.LinkStreambedThickness;
var
  LocalScreenObject: TScreenObject;
  Sfr6StreambedThicknessArray: TDataArray;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.TalksTo(StreambedThicknessObserver);
    if ParentModel <> nil then
    begin
      Sfr6StreambedThicknessArray := (ParentModel as TCustomModel).DataArrayManager.
        GetDataSetByName(KStreambedThicknessSFR6);
      if Sfr6StreambedThicknessArray <> nil then
      begin
        StreambedThicknessObserver.TalksTo(Sfr6StreambedThicknessArray);
      end;
    end;
  end;
end;

procedure TSfrMf6Boundary.LinkStreambedTop;
var
  LocalScreenObject: TScreenObject;
  Sfr6StreambedTopArray: TDataArray;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.TalksTo(StreambedTopObserver);
    if ParentModel <> nil then
    begin
      Sfr6StreambedTopArray := (ParentModel as TCustomModel).DataArrayManager.
        GetDataSetByName(KStreambedTopSFR6);
      if Sfr6StreambedTopArray <> nil then
      begin
        StreambedTopObserver.TalksTo(Sfr6StreambedTopArray);
      end;
    end;
  end;
end;

procedure TSfrMf6Boundary.Loaded;
begin
  (Values as TSfrMf6Collection).Loaded;

  LinkReachLength;
  LinkReachWidth;
  LinkGradient;
  LinkStreambedTop;
  LinkStreambedThickness;
  LinkHydraulicConductivity;
end;

procedure TSfrMf6Boundary.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FReachLength,
    GlobalRemoveMFBoundarySubscription,
    GlobalRestoreMFBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FGradient,
    GlobalRemoveMFBoundarySubscription,
    GlobalRestoreMFBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FHydraulicConductivity,
    GlobalRemoveMFBoundarySubscription,
    GlobalRestoreMFBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FReachWidth,
    GlobalRemoveMFBoundarySubscription,
    GlobalRestoreMFBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FStreambedThickness,
    GlobalRemoveMFBoundarySubscription,
    GlobalRestoreMFBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FStreambedTop,
    GlobalRemoveMFBoundarySubscription,
    GlobalRestoreMFBoundarySubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FInflow,
    GlobalRemoveMFBoundarySubscription,
    GlobalRestoreMFBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRainfall,
    GlobalRemoveMFBoundarySubscription,
    GlobalRestoreMFBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FEvaporation,
    GlobalRemoveMFBoundarySubscription,
    GlobalRestoreMFBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRunoff,
    GlobalRemoveMFBoundarySubscription,
    GlobalRestoreMFBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FUpstreamFractionFormula,
    GlobalRemoveMFBoundarySubscription,
    GlobalRestoreMFBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FStage,
    GlobalRemoveMFBoundarySubscription,
    GlobalRestoreMFBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRoughnessFormula,
    GlobalRemoveMFBoundarySubscription,
    GlobalRestoreMFBoundarySubscription, self);




end;

procedure TSfrMf6Boundary.SetDiversions(const Value: TDiversionCollection);
begin
  FDiversions.Assign(Value);
end;

procedure TSfrMf6Boundary.SetDownstreamSegments(
  const Value: TIntegerCollection);
begin
  FDownstreamSegments.Assign(Value);
end;

procedure TSfrMf6Boundary.SetGradient(const Value: string);
begin
  UpdateFormulaBlocks(Value, SfrMf6GradientPosition, FGradient);
end;


procedure TSfrMf6Boundary.SetHydraulicConductivity(const Value: string);
begin
  UpdateFormulaBlocks(Value, SfrMf6HydraulicConductivityPosition, FHydraulicConductivity);
end;


procedure TSfrMf6Boundary.SetPestBoundaryFormula(FormulaIndex: integer;
  const Value: string);
begin
  case FormulaIndex of
    SfrMf6InflowPosition:
      begin
        PestInflowFormula := Value;
      end;
    SfrMf6RainfallPosition:
      begin
        PestRainfallFormula := Value;
      end;
    SfrMf6EvaporationPosition:
      begin
        PestEvaporationFormula := Value;
      end;
    SfrMf6RunoffPosition:
      begin
        PestRunoffFormula := Value;
      end;
    SfrMf6UpstreamFractionPosition:
      begin
        PestUpstreamFractionFormula := Value;
      end;
    SfrMf6StagePosition:
      begin
        PestStageFormula := Value;
      end;
    SfrMf6RoughnessPosition:
      begin
        PestRoughnessFormula := Value;
      end;
    else
      begin
        inherited;
//        Assert(False);
      end;
  end;
end;

procedure TSfrMf6Boundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
begin
  case FormulaIndex of
    SfrMf6InflowPosition:
      begin
        PestInflowMethod := Value;
      end;
    SfrMf6RainfallPosition:
      begin
        PestRainfallMethod := Value;
      end;
    SfrMf6EvaporationPosition:
      begin
        PestEvaporationMethod := Value;
      end;
    SfrMf6RunoffPosition:
      begin
        PestRunoffMethod := Value;
      end;
    SfrMf6UpstreamFractionPosition:
      begin
        PestUpstreamFractionMethod := Value;
      end;
    SfrMf6StagePosition:
      begin
        PestStageMethod := Value;
      end;
    SfrMf6RoughnessPosition:
      begin
        PestRoughnessMethod := Value;
      end;
    else
      begin
        inherited;
//        Assert(False);
      end;
  end;
end;

procedure TSfrMf6Boundary.SetPestEvaporationFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, SfrMf6EvaporationPosition + SfrMf6PestBoundaryOffset, FEvaporation);
end;

procedure TSfrMf6Boundary.SetPestEvaporationMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestEvaporationMethod, Value);
end;

procedure TSfrMf6Boundary.SetPestInflowFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, SfrMf6InflowPosition + SfrMf6PestBoundaryOffset, FInflow);
end;

procedure TSfrMf6Boundary.SetPestInflowMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestInflowMethod, Value);
end;

procedure TSfrMf6Boundary.SetPestRainfallFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, SfrMf6RainfallPosition + SfrMf6PestBoundaryOffset, FRainfall);
end;

procedure TSfrMf6Boundary.SetPestRainfallMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestRainfallMethod, Value);
end;

procedure TSfrMf6Boundary.SetPestRoughnessFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, SfrMf6RoughnessPosition + SfrMf6PestBoundaryOffset, FRoughnessFormula);
end;

procedure TSfrMf6Boundary.SetPestRoughnessMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestRoughnessMethod, Value);
end;

procedure TSfrMf6Boundary.SetPestRunoffFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, SfrMf6RunoffPosition + SfrMf6PestBoundaryOffset, FRunoff);
end;

procedure TSfrMf6Boundary.SetPestRunoffMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestRunoffMethod, Value);
end;

procedure TSfrMf6Boundary.SetPestStageFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, SfrMf6StagePosition + SfrMf6PestBoundaryOffset, FStage);
end;

procedure TSfrMf6Boundary.SetPestStageMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestStageMethod, Value);
end;

procedure TSfrMf6Boundary.SetPestUpstreamFractionFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, SfrMf6UpstreamFractionPosition + SfrMf6PestBoundaryOffset, FUpstreamFractionFormula);
end;

procedure TSfrMf6Boundary.SetPestUpstreamFractionMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestUpstreamFractionMethod, Value);
end;

procedure TSfrMf6Boundary.SetReachLength(const Value: string);
begin
  UpdateFormulaBlocks(Value, SfrMf6ReachLengthPosition, FReachLength);
end;

procedure TSfrMf6Boundary.SetReachWidth(const Value: string);
begin
  UpdateFormulaBlocks(Value, SfrMf6ReachWidthPosition, FReachWidth);
end;

//procedure TSfrMf6Boundary.SetRoughness(const Value: string);
//begin
//  UpdateFormula(Value, SteadyRoughnessPosition, FRoughness);
//end;
//

procedure TSfrMf6Boundary.SetSegmentNumber(const Value: Integer);
begin
  if FSegmentNumber <> Value then
  begin
    FSegmentNumber := Value;
    InvalidateModel;
  end;
end;

//procedure TSfrMf6Boundary.SetSfrInflowLocation(const Value: TSfrInflowLocation);
//begin
//  if FSfrInflowLocation <> Value then
//  begin
//    FSfrInflowLocation := Value;
//    InvalidateModel;
//  end;
//end;

procedure TSfrMf6Boundary.SetStreambedThickness(const Value: string);
begin
  UpdateFormulaBlocks(Value, SfrMf6StreambedThicknessPosition, FStreambedThickness);
end;

procedure TSfrMf6Boundary.SetStreambedTop(const Value: string);
begin
  UpdateFormulaBlocks(Value, SfrMf6StreambedTopPosition, FStreambedTop);
end;

//procedure TSfrMf6Boundary.SetUpstreamFraction(const Value: string);
//begin
//  UpdateFormula(Value, SteadyUpstreamFractionPosition, FUpstreamFraction);
//end;

{ TDivRecord }

procedure TDivRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin

end;

procedure TDivRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin

end;

{ TStrMf6_Cell }

procedure TSfrMf6_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompInt(Comp, StressPeriod);
  FValues.Cache(Comp, Strings);
end;

function TSfrMf6_Cell.GetColumn: integer;
begin
  result := FValues.Cell.Column;
end;

function TSfrMf6_Cell.GetEvapConcentrations: TGwtCellData;
begin
  result := FValues.EvapConcentrations
end;

function TSfrMf6_Cell.GetGwtStatus: TGwtStreamStatusArray;
begin
  result := FValues.GwtStatus
end;

function TSfrMf6_Cell.GetInflowConcentrations: TGwtCellData;
begin
  result := FValues.InflowConcentrations
end;

function TSfrMf6_Cell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TSfrMf6_Cell.GetIntegerValue(Index: integer;
  AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TSfrMf6_Cell.GetLayer: integer;
begin
  result := FValues.Cell.Layer;
end;

function TSfrMf6_Cell.GetMf6TimeSeriesName(Index: Integer): string;
begin
  case Index of
    SfrMf6InflowPosition: result := FValues.InflowTimeSeriesName;
    SfrMf6RainfallPosition: result := FValues.RainfallTimeSeriesName;
    SfrMf6EvaporationPosition: result := FValues.EvaporationTimeSeriesName;
    SfrMf6RunoffPosition: result := FValues.RunoffTimeSeriesName;
    SfrMf6UpstreamFractionPosition: result := inherited;
    SfrMf6StagePosition: result := FValues.StageTimeSeriesName;
    SfrMf6RoughnessPosition: result := FValues.RoughnessTimeSeriesName;
    else
      begin
        result := inherited;
      end;
  end;
end;

function TSfrMf6_Cell.GetMvrIndex: Integer;
begin
  result := Values.MvrIndex;
end;

function TSfrMf6_Cell.GetMvrUsed: Boolean;
begin
  result := Values.MvrUsed;
end;

function TSfrMf6_Cell.GetPestName(Index: Integer): string;
begin
  case Index of
    SfrMf6InflowPosition: result := FValues.InflowPest;
    SfrMf6RainfallPosition: result := FValues.RainfallPest;
    SfrMf6EvaporationPosition: result := FValues.EvaporationPest;
    SfrMf6RunoffPosition: result := FValues.RunoffPest;
    SfrMf6UpstreamFractionPosition: result := FValues.UpstreamFractionPest;
    SfrMf6StagePosition: result := FValues.StagePest;
    SfrMf6RoughnessPosition: result := FValues.RoughnessPest;
    else
      begin
        result := inherited;
      end;
  end;
end;

function TSfrMf6_Cell.GetPestSeriesMethod(Index: Integer): TPestParamMethod;
begin
  case Index of
    SfrMf6InflowPosition: result := FValues.InflowPestSeriesMethod;
    SfrMf6RainfallPosition: result := FValues.RainfallPestSeriesMethod;
    SfrMf6EvaporationPosition: result := FValues.EvaporationPestSeriesMethod;
    SfrMf6RunoffPosition: result := FValues.RunoffPestSeriesMethod;
    SfrMf6UpstreamFractionPosition: result := FValues.UpstreamFractionPestSeriesMethod;
    SfrMf6StagePosition: result := FValues.StagePestSeriesMethod;
    SfrMf6RoughnessPosition: result := FValues.RoughnessPestSeriesMethod;
    else
      begin
        result := inherited;
      end;
  end;
end;

function TSfrMf6_Cell.GetPestSeriesName(Index: Integer): string;
begin
  case Index of
    SfrMf6InflowPosition: result := FValues.InflowPestSeriesName;
    SfrMf6RainfallPosition: result := FValues.RainfallPestSeriesName;
    SfrMf6EvaporationPosition: result := FValues.EvaporationPestSeriesName;
    SfrMf6RunoffPosition: result := FValues.RunoffPestSeriesName;
    SfrMf6UpstreamFractionPosition: result := FValues.UpstreamFractionPestSeriesName;
    SfrMf6StagePosition: result := FValues.StagePestSeriesName;
    SfrMf6RoughnessPosition: result := FValues.RoughnessPestSeriesName;
    else
      begin
        result := inherited;
      end;
  end;
end;

function TSfrMf6_Cell.GetRainfallConcentrations: TGwtCellData;
begin
  result := FValues.RainfallConcentrations
end;

function TSfrMf6_Cell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    SfrMf6InflowPosition: result := FValues.InflowAnnotation;
    SfrMf6RainfallPosition: result := FValues.RainfallAnnotation;
    SfrMf6EvaporationPosition: result := FValues.EvaporationAnnotation;
    SfrMf6RunoffPosition: result := FValues.RunoffAnnotation;
    SfrMf6UpstreamFractionPosition: result := FValues.UpstreamFractionAnnotation;
    SfrMf6StagePosition: result := FValues.StageAnnotation;
    SfrMf6RoughnessPosition: result := FValues.RoughnessAnnotation;
    else
      begin
        result := FValues.DiversionAnnotations[Index-SfrMf6DiversionStartPosition];
      end;
  end;
end;

function TSfrMf6_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
//  result := 0;
  case Index of
    SfrMf6InflowPosition: result := FValues.Inflow;
    SfrMf6RainfallPosition: result := FValues.Rainfall;
    SfrMf6EvaporationPosition: result := FValues.Evaporation;
    SfrMf6RunoffPosition: result := FValues.Runoff;
    SfrMf6UpstreamFractionPosition: result := FValues.UpstreamFraction;
    SfrMf6StagePosition: result := FValues.Stage;
    SfrMf6RoughnessPosition: result := FValues.Roughness;
    else
      begin
        result := FValues.Diversions[Index-SfrMf6DiversionStartPosition];
      end;
  end;
end;

function TSfrMf6_Cell.GetRow: integer;
begin
  result := FValues.Cell.Row;
end;

function TSfrMf6_Cell.GetRunoffConcentrations: TGwtCellData;
begin
  result := FValues.RunoffConcentrations
end;

function TSfrMf6_Cell.GetSection: integer;
begin
  result := FValues.Cell.Section;
end;

function TSfrMf6_Cell.GetSpecifiedConcentrations: TGwtCellData;
begin
  result := FValues.SpecifiedConcentrations
end;

procedure TSfrMf6_Cell.RecordStrings(Strings: TStringList);
begin
  FValues.RecordStrings(Strings);
end;

procedure TSfrMf6_Cell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
 StressPeriod := ReadCompInt(Decomp);
 FValues.Restore(Decomp, Annotations);
end;

procedure TSfrMf6_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TSfrMf6_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TSfrMf6_Cell.SetMf6TimeSeriesName(Index: Integer;
  const Value: string);
begin
  case Index of
    SfrMf6InflowPosition:
      FValues.InflowTimeSeriesName := Value;
    SfrMf6RainfallPosition:
      FValues.RainfallTimeSeriesName := Value;
    SfrMf6EvaporationPosition:
      FValues.EvaporationTimeSeriesName := Value;
    SfrMf6RunoffPosition:
      FValues.RunoffTimeSeriesName := Value;
    SfrMf6UpstreamFractionPosition:
      inherited;
    SfrMf6StagePosition:
      FValues.StageTimeSeriesName := Value;
    SfrMf6RoughnessPosition:
      FValues.RoughnessTimeSeriesName := Value;
    else
      begin
        inherited;
      end;
  end;
end;

procedure TSfrMf6_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TStrMF6ConstantRecord }

procedure TSfrMF6ConstantRecord.Cache(Comp: TCompressionStream;
  Strings: TStringList);
var
  index: Integer;
begin
  WriteCompCell(Comp, Cell);
  WriteCompInt(Comp, ReachNumber);
  WriteCompReal(Comp, ReachLength);
  WriteCompReal(Comp, ReachWidth);
  WriteCompReal(Comp, Gradient);
  WriteCompReal(Comp, StreambedTop);
  WriteCompReal(Comp, StreambedThickness);
  WriteCompReal(Comp, HydraulicConductivity);
//  WriteCompReal(Comp, Roughness);

  WriteCompInt(Comp, Length(ConnectedReaches));
  for index := 0 to Length(ConnectedReaches) - 1 do
  begin
    WriteCompInt(Comp, ConnectedReaches[index]);
  end;
  WriteCompInt(Comp, Length(DownstreamDiversions));
  for index := 0 to Length(DownstreamDiversions) - 1 do
  begin
    DownstreamDiversions[index].Cache(Comp, Strings);
  end;

  WriteCompInt(Comp, Strings.IndexOf(ReachLengthAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(ReachWidthAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(GradientAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(StreambedTopAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(StreambedThicknessAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(HydraulicConductivityAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(BoundName));

  WriteCompInt(Comp, Strings.IndexOf(PestReachLength));
  WriteCompInt(Comp, Strings.IndexOf(PestReachWidth));
  WriteCompInt(Comp, Strings.IndexOf(PestGradient));
  WriteCompInt(Comp, Strings.IndexOf(PestStreambedTop));
  WriteCompInt(Comp, Strings.IndexOf(PestStreambedThickness));
  WriteCompInt(Comp, Strings.IndexOf(PestHydraulicConductivity));

  WriteCompInt(Comp, Length(ConnectedReacheAnnotations));
  for index := 0 to Length(ConnectedReacheAnnotations) - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(ConnectedReacheAnnotations[index]));
  end;
end;

function TSfrMF6ConstantRecord.GetBoundaryAnnotation(Index: Integer): string;
begin
  case Index of
    SfrMf6ReachLengthPosition:
      begin
        result := ReachLengthAnnotation;
      end;
    SfrMf6ReachWidthPosition:
      begin
        result := ReachWidthAnnotation;
      end;
    SfrMf6GradientPosition:
      begin
        result := GradientAnnotation;
      end;
    SfrMf6StreambedTopPosition:
      begin
        result := StreambedTopAnnotation;
      end;
    SfrMf6StreambedThicknessPosition:
      begin
        result := StreambedThicknessAnnotation;
      end;
    SfrMf6HydraulicConductivityPosition:
      begin
        result := HydraulicConductivityAnnotation;
      end;
//    SteadyRoughnessPosition:
//      begin
//        result := RoughnessAnnotation;
//      end;
//    SteadyUpstreamFractionPosition:
//      begin
//        result := UpstreamFractionAnnotation;
//      end;
    else
      Assert(False);
  end
end;

function TSfrMF6ConstantRecord.GetBoundaryValue(Index: Integer): double;
begin
  result := 0;
  case Index of
    SfrMf6ReachLengthPosition:
      begin
        result := ReachLength;
      end;
    SfrMf6ReachWidthPosition:
      begin
        result := ReachWidth;
      end;
    SfrMf6GradientPosition:
      begin
        result := Gradient;
      end;
    SfrMf6StreambedTopPosition:
      begin
        result := StreambedTop;
      end;
    SfrMf6StreambedThicknessPosition:
      begin
        result := StreambedThickness;
      end;
    SfrMf6HydraulicConductivityPosition:
      begin
        result := HydraulicConductivity;
      end;
    else
      Assert(False);
  end;
end;

function TSfrMF6ConstantRecord.GetPestParamName(Index: Integer): string;
begin
  result := '';
  case Index of
    SfrMf6ReachLengthPosition:
      begin
        result := PestReachLength;
      end;
    SfrMf6ReachWidthPosition:
      begin
        result := PestReachWidth;
      end;
    SfrMf6GradientPosition:
      begin
        result := PestGradient;
      end;
    SfrMf6StreambedTopPosition:
      begin
        result := PestStreambedTop;
      end;
    SfrMf6StreambedThicknessPosition:
      begin
        result := PestStreambedThickness;
      end;
    SfrMf6HydraulicConductivityPosition:
      begin
        result := PestHydraulicConductivity;
      end;
    else
      Assert(False);
  end;
end;

function TSfrMF6ConstantRecord.IsConnected(Value: Integer): boolean;
var
  ConnectIndex: Integer;
begin
  result := False;
  for ConnectIndex := 0 to Length(ConnectedReaches) - 1 do
  begin
    if ConnectedReaches[ConnectIndex] = Value then
    begin
      result := True;
      Exit;
    end;
  end;
end;

procedure TSfrMF6ConstantRecord.RecordStrings(Strings: TStringList);
var
  index: Integer;
begin
  Strings.Add(ReachLengthAnnotation);
  Strings.Add(ReachWidthAnnotation);
  Strings.Add(GradientAnnotation);
  Strings.Add(StreambedTopAnnotation);
  Strings.Add(StreambedThicknessAnnotation);
  Strings.Add(HydraulicConductivityAnnotation);
  Strings.Add(BoundName);

  Strings.Add(PestReachLength);
  Strings.Add(PestReachWidth);
  Strings.Add(PestGradient);
  Strings.Add(PestStreambedTop);
  Strings.Add(PestStreambedThickness);
  Strings.Add(PestHydraulicConductivity);

//  Strings.Add(RoughnessAnnotation);
  for index := 0 to Length(ConnectedReacheAnnotations) - 1 do
  begin
    Strings.Add(ConnectedReacheAnnotations[index]);
  end;
end;

procedure TSfrMF6ConstantRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
var
  Count: Integer;
  index: Integer;
begin
  Cell := ReadCompCell(Decomp);
  ReachNumber := ReadCompInt(Decomp);
  ReachLength := ReadCompReal(Decomp);
  ReachWidth := ReadCompReal(Decomp);
  Gradient := ReadCompReal(Decomp);
  StreambedTop := ReadCompReal(Decomp);
  StreambedThickness := ReadCompReal(Decomp);
  HydraulicConductivity := ReadCompReal(Decomp);
//  Roughness := ReadCompReal(Decomp);

  StreambedTop := ReadCompReal(Decomp);

  Count := ReadCompInt(Decomp);
  SetLength(ConnectedReaches, Count);
  for index := 0 to Count - 1 do
  begin
    ConnectedReaches[index] := ReadCompInt(Decomp);
  end;

  Count := ReadCompInt(Decomp);
  SetLength(DownstreamDiversions, Count);
  for index := 0 to Count - 1 do
  begin
    DownstreamDiversions[index].Restore(Decomp, Annotations);
  end;

  ReachLengthAnnotation := Annotations[ReadCompInt(Decomp)];
  ReachWidthAnnotation := Annotations[ReadCompInt(Decomp)];
  GradientAnnotation := Annotations[ReadCompInt(Decomp)];
  StreambedTopAnnotation := Annotations[ReadCompInt(Decomp)];
  StreambedThicknessAnnotation := Annotations[ReadCompInt(Decomp)];
  HydraulicConductivityAnnotation := Annotations[ReadCompInt(Decomp)];
  BoundName := Annotations[ReadCompInt(Decomp)];

  PestReachLength := Annotations[ReadCompInt(Decomp)];
  PestReachWidth := Annotations[ReadCompInt(Decomp)];
  PestGradient := Annotations[ReadCompInt(Decomp)];
  PestStreambedTop := Annotations[ReadCompInt(Decomp)];
  PestStreambedThickness := Annotations[ReadCompInt(Decomp)];
  PestHydraulicConductivity := Annotations[ReadCompInt(Decomp)];

  Count := ReadCompInt(Decomp);
  SetLength(ConnectedReacheAnnotations, Count);
  for index := 0 to Count - 1 do
  begin
    ConnectedReacheAnnotations[index] := Annotations[ReadCompInt(Decomp)];
  end;
end;

procedure TSfrMF6ConstantRecord.SetBoundaryAnnotation(Index: Integer;
  const Value: string);
begin
  case Index of
    SfrMf6ReachLengthPosition:
      begin
        ReachLengthAnnotation := Value;
      end;
    SfrMf6ReachWidthPosition:
      begin
        ReachWidthAnnotation := Value;
      end;
    SfrMf6GradientPosition:
      begin
        GradientAnnotation := Value;
      end;
    SfrMf6StreambedTopPosition:
      begin
        StreambedTopAnnotation := Value;
      end;
    SfrMf6StreambedThicknessPosition:
      begin
        StreambedThicknessAnnotation := Value;
      end;
    SfrMf6HydraulicConductivityPosition:
      begin
        HydraulicConductivityAnnotation := Value;
      end;
//    SteadyRoughnessPosition:
//      begin
//        RoughnessAnnotation := Value;
//      end;
//    SteadyUpstreamFractionPosition:
//      begin
//        UpstreamFractionAnnotation := Value;
//      end;
    else
      Assert(False);
  end;
end;

procedure TSfrMF6ConstantRecord.SetBoundaryValue(Index: Integer;
  const Value: double);
begin
  case Index of
    SfrMf6ReachLengthPosition:
      begin
        ReachLength := Value;
      end;
    SfrMf6ReachWidthPosition:
      begin
        ReachWidth := Value;
      end;
    SfrMf6GradientPosition:
      begin
        Gradient := Value;
      end;
    SfrMf6StreambedTopPosition:
      begin
        StreambedTop := Value;
      end;
    SfrMf6StreambedThicknessPosition:
      begin
        StreambedThickness := Value;
      end;
    SfrMf6HydraulicConductivityPosition:
      begin
        HydraulicConductivity := Value;
      end;
//    SteadyRoughnessPosition:
//      begin
//        Roughness := Value;
//      end;
//    SteadyUpstreamFractionPosition:
//      begin
//        UpstreamFraction := Value;
//      end;
    else
      Assert(False);
  end;
end;

procedure TSfrMF6ConstantRecord.SetPestParamName(Index: Integer;
  const Value: string);
begin
  case Index of
    SfrMf6ReachLengthPosition:
      begin
        PestReachLength := Value;
      end;
    SfrMf6ReachWidthPosition:
      begin
        PestReachWidth := Value;
      end;
    SfrMf6GradientPosition:
      begin
        PestGradient := Value;
      end;
    SfrMf6StreambedTopPosition:
      begin
        PestStreambedTop := Value;
      end;
    SfrMf6StreambedThicknessPosition:
      begin
        PestStreambedThickness := Value;
      end;
    SfrMf6HydraulicConductivityPosition:
      begin
        PestHydraulicConductivity := Value;
      end;
    else
      Assert(False);
  end;
end;

procedure TSfrMF6ConstantRecord.SetReachNumber(const Value: integer);
begin
  FReachNumber := Value;
end;

{ TSfrMf6TimeListLink }

procedure TSfrMf6TimeListLink.CreateTimeLists;
var
  LocalModel: TCustomModel;
  PhastModel: TPhastModel;
  SpeciesIndex: Integer;
  ConcTimeList: TModflowTimeList;
begin
  FInflow := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FInflow.NonParamDescription := StrSFR6Inflow;
  FInflow.ParamDescription := StrSFR6Inflow;

  FRainfall := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FRainfall.NonParamDescription := StrSFR6Rainfall;
  FRainfall.ParamDescription := StrSFR6Rainfall;

  FEvaporation := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FEvaporation.NonParamDescription := StrSFR6Evaporation;
  FEvaporation.ParamDescription := StrSFR6Evaporation;

  FRunoff := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FRunoff.NonParamDescription := StrSFR6Runoff;
  FRunoff.ParamDescription := StrSFR6Runoff;

  FUpstreamFraction := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FUpstreamFraction.NonParamDescription := StrSFR6UpstreamFracti;
  FUpstreamFraction.ParamDescription := StrSFR6UpstreamFracti;

  FStage := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStage.NonParamDescription := StrSFR6Stage;
  FStage.ParamDescription := StrSFR6Stage;

  FRoughness := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FRoughness.NonParamDescription := StrSFR6Roughness;
  FRoughness.ParamDescription := StrSFR6Roughness;

  FStreamStatus := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStreamStatus.NonParamDescription := StrSFR6StreamStatus;
  FStreamStatus.ParamDescription := StrSFR6StreamStatus;

  FReachNumber := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FReachNumber.NonParamDescription := StrSFR6ReachNumber;
  FReachNumber.ParamDescription := StrSFR6ReachNumber;

  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    FInflow.OnInvalidate := LocalModel.InvalidateSfr6Inflow;
    FRainfall.OnInvalidate := LocalModel.InvalidateSfr6Rainfall;
    FEvaporation.OnInvalidate := LocalModel.InvalidateSfr6Evaporation;
    FRunoff.OnInvalidate := LocalModel.InvalidateSfr6Runoff;
    FUpstreamFraction.OnInvalidate := LocalModel.InvalidateSfr6UpstreamFraction;
    FStage.OnInvalidate := LocalModel.InvalidateSfr6Stage;
    FRoughness.OnInvalidate := LocalModel.InvalidateSfr6Roughness;
    FStreamStatus.OnInvalidate := LocalModel.InvalidateSfr6StreamStatus;
    FReachNumber.OnInvalidate := LocalModel.InvalidateSfr6ReachNumber;
  end;

  AddTimeList(FInflow);
  AddTimeList(FRainfall);
  AddTimeList(FEvaporation);
  AddTimeList(FRunoff);
  AddTimeList(FUpstreamFraction);
  AddTimeList(FStage);
  AddTimeList(FRoughness);
  AddTimeList(FStreamStatus);
  AddTimeList(FReachNumber);

  FGwtStatusList := TModflowTimeLists.Create;
  FSpecifiedConcList := TModflowTimeLists.Create;
  FRainfallConcList := TModflowTimeLists.Create;
  FEvapConcList := TModflowTimeLists.Create;
  FRunoffConcList := TModflowTimeLists.Create;
  FInflowConcList := TModflowTimeLists.Create;
  
  PhastModel := frmGoPhast.PhastModel;
  if PhastModel.GwtUsed then
  begin
    for SpeciesIndex := 0 to PhastModel.MobileComponents.Count - 1 do
    begin
      ConcTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
      ConcTimeList.NonParamDescription := PhastModel.MobileComponents[SpeciesIndex].Name + ' SFT Status';
      ConcTimeList.ParamDescription :=  ConcTimeList.NonParamDescription;
      if Model <> nil then
      begin
        LocalModel := Model as TCustomModel;
//        ConcTimeList.OnInvalidate := LocalModel.InvalidateMfWellConc;
      end;
      AddTimeList(ConcTimeList);
      FGwtStatusList.Add(ConcTimeList);

      ConcTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
      ConcTimeList.NonParamDescription := PhastModel.MobileComponents[SpeciesIndex].Name + ' SFT Specified Concentration';
      ConcTimeList.ParamDescription :=  ConcTimeList.NonParamDescription;
      if Model <> nil then
      begin
        LocalModel := Model as TCustomModel;
//        ConcTimeList.OnInvalidate := LocalModel.InvalidateMfWellConc;
      end;
      AddTimeList(ConcTimeList);
      FSpecifiedConcList.Add(ConcTimeList);

      ConcTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
      ConcTimeList.NonParamDescription := PhastModel.MobileComponents[SpeciesIndex].Name + ' SFT Rainfall Concentration';
      ConcTimeList.ParamDescription :=  ConcTimeList.NonParamDescription;
      if Model <> nil then
      begin
        LocalModel := Model as TCustomModel;
//        ConcTimeList.OnInvalidate := LocalModel.InvalidateMfWellConc;
      end;
      AddTimeList(ConcTimeList);
      FRainfallConcList.Add(ConcTimeList);

      ConcTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
      ConcTimeList.NonParamDescription := PhastModel.MobileComponents[SpeciesIndex].Name + ' SFT Evaporation Concentration';
      ConcTimeList.ParamDescription :=  ConcTimeList.NonParamDescription;
      if Model <> nil then
      begin
        LocalModel := Model as TCustomModel;
//        ConcTimeList.OnInvalidate := LocalModel.InvalidateMfWellConc;
      end;
      AddTimeList(ConcTimeList);
      FEvapConcList.Add(ConcTimeList);

      ConcTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
      ConcTimeList.NonParamDescription := PhastModel.MobileComponents[SpeciesIndex].Name + ' SFT Runoff Concentration';
      ConcTimeList.ParamDescription :=  ConcTimeList.NonParamDescription;
      if Model <> nil then
      begin
        LocalModel := Model as TCustomModel;
//        ConcTimeList.OnInvalidate := LocalModel.InvalidateMfWellConc;
      end;
      AddTimeList(ConcTimeList);
      FRunoffConcList.Add(ConcTimeList);

      ConcTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
      ConcTimeList.NonParamDescription := PhastModel.MobileComponents[SpeciesIndex].Name + ' SFT Inflow Concentration';
      ConcTimeList.ParamDescription :=  ConcTimeList.NonParamDescription;
      if Model <> nil then
      begin
        LocalModel := Model as TCustomModel;
//        ConcTimeList.OnInvalidate := LocalModel.InvalidateMfWellConc;
      end;
      AddTimeList(ConcTimeList);
      FInflowConcList.Add(ConcTimeList);
    end;
  end;
end;

destructor TSfrMf6TimeListLink.Destroy;
begin
  FGwtStatusList.Free;
  FSpecifiedConcList.Free;
  FRainfallConcList.Free;
  FEvapConcList.Free;
  FRunoffConcList.Free;
  FInflowConcList.Free;

  FReachNumber.Free;
  FStreamStatus.Free;
  FRoughness.Free;
  FStage.Free;
  FUpstreamFraction.Free;
  FRunoff.Free;
  FEvaporation.Free;
  FRainfall.Free;
  FInflow.Free;
  inherited;
end;

{ TSftGwtConcCollection }

constructor TSftGwtConcCollection.Create(Model: TBaseModel;
  AScreenObject: TObject; ParentCollection: TSfrMf6Collection);
begin
  inherited Create(Model, AScreenObject, ParentCollection);
end;

{ TGwtStrStatusItem }

procedure TGwtStrStatusItem.Assign(Source: TPersistent);
begin
  if Source is TGwtStrStatusItem then
  begin
    GwtStreamStatus := TGwtStrStatusItem(Source).GwtStreamStatus;
  end
  else
  begin
    inherited;
  end;
end;

procedure TGwtStrStatusItem.SetGwtStreamStatus(const Value: TGwtStreamStatus);
begin
  FGwtStreamStatus := Value;
end;

{ TGwtStrStatusCollection }

constructor TGwtStrStatusCollection.Create;
begin
  inherited Create(TGwtStrStatusItem);
end;

function TGwtStrStatusCollection.GetItems(Index: Integer): TGwtStrStatusItem;
begin
  result := inherited Items[Index] as TGwtStrStatusItem
end;

procedure TGwtStrStatusCollection.SetItems(Index: Integer;
  const Value: TGwtStrStatusItem);
begin
  inherited Items[Index] := Value;
end;

initialization
  InitializeSfrObNames

finalization
  SfrObNames.Free;

end.
