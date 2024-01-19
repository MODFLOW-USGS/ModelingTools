unit ModflowMawUnit;

interface

uses
  ZLib, Classes, ModflowCellUnit, ModflowBoundaryUnit, OrderedCollectionUnit,
  FormulaManagerUnit, FormulaManagerInterfaceUnit,
  GoPhastTypes, RbwParser, SubscriptionUnit, Mt3dmsChemUnit,
  System.SysUtils, GwtStatusUnit, Modflow6DynamicTimeSeriesInterfaceUnit;

type
  TMawOb = (moHead, moFromMvr, moFlowRate, moFlowRateCells, moPumpRate,
    moRateToMvr, moFlowingWellFlowRate, moFlowWellToMvr, moStorageFlowRate,
    moConstantFlowRate, moConductance,
    moConductanceCells, moFlowingWellConductance);
  TMawObs = set of TMawOb;

  TMwtOb = (mtoConcentration, mstoStorage, mtoConstant, mtoFromMvr, mtoMwt,
    mtoMwtCells, mtoRate, mtoFwRate, mtoRateToMvr, mtoFwRateToMvr);
  TMwtObs = set of TMwtOb;

  // mcmTheim is for backwards compatibility.
  TMawConductanceMethod = (mcmSpecified, mcmThiem, mcmSkin, mcmCumulative,
    mcmMean, mcmTheim);

  TMawStatus = (mwActive, mwInactive, mwConstantHead);
  TFlowingWell = (fwNotFlowing, fwFlowing);
  TRateLimitation = (rlNone, rlScaling, rlShutoff);

  // <mawsetting>
  TMawTransientRecord = record
    Cell: TCellLocation;
    WellNumber: Integer;
    MawStatus: TMawStatus;
    FlowingWell: TFlowingWell;
    // ShutOff and RateScaling can not be used simultaneously.
    ShutOff: Boolean;
    RateScaling: Boolean;
    HeadLimitChoice: Boolean;

    FlowingWellElevation: double;
    FlowingWellConductance: double;
    FlowingWellReductionLength: Double;
    Rate: double;
    WellHead: double;
    HeadLimit: double;
    MinRate: double;
    MaxRate: double;
    PumpElevation: double;
    ScalingLength: double;

    FlowingWellElevationAnnotation: string;
    FlowingWellConductanceAnnotation: string;
    FlowingWellReductionLengthAnnotation: string;
    RateAnnotation: string;
    WellHeadAnnotation: string;
    HeadLimitAnnotation: string;
    MinRateAnnotation: string;
    MaxRateAnnotation: string;
    PumpElevationAnnotation: string;
    ScalingLengthAnnotation: string;

    MvrUsed: Boolean;
    MvrIndex: Integer;

    FlowingWellElevationPest: string;
    FlowingWellConductancePest: string;
    FlowingWellReductionLengthPest: string;
    RatePest: string;
    WellHeadPest: string;
    HeadLimitPest: string;
    MinRatePest: string;
    MaxRatePest: string;
    PumpElevationPest: string;
    ScalingLengthPest: string;

    FlowingWellElevationPestSeriesName: string;
    FlowingWellConductancePestSeriesName: string;
    FlowingWellReductionLengthPestSeriesName: string;
    RatePestSeriesName: string;
    WellHeadPestSeriesName: string;
    HeadLimitPestSeriesName: string;
    MinRatePestSeriesName: string;
    MaxRatePestSeriesName: string;
    PumpElevationPestSeriesName: string;
    ScalingLengthPestSeriesName: string;

    FlowingWellElevationPestSeriesMethod: TPestParamMethod;
    FlowingWellConductancePestSeriesMethod: TPestParamMethod;
    FlowingWellReductionLengthPestSeriesMethod: TPestParamMethod;
    RatePestSeriesMethod: TPestParamMethod;
    WellHeadPestSeriesMethod: TPestParamMethod;
    HeadLimitPestSeriesMethod: TPestParamMethod;
    MinRatePestSeriesMethod: TPestParamMethod;
    MaxRatePestSeriesMethod: TPestParamMethod;
    PumpElevationPestSeriesMethod: TPestParamMethod;
    ScalingLengthPestSeriesMethod: TPestParamMethod;

    RateTimeSeriesName: string;
    WellHeadTimeSeriesName: string;

    // Buyancy
    // Species count should always be 1 for Density and Viscosity
    Density: TGwtCellData;
    Viscosity: TGwtCellData;

    // GWT
    GwtStatus: TGwtBoundaryStatusArray;
    SpecifiedConcentrations: TGwtCellData;
    InjectionConcentrations: TGwtCellData;

    procedure Assign(const Item: TMawTransientRecord);
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TMawTransientArray = array of TMawTransientRecord;

  TMawTransientStorage = class(TCustomBoundaryStorage)
  private
    FMawTransientArray: TMawTransientArray;
    FSpeciesCount: Integer;
    function GetMawTransientArray: TMawTransientArray;
    procedure SetSpeciesCount(const Value: Integer);
  protected
    procedure Restore(DecompressionStream: TDecompressionStream;
      Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property MawTransientArray: TMawTransientArray read GetMawTransientArray;
    property SpeciesCount: Integer read FSpeciesCount write SetSpeciesCount;
  end;

  TMawWellCollection = class;

  TMawGwtConcCollection = class(TGwtConcStringCollection)
    constructor Create(Model: TBaseModel; AScreenObject: TObject;
      ParentCollection: TMawWellCollection);
  end;

  TMawItem = class(TCustomModflowBoundaryItem)
  private
    FFlowingWell: TFlowingWell;
    FMawStatus: TMawStatus;
    FFlowingWellConductance: IFormulaObject;
    FFlowingWellElevation: IFormulaObject;
    FFlowingWellReductionLength: IFormulaObject;
    FHeadLimit: IFormulaObject;
    FMaxRate: IFormulaObject;
    FMinRate: IFormulaObject;
    FPumpElevation: IFormulaObject;
    FRate: IFormulaObject;
    FScalingLength: IFormulaObject;
    FWellHead: IFormulaObject;
    FHeadLimitChoice: Boolean;
    FRateLimitation: TRateLimitation;
    FInjectionConcentrations: TMawGwtConcCollection;
    FSpecifiedConcentrations: TMawGwtConcCollection;
    FGwtStatus: TGwtBoundaryStatusCollection;
    FDensity: TMawGwtConcCollection;
    FViscosity: TMawGwtConcCollection;
    function GetFlowingWellConductance: string;
    function GetFlowingWellElevation: string;
    function GetFlowingWellReductionLength: string;
    function GetHeadLimit: string;
    function GetMaxRate: string;
    function GetMinRate: string;
    function GetPumpElevation: string;
    function GetRate: string;
    function GetScalingLength: string;
    function GetWellHead: string;
    procedure SetFlowingWell(const Value: TFlowingWell);
    procedure SetFlowingWellConductance(const Value: string);
    procedure SetFlowingWellElevation(const Value: string);
    procedure SetFlowingWellReductionLength(const Value: string);
    procedure SetHeadLimit(const Value: string);
    procedure SetMawStatus(const Value: TMawStatus);
    procedure SetMaxRate(const Value: string);
    procedure SetMinRate(const Value: string);
    procedure SetPumpElevation(const Value: string);
    procedure SetRate(const Value: string);
    procedure SetScalingLength(const Value: string);
    procedure SetWellHead(const Value: string);
    procedure SetHeadLimitChoice(const Value: Boolean);
    procedure SetRateLimitation(const Value: TRateLimitation);
    function GetRateScaling: Boolean;
    function GetShutoff: Boolean;
    procedure SetGwtStatus(const Value: TGwtBoundaryStatusCollection);
    procedure SetInjectionConcentrations(const Value: TMawGwtConcCollection);
    procedure SetSpecifiedConcentrations(const Value: TMawGwtConcCollection);
    procedure SetDensity(const Value: TMawGwtConcCollection);
    procedure SetViscosity(const Value: TMawGwtConcCollection);
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
  public
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent); override;
    property Shutoff: Boolean read GetShutoff;
    property RateScaling: Boolean read GetRateScaling;
  published
    // status
    property MawStatus: TMawStatus read FMawStatus write SetMawStatus;
    // rate
    property Rate: string read GetRate write SetRate;
    // well_head
    property WellHead: string read GetWellHead write SetWellHead;
    // FLOWING_WELL
    property FlowingWell: TFlowingWell read FFlowingWell write SetFlowingWell;
    // fwelev
    property FlowingWellElevation: string read GetFlowingWellElevation
      write SetFlowingWellElevation;
    // fwcond
    property FlowingWellConductance: string read GetFlowingWellConductance
      write SetFlowingWellConductance;
    // fwrlen
    property FlowingWellReductionLength: string read GetFlowingWellReductionLength
      write SetFlowingWellReductionLength;

    // ShutOff and RateScaling can not be used simultaneously.
    // RateLimitation chooses between no-limit, ShutOff, and RateScaling.
    // SHUT_OFF, RATE_SCALING
    property RateLimitation: TRateLimitation read FRateLimitation
      write SetRateLimitation;
    // minrate with SHUT_OFF
    property MinRate: string read GetMinRate write SetMinRate;
    // maxrate with SHUT_OFF
    property MaxRate: string read GetMaxRate write SetMaxRate;

    // pump_elevation with RATE_SCALING
    property PumpElevation: string read GetPumpElevation write SetPumpElevation;
    // scaling_length with RATE_SCALING
    property ScalingLength: string read GetScalingLength write SetScalingLength;
    // HEAD_LIMIT
    property HeadLimitChoice: Boolean read FHeadLimitChoice write SetHeadLimitChoice;
    // head_limit
    property HeadLimit: string read GetHeadLimit write SetHeadLimit;
    property GwtStatus: TGwtBoundaryStatusCollection read FGwtStatus write SetGwtStatus;
    property SpecifiedConcentrations: TMawGwtConcCollection read FSpecifiedConcentrations
      write SetSpecifiedConcentrations;
    property InjectionConcentrations: TMawGwtConcCollection read FInjectionConcentrations
      write SetInjectionConcentrations;
    // Buoyancy
    property Density: TMawGwtConcCollection read FDensity write SetDensity;
    // Viscosity
    property Viscosity: TMawGwtConcCollection read FViscosity write SetViscosity;
  end;

  TMawTimeListLink = class(TTimeListsModelLink)
  private
    procedure AddGwtTimeLists(SpeciesIndex: Integer);
    procedure RemoveGwtTimeLists(SpeciesIndex: Integer);
  protected
    FFlowingWellElevation: TModflowTimeList;
    FFlowingWellConductance: TModflowTimeList;
    FFlowingWellReductionLength: TModflowTimeList;
    FRate: TModflowTimeList;
    FWellHead: TModflowTimeList;
    FHeadLimit: TModflowTimeList;
    FMinRate: TModflowTimeList;
    FMaxRate: TModflowTimeList;
    FPumpElevation: TModflowTimeList;
    FScalingLength: TModflowTimeList;
    FDensity: TModflowTimeList;
    FViscosity: TModflowTimeList;
    // GWT
    FGwtStatusList: TModflowTimeLists;
    FSpecifiedConcList: TModflowTimeLists;
    FInjectionConcList: TModflowTimeLists;
    procedure CreateTimeLists; override;
    procedure UpdateGwtTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  TMawWellCollection = class(TCustomMF_ListBoundColl)
  private
    procedure InvalidateFlowingWellElevationData(Sender: TObject);
    procedure InvalidateFlowingWellConductanceData(Sender: TObject);
    procedure InvalidateFlowingWellReductionLengthData(Sender: TObject);
    procedure InvalidateRateData(Sender: TObject);
    procedure InvalidateWellHeadData(Sender: TObject);
    procedure InvalidateHeadLimitData(Sender: TObject);
    procedure InvalidateMinRateData(Sender: TObject);
    procedure InvalidateMaxRateData(Sender: TObject);
    procedure InvalidatePumpElevationData(Sender: TObject);
    procedure InvalidateScalingLengthData(Sender: TObject);
    // GWT
    procedure InvalidateGwtStatus(Sender: TObject);
    procedure InvalidateSpecifiedConcentrations(Sender: TObject);
    procedure InvalidateInjectionConcentrations(Sender: TObject);
    // Buoyancy
    procedure InvalidateDensity(Sender: TObject);
    procedure InvalidateViscosity(Sender: TObject);
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;

    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TWellStorage.WellArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
    procedure InvalidateModel; override;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(CellAssignmentData: TCellAssignmentData); override;
    procedure AssignDirectlySpecifiedValues( AnItem: TCustomModflowBoundaryItem;
      BoundaryStorage: TCustomBoundaryStorage); override;
    function AllowInactiveMf6Cells: boolean; override;
  end;

  TMawBoundary = class;

  TMawCell = class(TValueCell)
  private
    FValues: TMawTransientRecord;
    FStressPeriod: integer;
    function GetFlowingWell: TFlowingWell;
    function GetFlowingWellConductance: double;
    function GetFlowingWellConductanceAnnotation: string;
    function GetFlowingWellElevation: double;
    function GetFlowingWellElevationAnnotation: string;
    function GetHeadLimit: double;
    function GetHeadLimitAnnotation: string;
    function GetMawStatus: TMawStatus;
    function GetMaxRate: double;
    function GetMaxRateAnnotation: string;
    function GetMinRate: double;
    function GetMinRateAnnotation: string;
    function GetPumpElevation: double;
    function GetPumpElevationAnnotation: string;
    function GetRate: double;
    function GetRateAnnotation: string;
    function GetRateScaling: Boolean;
    function GetScalingLength: double;
    function GetScalingLengthAnnotation: string;
    function GetShutOff: Boolean;
    function GetWellHead: double;
    function GetWellHeadAnnotation: string;
    function GetWellNumber: Integer;
    function GetHeadLimitChoice: Boolean;
    function GetMawBoundary: TMawBoundary;
    function GetMvrIndex: Integer;
    function GetMvrUsed: Boolean;
    function GetFlowingWellReductionLength: double;
    function GetFlowingWellReductionLengthAnnotation: string;
    function GetGwtStatus: TGwtBoundaryStatusArray;
    function GetSpecifiedConcentrations: TGwtCellData;
    function GetInjectionConcentrations: TGwtCellData;
    function GetDensity: TGwtCellData;
    function GetViscosity: TGwtCellData;
  protected
    function GetColumn: integer; override;
    function GetLayer: integer; override;
    function GetRow: integer; override;
    function GetSection: integer; override;
    procedure SetColumn(const Value: integer); override;
    procedure SetLayer(const Value: integer); override;
    procedure SetRow(const Value: integer); override;
    function GetIntegerValue(Index: integer; AModel: TBaseModel): integer; override;
    function GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string; override;
    function GetRealValue(Index: integer; AModel: TBaseModel): double; override;
    function GetRealAnnotation(Index: integer; AModel: TBaseModel): string; override;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList); override;
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList); override;
    procedure RecordStrings(Strings: TStringList); override;
    function GetPestName(Index: Integer): string; override;
    function GetPestSeriesMethod(Index: Integer): TPestParamMethod; override;
    function GetPestSeriesName(Index: Integer): string; override;
    function GetMf6TimeSeriesName(Index: Integer): string; override;
    procedure SetMf6TimeSeriesName(Index: Integer; const Value: string); override;
  public
    property WellNumber: Integer read GetWellNumber;
    property MawStatus: TMawStatus read GetMawStatus;
    property FlowingWell: TFlowingWell read GetFlowingWell;
    // ShutOff and RateScaling can not be used simultaneously.
    property ShutOff: Boolean read GetShutOff;
    property RateScaling: Boolean read GetRateScaling;
    property HeadLimitChoice: Boolean read GetHeadLimitChoice;

    property FlowingWellElevation: double read GetFlowingWellElevation;
    property FlowingWellConductance: double read GetFlowingWellConductance;
    property FlowingWellReductionLength: double
      read GetFlowingWellReductionLength;
    property Rate: double read GetRate;
    property WellHead: double read GetWellHead;
    property HeadLimit: double read GetHeadLimit;
    property MinRate: double read GetMinRate;
    property MaxRate: double read GetMaxRate;
    property PumpElevation: double read GetPumpElevation;
    property ScalingLength: double read GetScalingLength;

    property FlowingWellElevationAnnotation: string
      read GetFlowingWellElevationAnnotation;
    property FlowingWellConductanceAnnotation: string
      read GetFlowingWellConductanceAnnotation;
    property FlowingWellReductionLengthAnnotation: string
      read GetFlowingWellReductionLengthAnnotation;
    property RateAnnotation: string read GetRateAnnotation;
    property WellHeadAnnotation: string read GetWellHeadAnnotation;
    property HeadLimitAnnotation: string read GetHeadLimitAnnotation;
    property MinRateAnnotation: string read GetMinRateAnnotation;
    property MaxRateAnnotation: string read GetMaxRateAnnotation;
    property PumpElevationAnnotation: string read GetPumpElevationAnnotation;
    property ScalingLengthAnnotation: string read GetScalingLengthAnnotation;

    property StressPeriod: Integer read FStressPeriod write FStressPeriod;
    property Values: TMawTransientRecord read FValues write FValues;

    property MawBoundary: TMawBoundary read GetMawBoundary;
    property MvrUsed: Boolean read GetMvrUsed;
    property MvrIndex: Integer read GetMvrIndex;
    // GWT
    Property GwtStatus: TGwtBoundaryStatusArray read GetGwtStatus;
    Property SpecifiedConcentrations: TGwtCellData read GetSpecifiedConcentrations;
    Property InjectionConcentrations: TGwtCellData read GetInjectionConcentrations;
    // Buoyancy
    Property Density: TGwtCellData read GetDensity;
    // Viscosity
    Property Viscosity: TGwtCellData read GetViscosity;
  end;

  TMawSteadyWellRecord = record
  private
    function GetBoundaryAnnotation(Index: Integer): string;
    function GetBoundaryValue(Index: Integer): double;
    procedure SetBoundaryAnnotation(Index: Integer; const Value: string);
    procedure SetBoundaryValue(Index: Integer; const Value: double);
    function GetPestParamName(Index: Integer): string;
    procedure SetPestParamName(Index: Integer; const Value: string);
  public
    WellNumber: Integer;
    Radius: Double;
    Bottom: Double;
    StartingHead: double;
    ConductanceMethod: TMawConductanceMethod;
    RadiusAnnotation: string;
    BottomAnnotation: string;
    StartingHeadAnnotation: string;
    CellCount: Integer;
    BoundName: string;
    ScreenObjectName: string;
    RadiusPestName: string;
    BottomPestName: string;
    StartingHeadPestName: string;
    Column: Integer;
    Row: Integer;
    Layer: Integer;
    // GWT
    StartingConcentrations: TGwtCellData;
    procedure Assign(const Item: TMawSteadyWellRecord);
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
    property BoundaryValue[Index: Integer]: double read GetBoundaryValue
      write SetBoundaryValue;
    property BoundaryAnnotation[Index: Integer]: string
      read GetBoundaryAnnotation write SetBoundaryAnnotation;
    property PestParamName[Index: Integer]: string read GetPestParamName
      write SetPestParamName;
  end;

  // CONNECTIONDATA block
  // connection between a well screen and a single cell.
  TMawSteadyConnectionRecord = record
    Cell: TCellLocation;
    WellNumber: Integer;
    ScreenTop: double;
    ScreenBottom: double;
    SkinK: Double;
    SkinRadius: double;
    ConnectionNumber: Integer;
    ScreenTopAnnotation: string;
    ScreenBottomAnnotation: string;
    SkinKAnnotation: string;
    SkinRadiusAnnotation: string;
    ScreenObjectName: string;

    ScreenTopPestName: string;
    ScreenBottomPestName: string;
    SkinKPestName: string;
    SkinRadiusPestName: string;

    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TMawSteadyConnectionArray = array of TMawSteadyConnectionRecord;

  // Well screens
  TMawSteadyConnectionStorage = class(TCustomBoundaryStorage)
  private
    FMawSteadyConnectionArray: TMawSteadyConnectionArray;
    function GetMawSteadyConnectionArray: TMawSteadyConnectionArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream;
      Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property MawSteadyConnectionArray: TMawSteadyConnectionArray
      read GetMawSteadyConnectionArray;
  end;

  // @name represents a well screen.
  // @link(StartTime) and @link(EndTime) are not used.
  TMawWellScreenItem = class(TCustomModflowBoundaryItem)
  private
    const
      ScreenBottomPosition = 0;
      ScreenTopPosition = 1;
      SkinKPosition = 2;
      SkinRadiusPosition = 3;
    var
    FScreenBottom: IFormulaObject;
    FScreenTop: IFormulaObject;
    FSkinK: IFormulaObject;
    FSkinRadius: IFormulaObject;
    function GetScreenBottom: string;
    function GetScreenTop: string;
    function GetSkinK: string;
    function GetSkinRadius: string;
    procedure SetScreenBottom(const Value: string);
    procedure SetScreenTop(const Value: string);
    procedure SetSkinK(const Value: string);
    procedure SetSkinRadius(const Value: string);
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
  public
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
    procedure Loaded;
  published
    // scrn_top
    property ScreenTop: string read GetScreenTop write SetScreenTop;
    // scrn_bot
    property ScreenBottom: string read GetScreenBottom write SetScreenBottom;
    // hk_skin
    property SkinK: string read GetSkinK write SetSkinK;
    // radius_skin
    property SkinRadius: string read GetSkinRadius write SetSkinRadius;
  end;

  TMawWellScreenTimeListLink = class(TTimeListsModelLink)
  private
    FScreenTopData: TModflowTimeList;
    FScreenBottomData: TModflowTimeList;
    FSkinKData: TModflowTimeList;
    FSkinRadiusData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  TMawWellScreenCollection = class(TCustomMF_ListBoundColl)
  private
    procedure InvalidateScreenTopData(Sender: TObject);
    procedure InvalidateScreenBottomData(Sender: TObject);
    procedure InvalidateSkinKData(Sender: TObject);
    procedure InvalidateSkinRadiusData(Sender: TObject);
    function GetItems(Index: Integer): TMawWellScreenItem;
    procedure SetItems(Index: Integer; const Value: TMawWellScreenItem);
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;

    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TWellStorage.WellArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
    procedure InvalidateModel; override;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(CellAssignmentData: TCellAssignmentData); override;
    procedure AssignArrayCellValues(DataSets: TList; ItemIndex: Integer;
      AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
      PestItemNames, TimeSeriesNames: TStringListObjectList); override;
    function ShouldDeleteItemsWithZeroDuration: Boolean; override;
  public
    procedure Loaded;
    property Items[Index: Integer]: TMawWellScreenItem read GetItems write SetItems; default;
  end;

  TMawBoundary = class(TModflowBoundary)
  private
    const
      RadiusPosition = 10;
      BottomPosition = 11;
      InitialHeadPosition = 12;
    var
    FPestFlowingWellElevationMethod: TPestParamMethod;
    FPestPumpElevationMethod: TPestParamMethod;
    FPestRateMethod: TPestParamMethod;
    FPestMaxRateMethod: TPestParamMethod;
    FPestHeadLimitMethod: TPestParamMethod;
    FPestFlowingWellReductionLengthMethod: TPestParamMethod;
    FPestMinRateMethod: TPestParamMethod;
    FPestFlowingWellConductanceMethod: TPestParamMethod;
    FPestWellHeadMethod: TPestParamMethod;
    FPestScalingLengthMethod: TPestParamMethod;
    FUsedObserver: TObserver;
    FPestFlowingWellElevationFormula: IFormulaObject;
    FPestFlowingWellConductanceFormula: IFormulaObject;
    FPestRateFormula: IFormulaObject;
    FPestWellHeadFormula: IFormulaObject;
    FPestHeadLimitFormula: IFormulaObject;
    FPestMinRateFormula: IFormulaObject;
    FPestMaxRateFormula: IFormulaObject;
    FPestPumpElevationFormula: IFormulaObject;
    FPestScalingLengthFormula: IFormulaObject;
    FPestFlowingWellReductionLengthFormula: IFormulaObject;
    FWellNumber: Integer;
    FBottomObserver: TObserver;
    FInitialHeadObserver: TObserver;
    FRadiusObserver: TObserver;
    FConductanceMethod: TMawConductanceMethod;
    FRadius: IFormulaObject;
    FBottom: IFormulaObject;
    FInitialHead: IFormulaObject;
    FWellScreens: TMawWellScreenCollection;
    FPestFlowingWellConductanceObserver: TObserver;
    FPestFlowingWellElevationObserver: TObserver;
    FPestFlowingWellReductionLengthObserver: TObserver;
    FPestHeadLimitObserver: TObserver;
    FPestMaxRateObserver: TObserver;
    FPestMinRateObserver: TObserver;
    FPestPumpElevationObserver: TObserver;
    FPestRateObserver: TObserver;
    FPestScalingLengthObserver: TObserver;
    FPestWellHeadObserver: TObserver;
    FStartingConcentrations: TStringConcCollection;
    FPestInjectionConcentrationObservers: TObserverList;
    FPestSpecifiedConcentrationObservers: TObserverList;
    FPestInjectionConcentrations: TMawGwtConcCollection;
    FPestSpecifiedConcentrations: TMawGwtConcCollection;
    FPestInjectionConcentrationMethods: TGwtPestMethodCollection;
    FPestSpecifiedConcentrationMethods: TGwtPestMethodCollection;
    FPestDensity: TMawGwtConcCollection;
    FPestDensityMethods: TGwtPestMethodCollection;
    FPestDensityObservers: TObserverList;
    FPestViscosity: TMawGwtConcCollection;
    FPestViscosityMethods: TGwtPestMethodCollection;
    FPestViscosityObservers: TObserverList;
    function GetStartingConcentrations: TStringConcCollection;
    procedure SetPestDensity(const Value: TMawGwtConcCollection);
    procedure SetPestDensityMethods(const Value: TGwtPestMethodCollection);
    function GetPestDensityObserver(const Index: Integer): TObserver;
    procedure SetPestViscosity(const Value: TMawGwtConcCollection);
    procedure SetPestViscosityMethods(const Value: TGwtPestMethodCollection);
    function GetPestViscosityObserver(const Index: Integer): TObserver;
    procedure SetWellNumber(const Value: Integer);
    function GetBottom: string;
    function GetInitialHead: string;
    function GetRadius: string;
    procedure SetBottom(const Value: string);
    procedure SetConductanceMethod(Value: TMawConductanceMethod);
    procedure SetInitialHead(const Value: string);
    procedure SetRadius(const Value: string);
    procedure CreateFormulaObjects;
    procedure RemoveFormulaObjects;
    function GetBottomObserver: TObserver;
    function GetInitialHeadObserver: TObserver;
    function GetRadiusObserver: TObserver;
    procedure SetWellScreens(const Value: TMawWellScreenCollection);
    procedure InvalidateDisplayTimeLists;
    procedure LinkRadius;
    procedure LinkBottom;
    procedure LinkInitialHead;
    procedure CreateObservers;
    procedure InvalidateFlowingWellElevationData(Sender: TObject);
    procedure InvalidateFlowingWellConductanceData(Sender: TObject);
    procedure InvalidateRateData(Sender: TObject);
    procedure InvalidateWellHeadData(Sender: TObject);
    procedure InvalidateHeadLimitData(Sender: TObject);
    procedure InvalidateMinRateData(Sender: TObject);
    procedure InvalidateMaxRateData(Sender: TObject);
    procedure InvalidatePumpElevationData(Sender: TObject);
    procedure InvalidateScalingLengthData(Sender: TObject);
    procedure InvalidateFlowingWellReductionLengthData(Sender: TObject);
    function GetPestFlowingWellConductanceFormula: string;
    function GetPestFlowingWellElevationFormula: string;
    function GetPestFlowingWellReductionLengthFormula: string;
    function GetPestHeadLimitFormula: string;
    function GetPestMaxRateFormula: string;
    function GetPestMinRateFormula: string;
    function GetPestPumpElevationFormula: string;
    function GetPestRateFormula: string;
    function GetPestScalingLengthFormula: string;
    function GetPestWellHeadFormula: string;
    procedure SetPestFlowingWellConductanceFormula(const Value: string);
    procedure SetPestFlowingWellConductanceMethod(
      const Value: TPestParamMethod);
    procedure SetPestFlowingWellElevationFormula(const Value: string);
    procedure SetPestFlowingWellElevationMethod(const Value: TPestParamMethod);
    procedure SetPestFlowingWellReductionLengthFormula(const Value: string);
    procedure SetPestFlowingWellReductionLengthMethod(
      const Value: TPestParamMethod);
    procedure SetPestHeadLimitFormula(const Value: string);
    procedure SetPestHeadLimitMethod(const Value: TPestParamMethod);
    procedure SetPestMaxRateFormula(const Value: string);
    procedure SetPestMaxRateMethod(const Value: TPestParamMethod);
    procedure SetPestMinRateFormula(const Value: string);
    procedure SetPestMinRateMethod(const Value: TPestParamMethod);
    procedure SetPestPumpElevationFormula(const Value: string);
    procedure SetPestPumpElevationMethod(const Value: TPestParamMethod);
    procedure SetPestRateFormula(const Value: string);
    procedure SetPestRateMethod(const Value: TPestParamMethod);
    procedure SetPestScalingLengthFormula(const Value: string);
    procedure SetPestScalingLengthMethod(const Value: TPestParamMethod);
    procedure SetPestWellHeadFormula(const Value: string);
    procedure SetPestWellHeadMethod(const Value: TPestParamMethod);
    function GetPestFlowingWellConductanceObserver: TObserver;
    function GetPestFlowingWellElevationObserver: TObserver;
    function GetPestFlowingWellReductionLengthObserver: TObserver;
    function GetPestHeadLimitObserver: TObserver;
    function GetPestMaxRateObserver: TObserver;
    function GetPestMinRateObserver: TObserver;
    function GetPestPumpElevationObserver: TObserver;
    function GetPestRateObserver: TObserver;
    function GetPestScalingLengthObserver: TObserver;
    function GetPestWellHeadObserver: TObserver;
    procedure SetStartingConcentrations(const Value: TStringConcCollection);
    procedure SetPestInjectionConcentrations(
      const Value: TMawGwtConcCollection);
    procedure SetPestSpecifiedConcentrations(
      const Value: TMawGwtConcCollection);
    procedure SetPestInjectionConcentrationMethods(
      const Value: TGwtPestMethodCollection);
    procedure SetPestSpecifiedConcentrationMethods(
      const Value: TGwtPestMethodCollection);
    function GetPestInjectionConcentrationObserver(const Index: Integer): TObserver;
    function GetPestSpecifiedConcentrationObserver(const Index: Integer): TObserver;
    procedure InvalidatePestSpecConcData(Sender: TObject);
    procedure InvalidatePestInjConcData(Sender: TObject);
    procedure InvalidatePestDensityData(Sender: TObject);
    procedure InvalidatePestViscosityData(Sender: TObject);
  protected
    property RadiusObserver: TObserver read GetRadiusObserver;
    property BottomObserver: TObserver read GetBottomObserver;
    property InitialHeadObserver: TObserver read GetInitialHeadObserver;
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    class function BoundaryCollectionClass: TMF_BoundCollClass;
      override;
    function BoundaryObserverPrefix: string; override;

    procedure HandleChangedValue(Observer: TObserver); //override;
    function GetUsedObserver: TObserver; //override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    function GetPestBoundaryFormula(FormulaIndex: integer): string; override;
    procedure SetPestBoundaryFormula(FormulaIndex: integer;
      const Value: string); override;
    function GetPestBoundaryMethod(FormulaIndex: integer): TPestParamMethod; override;
    procedure SetPestBoundaryMethod(FormulaIndex: integer;
      const Value: TPestParamMethod); override;
    property PestFlowingWellElevationObserver: TObserver
      read GetPestFlowingWellElevationObserver;
    property PestFlowingWellConductanceObserver: TObserver
      read GetPestFlowingWellConductanceObserver;
    property PestRateObserver: TObserver read GetPestRateObserver;
    property PestWellHeadObserver: TObserver read GetPestWellHeadObserver;
    property PestHeadLimitObserver: TObserver read GetPestHeadLimitObserver;
    property PestMinRateObserver: TObserver read GetPestMinRateObserver;
    property PestMaxRateObserver: TObserver read GetPestMaxRateObserver;
    property PestPumpElevationObserver: TObserver
      read GetPestPumpElevationObserver;
    property PestScalingLengthObserver: TObserver
      read GetPestScalingLengthObserver;
    property PestFlowingWellReductionLengthObserver: TObserver
      read GetPestFlowingWellReductionLengthObserver;
    property PestSpecifiedConcentrationObserver[const Index: Integer]: TObserver
      read GetPestSpecifiedConcentrationObserver;
    property PestInjectionConcentrationObserver[const Index: Integer]: TObserver
      read GetPestInjectionConcentrationObserver;
    property PestDensityObserver[const Index: Integer]: TObserver
      read GetPestDensityObserver;
    property PestViscosityObserver[const Index: Integer]: TObserver
      read GetPestViscosityObserver;
  public
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    Destructor Destroy; override;
    Procedure Assign(Source: TPersistent); override;
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    // The well number is assigned in the export process.
    property WellNumber: Integer read FWellNumber write SetWellNumber;
    procedure InitializeVariables;
    procedure Loaded;
    procedure InvalidateDisplay; override;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
  published
    // radius
    property Radius: string read GetRadius write SetRadius;
    // bottom
    property Bottom: string read GetBottom write SetBottom;
    // strt
    property InitialHead: string read GetInitialHead write SetInitialHead;
    // condeqn
    property ConductanceMethod: TMawConductanceMethod read FConductanceMethod
      write SetConductanceMethod;
    property WellScreens: TMawWellScreenCollection read FWellScreens
      write SetWellScreens;

    property PestFlowingWellElevationFormula: string
      read GetPestFlowingWellElevationFormula
      write SetPestFlowingWellElevationFormula;
    property PestFlowingWellElevationMethod: TPestParamMethod
      read FPestFlowingWellElevationMethod
      write SetPestFlowingWellElevationMethod;

    property PestFlowingWellConductanceFormula: string
      read GetPestFlowingWellConductanceFormula
      write SetPestFlowingWellConductanceFormula;
    property PestFlowingWellConductanceMethod: TPestParamMethod
      read FPestFlowingWellConductanceMethod
      write SetPestFlowingWellConductanceMethod;

    property PestRateFormula: string read GetPestRateFormula
      write SetPestRateFormula;
    property PestRateMethod: TPestParamMethod read FPestRateMethod
      write SetPestRateMethod;

    property PestWellHeadFormula: string read GetPestWellHeadFormula
      write SetPestWellHeadFormula;
    property PestWellHeadMethod: TPestParamMethod read FPestWellHeadMethod
      write SetPestWellHeadMethod;

    property PestHeadLimitFormula: string read GetPestHeadLimitFormula
      write SetPestHeadLimitFormula ;
    property PestHeadLimitMethod: TPestParamMethod read FPestHeadLimitMethod
      write SetPestHeadLimitMethod;

    property PestMinRateFormula: string read GetPestMinRateFormula
      write SetPestMinRateFormula;
    property PestMinRateMethod: TPestParamMethod read FPestMinRateMethod
      write SetPestMinRateMethod;

    property PestMaxRateFormula: string read GetPestMaxRateFormula
      write SetPestMaxRateFormula;
    property PestMaxRateMethod: TPestParamMethod read FPestMaxRateMethod
      write SetPestMaxRateMethod;

    property PestPumpElevationFormula: string read GetPestPumpElevationFormula
      write SetPestPumpElevationFormula;
    property PestPumpElevationMethod: TPestParamMethod
      read FPestPumpElevationMethod
      write SetPestPumpElevationMethod;

    property PestScalingLengthFormula: string read GetPestScalingLengthFormula
      write SetPestScalingLengthFormula;
    property PestScalingLengthMethod: TPestParamMethod
      read FPestScalingLengthMethod
      write SetPestScalingLengthMethod;

    property PestFlowingWellReductionLengthFormula: string
      read GetPestFlowingWellReductionLengthFormula
      write SetPestFlowingWellReductionLengthFormula;
    property PestFlowingWellReductionLengthMethod: TPestParamMethod
      read FPestFlowingWellReductionLengthMethod
      write SetPestFlowingWellReductionLengthMethod;
    // GWT
    property StartingConcentrations: TStringConcCollection
      read GetStartingConcentrations
      write SetStartingConcentrations;
      property PestInjectionConcentrations: TMawGwtConcCollection
        read FPestInjectionConcentrations write SetPestInjectionConcentrations;
      property PestSpecifiedConcentrations: TMawGwtConcCollection
        read FPestSpecifiedConcentrations write SetPestSpecifiedConcentrations;
      property PestInjectionConcentrationMethods: TGwtPestMethodCollection
        read FPestInjectionConcentrationMethods write SetPestInjectionConcentrationMethods;
      property PestSpecifiedConcentrationMethods: TGwtPestMethodCollection
        read FPestSpecifiedConcentrationMethods write SetPestSpecifiedConcentrationMethods;
        // Buoyancy
      property PestDensity: TMawGwtConcCollection
        read FPestDensity write SetPestDensity;
      property PestDensityMethods: TGwtPestMethodCollection
        read FPestDensityMethods write SetPestDensityMethods;
        // Viscosity
      property PestViscosity: TMawGwtConcCollection
        read FPestViscosity write SetPestViscosity;
      property PestViscosityMethods: TGwtPestMethodCollection
        read FPestViscosityMethods write SetPestViscosityMethods;
    end;

const
  MawFlowingWellElevationPosition = 0;
  MawFlowingWellConductancePosition = 1;
  MawRatePosition = 2;
  MawWellHeadPosition = 3;
  MawHeadLimitPosition = 4;
  MawMinRatePosition = 5;
  MawMaxRatePosition = 6;
  MawPumpElevationPosition = 7;
  MawScalingLengthPosition = 8;
  MawFlowingWellReductionLengthPosition = 9;
  MawDensityPosition = 10;
  MawViscosityPosition = 11;
  MawGwtStart = 12;

  MawRadiusPosition = 0;
  MawBottomPosition = 1;
  MawStartingHeadPosition = 2;
  MawGwtSteadyStart = 3;

Const
  MawGwtConcCount = 2;
  MawGwtSpecifiedConcentrationPosition = 0;
  MawGwtInjectionConcentrationsPosition = 1;

function TryGetMawOb(const MawObName: string; var MawOb: TMawOb): Boolean;
function TryGetMwtOb(const MwtObName: string; var MwtOb: TMwtOb): Boolean;
function MawObToString(const MawOb: TMawOb): string;
function MwtObToString(const MwtOb: TMwtOb): string;
Procedure FillMawSeriesNames(AList: TStrings);
Procedure FillMwtSeriesNames(AList: TStrings);

implementation

uses
  frmGoPhastUnit, PhastModelUnit, ScreenObjectUnit,
  GIS_Functions, ModflowTimeUnit, DataSetUnit, ModflowMnw2Unit,
  ModflowMvrUnit, DataSetNamesUnit, CellLocationUnit;

const MawObName: array[TMawOb] of string = ('Head', 'FromMvr', 'FlowRate',
  'FlowRateCells', 'PumpRate', 'RateToMvr',
  'FlowingWellFlowRate', 'FlowWellToMvr', 'StorageFlowRate',
  'ConstantFlowRate', 'Conductance',
  'ConductanceCells', 'FlowingWellConductance');

  {
  TMwtOb = (mtoConcentration, mstoStorage, mtoConstant, mtoFromMvr, mtoMwt,
    mtoMwtCells, mtoRate, mtoFwRate, mtoRateToMvr, mtoFwRateToMvr);
  }
const MwtObName: array[TMwtOb] of string = (
  'Concentration',
  'Storage',
  'Constant',
  'From-MVR',
  'Mass Flow Rate (MWT)',
  'Mass Flow Rate (Cells) (MWT + iconn)',
  'Well Mass-Flow-Rate (rate)',
  'Flowing Well Mass-Flow-Rate (fw-rate)',
  'Mass Flow Rate to MVR (rate-to-mvr)',
  'Flowing Well Mass Flow Rate to MVR (fw-rate-to-mvr)');

var
  MawObNames: TStringList;
  MwtObNames: TStringList;

procedure InitializeMawObNames;
var
  Index: TMawOb;
begin
  MawObNames := TStringList.Create;
  MawObNames.CaseSensitive := False;
  for Index := Low(TMawOb) to High(TMawOb) do
  begin
    MawObNames.Add(MawObName[Index]);
  end;
end;

procedure InitializeMwtObNames;
var
  Index: TMwtOb;
begin
  MwtObNames := TStringList.Create;
  MwtObNames.CaseSensitive := False;
  for Index := Low(TMwtOb) to High(TMwtOb) do
  begin
    MwtObNames.Add(MwtObName[Index]);
  end;
end;


function TryGetMawOb(const MawObName: string; var MawOb: TMawOb): Boolean;
var
  Index: Integer;
begin
  Index := MawObNames.IndexOf(MawObName);
  result := Index >= 0;
  if result then
  begin
    MawOb := TMawOb(Index);
  end;
end;

function TryGetMwtOb(const MwtObName: string; var MwtOb: TMwtOb): Boolean;
var
  Index: Integer;
begin
  Index := MwtObNames.IndexOf(MwtObName);
  result := Index >= 0;
  if result then
  begin
    MwtOb := TMwtOb(Index);
  end;
end;

function MawObToString(const MawOb: TMawOb): string;
begin
  result := MawObName[MawOb];
end;

function MwtObToString(const MwtOb: TMwtOb): string;
begin
  result := MwtObName[MwtOb];
end;

Procedure FillMawSeriesNames(AList: TStrings);
begin
  AList.Assign(MawObNames);
end;

Procedure FillMwtSeriesNames(AList: TStrings);
begin
  AList.Assign(MwtObNames);
end;

resourcestring
  StrScreenTop = 'Screen_Top';
  StrScreenBottom = 'Screen_Bottom';
  StrSkinK = 'Skin_K';
  StrSkinRadius = 'Skin_Radius';
  StrFlowingWellElevati = 'Flowing_Well_Elevation';
  StrFlowingWellConduct = 'Flowing_Well_Conductance';
  StrFlowingWellRedLenth = 'Flowing_Well_Reduction_Length';
  StrMultiaquiferWellRa = 'Multiaquifer_Well_Rate';
  StrMultiaquiferWellHe = 'Multiaquifer_Well_Head';
  StrHeadLimit = 'Head_Limit';
  StrMinimumPumpingRate = 'Minimum_Pumping_Rate';
  StrMaximumPumpingRate = 'Maximum_Pumping_Rate';
  StrPumpElevation = 'Pump_Elevation';
  StrScalingLength = 'Scaling_Length';
  StrMawFluidDensity = 'Maw_Fluid_Density';
  StrMawFluidViscosity = 'Maw_Fluid_Viscosity';

{ TMawSteadyConnectionRecord }

procedure TMawSteadyConnectionRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompInt(Comp, WellNumber);
  WriteCompReal(Comp, ScreenTop);
  WriteCompReal(Comp, ScreenBottom);
  WriteCompReal(Comp, SkinK);
  WriteCompReal(Comp, SkinRadius);
  WriteCompInt(Comp, ConnectionNumber);

  WriteCompInt(Comp, Strings.IndexOf(ScreenTopAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(ScreenBottomAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(SkinKAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(SkinRadiusAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(ScreenObjectName));

  WriteCompInt(Comp, Strings.IndexOf(ScreenTopPestName));
  WriteCompInt(Comp, Strings.IndexOf(ScreenBottomPestName));
  WriteCompInt(Comp, Strings.IndexOf(SkinKPestName));
  WriteCompInt(Comp, Strings.IndexOf(SkinRadiusPestName));
end;

procedure TMawSteadyConnectionRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(ScreenTopAnnotation);
  Strings.Add(ScreenBottomAnnotation);
  Strings.Add(SkinKAnnotation);
  Strings.Add(SkinRadiusAnnotation);
  Strings.Add(ScreenObjectName);

  Strings.Add(ScreenTopPestName);
  Strings.Add(ScreenBottomPestName);
  Strings.Add(SkinKPestName);
  Strings.Add(SkinRadiusPestName);
end;

procedure TMawSteadyConnectionRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  WellNumber := ReadCompInt(Decomp);
  ScreenTop := ReadCompReal(Decomp);
  ScreenBottom := ReadCompReal(Decomp);
  SkinK := ReadCompReal(Decomp);
  SkinRadius := ReadCompReal(Decomp);
  ConnectionNumber := ReadCompInt(Decomp);
  ScreenTopAnnotation := Annotations[ReadCompInt(Decomp)];
  ScreenBottomAnnotation := Annotations[ReadCompInt(Decomp)];
  SkinKAnnotation := Annotations[ReadCompInt(Decomp)];
  SkinRadiusAnnotation := Annotations[ReadCompInt(Decomp)];
  ScreenObjectName := Annotations[ReadCompInt(Decomp)];

  ScreenTopPestName := Annotations[ReadCompInt(Decomp)];
  ScreenBottomPestName := Annotations[ReadCompInt(Decomp)];
  SkinKPestName := Annotations[ReadCompInt(Decomp)];
  SkinRadiusPestName := Annotations[ReadCompInt(Decomp)];
end;

{ TMawStorage }

procedure TMawSteadyConnectionStorage.Clear;
begin
  SetLength(FMawSteadyConnectionArray, 0);
  FCleared := True;
end;

function TMawSteadyConnectionStorage.GetMawSteadyConnectionArray: TMawSteadyConnectionArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FMawSteadyConnectionArray;
end;

procedure TMawSteadyConnectionStorage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FMawSteadyConnectionArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FMawSteadyConnectionArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TMawSteadyConnectionStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    InitializeStrings(Strings);
    Count := Length(FMawSteadyConnectionArray);
    for Index := 0 to Count - 1 do
    begin
      FMawSteadyConnectionArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FMawSteadyConnectionArray[Index].Cache(Compressor, Strings);
    end;
  finally
    Strings.Free;
  end;
end;

{ TMawWellScreenItem }

procedure TMawWellScreenItem.Assign(Source: TPersistent);
var
  MawItem: TMawWellScreenItem;
  Index: integer;
begin
  // if Assign is updated, update IsSame too.
  if Source is TMawWellScreenItem then
  begin
    MawItem := TMawWellScreenItem(Source);
    for Index := 0 to BoundaryFormulaCount - 1 do
    begin
      BoundaryFormula[Index] := MawItem.BoundaryFormula[Index];
    end;
  end;
  inherited;
end;

procedure TMawWellScreenItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TMawWellScreenCollection;
  ScreenBottomObserver: TObserver;
  ScreenTopObserver: TObserver;
  SkinKObserver: TObserver;
  SkinRadiusObserver: TObserver;
begin
  ParentCollection := Collection as TMawWellScreenCollection;

  ScreenBottomObserver := FObserverList[ScreenBottomPosition];
  ScreenBottomObserver.OnUpToDateSet := ParentCollection.InvalidateScreenBottomData;

  ScreenTopObserver := FObserverList[ScreenTopPosition];
  ScreenTopObserver.OnUpToDateSet := ParentCollection.InvalidateScreenTopData;

  SkinKObserver := FObserverList[SkinKPosition];
  SkinKObserver.OnUpToDateSet := ParentCollection.InvalidateSkinKData;

  SkinRadiusObserver := FObserverList[SkinRadiusPosition];
  SkinRadiusObserver.OnUpToDateSet := ParentCollection.InvalidateSkinRadiusData;
end;

function TMawWellScreenItem.BoundaryFormulaCount: integer;
begin
  result := 4;
end;

procedure TMawWellScreenItem.CreateFormulaObjects;
begin
  FScreenBottom := CreateFormulaObject(dso3D);
  FScreenTop := CreateFormulaObject(dso3D);
  FSkinK := CreateFormulaObject(dso3D);
  FSkinRadius := CreateFormulaObject(dso3D);
end;

destructor TMawWellScreenItem.Destroy;
var
  Index: integer;
begin
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    BoundaryFormula[Index] := '0';
  end;
  inherited;
end;

function TMawWellScreenItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    ScreenBottomPosition:
      result := ScreenBottom;
    ScreenTopPosition:
      result := ScreenTop;
    SkinKPosition:
      result := SkinK;
    SkinRadiusPosition:
      result := SkinRadius;
    else Assert(False);
  end;
end;

procedure TMawWellScreenItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FScreenBottom as TObject then
  begin
    List.Add(FObserverList[ScreenBottomPosition]);
  end;
  if Sender = FScreenTop as TObject then
  begin
    List.Add(FObserverList[ScreenTopPosition]);
  end;
  if Sender = FSkinK as TObject then
  begin
    List.Add(FObserverList[SkinKPosition]);
  end;
  if Sender = FSkinRadius as TObject then
  begin
    List.Add(FObserverList[SkinRadiusPosition]);
  end;
end;

function TMawWellScreenItem.GetScreenBottom: string;
begin
  Result := FScreenBottom.Formula;
  ResetItemObserver(ScreenBottomPosition);
end;

function TMawWellScreenItem.GetScreenTop: string;
begin
  Result := FScreenTop.Formula;
  ResetItemObserver(ScreenTopPosition);
end;

function TMawWellScreenItem.GetSkinK: string;
begin
  Result := FSkinK.Formula;
  ResetItemObserver(SkinKPosition);
end;

function TMawWellScreenItem.GetSkinRadius: string;
begin
  Result := FSkinRadius.Formula;
  ResetItemObserver(SkinRadiusPosition);
end;

procedure TMawWellScreenItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
//    PhastModel.InvalidateMfWellPumpage(self);
  end;
end;

function TMawWellScreenItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TMawWellScreenItem;
  Index: integer;
begin
  result := (AnotherItem is TMawWellScreenItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TMawWellScreenItem(AnotherItem);
    for Index := 0 to BoundaryFormulaCount - 1 do
    begin
      result := BoundaryFormula[Index] = Item.BoundaryFormula[Index];
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TMawWellScreenItem.Loaded;
var
  ScreenBottomObserver: TObserver;
  ScreenTopObserver: TObserver;
  SkinKObserver: TObserver;
  SkinRadiusObserver: TObserver;
  ParentCollection: TMawWellScreenCollection;
  ScreenObject: TScreenObject;
  ScreenBottomDataArray: TDataArray;
  ScreenTopDataArray: TDataArray;
  SkinKDataArray: TDataArray;
  SkinRadiusDataArray: TDataArray;
begin
  ParentCollection := Collection as TMawWellScreenCollection;
  ScreenObject := ParentCollection.ScreenObject as TScreenObject;

  ScreenBottomObserver := FObserverList[ScreenBottomPosition];
  ScreenObject.TalksTo(ScreenBottomObserver);
  ScreenBottomObserver.OnUpToDateSet := ParentCollection.InvalidateScreenBottomData;
  ScreenBottomDataArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(KMAWScreenBottom);
  if ScreenBottomDataArray <> nil then
  begin
    ScreenBottomObserver.TalksTo(ScreenBottomDataArray);
  end;

  ScreenTopObserver := FObserverList[ScreenTopPosition];
  ScreenObject.TalksTo(ScreenTopObserver);
  ScreenTopObserver.OnUpToDateSet := ParentCollection.InvalidateScreenTopData;
  ScreenTopDataArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(KMAWScreenTop);
  if ScreenTopDataArray <> nil then
  begin
    ScreenTopObserver.TalksTo(ScreenTopDataArray);
  end;

  SkinKObserver := FObserverList[SkinKPosition];
  ScreenObject.TalksTo(SkinKObserver);
  SkinKObserver.OnUpToDateSet := ParentCollection.InvalidateSkinKData;
  SkinKDataArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(KMAWSkinK);
  if SkinKDataArray <> nil then
  begin
    SkinKObserver.TalksTo(SkinKDataArray);
  end;

  SkinRadiusObserver := FObserverList[SkinRadiusPosition];
  ScreenObject.TalksTo(SkinRadiusObserver);
  SkinRadiusObserver.OnUpToDateSet := ParentCollection.InvalidateSkinRadiusData;
  SkinRadiusDataArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(KMAWSkinRadius);
  if SkinRadiusDataArray <> nil then
  begin
    SkinRadiusObserver.TalksTo(SkinRadiusDataArray);
  end;

end;

procedure TMawWellScreenItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FScreenBottom,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FScreenTop,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FSkinK,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FSkinRadius,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TMawWellScreenItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    ScreenBottomPosition:
      ScreenBottom := Value;
    ScreenTopPosition:
      ScreenTop := Value;
    SkinKPosition:
      SkinK := Value;
    SkinRadiusPosition:
      SkinRadius := Value;
    else Assert(False);
  end;
end;

procedure TMawWellScreenItem.SetScreenBottom(const Value: string);
begin
  UpdateFormulaBlocks(Value, ScreenBottomPosition, FScreenBottom);
end;

procedure TMawWellScreenItem.SetScreenTop(const Value: string);
begin
  UpdateFormulaBlocks(Value, ScreenTopPosition, FScreenTop);
end;

procedure TMawWellScreenItem.SetSkinK(const Value: string);
begin
  UpdateFormulaBlocks(Value, SkinKPosition, FSkinK);
end;

procedure TMawWellScreenItem.SetSkinRadius(const Value: string);
begin
  UpdateFormulaBlocks(Value, SkinRadiusPosition, FSkinRadius);
end;

{ TMawWellScreenCollection }

procedure TMawWellScreenCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TMawSteadyConnectionStorage.Create(AModel));
end;

function TMawWellScreenCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Item: TMawWellScreenItem;
begin
  Item := Items[ItemIndex] as TMawWellScreenItem;
  result := Item.BoundaryFormula[FormulaIndex];
end;

procedure TMawWellScreenCollection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel; PestSeries: TStringList;
  PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList);
begin
  // does anything need to be done here?
  // called from TCustomListArrayBoundColl.AssignArrayCellsWithItem
  // which is called by TCustomListArrayBoundColl.EvaluateArrayBoundaries
//  inherited;

end;

procedure TMawWellScreenCollection.AssignCellList(CellAssignmentData: TCellAssignmentData);
var
  MawStorage: TMawSteadyConnectionStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
  Expression: TExpression;
  ACellList: TObject;
  BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer;
  Variables, DataSets: TList;
  AModel: TBaseModel;
  AScreenObject: TObject;
  PestName: string;
  PestSeriesName: string;
  TimeSeriesName: string;
  DynamicTimeSeries: IDynamicTimeSeries;
begin
  Expression := CellAssignmentData.Expression;
  ACellList := CellAssignmentData.ACellList;
  BoundaryStorage := CellAssignmentData.BoundaryStorage;
  BoundaryFunctionIndex := CellAssignmentData.BoundaryFunctionIndex;
  Variables := CellAssignmentData.Variables;
  DataSets := CellAssignmentData.DataSets;
  AModel := CellAssignmentData.AModel;
  AScreenObject := CellAssignmentData.AScreenObject;
  PestName := CellAssignmentData.PestName;
  PestSeriesName := CellAssignmentData.PestSeriesName;
//  PestSeriesMethod := CellAssignmentData.PestSeriesMethod;
  TimeSeriesName := CellAssignmentData.TimeSeriesName;
  DynamicTimeSeries := CellAssignmentData.DynamicTimeSeries;

  Assert(BoundaryFunctionIndex in
    [TMawWellScreenItem.ScreenBottomPosition .. TMawWellScreenItem.SkinRadiusPosition]);
  Assert(Expression <> nil);

  MawStorage := BoundaryStorage as TMawSteadyConnectionStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);

    Expression.Evaluate;
    with MawStorage.MawSteadyConnectionArray[Index] do
    begin
      case BoundaryFunctionIndex of
        TMawWellScreenItem.ScreenBottomPosition:
          begin
            ScreenBottom := Expression.DoubleResult;
            ScreenBottomAnnotation := ACell.Annotation;
            ScreenBottomPestName := PestName;
          end;
        TMawWellScreenItem.ScreenTopPosition:
          begin
            ScreenTop := Expression.DoubleResult;
            ScreenTopAnnotation := ACell.Annotation;
            ScreenTopPestName := PestName;
          end;
        TMawWellScreenItem.SkinKPosition:
          begin
            SkinK := Expression.DoubleResult;
            SkinKAnnotation := ACell.Annotation;
            SkinKPestName := PestName;
          end;
        TMawWellScreenItem.SkinRadiusPosition:
          begin
            SkinRadius := Expression.DoubleResult;
            SkinRadiusAnnotation := ACell.Annotation;
            SkinRadiusPestName := PestName;
          end;
        else Assert(False);
      end;
    end;
  end;
end;

procedure TMawWellScreenCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  MawStorage: TMawSteadyConnectionStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  MawStorage := BoundaryStorage as TMawSteadyConnectionStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
//    if ACell.LgrEdge then
//    begin
//      Continue;
//    end;
    with MawStorage.MawSteadyConnectionArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

function TMawWellScreenCollection.GetItems(Index: Integer): TMawWellScreenItem;
begin
  result := inherited Items[Index] as TMawWellScreenItem;
end;

class function TMawWellScreenCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TMawWellScreenTimeListLink;
end;

procedure TMawWellScreenCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
//    PhastModel.InvalidateMfWellPumpage(self);
  end;
end;

procedure TMawWellScreenCollection.InvalidateScreenBottomData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMawWellScreenTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ScreenBottomDataArray: TDataArray;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TMawWellScreenTimeListLink;
    Link.FScreenBottomData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TMawWellScreenTimeListLink;
        Link.FScreenBottomData.Invalidate;
      end;
    end;

    ScreenBottomDataArray := PhastModel.DataArrayManager.GetDataSetByName(KMAWScreenBottom);
    if ScreenBottomDataArray <> nil then
    begin
      ScreenBottomDataArray.Invalidate;
    end;
  end;
end;

procedure TMawWellScreenCollection.InvalidateScreenTopData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMawWellScreenTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ScreenTopDataArray: TDataArray;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TMawWellScreenTimeListLink;
    Link.FScreenTopData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TMawWellScreenTimeListLink;
        Link.FScreenTopData.Invalidate;
      end;
    end;

    ScreenTopDataArray := PhastModel.DataArrayManager.GetDataSetByName(KMAWScreenTop);
    if ScreenTopDataArray <> nil then
    begin
      ScreenTopDataArray.Invalidate;
    end;
  end;
end;

procedure TMawWellScreenCollection.InvalidateSkinKData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMawWellScreenTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  SkinKDataArray: TDataArray;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TMawWellScreenTimeListLink;
    Link.FSkinKData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TMawWellScreenTimeListLink;
        Link.FSkinKData.Invalidate;
      end;
    end;

    SkinKDataArray := PhastModel.DataArrayManager.GetDataSetByName(KMAWSkinK);
    if SkinKDataArray <> nil then
    begin
      SkinKDataArray.Invalidate;
    end;
  end;
end;

procedure TMawWellScreenCollection.InvalidateSkinRadiusData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMawWellScreenTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  SkinRadiusDataArray: TDataArray;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TMawWellScreenTimeListLink;
    Link.FSkinRadiusData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TMawWellScreenTimeListLink;
        Link.FSkinRadiusData.Invalidate;
      end;
    end;

    SkinRadiusDataArray := PhastModel.DataArrayManager.GetDataSetByName(KMAWSkinRadius);
    if SkinRadiusDataArray <> nil then
    begin
      SkinRadiusDataArray.Invalidate;
    end;
  end;
end;

class function TMawWellScreenCollection.ItemClass: TBoundaryItemClass;
begin
  result := TMawWellScreenItem;
end;

procedure TMawWellScreenCollection.Loaded;
var
  ItemIndex: Integer;
begin
  for ItemIndex := 0 to Count -1 do
  begin
    (Items[ItemIndex] as TMawWellScreenItem).Loaded;
  end;
end;

procedure TMawWellScreenCollection.SetBoundaryStartAndEndTime(
  BoundaryCount: Integer; Item: TCustomModflowBoundaryItem; ItemIndex: Integer;
  AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TMawSteadyConnectionStorage).FMawSteadyConnectionArray, BoundaryCount);
  inherited;
end;

procedure TMawWellScreenCollection.SetItems(Index: Integer;
  const Value: TMawWellScreenItem);
begin
  inherited Items[Index] := Value;
end;

function TMawWellScreenCollection.ShouldDeleteItemsWithZeroDuration: Boolean;
begin
  result := False;
end;

{ TMawWellScreenTimeListLink }

procedure TMawWellScreenTimeListLink.CreateTimeLists;
begin
  inherited;
  FScreenTopData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FScreenTopData.NonParamDescription := StrScreenTop;
  FScreenTopData.ParamDescription := StrScreenTop;
  if Model <> nil then
  begin
//    FScreenTopData.OnInvalidate := (Model as TCustomModel).InvalidateMfWellPumpage;
  end;
  AddTimeList(FScreenTopData);

  FScreenBottomData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FScreenBottomData.NonParamDescription := StrScreenBottom;
  FScreenBottomData.ParamDescription := StrScreenBottom;
  if Model <> nil then
  begin
//    FScreenBottomData.OnInvalidate := (Model as TCustomModel).InvalidateMfWellPumpage;
  end;
  AddTimeList(FScreenBottomData);

  FSkinKData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FSkinKData.NonParamDescription := StrSkinK;
  FSkinKData.ParamDescription := StrSkinK;
  if Model <> nil then
  begin
//    FSkinKData.OnInvalidate := (Model as TCustomModel).InvalidateMfWellPumpage;
  end;
  AddTimeList(FSkinKData);

  FSkinRadiusData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FSkinRadiusData.NonParamDescription := StrSkinRadius;
  FSkinRadiusData.ParamDescription := StrSkinRadius;
  if Model <> nil then
  begin
//    FSkinRadiusData.OnInvalidate := (Model as TCustomModel).InvalidateMfWellPumpage;
  end;
  AddTimeList(FSkinRadiusData);

end;

destructor TMawWellScreenTimeListLink.Destroy;
begin
  FScreenTopData.Free;
  FScreenBottomData.Free;
  FSkinKData.Free;
  FSkinRadiusData.Free;
  inherited;
end;

{ TMawBoundary }

procedure TMawBoundary.Assign(Source: TPersistent);
var
  SourceMAW: TMawBoundary;
  SourceMnw2: TMnw2Boundary;
  ScreenIndex: Integer;
  AScreen: TMawWellScreenItem;
  MnwWellScreen: TVerticalScreen;
  Mnw2ScreenObject: TScreenObject;
  SpatialItem: TMnw2SpatialItem;
  TimeIndex: Integer;
  MawItem: TMawItem;
//  Mnw2TimeItem: TMnw2TimeItem;
begin
  if Source is TMawBoundary then
  begin
    SourceMAW := TMawBoundary(Source);

    if Used <> SourceMAW.Used then
    begin
      if (ParentModel <> nil) and (ScreenObject <> nil)
        and (ScreenObject as TScreenObject).CanInvalidateModel  then
      begin
        InvalidateDisplayTimeLists;
      end;
    end;

    Radius := SourceMAW.Radius;
    Bottom := SourceMAW.Bottom;
    InitialHead := SourceMAW.InitialHead;
    ConductanceMethod := SourceMAW.ConductanceMethod;
    WellScreens := SourceMAW.WellScreens;

    PestFlowingWellElevationFormula := SourceMAW.PestFlowingWellElevationFormula;
    PestFlowingWellConductanceFormula := SourceMAW.PestFlowingWellConductanceFormula;
    PestRateFormula := SourceMAW.PestRateFormula;
    PestWellHeadFormula := SourceMAW.PestWellHeadFormula;
    PestHeadLimitFormula := SourceMAW.PestHeadLimitFormula;
    PestMinRateFormula := SourceMAW.PestMinRateFormula;
    PestMaxRateFormula := SourceMAW.PestMaxRateFormula;
    PestPumpElevationFormula := SourceMAW.PestPumpElevationFormula;
    PestScalingLengthFormula := SourceMAW.PestScalingLengthFormula;
    PestFlowingWellReductionLengthFormula := SourceMAW.PestFlowingWellReductionLengthFormula;

    PestFlowingWellElevationMethod := SourceMAW.PestFlowingWellElevationMethod;
    PestFlowingWellConductanceMethod := SourceMAW.PestFlowingWellConductanceMethod;
    PestRateMethod := SourceMAW.PestRateMethod;
    PestWellHeadMethod := SourceMAW.PestWellHeadMethod;
    PestHeadLimitMethod := SourceMAW.PestHeadLimitMethod;
    PestMinRateMethod := SourceMAW.PestMinRateMethod;
    PestMaxRateMethod := SourceMAW.PestMaxRateMethod;
    PestPumpElevationMethod := SourceMAW.PestPumpElevationMethod;
    PestScalingLengthMethod := SourceMAW.PestScalingLengthMethod;
    PestFlowingWellReductionLengthMethod := SourceMAW.PestFlowingWellReductionLengthMethod;
    StartingConcentrations := SourceMAW.StartingConcentrations;

    PestSpecifiedConcentrations := SourceMAW.PestSpecifiedConcentrations;
    PestSpecifiedConcentrationMethods := SourceMAW.PestSpecifiedConcentrationMethods;
    PestInjectionConcentrations := SourceMAW.PestInjectionConcentrations;
    PestInjectionConcentrationMethods := SourceMAW.PestInjectionConcentrationMethods;

    PestDensity := SourceMAW.PestDensity;
    PestDensityMethods := SourceMAW.PestDensityMethods;

    PestViscosity := SourceMAW.PestViscosity;
    PestViscosityMethods := SourceMAW.PestViscosityMethods;

    inherited;
  end
  else if Source is TMnw2Boundary then
  begin
    SourceMnw2 := TMnw2Boundary(Source);
    SpatialItem := SourceMnw2.Values[0] as TMnw2SpatialItem;
    Radius := SpatialItem.WellRadius;

    case SourceMnw2.Losstype of
      mltNone:
        begin
          ConductanceMethod := mcmThiem;
        end;
      mltThiem:
        begin
          ConductanceMethod := mcmThiem;
        end;
      mltSkin:
        begin
          ConductanceMethod := mcmCumulative;
        end;
      mltEquation:
        begin
          ConductanceMethod := mcmThiem;
        end;
      mtlSpecify:
        begin
          ConductanceMethod := mcmSpecified;
        end;
      else
        begin
          Assert(False);
        end;
    end;

    if SourceMnw2.VerticalScreens.Count > 0 then
    begin
      WellScreens.Clear;
      for ScreenIndex := 0 to SourceMnw2.VerticalScreens.Count - 1 do
      begin
        AScreen := WellScreens.Add as TMawWellScreenItem;
        MnwWellScreen := SourceMnw2.VerticalScreens[ScreenIndex];
        AScreen.ScreenTop := FortranFloatToStr(MnwWellScreen.ZTop);
        AScreen.ScreenBottom := FortranFloatToStr(MnwWellScreen.ZBottom);
        AScreen.SkinRadius := MnwWellScreen.SkinRadius;
        if ConductanceMethod = mcmSpecified then
        begin
          AScreen.SkinK := MnwWellScreen.CellToWellConductance;
        end
        else
        begin
          AScreen.SkinK := MnwWellScreen.SkinK;
        end;
      end;
    end
    else
    begin
      WellScreens.Clear;
      AScreen := WellScreens.Add as TMawWellScreenItem;
      Mnw2ScreenObject := SourceMnw2.ScreenObject as TScreenObject;
      if Mnw2ScreenObject.ElevationCount = ecTwo then
      begin
        AScreen.ScreenTop := Mnw2ScreenObject.HigherElevationFormula;
        AScreen.ScreenBottom := Mnw2ScreenObject.LowerElevationFormula;
      end
      else
      begin
        AScreen.ScreenTop := '0';
        AScreen.ScreenBottom := '0';
      end;
      AScreen.SkinRadius := SpatialItem.SkinRadius;
      if ConductanceMethod = mcmSpecified then
      begin
        AScreen.SkinK := SpatialItem.CellToWellConductance;
      end
      else
      begin
        AScreen.SkinK := SpatialItem.SkinK;
      end;
    end;
    Bottom := (WellScreens.Last as TMawWellScreenItem).ScreenBottom;
    InitialHead := rsModflow_Initial_Head;

    Values.Assign(SourceMnw2.TimeValues);

    if SourceMnw2.ConstrainPumping then
    begin
      for TimeIndex := 0 to Values.Count - 1 do
      begin
        MawItem := Values[TimeIndex] as TMawItem;
        MawItem.RateLimitation := rlShutoff;
      end;
    end;
  end;

end;

procedure TMawBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TMawCell;
  BoundaryValues: TMawTransientRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TMawTransientStorage;
  LocalModel: TCustomModel;
  LocalScreenObject: TScreenObject;
  MvrUsed: Boolean;
  LastIndex: Integer;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TMawTransientStorage;
  Assert(ScreenObject <> nil);
  LocalScreenObject := ScreenObject as TScreenObject;
  MvrUsed := (LocalScreenObject.ModflowMvr <> nil)
    and LocalScreenObject.ModflowMvr.Used
    and (LocalScreenObject.ModflowMvr.SourcePackageChoice = spcMaw);
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TMawCell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.MawTransientArray) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.MawTransientArray)
      end;
      LastIndex := Length(LocalBoundaryStorage.MawTransientArray) - 1;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.MawTransientArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.MawTransientArray[BoundaryIndex];
        BoundaryValues.MvrUsed := MvrUsed and (BoundaryIndex = LastIndex);
        BoundaryValues.MvrIndex := BoundaryIndex;
        Cell := TMawCell.Create;
        Assert(ScreenObject <> nil);
        Cell.IFace := (ScreenObject as TScreenObject).IFace;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values.Assign(BoundaryValues);
        Cell.ScreenObject := ScreenObjectI;
//        LocalModel.AdjustCellPosition(Cell);
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TMawBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TMawWellCollection;
end;

function TMawBoundary.BoundaryObserverPrefix: string;
begin
  result := 'MAW_';
end;

constructor TMawBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  FPestInjectionConcentrationObservers := TObserverList.Create;
  FPestSpecifiedConcentrationObservers := TObserverList.Create;
  FPestDensityObservers := TObserverList.Create;
  FPestViscosityObservers := TObserverList.Create;
  FPestInjectionConcentrations := TMawGwtConcCollection.Create(Model, ScreenObject, nil);
  FPestInjectionConcentrations.UsedForPestSeries := True;
  FPestSpecifiedConcentrations := TMawGwtConcCollection.Create(Model, ScreenObject, nil);
  FPestSpecifiedConcentrations.UsedForPestSeries := True;
  FPestInjectionConcentrationMethods := TGwtPestMethodCollection.Create(Model as TCustomModel);
  FPestSpecifiedConcentrationMethods := TGwtPestMethodCollection.Create(Model as TCustomModel);

  FPestDensity := TMawGwtConcCollection.Create(Model, ScreenObject, nil);
  FPestDensity.UsedForPestSeries := True;
  FPestDensityMethods := TGwtPestMethodCollection.Create(Model as TCustomModel);

  FPestViscosity := TMawGwtConcCollection.Create(Model, ScreenObject, nil);
  FPestViscosity.UsedForPestSeries := True;
  FPestViscosityMethods := TGwtPestMethodCollection.Create(Model as TCustomModel);

  FStartingConcentrations := TStringConcCollection.Create(Model as TCustomModel, ScreenObject, nil);
  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;
  FWellScreens := TMawWellScreenCollection.Create(Self, Model as TCustomModel, ScreenObject);

  LinkRadius;
  LinkBottom;
  LinkInitialHead;

  InitializeVariables;
end;

procedure TMawBoundary.CreateFormulaObjects;
var
  ConcIndex: Integer;
  LocalModel: TCustomModel;
begin
  FPestFlowingWellElevationFormula := CreateFormulaObjectBlocks(dsoTop);
  FPestFlowingWellConductanceFormula := CreateFormulaObjectBlocks(dsoTop);
  FPestRateFormula := CreateFormulaObjectBlocks(dsoTop);
  FPestWellHeadFormula := CreateFormulaObjectBlocks(dsoTop);
  FPestHeadLimitFormula := CreateFormulaObjectBlocks(dsoTop);
  FPestMinRateFormula := CreateFormulaObjectBlocks(dsoTop);
  FPestMaxRateFormula := CreateFormulaObjectBlocks(dsoTop);
  FPestPumpElevationFormula := CreateFormulaObjectBlocks(dsoTop);
  FPestScalingLengthFormula := CreateFormulaObjectBlocks(dsoTop);
  FPestFlowingWellReductionLengthFormula := CreateFormulaObjectBlocks(dsoTop);

  FRadius := CreateFormulaObjectBlocks(dso3D);
  FBottom := CreateFormulaObjectBlocks(dso3D);
  FInitialHead := CreateFormulaObjectBlocks(dso3D);

  LocalModel := ParentModel as TCustomModel;
  if (LocalModel <> nil) then
  begin
    if LocalModel.BuoyancyDensityUsed then
    begin
      PestDensity.Add;
    end;
    if LocalModel.ViscosityPkgViscUsed then
    begin
      PestViscosity.Add;
    end;

    if LocalModel.GwtUsed then
    begin
      for ConcIndex := 0 to LocalModel.MobileComponents.Count - 1 do
      begin
        FPestSpecifiedConcentrations.Add;
      end;
      for ConcIndex := 0 to LocalModel.MobileComponents.Count - 1 do
      begin
        FPestInjectionConcentrations.Add;
      end;
    end;
  end;

end;

procedure TMawBoundary.CreateObservers;
var
  Index: Integer;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(PestFlowingWellElevationObserver);
    FObserverList.Add(PestFlowingWellConductanceObserver);
    FObserverList.Add(PestRateObserver);
    FObserverList.Add(PestWellHeadObserver);
    FObserverList.Add(PestHeadLimitObserver);
    FObserverList.Add(PestMinRateObserver);
    FObserverList.Add(PestMaxRateObserver);
    FObserverList.Add(PestPumpElevationObserver);
    FObserverList.Add(PestScalingLengthObserver);
    FObserverList.Add(PestFlowingWellReductionLengthObserver);

    FObserverList.Add(RadiusObserver);
    FObserverList.Add(BottomObserver);
    FObserverList.Add(InitialHeadObserver);

    for Index := 0 to PestDensity.Count - 1 do
    begin
      FObserverList.Add(PestDensityObserver[Index]);
    end;
    for Index := 0 to PestViscosity.Count - 1 do
    begin
      FObserverList.Add(PestViscosityObserver[Index]);
    end;

    for Index := 0 to FPestSpecifiedConcentrations.Count - 1 do
    begin
      FObserverList.Add(PestSpecifiedConcentrationObserver[Index]);
    end;
    for Index := 0 to FPestInjectionConcentrations.Count - 1 do
    begin
      FObserverList.Add(PestInjectionConcentrationObserver[Index]);
    end;

  end;
end;

class function TMawBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    MawFlowingWellElevationPosition:
      begin
        result := ppmAdd;
      end;
    MawFlowingWellConductancePosition:
      begin
        result := ppmMultiply;
      end;
    MawRatePosition:
      begin
        result := ppmMultiply;
      end;
    MawWellHeadPosition:
      begin
        result := ppmAdd;
      end;
    MawHeadLimitPosition:
      begin
        result := ppmAdd;
      end;
    MawMinRatePosition:
      begin
        result := ppmMultiply;
      end;
    MawMaxRatePosition:
      begin
        result := ppmMultiply;
      end;
    MawPumpElevationPosition:
      begin
        result := ppmAdd;
      end;
    MawScalingLengthPosition:
      begin
        result := ppmMultiply;
      end;
    MawFlowingWellReductionLengthPosition:
      begin
        result := ppmMultiply;
      end;
    MawDensityPosition:
      begin
        result := ppmMultiply;
      end;
    MawViscosityPosition:
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

destructor TMawBoundary.Destroy;
begin
  PestFlowingWellElevationFormula := '';
  PestFlowingWellConductanceFormula := '';
  PestRateFormula := '';
  PestWellHeadFormula := '';
  PestHeadLimitFormula := '';
  PestMinRateFormula := '';
  PestMaxRateFormula := '';
  PestPumpElevationFormula := '';
  PestScalingLengthFormula := '';
  PestFlowingWellReductionLengthFormula := '';

  FWellScreens.Free;
  RemoveFormulaObjects;
  FStartingConcentrations.Free;
  inherited;

  FPestInjectionConcentrationMethods.Free;
  FPestSpecifiedConcentrationMethods.Free;
  FPestInjectionConcentrations.Free;
  FPestSpecifiedConcentrations.Free;
  FPestInjectionConcentrationObservers.Free;
  FPestSpecifiedConcentrationObservers.Free;

  FPestDensityMethods.Free;
  PestDensity.Free;
  FPestDensityObservers.Free;

  FPestViscosityMethods.Free;
  PestViscosity.Free;
  FPestViscosityObservers.Free;
end;

function TMawBoundary.GetBottom: string;
begin
  Result := FBottom.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(BottomPosition);
  end;
end;

function TMawBoundary.GetBottomObserver: TObserver;
begin
  if FBottomObserver = nil then
  begin
    CreateObserver('MAW_Bottom', FBottomObserver, nil);
  end;
  result := FBottomObserver;
end;

procedure TMawBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
var
  ValueIndex: Integer;
  BoundaryStorage: TMawTransientStorage;
begin
  EvaluateListBoundaries(AModel);
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TMawTransientStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
end;

function TMawBoundary.GetInitialHead: string;
begin
  Result := FInitialHead.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(InitialHeadPosition);
  end;
end;

function TMawBoundary.GetInitialHeadObserver: TObserver;
begin
  if FInitialHeadObserver = nil then
  begin
    CreateObserver('MAW_Initial_Head', FInitialHeadObserver, nil);
  end;
  result := FInitialHeadObserver;
end;

function TMawBoundary.GetPestInjectionConcentrationObserver(
  const Index: Integer): TObserver;
var
  AObserver: TObserver;
begin
  while Index >= FPestInjectionConcentrationObservers.Count do
  begin
    CreateObserver(Format('MawPestInjConc_%d', [Index+1]), AObserver, nil);
    FPestInjectionConcentrationObservers.Add(AObserver);
    AObserver.OnUpToDateSet := InvalidatePestInjConcData;
  end;
  result := FPestInjectionConcentrationObservers[Index];
end;

function TMawBoundary.GetPestBoundaryFormula(FormulaIndex: integer): string;
var
  ChemSpeciesCount: Integer;
begin
  case FormulaIndex of
    MawFlowingWellElevationPosition:
      begin
        result := PestFlowingWellElevationFormula;
      end;
    MawFlowingWellConductancePosition:
      begin
        result := PestFlowingWellConductanceFormula;
      end;
    MawRatePosition:
      begin
        result := PestRateFormula;
      end;
    MawWellHeadPosition:
      begin
        result := PestWellHeadFormula;
      end;
    MawHeadLimitPosition:
      begin
        result := PestHeadLimitFormula;
      end;
    MawMinRatePosition:
      begin
        result := PestMinRateFormula;
      end;
    MawMaxRatePosition:
      begin
        result := PestMaxRateFormula;
      end;
    MawPumpElevationPosition:
      begin
        result := PestPumpElevationFormula;
      end;
    MawScalingLengthPosition:
      begin
        result := PestScalingLengthFormula;
      end;
    MawFlowingWellReductionLengthPosition:
      begin
        result := PestFlowingWellReductionLengthFormula;
      end;
    MawDensityPosition:
      begin
        if PestDensity.Count < 1 then
        begin
          PestDensity.Add;
        end;
        result := PestDensity[0].Value;
      end;
    MawViscosityPosition:
      begin
        if PestViscosity.Count < 1 then
        begin
          PestViscosity.Add;
        end;
        result := PestViscosity[0].Value;
      end;
    else
      begin
        FormulaIndex := FormulaIndex-MawGwtStart;
        ChemSpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;

        while PestSpecifiedConcentrations.Count < ChemSpeciesCount do
        begin
          PestSpecifiedConcentrations.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          result := PestSpecifiedConcentrations[FormulaIndex].Value;
          Exit;
        end;

        FormulaIndex := FormulaIndex-ChemSpeciesCount;
        while PestInjectionConcentrations.Count < ChemSpeciesCount do
        begin
          PestInjectionConcentrations.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          result := PestInjectionConcentrations[FormulaIndex].Value;
          Exit;
        end;
        Assert(False);
      end;
  end;
end;

function TMawBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
var
  ChemSpeciesCount: Integer;
begin
  case FormulaIndex of
    MawFlowingWellElevationPosition:
      begin
        result := PestFlowingWellElevationMethod;
      end;
    MawFlowingWellConductancePosition:
      begin
        result := PestFlowingWellConductanceMethod;
      end;
    MawRatePosition:
      begin
        result := PestRateMethod;
      end;
    MawWellHeadPosition:
      begin
        result := PestWellHeadMethod;
      end;
    MawHeadLimitPosition:
      begin
        result := PestHeadLimitMethod;
      end;
    MawMinRatePosition:
      begin
        result := PestMinRateMethod;
      end;
    MawMaxRatePosition:
      begin
        result := PestMaxRateMethod;
      end;
    MawPumpElevationPosition:
      begin
        result := PestPumpElevationMethod;
      end;
    MawScalingLengthPosition:
      begin
        result := PestScalingLengthMethod;
      end;
    MawFlowingWellReductionLengthPosition:
      begin
        result := PestFlowingWellReductionLengthMethod;
      end;
    MawDensityPosition:
      begin
        if PestDensityMethods.Count < 1 then
        begin
          PestDensityMethods.Add;
        end;
        result := PestDensityMethods[0].PestParamMethod;
      end;
    MawViscosityPosition:
      begin
        if PestViscosityMethods.Count < 1 then
        begin
          PestViscosityMethods.Add;
        end;
        result := PestViscosityMethods[0].PestParamMethod;
      end;
    else
      begin
        FormulaIndex := FormulaIndex-MawGwtStart;
        ChemSpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;

        while PestSpecifiedConcentrationMethods.Count < ChemSpeciesCount do
        begin
          PestSpecifiedConcentrationMethods.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          result := PestSpecifiedConcentrationMethods[FormulaIndex].PestParamMethod;
          Exit;
        end;

        FormulaIndex := FormulaIndex-ChemSpeciesCount;
        while PestInjectionConcentrationMethods.Count < ChemSpeciesCount do
        begin
          PestInjectionConcentrationMethods.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          result := PestInjectionConcentrationMethods[FormulaIndex].PestParamMethod;
          Exit;
        end;
        Result := inherited;
        Assert(False);
      end;
  end;
end;

function TMawBoundary.GetPestDensityObserver(const Index: Integer): TObserver;
var
  AObserver: TObserver;
begin
  while Index >= FPestDensityObservers.Count do
  begin
    CreateObserver(Format('MawPestDensity_%d', [Index+1]), AObserver, nil);
    FPestDensityObservers.Add(AObserver);
    AObserver.OnUpToDateSet := InvalidatePestDensityData;
  end;
  result := FPestDensityObservers[Index];
end;

function TMawBoundary.GetPestFlowingWellConductanceFormula: string;
begin
  Result := FPestFlowingWellConductanceFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(MawFlowingWellConductancePosition);
  end;
end;

function TMawBoundary.GetPestFlowingWellConductanceObserver: TObserver;
begin
  if FPestFlowingWellConductanceObserver = nil then
  begin
    CreateObserver('PestFlowingWellConductance_', FPestFlowingWellConductanceObserver, nil);
    FPestFlowingWellConductanceObserver.OnUpToDateSet := InvalidateFlowingWellConductanceData;
  end;
  result := FPestFlowingWellConductanceObserver;
end;

function TMawBoundary.GetPestFlowingWellElevationFormula: string;
begin
  Result := FPestFlowingWellElevationFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(MawFlowingWellElevationPosition);
  end;
end;

function TMawBoundary.GetPestFlowingWellElevationObserver: TObserver;
begin
  if FPestFlowingWellElevationObserver = nil then
  begin
    CreateObserver('PestFlowingWellElevation_', FPestFlowingWellElevationObserver, nil);
    FPestFlowingWellElevationObserver.OnUpToDateSet := InvalidateFlowingWellElevationData;
  end;
  result := FPestFlowingWellElevationObserver;
end;

function TMawBoundary.GetPestFlowingWellReductionLengthFormula: string;
begin
  Result := FPestFlowingWellReductionLengthFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(MawFlowingWellReductionLengthPosition);
  end;
end;

function TMawBoundary.GetPestFlowingWellReductionLengthObserver: TObserver;
begin
  if FPestFlowingWellReductionLengthObserver = nil then
  begin
    CreateObserver('PestFlowingWellReductionLength_', FPestFlowingWellReductionLengthObserver, nil);
    FPestFlowingWellReductionLengthObserver.OnUpToDateSet := InvalidateFlowingWellReductionLengthData;
  end;
  result := FPestFlowingWellReductionLengthObserver;
end;

function TMawBoundary.GetPestHeadLimitFormula: string;
begin
  Result := FPestHeadLimitFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(MawHeadLimitPosition);
  end;
end;

function TMawBoundary.GetPestHeadLimitObserver: TObserver;
begin
  if FPestHeadLimitObserver = nil then
  begin
    CreateObserver('PestHeadLimit_', FPestHeadLimitObserver, nil);
    FPestHeadLimitObserver.OnUpToDateSet := InvalidateHeadLimitData;
  end;
  result := FPestHeadLimitObserver;
end;

function TMawBoundary.GetPestMaxRateFormula: string;
begin
  Result := FPestMaxRateFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(MawMaxRatePosition);
  end;
end;

function TMawBoundary.GetPestMaxRateObserver: TObserver;
begin
  if FPestMaxRateObserver = nil then
  begin
    CreateObserver('PestMaxRate_', FPestMaxRateObserver, nil);
    FPestMaxRateObserver.OnUpToDateSet := InvalidateMaxRateData;
  end;
  result := FPestMaxRateObserver;
end;

function TMawBoundary.GetPestMinRateFormula: string;
begin
  Result := FPestMinRateFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(MawMinRatePosition);
  end;
end;

function TMawBoundary.GetPestMinRateObserver: TObserver;
begin
  if FPestMinRateObserver = nil then
  begin
    CreateObserver('PestMinRate_', FPestMinRateObserver, nil);
    FPestMinRateObserver.OnUpToDateSet := InvalidateMinRateData;
  end;
  result := FPestMinRateObserver;
end;

function TMawBoundary.GetPestPumpElevationFormula: string;
begin
  Result := FPestPumpElevationFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(MawPumpElevationPosition);
  end;
end;

function TMawBoundary.GetPestPumpElevationObserver: TObserver;
begin
  if FPestPumpElevationObserver = nil then
  begin
    CreateObserver('PestPumpElevation_', FPestPumpElevationObserver, nil);
    FPestPumpElevationObserver.OnUpToDateSet := InvalidatePumpElevationData;
  end;
  result := FPestPumpElevationObserver;
end;

function TMawBoundary.GetPestRateFormula: string;
begin
  Result := FPestRateFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(MawRatePosition);
  end;
end;

function TMawBoundary.GetPestRateObserver: TObserver;
begin
  if FPestRateObserver = nil then
  begin
    CreateObserver('PestRate_', FPestRateObserver, nil);
    FPestRateObserver.OnUpToDateSet := InvalidateRateData;
  end;
  result := FPestRateObserver;
end;

function TMawBoundary.GetPestScalingLengthFormula: string;
begin
  Result := FPestScalingLengthFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(MawScalingLengthPosition);
  end;
end;

function TMawBoundary.GetPestScalingLengthObserver: TObserver;
begin
  if FPestScalingLengthObserver = nil then
  begin
    CreateObserver('PestScalingLength_', FPestScalingLengthObserver, nil);
    FPestScalingLengthObserver.OnUpToDateSet := InvalidateScalingLengthData;
  end;
  result := FPestScalingLengthObserver;
end;

function TMawBoundary.GetPestWellHeadFormula: string;
begin
  Result := FPestWellHeadFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(MawWellHeadPosition);
  end;
end;

function TMawBoundary.GetPestWellHeadObserver: TObserver;
begin
  if FPestWellHeadObserver = nil then
  begin
    CreateObserver('PestWellHead_', FPestWellHeadObserver, nil);
    FPestWellHeadObserver.OnUpToDateSet := InvalidateWellHeadData;
  end;
  result := FPestWellHeadObserver;
end;

procedure TMawBoundary.GetPropertyObserver(Sender: TObject; List: TList);
var
  StartIndex: Integer;
  Index: Integer;
begin
  if Sender = FPestFlowingWellElevationFormula as TObject then
  begin
    if MawFlowingWellElevationPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[MawFlowingWellElevationPosition]);
    end;
  end;

  if Sender = FPestFlowingWellConductanceFormula as TObject then
  begin
    if MawFlowingWellConductancePosition < FObserverList.Count then
    begin
      List.Add(FObserverList[MawFlowingWellConductancePosition]);
    end;
  end;

  if Sender = FPestRateFormula as TObject then
  begin
    if MawRatePosition < FObserverList.Count then
    begin
      List.Add(FObserverList[MawRatePosition]);
    end;
  end;

  if Sender = FPestWellHeadFormula as TObject then
  begin
    if MawWellHeadPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[MawWellHeadPosition]);
    end;
  end;

  if Sender = FPestHeadLimitFormula as TObject then
  begin
    if MawHeadLimitPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[MawHeadLimitPosition]);
    end;
  end;

  if Sender = FPestMinRateFormula as TObject then
  begin
    if MawMinRatePosition < FObserverList.Count then
    begin
      List.Add(FObserverList[MawMinRatePosition]);
    end;
  end;

  if Sender = FPestMaxRateFormula as TObject then
  begin
    if MawMaxRatePosition < FObserverList.Count then
    begin
      List.Add(FObserverList[MawMaxRatePosition]);
    end;
  end;

  if Sender = FPestPumpElevationFormula as TObject then
  begin
    if MawPumpElevationPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[MawPumpElevationPosition]);
    end;
  end;

  if Sender = FPestScalingLengthFormula as TObject then
  begin
    if MawScalingLengthPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[MawScalingLengthPosition]);
    end;
  end;

  if Sender = FPestFlowingWellReductionLengthFormula as TObject then
  begin
    if MawFlowingWellReductionLengthPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[MawFlowingWellReductionLengthPosition]);
    end;
  end;

  for Index := 0 to FPestDensity.Count - 1 do
  begin
    if FPestDensity[Index].ValueObject as TObject = Sender then
    begin
      List.Add(FObserverList[MawDensityPosition]);
    end;
  end;

  for Index := 0 to FPestViscosity.Count - 1 do
  begin
    if FPestViscosity[Index].ValueObject as TObject = Sender then
    begin
      List.Add(FObserverList[MawViscosityPosition]);
    end;
  end;

  StartIndex := MawGwtStart;
  for Index := 0 to FPestSpecifiedConcentrations.Count - 1 do
  begin
    if FPestSpecifiedConcentrations[Index].ValueObject as TObject = Sender then
    begin
      List.Add(FObserverList[StartIndex + Index]);
    end;
  end;

  StartIndex := StartIndex + FPestSpecifiedConcentrations.Count;
  for Index := 0 to FPestInjectionConcentrations.Count - 1 do
  begin
    if FPestInjectionConcentrations[Index].ValueObject as TObject = Sender then
    begin
      List.Add(FObserverList[StartIndex + Index]);
    end;
  end;

end;

function TMawBoundary.GetRadius: string;
begin
  Result := FRadius.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(RadiusPosition);
  end;
end;

function TMawBoundary.GetRadiusObserver: TObserver;
begin
  if FRadiusObserver = nil then
  begin
    CreateObserver('MAW_Radius', FRadiusObserver, nil);

  end;
  result := FRadiusObserver;
end;

function TMawBoundary.GetStartingConcentrations: TStringConcCollection;
var
  LocalModel: TCustomModel;
begin
  LocalModel := FStartingConcentrations.Model as TCustomModel;
  if LocalModel = nil then
  begin
    LocalModel := frmGoPhast.PhastModel;
  end;
  if (LocalModel <> nil)
    and (FStartingConcentrations.Count < LocalModel.MobileComponents.Count) then
  begin
    FStartingConcentrations.Count := LocalModel.MobileComponents.Count;
  end;
  result := FStartingConcentrations;
end;

function TMawBoundary.GetPestSpecifiedConcentrationObserver(
  const Index: Integer): TObserver;
var
  AObserver: TObserver;
begin
  while Index >= FPestSpecifiedConcentrationObservers.Count do
  begin
    CreateObserver(Format('MawPestSpecConc_%d', [Index+1]), AObserver, nil);
    FPestSpecifiedConcentrationObservers.Add(AObserver);
    AObserver.OnUpToDateSet := InvalidatePestSpecConcData;
  end;
  result := FPestSpecifiedConcentrationObservers[Index];
end;

function TMawBoundary.GetPestViscosityObserver(const Index: Integer): TObserver;
var
  AObserver: TObserver;
begin
  while Index >= FPestViscosityObservers.Count do
  begin
    CreateObserver(Format('MawPestViscosity_%d', [Index+1]), AObserver, nil);
    FPestViscosityObservers.Add(AObserver);
    AObserver.OnUpToDateSet := InvalidatePestViscosityData;
  end;
  result := FPestViscosityObservers[Index];
end;

function TMawBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('PestMAW_Used_', FUsedObserver, nil);
  end;
  result := FUsedObserver;
end;

procedure TMawBoundary.HandleChangedValue(Observer: TObserver);
begin
//  inherited;
  InvalidateDisplay;
end;

procedure TMawBoundary.InitializeVariables;
var
  PestIndex: Integer;
begin
  Radius := '0';
  Bottom := '0';
  InitialHead := '0';
  ConductanceMethod := mcmSpecified;

  PestFlowingWellElevationFormula := '';
  PestFlowingWellConductanceFormula := '';
  PestRateFormula := '';
  PestWellHeadFormula := '';
  PestHeadLimitFormula := '';
  PestMinRateFormula := '';
  PestMaxRateFormula := '';
  PestPumpElevationFormula := '';
  PestScalingLengthFormula := '';
  PestFlowingWellReductionLengthFormula := '';

  for PestIndex := MawFlowingWellElevationPosition to MawFlowingWellReductionLengthPosition do
  begin
    PestBoundaryMethod[PestIndex] := DefaultBoundaryMethod(PestIndex);
  end;
end;

procedure TMawBoundary.InvalidateDisplay;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    InvalidateDisplayTimeLists;
  end;
end;

procedure TMawBoundary.InvalidateDisplayTimeLists;
var
  LocaModel: TPhastModel;
begin
  LocaModel := ParentModel as TPhastModel;
  if LocaModel.Clearing then
  begin
    Exit;
  end;
  LocaModel.InvalidateMawFlowingWellElevation(self);
  LocaModel.InvalidateMawFlowingWellConductance(self);
  LocaModel.InvalidateMawFlowingWellReductionLength(self);
  LocaModel.InvalidateMawWell_Rate(self);
  LocaModel.InvalidateMawWell_Head(self);
  LocaModel.InvalidateMawWell_Limit(self);
  LocaModel.InvalidateMawMinimumPumpRate(self);
  LocaModel.InvalidateMawMaximumPumpRate(self);
  LocaModel.InvalidateMawPumpElevation(self);
  LocaModel.InvalidateMawScalingLength(self);
  LocaModel.InvalidateMawGwtConc(self);
end;


procedure TMawBoundary.InvalidateFlowingWellConductanceData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMawFlowingWellConductance(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMawFlowingWellConductance(self);
      end;
    end;
  end;
end;

procedure TMawBoundary.InvalidateFlowingWellElevationData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMawFlowingWellElevation(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMawFlowingWellElevation(self);
      end;
    end;
  end;
end;

procedure TMawBoundary.InvalidateFlowingWellReductionLengthData(
  Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMawFlowingWellReductionLength(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMawFlowingWellReductionLength(self);
      end;
    end;
  end;
end;

procedure TMawBoundary.InvalidateHeadLimitData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMawWell_Limit(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMawWell_Limit(self);
      end;
    end;
  end;
end;

procedure TMawBoundary.InvalidateMaxRateData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMawMaximumPumpRate(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMawMaximumPumpRate(self);
      end;
    end;
  end;
end;

procedure TMawBoundary.InvalidateMinRateData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMawMinimumPumpRate(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMawMinimumPumpRate(self);
      end;
    end;
  end;
end;

procedure TMawBoundary.InvalidatePestDensityData(Sender: TObject);
begin
  { TODO -cGWT : This needs to be implemented }
  //Assert(False);
end;

procedure TMawBoundary.InvalidatePestInjConcData(Sender: TObject);
begin
  { TODO -cGWT : This needs to be implemented }
  //Assert(False);
end;

procedure TMawBoundary.InvalidatePestSpecConcData(Sender: TObject);
begin
  { TODO -cGWT : This needs to be implemented }
  //Assert(False);
end;

procedure TMawBoundary.InvalidatePestViscosityData(Sender: TObject);
begin
  { TODO -cGWT : This needs to be implemented }
  //Assert(False);
end;

procedure TMawBoundary.InvalidatePumpElevationData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMawPumpElevation(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMawPumpElevation(self);
      end;
    end;
  end;
end;

procedure TMawBoundary.InvalidateRateData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMawWell_Rate(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMawWell_Rate(self);
      end;
    end;
  end;
end;

procedure TMawBoundary.InvalidateScalingLengthData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMawScalingLength(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMawScalingLength(self);
      end;
    end;
  end;
end;

procedure TMawBoundary.InvalidateWellHeadData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMawWell_Head(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMawWell_Head(self);
      end;
    end;
  end;
end;

procedure TMawBoundary.LinkBottom;
var
  LocalScreenObject: TScreenObject;
  MawBottomArray: TDataArray;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.TalksTo(BottomObserver);
    if ParentModel <> nil then
    begin
      MawBottomArray := (ParentModel as TCustomModel).DataArrayManager.
        GetDataSetByName(KMAWBottom);
      if MawBottomArray <> nil then
      begin
        BottomObserver.TalksTo(MawBottomArray);
      end;
    end;
  end;
end;

procedure TMawBoundary.LinkInitialHead;
var
  LocalScreenObject: TScreenObject;
  MawInitialHeadArray: TDataArray;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.TalksTo(InitialHeadObserver);
    if ParentModel <> nil then
    begin
      MawInitialHeadArray := (ParentModel as TCustomModel).DataArrayManager.
        GetDataSetByName(KMAWInitialHead);
      if MawInitialHeadArray <> nil then
      begin
        InitialHeadObserver.TalksTo(MawInitialHeadArray);
      end;
    end;
  end;
end;

procedure TMawBoundary.LinkRadius;
var
  LocalScreenObject: TScreenObject;
  MawRadiusArray: TDataArray;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.TalksTo(RadiusObserver);
    if ParentModel <> nil then
    begin
      MawRadiusArray := (ParentModel as TCustomModel).DataArrayManager.
        GetDataSetByName(KMAWRadius);
      if MawRadiusArray <> nil then
      begin
        RadiusObserver.TalksTo(MawRadiusArray);
      end;
    end;
  end;
end;

procedure TMawBoundary.Loaded;
begin
  LinkRadius;
  LinkBottom;
  LinkInitialHead;
  FWellScreens.Loaded;
end;

procedure TMawBoundary.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FBottom,
    GlobalRemoveMFBoundarySubscription,
    GlobalRestoreMFBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FInitialHead,
    GlobalRemoveMFBoundarySubscription,
    GlobalRestoreMFBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRadius,
    GlobalRemoveMFBoundarySubscription,
    GlobalRestoreMFBoundarySubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FPestFlowingWellElevationFormula,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FPestFlowingWellConductanceFormula,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FPestRateFormula,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FPestWellHeadFormula,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FPestHeadLimitFormula,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FPestMinRateFormula,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FPestMaxRateFormula,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FPestPumpElevationFormula,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FPestScalingLengthFormula,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FPestFlowingWellReductionLengthFormula,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TMawBoundary.SetBottom(const Value: string);
begin
  UpdateFormulaBlocks(Value, BottomPosition, FBottom);
end;

procedure TMawBoundary.SetConductanceMethod(Value: TMawConductanceMethod);
begin
  if Value = mcmTheim then
  begin
    Value := mcmThiem;
  end;

  if FConductanceMethod <> Value then
  begin
    FConductanceMethod := Value;
    InvalidateModel;
  end;
end;

procedure TMawBoundary.SetInitialHead(const Value: string);
begin
  UpdateFormulaBlocks(Value, InitialHeadPosition, FInitialHead);
end;

procedure TMawBoundary.SetPestBoundaryFormula(FormulaIndex: integer;
  const Value: string);
var
  ChemSpeciesCount: Integer;
begin
  case FormulaIndex of
    MawFlowingWellElevationPosition:
      begin
        PestFlowingWellElevationFormula := Value;
      end;
    MawFlowingWellConductancePosition:
      begin
        PestFlowingWellConductanceFormula := Value;
      end;
    MawRatePosition:
      begin
        PestRateFormula := Value;
      end;
    MawWellHeadPosition:
      begin
        PestWellHeadFormula := Value;
      end;
    MawHeadLimitPosition:
      begin
        PestHeadLimitFormula := Value;
      end;
    MawMinRatePosition:
      begin
        PestMinRateFormula := Value;
      end;
    MawMaxRatePosition:
      begin
        PestMaxRateFormula := Value;
      end;
    MawPumpElevationPosition:
      begin
        PestPumpElevationFormula := Value;
      end;
    MawScalingLengthPosition:
      begin
        PestScalingLengthFormula := Value;
      end;
    MawFlowingWellReductionLengthPosition:
      begin
        PestFlowingWellReductionLengthFormula := Value;
      end;
    MawDensityPosition:
      begin
        if PestDensity.Count < 1 then
        begin
          PestDensity.Add;
        end;
        PestDensity[0].Value := Value;
      end;
    MawViscosityPosition:
      begin
        if PestViscosity.Count < 1 then
        begin
          PestViscosity.Add;
        end;
        PestViscosity[0].Value := Value;
      end;
    else
      begin
        FormulaIndex := FormulaIndex-MawGwtStart;
        ChemSpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;

        while PestSpecifiedConcentrations.Count < ChemSpeciesCount do
        begin
          PestSpecifiedConcentrations.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          PestSpecifiedConcentrations[FormulaIndex].Value := Value;
          Exit;
        end;

        FormulaIndex := FormulaIndex-ChemSpeciesCount;
        while PestInjectionConcentrations.Count < ChemSpeciesCount do
        begin
          PestInjectionConcentrations.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          PestInjectionConcentrations[FormulaIndex].Value := Value;
          Exit;
        end;
        Assert(False);
      end;
  end;
end;

procedure TMawBoundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
var
  ChemSpeciesCount: Integer;
begin
  case FormulaIndex of
    MawFlowingWellElevationPosition:
      begin
        PestFlowingWellElevationMethod := Value;
      end;
    MawFlowingWellConductancePosition:
      begin
        PestFlowingWellConductanceMethod := Value;
      end;
    MawRatePosition:
      begin
        PestRateMethod := Value;
      end;
    MawWellHeadPosition:
      begin
        PestWellHeadMethod := Value;
      end;
    MawHeadLimitPosition:
      begin
        PestHeadLimitMethod := Value;
      end;
    MawMinRatePosition:
      begin
        PestMinRateMethod := Value;
      end;
    MawMaxRatePosition:
      begin
        PestMaxRateMethod := Value;
      end;
    MawPumpElevationPosition:
      begin
        PestPumpElevationMethod := Value;
      end;
    MawScalingLengthPosition:
      begin
        PestScalingLengthMethod := Value;
      end;
    MawFlowingWellReductionLengthPosition:
      begin
        PestFlowingWellReductionLengthMethod := Value;
      end;
    MawDensityPosition:
      begin
        if PestDensityMethods.Count < 1 then
        begin
          PestDensityMethods.Add;
        end;
        PestDensityMethods[0].PestParamMethod := Value;
      end;
    MawViscosityPosition:
      begin
        if PestViscosityMethods.Count < 1 then
        begin
          PestViscosityMethods.Add;
        end;
        PestViscosityMethods[0].PestParamMethod := Value;
      end;
    else
      begin
        FormulaIndex := FormulaIndex-MawGwtStart;
        ChemSpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;

        while PestSpecifiedConcentrationMethods.Count < ChemSpeciesCount do
        begin
          PestSpecifiedConcentrationMethods.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          PestSpecifiedConcentrationMethods[FormulaIndex].PestParamMethod := Value;
          Exit;
        end;

        FormulaIndex := FormulaIndex-ChemSpeciesCount;
        while PestInjectionConcentrationMethods.Count < ChemSpeciesCount do
        begin
          PestInjectionConcentrationMethods.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          PestInjectionConcentrationMethods[FormulaIndex].PestParamMethod := Value;
          Exit;
        end;
        Assert(False);
      end;
  end;
end;

procedure TMawBoundary.SetPestDensity(const Value: TMawGwtConcCollection);
begin
  FPestDensity.Assign(Value);
end;

procedure TMawBoundary.SetPestDensityMethods(
  const Value: TGwtPestMethodCollection);
begin
  FPestDensityMethods.Assign(Value);
end;

procedure TMawBoundary.SetPestFlowingWellConductanceFormula(
  const Value: string);
begin
  UpdateFormulaBlocks(Value, MawFlowingWellConductancePosition, FPestFlowingWellConductanceFormula);
end;

procedure TMawBoundary.SetPestFlowingWellConductanceMethod(
  const Value: TPestParamMethod);
begin
  FPestFlowingWellConductanceMethod := Value;
end;

procedure TMawBoundary.SetPestFlowingWellElevationFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, MawFlowingWellElevationPosition, FPestFlowingWellElevationFormula);
end;

procedure TMawBoundary.SetPestFlowingWellElevationMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestFlowingWellElevationMethod, Value);
end;

procedure TMawBoundary.SetPestFlowingWellReductionLengthFormula(
  const Value: string);
begin
  UpdateFormulaBlocks(Value, MawFlowingWellReductionLengthPosition, FPestFlowingWellReductionLengthFormula);
end;

procedure TMawBoundary.SetPestFlowingWellReductionLengthMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestFlowingWellReductionLengthMethod, Value);
end;

procedure TMawBoundary.SetPestHeadLimitFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, MawHeadLimitPosition, FPestHeadLimitFormula);
end;

procedure TMawBoundary.SetPestHeadLimitMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestHeadLimitMethod, Value);
end;

procedure TMawBoundary.SetPestInjectionConcentrationMethods(
  const Value: TGwtPestMethodCollection);
begin
  FPestInjectionConcentrationMethods.Assign(Value);
end;

procedure TMawBoundary.SetPestInjectionConcentrations(
  const Value: TMawGwtConcCollection);
begin
  FPestInjectionConcentrations.Assign(Value);
end;

procedure TMawBoundary.SetPestMaxRateFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, MawMaxRatePosition, FPestMaxRateFormula);
end;

procedure TMawBoundary.SetPestMaxRateMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestMaxRateMethod, Value);
end;

procedure TMawBoundary.SetPestMinRateFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, MawMinRatePosition, FPestMinRateFormula);
end;

procedure TMawBoundary.SetPestMinRateMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestMinRateMethod, Value);
end;

procedure TMawBoundary.SetPestPumpElevationFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, MawPumpElevationPosition, FPestPumpElevationFormula);
end;

procedure TMawBoundary.SetPestPumpElevationMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestPumpElevationMethod, Value);
end;

procedure TMawBoundary.SetPestRateFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, MawRatePosition, FPestRateFormula);
end;

procedure TMawBoundary.SetPestRateMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestRateMethod, Value);
end;

procedure TMawBoundary.SetPestScalingLengthFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, MawScalingLengthPosition, FPestScalingLengthFormula);
end;

procedure TMawBoundary.SetPestScalingLengthMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestScalingLengthMethod, Value);
end;

procedure TMawBoundary.SetPestSpecifiedConcentrationMethods(
  const Value: TGwtPestMethodCollection);
begin
  FPestSpecifiedConcentrationMethods.Assign(Value);
end;

procedure TMawBoundary.SetPestSpecifiedConcentrations(
  const Value: TMawGwtConcCollection);
begin
  FPestSpecifiedConcentrations.Assign(Value);
end;

procedure TMawBoundary.SetPestViscosity(const Value: TMawGwtConcCollection);
begin
  FPestViscosity.Assign(Value);
end;

procedure TMawBoundary.SetPestViscosityMethods(
  const Value: TGwtPestMethodCollection);
begin
  FPestViscosityMethods.Assign(Value);
end;

procedure TMawBoundary.SetPestWellHeadFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, MawWellHeadPosition, FPestWellHeadFormula);
end;

procedure TMawBoundary.SetPestWellHeadMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestWellHeadMethod, Value);
end;

procedure TMawBoundary.SetRadius(const Value: string);
begin
  UpdateFormulaBlocks(Value, RadiusPosition, FRadius);
end;

procedure TMawBoundary.SetStartingConcentrations(
  const Value: TStringConcCollection);
begin
  FStartingConcentrations.Assign(Value);
end;

procedure TMawBoundary.SetWellNumber(const Value: Integer);
begin
  if FWellNumber <> Value then
  begin
    FWellNumber := Value;
    InvalidateModel;
  end;
end;

procedure TMawBoundary.SetWellScreens(const Value: TMawWellScreenCollection);
begin
  FWellScreens.Assign(Value);
end;

{ TMawTransientRecord }

procedure TMawTransientRecord.Assign(const Item: TMawTransientRecord);
begin
  self := Item;
  SetLength(GwtStatus, Length(GwtStatus));
  SpecifiedConcentrations.Assign(Item.SpecifiedConcentrations);
  InjectionConcentrations.Assign(Item.InjectionConcentrations);
  Density.Assign(Item.Density);
  Viscosity.Assign(Item.Viscosity);
end;

procedure TMawTransientRecord.Cache(Comp: TCompressionStream;
  Strings: TStringList);
var
  GwtStatusCount: Integer;
  SpeciesIndex: Integer;
begin
  WriteCompCell(Comp, Cell);
  WriteCompInt(Comp, WellNumber);
  WriteCompInt(Comp, Ord(MawStatus));
  WriteCompInt(Comp, Ord(FlowingWell));
//  WriteCompInt(Comp, Ord(RateLimitation);
  WriteCompBoolean(Comp, ShutOff);
  WriteCompBoolean(Comp, RateScaling);
  WriteCompBoolean(Comp, HeadLimitChoice);

  WriteCompReal(Comp, FlowingWellElevation);
  WriteCompReal(Comp, FlowingWellConductance);
  WriteCompReal(Comp, FlowingWellReductionLength);
  WriteCompReal(Comp, Rate);
  WriteCompReal(Comp, WellHead);
  WriteCompReal(Comp, HeadLimit);
  WriteCompReal(Comp, MinRate);
  WriteCompReal(Comp, MaxRate);
  WriteCompReal(Comp, PumpElevation);
  WriteCompReal(Comp, ScalingLength);

  WriteCompInt(Comp, Strings.IndexOf(FlowingWellElevationAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(FlowingWellConductanceAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(FlowingWellReductionLengthAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(RateAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(WellHeadAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(HeadLimitAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(MinRateAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(MaxRateAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(PumpElevationAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(ScalingLengthAnnotation));

  WriteCompInt(Comp, Strings.IndexOf(FlowingWellElevationPest));
  WriteCompInt(Comp, Strings.IndexOf(FlowingWellConductancePest));
  WriteCompInt(Comp, Strings.IndexOf(FlowingWellReductionLengthPest));
  WriteCompInt(Comp, Strings.IndexOf(RatePest));
  WriteCompInt(Comp, Strings.IndexOf(WellHeadPest));
  WriteCompInt(Comp, Strings.IndexOf(HeadLimitPest));
  WriteCompInt(Comp, Strings.IndexOf(MinRatePest));
  WriteCompInt(Comp, Strings.IndexOf(MaxRatePest));
  WriteCompInt(Comp, Strings.IndexOf(PumpElevationPest));
  WriteCompInt(Comp, Strings.IndexOf(ScalingLengthPest));

  WriteCompInt(Comp, Strings.IndexOf(FlowingWellElevationPestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(FlowingWellConductancePestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(FlowingWellReductionLengthPestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(RatePestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(WellHeadPestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(HeadLimitPestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(MinRatePestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(MaxRatePestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(PumpElevationPestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(ScalingLengthPestSeriesName));

  WriteCompInt(Comp, Ord(FlowingWellElevationPestSeriesMethod));
  WriteCompInt(Comp, Ord(FlowingWellConductancePestSeriesMethod));
  WriteCompInt(Comp, Ord(FlowingWellReductionLengthPestSeriesMethod));
  WriteCompInt(Comp, Ord(RatePestSeriesMethod));
  WriteCompInt(Comp, Ord(WellHeadPestSeriesMethod));
  WriteCompInt(Comp, Ord(HeadLimitPestSeriesMethod));
  WriteCompInt(Comp, Ord(MinRatePestSeriesMethod));
  WriteCompInt(Comp, Ord(MaxRatePestSeriesMethod));
  WriteCompInt(Comp, Ord(PumpElevationPestSeriesMethod));
  WriteCompInt(Comp, Ord(ScalingLengthPestSeriesMethod));

  WriteCompInt(Comp, Strings.IndexOf(RateTimeSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(WellHeadTimeSeriesName));

  WriteCompBoolean(Comp, MvrUsed);
  WriteCompInt(Comp, MvrIndex);

  // GWT

  SpecifiedConcentrations.Cache(Comp, Strings);
  InjectionConcentrations.Cache(Comp, Strings);
  Density.Cache(Comp, Strings);
  Viscosity.Cache(Comp, Strings);

  GwtStatusCount := Length(GwtStatus);
  WriteCompInt(Comp, GwtStatusCount);
  for SpeciesIndex := 0 to GwtStatusCount - 1 do
  begin
    WriteCompInt(Comp, Ord(GwtStatus[SpeciesIndex]));
  end;

end;

procedure TMawTransientRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(FlowingWellElevationAnnotation);
  Strings.Add(FlowingWellConductanceAnnotation);
  Strings.Add(FlowingWellReductionLengthAnnotation);
  Strings.Add(RateAnnotation);
  Strings.Add(WellHeadAnnotation);
  Strings.Add(HeadLimitAnnotation);
  Strings.Add(MinRateAnnotation);
  Strings.Add(MaxRateAnnotation);
  Strings.Add(PumpElevationAnnotation);
  Strings.Add(ScalingLengthAnnotation);

  Strings.Add(FlowingWellElevationPest);
  Strings.Add(FlowingWellConductancePest);
  Strings.Add(FlowingWellReductionLengthPest);
  Strings.Add(RatePest);
  Strings.Add(WellHeadPest);
  Strings.Add(HeadLimitPest);
  Strings.Add(MinRatePest);
  Strings.Add(MaxRatePest);
  Strings.Add(PumpElevationPest);
  Strings.Add(ScalingLengthPest);

  Strings.Add(FlowingWellElevationPestSeriesName);
  Strings.Add(FlowingWellConductancePestSeriesName);
  Strings.Add(FlowingWellReductionLengthPestSeriesName);
  Strings.Add(RatePestSeriesName);
  Strings.Add(WellHeadPestSeriesName);
  Strings.Add(HeadLimitPestSeriesName);
  Strings.Add(MinRatePestSeriesName);
  Strings.Add(MaxRatePestSeriesName);
  Strings.Add(PumpElevationPestSeriesName);
  Strings.Add(ScalingLengthPestSeriesName);

  Strings.Add(RateTimeSeriesName);
  Strings.Add(WellHeadTimeSeriesName);

  // GWT
  SpecifiedConcentrations.RecordStrings(Strings);
  InjectionConcentrations.RecordStrings(Strings);
  Density.RecordStrings(Strings);
  Viscosity.RecordStrings(Strings);
end;

procedure TMawTransientRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
var
  GwtStatusCount: Integer;
  SpeciesIndex: Integer;
begin
  Cell := ReadCompCell(Decomp);

  WellNumber := ReadCompInt(Decomp);
  MawStatus := TMawStatus(ReadCompInt(Decomp));
  FlowingWell := TFlowingWell(ReadCompInt(Decomp));
//  RateLimitation := TRateLimitation(ReadCompInt(Decomp));
  ShutOff := ReadCompBoolean(Decomp);
  RateScaling := ReadCompBoolean(Decomp);
  HeadLimitChoice := ReadCompBoolean(Decomp);

  FlowingWellElevation := ReadCompReal(Decomp);
  FlowingWellConductance := ReadCompReal(Decomp);
  FlowingWellReductionLength := ReadCompReal(Decomp);
  Rate := ReadCompReal(Decomp);
  WellHead := ReadCompReal(Decomp);
  HeadLimit := ReadCompReal(Decomp);
  MinRate := ReadCompReal(Decomp);
  MaxRate := ReadCompReal(Decomp);
  PumpElevation := ReadCompReal(Decomp);
  ScalingLength := ReadCompReal(Decomp);

  FlowingWellElevationAnnotation := Annotations[ReadCompInt(Decomp)];
  FlowingWellConductanceAnnotation := Annotations[ReadCompInt(Decomp)];
  FlowingWellReductionLengthAnnotation := Annotations[ReadCompInt(Decomp)];
  RateAnnotation := Annotations[ReadCompInt(Decomp)];
  WellHeadAnnotation := Annotations[ReadCompInt(Decomp)];
  HeadLimitAnnotation := Annotations[ReadCompInt(Decomp)];
  MinRateAnnotation := Annotations[ReadCompInt(Decomp)];
  MaxRateAnnotation := Annotations[ReadCompInt(Decomp)];
  PumpElevationAnnotation := Annotations[ReadCompInt(Decomp)];
  ScalingLengthAnnotation := Annotations[ReadCompInt(Decomp)];

  FlowingWellElevationPest := Annotations[ReadCompInt(Decomp)];
  FlowingWellConductancePest := Annotations[ReadCompInt(Decomp)];
  FlowingWellReductionLengthPest := Annotations[ReadCompInt(Decomp)];
  RatePest := Annotations[ReadCompInt(Decomp)];
  WellHeadPest := Annotations[ReadCompInt(Decomp)];
  HeadLimitPest := Annotations[ReadCompInt(Decomp)];
  MinRatePest := Annotations[ReadCompInt(Decomp)];
  MaxRatePest := Annotations[ReadCompInt(Decomp)];
  PumpElevationPest := Annotations[ReadCompInt(Decomp)];
  ScalingLengthPest := Annotations[ReadCompInt(Decomp)];

  FlowingWellElevationPestSeriesName := Annotations[ReadCompInt(Decomp)];
  FlowingWellConductancePestSeriesName := Annotations[ReadCompInt(Decomp)];
  FlowingWellReductionLengthPestSeriesName := Annotations[ReadCompInt(Decomp)];
  RatePestSeriesName := Annotations[ReadCompInt(Decomp)];
  WellHeadPestSeriesName := Annotations[ReadCompInt(Decomp)];
  HeadLimitPestSeriesName := Annotations[ReadCompInt(Decomp)];
  MinRatePestSeriesName := Annotations[ReadCompInt(Decomp)];
  MaxRatePestSeriesName := Annotations[ReadCompInt(Decomp)];
  PumpElevationPestSeriesName := Annotations[ReadCompInt(Decomp)];
  ScalingLengthPestSeriesName := Annotations[ReadCompInt(Decomp)];

  FlowingWellElevationPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  FlowingWellConductancePestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  FlowingWellReductionLengthPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  RatePestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  WellHeadPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  HeadLimitPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  MinRatePestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  MaxRatePestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  PumpElevationPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  ScalingLengthPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));

  RateTimeSeriesName := Annotations[ReadCompInt(Decomp)];
  WellHeadTimeSeriesName := Annotations[ReadCompInt(Decomp)];

  MvrUsed := ReadCompBoolean(Decomp);
  MvrIndex := ReadCompInt(Decomp);

  // GWT
  SpecifiedConcentrations.Restore(Decomp, Annotations);
  InjectionConcentrations.Restore(Decomp, Annotations);
  // Buoyancy
  Density.Restore(Decomp, Annotations);
  // Viscosity
  Viscosity.Restore(Decomp, Annotations);

  GwtStatusCount := ReadCompInt(Decomp);
  SetLength(GwtStatus, GwtStatusCount);
  for SpeciesIndex := 0 to GwtStatusCount - 1 do
  begin
    GwtStatus[SpeciesIndex] := TGwtBoundaryStatus(ReadCompInt(Decomp));
  end;

end;

{ TMawTransientStorage }

procedure TMawTransientStorage.Clear;
begin
  SetLength(FMawTransientArray, 0);
  FCleared := True;
end;

function TMawTransientStorage.GetMawTransientArray: TMawTransientArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FMawTransientArray;
end;

procedure TMawTransientStorage.Restore(
  DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FMawTransientArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FMawTransientArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TMawTransientStorage.SetSpeciesCount(const Value: Integer);
var
  Index: Integer;
begin
  FSpeciesCount := Value;
  for Index := 0 to Length(FMawTransientArray)-1 do
  begin
    SetLength(FMawTransientArray[Index].GwtStatus, FSpeciesCount);
    FMawTransientArray[Index].SpecifiedConcentrations.SpeciesCount := FSpeciesCount;
    FMawTransientArray[Index].InjectionConcentrations.SpeciesCount := FSpeciesCount;
    FMawTransientArray[Index].Density.SpeciesCount := 1;
    FMawTransientArray[Index].Viscosity.SpeciesCount := 1;
  end;
end;

procedure TMawTransientStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    InitializeStrings(Strings);
    Count := Length(FMawTransientArray);
    for Index := 0 to Count - 1 do
    begin
      FMawTransientArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FMawTransientArray[Index].Cache(Compressor, Strings);
    end;
  finally
    Strings.Free;
  end;
end;

{ TMawSteadyWellRecord }

procedure TMawSteadyWellRecord.Assign(const Item: TMawSteadyWellRecord);
begin
  self := Item;
  self.StartingConcentrations.Assign(Item.StartingConcentrations);
end;

procedure TMawSteadyWellRecord.Cache(Comp: TCompressionStream;
  Strings: TStringList);
begin
  WriteCompInt(Comp, WellNumber);

  WriteCompReal(Comp, Radius);
  WriteCompReal(Comp, Bottom);
  WriteCompReal(Comp, StartingHead);
  WriteCompInt(Comp, Ord(ConductanceMethod));
  WriteCompInt(Comp, CellCount);

  WriteCompInt(Comp, Column);
  WriteCompInt(Comp, Row);
  WriteCompInt(Comp, Layer);

  WriteCompInt(Comp, Strings.IndexOf(RadiusAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(BottomAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(StartingHeadAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(BoundName));
  WriteCompInt(Comp, Strings.IndexOf(ScreenObjectName));

  WriteCompInt(Comp, Strings.IndexOf(RadiusPestName));
  WriteCompInt(Comp, Strings.IndexOf(BottomPestName));
  WriteCompInt(Comp, Strings.IndexOf(StartingHeadPestName));

  StartingConcentrations.Cache(Comp, Strings);
end;

function TMawSteadyWellRecord.GetBoundaryAnnotation(Index: Integer): string;
begin
  case Index of
    MawRadiusPosition:
      begin
        result := RadiusAnnotation;
      end;
    MawBottomPosition:
      begin
        result := BottomAnnotation;
      end;
    MawStartingHeadPosition:
      begin
        result := StartingHeadAnnotation;
      end;
    else
      begin
        // GWT
        Index := Index - MawGwtSteadyStart;
        result := StartingConcentrations.ValueAnnotations[Index]
      end;
  end
end;

function TMawSteadyWellRecord.GetBoundaryValue(Index: Integer): double;
begin
  case Index of
    MawRadiusPosition:
      begin
        result := Radius;
      end;
    MawBottomPosition:
      begin
        result := Bottom;
      end;
    MawStartingHeadPosition:
      begin
        result := StartingHead;
      end;
    else
      begin
        // GWT
        Index := Index - MawGwtSteadyStart;
        result := StartingConcentrations.Values[Index]
      end;
  end
end;

function TMawSteadyWellRecord.GetPestParamName(Index: Integer): string;
begin
  case Index of
    MawRadiusPosition:
      begin
        result := RadiusPestName;
      end;
    MawBottomPosition:
      begin
        result := BottomPestName;
      end;
    MawStartingHeadPosition:
      begin
        result := StartingHeadPestName;
      end;
    else
      begin
        // GWT
        Index := Index - MawGwtSteadyStart;
        result := StartingConcentrations.ValuePestNames[Index];
      end;
  end
end;

procedure TMawSteadyWellRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(RadiusAnnotation);
  Strings.Add(BottomAnnotation);
  Strings.Add(StartingHeadAnnotation);
  Strings.Add(BoundName);
  Strings.Add(ScreenObjectName);
  Strings.Add(RadiusPestName);
  Strings.Add(BottomPestName);
  Strings.Add(StartingHeadPestName);

  StartingConcentrations.RecordStrings(Strings);
end;

procedure TMawSteadyWellRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  WellNumber := ReadCompInt(Decomp);
  Radius := ReadCompReal(Decomp);
  Bottom := ReadCompReal(Decomp);
  StartingHead := ReadCompReal(Decomp);
  ConductanceMethod := TMawConductanceMethod(ReadCompInt(Decomp));
  CellCount := ReadCompInt(Decomp);

  Column := ReadCompInt(Decomp);
  Row := ReadCompInt(Decomp);
  Layer := ReadCompInt(Decomp);

  RadiusAnnotation := Annotations[ReadCompInt(Decomp)];
  BottomAnnotation := Annotations[ReadCompInt(Decomp)];
  StartingHeadAnnotation := Annotations[ReadCompInt(Decomp)];
  BoundName := Annotations[ReadCompInt(Decomp)];
  ScreenObjectName := Annotations[ReadCompInt(Decomp)];

  RadiusPestName := Annotations[ReadCompInt(Decomp)];
  BottomPestName := Annotations[ReadCompInt(Decomp)];
  StartingHeadPestName := Annotations[ReadCompInt(Decomp)];

  StartingConcentrations.Restore(Decomp, Annotations);
end;

procedure TMawSteadyWellRecord.SetBoundaryAnnotation(Index: Integer;
  const Value: string);
begin
  case Index of
    MawRadiusPosition:
      begin
        RadiusAnnotation := Value;
      end;
    MawBottomPosition:
      begin
        BottomAnnotation := Value;
      end;
    MawStartingHeadPosition:
      begin
        StartingHeadAnnotation := Value;
      end;
    else
      begin
        // GWT
        Index := Index - MawGwtSteadyStart;
        StartingConcentrations.ValueAnnotations[Index] := Value;
      end;
  end
end;

procedure TMawSteadyWellRecord.SetBoundaryValue(Index: Integer;
  const Value: double);
begin
  case Index of
    MawRadiusPosition:
      begin
        Radius := Value;
      end;
    MawBottomPosition:
      begin
        Bottom := Value;
      end;
    MawStartingHeadPosition:
      begin
        StartingHead := Value;
      end;
    else
      begin
        // GWT
        Index := Index - MawGwtSteadyStart;
        StartingConcentrations.Values[Index] := Value;
      end;
  end

//  MawRadiusPosition = 0;
//  MawBottomPosition = 1;
//  MawStartingHeadPosition = 2;
//  MawGwtSteadyStart = 3;

end;

procedure TMawSteadyWellRecord.SetPestParamName(Index: Integer;
  const Value: string);
begin
  case Index of
    MawRadiusPosition:
      begin
        RadiusPestName := Value;
      end;
    MawBottomPosition:
      begin
        BottomPestName := Value;
      end;
    MawStartingHeadPosition:
      begin
        StartingHeadPestName := Value;
      end;
    else
      begin
        // GWT
        Index := Index - MawGwtSteadyStart;
        StartingConcentrations.ValuePestNames[Index] := Value;
      end;
  end
end;

{ TMawItem }

procedure TMawItem.Assign(Source: TPersistent);
var
  MawSource: TMawItem;
  Mnw2Source: TMnw2TimeItem;
begin
  if Source is TMawItem then
  begin
    MawSource := TMawItem(Source);
//    WellNumber := MawSource.WellNumber;
    MawStatus := MawSource.MawStatus;
    FlowingWell := MawSource.FlowingWell;
    RateLimitation := MawSource.RateLimitation;
//    Shutoff := MawSource.Shutoff;
//    RateScaling := MawSource.RateScaling;
    HeadLimitChoice := MawSource.HeadLimitChoice;
    FlowingWellElevation := MawSource.FlowingWellElevation;
    FlowingWellConductance := MawSource.FlowingWellConductance;
    FlowingWellReductionLength := MawSource.FlowingWellReductionLength;
    Rate := MawSource.Rate;
    WellHead := MawSource.WellHead;
    HeadLimit := MawSource.HeadLimit;
    MinRate := MawSource.MinRate;
    MaxRate := MawSource.MaxRate;
    PumpElevation := MawSource.PumpElevation;
    ScalingLength := MawSource.ScalingLength;
    GwtStatus := MawSource.GwtStatus;
    SpecifiedConcentrations := MawSource.SpecifiedConcentrations;
    InjectionConcentrations := MawSource.InjectionConcentrations;
    Density := MawSource.Density;
    Viscosity := MawSource.Viscosity;
  end
  else
  if Source is TMnw2TimeItem then
  begin
    Mnw2Source := TMnw2TimeItem(Source);

    MawStatus := mwActive;
    FlowingWell := fwNotFlowing;
    RateLimitation := rlNone;
    HeadLimitChoice := False;
    FlowingWellElevation := '0';
    FlowingWellConductance := '0';
    Rate := Mnw2Source.PumpingRate;

    WellHead := '0';
    HeadLimit := Mnw2Source.LimitingWaterLevel;
    MinRate := Mnw2Source.InactivationPumpingRate;
    MaxRate := Mnw2Source.ReactivationPumpingRate;
    PumpElevation := '0';
    ScalingLength := '0';

{
    // QDes
    property PumpingRate: string read GetPumpingRate write SetPumpingRate;
    // CapMult
    property HeadCapacityMultiplier: string read GetHeadCapacityMultiplier
      write SetHeadCapacityMultiplier;
    // Hlim
    property LimitingWaterLevel: string read GetLimitingWaterLevel
      write SetLimitingWaterLevel;
    // Qfrcmn
    property InactivationPumpingRate: string read GetInactivationPumpingRate
      write SetInactivationPumpingRate;
    // Qfrcmx
    property ReactivationPumpingRate: string read GetReactivationPumpingRate
      write SetReactivationPumpingRate;
    // QCut
    property LimitMethod: TMnwLimitMethod read FLimitMethod
      write SetLimitMethod;
}

  end;
  inherited;

end;

procedure TMawItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TMawWellCollection;
  AnObserver: TObserver;
  ConcIndex: Integer;
  DensityIndex: Integer;
  ViscosityIndex: Integer;
begin
  ParentCollection := Collection as TMawWellCollection;

  AnObserver := FObserverList[MawFlowingWellElevationPosition];
  AnObserver.OnUpToDateSet := ParentCollection.InvalidateFlowingWellElevationData;

  AnObserver := FObserverList[MawFlowingWellConductancePosition];
  AnObserver.OnUpToDateSet := ParentCollection.InvalidateFlowingWellConductanceData;

  AnObserver := FObserverList[MawRatePosition];
  AnObserver.OnUpToDateSet := ParentCollection.InvalidateRateData;

  AnObserver := FObserverList[MawWellHeadPosition];
  AnObserver.OnUpToDateSet := ParentCollection.InvalidateWellHeadData;

  AnObserver := FObserverList[MawHeadLimitPosition];
  AnObserver.OnUpToDateSet := ParentCollection.InvalidateHeadLimitData;

  AnObserver := FObserverList[MawMinRatePosition];
  AnObserver.OnUpToDateSet := ParentCollection.InvalidateMinRateData;

  AnObserver := FObserverList[MawMaxRatePosition];
  AnObserver.OnUpToDateSet := ParentCollection.InvalidateMaxRateData;

  AnObserver := FObserverList[MawPumpElevationPosition];
  AnObserver.OnUpToDateSet := ParentCollection.InvalidatePumpElevationData;

  AnObserver := FObserverList[MawScalingLengthPosition];
  AnObserver.OnUpToDateSet := ParentCollection.InvalidateScalingLengthData;

  AnObserver := FObserverList[MawFlowingWellReductionLengthPosition];
  AnObserver.OnUpToDateSet := ParentCollection.InvalidateFlowingWellReductionLengthData;

  for DensityIndex := 0 to Density.Count - 1 do
  begin
    Density[DensityIndex].Observer.OnUpToDateSet
      := ParentCollection.InvalidateDensity;
  end;
  for ViscosityIndex := 0 to Viscosity.Count - 1 do
  begin
    Viscosity[ViscosityIndex].Observer.OnUpToDateSet
      := ParentCollection.InvalidateViscosity;
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

  for ConcIndex := 0 to InjectionConcentrations.Count - 1 do
  begin
    InjectionConcentrations[ConcIndex].Observer.OnUpToDateSet
      := ParentCollection.InvalidateInjectionConcentrations;
  end;
end;

function TMawItem.BoundaryFormulaCount: integer;
begin
  Result := Succ(MawViscosityPosition);
  if frmGoPhast.PhastModel.GwtUsed then
  begin
    result := result + frmGoPhast.PhastModel.MobileComponents.Count*2;
  end;
end;

constructor TMawItem.Create(Collection: TCollection);
var
  MawCollection: TMawWellCollection;
begin
  MawCollection := Collection as TMawWellCollection;

  FSpecifiedConcentrations := TMawGwtConcCollection.Create(Model as TCustomModel, ScreenObject,
    MawCollection);
  FInjectionConcentrations := TMawGwtConcCollection.Create(Model as TCustomModel, ScreenObject,
    MawCollection);
  FDensity := TMawGwtConcCollection.Create(Model as TCustomModel, ScreenObject,
    MawCollection);
  FViscosity := TMawGwtConcCollection.Create(Model as TCustomModel, ScreenObject,
    MawCollection);

  inherited;
  FGwtStatus := TGwtBoundaryStatusCollection.Create(Model as TCustomModel);

  FMawStatus := mwActive;
  FFlowingWell := fwNotFlowing;
  FRateLimitation := rlNone;
  FHeadLimitChoice := False;
  FlowingWellReductionLength := '1';
end;

procedure TMawItem.CreateFormulaObjects;
begin
  inherited;
  FFlowingWellConductance := CreateFormulaObject(dso3D);
  FFlowingWellElevation := CreateFormulaObject(dso3D);
  FHeadLimit := CreateFormulaObject(dso3D);
  FMaxRate := CreateFormulaObject(dso3D);
  FMinRate := CreateFormulaObject(dso3D);
  FPumpElevation := CreateFormulaObject(dso3D);
  FRate := CreateFormulaObject(dso3D);
  FScalingLength := CreateFormulaObject(dso3D);
  FWellHead := CreateFormulaObject(dso3D);
  FFlowingWellReductionLength := CreateFormulaObject(dso3D);

end;

destructor TMawItem.Destroy;
var
 Index: Integer;
begin
  FGwtStatus.Free;
  for Index := 0 to FSpecifiedConcentrations.Count - 1 do
  begin
    FSpecifiedConcentrations[Index].Value := '0';
  end;
  FSpecifiedConcentrations.Free;

  for Index := 0 to FInjectionConcentrations.Count - 1 do
  begin
    FInjectionConcentrations[Index].Value := '0';
  end;
  FInjectionConcentrations.Free;

  for Index := 0 to FDensity.Count - 1 do
  begin
    FDensity[Index].Value := '0';
  end;
  FDensity.Free;

  for Index := 0 to FViscosity.Count - 1 do
  begin
    FViscosity[Index].Value := '0';
  end;
  FViscosity.Free;

  Rate := '0';
  WellHead := '0';
  FlowingWellElevation := '0';
  FlowingWellConductance := '0';
  FlowingWellReductionLength := '0';
  MinRate := '0';
  MaxRate := '0';
  PumpElevation := '0';
  ScalingLength := '0';
  HeadLimit := '0';

  inherited;
end;

function TMawItem.GetBoundaryFormula(Index: integer): string;
var
  ChemSpeciesCount: Integer;
begin
  case index of
    MawFlowingWellElevationPosition:
      result := FlowingWellElevation;
    MawFlowingWellConductancePosition:
      result := FlowingWellConductance;
    MawRatePosition:
      result := Rate;
    MawWellHeadPosition:
      result := WellHead;
    MawHeadLimitPosition:
      result := HeadLimit;
    MawMinRatePosition:
      result := MinRate;
    MawMaxRatePosition:
      result := MaxRate;
    MawPumpElevationPosition:
      result := PumpElevation;
    MawScalingLengthPosition:
      result := ScalingLength;
    MawFlowingWellReductionLengthPosition:
      result := FlowingWellReductionLength;
    MawDensityPosition:
      begin
        result := '0';
        if frmGoPhast.PhastModel.BuoyancyDensityUsed then
        begin
          if Density.Count < 1 then
          begin
            Density.Add
          end;
          result := Density[0].Value;
        end;
      end;
    MawViscosityPosition:
      begin
        result := '0';
        if frmGoPhast.PhastModel.ViscosityPkgViscUsed then
        begin
          if Viscosity.Count < 1 then
          begin
            Viscosity.Add
          end;
          result := Viscosity[0].Value;
        end;
      end;
    else
      begin
        // GWT
        result := '0';
        if frmGoPhast.PhastModel.GwtUsed then
        begin
          Index := Index-MawGwtStart;
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

          while InjectionConcentrations.Count < ChemSpeciesCount do
          begin
            InjectionConcentrations.Add;
          end;
          if Index < ChemSpeciesCount then
          begin
            result := InjectionConcentrations[Index].Value;
            Exit;
          end;
          Assert(False);
//          Index := Index - ChemSpeciesCount;
        end
        else
        begin
          Assert(False);
        end;

      end;
  end;
end;

function TMawItem.GetFlowingWellConductance: string;
begin
  Result := FFlowingWellConductance.Formula;
  ResetItemObserver(MawFlowingWellConductancePosition);
end;

function TMawItem.GetFlowingWellElevation: string;
begin
  Result := FFlowingWellElevation.Formula;
  ResetItemObserver(MawFlowingWellElevationPosition);
end;

function TMawItem.GetFlowingWellReductionLength: string;
begin
  Result := FFlowingWellReductionLength.Formula;
  ResetItemObserver(MawFlowingWellReductionLengthPosition);
end;

function TMawItem.GetHeadLimit: string;
begin
  Result := FHeadLimit.Formula;
  ResetItemObserver(MawHeadLimitPosition);
end;

function TMawItem.GetMaxRate: string;
begin
  Result := FMaxRate.Formula;
  ResetItemObserver(MawMaxRatePosition);
end;

function TMawItem.GetMinRate: string;
begin
  Result := FMinRate.Formula;
  ResetItemObserver(MawMinRatePosition);
end;

procedure TMawItem.GetPropertyObserver(Sender: TObject; List: TList);
var
  ConcIndex: Integer;
  Item: TGwtConcStringValueItem;
  DensityIndex: Integer;
  ViscosityIndex: Integer;
begin
  inherited;
  if Sender = FFlowingWellConductance as TObject then
  begin
    List.Add(FObserverList[MawFlowingWellConductancePosition]);
  end
  else if Sender = FFlowingWellElevation as TObject then
  begin
    List.Add(FObserverList[MawFlowingWellElevationPosition]);
  end
  else if Sender = FFlowingWellReductionLength as TObject then
  begin
    List.Add(FObserverList[MawFlowingWellReductionLengthPosition]);
  end
  else if Sender = FHeadLimit as TObject then
  begin
    List.Add(FObserverList[MawHeadLimitPosition]);
  end
  else if Sender = FMaxRate as TObject then
  begin
    List.Add(FObserverList[MawMaxRatePosition]);
  end
  else if Sender = FMinRate as TObject then
  begin
    List.Add(FObserverList[MawMinRatePosition]);
  end
  else if Sender = FPumpElevation as TObject then
  begin
    List.Add(FObserverList[MawPumpElevationPosition]);
  end
  else if Sender = FRate as TObject then
  begin
    List.Add(FObserverList[MawRatePosition]);
  end
  else if Sender = FScalingLength as TObject then
  begin
    List.Add(FObserverList[MawScalingLengthPosition]);
  end
  else if Sender = FWellHead as TObject then
  begin
    List.Add(FObserverList[MawWellHeadPosition]);
  end;
  // Buoyancy
  for DensityIndex := 0 to Density.Count - 1 do
  begin
    Item := Density.Items[DensityIndex];
    if Item.ValueObject as TObject = Sender then
    begin
      List.Add(Item.Observer);
    end;
  end;
  // Viscosity
  for ViscosityIndex := 0 to Viscosity.Count - 1 do
  begin
    Item := Viscosity.Items[ViscosityIndex];
    if Item.ValueObject as TObject = Sender then
    begin
      List.Add(Item.Observer);
    end;
  end;
  // GWT
  for ConcIndex := 0 to SpecifiedConcentrations.Count - 1 do
  begin
    Item := SpecifiedConcentrations.Items[ConcIndex];
    if Item.ValueObject as TObject = Sender then
    begin
      List.Add(Item.Observer);
    end;
  end;

  for ConcIndex := 0 to InjectionConcentrations.Count - 1 do
  begin
    Item := InjectionConcentrations.Items[ConcIndex];
    if Item.ValueObject as TObject = Sender then
    begin
      List.Add(Item.Observer);
    end;
  end;
end;

function TMawItem.GetPumpElevation: string;
begin
  Result := FPumpElevation.Formula;
  ResetItemObserver(MawPumpElevationPosition);
end;

function TMawItem.GetRate: string;
begin
  FRate.ScreenObject := ScreenObjectI;
  try
    Result := FRate.Formula;
  finally
    FRate.ScreenObject := nil;
  end;
  ResetItemObserver(MawRatePosition);
end;

function TMawItem.GetRateScaling: Boolean;
begin
  result := RateLimitation = rlScaling;
end;

function TMawItem.GetScalingLength: string;
begin
  Result := FScalingLength.Formula;
  ResetItemObserver(MawScalingLengthPosition);
end;

function TMawItem.GetShutoff: Boolean;
begin
  result := RateLimitation = rlShutoff;
end;

function TMawItem.GetWellHead: string;
begin
  FWellHead.ScreenObject := ScreenObjectI;
  try
    Result := FWellHead.Formula;
  finally
    FWellHead.ScreenObject := nil;
  end;
  ResetItemObserver(MawWellHeadPosition);
end;

procedure TMawItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMawFlowingWellElevation(self);
    PhastModel.InvalidateMawFlowingWellConductance(self);
    PhastModel.InvalidateMawFlowingWellReductionLength(self);
    PhastModel.InvalidateMawWell_Rate(self);
    PhastModel.InvalidateMawWell_Head(self);
    PhastModel.InvalidateMawWell_Limit(self);
    PhastModel.InvalidateMawMinimumPumpRate(self);
    PhastModel.InvalidateMawMaximumPumpRate(self);
    PhastModel.InvalidateMawPumpElevation(self);
    PhastModel.InvalidateMawScalingLength(self);
  end;
end;

function TMawItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  SourceItem: TMawItem;
begin
  result := inherited IsSame(AnotherItem) and (AnotherItem is TMawItem);
  if result then
  begin
    SourceItem := TMawItem(AnotherItem);
    Result := (MawStatus = SourceItem.MawStatus)
      and (FlowingWell = SourceItem.FlowingWell)
      and (RateLimitation = SourceItem.RateLimitation)
      and (FlowingWellElevation = SourceItem.FlowingWellElevation)
      and (FlowingWellConductance = SourceItem.FlowingWellConductance)
      and (FlowingWellReductionLength = SourceItem.FlowingWellReductionLength)
      and (Rate = SourceItem.Rate)
      and (WellHead = SourceItem.WellHead)
      and (HeadLimit = SourceItem.HeadLimit)
      and (MinRate = SourceItem.MinRate)
      and (MaxRate = SourceItem.MaxRate)
      and (PumpElevation = SourceItem.PumpElevation)
      and (ScalingLength = SourceItem.ScalingLength)
      and (HeadLimitChoice = SourceItem.HeadLimitChoice)
      and SpecifiedConcentrations.IsSame(SourceItem.SpecifiedConcentrations)
      and InjectionConcentrations.IsSame(SourceItem.InjectionConcentrations)
      and Density.IsSame(SourceItem.Density)
      and Viscosity.IsSame(SourceItem.Viscosity)
      and GwtStatus.IsSame(SourceItem.GwtStatus);
  end;
end;

procedure TMawItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FFlowingWellConductance,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FFlowingWellElevation,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FHeadLimit,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FMaxRate,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FMinRate,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FPumpElevation,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRate,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FScalingLength,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FWellHead,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FFlowingWellReductionLength,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TMawItem.SetBoundaryFormula(Index: integer; const Value: string);
var
  ChemSpeciesCount: Integer;
begin
  case index of
    MawFlowingWellElevationPosition:
      FlowingWellElevation := Value;
    MawFlowingWellConductancePosition:
      FlowingWellConductance := Value;
    MawRatePosition:
      Rate := Value;
    MawWellHeadPosition:
      WellHead := Value;
    MawHeadLimitPosition:
      HeadLimit := Value;
    MawMinRatePosition:
      MinRate := Value;
    MawMaxRatePosition:
      MaxRate := Value;
    MawPumpElevationPosition:
      PumpElevation := Value;
    MawScalingLengthPosition:
      ScalingLength := Value;
    MawFlowingWellReductionLengthPosition:
      FlowingWellReductionLength := Value;
    MawDensityPosition:
      begin
        if frmGoPhast.PhastModel.BuoyancyDensityUsed then
        begin
          if Density.Count < 1 then
          begin
            Density.Add;
          end;
          Density[0].Value := Value;
        end;
      end;
    MawViscosityPosition:
      begin
        if frmGoPhast.PhastModel.ViscosityPkgViscUsed then
        begin
          if Viscosity.Count < 1 then
          begin
            Viscosity.Add;
          end;
          Viscosity[0].Value := Value;
        end;
      end;
    else
      begin
        // GWT
        if frmGoPhast.PhastModel.GwtUsed then
        begin
          Index := Index-MawGwtStart;
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

          while InjectionConcentrations.Count < ChemSpeciesCount do
          begin
            InjectionConcentrations.Add;
          end;
          if Index < ChemSpeciesCount then
          begin
            InjectionConcentrations[Index].Value := Value;
            Exit;
          end;
          Assert(False);
        end
        else
        begin
          Assert(False);
        end;
      end;
  end;
end;

procedure TMawItem.SetDensity(const Value: TMawGwtConcCollection);
begin
  FDensity.Assign(Value);
end;

procedure TMawItem.SetFlowingWell(const Value: TFlowingWell);
begin
  FFlowingWell := Value;
end;

procedure TMawItem.SetFlowingWellConductance(const Value: string);
begin
  UpdateFormulaBlocks(Value, MawFlowingWellConductancePosition, FFlowingWellConductance);
end;

procedure TMawItem.SetFlowingWellElevation(const Value: string);
begin
  UpdateFormulaBlocks(Value, MawFlowingWellElevationPosition, FFlowingWellElevation);
end;

procedure TMawItem.SetFlowingWellReductionLength(const Value: string);
begin
  UpdateFormulaBlocks(Value, MawFlowingWellReductionLengthPosition,
    FFlowingWellReductionLength);
end;

procedure TMawItem.SetGwtStatus(const Value: TGwtBoundaryStatusCollection);
begin
  FGwtStatus.Assign(Value);
end;

procedure TMawItem.SetHeadLimit(const Value: string);
begin
  UpdateFormulaBlocks(Value, MawHeadLimitPosition, FHeadLimit);
end;

procedure TMawItem.SetHeadLimitChoice(const Value: Boolean);
begin
  FHeadLimitChoice := Value;
end;

procedure TMawItem.SetInjectionConcentrations(
  const Value: TMawGwtConcCollection);
begin
  FInjectionConcentrations.Assign(Value);
end;

procedure TMawItem.SetMawStatus(const Value: TMawStatus);
begin
  FMawStatus := Value;
end;

procedure TMawItem.SetMaxRate(const Value: string);
begin
  UpdateFormulaBlocks(Value, MawMaxRatePosition, FMaxRate);
end;

procedure TMawItem.SetMinRate(const Value: string);
begin
  UpdateFormulaBlocks(Value, MawMinRatePosition, FMinRate);
end;

procedure TMawItem.SetPumpElevation(const Value: string);
begin
  UpdateFormulaBlocks(Value, MawPumpElevationPosition, FPumpElevation);
end;

procedure TMawItem.SetRate(const Value: string);
begin
  FRate.ScreenObject := ScreenObjectI;
  try
    UpdateFormulaBlocks(Value, MawRatePosition, FRate);
  finally
    FRate.ScreenObject := nil;
  end;
end;

procedure TMawItem.SetRateLimitation(const Value: TRateLimitation);
begin
  if FRateLimitation <> Value then
  begin
    FRateLimitation := Value;
    InvalidateModel;
  end;
end;

procedure TMawItem.SetScalingLength(const Value: string);
begin
  UpdateFormulaBlocks(Value, MawScalingLengthPosition, FScalingLength);
end;

procedure TMawItem.SetSpecifiedConcentrations(
  const Value: TMawGwtConcCollection);
begin
  FSpecifiedConcentrations.Assign(Value);
end;

procedure TMawItem.SetViscosity(const Value: TMawGwtConcCollection);
begin
  FViscosity.Assign(Value);
end;

procedure TMawItem.SetWellHead(const Value: string);
begin
  FWellHead.ScreenObject := ScreenObjectI;
  try
    UpdateFormulaBlocks(Value, MawWellHeadPosition, FWellHead);
  finally
    FWellHead.ScreenObject := nil;
  end;
end;

{ TMawTimeListLink }

procedure TMawTimeListLink.CreateTimeLists;
var
  PhastModel: TPhastModel;
  SpeciesIndex: Integer;
begin
  inherited;
  FFlowingWellElevation := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FFlowingWellElevation.NonParamDescription := StrFlowingWellElevati;
  FFlowingWellElevation.ParamDescription := StrFlowingWellElevati;
  if Model <> nil then
  begin
    FFlowingWellElevation.OnInvalidate := (Model as TCustomModel).InvalidateMawFlowingWellElevation;
  end;
  AddTimeList(FFlowingWellElevation);

  FFlowingWellConductance := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FFlowingWellConductance.NonParamDescription := StrFlowingWellConduct;
  FFlowingWellConductance.ParamDescription := StrFlowingWellConduct;
  if Model <> nil then
  begin
    FFlowingWellConductance.OnInvalidate := (Model as TCustomModel).InvalidateMawFlowingWellConductance;
  end;
  AddTimeList(FFlowingWellConductance);

  FRate := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FRate.NonParamDescription := StrMultiaquiferWellRa;
  FRate.ParamDescription := StrMultiaquiferWellRa;
  if Model <> nil then
  begin
    FRate.OnInvalidate := (Model as TCustomModel).InvalidateMawWell_Rate;
  end;
  AddTimeList(FRate);

  FWellHead := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FWellHead.NonParamDescription := StrMultiaquiferWellHe;
  FWellHead.ParamDescription := StrMultiaquiferWellHe;
  if Model <> nil then
  begin
    FWellHead.OnInvalidate := (Model as TCustomModel).InvalidateMawWell_Head;
  end;
  AddTimeList(FWellHead);

  FHeadLimit := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FHeadLimit.NonParamDescription := StrHeadLimit;
  FHeadLimit.ParamDescription := StrHeadLimit;
  if Model <> nil then
  begin
    FHeadLimit.OnInvalidate := (Model as TCustomModel).InvalidateMawWell_Limit;
  end;
  AddTimeList(FHeadLimit);

  FMinRate := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FMinRate.NonParamDescription := StrMinimumPumpingRate;
  FMinRate.ParamDescription := StrMinimumPumpingRate;
  if Model <> nil then
  begin
    FMinRate.OnInvalidate := (Model as TCustomModel).InvalidateMawMinimumPumpRate;
  end;
  AddTimeList(FMinRate);

  FMaxRate := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FMaxRate.NonParamDescription := StrMaximumPumpingRate;
  FMaxRate.ParamDescription := StrMaximumPumpingRate;
  if Model <> nil then
  begin
    FMaxRate.OnInvalidate := (Model as TCustomModel).InvalidateMawMaximumPumpRate;
  end;
  AddTimeList(FMaxRate);

  FPumpElevation := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FPumpElevation.NonParamDescription := StrPumpElevation;
  FPumpElevation.ParamDescription := StrPumpElevation;
  if Model <> nil then
  begin
    FPumpElevation.OnInvalidate := (Model as TCustomModel).InvalidateMawPumpElevation;
  end;
  AddTimeList(FPumpElevation);

  FScalingLength := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FScalingLength.NonParamDescription := StrScalingLength;
  FScalingLength.ParamDescription := StrScalingLength;
  if Model <> nil then
  begin
    FScalingLength.OnInvalidate := (Model as TCustomModel).InvalidateMawScalingLength;
  end;
  AddTimeList(FScalingLength);

  FFlowingWellReductionLength := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FFlowingWellReductionLength.NonParamDescription := StrFlowingWellRedLenth;
  FFlowingWellReductionLength.ParamDescription := StrFlowingWellRedLenth;
  if Model <> nil then
  begin
    FFlowingWellReductionLength.OnInvalidate := (Model as TCustomModel).InvalidateMawWell_Head;
  end;
  AddTimeList(FFlowingWellReductionLength);

  FDensity := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FDensity.NonParamDescription := StrMawFluidDensity;
  FDensity.ParamDescription := StrMawFluidDensity;
  if Model <> nil then
  begin
    FDensity.OnInvalidate := (Model as TCustomModel).InvalidateMawDensity;
  end;
  AddTimeList(FDensity);

  FViscosity := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FViscosity.NonParamDescription := StrMawFluidViscosity;
  FViscosity.ParamDescription := StrMawFluidViscosity;
  if Model <> nil then
  begin
    FViscosity.OnInvalidate := (Model as TCustomModel).InvalidateMawViscosity;
  end;
  AddTimeList(FViscosity);

  FGwtStatusList := TModflowTimeLists.Create;
  FSpecifiedConcList := TModflowTimeLists.Create;
  FInjectionConcList := TModflowTimeLists.Create;

  PhastModel := frmGoPhast.PhastModel;
  if PhastModel.GwtUsed then
  begin
    for SpeciesIndex := 0 to PhastModel.MobileComponents.Count - 1 do
    begin
      AddGwtTimeLists(SpeciesIndex);
    end;
  end;

end;

destructor TMawTimeListLink.Destroy;
begin
  FGwtStatusList.Free;
  FSpecifiedConcList.Free;
  FInjectionConcList.Free;

  FViscosity.Free;
  FDensity.Free;
  FFlowingWellElevation.Free;
  FFlowingWellConductance.Free;
  FRate.Free;
  FWellHead.Free;
  FHeadLimit.Free;
  FMinRate.Free;
  FMaxRate.Free;
  FPumpElevation.Free;
  FScalingLength.Free;
  FFlowingWellReductionLength.Free;

  inherited;
end;

procedure TMawTimeListLink.RemoveGwtTimeLists(SpeciesIndex: Integer);
var
  ConcTimeList: TModflowTimeList;
begin
  ConcTimeList := FGwtStatusList[SpeciesIndex];
  RemoveTimeList(ConcTimeList);
  FGwtStatusList.Delete(SpeciesIndex);

  ConcTimeList := FSpecifiedConcList[SpeciesIndex];
  RemoveTimeList(ConcTimeList);
  FSpecifiedConcList.Delete(SpeciesIndex);

  ConcTimeList := FInjectionConcList[SpeciesIndex];
  RemoveTimeList(ConcTimeList);
  FInjectionConcList.Delete(SpeciesIndex);
end;

procedure TMawTimeListLink.UpdateGwtTimeLists;
var
  LocalModel: TCustomModel;
  SpeciesIndex: Integer;
begin
  LocalModel := Model as TCustomModel;
  if LocalModel.GwtUsed then
  begin
    for SpeciesIndex := FSpecifiedConcList.Count to
      LocalModel.MobileComponents.Count - 1 do
    begin
      AddGwtTimeLists(SpeciesIndex);
    end;
    for SpeciesIndex := LocalModel.MobileComponents.Count to
      FSpecifiedConcList.Count - 1 do
    begin
      RemoveGwtTimeLists(SpeciesIndex);
    end;
  end;
end;

procedure TMawTimeListLink.AddGwtTimeLists(SpeciesIndex: Integer);
var
  ConcTimeList: TModflowTimeList;
  LocalModel: TCustomModel;
  PhastModel: TPhastModel;
begin
  PhastModel := frmGoPhast.PhastModel;
  ConcTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  ConcTimeList.NonParamDescription := PhastModel.MobileComponents[SpeciesIndex].Name + ' MAW Status';
  ConcTimeList.ParamDescription := ConcTimeList.NonParamDescription;
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    ConcTimeList.OnInvalidate := LocalModel.InvalidateMawGwtConc;
  end;
  AddTimeList(ConcTimeList);
  FGwtStatusList.Add(ConcTimeList);
  ConcTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  ConcTimeList.NonParamDescription := PhastModel.MobileComponents[SpeciesIndex].Name + ' MAW Specified Concentration';
  ConcTimeList.ParamDescription := ConcTimeList.NonParamDescription;
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    ConcTimeList.OnInvalidate := LocalModel.InvalidateMawGwtConc;
  end;
  AddTimeList(ConcTimeList);
  FSpecifiedConcList.Add(ConcTimeList);
  ConcTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  ConcTimeList.NonParamDescription := PhastModel.MobileComponents[SpeciesIndex].Name + ' MAW Injection Concentration';
  ConcTimeList.ParamDescription := ConcTimeList.NonParamDescription;
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    ConcTimeList.OnInvalidate := LocalModel.InvalidateMawGwtConc;
  end;
  AddTimeList(ConcTimeList);
  FInjectionConcList.Add(ConcTimeList);
end;

{ TMawWellCollection }

procedure TMawWellCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TMawTransientStorage.Create(AModel));
end;

function TMawWellCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Item: TMawItem;
begin
  Item := Items[ItemIndex] as TMawItem;
  result := Item.BoundaryFormula[FormulaIndex];
end;

function TMawWellCollection.AllowInactiveMf6Cells: boolean;
begin
  result := True;
end;

procedure TMawWellCollection.AssignCellList(CellAssignmentData: TCellAssignmentData);
var
  MawStorage: TMawTransientStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
  FormulaIndex: Integer;
  SpeciesCount: Integer;
  Expression: TExpression;
  ACellList: TObject;
  BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer;
  Variables, DataSets: TList;
  AModel: TBaseModel;
  AScreenObject: TObject;
  PestName: string;
  PestSeriesName: string;
  PestSeriesMethod: TPestParamMethod;
  TimeSeriesName: string;
  DynamicTimeSeries: IDynamicTimeSeries;
begin
  Expression := CellAssignmentData.Expression;
  ACellList := CellAssignmentData.ACellList;
  BoundaryStorage := CellAssignmentData.BoundaryStorage;
  BoundaryFunctionIndex := CellAssignmentData.BoundaryFunctionIndex;
  Variables := CellAssignmentData.Variables;
  DataSets := CellAssignmentData.DataSets;
  AModel := CellAssignmentData.AModel;
  AScreenObject := CellAssignmentData.AScreenObject;
  PestName := CellAssignmentData.PestName;
  PestSeriesName := CellAssignmentData.PestSeriesName;
  PestSeriesMethod := CellAssignmentData.PestSeriesMethod;
  TimeSeriesName := CellAssignmentData.TimeSeriesName;
  DynamicTimeSeries := CellAssignmentData.DynamicTimeSeries;

  BoundaryGroup.Mf6TimeSeriesNames.Add(TimeSeriesName);
//  Assert(BoundaryFunctionIndex in
//    [MawFlowingWellElevationPosition..MawFlowingWellReductionLengthPosition]);
  Assert(Expression <> nil);

  MawStorage := BoundaryStorage as TMawTransientStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);

    AssignDynamicTimeSeries(TimeSeriesName, DynamicTimeSeries, ACell);

    // 2. update locations
    Expression.Evaluate;
    with MawStorage.MawTransientArray[Index] do
    begin
      case BoundaryFunctionIndex of
        MawFlowingWellElevationPosition:
          begin
            FlowingWellElevation := Expression.DoubleResult;
            FlowingWellElevationAnnotation := ACell.Annotation;
            FlowingWellElevationPest := PestName;
            FlowingWellElevationPestSeriesName := PestSeriesName;
            FlowingWellElevationPestSeriesMethod := PestSeriesMethod;
          end;
        MawFlowingWellConductancePosition:
          begin
            FlowingWellConductance := Expression.DoubleResult;
            FlowingWellConductanceAnnotation := ACell.Annotation;
            FlowingWellConductancePest := PestName;
            FlowingWellConductancePestSeriesName := PestSeriesName;
            FlowingWellConductancePestSeriesMethod := PestSeriesMethod;
          end;
        MawRatePosition:
          begin
            Rate := Expression.DoubleResult;
            RateAnnotation := ACell.Annotation;
            RatePest := PestName;
            RatePestSeriesName := PestSeriesName;
            RatePestSeriesMethod := PestSeriesMethod;
            RateTimeSeriesName := TimeSeriesName;
          end;
        MawWellHeadPosition:
          begin
            WellHead := Expression.DoubleResult;
            WellHeadAnnotation := ACell.Annotation;
            WellHeadPest := PestName;
            WellHeadPestSeriesName := PestSeriesName;
            WellHeadPestSeriesMethod := PestSeriesMethod;
            WellHeadTimeSeriesName := TimeSeriesName;
          end;
        MawHeadLimitPosition:
          begin
            HeadLimit := Expression.DoubleResult;
            HeadLimitAnnotation := ACell.Annotation;
            HeadLimitPest := PestName;
            HeadLimitPestSeriesName := PestSeriesName;
            HeadLimitPestSeriesMethod := PestSeriesMethod;
          end;
        MawMinRatePosition:
          begin
            MinRate := Expression.DoubleResult;
            MinRateAnnotation := ACell.Annotation;
            MinRatePest := PestName;
            MinRatePestSeriesName := PestSeriesName;
            MinRatePestSeriesMethod := PestSeriesMethod;
          end;
        MawMaxRatePosition:
          begin
            MaxRate := Expression.DoubleResult;
            MaxRateAnnotation := ACell.Annotation;
            MaxRatePest := PestName;
            MaxRatePestSeriesName := PestSeriesName;
            MaxRatePestSeriesMethod := PestSeriesMethod;
          end;
        MawPumpElevationPosition:
          begin
            PumpElevation := Expression.DoubleResult;
            PumpElevationAnnotation := ACell.Annotation;
            PumpElevationPest := PestName;
            PumpElevationPestSeriesName := PestSeriesName;
            PumpElevationPestSeriesMethod := PestSeriesMethod;
          end;
        MawScalingLengthPosition:
          begin
            ScalingLength := Expression.DoubleResult;
            ScalingLengthAnnotation := ACell.Annotation;
            ScalingLengthPest := PestName;
            ScalingLengthPestSeriesName := PestSeriesName;
            ScalingLengthPestSeriesMethod := PestSeriesMethod;
          end;
        MawFlowingWellReductionLengthPosition:
          begin
            FlowingWellReductionLength := Expression.DoubleResult;
            FlowingWellReductionLengthAnnotation := ACell.Annotation;
            FlowingWellReductionLengthPest := PestName;
            FlowingWellReductionLengthPestSeriesName := PestSeriesName;
            FlowingWellReductionLengthPestSeriesMethod := PestSeriesMethod;
          end;
        MawDensityPosition:
          begin
            Density.Values[0] := Expression.DoubleResult;
            Density.ValueAnnotations[0] := ACell.Annotation;
            Density.ValuePestNames[0] := PestName;
            Density.ValuePestSeriesNames[0] := PestSeriesName;
            Density.ValuePestSeriesMethods[0] := PestSeriesMethod;
            Density.ValueTimeSeriesNames[0] := TimeSeriesName;
          end;
        MawViscosityPosition:
          begin
            Viscosity.Values[0] := Expression.DoubleResult;
            Viscosity.ValueAnnotations[0] := ACell.Annotation;
            Viscosity.ValuePestNames[0] := PestName;
            Viscosity.ValuePestSeriesNames[0] := PestSeriesName;
            Viscosity.ValuePestSeriesMethods[0] := PestSeriesMethod;
            Viscosity.ValueTimeSeriesNames[0] := TimeSeriesName;
          end;
        else
          begin
            FormulaIndex := BoundaryFunctionIndex - MawGwtStart;
            // GWT
            SpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;
            if FormulaIndex < SpeciesCount then
            begin
              with MawStorage.MawTransientArray[Index] do
              begin
                SpecifiedConcentrations.Values[FormulaIndex] := Expression.DoubleResult;
                SpecifiedConcentrations.ValueAnnotations[FormulaIndex] := ACell.Annotation;
                SpecifiedConcentrations.ValuePestNames[FormulaIndex] := PestName;
                SpecifiedConcentrations.ValuePestSeriesNames[FormulaIndex] := PestSeriesName;
                SpecifiedConcentrations.ValuePestSeriesMethods[FormulaIndex] := PestSeriesMethod;
                SpecifiedConcentrations.ValueTimeSeriesNames[FormulaIndex] := TimeSeriesName;
              end;
              break;
            end;

            FormulaIndex := FormulaIndex - SpeciesCount;
            if FormulaIndex < SpeciesCount then
            begin
              with MawStorage.MawTransientArray[Index] do
              begin
                InjectionConcentrations.Values[FormulaIndex] := Expression.DoubleResult;
                InjectionConcentrations.ValueAnnotations[FormulaIndex] := ACell.Annotation;
                InjectionConcentrations.ValuePestNames[FormulaIndex] := PestName;
                InjectionConcentrations.ValuePestSeriesNames[FormulaIndex] := PestSeriesName;
                InjectionConcentrations.ValuePestSeriesMethods[FormulaIndex] := PestSeriesMethod;
                InjectionConcentrations.ValueTimeSeriesNames[FormulaIndex] := TimeSeriesName;
              end;
              break;
            end;

          end;
      end;
    end;
  end;
end;

procedure TMawWellCollection.AssignDirectlySpecifiedValues(
  AnItem: TCustomModflowBoundaryItem; BoundaryStorage: TCustomBoundaryStorage);
var
  MawStorage: TMawTransientStorage;
  index: integer;
  MawItem: TMawItem;
  MawBoundary: TMawBoundary;
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
  MawStorage := BoundaryStorage as TMawTransientStorage;
  MawItem := AnItem as TMawItem;

  MawBoundary := (MawItem.Collection as TMawWellCollection).
    BoundaryGroup as TMawBoundary;

  for index := 0 to Length(MawStorage.MawTransientArray) - 1 do
  begin
    MawStorage.MawTransientArray[index].WellNumber := MawBoundary.WellNumber;
    MawStorage.MawTransientArray[index].MawStatus := MawItem.MawStatus;
    MawStorage.MawTransientArray[index].FlowingWell := MawItem.FlowingWell;
    MawStorage.MawTransientArray[index].Shutoff := MawItem.Shutoff;
    MawStorage.MawTransientArray[index].RateScaling := MawItem.RateScaling;
    MawStorage.MawTransientArray[index].HeadLimitChoice := MawItem.HeadLimitChoice;

    SetLength(MawStorage.MawTransientArray[index].GwtStatus, SpeciesCount);
    while MawItem.GwtStatus.Count < SpeciesCount do
    begin
      MawItem.GwtStatus.Add;
    end;
    for SpeciesIndex := 0 to SpeciesCount - 1 do
    begin
      MawStorage.MawTransientArray[index].GwtStatus[SpeciesIndex] :=
        MawItem.GwtStatus[SpeciesIndex].GwtBoundaryStatus
    end;

  end;
end;

procedure TMawWellCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  MawStorage: TMawTransientStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  MawStorage := BoundaryStorage as TMawTransientStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    if ACell.LgrEdge then
    begin
      Continue;
    end;
    with MawStorage.MawTransientArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

class function TMawWellCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TMawTimeListLink;
end;

procedure TMawWellCollection.InvalidateDensity(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMawTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawTimeListLink;
    Link.FDensity.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TMawTimeListLink;
        Link.FDensity.Invalidate;
      end;
    end;
  end;
end;

procedure TMawWellCollection.InvalidateFlowingWellConductanceData(
  Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMawTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawTimeListLink;
    Link.FFlowingWellConductance.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TMawTimeListLink;
        Link.FFlowingWellConductance.Invalidate;
      end;
    end;
  end;
end;

procedure TMawWellCollection.InvalidateFlowingWellElevationData(
  Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMawTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawTimeListLink;
    Link.FFlowingWellElevation.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TMawTimeListLink;
        Link.FFlowingWellElevation.Invalidate;
      end;
    end;
  end;
end;

procedure TMawWellCollection.InvalidateFlowingWellReductionLengthData(
  Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMawTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawTimeListLink;
    Link.FFlowingWellReductionLength.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TMawTimeListLink;
        Link.FFlowingWellReductionLength.Invalidate;
      end;
    end;
  end;
end;

procedure TMawWellCollection.InvalidateGwtStatus(Sender: TObject);
begin

end;

procedure TMawWellCollection.InvalidateHeadLimitData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMawTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawTimeListLink;
    Link.FHeadLimit.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TMawTimeListLink;
        Link.FHeadLimit.Invalidate;
      end;
    end;
  end;
end;

procedure TMawWellCollection.InvalidateInjectionConcentrations(Sender: TObject);
var
  Index: Integer;
  TimeList: TModflowTimeList;
  PhastModel: TPhastModel;
  Link: TMawTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawTimeListLink;
    for Index := 0 to Link.FInjectionConcList.Count - 1 do
    begin
      TimeList := Link.FInjectionConcList[Index];
      TimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TMawTimeListLink;
        for Index := 0 to Link.FInjectionConcList.Count - 1 do
        begin
          TimeList := Link.FInjectionConcList[Index];
          TimeList.Invalidate;
        end;
      end;
    end;
  end;
end;

procedure TMawWellCollection.InvalidateMaxRateData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMawTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawTimeListLink;
    Link.FMaxRate.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TMawTimeListLink;
        Link.FMaxRate.Invalidate;
      end;
    end;
  end;
end;

procedure TMawWellCollection.InvalidateMinRateData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMawTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawTimeListLink;
    Link.FMinRate.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TMawTimeListLink;
        Link.FMinRate.Invalidate;
      end;
    end;
  end;
end;

procedure TMawWellCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMawFlowingWellElevation(self);
    PhastModel.InvalidateMawFlowingWellConductance(self);
    PhastModel.InvalidateMawFlowingWellReductionLength(self);
    PhastModel.InvalidateMawWell_Rate(self);
    PhastModel.InvalidateMawWell_Head(self);
    PhastModel.InvalidateMawWell_Limit(self);
    PhastModel.InvalidateMawMinimumPumpRate(self);
    PhastModel.InvalidateMawMaximumPumpRate(self);
    PhastModel.InvalidateMawPumpElevation(self);
    PhastModel.InvalidateMawScalingLength(self);
  end;
end;

procedure TMawWellCollection.InvalidatePumpElevationData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMawTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawTimeListLink;
    Link.FPumpElevation.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TMawTimeListLink;
        Link.FPumpElevation.Invalidate;
      end;
    end;
  end;
end;

procedure TMawWellCollection.InvalidateRateData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMawTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawTimeListLink;
    Link.FRate.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TMawTimeListLink;
        Link.FRate.Invalidate;
      end;
    end;
  end;
end;

procedure TMawWellCollection.InvalidateScalingLengthData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMawTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawTimeListLink;
    Link.FScalingLength.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TMawTimeListLink;
        Link.FScalingLength.Invalidate;
      end;
    end;
  end;
end;

procedure TMawWellCollection.InvalidateSpecifiedConcentrations(Sender: TObject);
var
  Index: Integer;
  TimeList: TModflowTimeList;
  PhastModel: TPhastModel;
  Link: TMawTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawTimeListLink;
    for Index := 0 to Link.FSpecifiedConcList.Count - 1 do
    begin
      TimeList := Link.FSpecifiedConcList[Index];
      TimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TMawTimeListLink;
        for Index := 0 to Link.FSpecifiedConcList.Count - 1 do
        begin
          TimeList := Link.FSpecifiedConcList[Index];
          TimeList.Invalidate;
        end;
      end;
    end;
  end;
end;

procedure TMawWellCollection.InvalidateViscosity(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMawTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawTimeListLink;
    Link.FViscosity.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TMawTimeListLink;
        Link.FViscosity.Invalidate;
      end;
    end;
  end;
end;

procedure TMawWellCollection.InvalidateWellHeadData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMawTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawTimeListLink;
    Link.FWellHead.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TMawTimeListLink;
        Link.FWellHead.Invalidate;
      end;
    end;
  end;
end;

class function TMawWellCollection.ItemClass: TBoundaryItemClass;
begin
  result := TMawItem;
end;

procedure TMawWellCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  inherited;
  SetLength((Boundaries[ItemIndex, AModel] as TMawTransientStorage).FMawTransientArray,
    BoundaryCount);
  (Boundaries[ItemIndex, AModel] as TMawTransientStorage).SpeciesCount
    := (AModel as TCustomModel).MobileComponents.Count;
end;

{ TMawCell }

procedure TMawCell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  FValues.Cache(Comp, Strings);
  WriteCompInt(Comp, FStressPeriod);
end;

function TMawCell.GetColumn: integer;
begin
  result := FValues.Cell.Column
end;

function TMawCell.GetDensity: TGwtCellData;
begin
  result := FValues.Density
end;

function TMawCell.GetFlowingWell: TFlowingWell;
begin
  result := FValues.FlowingWell
end;

function TMawCell.GetFlowingWellConductance: double;
begin
  result := FValues.FlowingWellConductance
end;

function TMawCell.GetFlowingWellConductanceAnnotation: string;
begin
  result := FValues.FlowingWellConductanceAnnotation
end;

function TMawCell.GetFlowingWellElevation: double;
begin
  result := FValues.FlowingWellElevation
end;

function TMawCell.GetFlowingWellElevationAnnotation: string;
begin
  result := FValues.FlowingWellElevationAnnotation
end;

function TMawCell.GetFlowingWellReductionLength: double;
begin
  result := FValues.FlowingWellReductionLength;
end;

function TMawCell.GetFlowingWellReductionLengthAnnotation: string;
begin
  result := FValues.FlowingWellReductionLengthAnnotation;
end;

function TMawCell.GetGwtStatus: TGwtBoundaryStatusArray;
begin
  result := FValues.GwtStatus
end;

function TMawCell.GetHeadLimit: double;
begin
  result := FValues.HeadLimit
end;

function TMawCell.GetHeadLimitAnnotation: string;
begin
  result := FValues.HeadLimitAnnotation
end;

function TMawCell.GetHeadLimitChoice: Boolean;
begin
  result := FValues.HeadLimitChoice;
end;

function TMawCell.GetInjectionConcentrations: TGwtCellData;
begin
  result := FValues.InjectionConcentrations
end;

function TMawCell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TMawCell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TMawCell.GetLayer: integer;
begin
  result := FValues.Cell.Layer
end;

function TMawCell.GetMawBoundary: TMawBoundary;
begin
  result := (ScreenObject as TScreenObject).ModflowMawBoundary
end;

function TMawCell.GetMawStatus: TMawStatus;
begin
  result := FValues.MawStatus
end;

function TMawCell.GetMaxRate: double;
begin
  result := FValues.MaxRate
end;

function TMawCell.GetMaxRateAnnotation: string;
begin
  result := FValues.MaxRateAnnotation
end;

function TMawCell.GetMf6TimeSeriesName(Index: Integer): string;
var
  GwtPosition: Integer;
  GwtSource: Integer;
  SpeciesIndex: Integer;
begin
  case Index of
    MawFlowingWellElevationPosition:
        result := inherited;
    MawFlowingWellConductancePosition:
        result := inherited;
    MawRatePosition:
      result := FValues.RateTimeSeriesName;
    MawWellHeadPosition:
      result := FValues.WellHeadTimeSeriesName;
    MawHeadLimitPosition:
        result := inherited;
    MawMinRatePosition:
        result := inherited;
    MawMaxRatePosition:
        result := inherited;
    MawPumpElevationPosition:
        result := inherited;
    MawScalingLengthPosition:
        result := inherited;
    MawFlowingWellReductionLengthPosition:
        result := inherited;
    MawDensityPosition:
        result := FValues.Density.ValueTimeSeriesNames[0];
    MawViscosityPosition:
        result := FValues.Viscosity.ValueTimeSeriesNames[0];
    else
      begin
        Index := Index-MawGwtStart;
        GwtPosition := Index ;
        Assert(GwtPosition >= 0);
        GwtSource := GwtPosition mod MawGwtConcCount;
        SpeciesIndex := GwtPosition div MawGwtConcCount;
        case GwtSource of
          MawGwtSpecifiedConcentrationPosition:
            begin
              result := FValues.SpecifiedConcentrations.ValueTimeSeriesNames[SpeciesIndex]
            end;
          MawGwtInjectionConcentrationsPosition:
            begin
              result := FValues.InjectionConcentrations.ValueTimeSeriesNames[SpeciesIndex]
            end;
          else
            Assert(False);
        end;
      end;
  end;
end;

function TMawCell.GetMinRate: double;
begin
  result := FValues.MinRate
end;

function TMawCell.GetMinRateAnnotation: string;
begin
  result := FValues.MinRateAnnotation
end;

function TMawCell.GetMvrIndex: Integer;
begin
  result := FValues.MvrIndex;
end;

function TMawCell.GetMvrUsed: Boolean;
begin
  result := FValues.MvrUsed;
end;

function TMawCell.GetPestName(Index: Integer): string;
var
  GwtPosition: Integer;
  GwtSource: Integer;
  SpeciesIndex: Integer;
begin
  case Index of
    MawFlowingWellElevationPosition:
      result := FValues.FlowingWellElevationPest;
    MawFlowingWellConductancePosition:
      result := FValues.FlowingWellConductancePest;
    MawRatePosition:
      result := FValues.RatePest;
    MawWellHeadPosition:
      result := FValues.WellHeadPest;
    MawHeadLimitPosition:
      result := FValues.HeadLimitPest;
    MawMinRatePosition:
      result := FValues.MinRatePest;
    MawMaxRatePosition:
      result := FValues.MaxRatePest;
    MawPumpElevationPosition:
      result := FValues.PumpElevationPest;
    MawScalingLengthPosition:
      result := FValues.ScalingLengthPest;
    MawFlowingWellReductionLengthPosition:
      result := FValues.FlowingWellReductionLengthPest;
    MawDensityPosition:
      result := FValues.Density.ValuePestNames[0];
    MawViscosityPosition:
      result := FValues.Viscosity.ValuePestNames[0];
    else
      begin
        Index := Index-MawGwtStart;
        GwtPosition := Index ;
        Assert(GwtPosition >= 0);
        GwtSource := GwtPosition mod MawGwtConcCount;
        SpeciesIndex := GwtPosition div MawGwtConcCount;
        case GwtSource of
          MawGwtSpecifiedConcentrationPosition:
            begin
              result := FValues.SpecifiedConcentrations.ValuePestNames[SpeciesIndex]
            end;
          MawGwtInjectionConcentrationsPosition:
            begin
              result := FValues.InjectionConcentrations.ValuePestNames[SpeciesIndex]
            end;
          else
            Assert(False);
        end;
      end;
  end;
end;

function TMawCell.GetPestSeriesMethod(Index: Integer): TPestParamMethod;
var
  GwtPosition: Integer;
  GwtSource: Integer;
  SpeciesIndex: Integer;
begin
  case Index of
    MawFlowingWellElevationPosition:
      result := FValues.FlowingWellElevationPestSeriesMethod;
    MawFlowingWellConductancePosition:
      result := FValues.FlowingWellConductancePestSeriesMethod;
    MawRatePosition:
      result := FValues.RatePestSeriesMethod;
    MawWellHeadPosition:
      result := FValues.WellHeadPestSeriesMethod;
    MawHeadLimitPosition:
      result := FValues.HeadLimitPestSeriesMethod;
    MawMinRatePosition:
      result := FValues.MinRatePestSeriesMethod;
    MawMaxRatePosition:
      result := FValues.MaxRatePestSeriesMethod;
    MawPumpElevationPosition:
      result := FValues.PumpElevationPestSeriesMethod;
    MawScalingLengthPosition:
      result := FValues.ScalingLengthPestSeriesMethod;
    MawFlowingWellReductionLengthPosition:
      result := FValues.FlowingWellReductionLengthPestSeriesMethod;
    MawDensityPosition:
      result := FValues.Density.ValuePestSeriesMethods[0];
    MawViscosityPosition:
      result := FValues.Viscosity.ValuePestSeriesMethods[0];
    else
      begin
        Index := Index-MawGwtStart;
        GwtPosition := Index ;
        Assert(GwtPosition >= 0);
        GwtSource := GwtPosition mod MawGwtConcCount;
        SpeciesIndex := GwtPosition div MawGwtConcCount;
        case GwtSource of
          MawGwtSpecifiedConcentrationPosition:
            begin
              result := FValues.SpecifiedConcentrations.ValuePestSeriesMethods[SpeciesIndex]
            end;
          MawGwtInjectionConcentrationsPosition:
            begin
              result := FValues.InjectionConcentrations.ValuePestSeriesMethods[SpeciesIndex]
            end;
          else
            begin
              result := ppmMultiply;
              Assert(False);
            end;
        end;
      end;
  end;
end;

function TMawCell.GetPestSeriesName(Index: Integer): string;
var
  GwtPosition: Integer;
  GwtSource: Integer;
  SpeciesIndex: Integer;
begin
  case Index of
    MawFlowingWellElevationPosition:
      result := FValues.FlowingWellElevationPestSeriesName;
    MawFlowingWellConductancePosition:
      result := FValues.FlowingWellConductancePestSeriesName;
    MawRatePosition:
      result := FValues.RatePestSeriesName;
    MawWellHeadPosition:
      result := FValues.WellHeadPestSeriesName;
    MawHeadLimitPosition:
      result := FValues.HeadLimitPestSeriesName;
    MawMinRatePosition:
      result := FValues.MinRatePestSeriesName;
    MawMaxRatePosition:
      result := FValues.MaxRatePestSeriesName;
    MawPumpElevationPosition:
      result := FValues.PumpElevationPestSeriesName;
    MawScalingLengthPosition:
      result := FValues.ScalingLengthPestSeriesName;
    MawFlowingWellReductionLengthPosition:
      result := FValues.FlowingWellReductionLengthPestSeriesName;
    MawDensityPosition:
      result := FValues.Density.ValuePestSeriesNames[0];
    MawViscosityPosition:
      result := FValues.Viscosity.ValuePestSeriesNames[0];
    else
      begin
        Index := Index-MawGwtStart;
        GwtPosition := Index ;
        Assert(GwtPosition >= 0);
        GwtSource := GwtPosition mod MawGwtConcCount;
        SpeciesIndex := GwtPosition div MawGwtConcCount;
        case GwtSource of
          MawGwtSpecifiedConcentrationPosition:
            begin
              result := FValues.SpecifiedConcentrations.ValuePestSeriesNames[SpeciesIndex]
            end;
          MawGwtInjectionConcentrationsPosition:
            begin
              result := FValues.InjectionConcentrations.ValuePestSeriesNames[SpeciesIndex]
            end;
          else
            Assert(False);
        end;
      end;
  end;
end;

function TMawCell.GetPumpElevation: double;
begin
  result := FValues.PumpElevation
end;

function TMawCell.GetPumpElevationAnnotation: string;
begin
  result := FValues.PumpElevationAnnotation
end;

function TMawCell.GetRate: double;
begin
  result := FValues.Rate
end;

function TMawCell.GetRateAnnotation: string;
begin
  result := FValues.RateAnnotation
end;

function TMawCell.GetRateScaling: Boolean;
begin
  result := FValues.RateScaling
end;

function TMawCell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
var
  GwtPosition: Integer;
  GwtSource: Integer;
  SpeciesIndex: Integer;
begin
  case Index of
    MawFlowingWellElevationPosition:
      result := FlowingWellElevationAnnotation;
    MawFlowingWellConductancePosition:
      result := FlowingWellConductanceAnnotation;
    MawRatePosition:
      result := RateAnnotation;
    MawWellHeadPosition:
      result := WellHeadAnnotation;
    MawHeadLimitPosition:
      result := HeadLimitAnnotation;
    MawMinRatePosition:
      result := MinRateAnnotation;
    MawMaxRatePosition:
      result := MaxRateAnnotation;
    MawPumpElevationPosition:
      result := PumpElevationAnnotation;
    MawScalingLengthPosition:
      result := ScalingLengthAnnotation;
    MawFlowingWellReductionLengthPosition:
      result := FlowingWellReductionLengthAnnotation;
    MawDensityPosition:
      result := FValues.Density.ValueAnnotations[0];
    MawViscosityPosition:
      result := FValues.Viscosity.ValueAnnotations[0];
    else
      begin
        Index := Index-MawGwtStart;
        GwtPosition := Index ;
        Assert(GwtPosition >= 0);
        GwtSource := GwtPosition mod MawGwtConcCount;
        SpeciesIndex := GwtPosition div MawGwtConcCount;
        case GwtSource of
          MawGwtSpecifiedConcentrationPosition:
            begin
              result := FValues.SpecifiedConcentrations.ValueAnnotations[SpeciesIndex]
            end;
          MawGwtInjectionConcentrationsPosition:
            begin
              result := FValues.InjectionConcentrations.ValueAnnotations[SpeciesIndex]
            end;
          else
            Assert(False);
        end;
      end;
  end;
end;

function TMawCell.GetRealValue(Index: integer; AModel: TBaseModel): double;
var
  GwtPosition: Integer;
  GwtSource: Integer;
  SpeciesIndex: Integer;
begin
  result := 0;
  case Index of
    MawFlowingWellElevationPosition:
      result := FlowingWellElevation;
    MawFlowingWellConductancePosition:
      result := FlowingWellConductance;
    MawRatePosition:
      result := Rate;
    MawWellHeadPosition:
      result := WellHead;
    MawHeadLimitPosition:
      result := HeadLimit;
    MawMinRatePosition:
      result := MinRate;
    MawMaxRatePosition:
      result := MaxRate;
    MawPumpElevationPosition:
      result := PumpElevation;
    MawScalingLengthPosition:
      result := ScalingLength;
    MawFlowingWellReductionLengthPosition:
      result := FlowingWellReductionLength;
    MawDensityPosition:
      result := FValues.Density.Values[0];
    MawViscosityPosition:
      result := FValues.Viscosity.Values[0];
    else
      begin
        Index := Index-MawGwtStart;
        GwtPosition := Index ;
        Assert(GwtPosition >= 0);
        GwtSource := GwtPosition mod MawGwtConcCount;
        SpeciesIndex := GwtPosition div MawGwtConcCount;
        case GwtSource of
          MawGwtSpecifiedConcentrationPosition:
            begin
              result := FValues.SpecifiedConcentrations.Values[SpeciesIndex]
            end;
          MawGwtInjectionConcentrationsPosition:
            begin
              result := FValues.InjectionConcentrations.Values[SpeciesIndex]
            end;
          else
            begin
              result := inherited GetRealValue(Index, AModel);
              Assert(False);
            end;
        end;
      end;
  end;
end;

function TMawCell.GetRow: integer;
begin
  result := FValues.Cell.Row
end;

function TMawCell.GetScalingLength: double;
begin
  result := FValues.ScalingLength
end;

function TMawCell.GetScalingLengthAnnotation: string;
begin
  result := FValues.ScalingLengthAnnotation
end;

function TMawCell.GetSection: integer;
begin
  result := FValues.Cell.Section
end;

function TMawCell.GetShutOff: Boolean;
begin
  result := FValues.ShutOff
end;

function TMawCell.GetSpecifiedConcentrations: TGwtCellData;
begin
  result := FValues.SpecifiedConcentrations
end;

function TMawCell.GetViscosity: TGwtCellData;
begin
  result := FValues.Viscosity
end;

function TMawCell.GetWellHead: double;
begin
  result := FValues.WellHead
end;

function TMawCell.GetWellHeadAnnotation: string;
begin
  result := FValues.WellHeadAnnotation
end;

function TMawCell.GetWellNumber: Integer;
begin
  result := FValues.WellNumber
end;

procedure TMawCell.RecordStrings(Strings: TStringList);
begin
  inherited;
  FValues.RecordStrings(Strings);
end;

procedure TMawCell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;
  FValues.Restore(Decomp, Annotations);
  FStressPeriod := ReadCompInt(Decomp);
end;

procedure TMawCell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TMawCell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TMawCell.SetMf6TimeSeriesName(Index: Integer; const Value: string);
var
  GwtPosition: Integer;
  GwtSource: Integer;
  SpeciesIndex: Integer;
begin
  case Index of
    MawFlowingWellElevationPosition:
        inherited;
    MawFlowingWellConductancePosition:
        inherited;
    MawRatePosition:
      FValues.RateTimeSeriesName := Value;
    MawWellHeadPosition:
      FValues.WellHeadTimeSeriesName := Value;
    MawHeadLimitPosition:
        inherited;
    MawMinRatePosition:
        inherited;
    MawMaxRatePosition:
        inherited;
    MawPumpElevationPosition:
        inherited;
    MawScalingLengthPosition:
        inherited;
    MawFlowingWellReductionLengthPosition:
        inherited;
    MawDensityPosition:
        FValues.Density.ValueTimeSeriesNames[0] := Value;
    MawViscosityPosition:
        FValues.Viscosity.ValueTimeSeriesNames[0] := Value;
    else
      begin
        Index := Index-MawGwtStart;
        GwtPosition := Index ;
        Assert(GwtPosition >= 0);
        GwtSource := GwtPosition mod MawGwtConcCount;
        SpeciesIndex := GwtPosition div MawGwtConcCount;
        case GwtSource of
          MawGwtSpecifiedConcentrationPosition:
            begin
              FValues.SpecifiedConcentrations.ValueTimeSeriesNames[SpeciesIndex] := Value;
            end;
          MawGwtInjectionConcentrationsPosition:
            begin
              FValues.InjectionConcentrations.ValueTimeSeriesNames[SpeciesIndex] := Value;
            end;
          else
            Assert(False);
        end;
      end;
  end;
end;

procedure TMawCell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TMawGwtConcCollection }

constructor TMawGwtConcCollection.Create(Model: TBaseModel;
  AScreenObject: TObject; ParentCollection: TMawWellCollection);
begin
  inherited Create(Model, AScreenObject, ParentCollection);
end;

initialization
  InitializeMawObNames;
  InitializeMwtObNames;

finalization
  MawObNames.Free;
  MwtObNames.Free;

end.
