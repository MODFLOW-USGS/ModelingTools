unit ModflowMawUnit;

interface

uses
  ZLib, Classes, ModflowCellUnit, ModflowBoundaryUnit, OrderedCollectionUnit,
  FormulaManagerUnit, GoPhastTypes, RbwParser, SubscriptionUnit;

type
  TMawOb = (moHead, moFromMvr, moFlowRate, moFlowRateCells, moPumpRate, moRateToMvr,
    moFlowingWellFlowRate, moFlowWellToMvr, moStorageFlowRate, moConstantFlowRate, moConductance,
    moConductanceCells, moFlowingWellConductance);
  TMawObs = set of TMawOb;


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
    Rate: double;
    WellHead: double;
    HeadLimit: double;
    MinRate: double;
    MaxRate: double;
    PumpElevation: double;
    ScalingLength: double;

    FlowingWellElevationAnnotation: string;
    FlowingWellConductanceAnnotation: string;
    RateAnnotation: string;
    WellHeadAnnotation: string;
    HeadLimitAnnotation: string;
    MinRateAnnotation: string;
    MaxRateAnnotation: string;
    PumpElevationAnnotation: string;
    ScalingLengthAnnotation: string;
    MvrUsed: Boolean;
    MvrIndex: Integer;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TMawTransientArray = array of TMawTransientRecord;

  TMawTransientStorage = class(TCustomBoundaryStorage)
  private
    FMawTransientArray: TMawTransientArray;
    function GetMawTransientArray: TMawTransientArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream;
      Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property MawTransientArray: TMawTransientArray read GetMawTransientArray;
  end;

  TMawItem = class(TCustomModflowBoundaryItem)
  private
    FFlowingWell: TFlowingWell;
    FMawStatus: TMawStatus;
    FFlowingWellConductance: TFormulaObject;
    FFlowingWellElevation: TFormulaObject;
    FHeadLimit: TFormulaObject;
    FMaxRate: TFormulaObject;
    FMinRate: TFormulaObject;
    FPumpElevation: TFormulaObject;
    FRate: TFormulaObject;
    FScalingLength: TFormulaObject;
    FWellHead: TFormulaObject;
    FHeadLimitChoice: Boolean;
    FRateLimitation: TRateLimitation;
    function GetFlowingWellConductance: string;
    function GetFlowingWellElevation: string;
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
    property MawStatus: TMawStatus read FMawStatus write SetMawStatus;
    property Rate: string read GetRate write SetRate;
    property WellHead: string read GetWellHead write SetWellHead;

    property FlowingWell: TFlowingWell read FFlowingWell write SetFlowingWell;
    property FlowingWellElevation: string read GetFlowingWellElevation write SetFlowingWellElevation;
    property FlowingWellConductance: string read GetFlowingWellConductance write SetFlowingWellConductance;

    // ShutOff and RateScaling can not be used simultaneously.
    // RateLimitation chooses between no-limit, ShutOff, and RateScaling.
    property RateLimitation: TRateLimitation read FRateLimitation write SetRateLimitation;
    property MinRate: string read GetMinRate write SetMinRate;
    property MaxRate: string read GetMaxRate write SetMaxRate;

    property PumpElevation: string read GetPumpElevation write SetPumpElevation;
    property ScalingLength: string read GetScalingLength write SetScalingLength;

    property HeadLimitChoice: Boolean read FHeadLimitChoice write SetHeadLimitChoice;
    property HeadLimit: string read GetHeadLimit write SetHeadLimit;
  end;

  TMawTimeListLink = class(TTimeListsModelLink)
  protected
    FFlowingWellElevation: TModflowTimeList;
    FFlowingWellConductance: TModflowTimeList;
    FRate: TModflowTimeList;
    FWellHead: TModflowTimeList;
    FHeadLimit: TModflowTimeList;
    FMinRate: TModflowTimeList;
    FMaxRate: TModflowTimeList;
    FPumpElevation: TModflowTimeList;
    FScalingLength: TModflowTimeList;
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  TMawWellCollection = class(TCustomMF_ListBoundColl)
  private
    procedure InvalidateFlowingWellElevationData(Sender: TObject);
    procedure InvalidateFlowingWellConductanceData(Sender: TObject);
    procedure InvalidateRateData(Sender: TObject);
    procedure InvalidateWellHeadData(Sender: TObject);
    procedure InvalidateHeadLimitData(Sender: TObject);
    procedure InvalidateMinRateData(Sender: TObject);
    procedure InvalidateMaxRateData(Sender: TObject);
    procedure InvalidatePumpElevationData(Sender: TObject);
    procedure InvalidateScalingLengthData(Sender: TObject);
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
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
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject); override;
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
    property Rate: double read GetRate;
    property WellHead: double read GetWellHead;
    property HeadLimit: double read GetHeadLimit;
    property MinRate: double read GetMinRate;
    property MaxRate: double read GetMaxRate;
    property PumpElevation: double read GetPumpElevation;
    property ScalingLength: double read GetScalingLength;

    property FlowingWellElevationAnnotation: string read GetFlowingWellElevationAnnotation;
    property FlowingWellConductanceAnnotation: string read GetFlowingWellConductanceAnnotation;
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
  end;

  TMawSteadyWellRecord = record
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
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
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
    FScreenBottom: TFormulaObject;
    FScreenTop: TFormulaObject;
    FSkinK: TFormulaObject;
    FSkinRadius: TFormulaObject;
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
    property ScreenTop: string read GetScreenTop write SetScreenTop;
    property ScreenBottom: string read GetScreenBottom write SetScreenBottom;
    property SkinK: string read GetSkinK write SetSkinK;
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
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
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
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject); override;
    procedure AssignArrayCellValues(DataSets: TList; ItemIndex: Integer;
      AModel: TBaseModel); override;
    function ShouldDeleteItemsWithZeroDuration: Boolean; override;
  public
    procedure Loaded; 
    property Items[Index: Integer]: TMawWellScreenItem read GetItems write SetItems; default;
  end;

  TMawBoundary = class(TModflowBoundary)
  private
    const
      RadiusPosition = 0;
      BottomPosition = 1;
      InitialHeadPosition = 2;
    var
    FWellNumber: Integer;
    FBottomObserver: TObserver;
    FInitialHeadObserver: TObserver;
    FRadiusObserver: TObserver;
    FConductanceMethod: TMawConductanceMethod;
    FRadius: TFormulaObject;
    FBottom: TFormulaObject;
    FInitialHead: TFormulaObject;
    FWellScreens: TMawWellScreenCollection;
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
  protected
    property RadiusObserver: TObserver read GetRadiusObserver;
    property BottomObserver: TObserver read GetBottomObserver;
    property InitialHeadObserver: TObserver read GetInitialHeadObserver;
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    class function BoundaryCollectionClass: TMF_BoundCollClass;
      override;
    function BoundaryObserverPrefix: string; override;
  public
    Procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    Destructor Destroy; override;
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel); override;
    // The well number is assigned in the export process.
    property WellNumber: Integer read FWellNumber write SetWellNumber;
    procedure InitializeVariables;
    procedure Loaded;
    procedure InvalidateDisplay; override;
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
  end;

const
  FlowingWellElevationPosition = 0;
  FlowingWellConductancePosition = 1;
  RatePosition = 2;
  WellHeadPosition = 3;
  HeadLimitPosition = 4;
  MinRatePosition = 5;
  MaxRatePosition = 6;
  PumpElevationPosition = 7;
  ScalingLengthPosition = 8;

function TryGetMawOb(const MawObName: string; var MawOb: TMawOb): Boolean;
function MawObToString(const MawOb: TMawOb): string;
Procedure FillMawSeriesNames(AList: TStrings);

implementation

uses
  frmGoPhastUnit, PhastModelUnit, ScreenObjectUnit,
  GIS_Functions, ModflowTimeUnit, DataSetUnit, ModflowMnw2Unit,
  ModelMuseUtilities, ModflowMvrUnit;

const MawObName: array[TMawOb] of string = ('Head', 'FromMvr', 'FlowRate', 'FlowRateCells', 'PumpRate', 'RateToMvr',
    'FlowingWellFlowRate', 'FlowWellToMvr', 'StorageFlowRate', 'ConstantFlowRate', 'Conductance',
    'ConductanceCells', 'FlowingWellConductance');

var
  MawObNames: TStringList;

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

function MawObToString(const MawOb: TMawOb): string;
begin
  result := MawObName[MawOb];
end;

Procedure FillMawSeriesNames(AList: TStrings);
begin
  AList.Assign(MawObNames);
end;
  {
  TMawOb = (moHead, moFromMvr, moFlowRate, moFlowRateCells, moPumpRate, moRateToMvr,
    moFlowingWellFlowRate, moFlowWellToMvr, moStorageFlowRate, moConstantFlowRate, moConductance,
    moConductanceCells, moFlowingWellConductance);
  }

resourcestring
  StrScreenTop = 'Screen_Top';
  StrScreenBottom = 'Screen_Bottom';
  StrSkinK = 'Skin_K';
  StrSkinRadius = 'Skin_Radius';
  StrFlowingWellElevati = 'Flowing_Well_Elevation';
  StrFlowingWellConduct = 'Flowing_Well_Conductance';
  StrMultiaquiferWellRa = 'Multiaquifer_Well_Rate';
  StrMultiaquiferWellHe = 'Multiaquifer_Well_Head';
  StrHeadLimit = 'Head_Limit';
  StrMinimumPumpingRate = 'Minimum_Pumping_Rate';
  StrMaximumPumpingRate = 'Maximum_Pumping_Rate';
  StrPumpElevation = 'Pump_Elevation';
  StrScalingLength = 'Scaling_Length';

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
  WriteCompInt(Comp, Strings.IndexOf(SkinRadiusAnnotation));
end;

procedure TMawSteadyConnectionRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(ScreenTopAnnotation);
  Strings.Add(ScreenBottomAnnotation);
  Strings.Add(SkinKAnnotation);
  Strings.Add(SkinRadiusAnnotation);
  Strings.Add(ScreenObjectName);

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
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
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
  if Sender = FScreenBottom then
  begin
    List.Add(FObserverList[ScreenBottomPosition]);
  end;
  if Sender = FScreenTop then
  begin
    List.Add(FObserverList[ScreenTopPosition]);
  end;
  if Sender = FSkinK then
  begin
    List.Add(FObserverList[SkinKPosition]);
  end;
  if Sender = FSkinRadius then
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
  ItemIndex: Integer; AModel: TBaseModel);
begin
  // does anything need to be done here?
  // called from TCustomListArrayBoundColl.AssignArrayCellsWithItem
  // which is called by TCustomListArrayBoundColl.EvaluateArrayBoundaries
//  inherited;

end;

procedure TMawWellScreenCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList;
  AModel: TBaseModel; AScreenObject: TObject);
var
  MawStorage: TMawSteadyConnectionStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
//  LocalScreenObject: TScreenObject;
begin
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
          end;
        TMawWellScreenItem.ScreenTopPosition:
          begin
            ScreenTop := Expression.DoubleResult;
            ScreenTopAnnotation := ACell.Annotation;
          end;
        TMawWellScreenItem.SkinKPosition:
          begin
            SkinK := Expression.DoubleResult;
            SkinKAnnotation := ACell.Annotation;

          end;
        TMawWellScreenItem.SkinRadiusPosition:
          begin
            SkinRadius := Expression.DoubleResult;
            SkinRadiusAnnotation := ACell.Annotation;
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

function TMawWellScreenCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawWellScreenTimeListLink;
    Link.FScreenBottomData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMawWellScreenTimeListLink;
      Link.FScreenBottomData.Invalidate;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawWellScreenTimeListLink;
    Link.FScreenTopData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMawWellScreenTimeListLink;
      Link.FScreenTopData.Invalidate;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawWellScreenTimeListLink;
    Link.FSkinKData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMawWellScreenTimeListLink;
      Link.FSkinKData.Invalidate;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawWellScreenTimeListLink;
    Link.FSkinRadiusData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMawWellScreenTimeListLink;
      Link.FSkinRadiusData.Invalidate;
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
        Cell.Values := BoundaryValues;
        Cell.ScreenObject := ScreenObject;
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
  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;
  FWellScreens := TMawWellScreenCollection.Create(Self, Model, ScreenObject);

  LinkRadius;
  LinkBottom;
  LinkInitialHead;

  InitializeVariables;
end;

procedure TMawBoundary.CreateFormulaObjects;
begin
  FRadius := CreateFormulaObjectBlocks(dso3D);
  FBottom := CreateFormulaObjectBlocks(dso3D);
  FInitialHead := CreateFormulaObjectBlocks(dso3D);
end;

procedure TMawBoundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(RadiusObserver);
    FObserverList.Add(BottomObserver);
    FObserverList.Add(InitialHeadObserver);
  end;
end;

destructor TMawBoundary.Destroy;
begin
  FWellScreens.Free;
  RemoveFormulaObjects;
  inherited;
end;

function TMawBoundary.GetBottom: string;
begin
  Result := FBottom.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(BottomPosition);
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
  ParamList: TStringList; AModel: TBaseModel);
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
    ResetItemObserver(InitialHeadPosition);
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

function TMawBoundary.GetRadius: string;
begin
  Result := FRadius.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(RadiusPosition);
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

procedure TMawBoundary.InitializeVariables;
begin
  Radius := '0';
  Bottom := '0';
  InitialHead := '0';
  ConductanceMethod := mcmSpecified;
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
  LocaModel.InvalidateMawFlowingWellElevation(self);
  LocaModel.InvalidateMawFlowingWellConductance(self);
  LocaModel.InvalidateMawWell_Rate(self);
  LocaModel.InvalidateMawWell_Head(self);
  LocaModel.InvalidateMawWell_Limit(self);
  LocaModel.InvalidateMawMinimumPumpRate(self);
  LocaModel.InvalidateMawMaximumPumpRate(self);
  LocaModel.InvalidateMawPumpElevation(self);
  LocaModel.InvalidateMawScalingLength(self);
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
  MawRadiushArray: TDataArray;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.TalksTo(RadiusObserver);
    if ParentModel <> nil then
    begin
      MawRadiushArray := (ParentModel as TCustomModel).DataArrayManager.
        GetDataSetByName(KMAWRadius);
      if MawRadiushArray <> nil then
      begin
        RadiusObserver.TalksTo(MawRadiushArray);
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

procedure TMawBoundary.SetRadius(const Value: string);
begin
  UpdateFormulaBlocks(Value, RadiusPosition, FRadius);
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

procedure TMawTransientRecord.Cache(Comp: TCompressionStream;
  Strings: TStringList);
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
  WriteCompReal(Comp, Rate);
  WriteCompReal(Comp, WellHead);
  WriteCompReal(Comp, HeadLimit);
  WriteCompReal(Comp, MinRate);
  WriteCompReal(Comp, MaxRate);
  WriteCompReal(Comp, PumpElevation);
  WriteCompReal(Comp, ScalingLength);

  WriteCompInt(Comp, Strings.IndexOf(FlowingWellElevationAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(FlowingWellConductanceAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(RateAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(WellHeadAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(HeadLimitAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(MinRateAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(MaxRateAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(PumpElevationAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(ScalingLengthAnnotation));
  WriteCompBoolean(Comp, MvrUsed);
  WriteCompInt(Comp, MvrIndex);
end;

procedure TMawTransientRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(FlowingWellElevationAnnotation);
  Strings.Add(FlowingWellConductanceAnnotation);
  Strings.Add(RateAnnotation);
  Strings.Add(WellHeadAnnotation);
  Strings.Add(HeadLimitAnnotation);
  Strings.Add(MinRateAnnotation);
  Strings.Add(MaxRateAnnotation);
  Strings.Add(PumpElevationAnnotation);
  Strings.Add(ScalingLengthAnnotation);
end;

procedure TMawTransientRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
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
  Rate := ReadCompReal(Decomp);
  WellHead := ReadCompReal(Decomp);
  HeadLimit := ReadCompReal(Decomp);
  MinRate := ReadCompReal(Decomp);
  MaxRate := ReadCompReal(Decomp);
  PumpElevation := ReadCompReal(Decomp);
  ScalingLength := ReadCompReal(Decomp);

  FlowingWellElevationAnnotation := Annotations[ReadCompInt(Decomp)];
  FlowingWellConductanceAnnotation := Annotations[ReadCompInt(Decomp)];
  RateAnnotation := Annotations[ReadCompInt(Decomp)];
  WellHeadAnnotation := Annotations[ReadCompInt(Decomp)];
  HeadLimitAnnotation := Annotations[ReadCompInt(Decomp)];
  MinRateAnnotation := Annotations[ReadCompInt(Decomp)];
  MaxRateAnnotation := Annotations[ReadCompInt(Decomp)];
  PumpElevationAnnotation := Annotations[ReadCompInt(Decomp)];
  ScalingLengthAnnotation := Annotations[ReadCompInt(Decomp)];

  MvrUsed := ReadCompBoolean(Decomp);
  MvrIndex := ReadCompInt(Decomp);
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

procedure TMawTransientStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
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

procedure TMawSteadyWellRecord.Cache(Comp: TCompressionStream;
  Strings: TStringList);
begin
  WriteCompInt(Comp, WellNumber);

  WriteCompReal(Comp, Radius);
  WriteCompReal(Comp, Bottom);
  WriteCompReal(Comp, StartingHead);
  WriteCompInt(Comp, Ord(ConductanceMethod));
  WriteCompInt(Comp, CellCount);

  WriteCompInt(Comp, Strings.IndexOf(RadiusAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(BottomAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(StartingHeadAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(BoundName));
  WriteCompInt(Comp, Strings.IndexOf(ScreenObjectName));

end;

procedure TMawSteadyWellRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(RadiusAnnotation);
  Strings.Add(BottomAnnotation);
  Strings.Add(StartingHeadAnnotation);
  Strings.Add(BoundName);
  Strings.Add(ScreenObjectName);
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

  RadiusAnnotation := Annotations[ReadCompInt(Decomp)];
  BottomAnnotation := Annotations[ReadCompInt(Decomp)];
  StartingHeadAnnotation := Annotations[ReadCompInt(Decomp)];
  BoundName := Annotations[ReadCompInt(Decomp)];
  ScreenObjectName := Annotations[ReadCompInt(Decomp)];
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
    Rate := MawSource.Rate;
    WellHead := MawSource.WellHead;
    HeadLimit := MawSource.HeadLimit;
    MinRate := MawSource.MinRate;
    MaxRate := MawSource.MaxRate;
    PumpElevation := MawSource.PumpElevation;
    ScalingLength := MawSource.ScalingLength;
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
begin
  ParentCollection := Collection as TMawWellCollection;

  AnObserver := FObserverList[FlowingWellElevationPosition];
  AnObserver.OnUpToDateSet := ParentCollection.InvalidateFlowingWellElevationData;

  AnObserver := FObserverList[FlowingWellConductancePosition];
  AnObserver.OnUpToDateSet := ParentCollection.InvalidateFlowingWellConductanceData;

  AnObserver := FObserverList[RatePosition];
  AnObserver.OnUpToDateSet := ParentCollection.InvalidateRateData;

  AnObserver := FObserverList[WellHeadPosition];
  AnObserver.OnUpToDateSet := ParentCollection.InvalidateWellHeadData;

  AnObserver := FObserverList[HeadLimitPosition];
  AnObserver.OnUpToDateSet := ParentCollection.InvalidateHeadLimitData;

  AnObserver := FObserverList[MinRatePosition];
  AnObserver.OnUpToDateSet := ParentCollection.InvalidateMinRateData;

  AnObserver := FObserverList[MaxRatePosition];
  AnObserver.OnUpToDateSet := ParentCollection.InvalidateMaxRateData;

  AnObserver := FObserverList[PumpElevationPosition];
  AnObserver.OnUpToDateSet := ParentCollection.InvalidatePumpElevationData;

  AnObserver := FObserverList[ScalingLengthPosition];
  AnObserver.OnUpToDateSet := ParentCollection.InvalidateScalingLengthData;

end;

function TMawItem.BoundaryFormulaCount: integer;
begin
  Result := 9;
end;

constructor TMawItem.Create(Collection: TCollection);
begin
  inherited;
  FMawStatus := mwActive;
  FFlowingWell := fwNotFlowing;
  FRateLimitation := rlNone;
  FHeadLimitChoice := False;
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

end;

destructor TMawItem.Destroy;
begin
  Rate := '0';
  WellHead := '0';
  FlowingWellElevation := '0';
  FlowingWellConductance := '0';
  MinRate := '0';
  MaxRate := '0';
  PumpElevation := '0';
  ScalingLength := '0';
  HeadLimit := '0';

  inherited;
end;

function TMawItem.GetBoundaryFormula(Index: integer): string;
begin
  case index of
    FlowingWellElevationPosition:
      result := FlowingWellElevation;
    FlowingWellConductancePosition:
      result := FlowingWellConductance;
    RatePosition:
      result := Rate;
    WellHeadPosition:
      result := WellHead;
    HeadLimitPosition:
      result := HeadLimit;
    MinRatePosition:
      result := MinRate;
    MaxRatePosition:
      result := MaxRate;
    PumpElevationPosition:
      result := PumpElevation;
    ScalingLengthPosition:
      result := ScalingLength;
    else
      Assert(False);
  end;
end;

function TMawItem.GetFlowingWellConductance: string;
begin
  Result := FFlowingWellConductance.Formula;
  ResetItemObserver(FlowingWellConductancePosition);
end;

function TMawItem.GetFlowingWellElevation: string;
begin
  Result := FFlowingWellElevation.Formula;
  ResetItemObserver(FlowingWellElevationPosition);
end;

function TMawItem.GetHeadLimit: string;
begin
  Result := FHeadLimit.Formula;
  ResetItemObserver(HeadLimitPosition);
end;

function TMawItem.GetMaxRate: string;
begin
  Result := FMaxRate.Formula;
  ResetItemObserver(MaxRatePosition);
end;

function TMawItem.GetMinRate: string;
begin
  Result := FMinRate.Formula;
  ResetItemObserver(MinRatePosition);
end;

procedure TMawItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  inherited;
  if Sender = FFlowingWellConductance then
  begin
    List.Add(FObserverList[FlowingWellConductancePosition]);
  end
  else if Sender = FFlowingWellElevation then
  begin
    List.Add(FObserverList[FlowingWellElevationPosition]);
  end
  else if Sender = FHeadLimit then
  begin
    List.Add(FObserverList[HeadLimitPosition]);
  end
  else if Sender = FMaxRate then
  begin
    List.Add(FObserverList[MaxRatePosition]);
  end
  else if Sender = FMinRate then
  begin
    List.Add(FObserverList[MinRatePosition]);
  end
  else if Sender = FPumpElevation then
  begin
    List.Add(FObserverList[PumpElevationPosition]);
  end
  else if Sender = FRate then
  begin
    List.Add(FObserverList[RatePosition]);
  end
  else if Sender = FScalingLength then
  begin
    List.Add(FObserverList[ScalingLengthPosition]);
  end
  else if Sender = FWellHead then
  begin
    List.Add(FObserverList[WellHeadPosition]);
  end
end;

function TMawItem.GetPumpElevation: string;
begin
  Result := FPumpElevation.Formula;
  ResetItemObserver(PumpElevationPosition);
end;

function TMawItem.GetRate: string;
begin
  Result := FRate.Formula;
  ResetItemObserver(RatePosition);
end;

function TMawItem.GetRateScaling: Boolean;
begin
  result := RateLimitation = rlScaling;
end;

function TMawItem.GetScalingLength: string;
begin
  Result := FScalingLength.Formula;
  ResetItemObserver(ScalingLengthPosition);
end;

function TMawItem.GetShutoff: Boolean;
begin
  result := RateLimitation = rlShutoff;
end;

function TMawItem.GetWellHead: string;
begin
  Result := FWellHead.Formula;
  ResetItemObserver(WellHeadPosition);
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
      and (Rate = SourceItem.Rate)
      and (WellHead = SourceItem.WellHead)
      and (HeadLimit = SourceItem.HeadLimit)
      and (MinRate = SourceItem.MinRate)
      and (MaxRate = SourceItem.MaxRate)
      and (PumpElevation = SourceItem.PumpElevation)
      and (ScalingLength = SourceItem.ScalingLength)
      and (HeadLimitChoice = SourceItem.HeadLimitChoice)

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
end;

procedure TMawItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case index of
    FlowingWellElevationPosition:
      FlowingWellElevation := Value;
    FlowingWellConductancePosition:
      FlowingWellConductance := Value;
    RatePosition:
      Rate := Value;
    WellHeadPosition:
      WellHead := Value;
    HeadLimitPosition:
      HeadLimit := Value;
    MinRatePosition:
      MinRate := Value;
    MaxRatePosition:
      MaxRate := Value;
    PumpElevationPosition:
      PumpElevation := Value;
    ScalingLengthPosition:
      ScalingLength := Value;
    else
      Assert(False);
  end;
end;

procedure TMawItem.SetFlowingWell(const Value: TFlowingWell);
begin
  FFlowingWell := Value;
end;

procedure TMawItem.SetFlowingWellConductance(const Value: string);
begin
  UpdateFormulaBlocks(Value, FlowingWellConductancePosition, FFlowingWellConductance);
end;

procedure TMawItem.SetFlowingWellElevation(const Value: string);
begin
  UpdateFormulaBlocks(Value, FlowingWellElevationPosition, FFlowingWellElevation);
end;

procedure TMawItem.SetHeadLimit(const Value: string);
begin
  UpdateFormulaBlocks(Value, HeadLimitPosition, FHeadLimit);
end;

procedure TMawItem.SetHeadLimitChoice(const Value: Boolean);
begin
  FHeadLimitChoice := Value;
end;

procedure TMawItem.SetMawStatus(const Value: TMawStatus);
begin
  FMawStatus := Value;
end;

procedure TMawItem.SetMaxRate(const Value: string);
begin
  UpdateFormulaBlocks(Value, MaxRatePosition, FMaxRate);
end;

procedure TMawItem.SetMinRate(const Value: string);
begin
  UpdateFormulaBlocks(Value, MinRatePosition, FMinRate);
end;

procedure TMawItem.SetPumpElevation(const Value: string);
begin
  UpdateFormulaBlocks(Value, PumpElevationPosition, FPumpElevation);
end;

procedure TMawItem.SetRate(const Value: string);
begin
  UpdateFormulaBlocks(Value, RatePosition, FRate);
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
  UpdateFormulaBlocks(Value, ScalingLengthPosition, FScalingLength);
end;

procedure TMawItem.SetWellHead(const Value: string);
begin
  UpdateFormulaBlocks(Value, WellHeadPosition, FWellHead);
end;

{ TMawTimeListLink }

procedure TMawTimeListLink.CreateTimeLists;
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
end;

destructor TMawTimeListLink.Destroy;
begin
  FFlowingWellElevation.Free;
  FFlowingWellConductance.Free;
  FRate.Free;
  FWellHead.Free;
  FHeadLimit.Free;
  FMinRate.Free;
  FMaxRate.Free;
  FPumpElevation.Free;
  FScalingLength.Free;

  inherited;
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

procedure TMawWellCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList;
  AModel: TBaseModel; AScreenObject: TObject);
var
  MawStorage: TMawTransientStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  Assert(BoundaryFunctionIndex in [FlowingWellElevationPosition..ScalingLengthPosition]);
  Assert(Expression <> nil);

  MawStorage := BoundaryStorage as TMawTransientStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);
    // 2. update locations
    Expression.Evaluate;
    with MawStorage.MawTransientArray[Index] do
    begin
      case BoundaryFunctionIndex of
        FlowingWellElevationPosition:
          begin
            FlowingWellElevation := Expression.DoubleResult;
            FlowingWellElevationAnnotation := ACell.Annotation;
          end;
        FlowingWellConductancePosition:
          begin
            FlowingWellConductance := Expression.DoubleResult;
            FlowingWellConductanceAnnotation := ACell.Annotation;
          end;
        RatePosition:
          begin
            Rate := Expression.DoubleResult;
            RateAnnotation := ACell.Annotation;
          end;
        WellHeadPosition:
          begin
            WellHead := Expression.DoubleResult;
            WellHeadAnnotation := ACell.Annotation;
          end;
        HeadLimitPosition:
          begin
            HeadLimit := Expression.DoubleResult;
            HeadLimitAnnotation := ACell.Annotation;
          end;
        MinRatePosition:
          begin
            MinRate := Expression.DoubleResult;
            MinRateAnnotation := ACell.Annotation;
          end;
        MaxRatePosition:
          begin
            MaxRate := Expression.DoubleResult;
            MaxRateAnnotation := ACell.Annotation;
          end;
        PumpElevationPosition:
          begin
            PumpElevation := Expression.DoubleResult;
            PumpElevationAnnotation := ACell.Annotation;
          end;
        ScalingLengthPosition:
          begin
            ScalingLength := Expression.DoubleResult;
            ScalingLengthAnnotation := ACell.Annotation;
          end;
        else
          Assert(False);
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
begin
  inherited;
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

function TMawWellCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TMawTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawTimeListLink;
    Link.FFlowingWellConductance.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMawTimeListLink;
      Link.FFlowingWellConductance.Invalidate;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawTimeListLink;
    Link.FFlowingWellElevation.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMawTimeListLink;
      Link.FFlowingWellElevation.Invalidate;
    end;
  end;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawTimeListLink;
    Link.FHeadLimit.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMawTimeListLink;
      Link.FHeadLimit.Invalidate;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawTimeListLink;
    Link.FMaxRate.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMawTimeListLink;
      Link.FMaxRate.Invalidate;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawTimeListLink;
    Link.FMinRate.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMawTimeListLink;
      Link.FMinRate.Invalidate;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawTimeListLink;
    Link.FPumpElevation.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMawTimeListLink;
      Link.FPumpElevation.Invalidate;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawTimeListLink;
    Link.FRate.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMawTimeListLink;
      Link.FRate.Invalidate;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawTimeListLink;
    Link.FScalingLength.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMawTimeListLink;
      Link.FScalingLength.Invalidate;
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
    Link := TimeListLink.GetLink(PhastModel) as TMawTimeListLink;
    Link.FWellHead.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMawTimeListLink;
      Link.FWellHead.Invalidate;
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
begin
  case Index of
    FlowingWellElevationPosition:
      result := FlowingWellElevationAnnotation;
    FlowingWellConductancePosition:
      result := FlowingWellConductanceAnnotation;
    RatePosition:
      result := RateAnnotation;
    WellHeadPosition:
      result := WellHeadAnnotation;
    HeadLimitPosition:
      result := HeadLimitAnnotation;
    MinRatePosition:
      result := MinRateAnnotation;
    MaxRatePosition:
      result := MaxRateAnnotation;
    PumpElevationPosition:
      result := PumpElevationAnnotation;
    ScalingLengthPosition:
      result := ScalingLengthAnnotation;
    else
      Assert(False);
  end;
end;

function TMawCell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    FlowingWellElevationPosition:
      result := FlowingWellElevation;
    FlowingWellConductancePosition:
      result := FlowingWellConductance;
    RatePosition:
      result := Rate;
    WellHeadPosition:
      result := WellHead;
    HeadLimitPosition:
      result := HeadLimit;
    MinRatePosition:
      result := MinRate;
    MaxRatePosition:
      result := MaxRate;
    PumpElevationPosition:
      result := PumpElevation;
    ScalingLengthPosition:
      result := ScalingLength;
    else
      Assert(False);
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

procedure TMawCell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

initialization
  InitializeMawObNames;

finalization
  MawObNames.Free;

end.
