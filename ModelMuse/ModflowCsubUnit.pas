unit ModflowCsubUnit;

interface

uses
  GoPhastTypes, System.Classes, ModflowCellUnit, System.ZLib,
  ModflowBoundaryUnit,
  FormulaManagerUnit, FormulaManagerInterfaceUnit,
  OrderedCollectionUnit, RbwParser,
  RealListUnit, System.SysUtils, SubscriptionUnit,
  OrderedCollectionInterfaceUnit, Modflow6DynamicTimeSeriesInterfaceUnit;

type
  TCSubOb = (coCSub, coInelastCSub, coElastCSub, coCoarseCSub, coCSubCell,
    coWcompCSubCell, coSk, coSke, coSkCell, coSkeCell, coEStressCell,
    coGStressCell, coIntbedComp, coInelastComp, coElastComp, coCoarseCompaction,
    coCompCell, coThickness, coCoarseThickness, coThickCell, coTheta,
    coCoarseTheta, coThetaCell, coDelayFlowTop, coDelayFlowBot,
    coDelayHead,
    coDelayGStress, coDelayEStress, coDelayPreConStress, coDelayComp,
    coDelayThickness, coDelayTheta,
    coPreConsStressCell);
  TSubObsSet = set of TCSubOb;

//  TCSubObs = set of TCSubOb;
  TCSubObs = class(TPersistent)
  private
    FCSubObsOptions: TSubObsSet;
    function GetCSubObsOptions: string;
    procedure SetCSubObsOptions(const Value: string);
    function CSubObToString(CSubOb: TCSubOb): string;
    function StringToCSubOb(AString: string): TCSubOb;
  public
    procedure Assign(Source: TPersistent); override;
    property CSubObsSet: TSubObsSet read FCSubObsOptions write FCSubObsOptions;
  published
    property CSubObsOptions: string read GetCSubObsOptions write SetCSubObsOptions;
  end;

  TCSubPackageData = class(TFormulaOrderedItem)
  private
    FInterbedSystemName: string;
    FUsed: Boolean;
    FInterbed: TObject;
    FDelayKv: IFormulaObject;
    FEquivInterbedNumber: IFormulaObject;
    FInitialDelayHeadOffset: IFormulaObject;
    FInitialElasticSpecificStorage: IFormulaObject;
    FInitialInelasticSpecificStorage: IFormulaObject;
    FInitialOffset: IFormulaObject;
    FInitialPorosity: IFormulaObject;
    FThickness: IFormulaObject;
    function GetInterbedSystemName: string;
    procedure SetUsed(const Value: Boolean);
    procedure SetInterbed(const Value: TObject);
    function GetDelayKv: string;
    function GetEquivInterbedNumber: string;
    function GetInitialDelayHeadOffset: string;
    function GetInitialElasticSpecificStorage: string;
    function GetInitialInelasticSpecificStorage: string;
    function GetInitialOffset: string;
    function GetInitialPorosity: string;
    function GetThickness: string;
    procedure SetDelayKv(const Value: string);
    procedure SetEquivInterbedNumber(const Value: string);
    procedure SetInitialDelayHeadOffset(const Value: string);
    procedure SetInitialElasticSpecificStorage(const Value: string);
    procedure SetInitialInelasticSpecificStorage(const Value: string);
    procedure SetInitialOffset(const Value: string);
    procedure SetInitialPorosity(const Value: string);
    procedure SetInterbedSystemName(const Value: string);
    procedure SetThickness(const Value: string);
  protected
    procedure Loaded;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function GetScreenObject: TObject; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Interbed: TObject read FInterbed write SetInterbed;
    procedure CreateFormulaObjects;
    property StoredInterbedSystemName: string read FInterbedSystemName;
  published
    property InterbedSystemName: string read GetInterbedSystemName write SetInterbedSystemName;
    property Used: Boolean read FUsed write SetUsed;
    {pcs0—is the initial offset from the calculated initial effective stress or initial preconsolidation
    stress in the interbed, in units of height of a column of water. PCS0 is the initial
    preconsolidation stress if SPECIFIED INITIAL INTERBED STATE or SPECIFIED
    INITIAL PRECONSOLIDATION STRESS are specified in the OPTIONS block. If
    HEAD BASED is specified in the OPTIONS block, PCS0 is the initial offset from the calculated
    initial head or initial preconsolidation head in the CSUB interbed and the initial preconsolidation
    stress is calculated from the calculated initial effective stress or calculated initial geostatic stress,
    respectively.}
    property InitialOffset: string read GetInitialOffset write SetInitialOffset;
    {thick frac—is the interbed thickness or cell fraction of the interbed. Interbed thickness is specified as
    a fraction of the cell thickness if CELL FRACTION is specified in the OPTIONS block.}
    property Thickness: string read GetThickness write SetThickness;
    {rnb—is the interbed material factor equivalent number of interbeds in the interbed system represented
    by the interbed. RNB must be greater than or equal to 1 if CDELAY is DELAY. Otherwise, RNB
    can be any value.}
    property EquivInterbedNumber: string read GetEquivInterbedNumber write SetEquivInterbedNumber;
    {ssv cc—is the initial inelastic specific storage or compression index of the interbed. The compression
    index is specified if COMPRESSION INDICES is specified in the OPTIONS block. Specified
    or calculated interbed inelastic specific storage values are not adjusted from initial values if
    HEAD BASED is specified in the OPTIONS block.}
    property InitialInelasticSpecificStorage: string read GetInitialInelasticSpecificStorage write SetInitialInelasticSpecificStorage;
    //sse cr—is the initial elastic coarse-grained material specific storage or recompression index of the
    //interbed. The recompression index is specified if COMPRESSION INDICES is specified in the
    //OPTIONS block. Specified or calculated interbed elastic specific storage values are not adjusted
    //from initial values if HEAD BASED is specified in the OPTIONS block.
    property InitialElasticSpecificStorage: string read GetInitialElasticSpecificStorage write SetInitialElasticSpecificStorage;
    //theta—is the initial porosity of the interbed.
    property InitialPorosity: string read GetInitialPorosity write SetInitialPorosity;
    //kv—is the vertical hydraulic conductivity of the delay interbed. KV must be greater than 0 if CDELAY
    //is DELAY. Otherwise, KV can be any value.
    property DelayKv: string read GetDelayKv write SetDelayKv;
    //h0—is the initial offset from the head in cell cellid or the initial head in the delay interbed. H0
    //is the initial head in the delay bed if SPECIFIED INITIAL INTERBED STATE or SPECIFIED
    //INITIAL DELAY HEAD are specified in the OPTIONS block. H0 can be any value if CDELAY
    //is NODELAY.
    property InitialDelayHeadOffset: string read GetInitialDelayHeadOffset write SetInitialDelayHeadOffset;
  end;

  TCSubPackageDataCollection = class(TCustomObjectOrderedCollection)
  private
    function GetItem(Index: integer): TCSubPackageData;
    procedure SetItem(Index: integer; const Value: TCSubPackageData);
    function GetUsed: Boolean;
  public
    property Used: Boolean read GetUsed;
    constructor Create(Model: IModelForTOrderedCollection; ScreenObject: TObject);
    property Items[Index: integer]: TCSubPackageData read GetItem write SetItem; default;
    function Add: TCSubPackageData;
    function IsSame(AnOrderedCollection: TOrderedCollection): boolean; override;
    procedure Loaded;
  end;

  TCSubRecord = record
    Cell: TCellLocation;
    StressOffset: double;
    StartingTime: double;
    EndingTime: double;
    StressOffsetAnnotation: string;
    StressOffsetPest: string;
    StressOffsetPestSeriesName: string;
    StressOffsetPestSeriesMethod: TPestParamMethod;
    StressOffsetTimeSeriesName: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  // @name is an array of @link(TCsubRecord)s.
  TCSubArray = array of TCSubRecord;

  // @name extends @link(TCustomBoundaryStorage) by adding the locations
  // and values of series of wells.
  TCSubStorage = class(TCustomBoundaryStorage)
  private
    FCSubArray: TCSubArray;
    function GetCSubArray: TCSubArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property CSubArray: TCSubArray read GetCSubArray;
  end;

  TCSubItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(StressOffset).
    FStressOffset: IFormulaObject;
    // See @link(StressOffset).
    procedure SetStressOffset(const Value: string);
    function GetStressOffset: string;
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
    Destructor Destroy; override;
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
  published
    // @name is the formula used to set the StressOffset
    // of this boundary.
    property StressOffset: string read GetStressOffset write SetStressOffset;
  end;

  TCSubTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the StressOffsets for a series of
    // CSUB boundaries over a series of time intervals.
    FStressOffsetData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  TCSubCollection = class(TCustomMF_ListBoundColl)
  private
    procedure InvalidateStressOffsetData(Sender: TObject);
    function GetItem(Index: Integer): TCSubItem;
    procedure SetItem(Index: Integer; const Value: TCSubItem);
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
   public
     function Add: TCSubItem;
     property Items[Index: Integer]: TCSubItem read GetItem write SetItem; default;
  end;

  TCSubCell = class(TValueCell)
  private
    FValues: TCSubRecord;
    FStressPeriod: integer;
    function GetStressOffset: double;
    function GetStressOffsetAnnotation: string;
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
    procedure Cache(Comp: TCompressionStream; Strings: TStringList); override;
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList); override;
    function GetSection: integer; override;
    procedure RecordStrings(Strings: TStringList); override;
    function GetPestName(Index: Integer): string; override;
    function GetPestSeriesMethod(Index: Integer): TPestParamMethod; override;
    function GetPestSeriesName(Index: Integer): string; override;
    function GetMf6TimeSeriesName(Index: Integer): string; override;
    procedure SetMf6TimeSeriesName(Index: Integer; const Value: string); override;
  public
    property StressOffset: double read GetStressOffset;
    property StressOffsetAnnotation: string read GetStressOffsetAnnotation;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
  end;

  TCSubBoundary = class(TModflowParamBoundary)
  private
    FCSubPackageData: TCSubPackageDataCollection;
    FPestStressOffsetMethod: TPestParamMethod;
    FPestStressOffsetFormula: IFormulaObject;
    FPestStressOffsetObserver: TObserver;
    FUsedObserver: TObserver;
    procedure SetCSubPackageData(const Value: TCSubPackageDataCollection);
    procedure InvalidateStressOffsetData(Sender: TObject);
    function GetPestStressOffsetFormula: string;
    function GetPestStressOffsetObserver: TObserver;
    procedure SetPestStressOffsetFormula(const Value: string);
    procedure SetPestStressOffsetMethod(const Value: TPestParamMethod);
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TCSubCell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    // See @link(TModflowParamBoundary.ModflowParamItemClass
    // TModflowParamBoundary.ModflowParamItemClass).
    class function ModflowParamItemClass: TModflowParamItemClass; override;
    function ParameterType: TParameterType; override;

    procedure HandleChangedValue(Observer: TObserver); //override;
    function GetUsedObserver: TObserver; //override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure CreateFormulaObjects; //override;
    function BoundaryObserverPrefix: string; override;
    procedure CreateObservers; //override;
    function GetPestBoundaryFormula(FormulaIndex: integer): string; override;
    procedure SetPestBoundaryFormula(FormulaIndex: integer;
      const Value: string); override;
    function GetPestBoundaryMethod(FormulaIndex: integer): TPestParamMethod; override;
    procedure SetPestBoundaryMethod(FormulaIndex: integer;
      const Value: TPestParamMethod); override;
    property PestStressOffsetObserver: TObserver read GetPestStressOffsetObserver;
  public
    constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TCSubStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW Well parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TCSubStorage) in @link(TCustomMF_BoundColl.Boundaries
    // Param.Param.Boundaries)
    // Those represent parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    procedure InvalidateDisplay; override;
    procedure UpdateTimes(Times: TRealList; StartTestTime, EndTestTime: double;
      var StartRangeExtended, EndRangeExtended: boolean; AModel: TBaseModel); override;
    function Used: boolean; override;
    procedure Loaded;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
  published
    property CSubPackageData: TCSubPackageDataCollection read FCSubPackageData write SetCSubPackageData;
    property PestStressOffsetFormula: string read GetPestStressOffsetFormula
      write SetPestStressOffsetFormula;
    property PestStressOffsetMethod: TPestParamMethod read FPestStressOffsetMethod
      write SetPestStressOffsetMethod;
  end;

function TryGetCSubOb(const CSubObName: string; var CSubOb: TCSubOb): Boolean;
function CSubObToString(const CSubOb: TCSubOb): string;
Procedure FillCSubSeriesNames(AList: TStrings);

const
  CsubStressOffsetPosition = 0;

implementation

uses
  frmGoPhastUnit, PhastModelUnit, ScreenObjectUnit, GIS_Functions,
  frmErrorsAndWarningsUnit, ModflowTimeUnit,
  ModflowPackageSelectionUnit, ModflowCSubInterbed, DataSetUnit,
  DataArrayManagerUnit, DataSetNamesUnit, CellLocationUnit;

const
  CSubObName: array[TCSubOb] of string =
   ('CSub_interbed_flow', 'inelastic-csub',
    'elastic-csub', 'coarse-csub',
    'csub-cell', 'wcomp-csub-cell',
    'sk',
    'ske', 'sk-cell',
    'ske-cell', 'estress-cell',
    'gstress-cell', 'interbed-compaction',
    'inelastic-compaction', 'elastic-compaction',
    'coarse-compaction', 'compaction-cell',
    'thickness', 'coarse-thickness',
    'thickness-cell', 'theta',
    'coarse-theta', 'theta-cell',
    'delay-flowtop',
    'delay-flowbot',
    'delay-head',
    'delay-head', 'delay-estress', 'delay-preconstress', 'delay-compaction',
    'delay-thickness', 'delay-theta',
    'preconstress-cell');

var
  CSubObNames: TStringList;

procedure InitializeCSubObNames;
var
  Index: TCSubOb;
begin
  CSubObNames := TStringList.Create;
  CSubObNames.CaseSensitive := False;
  for Index := Low(TCSubOb) to High(TCSubOb) do
  begin
    CSubObNames.Add(CSubObName[Index]);
  end;
end;

function TryGetCSubOb(const CSubObName: string; var CSubOb: TCSubOb): Boolean;
var
  index: Integer;
begin
  index := CSubObNames.IndexOf(CSubObName);
  result := index >= 0;
  if result then
  begin
    CSubOb := TCSubOb(index);
  end;
end;

Procedure FillCSubSeriesNames(AList: TStrings);
begin
  AList.Assign(CSubObNames);
end;

function CSubObToString(const CSubOb: TCSubOb): string;
begin
  result := CSubObName[CSubOb];
end;

resourcestring
  StrStressOffsetMultip = 'Stress offset multiplier';
  StrStressOffsetSetTo = 'Stress offset set to zero because of a math error';

var
  CSubOptionNames: TStringList;

{ TCSubPackageData }

procedure TCSubPackageData.Assign(Source: TPersistent);
var
  SubSource: TCSubPackageData;
begin
  if Source is TCSubPackageData then
  begin
    SubSource := TCSubPackageData(Source);
    InterbedSystemName := SubSource.InterbedSystemName;
    Used := SubSource.Used;
    if SubSource.InitialOffset <> '' then
    begin
      InitialOffset := SubSource.InitialOffset;
    end;
    if SubSource.Thickness <> '' then
    begin
      Thickness := SubSource.Thickness;
    end;
    if SubSource.EquivInterbedNumber <> '' then
    begin
      EquivInterbedNumber := SubSource.EquivInterbedNumber;
    end;
    if SubSource.InitialInelasticSpecificStorage <> '' then
    begin
      InitialInelasticSpecificStorage := SubSource.InitialInelasticSpecificStorage;
    end;
    if SubSource.InitialElasticSpecificStorage <> '' then
    begin
      InitialElasticSpecificStorage := SubSource.InitialElasticSpecificStorage;
    end;
    if SubSource.InitialPorosity <> '' then
    begin
      InitialPorosity := SubSource.InitialPorosity;
    end;
    if SubSource.DelayKv <> '' then
    begin
      DelayKv := SubSource.DelayKv;
    end;
    if SubSource.InitialDelayHeadOffset <> '' then
    begin
      InitialDelayHeadOffset := SubSource.InitialDelayHeadOffset;
    end;
//    InterbedSystemName := SubSource.InterbedSystemName;
  end
  else
  begin
    inherited;
  end;
end;

constructor TCSubPackageData.Create(Collection: TCollection);
begin
  inherited;
  CreateFormulaObjects;
  
  InitialOffset := '0';
  Thickness := '0';
  EquivInterbedNumber := '1';
  InitialInelasticSpecificStorage := '0.01';
  InitialElasticSpecificStorage := '1E-5';
  InitialPorosity := '0.25';
  DelayKv := '2.5E-6';
  InitialDelayHeadOffset := '0';

{
    property InitialOffset: string read GetInitialOffset write SetInitialOffset;
    {thick frac—is the interbed thickness or cell fraction of the interbed. Interbed thickness is specified as
    a fraction of the cell thickness if CELL FRACTION is specified in the OPTIONS block.}
//    property Thickness: string read GetThickness write SetThickness;
    {rnb—is the interbed material factor equivalent number of interbeds in the interbed system represented
    by the interbed. RNB must be greater than or equal to 1 if CDELAY is DELAY. Otherwise, RNB
    can be any value.}
//    property EquivInterbedNumber: string read GetEquivInterbedNumber write SetEquivInterbedNumber;
    {ssv cc—is the initial inelastic specific storage or compression index of the interbed. The compression
    index is specified if COMPRESSION INDICES is specified in the OPTIONS block. Specified
    or calculated interbed inelastic specific storage values are not adjusted from initial values if
    HEAD BASED is specified in the OPTIONS block.}
//    property InitialInelasticSpecificStorage: string read GetInitialInelasticSpecificStorage write SetInitialInelasticSpecificStorage;
    //sse cr—is the initial elastic coarse-grained material specific storage or recompression index of the
    //interbed. The recompression index is specified if COMPRESSION INDICES is specified in the
    //OPTIONS block. Specified or calculated interbed elastic specific storage values are not adjusted
    //from initial values if HEAD BASED is specified in the OPTIONS block.
//    property InitialElasticSpecificStorage: string read GetInitialElasticSpecificStorage write SetInitialElasticSpecificStorage;
    //theta—is the initial porosity of the interbed.
//    property InitialPorosity: string read GetInitialPorosity write SetInitialPorosity;
    //kv—is the vertical hydraulic conductivity of the delay interbed. KV must be greater than 0 if CDELAY
    //is DELAY. Otherwise, KV can be any value.
//    property DelayKv: string read GetDelayKv write SetDelayKv;
    //h0—is the initial offset from the head in cell cellid or the initial head in the delay interbed. H0
    //is the initial head in the delay bed if SPECIFIED INITIAL INTERBED STATE or SPECIFIED
    //INITIAL DELAY HEAD are specified in the OPTIONS block. H0 can be any value if CDELAY
    //is NODELAY.
//    property InitialDelayHeadOffset: string read GetInitialDelayHeadOffset write SetInitialDelayHeadOffset;
//}
end;

procedure TCSubPackageData.CreateFormulaObjects;
begin

  FDelayKv := CreateBlockFormulaObject(dso3D) as TFormulaObject;
  FEquivInterbedNumber := CreateBlockFormulaObject(dso3D) as TFormulaObject;
  FInitialDelayHeadOffset := CreateBlockFormulaObject(dso3D) as TFormulaObject;
  FInitialElasticSpecificStorage := CreateBlockFormulaObject(dso3D) as TFormulaObject;
  FInitialInelasticSpecificStorage := CreateBlockFormulaObject(dso3D) as TFormulaObject;
  FInitialOffset := CreateBlockFormulaObject(dso3D) as TFormulaObject;
  FInitialPorosity := CreateBlockFormulaObject(dso3D) as TFormulaObject;
  FThickness := CreateBlockFormulaObject(dso3D) as TFormulaObject;
end;

destructor TCSubPackageData.Destroy;
begin
  InitialOffset := '0';
  Thickness := '0';
  EquivInterbedNumber := '0';
  InitialInelasticSpecificStorage := '0';
  InitialElasticSpecificStorage := '0';
  InitialPorosity := '0';
  DelayKv := '0';
  InitialDelayHeadOffset := '0';

  if frmGoPhast.PhastModel <> nil then
  begin
    frmGoPhast.PhastModel.FormulaManager.Remove(FDelayKv, nil, nil, self);
    frmGoPhast.PhastModel.FormulaManager.Remove(FEquivInterbedNumber, nil, nil, self);
    frmGoPhast.PhastModel.FormulaManager.Remove(FInitialDelayHeadOffset, nil, nil, self);
    frmGoPhast.PhastModel.FormulaManager.Remove(FInitialElasticSpecificStorage, nil, nil, self);
    frmGoPhast.PhastModel.FormulaManager.Remove(FInitialInelasticSpecificStorage, nil, nil, self);
    frmGoPhast.PhastModel.FormulaManager.Remove(FInitialOffset, nil, nil, self);
    frmGoPhast.PhastModel.FormulaManager.Remove(FInitialPorosity, nil, nil, self);
    frmGoPhast.PhastModel.FormulaManager.Remove(FThickness, nil, nil, self);
  end;

  inherited;
end;

function TCSubPackageData.GetDelayKv: string;
begin
  result := FDelayKv.Formula;
end;

function TCSubPackageData.GetEquivInterbedNumber: string;
begin
  result := FEquivInterbedNumber.Formula;
end;

function TCSubPackageData.GetInitialDelayHeadOffset: string;
begin
  result := FInitialDelayHeadOffset.Formula;
end;

function TCSubPackageData.GetInitialElasticSpecificStorage: string;
begin
  result := FInitialElasticSpecificStorage.Formula;
end;

function TCSubPackageData.GetInitialInelasticSpecificStorage: string;
begin
  result := FInitialInelasticSpecificStorage.Formula;
end;

function TCSubPackageData.GetInitialOffset: string;
begin
  result := FInitialOffset.Formula;
end;

function TCSubPackageData.GetInitialPorosity: string;
begin
  result := FInitialPorosity.Formula;
end;

function TCSubPackageData.GetInterbedSystemName: string;
begin
  if FInterbed <> nil then
  begin
    result := (FInterbed as TCSubInterbed).Name;
  end
  else
  begin
    Result := FInterbedSystemName
  end;
end;

function TCSubPackageData.GetScreenObject: TObject;
begin
  result := (Collection as TCSubPackageDataCollection).ScreenObject as TObject;
end;

function TCSubPackageData.GetThickness: string;
begin
  result := FThickness.Formula;
end;

function TCSubPackageData.IsSame(AnotherItem: TOrderedItem): boolean;
var
  CSub: TCSubPackageData;
begin
  if AnotherItem is TCSubPackageData then
  begin
    CSub := AnotherItem as TCSubPackageData;
    result := (InterbedSystemName = CSub.InterbedSystemName)
      and (Used = CSub.Used)
      and (InitialOffset = CSub.InitialOffset)
      and (Thickness = CSub.Thickness)
      and (EquivInterbedNumber = CSub.EquivInterbedNumber)
      and (InitialInelasticSpecificStorage = CSub.InitialInelasticSpecificStorage)
      and (InitialElasticSpecificStorage = CSub.InitialElasticSpecificStorage)
      and (InitialPorosity = CSub.InitialPorosity)
      and (DelayKv = CSub.DelayKv)
      and (InitialDelayHeadOffset = CSub.InitialDelayHeadOffset)
  end
  else
  begin
    result := False;
  end;
end;

procedure TCSubPackageData.Loaded;
var
  LocalModel: TCustomModel;
  Interbeds: TCSubInterbeds;
  InterbedIndex: Integer;
  LocalScreenObject: TScreenObject;
  DataArrayManager: TDataArrayManager;
  ADataArray: TDataArray;
  LocalInterbed: TCSubInterbed;
  DataSetIndex: Integer;
  FoundInterbed: Boolean;
begin
  LocalModel := (Collection as TCSubPackageDataCollection).Model as TCustomModel;
  if LocalModel <> nil then
  begin
    Interbeds := LocalModel.ModflowPackages.CSubPackage.Interbeds;
    if Interbed = nil then
    begin
      for InterbedIndex := 0 to Interbeds.Count - 1 do
      begin
        if Interbeds[InterbedIndex].Name = InterbedSystemName then
        begin
          Interbed := Interbeds[InterbedIndex];
          break;
        end;
      end;
    end
    else
    begin
      FoundInterbed := False;
      for InterbedIndex := 0 to Interbeds.Count - 1 do
      begin
        if Interbeds[InterbedIndex] = Interbed then
        begin
          FoundInterbed := True;
          break;
        end;
      end;
      if not FoundInterbed then
      begin
        Interbed := nil;
      end;
    end;

    if (Interbed <> nil) and Used then
    begin
      LocalInterbed := Interbed as TCSubInterbed;
      DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
      LocalScreenObject := ScreenObject as TScreenObject;

      if LocalInterbed.DelayKvName <> '' then
      begin
        ADataArray := DataArrayManager.GetDataSetByName(LocalInterbed.DelayKvName);
      end
      else
      begin
        ADataArray := nil;
      end;
      if (LocalInterbed.InterbedType = itDelay) {and (ADataArray <> nil} then
      begin
        Assert(ADataArray <> nil);
        DataSetIndex := LocalScreenObject.AddDataSet(ADataArray);
        LocalScreenObject.DataSetFormulas[DataSetIndex] := DelayKv;
      end
      else
      begin
        if ADataArray <> nil then
        begin
          LocalScreenObject.RemoveDataSet(ADataArray);
        end;
      end;

      if LocalInterbed.EquivInterbedNumberName <> '' then
      begin
        ADataArray := DataArrayManager.GetDataSetByName(LocalInterbed.EquivInterbedNumberName);
      end
      else
      begin
        ADataArray := nil;
      end;
      if (LocalInterbed.InterbedType = itDelay) {and (ADataArray <> nil)} then
      begin
        Assert(ADataArray <> nil);
        DataSetIndex := LocalScreenObject.AddDataSet(ADataArray);
        LocalScreenObject.DataSetFormulas[DataSetIndex] := EquivInterbedNumber;
      end
      else
      begin
        if ADataArray <> nil then
        begin
          LocalScreenObject.RemoveDataSet(ADataArray);
        end;
      end;

      if LocalInterbed.InitialDelayHeadOffset <> '' then
      begin
        ADataArray := DataArrayManager.GetDataSetByName(LocalInterbed.InitialDelayHeadOffset);
      end
      else
      begin
        ADataArray := nil;
      end;
      if (LocalInterbed.InterbedType = itDelay) {and (ADataArray <> nil)} then
      begin
        Assert(ADataArray <> nil);
        DataSetIndex := LocalScreenObject.AddDataSet(ADataArray);
        LocalScreenObject.DataSetFormulas[DataSetIndex] := InitialDelayHeadOffset;
      end
      else
      begin
        if ADataArray <> nil then
        begin
          LocalScreenObject.RemoveDataSet(ADataArray);
        end;
      end;

      ADataArray := DataArrayManager.GetDataSetByName(LocalInterbed.InitialElasticSpecificStorage);
      Assert(ADataArray <> nil);
      DataSetIndex := LocalScreenObject.AddDataSet(ADataArray);
      LocalScreenObject.DataSetFormulas[DataSetIndex] := InitialElasticSpecificStorage;

      ADataArray := DataArrayManager.GetDataSetByName(LocalInterbed.InitialInelasticSpecificStorage);
      Assert(ADataArray <> nil);
      DataSetIndex := LocalScreenObject.AddDataSet(ADataArray);
      LocalScreenObject.DataSetFormulas[DataSetIndex] := InitialInelasticSpecificStorage;

      ADataArray := DataArrayManager.GetDataSetByName(LocalInterbed.InitialOffset);
      Assert(ADataArray <> nil);
      DataSetIndex := LocalScreenObject.AddDataSet(ADataArray);
      LocalScreenObject.DataSetFormulas[DataSetIndex] := InitialOffset;

      ADataArray := DataArrayManager.GetDataSetByName(LocalInterbed.InitialPorosity);
      Assert(ADataArray <> nil);
      DataSetIndex := LocalScreenObject.AddDataSet(ADataArray);
      LocalScreenObject.DataSetFormulas[DataSetIndex] := InitialPorosity;

      ADataArray := DataArrayManager.GetDataSetByName(LocalInterbed.Thickness);
      Assert(ADataArray <> nil);
      DataSetIndex := LocalScreenObject.AddDataSet(ADataArray);
      LocalScreenObject.DataSetFormulas[DataSetIndex] := Thickness;

      ADataArray := DataArrayManager.GetDataSetByName(LocalInterbed.CSubBoundName);
      Assert(ADataArray <> nil);
      DataSetIndex := LocalScreenObject.AddDataSet(ADataArray);
      LocalScreenObject.DataSetFormulas[DataSetIndex] := Format('"%s"', [LocalScreenObject.Name]);
    end;
  end;
end;

procedure TCSubPackageData.SetDelayKv(const Value: string);
begin
  if FDelayKv.Formula <> Value then
  begin
    InvalidateModel;
    frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
      FDelayKv, Value, frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
      nil, nil, self);
  end;
end;

procedure TCSubPackageData.SetEquivInterbedNumber(const Value: string);
begin
  if FEquivInterbedNumber.Formula <> Value then
  begin
    InvalidateModel;
    frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
      FEquivInterbedNumber, Value, frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
      nil, nil, self);
  end;
end;

procedure TCSubPackageData.SetInitialDelayHeadOffset(const Value: string);
begin
  if FInitialDelayHeadOffset.Formula <> Value then
  begin
    InvalidateModel;
    frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
      FInitialDelayHeadOffset, Value, frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
      nil, nil, self);
  end;
end;

procedure TCSubPackageData.SetInitialElasticSpecificStorage(
  const Value: string);
begin
  if FInitialElasticSpecificStorage.Formula <> Value then
  begin
    InvalidateModel;
    frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
      FInitialElasticSpecificStorage, Value, frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
      nil, nil, self);
  end;
end;

procedure TCSubPackageData.SetInitialInelasticSpecificStorage(
  const Value: string);
begin
  if FInitialInelasticSpecificStorage.Formula <> Value then
  begin
    InvalidateModel;
    frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
      FInitialInelasticSpecificStorage, Value, frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
      nil, nil, self);
  end;
end;

procedure TCSubPackageData.SetInitialOffset(const Value: string);
begin
  if FInitialOffset.Formula <> Value then
  begin
    InvalidateModel;
    frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
      FInitialOffset, Value, frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
      nil, nil, self);
  end;
end;

procedure TCSubPackageData.SetInitialPorosity(const Value: string);
begin
  if FInitialPorosity.Formula <> Value then
  begin
    InvalidateModel;
    frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
      FInitialPorosity, Value, frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
      nil, nil, self);
  end;
end;

procedure TCSubPackageData.SetInterbed(const Value: TObject);
begin
  FInterbed := Value;
  if FInterbed <> nil then
  begin
    FInterbedSystemName := (FInterbed as TCSubInterbed).Name;
  end;
end;

procedure TCSubPackageData.SetInterbedSystemName(const Value: string);
begin
  FInterbedSystemName := Value;
end;

procedure TCSubPackageData.SetThickness(const Value: string);
begin
  if FThickness.Formula <> Value then
  begin
    InvalidateModel;
    frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
      FThickness, Value, frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
      nil, nil, self);
  end;
end;

procedure TCSubPackageData.SetUsed(const Value: Boolean);
begin
  SetBooleanProperty(FUsed, Value);
end;

{ TCSubPackageDataCollection }

function TCSubPackageDataCollection.Add: TCSubPackageData;
begin
  result := inherited Add as TCSubPackageData;
end;

constructor TCSubPackageDataCollection.Create(Model: IModelForTOrderedCollection; ScreenObject: TObject);
begin
  inherited Create(TCSubPackageData, Model, ScreenObject as TScreenObject);
end;

function TCSubPackageDataCollection.GetItem(Index: integer): TCSubPackageData;
begin
  result := inherited Items[Index] as TCSubPackageData;
end;

function TCSubPackageDataCollection.GetUsed: Boolean;
var
  ItemIndex: Integer;
begin
  result := False;
  for ItemIndex := 0 to Count -1 do
  begin
    result := Items[ItemIndex].Used;
    if result then
    begin
      Exit;
    end;
  end;
end;

function TCSubPackageDataCollection.IsSame(
  AnOrderedCollection: TOrderedCollection): boolean;
begin
  result := AnOrderedCollection is TCSubPackageDataCollection;
  if result then
  begin
    result := inherited IsSame(AnOrderedCollection);
  end;
end;

procedure TCSubPackageDataCollection.Loaded;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].Loaded;
  end;
end;

procedure TCSubPackageDataCollection.SetItem(Index: integer;
  const Value: TCSubPackageData);
begin
  inherited Items[Index] := Value;
end;

{ TCsubRecord }

procedure TCSubRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, StressOffset);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Strings.IndexOf(StressOffsetAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(StressOffsetPest));
  WriteCompInt(Comp, Strings.IndexOf(StressOffsetPestSeriesName));
  WriteCompInt(Comp, Ord(StressOffsetPestSeriesMethod));
  WriteCompInt(Comp, Strings.IndexOf(StressOffsetTimeSeriesName));
end;

procedure TCSubRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(StressOffsetAnnotation);
  Strings.Add(StressOffsetPest);
  Strings.Add(StressOffsetPestSeriesName);
  Strings.Add(StressOffsetTimeSeriesName);
end;

procedure TCSubRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  StressOffset := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  StressOffsetAnnotation := Annotations[ReadCompInt(Decomp)];
  StressOffsetPest := Annotations[ReadCompInt(Decomp)];
  StressOffsetPestSeriesName := Annotations[ReadCompInt(Decomp)];
  StressOffsetPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  StressOffsetTimeSeriesName := Annotations[ReadCompInt(Decomp)];
end;

{ TCsubStorage }

procedure TCSubStorage.Clear;
begin
  SetLength(FCSubArray, 0);
  FCleared := True;
end;

function TCSubStorage.GetCSubArray: TCSubArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FCSubArray;
end;

procedure TCSubStorage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FCSubArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FCSubArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TCSubStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FCSubArray);
    for Index := 0 to Count - 1 do
    begin
      FCSubArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FCSubArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

{ TWellItem }

procedure TCSubItem.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if Source is TCSubItem then
  begin
    StressOffset := TCSubItem(Source).StressOffset;
  end;
  inherited;
end;

procedure TCSubItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TCSubCollection;
  PumpingRateObserver: TObserver;
begin
  ParentCollection := Collection as TCSubCollection;
  PumpingRateObserver := FObserverList[CsubStressOffsetPosition];
  PumpingRateObserver.OnUpToDateSet := ParentCollection.InvalidateStressOffsetData;
end;

function TCSubItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

procedure TCSubItem.CreateFormulaObjects;
begin
  FStressOffset := CreateFormulaObject(dso3D);
end;

destructor TCSubItem.Destroy;
begin
  StressOffset := '0';
  inherited;
end;

function TCSubItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    CsubStressOffsetPosition: result := StressOffset;
    else Assert(False);
  end;
end;

procedure TCSubItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  Assert(Sender = FStressOffset as TObject);
  List.Add(FObserverList[CsubStressOffsetPosition]);
end;

function TCSubItem.GetStressOffset: string;
begin
  Result := FStressOffset.Formula;
  ResetItemObserver(CsubStressOffsetPosition);
end;

procedure TCSubItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateCSubStressOffset(self);
  end;
end;

function TCSubItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TCSubItem;
begin
  result := (AnotherItem is TCSubItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TCSubItem(AnotherItem);
    result := (Item.StressOffset = StressOffset)
  end;
end;

procedure TCSubItem.RemoveFormulaObjects;
begin
  inherited;
  frmGoPhast.PhastModel.FormulaManager.Remove(FStressOffset,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TCSubItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  inherited;
  case Index of
    CsubStressOffsetPosition: StressOffset := Value;
    else Assert(False);
  end;
end;

procedure TCSubItem.SetStressOffset(const Value: string);
begin
  UpdateFormulaBlocks(Value, CsubStressOffsetPosition, FStressOffset);
end;

{ TCSubCollection }

function TCSubCollection.Add: TCSubItem;
begin
  result := inherited Add as TCSubItem;
end;

procedure TCSubCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TCSubStorage.Create(AModel));
end;

function TCSubCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Item: TCSubItem;
begin
  result := '';
  if FormulaIndex = 0 then
  begin
    Item := Items[ItemIndex] as TCSubItem;
    result := Item.StressOffset;
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TCSubCollection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel; PestSeries: TStringList;
  PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList);
begin
  inherited;
  Assert(False);
end;

procedure TCSubCollection.AssignCellList(CellAssignmentData: TCellAssignmentData);
var
  CSubStorage: TCSubStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
  LocalScreenObject: TScreenObject;
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
  Assert(BoundaryFunctionIndex = 0);
  Assert(Expression <> nil);

  CSubStorage := BoundaryStorage as TCSubStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);
    // 2. update locations
    try
      Expression.Evaluate;
      with CSubStorage.CSubArray[Index] do
      begin
        StressOffset := Expression.DoubleResult;
        StressOffsetAnnotation := ACell.Annotation;
        StressOffsetPest := PestName;
        StressOffsetPestSeriesName := PestSeriesName;
        StressOffsetPestSeriesMethod := PestSeriesMethod;
        StressOffsetTimeSeriesName := TimeSeriesName;
      end;
    except on E: EMathError do
      begin
        with CSubStorage.CSubArray[Index] do
        begin
          StressOffset := 0;
          StressOffsetAnnotation := StrStressOffsetSetTo;
          StressOffsetPest := PestName;
          StressOffsetPestSeriesName := PestSeriesName;
          StressOffsetPestSeriesMethod := PestSeriesMethod;
          StressOffsetTimeSeriesName := TimeSeriesName;
        end;
        LocalScreenObject := ScreenObject as TScreenObject;

        frmErrorsAndWarnings.AddError(AModel, StrStressOffsetSetTo,
          Format(StrObject0sLayerError,
          [LocalScreenObject.Name, ACell.Layer+1, ACell.Row+1,
          ACell.Column+1, E.Message]), LocalScreenObject);
      end;
    end;
  end;
end;

procedure TCSubCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  CSubStorage: TCSubStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  CSubStorage := BoundaryStorage as TCSubStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    if ACell.LgrEdge then
    begin
      Continue;
    end;
    with CSubStorage.CSubArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

function TCSubCollection.GetItem(Index: Integer): TCSubItem;
begin
  result := inherited Items[Index] as TCSubItem;
end;

class function TCSubCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TCSubTimeListLink;
end;

procedure TCSubCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateCSubStressOffset(self);
  end;
end;

procedure TCSubCollection.InvalidateStressOffsetData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TCSubTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TCSubTimeListLink;
    Link.FStressOffsetData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TCSubTimeListLink;
        Link.FStressOffsetData.Invalidate;
      end;
    end;
  end;
end;

class function TCSubCollection.ItemClass: TBoundaryItemClass;
begin
  result := TCSubItem;
end;

procedure TCSubCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TCSubStorage).FCSubArray, BoundaryCount);
  inherited;
end;

procedure TCSubCollection.SetItem(Index: Integer; const Value: TCSubItem);
begin
  inherited Items[Index] := Value;
end;

{ TCSubTimeListLink }

procedure TCSubTimeListLink.CreateTimeLists;
begin
  inherited;
  FStressOffsetData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStressOffsetData.NonParamDescription := StrStressOffset;
  FStressOffsetData.ParamDescription := StrStressOffsetMultip;
  if Model <> nil then
  begin
    FStressOffsetData.OnInvalidate := (Model as TCustomModel).InvalidateCSubStressOffset;
  end;
  AddTimeList(FStressOffsetData);
end;

destructor TCSubTimeListLink.Destroy;
begin
  FStressOffsetData.Free;
  inherited;
end;

{ TCSubCell }

procedure TCSubCell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  FValues.Cache(Comp, Strings);
  WriteCompInt(Comp, FStressPeriod);
end;

function TCSubCell.GetColumn: integer;
begin
  result := FValues.Cell.Column;
end;

function TCSubCell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TCSubCell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TCSubCell.GetLayer: integer;
begin
  result := FValues.Cell.Layer;
end;

function TCSubCell.GetMf6TimeSeriesName(Index: Integer): string;
begin
  if Index = 0 then
  begin
    result := FValues.StressOffsetTimeSeriesName;
  end
  else
  begin
    result := inherited;
    Assert(False);
  end;
end;

function TCSubCell.GetPestName(Index: Integer): string;
begin
  if Index = 0 then
  begin
    result := FValues.StressOffsetPest;
  end
  else
  begin
    result := inherited;
    Assert(False);
  end;
end;

function TCSubCell.GetPestSeriesMethod(Index: Integer): TPestParamMethod;
begin
  if Index = 0 then
  begin
    result := FValues.StressOffsetPestSeriesMethod;
  end
  else
  begin
    result := inherited;
    Assert(False);
  end;
end;

function TCSubCell.GetPestSeriesName(Index: Integer): string;
begin
  if Index = 0 then
  begin
    result := FValues.StressOffsetPestSeriesName;
  end
  else
  begin
    result := inherited;
    Assert(False);
  end;
end;

function TCSubCell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    0: result := StressOffsetAnnotation;
    else Assert(False);
  end;
end;

function TCSubCell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    0: result := StressOffset;
    else Assert(False);
  end;
end;

function TCSubCell.GetRow: integer;
begin
  result := FValues.Cell.Row;
end;

function TCSubCell.GetSection: integer;
begin
  result := FValues.Cell.Section;
end;

function TCSubCell.GetStressOffset: double;
begin
  result := FValues.StressOffset;
end;

function TCSubCell.GetStressOffsetAnnotation: string;
begin
  result := FValues.StressOffsetAnnotation;
end;

function TCSubCell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  CSubCell: TCSubCell;
begin
  result := AnotherCell is TCSubCell;
  if result then
  begin
    CSubCell := TCSubCell(AnotherCell);
    result :=
      (StressOffset = CSubCell.StressOffset)
      and (IFace = CSubCell.IFace)
      and (FValues.Cell = CSubCell.FValues.Cell);
  end;
end;

procedure TCSubCell.RecordStrings(Strings: TStringList);
begin
  inherited;
  FValues.RecordStrings(Strings);
end;

procedure TCSubCell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;
  FValues.Restore(Decomp, Annotations);
  FStressPeriod := ReadCompInt(Decomp);
end;

procedure TCSubCell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TCSubCell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TCSubCell.SetMf6TimeSeriesName(Index: Integer; const Value: string);
begin
  if Index = 0 then
  begin
    FValues.StressOffsetTimeSeriesName := Value;
  end
  else
  begin
    inherited;
    Assert(False);
  end;
end;

procedure TCSubCell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TCSubBoundary }

procedure TCSubBoundary.Assign(Source: TPersistent);
var
  SourceCSub: TCSubBoundary;
begin
  if Source is TCSubBoundary then
  begin
    SourceCSub := TCSubBoundary(Source);
    CSubPackageData := SourceCSub.CSubPackageData;

    PestStressOffsetFormula := SourceCSub.PestStressOffsetFormula;
    PestStressOffsetMethod := SourceCSub.PestStressOffsetMethod;
  end;
  Loaded;
  inherited;
end;

procedure TCSubBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TCSubCell;
  BoundaryValues: TCSubRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TCSubStorage;
  LocalModel: TCustomModel;
  LocalScreenObject: TScreenObject;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TCSubStorage;
  Assert(ScreenObject <> nil);
  LocalScreenObject := ScreenObject as TScreenObject;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TCSubCell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.CSubArray) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.CSubArray)
      end;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.CSubArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.CSubArray[BoundaryIndex];
        if FCurrentParameter <> nil then
        begin
          BoundaryValues.StressOffset :=
            BoundaryValues.StressOffset * FCurrentParameter.Value;
//          BoundaryValues.StressOffsetAnnotation :=
//            BoundaryValues.StressOffsetAnnotation
//            + ' multiplied by the parameter value for "'+ FCurrentParameter.ParameterName + '."';
          BoundaryValues.StressOffsetAnnotation := Format(Str0sMultipliedByT, [
            BoundaryValues.StressOffsetAnnotation, FCurrentParameter.ParameterName]);
        end;
        Cell := TCSubCell.Create;
        Assert(ScreenObject <> nil);
        Cell.IFace := LocalScreenObject.IFace;
        Cells.Add(Cell);
        Cell.FStressPeriod := TimeIndex;
        Cell.FValues := BoundaryValues;
        Cell.ScreenObject := ScreenObjectI;
        LocalModel.AdjustCellPosition(Cell);
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TCSubBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TCSubCollection;
end;

function TCSubBoundary.BoundaryObserverPrefix: string;
begin
  result := 'PestCsub_';
end;

procedure TCSubBoundary.Clear;
begin
  inherited;
  CSubPackageData.Clear;
end;

constructor TCSubBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

  PestStressOffsetFormula := '';
  PestStressOffsetMethod := DefaultBoundaryMethod(CsubStressOffsetPosition);

  FCSubPackageData := TCSubPackageDataCollection.Create(Model as TCustomModel, ScreenObject);
end;

procedure TCSubBoundary.CreateFormulaObjects;
begin
  FPestStressOffsetFormula := CreateFormulaObjectBlocks(dso3D);
end;

procedure TCSubBoundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(PestStressOffsetObserver);
  end;
end;

class function TCSubBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    CsubStressOffsetPosition:
      begin
        result := ppmAdd;
      end;
    else
      begin
        result := inherited;
        Assert(False);
      end;
  end;
end;

destructor TCSubBoundary.Destroy;
begin
  PestStressOffsetFormula := '';
  FCSubPackageData.Free;
  inherited;
end;

procedure TCSubBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
const
  NoData = 3.0E30;
var
  ValueIndex: Integer;
  BoundaryStorage: TCSubStorage;
begin
  FCurrentParameter := nil;
  EvaluateListBoundaries(AModel);

  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TCSubStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
end;

function TCSubBoundary.GetPestBoundaryFormula(FormulaIndex: integer): string;
begin
  case FormulaIndex of
    CsubStressOffsetPosition:
      begin
        result := PestStressOffsetFormula;
      end;
    else
      begin
        result := inherited;
        Assert(False);
      end;
  end;
end;

function TCSubBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    CsubStressOffsetPosition:
      begin
        result := PestStressOffsetMethod;
      end;
    else
      begin
        result := inherited;
        Assert(False);
      end;
  end;
end;

function TCSubBoundary.GetPestStressOffsetFormula: string;
begin
  Result := FPestStressOffsetFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(CsubStressOffsetPosition);
  end;
end;

function TCSubBoundary.GetPestStressOffsetObserver: TObserver;
begin
  if FPestStressOffsetObserver = nil then
  begin
    CreateObserver('PestStressOffset_', FPestStressOffsetObserver, nil);
    FPestStressOffsetObserver.OnUpToDateSet := InvalidateStressOffsetData;
  end;
  result := FPestStressOffsetObserver;

end;

procedure TCSubBoundary.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FPestStressOffsetFormula as TObject then
  begin
    if CsubStressOffsetPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[CsubStressOffsetPosition]);
    end;
  end;
end;

function TCSubBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('PestCSUB_Used_', FUsedObserver, nil);
  end;
  result := FUsedObserver;
end;

procedure TCSubBoundary.HandleChangedValue(Observer: TObserver);
begin
//  inherited;
  InvalidateDisplay;
end;

procedure TCSubBoundary.InvalidateDisplay;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    (ParentModel as TPhastModel).InvalidateCSubStressOffset(self);
  end;
end;

procedure TCSubBoundary.InvalidateStressOffsetData(Sender: TObject);
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
    PhastModel.InvalidateCSubStressOffset(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateCSubStressOffset(self);
      end;
    end;
  end;
end;

procedure TCSubBoundary.Loaded;
begin
  CSubPackageData.Loaded;
end;

class function TCSubBoundary.ModflowParamItemClass: TModflowParamItemClass;
begin
  result := nil;
end;

function TCSubBoundary.ParameterType: TParameterType;
begin
  result := ptUndefined;
end;

procedure TCSubBoundary.SetCSubPackageData(
  const Value: TCSubPackageDataCollection);
begin
  FCSubPackageData.Assign(Value);
end;

procedure TCSubBoundary.SetPestBoundaryFormula(FormulaIndex: integer;
  const Value: string);
begin
  case FormulaIndex of
    CsubStressOffsetPosition:
      begin
        PestStressOffsetFormula := Value;
      end;
    else
      begin
        inherited;
        Assert(False);
      end;
  end;
end;

procedure TCSubBoundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
begin
  case FormulaIndex of
    CsubStressOffsetPosition:
      begin
        PestStressOffsetMethod := Value;
      end;
    else
      begin
        inherited;
        Assert(False);
      end;
  end;
end;

procedure TCSubBoundary.SetPestStressOffsetFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, CsubStressOffsetPosition, FPestStressOffsetFormula);
end;

procedure TCSubBoundary.SetPestStressOffsetMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestStressOffsetMethod, Value);
end;

procedure TCSubBoundary.UpdateTimes(Times: TRealList; StartTestTime,
  EndTestTime: double; var StartRangeExtended, EndRangeExtended: boolean;
  AModel: TBaseModel);
//var
//  LocalModel: TCustomModel;
//  ParamIndex: Integer;
//  Param: TModflowParamItem;
begin
//  LocalModel := AModel as TCustomModel;
//  if LocalModel.ModflowPackages.CSubPackage.UseTabFilesInThisModel then
//  begin
//    // tab files only apply to non parameter boundaries.
//    for ParamIndex := 0 to Parameters.Count - 1 do
//    begin
//      Param := Parameters[ParamIndex];
//      AddBoundaryTimes(Param.Param, Times, StartTestTime, EndTestTime,
//        StartRangeExtended, EndRangeExtended);
//    end;
//  end
//  else
//  begin
    inherited;
//  end;


end;

function TCSubBoundary.Used: boolean;
begin
  result := inherited Used or CSubPackageData.Used;
end;

{ TCSubObs }

procedure TCSubObs.Assign(Source: TPersistent);
begin
  if Source is TCSubObs then
  begin
    CSubObsOptions := TCSubObs(Source).CSubObsOptions;
  end
  else
  begin
    inherited;
  end;

end;

function TCSubObs.GetCSubObsOptions: string;
var
  CSubObs: TCSubOb;
begin
  Result:= '';
  for CSubObs:= Low(TCSubOb) to High(TCSubOb) do
  begin
    if CSubObs in FCSubObsOptions then
    begin
      Result:= Result + CSubObToString(CSubObs) + ',';
    end;
  end;
end;

procedure TCSubObs.SetCSubObsOptions(const Value: string);
var
  Items: TStringList;
  ItemIndex: Integer;
  Item: TCSubOb;
  AnItemName: String;
begin
  FCSubObsOptions := [];
  if Value <> '' then
  begin
    Items := TStringList.Create;
    try
      Items.CommaText := Value;
      for ItemIndex := 0 to Items.Count - 1 do
      begin
        AnItemName := Items[ItemIndex];
        if AnItemName <> '' then
        begin
          Item := StringToCSubOb(Items[ItemIndex]);
          Include(FCSubObsOptions, Item);
        end;
      end;
    finally
      Items.Free;
    end;
  end;
end;

function TCSubObs.StringToCSubOb(AString: string): TCSubOb;
var
  ItemIndex: Integer;
begin
  ItemIndex := CSubOptionNames.IndexOf(AString);
  Assert(ItemIndex >= 0);
  result := TCSubOb(ItemIndex);
end;

function TCSubObs.CSubObToString(CSubOb: TCSubOb): string;
begin
  result := CSubOptionNames[Ord(CSubOb)];
end;

initialization
  InitializeCSubObNames;

  CSubOptionNames := TStringList.Create;
  CSubOptionNames.Add('coCSub');
  CSubOptionNames.Add('coInelastCSub');
  CSubOptionNames.Add('coElastCSub');
  CSubOptionNames.Add('coCoarseCSub');
  CSubOptionNames.Add('coCSubCell');
  CSubOptionNames.Add('coWcompCSubCell');
  CSubOptionNames.Add('coSk');
  CSubOptionNames.Add('coSke');
  CSubOptionNames.Add('coSkCell');
  CSubOptionNames.Add('coSkeCell');
  CSubOptionNames.Add('coEStressCell');
  CSubOptionNames.Add('coGStressCell');
  CSubOptionNames.Add('coIntbedComp');
  CSubOptionNames.Add('coInelastComp');
  CSubOptionNames.Add('coElastComp');
  CSubOptionNames.Add('coCoarseCompaction');
  CSubOptionNames.Add('coCompCell');
  CSubOptionNames.Add('coThickness');
  CSubOptionNames.Add('coCoarseThickness');
  CSubOptionNames.Add('coThickCell');
  CSubOptionNames.Add('coTheta');
  CSubOptionNames.Add('coCoarseTheta');
  CSubOptionNames.Add('coThetaCell');
  CSubOptionNames.Add('coDelayFlowTop');
  CSubOptionNames.Add('coDelayFlowBot');
  CSubOptionNames.Add('coDelayHead');
  CSubOptionNames.Add('coDelayGStress');
  CSubOptionNames.Add('coDelayEStress');
  CSubOptionNames.Add('coDelayPreConStress');
  CSubOptionNames.Add('coDelayComp');
  CSubOptionNames.Add('coDelayThickness');
  CSubOptionNames.Add('coDelayTheta');
  CSubOptionNames.Add('coPreConsStressCell');

{
  TCSubOb = (coCSub, coInelastCSub, coElastCSub, coCoarseCSub, coCSubCell,
    coWcompCSubCell, coSk, coSke, coSkCell, coSkeCell, coEStressCell,
    coGStressCell, coIntbedComp, coInelastComp, coElastComp, coCoarseCompaction,
    coCompCell, coThickness, coCoarseThickness, coThickCell, coTheta,
    coCoarseTheta, ooThetaCell, coDelayFlowTop, coDelayFlowBot,
    coDelayHead,
    coDelayGStress, coDelayEStress, coDelayPreConStress, coDelayComp,
    coDelayThickness, coDelayTheta,
    coPreConsStressCell);
}

finalization
  CSubOptionNames.Free;
  CSubObNames.Free;

end.
