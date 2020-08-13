unit ModflowLakMf6Unit;

interface

uses
  System.Classes, ModflowBoundaryUnit, GoPhastTypes, OrderedCollectionUnit,
  FormulaManagerUnit, SubscriptionUnit, System.Generics.Collections,
  ModflowCellUnit, RealListUnit;

type
  // related to claktype.
//  Tmf6LakeType = (mltOnlyVertical, mltOnlyHorizontal, mltBoth);

  TLakOb = (loStage, loExternalInflow, loSimOutletInflow, loSumInflow, loFromMvr, loRain,
    loRunoff, loFlowRate, loWithdrawal, loEvap, loExternalOutflow, loToMvr, loStorage,
    loConstantFlow, loOutlet, loVolume, loSurfaceArea, loWettedArea,
    loConductance);
  TLakObs = set of TLakOb;

//    loExternalOutflow, loOutlet,

  // related to bedleak
  Tmf6LakLeakanceUsed = (lluNotUsed, lluUsed);
  // related to couttyp
  TLakeOutletType = (lotSpecified, lotManning, lotWeir);
  // related to STATUS
  TLakeStatus = (lsActive, lsInactive, lsConstant);

  TLakeConnectionType = (lctHorizontal, lctVertical);
  TLakeConnectionTypes = set of TLakeConnectionType;

  TMf6LakeConnectionType = (mlctVertical, mlctHorizontal, mlctEmbeddedVertical,
    mlctEmbeddedHorizontal);

  TLakConnectionMf6Record = record
    Cell: TCellLocation;
    ConnectionType: TMf6LakeConnectionType;
//    B
  end;

  TLakeOutletTimeItem = class(TCustomModflowBoundaryItem)
  private
    const
      KRatePosition = 0;
      KInvertPosition = 1;
      KRoughnessPosition = 2;
      KWidthPosition = 3;
      KSlopePosition = 4;
    var
    FRate: TFormulaObject;
    FInvert: TFormulaObject;
    FRoughness: TFormulaObject;
    FSlope: TFormulaObject;
    FWidth: TFormulaObject;
    function GetInvert: string;
    function GetRate: string;
    function GetRoughness: string;
    function GetSlope: string;
    function GetWidth: string;
    procedure SetInvert(const Value: string);
    procedure SetRate(const Value: string);
    procedure SetRoughness(const Value: string);
    procedure SetSlope(const Value: string);
    procedure SetWidth(const Value: string);
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    function GetBoundaryFormula(Index: integer): string; override;
    procedure SetBoundaryFormula(Index: integer; const Value: string);
      override;
    function BoundaryFormulaCount: integer; override;
  public
    procedure Assign(Source: TPersistent); override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  published
    property Rate: string read GetRate write SetRate;
    property Invert: string read GetInvert write SetInvert;
    property Roughness: string read GetRoughness write SetRoughness;
    property Width: string read GetWidth write SetWidth;
    property Slope: string read GetSlope write SetSlope;
  end;

  TLakOutletTimeCollection = class(TCustomNonSpatialBoundColl)
  private
    procedure InvalidateRate(Sender: TObject);
    procedure InvalidateInvert(Sender: TObject);
    procedure InvalidateRoughness(Sender: TObject);
    procedure InvalidateWidth(Sender: TObject);
    procedure InvalidateSlope(Sender: TObject);
    function GetItem(Index: Integer): TLakeOutletTimeItem;
    procedure SetItem(Index: Integer; const Value: TLakeOutletTimeItem);
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    property Items[Index: Integer]: TLakeOutletTimeItem read GetItem write SetItem; default;
    function Add: TLakeOutletTimeItem;
  end;

  TLakeOutlet = class(TModflowScreenObjectProperty)
  private
    FOutletType: TLakeOutletType;
    FLakeTimes: TLakOutletTimeCollection;
    FOutletObjectName: string;
    FOutletObject: TObject;
    FOutletIndex: Integer;
//    procedure SetOutlet(const Value: Integer);
    procedure SetOutletType(const Value: TLakeOutletType);
    procedure SetLakeTimes(const Value: TLakOutletTimeCollection);
    function IsSame(AnotherLakeOutlet: TLakeOutlet): boolean;
    function GetOutletObjectName: string;
    procedure SetOutletObjectName(const Value: string);
    procedure SetOutletObject(const Value: TObject);
    procedure UpdateOutletObject;
    function GetOutletObject: TObject;
  protected
    function BoundaryObserverPrefix: string; override;
  public
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Used: boolean; override;
    // @name is a @link(TScreenObject). It identifies the lake into which the
    // lake flows.
    property OutletObject: TObject read GetOutletObject write SetOutletObject;
    property OutletIndex: Integer read FOutletIndex write FOutletIndex;
  published
    property OutletType: TLakeOutletType read FOutletType write SetOutletType;
    property LakeTimes: TLakOutletTimeCollection read FLakeTimes write SetLakeTimes;
    property OutletObjectName: string read GetOutletObjectName write SetOutletObjectName;
  end;

  TLakeOutletItem = class(TFormulaOrderedItem)
  private
    FOutlet: TLakeOutlet;
    procedure SetOutlet(const Value: TLakeOutlet);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Loaded;
  published
    property Outlet: TLakeOutlet read FOutlet write SetOutlet;
  end;

  TLakeOutlets = class(TCustomObjectOrderedCollection)
  private
//    FScreenObject: TObject;
    function GetItems(Index: Integer): TLakeOutletItem;
    procedure SetItems(Index: Integer; const Value: TLakeOutletItem);
    procedure Loaded;
  public
    constructor Create(Model: TBaseModel; ScreenObject: TObject);
//    constructor Create(InvalidateModelEvent: TNotifyEvent);
    property Items[Index: Integer]: TLakeOutletItem read GetItems write SetItems; default;
    function Add: TLakeOutletItem;
  end;

  TLakeTableItemMf6 = class(TFormulaOrderedItem)
  private
  const
    FormulaCount = 4;
    StagePosition = 0;
    VolumePosition = 1;
    SurfaceAreaPosition = 2;
    ExchangeAreaEvapPosition = 3;
  var
    FStage: TFormulaObject;
    FVolume: TFormulaObject;
    FSurfaceArea: TFormulaObject;
    FExchangeArea: TFormulaObject;
    FObserverList: TObjectList<TObserver>;
    function GetExchangeArea: string;
    function GetStage: string;
    function GetSurfaceArea: string;
    function GetVolume: string;
    procedure SetExchangeArea(const Value: string);
    procedure SetStage(const Value: string);
    procedure SetSurfaceArea(const Value: string);
    procedure SetVolume(const Value: string);
    procedure CreateFormulaObjects;
    function CreateFormulaObject(Orientation:
      TDataSetOrientation): TFormulaObject;
    procedure RemoveFormulaObjects;
    procedure ResetItemObserver(Index: integer);
    procedure StopTalkingToAnyone;
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function GetObserver(Index: Integer): TObserver; override;
    function GetScreenObject: TObject; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    // stage
    property Stage: string read GetStage write SetStage;
    // volume
    property Volume: string read GetVolume write SetVolume;
    // sarea
    property SurfaceArea: string read GetSurfaceArea write SetSurfaceArea;
    // barea
    property ExchangeArea: string read GetExchangeArea write SetExchangeArea;
  end;

  TLakeTableMf6 = class(TEnhancedOrderedCollection)
  private
    function GetItems(Index: Integer): TLakeTableItemMf6;
    procedure SetItems(Index: Integer; const Value: TLakeTableItemMf6);
  public
    constructor Create(Model: TBaseModel);
    property Items[Index: Integer]: TLakeTableItemMf6 read GetItems
      write SetItems; default;
    function Add: TLakeTableItemMf6;
    procedure StopTalkingToAnyone;
  end;

  TLakeTimeItem = class(TCustomModflowBoundaryItem)
  private
    const
      KStagePosition = 0;
      KRainfallPosition = 1;
      KRunoffPosition = 2;
      KEvaporationPosition = 3;
      KInflowPosition = 4;
      KWithdrawalPosition = 5;
    var
    FStatus: TLakeStatus;
    FStage: TFormulaObject;
    FRainfall: TFormulaObject;
    FRunoff: TFormulaObject;
    FEvaporation: TFormulaObject;
    FInflow: TFormulaObject;
    FWithdrawal: TFormulaObject;
    function GetStage: string;
    procedure SetStage(const Value: string);
    function GetRainfall: string;
    procedure SetRainfall(const Value: string);
    procedure SetStatus(const Value: TLakeStatus);
    function GetRunoff: string;
    procedure SetRunoff(const Value: string);
    function GetEvaporation: string;
    procedure SetEvaporation(const Value: string);
    function GetWithdrawal: string;
    procedure SetWithdrawal(const Value: string);
    function GetInflow: string;
    procedure SetInflow(const Value: string);
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    function GetBoundaryFormula(Index: integer): string; override;
    procedure SetBoundaryFormula(Index: integer; const Value: string);
      override;
    function BoundaryFormulaCount: integer; override;
  public
    procedure Assign(Source: TPersistent); override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  published
    property Status: TLakeStatus read FStatus write SetStatus;
    property Stage: string read GetStage write SetStage;
    property Rainfall: string read GetRainfall write SetRainfall;
    property Evaporation: string read GetEvaporation write SetEvaporation;
    property Runoff: string read GetRunoff write SetRunoff;
    property Inflow: string read GetInflow write SetInflow;
    property Withdrawal: string read GetWithdrawal write SetWithdrawal;
  end;

  TLakTimeCollection = class(TCustomMF_BoundColl)
  private
    procedure InvalidateStage(Sender: TObject);
    procedure InvalidateRainfall(Sender: TObject);
    procedure InvalidateEvaporation(Sender: TObject);
    procedure InvalidateRunoff(Sender: TObject);
    procedure InvalidateInflow(Sender: TObject);
    procedure InvalidateWithdrawal(Sender: TObject);
  protected
    class function ItemClass: TBoundaryItemClass; override;
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
  end;

  TLakeMf6 = class(TModflowBoundary)
  private
  const
    BottomElevationPosition = 0;
    TopElevationPosition = 1;
    BedKPosition = 2;
    BedThicknessPosition = 3;
    ConnectionLengthPosition = 4;
//    ConnectionWidthPosition = 5;
    StartingStagePosition = 5;
  var
    FOutlets: TLakeOutlets;
    FLakeTable: TLakeTableMf6;
    FLakeConnections: TLakeConnectionTypes;
    FEmbedded: Boolean;
    FBottomElevation: TFormulaObject;
    FTopElevation: TFormulaObject;
    FBedK: TFormulaObject;
    FBedThickness: TFormulaObject;
    FConnectionLength: TFormulaObject;
//    FConnectionWidth: TFormulaObject;
    FStartingStage: TFormulaObject;
    FBottomElevationObserver: TObserver;
    FTopElevationObserver: TObserver;
    FBedKObserver: TObserver;
    FBedThicknessObserver: TObserver;
    FConnectionLengthObserver: TObserver;
    FConnectionWidthObserver: TObserver;
    FStartingStageObserver: TObserver;
    procedure SetOutlets(const Value: TLakeOutlets);
    procedure SetLakeTable(const Value: TLakeTableMf6);
    procedure SetEmbedded(const Value: Boolean);
    procedure SetLakeConnections(const Value: TLakeConnectionTypes);
    function GetBedK: string;
    function GetBedThickness: string;
    function GetBottomElevation: string;
    function GetConnectionLength: string;
//    function GetConnectionWidth: string;
    function GetTopElevation: string;
    procedure SetBedK(const Value: string);
    procedure SetBedThickness(const Value: string);
    procedure SetBottomElevation(const Value: string);
    procedure SetConnectionLength(const Value: string);
//    procedure SetConnectionWidth(const Value: string);
    procedure SetTopElevation(const Value: string);
    function GetBedKObserver: TObserver;
    function GetBedThicknessObserver: TObserver;
    function GetBottomElevationObserver: TObserver;
    function GetConnectionLengthObserver: TObserver;
    function GetConnectionWidthObserver: TObserver;
    function GetTopElevationObserver: TObserver;
    function GetStartingStageObserver: TObserver;
    function GetStartingStage: string;
    procedure SetStartingStage(const Value: string);
  protected
    class function BoundaryCollectionClass: TMF_BoundCollClass;
      override;
    function BoundaryObserverPrefix: string; override;
    procedure CreateFormulaObjects;
    property BottomElevationObserver: TObserver read GetBottomElevationObserver;
    property TopElevationObserver: TObserver read GetTopElevationObserver;
    property BedKObserver: TObserver read GetBedKObserver;
    property BedThicknessObserver: TObserver read GetBedThicknessObserver;
    property ConnectionLengthObserver: TObserver read GetConnectionLengthObserver;
    property ConnectionWidthObserver: TObserver read GetConnectionWidthObserver;
    property StartingStageObserver: TObserver read GetStartingStageObserver;
    procedure CreateObservers;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
//    function GetUsedObserver: TObserver; override;
  public
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel); override;
    procedure Loaded;
    procedure UpdateTimes(Times: TRealList; StartTestTime,
      EndTestTime: double; var StartRangeExtended, EndRangeExtended: boolean;
      AModel: TBaseModel); override;
  published
    property Outlets: TLakeOutlets read FOutlets write SetOutlets;
    property LakeTable: TLakeTableMf6 read FLakeTable write SetLakeTable;
    property LakeConnections: TLakeConnectionTypes read FLakeConnections
      write SetLakeConnections Stored True;
    property Embedded: Boolean read FEmbedded write SetEmbedded Stored True;
    // belev
    Property BottomElevation: string read GetBottomElevation write SetBottomElevation;
    // telev
    Property TopElevation: string read GetTopElevation write SetTopElevation;
    // bedleak
    Property BedK: string read GetBedK write SetBedK;
    // bedleak
    Property BedThickness: string read GetBedThickness write SetBedThickness;
    // connlen
    property ConnectionLength: string read GetConnectionLength write SetConnectionLength;
//    property ConnectionWidth: string read GetConnectionWidth write SetConnectionWidth;
    // strt
    property StartingStage: string read GetStartingStage write SetStartingStage;

  end;

function TryGetLakOb(const CSubObName: string; var LakOb: TLakOb): Boolean;
function LakObToString(const LakOb: TLakOb): string;
Procedure FillLakSeriesNames(AList: TStrings);

implementation

uses
  frmGoPhastUnit, ScreenObjectUnit, PhastModelUnit, DataSetUnit;

{ TLakeTimeItem }

const
  LakeObName: array[TLakOb] of string = ('Stage', 'ExternalInflow', 'SimOutletInflow', 'SumInflow', 'FromMvr', 'Rain',
    'Runoff', 'FlowRate', 'Withdrawal', 'Evap', 'ExternalOutflow', 'ToMvr', 'Storage',
    'ConstantFlow', 'Outlet', 'Volume', 'SurfaceArea', 'WettedArea',
    'Conductance');
	
var
  LakeObNames: TStringList;
  
procedure InitializeLakeObNames;
var
  Index: TLakOb; 
begin
  LakeObNames := TStringList.Create;
  LakeObNames.CaseSensitive := False;
  for Index := Low(TLakOb) to High(TLakOb) do
  begin
    LakeObNames.Add(LakeObName[Index]);
  end;  
end;  
	

function TryGetLakOb(const CSubObName: string; var LakOb: TLakOb): Boolean;
var
  Index: Integer;
begin
  Index := LakeObNames.IndexOf(CSubObName);
  result := Index >= 0;
  if result then
  begin
    LakOb := TLakOb(Index);
  end;
end;

Procedure FillLakSeriesNames(AList: TStrings);
begin
  AList.Assign(LakeObNames);
end;

function LakObToString(const LakOb: TLakOb): string;
begin
  result := LakeObName[LakOb];
end;
  
{
  TLakOb = (loStage, loExternalInflow, loSimOutletInflow, loSumInflow, loFromMvr, loRain,
    loRunoff, loFlowRate, loWithdrawal, loEvap, loExternalOutflow, loToMvr, loStorage,
    loConstantFlow, loOutlet, loVolume, loSurfaceArea, loWettedArea,
    loConductance);
}  

procedure TLakeTimeItem.Assign(Source: TPersistent);
var
  LakeItem: TLakeTimeItem;
begin
  if Source is TLakeTimeItem then
  begin
    LakeItem := TLakeTimeItem(Source);
    Status := LakeItem.Status;
    Stage := LakeItem.Stage;
    Rainfall := LakeItem.Rainfall;
    Evaporation := LakeItem.Evaporation;
    Runoff := LakeItem.Runoff;
    Inflow := LakeItem.Inflow;
    Withdrawal := LakeItem.Withdrawal;
  end;
  inherited;
end;

procedure TLakeTimeItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TLakTimeCollection;
  StageObserver: TObserver;
  RainfallObserver: TObserver;
  RunoffObserver: TObserver;
  EvaporationObserver: TObserver;
  WithdrawalObserver: TObserver;
  InflowObserver: TObserver;
begin
  inherited;
//  inherited;
  ParentCollection := Collection as TLakTimeCollection;

  StageObserver := FObserverList[KStagePosition];
  StageObserver.OnUpToDateSet := ParentCollection.InvalidateStage;

  RainfallObserver := FObserverList[KRainfallPosition];
  RainfallObserver.OnUpToDateSet := ParentCollection.InvalidateRainfall;

  RunoffObserver := FObserverList[KRunoffPosition];
  RunoffObserver.OnUpToDateSet := ParentCollection.InvalidateRunoff;

  EvaporationObserver := FObserverList[KEvaporationPosition];
  EvaporationObserver.OnUpToDateSet := ParentCollection.InvalidateEvaporation;

  InflowObserver := FObserverList[KInflowPosition];
  InflowObserver.OnUpToDateSet := ParentCollection.InvalidateInflow;

  WithdrawalObserver := FObserverList[KWithdrawalPosition];
  WithdrawalObserver.OnUpToDateSet := ParentCollection.InvalidateWithdrawal;
end;

function TLakeTimeItem.BoundaryFormulaCount: integer;
begin
  result := 6;
end;

procedure TLakeTimeItem.CreateFormulaObjects;
begin
  inherited;
  FStage := CreateFormulaObject(dsoTop);
  FRainfall := CreateFormulaObject(dsoTop);
  FRunoff := CreateFormulaObject(dsoTop);
  FEvaporation := CreateFormulaObject(dsoTop);
  FInflow := CreateFormulaObject(dsoTop);
  FWithdrawal := CreateFormulaObject(dsoTop);
end;

function TLakeTimeItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    KStagePosition: result := Stage;
    KRainfallPosition: result := Rainfall;
    KRunoffPosition: result := Runoff;
    KEvaporationPosition: result := Evaporation;
    KInflowPosition: result := Inflow;
    KWithdrawalPosition: result := Withdrawal;
    else
      Assert(False);
  end;
end;

function TLakeTimeItem.GetEvaporation: string;
begin
  Result := FEvaporation.Formula;
  ResetItemObserver(KEvaporationPosition);
end;

function TLakeTimeItem.GetInflow: string;
begin
  Result := FInflow.Formula;
  ResetItemObserver(KInflowPosition);
end;

procedure TLakeTimeItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  inherited;
  if Sender = FStage then
  begin
    List.Add(FObserverList[KStagePosition]);
  end;
  if Sender = FRainfall then
  begin
    List.Add(FObserverList[KRainfallPosition]);
  end;
  if Sender = FRunoff then
  begin
    List.Add(FObserverList[KRunoffPosition]);
  end;
  if Sender = FEvaporation then
  begin
    List.Add(FObserverList[KEvaporationPosition]);
  end;
  if Sender = FInflow then
  begin
    List.Add(FObserverList[KInflowPosition]);
  end;
  if Sender = FWithdrawal then
  begin
    List.Add(FObserverList[KWithdrawalPosition]);
  end;
end;

function TLakeTimeItem.GetRainfall: string;
begin
  Result := FRainfall.Formula;
  ResetItemObserver(KRainfallPosition);
end;

function TLakeTimeItem.GetRunoff: string;
begin
  Result := FRunoff.Formula;
  ResetItemObserver(KRunoffPosition);
end;

function TLakeTimeItem.GetStage: string;
begin
  Result := FStage.Formula;
  ResetItemObserver(KStagePosition);
end;

function TLakeTimeItem.GetWithdrawal: string;
begin
  Result := FWithdrawal.Formula;
  ResetItemObserver(KWithdrawalPosition);
end;

function TLakeTimeItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  LakeItem: TLakeTimeItem;
begin
  Result := inherited IsSame(AnotherItem) and (AnotherItem is TLakeTimeItem);
  if result then
  begin
    LakeItem := TLakeTimeItem(AnotherItem);
    result :=
      (Status = LakeItem.Status)
      and (Stage = LakeItem.Stage)
      and (Rainfall = LakeItem.Rainfall)
      and (Evaporation = LakeItem.Evaporation)
      and (Runoff = LakeItem.Runoff)
      and (Inflow = LakeItem.Inflow)
      and (Withdrawal = LakeItem.Withdrawal);
  end;
end;

procedure TLakeTimeItem.RemoveFormulaObjects;
begin
  inherited;
  frmGoPhast.PhastModel.FormulaManager.Remove(FStage,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRainfall,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRunoff,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FEvaporation,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FInflow,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FWithdrawal,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TLakeTimeItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    KStagePosition:
      Stage := Value;
    KRainfallPosition:
      Rainfall := Value;
    KRunoffPosition:
      Runoff := Value;
    KEvaporationPosition:
      Evaporation := Value;
    KInflowPosition:
      Inflow := Value;
    KWithdrawalPosition:
      Withdrawal := Value;
    else
      Assert(False);
  end;
end;

procedure TLakeTimeItem.SetEvaporation(const Value: string);
begin
  UpdateFormulaBlocks(Value, KEvaporationPosition, FEvaporation);
end;

procedure TLakeTimeItem.SetInflow(const Value: string);
begin
  UpdateFormulaBlocks(Value, KInflowPosition, FInflow);
end;

procedure TLakeTimeItem.SetRainfall(const Value: string);
begin
  UpdateFormulaBlocks(Value, KRainfallPosition, FRainfall);
end;

procedure TLakeTimeItem.SetRunoff(const Value: string);
begin
  UpdateFormulaBlocks(Value, KRunoffPosition, FRunoff);
end;

procedure TLakeTimeItem.SetStage(const Value: string);
begin
  UpdateFormulaBlocks(Value, KStagePosition, FStage);
end;

procedure TLakeTimeItem.SetStatus(const Value: TLakeStatus);
begin
  FStatus := Value;
end;

procedure TLakeTimeItem.SetWithdrawal(const Value: string);
begin
  UpdateFormulaBlocks(Value, KWithdrawalPosition, FWithdrawal);
end;

{ TLakeOutletTimeItem }

procedure TLakeOutletTimeItem.Assign(Source: TPersistent);
var
  SourceItem: TLakeOutletTimeItem;
begin
  if Source is TLakeOutletTimeItem then
  begin
    SourceItem := TLakeOutletTimeItem(Source);
    Rate := SourceItem.Rate;
    Invert := SourceItem.Invert;
    Roughness := SourceItem.Roughness;
    Width := SourceItem.Width;
    Slope := SourceItem.Slope;
  end;
  inherited;
end;

procedure TLakeOutletTimeItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TLakOutletTimeCollection;
  RateObserver: TObserver;
  InvertObserver: TObserver;
  RoughnessObserver: TObserver;
  WidthObserver: TObserver;
  SlopeObserver: TObserver;
begin
//  inherited;
  ParentCollection := Collection as TLakOutletTimeCollection;

  RateObserver := FObserverList[KRatePosition];
  RateObserver.OnUpToDateSet := ParentCollection.InvalidateRate;

  InvertObserver := FObserverList[KInvertPosition];
  InvertObserver.OnUpToDateSet := ParentCollection.InvalidateInvert;

  RoughnessObserver := FObserverList[KRoughnessPosition];
  RoughnessObserver.OnUpToDateSet := ParentCollection.InvalidateRoughness;

  WidthObserver := FObserverList[KWidthPosition];
  WidthObserver.OnUpToDateSet := ParentCollection.InvalidateWidth;

  SlopeObserver := FObserverList[KSlopePosition];
  SlopeObserver.OnUpToDateSet := ParentCollection.InvalidateSlope;
end;

function TLakeOutletTimeItem.BoundaryFormulaCount: integer;
begin
  result := 5;
end;

procedure TLakeOutletTimeItem.CreateFormulaObjects;
begin
  FRate := CreateFormulaObject(dsoTop);
  FInvert := CreateFormulaObject(dsoTop);
  FRoughness := CreateFormulaObject(dsoTop);
  FWidth := CreateFormulaObject(dsoTop);
  FSlope := CreateFormulaObject(dsoTop);
end;

function TLakeOutletTimeItem.GetBoundaryFormula(Index: integer): string;
begin
  case index of
    KRatePosition: result := Rate;
    KInvertPosition: result := Invert;
    KRoughnessPosition: result := Roughness;
    KWidthPosition: result := Width;
    KSlopePosition: result := Slope;
    else
      Assert(False);
  end;
end;

function TLakeOutletTimeItem.GetInvert: string;
begin
  Result := FInvert.Formula;
  ResetItemObserver(KInvertPosition);
end;

procedure TLakeOutletTimeItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FRate then
  begin
    List.Add(FObserverList[KRatePosition]);
  end;
  if Sender = FInvert then
  begin
    List.Add(FObserverList[KInvertPosition]);
  end;
  if Sender = FRoughness then
  begin
    List.Add(FObserverList[KRoughnessPosition]);
  end;
  if Sender = FWidth then
  begin
    List.Add(FObserverList[KWidthPosition]);
  end;
  if Sender = FSlope then
  begin
    List.Add(FObserverList[KSlopePosition]);
  end;
end;

function TLakeOutletTimeItem.GetRate: string;
begin
  Result := FRate.Formula;
  ResetItemObserver(KRatePosition);
end;

function TLakeOutletTimeItem.GetRoughness: string;
begin
  Result := FRoughness.Formula;
  ResetItemObserver(KRoughnessPosition);
end;

function TLakeOutletTimeItem.GetSlope: string;
begin
  Result := FSlope.Formula;
  ResetItemObserver(KSlopePosition);
end;

function TLakeOutletTimeItem.GetWidth: string;
begin
  Result := FWidth.Formula;
  ResetItemObserver(KWidthPosition);
end;

function TLakeOutletTimeItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  SourceItem: TLakeOutletTimeItem;
begin
  Result := inherited IsSame(AnotherItem) and (AnotherItem is TLakeOutletTimeItem);
  if result then
  begin
    SourceItem := TLakeOutletTimeItem(AnotherItem);
    result := (Rate = SourceItem.Rate)
      and (Invert = SourceItem.Invert)
      and (Roughness = SourceItem.Roughness)
      and (Width = SourceItem.Width)
      and (Slope = SourceItem.Slope);
  end;
end;

procedure TLakeOutletTimeItem.RemoveFormulaObjects;
begin
  inherited;
  frmGoPhast.PhastModel.FormulaManager.Remove(FRate,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FInvert,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRoughness,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FWidth,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FSlope,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TLakeOutletTimeItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case index of
    KRatePosition:
      Rate := Value;
    KInvertPosition:
      Invert := Value;
    KRoughnessPosition:
      Roughness := Value;
    KWidthPosition:
      Width := Value;
    KSlopePosition:
      Slope := Value;
    else
      Assert(False);
  end;
end;

procedure TLakeOutletTimeItem.SetInvert(const Value: string);
begin
  UpdateFormulaBlocks(Value, KInvertPosition, FInvert);
end;

procedure TLakeOutletTimeItem.SetRate(const Value: string);
begin
  UpdateFormulaBlocks(Value, KRatePosition, FRate);
end;

procedure TLakeOutletTimeItem.SetRoughness(const Value: string);
begin
  UpdateFormulaBlocks(Value, KRoughnessPosition, FRoughness);
end;

procedure TLakeOutletTimeItem.SetSlope(const Value: string);
begin
  UpdateFormulaBlocks(Value, KSlopePosition, FSlope);
end;

procedure TLakeOutletTimeItem.SetWidth(const Value: string);
begin
  UpdateFormulaBlocks(Value, KWidthPosition, FWidth);
end;

{ TLakeOutlet }

procedure TLakeOutlet.Assign(Source: TPersistent);
var
  OutletSource: TLakeOutlet;
begin
  if Source is TLakeOutlet then
  begin
    OutletSource := TLakeOutlet(Source);
    OutletType := OutletSource.OutletType;
    LakeTimes := OutletSource.LakeTimes;
    OutletObjectName := OutletSource.OutletObjectName;
  end
  else
  begin
    inherited;
  end;
end;

function TLakeOutlet.BoundaryObserverPrefix: string;
begin
  result := 'LakeOutlet';
end;

constructor TLakeOutlet.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  FLakeTimes := TLakOutletTimeCollection.Create(self, Model, ScreenObject);
end;

destructor TLakeOutlet.Destroy;
begin
  FLakeTimes.Free;
  inherited;
end;

function TLakeOutlet.GetOutletObject: TObject;
var
  AScreenObject: TScreenObject;
begin
  result := FOutletObject;
  if result <> nil then
  begin
    AScreenObject := result as TScreenObject;
    if AScreenObject.Deleted
      or (AScreenObject.ModflowLak6 = nil)
      or not AScreenObject.ModflowLak6.Used then
    begin
      result := nil;
    end;
  end;
end;

function TLakeOutlet.GetOutletObjectName: string;
begin
  if FOutletObject <> nil then
  begin
    result := (FOutletObject as TScreenObject).Name;
  end
  else
  begin
    result := FOutletObjectName;
  end;
end;

function TLakeOutlet.IsSame(AnotherLakeOutlet: TLakeOutlet): boolean;
begin
  result := (OutletObjectName = AnotherLakeOutlet.OutletObjectName)
    and (OutletType = AnotherLakeOutlet.OutletType)
    and LakeTimes.IsSame(AnotherLakeOutlet.LakeTimes);
end;

procedure TLakeOutlet.SetLakeTimes(const Value: TLakOutletTimeCollection);
begin
  FLakeTimes.Assign(Value);
end;

procedure TLakeOutlet.SetOutletObject(const Value: TObject);
begin
  FOutletObject := Value;
  if (FOutletObject <> nil) then
  begin
    FOutletObjectName := (FOutletObject as TScreenObject).Name
  end
  else
  begin
    FOutletObjectName := '';
  end;
end;

procedure TLakeOutlet.SetOutletObjectName(const Value: string);
begin
  FOutletObjectName := Value;
  UpdateOutletObject;
end;

procedure TLakeOutlet.SetOutletType(const Value: TLakeOutletType);
begin
  FOutletType := Value;
end;

function TLakeOutlet.Used: boolean;
begin
  result := True;
end;

procedure TLakeOutlet.UpdateOutletObject;
var
  LocalModel: TCustomModel;
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
begin
  if ParentModel <> nil then
  begin
    LocalModel := ParentModel as TCustomModel;
    for ObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := LocalModel.ScreenObjects[ObjectIndex];
      if (AScreenObject.Name = FOutletObjectName)
        and not AScreenObject.Deleted
        and (AScreenObject.ModflowLak6 <> nil)
        and AScreenObject.ModflowLak6.Used then
      begin
        OutletObject := AScreenObject;
        break;
      end;
    end;
  end;
end;

{ TLakeOutletItem }

procedure TLakeOutletItem.Assign(Source: TPersistent);
begin
  if Source is TLakeOutletItem then
  begin
    Outlet := TLakeOutletItem(Source).Outlet
  end;
  inherited;
end;

constructor TLakeOutletItem.Create(Collection: TCollection);
var
  LakeOutlets: TLakeOutlets;
begin
  inherited;
  LakeOutlets := Collection as TLakeOutlets;
//  LakeOutlets.
  FOutlet := TLakeOutlet.Create(LakeOutlets.Model, LakeOutlets.ScreenObject);
  FOutlet.OutletIndex := Index + 1;
end;


destructor TLakeOutletItem.Destroy;
begin
  FOutlet.Free;
  inherited;
end;

function TLakeOutletItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  if AnotherItem is TLakeOutletItem then
  begin
    result := Outlet.IsSame(TLakeOutletItem(AnotherItem).Outlet);
  end
  else
  begin
    result := False;
  end;
end;

procedure TLakeOutletItem.Loaded;
begin
  FOutlet.UpdateOutletObject;
end;

procedure TLakeOutletItem.SetOutlet(const Value: TLakeOutlet);
begin
  FOutlet.Assign(Value);
end;

{ TLakeOutlets }

function TLakeOutlets.Add: TLakeOutletItem;
begin
  Result := inherited Add as TLakeOutletItem
end;

constructor TLakeOutlets.Create(Model: TBaseModel; ScreenObject: TObject);
begin
//  FScreenObject := ScreenObject;
  inherited Create(TLakeOutletItem, Model, ScreenObject);
end;

function TLakeOutlets.GetItems(Index: Integer): TLakeOutletItem;
begin
  Result := inherited Items[Index] as TLakeOutletItem;
end;

procedure TLakeOutlets.Loaded;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].Loaded;
  end;
end;

procedure TLakeOutlets.SetItems(Index: Integer; const Value: TLakeOutletItem);
begin
  inherited Items[Index] := Value;
end;

{ TLakOutletTimeCollection }

function TLakOutletTimeCollection.Add: TLakeOutletTimeItem;
begin
  result := inherited Add as TLakeOutletTimeItem;
end;

function TLakOutletTimeCollection.GetItem(Index: Integer): TLakeOutletTimeItem;
begin
  result := inherited Items[index] as TLakeOutletTimeItem;
end;

procedure TLakOutletTimeCollection.InvalidateInvert(Sender: TObject);
begin

end;

procedure TLakOutletTimeCollection.InvalidateRate(Sender: TObject);
begin

end;

procedure TLakOutletTimeCollection.InvalidateRoughness(Sender: TObject);
begin

end;

procedure TLakOutletTimeCollection.InvalidateSlope(Sender: TObject);
begin

end;

procedure TLakOutletTimeCollection.InvalidateWidth(Sender: TObject);
begin

end;

class function TLakOutletTimeCollection.ItemClass: TBoundaryItemClass;
begin
  Result := TLakeOutletTimeItem;
end;

procedure TLakOutletTimeCollection.SetItem(Index: Integer;
  const Value: TLakeOutletTimeItem);
begin
  inherited Items[index] := Value;
end;

{ TLakTimeCollection }

procedure TLakTimeCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  inherited;

end;

function TLakTimeCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := nil;
  Assert(False);
end;

procedure TLakTimeCollection.InvalidateEvaporation(Sender: TObject);
begin

end;

procedure TLakTimeCollection.InvalidateInflow(Sender: TObject);
begin

end;

procedure TLakTimeCollection.InvalidateRainfall(Sender: TObject);
begin

end;

procedure TLakTimeCollection.InvalidateRunoff(Sender: TObject);
begin

end;

procedure TLakTimeCollection.InvalidateStage(Sender: TObject);
begin

end;

procedure TLakTimeCollection.InvalidateWithdrawal(Sender: TObject);
begin

end;

class function TLakTimeCollection.ItemClass: TBoundaryItemClass;
begin
  result := TLakeTimeItem;
end;

{ TLakeMf6 }

procedure TLakeMf6.Assign(Source: TPersistent);
var
  LakeSource: TLakeMf6;
begin
  if Source is TLakeMf6 then
  begin
    LakeSource := TLakeMf6(Source);
    Outlets := LakeSource.Outlets;
    LakeTable := LakeSource.LakeTable;
    LakeConnections := LakeSource.LakeConnections;
    Embedded := LakeSource.Embedded;
    BottomElevation := LakeSource.BottomElevation;
    TopElevation := LakeSource.TopElevation;
    BedK := LakeSource.BedK;
    BedThickness := LakeSource.BedThickness;
    ConnectionLength := LakeSource.ConnectionLength;
//    ConnectionWidth := LakeSource.ConnectionWidth;
    StartingStage := LakeSource.StartingStage;
  end;
  inherited;
end;

procedure TLakeMf6.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
begin
//  inherited;
  Assert(False);
end;

class function TLakeMf6.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TLakTimeCollection;
end;

function TLakeMf6.BoundaryObserverPrefix: string;
begin
  Result := 'Lake';
end;

constructor TLakeMf6.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  CreateBoundaryObserver;
  FOutlets := TLakeOutlets.Create(Model, ScreenObject);
  FLakeTable := TLakeTableMf6.Create(Model);
  LakeConnections := [lctHorizontal, lctVertical];
  CreateFormulaObjects;
  CreateObservers;

  BottomElevation := '0';
  TopElevation := '0';
  BedK := '0';
  BedThickness := '0';
  ConnectionLength := '0';
//  ConnectionWidth := '0';
  StartingStage := '0';
end;

procedure TLakeMf6.CreateFormulaObjects;
begin
  FBottomElevation := CreateFormulaObjectBlocks(dso3D);
  FTopElevation := CreateFormulaObjectBlocks(dso3D);
  FBedK := CreateFormulaObjectBlocks(dso3D);
  FBedThickness := CreateFormulaObjectBlocks(dso3D);
  FConnectionLength := CreateFormulaObjectBlocks(dso3D);
//  FConnectionWidth := CreateFormulaObject(dso3D);
  FStartingStage := CreateFormulaObjectBlocks(dso3D);
end;

procedure TLakeMf6.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
//    property BottomElevationObserver: TObserver read GetBottomElevationObserver;
//    property TopElevationObserver: TObserver read GetTopElevationObserver;
//    property BedKObserver: TObserver read GetBedKObserver;
//    property BedThicknessObserver: TObserver read GetBedThicknessObserver;
//    property ConnectionLengthObserver: TObserver read GetConnectionLengthObserver;
//    property ConnectionWidthObserver: TObserver read GetConnectionWidthObserver;
    FObserverList.Add(BottomElevationObserver);
    FObserverList.Add(TopElevationObserver);
    FObserverList.Add(BedKObserver);
    FObserverList.Add(BedThicknessObserver);
    FObserverList.Add(ConnectionLengthObserver);
    FObserverList.Add(ConnectionWidthObserver);
    FObserverList.Add(StartingStageObserver);
  end;
end;

destructor TLakeMf6.Destroy;
begin
  BottomElevation := '0';
  TopElevation := '0';
  BedK := '0';
  BedThickness := '0';
  ConnectionLength := '0';
//  ConnectionWidth := '0';
  StartingStage := '0';
  FLakeTable.Free;
  FOutlets.Free;
  inherited;
end;

function TLakeMf6.GetBedK: string;
begin
  Result := FBedK.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(BedKPosition);
  end;
end;

function TLakeMf6.GetBedKObserver: TObserver;
var
//  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FBedKObserver = nil then
  begin
    DataArray := nil;
    if ParentModel <> nil then
    begin
//      Model := ParentModel as TPhastModel;
//      DataArray := Model.DataArrayManager.GetDataSetByName(KCfpFixedHeads);
    end;
    CreateObserver('Lake_BedK', FBedKObserver, DataArray);
  end;
  result := FBedKObserver;
end;

function TLakeMf6.GetBedThickness: string;
begin
  Result := FBedThickness.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(BedThicknessPosition);
  end;
end;

function TLakeMf6.GetBedThicknessObserver: TObserver;
var
//  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FBedThicknessObserver = nil then
  begin
    DataArray := nil;
    if ParentModel <> nil then
    begin
//      Model := ParentModel as TPhastModel;
//      DataArray := Model.DataArrayManager.GetDataSetByName(KCfpFixedHeads);
    end;
    CreateObserver('Lake_BedThickness', FBedThicknessObserver, DataArray);
  end;
  result := FBedThicknessObserver;
end;

function TLakeMf6.GetBottomElevation: string;
begin
  Result := FBottomElevation.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(BottomElevationPosition);
  end;
end;

function TLakeMf6.GetBottomElevationObserver: TObserver;
var
//  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FBottomElevationObserver = nil then
  begin
    DataArray := nil;
    if ParentModel <> nil then
    begin
//      Model := ParentModel as TPhastModel;
//      DataArray := Model.DataArrayManager.GetDataSetByName(KCfpFixedHeads);
    end;
    CreateObserver('Lake_BottomElevation', FBottomElevationObserver, DataArray);
  end;
  result := FBottomElevationObserver;
end;

procedure TLakeMf6.GetCellValues(ValueTimeList: TList; ParamList: TStringList;
  AModel: TBaseModel);
begin
//  inherited;

end;

function TLakeMf6.GetConnectionLength: string;
begin
  Result := FConnectionLength.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(ConnectionLengthPosition);
  end;
end;

function TLakeMf6.GetConnectionLengthObserver: TObserver;
var
//  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FConnectionLengthObserver = nil then
  begin
    DataArray := nil;
    if ParentModel <> nil then
    begin
//      Model := ParentModel as TPhastModel;
//      DataArray := Model.DataArrayManager.GetDataSetByName(KCfpFixedHeads);
    end;
    CreateObserver('Lake_ConnectionLength', FConnectionLengthObserver, DataArray);
  end;
  result := FConnectionLengthObserver;
end;

//function TLakeMf6.GetConnectionWidth: string;
//begin
//  Result := FConnectionWidth.Formula;
//  if ScreenObject <> nil then
//  begin
//    ResetItemObserver(ConnectionWidthPosition);
//  end;
//end;

function TLakeMf6.GetConnectionWidthObserver: TObserver;
var
//  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FConnectionWidthObserver = nil then
  begin
    DataArray := nil;
    if ParentModel <> nil then
    begin
//      Model := ParentModel as TPhastModel;
//      DataArray := Model.DataArrayManager.GetDataSetByName(KCfpFixedHeads);
    end;
    CreateObserver('Lake_ConnectionWidth', FConnectionWidthObserver, DataArray);
  end;
  result := FConnectionWidthObserver;
end;

procedure TLakeMf6.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FBottomElevation then
  begin
    List.Add(FObserverList[BottomElevationPosition]);
  end
  else
  if Sender = FTopElevation then
  begin
    List.Add(FObserverList[TopElevationPosition]);
  end
  else
  if Sender = FBedK then
  begin
    List.Add(FObserverList[BedKPosition]);
  end
  else
  if Sender = FBedThickness then
  begin
    List.Add(FObserverList[BedThicknessPosition]);
  end
  else
  if Sender = FConnectionLength then
  begin
    List.Add(FObserverList[ConnectionLengthPosition]);
  end
  else
//  if Sender = FConnectionWidth then
//  begin
//    List.Add(FObserverList[ConnectionWidthPosition]);
//  end;
  if Sender = FStartingStage then
  begin
    List.Add(FObserverList[StartingStagePosition]);
  end;
{    FBottomElevation: TFormulaObject;
    FTopElevation: TFormulaObject;
    FBedK: TFormulaObject;
    FBedThickness: TFormulaObject;
    FConnectionLength: TFormulaObject;
    FConnectionWidth: TFormulaObject;
}
end;

function TLakeMf6.GetStartingStage: string;
begin
  Result := FStartingStage.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(StartingStagePosition);
  end;
end;

function TLakeMf6.GetStartingStageObserver: TObserver;
var
//  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FStartingStageObserver = nil then
  begin
    DataArray := nil;
    if ParentModel <> nil then
    begin
//      Model := ParentModel as TPhastModel;
//      DataArray := Model.DataArrayManager.GetDataSetByName(KCfpFixedHeads);
    end;
    CreateObserver('Lake_StartingStage', FStartingStageObserver, DataArray);
  end;
  result := FStartingStageObserver;
end;

function TLakeMf6.GetTopElevation: string;
begin
  Result := FTopElevation.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(TopElevationPosition);
  end;
end;

function TLakeMf6.GetTopElevationObserver: TObserver;
var
//  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FTopElevationObserver = nil then
  begin
    DataArray := nil;
    if ParentModel <> nil then
    begin
//      Model := ParentModel as TPhastModel;
//      DataArray := Model.DataArrayManager.GetDataSetByName(KCfpFixedHeads);
    end;
    CreateObserver('Lake_TopElevation', FTopElevationObserver, DataArray);
  end;
  result := FTopElevationObserver;
end;

procedure TLakeMf6.Loaded;
begin
  Outlets.Loaded;
end;

procedure TLakeMf6.SetBedK(const Value: string);
begin
  UpdateFormulaBlocks(Value, BedKPosition, FBedK);
end;

procedure TLakeMf6.SetBedThickness(const Value: string);
begin
  UpdateFormulaBlocks(Value, BedThicknessPosition, FBedThickness);
end;

procedure TLakeMf6.SetBottomElevation(const Value: string);
begin
  UpdateFormulaBlocks(Value, BottomElevationPosition, FBottomElevation);
end;

procedure TLakeMf6.SetConnectionLength(const Value: string);
begin
  UpdateFormulaBlocks(Value, ConnectionLengthPosition, FConnectionLength);
end;

//procedure TLakeMf6.SetConnectionWidth(const Value: string);
//begin
//  UpdateFormula(Value, ConnectionWidthPosition, FConnectionWidth);
//end;

procedure TLakeMf6.SetEmbedded(const Value: Boolean);
begin
  FEmbedded := Value;
end;

procedure TLakeMf6.SetLakeConnections(const Value: TLakeConnectionTypes);
begin
  FLakeConnections := Value;
end;

procedure TLakeMf6.SetLakeTable(const Value: TLakeTableMf6);
begin
  FLakeTable.Assign(Value);
end;

procedure TLakeMf6.SetOutlets(const Value: TLakeOutlets);
begin
  FOutlets.Assign(Value);
end;

procedure TLakeMf6.SetStartingStage(const Value: string);
begin
  UpdateFormulaBlocks(Value, StartingStagePosition, FStartingStage);
end;

procedure TLakeMf6.SetTopElevation(const Value: string);
begin
  UpdateFormulaBlocks(Value, TopElevationPosition, FTopElevation);
end;

procedure TLakeMf6.UpdateTimes(Times: TRealList; StartTestTime,
  EndTestTime: double; var StartRangeExtended, EndRangeExtended: boolean;
  AModel: TBaseModel);
var
  SP_Epsilon: Double;
  OutletIndex: Integer;
  AnOutlet: TLakeOutlet;
  TimeIndex: Integer;
  OutletTimeItem: TLakeOutletTimeItem;
  ClosestIndex: Integer;
  ExistingTime: Double;
begin
  inherited;
  SP_Epsilon := (AModel as TCustomModel).SP_Epsilon;
  for OutletIndex := 0 to Outlets.Count - 1 do
  begin
    AnOutlet := Outlets[OutletIndex].Outlet;
    for TimeIndex := 0 to AnOutlet.LakeTimes.Count - 1 do
    begin
      OutletTimeItem := AnOutlet.LakeTimes[TimeIndex];
      ClosestIndex := Times.IndexOfClosest(OutletTimeItem.StartTime);
      if ClosestIndex >= 0 then
      begin
        ExistingTime := Times[ClosestIndex];
        if Abs(ExistingTime-OutletTimeItem.StartTime) >  SP_Epsilon then
        begin
          Times.AddUnique(OutletTimeItem.StartTime);
        end;
      end;
      ClosestIndex := Times.IndexOfClosest(OutletTimeItem.EndTime);
      if ClosestIndex >= 0 then
      begin
        ExistingTime := Times[ClosestIndex];
        if Abs(ExistingTime-OutletTimeItem.EndTime) >  SP_Epsilon then
        begin
          Times.AddUnique(OutletTimeItem.EndTime);
        end;
      end;
      if (OutletTimeItem.StartTime < StartTestTime-SP_Epsilon) then
      begin
        StartRangeExtended := True;
      end;
      if (OutletTimeItem.EndTime > EndTestTime+SP_Epsilon) then
      begin
        EndRangeExtended := True;
      end;
    end;
  end;
end;

{ TLakeTableItemMf6 }

procedure TLakeTableItemMf6.Assign(Source: TPersistent);
var
  LKSource: TLakeTableItemMf6;
begin
  if Source is TLakeTableItemMf6 then
  begin
    LKSource := TLakeTableItemMf6(Source);
    Stage := LKSource.Stage;
    Volume := LKSource.Volume;
    SurfaceArea := LKSource.SurfaceArea;
    ExchangeArea := LKSource.ExchangeArea;
  end;
  inherited;
end;

constructor TLakeTableItemMf6.Create(Collection: TCollection);
var
  Index: Integer;
  Observer: TObserver;
begin
  inherited;
  CreateFormulaObjects;
  FObserverList := TObjectList<TObserver>.Create;
  for Index := 0 to FormulaCount - 1 do
  begin
    Observer := TObserver.Create(nil);
    FObserverList.Add(Observer);
  end;

end;

function TLakeTableItemMf6.CreateFormulaObject(
  Orientation: TDataSetOrientation): TFormulaObject;
begin
  result := CreateBlockFormulaObject(Orientation);
  result.AddSubscriptionEvents(
    GlobalRemoveFormulaObjectSubscription,
    GlobalRestoreFormulaObjectSubscription, self);
end;

procedure TLakeTableItemMf6.CreateFormulaObjects;
begin
  FStage := CreateFormulaObject(dso3D);
  FVolume := CreateFormulaObject(dso3D);
  FSurfaceArea := CreateFormulaObject(dso3D);
  FExchangeArea := CreateFormulaObject(dso3D);
end;

destructor TLakeTableItemMf6.Destroy;
begin
  inherited;

  FObserverList.Free;
  RemoveFormulaObjects;
end;

function TLakeTableItemMf6.GetExchangeArea: string;
begin
  Result := FExchangeArea.Formula;
  ResetItemObserver(ExchangeAreaEvapPosition);
end;

function TLakeTableItemMf6.GetObserver(Index: Integer): TObserver;
begin
  result := FObserverList[Index];
end;

function TLakeTableItemMf6.GetScreenObject: TObject;
begin
  result := nil;
end;

function TLakeTableItemMf6.GetStage: string;
begin
  Result := FStage.Formula;
  ResetItemObserver(StagePosition);
end;

function TLakeTableItemMf6.GetSurfaceArea: string;
begin
  Result := FSurfaceArea.Formula;
  ResetItemObserver(SurfaceAreaPosition);
end;

function TLakeTableItemMf6.GetVolume: string;
begin
  Result := FVolume.Formula;
  ResetItemObserver(VolumePosition);
end;

function TLakeTableItemMf6.IsSame(AnotherItem: TOrderedItem): boolean;
var
  LKSource: TLakeTableItemMf6;
begin
  result := AnotherItem is TLakeTableItemMf6;
  if result then
  begin
    LKSource := TLakeTableItemMf6(AnotherItem);
    result := (Stage = LKSource.Stage)
      and (Volume = LKSource.Volume)
      and (SurfaceArea = LKSource.SurfaceArea)
      and (ExchangeArea = LKSource.ExchangeArea);
  end;
end;

procedure TLakeTableItemMf6.RemoveFormulaObjects;
var
  FormulaManager: TFormulaManager;
begin
  FormulaManager := frmGoPhast.PhastModel.FormulaManager;
  FormulaManager.Remove(FStage, GlobalRemoveFormulaObjectSubscription,
    GlobalRestoreFormulaObjectSubscription, self);
  FormulaManager.Remove(FVolume, GlobalRemoveFormulaObjectSubscription,
    GlobalRestoreFormulaObjectSubscription, self);
  FormulaManager.Remove(FSurfaceArea, GlobalRemoveFormulaObjectSubscription,
    GlobalRestoreFormulaObjectSubscription, self);
  FormulaManager.Remove(FExchangeArea, GlobalRemoveFormulaObjectSubscription,
    GlobalRestoreFormulaObjectSubscription, self);
end;

procedure TLakeTableItemMf6.ResetItemObserver(Index: integer);
var
  Observer: TObserver;
begin
  Observer := FObserverList[Index];
  Observer.UpToDate := True;
end;

procedure TLakeTableItemMf6.SetExchangeArea(const Value: string);
begin
  UpdateFormulaBlocks(Value, ExchangeAreaEvapPosition, FExchangeArea);
end;

procedure TLakeTableItemMf6.SetStage(const Value: string);
begin
  UpdateFormulaBlocks(Value, StagePosition, FStage);
end;

procedure TLakeTableItemMf6.SetSurfaceArea(const Value: string);
begin
  UpdateFormulaBlocks(Value, SurfaceAreaPosition, FSurfaceArea);
end;

procedure TLakeTableItemMf6.SetVolume(const Value: string);
begin
  UpdateFormulaBlocks(Value, VolumePosition, FVolume);
end;

procedure TLakeTableItemMf6.StopTalkingToAnyone;
var
  ObserverIndex: Integer;
begin
  for ObserverIndex := 0 to FObserverList.Count - 1 do
  begin
    FObserverList[ObserverIndex].StopTalkingToAnyone;
  end;
end;

{ TLakeTableMf6 }

function TLakeTableMf6.Add: TLakeTableItemMf6;
begin
  result := inherited Add as TLakeTableItemMf6;
end;

constructor TLakeTableMf6.Create(Model: TBaseModel);
begin
  inherited Create(TLakeTableItemMf6, Model);
end;

function TLakeTableMf6.GetItems(Index: Integer): TLakeTableItemMf6;
begin
  result := inherited Items[Index] as TLakeTableItemMf6;
end;

procedure TLakeTableMf6.SetItems(Index: Integer;
  const Value: TLakeTableItemMf6);
begin
  inherited Items[Index] := Value;
end;

procedure TLakeTableMf6.StopTalkingToAnyone;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].StopTalkingToAnyone;
  end;
end;

initialization
  InitializeLakeObNames
  
finalization 
  LakeObNames.Free; 

end.
