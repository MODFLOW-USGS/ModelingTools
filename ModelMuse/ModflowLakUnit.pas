unit ModflowLakUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, ModflowCellUnit, DataSetUnit, FormulaManagerUnit,
  SubscriptionUnit, GoPhastTypes, PestObsUnit;

type
  TLakRecord = record
    Cell: TCellLocation;
    MinimumStage: double;
    MaximumStage: double;
    Precipitation: double;
    Evaporation: double;
    OverlandRunoff: double;
    Withdrawal: double;
    MinimumStageAnnotation: string;
    MaximumStageAnnotation: string;
    PrecipitationAnnotation: string;
    EvaporationAnnotation: string;
    OverlandRunoffAnnotation: string;
    WithdrawalAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TLakArray = array of TLakRecord;

  TLakStorage = class(TCustomBoundaryStorage)
  private
    FLakArray: TLakArray;
    function GetLakArray: TLakArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property LakArray: TLakArray read GetLakArray;
  end;

  // @name represents a MODFLOW Drain boundary for one time interval.
  // @name is stored by @link(TResCollection).
  TLakItem = class(TCustomModflowBoundaryItem)
  private
    FMaximumStage: TFormulaObject;
    FPrecipitation: TFormulaObject;
    FMinimumStage: TFormulaObject;
    FWithdrawal: TFormulaObject;
    FOverlandRunoff: TFormulaObject;
    FEvaporation: TFormulaObject;
    procedure SetEvaporation(const Value: string);
    procedure SetMaximumStage(const Value: string);
    procedure SetMinimumStage(const Value: string);
    procedure SetOverlandRunoff(const Value: string);
    procedure SetPrecipitation(const Value: string);
    procedure SetWithdrawal(const Value: string);
    function ConvertString(Const AString: string): double;
    function GetEvaporation: string;
    function GetMaximumStage: string;
    function GetMinimumStage: string;
    function GetOverlandRunoff: string;
    function GetPrecipitation: string;
    function GetWithdrawal: string;
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
    function BoundaryFormulaCount: integer; override;
  public
    function SSMN: double;
    function SSMX: double;
    function PRCPLK: double;
    function EVAPLK: double;
    function RNF: double;
    function WTHDRW: double;
    Destructor Destroy; override;
  published
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    property MinimumStage: string read GetMinimumStage write SetMinimumStage;
    property MaximumStage: string read GetMaximumStage write SetMaximumStage;
    property Precipitation: string read GetPrecipitation write SetPrecipitation;
    property Evaporation: string read GetEvaporation write SetEvaporation;
    property OverlandRunoff: string read GetOverlandRunoff write SetOverlandRunoff;
    property Withdrawal: string read GetWithdrawal write SetWithdrawal;
  end;

  TLak_Cell = class(TValueCell)
  private
    Values: TLakRecord;
    StressPeriod: integer;
    function GetEvaporation: double;
    function GetMaximumStage: double;
    function GetMinimumStage: double;
    function GetOverlandRunoff: double;
    function GetPrecipitation: double;
    function GetWithdrawal: double;
    function GetEvaporationAnnotation: string;
    function GetMaximumStageAnnotation: string;
    function GetMinimumStageAnnotation: string;
    function GetOverlandRunoffAnnotation: string;
    function GetPrecipitationAnnotation: string;
    function GetWithdrawalAnnotation: string;
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
  public
    property MinimumStage: double read GetMinimumStage;
    property MaximumStage: double read GetMaximumStage;
    property Precipitation: double read GetPrecipitation;
    property Evaporation: double read GetEvaporation;
    property OverlandRunoff: double read GetOverlandRunoff;
    property Withdrawal: double read GetWithdrawal;
    property MinimumStageAnnotation: string read GetMinimumStageAnnotation;
    property MaximumStageAnnotation: string read GetMaximumStageAnnotation;
    property PrecipitationAnnotation: string read GetPrecipitationAnnotation;
    property EvaporationAnnotation: string read GetEvaporationAnnotation;
    property OverlandRunoffAnnotation: string read GetOverlandRunoffAnnotation;
    property WithdrawalAnnotation: string read GetWithdrawalAnnotation;
  end;

  TLakTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the stages for a series of
    // lake Boundaries over a series of time intervals.
    FMinimumStageData: TModflowTimeList;
    FMaximumStageData: TModflowTimeList;
    FPrecipitationData: TModflowTimeList;
    FEvaporationData: TModflowTimeList;
    FOverlandRunoffData: TModflowTimeList;
    FWithdrawalData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  // @name represents MODFLOW lake boundaries
  // for a series of time intervals.
  TLakCollection = class(TCustomMF_ArrayBoundColl)
  private
    procedure InvalidateMinStageData(Sender: TObject);
    procedure InvalidateMaxStageData(Sender: TObject);
    procedure InvalidatePrecipData(Sender: TObject);
    procedure InvalidateEvapData(Sender: TObject);
    procedure InvalidateRunoffData(Sender: TObject);
    procedure InvalidateWithdrawalData(Sender: TObject);
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    // See @link(TCustomListArrayBoundColl.AssignArrayCellValues
    // TCustomListArrayBoundColl.AssignArrayCellValues)
    procedure AssignArrayCellValues(DataSets: TList; ItemIndex: Integer;
      AModel: TBaseModel); override;
    // See @link(TCustomListArrayBoundColl.InitializeTimeLists
    // TCustomListArrayBoundColl.InitializeTimeLists)
    procedure InitializeTimeLists(ListOfTimeLists: TList;
      AModel: TBaseModel); override;
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TResStorage.ResArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
  end;

  TLakeTableItem = class(TPhastCollectionItem)
  private
    FSurfaceArea: double;
    FVolume: double;
    FStage: double;
    procedure SetStage(const Value: double);
    procedure SetSurfaceArea(const Value: double);
    procedure SetVolume(const Value: double);
    procedure ReadStage(Reader: TReader);
    procedure WriteStage(Writer: TWriter);
    procedure ReadVolume(Reader: TReader);
    procedure WriteVolume(Writer: TWriter);
    procedure ReadSurfaceArea(Reader: TReader);
    procedure WriteSurfaceArea(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Stage: double read FStage write SetStage;
    property Volume: double read FVolume write SetVolume;
    property SurfaceArea: double read FSurfaceArea write SetSurfaceArea;
  end;

  TLakeTable = class(TPhastCollection)
  strict private
    { TODO -cRefactor : Consider replacing FModel with a TNotifyEvent or interface. }
    //
    FModel: TBaseModel;
  private
    function GetItems(Index: integer): TLakeTableItem;
    procedure SetItems(Index: integer; const Value: TLakeTableItem);
  public
    constructor Create(Model: TBaseModel);
    property Items[Index: integer]: TLakeTableItem read GetItems
      write SetItems; default;
    function Add: TLakeTableItem;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    property Model: TBaseModel read FModel;
  end;

  TLakeTableChoice = (lctInternal, lctExternal);

  TExternalLakeTable = class(TGoPhastPersistent)
  strict private
    FModel: TBaseModel;
  private
    FLakeTableChoice: TLakeTableChoice;
    FLakeTable: TLakeTable;
    FFullLakeTableFileName: string;
    procedure SetLakeTable(const Value: TLakeTable);
    procedure SetLakeTableChoice(const Value: TLakeTableChoice);
    procedure SetFullLakeTableFileName(const Value: string);
    function GetLakeTableFileName: string;
    procedure SetLakeTableFileName(const Value: string);
  public
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property Model: TBaseModel read FModel;
    procedure Assign(Source: TPersistent); override;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    Constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    property FullLakeTableFileName: string read FFullLakeTableFileName
      write SetFullLakeTableFileName;
  published
    property LakeTableChoice: TLakeTableChoice read FLakeTableChoice
      write SetLakeTableChoice;
    property LakeTableFileName: string read GetLakeTableFileName
      write SetLakeTableFileName;
    property LakeTable: TLakeTable read FLakeTable write SetLakeTable;
  end;

  TLakeObs = class(TCustomTimeObservationItem)
  private
    FObsType: Integer;
    procedure SetObsType(const Value: Integer);
  protected
    function GetObsTypeIndex: Integer; override;
    procedure SetObsTypeIndex(const Value: Integer); override;
    function GetObsTypeString: string; override;
    procedure SetObsTypeString(const Value: string); override;
  public
    procedure Assign(Source: TPersistent); override;
    function ObservationType: string; override;
    function Units: string; override;
  published
    property ObsType: Integer read FObsType write SetObsType stored True;
    property GUID;
  end;

  TLakeObservations = class(TCustomComparisonCollection)
  private
    FGageOutputName: string;
    function GetLakeItem(Index: Integer): TLakeObs;
    procedure SetLakeItem(Index: Integer; const Value: TLakeObs);
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent; ScreenObject: TObject);
    property Items[Index: Integer]: TLakeObs read GetLakeItem
      write SetLakeItem; default;
    function Add: TLakeObs;
    property GageOutputName: string read FGageOutputName write FGageOutputName;
  end;

  TLakBoundary = class(TModflowBoundary)
  private
    FSill: double;
    FInitialStage: double;
    FCenterLake: integer;
    FLakeID: integer;
    FSubLakes: TList;
    FTrueLakeID: integer;
    FFluxCondGage: boolean;
    FStandardGage: boolean;
    FDeltaGage: boolean;
    FGage4: boolean;
    FExternalLakeTable: TExternalLakeTable;
    FObservations: TLakeObservations;
    procedure SetCenterLake(const Value: integer);
    procedure SetInitialStage(const Value: double);
    procedure SetSill(const Value: double);
    procedure SetLakeID(const Value: integer);
    function GetSubLakeCount: integer;
    function GetSubLake(Index: Integer): TObject;
    procedure SetDeltaGage(const Value: boolean);
    procedure SetFluxCondGage(const Value: boolean);
    procedure SetStandardGage(const Value: boolean);
    function GetOutType: integer;
    procedure SetGage4(const Value: boolean);
    procedure SetExternalLakeTable(const Value: TExternalLakeTable);
    procedure SetObservations(const Value: TLakeObservations);
  protected
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel); override;
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    // @name destroys the current instance of @classname.  Do not call
    // @name directly.  Call Free instead.
    Destructor Destroy; override;
    property SubLakeCount: integer read GetSubLakeCount;
    property SubLakes[Index: integer]: TObject read GetSubLake;
    procedure ClearSubLakes;
    procedure AddSubLake(Lake: TObject);
    property TrueLakeID: integer read FTrueLakeID write FTrueLakeID;
    procedure DeleteSubLake(Index: integer);
    property OutType: integer read GetOutType;
  published
    property InitialStage: double read FInitialStage write SetInitialStage;
    property CenterLake: integer read FCenterLake write SetCenterLake;
    property Sill: double read FSill write SetSill;
    property LakeID: integer read FLakeID write SetLakeID;
    property StandardGage: boolean read FStandardGage write SetStandardGage;
    property FluxCondGage: boolean read FFluxCondGage write SetFluxCondGage;
    property DeltaGage: boolean read FDeltaGage write SetDeltaGage;
    property Gage4: boolean read FGage4 write SetGage4;
    property ExternalLakeTable: TExternalLakeTable read FExternalLakeTable write SetExternalLakeTable;
    property Observations: TLakeObservations read FObservations
      write SetObservations
      {$IFNDEF PEST}
      stored False
      {$ENDIF}
      ;
  end;

var
  LakeGageOutputTypes: TStringList;
  LakeGageUnits: TStringList;

implementation

uses RbwParser, ScreenObjectUnit, PhastModelUnit, ModflowTimeUnit,
  ModflowTransientListParameterUnit, TempFiles,
  frmFormulaErrorsUnit, frmGoPhastUnit;

resourcestring
  StrEvaporationForThe = '(evaporation for the Lake package)';
  StrPrecipitationForT = '(precipitation for the Lake package)';
  StrRunoffForTheLake = '(runoff for the Lake package)';
  StrMinimumStageForT = '(minimum stage for the Lake package)';
  StrMaximumStageForT = '(maximum stage for the Lake package)';
  StrWithdrawalForThe = '(withdrawal for the Lake package)';
  StrMinimumStage = 'Minimum stage';
  StrMaximumStage = 'Maximum stage';
  StrPrecipitation = 'Precipitation';
  StrEvaporation = 'Evaporation';
  StrOverlandRunoff = 'Overland runoff';
  StrWithdrawal = 'Withdrawal';

const
  MinimumStagePosition = 0;
  MaximumStagePosition = 1;
  PrecipitationPosition = 2;
  EvaporationPosition = 3;
  OverlandRunoffPosition = 4;
  WithdrawalPosition = 5;

//  StreamGageOutputTypes: TStringList;

procedure InitializeGageOutputTypes;
begin
  LakeGageOutputTypes := TStringList.Create;
  LakeGageUnits := TStringList.Create;
  
  LakeGageOutputTypes.CaseSensitive := False;
  
  LakeGageOutputTypes.Add('Stage(H)');    LakeGageUnits.Add('L');
  LakeGageOutputTypes.Add('Volume');      LakeGageUnits.Add('L3');
  LakeGageOutputTypes.Add('Precip.');     LakeGageUnits.Add('L3');
  LakeGageOutputTypes.Add('Evap.');       LakeGageUnits.Add('L3');
  LakeGageOutputTypes.Add('Runoff');      LakeGageUnits.Add('L3');
  LakeGageOutputTypes.Add('GW-Inflw');    LakeGageUnits.Add('L3');
  LakeGageOutputTypes.Add('GW-Outflw');   LakeGageUnits.Add('L3');
  LakeGageOutputTypes.Add('SW-Inflw');    LakeGageUnits.Add('L3');
  LakeGageOutputTypes.Add('SW-Outflw');   LakeGageUnits.Add('L3');
  LakeGageOutputTypes.Add('Withdrawal');  LakeGageUnits.Add('L3');
  LakeGageOutputTypes.Add('Lake-Inflx');  LakeGageUnits.Add('L3');
  LakeGageOutputTypes.Add('Total-Cond.'); LakeGageUnits.Add('L2/T');
  LakeGageOutputTypes.Add('Del-H-TS');    LakeGageUnits.Add('L/T');
  LakeGageOutputTypes.Add('Del-V-TS');    LakeGageUnits.Add('L3/T');
  LakeGageOutputTypes.Add('Del-H-Cum');   LakeGageUnits.Add('L');
  LakeGageOutputTypes.Add('Del-V-Cum');   LakeGageUnits.Add('L3');
end;


{ TLakItem }

procedure TLakItem.Assign(Source: TPersistent);
var
  Lak: TLakItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TLakItem then
  begin
    Lak := TLakItem(Source);
    MinimumStage := Lak.MinimumStage;
    MaximumStage := Lak.MaximumStage;
    Precipitation := Lak.Precipitation;
    Evaporation := Lak.Evaporation;
    OverlandRunoff := Lak.OverlandRunoff;
    Withdrawal := Lak.Withdrawal;
  end;
  inherited;
end;

procedure TLakItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TLakCollection;
  MinStageObserver: TObserver;
  MaxStageObserver: TObserver;
  PrecipObserver: TObserver;
  EvapObserver: TObserver;
  RunoffObserver: TObserver;
  WithdrawalObserver: TObserver;
begin
  ParentCollection := Collection as TLakCollection;
  MinStageObserver := FObserverList[MinimumStagePosition];
  MinStageObserver.OnUpToDateSet := ParentCollection.InvalidateMinStageData;
  MaxStageObserver := FObserverList[MaximumStagePosition];
  MaxStageObserver.OnUpToDateSet := ParentCollection.InvalidateMaxStageData;
  PrecipObserver := FObserverList[PrecipitationPosition];
  PrecipObserver.OnUpToDateSet := ParentCollection.InvalidatePrecipData;
  EvapObserver := FObserverList[EvaporationPosition];
  EvapObserver.OnUpToDateSet := ParentCollection.InvalidateEvapData;
  RunoffObserver := FObserverList[OverlandRunoffPosition];
  RunoffObserver.OnUpToDateSet := ParentCollection.InvalidateRunoffData;
  WithdrawalObserver := FObserverList[WithdrawalPosition];
  WithdrawalObserver.OnUpToDateSet := ParentCollection.InvalidateWithdrawalData;
end;

function TLakItem.BoundaryFormulaCount: integer;
begin
  result := 6;
end;

function TLakItem.ConvertString(const AString: string): double;
var
  Compiler: TRbwParser;
  Model: TPhastModel;
  OrderedCollection: TOrderedCollection;
  TempFormula: string;
begin
  OrderedCollection := Collection as TOrderedCollection;
  Model := OrderedCollection.Model as TPhastModel;
  Compiler := Model.rpThreeDFormulaCompiler;

  TempFormula := AString;
  Compiler.Compile(TempFormula);
  Compiler.CurrentExpression.Evaluate;
  result := Compiler.CurrentExpression.DoubleResult;
end;

procedure TLakItem.CreateFormulaObjects;
begin
  inherited;
  FMinimumStage := CreateFormulaObject(dso3D);
  FMaximumStage := CreateFormulaObject(dso3D);
  FPrecipitation := CreateFormulaObject(dso3D);
  FEvaporation := CreateFormulaObject(dso3D);
  FOverlandRunoff := CreateFormulaObject(dso3D);
  FWithdrawal := CreateFormulaObject(dso3D);
end;

destructor TLakItem.Destroy;
begin
  MinimumStage := '0';
  MaximumStage := '0';
  Precipitation := '0';
  Evaporation := '0';
  OverlandRunoff := '0';
  Withdrawal := '0';
  inherited;
end;

function TLakItem.EVAPLK: double;
var
  LocalScreenObject: TScreenObject;
begin
  try
    result := ConvertString(Evaporation);
  except on E: ERbwParserError do
    begin
      LocalScreenObject := ScreenObject as TScreenObject;
      frmFormulaErrors.AddFormulaError(LocalScreenObject.Name,
        StrEvaporationForThe,
        Evaporation, E.Message);
      Evaporation := '0.';
      result := ConvertString(Evaporation);
    end;
  end;
end;

function TLakItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    MinimumStagePosition: result := MinimumStage;
    MaximumStagePosition: result := MaximumStage;
    PrecipitationPosition: result := Precipitation;
    EvaporationPosition: result := Evaporation;
    OverlandRunoffPosition: result := OverlandRunoff;
    WithdrawalPosition: result := Withdrawal;
    else Assert(False);
  end;
end;

function TLakItem.GetEvaporation: string;
begin
  Result := FEvaporation.Formula;
  ResetItemObserver(EvaporationPosition);
end;

function TLakItem.GetMaximumStage: string;
begin
  Result := FMaximumStage.Formula;
  ResetItemObserver(MaximumStagePosition);
end;

function TLakItem.GetMinimumStage: string;
begin
  Result := FMinimumStage.Formula;
  ResetItemObserver(MinimumStagePosition);
end;

function TLakItem.GetOverlandRunoff: string;
begin
  Result := FOverlandRunoff.Formula;
  ResetItemObserver(OverlandRunoffPosition);
end;

function TLakItem.GetPrecipitation: string;
begin
  Result := FPrecipitation.Formula;
  ResetItemObserver(PrecipitationPosition);
end;

procedure TLakItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FMinimumStage then
  begin
    List.Add(FObserverList[MinimumStagePosition]);
  end;
  if Sender = FMaximumStage then
  begin
    List.Add(FObserverList[MaximumStagePosition]);
  end;
  if Sender = FPrecipitation then
  begin
    List.Add(FObserverList[PrecipitationPosition]);
  end;
  if Sender = FEvaporation then
  begin
    List.Add(FObserverList[EvaporationPosition]);
  end;
  if Sender = FOverlandRunoff then
  begin
    List.Add(FObserverList[OverlandRunoffPosition]);
  end;
  if Sender = FWithdrawal then
  begin
    List.Add(FObserverList[WithdrawalPosition]);
  end;
end;

function TLakItem.GetWithdrawal: string;
begin
  Result := FWithdrawal.Formula;
  ResetItemObserver(WithdrawalPosition);
end;

function TLakItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TLakItem;
begin
  result := (AnotherItem is TLakItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TLakItem(AnotherItem);
    result :=
      (Item.MinimumStage = MinimumStage)
      and (Item.MaximumStage = MaximumStage)
      and (Item.Precipitation = Precipitation)
      and (Item.Evaporation = Evaporation)
      and (Item.OverlandRunoff = OverlandRunoff)
      and (Item.Withdrawal = Withdrawal);
  end;
end;

function TLakItem.PRCPLK: double;
var
  LocalScreenObject: TScreenObject;
begin
  try
    result := ConvertString(Precipitation);
  except on E: ERbwParserError do
    begin
      LocalScreenObject := ScreenObject as TScreenObject;
      frmFormulaErrors.AddFormulaError(LocalScreenObject.Name,
        StrPrecipitationForT,
        Precipitation, E.Message);
      Precipitation := '0.';
      result := ConvertString(Precipitation);
    end;
  end;
end;

procedure TLakItem.RemoveFormulaObjects;
begin
  inherited;
  frmGoPhast.PhastModel.FormulaManager.Remove(FWithdrawal,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FOverlandRunoff,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FEvaporation,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FPrecipitation,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FMaximumStage,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FMinimumStage,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
end;

function TLakItem.RNF: double;
var
  LocalScreenObject: TScreenObject;
begin
  try
    result := ConvertString(OverlandRunoff);
  except on E: ERbwParserError do
    begin
      LocalScreenObject := ScreenObject as TScreenObject;
      frmFormulaErrors.AddFormulaError(LocalScreenObject.Name,
        StrRunoffForTheLake,
        OverlandRunoff, E.Message);
      OverlandRunoff := '0.';
      result := ConvertString(OverlandRunoff);
    end;
  end;
end;

procedure TLakItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  inherited;
  case Index of
    MinimumStagePosition: MinimumStage := Value;
    MaximumStagePosition: MaximumStage := Value;
    PrecipitationPosition: Precipitation := Value;
    EvaporationPosition: Evaporation := Value;
    OverlandRunoffPosition: OverlandRunoff := Value;
    WithdrawalPosition: Withdrawal := Value;
    else Assert(False);
  end;
end;

procedure TLakItem.SetEvaporation(const Value: string);
begin
  UpdateFormula(Value, EvaporationPosition, FEvaporation);
end;

procedure TLakItem.SetMaximumStage(const Value: string);
begin
//  MinimumStagePosition = 0;
//  MaximumStagePosition = 1;
//  PrecipitationPosition = 2;
//  EvaporationPosition = 3;
//  OverlandRunoffPosition = 4;
//  WithdrawalPosition = 5;

//    FMaximumStage: TFormulaObject;
//    FPrecipitation: TFormulaObject;
//    FMinimumStage: TFormulaObject;
//    FWithdrawal: TFormulaObject;
//    FOverlandRunoff: TFormulaObject;
//    FEvaporation: TFormulaObject;
  UpdateFormula(Value, MaximumStagePosition, FMaximumStage);
end;

procedure TLakItem.SetMinimumStage(const Value: string);
begin
  UpdateFormula(Value, MinimumStagePosition, FMinimumStage);
end;

procedure TLakItem.SetOverlandRunoff(const Value: string);
begin
  UpdateFormula(Value, OverlandRunoffPosition, FOverlandRunoff);
end;

procedure TLakItem.SetPrecipitation(const Value: string);
begin
  UpdateFormula(Value, PrecipitationPosition, FPrecipitation);
end;

procedure TLakItem.SetWithdrawal(const Value: string);
begin
  UpdateFormula(Value, WithdrawalPosition, FWithdrawal);
end;

function TLakItem.SSMN: double;
var
  LocalScreenObject: TScreenObject;
begin
  try
    result := ConvertString(MinimumStage);
  except on E: ERbwParserError do
    begin
      LocalScreenObject := ScreenObject as TScreenObject;
      frmFormulaErrors.AddFormulaError(LocalScreenObject.Name,
        StrMinimumStageForT,
        MinimumStage, E.Message);
      MinimumStage := '0.';
      result := ConvertString(MinimumStage);
    end;
  end;
end;

function TLakItem.SSMX: double;
var
  LocalScreenObject: TScreenObject;
begin
  try
    result := ConvertString(MaximumStage);
  except on E: ERbwParserError do
    begin
      LocalScreenObject := ScreenObject as TScreenObject;
      frmFormulaErrors.AddFormulaError(LocalScreenObject.Name,
        StrMaximumStageForT,
        MaximumStage, E.Message);
      MaximumStage := '0.';
      result := ConvertString(MaximumStage);
    end;
  end;
end;

function TLakItem.WTHDRW: double;
var
  LocalScreenObject: TScreenObject;
begin
  try
  result := ConvertString(Withdrawal);
  except on E: ERbwParserError do
    begin
      LocalScreenObject := ScreenObject as TScreenObject;
      frmFormulaErrors.AddFormulaError(LocalScreenObject.Name,
        StrWithdrawalForThe,
        Withdrawal, E.Message);
      Withdrawal := '0.';
      result := ConvertString(Withdrawal);
    end;
  end;
end;

{ TLak_Cell }

procedure TLak_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TLak_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TLak_Cell.GetEvaporation: double;
begin
  result := Values.Evaporation;
end;

function TLak_Cell.GetEvaporationAnnotation: string;
begin
  result := Values.EvaporationAnnotation;
end;

function TLak_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TLak_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TLak_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TLak_Cell.GetMaximumStage: double;
begin
  result := Values.MaximumStage;
end;

function TLak_Cell.GetMaximumStageAnnotation: string;
begin
  result := Values.MaximumStageAnnotation;
end;

function TLak_Cell.GetMinimumStage: double;
begin
  result := Values.MinimumStage;
end;

function TLak_Cell.GetMinimumStageAnnotation: string;
begin
  result := Values.MinimumStageAnnotation;
end;

function TLak_Cell.GetOverlandRunoff: double;
begin
  result := Values.OverlandRunoff;
end;

function TLak_Cell.GetOverlandRunoffAnnotation: string;
begin
  result := Values.OverlandRunoffAnnotation;
end;

function TLak_Cell.GetPrecipitation: double;
begin
  result := Values.Precipitation;
end;

function TLak_Cell.GetPrecipitationAnnotation: string;
begin
  result := Values.PrecipitationAnnotation;
end;

function TLak_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    0: result := Values.MinimumStageAnnotation;
    1: result := Values.MaximumStageAnnotation;
    2: result := Values.PrecipitationAnnotation;
    3: result := Values.EvaporationAnnotation;
    4: result := Values.OverlandRunoffAnnotation;
    5: result := Values.WithdrawalAnnotation;
    else Assert(False);
  end;
end;

function TLak_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    0: result := Values.MinimumStage;
    1: result := Values.MaximumStage;
    2: result := Values.Precipitation;
    3: result := Values.Evaporation;
    4: result := Values.OverlandRunoff;
    5: result := Values.Withdrawal;
    else Assert(False);
  end;
end;

function TLak_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TLak_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TLak_Cell.GetWithdrawal: double;
begin
  result := Values.Withdrawal;
end;

function TLak_Cell.GetWithdrawalAnnotation: string;
begin
  result := Values.WithdrawalAnnotation;
end;

procedure TLak_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TLak_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TLak_Cell.SetColumn(const Value: integer);
begin
  Values.Cell.Column := Value;
end;

procedure TLak_Cell.SetLayer(const Value: integer);
begin
  Values.Cell.Layer := Value;
end;

procedure TLak_Cell.SetRow(const Value: integer);
begin
  Values.Cell.Row := Value;
end;

{ TLakCollection }

procedure TLakCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TLakStorage.Create(AModel));
end;

procedure TLakCollection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel);
var
  MinimumStageArray: TDataArray;
  MaximumStageArray: TDataArray;
  PrecipitationArray: TDataArray;
  EvaporationArray: TDataArray;
  OverlandRunoffArray: TDataArray;
  WithdrawalArray: TDataArray;
  Boundary: TLakStorage;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  BoundaryIndex: Integer;
  LocalModel: TCustomModel;
  LayerMin: Integer;
  RowMin: Integer;
  ColMin: Integer;
  LayerMax: Integer;
  RowMax: Integer;
  ColMax: Integer;
begin
  LocalModel := AModel as TCustomModel;
  BoundaryIndex := 0;
  MinimumStageArray := DataSets[0];
  MaximumStageArray := DataSets[1];
  PrecipitationArray := DataSets[2];
  EvaporationArray := DataSets[3];
  OverlandRunoffArray := DataSets[4];
  WithdrawalArray := DataSets[5];
  Boundary := Boundaries[ItemIndex, AModel] as TLakStorage;
  MinimumStageArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
    LayerMax, RowMax, ColMax);
  if LayerMin >= 0 then
  begin
    for LayerIndex := LayerMin to LayerMax do
    begin
      if LocalModel.IsLayerSimulated(LayerIndex) then
      begin
        for RowIndex := RowMin to RowMax do
        begin
          for ColIndex := ColMin to ColMax do
          begin
            if MinimumStageArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              Assert(MaximumStageArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(PrecipitationArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(EvaporationArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(OverlandRunoffArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(WithdrawalArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              with Boundary.LakArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                MinimumStage := MinimumStageArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                MinimumStageAnnotation := MinimumStageArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];

                MaximumStage := MaximumStageArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                MaximumStageAnnotation := MaximumStageArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];

                Precipitation := PrecipitationArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                PrecipitationAnnotation := PrecipitationArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];

                Evaporation := EvaporationArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                EvaporationAnnotation := EvaporationArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];

                OverlandRunoff := OverlandRunoffArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                OverlandRunoffAnnotation := OverlandRunoffArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];

                Withdrawal := WithdrawalArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                WithdrawalAnnotation := WithdrawalArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  MinimumStageArray.CacheData;
  MaximumStageArray.CacheData;
  PrecipitationArray.CacheData;
  EvaporationArray.CacheData;
  OverlandRunoffArray.CacheData;
  WithdrawalArray.CacheData;
  Boundary.CacheData;
end;

function TLakCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TLakTimeListLink;
end;

procedure TLakCollection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TLakItem;
  Boundary: TLakBoundary;
  ScreenObject: TScreenObject;
  ALink: TLakTimeListLink;
  MinimumStageData: TModflowTimeList;
  MaximumStageData: TModflowTimeList;
  PrecipitationData: TModflowTimeList;
  EvaporationData: TModflowTimeList;
  OverlandRunoffData: TModflowTimeList;
  WithdrawalData: TModflowTimeList;
begin
  Boundary := BoundaryGroup as TLakBoundary;
  ScreenObject := Boundary.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TLakItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.MinimumStage;
  end;
  ALink := TimeListLink.GetLink(AModel) as TLakTimeListLink;
  MinimumStageData := ALink.FMinimumStageData;
  MinimumStageData.Initialize(BoundaryValues, ScreenObject, lctIgnore);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TLakItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.MaximumStage;
  end;
  MaximumStageData := ALink.FMaximumStageData;
  MaximumStageData.Initialize(BoundaryValues, ScreenObject, lctIgnore);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TLakItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.Precipitation;
  end;
  PrecipitationData := ALink.FPrecipitationData;
  PrecipitationData.Initialize(BoundaryValues, ScreenObject, lctIgnore);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TLakItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.Evaporation;
  end;
  EvaporationData := ALink.FEvaporationData;
  EvaporationData.Initialize(BoundaryValues, ScreenObject, lctIgnore);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TLakItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.OverlandRunoff;
  end;
  OverlandRunoffData := ALink.FOverlandRunoffData;
  OverlandRunoffData.Initialize(BoundaryValues, ScreenObject, lctIgnore);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TLakItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.Withdrawal;
  end;
  WithdrawalData := ALink.FWithdrawalData;
  WithdrawalData.Initialize(BoundaryValues, ScreenObject, lctIgnore);

  Assert(MinimumStageData.Count = Count);
  Assert(MaximumStageData.Count = Count);
  Assert(PrecipitationData.Count = Count);
  Assert(EvaporationData.Count = Count);
  Assert(OverlandRunoffData.Count = Count);
  Assert(WithdrawalData.Count = Count);

  ClearBoundaries(AModel);
  SetBoundaryCapacity(MinimumStageData.Count, AModel);
  for TimeIndex := 0 to MinimumStageData.Count - 1 do
  begin
    AddBoundary(TLakStorage.Create(AModel));
  end;

  ListOfTimeLists.Add(MinimumStageData);
  ListOfTimeLists.Add(MaximumStageData);
  ListOfTimeLists.Add(PrecipitationData);
  ListOfTimeLists.Add(EvaporationData);
  ListOfTimeLists.Add(OverlandRunoffData);
  ListOfTimeLists.Add(WithdrawalData);
end;

procedure TLakCollection.InvalidateEvapData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TLakTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TLakTimeListLink;
    Link.FEvaporationData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TLakTimeListLink;
      Link.FEvaporationData.Invalidate;
    end;
  end;
end;

procedure TLakCollection.InvalidateMaxStageData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TLakTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TLakTimeListLink;
    Link.FMaximumStageData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TLakTimeListLink;
      Link.FMaximumStageData.Invalidate;
    end;
  end;
end;

procedure TLakCollection.InvalidateMinStageData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TLakTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TLakTimeListLink;
    Link.FMinimumStageData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TLakTimeListLink;
      Link.FMinimumStageData.Invalidate;
    end;
  end;
end;

procedure TLakCollection.InvalidatePrecipData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TLakTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TLakTimeListLink;
    Link.FPrecipitationData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TLakTimeListLink;
      Link.FPrecipitationData.Invalidate;
    end;
  end;
end;

procedure TLakCollection.InvalidateRunoffData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TLakTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TLakTimeListLink;
    Link.FOverlandRunoffData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TLakTimeListLink;
      Link.FOverlandRunoffData.Invalidate;
    end;
  end;
end;

procedure TLakCollection.InvalidateWithdrawalData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TLakTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TLakTimeListLink;
    Link.FWithdrawalData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TLakTimeListLink;
      Link.FWithdrawalData.Invalidate;
    end;
  end;
end;

class function TLakCollection.ItemClass: TBoundaryItemClass;
begin
  result := TLakItem;
end;

procedure TLakCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TLakStorage).FLakArray, BoundaryCount);
  inherited;
end;

{ TLakBoundary }

procedure TLakBoundary.AddSubLake(Lake: TObject);
begin
  Assert(Lake is TScreenObject);
  FSubLakes.Add(Lake);
end;

procedure TLakBoundary.Assign(Source: TPersistent);
var
  Lake: TLakBoundary;
begin
  if Source is TLakBoundary then
  begin
    Lake := TLakBoundary(Source);
    InitialStage := Lake.InitialStage;
    CenterLake := Lake.CenterLake;
    Sill := Lake.Sill;
    LakeID := Lake.LakeID;
    StandardGage := Lake.StandardGage;
    FluxCondGage := Lake.FluxCondGage;
    DeltaGage := Lake.DeltaGage;
    Gage4 := Lake.Gage4;
    ExternalLakeTable := Lake.ExternalLakeTable;
    Observations := Lake.Observations;
  end;
  inherited;
end;

procedure TLakBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TLak_Cell;
  BoundaryValues: TLakRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TLakStorage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TLakStorage;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TLak_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.LakArray) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.LakArray)
      end;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.LakArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.LakArray[BoundaryIndex];
        Cell := TLak_Cell.Create;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
        Cell.ScreenObject := ScreenObject;
        LocalModel.AdjustCellPosition(Cell);
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TLakBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TLakCollection;
end;

procedure TLakBoundary.ClearSubLakes;
begin
  FSubLakes.Clear;
end;

constructor TLakBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
var
  OnInvalidateModelEvent: TNotifyEvent;
begin
  if Model = nil then
  begin
    OnInvalidateModelEvent := nil;
  end
  else
  begin
    OnInvalidateModelEvent := Model.Invalidate;
  end;
  inherited;
  FSubLakes:= TList.Create;
  FExternalLakeTable := TExternalLakeTable.Create(Model);
  FObservations := TLakeObservations.Create(OnInvalidateModelEvent, ScreenObject);
end;

procedure TLakBoundary.DeleteSubLake(Index: integer);
begin
  FSubLakes.Delete(Index);
end;

destructor TLakBoundary.Destroy;
begin
  FObservations.Free;
  FExternalLakeTable.Free;
  FSubLakes.Free;
  inherited;
end;

procedure TLakBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TLakStorage;
begin
  EvaluateArrayBoundaries(AModel);
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TLakStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
end;

function TLakBoundary.GetOutType: integer;
begin
  if StandardGage then
  begin
    result := 0;
    if FluxCondGage then
    begin
      result := 1;
    end;
    if DeltaGage then
    begin
      result := result + 2;
    end;
  end
  else
  begin
    result := -1;
  end;
end;

function TLakBoundary.GetSubLake(Index: Integer): TObject;
begin
  result := FSubLakes[Index];
end;

function TLakBoundary.GetSubLakeCount: integer;
begin
  result := FSubLakes.Count;
end;

procedure TLakBoundary.SetCenterLake(const Value: integer);
begin
  if FCenterLake <> Value then
  begin
    FCenterLake := Value;
    InvalidateModel;
  end;
end;

procedure TLakBoundary.SetDeltaGage(const Value: boolean);
begin
  if FDeltaGage <> Value then
  begin
    FDeltaGage := Value;
    InvalidateModel;
  end;
end;

procedure TLakBoundary.SetExternalLakeTable(const Value: TExternalLakeTable);
begin
  FExternalLakeTable.Assign(Value);
end;

procedure TLakBoundary.SetFluxCondGage(const Value: boolean);
begin
  if FFluxCondGage <> Value then
  begin
    FFluxCondGage := Value;
    InvalidateModel;
  end;
end;

procedure TLakBoundary.SetGage4(const Value: boolean);
begin
  if FGage4 <> Value then
  begin
    FGage4 := Value;
    InvalidateModel;
  end;
end;

procedure TLakBoundary.SetInitialStage(const Value: double);
begin
  if FInitialStage <> Value then
  begin
    FInitialStage := Value;
    InvalidateModel;
  end;
end;

procedure TLakBoundary.SetLakeID(const Value: integer);
begin
  if FLakeID <> Value then
  begin
    FLakeID := Value;
    InvalidateModel;
    if (ScreenObject <> nil)
        and (ScreenObject as TScreenObject).CanInvalidateModel
        and (ParentModel <> nil) then
    begin
      (ParentModel as TPhastModel).DischargeRoutingUpdate;
    end;
  end;
end;

procedure TLakBoundary.SetObservations(const Value: TLakeObservations);
begin
  FObservations.Assign(Value);
end;

procedure TLakBoundary.SetSill(const Value: double);
begin
  if FSill <> Value then
  begin
    FSill := Value;
    InvalidateModel;
  end;
end;

procedure TLakBoundary.SetStandardGage(const Value: boolean);
begin
  if FStandardGage <> Value then
  begin
    FStandardGage := Value;
    InvalidateModel;
  end;
end;

{ TLakRecord }

procedure TLakRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, MinimumStage);
  WriteCompReal(Comp, MaximumStage);
  WriteCompReal(Comp, Precipitation);
  WriteCompReal(Comp, Evaporation);
  WriteCompReal(Comp, OverlandRunoff);
  WriteCompReal(Comp, Withdrawal);

  WriteCompInt(Comp, Strings.IndexOf(MinimumStageAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(MaximumStageAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(PrecipitationAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(EvaporationAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(OverlandRunoffAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(WithdrawalAnnotation));
//  WriteCompString(Comp, MinimumStageAnnotation);
//  WriteCompString(Comp, MaximumStageAnnotation);
//  WriteCompString(Comp, PrecipitationAnnotation);
//  WriteCompString(Comp, EvaporationAnnotation);
//  WriteCompString(Comp, OverlandRunoffAnnotation);
//  WriteCompString(Comp, WithdrawalAnnotation);
end;

procedure TLakRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(MinimumStageAnnotation);
  Strings.Add(MaximumStageAnnotation);
  Strings.Add(PrecipitationAnnotation);
  Strings.Add(EvaporationAnnotation);
  Strings.Add(OverlandRunoffAnnotation);
  Strings.Add(WithdrawalAnnotation);
end;

procedure TLakRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  MinimumStage := ReadCompReal(Decomp);
  MaximumStage := ReadCompReal(Decomp);
  Precipitation := ReadCompReal(Decomp);
  Evaporation := ReadCompReal(Decomp);
  OverlandRunoff := ReadCompReal(Decomp);
  Withdrawal := ReadCompReal(Decomp);

  MinimumStageAnnotation := Annotations[ReadCompInt(Decomp)];
  MaximumStageAnnotation := Annotations[ReadCompInt(Decomp)];
  PrecipitationAnnotation := Annotations[ReadCompInt(Decomp)];
  EvaporationAnnotation := Annotations[ReadCompInt(Decomp)];
  OverlandRunoffAnnotation := Annotations[ReadCompInt(Decomp)];
  WithdrawalAnnotation := Annotations[ReadCompInt(Decomp)];
//  MinimumStageAnnotation := ReadCompString(Decomp, Annotations);
//  MaximumStageAnnotation := ReadCompString(Decomp, Annotations);
//  PrecipitationAnnotation := ReadCompString(Decomp, Annotations);
//  EvaporationAnnotation := ReadCompString(Decomp, Annotations);
//  OverlandRunoffAnnotation := ReadCompString(Decomp, Annotations);
//  WithdrawalAnnotation := ReadCompString(Decomp, Annotations);
end;

{ TLakStorage }
procedure TLakStorage.Clear;
begin
  SetLength(FLakArray, 0);
  FCleared := True;
end;

procedure TLakStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FLakArray);
    for Index := 0 to Count - 1 do
    begin
      FLakArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FLakArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TLakStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Count: Integer;
  Index: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FLakArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FLakArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TLakStorage.GetLakArray: TLakArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FLakArray;
end;

{ TLakTimeListLink }

procedure TLakTimeListLink.CreateTimeLists;
begin
  inherited;
  FMinimumStageData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FMaximumStageData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FPrecipitationData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FEvaporationData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FOverlandRunoffData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FWithdrawalData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FMinimumStageData.NonParamDescription := StrMinimumStage;
  FMinimumStageData.ParamDescription := ' ' + LowerCase(StrMinimumStage);
  FMaximumStageData.NonParamDescription := StrMaximumStage;
  FMaximumStageData.ParamDescription := ' ' + LowerCase(StrMaximumStage);
  FPrecipitationData.NonParamDescription := StrPrecipitation;
  FPrecipitationData.ParamDescription := ' ' + LowerCase(StrPrecipitation);
  FEvaporationData.NonParamDescription := StrEvaporation;
  FEvaporationData.ParamDescription := ' ' + LowerCase(StrEvaporation);
  FOverlandRunoffData.NonParamDescription := StrOverlandRunoff;
  FOverlandRunoffData.ParamDescription := ' ' + LowerCase(StrOverlandRunoff);
  FWithdrawalData.NonParamDescription := StrWithdrawal;
  FWithdrawalData.ParamDescription := ' ' + LowerCase(StrWithdrawal);
  FMinimumStageData.DataType := rdtDouble;
  FMaximumStageData.DataType := rdtDouble;
  FPrecipitationData.DataType := rdtDouble;
  FEvaporationData.DataType := rdtDouble;
  FOverlandRunoffData.DataType := rdtDouble;
  FWithdrawalData.DataType := rdtDouble;
  AddTimeList(FMinimumStageData);
  AddTimeList(FMaximumStageData);
  AddTimeList(FPrecipitationData);
  AddTimeList(FEvaporationData);
  AddTimeList(FOverlandRunoffData);
  AddTimeList(FWithdrawalData);
end;

destructor TLakTimeListLink.Destroy;
begin
  FMinimumStageData.Free;
  FMaximumStageData.Free;
  FPrecipitationData.Free;
  FEvaporationData.Free;
  FOverlandRunoffData.Free;
  FWithdrawalData.Free;
  inherited;
end;

{ TLakeTableItem }

procedure TLakeTableItem.Assign(Source: TPersistent);
var
  SourceItem: TLakeTableItem;
begin
  if Source is TLakeTableItem then
  begin
    SourceItem := TLakeTableItem(Source);
    Stage := SourceItem.Stage;
    Volume := SourceItem.Volume;
    SurfaceArea := SourceItem.SurfaceArea;
  end
  else
  begin
    inherited;
  end;
end;

procedure TLakeTableItem.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Stage', ReadStage, WriteStage, Stage = 0);
  Filer.DefineProperty('Volume', ReadVolume, WriteVolume, Volume = 0);
  Filer.DefineProperty('SurfaceArea', ReadSurfaceArea, WriteSurfaceArea,
    SurfaceArea = 0);
end;

procedure TLakeTableItem.ReadStage(Reader: TReader);
begin
  Stage := Reader.ReadFloat;
end;

procedure TLakeTableItem.ReadSurfaceArea(Reader: TReader);
begin
  SurfaceArea := Reader.ReadFloat;
end;

procedure TLakeTableItem.ReadVolume(Reader: TReader);
begin
  Volume := Reader.ReadFloat;
end;

procedure TLakeTableItem.SetStage(const Value: double);
begin
  SetRealProperty(FStage, Value);
end;

procedure TLakeTableItem.SetSurfaceArea(const Value: double);
begin
  SetRealProperty(FSurfaceArea, Value);
end;

procedure TLakeTableItem.SetVolume(const Value: double);
begin
  SetRealProperty(FVolume, Value);
end;

procedure TLakeTableItem.WriteStage(Writer: TWriter);
begin
  Writer.WriteFloat(Stage);
end;

procedure TLakeTableItem.WriteSurfaceArea(Writer: TWriter);
begin
  Writer.WriteFloat(SurfaceArea);
end;

procedure TLakeTableItem.WriteVolume(Writer: TWriter);
begin
  Writer.WriteFloat(Volume);
end;

{ TLakeTable }

function TLakeTable.Add: TLakeTableItem;
begin
  result := inherited Add as TLakeTableItem;
end;

constructor TLakeTable.Create(Model: TBaseModel);
var
  InvalidateModelEvent: TNotifyEvent;
begin
  FModel := Model;
  if Model = nil then
  begin
    InvalidateModelEvent := nil;
  end
  else
  begin
    InvalidateModelEvent := Model.Invalidate;
  end;
  inherited Create(TLakeTableItem, InvalidateModelEvent);
end;

function TLakeTable.GetItems(Index: integer): TLakeTableItem;
begin
  result := inherited Items[Index] as TLakeTableItem
end;

procedure TLakeTable.SetItems(Index: integer; const Value: TLakeTableItem);
begin
  inherited Items[Index] := Value;
end;

{ TExternalLakeTable }

procedure TExternalLakeTable.Assign(Source: TPersistent);
var
  SourceLakeTable: TExternalLakeTable;
begin
  if Source is TExternalLakeTable then
  begin
    SourceLakeTable := TExternalLakeTable(Source);
    LakeTableChoice := SourceLakeTable.LakeTableChoice;
    FullLakeTableFileName := SourceLakeTable.FullLakeTableFileName;
    LakeTableFileName := SourceLakeTable.FullLakeTableFileName;
    LakeTable := SourceLakeTable.LakeTable;
  end
  else
  begin
    inherited;
  end;
end;

constructor TExternalLakeTable.Create(Model: TBaseModel);
begin
  if Model = nil then
  begin
    inherited Create(nil);
  end
  else
  begin
    inherited Create(Model.Invalidate);
  end;
  Assert((Model = nil) or (Model is TCustomModel));
  FModel := Model;
  FLakeTable := TLakeTable.Create(Model);
  FLakeTableChoice := lctInternal;
end;

destructor TExternalLakeTable.Destroy;
begin
  FLakeTable.Free;
  inherited;
end;

function TExternalLakeTable.GetLakeTableFileName: string;
var
  LocalModel: TPhastModel;
begin
  if Model = nil then
  begin
    result := FullLakeTableFileName;
  end
  else
  begin
    LocalModel := Model as TPhastModel;
    result := ExtractRelativePath(LocalModel.ModelFileName, FullLakeTableFileName);
  end;
end;

procedure TExternalLakeTable.SetLakeTable(const Value: TLakeTable);
begin
  FLakeTable.Assign(Value);
end;

procedure TExternalLakeTable.SetLakeTableChoice(const Value: TLakeTableChoice);
begin
  if FLakeTableChoice <> Value then
  begin
    FLakeTableChoice := Value;
    InvalidateModel;
  end;
end;

procedure TExternalLakeTable.SetLakeTableFileName(const Value: string);
var
  LocalModel: TPhastModel;
  CurDir: string;
begin
  if Model = nil then
  begin
//    FullLakeTableFileName := Value;
  end
  else
  begin
    LocalModel := Model as TPhastModel;
    CurDir := GetCurrentDir;
    try
      SetCurrentDir(ExtractFileDir(LocalModel.ModelFileName));
      FullLakeTableFileName := ExpandFileName(Value);
    finally
      SetCurrentDir(CurDir);
    end;
  end;
end;

procedure TExternalLakeTable.SetFullLakeTableFileName(const Value: string);
begin
  SetStringProperty(FFullLakeTableFileName, Value);
end;

{ TLakeObs }

procedure TLakeObs.Assign(Source: TPersistent);
begin
  if Source is TLakeObs then
  begin
    ObsType := TLakeObs(Source).ObsType;
  end;
  inherited;

end;

function TLakeObs.GetObsTypeIndex: Integer;
begin
  result := ObsType;
end;

function TLakeObs.GetObsTypeString: string;
begin
  result := ObservationType;
end;

function TLakeObs.ObservationType: string;
begin
  if (FObsType >= 0) and (FObsType < LakeGageOutputTypes.Count) then
  begin
    result := LakeGageOutputTypes[FObsType]
  end
  else
  begin
    result := inherited;
  end;
end;

procedure TLakeObs.SetObsType(const Value: Integer);
begin
  SetIntegerProperty(FObsType, Value);
end;

procedure TLakeObs.SetObsTypeIndex(const Value: Integer);
begin
  ObsType := Value;
end;

procedure TLakeObs.SetObsTypeString(const Value: string);
begin
  inherited;
  ObsType := LakeGageOutputTypes.IndexOf(Value);
end;

function TLakeObs.Units: string;
begin
  if (FObsType >= 0) and (FObsType < LakeGageUnits.Count) then
  begin
    result := LakeGageUnits[FObsType]
  end
  else
  begin
    result := inherited;
  end;

end;

{ TLakeObservations }

function TLakeObservations.Add: TLakeObs;
begin
  result := inherited Add as TLakeObs;
end;

constructor TLakeObservations.Create(InvalidateModelEvent: TNotifyEvent;
  ScreenObject: TObject);
begin
  inherited Create(TLakeObs, InvalidateModelEvent, ScreenObject);
end;

function TLakeObservations.GetLakeItem(Index: Integer): TLakeObs;
begin
  result := inherited Items[Index] as TLakeObs;
end;

procedure TLakeObservations.SetLakeItem(Index: Integer; const Value: TLakeObs);
begin
  inherited Items[Index] := Value;
end;

Initialization
  InitializeGageOutputTypes;

Finalization
  LakeGageOutputTypes.Free;
  LakeGageUnits.Free;

end.
