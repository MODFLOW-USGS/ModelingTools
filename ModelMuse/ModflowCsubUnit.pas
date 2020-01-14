unit ModflowCsubUnit;

interface

uses
  GoPhastTypes, System.Classes, ModflowCellUnit, System.ZLib,
  ModflowBoundaryUnit, FormulaManagerUnit, OrderedCollectionUnit, RbwParser,
  RealListUnit, System.SysUtils, System.Contnrs;

type
  TCSubPackageData = class(TPhastCollectionItem)
  private
    FInterbedSystemName: string;
    FUsed: Boolean;
    FInterbed: TObject;
    FDelayKv: TFormulaObject;
    FEquivInterbedNumber: TFormulaObject;
    FInitialDelayHeadOffset: TFormulaObject;
    FInitialElasticSpecificStorage: TFormulaObject;
    FInitialInelasticSpecificStorage: TFormulaObject;
    FInitialOffset: TFormulaObject;
    FInitialPorosity: TFormulaObject;
    FThickness: TFormulaObject;
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
    function CreateFormulaObject(Orientation:
      TDataSetOrientation): TFormulaObject; virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Interbed: TObject read FInterbed write SetInterbed;
    function IsSame(CSub: TCSubPackageData): Boolean;
    procedure CreateFormulaObjects;
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

  TCSubPackageDataCollection = class(TPhastCollection)
  private
    function GetItem(Index: integer): TCSubPackageData;
    procedure SetItem(Index: integer; const Value: TCSubPackageData);
  public
    constructor Create(Model: TBaseModel);
    property Items[Index: integer]: TCSubPackageData read GetItem write SetItem; default;
    function IsSame(CSub: TCSubPackageDataCollection): Boolean;
    function Add: TCSubPackageData;
  end;

  TCSubRecord = record
    Cell: TCellLocation;
    StressOffset: double;
    StartingTime: double;
    EndingTime: double;
    StressOffsetAnnotation: string;
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
    const
      StressOffsetPosition = 0;
    var
    // See @link(StressOffset).
    FStressOffset: TFormulaObject;
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
   public
     function Add: TCSubItem;
     property Items[Index: Integer]: TCSubItem read GetItem write SetItem; default;
  end;

  TCSubCell = class(TValueCell)
  private
    Values: TCSubRecord;
    StressPeriod: integer;
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
  public
    property StressOffset: double read GetStressOffset;
    property StressOffsetAnnotation: string read GetStressOffsetAnnotation;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
  end;

  TCSubBoundary = class(TSpecificModflowBoundary)
  private
    FCSubPackageData: TCSubPackageDataCollection;
    procedure SetCSubPackageData(const Value: TCSubPackageDataCollection);
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
      AModel: TBaseModel); override;
    procedure InvalidateDisplay; override;
    procedure UpdateTimes(Times: TRealList; StartTestTime, EndTestTime: double;
      var StartRangeExtended, EndRangeExtended: boolean; AModel: TBaseModel); override;
    function Used: boolean; override;
  published
    property CSubPackageData: TCSubPackageDataCollection read FCSubPackageData write SetCSubPackageData;
//    property Interp;
  end;

implementation

uses
  SubscriptionUnit, frmGoPhastUnit, PhastModelUnit, ScreenObjectUnit, GIS_Functions,
  frmErrorsAndWarningsUnit, ModflowTimeUnit, ModflowTimeSeriesUnit, ModflowPackageSelectionUnit;

resourcestring
  StrStressOffsetMultip = 'Stress offset multiplier';
  StrStressOffsetSetTo = 'Stress offset set to zero because of a math error';

{ TCSubPackageData }

procedure TCSubPackageData.Assign(Source: TPersistent);
var
  SubSource: TCSubPackageData;
begin
  if Source is TCSubPackageData then
  begin
    SubSource := TCSubPackageData(Source);
    InitialOffset := SubSource.InitialOffset;
    Thickness := SubSource.Thickness;
    EquivInterbedNumber := SubSource.EquivInterbedNumber;
    InitialInelasticSpecificStorage := SubSource.InitialInelasticSpecificStorage;
    InitialElasticSpecificStorage := SubSource.InitialElasticSpecificStorage;
    InitialPorosity := SubSource.InitialPorosity;
    DelayKv := SubSource.DelayKv;
    InitialDelayHeadOffset := SubSource.InitialDelayHeadOffset;
    InterbedSystemName := SubSource.InterbedSystemName;
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
end;

function TCSubPackageData.CreateFormulaObject(
  Orientation: TDataSetOrientation): TFormulaObject;
begin
  result := frmGoPhast.PhastModel.FormulaManager.Add;
  case Orientation of
    dsoTop:
      begin
        result.Parser := frmGoPhast.PhastModel.rpTopFormulaCompiler;
      end;
    dso3D:
      begin
        result.Parser := frmGoPhast.PhastModel.rpThreeDFormulaCompiler;
      end;
    else Assert(False);
  end;
  Assert(False);
  // This next statement may be wrong.
  result.AddSubscriptionEvents(nil, nil, self);
end;

procedure TCSubPackageData.CreateFormulaObjects;
begin

  FDelayKv := CreateFormulaObject(dso3D);
  FEquivInterbedNumber := CreateFormulaObject(dso3D);
  FInitialDelayHeadOffset := CreateFormulaObject(dso3D);
  FInitialElasticSpecificStorage := CreateFormulaObject(dso3D);
  FInitialInelasticSpecificStorage := CreateFormulaObject(dso3D);
  FInitialOffset := CreateFormulaObject(dso3D);
  FInitialPorosity := CreateFormulaObject(dso3D);
  FThickness := CreateFormulaObject(dso3D);
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
    result := (FInterbed as TInterbed).Name;
  end
  else
  begin
    Result := FInterbedSystemName
  end;
end;

function TCSubPackageData.GetThickness: string;
begin
  result := FThickness.Formula;
end;

function TCSubPackageData.IsSame(CSub: TCSubPackageData): Boolean;
begin
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
    FInterbedSystemName := (FInterbed as TInterbed).Name;
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

constructor TCSubPackageDataCollection.Create(Model: TBaseModel);
var
  OnInvalidateModelEvent: TNotifyEvent;
begin
  if Model = nil then
  begin
    OnInvalidateModelEvent := nil;
  end
  else
  begin
    OnInvalidateModelEvent := OnInvalidateModel;
  end;
  inherited Create(TCSubPackageData, OnInvalidateModelEvent);
end;

function TCSubPackageDataCollection.GetItem(Index: integer): TCSubPackageData;
begin
  result := inherited Items[Index] as TCSubPackageData;
end;

function TCSubPackageDataCollection.IsSame(
  CSub: TCSubPackageDataCollection): Boolean;
var
  ItemIndex: Integer;
begin
  result := Count = CSub.Count;
  if result then
  begin
    for ItemIndex := 0 to Count - 1 do
    begin
      result := Items[ItemIndex].IsSame(CSub.Items[ItemIndex]);
      if not result then
      begin
        Exit;
      end;
    end;
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
end;

procedure TCSubRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(StressOffsetAnnotation);
end;

procedure TCSubRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  StressOffset := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  StressOffsetAnnotation := Annotations[ReadCompInt(Decomp)];
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
end;

procedure TCSubItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TCSubCollection;
  PumpingRateObserver: TObserver;
begin
  ParentCollection := Collection as TCSubCollection;
  PumpingRateObserver := FObserverList[StressOffsetPosition];
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
    StressOffsetPosition: result := StressOffset;
    else Assert(False);
  end;
end;

procedure TCSubItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  Assert(Sender = FStressOffset);
  List.Add(FObserverList[StressOffsetPosition]);
end;

function TCSubItem.GetStressOffset: string;
begin
  Result := FStressOffset.Formula;
  ResetItemObserver(StressOffsetPosition);
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
    StressOffsetPosition: StressOffset := Value;
    else Assert(False);
  end;
end;

procedure TCSubItem.SetStressOffset(const Value: string);
begin
  UpdateFormula(Value, StressOffsetPosition, FStressOffset);
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
  Boundary: TCSubBoundary;
  Item: TCSubItem;
  ScreenObject: TScreenObject;
begin
  result := '';
  if FormulaIndex = 0 then
  begin
    Boundary := BoundaryGroup as TCSubBoundary;
    ScreenObject := Boundary.ScreenObject as TScreenObject;
    Item := Items[ItemIndex] as TCSubItem;
    case Boundary.FormulaInterpretation of
      fiSpecific:
        begin
          if ScreenObject.ScreenObjectLength = 0 then
          begin
            result := Item.StressOffset;
          end
          else if ScreenObject.Closed then
          begin
            result := '(' + Item.StressOffset
              + ') * ' + StrObjectIntersectArea;
          end
          else
          begin
            result := '(' + Item.StressOffset
              + ') * ' + StrObjectSectionIntersectLength;
          end;
        end;
      fiDirect:
        begin
          result := Item.StressOffset;
        end;
      fiTotal:
        begin
          if ScreenObject.ScreenObjectLength = 0 then
          begin
            result := Item.StressOffset;
          end
          else if ScreenObject.Closed then
          begin
            result := '((' + Item.StressOffset
              + ') * ' + StrObjectIntersectArea + ') / ' + StrObjectArea;
          end
          else
          begin
            result := '((' + Item.StressOffset
              + ') * ' + StrObjectSectionIntersectLength+ ') / ' + StrObjectLength;
          end;
        end;
      else Assert(False);
    end;
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TCSubCollection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel);
begin
  inherited;
  Assert(False);
end;

procedure TCSubCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList;
  AModel: TBaseModel; AScreenObject: TObject);
var
  CSubStorage: TCSubStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
  LocalScreenObject: TScreenObject;
begin
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
      end;
    except on E: EMathError do
      begin
        with CSubStorage.CSubArray[Index] do
        begin
          StressOffset := 0;
          StressOffsetAnnotation := StrStressOffsetSetTo;
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

function TCSubCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
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
    Link := TimeListLink.GetLink(PhastModel) as TCSubTimeListLink;
    Link.FStressOffsetData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TCSubTimeListLink;
      Link.FStressOffsetData.Invalidate;
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
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TCSubCell.GetColumn: integer;
begin
  result := Values.Cell.Column;
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
  result := Values.Cell.Layer;
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
  result := Values.Cell.Row;
end;

function TCSubCell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TCSubCell.GetStressOffset: double;
begin
  result := Values.StressOffset;
end;

function TCSubCell.GetStressOffsetAnnotation: string;
begin
  result := Values.StressOffsetAnnotation;
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
      and (Values.Cell = CSubCell.Values.Cell);
  end;
end;

procedure TCSubCell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TCSubCell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TCSubCell.SetColumn(const Value: integer);
begin
  Values.Cell.Column := Value;
end;

procedure TCSubCell.SetLayer(const Value: integer);
begin
  Values.Cell.Layer := Value;
end;

procedure TCSubCell.SetRow(const Value: integer);
begin
  Values.Cell.Row := Value;
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
//    Interp := SourceCSub.Interp;
  end;
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

class function TCSubBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TCSubCollection;
end;

procedure TCSubBoundary.Clear;
begin
  inherited;
  CSubPackageData.Clear;
end;

constructor TCSubBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  FCSubPackageData := TCSubPackageDataCollection.Create(Model);
end;

destructor TCSubBoundary.Destroy;
begin
  FCSubPackageData.Free;
  inherited;
end;

procedure TCSubBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
const
  NoData = 3.0E30;
var
  ValueIndex: Integer;
  BoundaryStorage: TCSubStorage;
  ParamIndex: Integer;
  Param: TModflowParamItem;
  Times: TList;
  Position: integer;
  ParamName: string;
  LocalModel: TCustomModel;
  TimeSeriesList: TTimeSeriesList;
  BoundaryList: TList;
  TimeSeries: TTimeSeries;
  StartTime: Double;
  StressPeriods: TModflowStressPeriods;
  EndTime: Double;
  TimeCount: Integer;
  ItemIndex: Integer;
  SeriesIndex: Integer;
  InitialTime: Double;
begin
  FCurrentParameter := nil;
//  EvaluateArrayBoundaries;
  EvaluateListBoundaries(AModel);
  LocalModel := AModel as TCustomModel;
//  FMaxTabCells := 0;
//  if LocalModel.ModflowPackages.CSubPackage.UseTabFilesInThisModel then
//  begin
//    if (Values.Count > 0) and (Values.BoundaryCount[AModel] > 0) then
//    begin
//      BoundaryStorage := Values.Boundaries[0, AModel] as TCSubStorage;
//      FMaxTabCells := Length(BoundaryStorage.CSubArray);
//      BoundaryStorage.CacheData;
//    end;
//  end;
//  else
//  begin
    for ValueIndex := 0 to Values.Count - 1 do
    begin
      if ValueIndex < Values.BoundaryCount[AModel] then
      begin
        BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TCSubStorage;
        AssignCells(BoundaryStorage, ValueTimeList, AModel);
      end;
    end;
//  end;
  for ParamIndex := 0 to Parameters.Count - 1 do
  begin
    Param := Parameters[ParamIndex];
    ParamName := Param.Param.ParamName;
    if LocalModel.ModelSelection = msModflow2015 then
    begin
      FCurrentParameter := LocalModel.ModflowTransientParameters.GetParamByName(ParamName);
    end
    else
    begin
      FCurrentParameter := nil;
    end;
    Position := ParamList.IndexOf(ParamName);
    if Position < 0 then
    begin
      Times := TObjectList.Create;
      ParamList.AddObject(ParamName, Times);
    end
    else
    begin
      Times := ParamList.Objects[Position] as TList;
    end;

    if FCurrentParameter <> nil then
    begin
      BoundaryList := Param.Param.BoundaryList[AModel];
      StressPeriods := (AModel as TCustomModel).ModflowFullStressPeriods;
      StartTime := StressPeriods.First.StartTime;
      EndTime := StressPeriods.Last.EndTime;
      TimeCount := BoundaryList.Count;
      for ItemIndex := 0 to BoundaryList.Count - 1 do
      begin
        BoundaryStorage := BoundaryList[ItemIndex];
        if BoundaryStorage.StartingTime > StartTime then
        begin
          Inc(TimeCount);
        end;
        StartTime := BoundaryStorage.EndingTime;
      end;
      BoundaryStorage := BoundaryList.Last;
      if BoundaryStorage.EndingTime <= EndTime then
      begin
        Inc(TimeCount);
      end;

      TimeSeriesList := FCurrentParameter.TimeSeriesList;
      TimeSeries := TTimeSeries.Create;
      TimeSeriesList.Add(TimeSeries);
      TimeSeries.SeriesCount := Length(BoundaryStorage.CSubArray);
      TimeSeries.TimeCount := TimeCount;
      TimeSeries.ParameterName := FCurrentParameter.ParameterName;
      TimeSeries.ObjectName := (ScreenObject as TScreenObject).Name;
      for SeriesIndex := 0 to Length(BoundaryStorage.CSubArray) - 1 do
      begin
        TimeSeries.SeriesNames[SeriesIndex] :=
          Format('%0:s_%1d_%2:d', [TimeSeries.ParameterName,
          TimeSeriesList.Count, SeriesIndex+1]);
        TimeSeries.InterpolationMethods[SeriesIndex] := Interp;
        TimeSeries.ScaleFactors[SeriesIndex] := FCurrentParameter.Value;
      end;

      TimeCount := 0;
      StartTime := StressPeriods.First.StartTime;
      InitialTime := StartTime;
      for ItemIndex := 0 to BoundaryList.Count - 1 do
      begin
        BoundaryStorage := BoundaryList[ItemIndex];
        if BoundaryStorage.StartingTime > StartTime then
        begin
          TimeSeries.Times[TimeCount] := StartTime - InitialTime;
          for SeriesIndex := 0 to Length(BoundaryStorage.CSubArray) - 1 do
          begin
            if ItemIndex > 0 then
            begin
              TimeSeries.Values[SeriesIndex,TimeCount] := NoData;
            end
            else
            begin
              TimeSeries.Values[SeriesIndex,TimeCount] :=
                BoundaryStorage.CSubArray[SeriesIndex].StressOffset;
            end;
          end;
          Inc(TimeCount);
        end;
        TimeSeries.Times[TimeCount] := BoundaryStorage.StartingTime - InitialTime;
        for SeriesIndex := 0 to Length(BoundaryStorage.CSubArray) - 1 do
        begin
          TimeSeries.Values[SeriesIndex,TimeCount] :=
            BoundaryStorage.CSubArray[SeriesIndex].StressOffset;
//          BoundaryStorage.CSubArray[SeriesIndex].TimeSeriesName :=
//            TimeSeries.SeriesNames[SeriesIndex];
        end;
        StartTime := BoundaryStorage.EndingTime;
        Inc(TimeCount);
      end;
      BoundaryStorage := BoundaryList.Last;
      if BoundaryStorage.EndingTime <= EndTime then
      begin
        TimeSeries.Times[TimeCount] := EndTime - InitialTime;
        for SeriesIndex := 0 to Length(BoundaryStorage.CSubArray) - 1 do
        begin
          TimeSeries.Values[SeriesIndex,TimeCount] :=
            BoundaryStorage.CSubArray[SeriesIndex].StressOffset;
        end;
      end;
    end;

    for ValueIndex := 0 to Param.Param.Count - 1 do
    begin
      if ValueIndex < Param.Param.BoundaryCount[AModel] then
      begin
        BoundaryStorage := Param.Param.Boundaries[ValueIndex, AModel] as TCSubStorage;
        AssignCells(BoundaryStorage, Times, AModel);
      end;
    end;
  end;
end;

procedure TCSubBoundary.InvalidateDisplay;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    (ParentModel as TPhastModel).InvalidateCSubStressOffset(self);
  end;
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
var
  ItemIndex: Integer;
begin
  result := inherited Used;
  if not result and (CSubPackageData.Count > 0) then
  begin
    for ItemIndex := 0 to CSubPackageData.Count -1 do
    begin
	  result := CSubPackageData[ItemIndex].Used;
	  if result then
	  begin
	    Exit;
	  end;
	end;
  end;
end;

end.
