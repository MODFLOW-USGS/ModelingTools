unit ModflowCsubUnit;

interface

uses
  GoPhastTypes, System.Classes, ModflowCellUnit, System.ZLib,
  ModflowBoundaryUnit, FormulaManagerUnit, OrderedCollectionUnit, RbwParser,
  RealListUnit, System.SysUtils, System.Contnrs;

type
  TCSubPackageData = class(TPhastCollectionItem)
  private
    FStoredInitialElasticSpecificStorage: TRealStorage;
    FStoredInitialInelasticSpecificStorage: TRealStorage;
    FStoredThickness: TRealStorage;
    FStoredInitialDelayHeadOffset: TRealStorage;
    FStoredInitialPorosity: TRealStorage;
    FStoredDelayKv: TRealStorage;
    FStoredInitialOffset: TRealStorage;
    FInterbedSystemName: string;
    FStoredEquivInterbedNumber: TRealStorage;
    procedure SetStoredDelayKv(const Value: TRealStorage);
    procedure SetStoredEquivInterbedNumber(const Value: TRealStorage);
    procedure SetStoredInitialDelayHeadOffset(const Value: TRealStorage);
    procedure SetStoredInitialElasticSpecificStorage(const Value: TRealStorage);
    procedure SetStoredInitialInelasticSpecificStorage(const Value: TRealStorage);
    procedure SetStoredInitialPorosity(const Value: TRealStorage);
    procedure SetInterbedSystemName(const Value: string);
    procedure SetStoredInitialOffset(const Value: TRealStorage);
    procedure SetStoredThickness(const Value: TRealStorage);
    function GetInterbedSystemName: string;
    function GetInitialOffset: Double;
    procedure SetInitialOffset(const Value: Double);
    function GetDelayKv: Double;
    function GetEquivInterbedNumber: Double;
    function GetInitialDelayHeadOffset: Double;
    function GetInitialElasticSpecificStorage: Double;
    function GetInitialInelasticSpecificStorage: Double;
    function GetInitialPorosity: Double;
    function GetThickness: Double;
    procedure SetDelayKv(const Value: Double);
    procedure SetEquivInterbedNumber(const Value: Double);
    procedure SetInitialDelayHeadOffset(const Value: Double);
    procedure SetInitialElasticSpecificStorage(const Value: Double);
    procedure SetInitialInelasticSpecificStorage(const Value: Double);
    procedure SetInitialPorosity(const Value: Double);
    procedure SetThickness(const Value: Double);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property InitialOffset: Double read GetInitialOffset write SetInitialOffset;
    property Thickness: Double read GetThickness write SetThickness;
    property EquivInterbedNumber: Double read GetEquivInterbedNumber write SetEquivInterbedNumber;
    property InitialInelasticSpecificStorage: Double read GetInitialInelasticSpecificStorage write SetInitialInelasticSpecificStorage;
    property InitialElasticSpecificStorage: Double read GetInitialElasticSpecificStorage write SetInitialElasticSpecificStorage;
    property InitialPorosity: Double read GetInitialPorosity write SetInitialPorosity;
    property DelayKv: Double read GetDelayKv write SetDelayKv;
    property InitialDelayHeadOffset: Double read GetInitialDelayHeadOffset write SetInitialDelayHeadOffset;
  published
    property InterbedSystemName: string read GetInterbedSystemName write SetInterbedSystemName;
    {pcs0—is the initial offset from the calculated initial effective stress or initial preconsolidation
    stress in the interbed, in units of height of a column of water. PCS0 is the initial
    preconsolidation stress if SPECIFIED INITIAL INTERBED STATE or SPECIFIED
    INITIAL PRECONSOLIDATION STRESS are specified in the OPTIONS block. If
    HEAD BASED is specified in the OPTIONS block, PCS0 is the initial offset from the calculated
    initial head or initial preconsolidation head in the CSUB interbed and the initial preconsolidation
    stress is calculated from the calculated initial effective stress or calculated initial geostatic stress,
    respectively.}
    property StoredInitialOffset: TRealStorage read FStoredInitialOffset write SetStoredInitialOffset;
    {thick frac—is the interbed thickness or cell fraction of the interbed. Interbed thickness is specified as
    a fraction of the cell thickness if CELL FRACTION is specified in the OPTIONS block.}
    property StoredThickness: TRealStorage read FStoredThickness write SetStoredThickness;
    {rnb—is the interbed material factor equivalent number of interbeds in the interbed system represented
    by the interbed. RNB must be greater than or equal to 1 if CDELAY is DELAY. Otherwise, RNB
    can be any value.}
    property StoredEquivInterbedNumber: TRealStorage read FStoredEquivInterbedNumber write SetStoredEquivInterbedNumber;
    {ssv cc—is the initial inelastic specific storage or compression index of the interbed. The compression
    index is specified if COMPRESSION INDICES is specified in the OPTIONS block. Specified
    or calculated interbed inelastic specific storage values are not adjusted from initial values if
    HEAD BASED is specified in the OPTIONS block.}
    property StoredInitialInelasticSpecificStorage: TRealStorage read FStoredInitialInelasticSpecificStorage write SetStoredInitialInelasticSpecificStorage;
    //sse cr—is the initial elastic coarse-grained material specific storage or recompression index of the
    //interbed. The recompression index is specified if COMPRESSION INDICES is specified in the
    //OPTIONS block. Specified or calculated interbed elastic specific storage values are not adjusted
    //from initial values if HEAD BASED is specified in the OPTIONS block.
    property StoredInitialElasticSpecificStorage: TRealStorage read FStoredInitialElasticSpecificStorage write SetStoredInitialElasticSpecificStorage;
    //theta—is the initial porosity of the interbed.
    property StoredInitialPorosity: TRealStorage read FStoredInitialPorosity write SetStoredInitialPorosity;
    //kv—is the vertical hydraulic conductivity of the delay interbed. KV must be greater than 0 if CDELAY
    //is DELAY. Otherwise, KV can be any value.
    property StoredDelayKv: TRealStorage read FStoredDelayKv write SetStoredDelayKv;
    //h0—is the initial offset from the head in cell cellid or the initial head in the delay interbed. H0
    //is the initial head in the delay bed if SPECIFIED INITIAL INTERBED STATE or SPECIFIED
    //INITIAL DELAY HEAD are specified in the OPTIONS block. H0 can be any value if CDELAY
    //is NODELAY.
    property StoredInitialDelayHeadOffset: TRealStorage read FStoredInitialDelayHeadOffset write SetStoredInitialDelayHeadOffset;
  end;

  TCSubPackageDataCollection = class(TPhastCollection)
    constructor Create(Model: TBaseModel);
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
    procedure Assign(Source: TPersistent);override;
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
  frmErrorsAndWarningsUnit, ModflowTimeUnit, ModflowTimeSeriesUnit;

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
  FStoredInitialElasticSpecificStorage := TRealStorage.Create;
  FStoredInitialElasticSpecificStorage.OnChange := OnInvalidateModel;

  FStoredInitialInelasticSpecificStorage := TRealStorage.Create;
  FStoredInitialInelasticSpecificStorage.OnChange := OnInvalidateModel;

  FStoredThickness := TRealStorage.Create;
  FStoredThickness.OnChange := OnInvalidateModel;

  FStoredInitialDelayHeadOffset := TRealStorage.Create;
  FStoredInitialDelayHeadOffset.OnChange := OnInvalidateModel;

  FStoredInitialPorosity := TRealStorage.Create;
  FStoredInitialPorosity.OnChange := OnInvalidateModel;

  FStoredDelayKv := TRealStorage.Create;
  FStoredDelayKv.OnChange := OnInvalidateModel;

  FStoredInitialOffset := TRealStorage.Create;
  FStoredInitialOffset.OnChange := OnInvalidateModel;

  FStoredEquivInterbedNumber := TRealStorage.Create;
  FStoredEquivInterbedNumber.OnChange := OnInvalidateModel;
end;

destructor TCSubPackageData.Destroy;
begin
  FStoredEquivInterbedNumber.Free;
  FStoredInitialOffset.Free;
  FStoredDelayKv.Free;
  FStoredInitialPorosity.Free;
  FStoredInitialDelayHeadOffset.Free;
  FStoredThickness.Free;
  FStoredInitialInelasticSpecificStorage.Free;
  FStoredInitialElasticSpecificStorage.Free;

  inherited;
end;

function TCSubPackageData.GetDelayKv: Double;
begin
  Result := StoredDelayKv.Value;
end;

function TCSubPackageData.GetEquivInterbedNumber: Double;
begin
  Result := StoredEquivInterbedNumber.Value;
end;

function TCSubPackageData.GetInitialDelayHeadOffset: Double;
begin
  Result := StoredInitialDelayHeadOffset.Value;
end;

function TCSubPackageData.GetInitialElasticSpecificStorage: Double;
begin
  Result := StoredInitialElasticSpecificStorage.Value;
end;

function TCSubPackageData.GetInitialInelasticSpecificStorage: Double;
begin
  Result := StoredInitialInelasticSpecificStorage.Value;
end;

function TCSubPackageData.GetInitialOffset: Double;
begin
  result := StoredInitialOffset.Value;
end;

function TCSubPackageData.GetInitialPorosity: Double;
begin
  Result := StoredInitialPorosity.Value;
end;

function TCSubPackageData.GetInterbedSystemName: string;
begin
  Result := FInterbedSystemName
end;

function TCSubPackageData.GetThickness: Double;
begin
  Result := StoredThickness.Value;
end;

procedure TCSubPackageData.SetStoredDelayKv(const Value: TRealStorage);
begin
  FStoredDelayKv.Assign(Value);
end;

procedure TCSubPackageData.SetStoredEquivInterbedNumber(const Value: TRealStorage);
begin
  FStoredEquivInterbedNumber.Assign(Value);
end;

procedure TCSubPackageData.SetStoredInitialDelayHeadOffset(const Value: TRealStorage);
begin
  FStoredInitialDelayHeadOffset.Assign(Value);
end;

procedure TCSubPackageData.SetStoredInitialElasticSpecificStorage(
  const Value: TRealStorage);
begin
  FStoredInitialElasticSpecificStorage.Assign(Value);
end;

procedure TCSubPackageData.SetStoredInitialInelasticSpecificStorage(
  const Value: TRealStorage);
begin
  FStoredInitialInelasticSpecificStorage.Assign(Value);
end;

procedure TCSubPackageData.SetDelayKv(const Value: Double);
begin
  StoredDelayKv.Value := Value;
end;

procedure TCSubPackageData.SetEquivInterbedNumber(const Value: Double);
begin
  StoredEquivInterbedNumber.Value := Value;
end;

procedure TCSubPackageData.SetInitialDelayHeadOffset(const Value: Double);
begin
  StoredInitialDelayHeadOffset.Value := Value;
end;

procedure TCSubPackageData.SetInitialElasticSpecificStorage(
  const Value: Double);
begin
  StoredInitialElasticSpecificStorage.Value := Value;
end;

procedure TCSubPackageData.SetInitialInelasticSpecificStorage(
  const Value: Double);
begin
  StoredInitialInelasticSpecificStorage.Value := Value;
end;

procedure TCSubPackageData.SetInitialOffset(const Value: Double);
begin
  StoredInitialOffset.Value := Value;
end;

procedure TCSubPackageData.SetInitialPorosity(const Value: Double);
begin
  StoredInitialPorosity.Value := Value;
end;

procedure TCSubPackageData.SetStoredInitialPorosity(const Value: TRealStorage);
begin
  FStoredInitialPorosity.Assign(Value);
end;

procedure TCSubPackageData.SetInterbedSystemName(const Value: string);
begin
  FInterbedSystemName := Value;
end;

procedure TCSubPackageData.SetStoredInitialOffset(const Value: TRealStorage);
begin
  FStoredInitialOffset.Assign(Value);
end;

procedure TCSubPackageData.SetStoredThickness(const Value: TRealStorage);
begin
  FStoredThickness.Assign(Value);
end;

procedure TCSubPackageData.SetThickness(const Value: Double);
begin
  StoredThickness.Value := Value;
end;

{ TCSubPackageDataCollection }

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
begin
 result := inherited Used or (CSubPackageData.Count > 0);
end;

end.
