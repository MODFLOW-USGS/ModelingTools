unit ModflowCfpRechargeUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, GoPhastTypes;

type
  TCfpRchFractionRecord = record
    Cell: TCellLocation;
    CfpRechargeFraction: double;
    StartingTime: double;
    EndingTime: double;
    CfpRechargeFractionAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TCfpRchFractionArray = array of TCfpRchFractionRecord;

  TCfpRchFractionStorage = class(TCustomBoundaryStorage)
  private
    FCfpRchFractionArray: TCfpRchFractionArray;
    function GetCfpRchFractionArray: TCfpRchFractionArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream;
      Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property CfpRchFractionArray: TCfpRchFractionArray read GetCfpRchFractionArray;
  end;

  TCfpRchFractionItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(CfpRechargeFraction).
    FCfpRechargeFraction: TFormulaObject;
    // See @link(CfpRechargeFraction).
    procedure SetCfpRechargeFraction(const Value: string);
    function GetCfpRechargeFraction: string;
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
    Destructor Destroy; override;
  published
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    // @name is the formula used to set the recharge rate
    // or the recharge rate multiplier of this boundary.
    property CfpRechargeFraction: string read GetCfpRechargeFraction write SetCfpRechargeFraction;
  end;

  TCfpRchFractionTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the recharge rates for a series of
    // cells over a series of time intervals.
    FCfpRechargeFractionData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
    property CfpRechargeFractionData: TModflowTimeList read FCfpRechargeFractionData;
  public
    Destructor Destroy; override;
  end;

  TCfpRchFractionCollection = class(TCustomMF_ArrayBoundColl)
  private
    procedure InvalidateRechargeData(Sender: TObject);
  protected
    function PackageAssignmentMethod(AModel: TBaseModel): TUpdateMethod; virtual;
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    // See @link(TCustomListArrayBoundColl.AssignArrayCellValues
    // TCustomListArrayBoundColl.AssignArrayCellValues)
    procedure AssignArrayCellValues(DataSets: TList; ItemIndex: Integer;
      AModel: TBaseModel); override;
    // See @link(TCustomListArrayBoundColl.InitializeTimeLists
    // TCustomListArrayBoundColl.InitializeTimeLists)
    procedure InitializeTimeLists(ListOfTimeLists: TList; AModel: TBaseModel); override;
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TCfpRchFractionStorage.CfpRchFractionArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
  end;

  TCfpRchFraction_Cell = class(TValueCell)
  private
    FValues: TCfpRchFractionRecord;
    FStressPeriod: integer;
    function GetCfpRechargeFraction: double;
    function GetCfpRechargeFractionAnnotation: string;
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
    property StressPeriod: integer read FStressPeriod write FStressPeriod;
    property Values: TCfpRchFractionRecord read FValues write FValues;
    property CfpRechargeFraction: double read GetCfpRechargeFraction;
    property CfpRechargeFractionAnnotation: string read GetCfpRechargeFractionAnnotation;
  end;

  // @name is used to specify data set 2 in the CRCH in the
  // Conduit Flow Process.
  TCfpRchFractionBoundary = class(TModflowBoundary)
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TCfpRchFraction_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
  public
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TCfpRchFractionStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW Recharge parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TCfpRchFractionStorage) in @link(TCustomMF_BoundColl.Boundaries
    // Param.Param.Boundaries)
    // Those represent parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel); override;
    procedure InvalidateDisplay; override;
  end;


implementation

uses RbwParser, ScreenObjectUnit, PhastModelUnit, ModflowTimeUnit,
  ModflowTransientListParameterUnit, frmGoPhastUnit, TempFiles,
  AbstractGridUnit;

const
  RechPosition = 0;

  StrRechargeFraction = 'Recharge fraction';
  StrRechargeFractionMulti = ' recharge fraction multiplier';


{ TCfpRchFractionRecord }

procedure TCfpRchFractionRecord.Cache(Comp: TCompressionStream;
  Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, CfpRechargeFraction);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Strings.IndexOf(CfpRechargeFractionAnnotation));
end;

procedure TCfpRchFractionRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(CfpRechargeFractionAnnotation);
end;

procedure TCfpRchFractionRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  CfpRechargeFraction := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  CfpRechargeFractionAnnotation := Annotations[ReadCompInt(Decomp)];
end;

{ TCfpRchFractionStorage }

procedure TCfpRchFractionStorage.Clear;
begin
  SetLength(FCfpRchFractionArray, 0);
  FCleared := True;
end;

function TCfpRchFractionStorage.GetCfpRchFractionArray: TCfpRchFractionArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FCfpRchFractionArray;
end;

procedure TCfpRchFractionStorage.Restore(
  DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FCfpRchFractionArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FCfpRchFractionArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TCfpRchFractionStorage.Store(Compressor: TCompressionStream);
var
  Count: Integer;
  Index: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FCfpRchFractionArray);
    for Index := 0 to Count - 1 do
    begin
      FCfpRchFractionArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FCfpRchFractionArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

{ TCfpRchFractionItem }

procedure TCfpRchFractionItem.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if Source is TCfpRchFractionItem then
  begin
    CfpRechargeFraction := TCfpRchFractionItem(Source).CfpRechargeFraction;
  end;
  inherited;

end;

procedure TCfpRchFractionItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TCfpRchFractionCollection;
  RechObserver: TObserver;
begin
  ParentCollection := Collection as TCfpRchFractionCollection;
  RechObserver := FObserverList[RechPosition];
  RechObserver.OnUpToDateSet := ParentCollection.InvalidateRechargeData;
end;

function TCfpRchFractionItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

procedure TCfpRchFractionItem.CreateFormulaObjects;
begin
  inherited;
  FCfpRechargeFraction := CreateFormulaObject(dso3D);
end;

destructor TCfpRchFractionItem.Destroy;
begin
  CfpRechargeFraction := '0';
  inherited;
end;

function TCfpRchFractionItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    RechPosition: result := CfpRechargeFraction;
    else Assert(False);
  end;
end;

procedure TCfpRchFractionItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  Assert(Sender = FCfpRechargeFraction);
  List.Add(FObserverList[RechPosition]);
end;

function TCfpRchFractionItem.GetCfpRechargeFraction: string;
begin
  Result := FCfpRechargeFraction.Formula;
  ResetItemObserver(RechPosition);
end;

function TCfpRchFractionItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TCfpRchFractionItem;
begin
  result := (AnotherItem is TCfpRchFractionItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TCfpRchFractionItem(AnotherItem);
    result := (Item.CfpRechargeFraction = CfpRechargeFraction)
  end;
end;

procedure TCfpRchFractionItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FCfpRechargeFraction,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TCfpRchFractionItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    RechPosition: CfpRechargeFraction := Value;
    else Assert(False);
  end;
end;

procedure TCfpRchFractionItem.SetCfpRechargeFraction(const Value: string);
begin
  UpdateFormulaBlocks(Value, RechPosition, FCfpRechargeFraction);
end;

{ TCfpRchFractionTimeListLink }

procedure TCfpRchFractionTimeListLink.CreateTimeLists;
begin
  inherited;
  FCfpRechargeFractionData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FCfpRechargeFractionData.NonParamDescription := StrRechargeFraction;
  FCfpRechargeFractionData.ParamDescription := StrRechargeFractionMulti;
  AddTimeList(FCfpRechargeFractionData);
  if Model <> nil then
  begin
    FCfpRechargeFractionData.OnInvalidate := (Model as TCustomModel).InvalidateMfConduitRecharge;
  end;
end;

destructor TCfpRchFractionTimeListLink.Destroy;
begin
  FCfpRechargeFractionData.Free;
  inherited;
end;

{ TCfpRchFractionCollection }

procedure TCfpRchFractionCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TCfpRchFractionStorage.Create(AModel));
end;

procedure TCfpRchFractionCollection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel);
var
  RechargeRateArray: TDataArray;
  Boundary: TCfpRchFractionStorage;
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
  RechargeRateArray := DataSets[RechPosition];
  Boundary := Boundaries[ItemIndex, AModel] as TCfpRchFractionStorage;
  RechargeRateArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
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
            if RechargeRateArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              with Boundary.CfpRchFractionArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                CfpRechargeFraction := RechargeRateArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                CfpRechargeFractionAnnotation := RechargeRateArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  RechargeRateArray.CacheData;
  Boundary.CacheData;
end;

function TCfpRchFractionCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TCfpRchFractionTimeListLink;
end;

procedure TCfpRchFractionCollection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TCfpRchFractionItem;
  ScreenObject: TScreenObject;
  ALink: TCfpRchFractionTimeListLink;
  RechargeRateData: TModflowTimeList;
  DataArrayIndex: Integer;
  DataArray: TTransientRealSparseDataSet;
  Grid: TCustomModelGrid;
  RowIndex: Integer;
  ColIndex: Integer;
  LayerIndex: Integer;
  ShouldRemove: Boolean;
begin
  ScreenObject := BoundaryGroup.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TCfpRchFractionItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.CfpRechargeFraction;
  end;
  ALink := TimeListLink.GetLink(AModel) as TCfpRchFractionTimeListLink;
  RechargeRateData := ALink.FCfpRechargeFractionData;
  RechargeRateData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(RechargeRateData.Count = Count);

  if PackageAssignmentMethod(AModel) = umAdd then
  begin
    Grid := (AModel as TCustomModel).Grid;
    for DataArrayIndex := 0 to RechargeRateData.Count - 1 do
    begin
      DataArray := RechargeRateData[DataArrayIndex] as TTransientRealSparseDataSet;
      for RowIndex := 0 to Grid.RowCount - 1 do
      begin
        for ColIndex := 0 to Grid.ColumnCount - 1 do
        begin
          ShouldRemove := False;
          for LayerIndex := Grid.LayerCount -1 downto 0 do
          begin
            if ShouldRemove then
            begin
              DataArray.RemoveValue(LayerIndex, RowIndex, ColIndex);
            end
            else
            begin
              ShouldRemove := DataArray.IsValue[LayerIndex, RowIndex, ColIndex];
            end;
          end;
        end;
      end;
    end;
  end;

  ClearBoundaries(AModel);
  SetBoundaryCapacity(RechargeRateData.Count, AModel);
  for TimeIndex := 0 to RechargeRateData.Count - 1 do
  begin
    AddBoundary(TCfpRchFractionStorage.Create(AModel));
  end;
  ListOfTimeLists.Add(RechargeRateData);
end;

procedure TCfpRchFractionCollection.InvalidateRechargeData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TCfpRchFractionTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TCfpRchFractionTimeListLink;
    Link.FCfpRechargeFractionData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TCfpRchFractionTimeListLink;
      Link.FCfpRechargeFractionData.Invalidate;
    end;
  end;
end;

class function TCfpRchFractionCollection.ItemClass: TBoundaryItemClass;
begin
  result := TCfpRchFractionItem;
end;

function TCfpRchFractionCollection.PackageAssignmentMethod(
  AModel: TBaseModel): TUpdateMethod;
begin
  result := umAssign;
end;

procedure TCfpRchFractionCollection.SetBoundaryStartAndEndTime(
  BoundaryCount: Integer; Item: TCustomModflowBoundaryItem; ItemIndex: Integer;
  AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TCfpRchFractionStorage).FCfpRchFractionArray, BoundaryCount);
  inherited;
end;

{ TCfpRchFraction_Cell }

procedure TCfpRchFraction_Cell.Cache(Comp: TCompressionStream;
  Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TCfpRchFraction_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TCfpRchFraction_Cell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TCfpRchFraction_Cell.GetIntegerValue(Index: integer;
  AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TCfpRchFraction_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TCfpRchFraction_Cell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    RechPosition: result := CfpRechargeFractionAnnotation;
    else Assert(False);
  end;
end;

function TCfpRchFraction_Cell.GetRealValue(Index: integer;
  AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    RechPosition: result := CfpRechargeFraction;
    else Assert(False);
  end;
end;

function TCfpRchFraction_Cell.GetCfpRechargeFraction: double;
begin
  result := Values.CfpRechargeFraction;
end;

function TCfpRchFraction_Cell.GetCfpRechargeFractionAnnotation: string;
begin
  result := Values.CfpRechargeFractionAnnotation;
end;

function TCfpRchFraction_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TCfpRchFraction_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

procedure TCfpRchFraction_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TCfpRchFraction_Cell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TCfpRchFraction_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TCfpRchFraction_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TCfpRchFraction_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TCfpRchFractionBoundary }

procedure TCfpRchFractionBoundary.AssignCells(
  BoundaryStorage: TCustomBoundaryStorage; ValueTimeList: TList;
  AModel: TBaseModel);
var
  Cell: TCfpRchFraction_Cell;
  BoundaryValues: TCfpRchFractionRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TCfpRchFractionStorage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TCfpRchFractionStorage;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TCfpRchFraction_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.CfpRchFractionArray) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.CfpRchFractionArray)
      end;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.CfpRchFractionArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.CfpRchFractionArray[BoundaryIndex];
        Cell := TCfpRchFraction_Cell.Create;
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

class function TCfpRchFractionBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TCfpRchFractionCollection;
end;

procedure TCfpRchFractionBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TCfpRchFractionStorage;
//  ParamIndex: Integer;
//  Param: TModflowParamItem;
//  Times: TList;
//  Position: integer;
//  ParamName: string;
//  Model: TCustomModel;
begin
  EvaluateArrayBoundaries(AModel);
//  Model := ParentModel as TCustomModel;
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TCfpRchFractionStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
  ClearBoundaries(AModel);

end;

procedure TCfpRchFractionBoundary.InvalidateDisplay;
var
  Model: TCustomModel;
begin
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TCustomModel;
    Model.InvalidateMfConduitRecharge(self);
  end;

end;

end.
