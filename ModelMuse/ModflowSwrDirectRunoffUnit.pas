unit ModflowSwrDirectRunoffUnit;

interface

uses
  ZLib, Classes, ModflowCellUnit, ModflowBoundaryUnit, FormulaManagerUnit,
  OrderedCollectionUnit, GoPhastTypes, SysUtils;

type
  TSwrDirectRunoffRecord = record
    Cell: TCellLocation;
    Reach: integer;
    Runoff: double;
    StartingTime: double;
    EndingTime: double;
    ReachAnnotation: string;
    RunoffAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TSwrDirectRunoffArray = array of TSwrDirectRunoffRecord;

  TSwrDirectRunoffStorage = class(TCustomBoundaryStorage)
  private
    FDirectRunoffArray: TSwrDirectRunoffArray;
    function GetDirectRunoffArray: TSwrDirectRunoffArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream;
      Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property DirectRunoffArray: TSwrDirectRunoffArray read GetDirectRunoffArray;
  end;

  TSwrDirectRunoffItem = class(TCustomModflowBoundaryItem)
  private
    FReach: TFormulaObject;
    FRunoff: TFormulaObject;
    procedure SetReach(const Value: string);
    procedure SetRunoff(const Value: string);
    function GetReach: string;
    function GetRunoff: string;
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
    property Runoff: string read GetRunoff write SetRunoff;
    property Reach: string read GetReach write SetReach;
  end;

  TSwrDirectRunoffListLink = class(TTimeListsModelLink)
  private
    FReachData: TModflowTimeList;
    FRunoffData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  TSwrDirectRunoffCollection = class(TCustomMF_ArrayBoundColl)
  private
    procedure InvalidateReach(Sender: TObject);
    procedure InvalidateRunoff(Sender: TObject);
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    // See @link(TCustomListArrayBoundColl.AssignArrayCellValues
    // TCustomListArrayBoundColl.AssignArrayCellValues)
    procedure AssignArrayCellValues(DataSets: TList; ItemIndex: Integer;
      AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList; PestItemNames: TStringListObjectList); override;
    // See @link(TCustomListArrayBoundColl.InitializeTimeLists
    // TCustomListArrayBoundColl.InitializeTimeLists)
    procedure InitializeTimeLists(ListOfTimeLists: TList;
      AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList; PestItemNames: TStringListObjectList); override;
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
  end;

  TSwrDirectRunoff_Cell = class(TValueCell)
  private
    Values: TSwrDirectRunoffRecord;
    StressPeriod: integer;
    function GetReach: integer;
    function GetRunoff: double;
    function GetReachAnnotation: string;
    function GetRunoffAnnotation: string;
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
    property Reach: integer read GetReach;
    property Runoff: double read GetRunoff;
    property RunoffAnnotation: string read GetRunoffAnnotation;
    property ReachAnnotation: string read GetReachAnnotation;
  end;

  TSwrDirectRunoffBoundary = class(TModflowBoundary)
  protected
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
  public
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel); override;
    procedure InvalidateDisplay; override;
  end;

implementation

uses
  PhastModelUnit, SubscriptionUnit, DataSetUnit,
  ModflowTimeUnit, ScreenObjectUnit, RbwParser, frmGoPhastUnit;

resourcestring
  StrReach = 'Reach (DROMAP2D)';
  StrRunoff = 'Runoff (DROVAL2D)';

const
  ReachPosition = 0;
  RunoffPosition = 1;

{ TSwrDirectRunoffRecord }

procedure TSwrDirectRunoffRecord.Cache(Comp: TCompressionStream;
  Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompInt(Comp, Reach);
  WriteCompReal(Comp, Runoff);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Strings.IndexOf(ReachAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(RunoffAnnotation));
end;

procedure TSwrDirectRunoffRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(ReachAnnotation);
  Strings.Add(RunoffAnnotation);
end;

procedure TSwrDirectRunoffRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  Reach := ReadCompInt(Decomp);
  Runoff := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  ReachAnnotation := Annotations[ReadCompInt(Decomp)];
  RunoffAnnotation := Annotations[ReadCompInt(Decomp)];
end;

{ TTSwrDirectRunoffStorage }

procedure TSwrDirectRunoffStorage.Clear;
begin
  SetLength(FDirectRunoffArray, 0);
  FCleared := True;
end;

function TSwrDirectRunoffStorage.GetDirectRunoffArray: TSwrDirectRunoffArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FDirectRunoffArray;
end;

procedure TSwrDirectRunoffStorage.Restore(
  DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FDirectRunoffArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FDirectRunoffArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TSwrDirectRunoffStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FDirectRunoffArray);
    for Index := 0 to Count - 1 do
    begin
      FDirectRunoffArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FDirectRunoffArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

{ TSwrDirectRunoffItem }

procedure TSwrDirectRunoffItem.Assign(Source: TPersistent);
var
  SourceItem: TSwrDirectRunoffItem;
begin
  if Source is TSwrDirectRunoffItem then
  begin
  // if Assign is updated, update IsSame too.
    SourceItem := TSwrDirectRunoffItem(Source);
    Reach := SourceItem.Reach;
    Runoff := SourceItem.Runoff;
  end;
  inherited;
end;

procedure TSwrDirectRunoffItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TSwrDirectRunoffCollection;
  ReachObserver: TObserver;
  RunoffObserver: TObserver;
begin
  ParentCollection := Collection as TSwrDirectRunoffCollection;
  ReachObserver := FObserverList[ReachPosition];
  ReachObserver.OnUpToDateSet := ParentCollection.InvalidateReach;
  RunoffObserver := FObserverList[RunoffPosition];
  RunoffObserver.OnUpToDateSet := ParentCollection.InvalidateRunoff;
end;

function TSwrDirectRunoffItem.BoundaryFormulaCount: integer;
begin
  result := 2;
end;

procedure TSwrDirectRunoffItem.CreateFormulaObjects;
begin
  inherited;
  FReach := CreateFormulaObject(dsoTop);
  FRunoff := CreateFormulaObject(dsoTop);
end;

destructor TSwrDirectRunoffItem.Destroy;
begin
  Reach := '0';
  Runoff := '0';
  inherited;
end;

function TSwrDirectRunoffItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    ReachPosition: result := Reach;
    RunoffPosition: result := Runoff;
    else Assert(False);
  end;
end;

procedure TSwrDirectRunoffItem.GetPropertyObserver(Sender: TObject;
  List: TList);
begin
  if Sender = FReach then
  begin
    List.Add( FObserverList[ReachPosition]);
  end;
  if Sender = FRunoff then
  begin
    List.Add( FObserverList[RunoffPosition]);
  end;
end;

function TSwrDirectRunoffItem.GetReach: string;
begin
  Result := FReach.Formula;
  ResetItemObserver(ReachPosition);
end;

function TSwrDirectRunoffItem.GetRunoff: string;
begin
  Result := FRunoff.Formula;
  ResetItemObserver(RunoffPosition);
end;

function TSwrDirectRunoffItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TSwrDirectRunoffItem;
begin
  result := (AnotherItem is TSwrDirectRunoffItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TSwrDirectRunoffItem(AnotherItem);
    result :=
      (Item.Reach = Reach)
      and (Item.Runoff = Runoff)
  end;
end;

procedure TSwrDirectRunoffItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FReach,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRunoff,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TSwrDirectRunoffItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  inherited;
  case Index of
    ReachPosition: Reach := Value;
    RunoffPosition: Runoff := Value;
    else Assert(False);
  end;

end;

procedure TSwrDirectRunoffItem.SetReach(const Value: string);
begin
  UpdateFormulaBlocks(Value, ReachPosition, FReach);
end;

procedure TSwrDirectRunoffItem.SetRunoff(const Value: string);
begin
  UpdateFormulaBlocks(Value, RunoffPosition, FRunoff);
end;

{ TDirectRunoffListLink }

procedure TSwrDirectRunoffListLink.CreateTimeLists;
var
  LocalModel: TCustomModel;
begin
  inherited;
  FReachData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FReachData.NonParamDescription := StrReach;
  FReachData.ParamDescription := ' ' + LowerCase(StrReach);
  FReachData.DataType := rdtInteger;
  FReachData.Orientation := dsoTop;
  FReachData.Direction := dsoTop;
  AddTimeList(FReachData);

  FRunoffData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FRunoffData.NonParamDescription := StrRunoff;
  FRunoffData.ParamDescription := ' ' + LowerCase(StrRunoff);
  FRunoffData.Orientation := dsoTop;
  FRunoffData.Direction := dsoTop;
  AddTimeList(FRunoffData);

  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    FReachData.OnInvalidate := LocalModel.InvalidateMfSwrDirectRunoffReach;
    FRunoffData.OnInvalidate := LocalModel.InvalidateMfSwrDirectRunoffValue;
  end;
end;

destructor TSwrDirectRunoffListLink.Destroy;
begin
  FReachData.Free;
  FRunoffData.Free;
  inherited;
end;

{ TSwrDirectRunoffCollection }

procedure TSwrDirectRunoffCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  inherited;
  AddBoundary(TSwrDirectRunoffStorage.Create(AModel));
end;

procedure TSwrDirectRunoffCollection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel; PestSeries: TStringList;
  PestMethods: TPestMethodList; PestItemNames: TStringListObjectList);
var
  ReachArray: TDataArray;
  RunoffArray: TDataArray;
  Boundary: TSwrDirectRunoffStorage;
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
  ReachArray := DataSets[ReachPosition];
  RunoffArray := DataSets[RunoffPosition];
  Boundary := Boundaries[ItemIndex, AModel] as TSwrDirectRunoffStorage;
  ReachArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
    LayerMax, RowMax, ColMax);
  if LayerMin >= 0 then
  begin
    for LayerIndex := 0 to ReachArray.LayerCount - 1 do
    begin
      if LocalModel.IsLayerSimulated(LayerIndex) then
      begin
        for RowIndex := 0 to ReachArray.RowCount - 1 do
        begin
          for ColIndex := 0 to ReachArray.ColumnCount - 1 do
          begin
            if ReachArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              Assert(RunoffArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              with Boundary.DirectRunoffArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                Reach := ReachArray.
                  IntegerData[LayerIndex, RowIndex, ColIndex];
                ReachAnnotation := ReachArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
                Runoff := RunoffArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                RunoffAnnotation := RunoffArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  ReachArray.CacheData;
  RunoffArray.CacheData;
  Boundary.CacheData;
end;

function TSwrDirectRunoffCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  Result := TSwrDirectRunoffListLink;
end;

procedure TSwrDirectRunoffCollection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList; PestItemNames: TStringListObjectList);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TSwrDirectRunoffItem;
  Boundary: TSwrDirectRunoffBoundary;
  ScreenObject: TScreenObject;
  ALink: TSwrDirectRunoffListLink;
  ReachData: TModflowTimeList;
  RunoffData: TModflowTimeList;
begin
  Boundary := BoundaryGroup as TSwrDirectRunoffBoundary;
  ScreenObject := Boundary.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TSwrDirectRunoffItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.Reach;
  end;
  ALink := TimeListLink.GetLink(AModel) as TSwrDirectRunoffListLink;
  ReachData := ALink.FReachData;
  ReachData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(ReachData.Count = Count);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TSwrDirectRunoffItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.Runoff;
  end;
  RunoffData := ALink.FRunoffData;
  RunoffData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(RunoffData.Count = Count);

  ClearBoundaries(AModel);
  SetBoundaryCapacity(ReachData.Count, AModel);
  for TimeIndex := 0 to ReachData.Count - 1 do
  begin
    AddBoundary(TSwrDirectRunoffStorage.Create(AModel));
  end;
  ListOfTimeLists.Add(ReachData);
  ListOfTimeLists.Add(RunoffData);
end;

procedure TSwrDirectRunoffCollection.InvalidateReach(Sender: TObject);
var
  LocalModel: TCustomModel;
  Link: TSwrDirectRunoffListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  PhastModel: TPhastModel;
begin
  if (not (Sender as TObserver).UpToDate) and (Model <> nil) then
  begin
    LocalModel := Model as TCustomModel;
    if LocalModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(LocalModel) as TSwrDirectRunoffListLink;
    Link.FReachData.Invalidate;
    if LocalModel is TPhastModel then
    begin
      PhastModel := TPhastModel(LocalModel);
      for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
      begin
        ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
        Link := TimeListLink.GetLink(ChildModel) as TSwrDirectRunoffListLink;
        Link.FReachData.Invalidate;
      end;
    end;
  end;
end;

procedure TSwrDirectRunoffCollection.InvalidateRunoff(Sender: TObject);
var
  LocalModel: TCustomModel;
  Link: TSwrDirectRunoffListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  PhastModel: TPhastModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    if Model <> nil then
    begin
      LocalModel := Model as TCustomModel;
    if LocalModel.Clearing then
    begin
      Exit;
    end;
      Link := TimeListLink.GetLink(LocalModel) as TSwrDirectRunoffListLink;
      Link.FRunoffData.Invalidate;
      if LocalModel is TPhastModel then
      begin
        PhastModel := TPhastModel(LocalModel);
        for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
        begin
          ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
          Link := TimeListLink.GetLink(ChildModel) as TSwrDirectRunoffListLink;
          Link.FRunoffData.Invalidate;
        end;
      end;
    end;
  end;
end;

class function TSwrDirectRunoffCollection.ItemClass: TBoundaryItemClass;
begin
  result := TSwrDirectRunoffItem
end;

procedure TSwrDirectRunoffCollection.SetBoundaryStartAndEndTime(
  BoundaryCount: Integer; Item: TCustomModflowBoundaryItem; ItemIndex: Integer;
  AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TSwrDirectRunoffStorage).FDirectRunoffArray,
    BoundaryCount);
  inherited;
end;

{ TSwrDirectRunoff_Cell }

procedure TSwrDirectRunoff_Cell.Cache(Comp: TCompressionStream;
  Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TSwrDirectRunoff_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TSwrDirectRunoff_Cell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    ReachPosition: result := ReachAnnotation;
    else Assert(False);
  end;
end;

function TSwrDirectRunoff_Cell.GetIntegerValue(Index: integer;
  AModel: TBaseModel): integer;
begin
  result := 0;
  case Index of
    ReachPosition: result := Reach;
    else Assert(False);
  end;
end;

function TSwrDirectRunoff_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TSwrDirectRunoff_Cell.GetReach: integer;
begin
  result := Values.Reach;
end;

function TSwrDirectRunoff_Cell.GetReachAnnotation: string;
begin
  result := Values.ReachAnnotation;
end;

function TSwrDirectRunoff_Cell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    ReachPosition: result := ReachAnnotation;
    RunoffPosition: result := RunoffAnnotation;
    else Assert(False);
  end;
end;

function TSwrDirectRunoff_Cell.GetRealValue(Index: integer;
  AModel: TBaseModel): double;
begin
  result := 0.;
  case Index of
    ReachPosition: result := Reach;
    RunoffPosition: result := Runoff;
    else Assert(False);
  end;
end;

function TSwrDirectRunoff_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TSwrDirectRunoff_Cell.GetRunoff: double;
begin
  result := Values.Runoff;
end;

function TSwrDirectRunoff_Cell.GetRunoffAnnotation: string;
begin
  result := Values.RunoffAnnotation;
end;

function TSwrDirectRunoff_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

procedure TSwrDirectRunoff_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TSwrDirectRunoff_Cell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TSwrDirectRunoff_Cell.SetColumn(const Value: integer);
begin
  Values.Cell.Column := Value;
end;

procedure TSwrDirectRunoff_Cell.SetLayer(const Value: integer);
begin
  Values.Cell.Layer := Value;
end;

procedure TSwrDirectRunoff_Cell.SetRow(const Value: integer);
begin
  Values.Cell.Row := Value;
end;

{ TSwrDirectRunoffBoundary }

procedure TSwrDirectRunoffBoundary.AssignCells(
  BoundaryStorage: TCustomBoundaryStorage; ValueTimeList: TList;
  AModel: TBaseModel);
var
  Cell: TSwrDirectRunoff_Cell;
  BoundaryValues: TSwrDirectRunoffRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TSwrDirectRunoffStorage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TSwrDirectRunoffStorage;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TSwrDirectRunoff_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.DirectRunoffArray) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.DirectRunoffArray)
      end;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.DirectRunoffArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.DirectRunoffArray[BoundaryIndex];
        Cell := TSwrDirectRunoff_Cell.Create;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
        Cell.ScreenObject := ScreenObject;
//        Cell.Values.Layer := 0;
//        LocalModel.AdjustCellPosition(Cell);
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TSwrDirectRunoffBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  Result := TSwrDirectRunoffCollection;
end;

procedure TSwrDirectRunoffBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TSwrDirectRunoffStorage;
//  LocalModel: TPhastModel;
begin
  EvaluateArrayBoundaries(AModel);
//  LocalModel := ParentModel as TPhastModel;
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TSwrDirectRunoffStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
  ClearBoundaries(AModel);
end;

procedure TSwrDirectRunoffBoundary.InvalidateDisplay;
var
  LocalModel: TPhastModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    LocalModel := ParentModel as TPhastModel;
    LocalModel.InvalidateMfSwrDirectRunoffReach(self);
    LocalModel.InvalidateMfSwrDirectRunoffValue(self);
  end;
end;

end.
