unit ModflowResUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, ModflowCellUnit, DataSetUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, GoPhastTypes;

type
  TResRecord = record
    Cell: TCellLocation;
    ResID: integer;
    StartingTime: double;
    EndingTime: double;
    ResIDAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TResArray = array of TResRecord;

  TResStorage = class(TCustomBoundaryStorage)
  private
    FResArray: TResArray;
    function GetResArray: TResArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property ResArray: TResArray read GetResArray;
  end;

  // @name represents a MODFLOW reservoir boundary for one time interval.
  // @name is stored by @link(TResCollection).
  TResItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(StartHead).
    FStartHead: TFormulaObject;
    // See @link(EndHead).
    FEndHead: TFormulaObject;
    // See @link(StartHead).
    procedure SetStartHead(const Value: string);
    // See @link(EndHead).
    procedure SetEndHead(const Value: string);
    function GetEndHead: string;
    function GetStartHead: string;
  protected
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
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    // @name is the formula used to set the elevation
    // of this boundary.
    property StartHead: string read GetStartHead write SetStartHead;
    // @name is the formula used to set the EndHead
    // or the EndHead multiplier of this boundary.
    property EndHead: string read GetEndHead write SetEndHead;
  end;

  TRes_Cell = class(TValueCell)
  private
    Values: TResRecord;
    StressPeriod: integer;
    function GetResID: integer;
    function GetResIdAnnotation: string;
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
    property ResID: integer read GetResID;
    property ResIdAnnotation: string read GetResIdAnnotation;
  end;

  TResTimeListLink = class(TTimeListsModelLink)
  private
    FResIDData: TModflowTimeList;
    FEndHeadData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  // @name represents MODFLOW Reservoir boundaries
  // for a series of time intervals.
  TResCollection = class(TCustomMF_ArrayBoundColl)
  private
    procedure InvalidateStartHeadData(Sender: TObject);
    procedure InvalidateEndHeadData(Sender: TObject);
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

  TResBoundary = class(TModflowBoundary)
  private
    FResId: integer;
    procedure SetResId(const Value: integer);
  protected
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
  public
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel); override;
    property ResId: integer read FResId write SetResId;
  end;

implementation

uses RbwParser, ScreenObjectUnit, ModflowTimeUnit, PhastModelUnit, TempFiles, 
  frmGoPhastUnit;

resourcestring
  StrStartingStage = 'Starting head (Ststage)';
  StrEndingStage = 'Ending head (Endstage)';

const
  StartPosition = 0;
  EndPosition = 1;

{ TResItem }

procedure TResItem.Assign(Source: TPersistent);
var
  Res: TResItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TResItem then
  begin
    Res := TResItem(Source);
    StartHead := Res.StartHead;
    EndHead := Res.EndHead;
  end;
  inherited;
end;

procedure TResItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TResCollection;
  StartObserver: TObserver;
  EndObserver: TObserver;
begin
  ParentCollection := Collection as TResCollection;
  StartObserver := FObserverList[StartPosition];
  StartObserver.OnUpToDateSet := ParentCollection.InvalidateStartHeadData;
  EndObserver := FObserverList[EndPosition];
  EndObserver.OnUpToDateSet := ParentCollection.InvalidateEndHeadData;
end;

function TResItem.BoundaryFormulaCount: integer;
begin
  result := 2;
end;

procedure TResItem.CreateFormulaObjects;
begin
  inherited;
  FStartHead := CreateFormulaObject(dso3D);
  FEndHead := CreateFormulaObject(dso3D);
end;

destructor TResItem.Destroy;
begin
  StartHead := '0';
  EndHead := '0';
  inherited;
end;

function TResItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    StartPosition: result := StartHead;
    EndPosition: result := EndHead;
    else Assert(False);
  end;
end;

function TResItem.GetEndHead: string;
begin
  Result := FEndHead.Formula;
  ResetItemObserver(EndPosition);
end;

procedure TResItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FStartHead then
  begin
    List.Add(FObserverList[StartPosition]);
  end;
  if Sender = FEndHead then
  begin
    List.Add(FObserverList[EndPosition]);
  end;
end;

function TResItem.GetStartHead: string;
begin
  Result := FStartHead.Formula;
  ResetItemObserver(StartPosition);
end;

function TResItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TResItem;
begin
  result := (AnotherItem is TResItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TResItem(AnotherItem);
    result := (Item.StartHead = StartHead)
      and (Item.EndHead = EndHead);
  end;
end;

procedure TResItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FEndHead,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FStartHead,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TResItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    StartPosition: StartHead := Value;
    EndPosition: EndHead := Value;
    else Assert(False);
  end;
end;

procedure TResItem.SetEndHead(const Value: string);
begin
  UpdateFormulaBlocks(Value, EndPosition, FEndHead);
end;

procedure TResItem.SetStartHead(const Value: string);
begin
  UpdateFormulaBlocks(Value, StartPosition, FStartHead);
end;

{ TResCollection }

procedure TResCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TResStorage.Create(AModel));
end;

procedure TResCollection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel);
var
  ResIDArray: TDataArray;
  Boundary: TResStorage;
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
  ResIDArray := DataSets[0];
  Boundary := Boundaries[ItemIndex, AModel] as TResStorage;
  ResIDArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
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
            if ResIDArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              with Boundary.ResArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                ResID := ResIDArray.
                  IntegerData[LayerIndex, RowIndex, ColIndex];
                ResIDAnnotation := ResIDArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  ResIDArray.CacheData;
  Boundary.CacheData;
end;

function TResCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TResTimeListLink;
end;

procedure TResCollection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TResItem;
  Boundary: TResBoundary;
  ScreenObject: TScreenObject;
  ALink: TResTimeListLink;
  ResIDData: TModflowTimeList;
begin
  Boundary := BoundaryGroup as TResBoundary;
  ScreenObject := Boundary.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TResItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := IntToStr(Boundary.ResID);
  end;
  ALink := TimeListLink.GetLink(AModel) as TResTimeListLink;
  ResIDData := ALink.FResIDData;
  ResIDData.Initialize(BoundaryValues, ScreenObject, lctIgnore);

  Assert(ResIDData.Count = Count);
  ClearBoundaries(AModel);
  SetBoundaryCapacity(ResIDData.Count, AModel);
  for TimeIndex := 0 to ResIDData.Count - 1 do
  begin
    AddBoundary(TResStorage.Create(AModel));
  end;
  ListOfTimeLists.Add(ResIDData);
end;

procedure TResCollection.InvalidateEndHeadData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TResTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TResTimeListLink;
    Link.FEndHeadData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TResTimeListLink;
      Link.FEndHeadData.Invalidate;
    end;
  end;
//  if not (Sender as TObserver).UpToDate then
//  begin
//    FEndHeadData.Invalidate;
//  end;
end;

procedure TResCollection.InvalidateStartHeadData(Sender: TObject);
begin
  // do nothing?
end;

class function TResCollection.ItemClass: TBoundaryItemClass;
begin
  result := TResItem;
end;

procedure TResCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TResStorage).FResArray, BoundaryCount);
  inherited;
end;

{ TResBoundary }

procedure TResBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TRes_Cell;
  BoundaryValues: TResRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TResStorage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TResStorage;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TRes_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.ResArray) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.ResArray)
      end;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.ResArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.ResArray[BoundaryIndex];
        Cell := TRes_Cell.Create;
        Assert(ScreenObject <> nil);
        Cell.IFace := (ScreenObject as TScreenObject).IFace;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
        LocalModel.AdjustCellPosition(Cell);
        Cell.ScreenObject := ScreenObject;
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TResBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TResCollection;
end;

procedure TResBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TResStorage;
begin
  EvaluateArrayBoundaries(AModel);
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TResStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
end;

procedure TResBoundary.SetResId(const Value: integer);
begin
  if FResId <> Value then
  begin
    FResId := Value;
    InvalidateModel;
  end;
end;

{ TRes_Cell }

procedure TRes_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TRes_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TRes_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    0: result := ResIDAnnotation;
    else Assert(False);
  end;
end;

function TRes_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := -1;
  case Index of
    0: result := ResID;
    else Assert(False);
  end;
end;

function TRes_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TRes_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TRes_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  Assert(False);
end;

function TRes_Cell.GetResID: integer;
begin
  result := Values.ResID;
end;

function TRes_Cell.GetResIdAnnotation: string;
begin
  result := Values.ResIDAnnotation;
end;

function TRes_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TRes_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

procedure TRes_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TRes_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TRes_Cell.SetColumn(const Value: integer);
begin
  Values.Cell.Column := Value;
end;

procedure TRes_Cell.SetLayer(const Value: integer);
begin
  Values.Cell.Layer := Value;
end;

procedure TRes_Cell.SetRow(const Value: integer);
begin
  Values.Cell.Row := Value;
end;

{ TResRecord }

procedure TResRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompInt(Comp, ResID);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Strings.IndexOf(ResIDAnnotation));
//  WriteCompString(Comp, ResIDAnnotation);
end;

procedure TResRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(ResIDAnnotation);
end;

procedure TResRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  ResID := ReadCompInt(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  ResIDAnnotation := Annotations[ReadCompInt(Decomp)];
//  ResIDAnnotation := ReadCompString(Decomp, Annotations);
end;

{ TResStorage }

procedure TResStorage.Clear;
begin
  SetLength(FResArray, 0);
  FCleared := True;
end;

procedure TResStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FResArray);
    for Index := 0 to Count - 1 do
    begin
      FResArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FResArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TResStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Count: Integer;
  Index: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FResArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FResArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TResStorage.GetResArray: TResArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FResArray;
end;

{ TRchTimeListLink }

procedure TResTimeListLink.CreateTimeLists;
begin
  inherited;
  FResIDData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FEndHeadData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FResIDData.NonParamDescription := StrStartingStage;
  FResIDData.ParamDescription := ' ' + LowerCase(StrStartingStage);
  FEndHeadData.NonParamDescription := StrEndingStage;
  FEndHeadData.ParamDescription := ' ' + LowerCase(StrEndingStage);
  FResIDData.DataType := rdtInteger;
  AddTimeList(FResIDData);
  AddTimeList(FEndHeadData);
end;

destructor TResTimeListLink.Destroy;
begin
  FResIDData.Free;
  FEndHeadData.Free;
  inherited;
end;

end.
