{This unit is used to define multiple array boundaries in MODFLOW FMP version 4.}
unit ModflowFmp4BoundaryUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, GoPhastTypes;

type
  {
    @longcode(
  TFmp4Record = record
    Cell: TCellLocation;
    FmpValue: double;
    StartingTime: double;
    EndingTime: double;
    FmpValueAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;
    )
    @name stores the location, time and reference evapotranspiration rate for a cell.
  }
  TFmp4Record = record
    Cell: TCellLocation;
    FmpValue: double;
    StartingTime: double;
    EndingTime: double;
    FmpValueAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  // @name is an array of @link(TFmp4Record)s.
  TFmp4Array = array of TFmp4Record;

  // @name extends @link(TCustomBoundaryStorage) by adding the locations
  // and values of series of reference evapotranspiration cells.
  TFmp4Storage = class(TCustomBoundaryStorage)
  private
    FFmp4Array: TFmp4Array;
    function GetFmp4Array: TFmp4Array;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream;
      Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property Fmp4Array: TFmp4Array read GetFmp4Array;
  end;

  // @name represents a MODFLOW reference evapotranspiration in the Farm Process
  // for one time interval.
  // @name is stored by @link(TFmp4Collection).
  TFmp4Item = class(TCustomModflowBoundaryItem)
  private
    // See @link(FmpValue).
    FFmp4Value: TFormulaObject;
    // See @link(FmpValue).
    procedure SetFmp4Value(const Value: string);
    function GetFmp4Value: string;
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
    property FmpValue: string read GetFmp4Value write SetFmp4Value;
  end;

  TFmp4TimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the recharge rates for a series of
    // cells over a series of time intervals.
    FFmp4ValueData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
    property Fmp4ValueData: TModflowTimeList read FFmp4ValueData;
    { TODO -cFMP4 : override GetDescription }
    class function GetDescription: string; virtual; abstract;
    { TODO -cFMP4 : override AssignInvalidateEvent }
    procedure AssignInvalidateEvent; virtual; abstract;
  public
    Destructor Destroy; override;
  end;

  // @name represents MODFLOW Farm Process reference evapotranspiration boundaries
  // for a series of time intervals.
  TFmp4Collection = class(TCustomMF_ArrayBoundColl)
  private
    procedure InvalidateFmp4Data(Sender: TObject);
  protected
    function PackageAssignmentMethod(AModel: TBaseModel): TUpdateMethod; virtual;
    { TODO -cFMP4 : override GetTimeListLinkClass }
    // override this
//    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    // See @link(TCustomListArrayBoundColl.AssignArrayCellValues
    // TCustomListArrayBoundColl.AssignArrayCellValues)
    procedure AssignArrayCellValues(DataSets: TList; ItemIndex: Integer;
      AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
      PestItemNames, TimeSeriesNames: TStringListObjectList); override;
    // See @link(TCustomListArrayBoundColl.InitializeTimeLists
    // TCustomListArrayBoundColl.InitializeTimeLists)
    procedure InitializeTimeLists(ListOfTimeLists: TList; AModel: TBaseModel;
      PestSeries: TStringList; PestMethods: TPestMethodList;
      PestItemNames, TimeSeriesNames: TStringListObjectList; Writer: TObject); override;
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TFmp4Storage.Fmp4Array) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
  end;

  // Each @name stores a @link(TFmp4Collection).
  // @classname is stored by @link(TModflowParameters).

  TFmp4_Cell = class(TValueCell)
  private
    FValues: TFmp4Record;
    FStressPeriod: integer;
    function GetFmp4Value: double;
    function GetFmp4Annotation: string;
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
    property Values: TFmp4Record read FValues write FValues;
    property FmpValue: double read GetFmp4Value;
    property FmpValueAnnotation: string read GetFmp4Annotation;
  end;


  // @name represents the MODFLOW Farm Process reference evapotranspiration
  // boundaries associated with
  // a single @link(TScreenObject).
  //
  // @seealso(TFmp4Collection)
  TFmp4Boundary = class(TModflowBoundary)
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TFmp4_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    { TODO -cFMP4 : override BoundaryCollectionClass }
    // override this
//    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
  public
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TFmp4Storage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // ParamList  is not used.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    function Used: boolean; override;
    function NonParameterColumns: integer; override;
    // besure to overide this
//    procedure InvalidateDisplay; override;
  end;

implementation

uses RbwParser, ScreenObjectUnit, PhastModelUnit, ModflowTimeUnit,
  ModflowTransientListParameterUnit, frmGoPhastUnit, TempFiles,
  AbstractGridUnit;

//resourcestring
//  StrRefEvapRate = 'Reference evapotranspiration rate';

const
  Fmp4Position = 0;

{ TFmp4Item }

procedure TFmp4Item.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if Source is TFmp4Item then
  begin
    FmpValue := TFmp4Item(Source).FmpValue;
  end;
  inherited;
end;

procedure TFmp4Item.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TFmp4Collection;
  FmpObserver: TObserver;
begin
  ParentCollection := Collection as TFmp4Collection;
  FmpObserver := FObserverList[Fmp4Position];
  FmpObserver.OnUpToDateSet := ParentCollection.InvalidateFmp4Data;
end;

function TFmp4Item.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

procedure TFmp4Item.CreateFormulaObjects;
begin
  inherited;
  FFmp4Value := CreateFormulaObject(dsoTop);
end;

destructor TFmp4Item.Destroy;
begin
  FmpValue := '0';
  inherited;
end;

function TFmp4Item.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    Fmp4Position: result := FmpValue;
    else Assert(False);
  end;
end;

procedure TFmp4Item.GetPropertyObserver(Sender: TObject; List: TList);
begin
  Assert(Sender = FFmp4Value);
  List.Add(FObserverList[Fmp4Position]);
end;

function TFmp4Item.GetFmp4Value: string;
begin
  Result := FFmp4Value.Formula;
  ResetItemObserver(Fmp4Position);
end;

function TFmp4Item.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TFmp4Item;
begin
  result := (AnotherItem is TFmp4Item) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TFmp4Item(AnotherItem);
    result := (Item.FmpValue = FmpValue)
  end;
end;

procedure TFmp4Item.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FFmp4Value,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TFmp4Item.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    Fmp4Position: FmpValue := Value;
    else Assert(False);
  end;
end;

procedure TFmp4Item.SetFmp4Value(const Value: string);
begin
  UpdateFormulaBlocks(Value, Fmp4Position, FFmp4Value);
end;

{ TFmp4Collection }

procedure TFmp4Collection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TFmp4Storage.Create(AModel));
end;

procedure TFmp4Collection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel; PestSeries: TStringList;
  PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList);
var
  Fmp4Array: TDataArray;
  Boundary: TFmp4Storage;
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
  Fmp4Array := DataSets[Fmp4Position];
  Boundary := Boundaries[ItemIndex, AModel] as TFmp4Storage;
  Fmp4Array.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
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
            if Fmp4Array.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              with Boundary.Fmp4Array[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                FmpValue := Fmp4Array.
                  RealData[LayerIndex, RowIndex, ColIndex];
                FmpValueAnnotation := Fmp4Array.
                  Annotation[LayerIndex, RowIndex, ColIndex];
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  Fmp4Array.CacheData;
  Boundary.CacheData;
end;

procedure TFmp4Collection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList; Writer: TObject);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TFmp4Item;
  ScreenObject: TScreenObject;
  ALink: TFmp4TimeListLink;
  Fmp4ValueData: TModflowTimeList;
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
    Item := Items[Index] as TFmp4Item;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.FmpValue;
  end;
  ALink := TimeListLink.GetLink(AModel) as TFmp4TimeListLink;
  Fmp4ValueData := ALink.FFmp4ValueData;
  Fmp4ValueData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(Fmp4ValueData.Count = Count);

  if PackageAssignmentMethod(AModel) = umAdd then
  begin
    Grid := (AModel as TCustomModel).Grid;
    for DataArrayIndex := 0 to Fmp4ValueData.Count - 1 do
    begin
      DataArray := Fmp4ValueData[DataArrayIndex] as TTransientRealSparseDataSet;
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
  SetBoundaryCapacity(Fmp4ValueData.Count, AModel);
  for TimeIndex := 0 to Fmp4ValueData.Count - 1 do
  begin
    AddBoundary(TFmp4Storage.Create(AModel));
  end;
  ListOfTimeLists.Add(Fmp4ValueData);
end;

procedure TFmp4Collection.InvalidateFmp4Data(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TFmp4TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TFmp4TimeListLink;
    Link.FFmp4ValueData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TFmp4TimeListLink;
      Link.FFmp4ValueData.Invalidate;
    end;
  end;
end;

class function TFmp4Collection.ItemClass: TBoundaryItemClass;
begin
  result := TFmp4Item;
end;

function TFmp4Collection.PackageAssignmentMethod(
  AModel: TBaseModel): TUpdateMethod;
begin
  result := umAssign;
end;

procedure TFmp4Collection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TFmp4Storage).FFmp4Array,
    BoundaryCount);
  inherited;
end;


{ TFmp4_Cell }

procedure TFmp4_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TFmp4_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TFmp4_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TFmp4_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  case Index of
    Fmp4Position: result := (AModel as TCustomModel).
      DataSetLayerToModflowLayer(Layer);
    else Assert(False);
  end;
end;

function TFmp4_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TFmp4_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    Fmp4Position: result := FmpValueAnnotation;
    else Assert(False);
  end;
end;

function TFmp4_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    Fmp4Position: result := FmpValue;
    else Assert(False);
  end;
end;

function TFmp4_Cell.GetFmp4Value: double;
begin
  result := Values.FmpValue;
end;

function TFmp4_Cell.GetFmp4Annotation: string;
begin
  result := Values.FmpValueAnnotation;
end;

function TFmp4_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TFmp4_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

procedure TFmp4_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TFmp4_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TFmp4_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TFmp4_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TFmp4_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TFmp4Boundary }

procedure TFmp4Boundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TFmp4_Cell;
  BoundaryValues: TFmp4Record;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TFmp4Storage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TFmp4Storage;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TFmp4_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.Fmp4Array) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.Fmp4Array)
      end;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.Fmp4Array) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.Fmp4Array[BoundaryIndex];
        Cell := TFmp4_Cell.Create;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
        Cell.ScreenObject := ScreenObject;
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

//class function TFmp4Boundary.BoundaryCollectionClass: TMF_BoundCollClass;
//begin
//  result := TFmp4Collection;
//end;

procedure TFmp4Boundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
var
  ValueIndex: Integer;
  BoundaryStorage: TFmp4Storage;
begin
  EvaluateArrayBoundaries(AModel, Writer);
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TFmp4Storage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
  ClearBoundaries(AModel);
end;

//procedure TFmp4Boundary.InvalidateDisplay;
//var
//  Model: TCustomModel;
//begin
//  inherited;
//  if Used and (ParentModel <> nil) then
//  begin
//    Model := ParentModel as TCustomModel;
//    if Model.ModelSelection = msModflowFmp then
//    begin
//      Model.InvalidateMfFmpEvap(self);
//    end
//    else
//    begin
//      Model.InvalidateMfFmp4Evap(self);
//    end;
//  end;
//end;

function TFmp4Boundary.NonParameterColumns: integer;
begin
  result := inherited NonParameterColumns;
end;

function TFmp4Boundary.Used: boolean;
begin
  result := inherited Used;
  if result and (ParentModel <> nil) then
  begin
    result := Values.Used;
  end;
end;

{ TFmp4Storage }

procedure TFmp4Storage.Clear;
begin
  SetLength(FFmp4Array, 0);
  FCleared := True;
end;

procedure TFmp4Storage.Store(Compressor: TCompressionStream);
var
  Count: Integer;
  Index: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FFmp4Array);
    for Index := 0 to Count - 1 do
    begin
      FFmp4Array[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FFmp4Array[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TFmp4Storage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FFmp4Array, Count);
  for Index := 0 to Count - 1 do
  begin
    FFmp4Array[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TFmp4Storage.GetFmp4Array: TFmp4Array;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FFmp4Array;
end;

{ TFmp4Record }

procedure TFmp4Record.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, FmpValue);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Strings.IndexOf(FmpValueAnnotation));
end;

procedure TFmp4Record.RecordStrings(Strings: TStringList);
begin
  Strings.Add(FmpValueAnnotation);
end;

procedure TFmp4Record.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  FmpValue := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  FmpValueAnnotation := Annotations[ReadCompInt(Decomp)];
end;

{ TFmp4TimeListLink }

procedure TFmp4TimeListLink.CreateTimeLists;
begin
  inherited;
  FFmp4ValueData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FFmp4ValueData.NonParamDescription := GetDescription;
  FFmp4ValueData.ParamDescription := ' ' + LowerCase(GetDescription);
  AddTimeList(FFmp4ValueData);
  if Model <> nil then
  begin
    AssignInvalidateEvent;
  end;
end;

destructor TFmp4TimeListLink.Destroy;
begin
  FFmp4ValueData.Free;
  inherited;
end;

end.
