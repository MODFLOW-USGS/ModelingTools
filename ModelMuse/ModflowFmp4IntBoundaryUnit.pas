{This unit is used to define multiple array boundaries in MODFLOW FMP version 4.}
unit ModflowFmp4IntBoundaryUnit;

interface

uses Windows, ZLib, SysUtils, Classes, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, GoPhastTypes;

type
  {
    @name stores the location, time and reference evapotranspiration rate for a cell.
  }
  TFmp4IntRecord = record
    Cell: TCellLocation;
    FmpIntValue: Integer;
    StartingTime: double;
    EndingTime: double;
    FmpIntValueAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  // @name is an array of @link(TFmp4IntRecord)s.
  TFmp4IntArray = array of TFmp4IntRecord;

  // @name extends @link(TCustomBoundaryStorage) by adding the locations
  // and values of series of reference evapotranspiration cells.
  TFmp4IntStorage = class(TCustomBoundaryStorage)
  private
    FFmp4IntArray: TFmp4IntArray;
    function GetFmp4IntArray: TFmp4IntArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream;
      Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property Fmp4IntArray: TFmp4IntArray read GetFmp4IntArray;
  end;

  // @name represents a MODFLOW reference evapotranspiration in the Farm Process
  // for one time interval.
  // @name is stored by @link(TFmp4IntCollection).
  TFmp4IntItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(FmpIntValue).
    FFmp4IntValue: TFormulaObject;
    // See @link(FmpIntValue).
    procedure SetFmp4IntValue(const Value: string);
    function GetFmp4IntValue: string;
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
    property FmpIntValue: string read GetFmp4IntValue write SetFmp4IntValue;
  end;

  TFmp4IntTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the recharge rates for a series of
    // cells over a series of time intervals.
    FFmp4IntValueData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
    property Fmp4IntValueData: TModflowTimeList read FFmp4IntValueData;
    { TODO -cFMP4 : override GetDescription  in each descendent}
    class function GetDescription: string; virtual; abstract;
    { TODO -cFMP4 : override AssignInvalidateEvent  in each descendent}
    procedure AssignInvalidateEvent; virtual; abstract;
  public
    Destructor Destroy; override;
  end;

  TFmp4TimeListLinkClass = class of TFmp4IntTimeListLink;

  // @name represents MODFLOW Farm Process reference evapotranspiration boundaries
  // for a series of time intervals.
  TFmp4IntCollection = class(TCustomMF_ArrayBoundColl)
  private
    procedure InvalidateFmp4IntData(Sender: TObject);
  protected
    function PackageAssignmentMethod(AModel: TBaseModel): TUpdateMethod; virtual;
    { TODO -cFMP4 : override GetTimeListLinkClass  in each descendent}
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
    // the @link(TFmp4IntStorage.Fmp4IntArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
  end;

  TFmp4IntCollectionClass = class of TFmp4IntCollection;

  // Each @name stores a @link(TFmp4IntCollection).
  // @classname is stored by @link(TModflowParameters).

  TFmp4Int_Cell = class(TValueCell)
  private
    FValues: TFmp4IntRecord;
    FStressPeriod: integer;
    function GetFmp4IntValue: Integer;
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
    property Values: TFmp4IntRecord read FValues write FValues;
    property FmpIntValue: Integer read GetFmp4IntValue;
    property FmpIntValueAnnotation: string read GetFmp4Annotation;
  end;


  // @name represents the MODFLOW Farm Process reference evapotranspiration
  // boundaries associated with
  // a single @link(TScreenObject).
  //
  // @seealso(TFmp4IntCollection)
  TFmp4IntBoundary = class(TModflowBoundary)
  private
    FUsedObserver: TObserver;
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TFmp4Int_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    { TODO -cFMP4 : override BoundaryCollectionClass in each descendent}
    // override this
//    class function BoundaryCollectionClass: TMF_BoundCollClass; override;

    procedure HandleChangedValue(Observer: TObserver); //override;
    { TODO -cFMP4 : override GetUsedObserver in each descendent}
    function GetUsedObserver: TObserver;
    function BoundaryObserverPrefix: string; override;
    procedure InvalidateData(Sender: TObject); virtual; abstract;
  public
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
//    Destructor Destroy; override;
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TFmp4IntStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // ParamList  is not used.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    function Used: boolean; override;
    function NonParameterColumns: integer; override;
    { TODO -cFMP4 : override ValueDescription in each descendent}
    class function ValueDescription: string; virtual; abstract;
    { TODO -cFMP4 : override InvalidateDisplay in each descendent}
    // be sure to overide this
//    procedure InvalidateDisplay; override;
    function IsSame(FmpBoundary: TFmp4IntBoundary): Boolean;
  end;

implementation

uses RbwParser, ScreenObjectUnit, PhastModelUnit, ModflowTimeUnit,
  frmGoPhastUnit,
  AbstractGridUnit;

const
  Fmp4Position = 0;

{ TFmp4IntItem }

procedure TFmp4IntItem.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if Source is TFmp4IntItem then
  begin
    FmpIntValue := TFmp4IntItem(Source).FmpIntValue;
  end;
  inherited;
end;

procedure TFmp4IntItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TFmp4IntCollection;
  FmpObserver: TObserver;
begin
  ParentCollection := Collection as TFmp4IntCollection;
  FmpObserver := FObserverList[Fmp4Position];
  FmpObserver.OnUpToDateSet := ParentCollection.InvalidateFmp4IntData;
end;

function TFmp4IntItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

procedure TFmp4IntItem.CreateFormulaObjects;
begin
  inherited;
  FFmp4IntValue := CreateFormulaObject(dsoTop);
end;

destructor TFmp4IntItem.Destroy;
begin
  FmpIntValue := '0';
  inherited;
end;

function TFmp4IntItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    Fmp4Position: result := FmpIntValue;
    else Assert(False);
  end;
end;

procedure TFmp4IntItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  Assert(Sender = FFmp4IntValue);
  List.Add(FObserverList[Fmp4Position]);
end;

function TFmp4IntItem.GetFmp4IntValue: string;
begin
  Result := FFmp4IntValue.Formula;
  ResetItemObserver(Fmp4Position);
end;

function TFmp4IntItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TFmp4IntItem;
begin
  result := (AnotherItem is TFmp4IntItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TFmp4IntItem(AnotherItem);
    result := (Item.FmpIntValue = FmpIntValue)
  end;
end;

procedure TFmp4IntItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FFmp4IntValue,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TFmp4IntItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    Fmp4Position: FmpIntValue := Value;
    else Assert(False);
  end;
end;

procedure TFmp4IntItem.SetFmp4IntValue(const Value: string);
begin
  UpdateFormulaBlocks(Value, Fmp4Position, FFmp4IntValue);
end;

{ TFmp4IntCollection }

procedure TFmp4IntCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TFmp4IntStorage.Create(AModel));
end;

procedure TFmp4IntCollection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel; PestSeries: TStringList;
  PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList);
var
  Fmp4IntArray: TDataArray;
  Boundary: TFmp4IntStorage;
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
  Fmp4IntArray := DataSets[Fmp4Position];
  Boundary := Boundaries[ItemIndex, AModel] as TFmp4IntStorage;
  Fmp4IntArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
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
            if Fmp4IntArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              with Boundary.Fmp4IntArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                FmpIntValue := Fmp4IntArray.
                  IntegerData[LayerIndex, RowIndex, ColIndex];
                FmpIntValueAnnotation := Fmp4IntArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  Fmp4IntArray.CacheData;
  Boundary.CacheData;
end;

procedure TFmp4IntCollection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList; Writer: TObject);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TFmp4IntItem;
  ScreenObject: TScreenObject;
  ALink: TFmp4IntTimeListLink;
  Fmp4IntValueData: TModflowTimeList;
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
    Item := Items[Index] as TFmp4IntItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.FmpIntValue;
  end;
  ALink := TimeListLink.GetLink(AModel) as TFmp4IntTimeListLink;
  Fmp4IntValueData := ALink.FFmp4IntValueData;
  Fmp4IntValueData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(Fmp4IntValueData.Count = Count);

  if PackageAssignmentMethod(AModel) = umAdd then
  begin
    Grid := (AModel as TCustomModel).Grid;
    for DataArrayIndex := 0 to Fmp4IntValueData.Count - 1 do
    begin
      DataArray := Fmp4IntValueData[DataArrayIndex] as TTransientRealSparseDataSet;
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
  SetBoundaryCapacity(Fmp4IntValueData.Count, AModel);
  for TimeIndex := 0 to Fmp4IntValueData.Count - 1 do
  begin
    AddBoundary(TFmp4IntStorage.Create(AModel));
  end;
  ListOfTimeLists.Add(Fmp4IntValueData);
end;

procedure TFmp4IntCollection.InvalidateFmp4IntData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TFmp4IntTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TFmp4IntTimeListLink;
    Link.FFmp4IntValueData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TFmp4IntTimeListLink;
        Link.FFmp4IntValueData.Invalidate;
      end;
    end;
  end;
end;

class function TFmp4IntCollection.ItemClass: TBoundaryItemClass;
begin
  result := TFmp4IntItem;
end;

function TFmp4IntCollection.PackageAssignmentMethod(
  AModel: TBaseModel): TUpdateMethod;
begin
  result := umAssign;
end;

procedure TFmp4IntCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TFmp4IntStorage).FFmp4IntArray,
    BoundaryCount);
  inherited;
end;


{ TFmp4Int_Cell }

procedure TFmp4Int_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TFmp4Int_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TFmp4Int_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    Fmp4Position: result := FmpIntValueAnnotation;
    else Assert(False);
  end;
end;

function TFmp4Int_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  case Index of
    Fmp4Position: result := FmpIntValue;
    else Assert(False);
  end;
end;

function TFmp4Int_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TFmp4Int_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TFmp4Int_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    Fmp4Position: result := FmpIntValue;
    else Assert(False);
  end;
end;

function TFmp4Int_Cell.GetFmp4IntValue: Integer;
begin
  result := Values.FmpIntValue;
end;

function TFmp4Int_Cell.GetFmp4Annotation: string;
begin
  result := Values.FmpIntValueAnnotation;
end;

function TFmp4Int_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TFmp4Int_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

procedure TFmp4Int_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TFmp4Int_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TFmp4Int_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TFmp4Int_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TFmp4Int_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TFmp4IntBoundary }

procedure TFmp4IntBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TFmp4Int_Cell;
  BoundaryValues: TFmp4IntRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TFmp4IntStorage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TFmp4IntStorage;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TFmp4Int_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.Fmp4IntArray) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.Fmp4IntArray)
      end;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.Fmp4IntArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.Fmp4IntArray[BoundaryIndex];
        Cell := TFmp4Int_Cell.Create;
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

function TFmp4IntBoundary.BoundaryObserverPrefix: string;
begin
  result := ValueDescription + '_';
end;

constructor TFmp4IntBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  CreateBoundaryObserver;
end;

procedure TFmp4IntBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
var
  ValueIndex: Integer;
  BoundaryStorage: TFmp4IntStorage;
begin
  EvaluateArrayBoundaries(AModel, Writer);
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TFmp4IntStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
  ClearBoundaries(AModel);
end;

function TFmp4IntBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver(ValueDescription + '_Used_', FUsedObserver, nil);
  end;
  result := FUsedObserver;
end;

procedure TFmp4IntBoundary.HandleChangedValue(Observer: TObserver);
begin
  InvalidateDisplay;
end;

function TFmp4IntBoundary.IsSame(FmpBoundary: TFmp4IntBoundary): Boolean;
begin
  result := Values.IsSame(FmpBoundary.Values);
end;

function TFmp4IntBoundary.NonParameterColumns: integer;
begin
  result := inherited NonParameterColumns;
end;

function TFmp4IntBoundary.Used: boolean;
begin
  result := inherited Used;
  if result and (ParentModel <> nil) then
  begin
    result := Values.Used;
  end;
end;

{ TFmp4IntStorage }

procedure TFmp4IntStorage.Clear;
begin
  SetLength(FFmp4IntArray, 0);
  FCleared := True;
end;

procedure TFmp4IntStorage.Store(Compressor: TCompressionStream);
var
  Count: Integer;
  Index: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FFmp4IntArray);
    for Index := 0 to Count - 1 do
    begin
      FFmp4IntArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FFmp4IntArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TFmp4IntStorage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FFmp4IntArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FFmp4IntArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TFmp4IntStorage.GetFmp4IntArray: TFmp4IntArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FFmp4IntArray;
end;

{ TFmp4IntRecord }

procedure TFmp4IntRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompInt(Comp, FmpIntValue);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Strings.IndexOf(FmpIntValueAnnotation));
end;

procedure TFmp4IntRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(FmpIntValueAnnotation);
end;

procedure TFmp4IntRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  FmpIntValue := ReadCompInt(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  FmpIntValueAnnotation := Annotations[ReadCompInt(Decomp)];
end;

{ TFmp4IntTimeListLink }

procedure TFmp4IntTimeListLink.CreateTimeLists;
begin
  inherited;
  FFmp4IntValueData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FFmp4IntValueData.DataType := rdtInteger;
  FFmp4IntValueData.NonParamDescription := GetDescription;
  FFmp4IntValueData.ParamDescription := ' ' + LowerCase(GetDescription);
  AddTimeList(FFmp4IntValueData);
  if Model <> nil then
  begin
    AssignInvalidateEvent;
  end;
end;

destructor TFmp4IntTimeListLink.Destroy;
begin
  FFmp4IntValueData.Free;
  inherited;
end;

end.
