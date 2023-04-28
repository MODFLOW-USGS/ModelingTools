{This unit is used to define multiple array boundaries in MODFLOW FMP version 4.}
unit ModflowFmp4BoundaryUnit;

interface

uses Windows, ZLib, SysUtils, Classes, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit,
  FormulaManagerUnit, FormulaManagerInterfaceUnit,
  SubscriptionUnit, GoPhastTypes, RbwParser;

type
  {
    @name stores the location, time and reference evapotranspiration rate for a cell.
  }
  TFmp4Record = record
    Cell: TCellLocation;
    FmpValue: double;
    FmpIntValue: Integer;
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

  // @name represents a value in the Farm Process
  // for one time interval.
  // @name is stored by @link(TFmp4Collection).
  TFmp4Item = class(TCustomModflowBoundaryItem)
  private
    // See @link(FmpValue).
    FFmp4Value: IFormulaObject;
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
    // @name is used to compute the values for a series of
    // cells over a series of time intervals.
    FFmp4ValueData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
    property Fmp4ValueData: TModflowTimeList read FFmp4ValueData;
    { TODO -cFMP4 : override GetDescription  in each descendent}
    class function GetDescription: string; virtual; abstract;
    { TODO -cFMP4 : override AssignInvalidateEvent  in each descendent}
    procedure AssignInvalidateEvent; virtual; abstract;
    // Override to assign an integer data type or any data type other than
    // double.
    function GetDefaultDataType: TRbwDataType; virtual;
  public
    property DefaultDataType: TRbwDataType read GetDefaultDataType;
    Destructor Destroy; override;
  end;

  TFmp4TimeListLinkClass = class of TFmp4TimeListLink;

  // @name represents MODFLOW Farm Process values
  // for a series of time intervals.
  TFmp4Collection = class(TCustomMF_ArrayBoundColl)
  private
    procedure InvalidateFmp4Data(Sender: TObject);
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
    // the @link(TFmp4Storage.Fmp4Array) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
  end;

  TFmp4CollectionClass = class of TFmp4Collection;

  // Each @name stores a @link(TFmp4Collection).
  // @classname is stored by @link(TModflowParameters).

  TFmp4_Cell = class(TValueCell)
  private
    FValues: TFmp4Record;
    FStressPeriod: integer;
    function GetFmp4Value: double;
    function GetFmp4Annotation: string;
    function GetFmp4IntValue: Integer;
  protected
    function GetColumn: integer; override;
    function GetLayer: integer; override;
    function GetRow: integer; override;
    procedure SetColumn(const Value: integer); override;
    procedure SetLayer(const Value: integer); override;
    procedure SetRow(const Value: integer); override;
    function GetBooleanValue(Index: integer; AModel: TBaseModel): Boolean; override;
    function GetIntegerValue(Index: integer; AModel: TBaseModel): integer; override;
    function GetRealValue(Index: integer; AModel: TBaseModel): double; override;
    function GetRealAnnotation(Index: integer; AModel: TBaseModel): string; override;
    function GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string; override;
    function GetBooleanAnnotation(Index: integer; AModel: TBaseModel): string; override;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList); override;
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList); override;
    function GetSection: integer; override;
    procedure RecordStrings(Strings: TStringList); override;
  public
    property StressPeriod: integer read FStressPeriod write FStressPeriod;
    property Values: TFmp4Record read FValues write FValues;
    property FmpValue: double read GetFmp4Value;
    property FmpIntValue: Integer read GetFmp4IntValue;
    property FmpValueAnnotation: string read GetFmp4Annotation;
  end;


  // @name represents the MODFLOW Farm Process values associated with
  // a single @link(TScreenObject).
  //
  // @seealso(TFmp4Collection)
  TFmp4Boundary = class(TModflowBoundary)
  private
    FPestValueMethod: TPestParamMethod;
    FPestValueFormula: IFormulaObject;
    FPestValueObserver: TObserver;
    FUsedObserver: TObserver;
    function GetPestValueFormula: string;
    procedure SetPestValueFormula(const Value: string);
    procedure SetPestValueMethod(const Value: TPestParamMethod);
    procedure RemoveFormulaObjects;
  protected
    { TODO -cFMP4 : override GetPestValueObserver in each descendent}
    function GetPestValueObserver: TObserver;
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TFmp4_Cell)s for that stress period.
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
    procedure CreateFormulaObjects; //override;
    procedure CreateObservers; //override;
    function GetPestBoundaryFormula(FormulaIndex: integer): string; override;
    procedure SetPestBoundaryFormula(FormulaIndex: integer;
      const Value: string); override;
    function GetPestBoundaryMethod(FormulaIndex: integer): TPestParamMethod; override;
    procedure SetPestBoundaryMethod(FormulaIndex: integer;
      const Value: TPestParamMethod); override;
    property PestValueObserver: TObserver read GetPestValueObserver;
    function BoundaryObserverPrefix: string; override;
    procedure InvalidateData(Sender: TObject); virtual; abstract;
  public
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    Destructor Destroy; override;
    Procedure Assign(Source: TPersistent); override;
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TFmp4Storage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // ParamList  is not used.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    function Used: boolean; override;
    function NonParameterColumns: integer; override;
    { TODO -cFMP4 : override ValueDescription in each descendent}
    class function ValueDescription: string; virtual; abstract;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
    { TODO -cFMP4 : override InvalidateDisplay in each descendent}
    // be sure to overide this
//    procedure InvalidateDisplay; override;
    function IsSame(FmpBoundary: TFmp4Boundary): Boolean;
  published
    property PestValueFormula: string read GetPestValueFormula
      write SetPestValueFormula;
    property PestValueMethod: TPestParamMethod read FPestValueMethod
      write SetPestValueMethod;
  end;

implementation

uses ScreenObjectUnit, PhastModelUnit, ModflowTimeUnit,
  frmGoPhastUnit,
  AbstractGridUnit;

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
  Assert(Sender = FFmp4Value as TObject);
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
    case Fmp4Array.DataType of
      rdtDouble:
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
      rdtInteger:
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
                      FmpIntValue := Fmp4Array.
                        IntegerData[LayerIndex, RowIndex, ColIndex];
                      FmpValueAnnotation := Fmp4Array.
                        Annotation[LayerIndex, RowIndex, ColIndex];
                    end;
                    Inc(BoundaryIndex);
                  end;
                end;
              end;
            end
          end
        end;
      rdtBoolean:
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
                      FmpIntValue := Ord(Fmp4Array.
                        BooleanData[LayerIndex, RowIndex, ColIndex]);
                      FmpValueAnnotation := Fmp4Array.
                        Annotation[LayerIndex, RowIndex, ColIndex];
                    end;
                    Inc(BoundaryIndex);
                  end;
                end;
              end;
            end
          end
        end;
      rdtString:
        begin
          Assert(False);
        end;
      else
        begin
          Assert(False);
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
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TFmp4TimeListLink;
        Link.FFmp4ValueData.Invalidate;
      end;
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

function TFmp4_Cell.GetBooleanAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    Fmp4Position: result := FmpValueAnnotation;
    else Assert(False);
  end;
end;

function TFmp4_Cell.GetBooleanValue(Index: integer;
  AModel: TBaseModel): Boolean;
begin
  result := False;
  case Index of
    Fmp4Position: result := FmpIntValue <> 0;
    else Assert(False);
  end;
end;

function TFmp4_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TFmp4_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    Fmp4Position: result := FmpValueAnnotation;
    else Assert(False);
  end;
end;

function TFmp4_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  case Index of
    Fmp4Position: result := FmpIntValue;
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

function TFmp4_Cell.GetFmp4IntValue: Integer;
begin
  result := Values.FmpIntValue;
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

procedure TFmp4Boundary.Assign(Source: TPersistent);
var
  FmpSource: TFmp4Boundary;
begin
  if Source is TFmp4Boundary then
  begin
    FmpSource := TFmp4Boundary(Source);
    PestValueFormula := FmpSource.PestValueFormula;
    PestValueMethod := FmpSource.PestValueMethod;
  end;
  inherited;

end;

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
        Cell.ScreenObject := ScreenObjectI;
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

function TFmp4Boundary.BoundaryObserverPrefix: string;
begin
  result := ValueDescription + '_';
end;

constructor TFmp4Boundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

  PestValueFormula := '';
  PestValueMethod := DefaultBoundaryMethod(0);

end;

procedure TFmp4Boundary.CreateFormulaObjects;
begin
  FPestValueFormula := CreateFormulaObjectBlocks(dsoTop);
end;

procedure TFmp4Boundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(PestValueObserver);
  end;
end;

class function TFmp4Boundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  result := inherited;
end;

destructor TFmp4Boundary.Destroy;
begin
  PestValueFormula := '';
  RemoveFormulaObjects;

  inherited;
end;

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

function TFmp4Boundary.GetPestBoundaryFormula(FormulaIndex: integer): string;
begin
  case FormulaIndex of
    Fmp4Position:
      begin
        result := PestValueFormula;
      end;
    else
      begin
        Assert(False);
      end;
  end;
end;

function TFmp4Boundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    Fmp4Position:
      begin
        result := FPestValueMethod;
      end;
    else
      begin
        result := DefaultBoundaryMethod(FormulaIndex);
        Assert(False);
      end;
  end;
end;

function TFmp4Boundary.GetPestValueFormula: string;
begin
  Result := FPestValueFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(Fmp4Position);
  end;
end;

function TFmp4Boundary.GetPestValueObserver: TObserver;
begin
  if FPestValueObserver = nil then
  begin
    CreateObserver(ValueDescription + '_Pest_', FPestValueObserver, nil);
    FPestValueObserver.OnUpToDateSet := InvalidateData;
  end;
  result := FPestValueObserver;
end;

function TFmp4Boundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver(ValueDescription + '_Used_', FUsedObserver, nil);
  end;
  result := FUsedObserver;
end;

procedure TFmp4Boundary.HandleChangedValue(Observer: TObserver);
begin
  InvalidateDisplay;
end;

function TFmp4Boundary.IsSame(FmpBoundary: TFmp4Boundary): Boolean;
begin
  result := (PestValueFormula = FmpBoundary.PestValueFormula)
    and (PestValueMethod = FmpBoundary.PestValueMethod)
    and (Values.IsSame(FmpBoundary.Values));
end;

function TFmp4Boundary.NonParameterColumns: integer;
begin
  result := inherited NonParameterColumns;
end;

procedure TFmp4Boundary.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FPestValueFormula,
    GlobalRemoveMFBoundarySubscription,
    GlobalRestoreMFBoundarySubscription, self);
end;

procedure TFmp4Boundary.SetPestBoundaryFormula(FormulaIndex: integer;
  const Value: string);
begin
  case FormulaIndex of
    Fmp4Position:
      begin
        PestValueFormula := Value;
      end;
    else
      begin
        inherited;
        Assert(False);
      end;
  end;
end;

procedure TFmp4Boundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
begin
  case FormulaIndex of
    Fmp4Position:
      begin
        FPestValueMethod := Value;
      end;
    else
      begin
        Assert(False);
      end;
  end;
end;

procedure TFmp4Boundary.SetPestValueFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, Fmp4Position, FPestValueFormula);
end;

procedure TFmp4Boundary.SetPestValueMethod(const Value: TPestParamMethod);
begin
  FPestValueMethod := Value;
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
  WriteCompInt(Comp, FmpIntValue);
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
  FmpIntValue := ReadCompInt(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  FmpValueAnnotation := Annotations[ReadCompInt(Decomp)];
end;

{ TFmp4TimeListLink }

procedure TFmp4TimeListLink.CreateTimeLists;
begin
  inherited;
  FFmp4ValueData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FFmp4ValueData.DataType := DefaultDataType;
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

function TFmp4TimeListLink.GetDefaultDataType: TRbwDataType;
begin
  result := rdtDouble;
end;

end.
