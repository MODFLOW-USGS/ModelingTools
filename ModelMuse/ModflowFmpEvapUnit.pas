unit ModflowFmpEvapUnit;

interface

uses Windows, ZLib, SysUtils, Classes, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit,
  FormulaManagerUnit, FormulaManagerInterfaceUnit,
  SubscriptionUnit, GoPhastTypes;

type
  {
    @longcode(
  TFmpEvapRecord = record
    Cell: TCellLocation;
    RefEvapRate: double;
    StartingTime: double;
    EndingTime: double;
    RefEvapRateAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;
    )
    @name stores the location, time and reference evapotranspiration rate for a cell.
  }
  TFmpEvapRecord = record
    Cell: TCellLocation;
    RefEvapRate: double;
    StartingTime: double;
    EndingTime: double;
    RefEvapRateAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  // @name is an array of @link(TFmpEvapRecord)s.
  TFmpRefEvapArray = array of TFmpEvapRecord;

  // @name extends @link(TCustomBoundaryStorage) by adding the locations
  // and values of series of reference evapotranspiration cells.
  TFmpRefEvapStorage = class(TCustomBoundaryStorage)
  private
    FRefEvapArray: TFmpRefEvapArray;
    function GetRefEvapArray: TFmpRefEvapArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream;
      Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property RefEvapArray: TFmpRefEvapArray read GetRefEvapArray;
  end;

  // @name represents a MODFLOW reference evapotranspiration in the Farm Process
  // for one time interval.
  // @name is stored by @link(TFmpRefEvapCollection).
  TFmpRefEvapItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(RefEvapRate).
    FRefEvapRate: IFormulaObject;
    // See @link(RefEvapRate).
    procedure SetRefEvapRate(const Value: string);
    function GetRefEvapRate: string;
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
    property RefEvapRate: string read GetRefEvapRate write SetRefEvapRate;
  end;

  TFmpRefEvapTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the recharge rates for a series of
    // cells over a series of time intervals.
    FRefEvapRateData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
    property RefEvapRateData: TModflowTimeList read FRefEvapRateData;
  public
    Destructor Destroy; override;
  end;

  // @name represents MODFLOW Farm Process reference evapotranspiration boundaries
  // for a series of time intervals.
  TFmpRefEvapCollection = class(TCustomMF_ArrayBoundColl)
  private
    procedure InvalidateRefEvapData(Sender: TObject);
  protected
    function PackageAssignmentMethod(AModel: TBaseModel): TUpdateMethod; virtual;
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
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
    // the @link(TFmpRefEvapStorage.RefEvapArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
  end;

  // Each @name stores a @link(TFmpRefEvapCollection).
  // @classname is stored by @link(TModflowParameters).

  TRefEvap_Cell = class(TValueCell)
  private
    FValues: TFmpEvapRecord;
    FStressPeriod: integer;
    function GetRefEvapRate: double;
    function GetRefEvapRateAnnotation: string;
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
    property Values: TFmpEvapRecord read FValues write FValues;
    property RefEvapRate: double read GetRefEvapRate;
    property RefEvapRateAnnotation: string read GetRefEvapRateAnnotation;
  end;


  // @name represents the MODFLOW Farm Process reference evapotranspiration
  // boundaries associated with
  // a single @link(TScreenObject).
  //
  // @seealso(TFmpRefEvapCollection)
  TFmpRefEvapBoundary = class(TModflowBoundary)
  private
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TRefEvap_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    // See @link(TModflowParamBoundary.ModflowParamItemClass
    // TModflowParamBoundary.ModflowParamItemClass).
//    class function ModflowParamItemClass: TModflowParamItemClass; override;
//    function ParameterType: TParameterType; override;
  public
//    procedure Assign(Source: TPersistent);override;

//    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
//    Destructor Destroy; override;
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TFmpRefEvapStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW Recharge parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TFmpRefEvapStorage) in @link(TCustomMF_BoundColl.Boundaries
    // Param.Param.Boundaries)
    // Those represent parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    function Used: boolean; override;
//    procedure EvaluateArrayBoundaries(AModel: TBaseModel); override;
    function NonParameterColumns: integer; override;
    procedure InvalidateDisplay; override;
//    procedure Clear; override;
  end;

implementation

uses RbwParser, ScreenObjectUnit, PhastModelUnit, ModflowTimeUnit,
  frmGoPhastUnit,
  AbstractGridUnit;

resourcestring
  StrRefEvapRate = 'Reference evapotranspiration rate';

const
  RefEvapPosition = 0;

{ TFmpRefEvapItem }

procedure TFmpRefEvapItem.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if Source is TFmpRefEvapItem then
  begin
    RefEvapRate := TFmpRefEvapItem(Source).RefEvapRate;
  end;
  inherited;
end;

procedure TFmpRefEvapItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TFmpRefEvapCollection;
  RechObserver: TObserver;
begin
  ParentCollection := Collection as TFmpRefEvapCollection;
  RechObserver := FObserverList[RefEvapPosition];
  RechObserver.OnUpToDateSet := ParentCollection.InvalidateRefEvapData;
end;

function TFmpRefEvapItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

procedure TFmpRefEvapItem.CreateFormulaObjects;
begin
  inherited;
  FRefEvapRate := CreateFormulaObject(dsoTop);
end;

destructor TFmpRefEvapItem.Destroy;
begin
  RefEvapRate := '0';
  inherited;
end;

function TFmpRefEvapItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    RefEvapPosition: result := RefEvapRate;
    else Assert(False);
  end;
end;

procedure TFmpRefEvapItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  Assert(Sender = FRefEvapRate as TObject);
  List.Add(FObserverList[RefEvapPosition]);
end;

function TFmpRefEvapItem.GetRefEvapRate: string;
begin
  Result := FRefEvapRate.Formula;
  ResetItemObserver(RefEvapPosition);
end;

function TFmpRefEvapItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TFmpRefEvapItem;
begin
  result := (AnotherItem is TFmpRefEvapItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TFmpRefEvapItem(AnotherItem);
    result := (Item.RefEvapRate = RefEvapRate)
  end;
end;

procedure TFmpRefEvapItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FRefEvapRate,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TFmpRefEvapItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    RefEvapPosition: RefEvapRate := Value;
    else Assert(False);
  end;
end;

procedure TFmpRefEvapItem.SetRefEvapRate(const Value: string);
begin
  UpdateFormulaBlocks(Value, RefEvapPosition, FRefEvapRate);
end;

{ TFmpRefEvapCollection }

procedure TFmpRefEvapCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TFmpRefEvapStorage.Create(AModel));
end;

procedure TFmpRefEvapCollection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel; PestSeries: TStringList;
  PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList);
var
  RefEvapArray: TDataArray;
  Boundary: TFmpRefEvapStorage;
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
  RefEvapArray := DataSets[RefEvapPosition];
  Boundary := Boundaries[ItemIndex, AModel] as TFmpRefEvapStorage;
  RefEvapArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
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
            if RefEvapArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              with Boundary.RefEvapArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                RefEvapRate := RefEvapArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                RefEvapRateAnnotation := RefEvapArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  RefEvapArray.CacheData;
  Boundary.CacheData;
end;

class function TFmpRefEvapCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmpRefEvapTimeListLink;
end;

procedure TFmpRefEvapCollection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList; Writer: TObject);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TFmpRefEvapItem;
  ScreenObject: TScreenObject;
  ALink: TFmpRefEvapTimeListLink;
  RefEvapRateData: TModflowTimeList;
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
    Item := Items[Index] as TFmpRefEvapItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.RefEvapRate;
  end;
  ALink := TimeListLink.GetLink(AModel) as TFmpRefEvapTimeListLink;
  RefEvapRateData := ALink.FRefEvapRateData;
  RefEvapRateData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(RefEvapRateData.Count = Count);

  if PackageAssignmentMethod(AModel) = umAdd then
  begin
    Grid := (AModel as TCustomModel).Grid;
    for DataArrayIndex := 0 to RefEvapRateData.Count - 1 do
    begin
      DataArray := RefEvapRateData[DataArrayIndex] as TTransientRealSparseDataSet;
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
  SetBoundaryCapacity(RefEvapRateData.Count, AModel);
  for TimeIndex := 0 to RefEvapRateData.Count - 1 do
  begin
    AddBoundary(TFmpRefEvapStorage.Create(AModel));
  end;
  ListOfTimeLists.Add(RefEvapRateData);
end;

procedure TFmpRefEvapCollection.InvalidateRefEvapData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TFmpRefEvapTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TFmpRefEvapTimeListLink;
    Link.FRefEvapRateData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TFmpRefEvapTimeListLink;
        Link.FRefEvapRateData.Invalidate;
      end;
    end;
  end;
end;

class function TFmpRefEvapCollection.ItemClass: TBoundaryItemClass;
begin
  result := TFmpRefEvapItem;
end;

function TFmpRefEvapCollection.PackageAssignmentMethod(
  AModel: TBaseModel): TUpdateMethod;
var
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  result := LocalModel.ModflowPackages.FarmProcess.AssignmentMethod;
end;

procedure TFmpRefEvapCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TFmpRefEvapStorage).FRefEvapArray,
    BoundaryCount);
  inherited;
end;


{ TRefEvap_Cell }

procedure TRefEvap_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TRefEvap_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TRefEvap_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
//  case Index of
//    PrecipPosition: result := StrAssignedFromTheCe;
//    else Assert(False);
//  end;
end;

function TRefEvap_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  case Index of
    RefEvapPosition: result := (AModel as TCustomModel).
      DataSetLayerToModflowLayer(Layer);
    else Assert(False);
  end;
end;

function TRefEvap_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TRefEvap_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    RefEvapPosition: result := RefEvapRateAnnotation;
    else Assert(False);
  end;
end;

function TRefEvap_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    RefEvapPosition: result := RefEvapRate;
    else Assert(False);
  end;
end;

function TRefEvap_Cell.GetRefEvapRate: double;
begin
  result := Values.RefEvapRate;
end;

function TRefEvap_Cell.GetRefEvapRateAnnotation: string;
begin
  result := Values.RefEvapRateAnnotation;
end;

function TRefEvap_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TRefEvap_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

procedure TRefEvap_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TRefEvap_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TRefEvap_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TRefEvap_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TRefEvap_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TFmpRefEvapBoundary }

//procedure TFmpPrecipBoundary.Assign(Source: TPersistent);
//begin
//  inherited;
//end;

procedure TFmpRefEvapBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TRefEvap_Cell;
  BoundaryValues: TFmpEvapRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TFmpRefEvapStorage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TFmpRefEvapStorage;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TRefEvap_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.RefEvapArray) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.RefEvapArray)
      end;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.RefEvapArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.RefEvapArray[BoundaryIndex];
        Cell := TRefEvap_Cell.Create;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
        Cell.ScreenObject := ScreenObjectI;
//        LocalModel.AdjustCellPosition(Cell);
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TFmpRefEvapBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmpRefEvapCollection;
end;

//procedure TFmpPrecipBoundary.Clear;
//begin
//  inherited;
//end;

//constructor TFmpPrecipBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
//begin
//  inherited Create(Model, ScreenObject);
//end;
//
//destructor TFmpPrecipBoundary.Destroy;
//begin
//  inherited;
//end;

//procedure TFmpPrecipBoundary.EvaluateArrayBoundaries(AModel: TBaseModel);
//begin
//  inherited;
//end;

procedure TFmpRefEvapBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
var
  ValueIndex: Integer;
  BoundaryStorage: TFmpRefEvapStorage;
//  ParamIndex: Integer;
//  Param: TModflowParamItem;
//  Times: TList;
//  Position: integer;
//  ParamName: string;
//  Model: TCustomModel;
begin
  EvaluateArrayBoundaries(AModel, Writer);
//  Model := ParentModel as TCustomModel;
//  if Model.ModflowTransientParameters.CountParam(ParameterType) = 0 then
//  begin
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TFmpRefEvapStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
//  end
//  else
//  begin
//    for ParamIndex := 0 to Parameters.Count - 1 do
//    begin
//      Param := Parameters[ParamIndex];
//      ParamName := Param.Param.ParamName;
//      Position := ParamList.IndexOf(ParamName);
//      if Position < 0 then
//      begin
//        Times := TObjectList.Create;
//        ParamList.AddObject(ParamName, Times);
//      end
//      else
//      begin
//        Times := ParamList.Objects[Position] as TList;
//      end;
//      for ValueIndex := 0 to Param.Param.Count - 1 do
//      begin
//        if ValueIndex < Param.Param.BoundaryCount[AModel] then
//        begin
//          BoundaryStorage := Param.Param.Boundaries[ValueIndex, AModel] as TFmpPrecipStorage;
//          AssignCells(BoundaryStorage, Times, AModel);
//        end;
//      end;
//    end;
//  end;
  ClearBoundaries(AModel);
end;

procedure TFmpRefEvapBoundary.InvalidateDisplay;
var
  Model: TCustomModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TCustomModel;
    if Model.ModelSelection = msModflowFmp then
    begin
      Model.InvalidateMfFmpEvap(self);
    end
    else
    begin
      Model.InvalidateMfFmp4Evap(self);
    end;
  end;
end;

//class function TFmpPrecipBoundary.ModflowParamItemClass: TModflowParamItemClass;
//begin
//  result := TFmpPrecipParamItem;
//end;

function TFmpRefEvapBoundary.NonParameterColumns: integer;
begin
  result := inherited NonParameterColumns;
end;

//function TFmpPrecipBoundary.ParameterType: TParameterType;
//begin
//    Assert(False);
//  result := ptUndefined;
//end;

function TFmpRefEvapBoundary.Used: boolean;
//var
//  Model: TCustomModel;
//  ParamIndex: Integer;
//  Param: TModflowTransientListParameter;
begin
  result := inherited Used;
  if result and (ParentModel <> nil) then
  begin
//    Model := ParentModel as TCustomModel;
//    for ParamIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
//    begin
//      Param := Model.ModflowTransientParameters[ParamIndex];
//      if Param.ParameterType = ptRCH then
//      begin
//        result := Parameters.Used;
//        Exit;
//      end;
//    end;
    result := Values.Used;
  end;
end;

{ TFmpPrecipStorage }

procedure TFmpRefEvapStorage.Clear;
begin
  SetLength(FRefEvapArray, 0);
  FCleared := True;
end;

procedure TFmpRefEvapStorage.Store(Compressor: TCompressionStream);
var
  Count: Integer;
  Index: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FRefEvapArray);
    for Index := 0 to Count - 1 do
    begin
      FRefEvapArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FRefEvapArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TFmpRefEvapStorage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FRefEvapArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FRefEvapArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TFmpRefEvapStorage.GetRefEvapArray: TFmpRefEvapArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FRefEvapArray;
end;

{ TFmpEvapRecord }

procedure TFmpEvapRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, RefEvapRate);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Strings.IndexOf(RefEvapRateAnnotation));
end;

procedure TFmpEvapRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(RefEvapRateAnnotation);
end;

procedure TFmpEvapRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  RefEvapRate := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  RefEvapRateAnnotation := Annotations[ReadCompInt(Decomp)];
end;

{ TFmpRefEvapTimeListLink }

procedure TFmpRefEvapTimeListLink.CreateTimeLists;
begin
  inherited;
  FRefEvapRateData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FRefEvapRateData.NonParamDescription := StrRefEvapRate;
  FRefEvapRateData.ParamDescription := ' ' + LowerCase(StrRefEvapRate);
  AddTimeList(FRefEvapRateData);
  if Model <> nil then
  begin
    if Model.ModelSelection = msModflowFmp then
    begin
      FRefEvapRateData.OnInvalidate := (Model as TCustomModel).InvalidateMfFmpEvap;
    end
    {$IFDEF OWHMV2}
    else if Model.ModelSelection = msModflowOwhm2 then
    begin
      FRefEvapRateData.OnInvalidate := (Model as TCustomModel).InvalidateMfFmp4Evap;
    end
    {$ENDIF}
    ;
  end;
end;

destructor TFmpRefEvapTimeListLink.Destroy;
begin
  FRefEvapRateData.Free;
  inherited;
end;

end.
