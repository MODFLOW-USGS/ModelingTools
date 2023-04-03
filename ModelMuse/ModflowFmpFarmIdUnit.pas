unit ModflowFmpFarmIdUnit;

interface

uses Windows, ZLib, SysUtils, Classes, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, GoPhastTypes;

type
  {
    @longcode(
  TFmpFarmIDRecord = record
    Cell: TCellLocation;
    FarmID: integer;
    StartingTime: double;
    EndingTime: double;
    FarmIDAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;
    )
    @name stores the location, time and Farm ID for a cell.
  }
  TFmpFarmIDRecord = record
    Cell: TCellLocation;
    FarmID: integer;
    StartingTime: double;
    EndingTime: double;
    FarmIDAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  // @name is an array of @link(TFmpFarmIDRecord)s.
  TFmpFarmIDArray = array of TFmpFarmIDRecord;

  // @name extends @link(TCustomBoundaryStorage) by adding the locations
  // and values of series of Farm ID cells.
  TFmpFarmIDStorage = class(TCustomBoundaryStorage)
  private
    FFarmIDArray: TFmpFarmIDArray;
    function GetFarmIDArray: TFmpFarmIDArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream;
      Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property FarmIDArray: TFmpFarmIDArray read GetFarmIDArray;
  end;

  // @name represents a MODFLOW Farm ID in the Farm Process
  // for one time interval.
  // @name is stored by @link(TFmpFarmIDCollection).
  TFmpFarmIDItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(FarmID).
    FFarmID: TFormulaObject;
    // See @link(FarmID).
    procedure SetFarmID(const Value: string);
    function GetFarmID: string;
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
    property FarmID: string read GetFarmID write SetFarmID;
  end;

  TFmpFarmIDTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the Farm ID for a series of
    // cells over a series of time intervals.
    FFarmIDData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
    property FarmIDData: TModflowTimeList read FFarmIDData;
  public
    Destructor Destroy; override;
  end;

  // @name represents MODFLOW Farm Process Farm ID boundaries
  // for a series of time intervals.
  TFmpFarmIDCollection = class(TCustomMF_ArrayBoundColl)
  private
    procedure InvalidateFarmIDData(Sender: TObject);
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
    // the @link(TFmpFarmIDStorage.FarmIDArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
  end;

  // Each @name stores a @link(TFmpFarmIDCollection).
  // @classname is stored by @link(TModflowParameters).

  TFarmID_Cell = class(TValueCell)
  private
    FValues: TFmpFarmIDRecord;
    FStressPeriod: integer;
    function GetFarmID: integer;
    function GetFarmIDAnnotation: string;
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
    property Values: TFmpFarmIDRecord read FValues write FValues;
    property FarmID: integer read GetFarmID;
    property FarmIDAnnotation: string read GetFarmIDAnnotation;
  end;


  // @name represents the MODFLOW Farm Process Farm ID
  // values associated with
  // a single @link(TScreenObject).
  //
  // @seealso(TFmpFarmIDCollection)
  TFmpFarmIDBoundary = class(TModflowBoundary)
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TFarmID_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
  public
//    procedure Assign(Source: TPersistent);override;

//    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
//    Destructor Destroy; override;
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TFmpFarmIDStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW Recharge parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TFmpFarmIDStorage) in @link(TCustomMF_BoundColl.Boundaries
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
  StrFarmID = 'Farm ID';

const
  FarmIDPosition = 0;

{ TFmpFarmIDItem }

procedure TFmpFarmIDItem.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if Source is TFmpFarmIDItem then
  begin
    FarmID := TFmpFarmIDItem(Source).FarmID;
  end;
  inherited;
end;

procedure TFmpFarmIDItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TFmpFarmIDCollection;
  RechObserver: TObserver;
begin
  ParentCollection := Collection as TFmpFarmIDCollection;
  RechObserver := FObserverList[FarmIDPosition];
  RechObserver.OnUpToDateSet := ParentCollection.InvalidateFarmIDData;
end;

function TFmpFarmIDItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

procedure TFmpFarmIDItem.CreateFormulaObjects;
begin
  inherited;
  FFarmID := CreateFormulaObject(dsoTop);
end;

destructor TFmpFarmIDItem.Destroy;
begin
  FarmID := '0';
  inherited;
end;

function TFmpFarmIDItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    FarmIDPosition: result := FarmID;
    else Assert(False);
  end;
end;

procedure TFmpFarmIDItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  Assert(Sender = FFarmID);
  List.Add(FObserverList[FarmIDPosition]);
end;

function TFmpFarmIDItem.GetFarmID: string;
begin
  Result := FFarmID.Formula;
  ResetItemObserver(FarmIDPosition);
end;

function TFmpFarmIDItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TFmpFarmIDItem;
begin
  result := (AnotherItem is TFmpFarmIDItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TFmpFarmIDItem(AnotherItem);
    result := (Item.FarmID = FarmID)
  end;
end;

procedure TFmpFarmIDItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FFarmID,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TFmpFarmIDItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    FarmIDPosition: FarmID := Value;
    else Assert(False);
  end;
end;

procedure TFmpFarmIDItem.SetFarmID(const Value: string);
begin
  UpdateFormulaBlocks(Value, FarmIDPosition, FFarmID);
end;

{ TFmpFarmIDCollection }

procedure TFmpFarmIDCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TFmpFarmIDStorage.Create(AModel));
end;

procedure TFmpFarmIDCollection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel; PestSeries: TStringList;
  PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList);
var
  FarmIDArray: TDataArray;
  Boundary: TFmpFarmIDStorage;
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
  FarmIDArray := DataSets[FarmIDPosition];
  Boundary := Boundaries[ItemIndex, AModel] as TFmpFarmIDStorage;
  FarmIDArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
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
            if FarmIDArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              with Boundary.FarmIDArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                FarmID := FarmIDArray.
                  IntegerData[LayerIndex, RowIndex, ColIndex];
                FarmIDAnnotation := FarmIDArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  FarmIDArray.CacheData;
  Boundary.CacheData;
end;

class function TFmpFarmIDCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmpFarmIDTimeListLink;
end;

procedure TFmpFarmIDCollection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList; Writer: TObject);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TFmpFarmIDItem;
  ScreenObject: TScreenObject;
  ALink: TFmpFarmIDTimeListLink;
  FarmIDData: TModflowTimeList;
  DataArrayIndex: Integer;
  DataArray: TTransientIntegerSparseDataSet;
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
    Item := Items[Index] as TFmpFarmIDItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.FarmID;
  end;
  ALink := TimeListLink.GetLink(AModel) as TFmpFarmIDTimeListLink;
  FarmIDData := ALink.FFarmIDData;
  FarmIDData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(FarmIDData.Count = Count);

  if PackageAssignmentMethod(AModel) = umAdd then
  begin
    Grid := (AModel as TCustomModel).Grid;
    for DataArrayIndex := 0 to FarmIDData.Count - 1 do
    begin
      DataArray := FarmIDData[DataArrayIndex] as TTransientIntegerSparseDataSet;
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
  SetBoundaryCapacity(FarmIDData.Count, AModel);
  for TimeIndex := 0 to FarmIDData.Count - 1 do
  begin
    AddBoundary(TFmpFarmIDStorage.Create(AModel));
  end;
  ListOfTimeLists.Add(FarmIDData);
end;

procedure TFmpFarmIDCollection.InvalidateFarmIDData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TFmpFarmIDTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TFmpFarmIDTimeListLink;
    Link.FFarmIDData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TFmpFarmIDTimeListLink;
        Link.FFarmIDData.Invalidate;
      end;
    end;
  end;
end;

class function TFmpFarmIDCollection.ItemClass: TBoundaryItemClass;
begin
  result := TFmpFarmIDItem;
end;

function TFmpFarmIDCollection.PackageAssignmentMethod(
  AModel: TBaseModel): TUpdateMethod;
var
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  result := LocalModel.ModflowPackages.FarmProcess.AssignmentMethod;
end;

procedure TFmpFarmIDCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TFmpFarmIDStorage).FFarmIDArray,
    BoundaryCount);
  inherited;
end;


{ TFarmID_Cell }

procedure TFarmID_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TFarmID_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TFarmID_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    FarmIDPosition: result := FarmIDAnnotation;
    else Assert(False);
  end;
end;

function TFarmID_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  case Index of
    FarmIDPosition: result := FarmID;
    else Assert(False);
  end;
end;

function TFarmID_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TFarmID_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    FarmIDPosition: result := FarmIDAnnotation;
    else Assert(False);
  end;
end;

function TFarmID_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    FarmIDPosition: result := FarmID;
    else Assert(False);
  end;
end;

function TFarmID_Cell.GetFarmID: integer;
begin
  result := Values.FarmID;
end;

function TFarmID_Cell.GetFarmIDAnnotation: string;
begin
  result := Values.FarmIDAnnotation;
end;

function TFarmID_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TFarmID_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

procedure TFarmID_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TFarmID_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TFarmID_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TFarmID_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TFarmID_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TFmpFarmIDBoundary }

//procedure TFmpPrecipBoundary.Assign(Source: TPersistent);
//begin
//  inherited;
//end;

procedure TFmpFarmIDBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TFarmID_Cell;
  BoundaryValues: TFmpFarmIDRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TFmpFarmIDStorage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TFmpFarmIDStorage;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TFarmID_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.FarmIDArray) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.FarmIDArray)
      end;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.FarmIDArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.FarmIDArray[BoundaryIndex];
        Cell := TFarmID_Cell.Create;
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

class function TFmpFarmIDBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmpFarmIDCollection;
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

procedure TFmpFarmIDBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
var
  ValueIndex: Integer;
  BoundaryStorage: TFmpFarmIDStorage;
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
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TFmpFarmIDStorage;
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

procedure TFmpFarmIDBoundary.InvalidateDisplay;
var
  Model: TCustomModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TCustomModel;
    if Model.ModelSelection = msModflow2015 then
    begin
      Model.InvalidateMfFmpFarmID(self);
    end
  {$IFDEF OWHMV2}
    else if Model.ModelSelection = msModflowOwhm2 then
    begin
      Model.InvalidateMfFmp4FarmID(self);
    end
  {$ENDIF}
    ;
  end;
end;

//class function TFmpPrecipBoundary.ModflowParamItemClass: TModflowParamItemClass;
//begin
//  result := TFmpPrecipParamItem;
//end;

function TFmpFarmIDBoundary.NonParameterColumns: integer;
begin
  result := inherited NonParameterColumns;
end;

//function TFmpPrecipBoundary.ParameterType: TParameterType;
//begin
//    Assert(False);
//  result := ptUndefined;
//end;

function TFmpFarmIDBoundary.Used: boolean;
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

procedure TFmpFarmIDStorage.Clear;
begin
  SetLength(FFarmIDArray, 0);
  FCleared := True;
end;

procedure TFmpFarmIDStorage.Store(Compressor: TCompressionStream);
var
  Count: Integer;
  Index: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FFarmIDArray);
    for Index := 0 to Count - 1 do
    begin
      FFarmIDArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FFarmIDArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TFmpFarmIDStorage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FFarmIDArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FFarmIDArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TFmpFarmIDStorage.GetFarmIDArray: TFmpFarmIDArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FFarmIDArray;
end;

{ TFmpFarmIDRecord }

procedure TFmpFarmIDRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompInt(Comp, FarmID);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Strings.IndexOf(FarmIDAnnotation));
end;

procedure TFmpFarmIDRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(FarmIDAnnotation);
end;

procedure TFmpFarmIDRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  FarmID := ReadCompInt(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  FarmIDAnnotation := Annotations[ReadCompInt(Decomp)];
end;

{ TFmpFarmIDTimeListLink }

procedure TFmpFarmIDTimeListLink.CreateTimeLists;
begin
  inherited;
  FFarmIDData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FFarmIDData.NonParamDescription := StrFarmID2;
  FFarmIDData.ParamDescription := ' ' + LowerCase(StrFarmID2);
  FFarmIDData.DataType := rdtInteger;
  AddTimeList(FFarmIDData);
  if Model <> nil then
  begin
    FFarmIDData.OnInvalidate := (Model as TCustomModel).InvalidateMfFmpFarmID;
  end;
end;

destructor TFmpFarmIDTimeListLink.Destroy;
begin
  FFarmIDData.Free;
  inherited;
end;

end.
