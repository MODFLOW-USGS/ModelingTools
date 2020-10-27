unit ModflowFmpPrecipitationUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, GoPhastTypes;

type
  {
    @longcode(
  TFmpPrecipRecord = record
    Cell: TCellLocation;
    PrecipRate: double;
    StartingTime: double;
    EndingTime: double;
    PrecipRateAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;
    )
    @name stores the location, time and precipitation rate for a cell.
  }
  TFmpPrecipRecord = record
    Cell: TCellLocation;
    PrecipRate: double;
    StartingTime: double;
    EndingTime: double;
    PrecipRateAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  // @name is an array of @link(TFmpPrecipRecord)s.
  TFmpPrecipArray = array of TFmpPrecipRecord;

  // @name extends @link(TCustomBoundaryStorage) by adding the locations
  // and values of series of precipitation cells.
  TFmpPrecipStorage = class(TCustomBoundaryStorage)
  private
    FPrecipArray: TFmpPrecipArray;
    function GetPrecipArray: TFmpPrecipArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream;
      Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property PrecipArray: TFmpPrecipArray read GetPrecipArray;
  end;

  // @name represents a MODFLOW precipitation in the Farm Process
  // for one time interval.
  // @name is stored by @link(TFmpPrecipCollection).
  TFmpPrecipItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(PrecipRate).
    FPrecipRate: TFormulaObject;
    // See @link(PrecipRate).
    procedure SetPrecipRate(const Value: string);
    function GetPrecipRate: string;
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
    property PrecipRate: string read GetPrecipRate write SetPrecipRate;
  end;

  TFmpPrecipTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the precipitation rates for a series of
    // cells over a series of time intervals.
    FPrecipRateData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
    property PrecipRateData: TModflowTimeList read FPrecipRateData;
  public
    Destructor Destroy; override;
  end;

  // @name represents MODFLOW Farm Process Precipitation boundaries
  // for a series of time intervals.
  TFmpPrecipCollection = class(TCustomMF_ArrayBoundColl)
  private
    procedure InvalidatePrecipData(Sender: TObject);
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
    // the @link(TFmpPrecipStorage.PrecipArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
  end;

  // Each @name stores a @link(TFmpPrecipCollection).
  // @classname is stored by @link(TModflowParameters).
//  TFmpPrecipParamItem = class(TModflowParamItem)
//  protected
//    class function BoundaryClass: TMF_BoundCollClass; override;
//  end;

//  TRechargeCell = class(TValueCell);

  TPrecip_Cell = class(TValueCell)
  private
    FValues: TFmpPrecipRecord;
    FStressPeriod: integer;
    function GetPrecipRate: double;
    function GetPrecipRateAnnotation: string;
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
    property Values: TFmpPrecipRecord read FValues write FValues;
    property PrecipRate: double read GetPrecipRate;
    property PrecipRateAnnotation: string read GetPrecipRateAnnotation;
  end;


  // @name represents the MODFLOW Farm Process precipitation
  // boundaries associated with
  // a single @link(TScreenObject).
  //
  // @seealso(TFmpPrecipCollection)
  TFmpPrecipBoundary = class(TModflowBoundary)
  private
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TPrecip_Cell)s for that stress period.
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
    // link  @link(TFmpPrecipStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW Recharge parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TFmpPrecipStorage) in @link(TCustomMF_BoundColl.Boundaries
    // Param.Param.Boundaries)
    // Those represent parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel); override;
    function Used: boolean; override;
//    procedure EvaluateArrayBoundaries(AModel: TBaseModel); override;
    function NonParameterColumns: integer; override;
    procedure InvalidateDisplay; override;
//    procedure Clear; override;
  end;

implementation

uses RbwParser, ScreenObjectUnit, PhastModelUnit, ModflowTimeUnit,
  ModflowTransientListParameterUnit, frmGoPhastUnit, TempFiles,
  AbstractGridUnit;

resourcestring
  StrPrecipitationRate = 'Precipitation rate';

const
  PrecipPosition = 0;

{ TFmpPrecipItem }

procedure TFmpPrecipItem.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if Source is TFmpPrecipItem then
  begin
    PrecipRate := TFmpPrecipItem(Source).PrecipRate;
  end;
  inherited;
end;

procedure TFmpPrecipItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TFmpPrecipCollection;
  RechObserver: TObserver;
begin
  ParentCollection := Collection as TFmpPrecipCollection;
  RechObserver := FObserverList[PrecipPosition];
  RechObserver.OnUpToDateSet := ParentCollection.InvalidatePrecipData;
end;

function TFmpPrecipItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

procedure TFmpPrecipItem.CreateFormulaObjects;
begin
  inherited;
  FPrecipRate := CreateFormulaObject(dsoTop);
end;

destructor TFmpPrecipItem.Destroy;
begin
  PrecipRate := '0';
  inherited;
end;

function TFmpPrecipItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    PrecipPosition: result := PrecipRate;
    else Assert(False);
  end;
end;

procedure TFmpPrecipItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  Assert(Sender = FPrecipRate);
  List.Add(FObserverList[PrecipPosition]);
end;

function TFmpPrecipItem.GetPrecipRate: string;
begin
  Result := FPrecipRate.Formula;
  ResetItemObserver(PrecipPosition);
end;

function TFmpPrecipItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TFmpPrecipItem;
begin
  result := (AnotherItem is TFmpPrecipItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TFmpPrecipItem(AnotherItem);
    result := (Item.PrecipRate = PrecipRate)
  end;
end;

procedure TFmpPrecipItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FPrecipRate,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TFmpPrecipItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    PrecipPosition: PrecipRate := Value;
    else Assert(False);
  end;
end;

procedure TFmpPrecipItem.SetPrecipRate(const Value: string);
begin
  UpdateFormulaBlocks(Value, PrecipPosition, FPrecipRate);
end;

{ TFmpPrecipCollection }

procedure TFmpPrecipCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TFmpPrecipStorage.Create(AModel));
end;

procedure TFmpPrecipCollection.AssignArrayCellValues(DataSets: TList; ItemIndex: Integer;
  AModel: TBaseModel);
var
  RechargeRateArray: TDataArray;
  Boundary: TFmpPrecipStorage;
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
  RechargeRateArray := DataSets[PrecipPosition];
  Boundary := Boundaries[ItemIndex, AModel] as TFmpPrecipStorage;
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
              with Boundary.PrecipArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                PrecipRate := RechargeRateArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                PrecipRateAnnotation := RechargeRateArray.
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

function TFmpPrecipCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmpPrecipTimeListLink;
end;

procedure TFmpPrecipCollection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TFmpPrecipItem;
  ScreenObject: TScreenObject;
  ALink: TFmpPrecipTimeListLink;
  PrecipRateData: TModflowTimeList;
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
    Item := Items[Index] as TFmpPrecipItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.PrecipRate;
  end;
  ALink := TimeListLink.GetLink(AModel) as TFmpPrecipTimeListLink;
  PrecipRateData := ALink.FPrecipRateData;
  PrecipRateData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(PrecipRateData.Count = Count);

  if PackageAssignmentMethod(AModel) = umAdd then
  begin
    Grid := (AModel as TCustomModel).Grid;
    for DataArrayIndex := 0 to PrecipRateData.Count - 1 do
    begin
      DataArray := PrecipRateData[DataArrayIndex] as TTransientRealSparseDataSet;
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
  SetBoundaryCapacity(PrecipRateData.Count, AModel);
  for TimeIndex := 0 to PrecipRateData.Count - 1 do
  begin
    AddBoundary(TFmpPrecipStorage.Create(AModel));
  end;
  ListOfTimeLists.Add(PrecipRateData);
end;

procedure TFmpPrecipCollection.InvalidatePrecipData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TFmpPrecipTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TFmpPrecipTimeListLink;
    Link.FPrecipRateData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TFmpPrecipTimeListLink;
      Link.FPrecipRateData.Invalidate;
    end;
  end;
end;

class function TFmpPrecipCollection.ItemClass: TBoundaryItemClass;
begin
  result := TFmpPrecipItem;
end;

function TFmpPrecipCollection.PackageAssignmentMethod(
  AModel: TBaseModel): TUpdateMethod;
var
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  result := LocalModel.ModflowPackages.FarmProcess.AssignmentMethod;
end;

procedure TFmpPrecipCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TFmpPrecipStorage).FPrecipArray,
    BoundaryCount);
  inherited;
end;

{ TFmpPrecipParamItem }

//class function TFmpPrecipParamItem.BoundaryClass: TMF_BoundCollClass;
//begin
//  result := TFmpPrecipCollection;
//end;

{ TPrecip_Cell }

procedure TPrecip_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TPrecip_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TPrecip_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
//  case Index of
//    PrecipPosition: result := StrAssignedFromTheCe;
//    else Assert(False);
//  end;
end;

function TPrecip_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  case Index of
    PrecipPosition: result := (AModel as TCustomModel).
      DataSetLayerToModflowLayer(Layer);
    else Assert(False);
  end;
end;

function TPrecip_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TPrecip_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    PrecipPosition: result := PrecipRateAnnotation;
    else Assert(False);
  end;
end;

function TPrecip_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    PrecipPosition: result := PrecipRate;
    else Assert(False);
  end;
end;

function TPrecip_Cell.GetPrecipRate: double;
begin
  result := Values.PrecipRate;
end;

function TPrecip_Cell.GetPrecipRateAnnotation: string;
begin
  result := Values.PrecipRateAnnotation;
end;

function TPrecip_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TPrecip_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

procedure TPrecip_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TPrecip_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TPrecip_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TPrecip_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TPrecip_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TFmpPrecipBoundary }

//procedure TFmpPrecipBoundary.Assign(Source: TPersistent);
//begin
//  inherited;
//end;

procedure TFmpPrecipBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TPrecip_Cell;
  BoundaryValues: TFmpPrecipRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TFmpPrecipStorage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TFmpPrecipStorage;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TPrecip_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.PrecipArray) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.PrecipArray)
      end;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.PrecipArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.PrecipArray[BoundaryIndex];
        Cell := TPrecip_Cell.Create;
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

class function TFmpPrecipBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmpPrecipCollection;
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
//
//procedure TFmpPrecipBoundary.EvaluateArrayBoundaries(AModel: TBaseModel);
//begin
//  inherited;
//end;

procedure TFmpPrecipBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TFmpPrecipStorage;
//  ParamIndex: Integer;
//  Param: TModflowParamItem;
//  Times: TList;
//  Position: integer;
//  ParamName: string;
//  Model: TCustomModel;
begin
  EvaluateArrayBoundaries(AModel);
//  Model := ParentModel as TCustomModel;
//  if Model.ModflowTransientParameters.CountParam(ParameterType) = 0 then
//  begin
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TFmpPrecipStorage;
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

procedure TFmpPrecipBoundary.InvalidateDisplay;
var
  Model: TCustomModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TCustomModel;
//    Assert(False);
    Model.InvalidateMfFmpPrecip(self);
  end;
end;

//class function TFmpPrecipBoundary.ModflowParamItemClass: TModflowParamItemClass;
//begin
//  result := TFmpPrecipParamItem;
//end;

function TFmpPrecipBoundary.NonParameterColumns: integer;
begin
  result := inherited NonParameterColumns;
end;

//function TFmpPrecipBoundary.ParameterType: TParameterType;
//begin
//    Assert(False);
//  result := ptUndefined;
//end;

function TFmpPrecipBoundary.Used: boolean;
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

procedure TFmpPrecipStorage.Clear;
begin
  SetLength(FPrecipArray, 0);
  FCleared := True;
end;

procedure TFmpPrecipStorage.Store(Compressor: TCompressionStream);
var
  Count: Integer;
  Index: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FPrecipArray);
    for Index := 0 to Count - 1 do
    begin
      FPrecipArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FPrecipArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TFmpPrecipStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FPrecipArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FPrecipArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TFmpPrecipStorage.GetPrecipArray: TFmpPrecipArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FPrecipArray;
end;

{ TFmpPrecipRecord }

procedure TFmpPrecipRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, PrecipRate);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Strings.IndexOf(PrecipRateAnnotation));
//  WriteCompString(Comp, PrecipRateAnnotation);
end;

procedure TFmpPrecipRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(PrecipRateAnnotation);
end;

procedure TFmpPrecipRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  PrecipRate := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  PrecipRateAnnotation := Annotations[ReadCompInt(Decomp)];
//  PrecipRateAnnotation := ReadCompString(Decomp, Annotations);
end;

{ TFmpPrecipTimeListLink }

procedure TFmpPrecipTimeListLink.CreateTimeLists;
begin
  inherited;
  FPrecipRateData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FPrecipRateData.NonParamDescription := StrPrecipitationRate;
  FPrecipRateData.ParamDescription := ' ' + LowerCase(StrPrecipitationRate);
  AddTimeList(FPrecipRateData);
  if Model <> nil then
  begin
    FPrecipRateData.OnInvalidate := (Model as TCustomModel).InvalidateMfFmpPrecip;
  end;
end;

destructor TFmpPrecipTimeListLink.Destroy;
begin
  FPrecipRateData.Free;
  inherited;
end;

end.
