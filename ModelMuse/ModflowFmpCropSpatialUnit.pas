unit ModflowFmpCropSpatialUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, GoPhastTypes;

type
  {
    @longcode(
  TFmpCropIDRecord = record
    Cell: TCellLocation;
    CropID: integer;
    StartingTime: double;
    EndingTime: double;
    CropIDAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;
    )
    @name stores the location, time and Crop ID rate for a cell.
  }
  TFmpCropIDRecord = record
    Cell: TCellLocation;
    CropID: integer;
    StartingTime: double;
    EndingTime: double;
    CropIDAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  // @name is an array of @link(TFmpCropIDRecord)s.
  TFmpCropIDArray = array of TFmpCropIDRecord;

  // @name extends @link(TCustomBoundaryStorage) by adding the locations
  // and values of series of Crop ID cells.
  TFmpCropIDStorage = class(TCustomBoundaryStorage)
  private
    FCropIDArray: TFmpCropIDArray;
    function GetCropIDArray: TFmpCropIDArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream;
      Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property CropIDArray: TFmpCropIDArray read GetCropIDArray;
  end;

  // @name represents a MODFLOW Crop ID in the Farm Process
  // for one time interval.
  // @name is stored by @link(TFmpCropIDCollection).
  TFmpCropIDItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(CropID).
    FCropID: TFormulaObject;
    // See @link(CropID).
    procedure SetCropID(const Value: string);
    function GetCropID: string;
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
    property CropID: string read GetCropID write SetCropID;
  end;

  TFmpCropIDTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the Crop ID for a series of
    // cells over a series of time intervals.
    FCropIDData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
    property CropIDData: TModflowTimeList read FCropIDData;
  public
    Destructor Destroy; override;
  end;

  // @name represents MODFLOW Farm Process Crop ID boundaries
  // for a series of time intervals.
  TFmpCropIDCollection = class(TCustomMF_ArrayBoundColl)
  private
    procedure InvalidateCropIDData(Sender: TObject);
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
    // the @link(TFmpCropIDStorage.CropIDArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
  end;

  // Each @name stores a @link(TFmpCropIDCollection).
  // @classname is stored by @link(TModflowParameters).

  TCropID_Cell = class(TValueCell)
  private
    FValues: TFmpCropIDRecord;
    FStressPeriod: integer;
    function GetCropID: integer;
    function GetCropIDAnnotation: string;
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
    property Values: TFmpCropIDRecord read FValues write FValues;
    property CropID: integer read GetCropID;
    property CropIDAnnotation: string read GetCropIDAnnotation;
  end;


  // @name represents the MODFLOW Farm Process Crop ID
  // values associated with
  // a single @link(TScreenObject).
  //
  // @seealso(TFmpCropIDCollection)
  TFmpCropIDBoundary = class(TModflowBoundary)
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TCropID_Cell)s for that stress period.
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
    // link  @link(TFmpCropIDStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW Recharge parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TFmpCropIDStorage) in @link(TCustomMF_BoundColl.Boundaries
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
  StrCropID = 'Crop ID';

const
  CropIDPosition = 0;

{ TFmpCropIDItem }

procedure TFmpCropIDItem.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if Source is TFmpCropIDItem then
  begin
    CropID := TFmpCropIDItem(Source).CropID;
  end;
  inherited;
end;

procedure TFmpCropIDItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TFmpCropIDCollection;
  RechObserver: TObserver;
begin
  ParentCollection := Collection as TFmpCropIDCollection;
  RechObserver := FObserverList[CropIDPosition];
  RechObserver.OnUpToDateSet := ParentCollection.InvalidateCropIDData;
end;

function TFmpCropIDItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

procedure TFmpCropIDItem.CreateFormulaObjects;
begin
  inherited;
  FCropID := CreateFormulaObject(dsoTop);
end;

destructor TFmpCropIDItem.Destroy;
begin
  CropID := '0';
  inherited;
end;

function TFmpCropIDItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    CropIDPosition: result := CropID;
    else Assert(False);
  end;
end;

procedure TFmpCropIDItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  Assert(Sender = FCropID);
  List.Add(FObserverList[CropIDPosition]);
end;

function TFmpCropIDItem.GetCropID: string;
begin
  Result := FCropID.Formula;
  ResetItemObserver(CropIDPosition);
end;

function TFmpCropIDItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TFmpCropIDItem;
begin
  result := (AnotherItem is TFmpCropIDItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TFmpCropIDItem(AnotherItem);
    result := (Item.CropID = CropID)
  end;
end;

procedure TFmpCropIDItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FCropID,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TFmpCropIDItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    CropIDPosition: CropID := Value;
    else Assert(False);
  end;
end;

procedure TFmpCropIDItem.SetCropID(const Value: string);
begin
  UpdateFormula(Value, CropIDPosition, FCropID);
end;

{ TFmpCropIDCollection }

procedure TFmpCropIDCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TFmpCropIDStorage.Create(AModel));
end;

procedure TFmpCropIDCollection.AssignArrayCellValues(DataSets: TList; ItemIndex: Integer;
  AModel: TBaseModel);
var
  CropIDArray: TDataArray;
  Boundary: TFmpCropIDStorage;
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
  CropIDArray := DataSets[CropIDPosition];
  Boundary := Boundaries[ItemIndex, AModel] as TFmpCropIDStorage;
  CropIDArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
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
            if CropIDArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              with Boundary.CropIDArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                CropID := CropIDArray.
                  IntegerData[LayerIndex, RowIndex, ColIndex];
                CropIDAnnotation := CropIDArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  CropIDArray.CacheData;
  Boundary.CacheData;
end;

function TFmpCropIDCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFmpCropIDTimeListLink;
end;

procedure TFmpCropIDCollection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TFmpCropIDItem;
  ScreenObject: TScreenObject;
  ALink: TFmpCropIDTimeListLink;
  CropIDData: TModflowTimeList;
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
    Item := Items[Index] as TFmpCropIDItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.CropID;
  end;
  ALink := TimeListLink.GetLink(AModel) as TFmpCropIDTimeListLink;
  CropIDData := ALink.FCropIDData;
  CropIDData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(CropIDData.Count = Count);

  if PackageAssignmentMethod(AModel) = umAdd then
  begin
    Grid := (AModel as TCustomModel).Grid;
    for DataArrayIndex := 0 to CropIDData.Count - 1 do
    begin
      DataArray := CropIDData[DataArrayIndex] as TTransientIntegerSparseDataSet;
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
  SetBoundaryCapacity(CropIDData.Count, AModel);
  for TimeIndex := 0 to CropIDData.Count - 1 do
  begin
    AddBoundary(TFmpCropIDStorage.Create(AModel));
  end;
  ListOfTimeLists.Add(CropIDData);
end;

procedure TFmpCropIDCollection.InvalidateCropIDData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TFmpCropIDTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TFmpCropIDTimeListLink;
    Link.FCropIDData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TFmpCropIDTimeListLink;
      Link.FCropIDData.Invalidate;
    end;
  end;
end;

class function TFmpCropIDCollection.ItemClass: TBoundaryItemClass;
begin
  result := TFmpCropIDItem;
end;

function TFmpCropIDCollection.PackageAssignmentMethod(
  AModel: TBaseModel): TUpdateMethod;
var
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  result := LocalModel.ModflowPackages.FarmProcess.AssignmentMethod;
end;

procedure TFmpCropIDCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TFmpCropIDStorage).FCropIDArray,
    BoundaryCount);
  inherited;
end;


{ TCropID_Cell }

procedure TCropID_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TCropID_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TCropID_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    CropIDPosition: result := CropIDAnnotation;
    else Assert(False);
  end;
end;

function TCropID_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  case Index of
    CropIDPosition: result := CropID;
    else Assert(False);
  end;
end;

function TCropID_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TCropID_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    CropIDPosition: result := CropIDAnnotation;
    else Assert(False);
  end;
end;

function TCropID_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    CropIDPosition: result := CropID;
    else Assert(False);
  end;
end;

function TCropID_Cell.GetCropID: integer;
begin
  result := Values.CropID;
end;

function TCropID_Cell.GetCropIDAnnotation: string;
begin
  result := Values.CropIDAnnotation;
end;

function TCropID_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TCropID_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

procedure TCropID_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TCropID_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TCropID_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TCropID_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TCropID_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TFmpCropIDBoundary }

//procedure TFmpPrecipBoundary.Assign(Source: TPersistent);
//begin
//  inherited;
//end;

procedure TFmpCropIDBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TCropID_Cell;
  BoundaryValues: TFmpCropIDRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TFmpCropIDStorage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TFmpCropIDStorage;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TCropID_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.CropIDArray) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.CropIDArray)
      end;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.CropIDArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.CropIDArray[BoundaryIndex];
        Cell := TCropID_Cell.Create;
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

class function TFmpCropIDBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFmpCropIDCollection;
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

procedure TFmpCropIDBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TFmpCropIDStorage;
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
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TFmpCropIDStorage;
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

procedure TFmpCropIDBoundary.InvalidateDisplay;
var
  Model: TCustomModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TCustomModel;
    Model.InvalidateMfFmpCropID(self);
  end;
end;

//class function TFmpPrecipBoundary.ModflowParamItemClass: TModflowParamItemClass;
//begin
//  result := TFmpPrecipParamItem;
//end;

function TFmpCropIDBoundary.NonParameterColumns: integer;
begin
  result := inherited NonParameterColumns;
end;

//function TFmpPrecipBoundary.ParameterType: TParameterType;
//begin
//    Assert(False);
//  result := ptUndefined;
//end;

function TFmpCropIDBoundary.Used: boolean;
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

procedure TFmpCropIDStorage.Clear;
begin
  SetLength(FCropIDArray, 0);
  FCleared := True;
end;

procedure TFmpCropIDStorage.Store(Compressor: TCompressionStream);
var
  Count: Integer;
  Index: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FCropIDArray);
    for Index := 0 to Count - 1 do
    begin
      FCropIDArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FCropIDArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TFmpCropIDStorage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FCropIDArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FCropIDArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TFmpCropIDStorage.GetCropIDArray: TFmpCropIDArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FCropIDArray;
end;

{ TFmpCropIDRecord }

procedure TFmpCropIDRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompInt(Comp, CropID);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Strings.IndexOf(CropIDAnnotation));
end;

procedure TFmpCropIDRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(CropIDAnnotation);
end;

procedure TFmpCropIDRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  CropID := ReadCompInt(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  CropIDAnnotation := Annotations[ReadCompInt(Decomp)];
end;

{ TFmpCropIDTimeListLink }

procedure TFmpCropIDTimeListLink.CreateTimeLists;
begin
  inherited;
  FCropIDData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FCropIDData.NonParamDescription := StrCropID;
  FCropIDData.ParamDescription := ' ' + LowerCase(StrCropID);
  FCropIDData.DataType := rdtInteger;
  AddTimeList(FCropIDData);
  if Model <> nil then
  begin
    FCropIDData.OnInvalidate := (Model as TCustomModel).InvalidateMfFmpCropID;
  end;
end;

destructor TFmpCropIDTimeListLink.Destroy;
begin
  FCropIDData.Free;
  inherited;
end;

end.
