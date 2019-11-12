unit ModflowEtsUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit, ModflowEvtUnit,
  FormulaManagerUnit, SubscriptionUnit, RbwParser, SparseDataSets, GoPhastTypes,
  ModflowTransientListParameterUnit;

type

  TEtsSurfDepthRecord = record
    Cell: TCellLocation;
    EvapotranspirationSurface: double;
    EvapotranspirationDepth: double;
    DepthFractions: array of double;
    EtFractions: array of double;
    StartingTime: double;
    EndingTime: double;
    EvapotranspirationSurfaceAnnotation: string;
    EvapotranspirationDepthAnnotation: string;
    DepthFractionAnnotations: array of string;
    EtFractionAnnotations: array of string;
    procedure Assign(const Item: TEtsSurfDepthRecord);
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  // @name is an array of @link(TEtsSurfDepthRecord)s.
  TEtsSurfDepthArray = array of TEtsSurfDepthRecord;

  TEtsSurfDepthStorage = class(TCustomBoundaryStorage)
  private
    FEtsSurfDepthArray: TEtsSurfDepthArray;
    function GetEtsSurfDepthArray: TEtsSurfDepthArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property EtsSurfDepthArray: TEtsSurfDepthArray read GetEtsSurfDepthArray;
  end;

  TStringCollection = class;

  TStringValueItem = class(TFormulaOrderedItem)
  private
    FValue: TFormulaObject;
    FObserver: TObserver;
    procedure SetValue(const Value: string);
    function StringCollection: TStringCollection;
    function GetValue: string;
    procedure RemoveSubscription(Sender: TObject; const AName: string);
    procedure RestoreSubscription(Sender: TObject; const AName: string);
//    procedure UpdateFormula(Value: string;
//      var FormulaObject: TFormulaObject);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function GetObserver(Index: Integer): TObserver; override;
//    procedure UpdateFormulaDependencies(OldFormula: string; var
//      NewFormula: string; Observer: TObserver; Compiler: TRbwParser); override;
    function GetScreenObject: TObject; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Value: string read GetValue write SetValue;
  end;

  TStringCollectionPurpose = (scpDepthFractions, scpEtFractions);
  TEtsSurfDepthCollection = class;

  TStringCollection = class(TOrderedCollection)
  private
    FPurpose: TStringCollectionPurpose;
    FScreenObject: TObject;
    FEtsSurfDepthCollection: TCollection;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Model: TBaseModel; ScreenObject: TObject;
      EtsSurfDepthCollection: TCollection);
    property Purpose: TStringCollectionPurpose read FPurpose write FPurpose;
  end;

  // @name represents a MODFLOW ET layer for one time interval.
  // @name is stored by @link(TEtsSurfDepthCollection).
  TEtsSurfDepthItem = class(TCustomModflowBoundaryItem)
  private
    FEvapotranspirationSurface: TFormulaObject;
    FEvapotranspirationDepth: TFormulaObject;
    FDepthFractions: TStringCollection;
    FEtFractions: TStringCollection;
    // See @link(EvapotranspirationSurface).
    procedure SetEvapotranspirationSurface(const Value: string);
    procedure SetEvapotranspirationDepth(const Value: string);
    procedure SetDepthFractions(const Value: TStringCollection);
    procedure SetEtFractions(const Value: TStringCollection);
    function GetEvapotranspirationDepth: string;
    function GetEvapotranspirationSurface: string;
  protected
    procedure RemoveFormulaObjects; override;
    procedure CreateFormulaObjects; override;
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    // @name checks whether AnotherItem is the same as the current @classname.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function BoundaryFormulaCount: integer; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    // @name is the formula used to set the recharge rate
    // or the recharge rate multiplier of this boundary.
    property EvapotranspirationSurface: string read GetEvapotranspirationSurface
      write SetEvapotranspirationSurface;
    property EvapotranspirationDepth: string read GetEvapotranspirationDepth
      write SetEvapotranspirationDepth;
    property EtFractions: TStringCollection read FEtFractions
      write SetEtFractions;
    property DepthFractions: TStringCollection read FDepthFractions
      write SetDepthFractions;
  end;

  TEtsSurfDepthTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the evapotranspiration surface for a series of
    // cells over a series of time intervals.
    FEvapotranspirationSurfaceData: TModflowTimeList;
    FEvapotranspirationDepthData: TModflowTimeList;
    FListOfEtFractionLists: TList;
    FListOfDepthFractionLists: TList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  TEtsSurfDepthCollection = class(TCustomMF_ArrayBoundColl)
  private
    FTimeListCount: integer;
    procedure InvalidateEtFractions(Sender: TObject);
    procedure InvalidateDepthFractions(Sender: TObject);
    procedure InvalidateEtSurface(Sender: TObject);
    procedure InvalidateEtDepth(Sender: TObject);
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    function GetTimeList(Index: integer; AModel: TBaseModel): TModflowTimeList; override;
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
    // the @link(TEtsSurfDepthStorage.EtsSurfDepthArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // It also sets the length of each member of  to
    // @link(TEtsPackageSelection.SegmentCount
    // TPhastModel.ModflowPackages.EtsPackage.SegmentCount).
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
  public
    procedure Assign(Source: TPersistent); override;
    function TimeListCount(AModel: TBaseModel): integer; override;
  end;

  TEtsLayerTimeListLink = class(TEvtLayerTimeListLink)
  protected
    procedure CreateTimeLists; override;
  end;

  TEtsLayerCollection = class(TEvtLayerCollection)
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TEtsSurfDepth_Cell = class(TValueCell)
  private
    Values: TEtsSurfDepthRecord;
    StressPeriod: integer;
    function GetEvapotranspirationSurface: double;
    function GetEvapotranspirationDepth: double;
    function GetDepthFractions(const Index: integer): double;
    function GetEtFractions(const Index: integer): double;
    function GetDepthFractionAnnotations(const Index: integer): string;
    function GetEtFractionAnnotations(const Index: integer): string;
    function GetEvapotranspirationDepthAnnotation: string;
    function GetEvapotranspirationSurfaceAnnotation: string;
//    function GetTimeSeriesName: string;
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
    property EvapotranspirationSurface: double read GetEvapotranspirationSurface;
    property EvapotranspirationDepth: double read GetEvapotranspirationDepth;
    property DepthFractions[const Index: integer]: double read GetDepthFractions;
    property EtFractions[const Index: integer]: double read GetEtFractions;
    property EvapotranspirationSurfaceAnnotation: string read GetEvapotranspirationSurfaceAnnotation;
    property EvapotranspirationDepthAnnotation: string read GetEvapotranspirationDepthAnnotation;
    property DepthFractionAnnotations[const Index: integer]: string read GetDepthFractionAnnotations;
    property EtFractionAnnotations[const Index: integer]: string read GetEtFractionAnnotations;
//    property TimeSeriesName: string read GetTimeSeriesName;
  end;

  TEtsTimeListLink = class(TEvtTimeListLink)
  protected
    procedure CreateTimeLists; override;
  end;

  TEtsCollection = class(TEvtCollection)
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  // @name represents the MODFLOW Evapotranspiration boundaries associated with
  // a single @link(TScreenObject).
  //
  // @seealso(TEvtCollection)
  // @seealso(TEtsSurfDepthCollection)
  TEtsBoundary = class(TModflowParamBoundary)
  private
    FEvapotranspirationLayers: TEtsLayerCollection;
    FEvtSurfDepthCollection: TEtsSurfDepthCollection;
    FNonParameterColumns: integer;
//    FInterp: TMf6InterpolationMethods;
    FCurrentParameter: TModflowTransientListParameter;
    procedure SetEvapotranspirationLayers(const Value: TEtsLayerCollection);
    procedure SetEvtSurfDepthCollection(const Value: TEtsSurfDepthCollection);
    function GetTimeVaryingEvapotranspirationLayers: boolean;
    procedure AssignEvapotranspirationLayerCells(BoundaryStorage: TEvtLayerStorage;
      ValueTimeList: TList);
    procedure AssignSurfaceDepthCells(AModel: TBaseModel;
      BoundaryStorage: TEtsSurfDepthStorage; ValueTimeList: TList);
//    procedure SetInterp(const Value: TMf6InterpolationMethods);
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TEvt_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    // See @link(TModflowParamBoundary.ModflowParamItemClass
    // TModflowParamBoundary.ModflowParamItemClass).
    class function ModflowParamItemClass: TModflowParamItemClass; override;
    function ParameterType: TParameterType; override;
  public
    procedure Assign(Source: TPersistent);override;

    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    Destructor Destroy; override;
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TEvtStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW Evapotranspiration parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TEvtStorage) in @link(TCustomMF_BoundColl.Boundaries
    // Param.Param.Boundaries)
    // Those represent parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel); override;
    function Used: boolean; override;
    procedure EvaluateArrayBoundaries(AModel: TBaseModel); override;
    function NonParameterColumns: integer; override;
    property TimeVaryingEvapotranspirationLayers: boolean
      read GetTimeVaryingEvapotranspirationLayers;
    procedure GetEvapotranspirationLayerCells(LayerTimeList: TList; AModel: TBaseModel);
    procedure GetEvapotranspirationSurfaceDepthCells(LayerTimeList: TList; AModel: TBaseModel);
    procedure InvalidateDisplay; override;
    procedure Clear; override;
  published
    property EvapotranspirationLayers: TEtsLayerCollection
      read FEvapotranspirationLayers write SetEvapotranspirationLayers;
    property EtsSurfDepthCollection: TEtsSurfDepthCollection
      read FEvtSurfDepthCollection write SetEvtSurfDepthCollection;
    property Interp;
  end;

procedure StringValueRemoveSubscription(Sender: TObject; Subject: TObject;
  const AName: string);

procedure StringValueRestoreSubscription(Sender: TObject; Subject: TObject;
  const AName: string);

resourcestring
  StrEvapotranspirationD_ETS = 'Evapotranspiration depth in the ETS package ' +
  'is less than zero';

implementation

uses ScreenObjectUnit, PhastModelUnit, ModflowTimeUnit, TempFiles,
  frmGoPhastUnit, frmErrorsAndWarningsUnit,
  ModflowTimeSeriesUnit;

resourcestring
  StrFractionalRateS = 'Fractional rate %s';

const
  SurfacePosition = 0;
  DepthPosition = 1;

{ TEtsBoundary }

procedure TEtsBoundary.Assign(Source: TPersistent);
var
  SourceBoundary: TEtsBoundary;
  EvtBoundary: TEvtBoundary;
begin
  if Source is TEtsBoundary then
  begin
    SourceBoundary := TEtsBoundary(Source);
    EvapotranspirationLayers := SourceBoundary.EvapotranspirationLayers;
//    TimeVaryingEvapotranspirationLayers := SourceBoundary.TimeVaryingEvapotranspirationLayers;
    EtsSurfDepthCollection := SourceBoundary.EtsSurfDepthCollection;
    FNonParameterColumns := SourceBoundary.NonParameterColumns;
  end;
  if Source is TEvtBoundary then
  begin
    EvtBoundary := TEvtBoundary(Source);
    EvapotranspirationLayers.Assign(EvtBoundary.EvapotranspirationLayers);
//    TimeVaryingEvapotranspirationLayers := EvtBoundary.TimeVaryingEvapotranspirationLayers;
    EtsSurfDepthCollection.Assign(EvtBoundary.EvtSurfDepthCollection);
    FNonParameterColumns := EvtBoundary.NonParameterColumns;
  end;
  inherited;
end;

procedure TEtsBoundary.AssignEvapotranspirationLayerCells(BoundaryStorage: TEvtLayerStorage;
  ValueTimeList: TList);
var
  Cell: TEvapotranspirationLayerCell;
  BoundaryValues: TEvtLayerRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TEvtLayerStorage;
begin
  LocalBoundaryStorage := BoundaryStorage;// as TEvtStorage;
  for TimeIndex := 0 to
    (ParentModel as TPhastModel).ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TEvapotranspirationLayerCell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := (ParentModel as TPhastModel).ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime <= LocalBoundaryStorage.EndingTime) then
    begin
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.EvtLayerArray) - 1 do
      begin
//        Cells.Cached := False;
        BoundaryValues := LocalBoundaryStorage.EvtLayerArray[BoundaryIndex];
        Cell := TEvapotranspirationLayerCell.Create;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

procedure TEtsBoundary.AssignSurfaceDepthCells(AModel: TBaseModel;
  BoundaryStorage: TEtsSurfDepthStorage; ValueTimeList: TList);
var
  Cell: TEtsSurfDepth_Cell;
  BoundaryValues: TEtsSurfDepthRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TEtsSurfDepthStorage;
begin
  LocalBoundaryStorage := BoundaryStorage;
  for TimeIndex := 0 to
    (ParentModel as TPhastModel).ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TEtsSurfDepth_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := (ParentModel as TPhastModel).ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime <= LocalBoundaryStorage.EndingTime) then
    begin
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.EtsSurfDepthArray) - 1 do
      begin
//        Cells.Cached := False;
        BoundaryValues := LocalBoundaryStorage.EtsSurfDepthArray[BoundaryIndex];
        Cell := TEtsSurfDepth_Cell.Create;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values.Assign(BoundaryValues);
        if BoundaryValues.EvapotranspirationDepth <= 0 then
        begin
          frmErrorsAndWarnings.AddError(AModel, StrEvapotranspirationD_ETS,
            Format(StrSP_Lay_Row_Col,
            [TimeIndex+1, Cell.Layer+1, Cell.Row+1, Cell.Column + 1]));
        end;
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

procedure TEtsBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TEvt_Cell;
  BoundaryValues: TEvtRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TEvtStorage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TEvtStorage;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
//      Cells.CheckRestore;
    end
    else
    begin
      Cells := TValueCellList.Create(TEvt_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.EvtArray) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.EvtArray)
      end;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.EvtArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.EvtArray[BoundaryIndex];
        Cell := TEvt_Cell.Create;
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

class function TEtsBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TEtsCollection;
end;

procedure TEtsBoundary.Clear;
begin
  inherited;
  EvapotranspirationLayers.Clear;
  EtsSurfDepthCollection.Clear;
end;

constructor TEtsBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited Create(Model, ScreenObject);
  FEvapotranspirationLayers := TEtsLayerCollection.Create(self, Model, ScreenObject);
  FEvtSurfDepthCollection := TEtsSurfDepthCollection.Create(self, Model, ScreenObject);
end;

destructor TEtsBoundary.Destroy;
begin
  FEvtSurfDepthCollection.Free;
  FEvapotranspirationLayers.Free;
  inherited;
end;

procedure TEtsBoundary.EvaluateArrayBoundaries(AModel: TBaseModel);
begin
  inherited;
  EtsSurfDepthCollection.EvaluateArrayBoundaries(AModel);
  if (AModel as TCustomModel).
    ModflowPackages.EtsPackage.TimeVaryingLayers then
  begin
    EvapotranspirationLayers.EvaluateArrayBoundaries(AModel);
  end;
end;

procedure TEtsBoundary.GetEvapotranspirationLayerCells(LayerTimeList: TList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TEvtLayerStorage;
begin
  if not (ParentModel as TPhastModel).ModflowPackages.
    EtsPackage.TimeVaryingLayers then
  begin
    Exit;
  end;
  for ValueIndex := 0 to EvapotranspirationLayers.Count - 1 do
  begin
    if ValueIndex < EvapotranspirationLayers.BoundaryCount[AModel] then
    begin
      BoundaryStorage := EvapotranspirationLayers.Boundaries[ValueIndex, AModel] as TEvtLayerStorage;
      AssignEvapotranspirationLayerCells(BoundaryStorage, LayerTimeList);
    end;
  end;
end;

procedure TEtsBoundary.GetEvapotranspirationSurfaceDepthCells(
  LayerTimeList: TList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TEtsSurfDepthStorage;
begin
  for ValueIndex := 0 to EtsSurfDepthCollection.Count - 1 do
  begin
    if ValueIndex < EtsSurfDepthCollection.BoundaryCount[AModel] then
    begin
      BoundaryStorage := EtsSurfDepthCollection.Boundaries[ValueIndex, AModel] as TEtsSurfDepthStorage;
      AssignSurfaceDepthCells(AModel, BoundaryStorage, LayerTimeList);
    end;
  end;
end;

procedure TEtsBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
const
  NoData = 3.0E30;
var
  ValueIndex: Integer;
  BoundaryStorage: TEvtStorage;
  ParamIndex: Integer;
  Param: TModflowParamItem;
  Times: TList;
  Position: integer;
  ParamName: string;
  Model: TPhastModel;
  BoundaryList: TList;
  StressPeriods: TModflowStressPeriods;
  StartTime: Double;
  EndTime: Double;
  TimeCount: Integer;
  ItemIndex: Integer;
  TimeSeriesList: TTimeSeriesList;
  TimeSeries: TTimeSeries;
  SeriesIndex: Integer;
  InitialTime: Double;
begin
  FCurrentParameter := nil;
  EvaluateArrayBoundaries(AModel);
  Model := ParentModel as TPhastModel;
  if Model.ModflowTransientParameters.CountParam(ParameterType) = 0 then
  begin
    for ValueIndex := 0 to Values.Count - 1 do
    begin
      if ValueIndex < Values.BoundaryCount[AModel] then
      begin
        BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TEvtStorage;
        AssignCells(BoundaryStorage, ValueTimeList, AModel);
      end;
    end;
  end
  else
  begin
    for ParamIndex := 0 to Parameters.Count - 1 do
    begin
      Param := Parameters[ParamIndex];
      ParamName := Param.Param.ParamName;
      if Model.ModelSelection = msModflow2015 then
      begin
        FCurrentParameter := Model.ModflowTransientParameters.GetParamByName(ParamName);
      end
      else
      begin
        FCurrentParameter := nil;
      end;
      Position := ParamList.IndexOf(ParamName);
      if Position < 0 then
      begin
        Times := TObjectList.Create;
        ParamList.AddObject(ParamName, Times);
      end
      else
      begin
        Times := ParamList.Objects[Position] as TList;
      end;

      if FCurrentParameter <> nil then
      begin
        BoundaryList := Param.Param.BoundaryList[AModel];
        StressPeriods := (AModel as TCustomModel).ModflowFullStressPeriods;
        StartTime := StressPeriods.First.StartTime;
        EndTime := StressPeriods.Last.EndTime;
        TimeCount := BoundaryList.Count;
        for ItemIndex := 0 to BoundaryList.Count - 1 do
        begin
          BoundaryStorage := BoundaryList[ItemIndex];
          if BoundaryStorage.StartingTime > StartTime then
          begin
            Inc(TimeCount);
          end;
          StartTime := BoundaryStorage.EndingTime;
        end;
        BoundaryStorage := BoundaryList.Last;
        if BoundaryStorage.EndingTime <= EndTime then
        begin
          Inc(TimeCount);
        end;

        TimeSeriesList := FCurrentParameter.TimeSeriesList;
        TimeSeries := TTimeSeries.Create;
        TimeSeriesList.Add(TimeSeries);
        TimeSeries.SeriesCount := Length(BoundaryStorage.EvtArray);
        TimeSeries.TimeCount := TimeCount;
        TimeSeries.ParameterName := FCurrentParameter.ParameterName;
        TimeSeries.ObjectName := (ScreenObject as TScreenObject).Name;
        for SeriesIndex := 0 to Length(BoundaryStorage.EvtArray) - 1 do
        begin
          TimeSeries.SeriesNames[SeriesIndex] :=
            Format('%0:s_%1d_%2:d', [TimeSeries.ParameterName,
            TimeSeriesList.Count, SeriesIndex+1]);
          TimeSeries.InterpolationMethods[SeriesIndex] := Interp;
          TimeSeries.ScaleFactors[SeriesIndex] := FCurrentParameter.Value;
        end;

        TimeCount := 0;
        StartTime := StressPeriods.First.StartTime;
        InitialTime := StartTime;
        for ItemIndex := 0 to BoundaryList.Count - 1 do
        begin
          BoundaryStorage := BoundaryList[ItemIndex];
          if BoundaryStorage.StartingTime > StartTime then
          begin
            TimeSeries.Times[TimeCount] := StartTime - InitialTime;
            for SeriesIndex := 0 to Length(BoundaryStorage.EvtArray) - 1 do
            begin
              if ItemIndex > 0 then
              begin
                TimeSeries.Values[SeriesIndex,TimeCount] := NoData;
              end
              else
              begin
                TimeSeries.Values[SeriesIndex,TimeCount] :=
                  BoundaryStorage.EvtArray[SeriesIndex].EvapotranspirationRate;
              end;
            end;
            Inc(TimeCount);
          end;
          TimeSeries.Times[TimeCount] := BoundaryStorage.StartingTime - InitialTime;
          for SeriesIndex := 0 to Length(BoundaryStorage.EvtArray) - 1 do
          begin
            TimeSeries.Values[SeriesIndex,TimeCount] :=
              BoundaryStorage.EvtArray[SeriesIndex].EvapotranspirationRate;
            BoundaryStorage.EvtArray[SeriesIndex].TimeSeriesName :=
              TimeSeries.SeriesNames[SeriesIndex];
          end;
          StartTime := BoundaryStorage.EndingTime;
          Inc(TimeCount);
        end;
        BoundaryStorage := BoundaryList.Last;
        if BoundaryStorage.EndingTime <= EndTime then
        begin
          TimeSeries.Times[TimeCount] := EndTime - InitialTime;
          for SeriesIndex := 0 to Length(BoundaryStorage.EvtArray) - 1 do
          begin
            TimeSeries.Values[SeriesIndex,TimeCount] :=
              BoundaryStorage.EvtArray[SeriesIndex].EvapotranspirationRate;
          end;
        end;
      end;

      for ValueIndex := 0 to Param.Param.Count - 1 do
      begin
        if ValueIndex < Param.Param.BoundaryCount[AModel] then
        begin
          BoundaryStorage := Param.Param.Boundaries[ValueIndex, AModel] as TEvtStorage;
          AssignCells(BoundaryStorage, Times, AModel);
        end;
      end;
    end;
  end;
  ClearBoundaries(AModel);
end;

function TEtsBoundary.GetTimeVaryingEvapotranspirationLayers: boolean;
begin
  if ParentModel = nil then
  begin
    result := frmGoPhast.PhastModel.ModflowPackages.
      EtsPackage.TimeVaryingLayers;
  end
  else
  begin
    result := (ParentModel as TPhastModel).ModflowPackages.
      EtsPackage.TimeVaryingLayers;
  end;
end;

procedure TEtsBoundary.InvalidateDisplay;
var
  LocalModel: TPhastModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    LocalModel := ParentModel as TPhastModel;
    LocalModel.InvalidateMfEtsEvapRate(self);
    LocalModel.InvalidateMfEtsEvapSurface(self);
    LocalModel.InvalidateMfEtsEvapDepth(self);
    LocalModel.InvalidateMfEtsEvapLayer(self);
    LocalModel.InvalidateEtsDepthFractions(self);
    LocalModel.InvalidateEtsRateFractions(self);
  end;
end;

class function TEtsBoundary.ModflowParamItemClass: TModflowParamItemClass;
begin
  result := TEvtParamItem;
end;

function TEtsBoundary.NonParameterColumns: integer;
begin
  if ParentModel = nil then
  begin
    result := inherited NonParameterColumns + 2
      + (frmGoPhast.PhastModel.ModflowPackages.EtsPackage.SegmentCount-1) * 2;
    if TimeVaryingEvapotranspirationLayers then
    begin
      result := result + EvapotranspirationLayers.TimeListCount(frmGoPhast.PhastModel);
    end;
  end
  else
  begin
    result := inherited NonParameterColumns + 2
      + ((ParentModel as TPhastModel).ModflowPackages.EtsPackage.SegmentCount-1) * 2;
    if TimeVaryingEvapotranspirationLayers then
    begin
      result := result + EvapotranspirationLayers.TimeListCount(ParentModel);
    end;
  end;
end;

function TEtsBoundary.ParameterType: TParameterType;
begin
  result := ptETS;
end;

procedure TEtsBoundary.SetEvapotranspirationLayers(const Value: TEtsLayerCollection);
begin
  FEvapotranspirationLayers.Assign(Value);
end;

procedure TEtsBoundary.SetEvtSurfDepthCollection(
  const Value: TEtsSurfDepthCollection);
begin
  FEvtSurfDepthCollection.Assign(Value);
end;


//procedure TEtsBoundary.SetInterp(const Value: TMf6InterpolationMethods);
//begin
//  if FInterp <> Value then
//  begin
//    InvalidateModel;
//    FInterp := Value;
//  end;
//end;

function TEtsBoundary.Used: boolean;
var
  LocalModel: TPhastModel;
  ParamIndex: Integer;
  Param: TModflowTransientListParameter;
begin
  if ParentModel <> nil then
  begin
    LocalModel := ParentModel as TPhastModel;
    result := LocalModel.ModflowPackages.EtsPackage.TimeVaryingLayers
      and EvapotranspirationLayers.Used;
  end
  else
  begin
    result := EvapotranspirationLayers.Used;
  end;
  if result then Exit;
  result := EtsSurfDepthCollection.Used;
  if result then Exit;
  result := inherited Used;
  if result and (ParentModel <> nil) then
  begin
    LocalModel := ParentModel as TPhastModel;
    if csLoading in LocalModel.ComponentState then
    begin
      Exit;
    end;
    for ParamIndex := 0 to LocalModel.ModflowTransientParameters.Count - 1 do
    begin
      Param := LocalModel.ModflowTransientParameters[ParamIndex];
      if Param.ParameterType = ptETS then
      begin
        result := Parameters.Used;
        Exit;
      end;
    end;
    result := Values.Used;
  end;
end;

{ TEtsSurfDepthItem }

procedure TEtsSurfDepthItem.Assign(Source: TPersistent);
var
  SourceItem: TEtsSurfDepthItem;
  EvtItem: TEvtSurfDepthItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TEtsSurfDepthItem then
  begin
    SourceItem := TEtsSurfDepthItem(Source);
    EvapotranspirationSurface := SourceItem.EvapotranspirationSurface;
    EvapotranspirationDepth := SourceItem.EvapotranspirationDepth;
    EtFractions := SourceItem.EtFractions;
    DepthFractions := SourceItem.DepthFractions;
  end
  else if Source is TEvtSurfDepthItem then
  begin
    EvtItem := TEvtSurfDepthItem(Source);
    EvapotranspirationSurface := EvtItem.EvapotranspirationSurface;
    EvapotranspirationDepth := EvtItem.EvapotranspirationDepth;
  end;
  inherited;
end;

procedure TEtsSurfDepthItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TEtsSurfDepthCollection;
  SurfaceObserver: TObserver;
  DepthObserver: TObserver;
begin
  ParentCollection := Collection as TEtsSurfDepthCollection;
  SurfaceObserver := FObserverList[SurfacePosition];
  SurfaceObserver.OnUpToDateSet := ParentCollection.InvalidateEtSurface;
  DepthObserver := FObserverList[DepthPosition];
  DepthObserver.OnUpToDateSet := ParentCollection.InvalidateEtDepth;
end;

function TEtsSurfDepthItem.BoundaryFormulaCount: integer;
begin
  result := 2;
  if (EtFractions <> nil) and (DepthFractions <> nil) then
  begin
    result := result + EtFractions.Count + DepthFractions.Count;
  end;

end;

constructor TEtsSurfDepthItem.Create(Collection: TCollection);
var
  Model: TBaseModel;
begin
  inherited;
  Model := (Collection as TOrderedCollection).Model;
  FEtFractions := TStringCollection.Create(Model, ScreenObject, Collection);
  FEtFractions.Purpose := scpEtFractions;
  FDepthFractions := TStringCollection.Create(Model, ScreenObject, Collection);
  FDepthFractions.Purpose := scpDepthFractions;
end;

procedure TEtsSurfDepthItem.CreateFormulaObjects;
begin
  FEvapotranspirationSurface := CreateFormulaObject(dsoTop);
  FEvapotranspirationDepth := CreateFormulaObject(dsoTop);
end;

destructor TEtsSurfDepthItem.Destroy;
begin
  EvapotranspirationSurface := '0';
  EvapotranspirationDepth := '0';
  FEtFractions.Free;
  FDepthFractions.Free;
  inherited;
end;

function TEtsSurfDepthItem.GetBoundaryFormula(Index: integer): string;
var
  Item: TStringValueItem;
  Collection: TStringCollection;
begin
  case Index of
    SurfacePosition: result := EvapotranspirationSurface;
    DepthPosition: result := EvapotranspirationDepth;
    else
      begin
        Dec(Index, 2);
        if Odd(Index) then
        begin
          Collection := EtFractions;
        end
        else
        begin
          Collection := DepthFractions;
        end;
        Index := Index div 2;
        while Collection.Count <= Index do
        begin
          Collection.Add;
        end;
        Item := Collection.Items[Index] as TStringValueItem;
        result := Item.Value;
      end;
  end;
end;

function TEtsSurfDepthItem.GetEvapotranspirationDepth: string;
begin
  Result := FEvapotranspirationDepth.Formula;
  ResetItemObserver(DepthPosition);
end;

function TEtsSurfDepthItem.GetEvapotranspirationSurface: string;
begin
  Result := FEvapotranspirationSurface.Formula;
  ResetItemObserver(SurfacePosition);
end;

procedure TEtsSurfDepthItem.GetPropertyObserver(Sender: TObject; List: TList);
var
  Index: integer;
  Item: TStringValueItem;
begin
  if Sender = FEvapotranspirationSurface then
  begin
    List.Add(FObserverList[SurfacePosition]);
  end;
  if Sender = FEvapotranspirationDepth then
  begin
    List.Add(FObserverList[DepthPosition]);
  end;

  for Index := 0 to EtFractions.Count - 1 do
  begin
    Item := EtFractions.Items[Index] as TStringValueItem;
    if Item.FValue = Sender then
    begin
      List.Add(Item.FObserver);
    end;
  end;
  for Index := 0 to DepthFractions.Count - 1 do
  begin
    Item := DepthFractions.Items[Index] as TStringValueItem;
    if Item.FValue = Sender then
    begin
      List.Add(Item.FObserver);
    end;
  end;
end;

function TEtsSurfDepthItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TEtsSurfDepthItem;
begin
  result := (AnotherItem is TEtsSurfDepthItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TEtsSurfDepthItem(AnotherItem);
    result :=
      (Item.EvapotranspirationSurface = EvapotranspirationSurface)
      and (Item.EvapotranspirationDepth = EvapotranspirationDepth)
      and Item.EtFractions.IsSame(EtFractions)
      and Item.DepthFractions.IsSame(DepthFractions)
  end;
end;

procedure TEtsSurfDepthItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FEvapotranspirationDepth,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FEvapotranspirationSurface,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TEtsSurfDepthItem.SetBoundaryFormula(Index: integer;
  const Value: string);
var
  Item: TStringValueItem;
  Collection: TStringCollection;
begin
  case Index of
    0: EvapotranspirationSurface := Value;
    1: EvapotranspirationDepth := Value;
    else
      begin
        if Odd(Index) then
        begin
          Collection := EtFractions;
        end
        else
        begin
          Collection := DepthFractions;
        end;
        Index := (Index -2) div 2;
        while Index >= Collection.Count do
        begin
          Collection.Add;
        end;
        Item := Collection.Items[Index] as TStringValueItem;
        Item.Value := Value;
      end;
  end;
end;

procedure TEtsSurfDepthItem.SetDepthFractions(const Value: TStringCollection);
begin
  FDepthFractions.Assign(Value);
end;

procedure TEtsSurfDepthItem.SetEtFractions(const Value: TStringCollection);
begin
  FEtFractions.Assign(Value);
end;

procedure TEtsSurfDepthItem.SetEvapotranspirationDepth(const Value: string);
begin
  UpdateFormula(Value, DepthPosition, FEvapotranspirationDepth);
end;

procedure TEtsSurfDepthItem.SetEvapotranspirationSurface(const Value: string);
begin
  UpdateFormula(Value, SurfacePosition, FEvapotranspirationSurface);
end;

{ TEtsSurfDepthCollection }

procedure TEtsSurfDepthCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TEtsSurfDepthStorage.Create(AModel));
end;

procedure TEtsSurfDepthCollection.Assign(Source: TPersistent);
begin
  if Source is TEtsSurfDepthCollection then
  begin
    FTimeListCount := TEtsSurfDepthCollection(Source).TimeListCount(Model);
  end
  else if Source is TEvtSurfDepthCollection then
  begin
    FTimeListCount := TEvtSurfDepthCollection(Source).TimeListCount(Model);
  end;
  inherited;
end;

procedure TEtsSurfDepthCollection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel);
var
  EvapotranspirationSurfaceArray: TDataArray;
  EvapotranspirationDepthArray: TDataArray;
  Boundary: TEtsSurfDepthStorage;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  BoundaryIndex: Integer;
  SegmentCount: integer;
  SegmentIndex: integer;
  FractionalDepthArray: TDataArray;
  FractionalRateArray: TDataArray;
  LocalModel: TCustomModel;
  LayerMin: Integer;
  RowMin: Integer;
  ColMin: Integer;
  LayerMax: Integer;
  RowMax: Integer;
  ColMax: Integer;
begin
  LocalModel := AModel as TCustomModel;
  SegmentCount := LocalModel.
    ModflowPackages.EtsPackage.SegmentCount;
  BoundaryIndex := 0;
  EvapotranspirationSurfaceArray := DataSets[0];
  EvapotranspirationDepthArray := DataSets[1];
  Boundary := Boundaries[ItemIndex, AModel] as TEtsSurfDepthStorage;
  EvapotranspirationSurfaceArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
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
            if EvapotranspirationSurfaceArray.IsValue[
              LayerIndex, RowIndex, ColIndex] then
            begin
              Assert(EvapotranspirationDepthArray.IsValue[
                LayerIndex, RowIndex, ColIndex]);
              with Boundary.EtsSurfDepthArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                EvapotranspirationSurface := EvapotranspirationSurfaceArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                EvapotranspirationSurfaceAnnotation := EvapotranspirationSurfaceArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
                EvapotranspirationDepth := EvapotranspirationDepthArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                EvapotranspirationDepthAnnotation := EvapotranspirationDepthArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];

                for SegmentIndex := 1 to SegmentCount - 1 do
                begin
                  FractionalDepthArray := DataSets[SegmentIndex*2];
                  Assert(FractionalDepthArray.IsValue[LayerIndex, RowIndex, ColIndex]);
                  DepthFractions[SegmentIndex-1] := FractionalDepthArray.
                    RealData[LayerIndex, RowIndex, ColIndex];
                  DepthFractionAnnotations[SegmentIndex-1] := FractionalDepthArray.
                    Annotation[LayerIndex, RowIndex, ColIndex];

                  FractionalRateArray := DataSets[SegmentIndex*2+1];
                  Assert(FractionalRateArray.IsValue[LayerIndex, RowIndex, ColIndex]);
                  EtFractions[SegmentIndex-1] := FractionalRateArray.
                    RealData[LayerIndex, RowIndex, ColIndex];
                  EtFractionAnnotations[SegmentIndex-1] := FractionalRateArray.
                    Annotation[LayerIndex, RowIndex, ColIndex];
                end;
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  EvapotranspirationSurfaceArray.CacheData;
  EvapotranspirationDepthArray.CacheData;
  for SegmentIndex := 1 to SegmentCount - 1 do
  begin
    FractionalDepthArray := DataSets[SegmentIndex*2];
    FractionalRateArray := DataSets[SegmentIndex*2+1];

    FractionalDepthArray.CacheData;
    FractionalRateArray.CacheData;
  end;
  Boundary.CacheData;
end;

function TEtsSurfDepthCollection.GetTimeList(Index: integer; AModel: TBaseModel): TModflowTimeList;
var
  TimeList: TModflowTimeList;
  Count: integer;
  FractionIndex: string;
  Link: TEtsSurfDepthTimeListLink;
  ListOfEtFractionLists: TList;
  ListOfDepthFractionLists: TList;
begin
  Link := TimeListLink.GetLink(AModel) as TEtsSurfDepthTimeListLink;
  ListOfEtFractionLists := Link.FListOfEtFractionLists;
  ListOfDepthFractionLists := Link.FListOfDepthFractionLists;
  While Index >= inherited TimeListCount(AModel) do
  begin
    TimeList := TModflowTimeList.Create(AModel, ScreenObject);
    Count := inherited TimeListCount(AModel);
    FractionIndex := IntToStr(Count div 2);
    if Odd(Count)  then
    begin
      TimeList.NonParamDescription := Format(StrFractionalRateS, [FractionIndex]);
      TimeList.ParamDescription := ' ' + LowerCase(TimeList.NonParamDescription);
      ListOfEtFractionLists.Add(TimeList);
      if Model <> nil then
      begin
        TimeList.OnInvalidate := (AModel as TCustomModel).InvalidateEtsRateFractions;
      end;
    end
    else
    begin
      TimeList.NonParamDescription := 'Fractional depth ' + FractionIndex;
      TimeList.ParamDescription := ' fractional depth ' + FractionIndex;
      ListOfDepthFractionLists.Add(TimeList);
      if Model <> nil then
      begin
        TimeList.OnInvalidate := (AModel as TCustomModel).InvalidateEtsDepthFractions;
      end;
    end;
    AddTimeList(TimeList, AModel);
  end;
  result := Inherited GetTimeList(index, AModel);
end;

function TEtsSurfDepthCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TEtsSurfDepthTimeListLink;
end;

procedure TEtsSurfDepthCollection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TEtsSurfDepthItem;
  Boundary: TEtsBoundary;
  ScreenObject: TScreenObject;
  SegmentIndex: Integer;
  FractionalDepthTimeList: TModflowTimeList;
  FractionalRateTimeList: TModflowTimeList;
  ALink: TEtsSurfDepthTimeListLink;
  EvapotranspirationSurfaceData: TModflowTimeList;
  EvapotranspirationDepthData: TModflowTimeList;
  ListOfEtFractionLists: TList;
  ListOfDepthFractionLists: TList;
begin
  Boundary := BoundaryGroup as TEtsBoundary;
  ScreenObject := Boundary.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TEtsSurfDepthItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.EvapotranspirationSurface;
  end;
  ALink := TimeListLink.GetLink(AModel) as TEtsSurfDepthTimeListLink;
  EvapotranspirationSurfaceData := ALink.FEvapotranspirationSurfaceData;
  EvapotranspirationSurfaceData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(EvapotranspirationSurfaceData.Count = Count);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TEtsSurfDepthItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.EvapotranspirationDepth;
  end;
  EvapotranspirationDepthData := ALink.FEvapotranspirationDepthData;
  EvapotranspirationDepthData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(EvapotranspirationDepthData.Count = Count);

  // assign fractional depths and rates.

  if Count > 0 then
  begin
    for SegmentIndex := 1 to (Model as TPhastModel).
      ModflowPackages.EtsPackage.SegmentCount - 1 do
    begin
      FractionalDepthTimeList := TimeLists[SegmentIndex*2, AModel];
      for Index := 0 to Count - 1 do
      begin
        Item := Items[Index] as TEtsSurfDepthItem;
        BoundaryValues[Index].Time := Item.StartTime;
        BoundaryValues[Index].Formula := (Item.DepthFractions.
          Items[SegmentIndex-1] as TStringValueItem).Value;
        if BoundaryValues[Index].Formula = '' then
        begin
          BoundaryValues[Index].Formula := '1';
        end;
      end;
      FractionalDepthTimeList.Initialize(BoundaryValues, ScreenObject, lctUse);
      Assert(FractionalDepthTimeList.Count = Count);

      FractionalRateTimeList := TimeLists[SegmentIndex*2+1, AModel];
      for Index := 0 to Count - 1 do
      begin
        Item := Items[Index] as TEtsSurfDepthItem;
        BoundaryValues[Index].Time := Item.StartTime;
        BoundaryValues[Index].Formula := (Item.EtFractions.
          Items[SegmentIndex-1] as TStringValueItem).Value;
        if BoundaryValues[Index].Formula = '' then
        begin
          BoundaryValues[Index].Formula := '0';
        end;
      end;
      FractionalRateTimeList.Initialize(BoundaryValues, ScreenObject, lctUse);
      Assert(FractionalRateTimeList.Count = Count);
    end;
  end;


  ClearBoundaries(AModel);
  SetBoundaryCapacity(EvapotranspirationSurfaceData.Count, AModel);
  for TimeIndex := 0 to EvapotranspirationSurfaceData.Count - 1 do
  begin
    AddBoundary(TEtsSurfDepthStorage.Create(AModel));
  end;
  ListOfTimeLists.Add(EvapotranspirationSurfaceData);
  ListOfTimeLists.Add(EvapotranspirationDepthData);
  ListOfEtFractionLists := ALink.FListOfEtFractionLists;
  ListOfDepthFractionLists := ALink.FListOfDepthFractionLists;
  if Count > 0 then
  begin
    for SegmentIndex := 1 to (Model as TPhastModel).
      ModflowPackages.EtsPackage.SegmentCount - 1 do
    begin
      FractionalDepthTimeList := ListOfDepthFractionLists[SegmentIndex-1];
      ListOfTimeLists.Add(FractionalDepthTimeList);
      FractionalRateTimeList := ListOfEtFractionLists[SegmentIndex-1];
      ListOfTimeLists.Add(FractionalRateTimeList);
    end;
  end;
end;

procedure TEtsSurfDepthCollection.InvalidateDepthFractions(Sender: TObject);
var
  Index: Integer;
  TimeList: TModflowTimeList;
  PhastModel: TPhastModel;
  Link: TEtsSurfDepthTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TEtsSurfDepthTimeListLink;
    for Index := 0 to Link.FListOfDepthFractionLists.Count - 1 do
    begin
      TimeList := Link.FListOfDepthFractionLists[Index];
      TimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TEtsSurfDepthTimeListLink;
      for Index := 0 to Link.FListOfDepthFractionLists.Count - 1 do
      begin
        TimeList := Link.FListOfDepthFractionLists[Index];
        TimeList.Invalidate;
      end;
    end;
  end;
end;

procedure TEtsSurfDepthCollection.InvalidateEtDepth(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TEtsSurfDepthTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TEtsSurfDepthTimeListLink;
    Link.FEvapotranspirationDepthData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TEtsSurfDepthTimeListLink;
      Link.FEvapotranspirationDepthData.Invalidate;
    end;
  end;
end;

procedure TEtsSurfDepthCollection.InvalidateEtFractions(Sender: TObject);
var
  Index: Integer;
  TimeList: TModflowTimeList;
  PhastModel: TPhastModel;
  Link: TEtsSurfDepthTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TEtsSurfDepthTimeListLink;
    for Index := 0 to Link.FListOfEtFractionLists.Count - 1 do
    begin
      TimeList := Link.FListOfEtFractionLists[Index];
      TimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TEtsSurfDepthTimeListLink;
      for Index := 0 to Link.FListOfEtFractionLists.Count - 1 do
      begin
        TimeList := Link.FListOfEtFractionLists[Index];
        TimeList.Invalidate;
      end;
    end;
  end;
end;

procedure TEtsSurfDepthCollection.InvalidateEtSurface(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TEtsSurfDepthTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TEtsSurfDepthTimeListLink;
    Link.FEvapotranspirationSurfaceData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TEtsSurfDepthTimeListLink;
      Link.FEvapotranspirationSurfaceData.Invalidate;
    end;
  end;
end;

class function TEtsSurfDepthCollection.ItemClass: TBoundaryItemClass;
begin
  result := TEtsSurfDepthItem;
end;

procedure TEtsSurfDepthCollection.SetBoundaryStartAndEndTime(
  BoundaryCount: Integer; Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
var
  EtsSurfDepthStorage : TEtsSurfDepthStorage;
  SegmentCount: integer;
  Index: integer;
begin
  EtsSurfDepthStorage := Boundaries[ItemIndex, AModel] as TEtsSurfDepthStorage;
  SetLength(EtsSurfDepthStorage.FEtsSurfDepthArray, BoundaryCount);
  SegmentCount := (Model as TPhastModel).
    ModflowPackages.EtsPackage.SegmentCount;
  for Index := 0 to BoundaryCount - 1 do
  begin
    SetLength(EtsSurfDepthStorage.EtsSurfDepthArray[Index].
      DepthFractions, SegmentCount-1);
    SetLength(EtsSurfDepthStorage.EtsSurfDepthArray[Index].
      EtFractions, SegmentCount-1);
    SetLength(EtsSurfDepthStorage.EtsSurfDepthArray[Index].
      DepthFractionAnnotations, SegmentCount-1);
    SetLength(EtsSurfDepthStorage.EtsSurfDepthArray[Index].
      EtFractionAnnotations, SegmentCount-1);
  end;
  inherited;

end;

function TEtsSurfDepthCollection.TimeListCount(AModel: TBaseModel): integer;
begin
  if Model = nil then
  begin
    if frmGoPhast.PhastModel <> nil then
    begin
      result := 2
        + (frmGoPhast.PhastModel.ModflowPackages.EtsPackage.SegmentCount-1) * 2;
    end
    else
    begin
      result := FTimeListCount;
    end;
  end
  else
  begin
    result := 2
      + ((Model as TPhastModel).ModflowPackages.EtsPackage.SegmentCount-1) * 2;
  end;
end;

{ TEtsSurfDepth_Cell }

procedure TEtsSurfDepth_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TEtsSurfDepth_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TEtsSurfDepth_Cell.GetDepthFractionAnnotations(
  const Index: integer): string;
begin
  result := Values.DepthFractionAnnotations[Index];
end;

function TEtsSurfDepth_Cell.GetDepthFractions(const Index: integer): double;
begin
  result := Values.DepthFractions[Index];
end;

function TEtsSurfDepth_Cell.GetEtFractionAnnotations(
  const Index: integer): string;
begin
  result := Values.EtFractionAnnotations[Index];
end;

function TEtsSurfDepth_Cell.GetEtFractions(const Index: integer): double;
begin
  result := Values.EtFractions[Index];
end;

function TEtsSurfDepth_Cell.GetEvapotranspirationDepth: double;
begin
  result := Values.EvapotranspirationDepth;
end;

function TEtsSurfDepth_Cell.GetEvapotranspirationDepthAnnotation: string;
begin
  result := Values.EvapotranspirationDepthAnnotation;
end;

function TEtsSurfDepth_Cell.GetEvapotranspirationSurface: double;
begin
  result := Values.EvapotranspirationSurface;
end;

function TEtsSurfDepth_Cell.GetEvapotranspirationSurfaceAnnotation: string;
begin
  result := Values.EvapotranspirationSurfaceAnnotation;
end;

function TEtsSurfDepth_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TEtsSurfDepth_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TEtsSurfDepth_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TEtsSurfDepth_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  case Index of
    0: result := EvapotranspirationSurfaceAnnotation;
    1: result := EvapotranspirationDepthAnnotation;
    else
      begin
        Dec(Index, 2);
        if Odd(Index) then
        begin
          result := EtFractionAnnotations[Index div 2];
        end
        else
        begin
          result := DepthFractionAnnotations[Index div 2];
        end;
      end;
  end;
end;

function TEtsSurfDepth_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  case Index of
    0: result := EvapotranspirationSurface;
    1: result := EvapotranspirationDepth;
    else
      begin
        Dec(Index, 2);
        if Odd(Index) then
        begin
          result := EtFractions[Index div 2];
        end
        else
        begin
          result := DepthFractions[Index div 2];
        end;
      end;
  end;
end;

function TEtsSurfDepth_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TEtsSurfDepth_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

//function TEtsSurfDepth_Cell.GetTimeSeriesName: string;
//begin
//  result := Values.TimeSeriesName;
//end;

procedure TEtsSurfDepth_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TEtsSurfDepth_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TEtsSurfDepth_Cell.SetColumn(const Value: integer);
begin
  Values.Cell.Column := Value;
end;

procedure TEtsSurfDepth_Cell.SetLayer(const Value: integer);
begin
  Values.Cell.Layer := Value;
end;

procedure TEtsSurfDepth_Cell.SetRow(const Value: integer);
begin
  Values.Cell.Row := Value;
end;

{ TEtsSurfDepthRecord }

procedure TEtsSurfDepthRecord.Assign(const Item: TEtsSurfDepthRecord);
begin
  self := Item;
  SetLength(DepthFractions, Length(DepthFractions));
  SetLength(EtFractions, Length(EtFractions));
  SetLength(DepthFractionAnnotations, Length(DepthFractionAnnotations));
  SetLength(EtFractionAnnotations, Length(EtFractionAnnotations));
end;

procedure TEtsSurfDepthRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
var
  Count: integer;
  Index: Integer;
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, EvapotranspirationSurface);
  WriteCompReal(Comp, EvapotranspirationDepth);
  Count := Length(DepthFractions);
  WriteCompInt(Comp, Count);
  for Index := 0 to Count - 1 do
  begin
    WriteCompReal(Comp, DepthFractions[Index]);
  end;
  for Index := 0 to Count - 1 do
  begin
    WriteCompReal(Comp, EtFractions[Index]);
  end;
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);

  WriteCompInt(Comp, Strings.IndexOf(EvapotranspirationSurfaceAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(EvapotranspirationDepthAnnotation));

//  WriteCompString(Comp, EvapotranspirationSurfaceAnnotation);
//  WriteCompString(Comp, EvapotranspirationDepthAnnotation);
  for Index := 0 to Count - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(DepthFractionAnnotations[Index]));
//    WriteCompString(Comp, DepthFractionAnnotations[Index]);
  end;
  for Index := 0 to Count - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(EtFractionAnnotations[Index]));
//    WriteCompString(Comp, EtFractionAnnotations[Index]);
  end;
end;

procedure TEtsSurfDepthRecord.RecordStrings(Strings: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  Count := Length(DepthFractions);
  Strings.Add(EvapotranspirationSurfaceAnnotation);
  Strings.Add(EvapotranspirationDepthAnnotation);
  for Index := 0 to Count - 1 do
  begin
    Strings.Add(DepthFractionAnnotations[Index]);
  end;
  for Index := 0 to Count - 1 do
  begin
    Strings.Add(EtFractionAnnotations[Index]);
  end;
end;

procedure TEtsSurfDepthRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
var
  Count: integer;
  Index: Integer;
begin
  Cell := ReadCompCell(Decomp);
  EvapotranspirationSurface := ReadCompReal(Decomp);
  EvapotranspirationDepth := ReadCompReal(Decomp);
  Count := ReadCompInt(Decomp);
  SetLength(DepthFractions, Count);
  for Index := 0 to Count - 1 do
  begin
    DepthFractions[Index] := ReadCompReal(Decomp);
  end;
  SetLength(EtFractions, Count);
  for Index := 0 to Count - 1 do
  begin
    EtFractions[Index] := ReadCompReal(Decomp);
  end;
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);

  EvapotranspirationSurfaceAnnotation := Annotations[ReadCompInt(Decomp)];
  EvapotranspirationDepthAnnotation := Annotations[ReadCompInt(Decomp)];

//  EvapotranspirationSurfaceAnnotation := ReadCompString(Decomp, Annotations);
//  EvapotranspirationDepthAnnotation := ReadCompString(Decomp, Annotations);
  SetLength(DepthFractionAnnotations, Count);
  for Index := 0 to Count - 1 do
  begin
    DepthFractionAnnotations[Index] := Annotations[ReadCompInt(Decomp)];
//    DepthFractionAnnotations[Index] := ReadCompString(Decomp, Annotations);
  end;
  SetLength(EtFractionAnnotations, Count);
  for Index := 0 to Count - 1 do
  begin
    EtFractionAnnotations[Index] := Annotations[ReadCompInt(Decomp)];
//    EtFractionAnnotations[Index] := ReadCompString(Decomp, Annotations);
  end;
end;

{ TStringValueItem }

procedure TStringValueItem.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if Source is TStringValueItem then
  begin
    Value := TStringValueItem(Source).Value;
  end;
  inherited;
end;

procedure StringValueRemoveSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TStringValueItem).RemoveSubscription(Sender, AName);
end;

procedure StringValueRestoreSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TStringValueItem).RestoreSubscription(Sender, AName);
end;

constructor TStringValueItem.Create(Collection: TCollection);
var
  SCollection: TStringCollection;
  EtsSurfDepth: TEtsSurfDepthCollection;
  LocalScreenObject: TScreenObject;
begin
  inherited;
  FObserver:= TObserver.Create(nil);
  SCollection := StringCollection;
  EtsSurfDepth := SCollection.FEtsSurfDepthCollection as TEtsSurfDepthCollection;
  case SCollection.Purpose of
    scpDepthFractions:
      begin
        FObserver.OnUpToDateSet := EtsSurfDepth.InvalidateDepthFractions;
      end;
    scpEtFractions:
      begin
        FObserver.OnUpToDateSet := EtsSurfDepth.InvalidateEtFractions;
      end;
    else
      Assert(False);
  end;
  LocalScreenObject := EtsSurfDepth.ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.TalksTo(FObserver);
  end;

  OnRemoveSubscription := StringValueRemoveSubscription;
  OnRestoreSubscription := StringValueRestoreSubscription;
  FValue := frmGoPhast.PhastModel.FormulaManager.Add;
  FValue.Parser := frmGoPhast.PhastModel.rpTopFormulaCompiler;
  FValue.AddSubscriptionEvents(StringValueRemoveSubscription,
  StringValueRestoreSubscription, self);

end;

destructor TStringValueItem.Destroy;
var
  LocalScreenObject: TScreenObject;
  SCollection: TStringCollection;
  EtsSurfDepth: TEtsSurfDepthCollection;
begin
  Value := '0';
  SCollection := StringCollection;
  EtsSurfDepth := SCollection.FEtsSurfDepthCollection as TEtsSurfDepthCollection;
  LocalScreenObject := EtsSurfDepth.ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.StopsTalkingTo(FObserver);
  end;
  frmGoPhast.PhastModel.FormulaManager.Remove(FValue,
    StringValueRemoveSubscription,
    StringValueRestoreSubscription, self);
  FObserver.Free;
  inherited;
end;

function TStringValueItem.GetObserver(Index: Integer): TObserver;
begin
  result := FObserver;
end;

function TStringValueItem.GetScreenObject: TObject;
begin
  result := StringCollection.FScreenObject;
end;

function TStringValueItem.GetValue: string;
begin
  Result := FValue.Formula;
  FObserver.UpToDate := True;
end;

function TStringValueItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  if AnotherItem is TStringValueItem then
  begin
    result := Value = TStringValueItem(AnotherItem).Value;
  end
  else
  begin
    result := false;
  end;
end;

procedure TStringValueItem.RemoveSubscription(Sender: TObject;
  const AName: string);
var
  DS: TObserver;
begin
  DS := frmGoPhast.PhastModel.GetObserverByName(AName);
  DS.StopsTalkingTo(FObserver);
end;

procedure TStringValueItem.RestoreSubscription(Sender: TObject;
  const AName: string);
var
  DS: TObserver;
begin
  DS := frmGoPhast.PhastModel.GetObserverByName(AName);
  DS.TalksTo(FObserver);
  FObserver.UpToDate := False;
end;

procedure TStringValueItem.SetValue(const Value: string);
var
  Dummy: Integer;
begin
  Dummy := 0;
  UpdateFormula(Value, Dummy, FValue);
end;

function TStringValueItem.StringCollection: TStringCollection;
begin
  result := Collection as TStringCollection;
end;

//procedure TStringValueItem.UpdateFormula(Value: string;
//  var FormulaObject: TFormulaObject);
//var
//  ParentModel: TPhastModel;
//  Compiler: TRbwParser;
//begin
//  if FormulaObject.Formula <> Value then
//  begin
//    ParentModel := Model as TPhastModel;
//    if ParentModel <> nil then
//    begin
//      Compiler := ParentModel.rpThreeDFormulaCompiler;
//      UpdateFormulaDependencies(FormulaObject.Formula, Value, FObserver, Compiler);
//    end;
//    InvalidateModel;
//    frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
//      FormulaObject, Value,
//      frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
//      StringValueRemoveSubscription,
//      StringValueRestoreSubscription, self);
//  end;
//end;

//procedure TStringValueItem.UpdateFormulaDependencies(OldFormula: string;
//  var NewFormula: string; Observer: TObserver; Compiler: TRbwParser);
//var
//  OldUses: TStringList;
//  NewUses: TStringList;
//  Position: Integer;
//  DS: TObserver;
//  ParentScreenObject: TScreenObject;
//  Index: integer;
//  procedure CompileFormula(var AFormula: string;
//    UsesList: TStringList);
//  begin
//    if AFormula <> '' then
//    begin
//      try
//        Compiler.Compile(AFormula);
//        UsesList.Assign(Compiler.CurrentExpression.VariablesUsed);
//      except on E: ERbwParserError do
//        begin
//        end;
//      end;
//    end;
//  end;
//begin
//  OldFormula := Trim(OldFormula);
//  NewFormula := Trim(NewFormula);
//  if OldFormula = NewFormula then
//  begin
//    Exit;
//  end;
//  ParentScreenObject := StringCollection.FScreenObject as TScreenObject;
//  if (ParentScreenObject = nil)
//    or not ParentScreenObject.CanInvalidateModel then
//  begin
//    Exit;
//  end;
//  OldUses := TStringList.Create;
//  NewUses := TStringList.Create;
//  try
//    CompileFormula(OldFormula, OldUses);
//    CompileFormula(NewFormula, NewUses);
//    for Index := OldUses.Count - 1 downto 0 do
//    begin
//      Position := NewUses.IndexOf(OldUses[Index]);
//      if Position >= 0 then
//      begin
//        OldUses.Delete(Index);
//        NewUses.Delete(Position);
//      end;
//    end;
//    for Index := 0 to OldUses.Count - 1 do
//    begin
//      DS := frmGoPhast.PhastModel.GetObserverByName(OldUses[Index]);
//      Assert(DS <> nil);
//      DS.StopsTalkingTo(Observer);
//    end;
//    for Index := 0 to NewUses.Count - 1 do
//    begin
//      DS := frmGoPhast.PhastModel.GetObserverByName(NewUses[Index]);
//      Assert(DS <> nil);
//      DS.TalksTo(Observer);
//    end;
//  finally
//    NewUses.Free;
//    OldUses.Free;
//  end;
//end;

{ TStringCollection }

procedure TStringCollection.Assign(Source: TPersistent);
begin
  if Source is TStringCollection then
  begin
    Purpose := TStringCollection(Source).Purpose;
  end;
  inherited;
end;

constructor TStringCollection.Create(Model: TBaseModel; ScreenObject: TObject;
  EtsSurfDepthCollection: TCollection);
begin
  inherited Create(TStringValueItem, Model);
  FScreenObject := ScreenObject;
  FEtsSurfDepthCollection := EtsSurfDepthCollection;
end;

{ TEtsLayerCollection }

function TEtsLayerCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TEtsLayerTimeListLink;
end;

{ TEtsCollection }

function TEtsCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TEtsTimeListLink;
end;

{ TEtsSurfDepthStorage }

procedure TEtsSurfDepthStorage.Clear;
begin
  SetLength(FEtsSurfDepthArray, 0);
  FCleared := True;
end;

procedure TEtsSurfDepthStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FEtsSurfDepthArray);
    for Index := 0 to Count - 1 do
    begin
      FEtsSurfDepthArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FEtsSurfDepthArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TEtsSurfDepthStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Count: Integer;
  Index: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FEtsSurfDepthArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FEtsSurfDepthArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TEtsSurfDepthStorage.GetEtsSurfDepthArray: TEtsSurfDepthArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FEtsSurfDepthArray;
end;

{ TUzfEvtTimeListLink }

procedure TEtsTimeListLink.CreateTimeLists;
begin
  inherited;
  if Model <> nil then
  begin
    EvapotranspirationRateData.OnInvalidate :=
      (Model as TCustomModel).InvalidateMfEtsEvapRate;
  end;
end;

{ TEtsLayerTimeListLink }

procedure TEtsLayerTimeListLink.CreateTimeLists;
begin
  inherited;
  if Model <> nil then
  begin
    EvapotranspirationLayerData.OnInvalidate :=
      (Model as TCustomModel).InvalidateMfEtsEvapLayer;
  end;
end;

{ TEtsSurfDepthTimeListLink }

procedure TEtsSurfDepthTimeListLink.CreateTimeLists;
begin
  inherited;
  FEvapotranspirationSurfaceData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FEvapotranspirationSurfaceData.NonParamDescription := StrEvapoTranspirationSurf;
  FEvapotranspirationSurfaceData.ParamDescription := ' '+ LowerCase(StrEvapoTranspirationSurf);
  AddTimeList(FEvapotranspirationSurfaceData);
  FEvapotranspirationDepthData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FEvapotranspirationDepthData.NonParamDescription := StrEvapoTranspirationDepth;
  FEvapotranspirationDepthData.ParamDescription := ' ' + LowerCase(StrEvapoTranspirationDepth);
  AddTimeList(FEvapotranspirationDepthData);
  if Model <> nil then
  begin
    FEvapotranspirationSurfaceData.OnInvalidate := (Model as TCustomModel).InvalidateMfEtsEvapSurface;
    FEvapotranspirationDepthData.OnInvalidate := (Model as TCustomModel).InvalidateMfEtsEvapDepth;
  end;
  FListOfEtFractionLists := TObjectList.Create;
  FListOfDepthFractionLists := TObjectList.Create;
end;

destructor TEtsSurfDepthTimeListLink.Destroy;
begin
  FEvapotranspirationDepthData.Free;
  FEvapotranspirationSurfaceData.Free;
  FListOfEtFractionLists.Free;
  FListOfDepthFractionLists.Free;
  inherited;
end;

end.
