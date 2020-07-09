{@name defines how evapotranspiration package data for one @link(TScreenObject)
 is stored and processed. }
unit ModflowEvtUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, GoPhastTypes;

type
  {
     @name stores the location, time and evapotranspiration rate for a cell.
  }
  TEvtRecord = record
    Cell: TCellLocation;
    EvapotranspirationRate: double;
    StartingTime: double;
    EndingTime: double;
    EvapotranspirationRateAnnotation: string;
    TimeSeriesName: string;
    ETParameterName: string;
    ETParameterValue: double;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  {
    @name stores the location, time, evaporation surface and evaporation depth for a cell.
  }
  TEvtSurfDepthRecord = record
    Cell: TCellLocation;
    EvapotranspirationSurface: double;
    EvapotranspirationDepth: double;
    StartingTime: double;
    EndingTime: double;
    EvapotranspirationSurfaceAnnotation: string;
    EvapotranspirationDepthAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  {
    @name stores the location, and evaporation layer for a cell.
  }
  TEvtLayerRecord = record
    Cell: TCellLocation;
    EvapotranspirationLayer: integer;
    StartingTime: double;
    EndingTime: double;
    EvapotranspirationLayerAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  // @name is an array of @link(TEvtRecord)s.
  TEvtArray = array of TEvtRecord;
  // @name is an array of @link(TEvtLayerRecord)s.
  TEvtLayerArray = array of TEvtLayerRecord;
  // @name is an array of @link(TEvtSurfDepthRecord)s.
  TEvtSurfDepthArray = array of TEvtSurfDepthRecord;

  // @name extends @link(TCustomBoundaryStorage) by adding the locations
  // and evapotranspiration rates of series of evapotranspiration cells.
  TEvtStorage = class(TCustomBoundaryStorage)
  private
    FEvtArray: TEvtArray;
    function GetEvtArray: TEvtArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property EvtArray: TEvtArray read GetEvtArray;
  end;

  // @name extends @link(TCustomBoundaryStorage) by adding the locations
  // and evapotranspiration layers of series of evapotranspiration cells.
  TEvtLayerStorage = class(TCustomBoundaryStorage)
  private
    FEvtLayerArray: TEvtLayerArray;
    function GetEvtLayerArray: TEvtLayerArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property EvtLayerArray: TEvtLayerArray read GetEvtLayerArray;
  end;

  // @name extends @link(TCustomBoundaryStorage) by adding the locations
  // and evapotranspiration surfaces and depths of series of
  // evapotranspiration cells.
  TEvtSurfDepthStorage = class(TCustomBoundaryStorage)
  private
    FEvtSurfDepthArray: TEvtSurfDepthArray;
    function GetEvtSurfDepthArray: TEvtSurfDepthArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property EvtSurfDepthArray: TEvtSurfDepthArray read GetEvtSurfDepthArray;
  end;

  // @name represents a MODFLOW evapotranspiration for one time interval.
  // @name is stored by @link(TEvtCollection).
  TEvtItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(EvapotranspirationRate).
    FEvapotranspirationRate: TFormulaObject;
    // See @link(EvapotranspirationRate).
    procedure SetEvapotranspirationRate(const Value: string);
    function GetEvapotranspirationRate: string;
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
    property EvapotranspirationRate: string read GetEvapotranspirationRate
      write SetEvapotranspirationRate;
  end;

  // @name represents a MODFLOW ET layer for one time interval.
  // @name is stored by @link(TEvtLayerCollection).
  TEvtLayerItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(EvapotranspirationLayer).
    FEvapotranspirationLayer: TFormulaObject;
    // See @link(EvapotranspirationLayer).
    procedure SetEvapotranspirationLayer(const Value: string);
    function GetEvapotranspirationLayer: string;
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
    property EvapotranspirationLayer: string read GetEvapotranspirationLayer
      write SetEvapotranspirationLayer;
  end;

  // @name represents a MODFLOW ET layer for one time interval.
  // @name is stored by @link(TEvtSurfDepthCollection).
  TEvtSurfDepthItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(EvapotranspirationSurface).
    FEvapotranspirationSurface: TFormulaObject;
    FEvapotranspirationDepth: TFormulaObject;
    // See @link(EvapotranspirationSurface).
    procedure SetEvapotranspirationSurface(const Value: string);
    procedure SetEvapotranspirationDepth(const Value: string);
    function GetEvapotranspirationDepth: string;
    function GetEvapotranspirationSurface: string;
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
    property EvapotranspirationSurface: string read GetEvapotranspirationSurface
      write SetEvapotranspirationSurface;
    property EvapotranspirationDepth: string read GetEvapotranspirationDepth
      write SetEvapotranspirationDepth;
  end;

  TEvtTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the recharge rates for a series of
    // cells over a series of time intervals.
    FEvapotranspirationRateData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
    property EvapotranspirationRateData: TModflowTimeList read FEvapotranspirationRateData;
  public
    Destructor Destroy; override;
  end;


  // @name represents MODFLOW Evapotranspiration boundaries
  // for a series of time intervals.
  TEvtCollection = class(TCustomMF_ArrayBoundColl)
  private
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
    // the @link(TEvtStorage.EvtArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer;
      AModel: TBaseModel); override;
    procedure InvalidateEtRateData(Sender: TObject);
  end;

  TEvtLayerTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the evapotranspiration layer for a series of
    // cells over a series of time intervals.
    FEvapotranspirationLayerData: TModflowTimeList;
  protected
    property EvapotranspirationLayerData: TModflowTimeList read FEvapotranspirationLayerData;
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  TEvtLayerCollection = class(TCustomMF_ArrayBoundColl)
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
    // the @link(TEvtStorage.EvtArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
    procedure InvalidateEtLayer(Sender: TObject);
  end;

  TEvtSurfDepthListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the evapotranspiration surface for a series of
    // cells over a series of time intervals.
    FEvapotranspirationSurfaceData: TModflowTimeList;
    // @name is used to compute the evapotranspiration
    // cutoff depth for a series of
    // cells over a series of time intervals.
    FEvapotranspirationDepthData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  TEvtSurfDepthCollection = class(TCustomMF_ArrayBoundColl)
  private
    procedure InvalidateSurfaceData(Sender: TObject);
    procedure InvalidateDepthData(Sender: TObject);
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
    // the @link(TEvtStorage.EvtArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
  end;

  // Each @name stores a @link(TEvtCollection).
  // @classname is stored by @link(TModflowParameters).
  TEvtParamItem = class(TModflowParamItem)
  protected
    class function BoundaryClass: TMF_BoundCollClass; override;
  end;

  TEvapotranspirationCell = class(TValueCell);

  TEvt_Cell = class(TEvapotranspirationCell)
  private
    FValues: TEvtRecord;
    FStressPeriod: integer;
    function GetEvapotranspirationRate: double;
    function GetEvapotranspirationRateAnnotation: string;
    function GetTimeSeriesName: string;
    function GetETParameterName: string;
    function GetETParameterValue: double;
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
    property EvapotranspirationRate: double read GetEvapotranspirationRate;
    property EvapotranspirationRateAnnotation: string read GetEvapotranspirationRateAnnotation;
    property TimeSeriesName: string read GetTimeSeriesName;
    property StressPeriod: integer read FStressPeriod write FStressPeriod;
    property Values: TEvtRecord read FValues write FValues;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
    property ETParameterName: string read GetETParameterName;
    property ETParameterValue: double read GetETParameterValue;
  end;

  TEvapotranspirationLayerCell = class(TEvapotranspirationCell)
  private
    FValues: TEvtLayerRecord;
    FStressPeriod: integer;
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
    property Values: TEvtLayerRecord read FValues write FValues;
    property EvapotranspirationLayer: Integer read FValues.EvapotranspirationLayer;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
  end;

  TEvtSurfDepth_Cell = class(TEvapotranspirationCell)
  private
    Values: TEvtSurfDepthRecord;
    StressPeriod: integer;
    function GetEvapotranspirationSurface: double;
    function GetEvapotranspirationDepth: double;
    function GetEvapotranspirationDepthAnnotation: string;
    function GetEvapotranspirationSurfaceAnnotation: string;
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
    property EvapotranspirationSurfaceAnnotation: string read GetEvapotranspirationSurfaceAnnotation;
    property EvapotranspirationDepthAnnotation: string read GetEvapotranspirationDepthAnnotation;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
  end;


  // @name represents the MODFLOW Evapotranspiration boundaries associated with
  // a single @link(TScreenObject).
  //
  // @seealso(TEvtCollection)
  TEvtBoundary = class(TModflowParamBoundary)
  private
    FEvapotranspirationLayers: TEvtLayerCollection;
    FEvtSurfDepthCollection: TEvtSurfDepthCollection;
    procedure SetEvapotranspirationLayers(const Value: TEvtLayerCollection);
    procedure SetEvtSurfDepthCollection(const Value: TEvtSurfDepthCollection);
    function GetTimeVaryingEvapotranspirationLayers: boolean;
    procedure AssignEvapotranspirationLayerCells(BoundaryStorage: TEvtLayerStorage;
      ValueTimeList: TList);
    procedure AssignSurfaceDepthCells(AModel: TBaseModel; BoundaryStorage: TEvtSurfDepthStorage;
      ValueTimeList: TList);
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
    procedure Clear; override;
  published
    property EvapotranspirationLayers: TEvtLayerCollection
      read FEvapotranspirationLayers write SetEvapotranspirationLayers;
    property EvtSurfDepthCollection: TEvtSurfDepthCollection
      read FEvtSurfDepthCollection write SetEvtSurfDepthCollection;
    procedure InvalidateDisplay; override;
  end;

resourcestring
  StrEvapoTranspirationSurf = 'Evapo- transpiration surface';
  StrEvapoTranspirationDepth = 'Evapo- transpiration depth';
  StrEvapotranspirationD_EVT = 'Evapotranspiration depth in the EVT package ' +
  'is less than or equal to zero';

implementation

uses RbwParser, ScreenObjectUnit, PhastModelUnit, ModflowTimeUnit,
  ModflowTransientListParameterUnit, frmGoPhastUnit, TempFiles,
  frmErrorsAndWarningsUnit;

resourcestring
  StrEvapoTranspirationRate = 'Evapo- transpiration rate';
  StrEvapoTranspiratioMult = ' evapo- transpiration rate multiplier';
  StrEvapoTranspirationLayer = 'Evapo- transpiration layer';

const
  RatePosition = 0;
  LayerPosition = 0;
  SurfacePosition = 0;
  DepthPosition = 1;

{ TEvtItem }

procedure TEvtItem.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if Source is TEvtItem then
  begin
    EvapotranspirationRate := TEvtItem(Source).EvapotranspirationRate;
  end;
  inherited;
end;

procedure TEvtItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TEvtCollection;
  RateObserver: TObserver;
begin
  ParentCollection := Collection as TEvtCollection;
  RateObserver := FObserverList[RatePosition];
  RateObserver.OnUpToDateSet := ParentCollection.InvalidateEtRateData;
end;

function TEvtItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

procedure TEvtItem.CreateFormulaObjects;
begin
  FEvapotranspirationRate := CreateFormulaObject(dsoTop);
end;

destructor TEvtItem.Destroy;
begin
  EvapotranspirationRate := '0';
  inherited;
end;

function TEvtItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    RatePosition: result := EvapotranspirationRate;
    else Assert(False);
  end;
end;

function TEvtItem.GetEvapotranspirationRate: string;
begin
  Result := FEvapotranspirationRate.Formula;
  ResetItemObserver(RatePosition);
end;

procedure TEvtItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  Assert(Sender = FEvapotranspirationRate);
  List.Add(FObserverList[RatePosition]);
end;

function TEvtItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TEvtItem;
begin
  result := (AnotherItem is TEvtItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TEvtItem(AnotherItem);
    result := (Item.EvapotranspirationRate = EvapotranspirationRate)
  end;
end;

procedure TEvtItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FEvapotranspirationRate,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TEvtItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    RatePosition: EvapotranspirationRate := Value;
    else Assert(False);
  end;
end;

procedure TEvtItem.SetEvapotranspirationRate(const Value: string);
begin
  UpdateFormula(Value, RatePosition, FEvapotranspirationRate);
end;

{ TEvtCollection }

procedure TEvtCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TEvtStorage.Create(AModel));
end;

procedure TEvtCollection.AssignArrayCellValues(DataSets: TList; ItemIndex: Integer;
  AModel: TBaseModel);
var
  EvapotranspirationRateArray: TDataArray;
  Boundary: TEvtStorage;
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
  EvapotranspirationRateArray := DataSets[RatePosition];
  Boundary := Boundaries[ItemIndex, AModel] as TEvtStorage;
  EvapotranspirationRateArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
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
            if EvapotranspirationRateArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              with Boundary.EvtArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                EvapotranspirationRate := EvapotranspirationRateArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                EvapotranspirationRateAnnotation := EvapotranspirationRateArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  EvapotranspirationRateArray.CacheData;
  Boundary.CacheData;
end;

function TEvtCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TEvtTimeListLink;
end;

procedure TEvtCollection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TEvtItem;
  ScreenObject: TScreenObject;
  ALink: TEvtTimeListLink;
  FEvapotranspirationRateData: TModflowTimeList;
begin
  ScreenObject := BoundaryGroup.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TEvtItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.EvapotranspirationRate;
  end;
  ALink := TimeListLink.GetLink(AModel) as TEvtTimeListLink;
  FEvapotranspirationRateData := ALink.FEvapotranspirationRateData;
  FEvapotranspirationRateData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(FEvapotranspirationRateData.Count = Count);
  ClearBoundaries(AModel);
  SetBoundaryCapacity(FEvapotranspirationRateData.Count, AModel);
  for TimeIndex := 0 to FEvapotranspirationRateData.Count - 1 do
  begin
    AddBoundary(TEvtStorage.Create(AModel));
  end;
  ListOfTimeLists.Add(FEvapotranspirationRateData);
end;

procedure TEvtCollection.InvalidateEtRateData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TEvtTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TEvtTimeListLink;
    Link.FEvapotranspirationRateData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TEvtTimeListLink;
      Link.FEvapotranspirationRateData.Invalidate;
    end;
  end;
end;

class function TEvtCollection.ItemClass: TBoundaryItemClass;
begin
  result := TEvtItem;
end;

procedure TEvtCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TEvtStorage).FEvtArray, BoundaryCount);
  inherited;
end;

{ TEvtParamItem }

class function TEvtParamItem.BoundaryClass: TMF_BoundCollClass;
begin
  result := TEvtCollection;
end;

{ TEvt_Cell }

procedure TEvt_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TEvt_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TEvt_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TEvt_Cell.GetETParameterName: string;
begin
  result := Values.ETParameterName;
end;

function TEvt_Cell.GetETParameterValue: double;
begin
  result := Values.ETParameterValue;
end;

function TEvt_Cell.GetEvapotranspirationRate: double;
begin
  result := Values.EvapotranspirationRate;
end;

function TEvt_Cell.GetEvapotranspirationRateAnnotation: string;
begin
  result := Values.EvapotranspirationRateAnnotation;
end;

function TEvt_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    RatePosition: result := 'Assigned from the cell''s layer';
    else Assert(False);
  end;
end;

function TEvt_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  case Index of
    RatePosition: result := (AModel as TCustomModel).
      DataSetLayerToModflowLayer(Layer);
    else Assert(False);
  end;
end;

function TEvt_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    RatePosition: result := EvapotranspirationRateAnnotation;
    else Assert(False);
  end;
end;

function TEvt_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    RatePosition: result := EvapotranspirationRate;
    else Assert(False);
  end;
end;

function TEvt_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TEvt_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TEvt_Cell.GetTimeSeriesName: string;
begin
  result := Values.TimeSeriesName;
end;

function TEvt_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  Evt_Cell: TEvt_Cell;
begin
  result := AnotherCell is TEvt_Cell;
  if result then
  begin
    Evt_Cell := TEvt_Cell(AnotherCell);
    result :=
      (EvapotranspirationRate = Evt_Cell.EvapotranspirationRate);
  end;
end;

procedure TEvt_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TEvt_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TEvt_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TEvt_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TEvt_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TEvtBoundary }

procedure TEvtBoundary.Assign(Source: TPersistent);
var
  SourceBoundary: TEvtBoundary;
begin
  if Source is TEvtBoundary then
  begin
    SourceBoundary := TEvtBoundary(Source);
    EvapotranspirationLayers := SourceBoundary.EvapotranspirationLayers;
    EvtSurfDepthCollection := SourceBoundary.EvtSurfDepthCollection;
  end;
  inherited;
end;

procedure TEvtBoundary.AssignEvapotranspirationLayerCells(BoundaryStorage: TEvtLayerStorage;
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


procedure TEvtBoundary.AssignSurfaceDepthCells(AModel: TBaseModel;
  BoundaryStorage: TEvtSurfDepthStorage; ValueTimeList: TList);
var
  Cell: TEvtSurfDepth_Cell;
  BoundaryValues: TEvtSurfDepthRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TEvtSurfDepthStorage;
  Active: TDataArray;
begin
  LocalBoundaryStorage := BoundaryStorage;
  for TimeIndex := 0 to
    (ParentModel as TPhastModel).ModflowFullStressPeriods.Count - 1 do
  begin
    Active := (AModel as TCustomModel).DataArrayManager.GetDataSetByName(rsActive);
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TEvtSurfDepth_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := (ParentModel as TPhastModel).ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime <= LocalBoundaryStorage.EndingTime) then
    begin
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.EvtSurfDepthArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.EvtSurfDepthArray[BoundaryIndex];
        Cell := TEvtSurfDepth_Cell.Create;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
        if BoundaryValues.EvapotranspirationDepth <= 0 then
        begin
          if Active.BooleanData[Cell.Layer, Cell.Row, Cell.Column] then
          begin
            frmErrorsAndWarnings.AddError(AModel, StrEvapotranspirationD_EVT,
              Format(StrSP_Lay_Row_Col,
              [TimeIndex+1, Cell.Layer+1, Cell.Row+1, Cell.Column + 1]));
          end;
        end;
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

procedure TEvtBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
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

class function TEvtBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TEvtCollection;
end;

procedure TEvtBoundary.Clear;
begin
  inherited;
  EvapotranspirationLayers.Clear;
  FEvtSurfDepthCollection.Clear;
end;

constructor TEvtBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited Create(Model, ScreenObject);
  FEvapotranspirationLayers := TEvtLayerCollection.Create(self, Model, ScreenObject);
  FEvtSurfDepthCollection := TEvtSurfDepthCollection.Create(self, Model, ScreenObject);
end;

destructor TEvtBoundary.Destroy;
begin
  FEvtSurfDepthCollection.Free;
  FEvapotranspirationLayers.Free;
  inherited;
end;

procedure TEvtBoundary.EvaluateArrayBoundaries(AModel: TBaseModel);
begin
  inherited;
  EvtSurfDepthCollection.EvaluateArrayBoundaries(AModel);
  if (ParentModel as TPhastModel).
    ModflowPackages.EvtPackage.TimeVaryingLayers then
  begin
    EvapotranspirationLayers.EvaluateArrayBoundaries(AModel);
  end;
end;

procedure TEvtBoundary.GetEvapotranspirationLayerCells(LayerTimeList: TList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TEvtLayerStorage;
begin
  if not (ParentModel as TPhastModel).ModflowPackages.
    EvtPackage.TimeVaryingLayers then
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

procedure TEvtBoundary.GetEvapotranspirationSurfaceDepthCells(
  LayerTimeList: TList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TEvtSurfDepthStorage;
begin
  for ValueIndex := 0 to EvtSurfDepthCollection.Count - 1 do
  begin
    if ValueIndex < EvtSurfDepthCollection.BoundaryCount[AModel] then
    begin
      BoundaryStorage := EvtSurfDepthCollection.Boundaries[ValueIndex, AModel]
        as TEvtSurfDepthStorage;
      AssignSurfaceDepthCells(AModel, BoundaryStorage, LayerTimeList);
    end;
  end;
end;

procedure TEvtBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TEvtStorage;
  ParamIndex: Integer;
  Param: TModflowParamItem;
  Times: TList;
  Position: integer;
  ParamName: string;
  Model: TPhastModel;
begin
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


function TEvtBoundary.GetTimeVaryingEvapotranspirationLayers: boolean;
begin
  if ParentModel = nil then
  begin
    result := frmGoPhast.PhastModel.ModflowPackages.
      EvtPackage.TimeVaryingLayers;
  end
  else
  begin
    result := (ParentModel as TPhastModel).ModflowPackages.
      EvtPackage.TimeVaryingLayers;
  end;
end;

procedure TEvtBoundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TPhastModel;
    Model.InvalidateMfEvtEvapRate(self);
    Model.InvalidateMfEvtEvapSurface(self);
    Model.InvalidateMfEvtEvapDepth(self);
    Model.InvalidateMfEvtEvapLayer(self);
  end;
end;

class function TEvtBoundary.ModflowParamItemClass: TModflowParamItemClass;
begin
  result := TEvtParamItem;
end;

function TEvtBoundary.NonParameterColumns: integer;
begin
  result := inherited NonParameterColumns + 2;
  if TimeVaryingEvapotranspirationLayers then
  begin
    result := result + EvapotranspirationLayers.TimeListCount(frmGoPhast.PhastModel);
  end;
end;

function TEvtBoundary.ParameterType: TParameterType;
begin
  result := ptEVT;
end;

procedure TEvtBoundary.SetEvapotranspirationLayers(const Value: TEvtLayerCollection);
begin
  FEvapotranspirationLayers.Assign(Value);
end;

procedure TEvtBoundary.SetEvtSurfDepthCollection(
  const Value: TEvtSurfDepthCollection);
begin
  FEvtSurfDepthCollection.Assign(Value);
end;

function TEvtBoundary.Used: boolean;
var
  Model: TPhastModel;
  ParamIndex: Integer;
  Param: TModflowTransientListParameter;
begin
  if ParentModel <> nil then
  begin
    Model := ParentModel as TPhastModel;
    result := Model.ModflowPackages.EvtPackage.TimeVaryingLayers
      and EvapotranspirationLayers.Used;
  end
  else
  begin
    result := EvapotranspirationLayers.Used;
  end;
  if result then Exit;
  result := EvtSurfDepthCollection.Used;
  if result then Exit;
  result := inherited Used;
  if result and (ParentModel <> nil) then
  begin
    Model := ParentModel as TPhastModel;
    if csLoading in Model.ComponentState then
    begin
      Exit;
    end;
    for ParamIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
    begin
      Param := Model.ModflowTransientParameters[ParamIndex];
      if Param.ParameterType = ptEVT then
      begin
        result := Parameters.Used;
        Exit;
      end;
    end;
    result := Values.Used;
  end;
end;

{ TEvtLayerItem }

procedure TEvtLayerItem.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if Source is TEvtLayerItem then
  begin
    EvapotranspirationLayer := TEvtLayerItem(Source).EvapotranspirationLayer;
  end;
  inherited;
end;

procedure TEvtLayerItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TEvtLayerCollection;
  LayerObserver: TObserver;
begin
  ParentCollection := Collection as TEvtLayerCollection;
  LayerObserver := FObserverList[LayerPosition];
  LayerObserver.OnUpToDateSet := ParentCollection.InvalidateEtLayer;
end;

function TEvtLayerItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

procedure TEvtLayerItem.CreateFormulaObjects;
begin
  FEvapotranspirationLayer := CreateFormulaObject(dsoTop);
end;

destructor TEvtLayerItem.Destroy;
begin
  EvapotranspirationLayer := '0';
  inherited;
end;

function TEvtLayerItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    LayerPosition: result := EvapotranspirationLayer;
    else Assert(False);
  end;
end;

function TEvtLayerItem.GetEvapotranspirationLayer: string;
begin
  Result := FEvapotranspirationLayer.Formula;
  ResetItemObserver(LayerPosition);
end;

procedure TEvtLayerItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  Assert(Sender = FEvapotranspirationLayer);
  List.Add(FObserverList[LayerPosition]);
end;

function TEvtLayerItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TEvtLayerItem;
begin
  result := (AnotherItem is TEvtItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TEvtLayerItem(AnotherItem);
    result := (Item.EvapotranspirationLayer = EvapotranspirationLayer)
  end;
end;

procedure TEvtLayerItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FEvapotranspirationLayer,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TEvtLayerItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    LayerPosition: EvapotranspirationLayer := Value;
    else Assert(False);
  end;
end;

procedure TEvtLayerItem.SetEvapotranspirationLayer(const Value: string);
begin
  UpdateFormula(Value, LayerPosition, FEvapotranspirationLayer);
end;

{ TEvtLayerCollection }

procedure TEvtLayerCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TEvtLayerStorage.Create(AModel));
end;

procedure TEvtLayerCollection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel);
var
  EvapotranspirationLayerArray: TDataArray;
  Boundary: TEvtLayerStorage;
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
  EvapotranspirationLayerArray := DataSets[LayerPosition];
  Boundary := Boundaries[ItemIndex, AModel] as TEvtLayerStorage;
  EvapotranspirationLayerArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
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
            if EvapotranspirationLayerArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              with Boundary.EvtLayerArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                EvapotranspirationLayer := EvapotranspirationLayerArray.
                  IntegerData[LayerIndex, RowIndex, ColIndex];
                EvapotranspirationLayerAnnotation := EvapotranspirationLayerArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  EvapotranspirationLayerArray.CacheData;
  Boundary.CacheData;
end;

function TEvtLayerCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TEvtLayerTimeListLink;
end;

procedure TEvtLayerCollection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TEvtLayerItem;
  ScreenObject: TScreenObject;
  ALink: TEvtLayerTimeListLink;
  EvapotranspirationLayerData: TModflowTimeList;
begin
  ScreenObject := BoundaryGroup.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TEvtLayerItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.EvapotranspirationLayer;
  end;
  ALink := TimeListLink.GetLink(AModel) as TEvtLayerTimeListLink;
  EvapotranspirationLayerData := ALink.FEvapotranspirationLayerData;
  EvapotranspirationLayerData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(EvapotranspirationLayerData.Count = Count);
  ClearBoundaries(AModel);
  SetBoundaryCapacity(EvapotranspirationLayerData.Count, AModel);
  for TimeIndex := 0 to EvapotranspirationLayerData.Count - 1 do
  begin
    AddBoundary(TEvtLayerStorage.Create(AModel));
  end;
  ListOfTimeLists.Add(EvapotranspirationLayerData);
end;

procedure TEvtLayerCollection.InvalidateEtLayer(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TEvtLayerTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TEvtLayerTimeListLink;
    Link.FEvapotranspirationLayerData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TEvtLayerTimeListLink;
      Link.FEvapotranspirationLayerData.Invalidate;
    end;
  end;
end;

class function TEvtLayerCollection.ItemClass: TBoundaryItemClass;
begin
  result := TEvtLayerItem;
end;

procedure TEvtLayerCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TEvtLayerStorage).FEvtLayerArray,
    BoundaryCount);
  inherited;
end;

{ TEvapotranspirationLayerCell }

procedure TEvapotranspirationLayerCell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TEvapotranspirationLayerCell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TEvapotranspirationLayerCell.GetIntegerAnnotation(
  Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    LayerPosition: result := Values.EvapotranspirationLayerAnnotation;
    else Assert(False);
  end;
end;

function TEvapotranspirationLayerCell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  case Index of
    LayerPosition: result := Layer + 1;
    else Assert(False);
  end;
end;

function TEvapotranspirationLayerCell.GetLayer: integer;
begin
  // 1 is subtractred from EvapotranspirationLayer in order to compensate
  // for 1 being added to the layer in TModflowEVT_Writer.WriteEvapotranspirationLayer.
  result := Values.EvapotranspirationLayer-1;
end;

function TEvapotranspirationLayerCell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TEvapotranspirationLayerCell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  Assert(False);
end;

function TEvapotranspirationLayerCell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;
function TEvapotranspirationLayerCell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TEvapotranspirationLayerCell.IsIdentical(
  AnotherCell: TValueCell): boolean;
var
  EtLayerCell: TEvapotranspirationLayerCell;
begin
  result := AnotherCell is TEvapotranspirationLayerCell;
  if Result then
  begin
    EtLayerCell := TEvapotranspirationLayerCell(AnotherCell);
    Result := (EvapotranspirationLayer = EtLayerCell.EvapotranspirationLayer)
      and (FValues.Cell = EtLayerCell.FValues.Cell);
  end;
end;

procedure TEvapotranspirationLayerCell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TEvapotranspirationLayerCell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TEvapotranspirationLayerCell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TEvapotranspirationLayerCell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TEvapotranspirationLayerCell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TEvtSurfDepthItem }

procedure TEvtSurfDepthItem.Assign(Source: TPersistent);
var
  SourceItem: TEvtSurfDepthItem;
begin
  if Source is TEvtSurfDepthItem then
  begin
  // if Assign is updated, update IsSame too.
    SourceItem := TEvtSurfDepthItem(Source);
    EvapotranspirationSurface := SourceItem.EvapotranspirationSurface;
    EvapotranspirationDepth := SourceItem.EvapotranspirationDepth;
  end;
  inherited;
end;

procedure TEvtSurfDepthItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TEvtSurfDepthCollection;
  SurfaceObserver: TObserver;
  DepthObserver: TObserver;
begin
  ParentCollection := Collection as TEvtSurfDepthCollection;
  SurfaceObserver := FObserverList[SurfacePosition];
  SurfaceObserver.OnUpToDateSet := ParentCollection.InvalidateSurfaceData;
  DepthObserver := FObserverList[DepthPosition];
  DepthObserver.OnUpToDateSet := ParentCollection.InvalidateDepthData;
end;

function TEvtSurfDepthItem.BoundaryFormulaCount: integer;
begin
  result := 2;
end;

procedure TEvtSurfDepthItem.CreateFormulaObjects;
begin
  inherited;
  FEvapotranspirationSurface := CreateFormulaObject(dsoTop);
  FEvapotranspirationDepth := CreateFormulaObject(dsoTop);
end;

destructor TEvtSurfDepthItem.Destroy;
begin
  EvapotranspirationSurface := '0';
  EvapotranspirationDepth := '0';
  inherited;
end;

function TEvtSurfDepthItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    SurfacePosition: result := EvapotranspirationSurface;
    DepthPosition: result := EvapotranspirationDepth;
    else Assert(False);
  end;
end;

function TEvtSurfDepthItem.GetEvapotranspirationDepth: string;
begin
  Result := FEvapotranspirationDepth.Formula;
  ResetItemObserver(DepthPosition);
end;

function TEvtSurfDepthItem.GetEvapotranspirationSurface: string;
begin
  Result := FEvapotranspirationSurface.Formula;
  ResetItemObserver(SurfacePosition);
end;

procedure TEvtSurfDepthItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FEvapotranspirationSurface then
  begin
    List.Add( FObserverList[SurfacePosition]);
  end;
  if Sender = FEvapotranspirationDepth then
  begin
    List.Add( FObserverList[DepthPosition]);
  end;
end;

function TEvtSurfDepthItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TEvtSurfDepthItem;
begin
  result := (AnotherItem is TEvtSurfDepthItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TEvtSurfDepthItem(AnotherItem);
    result :=
      (Item.EvapotranspirationSurface = EvapotranspirationSurface)
      and (Item.EvapotranspirationDepth = EvapotranspirationDepth)
  end;
end;

procedure TEvtSurfDepthItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FEvapotranspirationDepth,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FEvapotranspirationSurface,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TEvtSurfDepthItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    SurfacePosition: EvapotranspirationSurface := Value;
    DepthPosition: EvapotranspirationDepth := Value;
    else Assert(False);
  end;
end;

procedure TEvtSurfDepthItem.SetEvapotranspirationDepth(const Value: string);
begin
  UpdateFormula(Value, DepthPosition, FEvapotranspirationDepth);
end;

procedure TEvtSurfDepthItem.SetEvapotranspirationSurface(const Value: string);
begin
  UpdateFormula(Value, SurfacePosition, FEvapotranspirationSurface);
end;

{ TEvtSurfDepthCollection }

procedure TEvtSurfDepthCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TEvtSurfDepthStorage.Create(AModel));
end;

procedure TEvtSurfDepthCollection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel);
var
  EvapotranspirationSurfaceArray: TDataArray;
  EvapotranspirationDepthArray: TDataArray;
  Boundary: TEvtSurfDepthStorage;
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
  EvapotranspirationSurfaceArray := DataSets[SurfacePosition];
  EvapotranspirationDepthArray := DataSets[DepthPosition];
  Boundary := Boundaries[ItemIndex, AModel] as TEvtSurfDepthStorage;
  EvapotranspirationSurfaceArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
    LayerMax, RowMax, ColMax);
  if LayerMin >= 0 then
  begin
    for LayerIndex := 0 to EvapotranspirationSurfaceArray.LayerCount - 1 do
    begin
      if LocalModel.IsLayerSimulated(LayerIndex) then
      begin
        for RowIndex := 0 to EvapotranspirationSurfaceArray.RowCount - 1 do
        begin
          for ColIndex := 0 to EvapotranspirationSurfaceArray.ColumnCount - 1 do
          begin
            if EvapotranspirationSurfaceArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              Assert(EvapotranspirationDepthArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              with Boundary.EvtSurfDepthArray[BoundaryIndex] do
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
  Boundary.CacheData;
end;

function TEvtSurfDepthCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TEvtSurfDepthListLink;
end;

procedure TEvtSurfDepthCollection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TEvtSurfDepthItem;
  Boundary: TEvtBoundary;
  ScreenObject: TScreenObject;
  ALink: TEvtSurfDepthListLink;
  EvapotranspirationSurfaceData: TModflowTimeList;
  EvapotranspirationDepthData: TModflowTimeList;
begin
  Boundary := BoundaryGroup as TEvtBoundary;
  ScreenObject := Boundary.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TEvtSurfDepthItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.EvapotranspirationSurface;
  end;
  ALink := TimeListLink.GetLink(AModel) as TEvtSurfDepthListLink;
  EvapotranspirationSurfaceData := ALink.FEvapotranspirationSurfaceData;
  EvapotranspirationSurfaceData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(EvapotranspirationSurfaceData.Count = Count);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TEvtSurfDepthItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.EvapotranspirationDepth;
  end;
  EvapotranspirationDepthData := ALink.FEvapotranspirationDepthData;
  EvapotranspirationDepthData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(EvapotranspirationDepthData.Count = Count);

  ClearBoundaries(AModel);
  SetBoundaryCapacity(EvapotranspirationSurfaceData.Count, AModel);
  for TimeIndex := 0 to EvapotranspirationSurfaceData.Count - 1 do
  begin
    AddBoundary(TEvtSurfDepthStorage.Create(AModel));
  end;
  ListOfTimeLists.Add(EvapotranspirationSurfaceData);
  ListOfTimeLists.Add(EvapotranspirationDepthData);
end;

procedure TEvtSurfDepthCollection.InvalidateDepthData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TEvtSurfDepthListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TEvtSurfDepthListLink;
    Link.FEvapotranspirationDepthData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TEvtSurfDepthListLink;
      Link.FEvapotranspirationDepthData.Invalidate;
    end;
  end;
end;

procedure TEvtSurfDepthCollection.InvalidateSurfaceData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TEvtSurfDepthListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TEvtSurfDepthListLink;
    Link.FEvapotranspirationSurfaceData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TEvtSurfDepthListLink;
      Link.FEvapotranspirationSurfaceData.Invalidate;
    end;
  end;
end;

class function TEvtSurfDepthCollection.ItemClass: TBoundaryItemClass;
begin
  result := TEvtSurfDepthItem;
end;

procedure TEvtSurfDepthCollection.SetBoundaryStartAndEndTime(
  BoundaryCount: Integer; Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TEvtSurfDepthStorage).FEvtSurfDepthArray,
    BoundaryCount);
  inherited;

end;

{ TEvtSurfDepth_Cell }

procedure TEvtSurfDepth_Cell.Cache(Comp: TCompressionStream;
  Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TEvtSurfDepth_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TEvtSurfDepth_Cell.GetEvapotranspirationDepth: double;
begin
  result := Values.EvapotranspirationDepth;
end;

function TEvtSurfDepth_Cell.GetEvapotranspirationDepthAnnotation: string;
begin
  result := Values.EvapotranspirationDepthAnnotation;
end;

function TEvtSurfDepth_Cell.GetEvapotranspirationSurface: double;
begin
  result := Values.EvapotranspirationSurface;
end;

function TEvtSurfDepth_Cell.GetEvapotranspirationSurfaceAnnotation: string;
begin
  result := Values.EvapotranspirationSurfaceAnnotation;
end;

function TEvtSurfDepth_Cell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TEvtSurfDepth_Cell.GetIntegerValue(Index: integer;
  AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TEvtSurfDepth_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TEvtSurfDepth_Cell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    SurfacePosition: result := EvapotranspirationSurfaceAnnotation;
    DepthPosition: result := EvapotranspirationDepthAnnotation;
    else Assert(False);
  end;
end;

function TEvtSurfDepth_Cell.GetRealValue(Index: integer;
  AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    SurfacePosition: result := EvapotranspirationSurface;
    DepthPosition: result := EvapotranspirationDepth;
    else Assert(False);
  end;
end;

function TEvtSurfDepth_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TEvtSurfDepth_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TEvtSurfDepth_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  ETCell: TEvtSurfDepth_Cell;
begin
  result := AnotherCell is TEvtSurfDepth_Cell;
  if result then
  begin
    ETCell := TEvtSurfDepth_Cell(AnotherCell);
    result :=
      (ETCell.GetEvapotranspirationSurface = GetEvapotranspirationSurface)
      and (ETCell.EvapotranspirationDepth = EvapotranspirationDepth)
      and (ETCell.Values.Cell = Values.Cell);
  end;
end;

procedure TEvtSurfDepth_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TEvtSurfDepth_Cell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TEvtSurfDepth_Cell.SetColumn(const Value: integer);
begin
  Values.Cell.Column := Value;
end;

procedure TEvtSurfDepth_Cell.SetLayer(const Value: integer);
begin
  Values.Cell.Layer := Value;
end;

procedure TEvtSurfDepth_Cell.SetRow(const Value: integer);
begin
  Values.Cell.Row := Value;
end;

{ TEvtStorage }

procedure TEvtStorage.Clear;
begin
  SetLength(FEvtArray, 0);
  FCleared := True;
end;

procedure TEvtStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FEvtArray);
    for Index := 0 to Count - 1 do
    begin
      FEvtArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FEvtArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TEvtStorage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FEvtArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FEvtArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TEvtStorage.GetEvtArray: TEvtArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FEvtArray;
end;

{ TEvtLayerStorage }

procedure TEvtLayerStorage.Clear;
begin
  SetLength(FEvtLayerArray, 0);
  FCleared := True;
end;

procedure TEvtLayerStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FEvtLayerArray);
    for Index := 0 to Count - 1 do
    begin
      FEvtLayerArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FEvtLayerArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TEvtLayerStorage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FEvtLayerArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FEvtLayerArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TEvtLayerStorage.GetEvtLayerArray: TEvtLayerArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FEvtLayerArray;
end;

{ TEvtSurfDepthStorage }

procedure TEvtSurfDepthStorage.Clear;
begin
  SetLength(FEvtSurfDepthArray, 0);
  FCleared := True;
end;

procedure TEvtSurfDepthStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FEvtSurfDepthArray);
    for Index := 0 to Count - 1 do
    begin
      FEvtSurfDepthArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FEvtSurfDepthArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TEvtSurfDepthStorage.Restore(
  DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FEvtSurfDepthArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FEvtSurfDepthArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TEvtSurfDepthStorage.GetEvtSurfDepthArray: TEvtSurfDepthArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FEvtSurfDepthArray;
end;

{ TEvtRecord }

procedure TEvtRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  Comp.Write(Cell, SizeOf(Cell));
  Comp.Write(EvapotranspirationRate, SizeOf(EvapotranspirationRate));
  Comp.Write(StartingTime, SizeOf(StartingTime));
  Comp.Write(EndingTime, SizeOf(EndingTime));
  Comp.Write(EndingTime, SizeOf(ETParameterValue));

  WriteCompInt(Comp, Strings.IndexOf(EvapotranspirationRateAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(TimeSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(ETParameterName));

end;

procedure TEvtRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(EvapotranspirationRateAnnotation);
  Strings.Add(TimeSeriesName);
  Strings.Add(ETParameterName);
  {
    ETParameterName: string;
    ETParameterValue: double;
  }
end;

procedure TEvtRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  EvapotranspirationRate := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  ETParameterValue := ReadCompReal(Decomp);
  EvapotranspirationRateAnnotation := Annotations[ReadCompInt(Decomp)];
  TimeSeriesName := Annotations[ReadCompInt(Decomp)];
  ETParameterName := Annotations[ReadCompInt(Decomp)];
end;

{ TEvtSurfDepthRecord }

procedure TEvtSurfDepthRecord.Cache(Comp: TCompressionStream;
  Strings: TStringList);
//var
//  CommentLength: Integer;
begin
  Comp.Write(Cell, SizeOf(Cell));
  Comp.Write(EvapotranspirationSurface, SizeOf(EvapotranspirationSurface));
  Comp.Write(EvapotranspirationDepth, SizeOf(EvapotranspirationDepth));
  Comp.Write(StartingTime, SizeOf(StartingTime));
  Comp.Write(EndingTime, SizeOf(EndingTime));

  WriteCompInt(Comp, Strings.IndexOf(EvapotranspirationSurfaceAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(EvapotranspirationDepthAnnotation));

//  CommentLength := Length(EvapotranspirationSurfaceAnnotation);
//  Comp.Write(CommentLength, SizeOf(CommentLength));
//  Comp.WriteBuffer(Pointer(EvapotranspirationSurfaceAnnotation)^,
//    CommentLength * SizeOf(Char));
//
//  CommentLength := Length(EvapotranspirationDepthAnnotation);
//  Comp.Write(CommentLength, SizeOf(CommentLength));
//  Comp.WriteBuffer(Pointer(EvapotranspirationDepthAnnotation)^,
//    CommentLength * SizeOf(Char));
end;

procedure TEvtSurfDepthRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(EvapotranspirationSurfaceAnnotation);
  Strings.Add(EvapotranspirationDepthAnnotation);
end;

procedure TEvtSurfDepthRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  EvapotranspirationSurface := ReadCompReal(Decomp);
  EvapotranspirationDepth := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  EvapotranspirationSurfaceAnnotation := Annotations[ReadCompInt(Decomp)];
  EvapotranspirationDepthAnnotation := Annotations[ReadCompInt(Decomp)];
//  EvapotranspirationSurfaceAnnotation := ReadCompString(Decomp, Annotations);
//  EvapotranspirationDepthAnnotation := ReadCompString(Decomp, Annotations);
end;

{ TEvtLayerRecord }

procedure TEvtLayerRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
//var
//  CommentLength: Integer;
begin
  Comp.Write(Cell, SizeOf(Cell));
  Comp.Write(EvapotranspirationLayer, SizeOf(EvapotranspirationLayer));
  Comp.Write(StartingTime, SizeOf(StartingTime));
  Comp.Write(EndingTime, SizeOf(EndingTime));

  WriteCompInt(Comp, Strings.IndexOf(EvapotranspirationLayerAnnotation));
//  CommentLength := Length(EvapotranspirationLayerAnnotation);
//  Comp.Write(CommentLength, SizeOf(CommentLength));
//  Comp.WriteBuffer(Pointer(EvapotranspirationLayerAnnotation)^,
//    CommentLength * SizeOf(Char));
end;

procedure TEvtLayerRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(EvapotranspirationLayerAnnotation);
end;

procedure TEvtLayerRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  EvapotranspirationLayer := ReadCompInt(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  EvapotranspirationLayerAnnotation := Annotations[ReadCompInt(Decomp)];
//  EvapotranspirationLayerAnnotation := ReadCompString(Decomp, Annotations);
end;

{ TEvtTimeListLink }

procedure TEvtTimeListLink.CreateTimeLists;
begin
  inherited;
  FEvapotranspirationRateData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FEvapotranspirationRateData.NonParamDescription := StrEvapoTranspirationRate;
  FEvapotranspirationRateData.ParamDescription := StrEvapoTranspiratioMult;
  AddTimeList(FEvapotranspirationRateData);
  if Model <> nil then
  begin
    FEvapotranspirationRateData.OnInvalidate :=
      (Model as TCustomModel).InvalidateMfEvtEvapRate;
  end;
end;

destructor TEvtTimeListLink.Destroy;
begin
  FEvapotranspirationRateData.Free;
  inherited;
end;

{ TEvtLayerTimeListLink }

procedure TEvtLayerTimeListLink.CreateTimeLists;
begin
  inherited;
  FEvapotranspirationLayerData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FEvapotranspirationLayerData.NonParamDescription := StrEvapoTranspirationLayer;
  FEvapotranspirationLayerData.ParamDescription := ' ' + LowerCase(StrEvapoTranspirationLayer);
  FEvapotranspirationLayerData.DataType := rdtInteger;
  AddTimeList(FEvapotranspirationLayerData);
  if Model <> nil then
  begin
    FEvapotranspirationLayerData.OnInvalidate :=
      (Model as TCustomModel).InvalidateMfEvtEvapLayer;
  end;
end;

destructor TEvtLayerTimeListLink.Destroy;
begin
  FEvapotranspirationLayerData.Free;
  inherited;
end;

{ TEvtSurfDepthListLink }

procedure TEvtSurfDepthListLink.CreateTimeLists;
var
  LocalModel: TCustomModel;
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
    LocalModel := Model as TCustomModel;
    FEvapotranspirationSurfaceData.OnInvalidate := LocalModel.InvalidateMfEvtEvapSurface;
    FEvapotranspirationDepthData.OnInvalidate := LocalModel.InvalidateMfEvtEvapDepth;
  end;
end;

destructor TEvtSurfDepthListLink.Destroy;
begin
  FEvapotranspirationSurfaceData.Free;
  FEvapotranspirationDepthData.Free;
  inherited;
end;

end.
