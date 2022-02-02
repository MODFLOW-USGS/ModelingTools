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
    // MODFLOW 6 TimeSeries (unused)
    DepthFractionTimeSeries: array of string;
    EtFractionTimeSeries: array of string;
    // PEST
    SurfacePest: string;
    SurfacePestSeries: string;
    SurfacePestMethod: TPestParamMethod;
    DepthPest: string;
    DepthPestSeries: string;
    DepthPestMethod: TPestParamMethod;
    // MODFLOW 6 TimeSeries
    SurfaceTimeSeries: string;
    DepthTimeSeries: string;
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

  TEtsStringValueItem = class(TCustomStringValueItem)
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  end;

  TStringCollectionPurpose = (scpDepthFractions, scpEtFractions);
  TEtsSurfDepthCollection = class;

  TEtsStringCollection = class(TCustomStringCollection)
  private
    FPurpose: TStringCollectionPurpose;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Model: TBaseModel; ScreenObject: TObject;
      EtsSurfDepthCollection: TCustomListArrayBoundColl);
    property Purpose: TStringCollectionPurpose read FPurpose write FPurpose;
  end;

  // @name represents a MODFLOW ET layer for one time interval.
  // @name is stored by @link(TEtsSurfDepthCollection).
  TEtsSurfDepthItem = class(TCustomModflowBoundaryItem)
  private
    FEvapotranspirationSurface: TFormulaObject;
    FEvapotranspirationDepth: TFormulaObject;
    FDepthFractions: TEtsStringCollection;
    FEtFractions: TEtsStringCollection;
    // See @link(EvapotranspirationSurface).
    procedure SetEvapotranspirationSurface(const Value: string);
    procedure SetEvapotranspirationDepth(const Value: string);
    procedure SetDepthFractions(const Value: TEtsStringCollection);
    procedure SetEtFractions(const Value: TEtsStringCollection);
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
    property EtFractions: TEtsStringCollection read FEtFractions
      write SetEtFractions;
    property DepthFractions: TEtsStringCollection read FDepthFractions
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
    function GetDepthPest: string;
    function GetDepthPestMethod: TPestParamMethod;
    function GetDepthPestSeries: string;
    function GetSurfacePest: string;
    function GetSurfacePestMethod: TPestParamMethod;
    function GetSurfacePestSeries: string;
    function GetDepthTimeSeries: string;
    function GetSurfaceTimeSeries: string;
    procedure SetDepthTimeSeries(const Value: string);
    procedure SetSurfaceTimeSeries(const Value: string);
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
    function GetPestName(Index: Integer): string; override;
    function GetPestSeriesMethod(Index: Integer): TPestParamMethod; override;
    function GetPestSeriesName(Index: Integer): string; override;
    function GetMf6TimeSeriesName(Index: Integer): string; override;
    procedure SetMf6TimeSeriesName(Index: Integer; const Value: string); override;
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
    //PEST properties
    property SurfacePest: string read GetSurfacePest;
    property SurfacePestSeries: string read GetSurfacePestSeries;
    property SurfacePestMethod: TPestParamMethod read GetSurfacePestMethod;
    property DepthPest: string read GetDepthPest;
    property DepthPestSeries: string read GetDepthPestSeries;
    property DepthPestMethod: TPestParamMethod read GetDepthPestMethod;
    property SurfaceTimeSeries: string read GetSurfaceTimeSeries
      write SetSurfaceTimeSeries;
    property DepthTimeSeries: string read GetDepthTimeSeries
      write SetDepthTimeSeries;
  end;

  TEtsTimeListLink = class(TEvtTimeListLink)
  private
    FConcList: TList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  TEtsCollection = class;

  TEtsGwtConcCollection = class(TGwtConcStringCollection)
    constructor Create(Model: TBaseModel; AScreenObject: TObject;
      ParentCollection: TEtsCollection);
  end;

  TEtsCollection = class(TEvtCollection)
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AssignConcentrationArrayCellValues(DataSets: TList; ItemIndex: Integer;
      AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
      PestItemNames, TimeSeriesNames: TStringListObjectList); override;
    procedure InitializeTimeLists(ListOfTimeLists: TList;
      AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
      PestItemNames, TimeSeriesNames: TStringListObjectList; Writer: TObject); override;
    function SpeciesCount: Integer; override;
    procedure InvalidateGwtConcentrations(Sender: TObject); override;
  public
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
    FCurrentParameter: TModflowTransientListParameter;
    FPestDepthMethod: TPestParamMethod;
    FPestEvapotranspirationRateMethod: TPestParamMethod;
    FPestSurfaceMethod: TPestParamMethod;
    FPestEvapotranspirationRateFormula: TFormulaObject;
    FPestSurfaceFormula: TFormulaObject;
    FPestDepthFormula: TFormulaObject;
    FPestDepthObserver: TObserver;
    FPestEvapotranspirationRateObserver: TObserver;
    FPestSurfaceObserver: TObserver;
    FUsedObserver: TObserver;
    FPestConcentrationMethods: TPestMethodCollection;
    FPestConcentrationFormulas: TEtsGwtConcCollection;
    FConcentrationObservers: TObserverList;
    procedure SetEvapotranspirationLayers(const Value: TEtsLayerCollection);
    procedure SetEvtSurfDepthCollection(const Value: TEtsSurfDepthCollection);
    function GetTimeVaryingEvapotranspirationLayers: boolean;
    procedure AssignEvapotranspirationLayerCells(BoundaryStorage: TEvtLayerStorage;
      ValueTimeList: TList);
    procedure AssignSurfaceDepthCells(AModel: TBaseModel;
      BoundaryStorage: TEtsSurfDepthStorage; ValueTimeList: TList);
    function GetPestDepthFormula: string;
    function GetPestDepthObserver: TObserver;
    function GetPestEvapotranspirationRateFormula: string;
    function GetPestEvapotranspirationRateObserver: TObserver;
    function GetPestSurfaceObserver: TObserver;
    function GetPestSurfaceFormula: string;
    procedure SetPestDepthFormula(const Value: string);
    procedure SetPestDepthMethod(const Value: TPestParamMethod);
    procedure SetPestEvapotranspirationRateFormula(const Value: string);
    procedure SetPestEvapotranspirationRateMethod(
      const Value: TPestParamMethod);
    procedure SetPestSurfaceFormula(const Value: string);
    procedure SetPestSurfaceMethod(const Value: TPestParamMethod);
    procedure InvalidateDepthData(Sender: TObject);
    procedure InvalidateEvapotranspirationRateData(Sender: TObject);
    procedure InvalidateSurfaceData(Sender: TObject);
    procedure InvalidateConcData(Sender: TObject);
    procedure SetPestConcentrationFormulas(const Value: TEtsGwtConcCollection);
    procedure SetPestConcentrationMethods(const Value: TPestMethodCollection);
    function GetConcentrationObserver(const Index: Integer): TObserver;
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
    // PEST
    procedure HandleChangedValue(Observer: TObserver); //override;
    function GetUsedObserver: TObserver; //override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure CreateFormulaObjects; //override;
    function BoundaryObserverPrefix: string; override;
    procedure CreateObservers; //override;
    function GetPestBoundaryFormula(FormulaIndex: integer): string; override;
    procedure SetPestBoundaryFormula(FormulaIndex: integer;
      const Value: string); override;
    function GetPestBoundaryMethod(FormulaIndex: integer): TPestParamMethod; override;
    procedure SetPestBoundaryMethod(FormulaIndex: integer;
      const Value: TPestParamMethod); override;
    property PestEvapotranspirationRateObserver: TObserver
      read GetPestEvapotranspirationRateObserver;
    property PestSurfaceObserver: TObserver
      read GetPestSurfaceObserver;
    property PestDepthObserver: TObserver
      read GetPestDepthObserver;
    property ConcentrationObserver[const Index: Integer]: TObserver
      read GetConcentrationObserver;
  public
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent);override;
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
      AModel: TBaseModel; Writer: TObject); override;
    function Used: boolean; override;
    procedure EvaluateArrayBoundaries(AModel: TBaseModel; Writer: TObject); override;
    function NonParameterColumns: integer; override;
    property TimeVaryingEvapotranspirationLayers: boolean
      read GetTimeVaryingEvapotranspirationLayers;
    procedure GetEvapotranspirationLayerCells(LayerTimeList: TList; AModel: TBaseModel);
    procedure GetEvapotranspirationSurfaceDepthCells(LayerTimeList: TList; AModel: TBaseModel);
    procedure InvalidateDisplay; override;
    procedure Clear; override;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
  published
    property EvapotranspirationLayers: TEtsLayerCollection
      read FEvapotranspirationLayers write SetEvapotranspirationLayers;
    property EtsSurfDepthCollection: TEtsSurfDepthCollection
      read FEvtSurfDepthCollection write SetEvtSurfDepthCollection;
    property Interp;
    property PestEvapotranspirationRateFormula: string
      read GetPestEvapotranspirationRateFormula
      write SetPestEvapotranspirationRateFormula
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestEvapotranspirationRateMethod: TPestParamMethod
      read FPestEvapotranspirationRateMethod
      write SetPestEvapotranspirationRateMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestSurfaceFormula: string read GetPestSurfaceFormula
      write SetPestSurfaceFormula
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestSurfaceMethod: TPestParamMethod read FPestSurfaceMethod
      write SetPestSurfaceMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestDepthFormula: string read GetPestDepthFormula
      write SetPestDepthFormula
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestDepthMethod: TPestParamMethod read FPestDepthMethod
      write SetPestDepthMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestConcentrationFormulas: TEtsGwtConcCollection
      read FPestConcentrationFormulas write SetPestConcentrationFormulas
      {$IFNDEF GWT}
      Stored False
      {$ENDIF}
      ;
    property PestConcentrationMethods: TPestMethodCollection
      read FPestConcentrationMethods write SetPestConcentrationMethods
      {$IFNDEF GWT}
      Stored False
      {$ENDIF}
      ;
  end;

resourcestring
  StrEvapotranspirationD_ETS = 'Evapotranspiration depth in the ETS package ' +
  'is less than zero';
  StrEvaporationDepthFr = 'Evaporation depth or rate fractions not assigned';

const
  EtsSurfacePosition = 0;
  EtsDepthPosition = 1;

const
  RateBoundaryPosition = 0;
  SurfaceBoundaryPosition = 1;
  DepthBoundaryPosition = 2;
  EtsStartConcentration = 3;

implementation

uses ScreenObjectUnit, PhastModelUnit, ModflowTimeUnit, TempFiles,
  frmGoPhastUnit, frmErrorsAndWarningsUnit, ModflowTimeSeriesUnit,
  ModflowParameterUnit, ModelMuseUtilities, CustomModflowWriterUnit,
  System.Generics.Collections;

resourcestring
  StrFractionalRateS = 'Fractional rate %s';
  StrInSEvaportionsDe = 'In %s evaporation depth or rate fractions have not been ass' +
  'igned.';

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

    PestEvapotranspirationRateFormula := SourceBoundary.PestEvapotranspirationRateFormula;
    PestEvapotranspirationRateMethod := SourceBoundary.PestEvapotranspirationRateMethod;
    PestSurfaceFormula := SourceBoundary.PestSurfaceFormula;
    PestSurfaceMethod := SourceBoundary.PestSurfaceMethod;
    PestDepthFormula := SourceBoundary.PestDepthFormula;
    PestDepthMethod := SourceBoundary.PestDepthMethod;

    PestConcentrationFormulas := SourceBoundary.PestConcentrationFormulas;
    PestConcentrationMethods := SourceBoundary.PestConcentrationMethods;
  end;
  if Source is TEvtBoundary then
  begin
    EvtBoundary := TEvtBoundary(Source);
    EvapotranspirationLayers.Assign(EvtBoundary.EvapotranspirationLayers);
//    TimeVaryingEvapotranspirationLayers := EvtBoundary.TimeVaryingEvapotranspirationLayers;
    EtsSurfDepthCollection.Assign(EvtBoundary.EvtSurfDepthCollection);
    FNonParameterColumns := EvtBoundary.NonParameterColumns;

    PestEvapotranspirationRateFormula := EvtBoundary.PestEvapotranspirationRateFormula;
    PestEvapotranspirationRateMethod := EvtBoundary.PestEvapotranspirationRateMethod;
    PestSurfaceFormula := EvtBoundary.PestSurfaceFormula;
    PestSurfaceMethod := EvtBoundary.PestSurfaceMethod;
    PestDepthFormula := EvtBoundary.PestDepthFormula;
    PestDepthMethod := EvtBoundary.PestDepthMethod;
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
  LocalScreenObject: TScreenObject;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TEvtStorage;
  Assert(ScreenObject <> nil);
  LocalScreenObject := ScreenObject as TScreenObject;
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
        if FCurrentParameter <> nil then
        begin
          BoundaryValues.EvapotranspirationRate :=
            BoundaryValues.EvapotranspirationRate * FCurrentParameter.Value;
          BoundaryValues.EvapotranspirationRateAnnotation := Format(Str0sMultipliedByT, [
            BoundaryValues.EvapotranspirationRateAnnotation, FCurrentParameter.ParameterName]);
          BoundaryValues.ETParameterName := FCurrentParameter.ParameterName;
          BoundaryValues.ETParameterValue := FCurrentParameter.Value;
        end
        else
        begin
          BoundaryValues.ETParameterName := '';
          BoundaryValues.ETParameterValue := 1;
        end;
        Cell := TEvt_Cell.Create;
        Cell.BoundaryIndex := BoundaryIndex;
        Assert(ScreenObject <> nil);
        Cell.IFace := LocalScreenObject.IFace;
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

function TEtsBoundary.BoundaryObserverPrefix: string;
begin
  result := 'PestEts_';
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
  FPestConcentrationFormulas:= TEtsGwtConcCollection.Create(Model, ScreenObject, nil);
  FPestConcentrationMethods := TPestMethodCollection.Create(Model);
  FConcentrationObservers := TObserverList.Create;

  FEvapotranspirationLayers := TEtsLayerCollection.Create(self, Model, ScreenObject);
  FEvtSurfDepthCollection := TEtsSurfDepthCollection.Create(self, Model, ScreenObject);

  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

  PestEvapotranspirationRateFormula := '';
  PestSurfaceFormula := '';
  PestDepthFormula := '';
  FPestEvapotranspirationRateMethod := DefaultBoundaryMethod(RateBoundaryPosition);
  FPestSurfaceMethod := DefaultBoundaryMethod(SurfaceBoundaryPosition);
  FPestDepthMethod := DefaultBoundaryMethod(DepthBoundaryPosition);
end;

procedure TEtsBoundary.CreateFormulaObjects;
var
  LocalModel: TPhastModel;
  ConcIndex: Integer;
begin
  FPestEvapotranspirationRateFormula := CreateFormulaObjectBlocks(dsoTop);
  FPestSurfaceFormula := CreateFormulaObjectBlocks(dsoTop);
  FPestDepthFormula := CreateFormulaObjectBlocks(dsoTop);
  LocalModel := ParentModel as TPhastModel;
  if (LocalModel <> nil) and LocalModel.GwtUsed then
  begin
    for ConcIndex := 0 to LocalModel.MobileComponents.Count - 1 do
    begin
      FPestConcentrationFormulas.Add;
    end;
  end;
end;

procedure TEtsBoundary.CreateObservers;
var
  Index: Integer;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(PestEvapotranspirationRateObserver);
    FObserverList.Add(PestSurfaceObserver);
    FObserverList.Add(PestDepthObserver);
    for Index := 0 to FPestConcentrationFormulas.Count - 1 do
    begin
      FObserverList.Add(ConcentrationObserver[Index]);
    end;
  end;
end;

class function TEtsBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    RateBoundaryPosition:
      begin
        result := ppmMultiply;
      end;
    SurfaceBoundaryPosition:
      begin
        result := ppmAdd;
      end;
    DepthBoundaryPosition:
      begin
        result := ppmMultiply;
      end;
    else
      begin
        result := inherited;
        Assert(False);
      end;
  end;
end;

destructor TEtsBoundary.Destroy;
begin
  PestEvapotranspirationRateFormula := '';
  PestSurfaceFormula := '';
  PestDepthFormula := '';

  FEvtSurfDepthCollection.Free;
  FEvapotranspirationLayers.Free;
  inherited;
  FPestConcentrationMethods.Free;
  FPestConcentrationFormulas.Free;
  FConcentrationObservers.Free;
end;

procedure TEtsBoundary.EvaluateArrayBoundaries(AModel: TBaseModel; Writer: TObject);
begin
  inherited;
  EtsSurfDepthCollection.EvaluateArrayBoundaries(AModel, Writer);
  if (AModel as TCustomModel).
    ModflowPackages.EtsPackage.TimeVaryingLayers then
  begin
    EvapotranspirationLayers.EvaluateArrayBoundaries(AModel, Writer);
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
      BoundaryStorage := EvapotranspirationLayers.Boundaries[ValueIndex,
        AModel] as TEvtLayerStorage;
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

function TEtsBoundary.GetPestBoundaryFormula(FormulaIndex: integer): string;
var
  ConcIndex: Integer;
begin
  case FormulaIndex of
    RateBoundaryPosition:
      begin
        result := PestEvapotranspirationRateFormula;
      end;
    SurfaceBoundaryPosition:
      begin
        result := PestSurfaceFormula;
      end;
    DepthBoundaryPosition:
      begin
        result := PestDepthFormula;
      end;
    else
      begin
        ConcIndex := FormulaIndex - EtsStartConcentration;
        while ConcIndex >= PestConcentrationFormulas.Count do
        begin
          PestConcentrationFormulas.Add;
        end;
        result := PestConcentrationFormulas[ConcIndex].Value;
      end;
//      Assert(False);
  end;
end;

function TEtsBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
var
  ConcIndex: Integer;
begin
  case FormulaIndex of
    RateBoundaryPosition:
      begin
        result := PestEvapotranspirationRateMethod;
      end;
    SurfaceBoundaryPosition:
      begin
        result := PestSurfaceMethod;
      end;
    DepthBoundaryPosition:
      begin
        result := PestDepthMethod;
      end;
    else
      begin
        ConcIndex := FormulaIndex - EtsStartConcentration;
        while ConcIndex >= FPestConcentrationMethods.Count do
        begin
          FPestConcentrationMethods.Add;
        end;
        result := FPestConcentrationMethods[ConcIndex].PestParamMethod;
      end;
  end;
end;

function TEtsBoundary.GetPestDepthFormula: string;
begin
  Result := FPestDepthFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(DepthBoundaryPosition);
  end;
end;

function TEtsBoundary.GetPestDepthObserver: TObserver;
begin
  if FPestDepthObserver = nil then
  begin
    CreateObserver('PestDepth_', FPestDepthObserver, nil);
    FPestDepthObserver.OnUpToDateSet := InvalidateDepthData;
  end;
  result := FPestDepthObserver;
end;

function TEtsBoundary.GetPestEvapotranspirationRateFormula: string;
begin
  Result := FPestEvapotranspirationRateFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(RateBoundaryPosition);
  end;
end;

function TEtsBoundary.GetPestEvapotranspirationRateObserver: TObserver;
begin
  if FPestEvapotranspirationRateObserver = nil then
  begin
    CreateObserver('PestEtsRate_', FPestEvapotranspirationRateObserver, nil);
    FPestEvapotranspirationRateObserver.OnUpToDateSet := InvalidateEvapotranspirationRateData;
  end;
  result := FPestEvapotranspirationRateObserver;
end;

function TEtsBoundary.GetPestSurfaceFormula: string;
begin
  Result := FPestSurfaceFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(SurfaceBoundaryPosition);
  end;
end;

function TEtsBoundary.GetPestSurfaceObserver: TObserver;
begin
  if FPestSurfaceObserver = nil then
  begin
    CreateObserver('PestEtsSurface_', FPestSurfaceObserver, nil);
    FPestSurfaceObserver.OnUpToDateSet := InvalidateSurfaceData;
  end;
  result := FPestSurfaceObserver;
end;

procedure TEtsBoundary.GetPropertyObserver(Sender: TObject; List: TList);
var
  Index: Integer;
begin
  if Sender = FPestEvapotranspirationRateFormula then
  begin
    if RateBoundaryPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[RateBoundaryPosition]);
    end;
  end;
  if Sender = FPestSurfaceFormula then
  begin
    if SurfaceBoundaryPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[SurfaceBoundaryPosition]);
    end;
  end;
  if Sender = FPestDepthFormula then
  begin
    if DepthBoundaryPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[DepthBoundaryPosition]);
    end;
  end;
  for Index := 0 to FPestConcentrationFormulas.Count - 1 do
  begin
    if FPestConcentrationFormulas[Index].ValueObject = Sender then
    begin
      List.Add(FObserverList[EtsStartConcentration + Index]);
    end;
  end;
end;

procedure TEtsBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
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
//  StressPeriods: TModflowStressPeriods;
//  StartTime: Double;
//  EndTime: Double;
//  TimeCount: Integer;
  ItemIndex: Integer;
//  TimeSeriesList: TTimeSeriesList;
//  TimeSeries: TTimeSeries;
//  SeriesIndex: Integer;
//  InitialTime: Double;
  ArrayIndex: Integer;
begin
  FCurrentParameter := nil;
  EvaluateArrayBoundaries(AModel, Writer);
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
//        StressPeriods := (AModel as TCustomModel).ModflowFullStressPeriods;
//        StartTime := StressPeriods.First.StartTime;
//        EndTime := StressPeriods.Last.EndTime;
//        TimeCount := BoundaryList.Count;
        for ItemIndex := 0 to BoundaryList.Count - 1 do
        begin
          BoundaryStorage := BoundaryList[ItemIndex];
          for ArrayIndex := 0 to Length(BoundaryStorage.EvtArray) - 1 do
          begin
            BoundaryStorage.EvtArray[ArrayIndex].ETParameterName := FCurrentParameter.ParameterName;
            BoundaryStorage.EvtArray[ArrayIndex].ETParameterValue := FCurrentParameter.Value;
          end;
//          if BoundaryStorage.StartingTime > StartTime then
//          begin
////            Inc(TimeCount);
//          end;
//          StartTime := BoundaryStorage.EndingTime;
        end;
//        BoundaryStorage := BoundaryList.Last;
//        if BoundaryStorage.EndingTime <= EndTime then
//        begin
////          Inc(TimeCount);
//        end;

//        TimeSeriesList := FCurrentParameter.TimeSeriesList;
//        TimeSeries := TTimeSeries.Create;
//        TimeSeriesList.Add(TimeSeries);
//        TimeSeries.SeriesCount := Length(BoundaryStorage.EvtArray);
//        TimeSeries.TimeCount := TimeCount;
//        TimeSeries.ParameterName := FCurrentParameter.ParameterName;
//        TimeSeries.ObjectName := (ScreenObject as TScreenObject).Name;
//        for SeriesIndex := 0 to Length(BoundaryStorage.EvtArray) - 1 do
//        begin
//          TimeSeries.SeriesNames[SeriesIndex] :=
//            Format('%0:s_%1d_%2:d', [TimeSeries.ParameterName,
//            TimeSeriesList.Count, SeriesIndex+1]);
//          TimeSeries.InterpolationMethods[SeriesIndex] := Interp;
//          TimeSeries.ScaleFactors[SeriesIndex] := FCurrentParameter.Value;
//        end;

//        TimeCount := 0;
//        StartTime := StressPeriods.First.StartTime;
//        InitialTime := StartTime;
//        for ItemIndex := 0 to BoundaryList.Count - 1 do
//        begin
////          BoundaryStorage := BoundaryList[ItemIndex];
////          if BoundaryStorage.StartingTime > StartTime then
////          begin
//////            TimeSeries.Times[TimeCount] := StartTime - InitialTime;
//////            for SeriesIndex := 0 to Length(BoundaryStorage.EvtArray) - 1 do
//////            begin
//////              if ItemIndex > 0 then
//////              begin
//////                TimeSeries.Values[SeriesIndex,TimeCount] := NoData;
//////              end
//////              else
//////              begin
//////                TimeSeries.Values[SeriesIndex,TimeCount] :=
//////                  BoundaryStorage.EvtArray[SeriesIndex].EvapotranspirationRate;
//////              end;
//////            end;
//////            Inc(TimeCount);
////          end;
////          TimeSeries.Times[TimeCount] := BoundaryStorage.StartingTime - InitialTime;
////          for SeriesIndex := 0 to Length(BoundaryStorage.EvtArray) - 1 do
////          begin
////            TimeSeries.Values[SeriesIndex,TimeCount] :=
////              BoundaryStorage.EvtArray[SeriesIndex].EvapotranspirationRate;
////            BoundaryStorage.EvtArray[SeriesIndex].TimeSeriesName :=
////              TimeSeries.SeriesNames[SeriesIndex];
////          end;
////          StartTime := BoundaryStorage.EndingTime;
////          Inc(TimeCount);
//        end;
//        BoundaryStorage := BoundaryList.Last;
//        if BoundaryStorage.EndingTime <= EndTime then
//        begin
//          TimeSeries.Times[TimeCount] := EndTime - InitialTime;
//          for SeriesIndex := 0 to Length(BoundaryStorage.EvtArray) - 1 do
//          begin
//            TimeSeries.Values[SeriesIndex,TimeCount] :=
//              BoundaryStorage.EvtArray[SeriesIndex].EvapotranspirationRate;
//          end;
//        end;
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

function TEtsBoundary.GetConcentrationObserver(const Index: Integer): TObserver;
var
  AObserver: TObserver;
begin
  while Index >= FConcentrationObservers.Count do
  begin
    CreateObserver(Format('EtsConc_%d', [Index+1]), AObserver, nil);
    FConcentrationObservers.Add(AObserver);
    AObserver.OnUpToDateSet := InvalidateConcData;
  end;
  result := FConcentrationObservers[Index];
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

function TEtsBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('PestEts_Used_', FUsedObserver, nil);
  end;
  result := FUsedObserver;
end;

procedure TEtsBoundary.HandleChangedValue(Observer: TObserver);
begin
//  inherited;
  InvalidateDisplay;
end;

procedure TEtsBoundary.InvalidateConcData(Sender: TObject);
var
  PhastModel: TPhastModel;
begin
  PhastModel := frmGoPhast.PhastModel;
  if PhastModel.Clearing then
  begin
    Exit;
  end;
  PhastModel.InvalidateEtsConc(self);
end;

procedure TEtsBoundary.InvalidateDepthData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfEtsEvapDepth(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateMfEtsEvapDepth(self);
    end;
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
    LocalModel.InvalidateEtsConc(self);
  end;
end;

procedure TEtsBoundary.InvalidateEvapotranspirationRateData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfEtsEvapRate(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateMfEtsEvapRate(self);
    end;
  end;
end;

procedure TEtsBoundary.InvalidateSurfaceData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfEtsEvapSurface(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateMfEtsEvapSurface(self);
    end;
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


procedure TEtsBoundary.SetPestBoundaryFormula(FormulaIndex: integer;
  const Value: string);
var
  ConcIndex: Integer;
begin
  case FormulaIndex of
    RateBoundaryPosition:
      begin
        PestEvapotranspirationRateFormula := Value;
      end;
    SurfaceBoundaryPosition:
      begin
        PestSurfaceFormula := Value;
      end;
    DepthBoundaryPosition:
      begin
        PestDepthFormula := Value;
      end;
    else
      begin
        ConcIndex := FormulaIndex - EtsStartConcentration;
        while ConcIndex >= PestConcentrationFormulas.Count do
        begin
          PestConcentrationFormulas.Add;
        end;
        PestConcentrationFormulas[ConcIndex].Value := Value;
      end;
  end;
end;

procedure TEtsBoundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
var
  ConcIndex: Integer;
begin
  case FormulaIndex of
    RateBoundaryPosition:
      begin
        PestEvapotranspirationRateMethod := Value;
      end;
    SurfaceBoundaryPosition:
      begin
        PestSurfaceMethod := Value;
      end;
    DepthBoundaryPosition:
      begin
        PestDepthMethod := Value;
      end;
    else
      begin
        ConcIndex := FormulaIndex - EtsStartConcentration;
        while ConcIndex >= FPestConcentrationMethods.Count do
        begin
          FPestConcentrationMethods.Add;
        end;
        FPestConcentrationMethods[ConcIndex].PestParamMethod := Value;
      end;
  end;
end;

procedure TEtsBoundary.SetPestConcentrationFormulas(
  const Value: TEtsGwtConcCollection);
begin
  FPestConcentrationFormulas.Assign(Value);
end;

procedure TEtsBoundary.SetPestConcentrationMethods(
  const Value: TPestMethodCollection);
begin
  FPestConcentrationMethods.Assign(Value);
end;

procedure TEtsBoundary.SetPestDepthFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, DepthBoundaryPosition, FPestDepthFormula);
end;

procedure TEtsBoundary.SetPestDepthMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestDepthMethod, Value);
end;

procedure TEtsBoundary.SetPestEvapotranspirationRateFormula(
  const Value: string);
begin
  UpdateFormulaBlocks(Value, RateBoundaryPosition, FPestEvapotranspirationRateFormula);
end;

procedure TEtsBoundary.SetPestEvapotranspirationRateMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestEvapotranspirationRateMethod, Value);
end;

procedure TEtsBoundary.SetPestSurfaceFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, SurfaceBoundaryPosition, FPestSurfaceFormula);
end;

procedure TEtsBoundary.SetPestSurfaceMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestSurfaceMethod, Value);
end;

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
  SurfaceObserver := FObserverList[EtsSurfacePosition];
  SurfaceObserver.OnUpToDateSet := ParentCollection.InvalidateEtSurface;
  DepthObserver := FObserverList[EtsDepthPosition];
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
  LocalCollection: TCustomListArrayBoundColl;
begin
  inherited;
  LocalCollection := Collection as TCustomListArrayBoundColl;
  Model := LocalCollection.Model;
  FEtFractions := TEtsStringCollection.Create(Model, ScreenObject, LocalCollection);
  FEtFractions.Purpose := scpEtFractions;
  FDepthFractions := TEtsStringCollection.Create(Model, ScreenObject, LocalCollection);
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
  Item: TEtsStringValueItem;
  Collection: TEtsStringCollection;
begin
  case Index of
    EtsSurfacePosition: result := EvapotranspirationSurface;
    EtsDepthPosition: result := EvapotranspirationDepth;
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
        Item := Collection.Items[Index] as TEtsStringValueItem;
        result := Item.Value;
      end;
  end;
end;

function TEtsSurfDepthItem.GetEvapotranspirationDepth: string;
begin
  Result := FEvapotranspirationDepth.Formula;
  ResetItemObserver(EtsDepthPosition);
end;

function TEtsSurfDepthItem.GetEvapotranspirationSurface: string;
begin
  Result := FEvapotranspirationSurface.Formula;
  ResetItemObserver(EtsSurfacePosition);
end;

procedure TEtsSurfDepthItem.GetPropertyObserver(Sender: TObject; List: TList);
var
  Index: integer;
  Item: TEtsStringValueItem;
begin
  if Sender = FEvapotranspirationSurface then
  begin
    List.Add(FObserverList[EtsSurfacePosition]);
  end;
  if Sender = FEvapotranspirationDepth then
  begin
    List.Add(FObserverList[EtsDepthPosition]);
  end;

  for Index := 0 to EtFractions.Count - 1 do
  begin
    Item := EtFractions.Items[Index] as TEtsStringValueItem;
    if Item.FValue = Sender then
    begin
      List.Add(Item.Observer);
    end;
  end;
  for Index := 0 to DepthFractions.Count - 1 do
  begin
    Item := DepthFractions.Items[Index] as TEtsStringValueItem;
    if Item.FValue = Sender then
    begin
      List.Add(Item.Observer);
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
  Item: TEtsStringValueItem;
  Collection: TEtsStringCollection;
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
        Item := Collection.Items[Index] as TEtsStringValueItem;
        Item.Value := Value;
      end;
  end;
end;

procedure TEtsSurfDepthItem.SetDepthFractions(const Value: TEtsStringCollection);
begin
  FDepthFractions.Assign(Value);
end;

procedure TEtsSurfDepthItem.SetEtFractions(const Value: TEtsStringCollection);
begin
  FEtFractions.Assign(Value);
end;

procedure TEtsSurfDepthItem.SetEvapotranspirationDepth(const Value: string);
begin
  UpdateFormulaBlocks(Value, EtsDepthPosition, FEvapotranspirationDepth);
end;

procedure TEtsSurfDepthItem.SetEvapotranspirationSurface(const Value: string);
begin
  UpdateFormulaBlocks(Value, EtsSurfacePosition, FEvapotranspirationSurface);
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
  ItemIndex: Integer; AModel: TBaseModel; PestSeries: TStringList;
  PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList);
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
  LocalSurfacePestSeries: string;
  LocalSurfacePestMethod: TPestParamMethod;
  SurfacePestItems: TStringList;
  LocalSurfacePest: string;
  LocalDepthPestSeries: string;
  LocalDepthPestMethod: TPestParamMethod;
  DepthPestItems: TStringList;
  LocalDepthPest: string;
  SurfaceTimeSeries: TStringList;
  LocalSurfaceTimeSeries: string;
  DepthTimeSeries: TStringList;
  LocalDepthTimeSeries: string;
begin
  LocalModel := AModel as TCustomModel;
  SegmentCount := LocalModel.
    ModflowPackages.EtsPackage.SegmentCount;
  BoundaryIndex := 0;
  EvapotranspirationSurfaceArray := DataSets[EtsSurfacePosition];
  EvapotranspirationDepthArray := DataSets[EtsDepthPosition];
  Boundary := Boundaries[ItemIndex, AModel] as TEtsSurfDepthStorage;
  EvapotranspirationSurfaceArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
    LayerMax, RowMax, ColMax);

  LocalSurfacePestSeries := PestSeries[EtsSurfacePosition];
  LocalSurfacePestMethod := PestMethods[EtsSurfacePosition];
  SurfacePestItems := PestItemNames[EtsSurfacePosition];
  LocalSurfacePest := SurfacePestItems[ItemIndex];

  SurfaceTimeSeries := TimeSeriesNames[EtsSurfacePosition];
  LocalSurfaceTimeSeries := SurfaceTimeSeries[ItemIndex];

  LocalDepthPestSeries := PestSeries[EtsDepthPosition];
  LocalDepthPestMethod := PestMethods[EtsDepthPosition];
  DepthPestItems := PestItemNames[EtsDepthPosition];
  LocalDepthPest := DepthPestItems[ItemIndex];

  DepthTimeSeries := TimeSeriesNames[EtsDepthPosition];
  LocalDepthTimeSeries := DepthTimeSeries[ItemIndex];

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

                SurfacePest := LocalSurfacePest;
                SurfacePestSeries := LocalSurfacePestSeries;
                SurfacePestMethod := LocalSurfacePestMethod;

                DepthPest := LocalDepthPest;
                DepthPestSeries := LocalDepthPestSeries;
                DepthPestMethod := LocalDepthPestMethod;

                SurfaceTimeSeries := LocalSurfaceTimeSeries;
                DepthTimeSeries := LocalDepthTimeSeries;

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
  AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList; Writer: TObject);
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
  AScreenObject: TScreenObject;
  LocalModel: TCustomModel;
//  CustomWriter: TCustomFileWriter;
  ItemFormula: string;
  SeriesName: string;
  SeriesMethod: TPestParamMethod;
  PestItems: TStringList;
  TimeSeriesItems: TStringList;
begin
//  CustomWriter := nil;
  LocalModel := AModel as TCustomModel;
  Boundary := BoundaryGroup as TEtsBoundary;
  ScreenObject := Boundary.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);

  SeriesName := BoundaryGroup.PestBoundaryFormula[SurfaceBoundaryPosition];
  PestSeries.Add(SeriesName);
  SeriesMethod := BoundaryGroup.PestBoundaryMethod[SurfaceBoundaryPosition];
  PestMethods.Add(SeriesMethod);

  PestItems := TStringList.Create;
  PestItemNames.Add(PestItems);
  TimeSeriesItems := TStringList.Create;
  TimeSeriesNames.Add(TimeSeriesItems);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TEtsSurfDepthItem;
    BoundaryValues[Index].Time := Item.StartTime;

    ItemFormula := Item.EvapotranspirationSurface;
    AssignBoundaryFormula(AModel, SeriesName, SeriesMethod,
      PestItems, TimeSeriesItems, ItemFormula, Writer, BoundaryValues[Index]);

//    BoundaryValues[Index].Formula := Item.EvapotranspirationSurface;
  end;

  ALink := TimeListLink.GetLink(AModel) as TEtsSurfDepthTimeListLink;
  EvapotranspirationSurfaceData := ALink.FEvapotranspirationSurfaceData;
  EvapotranspirationSurfaceData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(EvapotranspirationSurfaceData.Count = Count);

  SeriesName := BoundaryGroup.PestBoundaryFormula[DepthBoundaryPosition];
  PestSeries.Add(SeriesName);
  SeriesMethod := BoundaryGroup.PestBoundaryMethod[DepthBoundaryPosition];
  PestMethods.Add(SeriesMethod);

  PestItems := TStringList.Create;
  PestItemNames.Add(PestItems);
  TimeSeriesItems := TStringList.Create;
  TimeSeriesNames.Add(TimeSeriesItems);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TEtsSurfDepthItem;
    BoundaryValues[Index].Time := Item.StartTime;

    ItemFormula := Item.EvapotranspirationDepth;
    AssignBoundaryFormula(AModel, SeriesName, SeriesMethod,
      PestItems, TimeSeriesItems, ItemFormula, Writer, BoundaryValues[Index]);

//    BoundaryValues[Index].Formula := Item.EvapotranspirationDepth;
  end;
  EvapotranspirationDepthData := ALink.FEvapotranspirationDepthData;
  EvapotranspirationDepthData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(EvapotranspirationDepthData.Count = Count);

  // assign fractional depths and rates.

  if Count > 0 then
  begin
    for SegmentIndex := 1 to LocalModel.
      ModflowPackages.EtsPackage.SegmentCount - 1 do
    begin
      FractionalDepthTimeList := TimeLists[SegmentIndex*2, AModel];
      for Index := 0 to Count - 1 do
      begin
        Item := Items[Index] as TEtsSurfDepthItem;
        BoundaryValues[Index].Time := Item.StartTime;
        if SegmentIndex > Item.DepthFractions.Count then
        begin
          AScreenObject := ScreenObject as TScreenObject;
          frmErrorsAndWarnings.AddError(Model,
            StrEvaporationDepthFr,
            Format(StrInSEvaportionsDe, [AScreenObject.Name]), AScreenObject);
          BoundaryValues[Index].Formula := '';
        end
        else
        begin
          BoundaryValues[Index].Formula := (Item.DepthFractions.
            Items[SegmentIndex-1] as TEtsStringValueItem).Value;
        end;
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
        if SegmentIndex > Item.EtFractions.Count then
        begin
          AScreenObject := ScreenObject as TScreenObject;
          frmErrorsAndWarnings.AddError(Model,
            StrEvaporationDepthFr,
            Format(StrInSEvaportionsDe, [AScreenObject.Name]), AScreenObject);
          BoundaryValues[Index].Formula := '';
        end
        else
        begin
          BoundaryValues[Index].Formula := (Item.EtFractions.
            Items[SegmentIndex-1] as TEtsStringValueItem).Value;
        end;
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
    for SegmentIndex := 1 to LocalModel.
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
    if PhastModel.Clearing then
    begin
      Exit;
    end;
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
    if PhastModel.Clearing then
    begin
      Exit;
    end;
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
    if PhastModel.Clearing then
    begin
      Exit;
    end;
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
    if PhastModel.Clearing then
    begin
      Exit;
    end;
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
    SetLength(EtsSurfDepthStorage.EtsSurfDepthArray[Index].
      DepthFractionTimeSeries, SegmentCount-1);
    SetLength(EtsSurfDepthStorage.EtsSurfDepthArray[Index].
      EtFractionTimeSeries, SegmentCount-1);
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

function TEtsSurfDepth_Cell.GetDepthPest: string;
begin
  result := Values.DepthPest;
end;

function TEtsSurfDepth_Cell.GetDepthPestMethod: TPestParamMethod;
begin
  result := Values.DepthPestMethod;
end;

function TEtsSurfDepth_Cell.GetDepthPestSeries: string;
begin
  result := Values.DepthPestSeries;
end;

function TEtsSurfDepth_Cell.GetDepthTimeSeries: string;
begin
  result := Values.DepthTimeSeries;
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

function TEtsSurfDepth_Cell.GetMf6TimeSeriesName(Index: Integer): string;
begin
  case Index of
    EtsSurfacePosition:
      begin
        result := SurfaceTimeSeries;
      end;
    EtsDepthPosition:
      begin
        result := DepthTimeSeries;
      end;
    else
      begin
        result := inherited;
//        Assert(False);
      end;
  end;
end;

function TEtsSurfDepth_Cell.GetPestName(Index: Integer): string;
begin
  case Index of
    EtsSurfacePosition:
      begin
        result := SurfacePest;
      end;
    EtsDepthPosition:
      begin
        result := DepthPest;
      end;
    else
      begin
        result := inherited;
//        Assert(False);
      end;
  end;
end;

function TEtsSurfDepth_Cell.GetPestSeriesMethod(
  Index: Integer): TPestParamMethod;
begin
  case Index of
    EtsSurfacePosition:
      begin
        result := SurfacePestMethod;
      end;
    EtsDepthPosition:
      begin
        result := DepthPestMethod;
      end;
    else
      begin
        result := inherited;
//        Assert(False);
      end;
  end;
end;

function TEtsSurfDepth_Cell.GetPestSeriesName(Index: Integer): string;
begin
  case Index of
    EtsSurfacePosition:
      begin
        result := SurfacePestSeries;
      end;
    EtsDepthPosition:
      begin
        result := DepthPestSeries;
      end;
    else
      begin
        result := inherited;
//        Assert(False);
      end;
  end;
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

function TEtsSurfDepth_Cell.GetSurfacePest: string;
begin
  result := Values.SurfacePest;
end;

function TEtsSurfDepth_Cell.GetSurfacePestMethod: TPestParamMethod;
begin
  result := Values.SurfacePestMethod;
end;

function TEtsSurfDepth_Cell.GetSurfacePestSeries: string;
begin
  result := Values.SurfacePestSeries;
end;

function TEtsSurfDepth_Cell.GetSurfaceTimeSeries: string;
begin
  result := Values.SurfaceTimeSeries;
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

procedure TEtsSurfDepth_Cell.SetDepthTimeSeries(const Value: string);
begin
  Values.DepthTimeSeries := Value;
end;

procedure TEtsSurfDepth_Cell.SetLayer(const Value: integer);
begin
  Values.Cell.Layer := Value;
end;

procedure TEtsSurfDepth_Cell.SetMf6TimeSeriesName(Index: Integer;
  const Value: string);
begin
  case Index of
    EtsSurfacePosition:
      begin
        SurfaceTimeSeries := Value;
      end;
    EtsDepthPosition:
      begin
        DepthTimeSeries := Value;
      end;
    else
      begin
        inherited;
//        Assert(False);
      end;
  end;
end;

procedure TEtsSurfDepth_Cell.SetRow(const Value: integer);
begin
  Values.Cell.Row := Value;
end;

procedure TEtsSurfDepth_Cell.SetSurfaceTimeSeries(const Value: string);
begin
  Values.SurfaceTimeSeries := Value;
end;

{ TEtsSurfDepthRecord }

procedure TEtsSurfDepthRecord.Assign(const Item: TEtsSurfDepthRecord);
begin
  self := Item;
  SetLength(DepthFractions, Length(DepthFractions));
  SetLength(EtFractions, Length(EtFractions));
  SetLength(DepthFractionAnnotations, Length(DepthFractionAnnotations));
  SetLength(EtFractionAnnotations, Length(EtFractionAnnotations));
  SetLength(DepthFractionTimeSeries, Length(DepthFractionTimeSeries));
  SetLength(EtFractionTimeSeries, Length(EtFractionTimeSeries));
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

  for Index := 0 to Count - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(DepthFractionAnnotations[Index]));
  end;
  for Index := 0 to Count - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(EtFractionAnnotations[Index]));
  end;

  WriteCompInt(Comp, Strings.IndexOf(SurfacePest));
  WriteCompInt(Comp, Strings.IndexOf(SurfacePestSeries));
  WriteCompInt(Comp, Ord(SurfacePestMethod));

  WriteCompInt(Comp, Strings.IndexOf(DepthPest));
  WriteCompInt(Comp, Strings.IndexOf(DepthPestSeries));
  WriteCompInt(Comp, Ord(DepthPestMethod));

  WriteCompInt(Comp, Strings.IndexOf(SurfaceTimeSeries));
  WriteCompInt(Comp, Strings.IndexOf(DepthTimeSeries));
  for Index := 0 to Count - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(DepthFractionTimeSeries[Index]));
  end;
  for Index := 0 to Count - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(EtFractionTimeSeries[Index]));
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
  Strings.Add(SurfaceTimeSeries);
  Strings.Add(DepthTimeSeries);

  for Index := 0 to Count - 1 do
  begin
    Strings.Add(DepthFractionAnnotations[Index]);
  end;
  for Index := 0 to Count - 1 do
  begin
    Strings.Add(EtFractionAnnotations[Index]);
  end;
  for Index := 0 to Count - 1 do
  begin
    Strings.Add(DepthFractionTimeSeries[Index]);
  end;
  for Index := 0 to Count - 1 do
  begin
    Strings.Add(EtFractionTimeSeries[Index]);
  end;

  Strings.Add(SurfacePest);
  Strings.Add(SurfacePestSeries);
  Strings.Add(DepthPest);
  Strings.Add(DepthPestSeries);
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

  SetLength(DepthFractionAnnotations, Count);
  for Index := 0 to Count - 1 do
  begin
    DepthFractionAnnotations[Index] := Annotations[ReadCompInt(Decomp)];
  end;

  SetLength(EtFractionAnnotations, Count);
  for Index := 0 to Count - 1 do
  begin
    EtFractionAnnotations[Index] := Annotations[ReadCompInt(Decomp)];
  end;

  SurfacePest := Annotations[ReadCompInt(Decomp)];
  SurfacePestSeries := Annotations[ReadCompInt(Decomp)];
  SurfacePestMethod := TPestParamMethod(ReadCompInt(Decomp));

  DepthPest := Annotations[ReadCompInt(Decomp)];
  DepthPestSeries := Annotations[ReadCompInt(Decomp)];
  DepthPestMethod := TPestParamMethod(ReadCompInt(Decomp));

  SurfaceTimeSeries := Annotations[ReadCompInt(Decomp)];
  DepthTimeSeries := Annotations[ReadCompInt(Decomp)];
  SetLength(DepthFractionTimeSeries, Count);
  for Index := 0 to Count - 1 do
  begin
    DepthFractionTimeSeries[Index] := Annotations[ReadCompInt(Decomp)];
  end;
  SetLength(EtFractionTimeSeries, Count);
  for Index := 0 to Count - 1 do
  begin
    EtFractionTimeSeries[Index] := Annotations[ReadCompInt(Decomp)];
  end;
end;

{ TEtsStringValueItem }

constructor TEtsStringValueItem.Create(Collection: TCollection);
var
  SCollection: TEtsStringCollection;
  EtsSurfDepth: TEtsSurfDepthCollection;
  LocalScreenObject: TScreenObject;
begin
  inherited;
  SCollection := StringCollection as TEtsStringCollection;
  EtsSurfDepth := SCollection.ParentCollection as TEtsSurfDepthCollection;
  case SCollection.Purpose of
    scpDepthFractions:
      begin
        Observer.OnUpToDateSet := EtsSurfDepth.InvalidateDepthFractions;
      end;
    scpEtFractions:
      begin
        Observer.OnUpToDateSet := EtsSurfDepth.InvalidateEtFractions;
      end;
    else
      Assert(False);
  end;
  LocalScreenObject := EtsSurfDepth.ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.TalksTo(Observer);
  end;
end;

destructor TEtsStringValueItem.Destroy;
var
  LocalScreenObject: TScreenObject;
  SCollection: TEtsStringCollection;
  EtsSurfDepth: TEtsSurfDepthCollection;
begin
  SCollection := StringCollection as TEtsStringCollection;
  EtsSurfDepth := SCollection.ParentCollection as TEtsSurfDepthCollection;
  LocalScreenObject := EtsSurfDepth.ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.StopsTalkingTo(Observer);
  end;
  inherited;
end;

{ TEtsStringCollection }

procedure TEtsStringCollection.Assign(Source: TPersistent);
begin
  if Source is TEtsStringCollection then
  begin
    Purpose := TEtsStringCollection(Source).Purpose;
  end;
  inherited;
end;

constructor TEtsStringCollection.Create(Model: TBaseModel; ScreenObject: TObject;
  EtsSurfDepthCollection: TCustomListArrayBoundColl);
begin
  inherited Create(TEtsStringValueItem, Model, ScreenObject, EtsSurfDepthCollection);
end;

{ TEtsLayerCollection }

function TEtsLayerCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TEtsLayerTimeListLink;
end;

{ TEtsCollection }

procedure TEtsCollection.AssignConcentrationArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel; PestSeries: TStringList;
  PestMethods: TPestMethodList; PestItemNames,
  TimeSeriesNames: TStringListObjectList);
var
  LocalModel: TCustomModel;
  Boundary: TEvtStorage;
  BoundaryIndex: Integer;
  SpeciesIndex: Integer;
  ConcentrationArray: TDataArray;
  LocalConcentrationPestSeries: string;
  LocalConcentrationPestMethod: TPestParamMethod;
  ConcentrationPestItems: TStringList;
  LocalConcentrationPest: string;
  ConcentrationTimeItems: TStringList;
  LocalConcentrationTimeSeries: string;
  LayerMin: Integer;
  RowMin: Integer;
  ColMin: Integer;
  LayerMax: Integer;
  RowMax: Integer;
  ColMax: Integer;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  inherited;
  LocalModel := AModel as TCustomModel;
  Boundary := Boundaries[ItemIndex, AModel] as TEvtStorage;

  if LocalModel.GwtUsed then
  begin
    for SpeciesIndex := 0 to LocalModel.MobileComponents.Count - 1 do
    begin
      ConcentrationArray := DataSets[EvtStartConcentration+SpeciesIndex];
      ConcentrationArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
        LayerMax, RowMax, ColMax);

      LocalConcentrationPestSeries := PestSeries[EvtStartConcentration+SpeciesIndex];
      LocalConcentrationPestMethod := PestMethods[EvtStartConcentration+SpeciesIndex];
      ConcentrationPestItems := PestItemNames[EvtStartConcentration+SpeciesIndex];
      LocalConcentrationPest := ConcentrationPestItems[ItemIndex];
      ConcentrationTimeItems := TimeSeriesNames[EvtStartConcentration+SpeciesIndex];
      LocalConcentrationTimeSeries := ConcentrationTimeItems[ItemIndex];

      BoundaryIndex := 0;
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
                if ConcentrationArray.IsValue[LayerIndex, RowIndex, ColIndex] then
                begin
                  with Boundary.EvtArray[BoundaryIndex] do
                  begin
                    GwtConcentrations.Concentrations[SpeciesIndex] := ConcentrationArray.
                      RealData[LayerIndex, RowIndex, ColIndex];
                    GwtConcentrations.ConcentrationAnnotations[SpeciesIndex] := ConcentrationArray.
                      Annotation[LayerIndex, RowIndex, ColIndex];
                    GwtConcentrations.ConcentrationPestNames[SpeciesIndex] := LocalConcentrationPest;
                    GwtConcentrations.ConcentrationPestSeriesNames[SpeciesIndex] := LocalConcentrationPestSeries;
                    GwtConcentrations.ConcentrationPestSeriesMethods[SpeciesIndex] := LocalConcentrationPestMethod;
                    GwtConcentrations.ConcentrationTimeSeriesNames[SpeciesIndex] := LocalConcentrationTimeSeries;
                  end;
                  Inc(BoundaryIndex);
                end;
              end;
            end;
          end;
        end;
      end;
      ConcentrationArray.CacheData;
    end;
  end;

end;

function TEtsCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TEtsTimeListLink;
end;

procedure TEtsCollection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList; Writer: TObject);
var
  BoundaryValues: TBoundaryValueArray;
  ScreenObject: TScreenObject;
  SpeciesCount: Integer;
  SpeciesIndex: Integer;
  ConcentrationSeriesName: string;
  ConcentrationMethod: TPestParamMethod;
  ConcentrationItems: TStringList;
  ConcentrationTimeSeriesItems: TStringList;
  ConcPestItemList: TList<TStringList>;
  ConcTimeSeriesItemList: TList<TStringList>;
  ConcentrationData: TModflowTimeList;
  LocalModel: TCustomModel;
  Index: Integer;
  Item: TEvtItem;
  ItemFormula: string;
  ALink: TEtsTimeListLink;
begin
  inherited;
  LocalModel := AModel as TCustomModel;
  if LocalModel.GwtUsed then
  begin
    ALink := TimeListLink.GetLink(AModel) as TEtsTimeListLink;

    ConcPestItemList := TList<TStringList>.Create;
    ConcTimeSeriesItemList := TList<TStringList>.Create;
    try
      ScreenObject := BoundaryGroup.ScreenObject as TScreenObject;
      SetLength(BoundaryValues, Count);
      SpeciesCount := LocalModel.MobileComponents.Count;

      for SpeciesIndex := 0 to SpeciesCount - 1 do
      begin
        ConcentrationSeriesName := BoundaryGroup.PestBoundaryFormula[
          EtsStartConcentration + SpeciesIndex];
        PestSeries.Add(ConcentrationSeriesName);
        ConcentrationMethod := BoundaryGroup.PestBoundaryMethod[
          EtsStartConcentration + SpeciesIndex];
        PestMethods.Add(ConcentrationMethod);

        ConcentrationItems := TStringList.Create;
        PestItemNames.Add(ConcentrationItems);
        ConcPestItemList.Add(ConcentrationItems);

        ConcentrationTimeSeriesItems := TStringList.Create;
        TimeSeriesNames.Add(ConcentrationTimeSeriesItems);
        ConcTimeSeriesItemList.Add(ConcentrationTimeSeriesItems);
      end;

      for SpeciesIndex := 0 to SpeciesCount - 1 do
      begin
        for Index := 0 to Count - 1 do
        begin
          Item := Items[Index] as TEvtItem;
          BoundaryValues[Index].Time := Item.StartTime;

          ConcentrationSeriesName := BoundaryGroup.PestBoundaryFormula[
            EtsStartConcentration + SpeciesIndex];
          ConcentrationMethod := BoundaryGroup.PestBoundaryMethod[
            EtsStartConcentration + SpeciesIndex];
          ConcentrationItems := ConcPestItemList[SpeciesIndex];
          ConcentrationTimeSeriesItems := ConcTimeSeriesItemList[SpeciesIndex];
          ItemFormula := Item.GwtConcentrations[SpeciesIndex].Value;
          AssignBoundaryFormula(AModel, ConcentrationSeriesName, ConcentrationMethod,
            ConcentrationItems, ConcentrationTimeSeriesItems, ItemFormula,
            Writer, BoundaryValues[Index]);
        end;

        ConcentrationData := ALink.FConcList[SpeciesIndex];
        ConcentrationData.Initialize(BoundaryValues, ScreenObject, lctUse);
        Assert(ConcentrationData.Count = Count);
      end;

      for SpeciesIndex := 0 to SpeciesCount - 1 do
      begin
        ConcentrationData := ALink.FConcList[SpeciesIndex];
        ListOfTimeLists.Add(ConcentrationData);
      end;

    finally
      ConcTimeSeriesItemList.Free;
      ConcPestItemList.Free;
    end;
  end;
end;

procedure TEtsCollection.InvalidateGwtConcentrations(Sender: TObject);
var
  Index: Integer;
  TimeList: TModflowTimeList;
  PhastModel: TPhastModel;
  Link: TEtsTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TEtsTimeListLink;
    for Index := 0 to Link.FConcList.Count - 1 do
    begin
      TimeList := Link.FConcList[Index];
      TimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TEtsTimeListLink;
      for Index := 0 to Link.FConcList.Count - 1 do
      begin
        TimeList := Link.FConcList[Index];
        TimeList.Invalidate;
      end;
    end;
  end;
end;


function TEtsCollection.SpeciesCount: Integer;
var
  LocalModel: TCustomModel;
begin
  LocalModel := Model as TCustomModel;
  if LocalModel.GwtUsed then
  begin
    result := LocalModel.MobileComponents.Count;
  end
  else
  begin
    result := inherited;
  end;
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
var
  PhastModel: TPhastModel;
  SpeciesIndex: Integer;
  ConcTimeList: TModflowTimeList;
  LocalModel: TCustomModel;
begin
  inherited;
  if Model <> nil then
  begin
    EvapotranspirationRateData.OnInvalidate :=
      (Model as TCustomModel).InvalidateMfEtsEvapRate;
  end;

  FConcList := TObjectList.Create;

  PhastModel := frmGoPhast.PhastModel;
  if PhastModel.GwtUsed then
  begin
    for SpeciesIndex := 0 to PhastModel.MobileComponents.Count - 1 do
    begin
      ConcTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
      ConcTimeList.NonParamDescription := PhastModel.MobileComponents[SpeciesIndex].Name;
      ConcTimeList.ParamDescription :=  ConcTimeList.NonParamDescription;
      if Model <> nil then
      begin
        LocalModel := Model as TCustomModel;
        ConcTimeList.OnInvalidate := LocalModel.InvalidateEtsConc;
      end;
      AddTimeList(ConcTimeList);
      FConcList.Add(ConcTimeList);
    end;
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

destructor TEtsTimeListLink.Destroy;
begin
  FConcList.Free;
  inherited;
end;

{ TRchGwtConcCollection }

constructor TEtsGwtConcCollection.Create(Model: TBaseModel;
  AScreenObject: TObject; ParentCollection: TEtsCollection);
begin
  inherited Create(Model, AScreenObject, ParentCollection);
end;

end.
