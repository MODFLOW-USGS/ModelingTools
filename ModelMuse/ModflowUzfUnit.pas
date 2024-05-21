unit ModflowUzfUnit;

interface

uses Windows, ZLib, SysUtils, Classes, RealListUnit,
  OrderedCollectionUnit, ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit,
  ModflowRchUnit, ModflowEvtUnit,
  FormulaManagerUnit, FormulaManagerInterfaceUnit,
  SubscriptionUnit,
  GoPhastTypes, System.Math;

type

  {
    @name stores the location, time, and ET extinction depth for a cell.
  }
  TUzfExtinctionDepthRecord = record
    Cell: TCellLocation;
    ExtinctionDepth: double;
    StartingTime: double;
    EndingTime: double;
    ExtinctionDepthAnnotation: string;
    ExtinctionDepthPest: string;
    ExtinctionDepthPestSeries: string;
    ExtinctionDepthPestMethod: TPestParamMethod;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  {
    @name stores the location, time, and ET extinction water content for a cell.
  }
  TUzfWaterContentRecord = record
    Cell: TCellLocation;
    MinimumWaterContent: double;
    StartingTime: double;
    EndingTime: double;
    MinimumWaterContentAnnotation: string;
    MinimumWaterContentPest: string;
    MinimumWaterContentPestSeries: string;
    MinimumWaterContentPestMethod: TPestParamMethod;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  // @name is an array of @link(TUzfExtinctionDepthRecord)s.
  TUzfExtinctDepthArray = array of TUzfExtinctionDepthRecord;
  // @name is an array of @link(TUzfWaterContentRecord)s.
  TUzfWaterContentArray = array of TUzfWaterContentRecord;

  // @name extends @link(TCustomBoundaryStorage) by adding
  // a @link(TUzfExtinctDepthArray).
  TUzfExtinctDepthStorage = class(TCustomBoundaryStorage)
  private
    FExtinctDepthArray: TUzfExtinctDepthArray;
    function GetExtinctDepthArray: TUzfExtinctDepthArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property ExtinctDepthArray: TUzfExtinctDepthArray read GetExtinctDepthArray;
  end;

  // @name extends @link(TCustomBoundaryStorage) by adding
  // a @link(TUzfWaterContentArray).
  TUzfWaterContentStorage = class(TCustomBoundaryStorage)
  private
    FWaterContentArray: TUzfWaterContentArray;
    function GetWaterContentArray: TUzfWaterContentArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property WaterContentArray: TUzfWaterContentArray read GetWaterContentArray;
  end;

  // @name represents a MODFLOW UZF Extinction Depth for one time interval.
  // @name is stored by @link(TUzfExtinctionDepthCollection).
  TUzfExtinctDepthItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(UzfExtinctDepth).
    FUzfExtinctDepth: IFormulaObject;
    // See @link(UzfExtinctDepth).
    procedure SetUzfExtinctDepth(const Value: string);
    function GetUzfExtinctDepth: string;
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
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    Destructor Destroy; override;
  published
    // @name is the formula used to set the recharge rate
    // or the recharge rate multiplier of this boundary.
    property UzfExtinctDepth: string read GetUzfExtinctDepth write SetUzfExtinctDepth;
  end;

  // @name represents a MODFLOW UZF Extinction water content for one time interval.
  // @name is stored by @link(TUzfWaterContentCollection).
  TUzfWaterContentItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(UzfWaterContent).
    FUzfWaterContent: IFormulaObject;
    // See @link(UzfWaterContent).
    procedure SetUzfWaterContent(const Value: string);
    function GetUzfWaterContent: string;
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
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    Destructor Destroy; override;
  published
    // @name is the formula used to set the recharge rate
    // or the recharge rate multiplier of this boundary.
    property UzfWaterContent: string read GetUzfWaterContent write SetUzfWaterContent;
  end;

  TUzfExtinctionDepthTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the extinction depths for a series of
    // cells over a series of time intervals.
    FExtinctionDepthData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  // @name represents MODFLOW UZF extinction depth
  // for a series of time intervals.
  TUzfExtinctionDepthCollection = class(TCustomMF_ArrayBoundColl)
  private
    procedure InvalidateUzfExtinctDepthData(Sender: TObject);
  protected
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
    // the @link(TEvtStorage.EvtArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
  end;

  TUzfWaterContentTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the water contents for a series of
    // cells over a series of time intervals.
    FWaterContentData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  // @name represents MODFLOW UZF water content boundaries
  // for a series of time intervals.
  TUzfWaterContentCollection = class(TCustomMF_ArrayBoundColl)
  private
    procedure InvalidateUzfWaterContentData(Sender: TObject);
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    // See @link(TCustomListArrayBoundColl.AssignArrayCellValues
    // TCustomListArrayBoundColl.AssignArrayCellValues)
    procedure AssignArrayCellValues(DataSets: TList;ItemIndex: Integer;
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
    // the @link(TEvtStorage.EvtArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
  end;

  TUzfInfiltrationRateTimeListLink = class(TRchTimeListLink)
  protected
    procedure CreateTimeLists; override;
  end;

  TUzfInfiltrationRateCollection = class(TRchCollection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    function PackageAssignmentMethod(AModel: TBaseModel): TUpdateMethod; override;
  end;

  TUzfEvtTimeListLink = class(TEvtTimeListLink)
  protected
    procedure CreateTimeLists; override;
  end;

  TUzfEvapotranspirationDemandCollection = class(TEvtCollection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TUzfExtinctionDepthCell = class(TValueCell)
  private
    FValues: TUzfExtinctionDepthRecord;
    FStressPeriod: integer;
    function GetExtinctionDepth: double;
    function GetExtinctionDepthAnnotation: string;
    function GetExtinctionDepthPest: string;
    function GetExtinctionDepthPestMethod: TPestParamMethod;
    function GetExtinctionDepthPestSeries: string;
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
  public
    property ExtinctionDepth: double read GetExtinctionDepth;
    property ExtinctionDepthAnnotation: string read GetExtinctionDepthAnnotation;
    property StressPeriod: integer read FStressPeriod write FStressPeriod;
    property Values: TUzfExtinctionDepthRecord read FValues write FValues;
    property ExtinctionDepthPest: string read GetExtinctionDepthPest;
    property ExtinctionDepthPestSeries: string read GetExtinctionDepthPestSeries;
    property ExtinctionDepthPestMethod: TPestParamMethod read GetExtinctionDepthPestMethod;
  end;

  TUzfWaterContentCell = class(TValueCell)
  private
    FValues: TUzfWaterContentRecord;
    FStressPeriod: integer;
    function GetWaterContent: double;
    function GetWaterContentAnnotation: string;
    function GetMinimumWaterContentPest: string;
    function GetMinimumWaterContentPestMethod: TPestParamMethod;
    function GetMinimumWaterContentPestSeries: string;
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
  public
    property WaterContent: double read GetWaterContent;
    property WaterContentAnnotation: string read GetWaterContentAnnotation;
    property StressPeriod: integer read FStressPeriod write FStressPeriod;
    property Values: TUzfWaterContentRecord read FValues write FValues;
    property MinimumWaterContentPest: string read GetMinimumWaterContentPest;
    property MinimumWaterContentPestSeries: string read GetMinimumWaterContentPestSeries;
    property MinimumWaterContentPestMethod: TPestParamMethod read GetMinimumWaterContentPestMethod;
  end;

  TUzfBoundary = class(TModflowBoundary)
  private
    FEvapotranspirationDemand: TUzfEvapotranspirationDemandCollection;
    FExtinctionDepth: TUzfExtinctionDepthCollection;
    FWaterContent: TUzfWaterContentCollection;
    FGageOption2: integer;
    FGageOption1: integer;
    FPestExtinctionDepthMethod: TPestParamMethod;
    FPestWaterContentMethod: TPestParamMethod;
    FPestETDemandMethod: TPestParamMethod;
    FPestInfiltrationRateMethod: TPestParamMethod;
    FPestInfiltrationRateFormula: IFormulaObject;
    FPestETDemandFormula: IFormulaObject;
    FPestExtinctionDepthFormula: IFormulaObject;
    FPestWaterContentFormula: IFormulaObject;
    FPestETDemandObserver: TObserver;
    FPestExtinctionDepthObserver: TObserver;
    FPestInfiltrationRateObserver: TObserver;
    FPestWaterContentObserver: TObserver;
    FUsedObserver: TObserver;
    procedure SetEvapotranspirationDemand(
      const Value: TUzfEvapotranspirationDemandCollection);
    procedure SetExtinctionDepth(const Value: TUzfExtinctionDepthCollection);
    procedure SetWaterContent(const Value: TUzfWaterContentCollection);
    procedure AssignEvapotranspirationDemandCells(BoundaryStorage: TEvtStorage;
      ValueTimeList: TList);
    procedure AssignExtinctionDepthCells(BoundaryStorage: TUzfExtinctDepthStorage;
      ValueTimeList: TList);
    procedure AssignWaterContentCells(BoundaryStorage: TUzfWaterContentStorage;
      ValueTimeList: TList);
    procedure SetGageOption1(const Value: integer);
    procedure SetGageOption2(const Value: integer);
    procedure InvalidateInfiltrationRateData(Sender: TObject);
    procedure InvalidateETDemandData(Sender: TObject);
    procedure InvalidateExtinctionDepthData(Sender: TObject);
    procedure InvalidateWaterContentData(Sender: TObject);
    function GetPestETDemandFormula: string;
    function GetPestETDemandObserver: TObserver;
    function GetPestExtinctionDepthFormula: string;
    function GetPestExtinctionDepthObserver: TObserver;
    function GetPestInfiltrationRateFormula: string;
    function GetPestInfiltrationRateObserver: TObserver;
    function GetPestWaterContentFormula: string;
    function GetPestWaterContentObserver: TObserver;
    procedure SetPestETDemandFormula(const Value: string);
    procedure SetPestETDemandMethod(const Value: TPestParamMethod);
    procedure SetPestExtinctionDepthFormula(const Value: string);
    procedure SetPestExtinctionDepthMethod(const Value: TPestParamMethod);
    procedure SetPestInfiltrationRateFormula(const Value: string);
    procedure SetPestInfiltrationRateMethod(const Value: TPestParamMethod);
    procedure SetPestWaterContentFormula(const Value: string);
    procedure SetPestWaterContentMethod(const Value: TPestParamMethod);
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TRch_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;

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
    property PestInfiltrationRateObserver: TObserver read GetPestInfiltrationRateObserver;
    property PestETDemandObserver: TObserver read GetPestETDemandObserver;
    property PestExtinctionDepthObserver: TObserver read GetPestExtinctionDepthObserver;
    property PestWaterContentObserver: TObserver read GetPestWaterContentObserver;
  public
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent);override;
    procedure Clear; override;
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TRchStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW UZF parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TRchStorage) in @link(TCustomMF_BoundColl.Boundaries
    // Param.Param.Boundaries)
    // Those represent parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    function Used: boolean; override;
    procedure EvaluateArrayBoundaries(AModel: TBaseModel; Writer: TObject); override;
    procedure InvalidateDisplay; override;
    procedure GetEvapotranspirationDemandCells(LayerTimeList: TList; AModel: TBaseModel);
    procedure GetExtinctionDepthCells(LayerTimeList: TList; AModel: TBaseModel);
    procedure GetWaterContentCells(LayerTimeList: TList; AModel: TBaseModel);
    procedure UpdateTimes(Times: TRealList; StartTestTime, EndTestTime: double;
      var StartRangeExtended, EndRangeExtended: boolean; AModel: TBaseModel); override;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
  published
    property GageOption1: integer read FGageOption1 write SetGageOption1;
    property GageOption2: integer read FGageOption2 write SetGageOption2;
    property EvapotranspirationDemand: TUzfEvapotranspirationDemandCollection
      read FEvapotranspirationDemand write SetEvapotranspirationDemand;
    property ExtinctionDepth: TUzfExtinctionDepthCollection
      read FExtinctionDepth write SetExtinctionDepth;
    property WaterContent: TUzfWaterContentCollection
      read FWaterContent write SetWaterContent;
    property PestInfiltrationRateFormula: string read GetPestInfiltrationRateFormula
      write SetPestInfiltrationRateFormula;
    property PestInfiltrationRateMethod: TPestParamMethod read FPestInfiltrationRateMethod
      write SetPestInfiltrationRateMethod;
    property PestETDemandFormula: string read GetPestETDemandFormula
      write SetPestETDemandFormula ;
    property PestETDemandMethod: TPestParamMethod read FPestETDemandMethod
      write SetPestETDemandMethod;
    property PestExtinctionDepthFormula: string read GetPestExtinctionDepthFormula
      write SetPestExtinctionDepthFormula;
    property PestExtinctionDepthMethod: TPestParamMethod read FPestExtinctionDepthMethod
      write SetPestExtinctionDepthMethod;
    property PestWaterContentFormula: string read GetPestWaterContentFormula
      write SetPestWaterContentFormula;
    property PestWaterContentMethod: TPestParamMethod read FPestWaterContentMethod
      write SetPestWaterContentMethod;
  end;

const
  UzfInfiltrationBoundaryPosition = 0;
  UzfETDemandBoundaryPosition = 1;
  UzfExtinctionDepthBoundaryPosition = 2;
  UzfWaterContentBoundaryPosition = 3;


implementation

uses ScreenObjectUnit, PhastModelUnit, ModflowTimeUnit,
  frmGoPhastUnit;

resourcestring
  StrEvapoTranspiration = 'Evapo- transpiration demand';
  StrETExtinctionDepth = 'ET extinction depth';
  StrETExtinctionWater = 'ET extinction water content';
  StrInfiltrationRate = 'Infiltration rate';

{ TUzfRateCollection }

const
  UzfExtinctDepthPosition = 0;
  UzfWaterContentPosition = 0;

{ TUzfBoundary }

procedure TUzfBoundary.Assign(Source: TPersistent);
var
  SourceBoundary: TUzfBoundary;
begin
  if Source is TUzfBoundary then
  begin
    SourceBoundary := TUzfBoundary(Source);
    EvapotranspirationDemand := SourceBoundary.EvapotranspirationDemand;
    ExtinctionDepth := SourceBoundary.ExtinctionDepth;
    WaterContent := SourceBoundary.WaterContent;
    GageOption1 := SourceBoundary.GageOption1;
    GageOption2 := SourceBoundary.GageOption2;

    PestInfiltrationRateFormula := SourceBoundary.PestInfiltrationRateFormula;
    PestInfiltrationRateMethod := SourceBoundary.PestInfiltrationRateMethod;
    PestETDemandFormula := SourceBoundary.PestETDemandFormula;
    PestETDemandMethod := SourceBoundary.PestETDemandMethod;
    PestExtinctionDepthFormula := SourceBoundary.PestExtinctionDepthFormula;
    PestExtinctionDepthMethod := SourceBoundary.PestExtinctionDepthMethod;
    PestWaterContentFormula := SourceBoundary.PestWaterContentFormula;
    PestWaterContentMethod := SourceBoundary.PestWaterContentMethod;

  end;
  inherited;
end;

procedure TUzfBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TRch_Cell;
  BoundaryValues: TRchRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TRchStorage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TRchStorage;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TRch_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.RchArray) then
      begin
        Cells.Capacity := Cells.Count + Max(Length(LocalBoundaryStorage.RchArray), Cells.Count div 4);
      end;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.RchArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.RchArray[BoundaryIndex];
        Cell := TRch_Cell.Create;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
        Cell.ScreenObject := ScreenObjectI;
        LocalModel.AdjustCellPosition(Cell);
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

procedure TUzfBoundary.AssignEvapotranspirationDemandCells(
  BoundaryStorage: TEvtStorage; ValueTimeList: TList);
var
  Cell: TEvt_Cell;
  BoundaryValues: TEvtRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TEvtStorage;
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
      Cells := TValueCellList.Create(TEvt_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := (ParentModel as TPhastModel).ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime <= LocalBoundaryStorage.EndingTime) then
    begin
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.EvtArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.EvtArray[BoundaryIndex];
        Cell := TEvt_Cell.Create;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

procedure TUzfBoundary.AssignExtinctionDepthCells(
  BoundaryStorage: TUzfExtinctDepthStorage; ValueTimeList: TList);
var
  Cell: TUzfExtinctionDepthCell;
  BoundaryValues: TUzfExtinctionDepthRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TUzfExtinctDepthStorage;
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
      Cells := TValueCellList.Create(TUzfExtinctionDepthCell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := (ParentModel as TPhastModel).ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime <= LocalBoundaryStorage.EndingTime) then
    begin
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.ExtinctDepthArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.ExtinctDepthArray[BoundaryIndex];
        Cell := TUzfExtinctionDepthCell.Create;
        Assert(ScreenObject <> nil);
        Cell.IFace := (ScreenObject as TScreenObject).IFace;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

procedure TUzfBoundary.AssignWaterContentCells(
  BoundaryStorage: TUzfWaterContentStorage; ValueTimeList: TList);
var
  Cell: TUzfWaterContentCell;
  BoundaryValues: TUzfWaterContentRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TUzfWaterContentStorage;
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
      Cells := TValueCellList.Create(TUzfWaterContentCell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := (ParentModel as TPhastModel).ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime <= LocalBoundaryStorage.EndingTime) then
    begin
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.WaterContentArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.WaterContentArray[BoundaryIndex];
        Cell := TUzfWaterContentCell.Create;
        Assert(ScreenObject <> nil);
        Cell.IFace := (ScreenObject as TScreenObject).IFace;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TUzfBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TUzfInfiltrationRateCollection;
end;

function TUzfBoundary.BoundaryObserverPrefix: string;
begin
  result := 'PestUzf_';
end;

procedure TUzfBoundary.Clear;
begin
  inherited;
  EvapotranspirationDemand.Clear;
  ExtinctionDepth.Clear;
  WaterContent.Clear;
  GageOption1 := 0;
  GageOption2 := 0;
end;

constructor TUzfBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited Create(Model, ScreenObject);
  FEvapotranspirationDemand := TUzfEvapotranspirationDemandCollection.
    Create(self, Model as TCustomModel, ScreenObject);
  FExtinctionDepth := TUzfExtinctionDepthCollection.
    Create(self, Model as TCustomModel, ScreenObject);
  FWaterContent := TUzfWaterContentCollection.
    Create(self, Model as TCustomModel, ScreenObject);

  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

  PestInfiltrationRateFormula := '';
  PestETDemandFormula := '';
  PestExtinctionDepthFormula := '';
  PestWaterContentFormula := '';
  FPestInfiltrationRateMethod := DefaultBoundaryMethod(UzfInfiltrationBoundaryPosition);
  FPestETDemandMethod := DefaultBoundaryMethod(UzfETDemandBoundaryPosition);
  FPestExtinctionDepthMethod := DefaultBoundaryMethod(UzfExtinctionDepthBoundaryPosition);
  FPestWaterContentMethod := DefaultBoundaryMethod(UzfWaterContentBoundaryPosition);

end;

procedure TUzfBoundary.CreateFormulaObjects;
begin
  FPestInfiltrationRateFormula := CreateFormulaObjectBlocks(dsoTop);
  FPestETDemandFormula := CreateFormulaObjectBlocks(dsoTop);
  FPestExtinctionDepthFormula := CreateFormulaObjectBlocks(dsoTop);
  FPestWaterContentFormula := CreateFormulaObjectBlocks(dsoTop);
end;

procedure TUzfBoundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(PestInfiltrationRateObserver);
    FObserverList.Add(PestETDemandObserver);
    FObserverList.Add(PestExtinctionDepthObserver);
    FObserverList.Add(PestWaterContentObserver);
  end;
end;

class function TUzfBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    UzfInfiltrationBoundaryPosition:
      begin
        result := ppmMultiply
      end;
    UzfETDemandBoundaryPosition:
      begin
        result := ppmMultiply
      end;
    UzfExtinctionDepthBoundaryPosition:
      begin
        result := ppmMultiply
      end;
    UzfWaterContentBoundaryPosition:
      begin
        result := ppmMultiply
      end;
    else
      begin
        result := inherited;
        Assert(False);
      end;
  end;
end;

destructor TUzfBoundary.Destroy;
begin
  PestInfiltrationRateFormula := '';
  PestETDemandFormula := '';
  PestExtinctionDepthFormula := '';
  PestWaterContentFormula := '';

  FWaterContent.Free;
  FExtinctionDepth.Free;
  FEvapotranspirationDemand.Free;
  inherited;
end;

procedure TUzfBoundary.EvaluateArrayBoundaries(AModel: TBaseModel; Writer: TObject);
begin
  inherited;
  if (ParentModel as TPhastModel).ModflowPackages.UzfPackage.SimulateET then
  begin
    EvapotranspirationDemand.EvaluateArrayBoundaries(AModel, Writer);
    ExtinctionDepth.EvaluateArrayBoundaries(AModel, Writer);
    FWaterContent.EvaluateArrayBoundaries(AModel, Writer);
  end;
end;

procedure TUzfBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
var
  ValueIndex: Integer;
  BoundaryStorage: TRchStorage;
begin
  EvaluateArrayBoundaries(AModel, Writer);
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TRchStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
end;

procedure TUzfBoundary.GetEvapotranspirationDemandCells(LayerTimeList: TList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TEvtStorage;
begin
  for ValueIndex := 0 to EvapotranspirationDemand.Count - 1 do
  begin
    if ValueIndex < EvapotranspirationDemand.BoundaryCount[AModel] then
    begin
      BoundaryStorage := EvapotranspirationDemand.Boundaries[
        ValueIndex, AModel] as TEvtStorage;
      AssignEvapotranspirationDemandCells(BoundaryStorage, LayerTimeList);
    end;
  end;
end;

procedure TUzfBoundary.GetExtinctionDepthCells(LayerTimeList: TList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TUzfExtinctDepthStorage;
begin
  for ValueIndex := 0 to ExtinctionDepth.Count - 1 do
  begin
    if ValueIndex < ExtinctionDepth.BoundaryCount[AModel] then
    begin
      BoundaryStorage := ExtinctionDepth.Boundaries[ValueIndex, AModel] as TUzfExtinctDepthStorage;
      AssignExtinctionDepthCells(BoundaryStorage, LayerTimeList);
    end;
  end;
end;

function TUzfBoundary.GetPestBoundaryFormula(FormulaIndex: integer): string;
begin
  case FormulaIndex of
    UzfInfiltrationBoundaryPosition:
      begin
        result := PestInfiltrationRateFormula;
      end;
    UzfETDemandBoundaryPosition:
      begin
        result := PestETDemandFormula;
      end;
    UzfExtinctionDepthBoundaryPosition:
      begin
        result := PestExtinctionDepthFormula;
      end;
    UzfWaterContentBoundaryPosition:
      begin
        result := PestWaterContentFormula;
      end;
    else
      begin
        result := inherited;
//        Assert(False);
      end;
  end;
//  UzfInfiltrationBoundaryPosition = 0;
//  UzfEtDemandBoundaryPosition = 1;
//  UzfEtExtinctionDepthBoundaryPosition = 2;
//  UzfWaterContentBoundaryPosition = 3;

end;

function TUzfBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    UzfInfiltrationBoundaryPosition:
      begin
        result := PestInfiltrationRateMethod;
      end;
    UzfETDemandBoundaryPosition:
      begin
        result := PestETDemandMethod;
      end;
    UzfExtinctionDepthBoundaryPosition:
      begin
        result := PestExtinctionDepthMethod;
      end;
    UzfWaterContentBoundaryPosition:
      begin
        result := PestWaterContentMethod;
      end;
    else
      begin
        result := inherited;
//        Assert(False);
      end;
  end;
end;

function TUzfBoundary.GetPestETDemandFormula: string;
begin
  Result := FPestETDemandFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(UzfETDemandBoundaryPosition);
  end;
end;

function TUzfBoundary.GetPestETDemandObserver: TObserver;
begin
  if FPestETDemandObserver = nil then
  begin
    CreateObserver('PestETDemand_', FPestETDemandObserver, nil);
    FPestETDemandObserver.OnUpToDateSet := InvalidateETDemandData;
  end;
  result := FPestETDemandObserver;
end;

function TUzfBoundary.GetPestExtinctionDepthFormula: string;
begin
  Result := FPestExtinctionDepthFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(UzfExtinctionDepthBoundaryPosition);
  end;
end;

function TUzfBoundary.GetPestExtinctionDepthObserver: TObserver;
begin
  if FPestExtinctionDepthObserver = nil then
  begin
    CreateObserver('PestExtinctionDepth_', FPestExtinctionDepthObserver, nil);
    FPestExtinctionDepthObserver.OnUpToDateSet := InvalidateExtinctionDepthData;
  end;
  result := FPestExtinctionDepthObserver;
end;

function TUzfBoundary.GetPestInfiltrationRateFormula: string;
begin
  Result := FPestInfiltrationRateFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(UzfInfiltrationBoundaryPosition);
  end;
end;

function TUzfBoundary.GetPestInfiltrationRateObserver: TObserver;
begin
  if FPestInfiltrationRateObserver = nil then
  begin
    CreateObserver('PestInfiltrationRate_', FPestInfiltrationRateObserver, nil);
    FPestInfiltrationRateObserver.OnUpToDateSet := InvalidateInfiltrationRateData;
  end;
  result := FPestInfiltrationRateObserver;
end;

function TUzfBoundary.GetPestWaterContentFormula: string;
begin
  Result := FPestWaterContentFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(UzfWaterContentBoundaryPosition);
  end;
end;

function TUzfBoundary.GetPestWaterContentObserver: TObserver;
begin
  if FPestWaterContentObserver = nil then
  begin
    CreateObserver('PestWaterContent_', FPestWaterContentObserver, nil);
    FPestWaterContentObserver.OnUpToDateSet := InvalidateWaterContentData;
  end;
  result := FPestWaterContentObserver;
end;

procedure TUzfBoundary.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FPestInfiltrationRateFormula as TObject then
  begin
    if UzfInfiltrationBoundaryPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[UzfInfiltrationBoundaryPosition]);
    end;
  end;
  if Sender = FPestEtDemandFormula as TObject then
  begin
    if UzfEtDemandBoundaryPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[UzfEtDemandBoundaryPosition]);
    end;
  end;
  if Sender = FPestExtinctionDepthFormula as TObject then
  begin
    if UzfExtinctionDepthBoundaryPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[UzfExtinctionDepthBoundaryPosition]);
    end;
  end;
  if Sender = FPestWaterContentFormula as TObject then
  begin
    if UzfWaterContentBoundaryPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[UzfWaterContentBoundaryPosition]);
    end;
  end;
end;

function TUzfBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('PestUzf_Used_', FUsedObserver, nil);
  end;
  result := FUsedObserver;
end;

procedure TUzfBoundary.GetWaterContentCells(LayerTimeList: TList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TUzfWaterContentStorage;
begin
  for ValueIndex := 0 to WaterContent.Count - 1 do
  begin
    if ValueIndex < WaterContent.BoundaryCount[AModel] then
    begin
      BoundaryStorage := WaterContent.Boundaries[ValueIndex, AModel] as TUzfWaterContentStorage;
      AssignWaterContentCells(BoundaryStorage, LayerTimeList);
    end;
  end;
end;

procedure TUzfBoundary.HandleChangedValue(Observer: TObserver);
begin
//  inherited;
  InvalidateDisplay;
end;

procedure TUzfBoundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TPhastModel;
    Model.InvalidateMfUzfInfiltration(self);
    Model.InvalidateMfUzfEtDemand(self);
    Model.InvalidateMfUzfExtinctionDepth(self);
    Model.InvalidateMfUzfWaterContent(self);
  end;
end;

procedure TUzfBoundary.InvalidateETDemandData(Sender: TObject);
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
    PhastModel.InvalidateMfUzfEtDemand(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfUzfEtDemand(self);
      end;
    end;
  end;
end;

procedure TUzfBoundary.InvalidateExtinctionDepthData(Sender: TObject);
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
    PhastModel.InvalidateMfUzfExtinctionDepth(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfUzfExtinctionDepth(self);
      end;
    end;
  end;
end;

procedure TUzfBoundary.InvalidateInfiltrationRateData(Sender: TObject);
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
    PhastModel.InvalidateMfUzfInfiltration(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfUzfInfiltration(self);
      end;
    end;
  end;
end;

procedure TUzfBoundary.InvalidateWaterContentData(Sender: TObject);
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
    PhastModel.InvalidateMfUzfWaterContent(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfUzfWaterContent(self);
      end;
    end;
  end;
end;

procedure TUzfBoundary.SetEvapotranspirationDemand(
  const Value: TUzfEvapotranspirationDemandCollection);
begin
  FEvapotranspirationDemand.Assign(Value);
end;

procedure TUzfBoundary.SetExtinctionDepth(
  const Value: TUzfExtinctionDepthCollection);
begin
  FExtinctionDepth.Assign(Value);
end;

procedure TUzfBoundary.SetGageOption1(const Value: integer);
begin
  if FGageOption1 <> Value then
  begin
    Assert(Value in [0, 1, 2]);
    FGageOption1 := Value;
    if ScreenObject <> nil then
    begin
      (ScreenObject as TScreenObject).UpdateUzfGage1and2;
    end;
    InvalidateModel;
  end;
end;

procedure TUzfBoundary.SetGageOption2(const Value: integer);
begin
  if FGageOption2 <> Value then
  begin
    Assert(Value in [0, 3]);
    FGageOption2 := Value;
    if ScreenObject <> nil then
    begin
      (ScreenObject as TScreenObject).UpdateUzfGage3;
    end;
    InvalidateModel;
  end;
end;

procedure TUzfBoundary.SetPestBoundaryFormula(FormulaIndex: integer;
  const Value: string);
begin
  case FormulaIndex of
    UzfInfiltrationBoundaryPosition:
      begin
        PestInfiltrationRateFormula := Value;
      end;
    UzfETDemandBoundaryPosition:
      begin
        PestETDemandFormula := Value;
      end;
    UzfExtinctionDepthBoundaryPosition:
      begin
        PestExtinctionDepthFormula := Value;
      end;
    UzfWaterContentBoundaryPosition:
      begin
        PestWaterContentFormula := Value;
      end;
    else
      begin
        inherited;
//        Assert(False);
      end;
  end;
end;

procedure TUzfBoundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
begin
  case FormulaIndex of
    UzfInfiltrationBoundaryPosition:
      begin
        PestInfiltrationRateMethod := Value;
      end;
    UzfETDemandBoundaryPosition:
      begin
        PestETDemandMethod := Value;
      end;
    UzfExtinctionDepthBoundaryPosition:
      begin
        PestExtinctionDepthMethod := Value;
      end;
    UzfWaterContentBoundaryPosition:
      begin
        PestWaterContentMethod := Value;
      end;
    else
      begin
        inherited;
//        Assert(False);
      end;
  end;
end;

procedure TUzfBoundary.SetPestETDemandFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, UzfETDemandBoundaryPosition, FPestETDemandFormula);
end;

procedure TUzfBoundary.SetPestETDemandMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestETDemandMethod, Value);
end;

procedure TUzfBoundary.SetPestExtinctionDepthFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, UzfExtinctionDepthBoundaryPosition, FPestExtinctionDepthFormula);
end;

procedure TUzfBoundary.SetPestExtinctionDepthMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestExtinctionDepthMethod, Value);
end;

procedure TUzfBoundary.SetPestInfiltrationRateFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, UzfInfiltrationBoundaryPosition, FPestInfiltrationRateFormula);
end;

procedure TUzfBoundary.SetPestInfiltrationRateMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestInfiltrationRateMethod, Value);
end;

procedure TUzfBoundary.SetPestWaterContentFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, UzfWaterContentBoundaryPosition, FPestWaterContentFormula);
end;

procedure TUzfBoundary.SetPestWaterContentMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestWaterContentMethod, Value);
end;

procedure TUzfBoundary.SetWaterContent(const Value: TUzfWaterContentCollection);
begin
  FWaterContent.Assign(Value);
end;

procedure TUzfBoundary.UpdateTimes(Times: TRealList;
  StartTestTime, EndTestTime: double; var StartRangeExtended,
  EndRangeExtended: boolean; AModel: TBaseModel);
begin
  inherited;
  AddBoundaryTimes(EvapotranspirationDemand, Times, StartTestTime, EndTestTime, StartRangeExtended, EndRangeExtended);
  AddBoundaryTimes(ExtinctionDepth, Times, StartTestTime, EndTestTime, StartRangeExtended, EndRangeExtended);
  AddBoundaryTimes(WaterContent, Times, StartTestTime, EndTestTime, StartRangeExtended, EndRangeExtended);
end;

function TUzfBoundary.Used: boolean;
begin
  result := inherited Used
    or EvapotranspirationDemand.Used
    or ExtinctionDepth.Used
    or WaterContent.Used
    or (GageOption1 <> 0)
    or (GageOption2 <> 0)
end;

{ TUzfEvapotranspirationDemandCollection }

class function TUzfEvapotranspirationDemandCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TUzfEvtTimeListLink;
end;

{ TUzfExtinctDepthItem }

procedure TUzfExtinctDepthItem.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if Source is TUzfExtinctDepthItem then
  begin
    UzfExtinctDepth := TUzfExtinctDepthItem(Source).UzfExtinctDepth;
  end;
  inherited;
end;

procedure TUzfExtinctDepthItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TUzfExtinctionDepthCollection;
  UzfExtinctDepthObserver: TObserver;
begin
  ParentCollection := Collection as TUzfExtinctionDepthCollection;
  UzfExtinctDepthObserver := FObserverList[UzfExtinctDepthPosition];
  UzfExtinctDepthObserver.OnUpToDateSet := ParentCollection.InvalidateUzfExtinctDepthData;
end;

function TUzfExtinctDepthItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

procedure TUzfExtinctDepthItem.CreateFormulaObjects;
begin
  FUzfExtinctDepth := CreateFormulaObject(dso3D);
end;

destructor TUzfExtinctDepthItem.Destroy;
begin
  UzfExtinctDepth := '0';
  inherited;
end;

function TUzfExtinctDepthItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    UzfExtinctDepthPosition: result := UzfExtinctDepth;
    else Assert(False);
  end;
end;

procedure TUzfExtinctDepthItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  Assert(Sender = FUzfExtinctDepth as TObject);
  List.Add( FObserverList[UzfExtinctDepthPosition]);
end;

function TUzfExtinctDepthItem.GetUzfExtinctDepth: string;
begin
  Result := FUzfExtinctDepth.Formula;
  ResetItemObserver(UzfExtinctDepthPosition);
end;

function TUzfExtinctDepthItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TUzfExtinctDepthItem;
begin
  result := (AnotherItem is TUzfExtinctDepthItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TUzfExtinctDepthItem(AnotherItem);
    result := (Item.UzfExtinctDepth = UzfExtinctDepth)
  end;
end;

procedure TUzfExtinctDepthItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FUzfExtinctDepth,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TUzfExtinctDepthItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    UzfExtinctDepthPosition: UzfExtinctDepth := Value;
    else Assert(False);
  end;
end;

procedure TUzfExtinctDepthItem.SetUzfExtinctDepth(const Value: string);
begin
  UpdateFormulaBlocks(Value, UzfExtinctDepthPosition, FUzfExtinctDepth);
end;

{ TUzfWaterContentItem }

procedure TUzfWaterContentItem.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if Source is TUzfWaterContentItem then
  begin
    UzfWaterContent := TUzfWaterContentItem(Source).UzfWaterContent;
  end;
  inherited;
end;

procedure TUzfWaterContentItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TUzfWaterContentCollection;
  UzfWaterContentObserver: TObserver;
begin
  ParentCollection := Collection as TUzfWaterContentCollection;
  UzfWaterContentObserver := FObserverList[UzfWaterContentPosition];
  UzfWaterContentObserver.OnUpToDateSet := ParentCollection.InvalidateUzfWaterContentData;
end;

function TUzfWaterContentItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

procedure TUzfWaterContentItem.CreateFormulaObjects;
begin
  FUzfWaterContent := CreateFormulaObject(dso3D);
end;

destructor TUzfWaterContentItem.Destroy;
begin
  UzfWaterContent := '0';
  inherited;
end;

function TUzfWaterContentItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    UzfWaterContentPosition: result := UzfWaterContent;
    else Assert(False);
  end;
end;

procedure TUzfWaterContentItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  Assert(Sender = FUzfWaterContent as TObject);
  List.Add( FObserverList[UzfWaterContentPosition]);
end;

function TUzfWaterContentItem.GetUzfWaterContent: string;
begin
  Result := FUzfWaterContent.Formula;
  ResetItemObserver(UzfWaterContentPosition);
end;

function TUzfWaterContentItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TUzfWaterContentItem;
begin
  result := (AnotherItem is TUzfWaterContentItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TUzfWaterContentItem(AnotherItem);
    result := (Item.UzfWaterContent = UzfWaterContent)
  end;
end;

procedure TUzfWaterContentItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FUzfWaterContent,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TUzfWaterContentItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    UzfWaterContentPosition: UzfWaterContent := Value;
    else Assert(False);
  end;
end;

procedure TUzfWaterContentItem.SetUzfWaterContent(const Value: string);
begin
  UpdateFormulaBlocks(Value, UzfWaterContentPosition, FUzfWaterContent);
end;

{ TUzfExtinctionDepthCollection }

procedure TUzfExtinctionDepthCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TUzfExtinctDepthStorage.Create(AModel));
end;

procedure TUzfExtinctionDepthCollection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel; PestSeries: TStringList;
  PestMethods: TPestMethodList; PestItemNames, TimeSeriesNames: TStringListObjectList);
var
  ExtinctionDepthRateArray: TDataArray;
  Boundary: TUzfExtinctDepthStorage;
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
  LocalExtinctDepthPestSeries: string;
  LocalExtinctDepthPestMethod: TPestParamMethod;
  ExtinctDepthItems: TStringList;
  LocalExtinctDepthPest: string;
begin
  LocalModel := AModel as TCustomModel;
  BoundaryIndex := 0;
  ExtinctionDepthRateArray := DataSets[UzfExtinctDepthPosition];
  Boundary := Boundaries[ItemIndex, AModel] as TUzfExtinctDepthStorage;
  ExtinctionDepthRateArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
    LayerMax, RowMax, ColMax);

  LocalExtinctDepthPestSeries := PestSeries[UzfExtinctDepthPosition];
  LocalExtinctDepthPestMethod := PestMethods[UzfExtinctDepthPosition];
  ExtinctDepthItems := PestItemNames[UzfExtinctDepthPosition];
  LocalExtinctDepthPest := ExtinctDepthItems[ItemIndex];

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
            if ExtinctionDepthRateArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              with Boundary.ExtinctDepthArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                ExtinctionDepth := ExtinctionDepthRateArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                ExtinctionDepthAnnotation := ExtinctionDepthRateArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
                ExtinctionDepthPest := LocalExtinctDepthPest;
                ExtinctionDepthPestMethod := LocalExtinctDepthPestMethod;
                ExtinctionDepthPestSeries := LocalExtinctDepthPestSeries;
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  ExtinctionDepthRateArray.CacheData;
  Boundary.CacheData;
end;

class function TUzfExtinctionDepthCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TUzfExtinctionDepthTimeListLink;
end;

procedure TUzfExtinctionDepthCollection.InitializeTimeLists(
  ListOfTimeLists: TList; AModel: TBaseModel; PestSeries: TStringList;
  PestMethods: TPestMethodList; PestItemNames, TimeSeriesNames: TStringListObjectList;
  Writer: TObject);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TUzfExtinctDepthItem;
  ScreenObject: TScreenObject;
  ALink: TUzfExtinctionDepthTimeListLink;
  FExtinctionDepthData: TModflowTimeList;
  PestExtinctionDepthSeriesName: string;
  ExtinctionDepthMethod: TPestParamMethod;
  ExtinctionDepthItems: TStringList;
  ItemFormula: string;
  TimeSeriesItems: TStringList;
begin
  ScreenObject := BoundaryGroup.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);

  PestExtinctionDepthSeriesName := BoundaryGroup.PestBoundaryFormula[
    UzfExtinctionDepthBoundaryPosition];
  PestSeries.Add(PestExtinctionDepthSeriesName);
  ExtinctionDepthMethod := BoundaryGroup.PestBoundaryMethod[
    UzfExtinctionDepthBoundaryPosition];
  PestMethods.Add(ExtinctionDepthMethod);

  ExtinctionDepthItems := TStringList.Create;
  PestItemNames.Add(ExtinctionDepthItems);
  TimeSeriesItems := TStringList.Create;
  TimeSeriesNames.Add(TimeSeriesItems);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TUzfExtinctDepthItem;
    BoundaryValues[Index].Time := Item.StartTime;

    ItemFormula := Item.UzfExtinctDepth;
    AssignBoundaryFormula(AModel, PestExtinctionDepthSeriesName, ExtinctionDepthMethod,
      ExtinctionDepthItems, TimeSeriesItems, ItemFormula, Writer, BoundaryValues[Index]);

//    BoundaryValues[Index].Formula := Item.UzfExtinctDepth;
  end;
  ALink := TimeListLink.GetLink(AModel) as TUzfExtinctionDepthTimeListLink;
  FExtinctionDepthData := ALink.FExtinctionDepthData;
  FExtinctionDepthData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(FExtinctionDepthData.Count = Count);
  ClearBoundaries(AModel);
  SetBoundaryCapacity(FExtinctionDepthData.Count, AModel);
  for TimeIndex := 0 to FExtinctionDepthData.Count - 1 do
  begin
    AddBoundary(TUzfExtinctDepthStorage.Create(AModel));
  end;
  ListOfTimeLists.Add(FExtinctionDepthData);
end;

procedure TUzfExtinctionDepthCollection.InvalidateUzfExtinctDepthData(
  Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TUzfExtinctionDepthTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TUzfExtinctionDepthTimeListLink;
    Link.FExtinctionDepthData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TUzfExtinctionDepthTimeListLink;
        Link.FExtinctionDepthData.Invalidate;
      end;
    end;
  end;
end;

class function TUzfExtinctionDepthCollection.ItemClass: TBoundaryItemClass;
begin
  result := TUzfExtinctDepthItem
end;

procedure TUzfExtinctionDepthCollection.SetBoundaryStartAndEndTime(
  BoundaryCount: Integer; Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TUzfExtinctDepthStorage).
    FExtinctDepthArray, BoundaryCount);
  inherited;
end;

{ TUzfWaterContentCollection }

procedure TUzfWaterContentCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TUzfWaterContentStorage.Create(AModel));
end;

procedure TUzfWaterContentCollection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel; PestSeries: TStringList;
  PestMethods: TPestMethodList; PestItemNames, TimeSeriesNames: TStringListObjectList);
var
  WaterContentArray: TDataArray;
  Boundary: TUzfWaterContentStorage;
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
  LocalMinimumWaterContentPestSeries: string;
  LocalMinimumWaterContentPestMethod: TPestParamMethod;
  MinimumWaterContentItems: TStringList;
  LocalMinimumWaterContentPest: string;
begin
  LocalModel := AModel as TCustomModel;
  BoundaryIndex := 0;
  WaterContentArray := DataSets[UzfWaterContentPosition];
  Boundary := Boundaries[ItemIndex, AModel] as TUzfWaterContentStorage;
  WaterContentArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
    LayerMax, RowMax, ColMax);

  LocalMinimumWaterContentPestSeries := PestSeries[UzfWaterContentPosition];
  LocalMinimumWaterContentPestMethod := PestMethods[UzfWaterContentPosition];
  MinimumWaterContentItems := PestItemNames[UzfWaterContentPosition];
  LocalMinimumWaterContentPest := MinimumWaterContentItems[ItemIndex];


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
            if WaterContentArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              with Boundary.WaterContentArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                MinimumWaterContent := WaterContentArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                MinimumWaterContentAnnotation := WaterContentArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
                MinimumWaterContentPest := LocalMinimumWaterContentPest;
                MinimumWaterContentPestMethod := LocalMinimumWaterContentPestMethod;
                MinimumWaterContentPestSeries := LocalMinimumWaterContentPestSeries;
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  WaterContentArray.CacheData;
  Boundary.CacheData;
end;

class function TUzfWaterContentCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TUzfWaterContentTimeListLink;
end;

procedure TUzfWaterContentCollection.InitializeTimeLists(
  ListOfTimeLists: TList; AModel: TBaseModel; PestSeries: TStringList;
  PestMethods: TPestMethodList; PestItemNames, TimeSeriesNames: TStringListObjectList;
  Writer: TObject);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TUzfWaterContentItem;
  ScreenObject: TScreenObject;
  ALink: TUzfWaterContentTimeListLink;
  WaterContentData: TModflowTimeList;
  PestWaterContentSeriesName: string;
  WaterContentMethod: TPestParamMethod;
  WaterContentItems: TStringList;
  ItemFormula: string;
  TimeSeriesItems: TStringList;
begin
  ScreenObject := BoundaryGroup.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);

  PestWaterContentSeriesName := BoundaryGroup.PestBoundaryFormula[UzfWaterContentBoundaryPosition];
  PestSeries.Add(PestWaterContentSeriesName);
  WaterContentMethod := BoundaryGroup.PestBoundaryMethod[UzfWaterContentBoundaryPosition];
  PestMethods.Add(WaterContentMethod);

  WaterContentItems := TStringList.Create;
  PestItemNames.Add(WaterContentItems);
  TimeSeriesItems := TStringList.Create;
  TimeSeriesNames.Add(TimeSeriesItems);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TUzfWaterContentItem;
    BoundaryValues[Index].Time := Item.StartTime;

    ItemFormula := Item.UzfWaterContent;
    AssignBoundaryFormula(AModel, PestWaterContentSeriesName, WaterContentMethod,
      WaterContentItems, TimeSeriesItems, ItemFormula, Writer, BoundaryValues[Index]);

//    BoundaryValues[Index].Formula := Item.UzfWaterContent;
  end;
  ALink := TimeListLink.GetLink(AModel) as TUzfWaterContentTimeListLink;
  WaterContentData := ALink.FWaterContentData;
  WaterContentData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(WaterContentData.Count = Count);
  ClearBoundaries(AModel);
  SetBoundaryCapacity(WaterContentData.Count, AModel);
  for TimeIndex := 0 to WaterContentData.Count - 1 do
  begin
    AddBoundary(TUzfWaterContentStorage.Create(AModel));
  end;
  ListOfTimeLists.Add(WaterContentData);
end;

procedure TUzfWaterContentCollection.InvalidateUzfWaterContentData(
  Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TUzfWaterContentTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TUzfWaterContentTimeListLink;
    Link.FWaterContentData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TUzfWaterContentTimeListLink;
        Link.FWaterContentData.Invalidate;
      end;
    end;
  end;
end;

class function TUzfWaterContentCollection.ItemClass: TBoundaryItemClass;
begin
  result := TUzfWaterContentItem;
end;

procedure TUzfWaterContentCollection.SetBoundaryStartAndEndTime(
  BoundaryCount: Integer; Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TUzfWaterContentStorage).
    FWaterContentArray, BoundaryCount);
  inherited;
end;

{ TUzfExtinctionDepthCell }

procedure TUzfExtinctionDepthCell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TUzfExtinctionDepthCell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TUzfExtinctionDepthCell.GetExtinctionDepth: double;
begin
  result := Values.ExtinctionDepth;
end;

function TUzfExtinctionDepthCell.GetExtinctionDepthAnnotation: string;
begin
  result := Values.ExtinctionDepthAnnotation;
end;

function TUzfExtinctionDepthCell.GetExtinctionDepthPest: string;
begin
  result := Values.ExtinctionDepthPest;
end;

function TUzfExtinctionDepthCell.GetExtinctionDepthPestMethod: TPestParamMethod;
begin
  result := Values.ExtinctionDepthPestMethod;
end;

function TUzfExtinctionDepthCell.GetExtinctionDepthPestSeries: string;
begin
  result := Values.ExtinctionDepthPestSeries;
end;

function TUzfExtinctionDepthCell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TUzfExtinctionDepthCell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TUzfExtinctionDepthCell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TUzfExtinctionDepthCell.GetPestName(Index: Integer): string;
begin
  case Index of
    UzfExtinctDepthPosition:
      begin
        result := ExtinctionDepthPest;
      end;
    else
      begin
        result := inherited;
      end;
  end;
end;

function TUzfExtinctionDepthCell.GetPestSeriesMethod(
  Index: Integer): TPestParamMethod;
begin
  case Index of
    UzfExtinctDepthPosition:
      begin
        result := ExtinctionDepthPestMethod;
      end;
    else
      begin
        result := inherited;
      end;
  end;
end;

function TUzfExtinctionDepthCell.GetPestSeriesName(Index: Integer): string;
begin
  case Index of
    UzfExtinctDepthPosition:
      begin
        result := ExtinctionDepthPestSeries;
      end;
    else
      begin
        result := inherited;
      end;
  end;
end;

function TUzfExtinctionDepthCell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    UzfExtinctDepthPosition: result := ExtinctionDepthAnnotation;
    else Assert(False);
  end;
end;

function TUzfExtinctionDepthCell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    UzfExtinctDepthPosition: result := ExtinctionDepth;
    else Assert(False);
  end;
end;

function TUzfExtinctionDepthCell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TUzfExtinctionDepthCell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

procedure TUzfExtinctionDepthCell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TUzfExtinctionDepthCell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TUzfExtinctionDepthCell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TUzfExtinctionDepthCell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TUzfExtinctionDepthCell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TUzfWaterContentCell }

procedure TUzfWaterContentCell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TUzfWaterContentCell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TUzfWaterContentCell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TUzfWaterContentCell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TUzfWaterContentCell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TUzfWaterContentCell.GetMinimumWaterContentPest: string;
begin
  result := Values.MinimumWaterContentPest
end;

function TUzfWaterContentCell.GetMinimumWaterContentPestMethod: TPestParamMethod;
begin
  result := Values.MinimumWaterContentPestMethod;
end;

function TUzfWaterContentCell.GetMinimumWaterContentPestSeries: string;
begin
  result := Values.MinimumWaterContentPestSeries
end;

function TUzfWaterContentCell.GetPestName(Index: Integer): string;
begin
  case Index of
    UzfWaterContentPosition:
      begin
        result := MinimumWaterContentPest;
      end;
    else
      begin
        result := inherited;
      end;
  end;
end;

function TUzfWaterContentCell.GetPestSeriesMethod(
  Index: Integer): TPestParamMethod;
begin
  case Index of
    UzfWaterContentPosition:
      begin
        result := MinimumWaterContentPestMethod;
      end;
    else
      begin
        result := inherited;
      end;
  end;
end;

function TUzfWaterContentCell.GetPestSeriesName(Index: Integer): string;
begin
  case Index of
    UzfWaterContentPosition:
      begin
        result := MinimumWaterContentPestSeries;
      end;
    else
      begin
        result := inherited;
      end;
  end;
end;

function TUzfWaterContentCell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    UzfWaterContentPosition: result := WaterContentAnnotation;
    else Assert(False);
  end;
end;

function TUzfWaterContentCell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    UzfWaterContentPosition: result := WaterContent;
    else Assert(False);
  end;
end;

function TUzfWaterContentCell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TUzfWaterContentCell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TUzfWaterContentCell.GetWaterContent: double;
begin
  result := Values.MinimumWaterContent;
end;

function TUzfWaterContentCell.GetWaterContentAnnotation: string;
begin
  result := Values.MinimumWaterContentAnnotation;
end;

procedure TUzfWaterContentCell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TUzfWaterContentCell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TUzfWaterContentCell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TUzfWaterContentCell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TUzfWaterContentCell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TUzfExtinctionDepthRecord }

procedure TUzfExtinctionDepthRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, ExtinctionDepth);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Strings.IndexOf(ExtinctionDepthAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(ExtinctionDepthPest));
  WriteCompInt(Comp, Strings.IndexOf(ExtinctionDepthPestSeries));
  WriteCompInt(Comp, Ord(ExtinctionDepthPestMethod));
//  WriteCompString(Comp, ExtinctionDepthAnnotation);
end;

procedure TUzfExtinctionDepthRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(ExtinctionDepthAnnotation);
  Strings.Add(ExtinctionDepthPest);
  Strings.Add(ExtinctionDepthPestSeries);
end;

procedure TUzfExtinctionDepthRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  ExtinctionDepth := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  ExtinctionDepthAnnotation := Annotations[ReadCompInt(Decomp)];
  ExtinctionDepthPest := Annotations[ReadCompInt(Decomp)];
  ExtinctionDepthPestSeries := Annotations[ReadCompInt(Decomp)];
  ExtinctionDepthPestMethod := TPestParamMethod(ReadCompInt(Decomp));
end;

{ TUzfWaterContentRecord }

procedure TUzfWaterContentRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, MinimumWaterContent);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Strings.IndexOf(MinimumWaterContentAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(MinimumWaterContentPest));
  WriteCompInt(Comp, Strings.IndexOf(MinimumWaterContentPestSeries));
  WriteCompInt(Comp, Ord(MinimumWaterContentPestMethod));
end;

procedure TUzfWaterContentRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(MinimumWaterContentAnnotation);
  Strings.Add(MinimumWaterContentPest);
  Strings.Add(MinimumWaterContentPestSeries);
end;

procedure TUzfWaterContentRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  MinimumWaterContent := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  MinimumWaterContentAnnotation := Annotations[ReadCompInt(Decomp)];
  MinimumWaterContentPest := Annotations[ReadCompInt(Decomp)];
  MinimumWaterContentPestSeries := Annotations[ReadCompInt(Decomp)];
  MinimumWaterContentPestMethod := TPestParamMethod(ReadCompInt(Decomp));
end;

{ TUzfExtinctDepthStorage }

procedure TUzfExtinctDepthStorage.Clear;
begin
  SetLength(FExtinctDepthArray, 0);
  FCleared := True;
end;

procedure TUzfExtinctDepthStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    InitializeStrings(Strings);
    Count := Length(FExtinctDepthArray);
    for Index := 0 to Count - 1 do
    begin
      FExtinctDepthArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FExtinctDepthArray[Index].Cache(Compressor, Strings);
    end;
  finally
    Strings.Free;
  end;
end;

procedure TUzfExtinctDepthStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FExtinctDepthArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FExtinctDepthArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TUzfExtinctDepthStorage.GetExtinctDepthArray: TUzfExtinctDepthArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FExtinctDepthArray;
end;

{ TUzfWaterContentStorage }

procedure TUzfWaterContentStorage.Clear;
begin
  SetLength(FWaterContentArray, 0);
  FCleared := True;
end;

procedure TUzfWaterContentStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    InitializeStrings(Strings);
    Count := Length(FWaterContentArray);
    for Index := 0 to Count - 1 do
    begin
      FWaterContentArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FWaterContentArray[Index].Cache(Compressor, Strings);
    end;
  finally
    Strings.Free;
  end;
end;

procedure TUzfWaterContentStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FWaterContentArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FWaterContentArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TUzfWaterContentStorage.GetWaterContentArray: TUzfWaterContentArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FWaterContentArray;
end;

{ TUzfEvtTimeListLink }

procedure TUzfEvtTimeListLink.CreateTimeLists;
begin
  inherited;
  EvapotranspirationRateData.NonParamDescription :=
    StrEvapoTranspiration;
  if Model <> nil then
  begin
    EvapotranspirationRateData.OnInvalidate :=
      (Model as TCustomModel).InvalidateMfUzfEtDemand;
  end;
end;

{ TUzfExtinctionDepthTimeListLink }

procedure TUzfExtinctionDepthTimeListLink.CreateTimeLists;
begin
  inherited;
  FExtinctionDepthData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FExtinctionDepthData.NonParamDescription := StrETExtinctionDepth;
  FExtinctionDepthData.ParamDescription := ' ' + StrETExtinctionDepth;
  AddTimeList(FExtinctionDepthData);
  if Model <> nil then
  begin
    FExtinctionDepthData.OnInvalidate := (Model as TCustomModel).InvalidateMfUzfExtinctionDepth;
  end;
end;

destructor TUzfExtinctionDepthTimeListLink.Destroy;
begin
  FExtinctionDepthData.Free;
  inherited;
end;

{ TUzfWaterContentTimeListLink }

procedure TUzfWaterContentTimeListLink.CreateTimeLists;
begin
  inherited;
  FWaterContentData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FWaterContentData.NonParamDescription := StrETExtinctionWater;
  FWaterContentData.ParamDescription := ' ' + StrETExtinctionWater;
  AddTimeList(FWaterContentData);
  if Model <> nil then
  begin
    FWaterContentData.OnInvalidate := (Model as TCustomModel).InvalidateMfUzfWaterContent;
  end;
end;

destructor TUzfWaterContentTimeListLink.Destroy;
begin
  FWaterContentData.Free;
  inherited;
end;

{ TUzfInfiltrationRateTimeListLink }

procedure TUzfInfiltrationRateTimeListLink.CreateTimeLists;
begin
  inherited;
  RechargeRateData.NonParamDescription := StrInfiltrationRate;
  if Model <> nil then
  begin
    RechargeRateData.OnInvalidate := (Model as TCustomModel).InvalidateMfUzfInfiltration;
  end;
end;

class function TUzfInfiltrationRateCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TUzfInfiltrationRateTimeListLink;
end;

function TUzfInfiltrationRateCollection.PackageAssignmentMethod(
  AModel: TBaseModel): TUpdateMethod;
var
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  result := LocalModel.ModflowPackages.UzfPackage.AssignmentMethod;
end;

end.

