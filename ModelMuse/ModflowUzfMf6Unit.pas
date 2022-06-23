unit ModflowUzfMf6Unit;

interface

uses Windows, ZLib, SysUtils, ModflowCellUnit, System.Classes,
  ModflowBoundaryUnit, FormulaManagerUnit, OrderedCollectionUnit, GoPhastTypes,
  SubscriptionUnit, Mt3dmsChemUnit;

type
  TUzfOb = (uoGW_Recharge, uoGW_Discharge, uoDischargeToMvr,
    uoSatZoneEvapotranspiration, uoInfiltration, uoMvrInflow,
    uoRejectInfiltration, uoRejectInfiltrationToMvr,
    uoUnsatZoneEvapotranspiration, uoStorage, uoNetInfiltration, uoWaterContent);
  TUzfObs = set of TUzfOb;

  TUzfMf6Record = record
    Cell: TCellLocation;
    StartingTime: double;
    EndingTime: double;

    Infiltration: double;
    PotentialET: double;
    ExtinctionDepth: double;
    ExtinctionWaterContent: double;
    AirEntryPotential: double;
    RootPotential: double;
    RootActivity: double;

    InfiltrationAnnotation: string;
    PotentialETAnnotation: string;
    ExtinctionDepthAnnotation: string;
    ExtinctionWaterContentAnnotation: string;
    AirEntryPotentialAnnotation: string;
    RootPotentialAnnotation: string;
    RootActivityAnnotation: string;

    InfiltrationPest: string;
    PotentialETPest: string;
    ExtinctionDepthPest: string;
    ExtinctionWaterContentPest: string;
    AirEntryPotentialPest: string;
    RootPotentialPest: string;
    RootActivityPest: string;

    InfiltrationPestSeriesName: string;
    PotentialETPestSeriesName: string;
    ExtinctionDepthPestSeriesName: string;
    ExtinctionWaterContentPestSeriesName: string;
    AirEntryPotentialPestSeriesName: string;
    RootPotentialPestSeriesName: string;
    RootActivityPestSeriesName: string;

    InfiltrationPestSeriesMethod: TPestParamMethod;
    PotentialETPestSeriesMethod: TPestParamMethod;
    ExtinctionDepthPestSeriesMethod: TPestParamMethod;
    ExtinctionWaterContentPestSeriesMethod: TPestParamMethod;
    AirEntryPotentialPestSeriesMethod: TPestParamMethod;
    RootPotentialPestSeriesMethod: TPestParamMethod;
    RootActivityPestSeriesMethod: TPestParamMethod;

    InfiltrationTimeSeriesName: string;
    PotentialETTimeSeriesName: string;
    ExtinctionDepthTimeSeriesName: string;
    ExtinctionWaterContentTimeSeriesName: string;
    AirEntryPotentialTimeSeriesName: string;
    RootPotentialTimeSeriesName: string;
    RootActivityTimeSeriesName: string;

    MvrUsed: Boolean;
    MvrIndex: Integer;

    // GWT
    GwtStatus: TGwtBoundaryStatusArray;
    SpecifiedConcentrations: TGwtCellData;
    InfiltrationConcentrations: TGwtCellData;
    EvapConcentrations: TGwtCellData;

    procedure Assign(const Item: TUzfMf6Record);
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TUzfMf6Array = array of TUzfMf6Record;

  TUzfMf6Storage = class(TCustomBoundaryStorage)
  private
    FUzfMf6Array: TUzfMf6Array;
    FSpeciesCount: Integer;
    function GetUzfMf6Array: TUzfMf6Array;
    procedure SetSpeciesCount(const Value: Integer);
  protected
    procedure Restore(DecompressionStream: TDecompressionStream;
      Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property UzfMf6Array: TUzfMf6Array read GetUzfMf6Array;
    property SpeciesCount: Integer read FSpeciesCount write SetSpeciesCount;
  end;

  TUzfMf6Collection = class;

  TUztGwtConcCollection = class(TGwtConcStringCollection)
    constructor Create(Model: TBaseModel; AScreenObject: TObject;
      ParentCollection: TUzfMf6Collection);
  end;

  TUzfMf6Item = class(TCustomModflowBoundaryItem)
  private
    // See @link(UzfExtinctDepth).
    FInfiltration: TFormulaObject;
    FPotentialET: TFormulaObject;
    FUzfExtinctDepth: TFormulaObject;
    FExtinctionWaterContent: TFormulaObject;
    FAirEntryPotential: TFormulaObject;
    FRootPotential: TFormulaObject;
    FRootActivity: TFormulaObject;
    // GWT
    FGwtStatus: TGwtBoundaryStatusCollection;
    FSpecifiedConcentrations: TUztGwtConcCollection;
    FInfiltrationConcentrations: TUztGwtConcCollection;
    FEvapConcentrations: TUztGwtConcCollection;
    // See @link(UzfExtinctDepth).
    function GetAirEntryPotential: string;
    function GetExtinctionDepth: string;
    function GetExtinctionWaterContent: string;
    function GetInfiltration: string;
    function GetPotentialET: string;
    function GetRootActivity: string;
    function GetRootPotential: string;
    procedure SetAirEntryPotential(const Value: string);
    procedure SetExtinctionDepth(const Value: string);
    procedure SetExtinctionWaterContent(const Value: string);
    procedure SetInfiltration(const Value: string);
    procedure SetPotentialET(const Value: string);
    procedure SetRootActivity(const Value: string);
    procedure SetRootPotential(const Value: string);
    procedure SetEvapConcentrations(const Value: TUztGwtConcCollection);
    procedure SetGwtStatus(const Value: TGwtBoundaryStatusCollection);
    procedure SetInfiltrationConcentrations(const Value: TUztGwtConcCollection);
    procedure SetSpecifiedConcentrations(const Value: TUztGwtConcCollection);
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
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
  published
    // finf (always used)
    property Infiltration: string read GetInfiltration write SetInfiltration;
    // pet (Use only if SIMULATE_ET has been specified.)
    property PotentialET: string read GetPotentialET write SetPotentialET;
    // extdp (Use only if SIMULATE_ET has been specified.)
    property ExtinctionDepth: string read GetExtinctionDepth write SetExtinctionDepth;
    // extwc (Use only if SIMULATE_ET and UNSAT_ETWC have been specified.)
    property ExtinctionWaterContent: string read GetExtinctionWaterContent write SetExtinctionWaterContent;
    // ha (Use only if SIMULATE_ET and UNSAT_ETAE have been specified.)
    property AirEntryPotential: string read GetAirEntryPotential write SetAirEntryPotential;
    // hroot (Use only if SIMULATE_ET and UNSAT_ETAE have been specified.)
    property RootPotential: string read GetRootPotential write SetRootPotential;
    // rootact (Use only if SIMULATE_ET and UNSAT_ETAE have been specified.)
    property RootActivity: string read GetRootActivity write SetRootActivity;
    property GwtStatus: TGwtBoundaryStatusCollection read FGwtStatus write SetGwtStatus
      {$IFNDEF GWT}
      stored False
      {$ENDIF}
      ;
    property SpecifiedConcentrations: TUztGwtConcCollection read FSpecifiedConcentrations
      write SetSpecifiedConcentrations
      {$IFNDEF GWT}
      stored False
      {$ENDIF}
      ;
    property InfiltrationConcentrations: TUztGwtConcCollection read FInfiltrationConcentrations
      write SetInfiltrationConcentrations
      {$IFNDEF GWT}
      stored False
      {$ENDIF}
      ;
    property EvapConcentrations: TUztGwtConcCollection read FEvapConcentrations
      write SetEvapConcentrations
      {$IFNDEF GWT}
      stored False
      {$ENDIF}
      ;
  end;

  TUzfMf6TimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the extinction depths for a series of
    // cells over a series of time intervals.
    FInfiltrationData: TModflowTimeList;
    FPotentialETData: TModflowTimeList;
    FExtinctionDepthData: TModflowTimeList;
    FExtinctionWaterContentData: TModflowTimeList;
    FAirEntryPotentialData: TModflowTimeList;
    FRootPotentialData: TModflowTimeList;
    FRootActivityData: TModflowTimeList;
    // GWT
    FGwtStatusList: TModflowTimeLists;
    FSpecifiedConcList: TModflowTimeLists;
    FInfiltrationConcList: TModflowTimeLists;
    FEvapConcList: TModflowTimeLists;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  TUzfMf6Collection = class(TCustomMF_ArrayBoundColl)
  private
    procedure InvalidateUzfInfiltrationData(Sender: TObject);
    procedure InvalidateUzfPotentialETData(Sender: TObject);
    procedure InvalidateUzfExtinctionDepthData(Sender: TObject);
    procedure InvalidateUzfExtinctionWaterContentData(Sender: TObject);
    procedure InvalidateUzfAirEntryPotentialData(Sender: TObject);
    procedure InvalidateUzfRootPotentialData(Sender: TObject);
    procedure InvalidateUzfRootActivityData(Sender: TObject);
    // GWT
    procedure InvalidateGwtStatus(Sender: TObject);
    procedure InvalidateSpecifiedConcentrations(Sender: TObject);
    procedure InvalidateInfiltrationConcentrations(Sender: TObject);
    procedure InvalidateEvapConcentrations(Sender: TObject);
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
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
    procedure AssignDirectlySpecifiedValues( AnItem: TCustomModflowBoundaryItem;
      BoundaryStorage: TCustomBoundaryStorage); override;
  end;

  TUzfMf6_Cell = class(TValueCell)
  private
    FValues: TUzfMf6Record;
    FStressPeriod: integer;
    function GetInfiltration: double;
    function GetPotentialET: double;
    function GetExtinctionDepth: double;
    function GetExtinctionWaterContent: double;
    function GetAirEntryPotential: double;
    function GetRootPotential: double;
    function GetRootActivity: double;
    function GetInfiltrationAnnotation: string;
    function GetPotentialETAnnotation: string;
    function GetExtinctionDepthAnnotation: string;
    function GetExtinctionWaterContentAnnotation: string;
    function GetAirEntryPotentialAnnotation: string;
    function GetRootPotentialAnnotation: string;
    function GetRootActivityAnnotation: string;
    function GetMvrIndex: Integer;
    function GetMvrUsed: Boolean;
    function GetAirEntryPotentialPest: string;
    function GetAirEntryPotentialPestSeriesMethod: TPestParamMethod;
    function GetAirEntryPotentialPestSeriesName: string;
    function GetExtinctionDepthPest: string;
    function GetExtinctionDepthPestSeriesMethod: TPestParamMethod;
    function GetExtinctionDepthPestSeriesName: string;
    function GetExtinctionWaterContentPest: string;
    function GetExtinctionWaterContentPestSeriesMethod: TPestParamMethod;
    function GetExtinctionWaterContentPestSeriesName: string;
    function GetInfiltrationPest: string;
    function GetInfiltrationPestSeriesMethod: TPestParamMethod;
    function GetInfiltrationPestSeriesName: string;
    function GetPotentialETPest: string;
    function GetPotentialETPestSeriesMethod: TPestParamMethod;
    function GetPotentialETPestSeriesName: string;
    function GetRootActivityPest: string;
    function GetRootActivityPestSeriesMethod: TPestParamMethod;
    function GetRootActivityPestSeriesName: string;
    function GetRootPotentialPest: string;
    function GetRootPotentialPestSeriesMethod: TPestParamMethod;
    function GetRootPotentialPestSeriesName: string;
    function GetAirEntryPotentialTimeSeriesName: string;
    function GetExtinctionDepthTimeSeriesName: string;
    function GetExtinctionWaterContentTimeSeriesName: string;
    function GetPotentialETTimeSeriesName: string;
    function GetRootActivityTimeSeriesName: string;
    function GetRootPotentialTimeSeriesName: string;
    procedure SetAirEntryPotentialTimeSeriesName(const Value: string);
    procedure SetExtinctionDepthTimeSeriesName(const Value: string);
    procedure SetExtinctionWaterContentTimeSeriesName(const Value: string);
    procedure SetInfiltrationTimeSeriesName(const Value: string);
    procedure SetPotentialETTimeSeriesName(const Value: string);
    procedure SetRootActivityTimeSeriesName(const Value: string);
    procedure SetRootPotentialTimeSeriesName(const Value: string);
    function GetInfiltrationTimeSeriesName: string;
    function GeInfiltrationConcentrations: TGwtCellData;
    function GetEvapConcentrations: TGwtCellData;
    function GetGwtStatus: TGwtBoundaryStatusArray;
    function GetSpecifiedConcentrations: TGwtCellData;
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
    property Infiltration: double read GetInfiltration;
    property PotentialET: double read GetPotentialET;
    property ExtinctionDepth: double read GetExtinctionDepth;
    property ExtinctionWaterContent: double read GetExtinctionWaterContent;
    property AirEntryPotential: double read GetAirEntryPotential;
    property RootPotential: double read GetRootPotential;
    property RootActivity: double read GetRootActivity;
    property InfiltrationAnnotation: string read GetInfiltrationAnnotation;
    property PotentialETAnnotation: string read GetPotentialETAnnotation;
    property ExtinctionDepthAnnotation: string read GetExtinctionDepthAnnotation;
    property ExtinctionWaterContentAnnotation: string read GetExtinctionWaterContentAnnotation;
    property AirEntryPotentialAnnotation: string read GetAirEntryPotentialAnnotation;
    property RootPotentialAnnotation: string read GetRootPotentialAnnotation;
    property RootActivityAnnotation: string read GetRootActivityAnnotation;
    property MvrUsed: Boolean read GetMvrUsed;
    property MvrIndex: Integer read GetMvrIndex;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;

    property InfiltrationPest: string read GetInfiltrationPest;
    property PotentialETPest: string read GetPotentialETPest;
    property ExtinctionDepthPest: string read GetExtinctionDepthPest;
    property ExtinctionWaterContentPest: string read GetExtinctionWaterContentPest;
    property AirEntryPotentialPest: string read GetAirEntryPotentialPest;
    property RootPotentialPest: string read GetRootPotentialPest;
    property RootActivityPest: string read GetRootActivityPest;

    property InfiltrationPestSeriesName: string read GetInfiltrationPestSeriesName;
    property PotentialETPestSeriesName: string read GetPotentialETPestSeriesName;
    property ExtinctionDepthPestSeriesName: string read GetExtinctionDepthPestSeriesName;
    property ExtinctionWaterContentPestSeriesName: string read GetExtinctionWaterContentPestSeriesName;
    property AirEntryPotentialPestSeriesName: string read GetAirEntryPotentialPestSeriesName;
    property RootPotentialPestSeriesName: string read GetRootPotentialPestSeriesName;
    property RootActivityPestSeriesName: string read GetRootActivityPestSeriesName;

    property InfiltrationPestSeriesMethod: TPestParamMethod read GetInfiltrationPestSeriesMethod;
    property PotentialETPestSeriesMethod: TPestParamMethod read GetPotentialETPestSeriesMethod;
    property ExtinctionDepthPestSeriesMethod: TPestParamMethod read GetExtinctionDepthPestSeriesMethod;
    property ExtinctionWaterContentPestSeriesMethod: TPestParamMethod read GetExtinctionWaterContentPestSeriesMethod;
    property AirEntryPotentialPestSeriesMethod: TPestParamMethod read GetAirEntryPotentialPestSeriesMethod;
    property RootPotentialPestSeriesMethod: TPestParamMethod read GetRootPotentialPestSeriesMethod;
    property RootActivityPestSeriesMethod: TPestParamMethod read GetRootActivityPestSeriesMethod;

    property InfiltrationTimeSeriesName: string
      read GetInfiltrationTimeSeriesName write SetInfiltrationTimeSeriesName;
    property PotentialETTimeSeriesName: string read GetPotentialETTimeSeriesName
      write SetPotentialETTimeSeriesName;
    property ExtinctionDepthTimeSeriesName: string
      read GetExtinctionDepthTimeSeriesName
      write SetExtinctionDepthTimeSeriesName;
    property ExtinctionWaterContentTimeSeriesName: string
      read GetExtinctionWaterContentTimeSeriesName
      write SetExtinctionWaterContentTimeSeriesName;
    property AirEntryPotentialTimeSeriesName: string
      read GetAirEntryPotentialTimeSeriesName
      write SetAirEntryPotentialTimeSeriesName;
    property RootPotentialTimeSeriesName: string
      read GetRootPotentialTimeSeriesName
      write SetRootPotentialTimeSeriesName;
    property RootActivityTimeSeriesName: string
      read GetRootActivityTimeSeriesName
      write SetRootActivityTimeSeriesName;
    // GWT
    Property GwtStatus: TGwtBoundaryStatusArray read GetGwtStatus;
    Property SpecifiedConcentrations: TGwtCellData read GetSpecifiedConcentrations;
    Property InfiltrationConcentrations: TGwtCellData read GeInfiltrationConcentrations;
    Property EvapConcentrations: TGwtCellData read GetEvapConcentrations;
  end;

  TUzfMf6Boundary = class(TModflowBoundary)
  private

    FSurfaceDepressionDepth: TFormulaObject;
    FVerticalSaturatedK: TFormulaObject;
    FResidualWaterContent: TFormulaObject;
    FSaturatedWaterContent: TFormulaObject;
    FInitialWaterContent: TFormulaObject;
    FBrooksCoreyEpsilon: TFormulaObject;

    FSurfaceDepressionDepthObserver: TObserver;
    FVerticalSaturatedKObserver: TObserver;
    FResidualWaterContentObserver: TObserver;
    FSaturatedWaterContentObserver: TObserver;
    FInitialWaterContentObserver: TObserver;
    FBrooksCoreyEpsilonObserver: TObserver;
    FPestExtinctionDepthMethod: TPestParamMethod;
    FPestPotentialETMethod: TPestParamMethod;
    FPestExtinctionWaterContentMethod: TPestParamMethod;
    FPestAirEntryPotentialMethod: TPestParamMethod;
    FPestRootPotentialMethod: TPestParamMethod;
    FPestRootActivityMethod: TPestParamMethod;
    FPestInfiltrationMethod: TPestParamMethod;
    FInfiltration: TFormulaObject;
    FPotentialET: TFormulaObject;
    FExtinctionDepth: TFormulaObject;
    FExtinctionWaterContent: TFormulaObject;
    FAirEntryPotential: TFormulaObject;
    FRootPotential: TFormulaObject;
    FRootActivity: TFormulaObject;
    FPestAirEntryPotentialObserver: TObserver;
    FPestExtinctionDepthObserver: TObserver;
    FPestExtinctionWaterContentObserver: TObserver;
    FPestInfiltrationObserver: TObserver;
    FPestPotentialETObserver: TObserver;
    FPestRootActivityObserver: TObserver;
    FPestRootPotentialObserver: TObserver;
    FUsedObserver: TObserver;
    // GWT
    FStartingConcentrations: TStringConcCollection;
    FPestSpecifiedConcentrations: TUztGwtConcCollection;
    FPestSpecifiedConcentrationMethods: TGwtPestMethodCollection;
    FPestEvaporationConcentrations: TUztGwtConcCollection;
    FPestEvaporationConcentrationMethods: TGwtPestMethodCollection;
    FPestInfiltrationConcentrations: TUztGwtConcCollection;
    FPestInfiltrationConcentrationMethods: TGwtPestMethodCollection;
    FPestSpecifiedConcentrationObservers: TObserverList;
    FPestInfiltrationConcentrationObservers: TObserverList;
    FPestEvaporationConcentrationObservers: TObserverList;

    function GetBrooksCoreyEpsilon: string;
    function GetInitialWaterContent: string;
    function GetResidualWaterContent: string;
    function GetSaturatedWaterContent: string;
    function GetSurfaceDepressionDepth: string;
    function GetVerticalSaturatedK: string;
    procedure SetBrooksCoreyEpsilon(const Value: string);
    procedure SetInitialWaterContent(const Value: string);
    procedure SetResidualWaterContent(const Value: string);
    procedure SetSaturatedWaterContent(const Value: string);
    procedure SetSurfaceDepressionDepth(const Value: string);
    procedure SetVerticalSaturatedK(const Value: string);
    function GetBrooksCoreyEpsilonObserver: TObserver;
    function GetInitialWaterContentObserver: TObserver;
    function GetResidualWaterContentObserver: TObserver;
    function GetSaturatedWaterContentObserver: TObserver;
    function GetSurfaceDepressionDepthObserver: TObserver;
    function GetVerticalSaturatedKObserver: TObserver;
    procedure InvalidateInfiltrationData(Sender: TObject);
    procedure InvalidatePotentialETData(Sender: TObject);
    procedure InvalidateExtinctionDepthData(Sender: TObject);
    procedure InvalidateExtinctionWaterContentData(Sender: TObject);
    procedure InvalidateAirEntryPotentialData(Sender: TObject);
    procedure InvalidateRootPotentialData(Sender: TObject);
    procedure InvalidateRootActivityData(Sender: TObject);
    function GetPestAirEntryPotentialFormula: string;
    function GetPestAirEntryPotentialObserver: TObserver;
    function GetPestExtinctionDepthFormula: string;
    function GetPestExtinctionDepthObserver: TObserver;
    function GetPestExtinctionWaterContentFormula: string;
    function GetPestExtinctionWaterContentObserver: TObserver;
    function GetPestInfiltrationFormula: string;
    function GetPestInfiltrationObserver: TObserver;
    function GetPestPotentialETFormula: string;
    function GetPestPotentialETObserver: TObserver;
    function GetPestRootActivityFormula: string;
    function GetPestRootActivityObserver: TObserver;
    function GetPestRootPotentialFormula: string;
    function GetPestRootPotentialObserver: TObserver;
    procedure SetPestAirEntryPotentialFormula(const Value: string);
    procedure SetPestAirEntryPotentialMethod(const Value: TPestParamMethod);
    procedure SetPestExtinctionDepthFormula(const Value: string);
    procedure SetPestExtinctionDepthMethod(const Value: TPestParamMethod);
    procedure SetPestExtinctionWaterContentFormula(const Value: string);
    procedure SetPestExtinctionWaterContentMethod(
      const Value: TPestParamMethod);
    procedure SetPestInfiltrationFormula(const Value: string);
    procedure SetPestInfiltrationMethod(const Value: TPestParamMethod);
    procedure SetPestPotentialETFormula(const Value: string);
    procedure SetPestPotentialETMethod(const Value: TPestParamMethod);
    procedure SetPestRootActivityFormula(const Value: string);
    procedure SetPestRootActivityMethod(const Value: TPestParamMethod);
    procedure SetPestRootPotentialFormula(const Value: string);
    procedure SetPestRootPotentialMethod(const Value: TPestParamMethod);
    procedure SetPestEvaporationConcentrationMethods(
      const Value: TGwtPestMethodCollection);
    procedure SetPestEvaporationConcentrations(
      const Value: TUztGwtConcCollection);
    procedure SetPestInfiltrationConcentrationMethods(
      const Value: TGwtPestMethodCollection);
    procedure SetPestInfiltrationConcentrations(
      const Value: TUztGwtConcCollection);
    procedure SetPestSpecifiedConcentrationMethods(
      const Value: TGwtPestMethodCollection);
    procedure SetPestSpecifiedConcentrations(
      const Value: TUztGwtConcCollection);
    procedure SetStartingConcentrations(const Value: TStringConcCollection);
    function GetPestEvaporationConcentrationObserver(
      const Index: Integer): TObserver;
    function GetPestInfiltrationConcentrationObserver(
      const Index: Integer): TObserver;
    function GetPestSpecifiedConcentrationObserver(
      const Index: Integer): TObserver;
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TUzfMf6_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;

    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure CreateFormulaObjects; //override;
    function BoundaryObserverPrefix: string; override;
    procedure CreateObservers; //override;

    property SurfaceDepressionDepthObserver: TObserver read GetSurfaceDepressionDepthObserver;
    property VerticalSaturatedKObserver: TObserver read GetVerticalSaturatedKObserver;
    property ResidualWaterContentObserver: TObserver read GetResidualWaterContentObserver;
    property SaturatedWaterContentObserver: TObserver read GetSaturatedWaterContentObserver;
    property InitialWaterContentObserver: TObserver read GetInitialWaterContentObserver;
    property BrooksCoreyEpsilonObserver: TObserver read GetBrooksCoreyEpsilonObserver;

    procedure HandleChangedValue(Observer: TObserver); //override;
    function GetUsedObserver: TObserver; //override;
    function GetPestBoundaryFormula(FormulaIndex: integer): string; override;
    procedure SetPestBoundaryFormula(FormulaIndex: integer;
      const Value: string); override;
    function GetPestBoundaryMethod(FormulaIndex: integer): TPestParamMethod; override;
    procedure SetPestBoundaryMethod(FormulaIndex: integer;
      const Value: TPestParamMethod); override;
    property PestInfiltrationObserver: TObserver read GetPestInfiltrationObserver;
    property PestPotentialETObserver: TObserver read GetPestPotentialETObserver;
    property PestExtinctionDepthObserver: TObserver read GetPestExtinctionDepthObserver;
    property PestExtinctionWaterContentObserver: TObserver read GetPestExtinctionWaterContentObserver;
    property PestAirEntryPotentialObserver: TObserver read GetPestAirEntryPotentialObserver;
    property PestRootPotentialObserver: TObserver read GetPestRootPotentialObserver;
    property PestRootActivityObserver: TObserver read GetPestRootActivityObserver;

    property PestSpecifiedConcentrationObserver[const Index: Integer]: TObserver
      read GetPestSpecifiedConcentrationObserver;
    property PestInfiltrationConcentrationObserver[const Index: Integer]: TObserver
      read GetPestInfiltrationConcentrationObserver;
    property PestEvaporationConcentrationObserver[const Index: Integer]: TObserver
      read GetPestEvaporationConcentrationObserver;
    procedure InvalidatePestSpecConcData(Sender: TObject);
    procedure InvalidatePestEvapConcData(Sender: TObject);
    procedure InvalidatePestInfiltrationConcData(Sender: TObject);
  public
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent);override;
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TUzfMf6Storage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // ParamList is not used.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    procedure InvalidateDisplay; override;
    procedure Loaded;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
  published
    // surfdep
    property SurfaceDepressionDepth: string read GetSurfaceDepressionDepth
      write SetSurfaceDepressionDepth;
    // vks
    property VerticalSaturatedK: string read GetVerticalSaturatedK
      write SetVerticalSaturatedK;
    // thtr
    property ResidualWaterContent: string read GetResidualWaterContent
      write SetResidualWaterContent;
    // thts
    property SaturatedWaterContent: string read GetSaturatedWaterContent
      write SetSaturatedWaterContent;
    // thti
    property InitialWaterContent: string read GetInitialWaterContent
      write SetInitialWaterContent;
    // eps
    property BrooksCoreyEpsilon: string read GetBrooksCoreyEpsilon
      write SetBrooksCoreyEpsilon;

    property PestInfiltrationFormula: string
      read GetPestInfiltrationFormula
      write SetPestInfiltrationFormula;
    property PestInfiltrationMethod: TPestParamMethod
      read FPestInfiltrationMethod
      write SetPestInfiltrationMethod;
    property PestPotentialETFormula: string
      read GetPestPotentialETFormula
      write SetPestPotentialETFormula;
    property PestPotentialETMethod: TPestParamMethod
      read FPestPotentialETMethod
      write SetPestPotentialETMethod;
    property PestExtinctionDepthFormula: string
      read GetPestExtinctionDepthFormula
      write SetPestExtinctionDepthFormula;
    property PestExtinctionDepthMethod: TPestParamMethod
      read FPestExtinctionDepthMethod
      write SetPestExtinctionDepthMethod;
    property PestExtinctionWaterContentFormula: string
      read GetPestExtinctionWaterContentFormula
      write SetPestExtinctionWaterContentFormula;
    property PestExtinctionWaterContentMethod: TPestParamMethod
      read FPestExtinctionWaterContentMethod
      write SetPestExtinctionWaterContentMethod;
    property PestAirEntryPotentialFormula: string
      read GetPestAirEntryPotentialFormula
      write SetPestAirEntryPotentialFormula;
    property PestAirEntryPotentialMethod: TPestParamMethod
      read FPestAirEntryPotentialMethod
      write SetPestAirEntryPotentialMethod;
    property PestRootPotentialFormula: string
      read GetPestRootPotentialFormula
      write SetPestRootPotentialFormula;
    property PestRootPotentialMethod: TPestParamMethod
      read FPestRootPotentialMethod
      write SetPestRootPotentialMethod;
    property PestRootActivityFormula: string
      read GetPestRootActivityFormula
      write SetPestRootActivityFormula;
    property PestRootActivityMethod: TPestParamMethod
      read FPestRootActivityMethod
      write SetPestRootActivityMethod;
    // GWT
    property StartingConcentrations: TStringConcCollection
      read FStartingConcentrations write SetStartingConcentrations
      {$IFNDEF GWT}
      stored False
      {$ENDIF}
      ;
      property PestSpecifiedConcentrations: TUztGwtConcCollection
        read FPestSpecifiedConcentrations write SetPestSpecifiedConcentrations
      {$IFNDEF GWT}
      stored False
      {$ENDIF}
      ;
    property PestSpecifiedConcentrationMethods: TGwtPestMethodCollection
      read FPestSpecifiedConcentrationMethods write SetPestSpecifiedConcentrationMethods
      {$IFNDEF GWT}
      stored False
      {$ENDIF}
      ;
      property PestInfiltrationConcentrations: TUztGwtConcCollection
        read FPestInfiltrationConcentrations write SetPestInfiltrationConcentrations
      {$IFNDEF GWT}
      stored False
      {$ENDIF}
      ;
    property PestInfiltrationConcentrationMethods: TGwtPestMethodCollection
      read FPestInfiltrationConcentrationMethods write SetPestInfiltrationConcentrationMethods
      {$IFNDEF GWT}
      stored False
      {$ENDIF}
      ;
      property PestEvaporationConcentrations: TUztGwtConcCollection
        read FPestEvaporationConcentrations write SetPestEvaporationConcentrations
      {$IFNDEF GWT}
      stored False
      {$ENDIF}
      ;
    property PestEvaporationConcentrationMethods: TGwtPestMethodCollection
      read FPestEvaporationConcentrationMethods write SetPestEvaporationConcentrationMethods
      {$IFNDEF GWT}
      stored False
      {$ENDIF}
      ;
  end;

const
  UzfMf6InfiltrationPosition = 0;
  UzfMf6PotentialETPosition = 1;
  UzfMf6ExtinctionDepthPosition = 2;
  UzfMf6ExtinctionWaterContentPosition = 3;
  UzfMf6AirEntryPotentialPosition = 4;
  UzfMf6RootPotentialPosition = 5;
  UzfMf6RootActivityPosition = 6;
//  UzfGwtStart = 7;
  UzfBoundaryGwtStart = 13;

  UztGwtConcCount = 3;
  UzfGwtSpecifiedConcentrationPosition = 0;
  UzfGwtInfiltrationConcentrationsPosition = 1;
  UzfGwtEvapConcentrationsPosition = 2;
//  UzfBoundaryGwtStart = 13;



function TryGetUzfOb(const UzfObName: string; var UzfOb: TUzfOb): Boolean;
function UzfObToString(const UzfOb: TUzfOb): string;
Procedure FillUzfSeriesNames(AList: TStrings);

implementation

uses
  frmGoPhastUnit, PhastModelUnit, DataSetUnit,
  ScreenObjectUnit, ModflowTimeUnit, ModflowMvrUnit, ModflowUzfUnit,
  ModelMuseUtilities, ModflowRchUnit, ModflowEvtUnit,
  ModflowParameterUnit, CustomModflowWriterUnit;

const
  UzfObsNames: array[TUzfOb] of string = ('UZF_GW_Recharge', 'UZF_GW_Discharge', 'UZF_DischargeToMvr',
    'UZF_SatZoneEvapotranspiration', 'UZF_Infiltration', 'UZF_MvrInflow',
    'UZF_RejectInfiltration', 'UZF_RejectInfiltrationToMvr',
    'UZF_UnsatZoneEvapotranspiration', 'UZF_Storage', 'UZF_NetInfiltration', 'UZF_WaterContent');

var
  UzfObsNameList: TStringList;

procedure InitializeUzfObsNameList;
var
  index: TUzfOb;
begin
  UzfObsNameList := TStringList.Create;
  UzfObsNameList.CaseSensitive := False;
  for index := Low(TUzfOb) to High(TUzfOb) do
  begin
    UzfObsNameList.Add(UzfObsNames[index]);
  end;
end;

function TryGetUzfOb(const UzfObName: string; var UzfOb: TUzfOb): Boolean;
var
  index: Integer;
begin
  index := UzfObsNameList.IndexOf(UzfObName);
  result := index >= 0;
  if result then
  begin
    UzfOb := TUzfOb(index);
  end;
end;

Procedure FillUzfSeriesNames(AList: TStrings);
begin
  AList.Assign(UzfObsNameList);
end;

function UzfObToString(const UzfOb: TUzfOb): string;
begin
  result := UzfObsNames[UzfOb];
end;
{
  TUzfOb = (uoGW_Recharge, uoGW_Discharge, uoDischargeToMvr,
    uoSatZoneEvapotranspiration, uoInfiltration, uoMvrInflow,
    uoRejectInfiltration, uoRejectInfiltrationToMvr,
    uoUnsatZoneEvapotranspiration, uoStorage, uoNetInfiltration, uoWaterContent);
}

resourcestring
  StrUZFInfiltrationDat = 'UZF infiltration';
  StrUZFPotentialET = 'UZF potential ET';
  StrUZFExtinctionDepth = 'UZF extinction depth';
  StrUZFExtinctionWater = 'UZF extinction water content';
  StrUZFAirEntryPotent = 'UZF air entry potential';
  StrUZFRootPotential = 'UZF root potential';
  StrUZFRootActivity = 'UZF root activity';

const
  SurfaceDepressionDepthPosition = 0;
  VerticalSaturatedKPosition = 1;
  ResidualWaterContentPosition = 2;
  SaturatedWaterContentPosition = 3;
  InitialWaterContentPosition = 4;
  BrooksCoreyEpsilonPosition = 5;

  InfiltrationPosition = 6;
  PotentialETPosition = 7;
  ExtinctionDepthPosition = 8;
  ExtinctionWaterContentPosition = 9;
  AirEntryPotentialPosition = 10;
  RootPotentialPosition = 11;
  RootActivityPosition = 12;


{ TUzfMf6Record }

procedure TUzfMf6Record.Assign(const Item: TUzfMf6Record);
begin
  self := Item;
  SpecifiedConcentrations.Assign(Item.SpecifiedConcentrations);
  InfiltrationConcentrations.Assign(Item.InfiltrationConcentrations);
  EvapConcentrations.Assign(Item.EvapConcentrations);
  SetLength(GwtStatus, Length(GwtStatus));
end;

procedure TUzfMf6Record.Cache(Comp: TCompressionStream; Strings: TStringList);
var
  GwtStatusCount: Integer;
  SpeciesIndex: Integer;
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);

  WriteCompReal(Comp, Infiltration);
  WriteCompReal(Comp, PotentialET);
  WriteCompReal(Comp, ExtinctionDepth);
  WriteCompReal(Comp, ExtinctionWaterContent);
  WriteCompReal(Comp, AirEntryPotential);
  WriteCompReal(Comp, RootPotential);
  WriteCompReal(Comp, RootActivity);

  WriteCompInt(Comp, Strings.IndexOf(InfiltrationAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(PotentialETAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(ExtinctionDepthAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(ExtinctionWaterContentAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(AirEntryPotentialAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(RootPotentialAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(RootActivityAnnotation));

  WriteCompInt(Comp, Strings.IndexOf(InfiltrationPest));
  WriteCompInt(Comp, Strings.IndexOf(PotentialETPest));
  WriteCompInt(Comp, Strings.IndexOf(ExtinctionDepthPest));
  WriteCompInt(Comp, Strings.IndexOf(ExtinctionWaterContentPest));
  WriteCompInt(Comp, Strings.IndexOf(AirEntryPotentialPest));
  WriteCompInt(Comp, Strings.IndexOf(RootPotentialPest));
  WriteCompInt(Comp, Strings.IndexOf(RootActivityPest));

  WriteCompInt(Comp, Strings.IndexOf(InfiltrationPestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(PotentialETPestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(ExtinctionDepthPestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(ExtinctionWaterContentPestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(AirEntryPotentialPestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(RootPotentialPestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(RootActivityPestSeriesName));

  WriteCompInt(Comp, Ord(InfiltrationPestSeriesMethod));
  WriteCompInt(Comp, Ord(PotentialETPestSeriesMethod));
  WriteCompInt(Comp, Ord(ExtinctionDepthPestSeriesMethod));
  WriteCompInt(Comp, Ord(ExtinctionWaterContentPestSeriesMethod));
  WriteCompInt(Comp, Ord(AirEntryPotentialPestSeriesMethod));
  WriteCompInt(Comp, Ord(RootPotentialPestSeriesMethod));
  WriteCompInt(Comp, Ord(RootActivityPestSeriesMethod));

  WriteCompInt(Comp, Strings.IndexOf(InfiltrationTimeSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(PotentialETTimeSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(ExtinctionDepthTimeSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(ExtinctionWaterContentTimeSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(AirEntryPotentialTimeSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(RootPotentialTimeSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(RootActivityTimeSeriesName));

  WriteCompBoolean(Comp, MvrUsed);
  WriteCompInt(Comp, MvrIndex);

  // GWT
  SpecifiedConcentrations.Cache(Comp, Strings);
  InfiltrationConcentrations.Cache(Comp, Strings);
  EvapConcentrations.Cache(Comp, Strings);

  GwtStatusCount := Length(GwtStatus);
  WriteCompInt(Comp, GwtStatusCount);
  for SpeciesIndex := 0 to GwtStatusCount - 1 do
  begin
    WriteCompInt(Comp, Ord(GwtStatus[SpeciesIndex]));
  end;

end;

procedure TUzfMf6Record.RecordStrings(Strings: TStringList);
begin
  Strings.Add(InfiltrationAnnotation);
  Strings.Add(PotentialETAnnotation);
  Strings.Add(ExtinctionDepthAnnotation);
  Strings.Add(ExtinctionWaterContentAnnotation);
  Strings.Add(AirEntryPotentialAnnotation);
  Strings.Add(RootPotentialAnnotation);
  Strings.Add(RootActivityAnnotation);

  Strings.Add(InfiltrationPest);
  Strings.Add(PotentialETPest);
  Strings.Add(ExtinctionDepthPest);
  Strings.Add(ExtinctionWaterContentPest);
  Strings.Add(AirEntryPotentialPest);
  Strings.Add(RootPotentialPest);
  Strings.Add(RootActivityPest);

  Strings.Add(InfiltrationPestSeriesName);
  Strings.Add(PotentialETPestSeriesName);
  Strings.Add(ExtinctionDepthPestSeriesName);
  Strings.Add(ExtinctionWaterContentPestSeriesName);
  Strings.Add(AirEntryPotentialPestSeriesName);
  Strings.Add(RootPotentialPestSeriesName);
  Strings.Add(RootActivityPestSeriesName);

  Strings.Add(InfiltrationTimeSeriesName);
  Strings.Add(PotentialETTimeSeriesName);
  Strings.Add(ExtinctionDepthTimeSeriesName);
  Strings.Add(ExtinctionWaterContentTimeSeriesName);
  Strings.Add(AirEntryPotentialTimeSeriesName);
  Strings.Add(RootPotentialTimeSeriesName);
  Strings.Add(RootActivityTimeSeriesName);

  // GWT
  SpecifiedConcentrations.RecordStrings(Strings);
  InfiltrationConcentrations.RecordStrings(Strings);
  EvapConcentrations.RecordStrings(Strings);

end;

procedure TUzfMf6Record.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
var
  GwtStatusCount: Integer;
  SpeciesIndex: Integer;
begin
  Cell := ReadCompCell(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);

  Infiltration := ReadCompReal(Decomp);
  PotentialET := ReadCompReal(Decomp);
  ExtinctionDepth := ReadCompReal(Decomp);
  ExtinctionWaterContent := ReadCompReal(Decomp);
  AirEntryPotential := ReadCompReal(Decomp);
  RootPotential := ReadCompReal(Decomp);
  RootActivity := ReadCompReal(Decomp);

  InfiltrationAnnotation := Annotations[ReadCompInt(Decomp)];
  PotentialETAnnotation := Annotations[ReadCompInt(Decomp)];
  ExtinctionDepthAnnotation := Annotations[ReadCompInt(Decomp)];
  ExtinctionWaterContentAnnotation := Annotations[ReadCompInt(Decomp)];
  AirEntryPotentialAnnotation := Annotations[ReadCompInt(Decomp)];
  RootPotentialAnnotation := Annotations[ReadCompInt(Decomp)];
  RootActivityAnnotation := Annotations[ReadCompInt(Decomp)];

  InfiltrationPest := Annotations[ReadCompInt(Decomp)];
  PotentialETPest := Annotations[ReadCompInt(Decomp)];
  ExtinctionDepthPest := Annotations[ReadCompInt(Decomp)];
  ExtinctionWaterContentPest := Annotations[ReadCompInt(Decomp)];
  AirEntryPotentialPest := Annotations[ReadCompInt(Decomp)];
  RootPotentialPest := Annotations[ReadCompInt(Decomp)];
  RootActivityPest := Annotations[ReadCompInt(Decomp)];

  InfiltrationPestSeriesName := Annotations[ReadCompInt(Decomp)];
  PotentialETPestSeriesName := Annotations[ReadCompInt(Decomp)];
  ExtinctionDepthPestSeriesName := Annotations[ReadCompInt(Decomp)];
  ExtinctionWaterContentPestSeriesName := Annotations[ReadCompInt(Decomp)];
  AirEntryPotentialPestSeriesName := Annotations[ReadCompInt(Decomp)];
  RootPotentialPestSeriesName := Annotations[ReadCompInt(Decomp)];
  RootActivityPestSeriesName := Annotations[ReadCompInt(Decomp)];

  InfiltrationPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  PotentialETPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  ExtinctionDepthPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  ExtinctionWaterContentPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  AirEntryPotentialPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  RootPotentialPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  RootActivityPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));

  InfiltrationTimeSeriesName := Annotations[ReadCompInt(Decomp)];
  PotentialETTimeSeriesName := Annotations[ReadCompInt(Decomp)];
  ExtinctionDepthTimeSeriesName := Annotations[ReadCompInt(Decomp)];
  ExtinctionWaterContentTimeSeriesName := Annotations[ReadCompInt(Decomp)];
  AirEntryPotentialTimeSeriesName := Annotations[ReadCompInt(Decomp)];
  RootPotentialTimeSeriesName := Annotations[ReadCompInt(Decomp)];
  RootActivityTimeSeriesName := Annotations[ReadCompInt(Decomp)];

  MvrUsed := ReadCompBoolean(Decomp);
  MvrIndex := ReadCompInt(Decomp);

  // GWT
  SpecifiedConcentrations.Restore(Decomp, Annotations);
  InfiltrationConcentrations.Restore(Decomp, Annotations);
  EvapConcentrations.Restore(Decomp, Annotations);

  GwtStatusCount := ReadCompInt(Decomp);
  SetLength(GwtStatus, GwtStatusCount);
  for SpeciesIndex := 0 to GwtStatusCount - 1 do
  begin
    GwtStatus[SpeciesIndex] := TGwtBoundaryStatus(ReadCompInt(Decomp));
  end;

end;

{ TUzfMf6Storage }

procedure TUzfMf6Storage.Clear;
begin
  SetLength(FUzfMf6Array, 0);
  FCleared := True;
end;

function TUzfMf6Storage.GetUzfMf6Array: TUzfMf6Array;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FUzfMf6Array;
end;

procedure TUzfMf6Storage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FUzfMf6Array, Count);
  for Index := 0 to Count - 1 do
  begin
    FUzfMf6Array[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TUzfMf6Storage.SetSpeciesCount(const Value: Integer);
var
  Index: Integer;
begin
  FSpeciesCount := Value;
  for Index := 0 to Length(FUzfMf6Array)-1 do
  begin
    SetLength(FUzfMf6Array[Index].GwtStatus, FSpeciesCount);
    FUzfMf6Array[Index].SpecifiedConcentrations.SpeciesCount := FSpeciesCount;
    FUzfMf6Array[Index].InfiltrationConcentrations.SpeciesCount := FSpeciesCount;
    FUzfMf6Array[Index].EvapConcentrations.SpeciesCount := FSpeciesCount;
  end;
end;

procedure TUzfMf6Storage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin

  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FUzfMf6Array);
    for Index := 0 to Count - 1 do
    begin
      FUzfMf6Array[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FUzfMf6Array[Index].Cache(Compressor, Strings);
    end;
  finally
    Strings.Free;
  end;
end;

{ TUzfMf6Item }

procedure TUzfMf6Item.Assign(Source: TPersistent);
var
  UzfMf6Item: TUzfMf6Item;
  RchItem: TRchItem;
  EvtItem: TEvtItem;
  ExtinctDepthItem: TUzfExtinctDepthItem;
  WaterContentItem: TUzfWaterContentItem;
begin
  if Source is TUzfMf6Item then
  begin
    UzfMf6Item := TUzfMf6Item(Source);
    Infiltration := UzfMf6Item.Infiltration;
    PotentialET := UzfMf6Item.PotentialET;
    ExtinctionDepth := UzfMf6Item.ExtinctionDepth;
    ExtinctionWaterContent := UzfMf6Item.ExtinctionWaterContent;
    AirEntryPotential := UzfMf6Item.AirEntryPotential;
    RootPotential := UzfMf6Item.RootPotential;
    RootActivity := UzfMf6Item.RootActivity;

    // GWT
    GwtStatus := UzfMf6Item.GwtStatus;
    SpecifiedConcentrations := UzfMf6Item.SpecifiedConcentrations;
    InfiltrationConcentrations := UzfMf6Item.InfiltrationConcentrations;
    EvapConcentrations := UzfMf6Item.EvapConcentrations;
  end
  else if Source is TRchItem then
  begin
    RchItem := TRchItem(Source);
    Infiltration := RchItem.RechargeRate;
  end
  else if Source is TEvtItem then
  begin
    EvtItem := TEvtItem(Source);
    PotentialET := EvtItem.EvapotranspirationRate;
  end
  else if Source is TUzfExtinctDepthItem then
  begin
    ExtinctDepthItem := TUzfExtinctDepthItem(Source);
    ExtinctionDepth := ExtinctDepthItem.UzfExtinctDepth;
  end
  else if Source is TUzfExtinctDepthItem then
  begin
    WaterContentItem := TUzfWaterContentItem(Source);
    ExtinctionDepth := WaterContentItem.UzfWaterContent;
  end;

  inherited;
end;

procedure TUzfMf6Item.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TUzfMf6Collection;
  Observer: TObserver;
  ConcIndex: Integer;
begin
  ParentCollection := Collection as TUzfMf6Collection;

  Observer := FObserverList[UzfMf6InfiltrationPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateUzfInfiltrationData;

  Observer := FObserverList[UzfMf6PotentialETPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateUzfPotentialETData;

  Observer := FObserverList[UzfMf6ExtinctionDepthPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateUzfExtinctionDepthData;

  Observer := FObserverList[UzfMf6ExtinctionWaterContentPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateUzfExtinctionWaterContentData;

  Observer := FObserverList[UzfMf6AirEntryPotentialPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateUzfAirEntryPotentialData;

  Observer := FObserverList[UzfMf6RootPotentialPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateUzfRootPotentialData;

  Observer := FObserverList[UzfMf6RootActivityPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateUzfRootActivityData;

  for ConcIndex := 0 to SpecifiedConcentrations.Count - 1 do
  begin
    SpecifiedConcentrations[ConcIndex].Observer.OnUpToDateSet
      := ParentCollection.InvalidateSpecifiedConcentrations;
  end;

  for ConcIndex := 0 to InfiltrationConcentrations.Count - 1 do
  begin
    InfiltrationConcentrations[ConcIndex].Observer.OnUpToDateSet
      := ParentCollection.InvalidateInfiltrationConcentrations;
  end;

  for ConcIndex := 0 to EvapConcentrations.Count - 1 do
  begin
    EvapConcentrations[ConcIndex].Observer.OnUpToDateSet
      := ParentCollection.InvalidateEvapConcentrations;
  end;

end;

function TUzfMf6Item.BoundaryFormulaCount: integer;
const
  OffSet = 6;
begin
  result := 7;
  if frmGoPhast.PhastModel.GwtUsed then
  begin
    result := result + frmGoPhast.PhastModel.MobileComponents.Count * UztGwtConcCount + OffSet;
  end;
end;

constructor TUzfMf6Item.Create(Collection: TCollection);
var
  UzfCollection: TUzfMf6Collection;
begin
  UzfCollection := Collection as TUzfMf6Collection;
  FSpecifiedConcentrations := TUztGwtConcCollection.Create(Model, ScreenObject,
    UzfCollection);
  FInfiltrationConcentrations := TUztGwtConcCollection.Create(Model, ScreenObject,
    UzfCollection);
  FEvapConcentrations := TUztGwtConcCollection.Create(Model, ScreenObject,
    UzfCollection);

  inherited;
  FGwtStatus := TGwtBoundaryStatusCollection.Create(Model);
end;

procedure TUzfMf6Item.CreateFormulaObjects;
begin
  inherited;
  FInfiltration := CreateFormulaObject(dso3D);
  FPotentialET := CreateFormulaObject(dso3D);
  FUzfExtinctDepth := CreateFormulaObject(dso3D);
  FExtinctionWaterContent := CreateFormulaObject(dso3D);
  FAirEntryPotential := CreateFormulaObject(dso3D);
  FRootPotential := CreateFormulaObject(dso3D);
  FRootActivity := CreateFormulaObject(dso3D);
end;

destructor TUzfMf6Item.Destroy;
var
  Index: Integer;
begin
  FGwtStatus.Free;
  for Index := 0 to FSpecifiedConcentrations.Count - 1 do
  begin
    FSpecifiedConcentrations[Index].Value := '0';
  end;
  FSpecifiedConcentrations.Free;

  for Index := 0 to FinfiltrationConcentrations.Count - 1 do
  begin
    FinfiltrationConcentrations[Index].Value := '0';
  end;
  FinfiltrationConcentrations.Free;

  for Index := 0 to FEvapConcentrations.Count - 1 do
  begin
    FEvapConcentrations[Index].Value := '0';
  end;
  FEvapConcentrations.Free;


  Infiltration := '0';
  PotentialET := '0';
  ExtinctionDepth := '0';
  ExtinctionWaterContent := '0';
  AirEntryPotential := '0';
  RootPotential := '0';
  RootActivity := '0';
  inherited;
end;

function TUzfMf6Item.GetAirEntryPotential: string;
begin
  Result := FAirEntryPotential.Formula;
  ResetItemObserver(UzfMf6AirEntryPotentialPosition);
end;

function TUzfMf6Item.GetBoundaryFormula(Index: integer): string;
var
  ChemSpeciesCount: Integer;
begin
  result := '';
  case Index of
    UzfMf6InfiltrationPosition: result := Infiltration;
    UzfMf6PotentialETPosition: result := PotentialET;
    UzfMf6ExtinctionDepthPosition: result := ExtinctionDepth;
    UzfMf6ExtinctionWaterContentPosition: result := ExtinctionWaterContent;
    UzfMf6AirEntryPotentialPosition: result := AirEntryPotential;
    UzfMf6RootPotentialPosition: result := RootPotential;
    UzfMf6RootActivityPosition: result := RootActivity;
    else
      begin
        // GWT
        if frmGoPhast.PhastModel.GwtUsed then
        begin
          Index := Index-UzfBoundaryGwtStart;
          if Index < 0 then
          begin
            Exit;
          end;
//          Assert(Index >= 0);
          ChemSpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;
          while SpecifiedConcentrations.Count < ChemSpeciesCount do
          begin
            SpecifiedConcentrations.Add;
          end;
          if Index < ChemSpeciesCount then
          begin
            result := SpecifiedConcentrations[Index].Value;
            Exit;
          end;
          Index := Index - ChemSpeciesCount;

          while InfiltrationConcentrations.Count < ChemSpeciesCount do
          begin
            InfiltrationConcentrations.Add;
          end;
          if Index < ChemSpeciesCount then
          begin
            result := InfiltrationConcentrations[Index].Value;
            Exit;
          end;
          Index := Index - ChemSpeciesCount;

          while EvapConcentrations.Count < ChemSpeciesCount do
          begin
            EvapConcentrations.Add;
          end;
          if Index < ChemSpeciesCount then
          begin
            result := EvapConcentrations[Index].Value;
            Exit;
          end;
          Assert(False);
        end;
      end
  end;
end;

function TUzfMf6Item.GetExtinctionDepth: string;
begin
  Result := FUzfExtinctDepth.Formula;
  ResetItemObserver(UzfMf6ExtinctionDepthPosition);
end;

function TUzfMf6Item.GetExtinctionWaterContent: string;
begin
  Result := FExtinctionWaterContent.Formula;
  ResetItemObserver(UzfMf6ExtinctionWaterContentPosition);
end;

function TUzfMf6Item.GetInfiltration: string;
begin
  Result := FInfiltration.Formula;
  ResetItemObserver(UzfMf6InfiltrationPosition);
end;

function TUzfMf6Item.GetPotentialET: string;
begin
  Result := FPotentialET.Formula;
  ResetItemObserver(UzfMf6PotentialETPosition);
end;

procedure TUzfMf6Item.GetPropertyObserver(Sender: TObject; List: TList);
var
  ConcIndex: Integer;
  Item: TGwtConcStringValueItem;
begin
  if (Sender = FInfiltration) then
  begin
    List.Add( FObserverList[UzfMf6InfiltrationPosition]);
  end
  else if (Sender = FPotentialET) then
  begin
    List.Add( FObserverList[UzfMf6PotentialETPosition]);
  end
  else if (Sender = FUzfExtinctDepth) then
  begin
    List.Add( FObserverList[UzfMf6ExtinctionDepthPosition]);
  end
  else if (Sender = FExtinctionWaterContent) then
  begin
    List.Add( FObserverList[UzfMf6ExtinctionWaterContentPosition]);
  end
  else if (Sender = FAirEntryPotential) then
  begin
    List.Add( FObserverList[UzfMf6AirEntryPotentialPosition]);
  end
  else if (Sender = FRootPotential) then
  begin
    List.Add( FObserverList[UzfMf6RootPotentialPosition]);
  end
  else if (Sender = FRootActivity) then
  begin
    List.Add( FObserverList[UzfMf6RootActivityPosition]);
  end
  else
  begin
    // GWT
    for ConcIndex := 0 to SpecifiedConcentrations.Count - 1 do
    begin
      Item := SpecifiedConcentrations.Items[ConcIndex];
      if Item.ValueObject = Sender then
      begin
        List.Add(Item.Observer);
      end;
    end;

    for ConcIndex := 0 to InfiltrationConcentrations.Count - 1 do
    begin
      Item := InfiltrationConcentrations.Items[ConcIndex];
      if Item.ValueObject = Sender then
      begin
        List.Add(Item.Observer);
      end;
    end;

    for ConcIndex := 0 to EvapConcentrations.Count - 1 do
    begin
      Item := EvapConcentrations.Items[ConcIndex];
      if Item.ValueObject = Sender then
      begin
        List.Add(Item.Observer);
      end;
    end;
  end;
end;

function TUzfMf6Item.GetRootActivity: string;
begin
  Result := FRootActivity.Formula;
  ResetItemObserver(UzfMf6RootActivityPosition);
end;

function TUzfMf6Item.GetRootPotential: string;
begin
  Result := FRootPotential.Formula;
  ResetItemObserver(UzfMf6RootPotentialPosition);
end;

function TUzfMf6Item.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TUzfMf6Item;
begin
  result := (AnotherItem is TUzfMf6Item) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TUzfMf6Item(AnotherItem);
    result := (Item.Infiltration = Infiltration)
      and (Item.PotentialET = PotentialET)
      and (Item.ExtinctionDepth = ExtinctionDepth)
      and (Item.ExtinctionWaterContent = ExtinctionWaterContent)
      and (Item.AirEntryPotential = AirEntryPotential)
      and (Item.RootPotential = RootPotential)
      and (Item.RootActivity = RootActivity)
      and Item.SpecifiedConcentrations.IsSame(SpecifiedConcentrations)
      and Item.InfiltrationConcentrations.IsSame(InfiltrationConcentrations)
      and Item.EvapConcentrations.IsSame(EvapConcentrations)
      and Item.GwtStatus.IsSame(GwtStatus);
  end;
end;

procedure TUzfMf6Item.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FInfiltration,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FPotentialET,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FUzfExtinctDepth,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FExtinctionWaterContent,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FAirEntryPotential,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRootPotential,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRootActivity,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TUzfMf6Item.SetAirEntryPotential(const Value: string);
begin
  UpdateFormulaBlocks(Value, UzfMf6AirEntryPotentialPosition, FAirEntryPotential);
end;

procedure TUzfMf6Item.SetBoundaryFormula(Index: integer; const Value: string);
var
  ChemSpeciesCount: Integer;
begin
  case Index of
    UzfMf6InfiltrationPosition:
      Infiltration := Value;
    UzfMf6PotentialETPosition:
      PotentialET := Value;
    UzfMf6ExtinctionDepthPosition:
      ExtinctionDepth := Value;
    UzfMf6ExtinctionWaterContentPosition:
      ExtinctionWaterContent := Value;
    UzfMf6AirEntryPotentialPosition:
      AirEntryPotential := Value;
    UzfMf6RootPotentialPosition:
      RootPotential := Value;
    UzfMf6RootActivityPosition:
      RootActivity := Value;
    else
      begin
        // GWT
        if frmGoPhast.PhastModel.GwtUsed then
        begin
          Index := Index-UzfBoundaryGwtStart;
          if Index < 0 then
          begin
            Exit;
          end;
//          Assert(Index >= 0);
          ChemSpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;
          while SpecifiedConcentrations.Count < ChemSpeciesCount do
          begin
            SpecifiedConcentrations.Add;
          end;
          if Index < ChemSpeciesCount then
          begin
            SpecifiedConcentrations[Index].Value := Value;
            Exit;
          end;
          Index := Index - ChemSpeciesCount;

          while InfiltrationConcentrations.Count < ChemSpeciesCount do
          begin
            InfiltrationConcentrations.Add;
          end;
          if Index < ChemSpeciesCount then
          begin
            InfiltrationConcentrations[Index].Value := Value;
            Exit;
          end;
          Index := Index - ChemSpeciesCount;

          while EvapConcentrations.Count < ChemSpeciesCount do
          begin
            EvapConcentrations.Add;
          end;
          if Index < ChemSpeciesCount then
          begin
            EvapConcentrations[Index].Value := Value;
            Exit;
          end;
          Assert(False);
        end
        else
        begin
          Assert(False);
        end;
      end
  end;
end;

procedure TUzfMf6Item.SetEvapConcentrations(const Value: TUztGwtConcCollection);
begin
  FEvapConcentrations.Assign(Value);
end;

procedure TUzfMf6Item.SetExtinctionDepth(const Value: string);
begin
  UpdateFormulaBlocks(Value, UzfMf6ExtinctionDepthPosition, FUzfExtinctDepth);
end;

procedure TUzfMf6Item.SetExtinctionWaterContent(const Value: string);
begin
  UpdateFormulaBlocks(Value, UzfMf6ExtinctionWaterContentPosition, FExtinctionWaterContent);
end;

procedure TUzfMf6Item.SetGwtStatus(const Value: TGwtBoundaryStatusCollection);
begin
  FGwtStatus.Assign(Value);
end;

procedure TUzfMf6Item.SetInfiltration(const Value: string);
begin
  UpdateFormulaBlocks(Value, UzfMf6InfiltrationPosition, FInfiltration);
end;

procedure TUzfMf6Item.SetPotentialET(const Value: string);
begin
  UpdateFormulaBlocks(Value, UzfMf6PotentialETPosition, FPotentialET);
end;

procedure TUzfMf6Item.SetInfiltrationConcentrations(
  const Value: TUztGwtConcCollection);
begin
  FInfiltrationConcentrations.Assign(Value);
end;

procedure TUzfMf6Item.SetRootActivity(const Value: string);
begin
  UpdateFormulaBlocks(Value, UzfMf6RootActivityPosition, FRootActivity);
end;

procedure TUzfMf6Item.SetRootPotential(const Value: string);
begin
  UpdateFormulaBlocks(Value, UzfMf6RootPotentialPosition, FRootPotential);
end;

procedure TUzfMf6Item.SetSpecifiedConcentrations(
  const Value: TUztGwtConcCollection);
begin
  FSpecifiedConcentrations.Assign(Value);
end;

{ TUzfMf6Collection }

procedure TUzfMf6Collection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TUzfMf6Storage.Create(AModel));
end;

procedure TUzfMf6Collection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel; PestSeries: TStringList;
  PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList);
const
  PestOffset = 6; // 6 might not be the correct value.
var
  InfiltrationArray: TDataArray;
  PotentialETArray: TDataArray;
  ExtinctionDepthArray: TDataArray;
  ExtinctionWaterContentArray: TDataArray;
  AirEntryPotentialArray: TDataArray;
  RootPotentialArray: TDataArray;
  RootActivityArray: TDataArray;
  Boundary: TUzfMf6Storage;
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
  InfiltrationItems: TStringList;
  LocalInfiltrationPest: string;
  PotentialETItems: TStringList;
  LocalPotentialETPest: string;
  LocalExtinctionDepthPestSeriesName: string;
  ExtinctionDepthItems: TStringList;
  LocalExtinctionDepthPest: string;
  ExtinctionWaterContentItems: TStringList;
  LocalExtinctionWaterContentPest: string;
  AirEntryPotentialItems: TStringList;
  LocalAirEntryPotentialPest: string;
  RootPotentialItems: TStringList;
  LocalRootPotentialPest: string;
  RootActivityItems: TStringList;
  LocalRootActivityPest: string;
  LocalPotentialETPestSeriesName: string;
  LocalInfiltrationPestSeriesName: string;
  LocalInfiltrationPestSeriesMethod: TPestParamMethod;
  LocalPotentialETPestSeriesMethod: TPestParamMethod;
  LocalExtinctionDepthPestSeriesMethod: TPestParamMethod;
  LocalExtinctionWaterContentPestSeriesName: string;
  LocalExtinctionWaterContentPestSeriesMethod: TPestParamMethod;
  LocalAirEntryPotentialPestSeriesName: string;
  LocalAirEntryPotentialPestSeriesMethod: TPestParamMethod;
  LocalRootPotentialPestSeriesName: string;
  LocalRootPotentialPestSeriesMethod: TPestParamMethod;
  LocalRootActivityPestSeriesName: string;
  LocalRootActivityPestSeriesMethod: TPestParamMethod;
  InfiltrationTimeSeries: TStringList;
  LocalInfiltrationTimeSeriesName: String;
  PotentialETTimeSeries: TStringList;
  LocalPotentialETTimeSeriesName: String;
  ExtinctionDepthTimeSeries: TStringList;
  LocalExtinctionDepthTimeSeriesName: String;
  ExtinctionWaterContentTimeSeries: TStringList;
  LocalExtinctionWaterContentTimeSeriesName: String;
  AirEntryPotentialTimeSeries: TStringList;
  LocalAirEntryPotentialTimeSeriesName: String;
  RootPotentialTimeSeries: TStringList;
  LocalRootPotentialTimeSeriesName: String;
  RootActivityTimeSeries: TStringList;
  LocalRootActivityTimeSeriesName: String;
  LocalSpecifiedConcentrations: TDataArrayList;
  LocalInfiltrationConcentrations: TDataArrayList;
  LocalEvapConcentrations: TDataArrayList;
  ChemSpeciesCount: Integer;
  SpeciesIndex: Integer;
  DataSetIndex: Integer;
  SpecifiedConcentrationPestSeriesNames: TStringList;
  InfiltrationConcentrationPestSeriesNames: TStringList;
  EvapConcentrationPestSeriesNames: TStringList;
  APestSeriesName: string;
  SpecifiedConcentrationPestSeriesMethods: TPestMethodList;
  InfiltrationConcentrationPestSeriesMethods: TPestMethodList;
  EvapConcentrationPestSeriesMethods: TPestMethodList;
  APestSeriesMethod: TPestParamMethod;
  SpecifiedConcItemsList: TStringList;
  InfiltrationConcItemsList: TStringList;
  EvapConcItemsList: TStringList;
  PestItems: TStringList;
  SpecifiedConcentrationTimeSeriesNames: TStringList;
  InfiltrationConcentrationTimeSeriesNames: TStringList;
  EvapConcentrationTimeSeriesNames: TStringList;
  ATimeSeries: TStringList;
  ADataArray: TDataArray;
  PestItem: string;
  ATimeSeriesName: string;
begin
  LocalSpecifiedConcentrations := TDataArrayList.Create;
  LocalInfiltrationConcentrations := TDataArrayList.Create;
  LocalEvapConcentrations := TDataArrayList.Create;

  SpecifiedConcentrationPestSeriesNames := TStringList.Create;
  InfiltrationConcentrationPestSeriesNames := TStringList.Create;
  EvapConcentrationPestSeriesNames := TStringList.Create;

  SpecifiedConcentrationPestSeriesMethods := TPestMethodList.Create;
  InfiltrationConcentrationPestSeriesMethods := TPestMethodList.Create;
  EvapConcentrationPestSeriesMethods := TPestMethodList.Create;

  SpecifiedConcItemsList := TStringList.Create;
  InfiltrationConcItemsList := TStringList.Create;
  EvapConcItemsList := TStringList.Create;

  SpecifiedConcentrationTimeSeriesNames := TStringList.Create;
  InfiltrationConcentrationTimeSeriesNames := TStringList.Create;
  EvapConcentrationTimeSeriesNames := TStringList.Create;
  try
    LocalModel := AModel as TCustomModel;
    BoundaryIndex := 0;
    ChemSpeciesCount := LocalModel.MobileComponents.Count;

    InfiltrationArray := DataSets[UzfMf6InfiltrationPosition];
    PotentialETArray := DataSets[UzfMf6PotentialETPosition];
    ExtinctionDepthArray := DataSets[UzfMf6ExtinctionDepthPosition];
    ExtinctionWaterContentArray := DataSets[UzfMf6ExtinctionWaterContentPosition];
    AirEntryPotentialArray := DataSets[UzfMf6AirEntryPotentialPosition];
    RootPotentialArray := DataSets[UzfMf6RootPotentialPosition];
    RootActivityArray := DataSets[UzfMf6RootActivityPosition];
    if LocalModel.GwtUsed then
    begin
      DataSetIndex := UzfBoundaryGwtStart;
      for SpeciesIndex := 0 to ChemSpeciesCount - 1 do
      begin
        ADataArray := DataSets[DataSetIndex+SpeciesIndex-PestOffset];
        LocalSpecifiedConcentrations.Add(ADataArray);
        APestSeriesName := PestSeries[DataSetIndex+SpeciesIndex-PestOffset];
        SpecifiedConcentrationPestSeriesNames.Add(APestSeriesName);
        APestSeriesMethod := PestMethods[DataSetIndex+SpeciesIndex-PestOffset];
        SpecifiedConcentrationPestSeriesMethods.Add(APestSeriesMethod);
        PestItems := PestItemNames[DataSetIndex+SpeciesIndex-PestOffset];
        SpecifiedConcItemsList.Add(PestItems[ItemIndex]);
        ATimeSeries := TimeSeriesNames[DataSetIndex+SpeciesIndex-PestOffset];
        SpecifiedConcentrationTimeSeriesNames.Add(ATimeSeries[ItemIndex]);
      end;
      Inc(DataSetIndex, ChemSpeciesCount);
      for SpeciesIndex := 0 to ChemSpeciesCount - 1 do
      begin
        ADataArray := DataSets[DataSetIndex+SpeciesIndex-PestOffset];
        LocalInfiltrationConcentrations.Add(ADataArray);
        APestSeriesName := PestSeries[DataSetIndex+SpeciesIndex-PestOffset];
        InfiltrationConcentrationPestSeriesNames.Add(APestSeriesName);
        APestSeriesMethod := PestMethods[DataSetIndex+SpeciesIndex-PestOffset];
        InfiltrationConcentrationPestSeriesMethods.Add(APestSeriesMethod);
        PestItems := PestItemNames[DataSetIndex+SpeciesIndex-PestOffset];
        InfiltrationConcItemsList.Add(PestItems[ItemIndex]);
        ATimeSeries := TimeSeriesNames[DataSetIndex+SpeciesIndex-PestOffset];
        InfiltrationConcentrationTimeSeriesNames.Add(ATimeSeries[ItemIndex]);
//        LocalInfiltrationTimeSeriesName := InfiltrationTimeSeries[ItemIndex];

      end;
      Inc(DataSetIndex, ChemSpeciesCount);
      for SpeciesIndex := 0 to ChemSpeciesCount - 1 do
      begin
        ADataArray := DataSets[DataSetIndex+SpeciesIndex-PestOffset];
        LocalEvapConcentrations.Add(ADataArray);
        APestSeriesName := PestSeries[DataSetIndex+SpeciesIndex-PestOffset];
        EvapConcentrationPestSeriesNames.Add(APestSeriesName);
        APestSeriesMethod := PestMethods[DataSetIndex+SpeciesIndex-PestOffset];
        EvapConcentrationPestSeriesMethods.Add(APestSeriesMethod);
        PestItems := PestItemNames[DataSetIndex+SpeciesIndex-PestOffset];
        EvapConcItemsList.Add(PestItems[ItemIndex]);
        ATimeSeries := TimeSeriesNames[DataSetIndex+SpeciesIndex-PestOffset];
        EvapConcentrationTimeSeriesNames.Add(ATimeSeries[ItemIndex]);
      end;

    end;

    LocalInfiltrationPestSeriesName := PestSeries[InfiltrationPosition-PestOffset];
    LocalInfiltrationPestSeriesMethod := PestMethods[InfiltrationPosition-PestOffset];
    InfiltrationItems := PestItemNames[InfiltrationPosition-PestOffset];
    LocalInfiltrationPest := InfiltrationItems[ItemIndex];

    InfiltrationTimeSeries := TimeSeriesNames[InfiltrationPosition-PestOffset];
    LocalInfiltrationTimeSeriesName := InfiltrationTimeSeries[ItemIndex];

    LocalPotentialETPestSeriesName := PestSeries[PotentialETPosition-PestOffset];
    LocalPotentialETPestSeriesMethod := PestMethods[PotentialETPosition-PestOffset];
    PotentialETItems := PestItemNames[PotentialETPosition-PestOffset];
    LocalPotentialETPest := PotentialETItems[ItemIndex];

    PotentialETTimeSeries := TimeSeriesNames[PotentialETPosition-PestOffset];
    LocalPotentialETTimeSeriesName := PotentialETTimeSeries[ItemIndex];

    LocalExtinctionDepthPestSeriesName := PestSeries[ExtinctionDepthPosition-PestOffset];
    LocalExtinctionDepthPestSeriesMethod := PestMethods[ExtinctionDepthPosition-PestOffset];
    ExtinctionDepthItems := PestItemNames[ExtinctionDepthPosition-PestOffset];
    LocalExtinctionDepthPest := ExtinctionDepthItems[ItemIndex];

    ExtinctionDepthTimeSeries := TimeSeriesNames[ExtinctionDepthPosition-PestOffset];
    LocalExtinctionDepthTimeSeriesName := ExtinctionDepthTimeSeries[ItemIndex];

    LocalExtinctionWaterContentPestSeriesName := PestSeries[ExtinctionWaterContentPosition-PestOffset];
    LocalExtinctionWaterContentPestSeriesMethod := PestMethods[ExtinctionWaterContentPosition-PestOffset];
    ExtinctionWaterContentItems := PestItemNames[ExtinctionWaterContentPosition-PestOffset];
    LocalExtinctionWaterContentPest := ExtinctionWaterContentItems[ItemIndex];

    ExtinctionWaterContentTimeSeries := TimeSeriesNames[ExtinctionWaterContentPosition-PestOffset];
    LocalExtinctionWaterContentTimeSeriesName := ExtinctionWaterContentTimeSeries[ItemIndex];

    LocalAirEntryPotentialPestSeriesName := PestSeries[AirEntryPotentialPosition-PestOffset];
    LocalAirEntryPotentialPestSeriesMethod := PestMethods[AirEntryPotentialPosition-PestOffset];
    AirEntryPotentialItems := PestItemNames[AirEntryPotentialPosition-PestOffset];
    LocalAirEntryPotentialPest := AirEntryPotentialItems[ItemIndex];

    AirEntryPotentialTimeSeries := TimeSeriesNames[AirEntryPotentialPosition-PestOffset];
    LocalAirEntryPotentialTimeSeriesName := AirEntryPotentialTimeSeries[ItemIndex];

    LocalRootPotentialPestSeriesName := PestSeries[RootPotentialPosition-PestOffset];
    LocalRootPotentialPestSeriesMethod := PestMethods[RootPotentialPosition-PestOffset];
    RootPotentialItems := PestItemNames[RootPotentialPosition-PestOffset];
    LocalRootPotentialPest := RootPotentialItems[ItemIndex];

    RootPotentialTimeSeries := TimeSeriesNames[RootPotentialPosition-PestOffset];
    LocalRootPotentialTimeSeriesName := RootPotentialTimeSeries[ItemIndex];

    LocalRootActivityPestSeriesName := PestSeries[RootActivityPosition-PestOffset];
    LocalRootActivityPestSeriesMethod := PestMethods[RootActivityPosition-PestOffset];
    RootActivityItems := PestItemNames[RootActivityPosition-PestOffset];
    LocalRootActivityPest := RootActivityItems[ItemIndex];

    RootActivityTimeSeries := TimeSeriesNames[RootActivityPosition-PestOffset];
    LocalRootActivityTimeSeriesName := RootActivityTimeSeries[ItemIndex];

    Boundary := Boundaries[ItemIndex, AModel] as TUzfMf6Storage;
    InfiltrationArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
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
              if InfiltrationArray.IsValue[LayerIndex, RowIndex, ColIndex] then
              begin
                Assert(PotentialETArray.IsValue[LayerIndex, RowIndex, ColIndex]);
                Assert(ExtinctionDepthArray.IsValue[LayerIndex, RowIndex, ColIndex]);
                Assert(ExtinctionWaterContentArray.IsValue[LayerIndex, RowIndex, ColIndex]);
                Assert(AirEntryPotentialArray.IsValue[LayerIndex, RowIndex, ColIndex]);
                Assert(RootPotentialArray.IsValue[LayerIndex, RowIndex, ColIndex]);
                Assert(RootActivityArray.IsValue[LayerIndex, RowIndex, ColIndex]);

                with Boundary.UzfMf6Array[BoundaryIndex] do
                begin
                  Cell.Layer := LayerIndex;
                  Cell.Row := RowIndex;
                  Cell.Column := ColIndex;

                  Infiltration := InfiltrationArray.
                    RealData[LayerIndex, RowIndex, ColIndex];
                  InfiltrationAnnotation := InfiltrationArray.
                    Annotation[LayerIndex, RowIndex, ColIndex];
                  InfiltrationPest := LocalInfiltrationPest;
                  InfiltrationPestSeriesMethod := LocalInfiltrationPestSeriesMethod;
                  InfiltrationPestSeriesName := LocalInfiltrationPestSeriesName;
                  InfiltrationTimeSeriesName := LocalInfiltrationTimeSeriesName;

                  PotentialET := PotentialETArray.
                    RealData[LayerIndex, RowIndex, ColIndex];
                  PotentialETAnnotation := PotentialETArray.
                    Annotation[LayerIndex, RowIndex, ColIndex];
                  PotentialETPest := LocalPotentialETPest;
                  PotentialETPestSeriesMethod := LocalPotentialETPestSeriesMethod;
                  PotentialETPestSeriesName := LocalPotentialETPestSeriesName;
                  PotentialETTimeSeriesName := LocalPotentialETTimeSeriesName;

                  ExtinctionDepth := ExtinctionDepthArray.
                    RealData[LayerIndex, RowIndex, ColIndex];
                  ExtinctionDepthAnnotation := ExtinctionDepthArray.
                    Annotation[LayerIndex, RowIndex, ColIndex];
                  ExtinctionDepthPest := LocalExtinctionDepthPest;
                  ExtinctionDepthPestSeriesMethod := LocalExtinctionDepthPestSeriesMethod;
                  ExtinctionDepthPestSeriesName := LocalExtinctionDepthPestSeriesName;
                  ExtinctionDepthTimeSeriesName := LocalExtinctionDepthTimeSeriesName;

                  ExtinctionWaterContent := ExtinctionWaterContentArray.
                    RealData[LayerIndex, RowIndex, ColIndex];
                  ExtinctionWaterContentAnnotation := ExtinctionWaterContentArray.
                    Annotation[LayerIndex, RowIndex, ColIndex];
                  ExtinctionWaterContentPest := LocalExtinctionWaterContentPest;
                  ExtinctionWaterContentPestSeriesMethod := LocalExtinctionWaterContentPestSeriesMethod;
                  ExtinctionWaterContentPestSeriesName := LocalExtinctionWaterContentPestSeriesName;
                  ExtinctionWaterContentTimeSeriesName := LocalExtinctionWaterContentTimeSeriesName;

                  AirEntryPotential := AirEntryPotentialArray.
                    RealData[LayerIndex, RowIndex, ColIndex];
                  AirEntryPotentialAnnotation := AirEntryPotentialArray.
                    Annotation[LayerIndex, RowIndex, ColIndex];
                  AirEntryPotentialPest := LocalAirEntryPotentialPest;
                  AirEntryPotentialPestSeriesMethod := LocalAirEntryPotentialPestSeriesMethod;
                  AirEntryPotentialPestSeriesName := LocalAirEntryPotentialPestSeriesName;
                  AirEntryPotentialTimeSeriesName := LocalAirEntryPotentialTimeSeriesName;

                  RootPotential := RootPotentialArray.
                    RealData[LayerIndex, RowIndex, ColIndex];
                  RootPotentialAnnotation := RootPotentialArray.
                    Annotation[LayerIndex, RowIndex, ColIndex];
                  RootPotentialPest := LocalRootPotentialPest;
                  RootPotentialPestSeriesMethod := LocalRootPotentialPestSeriesMethod;
                  RootPotentialPestSeriesName := LocalRootPotentialPestSeriesName;
                  RootPotentialTimeSeriesName := LocalRootPotentialTimeSeriesName;

                  RootActivity := RootActivityArray.
                    RealData[LayerIndex, RowIndex, ColIndex];
                  RootActivityAnnotation := RootActivityArray.
                    Annotation[LayerIndex, RowIndex, ColIndex];
                  RootActivityPest := LocalRootActivityPest;
                  RootActivityPestSeriesMethod := LocalRootActivityPestSeriesMethod;
                  RootActivityPestSeriesName := LocalRootActivityPestSeriesName;
                  RootActivityTimeSeriesName := LocalRootActivityTimeSeriesName;

                  if LocalModel.GwtUsed then
                  begin
                    SpecifiedConcentrations.SpeciesCount := ChemSpeciesCount;
                    InfiltrationConcentrations.SpeciesCount := ChemSpeciesCount;
                    EvapConcentrations.SpeciesCount := ChemSpeciesCount;
                    for SpeciesIndex := 0 to ChemSpeciesCount - 1 do
                    begin
                      ADataArray := LocalSpecifiedConcentrations[SpeciesIndex];
                      APestSeriesName := SpecifiedConcentrationPestSeriesNames[SpeciesIndex];
                      APestSeriesMethod := SpecifiedConcentrationPestSeriesMethods[SpeciesIndex];
                      PestItem := SpecifiedConcItemsList[SpeciesIndex];
                      ATimeSeriesName := SpecifiedConcentrationTimeSeriesNames[SpeciesIndex];

                      SpecifiedConcentrations.Values[SpeciesIndex] :=
                        ADataArray.RealData[LayerIndex, RowIndex, ColIndex];
                      SpecifiedConcentrations.ValueAnnotations[SpeciesIndex] :=
                        ADataArray.Annotation[LayerIndex, RowIndex, ColIndex];
                      SpecifiedConcentrations.ValuePestNames[SpeciesIndex] := PestItem;
                      SpecifiedConcentrations.ValuePestSeriesNames[SpeciesIndex] := APestSeriesName;
                      SpecifiedConcentrations.ValuePestSeriesMethods[SpeciesIndex] := APestSeriesMethod;
                      SpecifiedConcentrations.ValueTimeSeriesNames[SpeciesIndex] := ATimeSeriesName;

                      ADataArray := LocalInfiltrationConcentrations[SpeciesIndex];
                      APestSeriesName := InfiltrationConcentrationPestSeriesNames[SpeciesIndex];
                      APestSeriesMethod := InfiltrationConcentrationPestSeriesMethods[SpeciesIndex];
                      PestItem := InfiltrationConcItemsList[SpeciesIndex];
                      ATimeSeriesName := InfiltrationConcentrationTimeSeriesNames[SpeciesIndex];

                      InfiltrationConcentrations.Values[SpeciesIndex] :=
                        ADataArray.RealData[LayerIndex, RowIndex, ColIndex];
                      InfiltrationConcentrations.ValueAnnotations[SpeciesIndex] :=
                        ADataArray.Annotation[LayerIndex, RowIndex, ColIndex];
                      InfiltrationConcentrations.ValuePestNames[SpeciesIndex] := PestItem;
                      InfiltrationConcentrations.ValuePestSeriesNames[SpeciesIndex] := APestSeriesName;
                      InfiltrationConcentrations.ValuePestSeriesMethods[SpeciesIndex] := APestSeriesMethod;
                      InfiltrationConcentrations.ValueTimeSeriesNames[SpeciesIndex] := ATimeSeriesName;

                      ADataArray := LocalEvapConcentrations[SpeciesIndex];
                      APestSeriesName := EvapConcentrationPestSeriesNames[SpeciesIndex];
                      APestSeriesMethod := EvapConcentrationPestSeriesMethods[SpeciesIndex];
                      PestItem := EvapConcItemsList[SpeciesIndex];
                      ATimeSeriesName := EvapConcentrationTimeSeriesNames[SpeciesIndex];

                      EvapConcentrations.Values[SpeciesIndex] :=
                        ADataArray.RealData[LayerIndex, RowIndex, ColIndex];
                      EvapConcentrations.ValueAnnotations[SpeciesIndex] :=
                        ADataArray.Annotation[LayerIndex, RowIndex, ColIndex];
                      EvapConcentrations.ValuePestNames[SpeciesIndex] := PestItem;
                      EvapConcentrations.ValuePestSeriesNames[SpeciesIndex] := APestSeriesName;
                      EvapConcentrations.ValuePestSeriesMethods[SpeciesIndex] := APestSeriesMethod;
                      EvapConcentrations.ValueTimeSeriesNames[SpeciesIndex] := ATimeSeriesName;
                    end;
                  end;
                end;
                Inc(BoundaryIndex);
              end;
            end;
          end;
        end;
      end;
    end;
    InfiltrationArray.CacheData;
    PotentialETArray.CacheData;
    ExtinctionDepthArray.CacheData;
    ExtinctionWaterContentArray.CacheData;
    AirEntryPotentialArray.CacheData;
    RootPotentialArray.CacheData;
    RootActivityArray.CacheData;

    Boundary.CacheData;
  finally
    LocalEvapConcentrations.Free;
    LocalInfiltrationConcentrations.Free;
    LocalSpecifiedConcentrations.Free;
    SpecifiedConcentrationPestSeriesNames.Free;
    InfiltrationConcentrationPestSeriesNames.Free;
    EvapConcentrationPestSeriesNames.Free;
    SpecifiedConcentrationPestSeriesMethods.Free;
    InfiltrationConcentrationPestSeriesMethods.Free;
    EvapConcentrationPestSeriesMethods.Free;
    SpecifiedConcItemsList.Free;
    InfiltrationConcItemsList.Free;
    EvapConcItemsList.Free;
    SpecifiedConcentrationTimeSeriesNames.Free;
    InfiltrationConcentrationTimeSeriesNames.Free;
    EvapConcentrationTimeSeriesNames.Free;
  end;
end;

procedure TUzfMf6Collection.AssignDirectlySpecifiedValues(
  AnItem: TCustomModflowBoundaryItem; BoundaryStorage: TCustomBoundaryStorage);
var
  UzfStorage: TUzfMf6Storage;
  index: integer;
  UzfMf6Item: TUzfMf6Item;
  SpeciesCount: Integer;
  SpeciesIndex: Integer;
begin
  inherited;
  // GWT
  if frmGoPhast.PhastModel.GwtUsed then
  begin
    SpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;
  end
  else
  begin
    SpeciesCount := 0;
  end;
  UzfStorage := BoundaryStorage as TUzfMf6Storage;
  UzfMf6Item := AnItem as TUzfMf6Item;

  for index := 0 to Length(UzfStorage.UzfMF6Array) - 1 do
  begin

    // Is this needed?
//    SetLength(UzfStorage.UzfMF6Array[index].GwtStatus, SpeciesCount);

    while UzfMf6Item.GwtStatus.Count < SpeciesCount do
    begin
      UzfMf6Item.GwtStatus.Add;
    end;
    for SpeciesIndex := 0 to SpeciesCount - 1 do
    begin
      UzfStorage.UzfMF6Array[index].GwtStatus[SpeciesIndex] :=
        UzfMf6Item.GwtStatus[SpeciesIndex].GwtBoundaryStatus
    end;
  end;
end;

function TUzfMf6Collection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  Result := TUzfMf6TimeListLink;
end;

procedure TUzfMf6Collection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList; Writer: TObject);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TUzfMf6Item;
  ScreenObject: TScreenObject;
  ALink: TUzfMf6TimeListLink;
  SeriesName: string;
  SeriesMethod: TPestParamMethod;
  PestItems: TStringList;
  TimeSeriesItems: TStringList;
  ItemFormula: string;
  TimeList: TModflowTimeList;
  LocalModel: TCustomModel;
  SpeciesIndex: Integer;
  BoundaryFormulaIndex: Integer;
  ChemSpeciesCount: Integer;
begin
  LocalModel := AModel as TCustomModel;
  ScreenObject := BoundaryGroup.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);

  // Infiltration
  SeriesName := BoundaryGroup.PestBoundaryFormula[InfiltrationPosition];
  PestSeries.Add(SeriesName);
  SeriesMethod := BoundaryGroup.PestBoundaryMethod[InfiltrationPosition];
  PestMethods.Add(SeriesMethod);

  PestItems := TStringList.Create;
  PestItemNames.Add(PestItems);
  TimeSeriesItems := TStringList.Create;
  TimeSeriesNames.Add(TimeSeriesItems);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TUzfMf6Item;
    BoundaryValues[Index].Time := Item.StartTime;

    ItemFormula := Item.Infiltration;
    AssignBoundaryFormula(AModel, SeriesName, SeriesMethod,
      PestItems, TimeSeriesItems, ItemFormula, Writer, BoundaryValues[Index]);
  end;
  ALink := TimeListLink.GetLink(AModel) as TUzfMf6TimeListLink;

  TimeList := ALink.FInfiltrationData;
  TimeList.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(TimeList.Count = Count);

  // Potential ET
  SeriesName := BoundaryGroup.PestBoundaryFormula[PotentialETPosition];
  PestSeries.Add(SeriesName);
  SeriesMethod := BoundaryGroup.PestBoundaryMethod[PotentialETPosition];
  PestMethods.Add(SeriesMethod);

  PestItems := TStringList.Create;
  PestItemNames.Add(PestItems);
  TimeSeriesItems := TStringList.Create;
  TimeSeriesNames.Add(TimeSeriesItems);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TUzfMf6Item;
    BoundaryValues[Index].Time := Item.StartTime;

    ItemFormula := Item.PotentialET;
    AssignBoundaryFormula(AModel, SeriesName, SeriesMethod,
      PestItems, TimeSeriesItems, ItemFormula, Writer, BoundaryValues[Index]);
  end;
  TimeList := ALink.FPotentialETData;
  TimeList.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(TimeList.Count = Count);

  // Extinction depth
  SeriesName := BoundaryGroup.PestBoundaryFormula[ExtinctionDepthPosition];
  PestSeries.Add(SeriesName);
  SeriesMethod := BoundaryGroup.PestBoundaryMethod[ExtinctionDepthPosition];
  PestMethods.Add(SeriesMethod);

  PestItems := TStringList.Create;
  PestItemNames.Add(PestItems);
  TimeSeriesItems := TStringList.Create;
  TimeSeriesNames.Add(TimeSeriesItems);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TUzfMf6Item;
    BoundaryValues[Index].Time := Item.StartTime;

    ItemFormula := Item.ExtinctionDepth;
    AssignBoundaryFormula(AModel, SeriesName, SeriesMethod,
      PestItems, TimeSeriesItems, ItemFormula, Writer, BoundaryValues[Index]);
  end;
  TimeList := ALink.FExtinctionDepthData;
  TimeList.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(TimeList.Count = Count);

  // Extinction water content
  SeriesName := BoundaryGroup.PestBoundaryFormula[ExtinctionWaterContentPosition];
  PestSeries.Add(SeriesName);
  SeriesMethod := BoundaryGroup.PestBoundaryMethod[ExtinctionWaterContentPosition];
  PestMethods.Add(SeriesMethod);

  PestItems := TStringList.Create;
  PestItemNames.Add(PestItems);
  TimeSeriesItems := TStringList.Create;
  TimeSeriesNames.Add(TimeSeriesItems);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TUzfMf6Item;
    BoundaryValues[Index].Time := Item.StartTime;

    ItemFormula := Item.ExtinctionWaterContent;
    AssignBoundaryFormula(AModel, SeriesName, SeriesMethod,
      PestItems, TimeSeriesItems, ItemFormula, Writer, BoundaryValues[Index]);
  end;
  TimeList := ALink.FExtinctionWaterContentData;
  TimeList.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(TimeList.Count = Count);

  // air entry potential
  SeriesName := BoundaryGroup.PestBoundaryFormula[AirEntryPotentialPosition];
  PestSeries.Add(SeriesName);
  SeriesMethod := BoundaryGroup.PestBoundaryMethod[AirEntryPotentialPosition];
  PestMethods.Add(SeriesMethod);

  PestItems := TStringList.Create;
  PestItemNames.Add(PestItems);
  TimeSeriesItems := TStringList.Create;
  TimeSeriesNames.Add(TimeSeriesItems);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TUzfMf6Item;
    BoundaryValues[Index].Time := Item.StartTime;

    ItemFormula := Item.AirEntryPotential;
    AssignBoundaryFormula(AModel, SeriesName, SeriesMethod,
      PestItems, TimeSeriesItems, ItemFormula, Writer, BoundaryValues[Index]);
  end;
  TimeList := ALink.FAirEntryPotentialData;
  TimeList.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(TimeList.Count = Count);

  // root potential
  SeriesName := BoundaryGroup.PestBoundaryFormula[RootPotentialPosition];
  PestSeries.Add(SeriesName);
  SeriesMethod := BoundaryGroup.PestBoundaryMethod[RootPotentialPosition];
  PestMethods.Add(SeriesMethod);

  PestItems := TStringList.Create;
  PestItemNames.Add(PestItems);
  TimeSeriesItems := TStringList.Create;
  TimeSeriesNames.Add(TimeSeriesItems);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TUzfMf6Item;
    BoundaryValues[Index].Time := Item.StartTime;

    ItemFormula := Item.RootPotential;
    AssignBoundaryFormula(AModel, SeriesName, SeriesMethod,
      PestItems, TimeSeriesItems, ItemFormula, Writer, BoundaryValues[Index]);
  end;
  TimeList := ALink.FRootPotentialData;
  TimeList.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(TimeList.Count = Count);

  // root activity
  SeriesName := BoundaryGroup.PestBoundaryFormula[RootActivityPosition];
  PestSeries.Add(SeriesName);
  SeriesMethod := BoundaryGroup.PestBoundaryMethod[RootActivityPosition];
  PestMethods.Add(SeriesMethod);

  PestItems := TStringList.Create;
  PestItemNames.Add(PestItems);
  TimeSeriesItems := TStringList.Create;
  TimeSeriesNames.Add(TimeSeriesItems);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TUzfMf6Item;
    BoundaryValues[Index].Time := Item.StartTime;

    ItemFormula := Item.RootActivity;
    AssignBoundaryFormula(AModel, SeriesName, SeriesMethod,
      PestItems, TimeSeriesItems, ItemFormula, Writer, BoundaryValues[Index]);
  end;
  TimeList := ALink.FRootActivityData;
  TimeList.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(TimeList.Count = Count);

  ChemSpeciesCount := 0;
  if LocalModel.GwtUsed then
  begin
    ChemSpeciesCount := LocalModel.MobileComponents.Count;
    BoundaryFormulaIndex := UzfBoundaryGwtStart;

    // specified concentrations
    for SpeciesIndex := 0 to ChemSpeciesCount - 1 do
    begin
      SeriesName := BoundaryGroup.PestBoundaryFormula[BoundaryFormulaIndex+SpeciesIndex];
      PestSeries.Add(SeriesName);
      SeriesMethod := BoundaryGroup.PestBoundaryMethod[BoundaryFormulaIndex+SpeciesIndex];
      PestMethods.Add(SeriesMethod);

      PestItems := TStringList.Create;
      PestItemNames.Add(PestItems);
      TimeSeriesItems := TStringList.Create;
      TimeSeriesNames.Add(TimeSeriesItems);

      for Index := 0 to Count - 1 do
      begin
        Item := Items[Index] as TUzfMf6Item;
        BoundaryValues[Index].Time := Item.StartTime;

        ItemFormula := Item.SpecifiedConcentrations[SpeciesIndex].Value;
        AssignBoundaryFormula(AModel, SeriesName, SeriesMethod,
          PestItems, TimeSeriesItems, ItemFormula, Writer, BoundaryValues[Index]);
      end;
      TimeList := ALink.FSpecifiedConcList[SpeciesIndex];
      TimeList.Initialize(BoundaryValues, ScreenObject, lctUse);
      Assert(TimeList.Count = Count);
    end;

    // infiltration concentrations
    Inc(BoundaryFormulaIndex, ChemSpeciesCount);
    for SpeciesIndex := 0 to ChemSpeciesCount - 1 do
    begin
      SeriesName := BoundaryGroup.PestBoundaryFormula[BoundaryFormulaIndex+SpeciesIndex];
      PestSeries.Add(SeriesName);
      SeriesMethod := BoundaryGroup.PestBoundaryMethod[BoundaryFormulaIndex+SpeciesIndex];
      PestMethods.Add(SeriesMethod);

      PestItems := TStringList.Create;
      PestItemNames.Add(PestItems);
      TimeSeriesItems := TStringList.Create;
      TimeSeriesNames.Add(TimeSeriesItems);

      for Index := 0 to Count - 1 do
      begin
        Item := Items[Index] as TUzfMf6Item;
        BoundaryValues[Index].Time := Item.StartTime;

        ItemFormula := Item.InfiltrationConcentrations[SpeciesIndex].Value;
        AssignBoundaryFormula(AModel, SeriesName, SeriesMethod,
          PestItems, TimeSeriesItems, ItemFormula, Writer, BoundaryValues[Index]);
      end;
      TimeList := ALink.FInfiltrationConcList[SpeciesIndex];
      TimeList.Initialize(BoundaryValues, ScreenObject, lctUse);
      Assert(TimeList.Count = Count);
    end;

    // evaporation concentrations
    Inc(BoundaryFormulaIndex, ChemSpeciesCount);
    for SpeciesIndex := 0 to ChemSpeciesCount - 1 do
    begin
      SeriesName := BoundaryGroup.PestBoundaryFormula[BoundaryFormulaIndex+SpeciesIndex];
      PestSeries.Add(SeriesName);
      SeriesMethod := BoundaryGroup.PestBoundaryMethod[BoundaryFormulaIndex+SpeciesIndex];
      PestMethods.Add(SeriesMethod);

      PestItems := TStringList.Create;
      PestItemNames.Add(PestItems);
      TimeSeriesItems := TStringList.Create;
      TimeSeriesNames.Add(TimeSeriesItems);

      for Index := 0 to Count - 1 do
      begin
        Item := Items[Index] as TUzfMf6Item;
        BoundaryValues[Index].Time := Item.StartTime;

        ItemFormula := Item.EvapConcentrations[SpeciesIndex].Value;
        AssignBoundaryFormula(AModel, SeriesName, SeriesMethod,
          PestItems, TimeSeriesItems, ItemFormula, Writer, BoundaryValues[Index]);
      end;
      TimeList := ALink.FEvapConcList[SpeciesIndex];
      TimeList.Initialize(BoundaryValues, ScreenObject, lctUse);
      Assert(TimeList.Count = Count);
    end;
  end;

  ClearBoundaries(AModel);
  SetBoundaryCapacity(TimeList.Count, AModel);
  for TimeIndex := 0 to TimeList.Count - 1 do
  begin
    AddBoundary(TUzfMf6Storage.Create(AModel));
  end;
  ListOfTimeLists.Add(ALink.FInfiltrationData);
  ListOfTimeLists.Add(ALink.FPotentialETData);
  ListOfTimeLists.Add(ALink.FExtinctionDepthData);
  ListOfTimeLists.Add(ALink.FExtinctionWaterContentData);
  ListOfTimeLists.Add(ALink.FAirEntryPotentialData);
  ListOfTimeLists.Add(ALink.FRootPotentialData);
  ListOfTimeLists.Add(ALink.FRootActivityData);
  if LocalModel.GwtUsed then
  begin
    for SpeciesIndex := 0 to ChemSpeciesCount - 1 do
    begin
      TimeList := ALink.FSpecifiedConcList[SpeciesIndex];
      ListOfTimeLists.Add(TimeList);
    end;

    for SpeciesIndex := 0 to ChemSpeciesCount - 1 do
    begin
      TimeList := ALink.FInfiltrationConcList[SpeciesIndex];
      ListOfTimeLists.Add(TimeList);
    end;

    for SpeciesIndex := 0 to ChemSpeciesCount - 1 do
    begin
      TimeList := ALink.FEvapConcList[SpeciesIndex];
      ListOfTimeLists.Add(TimeList);
    end;
  end;
end;

procedure TUzfMf6Collection.InvalidateEvapConcentrations(Sender: TObject);
var
  Index: Integer;
  TimeList: TModflowTimeList;
  PhastModel: TPhastModel;
  Link: TUzfMf6TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TUzfMf6TimeListLink;
    for Index := 0 to Link.FEvapConcList.Count - 1 do
    begin
      TimeList := Link.FEvapConcList[Index];
      TimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TUzfMf6TimeListLink;
      for Index := 0 to Link.FEvapConcList.Count - 1 do
      begin
        TimeList := Link.FEvapConcList[Index];
        TimeList.Invalidate;
      end;
    end;
  end;
end;

procedure TUzfMf6Collection.InvalidateGwtStatus(Sender: TObject);
begin

end;

procedure TUzfMf6Collection.InvalidateInfiltrationConcentrations(
  Sender: TObject);
var
  Index: Integer;
  TimeList: TModflowTimeList;
  PhastModel: TPhastModel;
  Link: TUzfMf6TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TUzfMf6TimeListLink;
    for Index := 0 to Link.FInfiltrationConcList.Count - 1 do
    begin
      TimeList := Link.FInfiltrationConcList[Index];
      TimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TUzfMf6TimeListLink;
      for Index := 0 to Link.FInfiltrationConcList.Count - 1 do
      begin
        TimeList := Link.FInfiltrationConcList[Index];
        TimeList.Invalidate;
      end;
    end;
  end;
end;

procedure TUzfMf6Collection.InvalidateSpecifiedConcentrations(Sender: TObject);
var
  Index: Integer;
  TimeList: TModflowTimeList;
  PhastModel: TPhastModel;
  Link: TUzfMf6TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TUzfMf6TimeListLink;
    for Index := 0 to Link.FSpecifiedConcList.Count - 1 do
    begin
      TimeList := Link.FSpecifiedConcList[Index];
      TimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TUzfMf6TimeListLink;
      for Index := 0 to Link.FSpecifiedConcList.Count - 1 do
      begin
        TimeList := Link.FSpecifiedConcList[Index];
        TimeList.Invalidate;
      end;
    end;
  end;
end;

procedure TUzfMf6Collection.InvalidateUzfAirEntryPotentialData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TUzfMf6TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TUzfMf6TimeListLink;
    Link.FAirEntryPotentialData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TUzfMf6TimeListLink;
      Link.FAirEntryPotentialData.Invalidate;
    end;
  end;
end;

procedure TUzfMf6Collection.InvalidateUzfExtinctionDepthData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TUzfMf6TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TUzfMf6TimeListLink;
    Link.FExtinctionDepthData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TUzfMf6TimeListLink;
      Link.FExtinctionDepthData.Invalidate;
    end;
  end;
end;

procedure TUzfMf6Collection.InvalidateUzfExtinctionWaterContentData(
  Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TUzfMf6TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TUzfMf6TimeListLink;
    Link.FExtinctionWaterContentData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TUzfMf6TimeListLink;
      Link.FExtinctionWaterContentData.Invalidate;
    end;
  end;
end;

procedure TUzfMf6Collection.InvalidateUzfInfiltrationData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TUzfMf6TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TUzfMf6TimeListLink;
    Link.FInfiltrationData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TUzfMf6TimeListLink;
      Link.FInfiltrationData.Invalidate;
    end;
  end;
end;

procedure TUzfMf6Collection.InvalidateUzfPotentialETData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TUzfMf6TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TUzfMf6TimeListLink;
    Link.FPotentialETData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TUzfMf6TimeListLink;
      Link.FPotentialETData.Invalidate;
    end;
  end;
end;

procedure TUzfMf6Collection.InvalidateUzfRootActivityData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TUzfMf6TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TUzfMf6TimeListLink;
    Link.FRootActivityData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TUzfMf6TimeListLink;
      Link.FRootActivityData.Invalidate;
    end;
  end;
end;

procedure TUzfMf6Collection.InvalidateUzfRootPotentialData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TUzfMf6TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TUzfMf6TimeListLink;
    Link.FRootPotentialData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TUzfMf6TimeListLink;
      Link.FRootPotentialData.Invalidate;
    end;
  end;
end;

class function TUzfMf6Collection.ItemClass: TBoundaryItemClass;
begin
  result := TUzfMf6Item;
end;

procedure TUzfMf6Collection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TUzfMf6Storage).
    FUzfMf6Array, BoundaryCount);
  (Boundaries[ItemIndex, AModel] as TUzfMf6Storage).SpeciesCount
    := (AModel as TCustomModel).MobileComponents.Count;
  inherited;
end;
{ TUzfMf6TimeListLink }

procedure TUzfMf6TimeListLink.CreateTimeLists;
var
  PhastModel: TPhastModel;
  SpeciesIndex: Integer;
  ConcTimeList: TModflowTimeList;
  LocalModel: TCustomModel;
begin
  inherited;

  FInfiltrationData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FInfiltrationData.NonParamDescription := StrUZFInfiltrationDat ;
  FInfiltrationData.ParamDescription := ' ' + StrUZFInfiltrationDat;
  AddTimeList(FInfiltrationData);
  if Model <> nil then
  begin
    FInfiltrationData.OnInvalidate :=
      (Model as TCustomModel).InvalidateUzfMf6Infiltration;
  end;

  FPotentialETData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FPotentialETData.NonParamDescription := StrUZFPotentialET ;
  FPotentialETData.ParamDescription := ' ' + StrUZFPotentialET;
  AddTimeList(FPotentialETData);
  if Model <> nil then
  begin
    FPotentialETData.OnInvalidate :=
      (Model as TCustomModel).InvalidateUzfMf6PotentialET;
  end;

  FExtinctionDepthData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FExtinctionDepthData.NonParamDescription := StrUZFExtinctionDepth ;
  FExtinctionDepthData.ParamDescription := ' ' + StrUZFExtinctionDepth;
  AddTimeList(FExtinctionDepthData);
  if Model <> nil then
  begin
    FExtinctionDepthData.OnInvalidate :=
      (Model as TCustomModel).InvalidateUzfMf6ExtinctionDepth;
  end;

  FExtinctionWaterContentData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FExtinctionWaterContentData.NonParamDescription := StrUZFExtinctionWater ;
  FExtinctionWaterContentData.ParamDescription := ' ' + StrUZFExtinctionWater;
  AddTimeList(FExtinctionWaterContentData);
  if Model <> nil then
  begin
    FExtinctionWaterContentData.OnInvalidate :=
      (Model as TCustomModel).InvalidateUzfMf6ExtinctionWaterContent;
  end;

  FAirEntryPotentialData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FAirEntryPotentialData.NonParamDescription := StrUZFAirEntryPotent ;
  FAirEntryPotentialData.ParamDescription := ' ' + StrUZFAirEntryPotent;
  AddTimeList(FAirEntryPotentialData);
  if Model <> nil then
  begin
    FAirEntryPotentialData.OnInvalidate :=
      (Model as TCustomModel).InvalidateUzfMf6AirEntryPotential;
  end;

  FRootPotentialData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FRootPotentialData.NonParamDescription := StrUZFRootPotential ;
  FRootPotentialData.ParamDescription := ' ' + StrUZFRootPotential;
  AddTimeList(FRootPotentialData);
  if Model <> nil then
  begin
    FRootPotentialData.OnInvalidate :=
      (Model as TCustomModel).InvalidateUzfMf6RootPotential;
  end;

  FRootActivityData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FRootActivityData.NonParamDescription := StrUZFRootActivity ;
  FRootActivityData.ParamDescription := ' ' + StrUZFRootActivity;
  AddTimeList(FRootActivityData);
  if Model <> nil then
  begin
    FRootActivityData.OnInvalidate :=
      (Model as TCustomModel).InvalidateUzfMf6RootActivity;
  end;

  FGwtStatusList := TModflowTimeLists.Create;
  FSpecifiedConcList := TModflowTimeLists.Create;
  FInfiltrationConcList := TModflowTimeLists.Create;
  FEvapConcList := TModflowTimeLists.Create;

  PhastModel := frmGoPhast.PhastModel;
  if PhastModel.GwtUsed then
  begin
    for SpeciesIndex := 0 to PhastModel.MobileComponents.Count - 1 do
    begin
      ConcTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
      ConcTimeList.NonParamDescription := PhastModel.MobileComponents[SpeciesIndex].Name + ' UZT Status';
      ConcTimeList.ParamDescription :=  ConcTimeList.NonParamDescription;
      if Model <> nil then
      begin
        LocalModel := Model as TCustomModel;
//        ConcTimeList.OnInvalidate := LocalModel.InvalidateMfWellConc;
      end;
      AddTimeList(ConcTimeList);
      FGwtStatusList.Add(ConcTimeList);

      ConcTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
      ConcTimeList.NonParamDescription := PhastModel.MobileComponents[SpeciesIndex].Name + ' UZT Specified Concentration';
      ConcTimeList.ParamDescription :=  ConcTimeList.NonParamDescription;
      if Model <> nil then
      begin
        LocalModel := Model as TCustomModel;
//        ConcTimeList.OnInvalidate := LocalModel.InvalidateMfWellConc;
      end;
      AddTimeList(ConcTimeList);
      FSpecifiedConcList.Add(ConcTimeList);

      ConcTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
      ConcTimeList.NonParamDescription := PhastModel.MobileComponents[SpeciesIndex].Name + ' UZT Infiltration Concentration';
      ConcTimeList.ParamDescription :=  ConcTimeList.NonParamDescription;
      if Model <> nil then
      begin
        LocalModel := Model as TCustomModel;
//        ConcTimeList.OnInvalidate := LocalModel.InvalidateMfWellConc;
      end;
      AddTimeList(ConcTimeList);
      FInfiltrationConcList.Add(ConcTimeList);

      ConcTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
      ConcTimeList.NonParamDescription := PhastModel.MobileComponents[SpeciesIndex].Name + ' UZT Evaporation Concentration';
      ConcTimeList.ParamDescription :=  ConcTimeList.NonParamDescription;
      if Model <> nil then
      begin
        LocalModel := Model as TCustomModel;
//        ConcTimeList.OnInvalidate := LocalModel.InvalidateMfWellConc;
      end;
      AddTimeList(ConcTimeList);
      FEvapConcList.Add(ConcTimeList);
    end;
  end;

end;

destructor TUzfMf6TimeListLink.Destroy;
begin
  FGwtStatusList.Free;
  FSpecifiedConcList.Free;
  FInfiltrationConcList.Free;
  FEvapConcList.Free;

  FInfiltrationData.Free;
  FPotentialETData.Free;
  FExtinctionDepthData.Free;
  FExtinctionWaterContentData.Free;
  FAirEntryPotentialData.Free;
  FRootPotentialData.Free;
  FRootActivityData.Free;
  inherited;
end;

{ TUzfMf6_Cell }

procedure TUzfMf6_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  FValues.Cache(Comp, Strings);
  WriteCompInt(Comp, FStressPeriod);
end;

function TUzfMf6_Cell.GeInfiltrationConcentrations: TGwtCellData;
begin
  result := FValues.InfiltrationConcentrations
end;

function TUzfMf6_Cell.GetAirEntryPotential: double;
begin
  result := FValues.AirEntryPotential;
end;

function TUzfMf6_Cell.GetAirEntryPotentialAnnotation: string;
begin
  result := FValues.AirEntryPotentialAnnotation;
end;

function TUzfMf6_Cell.GetAirEntryPotentialPest: string;
begin
  result := FValues.AirEntryPotentialPest;
end;

function TUzfMf6_Cell.GetAirEntryPotentialPestSeriesMethod: TPestParamMethod;
begin
  result := FValues.AirEntryPotentialPestSeriesMethod;
end;

function TUzfMf6_Cell.GetAirEntryPotentialPestSeriesName: string;
begin
  result := FValues.AirEntryPotentialPestSeriesName;
end;

function TUzfMf6_Cell.GetAirEntryPotentialTimeSeriesName: string;
begin
  result := FValues.AirEntryPotentialTimeSeriesName;
end;

function TUzfMf6_Cell.GetColumn: integer;
begin
  result := FValues.Cell.Column;
end;

function TUzfMf6_Cell.GetEvapConcentrations: TGwtCellData;
begin
  result := FValues.EvapConcentrations
end;

function TUzfMf6_Cell.GetExtinctionDepth: double;
begin
  result := FValues.ExtinctionDepth;
end;

function TUzfMf6_Cell.GetExtinctionDepthAnnotation: string;
begin
  result := FValues.ExtinctionDepthAnnotation;
end;

function TUzfMf6_Cell.GetExtinctionDepthPest: string;
begin
  result := FValues.ExtinctionDepthPest;
end;

function TUzfMf6_Cell.GetExtinctionDepthPestSeriesMethod: TPestParamMethod;
begin
  result := FValues.ExtinctionDepthPestSeriesMethod;
end;

function TUzfMf6_Cell.GetExtinctionDepthPestSeriesName: string;
begin
  result := FValues.ExtinctionDepthPestSeriesName;
end;

function TUzfMf6_Cell.GetExtinctionDepthTimeSeriesName: string;
begin
  result := FValues.ExtinctionDepthTimeSeriesName;
end;

function TUzfMf6_Cell.GetExtinctionWaterContent: double;
begin
  result := FValues.ExtinctionWaterContent;
end;

function TUzfMf6_Cell.GetExtinctionWaterContentAnnotation: string;
begin
  result := FValues.ExtinctionDepthAnnotation;
end;

function TUzfMf6_Cell.GetExtinctionWaterContentPest: string;
begin
  result := FValues.ExtinctionWaterContentPest;
end;

function TUzfMf6_Cell.GetExtinctionWaterContentPestSeriesMethod: TPestParamMethod;
begin
  result := FValues.ExtinctionWaterContentPestSeriesMethod;
end;

function TUzfMf6_Cell.GetExtinctionWaterContentPestSeriesName: string;
begin
  result := FValues.ExtinctionWaterContentPestSeriesName;
end;

function TUzfMf6_Cell.GetExtinctionWaterContentTimeSeriesName: string;
begin
  result := FValues.ExtinctionWaterContentTimeSeriesName;
end;

function TUzfMf6_Cell.GetGwtStatus: TGwtBoundaryStatusArray;
begin
  result := FValues.GwtStatus
end;

function TUzfMf6_Cell.GetInfiltration: double;
begin
  result := FValues.Infiltration;
end;

function TUzfMf6_Cell.GetInfiltrationAnnotation: string;
begin
  result := FValues.InfiltrationAnnotation;
end;

function TUzfMf6_Cell.GetInfiltrationPest: string;
begin
  result := FValues.InfiltrationPest;
end;

function TUzfMf6_Cell.GetInfiltrationPestSeriesMethod: TPestParamMethod;
begin
  result := FValues.InfiltrationPestSeriesMethod;
end;

function TUzfMf6_Cell.GetInfiltrationPestSeriesName: string;
begin
  result := FValues.InfiltrationPestSeriesName;
end;

function TUzfMf6_Cell.GetInfiltrationTimeSeriesName: string;
begin
  result := FValues.InfiltrationTimeSeriesName;
end;

function TUzfMf6_Cell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TUzfMf6_Cell.GetIntegerValue(Index: integer;
  AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TUzfMf6_Cell.GetLayer: integer;
begin
  result := FValues.Cell.Layer;
end;

function TUzfMf6_Cell.GetMf6TimeSeriesName(Index: Integer): string;
var
  GwtPosition: Integer;
  GwtSource: Integer;
  SpeciesIndex: Integer;
begin
  case Index of
    UzfMf6InfiltrationPosition: result := InfiltrationTimeSeriesName;
    UzfMf6PotentialETPosition: result := PotentialETTimeSeriesName;
    UzfMf6ExtinctionDepthPosition: result := ExtinctionDepthTimeSeriesName;
    UzfMf6ExtinctionWaterContentPosition: result := ExtinctionWaterContentTimeSeriesName;
    UzfMf6AirEntryPotentialPosition: result := AirEntryPotentialTimeSeriesName;
    UzfMf6RootPotentialPosition: result := RootPotentialTimeSeriesName;
    UzfMf6RootActivityPosition: result := RootActivityTimeSeriesName;
    else
      begin
        GwtPosition := Index - UzfBoundaryGwtStart;
        Assert(GwtPosition >= 0);
        GwtSource := GwtPosition mod UztGwtConcCount;
        SpeciesIndex := GwtPosition div UztGwtConcCount;
        case GwtSource of
          UzfGwtSpecifiedConcentrationPosition:
            begin
              result := FValues.SpecifiedConcentrations.ValueTimeSeriesNames[SpeciesIndex];
            end;
          UzfGwtInfiltrationConcentrationsPosition:
            begin
              result := FValues.InfiltrationConcentrations.ValueTimeSeriesNames[SpeciesIndex];
            end;
          UzfGwtEvapConcentrationsPosition:
            begin
              result := FValues.EvapConcentrations.ValueTimeSeriesNames[SpeciesIndex];
            end;
          else
            begin
              Index := Index - UzfBoundaryGwtStart;
              GwtPosition := Index;
              Assert(GwtPosition >= 0);
              GwtSource := GwtPosition mod UztGwtConcCount;
              SpeciesIndex := GwtPosition div UztGwtConcCount;
              case GwtSource of
                UzfGwtSpecifiedConcentrationPosition:
                  begin
                    result := FValues.SpecifiedConcentrations.ValueTimeSeriesNames[SpeciesIndex];
                  end;
                UzfGwtInfiltrationConcentrationsPosition:
                  begin
                    result := FValues.InfiltrationConcentrations.ValueTimeSeriesNames[SpeciesIndex];
                  end;
                UzfGwtEvapConcentrationsPosition:
                  begin
                    result := FValues.EvapConcentrations.ValueTimeSeriesNames[SpeciesIndex];
                  end;
                else
                  Assert(False);
              end;
            end
        end;
      end;
  end;
end;

function TUzfMf6_Cell.GetMvrIndex: Integer;
begin
  result := FValues.MvrIndex;
end;

function TUzfMf6_Cell.GetMvrUsed: Boolean;
begin
  result := FValues.MvrUsed;
end;

function TUzfMf6_Cell.GetPestName(Index: Integer): string;
var
  GwtPosition: Integer;
  GwtSource: Integer;
  SpeciesIndex: Integer;
begin
  case Index of
    UzfMf6InfiltrationPosition: result := InfiltrationPest;
    UzfMf6PotentialETPosition: result := PotentialETPest;
    UzfMf6ExtinctionDepthPosition: result := ExtinctionDepthPest;
    UzfMf6ExtinctionWaterContentPosition: result := ExtinctionWaterContentPest;
    UzfMf6AirEntryPotentialPosition: result := AirEntryPotentialPest;
    UzfMf6RootPotentialPosition: result := RootPotentialPest;
    UzfMf6RootActivityPosition: result := RootActivityPest;
    else
      begin
        GwtPosition := Index - UzfBoundaryGwtStart;
        Assert(GwtPosition >= 0);
        GwtSource := GwtPosition mod UztGwtConcCount;
        SpeciesIndex := GwtPosition div UztGwtConcCount;
        case GwtSource of
          UzfGwtSpecifiedConcentrationPosition:
            begin
              result := FValues.SpecifiedConcentrations.ValuePestNames[SpeciesIndex];
            end;
          UzfGwtInfiltrationConcentrationsPosition:
            begin
              result := FValues.InfiltrationConcentrations.ValuePestNames[SpeciesIndex];
            end;
          UzfGwtEvapConcentrationsPosition:
            begin
              result := FValues.EvapConcentrations.ValuePestNames[SpeciesIndex];
            end;
          else
            Assert(False);
        end;
      end;
  end;
end;

function TUzfMf6_Cell.GetPestSeriesMethod(Index: Integer): TPestParamMethod;
var
  GwtPosition: Integer;
  GwtSource: Integer;
  SpeciesIndex: Integer;
begin
  case Index of
    UzfMf6InfiltrationPosition: result := InfiltrationPestSeriesMethod;
    UzfMf6PotentialETPosition: result := PotentialETPestSeriesMethod;
    UzfMf6ExtinctionDepthPosition: result := ExtinctionDepthPestSeriesMethod;
    UzfMf6ExtinctionWaterContentPosition: result := ExtinctionWaterContentPestSeriesMethod;
    UzfMf6AirEntryPotentialPosition: result := AirEntryPotentialPestSeriesMethod;
    UzfMf6RootPotentialPosition: result := RootPotentialPestSeriesMethod;
    UzfMf6RootActivityPosition: result := RootActivityPestSeriesMethod;
    else
      begin
        GwtPosition := Index - UzfBoundaryGwtStart;
        Assert(GwtPosition >= 0);
        GwtSource := GwtPosition mod UztGwtConcCount;
        SpeciesIndex := GwtPosition div UztGwtConcCount;
        case GwtSource of
          UzfGwtSpecifiedConcentrationPosition:
            begin
              result := FValues.SpecifiedConcentrations.ValuePestSeriesMethods[SpeciesIndex];
            end;
          UzfGwtInfiltrationConcentrationsPosition:
            begin
              result := FValues.InfiltrationConcentrations.ValuePestSeriesMethods[SpeciesIndex];
            end;
          UzfGwtEvapConcentrationsPosition:
            begin
              result := FValues.EvapConcentrations.ValuePestSeriesMethods[SpeciesIndex];
            end;
          else
            begin
              Result := inherited;
              Assert(False);
            end;
        end;
      end;
  end;
end;

function TUzfMf6_Cell.GetPestSeriesName(Index: Integer): string;
var
  GwtPosition: Integer;
  GwtSource: Integer;
  SpeciesIndex: Integer;
begin
  case Index of
    UzfMf6InfiltrationPosition: result := InfiltrationPestSeriesName;
    UzfMf6PotentialETPosition: result := PotentialETPestSeriesName;
    UzfMf6ExtinctionDepthPosition: result := ExtinctionDepthPestSeriesName;
    UzfMf6ExtinctionWaterContentPosition: result := ExtinctionWaterContentPestSeriesName;
    UzfMf6AirEntryPotentialPosition: result := AirEntryPotentialPestSeriesName;
    UzfMf6RootPotentialPosition: result := RootPotentialPestSeriesName;
    UzfMf6RootActivityPosition: result := RootActivityPestSeriesName;
    else
      begin
        GwtPosition := Index - UzfBoundaryGwtStart;
        Assert(GwtPosition >= 0);
        GwtSource := GwtPosition mod UztGwtConcCount;
        SpeciesIndex := GwtPosition div UztGwtConcCount;
        case GwtSource of
          UzfGwtSpecifiedConcentrationPosition:
            begin
              result := FValues.SpecifiedConcentrations.ValuePestSeriesNames[SpeciesIndex];
            end;
          UzfGwtInfiltrationConcentrationsPosition:
            begin
              result := FValues.InfiltrationConcentrations.ValuePestSeriesNames[SpeciesIndex];
            end;
          UzfGwtEvapConcentrationsPosition:
            begin
              result := FValues.EvapConcentrations.ValuePestSeriesNames[SpeciesIndex];
            end;
          else
            Assert(False);
        end;
      end;
  end;
end;

function TUzfMf6_Cell.GetPotentialET: double;
begin
  result := FValues.PotentialET;
end;

function TUzfMf6_Cell.GetPotentialETAnnotation: string;
begin
  result := FValues.PotentialETAnnotation;
end;

function TUzfMf6_Cell.GetPotentialETPest: string;
begin
  result := FValues.PotentialETPest;
end;

function TUzfMf6_Cell.GetPotentialETPestSeriesMethod: TPestParamMethod;
begin
  result := FValues.PotentialETPestSeriesMethod;
end;

function TUzfMf6_Cell.GetPotentialETPestSeriesName: string;
begin
  result := FValues.PotentialETPestSeriesName;
end;

function TUzfMf6_Cell.GetPotentialETTimeSeriesName: string;
begin
  result := FValues.PotentialETTimeSeriesName;
end;

function TUzfMf6_Cell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
var
  GwtPosition: Integer;
  GwtSource: Integer;
  SpeciesIndex: Integer;
begin
  result := '';
  case Index of
    UzfMf6InfiltrationPosition: result := InfiltrationAnnotation;
    UzfMf6PotentialETPosition: result := PotentialETAnnotation;
    UzfMf6ExtinctionDepthPosition: result := ExtinctionDepthAnnotation;
    UzfMf6ExtinctionWaterContentPosition: result := ExtinctionWaterContentAnnotation;
    UzfMf6AirEntryPotentialPosition: result := AirEntryPotentialAnnotation;
    UzfMf6RootPotentialPosition: result := RootPotentialAnnotation;
    UzfMf6RootActivityPosition: result := RootActivityAnnotation;
    else
      begin
        GwtPosition := Index - UzfBoundaryGwtStart;
        Assert(GwtPosition >= 0);
        GwtSource := GwtPosition mod UztGwtConcCount;
        SpeciesIndex := GwtPosition div UztGwtConcCount;
        case GwtSource of
          UzfGwtSpecifiedConcentrationPosition:
            begin
              result := FValues.SpecifiedConcentrations.ValueAnnotations[SpeciesIndex];
            end;
          UzfGwtInfiltrationConcentrationsPosition:
            begin
              result := FValues.InfiltrationConcentrations.ValueAnnotations[SpeciesIndex];
            end;
          UzfGwtEvapConcentrationsPosition:
            begin
              result := FValues.EvapConcentrations.ValueAnnotations[SpeciesIndex];
            end;
          else
            Assert(False);
        end;
      end;
  end;
end;

function TUzfMf6_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
var
  GwtPosition: Integer;
  GwtSource: Integer;
  SpeciesIndex: Integer;
begin
  result := 0;
  case Index of
    UzfMf6InfiltrationPosition: result := Infiltration;
    UzfMf6PotentialETPosition: result := PotentialET;
    UzfMf6ExtinctionDepthPosition: result := ExtinctionDepth;
    UzfMf6ExtinctionWaterContentPosition: result := ExtinctionWaterContent;
    UzfMf6AirEntryPotentialPosition: result := AirEntryPotential;
    UzfMf6RootPotentialPosition: result := RootPotential;
    UzfMf6RootActivityPosition: result := RootActivity;
    else
      begin
        GwtPosition := Index - UzfBoundaryGwtStart;
        Assert(GwtPosition >= 0);
        GwtSource := GwtPosition mod UztGwtConcCount;
        SpeciesIndex := GwtPosition div UztGwtConcCount;
        case GwtSource of
          UzfGwtSpecifiedConcentrationPosition:
            begin
              result := FValues.SpecifiedConcentrations.Values[SpeciesIndex];
            end;
          UzfGwtInfiltrationConcentrationsPosition:
            begin
              result := FValues.InfiltrationConcentrations.Values[SpeciesIndex];
            end;
          UzfGwtEvapConcentrationsPosition:
            begin
              result := FValues.EvapConcentrations.Values[SpeciesIndex];
            end;
          else
            begin
              Index := Index - UzfBoundaryGwtStart;
              GwtPosition := Index;
              Assert(GwtPosition >= 0);
              GwtSource := GwtPosition mod UztGwtConcCount;
              SpeciesIndex := GwtPosition div UztGwtConcCount;
              case GwtSource of
                UzfGwtSpecifiedConcentrationPosition:
                  begin
                    result := FValues.SpecifiedConcentrations.Values[SpeciesIndex];
                  end;
                UzfGwtInfiltrationConcentrationsPosition:
                  begin
                    result := FValues.InfiltrationConcentrations.Values[SpeciesIndex];
                  end;
                UzfGwtEvapConcentrationsPosition:
                  begin
                    result := FValues.EvapConcentrations.Values[SpeciesIndex];
                  end;
                else
                  Assert(False);
              end;
            end
        end;
      end
  end;
end;

function TUzfMf6_Cell.GetRootActivity: double;
begin
  result := FValues.RootActivity;
end;

function TUzfMf6_Cell.GetRootActivityAnnotation: string;
begin
  result := FValues.RootActivityAnnotation;
end;

function TUzfMf6_Cell.GetRootActivityPest: string;
begin
  result := FValues.RootActivityPest;
end;

function TUzfMf6_Cell.GetRootActivityPestSeriesMethod: TPestParamMethod;
begin
  result := FValues.RootActivityPestSeriesMethod;
end;

function TUzfMf6_Cell.GetRootActivityPestSeriesName: string;
begin
  result := FValues.RootActivityPestSeriesName;
end;

function TUzfMf6_Cell.GetRootActivityTimeSeriesName: string;
begin
  result := FValues.RootActivityTimeSeriesName;
end;

function TUzfMf6_Cell.GetRootPotential: double;
begin
  result := FValues.RootPotential;
end;

function TUzfMf6_Cell.GetRootPotentialAnnotation: string;
begin
  result := FValues.RootPotentialAnnotation;
end;

function TUzfMf6_Cell.GetRootPotentialPest: string;
begin
  result := FValues.RootPotentialPest;
end;

function TUzfMf6_Cell.GetRootPotentialPestSeriesMethod: TPestParamMethod;
begin
  result := FValues.RootPotentialPestSeriesMethod;
end;

function TUzfMf6_Cell.GetRootPotentialPestSeriesName: string;
begin
  result := FValues.RootPotentialPestSeriesName;
end;

function TUzfMf6_Cell.GetRootPotentialTimeSeriesName: string;
begin
  result := FValues.RootPotentialTimeSeriesName;
end;

function TUzfMf6_Cell.GetRow: integer;
begin
  result := FValues.Cell.Row;
end;

function TUzfMf6_Cell.GetSection: integer;
begin
  result := FValues.Cell.Section;
end;

function TUzfMf6_Cell.GetSpecifiedConcentrations: TGwtCellData;
begin
  result := FValues.SpecifiedConcentrations
end;

function TUzfMf6_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  Uzf_Cell: TUzfMf6_Cell;
begin
  result := AnotherCell is TUzfMf6_Cell;
  if result then
  begin
    Uzf_Cell := TUzfMf6_Cell(AnotherCell);
    result :=
      (Infiltration = Uzf_Cell.Infiltration)
      and (PotentialET = Uzf_Cell.PotentialET)
      and (ExtinctionDepth = Uzf_Cell.ExtinctionDepth)
      and (ExtinctionWaterContent = Uzf_Cell.ExtinctionWaterContent)
      and (AirEntryPotential = Uzf_Cell.AirEntryPotential)
      and (RootPotential = Uzf_Cell.RootPotential)
      and (RootActivity = Uzf_Cell.RootActivity)
  end;
end;

procedure TUzfMf6_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  FValues.RecordStrings(Strings);
end;

procedure TUzfMf6_Cell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;
  FValues.Restore(Decomp, Annotations);
  FStressPeriod := ReadCompInt(Decomp);
end;

procedure TUzfMf6_Cell.SetAirEntryPotentialTimeSeriesName(const Value: string);
begin
  FValues.AirEntryPotentialTimeSeriesName := Value;
end;

procedure TUzfMf6_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TUzfMf6_Cell.SetExtinctionDepthTimeSeriesName(const Value: string);
begin
  FValues.ExtinctionDepthTimeSeriesName := Value;
end;

procedure TUzfMf6_Cell.SetExtinctionWaterContentTimeSeriesName(
  const Value: string);
begin
  FValues.ExtinctionWaterContentTimeSeriesName := Value;
end;

procedure TUzfMf6_Cell.SetInfiltrationTimeSeriesName(const Value: string);
begin
  FValues.InfiltrationTimeSeriesName := Value;
end;

procedure TUzfMf6_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TUzfMf6_Cell.SetMf6TimeSeriesName(Index: Integer;
  const Value: string);
var
  GwtPosition: Integer;
  GwtSource: Integer;
  SpeciesIndex: Integer;
begin
  case Index of
    UzfMf6InfiltrationPosition:
      InfiltrationTimeSeriesName := Value;
    UzfMf6PotentialETPosition:
      PotentialETTimeSeriesName := Value;
    UzfMf6ExtinctionDepthPosition:
      ExtinctionDepthTimeSeriesName := Value;
    UzfMf6ExtinctionWaterContentPosition:
      ExtinctionWaterContentTimeSeriesName := Value;
    UzfMf6AirEntryPotentialPosition:
      AirEntryPotentialTimeSeriesName := Value;
    UzfMf6RootPotentialPosition:
      RootPotentialTimeSeriesName := Value;
    UzfMf6RootActivityPosition:
      RootActivityTimeSeriesName := Value;
    else
      begin
        GwtPosition := Index - UzfBoundaryGwtStart;
        Assert(GwtPosition >= 0);
        GwtSource := GwtPosition mod UztGwtConcCount;
        SpeciesIndex := GwtPosition div UztGwtConcCount;
        case GwtSource of
          UzfGwtSpecifiedConcentrationPosition:
            begin
              FValues.SpecifiedConcentrations.ValueTimeSeriesNames[SpeciesIndex] := Value;
            end;
          UzfGwtInfiltrationConcentrationsPosition:
            begin
              FValues.InfiltrationConcentrations.ValueTimeSeriesNames[SpeciesIndex] := Value;
            end;
          UzfGwtEvapConcentrationsPosition:
            begin
              FValues.EvapConcentrations.ValueTimeSeriesNames[SpeciesIndex] := Value;
            end;
          else
            Assert(False);
        end;
      end;
  end;
end;

procedure TUzfMf6_Cell.SetPotentialETTimeSeriesName(const Value: string);
begin
  FValues.PotentialETTimeSeriesName := Value;
end;

procedure TUzfMf6_Cell.SetRootActivityTimeSeriesName(const Value: string);
begin
  FValues.RootActivityTimeSeriesName := Value;
end;

procedure TUzfMf6_Cell.SetRootPotentialTimeSeriesName(const Value: string);
begin
  FValues.RootPotentialTimeSeriesName := Value;
end;

procedure TUzfMf6_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TUzfMf6Boundary }

procedure TUzfMf6Boundary.Assign(Source: TPersistent);
var
  Uzf6Source: TUzfMf6Boundary;
  UzfSource: TUzfBoundary;
  LocalModel: TCustomModel;
begin
  if Source is TUzfMf6Boundary then
  begin
    Uzf6Source := TUzfMf6Boundary(Source);
    SurfaceDepressionDepth := Uzf6Source.SurfaceDepressionDepth;
    VerticalSaturatedK := Uzf6Source.VerticalSaturatedK;
    ResidualWaterContent := Uzf6Source.ResidualWaterContent;
    SaturatedWaterContent := Uzf6Source.SaturatedWaterContent;
    InitialWaterContent := Uzf6Source.InitialWaterContent;
    BrooksCoreyEpsilon := Uzf6Source.BrooksCoreyEpsilon;

    PestInfiltrationFormula := Uzf6Source.PestInfiltrationFormula;
    PestPotentialETFormula := Uzf6Source.PestPotentialETFormula;
    PestExtinctionDepthFormula := Uzf6Source.PestExtinctionDepthFormula;
    PestExtinctionWaterContentFormula := Uzf6Source.PestExtinctionWaterContentFormula;
    PestAirEntryPotentialFormula := Uzf6Source.PestAirEntryPotentialFormula;
    PestRootPotentialFormula := Uzf6Source.PestRootPotentialFormula;
    PestRootActivityFormula := Uzf6Source.PestRootActivityFormula;

    PestInfiltrationMethod := Uzf6Source.PestInfiltrationMethod;
    PestPotentialETMethod := Uzf6Source.PestPotentialETMethod;
    PestExtinctionDepthMethod := Uzf6Source.PestExtinctionDepthMethod;
    PestExtinctionWaterContentMethod := Uzf6Source.PestExtinctionWaterContentMethod;
    PestAirEntryPotentialMethod := Uzf6Source.PestAirEntryPotentialMethod;
    PestRootPotentialMethod := Uzf6Source.PestRootPotentialMethod;
    PestRootActivityMethod := Uzf6Source.PestRootActivityMethod;
    // GWT
    StartingConcentrations := Uzf6Source.StartingConcentrations;

    PestSpecifiedConcentrations := Uzf6Source.PestSpecifiedConcentrations;
    PestSpecifiedConcentrationMethods := Uzf6Source.PestSpecifiedConcentrationMethods;
    PestInfiltrationConcentrations := Uzf6Source.PestInfiltrationConcentrations;
    PestInfiltrationConcentrationMethods := Uzf6Source.PestInfiltrationConcentrationMethods;
    PestEvaporationConcentrations := Uzf6Source.PestEvaporationConcentrations;
    PestEvaporationConcentrationMethods := Uzf6Source.PestEvaporationConcentrationMethods;


  end
  else if Source is TUzfBoundary then
  begin
    UzfSource := TUzfBoundary(Source);
    LocalModel := UzfSource.ParentModel as TCustomModel;
    Assert(LocalModel <> nil);

    SurfaceDepressionDepth := FortranFloatToStr(LocalModel.ModflowPackages.
      UzfPackage.DepthOfUndulations);

    if LocalModel.DataArrayManager.GetDataSetByName(StrUzfVerticalK) <> nil then
    begin
      VerticalSaturatedK := StrUzfVerticalK;
    end
    else
    begin
      VerticalSaturatedK := rsKz;
    end;
    if LocalModel.DataArrayManager.GetDataSetByName(StrUzfReisidualWaterContent) <> nil then
    begin
      ResidualWaterContent := StrUzfReisidualWaterContent;
    end
    else
    begin
      ResidualWaterContent := '0.2';
    end;
    if LocalModel.DataArrayManager.GetDataSetByName(StrUzfSaturatedWaterContent) <> nil then
    begin
      SaturatedWaterContent := StrUzfSaturatedWaterContent;
    end
    else
    begin
      SaturatedWaterContent := '0.3';
    end;
    if LocalModel.DataArrayManager.GetDataSetByName(StrUzfInitialUnsaturatedWaterContent) <> nil then
    begin
      InitialWaterContent := StrUzfInitialUnsaturatedWaterContent;
    end
    else
    begin
      InitialWaterContent := '0.3';
    end;
    if LocalModel.DataArrayManager.GetDataSetByName(StrUzfBrooksCoreyEpsilon) <> nil then
    begin
      BrooksCoreyEpsilon := StrUzfBrooksCoreyEpsilon;
    end
    else
    begin
      BrooksCoreyEpsilon := '3.5';
    end;

    PestInfiltrationFormula := UzfSource.PestInfiltrationRateFormula;
    PestInfiltrationMethod := UzfSource.PestInfiltrationRateMethod;
    PestPotentialETFormula := UzfSource.PestETDemandFormula;
    PestPotentialETMethod := UzfSource.PestETDemandMethod;
    PestExtinctionDepthFormula := UzfSource.PestExtinctionDepthFormula;
    PestExtinctionDepthMethod := UzfSource.PestExtinctionDepthMethod;
    PestExtinctionWaterContentFormula := UzfSource.PestWaterContentFormula;
    PestExtinctionWaterContentMethod := UzfSource.PestWaterContentMethod;
  end;

  inherited;
  if Source is TUzfBoundary then
  begin
    UzfSource := TUzfBoundary(Source);
    if UzfSource.EvapotranspirationDemand.Count > 0 then
    begin
      Values.Assign(UzfSource.EvapotranspirationDemand);
    end;
    if UzfSource.ExtinctionDepth.Count > 0 then
    begin
      Values.Assign(UzfSource.ExtinctionDepth);
    end;
    if UzfSource.WaterContent.Count > 0 then
    begin
      Values.Assign(UzfSource.WaterContent);
    end;

    Loaded;
  end;

end;

procedure TUzfMf6Boundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TUzfMf6_Cell;
  BoundaryValues: TUzfMf6Record;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TUzfMf6Storage;
  LocalModel: TCustomModel;
  MvrUsed: Boolean;
  LocalScreenObject: TScreenObject;
  IDOMAINDataArray: TDataArray;
  BIndex: Integer;
begin
  LocalModel := AModel as TCustomModel;
  IDOMAINDataArray := LocalModel.DataArrayManager.GetDataSetByName(K_IDOMAIN);
  LocalBoundaryStorage := BoundaryStorage as TUzfMf6Storage;
  LocalScreenObject := ScreenObject as TScreenObject;
  MvrUsed := (LocalScreenObject.ModflowMvr <> nil)
    and LocalScreenObject.ModflowMvr.Used
    and (LocalScreenObject.ModflowMvr.SourcePackageChoice = spcUzf);
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TUzfMf6_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >=
      LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <=
      LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.UzfMf6Array) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.UzfMf6Array)
      end;
//      Cells.CheckRestore;
      BIndex := 0;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.UzfMf6Array) - 1 do
      begin
//        Cells.Cached := False;
        BoundaryValues := LocalBoundaryStorage.UzfMf6Array[BoundaryIndex];
        if (IDOMAINDataArray.IntegerData[
          BoundaryValues.Cell.Layer, BoundaryValues.Cell.Row,
          BoundaryValues.Cell.Column] > 0) then
        begin
          BoundaryValues.MvrUsed := MvrUsed;
          BoundaryValues.MvrIndex := BIndex;
          Cell := TUzfMf6_Cell.Create;
          Cells.Add(Cell);
          Cell.FStressPeriod := TimeIndex;
          Cell.FValues := BoundaryValues;
          Cell.ScreenObject := ScreenObject;
          LocalModel.AdjustCellPosition(Cell);
          Inc(BIndex);
        end;
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TUzfMf6Boundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TUzfMf6Collection;
end;

function TUzfMf6Boundary.BoundaryObserverPrefix: string;
begin
  result := 'UzfBoundary_';
end;

constructor TUzfMf6Boundary.Create(Model: TBaseModel; ScreenObject: TObject);
var
  InvalidateEvent: TNotifyEvent;
begin
  inherited;
  if Model = nil then
  begin
    InvalidateEvent := nil;
  end
  else
  begin
    InvalidateEvent := Model.Invalidate;
  end;
  FStartingConcentrations := TStringConcCollection.Create(Model, ScreenObject, nil);
  FPestSpecifiedConcentrationObservers := TObserverList.Create;
  FPestInfiltrationConcentrationObservers := TObserverList.Create;
  FPestEvaporationConcentrationObservers := TObserverList.Create;

  FPestSpecifiedConcentrations := TUztGwtConcCollection.Create(Model, ScreenObject, nil);
  FPestSpecifiedConcentrations.UsedForPestSeries := True;
  FPestInfiltrationConcentrations := TUztGwtConcCollection.Create(Model, ScreenObject, nil);
  FPestInfiltrationConcentrations.UsedForPestSeries := True;
  FPestEvaporationConcentrations := TUztGwtConcCollection.Create(Model, ScreenObject, nil);
  FPestEvaporationConcentrations.UsedForPestSeries := True;

  FPestSpecifiedConcentrationMethods := TGwtPestMethodCollection.Create(Model);
  FPestInfiltrationConcentrationMethods := TGwtPestMethodCollection.Create(Model);
  FPestEvaporationConcentrationMethods := TGwtPestMethodCollection.Create(Model);

  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

  SurfaceDepressionDepth := '1';
  VerticalSaturatedK := '0';
  ResidualWaterContent := '0';
  SaturatedWaterContent := '0';
  InitialWaterContent := '0';
  BrooksCoreyEpsilon := '3.5';

  PestInfiltrationFormula := '';
  PestPotentialETFormula := '';
  PestExtinctionDepthFormula := '';
  PestExtinctionWaterContentFormula := '';
  PestAirEntryPotentialFormula := '';
  PestRootPotentialFormula := '';
  PestRootActivityFormula := '';

  PestInfiltrationMethod := DefaultBoundaryMethod(InfiltrationPosition);
  PestPotentialETMethod := DefaultBoundaryMethod(PotentialETPosition);
  PestExtinctionDepthMethod := DefaultBoundaryMethod(ExtinctionDepthPosition);
  PestExtinctionWaterContentMethod := DefaultBoundaryMethod(ExtinctionWaterContentPosition);
  PestAirEntryPotentialMethod := DefaultBoundaryMethod(AirEntryPotentialPosition);
  PestRootPotentialMethod := DefaultBoundaryMethod(RootPotentialPosition);
  PestRootActivityMethod := DefaultBoundaryMethod(RootActivityPosition);
end;

procedure TUzfMf6Boundary.CreateFormulaObjects;
var
  LocalModel: TCustomModel;
  ConcIndex: Integer;
begin
  inherited;
  FSurfaceDepressionDepth := CreateFormulaObjectBlocks(dsoTop);
  FVerticalSaturatedK := CreateFormulaObjectBlocks(dsoTop);
  FResidualWaterContent := CreateFormulaObjectBlocks(dsoTop);
  FSaturatedWaterContent := CreateFormulaObjectBlocks(dsoTop);
  FInitialWaterContent := CreateFormulaObjectBlocks(dsoTop);
  FBrooksCoreyEpsilon := CreateFormulaObjectBlocks(dsoTop);

  FInfiltration := CreateFormulaObjectBlocks(dsoTop);
  FPotentialET := CreateFormulaObjectBlocks(dsoTop);
  FExtinctionDepth := CreateFormulaObjectBlocks(dsoTop);
  FExtinctionWaterContent := CreateFormulaObjectBlocks(dsoTop);
  FAirEntryPotential := CreateFormulaObjectBlocks(dsoTop);
  FRootPotential := CreateFormulaObjectBlocks(dsoTop);
  FRootActivity := CreateFormulaObjectBlocks(dsoTop);

  LocalModel := ParentModel as TCustomModel;
  if (LocalModel <> nil) and LocalModel.GwtUsed then
  begin
    for ConcIndex := 0 to LocalModel.MobileComponents.Count - 1 do
    begin
      FPestSpecifiedConcentrations.Add;
    end;
    for ConcIndex := 0 to LocalModel.MobileComponents.Count - 1 do
    begin
      FPestInfiltrationConcentrations.Add;
    end;
    for ConcIndex := 0 to LocalModel.MobileComponents.Count - 1 do
    begin
      FPestEvaporationConcentrations.Add;
    end;
  end;

end;

procedure TUzfMf6Boundary.CreateObservers;
var
  Index: Integer;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(SurfaceDepressionDepthObserver);
    FObserverList.Add(VerticalSaturatedKObserver);
    FObserverList.Add(ResidualWaterContentObserver);
    FObserverList.Add(SaturatedWaterContentObserver);
    FObserverList.Add(InitialWaterContentObserver);
    FObserverList.Add(BrooksCoreyEpsilonObserver);

    FObserverList.Add(PestInfiltrationObserver);
    FObserverList.Add(PestPotentialETObserver);
    FObserverList.Add(PestExtinctionDepthObserver);
    FObserverList.Add(PestExtinctionWaterContentObserver);
    FObserverList.Add(PestAirEntryPotentialObserver);
    FObserverList.Add(PestRootPotentialObserver);
    FObserverList.Add(PestRootActivityObserver);

    for Index := 0 to FPestSpecifiedConcentrations.Count - 1 do
    begin
      FObserverList.Add(PestSpecifiedConcentrationObserver[Index]);
    end;
    for Index := 0 to FPestInfiltrationConcentrations.Count - 1 do
    begin
      FObserverList.Add(PestInfiltrationConcentrationObserver[Index]);
    end;
    for Index := 0 to FPestEvaporationConcentrations.Count - 1 do
    begin
      FObserverList.Add(PestEvaporationConcentrationObserver[Index]);
    end;
  end;
end;

class function TUzfMf6Boundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    InfiltrationPosition:
      begin
        result := ppmMultiply;
      end;
    PotentialETPosition:
      begin
        result := ppmMultiply;
      end;
    ExtinctionDepthPosition:
      begin
        result := ppmMultiply;
      end;
    ExtinctionWaterContentPosition:
      begin
        result := ppmMultiply;
      end;
    AirEntryPotentialPosition:
      begin
        result := ppmMultiply;
      end;
    RootPotentialPosition:
      begin
        result := ppmMultiply;
      end;
    RootActivityPosition:
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

destructor TUzfMf6Boundary.Destroy;
begin
  SurfaceDepressionDepth := '1';
  VerticalSaturatedK := '0';
  ResidualWaterContent := '0';
  SaturatedWaterContent := '0';
  InitialWaterContent := '0';
  BrooksCoreyEpsilon := '3.5';

  PestInfiltrationFormula := '';
  PestPotentialETFormula := '';
  PestExtinctionDepthFormula := '';
  PestExtinctionWaterContentFormula := '';
  PestAirEntryPotentialFormula := '';
  PestRootPotentialFormula := '';
  PestRootActivityFormula := '';

  FStartingConcentrations.Free;

  FPestSpecifiedConcentrationMethods.Free;
  FPestInfiltrationConcentrationMethods.Free;
  FPestEvaporationConcentrationMethods.Free;

  FPestSpecifiedConcentrations.Free;
  FPestInfiltrationConcentrations.Free;
  FPestEvaporationConcentrations.Free;

  FPestSpecifiedConcentrationObservers.Free;
  FPestInfiltrationConcentrationObservers.Free;
  FPestEvaporationConcentrationObservers.Free;
  inherited;
end;

function TUzfMf6Boundary.GetBrooksCoreyEpsilon: string;
begin
  Result := FBrooksCoreyEpsilon.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(BrooksCoreyEpsilonPosition);
  end;
end;

function TUzfMf6Boundary.GetBrooksCoreyEpsilonObserver: TObserver;
var
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FBrooksCoreyEpsilonObserver = nil then
  begin
    if ParentModel <> nil then
    begin
      Model := ParentModel as TPhastModel;
      DataArray := Model.DataArrayManager.GetDataSetByName(StrUzfMf6BrooksCoreyEpsilon);
    end
    else
    begin
      DataArray := nil;
    end;
    CreateObserver('UZF6_BrooksCoreyEpsilon_', FBrooksCoreyEpsilonObserver, DataArray);
  end;
  result := FBrooksCoreyEpsilonObserver;
end;

procedure TUzfMf6Boundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
var
  ValueIndex: Integer;
  BoundaryStorage: TUzfMf6Storage;
begin
  EvaluateArrayBoundaries(AModel, Writer);
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TUzfMf6Storage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
end;

function TUzfMf6Boundary.GetInitialWaterContent: string;
begin
  Result := FInitialWaterContent.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(InitialWaterContentPosition);
  end;
end;

function TUzfMf6Boundary.GetInitialWaterContentObserver: TObserver;
var
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FInitialWaterContentObserver = nil then
  begin
    if ParentModel <> nil then
    begin
      Model := ParentModel as TPhastModel;
      DataArray := Model.DataArrayManager.GetDataSetByName(StrUzfMf6InitialUnsaturatedWaterContent);
    end
    else
    begin
      DataArray := nil;
    end;
    CreateObserver('UZF6_Initial_Water_Content_', FInitialWaterContentObserver, DataArray);
  end;
  result := FInitialWaterContentObserver;
end;

function TUzfMf6Boundary.GetPestAirEntryPotentialFormula: string;
begin
  Result := FAirEntryPotential.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(AirEntryPotentialPosition);
  end;
end;

function TUzfMf6Boundary.GetPestAirEntryPotentialObserver: TObserver;
begin
  if FPestAirEntryPotentialObserver = nil then
  begin
    CreateObserver('PestAirEntryPotential_', FPestAirEntryPotentialObserver, nil);
    FPestAirEntryPotentialObserver.OnUpToDateSet := InvalidateAirEntryPotentialData;
  end;
  result := FPestAirEntryPotentialObserver;
end;

function TUzfMf6Boundary.GetPestBoundaryFormula(FormulaIndex: integer): string;
var
  ChemSpeciesCount: Integer;
begin
  case FormulaIndex of
    InfiltrationPosition:
      begin
        result := PestInfiltrationFormula
      end;
    PotentialETPosition:
      begin
        result := PestPotentialETFormula
      end;
    ExtinctionDepthPosition:
      begin
        result := PestExtinctionDepthFormula
      end;
    ExtinctionWaterContentPosition:
      begin
        result := PestExtinctionWaterContentFormula
      end;
    AirEntryPotentialPosition:
      begin
        result := PestAirEntryPotentialFormula
      end;
    RootPotentialPosition:
      begin
        result := PestRootPotentialFormula
      end;
    RootActivityPosition:
      begin
        result := PestRootActivityFormula
      end;
    else
      begin
        FormulaIndex := FormulaIndex-UzfBoundaryGwtStart;
        ChemSpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;

        while PestSpecifiedConcentrations.Count < ChemSpeciesCount do
        begin
          PestSpecifiedConcentrations.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          result := PestSpecifiedConcentrations[FormulaIndex].Value;
          Exit;
        end;

        FormulaIndex := FormulaIndex-ChemSpeciesCount;
        while PestInfiltrationConcentrations.Count < ChemSpeciesCount do
        begin
          PestInfiltrationConcentrations.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          result := PestInfiltrationConcentrations[FormulaIndex].Value;
          Exit;
        end;

        FormulaIndex := FormulaIndex-ChemSpeciesCount;
        while PestEvaporationConcentrations.Count < ChemSpeciesCount do
        begin
          PestEvaporationConcentrations.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          result := PestEvaporationConcentrations[FormulaIndex].Value;
          Exit;
        end;

        result := inherited;
        Assert(False);
      end;

  end;
end;

function TUzfMf6Boundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
var
  ChemSpeciesCount: Integer;
begin
  case FormulaIndex of
    InfiltrationPosition:
      begin
        result := PestInfiltrationMethod
      end;
    PotentialETPosition:
      begin
        result := PestPotentialETMethod
      end;
    ExtinctionDepthPosition:
      begin
        result := PestExtinctionDepthMethod
      end;
    ExtinctionWaterContentPosition:
      begin
        result := PestExtinctionWaterContentMethod
      end;
    AirEntryPotentialPosition:
      begin
        result := PestAirEntryPotentialMethod
      end;
    RootPotentialPosition:
      begin
        result := PestRootPotentialMethod
      end;
    RootActivityPosition:
      begin
        result := PestRootActivityMethod
      end;
    else
      begin
        FormulaIndex := FormulaIndex-UzfBoundaryGwtStart;
        ChemSpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;

        while PestSpecifiedConcentrationMethods.Count < ChemSpeciesCount do
        begin
          PestSpecifiedConcentrationMethods.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          result := PestSpecifiedConcentrationMethods[FormulaIndex].PestParamMethod;
          Exit;
        end;

        FormulaIndex := FormulaIndex-ChemSpeciesCount;
        while PestInfiltrationConcentrationMethods.Count < ChemSpeciesCount do
        begin
          PestInfiltrationConcentrationMethods.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          result := PestInfiltrationConcentrationMethods[FormulaIndex].PestParamMethod;
          Exit;
        end;

        FormulaIndex := FormulaIndex-ChemSpeciesCount;
        while PestEvaporationConcentrationMethods.Count < ChemSpeciesCount do
        begin
          PestEvaporationConcentrationMethods.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          result := PestEvaporationConcentrationMethods[FormulaIndex].PestParamMethod;
          Exit;
        end;

        result := inherited;
        Assert(False);
      end;
  end;
end;

function TUzfMf6Boundary.GetPestEvaporationConcentrationObserver(
  const Index: Integer): TObserver;
var
  AObserver: TObserver;
begin
  while Index >= FPestEvaporationConcentrationObservers.Count do
  begin
    CreateObserver(Format('UzfPestEvapConc_%d', [Index+1]), AObserver, nil);
    FPestEvaporationConcentrationObservers.Add(AObserver);
    AObserver.OnUpToDateSet := InvalidatePestEvapConcData;
  end;
  result := FPestEvaporationConcentrationObservers[Index];
end;

function TUzfMf6Boundary.GetPestExtinctionDepthFormula: string;
begin
  Result := FExtinctionDepth.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(ExtinctionDepthPosition);
  end;
end;

function TUzfMf6Boundary.GetPestExtinctionDepthObserver: TObserver;
begin
  if FPestExtinctionDepthObserver = nil then
  begin
    CreateObserver('PestExtinctionDepth_', FPestExtinctionDepthObserver, nil);
    FPestExtinctionDepthObserver.OnUpToDateSet := InvalidateExtinctionDepthData;
  end;
  result := FPestExtinctionDepthObserver;
end;

function TUzfMf6Boundary.GetPestExtinctionWaterContentFormula: string;
begin
  Result := FExtinctionWaterContent.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(ExtinctionWaterContentPosition);
  end;
end;

function TUzfMf6Boundary.GetPestExtinctionWaterContentObserver: TObserver;
begin
  if FPestExtinctionWaterContentObserver = nil then
  begin
    CreateObserver('PestExtinctionWaterContent_', FPestExtinctionWaterContentObserver, nil);
    FPestExtinctionWaterContentObserver.OnUpToDateSet := InvalidateExtinctionWaterContentData;
  end;
  result := FPestExtinctionWaterContentObserver;
end;

function TUzfMf6Boundary.GetPestInfiltrationFormula: string;
begin
  Result := FInfiltration.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(InfiltrationPosition);
  end;
end;

function TUzfMf6Boundary.GetPestInfiltrationObserver: TObserver;
begin
  if FPestInfiltrationObserver = nil then
  begin
    CreateObserver('PestInfiltration_', FPestInfiltrationObserver, nil);
    FPestInfiltrationObserver.OnUpToDateSet := InvalidateInfiltrationData;
  end;
  result := FPestInfiltrationObserver;
end;

function TUzfMf6Boundary.GetPestPotentialETFormula: string;
begin
  Result := FPotentialET.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(PotentialETPosition);
  end;
end;

function TUzfMf6Boundary.GetPestPotentialETObserver: TObserver;
begin
  if FPestPotentialETObserver = nil then
  begin
    CreateObserver('PestPotentialET_', FPestPotentialETObserver, nil);
    FPestPotentialETObserver.OnUpToDateSet := InvalidatePotentialETData;
  end;
  result := FPestPotentialETObserver;
end;

function TUzfMf6Boundary.GetPestInfiltrationConcentrationObserver(
  const Index: Integer): TObserver;
var
  AObserver: TObserver;
begin
  while Index >= FPestInfiltrationConcentrationObservers.Count do
  begin
    CreateObserver(Format('UzfPestInfiltrationConc_%d', [Index+1]), AObserver, nil);
    FPestInfiltrationConcentrationObservers.Add(AObserver);
    AObserver.OnUpToDateSet := InvalidatePestInfiltrationConcData;
  end;
  result := FPestInfiltrationConcentrationObservers[Index];
end;

function TUzfMf6Boundary.GetPestRootActivityFormula: string;
begin
  Result := FRootActivity.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(RootActivityPosition);
  end;
end;

function TUzfMf6Boundary.GetPestRootActivityObserver: TObserver;
begin
  if FPestRootActivityObserver = nil then
  begin
    CreateObserver('PestRootActivity_', FPestRootActivityObserver, nil);
    FPestRootActivityObserver.OnUpToDateSet := InvalidateRootActivityData;
  end;
  result := FPestRootActivityObserver;
end;

function TUzfMf6Boundary.GetPestRootPotentialFormula: string;
begin
  Result := FRootPotential.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(RootPotentialPosition);
  end;
end;

function TUzfMf6Boundary.GetPestRootPotentialObserver: TObserver;
begin
  if FPestRootPotentialObserver = nil then
  begin
    CreateObserver('PestRootPotential_', FPestRootPotentialObserver, nil);
    FPestRootPotentialObserver.OnUpToDateSet := InvalidateRootPotentialData;
  end;
  result := FPestRootPotentialObserver;
end;

function TUzfMf6Boundary.GetPestSpecifiedConcentrationObserver(
  const Index: Integer): TObserver;
var
  AObserver: TObserver;
begin
  while Index >= FPestSpecifiedConcentrationObservers.Count do
  begin
    CreateObserver(Format('UzfPestSpecConc_%d', [Index+1]), AObserver, nil);
    FPestSpecifiedConcentrationObservers.Add(AObserver);
    AObserver.OnUpToDateSet := InvalidatePestSpecConcData;
  end;
  result := FPestSpecifiedConcentrationObservers[Index];
end;

procedure TUzfMf6Boundary.GetPropertyObserver(Sender: TObject; List: TList);
var
  StartIndex: Integer;
  Index: Integer;
begin
  if Sender = FSurfaceDepressionDepth then
  begin
    List.Add(FObserverList[SurfaceDepressionDepthPosition]);
  end
  else if Sender = FVerticalSaturatedK then
  begin
    List.Add(FObserverList[VerticalSaturatedKPosition]);
  end
  else if Sender = FResidualWaterContent then
  begin
    List.Add(FObserverList[ResidualWaterContentPosition]);
  end
  else if Sender = FSaturatedWaterContent then
  begin
    List.Add(FObserverList[SaturatedWaterContentPosition]);
  end
  else if Sender = FInitialWaterContentObserver then
  begin
    List.Add(FObserverList[InitialWaterContentPosition]);
  end
  else if Sender = FBrooksCoreyEpsilon then
  begin
    List.Add(FObserverList[BrooksCoreyEpsilonPosition]);
  end

  else if Sender = FInfiltration then
  begin
    List.Add(FObserverList[InfiltrationPosition]);
  end
  else if Sender = FPotentialET then
  begin
    List.Add(FObserverList[PotentialETPosition]);
  end
  else if Sender = FExtinctionDepth then
  begin
    List.Add(FObserverList[ExtinctionDepthPosition]);
  end
  else if Sender = FExtinctionWaterContent then
  begin
    List.Add(FObserverList[ExtinctionWaterContentPosition]);
  end
  else if Sender = FAirEntryPotential then
  begin
    List.Add(FObserverList[AirEntryPotentialPosition]);
  end
  else if Sender = FRootPotential then
  begin
    List.Add(FObserverList[RootPotentialPosition]);
  end
  else if Sender = FRootActivity then
  begin
    List.Add(FObserverList[RootActivityPosition]);
  end;

  StartIndex := UzfBoundaryGwtStart;
  for Index := 0 to FPestSpecifiedConcentrations.Count - 1 do
  begin
    if FPestSpecifiedConcentrations[Index].ValueObject = Sender then
    begin
      List.Add(FObserverList[StartIndex + Index]);
    end;
  end;

  StartIndex := StartIndex + FPestSpecifiedConcentrations.Count;
  for Index := 0 to PestInfiltrationConcentrations.Count - 1 do
  begin
    if PestInfiltrationConcentrations[Index].ValueObject = Sender then
    begin
      List.Add(FObserverList[StartIndex + Index]);
    end;
  end;

  StartIndex := StartIndex + PestInfiltrationConcentrations.Count;
  for Index := 0 to PestEvaporationConcentrations.Count - 1 do
  begin
    if PestEvaporationConcentrations[Index].ValueObject = Sender then
    begin
      List.Add(FObserverList[StartIndex + Index]);
    end;
  end;
end;

function TUzfMf6Boundary.GetResidualWaterContent: string;
begin
  Result := FResidualWaterContent.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(ResidualWaterContentPosition);
  end;
end;

function TUzfMf6Boundary.GetResidualWaterContentObserver: TObserver;
var
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FResidualWaterContentObserver = nil then
  begin
    if ParentModel <> nil then
    begin
      Model := ParentModel as TPhastModel;
      DataArray := Model.DataArrayManager.GetDataSetByName(StrUzfMf6ReisidualWaterContent);
    end
    else
    begin
      DataArray := nil;
    end;
    CreateObserver('UZF6_Residual_Content_', FResidualWaterContentObserver, DataArray);
  end;
  result := FResidualWaterContentObserver;
end;

function TUzfMf6Boundary.GetSaturatedWaterContent: string;
begin
  Result := FSaturatedWaterContent.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(SaturatedWaterContentPosition);
  end;
end;

function TUzfMf6Boundary.GetSaturatedWaterContentObserver: TObserver;
var
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FSaturatedWaterContentObserver = nil then
  begin
    if ParentModel <> nil then
    begin
      Model := ParentModel as TPhastModel;
      DataArray := Model.DataArrayManager.GetDataSetByName(StrUzfMf6SaturatedWaterContent);
    end
    else
    begin
      DataArray := nil;
    end;
    CreateObserver('UZF6_Saturated_Water_Content_', FSaturatedWaterContentObserver, DataArray);
  end;
  result := FSaturatedWaterContentObserver;
end;

function TUzfMf6Boundary.GetSurfaceDepressionDepth: string;
begin
  Result := FSurfaceDepressionDepth.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(SurfaceDepressionDepthPosition);
  end;
end;

function TUzfMf6Boundary.GetSurfaceDepressionDepthObserver: TObserver;
var
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FSurfaceDepressionDepthObserver = nil then
  begin
    if ParentModel <> nil then
    begin
      Model := ParentModel as TPhastModel;
      DataArray := Model.DataArrayManager.GetDataSetByName(StrUzfMf6SurfaceDepressionDepth);
    end
    else
    begin
      DataArray := nil;
    end;
    CreateObserver('UZF6_Surface_Depression_Depth_', FSurfaceDepressionDepthObserver, DataArray);
  end;
  result := FSurfaceDepressionDepthObserver;
end;

function TUzfMf6Boundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('PestUZF_Used_', FUsedObserver, nil);
  end;
  result := FUsedObserver;
end;

function TUzfMf6Boundary.GetVerticalSaturatedK: string;
begin
  Result := FVerticalSaturatedK.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(VerticalSaturatedKPosition);
  end;
end;

function TUzfMf6Boundary.GetVerticalSaturatedKObserver: TObserver;
var
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FVerticalSaturatedKObserver = nil then
  begin
    if ParentModel <> nil then
    begin
      Model := ParentModel as TPhastModel;
      DataArray := Model.DataArrayManager.GetDataSetByName(StrUzfMf6VerticalSaturatedK);
    end
    else
    begin
      DataArray := nil;
    end;
    CreateObserver('UZF6_Kz_', FVerticalSaturatedKObserver, DataArray);
  end;
  result := FVerticalSaturatedKObserver;
end;

procedure TUzfMf6Boundary.HandleChangedValue(Observer: TObserver);
begin
//  inherited;
  InvalidateDisplay;
end;

procedure TUzfMf6Boundary.InvalidateAirEntryPotentialData(Sender: TObject);
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
    PhastModel.InvalidateUzfMf6AirEntryPotential(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateUzfMf6AirEntryPotential(self);
    end;
  end;
end;

procedure TUzfMf6Boundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TPhastModel;
    Model.InvalidateUzfMf6Infiltration(self);
    Model.InvalidateUzfMf6PotentialET(self);
    Model.InvalidateUzfMf6ExtinctionDepth(self);
    Model.InvalidateUzfMf6ExtinctionWaterContent(self);
    Model.InvalidateUzfMf6AirEntryPotential(self);
    Model.InvalidateUzfMf6RootPotential(self);
    Model.InvalidateUzfMf6RootActivity(self);
  end;
end;

procedure TUzfMf6Boundary.InvalidateExtinctionDepthData(Sender: TObject);
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
    PhastModel.InvalidateUzfMf6ExtinctionDepth(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateUzfMf6ExtinctionDepth(self);
    end;
  end;
end;

procedure TUzfMf6Boundary.InvalidateExtinctionWaterContentData(Sender: TObject);
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
    PhastModel.InvalidateUzfMf6ExtinctionWaterContent(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateUzfMf6ExtinctionWaterContent(self);
    end;
  end;
end;

procedure TUzfMf6Boundary.InvalidateInfiltrationData(Sender: TObject);
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
    PhastModel.InvalidateUzfMf6Infiltration(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateUzfMf6Infiltration(self);
    end;
  end;
end;

procedure TUzfMf6Boundary.InvalidatePestEvapConcData(Sender: TObject);
begin
  { TODO -cGWT : This needs to be implemented }
end;

procedure TUzfMf6Boundary.InvalidatePestInfiltrationConcData(Sender: TObject);
begin
  { TODO -cGWT : This needs to be implemented }
end;

procedure TUzfMf6Boundary.InvalidatePestSpecConcData(Sender: TObject);
begin
  { TODO -cGWT : This needs to be implemented }
end;

procedure TUzfMf6Boundary.InvalidatePotentialETData(Sender: TObject);
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
    PhastModel.InvalidateUzfMf6PotentialET(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateUzfMf6PotentialET(self);
    end;
  end;
end;

procedure TUzfMf6Boundary.InvalidateRootActivityData(Sender: TObject);
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
    PhastModel.InvalidateUzfMf6RootActivity(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateUzfMf6RootActivity(self);
    end;
  end;
end;

procedure TUzfMf6Boundary.InvalidateRootPotentialData(Sender: TObject);
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
    PhastModel.InvalidateUzfMf6RootPotential(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateUzfMf6RootPotential(self);
    end;
  end;
end;

procedure TUzfMf6Boundary.Loaded;
var
  Brooks_Corey_Epsilon: TDataArray;
  Initial_Unsaturated_Water_Content: TDataArray;
  Residual_Water_Content: TDataArray;
  Saturated_Water_Content: TDataArray;
  Surface_Depression_Depth: TDataArray;
  Vertical_Saturated_K: TDataArray;
  AScreenObject: TScreenObject;
  DataSetIndex: Integer;
begin
  Brooks_Corey_Epsilon := frmGoPhast.PhastModel.DataArrayManager.
    GetDataSetByName(StrUzfMf6BrooksCoreyEpsilon);
  Initial_Unsaturated_Water_Content := frmGoPhast.PhastModel.DataArrayManager.
    GetDataSetByName(StrUzfMf6InitialUnsaturatedWaterContent);
  Residual_Water_Content := frmGoPhast.PhastModel.DataArrayManager.
    GetDataSetByName(StrUzfMf6ReisidualWaterContent);
  Saturated_Water_Content := frmGoPhast.PhastModel.DataArrayManager.
    GetDataSetByName(StrUzfMf6SaturatedWaterContent);
  Surface_Depression_Depth := frmGoPhast.PhastModel.DataArrayManager.
    GetDataSetByName(StrUzfMf6SurfaceDepressionDepth);
  Vertical_Saturated_K := frmGoPhast.PhastModel.DataArrayManager.
    GetDataSetByName(StrUzfMf6VerticalSaturatedK);

  AScreenObject := ScreenObject as TScreenObject;

  if Brooks_Corey_Epsilon <> nil then
  begin
    DataSetIndex := AScreenObject.AddDataSet(Brooks_Corey_Epsilon);
    AScreenObject.DataSetFormulas[DataSetIndex] := BrooksCoreyEpsilon;
  end;

  if Initial_Unsaturated_Water_Content <> nil then
  begin
    DataSetIndex := AScreenObject.AddDataSet(Initial_Unsaturated_Water_Content);
    AScreenObject.DataSetFormulas[DataSetIndex] := InitialWaterContent;
  end;

  if Residual_Water_Content <> nil then
  begin
    DataSetIndex := AScreenObject.AddDataSet(Residual_Water_Content);
    AScreenObject.DataSetFormulas[DataSetIndex] := ResidualWaterContent;
  end;

  if Saturated_Water_Content <> nil then
  begin
    DataSetIndex := AScreenObject.AddDataSet(Saturated_Water_Content);
    AScreenObject.DataSetFormulas[DataSetIndex] := SaturatedWaterContent;
  end;

  if Surface_Depression_Depth <> nil then
  begin
    DataSetIndex := AScreenObject.AddDataSet(Surface_Depression_Depth);
    AScreenObject.DataSetFormulas[DataSetIndex] := SurfaceDepressionDepth;
  end;

  if Vertical_Saturated_K <> nil then
  begin
    DataSetIndex := AScreenObject.AddDataSet(Vertical_Saturated_K);
    AScreenObject.DataSetFormulas[DataSetIndex] := VerticalSaturatedK;
  end;

end;

procedure TUzfMf6Boundary.SetBrooksCoreyEpsilon(const Value: string);
begin
  UpdateFormulaBlocks(Value, BrooksCoreyEpsilonPosition, FBrooksCoreyEpsilon);
end;

procedure TUzfMf6Boundary.SetInitialWaterContent(const Value: string);
begin
  UpdateFormulaBlocks(Value, InitialWaterContentPosition, FInitialWaterContent);
end;

procedure TUzfMf6Boundary.SetPestAirEntryPotentialFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, AirEntryPotentialPosition, FAirEntryPotential);
end;

procedure TUzfMf6Boundary.SetPestAirEntryPotentialMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestAirEntryPotentialMethod, Value);
end;

procedure TUzfMf6Boundary.SetPestBoundaryFormula(FormulaIndex: integer;
  const Value: string);
var
  ChemSpeciesCount: Integer;
begin
  case FormulaIndex of
    InfiltrationPosition:
      begin
        PestInfiltrationFormula := Value;
      end;
    PotentialETPosition:
      begin
        PestPotentialETFormula := Value;
      end;
    ExtinctionDepthPosition:
      begin
        PestExtinctionDepthFormula := Value;
      end;
    ExtinctionWaterContentPosition:
      begin
        PestExtinctionWaterContentFormula := Value;
      end;
    AirEntryPotentialPosition:
      begin
        PestAirEntryPotentialFormula := Value;
      end;
    RootPotentialPosition:
      begin
        PestRootPotentialFormula := Value;
      end;
    RootActivityPosition:
      begin
        PestRootActivityFormula := Value;
      end;
    else
      begin
        FormulaIndex := FormulaIndex-UzfBoundaryGwtStart;
        ChemSpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;

        while PestSpecifiedConcentrations.Count < ChemSpeciesCount do
        begin
          PestSpecifiedConcentrations.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          PestSpecifiedConcentrations[FormulaIndex].Value := Value;
          Exit;
        end;

        FormulaIndex := FormulaIndex-ChemSpeciesCount;
        while PestInfiltrationConcentrations.Count < ChemSpeciesCount do
        begin
          PestInfiltrationConcentrations.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          PestInfiltrationConcentrations[FormulaIndex].Value := Value;
          Exit;
        end;

        FormulaIndex := FormulaIndex-ChemSpeciesCount;
        while PestEvaporationConcentrations.Count < ChemSpeciesCount do
        begin
          PestEvaporationConcentrations.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          PestEvaporationConcentrations[FormulaIndex].Value := Value;
          Exit;
        end;

        inherited;
        Assert(False);
      end;

  end;
end;

procedure TUzfMf6Boundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
var
  ChemSpeciesCount: Integer;
begin
  case FormulaIndex of
    InfiltrationPosition:
      begin
        PestInfiltrationMethod := Value;
      end;
    PotentialETPosition:
      begin
        PestPotentialETMethod := Value;
      end;
    ExtinctionDepthPosition:
      begin
        PestExtinctionDepthMethod := Value;
      end;
    ExtinctionWaterContentPosition:
      begin
        PestExtinctionWaterContentMethod := Value;
      end;
    AirEntryPotentialPosition:
      begin
        PestAirEntryPotentialMethod := Value;
      end;
    RootPotentialPosition:
      begin
        PestRootPotentialMethod := Value;
      end;
    RootActivityPosition:
      begin
        PestRootActivityMethod := Value;
      end;
    else
      begin
        FormulaIndex := FormulaIndex-UzfBoundaryGwtStart;
        ChemSpeciesCount := frmGoPhast.PhastModel.MobileComponents.Count;

        while PestSpecifiedConcentrationMethods.Count < ChemSpeciesCount do
        begin
          PestSpecifiedConcentrationMethods.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          PestSpecifiedConcentrationMethods[FormulaIndex].PestParamMethod := Value;
          Exit;
        end;

        FormulaIndex := FormulaIndex-ChemSpeciesCount;
        while PestInfiltrationConcentrationMethods.Count < ChemSpeciesCount do
        begin
          PestInfiltrationConcentrationMethods.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          PestInfiltrationConcentrationMethods[FormulaIndex].PestParamMethod := Value;
          Exit;
        end;

        FormulaIndex := FormulaIndex-ChemSpeciesCount;
        while PestEvaporationConcentrationMethods.Count < ChemSpeciesCount do
        begin
          PestEvaporationConcentrationMethods.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          PestEvaporationConcentrationMethods[FormulaIndex].PestParamMethod := Value;
          Exit;
        end;

        inherited;
        Assert(False);
      end;

  end;
end;

procedure TUzfMf6Boundary.SetPestEvaporationConcentrationMethods(
  const Value: TGwtPestMethodCollection);
begin
  FPestEvaporationConcentrationMethods.Assign(Value);
end;

procedure TUzfMf6Boundary.SetPestEvaporationConcentrations(
  const Value: TUztGwtConcCollection);
begin
  FPestEvaporationConcentrations.Assign(Value);
end;

procedure TUzfMf6Boundary.SetPestExtinctionDepthFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, ExtinctionDepthPosition, FExtinctionDepth);
end;

procedure TUzfMf6Boundary.SetPestExtinctionDepthMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestExtinctionDepthMethod, Value);
end;

procedure TUzfMf6Boundary.SetPestExtinctionWaterContentFormula(
  const Value: string);
begin
  UpdateFormulaBlocks(Value, ExtinctionWaterContentPosition, FExtinctionWaterContent);
end;

procedure TUzfMf6Boundary.SetPestExtinctionWaterContentMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestExtinctionWaterContentMethod, Value);
end;

procedure TUzfMf6Boundary.SetPestInfiltrationConcentrationMethods(
  const Value: TGwtPestMethodCollection);
begin
  FPestInfiltrationConcentrationMethods.Assign(Value);
end;

procedure TUzfMf6Boundary.SetPestInfiltrationConcentrations(
  const Value: TUztGwtConcCollection);
begin
  FPestInfiltrationConcentrations.Assign(Value);
end;

procedure TUzfMf6Boundary.SetPestInfiltrationFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, InfiltrationPosition, FInfiltration);
end;

procedure TUzfMf6Boundary.SetPestInfiltrationMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestInfiltrationMethod, Value);
end;

procedure TUzfMf6Boundary.SetPestPotentialETFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, PotentialETPosition, FPotentialET);
end;

procedure TUzfMf6Boundary.SetPestPotentialETMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestPotentialETMethod, Value);
end;

procedure TUzfMf6Boundary.SetPestRootActivityFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, RootActivityPosition, FRootActivity);
end;

procedure TUzfMf6Boundary.SetPestRootActivityMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestRootActivityMethod, Value);
end;

procedure TUzfMf6Boundary.SetPestRootPotentialFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, RootPotentialPosition, FRootPotential);
end;

procedure TUzfMf6Boundary.SetPestRootPotentialMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestRootPotentialMethod, Value);
end;

procedure TUzfMf6Boundary.SetPestSpecifiedConcentrationMethods(
  const Value: TGwtPestMethodCollection);
begin
  FPestSpecifiedConcentrationMethods.Assign(Value);
end;

procedure TUzfMf6Boundary.SetPestSpecifiedConcentrations(
  const Value: TUztGwtConcCollection);
begin
  FPestSpecifiedConcentrations.Assign(Value);
end;

procedure TUzfMf6Boundary.SetResidualWaterContent(const Value: string);
begin
  UpdateFormulaBlocks(Value, ResidualWaterContentPosition, FResidualWaterContent);
end;

procedure TUzfMf6Boundary.SetSaturatedWaterContent(const Value: string);
begin
  UpdateFormulaBlocks(Value, SaturatedWaterContentPosition, FSaturatedWaterContent);
end;

procedure TUzfMf6Boundary.SetStartingConcentrations(
  const Value: TStringConcCollection);
begin
  FStartingConcentrations.Assign(Value);
end;

procedure TUzfMf6Boundary.SetSurfaceDepressionDepth(const Value: string);
begin
  UpdateFormulaBlocks(Value, SurfaceDepressionDepthPosition, FSurfaceDepressionDepth);
end;

procedure TUzfMf6Boundary.SetVerticalSaturatedK(const Value: string);
begin
  UpdateFormulaBlocks(Value, VerticalSaturatedKPosition, FVerticalSaturatedK);
end;

{ TUztGwtConcCollection }

constructor TUztGwtConcCollection.Create(Model: TBaseModel;
  AScreenObject: TObject; ParentCollection: TUzfMf6Collection);
begin
  inherited Create(Model, AScreenObject, ParentCollection);
end;

initialization
  InitializeUzfObsNameList;

finalization
  UzfObsNameList.Free;

end.
