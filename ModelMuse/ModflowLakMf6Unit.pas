unit ModflowLakMf6Unit;

interface

uses
  System.Classes, ModflowBoundaryUnit, GoPhastTypes, OrderedCollectionUnit,
  FormulaManagerUnit, FormulaManagerInterfaceUnit, SubscriptionUnit, System.Generics.Collections,
  ModflowCellUnit, RealListUnit, Mt3dmsChemUnit, System.SysUtils, GwtStatusUnit,
  OrderedCollectionInterfaceUnit;

type
  // related to claktype.
//  Tmf6LakeType = (mltOnlyVertical, mltOnlyHorizontal, mltBoth);

  TLakOb = (loStage, loExternalInflow, loSimOutletInflow, loSumInflow, loFromMvr, loRain,
    loRunoff, loFlowRate, loWithdrawal, loEvap, loExternalOutflow, loToMvr, loStorage,
    loConstantFlow, loOutlet, loVolume, loSurfaceArea, loWettedArea,
    loConductance);
  TLakObs = set of TLakOb;

  TLktOb = (ltoConcentration,
//    ltoFlowJaFacc,
    ltoStorage, ltoConstant, ltoFromMvr, ltoToMvr,
    ltoLKT, ltoRainfall, ltoEvaporation, ltoRunoff, ltoExtInflow, ltoWithdrawal,
    ltoExtOutflow);
  TLktObs = set of TLktOb;


//    loExternalOutflow, loOutlet,

  // related to bedleak
  Tmf6LakLeakanceUsed = (lluNotUsed, lluUsed);
  // related to couttyp
  TLakeOutletType = (lotSpecified, lotManning, lotWeir);
  // related to STATUS
  TLakeStatus = (lsActive, lsInactive, lsConstant);

  TLakeConnectionType = (lctHorizontal, lctVertical);
  TLakeConnectionTypes = set of TLakeConnectionType;

  TMf6LakeConnectionType = (mlctVertical, mlctHorizontal, mlctEmbeddedVertical,
    mlctEmbeddedHorizontal);

  TLakConnectionMf6Record = record
    Cell: TCellLocation;
    ConnectionType: TMf6LakeConnectionType;
  end;

  TLakeOutletTimeItem = class(TCustomModflowBoundaryItem)
  private
    const
      KRatePosition = 0;
      KInvertPosition = 1;
      KRoughnessPosition = 2;
      KWidthPosition = 3;
      KSlopePosition = 4;
    var
    FRate: IFormulaObject;
    FInvert: IFormulaObject;
    FRoughness: IFormulaObject;
    FSlope: IFormulaObject;
    FWidth: IFormulaObject;
    function GetInvert: string;
    function GetRate: string;
    function GetRoughness: string;
    function GetSlope: string;
    function GetWidth: string;
    procedure SetInvert(const Value: string);
    procedure SetRate(const Value: string);
    procedure SetRoughness(const Value: string);
    procedure SetSlope(const Value: string);
    procedure SetWidth(const Value: string);
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    function GetBoundaryFormula(Index: integer): string; override;
    procedure SetBoundaryFormula(Index: integer; const Value: string);
      override;
    function BoundaryFormulaCount: integer; override;
  public
    procedure Assign(Source: TPersistent); override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  published
    property Rate: string read GetRate write SetRate;
    property Invert: string read GetInvert write SetInvert;
    property Roughness: string read GetRoughness write SetRoughness;
    property Width: string read GetWidth write SetWidth;
    property Slope: string read GetSlope write SetSlope;
  end;

  TLakOutletTimeCollection = class(TCustomNonSpatialBoundColl)
  private
    procedure InvalidateRate(Sender: TObject);
    procedure InvalidateInvert(Sender: TObject);
    procedure InvalidateRoughness(Sender: TObject);
    procedure InvalidateWidth(Sender: TObject);
    procedure InvalidateSlope(Sender: TObject);
    function GetItem(Index: Integer): TLakeOutletTimeItem;
    procedure SetItem(Index: Integer; const Value: TLakeOutletTimeItem);
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    property Items[Index: Integer]: TLakeOutletTimeItem read GetItem write SetItem; default;
    function Add: TLakeOutletTimeItem;
  end;

  TLakeOutlet = class(TModflowScreenObjectProperty)
  private
    FOutletType: TLakeOutletType;
    FLakeTimes: TLakOutletTimeCollection;
    FOutletObjectName: string;
    FOutletObject: TObject;
    FOutletIndex: Integer;
//    procedure SetOutlet(const Value: Integer);
    procedure SetOutletType(const Value: TLakeOutletType);
    procedure SetLakeTimes(const Value: TLakOutletTimeCollection);
    function IsSame(AnotherLakeOutlet: TLakeOutlet): boolean;
    function GetOutletObjectName: string;
    procedure SetOutletObjectName(const Value: string);
    procedure SetOutletObject(const Value: TObject);
    procedure UpdateOutletObject;
    function GetOutletObject: TObject;
  protected
    function BoundaryObserverPrefix: string; override;
  public
    Constructor Create(Model: IModelForTOrderedCollection; ScreenObject: TObject);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Used: boolean; override;
    // @name is a @link(TScreenObject). It identifies the lake into which the
    // lake flows.
    property OutletObject: TObject read GetOutletObject write SetOutletObject;
    property OutletIndex: Integer read FOutletIndex write FOutletIndex;
  published
    property OutletType: TLakeOutletType read FOutletType write SetOutletType;
    property LakeTimes: TLakOutletTimeCollection read FLakeTimes write SetLakeTimes;
    property OutletObjectName: string read GetOutletObjectName write SetOutletObjectName;
  end;

  TLakeOutletItem = class(TFormulaOrderedItem)
  private
    FOutlet: TLakeOutlet;
    procedure SetOutlet(const Value: TLakeOutlet);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Loaded;
  published
    property Outlet: TLakeOutlet read FOutlet write SetOutlet;
  end;

  TLakeOutlets = class(TCustomObjectOrderedCollection)
  private
    function GetItems(Index: Integer): TLakeOutletItem;
    procedure SetItems(Index: Integer; const Value: TLakeOutletItem);
    procedure Loaded;
  public
    constructor Create(Model: IModelForTOrderedCollection; ScreenObject: TObject);
    property Items[Index: Integer]: TLakeOutletItem read GetItems write SetItems; default;
    function Add: TLakeOutletItem;
  end;

  TLakeTableItemMf6 = class(TFormulaOrderedItem)
  private
  const
    FormulaCount = 4;
    StagePosition = 0;
    VolumePosition = 1;
    SurfaceAreaPosition = 2;
    ExchangeAreaEvapPosition = 3;
  var
    FStage: IFormulaObject;
    FVolume: IFormulaObject;
    FSurfaceArea: IFormulaObject;
    FExchangeArea: IFormulaObject;
    FObserverList: TObjectList<TObserver>;
    function GetExchangeArea: string;
    function GetStage: string;
    function GetSurfaceArea: string;
    function GetVolume: string;
    procedure SetExchangeArea(const Value: string);
    procedure SetStage(const Value: string);
    procedure SetSurfaceArea(const Value: string);
    procedure SetVolume(const Value: string);
    procedure CreateFormulaObjects;
    function CreateFormulaObject(Orientation:
      TDataSetOrientation): IFormulaObject;
    procedure RemoveFormulaObjects;
    procedure ResetItemObserver(Index: integer);
    procedure StopTalkingToAnyone;
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function GetObserver(Index: Integer): TObserver; override;
    function GetScreenObject: TObject; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    // stage
    property Stage: string read GetStage write SetStage;
    // volume
    property Volume: string read GetVolume write SetVolume;
    // sarea
    property SurfaceArea: string read GetSurfaceArea write SetSurfaceArea;
    // barea
    property ExchangeArea: string read GetExchangeArea write SetExchangeArea;
  end;

  TLakeTableMf6 = class(TEnhancedOrderedCollection)
  private
    function GetItems(Index: Integer): TLakeTableItemMf6;
    procedure SetItems(Index: Integer; const Value: TLakeTableItemMf6);
  public
    constructor Create(Model: IModelForTOrderedCollection);
    property Items[Index: Integer]: TLakeTableItemMf6 read GetItems
      write SetItems; default;
    function Add: TLakeTableItemMf6;
    procedure StopTalkingToAnyone;
  end;

  TLakTimeCollection = class;

  TLktGwtConcCollection = class(TGwtConcStringCollection)
    constructor Create(Model: TBaseModel; AScreenObject: TObject;
      ParentCollection: TLakTimeCollection);
  end;

  TLakeTimeItem = class(TCustomModflowBoundaryItem)
  private
    FStatus: TLakeStatus;
    FStage: IFormulaObject;
    FRainfall: IFormulaObject;
    FRunoff: IFormulaObject;
    FEvaporation: IFormulaObject;
    FInflow: IFormulaObject;
    FWithdrawal: IFormulaObject;
    // GWT
    FGwtStatus: TGwtBoundaryStatusCollection;
    FSpecifiedConcentrations: TLktGwtConcCollection;
    FRainfallConcentrations: TLktGwtConcCollection;
    FEvapConcentrations: TLktGwtConcCollection;
    FRunoffConcentrations: TLktGwtConcCollection;
    FInflowConcentrations: TLktGwtConcCollection;
    FDensity: TLktGwtConcCollection;
    function GetStage: string;
    procedure SetStage(const Value: string);
    function GetRainfall: string;
    procedure SetRainfall(const Value: string);
    procedure SetStatus(const Value: TLakeStatus);
    function GetRunoff: string;
    procedure SetRunoff(const Value: string);
    function GetEvaporation: string;
    procedure SetEvaporation(const Value: string);
    function GetWithdrawal: string;
    procedure SetWithdrawal(const Value: string);
    function GetInflow: string;
    procedure SetInflow(const Value: string);
    procedure SetEvapConcentrations(const Value: TLktGwtConcCollection);
    procedure SetGwtStatus(const Value: TGwtBoundaryStatusCollection);
    procedure SetInflowConcentrations(const Value: TLktGwtConcCollection);
    procedure SetRainfallConcentrations(const Value: TLktGwtConcCollection);
    procedure SetRunoffConcentrations(const Value: TLktGwtConcCollection);
    procedure SetSpecifiedConcentrations(const Value: TLktGwtConcCollection);
    procedure SetDensity(const Value: TLktGwtConcCollection);
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    function GetBoundaryFormula(Index: integer): string; override;
    procedure SetBoundaryFormula(Index: integer; const Value: string);
      override;
    function BoundaryFormulaCount: integer; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  published
    property Status: TLakeStatus read FStatus write SetStatus;
    property Stage: string read GetStage write SetStage;
    property Rainfall: string read GetRainfall write SetRainfall;
    property Evaporation: string read GetEvaporation write SetEvaporation;
    property Runoff: string read GetRunoff write SetRunoff;
    property Inflow: string read GetInflow write SetInflow;
    property Withdrawal: string read GetWithdrawal write SetWithdrawal;
    // Buoyancy
    property Density: TLktGwtConcCollection read FDensity write SetDensity;
    // GWT
    property GwtStatus: TGwtBoundaryStatusCollection read FGwtStatus write SetGwtStatus;
    property SpecifiedConcentrations: TLktGwtConcCollection read FSpecifiedConcentrations
      write SetSpecifiedConcentrations;
    property RainfallConcentrations: TLktGwtConcCollection read FRainfallConcentrations
      write SetRainfallConcentrations;
    property EvapConcentrations: TLktGwtConcCollection read FEvapConcentrations
      write SetEvapConcentrations;
    property RunoffConcentrations: TLktGwtConcCollection read FRunoffConcentrations
      write SetRunoffConcentrations;
    property InflowConcentrations: TLktGwtConcCollection read FInflowConcentrations
      write SetInflowConcentrations;
  end;

  TLakTimeCollection = class(TCustomMF_BoundColl)
  private
    procedure InvalidateStage(Sender: TObject);
    procedure InvalidateRainfall(Sender: TObject);
    procedure InvalidateEvaporation(Sender: TObject);
    procedure InvalidateRunoff(Sender: TObject);
    procedure InvalidateInflow(Sender: TObject);
    procedure InvalidateWithdrawal(Sender: TObject);
    procedure InvalidateDensity(Sender: TObject);
    // GWT
    procedure InvalidateGwtStatus(Sender: TObject);
    procedure InvalidateSpecifiedConcentrations(Sender: TObject);
    procedure InvalidateRainfallConcentrations(Sender: TObject);
    procedure InvalidateEvapConcentrations(Sender: TObject);
    procedure InvalidateRunoffConcentrations(Sender: TObject);
    procedure InvalidateInflowConcentrations(Sender: TObject);
  protected
    class function ItemClass: TBoundaryItemClass; override;
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
  end;

  TLakeMf6 = class(TModflowBoundary)
  private
  const
    BottomElevationPosition = 7;
    TopElevationPosition = 8;
    BedKPosition = 9;
    BedThicknessPosition = 10;
    ConnectionLengthPosition = 11;
    StartingStagePosition = 12;
  var
    FPestRainfallMethod: TPestParamMethod;
    FPestRunoffMethod: TPestParamMethod;
    FPestWithdrawalMethod: TPestParamMethod;
    FPestInflowMethod: TPestParamMethod;
    FPestEvaporationMethod: TPestParamMethod;
    FPestStageMethod: TPestParamMethod;
    FStartingConcentrations: TStringConcCollection;
    FPestRainfallConcentrations: TLktGwtConcCollection;
    FPestRainfallConcentrationMethods: TGwtPestMethodCollection;
    FPestSpecifiedConcentrations: TLktGwtConcCollection;
    FPestRunoffConcentrations: TLktGwtConcCollection;
    FPestSpecifiedConcentrationMethods: TGwtPestMethodCollection;
    FPestInflowConcentrations: TLktGwtConcCollection;
    FPestRunoffConcentrationMethods: TGwtPestMethodCollection;
    FPestEvaporationConcentrations: TLktGwtConcCollection;
    FPestInflowConcentrationMethods: TGwtPestMethodCollection;
    FPestEvaporationConcentrationMethods: TGwtPestMethodCollection;
    FPestDensityMethod: TPestParamMethod;
    function GetStartingConcentrations: TStringConcCollection;
    function GetPestDensityFormula: string;
    procedure SetPestDensityFormula(const Value: string);
    procedure SetPestDensityMethod(const Value: TPestParamMethod);
    function GetPestEvaporationFormula: string;
    function GetPestEvaporationObserver: TObserver;
    function GetPestInflowFormula: string;
    function GetPestInflowObserver: TObserver;
    function GetPestRainfallFormula: string;
    function GetPestRainfallObserver: TObserver;
    function GetPestRunoffFormula: string;
    function GetPestRunoffObserver: TObserver;
    function GetPestStageFormula: string;
    function GetPestStageObserver: TObserver;
    function GetPestWithdrawalFormula: string;
    function GetPestWithdrawalObserver: TObserver;
    procedure SetPestEvaporationFormula(const Value: string);
    procedure SetPestEvaporationMethod(const Value: TPestParamMethod);
    procedure SetPestInflowFormula(const Value: string);
    procedure SetPestInflowMethod(const Value: TPestParamMethod);
    procedure SetPestRainfallFormula(const Value: string);
    procedure SetPestRainfallMethod(const Value: TPestParamMethod);
    procedure SetPestRunoffFormula(const Value: string);
    procedure SetPestRunoffMethod(const Value: TPestParamMethod);
    procedure SetPestStageFormula(const Value: string);
    procedure SetPestStageMethod(const Value: TPestParamMethod);
    procedure SetPestWithdrawalFormula(const Value: string);
    procedure SetPestWithdrawalMethod(const Value: TPestParamMethod);
    procedure SetStartingConcentrations(const Value: TStringConcCollection);
    procedure SetPestEvaporationConcentrationMethods(
      const Value: TGwtPestMethodCollection);
    procedure SetPestEvaporationConcentrations(
      const Value: TLktGwtConcCollection);
    procedure SetPestInflowConcentrationMethods(
      const Value: TGwtPestMethodCollection);
    procedure SetPestInflowConcentrations(const Value: TLktGwtConcCollection);
    procedure SetPestRainfallConcentrationMethods(
      const Value: TGwtPestMethodCollection);
    procedure SetPestRainfallConcentrations(const Value: TLktGwtConcCollection);
    procedure SetPestRunoffConcentrationMethods(
      const Value: TGwtPestMethodCollection);
    procedure SetPestRunoffConcentrations(const Value: TLktGwtConcCollection);
    procedure SetPestSpecifiedConcentrationMethods(
      const Value: TGwtPestMethodCollection);
    procedure SetPestSpecifiedConcentrations(
      const Value: TLktGwtConcCollection);
    function GetPestEvaporationConcentrationObserver(
      const Index: Integer): TObserver;
    function GetPestInflowConcentrationObserver(
      const Index: Integer): TObserver;
    function GetPestRainfallConcentrationObserver(
      const Index: Integer): TObserver;
    function GetPestRunoffConcentrationObserver(
      const Index: Integer): TObserver;
    function GetPestSpecifiedConcentrationObserver(
      const Index: Integer): TObserver;
    procedure InvalidatePestSpecConcData(Sender: TObject);
    procedure InvalidatePestEvapConcData(Sender: TObject);
    procedure InvalidatePestRainfallConcData(Sender: TObject);
    procedure InvalidatePestInflowConcData(Sender: TObject);
    procedure InvalidatePestRunoffConcData(Sender: TObject);
  var
    FOutlets: TLakeOutlets;
    FLakeTable: TLakeTableMf6;
    FLakeConnections: TLakeConnectionTypes;
    FEmbedded: Boolean;
    FBottomElevation: IFormulaObject;
    FTopElevation: IFormulaObject;
    FBedK: IFormulaObject;
    FBedThickness: IFormulaObject;
    FConnectionLength: IFormulaObject;
    FStartingStage: IFormulaObject;
    FBottomElevationObserver: TObserver;
    FTopElevationObserver: TObserver;
    FBedKObserver: TObserver;
    FBedThicknessObserver: TObserver;
    FConnectionLengthObserver: TObserver;
    FConnectionWidthObserver: TObserver;
    FStartingStageObserver: TObserver;
    FPestStageFormula: IFormulaObject;
    FPestEvaporationFormula: IFormulaObject;
    FPestInflowFormula: IFormulaObject;
    FPestWithdrawalFormula: IFormulaObject;
    FPestDensityFormula: IFormulaObject;
    FUsedObserver: TObserver;
    FPestEvaporationObserver: TObserver;
    FPestInflowObserver: TObserver;
    FPestRainfallObserver: TObserver;
    FPestRunoffObserver: TObserver;
    FPestStageObserver: TObserver;
    FPestWithdrawalObserver: TObserver;
    FPestRainfallFormula: IFormulaObject;
    FPestRunoffFormula: IFormulaObject;
    FPestSpecifiedConcentrationObservers: TObserverList;
    FPestRainfallConcentrationObservers: TObserverList;
    FPestEvaporationConcentrationObservers: TObserverList;
    FPestRunoffConcentrationObservers: TObserverList;
    FPestInflowConcentrationObservers: TObserverList;
    procedure SetOutlets(const Value: TLakeOutlets);
    procedure SetLakeTable(const Value: TLakeTableMf6);
    procedure SetEmbedded(const Value: Boolean);
    procedure SetLakeConnections(const Value: TLakeConnectionTypes);
    function GetBedK: string;
    function GetBedThickness: string;
    function GetBottomElevation: string;
    function GetConnectionLength: string;
    function GetTopElevation: string;
    procedure SetBedK(const Value: string);
    procedure SetBedThickness(const Value: string);
    procedure SetBottomElevation(const Value: string);
    procedure SetConnectionLength(const Value: string);
    procedure SetTopElevation(const Value: string);
    function GetBedKObserver: TObserver;
    function GetBedThicknessObserver: TObserver;
    function GetBottomElevationObserver: TObserver;
    function GetConnectionLengthObserver: TObserver;
    function GetConnectionWidthObserver: TObserver;
    function GetTopElevationObserver: TObserver;
    function GetStartingStageObserver: TObserver;
    function GetStartingStage: string;
    procedure SetStartingStage(const Value: string);
  protected
    class function BoundaryCollectionClass: TMF_BoundCollClass;
      override;
    function BoundaryObserverPrefix: string; override;
    procedure CreateFormulaObjects;
    property BottomElevationObserver: TObserver read GetBottomElevationObserver;
    property TopElevationObserver: TObserver read GetTopElevationObserver;
    property BedKObserver: TObserver read GetBedKObserver;
    property BedThicknessObserver: TObserver read GetBedThicknessObserver;
    property ConnectionLengthObserver: TObserver read GetConnectionLengthObserver;
    property ConnectionWidthObserver: TObserver read GetConnectionWidthObserver;
    property StartingStageObserver: TObserver read GetStartingStageObserver;
    procedure CreateObservers;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;

    procedure HandleChangedValue(Observer: TObserver); //override;
    function GetUsedObserver: TObserver; //override;
    function GetPestBoundaryFormula(FormulaIndex: integer): string; override;
    procedure SetPestBoundaryFormula(FormulaIndex: integer;
      const Value: string); override;
    function GetPestBoundaryMethod(FormulaIndex: integer): TPestParamMethod; override;
    procedure SetPestBoundaryMethod(FormulaIndex: integer;
      const Value: TPestParamMethod); override;
    property PestStageObserver: TObserver read GetPestStageObserver;
    property PestRainfallObserver: TObserver read GetPestRainfallObserver;
    property PestRunoffObserver: TObserver read GetPestRunoffObserver;
    property PestEvaporationObserver: TObserver read GetPestEvaporationObserver;
    property PestInflowObserver: TObserver read GetPestInflowObserver;
    property PestWithdrawalObserver: TObserver read GetPestWithdrawalObserver;
    property PestSpecifiedConcentrationObserver[const Index: Integer]: TObserver
      read GetPestSpecifiedConcentrationObserver;
    property PestRainfallConcentrationObserver[const Index: Integer]: TObserver
      read GetPestRainfallConcentrationObserver;
    property PestEvaporationConcentrationObserver[const Index: Integer]: TObserver
      read GetPestEvaporationConcentrationObserver;
    property PestRunoffConcentrationObserver[const Index: Integer]: TObserver
      read GetPestRunoffConcentrationObserver;
    property PestInflowConcentrationObserver[const Index: Integer]: TObserver
      read GetPestInflowConcentrationObserver;
  public
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    procedure Loaded;
    procedure UpdateTimes(Times: TRealList; StartTestTime,
      EndTestTime: double; var StartRangeExtended, EndRangeExtended: boolean;
      AModel: TBaseModel); override;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
  published
    property Outlets: TLakeOutlets read FOutlets write SetOutlets;
    property LakeTable: TLakeTableMf6 read FLakeTable write SetLakeTable;
    property LakeConnections: TLakeConnectionTypes read FLakeConnections
      write SetLakeConnections Stored True;
    property Embedded: Boolean read FEmbedded write SetEmbedded Stored True;
    // belev
    Property BottomElevation: string read GetBottomElevation
      write SetBottomElevation;
    // telev
    Property TopElevation: string read GetTopElevation write SetTopElevation;
    // bedleak
    Property BedK: string read GetBedK write SetBedK;
    // bedleak
    Property BedThickness: string read GetBedThickness write SetBedThickness;
    // connlen
    property ConnectionLength: string read GetConnectionLength
      write SetConnectionLength;
    // strt
    property StartingStage: string read GetStartingStage write SetStartingStage;
    property PestStageFormula: string read GetPestStageFormula
      write SetPestStageFormula;
    property PestStageMethod: TPestParamMethod read FPestStageMethod
      write SetPestStageMethod;
    property PestRainfallFormula: string read GetPestRainfallFormula
      write SetPestRainfallFormula;
    property PestRainfallMethod: TPestParamMethod read FPestRainfallMethod
      write SetPestRainfallMethod;
    property PestRunoffFormula: string read GetPestRunoffFormula
      write SetPestRunoffFormula;
    property PestRunoffMethod: TPestParamMethod read FPestRunoffMethod
      write SetPestRunoffMethod;
    property PestEvaporationFormula: string read GetPestEvaporationFormula
      write SetPestEvaporationFormula;
    property PestEvaporationMethod: TPestParamMethod read FPestEvaporationMethod
      write SetPestEvaporationMethod;
    property PestInflowFormula: string read GetPestInflowFormula
      write SetPestInflowFormula;
    property PestInflowMethod: TPestParamMethod read FPestInflowMethod
      write SetPestInflowMethod;
    property PestWithdrawalFormula: string read GetPestWithdrawalFormula
      write SetPestWithdrawalFormula;
    property PestWithdrawalMethod: TPestParamMethod read FPestWithdrawalMethod
      write SetPestWithdrawalMethod;
    property PestDensityFormula: string read GetPestDensityFormula
      write SetPestDensityFormula;
    property PestDensityMethod: TPestParamMethod read FPestDensityMethod
      write SetPestDensityMethod;
    property StartingConcentrations: TStringConcCollection
      read GetStartingConcentrations
      write SetStartingConcentrations;
    property PestSpecifiedConcentrations: TLktGwtConcCollection
        read FPestSpecifiedConcentrations write SetPestSpecifiedConcentrations;
    property PestSpecifiedConcentrationMethods: TGwtPestMethodCollection
      read FPestSpecifiedConcentrationMethods write SetPestSpecifiedConcentrationMethods;
      property PestRainfallConcentrations: TLktGwtConcCollection
        read FPestRainfallConcentrations write SetPestRainfallConcentrations;
    property PestRainfallConcentrationMethods: TGwtPestMethodCollection
      read FPestRainfallConcentrationMethods write SetPestRainfallConcentrationMethods;
      property PestEvaporationConcentrations: TLktGwtConcCollection
        read FPestEvaporationConcentrations write SetPestEvaporationConcentrations;
    property PestEvaporationConcentrationMethods: TGwtPestMethodCollection
      read FPestEvaporationConcentrationMethods write SetPestEvaporationConcentrationMethods;
      property PestRunoffConcentrations: TLktGwtConcCollection
        read FPestRunoffConcentrations write SetPestRunoffConcentrations;
    property PestRunoffConcentrationMethods: TGwtPestMethodCollection
      read FPestRunoffConcentrationMethods write SetPestRunoffConcentrationMethods;
      property PestInflowConcentrations: TLktGwtConcCollection
        read FPestInflowConcentrations write SetPestInflowConcentrations;
    property PestInflowConcentrationMethods: TGwtPestMethodCollection
      read FPestInflowConcentrationMethods write SetPestInflowConcentrationMethods;
//    property GwtStatus: TGwtBoundaryStatusCollection read FGwtStatus write SetGwtStatus;
  end;

function TryGetLakOb(const LakObName: string; var LakOb: TLakOb): Boolean;
function TryGetLktOb(const LktObName: string; var LktOb: TLktOb): Boolean;
function LakObToString(const LakOb: TLakOb): string;
function LktObToString(const LktOb: TLktOb): string;
Procedure FillLakSeriesNames(AList: TStrings);
Procedure FillLKtSeriesNames(AList: TStrings);

const
  Lak6StagePosition = 0;
  Lak6RainfallPosition = 1;
  Lak6RunoffPosition = 2;
  Lak6EvaporationPosition = 3;
  Lak6InflowPosition = 4;
  Lak6WithdrawalPosition = 5;
  LakeDensityPosition = 6;
  Lak6GwtPestStartPosition = 7;

implementation

uses
  frmGoPhastUnit, ScreenObjectUnit, PhastModelUnit, DataSetUnit,
  DataSetNamesUnit;

const
  LakeObName: array[TLakOb] of string = ('Stage', 'ExternalInflow', 'SimOutletInflow', 'SumInflow', 'FromMvr', 'Rain',
    'Runoff', 'FlowRate', 'Withdrawal', 'Evap', 'ExternalOutflow', 'ToMvr', 'Storage',
    'ConstantFlow', 'Outlet', 'Volume', 'SurfaceArea', 'WettedArea',
    'Conductance');
  LktObName: array[TLktOb] of string =
    (
      'Concentration',
//      'flow-ja-face',
      'Storage',
      'Constant',
      'From-MVR',
      'To-MVR',
      'LKT',
      'Rainfall',
      'Evaporation',
      'Runoff',
      'External inflow',
      'Withdrawal',
      'External outflow'
    );

var
  LakeObNames: TStringList;
  LktObNames: TStringList;

procedure InitializeLakeObNames;
var
  Index: TLakOb;
begin
  LakeObNames := TStringList.Create;
  LakeObNames.CaseSensitive := False;
  for Index := Low(TLakOb) to High(TLakOb) do
  begin
    LakeObNames.Add(LakeObName[Index]);
  end;
end;

procedure InitializeLktObNames;
var
  Index: TLktOb;
begin
  LktObNames := TStringList.Create;
  LktObNames.CaseSensitive := False;
  for Index := Low(TLktOb) to High(TLktOb) do
  begin
    LktObNames.Add(LktObName[Index]);
  end;
end;


function TryGetLakOb(const LakObName: string; var LakOb: TLakOb): Boolean;
var
  Index: Integer;
begin
  Index := LakeObNames.IndexOf(LakObName);
  result := Index >= 0;
  if result then
  begin
    LakOb := TLakOb(Index);
  end;
end;

function TryGetLktOb(const LktObName: string; var LktOb: TLktOb): Boolean;
var
  Index: Integer;
begin
  Index := LktObNames.IndexOf(LktObName);
  result := Index >= 0;
  if result then
  begin
    LktOb := TLktOb(Index);
  end;
end;

Procedure FillLakSeriesNames(AList: TStrings);
begin
  AList.Assign(LakeObNames);
end;

Procedure FillLKtSeriesNames(AList: TStrings);
begin
  AList.Assign(LktObNames);
end;

function LakObToString(const LakOb: TLakOb): string;
begin
  result := LakeObName[LakOb];
end;

function LktObToString(const LktOb: TLktOb): string;
begin
  result := LktObName[LktOb];
end;

{ TLakeTimeItem }

procedure TLakeTimeItem.Assign(Source: TPersistent);
var
  LakeItem: TLakeTimeItem;
begin
  if Source is TLakeTimeItem then
  begin
    LakeItem := TLakeTimeItem(Source);
    Status := LakeItem.Status;
    Stage := LakeItem.Stage;
    Rainfall := LakeItem.Rainfall;
    Evaporation := LakeItem.Evaporation;
    Runoff := LakeItem.Runoff;
    Inflow := LakeItem.Inflow;
    Withdrawal := LakeItem.Withdrawal;
    Density := LakeItem.Density;
    GwtStatus := LakeItem.GwtStatus;
    SpecifiedConcentrations := LakeItem.SpecifiedConcentrations;
    RainfallConcentrations := LakeItem.RainfallConcentrations;
    EvapConcentrations := LakeItem.EvapConcentrations;
    RunoffConcentrations := LakeItem.RunoffConcentrations;
    InflowConcentrations := LakeItem.InflowConcentrations;
  end;
  inherited;
end;

procedure TLakeTimeItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TLakTimeCollection;
  StageObserver: TObserver;
  RainfallObserver: TObserver;
  RunoffObserver: TObserver;
  EvaporationObserver: TObserver;
  WithdrawalObserver: TObserver;
  DensityObserver: TObserver;
  InflowObserver: TObserver;
  ConcIndex: Integer;
  DensityIndex: Integer;
begin
  inherited;
//  inherited;
  ParentCollection := Collection as TLakTimeCollection;

  StageObserver := FObserverList[Lak6StagePosition];
  StageObserver.OnUpToDateSet := ParentCollection.InvalidateStage;

  RainfallObserver := FObserverList[Lak6RainfallPosition];
  RainfallObserver.OnUpToDateSet := ParentCollection.InvalidateRainfall;

  RunoffObserver := FObserverList[Lak6RunoffPosition];
  RunoffObserver.OnUpToDateSet := ParentCollection.InvalidateRunoff;

  EvaporationObserver := FObserverList[Lak6EvaporationPosition];
  EvaporationObserver.OnUpToDateSet := ParentCollection.InvalidateEvaporation;

  InflowObserver := FObserverList[Lak6InflowPosition];
  InflowObserver.OnUpToDateSet := ParentCollection.InvalidateInflow;

  WithdrawalObserver := FObserverList[Lak6WithdrawalPosition];
  WithdrawalObserver.OnUpToDateSet := ParentCollection.InvalidateWithdrawal;

  for DensityIndex := 0 to Density.Count - 1 do
  begin
    Density[DensityIndex].Observer.OnUpToDateSet
      := ParentCollection.InvalidateDensity;
  end;
//  DensityObserver := FObserverList[LakeDensityPosition];
//  DensityObserver.OnUpToDateSet := ParentCollection.InvalidateDensity;

//  for ConcIndex := 0 to GwtStatus.Count - 1 do
//  begin
//    GwtStatus[ConcIndex].Observer.OnUpToDateSet
//      := ParentCollection.InvalidateSpecifiedConcentrations;
//  end;

  for ConcIndex := 0 to SpecifiedConcentrations.Count - 1 do
  begin
    SpecifiedConcentrations[ConcIndex].Observer.OnUpToDateSet
      := ParentCollection.InvalidateSpecifiedConcentrations;
  end;

  for ConcIndex := 0 to RainfallConcentrations.Count - 1 do
  begin
    RainfallConcentrations[ConcIndex].Observer.OnUpToDateSet
      := ParentCollection.InvalidateRainfallConcentrations;
  end;

  for ConcIndex := 0 to EvapConcentrations.Count - 1 do
  begin
    EvapConcentrations[ConcIndex].Observer.OnUpToDateSet
      := ParentCollection.InvalidateEvapConcentrations;
  end;

  for ConcIndex := 0 to RunoffConcentrations.Count - 1 do
  begin
    RunoffConcentrations[ConcIndex].Observer.OnUpToDateSet
      := ParentCollection.InvalidateRunoffConcentrations;
  end;

  for ConcIndex := 0 to InflowConcentrations.Count - 1 do
  begin
    InflowConcentrations[ConcIndex].Observer.OnUpToDateSet
      := ParentCollection.InvalidateInflowConcentrations;
  end;

end;

function TLakeTimeItem.BoundaryFormulaCount: integer;
begin
  result := Succ(LakeDensityPosition);
  if frmGoPhast.PhastModel.GwtUsed then
  begin
    result := result + frmGoPhast.PhastModel.MobileComponents.Count *5;
  end;
end;

constructor TLakeTimeItem.Create(Collection: TCollection);
var
  LakCollection: TLakTimeCollection;
begin
  LakCollection := Collection as TLakTimeCollection;
  // Buoyancy
  FDensity := TLktGwtConcCollection.Create(Model as TCustomModel, ScreenObject,
    LakCollection);
  // GWT
  FSpecifiedConcentrations := TLktGwtConcCollection.Create(Model as TCustomModel, ScreenObject,
    LakCollection);
  FRainfallConcentrations := TLktGwtConcCollection.Create(Model as TCustomModel, ScreenObject,
    LakCollection);
  FEvapConcentrations := TLktGwtConcCollection.Create(Model as TCustomModel, ScreenObject,
    LakCollection);
  FRunoffConcentrations := TLktGwtConcCollection.Create(Model as TCustomModel, ScreenObject,
    LakCollection);
  FInflowConcentrations := TLktGwtConcCollection.Create(Model as TCustomModel, ScreenObject,
    LakCollection);

  inherited;
  FGwtStatus := TGwtBoundaryStatusCollection.Create(Model as TCustomModel);
end;

procedure TLakeTimeItem.CreateFormulaObjects;
begin
  inherited;
  FStage := CreateFormulaObject(dsoTop);
  FRainfall := CreateFormulaObject(dsoTop);
  FRunoff := CreateFormulaObject(dsoTop);
  FEvaporation := CreateFormulaObject(dsoTop);
  FInflow := CreateFormulaObject(dsoTop);
  FWithdrawal := CreateFormulaObject(dsoTop);
end;

destructor TLakeTimeItem.Destroy;
var
  Index: Integer;
begin
  FGwtStatus.Free;
  for Index := 0 to FSpecifiedConcentrations.Count - 1 do
  begin
    FSpecifiedConcentrations[Index].Value := '0';
  end;
  FSpecifiedConcentrations.Free;

  for Index := 0 to FRainfallConcentrations.Count - 1 do
  begin
    FRainfallConcentrations[Index].Value := '0';
  end;
  FRainfallConcentrations.Free;

  for Index := 0 to FEvapConcentrations.Count - 1 do
  begin
    FEvapConcentrations[Index].Value := '0';
  end;
  FEvapConcentrations.Free;

  for Index := 0 to FRunoffConcentrations.Count - 1 do
  begin
    FRunoffConcentrations[Index].Value := '0';
  end;
  FRunoffConcentrations.Free;

  for Index := 0 to FInflowConcentrations.Count - 1 do
  begin
    FInflowConcentrations[Index].Value := '0';
  end;
  FInflowConcentrations.Free;

  for Index := 0 to FDensity.Count - 1 do
  begin
    FDensity[Index].Value := '0';
  end;
  FDensity.Free;

  inherited;
end;

function TLakeTimeItem.GetBoundaryFormula(Index: integer): string;
var
  ChemSpeciesCount: Integer;
begin
  case Index of
    Lak6StagePosition: result := Stage;
    Lak6RainfallPosition: result := Rainfall;
    Lak6RunoffPosition: result := Runoff;
    Lak6EvaporationPosition: result := Evaporation;
    Lak6InflowPosition: result := Inflow;
    Lak6WithdrawalPosition: result := Withdrawal;
    LakeDensityPosition:
      begin
        if Density.Count < 1 then
        begin
          Density.Add;
        end;
        result := Density[0].Value;
      end
    else
      begin
        // GWT
        if frmGoPhast.PhastModel.GwtUsed then
        begin
          Index := Index-Lak6GwtPestStartPosition;
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

          while RainfallConcentrations.Count < ChemSpeciesCount do
          begin
            RainfallConcentrations.Add;
          end;
          if Index < ChemSpeciesCount then
          begin
            result := RainfallConcentrations[Index].Value;
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
          Index := Index - ChemSpeciesCount;

          while RunoffConcentrations.Count < ChemSpeciesCount do
          begin
            RunoffConcentrations.Add;
          end;
          if Index < ChemSpeciesCount then
          begin
            result := RunoffConcentrations[Index].Value;
            Exit;
          end;
          Index := Index - ChemSpeciesCount;

          while InflowConcentrations.Count < ChemSpeciesCount do
          begin
            InflowConcentrations.Add;
          end;
          if Index < ChemSpeciesCount then
          begin
            result := InflowConcentrations[Index].Value;
            Exit;
          end;
          Assert(False);
        end
        else
        begin
          Assert(False);
        end;
      end;
  end;
end;

function TLakeTimeItem.GetEvaporation: string;
begin
  Result := FEvaporation.Formula;
  ResetItemObserver(Lak6EvaporationPosition);
end;

function TLakeTimeItem.GetInflow: string;
begin
  Result := FInflow.Formula;
  ResetItemObserver(Lak6InflowPosition);
end;

procedure TLakeTimeItem.GetPropertyObserver(Sender: TObject; List: TList);
var
  ConcIndex: Integer;
  Item: TGwtConcStringValueItem;
begin
  inherited;
  if Sender = FStage as TObject then
  begin
    List.Add(FObserverList[Lak6StagePosition]);
  end;
  if Sender = FRainfall as TObject then
  begin
    List.Add(FObserverList[Lak6RainfallPosition]);
  end;
  if Sender = FRunoff as TObject then
  begin
    List.Add(FObserverList[Lak6RunoffPosition]);
  end;
  if Sender = FEvaporation as TObject then
  begin
    List.Add(FObserverList[Lak6EvaporationPosition]);
  end;
  if Sender = FInflow as TObject then
  begin
    List.Add(FObserverList[Lak6InflowPosition]);
  end;
  if Sender = FWithdrawal as TObject then
  begin
    List.Add(FObserverList[Lak6WithdrawalPosition]);
  end;
  if Sender = FDensity as TObject then
  begin
    List.Add(FObserverList[LakeDensityPosition]);
  end;

  // GWT
  for ConcIndex := 0 to SpecifiedConcentrations.Count - 1 do
  begin
    Item := SpecifiedConcentrations.Items[ConcIndex];
    if Item.ValueObject as TObject = Sender then
    begin
      List.Add(Item.Observer);
    end;
  end;

  for ConcIndex := 0 to RainfallConcentrations.Count - 1 do
  begin
    Item := RainfallConcentrations.Items[ConcIndex];
    if Item.ValueObject as TObject = Sender then
    begin
      List.Add(Item.Observer);
    end;
  end;

  for ConcIndex := 0 to EvapConcentrations.Count - 1 do
  begin
    Item := EvapConcentrations.Items[ConcIndex];
    if Item.ValueObject as TObject = Sender then
    begin
      List.Add(Item.Observer);
    end;
  end;

  for ConcIndex := 0 to RunoffConcentrations.Count - 1 do
  begin
    Item := RunoffConcentrations.Items[ConcIndex];
    if Item.ValueObject as TObject = Sender then
    begin
      List.Add(Item.Observer);
    end;
  end;

  for ConcIndex := 0 to InflowConcentrations.Count - 1 do
  begin
    Item := InflowConcentrations.Items[ConcIndex];
    if Item.ValueObject as TObject = Sender then
    begin
      List.Add(Item.Observer);
    end;
  end;

end;

function TLakeTimeItem.GetRainfall: string;
begin
  Result := FRainfall.Formula;
  ResetItemObserver(Lak6RainfallPosition);
end;

function TLakeTimeItem.GetRunoff: string;
begin
  Result := FRunoff.Formula;
  ResetItemObserver(Lak6RunoffPosition);
end;

function TLakeTimeItem.GetStage: string;
begin
  Result := FStage.Formula;
  ResetItemObserver(Lak6StagePosition);
end;

function TLakeTimeItem.GetWithdrawal: string;
begin
  Result := FWithdrawal.Formula;
  ResetItemObserver(Lak6WithdrawalPosition);
end;

function TLakeTimeItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  LakeItem: TLakeTimeItem;
//  Index: Integer;
begin
  Result := inherited IsSame(AnotherItem) and (AnotherItem is TLakeTimeItem);
  if result then
  begin
    LakeItem := TLakeTimeItem(AnotherItem);
    result :=
      (Status = LakeItem.Status)
      and (Stage = LakeItem.Stage)
      and (Rainfall = LakeItem.Rainfall)
      and (Evaporation = LakeItem.Evaporation)
      and (Runoff = LakeItem.Runoff)
      and (Inflow = LakeItem.Inflow)
      and (Withdrawal = LakeItem.Withdrawal)
      and (Density = LakeItem.Density)
      and LakeItem.SpecifiedConcentrations.IsSame(SpecifiedConcentrations)
      and LakeItem.RainfallConcentrations.IsSame(RainfallConcentrations)
      and LakeItem.EvapConcentrations.IsSame(EvapConcentrations)
      and LakeItem.RunoffConcentrations.IsSame(RunoffConcentrations)
      and LakeItem.InflowConcentrations.IsSame(InflowConcentrations)
      and LakeItem.GwtStatus.IsSame(GwtStatus);
  end;
end;

procedure TLakeTimeItem.RemoveFormulaObjects;
begin
  inherited;
  frmGoPhast.PhastModel.FormulaManager.Remove(FStage,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRainfall,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRunoff,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FEvaporation,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FInflow,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FWithdrawal,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TLakeTimeItem.SetBoundaryFormula(Index: integer; const Value: string);
var
  ChemSpeciesCount: Integer;
begin
  case Index of
    Lak6StagePosition:
      Stage := Value;
    Lak6RainfallPosition:
      Rainfall := Value;
    Lak6RunoffPosition:
      Runoff := Value;
    Lak6EvaporationPosition:
      Evaporation := Value;
    Lak6InflowPosition:
      Inflow := Value;
    Lak6WithdrawalPosition:
      Withdrawal := Value;
    LakeDensityPosition:
      begin
        if Density.Count < 1 then
        begin
          Density.Add;
        end;
        Density[0].Value := Value;
      end;
    else
      begin
        // GWT
        if frmGoPhast.PhastModel.GwtUsed then
        begin
          Index := Index - Lak6GwtPestStartPosition;
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

          while RainfallConcentrations.Count < ChemSpeciesCount do
          begin
            RainfallConcentrations.Add;
          end;
          if Index < ChemSpeciesCount then
          begin
            RainfallConcentrations[Index].Value := Value;
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
          Index := Index - ChemSpeciesCount;

          while RunoffConcentrations.Count < ChemSpeciesCount do
          begin
            RunoffConcentrations.Add;
          end;
          if Index < ChemSpeciesCount then
          begin
            RunoffConcentrations[Index].Value := Value;
            Exit;
          end;
          Index := Index - ChemSpeciesCount;

          while InflowConcentrations.Count < ChemSpeciesCount do
          begin
            InflowConcentrations.Add;
          end;
          if Index < ChemSpeciesCount then
          begin
            InflowConcentrations[Index].Value := Value;
            Exit;
          end;
          Assert(False);
        end
        else
        begin
          Assert(False);
        end
      end;
  end;
end;

procedure TLakeTimeItem.SetDensity(const Value: TLktGwtConcCollection);
begin
  FDensity.Assign(Value);
end;

procedure TLakeTimeItem.SetEvapConcentrations(
  const Value: TLktGwtConcCollection);
begin
  FEvapConcentrations.Assign(Value);
end;

procedure TLakeTimeItem.SetEvaporation(const Value: string);
begin
  UpdateFormulaBlocks(Value, Lak6EvaporationPosition, FEvaporation);
end;

procedure TLakeTimeItem.SetGwtStatus(const Value: TGwtBoundaryStatusCollection);
begin
  FGwtStatus.Assign(Value);
end;

procedure TLakeTimeItem.SetInflow(const Value: string);
begin
  UpdateFormulaBlocks(Value, Lak6InflowPosition, FInflow);
end;

procedure TLakeTimeItem.SetInflowConcentrations(
  const Value: TLktGwtConcCollection);
begin
  FInflowConcentrations.Assign(Value);
end;

procedure TLakeTimeItem.SetRainfall(const Value: string);
begin
  UpdateFormulaBlocks(Value, Lak6RainfallPosition, FRainfall);
end;

procedure TLakeTimeItem.SetRainfallConcentrations(
  const Value: TLktGwtConcCollection);
begin
  FRainfallConcentrations.Assign(Value);
end;

procedure TLakeTimeItem.SetRunoff(const Value: string);
begin
  UpdateFormulaBlocks(Value, Lak6RunoffPosition, FRunoff);
end;

procedure TLakeTimeItem.SetRunoffConcentrations(
  const Value: TLktGwtConcCollection);
begin
  FRunoffConcentrations.Assign(Value);
end;

procedure TLakeTimeItem.SetSpecifiedConcentrations(
  const Value: TLktGwtConcCollection);
begin
  FSpecifiedConcentrations.Assign(Value);
end;

procedure TLakeTimeItem.SetStage(const Value: string);
begin
  UpdateFormulaBlocks(Value, Lak6StagePosition, FStage);
end;

procedure TLakeTimeItem.SetStatus(const Value: TLakeStatus);
begin
  FStatus := Value;
end;

procedure TLakeTimeItem.SetWithdrawal(const Value: string);
begin
  UpdateFormulaBlocks(Value, Lak6WithdrawalPosition, FWithdrawal);
end;

{ TLakeOutletTimeItem }

procedure TLakeOutletTimeItem.Assign(Source: TPersistent);
var
  SourceItem: TLakeOutletTimeItem;
begin
  if Source is TLakeOutletTimeItem then
  begin
    SourceItem := TLakeOutletTimeItem(Source);
    Rate := SourceItem.Rate;
    Invert := SourceItem.Invert;
    Roughness := SourceItem.Roughness;
    Width := SourceItem.Width;
    Slope := SourceItem.Slope;
  end;
  inherited;
end;

procedure TLakeOutletTimeItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TLakOutletTimeCollection;
  RateObserver: TObserver;
  InvertObserver: TObserver;
  RoughnessObserver: TObserver;
  WidthObserver: TObserver;
  SlopeObserver: TObserver;
begin
//  inherited;
  ParentCollection := Collection as TLakOutletTimeCollection;

  RateObserver := FObserverList[KRatePosition];
  RateObserver.OnUpToDateSet := ParentCollection.InvalidateRate;

  InvertObserver := FObserverList[KInvertPosition];
  InvertObserver.OnUpToDateSet := ParentCollection.InvalidateInvert;

  RoughnessObserver := FObserverList[KRoughnessPosition];
  RoughnessObserver.OnUpToDateSet := ParentCollection.InvalidateRoughness;

  WidthObserver := FObserverList[KWidthPosition];
  WidthObserver.OnUpToDateSet := ParentCollection.InvalidateWidth;

  SlopeObserver := FObserverList[KSlopePosition];
  SlopeObserver.OnUpToDateSet := ParentCollection.InvalidateSlope;
end;

function TLakeOutletTimeItem.BoundaryFormulaCount: integer;
begin
  result := 5;
end;

procedure TLakeOutletTimeItem.CreateFormulaObjects;
begin
  FRate := CreateFormulaObject(dsoTop);
  FInvert := CreateFormulaObject(dsoTop);
  FRoughness := CreateFormulaObject(dsoTop);
  FWidth := CreateFormulaObject(dsoTop);
  FSlope := CreateFormulaObject(dsoTop);
end;

function TLakeOutletTimeItem.GetBoundaryFormula(Index: integer): string;
begin
  case index of
    KRatePosition: result := Rate;
    KInvertPosition: result := Invert;
    KRoughnessPosition: result := Roughness;
    KWidthPosition: result := Width;
    KSlopePosition: result := Slope;
    else
      Assert(False);
  end;
end;

function TLakeOutletTimeItem.GetInvert: string;
begin
  FInvert.ScreenObject := ScreenObjectI;
  try
    Result := FInvert.Formula;
  finally
    FInvert.ScreenObject := nil;
  end;
  ResetItemObserver(KInvertPosition);
end;

procedure TLakeOutletTimeItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FRate as TObject then
  begin
    List.Add(FObserverList[KRatePosition]);
  end;
  if Sender = FInvert as TObject then
  begin
    List.Add(FObserverList[KInvertPosition]);
  end;
  if Sender = FRoughness as TObject then
  begin
    List.Add(FObserverList[KRoughnessPosition]);
  end;
  if Sender = FWidth as TObject then
  begin
    List.Add(FObserverList[KWidthPosition]);
  end;
  if Sender = FSlope as TObject then
  begin
    List.Add(FObserverList[KSlopePosition]);
  end;
end;

function TLakeOutletTimeItem.GetRate: string;
begin
  FRate.ScreenObject := ScreenObjectI;
  try
    Result := FRate.Formula;
  finally
    FRate.ScreenObject := nil;
  end;
  ResetItemObserver(KRatePosition);
end;

function TLakeOutletTimeItem.GetRoughness: string;
begin
  FRoughness.ScreenObject := ScreenObjectI;
  try
    Result := FRoughness.Formula;
  finally
    FRoughness.ScreenObject := nil;
  end;
  ResetItemObserver(KRoughnessPosition);
end;

function TLakeOutletTimeItem.GetSlope: string;
begin
  FSlope.ScreenObject := ScreenObjectI;
  try
    Result := FSlope.Formula;
  finally
    FSlope.ScreenObject := nil;
  end;
  ResetItemObserver(KSlopePosition);
end;

function TLakeOutletTimeItem.GetWidth: string;
begin
  FWidth.ScreenObject := ScreenObjectI;
  try
    Result := FWidth.Formula;
  finally
    FWidth.ScreenObject := nil;
  end;
  ResetItemObserver(KWidthPosition);
end;

function TLakeOutletTimeItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  SourceItem: TLakeOutletTimeItem;
begin
  Result := inherited IsSame(AnotherItem) and (AnotherItem is TLakeOutletTimeItem);
  if result then
  begin
    SourceItem := TLakeOutletTimeItem(AnotherItem);
    result := (Rate = SourceItem.Rate)
      and (Invert = SourceItem.Invert)
      and (Roughness = SourceItem.Roughness)
      and (Width = SourceItem.Width)
      and (Slope = SourceItem.Slope);
  end;
end;

procedure TLakeOutletTimeItem.RemoveFormulaObjects;
begin
  inherited;
  frmGoPhast.PhastModel.FormulaManager.Remove(FRate,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FInvert,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRoughness,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FWidth,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FSlope,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TLakeOutletTimeItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case index of
    KRatePosition:
      Rate := Value;
    KInvertPosition:
      Invert := Value;
    KRoughnessPosition:
      Roughness := Value;
    KWidthPosition:
      Width := Value;
    KSlopePosition:
      Slope := Value;
    else
      Assert(False);
  end;
end;

procedure TLakeOutletTimeItem.SetInvert(const Value: string);
begin
  FInvert.ScreenObject := ScreenObjectI;
  try
    UpdateFormulaBlocks(Value, KInvertPosition, FInvert);
  finally
    FInvert.ScreenObject := nil;
  end;
end;

procedure TLakeOutletTimeItem.SetRate(const Value: string);
begin
  FRate.ScreenObject := ScreenObjectI;
  try
    UpdateFormulaBlocks(Value, KRatePosition, FRate);
  finally
    FRate.ScreenObject := nil;
  end;
end;

procedure TLakeOutletTimeItem.SetRoughness(const Value: string);
begin
  FRoughness.ScreenObject := ScreenObjectI;
  try
    UpdateFormulaBlocks(Value, KRoughnessPosition, FRoughness);
  finally
    FRoughness.ScreenObject := nil;
  end;
end;

procedure TLakeOutletTimeItem.SetSlope(const Value: string);
begin
  FSlope.ScreenObject := ScreenObjectI;
  try
    UpdateFormulaBlocks(Value, KSlopePosition, FSlope);
  finally
    FSlope.ScreenObject := nil;
  end;
end;

procedure TLakeOutletTimeItem.SetWidth(const Value: string);
begin
  FWidth.ScreenObject := ScreenObjectI;
  try
    UpdateFormulaBlocks(Value, KWidthPosition, FWidth);
  finally
    FWidth.ScreenObject := nil;
  end;
end;

{ TLakeOutlet }

procedure TLakeOutlet.Assign(Source: TPersistent);
var
  OutletSource: TLakeOutlet;
begin
  if Source is TLakeOutlet then
  begin
    OutletSource := TLakeOutlet(Source);
    OutletType := OutletSource.OutletType;
    LakeTimes := OutletSource.LakeTimes;
    OutletObjectName := OutletSource.OutletObjectName;
  end
  else
  begin
    inherited;
  end;
end;

function TLakeOutlet.BoundaryObserverPrefix: string;
begin
  result := 'LakeOutlet';
end;

constructor TLakeOutlet.Create(Model: IModelForTOrderedCollection; ScreenObject: TObject);
begin
  inherited Create(Model, ScreenObject);
  FLakeTimes := TLakOutletTimeCollection.Create(self, Model, ScreenObject);
end;

destructor TLakeOutlet.Destroy;
begin
  FLakeTimes.Free;
  inherited;
end;

function TLakeOutlet.GetOutletObject: TObject;
var
  AScreenObject: TScreenObject;
begin
  result := FOutletObject;
  if result <> nil then
  begin
    AScreenObject := result as TScreenObject;
    if AScreenObject.Deleted
      or (AScreenObject.ModflowLak6 = nil)
      or not AScreenObject.ModflowLak6.Used then
    begin
      result := nil;
    end;
  end;
end;

function TLakeOutlet.GetOutletObjectName: string;
begin
  if FOutletObject <> nil then
  begin
    result := (FOutletObject as TScreenObject).Name;
  end
  else
  begin
    result := FOutletObjectName;
  end;
end;

function TLakeOutlet.IsSame(AnotherLakeOutlet: TLakeOutlet): boolean;
begin
  result := (OutletObjectName = AnotherLakeOutlet.OutletObjectName)
    and (OutletType = AnotherLakeOutlet.OutletType)
    and LakeTimes.IsSame(AnotherLakeOutlet.LakeTimes);
end;

procedure TLakeOutlet.SetLakeTimes(const Value: TLakOutletTimeCollection);
begin
  FLakeTimes.Assign(Value);
end;

procedure TLakeOutlet.SetOutletObject(const Value: TObject);
begin
  FOutletObject := Value;
  if (FOutletObject <> nil) then
  begin
    FOutletObjectName := (FOutletObject as TScreenObject).Name
  end
  else
  begin
    FOutletObjectName := '';
  end;
end;

procedure TLakeOutlet.SetOutletObjectName(const Value: string);
begin
  FOutletObjectName := Value;
  UpdateOutletObject;
end;

procedure TLakeOutlet.SetOutletType(const Value: TLakeOutletType);
begin
  FOutletType := Value;
end;

function TLakeOutlet.Used: boolean;
begin
  result := True;
end;

procedure TLakeOutlet.UpdateOutletObject;
var
  LocalModel: TCustomModel;
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
begin
  if ParentModel <> nil then
  begin
    LocalModel := ParentModel as TCustomModel;
    for ObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := LocalModel.ScreenObjects[ObjectIndex];
      if (AScreenObject.Name = FOutletObjectName)
        and not AScreenObject.Deleted
        and (AScreenObject.ModflowLak6 <> nil)
        and AScreenObject.ModflowLak6.Used then
      begin
        OutletObject := AScreenObject;
        break;
      end;
    end;
  end;
end;

{ TLakeOutletItem }

procedure TLakeOutletItem.Assign(Source: TPersistent);
begin
  if Source is TLakeOutletItem then
  begin
    Outlet := TLakeOutletItem(Source).Outlet
  end;
  inherited;
end;

constructor TLakeOutletItem.Create(Collection: TCollection);
var
  LakeOutlets: TLakeOutlets;
begin
  inherited;
  LakeOutlets := Collection as TLakeOutlets;
//  LakeOutlets.
  FOutlet := TLakeOutlet.Create(LakeOutlets.Model, LakeOutlets.ScreenObject as TScreenObject);
  FOutlet.OutletIndex := Index + 1;
end;


destructor TLakeOutletItem.Destroy;
begin
  FOutlet.Free;
  inherited;
end;

function TLakeOutletItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  if AnotherItem is TLakeOutletItem then
  begin
    result := Outlet.IsSame(TLakeOutletItem(AnotherItem).Outlet);
  end
  else
  begin
    result := False;
  end;
end;

procedure TLakeOutletItem.Loaded;
begin
  FOutlet.UpdateOutletObject;
end;

procedure TLakeOutletItem.SetOutlet(const Value: TLakeOutlet);
begin
  FOutlet.Assign(Value);
end;

{ TLakeOutlets }

function TLakeOutlets.Add: TLakeOutletItem;
begin
  Result := inherited Add as TLakeOutletItem
end;

constructor TLakeOutlets.Create(Model: IModelForTOrderedCollection; ScreenObject: TObject);
begin
//  FScreenObject := ScreenObject;
  inherited Create(TLakeOutletItem, Model, ScreenObject as TScreenObject);
end;

function TLakeOutlets.GetItems(Index: Integer): TLakeOutletItem;
begin
  Result := inherited Items[Index] as TLakeOutletItem;
end;

procedure TLakeOutlets.Loaded;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].Loaded;
  end;
end;

procedure TLakeOutlets.SetItems(Index: Integer; const Value: TLakeOutletItem);
begin
  inherited Items[Index] := Value;
end;

{ TLakOutletTimeCollection }

function TLakOutletTimeCollection.Add: TLakeOutletTimeItem;
begin
  result := inherited Add as TLakeOutletTimeItem;
end;

function TLakOutletTimeCollection.GetItem(Index: Integer): TLakeOutletTimeItem;
begin
  result := inherited Items[index] as TLakeOutletTimeItem;
end;

procedure TLakOutletTimeCollection.InvalidateInvert(Sender: TObject);
begin

end;

procedure TLakOutletTimeCollection.InvalidateRate(Sender: TObject);
begin

end;

procedure TLakOutletTimeCollection.InvalidateRoughness(Sender: TObject);
begin

end;

procedure TLakOutletTimeCollection.InvalidateSlope(Sender: TObject);
begin

end;

procedure TLakOutletTimeCollection.InvalidateWidth(Sender: TObject);
begin

end;

class function TLakOutletTimeCollection.ItemClass: TBoundaryItemClass;
begin
  Result := TLakeOutletTimeItem;
end;

procedure TLakOutletTimeCollection.SetItem(Index: Integer;
  const Value: TLakeOutletTimeItem);
begin
  inherited Items[index] := Value;
end;

{ TLakTimeCollection }

procedure TLakTimeCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  inherited;

end;

class function TLakTimeCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := nil;
  Assert(False);
end;

procedure TLakTimeCollection.InvalidateDensity(Sender: TObject);
begin

end;

procedure TLakTimeCollection.InvalidateEvapConcentrations(Sender: TObject);
begin

end;

procedure TLakTimeCollection.InvalidateEvaporation(Sender: TObject);
begin

end;

procedure TLakTimeCollection.InvalidateGwtStatus(Sender: TObject);
begin

end;

procedure TLakTimeCollection.InvalidateInflow(Sender: TObject);
begin

end;

procedure TLakTimeCollection.InvalidateInflowConcentrations(Sender: TObject);
begin

end;

procedure TLakTimeCollection.InvalidateRainfall(Sender: TObject);
begin

end;

procedure TLakTimeCollection.InvalidateRainfallConcentrations(Sender: TObject);
begin

end;

procedure TLakTimeCollection.InvalidateRunoff(Sender: TObject);
begin

end;

procedure TLakTimeCollection.InvalidateRunoffConcentrations(Sender: TObject);
begin

end;

procedure TLakTimeCollection.InvalidateSpecifiedConcentrations(Sender: TObject);
begin

end;

procedure TLakTimeCollection.InvalidateStage(Sender: TObject);
begin

end;

procedure TLakTimeCollection.InvalidateWithdrawal(Sender: TObject);
begin

end;

class function TLakTimeCollection.ItemClass: TBoundaryItemClass;
begin
  result := TLakeTimeItem;
end;

{ TLakeMf6 }

procedure TLakeMf6.Assign(Source: TPersistent);
var
  LakeSource: TLakeMf6;
  Index: Integer;
begin
  if Source is TLakeMf6 then
  begin
    LakeSource := TLakeMf6(Source);
    Outlets := LakeSource.Outlets;
    LakeTable := LakeSource.LakeTable;
    LakeConnections := LakeSource.LakeConnections;
    Embedded := LakeSource.Embedded;
    BottomElevation := LakeSource.BottomElevation;
    TopElevation := LakeSource.TopElevation;
    BedK := LakeSource.BedK;
    BedThickness := LakeSource.BedThickness;
    ConnectionLength := LakeSource.ConnectionLength;
    StartingStage := LakeSource.StartingStage;
    StartingConcentrations := LakeSource.StartingConcentrations;

    PestSpecifiedConcentrations := LakeSource.PestSpecifiedConcentrations;
    PestSpecifiedConcentrationMethods := LakeSource.PestSpecifiedConcentrationMethods;
    PestRainfallConcentrations := LakeSource.PestRainfallConcentrations;
    PestRainfallConcentrationMethods := LakeSource.PestRainfallConcentrationMethods;
    PestEvaporationConcentrations := LakeSource.PestEvaporationConcentrations;
    PestEvaporationConcentrationMethods := LakeSource.PestEvaporationConcentrationMethods;
    PestRunoffConcentrations := LakeSource.PestRunoffConcentrations;
    PestRunoffConcentrationMethods := LakeSource.PestRunoffConcentrationMethods;
    PestInflowConcentrations := LakeSource.PestInflowConcentrations;
    PestInflowConcentrationMethods := LakeSource.PestInflowConcentrationMethods;

    for Index := Lak6StagePosition to LakeDensityPosition do
    begin
      PestBoundaryFormula[Index] := LakeSource.PestBoundaryFormula[Index];
      PestBoundaryMethod[Index] := LakeSource.PestBoundaryMethod[Index];
    end;
  end;
  inherited;
end;

procedure TLakeMf6.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
begin
//  inherited;
  Assert(False);
end;

class function TLakeMf6.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TLakTimeCollection;
end;

function TLakeMf6.BoundaryObserverPrefix: string;
begin
  Result := 'MF6_Lake';
end;

constructor TLakeMf6.Create(Model: TBaseModel; ScreenObject: TObject);
var
  Index: Integer;
begin
  inherited;

  FPestSpecifiedConcentrationObservers := TObserverList.Create;
  FPestRainfallConcentrationObservers := TObserverList.Create;
  FPestEvaporationConcentrationObservers := TObserverList.Create;
  FPestRunoffConcentrationObservers := TObserverList.Create;
  FPestInflowConcentrationObservers := TObserverList.Create;

  FPestSpecifiedConcentrations := TLktGwtConcCollection.Create(Model, ScreenObject, nil);
  FPestSpecifiedConcentrations.UsedForPestSeries := True;
  FPestRainfallConcentrations := TLktGwtConcCollection.Create(Model, ScreenObject, nil);
  FPestRainfallConcentrations.UsedForPestSeries := True;
  FPestEvaporationConcentrations := TLktGwtConcCollection.Create(Model, ScreenObject, nil);
  FPestEvaporationConcentrations.UsedForPestSeries := True;
  FPestRunoffConcentrations := TLktGwtConcCollection.Create(Model, ScreenObject, nil);
  FPestRunoffConcentrations.UsedForPestSeries := True;
  FPestInflowConcentrations := TLktGwtConcCollection.Create(Model, ScreenObject, nil);
  FPestInflowConcentrations.UsedForPestSeries := True;

  FPestSpecifiedConcentrationMethods := TGwtPestMethodCollection.Create(Model as TCustomModel);
  FPestRainfallConcentrationMethods := TGwtPestMethodCollection.Create(Model as TCustomModel);
  FPestEvaporationConcentrationMethods := TGwtPestMethodCollection.Create(Model as TCustomModel);
  FPestRunoffConcentrationMethods := TGwtPestMethodCollection.Create(Model as TCustomModel);
  FPestInflowConcentrationMethods := TGwtPestMethodCollection.Create(Model as TCustomModel);



  FStartingConcentrations := TStringConcCollection.Create(Model as TCustomModel, ScreenObject, nil);
//  FStartingConcentrationPestNames := TStringList.Create;
  CreateBoundaryObserver;
  FOutlets := TLakeOutlets.Create(Model as TCustomModel, ScreenObject);
  FLakeTable := TLakeTableMf6.Create(Model as TCustomModel);
  LakeConnections := [lctHorizontal, lctVertical];
  CreateFormulaObjects;
  CreateObservers;

  BottomElevation := '0';
  TopElevation := '0';
  BedK := '0';
  BedThickness := '0';
  ConnectionLength := '0';
//  ConnectionWidth := '0';
  StartingStage := '0';

  PestStageFormula := '';
  PestRainfallFormula := '';
  PestRunoffFormula := '';
  PestEvaporationFormula := '';
  PestInflowFormula := '';
  PestWithdrawalFormula := '';
  PestDensityFormula := '';

  for Index := Lak6StagePosition to LakeDensityPosition do
  begin
    PestBoundaryMethod[Index] := DefaultBoundaryMethod(Index);
  end;
end;

procedure TLakeMf6.CreateFormulaObjects;
var
  LocalModel: TCustomModel;
  ConcIndex: Integer;
begin
  FPestStageFormula := CreateFormulaObjectBlocks(dso3D);
  FPestRainfallFormula := CreateFormulaObjectBlocks(dso3D);
  FPestRunoffFormula := CreateFormulaObjectBlocks(dso3D);
  FPestEvaporationFormula := CreateFormulaObjectBlocks(dso3D);
  FPestInflowFormula := CreateFormulaObjectBlocks(dso3D);
  FPestWithdrawalFormula := CreateFormulaObjectBlocks(dso3D);
  FPestDensityFormula := CreateFormulaObjectBlocks(dso3D);

  FBottomElevation := CreateFormulaObjectBlocks(dso3D);
  FTopElevation := CreateFormulaObjectBlocks(dso3D);
  FBedK := CreateFormulaObjectBlocks(dso3D);
  FBedThickness := CreateFormulaObjectBlocks(dso3D);
  FConnectionLength := CreateFormulaObjectBlocks(dso3D);
  FStartingStage := CreateFormulaObjectBlocks(dso3D);

  LocalModel := ParentModel as TCustomModel;
  if (LocalModel <> nil) and LocalModel.GwtUsed then
  begin
    for ConcIndex := 0 to LocalModel.MobileComponents.Count - 1 do
    begin
      FPestSpecifiedConcentrations.Add;
    end;
    for ConcIndex := 0 to LocalModel.MobileComponents.Count - 1 do
    begin
      FPestRainfallConcentrations.Add;
    end;
    for ConcIndex := 0 to LocalModel.MobileComponents.Count - 1 do
    begin
      FPestEvaporationConcentrations.Add;
    end;
    for ConcIndex := 0 to LocalModel.MobileComponents.Count - 1 do
    begin
      FPestRunoffConcentrations.Add;
    end;
    for ConcIndex := 0 to LocalModel.MobileComponents.Count - 1 do
    begin
      FPestInflowConcentrations.Add;
    end;
  end;

end;

procedure TLakeMf6.CreateObservers;
var
  Index: Integer;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(PestStageObserver);
    FObserverList.Add(PestRainfallObserver);
    FObserverList.Add(PestRunoffObserver);
    FObserverList.Add(PestEvaporationObserver);
    FObserverList.Add(PestInflowObserver);
    FObserverList.Add(PestWithdrawalObserver);

    FObserverList.Add(BottomElevationObserver);
    FObserverList.Add(TopElevationObserver);
    FObserverList.Add(BedKObserver);
    FObserverList.Add(BedThicknessObserver);
    FObserverList.Add(ConnectionLengthObserver);
    FObserverList.Add(ConnectionWidthObserver);
    FObserverList.Add(StartingStageObserver);

    for Index := 0 to FPestSpecifiedConcentrations.Count - 1 do
    begin
      FObserverList.Add(PestSpecifiedConcentrationObserver[Index]);
    end;
    for Index := 0 to FPestRainfallConcentrations.Count - 1 do
    begin
      FObserverList.Add(PestRainfallConcentrationObserver[Index]);
    end;
    for Index := 0 to FPestEvaporationConcentrations.Count - 1 do
    begin
      FObserverList.Add(PestEvaporationConcentrationObserver[Index]);
    end;
    for Index := 0 to FPestRunoffConcentrations.Count - 1 do
    begin
      FObserverList.Add(PestRunoffConcentrationObserver[Index]);
    end;
    for Index := 0 to FPestInflowConcentrations.Count - 1 do
    begin
      FObserverList.Add(PestInflowConcentrationObserver[Index]);
    end;
  end;
end;

class function TLakeMf6.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    Lak6StagePosition:
      begin
        result := ppmAdd;
      end;
    Lak6RainfallPosition:
      begin
        result := ppmMultiply;
      end;
    Lak6RunoffPosition:
      begin
        result := ppmMultiply;
      end;
    Lak6EvaporationPosition:
      begin
        result := ppmMultiply;
      end;
    Lak6InflowPosition:
      begin
        result := ppmMultiply;
      end;
    Lak6WithdrawalPosition:
      begin
        result := ppmMultiply;
      end;
    LakeDensityPosition:
      begin
        result := ppmMultiply;
      end;
    else
      begin
        result := inherited;
//        Assert(False);
      end;
  end;
end;

destructor TLakeMf6.Destroy;
begin
  BottomElevation := '0';
  TopElevation := '0';
  BedK := '0';
  BedThickness := '0';
  ConnectionLength := '0';
//  ConnectionWidth := '0';
  StartingStage := '0';

  PestStageFormula := '';
  PestRainfallFormula := '';
  PestRunoffFormula := '';
  PestEvaporationFormula := '';
  PestInflowFormula := '';
  PestWithdrawalFormula := '';

//  FStartingConcentrationPestNames.Free;
  FStartingConcentrations.Free;
  FLakeTable.Free;
  FOutlets.Free;

  FPestSpecifiedConcentrationMethods.Free;
  FPestRainfallConcentrationMethods.Free;
  FPestEvaporationConcentrationMethods.Free;
  FPestRunoffConcentrationMethods.Free;
  FPestInflowConcentrationMethods.Free;

  FPestSpecifiedConcentrations.Free;
  FPestRainfallConcentrations.Free;
  FPestEvaporationConcentrations.Free;
  FPestRunoffConcentrations.Free;
  FPestInflowConcentrations.Free;

  FPestSpecifiedConcentrationObservers.Free;
  FPestRainfallConcentrationObservers.Free;
  FPestEvaporationConcentrationObservers.Free;
  FPestRunoffConcentrationObservers.Free;
  FPestInflowConcentrationObservers.Free;

  inherited;
end;

function TLakeMf6.GetBedK: string;
begin
  Result := FBedK.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(BedKPosition);
  end;
end;

function TLakeMf6.GetBedKObserver: TObserver;
var
//  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FBedKObserver = nil then
  begin
    DataArray := nil;
    if ParentModel <> nil then
    begin
//      Model := ParentModel as TPhastModel;
//      DataArray := Model.DataArrayManager.GetDataSetByName(KCfpFixedHeads);
    end;
    CreateObserver('Lake_BedK', FBedKObserver, DataArray);
  end;
  result := FBedKObserver;
end;

function TLakeMf6.GetBedThickness: string;
begin
  Result := FBedThickness.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(BedThicknessPosition);
  end;
end;

function TLakeMf6.GetBedThicknessObserver: TObserver;
var
//  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FBedThicknessObserver = nil then
  begin
    DataArray := nil;
    if ParentModel <> nil then
    begin
//      Model := ParentModel as TPhastModel;
//      DataArray := Model.DataArrayManager.GetDataSetByName(KCfpFixedHeads);
    end;
    CreateObserver('Lake_BedThickness', FBedThicknessObserver, DataArray);
  end;
  result := FBedThicknessObserver;
end;

function TLakeMf6.GetBottomElevation: string;
begin
  Result := FBottomElevation.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(BottomElevationPosition);
  end;
end;

function TLakeMf6.GetBottomElevationObserver: TObserver;
var
//  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FBottomElevationObserver = nil then
  begin
    DataArray := nil;
    if ParentModel <> nil then
    begin
//      Model := ParentModel as TPhastModel;
//      DataArray := Model.DataArrayManager.GetDataSetByName(KCfpFixedHeads);
    end;
    CreateObserver('Lake_BottomElevation', FBottomElevationObserver, DataArray);
  end;
  result := FBottomElevationObserver;
end;

procedure TLakeMf6.GetCellValues(ValueTimeList: TList; ParamList: TStringList;
  AModel: TBaseModel; Writer: TObject);
begin
//  inherited;

end;

function TLakeMf6.GetConnectionLength: string;
begin
  Result := FConnectionLength.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(ConnectionLengthPosition);
  end;
end;

function TLakeMf6.GetConnectionLengthObserver: TObserver;
var
//  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FConnectionLengthObserver = nil then
  begin
    DataArray := nil;
    if ParentModel <> nil then
    begin
//      Model := ParentModel as TPhastModel;
//      DataArray := Model.DataArrayManager.GetDataSetByName(KCfpFixedHeads);
    end;
    CreateObserver('Lake_ConnectionLength', FConnectionLengthObserver, DataArray);
  end;
  result := FConnectionLengthObserver;
end;

//function TLakeMf6.GetConnectionWidth: string;
//begin
//  Result := FConnectionWidth.Formula;
//  if ScreenObject <> nil then
//  begin
//    ResetItemObserver(ConnectionWidthPosition);
//  end;
//end;

function TLakeMf6.GetConnectionWidthObserver: TObserver;
var
//  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FConnectionWidthObserver = nil then
  begin
    DataArray := nil;
    if ParentModel <> nil then
    begin
//      Model := ParentModel as TPhastModel;
//      DataArray := Model.DataArrayManager.GetDataSetByName(KCfpFixedHeads);
    end;
    CreateObserver('Lake_ConnectionWidth', FConnectionWidthObserver, DataArray);
  end;
  result := FConnectionWidthObserver;
end;

function TLakeMf6.GetPestBoundaryFormula(FormulaIndex: integer): string;
var
//  Item: TStringConcValueItem;
  ChemSpeciesCount: Integer;
begin
  case FormulaIndex of
    Lak6StagePosition:
      begin
        result := PestStageFormula;
      end;
    Lak6RainfallPosition:
      begin
        result := PestRainfallFormula;
      end;
    Lak6RunoffPosition:
      begin
        result := PestRunoffFormula;
      end;
    Lak6EvaporationPosition:
      begin
        result := PestEvaporationFormula;
      end;
    Lak6InflowPosition:
      begin
        result := PestInflowFormula;
      end;
    Lak6WithdrawalPosition:
      begin
        result := PestWithdrawalFormula;
      end;
    LakeDensityPosition:
      begin
        result := PestDensityFormula;
      end;
    else
      begin
        FormulaIndex := FormulaIndex - Lak6GwtPestStartPosition;
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
        while PestRainfallConcentrations.Count < ChemSpeciesCount do
        begin
          PestRainfallConcentrations.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          result := PestRainfallConcentrations[FormulaIndex].Value;
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

        FormulaIndex := FormulaIndex-ChemSpeciesCount;
        while PestRunoffConcentrations.Count < ChemSpeciesCount do
        begin
          PestRunoffConcentrations.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          result := PestRunoffConcentrations[FormulaIndex].Value;
          Exit;
        end;

        FormulaIndex := FormulaIndex-ChemSpeciesCount;
        while PestInflowConcentrations.Count < ChemSpeciesCount do
        begin
          PestInflowConcentrations.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          result := PestInflowConcentrations[FormulaIndex].Value;
          Exit;
        end;

        result := inherited;
        Assert(False);
      end;
  end;
end;

function TLakeMf6.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
var
  ChemSpeciesCount: Integer;
begin
  case FormulaIndex of
    Lak6StagePosition:
      begin
        result := PestStageMethod;
      end;
    Lak6RainfallPosition:
      begin
        result := PestRainfallMethod;
      end;
    Lak6RunoffPosition:
      begin
        result := PestRunoffMethod;
      end;
    Lak6EvaporationPosition:
      begin
        result := PestEvaporationMethod;
      end;
    Lak6InflowPosition:
      begin
        result := PestInflowMethod;
      end;
    Lak6WithdrawalPosition:
      begin
        result := PestWithdrawalMethod;
      end;
    LakeDensityPosition:
      begin
        result := PestDensityMethod;
      end;
    else
      begin
        FormulaIndex := FormulaIndex - Lak6GwtPestStartPosition;
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
        while PestRainfallConcentrationMethods.Count < ChemSpeciesCount do
        begin
          PestRainfallConcentrationMethods.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          result := PestRainfallConcentrationMethods[FormulaIndex].PestParamMethod;
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

        FormulaIndex := FormulaIndex-ChemSpeciesCount;
        while PestRunoffConcentrationMethods.Count < ChemSpeciesCount do
        begin
          PestRunoffConcentrationMethods.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          result := PestRunoffConcentrationMethods[FormulaIndex].PestParamMethod;
          Exit;
        end;

        FormulaIndex := FormulaIndex-ChemSpeciesCount;
        while PestInflowConcentrationMethods.Count < ChemSpeciesCount do
        begin
          PestInflowConcentrationMethods.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          result := PestInflowConcentrationMethods[FormulaIndex].PestParamMethod;
          Exit;
        end;

        result := inherited;
        Assert(False);
      end;
  end;
end;

function TLakeMf6.GetPestDensityFormula: string;
begin
  Result := FPestDensityFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(LakeDensityPosition);
  end;
end;

function TLakeMf6.GetPestEvaporationConcentrationObserver(
  const Index: Integer): TObserver;
var
  AObserver: TObserver;
begin
  while Index >= FPestEvaporationConcentrationObservers.Count do
  begin
    CreateObserver(Format('LakPestEvapConc_%d', [Index+1]), AObserver, nil);
    FPestEvaporationConcentrationObservers.Add(AObserver);
    AObserver.OnUpToDateSet := InvalidatePestEvapConcData;
  end;
  result := FPestEvaporationConcentrationObservers[Index];
end;

function TLakeMf6.GetPestEvaporationFormula: string;
begin
  Result := FPestEvaporationFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(Lak6EvaporationPosition);
  end;
end;

function TLakeMf6.GetPestEvaporationObserver: TObserver;
begin
  if FPestEvaporationObserver = nil then
  begin
    CreateObserver('PestEvaporation_', FPestEvaporationObserver, nil);
//    FPestEvaporationObserver.OnUpToDateSet := InvalidateEvaporationData;
  end;
  result := FPestEvaporationObserver;
end;

function TLakeMf6.GetPestInflowConcentrationObserver(
  const Index: Integer): TObserver;
var
  AObserver: TObserver;
begin
  while Index >= FPestInflowConcentrationObservers.Count do
  begin
    CreateObserver(Format('LakPestInflowConc_%d', [Index+1]), AObserver, nil);
    FPestInflowConcentrationObservers.Add(AObserver);
    AObserver.OnUpToDateSet := InvalidatePestInflowConcData;
  end;
  result := FPestInflowConcentrationObservers[Index];
end;

function TLakeMf6.GetPestInflowFormula: string;
begin
  Result := FPestInflowFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(Lak6InflowPosition);
  end;
end;

function TLakeMf6.GetPestInflowObserver: TObserver;
begin
  if FPestInflowObserver = nil then
  begin
    CreateObserver('PestInflow_', FPestInflowObserver, nil);
//    FPestInflowObserver.OnUpToDateSet := InvalidateInflowData;
  end;
  result := FPestInflowObserver;
end;

function TLakeMf6.GetPestRainfallConcentrationObserver(
  const Index: Integer): TObserver;
var
  AObserver: TObserver;
begin
  while Index >= FPestRainfallConcentrationObservers.Count do
  begin
    CreateObserver(Format('LakPestRainfallConc_%d', [Index+1]), AObserver, nil);
    FPestRainfallConcentrationObservers.Add(AObserver);
    AObserver.OnUpToDateSet := InvalidatePestRainfallConcData;
  end;
  result := FPestRainfallConcentrationObservers[Index];
end;

function TLakeMf6.GetPestRainfallFormula: string;
begin
  Result := FPestRainfallFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(Lak6RainfallPosition);
  end;
end;

function TLakeMf6.GetPestRainfallObserver: TObserver;
begin
  if FPestRainfallObserver = nil then
  begin
    CreateObserver('PestRainfall_', FPestRainfallObserver, nil);
//    FPestRainfallObserver.OnUpToDateSet := InvalidateRainfallData;
  end;
  result := FPestRainfallObserver;
end;

function TLakeMf6.GetPestRunoffConcentrationObserver(
  const Index: Integer): TObserver;
var
  AObserver: TObserver;
begin
  while Index >= FPestRunoffConcentrationObservers.Count do
  begin
    CreateObserver(Format('LakPestRunoffConc_%d', [Index+1]), AObserver, nil);
    FPestRunoffConcentrationObservers.Add(AObserver);
    AObserver.OnUpToDateSet := InvalidatePestRunoffConcData;
  end;
  result := FPestRunoffConcentrationObservers[Index];
end;

function TLakeMf6.GetPestRunoffFormula: string;
begin
  Result := FPestRunoffFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(Lak6RunoffPosition);
  end;
end;

function TLakeMf6.GetPestRunoffObserver: TObserver;
begin
  if FPestRunoffObserver = nil then
  begin
    CreateObserver('PestRunoff_', FPestRunoffObserver, nil);
//    FPestRunoffObserver.OnUpToDateSet := InvalidateRunoffData;
  end;
  result := FPestRunoffObserver;
end;

function TLakeMf6.GetPestSpecifiedConcentrationObserver(
  const Index: Integer): TObserver;
var
  AObserver: TObserver;
begin
  while Index >= FPestSpecifiedConcentrationObservers.Count do
  begin
    CreateObserver(Format('LakPestSpecConc_%d', [Index+1]), AObserver, nil);
    FPestSpecifiedConcentrationObservers.Add(AObserver);
    AObserver.OnUpToDateSet := InvalidatePestSpecConcData;
  end;
  result := FPestSpecifiedConcentrationObservers[Index];
end;

function TLakeMf6.GetPestStageFormula: string;
begin
  Result := FPestStageFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(Lak6StagePosition);
  end;
end;

function TLakeMf6.GetPestStageObserver: TObserver;
begin
  if FPestStageObserver = nil then
  begin
    CreateObserver('PestStage_', FPestStageObserver, nil);
//    FPestStageObserver.OnUpToDateSet := InvalidateStageData;
  end;
  result := FPestStageObserver;
end;

function TLakeMf6.GetPestWithdrawalFormula: string;
begin
  Result := FPestWithdrawalFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(Lak6WithdrawalPosition);
  end;
end;

function TLakeMf6.GetPestWithdrawalObserver: TObserver;
begin
  if FPestWithdrawalObserver = nil then
  begin
    CreateObserver('PestWithdrawal_', FPestWithdrawalObserver, nil);
//    FPestWithdrawalObserver.OnUpToDateSet := InvalidateWithdrawalData;
  end;
  result := FPestWithdrawalObserver;
end;

procedure TLakeMf6.GetPropertyObserver(Sender: TObject; List: TList);
var
  StartIndex: Integer;
  Index: Integer;
begin
  if Sender = FPestStageFormula as TObject then
  begin
    List.Add(FObserverList[Lak6StagePosition]);
  end
  else if Sender = FPestRainfallFormula as TObject then
  begin
    List.Add(FObserverList[Lak6RainfallPosition]);
  end
  else if Sender = FPestRunoffFormula as TObject then
  begin
    List.Add(FObserverList[Lak6RunoffPosition]);
  end
  else if Sender = FPestEvaporationFormula as TObject then
  begin
    List.Add(FObserverList[Lak6RunoffPosition]);
  end
  else if Sender = FPestEvaporationFormula as TObject then
  begin
    List.Add(FObserverList[Lak6EvaporationPosition]);
  end
  else if Sender = FPestInflowFormula as TObject then
  begin
    List.Add(FObserverList[Lak6InflowPosition]);
  end
  else if Sender = FPestWithdrawalFormula as TObject then
  begin
    List.Add(FObserverList[Lak6WithdrawalPosition]);
  end
  else if Sender = FPestDensityFormula as TObject then
  begin
    List.Add(FObserverList[LakeDensityPosition]);
  end
  else if Sender = FBottomElevation as TObject then
  begin
    List.Add(FObserverList[BottomElevationPosition]);
  end
  else if Sender = FTopElevation as TObject then
  begin
    List.Add(FObserverList[TopElevationPosition]);
  end
  else if Sender = FBedK as TObject then
  begin
    List.Add(FObserverList[BedKPosition]);
  end
  else if Sender = FBedThickness as TObject then
  begin
    List.Add(FObserverList[BedThicknessPosition]);
  end
  else if Sender = FConnectionLength as TObject then
  begin
    List.Add(FObserverList[ConnectionLengthPosition]);
  end
  else if Sender = FStartingStage as TObject then
  begin
    List.Add(FObserverList[StartingStagePosition]);
  end;

  StartIndex := Lak6GwtPestStartPosition;
  for Index := 0 to FPestSpecifiedConcentrations.Count - 1 do
  begin
    if FPestSpecifiedConcentrations[Index].ValueObject as TObject = Sender then
    begin
      List.Add(FObserverList[StartIndex + Index]);
    end;
  end;

  StartIndex := StartIndex + FPestSpecifiedConcentrations.Count;
  for Index := 0 to PestRainfallConcentrations.Count - 1 do
  begin
    if PestRainfallConcentrations[Index].ValueObject as TObject = Sender then
    begin
      List.Add(FObserverList[StartIndex + Index]);
    end;
  end;

  StartIndex := StartIndex + PestRainfallConcentrations.Count;
  for Index := 0 to PestEvaporationConcentrations.Count - 1 do
  begin
    if PestEvaporationConcentrations[Index].ValueObject as TObject = Sender then
    begin
      List.Add(FObserverList[StartIndex + Index]);
    end;
  end;

  StartIndex := StartIndex + PestEvaporationConcentrations.Count;
  for Index := 0 to PestRunoffConcentrations.Count - 1 do
  begin
    if PestRunoffConcentrations[Index].ValueObject as TObject = Sender then
    begin
      List.Add(FObserverList[StartIndex + Index]);
    end;
  end;

  StartIndex := StartIndex + PestRunoffConcentrations.Count;
  for Index := 0 to PestInflowConcentrations.Count - 1 do
  begin
    if PestInflowConcentrations[Index].ValueObject as TObject = Sender then
    begin
      List.Add(FObserverList[StartIndex + Index]);
    end;
  end;

end;

function TLakeMf6.GetStartingConcentrations: TStringConcCollection;
var
  LocalModel: TCustomModel;
begin
  LocalModel := FStartingConcentrations.Model as TCustomModel;
  if LocalModel = nil then
  begin
    LocalModel := frmGoPhast.PhastModel;
  end;
  if (LocalModel <> nil)
    and (FStartingConcentrations.Count < LocalModel.MobileComponents.Count) then
  begin
    FStartingConcentrations.Count := LocalModel.MobileComponents.Count;
  end;
  result := FStartingConcentrations;
end;

function TLakeMf6.GetStartingStage: string;
begin
  Result := FStartingStage.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(StartingStagePosition);
  end;
end;

function TLakeMf6.GetStartingStageObserver: TObserver;
var
//  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FStartingStageObserver = nil then
  begin
    DataArray := nil;
    if ParentModel <> nil then
    begin
//      Model := ParentModel as TPhastModel;
//      DataArray := Model.DataArrayManager.GetDataSetByName(KCfpFixedHeads);
    end;
    CreateObserver('Lake_StartingStage', FStartingStageObserver, DataArray);
  end;
  result := FStartingStageObserver;
end;

function TLakeMf6.GetTopElevation: string;
begin
  Result := FTopElevation.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(TopElevationPosition);
  end;
end;

function TLakeMf6.GetTopElevationObserver: TObserver;
var
//  Model: TPhastModel;
  DataArray: TDataArray;
begin
  if FTopElevationObserver = nil then
  begin
    DataArray := nil;
    if ParentModel <> nil then
    begin
//      Model := ParentModel as TPhastModel;
//      DataArray := Model.DataArrayManager.GetDataSetByName(KCfpFixedHeads);
    end;
    CreateObserver('Lake_TopElevation', FTopElevationObserver, DataArray);
  end;
  result := FTopElevationObserver;
end;

function TLakeMf6.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('PestLAK6_Used_', FUsedObserver, nil);
  end;
  result := FUsedObserver;
end;

procedure TLakeMf6.HandleChangedValue(Observer: TObserver);
begin
//  inherited;
  InvalidateDisplay;
end;

procedure TLakeMf6.InvalidatePestEvapConcData(Sender: TObject);
begin
  { TODO -cGWT : This needs to be implemented }
end;

procedure TLakeMf6.InvalidatePestInflowConcData(Sender: TObject);
begin
  { TODO -cGWT : This needs to be implemented }
end;

procedure TLakeMf6.InvalidatePestRainfallConcData(Sender: TObject);
begin
  { TODO -cGWT : This needs to be implemented }
end;

procedure TLakeMf6.InvalidatePestRunoffConcData(Sender: TObject);
begin
  { TODO -cGWT : This needs to be implemented }
end;

procedure TLakeMf6.InvalidatePestSpecConcData(Sender: TObject);
begin
  { TODO -cGWT : This needs to be implemented }
end;

procedure TLakeMf6.Loaded;
begin
  Outlets.Loaded;
end;

procedure TLakeMf6.SetBedK(const Value: string);
begin
  UpdateFormulaBlocks(Value, BedKPosition, FBedK);
end;

procedure TLakeMf6.SetBedThickness(const Value: string);
begin
  UpdateFormulaBlocks(Value, BedThicknessPosition, FBedThickness);
end;

procedure TLakeMf6.SetBottomElevation(const Value: string);
begin
  UpdateFormulaBlocks(Value, BottomElevationPosition, FBottomElevation);
end;

procedure TLakeMf6.SetConnectionLength(const Value: string);
begin
  UpdateFormulaBlocks(Value, ConnectionLengthPosition, FConnectionLength);
end;

//procedure TLakeMf6.SetConnectionWidth(const Value: string);
//begin
//  UpdateFormula(Value, ConnectionWidthPosition, FConnectionWidth);
//end;

procedure TLakeMf6.SetEmbedded(const Value: Boolean);
begin
  FEmbedded := Value;
end;

//procedure TLakeMf6.SetGwtStatus(const Value: TGwtBoundaryStatusCollection);
//begin
//  FGwtStatus.Assign(Value);
//end;

procedure TLakeMf6.SetLakeConnections(const Value: TLakeConnectionTypes);
begin
  FLakeConnections := Value;
end;

procedure TLakeMf6.SetLakeTable(const Value: TLakeTableMf6);
begin
  FLakeTable.Assign(Value);
end;

procedure TLakeMf6.SetOutlets(const Value: TLakeOutlets);
begin
  FOutlets.Assign(Value);
end;

procedure TLakeMf6.SetPestBoundaryFormula(FormulaIndex: integer;
  const Value: string);
var
  ChemSpeciesCount: Integer;
begin
  case FormulaIndex of
    Lak6StagePosition:
      begin
        PestStageFormula := Value;
      end;
    Lak6RainfallPosition:
      begin
        PestRainfallFormula := Value;
      end;
    Lak6RunoffPosition:
      begin
        PestRunoffFormula := Value;
      end;
    Lak6EvaporationPosition:
      begin
        PestEvaporationFormula := Value;
      end;
    Lak6InflowPosition:
      begin
        PestInflowFormula := Value;
      end;
    Lak6WithdrawalPosition:
      begin
        PestWithdrawalFormula := Value;
      end;
    LakeDensityPosition:
      begin
        PestDensityFormula := Value;
      end;
    else
      begin
        FormulaIndex := FormulaIndex - Lak6GwtPestStartPosition;
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
        while PestRainfallConcentrations.Count < ChemSpeciesCount do
        begin
          PestRainfallConcentrations.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          PestRainfallConcentrations[FormulaIndex].Value := Value;
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

        FormulaIndex := FormulaIndex-ChemSpeciesCount;
        while PestRunoffConcentrations.Count < ChemSpeciesCount do
        begin
          PestRunoffConcentrations.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          PestRunoffConcentrations[FormulaIndex].Value := Value;
          Exit;
        end;

        FormulaIndex := FormulaIndex-ChemSpeciesCount;
        while PestInflowConcentrations.Count < ChemSpeciesCount do
        begin
          PestInflowConcentrations.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          PestInflowConcentrations[FormulaIndex].Value := Value;
          Exit;
        end;

        inherited;
        Assert(False);
      end
  end;
end;

procedure TLakeMf6.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
var
  ChemSpeciesCount: Integer;
begin
  case FormulaIndex of
    Lak6StagePosition:
      begin
        PestStageMethod := Value;
      end;
    Lak6RainfallPosition:
      begin
        PestRainfallMethod := Value;
      end;
    Lak6RunoffPosition:
      begin
        PestRunoffMethod := Value;
      end;
    Lak6EvaporationPosition:
      begin
        PestEvaporationMethod := Value;
      end;
    Lak6InflowPosition:
      begin
        PestInflowMethod := Value;
      end;
    Lak6WithdrawalPosition:
      begin
        PestWithdrawalMethod := Value;
      end;
    LakeDensityPosition:
      begin
        PestDensityMethod := Value;
      end;
    else
      begin
        FormulaIndex := FormulaIndex - Lak6GwtPestStartPosition;
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
        while PestRainfallConcentrationMethods.Count < ChemSpeciesCount do
        begin
          PestRainfallConcentrationMethods.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          PestRainfallConcentrationMethods[FormulaIndex].PestParamMethod := Value;
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

        FormulaIndex := FormulaIndex-ChemSpeciesCount;
        while PestRunoffConcentrationMethods.Count < ChemSpeciesCount do
        begin
          PestRunoffConcentrationMethods.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          PestRunoffConcentrationMethods[FormulaIndex].PestParamMethod := Value;
          Exit;
        end;

        FormulaIndex := FormulaIndex-ChemSpeciesCount;
        while PestInflowConcentrationMethods.Count < ChemSpeciesCount do
        begin
          PestInflowConcentrationMethods.Add;
        end;
        if FormulaIndex < ChemSpeciesCount then
        begin
          PestInflowConcentrationMethods[FormulaIndex].PestParamMethod := Value;
          Exit;
        end;

        inherited;
        Assert(False);
      end;
  end;
end;

procedure TLakeMf6.SetPestDensityFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, LakeDensityPosition, FPestDensityFormula);
end;

procedure TLakeMf6.SetPestDensityMethod(const Value: TPestParamMethod);
begin
  FPestDensityMethod := Value;
end;

procedure TLakeMf6.SetPestEvaporationConcentrationMethods(
  const Value: TGwtPestMethodCollection);
begin
  FPestEvaporationConcentrationMethods.Assign(Value);
end;

procedure TLakeMf6.SetPestEvaporationConcentrations(
  const Value: TLktGwtConcCollection);
begin
  FPestEvaporationConcentrations.Assign(Value);
end;

procedure TLakeMf6.SetPestEvaporationFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, Lak6EvaporationPosition, FPestEvaporationFormula);
end;

procedure TLakeMf6.SetPestEvaporationMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestEvaporationMethod, Value);
end;

procedure TLakeMf6.SetPestInflowConcentrationMethods(
  const Value: TGwtPestMethodCollection);
begin
  FPestInflowConcentrationMethods.Assign(Value);
end;

procedure TLakeMf6.SetPestInflowConcentrations(
  const Value: TLktGwtConcCollection);
begin
  FPestInflowConcentrations.Assign(Value);
end;

procedure TLakeMf6.SetPestInflowFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, Lak6InflowPosition, FPestInflowFormula);
end;

procedure TLakeMf6.SetPestInflowMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestInflowMethod, Value);
end;

procedure TLakeMf6.SetPestRainfallConcentrationMethods(
  const Value: TGwtPestMethodCollection);
begin
  FPestRainfallConcentrationMethods.Assign(Value);
end;

procedure TLakeMf6.SetPestRainfallConcentrations(
  const Value: TLktGwtConcCollection);
begin
  FPestRainfallConcentrations.Assign(Value);
end;

procedure TLakeMf6.SetPestRainfallFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, Lak6RainfallPosition, FPestRainfallFormula);
end;

procedure TLakeMf6.SetPestRainfallMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestRainfallMethod, Value);
end;

procedure TLakeMf6.SetPestRunoffConcentrationMethods(
  const Value: TGwtPestMethodCollection);
begin
  FPestRunoffConcentrationMethods.Assign(Value);
end;

procedure TLakeMf6.SetPestRunoffConcentrations(
  const Value: TLktGwtConcCollection);
begin
  FPestRunoffConcentrations.Assign(Value);
end;

procedure TLakeMf6.SetPestRunoffFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, Lak6RunoffPosition, FPestRunoffFormula);
end;

procedure TLakeMf6.SetPestRunoffMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestRainfallMethod, Value);
end;

procedure TLakeMf6.SetPestSpecifiedConcentrationMethods(
  const Value: TGwtPestMethodCollection);
begin
  FPestSpecifiedConcentrationMethods.Assign(Value);
end;

procedure TLakeMf6.SetPestSpecifiedConcentrations(
  const Value: TLktGwtConcCollection);
begin
  FPestSpecifiedConcentrations.Assign(Value);
end;

procedure TLakeMf6.SetPestStageFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, Lak6StagePosition, FPestStageFormula);
end;

procedure TLakeMf6.SetPestStageMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestStageMethod, Value);
end;

procedure TLakeMf6.SetPestWithdrawalFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, Lak6WithdrawalPosition, FPestWithdrawalFormula);
end;

procedure TLakeMf6.SetPestWithdrawalMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestWithdrawalMethod, Value);
end;

procedure TLakeMf6.SetStartingConcentrations(
  const Value: TStringConcCollection);
begin
  FStartingConcentrations.Assign(Value);
end;

procedure TLakeMf6.SetStartingStage(const Value: string);
begin
  UpdateFormulaBlocks(Value, StartingStagePosition, FStartingStage);
end;

procedure TLakeMf6.SetTopElevation(const Value: string);
begin
  UpdateFormulaBlocks(Value, TopElevationPosition, FTopElevation);
end;

procedure TLakeMf6.UpdateTimes(Times: TRealList; StartTestTime,
  EndTestTime: double; var StartRangeExtended, EndRangeExtended: boolean;
  AModel: TBaseModel);
var
  SP_Epsilon: Double;
  OutletIndex: Integer;
  AnOutlet: TLakeOutlet;
  TimeIndex: Integer;
  OutletTimeItem: TLakeOutletTimeItem;
  ClosestIndex: Integer;
  ExistingTime: Double;
begin
  inherited;
  SP_Epsilon := (AModel as TCustomModel).SP_Epsilon;
  for OutletIndex := 0 to Outlets.Count - 1 do
  begin
    AnOutlet := Outlets[OutletIndex].Outlet;
    for TimeIndex := 0 to AnOutlet.LakeTimes.Count - 1 do
    begin
      OutletTimeItem := AnOutlet.LakeTimes[TimeIndex];
      ClosestIndex := Times.IndexOfClosest(OutletTimeItem.StartTime);
      if ClosestIndex >= 0 then
      begin
        ExistingTime := Times[ClosestIndex];
        if Abs(ExistingTime-OutletTimeItem.StartTime) >  SP_Epsilon then
        begin
          Times.AddUnique(OutletTimeItem.StartTime);
        end;
      end;
      ClosestIndex := Times.IndexOfClosest(OutletTimeItem.EndTime);
      if ClosestIndex >= 0 then
      begin
        ExistingTime := Times[ClosestIndex];
        if Abs(ExistingTime-OutletTimeItem.EndTime) >  SP_Epsilon then
        begin
          Times.AddUnique(OutletTimeItem.EndTime);
        end;
      end;
      if (OutletTimeItem.StartTime < StartTestTime-SP_Epsilon) then
      begin
        StartRangeExtended := True;
      end;
      if (OutletTimeItem.EndTime > EndTestTime+SP_Epsilon) then
      begin
        EndRangeExtended := True;
      end;
    end;
  end;
end;

{ TLakeTableItemMf6 }

procedure TLakeTableItemMf6.Assign(Source: TPersistent);
var
  LKSource: TLakeTableItemMf6;
begin
  if Source is TLakeTableItemMf6 then
  begin
    LKSource := TLakeTableItemMf6(Source);
    Stage := LKSource.Stage;
    Volume := LKSource.Volume;
    SurfaceArea := LKSource.SurfaceArea;
    ExchangeArea := LKSource.ExchangeArea;
  end;
  inherited;
end;

constructor TLakeTableItemMf6.Create(Collection: TCollection);
var
  Index: Integer;
  Observer: TObserver;
begin
  inherited;
  CreateFormulaObjects;
  FObserverList := TObjectList<TObserver>.Create;
  for Index := 0 to FormulaCount - 1 do
  begin
    Observer := TObserver.Create(nil);
    FObserverList.Add(Observer);
  end;

end;

function TLakeTableItemMf6.CreateFormulaObject(
  Orientation: TDataSetOrientation): IFormulaObject;
begin
  result := CreateBlockFormulaObject(Orientation) as IFormulaObject;
  result.AddSubscriptionEvents(
    GlobalRemoveFormulaObjectSubscription,
    GlobalRestoreFormulaObjectSubscription, self);
end;

procedure TLakeTableItemMf6.CreateFormulaObjects;
begin
  FStage := CreateFormulaObject(dso3D);
  FVolume := CreateFormulaObject(dso3D);
  FSurfaceArea := CreateFormulaObject(dso3D);
  FExchangeArea := CreateFormulaObject(dso3D);
end;

destructor TLakeTableItemMf6.Destroy;
begin
  inherited;

  FObserverList.Free;
  RemoveFormulaObjects;
end;

function TLakeTableItemMf6.GetExchangeArea: string;
begin
  FExchangeArea.ScreenObject := ScreenObjectI;
  try
    Result := FExchangeArea.Formula;
  finally
    FExchangeArea.ScreenObject := nil;
  end;
  ResetItemObserver(ExchangeAreaEvapPosition);
end;

function TLakeTableItemMf6.GetObserver(Index: Integer): TObserver;
begin
  result := FObserverList[Index];
end;

function TLakeTableItemMf6.GetScreenObject: TObject;
begin
  result := nil;
end;

function TLakeTableItemMf6.GetStage: string;
begin
  FStage.ScreenObject := ScreenObjectI;
  try
    Result := FStage.Formula;
  finally
    FStage.ScreenObject := nil;
  end;
  ResetItemObserver(StagePosition);
end;

function TLakeTableItemMf6.GetSurfaceArea: string;
begin
  FSurfaceArea.ScreenObject := ScreenObjectI;
  try
    Result := FSurfaceArea.Formula;
  finally
    FSurfaceArea.ScreenObject := nil;
  end;
  ResetItemObserver(SurfaceAreaPosition);
end;

function TLakeTableItemMf6.GetVolume: string;
begin
  FVolume.ScreenObject := ScreenObjectI;
  try
    Result := FVolume.Formula;
  finally
    FVolume.ScreenObject := nil;
  end;
  ResetItemObserver(VolumePosition);
end;

function TLakeTableItemMf6.IsSame(AnotherItem: TOrderedItem): boolean;
var
  LKSource: TLakeTableItemMf6;
begin
  result := AnotherItem is TLakeTableItemMf6;
  if result then
  begin
    LKSource := TLakeTableItemMf6(AnotherItem);
    result := (Stage = LKSource.Stage)
      and (Volume = LKSource.Volume)
      and (SurfaceArea = LKSource.SurfaceArea)
      and (ExchangeArea = LKSource.ExchangeArea);
  end;
end;

procedure TLakeTableItemMf6.RemoveFormulaObjects;
var
  FormulaManager: TFormulaManager;
begin
  FormulaManager := frmGoPhast.PhastModel.FormulaManager;
  FormulaManager.Remove(FStage, GlobalRemoveFormulaObjectSubscription,
    GlobalRestoreFormulaObjectSubscription, self);
  FormulaManager.Remove(FVolume, GlobalRemoveFormulaObjectSubscription,
    GlobalRestoreFormulaObjectSubscription, self);
  FormulaManager.Remove(FSurfaceArea, GlobalRemoveFormulaObjectSubscription,
    GlobalRestoreFormulaObjectSubscription, self);
  FormulaManager.Remove(FExchangeArea, GlobalRemoveFormulaObjectSubscription,
    GlobalRestoreFormulaObjectSubscription, self);
end;

procedure TLakeTableItemMf6.ResetItemObserver(Index: integer);
var
  Observer: TObserver;
begin
  Observer := FObserverList[Index];
  Observer.UpToDate := True;
end;

procedure TLakeTableItemMf6.SetExchangeArea(const Value: string);
begin
  FExchangeArea.ScreenObject := ScreenObjectI;
  try
    UpdateFormulaBlocks(Value, ExchangeAreaEvapPosition, FExchangeArea);
  finally
    FExchangeArea.ScreenObject := nil;
  end;
end;

procedure TLakeTableItemMf6.SetStage(const Value: string);
begin
  FStage.ScreenObject := ScreenObjectI;
  try
    UpdateFormulaBlocks(Value, StagePosition, FStage);
  finally
    FStage.ScreenObject := nil;
  end;
end;

procedure TLakeTableItemMf6.SetSurfaceArea(const Value: string);
begin
  FSurfaceArea.ScreenObject := ScreenObjectI;
  try
    UpdateFormulaBlocks(Value, SurfaceAreaPosition, FSurfaceArea);
  finally
    FSurfaceArea.ScreenObject := nil;
  end;
end;

procedure TLakeTableItemMf6.SetVolume(const Value: string);
begin
  FVolume.ScreenObject := ScreenObjectI;
  try
    UpdateFormulaBlocks(Value, VolumePosition, FVolume);
  finally
    FVolume.ScreenObject := nil;
  end;
end;

procedure TLakeTableItemMf6.StopTalkingToAnyone;
var
  ObserverIndex: Integer;
begin
  for ObserverIndex := 0 to FObserverList.Count - 1 do
  begin
    FObserverList[ObserverIndex].StopTalkingToAnyone;
  end;
end;

{ TLakeTableMf6 }

function TLakeTableMf6.Add: TLakeTableItemMf6;
begin
  result := inherited Add as TLakeTableItemMf6;
end;

constructor TLakeTableMf6.Create(Model: IModelForTOrderedCollection);
begin
  inherited Create(TLakeTableItemMf6, Model);
end;

function TLakeTableMf6.GetItems(Index: Integer): TLakeTableItemMf6;
begin
  result := inherited Items[Index] as TLakeTableItemMf6;
end;

procedure TLakeTableMf6.SetItems(Index: Integer;
  const Value: TLakeTableItemMf6);
begin
  inherited Items[Index] := Value;
end;

procedure TLakeTableMf6.StopTalkingToAnyone;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].StopTalkingToAnyone;
  end;
end;

{ TLktGwtConcCollection }

constructor TLktGwtConcCollection.Create(Model: TBaseModel;
  AScreenObject: TObject; ParentCollection: TLakTimeCollection);
begin
  inherited Create(Model, AScreenObject, ParentCollection);
end;

initialization
  InitializeLakeObNames;
  InitializeLktObNames;
  
finalization 
  LakeObNames.Free;
  LktObNames.Free;

end.
