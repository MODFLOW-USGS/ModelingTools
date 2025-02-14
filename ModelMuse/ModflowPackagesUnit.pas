unit ModflowPackagesUnit;

interface

Uses Classes, ModflowPackageSelectionUnit, GoPhastTypes;

type
  TModflowPackages = class(TPersistent)
  private
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    FModel: TBaseModel;
    FChdBoundary: TChdPackage;
    FLpfPackage: TLpfSelection;
    FPcgPackage: TPcgSelection;
    FGhbBoundary: TGhbPackage;
    FWelPackage: TWellPackage;
    FRivPackage: TRivPackage;
    FDrnPackage: TDrnPackage;
    FDrtPackage: TDrtPackage;
    FRchPackage: TRchPackageSelection;
    FEvtPackage: TEvtPackageSelection;
    FEtsPackage: TEtsPackageSelection;
    FResPackage: TResPackageSelection;
    FLakPackage: TLakePackageSelection;
    FSfrPackage: TSfrPackageSelection;
    FUzfPackage: TUzfPackageSelection;
    FGmgPackage: TGmgPackageSelection;
    FSipPackage: TSIPPackageSelection;
    FDe4Package: TDE4PackageSelection;
    FHobPackage: THobPackageSelection;
    FHfbPackage: TModflowPackageSelection;
    FModPath: TModpathSelection;
    FChobPackage: TModflowPackageSelection;
    FRvobPackage: TModflowPackageSelection;
    FGbobPackage: TModflowPackageSelection;
    FDrobPackage: TModflowPackageSelection;
    FHufPackage: THufPackageSelection;
    FMnw2Package: TMultinodeWellSelection;
    FBcfPackage: TModflowPackageSelection;
    FSubPackage: TSubPackageSelection;
    FZoneBudget: TZoneBudgetSelect;
    FSwtPackage: TSwtPackageSelection;
    FHydmodPackage: THydPackageSelection;
    FUpwPackage: TUpwPackageSelection;
    FNwtPackage: TNwtPackageSelection;
    FMt3dBasic: TMt3dBasic;
    FMt3dmsGCGSolver: TMt3dmsGCGSolverPackage;
    FMt3dmsAdvection: TMt3dmsAdvection;
    FMt3dmsDispersion: TMt3dmsDispersion;
    FMt3dmsSourceSink: TMt3dmsSourceSinkMixing;
    FMt3dmsChemReaction: TMt3dmsChemReaction;
    FMt3dmsTransObs: TMt3dmsTransportObservations;
    FPcgnPackage: TPcgnSelection;
    FStrPackage: TStrPackageSelection;
    FStobPackage: TModflowPackageSelection;
    FFhbPackage: TFhbPackageSelection;
    FFarmProcess: TFarmProcess;
    FConduitFlowProcess: TConduitFlowProcess;
    FSwiPackage: TSwiPackage;
    FSwrPackage: TSwrPackage;
    FMnw1Package: TMnw1Package;
    FNpfPackage: TNpfPackage;
    FStoPackage: TStoPackage;
    FSmsPackage: TSmsPackageSelection;
    FRipPackage: TRipPackage;
    FMt3dUnsatTransport: TMt3dUztPackage;
    FSfrModflow6Package: TSfrModflow6PackageSelection;
    FMawPackage: TMawPackage;
    FGncPackage: TGncPackage;
    FMf6ObservationUtility: TMf6ObservationUtility;
    FLakMf6Package: TLakeMf6PackageSelection;
    FMvrPackage: TMvrPackage;
    FUzfMf6Package: TUzfMf6PackageSelection;
    FMt3dLkt: TMt3dLktPackage;
    FMt3dSft: TMt3dSftPackageSelection;
    FMt3dCts: TMt3dCtsPackageSelection;
    FCsubPackage: TCSubPackageSelection;
    FGwtPackages: TGwtPackageCollection;
    FGwtDispersionPackage: TGwtDispersionPackage;
    FGwtAdvectionPackage: TGwtAdvectionPackage;
    FGwtSsmPackage: TGWtSsmPackage;
    FGwtCncPackage: TGwtCncPackage;
    FGwtSrcPackage: TGwtSrcPackage;
    FGwtProcess: TGwtProcess;
    FFarmProcess4: TFarmProcess4;
    FFarmSurfaceWater4: TFarmProcess4SurfaceWater;
    FFarmSoil4: TFarmProcess4Soil;
    FFarmAllotments: TFarmProcess4Allotments;
    FFarmLandUse: TFarmProcess4LandUse;
    FFarmSalinityFlush: TFarmProcess4SalinityFlush;
    FFarmClimate4: TFarmProcess4Climate;
    FFarmWells4: TFarmProcess4Wells;
    FBuoyancyPackage: TBuoyancyPackage;
    FViscosityPackage: TViscosityPackage;
    FTvkPackage: TTvkPackage;
    FTvsPackage: TTvsPackage;
    FPrtModels: TPrtModels;
    FGweProcess: TGwtProcess;
    FGweCtpPackage: TGweCtpPackage;
    FGweEstPackage: TGweEstPackage;
    FGweConductionAndDispersionPackage: TGweConductionAndDispersionPackage;
    FGweEslPackage: TGweEslPackage;
    FGweAdvectionPackage: TGwtAdvectionPackage;
    FGweSsmPackage: TGwtSsmPackage;
    procedure SetChdBoundary(const Value: TChdPackage);
    procedure SetLpfPackage(const Value: TLpfSelection);
    procedure SetPcgPackage(const Value: TPcgSelection);
    procedure SetGhbBoundary(const Value: TGhbPackage);
    procedure SetWelPackage(const Value: TWellPackage);
    procedure SetRivPackage(const Value: TRivPackage);
    procedure SetDrnPackage(const Value: TDrnPackage);
    procedure SetDrtPackage(const Value: TDrtPackage);
    procedure SetRchPackage(const Value: TRchPackageSelection);
    procedure SetEvtPackage(const Value: TEvtPackageSelection);
    procedure SetEtsPackage(const Value: TEtsPackageSelection);
    procedure SetResPackage(const Value: TResPackageSelection);
    procedure SetLakPackage(const Value: TLakePackageSelection);
    procedure SetSfrPackage(const Value: TSfrPackageSelection);
    procedure SetUzfPackage(const Value: TUzfPackageSelection);
    procedure SetGmgPackage(const Value: TGmgPackageSelection);
    procedure SetSipPackage(const Value: TSIPPackageSelection);
    procedure SetDe4Package(const Value: TDE4PackageSelection);
    procedure SetHobPackage(const Value: THobPackageSelection);
    procedure SetHfbPackage(const Value: TModflowPackageSelection);
    procedure SetModPath(const Value: TModpathSelection);
    procedure SetChobPackage(const Value: TModflowPackageSelection);
    procedure SetDrobPackage(const Value: TModflowPackageSelection);
    procedure SetGbobPackage(const Value: TModflowPackageSelection);
    procedure SetRvobPackage(const Value: TModflowPackageSelection);
    procedure SetHufPackage(const Value: THufPackageSelection);
    procedure SetMnw2Package(const Value: TMultinodeWellSelection);
    procedure SetBcfPackage(const Value: TModflowPackageSelection);
    procedure SetSubPackage(const Value: TSubPackageSelection);
    procedure SetZoneBudget(const Value: TZoneBudgetSelect);
    procedure SetSwtPackage(const Value: TSwtPackageSelection);
    procedure SetHydmodPackage(const Value: THydPackageSelection);
    procedure SetUpwPackage(const Value: TUpwPackageSelection);
    procedure SetNwtPackage(const Value: TNwtPackageSelection);
    procedure SetMt3dBasic(const Value: TMt3dBasic);
    procedure SetMt3dmsGCGSolver(const Value: TMt3dmsGCGSolverPackage);
    procedure SetMt3dmsAdvection(const Value: TMt3dmsAdvection);
    procedure SetMt3dmsDispersion(const Value: TMt3dmsDispersion);
    procedure SetMt3dmsSourceSink(const Value: TMt3dmsSourceSinkMixing);
    procedure SetMt3dmsChemReaction(const Value: TMt3dmsChemReaction);
    procedure SetMt3dmsTransObs(const Value: TMt3dmsTransportObservations);
    procedure SetPcgnPackage(const Value: TPcgnSelection);
    procedure SetStrPackage(const Value: TStrPackageSelection);
    procedure SetStobPackage(const Value: TModflowPackageSelection);
    procedure SetFhbPackage(const Value: TFhbPackageSelection);
    procedure SetFarmProcess(const Value: TFarmProcess);
    procedure SetConduitFlowProcess(const Value: TConduitFlowProcess);
    procedure SetSwiPackage(const Value: TSwiPackage);
    procedure SetSwrPackage(const Value: TSwrPackage);
    procedure SetMnw1Package(const Value: TMnw1Package);
    procedure SetNpfPackage(const Value: TNpfPackage);
    procedure SetStoPackage(const Value: TStoPackage);
    procedure SetSmsPackage(const Value: TSmsPackageSelection);
    procedure SetRipPackage(const Value: TRipPackage);
    procedure SetMt3dUnsatTransport(const Value: TMt3dUztPackage);
    procedure SetSfrModflow6Package(const Value: TSfrModflow6PackageSelection);
    procedure SetMawPackage(const Value: TMawPackage);
    procedure SetGncPackage(const Value: TGncPackage);
    procedure SetMf6ObservationUtility(const Value: TMf6ObservationUtility);
    procedure SetLakMf6Package(const Value: TLakeMf6PackageSelection);
    procedure SetMvrPackage(const Value: TMvrPackage);
    procedure SetUzfMf6Package(const Value: TUzfMf6PackageSelection);
    procedure SetMt3dLkt(const Value: TMt3dLktPackage);
    procedure SetMt3dSft(const Value: TMt3dSftPackageSelection);
    procedure SetMt3dCts(const Value: TMt3dCtsPackageSelection);
    procedure SetCsubPackage(const Value: TCSubPackageSelection);
    procedure SetGwtPackges(const Value: TGwtPackageCollection);
    procedure SetGwtDispersionPackage(const Value: TGwtDispersionPackage);
    procedure SetGwtAdvectionPackage(const Value: TGwtAdvectionPackage);
    procedure SetGwtSsmPackage(const Value: TGWtSsmPackage);
    procedure SetGwtCncPackage(const Value: TGwtCncPackage);
    procedure SetGwtSrcPackage(const Value: TGwtSrcPackage);
    procedure SetGwtProcess(const Value: TGwtProcess);
    procedure SetFarmProcess4(const Value: TFarmProcess4);
    procedure SetFarmAllotments(const Value: TFarmProcess4Allotments);
    procedure SetFarmClimate4(const Value: TFarmProcess4Climate);
    procedure SetFarmLandUse(const Value: TFarmProcess4LandUse);
    procedure SetFarmSalinityFlush(const Value: TFarmProcess4SalinityFlush);
    procedure SetFarmSoil4(const Value: TFarmProcess4Soil);
    procedure SetFarmSurfaceWater4(const Value: TFarmProcess4SurfaceWater);
    procedure SetFarmWells4(const Value: TFarmProcess4Wells);
    procedure SetBuoyancyPackage(const Value: TBuoyancyPackage);
    procedure SetViscosityPackage(const Value: TViscosityPackage);
    procedure SetTvkPackage(const Value: TTvkPackage);
    procedure SetTvsPackage(const Value: TTvsPackage);
    procedure SetPrtModels(const Value: TPrtModels);
    procedure SetGweProcess(const Value: TGwtProcess);
    procedure SetGweAdvectionPackage(const Value: TGwtAdvectionPackage);
    procedure SetGweConductionAndDispersionPackage(
      const Value: TGweConductionAndDispersionPackage);
    procedure SetGweCtpPackage(const Value: TGweCtpPackage);
    procedure SetGweEslPackage(const Value: TGweEslPackage);
    procedure SetGweEstPackage(const Value: TGweEstPackage);
    procedure SetGweSsmPackage(const Value: TGwtSsmPackage);
  public
    procedure Assign(Source: TPersistent); override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property Model: TBaseModel read FModel;
    procedure Reset;
    // @name is used to set the progress bar limits when exporting
    // the MODFLOW input files.
    function SelectedModflowPackageCount: integer;
    procedure Loaded;
  published
    property ChdBoundary: TChdPackage read FChdBoundary write SetChdBoundary;
    property GhbBoundary: TGhbPackage read FGhbBoundary write SetGhbBoundary;
    property LpfPackage: TLpfSelection read FLpfPackage write SetLpfPackage;
    property PcgPackage: TPcgSelection read FPcgPackage write SetPcgPackage;
    property PcgnPackage: TPcgnSelection read FPcgnPackage write SetPcgnPackage;
    property WelPackage: TWellPackage read FWelPackage write SetWelPackage;
    property RivPackage: TRivPackage read FRivPackage write SetRivPackage;
    property DrnPackage: TDrnPackage read FDrnPackage write SetDrnPackage;
    property DrtPackage: TDrtPackage read FDrtPackage write SetDrtPackage;
    property RchPackage: TRchPackageSelection
      read FRchPackage write SetRchPackage;
    property EvtPackage: TEvtPackageSelection
      read FEvtPackage write SetEvtPackage;
    property EtsPackage: TEtsPackageSelection
      read FEtsPackage write SetEtsPackage;
    property ResPackage: TResPackageSelection
      read FResPackage write SetResPackage;
    property LakPackage: TLakePackageSelection
      read FLakPackage write SetLakPackage;
    property SfrPackage: TSfrPackageSelection
      read FSfrPackage write SetSfrPackage;
    property UzfPackage: TUzfPackageSelection
      read FUzfPackage write SetUzfPackage;
    property GmgPackage: TGmgPackageSelection
      read FGmgPackage write SetGmgPackage;
    property SipPackage: TSIPPackageSelection
      read FSipPackage write SetSipPackage;
    property De4Package: TDE4PackageSelection
      read FDe4Package write SetDe4Package;
    property HobPackage: THobPackageSelection
      read FHobPackage write SetHobPackage;
    property HfbPackage: TModflowPackageSelection
      read FHfbPackage write SetHfbPackage;
    property ModPath: TModpathSelection read FModPath write SetModPath;
    property ChobPackage: TModflowPackageSelection
      read FChobPackage write SetChobPackage;
    property DrobPackage: TModflowPackageSelection
      read FDrobPackage write SetDrobPackage;
    property GbobPackage: TModflowPackageSelection
      read FGbobPackage write SetGbobPackage;
    property RvobPackage: TModflowPackageSelection
      read FRvobPackage write SetRvobPackage;
    property StobPackage: TModflowPackageSelection
      read FStobPackage write SetStobPackage;
    property HufPackage: THufPackageSelection
      read FHufPackage write SetHufPackage;
    property Mnw2Package: TMultinodeWellSelection
      read FMnw2Package write SetMnw2Package;
    property BcfPackage: TModflowPackageSelection
      read FBcfPackage write SetBcfPackage;
    property SubPackage: TSubPackageSelection
      read FSubPackage write SetSubPackage;
    property ZoneBudget: TZoneBudgetSelect
      read FZoneBudget write SetZoneBudget;
    property SwtPackage: TSwtPackageSelection
      read FSwtPackage write SetSwtPackage;
    property HydmodPackage: THydPackageSelection
      read FHydmodPackage write SetHydmodPackage;
    property UpwPackage: TUpwPackageSelection
      read FUpwPackage write SetUpwPackage;
    property NwtPackage: TNwtPackageSelection
      read FNwtPackage write SetNwtPackage;
    property Mt3dBasic: TMt3dBasic read FMt3dBasic write SetMt3dBasic;
    property Mt3dmsGCGSolver: TMt3dmsGCGSolverPackage read FMt3dmsGCGSolver
      write SetMt3dmsGCGSolver;
    property Mt3dmsAdvection: TMt3dmsAdvection read FMt3dmsAdvection
      write SetMt3dmsAdvection;
    property Mt3dmsDispersion: TMt3dmsDispersion read FMt3dmsDispersion
      write SetMt3dmsDispersion;
    property Mt3dmsSourceSink: TMt3dmsSourceSinkMixing read FMt3dmsSourceSink
      write SetMt3dmsSourceSink;
    property Mt3dmsChemReact: TMt3dmsChemReaction read FMt3dmsChemReaction
      write SetMt3dmsChemReaction;
    property Mt3dmsTransObs: TMt3dmsTransportObservations read FMt3dmsTransObs
      write SetMt3dmsTransObs;
    property Mt3dUnsatTransport: TMt3dUztPackage read FMt3dUnsatTransport
      write SetMt3dUnsatTransport;
    property Mt3dLkt: TMt3dLktPackage read FMt3dLkt
      write SetMt3dLkt;
    property Mt3dSft: TMt3dSftPackageSelection read FMt3dSft write SetMt3dSft;
    property StrPackage: TStrPackageSelection read FStrPackage
      write SetStrPackage;
    property FhbPackage: TFhbPackageSelection read FFhbPackage
      write SetFhbPackage;
    property FarmProcess: TFarmProcess read FFarmProcess write SetFarmProcess;
    property ConduitFlowProcess: TConduitFlowProcess read FConduitFlowProcess
      write SetConduitFlowProcess;
    property SwiPackage: TSwiPackage read FSwiPackage write SetSwiPackage;
    property SwrPackage: TSwrPackage read FSwrPackage write SetSwrPackage;
    property Mnw1Package: TMnw1Package read FMnw1Package write SetMnw1Package;
    property NpfPackage: TNpfPackage read FNpfPackage write SetNpfPackage;
    property StoPackage: TStoPackage read FStoPackage write SetStoPackage;
    property SmsPackage: TSmsPackageSelection read FSmsPackage
      write SetSmsPackage;
    property RipPackage: TRipPackage read FRipPackage
      write SetRipPackage;
    property SfrModflow6Package: TSfrModflow6PackageSelection
      read FSfrModflow6Package write SetSfrModflow6Package;
    property MawPackage: TMawPackage read FMawPackage write SetMawPackage;
    property GncPackage: TGncPackage read FGncPackage write SetGncPackage;
    property Mf6ObservationUtility: TMf6ObservationUtility
      read FMf6ObservationUtility write SetMf6ObservationUtility;
    property LakMf6Package: TLakeMf6PackageSelection read FLakMf6Package
      write SetLakMf6Package;
    property MvrPackage: TMvrPackage read FMvrPackage write SetMvrPackage;
    property UzfMf6Package: TUzfMf6PackageSelection read FUzfMf6Package
      write SetUzfMf6Package;
    property Mt3dCts: TMt3dCtsPackageSelection read FMt3dCts write SetMt3dCts;
    property CSubPackage: TCSubPackageSelection read FCsubPackage
      write SetCsubPackage;
      // @name is used for the GWT (solute transport) in MODFLOW 6.
    property GwtProcess: TGwtProcess read FGwtProcess write SetGwtProcess;
    property GwtDispersionPackage: TGwtDispersionPackage
      read FGwtDispersionPackage write SetGwtDispersionPackage;
    property GwtAdvectionPackage: TGwtAdvectionPackage
      read FGwtAdvectionPackage write SetGwtAdvectionPackage;
    property GwtSsmPackage: TGWtSsmPackage read FGwtSsmPackage
      write SetGwtSsmPackage;
    property GwtCncPackage: TGwtCncPackage read FGwtCncPackage
      write SetGwtCncPackage;
    property GwtSrcPackage: TGwtSrcPackage read FGwtSrcPackage
      write SetGwtSrcPackage;
    property GwtPackages: TGwtPackageCollection read FGwtPackages
      write SetGwtPackges;
    property FarmProcess4: TFarmProcess4 read FFarmProcess4
      write SetFarmProcess4;
    property FarmSoil4: TFarmProcess4Soil read FFarmSoil4 write SetFarmSoil4;
    property FarmClimate4: TFarmProcess4Climate read FFarmClimate4
      write SetFarmClimate4;
    property FarmSurfaceWater4: TFarmProcess4SurfaceWater
      read FFarmSurfaceWater4 write SetFarmSurfaceWater4;
    property FarmWells4: TFarmProcess4Wells read FFarmWells4
      write SetFarmWells4;
    property FarmAllotments: TFarmProcess4Allotments read FFarmAllotments
      write SetFarmAllotments;
    property FarmLandUse: TFarmProcess4LandUse read FFarmLandUse write SetFarmLandUse;
    property FarmSalinityFlush: TFarmProcess4SalinityFlush read FFarmSalinityFlush
      write SetFarmSalinityFlush;
    property BuoyancyPackage: TBuoyancyPackage read FBuoyancyPackage
      write SetBuoyancyPackage;
    property ViscosityPackage: TViscosityPackage read FViscosityPackage
      write SetViscosityPackage;
    property TvkPackage: TTvkPackage read FTvkPackage write SetTvkPackage;
    property TvsPackage: TTvsPackage read FTvsPackage write SetTvsPackage;
    property PrtModels: TPrtModels read FPrtModels write SetPrtModels
    {$IFNDEF PRT}
      stored False
    {$ENDIF};
      // @name is used for the GWT (solute transport) in MODFLOW 6.
    property GweProcess: TGwtProcess read FGweProcess write SetGweProcess
    {$IFNDEF GWE}
      stored False
    {$ENDIF}
      ;
    property GweAdvectionPackage: TGwtAdvectionPackage read FGweAdvectionPackage
      write SetGweAdvectionPackage
    {$IFNDEF GWE}
      stored False
    {$ENDIF}
      ;
    property GweConductionAndDispersionPackage: TGweConductionAndDispersionPackage
        read FGweConductionAndDispersionPackage write SetGweConductionAndDispersionPackage
    {$IFNDEF GWE}
      stored False
    {$ENDIF}
      ;
    property GweEstPackage: TGweEstPackage read FGweEstPackage write SetGweEstPackage
    {$IFNDEF GWE}
      stored False
    {$ENDIF}
      ;
    property GweSsmPackage: TGwtSsmPackage read FGweSsmPackage write SetGweSsmPackage
     {$IFNDEF GWE}
      stored False
    {$ENDIF}
      ;
    property GweCtpPackage: TGweCtpPackage read FGweCtpPackage write SetGweCtpPackage
    {$IFNDEF GWE}
      stored False
    {$ENDIF}
      ;
    property GweEslPackage: TGweEslPackage read FGweEslPackage write SetGweEslPackage
    {$IFNDEF GWE}
      stored False
    {$ENDIF}
      ;

    // Assign, Create, Destroy, and Reset must be updated each time a new
    // package is added.
    // SelectedModflowPackageCount must be updated if the new package is a
    // MODFLOW package.
    // Sometimes Loaded must be updated too.
    // New items normally have "stored False" enclosed within IFNDEFs until
    // the new feature is ready for release.
  end;

resourcestring
  StrBoundaryCondition = 'Boundary conditions';
  StrSpecifiedHeadPackages = 'Specified head';
  StrLPF_Identifier = 'LPF: Layer Property Flow package';
  StrSFR_Identifier = 'SFR: Stream-Flow Routing package';
  StrUPW_Identifier = 'UPW: Upstream Weighting package';
  StrFlowPackages = 'Flow Packages';
  StrSpecifiedFlux = 'Specified flux';
  StrHeaddependentFlux = 'Head-dependent flux';
  StrSolver = 'Solvers';
  StrObservations = 'Observations';
  StrPostProcessors = 'Post processors';
  StrSubsidence = 'Subsidence';
  StrSurfaceWaterRoutin = 'Surface-Water Routing';
  StrGNCGhostNodeCorr = 'GNC: Ghost-Node Correction package';

  function BC_SpecHead: string;
  function BC_SpecifiedFlux: string;
  function BC_HeadDependentFlux: string;

implementation

uses
  frmGoPhastUnit, PhastModelUnit, OrderedCollectionUnit;

resourcestring
  StrHUF_Identifier = 'HUF2: Hydrogeologic Unit Flow package';
  StrOutput = 'Output';
  StrCHDTimeVariantSp = 'CHD: Time-Variant Specified-Head package';
  StrFHBPackage = 'FHB: Flow and Head Boundary package';
  StrPCGPreconditioned = 'PCG: Preconditioned Conjugate Gradient package';
  StrGHBGeneralHeadBo = 'GHB: General-Head Boundary package';
  StrWELWellPackage = 'WEL: Well package';
  StrRIVRiverPackage = 'RIV: River package';
  StrDRNDrainPackage = 'DRN: Drain package';
  StrDRTDrainReturnPa = 'DRT: Drain Return package';
  StrRCHRechargePackag = 'RCH: Recharge package';
  StrEVTEvapotranspirat = 'EVT: Evapotranspiration package';
  StrETSEvapotranspirat = 'ETS: Evapotranspiration Segments package';
  StrRESReservoirPacka = 'RES: Reservoir package';
  StrLAKLakePackage = 'LAK: Lake package';
  StrUZFUnsaturatedZon = 'UZF: Unsaturated-Zone Flow package';
  StrGMGGeometricMulti = 'GMG: Geometric Multigrid package';
  StrSIPStronglyImplic = 'SIP: Strongly Implicit Procedure package';
  StrDE4DirectSolverP = 'DE4: Direct Solver package';
  StrHOBHeadObservatio = 'HOB: Head Observation package';
  StrHFBHorizontalFlow = 'HFB: Horizontal Flow Barrier package';
  StrMODPATH = 'MODPATH';
  StrCHOBSpecifiedHead = 'CHOB: Specified-Head Flow Observation package';
  StrDROBDrainObservat = 'DROB: Drain Observation package';
  StrGBOBGeneralHeadB = 'GBOB: General-Head-Boundary Observation package';
  StrRVOBRiverObservat = 'RVOB: River Observation package';
  StrMNW2MultiNodeWel = 'MNW2: Multi-Node Well package';
  StrBCF6BlockCentered = 'BCF6: Block-Centered Flow package';
  StrSUBSubsidenceAnd = 'SUB: Subsidence and Aquifer-System Compaction Packa' +
  'ge';
  StrSWTSubsidenceAnd = 'SWT: Subsidence and Aquifer-System Compaction Packa' +
  'ge for Water-Table Aquifers';
  StrZONEBUDGET = 'ZONEBUDGET';
  StrHYDHYDMODPackage = 'HYD: HYDMOD package';
  StrNWTNewtonSolver = 'NWT: Newton Solver';
  StrBTNMT3DMSBasicTr = 'BTN: Basic Transport package';
  StrGCGGeneralizedCon = 'GCG: Generalized Conjugate Gradient Solver';
  StrADVAdvectionPacka = 'ADV: Advection package';
  StrDSPDispersionPack = 'DSP: Dispersion package';
  StrSSMSinkSourceM = 'SSM: Sink and Source Mixing package';
  StrRCTChemicalReacti = 'RCT: Chemical Reactions package';
  StrTOBTransportObser = 'TOB: Transport Observation package';
  StrUztUnsatTransport = 'UZT: Unsaturated-Zone Transport package (MT3D-USGS)';
  StrPCGNPreconditioned = 'PCGN: Preconditioned Conjugate Gradient Solver wi' +
  'th Improved Nonlinear Control';
  StrSTRStreamPackage = 'STR: Stream package';
  StrSTOBStreamObserva = 'STOB: Stream Observation package';
  StrFarmProcess = 'FMP: Farm Process V3';
  StrFarmProcessClassification = 'Farm process';
  StrCFPConduitFlowPr = 'CFP: Conduit Flow process';
  StrConduitFlowProcess = 'Conduit Flow process';
  StrSWI2SeawaterIntru = 'SWI2: Seawater Intrusion package';
  StrSurfaceWaterRoutin2 = 'SWR: Surface-Water Routing process';
  StrMNW1MultiNodeWel = 'MNW1: Multi-Node Well package';
  StrNPFNodePropertyF = 'NPF: Node Property Flow package';
  StrSTOStoragePackage = 'STO: Storage package';
  StrRIPRiparianEvapot = 'RIP: Riparian Evapotranspiration Package';
  StrMawPackage = 'MAW: Multi-Aquifer Well package';
  StrSFRMODFLOW6Strea = 'SFR: MODFLOW-6 Stream Flow Routing package';
  StrOBSObservationUtil = 'OBS: Observation Utility';
  StrLAKLakePackageMf6 = 'LAK: Lake package for MODFLOW 6';
  StrMVRWaterMoverPac = 'MVR: Water Mover package';
  StrUZFUnsaturatedZonMf6 = 'UZF6: Unsaturated-Zone Flow Package for MODFLOW 6';
  StrLKTLakeTransport = 'LKT: Lake Transport Package';
  StrSFTStreamFlowTra = 'SFT: Stream Flow Transport Package';
  StrCTSContaminantTre = 'CTS: Contaminant Treatment System Package';
  StrCSUBSkeletalStora = 'CSUB: Skeletal Storage, Compaction, and Subsidence' +
  ' Package';
  StrGWTDispersionPacka = 'DSP: GWT Dispersion Package';
  StrADVGWTAdvectionP = 'ADV: GWT Advection Package';
  StrSSMGWTSourceAnd = 'SSM: GWT Source and Sink Mixing Package';
  StrCNCGWTConstantCo = 'CNC: GWT Constant Concentration Package';
  StrSRCGWTMassSource = 'SRC: GWT Mass Source Loading Package';
  StrGWTGroundwaterTra = 'GWT: Groundwater Transport Process';
  StrFMPFarmProcessV4 = 'FMP: Farm Process V4';
  StrSOILFarmProcessV = 'SOIL: Farm Process V4 Soil Options';
  StrCLIMATEFarmProces = 'CLIMATE: Farm Process V4 Climate Options';
  StrSURFACEWATERFarm = 'SURFACE_WATER: Farm Process V4 Surface Water Option' +
  's';
  StrSUPPLYWELLFarmPr = 'SUPPLY_WELL: Farm Process V4 Supply Well Options';
  StrALLOTMENTSFarmPro = 'ALLOTMENTS: Farm Process V4 Allotment Options';
  StrLANDUSEFarmProce = 'LAND_USE: Farm Process V4 Land Use Options';
  StrSALINITYFLUSHIRRIG = 'SALINITY_FLUSH_IRRIGATION: Farm Process V4 Salini' +
  'ty Flush Irrigation Options';
  StrBUYBuoyancyPackag = 'BUY: Buoyancy Package';
  StrVSCViscosityPacka = 'VSC: Viscosity Package';
  StrTVKTimeVaryingHy = 'TVK: Time-Varying Hydraulic Conductivity Package';
  StrTVSTimeVaryingSt = 'TVS: Time-Varying Storage Package';
  StrGWEGroundwaterEne = 'GWE: Groundwater Energy Process';
  StrADVGWEAdvectionP = 'ADV: GWE Advection Package';
  StrCNDGWEConduction = 'CND: GWE Conduction and Dispersion Package';
  StrESTGWEEnergyStor = 'EST: GWE Energy Storage and Transfer Package';
  StrSSMGWESourceAnd = 'SSM: GWE Source and Sink Mixing Package';
  StrCTPGWEConstantTe = 'CTP: GWE Constant Temperature Package';
  StrESLGWEEnergySour = 'ESL: GWE Energy Source Loading Package';
//  StrGroundwaterTranspor = 'GWT: Groundwater Transport';


{ TModflowPackages }

procedure TModflowPackages.Assign(Source: TPersistent);
var
  SourcePackages: TModflowPackages;
begin
  if Source is TModflowPackages then
  begin
    SourcePackages := TModflowPackages(Source);
    ChdBoundary := SourcePackages.ChdBoundary;
    LpfPackage := SourcePackages.LpfPackage;
    PcgPackage := SourcePackages.PcgPackage;
    PcgnPackage := SourcePackages.PcgnPackage;
    GhbBoundary := SourcePackages.GhbBoundary;
    WelPackage := SourcePackages.WelPackage;
    RivPackage := SourcePackages.RivPackage;
    DrnPackage := SourcePackages.DrnPackage;
    DrtPackage := SourcePackages.DrtPackage;
    RchPackage := SourcePackages.RchPackage;
    EvtPackage := SourcePackages.EvtPackage;
    EtsPackage := SourcePackages.EtsPackage;
    ResPackage := SourcePackages.ResPackage;
    LakPackage := SourcePackages.LakPackage;
    SfrPackage := SourcePackages.SfrPackage;
    StrPackage := SourcePackages.StrPackage;
    UzfPackage := SourcePackages.UzfPackage;
    GmgPackage := SourcePackages.GmgPackage;
    SipPackage := SourcePackages.SipPackage;
    De4Package := SourcePackages.De4Package;
    HobPackage := SourcePackages.HobPackage;
    HfbPackage := SourcePackages.HfbPackage;
    Modpath    := SourcePackages.Modpath;
    ChobPackage := SourcePackages.ChobPackage;
    DrobPackage := SourcePackages.DrobPackage;
    GbobPackage := SourcePackages.GbobPackage;
    RvobPackage := SourcePackages.RvobPackage;
    StobPackage := SourcePackages.StobPackage;
    HufPackage := SourcePackages.HufPackage;
    Mnw2Package := SourcePackages.Mnw2Package;
    BcfPackage := SourcePackages.BcfPackage;
    SubPackage := SourcePackages.SubPackage;
    ZoneBudget := SourcePackages.ZoneBudget;
    SwtPackage := SourcePackages.SwtPackage;
    HydmodPackage := SourcePackages.HydmodPackage;
    UpwPackage := SourcePackages.UpwPackage;
    NwtPackage := SourcePackages.NwtPackage;
    Mt3dBasic := SourcePackages.Mt3dBasic;
    Mt3dmsGCGSolver := SourcePackages.Mt3dmsGCGSolver;
    Mt3dmsAdvection := SourcePackages.Mt3dmsAdvection;
    Mt3dmsDispersion := SourcePackages.Mt3dmsDispersion;
    Mt3dmsSourceSink := SourcePackages.Mt3dmsSourceSink;
    Mt3dmsChemReact := SourcePackages.Mt3dmsChemReact;
    Mt3dmsTransObs := SourcePackages.Mt3dmsTransObs;
    Mt3dUnsatTransport := SourcePackages.Mt3dUnsatTransport;
    FhbPackage := SourcePackages.FhbPackage;
    FarmProcess := SourcePackages.FarmProcess;
    ConduitFlowProcess := SourcePackages.ConduitFlowProcess;
    SwiPackage := SourcePackages.SwiPackage;
    SwrPackage := SourcePackages.SwrPackage;
    Mnw1Package := SourcePackages.Mnw1Package;
    NpfPackage := SourcePackages.NpfPackage;
    StoPackage := SourcePackages.StoPackage;
    SmsPackage := SourcePackages.SmsPackage;
    RipPackage := SourcePackages.RipPackage;
    SfrModflow6Package := SourcePackages.SfrModflow6Package;
    MawPackage := SourcePackages.MawPackage;
    GncPackage := SourcePackages.GncPackage;
    Mf6ObservationUtility := SourcePackages.Mf6ObservationUtility;
    LakMf6Package := SourcePackages.LakMf6Package;
    MvrPackage := SourcePackages.MvrPackage;
    UzfMf6Package := SourcePackages.UzfMf6Package;
    Mt3dLkt := SourcePackages.Mt3dLkt;
    Mt3dSft := SourcePackages.Mt3dSft;
    Mt3dCts := SourcePackages.Mt3dCts;
    CsubPackage := SourcePackages.CsubPackage;
    GwtProcess := SourcePackages.GwtProcess;
    GwtDispersionPackage := SourcePackages.GwtDispersionPackage;
    GwtAdvectionPackage := SourcePackages.GwtAdvectionPackage;
    GwtSsmPackage := SourcePackages.GwtSsmPackage;
    GwtCncPackage := SourcePackages.GwtCncPackage;
    GwtSrcPackage := SourcePackages.GwtSrcPackage;
    GwtPackages := SourcePackages.GwtPackages;

    GweProcess := SourcePackages.GweProcess;
    GweAdvectionPackage := SourcePackages.GweAdvectionPackage;
    GweConductionAndDispersionPackage := SourcePackages.GweConductionAndDispersionPackage;
    GweEstPackage := SourcePackages.GweEstPackage;
    GweSsmPackage := SourcePackages.GweSsmPackage;
    GweCtpPackage := SourcePackages.GweCtpPackage;
    GweEslPackage := SourcePackages.GweEslPackage;

    FarmProcess4 := SourcePackages.FarmProcess4;
    FarmSoil4 := SourcePackages.FarmSoil4;
    FarmClimate4 := SourcePackages.FarmClimate4;
    FarmSurfaceWater4 := SourcePackages.FarmSurfaceWater4;
    FarmWells4 := SourcePackages.FarmWells4;
    FarmAllotments := SourcePackages.FarmAllotments;
    FarmLandUse := SourcePackages.FarmLandUse;
    FarmSalinityFlush := SourcePackages.FarmSalinityFlush;
    BuoyancyPackage := SourcePackages.BuoyancyPackage;
    ViscosityPackage := SourcePackages.ViscosityPackage;
    TvkPackage := SourcePackages.TvkPackage;
    TvsPackage := SourcePackages.TvsPackage;
    PrtModels := SourcePackages.PrtModels;

  end
  else
  begin
    inherited;
  end;
end;

constructor TModflowPackages.Create(Model: TBaseModel);
begin
  inherited Create;
  FModel := Model;

  FChdBoundary := TChdPackage.Create(Model);
  FChdBoundary.PackageIdentifier := StrCHDTimeVariantSp;
  FChdBoundary.Classification := BC_SpecHead;

  FLpfPackage := TLpfSelection.Create(Model);
  FLpfPackage.PackageIdentifier := StrLPF_Identifier;
  FLpfPackage.Classification := StrFlowPackages;
  FLpfPackage.SelectionType := stRadioButton;

  FHufPackage := THufPackageSelection.Create(Model);
  FHufPackage.PackageIdentifier := StrHUF_Identifier;
  FHufPackage.Classification := StrFlowPackages;
  FHufPackage.SelectionType := stRadioButton;

  FPcgPackage := TPcgSelection.Create(Model);
  FPcgPackage.PackageIdentifier :=
    StrPCGPreconditioned;
  FPcgPackage.Classification := StrSolver;
  FPcgPackage.SelectionType := stRadioButton;

  FPcgnPackage := TPcgnSelection.Create(Model);
  FPcgnPackage.PackageIdentifier :=
    StrPCGNPreconditioned;
  FPcgnPackage.Classification := StrSolver;
  FPcgnPackage.SelectionType := stRadioButton;

  FGhbBoundary := TGhbPackage.Create(Model);
  FGhbBoundary.PackageIdentifier := StrGHBGeneralHeadBo;
  FGhbBoundary.Classification := BC_HeadDependentFlux;

  FWelPackage := TWellPackage.Create(Model);
  FWelPackage.PackageIdentifier := StrWELWellPackage;
  FWelPackage.Classification := BC_SpecifiedFlux;

  FRivPackage := TRivPackage.Create(Model);
  FRivPackage.PackageIdentifier := StrRIVRiverPackage;
  FRivPackage.Classification := BC_HeadDependentFlux;

  FDrnPackage := TDrnPackage.Create(Model);
  FDrnPackage.PackageIdentifier := StrDRNDrainPackage;
  FDrnPackage.Classification := BC_HeadDependentFlux;

  FDrtPackage := TDrtPackage.Create(Model);
  FDrtPackage.PackageIdentifier := StrDRTDrainReturnPa;
  FDrtPackage.Classification := BC_HeadDependentFlux;

  FRchPackage := TRchPackageSelection.Create(Model);
  FRchPackage.PackageIdentifier := StrRCHRechargePackag;
  FRchPackage.Classification := BC_SpecifiedFlux;

  FFhbPackage := TFhbPackageSelection.Create(Model);
  FFhbPackage.PackageIdentifier := StrFHBPackage;
  FFhbPackage.Classification := BC_SpecifiedFlux;
  FFhbPackage.AlternativeClassification := BC_SpecHead;
  FFhbPackage.SelectionType := stCheckBox;

  FEvtPackage := TEvtPackageSelection.Create(Model);
  FEvtPackage.PackageIdentifier := StrEVTEvapotranspirat;
  FEvtPackage.Classification := BC_HeadDependentFlux;

  FEtsPackage := TEtsPackageSelection.Create(Model);
  FEtsPackage.PackageIdentifier := StrETSEvapotranspirat;
  FEtsPackage.Classification := BC_HeadDependentFlux;

  FResPackage := TResPackageSelection.Create(Model);
  FResPackage.PackageIdentifier := StrRESReservoirPacka;
  FResPackage.Classification := BC_HeadDependentFlux;

  FLakPackage := TLakePackageSelection.Create(Model);
  LakPackage.PackageIdentifier := StrLAKLakePackage;
  LakPackage.Classification := BC_HeadDependentFlux;

  FSfrPackage := TSfrPackageSelection.Create(Model as TCustomModel);
  SfrPackage.PackageIdentifier := StrSFR_Identifier;
  SfrPackage.Classification := BC_HeadDependentFlux;

  FStrPackage := TStrPackageSelection.Create(Model);
  FStrPackage.PackageIdentifier := StrSTRStreamPackage;
  FStrPackage.Classification := BC_HeadDependentFlux;

  FUzfPackage := TUzfPackageSelection.Create(Model);
  UzfPackage.PackageIdentifier := StrUZFUnsaturatedZon;
  UzfPackage.Classification := StrFlowPackages;

  FGmgPackage := TGmgPackageSelection.Create(Model);
  FGmgPackage.PackageIdentifier := StrGMGGeometricMulti;
  FGmgPackage.Classification := StrSolver;
  FGmgPackage.SelectionType := stRadioButton;

  FSipPackage := TSIPPackageSelection.Create(Model);
  FSipPackage.PackageIdentifier := StrSIPStronglyImplic;
  FSipPackage.Classification := StrSolver;
  FSipPackage.SelectionType := stRadioButton;

  FDe4Package := TDE4PackageSelection.Create(Model);
  FDe4Package.PackageIdentifier := StrDE4DirectSolverP;
  FDe4Package.Classification := StrSolver;
  FDe4Package.SelectionType := stRadioButton;

  FHobPackage := THobPackageSelection.Create(Model);
  FHobPackage.PackageIdentifier := StrHOBHeadObservatio;
  FHobPackage.Classification := StrObservations;
  FHobPackage.SelectionType := stCheckBox;

  FHfbPackage := TModflowPackageSelection.Create(Model);
  FHfbPackage.PackageIdentifier := StrHFBHorizontalFlow;
  FHfbPackage.Classification := StrFlowPackages;
  FHfbPackage.SelectionType := stCheckBox;

  FModPath := TModpathSelection.Create(Model);
  FModPath.PackageIdentifier := StrMODPATH;
  FModPath.Classification := StrPostProcessors;
  FModPath.SelectionType := stCheckBox;

  FChobPackage := TModflowPackageSelection.Create(Model);
  FChobPackage.PackageIdentifier := StrCHOBSpecifiedHead;
  FChobPackage.Classification := StrObservations;
  FChobPackage.SelectionType := stCheckBox;

  FDrobPackage := TModflowPackageSelection.Create(Model);
  FDrobPackage.PackageIdentifier := StrDROBDrainObservat;
  FDrobPackage.Classification := StrObservations;
  FDrobPackage.SelectionType := stCheckBox;

  FGbobPackage := TModflowPackageSelection.Create(Model);
  FGbobPackage.PackageIdentifier := StrGBOBGeneralHeadB;
  FGbobPackage.Classification := StrObservations;
  FGbobPackage.SelectionType := stCheckBox;

  FRvobPackage := TModflowPackageSelection.Create(Model);
  FRvobPackage.PackageIdentifier := StrRVOBRiverObservat;
  FRvobPackage.Classification := StrObservations;
  FRvobPackage.SelectionType := stCheckBox;

  FStobPackage := TModflowPackageSelection.Create(Model);
  FStobPackage.PackageIdentifier := StrSTOBStreamObserva;
  FStobPackage.Classification := StrObservations;
  FStobPackage.SelectionType := stCheckBox;

  FMnw2Package := TMultinodeWellSelection.Create(Model);
  FMnw2Package.PackageIdentifier := StrMNW2MultiNodeWel;
  FMnw2Package.Classification := BC_HeadDependentFlux;
  FMnw2Package.SelectionType := stCheckBox;

  FBcfPackage := TModflowPackageSelection.Create(Model);
  FBcfPackage.PackageIdentifier := StrBCF6BlockCentered;
  FBcfPackage.Classification := StrFlowPackages;
  FBcfPackage.SelectionType := stRadioButton;

  FSubPackage := TSubPackageSelection.Create(Model);
  FSubPackage.PackageIdentifier := StrSUBSubsidenceAnd;
  FSubPackage.Classification := StrSubsidence;
  FSubPackage.SelectionType := stCheckBox;

  FSwtPackage := TSwtPackageSelection.Create(Model);
  FSwtPackage.PackageIdentifier := StrSWTSubsidenceAnd;
  FSwtPackage.Classification := StrSubsidence;
  FSwtPackage.SelectionType := stCheckBox;

  FZoneBudget := TZoneBudgetSelect.Create(Model);
  FZoneBudget.PackageIdentifier := StrZONEBUDGET;
  FZoneBudget.Classification := StrPostProcessors;
  FZoneBudget.SelectionType := stCheckBox;

  FHydmodPackage := THydPackageSelection.Create(Model);
  FHydmodPackage.PackageIdentifier := StrHYDHYDMODPackage;
  FHydmodPackage.Classification := StrOutput;
  FHydmodPackage.SelectionType := stCheckBox;

  FUpwPackage := TUpwPackageSelection.Create(Model);
  FUpwPackage.PackageIdentifier := StrUPW_Identifier;
  FUpwPackage.Classification := StrFlowPackages;
  FUpwPackage.SelectionType := stRadioButton;

  FNwtPackage := TNwtPackageSelection.Create(Model);
  FNwtPackage.PackageIdentifier := StrNWTNewtonSolver;
  FNwtPackage.Classification := StrSolver;
  FNwtPackage.SelectionType := stRadioButton;

  FMt3dBasic := TMt3dBasic.Create(Model);
  FMt3dBasic.PackageIdentifier := StrBTNMT3DMSBasicTr;
  FMt3dBasic.Classification := StrMt3dClassification;
  FMt3dBasic.SelectionType := stCheckBox;

  FMt3dmsGCGSolver := TMt3dmsGCGSolverPackage.Create(Model);
  FMt3dmsGCGSolver.PackageIdentifier := StrGCGGeneralizedCon;
  FMt3dmsGCGSolver.Classification := StrMt3dClassification;
  FMt3dmsGCGSolver.SelectionType := stCheckBox;

  FMt3dmsAdvection := TMt3dmsAdvection.Create(Model);
  FMt3dmsAdvection.PackageIdentifier := StrADVAdvectionPacka;
  FMt3dmsAdvection.Classification := StrMt3dClassification;
  FMt3dmsAdvection.SelectionType := stCheckBox;

  FMt3dmsDispersion := TMt3dmsDispersion.Create(Model);
  FMt3dmsDispersion.PackageIdentifier := StrDSPDispersionPack;
  FMt3dmsDispersion.Classification := StrMt3dClassification;
  FMt3dmsDispersion.SelectionType := stCheckBox;

  FMt3dmsSourceSink := TMt3dmsSourceSinkMixing.Create(Model);
  FMt3dmsSourceSink.PackageIdentifier := StrSSMSinkSourceM;
  FMt3dmsSourceSink.Classification := StrMt3dClassification;
  FMt3dmsSourceSink.SelectionType := stCheckBox;

  FMt3dmsChemReaction := TMt3dmsChemReaction.Create(Model);
  FMt3dmsChemReaction.PackageIdentifier := StrRCTChemicalReacti;
  FMt3dmsChemReaction.Classification := StrMt3dClassification;
  FMt3dmsChemReaction.SelectionType := stCheckBox;

  FMt3dmsTransObs := TMt3dmsTransportObservations.Create(Model);
  FMt3dmsTransObs.PackageIdentifier := StrTOBTransportObser;
  FMt3dmsTransObs.Classification := StrMt3dClassification;
  FMt3dmsTransObs.SelectionType := stCheckBox;

  FMt3dUnsatTransport := TMt3dUztPackage.Create(Model);
  FMt3dUnsatTransport.PackageIdentifier := StrUztUnsatTransport;
  FMt3dUnsatTransport.Classification := StrMt3dClassification;
  FMt3dUnsatTransport.SelectionType := stCheckBox;

  FMt3dLkt := TMt3dLktPackage.Create(Model);
  FMt3dLkt.PackageIdentifier := StrLKTLakeTransport;
  FMt3dLkt.Classification := StrMt3dClassification;
  FMt3dLkt.SelectionType := stCheckBox;

  FMt3dSft := TMt3dSftPackageSelection.Create(Model);
  FMt3dSft.PackageIdentifier := StrSFTStreamFlowTra;;
  FMt3dSft.Classification := StrMt3dClassification;
  FMt3dSft.SelectionType := stCheckBox;

  FMt3dCts := TMt3dCtsPackageSelection.Create(Model);
  FMt3dCts.PackageIdentifier := StrCTSContaminantTre;;
  FMt3dCts.Classification := StrMt3dClassification;
  FMt3dCts.SelectionType := stCheckBox;

  FFarmProcess := TFarmProcess.Create(Model);
  FFarmProcess.PackageIdentifier := StrFarmProcess;
  FFarmProcess.Classification := StrFarmProcessClassification;
  FFarmProcess.SelectionType := stCheckBox;

  FConduitFlowProcess := TConduitFlowProcess.Create(Model);
  FConduitFlowProcess.PackageIdentifier := StrCFPConduitFlowPr;
  FConduitFlowProcess.Classification := StrConduitFlowProcess;
  FConduitFlowProcess.SelectionType := stCheckBox;

  FSwiPackage := TSwiPackage.Create(Model);
  FSwiPackage.PackageIdentifier := StrSWI2SeawaterIntru;
  FSwiPackage.Classification := StrFlowPackages;
  FSwiPackage.SelectionType := stCheckBox;

  FSwrPackage := TSwrPackage.Create(Model);
  FSwrPackage.PackageIdentifier := StrSurfaceWaterRoutin2;
  FSwrPackage.Classification := StrSurfaceWaterRoutin;
  FSwrPackage.SelectionType := stCheckBox;

  FMnw1Package := TMnw1Package.Create(Model);
  FMnw1Package.PackageIdentifier := StrMNW1MultiNodeWel;
  FMnw1Package.Classification := BC_HeadDependentFlux;
  FMnw1Package.SelectionType := stCheckBox;

  FMawPackage := TMawPackage.Create(Model);
  FMawPackage.PackageIdentifier := StrMawPackage;
  FMawPackage.Classification := BC_HeadDependentFlux;
  FMawPackage.SelectionType := stCheckBox;

  FNpfPackage := TNpfPackage.Create(Model);
  FNpfPackage.PackageIdentifier := StrNPFNodePropertyF;
  FNpfPackage.Classification := StrFlowPackages;
  FNpfPackage.SelectionType := stRadioButton;

  FStoPackage := TStoPackage.Create(Model);
  FStoPackage.PackageIdentifier := StrSTOStoragePackage;
  FStoPackage.Classification := StrFlowPackages;
  FStoPackage.SelectionType := stCheckBox;

  FSmsPackage := TSmsPackageSelection.Create(Model);
  FSmsPackage.PackageIdentifier := StrSMSSparseMatrixS;
  FSmsPackage.Classification := StrSolver;
  FSmsPackage.SelectionType := stRadioButton;

  FRipPackage := TRipPackage.Create(Model);
  FRipPackage.PackageIdentifier := StrRIPRiparianEvapot;
  FRipPackage.Classification := BC_HeadDependentFlux;
  FRipPackage.SelectionType := stCheckBox;

  FSfrModflow6Package := TSfrModflow6PackageSelection.Create(Model);
  FSfrModflow6Package.PackageIdentifier := StrSFRMODFLOW6Strea;
  FSfrModflow6Package.Classification := BC_HeadDependentFlux;
  FSfrModflow6Package.SelectionType := stCheckBox;

  FGncPackage := TGncPackage.Create(Model);
  FGncPackage.PackageIdentifier := StrGNCGhostNodeCorr;
  FGncPackage.Classification := StrFlowPackages;
  FGncPackage.SelectionType := stCheckBox;

  FMf6ObservationUtility := TMf6ObservationUtility.Create(Model);
  FMf6ObservationUtility.PackageIdentifier := StrOBSObservationUtil;
  FMf6ObservationUtility.Classification := StrObservations;
  FMf6ObservationUtility.SelectionType := stCheckBox;

  FLakMf6Package := TLakeMf6PackageSelection.Create(Model);
  FLakMf6Package.PackageIdentifier := StrLAKLakePackageMf6;
  FLakMf6Package.Classification := BC_HeadDependentFlux;
  FLakMf6Package.SelectionType := stCheckBox;

  FMvrPackage := TMvrPackage.Create(Model);
  FMvrPackage.PackageIdentifier := StrMVRWaterMoverPac;
  FMvrPackage.Classification := BC_HeadDependentFlux;
  FMvrPackage.SelectionType := stCheckBox;

  FUzfMf6Package := TUzfMf6PackageSelection.Create(Model);
  FUzfMf6Package.PackageIdentifier := StrUZFUnsaturatedZonMf6;
  FUzfMf6Package.Classification := BC_HeadDependentFlux;
  FUzfMf6Package.SelectionType := stCheckBox;

  FCsubPackage := TCSubPackageSelection.Create(Model);
  FCsubPackage.PackageIdentifier := StrCSUBSkeletalStora;
  FCsubPackage.Classification := StrSubsidence;
  FCsubPackage.SelectionType := stCheckBox;

  FGwtProcess := TGwtProcess.Create(Model);
  FGwtProcess.PackageIdentifier := StrGWTGroundwaterTra;
  FGwtProcess.Classification := StrGwtClassification;
  FGwtProcess.SelectionType := stCheckBox;

  FGwtDispersionPackage := TGwtDispersionPackage.Create(Model);
  FGwtDispersionPackage.PackageIdentifier := StrGWTDispersionPacka;
  FGwtDispersionPackage.Classification := StrGwtClassification;
  FGwtDispersionPackage.SelectionType := stCheckBox;

  FGwtAdvectionPackage := TGwtAdvectionPackage.Create(Model);
  FGwtAdvectionPackage.PackageIdentifier := StrADVGWTAdvectionP;
  FGwtAdvectionPackage.Classification := StrGwtClassification;
  FGwtAdvectionPackage.SelectionType := stCheckBox;

  FGwtSsmPackage := TGWtSsmPackage.Create(Model);
  FGwtSsmPackage.PackageIdentifier := StrSSMGWTSourceAnd;
  FGwtSsmPackage.Classification := StrGwtClassification;
  FGwtSsmPackage.SelectionType := stCheckBox;

  FGwtCncPackage := TGwtCncPackage.Create(Model);
  FGwtCncPackage.PackageIdentifier := StrCNCGWTConstantCo;
  FGwtCncPackage.Classification := StrGwtClassification;
  FGwtCncPackage.SelectionType := stCheckBox;

  FGwtSrcPackage := TGwtSrcPackage.Create(Model);
  FGwtSrcPackage.PackageIdentifier := StrSRCGWTMassSource;
  FGwtSrcPackage.Classification := StrGwtClassification;
  FGwtSrcPackage.SelectionType := stCheckBox;

  FGwtPackages := TGwtPackageCollection.Create(Model);



  FGweProcess := TGwtProcess.Create(Model);
  FGweProcess.PackageIdentifier := StrGWEGroundwaterEne;
  FGweProcess.Classification := StrGweClassification;
  FGweProcess.SelectionType := stCheckBox;

  FGweAdvectionPackage := TGwtAdvectionPackage.Create(Model);
  FGweAdvectionPackage.PackageIdentifier := StrADVGWEAdvectionP;
  FGweAdvectionPackage.Classification := StrGweClassification;
  FGweAdvectionPackage.SelectionType := stCheckBox;

  FGweConductionAndDispersionPackage := TGweConductionAndDispersionPackage.Create(Model);
  FGweConductionAndDispersionPackage.PackageIdentifier := StrCNDGWEConduction;
  FGweConductionAndDispersionPackage.Classification := StrGweClassification;
  FGweConductionAndDispersionPackage.SelectionType := stCheckBox;

  FGweEstPackage := TGweEstPackage.Create(Model);
  FGweEstPackage.PackageIdentifier := StrESTGWEEnergyStor;
  FGweEstPackage.Classification := StrGweClassification;
  FGweEstPackage.SelectionType := stCheckBox;

  FGweSsmPackage := TGwtSsmPackage.Create(Model);
  FGweSsmPackage.PackageIdentifier := StrSSMGWESourceAnd;
  FGweSsmPackage.Classification := StrGweClassification;
  FGweSsmPackage.SelectionType := stCheckBox;

  FGweCtpPackage := TGweCtpPackage.Create(Model);
  FGweCtpPackage.PackageIdentifier := StrCTPGWEConstantTe;
  FGweCtpPackage.Classification := StrGweClassification;
  FGweCtpPackage.SelectionType := stCheckBox;

  FGweEslPackage := TGweEslPackage.Create(Model);
  FGweEslPackage.PackageIdentifier := StrESLGWEEnergySour;
  FGweEslPackage.Classification := StrGweClassification;
  FGweEslPackage.SelectionType := stCheckBox;


  FFarmProcess4 := TFarmProcess4.Create(Model);
  FFarmProcess4.PackageIdentifier := StrFMPFarmProcessV4;
  FFarmProcess4.Classification := StrFarmProcessClassification;
  FFarmProcess4.SelectionType := stCheckBox;

  FFarmSoil4 := TFarmProcess4Soil.Create(Model);
  FFarmSoil4.PackageIdentifier := StrSOILFarmProcessV;
  FFarmSoil4.Classification := StrFarmProcessClassification;
  FFarmSoil4.SelectionType := stCheckBox;

  FFarmClimate4 := TFarmProcess4Climate.Create(Model);
  FFarmClimate4.PackageIdentifier := StrCLIMATEFarmProces;
  FFarmClimate4.Classification := StrFarmProcessClassification;
  FFarmClimate4.SelectionType := stCheckBox;

  FFarmSurfaceWater4 := TFarmProcess4SurfaceWater.Create(Model);
  FFarmSurfaceWater4.PackageIdentifier := StrSURFACEWATERFarm;
  FFarmSurfaceWater4.Classification := StrFarmProcessClassification;
  FFarmSurfaceWater4.SelectionType := stCheckBox;

  FFarmWells4 := TFarmProcess4Wells.Create(Model);
  FFarmWells4.PackageIdentifier := StrSUPPLYWELLFarmPr;
  FFarmWells4.Classification := StrFarmProcessClassification;
  FFarmWells4.SelectionType := stCheckBox;

  FFarmAllotments := TFarmProcess4Allotments.Create(Model);
  FFarmAllotments.PackageIdentifier := StrALLOTMENTSFarmPro;
  FFarmAllotments.Classification := StrFarmProcessClassification;
  FFarmAllotments.SelectionType := stCheckBox;

  FFarmLandUse := TFarmProcess4LandUse.Create(Model);
  FFarmLandUse.PackageIdentifier := StrLANDUSEFarmProce;
  FFarmLandUse.Classification := StrFarmProcessClassification;
  FFarmLandUse.SelectionType := stCheckBox;

  FFarmSalinityFlush := TFarmProcess4SalinityFlush.Create(Model);
  FFarmSalinityFlush.PackageIdentifier := StrSALINITYFLUSHIRRIG;
  FFarmSalinityFlush.Classification := StrFarmProcessClassification;
  FFarmSalinityFlush.SelectionType := stCheckBox;

  FBuoyancyPackage := TBuoyancyPackage.Create(Model);
  FBuoyancyPackage.PackageIdentifier := StrBUYBuoyancyPackag;
  FBuoyancyPackage.Classification := StrFlowPackages;
  FBuoyancyPackage.SelectionType := stCheckBox;

  FViscosityPackage := TViscosityPackage.Create(Model);
  FViscosityPackage.PackageIdentifier := StrVSCViscosityPacka;
  FViscosityPackage.Classification := StrFlowPackages;
  FViscosityPackage.SelectionType := stCheckBox;

  FTvkPackage := TTvkPackage.Create(Model);
  FTvkPackage.PackageIdentifier := StrTVKTimeVaryingHy;
  FTvkPackage.Classification := StrFlowPackages;
  FTvkPackage.SelectionType := stCheckBox;

  FTvsPackage := TTvsPackage.Create(Model);
  FTvsPackage.PackageIdentifier := StrTVSTimeVaryingSt;
  FTvsPackage.Classification := StrFlowPackages;
  FTvsPackage.SelectionType := stCheckBox;

  FPrtModels := TPrtModels.Create(Model);

end;

destructor TModflowPackages.Destroy;
begin
  FTvsPackage.Free;
  FTvkPackage.Free;
  FViscosityPackage.Free;
  FBuoyancyPackage.Free;
  FFarmSalinityFlush.Free;
  FFarmLandUse.Free;
  FFarmAllotments.Free;
  FFarmWells4.Free;
  FFarmSurfaceWater4.Free;
  FFarmClimate4.Free;
  FFarmSoil4.Free;
  FFarmProcess4.Free;

  FGweProcess.Free;
  FGweAdvectionPackage.Free;
  FGweConductionAndDispersionPackage.Free;
  FGweEstPackage.Free;
  FGweSsmPackage.Free;
  FGweCtpPackage.Free;
  FGweEslPackage.Free;

  FGwtPackages.Free;
  FGwtSrcPackage.Free;
  FGwtCncPackage.Free;
  FGwtSsmPackage.Free;
  FGwtAdvectionPackage.Free;
  FGwtDispersionPackage.Free;
  FGwtProcess.Free;
  FCsubPackage.Free;
  FUzfMf6Package.Free;
  FMvrPackage.Free;
  FLakMf6Package.Free;
  FMf6ObservationUtility.Free;
  FGncPackage.Free;
  FSfrModflow6Package.Free;
  FRipPackage.Free;
  FSmsPackage.Free;
  FStoPackage.Free;
  FNpfPackage.Free;
  FSwrPackage.Free;
  FSwiPackage.Free;
  FConduitFlowProcess.Free;
  FFarmProcess.Free;
  FMt3dUnsatTransport.Free;
  FMt3dmsTransObs.Free;
  FMt3dmsChemReaction.Free;
  FMt3dmsSourceSink.Free;
  FMt3dmsDispersion.Free;
  FMt3dmsAdvection.Free;
  FMt3dmsGCGSolver.Free;
  FMt3dBasic.Free;
  FNwtPackage.Free;
  FUpwPackage.Free;
  FHydmodPackage.Free;
  FSwtPackage.Free;
  FZoneBudget.Free;
  FSubPackage.Free;
  FBcfPackage.Free;
  FStobPackage.Free;
  FRvobPackage.Free;
  FGbobPackage.Free;
  FDrobPackage.Free;
  FChobPackage.Free;
  FHfbPackage.Free;
  FHobPackage.Free;
  FDe4Package.Free;
  FSipPackage.Free;
  FGmgPackage.Free;
  FUzfPackage.Free;
  FStrPackage.Free;
  FSfrPackage.Free;
  FLakPackage.Free;
  FResPackage.Free;
  FEtsPackage.Free;
  FEvtPackage.Free;
  FRchPackage.Free;
  FDrtPackage.Free;
  FDrnPackage.Free;
  FRivPackage.Free;
  FWelPackage.Free;
  FGhbBoundary.Free;
  FFhbPackage.Free;
  FChdBoundary.Free;
  FHufPackage.Free;
  FLpfPackage.Free;
  FPcgnPackage.Free;
  FPcgPackage.Free;
  FModPath.Free;
  FMnw2Package.Free;
  FMnw1Package.Free;
  FMawPackage.Free;
  FMt3dLkt.Free;
  FMt3dSft.Free;
  FMt3dCts.Free;
  FPrtModels.Free;
  inherited;
end;

procedure TModflowPackages.Loaded;
begin
  Mt3dSft.Loaded;
  CsubPackage.Loaded;
  FarmLandUse.Loaded;
end;

procedure TModflowPackages.Reset;
begin
  DrtPackage.InitializeVariables;
  DrnPackage.InitializeVariables;
  RivPackage.InitializeVariables;
  WelPackage.InitializeVariables;
  ChdBoundary.InitializeVariables;
  GhbBoundary.InitializeVariables;
  LpfPackage.InitializeVariables;
  PcgPackage.InitializeVariables;
  PcgnPackage.InitializeVariables;
  RchPackage.InitializeVariables;
  EvtPackage.InitializeVariables;
  EtsPackage.InitializeVariables;
  ResPackage.InitializeVariables;
  LakPackage.InitializeVariables;
  SfrPackage.InitializeVariables;
  UzfPackage.InitializeVariables;
  GmgPackage.InitializeVariables;
  SipPackage.InitializeVariables;
  De4Package.InitializeVariables;
  HobPackage.InitializeVariables;
  HfbPackage.InitializeVariables;
  ModPath.InitializeVariables;
  ChobPackage.InitializeVariables;
  DrobPackage.InitializeVariables;
  GbobPackage.InitializeVariables;
  RvobPackage.InitializeVariables;
  StobPackage.InitializeVariables;
  HufPackage.InitializeVariables;
  BcfPackage.InitializeVariables;
  SubPackage.InitializeVariables;
  ZoneBudget.InitializeVariables;
  SwtPackage.InitializeVariables;
  HydmodPackage.InitializeVariables;
  UpwPackage.InitializeVariables;
  NwtPackage.InitializeVariables;
  Mt3dBasic.InitializeVariables;
  Mt3dmsGCGSolver.InitializeVariables;
  Mt3dmsAdvection.InitializeVariables;
  Mt3dmsDispersion.InitializeVariables;
  Mt3dmsSourceSink.InitializeVariables;
  Mt3dmsChemReact.InitializeVariables;
  Mt3dmsTransObs.InitializeVariables;
  Mt3dUnsatTransport.InitializeVariables;
  Mt3dLkt.InitializeVariables;
  StrPackage.InitializeVariables;
  FhbPackage.InitializeVariables;
  FarmProcess.InitializeVariables;
  ConduitFlowProcess.InitializeVariables;
  SwiPackage.InitializeVariables;
  SwrPackage.InitializeVariables;
  Mnw1Package.InitializeVariables;
  NpfPackage.InitializeVariables;
  StoPackage.InitializeVariables;
  SmsPackage.InitializeVariables;
  RipPackage.InitializeVariables;
  SfrModflow6Package.InitializeVariables;
  MawPackage.InitializeVariables;
  GncPackage.InitializeVariables;
  Mf6ObservationUtility.InitializeVariables;
  LakMf6Package.InitializeVariables;
  MvrPackage.InitializeVariables;
  UzfMf6Package.InitializeVariables;
  Mt3dSft.InitializeVariables;
  Mt3dCts.InitializeVariables;
  CsubPackage.InitializeVariables;
  GwtProcess.InitializeVariables;
  GwtPackages.InitializeVariables;
  GwtDispersionPackage.InitializeVariables;
  GwtSsmPackage.InitializeVariables;
  GwtAdvectionPackage.InitializeVariables;
  GwtCncPackage.InitializeVariables;
  GwtSrcPackage.InitializeVariables;

  GweProcess.InitializeVariables;
  GweAdvectionPackage.InitializeVariables;
  GweConductionAndDispersionPackage.InitializeVariables;
  GweEstPackage.InitializeVariables;
  GweSsmPackage.InitializeVariables;
  GweCtpPackage.InitializeVariables;
  GweEslPackage.InitializeVariables;

  FarmProcess4.InitializeVariables;
  FarmSoil4.InitializeVariables;
  FarmClimate4.InitializeVariables;
  FarmSurfaceWater4.InitializeVariables;
  FarmWells4.InitializeVariables;
  FarmAllotments.InitializeVariables;
  FarmLandUse.InitializeVariables;
  FarmSalinityFlush.InitializeVariables;

  BuoyancyPackage.InitializeVariables;
  ViscosityPackage.InitializeVariables;
  TvkPackage.InitializeVariables;
  TvsPackage.InitializeVariables;
  PrtModels.Clear;
end;


function TModflowPackages.SelectedModflowPackageCount: integer;
var
  LocalModel: TCustomModel;
begin
  result := 0;
  if ChdBoundary.IsSelected then
  begin
    Inc(Result);
  end;
  if FhbPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;
  if GhbBoundary.IsSelected then
  begin
    Inc(Result);
  end;
  if LpfPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;
  if PcgPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;
  if PcgnPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;

  if WelPackage.IsSelected then
  begin
    Inc(Result);
  end;
  if RivPackage.IsSelected then
  begin
    Inc(Result);
  end;
  if DrnPackage.IsSelected then
  begin
    Inc(Result);
  end;
  if DrtPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;

  if RchPackage.IsSelected then
  begin
    Inc(Result);
  end;
  if EvtPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;
  if EtsPackage.IsSelected then
  begin
    Inc(Result);
  end;
  if ResPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;

  if LakPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;
  if SfrPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;
  if UzfPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;
  if GmgPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;

  if SipPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;
  if De4Package.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;
  if HobPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;
  if HfbPackage.IsSelected then
  begin
    Inc(Result);
  end;
  if ChobPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;
  if DrobPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;
  if GbobPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;
  if RvobPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;
  if StobPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;
  if HufPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
    if (FModel as TCustomModel).HufParameters.CountParameters(
      [ptHUF_KDEP]) > 0 then
    begin
      Inc(Result);
    end;
    if (FModel as TCustomModel).ModflowSteadyParameters.CountParameters(
      [ptHUF_LVDA]) > 0 then
    begin
      Inc(Result);
    end;
  end;
  if Mnw2Package.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;
  if BcfPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;
  if SubPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;
  if SwtPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;
  if HydmodPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;
  if UpwPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;
  if NwtPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;
  if StrPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;
//  if FHfbPackage.IsSelected then
//  begin
//    Inc(Result);
//  end;

  if FarmProcess.IsSelected and (Model.ModelSelection = msModflowFmp) then
  begin
    Inc(Result);
  end;

  if FarmProcess4.IsSelected and (Model.ModelSelection = msModflowOwhm2) then
  begin
    Inc(Result);
  end;

  if RipPackage.IsSelected and (Model.ModelSelection in
    [msModflowFmp, msModflowOwhm2]) then
  begin
    Inc(Result);
  end;

  if ConduitFlowProcess.IsSelected and (Model.ModelSelection in
    [msModflowCfp, msModflowOwhm2]) then
  begin
    Inc(Result);
  end;

  if SwiPackage.IsSelected
    and (Model.ModelSelection in [msModflow, msModflowNWT]) then
  begin
    Inc(Result);
  end;

  if SwrPackage.IsSelected
    and (Model.ModelSelection in
    [msModflow, msModflowNWT, msModflowFmp, msModflowOwhm2]) then
  begin
    Inc(Result);
  end;

  if Mnw1Package.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    Inc(Result);
  end;

  if NpfPackage.IsSelected and (Model.ModelSelection = msModflow2015) then
  begin
    Inc(Result);
  end;

  if StoPackage.IsSelected and (Model.ModelSelection = msModflow2015) then
  begin
    Inc(Result);
  end;

  if SmsPackage.IsSelected and (Model.ModelSelection = msModflow2015) then
  begin
    Inc(Result);
  end;

  if SfrModflow6Package.IsSelected and (Model.ModelSelection = msModflow2015) then
  begin
    Inc(Result);
  end;

  if MawPackage.IsSelected and (Model.ModelSelection = msModflow2015) then
  begin
    Inc(Result);
  end;

  if GncPackage.IsSelected and ((Model as TCustomModel).DisvUsed) then
  begin
    Inc(Result);
  end;

  if Mf6ObservationUtility.IsSelected and (Model.ModelSelection = msModflow2015) then
  begin
    Inc(Result);
  end;

  if LakMf6Package.IsSelected and (Model.ModelSelection = msModflow2015) then
  begin
    Inc(Result);
  end;

  if MvrPackage.IsSelected and (Model.ModelSelection = msModflow2015) then
  begin
    Inc(Result);
  end;

  if UzfMf6Package.IsSelected and (Model.ModelSelection = msModflow2015) then
  begin
    Inc(Result);
  end;

  if CsubPackage.IsSelected and (Model.ModelSelection = msModflow2015) then
  begin
    Inc(Result);
  end;

  if BuoyancyPackage.IsSelected and (Model.ModelSelection = msModflow2015)  then
  begin
    Inc(Result);
  end;

  if ViscosityPackage.IsSelected and (Model.ModelSelection = msModflow2015)  then
  begin
    Inc(Result);
  end;

  if TvkPackage.IsSelected and (Model.ModelSelection = msModflow2015)  then
  begin
    Inc(Result);
  end;

  if TvsPackage.IsSelected and (Model.ModelSelection = msModflow2015)  then
  begin
    Inc(Result);
  end;

  LocalModel := Model as TCustomModel;
  if LocalModel.GwtUsed then
  begin
    Inc(Result, LocalModel.MobileComponents.Count);
  end;

  if GweProcess.IsSelected and (Model.ModelSelection = msModflow2015)  then
  begin
    Inc(Result);
  end;

  if GweAdvectionPackage.IsSelected and (Model.ModelSelection = msModflow2015)  then
  begin
    Inc(Result);
  end;

  if GweConductionAndDispersionPackage.IsSelected and (Model.ModelSelection = msModflow2015)  then
  begin
    Inc(Result);
  end;

  if GweEstPackage.IsSelected and (Model.ModelSelection = msModflow2015)  then
  begin
    Inc(Result);
  end;

  if GweSsmPackage.IsSelected and (Model.ModelSelection = msModflow2015)  then
  begin
    Inc(Result);
  end;

  if GweCtpPackage.IsSelected and (Model.ModelSelection = msModflow2015)  then
  begin
    Inc(Result);
  end;

  if GweEslPackage.IsSelected and (Model.ModelSelection = msModflow2015)  then
  begin
    Inc(Result);
  end;



  {$IFDEF PRT}
  if Model.ModelSelection = msModflow2015 then
  begin
    for var I := 0 to PrtModels.Count - 1 do
    begin
      Var PrtModel: TPrtModel := PrtModels[I].PrtModel;
      if PrtModel.IsSelected then
      begin
        Inc(result, 3);
        for var J := 0 to PrtModel.Count - 1 do
        begin
          var PrpPackage := PrtModel[J].PrpPackage;
          if PrpPackage.IsSelected then
          begin
            Inc(result);
          end;
        end;
      end;
    end;
  end;
  {$ENDIF};

  // Don't count Modpath or ZoneBudget
  // because they are exported seperately from MODFLOW.
//  if ZoneBudget.IsSelected then
//  begin
//    Inc(Result);
//  end;
end;

procedure TModflowPackages.SetBcfPackage(const Value: TModflowPackageSelection);
begin
  FBcfPackage.Assign(Value);
end;

procedure TModflowPackages.SetBuoyancyPackage(const Value: TBuoyancyPackage);
begin
  FBuoyancyPackage.Assign(Value);
end;

procedure TModflowPackages.SetChdBoundary(
  const Value: TChdPackage);
begin
  FChdBoundary.Assign(Value);
end;

procedure TModflowPackages.SetChobPackage(
  const Value: TModflowPackageSelection);
begin
  FChobPackage.Assign(Value);
end;

procedure TModflowPackages.SetConduitFlowProcess(
  const Value: TConduitFlowProcess);
begin
  FConduitFlowProcess.Assign(Value);
end;

procedure TModflowPackages.SetCsubPackage(const Value: TCSubPackageSelection);
begin
  FCsubPackage.Assign(Value);
end;

procedure TModflowPackages.SetDe4Package(const Value: TDE4PackageSelection);
begin
  FDe4Package.Assign(Value);
end;

procedure TModflowPackages.SetDrnPackage(const Value: TDrnPackage);
begin
  FDrnPackage.Assign(Value);
end;

procedure TModflowPackages.SetDrobPackage(
  const Value: TModflowPackageSelection);
begin
  FDrobPackage.Assign(Value);
end;

procedure TModflowPackages.SetDrtPackage(const Value: TDrtPackage);
begin
  FDrtPackage.Assign(Value);
end;

procedure TModflowPackages.SetEtsPackage(const Value: TEtsPackageSelection);
begin
  FEtsPackage.Assign(Value);
end;

procedure TModflowPackages.SetEvtPackage(const Value: TEvtPackageSelection);
begin
  FEvtPackage.Assign(Value);
end;

procedure TModflowPackages.SetFarmAllotments(
  const Value: TFarmProcess4Allotments);
begin
  FFarmAllotments.Assign(Value);
end;

procedure TModflowPackages.SetFarmClimate4(const Value: TFarmProcess4Climate);
begin
  FFarmClimate4.Assign(Value);
end;

procedure TModflowPackages.SetFarmLandUse(const Value: TFarmProcess4LandUse);
begin
  FFarmLandUse.Assign(Value);
end;

procedure TModflowPackages.SetFarmProcess(const Value: TFarmProcess);
begin
  FFarmProcess.Assign(Value);
end;

procedure TModflowPackages.SetFarmProcess4(const Value: TFarmProcess4);
begin
  FFarmProcess4.Assign(Value);
end;

procedure TModflowPackages.SetFarmSalinityFlush(
  const Value: TFarmProcess4SalinityFlush);
begin
  FFarmSalinityFlush.Assign(Value);
end;

procedure TModflowPackages.SetFarmSoil4(const Value: TFarmProcess4Soil);
begin
  FFarmSoil4.Assign(Value);
end;

procedure TModflowPackages.SetFarmSurfaceWater4(
  const Value: TFarmProcess4SurfaceWater);
begin
  FFarmSurfaceWater4.Assign(Value);
end;

procedure TModflowPackages.SetFarmWells4(const Value: TFarmProcess4Wells);
begin
  FFarmWells4.Assign(Value);
end;

procedure TModflowPackages.SetFhbPackage(const Value: TFhbPackageSelection);
begin
  FFhbPackage.Assign(Value);
end;

procedure TModflowPackages.SetGbobPackage(
  const Value: TModflowPackageSelection);
begin
  FGbobPackage.Assign(Value);
end;

procedure TModflowPackages.SetGhbBoundary(
  const Value: TGhbPackage);
begin
  FGhbBoundary.Assign(Value);
end;

procedure TModflowPackages.SetGmgPackage(const Value: TGmgPackageSelection);
begin
  FGmgPackage.Assign(Value);
end;

procedure TModflowPackages.SetGncPackage(const Value: TGncPackage);
begin
  FGncPackage.Assign(Value)
end;

procedure TModflowPackages.SetGweAdvectionPackage(
  const Value: TGwtAdvectionPackage);
begin
  FGweAdvectionPackage.Assign(Value);
end;

procedure TModflowPackages.SetGweConductionAndDispersionPackage(
  const Value: TGweConductionAndDispersionPackage);
begin
  FGweConductionAndDispersionPackage.Assign(Value);
end;

procedure TModflowPackages.SetGweCtpPackage(const Value: TGweCtpPackage);
begin
  FGweCtpPackage.Assign(Value);
end;

procedure TModflowPackages.SetGweEslPackage(const Value: TGweEslPackage);
begin
  FGweEslPackage.Assign(Value);
end;

procedure TModflowPackages.SetGweProcess(const Value: TGwtProcess);
begin
  FGweProcess.Assign(Value);
end;

procedure TModflowPackages.SetGweSsmPackage(const Value: TGwtSsmPackage);
begin
  FGweSsmPackage.Assign(Value);
end;

procedure TModflowPackages.SetGwtAdvectionPackage(
  const Value: TGwtAdvectionPackage);
begin
  FGwtAdvectionPackage.Assign(Value);
end;

procedure TModflowPackages.SetGwtDispersionPackage(
  const Value: TGwtDispersionPackage);
begin
  FGwtDispersionPackage.Assign(Value);
end;

procedure TModflowPackages.SetGwtPackges(const Value: TGwtPackageCollection);
begin
  FGwtPackages.Assign(Value);
end;

procedure TModflowPackages.SetGwtProcess(const Value: TGwtProcess);
begin
  FGwtProcess.Assign(Value);
end;

procedure TModflowPackages.SetGwtSrcPackage(const Value: TGwtSrcPackage);
begin
  FGwtSrcPackage.Assign(Value);
end;

procedure TModflowPackages.SetGwtSsmPackage(const Value: TGWtSsmPackage);
begin
  FGwtSsmPackage.Assign(Value);
end;

procedure TModflowPackages.SetGwtCncPackage(const Value: TGwtCncPackage);
begin
  FGwtCncPackage.Assign(Value);
end;


procedure TModflowPackages.SetHfbPackage(const Value: TModflowPackageSelection);
begin
  FHfbPackage.Assign(Value);
end;

procedure TModflowPackages.SetHobPackage(const Value: THobPackageSelection);
begin
  FHobPackage.Assign(Value);
end;

procedure TModflowPackages.SetHufPackage(const Value: THufPackageSelection);
begin
  FHufPackage.Assign(Value);
end;

procedure TModflowPackages.SetHydmodPackage(const Value: THydPackageSelection);
begin
  FHydmodPackage.Assign(Value);
end;

procedure TModflowPackages.SetLakMf6Package(
  const Value: TLakeMf6PackageSelection);
begin
  FLakMf6Package.Assign(Value);
end;

procedure TModflowPackages.SetLakPackage(const Value: TLakePackageSelection);
begin
  FLakPackage.Assign(Value);
end;

procedure TModflowPackages.SetLpfPackage(const Value: TLpfSelection);
begin
  FLpfPackage.Assign(Value);
end;

procedure TModflowPackages.SetMawPackage(const Value: TMawPackage);
begin
  FMawPackage.Assign(Value);
end;

procedure TModflowPackages.SetMf6ObservationUtility(
  const Value: TMf6ObservationUtility);
begin
  FMf6ObservationUtility.Assign(Value)
end;

procedure TModflowPackages.SetMnw1Package(const Value: TMnw1Package);
begin
  FMnw1Package.Assign(Value);
end;

procedure TModflowPackages.SetMnw2Package(const Value: TMultinodeWellSelection);
begin
  FMnw2Package.Assign(Value);
end;

procedure TModflowPackages.SetModPath(const Value: TModpathSelection);
begin
  if FModel <> nil then
  begin
    if (Value.IsSelected)
      and ((Value.BeginningTime <> FModPath.BeginningTime)
      or (Value.EndingTime <> FModPath.EndingTime)) then
    begin
      frmGoPhast.CreateNewCompositeBudgetFile := True;
    end;
  end;
  FModPath.Assign(Value);
end;

procedure TModflowPackages.SetMt3dBasic(const Value: TMt3dBasic);
begin
  FMt3dBasic.Assign(Value);
end;

procedure TModflowPackages.SetMt3dCts(const Value: TMt3dCtsPackageSelection);
begin
  FMt3dCts.Assign(Value);
end;

procedure TModflowPackages.SetMt3dLkt(const Value: TMt3dLktPackage);
begin
  FMt3dLkt.Assign(Value);
end;

procedure TModflowPackages.SetMt3dmsAdvection(const Value: TMt3dmsAdvection);
begin
  FMt3dmsAdvection.Assign(Value);
end;

procedure TModflowPackages.SetMt3dmsChemReaction(
  const Value: TMt3dmsChemReaction);
begin
  FMt3dmsChemReaction.Assign(Value);
end;

procedure TModflowPackages.SetMt3dmsDispersion(const Value: TMt3dmsDispersion);
begin
  FMt3dmsDispersion.Assign(Value);
end;

procedure TModflowPackages.SetMt3dmsGCGSolver(
  const Value: TMt3dmsGCGSolverPackage);
begin
  FMt3dmsGCGSolver.Assign(Value);
end;

procedure TModflowPackages.SetMt3dmsSourceSink(
  const Value: TMt3dmsSourceSinkMixing);
begin
  FMt3dmsSourceSink.Assign(Value);
end;

procedure TModflowPackages.SetMt3dmsTransObs(
  const Value: TMt3dmsTransportObservations);
begin
  FMt3dmsTransObs.Assign(Value);
end;

procedure TModflowPackages.SetMt3dSft(const Value: TMt3dSftPackageSelection);
begin
  FMt3dSft.Assign(Value);
end;

procedure TModflowPackages.SetMt3dUnsatTransport(const Value: TMt3dUztPackage);
begin
  FMt3dUnsatTransport.Assign(Value);
end;

procedure TModflowPackages.SetMvrPackage(const Value: TMvrPackage);
begin
  FMvrPackage.Assign(Value);
end;

procedure TModflowPackages.SetNpfPackage(const Value: TNpfPackage);
begin
  FNpfPackage.Assign(Value);
end;

procedure TModflowPackages.SetNwtPackage(const Value: TNwtPackageSelection);
begin
  FNwtPackage.Assign(Value);
end;

procedure TModflowPackages.SetPcgnPackage(const Value: TPcgnSelection);
begin
  FPcgnPackage.Assign(Value);
end;

procedure TModflowPackages.SetPcgPackage(const Value: TPcgSelection);
begin
  FPcgPackage.Assign(Value);
end;

procedure TModflowPackages.SetPrtModels(const Value: TPrtModels);
begin
  FPrtModels.Assign(Value);
end;

procedure TModflowPackages.SetRchPackage(const Value: TRchPackageSelection);
begin
  FRchPackage.Assign(Value);
end;

procedure TModflowPackages.SetResPackage(const Value: TResPackageSelection);
begin
  FResPackage.Assign(Value);
end;

procedure TModflowPackages.SetRipPackage(const Value: TRipPackage);
begin
  FRipPackage.Assign(Value);
end;

procedure TModflowPackages.SetRivPackage(const Value: TRivPackage);
begin
  FRivPackage.Assign(Value);
end;

procedure TModflowPackages.SetRvobPackage(
  const Value: TModflowPackageSelection);
begin
  FRvobPackage.Assign(Value);
end;

procedure TModflowPackages.SetSfrModflow6Package(
  const Value: TSfrModflow6PackageSelection);
begin
  FSfrModflow6Package.Assign(Value);
end;

procedure TModflowPackages.SetSfrPackage(const Value: TSfrPackageSelection);
begin
  FSfrPackage.Assign(Value);
end;

procedure TModflowPackages.SetSipPackage(const Value: TSIPPackageSelection);
begin
  FSipPackage.Assign(Value);
end;

procedure TModflowPackages.SetSmsPackage(const Value: TSmsPackageSelection);
begin
  FSmsPackage.Assign(Value);
end;

procedure TModflowPackages.SetStobPackage(
  const Value: TModflowPackageSelection);
begin
  FStobPackage.Assign(Value);
end;

procedure TModflowPackages.SetStoPackage(const Value: TStoPackage);
begin
  FStoPackage.Assign(Value);
end;

procedure TModflowPackages.SetStrPackage(const Value: TStrPackageSelection);
begin
  FStrPackage.Assign(Value);
end;

procedure TModflowPackages.SetSubPackage(const Value: TSubPackageSelection);
begin
  FSubPackage.Assign(Value);
end;

procedure TModflowPackages.SetSwiPackage(const Value: TSwiPackage);
begin
  FSwiPackage.Assign(Value);
end;

procedure TModflowPackages.SetSwrPackage(const Value: TSwrPackage);
begin
  FSwrPackage.Assign(Value);
end;

procedure TModflowPackages.SetSwtPackage(const Value: TSwtPackageSelection);
begin
  FSwtPackage.Assign(Value);
end;

procedure TModflowPackages.SetGweEstPackage(const Value: TGweEstPackage);
begin
  FGweEstPackage.Assign(Value);
end;

procedure TModflowPackages.SetTvkPackage(const Value: TTvkPackage);
begin
  FTvkPackage.Assign(Value);
end;

procedure TModflowPackages.SetTvsPackage(const Value: TTvsPackage);
begin
  FTvsPackage.Assign(Value);
end;

procedure TModflowPackages.SetUpwPackage(const Value: TUpwPackageSelection);
begin
  FUpwPackage.Assign(Value);
end;

procedure TModflowPackages.SetUzfMf6Package(
  const Value: TUzfMf6PackageSelection);
begin
  FUzfMf6Package.Assign(Value);
end;

procedure TModflowPackages.SetUzfPackage(const Value: TUzfPackageSelection);
begin
  FUzfPackage.Assign(Value);
end;

procedure TModflowPackages.SetViscosityPackage(const Value: TViscosityPackage);
begin
  FViscosityPackage.Assign(Value);
end;

procedure TModflowPackages.SetWelPackage(const Value: TWellPackage);
begin
  FWelPackage.Assign(Value);
end;

procedure TModflowPackages.SetZoneBudget(const Value: TZoneBudgetSelect);
begin
  FZoneBudget.Assign(Value);
end;

  // Except in the initialization section, the following variables
  // should be treated as constants.
var
  FBC_SpecHead: string;
  FBC_SpecifiedFlux: string;
  FBC_HeadDependentFlux: string;

function BC_SpecHead: string;
begin
  result := FBC_SpecHead;
end;

function BC_SpecifiedFlux: string;
begin
  result := FBC_SpecifiedFlux;
end;

function BC_HeadDependentFlux: string;
begin
  result := FBC_HeadDependentFlux;
end;

initialization
  FBC_SpecHead := StrBoundaryCondition + '|' + StrSpecifiedHeadPackages;
  FBC_SpecifiedFlux := StrBoundaryCondition + '|' + StrSpecifiedFlux;
  FBC_HeadDependentFlux := StrBoundaryCondition + '|' + StrHeaddependentFlux;

end.
