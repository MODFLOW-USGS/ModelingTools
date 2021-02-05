{@abstract(The primary purpose of @name is to define @link(TfrmImportShapefile)
  which is used to import Shapefiles.)
  @name also defines @link(TUndoImportShapefile) which is used to undo or
  redo the import of the Shapefile.
  @author(Richard B. Winston <rbwinst@usgs.gov>)}
unit frmImportShapefileUnit;

interface

{ TODO : When importing objects, move to first object if none are
visisble on screen. }
{ TODO :
There needs to be a way to import lots of Time-Series data
from a data base (Access or Excel) }
{ TODO :
There should be a method to convert coordinates from lat-long to X-Y
using Lambert and Albers projections (and UTM).  Get a table of  the
parameters for all the state-plane coordinate systems for use in the
conversion. }
uses System.UITypes, Windows,
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, XBase1, Buttons, ExtCtrls,
  Grids, RbwDataGrid4, ShapefileUnit, IntListUnit, ScreenObjectUnit,
  RbwParser, ComCtrls, Spin, UndoItemsScreenObjects, JvExStdCtrls, JvCombobox,
  JvListComb, Mask, JvExMask, JvSpin, JvExControls, JvPageList, ArgusDataEntry,
  ModflowPackagesUnit, ModflowBoundaryUnit, DataSetUnit, UndoItems, RbwEdit,
  JvExComCtrls, JvComCtrls, ModflowPackageSelectionUnit,
  frameLocationMethodUnit, JvToolEdit, ModflowTransientListParameterUnit,
  OrderedCollectionUnit, Mt3dmsChemUnit, Mt3dmsChemSpeciesUnit, GoPhastTypes,
  frameGridUnit, System.Generics.Collections, GrayTabs, frmDuplicateNamesUnit,
  Vcl.CheckLst, System.StrUtils;

type
  TSfrColumns = (scStartTime, scEndTime, scIcalc,
    scOutflowSegment, scDiversionSegment, scIprior,
    scFlow, scPtsw, scEtsw,
    scRunoff, scRoughCh, scRoughBk, scCdpth, scFdpth, scAwdth, edBwdth,
    scHcond1, scThickM1, scElevUp, scWidth1, scDepth1, scHcond2, scThickM2,
    scElevDn, scWidth2, scDepth2,
    scDist1, scDist2, scDist3, scDist4, scDist5, scDist6, scDist7, scDist8,
    scZ1, scZ2, scZ3, scZ4, scZ5, scZ6, scZ7, scZ8);

  TSfr_Mf6_Column = (smcStartTime, smcEndTime, smcStatus, smcStage,
    smcInflow, smcRainfall, smcEvaporation, smcRunoff, smcRoughness,
    smcUpstreamFraction);

  TStrColumn = (strcStartTime, strcEndTime, strcOutflow,
    strcDiversion, strcFlow, strcHead, strcConductance, strcBedBottom,
    strcBedTop, strcWidth, strcSlope, strcRoughness);

  TLakeColumns = (lcStartTime, lcEndTime, lcMinStage, lcMaxStage, lcPrecip,
    lcEvap, lcRunoff, lcWithdrawl, lcConcentration);

  TLakeMf6Columns = (l6cStartTim, l6cEndTime, l6cStatus, l6cStage, l6cRainfall,
    l6cEvaporation, l6cRunoff, l6cInflow, l6cWithdrawal);

  TUzfColumns = (ucStartTime, ucEndTime, ucInfiltration, ucEvapRate,
    ucExtinctDepth, ucExtinctWaterContent);

  TRchColumns = (rcStartTime, rcEndTime, rcParameterName, rcFluxRate, rcLayer,
    rcConcentration);

  TChdColumns = (ccStartingTime, ccEndingTime, ccParameterName,
    ccStartingHead, ccEndingHead, ccConcentration);

  TDrnColumns = (dcStartingTime, dcEndingTime, dcParameterName,
    dcElevation, dcConductance, dcConcentration);

  TDrtColumns = (dtcStartingTime, dtcEndingTime, dtcParameterName,
    dtcElevation, dtcConductance, dtcReturnFraction, dtcConcentration);

  TEtsColumns = (etscStartingTime, etscEndingTime, etscParameterName,
    etscRate, etscSurface, etscDepth, etscConcentration);

  TEvtColumns = (evtcStartingTime, evtcEndingTime, evtcParameterName,
    evtRate, evtcSurface, evtcDepth, evtcLayer, evtcConcentration);

  TGhbColumns = (ghbcStartingTime, ghbcEndingTime, ghbcParameterName,
    ghbcHead, ghbcConductance, ghbcConcentration);

  TRivColumns = (rivcStartingTime, rivcEndingTime, rivcParameterName,
    rivcBottom, rivcStage, rivcConductance, rivcConcentration);

  TWelColumns = (welcStartingTime, welcEndingTime, welcParameterName,
    welcPumpingRate, welcConcentration);

  TResColumns = (rescStartingTime, rescEndingTime, rescStartingHead,
    rescEndingHead, rescConcentration);

  TMawColumns = (mcStartTime, mcEndTime, mcStatus, mcRate, mcWellHead,
    mcMawFlowingWell,
    mcMawFlowingWellElevation, mcFlowingWellConductance, mcFlowingWellReductionLength,  mcRateLimitation,
    mcMinRate, mcMaxRate, mcPumpElevation, mcScalingLength, mcHeadLimitChoice,
    mcHeadLimit);

  TMawWellScreenColumns = (mwscScreenTop, mwscScreenBottom, mwscSkinK, mwscSkinRadius);

  TMf6PestObsColumns = (mpocName, mpocType, mpocTime, mpocValue, mpocWeight);

  TCsvAttribute = class(TObject)
  private
    FFileName: string;
    FAttributeName: string;
    FPosition: Integer;
    procedure SetAttributeName(const Value: string);
    procedure SetFileName(const Value: string);
    procedure SetPosition(const Value: Integer);
  public
    property FileName: string read FFileName write SetFileName;
    property AttributeName: string read FAttributeName write SetAttributeName;
    property Position: Integer read FPosition write SetPosition;
  end;

  TCsvAttributes = TObjectList<TCsvAttribute>;

  {@abstract(@name is used to undo or redo the import of a Shapefile)}
  TUndoImportShapefile = class(TCustomImportMultipleScreenObjects)
  private
    FTopDataSet: TDataArray;
    FThreeDDataSet: TDataArray;
  protected
    FNewDataSets: TList;
    FOldProperties: TList;
    FNewProperties: TList;
    // @name describes what @classname does.
    function Description: string; override;
  public
    constructor Create;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name makes sure the (TScreenObject.Deleted)
    // property @link(TScreenObject)s is @false.
    procedure DoCommand; override;
    // @name calls @link(DoCommand).
    procedure Redo; override;
    procedure StoreChangedDataSetProperties(var OldProperties,
      NewProperties: TList);
    procedure StoreNewDataSets(var NewDataSets: TList);
    // @name makes sure the (TScreenObject.Deleted)
    // property @link(TScreenObject)s is @true.
    procedure Undo; override;
    // @name notifies @link(frmGoPhast) that
    // frmGoPhast.@link(TfrmGoPhast.TopScreenObjectsChanged) is @True or
    // frmGoPhast.@link(TfrmGoPhast.FrontScreenObjectsChanged) is @True or
    // frmGoPhast.@link(TfrmGoPhast.SideScreenObjectsChanged) is @True
    // all three or true.
    // It then sets AScreenObject.@link(TObserver.UpToDate) to @True.
  end;

{ TODO : Check for similarities between TfrmImportShapefile and TfrmImportDXF. }

  { TODO : Allow there to be a persistent connection between the imported
  TScreenObjects and the original Shapefile.
  The user can update when desired or
  have updating done automatically.}

  {@abstract(@name is used to import Shapefiles.)}
  TfrmImportShapefile = class(TfrmCustomGoPhast)
    // @name: TButton;
    // See @link(btnSelectClick).
    btnAll: TButton;
    // @name: TBitBtn;
    // Clicking @name closes the @classname without changing anything.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // Clicking @name show help on the @classname.
    btnHelp: TBitBtn;
    // @name: TButton;
    // See @link(btnSelectClick).
    btnNone: TButton;
    // @name: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name: TButton;
    // See @link(btnToggleClick).
    btnToggle: TButton;
    // @name is used to turn on or off coordinate transformations from
    // decimal degrees to UTM coordinates.
    cbCoordinateConversion: TCheckBox;
    // @name is used to choose the ellipsoid for the coordinate conversion.
    comboEllipsoid: TComboBox;
    // @name is an image of the UTM zones.
    imageUtmZones: TImage;
    // @name tells that 10 points will be plotted on the map if coordinate
    // conversion is to be performed.
    lblCoordinateConversionInfo: TLabel;
    // @name gives the coordinates of the first point.
    lblCoordinates: TLabel;
    // @name is the label for @link(comboEllipsoid).
    lblEllipsoid: TLabel;
    // @name is the label for @link(seZoneNumber).
    lblUtmZoneNumber: TLabel;
    // @name: TOpenDialog;
    // @name is used to select the Shapefile.
    OpenDialogShape: TOpenDialog;
    // @name is TPageControl used to hold @link(tabData) and
    // @link(tabCoordinateConversion).
    pcImportShape: TPageControl;
    // @name: TPanel;
    // @name holds the buttons and other controls at the bottom of the
    // @classname.
    pnlButton: TPanel;
    // @name holds controls used to determine how the shapes in the
    // Shapefile will be imported.
    pnlData: TPanel;
    // @name shows the first point to be plotted on the map.
    Shape1: TShape;
    // @name shows the second point to be plotted on the map.
    Shape2: TShape;
    // @name shows the third point to be plotted on the map.
    Shape3: TShape;
    // @name shows the fourth point to be plotted on the map.
    Shape4: TShape;
    // @name shows the fifth point to be plotted on the map.
    Shape5: TShape;
    // @name shows the sixth point to be plotted on the map.
    Shape6: TShape;
    // @name shows the seventh point to be plotted on the map.
    Shape7: TShape;
    // @name shows the eighth point to be plotted on the map.
    Shape8: TShape;
    // @name shows the ninth point to be plotted on the map.
    Shape9: TShape;
    // @name shows the tenth point to be plotted on the map.
    Shape10: TShape;
    // @name holds the controls that determine how the shapes in the
    // Shapefile will be imported.
    tabData: TTabSheet;
    // @name holds the controls used to convert from decimal degrees to UTM
    // coordinates.
    tabCoordinateConversion: TTabSheet;
    // @name: TXBase;
    // @name is used to read the database file that is part of the
    // Shapefile.
    xbShapeDataBase: TXBase;
    seZoneNumber: TJvSpinEdit;
    pnlDataGrids: TPanel;
    dgFields: TRbwDataGrid4;
    tabFeatures: TTabSheet;
    pnlBoundaryCondition: TPanel;
    splitterBoundary: TSplitter;
    pnlBoundaryControls: TPanel;
    lblBoundaryTimeCount: TLabel;
    comboBoundaryChoice: TComboBox;
    seBoundaryTimeCount: TJvSpinEdit;
    rdgBoundaryConditions: TRbwDataGrid4;
    plBoundary: TJvPageList;
    jvspNone: TJvStandardPage;
    jvspPhastSpecifiedHead: TJvStandardPage;
    lblSolutionType: TLabel;
    comboSolutionType: TComboBox;
    jvspPhastLeaky: TJvStandardPage;
    jvspPhastRiver: TJvStandardPage;
    jvspPhastWell: TJvStandardPage;
    lblRiverDescripton: TLabel;
    lblRiverHydraulicConductivity: TLabel;
    lblRiverWidth: TLabel;
    lblRiverDepth: TLabel;
    lblRiverBedThickness: TLabel;
    lblLeakyHydraulicConductivity: TLabel;
    lblLeakyThickness: TLabel;
    comboLeakyHydraulicConductivity: TComboBox;
    comboLeakyThickness: TComboBox;
    comboRiverDescripton: TComboBox;
    comboRiverHydraulicConductivity: TComboBox;
    comboRiverWidth: TComboBox;
    comboRiverDepth: TComboBox;
    comboRiverBedThickness: TComboBox;
    pnlPhastWell: TPanel;
    lblWellDescription: TLabel;
    WellDescription: TComboBox;
    lblWellDiameter: TLabel;
    comboWellDiameter: TComboBox;
    lblWellLandSurfaceDatum: TLabel;
    comboWellLandSurfaceDatum: TComboBox;
    lblWellPumpAllocation: TLabel;
    comboWellPumpAllocation: TComboBox;
    comboWellIntervalStyle: TComboBox;
    lblWellIntervalStyle: TLabel;
    lblWellIntervals: TLabel;
    seWellIntervals: TJvSpinEdit;
    dgWellElevations: TRbwDataGrid4;
    jvspConductanceInterp: TJvStandardPage;
    lblConductanceInterpretation: TLabel;
    comboFormulaInterp: TComboBox;
    lblBoundaryChoice: TLabel;
    jvspModflowSFR: TJvStandardPage;
    pcSFR: TPageControl;
    tabSfrBasic: TTabSheet;
    tabSfrUnsaturated: TTabSheet;
    lblSfrSegmentNumber: TLabel;
    comboSfrSegmentNumber: TComboBox;
    lblReachLength: TLabel;
    lblStreamTop: TLabel;
    lblStreambedThickness: TLabel;
    lblSlope: TLabel;
    lblStreambedK: TLabel;
    lblSaturatedVolumetricWater: TLabel;
    lblInitialVolumetricWater: TLabel;
    lblBrooksCoreyExponent: TLabel;
    lblMaxUnsaturatedKz: TLabel;
    comboSfrReachLength: TComboBox;
    comboSfrStreambedTop: TComboBox;
    comboSfrStreamSlope: TComboBox;
    comboSfrStreambedThickness: TComboBox;
    comboSfrStreambedKv: TComboBox;
    comboSaturatedVolumetricWater: TComboBox;
    comboInitialVolumetricWater: TComboBox;
    comboBrooksCoreyExponent: TComboBox;
    comboaxUnsaturatedKz: TComboBox;
    jvspModflowLAK: TJvStandardPage;
    LblLakeID: TLabel;
    comboLakeID: TComboBox;
    lblInitialStage: TLabel;
    comboInitialStage: TComboBox;
    lblSill: TLabel;
    comboSill: TComboBox;
    lblCenterLake: TLabel;
    comboCenterLake: TComboBox;
    lblNumShapes: TLabel;
    tabOptions: TTabSheet;
    cbImportObjects: TCheckBox;
    cbEnclosedCells: TCheckBox;
    cbIntersectedCells: TCheckBox;
    cbInterpolation: TCheckBox;
    cbImportGrid: TCheckBox;
    lblImportCriterion: TLabel;
    lblCombineShapes: TLabel;
    lblVisibility: TLabel;
    edImportCriterion: TEdit;
    comboJoinObjects: TJvImageComboBox;
    comboVisibility: TJvImageComboBox;
    rgEvaluatedAt: TRadioGroup;
    btnImportCriterion: TButton;
    rgElevationCount: TRadioGroup;
    lblZ: TLabel;
    edZ: TRbwEdit;
    lblHighZ: TLabel;
    edHighZ: TRbwEdit;
    lblLowZ: TLabel;
    edLowZ: TRbwEdit;
    btnZ: TButton;
    btnHighZ: TButton;
    btnLowZ: TButton;
    jvspModflowDRT: TJvStandardPage;
    lblConductanceInterpretationDRT: TLabel;
    comboFormulaInterpDRT: TComboBox;
    lblDrainReturnLocationMethod: TLabel;
    comboDrainReturnLocationMethod: TComboBox;
    pcDrtReturnLChoice: TJvPageControl;
    tabDrtNone: TTabSheet;
    tabDrtLocation: TTabSheet;
    lblDrtX: TLabel;
    lblDrtY: TLabel;
    lblDrtZ: TLabel;
    rdeDrtX: TRbwDataEntry;
    rdeDrtY: TRbwDataEntry;
    rdeDrtZ: TRbwDataEntry;
    tabDrtCell: TTabSheet;
    lblDrtCol: TLabel;
    lblDrtRow: TLabel;
    lblDrtLay: TLabel;
    rdeDrtLay: TRbwDataEntry;
    rdeDrtRow: TRbwDataEntry;
    rdeDrtCol: TRbwDataEntry;
    jvspModflowHFB: TJvStandardPage;
    lblHydraulicConductivity: TLabel;
    lblBarrierThickness: TLabel;
    rgAngleAdjustment: TRadioGroup;
    comboHfbHydCond: TComboBox;
    comboHfbThickness: TComboBox;
    jvspModflowHOB: TJvStandardPage;
    lblHeadObservationNames: TLabel;
    comboHeadObservationNames: TComboBox;
    lblHeadObsType: TLabel;
    comboHeadObsType: TComboBox;
    rdeIgnoreValues: TRbwDataEntry;
    lblIgnoreValues: TLabel;
    comboITT: TComboBox;
    lblITT: TLabel;
    jvspModflowMNW2: TJvStandardPage;
    pcMnw2: TPageControl;
    tabBasic: TTabSheet;
    lblWellId: TLabel;
    lblLossType: TLabel;
    lblPartialPenetration: TLabel;
    lblZPump: TLabel;
    tabLossControls: TTabSheet;
    lblWellRadius: TLabel;
    lblSkinRadius: TLabel;
    lblBCoefficient: TLabel;
    lblCCoefficient: TLabel;
    lblPCoefficient: TLabel;
    lblCellToWellConductance: TLabel;
    lblKSkin: TLabel;
    tabDischargeAdjustment: TTabSheet;
    lblReferenceHead: TLabel;
    lblLiftQ0: TLabel;
    lblLiftQMax: TLabel;
    lblWellTolerance: TLabel;
    lblMnw2PumplocX: TLabel;
    lblMnw2PumplocY: TLabel;
    lblMnw2PumplocZ: TLabel;
    lblPumpLocation: TLabel;
    comboMnw2WellId: TComboBox;
    comboMnw2LossType: TComboBox;
    comboSpecifyPump: TComboBox;
    lblSpecifyPump: TLabel;
    comboZPump: TComboBox;
    comboMnw2PumplocX: TComboBox;
    comboMnw2PumplocY: TComboBox;
    comboMnw2PumplocZ: TComboBox;
    lblConstrainPumping: TLabel;
    comboConstrainPumping: TComboBox;
    lblPartialPenetrationFlag: TLabel;
    comboPartialPenetrationFlag: TComboBox;
    comboPartialPenetration: TComboBox;
    lblPumpCap: TLabel;
    comboPumpCap: TComboBox;
    comboWellRadius: TComboBox;
    comboSkinRadius: TComboBox;
    comboKSkin: TComboBox;
    comboBCoefficient: TComboBox;
    comboCCoefficient: TComboBox;
    comboPCoefficient: TComboBox;
    comboCellToWellConductance: TComboBox;
    comboReferenceHead: TComboBox;
    comboLiftQ0: TComboBox;
    comboLiftQMax: TComboBox;
    comboWellTolerance: TComboBox;
    rpShapeCompiler: TRbwParser;
    memoMultipleParts: TMemo;
    pnlDataTop: TPanel;
    cbSelect: TCheckBox;
    comboInterpolaters: TComboBox;
    jvspModflowSTR: TJvStandardPage;
    lblConductanceInterpSTR: TLabel;
    comboConductanceInterpSTR: TComboBox;
    lblStrSegmentNumber: TLabel;
    comboStrSegmentNumber: TComboBox;
    lblParameterName: TLabel;
    comboStrParameterName: TComboBox;
    jvspFootprintWell: TJvStandardPage;
    lblFootprintWell: TLabel;
    comboFootprintWell: TComboBox;
    jvspModflowCFP: TJvStandardPage;
    lblCfpDiameter: TLabel;
    comboCfpDiameter: TComboBox;
    lblCfpTortuosity: TLabel;
    comboCfpTortuosity: TComboBox;
    lblCfpRoughnessHeight: TLabel;
    comboCfpRoughnessHeight: TComboBox;
    lblCfpLowerReynolds: TLabel;
    comboCfpLowerReynolds: TComboBox;
    lblCfbConductance: TLabel;
    comboCfbConductance: TComboBox;
    lblCfpPipeElevation: TLabel;
    comboCfpPipeElevation: TComboBox;
    lblCfpHigherReynolds: TLabel;
    comboCfpHigherReynolds: TComboBox;
    lblCfpSavePipeValues: TLabel;
    comboCfpSavePipeValues: TComboBox;
    lblCfpSaveNodeValues: TLabel;
    comboCfpSaveNodeValues: TComboBox;
    memoShapeFileInfo: TMemo;
    tabCsv: TTabSheet;
    frameCSV: TframeGrid;
    dlgOpenCsv: TOpenDialog;
    jvspModflowSFR_MF6: TJvStandardPage;
    pgcSfrMf6: TPageControl;
    tabSfrMf6Properties: TTabSheet;
    tabSfrMf6DownstreamSegments: TTabSheet;
    lblSegNum: TLabel;
    combolSegNum: TComboBox;
    lblReachLengthMf6: TLabel;
    comboReachLengthMf6: TComboBox;
    lblRwid: TLabel;
    comboRwid: TComboBox;
    lblGrd: TLabel;
    comboGrd: TComboBox;
    lblRtp: TLabel;
    comboRtp: TComboBox;
    lblRbth: TLabel;
    comboRbth: TComboBox;
    lblRhk: TLabel;
    comboRhk: TComboBox;
    frameDownstreamSegmentsSfrMf6: TframeGrid;
    tabDiversions: TTabSheet;
    frameDiversionsSfrMf6: TframeGrid;
    jvspModflowMAW: TJvStandardPage;
    pgcModflowMAW: TPageControl;
    tabMawBasic: TTabSheet;
    tabMawWellScreens: TTabSheet;
    lblMawRadius: TLabel;
    comboMawRadius: TComboBox;
    lblMawBottom: TLabel;
    comboMawBottom: TComboBox;
    lblMawConductanceEquation: TLabel;
    lblMawInitialHead: TLabel;
    comboMawInitialHead: TComboBox;
    comboMawConductanceEquation: TComboBox;
    frameMawWellScreens: TframeGrid;
    jvspModflow6Obs: TJvStandardPage;
    cbHeadObservation: TCheckBox;
    cbDrawdownObservation: TCheckBox;
    cbGroundwaterFlowObservation: TCheckBox;
    lblTypesOfFlowObservation: TLabel;
    chklstFlowObs: TCheckListBox;
    comboModflow6ObsName: TComboBox;
    lblModflow6ObsName: TLabel;
    lblBoundaryFlowObservations: TLabel;
    chklstBoundaryFlow: TCheckListBox;
    tabOptions2: TTabSheet;
    cbImportZ: TCheckBox;
    cbImportMeasured: TCheckBox;
    lblConvert: TLabel;
    comboFromUnits: TComboBox;
    lblTo: TLabel;
    comboToUnits: TComboBox;
    comboObjectNameMethod: TComboBox;
    lblObjectNameMethod: TLabel;
    comboNameAttribute: TComboBox;
    lblNameAttribute: TLabel;
    cbLockObject: TCheckBox;
    comboMultilayer: TComboBox;
    lblMultilayer: TLabel;
    jvspLakMf6: TJvStandardPage;
    comboLakeMf6Embeded: TComboBox;
    lblLakeMf6Embeded: TLabel;
    lblStartingStage: TLabel;
    lblBottomElev: TLabel;
    lblTopElev: TLabel;
    lblLakebedK: TLabel;
    lblLakebedThickness: TLabel;
    lblConnLength: TLabel;
    comboStartingStage: TComboBox;
    comboBottomElev: TComboBox;
    comboTopElev: TComboBox;
    comboLakebedK: TComboBox;
    comboLakebedThickness: TComboBox;
    comboConnLength: TComboBox;
    lblHorizontal: TLabel;
    comboHorizontal: TComboBox;
    comboVertical: TComboBox;
    lblVertical: TLabel;
    // @name edits the formula in @link(edImportCriterion).
    procedure btnImportCriterionClick(Sender: TObject);
    // @name sets all the checkboxes to checked
    // in column 1 of @link(dgFields) to
    // @true if Sender = btnAll.  Otherwise it sets them all to unchecked.
    // @name is the OnClick event-handler for @link(btnAll) and @link(btnNone).
    procedure btnSelectClick(Sender: TObject);
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @name toggles the checkboxes in column 1 of @link(dgFields) from
    // Checked to Unchecked or the reverse.
    procedure btnToggleClick(Sender: TObject);
    // @name activates or deactivates controls related to coordinate conversion
    // and shows or hides points on the image of the UTM zones.
    procedure cbCoordinateConversionClick(Sender: TObject);
    // @name checks that at least one of @link(cbEnclosedCells),
    // @link(cbIntersectedCells), or @link(cbInterpolation) is checked
    // and emphasisizes them if not.  It also disables the OK button until
    // at least one of them is checked.
    procedure cbEnclosedCellsClick(Sender: TObject);
    // @name changes the ellipsoid used for the coordinate conversions.
    procedure comboEllipsoidChange(Sender: TObject);
    // @name draws some cells in a disabled state.
    procedure dgFieldsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    // @name assigns interpolators and @link(TDataArray)s to
    // the picklist for columns 2 and 3.
    // See @link(GetDataSets) and @link(GetInterpolators).
    procedure dgFieldsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    // @name displays the interpolator for the selected @link(TDataArray)
    // in column 3.
    procedure dgFieldsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    // @name checks the formula in edImportCriterion.
    // See @link(CheckImportCriterionFormula).
    procedure edImportCriterionExit(Sender: TObject);
    // @name initialized @classname.
    procedure FormCreate(Sender: TObject); override;
    // @name destroys @link(FGeometryFile).
    procedure FormDestroy(Sender: TObject); override;
    // @name changes the captions of @link(cbEnclosedCells),
    // @link(cbIntersectedCells), and @link(cbInterpolation).
    procedure rgEvaluatedAtClick(Sender: TObject);
    procedure cbImportObjectsClick(Sender: TObject);
    procedure cbImportGridClick(Sender: TObject);
    procedure comboBoundaryChoiceChange(Sender: TObject);
    procedure comboRealFieldChange(Sender: TObject);
    procedure comboBooleanFieldChange(Sender: TObject);
    procedure BoundaryGridBeforeDrawCell(Sender: TObject; ACol, ARow: Integer);
    procedure seWellIntervalsChange(Sender: TObject);
    procedure comboJoinObjectsChange(Sender: TObject);
    procedure seBoundaryTimeCountChange(Sender: TObject);
    procedure rdgBoundaryConditionsDistributeTextProgress(Sender: TObject;
      Position, Max: Integer);
    procedure btnElevFormulaEdit(Sender: TObject);
    procedure edZExit(Sender: TObject);
    procedure edHighZExit(Sender: TObject);
    procedure edLowZExit(Sender: TObject);
    procedure rgElevationCountClick(Sender: TObject);
    procedure comboDrainReturnLocationMethodChange(Sender: TObject);
    procedure dgFieldsColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure dgFieldsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cbSelectClick(Sender: TObject);
    procedure comboInterpolatersChange(Sender: TObject);
    procedure frameGrid1GridButtonClick(Sender: TObject; ACol, ARow: Integer);
    procedure frameCSVGridEndUpdate(Sender: TObject);
    procedure comboFormulaInterpDRTChange(Sender: TObject);
    procedure comboFormulaInterpChange(Sender: TObject);
    procedure frameDiversionsSfrMf6seNumberChange(Sender: TObject);
    procedure cbGroundwaterFlowObservationClick(Sender: TObject);
    procedure comboObjectNameMethodChange(Sender: TObject);
    procedure pcImportShapeChange(Sender: TObject);
  private
    FGeometryFileName: string;
    FIndexFileName: string;
    FDataBaseFileName: string;
    FAllowShapesToCombine: boolean;
    FShouldEnableImportGrid: Boolean;
    // @name stores the TRbwDataType of the fields in the Shapefile.
    FFieldTypes: array of TRbwDataType;
    // @name is the geometry file of the Shapefile.
    FGeometryFile: TShapefileGeometryReader;
    FRealFieldNames: TStringList;
    FIntegerFieldNames: TStringList;
    FBooleanFieldNames: TStringList;
    FStringFieldNames: TStringList;
    FRealFieldAndGlobalVariablesNames: TStringList;
    FRealFieldGlobalsAndDataSetsNames: TStringList;
    FShapeCount: integer;
    FFieldNumbers: TStringList;
    FNumPointsInCurrentShape: Integer;
    FInvalidParameterNames: TStringList;
    FCombinedObjectsAllowed: Boolean;
    FObsCount: Integer;
    FCombinedObjects: boolean;
    FShapeIndex: Integer;
    FMinXVar: TRealVariable;
    FMinYVar: TRealVariable;
    FMaxXVar: TRealVariable;
    FMaxYVar: TRealVariable;
    FShapeType: Integer;
    FMinZVar: TRealVariable;
    FMaxZVar: TRealVariable;
    FConductanceCol: Integer;
    FHeadObsNames: TStringList;
    FCsvAttributes: TCsvAttributes;
    FCsvDictionary: TDictionary<String,TCsvAttribute>;
    FCsvFiles: TStringList;
    FShapeFileValidFieldCount: Integer;
    FShapeFileRealFieldCount: Integer;
    FValidIndiciesCount: Integer;
    FRealFieldGlobalsAndDataSetsNamesCount: Integer;
    FDuplicateTreatment: TDupResponse;
    FScreenObjectsToDelete: TScreenObjectList;
    // @name checks for valid data in @link(dgFields).
    function CheckDataSets: boolean;
    // @name checks that AFormula is a valid formula.
    procedure CheckImportCriterionFormula(AFormula: string);
    // @name creates variables in Parser for each attribute in the shape file.
    procedure CreateVariables(Parser: TRbwParser);
    // @name stores in @link(dgFields).Columns[2].PickList the names
    // of the TDataSets that can be used with the parameter specified
    // in ARow of @link(dgFields).
    procedure GetDataSets(const ARow: integer);
    // @name stores in @link(dgFields).Columns[3].PickList the names
    // of the interpolators that can be used with th parameter in ARow.
    procedure GetInterpolators(const ARow: integer);
    // @name converts a latitude and longitude in degrees to a point
    // on @link(imageUtmZones).
    function LatLongToPoint(Long, Lat: double): TPoint;
    // @name converts a latitude and longitude in degrees to a UTM zone.
    // @name takes into account the non-regular UTM zones.
    function LatLongToUTM_Zone(const LongitudeDegrees, LatitudeDegrees: double):
      integer;
    // @name makes any required new data sets.
    procedure MakeNewDataSets(NewDataSets: TList);
    { Set the captions of @link(cbEnclosedCells), @link(cbIntersectedCells),
      and @link(cbInterpolation) based on @link(rgEvaluatedAt).ItemIndex.}
    procedure SetCheckBoxCaptions;
    // @name imports the Shapes into GoPhast.
    procedure SetData;
    // @name is used to display a progress indication when reeding the
    // Shapefile.
    procedure ShapefileProgress(Sender: TObject; FractionDone: double);
    // @name converts a latitude and longitude in degrees to a UTM zone
    // for the non-special UTM zones.  @seealso(LatLongToUTM_Zone).
    function SimpleLongToUTM_Zone(const LongitudeDegrees: double): integer;
    procedure ImportGrid(FieldNames: TStringList);
    procedure EnableOK;
    procedure InitializeBoundaryConditionControls;
    procedure AssignBoundary(AScreenObject: TScreenObject);
    procedure AssignAPhastSpecifiedHeadBoundary(AScreenObject: TScreenObject);
    procedure AssignAPhastBoundary(Boundary: TCustomInterpolatedBoundary);
    function GetRealFormulaFromText(const Text: string;
      DataSetsOK: boolean = True; FormulaOK: boolean = False): string;
    function GetRealValueFromText(const FieldName: AnsiString; var ShouldIgnore: Boolean): Extended; overload;
    function GetRealValueFromText(const FieldName: String; var ShouldIgnore: Boolean): Extended; overload;
    procedure AssignAPhastLeakyBoundary(AScreenObject: TScreenObject);
    procedure AssignAPhastRiverBoundary(AScreenObject: TScreenObject);
    function GetStringValueFromText(const FieldName: String): string; overload;
    function GetStringValueFromText(const FieldName: AnsiString): string; overload;
    function GetBooleanValueFromText(FieldName: AnsiString): Boolean; overload;
    function GetBooleanValueFromText(FieldName: String): Boolean; overload;
    procedure AssignAPhastWellBoundary(AScreenObject: TScreenObject);
    procedure EnableFeatureImport;
    procedure ImportModflowChdBoundary(AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForCHD;
    procedure InitializeBoundaryControlsForGHB;
    procedure ImportModflowGhbBoundary(AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForWEL;
    procedure ImportModflowWelBoundary(AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForRIV;
    procedure ImportModflowRivPackage(AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForDRN;
    procedure ImportModflowDrnPackage(AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForRCH(Packages: TModflowPackages);
    function GetIntegerValueFromText(const FieldName: AnsiString): integer; overload;
    function GetIntegerValueFromText(const FieldName: String): integer; overload;
    procedure ImportModflowRchPackage(Packages: TModflowPackages; AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForEVT(Packages: TModflowPackages);
    procedure ImportModflowEvtPackage(Packages: TModflowPackages; AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForETS(Packages: TModflowPackages);
    procedure ImportModflowEtsPackage(AScreenObject: TScreenObject;
      Packages: TModflowPackages);
    procedure InitializeBoundaryControlsForHOB;
    procedure InitializeBoundaryControlsForModflow6Obs;
    procedure ImportModflowHobPackage(AScreenObject: TScreenObject);
    procedure ImportModflow6Obs(AScreenObject: TScreenObject);
    function GetFormulaInterpretation(combo: TComboBox): TFormulaInterpretation;
    procedure AssignColFeatureProperties;
    procedure EnableEvalAt;
    procedure AssignInterpolator(DataSet: TDataArray; Index: Integer;
      out NewProperties, OldProperties: TPhastDataSetStorage);
    procedure ChangeInterpolators(NewProperties, OldProperties: TList);
    procedure InitializeBoundaryControlsForSFR;
    procedure ImportModflowSfrPackage(AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForSFR_MF6;
    procedure ImportModflowSfr_MF6_Package(AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForLAK;
    procedure ImportModflowLakPackage(AScreenObject: TScreenObject);
    procedure CheckElevationFormula(Edit: TRbwEdit; AFormula: string);
    procedure InitializeBoundaryControlsForDRT;
    procedure ImportModflowDrtPackage(AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForRES(Packages: TModflowPackages);
    procedure ImportModflowResPackage(AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForHFB;
    procedure ImportModflowHfbPackage(AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForUZF;
    procedure ImportModflowUzfPackage(AScreenObject: TScreenObject);
    function GetIntegerFormulaFromText(const text: AnsiString;
      DataSetsOK: boolean = True): string; overload;
    function GetIntegerFormulaFromText(const text: String;
      DataSetsOK: boolean = True): string; overload;
    procedure CreateDataSetVariables(Parser: TRbwParser;
      EvalAt: TEvaluatedAt);
    function DataArrayOrientationOK(DataArray: TDataArray): boolean;
    procedure AddModflowPackageToImportChoices(
      APackage: TModflowPackageSelection);
    procedure InitializeBoundaryControlsForMnw2;
    procedure ImportModflowMnw2Package(AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForMAW;
    procedure ImportModflowMawPackage(AScreenObject: TScreenObject);
    procedure GetTransientParameter(var Param: TModflowTransientListParameter;
      var ParameterName: string; ParameterColumn: Integer; Row: Integer);
    procedure GetNewOrExistingBoundaryItem(
      var AnItem: TCustomModflowBoundaryItem;
      const ParameterName: string; var Param: TModflowTransientListParameter;
      var ParamItem: TModflowParamItem; Boundary: TModflowParamBoundary;
      ItemIndex: integer);
    procedure AddParameterNamesToPickList(ParameterType: TParameterType;
      ParameterColumn: Integer);
    procedure EnableJoinObjects;
    function GetFieldNumberFromName(CellText: AnsiString): Integer;
    procedure Fill_comboInterpolaters;
    function AssociatedConcColumns: integer;
    function CreateConcItem(ConcBoundary: TMt3dmsConcBoundary;
      ItemIndex: Integer; Item: TCustomModflowBoundaryItem): TMt3dmsConcItem;
    function CreateConcBoundary(AScreenObject: TScreenObject): TMt3dmsConcBoundary;
    procedure ImportConcItemForCombinedShapes(ConcItem: TMt3dmsConcItem;
      StartingConcIndex: Integer;  ItemIndex: Integer;
      AScreenObject: TScreenObject);
    function GetConcSpeciesItem(AComp: TMobileChemSpeciesItem;
      ConcIndex: Integer; ConcItem: TMt3dmsConcItem): TStringConcValueItem;
    procedure ImportConcItemForSeparateShapes(ItemIndex: Integer;
      ConcItem: TMt3dmsConcItem; StartingConcIndex: Integer);
    procedure InitializeColumnsForMt3dConc(StartingConcIndex: Integer);
    procedure ImportModflowStrPackage(AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForSTR;
    procedure InitializeBoundaryControlsForFootprintWell;
    procedure ImportFootprintWell(AScreenObject: TScreenObject);
    procedure ImportModflowCFP_Pipe(AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForCFP_Pipe;
//    procedure ChangeConductInterp(Value: Integer);
    procedure ReadCsvAttributeNames;
    procedure AssignRealFieldNamesToControls;
    function GetValueFromCsv(AttribName: string; var ShouldIgnore: Boolean): double;
    procedure AssignRealValueAttributesToControls;
    Procedure HowTreatDuplicateNames;
    procedure AssignFormulaInterpretationText(combo: TComboBox);
    procedure InitializeBoundaryControlsForLAKMf6;
    procedure ImportModflowLakMf6Package(AScreenObject: TScreenObject);
    { Private declarations }
  public
    // @name returns @true if the Shapefile is selected.
    function GetData: boolean;
    { Public declarations }
  end;

  TFieldNumStorage = class(TObject)
    FXBase: TXBase;
    FieldNumber: integer;
    Formula: string;
    RealValue: double;
    IntValue: integer;
    StringValue: string;
    Cached: boolean;
    function GetRealValue: double;
    function GetIntValue: integer;
    function GetStringValue: string;
    Constructor Create(XBase: TXBase);
    function RealFormula: string;
    function IntFormula: string;
    function StringFormula: string;
  end;

implementation

uses Math, Contnrs , frmGoPhastUnit, frmProgressUnit,
  frmDataSetsUnits, ModelMuseUtilities, frmShowHideObjectsUnit,
  CoordinateConversionUnit, frmFormulaUnit, FastGEO, RealListUnit,
  ValueArrayStorageUnit, GIS_Functions, PhastModelUnit, TimeUnit,
  ModflowConstantHeadBoundaryUnit, ModflowGhbUnit, ModflowWellUnit, 
  ModflowRivUnit, ModflowDrnUnit, ModflowRchUnit, ModflowEvtUnit, 
  ModflowEtsUnit, ModflowHobUnit, ModflowSfrReachUnit,
  ModflowSfrParamIcalcUnit, ModflowSfrFlows, ModflowSfrChannelUnit,
  ModflowSfrEquationUnit, ModflowSfrSegment, ModflowSfrUnit, ModflowTimeUnit, 
  ModflowLakUnit, ModflowDrtUnit, ModflowResUnit, ModflowHfbUnit, 
  ModflowUzfUnit, GlobalVariablesUnit, frameScreenObjectMNW2Unit,
  ModflowMnw2Unit, frmErrorsAndWarningsUnit, ModflowSfrTable, frmGridValueUnit,
  LayerStructureUnit, ModflowStrUnit, FootprintBoundary, ModflowCfpPipeUnit,
  System.IOUtils, SutraMeshUnit, SubscriptionUnit, FootPrintUtilities,
  ModflowSfr6Unit, ModflowMawUnit, Modflow6ObsUnit, frameScreenObjectSfr6Unit,
  ModflowBoundaryDisplayUnit, Mt3dUztRchUnit, Mt3dUztSatEtUnit,
  Mt3dUztUnsatEtUnit, ModflowLakMf6Unit;

resourcestring
  StrParameterName = 'Parameter name';
  StrImportShapeFile = 'import shape file';
  StrWarningRoot = 'No parameters with the following names exist. '
    + 'Import of the feature will be skipped for the shapes for which these '
    + 'names were specified.';
  StrImport = 'Import';
  StrDataSet = 'Data Set';
  StrInterpolation = 'Interpolation';
  StrTheShpFileS = 'The ".shp" file "%s" does not exist.';
  StrTheDbfFileS = 'The ".dbf" file "%s" does not exist.  Do you want to jus' +
  't import the geometry of the shapes in the shape file';
  StrReadingShapeGeomet = 'Reading Shape Geometry File';
  StrReadingShape1 = 'Reading shape 1';
  StrNumberOfShapes = 'Number of shapes = %s';
  StrNoneOfTheFieldsI = 'None of the fields in "%s" can be imported.  Do you' +
  ' want to just import the geometry of the shapes in the shape file';
  StrNone = 'None';
  StrTheShapesCanNotB = 'The shapes can not be combined because the followin' +
  'g shapes have multiple parts.';
  StrDataSets = 'Data Sets';
  StrSPECIFIC = 'SPECIFIC';
  StrDIRECT = 'DIRECT';
  StrTOTAL = 'TOTAL';
  StrNONE_UC = 'NONE';
  StrTHIEM = 'THIEM';
  StrSKIN = 'SKIN';
  StrGENERAL = 'GENERAL';
  StrSPECIFYCWC = 'SPECIFYCWC';
  StrUZFRechargeRated = 'UZF_RechargeRate%d';
  StrUZFEvapotranspirati = 'UZF_EvapotranspirationRate%d';
  StrUZFExtinctionDepth = 'UZF_ExtinctionDepth%d';
  StrUZFWaterContentd = 'UZF_WaterContent%d';
  StrHFBHydraulicConduct = 'HFB_HydraulicConductivity';
  StrHFBThickness = 'HFB_Thickness';
  StrInfiltrationRate = 'Infiltration rate';
  StrEvapoTranspiration = 'Evapo- transpiration demand';
  StrETExtinctionDepth = 'ET extinction depth';
  StrETExtinctionWater = 'ET extinction water content';
  StrMinimumStage = 'Minimum stage';
  StrMaximumStage = 'Maximum stage';
  StrPrecipitation = 'Precipitation';
  StrEvaporation = 'Evaporation';
  StrOverlandRunoff = 'Overland runoff';
  StrWithdrawal = 'Withdrawal';
  StrFootprintWithdrawal = 'Withdrawal';
  StrOutflowSegments = 'Outflow Segments';
  StrICALC = 'ICALC';
  StrFLOW = 'FLOW';
  StrDiversionSegments = 'Diversion Segments';
  StrIPRIOR = 'IPRIOR';
  StrPTSW = 'PTSW';
  StrETSW = 'ETSW';
  StrRUNOFF = 'RUNOFF';
  StrROUGHCH = 'ROUGHCH';
  StrROUGHBK = 'ROUGHBK';
  StrCDPTH = 'CDPTH';
  StrFDPTH = 'FDPTH';
  StrAWDTH = 'AWDTH';
  StrBWDTH = 'BWDTH';
  StrHCOND1 = 'HCOND1';
  StrTHICKM1 = 'THICKM1';
  StrELEVUP = 'ELEVUP';
  StrWIDTH1 = 'WIDTH1';
  StrDEPTH1 = 'DEPTH1';
  StrHCOND2 = 'HCOND2';
  StrTHICKM2 = 'THICKM2';
  StrELEVDN = 'ELEVDN';
  StrWIDTH2 = 'WIDTH2';
  StrDEPTH2 = 'DEPTH2';
  StrXCPT1 = 'XCPT1';
  StrXCPT2 = 'XCPT2';
  StrXCPT3 = 'XCPT3';
  StrXCPT4 = 'XCPT4';
  StrXCPT5 = 'XCPT5';
  StrXCPT6 = 'XCPT6';
  StrXCPT7 = 'XCPT7';
  StrXCPT8 = 'XCPT8';
  StrZCPT1 = 'ZCPT1';
  StrZCPT2 = 'ZCPT2';
  StrZCPT3 = 'ZCPT3';
  StrZCPT4 = 'ZCPT4';
  StrZCPT5 = 'ZCPT5';
  StrZCPT6 = 'ZCPT6';
  StrZCPT7 = 'ZCPT7';
  StrZCPT8 = 'ZCPT8';
  StrTime = 'Time';
  StrObservedHead = 'Observed head';
  StrStatistic = 'Statistic';
  StrStatFlag = 'Stat Flag';
  StrRESStartHeadd = 'RES_StartHead%d';
  StrRESEndingHeadd = 'RES_EndingHead%d';
  StrETSEvapotranspirati = 'ETS_EvapotranspirationRate%d';
  StrETSEvapotranspiratiS = 'ETS_EvapotranspirationSurface%d';
  StrETSEvapotranspiratiD = 'ETS_EvapotranspirationDepth%d';
  StrETSDepthFraction0 = 'ETS_DepthFraction%0:d_%1:d';
  StrETSEtFraction0d = 'ETS_EtFraction%0:d_%1:d';
  StrETSLayerd = 'ETS_Layer%d';
  StrEvapoTranspirationR = 'Evapo- transpiration rate';
  StrEvapoTranspirationS = 'Evapo- transpiration surface';
  StrEvapoTranspirationDe = 'Evapo- transpiration depth';
  StrFractionalDepthD = 'Fractional depth %d';
  StrFractionalRateD = 'Fractional rate %d';
  StrEvapoTranspirationL = 'Evapo- transpiration layer';
  StrEVTEvapotranspirati = 'EVT_EvapotranspirationRate%d';
  StrEVTEvapotranspiratiS = 'EVT_EvapotranspirationSurface%d';
  StrEVTEvapotranspiratiD = 'EVT_EvapotranspirationDepth%d';
  StrEVTLayerd = 'EVT_Layer%d';
  StrRCHLayerd = 'RCH_Layer%d';
  StrRechargeRate = 'Recharge rate';
  StrRechargeLayer = 'Recharge layer';
  StrDRTElevationd = 'DRT_Elevation%d';
  StrDRTConductanced = 'DRT_Conductance%d';
  StrDRTReturnFractiond = 'DRT_ReturnFraction%d';
  StrDRNElevationd = 'DRN_Elevation%d';
  StrDRNConductanced = 'DRN_Conductance%d';
  StrConductanceInterpre = 'Conductance interpretation';
  StrElevation = 'Elevation';
  StrReturnFraction = 'Return fraction';
  StrRIVBottomd = 'RIV_Bottom%d';
  StrRIVStaged = 'RIV_Stage%d';
  StrRIVConductanced = 'RIV_Conductance%d';
  StrWELPumpingRated = 'WEL_PumpingRate%d';
  StrPumpingRateInterpr = 'Pumping rate interpretation';
  StrGHBBoundaryHeadd = 'GHB_BoundaryHead%d';
  StrGHBConductanced = 'GHB_Conductance%d';
  StrCHDStartingHeadd = 'CHD_StartingHead%d';
  StrCHDEndingHeadd = 'CHD_EndingHead%d';
  StrELEVATION_UC = 'ELEVATION';
  StrDEPTH_UC = 'DEPTH';
  StrSPECIFIED_UC = 'SPECIFIED';
  StrASSOCIATED_UC = 'ASSOCIATED';
  StrSpecifiedHead = 'Specified head';
  StrFluxBoundary = 'Flux boundary';
  StrLeakyBoundary = 'Leaky boundary';
  StrRiverBoundary = 'River boundary';
  StrWellBoundary = 'Well boundary';
  StrTheShapefileAppear = 'The Shapefile appears not to contain grid informa' +
  'tion.';
  StrCreatingGrid = 'Creating Grid';
  Str0OutOfD = '0 out of %d.';
  StrShape = 'Shape ';
  StrUnableToImportGri = 'Unable to import grid.';
  StrHasBeenSelected = '" has been selected for two or more fields that are ' +
  'being imported.  You need to correct this before continuing.';
  Str0sContainsInte = '"%0:s" contains integers but %1:s does not.  You need' +
  ' to correct this before continuing.';
  Str0sContainsReal = '"%0:s" contains real numbers but %1:s does not.  You ' +
  'need to correct this before continuing.';
  Str0sContainsBool = '"%0:s" contains booleans but %1:s does not.  You need' +
  ' to correct this before continuing.';
  Str0sContainsStri = '"%0:s" contains strings but %1:s does not.  You need ' +
  'to correct this before continuing.';
  StrCreatedFromShapefi = 'Created from Shapefile Attribute in ';
  StrCreatingObjects = 'Creating Objects';
  StrObject0dOutOf = 'Object %0:d out of %1:d.  Point %2:d out of %3:d.';
  StrAssigningFormulas = 'Assigning Formulas';
  Str0OutOf0d = '0 out of %0:d.';
  StrFormula = 'Formula ';
  StrDObjectsWereInva = '%d objects were invalid because they cross themselv' +
  'es and have been skipped.  Do you want to see the numbers of the ones tha' +
  't have been skipped?';
  StrDObjectsHadAttri = '%d objects had attributes that could not be read in' +
  'to ModelMuse properly. They have been skipped.  Do you want to see the nu' +
  'mbers of the ones that have been skipped?';
  StrReadingShapeS = 'Reading shape %s';
  StrSorryImportingThe = 'Sorry. Importing the grid while performing a coord' +
  'inate conversion is not allowed.';
  StrCoordinatesOfFirst = 'Coordinates of first point = (%0:g, %1:g).';
  StrShapeNS = 'Shape%d';
  StrOneOrMoreOfYour = 'One or more of your data points appear to has invali' +
  'd coordinates. Coordinate conversion can not be performed on this shape f' +
  'ile. Coordinates must be in decimal degrees to be converted.';
  StrHead = 'Head';
  StrSolution = 'Solution';
  StrFlux = 'Flux';
  StrPumpingRange = 'Pumping Range;';
  StrConcentrationS = 'Concentration: %s';
  StrRechConcentrationS = 'Recharge Concentration: %s';
  StrSatETConcentrationS = 'Sat ET Concentration: %s';
  StrUnSatETConcentrationS = 'Unsat ET Concentration: %s';
  StrConcentrationsd = 'Concentration_%s%d';
  StrYouMustChangeThe = 'You must change the number of Z-formulas to zero if' +
  ' you want the imported shapes to affect the layer definition data sets.';
  StrThereWasAnErrorP = 'There was an error processing the database file tha' +
  't is part of the Shapefile. The error was "%s" If you can not solve this' +
  'problem yourself, you may contact the ModelMuse developer.';
  StrTheNumberOfZform = 'The number of Z-formulas has been changed to 1 beca' +
  'use pipes in the conduit flow process must have one and only one Z-formul' +
  'a.';
  StrPipesIn = 'Pipes in ';
  StrThereShouldBe0d = 'There should be %0:d lines in %1:s to match the numb' +
  'er of Shapes in the shape file but there are actually %2:d lines.';
  StrSIsAnInvalidSha = '%s is an invalid Shapefile.';
  StrNumberOfShapesD = 'Number of shapes = %d';
  StrMinimumXG = 'Minimum X = %g';
  StrMaximumXG = 'Maximum X = %g';
  StrMinimumYG = 'Minimum Y = %g';
  StrMaximumYG = 'Maximum Y = %g';
  StrMinimumZG = 'Minimum Z = %g';
  StrMaximumZG = 'Maximum Z = %g';
  StrCommaSeparatedValu = 'Comma-Separated Value (CSV) files';
  StrDownstreamSegmentN = 'Downstream Segment numbers';
  StrDiversionSegmentNu = 'Diversion Segment numbers';
  StrInactive = 'Inactive';
  StrActive = 'Active';
//  StrSimple = 'Simple';
  StrDiversionRate = 'Diversion Rate ';
  StrConstantHead = 'Constant Head';
  StrNotFlowing = 'Not Flowing';
  StrFlowing = 'Flowing';
  StrFlowingWell = 'Flowing Well';
  StrRateLimitation = 'Rate Limitation';
//  StrNone = 'None';
  StrScaling = 'Scaling';
  StrShutoff = 'Shutoff';
  StrHeadNotLimited = 'Head not limited';
  StrHeadLimited = 'Head limited';
  StrWellHead = 'Well Head';
  StrFlowingWellElevati = 'Flowing Well Elevation';
  StrFlowingWellConduct = 'Flowing Well Conductance';
  StrMinimumFlowRate = 'Minimum Flow Rate';
  StrMaximumFlowRate = 'Maximum Flow Rate';
  StrPumpElevation = 'Pump Elevation';
  StrScalingLength = 'Scaling Length';
  StrHeadLimit = 'Head Limit';
  StrScreenTop = 'Screen Top';
  StrScreenBottom = 'Screen Bottom';
  StrSkinHydraulicCondu = 'Skin Hydraulic Conductivity';
  StrSkinRadius = 'Skin Radius';
  StrTheAttributeNamed = 'The attribute named %0:s on row %1:d has the same ' +
  'name as another attribute. It will be skipped.';
  StrTheCSVAttributeNa = 'The CSV attribute named %0:s has the same ' +
  'name as another attribute. It will be skipped.';
  StrTheAttribute0sI = 'The attribute #%0:d (%1:s) in file "%2:s" is a duplicate and' +
  ' will be skipped.';
  StrUZTRechConcd = 'UZT_RechConc_%0:d_%1:d';
  StrUZTSatEtConcd = 'UZT_SatEtConc_%0:d_%1:d';
  StrUZTUnSatEtConcd = 'UZT_UnSatEtConc_%0:d_%1:d';

const
  StrShapeMinX = 'ShapeMinX';
  StrShapeMinY = 'ShapeMinY';
  StrShapeMaxX = 'ShapeMaxX';
  StrShapeMaxY = 'ShapeMaxY';
  StrShapeMinZ = 'ShapeMinZ';
  StrShapeMaxZ = 'ShapeMaxZ';
//  StrFootprintWithdrawal = 'Withdrawal';

var
  PestObsTypes: TStringList;

{$R *.dfm}

type
  TFieldGridColumns = (fgcAttributes, fgcImport, fgcDataSet, fgcInterpolator);

  TValueRecord = record
    StringData: string;
    case DataType: TRbwDataType of
      rdtDouble: (RealData: double);
      rdtInteger: (IntData: integer);
      rdtBoolean: (BoolData: boolean);
  end;

  TValueObject = class(TObject)
    Data: TValueRecord;
  end;

  TValueBool = class(TBooleanVariable)
  public
    function Decompile: string; override;
  end;

  TValueInt = class(TIntegerVariable)
  public
    function Decompile: string; override;
  end;

  TValueReal = class(TRealVariable)
  public
    function Decompile: string; override;
  end;

  TValueStr = class(TStringVariable)
    function Decompile: string; override;
  end;

  TDecompileType = (dcNormal, dcValue);

var
  GlobalDecompileType: TDecompileType = dcNormal;

const
  StrAttribute = 'Attribute';
  StrAttributes = 'Attributes';

function ConvertPoint(const ShapePoint: TShapePoint): TPoint2D;
begin
  result.X := ShapePoint.X;
  result.Y := ShapePoint.Y;
end;

function DistanceOriginToLine(const StartingPoint, EndingPoint: TShapePoint;
  out DistanceToOrigin: double): boolean;
const
  Origin: TPoint2D = (X: 0; Y: 0);
var
  LineMag: double;
  U: double;
  Intersection: TPoint2D;
  StartPoint, EndPoint: TPoint2D;
begin
  StartPoint := ConvertPoint(StartingPoint);
  EndPoint := ConvertPoint(EndingPoint);
  LineMag := Distance( EndPoint, StartPoint );
  result := LineMag > 0;
  if not result then
  begin
    Exit;
  end;

  U := ( ( ( Origin.X - StartPoint.X ) * ( EndPoint.X - StartPoint.X ) ) +
      ( ( Origin.Y - StartPoint.Y ) * ( EndPoint.Y - StartPoint.Y ) )  ) /
      ( Sqr(LineMag) );

  Intersection.X := StartPoint.X + U * ( EndPoint.X - StartPoint.X );
  Intersection.Y := StartPoint.Y + U * ( EndPoint.Y - StartPoint.Y );

  DistanceToOrigin := Distance( Origin, Intersection );
end;

function FieldToVarName(AString: string): string;
var
  Index: Integer;
begin
  result := AString;
  if Length(result) > 0 then
  begin
    if not CharInSet(result[1], ['_', 'A'..'Z', 'a'..'z']) then
    result := '_' + result;
    for Index := 2 to Length(result) do
    begin
      if not CharInSet(result[Index], ['_', 'A'..'Z', 'a'..'z', '0'..'9']) then
      begin
        result[Index] := '_';
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.FormCreate(Sender: TObject);
begin
  inherited;
  FCsvAttributes := TCsvAttributes.Create;
  FCsvDictionary := TDictionary<String,TCsvAttribute>.Create;
  FCsvFiles := TStringList.Create;
  FCsvFiles.OwnsObjects := True;

  FObsCount := 0;
  FCombinedObjectsAllowed := True;
  FFieldNumbers := TStringList.Create;
  FFieldNumbers.CaseSensitive := False;
  FFieldNumbers.Sorted := True;

  FRealFieldNames := TStringList.Create;
  FIntegerFieldNames := TStringList.Create;
  FBooleanFieldNames := TStringList.Create;
  FBooleanFieldNames.Add('False');
  FBooleanFieldNames.Add('True');
  FStringFieldNames := TStringList.Create;
  FRealFieldAndGlobalVariablesNames := TStringList.Create;
  FRealFieldGlobalsAndDataSetsNames := TStringList.Create;

  cbEnclosedCellsClick(nil);
  SetCheckBoxCaptions;
  comboEllipsoid.ItemIndex := 2;
  comboEllipsoidChange(nil);

  pcImportShape.ActivePageIndex := 0;

  dgFields.Cells[Ord(fgcAttributes), 0] := StrAttribute;
  dgFields.Cells[Ord(fgcImport), 0] := StrImport;
  dgFields.Cells[Ord(fgcDataSet), 0] := StrDataSet;
  dgFields.Cells[Ord(fgcInterpolator), 0] := StrInterpolation;
  dgFields.ColWidths[Ord(fgcDataSet)] := 120;

  frameCSV.Grid.Cells[0,0] := StrCommaSeparatedValu;

  pcSFR.ActivePageIndex := 0;

  InitializeBoundaryConditionControls;
  EnableFeatureImport;
  Fill_comboInterpolaters;

  if frmGoPhast.ModelSelection = msFootPrint then
  begin
    rdgBoundaryConditions.Visible := False;
    seBoundaryTimeCount.Visible := False;
    lblBoundaryTimeCount.Visible := False;
    plBoundary.Align := alClient;
//    tabFeatures.Caption := StrWellWithdrawals;
  end;

  FScreenObjectsToDelete := TScreenObjectList.Create;
end;

procedure TfrmImportShapefile.FormDestroy(Sender: TObject);
var
  Index: Integer;
begin
  inherited;
  FScreenObjectsToDelete.Free;
  FHeadObsNames.Free;
  FRealFieldGlobalsAndDataSetsNames.Free;
  FRealFieldAndGlobalVariablesNames.Free;
  FRealFieldNames.Free;
  FIntegerFieldNames.Free;
  FBooleanFieldNames.Free;
  FStringFieldNames.Free;
  FGeometryFile.Free;
  for Index := 0 to FFieldNumbers.Count - 1 do
  begin
    FFieldNumbers.Objects[Index].Free;
  end;
  FFieldNumbers.Free;
  FCsvAttributes.Free;
  FCsvDictionary.Free;
  FCsvFiles.Free;
end;

function TfrmImportShapefile.GetValueFromCsv(AttribName: string; var ShouldIgnore: Boolean): double;
var
  AnAttribute: TCsvAttribute;
  FileIndex: Integer;
  AttFile: TStringList;
  Splitter: TStringList;
  Text: String;
  AValue: Extended;
begin
  result := 0;
  if FCsvDictionary.TryGetValue(AttribName, AnAttribute) then
  begin
    FileIndex := FCsvFiles.IndexOf(AnAttribute.FileName);
    if FileIndex >= 0 then
    begin
      AttFile := FCsvFiles.Objects[FileIndex] as TStringList;
      Splitter := TStringList.Create;
      try
        Splitter.StrictDelimiter := True;
        Splitter.DelimitedText := AttFile[FShapeIndex+1];
        if Splitter.Count > AnAttribute.Position then
        begin
          Text := Splitter[AnAttribute.Position];
          ShouldIgnore := not TryFortranStrToFloat(Text, AValue);
          result := AValue;
        end;
      finally
        Splitter.Free;
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.ReadCsvAttributeNames;
var
  FileIndex: Integer;
  AFileName: string;
  Splitter: TStringList;
  AttFile: TStringList;
  Warnings: TStringList;
  AttribIndex: Integer;
  AnAttribute: TCsvAttribute;
  AttributeName: string;
  procedure InitializeAttributeObjects;
  begin
    FCsvAttributes.Clear;
    FCsvDictionary.Clear;
    FCsvFiles.Clear;
    while FRealFieldNames.Count > FShapeFileRealFieldCount do
    begin
      FRealFieldNames.Delete(FRealFieldNames.Count-1);
    end;
    while FRealFieldGlobalsAndDataSetsNames.Count > FRealFieldGlobalsAndDataSetsNamesCount do
    begin
      FRealFieldGlobalsAndDataSetsNames.Delete(FRealFieldGlobalsAndDataSetsNames.Count-1);
    end;

    SetLength(FFieldTypes, FValidIndiciesCount);


  end;
begin
  Splitter := TStringList.Create;
  Warnings := TStringList.Create;
  try
    InitializeAttributeObjects;
    for FileIndex := 1 to frameCSV.seNumber.AsInteger do
    begin
      AFileName := frameCSV.Grid.Cells[0, FileIndex];
      if TFile.Exists(AFileName) then
      begin
        AttFile := TStringList.Create;
        FCsvFiles.AddObject(AFileName, AttFile);
        try
          AttFile.LoadFromFile(AFileName);
        except on E: EFOpenError do
          begin
            InitializeAttributeObjects;
            Beep;
            MessageDlg(E.Message, mtError, [mbOK], 0);
            Exit;
          end;
        end;
        if AttFile.Count > 0 then
        begin
          if FShapeCount <> AttFile.Count -1 then
          begin
            Warnings.Add(Format(StrThereShouldBe0d,
              [FShapeCount+1, AFileName, AttFile.Count]));
          end;

          Splitter.StrictDelimiter := True;
          Splitter.DelimitedText := AttFile[0];
          for AttribIndex := 0 to Splitter.Count - 1 do
          begin
            AttributeName := FieldToVarName(Trim(Splitter[AttribIndex]));
            if FCsvDictionary.ContainsKey(AttributeName) then
            begin
              Beep;
              MessageDlg(Format(StrTheAttribute0sI,
                [AttribIndex+1, AttributeName, AFileName]), mtWarning, [mbOK], 0);
            end
            else
            begin
              AnAttribute := TCsvAttribute.Create;
              AnAttribute.FileName := AFileName;
              AnAttribute.AttributeName := AttributeName;
              AnAttribute.Position := AttribIndex;
              FCsvAttributes.Add(AnAttribute);
              FRealFieldNames.Add(AnAttribute.AttributeName);
              FRealFieldGlobalsAndDataSetsNames.Add(AnAttribute.AttributeName);
              FCsvDictionary.Add(AnAttribute.AttributeName, AnAttribute);
            end;
          end;
        end;
      end;
    end;
    dgFields.RowCount := FShapeFileValidFieldCount + FCsvAttributes.Count + 1;
    SetLength(FFieldTypes, FValidIndiciesCount + FCsvAttributes.Count);

    for AttribIndex := 0 to FCsvAttributes.Count - 1 do
    begin
      dgFields.Cells[Ord(fgcAttributes), AttribIndex+FShapeFileValidFieldCount+1]
        := FCsvAttributes[AttribIndex].AttributeName;
      dgFields.Cells[Ord(fgcDataSet), AttribIndex+FShapeFileValidFieldCount + 1] := rsNewDataSet;
      dgFields.Cells[Ord(fgcInterpolator), AttribIndex+FShapeFileValidFieldCount + 1] := StrNone;
      FFieldTypes[AttribIndex+FShapeFileValidFieldCount] := rdtDouble;
    end;

    AssignRealFieldNamesToControls;
    FCsvFiles.Sorted := True;

    if Warnings.Count > 0 then
    begin
      Beep;
      MessageDlg(Warnings.Text, mtWarning, [mbOK], 0);
    end;

  finally
    Splitter.Free;
    Warnings.Free;
  end;
end;

procedure TfrmImportShapefile.frameCSVGridEndUpdate(Sender: TObject);
begin
  inherited;
  frameCSV.GridEndUpdate(Sender);
  ReadCsvAttributeNames;
  comboBoundaryChoiceChange(nil);
end;

procedure TfrmImportShapefile.frameDiversionsSfrMf6seNumberChange(
  Sender: TObject);
var
  NoDiversionColCount: Integer;
  ItemIndex: Integer;
  ColIndex: Integer;
begin
  inherited;
  frameDiversionsSfrMf6.seNumberChange(Sender);
  NoDiversionColCount := Ord(High(TSfr_Mf6_Column)) + 1;
  rdgBoundaryConditions.ColCount := NoDiversionColCount + frameDiversionsSfrMf6.seNumber.AsInteger;
  ItemIndex := 1;
  rdgBoundaryConditions.BeginUpdate;
  try
    for ColIndex := NoDiversionColCount to rdgBoundaryConditions.ColCount - 1 do
    begin
      rdgBoundaryConditions.Cells[ColIndex, 0] := StrDiversionRate + ItemIndex.ToString;

      rdgBoundaryConditions.Columns[ColIndex].ComboUsed := True;
      rdgBoundaryConditions.Columns[ColIndex].Format := rcf4String;
      rdgBoundaryConditions.Columns[ColIndex].PickList := FRealFieldGlobalsAndDataSetsNames;

      Inc(ItemIndex);
    end;
    AssignColFeatureProperties;
  finally
    rdgBoundaryConditions.EndUpdate;
  end;

end;

procedure TfrmImportShapefile.frameGrid1GridButtonClick(Sender: TObject; ACol,
  ARow: Integer);
var
  FileIndex: Integer;
begin
  inherited;
  if dlgOpenCsv.Execute then
  begin
    frameCSV.Grid.BeginUpdate;
    try
      frameCSV.seNumber.AsInteger := Max(dlgOpenCsv.Files.Count + ARow -1,
        frameCSV.seNumber.AsInteger);
      frameCSV.seNumber.OnChange(nil);
      for FileIndex := 0 to dlgOpenCsv.Files.Count - 1 do
      begin
        frameCSV.Grid.Cells[0, FileIndex+ARow] := dlgOpenCsv.Files[FileIndex];
      end;
    finally
      frameCSV.Grid.EndUpdate;
    end;
  end;
end;

function TfrmImportShapefile.GetBooleanValueFromText(
  FieldName: String): Boolean;
begin
  result := GetBooleanValueFromText(AnsiString(FieldName));
end;

function TfrmImportShapefile.GetData: boolean;
var
  FilesOK: boolean;
  Index: integer;
  ValidFields: TStringList;
  ValidIndicies: TIntegerList;
  ShapeIndex: Integer;
  Shape: TShapeObject;
  VarIndex: Integer;
  Variable: TGlobalVariable;
  DSIndex: Integer;
  DataArray: TDataArray;
  DataArrayManager: TDataArrayManager;
begin
  inherited;
  try
    dgFields.DefaultRowHeight := dgFields.Canvas.TextHeight('Fields')+4;

    rgEvaluatedAt.Items[Ord(eaBlocks)] := EvalAtToString(eaBlocks,
      frmGoPhast.PhastModel.ModelSelection, True, True);
    rgEvaluatedAt.Items[Ord(eaNodes)] := EvalAtToString(eaNodes,
      frmGoPhast.PhastModel.ModelSelection, True, True);
    EnableEvalAt;

    FShouldEnableImportGrid := False;
    FAllowShapesToCombine := True;
    FilesOK := False;
    try
      result := OpenDialogShape.Execute;
      if result then
      begin
        FGeometryFileName := OpenDialogShape.FileName;
        Caption := Caption + ' - ' + FGeometryFileName;
        FIndexFileName := ChangeFileExt(FGeometryFileName, '.shx');
        FDataBaseFileName := ChangeFileExt(FGeometryFileName, '.dbf');
        if not FileExists(FGeometryFileName) then
        begin
          Beep;
          result := False;
          MessageDlg(Format(StrTheShpFileS, [FGeometryFileName]),
             mtError, [mbOK], 0);
          Exit;
        end;
        if not FileExists(FDataBaseFileName) then
        begin
          Beep;
          if MessageDlg(Format(StrTheDbfFileS, [FDataBaseFileName]),

            mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
          begin
            result := False;
            Exit;
          end;
          FDataBaseFileName := '';
        end;
        FGeometryFile := TShapefileGeometryReader.Create;
        frmProgressMM.pbProgress.Max := 1000;
        frmProgressMM.pbProgress.Position := 0;
        frmProgressMM.Caption := StrReadingShapeGeomet;
        frmProgressMM.PopupParent := self;
        frmProgressMM.ProgressLabelCaption := StrReadingShape1;
        FShapeCount := 0;
        frmProgressMM.Show;
        try
          FGeometryFile.OnProgress := ShapefileProgress;
          try
            FGeometryFile.ReadFromFile(FGeometryFileName, FIndexFileName);
          except
            on EInvalidShapeFile do
            begin
              Beep;
              MessageDlg(Format(StrSIsAnInvalidSha,
                [FGeometryFileName]), mtError, [mbOK], 0);
              result := False;
              Exit;
            end;
            on E: EMismatchHeader do
            begin
              Beep;
              MessageDlg(E.message, mtError, [mbOK], 0);
              result := False;
              Exit;
            end;
          end;
        finally
          frmProgressMM.Hide;
        end;

        if FGeometryFile.NumberOfPoints > 10000 then
        begin
          comboVisibility.ItemIndex := 2;
        end;

        FShapeType := FGeometryFile.FileHeader.ShapeType;
        memoShapeFileInfo.Lines.Add(Format(StrNumberOfShapesD, [FGeometryFile.Count]));
        memoShapeFileInfo.Lines.Add(Format(StrMinimumXG, [FGeometryFile.FileHeader.BoundingBoxXMin]));
        memoShapeFileInfo.Lines.Add(Format(StrMaximumXG, [FGeometryFile.FileHeader.BoundingBoxXMax]));
        memoShapeFileInfo.Lines.Add(Format(StrMinimumYG, [FGeometryFile.FileHeader.BoundingBoxYMin]));
        memoShapeFileInfo.Lines.Add(Format(StrMaximumYG, [FGeometryFile.FileHeader.BoundingBoxYMax]));
        if FShapeType in [stPointZ, stPolyLineZ, stPolygonZ, stMultiPointZ] then
        begin
          memoShapeFileInfo.Lines.Add(Format(StrMinimumZG, [FGeometryFile.FileHeader.BoundingBoxZMin]));
          memoShapeFileInfo.Lines.Add(Format(StrMaximumZG, [FGeometryFile.FileHeader.BoundingBoxZMax]));
        end;

        cbImportZ.Enabled := FGeometryFile.FileHeader.ShapeType in
          [stPointZ, stPolyLineZ, stPolygonZ, stMultiPointZ];
        cbImportMeasured.Enabled := FGeometryFile.FileHeader.ShapeType in
          [stPointZ, stPolyLineZ, stPolygonZ, stMultiPointZ,
           stPointM, stPolyLineM, stPolygonM, stMultiPointM];
        if cbImportZ.Enabled then
        begin
          cbImportZ.Checked := True;
        end;
        if cbImportMeasured.Enabled then
        begin
          cbImportMeasured.Checked := True;
        end;

        lblNumShapes.Caption := Format(StrNumberOfShapes,
          [IntToStrFormatted(FGeometryFile.Count)]);

        cbImportObjectsClick(nil);

        if FDataBaseFileName <> '' then
        begin
          xbShapeDataBase.FileName := FDataBaseFileName;
          try
            xbShapeDataBase.Active := True;
          Except on E: EFOpenError do
            begin
              result := False;
              Beep;
              MessageDlg(E.message, mtError, [mbOK], 0);
              Exit;
            end;
          end;
          if xbShapeDataBase.RecordCount <> FGeometryFile.Count then
          begin
            result := False;
            Beep;
            MessageDlg('Error. There is a mismatch between the number of shapes in the shape geometry file and the number of records in the shapefile data base.', mtError, [mbOK], 0);
            Exit;
          end;
          Assert(xbShapeDataBase.RecordCount = FGeometryFile.Count);
          xbShapeDataBase.GotoBOF;
          ValidFields := TStringList.Create;
          ValidIndicies := TIntegerList.Create;
          try
            for Index := 1 to xbShapeDataBase.FieldCount do
            begin
              if xbShapeDataBase.GetFieldType(Index)
                in [xbfChar, xbfNumber, xbfLogic] then
              begin
                ValidIndicies.Add(Index);
                ValidFields.Add(string(xbShapeDataBase.GetFieldName(Index)))
              end;
            end;
            if (ValidFields.Count = 0) and (MessageDlg(Format(StrNoneOfTheFieldsI,
              [FDataBaseFileName]), mtConfirmation, [mbYes, mbNo], 0)
              <> mrYes) then
            begin
              result := False;
              Exit;
            end;

            FValidIndiciesCount := ValidIndicies.Count;
            SetLength(FFieldTypes, FValidIndiciesCount);
            for Index := 0 to ValidIndicies.Count - 1 do
            begin
              case xbShapeDataBase.GetFieldType(ValidIndicies[Index]) of
                xbfChar:
                  begin
                    FStringFieldNames.Add(ValidFields[Index]);
                    FFieldTypes[Index] := rdtString;
                  end;
                xbfNumber:
                  begin
                    if xbShapeDataBase.GetFieldDecimals(
                      ValidIndicies[Index]) = 0 then
                    begin
                      FFieldTypes[Index] := rdtInteger;
                      FIntegerFieldNames.Add(ValidFields[Index]);
                      FRealFieldNames.Add(ValidFields[Index]);
                    end
                    else
                    begin
                      FFieldTypes[Index] := rdtDouble;
                      FRealFieldNames.Add(ValidFields[Index]);
                    end;
                  end;
                xbfLogic:
                  begin
                    FFieldTypes[Index] := rdtBoolean;
                    FBooleanFieldNames.Add(ValidFields[Index]);
                  end;
              else
                Assert(False);
              end;
            end;

            if FStringFieldNames.Count > 0 then
            begin
              comboNameAttribute.Items.Assign(FStringFieldNames);
            end
            else
            begin
              comboObjectNameMethod.Enabled := False;
            end;

            FRealFieldAndGlobalVariablesNames.Assign(FRealFieldNames);
            for VarIndex := 0 to frmGoPhast.PhastModel.GlobalVariables.Count - 1 do
            begin
              Variable := frmGoPhast.PhastModel.GlobalVariables[VarIndex];
              if Variable.Format in [rdtDouble, rdtInteger] then
              begin
                FRealFieldAndGlobalVariablesNames.Add(Variable.Name)
              end;
            end;
            FRealFieldGlobalsAndDataSetsNames.Assign(
              FRealFieldAndGlobalVariablesNames);
            DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
            for DSIndex := 0 to DataArrayManager.DataSetCount - 1 do
            begin
              DataArray := DataArrayManager.DataSets[DSIndex];
              if not DataArray.Visible then
              begin
                Continue;
              end;

              if DataArray is TFootprintWithdrawalDataArray then
              begin
                Continue;
              end;

              if DataArray.DataType in [rdtDouble, rdtInteger] then
              begin
                FRealFieldGlobalsAndDataSetsNames.Add(DataArray.Name)
              end;
            end;
            FRealFieldGlobalsAndDataSetsNamesCount := FRealFieldGlobalsAndDataSetsNames.Count;

            WellDescription.Items := FStringFieldNames;
            AssignRealFieldNamesToControls;

            comboWellPumpAllocation.Items := FBooleanFieldNames;
            comboWellIntervalStyle.Items.AddStrings(FStringFieldNames);
            comboSolutionType.Items.AddStrings(FStringFieldNames);
            comboRiverDescripton.Items := FStringFieldNames;
            rdgBoundaryConditions.Columns[2].PickList := FIntegerFieldNames;

            FShapeFileValidFieldCount := ValidFields.Count;
            FShapeFileRealFieldCount := FRealFieldNames.Count;
            dgFields.RowCount := ValidFields.Count + 1;
            for Index := 0 to ValidFields.Count - 1 do
            begin
              dgFields.Cells[Ord(fgcAttributes), Index + 1] := ValidFields[Index];
              dgFields.Cells[Ord(fgcDataSet), Index + 1] := rsNewDataSet;
              dgFields.Cells[Ord(fgcInterpolator), Index + 1] := StrNone;
            end;
            FilesOK := True;
            FShouldEnableImportGrid := cbEnclosedCells.Enabled;
            if FShouldEnableImportGrid then
            begin
              FShouldEnableImportGrid := (ValidFields.IndexOf('X_INDEX') >= 0)
                and (ValidFields.IndexOf('Y_INDEX') >= 0);
              if FShouldEnableImportGrid then
              begin
                for ShapeIndex := 0 to FGeometryFile.Count - 1 do
                begin
                  Shape := FGeometryFile[ShapeIndex];
                  FShouldEnableImportGrid := (Shape.FNumPoints = 5)
                    and (Shape.FNumParts = 1);
                  if not FShouldEnableImportGrid then
                  begin
                    break;
                  end;
                end;
              end;
            end;
            FAllowShapesToCombine := True;
            for ShapeIndex := 0 to FGeometryFile.Count - 1 do
            begin
              Shape := FGeometryFile[ShapeIndex];
              if Shape.FNumParts > 1 then
              begin
                FAllowShapesToCombine := False;
                if memoMultipleParts.Lines.Count = 0 then
                begin
                  memoMultipleParts.Lines.Add(StrTheShapesCanNotB);
                end;
                memoMultipleParts.Lines.Add(IntToStr(ShapeIndex+1));
              end;
  //            FAllowShapesToCombine := Shape.FNumParts <= 1;
  //            if not FAllowShapesToCombine then
  //            begin
  //              lblSeparateObjectsExplanation.Caption := 'The shapes can not be '
  //                + 'combined because Shape ' + IntToStr(ShapeIndex) + ' has '
  //                + 'multiple parts.';
  //              break;
  //            end;

            end;
            memoMultipleParts.Visible := not FAllowShapesToCombine;
          finally
            ValidFields.Free;
            ValidIndicies.Free;
          end;
        end;
        cbImportGrid.Enabled := FShouldEnableImportGrid;
        if not FShouldEnableImportGrid then
        begin
          cbImportGrid.Checked := False;
        end;
        comboJoinObjects.Enabled := FAllowShapesToCombine;
        if not comboJoinObjects.Enabled then
        begin
          comboJoinObjects.ItemIndex := 0;
        end;
      end;
    finally
      if not FilesOK then
      begin
        Close;
      end;
    end;
  except on E: EFOpenError do
    begin
      Beep;
      MessageDlg(E.message, mtError, [mbOK], 0);
      result := False;
    end;

  end;
  EnableFeatureImport
end;

procedure TfrmImportShapefile.AssignFormulaInterpretationText(combo: TComboBox);
var
  Interp: TFormulaInterpretation;
begin
  Interp := GetFormulaInterpretation(combo);
  case Interp of
    fiSpecific:
      begin
        rdgBoundaryConditions.cells[FConductanceCol, 0] := Format(StrSPerUnitLength, [StrConductance]);
      end;
    fiDirect:
      begin
        rdgBoundaryConditions.cells[FConductanceCol, 0] := StrConductance;
      end;
    fiTotal:
      begin
        rdgBoundaryConditions.cells[FConductanceCol, 0] := Format(StrTotalSPerLayer, [StrConductance]);
      end;
  else
    Assert(False);
  end;
end;

procedure TfrmImportShapefile.AssignRealValueAttributesToControls;
begin
  // MNW2
  // Basic tab
  comboPartialPenetration.Items := FRealFieldGlobalsAndDataSetsNames;
  // Loss Controls tab
  comboWellRadius.Items := FRealFieldGlobalsAndDataSetsNames;
  comboSkinRadius.Items := FRealFieldGlobalsAndDataSetsNames;
  comboKSkin.Items := FRealFieldGlobalsAndDataSetsNames;
  comboBCoefficient.Items := FRealFieldGlobalsAndDataSetsNames;
  comboCCoefficient.Items := FRealFieldGlobalsAndDataSetsNames;
  comboPCoefficient.Items := FRealFieldGlobalsAndDataSetsNames;
  comboCellToWellConductance.Items := FRealFieldGlobalsAndDataSetsNames;

  // SFR
  comboSfrReachLength.Items := FRealFieldGlobalsAndDataSetsNames;
  comboSfrStreambedTop.Items := FRealFieldGlobalsAndDataSetsNames;
  comboSfrStreamSlope.Items := FRealFieldGlobalsAndDataSetsNames;
  comboSfrStreambedThickness.Items := FRealFieldGlobalsAndDataSetsNames;
  comboSfrStreambedKv.Items := FRealFieldGlobalsAndDataSetsNames;
  comboSaturatedVolumetricWater.Items := FRealFieldGlobalsAndDataSetsNames;
  comboInitialVolumetricWater.Items := FRealFieldGlobalsAndDataSetsNames;
  comboBrooksCoreyExponent.Items := FRealFieldGlobalsAndDataSetsNames;
  comboaxUnsaturatedKz.Items := FRealFieldGlobalsAndDataSetsNames;

  // SFR in MF6
  comboReachLengthMf6.Items := FRealFieldGlobalsAndDataSetsNames;
  comboRwid.Items := FRealFieldGlobalsAndDataSetsNames;
  comboGrd.Items := FRealFieldGlobalsAndDataSetsNames;
  comboRtp.Items := FRealFieldGlobalsAndDataSetsNames;
  comboRbth.Items := FRealFieldGlobalsAndDataSetsNames;
  comboRhk.Items := FRealFieldGlobalsAndDataSetsNames;


end;

procedure TfrmImportShapefile.AssignRealFieldNamesToControls;
begin
  // PHAST
  comboWellDiameter.Items := FRealFieldNames;
  comboWellLandSurfaceDatum.Items := FRealFieldNames;
  comboLeakyHydraulicConductivity.Items := FRealFieldNames;
  comboLeakyThickness.Items := FRealFieldNames;
  comboRiverHydraulicConductivity.Items := FRealFieldNames;
  comboRiverWidth.Items := FRealFieldNames;
  comboRiverDepth.Items := FRealFieldNames;
  comboRiverBedThickness.Items := FRealFieldNames;
  dgWellElevations.Columns[1].PickList := FRealFieldNames;
  dgWellElevations.Columns[2].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[0].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[1].PickList := FRealFieldNames;
  // WellFootprint withdrawal rate.
  comboFootprintWell.Items := FRealFieldNames;

  // MNW2
  // Basic tab
  comboZPump.Items := FRealFieldNames;
  comboMnw2PumplocX.Items := FRealFieldNames;
  comboMnw2PumplocY.Items := FRealFieldNames;
  comboMnw2PumplocZ.Items := FRealFieldNames;
  // Discharge Adjustment tab
  comboReferenceHead.Items := FRealFieldNames;
  comboLiftQ0.Items := FRealFieldNames;
  comboLiftQMax.Items := FRealFieldNames;
  comboWellTolerance.Items := FRealFieldNames;

  // LAK MF2005
  comboInitialStage.Items := FRealFieldNames;
  comboSill.Items := FRealFieldNames;

  // LAK MF6
  comboStartingStage.Items := FRealFieldNames;
  comboBottomElev.Items := FRealFieldNames;
  comboTopElev.Items := FRealFieldNames;
  comboLakebedK.Items := FRealFieldNames;
  comboLakebedThickness.Items := FRealFieldNames;
  comboConnLength.Items := FRealFieldNames;

  // DRT
  rdeDrtX.Items := FRealFieldNames;
  rdeDrtY.Items := FRealFieldNames;
  rdeDrtZ.Items := FRealFieldNames;

end;

procedure TfrmImportShapefile.InitializeColumnsForMt3dConc(
  StartingConcIndex: Integer);
var
  AComp: TMobileChemSpeciesItem;
  ConcIndex: Integer;
begin
  if not frmGoPhast.PhastModel.Mt3dmsSsmIsSelected then
  begin
    Exit;
  end;
  rdgBoundaryConditions.BeginUpdate;
  try
    for ConcIndex := 0 to frmGoPhast.PhastModel.MobileComponents.Count - 1 do
    begin
      rdgBoundaryConditions.Columns[StartingConcIndex + ConcIndex].ComboUsed
        := True;
      rdgBoundaryConditions.Columns[StartingConcIndex + ConcIndex].Format
        := rcf4String;
      rdgBoundaryConditions.Columns[StartingConcIndex + ConcIndex].PickList
        := FRealFieldGlobalsAndDataSetsNames;
      rdgBoundaryConditions.Columns[StartingConcIndex + ConcIndex].
        WordWrapCaptions := True;
      AComp := frmGoPhast.PhastModel.MobileComponents[ConcIndex];
      rdgBoundaryConditions.Cells[StartingConcIndex + ConcIndex, 0]
        := Format(StrConcentrationS, [AComp.Name]);
    end;
  finally
    rdgBoundaryConditions.EndUpdate;
  end;
end;

procedure TfrmImportShapefile.ImportConcItemForSeparateShapes(
  ItemIndex: Integer; ConcItem: TMt3dmsConcItem; StartingConcIndex: Integer);
var
  ConcSpecItem: TStringConcValueItem;
  AComp: TMobileChemSpeciesItem;
  ConcIndex: Integer;
  AFormula: string;
begin
  if not frmGoPhast.PhastModel.Mt3dmsSsmIsSelected then
  begin
    Exit;
  end;
  for ConcIndex := 0 to frmGoPhast.PhastModel.MobileComponents.Count - 1 do
  begin
    AComp := frmGoPhast.PhastModel.MobileComponents[ConcIndex];
    ConcSpecItem := GetConcSpeciesItem(AComp, ConcIndex, ConcItem);
    if (rdgBoundaryConditions.Cells[StartingConcIndex + ConcIndex,
      ItemIndex + 1] <> '') then
    begin
      AFormula := GetRealFormulaFromText(rdgBoundaryConditions.
        Cells[StartingConcIndex + ConcIndex, ItemIndex + 1]);
      ConcSpecItem.Value := AFormula;
    end
    else
    begin
      ConcSpecItem.Value := '0.';
    end;
  end;
end;

function TfrmImportShapefile.GetConcSpeciesItem(AComp: TMobileChemSpeciesItem;
  ConcIndex: Integer; ConcItem: TMt3dmsConcItem): TStringConcValueItem;
begin
  Assert(ConcItem <> nil);
  if ConcIndex < ConcItem.StringConcCollection.Count then
  begin
    result := ConcItem.StringConcCollection[ConcIndex];
  end
  else
  begin
    result := ConcItem.StringConcCollection.Add;
    result.Name := AComp.Name;
  end;
end;

procedure TfrmImportShapefile.ImportConcItemForCombinedShapes(
  ConcItem: TMt3dmsConcItem; StartingConcIndex: Integer;  ItemIndex: Integer;
  AScreenObject: TScreenObject);
var
  ConcSpecItem: TStringConcValueItem;
  AComp: TMobileChemSpeciesItem;
  ConcIndex: Integer;
  ItemName: string;
  AValue: Extended;
  ValueItem: TValueArrayItem;
  Dummy: Boolean;
begin
  for ConcIndex := 0 to frmGoPhast.PhastModel.MobileComponents.Count - 1 do
  begin
    AComp := frmGoPhast.PhastModel.MobileComponents[ConcIndex];
    Assert(ConcItem <> nil);
    ConcSpecItem := GetConcSpeciesItem(AComp, ConcIndex, ConcItem);
    AValue := GetRealValueFromText(rdgBoundaryConditions.
      Cells[StartingConcIndex + ConcIndex, ItemIndex + 1], Dummy);
    ItemName := Format(StrConcentrationsd, [AComp.Name, ItemIndex]);
    ValueItem := AScreenObject.ImportedValues.ValueItemByName(ItemName);
    if ValueItem = nil then
    begin
      ValueItem := AScreenObject.ImportedValues.Add as TValueArrayItem;
      ValueItem.Name := ItemName;
      ValueItem.Values.DataType := rdtDouble;
      ValueItem.Values.Count := 0;
      ConcSpecItem.Value := rsObjectImportedValuesR + '("' + ItemName + '")';
    end;
    ValueItem.Values.Add(AValue);
  end;
end;

function TfrmImportShapefile.CreateConcBoundary(
  AScreenObject: TScreenObject): TMt3dmsConcBoundary;
begin
  if frmGoPhast.PhastModel.Mt3dmsSsmIsSelected then
  begin
    AScreenObject.CreateMt3dmsConcBoundary;
    result := AScreenObject.Mt3dmsConcBoundary;
  end
  else
  begin
    result := nil;
  end;
end;

function TfrmImportShapefile.CreateConcItem(ConcBoundary: TMt3dmsConcBoundary;
  ItemIndex: Integer; Item: TCustomModflowBoundaryItem): TMt3dmsConcItem;
begin
  if ConcBoundary <> nil then
  begin
    if ItemIndex < ConcBoundary.Values.Count then
    begin
      result := ConcBoundary.Values[ItemIndex] as TMt3dmsConcItem;
    end
    else
    begin
      result := ConcBoundary.Values.Add as TMt3dmsConcItem;
    end;
    result.StartTime := Item.StartTime;
    result.EndTime := Item.EndTime;
  end
  else
  begin
    result := nil;
  end;
end;

procedure TfrmImportShapefile.Fill_comboInterpolaters;
var
  List: TList;
  AType: TInterpolatorType;
  Index: integer;
begin
  comboInterpolaters.Items.Add(StrNone);
  Assert(SizeOf(TObject) = SizeOf(TInterpolatorType));
  List := TList.Create;
  try
    AddInterpolatorsToList(List);
    for Index := 0 to List.Count - 1 do
    begin
      AType := List[Index];
      comboInterpolaters.Items.Add(AType.InterpolatorName);
    end;
  finally
    List.Free;
  end;
  comboInterpolaters.ItemIndex := 0;
end;

function TfrmImportShapefile.GetFieldNumberFromName(
  CellText: AnsiString): Integer;
begin
  if CellText = '' then
  begin
    result := 0;
  end
  else
  begin
    result := xbShapeDataBase.GetFieldNumberFromName(CellText);
  end;
end;

procedure TfrmImportShapefile.EnableJoinObjects;
begin
  comboJoinObjects.Enabled := FAllowShapesToCombine
    and cbImportObjects.Checked
    and FCombinedObjectsAllowed;
  if not comboJoinObjects.Enabled then
  begin
    comboJoinObjects.ItemIndex := 0
  end;
end;

procedure TfrmImportShapefile.AddParameterNamesToPickList(
  ParameterType: TParameterType; ParameterColumn: Integer);
var
  ParamIndex: Integer;
  Param: TModflowTransientListParameter;
begin
  for ParamIndex := 0 to frmGoPhast.PhastModel.
    ModflowTransientParameters.Count - 1 do
  begin
    Param := frmGoPhast.PhastModel.ModflowTransientParameters[ParamIndex];
    if Param.ParameterType = ParameterType then
    begin
      rdgBoundaryConditions.Columns[ParameterColumn].
        PickList.Add(Param.ParameterName);
    end;
  end;
end;

procedure TfrmImportShapefile.GetNewOrExistingBoundaryItem(
  var AnItem: TCustomModflowBoundaryItem; const ParameterName: string;
  var Param: TModflowTransientListParameter;
  var ParamItem: TModflowParamItem; Boundary: TModflowParamBoundary;
  ItemIndex: integer);

begin
  if Param = nil then
  begin
    if ItemIndex < Boundary.Values.Count then
    begin
      AnItem := Boundary.Values[ItemIndex] as TCustomModflowBoundaryItem;
    end
    else
    begin
      AnItem := Boundary.Values.Add
        as TCustomModflowBoundaryItem;
    end;
  end
  else
  begin
    if ParamItem = nil then
    begin
      ParamItem := Boundary.Parameters.Add
        as TModflowParamItem;
      ParamItem.Param.ParamName := ParameterName;
    end;
    if ItemIndex < ParamItem.Param.Count then
    begin
      AnItem := ParamItem.Param[ItemIndex] as TCustomModflowBoundaryItem;
    end
    else
    begin
      AnItem := ParamItem.Param.Add as TCustomModflowBoundaryItem;
    end;
  end;
end;

procedure TfrmImportShapefile.GetTransientParameter(
  var Param: TModflowTransientListParameter; var ParameterName: string;
  ParameterColumn: Integer; Row: Integer);
begin
  ParameterName := GetStringValueFromText(AnsiString(
    rdgBoundaryConditions.Cells[ParameterColumn, Row]));
  ParameterName := StringReplace(ParameterName, '"', '',
    [rfReplaceAll, rfIgnoreCase]);
  if ParameterName = '' then
  begin
    Param := nil;
  end
  else
  begin
    Param := frmGoPhast.PhastModel.
      ModflowTransientParameters.GetParamByName(ParameterName);
    if Param = nil then
    begin
      FInvalidParameterNames.Add(ParameterName);
    end;
  end;
end;

procedure TfrmImportShapefile.AddModflowPackageToImportChoices(
  APackage: TModflowPackageSelection);
begin
  if frmGoPhast.PhastModel.PackageIsSelected(APackage) then
  begin
//    if APackage is TMawPackage then
//    begin
//
//    end;
//    if (APackage is TSfrPackageSelection)
//      or (APackage is TMultinodeWellSelection)
//      or (APackage is TLakePackageSelection)
//      or (APackage is THobPackageSelection)
//      then
//    begin
//      comboJoinObjects.ItemIndex := 0;
//    end;
    comboBoundaryChoice.Items.AddObject(APackage.PackageIdentifier, APackage);
  end;
end;

procedure TfrmImportShapefile.CreateDataSetVariables(Parser: TRbwParser;
  EvalAt: TEvaluatedAt);
var
  DataArray: TDataArray;
  Index: Integer;
  DataArrayManager: TDataArrayManager;
  AVariable: TCustomVariable;
begin
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  for Index := 0 to DataArrayManager.DataSetCount - 1 do
  begin
    DataArray := DataArrayManager.DataSets[Index];
    if DataArray.EvaluatedAt <> EvalAt then
    begin
      Continue;
    end;
    AVariable := nil;
    if (DataArray.Orientation = dsoTop)
      and (Parser.IndexOfVariable(DataArray.Name) < 0) then
    begin
      case DataArray.DataType of
        rdtDouble:
          AVariable := Parser.CreateVariable(DataArray.Name, StrDataSets, 0, TRealVariable, DataArray.Name);
        rdtInteger:
          AVariable := Parser.CreateVariable(DataArray.Name, StrDataSets, 0, TIntegerVariable, DataArray.Name);
        rdtBoolean:
          AVariable := Parser.CreateVariable(DataArray.Name, StrDataSets, False, TBooleanVariable, DataArray.Name);
        rdtString:
          AVariable := Parser.CreateVariable(DataArray.Name, StrDataSets, '', TStringVariable, DataArray.Name);
        else
          Assert(False);
      end;
      AVariable.Classification := DataArray.FullClassification;
    end;
  end;
end;

procedure TfrmImportShapefile.ChangeInterpolators(NewProperties,
  OldProperties: TList);
var
  DataSet: TDataArray;
  DataSetIndex: Integer;
  Index: Integer;
  NewProp, OldProp: TPhastDataSetStorage;
begin
  for Index := 1 to dgFields.RowCount - 1 do
  begin
    if dgFields.Checked[Ord(fgcImport), Index]
      and (dgFields.Cells[Ord(fgcDataSet), Index] <> rsNewDataSet) then
    begin
      GetInterpolators(Index);
      DataSetIndex := dgFields.Columns[Ord(fgcDataSet)].PickList.
        IndexOf(dgFields.Cells[Ord(fgcDataSet), Index]);
      if DataSetIndex >= 0 then
      begin
        DataSet := dgFields.Columns[Ord(fgcDataSet)].PickList.
          Objects[DataSetIndex] as TDataArray;
        AssignInterpolator(DataSet, Index, NewProp, OldProp);
        if NewProp = nil then
        begin
          Assert(OldProp = nil);
        end
        else
        begin
          Assert(OldProp <> nil);
          NewProperties.Add(NewProp);
          OldProperties.Add(OldProp);
        end;
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.AssignInterpolator(DataSet: TDataArray;
  Index: Integer; out NewProperties, OldProperties: TPhastDataSetStorage);
var
  Interpolator: TCustom2DInterpolater;
  AType: TInterpolatorType;
  InterpolatorIndex: Integer;
begin
  NewProperties := nil;
  OldProperties := nil;
  if dgFields.Cells[Ord(fgcInterpolator), Index] <> '' then
  begin
    InterpolatorIndex := dgFields.ItemIndex[Ord(fgcInterpolator), Index];
    if InterpolatorIndex < 0 then
    begin
      if DataSet.TwoDInterpolator <> nil then
      begin
        OldProperties := TPhastDataSetStorage.Create;
        OldProperties.Assign(DataSet);
      end;
      DataSet.TwoDInterpolator := nil;
      if OldProperties <> nil then
      begin
        NewProperties := TPhastDataSetStorage.Create;
        NewProperties.Assign(DataSet);
      end;
    end
    else
    begin
      AType := TInterpolatorType(dgFields.Columns[Ord(fgcInterpolator)].
        PickList.Objects[InterpolatorIndex]);
      if AType = nil then
      begin
        if DataSet.TwoDInterpolator <> nil then
        begin
          OldProperties := TPhastDataSetStorage.Create;
          OldProperties.Assign(DataSet);
        end;
        DataSet.TwoDInterpolator := nil;
        if OldProperties <> nil then
        begin
          NewProperties := TPhastDataSetStorage.Create;
          NewProperties.Assign(DataSet);
        end;
      end
      else if (DataSet.TwoDInterpolator = nil)
        or not (DataSet.TwoDInterpolator is AType) then
      begin
        Interpolator := AType.Create(nil);
        try
          OldProperties := TPhastDataSetStorage.Create;
          OldProperties.Assign(DataSet);
          DataSet.TwoDInterpolator := Interpolator;
          NewProperties := TPhastDataSetStorage.Create;
          NewProperties.Assign(DataSet);
        finally
          Interpolator.Free;
        end;
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.EnableEvalAt;
begin
  rgEvaluatedAt.Enabled := cbImportObjects.Checked
    and (frmGoPhast.PhastModel.ModelSelection in
    [msPhast, msSutra22, msSutra30]);
end;

procedure TfrmImportShapefile.AssignColFeatureProperties;
var
  Index: Integer;
begin
  rdgBoundaryConditions.BeginUpdate;
  try
    for Index := 0 to rdgBoundaryConditions.ColCount - 1 do
    begin
      rdgBoundaryConditions.Columns[Index].AutoAdjustColWidths := True;
      rdgBoundaryConditions.Columns[Index].AutoAdjustRowHeights := True;
      rdgBoundaryConditions.Columns[Index].WordWrapCaptions := True;
    end;
  finally
    rdgBoundaryConditions.EndUpdate;
  end;
end;

function TfrmImportShapefile.GetFormulaInterpretation(
  combo: TComboBox): TFormulaInterpretation;
begin
  if combo.ItemIndex in [Ord(Low(TFormulaInterpretation))..
    Ord(High(TFormulaInterpretation))] then
  begin
    result := TFormulaInterpretation(combo.ItemIndex);
  end
  else
  begin
    Text := UpperCase(GetStringValueFromText(combo.Text));
    if Text = StrSPECIFIC then
    begin
      result := fiSpecific;
    end
    else if Text = StrDIRECT then
    begin
      result := fiDirect;
    end
    else if Text = StrTOTAL then
    begin
      result := fiTotal;
    end
    else
    begin
      result := fiDirect;
    end;
  end;
end;

procedure TfrmImportShapefile.ImportFootprintWell(
  AScreenObject: TScreenObject);
var
  Footprint: TFootprintWell;
  AValue: Extended;
  ItemName: string;
  ValueItem: TValueArrayItem;
  DataArray: TDataArray;
  DataArrayPosition: Integer;
  Dummy: Boolean;
begin
  AScreenObject.CreateFootprintWell;
  Footprint := AScreenObject.FootprintWell;
  Footprint.IsUsed := True;

  if FCombinedObjects then
  begin
    AValue := GetRealValueFromText(comboFootprintWell.Text, Dummy);
    ItemName := StrFootprintWithdrawal;
    ValueItem := AScreenObject.ImportedValues.ValueItemByName(
      ItemName);
    if ValueItem = nil then
    begin
      ValueItem := AScreenObject.
        ImportedValues.Add as TValueArrayItem;
      ValueItem.Name := ItemName;
      ValueItem.Values.DataType := rdtDouble;
      ValueItem.Values.Count := 0;
      Footprint.Withdrawal := rsObjectImportedValuesR
        + '("' + ItemName + '")';
    end;
    ValueItem.Values.Add(AValue);
  end
  else
  begin
    Footprint.Withdrawal :=
      GetRealFormulaFromText(comboFootprintWell.Text);
    DataArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(KWithdrawals);
    DataArrayPosition := AScreenObject.AddDataSet(DataArray);
    AScreenObject.DataSetFormulas[DataArrayPosition] := Footprint.Withdrawal;
  end;
end;

procedure TfrmImportShapefile.ImportModflowMawPackage(
  AScreenObject: TScreenObject);
var
  Boundary: TMawBoundary;
  CondMethodIndex: Integer;
  StringValue: string;
  WellScreenIndex: Integer;
  ScreenTop: string;
  ScreenBottom: string;
  WellScreen: TMawWellScreenItem;
  Index: Integer;
  MawItem: TMawItem;
  StartTime: Extended;
  EndTime: Extended;
  StartTimeBad: Boolean;
  EndTimeBad: Boolean;
  PickList: TStringList;
  ItemIndex: Integer;
begin
  AScreenObject.CreateMawBoundary;
  Boundary := AScreenObject.ModflowMawBoundary;

  Boundary.Radius := GetRealFormulaFromText(comboMawRadius.Text, False, True);
  Boundary.Bottom := GetRealFormulaFromText(comboMawBottom.Text, False, True);
  Boundary.InitialHead := GetRealFormulaFromText(comboMawInitialHead.Text, False, True);
  CondMethodIndex := comboMawConductanceEquation.ItemIndex;
  if CondMethodIndex < 0 then
  begin
    Boundary.ConductanceMethod := mcmSpecified;
  end
  else if (CondMethodIndex <= Ord(High(TMawConductanceMethod))) then
  begin
    Boundary.ConductanceMethod := TMawConductanceMethod(CondMethodIndex);
  end
  else
  begin
    StringValue := GetStringValueFromText(comboMawConductanceEquation.Text);
    CondMethodIndex := comboMawInitialHead.Items.IndexOf(StringValue);
    if CondMethodIndex < 0 then
    begin
      Boundary.ConductanceMethod := mcmSpecified;
    end
    else if (CondMethodIndex <= Ord(High(TMawConductanceMethod))) then
    begin
      Boundary.ConductanceMethod := TMawConductanceMethod(CondMethodIndex);
    end
    else
    begin
      Boundary.ConductanceMethod := mcmSpecified;
    end;
  end;

//  TMawWellScreenColumns = (mwscScreenTop, mwscScreenBottom, mwscSkinK, mwscSkinRadius);
  for WellScreenIndex := 0 to frameMawWellScreens.seNumber.AsInteger - 1 do
  begin
    ScreenTop := GetRealFormulaFromText(frameMawWellScreens.Grid.Cells[
      Ord(mwscScreenTop), WellScreenIndex+1], False, True);
    ScreenBottom := GetRealFormulaFromText(frameMawWellScreens.Grid.Cells[
      Ord(mwscScreenBottom), WellScreenIndex+1], False, True);
    if ScreenTop <> ScreenBottom then
    begin
      WellScreen := Boundary.WellScreens.Add as TMawWellScreenItem;
      WellScreen.ScreenTop := ScreenTop;
      WellScreen.SkinK := GetRealFormulaFromText(frameMawWellScreens.Grid.Cells[
        Ord(mwscSkinK), WellScreenIndex+1], False, True);
      WellScreen.SkinRadius := GetRealFormulaFromText(frameMawWellScreens.Grid.Cells[
        Ord(mwscSkinRadius), WellScreenIndex+1], False, True);
    end;
  end;


  PickList := TStringList.Create;
  try
    for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
    begin
      StartTime := GetRealValueFromText(
        rdgBoundaryConditions.Cells[Ord(mcStartTime), Index+1], StartTimeBad);
      EndTime := GetRealValueFromText(
        rdgBoundaryConditions.Cells[Ord(mcEndTime), Index+1], EndTimeBad);
      if (not StartTimeBad) and (not EndTimeBad) then
      begin
        MawItem := Boundary.Values.Add as TMawItem;
        MawItem.StartTime := StartTime;
        MawItem.EndTime := EndTime;

        PickList.Assign(rdgBoundaryConditions.Columns[Ord(mcStatus)].PickList);
        PickList.CaseSensitive := False;
        StringValue := rdgBoundaryConditions.Cells[Ord(mcStatus),Index+1];
        ItemIndex := PickList.IndexOf(StringValue);
        if ItemIndex > Ord(High(TMawStatus)) then
        begin
          StringValue := GetStringValueFromText(StringValue);
          ItemIndex := PickList.IndexOf(StringValue);
        end;
        if ItemIndex < 0 then
        begin
          MawItem.MawStatus := Low(TMawStatus);
        end
        else
        begin
          MawItem.MawStatus := TMawStatus(ItemIndex);
        end;

        MawItem.Rate := GetRealFormulaFromText(
          rdgBoundaryConditions.Cells[Ord(mcRate),Index+1], False, True);
        MawItem.WellHead := GetRealFormulaFromText(
          rdgBoundaryConditions.Cells[Ord(mcWellHead),Index+1], False, True);

        PickList.Assign(rdgBoundaryConditions.Columns[Ord(mcMawFlowingWell)].PickList);
        PickList.CaseSensitive := False;
        StringValue := rdgBoundaryConditions.Cells[Ord(mcMawFlowingWell),Index+1];
        ItemIndex := PickList.IndexOf(StringValue);
        if ItemIndex > Ord(High(TFlowingWell)) then
        begin
          StringValue := GetStringValueFromText(StringValue);
          ItemIndex := PickList.IndexOf(StringValue);
        end;
        if ItemIndex < 0 then
        begin
          MawItem.FlowingWell := Low(TFlowingWell);
        end
        else
        begin
          MawItem.FlowingWell := TFlowingWell(ItemIndex);
        end;

        MawItem.FlowingWellElevation := GetRealFormulaFromText(
          rdgBoundaryConditions.Cells[Ord(mcMawFlowingWellElevation),Index+1], False, True);
        MawItem.FlowingWellConductance := GetRealFormulaFromText(
          rdgBoundaryConditions.Cells[Ord(mcFlowingWellConductance),Index+1], False, True);
        MawItem.FlowingWellReductionLength := GetRealFormulaFromText(
          rdgBoundaryConditions.Cells[Ord(mcFlowingWellReductionLength),Index+1], False, True);

        PickList.Assign(rdgBoundaryConditions.Columns[Ord(mcRateLimitation)].PickList);
        PickList.CaseSensitive := False;
        StringValue := rdgBoundaryConditions.Cells[Ord(mcRateLimitation),Index+1];
        ItemIndex := PickList.IndexOf(StringValue);
        if ItemIndex > Ord(High(TRateLimitation)) then
        begin
          StringValue := GetStringValueFromText(StringValue);
          ItemIndex := PickList.IndexOf(StringValue);
        end;
        if ItemIndex < 0 then
        begin
          MawItem.RateLimitation := Low(TRateLimitation);
        end
        else
        begin
          MawItem.RateLimitation := TRateLimitation(ItemIndex);
        end;

        MawItem.MinRate := GetRealFormulaFromText(
          rdgBoundaryConditions.Cells[Ord(mcMinRate),Index+1], False, True);
        MawItem.MaxRate := GetRealFormulaFromText(
          rdgBoundaryConditions.Cells[Ord(mcMaxRate),Index+1], False, True);
        MawItem.PumpElevation := GetRealFormulaFromText(
          rdgBoundaryConditions.Cells[Ord(mcPumpElevation),Index+1], False, True);
        MawItem.ScalingLength := GetRealFormulaFromText(
          rdgBoundaryConditions.Cells[Ord(mcScalingLength),Index+1], False, True);

        PickList.Assign(rdgBoundaryConditions.Columns[Ord(mcHeadLimitChoice)].PickList);
        PickList.CaseSensitive := False;
        StringValue := rdgBoundaryConditions.Cells[Ord(mcHeadLimitChoice),Index+1];
        ItemIndex := PickList.IndexOf(StringValue);
        if ItemIndex > Ord(High(Boolean)) then
        begin
          StringValue := GetStringValueFromText(StringValue);
          ItemIndex := PickList.IndexOf(StringValue);
        end;
        if ItemIndex < 0 then
        begin
          MawItem.HeadLimitChoice := Low(Boolean);
        end
        else
        begin
          MawItem.HeadLimitChoice := Boolean(ItemIndex);
        end;

        MawItem.HeadLimit := GetRealFormulaFromText(
          rdgBoundaryConditions.Cells[Ord(mcHeadLimit),Index+1], False, True);
      end;
    end;
  finally
    PickList.Free;
  end;
//  TMawColumns = (mcStartTime, mcEndTime, mcStatus, mcRate, mcWellHead,
//    mcMawFlowingWell,
//    mcMawFlowingWellElevation, mcFlowingWellConductance, mcRateLimitation,
//    mcMinRate, mcMaxRate, mcPumpElevation, mcScalingLength, mcHeadLimitChoice,
//    mcHeadLimit);
end;

procedure TfrmImportShapefile.ImportModflowMnw2Package(
  AScreenObject: TScreenObject);
var
  Boundary: TMnw2Boundary;
  Index: Integer;
  UseRow: Boolean;
  StartTime: double;
  EndTime: double;
  TimeItem: TMnw2TimeItem;
  LossTypeString: string;
  SpatialItem: TMnw2SpatialItem;
  Row: Integer;
  RowIndex: Integer;
  TextToUse: string;
  QCUT: Integer;
  Dummy: Boolean;
begin
  AScreenObject.CreateMnw2Boundary;
  Boundary := AScreenObject.ModflowMnw2Boundary;
  SpatialItem := Boundary.Values.Add as TMnw2SpatialItem;

  // Basic tab
  Boundary.WellID := GetStringValueFromText(comboMnw2WellId.Text);

  LossTypeString := GetStringValueFromText(comboMnw2LossType.Text);
  LossTypeString := UpperCase(Trim(LossTypeString));
  if LossTypeString = StrNONE_UC then
  begin
    Boundary.LossType := mltNone;
  end
  else if LossTypeString = StrTHIEM then
  begin
    Boundary.LossType := mltThiem;
  end
  else if LossTypeString = StrSKIN then
  begin
    Boundary.LossType := mltSkin;
  end
  else if LossTypeString = StrGENERAL then
  begin
    Boundary.LossType := mltEquation;
  end
  else if LossTypeString = StrSPECIFYCWC then
  begin
    Boundary.LossType := mtlSpecify;
  end;

  Boundary.SpecifyPump := GetIntegerValueFromText(comboSpecifyPump.Text) <> 0;

  if Boundary.SpecifyPump then
  begin
    if FNumPointsInCurrentShape = 1 then
    begin
      Boundary.PumpElevation := GetRealValueFromText(comboZPump.Text, Dummy);
    end
    else
    begin
      Boundary.PumpCellTarget.TargetType := ttLocation;
      Boundary.PumpCellTarget.TargetLocation.X :=
        GetRealValueFromText(comboMnw2PumplocX.Text, Dummy);
      Boundary.PumpCellTarget.TargetLocation.Y :=
        GetRealValueFromText(comboMnw2PumplocY.Text, Dummy);
      Boundary.PumpCellTarget.TargetLocation.Z :=
        GetRealValueFromText(comboMnw2PumplocZ.Text, Dummy);
    end;
  end;

  Boundary.ConstrainPumping :=
    GetIntegerValueFromText(comboConstrainPumping.Text) <> 0;
  Boundary.PartialPenetrationCorrection :=
    GetIntegerValueFromText(comboPartialPenetrationFlag.Text) <> 0;

  SpatialItem.PartialPenetration :=
    GetRealFormulaFromText(comboPartialPenetration.Text);

  Boundary.AdjustPumping :=
    GetIntegerValueFromText(comboPumpCap.Text) <> 0;

  // Loss Controls tab
  case Boundary.LossType of
    mltNone: ; // do nothing
    mltThiem:
      begin
        SpatialItem.WellRadius :=
          GetRealFormulaFromText(comboWellRadius.Text);
      end;
    mltSkin:
      begin
        SpatialItem.WellRadius :=
          GetRealFormulaFromText(comboWellRadius.Text);
        SpatialItem.SkinRadius :=
          GetRealFormulaFromText(comboSkinRadius.Text);
        SpatialItem.SkinK :=
          GetRealFormulaFromText(comboKSkin.Text);
      end;
    mltEquation:
      begin
        SpatialItem.WellRadius :=
          GetRealFormulaFromText(comboWellRadius.Text);
        SpatialItem.B :=
          GetRealFormulaFromText(comboBCoefficient.Text);
        SpatialItem.C :=
          GetRealFormulaFromText(comboCCoefficient.Text);
        SpatialItem.P :=
          GetRealFormulaFromText(comboPCoefficient.Text);
      end;
    mtlSpecify: 
      begin
        SpatialItem.CellToWellConductance :=
          GetRealFormulaFromText(comboCellToWellConductance.Text);
      end;
    else Assert(false);
  end;

  // Discharge Adjustment tab
  if Boundary.AdjustPumping then
  begin
    Boundary.ReferenceHead := GetRealValueFromText(comboReferenceHead.Text, Dummy);
    Boundary.MaximumLift := GetRealValueFromText(comboLiftQ0.Text, Dummy);
    Boundary.LiftAtMaxRate := GetRealValueFromText(comboLiftQMax.Text, Dummy);
    Boundary.WellTolerance := GetRealValueFromText(comboWellTolerance.Text, Dummy);
  end;

  // Time data
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    Row := Index + 1;
    UseRow := (rdgBoundaryConditions.Cells[Ord(mtcStartTime), Row] <> '')
      and (rdgBoundaryConditions.Cells[Ord(mtcEndTime), Row] <> '')
      and (rdgBoundaryConditions.Cells[Ord(mtcPumpingRate), Row] <> '');
    if UseRow then
    begin
      StartTime := GetRealValueFromText(
        rdgBoundaryConditions.Cells[Ord(mtcStartTime), Row], Dummy);
      EndTime := GetRealValueFromText(
        rdgBoundaryConditions.Cells[Ord(mtcEndTime), Row], Dummy);

      TimeItem := Boundary.TimeValues.Add as TMnw2TimeItem;
      TimeItem.StartTime := StartTime;
      TimeItem.EndTime := EndTime;
      TimeItem.PumpingRate := GetRealFormulaFromText(
        rdgBoundaryConditions.Cells[Ord(mtcPumpingRate), Row]);

      if Boundary.AdjustPumping then
      begin
        TextToUse := '';
        for RowIndex := Row downto 1 do
        begin
          if rdgBoundaryConditions.Cells[
            Ord(mtcMultiplier), RowIndex] <> '' then
          begin
            TextToUse := rdgBoundaryConditions.
              Cells[Ord(mtcMultiplier), RowIndex];
            break;
          end;
        end;
        if TextToUse <> '' then
        begin
          TimeItem.HeadCapacityMultiplier := GetRealFormulaFromText(TextToUse);
        end;
      end;

      if Boundary.ConstrainPumping then
      begin
        TextToUse := '';
        for RowIndex := Row downto 1 do
        begin
          if rdgBoundaryConditions.Cells[
            Ord(mtcLimitingWaterLevel), RowIndex] <> '' then
          begin
            TextToUse := rdgBoundaryConditions.
              Cells[Ord(mtcLimitingWaterLevel), RowIndex];
            break;
          end;
        end;
        if TextToUse <> '' then
        begin
          TimeItem.LimitingWaterLevel := GetRealFormulaFromText(TextToUse);
        end;

        TextToUse := '';
        for RowIndex := Row downto 1 do
        begin
          if rdgBoundaryConditions.Cells[
            Ord(mtcLimitMethod), RowIndex] <> '' then
          begin
            TextToUse := rdgBoundaryConditions.
              Cells[Ord(mtcLimitMethod), RowIndex];
            break;
          end;
        end;
        if TextToUse <> '' then
        begin
          QCUT := GetIntegerValueFromText(TextToUse);
          if QCUT = 0 then
          begin
            TimeItem.LimitMethod := mlmNoMinimum;
          end
          else if QCUT > 0 then
          begin
            TimeItem.LimitMethod := mlmRate;
          end
          else
          begin
            TimeItem.LimitMethod := mlmFraction;
          end;
        end;

        if TimeItem.LimitMethod <> mlmNoMinimum then
        begin
          TextToUse := '';
          for RowIndex := Row downto 1 do
          begin
            if rdgBoundaryConditions.Cells[
              Ord(mtcMinRate), RowIndex] <> '' then
            begin
              TextToUse := rdgBoundaryConditions.
                Cells[Ord(mtcMinRate), RowIndex];
              break;
            end;
          end;
          if TextToUse <> '' then
          begin
            TimeItem.InactivationPumpingRate :=
              GetRealFormulaFromText(TextToUse);
          end;

          TextToUse := '';
          for RowIndex := Row downto 1 do
          begin
            if rdgBoundaryConditions.Cells[
              Ord(mtcMaxRate), RowIndex] <> '' then
            begin
              TextToUse := rdgBoundaryConditions.
                Cells[Ord(mtcMaxRate), RowIndex];
              break;
            end;
          end;
          if TextToUse <> '' then
          begin
            TimeItem.ReactivationPumpingRate :=
              GetRealFormulaFromText(TextToUse);
          end;
        end;
      end;
    end;
  end;

end;

procedure TfrmImportShapefile.ImportModflowUzfPackage(
  AScreenObject: TScreenObject);
var
  Boundary: TUzfBoundary;
  Index: Integer;
  UseRow: Boolean;
  StartTime: Extended;
  EndTime: Extended;
  Item: TRchItem;
  EvtItem: TEvtItem;
  ExtinctItem: TUzfExtinctDepthItem;
  WaterContentItem: TUzfWaterContentItem;
  Count: Integer;
  AValue: Extended;
  ItemName: string;
  ValueItem: TValueArrayItem;
  Dummy: Boolean;
  StartingConcIndex: Integer;
  UztRchBoundary: TMt3dUztRchConcBoundary;
  UztRchItem: TMt3dUztRchConcItem;
  UztSatEtBoundary: TMt3dUztSatEtConcBoundary;
  UztUnsatETBoundary: TMt3dUztUnsatEtConcBoundary;
  UztSatEtItem: TMt3dUztSatEtConcItem;
  UztUnsatItem: TMt3dUztUnsatEtConcItem;
  ColIndex: Integer;
  ConcIndex: Integer;
begin
  AScreenObject.CreateUzfBoundary;
  Boundary := AScreenObject.ModflowUzfBoundary;

  if frmGoPhast.PhastModel.Mt3d_UztIsSelected then
  begin
    AScreenObject.CreateMt3dUzfRchConcBoundary;
    UztRchBoundary := AScreenObject.Mt3dUzfRechConc;
  end
  else
  begin
    UztRchBoundary := nil;
  end;

  if frmGoPhast.PhastModel.Mt3d_UztEtIsSelected then
  begin
    AScreenObject.CreateMt3dUztSatEtConcBoundary;
    AScreenObject.CreateMt3dUztUnsatEtConcBoundary;
    UztSatEtBoundary := AScreenObject.Mt3dUztSatEtConcBoundary;
    UztUnsatETBoundary := AScreenObject.Mt3dUztUnsatEtConcBoundary;
  end
  else
  begin
    UztSatEtBoundary := nil;
    UztUnsatETBoundary := nil;
  end;

  Count := 0;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(ucStartTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(ucEndTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(ucInfiltration), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(ucEvapRate), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(ucExtinctDepth), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(ucEndTime), Index + 1] <> '');
    if UseRow and frmGoPhast.PhastModel.Mt3d_UztIsSelected then
    begin
      StartingConcIndex := Ord(ucExtinctWaterContent) +1;
      for ColIndex := StartingConcIndex to rdgBoundaryConditions.ColCount - 1 do
      begin
        UseRow := rdgBoundaryConditions.Cells[ColIndex, Index + 1] <> '';
        if not UseRow then
        begin
          break;
        end;
      end;
    end;
    if UseRow then
    begin
      StartTime := GetRealValueFromText(
        rdgBoundaryConditions.Cells[Ord(scStartTime), Index + 1], Dummy);
      EndTime := GetRealValueFromText(
        rdgBoundaryConditions.Cells[Ord(scEndTime), Index + 1], Dummy);

      if Count < Boundary.Values.Count then
      begin
        Item := Boundary.Values[Count] as TRchItem;
      end
      else
      begin
        Item := Boundary.Values.Add as TRchItem;
      end;
      Item.StartTime := StartTime;
      Item.EndTime := EndTime;

      if FCombinedObjects then
      begin
        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(ucInfiltration), Index + 1], Dummy);
        ItemName := Format(StrUZFRechargeRated, [Index]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          Item.RechargeRate := rsObjectImportedValuesR + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);
      end
      else
      begin
        Item.RechargeRate := GetRealFormulaFromText(
          rdgBoundaryConditions.Cells[Ord(ucInfiltration), Index + 1]);
      end;

      if Count < Boundary.EvapotranspirationDemand.Count then
      begin
        EvtItem := Boundary.EvapotranspirationDemand.Items[Count] as TEvtItem;
      end
      else
      begin
        EvtItem := Boundary.EvapotranspirationDemand.Add as TEvtItem;
      end;
      EvtItem.StartTime := StartTime;
      EvtItem.EndTime := EndTime;
      if FCombinedObjects then
      begin
        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(ucEvapRate), Index + 1], Dummy);
        ItemName := Format(StrUZFEvapotranspirati, [Index]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          EvtItem.EvapotranspirationRate := rsObjectImportedValuesR
            + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);
      end
      else
      begin
        EvtItem.EvapotranspirationRate := GetRealFormulaFromText(
          rdgBoundaryConditions.Cells[Ord(ucEvapRate), Index + 1]);
      end;

      if Count < Boundary.ExtinctionDepth.Count then
      begin
        ExtinctItem := Boundary.ExtinctionDepth.Items[Count]
          as TUzfExtinctDepthItem;
      end
      else
      begin
        ExtinctItem := Boundary.ExtinctionDepth.Add as TUzfExtinctDepthItem;
      end;
      ExtinctItem.StartTime := StartTime;
      ExtinctItem.EndTime := EndTime;
      if FCombinedObjects then
      begin
        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(ucExtinctDepth), Index + 1], Dummy);
        ItemName := Format(StrUZFExtinctionDepth, [Index]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          ExtinctItem.UzfExtinctDepth := rsObjectImportedValuesR
            + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);
      end
      else
      begin
        ExtinctItem.UzfExtinctDepth := GetRealFormulaFromText(
          rdgBoundaryConditions.Cells[Ord(ucExtinctDepth), Index + 1]);
      end;

      if Count < Boundary.WaterContent.Count then
      begin
        WaterContentItem := Boundary.WaterContent.Items[Count]
          as TUzfWaterContentItem;
      end
      else
      begin
        WaterContentItem := Boundary.WaterContent.Add as TUzfWaterContentItem;
      end;
      WaterContentItem.StartTime := StartTime;
      WaterContentItem.EndTime := EndTime;
      if FCombinedObjects then
      begin
        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(ucExtinctWaterContent), Index + 1], Dummy);
        ItemName := Format(StrUZFWaterContentd, [Index]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          WaterContentItem.UzfWaterContent := rsObjectImportedValuesR
            + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);
      end
      else
      begin
        WaterContentItem.UzfWaterContent := GetRealFormulaFromText(
          rdgBoundaryConditions.Cells[Ord(ucExtinctWaterContent), Index + 1]);
      end;

      StartingConcIndex := Ord(ucExtinctWaterContent) +1;
      if UztRchBoundary <> nil then
      begin
        if Count < UztRchBoundary.Values.Count then
        begin
          UztRchItem := UztRchBoundary.Values[Count] as TMt3dUztRchConcItem;
        end
        else
        begin
          UztRchItem := UztRchBoundary.Values.Add as TMt3dUztRchConcItem;
        end;
        UztRchItem.StartTime := StartTime;
        UztRchItem.EndTime := EndTime;

        for ConcIndex := 0 to frmGoPhast.PhastModel.MobileComponents.Count - 1 do
        begin
          if FCombinedObjects then
          begin
            AValue := GetRealValueFromText(rdgBoundaryConditions.
              Cells[StartingConcIndex + ConcIndex, Index + 1], Dummy);
            ItemName := Format(StrUZTRechConcd, [ConcIndex,Index]);
            ValueItem := AScreenObject.ImportedValues.ValueItemByName(
              ItemName);
            if ValueItem = nil then
            begin
              ValueItem := AScreenObject.
                ImportedValues.Add as TValueArrayItem;
              ValueItem.Name := ItemName;
              ValueItem.Values.DataType := rdtDouble;
              ValueItem.Values.Count := 0;
              UztRchItem.Mt3dmsConcRate[ConcIndex] := rsObjectImportedValuesR
                + '("' + ItemName + '")';
            end;
            ValueItem.Values.Add(AValue);
          end
          else
          begin
            UztRchItem.Mt3dmsConcRate[ConcIndex] := GetRealFormulaFromText(
              rdgBoundaryConditions.Cells[StartingConcIndex + ConcIndex, Index + 1]);
          end;
        end;
      end;

      if UztSatEtBoundary <> nil then
      begin
        if Count < UztSatEtBoundary.Values.Count then
        begin
          UztSatEtItem := UztSatEtBoundary.Values[Count] as TMt3dUztSatEtConcItem;
        end
        else
        begin
          UztSatEtItem := UztSatEtBoundary.Values.Add as TMt3dUztSatEtConcItem;
        end;
        UztSatEtItem.StartTime := StartTime;
        UztSatEtItem.EndTime := EndTime;

        Inc(StartingConcIndex, frmGoPhast.PhastModel.MobileComponents.Count);
        for ConcIndex := 0 to frmGoPhast.PhastModel.MobileComponents.Count - 1 do
        begin
          if FCombinedObjects then
          begin
            AValue := GetRealValueFromText(rdgBoundaryConditions.
              Cells[StartingConcIndex + ConcIndex, Index + 1], Dummy);
            ItemName := Format(StrUZTSatEtConcd, [ConcIndex, Index]);
            ValueItem := AScreenObject.ImportedValues.ValueItemByName(
              ItemName);
            if ValueItem = nil then
            begin
              ValueItem := AScreenObject.
                ImportedValues.Add as TValueArrayItem;
              ValueItem.Name := ItemName;
              ValueItem.Values.DataType := rdtDouble;
              ValueItem.Values.Count := 0;
              UztSatEtItem.Mt3dmsConcRate[ConcIndex] := rsObjectImportedValuesR
                + '("' + ItemName + '")';
            end;
            ValueItem.Values.Add(AValue);
          end
          else
          begin
            UztSatEtItem.Mt3dmsConcRate[ConcIndex] := GetRealFormulaFromText(
              rdgBoundaryConditions.Cells[StartingConcIndex + ConcIndex, Index + 1]);
          end;
        end;
      end;

      if UztUnsatETBoundary <> nil then
      begin
        if Count < UztUnsatETBoundary.Values.Count then
        begin
          UztUnsatItem := UztUnsatETBoundary.Values[Count] as TMt3dUztUnsatEtConcItem;
        end
        else
        begin
          UztUnsatItem := UztUnsatETBoundary.Values.Add as TMt3dUztUnsatEtConcItem;
        end;
        UztUnsatItem.StartTime := StartTime;
        UztUnsatItem.EndTime := EndTime;

        Inc(StartingConcIndex, frmGoPhast.PhastModel.MobileComponents.Count);
        for ConcIndex := 0 to frmGoPhast.PhastModel.MobileComponents.Count - 1 do
        begin
          if FCombinedObjects then
          begin
            AValue := GetRealValueFromText(rdgBoundaryConditions.
              Cells[StartingConcIndex + ConcIndex, Index + 1], Dummy);
            ItemName := Format(StrUZTUnSatEtConcd, [ConcIndex, Index]);
            ValueItem := AScreenObject.ImportedValues.ValueItemByName(
              ItemName);
            if ValueItem = nil then
            begin
              ValueItem := AScreenObject.
                ImportedValues.Add as TValueArrayItem;
              ValueItem.Name := ItemName;
              ValueItem.Values.DataType := rdtDouble;
              ValueItem.Values.Count := 0;
              UztUnsatItem.Mt3dmsConcRate[ConcIndex] := rsObjectImportedValuesR
                + '("' + ItemName + '")';
            end;
            ValueItem.Values.Add(AValue);
          end
          else
          begin
            UztUnsatItem.Mt3dmsConcRate[ConcIndex] := GetRealFormulaFromText(
              rdgBoundaryConditions.Cells[StartingConcIndex + ConcIndex, Index + 1]);
          end;
        end;
      end;

    end;
  end;
end;

type
  TMf6ObsRows = (morChdFlow, morDrnFlow,morEvtFlow,morGhbFlow,morRchFlow,morRivFlow,morWelFlow);

procedure TfrmImportShapefile.ImportModflow6Obs(AScreenObject: TScreenObject);
var
  ObsName: string;
  Root: string;
  ObsIndex: Integer;
  GwFlowObs: TGwFlowObs;
  ObsItem: TGwFlowOb;
  GeneralObs: TObGenerals;
  RowIndex: Integer;
  AName: string;
  TypeIndex: Integer;
  PestObsType: string;
  CalibrationObservations: TMf6CalibrationObservations;
  CalibObs: TMf6CalibrationObs;
  MyGuid: TGUID;
  ObsTime: double;
  ObsValue: double;
  ObsWeight: double;
  ShouldIgnore: Boolean;
begin
  if FHeadObsNames = nil then
  begin
    FHeadObsNames := TStringList.Create;
    FHeadObsNames.Sorted := True;
  end;
  AScreenObject.CreateMf6Obs;
  ObsName := GetStringValueFromText(comboModflow6ObsName.Text);
  if ObsName = '' then
  begin
    Root := 'Obs_%d';
    ObsName := 'Obs_1';
  end
  else
  begin
    Root := ObsName + '_%d';
  end;
  ObsIndex := 1;
  While FHeadObsNames.IndexOf(ObsName) >= 0 do
  begin
    ObsName := Format(Root, [ObsIndex]);
    Inc(ObsIndex);
  end;
  AScreenObject.Modflow6Obs.Name := ObsName;
  FHeadObsNames.Add(ObsName);
  if AScreenObject.Modflow6Obs.Name = '' then
  begin
    Inc(FObsCount);
    AScreenObject.Modflow6Obs.Name :=
      'Obs' + IntToStr(FObsCount);
  end;
//  AScreenObject.Modflow6Obs.Used := True;
  GeneralObs := [];
  if cbHeadObservation.Checked then
  begin
    Include(GeneralObs, ogHead);
  end;
//  AScreenObject.Modflow6Obs.HeadObs := cbHeadObservation.Checked;
  if cbDrawdownObservation.Checked then
  begin
    Include(GeneralObs, ogDrawdown);
  end;
//  AScreenObject.Modflow6Obs.DrawdownObs := cbDrawdownObservation.Checked;
  AScreenObject.Modflow6Obs.GroundwaterFlowObs
    := cbGroundwaterFlowObservation.Checked;

  GwFlowObs := [];
  for ObsItem := Low(TGwFlowOb) to High(TGwFlowOb) do
  begin
    if chklstFlowObs.Checked[Ord(ObsItem)] then
    begin
      GwFlowObs := GwFlowObs + [ObsItem];
    end;
  end;
  AScreenObject.Modflow6Obs.GwFlowObsChoices := GwFlowObs;

  if chklstBoundaryFlow.Checked[Ord(morChdFlow)] then
  begin
    Include(GeneralObs, ogCHD);
  end;
  if chklstBoundaryFlow.Checked[Ord(morDrnFlow)] then
  begin
    Include(GeneralObs, ogDrain);
  end;
  if chklstBoundaryFlow.Checked[Ord(morGhbFlow)] then
  begin
    Include(GeneralObs, ogGHB);
  end;
  if chklstBoundaryFlow.Checked[Ord(morRivFlow)] then
  begin
    Include(GeneralObs, ogRiv);
  end;
  if chklstBoundaryFlow.Checked[Ord(morWelFlow)] then
  begin
    Include(GeneralObs, ogWell);
  end;
  if chklstBoundaryFlow.Checked[Ord(morRchFlow)] then
  begin
    Include(GeneralObs, ogRch);
  end;
  if chklstBoundaryFlow.Checked[Ord(morEvtFlow)] then
  begin
    Include(GeneralObs, ogEVT);
  end;

  AScreenObject.Modflow6Obs.General := GeneralObs;

  CalibrationObservations := AScreenObject.Modflow6Obs.CalibrationObservations;
  if frmGoPhast.PhastModel.PestUsed then
  begin
    for RowIndex := 1 to seBoundaryTimeCount.AsInteger do
    begin
      AName := GetStringValueFromText(rdgBoundaryConditions.Cells[Ord(mpocName), RowIndex]);
//      if AName = '' then
//      begin
//        Continue;
//      end;
      TypeIndex := PestObsTypes.IndexOf(rdgBoundaryConditions.Cells[Ord(mpocType), RowIndex]);
      if TypeIndex < 0 then
      begin
        PestObsType := GetStringValueFromText(rdgBoundaryConditions.Cells[Ord(mpocType), RowIndex]);
        TypeIndex := PestObsTypes.IndexOf(PestObsType);
      end;
      if TypeIndex < 0 then
      begin
        Continue;
      end;

      if AName = '' then
      begin
        AName := ReplaceStr(PestObsType, ' ', '_');
        AName := Format('%0:s_%1:d_%2:d', [AName, FShapeIndex+1, RowIndex]);
      end;

      ObsTime := GetRealValueFromText(rdgBoundaryConditions.Cells[Ord(mpocTime), RowIndex], ShouldIgnore);
      if ShouldIgnore then
      begin
        Continue;
      end;
      ObsValue := GetRealValueFromText(rdgBoundaryConditions.Cells[Ord(mpocValue), RowIndex], ShouldIgnore);
      if ShouldIgnore then
      begin
        Continue;
      end;
      ObsWeight := GetRealValueFromText(rdgBoundaryConditions.Cells[Ord(mpocWeight), RowIndex], ShouldIgnore);
      if ShouldIgnore then
      begin
        Continue;
      end;

      CalibObs := CalibrationObservations.Add;
      CalibObs.Name := AName;
      CalibObs.ObSeries := osGeneral;
      CalibObs.ObGeneral := TObGeneral(TypeIndex);
      if CreateGUID(MyGuid) = 0 then
      begin
        CalibObs.GUID := GUIDToString(MyGuid);
      end;
      CalibObs.Time := ObsTime;
      CalibObs.ObservedValue := ObsValue;
      CalibObs.Weight := ObsWeight;
    end;
    CalibrationObservations.MultiLayer := GetBooleanValueFromText(comboMultilayer.Text);
  end;
//  TMf6PestObsColumns = (mpocName, mpocType, mpocTime, mpocValue, mpocWeight);

//  AScreenObject.Modflow6Obs.ChdFlowObs := chklstBoundaryFlow.Checked[Ord(morChdFlow)];
//  AScreenObject.Modflow6Obs.DrnFlowObs := chklstBoundaryFlow.Checked[Ord(morDrnFlow)];
//  AScreenObject.Modflow6Obs.GhbFlowObs := chklstBoundaryFlow.Checked[Ord(morGhbFlow)];
//  AScreenObject.Modflow6Obs.RivFlowObs := chklstBoundaryFlow.Checked[Ord(morRivFlow)];
//  AScreenObject.Modflow6Obs.WelFlowObs := chklstBoundaryFlow.Checked[Ord(morWelFlow)];
//  AScreenObject.Modflow6Obs.RchFlowObs := chklstBoundaryFlow.Checked[Ord(morRchFlow)];
//  AScreenObject.Modflow6Obs.EvtFlowObs := chklstBoundaryFlow.Checked[Ord(morEvtFlow)];


end;

procedure TfrmImportShapefile.ImportModflowCFP_Pipe(
  AScreenObject: TScreenObject);
var
  Boundary: TCfpPipeBoundary;
begin
  AScreenObject.CreateCfpBoundary;
  Boundary := AScreenObject.ModflowCfpPipes;
  Boundary.IsUsed := True;
  Boundary.Diameter := GetRealFormulaFromText(comboCfpDiameter.Text);
  Boundary.Tortuosity := GetRealFormulaFromText(comboCfpTortuosity.Text);
  Boundary.RoughnessHeight := GetRealFormulaFromText(comboCfpRoughnessHeight.Text);
  Boundary.LowerCriticalR := GetRealFormulaFromText(comboCfpLowerReynolds.Text);
  Boundary.HigherCriticalR := GetRealFormulaFromText(comboCfpHigherReynolds.Text);
  Boundary.ConductancePermeability := GetRealFormulaFromText(comboCfbConductance.Text);
  Boundary.Elevation := GetRealFormulaFromText(comboCfpPipeElevation.Text);
  Boundary.RecordNodeValues := GetBooleanValueFromText(comboCfpSaveNodeValues.Text);
  Boundary.RecordPipeValues := GetBooleanValueFromText(comboCfpSavePipeValues.Text);

end;

procedure TfrmImportShapefile.ImportModflowHfbPackage(
  AScreenObject: TScreenObject);
var
  Boundary: THfbBoundary;
  AValue: Extended;
  ItemName: string;
  ValueItem: TValueArrayItem;
  Dummy: Boolean;
begin
  AScreenObject.CreateHfbBoundary;
  Boundary := AScreenObject.ModflowHfbBoundary;
  Boundary.IsUsed := True;

  if FCombinedObjects then
  begin
    AValue := GetRealValueFromText(comboHfbHydCond.Text, Dummy);
    ItemName := StrHFBHydraulicConduct;
    ValueItem := AScreenObject.ImportedValues.ValueItemByName(
      ItemName);
    if ValueItem = nil then
    begin
      ValueItem := AScreenObject.
        ImportedValues.Add as TValueArrayItem;
      ValueItem.Name := ItemName;
      ValueItem.Values.DataType := rdtDouble;
      ValueItem.Values.Count := 0;
      Boundary.HydraulicConductivityFormula := rsObjectImportedValuesR
        + '("' + ItemName + '")';
    end;
    ValueItem.Values.Add(AValue);

    AValue := GetRealValueFromText(comboHfbThickness.Text, Dummy);
    ItemName := StrHFBThickness;
    ValueItem := AScreenObject.ImportedValues.ValueItemByName(
      ItemName);
    if ValueItem = nil then
    begin
      ValueItem := AScreenObject.
        ImportedValues.Add as TValueArrayItem;
      ValueItem.Name := ItemName;
      ValueItem.Values.DataType := rdtDouble;
      ValueItem.Values.Count := 0;
      Boundary.ThicknessFormula := rsObjectImportedValuesR
        + '("' + ItemName + '")';
    end;
    ValueItem.Values.Add(AValue);
  end
  else
  begin
    Boundary.HydraulicConductivityFormula :=
      GetRealFormulaFromText(comboHfbHydCond.Text);
    Boundary.ThicknessFormula :=
      GetRealFormulaFromText(comboHfbThickness.Text);
  end;
  Boundary.AdjustmentMethod := TAdjustmentMethod(rgAngleAdjustment.ItemIndex);
end;

procedure TfrmImportShapefile.ImportModflowLakMf6Package(
  AScreenObject: TScreenObject);
var
  Boundary: TLakeMf6;
  Connections: TLakeConnectionTypes;
  Index: Integer;
  UseRow: Boolean;
  StartTime: double;
  EndTime: double;
  Item: TLakeTimeItem;
  ItemIndex: Integer;
  AValue: string;
  Dummy: Boolean;
begin
  AScreenObject.CreateLakMf6Boundary;
  Boundary := AScreenObject.ModflowLak6;

  Boundary.Embedded := GetBooleanValueFromText(comboLakeMf6Embeded.Text);
  Connections := [];
  if GetBooleanValueFromText(comboHorizontal.Text) then
  begin
    Connections := Connections + [lctHorizontal];
  end;
  if GetBooleanValueFromText(comboVertical.Text) then
  begin
    Connections := Connections + [lctVertical];
  end;
  Boundary.LakeConnections := Connections;
  Boundary.BottomElevation := GetRealFormulaFromText(comboBottomElev.Text);
  Boundary.TopElevation := GetRealFormulaFromText(comboTopElev.Text);
  Boundary.BedK := GetRealFormulaFromText(comboLakebedK.Text);
  Boundary.BedThickness := GetRealFormulaFromText(comboLakebedThickness.Text);
  Boundary.StartingStage := GetRealFormulaFromText(comboStartingStage.Text);
  Boundary.ConnectionLength := GetRealFormulaFromText(comboConnLength.Text);

  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[0, Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[1, Index + 1] <> '');
    if UseRow then
    begin
      StartTime := GetRealValueFromText(
        rdgBoundaryConditions.Cells[Ord(l6cStartTim), Index + 1], Dummy);
      EndTime := GetRealValueFromText(
        rdgBoundaryConditions.Cells[Ord(l6cEndTime), Index + 1], Dummy);

      Item := Boundary.Values.Add as TLakeTimeItem;
      Item.StartTime := StartTime;
      Item.EndTime := EndTime;

      ItemIndex := rdgBoundaryConditions.ItemIndex[Ord(l6cStatus), Index + 1];
      if ItemIndex >= 0 then
      begin
        Item.Status := TLakeStatus(ItemIndex);
      end;
//  TLakeMf6Columns = (l6cStartTim, l6cEndTime, l6cStatus, l6cStage, l6cRainfall,
//    l6cEvaporation, l6cRunoff, l6cInflow, l6cWithdrawal);


      AValue := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
        Ord(l6cStage), Index + 1]);
      Item.Stage := AValue;

      AValue := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
        Ord(l6cRainfall), Index + 1]);
      Item.Rainfall := AValue;

      AValue := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
        Ord(l6cEvaporation), Index + 1]);
      Item.Evaporation := AValue;

      AValue := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
        Ord(l6cRunoff), Index + 1]);
      Item.Runoff := AValue;

      AValue := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
        Ord(l6cInflow), Index + 1]);
      Item.Inflow := AValue;

      AValue := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
        Ord(l6cWithdrawal), Index + 1]);
      Item.Withdrawal := AValue;

//      ConcItem := CreateConcItem(ConcBoundary, Index, Item);
//      ImportConcItemForSeparateShapes(Index, ConcItem,
//        StartingConcIndex);

    end;
  end
end;

procedure TfrmImportShapefile.ImportModflowLakPackage(
  AScreenObject: TScreenObject);
var
  Index: Integer;
  UseRow: Boolean;
  Item: TLakItem;
  StartTime: Extended;
  EndTime: Extended;
  Boundary: TLakBoundary;
  AValue: string;
  StartingConcIndex: Integer;
  ConcBoundary: TMt3dmsConcBoundary;
  ConcItem: TMt3dmsConcItem;
  Dummy: Boolean;
begin
  AScreenObject.CreateLakBoundary;
  Boundary := AScreenObject.ModflowLakBoundary;

  StartingConcIndex := Ord(lcConcentration);
  ConcBoundary := CreateConcBoundary(AScreenObject);

  Boundary.InitialStage :=
    GetRealValueFromText(comboInitialStage.Text, Dummy);
  Boundary.CenterLake :=
    GetIntegerValueFromText(comboLakeID.Text);
  Boundary.Sill :=
    GetRealValueFromText(comboSill.Text, Dummy);
  Boundary.LakeID :=
    GetIntegerValueFromText(comboLakeID.Text);

  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[0, Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[1, Index + 1] <> '');
    if UseRow then
    begin
      StartTime := GetRealValueFromText(
        rdgBoundaryConditions.Cells[Ord(scStartTime), Index + 1], Dummy);
      EndTime := GetRealValueFromText(
        rdgBoundaryConditions.Cells[Ord(scEndTime), Index + 1], Dummy);

      Item := Boundary.Values.Add as TLakItem;
      Item.StartTime := StartTime;
      Item.EndTime := EndTime;

      AValue := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
        Ord(lcMinStage), Index + 1]);
      Item.MinimumStage := AValue;

      AValue := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
        Ord(lcMaxStage), Index + 1]);
      Item.MaximumStage := AValue;

      AValue := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
        Ord(lcPrecip), Index + 1]);
      Item.Precipitation := AValue;

      AValue := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
        Ord(lcEvap), Index + 1]);
      Item.Evaporation := AValue;

      AValue := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
        Ord(lcRunoff), Index + 1]);
      Item.OverlandRunoff := AValue;

      AValue := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
        Ord(lcWithdrawl), Index + 1]);
      Item.Withdrawal := AValue;

      ConcItem := CreateConcItem(ConcBoundary, Index, Item);
      ImportConcItemForSeparateShapes(Index, ConcItem,
        StartingConcIndex);

    end;
  end;
end;

procedure TfrmImportShapefile.ImportModflowStrPackage(
  AScreenObject: TScreenObject);
var
  Boundary: TStrBoundary;
  Index: Integer;
  UseRow: Boolean;
  InitializeGrid: Boolean;
  CellText: AnsiString;
  CachedPosition: Integer;
  FieldStorage: TFieldNumStorage;
  FieldNumber: Integer;
  ColIndex: Integer;
  First: Boolean;
  ParameterName: string;
  ParamItem: TModflowParamItem;
  Param: TModflowTransientListParameter;
  AnItem: TCustomModflowBoundaryItem;
  Count: Integer;
  StartTime: Double;
  EndTime: Double;
  Item: TStrItem;
  StageCalculated: Boolean;
begin
  AScreenObject.CreateStrBoundary;
  Boundary := AScreenObject.ModflowStrBoundary;

  InitializeGrid := False;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(strcStartTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(strcEndTime), Index + 1] <> '');
    if UseRow then
    begin
      InitializeGrid := rdgBoundaryConditions.Objects[0,Index+1] = nil;
      break;
    end;
  end;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(strcStartTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(strcEndTime), Index + 1] <> '');
    if UseRow then
    begin
      for ColIndex := 0 to rdgBoundaryConditions.ColCount - 1 do
      begin
        if InitializeGrid then
        begin
          CellText := AnsiString(rdgBoundaryConditions.
            Cells[ColIndex, Index + 1]);
          if CellText = '' then
          begin
            CellText := '0';
          end;
          CachedPosition := FFieldNumbers.Indexof(string(CellText));
          if CachedPosition >= 0 then
          begin
            FieldStorage := FFieldNumbers.Objects[CachedPosition]
              as TFieldNumStorage;
          end
          else
          begin
            FieldNumber := GetFieldNumberFromName(CellText);
            FieldStorage := TFieldNumStorage.Create(xbShapeDataBase);
            FieldStorage.FieldNumber := FieldNumber;
            FieldStorage.Formula := string(CellText);
            FFieldNumbers.AddObject(string(CellText), FieldStorage);
            if FieldNumber = 0 then
            begin
              FieldStorage.RealValue := StrToFloatDef(string(CellText), 0);
              FieldStorage.IntValue := StrToIntDef(string(CellText), 0);
            end;
            FieldStorage.StringValue := string(CellText);
          end;
          rdgBoundaryConditions.Objects[ColIndex, Index+1] := FieldStorage;
        end
        else
        begin
          FieldStorage := TFieldNumStorage(rdgBoundaryConditions.
            Objects[ColIndex, Index+1]);
          FieldStorage.Cached := False;
        end;
      end;
    end;
  end;

  First := True;
  ParamItem := nil;
  Count := 0;
  StageCalculated := frmGoPhast.PhastModel.ModflowPackages.StrPackage.CalculateStage;

  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(strcStartTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(strcEndTime), Index + 1] <> '');
    if UseRow then
    begin
      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(strcStartTime), Index + 1]);
      StartTime := FieldStorage.GetRealValue;
      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(strcEndTime), Index + 1]);
      EndTime := FieldStorage.GetRealValue;
      if First then
      begin
        First := False;
        Boundary.FormulaInterpretation := TFormulaInterpretation(
          comboConductanceInterpSTR.ItemIndex);

        if comboStrSegmentNumber.Text = '' then
        begin
          Boundary.SegmentNumber := FShapeIndex + 2;
        end
        else
        begin
          Boundary.SegmentNumber :=
            GetIntegerValueFromText(comboStrSegmentNumber.Text);
        end;

        ParameterName := comboStrParameterName.Text;
        ParameterName := StringReplace(ParameterName, '"', '',
          [rfReplaceAll, rfIgnoreCase]);
        if ParameterName = '' then
        begin
          Param := nil;
        end
        else
        begin
          Param := frmGoPhast.PhastModel.
            ModflowTransientParameters.GetParamByName(ParameterName);
          if Param = nil then
          begin
            FInvalidParameterNames.Add(ParameterName);
          end
          else if Param.ParameterType <> ptSTR then
          begin
            Param := nil;
            FInvalidParameterNames.Add(ParameterName);
          end;
        end;
      end;

      GetNewOrExistingBoundaryItem(AnItem, ParameterName, Param, ParamItem,
        Boundary, Count);


      Item := AnItem as TStrItem;
      Item.StartTime := StartTime;
      Item.EndTime := EndTime;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(strcOutflow), Index + 1]);
      Item.OutflowSegment := FieldStorage.GetIntValue;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(strcDiversion), Index + 1]);
      Item.DiversionSegment := FieldStorage.GetIntValue;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(strcFlow), Index + 1]);
      Item.Flow := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(strcHead), Index + 1]);
      Item.Stage := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(strcConductance), Index + 1]);
      Item.Conductance := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(strcBedBottom), Index + 1]);
      Item.BedBottom := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(strcBedTop), Index + 1]);
      Item.BedTop := FieldStorage.RealFormula;

      if StageCalculated then
      begin
        FieldStorage := TFieldNumStorage(
          rdgBoundaryConditions.Objects[Ord(strcWidth), Index + 1]);
        Item.Width := FieldStorage.RealFormula;

        FieldStorage := TFieldNumStorage(
          rdgBoundaryConditions.Objects[Ord(strcSlope), Index + 1]);
        Item.Slope := FieldStorage.RealFormula;

        FieldStorage := TFieldNumStorage(
          rdgBoundaryConditions.Objects[Ord(strcRoughness), Index + 1]);
        Item.Roughness := FieldStorage.RealFormula;

      end;

    end;
  end
end;

procedure TfrmImportShapefile.ImportModflowSfrPackage(
  AScreenObject: TScreenObject);
var
  Index: Integer;
  UseRow: Boolean;
  Item: TSfrItem;
  IcalcItem: TSfrParamIcalcItem;
  StartTime: Extended;
  EndTime: Extended;
  SegmentFlowItem: TSfrSegmentFlowItem;
  ChannelItem: TSfrChannelItem;
  EqItem: TSfrEquationItem;
  SegItem: TSfrSegmentItem;
  First: boolean;
  Boundary: TSfrBoundary;
  IPrior: integer;
  RCHLEN: string;
  FieldNumber: Integer;
  Value: Extended;
  ICalc: Integer;
  InitializeGrid: boolean;
  ColIndex: Integer;
  CellText: AnsiString;
  CachedPosition: Integer;
  FieldStorage: TFieldNumStorage;
  SFR_Package: TSfrPackageSelection;
begin
  SFR_Package := frmGoPhast.PhastModel.ModflowPackages.SfrPackage;
  First := True;
  AScreenObject.CreateSfrBoundary;
  Boundary := AScreenObject.ModflowSfrBoundary;
  InitializeGrid := False;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[0, Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[1, Index + 1] <> '');
    if UseRow then
    begin
      InitializeGrid := rdgBoundaryConditions.Objects[0,Index+1] = nil;
      break;
    end;
  end;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[0, Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[1, Index + 1] <> '');
    if UseRow then
    begin
      for ColIndex := 0 to rdgBoundaryConditions.ColCount - 1 do
      begin
        if InitializeGrid then
        begin
          CellText := AnsiString(rdgBoundaryConditions.
            Cells[ColIndex, Index + 1]);
          if CellText = '' then
          begin
            CellText := '0';
          end;
          CachedPosition := FFieldNumbers.Indexof(string(CellText));
          if CachedPosition >= 0 then
          begin
            FieldStorage := FFieldNumbers.Objects[CachedPosition]
              as TFieldNumStorage;
          end
          else
          begin
            FieldNumber := GetFieldNumberFromName(CellText);
            FieldStorage := TFieldNumStorage.Create(xbShapeDataBase);
            FieldStorage.FieldNumber := FieldNumber;
            FieldStorage.Formula := string(CellText);
            FFieldNumbers.AddObject(string(CellText), FieldStorage);
            if FieldNumber = 0 then
            begin
              FieldStorage.RealValue := StrToFloatDef(string(CellText), 0);
              FieldStorage.IntValue := StrToIntDef(string(CellText), 0);
            end;
          end;
          rdgBoundaryConditions.Objects[ColIndex, Index+1] := FieldStorage;
        end
        else
        begin
          FieldStorage := TFieldNumStorage(rdgBoundaryConditions.
            Objects[ColIndex, Index+1]);
          FieldStorage.Cached := False;
        end;
      end;
    end;
  end;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[0, Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[1, Index + 1] <> '');
    if UseRow then
    begin
      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scStartTime), Index + 1]);
      StartTime := FieldStorage.GetRealValue;
      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scEndTime), Index + 1]);
      EndTime := FieldStorage.GetRealValue;
      if First then
      begin
        First := False;

        if comboSfrSegmentNumber.Text = '' then
        begin
          Boundary.SegmentNumber := FShapeIndex + 1;
        end
        else
        begin
          Boundary.SegmentNumber :=
            GetIntegerValueFromText(comboSfrSegmentNumber.Text);
        end;

        Item := Boundary.Values.Add as TSfrItem;
        Item.StartTime := StartTime;
        Item.EndTime := EndTime;

        FieldNumber := GetFieldNumberFromName(
          AnsiString(comboSfrReachLength.Text));
        if FieldNumber = 0 then
        begin
          // not a field
          if TryStrToFloat(comboSfrReachLength.Text, Value) then
          begin
            RCHLEN := FortranFloatToStr(Value);
          end
          else
          begin
            RCHLEN := comboSfrReachLength.Text;
          end;
        end
        else
        begin
          Value := xbShapeDataBase.GetFieldNum(FieldNumber);
          RCHLEN := FortranFloatToStr(Value);
        end;
        Item.ReachLength := RCHLEN;
        Item.HydraulicConductivity :=
          GetRealFormulaFromText(comboSfrStreambedKv.Text,
          SFR_Package.Isfropt in [1,2,3], True);
        Item.StreamBedThickness :=
          GetRealFormulaFromText(comboSfrStreambedThickness.Text,
          SFR_Package.Isfropt in [1,2,3], True);
        Item.StreambedElevation :=
          GetRealFormulaFromText(comboSfrStreambedTop.Text,
          SFR_Package.Isfropt in [1,2,3], True);
        Item.StreamSlope :=
          GetRealFormulaFromText(comboSfrStreamSlope.Text,
          SFR_Package.Isfropt in [1,2,3], True);
        Item.SaturatedWaterContent :=
          GetRealFormulaFromText(comboSaturatedVolumetricWater.Text,
          SFR_Package.Isfropt in [2,3], True);
        Item.InitialWaterContent :=
          GetRealFormulaFromText(comboInitialVolumetricWater.Text,
          SFR_Package.Isfropt in [2,3], True);
        Item.BrooksCoreyExponent :=
          GetRealFormulaFromText(comboBrooksCoreyExponent.Text,
          SFR_Package.Isfropt in [2,3], True);
        Item.VerticalK :=
          GetRealFormulaFromText(comboaxUnsaturatedKz.Text,
          SFR_Package.Isfropt = 3, True);
      end;

      IcalcItem := Boundary.ParamIcalc.Add as TSfrParamIcalcItem;
      IcalcItem.StartTime := StartTime;
      IcalcItem.EndTime := EndTime;
      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scIcalc), Index + 1]);
      ICalc := FieldStorage.GetIntValue;
      if (ICalc < 0) or (ICalc > 4) then
      begin
        ICalc := 0;
      end;
      IcalcItem.ICalc := ICalc;
      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scOutflowSegment), Index + 1]);
      IcalcItem.OutflowSegment := FieldStorage.GetIntValue;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scDiversionSegment), Index + 1]);
      IcalcItem.DiversionSegment := FieldStorage.GetIntValue;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scIprior), Index + 1]);
      IPrior := FieldStorage.GetIntValue;

      if (IPrior > 0) or (IPrior < -3) then
      begin
        IPrior := 0;
      end;
      IcalcItem.IPRIOR := IPrior;

      SegmentFlowItem := Boundary.SegmentFlows.Add as TSfrSegmentFlowItem;
      SegmentFlowItem.StartTime := StartTime;
      SegmentFlowItem.EndTime := EndTime;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scFlow), Index + 1]);
      SegmentFlowItem.Flow := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scPtsw), Index + 1]);
      SegmentFlowItem.Precipitation := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scEtsw), Index + 1]);
      SegmentFlowItem.Evapotranspiration := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scRunoff), Index + 1]);
      SegmentFlowItem.Runnoff := FieldStorage.RealFormula;

      if ICalc in [1,2] then
      begin
        ChannelItem := Boundary.ChannelValues.Add as TSfrChannelItem;
        ChannelItem.StartTime := StartTime;
        ChannelItem.EndTime := EndTime;

        FieldStorage := TFieldNumStorage(
          rdgBoundaryConditions.Objects[Ord(scRoughCh), Index + 1]);
        ChannelItem.ChannelRoughness := FieldStorage.RealFormula;

        FieldStorage := TFieldNumStorage(
          rdgBoundaryConditions.Objects[Ord(scRoughBk), Index + 1]);
        ChannelItem.BankRoughness := FieldStorage.RealFormula;

        if ICalc = 2 then
        begin
          FieldStorage := TFieldNumStorage(
            rdgBoundaryConditions.Objects[Ord(scDist1), Index + 1]);
          ChannelItem.X[0] := FieldStorage.RealFormula;

          FieldStorage := TFieldNumStorage(
            rdgBoundaryConditions.Objects[Ord(scDist2), Index + 1]);
          ChannelItem.X[1] := FieldStorage.RealFormula;

          FieldStorage := TFieldNumStorage(
            rdgBoundaryConditions.Objects[Ord(scDist3), Index + 1]);
          ChannelItem.X[2] := FieldStorage.RealFormula;

          FieldStorage := TFieldNumStorage(
            rdgBoundaryConditions.Objects[Ord(scDist4), Index + 1]);
          ChannelItem.X[3] := FieldStorage.RealFormula;

          FieldStorage := TFieldNumStorage(
            rdgBoundaryConditions.Objects[Ord(scDist5), Index + 1]);
          ChannelItem.X[4] := FieldStorage.RealFormula;

          FieldStorage := TFieldNumStorage(
            rdgBoundaryConditions.Objects[Ord(scDist6), Index + 1]);
          ChannelItem.X[5] := FieldStorage.RealFormula;

          FieldStorage := TFieldNumStorage(
            rdgBoundaryConditions.Objects[Ord(scDist7), Index + 1]);
          ChannelItem.X[6] := FieldStorage.RealFormula;

          FieldStorage := TFieldNumStorage(
            rdgBoundaryConditions.Objects[Ord(scDist8), Index + 1]);
          ChannelItem.X[7] := FieldStorage.RealFormula;

          FieldStorage := TFieldNumStorage(
            rdgBoundaryConditions.Objects[Ord(scZ1), Index + 1]);
          ChannelItem.Z[0] := FieldStorage.RealFormula;

          FieldStorage := TFieldNumStorage(
            rdgBoundaryConditions.Objects[Ord(scZ2), Index + 1]);
          ChannelItem.Z[1] := FieldStorage.RealFormula;

          FieldStorage := TFieldNumStorage(
            rdgBoundaryConditions.Objects[Ord(scZ3), Index + 1]);
          ChannelItem.Z[2] := FieldStorage.RealFormula;

          FieldStorage := TFieldNumStorage(
            rdgBoundaryConditions.Objects[Ord(scZ4), Index + 1]);
          ChannelItem.Z[3] := FieldStorage.RealFormula;

          FieldStorage := TFieldNumStorage(
            rdgBoundaryConditions.Objects[Ord(scZ5), Index + 1]);
          ChannelItem.Z[4] := FieldStorage.RealFormula;

          FieldStorage := TFieldNumStorage(
            rdgBoundaryConditions.Objects[Ord(scZ6), Index + 1]);
          ChannelItem.Z[5] := FieldStorage.RealFormula;

          FieldStorage := TFieldNumStorage(
            rdgBoundaryConditions.Objects[Ord(scZ7), Index + 1]);
          ChannelItem.Z[6] := FieldStorage.RealFormula;

          FieldStorage := TFieldNumStorage(
            rdgBoundaryConditions.Objects[Ord(scZ8), Index + 1]);
          ChannelItem.Z[7] := FieldStorage.RealFormula;
        end;
      end;

      if ICalc = 3 then
      begin
        EqItem := Boundary.EquationValues.Add as TSfrEquationItem;
        EqItem.StartTime := StartTime;
        EqItem.EndTime := EndTime;

        FieldStorage := TFieldNumStorage(
          rdgBoundaryConditions.Objects[Ord(scCdpth), Index + 1]);
        EqItem.DepthCoefficient := FieldStorage.RealFormula;

        FieldStorage := TFieldNumStorage(
          rdgBoundaryConditions.Objects[Ord(scFdpth), Index + 1]);
        EqItem.DepthExponent := FieldStorage.RealFormula;

        FieldStorage := TFieldNumStorage(
          rdgBoundaryConditions.Objects[Ord(scAwdth), Index + 1]);
        EqItem.WidthCoefficient := FieldStorage.RealFormula;

        FieldStorage := TFieldNumStorage(
          rdgBoundaryConditions.Objects[Ord(edBwdth), Index + 1]);
        EqItem.WidthExponent := FieldStorage.RealFormula;
      end;

      SegItem := Boundary.UpstreamSegmentValues.Add as TSfrSegmentItem;
      SegItem.StartTime := StartTime;
      SegItem.EndTime := EndTime;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scHcond1), Index + 1]);
      SegItem.HydraulicConductivity := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scThickM1), Index + 1]);
      SegItem.StreamBedThickness := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scElevUp), Index + 1]);
      SegItem.StreambedElevation := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scWidth1), Index + 1]);
      SegItem.StreamWidth := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scDepth1), Index + 1]);
      SegItem.StreamDepth := FieldStorage.RealFormula;

      SegItem := Boundary.DownstreamSegmentValues.Add as TSfrSegmentItem;
      SegItem.StartTime := StartTime;
      SegItem.EndTime := EndTime;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scHcond2), Index + 1]);
      SegItem.HydraulicConductivity := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scThickM2), Index + 1]);
      SegItem.StreamBedThickness := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scElevDn), Index + 1]);
      SegItem.StreambedElevation := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scWidth2), Index + 1]);
      SegItem.StreamWidth := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scDepth2), Index + 1]);
      SegItem.StreamDepth := FieldStorage.RealFormula;
    end;
  end;
end;

procedure TfrmImportShapefile.ImportModflowSfr_MF6_Package(
  AScreenObject: TScreenObject);
var
  Index: Integer;
  UseRow: Boolean;
  Item: TSfrMf6Item;
//  IcalcItem: TSfrParamIcalcItem;
  StartTime: Extended;
  EndTime: Extended;
//  SegmentFlowItem: TSfrSegmentFlowItem;
//  ChannelItem: TSfrChannelItem;
//  EqItem: TSfrEquationItem;
//  SegItem: TSfrSegmentItem;
//  First: boolean;
  Boundary: TSfrMf6Boundary;
//  IPrior: integer;
//  RCHLEN: string;
  FieldNumber: Integer;
//  Value: Extended;
//  ICalc: Integer;
  InitializeGrid: boolean;
  ColIndex: Integer;
  CellText: AnsiString;
  CachedPosition: Integer;
  FieldStorage: TFieldNumStorage;
//  SFR_Mf6_Package: TSfrModflow6PackageSelection;
  StatusIndex: Integer;
  StatusStr: String;
  DownstreamIndex: Integer;
  ASegmentNumber: Integer;
  PriorityIndex: Integer;
  DiversionItem: TSDiversionItem;
  PIndex: TDivisionPriority;
  DiversionIndex: Integer;
  StringValue: string;
  NoDiversionColCount: Integer;
begin
//  SFR_Mf6_Package := frmGoPhast.PhastModel.ModflowPackages.SfrModflow6Package;
//  First := True;
  AScreenObject.CreateSfr6Boundary;
  Boundary := AScreenObject.ModflowSfr6Boundary;
  InitializeGrid := False;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[0, Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[1, Index + 1] <> '');
    if UseRow then
    begin
      InitializeGrid := rdgBoundaryConditions.Objects[0,Index+1] = nil;
      break;
    end;
  end;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[0, Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[1, Index + 1] <> '');
    if UseRow then
    begin
      for ColIndex := 0 to rdgBoundaryConditions.ColCount - 1 do
      begin
        if InitializeGrid then
        begin
          CellText := AnsiString(rdgBoundaryConditions.
            Cells[ColIndex, Index + 1]);
          if CellText = '' then
          begin
            CellText := '0';
          end;
          CachedPosition := FFieldNumbers.Indexof(string(CellText));
          if CachedPosition >= 0 then
          begin
            FieldStorage := FFieldNumbers.Objects[CachedPosition]
              as TFieldNumStorage;
          end
          else
          begin
            FieldNumber := GetFieldNumberFromName(CellText);
            FieldStorage := TFieldNumStorage.Create(xbShapeDataBase);
            FieldStorage.FieldNumber := FieldNumber;
            FieldStorage.Formula := string(CellText);
            FFieldNumbers.AddObject(string(CellText), FieldStorage);
            if FieldNumber = 0 then
            begin
              FieldStorage.RealValue := StrToFloatDef(string(CellText), 0);
              FieldStorage.IntValue := StrToIntDef(string(CellText), 0);
            end;
          end;
          rdgBoundaryConditions.Objects[ColIndex, Index+1] := FieldStorage;
        end
        else
        begin
          FieldStorage := TFieldNumStorage(rdgBoundaryConditions.
            Objects[ColIndex, Index+1]);
          FieldStorage.Cached := False;
        end;
      end;
    end;
  end;

  if combolSegNum.Text = '' then
  begin
    Boundary.SegmentNumber := FShapeIndex + 1;
  end
  else
  begin
    Boundary.SegmentNumber :=
      GetIntegerValueFromText(combolSegNum.Text);
  end;

  Boundary.ReachLength :=
    GetRealFormulaFromText(comboReachLengthMf6.Text,
    True, True);
  Boundary.ReachWidth :=
    GetRealFormulaFromText(comboRwid.Text,
    True, True);
  Boundary.Gradient :=
    GetRealFormulaFromText(comboGrd.Text,
    True, True);
  Boundary.StreambedTop :=
    GetRealFormulaFromText(comboRtp.Text,
    True, True);
  Boundary.StreambedThickness :=
    GetRealFormulaFromText(comboRbth.Text,
    True, True);
  Boundary.HydraulicConductivity :=
    GetRealFormulaFromText(comboRhk.Text,
    True, True);

  NoDiversionColCount := Ord(High(TSfr_Mf6_Column)) + 1;

  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[0, Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[1, Index + 1] <> '');
    if UseRow then
    begin
      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scStartTime), Index + 1]);
      StartTime := FieldStorage.GetRealValue;
      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scEndTime), Index + 1]);
      EndTime := FieldStorage.GetRealValue;

      Item := Boundary.Values.Add as TSfrMf6Item;
      Item.StartTime := StartTime;
      Item.EndTime := EndTime;

      StatusIndex := rdgBoundaryConditions.ItemIndex[Ord(smcStatus), Index + 1];
      if (StatusIndex > Ord(High(TStreamStatus))) and (StatusIndex >= 0) then
      begin
        FieldStorage := TFieldNumStorage(
          rdgBoundaryConditions.Objects[Ord(smcStatus), Index + 1]);
        StatusStr := FieldStorage.GetStringValue;
        if SameText(StatusStr, 'inactive') then
        begin
          Item.StreamStatus := ssInactive;
        end
        else if SameText(StatusStr, 'active') then
        begin
          Item.StreamStatus := ssActive;
        end
        else if SameText(StatusStr, 'simple') then
        begin
          Item.StreamStatus := ssSimple;
        end
        else
        begin
          Item.StreamStatus := ssInactive;
        end;
      end
      else
      begin
        Item.StreamStatus := TStreamStatus(StatusIndex);
      end;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(smcStage), Index + 1]);
      Item.Stage := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(smcInflow), Index + 1]);
      Item.Inflow := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(smcRainfall), Index + 1]);
      Item.Rainfall := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(smcEvaporation), Index + 1]);
      Item.Evaporation := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(smcRunoff), Index + 1]);
      Item.Runoff := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(smcRoughness), Index + 1]);
      Item.Roughness := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(smcUpstreamFraction), Index + 1]);
      Item.UpstreamFraction := FieldStorage.RealFormula;

      Item.DiversionCount := frameDiversionsSfrMf6.seNumber.AsInteger;
      for DiversionIndex := 0 to Item.DiversionCount - 1 do
      begin
        FieldStorage := TFieldNumStorage(
          rdgBoundaryConditions.Objects[
          DiversionIndex + NoDiversionColCount, Index + 1]);
        Item.DiversionFormulas[DiversionIndex] := FieldStorage.RealFormula;
      end;

    end;
  end;

  for DownstreamIndex := 1 to frameDownstreamSegmentsSfrMf6.seNumber.AsInteger do
  begin
    ASegmentNumber := GetIntegerValueFromText(frameDownstreamSegmentsSfrMf6.Grid.Cells[0,DownstreamIndex]);
    if ASegmentNumber > 0 then
    begin
      Boundary.DownstreamSegments.Add. Value := ASegmentNumber;
    end;
  end;

  for DiversionIndex := 1 to frameDiversionsSfrMf6.seNumber.AsInteger do
  begin
    ASegmentNumber := GetIntegerValueFromText(
      frameDiversionsSfrMf6.Grid.Cells[0,DiversionIndex]);
    PriorityIndex := frameDiversionsSfrMf6.Grid.ItemIndex[1,DiversionIndex];
    if PriorityIndex > Ord(High(TDivisionPriority)) then
    begin
      StringValue := GetStringValueFromText(
        frameDiversionsSfrMf6.Grid.Cells[1,DiversionIndex]);
      PriorityIndex := -1;
      for PIndex := Low(TDivisionPriority) to High(TDivisionPriority) do
      begin
        if SameText(StringValue,
          frameDiversionsSfrMf6.Grid.Columns[1].Picklist[Ord(PIndex)]) then
        begin
          PriorityIndex := Ord(PIndex);
          break;
        end;
      end;
    end;
    if (ASegmentNumber > 0) and (PriorityIndex >= 0) then
    begin
      DiversionItem := Boundary.Diversions.Add;
      DiversionItem.DownstreamSegment := ASegmentNumber;
      DiversionItem.Priority := TDivisionPriority(PriorityIndex);
    end;
  end;

//  frameDownstreamSegmentsSfrMf6.Grid.Cells[0,0] := StrDownstreamSegmentN;
//  frameDownstreamSegmentsSfrMf6.Grid.Columns[0].PickList := FIntegerFieldNames;
//
//  frameDiversionsSfrMf6.Grid.Cells[0,0] := StrDiversionSegmentNu;
//  frameDiversionsSfrMf6.Grid.Cells[1,0] := StrPriorityCprior;
//  frameDiversionsSfrMf6.Grid.Columns[0].PickList := FIntegerFieldNames;

end;

procedure TfrmImportShapefile.ImportModflowHobPackage(
  AScreenObject: TScreenObject);
var
  UseRow: Boolean;
  Item: THobItem;
  ATime: Extended;
  Index: Integer;
  ShouldIgnoreValues: boolean;
  IgnoreValue: double;
  AValue: Extended;
  AnIntValue: Integer;
  ObsName: string;
  ObsIndex: Integer;
  Root: string;
  ShouldIgnore: Boolean;
  Dummy: Boolean;
begin
  if FHeadObsNames = nil then
  begin
    FHeadObsNames := TStringList.Create;
    FHeadObsNames.Sorted := True;
  end;
  ShouldIgnoreValues := rdeIgnoreValues.Text <> '';
  if ShouldIgnoreValues then
  begin
    IgnoreValue := StrToFloat(rdeIgnoreValues.Text);
  end
  else
  begin
    IgnoreValue := 0;
  end;
  AScreenObject.CreateHeadObservations;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[0, Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[1, Index + 1] <> '');
    if UseRow then
    begin
      ATime := GetRealValueFromText(rdgBoundaryConditions.Cells[0, Index + 1],
        ShouldIgnore);
      if ShouldIgnore then
      begin
        Continue;
      end;
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[1, Index + 1],
        ShouldIgnore);
      if ShouldIgnore then
      begin
        Continue;
      end;
      if ShouldIgnoreValues and ((ATime = IgnoreValue)
        or (AValue = IgnoreValue)) then
      begin
        Continue;
      end;
      Item := AScreenObject.ModflowHeadObservations.Values.Add as THobItem;
      Item.Time := ATime;
      Item.Head := AValue;

      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[2, Index + 1], Dummy);
      Item.Statistic := AValue;
      AnIntValue := GetIntegerValueFromText(
        rdgBoundaryConditions.Cells[3, Index + 1]);
      if (AnIntValue < 0) or (AnIntValue > Ord(High(TStatFlag))) then
      begin
        Item.StatFlag := stVariance;
      end
      else
      begin
        Item.StatFlag := TStatFlag(AnIntValue);
      end;
    end;
  end;
  ObsName := GetStringValueFromText(comboHeadObservationNames.Text);
  if ObsName = '' then
  begin
    Root := 'Obs_%d';
    ObsName := 'Obs_1';
  end
  else
  begin
    Root := ObsName + '_%d';
  end;
  ObsIndex := 1;
  While FHeadObsNames.IndexOf(ObsName) >= 0 do
  begin
    ObsName := Format(Root, [ObsIndex]);
    Inc(ObsIndex);
  end;
  AScreenObject.ModflowHeadObservations.ObservationName := ObsName;
  FHeadObsNames.Add(ObsName);

  if AScreenObject.ModflowHeadObservations.ObservationName = '' then
  begin
    Inc(FObsCount);
    AScreenObject.ModflowHeadObservations.ObservationName :=
      'Obs' + IntToStr(FObsCount);
  end;
  AScreenObject.ModflowHeadObservations.Purpose :=
    TObservationPurpose(comboHeadObsType.ItemIndex);
  AScreenObject.ModflowHeadObservations.MultiObsMethod :=
    TMultiObsMethod(comboITT.ItemIndex);
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForMAW;
var
  TimeIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  ColIndex: TMawColumns;
  WellScreenColIndex: TMawWellScreenColumns;
begin
  plBoundary.ActivePage := jvspModflowMAW;
  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := Ord(High(TMawColumns)) + 1;
  pgcModflowMAW.ActivePageIndex := 0;

  rdgBoundaryConditions.Cells[Ord(mcStartTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Columns[Ord(mcStartTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(mcStartTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(mcStartTime)].PickList.Clear;
  for TimeIndex := 0 to frmGoPhast.PhastModel.ModflowStressPeriods.Count - 1 do
  begin
    StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods.Items[TimeIndex];
    rdgBoundaryConditions.Columns[Ord(mcStartTime)].
      PickList.Add(FloatToStr(StressPeriod.StartTime));
  end;
  rdgBoundaryConditions.Columns[Ord(mcStartTime)].
    PickList.AddStrings(FRealFieldAndGlobalVariablesNames);

  rdgBoundaryConditions.Cells[Ord(mcEndTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Columns[Ord(mcEndTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(mcEndTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(mcEndTime)].PickList.Clear;
  for TimeIndex := 0 to frmGoPhast.PhastModel.ModflowStressPeriods.Count - 1 do
  begin
    StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods.Items[TimeIndex];
    rdgBoundaryConditions.Columns[Ord(mcEndTime)].
      PickList.Add(FloatToStr(StressPeriod.EndTime));
  end;
  rdgBoundaryConditions.Columns[Ord(mcEndTime)].
    PickList.AddStrings(FRealFieldAndGlobalVariablesNames);

  rdgBoundaryConditions.Cells[Ord(mcRate), 0] := StrDesiredPumpingRate;
  rdgBoundaryConditions.Cells[Ord(mcWellHead), 0] := StrWellHead;
  rdgBoundaryConditions.Cells[Ord(mcMawFlowingWellElevation), 0] :=
    StrFlowingWellElevati;
  rdgBoundaryConditions.Cells[Ord(mcFlowingWellConductance), 0] :=
    StrFlowingWellConduct;
  rdgBoundaryConditions.Cells[Ord(mcFlowingWellReductionLength), 0] :=
    'Flowing Well Reduction Length';
  rdgBoundaryConditions.Cells[Ord(mcMinRate), 0] := StrMinimumFlowRate;
  rdgBoundaryConditions.Cells[Ord(mcMaxRate), 0] := StrMaximumFlowRate;
  rdgBoundaryConditions.Cells[Ord(mcPumpElevation), 0] := StrPumpElevation;
  rdgBoundaryConditions.Cells[Ord(mcScalingLength), 0] := StrScalingLength;
  rdgBoundaryConditions.Cells[Ord(mcHeadLimit), 0] := StrHeadLimit;

  for ColIndex in [mcRate, mcWellHead, mcMawFlowingWellElevation,
    mcFlowingWellConductance, mcFlowingWellReductionLength, mcMinRate, mcMaxRate, mcPumpElevation,
    mcScalingLength, mcHeadLimit] do
  begin
    rdgBoundaryConditions.Columns[Ord(ColIndex)].ComboUsed := True;
    rdgBoundaryConditions.Columns[Ord(ColIndex)].Format := rcf4String;
    rdgBoundaryConditions.Columns[Ord(ColIndex)].PickList :=
      FRealFieldAndGlobalVariablesNames;
  end;

  rdgBoundaryConditions.Cells[Ord(mcStatus), 0] := StrStatusMf6;
  rdgBoundaryConditions.Columns[Ord(mcStatus)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(mcStatus)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(mcStatus)].PickList.Clear;
  rdgBoundaryConditions.Columns[Ord(mcStatus)].PickList.Add(StrActive);
  rdgBoundaryConditions.Columns[Ord(mcStatus)].PickList.Add(StrInactive);
  rdgBoundaryConditions.Columns[Ord(mcStatus)].PickList.Add(StrConstantHead);
  rdgBoundaryConditions.Columns[Ord(mcStatus)].PickList.AddStrings(FStringFieldNames);

  rdgBoundaryConditions.Cells[Ord(mcMawFlowingWell), 0] := StrFlowingWell;
  rdgBoundaryConditions.Columns[Ord(mcMawFlowingWell)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(mcMawFlowingWell)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(mcMawFlowingWell)].PickList.Clear;
  rdgBoundaryConditions.Columns[Ord(mcMawFlowingWell)].PickList.Add(StrNotFlowing);
  rdgBoundaryConditions.Columns[Ord(mcMawFlowingWell)].PickList.Add(StrFlowing);
  rdgBoundaryConditions.Columns[Ord(mcMawFlowingWell)].PickList.AddStrings(FStringFieldNames);

  rdgBoundaryConditions.Cells[Ord(mcRateLimitation), 0] := StrRateLimitation;
  rdgBoundaryConditions.Columns[Ord(mcRateLimitation)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(mcRateLimitation)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(mcRateLimitation)].PickList.Clear;
  rdgBoundaryConditions.Columns[Ord(mcRateLimitation)].PickList.Add(StrNone);
  rdgBoundaryConditions.Columns[Ord(mcRateLimitation)].PickList.Add(StrScaling);
  rdgBoundaryConditions.Columns[Ord(mcRateLimitation)].PickList.Add(StrShutoff);
  rdgBoundaryConditions.Columns[Ord(mcRateLimitation)].PickList.AddStrings(FStringFieldNames);

  rdgBoundaryConditions.Cells[Ord(mcHeadLimitChoice), 0] := StrRateLimitation;
  rdgBoundaryConditions.Columns[Ord(mcHeadLimitChoice)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(mcHeadLimitChoice)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(mcHeadLimitChoice)].PickList.Clear;
  rdgBoundaryConditions.Columns[Ord(mcHeadLimitChoice)].PickList.Add(StrHeadNotLimited);
  rdgBoundaryConditions.Columns[Ord(mcHeadLimitChoice)].PickList.Add(StrHeadLimited);
  rdgBoundaryConditions.Columns[Ord(mcHeadLimitChoice)].PickList.AddStrings(FStringFieldNames);

  AssignColFeatureProperties;

{
  TMawColumns = (mcStartTime, mcEndTime, mcStatus, mcRate, mcWellHead,
    mcMawFlowingWell,
    mcMawFlowingWellElevation, mcFlowingWellConductance, mcRateLimitation,
    mcMinRate, mcMaxRate, mcPumpElevation, mcScalingLength, mcHeadLimitChoice,
    mcHeadLimit);

    property MawStatus: TMawStatus read FMawStatus write SetMawStatus;
    property Rate: string read GetRate write SetRate;
    property WellHead: string read GetWellHead write SetWellHead;

    property FlowingWell: TFlowingWell read FFlowingWell write SetFlowingWell;
    property FlowingWellElevation: string read GetFlowingWellElevation write SetFlowingWellElevation;
    property FlowingWellConductance: string read GetFlowingWellConductance write SetFlowingWellConductance;

    // ShutOff and RateScaling can not be used simultaneously.
    // RateLimitation chooses between no-limit, ShutOff, and RateScaling.
    property RateLimitation: TRateLimitation read FRateLimitation write SetRateLimitation;
    property MinRate: string read GetMinRate write SetMinRate;
    property MaxRate: string read GetMaxRate write SetMaxRate;

    property PumpElevation: string read GetPumpElevation write SetPumpElevation;
    property ScalingLength: string read GetScalingLength write SetScalingLength;

    property HeadLimitChoice: Boolean read FHeadLimitChoice write SetHeadLimitChoice;
    property HeadLimit: string read GetHeadLimit write SetHeadLimit;
}
  comboMawRadius.Items := FRealFieldAndGlobalVariablesNames;
  comboMawBottom.Items := FRealFieldAndGlobalVariablesNames;
  comboMawInitialHead.Items := FRealFieldAndGlobalVariablesNames;
  comboMawConductanceEquation.Items.AddStrings(FRealFieldAndGlobalVariablesNames);

  frameMawWellScreens.Grid.BeginUpdate;
  try
    frameMawWellScreens.Grid.Cells[Ord(mwscScreenTop), 0] := StrScreenTop;
    frameMawWellScreens.Grid.Cells[Ord(mwscScreenBottom), 0] := StrScreenBottom;
    frameMawWellScreens.Grid.Cells[Ord(mwscSkinK), 0] := StrSkinHydraulicCondu;
    frameMawWellScreens.Grid.Cells[Ord(mwscSkinRadius), 0] := StrSkinRadius;
  //  TMawWellScreenColumns = (mwscScreenTop, mwscScreenBottom, mwscSkinK, mwscSkinRadius);
    for WellScreenColIndex := Low(TMawWellScreenColumns) to High(TMawWellScreenColumns) do
    begin
      frameMawWellScreens.Grid.Columns[Ord(WellScreenColIndex)].ComboUsed := True;
      frameMawWellScreens.Grid.Columns[Ord(WellScreenColIndex)].Format := rcf4String;
      frameMawWellScreens.Grid.Columns[Ord(WellScreenColIndex)].PickList :=
        FRealFieldAndGlobalVariablesNames;
    end;
  finally
    frameMawWellScreens.Grid.EndUpdate;
  end;

end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForMnw2;
var
  Index: Integer;
begin
  plBoundary.ActivePage := jvspModflowMNW2;
  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := 8;
  AssignColFeatureProperties;
  for Index := Ord(mtcStartTime) to Ord(mtcEndTime) do
  begin
    rdgBoundaryConditions.Columns[Index].ComboUsed := True;
    rdgBoundaryConditions.Columns[Index].Format := rcf4String;
    rdgBoundaryConditions.Columns[Index].PickList := FRealFieldNames;
  end;

  for Index := Ord(mtcPumpingRate) to Ord(mtcLimitingWaterLevel) do
  begin
    rdgBoundaryConditions.Columns[Index].ComboUsed := True;
    rdgBoundaryConditions.Columns[Index].Format := rcf4String;
    rdgBoundaryConditions.Columns[Index].PickList :=
      FRealFieldAndGlobalVariablesNames;
  end;

  rdgBoundaryConditions.Columns[Ord(mtcLimitMethod)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(mtcLimitMethod)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(mtcLimitMethod)].PickList :=
    FIntegerFieldNames;

  for Index := Ord(mtcMinRate) to Ord(mtcMaxRate) do
  begin
    rdgBoundaryConditions.Columns[Index].ComboUsed := True;
    rdgBoundaryConditions.Columns[Index].Format := rcf4String;
    rdgBoundaryConditions.Columns[Index].PickList :=
      FRealFieldAndGlobalVariablesNames;
  end;

  rdgBoundaryConditions.Cells[Ord(mtcStartTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Cells[Ord(mtcEndTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Cells[Ord(mtcPumpingRate), 0] := StrDesiredPumpingRate;
  rdgBoundaryConditions.Cells[Ord(mtcMultiplier), 0] := StrHeadCapacityMultip;
  rdgBoundaryConditions.Cells[Ord(mtcLimitingWaterLevel), 0] :=
    StrLimitingWaterLevel;
  rdgBoundaryConditions.Cells[Ord(mtcLimitMethod), 0] := StrPumpingLimitMethod;
  rdgBoundaryConditions.Cells[Ord(mtcMinRate), 0] := StrDeactivationPumping;
  rdgBoundaryConditions.Cells[Ord(mtcMaxRate), 0] := StrReactivationPumping;

  pcMnw2.ActivePageIndex := 0;

  // Basic tab
  comboMnw2WellId.Items := FStringFieldNames;
  comboMnw2LossType.Items := FStringFieldNames;
  comboSpecifyPump.Items := FIntegerFieldNames;
  comboConstrainPumping.Items := FIntegerFieldNames;
  comboPartialPenetrationFlag.Items := FIntegerFieldNames;
  comboPumpCap.Items := FIntegerFieldNames;

  AssignRealValueAttributesToControls;
  AssignRealFieldNamesToControls;

end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForModflow6Obs;
var
  TypePickList: TStrings;
  Index: Integer;
begin
  plBoundary.ActivePage := jvspModflow6Obs;
  comboModflow6ObsName.Items := FStringFieldNames;
  if frmGoPhast.PhastModel.PestUsed then
  begin
    rdgBoundaryConditions.ColCount := 5;
    AssignColFeatureProperties;
    rdgBoundaryConditions.Cells[Ord(mpocName), 0] := 'Observation name';
    rdgBoundaryConditions.Cells[Ord(mpocType), 0] := 'Observation type';
    rdgBoundaryConditions.Cells[Ord(mpocTime), 0] := 'Observation time';
    rdgBoundaryConditions.Cells[Ord(mpocValue), 0] := 'Observed value';
    rdgBoundaryConditions.Cells[Ord(mpocWeight), 0] := 'Observation weight';
    //  TMf6PestObsColumns = (mpocName, mpocType, mpocTime, mpocValue, mpocWeight);

    rdgBoundaryConditions.Columns[Ord(mpocName)].PickList := FStringFieldNames;
    TypePickList := rdgBoundaryConditions.Columns[Ord(mpocType)].PickList;
    TypePickList.Clear;
    TypePickList.AddStrings(PestObsTypes);
    TypePickList.AddStrings(FStringFieldNames);
    rdgBoundaryConditions.Columns[Ord(mpocTime)].PickList := FRealFieldNames;
    rdgBoundaryConditions.Columns[Ord(mpocValue)].PickList := FRealFieldNames;
    rdgBoundaryConditions.Columns[Ord(mpocWeight)].PickList := FRealFieldNames;

    for Index := 0 to rdgBoundaryConditions.ColCount - 1 do
    begin
      rdgBoundaryConditions.Columns[Index].ComboUsed := True;
    end;

  end
  else
  begin
    rdgBoundaryConditions.Enabled := False;
    rdgBoundaryConditions.Visible := False;
  end;

  comboMultilayer.Items.Assign(FBooleanFieldNames)

//  comboMf6ObsElevation.Items := FRealFieldGlobalsAndDataSetsNames;
//  comboMf6ObsLayer.Items := FIntegerFieldNames
end;


procedure TfrmImportShapefile.InitializeBoundaryControlsForUZF;
var
  Index: Integer;
  StartingConcIndex: Integer;
  ConcIndex: Integer;
  AComp: TMobileChemSpeciesItem;
begin
  rdgBoundaryConditions.BeginUpdate;
  try
    FCombinedObjects := comboJoinObjects.ItemIndex = 1;
    plBoundary.ActivePage := jvspNone;
    rdgBoundaryConditions.Enabled := True;

    if frmGoPhast.PhastModel.Mt3d_UztIsSelected then
    begin
      if frmGoPhast.PhastModel.Mt3d_UztEtIsSelected then
      begin
        rdgBoundaryConditions.ColCount := 6
          + frmGoPhast.PhastModel.MobileComponents.Count * 3;
      end
      else
      begin
        rdgBoundaryConditions.ColCount := 6
          + frmGoPhast.PhastModel.MobileComponents.Count;
      end;
    end
    else
    begin
      rdgBoundaryConditions.ColCount := 6;
    end;
    AssignColFeatureProperties;
    for Index := Ord(ucStartTime) to Ord(ucEndTime) do
    begin
      rdgBoundaryConditions.Columns[Index].ComboUsed := not FCombinedObjects;
      if FCombinedObjects then
      begin
        rdgBoundaryConditions.Columns[Index].Format := rcf4Integer;
      end
      else
      begin
        rdgBoundaryConditions.Columns[Index].Format := rcf4String;
      end;
      rdgBoundaryConditions.Columns[Index].PickList := FRealFieldNames;
    end;
    for Index := Ord(ucInfiltration) to Ord(ucExtinctWaterContent) do
    begin
      rdgBoundaryConditions.Columns[Index].ComboUsed := True;
      rdgBoundaryConditions.Columns[Index].Format := rcf4String;
      rdgBoundaryConditions.Columns[Index].PickList :=
        FRealFieldGlobalsAndDataSetsNames;
    end;

    if frmGoPhast.PhastModel.Mt3d_UztIsSelected then
    begin
      StartingConcIndex := Ord(ucExtinctWaterContent) +1;
      for ConcIndex := 0 to frmGoPhast.PhastModel.MobileComponents.Count - 1 do
      begin
        rdgBoundaryConditions.Columns[StartingConcIndex + ConcIndex].ComboUsed
          := True;
        rdgBoundaryConditions.Columns[StartingConcIndex + ConcIndex].Format
          := rcf4String;
        rdgBoundaryConditions.Columns[StartingConcIndex + ConcIndex].PickList
          := FRealFieldGlobalsAndDataSetsNames;
        rdgBoundaryConditions.Columns[StartingConcIndex + ConcIndex].
          WordWrapCaptions := True;
        AComp := frmGoPhast.PhastModel.MobileComponents[ConcIndex];
        rdgBoundaryConditions.Cells[StartingConcIndex + ConcIndex, 0]
          := Format(StrRechConcentrationS, [AComp.Name]);
      end;
      if frmGoPhast.PhastModel.Mt3d_UztEtIsSelected then
      begin
        Inc(StartingConcIndex, frmGoPhast.PhastModel.MobileComponents.Count);
        for ConcIndex := 0 to frmGoPhast.PhastModel.MobileComponents.Count - 1 do
        begin
          rdgBoundaryConditions.Columns[StartingConcIndex + ConcIndex].ComboUsed
            := True;
          rdgBoundaryConditions.Columns[StartingConcIndex + ConcIndex].Format
            := rcf4String;
          rdgBoundaryConditions.Columns[StartingConcIndex + ConcIndex].PickList
            := FRealFieldGlobalsAndDataSetsNames;
          rdgBoundaryConditions.Columns[StartingConcIndex + ConcIndex].
            WordWrapCaptions := True;
          AComp := frmGoPhast.PhastModel.MobileComponents[ConcIndex];
          rdgBoundaryConditions.Cells[StartingConcIndex + ConcIndex, 0]
            := Format(StrSatETConcentrationS, [AComp.Name]);
        end;
        Inc(StartingConcIndex, frmGoPhast.PhastModel.MobileComponents.Count);
        for ConcIndex := 0 to frmGoPhast.PhastModel.MobileComponents.Count - 1 do
        begin
          rdgBoundaryConditions.Columns[StartingConcIndex + ConcIndex].ComboUsed
            := True;
          rdgBoundaryConditions.Columns[StartingConcIndex + ConcIndex].Format
            := rcf4String;
          rdgBoundaryConditions.Columns[StartingConcIndex + ConcIndex].PickList
            := FRealFieldGlobalsAndDataSetsNames;
          rdgBoundaryConditions.Columns[StartingConcIndex + ConcIndex].
            WordWrapCaptions := True;
          AComp := frmGoPhast.PhastModel.MobileComponents[ConcIndex];
          rdgBoundaryConditions.Cells[StartingConcIndex + ConcIndex, 0]
            := Format(StrUnSatETConcentrationS, [AComp.Name]);
        end;
      end;
    end;

    rdgBoundaryConditions.Cells[Ord(ucStartTime), 0] := StrStartingTime;
    rdgBoundaryConditions.Cells[Ord(ucEndTime), 0] := StrEndingTime;
    rdgBoundaryConditions.Cells[Ord(ucInfiltration), 0] := StrInfiltrationRate;
    rdgBoundaryConditions.Cells[Ord(ucEvapRate), 0] := StrEvapoTranspiration;
    rdgBoundaryConditions.Cells[Ord(ucExtinctDepth), 0] := StrETExtinctionDepth;
    rdgBoundaryConditions.Cells[Ord(ucExtinctWaterContent), 0] :=
      StrETExtinctionWater;
  finally
    rdgBoundaryConditions.EndUpdate;
  end;
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForCFP_Pipe;
begin
  FCombinedObjects := False;
  plBoundary.ActivePage := jvspModflowCFP;
  rdgBoundaryConditions.ColCount := 0;
  rdgBoundaryConditions.Enabled := False;

  comboCfpDiameter.Items := FRealFieldAndGlobalVariablesNames;
  comboCfpTortuosity.Items := FRealFieldAndGlobalVariablesNames;
  comboCfpRoughnessHeight.Items := FRealFieldAndGlobalVariablesNames;
  comboCfpLowerReynolds.Items := FRealFieldAndGlobalVariablesNames;
  comboCfpHigherReynolds.Items := FRealFieldAndGlobalVariablesNames;
  comboCfbConductance.Items := FRealFieldAndGlobalVariablesNames;
  comboCfpPipeElevation.Items := FRealFieldAndGlobalVariablesNames;

  comboCfpSavePipeValues.Items := FBooleanFieldNames;
  comboCfpSaveNodeValues.Items := FBooleanFieldNames;

  if rgElevationCount.ItemIndex <> 1 then
  begin
    rgElevationCount.ItemIndex := 1;
    Beep;
    MessageDlg(StrTheNumberOfZform, mtInformation, [mbOK], 0);
  end;
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForHFB;
begin
  plBoundary.ActivePage := jvspModflowHFB;
  rdgBoundaryConditions.ColCount := 0;
  rdgBoundaryConditions.Enabled := False;
  comboHfbHydCond.Items := FRealFieldAndGlobalVariablesNames;
  comboHfbThickness.Items := FRealFieldAndGlobalVariablesNames;
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForLAKMf6;
var
  Index: Integer;
begin
  plBoundary.ActivePage := jvspLakMf6;

  comboLakeMf6Embeded.Items := FBooleanFieldNames;
  comboHorizontal.Items := FBooleanFieldNames;
  comboVertical.Items := FBooleanFieldNames;

  AssignRealFieldNamesToControls;

  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := Ord(l6cWithdrawal) + 1;
//    + AssociatedConcColumns;
  AssignColFeatureProperties;

  rdgBoundaryConditions.Cells[Ord(l6cStartTim), 0] := StrStartingTime;
  rdgBoundaryConditions.Columns[Ord(l6cStartTim)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(l6cStartTim)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(l6cStartTim)].PickList.Clear;
  frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithStartTimes(
    rdgBoundaryConditions, Ord(l6cStartTim));
  rdgBoundaryConditions.Columns[Ord(l6cStartTim)].
    PickList.AddStrings(FRealFieldAndGlobalVariablesNames);

  rdgBoundaryConditions.Cells[Ord(l6cEndTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Columns[Ord(l6cEndTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(l6cEndTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(l6cEndTime)].PickList.Clear;
  frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithEndTimes(
    rdgBoundaryConditions, Ord(l6cEndTime));
  rdgBoundaryConditions.Columns[Ord(l6cEndTime)].
    PickList.AddStrings(FRealFieldAndGlobalVariablesNames);

  {
  TLakeMf6Columns = (l6cStartTim, l6cEndTime, l6cStatus, l6cStage, l6cRainfall,
    l6cEvaporation, l6cRunoff, l6cInflow, l6cWithdrawal);
  }

  rdgBoundaryConditions.Cells[Ord(l6cStatus), 0] := 'Status';
  rdgBoundaryConditions.Cells[Ord(l6cStage), 0] := 'Stage';
  rdgBoundaryConditions.Cells[Ord(l6cRainfall), 0] := 'Rainfall';
  rdgBoundaryConditions.Cells[Ord(l6cEvaporation), 0] := StrEvaporation;
  rdgBoundaryConditions.Cells[Ord(l6cRunoff), 0] := 'Runoff';
  rdgBoundaryConditions.Cells[Ord(l6cInflow), 0] := 'Inflow';
  rdgBoundaryConditions.Cells[Ord(l6cWithdrawal), 0] := StrWithdrawal;

  rdgBoundaryConditions.Columns[Ord(l6cStatus)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(l6cStatus)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(l6cStatus)].PickList.Clear;
  rdgBoundaryConditions.Columns[Ord(l6cStatus)].PickList.Add('Active');
  rdgBoundaryConditions.Columns[Ord(l6cStatus)].PickList.Add('Inactive');
  rdgBoundaryConditions.Columns[Ord(l6cStatus)].PickList.Add('Constant');
//  rdgBoundaryConditions.Columns[l6cStatus].LimitToList := True;

  for Index := Ord(l6cStage) to Ord(l6cWithdrawal) do
  begin
//    rdgBoundaryConditions.Columns[Index].WordWrapCaptions := True;
    rdgBoundaryConditions.Columns[Index].ComboUsed := True;
    rdgBoundaryConditions.Columns[Index].Format := rcf4String;
    rdgBoundaryConditions.Columns[Index].PickList :=
      FRealFieldAndGlobalVariablesNames;
  end;

end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForLAK;
var
//  TimeIndex: Integer;
//  StressPeriod: TModflowStressPeriod;
  Index: Integer;
begin
  plBoundary.ActivePage := jvspModflowLAK;

  comboLakeID.Items := FIntegerFieldNames;
  comboCenterLake.Items := FIntegerFieldNames;

  AssignRealFieldNamesToControls;

  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := Ord(lcWithdrawl) + 1
    + AssociatedConcColumns;
  AssignColFeatureProperties;

  rdgBoundaryConditions.Columns[Ord(lcStartTime)].WordWrapCaptions := True;
  rdgBoundaryConditions.Cells[Ord(lcStartTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Columns[Ord(lcStartTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(lcStartTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(lcStartTime)].PickList.Clear;
  frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithStartTimes(
    rdgBoundaryConditions, Ord(lcStartTime));

//  for TimeIndex := 0 to frmGoPhast.PhastModel.ModflowStressPeriods.Count - 1 do
//  begin
//    StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods.Items[TimeIndex];
//    rdgBoundaryConditions.Columns[Ord(lcStartTime)].
//      PickList.Add(FloatToStr(StressPeriod.StartTime));
//  end;
  rdgBoundaryConditions.Columns[Ord(lcStartTime)].
    PickList.AddStrings(FRealFieldNames);

  rdgBoundaryConditions.Columns[Ord(lcEndTime)].WordWrapCaptions := True;
  rdgBoundaryConditions.Cells[Ord(lcEndTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Columns[Ord(lcEndTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(lcEndTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(lcEndTime)].PickList.Clear;
  frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithEndTimes(
    rdgBoundaryConditions, Ord(lcEndTime));
//  for TimeIndex := 0 to frmGoPhast.PhastModel.ModflowStressPeriods.Count - 1 do
//  begin
//    StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods.Items[TimeIndex];
//    rdgBoundaryConditions.Columns[Ord(lcEndTime)].
//      PickList.Add(FloatToStr(StressPeriod.EndTime));
//  end;
  rdgBoundaryConditions.Columns[Ord(lcEndTime)].
    PickList.AddStrings(FRealFieldNames);

  rdgBoundaryConditions.Cells[Ord(lcMinStage), 0] := StrMinimumStage;
  rdgBoundaryConditions.Cells[Ord(lcMaxStage), 0] := StrMaximumStage;
  rdgBoundaryConditions.Cells[Ord(lcPrecip), 0] := StrPrecipitation;
  rdgBoundaryConditions.Cells[Ord(lcEvap), 0] := StrEvaporation;
  rdgBoundaryConditions.Cells[Ord(lcRunoff), 0] := StrOverlandRunoff;
  rdgBoundaryConditions.Cells[Ord(lcWithdrawl), 0] := StrWithdrawal;

  for Index := Ord(lcMinStage) to Ord(lcWithdrawl) do
  begin
    rdgBoundaryConditions.Columns[Index].WordWrapCaptions := True;
    rdgBoundaryConditions.Columns[Index].ComboUsed := True;
    rdgBoundaryConditions.Columns[Index].Format := rcf4String;
    rdgBoundaryConditions.Columns[Index].PickList :=
      FRealFieldAndGlobalVariablesNames;
  end;

  InitializeColumnsForMt3dConc(Ord(lcConcentration));
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForSTR;
var
  StageCalculated: Boolean;
begin
  plBoundary.ActivePage := jvspModflowSTR;

  comboStrSegmentNumber.Items := FIntegerFieldNames;
  comboStrParameterName.Items := FStringFieldNames;

  rdgBoundaryConditions.Enabled := True;

  StageCalculated := frmGoPhast.PhastModel.ModflowPackages.StrPackage.CalculateStage;
  if StageCalculated then
  begin
    rdgBoundaryConditions.ColCount := Ord(High(TStrColumn)) + 1;
  end
  else
  begin
    rdgBoundaryConditions.ColCount := Ord(strcBedTop) + 1;
  end;
  AssignColFeatureProperties;

  rdgBoundaryConditions.Cells[Ord(strcStartTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Columns[Ord(strcStartTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(strcStartTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(strcStartTime)].PickList.Clear;

  frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithStartTimes(
    rdgBoundaryConditions, Ord(strcStartTime));
  rdgBoundaryConditions.Columns[Ord(strcStartTime)].
    PickList.AddStrings(FRealFieldAndGlobalVariablesNames);

  rdgBoundaryConditions.Cells[Ord(strcEndTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Columns[Ord(strcEndTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(strcEndTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(strcEndTime)].PickList.Clear;
  frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithEndTimes(
    rdgBoundaryConditions, Ord(strcEndTime));
  rdgBoundaryConditions.Columns[Ord(strcEndTime)].
    PickList.AddStrings(FRealFieldAndGlobalVariablesNames);

//  rdgBoundaryConditions.Cells[Ord(strcParameterName), 0] := StrParameterName;
//  rdgBoundaryConditions.Columns[Ord(strcParameterName)].ComboUsed := True;
//  rdgBoundaryConditions.Columns[Ord(strcParameterName)].Format := rcf4String;
//  rdgBoundaryConditions.Columns[Ord(strcParameterName)].PickList := FStringFieldNames;

  rdgBoundaryConditions.Cells[Ord(strcOutflow), 0] := 'Outflow Segment (ITRIB)';
  rdgBoundaryConditions.Columns[Ord(strcOutflow)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(strcOutflow)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(strcOutflow)].PickList := FIntegerFieldNames;

  rdgBoundaryConditions.Cells[Ord(strcDiversion), 0] := 'Diversion Segment (IUPSEG)';
  rdgBoundaryConditions.Columns[Ord(strcDiversion)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(strcDiversion)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(strcDiversion)].PickList := FIntegerFieldNames;

  rdgBoundaryConditions.Cells[Ord(strcFlow), 0] := StrFLOW;
  rdgBoundaryConditions.Columns[Ord(strcFlow)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(strcFlow)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(strcFlow)].PickList :=
    FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Cells[Ord(strcHead), 0] := 'Head';
  rdgBoundaryConditions.Columns[Ord(strcHead)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(strcHead)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(strcHead)].PickList :=
    FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Cells[Ord(strcConductance), 0] := 'Conductance';
  rdgBoundaryConditions.Columns[Ord(strcConductance)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(strcConductance)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(strcConductance)].PickList :=
    FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Cells[Ord(strcBedBottom), 0] := 'Streambed Bottom';
  rdgBoundaryConditions.Columns[Ord(strcBedBottom)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(strcBedBottom)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(strcBedBottom)].PickList :=
    FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Cells[Ord(strcBedTop), 0] := 'Streambed Top';
  rdgBoundaryConditions.Columns[Ord(strcBedTop)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(strcBedTop)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(strcBedTop)].PickList :=
    FRealFieldAndGlobalVariablesNames;

  if StageCalculated then
  begin
    rdgBoundaryConditions.Cells[Ord(strcWidth), 0] := 'Streambed Width';
    rdgBoundaryConditions.Columns[Ord(strcWidth)].ComboUsed := True;
    rdgBoundaryConditions.Columns[Ord(strcWidth)].Format := rcf4String;
    rdgBoundaryConditions.Columns[Ord(strcWidth)].PickList :=
      FRealFieldAndGlobalVariablesNames;

    rdgBoundaryConditions.Cells[Ord(strcSlope), 0] := 'Streambed Slope';
    rdgBoundaryConditions.Columns[Ord(strcSlope)].ComboUsed := True;
    rdgBoundaryConditions.Columns[Ord(strcSlope)].Format := rcf4String;
    rdgBoundaryConditions.Columns[Ord(strcSlope)].PickList :=
      FRealFieldAndGlobalVariablesNames;

    rdgBoundaryConditions.Cells[Ord(strcRoughness), 0] := 'Streambed Roughness';
    rdgBoundaryConditions.Columns[Ord(strcRoughness)].ComboUsed := True;
    rdgBoundaryConditions.Columns[Ord(strcRoughness)].Format := rcf4String;
    rdgBoundaryConditions.Columns[Ord(strcRoughness)].PickList :=
      FRealFieldAndGlobalVariablesNames;
  end;

//  TStrColumn = (strcStartTime, strcEndTime, strcOutflow, strcDiversion,
//    strcFlow, strcHead, strcConductance, strcBedBottom, strcBedTop)


end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForSFR;
var
  TimeIndex: Integer;
  StressPeriod: TModflowStressPeriod;
begin
  plBoundary.ActivePage := jvspModflowSFR;

  comboSfrSegmentNumber.Items := FIntegerFieldNames;

  AssignRealValueAttributesToControls;

  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := Ord(High(TSfrColumns)) + 1;
  AssignColFeatureProperties;

  rdgBoundaryConditions.Cells[Ord(scStartTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Columns[Ord(scStartTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scStartTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scStartTime)].PickList.Clear;
  for TimeIndex := 0 to frmGoPhast.PhastModel.ModflowStressPeriods.Count - 1 do
  begin
    StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods.Items[TimeIndex];
    rdgBoundaryConditions.Columns[Ord(scStartTime)].
      PickList.Add(FloatToStr(StressPeriod.StartTime));
  end;
  rdgBoundaryConditions.Columns[Ord(scStartTime)].
    PickList.AddStrings(FRealFieldAndGlobalVariablesNames);

  rdgBoundaryConditions.Cells[Ord(scEndTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Columns[Ord(scEndTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scEndTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scEndTime)].PickList.Clear;
  for TimeIndex := 0 to frmGoPhast.PhastModel.ModflowStressPeriods.Count - 1 do
  begin
    StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods.Items[TimeIndex];
    rdgBoundaryConditions.Columns[Ord(scEndTime)].
      PickList.Add(FloatToStr(StressPeriod.EndTime));
  end;
  rdgBoundaryConditions.Columns[Ord(scEndTime)].
    PickList.AddStrings(FRealFieldAndGlobalVariablesNames);

  rdgBoundaryConditions.Cells[Ord(scIcalc), 0] := StrICALC;
  rdgBoundaryConditions.Columns[Ord(scIcalc)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scIcalc)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scIcalc)].PickList := FIntegerFieldNames;

  rdgBoundaryConditions.Cells[Ord(scFlow), 0] := StrFLOW;
  rdgBoundaryConditions.Columns[Ord(scFlow)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scFlow)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scFlow)].PickList :=
    FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Cells[Ord(scOutflowSegment), 0] := StrOutflowSegments;
  rdgBoundaryConditions.Columns[Ord(scOutflowSegment)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scOutflowSegment)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scOutflowSegment)].PickList :=
    FIntegerFieldNames;

  rdgBoundaryConditions.Cells[Ord(scDiversionSegment), 0] :=
    StrDiversionSegments;
  rdgBoundaryConditions.Columns[Ord(scDiversionSegment)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scDiversionSegment)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scDiversionSegment)].PickList :=
    FIntegerFieldNames;

  rdgBoundaryConditions.Cells[Ord(scIprior), 0] := StrIPRIOR;
  rdgBoundaryConditions.Columns[Ord(scIprior)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scIprior)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scIprior)].PickList := FIntegerFieldNames;

  rdgBoundaryConditions.Cells[Ord(scPtsw), 0] := StrPTSW;
  rdgBoundaryConditions.Columns[Ord(scPtsw)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scPtsw)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scPtsw)].PickList :=
    FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Cells[Ord(scEtsw), 0] := StrETSW;
  rdgBoundaryConditions.Columns[Ord(scEtsw)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scEtsw)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scEtsw)].PickList :=
    FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Cells[Ord(scRunoff), 0] := StrRUNOFF;
  rdgBoundaryConditions.Columns[Ord(scRunoff)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scRunoff)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scRunoff)].PickList :=
    FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Cells[Ord(scRoughCh), 0] := StrROUGHCH;
  rdgBoundaryConditions.Columns[Ord(scRoughCh)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scRoughCh)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scRoughCh)].PickList :=
    FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Cells[Ord(scRoughBk), 0] := StrROUGHBK;
  rdgBoundaryConditions.Columns[Ord(scRoughBk)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scRoughBk)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scRoughBk)].PickList :=
    FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Cells[Ord(scCdpth), 0] := StrCDPTH;
  rdgBoundaryConditions.Columns[Ord(scCdpth)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scCdpth)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scCdpth)].PickList :=
    FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Cells[Ord(scFdpth), 0] := StrFDPTH;
  rdgBoundaryConditions.Columns[Ord(scFdpth)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scFdpth)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scFdpth)].PickList :=
    FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Cells[Ord(scAwdth), 0] := StrAWDTH;
  rdgBoundaryConditions.Columns[Ord(scAwdth)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scAwdth)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scAwdth)].PickList :=
    FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Cells[Ord(edBwdth), 0] := StrBWDTH;
  rdgBoundaryConditions.Columns[Ord(edBwdth)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(edBwdth)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(edBwdth)].PickList :=
    FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Cells[Ord(scHcond1), 0] := StrHCOND1;
  rdgBoundaryConditions.Columns[Ord(scHcond1)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scHcond1)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scHcond1)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scThickM1), 0] := StrTHICKM1;
  rdgBoundaryConditions.Columns[Ord(scThickM1)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scThickM1)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scThickM1)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scElevUp), 0] := StrELEVUP;
  rdgBoundaryConditions.Columns[Ord(scElevUp)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scElevUp)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scElevUp)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scWidth1), 0] := StrWIDTH1;
  rdgBoundaryConditions.Columns[Ord(scWidth1)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scWidth1)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scWidth1)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scDepth1), 0] := StrDEPTH1;
  rdgBoundaryConditions.Columns[Ord(scDepth1)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scDepth1)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scDepth1)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scHcond2), 0] := StrHCOND2;
  rdgBoundaryConditions.Columns[Ord(scHcond2)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scHcond2)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scHcond2)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scThickM2), 0] := StrTHICKM2;
  rdgBoundaryConditions.Columns[Ord(scThickM2)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scThickM2)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scThickM2)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scElevDn), 0] := StrELEVDN;
  rdgBoundaryConditions.Columns[Ord(scElevDn)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scElevDn)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scElevDn)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scWidth2), 0] := StrWIDTH2;
  rdgBoundaryConditions.Columns[Ord(scWidth2)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scWidth2)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scWidth2)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scDepth2), 0] := StrDEPTH2;
  rdgBoundaryConditions.Columns[Ord(scDepth2)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scDepth2)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scDepth2)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scDist1), 0] := StrXCPT1;
  rdgBoundaryConditions.Columns[Ord(scDist1)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scDist1)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scDist1)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scDist2), 0] := StrXCPT2;
  rdgBoundaryConditions.Columns[Ord(scDist2)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scDist2)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scDist2)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scDist3), 0] := StrXCPT3;
  rdgBoundaryConditions.Columns[Ord(scDist3)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scDist3)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scDist3)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scDist4), 0] := StrXCPT4;
  rdgBoundaryConditions.Columns[Ord(scDist4)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scDist4)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scDist4)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scDist5), 0] := StrXCPT5;
  rdgBoundaryConditions.Columns[Ord(scDist5)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scDist5)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scDist5)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scDist6), 0] := StrXCPT6;
  rdgBoundaryConditions.Columns[Ord(scDist6)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scDist6)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scDist6)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scDist7), 0] := StrXCPT7;
  rdgBoundaryConditions.Columns[Ord(scDist7)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scDist7)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scDist7)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scDist8), 0] := StrXCPT8;
  rdgBoundaryConditions.Columns[Ord(scDist8)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scDist8)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scDist8)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scZ1), 0] := StrZCPT1;
  rdgBoundaryConditions.Columns[Ord(scZ1)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scZ1)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scZ1)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scZ2), 0] := StrZCPT2;
  rdgBoundaryConditions.Columns[Ord(scZ2)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scZ2)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scZ2)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scZ3), 0] := StrZCPT3;
  rdgBoundaryConditions.Columns[Ord(scZ3)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scZ3)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scZ3)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scZ4), 0] := StrZCPT4;
  rdgBoundaryConditions.Columns[Ord(scZ4)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scZ4)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scZ4)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scZ5), 0] := StrZCPT5;
  rdgBoundaryConditions.Columns[Ord(scZ5)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scZ5)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scZ5)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scZ6), 0] := StrZCPT6;
  rdgBoundaryConditions.Columns[Ord(scZ6)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scZ6)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scZ6)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scZ7), 0] := StrZCPT7;
  rdgBoundaryConditions.Columns[Ord(scZ7)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scZ7)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scZ7)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scZ8), 0] := StrZCPT8;
  rdgBoundaryConditions.Columns[Ord(scZ8)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scZ8)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scZ8)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForSFR_MF6;
var
  TimeIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  RowIndex: Integer;
begin
  plBoundary.ActivePage := jvspModflowSFR_MF6;
  pgcSfrMf6.ActivePageIndex := 0;

  frameDownstreamSegmentsSfrMf6.Grid.Cells[0,0] := StrDownstreamSegmentN;
  frameDownstreamSegmentsSfrMf6.Grid.Columns[0].PickList := FIntegerFieldNames;

  frameDiversionsSfrMf6.Grid.Cells[0,0] := StrDiversionSegmentNu;
  frameDiversionsSfrMf6.Grid.Cells[1,0] := StrPriorityCprior;
  frameDiversionsSfrMf6.Grid.Columns[0].PickList := FIntegerFieldNames;
  frameDiversionsSfrMf6.Grid.Columns[1].PickList.AddStrings(FStringFieldNames);


  combolSegNum.Items := FIntegerFieldNames;

  AssignRealValueAttributesToControls;

  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := Ord(High(TSfr_Mf6_Column)) + 1;
  AssignColFeatureProperties;

  rdgBoundaryConditions.Cells[Ord(smcStartTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Columns[Ord(smcStartTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(smcStartTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(smcStartTime)].PickList.Clear;
  for TimeIndex := 0 to frmGoPhast.PhastModel.ModflowStressPeriods.Count - 1 do
  begin
    StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods.Items[TimeIndex];
    rdgBoundaryConditions.Columns[Ord(smcStartTime)].
      PickList.Add(FloatToStr(StressPeriod.StartTime));
  end;
  rdgBoundaryConditions.Columns[Ord(smcStartTime)].
    PickList.AddStrings(FRealFieldAndGlobalVariablesNames);

  rdgBoundaryConditions.Cells[Ord(smcEndTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Columns[Ord(smcEndTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(smcEndTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(smcEndTime)].PickList.Clear;
  for TimeIndex := 0 to frmGoPhast.PhastModel.ModflowStressPeriods.Count - 1 do
  begin
    StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods.Items[TimeIndex];
    rdgBoundaryConditions.Columns[Ord(smcEndTime)].
      PickList.Add(FloatToStr(StressPeriod.EndTime));
  end;
  rdgBoundaryConditions.Columns[Ord(smcEndTime)].
    PickList.AddStrings(FRealFieldAndGlobalVariablesNames);

  rdgBoundaryConditions.Cells[Ord(smcStatus), 0] := StrStatusMf6;
  rdgBoundaryConditions.Columns[Ord(smcStatus)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(smcStatus)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(smcStatus)].PickList.Clear;
  rdgBoundaryConditions.Columns[Ord(smcStatus)].PickList.Add(StrInactive);
  rdgBoundaryConditions.Columns[Ord(smcStatus)].PickList.Add(StrActive);
  rdgBoundaryConditions.Columns[Ord(smcStatus)].PickList.Add(StrSpecifiedHead);
  rdgBoundaryConditions.Columns[Ord(smcStatus)].PickList.AddStrings(FStringFieldNames);

  rdgBoundaryConditions.Cells[Ord(smcStage), 0] := StrStageMf6;
  rdgBoundaryConditions.Columns[Ord(smcStage)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(smcStage)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(smcStage)].PickList := FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(smcInflow), 0] := StrInflowMf6L3;
  rdgBoundaryConditions.Columns[Ord(smcInflow)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(smcInflow)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(smcInflow)].PickList := FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(smcRainfall), 0] := StrRainfallMf6L;
  rdgBoundaryConditions.Columns[Ord(smcRainfall)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(smcRainfall)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(smcRainfall)].PickList := FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(smcEvaporation), 0] := StrEvaporationMf6L;
  rdgBoundaryConditions.Columns[Ord(smcEvaporation)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(smcEvaporation)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(smcEvaporation)].PickList := FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(smcRunoff), 0] := StrRunoffMf6L3;
  rdgBoundaryConditions.Columns[Ord(smcRunoff)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(smcRunoff)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(smcRunoff)].PickList := FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(smcRoughness), 0] := StrRoughnessMf6;
  rdgBoundaryConditions.Columns[Ord(smcRoughness)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(smcRoughness)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(smcRoughness)].PickList := FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(smcUpstreamFraction), 0] := StrUpstreamFractionMf6;
  rdgBoundaryConditions.Columns[Ord(smcUpstreamFraction)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(smcUpstreamFraction)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(smcUpstreamFraction)].PickList := FRealFieldGlobalsAndDataSetsNames;

  comboGrd.Text := '0.01';
  for RowIndex := 1 to rdgBoundaryConditions.RowCount - 1 do
  begin
   rdgBoundaryConditions.Cells[Ord(smcRoughness), RowIndex] := '0.03';
  end;

end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForHOB;
begin
  plBoundary.ActivePage := jvspModflowHOB;
  comboHeadObservationNames.Items := FStringFieldNames;
  rdgBoundaryConditions.Enabled := True;

  AssignColFeatureProperties;
  rdgBoundaryConditions.Columns[0].ComboUsed := True;
  rdgBoundaryConditions.Columns[0].Format := rcf4String;
  rdgBoundaryConditions.Columns[0].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[1].ComboUsed := True;
  rdgBoundaryConditions.Columns[1].Format := rcf4String;
  rdgBoundaryConditions.Columns[1].PickList := FRealFieldNames;

  rdgBoundaryConditions.Columns[2].ComboUsed := True;
  rdgBoundaryConditions.Columns[2].Format := rcf4String;
  rdgBoundaryConditions.Columns[2].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[3].ComboUsed := True;
  rdgBoundaryConditions.Columns[3].Format := rcf4String;
  rdgBoundaryConditions.Columns[3].PickList := FIntegerFieldNames;

  rdgBoundaryConditions.Cells[0, 0] := StrTime;
  rdgBoundaryConditions.Cells[1, 0] := StrObservedHead;
  rdgBoundaryConditions.Cells[2, 0] := StrStatistic;
  rdgBoundaryConditions.Cells[3, 0] := StrStatFlag;
end;

procedure TfrmImportShapefile.ImportModflowResPackage(
  AScreenObject: TScreenObject);
var
  Index: Integer;
  UseRow: Boolean;
  Item: TResItem;
  AValue: Extended;
  AFormula: string;
  ItemName: string;
  ValueItem: TValueArrayItem;
  Count: Integer;
  StartingConcIndex: Integer;
  ConcBoundary: TMt3dmsConcBoundary;
  ConcItem: TMt3dmsConcItem;
  Dummy: Boolean;
begin
  AScreenObject.CreateResBoundary;
  StartingConcIndex := Ord(rescConcentration);
  ConcBoundary := CreateConcBoundary(AScreenObject);
  Count := 0;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(rescStartingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(rescEndingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[2, Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[3, Index + 1] <> '');
    if UseRow then
    begin
      if Count < AScreenObject.ModflowResBoundary.Values.Count then
      begin
        Item := AScreenObject.ModflowResBoundary.Values.Items[Count] as TResItem;
      end
      else
      begin
        Item := AScreenObject.ModflowResBoundary.Values.Add as TResItem;
      end;
      Inc(Count);
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[
        Ord(rescStartingTime), Index + 1], Dummy);
      Item.StartTime := AValue;
      AValue := GetRealValueFromText(rdgBoundaryConditions.
        Cells[Ord(rescEndingTime), Index + 1], Dummy);
      Item.EndTime := AValue;

      ConcItem := CreateConcItem(ConcBoundary, Index, Item);

      if FCombinedObjects then
      begin
        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(rescStartingHead), Index + 1], Dummy);
        ItemName := Format(StrRESStartHeadd, [Index]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          Item.StartHead := rsObjectImportedValuesR + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);

        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(rescEndingHead), Index + 1], Dummy);
        ItemName := Format(StrRESEndingHeadd, [Index]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          Item.EndHead := rsObjectImportedValuesR + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);

        ImportConcItemForCombinedShapes(ConcItem, StartingConcIndex, Index,
          AScreenObject);
      end
      else
      begin
        AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
          Ord(rescStartingHead), Index + 1]);
        Item.StartHead := AFormula;
        AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
          Ord(rescEndingHead), Index + 1]);
        Item.EndHead := AFormula;

        ImportConcItemForSeparateShapes(Index, ConcItem,
          StartingConcIndex);
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.ImportModflowEtsPackage(
  AScreenObject: TScreenObject; Packages: TModflowPackages);
var
  Index: Integer;
  UseRow: Boolean;
  Item: TEvtItem;
  AValue: Extended;
  FractionIndex: Integer;
  SurfDepthItem: TEtsSurfDepthItem;
  FractItem: TStringValueItem;
  LayerItem: TEvtLayerItem;
  AnIntFormula: string;
  AFormula: string;
  Boundary: TModflowParamBoundary;
  ParamItem: TModflowParamItem;
  ParameterName: string;
  AnItem: TCustomModflowBoundaryItem;
  Param: TModflowTransientListParameter;
  Count: integer;
  ItemName: string;
  ValueItem: TValueArrayItem;
  AnIntValue: Integer;
  NonConcColumnCount: Integer;
  StartingConcIndex: Integer;
  ConcBoundary: TMt3dmsConcBoundary;
  ConcItem: TMt3dmsConcItem;
  Dummy: Boolean;
begin
  if Packages.EtsPackage.TimeVaryingLayers then
  begin
    NonConcColumnCount :=
      (Packages.EtsPackage.SegmentCount - 1) * 2 + Ord(etscDepth) + 2;
  end
  else
  begin
    NonConcColumnCount :=
      (Packages.EtsPackage.SegmentCount - 1) * 2 + Ord(etscDepth) + 1;
  end;
  StartingConcIndex := NonConcColumnCount;
  AScreenObject.CreateEtsBoundary;

  Boundary := AScreenObject.ModflowEtsBoundary;
  ParamItem := nil;
  ConcBoundary := CreateConcBoundary(AScreenObject);

  Count := 0;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(etscStartingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(etscEndingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(etscRate), Index + 1] <> '');
    if UseRow then
    begin
      GetTransientParameter(Param, ParameterName, Ord(etscParameterName), Index+1);
      if (ParameterName <> '') and (Param = nil) then
      begin
        Continue;
      end;
      GetNewOrExistingBoundaryItem(AnItem, ParameterName, Param, ParamItem,
        Boundary, Count);
      Inc(Count);
      Item := AnItem as TEvtItem;

//      Item := AScreenObject.ModflowEtsBoundary.Values.Add as TEvtItem;
      AValue := GetRealValueFromText(rdgBoundaryConditions.
        Cells[Ord(etscStartingTime), Index + 1], Dummy);
      Item.StartTime := AValue;
      AValue := GetRealValueFromText(rdgBoundaryConditions.
        Cells[Ord(etscEndingTime), Index + 1], Dummy);
      Item.EndTime := AValue;

      ConcItem := CreateConcItem(ConcBoundary, Index, Item);

      if FCombinedObjects then
      begin
        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(etscRate), Index + 1], Dummy);
        ItemName := Format(StrETSEvapotranspirati, [Index]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          Item.EvapotranspirationRate := rsObjectImportedValuesR
            + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);

        ImportConcItemForCombinedShapes(ConcItem, StartingConcIndex, Index,
          AScreenObject);
      end
      else
      begin
        AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
          Ord(etscRate), Index + 1]);
        Item.EvapotranspirationRate := AFormula;

        ImportConcItemForSeparateShapes(Index, ConcItem,
          StartingConcIndex);
      end;
    end;
  end;
  Count := 0;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(etscStartingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(etscEndingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(etscSurface), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(etscDepth), Index + 1] <> '');
    for FractionIndex := 0 to Packages.EtsPackage.SegmentCount - 2 do
    begin
      UseRow := UseRow and (rdgBoundaryConditions.
        Cells[Ord(etscDepth)+1 + (FractionIndex * 2), Index + 1] <> '')
        and (rdgBoundaryConditions.
        Cells[Ord(etscDepth)+2 + (FractionIndex * 2), Index + 1] <> '');
    end;
    if UseRow then
    begin
      if Count < AScreenObject.ModflowEtsBoundary.
        EtsSurfDepthCollection.Count then
      begin
        SurfDepthItem := AScreenObject.ModflowEtsBoundary.
          EtsSurfDepthCollection.Items[Count] as TEtsSurfDepthItem;
      end
      else
      begin
        SurfDepthItem := AScreenObject.ModflowEtsBoundary.
          EtsSurfDepthCollection.Add as TEtsSurfDepthItem;
      end;
      Inc(Count);
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[
        Ord(etscStartingTime), Index + 1], Dummy);
      SurfDepthItem.StartTime := AValue;
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[
        Ord(etscEndingTime), Index + 1], Dummy);
      SurfDepthItem.EndTime := AValue;

      if FCombinedObjects then
      begin
        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(etscSurface), Index + 1], Dummy);
        ItemName := Format(StrETSEvapotranspiratiS, [Index]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          SurfDepthItem.EvapotranspirationSurface := rsObjectImportedValuesR
            + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);

        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(etscDepth), Index + 1], Dummy);
        ItemName := Format(StrETSEvapotranspiratiD, [Index]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          SurfDepthItem.EvapotranspirationDepth := rsObjectImportedValuesR
            + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);

        for FractionIndex := 0 to Packages.EtsPackage.SegmentCount - 2 do
        begin
          if FractionIndex < SurfDepthItem.DepthFractions.Count then
          begin
            FractItem := SurfDepthItem.DepthFractions.Items[FractionIndex]
              as TStringValueItem;
          end
          else
          begin
            FractItem := SurfDepthItem.DepthFractions.Add as TStringValueItem;
          end;
          AValue := GetRealValueFromText(rdgBoundaryConditions.
            Cells[Ord(etscDepth)+1 + (FractionIndex * 2), Index + 1], Dummy);
          ItemName := Format(StrETSDepthFraction0, [Index, FractionIndex]);
          ValueItem := AScreenObject.ImportedValues.ValueItemByName(
            ItemName);
          if ValueItem = nil then
          begin
            ValueItem := AScreenObject.
              ImportedValues.Add as TValueArrayItem;
            ValueItem.Name := ItemName;
            ValueItem.Values.DataType := rdtDouble;
            ValueItem.Values.Count := 0;
            FractItem.Value := rsObjectImportedValuesR + '("' + ItemName + '")';
          end;
          ValueItem.Values.Add(AValue);

          if FractionIndex < SurfDepthItem.EtFractions.Count then
          begin
            FractItem := SurfDepthItem.EtFractions.Items[FractionIndex]
              as TStringValueItem;
          end
          else
          begin
            FractItem := SurfDepthItem.EtFractions.Add as TStringValueItem;
          end;
          AValue := GetRealValueFromText(rdgBoundaryConditions.
            Cells[Ord(etscDepth)+2 + (FractionIndex * 2), Index + 1], Dummy);
          ItemName := Format(StrETSEtFraction0d, [Index, FractionIndex+1]);
          ValueItem := AScreenObject.ImportedValues.ValueItemByName(
            ItemName);
          if ValueItem = nil then
          begin
            ValueItem := AScreenObject.
              ImportedValues.Add as TValueArrayItem;
            ValueItem.Name := ItemName;
            ValueItem.Values.DataType := rdtDouble;
            ValueItem.Values.Count := 0;
            FractItem.Value := rsObjectImportedValuesR + '("' + ItemName + '")';
          end;
          ValueItem.Values.Add(AValue);
        end;
      end
      else
      begin
        AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
          Ord(etscSurface), Index + 1]);
        SurfDepthItem.EvapotranspirationSurface := AFormula;
        AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
          Ord(etscDepth), Index + 1]);
        SurfDepthItem.EvapotranspirationDepth := AFormula;
        for FractionIndex := 0 to Packages.EtsPackage.SegmentCount - 2 do
        begin
          FractItem := SurfDepthItem.DepthFractions.Add as TStringValueItem;
          AFormula := GetRealFormulaFromText(rdgBoundaryConditions.
            Cells[Ord(etscDepth)+1 + (FractionIndex * 2), Index + 1]);
          FractItem.Value := AFormula;

          FractItem := SurfDepthItem.EtFractions.Add as TStringValueItem;
          AFormula := GetRealFormulaFromText(rdgBoundaryConditions.
            Cells[Ord(etscDepth)+2 + (FractionIndex * 2), Index + 1]);
          FractItem.Value := AFormula;
        end;
      end;
    end;
  end;
  if Packages.EtsPackage.TimeVaryingLayers then
  begin
    Count := 0;
    for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
    begin
      UseRow := (rdgBoundaryConditions.Cells[Ord(etscStartingTime), Index + 1] <> '')
        and (rdgBoundaryConditions.Cells[Ord(etscEndingTime), Index + 1] <> '')
        and (rdgBoundaryConditions.
        Cells[rdgBoundaryConditions.ColCount - 1, Index + 1] <> '');
      if UseRow then
      begin
        if Count < AScreenObject.ModflowEtsBoundary.
          EvapotranspirationLayers.Count then
        begin
          LayerItem := AScreenObject.ModflowEtsBoundary.
            EvapotranspirationLayers.Items[Count] as TEvtLayerItem;
        end
        else
        begin
          LayerItem := AScreenObject.ModflowEtsBoundary.
            EvapotranspirationLayers.Add as TEvtLayerItem;
        end;
        Inc(Count);
        AValue := GetRealValueFromText(
          rdgBoundaryConditions.Cells[Ord(etscStartingTime), Index + 1], Dummy);
        LayerItem.StartTime := AValue;
        AValue := GetRealValueFromText(
          rdgBoundaryConditions.Cells[Ord(etscEndingTime), Index + 1], Dummy);
        LayerItem.EndTime := AValue;

        if FCombinedObjects then
        begin
          AnIntValue := GetIntegerValueFromText(rdgBoundaryConditions.
            Cells[Ord(etscRate), Index + 1]);
          ItemName := Format(StrETSLayerd, [Index]);
          ValueItem := AScreenObject.ImportedValues.ValueItemByName(
            ItemName);
          if ValueItem = nil then
          begin
            ValueItem := AScreenObject.
              ImportedValues.Add as TValueArrayItem;
            ValueItem.Name := ItemName;
            ValueItem.Values.DataType := rdtInteger;
            ValueItem.Values.Count := 0;
            LayerItem.EvapotranspirationLayer :=
              rsObjectImportedValuesI + '("' + ItemName + '")';
          end;
          ValueItem.Values.Add(AnIntValue);
        end
        else
        begin
          AnIntFormula := GetIntegerFormulaFromText(rdgBoundaryConditions.
            Cells[rdgBoundaryConditions.ColCount - 1, Index + 1]);
          LayerItem.EvapotranspirationLayer := AnIntFormula;
        end;
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForRES(
  Packages: TModflowPackages);
var
  Index: Integer;
begin
  FCombinedObjects := comboJoinObjects.ItemIndex = 1;
  plBoundary.ActivePage := jvspNone;
  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := Ord(rescEndingHead) + 1
    + AssociatedConcColumns;
  AssignColFeatureProperties;

  InitializeColumnsForMt3dConc(Ord(rescConcentration));

  for Index := Ord(rescStartingTime) to Ord(rescEndingTime) do
  begin
    rdgBoundaryConditions.Columns[Index].ComboUsed := not FCombinedObjects;
    if FCombinedObjects then
    begin
      rdgBoundaryConditions.Columns[Index].Format := rcf4Integer;
    end
    else
    begin
      rdgBoundaryConditions.Columns[Index].Format := rcf4String;
    end;
    rdgBoundaryConditions.Columns[Index].PickList := FRealFieldNames;
  end;
  for Index := Ord(rescStartingHead) to Ord(rescEndingHead) do
  begin
    rdgBoundaryConditions.Columns[Index].ComboUsed := True;
    rdgBoundaryConditions.Columns[Index].Format := rcf4String;
    rdgBoundaryConditions.Columns[Index].PickList :=
      FRealFieldAndGlobalVariablesNames;
  end;
  rdgBoundaryConditions.Cells[Ord(rescStartingTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Cells[Ord(rescEndingTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Cells[Ord(rescStartingHead), 0] := StrStartingHead;
  rdgBoundaryConditions.Cells[Ord(rescEndingHead), 0] := StrEndingHead;
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForETS(
  Packages: TModflowPackages);
var
  Index: Integer;
  NonConcColumnCount: Integer;
  StartingConcIndex: Integer;
begin
  FCombinedObjects := comboJoinObjects.ItemIndex = 1;
  plBoundary.ActivePage := jvspNone;
  rdgBoundaryConditions.Enabled := True;
  if Packages.EtsPackage.TimeVaryingLayers then
  begin
    NonConcColumnCount :=
      (Packages.EtsPackage.SegmentCount - 1) * 2 + Ord(etscDepth) + 2;
  end
  else
  begin
    NonConcColumnCount :=
      (Packages.EtsPackage.SegmentCount - 1) * 2 + Ord(etscDepth) + 1;
  end;
  rdgBoundaryConditions.ColCount := NonConcColumnCount + AssociatedConcColumns;
  StartingConcIndex := NonConcColumnCount;
  AssignColFeatureProperties;
  for Index := Ord(etscStartingTime) to Ord(etscEndingTime) do
  begin
    rdgBoundaryConditions.Columns[Index].ComboUsed := not FCombinedObjects;
    if FCombinedObjects then
    begin
      rdgBoundaryConditions.Columns[Index].Format := rcf4Integer;
    end
    else
    begin
      rdgBoundaryConditions.Columns[Index].Format := rcf4String;
    end;
    rdgBoundaryConditions.Columns[Index].PickList := FRealFieldNames;
  end;

  rdgBoundaryConditions.Columns[Ord(etscParameterName)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(etscParameterName)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(etscParameterName)].PickList :=
    FStringFieldNames;
  AddParameterNamesToPickList(ptETS, Ord(etscParameterName));

  for Index := Ord(etscRate) to NonConcColumnCount - 1 do
  begin
    rdgBoundaryConditions.Columns[Index].ComboUsed := True;
    rdgBoundaryConditions.Columns[Index].Format := rcf4String;
    rdgBoundaryConditions.Columns[Index].PickList :=
      FRealFieldGlobalsAndDataSetsNames;
  end;
  if Packages.EtsPackage.TimeVaryingLayers then
  begin
    rdgBoundaryConditions.Columns[NonConcColumnCount - 1].PickList
      := FIntegerFieldNames;
  end;
  rdgBoundaryConditions.Cells[Ord(etscStartingTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Cells[Ord(etscEndingTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Cells[Ord(etscParameterName), 0] := StrParameterName;
  rdgBoundaryConditions.Cells[Ord(etscRate), 0] := StrEvapoTranspirationR;
  rdgBoundaryConditions.Cells[Ord(etscSurface), 0] := StrEvapoTranspirationS;
  rdgBoundaryConditions.Cells[Ord(etscDepth), 0] := StrEvapoTranspirationDe;
  for Index := 0 to (Packages.EtsPackage.SegmentCount - 2) do
  begin
    rdgBoundaryConditions.Cells[Ord(etscDepth)+1 + (Index * 2), 0] :=
      Format(StrFractionalDepthD, [Index + 1]);
    rdgBoundaryConditions.Cells[Ord(etscDepth)+2 + (Index * 2), 0] :=
      Format(StrFractionalRateD, [Index + 1]);
  end;
  if Packages.EtsPackage.TimeVaryingLayers then
  begin
    rdgBoundaryConditions.Cells[NonConcColumnCount - 1, 0] :=
      StrEvapoTranspirationL;
  end;
  InitializeColumnsForMt3dConc(StartingConcIndex);

end;

procedure TfrmImportShapefile.ImportModflowEvtPackage(Packages: TModflowPackages;
  AScreenObject: TScreenObject);
var
  Index: Integer;
  UseRow: Boolean;
  Item: TEvtItem;
  AValue: Extended;
  SurfDepthItem: TEvtSurfDepthItem;
  LayerItem: TEvtLayerItem;
  AnIntFormula: string;
  AFormula: string;
  ParamItem: TModflowParamItem;
  Boundary: TModflowParamBoundary;
  ParameterName: string;
  Param: TModflowTransientListParameter;
  AnItem: TCustomModflowBoundaryItem;
  Count: Integer;
  ItemName: string;
  ValueItem: TValueArrayItem;
  AnIntValue: Integer;
  StartingConcIndex: Integer;
  ConcBoundary: TMt3dmsConcBoundary;
  ConcItem: TMt3dmsConcItem;
  Dummy: Boolean;
begin
  if Packages.EvtPackage.TimeVaryingLayers then
  begin
    StartingConcIndex := Ord(evtcLayer) + 1;
  end
  else
  begin
    StartingConcIndex := Ord(evtcDepth) + 1;
  end;
  AScreenObject.CreateEvtBoundary;
  Boundary := AScreenObject.ModflowEvtBoundary;
  ParamItem := nil;
  ConcBoundary := CreateConcBoundary(AScreenObject);
  Count := 0;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(evtcStartingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(evtcEndingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(evtRate), Index + 1] <> '');
    if UseRow then
    begin
      GetTransientParameter(Param, ParameterName, Ord(evtcParameterName), Index+1);
      if (ParameterName <> '') and (Param = nil) then
      begin
        Continue;
      end;
      GetNewOrExistingBoundaryItem(AnItem, ParameterName, Param, ParamItem,
        Boundary, Count);
      Inc(Count);
      Item := AnItem as TEvtItem;

      AValue := GetRealValueFromText(rdgBoundaryConditions.
        Cells[Ord(evtcStartingTime), Index + 1], Dummy);
      Item.StartTime := AValue;
      AValue := GetRealValueFromText(rdgBoundaryConditions.
        Cells[Ord(evtcEndingTime), Index + 1], Dummy);
      Item.EndTime := AValue;

      ConcItem := CreateConcItem(ConcBoundary, Index, Item);

      if FCombinedObjects then
      begin
        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(evtRate), Index + 1], Dummy);
        ItemName := Format(StrEVTEvapotranspirati, [Index]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          Item.EvapotranspirationRate := rsObjectImportedValuesR
            + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);

        ImportConcItemForCombinedShapes(ConcItem, StartingConcIndex, Index,
          AScreenObject);
      end
      else
      begin
        AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
          Ord(evtRate), Index + 1]);
        Item.EvapotranspirationRate := AFormula;

        ImportConcItemForSeparateShapes(Index, ConcItem,
          StartingConcIndex);
      end;
    end;
  end;
  Count := 0;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(evtcStartingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(evtcEndingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(evtcSurface), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(evtcDepth), Index + 1] <> '');
    if UseRow then
    begin
      if Count < AScreenObject.ModflowEvtBoundary.
        EvtSurfDepthCollection.Count then
      begin
        SurfDepthItem := AScreenObject.ModflowEvtBoundary.
          EvtSurfDepthCollection.Items[Count] as TEvtSurfDepthItem;
      end
      else
      begin
        SurfDepthItem := AScreenObject.ModflowEvtBoundary.
          EvtSurfDepthCollection.Add as TEvtSurfDepthItem;
      end;
      Inc(Count);
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[
        Ord(evtcStartingTime), Index + 1], Dummy);
      SurfDepthItem.StartTime := AValue;
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[
        Ord(evtcEndingTime), Index + 1], Dummy);
      SurfDepthItem.EndTime := AValue;
      if FCombinedObjects then
      begin
        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(evtcSurface), Index + 1], Dummy);
        ItemName := Format(StrEVTEvapotranspiratiS, [Index]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          SurfDepthItem.EvapotranspirationSurface :=
            rsObjectImportedValuesR + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);

        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(evtcDepth), Index + 1], Dummy);
        ItemName := Format(StrEVTEvapotranspiratiD, [Index]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          SurfDepthItem.EvapotranspirationDepth :=
            rsObjectImportedValuesR + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);
      end
      else
      begin
        AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
          Ord(evtcSurface), Index + 1]);
        SurfDepthItem.EvapotranspirationSurface := AFormula;
        AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
          Ord(evtcDepth), Index + 1]);
        SurfDepthItem.EvapotranspirationDepth := AFormula;
      end;
    end;
  end;
  if rdgBoundaryConditions.ColCount = Ord(evtcLayer)+1 then
  begin
    Count := 0;
    for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
    begin
      UseRow := (rdgBoundaryConditions.Cells[Ord(evtcStartingTime), Index + 1] <> '')
        and (rdgBoundaryConditions.Cells[Ord(evtcEndingTime), Index + 1] <> '')
        and (rdgBoundaryConditions.Cells[Ord(evtcLayer), Index + 1] <> '');
      if UseRow then
      begin
        if Count < AScreenObject.ModflowEvtBoundary.
          EvapotranspirationLayers.Count then
        begin
          LayerItem := AScreenObject.ModflowEvtBoundary.
            EvapotranspirationLayers.Items[Count] as TEvtLayerItem;
        end
        else
        begin
          LayerItem := AScreenObject.ModflowEvtBoundary.
            EvapotranspirationLayers.Add as TEvtLayerItem;
        end;
        Inc(Count);
        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(evtcStartingTime), Index + 1], Dummy);
        LayerItem.StartTime := AValue;
        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(evtcEndingTime), Index + 1], Dummy);
        LayerItem.EndTime := AValue;

        if FCombinedObjects then
        begin
          AnIntValue := GetIntegerValueFromText(rdgBoundaryConditions.
            Cells[Ord(evtcLayer), Index + 1]);
          ItemName := Format(StrEVTLayerd, [Index]);
          ValueItem := AScreenObject.ImportedValues.ValueItemByName(
            ItemName);
          if ValueItem = nil then
          begin
            ValueItem := AScreenObject.
              ImportedValues.Add as TValueArrayItem;
            ValueItem.Name := ItemName;
            ValueItem.Values.DataType := rdtInteger;
            ValueItem.Values.Count := 0;
            LayerItem.EvapotranspirationLayer :=
              rsObjectImportedValuesI + '("' + ItemName + '")';
          end;
          ValueItem.Values.Add(AnIntValue);
        end
        else
        begin
          AnIntFormula := GetIntegerFormulaFromText(rdgBoundaryConditions.
            Cells[Ord(evtcLayer), Index + 1]);
          LayerItem.EvapotranspirationLayer := AnIntFormula;
        end;
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForEVT(
  Packages: TModflowPackages);
var
  Index: Integer;
  StartingConcIndex: Integer;
begin
  FCombinedObjects := comboJoinObjects.ItemIndex = 1;
  plBoundary.ActivePage := jvspNone;
  rdgBoundaryConditions.Enabled := True;
  if Packages.EvtPackage.TimeVaryingLayers then
  begin
    StartingConcIndex := Ord(evtcLayer) + 1;
  end
  else
  begin
    StartingConcIndex := Ord(evtcDepth) + 1;
  end;
  rdgBoundaryConditions.ColCount := StartingConcIndex + AssociatedConcColumns;
  AssignColFeatureProperties;
  rdgBoundaryConditions.Columns[Ord(evtcStartingTime)].ComboUsed :=
    not FCombinedObjects;
  rdgBoundaryConditions.Columns[Ord(evtcEndingTime)].ComboUsed :=
    not FCombinedObjects;
  rdgBoundaryConditions.Columns[Ord(evtcParameterName)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(evtRate)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(evtcSurface)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(evtcDepth)].ComboUsed := True;
  if Packages.EvtPackage.TimeVaryingLayers then
  begin
    rdgBoundaryConditions.Columns[Ord(evtcLayer)].ComboUsed := True;
  end;

   InitializeColumnsForMt3dConc(StartingConcIndex);

  if FCombinedObjects then
  begin
    rdgBoundaryConditions.Columns[Ord(evtcStartingTime)].Format := rcf4Integer;
    rdgBoundaryConditions.Columns[Ord(evtcEndingTime)].Format := rcf4Integer;
  end
  else
  begin
    rdgBoundaryConditions.Columns[Ord(evtcStartingTime)].Format := rcf4String;
    rdgBoundaryConditions.Columns[Ord(evtcEndingTime)].Format := rcf4String;
  end;
  rdgBoundaryConditions.Columns[Ord(evtcParameterName)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(evtRate)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(evtcSurface)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(evtcDepth)].Format := rcf4String;
  if Packages.EvtPackage.TimeVaryingLayers then
  begin
    rdgBoundaryConditions.Columns[Ord(evtcLayer)].Format := rcf4String;
  end;
  for Index := Ord(evtcStartingTime) to Ord(evtcEndingTime) do
  begin
    rdgBoundaryConditions.Columns[Index].PickList := FRealFieldNames;
  end;
  rdgBoundaryConditions.Columns[Ord(evtcParameterName)].PickList :=
    FStringFieldNames;
  AddParameterNamesToPickList(ptEVT, Ord(evtcParameterName));
  for Index := Ord(evtRate) to StartingConcIndex - 1 do
  begin
    rdgBoundaryConditions.Columns[Index].PickList :=
      FRealFieldGlobalsAndDataSetsNames;
  end;
  if Packages.EvtPackage.TimeVaryingLayers then
  begin
    rdgBoundaryConditions.Columns[Ord(evtcLayer)].PickList := FIntegerFieldNames;
  end;
  rdgBoundaryConditions.Cells[Ord(evtcStartingTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Cells[Ord(evtcEndingTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Cells[Ord(evtcParameterName), 0] := StrParameterName;
  rdgBoundaryConditions.Cells[Ord(evtRate), 0] := StrEvapoTranspirationR;
  rdgBoundaryConditions.Cells[Ord(evtcSurface), 0] := StrEvapoTranspirationS;
  rdgBoundaryConditions.Cells[Ord(evtcDepth), 0] := StrEvapoTranspirationDe;
  if Packages.EvtPackage.TimeVaryingLayers then
  begin
    rdgBoundaryConditions.Cells[Ord(evtcLayer), 0] := StrEvapoTranspirationL;
  end;
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForFootprintWell;
begin
  plBoundary.ActivePage := jvspFootprintWell;
  rdgBoundaryConditions.Enabled := False;
  seBoundaryTimeCount.Enabled := False;
  AssignRealFieldNamesToControls;
//  AssignRealValueAttributesToControls;
end;

procedure TfrmImportShapefile.ImportModflowRchPackage(Packages: TModflowPackages;
  AScreenObject: TScreenObject);
var
  Index: Integer;
  UseRow: Boolean;
  Item: TRchItem;
  AValue: Extended;
  LayerItem: TRchLayerItem;
  AnIntFormula: string;
  AFormula: string;
  ParameterName: string;
  Param: TModflowTransientListParameter;
  AnItem: TCustomModflowBoundaryItem;
  ParamItem: TModflowParamItem;
  Boundary: TModflowParamBoundary;
  Count: Integer;
  ItemName: string;
  ValueItem: TValueArrayItem;
  AnIntValue: Integer;
  StartingConcIndex: Integer;
  ConcBoundary: TMt3dmsConcBoundary;
  ConcItem: TMt3dmsConcItem;
  Dummy: Boolean;
begin
  if Packages.RchPackage.TimeVaryingLayers then
  begin
    StartingConcIndex := Ord(rcConcentration);
  end
  else
  begin
    StartingConcIndex := Ord(rcConcentration)-1;
  end;
  AScreenObject.CreateRchBoundary;
  Boundary := AScreenObject.ModflowRchBoundary;
  ConcBoundary := CreateConcBoundary(AScreenObject);
  ParamItem := nil;
  Count := 0;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(rcStartTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(rcEndTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(rcFluxRate), Index + 1] <> '');
    if UseRow then
    begin
      GetTransientParameter(Param, ParameterName, Ord(rcParameterName), Index+1);
      if (ParameterName <> '') and (Param = nil) then
      begin
        Continue;
      end;

      GetNewOrExistingBoundaryItem(AnItem, ParameterName, Param, ParamItem,
        Boundary, Count);
      Inc(Count);
      Item := AnItem as TRchItem;

      AValue := GetRealValueFromText(rdgBoundaryConditions.
        Cells[Ord(rcStartTime), Index + 1], Dummy);
      Item.StartTime := AValue;
      AValue := GetRealValueFromText(rdgBoundaryConditions.
        Cells[Ord(rcEndTime), Index + 1], Dummy);
      Item.EndTime := AValue;

      ConcItem := CreateConcItem(ConcBoundary, Index, Item);

      if FCombinedObjects then
      begin
        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(rcFluxRate), Index + 1], Dummy);
        ItemName := Format('RCH_RechargeRate%d', [Index]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          Item.RechargeRate := rsObjectImportedValuesR + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);

        ImportConcItemForCombinedShapes(ConcItem, StartingConcIndex, Index,
          AScreenObject);
      end
      else
      begin
        AFormula := GetRealFormulaFromText(rdgBoundaryConditions.
          Cells[Ord(rcFluxRate), Index + 1]);
        Item.RechargeRate := AFormula;

        ImportConcItemForSeparateShapes(Index, ConcItem,
          StartingConcIndex);
      end;
    end;
  end;
  if rdgBoundaryConditions.ColCount = Ord(rcLayer)+1 then
  begin
    Count := 0;
    for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
    begin
      UseRow := (rdgBoundaryConditions.Cells[Ord(rcStartTime), Index + 1] <> '')
        and (rdgBoundaryConditions.Cells[Ord(rcEndTime), Index + 1] <> '')
        and (rdgBoundaryConditions.Cells[Ord(rcLayer), Index + 1] <> '');
      if UseRow then
      begin
        if Count < AScreenObject.ModflowRchBoundary.
          RechargeLayers.Count then
        begin
          LayerItem := AScreenObject.ModflowRchBoundary.
            RechargeLayers.Items[Count] as TRchLayerItem;
        end
        else
        begin
          LayerItem := AScreenObject.ModflowRchBoundary.
            RechargeLayers.Add as TRchLayerItem;
        end;
        Inc(Count);
        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(rcStartTime), Index + 1], Dummy);
        LayerItem.StartTime := AValue;
        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(rcEndTime), Index + 1], Dummy);
        LayerItem.EndTime := AValue;
        if FCombinedObjects then
        begin
          AnIntValue := GetIntegerValueFromText(rdgBoundaryConditions.
            Cells[Ord(rcLayer), Index + 1]);
          ItemName := Format(StrRCHLayerd, [Index]);
          ValueItem := AScreenObject.ImportedValues.ValueItemByName(
            ItemName);
          if ValueItem = nil then
          begin
            ValueItem := AScreenObject.
              ImportedValues.Add as TValueArrayItem;
            ValueItem.Name := ItemName;
            ValueItem.Values.DataType := rdtInteger;
            ValueItem.Values.Count := 0;
            LayerItem.RechargeLayer := rsObjectImportedValuesI
              + '("' + ItemName + '")';
          end;
          ValueItem.Values.Add(AnIntValue);
        end
        else
        begin
          AnIntFormula := GetIntegerFormulaFromText(rdgBoundaryConditions.
            Cells[Ord(rcLayer), Index + 1]);
          LayerItem.RechargeLayer := AnIntFormula;
        end;
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForRCH(
  Packages: TModflowPackages);
var
  ConcColum: integer;
begin
  FCombinedObjects := comboJoinObjects.ItemIndex = 1;
  plBoundary.ActivePage := jvspNone;
  rdgBoundaryConditions.Enabled := True;
  if Packages.RchPackage.TimeVaryingLayers then
  begin
    rdgBoundaryConditions.ColCount := Ord(rcLayer)+1+AssociatedConcColumns;
    ConcColum := Ord(rcConcentration);
  end
  else
  begin
    rdgBoundaryConditions.ColCount := Ord(rcFluxRate)+1+AssociatedConcColumns;
    ConcColum := Ord(rcConcentration)-1;
  end;
  AssignColFeatureProperties;
  rdgBoundaryConditions.Columns[Ord(rcStartTime)].ComboUsed :=
    not FCombinedObjects;
  rdgBoundaryConditions.Columns[Ord(rcEndTime)].ComboUsed :=
    not FCombinedObjects;
  rdgBoundaryConditions.Columns[Ord(rcParameterName)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(rcFluxRate)].ComboUsed := True;
  if Packages.RchPackage.TimeVaryingLayers then
  begin
    rdgBoundaryConditions.Columns[Ord(rcLayer)].ComboUsed := True;
  end;

  InitializeColumnsForMt3dConc(ConcColum);
  if FCombinedObjects then
  begin
    rdgBoundaryConditions.Columns[Ord(rcStartTime)].Format := rcf4Integer;
    rdgBoundaryConditions.Columns[Ord(rcEndTime)].Format := rcf4Integer;
  end
  else
  begin
    rdgBoundaryConditions.Columns[Ord(rcStartTime)].Format := rcf4String;
    rdgBoundaryConditions.Columns[Ord(rcEndTime)].Format := rcf4String;
  end;
  rdgBoundaryConditions.Columns[Ord(rcParameterName)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(rcFluxRate)].Format := rcf4String;
  if Packages.RchPackage.TimeVaryingLayers then
  begin
    rdgBoundaryConditions.Columns[Ord(rcLayer)].Format := rcf4String;
  end;
  rdgBoundaryConditions.Columns[Ord(rcStartTime)].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(rcEndTime)].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(rcParameterName)].PickList :=
    FStringFieldNames;
  AddParameterNamesToPickList(ptRCH, Ord(rcParameterName));
  rdgBoundaryConditions.Columns[Ord(rcFluxRate)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;
  if Packages.RchPackage.TimeVaryingLayers then
  begin
    rdgBoundaryConditions.Columns[Ord(rcLayer)].PickList := FIntegerFieldNames;
  end;
  rdgBoundaryConditions.Cells[Ord(rcStartTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Cells[Ord(rcEndTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Cells[Ord(rcParameterName), 0] := StrParameterName;
  rdgBoundaryConditions.Cells[Ord(rcFluxRate), 0] := StrRechargeRate;
  if Packages.RchPackage.TimeVaryingLayers then
  begin
    rdgBoundaryConditions.Cells[Ord(rcLayer), 0] := StrRechargeLayer;
  end;
end;

procedure TfrmImportShapefile.ImportModflowDrtPackage(
  AScreenObject: TScreenObject);
var
  AValue: Extended;
  Index: Integer;
  UseRow: Boolean;
  Item: TDrtItem;
  AnInteger: Integer;
  AFormula: string;
  Boundary: TModflowParamBoundary;
  ParamItem: TModflowParamItem;
  Param: TModflowTransientListParameter;
  ParameterName: string;
  AnItem: TCustomModflowBoundaryItem;
  Count: Integer;
  ItemName: string;
  ValueItem: TValueArrayItem;
  StartingConcIndex: Integer;
  ConcBoundary: TMt3dmsConcBoundary;
  ConcItem: TMt3dmsConcItem;
  Dummy: Boolean;
begin
  AScreenObject.CreateDrtBoundary;
  AScreenObject.ModflowDrtBoundary.FormulaInterpretation :=
    GetFormulaInterpretation(comboFormulaInterpDRT);

  case comboDrainReturnLocationMethod.ItemIndex of
    0:
      begin
        AScreenObject.ModflowDrtBoundary.DrainReturn.ReturnChoice := rtNone;
      end;
    1:
      begin
        AScreenObject.ModflowDrtBoundary.DrainReturn.ReturnChoice := rtLocation;
      end;
    2:
      begin
        AScreenObject.ModflowDrtBoundary.DrainReturn.ReturnChoice := rtCell;
      end;
    else Assert(False);
  end;
  StartingConcIndex := Ord(dtcConcentration);
  ConcBoundary := CreateConcBoundary(AScreenObject);
  case AScreenObject.ModflowDrtBoundary.DrainReturn.ReturnChoice of
    rtNone: ; // dp nothing.
    rtObject: Assert(False);
    rtLocation:
      begin
        AValue := GetRealValueFromText(rdeDrtX.Text, Dummy);
        AScreenObject.ModflowDrtBoundary.DrainReturn.ReturnLocation.X := AValue;
        AValue := GetRealValueFromText(rdeDrtY.Text, Dummy);
        AScreenObject.ModflowDrtBoundary.DrainReturn.ReturnLocation.Y := AValue;
        AValue := GetRealValueFromText(rdeDrtZ.Text, Dummy);
        AScreenObject.ModflowDrtBoundary.DrainReturn.ReturnLocation.Z := AValue;
      end;
    rtCell:
      begin
        AnInteger := GetIntegerValueFromText(rdeDrtCol.Text);
        AScreenObject.ModflowDrtBoundary.DrainReturn.ReturnCell.Col := AnInteger;
        AnInteger := GetIntegerValueFromText(rdeDrtRow.Text);
        AScreenObject.ModflowDrtBoundary.DrainReturn.ReturnCell.Row := AnInteger;
        AnInteger := GetIntegerValueFromText(rdeDrtLay.Text);
        AScreenObject.ModflowDrtBoundary.DrainReturn.ReturnCell.Lay := AnInteger;
      end;
    else Assert(False);
  end;

  Boundary := AScreenObject.ModflowDrtBoundary;
  ParamItem := nil;
  Count := 0;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(dtcStartingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(dtcEndingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(dtcElevation), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(dtcConductance), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(dtcReturnFraction), Index + 1] <> '');
    if UseRow then
    begin
      GetTransientParameter(Param, ParameterName, Ord(dtcParameterName), Index+1);
      if (ParameterName <> '') and (Param = nil) then
      begin
        Continue;
      end;

      GetNewOrExistingBoundaryItem(AnItem, ParameterName, Param, ParamItem,
        Boundary, Count);
      Inc(Count);
      Item := AnItem as TDrtItem;

      AValue := GetRealValueFromText(rdgBoundaryConditions.
        Cells[Ord(dtcStartingTime), Index + 1], Dummy);
      Item.StartTime := AValue;
      AValue := GetRealValueFromText(rdgBoundaryConditions.
        Cells[Ord(dtcEndingTime), Index + 1], Dummy);
      Item.EndTime := AValue;

      ConcItem := CreateConcItem(ConcBoundary, Index, Item);
      if FCombinedObjects then
      begin
        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(dtcElevation), Index + 1], Dummy);
        ItemName := Format(StrDRTElevationd, [Index]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          Item.Elevation := rsObjectImportedValuesR + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);

        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(dtcConductance), Index + 1], Dummy);
        ItemName := Format(StrDRTConductanced, [Index]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          Item.Conductance := rsObjectImportedValuesR + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);

        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(dtcReturnFraction), Index + 1], Dummy);
        ItemName := Format(StrDRTReturnFractiond, [Index]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          Item.ReturnFraction := rsObjectImportedValuesR + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);
        ImportConcItemForCombinedShapes(ConcItem, StartingConcIndex, Index,
          AScreenObject);
      end
      else
      begin
        AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
          Ord(dtcElevation), Index + 1]);
        Item.Elevation := AFormula;
        AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
          Ord(dtcConductance), Index + 1]);
        Item.Conductance := AFormula;
        AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
          Ord(dtcReturnFraction), Index + 1]);
        Item.ReturnFraction := AFormula;

        ImportConcItemForSeparateShapes(Index, ConcItem,
          StartingConcIndex);
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.ImportModflowDrnPackage(
  AScreenObject: TScreenObject);
var
  AValue: Extended;
  ItemIndex: Integer;
  UseRow: Boolean;
  Item: TDrnItem;
  AFormula: string;
  Boundary: TModflowParamBoundary;
  ParamItem: TModflowParamItem;
  Param: TModflowTransientListParameter;
  ParameterName: string;
  AnItem: TCustomModflowBoundaryItem;
  Count: Integer;
  ItemName: string;
  ValueItem: TValueArrayItem;
  StartingConcIndex: Integer;
  ConcBoundary: TMt3dmsConcBoundary;
  ConcItem: TMt3dmsConcItem;
  Dummy: Boolean;
begin
  AScreenObject.CreateDrnBoundary;
  Boundary := AScreenObject.ModflowDrnBoundary;
  ParamItem := nil;
  AScreenObject.ModflowDrnBoundary.FormulaInterpretation :=
    GetFormulaInterpretation(comboFormulaInterp);
  StartingConcIndex := Ord(dcConcentration);
  ConcBoundary := CreateConcBoundary(AScreenObject);
  Count := 0;
  for ItemIndex := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(dcStartingTime), ItemIndex + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(dcEndingTime), ItemIndex + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(dcElevation), ItemIndex + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(dcConductance), ItemIndex + 1] <> '');
    if UseRow then
    begin
      GetTransientParameter(Param, ParameterName, Ord(dcParameterName),
        ItemIndex+1);
      if (ParameterName <> '') and (Param = nil) then
      begin
        Continue;
      end;
      GetNewOrExistingBoundaryItem(AnItem, ParameterName, Param, ParamItem,
        Boundary, Count);
      Inc(Count);
      Item := AnItem as TDrnItem;
//      Item := AScreenObject.ModflowDrnBoundary.Values.Add as TDrnItem;
      AValue := GetRealValueFromText(rdgBoundaryConditions.
        Cells[Ord(dcStartingTime), ItemIndex + 1], Dummy);
      Item.StartTime := AValue;
      AValue := GetRealValueFromText(rdgBoundaryConditions.
        Cells[Ord(dcEndingTime), ItemIndex + 1], Dummy);
      Item.EndTime := AValue;

      ConcItem := CreateConcItem(ConcBoundary, ItemIndex, Item);

      if FCombinedObjects then
      begin
        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(dcElevation), ItemIndex + 1], Dummy);
        ItemName := Format(StrDRNElevationd, [ItemIndex]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          Item.Elevation := rsObjectImportedValuesR + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);

        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(dcConductance), ItemIndex + 1], Dummy);
        ItemName := Format(StrDRNConductanced, [ItemIndex]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          Item.Conductance := rsObjectImportedValuesR + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);

        ImportConcItemForCombinedShapes(ConcItem, StartingConcIndex, ItemIndex,
          AScreenObject);
      end
      else
      begin
        AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
          Ord(dcElevation), ItemIndex + 1]);
        Item.Elevation := AFormula;
        AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
          Ord(dcConductance), ItemIndex + 1]);
        Item.Conductance := AFormula;

        ImportConcItemForSeparateShapes(ItemIndex, ConcItem,
          StartingConcIndex);
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForDRT;
begin
  FCombinedObjects := comboJoinObjects.ItemIndex = 1;
  plBoundary.ActivePage := jvspModflowDRT;
  lblConductanceInterpretationDRT.Caption := StrConductanceInterpre;
  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := Ord(dtcReturnFraction)+1
    +AssociatedConcColumns;
  AssignColFeatureProperties;

  rdgBoundaryConditions.Columns[Ord(dtcStartingTime)].ComboUsed :=
    not FCombinedObjects;
  rdgBoundaryConditions.Columns[Ord(dtcEndingTime)].ComboUsed :=
    not FCombinedObjects;
  rdgBoundaryConditions.Columns[Ord(dtcParameterName)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(dtcElevation)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(dtcConductance)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(dtcReturnFraction)].ComboUsed := True;

  InitializeColumnsForMt3dConc(Ord(dtcConcentration));

  if FCombinedObjects then
  begin
    rdgBoundaryConditions.Columns[Ord(dtcStartingTime)].Format := rcf4Integer;
    rdgBoundaryConditions.Columns[Ord(dtcEndingTime)].Format := rcf4Integer;
  end
  else
  begin
    rdgBoundaryConditions.Columns[Ord(dtcStartingTime)].Format := rcf4String;
    rdgBoundaryConditions.Columns[Ord(dtcEndingTime)].Format := rcf4String;
  end;
  rdgBoundaryConditions.Columns[Ord(dtcParameterName)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(dtcElevation)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(dtcConductance)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(dtcReturnFraction)].Format := rcf4String;

  rdgBoundaryConditions.Columns[Ord(dtcStartingTime)].PickList :=
    FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(dtcEndingTime)].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(dtcParameterName)].PickList :=
    FStringFieldNames;
  AddParameterNamesToPickList(ptDRT, Ord(dtcParameterName));
  rdgBoundaryConditions.Columns[Ord(dtcElevation)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;
  rdgBoundaryConditions.Columns[Ord(dtcConductance)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;
  rdgBoundaryConditions.Columns[Ord(dtcReturnFraction)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(dtcStartingTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Cells[Ord(dtcEndingTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Cells[Ord(dtcParameterName), 0] := StrParameterName;
  rdgBoundaryConditions.Cells[Ord(dtcElevation), 0] := StrElevation;
  rdgBoundaryConditions.Cells[Ord(dtcConductance), 0] := StrConductance;
  FConductanceCol := Ord(dtcConductance);
  rdgBoundaryConditions.Cells[Ord(dtcReturnFraction), 0] := StrReturnFraction;

  while comboFormulaInterpDRT.Items.Count > 3 do
  begin
    comboFormulaInterpDRT.Items.Delete(comboFormulaInterpDRT.Items.Count-1);
  end;
  comboFormulaInterpDRT.Items.AddStrings(FStringFieldNames);

  AssignRealFieldNamesToControls;

  rdeDrtCol.Items := FIntegerFieldNames;
  rdeDrtRow.Items := FIntegerFieldNames;
  rdeDrtLay.Items := FIntegerFieldNames;

  comboDrainReturnLocationMethodChange(nil);
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForDRN;
begin
  FCombinedObjects := comboJoinObjects.ItemIndex = 1;
  plBoundary.ActivePage := jvspConductanceInterp;
  lblConductanceInterpretation.Caption := StrConductanceInterpre;
  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := Ord(dcConductance) + 1 +
    AssociatedConcColumns;
  AssignColFeatureProperties;
  rdgBoundaryConditions.Columns[Ord(dcStartingTime)].ComboUsed :=
    not FCombinedObjects;
  rdgBoundaryConditions.Columns[Ord(dcEndingTime)].ComboUsed :=
    not FCombinedObjects;
  rdgBoundaryConditions.Columns[Ord(dcParameterName)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(dcElevation)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(dcConductance)].ComboUsed := True;

  InitializeColumnsForMt3dConc(Ord(dcConcentration));

  if FCombinedObjects then
  begin
    rdgBoundaryConditions.Columns[Ord(dcStartingTime)].Format := rcf4Integer;
    rdgBoundaryConditions.Columns[Ord(dcEndingTime)].Format := rcf4Integer;
  end
  else
  begin
    rdgBoundaryConditions.Columns[Ord(dcStartingTime)].Format := rcf4String;
    rdgBoundaryConditions.Columns[Ord(dcEndingTime)].Format := rcf4String;
  end;
  rdgBoundaryConditions.Columns[Ord(dcParameterName)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(dcElevation)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(dcConductance)].Format := rcf4String;

  rdgBoundaryConditions.Columns[Ord(dcStartingTime)].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(dcEndingTime)].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(dcParameterName)].PickList :=
    FStringFieldNames;
  AddParameterNamesToPickList(ptDRN, Ord(dcParameterName));
  rdgBoundaryConditions.Columns[Ord(dcElevation)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;
  rdgBoundaryConditions.Columns[Ord(dcConductance)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(dcStartingTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Cells[Ord(dcEndingTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Cells[Ord(dcParameterName), 0] := StrParameterName;
  rdgBoundaryConditions.Cells[Ord(dcElevation), 0] := StrDrainElevation;
  rdgBoundaryConditions.Cells[Ord(dcConductance), 0] := StrConductance;
  FConductanceCol := Ord(dcConductance);

  while comboFormulaInterp.Items.Count > 3 do
  begin
    comboFormulaInterp.Items.Delete(comboFormulaInterp.Items.Count-1);
  end;
  comboFormulaInterp.Items.AddStrings(FStringFieldNames);
end;

procedure TfrmImportShapefile.ImportModflowRivPackage(
  AScreenObject: TScreenObject);
var
  AValue: Extended;
  Item: TRivItem;
  UseRow: Boolean;
  Index: Integer;
  AFormula: string;
  Param: TModflowTransientListParameter;
  ParameterName: string;
  AnItem: TCustomModflowBoundaryItem;
  ParamItem: TModflowParamItem;
  Boundary: TModflowParamBoundary;
  Count: Integer;
  ItemName: string;
  ValueItem: TValueArrayItem;
  StartingConcIndex: Integer;
  ConcBoundary: TMt3dmsConcBoundary;
  ConcItem: TMt3dmsConcItem;
  Dummy: Boolean;
begin
  AScreenObject.CreateRivBoundary;
  Boundary := AScreenObject.ModflowRivBoundary;
  ParamItem := nil;
  AScreenObject.ModflowRivBoundary.FormulaInterpretation :=
    GetFormulaInterpretation(comboFormulaInterp);
  StartingConcIndex := Ord(rivcConcentration);
  ConcBoundary := CreateConcBoundary(AScreenObject);
  Count := 0;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(rivcStartingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(rivcEndingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(rivcBottom), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(rivcStage), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(rivcConductance), Index + 1] <> '');
    if UseRow then
    begin
      GetTransientParameter(Param, ParameterName, Ord(rivcParameterName), Index+1);
      if (ParameterName <> '') and (Param = nil) then
      begin
        Continue;
      end;

      GetNewOrExistingBoundaryItem(AnItem, ParameterName, Param, ParamItem,
        Boundary, Count);
      Inc(Count);
      Item := AnItem as TRivItem;

//      Item := AScreenObject.ModflowRivBoundary.Values.Add as TRivItem;
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[
        Ord(rivcStartingTime), Index + 1], Dummy);
      Item.StartTime := AValue;
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[
        Ord(rivcEndingTime), Index + 1], Dummy);
      Item.EndTime := AValue;

      ConcItem := CreateConcItem(ConcBoundary, Index, Item);

      if FCombinedObjects then
      begin
        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(rivcBottom), Index + 1], Dummy);
        ItemName := Format(StrRIVBottomd, [Index]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          Item.RiverBottom := rsObjectImportedValuesR + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);

        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(rivcStage), Index + 1], Dummy);
        ItemName := Format(StrRIVStaged, [Index]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          Item.RiverStage := rsObjectImportedValuesR + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);

        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(rivcConductance), Index + 1], Dummy);
        ItemName := Format(StrRIVConductanced, [Index]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          Item.Conductance := rsObjectImportedValuesR + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);

        ImportConcItemForCombinedShapes(ConcItem, StartingConcIndex, Index,
          AScreenObject);
      end
      else
      begin
        AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
          Ord(rivcBottom), Index + 1]);
        Item.RiverBottom := AFormula;
        AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
          Ord(rivcStage), Index + 1]);
        Item.RiverStage := AFormula;
        AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
          Ord(rivcConductance), Index + 1]);
        Item.Conductance := AFormula;

        ImportConcItemForSeparateShapes(Index, ConcItem,
          StartingConcIndex);
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForRIV;
begin
  FCombinedObjects := comboJoinObjects.ItemIndex = 1;
  plBoundary.ActivePage := jvspConductanceInterp;
  lblConductanceInterpretation.Caption := StrConductanceInterpre;
  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := Ord(rivcConductance)+1+AssociatedConcColumns;
  AssignColFeatureProperties;

  rdgBoundaryConditions.Columns[Ord(rivcStartingTime)].ComboUsed :=
    not FCombinedObjects;
  rdgBoundaryConditions.Columns[Ord(rivcEndingTime)].ComboUsed :=
    not FCombinedObjects;
  rdgBoundaryConditions.Columns[Ord(rivcParameterName)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(rivcBottom)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(rivcStage)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(rivcConductance)].ComboUsed := True;

  InitializeColumnsForMt3dConc(Ord(rivcConcentration));

  if FCombinedObjects then
  begin
    rdgBoundaryConditions.Columns[Ord(rivcStartingTime)].Format := rcf4Integer;
    rdgBoundaryConditions.Columns[Ord(rivcEndingTime)].Format := rcf4Integer;
  end
  else
  begin
    rdgBoundaryConditions.Columns[Ord(rivcStartingTime)].Format := rcf4String;
    rdgBoundaryConditions.Columns[Ord(rivcEndingTime)].Format := rcf4String;
  end;
  rdgBoundaryConditions.Columns[Ord(rivcParameterName)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(rivcBottom)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(rivcStage)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(rivcConductance)].Format := rcf4String;

  rdgBoundaryConditions.Columns[Ord(rivcStartingTime)].PickList :=
    FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(rivcEndingTime)].PickList :=
    FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(rivcParameterName)].PickList :=
    FStringFieldNames;
  AddParameterNamesToPickList(ptRIV, Ord(rivcParameterName));
  rdgBoundaryConditions.Columns[Ord(rivcBottom)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;
  rdgBoundaryConditions.Columns[Ord(rivcStage)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;
  rdgBoundaryConditions.Columns[Ord(rivcConductance)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  while comboFormulaInterp.Items.Count > 3 do
  begin
    comboFormulaInterp.Items.Delete(comboFormulaInterp.Items.Count-1);
  end;
  comboFormulaInterp.Items.AddStrings(FStringFieldNames);
  rdgBoundaryConditions.Cells[Ord(rivcStartingTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Cells[Ord(rivcEndingTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Cells[Ord(rivcParameterName), 0] := StrParameterName;
  rdgBoundaryConditions.Cells[Ord(rivcBottom), 0] := StrRiverBottom;
  rdgBoundaryConditions.Cells[Ord(rivcStage), 0] := StrRiverStage;
  rdgBoundaryConditions.Cells[Ord(rivcConductance), 0] := StrConductance;
  FConductanceCol := Ord(rivcConductance);
end;

procedure TfrmImportShapefile.ImportModflowWelBoundary(
  AScreenObject: TScreenObject);
var
  AValue: Extended;
  Item: TWellItem;
  UseRow: Boolean;
  Index: Integer;
  AFormula: string;
  Param: TModflowTransientListParameter;
  ParameterName: string;
  AnItem: TCustomModflowBoundaryItem;
  ParamItem: TModflowParamItem;
  Boundary: TModflowParamBoundary;
  Count: Integer;
  ItemName: string;
  ValueItem: TValueArrayItem;
  StartingConcIndex: Integer;
  ConcBoundary: TMt3dmsConcBoundary;
  ConcItem: TMt3dmsConcItem;
  Dummy: Boolean;
begin
  AScreenObject.CreateWelBoundary;
  Boundary := AScreenObject.ModflowWellBoundary;
  ParamItem := nil;
  AScreenObject.ModflowWellBoundary.FormulaInterpretation :=
    GetFormulaInterpretation(comboFormulaInterp);
  StartingConcIndex := Ord(welcConcentration);
  ConcBoundary := CreateConcBoundary(AScreenObject);
  Count := 0;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(welcStartingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(welcEndingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(welcPumpingRate), Index + 1] <> '');
    if UseRow then
    begin
      GetTransientParameter(Param, ParameterName, Ord(welcParameterName), Index+1);
      if (ParameterName <> '') and (Param = nil) then
      begin
        Continue;
      end;

      GetNewOrExistingBoundaryItem(AnItem, ParameterName, Param, ParamItem,
        Boundary, Count);
      Inc(Count);
      Item := AnItem as TWellItem;

      AValue := GetRealValueFromText(rdgBoundaryConditions.
        Cells[Ord(welcStartingTime), Index + 1], Dummy);
      Item.StartTime := AValue;
      AValue := GetRealValueFromText(rdgBoundaryConditions.
        Cells[Ord(welcEndingTime), Index + 1], Dummy);
      Item.EndTime := AValue;

      ConcItem := CreateConcItem(ConcBoundary, Index, Item);

      if FCombinedObjects then
      begin
        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(welcPumpingRate), Index + 1], Dummy);
        ItemName := Format(StrWELPumpingRated, [Index]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          Item.PumpingRate := rsObjectImportedValuesR + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);

        ImportConcItemForCombinedShapes(ConcItem, StartingConcIndex, Index,
          AScreenObject);
      end
      else
      begin
        AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
          Ord(welcPumpingRate), Index + 1]);
        Item.PumpingRate := AFormula;

        ImportConcItemForSeparateShapes(Index, ConcItem,
          StartingConcIndex);
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForWEL;
begin
  FCombinedObjects := comboJoinObjects.ItemIndex = 1;
  plBoundary.ActivePage := jvspConductanceInterp;
  lblConductanceInterpretation.Caption := StrPumpingRateInterpr;
  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := Ord(welcPumpingRate)+1+AssociatedConcColumns;
  AssignColFeatureProperties;

  rdgBoundaryConditions.Columns[Ord(welcStartingTime)].ComboUsed :=
    not FCombinedObjects;
  rdgBoundaryConditions.Columns[Ord(welcEndingTime)].ComboUsed :=
    not FCombinedObjects;
  rdgBoundaryConditions.Columns[Ord(welcParameterName)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(welcPumpingRate)].ComboUsed := True;

  InitializeColumnsForMt3dConc(Ord(welcConcentration));

  if FCombinedObjects then
  begin
    rdgBoundaryConditions.Columns[Ord(welcStartingTime)].Format := rcf4Integer;
    rdgBoundaryConditions.Columns[Ord(welcEndingTime)].Format := rcf4Integer;
  end
  else
  begin
    rdgBoundaryConditions.Columns[Ord(welcStartingTime)].Format := rcf4String;
    rdgBoundaryConditions.Columns[Ord(welcEndingTime)].Format := rcf4String;
  end;
  rdgBoundaryConditions.Columns[Ord(welcParameterName)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(welcPumpingRate)].Format := rcf4String;

  rdgBoundaryConditions.Columns[Ord(welcStartingTime)].PickList :=
    FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(welcEndingTime)].PickList :=
    FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(welcParameterName)].PickList :=
    FStringFieldNames;
  rdgBoundaryConditions.Columns[Ord(welcPumpingRate)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;
  AddParameterNamesToPickList(ptQ, Ord(welcParameterName));

  rdgBoundaryConditions.Cells[Ord(welcStartingTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Cells[Ord(welcEndingTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Cells[Ord(welcParameterName), 0] := StrParameterName;
  rdgBoundaryConditions.Cells[Ord(welcPumpingRate), 0] := StrPumpingRate;

  while comboFormulaInterp.Items.Count > 3 do
  begin
    comboFormulaInterp.Items.Delete(comboFormulaInterp.Items.Count-1);
  end;
  comboFormulaInterp.Items.AddStrings(FStringFieldNames);
end;

procedure TfrmImportShapefile.ImportModflowGhbBoundary(
  AScreenObject: TScreenObject);
var
  Index: Integer;
  UseRow: Boolean;
  Item: TGhbItem;
  AValue: Extended;
  AFormula: string;
  Param: TModflowTransientListParameter;
  ParameterName: string;
  AnItem: TCustomModflowBoundaryItem;
  ParamItem: TModflowParamItem;
  Boundary: TModflowParamBoundary;
  Count: Integer;
  ItemName: string;
  ValueItem: TValueArrayItem;
  StartingConcIndex: Integer;
  ConcBoundary: TMt3dmsConcBoundary;
  ConcItem: TMt3dmsConcItem;
  Dummy: Boolean;
begin
  AScreenObject.CreateGhbBoundary;
  Boundary := AScreenObject.ModflowGhbBoundary;
  ParamItem := nil;
  AScreenObject.ModflowGhbBoundary.FormulaInterpretation :=
    GetFormulaInterpretation(comboFormulaInterp);
  StartingConcIndex := Ord(ghbcConcentration);
  ConcBoundary := CreateConcBoundary(AScreenObject);
  Count := 0;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(ghbcStartingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(ghbcEndingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(ghbcHead), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(ghbcConductance), Index + 1] <> '');
    if UseRow then
    begin
      GetTransientParameter(Param, ParameterName, Ord(ghbcParameterName), Index+1);
      if (ParameterName <> '') and (Param = nil) then
      begin
        Continue;
      end;
      GetNewOrExistingBoundaryItem(AnItem, ParameterName, Param, ParamItem,
        Boundary, Count);
      Inc(Count);
      Item := AnItem as TGhbItem;

//      Item := AScreenObject.ModflowGhbBoundary.Values.Add as TGhbItem;
      AValue := GetRealValueFromText(rdgBoundaryConditions.
        Cells[Ord(ghbcStartingTime), Index + 1], Dummy);
      Item.StartTime := AValue;
      AValue := GetRealValueFromText(rdgBoundaryConditions.
        Cells[Ord(ghbcEndingTime), Index + 1], Dummy);
      Item.EndTime := AValue;

      ConcItem := CreateConcItem(ConcBoundary, Index, Item);

      if FCombinedObjects then
      begin
        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(ghbcHead), Index + 1], Dummy);
        ItemName := Format(StrGHBBoundaryHeadd, [Index]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          Item.BoundaryHead := rsObjectImportedValuesR + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);

        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(ghbcConductance), Index + 1], Dummy);
        ItemName := Format(StrGHBConductanced, [Index]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          Item.Conductance := rsObjectImportedValuesR + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);

        ImportConcItemForCombinedShapes(ConcItem, StartingConcIndex, Index,
          AScreenObject);
      end
      else
      begin
        AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
          Ord(ghbcHead), Index + 1]);
        Item.BoundaryHead := AFormula;
        AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
          Ord(ghbcConductance), Index + 1]);
        Item.Conductance := AFormula;

        ImportConcItemForSeparateShapes(Index, ConcItem,
          StartingConcIndex);
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForGHB;
begin
  FCombinedObjects := comboJoinObjects.ItemIndex = 1;
  plBoundary.ActivePage := jvspConductanceInterp;
  lblConductanceInterpretation.Caption := StrConductanceInterpre;
  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := Ord(ghbcConductance)+1+AssociatedConcColumns;
  AssignColFeatureProperties;

  rdgBoundaryConditions.Columns[Ord(ghbcStartingTime)].ComboUsed :=
    not FCombinedObjects;
  rdgBoundaryConditions.Columns[Ord(ghbcEndingTime)].ComboUsed :=
    not FCombinedObjects;
  rdgBoundaryConditions.Columns[Ord(ghbcParameterName)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(ghbcHead)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(ghbcConductance)].ComboUsed := True;

  InitializeColumnsForMt3dConc(Ord(ghbcConcentration));

  if FCombinedObjects then
  begin
    rdgBoundaryConditions.Columns[Ord(ghbcStartingTime)].Format := rcf4Integer;
    rdgBoundaryConditions.Columns[Ord(ghbcEndingTime)].Format := rcf4Integer;
  end
  else
  begin
    rdgBoundaryConditions.Columns[Ord(ghbcStartingTime)].Format := rcf4String;
    rdgBoundaryConditions.Columns[Ord(ghbcEndingTime)].Format := rcf4String;
  end;
  rdgBoundaryConditions.Columns[Ord(ghbcParameterName)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(ghbcHead)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(ghbcConductance)].Format := rcf4String;

  rdgBoundaryConditions.Columns[Ord(ghbcStartingTime)].PickList :=
    FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(ghbcEndingTime)].PickList :=
    FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(ghbcParameterName)].PickList :=
    FStringFieldNames;
  AddParameterNamesToPickList(ptGHB, Ord(ghbcParameterName));
  rdgBoundaryConditions.Columns[Ord(ghbcHead)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;
  rdgBoundaryConditions.Columns[Ord(ghbcConductance)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(ghbcStartingTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Cells[Ord(ghbcEndingTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Cells[Ord(ghbcParameterName), 0] := StrParameterName;
  rdgBoundaryConditions.Cells[Ord(ghbcHead), 0] := StrBoundaryHead;
  rdgBoundaryConditions.Cells[Ord(ghbcConductance), 0] := StrConductance;
  FConductanceCol := Ord(ghbcConductance);

  while comboFormulaInterp.Items.Count > 3 do
  begin
    comboFormulaInterp.Items.Delete(comboFormulaInterp.Items.Count-1);
  end;
  comboFormulaInterp.Items.AddStrings(FStringFieldNames);
end;

function TfrmImportShapefile.AssociatedConcColumns: integer;
begin
  if frmGoPhast.PhastModel.Mt3dmsSsmIsSelected then
  begin
    result := frmGoPhast.PhastModel.MobileComponents.Count;
  end
  else
  begin
    result := 0
  end;
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForCHD;
begin
  FCombinedObjects := comboJoinObjects.ItemIndex = 1;
  plBoundary.ActivePage := jvspNone;
  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := Ord(ccEndingHead) + 1
    + AssociatedConcColumns;
  AssignColFeatureProperties;
  rdgBoundaryConditions.Columns[Ord(ccStartingTime)].ComboUsed :=
    not FCombinedObjects;
  rdgBoundaryConditions.Columns[Ord(ccEndingTime)].ComboUsed :=
    not FCombinedObjects;
  rdgBoundaryConditions.Columns[Ord(ccParameterName)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(ccStartingHead)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(ccEndingHead)].ComboUsed := True;

  InitializeColumnsForMt3dConc(Ord(ccConcentration));

  if FCombinedObjects then
  begin
    rdgBoundaryConditions.Columns[Ord(ccStartingTime)].Format := rcf4Integer;
    rdgBoundaryConditions.Columns[Ord(ccEndingTime)].Format := rcf4Integer;
  end
  else
  begin
    rdgBoundaryConditions.Columns[Ord(ccStartingTime)].Format := rcf4String;
    rdgBoundaryConditions.Columns[Ord(ccEndingTime)].Format := rcf4String;
  end;
  rdgBoundaryConditions.Columns[Ord(ccParameterName)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(ccStartingHead)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(ccEndingHead)].Format := rcf4String;

  rdgBoundaryConditions.Columns[Ord(ccStartingTime)].PickList :=
    FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(ccEndingTime)].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(ccParameterName)].PickList :=
    FStringFieldNames;
  AddParameterNamesToPickList(ptCHD, Ord(ccParameterName));
  rdgBoundaryConditions.Columns[Ord(ccStartingHead)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;
  rdgBoundaryConditions.Columns[Ord(ccEndingHead)].PickList :=
    FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(ccStartingTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Cells[Ord(ccEndingTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Cells[Ord(ccParameterName), 0] := StrParameterName;
  rdgBoundaryConditions.Cells[Ord(ccStartingHead), 0] := StrStartingHead;
  rdgBoundaryConditions.Cells[Ord(ccEndingHead), 0] := StrEndingHead;
end;

procedure TfrmImportShapefile.ImportModflowChdBoundary(
  AScreenObject: TScreenObject);
var
  AValue: Extended;
  Item: TChdItem;
  UseRow: Boolean;
  ItemIndex: Integer;
  AFormula: string;
  ParameterName: string;
  Param: TModflowTransientListParameter;
  AnItem: TCustomModflowBoundaryItem;
  ParamItem: TModflowParamItem;
  Boundary: TModflowParamBoundary;
  Count: Integer;
  ValueItem: TValueArrayItem;
  ItemName: string;
  ConcBoundary: TMt3dmsConcBoundary;
  ConcItem: TMt3dmsConcItem;
  StartingConcIndex: integer;
  Dummy: Boolean;
begin
  AScreenObject.CreateChdBoundary;
  Boundary := AScreenObject.ModflowChdBoundary;

  StartingConcIndex := Ord(ccConcentration);
  ConcBoundary := CreateConcBoundary(AScreenObject);

  ParamItem := nil;
  Count := 0;
  for ItemIndex := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(ccStartingTime), ItemIndex + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(ccEndingTime), ItemIndex + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(ccStartingHead), ItemIndex + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(ccEndingHead), ItemIndex + 1] <> '');
    if UseRow then
    begin
      GetTransientParameter(Param, ParameterName, Ord(ccParameterName), ItemIndex+1);
      if (ParameterName <> '') and (Param = nil) then
      begin
        Continue;
      end;
      GetNewOrExistingBoundaryItem(AnItem, ParameterName, Param, ParamItem,
        Boundary, Count);
      Inc(Count);
      Item := AnItem as TChdItem;

      AValue := GetRealValueFromText(rdgBoundaryConditions.
        Cells[Ord(ccStartingTime), ItemIndex + 1], Dummy);
      Item.StartTime := AValue;
      AValue := GetRealValueFromText(rdgBoundaryConditions.
        Cells[Ord(ccEndingTime), ItemIndex + 1], Dummy);
      Item.EndTime := AValue;

      ConcItem := CreateConcItem(ConcBoundary, ItemIndex, Item);

      if FCombinedObjects then
      begin
        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(ccStartingHead), ItemIndex + 1], Dummy);
        ItemName := Format(StrCHDStartingHeadd, [ItemIndex]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          Item.StartHead := rsObjectImportedValuesR + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);

        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(ccEndingHead), ItemIndex + 1], Dummy);
        ItemName := Format(StrCHDEndingHeadd, [ItemIndex]);
        ValueItem := AScreenObject.ImportedValues.ValueItemByName(
          ItemName);
        if ValueItem = nil then
        begin
          ValueItem := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          ValueItem.Name := ItemName;
          ValueItem.Values.DataType := rdtDouble;
          ValueItem.Values.Count := 0;
          Item.EndHead := rsObjectImportedValuesR + '("' + ItemName + '")';
        end;
        ValueItem.Values.Add(AValue);

        ImportConcItemForCombinedShapes(ConcItem, StartingConcIndex, ItemIndex,
          AScreenObject);
      end
      else
      begin
        AFormula := GetRealFormulaFromText(rdgBoundaryConditions.
          Cells[Ord(ccStartingHead), ItemIndex + 1]);
        Item.StartHead := AFormula;
        AFormula := GetRealFormulaFromText(rdgBoundaryConditions.
          Cells[Ord(ccEndingHead), ItemIndex + 1]);
        Item.EndHead := AFormula;

        ImportConcItemForSeparateShapes(ItemIndex, ConcItem,
          StartingConcIndex);
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.EnableFeatureImport;
var
  ShouldEnable: boolean;
begin
  ShouldEnable := (comboBoundaryChoice.Items.Count > 1);
  if ShouldEnable and (frmGoPhast.ModelSelection = msPhast) then
  begin
    ShouldEnable := (comboJoinObjects.ItemIndex = 0)
  end;
  if ShouldEnable then
  begin
    case frmGoPhast.ModelSelection of
      msUndefined:
        begin
          Assert(False);
        end;
      msPhast:
        begin
          ShouldEnable := (rgEvaluatedAt.ItemIndex = 1);
        end;
      msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
        msModflowFmp, msModflowCfp, msModflow2015:
        begin
          ShouldEnable := (rgEvaluatedAt.ItemIndex = 0);
        end;
      msFootPrint:
        begin
          { TODO -cFootprint : Enable import of well locations and data from Shapefiles. }
          ShouldEnable := (FGeometryFile <> nil) and (FGeometryFile.FileHeader.ShapeType in
            [stPoint, stMultiPoint, stPointZ, stMultiPointZ, stPointM, stMultipointM]);
        end
    else
      begin
        Assert(False);
      end;
    end;
  end;
  tabFeatures.TabVisible := ShouldEnable;
end;

procedure TfrmImportShapefile.AssignAPhastWellBoundary(
  AScreenObject: TScreenObject);
var
  AnInterval: TWellInterval;
  UseRow: Boolean;
  Index: Integer;
  WellElevationFormat: string;
  FieldNumber: Integer;
  FieldName: AnsiString;
  BooleanVariable: Boolean;
  AValue: Extended;
  Description: string;
  Dummy: Boolean;
begin
  Description := GetStringValueFromText(WellDescription.Text);
  AScreenObject.WellBoundary.Description := Description;
  AValue := GetRealValueFromText(comboWellDiameter.Text, Dummy);
  AScreenObject.WellBoundary.Diameter := AValue;
  AValue := GetRealValueFromText(comboWellLandSurfaceDatum.Text, Dummy);
  AScreenObject.WellBoundary.LandSurfaceDatum := AValue;
  BooleanVariable := GetBooleanValueFromText(comboWellPumpAllocation.Text);
  AScreenObject.WellBoundary.AllocateByPressureAndMobility := BooleanVariable;

  FieldName := AnsiString(comboWellIntervalStyle.Text);
  FieldNumber := GetFieldNumberFromName(FieldName);
  if FieldNumber = 0 then
  begin
    // not a field.
    if comboWellIntervalStyle.ItemIndex in [0, 1] then
    begin
      AScreenObject.WellBoundary.WellElevationFormat :=
        TWellElevationFormat(comboWellIntervalStyle.ItemIndex);
    end;
  end
  else
  begin
    WellElevationFormat := UpperCase(xbShapeDataBase.GetFieldStr(FieldName));
    if Pos(StrELEVATION_UC, WellElevationFormat) > 0 then
    begin
      AScreenObject.WellBoundary.WellElevationFormat := wefElevation;
    end
    else if Pos(StrDEPTH_UC, WellElevationFormat) > 0 then
    begin
      AScreenObject.WellBoundary.WellElevationFormat := wefDepth;
    end;
  end;

  for Index := 0 to seWellIntervals.AsInteger - 1 do
  begin
    UseRow := (dgWellElevations.Cells[1, Index + 1] <> '')
      and (dgWellElevations.Cells[2, Index + 1] <> '');
    if UseRow then
    begin
      AnInterval := AScreenObject.WellBoundary.Intervals.Add as TWellInterval;
      AValue := GetRealValueFromText(dgWellElevations.Cells[1, Index + 1], Dummy);
      AnInterval.FirstElevation := AValue;
      AValue := GetRealValueFromText(dgWellElevations.Cells[2, Index + 1], Dummy);
      AnInterval.SecondElevation := AValue;
    end;
  end;
  AssignAPhastBoundary(AScreenObject.WellBoundary);
end;

function TfrmImportShapefile.GetBooleanValueFromText(
  FieldName: AnsiString): boolean;
var
  Value: string;
  FieldNumber: Integer;
begin
  FieldNumber := GetFieldNumberFromName(FieldName);
  if FieldNumber = 0 then
  begin
    // not a field
    Value := UpperCase(String(FieldName));
    result := (Length(Value) > 0)
      and ((Value[1] = 'T') or (Value[1] = 'Y'));
  end
  else
  begin
    Value := xbShapeDataBase.GetFieldStr(FieldName);
    if (Value = 'Y') or (Value = 'y') or (Value = 'T') or (Value = 't') then
    begin
      result := True;
    end
    else
    begin
      result := False;
    end;
  end;
end;

function TfrmImportShapefile.GetStringValueFromText(
  const FieldName: AnsiString): string;
var
  FieldNumber: Integer;
  CachedPosition: Integer;
  FieldStorage: TFieldNumStorage;
begin
  if FieldName = '' then
  begin
    result := '';
    Exit;
  end;
  CachedPosition := FFieldNumbers.Indexof(string(FieldName));
  if CachedPosition >= 0 then
  begin
    FieldStorage := FFieldNumbers.Objects[CachedPosition] as TFieldNumStorage;
    FieldNumber := FieldStorage.FieldNumber;
  end
  else
  begin
    FieldNumber := GetFieldNumberFromName(FieldName);
    FieldStorage := TFieldNumStorage.Create(xbShapeDataBase);
    FieldStorage.FieldNumber := FieldNumber;
    FFieldNumbers.AddObject(string(FieldName), FieldStorage);
  end;
//  FieldNumber := GetFieldNumberFromName(FieldName);
  if FieldNumber = 0 then
  begin
    if Trim(string(FieldName)) = '' then
    begin
      result := '""';
    end
    else
    begin
      result := Trim(string(FieldName));
      if result[1] <> '"' then
      begin
        result := '"' + result;
      end;
      if result[Length(result)] <> '"' then
      begin
        result := result + '"';
      end;
    end;
  end
  else
  begin
    result := xbShapeDataBase.GetFieldbyNumber(FieldNumber);
  end;
  result := Trim(result);
end;

procedure TfrmImportShapefile.AssignAPhastRiverBoundary(
  AScreenObject: TScreenObject);
var
  AFormula: string;
  Description: string;
begin
  Description := GetStringValueFromText(comboRiverDescripton.Text);
  AScreenObject.RiverBoundary.Description := Description;
  AFormula := GetRealFormulaFromText(comboRiverHydraulicConductivity.Text);
  AScreenObject.RiverBoundary.BedHydraulicConductivity := AFormula;
  AFormula := GetRealFormulaFromText(comboRiverWidth.Text);
  AScreenObject.RiverBoundary.Width := AFormula;
  AFormula := GetRealFormulaFromText(comboRiverDepth.Text);
  AScreenObject.RiverBoundary.Depth := AFormula;
  AFormula := GetRealFormulaFromText(comboRiverBedThickness.Text);
  AScreenObject.RiverBoundary.BedThickness := AFormula;
  AssignAPhastBoundary(AScreenObject.RiverBoundary);
end;

procedure TfrmImportShapefile.AssignAPhastLeakyBoundary(
  AScreenObject: TScreenObject);
var
  AFormula: string;
begin
  AFormula := GetRealFormulaFromText(comboLeakyHydraulicConductivity.Text);
  AScreenObject.LeakyBoundary.HydraulicConductivity := AFormula;
  AFormula := GetRealFormulaFromText(comboLeakyThickness.Text);
  AScreenObject.LeakyBoundary.Thickness := AFormula;
  AssignAPhastBoundary(AScreenObject.LeakyBoundary);
end;

function TfrmImportShapefile.DataArrayOrientationOK(
  DataArray: TDataArray): boolean;
begin
  Assert(DataArray <> nil);
  if rgElevationCount.ItemIndex = 0 then
  begin
    result := DataArray.Orientation = dsoTop;
  end
  else
  begin
    result := DataArray.Orientation in [dsoTop, dso3D];
  end;
end;

function TfrmImportShapefile.GetRealFormulaFromText(
  const Text: string; DataSetsOK: boolean = True;
  FormulaOK: boolean = False): string;
var
  FieldNumber: Integer;
  DataArray: TDataArray;
  Variable: TGlobalVariable;
  CachedPosition: Integer;
  FieldStorage: TFieldNumStorage;
  Value: double;
  Dummy: Boolean;
begin
  if Text = '' then
  begin
    result := '0.';
    Exit;
  end;
  CachedPosition := FFieldNumbers.Indexof(Text);
  if CachedPosition >= 0 then
  begin
    FieldStorage := FFieldNumbers.Objects[CachedPosition] as TFieldNumStorage;
    FieldNumber := FieldStorage.FieldNumber;
  end
  else
  begin
    FieldNumber := GetFieldNumberFromName(AnsiString(Text));
    FieldStorage := TFieldNumStorage.Create(xbShapeDataBase);
    FieldStorage.FieldNumber := FieldNumber;
    FFieldNumbers.AddObject(Text, FieldStorage);
  end;
//  FieldNumber := GetFieldNumberFromName(Text);
  if FieldNumber <> 0 then
  begin
    if xbShapeDataBase.GetFieldType(FieldNumber) = xbfChar then
    begin
      result := GetRealFormulaFromText(Trim(
        GetStringValueFromText(Text)), DataSetsOK, FormulaOK);
    end
    else
    begin
      result := FortranFloatToStr(GetRealValueFromText(Text, Dummy));
    end;
  end
  else
  begin
//    if FieldStorage.Formula <> '' then
//    begin
//      result := FieldStorage.Formula;
//      Exit;
//    end;
    if TryStrToFloat(Text, Value) then
    begin
      result := Text;
      FieldStorage.Formula := result;
      exit;
    end;
    if DataSetsOK then
    begin
      DataArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(Text);
      if (DataArray <> nil) and
        DataArrayOrientationOK(DataArray) and
        (DataArray.DataType in [rdtDouble, rdtInteger]) then
      begin
        result := Text;
        FieldStorage.Formula := result;
        Exit;
      end;
    end;

    Variable := frmGoPhast.PhastModel.GlobalVariables.GetVariableByName(Text);
    if (Variable <> nil) and
      (Variable.Format in [rdtDouble, rdtInteger]) then
    begin
      result := Text;
      FieldStorage.Formula := result;
      Exit;
    end;

    if FormulaOK then
    begin
      result := Text;
      FieldStorage.Formula := result;
      Exit;
    end;

    result := FortranFloatToStr(GetRealValueFromText(Text, Dummy));
    FieldStorage.Formula := result;
  end;
end;

function TfrmImportShapefile.GetRealValueFromText(
  const FieldName: String; var ShouldIgnore: Boolean): Extended;
begin
  result := GetRealValueFromText(AnsiString(FieldName), ShouldIgnore);
end;

function TfrmImportShapefile.GetRealValueFromText(
  const FieldName: AnsiString; var ShouldIgnore: Boolean): Extended;
var
  FieldNumber: Integer;
  CachedPosition: Integer;
  FieldStorage: TFieldNumStorage;
begin
  ShouldIgnore := False;
  if FieldName = '' then
  begin
    result := 0;
    ShouldIgnore := True;
    Exit;
  end;
  CachedPosition := FFieldNumbers.Indexof(string(FieldName));
  if CachedPosition >= 0 then
  begin
    FieldStorage := FFieldNumbers.Objects[CachedPosition] as TFieldNumStorage;
    FieldNumber := FieldStorage.FieldNumber;
  end
  else
  begin
    FieldNumber := GetFieldNumberFromName(FieldName);
    FieldStorage := TFieldNumStorage.Create(xbShapeDataBase);
    FieldStorage.FieldNumber := FieldNumber;
    FFieldNumbers.AddObject(string(FieldName), FieldStorage);
  end;
  if FieldNumber = 0 then
  begin
    // not a field
    if not TryStrToFloat(string(FieldName), result) then
    begin
      result := GetValueFromCsv(string(FieldName), ShouldIgnore);
    end;
  end
  else
  begin
    ShouldIgnore := xbShapeDataBase.GetFieldByNumber(FieldNumber) = '';
    result := xbShapeDataBase.GetFieldNum(FieldNumber);
  end;
end;

function TfrmImportShapefile.GetStringValueFromText(
  const FieldName: String): string;
begin
  result := GetStringValueFromText(AnsiString(FieldName));
end;

function TfrmImportShapefile.GetIntegerFormulaFromText(
  const Text: AnsiString; DataSetsOK: boolean = True): string;
var
  FieldNumber: Integer;
  DataArray: TDataArray;
  Variable: TGlobalVariable;
  CachedPosition: Integer;
  FieldStorage: TFieldNumStorage;
  Value: integer;
begin
  CachedPosition := FFieldNumbers.Indexof(string(Text));
  if CachedPosition >= 0 then
  begin
    FieldStorage := FFieldNumbers.Objects[CachedPosition] as TFieldNumStorage;
    FieldNumber := FieldStorage.FieldNumber;
  end
  else
  begin
    FieldNumber := GetFieldNumberFromName(Text);
    FieldStorage := TFieldNumStorage.Create(xbShapeDataBase);
    FieldStorage.FieldNumber := FieldNumber;
    FFieldNumbers.AddObject(string(Text), FieldStorage);
  end;
//  FieldNumber := GetFieldNumberFromName(Text);
  if FieldNumber <> 0 then
  begin
    if xbShapeDataBase.GetFieldType(FieldNumber) = xbfChar then
    begin
      result := GetIntegerFormulaFromText(Trim(
        GetStringValueFromText(Text)), DataSetsOK);
    end
    else
    begin
      Result := IntToStr(GetIntegerValueFromText(Text));
    end;
  end
  else
  begin
    if FieldStorage.Formula <> '' then
    begin
      result := FieldStorage.Formula;
      Exit;
    end;
    if TryStrToInt(string(Text), Value) then
    begin
      result := string(Text);
      FieldStorage.Formula := result;
      exit;
    end;
    if DataSetsOK then
    begin
      DataArray := frmGoPhast.PhastModel.DataArrayManager.
        GetDataSetByName(string(Text));
      if (DataArray <> nil) and
        DataArrayOrientationOK(DataArray) and
        (DataArray.DataType = rdtInteger) then
      begin
        result := string(Text);
        FieldStorage.Formula := result;
        Exit;
      end;
    end;

    Variable := frmGoPhast.PhastModel.GlobalVariables.
      GetVariableByName(string(Text));
    if (Variable <> nil) and
      (Variable.Format = rdtInteger) then
    begin
      result := string(Text);
      FieldStorage.Formula := result;
      Exit;
    end;

    Result := IntToStr(GetIntegerValueFromText(Text));
    FieldStorage.Formula := result;
  end;
end;


function TfrmImportShapefile.GetIntegerFormulaFromText(const text: String;
  DataSetsOK: boolean): string;
begin
  result := GetIntegerFormulaFromText(AnsiString(text), DataSetsOK);
end;

function TfrmImportShapefile.GetIntegerValueFromText(
  const FieldName: String): integer;
begin
  result := GetIntegerValueFromText(AnsiString(FieldName));
end;

function TfrmImportShapefile.GetIntegerValueFromText(
  const FieldName: AnsiString): integer;
var
  FieldNumber: Integer;
  CachedPosition: Integer;
  FieldStorage: TFieldNumStorage;
begin
  CachedPosition := FFieldNumbers.Indexof(string(FieldName));
  if CachedPosition >= 0 then
  begin
    FieldStorage := FFieldNumbers.Objects[CachedPosition] as TFieldNumStorage;
    FieldNumber := FieldStorage.FieldNumber;
  end
  else
  begin
    FieldNumber := GetFieldNumberFromName(FieldName);
    FieldStorage := TFieldNumStorage.Create(xbShapeDataBase);
    FieldStorage.FieldNumber := FieldNumber;
    FFieldNumbers.AddObject(string(FieldName), FieldStorage);
  end;
//  FieldNumber := GetFieldNumberFromName(FieldName);
  if FieldNumber = 0 then
  begin
    // not a field
    if not TryStrToInt(string(FieldName), result) then
    begin
      result := 0;
    end;
  end
  else
  begin
    result := xbShapeDataBase.GetFieldInt(FieldNumber);
  end;
end;

procedure TfrmImportShapefile.AssignAPhastBoundary(
  Boundary: TCustomInterpolatedBoundary);
var
  IntItem: TIntegerPhastBoundaryCondition;
  AnInt: Integer;
  RealItem: TRealPhastBoundaryCondition;
  AValue: Extended;
  FieldNumber: Integer;
  FieldName: AnsiString;
  ATime: Extended;
  UseRow: Boolean;
  Index: Integer;
begin
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[0, Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[1, Index + 1] <> '');
    if UseRow then
    begin
      ATime := 0;
      FieldName := AnsiString(rdgBoundaryConditions.Cells[0, Index + 1]);
      FieldNumber := GetFieldNumberFromName(FieldName);
      if FieldNumber = 0 then
      begin
        // not a field
        if not TryStrToFloat(string(FieldName), ATime) then
        begin
          Continue;
        end;
      end
      else
      begin
        ATime := xbShapeDataBase.GetFieldNum(FieldName);
      end;
      AValue := 0;
      FieldName := AnsiString(rdgBoundaryConditions.Cells[1, Index + 1]);
      FieldNumber := GetFieldNumberFromName(FieldName);
      if FieldNumber = 0 then
      begin
        // not a field
        if not TryStrToFloat(String(FieldName), AValue) then
        begin
          Continue;
        end;
      end
      else
      begin
        AValue := xbShapeDataBase.GetFieldNum(FieldName);
      end;
      if (ATime = 0) and (Index < Boundary.BoundaryValue.Count) then
      begin
        RealItem := Boundary.BoundaryValue.Items[Index]
          as TRealPhastBoundaryCondition;
      end
      else
      begin
        RealItem := Boundary.BoundaryValue.Add as TRealPhastBoundaryCondition;
      end;
      RealItem.Time := ATime;
      RealItem.FormulaExpression := FortranFloatToStr(AValue);
      AnInt := 0;
      FieldName := AnsiString(rdgBoundaryConditions.Cells[2, Index + 1]);
      if FieldName <> '' then
      begin
        FieldNumber := GetFieldNumberFromName(FieldName);
        if FieldNumber = 0 then
        begin
          // not a field
          if not TryStrToInt(string(FieldName), AnInt) then
          begin
            Continue;
          end;
        end
        else
        begin
          AnInt := xbShapeDataBase.GetFieldInt(FieldName);
        end;
        if (ATime = 0) and (Index < Boundary.Solution.Count) then
        begin
          IntItem := Boundary.Solution.Items[Index]
            as TIntegerPhastBoundaryCondition;
        end
        else
        begin
          IntItem := Boundary.Solution.Add as TIntegerPhastBoundaryCondition;
        end;
        IntItem.Time := ATime;
        IntItem.FormulaExpression := IntToStr(AnInt);
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.AssignAPhastSpecifiedHeadBoundary(
  AScreenObject: TScreenObject);
var
  SolutionType: string;
  FieldNumber: Integer;
  FieldName: AnsiString;
begin
  FieldName := AnsiString(comboSolutionType.Text);
  FieldNumber := GetFieldNumberFromName(FieldName);
  if FieldNumber = 0 then
  begin
    // not a field.
    if comboSolutionType.ItemIndex in [0, 1] then
    begin
      AScreenObject.SpecifiedHeadBoundary.SolutionType :=
        TSolutionType(comboSolutionType.ItemIndex);
    end;
  end
  else
  begin
    SolutionType := UpperCase(xbShapeDataBase.GetFieldStr(FieldName));
    if Pos(StrSPECIFIED_UC, SolutionType) > 0 then
    begin
      AScreenObject.SpecifiedHeadBoundary.SolutionType := stSpecified;
    end
    else if Pos(StrASSOCIATED_UC, SolutionType) > 0 then
    begin
      AScreenObject.SpecifiedHeadBoundary.SolutionType := stAssociated;
    end;
  end;
  AssignAPhastBoundary(AScreenObject.SpecifiedHeadBoundary);
end;

procedure TfrmImportShapefile.InitializeBoundaryConditionControls;
var
  Model: TPhastModel;
  Packages: TModflowPackages;
  Index: Integer;
  Item: TTimeItem;
begin
  comboBoundaryChoice.Items.Clear;
  comboBoundaryChoice.Items.Add('none');
  plBoundary.ActivePage := jvspNone;
  Model := frmGoPhast.PhastModel;
  case Model.ModelSelection of
    msUndefined:
      begin
        Assert(False);
      end;
    msPhast:
      begin
        comboBoundaryChoice.Items.Add(StrSpecifiedHead);
        comboBoundaryChoice.Items.Add(StrFluxBoundary);
        comboBoundaryChoice.Items.Add(StrLeakyBoundary);
        comboBoundaryChoice.Items.Add(StrRiverBoundary);
        comboBoundaryChoice.Items.Add(StrWellBoundary);

        rdgBoundaryConditions.Cells[0,0] := 'Time';

        rdgBoundaryConditions.Columns[0].Format := rcf4Real;
        rdgBoundaryConditions.Columns[0].ComboUsed := True;

        rdgBoundaryConditions.Columns[0].PickList.Add(
          FloatToStr(Model.Times.StartTime.Value));
        for Index := 0 to Model.Times.Count - 1 do
        begin
          Item := Model.Times.Items[Index] as TTimeItem;
          rdgBoundaryConditions.Columns[0].PickList.Add(
            FloatToStr(Item.EndingTime));
        end;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp:
      begin
        Packages := Model.ModflowPackages;
        AddModflowPackageToImportChoices(Packages.ChdBoundary);
        AddModflowPackageToImportChoices(Packages.DrnPackage);
        AddModflowPackageToImportChoices(Packages.DrtPackage);
        AddModflowPackageToImportChoices(Packages.EtsPackage);
        AddModflowPackageToImportChoices(Packages.EvtPackage);
        AddModflowPackageToImportChoices(Packages.GhbBoundary);
        AddModflowPackageToImportChoices(Packages.HfbPackage);
        AddModflowPackageToImportChoices(Packages.HobPackage);
        AddModflowPackageToImportChoices(Packages.LakPackage);
        AddModflowPackageToImportChoices(Packages.Mnw2Package);
//        AddModflowPackageToImportChoices(Packages.Mf6ObservationUtility);
        AddModflowPackageToImportChoices(Packages.RchPackage);
        AddModflowPackageToImportChoices(Packages.ResPackage);
        AddModflowPackageToImportChoices(Packages.RivPackage);
        AddModflowPackageToImportChoices(Packages.SfrPackage);

        AddModflowPackageToImportChoices(Packages.StrPackage);

        AddModflowPackageToImportChoices(Packages.UzfPackage);
        AddModflowPackageToImportChoices(Packages.WelPackage);


        if (Model.ModelSelection = msModflowCfp)
          and  Packages.ConduitFlowProcess.IsSelected
          and Packages.ConduitFlowProcess.PipesUsed then
        begin
          comboBoundaryChoice.Items.AddObject(StrPipesIn +
            Packages.ConduitFlowProcess.PackageIdentifier,
            Packages.ConduitFlowProcess);
        end;
      end;
    msModflow2015:
      begin
        Packages := Model.ModflowPackages;
        AddModflowPackageToImportChoices(Packages.ChdBoundary);
        AddModflowPackageToImportChoices(Packages.DrnPackage);
        AddModflowPackageToImportChoices(Packages.EtsPackage);
        AddModflowPackageToImportChoices(Packages.GhbBoundary);
        AddModflowPackageToImportChoices(Packages.Mf6ObservationUtility);
        AddModflowPackageToImportChoices(Packages.LakMf6Package);
//        AddModflowPackageToImportChoices(Packages.HfbPackage);
        AddModflowPackageToImportChoices(Packages.RchPackage);
        AddModflowPackageToImportChoices(Packages.RivPackage);
        AddModflowPackageToImportChoices(Packages.MawPackage);
        AddModflowPackageToImportChoices(Packages.SfrModflow6Package);
//        AddModflowPackageToImportChoices(Packages.UzfPackage);
        AddModflowPackageToImportChoices(Packages.WelPackage);
      end;
    msSutra22, msSutra30:
      begin
        { TODO -cSUTRA : Enable import SUTRA boundaries from Shapefiles. }
      end;
    msFootPrint:
      begin
        comboBoundaryChoice.Items.AddObject(StrFootprintWithdrawal,
          Model.FootprintProperties);
        { TODO -cFootprint : Enable import of well locations and data from Shapefiles. }
      end
  else
    begin
      Assert(False);
    end;
  end;
  if comboBoundaryChoice.Items.Count <= 1 then
  begin
    pnlBoundaryCondition.Visible := false;
  end;
end;

procedure TfrmImportShapefile.EnableOK;
var
  ShouldEnable: boolean;
begin
  ShouldEnable := cbImportObjects.Checked
  or (cbImportGrid.Enabled and  cbImportGrid.Checked);
  if cbImportGrid.Enabled then
  begin
    EmphasizeCheckBoxes([cbImportObjects, cbImportGrid]);
  end
  else
  begin
    EmphasizeCheckBoxes([cbImportObjects]);
  end;
  if cbImportObjects.Checked then
  begin
    if cbEnclosedCells.Enabled then
    begin
      EmphasizeCheckBoxes([cbEnclosedCells, cbIntersectedCells,
        cbInterpolation]);
      ShouldEnable := cbEnclosedCells.Checked
        or cbIntersectedCells.Checked
        or cbInterpolation.Checked
    end
    else
    begin
      cbEnclosedCells.ParentFont := True;
      EmphasizeCheckBoxes([cbIntersectedCells, cbInterpolation]);
      ShouldEnable := cbIntersectedCells.Checked
        or cbInterpolation.Checked
    end;
  end;
  btnOK.Enabled := ShouldEnable;
end;

procedure TfrmImportShapefile.ImportGrid(FieldNames: TStringList);
var
  UndoCreateGrid: TUndoCreateGrid;
  ZeroPosition: Integer;
  DistanceToOrigin: Double;
  DeltaAngle: Double;
  ColumnAngle: Double;
  RowAngle: Double;
  ShapeObject: TShapeObject;
  Index: Integer;
  ColumnPositions: TRealList;
  RowPositions: TRealList;
  YCount: TIntegerList;
  XCount: TIntegerList;
  YIndex: Integer;
  XIndex: Integer;
  TestRowAngle, TestColAngle: double;
  FoundAngles: boolean;
  procedure NoGrid;
  begin
    Beep;
    MessageDlg(StrTheShapefileAppear, mtInformation, [mbOK], 0);
  end;
begin
  frmProgressMM.Caption := StrCreatingGrid;
  frmProgressMM.pbProgress.Max := FGeometryFile.Count;
  frmProgressMM.pbProgress.Position := 0;
  frmProgressMM.ProgressLabelCaption := Format(Str0OutOfD,
    [frmProgressMM.pbProgress.Max]);
  frmProgressMM.Prefix := StrShape;
  frmProgressMM.PopupParent := self;
  frmProgressMM.Show;

  FoundAngles := False;
  TestRowAngle := 0;
  TestColAngle := 0;
  RowAngle := 0;
  XIndex := FieldNames.IndexOf('X_INDEX');
  YIndex := FieldNames.IndexOf('Y_INDEX');
  Assert((XIndex >= 0) and (YIndex >= 0));
  XCount := TIntegerList.Create;
  YCount := TIntegerList.Create;
  RowPositions := TRealList.Create;
  ColumnPositions := TRealList.Create;
  try
    xbShapeDataBase.GotoBOF;
    for Index := 0 to FGeometryFile.Count - 1 do
    begin
      ShapeObject := FGeometryFile[Index];
      if (ShapeObject.FNumPoints <> 5) or (ShapeObject.FNumParts <> 1) then
      begin
        NoGrid;
        Exit;
      end;
      RowAngle := ArcTan2((ShapeObject.FPoints[0].Y - ShapeObject.FPoints[1].Y),
        (ShapeObject.FPoints[0].X - ShapeObject.FPoints[1].X));
      if RowAngle > Pi / 2 then
      begin
        RowAngle := RowAngle - Pi;
      end;
      if RowAngle <= -Pi / 2 then
      begin
        RowAngle := RowAngle + Pi;
      end;

      ColumnAngle := ArcTan2(
        (ShapeObject.FPoints[1].Y - ShapeObject.FPoints[2].Y),
        (ShapeObject.FPoints[1].X - ShapeObject.FPoints[2].X));
      if ColumnAngle > Pi / 2 then
      begin
        ColumnAngle := ColumnAngle - Pi;
      end;
      if ColumnAngle <= -Pi / 2 then
      begin
        ColumnAngle := ColumnAngle + Pi;
      end;

      if FoundAngles then
      begin
        if Abs(TestRowAngle - RowAngle) > 1E-08 then
        begin
          NoGrid;
          Exit;
        end;
        if Abs(TestColAngle - ColumnAngle) > 1E-08 then
        begin
          NoGrid;
          Exit;
        end;
      end
      else
      begin
        DeltaAngle := Abs(RowAngle - ColumnAngle) - Pi / 2;
        if DeltaAngle > 1E-08 then
        begin
          NoGrid;
          Exit;
        end;
        TestRowAngle := RowAngle;
        TestColAngle := ColumnAngle;
        FoundAngles := True;
      end;

      RowAngle := ArcTan2((ShapeObject.FPoints[3].Y - ShapeObject.FPoints[2].Y),
        (ShapeObject.FPoints[3].X - ShapeObject.FPoints[2].X));
      if RowAngle > Pi / 2 then
      begin
        RowAngle := RowAngle - Pi;
      end;
      if RowAngle <= -Pi / 2 then
      begin
        RowAngle := RowAngle + Pi;
      end;

      ColumnAngle := ArcTan2(
        (ShapeObject.FPoints[4].Y - ShapeObject.FPoints[3].Y),
        (ShapeObject.FPoints[4].X - ShapeObject.FPoints[3].X));
      if ColumnAngle > Pi / 2 then
      begin
        ColumnAngle := ColumnAngle - Pi;
      end;
      if ColumnAngle <= -Pi / 2 then
      begin
        ColumnAngle := ColumnAngle + Pi;
      end;

      if Abs(TestRowAngle - RowAngle) > 1E-08 then
      begin
        NoGrid;
        Exit;
      end;
      if Abs(TestColAngle - ColumnAngle) > 1E-08 then
      begin
        NoGrid;
        Exit;
      end;

      XIndex := xbShapeDataBase.GetFieldInt('X_INDEX');
      Assert(XIndex >= 1);
      YIndex := xbShapeDataBase.GetFieldInt('Y_INDEX');
      Assert(YIndex >= 1);
      while XCount.Count <= XIndex do
      begin
        XCount.Add(0);
      end;
      while ColumnPositions.Count <= XIndex do
      begin
        ColumnPositions.Add(0);
      end;
      if DistanceOriginToLine(ShapeObject.FPoints[1], ShapeObject.FPoints[2],
        DistanceToOrigin) then
      begin
        XCount[XIndex - 1] := XCount[XIndex - 1] + 1;
        ColumnPositions[XIndex - 1] :=
          ColumnPositions[XIndex - 1] + DistanceToOrigin;
      end;
      if DistanceOriginToLine(ShapeObject.FPoints[4], ShapeObject.FPoints[3],
        DistanceToOrigin) then
      begin
        XCount[XIndex] := XCount[XIndex] + 1;
        ColumnPositions[XIndex] := ColumnPositions[XIndex] + DistanceToOrigin;
      end;
      while YCount.Count <= YIndex do
      begin
        YCount.Add(0);
      end;
      while RowPositions.Count <= YIndex do
      begin
        RowPositions.Add(0);
      end;
      if DistanceOriginToLine(ShapeObject.FPoints[0], ShapeObject.FPoints[1],
        DistanceToOrigin) then
      begin
        YCount[YIndex] := YCount[YIndex] + 1;
        RowPositions[YIndex] := RowPositions[YIndex] + DistanceToOrigin;
      end;
      if DistanceOriginToLine(ShapeObject.FPoints[3], ShapeObject.FPoints[2],
        DistanceToOrigin) then
      begin
        YCount[YIndex - 1] := YCount[YIndex - 1] + 1;
        RowPositions[YIndex - 1] :=
          RowPositions[YIndex - 1] + DistanceToOrigin;
      end;
      xbShapeDataBase.GotoNext;
      frmProgressMM.StepIt;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
    end;
    ZeroPosition := XCount.IndexOf(0);
    while ZeroPosition >= 0 do
    begin
      XCount.Delete(ZeroPosition);
      ColumnPositions.Delete(ZeroPosition);
      ZeroPosition := XCount.IndexOf(0);
    end;
    ZeroPosition := YCount.IndexOf(0);
    while ZeroPosition >= 0 do
    begin
      YCount.Delete(ZeroPosition);
      RowPositions.Delete(ZeroPosition);
      ZeroPosition := YCount.IndexOf(0);
    end;
    if (XCount.Count = 0) or (YCount.Count = 0) then
    begin
      MessageDlg(StrUnableToImportGri, mtInformation, [mbOK], 0);
    end
    else
    begin
      UndoCreateGrid := TUndoCreateGrid.Create;
      try
        SetLength(UndoCreateGrid.FNewColumns, ColumnPositions.Count);
        for Index := 0 to ColumnPositions.Count - 1 do
        begin
          UndoCreateGrid.FNewColumns[Index] :=
            ColumnPositions[Index] / XCount[Index];
        end;
        SetLength(UndoCreateGrid.FNewRows, RowPositions.Count);
        for Index := 0 to RowPositions.Count - 1 do
        begin
          UndoCreateGrid.FNewRows[Index] :=
            RowPositions[Index] / YCount[Index];
        end;
        SetLength(UndoCreateGrid.FNewLayerElevations, 0);

        UndoCreateGrid.NewAngle := RowAngle;
      except
        UndoCreateGrid.Free;
        raise ;
      end;
      frmGoPhast.UndoStack.Submit(UndoCreateGrid);
    end;
  finally
    XCount.Free;
    YCount.Free;
    RowPositions.Free;
    ColumnPositions.Free;
  end;
end;

function TfrmImportShapefile.CheckDataSets: boolean;
var
  UsedDataSets: TStringList;
  Position: integer;
  DataSetName: string;
  Index: integer;
  DataSet: TDataArray;
  FieldIndex: integer;
begin
  result := False;
  UsedDataSets := TStringList.Create;
  try
    for Index := 1 to dgFields.RowCount - 1 do
    begin
      DataSetName := dgFields.Cells[Ord(fgcDataSet), Index];
      if dgFields.Checked[Ord(fgcImport), Index] and
        (DataSetName <> rsNewDataSet) then
      begin
        Position := UsedDataSets.IndexOf(DataSetName);
        if Position >= 0 then
        begin
          Beep;
          MessageDlg('"' + DataSetName + StrHasBeenSelected, mtWarning,
            [mbOK], 0);
          Exit;
        end;
        UsedDataSets.Add(DataSetName);
        DataSet := frmGoPhast.PhastModel.DataArrayManager.
          GetDataSetByName(DataSetName);
        Assert(DataSet <> nil);
//        DataSet := frmGoPhast.PhastModel.DataSets[Position];
        FieldIndex := GetFieldNumberFromName(AnsiString(dgFields.
          Cells[0, Index]));
        // A FieldIndex of zero indicates that the field does not come from
        // the shapefile database.
        case DataSet.DataType of
          rdtInteger:
            begin
              Assert(FieldIndex >= 1);
              if (xbShapeDataBase.GetFieldType(FieldIndex) <> xbfNumber)
                or (xbShapeDataBase.GetFieldDecimals(FieldIndex) <> 0) then
              begin
                Beep;
                MessageDlg(Format(Str0sContainsInte,
                  [DataSetName, dgFields.Cells[Ord(fgcAttributes), Index]]),
                  mtWarning, [mbOK], 0);
                Exit;
              end;
            end;
          rdtDouble:
            begin
              if (FieldIndex >= 1)
                and (xbShapeDataBase.GetFieldType(FieldIndex) <> xbfNumber) then
              begin
                Beep;
                MessageDlg(Format(Str0sContainsReal,
                  [DataSetName, dgFields.Cells[Ord(fgcAttributes), Index]]),
                  mtWarning, [mbOK], 0);
                Exit;
              end;
            end;
          rdtBoolean:
            begin
              Assert(FieldIndex >= 1);
              if xbShapeDataBase.GetFieldType(FieldIndex) <> xbfLogic then
              begin
                Beep;
                MessageDlg(Format(Str0sContainsBool,
                  [DataSetName, dgFields.Cells[Ord(fgcAttributes), Index]]),
                  mtWarning, [mbOK], 0);
                Exit;
              end;
            end;
          rdtString:
            begin
              Assert(FieldIndex >= 1);
              if xbShapeDataBase.GetFieldType(FieldIndex) <> xbfChar then
              begin
                Beep;
                MessageDlg(Format(Str0sContainsStri,
                  [DataSetName, dgFields.Cells[Ord(fgcAttributes), Index]]),
                  mtWarning, [mbOK], 0);
                Exit;
              end;
            end;
        else
          Assert(False);
        end;
      end;
    end;
    result := True;
  finally
    UsedDataSets.Free;
  end;
end;

procedure TfrmImportShapefile.MakeNewDataSets(NewDataSets: TList);
var
  Index: integer;
  NewDataSetName: string;
  DataSet: TDataArray;
  FieldIndex: integer;
  NewFormula: string;
  NewDataType: TRbwDataType;
  NewProperties: TPhastDataSetStorage;
  OldProperties: TPhastDataSetStorage;
  Orient: TDataSetOrientation;
  InvalidNames: TStringList;
  TestName: string;
  ExistingObserver: TObserver;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
begin
  InvalidNames := TStringList.Create;
  try
//    InvalidNames.AddStrings(dgFields.Columns[Ord(fgcAttributes)]);
    for Index := 1 to dgFields.RowCount - 1 do
    begin
      if dgFields.Checked[Ord(fgcImport), Index] and
        (dgFields.Cells[Ord(fgcDataSet), Index] = rsNewDataSet) then
      begin
        TestName := dgFields.Cells[Ord(fgcAttributes), Index];

        ExistingObserver := frmGoPhast.PhastModel.GetObserverByName(TestName);
        if (ExistingObserver = nil) or (FDuplicateTreatment = drNew)
          or not (ExistingObserver is TDataArray) then
        begin

          NewDataSetName := GenerateNewName(TestName, InvalidNames);

          FieldIndex := GetFieldNumberFromName(AnsiString(
            dgFields.Cells[Ord(fgcAttributes), Index]));

          // a FieldIndex >= 1 indicates the field is in the Shapefile
          // data base.
          NewDataType := rdtDouble;
          if FieldIndex >= 1 then
          begin
            case xbShapeDataBase.GetFieldType(FieldIndex) of
              xbfChar:
                begin
                  NewDataType := rdtString;
                  NewFormula := '""';
                end;
              xbfNumber:
                begin
                  if xbShapeDataBase.GetFieldDecimals(FieldIndex) = 0 then
                  begin
                    NewDataType := rdtInteger;
                    NewFormula := '0';
                  end
                  else
                  begin
                    NewDataType := rdtDouble;
                    NewFormula := '0.';
                  end;
                end;
              xbfLogic:
                begin
                  NewDataType := rdtBoolean;
                  NewFormula := 'False';
                end;
            else
              Assert(False);
            end;
          end
          else
          begin
            NewDataType := rdtDouble;
            NewFormula := '0.';
          end;

          if rgElevationCount.ItemIndex = 0 then
          begin
            Orient := dsoTop;
          end
          else
          begin
            Orient := dso3D;
          end;

          DataSet := frmGoPhast.PhastModel.DataArrayManager.CreateNewDataArray(
            TDataArray, NewDataSetName, NewFormula, NewDataSetName, [], NewDataType,
            TEvaluatedAt(rgEvaluatedAt.ItemIndex), Orient,
            strDefaultClassification + '|' + StrCreatedFromShapefi
            + ExtractFileName(FGeometryFileName));

          NewDataSets.Add(DataSet);

          DataSet.Units := '';
          AssignInterpolator(DataSet, Index, NewProperties, OldProperties);
          NewProperties.Free;
          OldProperties.Free;

          frmGoPhast.PhastModel.UpdateDataArrayDimensions(DataSet);

          dgFields.Cells[Ord(fgcDataSet), Index] := NewDataSetName;
          InvalidNames.Add(NewDataSetName);
        end
        else
        begin
          dgFields.Cells[Ord(fgcDataSet), Index] := TestName;
          if FDuplicateTreatment = drAssignAndDelete then
          begin
            for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
            begin
              AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
              if AScreenObject.Deleted then
              begin
                Continue;
              end;
              if AScreenObject.IndexOfDataSet(ExistingObserver as TDataArray) >= 0 then
              begin 
                FScreenObjectsToDelete.Add(AScreenObject);
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    InvalidNames.Free;
  end;
end;

procedure TfrmImportShapefile.pcImportShapeChange(Sender: TObject);
begin
  inherited;
  if pcImportShape.ActivePage = tabData then
  begin
    dgFields.HideEditor;
  end;
end;

procedure TfrmImportShapefile.SetData;
var
  Index: integer;
  ShapeObject: TShapeObject;
  AScreenObject: TScreenObject;
  UndoCreateScreenObject: TCustomUndo;
  ScreenObjectList: TList;
  PointIndex: integer;
  DataSetIndex: integer;
  DataSetName: string;
  Position: integer;
  DataSet: TDataArray;
  Value: string;
  DataSets: TList;
  FieldNames: TStringList;
  Undo: TUndoImportShapefile;
  EvalAt: TEvaluatedAt;
  Root: string;
  CentralMeridian: double;
  CentralMeridianDegrees: double;
  X, Y: double;
  ShapePoint: TShapePoint;
  PointRecord: TPoint2D;
  Variables: TList;
  IntVariable: TIntegerVariable;
  RealVariable: TRealVariable;
  BooleanVariable: TBooleanVariable;
  StringVariable: TStringVariable;
  AFormula: string;
  StringFormula: string;
  ImportCriterionExpression: TExpression;
  ZExpression: TExpression;
  HighZExpression: TExpression;
  LowZExpression: TExpression;
  Variable: TCustomVariable;
  InvalidObjectNumbers: TIntegerList;
  InvalidFormulaNumbers: TIntegerList;
  ErrorString: string;
  ExistingObjectCount: integer;
  SectionIndex: Integer;
  NextStart: integer;
  NewSection: boolean;
  Formula: string;
  ValueIndex: Integer;
  OptionalExtensions: TStringList;
  OptionalFileName: string;
  Item: TValueArrayItem;
  MultiValueList: TList;
  ValueList: TValueArrayStorage;
  DeleteCount, AddCount: integer;
  NewDataSets: TList;
  NewProperties: TList;
  OldProperties: TList;
  FieldName: string;
  RealFieldNames: TStringList;
  InvalidParametersIndex: Integer;
  PriorPoint: TPoint2D;
  ElevFormula: string;
  HighElevFormula: string;
  LowElevFormula: string;
  TestCompiler: TRbwParser;
  PointValueItem: TPointValuesItem;
  PointValue: TPointValue;
  ZValues: array of double;
  Distances: array of double;
  Slopes: array of double;
  ZValueList: TRealList;
  StartList: TIntegerList;
  Z: Double;
  StartIndex: Integer;
  EndList: TIntegerList;
  EndIndex: Integer;
  ListIndex: Integer;
  ASlope: double;
  MidList: TIntegerList;
  ConvertUnits: boolean;
  FromUnits: TSupportedLengthConv;
  ToUnits: TSupportedLengthConv;
  ADataArray: TDataArray;
  ARealFieldName: String;
  FileIndex: Integer;
  Dummy: Boolean;
  CombinedPointIndex: integer;
  ObjectNames: TStringList;
  AScreenObjectName: string;
begin
  CombinedPointIndex := -1;
  frmProgressMM.ShouldContinue := True;
  ZValues := nil;
  ConvertUnits := (comboFromUnits.ItemIndex > 0) and (comboToUnits.ItemIndex > 0);
  if ConvertUnits then
  begin
    FromUnits := TSupportedLengthConv(comboFromUnits.ItemIndex-1);
    ToUnits := TSupportedLengthConv(comboToUnits.ItemIndex-1);
  end
  else
  begin
    FromUnits := slcCm;
    ToUnits := slcCm;
  end;
  try
    ObjectNames := TStringList.Create;
    FInvalidParameterNames := TStringList.Create;
    try
      FInvalidParameterNames.Sorted := True;
      FInvalidParameterNames.Duplicates := dupIgnore;
      frmGoPhast.ChangingSelection := True;
      FCombinedObjects := comboJoinObjects.Enabled
        and (comboJoinObjects.ItemIndex = 1);
      GlobalDecompileType := dcValue;
      InvalidObjectNumbers := TIntegerList.Create;
      InvalidFormulaNumbers := nil;
      MultiValueList := TList.Create;
      NewDataSets:= TList.Create;
      NewProperties := TObjectList.Create;
      OldProperties := TObjectList.Create;
      try
        CentralMeridian := 0;
        CentralMeridianDegrees := 0;
        if cbCoordinateConversion.Checked then
        begin
          CentralMeridianDegrees := seZoneNumber.Value * 6 - 183;
          CentralMeridian := CentralMeridianDegrees / 180 * Pi;
        end;

        EvalAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
        frmGoPhast.CanDraw := False;
        try
          if not CheckDataSets then
          begin
            ModalResult := mrNone;
            Exit;
          end;
          frmProgressMM.ShouldContinue := True;
          MakeNewDataSets(NewDataSets);
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;
          ChangeInterpolators(NewProperties, OldProperties);

          Variables := TList.Create;
          DataSets := TList.Create;
          FieldNames := TStringList.Create;
          RealFieldNames := TStringList.Create;
          try
            RealFieldNames.CaseSensitive := False;
            rpShapeCompiler.ClearVariables;
            rpShapeCompiler.ClearExpressions;
            AddGIS_Functions(rpShapeCompiler, frmGoPhast.PhastModel.ModelSelection,
              TEvaluatedAt(rgEvaluatedAt.ItemIndex));
            CreateVariables(rpShapeCompiler);
            CreateDataSetVariables(rpShapeCompiler,
              TEValuatedAt(rgEvaluatedAt.ItemIndex));
            frmGoPhast.PhastModel.RegisterGlobalVariables(rpShapeCompiler, True);
            for DataSetIndex := 1 to dgFields.RowCount - 1 do
            begin
              DataSetName := dgFields.Cells[Ord(fgcDataSet), DataSetIndex];
              if dgFields.Checked[Ord(fgcImport), DataSetIndex] then
              begin
                DataSet := frmGoPhast.PhastModel.DataArrayManager.
                  GetDataSetByName(DataSetName);
                Assert(DataSet <> nil);
                DataSets.Add(DataSet);
              end
              else
              begin
                DataSets.Add(nil);
              end;
              FieldName := dgFields.Cells[Ord(fgcAttributes), DataSetIndex];
              if FieldName <> '' then
              begin
                RealFieldNames.Add(FieldName);
                FieldName := FieldToVarName(FieldName);
                FieldNames.Add(FieldName);
                Variable := rpShapeCompiler.Variables[rpShapeCompiler.
                  IndexOfVariable(FieldName)] as TCustomVariable;
                Variables.Add(Variable);
              end;
            end;
            AFormula := edImportCriterion.Text;
            GlobalDecompileType := dcNormal;
            try
              rpShapeCompiler.Compile(AFormula);
            finally
              GlobalDecompileType := dcValue;
            end;
            ImportCriterionExpression := rpShapeCompiler.CurrentExpression;
            Assert(ImportCriterionExpression.ResultType = rdtBoolean);

            ElevFormula := '';
            HighElevFormula := '';
            LowElevFormula := '';
            case rgElevationCount.ItemIndex of
              0:
                begin
                  ZExpression := nil;
                  HighZExpression := nil;
                  LowZExpression := nil;
                end;
              1:
                begin
                  AFormula := edZ.Text;
                  GlobalDecompileType := dcNormal;
                  try
                    rpShapeCompiler.Compile(AFormula);
                  finally
                    GlobalDecompileType := dcValue;
                  end;
                  ZExpression := rpShapeCompiler.CurrentExpression;
                  Assert(ZExpression.ResultType in [rdtDouble, rdtInteger, rdtString]);
                  ElevFormula := AFormula;
                  if (RealFieldNames.IndexOf(AFormula) < 0) or (ZExpression.ResultType = rdtString) then
                  begin
                    if frmGoPhast.PhastModel.GetObserverByName(ElevFormula)
                      <> nil then
                    begin
                      ZExpression := nil;
                    end
                    else
                    begin
                      TestCompiler := frmGoPhast.PhastModel.GetCompiler(dsoTop,
                        TEvaluatedAt(rgEvaluatedAt.ItemIndex));
                      try
                        TestCompiler.Compile(AFormula);
                        ZExpression := nil;
                      except on E: ERbwParserError do
                        // do nothing
                      end;
                    end;
                  end;


                  HighZExpression := nil;
                  LowZExpression := nil;
                end;
              2:
                begin
                  ZExpression := nil;

                  AFormula := edHighZ.Text;
                  GlobalDecompileType := dcNormal;
                  try
                    rpShapeCompiler.Compile(AFormula);
                  finally
                    GlobalDecompileType := dcValue;
                  end;
                  HighZExpression := rpShapeCompiler.CurrentExpression;

                  Assert(HighZExpression.ResultType in [rdtDouble, rdtInteger, rdtString]);
                  HighElevFormula := AFormula;
                  if (RealFieldNames.IndexOf(HighElevFormula) < 0) or (HighZExpression.ResultType = rdtString) then
                  begin
                    if frmGoPhast.PhastModel.GetObserverByName(HighElevFormula)
                      <> nil then
                    begin
                      HighZExpression := nil;
                    end
                    else
                    begin
                      TestCompiler := frmGoPhast.PhastModel.GetCompiler(dsoTop,
                        TEvaluatedAt(rgEvaluatedAt.ItemIndex));
                      try
                        TestCompiler.Compile(AFormula);
                        HighZExpression := nil;
                      except on E: ERbwParserError do
                        // do nothing
                      end;
                    end;
                  end;

                  AFormula := edLowZ.Text;
                  GlobalDecompileType := dcNormal;
                  try
                    rpShapeCompiler.Compile(AFormula);
                  finally
                    GlobalDecompileType := dcValue;
                  end;
                  LowZExpression := rpShapeCompiler.CurrentExpression;
                  Assert(LowZExpression.ResultType in [rdtDouble, rdtInteger, rdtString]);
                  LowElevFormula := AFormula;
                  if (RealFieldNames.IndexOf(LowElevFormula) < 0) or (LowZExpression.ResultType = rdtString) then
                  begin
                    if frmGoPhast.PhastModel.GetObserverByName(LowElevFormula)
                      <> nil then
                    begin
                      LowZExpression := nil;
                    end
                    else
                    begin
                      TestCompiler := frmGoPhast.PhastModel.GetCompiler(dsoTop,
                        TEvaluatedAt(rgEvaluatedAt.ItemIndex));
                      try
                        TestCompiler.Compile(AFormula);
                        LowZExpression := nil;
                      except on E: ERbwParserError do
                        // do nothing
                      end;
                    end;
                  end;
                end;
              else
                begin
                  ZExpression := nil;
                  HighZExpression := nil;
                  LowZExpression := nil;
                  Assert(False);
                end;
            end;

            Root := TScreenObject.ValidName(
              ExtractFileRoot(OpenDialogShape.FileName)+ '_');

            if cbImportGrid.Enabled and cbImportGrid.Checked then
            begin
              ImportGrid(FieldNames);
              if not frmProgressMM.ShouldContinue then
              begin
                Exit;
              end;
            end;

            if cbImportObjects.Checked then
            begin
              frmProgressMM.Caption := StrCreatingObjects;
              frmProgressMM.pbProgress.Max := FGeometryFile.Count;
              frmProgressMM.pbProgress.Position := 0;
              frmProgressMM.ProgressLabelCaption := Format(Str0OutOfD,
                [frmProgressMM.pbProgress.Max]);
              frmProgressMM.Prefix := StrShape;
              frmProgressMM.PopupParent := self;
              frmProgressMM.Show;

              if FDataBaseFileName <> '' then
              begin
                xbShapeDataBase.GotoBOF;
              end;
              frmGoPhast.PhastModel.BeginScreenObjectUpdate;
              ScreenObjectList := TObjectList.Create;
              try
                ExistingObjectCount :=
                  frmGoPhast.PhastModel.
                  NumberOfLargestScreenObjectsStartingWith(Root);

                Undo := TUndoImportShapefile.Create;
                try
                  if FCombinedObjects then
                  begin
                    ScreenObjectList.Capacity := 1;
                  end
                  else
                  begin
                    ScreenObjectList.Capacity := FGeometryFile.Count;
                  end;
                  AScreenObject := nil;
                  DeleteCount := 0;
                  AddCount := 0;
                  for Index := 0 to FGeometryFile.Count - 1 do
                  begin
                    if not frmProgressMM.ShouldContinue then
                    begin
                      Exit;
                    end;
                    FShapeIndex := Index;
                    ShapeObject := FGeometryFile[Index];
                    if ShapeObject.FShapeType = stNull then
                    begin
                      xbShapeDataBase.GoToNext;
                      Continue;
                    end;
                    FNumPointsInCurrentShape := ShapeObject.FNumPoints;
                    if not FCombinedObjects or (Index = 0) then
                    begin
                      AScreenObject := TScreenObject.CreateWithViewDirection(
                        frmGoPhast.PhastModel, vdTop,
                        UndoCreateScreenObject, False);
                      AScreenObject.Comment := 'Imported from ' + FGeometryFileName +' on ' + DateTimeToStr(Now);
                      AScreenObject.ElevationCount :=
                        TElevationCount(rgElevationCount.ItemIndex);
                      if FCombinedObjects then
                      begin
                        AScreenObject.BeginUpdate;
                        AScreenObject.Capacity := FGeometryFile.NumberOfPoints;
                        case AScreenObject.ElevationCount of
                          ecZero: ; // do nothing
                          ecOne:
                            begin
                              if ZExpression = nil then
                              begin
                                AScreenObject.ElevationFormula := ElevFormula;
                              end
                              else
                              begin
                                AScreenObject.ElevationFormula :=
                                  rsObjectImportedValuesR
                                  + '("' + StrImportedElevations + '")';
                              end;
                            end;
                          ecTwo:
                            begin
                              if HighZExpression = nil then
                              begin
                                AScreenObject.HigherElevationFormula :=
                                  HighElevFormula
                              end
                              else
                              begin
                                AScreenObject.HigherElevationFormula :=
                                  rsObjectImportedValuesR
                                  + '("' + StrImportedHigherElev + '")';
                              end;
                              if LowZExpression = nil then
                              begin
                                AScreenObject.LowerElevationFormula
                                  := LowElevFormula;
                              end
                              else
                              begin
                                AScreenObject.LowerElevationFormula :=
                                  rsObjectImportedValuesR
                                  + '("' + StrImportedLowerEleva + '")';
                              end;
                            end;
                          else Assert(False);
                        end;
                      end;

                      Inc(ExistingObjectCount);
                      if (comboObjectNameMethod.ItemIndex = 0) or (comboNameAttribute.Text = '') then
                      begin
                        AScreenObject.Name := Root + IntToStr(ExistingObjectCount);
                      end
                      else
                      begin
                        AScreenObjectName := TScreenObject.ValidName(
                          xbShapeDataBase.GetFieldStr(
                          AnsiString(comboNameAttribute.Text)));
                        if ObjectNames.IndexOf(AScreenObjectName) < 0 then
                        begin
                          AScreenObject.Name := AScreenObjectName;
                        end
                        else
                        begin
                          AScreenObject.Name := AScreenObjectName
                            + '_' + IntToStr(ExistingObjectCount);
                        end;
                        ObjectNames.Add(AScreenObject.Name);
                      end;
                      AScreenObject.PositionLocked := cbLockObject.Checked;
                      AScreenObject.SetValuesOfEnclosedCells :=
                        cbEnclosedCells.Enabled and cbEnclosedCells.Checked;
                      AScreenObject.SetValuesOfIntersectedCells :=
                        cbIntersectedCells.Checked;
                      AScreenObject.SetValuesByInterpolation :=
                        cbInterpolation.Checked;
                      AScreenObject.EvaluatedAt := EvalAt;
                      AScreenObject.Selected := comboVisibility.ItemIndex = 0;
                      AScreenObject.Visible := comboVisibility.ItemIndex <= 1;
                      if FCombinedObjects then
                      begin
                        for DataSetIndex := 0 to DataSets.Count - 1 do
                        begin
                          DataSet := DataSets[DataSetIndex];
                          if DataSet = nil then
                          begin
                            MultiValueList.Add(nil);
                          end
                          else
                          begin
                            Item := AScreenObject.
                              ImportedValues.Add as TValueArrayItem;
                            Item.Name := DataSet.Name;
                            Item.Values.DataType := DataSet.DataType;
                            Item.Values.Count := FGeometryFile.Count;
                            MultiValueList.Add(Item.Values);
                          end;
                        end;
                      end;
                    end;
                    try
                      if not FCombinedObjects then
                      begin
                        // Why is this needed at all?
                        AScreenObject.Invalidate;
                      end;

                      if FDataBaseFileName = '' then
                      begin
                        if FCombinedObjects then
                        begin
                          if ScreenObjectList.Count = 0 then
                          begin
                            ScreenObjectList.Add(AScreenObject);
                          end;
                        end
                        else
                        begin
                          ScreenObjectList.Add(AScreenObject);
                        end;
                      end
                      else
                      begin
                        Assert(DataSets.Count = Variables.Count);
                        Assert(DataSets.Count = FieldNames.Count);

                        for DataSetIndex := 0 to DataSets.Count - 1 do
                        begin
                          DataSet := DataSets[DataSetIndex];
                          if not FCombinedObjects or (Index = 0) then
                          begin
                            if DataSet <> nil then
                            begin
                              Position := AScreenObject.AddDataSet(DataSet);
                              Assert(Position >= 0);
                            end
                            else
                            begin
                              Position := -1;
                            end;
                          end
                          else
                          begin
                            Position := AScreenObject.IndexOfDataSet(DataSet);
                          end;

                          Variable := Variables[DataSetIndex];
                          ValueList := nil;
                          if FCombinedObjects then
                          begin
                            ValueList := MultiValueList[DataSetIndex];
                          end;
                          case Variable.ResultType of
                            rdtDouble:
                              begin
                                RealVariable := Variables[DataSetIndex];
                                ARealFieldName := RealFieldNames[DataSetIndex];
                                if xbShapeDataBase.GetFieldNumberFromName(AnsiString(ARealFieldName)) >= 1 then
                                begin
                                  RealVariable.Value :=
                                   xbShapeDataBase.GetFieldNum(AnsiString(ARealFieldName));
                                end
                                else
                                begin
                                  RealVariable.Value := GetValueFromCsv(ARealFieldName, Dummy);
                                end;
                                if DataSet <> nil then
                                begin
                                  if FCombinedObjects then
                                  begin
                                    ValueList.RealValues[Index-DeleteCount+AddCount]
                                      := RealVariable.Value;
                                  end
                                  else
                                  begin
                                    AScreenObject.DataSetFormulas[Position]
                                      := FortranFloatToStr(RealVariable.Value);
                                  end;
                                end;
                              end;
                            rdtInteger:
                              begin
                                IntVariable := Variables[DataSetIndex];
                                IntVariable.Value :=
                                  xbShapeDataBase.GetFieldInt(AnsiString(
                                  RealFieldNames[DataSetIndex]));
                                if DataSet <> nil then
                                begin
                                  if FCombinedObjects then
                                  begin
                                    if DataSet.DataType = rdtDouble then
                                    begin
                                      ValueList.RealValues[Index-DeleteCount+AddCount]
                                        := IntVariable.Value;
                                    end
                                    else
                                    begin
                                      ValueList.IntValues[Index-DeleteCount+AddCount]
                                        := IntVariable.Value;
                                    end;
                                  end
                                  else
                                  begin
                                    AScreenObject.DataSetFormulas[Position]
                                      := IntToStr(IntVariable.Value);
                                  end;
                                end;
                              end;
                            rdtBoolean:
                              begin
                                BooleanVariable := Variables[DataSetIndex];
                                Value :=
                                  xbShapeDataBase.GetFieldStr(AnsiString(
                                  RealFieldNames[DataSetIndex]));
                                if (Value = 'Y') or (Value = 'y')
                                  or (Value = 'T') or (Value = 't') then
                                begin
                                  BooleanVariable.Value := True;
                                  if DataSet <> nil then
                                  begin
                                    if FCombinedObjects then
                                    begin
                                      ValueList.BooleanValues[
                                        Index-DeleteCount+AddCount] := True;
                                    end
                                    else
                                    begin
                                      AScreenObject.DataSetFormulas[
                                        Position] := 'True';
                                    end;
                                  end;
                                end
                                else
                                begin
                                  BooleanVariable.Value := False;
                                  if DataSet <> nil then
                                  begin
                                    if FCombinedObjects then
                                    begin
                                      ValueList.BooleanValues[
                                        Index-DeleteCount+AddCount] := False;
                                    end
                                    else
                                    begin
                                      AScreenObject.DataSetFormulas[Position]
                                        := 'False';
                                    end;
                                  end;
                                end;
                              end;
                            rdtString:
                              begin
                                StringVariable := Variables[DataSetIndex];
                                StringFormula := xbShapeDataBase.GetFieldStr(
                                  AnsiString(RealFieldNames[DataSetIndex]));
                                StringFormula := StringReplace(StringFormula,
                                  '"', '''', [rfReplaceAll]);
                                StringVariable.Value := Trim(StringFormula);
                                if DataSet <> nil then
                                begin
                                  if FCombinedObjects then
                                  begin
                                    ValueList.StringValues[Index-DeleteCount+AddCount]
                                      := StringVariable.Value;
                                  end
                                  else
                                  begin
                                    AScreenObject.DataSetFormulas[Position] := '"'
                                      + StringVariable.Value + '"';
                                  end;
                                end;
                              end;
                          else
                            Assert(False);
                          end;
                        end;
                        FMinXVar.Value := ShapeObject.BoundingBox.XMin;
                        FMinYVar.Value := ShapeObject.BoundingBox.YMin;
                        FMaxXVar.Value := ShapeObject.BoundingBox.XMax;
                        FMaxYVar.Value := ShapeObject.BoundingBox.YMax;
                        if FMinZVar <> nil then
                        begin
                          FMinZVar.Value := ShapeObject.ZMin;
                        end;
                        if FMaxZVar <> nil then
                        begin
                          FMaxZVar.Value := ShapeObject.ZMax;
                        end;

                        ImportCriterionExpression.Evaluate;
                        if ImportCriterionExpression.BooleanResult then
                        begin
                          AssignBoundary(AScreenObject);
                          if FCombinedObjects then
                          begin
                            if ScreenObjectList.Count = 0 then
                            begin
                              ScreenObjectList.Add(AScreenObject);
                            end;
                          end
                          else
                          begin
                            ScreenObjectList.Add(AScreenObject);
                          end;
                        end
                        else
                        begin
                          if FCombinedObjects then
                          begin
                            Inc(DeleteCount);
                          end
                          else
                          begin
                            AScreenObject.Free;
                          end;
                          xbShapeDataBase.GotoNext;
                          frmProgressMM.StepIt;
                          Application.ProcessMessages;
                          if not frmProgressMM.ShouldContinue then
                          begin
                            Exit;
                          end;
                          Continue;
                        end;
                      end;

                      case AScreenObject.ElevationCount of
                        ecZero: ; // do nothing
                        ecOne:
                          begin
                            if ZExpression = nil then
                            begin
                              AScreenObject.ElevationFormula := ElevFormula;
                            end
                            else
                            begin
                              ZExpression.Evaluate;
                              if FCombinedObjects then
                              begin
                                AScreenObject.ImportedSectionElevations.Add
                                  (ZExpression.DoubleResult);
                              end
                              else
                              begin
                                AFormula := ZExpression.Decompile;
                                if ZExpression.ResultType = rdtString then
                                begin
                                  AFormula := Copy(AFormula, 2, Length(AFormula)-2);
                                end;
                                AScreenObject.ElevationFormula
                                  := AFormula;
                              end;
                            end;
                          end;
                        ecTwo:
                          begin
                            if HighZExpression = nil then
                            begin
                              AScreenObject.HigherElevationFormula
                                  := HighElevFormula;
                            end
                            else
                            begin
                              HighZExpression.Evaluate;
                              if FCombinedObjects then
                              begin
                                AScreenObject.ImportedHigherSectionElevations.Add
                                  (HighZExpression.DoubleResult);
                              end
                              else
                              begin
                                AFormula := HighZExpression.Decompile;
                                if HighZExpression.ResultType = rdtString then
                                begin
                                  AFormula := Copy(AFormula, 2, Length(AFormula)-2);
                                end;
                                AScreenObject.HigherElevationFormula
                                  := AFormula;
                              end;
                            end;
                            if LowZExpression = nil then
                            begin
                              AScreenObject.LowerElevationFormula
                                  := LowElevFormula;
                            end
                            else
                            begin
                              LowZExpression.Evaluate;
                              if FCombinedObjects then
                              begin
                                AScreenObject.ImportedLowerSectionElevations.Add
                                  (LowZExpression.DoubleResult);
                              end
                              else
                              begin
                                AFormula := LowZExpression.Decompile;
                                if LowZExpression.ResultType = rdtString then
                                begin
                                  AFormula := Copy(AFormula, 2, Length(AFormula)-2);
                                end;
                                AScreenObject.LowerElevationFormula
                                  := AFormula;
                              end;
                            end;
                          end;
                        else Assert(False)
                      end;


                      if not FCombinedObjects then
//                      begin
//                        if Index = 0 then
//                        begin
//                          AScreenObject.Capacity := ShapeObject.FNumPoints;
//                        end
//                        else
//                        begin
//                          AScreenObject.Capacity := AScreenObject.Capacity
//                            + ShapeObject.FNumPoints;
//                        end;
//                      end
//                      else
                      begin
                        AScreenObject.Capacity := ShapeObject.FNumPoints;
                      end;
                      SectionIndex := 0;
                      NextStart := -1;
                      if (SectionIndex < Length(ShapeObject.FParts)) then
                      begin
                        NextStart := ShapeObject.FParts[SectionIndex];
                      end;
                      AScreenObject.BeginUpdate;
                      try
                        PriorPoint.x := 0;
                        PriorPoint.y := 0;
                        if FCombinedObjects then
                        begin
                          if ZValues = nil then
                          begin
                            SetLength(ZValues, FGeometryFile.NumberOfPoints);
                            SetLength(Distances, FGeometryFile.NumberOfPoints);
                          end;
                        end
                        else
                        begin
                          SetLength(ZValues, ShapeObject.FNumPoints);
                          SetLength(Distances, ShapeObject.FNumPoints);
                        end;
                        for PointIndex := 0 to ShapeObject.FNumPoints - 1 do
                        begin
                          Inc(CombinedPointIndex);
                          NewSection := (PointIndex = NextStart) or (PointIndex = 0);
                          if PointIndex = NextStart then
                          begin
                            Inc(SectionIndex);
                            if (SectionIndex < Length(ShapeObject.FParts)) then
                            begin
                              NextStart := ShapeObject.FParts[SectionIndex];
                            end;
                          end;
                          ShapePoint := ShapeObject.FPoints[PointIndex];
                          if cbCoordinateConversion.Checked then
                          begin
                            X := ShapePoint.X;
                            Y := ShapePoint.Y;
                            if X > CentralMeridianDegrees + 180 then
                            begin
                              X := X - 360;
                            end
                            else if X < CentralMeridianDegrees - 180 then
                            begin
                              X := X + 360;
                            end;

                            ConvertToUTM(Y / 180 * Pi, X / 180 * Pi,
                              CentralMeridian, X, Y);
                            PointRecord.X := X;
                            PointRecord.Y := Y;
                            if NewSection or (PriorPoint.x <> PointRecord.x)
                              or (PriorPoint.y <> PointRecord.y) then
                            begin
                              if ConvertUnits then
                              begin
                                AScreenObject.AddPoint(ConvertPoint2D(PointRecord, FromUnits, ToUnits), NewSection);
                              end
                              else
                              begin
                                AScreenObject.AddPoint(PointRecord, NewSection);
                              end;
                            end;
                          end
                          else
                          begin
                            PointRecord := ConvertPoint(ShapePoint);
                            if NewSection or (PriorPoint.x <> PointRecord.x)
                              or (PriorPoint.y <> PointRecord.y) then
                            begin
                              if ConvertUnits then
                              begin
                                AScreenObject.AddPoint(ConvertPoint2D(PointRecord, FromUnits, ToUnits), NewSection);
                              end
                              else
                              begin
                                AScreenObject.AddPoint(PointRecord, NewSection);
                              end;
                            end;
  //                          PriorPoint := PointRecord;
                          end;

                          if (cbImportZ.Checked
                            and (Length(ShapeObject.FZArray) = ShapeObject.FNumPoints))
                            or (cbImportMeasured.Checked
                            and (Length(ShapeObject.FMArray) = ShapeObject.FNumPoints)) then
                          begin
                            PointValueItem := AScreenObject.PointPositionValues.Add;

                            if FCombinedObjects then
                            begin
                              PointValueItem.Position := CombinedPointIndex;
                            end
                            else
                            begin
                              PointValueItem.Position := PointIndex;
                            end;


                            if cbImportMeasured.Checked
                              and (Length(ShapeObject.FMArray) = ShapeObject.FNumPoints) then
                            begin
                              if ShapeObject.FMArray[0]>= -1e38 then
                              begin
                                PointValue := PointValueItem.Values.Add;
                                PointValue.Name := 'Measured';
                                PointValue.Value := ShapeObject.FMArray[PointIndex];
                              end;
                            end;
                            if cbImportZ.Checked
                              and (Length(ShapeObject.FZArray) = ShapeObject.FNumPoints) then
                            begin
                              PointValue := PointValueItem.Values.Add;
                              PointValue.Name := 'Z';
                              PointValue.Value := ShapeObject.FZArray[PointIndex];
                              ZValues[PointIndex] := ShapeObject.FZArray[PointIndex];
                              if PointIndex = 0 then
                              begin
                                Distances[PointIndex] := 0
                              end
                              else
                              begin
                                Distances[PointIndex] := Distances[PointIndex-1]
                                  + Distance(PointRecord,PriorPoint);
                              end;
                            end;
                          end;
                          PriorPoint := PointRecord;


                          if FCombinedObjects and NewSection and (PointIndex > 0) then
                          begin
                            Inc(AddCount);
                            for ValueIndex := 0 to MultiValueList.Count - 1 do
                            begin
                              ValueList := MultiValueList[ValueIndex];
                              ValueList.Count := ValueList.Count + 1;
                              case ValueList.DataType of
                                rdtDouble: ValueList.
                                  RealValues[Index-DeleteCount+AddCount]
                                  := ValueList.
                                  RealValues[Index-DeleteCount+AddCount-1];
                                rdtInteger:  ValueList.
                                  IntValues[Index-DeleteCount+AddCount]
                                  := ValueList.
                                  IntValues[Index-DeleteCount+AddCount-1];
                                rdtBoolean:  ValueList.
                                  BooleanValues[Index-DeleteCount+AddCount]
                                  := ValueList.
                                  BooleanValues[Index-DeleteCount+AddCount-1];
                                rdtString:  ValueList.
                                  StringValues[Index-DeleteCount+AddCount]
                                  := ValueList.
                                  StringValues[Index-DeleteCount+AddCount-1];
                                else Assert(False);
                              end;
                            end;
                          end;
                          if (PointIndex mod 100) = 99 then
                          begin
                            frmProgressMM.ProgressLabelCaption := Format(
                              StrObject0dOutOf,
                              [Index+1, frmProgressMM.pbProgress.Max,
                              PointIndex + 1, ShapeObject.FNumPoints]);
                            Application.ProcessMessages;
                          end;
                        end;
                        if cbImportZ.Checked then
                        begin
                          SetLength(Slopes, ShapeObject.FNumPoints);
                          ZValueList := TRealList.Create;
                          StartList := TIntegerList.Create;
                          EndList := TIntegerList.Create;
                          MidList := TIntegerList.Create;
                          try
                            Z := ZValues[0];
                            ZValueList.Add(Z);
                            StartList.Add(0);
                            for PointIndex := 1 to ShapeObject.FNumPoints - 1 do
                            begin
                              if ZValues[PointIndex] <> Z then
                              begin
                                Z := ZValues[PointIndex];
                                ZValueList.Add(Z);
                                StartList.Add(PointIndex);
                                EndList.Add(PointIndex-1);
                              end;
                            end;
                            EndList.Add(ShapeObject.FNumPoints - 1);

                            for PointIndex := 0 to ShapeObject.FNumPoints - 1 do
                            begin
                              Slopes[PointIndex] := 0;
                            end;
                            if ZValueList.Count > 1 then
                            begin
                              for ListIndex := 0 to StartList.Count - 1 do
                              begin
                                if ListIndex = 0 then
                                begin
                                  MidList.Add(0);
                                end
                                else  if ListIndex = StartList.Count - 1 then
                                begin
                                  MidList.Add(ShapeObject.FNumPoints - 1);
                                end
                                else
                                begin
                                  MidList.Add((StartList[ListIndex] + EndList[ListIndex]) div 2);
                                end;
                              end;
                              for ListIndex := 1 to MidList.Count - 1 do
                              begin
                                StartIndex := MidList[ListIndex-1];
                                EndIndex := MidList[ListIndex];
                                if (StartIndex = EndIndex)
                                  or (Distances[EndIndex] = Distances[StartIndex]) then
                                begin
                                  for PointIndex := StartIndex to EndIndex do
                                  begin
                                    Slopes[PointIndex] := 0;
                                  end;
                                end
                                else
                                begin
                                  ASlope :=
                                    (ZValues[StartIndex] - ZValues[EndIndex])
                                    /(Distances[EndIndex]-Distances[StartIndex]);
                                  for PointIndex := StartIndex to EndIndex do
                                  begin
                                    Slopes[PointIndex] := ASlope
                                  end;
                                end;
                              end;
                            end;
                            for PointIndex := 0 to ShapeObject.FNumPoints - 1 do
                            begin
                              PointValueItem := AScreenObject.PointPositionValues[PointIndex];
                              PointValue := PointValueItem.Values.Add;
                              PointValue.Name := 'Slope';
                              PointValue.Value := Slopes[PointIndex];
                            end;
                          finally
                            ZValueList.Free;
                            StartList.Free;
                            EndList.Free;
                            MidList.Free;
                          end;
                        end;
                      finally
                        AScreenObject.EndUpdate;
                      end;
                    except
                      on E: EScreenObjectError do
                      begin
                        ScreenObjectList.Remove(AScreenObject);
//                        AScreenObject.Free;
                        InvalidObjectNumbers.Add(Index+1);
                        if FCombinedObjects then
                        begin
                          break
                        end;
                      end;
                      on E: ERbwParserError do
                      begin
                        ScreenObjectList.Remove(AScreenObject);
//                        AScreenObject.Free;
                        if InvalidFormulaNumbers = nil then
                        begin
                          InvalidFormulaNumbers := TIntegerList.Create;
                        end;
                        InvalidFormulaNumbers.Add(Index+1);
                        if FCombinedObjects then
                        begin
                          break
                        end;
                      end;
                    end;
                    if FDataBaseFileName <> '' then
                    begin
                      xbShapeDataBase.GotoNext;
                    end;
                    frmProgressMM.StepIt;
                    Application.ProcessMessages;
                    if not frmProgressMM.ShouldContinue then
                    begin
                      Exit;
                    end;

                  end;
                  if FCombinedObjects then
                  begin
                    AScreenObject.EndUpdate;

                    Assert(ScreenObjectList.Count <= 1);
                    for ValueIndex := 0 to MultiValueList.Count - 1 do
                    begin
                      ValueList := MultiValueList[ValueIndex];
                      if ValueList <> nil then
                      begin
                        ValueList.Count := FGeometryFile.Count
                          - DeleteCount + AddCount;
                        ValueList.CacheData;
                      end;
                    end;

                    frmProgressMM.Caption := StrAssigningFormulas;
                    frmProgressMM.pbProgress.Position := 0;

                    if ScreenObjectList.Count = 1 then
                    begin
                      AScreenObject := ScreenObjectList[0];

                      frmProgressMM.pbProgress.Max := DataSets.Count;
                      frmProgressMM.ProgressLabelCaption := Format(Str0OutOf0d,
                        [frmProgressMM.pbProgress.Max]);
                      frmProgressMM.Prefix := StrFormula;
                      frmProgressMM.PopupParent := self;
                      frmProgressMM.Show;
                      Application.ProcessMessages;

                      for DataSetIndex := 0 to DataSets.Count - 1 do
                      begin
                        DataSet := DataSets[DataSetIndex];
                        if DataSet = nil then
                        begin
                          frmProgressMM.pbProgress.Max :=
                            frmProgressMM.pbProgress.Max -1;
                        end;
                      end;

                      for DataSetIndex := 0 to DataSets.Count - 1 do
                      begin
                        DataSet := DataSets[DataSetIndex];
                        if DataSet = nil then
                        begin
                          Continue;
                        end;
                        Position := AScreenObject.IndexOfDataSet(DataSet);
                        Assert(Position >= 0);
                        case DataSet.DataType of
                          rdtDouble: Formula := rsObjectImportedValuesR
                            + '("' + DataSet.Name + '")';
                          rdtInteger: Formula := rsObjectImportedValuesI
                            + '("' + DataSet.Name + '")';
                          rdtBoolean: Formula := rsObjectImportedValuesB
                            + '("' + DataSet.Name + '")';
                          rdtString: Formula := rsObjectImportedValuesT
                            + '("' + DataSet.Name + '")';
                          else Assert(False);
                        end;
                        AScreenObject.DataSetFormulas[Position] := Formula;
                        frmProgressMM.StepIt;
                        Application.ProcessMessages;
                        if not frmProgressMM.ShouldContinue then
                        begin
                          Exit;
                        end;
                      end;
                    end;
                  end;
                  for Index := 0 to ScreenObjectList.Count - 1 do
                  begin
                    AScreenObject := ScreenObjectList[Index];
                    AScreenObject.PointPositionValues.RemoveUnusedItems;
                  end;
                  if ScreenObjectList.Count > 0 then
                  begin
                    Undo.StoreNewScreenObjects(ScreenObjectList);
                    Undo.StoreNewDataSets(NewDataSets);
                    Undo.StoreChangedDataSetProperties(OldProperties,
                      NewProperties);
                    Undo.ScreenObjectsToDelete.Assign(
                      FScreenObjectsToDelete);
                     
                    frmGoPhast.UndoStack.Submit(Undo);
                    (ScreenObjectList as TObjectList).OwnsObjects := False;
                    Undo := nil;

                    frmGoPhast.PhastModel.AddFileToArchive(FGeometryFileName);
                    frmGoPhast.PhastModel.AddFileToArchive(FIndexFileName);
                    frmGoPhast.PhastModel.AddFileToArchive(FDataBaseFileName);
                    OptionalExtensions := TStringList.Create;
                    try
                      OptionalExtensions.Add('.apr');
                      OptionalExtensions.Add('.sbn');
                      OptionalExtensions.Add('.sbx');
                      OptionalExtensions.Add('.fbn');
                      OptionalExtensions.Add('.fbx');
                      OptionalExtensions.Add('.ain');
                      OptionalExtensions.Add('.aih');
                      OptionalExtensions.Add('.ixs');
                      OptionalExtensions.Add('.mxs');
                      OptionalExtensions.Add('.prj');
                      OptionalExtensions.Add('.atx');
                      OptionalExtensions.Add('.qix');
                      OptionalExtensions.Add('.shp.xml');
                      for Index := 0 to OptionalExtensions.Count - 1 do
                      begin
                        OptionalFileName := ChangeFileExt(FGeometryFileName,
                          OptionalExtensions[Index]);
                        if FileExists(OptionalFileName) then
                        begin
                          frmGoPhast.PhastModel.AddFileToArchive(OptionalFileName);
                        end;
                      end;
                    finally
                      OptionalExtensions.Free;
                    end;
                    for FileIndex := 1 to frameCSV.seNumber.AsInteger - 1 do
                    begin
                      if TFile.Exists(frameCSV.Grid.Cells[0,FileIndex]) then
                      begin
                        frmGoPhast.PhastModel.AddFileToArchive(frameCSV.Grid.Cells[0,FileIndex]);
                      end;
                    end;
                  end
                  else
                  begin
                    FreeAndNil(Undo);
                  end;
                except on E: Exception do
                  begin
                    MessageDlg(E.Message, mtError, [mbOK], 0);
                    FreeAndNil(Undo);
                  end;
                end;
              finally
                ScreenObjectList.Free;
                frmGoPhast.PhastModel.EndScreenObjectUpdate;
                FreeAndNil(Undo);
              end;
            end;
          finally
            Variables.Free;
            DataSets.Free;
            FieldNames.Free;
            RealFieldNames.Free;
            frmProgressMM.Hide;
          end;
        finally
          frmGoPhast.CanDraw := True;
          if InvalidObjectNumbers.Count > 0 then
          begin
            Beep;
            if MessageDlg(Format(StrDObjectsWereInva, [InvalidObjectNumbers.Count]),
              mtWarning, [mbYes, mbNo], 0)= mrYes then
            begin
              ErrorString := '';
              for Index := 0 to InvalidObjectNumbers.Count -1 do
              begin
                ErrorString := ErrorString
                  + IntToStr(InvalidObjectNumbers[Index]) + ', ';
              end;
              SetLength(ErrorString, Length(ErrorString) -2);

              MessageDlg(ErrorString, mtInformation, [mbOK], 0);
            end;
          end;
          if InvalidFormulaNumbers <> nil then
          begin
            Beep;
            if MessageDlg(Format(StrDObjectsHadAttri, [InvalidFormulaNumbers.Count]),
              mtWarning, [mbYes, mbNo], 0)= mrYes then
            begin
              ErrorString := '';
              for Index := 0 to InvalidFormulaNumbers.Count -1 do
              begin
                ErrorString := ErrorString
                  + IntToStr(InvalidFormulaNumbers[Index]) + ', ';
              end;
              SetLength(ErrorString, Length(ErrorString) -2);

              MessageDlg(ErrorString, mtInformation, [mbOK], 0);
            end;
          end;
        end;
      finally
        InvalidObjectNumbers.Free;
        InvalidFormulaNumbers.Free;
        MultiValueList.Free;
        if not frmProgressMM.ShouldContinue then
        begin
          for DataSetIndex := NewDataSets.Count - 1 downto 0 do
          begin
            ADataArray := NewDataSets[DataSetIndex];
            frmGoPhast.PhastModel.DataArrayManager.RemoveDataSet(ADataArray);
          end;
        end;
        NewDataSets.Free;
        NewProperties.Free;
        OldProperties.Free;
        GlobalDecompileType := dcNormal;
        frmGoPhast.ChangingSelection := False;
      end;
      for InvalidParametersIndex := 0 to FInvalidParameterNames.Count - 1 do
      begin
        frmErrorsAndWarnings.AddWarning(frmGoPhast.PhastModel, StrWarningRoot,
          FInvalidParameterNames[InvalidParametersIndex]);
      end;
      if FInvalidParameterNames.Count > 0 then
      begin
        frmErrorsAndWarnings.ShowAfterDelay;
      end;
    finally
      FInvalidParameterNames.Free;
      ObjectNames.Free;
    end;
  except on E: EXBaseException do
    begin
      Beep;
      MessageDlg(Format(StrThereWasAnErrorP, [E.Message]), mtError, [mbOK], 0);
    end;
  end;
end;


procedure TfrmImportShapefile.seWellIntervalsChange(Sender: TObject);
begin
  inherited;
  dgWellElevations.RowCount := seWellIntervals.AsInteger + 1;
end;

Procedure TfrmImportShapefile.HowTreatDuplicateNames;
var
  DuplicateNames: TStringList;
  RowIndex: Integer;
  AName: string;
begin
  dgFields.Handle;
  FDuplicateTreatment := drNew;
  DuplicateNames := TStringList.Create;
  try
    for RowIndex := 1 to dgFields.RowCount -1 do
    begin
      if dgFields.Checked[Ord(fgcImport),RowIndex]
        and (dgFields.Cells[Ord(fgcDataSet), RowIndex] = rsNewDataSet) then
      begin
        AName := dgFields.Cells[Ord(fgcAttributes),RowIndex];
        if frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(AName) <> nil then
        begin
          DuplicateNames.Add(AName)
        end;
      end;
    end;
    if DuplicateNames.Count > 0 then
    begin
      Beep;
      FDuplicateTreatment := DupNameTreatment(DuplicateNames);
    end;
  finally
    DuplicateNames.Free;
  end;
end;

procedure TfrmImportShapefile.btnOKClick(Sender: TObject);
var
  LayerBoundaries: TStringList;
  index: Integer;
  LayerGroup: TLayerGroup;
begin
//  OutputDebugString('SAMPLING ON');
  inherited;
  edImportCriterionExit(nil);
  if not btnOk.Enabled then
  begin
    ModalResult := mrNone;
    Exit;
  end;
  case rgElevationCount.ItemIndex of
    0:
      begin

      end;
    1:
      begin
        edZExit(nil);
        if not btnOk.Enabled then
        begin
          ModalResult := mrNone;
          Exit;
        end;
      end;
    2:
      begin
        edHighZExit(nil);
        if not btnOk.Enabled then
        begin
          ModalResult := mrNone;
          Exit;
        end;
        edLowZExit(nil);
        if not btnOk.Enabled then
        begin
          ModalResult := mrNone;
          Exit;
        end;
      end;
    else Assert(False);
  end;

  if (rgElevationCount.ItemIndex <> 0)
    and (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    LayerBoundaries := TStringList.Create;
    try
      for index := 0 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
      begin
        LayerGroup := frmGoPhast.PhastModel.LayerStructure[index];
        LayerBoundaries.Add(LayerGroup.DataArrayName);
      end;
      for index := 1 to dgFields.RowCount - 1 do
      begin
        if dgFields.Checked[Ord(fgcImport), index] then
        begin
          if LayerBoundaries.IndexOf(
            dgFields.Cells[Ord(fgcDataSet), index]) >= 0 then
          begin
            Beep;
            MessageDlg(StrYouMustChangeThe, mtError, [mbOK], 0);
            ModalResult := mrNone;
            Exit;
          end;
        end;
      end;
    finally
      LayerBoundaries.Free;
    end;
  end;

  HowTreatDuplicateNames;

  if FDuplicateTreatment = drCancel then
  begin
    ModalResult := mrNone;
    Exit;
  end;

  Hide;
  SetData;
  // ModalResult can not be set before calling SetData because SetData
  // may display a dialog box.  If ModalResult is already set, the
  // application will hang.
  ModalResult := mrOK;
//  OutputDebugString('SAMPLING OFF');
end;

procedure TfrmImportShapefile.GetInterpolators(const ARow: integer);
var
  List: TList;
  Index: integer;
  AType: TInterpolatorType;
  DataType: TRbwDataType;
  DataArray: TDataArray;
begin
  Assert(SizeOf(TObject) = SizeOf(TInterpolatorType));
  List := TList.Create;
  try
    AddInterpolatorsToList(List);
    dgFields.Columns[Ord(fgcInterpolator)].PickList.Clear;
    DataType := FFieldTypes[ARow - 1];
    if dgFields.Cells[Ord(fgcDataSet),ARow] <> rsNewDataSet then
    begin
      DataArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(
        dgFields.Cells[Ord(fgcDataSet),ARow]);
      if DataArray <> nil then
      begin
        DataType := DataArray.DataType;
      end;
    end;
    dgFields.Columns[Ord(fgcInterpolator)].PickList.Add(StrNone);
    for Index := 0 to List.Count - 1 do
    begin
      AType := List[Index];
      // We aren't checking the orientation here because Shapefiles
      // will always be imported to the top view.
      if DataType in AType.ValidReturnTypes then
      begin
        dgFields.Columns[Ord(fgcInterpolator)].PickList.AddObject(
          AType.InterpolatorName, TObject(AType));
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TfrmImportShapefile.GetDataSets(const ARow: integer);
var
  EvalAt: TEvaluatedAt;
  DataSet: TDataArray;
  Index: integer;
  PickList: TStrings;
  DataArrayManager: TDataArrayManager;
begin
  EvalAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
  PickList := dgFields.Columns[Ord(fgcDataSet)].PickList;

  PickList.Clear;
  PickList.AddObject(rsNewDataSet, nil);
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  for Index := 0 to DataArrayManager.DataSetCount - 1 do
  begin
    DataSet := DataArrayManager.DataSets[Index];
    if (DataSet.EvaluatedAt = EvalAt)
      and (not (dcFormula in DataSet.Lock))
      and DataSet.Visible
      and DataArrayOrientationOK(DataSet)
      and ((DataSet.DataType = FFieldTypes[ARow - 1])
      or ((DataSet.DataType = rdtDouble)
      and (FFieldTypes[ARow - 1] = rdtInteger))) then
    begin
      PickList.AddObject(DataSet.Name, DataSet);
    end;
  end;

  if PickList.IndexOf(
    dgFields.Cells[Ord(fgcDataSet), ARow]) < 0 then
  begin
    dgFields.Cells[Ord(fgcDataSet), ARow] := rsNewDataSet;
  end;
end;

procedure TfrmImportShapefile.SetCheckBoxCaptions;
var
  NodeElemString: string;
begin
  NodeElemString := EvalAtToString(TEvaluatedAt(rgEvaluatedAt.ItemIndex),
       frmGoPhast.ModelSelection, True, False);
  cbEnclosedCells.Caption := rsSetValueOfEnclosed + NodeElemString;
  cbIntersectedCells.Caption := rsSetValueOfIntersected + NodeElemString;
  cbInterpolation.Caption := rsSetValueOf + NodeElemString + rsByInterpolation;
//  case rgEvaluatedAt.ItemIndex of
//    0: // elements
//      begin
//        cbEnclosedCells.Caption := rsSetValueOfEnclosedElements;
//        cbIntersectedCells.Caption := rsSetValueOfIntersectedElements;
//        cbInterpolation.Caption := rsSetValueOfElementsByInterpolation;
//      end;
//    1: // cells
//      begin
//        cbEnclosedCells.Caption := rsSetValueOfEnclosedNodes;
//        cbIntersectedCells.Caption := rsSetValueOfIntersectedNodes;
//        cbInterpolation.Caption := rsSetValueOfNodesByInterpolation;
//      end;
//  else
//    Assert(False);
//  end;
end;

procedure TfrmImportShapefile.rdgBoundaryConditionsDistributeTextProgress(
  Sender: TObject; Position, Max: Integer);
begin
  inherited;
  seBoundaryTimeCount.AsInteger := rdgBoundaryConditions.RowCount -1;
end;

procedure TfrmImportShapefile.rgElevationCountClick(Sender: TObject);
begin
  inherited;
  edZ.Enabled := rgElevationCount.ItemIndex = 1;
  btnZ.Enabled := edZ.Enabled;
  lblZ.Enabled := edZ.Enabled;

  edHighZ.Enabled := rgElevationCount.ItemIndex = 2;
  btnHighZ.Enabled := edHighZ.Enabled;
  lblHighZ.Enabled := edHighZ.Enabled;

  edLowZ.Enabled := rgElevationCount.ItemIndex = 2;
  btnLowZ.Enabled := edLowZ.Enabled;
  lblLowZ.Enabled := edLowZ.Enabled;
end;

procedure TfrmImportShapefile.rgEvaluatedAtClick(Sender: TObject);
var
  Index: integer;
begin
  inherited;
  for Index := 1 to dgFields.RowCount - 1 do
  begin
    GetDataSets(Index);
  end;
  SetCheckBoxCaptions;
  EnableFeatureImport;
end;

procedure TfrmImportShapefile.seBoundaryTimeCountChange(Sender: TObject);
begin
  inherited;
  rdgBoundaryConditions.RowCount := seBoundaryTimeCount.AsInteger + 1;
end;

procedure TfrmImportShapefile.dgFieldsColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutControls(dgFields, cbSelect, nil, Ord(fgcImport), 3);
  LayoutControls(dgFields, comboInterpolaters, nil, Ord(fgcInterpolator));
end;

procedure TfrmImportShapefile.dgFieldsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  DrawCell: boolean;
begin
  inherited;
  if (ARow >= dgFields.FixedRows)
    and (TFieldGridColumns(ACol) in [fgcDataSet, fgcInterpolator]) then
  begin
    DrawCell := false;
    if not dgFields.Checked[Ord(fgcImport), ARow] then
    begin
      DrawCell := True;
      dgFields.Canvas.Brush.Color := Color;
    end;
//    if ACol = Ord(fgcInterpolator) then
//    begin
//      if dgFields.Cells[Ord(fgcDataSet), ARow] <> rsNewDataSet then
//      begin
//        DrawCell := True;
//        dgFields.Canvas.Brush.Color := Color;
//      end;
//    end;
    if DrawCell then
    begin
      dgFields.Canvas.FillRect(Rect);
      dgFields.Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 2,
        dgFields.Cells[ACol, ARow]);
    end;
  end;
end;

procedure TfrmImportShapefile.dgFieldsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  EnableMultiEditControl(dgFields, cbSelect, Ord(fgcImport));
  EnableMultiEditControl(dgFields, comboInterpolaters, Ord(fgcInterpolator));
end;

procedure TfrmImportShapefile.dgFieldsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if (ARow >= dgFields.FixedRows)
    and (TFieldGridColumns(ACol) in [fgcDataSet, fgcInterpolator]) then
  begin
    CanSelect := dgFields.Checked[Ord(fgcImport), ARow];
    if ACol = Ord(fgcInterpolator) then
    begin
      if CanSelect and not dgFields.Drawing then
      begin
        GetInterpolators(ARow);
      end;
    end;
    if (ACol = Ord(fgcDataSet)) and CanSelect and not dgFields.Drawing then
    begin
      GetDataSets(ARow);
    end;
  end;
end;

{ TUndoImportShapefile }
procedure TUndoImportShapefile.StoreChangedDataSetProperties(var OldProperties,
  NewProperties: TList);
begin
  FOldProperties.Free;
  FOldProperties := OldProperties;
  OldProperties := nil;
  FNewProperties.Free;
  FNewProperties := NewProperties;
  NewProperties := nil;
end;

procedure TUndoImportShapefile.StoreNewDataSets(var NewDataSets: TList);
begin
  FNewDataSets.Free;
  FNewDataSets := NewDataSets;
  NewDataSets := nil;
end;

constructor TUndoImportShapefile.Create;
begin
  inherited;
  FNewDataSets := TList.Create;
  FOldProperties := TList.Create;
  FNewProperties := TList.Create;
  FTopDataSet := nil;
  FThreeDDataSet := nil;
end;

function TUndoImportShapefile.Description: string;
begin
  result := StrImportShapeFile;
end;

destructor TUndoImportShapefile.Destroy;
begin
  inherited;
  FNewDataSets.Free;
  FOldProperties.Free;
  FNewProperties.Free;
end;

procedure TUndoImportShapefile.DoCommand;
var
  Index: integer;
  DataSet: TDataArray;
  Prop: TPhastDataSetStorage;
  ShouldInvalidate: array of boolean;
  DataArrayManager: TDataArrayManager;
begin
  frmGoPhast.CanDraw := False;
  try
    DisallowChildGridUpdates;
    UnDeleteNewScreenObjects;
    DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
    DataArrayManager.HandleAddedDataArrays(FNewDataSets);

    for Index := 0 to FNewDataSets.Count -1 do
    begin
      DataSet := FNewDataSets[Index];
      if FTopDataSet = DataSet then
      begin
        if frmGoPhast.PhastModel.Grid = nil then
        begin
          (frmGoPhast.PhastModel.Mesh as TSutraMesh3D).TopDataSet := DataSet;
        end
        else
        begin
          frmGoPhast.PhastModel.Grid.TopDataSet := DataSet;
        end;
      end;
      if FThreeDDataSet = DataSet then
      begin
        frmGoPhast.PhastModel.ThreeDDataSet := FThreeDDataSet;
      end;
    end;

    SetLength(ShouldInvalidate, FNewProperties.Count);
    for Index := 0 to FNewProperties.Count - 1 do
    begin
      Prop := FNewProperties[Index];
      Prop.AssignToDataSet(ShouldInvalidate[Index]);
      if ShouldInvalidate[Index] then
      begin
        Prop.InvalidateDataSet;
      end;
    end;
  finally
    frmGoPhast.CanDraw := True;
    AllowChildGridUpdates;
  end;
  FShouldUpdateShowHideObjects := True;
  inherited;
  WarnSfrLengthProblems(FNewScreenObjects);
  frmGoPhast.PhastModel.FormulaManager.Pack;
  UpdateFrmGridValue;
end;

procedure TUndoImportShapefile.Redo;
begin
  DoCommand;
  FShouldUpdateShowHideObjects := True;
  inherited;
  WarnSfrLengthProblems(FNewScreenObjects);
end;

procedure TUndoImportShapefile.Undo;
var
  Index: integer;
  DataSet: TDataArray;
  Prop: TPhastDataSetStorage;
  ShouldInvalidate: array of boolean;
  DataArrayManager: TDataArrayManager;
begin
  inherited;
  DisallowChildGridUpdates;
  frmGoPhast.CanDraw := False;
  try
    DeleteNewScreenObjects;
    FTopDataSet := nil;
    FThreeDDataSet := nil;
    DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
    for Index := 0 to FNewDataSets.Count - 1 do
    begin
      DataSet := FNewDataSets[Index];
      if frmGoPhast.PhastModel.TopDataSet = DataSet then
      begin
        FTopDataSet := DataSet;
        frmGoPhast.PhastModel.TopDataSet := nil;
      end;
      if frmGoPhast.PhastModel.ThreeDDataSet = DataSet then
      begin
        FThreeDDataSet := DataSet;
        frmGoPhast.PhastModel.ThreeDDataSet := nil;
      end;
    end;

    DataArrayManager.HandleDeletedDataArrays(FNewDataSets);
    SetLength(ShouldInvalidate, FOldProperties.Count);
    for Index := 0 to FOldProperties.Count - 1 do
    begin
      Prop := FOldProperties[Index];
      Prop.AssignToDataSet(ShouldInvalidate[Index]);
      if ShouldInvalidate[Index] then
      begin
        Prop.InvalidateDataSet;
      end;
    end;

  finally
    AllowChildGridUpdates;
    frmGoPhast.CanDraw := True;
  end;
  FShouldUpdateShowHideObjects := True;
  inherited;
//  UpdateDisplay;
  WarnSfrLengthProblems(FNewScreenObjects);
  frmGoPhast.PhastModel.FormulaManager.Pack;
  UpdateFrmGridValue;
end;

procedure TfrmImportShapefile.dgFieldsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
var
  DataSet: TDataArray;
  Index: integer;
begin
  inherited;
  if (ARow >= 1) and (ACol = Ord(fgcDataSet)) then
  begin
    if Value <> rsNewDataSet then
    begin
      Index := dgFields.Columns[Ord(fgcDataSet)].PickList.IndexOf(Value);
      Assert(Index > 0);
      DataSet := dgFields.Columns[Ord(fgcDataSet)].
        PickList.Objects[Index] as TDataArray;
      if DataSet.TwoDInterpolator = nil then
      begin
        GetInterpolators(ARow);
        dgFields.Cells[Ord(fgcInterpolator), ARow] :=
          dgFields.Columns[Ord(fgcInterpolator)].PickList[0];
      end
      else
      begin
        dgFields.Cells[Ord(fgcInterpolator), ARow] :=
          DataSet.TwoDInterpolator.InterpolatorName;
      end;
    end;
    dgFields.Invalidate;
  end;
end;

procedure TfrmImportShapefile.btnSelectClick(Sender: TObject);
var
  RowIndex: integer;
  SelectAll: boolean;
begin
  inherited;
  SelectAll := Sender = btnAll;
  for RowIndex := 1 to dgFields.RowCount - 1 do
  begin
    dgFields.Checked[Ord(fgcImport), RowIndex] := SelectAll;
  end;
end;

procedure TfrmImportShapefile.btnToggleClick(Sender: TObject);
var
  RowIndex: integer;
begin
  inherited;
  for RowIndex := 1 to dgFields.RowCount - 1 do
  begin
    dgFields.Checked[Ord(fgcImport), RowIndex] :=
      not dgFields.Checked[Ord(fgcImport), RowIndex];
  end;
end;

procedure TfrmImportShapefile.btnElevFormulaEdit(Sender: TObject);
var
  AFormula: string;
  Edit: TRbwEdit;
begin
  inherited;
  if Sender = btnZ then
  begin
    Edit := edZ;
  end
  else if Sender = btnHighZ then
  begin
    Edit := edHighZ;
  end
  else if Sender = btnLowZ then
  begin
    Edit := edLowZ;
  end
  else
  begin
    Edit := nil;
    Assert(false);
  end;
  rpShapeCompiler.ClearVariables;
  rpShapeCompiler.ClearExpressions;

//  with TfrmFormula.Create(self) do
  with frmFormula do
  begin
    try
      Initialize;
      IncludeGIS_Functions(TEvaluatedAt(rgEvaluatedAt.ItemIndex));
      PopupParent := self;
      DataSetGroupName := StrAttributes;
      // put the formula in the TfrmFormula.
      AFormula := Edit.Text;

      // register the appropriate variables with the
      // parser.
      CreateVariables(rbFormulaParser);
      CreateDataSetVariables(rbFormulaParser,
        TEValuatedAt(rgEvaluatedAt.ItemIndex));
      frmGoPhast.PhastModel.RegisterGlobalVariables(rpShapeCompiler);

//      RemoveGIS_Functions;
      // show the variables and functions
      UpdateTreeList;
      Formula := AFormula;

      // The user edits the formula.
      ShowModal;
      if ResultSet then
      begin
        CheckElevationFormula(Edit, Formula);
      end;

    finally
      Initialize;
//      Free;
    end;
  end
end;

procedure TfrmImportShapefile.ShapefileProgress(Sender: TObject;
  FractionDone: double);
begin
  frmProgressMM.btnAbort.Visible := True;

  frmProgressMM.pbProgress.Position
    := Round(frmProgressMM.pbProgress.Max * FractionDone);
  Inc(FShapeCount);
  frmProgressMM.ProgressLabelCaption := Format(StrReadingShapeS,
    [IntToStrFormatted(FShapeCount)]);
  if (FShapeCount mod 100 = 0) or (FractionDone = 1) then
  begin
    Application.ProcessMessages;
  end;

end;
procedure TfrmImportShapefile.cbCoordinateConversionClick(Sender: TObject);
var
  ShapeObject: TShapeObject;
  Index: integer;
  AName: string;
  Shape: TShape;
  Component: TComponent;
  ShapePoint: TShapePoint;
  Point: TPoint;
  Zone: integer;
  Error: boolean;
begin
  inherited;
  if FShouldEnableImportGrid then
  begin
    cbImportGrid.Enabled := FShouldEnableImportGrid
      and not cbCoordinateConversion.Checked;
    if not cbImportGrid.Enabled and cbImportGrid.Checked then
    begin
      cbImportGrid.Checked := False;
      Beep;
      MessageDlg(StrSorryImportingThe, mtInformation, [mbOK], 0);
    end;
  end;
  Error := False;
  seZoneNumber.Enabled := cbCoordinateConversion.Checked;
  comboEllipsoid.Enabled := cbCoordinateConversion.Checked;
  if comboEllipsoid.Enabled then
  begin
    comboEllipsoid.Color := clWindow;
  end
  else
  begin
    comboEllipsoid.Color := clBtnFace;
  end;

  if cbCoordinateConversion.Checked and (FGeometryFile.Count > 0) then
  begin
    try
      ShapeObject := FGeometryFile[0];
      if ShapeObject.FNumPoints > 0 then
      begin
        lblCoordinates.Caption := Format(StrCoordinatesOfFirst,
          [ShapeObject.FPoints[0].X, ShapeObject.FPoints[0].Y]);
        Zone := LatLongToUTM_Zone(ShapeObject.FPoints[0].X,
          ShapeObject.FPoints[0].Y);
        seZoneNumber.Value := Zone;
      end;

      for Index := 1 to 10 do
      begin
        AName := Format(StrShapeNS, [Index]);
        Component := self.FindComponent(AName);
        Shape := Component as TShape;
        ShapeObject := FGeometryFile[Random(FGeometryFile.Count)];
        if ShapeObject.FNumPoints > 0 then
        begin
          ShapePoint := ShapeObject.FPoints[0];
          if (ShapePoint.X < -360) or (ShapePoint.X > 360)
            or (ShapePoint.Y > 84) or (ShapePoint.Y < -80) then
          begin
            Shape.Visible := False;
            Error := True;
          end
          else
          begin
            Point := LatLongToPoint(ShapePoint.X, ShapePoint.Y);
            // X is from 29 to 708.
            // Y is from 43 to 382.
            Error := Error or
              (Point.X < 29) or (Point.X > 708) or
              (Point.Y < 43) or (Point.Y > 382);

            Shape.Left := Point.x - (Shape.Width div 2) + imageUtmZones.Left;
            Shape.Top := Point.Y - (Shape.Height div 2) + imageUtmZones.Top;
            Shape.Visible := True;
          end;
        end
        else
        begin
          Shape.Visible := False;
        end;
      end;
      if Error then
      begin
        Beep;
        MessageDlg(StrOneOrMoreOfYour, mtWarning, [mbOK], 0);
        cbCoordinateConversion.Checked := False;
      end;
    except on EInvalidOp do
      begin
        Beep;
        MessageDlg(StrOneOrMoreOfYour, mtWarning, [mbOK], 0);
        cbCoordinateConversion.Checked := False;
      end;
    end;

  end
  else
  begin
    for Index := 1 to 10 do
    begin
      AName := Format(StrShapeNS, [Index]);
      Component := self.FindComponent(AName);
      Shape := Component as TShape;
      Shape.Visible := False;
    end;
  end;
end;

function TfrmImportShapefile.LatLongToUTM_Zone(const LongitudeDegrees,
  LatitudeDegrees: double): integer;
begin
  if (LatitudeDegrees >= 56) and (LatitudeDegrees <= 64)
    and (LongitudeDegrees >= 0) and (LongitudeDegrees <= 12) then
  begin
    if LongitudeDegrees >= 3 then
    begin
      result := 32
    end
    else
    begin
      result := 31
    end;
  end
  else if (LatitudeDegrees >= 72) and (LatitudeDegrees <= 84)
    and (LongitudeDegrees >= 0) and (LongitudeDegrees <= 42) then
  begin
    if LongitudeDegrees <= 9 then
    begin
      result := 31;
    end
    else if LongitudeDegrees <= 21 then
    begin
      result := 33;
    end
    else if LongitudeDegrees <= 33 then
    begin
      result := 35;
    end
    else
    begin
      result := 37;
    end;
  end
  else
  begin
    result := SimpleLongToUTM_Zone(LongitudeDegrees);
  end;
end;

function TfrmImportShapefile.LatLongToPoint(Long, Lat: double): TPoint;
begin
  if Long > 180 then
  begin
    Long := Long - 360;
  end
  else if Long < -180 then
  begin
    Long := Long + 360;
  end;
  // X is from 29 to 708.
  result.x := Round((Long + 180) / 360 * (679 - 29) + 29);
  // Y is from 43 to 382
  result.y := Round(((-Lat) + 84) / 164 * (339 - 43) + 43);
end;

function TfrmImportShapefile.SimpleLongToUTM_Zone(
  const LongitudeDegrees: double): integer;
begin
  result := Floor(LongitudeDegrees / 6) + 31;
  if result > 60 then
  begin
    result := result - 60
  end;
end;

procedure TfrmImportShapefile.comboBooleanFieldChange(Sender: TObject);
var
  Combo: TComboBox;
begin
  inherited;
  Combo := Sender as TComboBox;
  if Combo.ItemIndex < 0 then
  begin
    if (UpperCase(Combo.Text) <> 'TRUE')
      and (UpperCase(Combo.Text) <> 'FALSE') then
    begin
      Combo.Color := clRed;
    end
    else
    begin
      Combo.Color := clWindow;
    end;
  end
  else
  begin
    Combo.Color := clWindow;
  end;
end;

procedure TfrmImportShapefile.comboBoundaryChoiceChange(Sender: TObject);
var
  Model: TPhastModel;
  APackage: TModflowPackageSelection;
  Packages: TModflowPackages;
begin
  inherited;
  FConductanceCol := -1;
  FCombinedObjectsAllowed := True;
  Model := frmGoPhast.PhastModel;
  rdgBoundaryConditions.Visible := True;
  rdgBoundaryConditions.Enabled := comboBoundaryChoice.ItemIndex > 0;
  seBoundaryTimeCount.Enabled := (comboBoundaryChoice.ItemIndex > 0)
    and (Model.ModelSelection <> msFootPrint);
  if comboBoundaryChoice.ItemIndex > 0 then
  begin
    case Model.ModelSelection of
      msUndefined:
        begin
          Assert(False);
        end;
      msPhast:
        begin
          rdgBoundaryConditions.Enabled := True;
          rdgBoundaryConditions.ColCount := 3;
          AssignColFeatureProperties;
          rdgBoundaryConditions.Columns[0].ComboUsed := True;
          rdgBoundaryConditions.Columns[1].ComboUsed := True;
          rdgBoundaryConditions.Columns[2].ComboUsed := True;
          rdgBoundaryConditions.Columns[0].Format := rcf4String;
          rdgBoundaryConditions.Columns[1].Format := rcf4String;
          rdgBoundaryConditions.Columns[2].Format := rcf4String;
          rdgBoundaryConditions.Cells[0,0] := StrStartingTime;
          case comboBoundaryChoice.ItemIndex of
            0: //none
              begin
                plBoundary.ActivePage := jvspNone;
              end;
            1: //specified head
              begin
                plBoundary.ActivePage := jvspPhastSpecifiedHead;
                rdgBoundaryConditions.Cells[1,0] := StrHead;
                rdgBoundaryConditions.Cells[2,0] := StrSolution;
              end;
            2: //specified flux
              begin
                plBoundary.ActivePage := jvspNone;
                rdgBoundaryConditions.Cells[1,0] := StrFlux;
                rdgBoundaryConditions.Cells[2,0] := StrSolution;
              end;
            3: //Leaky
              begin
                plBoundary.ActivePage := jvspPhastLeaky;
                rdgBoundaryConditions.Cells[1,0] := StrHead;
                rdgBoundaryConditions.Cells[2,0] := StrSolution;
              end;
            4: //River
              begin
                plBoundary.ActivePage := jvspPhastRiver;
                rdgBoundaryConditions.Cells[1,0] := StrHead;
                rdgBoundaryConditions.Cells[2,0] := StrSolution;
                FCombinedObjectsAllowed := False;
              end;
            5: //Well
              begin
                plBoundary.ActivePage := jvspPhastWell;
                rdgBoundaryConditions.Cells[1,0] := StrPumpingRange;
                rdgBoundaryConditions.Cells[2,0] := StrSolution;
                FCombinedObjectsAllowed := False;
              end;
            else
              begin
                Assert(False);
              end;
          end;
        end;
      msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
        msModflowFmp, msModflowCfp, msModflow2015:
        begin
          Packages := Model.ModflowPackages;
          APackage := comboBoundaryChoice.Items.Objects[
            comboBoundaryChoice.ItemIndex] as TModflowPackageSelection;
          if (APackage is TSfrPackageSelection)
            or (APackage is TMultinodeWellSelection)
            or (APackage is TLakePackageSelection)
            or (APackage is THobPackageSelection)
            or (APackage is TLakeMf6PackageSelection)
            then
          begin
            comboJoinObjects.ItemIndex := 0;
            comboJoinObjects.Enabled := False;
          end
          else
          begin
            comboJoinObjects.Enabled := Self.FAllowShapesToCombine;
            if not comboJoinObjects.Enabled then
            begin
              comboJoinObjects.ItemIndex := 0;
            end;
          end;
          if APackage = Packages.ChdBoundary then
          begin
            InitializeBoundaryControlsForCHD;
          end
          else if APackage = Packages.GhbBoundary then
          begin
            InitializeBoundaryControlsForGHB;
          end
          else if APackage = Packages.WelPackage then
          begin
            InitializeBoundaryControlsForWEL;
          end
          else if APackage = Packages.RivPackage then
          begin
            InitializeBoundaryControlsForRIV;
          end
          else if APackage = Packages.DrnPackage then
          begin
            InitializeBoundaryControlsForDRN;
          end
          else if APackage = Packages.DrtPackage then
          begin
            InitializeBoundaryControlsForDRT;
          end
          else if APackage = Packages.RchPackage then
          begin
            InitializeBoundaryControlsForRCH(Packages);
          end
          else if APackage = Packages.EvtPackage then
          begin
            InitializeBoundaryControlsForEVT(Packages);
          end
          else if APackage = Packages.EtsPackage then
          begin
            InitializeBoundaryControlsForETS(Packages);
          end
          else if APackage = Packages.ResPackage then
          begin
            InitializeBoundaryControlsForRES(Packages);
          end
          else if APackage = Packages.HobPackage then
          begin
            InitializeBoundaryControlsForHOB;
            FCombinedObjectsAllowed := False;
          end
          else if APackage = Packages.Mf6ObservationUtility then
          begin
            InitializeBoundaryControlsForModflow6Obs;
            FCombinedObjectsAllowed := False;
          end
          else if APackage = Packages.SfrPackage then
          begin
            InitializeBoundaryControlsForSFR;
            FCombinedObjectsAllowed := False;
          end
          else if APackage = Packages.SfrModflow6Package then
          begin
            InitializeBoundaryControlsForSFR_MF6;
            FCombinedObjectsAllowed := False;
          end
          else if APackage = Packages.STrPackage then
          begin
            InitializeBoundaryControlsForSTR;
            FCombinedObjectsAllowed := False;
          end
          else if APackage = Packages.LakPackage then
          begin
            InitializeBoundaryControlsForLAK;
          end
          else if APackage = Packages.LakMf6Package then
          begin
            InitializeBoundaryControlsForLAKMf6;
          end
          else if APackage = Packages.HfbPackage then
          begin
            InitializeBoundaryControlsForHFB
          end
          else if APackage = Packages.UzfPackage then
          begin
            InitializeBoundaryControlsForUZF
          end
          else if APackage = Packages.Mnw2Package then
          begin
            InitializeBoundaryControlsForMnw2;
            FCombinedObjectsAllowed := False;
          end
          else if APackage = Packages.MawPackage then
          begin
            InitializeBoundaryControlsForMAW;
            FCombinedObjectsAllowed := False;
          end
          else if APackage = Packages.ConduitFlowProcess then
          begin
            InitializeBoundaryControlsForCFP_Pipe;
            FCombinedObjectsAllowed := False;
          end;
        end;
      msFootPrint:
        begin
          InitializeBoundaryControlsForFootprintWell;
        end
      else
        begin
          Assert(False);
        end;
    end;
  end;
  EnableJoinObjects;
end;

procedure TfrmImportShapefile.comboDrainReturnLocationMethodChange(
  Sender: TObject);
begin
  inherited;
  pcDrtReturnLChoice.ActivePageIndex :=
    comboDrainReturnLocationMethod.ItemIndex;
end;

procedure TfrmImportShapefile.comboEllipsoidChange(Sender: TObject);
begin
  inherited;
  case comboEllipsoid.ItemIndex of
    0:
      begin
        Ellipsoid := Airy1830;
      end;
    1:
      begin
        Ellipsoid := Bessel1841;
      end;
    2:
      begin
        Ellipsoid := Clarke1866;
      end;
    3:
      begin
        Ellipsoid := Clarke1880;
      end;
    4:
      begin
        Ellipsoid := Everest1830;
      end;
    5:
      begin
        Ellipsoid := Fischer1960;
      end;
    6:
      begin
        Ellipsoid := Fischer1968;
      end;
    7:
      begin
        Ellipsoid := GRS67_1967;
      end;
    8:
      begin
        Ellipsoid := GRS75_1975;
      end;
    9:
      begin
        Ellipsoid := GRS80_1980;
      end;
    10:
      begin
        Ellipsoid := Hough1956;
      end;
    11:
      begin
        Ellipsoid := International1924;
      end;
    12:
      begin
        Ellipsoid := Krassowsky1940;
      end;
    13:
      begin
        Ellipsoid := SouthAmerican1969;
      end;
    14:
      begin
        Ellipsoid := WGS60_1960;
      end;
    15:
      begin
        Ellipsoid := WGS66_1966;
      end;
    16:
      begin
        Ellipsoid := WGS72_1972;
      end;
    17:
      begin
        Ellipsoid := WGS84;
      end;
  else
    Assert(False);
  end;
end;

procedure TfrmImportShapefile.comboFormulaInterpChange(Sender: TObject);
//var
//  combo: TComboBox;
begin
  inherited;
//  ChangeConductInterp(comboFormulaInterp.ItemIndex);

//  combo := comboFormulaInterp;
  AssignFormulaInterpretationText(comboFormulaInterp);
end;

procedure TfrmImportShapefile.comboFormulaInterpDRTChange(Sender: TObject);
begin
  inherited;
//  ChangeConductInterp(comboFormulaInterpDRT.ItemIndex);
  AssignFormulaInterpretationText(comboFormulaInterpDRT)
end;

//procedure TfrmImportShapefile.ChangeConductInterp(Value: Integer);
//begin
//  if FConductanceCol >= 0 then
//  begin
//    case Value of
//      0: // direct
//        begin
//          rdgBoundaryConditions.cells[FConductanceCol, 0] := StrConductance;
//        end;
//      1: // Specific
//        begin
//          rdgBoundaryConditions.cells[FConductanceCol, 0] := Format(StrSPerUnitLength, [StrConductance]);
//        end;
//      2: // Total
//        begin
//          rdgBoundaryConditions.cells[FConductanceCol, 0] := Format(StrTotalSPerLayer, [StrConductance]);
//        end;
//      else
//
//        Assert(False);
//    end;
//  end;
//end;

procedure TfrmImportShapefile.comboInterpolatersChange(Sender: TObject);
var
  RowIndex: Integer;
  CanSelect: Boolean;
begin
  inherited;
  for RowIndex := 1 to dgFields.RowCount - 1 do
  begin
    if dgFields.IsSelectedCell(Ord(fgcInterpolator), RowIndex) then
    begin
      CanSelect := True;
      dgFieldsSelectCell(dgFields, Ord(fgcInterpolator), RowIndex, CanSelect);
      if CanSelect and (dgFields.Columns[Ord(fgcInterpolator)].
        PickList.IndexOf(comboInterpolaters.Text) >= 0) then
      begin
        dgFields.Cells[Ord(fgcInterpolator), RowIndex] :=
          comboInterpolaters.Text
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.comboJoinObjectsChange(Sender: TObject);
begin
  inherited;
  InitializeBoundaryConditionControls;
  EnableFeatureImport;
end;

procedure TfrmImportShapefile.comboObjectNameMethodChange(Sender: TObject);
begin
  inherited;
  comboNameAttribute.Enabled := comboObjectNameMethod.ItemIndex > 0;
end;

procedure TfrmImportShapefile.comboRealFieldChange(Sender: TObject);
var
  Combo: TComboBox;
  Value: Extended;
begin
  inherited;
  Combo := Sender as TComboBox;
  if Combo.ItemIndex < 0 then
  begin
    if (Combo.Text <> '') and not TryStrToFloat(Combo.Text, Value) then
    begin
      Combo.Color := clRed;
    end
    else
    begin
      Combo.Color := clWindow;
    end;
  end
  else
  begin
    Combo.Color := clWindow;
  end;
end;

procedure TfrmImportShapefile.CreateVariables(Parser: TRbwParser);
var
  RowIndex: integer;
  VariableName: string;
  AttrIndex: Integer;
  FieldName: AnsiString;
  FieldNumber: Integer;
begin
  for RowIndex := 1 to FShapeFileValidFieldCount do
  begin
    FieldName := AnsiString(dgFields.Cells[Ord(fgcAttributes),RowIndex]);
    FieldNumber := xbShapeDataBase.GetFieldNumberFromName(FieldName);
    VariableName := FieldToVarName(dgFields.Cells[Ord(fgcAttributes),RowIndex]);
    if Parser.IndexOfVariable(VariableName) >= 0 then
    begin
      Beep;
      MessageDlg(Format(StrTheAttributeNamed, [VariableName, RowIndex]),
        mtWarning, [mbOK], 0);
      Continue;
    end;
    if VariableName <> '' then
    begin
      case xbShapeDataBase.GetFieldType(FieldNumber) of
        xbfChar:
          begin
            Parser.CreateVariable(VariableName, StrAttributes, '', TValueStr, VariableName);
          end;
        xbfNumber:
          begin
            if xbShapeDataBase.GetFieldDecimals(FieldNumber) = 0 then
            begin
              Parser.CreateVariable(VariableName, StrAttributes, 0, TValueInt, VariableName);
            end
            else
            begin
              Parser.CreateVariable(VariableName, StrAttributes, 0.0, TValueReal, VariableName);
            end;
          end;
        xbfLogic:
          begin
            Parser.CreateVariable(VariableName, StrAttributes, False, TValueBool, VariableName);
          end;
        else
          begin
            Assert(False);
          end;
      end;
    end;
  end;

  for AttrIndex := 0 to FCsvAttributes.Count - 1 do
  begin
    VariableName := FieldToVarName(FCsvAttributes[AttrIndex].AttributeName);
    if Parser.IndexOfVariable(VariableName) >= 0 then
    begin
      Beep;
      MessageDlg(Format(StrTheCSVAttributeNa, [VariableName]),
        mtWarning, [mbOK], 0);
      Continue;
    end;
    Parser.CreateVariable(VariableName, StrAttributes, 0.0, TValueReal, VariableName);
  end;

  FMinXVar := Parser.CreateVariable(StrShapeMinX, StrAttributes, 0.0, TValueReal, StrShapeMinX);
  FMinYVar := Parser.CreateVariable(StrShapeMinY, StrAttributes, 0.0, TValueReal, StrShapeMinY);
  FMaxXVar := Parser.CreateVariable(StrShapeMaxX, StrAttributes, 0.0, TValueReal, StrShapeMaxX);
  FMaxYVar := Parser.CreateVariable(StrShapeMaxY, StrAttributes, 0.0, TValueReal, StrShapeMaxY);
  if FShapeType in [stPointZ, stPolyLineZ, stPolygonZ, stMultiPointZ] then
  begin
    FMinZVar := Parser.CreateVariable(StrShapeMinZ, StrAttributes, 0.0, TValueReal, StrShapeMinZ);
    FMaxZVar := Parser.CreateVariable(StrShapeMaxZ, StrAttributes, 0.0, TValueReal, StrShapeMaxZ);
  end
  else
  begin
    FMinZVar := nil;
    FMaxZVar := nil;
  end;
end;

procedure TfrmImportShapefile.CheckImportCriterionFormula(AFormula: string);
var
  Expression: TExpression;
begin
  EnableOK;
  if cbImportObjects.Checked then
  begin
    rpShapeCompiler.ClearExpressions;
    rpShapeCompiler.ClearVariables;
    CreateVariables(rpShapeCompiler);
    try
      rpShapeCompiler.Compile(AFormula);
    Except
      on ERbwParserError do
      begin
        btnOK.Enabled := False;
        Exit;
      end;
    end;
    Expression := rpShapeCompiler.CurrentExpression;
    case Expression.ResultType of
      rdtDouble, rdtInteger:
        begin
          AFormula := '' + AFormula + '<>0';
        end;
      rdtBoolean:
        begin
          // do nothing.
        end;
      rdtString:
        begin
          AFormula := 'UpperCase(' + AFormula + ')="TRUE"';
        end;
    else
      Assert(False);
    end;

    try
      rpShapeCompiler.Compile(AFormula);
      Expression := rpShapeCompiler.CurrentExpression;

      edImportCriterion.Text := Expression.Decompile;
    Except
      on ERbwParserError do
      begin
        btnOK.Enabled := False;
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.CheckElevationFormula(Edit: TRbwEdit;
  AFormula: string);
var
  Expression: TExpression;
begin
  EnableOK;
  if cbImportObjects.Checked then
  begin
    rpShapeCompiler.ClearExpressions;
    rpShapeCompiler.ClearVariables;
    CreateVariables(rpShapeCompiler);
    CreateDataSetVariables(rpShapeCompiler,
      TEValuatedAt(rgEvaluatedAt.ItemIndex));
    AddGIS_Functions(rpShapeCompiler, frmGoPhast.PhastModel.ModelSelection,
      TEvaluatedAt(rgEvaluatedAt.ItemIndex));
    frmGoPhast.PhastModel.RegisterGlobalVariables(rpShapeCompiler);
    try
      rpShapeCompiler.Compile(AFormula);
    Except
      on ERbwParserError do
      begin
        btnOK.Enabled := False;
        Exit;
      end;
    end;
    Expression := rpShapeCompiler.CurrentExpression;
    case Expression.ResultType of
      rdtDouble, rdtInteger:
        begin
          // do nothing.
        end;
      rdtBoolean:
        begin
          AFormula := 'If(' + AFormula + ', 1, 0)';
        end;
      rdtString:
        begin
//          AFormula := 'TextToFloatDef(' +  AFormula + ', 0)';
        end;
    else
      Assert(False);
    end;

    try
      rpShapeCompiler.Compile(AFormula);
      Expression := rpShapeCompiler.CurrentExpression;

      Edit.Text := Expression.Decompile;
    Except
      on ERbwParserError do
      begin
        btnOK.Enabled := False;
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.AssignBoundary(AScreenObject: TScreenObject);
var
  Model: TPhastModel;
  Packages: TModflowPackages;
  Package: TObject;
begin
  if tabFeatures.TabVisible and (comboBoundaryChoice.ItemIndex > 0) then
  begin
    case frmGoPhast.ModelSelection of
      msUndefined:
        begin
          Assert(False);
        end;
      msPhast:
        begin
          case comboBoundaryChoice.ItemIndex of
            0:
              begin
                Assert(False);
              end;
          1: //specified head
            begin
              AssignAPhastSpecifiedHeadBoundary(AScreenObject);
            end;
          2: //specified flux
            begin
              AssignAPhastBoundary(AScreenObject.FluxBoundary);
            end;
          3: //Leaky
            begin
              AssignAPhastLeakyBoundary(AScreenObject);
            end;
          4: //River
            begin
              AssignAPhastRiverBoundary(AScreenObject);
            end;
          5: //Well
            begin
              AssignAPhastWellBoundary(AScreenObject);
            end;
          else
            begin
              Assert(False);
            end;
          end;
        end;
      msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
        msModflowFmp, msModflowCfp, msModflow2015:
        begin
          Model := frmGoPhast.PhastModel;
          Packages := Model.ModflowPackages;
          Package := comboBoundaryChoice.Items.Objects[
            comboBoundaryChoice.ItemIndex];
          if Packages.ChdBoundary = Package then
          begin
            ImportModflowChdBoundary(AScreenObject);
          end
          else if Package = Packages.GhbBoundary then
          begin
            ImportModflowGhbBoundary(AScreenObject);
          end
          else if Package = Packages.WelPackage then
          begin
            ImportModflowWelBoundary(AScreenObject);
          end
          else if Package = Packages.RivPackage then
          begin
            ImportModflowRivPackage(AScreenObject);
          end
          else if Package = Packages.DrnPackage then
          begin
            ImportModflowDrnPackage(AScreenObject);
          end
          else if Package = Packages.DrtPackage then
          begin
            ImportModflowDrtPackage(AScreenObject);
          end
          else if Package = Packages.RchPackage then
          begin
            ImportModflowRchPackage(Packages, AScreenObject);
          end
          else if Package = Packages.EvtPackage then
          begin
            ImportModflowEvtPackage(Packages, AScreenObject);
          end
          else if Package = Packages.EtsPackage then
          begin
            ImportModflowEtsPackage(AScreenObject, Packages);
          end
          else if Package = Packages.ResPackage then
          begin
            ImportModflowResPackage(AScreenObject);
          end
          else if Package = Packages.HobPackage then
          begin
            ImportModflowHobPackage(AScreenObject);
          end
          else if Package = Packages.Mf6ObservationUtility then
          begin
            ImportModflow6Obs(AScreenObject);
          end
          else if Package = Packages.SfrPackage then
          begin
            ImportModflowSfrPackage(AScreenObject);
          end
          else if Package = Packages.SfrModflow6Package then
          begin
            ImportModflowSfr_MF6_Package(AScreenObject);
          end
          else if Package = Packages.StrPackage then
          begin
            ImportModflowStrPackage(AScreenObject);
          end
          else if Package = Packages.LakPackage then
          begin
            ImportModflowLakPackage(AScreenObject);
          end
          else if Package = Packages.LakMf6Package then
          begin
            ImportModflowLakMf6Package(AScreenObject);
          end
          else if Package = Packages.HfbPackage then
          begin
            ImportModflowHfbPackage(AScreenObject);
          end
          else if Package = Packages.UzfPackage then
          begin
            ImportModflowUzfPackage(AScreenObject);
          end
          else if Package = Packages.Mnw2Package then
          begin
            ImportModflowMnw2Package(AScreenObject);
          end
          else if Package = Packages.MawPackage then
          begin
            ImportModflowMawPackage(AScreenObject);
          end
          else if Package = Packages.ConduitFlowProcess then
          begin
            ImportModflowCFP_Pipe(AScreenObject);
          end;

        end;
      msFootPrint:
        begin
          ImportFootprintWell(AScreenObject);
        end
      else
        begin
          Assert(False);
        end;
    end;
  end;
end;

procedure TfrmImportShapefile.BoundaryGridBeforeDrawCell(Sender: TObject; ACol,
  ARow: Integer);
var
  Grid: TRbwDataGrid4;
  Value: String;
  Dummy: Extended;
begin
  inherited;
  Grid := Sender as TRbwDataGrid4;
  if (ACol >= Grid.FixedCols) and (ARow >= Grid.FixedRows) then
  begin
    Value := Grid.Cells[ACol, ARow];
    if Value <> '' then
    begin
      if Grid.Columns[ACol].PickList.IndexOf(Value) < 0 then
      begin
        if not TryStrToFloat(Value, Dummy) then
        begin
          Grid.Canvas.Brush.Color := clRed;
        end;
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.btnImportCriterionClick(Sender: TObject);
var
  AFormula: string;
begin
  inherited;
  rpShapeCompiler.ClearVariables;
  rpShapeCompiler.ClearExpressions;

//  with TfrmFormula.Create(self) do
  with frmFormula do
  begin
    try
      Initialize;
      IncludeGIS_Functions(TEvaluatedAt(rgEvaluatedAt.ItemIndex));
      PopupParent := self;
      DataSetGroupName := StrAttributes;
      // put the formula in the TfrmFormula.
      AFormula := edImportCriterion.Text;

      // register the appropriate variables with the
      // parser.
      frmGoPhast.PhastModel.RegisterGlobalVariables(rpShapeCompiler);
      CreateVariables(rbFormulaParser);

      RemoveGIS_Functions;
      // show the variables and functions
      UpdateTreeList;
      Formula := AFormula;

      // The user edits the formula.
      ShowModal;
      if ResultSet then
      begin
        CheckImportCriterionFormula(Formula);
      end;

    finally
      Initialize;
//      Free;
    end;
  end
end;

procedure TfrmImportShapefile.edHighZExit(Sender: TObject);
begin
  inherited;
  CheckElevationFormula(edHighZ, edHighZ.Text);
end;

procedure TfrmImportShapefile.edImportCriterionExit(Sender: TObject);
begin
  inherited;
  CheckImportCriterionFormula(edImportCriterion.Text);
end;

procedure TfrmImportShapefile.edLowZExit(Sender: TObject);
begin
  inherited;
  CheckElevationFormula(edLowZ, edLowZ.Text);
end;

procedure TfrmImportShapefile.edZExit(Sender: TObject);
begin
  inherited;
  CheckElevationFormula(edZ, edZ.Text);
end;

procedure TfrmImportShapefile.cbEnclosedCellsClick(Sender: TObject);
begin
  inherited;
  EnableOK;
end;

procedure TfrmImportShapefile.cbGroundwaterFlowObservationClick(
  Sender: TObject);
begin
  inherited;
  chklstFlowObs.Enabled := cbGroundwaterFlowObservation.Checked;
end;

procedure TfrmImportShapefile.cbImportGridClick(Sender: TObject);
begin
  inherited;
  EnableOK;
end;

procedure TfrmImportShapefile.cbImportObjectsClick(Sender: TObject);
begin
  inherited;
  cbEnclosedCells.Enabled := cbImportObjects.Checked  and
    (FGeometryFile.FileHeader.ShapeType
    in [stPolygon, stPolygonZ, stPolygonM]);
  cbIntersectedCells.Enabled := cbImportObjects.Checked;
  cbInterpolation.Enabled := cbImportObjects.Checked;
  EnableEvalAt;
  edImportCriterion.Enabled := cbImportObjects.Checked;
  btnImportCriterion.Enabled := cbImportObjects.Checked;
  EnableJoinObjects;
  comboVisibility.Enabled := cbImportObjects.Checked;
  EnableOK;
end;

procedure TfrmImportShapefile.cbSelectClick(Sender: TObject);
var
  RowIndex: Integer;
begin
  inherited;
  for RowIndex := 1 to dgFields.RowCount - 1 do
  begin
    if dgFields.IsSelectedCell(Ord(fgcImport), RowIndex) then
    begin
      dgFields.Checked[Ord(fgcImport), RowIndex] := cbSelect.Checked;
    end;
  end;
end;

{ TValueBool }

function TValueBool.Decompile: string;
begin
  case GlobalDecompileType of
    dcNormal: result := inherited Decompile;
    dcValue:
      begin
        if BooleanResult then
        begin
          result := 'True';
        end
        else
        begin
          result := 'False';
        end;
      end;
    else Assert(False);
  end;
end;

{ TValueInt }

function TValueInt.Decompile: string;
begin
  case GlobalDecompileType of
    dcNormal: result := inherited Decompile;
    dcValue: result := IntToStr(IntegerResult);
    else Assert(False);
  end;
end;

{ TValueReal }

function TValueReal.Decompile: string;
begin
  case GlobalDecompileType of
    dcNormal: result := inherited Decompile;
    dcValue: result := FortranFloatToStr(DoubleResult);
    else Assert(False);
  end;

end;

{ TValueStr }

function TValueStr.Decompile: string;
begin
  case GlobalDecompileType of
    dcNormal: result := inherited Decompile;
    dcValue: result := '"' + StringResult + '"';
    else Assert(False);
  end;

end;

{ TFieldNumStorage }

constructor TFieldNumStorage.Create(XBase: TXBase);
begin
  inherited Create;
  FXBase := XBase;
end;

function TFieldNumStorage.GetIntValue: integer;
begin
  if FieldNumber = 0 then
  begin
    result := IntValue;
  end
  else
  begin
    result := FXBase.GetFieldInt(FieldNumber);
  end;
end;

function TFieldNumStorage.GetRealValue: double;
begin
  if FieldNumber = 0 then
  begin
    result := RealValue;
  end
  else
  begin
    result := FXBase.GetFieldNum(FieldNumber);
  end;
end;

function TFieldNumStorage.GetStringValue: string;
begin
  if FieldNumber = 0 then
  begin
    result := StringValue;
  end
  else
  begin
    result := FXBase.GetFieldByNumber(FieldNumber);
  end;
  result := Trim(result);
end;

function TFieldNumStorage.IntFormula: string;
begin
  if (FieldNumber = 0) or Cached then
  begin
    result := Formula;
  end
  else
  begin
    if  (FXBase.GetFieldType(FieldNumber) <> xbfNumber) then
    begin
      result := Trim(FXBase.GetFieldByNumber( FieldNumber ));
    end
    else
    begin
      Result := IntToStr(GetIntValue);
    end;
    Cached := True;
    Formula := result;
  end;
end;

function TFieldNumStorage.RealFormula: string;
begin
  if (FieldNumber = 0) or Cached  then
  begin
    result := Formula;
  end
  else
  begin
    if  (FXBase.GetFieldType(FieldNumber) <> xbfNumber) then
    begin
      result := Trim(FXBase.GetFieldByNumber( FieldNumber ))
    end
    else
    begin
      Result := FortranFloatToStr(GetRealValue);
    end;
    Cached := True;
    Formula := result;
  end;
end;

function TFieldNumStorage.StringFormula: string;
begin
  if (FieldNumber = 0) or Cached  then
  begin
    result := Formula;
  end
  else
  begin
    Cached := True;
    Formula := GetStringValue;
  end;
end;

{ TCsvAttribute }

procedure TCsvAttribute.SetAttributeName(const Value: string);
begin
  FAttributeName := Value;
end;

procedure TCsvAttribute.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TCsvAttribute.SetPosition(const Value: Integer);
begin
  FPosition := Value;
end;

initialization
  PestObsTypes := TStringList.Create;
  // These values must be in the same order as TObGeneral
  PestObsTypes.Add('Head');
  PestObsTypes.Add('Drawdown');
  PestObsTypes.Add('CHD');
  PestObsTypes.Add('Drain');
  PestObsTypes.Add('Well');
  PestObsTypes.Add('GHB');
  PestObsTypes.Add('Riv');
  PestObsTypes.Add('Rch');
  PestObsTypes.Add('EVT');
  PestObsTypes.CaseSensitive := False;

finalization
  PestObsTypes.Free;


end.
