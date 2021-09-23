{@abstract(The main purpose of @name is to define @link(TfrmGoPhast)
  and declare @link(frmGoPhast).  The latter is the main form of GoPhast.
  The former is the type of @link(frmGoPhast).)
  @author(Richard B. Winston <rbwinst@usgs.gov>)}
unit frmGoPhastUnit;

// Include MadExcept settings in GExperts backup.
// The GExperts Backup needs to be configured propertly for this to have
// an effect.

{#BACKUP ModelMuse.mes}
{#BACKUP ModelMuse_Icon.ico}
{#BACKUP ModelMuse_Icon2.ico}

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Types, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Menus, ActnList, StdActns, FastGEO,
  ImgList, RbwRuler, ScreenObjectUnit, PhastGridUnit, frmCustomGoPhastUnit,
  RbwDynamicCursor, ZoomBox2, frameViewUnit, Undo, Contnrs, frmSaveArchiveUnit,
  RbwParser, DataSetUnit, GoPhastTypes, SubscriptionUnit, PhastModelUnit,
  arcball, GLWidget, frame3DViewUnit, IniFiles, MostRecentlyUsedFiles,
  ToolWin, frmUndoUnit, ModflowGridUnit,
  AbstractGridUnit, JvExExtCtrls, JvNetscapeSplitter, frmRunModflowUnit,
  frmRunPhastUnit, ModelMateClassesUnit, SyncObjs, frmRunModpathUnit,
  frmRunZoneBudgetUnit, RbwModelCube, frmRunModelMateUnit, Mask, JvExMask,
  JvSpin, JvHint, frmRunMt3dmsUnit, JvExControls, JvArrowButton,
  frmExportModpathShapefileUnit, SutraMeshUnit, frmSwrObservationsUnit,
  JvExStdCtrls, JvCombobox, JvListComb, FootprintGridUnit, frmRunFootprintUnit,
  System.ImageList, System.Actions, ModflowIrregularMeshUnit, JvComponentBase,
  JvBalloonHint, frmRunPestUnit, frmRunParRepUnit;

  { TODO : 
Consider making CurrentTool a property of TframeView instead of 
TfrmGoPhast.  This might allow for simpler tools. }

{ TODO : Allow the user to zoom to show the full grid extents in one step. }

{ TODO : When creating a new object, it should automatically set values
of enclosed cells if it is a polygon and set values of intersected cells
otherwise. }

{ TODO : Add hints to all menu items; they show up on the status bar. }

type
  // @name is used to specify the format of the files that
  // can be opened or saved by GoPhast.
  //
  // ffAscii = an ASCII text file.
  //
  // ffBinary = a binary file.
  //
  // ffXML = an XML file.
  TFileFormat = (ffAscii, ffBinary, ffXML, ffZLib);

  TVersionCompare = (vcUnknown, vcSame, vcExternalOlder, vcExternalNewer);

  TDefaultCreateArchive = (dcaUnknown, dcaSave, dcaDontSave);

  // Modified from http://delphi.about.com/od/vclusing/a/menuitemhints.htm
  TMenuItemHint = class(THintWindow)
  private
    activeMenuItem : TMenuItem;
    showTimer : TTimer;
    hideTimer : TTimer;
    procedure HideTime(Sender : TObject);
    procedure ShowTime(Sender : TObject);
  public
    constructor Create(AOwner : TComponent); override;
    procedure DoActivateHint(menuItem : TMenuItem);
    destructor Destroy; override;
  end;

  TFramePosition = record
    XCenter: double;
    YCenter: double;
    Magnification: double;
  end;

  TPositionStorage = class(TObject)
    Top: TFramePosition;
    Front: TFramePosition;
    Side: TFramePosition;
  end;

  TOnNewPositionEvent = procedure (Sender: TObject;
    NewPosition: TPositionStorage) of object;

  TPositionList = class(TObject)
  private
    FList: TList;
    FCurrentPosition: integer;
    FMaxPositions: integer;
    FOnNewPosition: TOnNewPositionEvent;
    function GetCanRedo: boolean;
    function GetCanUndo: boolean;
  public
    Constructor Create(MaxPositions: integer);
    Destructor Destroy; override;
    procedure Submit(NewPosition: TPositionStorage);
    procedure Undo;
    procedure Redo;
    property OnNewPosition: TOnNewPositionEvent read FOnNewPosition
      write FOnNewPosition;
    property CanUndo: boolean read GetCanUndo;
    property CanRedo: boolean read GetCanRedo;
    procedure Clear;
  end;

  {
    @abstract(@name is the type of @link(frmGoPhast) which is
     the main form of GoPhast.)
    @name has tool bars and a menu
    to control the operation of GoPhast.  The main part of the form is
    occupied by three @link(TframeView)s which display top, front, and side
    views of the model.  Most of the user interaction with the spatial data
    in the model is through the three @link(TframeView)s.  A status bar
    @link(sbMain) on @name displays messages to the user.

    See @link(frmGoPhast).
  }
  TfrmGoPhast = class(TUndoForm)
    cbControlBar: TControlBar;
    tbarEdit: TToolBar;
    tbCut: TToolButton;
    tbCopy: TToolButton;
    tbPaste: TToolButton;
    tbUndo: TToolButton;
    tbRedo: TToolButton;
    tbarFile: TToolBar;
    tbSave: TToolButton;
    tbOpen: TToolButton;
    tbPrint: TToolButton;
    tbNew: TToolButton;
    tbarEditScreenObjects: TToolBar;
    tbSeparator1: TToolButton;
    tbInsertPoint: TToolButton;
    tbDeleteSegment: TToolButton;
    tbLasso: TToolButton;
    tbSelectPoint: TToolButton;
    tbSelect: TToolButton;
    tbSeparator2: TToolButton;
    tbShowHideObjects: TToolButton;
    tbarView: TToolBar;
    tbZoom: TToolButton;
    tbZoomOut: TToolButton;
    tbZoomIn: TToolButton;
    tbPan: TToolButton;
    tbarEditGrid: TToolBar;
    tbDeleteColumnRow: TToolButton;
    tbMove: TToolButton;
    tbAddVerticalBoundary: TToolButton;
    tbAddHorizontalBoundary: TToolButton;
    tbSubdivide: TToolButton;
    tbGridAngle: TToolButton;
    tbSpacing: TToolButton;
    tbGenerateGrid: TToolButton;
    tbarCreateScreenObject: TToolBar;
    tbStraightLine: TToolButton;
    tbLine: TToolButton;
    tbPolygon: TToolButton;
    tbRectangle: TToolButton;
    tbPoint: TToolButton;
    tbarView3D: TToolBar;
    tbShell: TToolButton;
    tbTopGrid: TToolButton;
    tbFrontGrid: TToolButton;
    tbSideGrid: TToolButton;
    tb3DColors: TToolButton;
    tb3DObjects: TToolButton;
    MostRecentlyUsed: TvRbwMostRecentlyUsed;
    miModel: TMenuItem;
    miModflow: TMenuItem;
    miPhast: TMenuItem;
    acModflowActive: TAction;
    acPhastActive: TAction;
    miNewModflowModel: TMenuItem;
    miNewPHASTModel: TMenuItem;
    acFileNewPhastModel: TAction;
    miLayers: TMenuItem;
    acLayers: TAction;
    miGeneral: TMenuItem;
    tbSelectColRowLayer: TToolButton;
    acSelectColRowLay: TAction;
    miSelectColRowLayer: TMenuItem;
    miTime: TMenuItem;
    miOutputControl: TMenuItem;
    sdModflowInput: TSaveDialog;
    miExport: TMenuItem;
    miExportModflow: TMenuItem;
    WarningsandErrors1: TMenuItem;
    miInvertSelection: TMenuItem;
    miSelectAllTop: TMenuItem;
    acSelectAllFront: TAction;
    acSelectAllSide: TAction;
    miSelectAllFront: TMenuItem;
    miSelectAllSide: TMenuItem;
    miShowGridValues: TMenuItem;
    miPackages: TMenuItem;
    miProgramLocations: TMenuItem;
    miEditGlobalVariables: TMenuItem;
    AddPartstoObject1: TMenuItem;
    tbAddPartsToObject: TToolButton;
    acAddPolygonsToObject: TAction;
    tbSeparator3: TToolButton;
    tbAddLinesToObjects: TToolButton;
    acAddLinesToObject: TAction;
    AddLinesToObject1: TMenuItem;
    tbAddPointsToObject: TToolButton;
    acAddPointsToObject: TAction;
    AddPointstoObject1: TMenuItem;
    ShallAllObjects1: TMenuItem;
    HideAllObjects1: TMenuItem;
    N8: TMenuItem;
    miFilesToArchive: TMenuItem;
    ModflowReference1: TMenuItem;
    miLinkSFRStreams: TMenuItem;
    IntroductoryVideo1: TMenuItem;
    comboZCount: TComboBox;
    miExportShapefile: TMenuItem;
    miModflow2005Model: TMenuItem;
    ilImageList: TImageList;
    miModelResults: TMenuItem;
    miScaleRotateorMoveObjects: TMenuItem;
    miMergeObjects: TMenuItem;
    tbShow2DGrid: TToolButton;
    miShow2DGridlines: TMenuItem;
    // @name separates @link(frameSideView) from @link(frameTopView)
    // and can be used to resize them.  See @link(splitVertTopMoved).
    splitVertTop: TJvNetscapeSplitter;
    // @name separates @link(frame3DView) from @link(frameFrontView)
    // and can be used to resize them.  See @link(splitVertBottomMoved).
    splitVertBottom: TJvNetscapeSplitter;
    // @name separates @link(pnlBottom) from @link(pnlTop) and can be used
    // to resize them.  See @link(splitHorizMoved).
    splitHoriz: TJvNetscapeSplitter;
    miModflowNameFile: TMenuItem;
    miReverseSelectedObjects: TMenuItem;
    acRunModflow: TAction;
    RestoreDefault2DView1: TMenuItem;
    tbRestoreDefault2DView: TToolButton;
    tbPositionUndo: TToolButton;
    tbPositionRedo: TToolButton;
    acPositionForward: TAction;
    acPositionBackward: TAction;
    miUndoPosition: TMenuItem;
    RedoPosition: TMenuItem;
    miGriddedData: TMenuItem;
    miExportModpath: TMenuItem;
    acExportModpath: TAction;
    sdModpathInput: TSaveDialog;
    miManageFluxObservations: TMenuItem;
    Create1: TMenuItem;
    Edit1: TMenuItem;
    Navigation1: TMenuItem;
    miAllVideos: TMenuItem;
    miShowVideoTips: TMenuItem;
    N2: TMenuItem;
    N9: TMenuItem;
    acExportModelMate: TAction;
    ModelMateFile1: TMenuItem;
    acImportModelMate: TAction;
    odModelMate: TOpenDialog;
    miImportModelMate: TMenuItem;
    miObservationType: TMenuItem;
    miObservations: TMenuItem;
    miPredictions: TMenuItem;
    acEditSelecteObjects: TAction;
    miMF_HydrogeologicUnits: TMenuItem;
    miBatchFileAdditions: TMenuItem;
    miSelectObjectsforEditing: TMenuItem;
    miDisplayDataSetValues: TMenuItem;
    miObjectstoShapefile: TMenuItem;
    miDeleteImage: TMenuItem;
    miPHASTProgramLocation: TMenuItem;
    SurferGridFile1: TMenuItem;
    SampleDEMData1: TMenuItem;
    miZONEBUDGETInputFiles: TMenuItem;
    sdZonebudgetInput: TSaveDialog;
    tbVertexValue: TToolButton;
    acVertexValue: TAction;
    EditVertexValues1: TMenuItem;
    sdModelMate: TSaveDialog;
    menuGridLineChoice: TPopupMenu;
    Showall1: TMenuItem;
    Showexterior1: TMenuItem;
    Showactive1: TMenuItem;
    ShowAll2: TMenuItem;
    Showexterior2: TMenuItem;
    Showactive2: TMenuItem;
    acShowAllGridLines: TAction;
    acShowExteriorGridLines: TAction;
    acShowActiveGridLines: TAction;
    Showactiveedge1: TMenuItem;
    acShowActiveEdge: TAction;
    Showactiveedge2: TMenuItem;
    acRestoreDefault2DView: TAction;
    acExportImage: TAction;
    ExportImage1: TMenuItem;
    miManageParameters: TMenuItem;
    miManageHeadObservations: TMenuItem;
    miContourstoShapefile: TMenuItem;
    sdShapefile: TSaveDialog;
    miShapefile: TMenuItem;
    miPathlinestoShapefile: TMenuItem;
    miEndpointsatStartingLocationstoShapefile: TMenuItem;
    miEndpointsatEndingLocationstoShapefile: TMenuItem;
    miTimeSeriestoShapefile: TMenuItem;
    miDataSetstoCSV: TMenuItem;
    tbShowGridValues: TToolButton;
    acShowGridValues: TAction;
    acModflowLgrActive: TAction;
    miModflowLgr: TMenuItem;
    miChildModels: TMenuItem;
    miASCII_RasterFile: TMenuItem;
    tbImportModelResults: TToolButton;
    acImportModelResults: TAction;
    acRunModflowLgr: TAction;
    sdModflowLgr: TSaveDialog;
    miExportModflowLgr: TMenuItem;
    miInvertSelectedVertices: TMenuItem;
    miSplitSelectedObjects: TMenuItem;
    miMakeSelectedVerticesASeparateObject: TMenuItem;
    miSplitObjectAtSelectedVertices: TMenuItem;
    acUndo: TAction;
    acRedo: TAction;
    miModflowNwt: TMenuItem;
    acModflowNwtActive: TAction;
    acRunModflowNwt: TAction;
    miExportModflowNwt: TMenuItem;
    acMeasure: TAction;
    tbMeasure: TToolButton;
    miMeasure: TMenuItem;
    miLockSelectedObjects: TMenuItem;
    miUnlockSelectedObjects: TMenuItem;
    btnDisplayData: TToolButton;
    acDisplayData: TAction;
    miDisplayData: TMenuItem;
    miClearUndoRedostack: TMenuItem;
    acRunMt3dms: TAction;
    dlgSaveMt3dms: TSaveDialog;
    miRunMt3dms: TMenuItem;
    btnRunModel: TJvArrowButton;
    acExportZoneBudget: TAction;
    pmExportModel: TPopupMenu;
    miExportModpathPopUp: TMenuItem;
    miExportZoneBudgetPopup: TMenuItem;
    miRunMt3dmsPopup: TMenuItem;
    acSutra22Active: TAction;
    miSUTRA: TMenuItem;
    acSutraLayers: TAction;
    N10: TMenuItem;
    miSUTRALayerGroups: TMenuItem;
    tlbMesh: TToolBar;
    tbCrossSection: TToolButton;
    acSutraOptions: TAction;
    miSutraOptions: TMenuItem;
    acSutraTimes: TAction;
    miSutraTimes: TMenuItem;
    acRunSutra: TAction;
    sdSutraInput: TSaveDialog;
    miRunSutra: TMenuItem;
    acSutraOutputControl: TAction;
    miSutraOutputControl: TMenuItem;
    miSutraProgramLocations: TMenuItem;
    acSutraProgramLocations: TAction;
    acImportTprogs: TAction;
    ImportTPROGSBinaryGridFile1: TMenuItem;
    miCustomizeSutraMesh: TMenuItem;
    tbRotateCrossSection: TToolButton;
    acImportSutraModelResults: TAction;
    miMesh: TMenuItem;
    miSpecifyCrossSection: TMenuItem;
    miMeshGenerationControls: TMenuItem;
    miRenumberMesh: TMenuItem;
    tbMoveNodes: TToolButton;
    btnGenerateMesh: TToolButton;
    tlb3dViewMesh: TToolBar;
    acShowTopMesh: TAction;
    acShowFrontMesh: TAction;
    btnShowTopMesh: TToolButton;
    btnShowFrontMesh: TToolButton;
    ShowTopMesh1: TMenuItem;
    ShowFrontMesh1: TMenuItem;
    acNewSutraModel: TAction;
    acNewSutraModel1: TMenuItem;
    btnFishnet: TToolButton;
    acFishnet: TAction;
    miViewMeshInformation1: TMenuItem;
    tbDrawElement: TToolButton;
    DeleteModelResults1: TMenuItem;
    acMoveCrossSection: TAction;
    miMoveCrossSection: TMenuItem;
    acRotateCrossSection: TAction;
    miRotateCrossSection: TMenuItem;
    acMoveNodesOrElements: TAction;
    miMoveNodesOrElements: TMenuItem;
    acDrawElement: TAction;
    miDrawElement: TMenuItem;
    miFishnet: TMenuItem;
    acDefaultCrossSection: TAction;
    SetDefaultCrossSection1: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    miSpecifyMesh: TMenuItem;
    miSpecifyFishnetQuadrilateral: TMenuItem;
    acModflowLgr2Active: TAction;
    MODFLOWLGRV21: TMenuItem;
    acModflowFmpActive: TAction;
    acRunModflowFmp: TAction;
    acFarmCrops: TAction;
    miFarmCrops: TMenuItem;
    acFarmSoils: TAction;
    FarmSoils1: TMenuItem;
    acFarmClimate: TAction;
    miFarmClimate: TMenuItem;
    miModflowFmpActive: TMenuItem;
    miMODFLOWFMP2InputFiles: TMenuItem;
    acFarmAllotment: TAction;
    miFarmAllotment: TMenuItem;
    acModflowCfpActive: TAction;
    miModflowCfpActive: TMenuItem;
    miUseOnlineHelp: TMenuItem;
    miUseLocalHelp: TMenuItem;
    acRunModflowCfp: TAction;
    miRunModflowCfp: TMenuItem;
    acHeadObsToShapefile: TAction;
    miHeadObsToShapefile: TMenuItem;
    dlgSaveHeadObsToShapefile: TSaveDialog;
    acSWR_Tabfiles: TAction;
    miSWR_Tabfiles: TMenuItem;
    acSWR_ReachGeometry: TAction;
    miSWR_ReachGeometry: TMenuItem;
    acSwrStructures: TAction;
    miSwrStructures: TMenuItem;
    acSwrObservations: TAction;
    miSwrObservations: TMenuItem;
    acImportGriddedDataFiles: TAction;
    miImportGriddedDataFiles: TMenuItem;
    acImportSutraMesh: TAction;
    dlgOpenImportSutraMesh: TOpenDialog;
    miImportSutraMesh: TMenuItem;
    miSWR: TMenuItem;
    tmrImportErrors: TTimer;
    acExportSutra2DMesh: TAction;
    sdSaveSutraMesh: TSaveDialog;
    miExportSutra2DMesh: TMenuItem;
    acEditFarms: TAction;
    miFarmProcessDialogBoxes: TMenuItem;
    miEditFarms: TMenuItem;
    miLinkedRasters: TMenuItem;
    acFootPrintActive: TAction;
    miFootPrintActive: TMenuItem;
    acFootprintProperties: TAction;
    miFootprintProperties: TMenuItem;
    acNewFootprintModel: TAction;
    miNewFootprintModel: TMenuItem;
    acRunFootprint: TAction;
    acFootprintProgramLocation: TAction;
    miFootprintProgramLocation: TMenuItem;
    sdFootprint: TSaveDialog;
    miRunFootprint: TMenuItem;
    acModflow6Active: TAction;
    miModflow6Active: TMenuItem;
    acRunModflow6: TAction;
    miRunModflow6: TMenuItem;
    acRipPlantGroups: TAction;
    miRipareanETPlantGroups: TMenuItem;
    acSutra30Active: TAction;
    mniSutra30Active: TMenuItem;
    acOutlineToShapefile: TAction;
    miOutlineToShapefile: TMenuItem;
    miEditGeoRef: TMenuItem;
    miFileTypes: TMenuItem;
    acShowOrHideRulers: TAction;
    miShowOrHideRulers: TMenuItem;
    miStructured: TMenuItem;
    miDISV_Choice: TMenuItem;
    tbarEditDisv: TToolBar;
    btnMoveCrossSection: TToolButton;
    btnRotateCrossSection: TToolButton;
    btnQuadmesh: TToolButton;
    acQuadmesh: TAction;
    tbGenerateGridDisv: TToolButton;
    miDISV: TMenuItem;
    miGenerateGrid1: TMenuItem;
    miMoveCrossSection1: TMenuItem;
    miRotateCrossSection1: TMenuItem;
    miQuadmesh: TMenuItem;
    miDefaultCrossSection: TMenuItem;
    acSpecifyCrossSection: TAction;
    miSpecifyCrossSection1: TMenuItem;
    acStructuredGrid: TAction;
    acDisvGrid: TAction;
    tbShow2DMesh: TToolButton;
    tbarShowGrid: TToolBar;
    acRotateAroundGridOrigin: TAction;
    miRotateAroundGridOrigin: TMenuItem;
    acMoveGrid: TAction;
    miMoveGrid: TMenuItem;
    acShowCellNumbers: TAction;
    Action11: TMenuItem;
    bhntMeasureRuler: TJvBalloonHint;
    SimplifySelectedObjects1: TMenuItem;
    acSimplifyScreenObjects: TAction;
    acEditCTS: TAction;
    EditContaminantTreatmentSystems1: TMenuItem;
    ModelSelection1: TMenuItem;
    acEditObservationComparisons: TAction;
    miEditObservationComparisons: TMenuItem;
    acAnonymizeObjects: TAction;
    miAnonymizeObjects: TMenuItem;
    acEditSutraFluxObs: TAction;
    miEditSutraFluxObs: TMenuItem;
    acPEST: TAction;
    miPESTProperties: TMenuItem;
    acRunPest: TAction;
    dlgSavePest: TSaveDialog;
    PESTControlfile1: TMenuItem;
    acArchiveModel: TAction;
    Archivemodelbydefault1: TMenuItem;
    acImportSutraFiles: TAction;
    miImportSutraFiles: TMenuItem;
    odSutraFiles: TOpenDialog;
    tbarPilotPoints: TToolBar;
    tbAddPilotPoint: TToolButton;
    tbDeletePilotPoint: TToolButton;
    acAddPilotPoint: TAction;
    acDeletePilotPoint: TAction;
    AddPilotPoint1: TMenuItem;
    DeletePilotPoint1: TMenuItem;
    odRunParRep: TOpenDialog;
    miRunPEST: TMenuItem;
    acExportParRep: TAction;
    miRunParRep: TMenuItem;
    acRunSvdaPrep: TAction;
    acCalcSuperParameters: TAction;
    miPEST: TMenuItem;
    miCalcSuperParameters: TMenuItem;
    miRunSutraPrep: TMenuItem;
    acImportMf6FeatureFromPest: TAction;
    ImportModelFeaturefromPEST1: TMenuItem;
    acImportSutraFeaturesFromPest: TAction;
    ImportModelFeaturefromPEST2: TMenuItem;
    pmExportModelSutra: TPopupMenu;
    miExportPESTcontrolfile: TMenuItem;
    procedure tbUndoClick(Sender: TObject);
    procedure acUndoExecute(Sender: TObject);
    procedure tbRedoClick(Sender: TObject);
    procedure acPhastActiveExecute(Sender: TObject);
    procedure acModflowActiveExecute(Sender: TObject);
    procedure acLayersExecute(Sender: TObject);
    procedure miGeneralClick(Sender: TObject);
    procedure acSelectColRowLayExecute(Sender: TObject);
    procedure miTimeClick(Sender: TObject);
    procedure miOutputControlClick(Sender: TObject);
    procedure miExportModflowClick(Sender: TObject);
    procedure WarningsandErrors1Click(Sender: TObject);
    procedure miInvertSelectionClick(Sender: TObject);
    procedure acSelectAllTopExecute(Sender: TObject);
    procedure acSelectAllFrontExecute(Sender: TObject);
    procedure acSelectAllSideExecute(Sender: TObject);
    procedure acShowGridValuesClick(Sender: TObject);
    procedure miPackagesClick(Sender: TObject);
    procedure miProgramLocationsClick(Sender: TObject);
    procedure miEditGlobalVariablesClick(Sender: TObject);
    procedure acAddPolygonsToObjectExecute(Sender: TObject);
    procedure acAddLinesToObjectExecute(Sender: TObject);
    procedure acAddPointsToObjectExecute(Sender: TObject);
    procedure ShallAllObjects1Click(Sender: TObject);
    procedure HideAllObjects1Click(Sender: TObject);
    procedure miFilesToArchiveClick(Sender: TObject);
    procedure sdSaveDialogClose(Sender: TObject);
    procedure sdSaveDialogShow(Sender: TObject);
    procedure ModflowReference1Click(Sender: TObject);
    procedure miLinkSFRStreamsClick(Sender: TObject);
    procedure IntroductoryVideo1Click(Sender: TObject);
    procedure miExportShapefileClick(Sender: TObject);
    procedure sdSaveDialogTypeChange(Sender: TObject);
    procedure miModelResultsClick(Sender: TObject);
    procedure miScaleRotateorMoveObjectsClick(Sender: TObject);
    procedure miMergeObjectsClick(Sender: TObject);
    procedure miModflowNameFileClick(Sender: TObject);
    procedure miReverseSelectedObjectsClick(Sender: TObject);
    procedure RestoreDefault2DView1Click(Sender: TObject);
    procedure sdModflowInputShow(Sender: TObject);
    procedure sdModflowInputClose(Sender: TObject);
    procedure sdPhastInputShow(Sender: TObject);
    procedure sdPhastInputClose(Sender: TObject);
    procedure acPositionForwardExecute(Sender: TObject);
    procedure acPositionBackwardExecute(Sender: TObject);
    procedure miGriddedDataClick(Sender: TObject);
    procedure sdModpathInputShow(Sender: TObject);
    procedure sdModpathInputClose(Sender: TObject);
    procedure acExportModpathExecute(Sender: TObject);
    procedure miFileClick(Sender: TObject);
    procedure miManageFluxObservationsClick(Sender: TObject);
    procedure miAllVideosClick(Sender: TObject);
    procedure miShowVideoTipsClick(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);
    procedure acCutExecute(Sender: TObject);
    procedure acPasteExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure acExportModelMateExecute(Sender: TObject);
    procedure acImportModelMateExecute(Sender: TObject);
    procedure miObservationsClick(Sender: TObject);
    procedure miMF_HydrogeologicUnitsClick(Sender: TObject);
    procedure miBatchFileAdditionsClick(Sender: TObject);
    procedure miSelectObjectsforEditingClick(Sender: TObject);
    procedure miDisplayDataSetValuesClick(Sender: TObject);
    procedure miObjectstoShapefileClick(Sender: TObject);
    procedure miDeleteImageClick(Sender: TObject);
    procedure miPHASTProgramLocationClick(Sender: TObject);
    procedure SurferGridFile1Click(Sender: TObject);
    procedure sbMainDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure SampleDEMData1Click(Sender: TObject);
    procedure miZONEBUDGETInputFilesClick(Sender: TObject);
    procedure sdZonebudgetInputShow(Sender: TObject);
    procedure sdZonebudgetInputClose(Sender: TObject);
    procedure acVertexValueExecute(Sender: TObject);
    procedure acExportImageExecute(Sender: TObject);
    procedure miManageParametersClick(Sender: TObject);
    procedure miManageHeadObservationsClick(Sender: TObject);
    procedure sdModelMateShow(Sender: TObject);
    procedure sdModelMateClose(Sender: TObject);
    procedure miContourstoShapefileClick(Sender: TObject);
    procedure miPathlinestoShapefileClick(Sender: TObject);
    procedure miEndpointsatStartingLocationstoShapefileClick(Sender: TObject);
    procedure miEndpointsatEndingLocationstoShapefileClick(Sender: TObject);
    procedure miTimeSeriestoShapefileClick(Sender: TObject);
    procedure miDataSetstoCSVClick(Sender: TObject);
    procedure acModflowLgrActiveExecute(Sender: TObject);
    procedure miChildModelsClick(Sender: TObject);
    procedure miASCII_RasterFileClick(Sender: TObject);
    procedure acRunModflowLgrExecute(Sender: TObject);
    procedure miInvertSelectedVerticesClick(Sender: TObject);
    procedure miSplitSelectedObjectsClick(Sender: TObject);
    procedure miMakeSelectedVerticesASeparateObjectClick(Sender: TObject);
    procedure miSplitObjectAtSelectedVerticesClick(Sender: TObject);
    procedure acModflowNwtActiveExecute(Sender: TObject);
    procedure acRunModflowNwtExecute(Sender: TObject);
    procedure acMeasureExecute(Sender: TObject);
    procedure miLockSelectedObjectsClick(Sender: TObject);
    procedure miUnlockSelectedObjectsClick(Sender: TObject);
    procedure acDisplayDataExecute(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure miClearUndoRedostackClick(Sender: TObject);
    procedure acRunMt3dmsExecute(Sender: TObject);
    procedure dlgSaveMt3dmsShow(Sender: TObject);
    procedure dlgSaveMt3dmsClose(Sender: TObject);
    procedure acSutra22ActiveExecute(Sender: TObject);
    procedure sdShapefileShow(Sender: TObject);
    procedure sdShapefileClose(Sender: TObject);
    procedure acSutraLayersExecute(Sender: TObject);
    procedure tbCrossSectionClick(Sender: TObject);
    procedure acSutraOptionsExecute(Sender: TObject);
    procedure AllowDrawing(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure acSutraTimesExecute(Sender: TObject);
    procedure acRunSutraExecute(Sender: TObject);
    procedure acSutraOutputControlExecute(Sender: TObject);
    procedure acSutraProgramLocationsExecute(Sender: TObject);
    procedure acImportTprogsExecute(Sender: TObject);
    procedure sdSutraInputShow(Sender: TObject);
    procedure sdSutraInputClose(Sender: TObject);
    procedure miCustomizeSutraMeshClick(Sender: TObject);
    procedure tbRotateCrossSectionClick(Sender: TObject);
    procedure miRenumberMeshClick(Sender: TObject);
    procedure tbMoveNodesClick(Sender: TObject);
    procedure acShowTopMeshExecute(Sender: TObject);
    procedure acShowFrontMeshExecute(Sender: TObject);
    procedure acFishnetExecute(Sender: TObject);
    procedure miViewMeshInformation1Click(Sender: TObject);
    procedure tbDrawElementClick(Sender: TObject);
    procedure DeleteModelResults1Click(Sender: TObject);
    procedure acDefaultCrossSectionExecute(Sender: TObject);
    procedure miSpecifyMeshClick(Sender: TObject);
    procedure miSpecifyFishnetQuadrilateralClick(Sender: TObject);
    procedure acModflowLgr2ActiveExecute(Sender: TObject);
    procedure acModflowFmpActiveExecute(Sender: TObject);
    procedure acFarmCropsExecute(Sender: TObject);
    procedure acFarmSoilsExecute(Sender: TObject);
    procedure acFarmClimateExecute(Sender: TObject);
    procedure acRunModflowFmpExecute(Sender: TObject);
    procedure acFarmAllotmentExecute(Sender: TObject);
    procedure acModflowCfpActiveExecute(Sender: TObject);
    procedure miUseLocalHelpClick(Sender: TObject);
    procedure miUseOnlineHelpClick(Sender: TObject);
    procedure acRunModflowCfpExecute(Sender: TObject);
    procedure acHeadObsToShapefileExecute(Sender: TObject);
    procedure acSWR_TabfilesExecute(Sender: TObject);
    procedure acSWR_ReachGeometryExecute(Sender: TObject);
    procedure acSwrStructuresExecute(Sender: TObject);
    procedure acSwrObservationsExecute(Sender: TObject);
    procedure acImportGriddedDataFilesExecute(Sender: TObject);
    procedure acImportSutraMeshExecute(Sender: TObject);
    procedure tmrImportErrorsTimer(Sender: TObject);
    procedure acExportSutra2DMeshExecute(Sender: TObject);
    procedure acEditFarmsExecute(Sender: TObject);
    procedure miLinkedRastersClick(Sender: TObject);
    procedure acFootPrintActiveExecute(Sender: TObject);
    procedure acFootprintPropertiesExecute(Sender: TObject);
    procedure acRunFootprintExecute(Sender: TObject);
    procedure acFootprintProgramLocationExecute(Sender: TObject);
    procedure sdFootprintShow(Sender: TObject);
    procedure sdFootprintClose(Sender: TObject);
    procedure acModflow6ActiveExecute(Sender: TObject);
    procedure acRunModflow6Execute(Sender: TObject);
    procedure acRipPlantGroupsExecute(Sender: TObject);
    procedure acSutra30ActiveExecute(Sender: TObject);
    procedure acOutlineToShapefileExecute(Sender: TObject);
    procedure miEditGeoRefClick(Sender: TObject);
    procedure miFileTypesClick(Sender: TObject);
    procedure acShowOrHideRulersExecute(Sender: TObject);
    procedure acQuadmeshExecute(Sender: TObject);
    procedure acSpecifyCrossSectionExecute(Sender: TObject);
    procedure acStructuredGridExecute(Sender: TObject);
    procedure acDisvGridExecute(Sender: TObject);
    procedure acRotateAroundGridOriginExecute(Sender: TObject);
    procedure acMoveGridExecute(Sender: TObject);
    procedure acShowCellNumbersExecute(Sender: TObject);
    procedure bhntMeasureRulerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure acSimplifyScreenObjectsExecute(Sender: TObject);
    procedure acEditCTSExecute(Sender: TObject);
    procedure acEditObservationComparisonsExecute(Sender: TObject);
    procedure acAnonymizeObjectsExecute(Sender: TObject);
    procedure acEditSutraFluxObsExecute(Sender: TObject);
    procedure acPESTExecute(Sender: TObject);
    procedure acRunPestExecute(Sender: TObject);
    procedure dlgSavePestShow(Sender: TObject);
    procedure dlgSavePestClose(Sender: TObject);
    procedure acArchiveModelExecute(Sender: TObject);
    procedure acImportSutraFilesExecute(Sender: TObject);
    procedure odSutraFilesTypeChange(Sender: TObject);
    procedure acAddPilotPointExecute(Sender: TObject);
    procedure acDeletePilotPointExecute(Sender: TObject);
    procedure odRunParRepShow(Sender: TObject);
    procedure odRunParRepClose(Sender: TObject);
    procedure acExportParRepExecute(Sender: TObject);
    procedure acCalcSuperParametersExecute(Sender: TObject);
    procedure acRunSvdaPrepExecute(Sender: TObject);
    procedure acImportMf6FeatureFromPestExecute(Sender: TObject);
    procedure acImportSutraFeaturesFromPestExecute(Sender: TObject);
  private
    FDefaultCreateArchive: TDefaultCreateArchive;
    FCreateArchive: Boolean;
    FCreateArchiveSet: boolean;
    FCreatingMainForm: Boolean;
    miHint : TMenuItemHint;
    FSaveModelForm: TfrmSaveArchive;
    FOtherSplitterMoving: Boolean;
    FSynchronizeCount: integer;
    FRunModflowForm: TfrmRunModflow;
    FRunModpathForm: TfrmRunModpath;
    FRunZoneBudgetForm: TfrmRunZoneBudget;
    FRunModflow: Boolean;
    FRunModpath: Boolean;
    FCreateNewCompositeBudgetFile: boolean;
    FRunZoneBudget: Boolean;
    FRunPhastForm: TfrmRunPhast;
    FRunSutraFrom: TfrmRunPhast;
    FRunPhast: boolean;
    FRunSutra: Boolean;
    FPositionList: TPositionList;
    FBrowser: string;
    FStartTime: TDateTime;
    FFileStream: TFileStream;
    FFileSize: Int64;
    FRunModelMateForm: TfrmRunModelMate;
    FRunModelMate: boolean;
    FObservationFileName: string;
    FPredictionFileName: string;
    FReadingFile: Boolean;
    FSizeWarningDisplayed: Boolean;
    FRunModelSelection: Integer;
    FSupressDrawing: Integer;
    FLastMoveTime: TDateTime;
    FRunMt3dmsForm: TfrmRunMt3dms;
    FRunMt3dms: boolean;
    FRunModpathModelSelection: integer;
    FExportModpathShapefileForm: TfrmExportModpathShapefile;
    FExportModpathShapeFileModelChoice: Integer;
    FRenumberMethod: integer;
    FAlreadyShown: Boolean;
    FSaveTime: TDateTime;
    FFootprintFileName: string;
    FRunFootprintForm: TfrmRunFootprint;
    FRunFootprint: boolean;
    FNewDescription: TCaption;
    FNeedFirstRedraw: Boolean;
    FCreatingModel: Boolean;
    FNoIniFile: Boolean;
    FRunMt3dModel: TCustomModel;
    FInvalidatingAllViews: Boolean;
    FRunPestForm: TfrmRunPest;
    FRunPest: TPestExportChoice;
    FExporting: Boolean;
    FRunParRepForm: TfrmRunParRep;
    FRunParRep: Boolean;
//    FfrmRunSupCalc: TfrmRunSupCalc;
//    FRunSupCalc: Boolean;
//    FWriteErrorRaised: Boolean;
    procedure SetCreateArchive(const Value: Boolean);
    property CreateArchive: Boolean read FCreateArchive write SetCreateArchive;
    procedure WMMenuSelect(var Msg: TWMMenuSelect); message WM_MENUSELECT;
    // @name deletes the last point the the current @link(TScreenObject).
    // If there is no current @link(TScreenObject), it deletes the selected
    // @link(TScreenObject)(s).
    procedure DeleteLastPointInScreenObject;
    // @name deletes the selected @link(TScreenObject)(s).
    procedure DeleteSelectedNodesOrSelectedScreenObjects;
    procedure CreatePhastModel;
    procedure UpdateDisplay(Sender: TObject);
    procedure ShowOrHideAllScreenObjects(ShowAll: Boolean);
    procedure PlayIntroductoryVideo;
    procedure StoreInitalPosition;
    procedure ReadModelMateProject(FileName: string;
      ModelMateProject: TProject);
    procedure InitializeModflowInputDialog;
    procedure EnableDeleteImage;
    procedure CancelCurrentScreenObject;
    procedure InternalSaveFile(FileName: string);
    procedure GetProgramLocations(Model: TCustomModel);
    function TestModpathLocationOK(Model: TCustomModel): Boolean;
    function TestZoneBudgetLocationOK(Model: TCustomModel): Boolean;
    procedure MenuItemsSetEnabled(AValue: Boolean);
    procedure ClearFileSaveDialogBoxNames;
    procedure HandleCommandLineParameters(FileName: string);
    procedure ExportFromCommandLine(FileName: string);
    procedure ImportPvalFile(PValFile: string);
    procedure ImportGlobalVariablesFile(GloVarFile: string);
    procedure ExportSutra(ShouldRunSutra: Boolean; const FileName: string);
    procedure NilDisplay;
    function TestCompatibleModflowMt3d: Boolean;
    procedure EnableGridItems;
    procedure HaveUsersDefineSutraLayers;
    procedure HaveUsersDefineModflowLayers;
    procedure SetToolbarPositions;
    procedure CheckSvdaActivated;
    function PestVersionOK: Boolean;
  published
    // @name is the TAction for @link(miAddVerticalGridLine)
    // and @link(tbAddVerticalBoundary).
    // See @link(tbAddVerticalBoundaryClick).
    acAddColumn: TAction;
    // @name is the TAction for @link(miAddHorizontalGridLine)
    // and @link(tbAddHorizontalBoundary).
    // See @link(tbAddHorizontalBoundaryClick).
    acAddRow: TAction;
    // @name is the TAction for @link(miColor) and @link(tb3DColors).
    // See @link(acColorExecute).
    acColor: TAction;
    // @name is the TAction for @link(miShowColoredGrid) and @link(tb3DColors).
    // See @link(tb3DColorsClick).
    acColoredGrid: TAction;
    // @name is the TAction for @link(miCopy) and @link(tbCopy).
    // @name is not yet used.
    acCopy: TAction;
    // @name is the TAction for @link(miCreateLine) and @link(tbLine).
    // See @link(tbLineClick).
    acCreateLine: TAction;
    // @name is the TAction for @link(miCreatePoint) and @link(tbPoint).
    // See @link(tbPointClick).
    acCreatePoint: TAction;
    // @name is the TAction for @link(miCreatePolygon) and @link(tbPolygon).
    // See @link(tbPolygonClick).
    acCreatePolygon: TAction;
    // @name is the TAction for @link(miCreateRectangle)
    // and @link(tbRectangle).
    // See @link(tbRectangleClick).
    acCreateRectangle: TAction;
    // @name is the TAction for @link(miCreateStraightLine)
    // and @link(tbStraightLine).
    // See @link(tbStraightLineClick).
    acCreateStraightLine: TAction;
    // @name is the TAction for @link(miCut) and @link(tbCut).
    // @name is not yet used.
    acCut: TAction;
    // @name is the TAction for @link(miDeleteGridLine)
    // and @link(tbDeleteColumnRow).
    // See @link(acDeleteColumnRowExecute).
    acDeleteColumnRow: TAction;
    // @name is the TAction for @link(miDeleteSegment)
    // and @link(tbDeleteSegment).
    // See @link(tbDeleteSegmentClick).
    acDeleteSegment: TAction;
    // @name is the TAction for @link(miEditDataSet).
    // See @link(acEditDataSetsExecute).
    acEditDataSets: TAction;
    // @name is the TAction for @link(miEditGridLines).
    // See @link(acEditGridLinesExecute).
    acEditGridLines: TAction;
    // @name is the TAction for @link(miVerticalExaggeration).
    // See @link(miVerticalExaggerationClick).
    acEditVerticalExaggeration: TAction;
    // @name is the TAction for @link(miExit).
    // See @link(acExitExecute).
    acExit: TAction;
    // @name is the TAction for @link(miExportPhast).
    // See @link(acExportPhastInputFileExecute).
    acExportPhastInputFile: TAction;
    acFileNewModflowModel: TAction;
    // @name is the TAction for @link(miOpen) and @link(tbOpen).
    // See @link(acFileOpenExecute).
    acFileOpen: TAction;
    // @name is the TAction for @link(miPrint) and @link(tbPrint).
    // @name is not yet used.
    acFilePrint: TAction;
    // @name is the TAction for @link(miSave) and @link(tbSave).
    // See @link(acFileSaveExecute).
    acFileSave: TAction;
    // @name is the TAction for @link(miSaveAs).
    // See @link(acFileSaveAsExecute).
    acFileSaveAs: TAction;
    // @name is the TAction for @link(miFont).
    // See @link(acFontExecute).
    acFont: TAction;
    // @name is the TAction for @link(miGenerateGrid) and @link(tbGenerateGrid).
    // See @link(acGenerateGridExecute).
    acGenerateGrid: TAction;
    // @name is the TAction for @link(miGridAngle).
    // See @link(acGridAngleExecute).
    acGridAngle: TAction;
    // @name is the TAction for @link(miDragtoRotate) and @link(tbGridAngle).
    // See @link(tbGridAngleClick).
    acGridDragRotate: TAction;
    // @name is the TAction for @link(miContents).
    // See @link(acHelpContentsExecute).
    acHelpContents: THelpContents;
    // @name is the TAction for @link(miInsertNode) and @link(tbInsertPoint).
    // See @link(tbInsertPointClick).
    acInsertNode: TAction;
    // @name is the TAction for @link(miMoveColumnOrRow) and @link(tbMove).
    // See @link(acMoveColumnOrRowExecute).
    acMoveColumnOrRow: TAction;
    // @name is the TAction for @link(miGoTo).
    // See @link(acMoveToExecute).
    acMoveTo: TAction;
    // @name is the TAction for @link(miPan) and @link(tbPan).
    // See @link(tbPanClick).
    acPan: TAction;
    // @name is the TAction for @link(miPaste) and @link(tbPaste).
    // @name is not used yet.
    acPaste: TAction;
    // @name is the TAction for @link(miRestoreDefaultView).
    // See @link(acRestoreDefaultViewExecute).
    acRestoreDefaultView: TAction;
    acSelectAllTop: TAction;
    // @name is the TAction for @link(miSelectNodes) and @link(tbSelectPoint).
    // See @link(tbSelectPointClick).
    acSelectNode: TAction;
    // @name is the TAction for @link(miSelectObjects) and @link(tbSelect).
    // See @link(tbSelectClick).
    acSelectObjects: TAction;
    // @name is the TAction for @link(miSelectWithLasso) and @link(tbLasso).
    // See @link(tbLassoClick).
    acSelectWithLasso: TAction;
    // @name is the TAction for @link(miSetSpacing) and @link(tbSpacing).
    // See @link(acSetSpacingExecute).
    acSetSpacing: TAction;
    // @name is the TAction for @link(miShow3DObjects) and @link(tb3DObjects).
    // See @link(tb3DObjectsClick).
    acShow3DObjects: TAction;
    // @name is the TAction for @link(miShowFrontGrid) and @link(tbFrontGrid).
    // See @link(acShowFrontGridExecute).
    acShowFrontGrid: TAction;
    // @name is the TAction for @link(miShowGridShell) and @link(tbShell).
    // See @link(acShowGridShellExecute).
    acShowGridShell: TAction;
    // @name is the action for showing or hiding @link(frmShowHideObjects).
    // See @link(miShowHideObjectsClick).
    acShowHideObjects: TAction;
    // @name is the TAction for @link(miShowSideGrid) and @link(tbSideGrid).
    // See @link(acShowSideGridExecute).
    acShowSideGrid: TAction;
    // @name is the TAction for @link(miShowTopGrid) and @link(tbTopGrid).
    // See @link(acShowTopGridExecute).
    acShowTopGrid: TAction;
    // @name is the TAction for @link(miSmoothGrid).
    // See @link(acSmoothGridExecute).
    acSmoothGrid: TAction;
    // @name is the TAction for @link(miSubdivide) and @link(tbSubdivide).
    // See @link(acSubdivideExecute).
    acSubdivide: TAction;
    // @name is the TAction for @link(miZoom) and @link(tbZoom).
    // See @link(tbZoomClick).
    acZoom: TAction;
    // @name is the TAction for @link(miZoomIn) and @link(tbZoomIn).
    // See @link(miZoomInClick).
    acZoomIn: TAction;
    // @name is the TAction for @link(miZoomOut) and @link(tbZoomOut).
    // See @link(tbZoomOutClick).
    acZoomOut: TAction;
    // @name holds the instances of TAction in GoPhast.
    alActionList: TActionList;
    // @name  is used to pick the background color for GoPhast
    // in @link(acColorExecute).
    cdColorDialog: TColorDialog;
    // @name is used to draw a rotated version of the AddColumn cursor
    // in the top view of the model.
    dcAddColCursor: TRbwDynamicCursor;
    // @name is used to draw a rotated version of the AddRow cursor
    // in the top view of the model.
    dcAddRowCursor: TRbwDynamicCursor;
    // @name is used to draw a rotated version of the MoveColumn cursor
    // in the top view of the model.
    dcMoveColCursor: TRbwDynamicCursor;
    // @name is used to draw a rotated version of the MoveRow cursor
    // in the top view of the model.
    dcMoveRowCursor: TRbwDynamicCursor;
    // @name is used to draw a rotated version of the SetCellSpacing cursor
    // in the top view of the model.
    dcSetSpacing: TRbwDynamicCursor;
    // @name is used to draw a rotated version of the Subdivide cursor
    // in the top view of the model.
    dcSubdivide: TRbwDynamicCursor;
    // @name  is used to pick the font for GoPhast
    // in @link(acFontExecute).
    fdFontDialog: TFontDialog;
    // @name displays a 3D view of the model and handles interaction
    // with the 3D view.
    frame3DView: Tframe3DView;
    // @name displays a front view of the model and handles interaction
    // with the front view.
    frameFrontView: TframeView;
    // @name displays a side view of the model and handles interaction
    // with the side view.
    frameSideView: TframeView;
    // @name displays a top view of the model and handles interaction
    // with the top view.
    frameTopView: TframeView;
    ilDisabledImageList: TImageList;
    // See @link(mi3D_ColorsClick).
    mi3D_Colors: TMenuItem;
    // See @link(miAboutClick).
    miAbout: TMenuItem;
    // See @link(tbAddHorizontalBoundaryClick) and @link(acAddRow).
    miAddHorizontalGridLine: TMenuItem;
    // See @link(tbAddVerticalBoundaryClick) and @link(acAddColumn).
    miAddVerticalGridLine: TMenuItem;
    // See @link(miChemistryOptionsClick).
    miChemistryOptions: TMenuItem;
    // See @link(acColorExecute) and @link(acColor).
    miColor: TMenuItem;
    // See @link(acHelpContentsExecute) and @link(acHelpContents).
    miContents: TMenuItem;
    // See @link(acCopy).
    miCopy: TMenuItem;
    // See @link(tbLineClick) and @link(acCreateLine).
    miCreateLine: TMenuItem;
    // See @link(tbPointClick) and @link(acCreatePoint).
    miCreatePoint: TMenuItem;
    // See @link(tbPolygonClick) and @link(acCreatePolygon).
    miCreatePolygon: TMenuItem;
    // See @link(tbRectangleClick) and @link(acCreateRectangle).
    miCreateRectangle: TMenuItem;
    // See @link(tbStraightLineClick) and @link(acCreateStraightLine).
    miCreateStraightLine: TMenuItem;
    // @name holds the @link(miFont), @link(miColor), @link(miHintDelay),
    // and @link(miRulerFormat) menu items.
    miCustomize: TMenuItem;
    // See @link(acCut).
    miCut: TMenuItem;
    // @name holds the @link(miEditDataSet), @link(miEditGlobalVariables),
    // @link(miDisplayData), @link(miShowGridValues)
    // and @link(miDisplayDataSetValues) menu items.
    miData: TMenuItem;
    // See @link(acDeleteColumnRowExecute) and @link(acDeleteColumnRow).
    miDeleteGridLine: TMenuItem;
    // See @link(tbDeleteSegmentClick) and @link(acDeleteSegment).
    miDeleteSegment: TMenuItem;
    // See @link(tbGridAngleClick) and @link(acGridDragRotate).
    miDragtoRotate: TMenuItem;
    // @name holds the @link(miUndo), @link(miRedo), @link(miCut),
    // @link(miCopy), @link(miPaste), @link(miSelectAll), @link(miEditBitmaps),
    // and @link(miShowHideBitmaps) menu items.
    miEdit: TMenuItem;
    // See @link(miEditBitmapsClick).
    miEditBitmaps: TMenuItem;
    // See @link(acEditDataSetsExecute) and @link(acEditDataSets).
    miEditDataSet: TMenuItem;
    // See @link(acEditGridLinesExecute) and @link(acEditGridLines).
    miEditGridLines: TMenuItem;
    // This TMenuItem [Object|Edit Selected Object(s)], allows the user to
    // edit the properties of the selected objects by displaying the
    // @link(TfrmScreenObjectProperties) dialog box.
    // @seealso(miEditSelectedObjectsClick)
    miEditSelectedObjects: TMenuItem;
    // The OnClick event for @name, @link(miExamplesClick), opens the default
    // web browser with the web page for the instructions for
    // recreating the examples.
    miExamples: TMenuItem;
    // See @link(acExitExecute) and @link(acExit).
    miExit: TMenuItem;
    // See @link(acExportPhastInputFileExecute)
    // and @link(acExportPhastInputFile).
    miExportPhast: TMenuItem;
    // @name holds the @link(miNew), @link(miOpen), @link(miSave),
    // @link(miSaveAs), @link(miImport), @link(miExport), @link(miPrint),
    // and @link(miExit) menu items.
    miFile: TMenuItem;
    // See @link(acFontExecute) and @link(acFont).
    miFont: TMenuItem;
    // See @link(miFreeSurfaceClick).
    miFreeSurface: TMenuItem;
    // See @link(acGenerateGridExecute) and @link(acGenerateGrid).
    miGenerateGrid: TMenuItem;
    // See @link(acMoveToExecute) and @link(acMoveTo).
    miGoTo: TMenuItem;
    // @name holds the
    // @link(miDeleteGridLine),
    // @link(miMoveColumnOrRow),
    // @link(miAddVerticalGridLine),
    // @link(miAddHorizontalGridLine),
    // @link(miSubdivide),
    // @link(miSetSpacing),
    // @link(miDragtoRotate),
    // @link(miGridAngle),
    // @link(miGenerateGrid),
    // @link(miEditGridLines),
    // @link(miSmoothGrid), and
    // @link(miSetSelectedColRowLayer) menu items.
    miGrid: TMenuItem;
    // See @link(acGridAngleExecute) and @link(acGridAngle).
    miGridAngle: TMenuItem;
    // See @link(miGridOptionsClick).
    miGridOptions: TMenuItem;
    // @name holds the
    // @link(miContents),
    // @link(miHelpOnMainWindow),
    // @link(miExamples), and
    // @link(miAbout) menu items.
    miHelp: TMenuItem;
    // @name provides help on the main window of GoPhast.
    miHelpOnMainWindow: TMenuItem;
    // See @link(miHintDelayClick).
    miHintDelay: TMenuItem;
    // @name holds the
    // @link(miImportDistributedDatabyZone),
    // @link(miImportShapefile),
    // @link(miImportDXFFile), and
    // @link(miImportBitmap) menu items.
    // @name is under the @link(miFile) menu item.
    miImport: TMenuItem;
    // See @link(miImportBitmapClick).
    miImportBitmap: TMenuItem;
    // See @link(miImportDistributedDatabyZoneClick).
    miImportDistributedDatabyZone: TMenuItem;
    // See @link(miImportDXFFileClick).
    miImportDXFFile: TMenuItem;
    // See @link(miImportPointsClick).
    miImportPoints: TMenuItem;
    // See @link(miImportShapefileClick).
    miImportShapefile: TMenuItem;
    // See @link(tbInsertPointClick) and @link(acInsertNode).
    miInsertNode: TMenuItem;
    // See @link(acMoveColumnOrRowExecute) and @link(acMoveColumnOrRow).
    miMoveColumnOrRow: TMenuItem;
    // @name holds @link(miNewModflowModel) and @link(miNewPHASTModel).
    miNew: TMenuItem;
    // @name holds the
    // @link(miSelectObjects),
    // @link(miSelectNodes),
    // @link(miSelectWithLasso),
    // @link(N4),
    // @link(miCreatePoint),
    // @link(miCreateLine),
    // @link(miCreatePolygon),
    // @link(miCreateStraightLine),
    // @link(miCreateRectangle),
    // @link(miInsertNode),
    // @link(miDeleteSegment),
    // @link(miRearrangeObjects),
    // @link(N1),
    // @link(miSearchForObject),
    // @link(miShowSelectedObjects),
    // @link(miShowHideObjects),
    // @link(miSelectObjectsByName), and
    // @link(mi3D_Colors) menu items.
    miObject: TMenuItem;
    // See @link(acFileOpenExecute) and @link(acFileOpen).
    miOpen: TMenuItem;
    // See @link(tbPanClick) and @link(acPan).
    miPan: TMenuItem;
    // See and @link(acPaste).
    miPaste: TMenuItem;
    // See @link(acFilePrint).
    miPrint: TMenuItem;
    // See @link(miPrintFrequencyClick).
    miPrintFrequency: TMenuItem;
    // See @link(miPrintInitialClick).
    miPrintInitial: TMenuItem;
    // See @link(miRearrangeObjectsClick).
    miRearrangeObjects: TMenuItem;
    // @name is used to redo the users actions.
    // Its OnClick event handler is assigned
    // at runtime in @link(TUndoStack.SetUndoMenuItems).
    // @SeeAlso(tbRedo).
    miRedo: TMenuItem;
    // See @link(acRestoreDefaultViewExecute) and @link(acRestoreDefaultView).
    miRestoreDefaultView: TMenuItem;
    // See @link(miRulerFormatClick).
    miRulerFormat: TMenuItem;
    // See @link(acFileSaveExecute) and @link(acFileSave).
    miSave: TMenuItem;
    // See @link(acFileSaveAsExecute) and @link(acFileSaveAs).
    miSaveAs: TMenuItem;
    // See @link(miSearchForObjectClick).
    miSearchForObject: TMenuItem;
    // @name is the parent menu item for  @link(miSelectAllTop),
    // @link(miSelectAllFront), and @link(miSelectAllSide).
    miSelectAll: TMenuItem;
    // See @link(tbSelectPointClick) and @link(acSelectNode).
    miSelectNodes: TMenuItem;
    // See @link(tbSelectClick) and @link(acSelectObjects).
    miSelectObjects: TMenuItem;
    // See @link(miSelectObjectsByNameClick).
    miSelectObjectsByName: TMenuItem;
    // See @link(tbLassoClick) and @link(acSelectWithLasso).
    miSelectWithLasso: TMenuItem;
    // See @link(miSetSelectedColRowLayerClick).
    miSetSelectedColRowLayer: TMenuItem;
    // See @link(acSetSpacingExecute) and @link(acSetSpacing).
    miSetSpacing: TMenuItem;
    // See @link(tb3DObjectsClick) and @link(acShow3DObjects).
    miShow3DObjects: TMenuItem;
    // See @link(tb3DColorsClick) and @link(acColoredGrid).
    miShowColoredGrid: TMenuItem;
    // See @link(miShowFormulaErrorsClick).
    miShowFormulaErrors: TMenuItem;
    // See @link(acShowFrontGridExecute) and @link(acShowFrontGrid).
    miShowFrontGrid: TMenuItem;
    // See @link(acShowGridShellExecute) and @link(acShowGridShell).
    miShowGridShell: TMenuItem;
    // See @link(miShowHideBitmapsClick).
    miShowHideBitmaps: TMenuItem;
    // See @link(miShowHideObjectsClick).
    miShowHideObjects: TMenuItem;
    // See @link(miShowSelectedObjectsClick).
    miShowSelectedObjects: TMenuItem;
    // See @link(acShowSideGridExecute) and @link(acShowSideGrid).
    miShowSideGrid: TMenuItem;
    // See @link(acShowTopGridExecute) and @link(acShowTopGrid).
    miShowTopGrid: TMenuItem;
    // See @link(acSmoothGridExecute) and @link(acSmoothGrid).
    miSmoothGrid: TMenuItem;
    // See @link(miSolutionMethodClick).
    miSolutionMethod: TMenuItem;
    // See @link(miSteadyFlowClick).
    miSteadyFlow: TMenuItem;
    // See @link(acSubdivideExecute) and @link(acSubdivide).
    miSubdivide: TMenuItem;
    // See @link(miTimeControlClick).
    miTimeControl: TMenuItem;
    // See @link(miTitleAndUnitsClick).
    miTitleAndUnits: TMenuItem;
    // @name is used to undo the users actions.
    // Its OnClick event handler is assigned
    // at runtime in @link(TUndoStack.SetUndoMenuItems).
    // @SeeAlso(tbUndo)
    miUndo: TMenuItem;
    // See @link(miVerticalExaggerationClick)
    // and @link(acEditVerticalExaggeration).
    miVerticalExaggeration: TMenuItem;
    // @name holds the
    // @link(miZoom),
    // @link(miZoomIn),
    // @link(miZoomOut),
    // @link(miPan),
    // @link(miGoTo),
    // @link(miVerticalExaggeration),
    // @link(N3),
    // @link(miShowGridShell),
    // @link(miShowTopGrid),
    // @link(miShowFrontGrid),
    // @link(miShowSideGrid),
    // @link(miShowColoredGrid),
    // @link(miShow3DObjects), and
    // @link(miRestoreDefaultView) menu items.
    miView: TMenuItem;
    // See @link(tbZoomClick) and @link(acZoom).
    miZoom: TMenuItem;
    // See @link(miZoomInClick) and @link(acZoomIn).
    miZoomIn: TMenuItem;
    // See @link(tbZoomOutClick) and @link(acZoomOut).
    miZoomOut: TMenuItem;
    // @name is the main menu of GoPhast.
    mmMainMenu: TMainMenu;
    // @name is a divider between menu items.
    N1: TMenuItem;
    // @name is a divider between menu items.
    N3: TMenuItem;
    // @name is a divider between menu items.
    N4: TMenuItem;
    // @name represents a line above the mostly opened files in the File menu.
    N5: TMenuItem;
    // @name represents a line below the mostly opened files in the File menu.
    N6: TMenuItem;
    // @name is used to open GoPhast files in @link(acFileOpenExecute).
    odOpenDialog: TOpenDialog;
    // @name contains the views of the model in the bottom half of the
    // main form.
    pnlBottom: TPanel;
    // @name contains the views of the model in the top half of the
    // main form.
    pnlTop: TPanel;
    // @name is the status bar at the bottom of the @classname.
    sbMain: TStatusBar;
    // @name is used when saving the PHAST input file to the disk in
    // @link(acExportPhastInputFileExecute).
    sdPhastInput: TSaveDialog;
    // @name is used to save GoPhast files to disk in
    // @link(acFileSaveAsExecute).
    sdSaveDialog: TSaveDialog;
    // The OnTimer event handler for @name is set to @link(ResizeZoomBoxes)
    // in TframeView.@link(TframeView.ZoomBoxResize).
    timTimer: TTimer;
    // @name changes the background color of GoPhast.
    procedure acColorExecute(Sender: TObject);
    // @name sets @link(CurrentTool) to @link(DeleteGridBoundaryTool).
    procedure acDeleteColumnRowExecute(Sender: TObject);
    // @name allows the user to edit data sets by showing @link(TfrmDataSets).
    procedure acEditDataSetsExecute(Sender: TObject);
    // @name allows the user to edit grid lines by showing
    // @link(TfrmGridSpacing).
    procedure acEditGridLinesExecute(Sender: TObject);
    // @name closes GoPhast.
    procedure acExitExecute(Sender: TObject);
    // @name exports the transport input file for PHAST.
    procedure acExportPhastInputFileExecute(Sender: TObject);
    {@name creates a new model.}
    procedure acFileNewModflowModelExecute(Sender: TObject);
    // @name reads an existing file from the disk.
    procedure acFileOpenExecute(Sender: TObject);
    // @name saves a file to the disk with a user-specified name.
    procedure acFileSaveAsExecute(Sender: TObject);
    // @name saves a file to the disk with the existing file name.
    procedure acFileSaveExecute(Sender: TObject);
    // @name sets the font of GoPhast.
    procedure acFontExecute(Sender: TObject);
    // @name generates a grid by showing @link(TfrmGenerateGrid).
    procedure acGenerateGridExecute(Sender: TObject);
    // @name allows the user to set the grid angle using @link(TfrmGridAngle).
    procedure acGridAngleExecute(Sender: TObject);
    // @name looks for help for HelpKeyword.
    // Displays the Help contents for GoPhast.
    procedure acHelpContentsExecute(Sender: TObject);
    // @name sets @link(CurrentTool) to @link(MovingGridBoundaryTool).
    procedure acMoveColumnOrRowExecute(Sender: TObject);
    // @name allows the user to move to a location, object, cell or element
    // by showing @link(TfrmGoTo).
    procedure acMoveToExecute(Sender: TObject);
    // @name restores the 3D view to its default orientation and magnification.
    procedure acRestoreDefaultViewExecute(Sender: TObject);
    // @name sets @link(CurrentTool) to @link(SpacingGridTool).
    procedure acSetSpacingExecute(Sender: TObject);
    // @name allows the user to show or hide the front grid in the 3D view.
    procedure acShowFrontGridExecute(Sender: TObject);
    // @name allows the user to show or hide the grid shell in the 3D view.
    procedure acShowGridShellExecute(Sender: TObject);
    // @name allows the user to show or hide the side grid in the 3D view.
    procedure acShowSideGridExecute(Sender: TObject);
    // @name allows the user to show or hide the top grid in the 3D view.
    procedure acShowTopGridExecute(Sender: TObject);
    // @name allows the user to adjust row, column, or layer widths
    //  by showing @link(TfrmSmoothGrid).
    procedure acSmoothGridExecute(Sender: TObject);
    // @name sets @link(CurrentTool) to @link(SubdivideGridTool).
    procedure acSubdivideExecute(Sender: TObject);
    // @name is used to draw a rotated version of the AddColumn cursor
    // in the top view of the model.
    procedure dcAddColCursorDrawCursor(Sender: TObject; const ABitMap,
      AMask: TBitmap);
    // @name is used to draw a rotated version of the AddRow cursor
    // in the top view of the model.
    procedure dcAddRowCursorDrawCursor(Sender: TObject; const ABitMap,
      AMask: TBitmap);
    // @name is used to draw a rotated version of the MoveColumn cursor
    // in the top view of the model.
    procedure dcMoveColCursorDrawCursor(Sender: TObject; const ABitMap,
      AMask: TBitmap);
    // @name is used to draw a rotated version of the MoveRow cursor
    // in the top view of the model.
    procedure dcMoveRowCursorDrawCursor(Sender: TObject; const ABitMap,
      AMask: TBitmap);
    // @name is used to draw a rotated version of the SetCellSpacing cursor
    // in the top view of the model.
    procedure dcSetSpacingDrawCursor(Sender: TObject; const ABitMap,
      AMask: TBitmap);
    // @name is used to draw a rotated version of the Subdivide cursor
    // in the top view of the model.
    procedure dcSubdivideDrawCursor(Sender: TObject; const ABitMap,
      AMask: TBitmap);
    // @name checks that the model has closed and allows the user an chance to
    // save it if it hasn't been saved.
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    { @name does the following
      1. It creates the observer list which is used to notify objects when they
         are out of date.
      2. It creates an initial model.
      3. It adds PHAST specific functions to each TRbwParser.
      4. It sets up the initial view of the model.
      5. It update the cursors.
      6. It sets some properties of the Application object.
    }
    procedure FormCreate(Sender: TObject); override;
    // @name cleans up by destroying the model and other objects.
    procedure FormDestroy(Sender: TObject); override;
    // When the form is resized, synchronize the rulers with the views
    // of the model.
    procedure FormResize(Sender: TObject);
    {@name responds to the Escape, Return, Delete, PageUp, PageDown
      and arrow keys.

     @unorderedList(
     @item(Escape: delete the last point in a @link(TScreenObject) that is being
       created.)

     @item(Return: finish any @link(TScreenObject)s.)

     @item(Delete: delete the selected @link(TScreenObject) or the selected
       vertices in the selected @link(TScreenObject).)

     @item(Left arrow: increase the selected column.)

     @item(Right arrow: decrease the selected column.)

     @item(Up arrow: increase the selected row.)

     @item(Down arrow: decrease the selected row.)

     @item(Page Up: increase the selected layer.)

     @item(Page Down: decrease the selected layer.))

     @name also implements the shortcuts of the TMenuItems.  It should not
     need to do that; CLX should do it but does not.
    }
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    // @name shows @link(frmColors) to allow the user to specify the
    // color of the light in the 3D view.
    procedure mi3D_ColorsClick(Sender: TObject);
    // @name shows the About form for GoPhast.
    // See link(frmAbout) and @link(TfrmAbout).
    procedure miAboutClick(Sender: TObject);
    // @name allows the user to set the options related to chemistry
    // in PHAST via @link(TfrmChemistryOptions).
    procedure miChemistryOptionsClick(Sender: TObject);
    // @name allows the user to edit the position of bitmaps
    // in GoPhast via @link(TfrmImportBitmap).
    procedure miEditBitmapsClick(Sender: TObject);
    // @name edits @link(TScreenObject TScreenObjects) by calling @link(EditScreenObjects).
    // @seealso(miEditSelectedObjects)
    procedure miEditSelectedObjectsClick(Sender: TObject);
    // See @link(miExamples)
    procedure miExamplesClick(Sender: TObject);
    // @name allows the user to set the options related to a free surface
    // in PHAST via @link(TfrmFreeSurface).
    procedure miFreeSurfaceClick(Sender: TObject);
    // @name allows the user to set the options related to the grid
    // in PHAST via @link(TfrmPhastGridOptions).
    procedure miGridOptionsClick(Sender: TObject);
    // See @link(miHelpOnMainWindow).
    procedure miHelpOnMainWindowClick(Sender: TObject);
    // @name allows the user to adjust how long hints should be visible
    // in GoPhast via @link(TfrmHintDelay).
    procedure miHintDelayClick(Sender: TObject);
    // @name allows the user to import bitmaps
    // into GoPhast via @link(TfrmImportBitmap).
    procedure miImportBitmapClick(Sender: TObject);
    // @name allows the user to import zones from an existing PHAST model
    // into GoPhast via @link(TfrmImportDistributedData).
    procedure miImportDistributedDatabyZoneClick(Sender: TObject);
    // @name allows the user to import DXF files
    // into GoPhast via @link(TfrmImportDXF).
    procedure miImportDXFFileClick(Sender: TObject);
    // @name is used to import scattered point data.
    procedure miImportPointsClick(Sender: TObject);
    // @name allows the user to import Shapefiles
    // into GoPhast via @link(TfrmImportShapefile).
    procedure miImportShapefileClick(Sender: TObject);
    // @name allows the user to set the options related to the print frequency
    // in PHAST via @link(TfrmPrintFrequency).
    procedure miPrintFrequencyClick(Sender: TObject);
    // @name allows the user to set the options related to the initial printing
    // in PHAST via @link(TfrmPrintInitial).
    procedure miPrintInitialClick(Sender: TObject);
    // @name allows the user to change the order of objects
    // in GoPhast via @link(TfrmRearrangeObjects).
    procedure miRearrangeObjectsClick(Sender: TObject);
    // @name allows the user to change the format of the rulers
    // in GoPhast via @link(TfrmRulerOptions).
    procedure miRulerFormatClick(Sender: TObject);
    // @name allows the user to select an object based on what it does
    // in GoPhast via @link(TfrmSearch).
    procedure miSearchForObjectClick(Sender: TObject);
    // @name allows the user to select an object based on its name
    // in GoPhast via @link(TfrmSelectObjects).
    procedure miSelectObjectsByNameClick(Sender: TObject);
    // @name allows the user to change the selected column, row, or layer
    // in GoPhast via @link(TfrmSelectColRowLayer).
    procedure miSetSelectedColRowLayerClick(Sender: TObject);
    // @name allows the user to see errors in formulas
    // in GoPhast via @link(frmFormulaErrors) and @link(TfrmFormulaErrors).
    procedure miShowFormulaErrorsClick(Sender: TObject);
    // @name allows the user to show or hide bitmaps
    // in GoPhast via @link(TfrmShowHideBitmaps).
    procedure miShowHideBitmapsClick(Sender: TObject);
    // @name allows the user to show or hide @link(TScreenObject)s
    // in GoPhast via @link(TfrmShowHideObjects).
    procedure miShowHideObjectsClick(Sender: TObject);
    // @name allows the user to see the names of the selected @link(TScreenObject)s
    // in GoPhast via @link(frmSelectedObjects) and @link(TfrmSelectedObjects).
    procedure miShowSelectedObjectsClick(Sender: TObject);
    // @name allows the user to set the options related to the solution method
    // in PHAST via @link(TfrmSolutionMethod).
    procedure miSolutionMethodClick(Sender: TObject);
    // @name allows the user to set the options related to steady flow
    // in PHAST via @link(TfrmSteadyFlow).
    procedure miSteadyFlowClick(Sender: TObject);
    // @name allows the user to set the options related to time control
    // in PHAST via @link(TfrmTimeControl).
    procedure miTimeControlClick(Sender: TObject);
    // @name allows the user to set the title and units
    // in PHAST via @link(TfrmUnits).
    procedure miTitleAndUnitsClick(Sender: TObject);
    // @name allows the user to change the vertical exaggeration of the model.
    // in GoPhast via @link(TfrmVerticalExaggeration).
    procedure miVerticalExaggerationClick(Sender: TObject);
    // @name allows the user to zoom in to a particular region.
    // @name sets @link(CurrentTool) to @link(ZoomInTool).
    procedure miZoomInClick(Sender: TObject);
    // @name is the event handler for the menu items created by
    // @link(MostRecentlyUsed).
    procedure OpenMostRecentlyUsed(Sender: TObject);
    // @name is used as the OnMouseMove event handler for most components.
    // It sets @link(CursorGrid) to cgNone;
    procedure pnlLowerRightMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    // If the cursor is over one of the dividers between panels on
    // sbMain when the MouseDown event occurs,
    // @name starts moving the divider.
    procedure sbMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    // When the cursor is over one of the dividers between panels in
    // sbMain or when the divider is being moved,
    // @name uses @link(crMoveColumn)
    // as the cursor.  Otherwise it, uses crDefault.
    procedure sbMainMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    // If the user is moving the dividers between two panels on sbMain,
    // @name moves the divider in the MouseUp event handler.
    procedure sbMainMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    // @name sets @link(FileFormat) based on the filter that the user
    // has selected in @link(sdSaveDialog).
    procedure sdSaveDialogFilterChange(Sender: TObject; NewIndex: Integer);
    // This procedure keeps the rulers updated when the horizontal splitter
    // separating the upper and lower halves of the main window is moved.
    procedure splitHorizMoved(Sender: TObject);
    // One vertical splitter separates the top and side views.
    // Another separates the bottom and blank panel.  This
    // procedure updates the upper one when the lower one is moved.
    procedure splitVertBottomMoved(Sender: TObject);
    // One vertical splitter separates the top and side views.
    // Another separates the bottom and blank panel.  This
    // procedure updates the lower one when the upper one is moved.
    procedure splitVertTopMoved(Sender: TObject);
    // @name shows or hides the colors of the selected @link(TDataArray) in the
    // 3D view of the model.
    procedure tb3DColorsClick(Sender: TObject);
    // @name shows or hides the colors of the @link(TScreenObject)s in the
    // 3D view of the model.
    procedure tb3DObjectsClick(Sender: TObject);
    // @name allows the user to add a horizontal boundary to the grid.
    // @name sets @link(CurrentTool) to @link(AddGridBoundaryTool).
    procedure tbAddHorizontalBoundaryClick(Sender: TObject);
    // @name allows the user to add a vertical boundary to the grid.
    // @name sets @link(CurrentTool) to @link(AddGridBoundaryTool).
    procedure tbAddVerticalBoundaryClick(Sender: TObject);
    // @name allows the user to delete a segment of a @link(TScreenObject)
    // @name sets @link(CurrentTool) to @link(DeleteSegmentTool).
    procedure tbDeleteSegmentClick(Sender: TObject);
    // @name allows the user to rotate the grid
    // @name sets @link(CurrentTool) to @link(RotateGridTool).
    procedure tbGridAngleClick(Sender: TObject);
    // @name allows the user to insert a point in a @link(TScreenObject)
    // @name sets @link(CurrentTool) to @link(InsertPointTool).
    procedure tbInsertPointClick(Sender: TObject);
    // @name allows the user to @link(TScreenObject)s
    // by dragging a line around them.
    // @name sets @link(CurrentTool) to @link(LassoTool).
    procedure tbLassoClick(Sender: TObject);
    // @name allows the user to create a line @link(TScreenObject).
    // @name sets @link(CurrentTool) to @link(CreateLineScreenObjectTool).
    procedure tbLineClick(Sender: TObject);
    // @name allows the user to start panning.
    // @name sets @link(CurrentTool) to @link(PanTool).
    procedure tbPanClick(Sender: TObject);
    // @name allows the user to create a point @link(TScreenObject).
    // @name sets @link(CurrentTool) to @link(CreatePointScreenObjectTool).
    procedure tbPointClick(Sender: TObject);
    // @name makes sure all buttons except the current one are up.
    // @param(Sender is the TToolButton that has been depressed.)
    procedure tbPointMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    // @name allows the user to create a polygon @link(TScreenObject).
    // @name sets @link(CurrentTool) to @link(CreateLineScreenObjectTool).
    procedure tbPolygonClick(Sender: TObject);
    // @name allows the user to create a rectangle @link(TScreenObject).
    // @name sets @link(CurrentTool) to @link(CreateRectangleScreenObjectTool).
    procedure tbRectangleClick(Sender: TObject);
    // @name allows the user to select a @link(TScreenObject) by clicking on it.
    // @name sets @link(CurrentTool) to @link(SelectScreenObjectTool).
    procedure tbSelectClick(Sender: TObject);
    // @name allows the user to select a node in a @link(TScreenObject)
    // by clicking on it.
    // @name sets @link(CurrentTool) to @link(SelectPointTool).
    procedure tbSelectPointClick(Sender: TObject);
    // @name shows or hides the grid shell in the
    // 3D view of the model.
    procedure tbShellClick(Sender: TObject);
    // @name allows the user to create a straight-line @link(TScreenObject).
    // @name sets @link(CurrentTool) to @link(CreateStraightLineScreenObjectTool).
    procedure tbStraightLineClick(Sender: TObject);
    // @name allows the user to zoom in on a particular region
    // by selecting it with the mouse.
    // @name sets @link(CurrentTool) to @link(ZoomTool).
    procedure tbZoomClick(Sender: TObject);
    // @name allows the user to zoom out from a particular region.
    // @name sets @link(CurrentTool) to @link(ZoomOutTool).
    procedure tbZoomOutClick(Sender: TObject);
    // When handling the OnMouseUp event, @name checks that the
    // user released the mouse while it was over the TToolButton.
    // If so, ToolButton.Down is set to false and @link(CurrentTool) is
    // set to @nil.
    procedure ToolButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetGridLineDrawingChoice(Sender: TObject);
  private
    // See @link(CanDraw).
    FCanDraw: boolean;
    // See @link(CanEdit).
    FCanEdit: boolean;
    // See @link(ClickedRuler).
    FClickedRuler: TObject;
    // See @link(CurrentTool).
    FCurrentTool: TCustomInteractiveTool;
    // See @link(CursorGrid).
    FCursorGrid: TCursorGrid;
    // See @link(CursorX).
    FCursorX: integer;
    // See @link(CursorY).
    FCursorY: integer;
    // See @link(FileFormat).
    FFileFormat: TFileFormat;
    // See @link(FrontDiscretizationChanged).
    FFrontDiscretizationChanged: boolean;
    // See @link(FrontScreenObjectsChanged).
    FFrontScreenObjectsChanged: boolean;
    // See @link(PhastModel).
    FPhastModel: TPhastModel;
    // @name indicates which boundary between panels on @link(sbMain) the
    // mouse was over when it started to drag the panel.
    FMovingPanelIndex: integer;
    // @name indicates whether or not the user is dragging the boundary
    // between two panels on @link(sbMain).
    FMovingStatusBarPanel: boolean;
    // @name is set to Height in @link(FormCreate) and @link(FormResize).
    // See @link(OldHeight).
    FOldHeight: integer;
    // @name is set to Width in @link(FormCreate) and @link(FormResize).
    // See @link(OldWidth).
    FOldWidth: integer;
    // See @link(SideDiscretizationChanged).
    FSideDiscretizationChanged: boolean;
    // See @link(SideScreenObjectsChanged).
    FSideScreenObjectsChanged: boolean;
    // See @link(TopDiscretizationChanged).
    FTopDiscretizationChanged: boolean;
    // See @link(TopScreenObjectsChanged).
    FTopScreenObjectsChanged: boolean;
    // See @link(IniFile).
    FIniFile: TMemInifile;
    FChangingSelection: boolean;
    FHelpFormat: THelpFormat;
    FRulersVisible: Boolean;
    function ModelUpToDate(const FileName: string;
      CorrectDate: TDateTime): boolean;
    function ModflowUpToDate: boolean;
    function Mf2005UpToDate: boolean;
    function MfNwtUpToDate: boolean;
    function MfLgrUpToDate: boolean;
    function ModpathUpToDate(Model: TCustomModel): boolean;
    function ZoneBudgetUpToDate: boolean;
    function ModelMateUpToDate: boolean;
    // @name is the event handler for @link(TPhastModel.On3DViewChanged).
    // It Invalidates @link(Tframe3DView.glWidModelView).
    procedure Invalidate3DView(Sender: TObject);
    // @name is the event handler for @link(TPhastModel.OnGetZoomBox).
    // @param(VD VD indicates which @link(TQrbwZoomBox2) is desired.)
    // @param(ZoomBox ZoomBox is the @link(TQrbwZoomBox2) in the
    //   @link(TframeView) indicated by VD.)
    procedure GetZoomBox(Sender: TObject; VD: TViewDirection;
      var ZoomBox: TQrbwZoomBox2);
    // @name is the event handler for
    // @link(TPhastModel.OnGetCurrentScreenObject).
    // @param(VD VD indicates which @link(TframeView) to check for the
    //  @link(TframeView.CurrentScreenObject).)
    // @param(ScreenObject ScreenObject is the
    //   @link(TframeView.CurrentScreenObject) in the
    //   @link(TframeView) indicated by VD.)
    procedure GetCurrentScreenObject (Sender: TObject; VD: TViewDirection;
      var ScreenObject: TScreenObject);
    // @name is the event handler for @link(TPhastModel.OnConvertPoint).
    // @param(VD VD indicates which @link(TframeView) that will be used for the
    //   conversion.)
    // @param(RealPoint RealPoint is the TPoint2D to be converted.)
    // @param(ScreenCoordinate ScreenCoordinate is the TPoint corresponding to
    //   RealPoint.)
    procedure ConvertPoint(Sender: TObject; VD: TViewDirection;
      const RealPoint: TPoint2D; var ScreenCoordinate: TPoint);
    // @name is the event handler for
    // @link(TPhastModel.OnScreenObjectSelected) and
    // @link(TPhastModel.OnScreenObjectUnSelected). It enables or disables
    // @link(miEditSelectedObjects),  @link(acAddPolygonsToObject),
    // @link(acAddLinesToObject), and @link(acAddPointsToObject).
    procedure ScreenObjectSelectionChange(Sender: TObject);
    // @name is the event handler for @link(TPhastModel.OnCheckScreenObject).
    // @name sets IsACurrentScreenObject to true if ScreenObject is the
    // @link(TframeView.CurrentScreenObject) on any @link(TframeView).
    procedure CheckScreenObject(Sender: TObject; ScreenObject: TScreenObject;
      var IsACurrentScreenObject: boolean);
    // @name adds FileName to @link(MostRecentlyUsed).
    procedure AddMostRecentlyUsedFile(const FileName: string);
    // @name adjusts SecondToolBar.Left so that is just to the right of
    // FirstToolBar.  It is assumed that FirstToolBar and SecondToolBar
    // are on the same row.
//    procedure AdjustToolbarPositions(FirstToolBar,
//      SecondToolBar: TToolBar);
    // If @link(frmProgressMM), @link(frmSelectedObjects), or @link(frmColors)
    // are visible, @name brings them to the front.
    // @name is the Application.OnActivate event handler.
    procedure BringFormsToFront(Sender: TObject);
    // @name checks that the model has closed and allows the user an chance to
    // save it if it hasn't been saved.
    function CheckModel: boolean;
    // If any individual vertices in a @link(TScreenObject) are selected
    // @name deselects them. @name is used in @link(tbSelectClick).
    procedure ClearSelectedNodes;
    // @name is used to draw the bitmaps for the
    // add-column or add-row cursor for the top view.
    // It is also used to help draw the subdivide cursor.
    // The cursor is drawn with lines
    // that are parallel to the grid.
    procedure DrawAddColRowCursor(const AnImage: TBitmap; Angle: real;
      const CursorComponent: TRbwDynamicCursor);
    // @name is used to draw the bitmaps for the
    // move-column or move-row cursor for the top view.
    // The cursor is drawn with lines
    // that are parallel to the grid.
    procedure DrawMoveColRowCursor(const AnImage: TBitmap; Angle: real;
      const CursorComponent: TRbwDynamicCursor);
    // @name is used to help draw the bitmaps for the
    // subdivide cursor for the top view.  The cursor is drawn with lines
    // that are parallel to the grid.
    procedure DrawSubdivideCursor(const AnImage: TBitmap; Angle: real;
      const CursorComponent: TRbwDynamicCursor);
    // @name writes the transport input file for PHAST.
    // The work is delegated to @link(WritePhastInput).
    procedure ExportFile(FileName: string; RunModel: boolean);
    // @name fills AList with the buttons that can't all be down at the same
    // time.
    procedure FillButtonList(AList: TList);
    // See @link(PhastGrid).
    function GetPhastGrid: TPhastGrid;
    // @name returns true if X indicates that the mouse is over
    // one of the dividers between panels on @link(sbMain).
    // @name is called from @link(sbMainMouseMove).
    function IsOverStatusPanelDivider(const X: integer): boolean;
    // @name saves the file named FileName.
    procedure SaveAFile(FileName: string);
    // If none of the buttons for creating @link(TScreenObject)s, editing
    // the grid or other similar activities is Down. @name sets @link(tbSelect)
    // down.
    procedure SelectDefaultButton;
    // @name toggles the checked state of an action or the action
    // associated with a control.
    // @param(Sender must be the action or control whose state is to be toggled
    // for @name to do anything.  If it isn't, @name does nothing.)
    procedure SetActionChecked(Sender: TObject);
    {@name sets the Down property of all buttons related to CurrentButton
     to false.
     @param(CurrentButton is the button that should remain down.)}
    procedure SetButtonsUp(const CurrentButton: TObject);
    // See @link(CurrentTool).
    procedure SetCurrentTool(const Value: TCustomInteractiveTool);
    // See @link(CursorGrid).
    procedure SetCursorGrid(const Value: TCursorGrid);
    // See @link(FileFormat).
    procedure SetFileFormat(const Value: TFileFormat);
    // See @link(FrontScreenObjectsChanged).
    procedure SetFrontScreenObjectsChanged(const Value: boolean);
    // See @link(SideScreenObjectsChanged).
    procedure SetSideScreenObjectsChanged(const Value: boolean);
    // See @link(TopScreenObjectsChanged).
    procedure SetTopScreenObjectsChanged(const Value: boolean);
    // @name sets TframeView..@link(TframeView.ZoomBox).Cursor and
    // TframeView.@link(TframeView.ZoomBox).ImageBox32.Cursor
    // to ACursor in @link(frameTopView), @link(frameFrontView),
    // and @link(frameSideView).
    procedure SetZB_Cursors(const ACursor: TCursor);
    // @name is the event-handler for Application.OnHint.
    // @name shows a long version of the hint on the status bar (@link(sbMain)).
    procedure ShowHint(Sender: TObject);
    function GetModflowGrid: TModflowGrid;
    function GetGrid: TCustomModelGrid;
    procedure InvalidateViewOfModel;
    function GetModelSelection: TModelSelection;
    procedure SetModelSelection(const Value: TModelSelection);
    procedure NewPosition(Sender: TObject; NewPosition: TPositionStorage);
    procedure CheckInternet;
    procedure SaveModelMateProject;
    procedure SetChangingSelection(const Value: boolean);
    procedure OnOpenFile(Sender: TObject);
    procedure SetCanDraw(const Value: boolean);
    function GetObservationFileName(SD: TSaveDialog): string;
    function GetPredictionFileName(SD: TSaveDialog): string;
    function GetCanDraw: boolean;
    procedure InitializeModflowLgrInputDialog;
    procedure DeleteLastPointInRuler;
    function TestMt3dUsgsLocationOK(Model: TCustomModel): Boolean;
    function TestMt3dmsLocationOK(Model: TCustomModel): Boolean;
    function Mt3dUpToDate: boolean;
    function Mt3dUsgsUpToDate: boolean;
    procedure ScreenOnActiveFormChange(Sender: TObject);
    procedure ExportMt3dFromCommandLine(FileName: string);
    function GetSutraMesh: TSutraMesh3D;
    function MfLgr2UpToDate: boolean;
    function MfOwhmUpToDate: boolean;
    function MfCfpUpToDate: boolean;
    function Mf6UpToDate: boolean;
    procedure AdjustSutraBoundaries;
    function GetHelpFormat: THelpFormat;
    procedure CrossSectionChanged(Sender: TObject);
    procedure EnableSwrObs;
    procedure MeshExportProgress(Sender: TObject);
    function GetFootPrintGrid: TFootprintGrid;
    function FootprintUpToDate: boolean;
    procedure ImportModflowOutputFromCommandLine(AFileName: string; ImportAll: boolean);
    procedure SetRulersVisible(const Value: Boolean);
    function GetDisvGrid: TModflowDisvGrid;
    function GetDisvUsed: Boolean;
    procedure GridTypeChanged(Sender: TObject);
    procedure SetTopDiscretizationChanged(const Value: boolean);
    procedure SetFrontDiscretizationChanged(const Value: boolean);
    procedure SetSideDiscretizationChanged(const Value: boolean);
    procedure EnableModelMate;
    procedure ArrangeToolBarRow(ToolBars: array of TToolBar; ToolBarTop: Integer);
//    procedure NoClick(Sender: TObject);
//    procedure OnSaveFormClose(Sender: TObject; var Action: TCloseAction);
//    procedure YesClick(Sender: TObject);
    { Private declarations }
  protected
    // @name is used to specify the format of the files that
    // can be opened or saved by GoPhast.
    property FileFormat: TFileFormat read FFileFormat write SetFileFormat;
    // @name assigns the event handlers to the undo/redo buttons and
    // undo/redo menu items and prompts the user to save the file once an hour.
    procedure OnAppIdle(Sender: TObject; var Done: Boolean);
    procedure EnableEditRipPlantGroups(Sender: TObject);
  public
    FCubeControl: TRbwModelCube;
    procedure EnableFarmMenuItems;
    procedure UpdateFrontCubeForMeshCrossSection(Sender: TObject);
    procedure UpdateVerticalExaggeration(VerticalExaggeration: Double);
    procedure EnableMeshRenumbering;
    procedure InvalidateImage32AllViews;
    procedure EnableHufMenuItems;
    procedure EnableMt3dmsMenuItems;
    procedure EnableSwrActions;
    property RulersVisible: Boolean read FRulersVisible write SetRulersVisible;
    property ChangingSelection: boolean read FChangingSelection
      write SetChangingSelection;
    procedure EnableManageFlowObservations;
    procedure EnableManageHeadObservations;
    // @name reads an ini file containing the most recently used files
    // and also other settings that will be set by the user for all models
    // rather than being saved in the model.
    // @seealso(WriteIniFile)
    procedure ReadIniFile;
    // @name writes an ini file containing the most recently used files
    // and also other settings that will be set by the user for all models
    // rather than being saved in the model.
    // @seealso(ReadIniFile)
    procedure WriteIniFile;
    procedure SynchronizeViews(SourceView: TViewDirection);
    procedure ReDrawAllViews(Sender: TObject);
    procedure EnableLinkStreams;
    // @name calls TframeView.@link(TframeView.AdjustScales) for
    // @link(frameTopView), @link(frameFrontView),
    // and @link(frameSideView).
    procedure AdjustScales;
    // Setting @name to False causes TframeView.@link(
    // TframeView.Paint)  and @link(Tframe3DView.glWidModelViewRender) to
    // exit immediately without doing anything.
    property CanDraw: boolean read GetCanDraw write SetCanDraw;
    // @name changes the width of the four views of the model. It is used in
    // a workaround of a problem in drawing the front view of the model in
    // @link(TCustomCellSelectionTool.DrawSelectedFrontCells)
    procedure AdjustDrawingWidth;
    procedure BeginSuppressDrawing;
    procedure EndSupressDrawing;
    // @name is used to prevent editing of two or more sets of
    // @link(TScreenObject TScreenObjects) at one time.
    property CanEdit: boolean read FCanEdit write FCanEdit;
    // @name is set to the TRbwRuler in
    // TframeView.@link(TframeView.rulerDblClick)
    // It determines which tab of @link(TfrmRulerOptions) will be visible
    // when it is first displayed.
    property ClickedRuler: TObject read FClickedRuler write FClickedRuler;
    // @name is the @link(TCustomInteractiveTool) that is currently
    // being used to handle the user interaction with
    // TframeView.@link(TframeView.ZoomBox).
    property CurrentTool: TCustomInteractiveTool read FCurrentTool
      write SetCurrentTool;
    // @name is used to indicate which view of the model, if any, the mouse
    // is over.
    property CursorGrid: TCursorGrid read FCursorGrid write SetCursorGrid;
    // @name is used to store the X-coordinate of the current mouse position
    // in the current TframeView.@link(TframeView.ZoomBox).
    property CursorX: integer read FCursorX write FCursorX;
    // @name is used to store the Y-coordinate of the current mouse position
    // in the current TframeView.@link(TframeView.ZoomBox).
    property CursorY: integer read FCursorY write FCursorY;
    // @name allows the user to edit the properties of the selected
    // screen objects.
    function DefaultVE: Real;
    procedure EditScreenObjects;
    // @name is used to indicate that a change has been made to the grid
    // in the front view of the model
    // so that the view of the model needs to be redrawn.
    property FrontDiscretizationChanged: boolean read FFrontDiscretizationChanged
      write SetFrontDiscretizationChanged;
    // Setting @name to true causes the front view of the model to be redrawn.
    property FrontScreenObjectsChanged: boolean read FFrontScreenObjectsChanged
      write SetFrontScreenObjectsChanged;
    // @name is the ini file for GoPhast.  It stores a list of the most
    // recently opened files.
    property Grid: TCustomModelGrid read GetGrid;
    property SutraMesh: TSutraMesh3D read GetSutraMesh;
    // @name is the name of a file containing the initialization data
    // for the program.  It contains the names of the most recently opened
    // files.
    property IniFile: TMemInifile read FIniFile;
    // @name invalidates all the @link(TDataArray)s in the model.
    procedure InvalidateDataSets;
    // @name calls  @link(InvalidateTop), @link(InvalidateFront), and
    // @link(InvalidateSide).
    procedure InvalidateAllViews;
   // @name causes the the front view of the model to be redrawn.
    procedure InvalidateFront;
    // @name invalidates @link(PhastModel).
    procedure InvalidateModel;
    procedure InvalidateScreenObjects;
   // @name causes the the side view of the model to be redrawn.
    procedure InvalidateSide;
   // @name causes the the top view of the model to be redrawn.
    procedure InvalidateTop;
    procedure InitializeView(ModelXWidth, ModelYWidth, ModelHeight: Real);
    property ModflowGrid: TModflowGrid read GetModflowGrid;
    property DisvGrid: TModflowDisvGrid read GetDisvGrid;
    property FootPrintGrid: TFootprintGrid read GetFootPrintGrid;
    // @name is the @link(TPhastModel) that is being edited in GoPhast.
    property PhastModel: TPhastModel read FPhastModel write FPhastModel;
    // @name represents the height of the main form in GoPhast when it is not
    // minimized.  It is used in TPhastModel.@link(TPhastModel.Height).
    property OldHeight: integer read FOldHeight;
    // @name represents the width of the main form in GoPhast when it is not
    // minimized.  It is used in TPhastModel.@link(TPhastModel.Width).
    property OldWidth: integer read FOldWidth;
    // @name is used to read a GoPhast file from the disk.
    // @param(FileName is the name of the file that is to be read.)
    function OpenAFile(const FileName: string): boolean;
    // @name is the @link(TCustomModel.PhastGrid) of @link(PhastModel).
    property PhastGrid: TPhastGrid read GetPhastGrid;
    // @name sets the @link(TScreenObject.Selected) property of
    // all @link(TScreenObject)s to false.  @name returns true if any
    // of them were selected.
    function ResetSelectedScreenObjects: boolean;
    // @name causes the @link(TFrameView.ZoomBox)es to be resized.
    // @name is set to be the OnTimer event handler of @link(timTimer)
    // in TframeView.@link(TframeView.ZoomBoxResize).
    procedure ResizeZoomBoxes(Sender: TObject);
    // @name is the event handler of @link(TPhastModel.OnScreenObjectsChanged).
    // @name is used to update all views of the model.
    procedure ScreenObjectsChanged(Sender: TObject);
    // @name is used to indicate that a change has been made to the grid
    // in the side view of the model
    // so that the view of the model needs to be redrawn.
    property SideDiscretizationChanged: boolean read FSideDiscretizationChanged
      write SetSideDiscretizationChanged;
    // Setting @name to true causes the side view of the model to be redrawn.
    property SideScreenObjectsChanged: boolean read FSideScreenObjectsChanged
      write SetSideScreenObjectsChanged;
    // @name is used to indicate that a change has been made to the grid
    // in the top view of the model
    // so that the view of the model needs to be redrawn.
    property TopDiscretizationChanged: boolean read FTopDiscretizationChanged
      write SetTopDiscretizationChanged;
    // Setting @name to true causes the top view of the model to be redrawn.
    property TopScreenObjectsChanged: boolean read FTopScreenObjectsChanged
      write SetTopScreenObjectsChanged;
    // @name tells all the @link(TDataArray)s what the new grid dimensions are.
    procedure UpdateDataSetDimensions;
    // @name is called from @link(TfrmStartUp).
    procedure UpdateModelSelection;
    // @seealso(ModelSelectionChange).
    // @seealso(TBaseModel.ModelSelection).
    property ModelSelection: TModelSelection read GetModelSelection
      write SetModelSelection;
    // @name is an event handler for when the @link(ModelSelection) changes
    // either when a user opens a file or when it is changed through
    // the menu.
    procedure ModelSelectionChange(Sender: TObject);
    procedure EnableInvertSelection;
    procedure InvalidateGrid;
    property CreateNewCompositeBudgetFile: boolean
      read FCreateNewCompositeBudgetFile write FCreateNewCompositeBudgetFile;
    property ObservationFileName[SD: TSaveDialog]: string read GetObservationFileName;
    property PredictionFileName[SD: TSaveDialog]: string read GetPredictionFileName;
    procedure EnableVisualization;
    procedure UpdateModelCubeBreaks;
    procedure UpdatePermanantDialogBoxAppearances;
    procedure SutraMeshTypeChanged(Sender: TObject);
    property HelpFormat: THelpFormat read GetHelpFormat write FHelpFormat;
    procedure EnableExportHeadObs(Sender: TObject);
    procedure EnableModpathToShapefile;
    function GetFootprintInputFileName: string;
    property DisvUsed:  Boolean read GetDisvUsed;
    procedure SetMt3dCaption;
    procedure EnableCTS;
    procedure EnableManageParameters;
//    property WriteErrorRaised: Boolean read FWriteErrorRaised;
    procedure WMEnterSizeMove(var Message: TMessage); message WM_ENTERSIZEMOVE;
    procedure WMExitSizeMove(var Message: TMessage); message WM_EXITSIZEMOVE;
    procedure EnablePilotPointItems;
    procedure UpdateControlsEnabledOrVisible;
    { Public declarations }
  end;


var
  // @name is the main form of ModelMuse.
  frmGoPhast: TfrmGoPhast;

resourcestring
  StrSelectedLayerD = 'Selected Layer: %d';
  StrSelectedRowD = 'Selected Row: %d';
  StrSelectedColD = 'Selected Col: %d';

const
  VideoUrl = 'https://water.usgs.gov/nrp/gwsoftware/ModelMuse/ModelMuseVideos.html';
  StrCustomization = 'Customization';
  StrShowTips = 'ShowTips';

var
  MfNwtDateVersion1_0_9: TDateTime;
  MfNwtDateVersion1_1_0: TDateTime;
  Mf2005DateVersion1_12: TDateTime;

implementation

{$WARN SYMBOL_PLATFORM OFF}

uses
  {$IFDEF Win64}
  GR32_Blend,
  {$ENDIF}
  Math, frmVerticalExaggerationUnit, CursorsFoiledAgain, frmSubdivideUnit,
  frmGridAngleUnit, frmGridSpacingUnit, frmSmoothGridUnit,
  frmAboutUnit, frmHintDelayUnit, UndoItemsScreenObjects,
  frmRearrangeObjectsUnit, frmSelectColRowLayerUnit, frmSetSpacingUnit,
  frmScreenObjectPropertiesUnit, frmDataSetsUnits,
  InteractiveTools, GIS_Functions, frmRulerOptionsUnit, frmGoToUnit,
  frmFormulaErrorsUnit, GridGeneration, frmGenerateGridUnit,
  frmPhastGridOptionsUnit, frmPrintFrequencyUnit, frmPrintInitialUnit,
  frmSolutionMethodUnit, frmUnitsUnit, frmSteadyFlowUnit, frmTimeControlUnit,
  rwXMLConv, WritePhastUnit, frmChemistryOptionsUnit,
  frmImportDistributedDataUnit, UndoItems, frmFreeSurfaceUnit,
  frmImportShapefileUnit, frmImportDXFUnit, CompressedImageUnit,
  frmImportBitmapUnit, frmSelectImageUnit, frmStartUpUnit, //OpenGL12x,
  frmSearchUnit, frmSelectedObjectsUnit, PhastDataSets, frmShowHideObjectsUnit,
  frmColorsUnit, ModelMuseUtilities, frmProgressUnit, frmShowHideBitmapsUnit,
  frmSelectObjectsUnit, frmImportPointsUnits, GuiSettingsUnit, frmLayersUnit,
  frmModflowOptionsUnit, frmModflowTimeUnit, frmModflowOutputControlUnit,
  frmErrorsAndWarningsUnit, CountObjectsUnit,frmGridValueUnit,
  frmModflowPackagesUnit, frmProgramLocationsUnit, frmGlobalVariablesUnit,
  IniFileUtilities, frmFilesToArchiveUnit, RequiredDataSetsUndoUnit,
  frmLinkStreamsUnit, frmExportShapefileUnit, frmImportModflowUnit,
  frmSelectResultToImportUnit, frmScaleRotateMoveUnit, RbwInternetUtilities,
  frmModflowNameFileUnit, frmImportGriddedDataUnit, FluxObservationUnit,
  frmManageFluxObservationsUnit, TempFiles, ZLib,
  GlobalTypesUnit, JupiterUnit, CheckInternetUnit, 
  frmHUF_LayersUnit, frmBatchFileAdditionsUnit, frmSelectObjectsForEditingUnit,
  frmDataSetValuesUnit, frmExportShapefileObjectsUnit, frmDeleteImageUnit,
  frmPhastLocationUnit, frmImportSurferGrdFileUnitUnit, frmImportDEMUnit,
  frmExportImageUnit, frmManageParametersUnit, frmManageHeadObservationsUnit,
  RealListUnit, ContourExport, frmExportCSVUnit, frmChildModelsUnit,
  frmImportAsciiRasterUnit, CustomModflowWriterUnit, ModflowUnitNumbers,
  ZoneBudgetWriterUnit, ModflowHobUnit, frmDisplayDataUnit, IOUtils,
  ReadPvalUnit, ModflowParameterUnit, OrderedCollectionUnit, ReadGlobalsUnit,
  GlobalVariablesUnit, frmSutraLayersUnit, frmSutraOptionsUnit,
  ModflowPackageSelectionUnit, frmSutraTimesUnit, SutraBoundaryWriterUnit,
  SutraBoundariesUnit, frmSutraOutputControlUnit, IntListUnit,
  SutraFileWriterUnit, SutraInitialConditionsWriterUnit,
  SutraObservationWriterUnit, SutraInputWriterUnit, SutraTimeScheduleWriterUnit,
  SutraOptionsUnit, frmSutraProgramLocationsUnit, frmImportTprogsUnit,
  Generics.Collections, frmCustomizeMeshUnit, frmImportSutraModelResultsUnit,
  frmSutraAngleUnit, frmMeshGenerationControlVariablesUnit, MeshRenumbering,
  frmRenumberingMethodUnit, frmMeshInformationUnit, frmSpecifyMeshUnit,
  CuthillMcKeeRenumbering, MeshRenumberingTypes, frmCropPropertiesUnit,
  frmSoilPropertiesUnit, frmClimateUnit, AdjustSutraBoundaryValuesUnit,
  SutraTimeScheduleUnit, frmFarmAllotmentUnit, frmHelpVersionUnit,
  ModflowCfpWriterUnit, frmSwrTabfilesUnit, frmSwrReachGeometryUnit,
  frmSwrStructuresUnit, frmImportMultipleGriddedDataFilesUnit, ImportQuadMesh,
  Export2DMeshUnit, frmFarmUnit, frmLinkRasterUnit, frmFootprintPropertiesUnit,
  frmFootprintLocationUnit, frmImportFootprintResultsUnit,
  frmModflowRipPlantGroupsUnit, frmExportModelOutlineUnit, SutraLakeWriterUnit,
  frmGeoRefUnit, GeoRefWriterUnit, SutraBoundaryUnit, SutraGeneralFlowNodesUnit,
  SutraGeneralFlowWriterUnit, SutraGeneralTransportWriterUnit, frmFileTypesUnit,
  ArchiveNodeInterface, DrawMeshTypesUnit, frmGridPositionUnit,
  frmSimplifyObjectsCriteriaUnit, ModflowOutputControlUnit,
  frmContaminantTreatmentSystemsUnit, frmObservationComparisonsUnit,
  SutraPestObsWriterUnit, frmManageSutraBoundaryObservationsUnit, frmPestUnit,
  PlProcUnit, PestControlFileWriterUnit, SutraImportUnit, frmSvdaPrepInputUnit,
  frmSupCalcUnit, PestPropertiesUnit,
  frmImportModflow6FeatureModifiedByPestUnit, frmImportSutraFeaturesUnit;

const
  StrDisplayOption = 'DisplayOption';
  StrColor = 'Color';
  StrContour = 'Contour';
  StrDisplayNone = 'None';
  StrNodeFont = 'Node Font';
  StrElementFont = 'Element Font';
  StrHelpFormat = 'Help Format';
  StrMaximized = 'Maximized';
  StrSaveArchive = 'SaveArchive';

resourcestring
  StrModelMate = 'ModelMate';
//  StrEnableModelMate = 'EnableModelMate';
  StrPathlineshp = 'Pathline.shp';
  StrEndpointsAtStartshp = 'EndpointsAtStart.shp';
  StrEndpointsAtEndshp = 'EndpointsAtEnd.shp';
  StrTimeSeriesshp = 'TimeSeries.shp';
  StrYouMustCreateThe = 'You must create the grid before attempting to impor' +
  't gridded data.';
  StrZONEBUDGETDoesNot = 'ZONEBUDGET does not exist at the location you spec' +
  'ified.  Do you still want to export the ZONEBUDGET input files?';
  StrThereWasAProblem = 'There was a problem reading the ModelMate project f' +
  'ile.';
  StrSDoesNotExistAt = '%s does not exist at the location you specified.  Do' +
  ' you still want to export the MODFLOW input files?';
  StrTheCurrentVersion = 'The current version of %s has a more recent date t' +
  'han the version you are using. Do you want to continue?';
  StrMODFLOW2005 = 'MODFLOW-2005';
  StrMODFLOW6 = 'MODFLOW 6';
  StrMODFLOWNWT = 'MODFLOW-NWT';
  StrMODFLOWLGR = 'MODFLOW-LGR Version 1';
  StrMODFLOWLGR2 = 'MODFLOW-LGR Version 2';
  StrMODFLOWOWHM = 'MODFLOW-OWHM';
  StrMODFLOWCFP = 'MODFLOW-CFP';
  StrMODPATH = 'MODPATH';
  StrZONEBUDGET = 'ZONEBUDGET';
  StrModelMuseFilesMust = 'ModelMuse files must use one of the following ext' +
  'ensions: .gpt, .gpb, .xml, or .mmZLib.';
  StrModelMuseFilesMust2 = 'ModelMuse files must have one of the following ' +
  'extensions: ".gpt", ".gpb", ".xml", or ".mmZLib".  The file you tried to ' +
  'open, "%s", does not have one of those extensions.';
  StrYouMustActivateZO = 'You must activate ZONEBUDGET in the MODFLOW Packag' +
  'es and Programs dialog box before running ZONEBUDGET.';
  StrSDoesNotExist = '"%s" does not exist.';
  StrAllOfTheViewsAre = 'All of the views are already zoomed in as far as th' +
  'ey can go.';
  StrAllOfTheViewsAre2 = 'All of the views are already zoomed out as far as ' +
  'they can go.';
  StrYouMustDefineSome = 'You must define some parameters for the HUF packag' +
  'e in the Model|MODFLOW Packages and programs dialog box before you can di' +
  'splay the MODFLOW Hydrogeologic Units dialog box.';
  StrIfYouWantToSave = 'If you want to save disk space, next time save this ' +
  'file as a .mmZLib file instead of a .gpt file.';
  StrMODPATHDoesNotExi = 'MODPATH does not exist at the location you specifi' +
  'ed. Do you still want to export the MODPATH input files?';
  StrMt3dmsDoesNotExi = 'MT3DMS does not exist at the location you specifi' +
  'ed. Do you still want to export the MT3DMS input files?';
  StrMt3dUsgsDoesNotExi = 'MT3D-USGS does not exist at the location you specifi' +
  'ed. Do you still want to export the MT3D-USGS input files?';
  StrYouMustContourDat = 'You must contour data on the grid or mesh to ' +
  'export contours to a Shapefile.';
  StrSorryYouCantDel = 'Sorry; You can''t delete that node.';
  StrDoYouWantToCreat = 'Do you want to create a model archive too?';
  StrSDoesNotContain = '%s does not contain any data.';
  StrTheFileCanNotBe = 'The file can not be opened because it is empty.  You' +
  ' may wish to see if you can open the backup file (with the extension ".ba' +
  'k" or there may be an archived file in the directory containing the file.';
  StrYouMustDefineThe = 'You must define the layer groups in your MODFLOW mo' +
  'del before generating the grid.';
  StrYouMustCreateAGr = 'You must create a grid in order to export Shapefile' +
  's.';
  StrYouMustDefineThe2 = 'You must define the grid before you can export the ' +
  'MODFLOW input files.';
  StrYouMustDefineFootprintGrid = 'You must define the grid before you can export the ' +
  'WellFootprint input files.';
  StrSpaceCharactersAre = 'Space characters are not allowed in the names of ' +
  'MODFLOW input files.';
  StrYouMustSaveYourM = 'You must save your ModelMuse file before creating o' +
  'r updating the ModelMate file.';
  StrTheDirectoryForTh = 'The directory for the ModelMate file does not exis' +
  't';
  StrYouMustActivateMO = 'You must activate MODPATH in the MODFLOW Packages ' +
  'and Programs dialog box before running MODPATH.';
  StrYouMustDefineThe3 = 'You must define the grid before you can export the' +
  ' PHAST input files.';
  StrYouMustSaveYourM2 = 'You must save your ModelMuse file before importing ' +
  'a ModelMate file.';
  StrYourModel = 'your model';
  StrDoYouWantToSave = 'Do you want to save the changes you made to %s?';
  StrYouMustDefineThe4 = 'You must define the grid before you can export the' +
  ' MODFLOW input files.';
  StrMODFLOWLGRDoesNot = 'MODFLOW-LGR does not exist at the location you spe' +
  'cified.  Do you still want to export the MODFLOW input files?';
  StrYouMustActivateMT = 'You must activate MT3DMS or MT3D-USGS in the MODFLOW Packages a' +
  'nd Programs dialog box before running MT3DMS or MT3D-USGS.';
//  StrMT3DMS = 'MT3DMS';
  StrModelMuseIsClosing = 'ModelMuse is closing becuase the ModelMuse.ini fi' +
  'le is locked.';
  StrSubdivideGridElem = 'Subdivide Grid &Elements...';
  StrSubdivideGridEleme = 'Subdivide grid elements|Click down and drag to se' +
  'lect elements to be subdivided.';
  StrSubdivideGridCell = 'Subdivide Grid &Cells...';
  StrSubdivideGridCells = 'Subdivide grid cells|Click down and drag to selec' +
  't cells to be subdivided.';
  StrCombinedModel = 'Combined model';
  StrMODFLOW = 'MODFLOW';
  StrThisModelDoesntH = 'This model doesn''t have any images.';
  StrTheXmlFileYouAr = 'The .xml file you are trying to open is not a valid ' +
  'file for ModelMuse.';
  StrSorryTheFileName = 'Sorry. The file name must be in ASCII characters. ' +
  'Please try again.';
  StrTheRestartFileUse = 'The restart file used for initial conditions must ' +
  'have a different name from the one generated by SUTRA for this model run.';
  StrYouMustGenerateTh = 'You must generate the mesh before attempting to ex' +
  'port the SUTRA input files.';
  StrNoRestartFile = 'The restart file used for initial conditions does ' +
  'not exist.';
  StrGridDataToShapef = '&Grid Data to Shapefile...';
  StrMeshDataToShapef = '&Mesh Data to Shapefile...';
  StrYouMustCreateAMe = 'You must create a mesh in order to export Shapefile' +
  's.';
  StrYouMustGenerate3D = 'You must generate the 3D mesh before attempting to' +
  ' export the SUTRA input files.';
  StrTheBandwidthHasCh = 'The bandwidth has changed from %0:d to %1:d.';
  StrNowMightBeAGood = 'Now might be a good time to activate the Upstream We' +
  'ighting (UPW) Flow and the NWT Solver packages in MODFLOW-NWT. Change them '
  + 'in the "Model|MODFLOW Packages and Programs" dialog box.';
  StrAnErrorOccuredWhe = 'An error occured when trying to convert the model ' +
  'data from a binary format to a text format. Try saving in a different for' +
  'mat such as a ModelMuse binary file (.gpb).';
  StrYouMustCloseTheD = 'You must close the Data Sets dialog box before open' +
  'ing the Global Variables dialog box.';
  StrYouMustCloseTheG = 'You must close the Global Variables dialog box befo' +
  're opening the Data Sets dialog box.';
  StrExportingSUTRA2DM = 'Exporting SUTRA 2D Mesh';
  StrDoYouWantToSaveModel = 'Do you want to save this ModelMuse file now?';
  StrYouRanOutOfMemor = 'You ran out of memory while trying to save your fil' +
  'e. Try saving the file as a .bin or .mmZlib instead of a %s file.';
  StrDoYouWantToSaveModelFileName = 'Do you want to save %s now?';
  StrYouMustDefineAtL = 'You must define at least one crop before you can ed' +
  'it farms.';
  StrReadingTheFileFai = 'Reading the file failed with the error message: "%s"' +
  '. Please check that you are using the most recent version of ModelMuse to' +
  ' open this file.';
  StrReadingTheFileFai2 = 'Reading the file failed with the error message: "' +
  '%0:s". The file may be corrupt. Check for a file named %1:s. It is a prev' +
  'ious version of the file with the extension changed to ".bak".';
  StrMeshData = 'Mesh Data...';
  StrImportMeshDataAs = 'Import mesh data as new objects';
  StrGriddedData = 'Gridded Data...';
  StrImportGriddedData = 'Import gridded data as new objects';
  StrGenerateGrid1 = '&Generate Grid...';
  StrGenerateGrid2 = 'Generate grid';
  StrGenerateMesh1 = '&Generate Mesh...';
  StrGenerateMesh2 = 'Generate mesh';
  StrYouMustSpecifyFootprint = 'You must specify the location of the Footpri' +
  'nt program before creating the WellFootprint input files.';
  StrAMoreRecentVersionFootprint = 'A more recent version of the WellFootprint p' +
  'rogram exists. Do you still want to export the WellFootprint input file.';
  StrTheUsualVersionOf = 'The usual version of MT3DMS only works with output' +
  ' from the single-precision verison of MODFLOW. You appear to be using the' +
  ' double-precision version of MODFLOW. Unless you have recompiled MT3DMS t' +
  'o work with the double-precision version of MODFLOW, you need to use the ' +
  'single-precision verison of MODFLOW.';
  StrThereAreNoContour = 'There are no contours for your data set.';
  StrSorryMT3DMSRestri = 'Sorry. MT3DMS restricts file names to 50 character' +
  's or less.';
  StrNwtVersion = 'The current version of MODFLOW-NWT has a more recent date' +
  ' than the version you are using. However, in the new version, the input files fo' +
  'r the WEL, SFR, and UZF packages have all changed in a way that is incomp' +
  'atible with the version 1.0.9 of MODFLOW-NWT. ModelMuse can support either' +
  ' format. If you continue, ModelMuse will use the format for MODFLOW-NWT ' +
  'version 1.0.9. Do you want to continue?';
  StrCurrentMt3dVersion = 'The current version of %s has a more recent date ' +
  'than the version you are using. '#13#10#13#10'You may be using MT3DMS ins' +
  'tead of MT3D-USGS.'#13#10#13#10'Do you want to continue?';
  StrItLooksLikeYouMi = 'It looks like you might be using MODPATH version 7.' +
  ' You can get MODPATH ' +
  'version 6 from the same web page from which you can get MODPATH version 7.' +
  sLineBreak +
  'Do you want to continue?' +
  sLineBreak +
  'If you are using MODPATH version 7, you should select "NO" and change the ' +
  'location of MODPATH in the "Model|MODFLOW Packages and Programs" dialog box.';
  StrYouMustDefineTheDisv = 'You must define the DISV grid before you can ex' +
  'port the MODFLOW input files.';
  StrYouMustCreateTheMesh = 'You must create the mesh before attempting to impor' +
  't mesh data.';
  StrDoYouWantToConve = 'Do you want to convert the Streamflow Routing packa' +
  'ge in MODFLOW-2005 to the Streamflow Routing package in MODFLOW 6?';
  StrDoYouWantToConti = 'Do you want to continue?';
  StrDoYouWantToConveDisv = 'You already have a DISV grid defined. ' +
  'Do you want to convert the structured grid to a new DIS' +
  'V grid? This can be time-consuming.';
  StrDoYouWantToConveStr = 'Do you want to convert the Stream packa' +
  'ge in MODFLOW-2005 to the Streamflow Routing package in MODFLOW 6?';
  StrDoYouWantToConveHFB = 'Do you want to convert the Horizontal Flow Barri' +
  'ers in MODFLOW-2005 to the Horizontal Flow Barriers in MODFLOW 6?';
  StrDoYouWantToConveObs = 'Do you want to convert the observations in MODFLOW-' +
  '2005 to observation locations in MODFLOW 6?';
  StrDoYouWantToConveMnw2 = 'Do you want to convert the Multinode Wells vers' +
  'ion 2 in MODFLOW-2005 to multi-aquifer wells in MODFLOW 6? You may need ' +
  'to edit the MAW wells after conversion to make sure any formulas used ' +
  'well radius, bottom, and starting head for the MAW wells work.';
  StrYouMustCreateTheDisv = 'You must create the DISV grid before attempting' +
  ' to import gridded data.';
  StrNoObjectsAreSelec = 'No objects are selected.';
  StrSDoesNotExistSutra = '"%s" does not exist. Do you want to export the SU' +
  'TRA input files anyway?';
  StrMODPATH7RequiresA = 'MODPATH 7 requires a binary head output file. You ' +
  'need to change the head output file type under "Model|MODFLOW Output Cont' +
  'rol" and run MODFLOW again before running MODPATH 7.';
  StrMT3DUSGSInputFile = 'MT3D-USGS Input Files';
  StrRunMT3DUSGS = 'Run MT3D-USGS';
  StrExportMT3DUSGSInp = 'Export MT3D-USGS Input Files';
  StrMT3DMSInputFiles = 'MT3DMS Input Files';
  StrRunMT3DMS = 'Run MT3DMS';
  StrExportMT3DMSInput = 'Export MT3DMS Input Files';
  StrDoYouWantToConveUZF = 'Do you want to convert the UZF package in MODFLO' +
  'W-2005 to UZF package in MODFLOW 6?';
  StrDoYouWantToConveSUB = 'Do you want to convert the SUB or SWT packages i' +
  'n MODFLOW-2005 to CSUB package in MODFLOW 6?';
  StrDoYouWantToConveFHB = 'Do you want to convert the FHB to the CHD and WE' +
  'L packages in MODFLOW 6?';
  StrThereWasAnErrorS = 'There was an error saving the file. ' +
  'The error message was "%s". ' +  slinebreak +
  'Be sure that you have permision to write to the directory where you are ' +
  'saving the file. If saving as a ' +
  'mmZLib file, try saving as a bin file instead. In the "File|Save As" dial' +
  'og box, you can also try unchecking the "Save data set values" check box.';
  StrDoYouWantToConveChd = 'Do you want to convert the CHD boundaries in MOD' +
  'FLOW-2005 to the CHD boundaries in MODFLOW 6?';
  StrSomethingWentWrong = 'Something went wrong while trying to save your fi' +
  'le. The error message was "%0:s". Try saving the file as a .bin or .mmZli' +
  'b instead of a %1:s file.';
  StrMODFLOWOWHMMayCra = 'MODFLOW-OWHM may crash if cell lists are printed in ' +
  'models with parameters. Do you want to turn off printing cell lists in the ' +
  'MODFLOW Output Control dialog box?';
  StrPrintingCellLists = 'Printing cell lists has been deactivated.';
  StrSutraObsExtractorex = 'SutraObsExtractor.exe';
  StrSWasNotARecog = '"%s" was not a recognized file type.';
  StrSingularValueDecom = 'Singular value decomposition is deactivated. Do y' +
  'ou want to activate it?';
  StrPESTIsActiveButT = 'PEST is active but the PEST directory "%0:s" does n' +
  'ot exist. Check the PEST directory in "Model|PEST Properties';
  StrPLPROCWasNotFound = 'PLPROC was not found in %s.';
  StrErrorSavingModelMu = 'Error saving ModelMuse initialization file. The e' +
  'rror message was "%s". Check that there is sufficient disk space.';
  StrAMoreRecentVersionPest = 'A more recent version of PEST is available on' +
  ' the PEST home page. Do you want to continue anyway?';
  StrThereWasAnErrorO = 'There was an error opening your file. A common caus' +
  'e of this error is an attempt to open a ModelMuse file created by a newer' +
  ' version of ModelMuse than was used to open the file. This version of Mod' +
  'elMuse that you are using to open the file is %s. Check the';
  StrMT3DCanOnlyBeUse = 'MT3D can only be used with structured grids.';

//e with the version 1.0.9 of MODFLOW-NWT. ModelMuse can support either format. If you continue, ModelMuse will use the format for MODFLOW-NWT version 1.0.9. Do you want to continue?';

{$R *.dfm}

const
  DividerWidth = 2;
  StrPred = '_pred';

var
  // @name represents the date of the current version of
  // MODFLOW-2005. It is set in the initialization section.
  Mf2005Date: TDateTime;
  ModelMateDate: TDateTime;
  MfNwtDate: TDateTime;
  Modpath6Date: TDateTime;
  Modpath7Date: TDateTime;
  MfLgr2Date: TDateTime;
  MfOwhmDate: TDateTime;
  MfCfpDate: TDateTime;
  Mf6Date: TDateTime;
  Mt3dUsgsDate: TDateTime;
  ZoneBudMf6Date: TDateTime;
  FootprintDate: TDateTime;
  PestDate: TDateTime;

const
//  MfNwtDate = 40933; //40907;//40819;
  MfLgrDate = 40315;
  Modpath5Date = 39748;
  zonebudDate = 39925;
//  ModelMateDate = 40669;
  M53dmsDate = 40230;

var
  SbMainHeight: integer;
//  Mt3dUsgsDate: TDate;

//const
//  HELP_TAB = 15;
//  TAB_CONTENTS = 0;
//  TAB_INDEX = -2;
//  TAB_FIND = 1;

//var
//  frmSaveModelDialog: TForm = nil;


procedure TfrmGoPhast.splitVertTopMoved(Sender: TObject);
begin
  if FOtherSplitterMoving or (ModelSelection  in SutraSelection) then
  begin
    Exit;
  end;

  FOtherSplitterMoving := True;
  try
    // One vertical splitter separates the top and side views.
    // Another separates the bottom and blank panel.  This
    // procedure updates the lower one when the upper one is moved.
    if (frameSideView <> nil) and (frameSideView.Width <= 0) then
    begin
      frameSideView.Width := 1;
    end;
    if frameSideView <> nil then
    begin
      if frameSideView.Width = splitVertTop.MinSize then
      begin
        splitVertBottom.Maximized := True;
      end
      else
      begin
        splitVertBottom.Maximized := False;
        frame3DView.Width := frameSideView.Width;
      end;
      splitVertBottom.Invalidate;
    end;
    AdjustScales;
  finally
    FOtherSplitterMoving := False;
  end;
end;

procedure TfrmGoPhast.SynchronizeViews(SourceView: TViewDirection);
var
  CenterPoint: TPoint;
  RealCenterPoint: TPoint2D;
  RotatedCenterPoint: TPoint2D;
  ZInt: integer;
  Z: double;
  XInt: integer;
  XPrime: double;
  YInt: Integer;
  YPrime: double;
  NewPosition: TPositionStorage;
  Mesh: IMesh3D;
  DrawMesh: IDrawMesh;
begin
  if FSynchronizeCount > 0 then
  begin
    Exit;
  end;
  Inc(FSynchronizeCount);
  try
    NewPosition := TPositionStorage.Create;
    try
      case SourceView of
        vdTop:
          begin
            Mesh := frmGoPhast.PhastModel.Mesh3D;
            if Mesh <> nil then
            begin
              if Mesh.Is3DMesh then
              begin
                CenterPoint.x := frameFrontView.ZoomBox.Width div 2;
              end
              else
              begin
                CenterPoint.x := frameTopView.ZoomBox.Width div 2;
              end;
              CenterPoint.Y := frameTopView.ZoomBox.Height div 2;
              RealCenterPoint.x := frameTopView.ZoomBox.X(CenterPoint.x);
              RealCenterPoint.Y := frameTopView.ZoomBox.Y(CenterPoint.y);
//              if PhastModel.ModelSelection  in SutraSelection then
//              begin
//                if PhastModel.Mesh = nil then
//                begin
//                  Exit;
//                end;
                RotatedCenterPoint := PhastModel.DrawMesh.
                  RotateFromRealWorldCoordinatesToMeshCoordinates(RealCenterPoint);
//              end
            end
            else
            begin
              if Grid = nil then
              begin
                Exit;
              end;
              CenterPoint.x := frameTopView.ZoomBox.Width div 2;
              CenterPoint.Y := frameTopView.ZoomBox.Height div 2;
              RealCenterPoint.x := frameTopView.ZoomBox.X(CenterPoint.x);
              RealCenterPoint.Y := frameTopView.ZoomBox.Y(CenterPoint.y);
              RotatedCenterPoint :=
                Grid.RotateFromRealWorldCoordinatesToGridCoordinates(RealCenterPoint);
            end;
            ZInt := frameFrontView.ZoomBox.Height div 2;
            Z := frameFrontView.ZoomBox.Y(ZInt);

            frameFrontView.ZoomBox.Magnification := frameTopView.ZoomBox.Magnification;
            frameSideView.ZoomBox.Magnification := frameTopView.ZoomBox.Magnification;

            SetFrontPosition(RotatedCenterPoint.x, Z);
            SetSidePosition(RotatedCenterPoint.Y, Z);

            NewPosition.Top.XCenter := RealCenterPoint.x;
            NewPosition.Top.YCenter := RealCenterPoint.y;
            NewPosition.Top.Magnification := frameTopView.ZoomBox.Magnification;

            NewPosition.Front.XCenter := RotatedCenterPoint.x;
            NewPosition.Front.YCenter := Z;
            NewPosition.Front.Magnification := frameFrontView.ZoomBox.Magnification;

            NewPosition.Side.XCenter := RotatedCenterPoint.Y;
            NewPosition.Side.YCenter := Z;
            NewPosition.Side.Magnification := frameSideView.ZoomBox.Magnification;
          end;
        vdFront:
          begin
            CenterPoint.x := frameFrontView.ZoomBox.Width div 2;
            CenterPoint.Y := frameTopView.ZoomBox.Height div 2;
            RealCenterPoint.x := frameTopView.ZoomBox.X(CenterPoint.x);
            RealCenterPoint.Y := frameTopView.ZoomBox.Y(CenterPoint.y);
            DrawMesh := PhastModel.DrawMesh;
            if DrawMesh <> nil then
            begin
              RotatedCenterPoint := DrawMesh.
                RotateFromRealWorldCoordinatesToMeshCoordinates(RealCenterPoint);
            end
            else
            begin
              if Grid = nil then
              begin
                Exit;
              end;
              RotatedCenterPoint :=
                Grid.RotateFromRealWorldCoordinatesToGridCoordinates(RealCenterPoint);
            end;

            XInt := frameFrontView.ZoomBox.Width div 2;
            XPrime := frameFrontView.ZoomBox.X(XInt);
            ZInt := frameFrontView.ZoomBox.Height div 2;
            Z := frameFrontView.ZoomBox.Y(ZInt);
            RotatedCenterPoint.X := XPrime;
            if DrawMesh <> nil then
            begin
              RealCenterPoint := DrawMesh.
                RotateFromMeshCoordinatesToRealWorldCoordinates(RotatedCenterPoint);
            end
            else
            begin
              RealCenterPoint :=
                Grid.RotateFromGridCoordinatesToRealWorldCoordinates(RotatedCenterPoint);
            end;

            frameTopView.ZoomBox.Magnification := frameFrontView.ZoomBox.Magnification;
            frameSideView.ZoomBox.Magnification := frameFrontView.ZoomBox.Magnification;

            SetTopPosition(RealCenterPoint.x, RealCenterPoint.y);
            SetSidePosition(RotatedCenterPoint.Y, Z);

            NewPosition.Top.XCenter := RealCenterPoint.x;
            NewPosition.Top.YCenter := RealCenterPoint.y;
            NewPosition.Top.Magnification := frameTopView.ZoomBox.Magnification;

            NewPosition.Front.XCenter := XPrime;
            NewPosition.Front.YCenter := Z;
            NewPosition.Front.Magnification := frameFrontView.ZoomBox.Magnification;

            NewPosition.Side.XCenter := RotatedCenterPoint.Y;
            NewPosition.Side.YCenter := Z;
            NewPosition.Side.Magnification := frameSideView.ZoomBox.Magnification;
          end;
        vdSide:
          begin
            CenterPoint.x := frameTopView.ZoomBox.Width div 2;
            CenterPoint.Y := frameTopView.ZoomBox.Height div 2;
            RealCenterPoint.x := frameTopView.ZoomBox.X(CenterPoint.x);
            RealCenterPoint.Y := frameTopView.ZoomBox.Y(CenterPoint.y);
            if PhastModel.ModelSelection  in SutraSelection then
            begin
              RotatedCenterPoint := RealCenterPoint;
            end
            else
            begin
              if Grid = nil then
              begin
                Exit;
              end;
              RotatedCenterPoint :=
                Grid.RotateFromRealWorldCoordinatesToGridCoordinates(RealCenterPoint);
            end;


            YInt := frameSideView.ZoomBox.Height div 2;
            YPrime := frameSideView.ZoomBox.Y(YInt);
            ZInt := frameSideView.ZoomBox.Width div 2;
            Z := frameSideView.ZoomBox.X(ZInt);
            RotatedCenterPoint.Y := YPrime;
            if PhastModel.ModelSelection  in SutraSelection then
            begin
              RealCenterPoint := RotatedCenterPoint;
            end
            else
            begin
              RealCenterPoint :=
                Grid.RotateFromGridCoordinatesToRealWorldCoordinates(RotatedCenterPoint);
            end;

            frameTopView.ZoomBox.Magnification := frameSideView.ZoomBox.Magnification;
            frameFrontView.ZoomBox.Magnification := frameSideView.ZoomBox.Magnification;

            SetTopPosition(RealCenterPoint.x, RealCenterPoint.y);
            SetFrontPosition(RotatedCenterPoint.X, Z);

            NewPosition.Top.XCenter := RealCenterPoint.x;
            NewPosition.Top.YCenter := RealCenterPoint.y;
            NewPosition.Top.Magnification := frameTopView.ZoomBox.Magnification;

            NewPosition.Front.XCenter := RotatedCenterPoint.X;
            NewPosition.Front.YCenter := Z;
            NewPosition.Front.Magnification := frameFrontView.ZoomBox.Magnification;

            NewPosition.Side.XCenter := YPrime;
            NewPosition.Side.YCenter := Z;
            NewPosition.Side.Magnification := frameSideView.ZoomBox.Magnification;
          end;
        else Assert(False);
      end;
      FPositionList.Submit(NewPosition);
      NewPosition := nil;
    finally
      NewPosition.Free;
    end;
  finally
    Dec(FSynchronizeCount);
  end;
end;

procedure TfrmGoPhast.splitVertBottomMoved(Sender: TObject);
begin
  if FOtherSplitterMoving then
  begin
    Exit;
  end;

  FOtherSplitterMoving := True;
  try
    // One vertical splitter separates the top and side views.
    // Another separates the bottom and blank panel.  This
    // procedure updates the upper one when the lower one is moved.
    if (frame3DView <> nil) and (frame3DView.Width <= 0) then
    begin
      frame3DView.Width := 1;
    end;
    if frameSideView <> nil then
    begin
      if frame3DView.Width = splitVertBottom.MinSize then
      begin
        splitVertTop.Maximized := True;
      end
      else
      begin
        splitVertTop.Maximized := False;
        frameSideView.Width := frame3DView.Width;
      end;
      splitVertTop.Invalidate;
    end;
    AdjustScales;
  finally
    FOtherSplitterMoving := False;
  end;
end;

procedure TfrmGoPhast.acFarmAllotmentExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmFarmAllotment);
end;

procedure TfrmGoPhast.acFarmClimateExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmClimate);
end;

procedure TfrmGoPhast.acFarmCropsExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmCropProperties);
end;

procedure TfrmGoPhast.acFarmSoilsExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmSoilProperties);
end;

procedure TfrmGoPhast.acFileNewModflowModelExecute(Sender: TObject);
begin
  if CheckModel then
  begin
    if frmScreenObjectProperties <> nil then
    begin
      frmScreenObjectProperties.ClearExpressionsAndVariables;
    end;
    CancelCurrentScreenObject;
    ClearFileSaveDialogBoxNames;

    // Hiding instead of freeing these other forms can cause
    // TfrmStartUp.ShowModal to fail.
    FreeAndNil(frmShowHideObjects);
    FreeAndNil(frmDisplayData);
    FreeAndNil(frmGridValue);
    FreeAndNil(frmMeshInformation);
    FreeAndNil(frmGlobalVariables);
    FreeAndNil(frmDataSets);
    FreefrmErrorsAndWarnings;
    FreeFrmSelectedObjects;

    MostRecentlyUsed.FileToIgnore := '';
    MostRecentlyUsed.Capacity := 4;
    {FileNewExecute creates a new model.}
    sdSaveDialog.FileName := '';
    Caption := StrModelName;

    // Clear the undo stack before clearing deleted data sets because
    // copies of TScreenObjects that are in the undo stack may need
    // access to the deleted data sets before they are destroyed.
    UndoStack.Clear;
    PhastModel.DataArrayManager.ClearDeletedDataSets;

    PhastModel.Clear;
    FPositionList.Clear;

    UndoStack.SetUndoActions(acUndo, acRedo);
    tbUndo.Enabled := False;
    tbRedo.Enabled := False;

    PhastModel.ClearExpressionsAndVariables;

    PhastGrid.ColumnCount := -1;
    PhastGrid.RowCount := -1;
    PhastGrid.LayerCount := -1;
    // Formula manager needs FPhastModel to be defined during FPhastModel.Free;
    FPhastModel.Free;
    FPhastModel := nil;

    frameTopView.ZoomBox.Exaggeration := 1;
    frameFrontView.ZoomBox.Exaggeration := 1;
    frameSideView.ZoomBox.Exaggeration := 1;

    if frmColors <> nil then
    begin
      frmColors.HideMe;
      frmColorsUnit.SetDefaults;
    end;
    CreatePhastModel;
    PhastModel.Name := 'Model';
    ResetScreenObjectCount;
    PhastGrid.OnSelectedLayerChange := frameTopView.ItemChange;
    PhastGrid.OnSelectedColumnChange := frameSideView.ItemChange;
    PhastGrid.OnSelectedRowChange := frameFrontView.ItemChange;
    ModflowGrid.OnSelectedLayerChange := frameTopView.ItemChange;
    ModflowGrid.OnSelectedColumnChange := frameSideView.ItemChange;
    ModflowGrid.OnSelectedRowChange := frameFrontView.ItemChange;
    TopDiscretizationChanged := True;
    FrontDiscretizationChanged := True;
    SideDiscretizationChanged := True;
    InvalidateAllViews;

    if not tbSelect.Down then
    begin
      tbSelect.Down := True;
      acSelectObjects.OnExecute(acSelectObjects);
    end;

    if Sender = miModflow2005Model then
    begin
      ShowAForm(TfrmImportModflow);
      if ModelSelection = msUndefined then
      begin
        Application.Terminate;
        Exit;
      end;
      if Grid = nil then
      begin
        with TfrmStartUp.Create(nil) do
        try
        begin
          rgChoice.ItemIndex := 0;
          btnNextClick(nil);
          btnDontCreateGridClick(nil);
        end;
        finally
          Free;
        end;
      end;
    end
    else
    begin
      with TfrmStartUp.Create(nil) do
      begin
        try
          if Sender = acFileNewModflowModel then
          begin
            rgChoice.ItemIndex := Ord(scNewModflow);
          end
          else if Sender = acFileNewPhastModel then
          begin
            rgChoice.ItemIndex := Ord(scNewPhast);
          end
          else if Sender = acNewSutraModel then
          begin
            rgChoice.ItemIndex := Ord(scNewSutra);
          end
          else if Sender = acNewFootprintModel then
          begin
            rgChoice.ItemIndex := Ord(scNewFootprint);
          end
          else
          begin
            Assert(False);
          end;

          btnNextClick(nil);
          self.Hide;
          try
            ShowModal;
          finally
            self.Show;
          end;
        finally
          Free;
        end;
      end;
    end;
    FPositionList.Clear;
    SynchronizeViews(vdTop);
    frame3DView.SetDefaultOrientation;
    EnableInvertSelection;
    Application.Title := StrModelName;
  end;
  EnableDeleteImage;
end;

procedure TfrmGoPhast.splitHorizMoved(Sender: TObject);
begin
  // This procedure keeps the rulers updated when the horizontal splitter
  // separating the upper and lower halves of the main window is moved.
  AdjustScales;

  if pnlBottom.Height <= 0 then
  begin
    pnlBottom.Height := 1;
  end;
  // Make sure that the wrong thing doesn't have it's height changed.

  if SbMainHeight < sbMain.Height then
  begin
    pnlBottom.Height := sbMain.Height - SbMainHeight;
    sbMain.Height := SbMainHeight;
  end;
  // Make sure splitHoriz is above the right thing.
  if splitHoriz.Top > pnlBottom.Top then
  begin
    splitHoriz.Top :=
      pnlBottom.Top - splitHoriz.Height;
  end;
end;

procedure TfrmGoPhast.miSplitObjectAtSelectedVerticesClick(Sender: TObject);
begin
  inherited;
  UndoStack.Submit(TUndoSplitScreenObject.Create);
end;

// modified from http://www.scalabium.com/faq/dct0039.htm
procedure SaveFont(FStream: TCustomIniFile; Section, Ident: string; smFont: TFont);
begin
  FStream.WriteString(Section, Ident + ' Name', smFont.Name);
  FStream.WriteInteger(Section, Ident + ' CharSet', smFont.CharSet);
  FStream.WriteInteger(Section, Ident + ' Color', smFont.Color);
  FStream.WriteInteger(Section, Ident + ' Size', smFont.Size);
  FStream.WriteInteger(Section, Ident + ' Style', Byte(smFont.Style));
end;

// modified from http://www.scalabium.com/faq/dct0039.htm
procedure LoadFont(FStream: TCustomIniFile; Section, Ident: string; smFont: TFont);
begin
  smFont.Name := FStream.ReadString(Section, Ident + ' Name', smFont.Name);
  smFont.CharSet := TFontCharSet(FStream.ReadInteger(Section, Ident +  ' CharSet', smFont.CharSet));
  smFont.Color := TColor(FStream.ReadInteger(Section, Ident + ' Color', smFont.Color));
  smFont.Size := FStream.ReadInteger(Section, Ident + ' Size', smFont.Size);
  smFont.Style := TFontStyles(Byte(FStream.ReadInteger(Section, Ident + ' Style', Byte(smFont.Style))));
end;

procedure TfrmGoPhast.ReadIniFile;
const
  MaxAttempts = 10;
var
  Keys: TStringList;
  Index: integer;
  FileName: string;
  IniFName: string;
  WebIniFileName: string;
  WebIniFile: TMemIniFile;
  ShownURL: Boolean;
  LastTipDate: TDateTime;
  LastCheckInternetDate: TDateTime;
  LocalHelpFormat: THelpFormat;
begin
  FIniFile.Free;
  IniFName := IniFileName(Handle, Application.ExeName);
  for Index := 1 to MaxAttempts do
  begin
    try
      FIniFile:= TMemInifile.Create(IniFName);
      break;
    except on E: EFOpenError do
      begin
        if Index = MaxAttempts then
        begin
          Beep;
          MessageDlg(StrModelMuseIsClosing, mtError, [mbOK], 0);
          Halt(1);
        end;
        Sleep(1000);
      end;
    end;
  end;

  miShowVideoTips.Checked := FIniFile.ReadBool(StrCustomization, StrShowTips, True);
  LoadFont(FIniFile, StrCustomization, StrNodeFont, SutraMesh.NodeFont);
  LoadFont(FIniFile, StrCustomization, StrElementFont, SutraMesh.ElementFont);

  LocalHelpFormat := THelpFormat(FIniFile.ReadInteger(StrCustomization,
    StrHelpFormat, Ord(hfUndefined)));
  if (FHelpFormat <> LocalHelpFormat) and (LocalHelpFormat <> hfUndefined) then
  begin
    FHelpFormat := LocalHelpFormat;
  end;
  case FHelpFormat of
    hfUndefined: ;
    hfLocal: miUseLocalHelp.Checked := True;
    hfWeb: miUseOnlineHelp.Checked := True;
    else Assert(False);
  end;

  if FIniFile.ReadBool(StrCustomization, StrMaximized, False) and FCreatingModel then
  begin
    WindowState := wsMaximized;
  end;

  if FIniFile.ValueExists(StrCustomization, StrSaveArchive) then
  begin
    CreateArchive := FIniFile.ReadBool(StrCustomization, StrSaveArchive, True);
    if CreateArchive then
    begin
      FDefaultCreateArchive := dcaSave;
      acArchiveModel.Checked := True;
    end
    else
    begin
      FDefaultCreateArchive := dcaDontSave;
      acArchiveModel.Checked := False;
    end;

  end;

  DisplayChoices[dcColor] := FIniFile.ReadInteger(StrDisplayOption, StrColor, 0);
  DisplayChoices[dcContour] := FIniFile.ReadInteger(StrDisplayOption, StrContour, 0);
  DisplayChoices[dcNone] := FIniFile.ReadInteger(StrDisplayOption, StrDisplayNone, 0);

  Keys := TStringList.Create;
  try
    IniFile.ReadSection(MRU_Section, Keys);
    for Index := Keys.Count -1 downto 0 do
    begin
      FileName := IniFile.ReadString(MRU_Section, Keys[Index], '');
      if FileExists(FileName) then
      begin
        MostRecentlyUsed.AddFileName(FileName);
      end;
    end;
    PhastModel.ProgramLocations.ReadFromIniFile(IniFile);
    N5.Visible :=  MostRecentlyUsed.MenuItemCount > 0;
    N6.Visible := N5.Visible;

    WebIniFileName := InternetIniFileName(Handle, Application.ExeName);
    if not FileExists(WebIniFileName) then
    begin
      WebIniFile:= TMemInifile.Create(WebIniFileName);
      try
        FIniFile.ReadSection(StrVideoDisplayed, Keys);
        for Index := 0 to Keys.Count - 1 do
        begin
          ShownURL := FIniFile.ReadBool(StrVideoDisplayed, Keys[Index], False);
          WebIniFile.WriteBool(StrVideoDisplayed, Keys[Index], ShownURL);
        end;
        LastTipDate := FIniFile.ReadDateTime(StrCustomization, StrTipDate, 0);
        WebIniFile.WriteDateTime(StrCustomization, StrTipDate, LastTipDate);
        LastCheckInternetDate := FIniFile.ReadDateTime(StrCustomization,
          StrInternetCheckDate, LastTipDate);
        WebIniFile.WriteDateTime(StrCustomization, StrInternetCheckDate,
          LastCheckInternetDate);

        try
          WebIniFile.UpdateFile;
        except
          on EFCreateError do
          begin
            // ignore error.
          end;
        end;
      finally
        WebIniFile.Free;
      end;
    end;

  finally
    Keys.Free;
  end;
end;

procedure TfrmGoPhast.WarningsandErrors1Click(Sender: TObject);
begin
  inherited;
  frmErrorsAndWarnings.Show;
  if frmErrorsAndWarnings.WindowState = wsMinimized then
  begin
    frmErrorsAndWarnings.WindowState := wsNormal
  end;
end;

procedure TfrmGoPhast.WMEnterSizeMove(var Message: TMessage);
begin
  CanDraw := False;
end;

procedure TfrmGoPhast.WMExitSizeMove(var Message: TMessage);
begin
  CanDraw := True;
end;

procedure TfrmGoPhast.SetToolbarPositions;
begin
  ArrangeToolBarRow([tbarFile, tbarEdit, tbarEditScreenObjects, tbarView], 0);
  ArrangeToolBarRow([tbarEditGrid, tbarEditDisv, tlbMesh,
    tbarCreateScreenObject, tlb3dViewMesh, tbarView3D, tbarShowGrid,
    tbarPilotPoints], 34);
end;

procedure TfrmGoPhast.WMMenuSelect(var Msg: TWMMenuSelect);
var
  menuItem : TMenuItem;
  hSubMenu : HMENU;
begin
  inherited; // from TCustomForm

  menuItem := nil;
  if (Msg.MenuFlag <> $FFFF) or (Msg.IDItem <> 0) then
  begin
    if Msg.MenuFlag and MF_POPUP = MF_POPUP then
    begin
      hSubMenu := GetSubMenu(Msg.Menu, Msg.IDItem);
      try
        menuItem := Self.Menu.FindItem(hSubMenu, fkHandle);
      except on ERangeError do
        menuItem := nil;
      end;
    end
    else
    begin
      try
        menuItem := Self.Menu.FindItem(Msg.IDItem, fkCommand);
      except on ERangeError do
        menuItem := nil;
      end;
    end;
    // Modified from http://delphi.about.com/od/vclusing/a/menuitemhints.htm
    // Only display hint windows for menu items for
    // the most recently opened files.
    if (menuItem <> nil) and not (menuItem is TRecentFileMenuItem) then
    begin
      menuItem := nil;
    end;
  end;

  miHint.DoActivateHint(menuItem);
end;

procedure TfrmGoPhast.WriteIniFile;
const
  IniBakExt = '.inibak';
var
  FileName: string;
  Index: integer;
  BackUpFileName: string;
begin
  if FNoIniFile then
  begin
    Exit;
  end;
  for Index := 0 to MostRecentlyUsed.FileNames.Count -1 do
  begin
    FileName := MostRecentlyUsed.FileNames[Index];
    IniFile.WriteString(MRU_Section,
      'FileName' + IntToStr(Index), FileName);
  end;

  While (DisplayChoices[dcColor] > MaxDisplayChoiceCount)
    or (DisplayChoices[dcContour] > MaxDisplayChoiceCount)
    or (DisplayChoices[dcNone] > MaxDisplayChoiceCount)
    do
  begin
    DisplayChoices[dcColor] := DisplayChoices[dcColor] div 2;
    DisplayChoices[dcContour] := DisplayChoices[dcContour] div 2;
    DisplayChoices[dcNone] := DisplayChoices[dcNone] div 2;
  end;

  FIniFile.WriteBool(StrCustomization, StrShowTips, miShowVideoTips.Checked);
  SaveFont(FIniFile, StrCustomization, StrNodeFont, SutraMesh.NodeFont);
  SaveFont(FIniFile, StrCustomization, StrElementFont, SutraMesh.ElementFont);
  FIniFile.WriteInteger(StrCustomization, StrHelpFormat, Ord(FHelpFormat));
  FIniFile.WriteBool(StrCustomization, StrMaximized, WindowState = wsMaximized);

  if FDefaultCreateArchive <> dcaUnknown then
  begin
    FIniFile.WriteBool(StrCustomization, StrSaveArchive,
      FDefaultCreateArchive = dcaSave);
  end;

  FIniFile.WriteInteger(StrDisplayOption, StrColor, DisplayChoices[dcColor]);
  FIniFile.WriteInteger(StrDisplayOption, StrContour, DisplayChoices[dcContour]);
  FIniFile.WriteInteger(StrDisplayOption, StrDisplayNone, DisplayChoices[dcNone]);

  PhastModel.ProgramLocations.WriteToIniFile(IniFile);
  try
    BackUpFileName := ChangeFileExt(IniFile.FileName, IniBakExt);
    if TFile.Exists(BackUpFileName) then
    begin
      TFile.Delete(BackUpFileName);
    end;
    if TFile.Exists(IniFile.FileName) then
    begin
      try
        TFile.Copy(IniFile.FileName, BackUpFileName, True);
      except on EInOutError do
        // do nothing.
      end;
    end;
  except
    on EFileStreamError do
    begin
      Exit;
    end;
    on EInOutError do
    begin
      Exit;
    end;
  end;
  try
    IniFile.UpdateFile;
  except
    on EFileStreamError do
    begin
      if TFile.Exists(BackUpFileName)
        and TFile.Exists(IniFile.FileName) then
      begin
        try
          TFile.Delete(IniFile.FileName);
          TFile.Move(BackUpFileName, IniFile.FileName);
        except on EInOutError do
          begin
            Exit;
          end;
        end;
      end;
    end;
    on E: EWriteError do
    begin
      Beep;
      MessageDlg(Format(StrErrorSavingModelMu, [E.message]), mtWarning, [mbOK], 0);
      if TFile.Exists(BackUpFileName)
        and TFile.Exists(IniFile.FileName) then
      begin
        try
          TFile.Delete(IniFile.FileName);
          TFile.Move(BackUpFileName, IniFile.FileName);
        except on EInOutError do
          begin
            Exit;
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmGoPhast.miZONEBUDGETInputFilesClick(Sender: TObject);
var
  FileName: string;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildFileName: string;
begin
  inherited;
  if not PhastModel.ZoneBudgetIsSelected then
  begin
    Beep;
    MessageDlg(StrYouMustActivateZO, mtWarning, [mbOK], 0);
    Exit;
  end;

  if (sdZonebudgetInput.FileName = '') and (sdModflowInput.FileName <> '') then
  begin
    sdZonebudgetInput.FileName := ChangeFileExt(sdModflowInput.FileName,
      sdZonebudgetInput.DefaultExt);
  end;
  if (sdZonebudgetInput.FileName = '') and (sdSaveDialog.FileName <> '') then
  begin
    sdZonebudgetInput.FileName := ChangeFileExt(sdSaveDialog.FileName,
      sdZonebudgetInput.DefaultExt);
  end;
  if sdZonebudgetInput.Execute then
  begin
    if sdZonebudgetInput.FileName <>
      string(AnsiString(sdZonebudgetInput.FileName)) then
    begin
      Beep;
      MessageDlg(StrSorryTheFileName, mtError, [mbOK], 0);
      Exit;
    end;
    if not TestZoneBudgetLocationOK(PhastModel) or not ZoneBudgetUpToDate then
    begin
      Exit;
    end;

    FileName := sdZonebudgetInput.FileName;
    frmFormulaErrors.sgErrors.BeginUpdate;
    try
      PhastModel.ClearZonebudgetFiles;
      if PhastModel.ModflowPackages.ZoneBudget.IsSelected then
      begin
        PhastModel.ExportZoneBudgetModel(FileName, FRunZoneBudget, False);
      end;
      for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
      begin
        ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
        if ChildModel.ModflowPackages.ZoneBudget.IsSelected then
        begin
          ChildFileName := ChangeFileExt(FileName, '');
          ChildFileName := ChildFileName + '_' + ChildModel.ModelNameForDos;
          ChildFileName := ChangeFileExt(ChildFileName, StrZbzones);
          ChildModel.ExportZoneBudgetModel(ChildFileName, FRunZoneBudget, False);
        end;
      end;
    finally
      frmFormulaErrors.sgErrors.EndUpdate;
    end;

    if frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.Show;
    end;
  end;

end;

function TfrmGoPhast.ZoneBudgetUpToDate: boolean;
var
  WarningMessage: string;
begin
  if ModelSelection = msModflow2015 then
  begin
  result := ModelUpToDate(PhastModel.ProgramLocations.ZoneBudgetLocationMf6,
    ZoneBudMf6Date);
  if not result then
  begin
    Beep;
    WarningMessage := Format(StrTheCurrentVersion, [StrZONEBUDGET]);
    result := (MessageDlg(WarningMessage, mtWarning, [mbYes, mbNo], 0) = mrYes);
  end;
  end
  else
  begin
  result := ModelUpToDate(PhastModel.ProgramLocations.ZoneBudgetLocation,
    zonebudDate);
  if not result then
  begin
    Beep;
    WarningMessage := Format(StrTheCurrentVersion, [StrZONEBUDGET]);
    result := (MessageDlg(WarningMessage, mtWarning, [mbYes, mbNo], 0) = mrYes);
  end;
  end;
end;

//procedure TfrmGoPhast.AdjustToolbarPositions(FirstToolBar,
//  SecondToolBar: TToolBar);
//begin
//  SecondToolBar.Left := FirstToolBar.Left + FirstToolBar.Width + 13;
//end;

procedure TfrmGoPhast.AllowDrawing(Sender: TObject);
begin
  inherited;
  // This is a work-around for a bug in Graphics32 in Win64;
  if ModelSelection <> msUndefined then
  begin
    FNeedFirstRedraw := False;
    CanDraw := True;
    timTimer.Enabled := False;
    timTimer.OnTimer := nil;
    frameTopView.ZoomBox.InvalidateImage32;
    frameFrontView.ZoomBox.InvalidateImage32;
    frameSideView.ZoomBox.InvalidateImage32;
    acRestoreDefaultViewExecute(nil);
  end;
end;

procedure TfrmGoPhast.NewPosition(Sender: TObject; NewPosition: TPositionStorage);
begin
  Inc(FSynchronizeCount);
  try
    frameTopView.ZoomBox.Magnification := NewPosition.Top.Magnification;
    frameFrontView.ZoomBox.Magnification := NewPosition.Front.Magnification;
    frameSideView.ZoomBox.Magnification := NewPosition.Side.Magnification;
    SetTopPosition(NewPosition.Top.XCenter, NewPosition.Top.YCenter);
    SetFrontPosition(NewPosition.Front.XCenter, NewPosition.Front.YCenter);
    SetSidePosition(NewPosition.Side.XCenter, NewPosition.Side.YCenter);

    acPositionForward.Enabled := FPositionList.CanRedo;
    acPositionBackward.Enabled := FPositionList.CanUndo;
  finally
    Dec(FSynchronizeCount);
  end;
end;

procedure TfrmGoPhast.ArrangeToolBarRow(ToolBars: array of TToolBar; ToolBarTop: Integer);
var
  TTop: Integer;
  TLeft: Integer;
  Index: Integer;
begin
  TTop := ToolBarTop;
  for Index := Length(ToolBars) - 1 downto 0 do
  begin
    if ToolBars[Index].Visible then
    begin
      ToolBars[Index].Top := TTop;
      ToolBars[Index].Left := Width - ToolBars[Index].Width;
      TTop := TTop + ToolBars[Index].Height;
    end;
  end;
  TLeft := 11;
  for Index := 0 to Length(ToolBars) - 1 do
  begin
    if ToolBars[Index].Visible then
    begin
      ToolBars[Index].Left := TLeft;
      TLeft := ToolBars[Index].Left + ToolBars[Index].Width;
      ToolBars[Index].Top := ToolBarTop;
    end;
  end;
end;

procedure TfrmGoPhast.FormCreate(Sender: TObject);
var
  AFont: TFont;
  OpenedFile: boolean;
  FileName: string;
  AComponent: TComponent;
  ComponentIndex: integer;
  WorkAreaRect: TRect;
  WorkAreaWidth: Integer;
  WorkAreaHeight: Integer;
  HelpFileName: string;
begin
  inherited;
  FDefaultCreateArchive := dcaUnknown;
  {$IFNDEF PEST}
  acEditObservationComparisons.Visible := False;
  acPEST.Visible := False;
  acRunPest.Visible := False;
  miPEST.Visible := False;
  acImportMf6FeatureFromPest.Visible := False;
  acImportSutraFeaturesFromPest.Visible := False;
  {$ENDIF}
  
  tbarEditScreenObjects.Width := 227;
  tbarView.Width := 176;
  tbarCreateScreenObject.Width := 231;
  tbarView3D.Width := 141;
  tbarEdit.Width := 145;
  tbarEditDisv.Width := tbarEditDisv.Constraints.MinWidth;
  tbarPilotPoints.Top := 34;
  tbarPilotPoints.Left := Width - tbarPilotPoints.Width-13;

  SetToolbarPositions;

//  tbarEdit.Left := tbarFile.Width;
//  tbarEdit.Top := 0;
//  tbarEditScreenObjects.Left := tbarEdit.Left + tbarEdit.Width;
//  tbarEditScreenObjects.Top := 0;
//  tbarView.Left := tbarEditScreenObjects.Left + tbarEditScreenObjects.Width;
//  tbarView.Top := 0;
//  tbarEditGrid.Top := tbarFile.Height;
//  tbarCreateScreenObject.Left := tbarEditGrid.Width;
//  tbarCreateScreenObject.Top := tbarEditGrid.Top;
//  tbarShowGrid.Left := Width - tbarShowGrid.Width;
//  tbarShowGrid.Top := tbarShowGrid.Top;
//  tbarView3D.Left := tbarCreateScreenObject.Left + tbarCreateScreenObject.Width;
//  tbarPilotPoints.Left := tbarView3D.Left + tbarView3D.Width;
//  tbarShowGrid.Left := tbarView3D.Left + tbarView3D.Width;

  frmErrorsAndWarnings.DelayShowing := True;

  RulersVisible := True;

  Screen.OnActiveFormChange := ScreenOnActiveFormChange;
  // Some laptops of the Dept. of the Interior contract have a
  // screen height of 600 pixels so ensure that the height of the main
  // form is never more than that when first created.
  if Height > 600 then
  begin
    Height := 600 ;
  end;

{$IFNDEF LinkedRasters}
  FreeAndNil(miLinkedRasters);
{$ENDIF}

  FRunSutra := True;

  frameTopView.miEditSelectedObjects.Action := acEditSelecteObjects;
  frameFrontView.miEditSelectedObjects.Action := acEditSelecteObjects;
  frameSideView.miEditSelectedObjects.Action := acEditSelecteObjects;

  FPositionList := TPositionList.Create(100);
  FPositionList.OnNewPosition := NewPosition;
  FRunModflow := True;
  FRunModpath := True;
  FRunZoneBudget := True;
  FRunPhast := True;
  FRunModelMate := True;
  FRunMt3dms := True;
  FRunFootprint := True;
  FRunPest := pecPestCheck;
//  FRunSupCalc := True;
  FRunParRep := True;
  FSynchronizeCount := 0;
  FCreatingMainForm := True;
  try
    {$IFDEF MSWINDOWS}
      // Because the CLX Screen.Height doesn't take the taskbar into account,
      // use the VCL Screen object instead under windows to determine the available
      // space on the screen.
      WorkAreaRect := Monitor.WorkareaRect;
    {$ELSE}
    {$IFDEF LINUX}
      // With Linux, the screen area and the available work area are the same.
      // Use the CLX screen object to determine them.
      WorkAreaRect.Top := 0;
      WorkAreaRect.Left := 0;
      WorkAreaRect.Right := Screen.Width;
      WorkAreaRect.Bottom := Screen.Height;
    {$ELSE}
      Assert(False);
    {$ENDIF}
    {$ENDIF}
    WorkAreaWidth := WorkAreaRect.Right - WorkAreaRect.Left;
    WorkAreaHeight := WorkAreaRect.Bottom - WorkAreaRect.Top;
    if Width > WorkAreaWidth then
    begin
      Width := WorkAreaWidth;
    end;
    if Height > WorkAreaHeight then
    begin
      Height := WorkAreaHeight;
    end;

    FCreateArchive := True;
    FCreateArchiveSet := False;
//    Application.HelpFile := ChangeFileExt(Application.ExeName, '.chm');
    Caption := StrModelName;
    miHint := TMenuItemHint.Create(self);

    ExistingColumnSelectionCellColor := frameSideView.ModelCube.SelectionColor;
    ExistingRowSelectionCellColor := frameFrontView.ModelCube.SelectionColor;
    ExistingLayerSelectionCellColor := frameTopView.ModelCube.SelectionColor;

    SbMainHeight := sbMain.Height;
//    ImageDLLLoader.Default.FindDLLs(ProgramPath);
    UndoStack.UndoToolButton := tbUndo;
    UndoStack.RedoToolButton := tbRedo;
    PopUpMode := pmNone;
    Popupparent := nil;
    HelpFileName := IncludeTrailingPathDelimiter(ExtractFileDir(Application.ExeName));
    HelpFileName := IncludeTrailingPathDelimiter(HelpFileName + 'Help');
    HelpFileName := HelpFileName + ExtractFileName(Application.ExeName);
    Application.HelpFile := ChangeFileExt(HelpFileName, '.chm');
    CanEdit := True;
    // Visual CLX patch version 3.10 doesn't work.
    // Shortcuts are broken and something is wrong with redrawing after
    // minimizing and then maximizing.
  //  Assert(PatchedVCLX = 3.9);

    // Adjust the toolbar postions so their isn't blank space between them.
//    AdjustToolbarPositions(tbarFile, tbarEdit);
//    AdjustToolbarPositions(tbarEdit, tbarEditScreenObjects);
//    AdjustToolbarPositions(tbarEditScreenObjects, tbarView);

    Application.OnActivate := BringFormsToFront;

  {$IFDEF Win64}
    CanDraw := False;
  {$ELSE}
    CanDraw := True;
  {$ENDIF}
    FileFormat := ffAscii;
    CreatePhastModel;
    ReadIniFile;

    // make sure the Panels aren't a weird size.

    frameFrontView.ZoomBox.Exaggeration := 20;
    frameSideView.ZoomBox.Exaggeration := 20;

    frameTopView.ZoomBox.Magnification := 0.014;
    frameTopView.ZoomBox.OriginX := -1000.0;
    frameTopView.ZoomBox.OriginY := -1000.0;

    frameFrontView.ZoomBox.Magnification := 0.04;
    frameFrontView.ZoomBox.OriginX := -1000.0;
    frameFrontView.ZoomBox.OriginY := -20;

    frameSideView.ZoomBox.Magnification := 0.015;
    frameSideView.ZoomBox.OriginY := -1000.0;
    frameSideView.ZoomBox.OriginX := -80;

    // Make sure that everything is the right color.
    pnlBottom.ParentColor := True;
    frameFrontView.ZoomBox.ParentColor := True;
    //  pnlLowerRight.ParentColor := True;
    pnlTop.ParentColor := True;
    frameTopView.ZoomBox.ParentColor := True;
    frameSideView.ZoomBox.ParentColor := True;

    frameTopView.ViewDirection := vdTop;
    frameFrontView.ViewDirection := vdFront;
    framesideView.ViewDirection := vdSide;

    // Make sure the rulers are synchronized with the views of the model.
    AdjustScales;

    // create the grid.
    PhastGrid.OnSelectedLayerChange := frameTopView.ItemChange;
    PhastGrid.OnSelectedColumnChange := frameSideView.ItemChange;
    PhastGrid.OnSelectedRowChange := frameFrontView.ItemChange;
    ModflowGrid.OnSelectedLayerChange := frameTopView.ItemChange;
    ModflowGrid.OnSelectedColumnChange := frameSideView.ItemChange;
    ModflowGrid.OnSelectedRowChange := frameFrontView.ItemChange;

    // Adjust the paint box sizes.
    if frameTopView <> nil then
    begin
      frameTopView.ZoomBox.ZoomBy(1);
    end;
    if frameFrontView <> nil then
    begin
      frameFrontView.ZoomBox.ZoomBy(1);
    end;
    if frameSideView <> nil then
    begin
      frameSideView.ZoomBox.ZoomBy(1);
    end;
    Cursor := crArrow;

    // update the cursors.
    dcMoveColCursor.RedrawCursor;
    dcMoveRowCursor.RedrawCursor;
    dcAddColCursor.RedrawCursor;
    dcAddRowCursor.RedrawCursor;
    dcSubdivide.RedrawCursor;
    dcSetSpacing.RedrawCursor;

  {$IFDEF LINUX}
    // The Arial font on Linux doesn't look good.
    // Use Helvetica instead.
    Font.Name := 'Helvetica';
  {$ENDIF}

    AFont := TFont.Create;
    try
      AFont.Assign(Font);
      AFont.Size := AFont.Size - 2;
      Font := AFont;
      GlobalFont := Font;
  //    Application.Font := Font;
      AFont.Size := AFont.Size + 2;
      Font := AFont;
    finally
      AFont.Free;
    end;
    GlobalColor := Color;

  //  Application.Font := Font;
  //  for Index := 0 to ComponentCount - 1 do
  //  begin
  //    AComponent := Components[Index];
  //    if AComponent is TMenuItem then
  //    begin
  //      AMenuItem := TMenuItem(AComponent);
  //      QPopupMenu_setFont(AMenuItem.Handle, Font.Handle);
  //    end;
  //  end;

    // Set the icon for the application.
//    Application.Icon := Icon;

    // Make sure the undo stack is updated.
    Application.OnIdle := OnAppIdle;

    // Make sure the background color of the hints is white.
    // (On some systems, it is black.)
    Application.HintColor := clWhite;

    // make Sure the color of the font used to draw hints is
    // black instead of white.
  //  Application.Style.DrawHint := DrawHint;

    // Show the hint in the status bar.
    Application.OnHint := ShowHint;

    PhastModel.UpToDate := True;

//    frmScreenObjectProperties := TfrmScreenObjectProperties.Create(Application);

    OpenedFile := False;
    if ParamCount > 0 then
    begin
      try
      FileName := ParamStr(1);
      if FileExists(FileName) then
      begin
        FileName := ExpandFileName(FileName);
        PhastModel.UpToDate := True;
        try
          OpenedFile :=OpenAFile(FileName);
        except on E: Exception do
          begin
            Beep;
            MessageDlg(E.Message, mtError, [mbOK], 0);
            raise;
          end;
        end;
        HandleCommandLineParameters(FileName);
      end
      else
      begin
        MessageDlg(Format(StrSDoesNotExist, [FileName]), mtError, [mbOK], 0);
      end;
      except On E: Exception do
        begin
          Application.Terminate;
        end;

      end;
    end;

    if not OpenedFile then
    begin
      if ShowAForm(TfrmStartUp) <> mrOK then
      begin
        Exit
      end;
    end;
    FPositionList.Clear;
    StoreInitalPosition;

    FOldWidth := Width;
    FOldHeight := Height;

    tbSelectClick(nil);
    frame3DView.SetDefaultOrientation;

    // Save the widths of the TToolBar components.
    for ComponentIndex := 0 to ComponentCount - 1 do
    begin
      AComponent := Components[ComponentIndex];
      if AComponent is TToolBar then
      begin
        AComponent.Tag := TToolBar(AComponent).Width;
      end;
    end;
    CheckInternet;
  finally
    FCreatingMainForm := false;
  end;
  {$IFDEF Win64}
  CanDraw := False;
  FNeedFirstRedraw := True;
//  FormShow(nil);
  {$ENDIF}
  FSaveTime := Now;
end;

procedure TfrmGoPhast.FormResize(Sender: TObject);
begin
  // When the form is resized, synchronize the rulers with the views
  // of the model.

  if frameSideView.Width > ClientWidth - splitVertTop.Width then
  begin
    frameSideView.Width := ClientWidth - splitVertTop.Width;
    frame3DView.Width := frameSideView.Width;
  end;

  if pnlBottom.Height > ClientHeight - cbControlBar.Height
    - splitHoriz.Height then
  begin
    pnlBottom.Height := ClientHeight - cbControlBar.Height - splitHoriz.Height;
  end;

  AdjustScales;

  if WindowState = wsNormal then
  begin
    FOldWidth := Width;
    FOldHeight := Height;
  end;

  { TODO :
When maximizing, make non modal forms (TfrmSelectedObjects,
TfrmShowHideObjects) stay on top (or else give the user the ability to 
make them stay on top). See TForm.FormStyle}
  inherited;
end;

procedure TfrmGoPhast.FormShow(Sender: TObject);
begin
  inherited;
  // This is a work-around for a bug in Graphics32 in Win64;
  if not CanDraw then
  begin
    timTimer.OnTimer := AllowDrawing;
    timTimer.Interval := 1000;
    timTimer.Enabled := True;
  end;
  if not FAlreadyShown then
  begin
    FAlreadyShown := True;
    frmFormulaErrors.DelayShowing := False;
    tmrImportErrors.Enabled := True;
  end;
  frmErrorsAndWarnings.DelayShowing := False;

end;

procedure TfrmGoPhast.tbPanClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbPan.OnMouseDown(tbPan, mbLeft, [ssLeft], 0, 0);
  end;

  if tbPan.Down then
  begin
    // Try to start panning.
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbPan);

    SetZB_Cursors(crHandFlat);
  end
  else
  begin
    SetZB_Cursors(crArrow);
  end;
  CurrentTool := PanTool;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.tbZoomClick(Sender: TObject);
var
  CanZoomIn: boolean;
  Procedure SetCursor(Frame: TframeView);
  begin
    if (Frame <> nil)
      and (Frame.ZoomBox.CanZoomOut) then
    begin
      Frame.ZoomBox.Cursor := crZoom;
      Frame.ZoomBox.Image32.Cursor := crZoom;
    end;
  end;
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbZoom.OnMouseDown(tbZoom, mbLeft, [ssLeft], 0, 0);
  end;

  if tbZoom.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbZoom);

    CanZoomIn := (frameTopView.ZoomBox.CanZoomIn)
      or (frameFrontView.ZoomBox.CanZoomIn)
      or (frameSideView.ZoomBox.CanZoomIn);
    SetZB_Cursors(crArrow);
    // Set the cursor for the top view.
    SetCursor(frameTopView);

    // Set the cursor for the front view.
    SetCursor(frameFrontView);

    // Set the cursor for the side view.
    SetCursor(frameSideView);

    if not CanZoomIn then
    begin
      tbZoom.Down := False;
      Beep;
      MessageDlg(StrAllOfTheViewsAre, mtInformation, [mbOK], 0, mbOK);
      Exit;
    end;
    CurrentTool := ZoomTool;
  end
  else
  begin
    // Reset the cursors.
    SetZB_Cursors(crArrow);
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.tbZoomOutClick(Sender: TObject);
var
  CanZoomOut: boolean;
  Procedure SetCursor(Frame: TframeView);
  begin
    if (Frame <> nil)
      and (Frame.ZoomBox.CanZoomOut) then
    begin
      Frame.ZoomBox.Cursor := crZoomOut;
      Frame.ZoomBox.Image32.Cursor := crZoomOut;
    end;
  end;
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbZoomOut.OnMouseDown(tbZoomOut, mbLeft, [ssLeft], 0, 0);
  end;

  if tbZoomOut.Down then
  begin
    CanZoomOut := (frameTopView.ZoomBox.CanZoomOut)
      or (frameFrontView.ZoomBox.CanZoomOut)
      or (frameSideView.ZoomBox.CanZoomOut);
    // Make sure all buttons except the current one are up.
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbZoomOut);

    // Set the cursors.
    SetZB_Cursors(crArrow);

    // Set the cursor for the top view.
    SetCursor(frameTopView);

    // Set the cursor for the front view.
    SetCursor(frameFrontView);

    // Set the cursor for the side view.
    SetCursor(frameSideView);

    if not CanZoomOut then
    begin
      tbZoomOut.Down := False;
      Beep;
      MessageDlg(StrAllOfTheViewsAre2, mtInformation, [mbOK], 0, mbOK);
      Exit;
    end;

    CurrentTool := ZoomOutTool;

  end
  else
  begin
    // Reset the cursors.
    SetZB_Cursors(crArrow);
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.FormDeactivate(Sender: TObject);
begin
  inherited;
  // allow cut, copy, and paste on other non-modal forms to work properly
  // by deactivating shortcuts when the main form becomes inactive.
  // Re-enable the Shortcuts in FormActivate.
  acCut.ShortCut := 0;
  acCopy.ShortCut := 0;
  acPaste.ShortCut := 0;
end;

procedure TfrmGoPhast.FormDestroy(Sender: TObject);
begin
  frame3DView.glWidModelView.Visible := False;
  FreeAndNil(frmGridValue);
  FreeAndNil(frmDisplayData);
//  OutputDebugString('SAMPLING ON');
  WriteIniFile;
  IniFile.Free;
  FPositionList.Free;

  // Hide doesn't work when the application is shutting down.

  {
  // It might take a while to completely get rid of the grid and objects
  // so hide the forms first.
  if frmShowHideObjects <> nil then
  begin
    frmShowHideObjects.Hide;
  end;
  Hide;
  }

  // The UndoStack may contain items that would cause an error when being
  // destroyed if the Observers like global variables were still talking
  // to them. Because of that, DisconnectObservers is called before clearing
  // the UndoStack
  FPhastModel.DisconnectObservers;
  UndoStack.Clear;
  // Get rid of the model.
//  FreeAndNil(FPhastModel);
  // Formula manager needs FPhastModel to be defined during FPhastModel.Free;
  FPhastModel.Free;
  FPhastModel := nil;
//  FPhastModel := nil;
  inherited;
//  OutputDebugString('SAMPLING OFF');
end;

procedure TfrmGoPhast.miVerticalExaggerationClick(Sender: TObject);
begin
  // change the vertical exaggeration of the model.
  ShowAForm(TfrmVerticalExaggeration);
end;

procedure TfrmGoPhast.miViewMeshInformation1Click(Sender: TObject);
begin
  inherited;
  if frmMeshInformation = nil then
  begin
    Application.CreateForm(TfrmMeshInformation, frmMeshInformation);
  end;
  frmMeshInformation.GetData;
  frmMeshInformation.Show;
end;

procedure TfrmGoPhast.AdjustDrawingWidth;
begin
  if frameFrontView.Width > 11 then
  begin
    frameSideView.Width := frameSideView.Width + 10;
  end
  else
  begin
    frameSideView.Width := frameSideView.Width - 7;
  end;
  splitVertTopMoved(frameSideView);
  InvalidateAllViews;
end;

procedure TfrmGoPhast.AdjustScales;
begin
  // Synchronize the horizontal and vertical scales with the views of the grid.

  if frameTopView <> nil then
  begin
    frameTopView.AdjustScales;
  end;

  if frameFrontView <> nil then
  begin
    frameFrontView.AdjustScales;
  end;

  if frameSideView <> nil then
  begin
    frameSideView.AdjustScales;
  end;
end;

procedure TfrmGoPhast.tbPointClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbPoint.OnMouseDown(Sender, mbLeft, [ssLeft], 0, 0);
  end;

  if tbPoint.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbPoint);
    // Set the cursor.
    SetZB_Cursors(crPointArrow);
    CurrentTool := CreatePointScreenObjectTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.miZoomInClick(Sender: TObject);
var
  CanZoomIn: boolean;
  Procedure SetCursor(Frame: TframeView);
  begin
    if (Frame <> nil)
      and (Frame.ZoomBox.CanZoomIn) then
    begin
      Frame.ZoomBox.Cursor := crZoomIn;
      Frame.ZoomBox.Image32.Cursor := crZoomIn;
    end;
  end;
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbZoomIn.OnMouseDown(tbZoomIn, mbLeft, [ssLeft], 0, 0);
  end;

  if tbZoomIn.Down then
  begin
    CanZoomIn := (frameTopView.ZoomBox.CanZoomIn)
      or (frameFrontView.ZoomBox.CanZoomIn)
      or (frameSideView.ZoomBox.CanZoomIn);
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbZoomIn);
    SetZB_Cursors(crArrow);
    // Set the cursor for the top view.
    SetCursor(frameTopView);

    // Set the cursor for the front view.
    SetCursor(frameFrontView);

    // Set the cursor for the side view.
    SetCursor(frameSideView);
    // If you can't zoom in, warn the user.
    if not CanZoomIn then
    begin
      tbZoomIn.Down := False;
      Beep;
      MessageDlg(StrAllOfTheViewsAre, mtInformation, [mbOK], 0, mbOK);
      Exit;
    end;
    CurrentTool := ZoomInTool;
  end
  else
  begin
    // Reset the cursors.
    SetZB_Cursors(crArrow);
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.miEditGeoRefClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmGeoRef);
end;

procedure TfrmGoPhast.miModelResultsClick(Sender: TObject);
begin
  inherited;
  case ModelSelection of
    msPhast: ;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT, msModflowFmp,
      msModflowCfp, msModflow2015:
      begin
        with TfrmSelectResultToImport.Create(nil) do
        begin
          try
            if SelectFiles then
            begin
              ShowModal;
            end;
          finally
            Free;
          end;
        end;
      end;
    msSutra22, msSutra30:
      begin
        ShowAForm(TfrmImportSutraModelResults);
      end;
    msFootPrint:
      begin
        ShowAForm(TfrmImportFootprintResults)
      end;
    else Assert(False);
  end;

end;

function TfrmGoPhast.ModelMateUpToDate: boolean;
var
  WarningMessage: string;
begin
  result := ModelUpToDate(PhastModel.ProgramLocations.ModelMateLocation, ModelMateDate);
  if not result then
  begin
    Beep;
    WarningMessage := Format(StrTheCurrentVersion, [StrModelMate]);
    result := (MessageDlg(WarningMessage, mtWarning, [mbYes, mbNo], 0) = mrYes);
  end;
end;

procedure TfrmGoPhast.UpdateControlsEnabledOrVisible;
var
  ControlList: TList<TComponent>;
  ShowControls: Boolean;
  ControlIndex: Integer;
  AComponent: TComponent;
begin
  {$IFDEF PEST}
  acImportMf6FeatureFromPest.Visible := PhastModel.ModelSelection = msModflow2015;
  acImportSutraFeaturesFromPest.Visible := PhastModel.ModelSelection in SutraSelection;
  {$ELSE}
  acImportMf6FeatureFromPest.Visible := False;
  acImportSutraFeaturesFromPest.Visible := False;
  {$ENDIF}

  case PhastModel.ModelSelection of
    msUndefined: ; // ignore
    msPhast:
      begin
        frameSideView.Visible := True;
        splitVertTop.Visible := True;
        frameTopView.ModelCube.ZOrigin := zoBottom;
        frameFrontView.ModelCube.YOrigin := yoSouth;
        acSubdivide.Visible := True;
        acSubdivide.Caption := StrSubdivideGridElem;
        acSubdivide.Hint := StrSubdivideGridEleme;
        btnRunModel.DropDown := nil;
        miExportShapefile.Caption := StrGridDataToShapef;
        acGenerateGrid.Caption := StrGenerateGrid1;
        acGenerateGrid.Hint := StrGenerateGrid2;
        frameTopView.ZoomBox.Exaggeration := 1;
        acExportSutra2DMesh.Enabled := False;
        pnlBottom.Visible := True;
        splitHoriz.Visible := True;
        acDeleteColumnRow.Enabled := True;
        acMoveColumnOrRow.Enabled := True;
        acEditGridLines.Enabled := True;
        acAddColumn.Enabled := True;
        acAddRow.Enabled := True;
        acSetSpacing.Enabled := True;
        acSmoothGrid.Enabled := True;
        acSelectColRowLay.Enabled := True;
        miSetSelectedColRowLayer.Enabled := True;
        acGridDragRotate.Enabled := True;
        acGridAngle.Enabled := True;
        acEditObservationComparisons.Visible := False;
        acEditSutraFluxObs.Visible := False;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msModflow2015:
      begin
        frameSideView.Visible := not DisvUsed;
        splitVertTop.Visible := not DisvUsed;
        frameTopView.ModelCube.ZOrigin := zoTop;
        frameFrontView.ModelCube.YOrigin := yoNorth;
        acSubdivide.Visible := not DisvUsed;
        acSubdivide.Caption := StrSubdivideGridCell;
        acSubdivide.Hint := StrSubdivideGridCells;
        btnRunModel.DropDown := pmExportModel;
        miExportShapefile.Caption := StrGridDataToShapef;
        acGenerateGrid.Caption := StrGenerateGrid1;
        acGenerateGrid.Hint := StrGenerateGrid2;
        frameTopView.ZoomBox.Exaggeration := 1;
        acExportSutra2DMesh.Enabled := False;
        pnlBottom.Visible := True;
        splitHoriz.Visible := True;
        acDeleteColumnRow.Enabled := not DisvUsed;
        acMoveColumnOrRow.Enabled := not DisvUsed;
        acEditGridLines.Enabled := not DisvUsed;
        acAddColumn.Enabled := not DisvUsed;
        acAddRow.Enabled := not DisvUsed;
        acSetSpacing.Enabled := not DisvUsed;
        acSmoothGrid.Enabled := not DisvUsed;
        acSelectColRowLay.Enabled := not DisvUsed;
        miSetSelectedColRowLayer.Enabled := not DisvUsed;
        acGridDragRotate.Enabled := not DisvUsed;
        acGridAngle.Enabled := not DisvUsed;
        {$IFDEF PEST}
        acEditObservationComparisons.Visible := True;
        acEditObservationComparisons.Enabled := PhastModel.PestUsed;
        {$ELSE}
        acEditObservationComparisons.Visible := False;
        {$ENDIF}
        acEditSutraFluxObs.Visible := False;
      end;
    msSutra22, msSutra30:
      begin
        frameSideView.Visible := False;
        splitVertTop.Visible := False;
        frameTopView.ModelCube.ZOrigin := zoTop;
        frameFrontView.ModelCube.YOrigin := yoSouth;
        acSubdivide.Visible := False;
//        acSubdivide.Caption := 'Subdivide Grid &Cells...';
//        acSubdivide.Hint := 'Subdivide grid cells|'
//          + 'Click down and drag to select cells to be subdivided.';
        btnRunModel.DropDown := pmExportModelSutra;
        miExportShapefile.Caption := StrMeshDataToShapef;
        acGenerateGrid.Caption := StrGenerateMesh1;
        acGenerateGrid.Hint := StrGenerateMesh2;
        acExportSutra2DMesh.Enabled := True;
        pnlBottom.Visible := True;
        splitHoriz.Visible := True;
        acDeleteColumnRow.Enabled := False;
        acMoveColumnOrRow.Enabled := False;
        acEditGridLines.Enabled := False;
        acAddColumn.Enabled := False;
        acAddRow.Enabled := False;
        acSetSpacing.Enabled := False;
        acSmoothGrid.Enabled := False;
        acSelectColRowLay.Enabled := False;
        miSetSelectedColRowLayer.Enabled := False;
        acGridDragRotate.Enabled := False;
        acGridAngle.Enabled := False;
        {$IFDEF PEST}
        acEditObservationComparisons.Visible := True;
        acEditObservationComparisons.Enabled := PhastModel.PestUsed;
        acEditSutraFluxObs.Visible := True;
        acEditSutraFluxObs.Enabled := PhastModel.PestUsed;
        {$ELSE}
        acEditObservationComparisons.Visible := False;
        acEditSutraFluxObs.Visible := False;
        {$ENDIF}
      end;
    msFootPrint:
      begin
        frameSideView.Visible := False;
        splitVertTop.Visible := False;
        frameTopView.ModelCube.ZOrigin := zoTop;
        frameFrontView.ModelCube.YOrigin := yoNorth;
        acSubdivide.Visible := True;
        acSubdivide.Caption := StrSubdivideGridCell;
        acSubdivide.Hint := StrSubdivideGridCells;
        btnRunModel.DropDown := nil;
        miExportShapefile.Caption := StrGridDataToShapef;
        acGenerateGrid.Caption := StrGenerateGrid1;
        acGenerateGrid.Hint := StrGenerateGrid2;
        frameTopView.ZoomBox.Exaggeration := 1;
        acExportSutra2DMesh.Enabled := False;
        pnlBottom.Visible := False;
        splitHoriz.Visible := False;
        acDeleteColumnRow.Enabled := False;
        acMoveColumnOrRow.Enabled := False;
        acEditGridLines.Enabled := False;
        acAddColumn.Enabled := False;
        acAddRow.Enabled := False;
        acSetSpacing.Enabled := False;
        acSmoothGrid.Enabled := False;
        acSelectColRowLay.Enabled := False;
        miSetSelectedColRowLayer.Enabled := False;
        acGridDragRotate.Enabled := True;
        acGridAngle.Enabled := True;
        acEditObservationComparisons.Visible := False;
        acEditSutraFluxObs.Visible := False;
      end
    else Assert(False);
  end;

  acDisvGrid.Visible := ModelSelection = msModflow2015;
  acStructuredGrid.Visible := ModelSelection = msModflow2015;

  acFootprintProperties.Visible := ModelSelection = msFootPrint;
  acRipPlantGroups.Visible := ModelSelection = msModflowFmp;
  comboZCount.Enabled := ModelSelection <> msFootPrint;
  if not comboZCount.Enabled then
  begin
    comboZCount.ItemIndex := 2;
  end;

  EnableVisualization;

  if frameSideView.Visible and (frameSideView.Left < splitVertTop.Left) then
  begin
    splitVertTop.Left := frameSideView.Left;
  end;

  EnableGridItems;

  miDISV.Visible := DisvUsed;
  acShowCellNumbers.Visible := DisvUsed;
  tbarEditDisv.Visible := DisvUsed;
  if tbarEditDisv.Visible then
  begin
    tbarEditDisv.Left := 0;
    tbarEditDisv.Top := tbarEditGrid.Top
  end;

  miMesh.Visible := PhastModel.ModelSelection  in SutraSelection;
  acShowTopMesh.Visible := (PhastModel.ModelSelection  in SutraSelection) or DisvUsed;
  acShowFrontMesh.Visible := (PhastModel.ModelSelection  in SutraSelection) or DisvUsed;

  EnableSwrActions;
//  miSWR.Visible := ModelSelection in [msModflow, msModflowNWT, msModflowFmp];

  acFarmCrops.Visible := PhastModel.ModelSelection = msModflowFmp;
  acFarmSoils.Visible := PhastModel.ModelSelection = msModflowFmp;
  acFarmClimate.Visible := PhastModel.ModelSelection = msModflowFmp;
  acFarmAllotment.Visible := PhastModel.ModelSelection = msModflowFmp;
  acEditFarms.Visible := PhastModel.ModelSelection = msModflowFmp;
  miFarmProcessDialogBoxes.Visible := PhastModel.ModelSelection = msModflowFmp;

  EnableFarmMenuItems;

  tbarEditGrid.Visible := (PhastModel.ModelSelection in ModelsWithGrid) and not DisvUsed;
  tbarView3D.Visible := tbarEditGrid.Visible;

  acImportModelResults.Enabled :=
    PhastModel.ModelSelection in [msModflow, msModflowLGR, msModflowLGR2,
      msModflowNWT, msModflowFmp, msModflowCfp, msSutra22, msSutra30, msFootPrint,
      msModflow2015];
  acImportSutraModelResults.Enabled :=
    PhastModel.ModelSelection  in SutraSelection;

  tlbMesh.Visible := PhastModel.ModelSelection  in SutraSelection;
  tlb3dViewMesh.Visible := (PhastModel.ModelSelection in SutraSelection) or DisVUsed;
  acImportSutraMesh.Enabled := PhastModel.ModelSelection in SutraSelection;
  acImportSutraFiles.Enabled := PhastModel.ModelSelection in SutraSelection;

  acExportModpath.Enabled :=
    PhastModel.ModelSelection in ModflowSelection;
  miImportDistributedDatabyZone.Enabled := PhastModel.ModelSelection = msPhast;
  miChildModels.Enabled := PhastModel.ModelSelection in [msModflowLGR,
    msModflowLGR2, msModflowFmp];

  acExportPhastInputFile.Enabled := PhastModel.ModelSelection = msPhast;
  acRunModflow.Enabled := PhastModel.ModelSelection = msModflow;
  acRunModflowLgr.Enabled := PhastModel.ModelSelection in [msModflowLGR,
    msModflowLGR2];
  acRunModflowNWT.Enabled := PhastModel.ModelSelection = msModflowNWT;
  acRunSutra.Enabled := PhastModel.ModelSelection  in SutraSelection;
  acRunModflowFmp.Enabled := PhastModel.ModelSelection = msModflowFmp;
  acRunModflowCfp.Enabled := PhastModel.ModelSelection = msModflowCfp;
  acRunFootprint.Enabled := PhastModel.ModelSelection = msFootPrint;
  acRunModflow6.Enabled := PhastModel.ModelSelection = msModflow2015;

  miLayers.Enabled :=
    PhastModel.ModelSelection in ModflowSelection;
  EnableHufMenuItems;
  EnableMt3dmsMenuItems;
  miGeneral.Enabled :=
    PhastModel.ModelSelection in ModflowSelection;
  miTime.Enabled :=
    PhastModel.ModelSelection in ModflowSelection;
  miOutputControl.Enabled :=
    PhastModel.ModelSelection in ModflowSelection;
  miPackages.Enabled :=
    PhastModel.ModelSelection in ModflowSelection;
  miProgramLocations.Enabled :=
    PhastModel.ModelSelection in ModflowSelection;
  EnableManageParameters;
  miModflowNameFile.Enabled :=
    PhastModel.ModelSelection in ModflowSelection;
  EnableLinkStreams;
  EnableManageFlowObservations;
  EnableManageHeadObservations;
  miObservationType.Enabled :=
    PhastModel.ModelSelection in ModflowSelection;

  miTitleAndUnits.Enabled := PhastModel.ModelSelection = msPhast;
  miGridOptions.Enabled := PhastModel.ModelSelection = msPhast;
  miChemistryOptions.Enabled := PhastModel.ModelSelection = msPhast;
  miSolutionMethod.Enabled := PhastModel.ModelSelection = msPhast;
  miSteadyFlow.Enabled := PhastModel.ModelSelection = msPhast;
  miTimeControl.Enabled := PhastModel.ModelSelection = msPhast;
  miFreeSurface.Enabled := PhastModel.ModelSelection = msPhast;
  miPrintInitial.Enabled := PhastModel.ModelSelection = msPhast;
  miPrintFrequency.Enabled := PhastModel.ModelSelection = msPhast;
  miPHASTProgramLocation.Enabled := PhastModel.ModelSelection = msPhast;
  EnableSwrActions;
  UpdateModelCubeBreaks;

  ControlList := TList<TComponent>.Create;
  try
    // MODFLOW
    ControlList.Add(miPackages);
    ControlList.Add(acLayers);
    ControlList.Add(miMF_HydrogeologicUnits);
    ControlList.Add(miTime);
    ControlList.Add(miOutputControl);
    ControlList.Add(miGeneral);
    ControlList.Add(miProgramLocations);
    ControlList.Add(miLinkSFRStreams);
//    ControlList.Add(miManageParameters);
    ControlList.Add(miManageHeadObservations);
    ControlList.Add(miManageFluxObservations);
    ControlList.Add(miModflowNameFile);
    ControlList.Add(miObservationType);
    ControlList.Add(miChildModels);
    ControlList.Add(acRunModflow);
    ControlList.Add(acRunModflowLgr);
    ControlList.Add(acRunModflowNwt);
    ControlList.Add(acRunModflowFMP);
    ControlList.Add(acRunModflowCfp);
    ControlList.Add(acExportModpath);
    ControlList.Add(acExportZoneBudget);
    ControlList.Add(acRunMt3dms);
    ControlList.Add(acRunModflow6);


    ShowControls := PhastModel.ModelSelection in ModflowSelection;
    for ControlIndex := 0 to ControlList.Count - 1 do
    begin
      AComponent := ControlList[ControlIndex];
      if AComponent is TMenuItem then
      begin
        TMenuItem(AComponent).Visible := ShowControls;
      end
      else if AComponent is TAction then
      begin
        TAction(AComponent).Visible := ShowControls;
      end
      else
      begin
        Assert(False);
      end;
    end;

    // PHAST
    ControlList.Clear;
    ControlList.Add(miTitleAndUnits);
    ControlList.Add(miGridOptions);
    ControlList.Add(miChemistryOptions);
    ControlList.Add(miSolutionMethod);
    ControlList.Add(miSteadyFlow);
    ControlList.Add(miTimeControl);
    ControlList.Add(miFreeSurface);
    ControlList.Add(miPrintInitial);
    ControlList.Add(miPrintFrequency);
    ControlList.Add(miPHASTProgramLocation);
    ControlList.Add(acExportPhastInputFile);

    ShowControls := PhastModel.ModelSelection = msPhast;
    for ControlIndex := 0 to ControlList.Count - 1 do
    begin
      AComponent := ControlList[ControlIndex];
      if AComponent is TMenuItem then
      begin
        TMenuItem(AComponent).Visible := ShowControls;
      end
      else if AComponent is TAction then
      begin
        TAction(AComponent).Visible := ShowControls;
      end
      else
      begin
        Assert(False);
      end;
    end;


    // SUTRA
    ControlList.Clear;
    ControlList.Add(acSutraLayers);
    ControlList.Add(acSutraOptions);
    ControlList.Add(acSutraTimes);
    ControlList.Add(acSutraOutputControl);
    ControlList.Add(acSutraProgramLocations);
    ControlList.Add(acRunSutra);
    ControlList.Add(miCustomizeSutraMesh);

    ShowControls := PhastModel.ModelSelection in SutraSelection;
    for ControlIndex := 0 to ControlList.Count - 1 do
    begin
      AComponent := ControlList[ControlIndex];
      if AComponent is TMenuItem then
      begin
        TMenuItem(AComponent).Visible := ShowControls;
      end
      else if AComponent is TAction then
      begin
        TAction(AComponent).Visible := ShowControls;
      end
      else
      begin
        Assert(False);
      end;
    end;

    ShowControls := PhastModel.ModelSelection = msFootPrint;
    acFootprintProgramLocation.Visible := ShowControls;
    acRunFootprint.Visible := ShowControls;

    N8.Visible := False;
    N10.Visible := False;
  finally
    ControlList.Free;
  end;

  EnableModelMate;
  EnableCTS;

  acPEST.Enabled := ModelSelection in (ModflowSelection + SutraSelection);

end;

procedure TfrmGoPhast.ModelSelectionChange(Sender: TObject);
const
  MinimumToolbarLeft = 11;
  ToolbarExtraWidth = 15;
  procedure UpdateRunShortCut(Action: TAction);
  begin
    if Action.Enabled then
    begin
      Action.ShortCut := ShortCut(Word('E'), [ssCtrl]);
      btnRunModel.Action := Action;
      btnRunModel.Caption := '';
    end
    else
    begin
      Action.ShortCut := 0;
    end;
  end;
begin
  if csDestroying in ComponentState then
  begin
    Exit;
  end;
  case PhastModel.ModelSelection of
    msUndefined: ; // ignore
    msPhast:
        acPhastActive.Checked := True;
    msModflow:
        acModflowActive.Checked := True;
    msModflowLGR:
        acModflowLgrActive.Checked := True;
    msModflowLGR2:
        acModflowLgr2Active.Checked := True;
    msModflowNWT:
        acModflowNwtActive.Checked := True;
    msModflowFmp:
        acModflowFmpActive.Checked := True;
    msModflowCfp:
        acModflowCfpActive.Checked := True;
    msSutra22:
        acSutra22Active.Checked := True;
    msSutra30:
        acSutra30Active.Checked := True;
    msFootPrint:
        acFootPrintActive.Checked := True;
    msModflow2015:
        acModflow6Active.Checked := True;
    else
      Assert(False);
  end;

  UpdateControlsEnabledOrVisible;

  frameTopView.AdjustScales;

  // update the cursors.
  dcMoveColCursor.RedrawCursor;
  dcMoveRowCursor.RedrawCursor;
  dcAddColCursor.RedrawCursor;
  dcAddRowCursor.RedrawCursor;
  dcSubdivide.RedrawCursor;
  dcSetSpacing.RedrawCursor;

  if PhastModel.ModelSelection  in SutraSelection then
  begin
    miGriddedData.Caption := StrMeshData;
    miGriddedData.Hint := StrImportMeshDataAs;
  end
  else
  begin
    miGriddedData.Caption := StrGriddedData;
    miGriddedData.Hint := StrImportGriddedData;
  end;

//  acImportGriddedDataFiles.Enabled :=
//    not (PhastModel.ModelSelection  in SutraSelection);
  SetToolbarPositions;


//  tbarShowGrid.Left := Width - tbarShowGrid.Width- ToolbarExtraWidth;
//  if tbarEditGrid.Visible then
//  begin
//    tbarEditGrid.Left := MinimumToolbarLeft;
//    tbarCreateScreenObject.Left := tbarEditGrid.Left + tbarEditGrid.Width
//      + ToolbarExtraWidth;
//    tbarView3D.Left := tbarCreateScreenObject.Left
//      + tbarCreateScreenObject.Width + ToolbarExtraWidth;
//    tbarView3D.Top := tbarCreateScreenObject.Top;
//    tbarShowGrid.Left := tbarView3D.Left + tbarView3D.Width;
//    tbarShowGrid.Top := tbarView3D.Top;
//
//  end
//  else
//  begin
//    tlbMesh.Left := MinimumToolbarLeft;
//    tbarCreateScreenObject.Left := tlbMesh.Left + tlbMesh.Width
//      + ToolbarExtraWidth;
//    tlb3dViewMesh.Left := tbarCreateScreenObject.Left
//      + tbarCreateScreenObject.Width + ToolbarExtraWidth;
//    tlb3dViewMesh.Top := tbarCreateScreenObject.Top;
//    tlbMesh.Top := tbarCreateScreenObject.Top;
//    tbarShowGrid.Left := tlb3dViewMesh.Left + tlb3dViewMesh.Width;
//  end;

  UpdateRunShortCut(acExportPhastInputFile);
  UpdateRunShortCut(acRunModflow);
  UpdateRunShortCut(acRunModflowLgr);
  UpdateRunShortCut(acRunModflowNWT);
  UpdateRunShortCut(acRunModflowFMP);
  UpdateRunShortCut(acRunModflowCFP);
  UpdateRunShortCut(acRunSUTRA);
  UpdateRunShortCut(acRunFootprint);
  UpdateRunShortCut(acRunModflow6);

  if DisvUsed then
  begin
    SetActionChecked(acDisvGrid);
  end
  else
  begin
    SetActionChecked(acStructuredGrid);
  end;

end;

procedure TfrmGoPhast.miMF_HydrogeologicUnitsClick(Sender: TObject);
begin
  inherited;
  if PhastModel.HufParameters.Count > 0 then
  begin
    ShowAForm(TfrmHUF_Layers);
  end
  else
  begin
    Beep;
    MessageDlg(StrYouMustDefineSome, mtWarning, [mbOK], 0);
  end;
end;

procedure TfrmGoPhast.miModflowNameFileClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmModflowNameFile);
end;

procedure TfrmGoPhast.ModflowReference1Click(Sender: TObject);
begin
  inherited;
  ShowHelp('Introduction', frmGoPhast.HelpFormat);
//  Application.HelpJump('Introduction');
//  HelpRouter.HelpJump('', 'Introduction');
end;

function TfrmGoPhast.ModflowUpToDate: boolean;
begin
  result := True;
  case PhastModel.ModelSelection of
    msModflow: result := Mf2005UpToDate;
    msModflowLGR: result := MfLgrUpToDate;
    msModflowLGR2: result := MfLgr2UpToDate;
    msModflowNWT: result := MfNwtUpToDate;
    msModflowFmp: result := MfOwhmUpToDate;
    msModflowCFP: result := MfCfpUpToDate;
    msModflow2015: result := Mf6UpToDate;
    else Assert(False);
  end;
end;

function TfrmGoPhast.ModpathUpToDate(Model: TCustomModel): boolean;
var
  WarningMessage: string;
begin
  result := False;
  case Model.ModflowPackages.ModPath.MpathVersion of
    mp5:
      begin
        result := ModelUpToDate(PhastModel.
          ProgramLocations.ModPathLocation, Modpath5Date);
      end;
    mp6:
      begin
        result := ModelUpToDate(PhastModel.
          ProgramLocations.ModPathLocationVersion6, Modpath6Date);
        if result and ModelUpToDate(PhastModel.
          ProgramLocations.ModPathLocationVersion6, Modpath7Date) then
        begin
          Beep;
          result := (MessageDlg(StrItLooksLikeYouMi, mtWarning,
            [mbYes, mbNo], 0) = mrYes);
          if not Result then
          begin
            Exit;
          end;
        end;
      end;
    mp7:
      begin
        result := ModelUpToDate(PhastModel.
          ProgramLocations.ModPathLocationVersion7, Modpath7Date);
      end;
    else
      Assert(False);
  end;
  if not result then
  begin
    Beep;
    WarningMessage := Format(StrTheCurrentVersion, [StrMODPATH]);
    result := (MessageDlg(WarningMessage, mtWarning, [mbYes, mbNo], 0) = mrYes);
  end;
end;

function TfrmGoPhast.Mt3dUsgsUpToDate: boolean;
var
  WarningMessage: string;
begin
  result := ModelUpToDate(PhastModel.ProgramLocations.Mt3dUsgsLocation, Mt3dUsgsDate);
  if not result then
  begin
    Beep;
    WarningMessage := Format(StrTheCurrentVersion, [StrMt3dUsgs]);
    result := (MessageDlg(WarningMessage, mtWarning, [mbYes, mbNo], 0) = mrYes);
  end
end;


function TfrmGoPhast.Mt3dUpToDate: boolean;
var
  WarningMessage: string;
begin
  result := ModelUpToDate(PhastModel.ProgramLocations.Mt3dmsLocation, M53dmsDate);
  if not result then
  begin
    Beep;
    WarningMessage := Format(StrTheCurrentVersion, [StrMT3DMS]);
    result := (MessageDlg(WarningMessage, mtWarning, [mbYes, mbNo], 0) = mrYes);
//  end
//  else
//  begin
//    if PhastModel.ModelSelection = msModflowNWT then
//    begin
//      result := ModelUpToDate(PhastModel.ProgramLocations.Mt3dmsLocation, Mt3dUsgsDate);
//      if not result then
//      begin
//        Beep;
//        WarningMessage := Format(StrCurrentMt3dVersion, [StrMt3dUsgs]);
//        result := (MessageDlg(WarningMessage, mtWarning, [mbYes, mbNo], 0) = mrYes);
//      end;
//    end;
  end;
end;

procedure TfrmGoPhast.GridTypeChanged(Sender: TObject);
begin
  ModelSelectionChange(Sender);
end;

procedure TfrmGoPhast.CreatePhastModel;
begin
  PhastModel := TPhastModel.Create(self);
  PhastModel.ContourFont := Font;
  PhastModel.OnGetZoomBox := GetZoomBox;
  PhastModel.OnGetCurrentScreenObject := GetCurrentScreenObject;
  PhastModel.OnConvertPoint := ConvertPoint;
  PhastModel.OnScreenObjectSelected := ScreenObjectSelectionChange;
  PhastModel.OnScreenObjectUnSelected := ScreenObjectSelectionChange;
  PhastModel.OnCheckScreenObject := CheckScreenObject;
  PhastModel.On3DViewChanged := Invalidate3DView;
  PhastModel.OnCrossSectionChanged := CrossSectionChanged;

  PhastModel.OnModelSelectionChange := ModelSelectionChange;
  PhastModel.OnScreenObjectsChanged := ScreenObjectsChanged;
  PhastModel.OnRefreshScreenObjects := UpdateDisplay;
  PhastModel.OnHeadOBsChanged := EnableExportHeadObs;

  PhastModel.OnGridTypeChanged := GridTypeChanged;
  PhastModel.GuiSettings := TGuiSettings.Create;

  PhastModel.ModflowPackages.RipPackage.OnSelectionChange :=
    EnableEditRipPlantGroups;

  ModelSelectionChange(PhastModel);

  PhastModel.UpdateTimeLists;
  PhastModel.Name := 'PhastModel';
  PhastModel.UpToDate := True;

  FCreatingModel := True;
  try
    ReadIniFile;
  finally
    FCreatingModel := False;
  end;

end;

procedure TfrmGoPhast.CrossSectionChanged(Sender: TObject);
begin
  InvalidateViewOfModel;
  InvalidateAllViews;
end;

procedure TfrmGoPhast.miDataSetstoCSVClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmExportCSV);
end;

procedure TfrmGoPhast.acDefaultCrossSectionExecute(Sender: TObject);
begin
  inherited;
  frmGoPhast.UndoStack.Submit(
    TUndoSpecifyCrossSection.Create(
    PhastModel.DrawMesh.DefaultCrossSectionLocation));
end;

procedure TfrmGoPhast.acDeleteColumnRowExecute(Sender: TObject);
begin
  inherited;
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbDeleteColumnRow.OnMouseDown(tbDeleteColumnRow, mbLeft, [ssLeft], 0, 0);
  end;

  if tbDeleteColumnRow.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbDeleteColumnRow);

    // Set the cursors.
    SetZB_Cursors(crArrow);
  end
  else
  begin
    // Reset the cursors.
    SetZB_Cursors(crArrow);
  end;
  CurrentTool := DeleteGridBoundaryTool;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.acDeletePilotPointExecute(Sender: TObject);
begin
  inherited;
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbDeletePilotPoint.OnMouseDown(tbDeletePilotPoint, mbLeft, [ssLeft], 0, 0);
  end;

  if tbDeletePilotPoint.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbDeletePilotPoint);
    // Set the cursors.
    SetZB_Cursors(crDeletePilotPoint);
    // don't draw a rectangle around the selected screen objects.
    CurrentTool := DeletePilotPointTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;

end;

procedure TfrmGoPhast.DrawSubdivideCursor(const AnImage: TBitmap;
  Angle: real; const CursorComponent: TRbwDynamicCursor);
const
  LineSeparation = 5.1;
var
  LineLength: real;
  X1, Y1, X2, Y2: integer;
  lsCa, llSa, lsSa, llCa: real;
  procedure DrawLine;
  begin
    with AnImage.Canvas do
    begin
      MoveTo(X1, Y1);
      LineTo(X2, Y2);
    end
  end;
begin
  // This procedure is used to help draw the bitmaps for the
  // subdivide cursor for the top view.  The cursor is drawn with lines
  // that are parallel to the grid.
  while Angle > Pi do
  begin
    Angle := Angle - Pi;
  end;

  while Angle < -Pi / 4 do
  begin
    Angle := Angle + Pi;
  end;

  if Angle > Pi * 3 / 4 then
  begin
    Angle := Angle - Pi;
  end;

  LineLength := (CursorComponent.CursorHeight - 7) / 2 + 0.1;
  CursorComponent.HotPointX := CursorComponent.CursorWidth div 2;
  CursorComponent.HotPointY := CursorComponent.CursorHeight div 2;

  lsCa := LineSeparation * Cos(Angle);
  llSa := LineLength * Sin(Angle);
  lsSa := LineSeparation * Sin(Angle);
  llCa := LineLength * Cos(Angle);

  X1 := Round(CursorComponent.HotPointX + lsCa - llSa);
  Y1 := Round(CursorComponent.HotPointY - (lsSa + llCa));
  X2 := Round(CursorComponent.HotPointX + lsCa + llSa);
  Y2 := Round(CursorComponent.HotPointY - (lsSa - llCa));
  DrawLine;

  X1 := Round(CursorComponent.HotPointX - lsCa - llSa);
  Y1 := Round(CursorComponent.HotPointY - (-lsSa + llCa));
  X2 := Round(CursorComponent.HotPointX - lsCa + llSa);
  Y2 := Round(CursorComponent.HotPointY - (-lsSa - llCa));
  DrawLine;
end;

procedure TfrmGoPhast.DrawAddColRowCursor(const AnImage: TBitmap;
  Angle: real; const CursorComponent: TRbwDynamicCursor);
var
  LineLength: real;
  X1, Y1, X2, Y2: integer;
  llSa, llCa: real;
  procedure DrawLine;
  begin
    with AnImage.Canvas do
    begin
      MoveTo(X1, Y1);
      LineTo(X2, Y2);
    end
  end;
begin
  // This procedure is used to draw the bitmaps for the
  // add-column or add-row cursor for the top view.
  // It is also used to help draw the subdivide cursor.
  // The cursor is drawn with lines
  // that are parallel to the grid.
  while Angle > Pi do
  begin
    Angle := Angle - Pi;
  end;

  while Angle < -Pi / 4 do
  begin
    Angle := Angle + Pi;
  end;

  if Angle > Pi * 3 / 4 then
  begin
    Angle := Angle - Pi;
  end;

  LineLength := (CursorComponent.CursorHeight - 5) / 2 + 0.1;
  CursorComponent.HotPointX := CursorComponent.CursorWidth div 2;
  CursorComponent.HotPointY := CursorComponent.CursorHeight div 2;

  llSa := LineLength * Sin(Angle);
  llCa := LineLength * Cos(Angle);

  X1 := Round(CursorComponent.HotPointX - llSa);
  Y1 := Round(CursorComponent.HotPointY - llCa);
  X2 := Round(CursorComponent.HotPointX + llSa);
  Y2 := Round(CursorComponent.HotPointY + llCa);
  DrawLine;
end;

procedure TfrmGoPhast.DrawMoveColRowCursor(const AnImage: TBitmap;
  Angle: real; const CursorComponent: TRbwDynamicCursor);
const
  LineSeparation = 3.1;
  ArrowLength = 7.1;
  ArrowHeadLength = 4.1;
var
  LineLength: real;
  X1, Y1, X2, Y2: integer;
  lsCa, llSa, lsSa, llCa: real;
  ArrowAngle: real;
  procedure DrawLine;
  begin
    with AnImage.Canvas do
    begin
      MoveTo(X1, Y1);
      LineTo(X2, Y2);
    end
  end;
begin
  // This procedure is used to draw the bitmaps for the
  // move-column or move-row cursor for the top view.
  // The cursor is drawn with lines
  // that are parallel to the grid.
  while Angle > Pi do
  begin
    Angle := Angle - Pi;
  end;

  while Angle < -Pi / 4 do
  begin
    Angle := Angle + Pi;
  end;

  if Angle > Pi * 3 / 4 then
  begin
    Angle := Angle - Pi;
  end;

  LineLength := (CursorComponent.CursorHeight - 5) / 2 + 0.1;
  CursorComponent.HotPointX := CursorComponent.CursorWidth div 2;
  CursorComponent.HotPointY := CursorComponent.CursorHeight div 2;

  lsCa := LineSeparation * Cos(Angle);
  llSa := LineLength * Sin(Angle);
  lsSa := LineSeparation * Sin(Angle);
  llCa := LineLength * Cos(Angle);

  X1 := Round(CursorComponent.HotPointX + lsCa - llSa);
  Y1 := Round(CursorComponent.HotPointY - (lsSa + llCa));
  X2 := Round(CursorComponent.HotPointX + lsCa + llSa);
  Y2 := Round(CursorComponent.HotPointY - (lsSa - llCa));
  DrawLine;

  X1 := Round(CursorComponent.HotPointX - lsCa - llSa);
  Y1 := Round(CursorComponent.HotPointY - (-lsSa + llCa));
  X2 := Round(CursorComponent.HotPointX - lsCa + llSa);
  Y2 := Round(CursorComponent.HotPointY - (-lsSa - llCa));
  DrawLine;

  // draw arrows;
  X2 := Round(CursorComponent.HotPointX + 2 * lsCa);
  Y2 := Round(CursorComponent.HotPointY - 2 * lsSa);

  X1 := X2 + Round(ArrowLength * Cos(Angle));
  Y1 := Y2 - Round(ArrowLength * Sin(Angle));
  DrawLine;

  ArrowAngle := Angle + Pi / 4;
  X2 := X1 - Round(ArrowHeadLength * Cos(ArrowAngle));
  Y2 := Y1 + Round(ArrowHeadLength * Sin(ArrowAngle));
  DrawLine;

  ArrowAngle := Angle - Pi / 4;
  X2 := X1 - Round(ArrowHeadLength * Cos(ArrowAngle));
  Y2 := Y1 + Round(ArrowHeadLength * Sin(ArrowAngle));
  DrawLine;

  X2 := Round(CursorComponent.HotPointX - 2 * lsCa);
  Y2 := Round(CursorComponent.HotPointY + 2 * lsSa);
  X1 := X2 - Round(ArrowLength * Cos(Angle));
  Y1 := Y2 + Round(ArrowLength * Sin(Angle));
  DrawLine;

  ArrowAngle := Angle + Pi / 4;
  X2 := X1 + Round(ArrowHeadLength * Cos(ArrowAngle));
  Y2 := Y1 - Round(ArrowHeadLength * Sin(ArrowAngle));
  DrawLine;

  ArrowAngle := Angle - Pi / 4;
  X2 := X1 + Round(ArrowHeadLength * Cos(ArrowAngle));
  Y2 := Y1 - Round(ArrowHeadLength * Sin(ArrowAngle));
  DrawLine;

end;

procedure TfrmGoPhast.dcMoveColCursorDrawCursor(Sender: TObject; const ABitMap,
  AMask: TBitmap);
begin
  inherited;
  // Draw the bitmaps for the move-column cursor.
  if Grid <> nil then
  begin
    DrawMoveColRowCursor(ABitMap, Grid.GridAngle, dcMoveColCursor);
    AMask.Canvas.Pen.Width := 3;
    DrawMoveColRowCursor(AMask, Grid.GridAngle, dcMoveColCursor);
    //    DrawMask(ABitMap, AMask);
  end;
end;

procedure TfrmGoPhast.dcMoveRowCursorDrawCursor(Sender: TObject; const ABitMap,
  AMask: TBitmap);
begin
  inherited;
  // Draw the bitmaps for the move-row cursor.
  // Use the same code as for the move-column cursor except change the angle
  // by 90 degrees.
  if Grid <> nil then
  begin
    DrawMoveColRowCursor(ABitMap, Grid.GridAngle + Pi / 2,
      dcMoveRowCursor);
    AMask.Canvas.Pen.Width := 3;
    DrawMoveColRowCursor(AMask, Grid.GridAngle + Pi / 2, dcMoveRowCursor);
    //    DrawMask(ABitMap, AMask);
  end;
end;

procedure TfrmGoPhast.acModflow6ActiveExecute(Sender: TObject);
var
  UndoHfb: TUndoConvertHfbMf6;
  UndoConvertObs: TUndoConvertObservationsMf6;
  UndoChd: TUndoConvertChd;
begin
  inherited;
  if ModelSelection <> msModflow2015 then
  begin
    UndoStack.Submit(TUndoModelSelectionChange.Create(msModflow2015));
    HaveUsersDefineModflowLayers;

    if PhastModel.ModflowPackages.SfrPackage.IsSelected
      and not PhastModel.ModflowPackages.SfrModflow6Package.IsSelected then
    begin
      if (MessageDlg(StrDoYouWantToConve, mtConfirmation,
        [mbYes, mbNo], 0) = mrYes) then
      begin
        UndoStack.Submit(TUndoConvertSfrStreamMF6.Create);
      end;
    end;

    if PhastModel.ModflowPackages.StrPackage.IsSelected
      and not PhastModel.ModflowPackages.SfrModflow6Package.IsSelected then
    begin
      if (MessageDlg(StrDoYouWantToConveStr, mtConfirmation,
        [mbYes, mbNo], 0) = mrYes) then
      begin
        UndoStack.Submit(TUndoConvertStrStreamMF6.Create);
      end;
    end;

    if PhastModel.ModflowPackages.FhbPackage.IsSelected then
    begin
      if (MessageDlg(StrDoYouWantToConveFHB, mtConfirmation,
        [mbYes, mbNo], 0) = mrYes) then
      begin
        UndoStack.Submit(TUndoConvertFhbToMf6.Create);
      end;
    end;

    if PhastModel.ModflowPackages.HfbPackage.IsSelected then
    begin
      UndoHfb := TUndoConvertHfbMf6.Create;
      try
        if  UndoHfb.ShouldConvert and
          (MessageDlg(StrDoYouWantToConveHFB, mtConfirmation,
          [mbYes, mbNo], 0) = mrYes) then
        begin
          UndoStack.Submit(UndoHfb);
          UndoHfb := nil;
        end;
      finally
        UndoHfb.Free;
      end;
    end;

    if PhastModel.ModflowPackages.ChdBoundary.IsSelected then
    begin
      UndoChd := TUndoConvertChd.Create;
      try
        if  UndoChd.ShouldConvert and
          (MessageDlg(StrDoYouWantToConveChd, mtConfirmation,
          [mbYes, mbNo], 0) = mrYes) then
        begin
          UndoStack.Submit(UndoChd);
          UndoChd := nil;
        end;
      finally
        UndoChd.Free;
      end;
    end;

    UndoConvertObs := TUndoConvertObservationsMf6.Create;
    try
      if UndoConvertObs.ShouldConvert and
        (MessageDlg(StrDoYouWantToConveObs, mtConfirmation,
        [mbYes, mbNo], 0) = mrYes) then
      begin
        UndoStack.Submit(UndoConvertObs);
        UndoConvertObs := nil;
      end;
    finally
      UndoConvertObs.Free;
    end;

    if PhastModel.Mnw2IsSelected and not PhastModel.MawIsSelected
      and (MessageDlg(StrDoYouWantToConveMnw2, mtConfirmation,
        [mbYes, mbNo], 0) = mrYes) then
    begin
      UndoStack.Submit(TUndoConvertMnw2ToMaw.Create);
    end;

    if PhastModel.UzfIsSelected and not PhastModel.UzfMf6IsSelected
      and (MessageDlg(StrDoYouWantToConveUZF, mtConfirmation,
        [mbYes, mbNo], 0) = mrYes) then
    begin
      UndoStack.Submit(TUndoConvertUzfToUzf6.Create);
    end;

    if (PhastModel.SubIsSelected or PhastModel.SwtIsSelected)
      and not PhastModel.CSubIsSelected
      and (MessageDlg(StrDoYouWantToConveSUB, mtConfirmation,
        [mbYes, mbNo], 0) = mrYes) then
    begin
      UndoStack.Submit(TUndoConvertSubAndSwtToCSub.Create);
    end;

    PhastModel.DataArrayManager.CreateInitialDataSets;
  end;
end;

procedure TfrmGoPhast.acModflowActiveExecute(Sender: TObject);
begin
  if ModelSelection <> msModflow then
  begin
    UndoStack.Submit(TUndoModelSelectionChange.Create(msModflow));
    HaveUsersDefineModflowLayers;
  end;
end;

procedure TfrmGoPhast.acModflowCfpActiveExecute(Sender: TObject);
begin
  if ModelSelection <> msModflowCFP then
  begin
    UndoStack.Submit(TUndoModelSelectionChange.Create(msModflowCFP));
    HaveUsersDefineModflowLayers;
  end;
end;

procedure TfrmGoPhast.acModflowFmpActiveExecute(Sender: TObject);
begin
  if ModelSelection <> msModflowFMP then
  begin
    UndoStack.Submit(TUndoModelSelectionChange.Create(msModflowFMP));
    HaveUsersDefineModflowLayers;
  end;
end;

procedure TfrmGoPhast.acModflowLgr2ActiveExecute(Sender: TObject);
begin
  inherited;
  if ModelSelection <> msModflowLGR2 then
  begin
    UndoStack.Submit(TUndoModelSelectionChange.Create(msModflowLGR2));
    HaveUsersDefineModflowLayers;
    if PhastModel.ChildModels.Count = 0 then
    begin
      miChildModelsClick(nil);
    end;
  end;
end;

procedure TfrmGoPhast.acModflowLgrActiveExecute(Sender: TObject);
begin
  inherited;
  if ModelSelection <> msModflowLGR then
  begin
    UndoStack.Submit(TUndoModelSelectionChange.Create(msModflowLGR));
    HaveUsersDefineModflowLayers;
    if PhastModel.ChildModels.Count = 0 then
    begin
      miChildModelsClick(nil);
    end;
  end;
end;

procedure TfrmGoPhast.acModflowNwtActiveExecute(Sender: TObject);
begin
  inherited;
  if ModelSelection <> msModflowNWT then
  begin
    UndoStack.Submit(TUndoModelSelectionChange.Create(msModflowNWT));
    HaveUsersDefineModflowLayers;
    Beep;
    MessageDlg(StrNowMightBeAGood, mtInformation, [mbOK], 0);
  end;
end;

procedure TfrmGoPhast.acMoveColumnOrRowExecute(Sender: TObject);
begin
  inherited;
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbMove.OnMouseDown(tbMove, mbLeft, [ssLeft], 0, 0);
  end;

  if tbMove.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbMove);

    // Set the cursors
    SetZB_Cursors(crArrow);
  end
  else
  begin
    // Reset the cursors.
    SetZB_Cursors(crArrow);
  end;
  CurrentTool := MovingGridBoundaryTool;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.acMoveGridExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmGridPosition);
end;

procedure TfrmGoPhast.miFilesToArchiveClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmFilesToArchive);
end;

procedure TfrmGoPhast.miFileTypesClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmFileTypes);
end;

procedure TfrmGoPhast.FillButtonList(AList: TList);
begin
  Assert(AList.Count = 0);
  AList.Add(tbZoomIn);
  AList.Add(tbZoomOut);
  AList.Add(tbZoom);
  AList.Add(tbPan);
  AList.Add(tbDeleteColumnRow);
  AList.Add(tbMove);
  AList.Add(tbAddVerticalBoundary);
  AList.Add(tbAddHorizontalBoundary);
  AList.Add(tbSubdivide);
  AList.Add(tbPoint);
  AList.Add(tbLine);
  AList.Add(tbPolygon);
  AList.Add(tbStraightLine);
  AList.Add(tbRectangle);
  AList.Add(tbSelect);
  AList.Add(tbLasso);
  AList.Add(tbSelectPoint);
  AList.Add(tbInsertPoint);
  AList.Add(tbDeleteSegment);
  AList.Add(tbGridAngle);
  AList.Add(tbSpacing);
  AList.Add(tbSelectColRowLayer);
  AList.Add(tbAddPartsToObject);
  AList.Add(tbAddLinesToObjects);
  AList.Add(tbAddPointsToObject);
  AList.Add(tbVertexValue);
  AList.Add(tbMeasure);
  AList.Add(tbCrossSection);
  AList.Add(tbRotateCrossSection);
  AList.Add(tbMoveNodes);
  AList.Add(btnFishnet);
  AList.Add(tbDrawElement);
  AList.Add(tbAddPilotPoint);
  AList.Add(tbDeletePilotPoint);
end;

procedure TfrmGoPhast.SetButtonsUp(const CurrentButton: TObject);
var
  AList: TList;
  index: integer;
  AButton: TToolButton;
begin
  // terminated any screen objects that haven't been ended yet.
  frameTopView.FinishScreenObjects;
  frameFrontView.FinishScreenObjects;
  frameSideView.FinishScreenObjects;

  // Make a list of all the buttons that might need to be set up.
  AList := TList.Create;
  try
    FillButtonList(AList);

    // Go through the list and if a button
    // is not the current button then it
    // need to have it's Down property set to
    // false.  If the button has an associated
    // TAction, set the Checked state
    // of the TAction to false.  This will not only change
    // the Down state of the button but also the Checked
    // state of the associated menu item.
    for index := 0 to AList.Count - 1 do
    begin
      AButton := AList[Index];
      if AButton <> CurrentButton then
      begin
        if AButton.Action = nil then
        begin
          AButton.Down := False;
        end
        else
        begin
          AButton.Down := False;
          // Setting the Action.Checked to false is not
          // enough to get the button to redraw if OnClick
          // event does not also occur.  Setting AButton.Down
          // forces it to redraw for reasons unknown.
          (AButton.Action as TAction).Checked := false;
        end;
      end;
    end;
  finally
    AList.Free;
  end;
  CurrentTool := nil;
end;

procedure TfrmGoPhast.tbAddVerticalBoundaryClick(Sender: TObject);
begin
  inherited;
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbAddVerticalBoundary.OnMouseDown(tbAddVerticalBoundary, mbLeft, [ssLeft],
      0, 0);
  end;

  if tbAddVerticalBoundary.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbAddVerticalBoundary);

    // Set the cursors.
    SetZB_Cursors(crVertical);
    if frameTopView <> nil then
    begin
      frameTopView.ZoomBox.Cursor := dcAddColCursor.Cursor;
      frameTopView.ZoomBox.Image32.Cursor := dcAddColCursor.Cursor;
    end;
  end
  else
  begin
    // Reset the cursors.
    SetZB_Cursors(crArrow);
  end;
  if tbAddVerticalBoundary.Down or tbAddHorizontalBoundary.Down then
  begin
    CurrentTool := AddGridBoundaryTool;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.tbAddHorizontalBoundaryClick(Sender: TObject);
begin
  inherited;
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbAddHorizontalBoundary.OnMouseDown(tbAddHorizontalBoundary, mbLeft,
      [ssLeft], 0, 0);
  end;

  if tbAddHorizontalBoundary.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbAddHorizontalBoundary);

    // Set the cursors.
    SetZB_Cursors(crHorizontal);
    if frameTopView <> nil then
    begin
      frameTopView.ZoomBox.Cursor := dcAddRowCursor.Cursor;
      frameTopView.ZoomBox.Image32.Cursor := dcAddRowCursor.Cursor;
    end;
  end
  else
  begin
    // Reset the cursors.
    SetZB_Cursors(crArrow);
  end;
  if tbAddVerticalBoundary.Down or tbAddHorizontalBoundary.Down then
  begin
    CurrentTool := AddGridBoundaryTool;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.dcAddColCursorDrawCursor(Sender: TObject;
  const ABitMap, AMask: TBitmap);
begin
  inherited;
  // Draw the bitmaps for the add-column cursor.
  if Grid <> nil then
  begin
    DrawAddColRowCursor(ABitMap, Grid.GridAngle, dcAddColCursor);
    AMask.Canvas.Pen.Width := 3;
    DrawAddColRowCursor(AMask, Grid.GridAngle, dcAddColCursor);
  end;
end;

procedure TfrmGoPhast.dcAddRowCursorDrawCursor(Sender: TObject;
  const ABitMap, AMask: TBitmap);
begin
  inherited;
  // Draw the bitmaps for the add-row cursor.
  // Use the same code as for the add-column cursor except change the angle
  // by 90 degrees.
  if Grid <> nil then
  begin
    DrawAddColRowCursor(ABitMap, Grid.GridAngle + Pi / 2, dcAddRowCursor);
    AMask.Canvas.Pen.Width := 3;
    DrawAddColRowCursor(AMask, Grid.GridAngle + Pi / 2, dcAddRowCursor);
  end;

end;

procedure TfrmGoPhast.acSubdivideExecute(Sender: TObject);
begin
  inherited;
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbSubdivide.OnMouseDown(tbSubdivide, mbLeft, [ssLeft], 0, 0);
  end;

  if tbSubdivide.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbSubdivide);

    // Set the cursors.
    SetZB_Cursors(crSubdivide);
    if frameTopView <> nil then
    begin
      frameTopView.ZoomBox.Cursor := dcSubdivide.Cursor;
      frameTopView.ZoomBox.Image32.Cursor := dcSubdivide.Cursor;
    end;
  end
  else
  begin
    // Reset the cursors.
    SetZB_Cursors(crArrow);
  end;
  CurrentTool := SubdivideGridTool;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.acSutra22ActiveExecute(Sender: TObject);
begin
  inherited;
  if ModelSelection <> msSutra22 then
  begin
    UndoStack.Submit(TUndoModelSelectionChange.Create(msSutra22));
    HaveUsersDefineSutraLayers;
  end;
end;

procedure TfrmGoPhast.acSutra30ActiveExecute(Sender: TObject);
begin
  inherited;
  if ModelSelection <> msSutra30 then
  begin
    UndoStack.Submit(TUndoModelSelectionChange.Create(msSutra30));
    HaveUsersDefineSutraLayers;
  end;
end;

procedure TfrmGoPhast.acSutraLayersExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmSutraLayers);
end;

procedure TfrmGoPhast.acSutraOptionsExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmSutraOptions);
end;

procedure TfrmGoPhast.acSutraOutputControlExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmSutraOutputControl);
end;

procedure TfrmGoPhast.acSutraProgramLocationsExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmSutraProgramLocations)
end;

procedure TfrmGoPhast.acSutraTimesExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmSutraTimes);
end;

procedure TfrmGoPhast.acSwrObservationsExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmSwrObservations);
end;

procedure TfrmGoPhast.acSwrStructuresExecute(Sender: TObject);
begin
  inherited;
  TfrmSwrStructures.AddNewStructure := False;
  TfrmSwrStructures.SelectedStructureIndex := 0;
  ShowAForm(TfrmSwrStructures);
end;

procedure TfrmGoPhast.acSWR_ReachGeometryExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmSwrReachGeometry);
end;

procedure TfrmGoPhast.acSWR_TabfilesExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmSwrTabfiles);
end;

procedure TfrmGoPhast.acEditObservationComparisonsExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmObservationComparisons);
end;

procedure TfrmGoPhast.acEditSutraFluxObsExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmManageSutraBoundaryObservations)
end;

procedure TfrmGoPhast.acSimplifyScreenObjectsExecute(Sender: TObject);
var
  SelectedScreenObjects: TScreenObjectList;
  Index: Integer;
  AScreenObject: TScreenObject;
  Undo: TUndoSimplifyObjects;
begin
  inherited;
  SelectedScreenObjects := TScreenObjectList.Create;
  try
    SelectedScreenObjects.Capacity := PhastModel.ScreenObjectCount;
    for Index := PhastModel.ScreenObjectCount - 1 downto 0 do
    begin
      AScreenObject := PhastModel.ScreenObjects[Index];
      if AScreenObject.Selected then
      begin
        SelectedScreenObjects.Add(AScreenObject);
      end;
    end;
    if SelectedScreenObjects.Count > 0 then
    begin
      frmSimplifyObjectsCriteria := TfrmSimplifyObjectsCriteria.Create(nil);
      try
        if frmSimplifyObjectsCriteria.ShowModal = mrOK then
        begin
          Undo := TUndoSimplifyObjects.Create(SelectedScreenObjects);
          Undo.MaxDeltaAngle := frmSimplifyObjectsCriteria.rdeAngle.RealValue;
          Undo.RequiredSpacing := frmSimplifyObjectsCriteria.rdeSpacing.RealValue;
          UndoStack.Submit(Undo);
        end;
      finally
        FreeAndNil(frmSimplifyObjectsCriteria);
      end;
    end
    else
    begin
      Beep;
      MessageDlg(StrNoObjectsAreSelec, mtError, [mbOK], 0);
    end;
  finally
    SelectedScreenObjects.Free;
  end;
end;

procedure TfrmGoPhast.acShowCellNumbersExecute(Sender: TObject);
begin
  inherited;
  if DisvUsed then
  begin
    DisvGrid.DrawCellNumbers := not DisvGrid.DrawCellNumbers;
	if DisvGrid.DrawCellNumbers then
	begin
	  Caption := 'Hide Cell Numbers';
	end
	else
	begin
	  Caption := 'Show Cell Numbers';
	end;
    InvalidateImage32AllViews;
  end;
end;

procedure TfrmGoPhast.acRipPlantGroupsExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmModflowRipPlantGroups);
end;

procedure TfrmGoPhast.acRotateAroundGridOriginExecute(Sender: TObject);
begin
  inherited;
  Application.CreateForm(TfrmGridAngle, frmGridAngle);
  try
    frameTopView.DeltaGridAngle := 0;
    frmGridAngle.ActionToTake := attRotateAroundGridOrigin;
    frmGridAngle.ShowModal;
  finally
    // There is a test to see if frmGridAngle is equal to nil
    // when drawing the rotated grid.
    FreeAndNil(frmGridAngle);
  end;
end;

procedure TfrmGoPhast.acDisplayDataExecute(Sender: TObject);
begin
  inherited;
  if frmDisplayData = nil then
  begin
    Application.CreateForm(TfrmDisplayData, frmDisplayData);
  end;
  UpdateFrmDisplayData(True);
  frmDisplayData.Show;
  if frmDisplayData.WindowState = wsMinimized then
  begin
    frmDisplayData.WindowState := wsNormal;
  end;
  frmDisplayData.HelpKeyword := 'Data_Visualization_Dialog_Box';
end;

procedure TfrmGoPhast.acDisvGridExecute(Sender: TObject);
var
  AssignMesh: Boolean;
begin
  inherited;
  if PhastModel.Mf6GridType = mgtLayered then
  begin
    Exit;
  end;
  tbSelectClick(acSelectObjects);
  if PhastModel.DisvGrid.TwoDGrid.ElementCount > 0 then
  begin
    AssignMesh := (MessageDlg(StrDoYouWantToConveDisv, mtInformation, [mbYes, mbNo], 0, mbNo) = mrYes);
  end
  else
  begin
    AssignMesh := True;
  end;
  UndoStack.Submit(TUndoChangeGridType.Create(mgtLayered, AssignMesh));
//  SetActionChecked(Sender);
end;

procedure TfrmGoPhast.acUndoExecute(Sender: TObject);
begin
  inherited;
  UndoStack.UndoEvent(Sender);
end;

procedure TfrmGoPhast.acVertexValueExecute(Sender: TObject);
begin
  inherited;
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbVertexValue.OnMouseDown(tbVertexValue, mbLeft, [ssLeft], 0, 0);
  end;

  if tbVertexValue.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbVertexValue);
    // Set the cursors.
    SetZB_Cursors(crVertexValue);
    // don't draw a rectangle around the selected screen objects.
    CurrentTool := EditVertexValueTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.dcSubdivideDrawCursor(Sender: TObject; const ABitMap,
  AMask: TBitmap);
begin
  inherited;
  // Draw the bitmaps for the Subdivide cursor.
  if Grid <> nil then
  begin
    DrawSubdivideCursor(ABitMap, Grid.GridAngle, dcSubdivide);
    DrawSubdivideCursor(ABitMap, Grid.GridAngle + Pi / 2, dcSubdivide);
    DrawAddColRowCursor(ABitMap, Grid.GridAngle, dcSubdivide);
    DrawAddColRowCursor(ABitMap, Grid.GridAngle + Pi / 2, dcSubdivide);
    AMask.Canvas.Pen.Width := 3;
    DrawSubdivideCursor(AMask, Grid.GridAngle, dcSubdivide);
    DrawSubdivideCursor(AMask, Grid.GridAngle + Pi / 2, dcSubdivide);
    DrawAddColRowCursor(AMask, Grid.GridAngle, dcSubdivide);
    DrawAddColRowCursor(AMask, Grid.GridAngle + Pi / 2, dcSubdivide);
  end;
end;

procedure TfrmGoPhast.SetZB_Cursors(const ACursor: TCursor);
begin
  // This sets the cursors for several controls at once.
  if frameTopView <> nil then
  begin
    frameTopView.ZoomBox.Cursor := ACursor;
    frameTopView.ZoomBox.Image32.Cursor := ACursor;
  end;
  if frameFrontView <> nil then
  begin
    frameFrontView.ZoomBox.Cursor := ACursor;
    frameFrontView.ZoomBox.Image32.Cursor := ACursor;
  end;
  if frameSideView <> nil then
  begin
    frameSideView.ZoomBox.Cursor := ACursor;
    frameSideView.ZoomBox.Image32.Cursor := ACursor;
  end;
end;

procedure TfrmGoPhast.InvalidateTop;
begin
  // redraw the top view.
  if frameTopView <> nil then
  begin
    frameTopView.ZoomBox.InvalidateImage32;
  end;
end;

procedure TfrmGoPhast.InvalidateFront;
begin
  // redraw the front view.
  if frameFrontView <> nil then
  begin
    frameFrontView.ZoomBox.InvalidateImage32;
  end;
end;

procedure TfrmGoPhast.InvalidateGrid;
begin
  InvalidateDataSets;
  TopDiscretizationChanged := True;
  FrontDiscretizationChanged := True;
  SideDiscretizationChanged := True;
  if Grid <> nil then
  begin
    Grid.NeedToRecalculateCellColors;
  end;
  InvalidateAllViews;
  if Grid <> nil then
  begin
    Grid.GridChanged;
  end;

end;

procedure TfrmGoPhast.InternalSaveFile(FileName: string);
const
  FiveMB = 5*1024*1024;
  OneSecond = 1/24/3600;
var
  CompressionStream: TCompressionStream;
  FileStream: TFileStream;
  MemStream: TMemoryStream;
  StartTime: Extended;
begin
  MemStream := TMemoryStream.Create;
  try
    try
      FileStream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite,
        ReadWritePermissions);
    except on EFCreateError do
      begin
        // try again after 1 second.
        StartTime := Now;
        repeat
        until (Now - StartTime > OneSecond);
        FileStream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite,
          ReadWritePermissions);
      end;
    end;
    try
      PhastModel.SutraMesh.UpdateNodeNumbers;
      PhastModel.SutraMesh.UpdateElementNumbers;

      case FileFormat of
        ffAscii:
          begin
            try
              MemStream.WriteComponent(PhastModel);
            except
              on EOutOfMemory do
              begin
                Beep;
                MessageDlg(Format(StrYouRanOutOfMemor, ['.gpt']),
                  mtError, [mbOK], 0);
                Exit;
              end;
              on E: EStreamError do
              begin
                Beep;
                MessageDlg(Format(StrSomethingWentWrong, [E.message, '.gpt']),
                  mtError, [mbOK], 0);
                Exit;
              end;
            end;
            PhastModel.ClearScreenObjectCollection;
            MemStream.Position := 0;
            try
              MM_ObjectBinaryToText(MemStream, FileStream);
            except on EAccessViolation do
              begin
                Beep;
                MessageDlg(StrAnErrorOccuredWhe, mtError, [mbOK], 0);
              end;
            end;
            if not FSizeWarningDisplayed and (FileStream.Size > FiveMB) then
            begin
              FSizeWarningDisplayed := True;
              Beep;
              MessageDlg(StrIfYouWantToSave, mtInformation, [mbOK], 0);
            end;
          end;
        ffBinary:
          begin
            FileStream.WriteComponent(PhastModel);
            PhastModel.ClearScreenObjectCollection;
          end;
        ffXML:
          begin
            try
              MemStream.WriteComponent(PhastModel);
            except
              on EOutOfMemory do
              begin
                Beep;
                MessageDlg(Format(StrYouRanOutOfMemor, ['.xml']), mtError, [mbOK], 0);
                Exit;
              end;
              on E: EStreamError do
              begin
                Beep;
                MessageDlg(Format(StrSomethingWentWrong, [E.message, '.xml']),
                  mtError, [mbOK], 0);
                Exit;
              end;
            end;
            PhastModel.ClearScreenObjectCollection;
            MemStream.Position := 0;
            rwObjectBinaryToXML(MemStream, FileStream);
          end;
        ffZLib:
          begin
            CompressionStream := TCompressionStream.Create(clMax, FileStream);
            try
              CompressionStream.WriteComponent(PhastModel);
            finally
              CompressionStream.Free;
            end;
            PhastModel.ClearScreenObjectCollection;
          end;
      else
        Assert(False);
      end;
    finally
      FileStream.Free;
    end;
  finally
    MemStream.Free;
  end;
end;

procedure TfrmGoPhast.SetGridLineDrawingChoice(Sender: TObject);
begin
  if acShowAllGridLines.Checked then
  begin
    PhastGrid.GridLineDrawingChoice := gldcAll;
    ModflowGrid.GridLineDrawingChoice := gldcAll;
    FootPrintGrid.GridLineDrawingChoice := gldcAll;
    DisvGrid.GridLineDrawingChoice := gldcAll;
    tbShow2DGrid.ImageIndex := acShowAllGridLines.ImageIndex;
    miShow2DGridlines.ImageIndex := acShowAllGridLines.ImageIndex;
    tbShow2DMesh.ImageIndex := acShowAllGridLines.ImageIndex;
  end
  else if acShowExteriorGridLines.Checked then
  begin
    PhastGrid.GridLineDrawingChoice := gldcExterior;
    ModflowGrid.GridLineDrawingChoice := gldcExterior;
    FootPrintGrid.GridLineDrawingChoice := gldcExterior;
    DisvGrid.GridLineDrawingChoice := gldcExterior;
    tbShow2DGrid.ImageIndex := acShowExteriorGridLines.ImageIndex;
    miShow2DGridlines.ImageIndex := acShowExteriorGridLines.ImageIndex;
    tbShow2DMesh.ImageIndex := acShowExteriorGridLines.ImageIndex;
  end
  else if acShowActiveGridLines.Checked then
  begin
    PhastGrid.GridLineDrawingChoice := gldcActive;
    ModflowGrid.GridLineDrawingChoice := gldcActive;
    FootPrintGrid.GridLineDrawingChoice := gldcActive;
    DisvGrid.GridLineDrawingChoice := gldcActive;
    tbShow2DGrid.ImageIndex := acShowActiveGridLines.ImageIndex;
    miShow2DGridlines.ImageIndex := acShowActiveGridLines.ImageIndex;
    tbShow2DMesh.ImageIndex := acShowActiveGridLines.ImageIndex;
  end
  else if acShowActiveEdge.Checked then
  begin
    PhastGrid.GridLineDrawingChoice := gldcActiveEdge;
    ModflowGrid.GridLineDrawingChoice := gldcActiveEdge;
    FootPrintGrid.GridLineDrawingChoice := gldcActiveEdge;
    DisvGrid.GridLineDrawingChoice := gldcActiveEdge;
    tbShow2DGrid.ImageIndex := acShowActiveEdge.ImageIndex;
    miShow2DGridlines.ImageIndex := acShowActiveEdge.ImageIndex;
    tbShow2DMesh.ImageIndex := acShowActiveEdge.ImageIndex;
  end
  else
  begin
    Assert(False);
  end;
  UpdateDisplay(nil);
end;

procedure TfrmGoPhast.CancelCurrentScreenObject;
begin
  if CurrentTool is TCustomCreateScreenObjectTool then
  begin
    CanEdit := False;
    try
      TCustomCreateScreenObjectTool(CurrentTool).FinishScreenObjects;
    finally
      CanEdit := True;
    end;
  end;
end;

procedure TfrmGoPhast.EnableCTS;
begin
  acEditCTS.Visible := (ModelSelection in ModflowSelection);
  acEditCTS.Enabled :=
    PhastModel.ModflowPackages.Mt3dCts.IsSelected;
end;

procedure TfrmGoPhast.EnableManageParameters;
begin
  miManageParameters.Enabled :=
    (PhastModel.ModelSelection in ModflowSelection)
    or ((PhastModel.ModelSelection in SutraSelection)
      and PhastModel.PestUsed);
  miManageParameters.Visible :=
    (PhastModel.ModelSelection in (ModflowSelection + SutraSelection))
end;

procedure TfrmGoPhast.EnableDeleteImage;
begin
  miDeleteImage.Enabled := PhastModel.Bitmaps.Count > 0;
end;

procedure TfrmGoPhast.EnableHufMenuItems;
begin
  miMF_HydrogeologicUnits.Enabled :=
    (PhastModel.ModelSelection in ModflowSelection)
    and PhastModel.ModflowPackages.HufPackage.IsSelected;
end;

procedure TfrmGoPhast.InitializeModflowInputDialog;
begin
  case PhastModel.ObservationPurpose of
    ofObserved: sdModflowInput.FileName := ObservationFileName[sdModflowInput];
    ofPredicted: sdModflowInput.FileName := PredictionFileName[sdModflowInput];
    else Assert(False);
  end;
end;

procedure TfrmGoPhast.InitializeModflowLgrInputDialog;
begin
  case PhastModel.ObservationPurpose of
    ofObserved: sdModflowLgr.FileName := ObservationFileName[sdModflowLgr];
    ofPredicted: sdModflowLgr.FileName := PredictionFileName[sdModflowLgr];
    else Assert(False);
  end;
end;

procedure TfrmGoPhast.ReadModelMateProject(FileName: string;
  ModelMateProject: TProject);
var
  MemStream: TMemoryStream;
  FileStream: TFileStream;
  TextStream: TMemoryStream;
  OldDecChar: Char;
begin
  OldDecChar := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := '.';
    FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    MemStream := TMemoryStream.Create;
    TextStream := TMemoryStream.Create;
    try
      FileStream.Position := 0;
      TextStream.CopyFrom(FileStream, FileStream.Size);
      TextStream.Position := 0;
      ObjectTextToBinary(TextStream, MemStream);
      MemStream.Position := 0;
      try
        MemStream.ReadComponent(ModelMateProject);
      except on E: EReadError do
        begin
          Beep;
          MessageDlg(StrThereWasAProblem, mtWarning, [mbOK], 0);
        end;
      end;
      ModelMateProject.FileName := FileName;
    finally
      TextStream.Free;
      MemStream.Free;
      FileStream.Free;
    end;
  finally
    FormatSettings.DecimalSeparator := OldDecChar;
  end;
end;

procedure TfrmGoPhast.StoreInitalPosition;
var
  NewPosition: TPositionStorage;
  Z: Double;
  ZInt: Integer;
  RotatedCenterPoint: TPoint2D;
  RealCenterPoint: TPoint2D;
  CenterPoint: TPoint;
begin
//  if Grid = nil then
//  begin
//    Exit;
//  end;
  CenterPoint.x := frameFrontView.ZoomBox.Width div 2;
  CenterPoint.Y := frameTopView.ZoomBox.Height div 2;
  RealCenterPoint.x := frameTopView.ZoomBox.X(CenterPoint.x);
  RealCenterPoint.Y := frameTopView.ZoomBox.Y(CenterPoint.y);
  if Grid <> nil then
  begin
    RotatedCenterPoint := Grid.RotateFromRealWorldCoordinatesToGridCoordinates(
      RealCenterPoint);
  end
  else
  begin
    RotatedCenterPoint := RealCenterPoint;
  end;
  ZInt := frameFrontView.ZoomBox.Height div 2;
  Z := frameFrontView.ZoomBox.Y(ZInt);

  NewPosition := TPositionStorage.Create;

  NewPosition.Top.XCenter := RealCenterPoint.x;
  NewPosition.Top.YCenter := RealCenterPoint.y;
  NewPosition.Top.Magnification := frameTopView.ZoomBox.Magnification;

  NewPosition.Front.XCenter := RotatedCenterPoint.x;
  NewPosition.Front.YCenter := Z;
  NewPosition.Front.Magnification := frameFrontView.ZoomBox.Magnification;

  NewPosition.Side.XCenter := RotatedCenterPoint.Y;
  NewPosition.Side.YCenter := Z;
  NewPosition.Side.Magnification := frameSideView.ZoomBox.Magnification;

  FPositionList.FList.Add(NewPosition);
  Inc(FPositionList.FCurrentPosition);

  acPositionBackward.Enabled := False;
  tbPositionRedo.Enabled := False;
end;

procedure TfrmGoPhast.SurferGridFile1Click(Sender: TObject);
begin
  inherited;
  with TfrmImportSurferGrdFile.Create(nil) do
  begin
    try
      if GetData then
      begin
        ShowModal;
      end;
    finally
      Free;
    end;
  end;

end;

procedure TfrmGoPhast.SutraMeshTypeChanged(Sender: TObject);
begin
  acSutraLayers.Enabled := PhastModel.SutraMesh.MeshType = mt3D;
end;

procedure TfrmGoPhast.EnableFarmMenuItems;
var
  FarmProcess: TFarmProcess;
begin
  FarmProcess := PhastModel.ModflowPackages.FarmProcess;
  acFarmClimate.Enabled := (PhastModel.ModelSelection = msModflowFmp)
    and FarmProcess.IsSelected
    and ((FarmProcess.RootingDepth = rdCalculated)
    or (FarmProcess.ConsumptiveUse = cuCalculated)
    or (FarmProcess.Precipitation = pTimeSeries));

  acFarmAllotment.Enabled := (PhastModel.ModelSelection = msModflowFmp)
    and FarmProcess.IsSelected
    and (FarmProcess.SurfaceWaterAllotment = swaEqual);
end;

procedure TfrmGoPhast.NilDisplay;
begin
  PhastModel.ThreeDDataSet := nil;
  PhastModel.TopDataSet := nil;
  PhastModel.FrontDataSet := nil;
  PhastModel.SideDataSet := nil;
  if PhastModel.Grid <> nil then
  begin
    PhastModel.Grid.ThreeDContourDataSet := nil;
    PhastModel.Grid.TopContourDataSet := nil;
    PhastModel.Grid.FrontContourDataSet := nil;
    PhastModel.Grid.SideContourDataSet := nil;
  end;
  if PhastModel.Mesh3D <> nil then
  begin
    PhastModel.Mesh3D.TopContourDataSet := nil;
    PhastModel.Mesh3D.ThreeDContourDataSet := nil;
  end;
  PhastModel.ThreeDTimeList := nil;
  PhastModel.TopTimeList := nil;
  PhastModel.FrontTimeList := nil;
  PhastModel.SideTimeList := nil;
  PhastModel.ContourLegend.ValueSource := nil;
  PhastModel.ColorLegend.ValueSource := nil;
  if frmDisplayData <> nil then
  begin
    frmDisplayData.NilDisplay;
  end;
end;

procedure TfrmGoPhast.odRunParRepClose(Sender: TObject);
begin
  inherited;
  FRunParRep := FRunParRepForm.cbRun.Checked;
  FRunParRepForm.Free;
end;

procedure TfrmGoPhast.odRunParRepShow(Sender: TObject);
begin
  inherited;
  FRunParRepForm := TfrmRunParRep.createfordialog(odRunParRep);
  FRunParRepForm.cbRun.Checked := FRunParRep;
end;

procedure TfrmGoPhast.odSutraFilesTypeChange(Sender: TObject);
var
  NewFileName: string;
begin
  inherited;
  if odSutraFiles.FileName <> '' then
  begin
    NewFileName := '';
    case odSutraFiles.FilterIndex of
      1,2:
        begin
          NewFileName := ChangeFileExt(odSutraFiles.FileName, '.14B');
        end;
      3:
        begin
          NewFileName := ChangeFileExt(odSutraFiles.FileName, '.15B');
        end;
      4:
        begin
          NewFileName := ChangeFileExt(odSutraFiles.FileName, '.PVEC');
        end;
      5:
        begin
          NewFileName := ChangeFileExt(odSutraFiles.FileName, '.UVEC');
        end;
    end;
    if NewFileName <> '' then
    begin
      odSutraFiles.FileName := NewFileName;
      UpdateDialogBoxFileName(odSutraFiles, NewFileName);
    end;

  end;
end;

procedure TfrmGoPhast.miPHASTProgramLocationClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmPhastLocation);

end;

procedure TfrmGoPhast.PlayIntroductoryVideo;
var
  URL: string;
begin
  URL := ExtractFileDir(ParamStr(0));
  if URL[Length(URL)] <> PathDelim then
  begin
    URL := URL + PathDelim;
  end;
  URL := URL + 'Videos\IntroductoryVideo\IntroductoryVideo.html';
  if FileExists(URL) then
  begin
    URL := FileNameToURL(URL);
  end
  else
  begin
    URL := 'https://water.usgs.gov/nrp/gwsoftware/ModelMuse/IntroductoryVideo/IntroductoryVideo.html';
  end;
  LaunchURL(FBrowser, URL);
end;

procedure TfrmGoPhast.EnableManageFlowObservations;
begin
  if PhastModel.ModelSelection in ModflowSelection then
  begin
    if PhastModel.ModelSelection = msModflow2015 then
    begin
//      {$IFDEF PEST}
//        miManageFluxObservations.Enabled :=
//          (PhastModel.ChobIsSelected
//          or PhastModel.DrobIsSelected
//          or PhastModel.GbobIsSelected
//          or PhastModel.RvobIsSelected
////          or PhastModel.StobIsSelected
//          or PhastModel.TobIsSelected);
//      {$ELSE}
        miManageFluxObservations.Enabled :=
          PhastModel.TobIsSelected;
//      {$ENDIF}
    end
    else
    begin
      miManageFluxObservations.Enabled :=
        (PhastModel.ModelSelection in ModflowSelection)
        and (PhastModel.ChobIsSelected
        or PhastModel.DrobIsSelected
        or PhastModel.GbobIsSelected
        or PhastModel.RvobIsSelected
        or PhastModel.StobIsSelected
        or PhastModel.TobIsSelected);
    end;
  end
  else
  begin
    miManageFluxObservations.Enabled := False;
  end;
end;

procedure TfrmGoPhast.EnableManageHeadObservations;
begin
  miManageHeadObservations.Enabled :=
    (PhastModel.ModelSelection in ModflowSelection)
    and (PhastModel.ModelSelection <> msModflow2015)
    and PhastModel.HobIsSelected
end;

procedure TfrmGoPhast.EnableMeshRenumbering;
begin

  if PhastModel = nil then
  begin
    Exit;
  end;
  miRenumberMesh.Enabled := (PhastModel.Mesh <> nil) ;
//  miRenumberMesh.Enabled := (PhastModel.Mesh <> nil)
//    and (PhastModel.Mesh.MeshType = mt3D);
end;

procedure TfrmGoPhast.EnableMt3dmsMenuItems;
begin
  acRunMt3dms.Enabled := PhastModel.Mt3dIsSelected;
end;

procedure TfrmGoPhast.EnablePilotPointItems;
begin
  acAddPilotPoint.Enabled := PhastModel.PestUsed
    and PhastModel.PestProperties.ShowPilotPoints;
  acDeletePilotPoint.Enabled := acAddPilotPoint.Enabled
    and
    ((PhastModel.PestProperties.SpecifiedPilotPoints.Count > 0)
    or (PhastModel.PestProperties.BetweenObservationsPilotPoints.Count > 0));
end;

function TfrmGoPhast.PestVersionOK: Boolean;
var
  PestName: string;
begin
  result := True;
  PestName := PhastModel.GetPestName;
  if FileExists(PestName) then
  begin
    if not ModelUpToDate(PestName, PestDate) then
    begin
      result := False;
      Beep;
      if (MessageDlg(StrAMoreRecentVersionPest, mtInformation,
        [mbYes, mbNo], 0, mbNo) = mrYes) then
      begin
        result := True;
      end;
    end;
  end;
end;

procedure TfrmGoPhast.CheckSvdaActivated;
var
  SvdProperties: TSingularValueDecompositionProperties;
begin
  SvdProperties := PhastModel.PestProperties.SvdProperties;
  if SvdProperties.Mode = smNone then
  begin
    Beep;
    if (MessageDlg(StrSingularValueDecom, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      SvdProperties.Mode := smNormal;
    end;
  end;
end;

procedure TfrmGoPhast.EnableSwrObs;
var
  ShouldEnable: Boolean;
  ChildIndex: Integer;
  SwrPackage: TSwrPackage;
begin
  ShouldEnable := acSWR_Tabfiles.Enabled;
  if ShouldEnable then
  begin
    SwrPackage := PhastModel.ModflowPackages.SwrPackage;
    ShouldEnable := SwrPackage.IsSelected
      and (SwrPackage.SaveObs in [ssoSaveObs, ssoSaveObsAll]);
    if not ShouldEnable and PhastModel.LgrUsed then
    begin
      for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
      begin
        SwrPackage := PhastModel.ChildModels[ChildIndex].ChildModel.ModflowPackages.SwrPackage;
        ShouldEnable := SwrPackage.IsSelected
          and (SwrPackage.SaveObs in [ssoSaveObs, ssoSaveObsAll]);
        if ShouldEnable then
        begin
          break;
        end;
      end;
    end;
  end;
  acSwrObservations.Enabled := ShouldEnable;
end;

procedure TfrmGoPhast.EnableSwrActions;
begin
  miSWR.Visible := ModelSelection in [msModflow, msModflowNWT, msModflowFmp];
  EnableSwrObs;
end;

procedure TfrmGoPhast.EnableVisualization;
var
  LocalGrid: TCustomModelGrid;
  Mesh: TSutraMesh3D;
begin
  if PhastModel = nil then
  begin
    Exit;
  end;
  case ModelSelection of
    msPhast, msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msFootPrint:
      begin
        LocalGrid := Grid;
        acDisplayData.Enabled := (LocalGrid <> nil)
          and (LocalGrid.ColumnCount > 0)
          and (LocalGrid.RowCount > 0)
          and (LocalGrid.LayerCount > 0);
      end;
    msModflow2015:
      begin
        case PhastModel.Mf6GridType of
          mgtStructured:
            begin
              LocalGrid := Grid;
              acDisplayData.Enabled := (LocalGrid <> nil)
                and (LocalGrid.ColumnCount > 0)
                and (LocalGrid.RowCount > 0)
                and (LocalGrid.LayerCount > 0);
            end;
          mgtLayered:
            begin
              acDisplayData.Enabled := (PhastModel.DisvGrid <> nil)
                and (PhastModel.DisvGrid.ActiveCellCount > 0);
            end;
          mgtUnstructured:
            begin
              acDisplayData.Enabled := False;
            end;
        end;
      end;
    msSutra22, msSutra30:
      begin
        Mesh := PhastModel.SutraMesh;
        acDisplayData.Enabled := (Mesh <> nil)
          and (Mesh.Mesh2D.Nodes.Count > 0)
          and (Mesh.Mesh2D.Elements.Count > 0)
      end;
    else
      begin
        acDisplayData.Enabled := False;
      end;
  end;
end;

procedure TfrmGoPhast.InvalidateImage32AllViews;
begin
  if FInvalidatingAllViews then
  begin
    Exit;
  end;
  FInvalidatingAllViews := True;
  try
    if csDestroying in ComponentState then
    begin
      Exit;
    end;
    frameTopView.ZoomBox.InvalidateImage32;
    frameFrontView.ZoomBox.InvalidateImage32;
    frameSideView.ZoomBox.InvalidateImage32;
  finally
    FInvalidatingAllViews := False;
  end;
end;

procedure TfrmGoPhast.ImportGlobalVariablesFile(GloVarFile: string);
var
  GloVar: TGlobalList;
  index: Integer;
  Item: TGlobalItem;
  Variable: TGlobalVariable;
  AFloat: double;
  AnInt: Integer;
  AValue: string;
  GlobalVariables: TGlobalVariables;
begin
  if (Length(GloVarFile) > 0) and (GloVarFile[1] = '"') then
  begin
    GloVarFile := Copy(GloVarFile, 2, MAXINT);
    if (Length(GloVarFile) > 0) and (GloVarFile[Length(GloVarFile)] = '"') then
    begin
      GloVarFile := Copy(GloVarFile, 1, Length(GloVarFile) - 1);
    end;
  end;
  if FileExists(GloVarFile) then
  begin
    GloVar := TGlobalList.Create;
    try
      if ReadGlobalFile(GloVarFile, GloVar) then
      begin
        GlobalVariables := TGlobalVariables.Create(nil);
        try
          GlobalVariables.Assign(PhastModel.GlobalVariables);
          for index := 0 to GloVar.Count - 1 do
          begin
            Item := GloVar[index];
            Variable :=  GlobalVariables.GetVariableByName(Item.Name);
            if Variable <> nil then
            begin
              case Variable.Format of
                rdtDouble:
                  begin
                    if TryStrToFloat(Item.Value, AFloat) then
                    begin
                      Variable.RealValue := AFloat;
                    end
                  end;
                rdtInteger:
                  if TryStrToInt(Item.Value, AnInt) then
                  begin
                    Variable.IntegerValue := AnInt;
                  end;
                rdtBoolean:
                  begin
                    if SameText(Item.Value, 'True') then
                    begin
                      Variable.BooleanValue := True;
                    end
                    else if SameText(Item.Value, 'False') then
                    begin
                      Variable.BooleanValue := False;
                    end
                  end;
                rdtString:
                  begin
                    AValue := Item.Value;
                    if (Length(AValue) > 0) then
                    begin
                      if AValue[1] = '"' then
                      begin
                        AValue := Copy(AValue, 2, MaxInt);
                        if (Length(AValue) > 0) and (AValue[Length(AValue)] = '"') then
                        begin
                          AValue := Copy(AValue, 1, Length(AValue)-1);
                        end;
                      end;
                    end;
                    Variable.StringValue := AValue;
                  end;
              end;
//              PhastModel.UpdateGlobalVariable(Variable);
            end;
          end;
          PhastModel.GlobalVariables := GlobalVariables;
        finally
          GlobalVariables.Free;
        end;
      end;
    finally
      GloVar.Free;
    end;
  end;
end;

procedure TfrmGoPhast.ImportPvalFile(PValFile: string);
var
  ParamList: TParamList;
  PvalIndex: Integer;
  ParamItem: TParamItem;
  AParam: TModflowParameter;
begin
  if (Length(PValFile) > 0) and (PValFile[1] = '"') then
  begin
    PValFile := Copy(PValFile, 2, MAXINT);
    if (Length(PValFile) > 0) and (PValFile[Length(PValFile)] = '"') then
    begin
      PValFile := Copy(PValFile, 1, Length(PValFile) - 1);
    end;
  end;
  if FileExists(PValFile) then
  begin
    ParamList := TParamList.Create;
    try
      if ReadPvalFile(PValFile, ParamList) then
      begin
        for PvalIndex := 0 to ParamList.Count - 1 do
        begin
          ParamItem := ParamList[PvalIndex];

          AParam := PhastModel.ModflowSteadyParameters.
            GetParamByName(ParamItem.Name);
          if AParam = nil then
          begin
            AParam := PhastModel.ModflowTransientParameters.
              GetParamByName(ParamItem.Name);
          end;
          if AParam = nil then
          begin
            AParam := PhastModel.HufParameters.
              GetParamByName(ParamItem.Name);
          end;
          if AParam <> nil then
          begin
            AParam.Value := ParamItem.Value;
          end;
        end;
      end;
    finally
      ParamList.Free;
    end;
  end;
end;

procedure TfrmGoPhast.ExportMt3dFromCommandLine(FileName: string);
var
  NewFileName: string;
  NameWriter: TCustomNameFileWriter;
begin
  case PhastModel.ModelSelection of
    msUndefined:
      Assert(False);
    msPhast:
      Assert(False);
    msModflow, msModflowNWT, msModflowCfp, msModflow2015:
      begin
        NewFileName := ChangeFileExt(FileName, '.nam');
        NewFileName := PhastModel.FixFileName(NewFileName);
        if PhastModel.ModflowPackages.Mt3dBasic.IsSelected then
        begin
          NameWriter := TMt3dmsNameWriter.Create(
            PhastModel, NewFileName, etExport);
          try
            PhastModel.NameFileWriter := NameWriter;
            PhastModel.ExportMt3dmsModel(FileName, False, False);
          finally
            NameWriter.Free;
            PhastModel.NameFileWriter := nil;
          end;
        end;
      end;
    msModflowLGR, msModflowLGR2, msModflowFmp:
      begin
        NewFileName := PhastModel.FixFileName(ChangeFileExt(FileName, '.lgr'));
        if PhastModel.ModflowPackages.Mt3dBasic.IsSelected then
        begin
          NameWriter := TMt3dmsNameWriter.Create(
            PhastModel, NewFileName, etExport);
          try
            PhastModel.NameFileWriter := NameWriter;
            PhastModel.ExportMt3dmsModel(FileName, False, False);
          finally
            NameWriter.Free;
            PhastModel.NameFileWriter := nil;
          end;
        end;
      end;
    msSutra22, msSutra30, msFootPrint:
      begin
        Assert(False);
      end
  else
    Assert(False);
  end;
end;

procedure TfrmGoPhast.ExportFromCommandLine(FileName: string);
var
  NewFileName: string;
  ChildModel: TChildModel;
  ChildModelNameFile: string;
  NameWriter: TCustomNameFileWriter;
  Index: Integer;
  RunSutraOK: Boolean;
  Options: TSutraOptions;
  SutraInputFileName: string;
  Mt3dExtension: string;
  Mt3dFileName: string;
  procedure WriteModflowFile;
  var
    SimNameWriter: IMf6_SimNameFileWriter;
  begin
    PhastModel.ClearModelFiles;
    NewFileName := ChangeFileExt(FileName, '.nam');
    NewFileName := PhastModel.FixFileName(NewFileName);
    NameWriter := TNameFileWriter.Create(PhastModel, NewFileName, etExport);
    SimNameWriter := TMf6_SimNameFileWriter.Create(PhastModel);
    try
      PhastModel.NameFileWriter := NameWriter;
      PhastModel.SimNameWriter := SimNameWriter;
      PhastModel.ExportModflowModel(NewFileName, False, False, False, False, False);
    finally
      NameWriter.Free;
      PhastModel.NameFileWriter := nil;
      SimNameWriter := nil;
    end;
    if PhastModel.ModflowPackages.ModPath.IsSelected then
    begin
      if (PhastModel.ModflowPackages.ModPath.MpathVersion = mp7)
        and (PhastModel.ModflowOutputControl.HeadOC.OutputFileType <> oftBinary) then
      begin
        Beep;
        MessageDlg(StrMODPATH7RequiresA, mtError, [mbOK], 0);
      end
      else
      begin
        PhastModel.ExportModpathModel(
          ChangeFileExt(FileName, '.mpn'), False, True);
      end;
    end;
    if PhastModel.ModflowPackages.ZoneBudget.IsSelected then
    begin
      PhastModel.ExportZoneBudgetModel(
        ChangeFileExt(FileName, StrZbzones), False, False);
    end;
    if PhastModel.ModflowPackages.Mt3dBasic.IsSelected then
    begin
      NameWriter := TMt3dmsNameWriter.Create(
        PhastModel, NewFileName, etExport);
      try
        PhastModel.NameFileWriter := NameWriter;
        PhastModel.ExportMt3dmsModel(FileName, False, False);
      finally
        NameWriter.Free;
        PhastModel.NameFileWriter := nil;
      end;
    end;
    PhastModel.SaveArchiveList(ChangeFileExt(FileName, '.axml'));
  end;
begin
  case PhastModel.ModelSelection of
    msUndefined:
      Assert(False);
    msPhast:
      begin
        ExportFile(ChangeFileExt(FileName, sdPhastInput.DefaultExt), False);
      end;
    msModflow, msModflowNWT, msModflowCfp, msModflow2015:
      begin
        WriteModflowFile;
      end;
    msModflowLGR, msModflowLGR2, msModflowFmp:
      begin
        if not PhastModel.LgrUsed then
        begin
          WriteModflowFile;
        end
        else
        begin
          PhastModel.ClearModelFiles;
          NewFileName := PhastModel.FixFileName(ChangeFileExt(FileName, '.lgr'));
          NameWriter := TNameFileWriter.Create(PhastModel, FileName, etExport);
          try
            PhastModel.NameFileWriter := NameWriter;
            for Index := 0 to PhastModel.ChildModels.Count - 1 do
            begin
              ChildModel := PhastModel.ChildModels[Index].ChildModel;
              ChildModelNameFile := ChildModel.Child_NameFile_Name(FileName);
              NameWriter := TNameFileWriter.Create(
                ChildModel, ChildModelNameFile, etExport);
              ChildModel.NameFileWriter := NameWriter;
            end;
            PhastModel.ExportModflowLgrModel(NewFileName, False, False, False, False, False);
          finally
            PhastModel.NameFileWriter.Free;
            PhastModel.NameFileWriter := nil;
            for Index := 0 to PhastModel.ChildModels.Count - 1 do
            begin
              ChildModel := PhastModel.ChildModels[Index].ChildModel;
              ChildModel.NameFileWriter.Free;
              ChildModel.NameFileWriter := nil;
            end;
          end;
          if PhastModel.ModflowPackages.ModPath.IsSelected then
          begin
            PhastModel.ExportModpathModel(
              ChangeFileExt(FileName, '.mpn'), False, True);
          end;
          if PhastModel.ModflowPackages.ZoneBudget.IsSelected then
          begin
            PhastModel.ExportZoneBudgetModel(
              ChangeFileExt(FileName, StrZbzones), False, False);
          end;
          if PhastModel.ModflowPackages.Mt3dBasic.IsSelected then
          begin
            NameWriter := TMt3dmsNameWriter.Create(
              PhastModel, NewFileName, etExport);
            try
              PhastModel.NameFileWriter := NameWriter;
              PhastModel.ExportMt3dmsModel(FileName, False, False);
            finally
              NameWriter.Free;
              PhastModel.NameFileWriter := nil;
            end;
          end;
          if PhastModel.LgrUsed then
          begin
            for Index := 0 to PhastModel.ChildModels.Count - 1 do
            begin
              ChildModel := PhastModel.ChildModels[Index].ChildModel;
              if ChildModel.ModflowPackages.Mt3dBasic.IsSelected then
              begin
                Mt3dExtension := ExtractFileExt(NewFileName);
                Mt3dFileName := ChildModel.Child_NameFile_Name(NewFileName);
                Mt3dFileName := ChangeFileExt(Mt3dFileName, Mt3dExtension);

                NameWriter := TMt3dmsNameWriter.Create(
                  ChildModel, Mt3dFileName, etExport);
                try
                  ChildModel.NameFileWriter := NameWriter;
                  ChildModel.ExportMt3dmsModel(Mt3dFileName, False, False);
                finally
                  NameWriter.Free;
                  ChildModel.NameFileWriter := nil;
                end;
              end;
            end;
          end;
          PhastModel.SaveArchiveList(ChangeFileExt(FileName, '.axml'));
        end;
      end;
    msSutra22, msSutra30:
      begin
        RunSutraOK := True;
        if PhastModel.SutraMesh = nil then
        begin
          RunSutraOK := False;
        end;
        Options := PhastModel.SutraOptions;
        if (Options.StartType = stWarm) then
        begin
          if not FileExists(Options.FullRestartFileName) then
          begin
            RunSutraOK := False;
          end;
        end;
        SutraInputFileName := ChangeFileExt(FileName, '.inp');
        if (Options.StartType = stWarm) then
        begin
          if UpperCase(SutraInputFileName) = UpperCase(Options.FullRestartFileName) then
          begin
            RunSutraOK := False;
          end;
        end;

        if RunSutraOK then
        begin
          ExportSutra(False, SutraInputFileName);
        end;
      end;
    msFootPrint:
      begin
        Assert(False);
      end;
  else
    Assert(False);
  end;
end;

procedure TfrmGoPhast.ImportModflowOutputFromCommandLine(AFileName: string; ImportAll: boolean);
begin
  if not (ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  if not TFile.Exists(AFileName) then
  begin
    Exit;
  end;

  with TfrmSelectResultToImport.Create(nil) do
  begin
    try
      if ShowDataSets(AFileName) then
      begin
        if ImportAll then      
        begin        
          btnSelectAllClick(nil);
        end;          
        btnOKClick(nil);
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmGoPhast.HandleCommandLineParameters(FileName: string);
var
  Option: string;
  Index: Integer;
  Parameters: TStringList;
  PValFile: string;
  GloVarFile: string;
  OutputFile: string;
begin
  if ParamCount > 1 then
  begin
    Parameters := TStringList.Create;
    try
      for Index := 2 to ParamCount do
      begin
        Parameters.Add(ParamStr(Index))
      end;
      Index := 0;
      while Index < Parameters.Count do
      begin
        Option := Parameters[Index];
        if (Length(Option) > 0) and (Option[1] = '-') then
        begin
          Option := LowerCase(Copy(Option, 2, MAXINT));
          if Option = 'p' then
          begin
            Inc(Index);
            Assert(Index < Parameters.Count);
            PValFile := Parameters[Index];
            ImportPvalFile(PValFile);
          end
          else if Option = 'g' then
          begin
            Inc(Index);
            Assert(Index < Parameters.Count);
            GloVarFile := Parameters[Index];
            ImportGlobalVariablesFile(GloVarFile);
          end
          else if Option = 'e' then
          begin
            ExportFromCommandLine(FileName);
          end
          else if Option = 'mte' then
          begin
            ExportMt3dFromCommandLine(FileName);
          end
          else if Option = 'il' then
          begin
            Inc(Index);
            Assert(Index < Parameters.Count);
            OutputFile := Parameters[Index];
            ImportModflowOutputFromCommandLine(OutputFile, False);
          end
          else if Option = 'ia' then
          begin
            Inc(Index);
            Assert(Index < Parameters.Count);
            OutputFile := Parameters[Index];
            ImportModflowOutputFromCommandLine(OutputFile, True);
          end
          else if Option = 'c' then
          begin
            FNoIniFile := True;
            Application.Terminate;
          end;
        end;

        Inc(Index);
      end;
    finally
      Parameters.Free;
    end;
  end;
end;

procedure TfrmGoPhast.ClearFileSaveDialogBoxNames;
begin
  sdModflowInput.FileName := '';
  sdPhastInput.FileName := '';
  sdZonebudgetInput.FileName := '';
  sdModelMate.FileName := '';
  sdModflowLgr.FileName := '';
  sdModpathInput.FileName := '';
  dlgSaveMt3dms.FileName := '';
  sdFootprint.FileName := '';
  sdSaveSutraMesh.FileName := '';
  sdSutraInput.FileName := '';
  FFootprintFileName := '';
end;

procedure TfrmGoPhast.MenuItemsSetEnabled(AValue: Boolean);
var
  Index: Integer;
begin
  for Index := 0 to mmMainMenu.Items.Count - 1 do
  begin
    mmMainMenu.Items[Index].Enabled := AValue;
  end;
  cbControlBar.Enabled := AValue;
  frameSideView.Enabled := AValue;
  frameTopView.Enabled := AValue;
  frameFrontView.Enabled := AValue;
  frame3DView.Enabled := AValue;
end;

function TfrmGoPhast.TestZoneBudgetLocationOK(Model: TCustomModel): Boolean;
begin
  result := True;
  if ModelSelection = msModflow2015 then
  begin
  if not FileExists(Model.ProgramLocations.ZoneBudgetLocationMf6) then
  begin
    GetProgramLocations(Model);
    if not FileExists(Model.ProgramLocations.ZoneBudgetLocationMf6) then
    begin
      Beep;
      if MessageDlg(StrZONEBUDGETDoesNot,
        mtWarning, [mbYes, mbNo], 0) <> mrYes then
      begin
        result := False;
      end;
    end;
  end;
  end
  else
  begin
  if not FileExists(Model.ProgramLocations.ZoneBudgetLocation) then
  begin
    GetProgramLocations(Model);
    if not FileExists(Model.ProgramLocations.ZoneBudgetLocation) then
    begin
      Beep;
      if MessageDlg(StrZONEBUDGETDoesNot,
        mtWarning, [mbYes, mbNo], 0) <> mrYes then
      begin
        result := False;
      end;
    end;
  end;
  end;
end;

procedure TfrmGoPhast.tmrImportErrorsTimer(Sender: TObject);
begin
  inherited;
  if frameTopView.FHasDrawn then
  begin
    tmrImportErrors.Enabled := False;
    if frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.Show;
      frmErrorsAndWarnings.BringToFront;
    end;
  end;
end;

function TfrmGoPhast.TestMt3dUsgsLocationOK(Model: TCustomModel): Boolean;
begin
  result := True;
  if not FileExists(Model.ProgramLocations.Mt3dUsgsLocation) then
  begin
    GetProgramLocations(Model);
    if not FileExists(Model.ProgramLocations.Mt3dUsgsLocation) then
    begin
      Beep;
      if MessageDlg(StrMt3dUsgsDoesNotExi, mtWarning, [mbYes, mbNo], 0) <> mrYes then
      begin
        result := False;
      end;
    end;
  end;
end;

function TfrmGoPhast.TestMt3dmsLocationOK(Model: TCustomModel): Boolean;
begin
  result := True;
  if not FileExists(Model.ProgramLocations.Mt3dmsLocation) then
  begin
    GetProgramLocations(Model);
    if not FileExists(Model.ProgramLocations.Mt3dmsLocation) then
    begin
      Beep;
      if MessageDlg(StrMt3dmsDoesNotExi, mtWarning, [mbYes, mbNo], 0) <> mrYes then
      begin
        result := False;
      end;
    end;
  end;
end;

function TfrmGoPhast.TestModpathLocationOK(Model: TCustomModel): Boolean;
begin
  result := True;
  if not FileExists(Model.ModPathLocation) then
  begin
    GetProgramLocations(Model);
    if not FileExists(Model.ModPathLocation) then
    begin
      Beep;
      if MessageDlg(StrMODPATHDoesNotExi, mtWarning,
        [mbYes, mbNo], 0) <> mrYes then
      begin
        result := False;
      end;
    end;
  end;
end;

procedure TfrmGoPhast.EndSupressDrawing;
begin
  Dec(FSupressDrawing);
  if FSupressDrawing = 0 then
  begin
    Invalidate3DView(nil);
  end;
end;

procedure TfrmGoPhast.GetProgramLocations(Model: TCustomModel);
var
  CurrentDir: string;
begin
  CurrentDir := GetCurrentDir;
  try
    with TfrmProgramLocations.Create(nil) do
    begin
      try
        GetData(Model);
        ShowModal;
      finally
        Free;
      end;
    end;
  finally
    SetCurrentDir(CurrentDir);
  end;
end;

function TfrmGoPhast.GetSutraMesh: TSutraMesh3D;
begin
  if PhastModel = nil then
  begin
    result := nil;
  end
  else
  begin
    result := PhastModel.SutraMesh;
  end;
end;

procedure TfrmGoPhast.miEndpointsatEndingLocationstoShapefileClick(
  Sender: TObject);
var
  FileName: string;
  AModel: TCustomModel;
begin
  inherited;
  FileName := ChangeFileExt(PhastModel.ModelFileName, '');
  if FileName = '' then
  begin
    FileName := IncludeTrailingPathDelimiter(GetCurrentDir)
      + StrEndpointsAtEndshp;
  end
  else
  begin
    FileName := FileName + '_' + StrEndpointsAtEndshp;
  end;
  sdShapefile.FileName := FileName;
  if sdShapefile.Execute then
  begin
    if FExportModpathShapeFileModelChoice > 0 then
    begin
      AModel := PhastModel.ChildModels[
        FExportModpathShapeFileModelChoice-1].ChildModel;
    end
    else
    begin
      AModel := PhastModel;
    end;
    AModel.EndPoints.
      ExportShapefileAtEndingLocations(sdShapefile.FileName);
  end;
end;

procedure TfrmGoPhast.miEndpointsatStartingLocationstoShapefileClick(
  Sender: TObject);
var
  FileName: string;
  AModel: TCustomModel;
begin
  inherited;
  FileName := ChangeFileExt(PhastModel.ModelFileName, '');
  if FileName = '' then
  begin
    FileName := IncludeTrailingPathDelimiter(GetCurrentDir)
      + StrEndpointsAtStartshp;
  end
  else
  begin
    FileName := FileName + '_' + StrEndpointsAtStartshp;
  end;
  sdShapefile.FileName := FileName;
  if sdShapefile.Execute then
  begin
    if FExportModpathShapeFileModelChoice > 0 then
    begin
      AModel := PhastModel.ChildModels[
        FExportModpathShapeFileModelChoice-1].ChildModel;
    end
    else
    begin
      AModel := PhastModel;
    end;
    AModel.EndPoints.
      ExportShapefileAtStartingLocations(sdShapefile.FileName);
  end;
end;

procedure TfrmGoPhast.EnableLinkStreams;
begin
  miLinkSFRStreams.Enabled :=
    (PhastModel.ModelSelection in ModflowSelection)
    and (PhastModel.SfrIsSelected or PhastModel.StrIsSelected
    or PhastModel.SwrIsSelected or PhastModel.Sfr6IsSelected
    );
end;

procedure TfrmGoPhast.ShowOrHideAllScreenObjects(ShowAll: Boolean);
var
  ScreenObject: TScreenObject;
  Position: Integer;
  Undo: TUndoShowHideScreenObject;
  Index: Integer;
begin
  Position := -1;
  Undo := TUndoShowHideScreenObject.Create;
  try
    for Index := 0 to PhastModel.ScreenObjectCount - 1 do
    begin
      ScreenObject := PhastModel.ScreenObjects[Index];
      if ScreenObject.Visible <> ShowAll then
      begin
        Position := Undo.AddScreenObjectToChange(ScreenObject);
      end;
    end;
    Undo.SetPostSelection;
  finally
    if Position = -1 then
    begin
      Undo.Free;
    end
    else
    begin
      UndoStack.Submit(Undo);
    end;
  end;
end;

procedure TfrmGoPhast.miSpecifyFishnetQuadrilateralClick(Sender: TObject);
begin
  inherited;
  with TfrmSpecifyMesh.Create(nil) do
  begin
    try
      GetData(mkQuadrilaterals);
      ShowModal;
    finally
      Free;
    end;
  end;

end;

procedure TfrmGoPhast.miSpecifyMeshClick(Sender: TObject);
begin
  inherited;
  with TfrmSpecifyMesh.Create(nil) do
  begin
    try
      GetData(mkComplete);
      ShowModal;
    finally
      Free;
    end;
  end;
end;

procedure TfrmGoPhast.UpdatePermanantDialogBoxAppearances;
begin
  if frmDisplayData <> nil then
  begin
    frmDisplayData.CustomizeControls;
  end;
//  if frmGridColor <> nil then
//  begin
//    frmGridColor.CustomizeControls;
//  end;
  frmProgressMM.CustomizeControls;
  frmSelectedObjects.CustomizeControls;
  frmColors.CustomizeControls;
  frmErrorsAndWarnings.CustomizeControls;
  if frmGridValue <> nil then
  begin
    frmGridValue.CustomizeControls;
  end;
end;

procedure TfrmGoPhast.UpdateVerticalExaggeration(VerticalExaggeration: Double);
var
  FrontCenter: TPoint2D;
  SideCenter: TPoint2D;
  TopCenter: TPoint2D;
  Temp: TPoint2D;
begin
  if VerticalExaggeration = 0 then
  begin
    VerticalExaggeration := 1;
  end;

  if (ModelSelection in SutraSelection) and (PhastModel.SutraMesh.MeshType = mtProfile) then
  begin
    if (frameTopView <> nil) and (frameTopView.ZoomBox.Exaggeration <>
      VerticalExaggeration) then
    begin
      FrontCenter.X := frameTopView.ZoomBox.X(
        frameTopView.ZoomBox.Image32.Width div 2);
      FrontCenter.Y := frameTopView.ZoomBox.Y(
        frameTopView.ZoomBox.Image32.Height div 2);

      PhastModel.Exaggeration := VerticalExaggeration;

      Temp.X := frameTopView.ZoomBox.X(
        frameTopView.ZoomBox.Image32.Width div 2);
      Temp.Y := frameTopView.ZoomBox.Y(
        frameTopView.ZoomBox.Image32.Height div 2);
      frameTopView.ZoomBox.OriginX := frameTopView.ZoomBox.OriginX
        - Temp.X + TopCenter.X;
      frameTopView.ZoomBox.OriginY := frameTopView.ZoomBox.OriginY
        - Temp.Y + TopCenter.Y;

      TopDiscretizationChanged := True;
      AdjustScales;
    end;
  end
  else
  begin
    frameTopView.ZoomBox.Exaggeration := 1;
    if (frameFrontView <> nil) and (frameFrontView.ZoomBox.Exaggeration <>
      VerticalExaggeration)
      or (frameSideView <> nil) and (frameSideView.ZoomBox.Exaggeration <>
      VerticalExaggeration) then
    begin
      FrontCenter.X := frameFrontView.ZoomBox.X(
        frameFrontView.ZoomBox.Image32.Width div 2);
      FrontCenter.Y := frameFrontView.ZoomBox.Y(
        frameFrontView.ZoomBox.Image32.Height div 2);
      SideCenter.X := frameSideView.ZoomBox.X(
        frameSideView.ZoomBox.Image32.Width div 2);
      SideCenter.Y := frameSideView.ZoomBox.Y(
        frameSideView.ZoomBox.Image32.Height div 2);

      PhastModel.Exaggeration := VerticalExaggeration;

      Temp.X := frameFrontView.ZoomBox.X(
        frameFrontView.ZoomBox.Image32.Width div 2);
      Temp.Y := frameFrontView.ZoomBox.Y(
        frameFrontView.ZoomBox.Image32.Height div 2);
      frameFrontView.ZoomBox.OriginX := frameFrontView.ZoomBox.OriginX
        - Temp.X + FrontCenter.X;
      frameFrontView.ZoomBox.OriginY := frameFrontView.ZoomBox.OriginY
        - Temp.Y + FrontCenter.Y;

      Temp.X := frameSideView.ZoomBox.X(
        frameSideView.ZoomBox.Image32.Width div 2);
      Temp.Y := frameSideView.ZoomBox.Y(
        frameSideView.ZoomBox.Image32.Height div 2);
      frameSideView.ZoomBox.OriginX := frameSideView.ZoomBox.OriginX
        - Temp.X + SideCenter.X;
      frameSideView.ZoomBox.OriginY := frameSideView.ZoomBox.OriginY
        - Temp.Y + SideCenter.Y;

      FrontDiscretizationChanged := True;
      SideDiscretizationChanged := True;
      frameSideView.ZoomBox.InvalidateImage32;
      frameFrontView.ZoomBox.InvalidateImage32;
      AdjustScales;
    end;
  end;
end;

procedure TfrmGoPhast.InvalidateScreenObjects;
begin
  if PhastModel <> nil then
  begin
    PhastModel.InvalidateScreenObjects;
  end;
end;

procedure TfrmGoPhast.InvalidateSide;
begin
  // redraw the side view.
  if frameSideView <> nil then
  begin
    frameSideView.ZoomBox.InvalidateImage32;
  end;
end;

procedure TfrmGoPhast.acGridAngleExecute(Sender: TObject);
begin
  inherited;
  // Allow the user to change the grid angle.
  Application.CreateForm(TfrmGridAngle, frmGridAngle);
  try
    frameTopView.DeltaGridAngle := 0;
    frmGridAngle.ActionToTake := attRotateAroundCenter;
    frmGridAngle.ShowModal;
  finally
    // There is a test to see if frmGridAngle is equal to nil
    // when drawing the rotated grid.
    FreeAndNil(frmGridAngle);
  end;
end;

procedure TfrmGoPhast.acEditGridLinesExecute(Sender: TObject);
begin
  inherited;
  // Allow the user the edit the positions of the grid lines.
  ShowAForm(TfrmGridSpacing);
end;

procedure TfrmGoPhast.acSmoothGridExecute(Sender: TObject);
begin
  inherited;
  // Allow the user the smooth the grid.
  ShowAForm(TfrmSmoothGrid);
end;

procedure TfrmGoPhast.acSpecifyCrossSectionExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmSutraAngle);
end;

procedure TfrmGoPhast.acStructuredGridExecute(Sender: TObject);
begin
  inherited;
  if PhastModel.Mf6GridType = mgtStructured then
  begin
    Exit;
  end;
  tbSelectClick(acSelectObjects);
  UndoStack.Submit(TUndoChangeGridType.Create(mgtStructured));
//  SetActionChecked(Sender);
end;

procedure TfrmGoPhast.miObjectstoShapefileClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmExportShapefileObjects);
end;

//procedure TfrmGoPhast.NoClick(Sender: TObject);
//begin
//  tmrSaveTimer.Enabled := False;
//  frmSaveModelDialog.Close;
//end;

//procedure TfrmGoPhast.OnSaveFormClose(Sender: TObject; var Action: TCloseAction);
//begin
//  tmrSaveTimer.Enabled := False;
//  frmSaveModelDialog := nil;
//  Action := caFree;
//end;

//procedure TfrmGoPhast.YesClick(Sender: TObject);
//begin
//  tmrSaveTimer.Enabled := False;
//  frmSaveModelDialog.Close;
//  acFileSaveExecute(nil);
//end;

procedure TfrmGoPhast.OnAppIdle(Sender: TObject; var Done: Boolean);
const
  OneHour = 1/24;
//var
//  YesButton: TButton;
//  NoButton: TButton;
begin
  // This assigns the event handlers to the undo/redo buttons and
  // undo/redo menu items.
  if UndoStack <> nil then
  begin
//    UndoStack.SetUndoMenuItems(miUndo, miRedo);
    UndoStack.SetUndoActions(acUndo, acRedo);
  end;

//  if (Now - FSaveTime > OneHour) and (frmDataSets = nil)
//    and (frmGlobalVariables = nil) and (frmScreenObjectProperties <> nil) and
//    not frmScreenObjectProperties.Visible and not ShowingForm then
//  begin
//    Beep;
//    FSaveTime := Now;
//    tmrSaveTimer.Enabled := True;
//
//    if PhastModel.ModelFileName <> '' then
//    begin
//      frmSaveModelDialog := CreateMessageDialog(Format(
//        StrDoYouWantToSaveModelFileName,
//        [PhastModel.ModelFileName]), mtInformation, [mbYes, mbNo], mbYes);
//    end
//    else
//    begin
//      frmSaveModelDialog := CreateMessageDialog(StrDoYouWantToSaveModel,
//        mtInformation, [mbYes, mbNo], mbYes);
//    end;
//    frmSaveModelDialog.OnClose := OnSaveFormClose;
//    YesButton := frmSaveModelDialog.FindComponent('Yes') as TButton;
//    YesButton.OnClick := YesClick;
//    NoButton := frmSaveModelDialog.FindComponent('No') as TButton;
//    NoButton.OnClick := NoClick;
//    frmSaveModelDialog.Show;
//  end;
end;

procedure TfrmGoPhast.SaveModelMateProject;
var
  FileStream: TFileStream;     // Text file defining one or more objects.
  MemStream: TMemoryStream;    // Temporarily hold an object.
  TextStream: TMemoryStream;   // Text form of a stream.
begin
  MemStream := TMemoryStream.Create;
  TextStream := TMemoryStream.Create;
  
  // Open a text file
  FileStream := TFileStream.Create(sdModelMate.FileName, fmCreate);
  try
    //
    // Write the TProject data to the memory stream.
    MemStream.WriteComponent(PhastModel.ModelMateProject);
    //MemStream.WriteComponent(PCurrent.UcProject);
    MemStream.Position := 0;
    // Convert the memory stream to a text stream.
    ObjectBinaryToText(MemStream, TextStream);
    // write the TProject text stream to the text file.
    TextStream.Position := 0;
    FileStream.CopyFrom(TextStream, TextStream.Size);
    //
  finally
    // Free all streams.
    FileStream.Free;
    MemStream.Free;
    TextStream.Free;
  end;
end;

procedure TfrmGoPhast.acFontExecute(Sender: TObject);
begin
{ TODO : Make sure the status bar is resized to be appropriate for the
selected font. }  
  // Change the font for the application.
  // Other forms will copy the
  // font from frmGoPhast when they are created.
  fdFontDialog.Font := Font;
  if fdFontDialog.Execute then
  begin
    UndoStack.Submit(TUndoChangeFont.Create(fdFontDialog.Font));
  end;
end;

procedure TfrmGoPhast.acFootPrintActiveExecute(Sender: TObject);
begin
  inherited;
  if ModelSelection <> msFootPrint then
  begin
    UndoStack.Submit(TUndoModelSelectionChange.Create(msFootPrint));
  end;
end;

procedure TfrmGoPhast.acFootprintProgramLocationExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmFootprintLocation);
end;

procedure TfrmGoPhast.acFootprintPropertiesExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmFootprintProperties);
end;

procedure TfrmGoPhast.acHeadObsToShapefileExecute(Sender: TObject);
var
  FileRoot: TFileName;
  ModelIndex: Integer;
  AChild: TChildModel;
  FileName: TFileName;
begin
  inherited;
  //
  if dlgSaveHeadObsToShapefile.Execute then
  begin
    PhastModel.ExportHeadObservationsToShapeFile(dlgSaveHeadObsToShapefile.FileName);
    if PhastModel.LgrUsed then
    begin
      FileRoot := ChangeFileExt(dlgSaveHeadObsToShapefile.FileName, '');
      for ModelIndex := 0 to PhastModel.ChildModels.Count - 1 do
      begin
        AChild := PhastModel.ChildModels[ModelIndex].ChildModel;
        FileName := FileRoot + AChild.ModelName + '.shp';
        AChild.ExportHeadObservationsToShapeFile(FileName);
      end;
    end;
  end;
end;

procedure TfrmGoPhast.EnableEditRipPlantGroups(Sender: TObject);
begin
  acRipPlantGroups.Enabled := PhastModel.RipIsSelected;
end;

procedure TfrmGoPhast.EnableExportHeadObs(Sender: TObject);
var
  ActionEnabled: boolean;
  ChildIndex: integer;
begin
  ActionEnabled :=  PhastModel.HeadObsResults.Count > 0;
  if not ActionEnabled and PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ActionEnabled := PhastModel.ChildModels[
        ChildIndex].ChildModel.HeadObsResults.Count > 0;
      if ActionEnabled then
      begin
        break;
      end;
    end;
  end;
  acHeadObsToShapefile.Enabled := ActionEnabled;
  if frmDisplayData <> nil then
  begin
    frmDisplayData.frameHeadObservationResults.UpdateSelectedModel;
  end;
end;

procedure TfrmGoPhast.EnableModelMate;
var
  ShouldEnable: boolean;
begin
  ShouldEnable := (ModelSelection in ModflowSelection) and (ModelSelection <> msModflow2015);
  acExportModelMate.Enabled := ShouldEnable;
  acImportModelMate.Enabled := ShouldEnable;
end;

procedure TfrmGoPhast.EnableModpathToShapefile;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  miPathlinestoShapefile.Enabled := PhastModel.PathLines.HasData;
  if not miPathlinestoShapefile.Enabled
    and PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      miPathlinestoShapefile.Enabled := ChildModel.PathLines.HasData;
      if miPathlinestoShapefile.Enabled then
      begin
        break;
      end;
    end;
  end;

  miEndpointsatStartingLocationstoShapefile.Enabled :=
    PhastModel.EndPoints.HasData;
  if not miEndpointsatStartingLocationstoShapefile.Enabled
    and PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      miEndpointsatStartingLocationstoShapefile.Enabled :=
        ChildModel.EndPoints.HasData;
      if miEndpointsatStartingLocationstoShapefile.Enabled then
      begin
        break;
      end;
    end;
  end;
  miEndpointsatEndingLocationstoShapefile.Enabled :=
    miEndpointsatStartingLocationstoShapefile.Enabled;


  miTimeSeriestoShapefile.Enabled := PhastModel.TimeSeries.HasData;
  if not miTimeSeriestoShapefile.Enabled
    and PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      miTimeSeriestoShapefile.Enabled :=
        ChildModel.TimeSeries.HasData;
      if miTimeSeriestoShapefile.Enabled then
      begin
        break;
      end;
    end;
  end;
end;

function TfrmGoPhast.GetFootprintInputFileName: string;
begin
  result := FFootprintFileName;
  if (result = '') and (PhastModel.ModelFileName <> '') then
  begin
    result := ChangeFileExt(PhastModel.ModelFileName, sdFootprint.DefaultExt);
    result := PhastModel.FixFileName(result);
    FFootprintFileName := Result;
  end;
end;

procedure TfrmGoPhast.HaveUsersDefineModflowLayers;
begin
  if PhastModel.LayerCount <= 0 then
  begin
    Beep;
    acLayersExecute(nil);
  end;
end;

procedure TfrmGoPhast.HaveUsersDefineSutraLayers;
begin
  if (PhastModel.SutraMesh.MeshType = mt3D) and (PhastModel.SutraMesh.LayerCount = 0) then
  begin
    Beep;
    acSutraLayersExecute(nil);
  end;
end;

procedure TfrmGoPhast.EnableGridItems;
var
  GridModel: Boolean;
begin
  GridModel := (PhastModel.ModelSelection in ModelsWithGrid);
  miShow2DGridlines.Visible := GridModel;
  GridModel := GridModel and not DisvUsed;
  miGrid.Visible := GridModel;
  acShowGridShell.Visible := GridModel;
  acShowTopGrid.Visible := GridModel;
  acShowFrontGrid.Visible := GridModel;
  acShowSideGrid.Visible := GridModel;
  acColoredGrid.Visible := GridModel;
  // This may need to be changed later.
  acShow3DObjects.Visible := GridModel;
end;

function TfrmGoPhast.TestCompatibleModflowMt3d: Boolean;
var
  ModelProgramName: string;
begin
  result := True;
  if (PhastModel.ModelSelection = msModflow)
    and PhastModel.Mt3dmsIsSelected
    and (PhastModel.ModflowPackages.Mt3dBasic.Mt3dVersion = mvMS) then
  begin
    ModelProgramName := PhastModel.ModflowLocation;
    if FileExists(ModelProgramName)
      and (CompareText(ExtractFileName(ModelProgramName),
      'mf2005dbl.exe') = 0) then
    begin
      Beep;
      if not (MessageDlg(StrTheUsualVersionOf, mtWarning, [mbYes, mbNo],
        0, mbNo) = mrYes) then
      begin
        result := false;
      end;
    end;
  end;
  if (PhastModel.ModelSelection = msModflowNWT)
    and PhastModel.Mt3dmsIsSelected
    and (PhastModel.ModflowPackages.Mt3dBasic.Mt3dVersion = mvMS)
    and (PhastModel.SfrIsSelected or PhastModel.LakIsSelected or PhastModel.UzfIsSelected) then
  begin
//    ModelProgramName := PhastModel.ModflowLocation;
    //if FileExists(ModelProgramName)
//      and (CompareText(ExtractFileName(ModelProgramName),
      //'mf2005dbl.exe') = 0) then
//    begin
      Beep;
      MessageDlg('MODFLOW-NWT can not be used with MT3DMS if the LAK, SFR, or UZF packages are selected. Use MT3D-USGS instead.', mtError, [mbOK],
        0);
//      begin
        result := false;
//      end;
//    end;
  end;
end;

procedure TfrmGoPhast.acAddLinesToObjectExecute(Sender: TObject);
begin
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbAddLinesToObjects.OnMouseDown(tbAddLinesToObjects, mbLeft, [ssLeft], 0, 0);
  end;

  if tbAddLinesToObjects.Down and tbAddLinesToObjects.Enabled then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbAddLinesToObjects);
    // Set the cursors.
    SetZB_Cursors(crMultiPartLine);
    // don't draw a rectangle around the selected screen objects.
    CurrentTool := AddLinePartTool;
  end
  else
  begin
    CurrentTool := nil;
    tbAddLinesToObjects.Down := False;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.acAddPilotPointExecute(Sender: TObject);
begin
  inherited;
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbAddPilotPoint.OnMouseDown(tbAddPilotPoint, mbLeft, [ssLeft], 0, 0);
  end;

  if tbAddPilotPoint.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbAddPilotPoint);
    // Set the cursors.
    SetZB_Cursors(crAddPilotPoint);
    // don't draw a rectangle around the selected screen objects.
    CurrentTool := AddPilotPointTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.acAddPointsToObjectExecute(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbAddPointsToObject.OnMouseDown(tbAddPointsToObject, mbLeft, [ssLeft], 0, 0);
  end;

  if tbAddPointsToObject.Down and tbAddPointsToObject.Enabled then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbAddPointsToObject);
    // Set the cursors.
    SetZB_Cursors(crMultiPartPoint);
    // don't draw a rectangle around the selected screen objects.
    CurrentTool := AddPointPartTool;
  end
  else
  begin
    CurrentTool := nil;
    tbAddPointsToObject.Down := False;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.acAddPolygonsToObjectExecute(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbAddPartsToObject.OnMouseDown(tbAddPartsToObject, mbLeft, [ssLeft], 0, 0);
  end;

  if tbAddPartsToObject.Down and tbAddPartsToObject.Enabled then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbAddPartsToObject);
    // Set the cursors.
    SetZB_Cursors(crMultiPartPolygon);
    // don't draw a rectangle around the selected screen objects.
    CurrentTool := AddPolygonPartTool;
  end
  else
  begin
    CurrentTool := nil;
    tbAddPartsToObject.Down := False;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.acAnonymizeObjectsExecute(Sender: TObject);
begin
  inherited;
  UndoStack.Submit(TUndoAnonymizeScreenObject.Create);
end;

procedure TfrmGoPhast.acArchiveModelExecute(Sender: TObject);
begin
  inherited;
  CreateArchive := not CreateArchive;
  if CreateArchive then
  begin
    FDefaultCreateArchive := dcaSave;
  end
  else
  begin
    FDefaultCreateArchive := dcaDontSave;
  end;
  acArchiveModel.Checked := CreateArchive;
end;

procedure TfrmGoPhast.acCalcSuperParametersExecute(Sender: TObject);
begin
  inherited;
  if not PestVersionOK then
  begin
    Exit;
  end;

  CheckSvdaActivated;
  if PhastModel.ModelFileName <> '' then
  begin
    PhastModel.SupCalcProperties.FileName := ChangeFileExt(PhastModel.ModelFileName, '.pst')
  end;
  frmSupCalc := TfrmSupCalc.Create(nil);
  try
    frmSupCalc.ShowModal;
    if frmSupCalc.ModalResult = mrOK then
    begin
      PhastModel.ExportSupCalcInput;
    end;
  finally
    frmSupCalc.Free;
  end;
end;

procedure TfrmGoPhast.acColorExecute(Sender: TObject);
begin
  // Change the color for the application.
  // Other forms will copy the
  // color from frmGoPhast when they are created.
  cdColorDialog.Color := Color;
  if cdColorDialog.Execute then
  begin
    Color := cdColorDialog.Color;
    GlobalColor := Color;
    UpdatePermanantDialogBoxAppearances;
//    mmMainMenu.Color := Color;
  end;

end;

procedure TfrmGoPhast.acCopyExecute(Sender: TObject);
begin
  inherited;
  PhastModel.CopyScreenObjectsToClipboard;
end;

procedure TfrmGoPhast.acCutExecute(Sender: TObject);
var
  SelectedScreenObjects: TScreenObjectList;
  Index: Integer;
  AScreenObject: TScreenObject;
  UndoCutScreenObjects: TUndoCutScreenObjects;
begin
  inherited;
  SelectedScreenObjects := TScreenObjectList.Create;
  try
    SelectedScreenObjects.Capacity := PhastModel.ScreenObjectCount;
    for Index := PhastModel.ScreenObjectCount - 1 downto 0 do
    begin
      AScreenObject := PhastModel.ScreenObjects[Index] as TScreenObject;
      if AScreenObject.Selected then
      begin
        SelectedScreenObjects.Add(AScreenObject);
      end;
    end;
    if SelectedScreenObjects.Count > 0 then
    begin
      UndoCutScreenObjects := TUndoCutScreenObjects.Create(SelectedScreenObjects);
      UndoStack.Submit(UndoCutScreenObjects);
      UndoCutScreenObjects.SetPostSelection;
    end;
  finally
    SelectedScreenObjects.Free;
  end;
end;

procedure TfrmGoPhast.miAboutClick(Sender: TObject);
begin
  // Show the about box.
  if frmAbout = nil then
  begin
    Application.CreateForm(TfrmAbout, frmAbout);
    frmAbout.Show;
  end
  else
  begin
    frmAbout.SetFocus;
  end;
end;

procedure TfrmGoPhast.miAllVideosClick(Sender: TObject);
begin
  inherited;

  LaunchURL(FBrowser, VideoUrl);
end;

procedure TfrmGoPhast.miASCII_RasterFileClick(Sender: TObject);
begin
  inherited;
  with TfrmImportAsciiRaster.Create(nil) do
  begin
    try
      if GetData then
      begin
        ShowModal;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmGoPhast.miBatchFileAdditionsClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmBatchFileAdditions);
end;

procedure TfrmGoPhast.SetCursorGrid(const Value: TCursorGrid);
begin
  if FCursorGrid <> Value then
  begin
    // record the view of the grid over which the mouse was moved.
    FCursorGrid := Value;
  end;
end;

procedure TfrmGoPhast.pnlLowerRightMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  CursorGrid := cgNone;
end;

procedure TfrmGoPhast.miProgramLocationsClick(Sender: TObject);
begin
  inherited;
  GetProgramLocations(PhastModel);
end;

procedure TfrmGoPhast.ShallAllObjects1Click(Sender: TObject);
begin
  inherited;
  ShowOrHideAllScreenObjects(True);
end;

procedure TfrmGoPhast.acShowGridValuesClick(Sender: TObject);
begin
  inherited;
  if frmGridValue = nil then
  begin
    frmGridValue := TfrmGridValue.Create(self);
  end;
  frmGridValue.Show;
end;

procedure TfrmGoPhast.acShowOrHideRulersExecute(Sender: TObject);
begin
  inherited;
  RulersVisible := not RulersVisible;
end;

procedure TfrmGoPhast.ShowHint(Sender: TObject);
begin
  // Show the hint on the status bar too.
  sbMain.Panels[0].Text := GetLongHint(Application.Hint);
end;

procedure TfrmGoPhast.tbLineClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbLine.OnMouseDown(tbLine, mbLeft, [ssLeft], 0, 0);
  end;

  if tbLine.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbLine);
    // Set the cursors.
    SetZB_Cursors(crLineArrow);
    CurrentTool := CreateLineScreenObjectTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.tbMoveNodesClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbMoveNodes.OnMouseDown(tbMoveNodes, mbLeft, [ssLeft], 0, 0);
  end;

  if tbMoveNodes.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbMoveNodes);
    // Set the cursors.
    SetZB_Cursors(crMoveNode);
    // don't draw a rectangle around the selected screen objects.
    CurrentTool := MoveSutraNodesTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.tbPolygonClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbPolygon.OnMouseDown(tbPolygon, mbLeft, [ssLeft], 0, 0);
  end;

  if tbPolygon.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbPolygon);
    // Set the cursors.
    SetZB_Cursors(crPolygonArrow);
    CurrentTool := CreateLineScreenObjectTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.tbStraightLineClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbStraightLine.OnMouseDown(tbStraightLine, mbLeft, [ssLeft], 0, 0);
  end;

  if tbStraightLine.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbStraightLine);
    // Set the cursors.
    SetZB_Cursors(crStraightLineArrow);
    CurrentTool := CreateStraightLineScreenObjectTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.tbUndoClick(Sender: TObject);
begin
  inherited;
  UndoStack.UndoEvent(Sender);
end;

procedure TfrmGoPhast.tbRectangleClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbRectangle.OnMouseDown(tbRectangle, mbLeft, [ssLeft], 0, 0);
  end;

  if tbRectangle.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbRectangle);
    // Set the cursors.
    SetZB_Cursors(crRectangleArrow);
    CurrentTool := CreateRectangleScreenObjectTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.tbRedoClick(Sender: TObject);
begin
  inherited;
  UndoStack.RedoEvent(Sender);
end;

procedure TfrmGoPhast.ClearSelectedNodes;
var
  Index: integer;
  AScreenObject: TScreenObject;
  //  Update: boolean;
  UndoChangeSelection: TUndoChangeSelection;
begin
  // Make sure no screen objects have selected nodes.
  UndoChangeSelection := TUndoChangeSelection.Create;
  //  Update := False;
  for Index := 0 to PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := PhastModel.ScreenObjects[Index];
    if AScreenObject.SelectedVertexCount > 0 then
    begin
      AScreenObject.ClearSelectedVertices;
      //      Update := True;
    end;
  end;

  UndoChangeSelection.SetPostSelection;
  if UndoChangeSelection.SelectionChanged then
  begin
    UndoStack.Submit(UndoChangeSelection);
  end
  else
  begin
    UndoChangeSelection.Free;
  end;
end;

procedure TfrmGoPhast.miContourstoShapefileClick(Sender: TObject);
var
  ContourExporter: TContourExtractor;
  ContourDataSet: TDataArray;
  LocalModel: TCustomModel;
begin
  inherited;
  if Grid <> nil then
  begin
    ContourDataSet := Grid.TopContourDataSet;
  end
  else
  begin
    Assert(PhastModel.Mesh3D <> nil);
    ContourDataSet := PhastModel.Mesh3D.TopContourDataSet;
  end;
  if ContourDataSet = nil then
  begin
    Beep;
    MessageDlg(StrYouMustContourDat, mtWarning, [mbOK], 0);
  end
  else if PhastModel.ContourLegend.Values.Count = 0 then
  begin
    Beep;
    MessageDlg(StrThereAreNoContour, mtWarning, [mbOK], 0);
  end
  else
  begin
    sdShapefile.FileName := IncludeTrailingPathDelimiter(GetCurrentDir)
      + ContourDataSet.Name + '.shp';
    if sdShapefile.Execute then
    begin
      if FExportModpathShapeFileModelChoice > 0 then
      begin
        LocalModel := PhastModel.ChildModels[FExportModpathShapeFileModelChoice-1].ChildModel;
        ContourDataSet := LocalModel.Grid.TopContourDataSet;
      end
      else
      begin
        LocalModel := PhastModel;
      end;

      ContourExporter := TContourExtractor.Create(LocalModel);
      try
        ContourExporter.CreateShapes(LocalModel.ContourLegend.Values,
          ContourDataSet, sdShapefile.FileName, LocalModel.ContourLabelSpacing);
      finally
        ContourExporter.Free;
      end;
    end;
  end;
end;

procedure TfrmGoPhast.miCustomizeSutraMeshClick(Sender: TObject);
begin
  inherited;
  if frmCustomizeMesh = nil then
  begin
    Application.CreateForm(TfrmCustomizeMesh, frmCustomizeMesh);
  end;
  frmCustomizeMesh.GetData;
  frmCustomizeMesh.Show;
end;

procedure TfrmGoPhast.ConvertPoint(Sender: TObject; VD: TViewDirection;
  const RealPoint: TPoint2D; var ScreenCoordinate: TPoint);
begin
  case VD of
    vdTop:
      begin
        ScreenCoordinate := frameTopView.ConvertPoint(RealPoint);
      end;
    vdFront:
      begin
        ScreenCoordinate := frameFrontView.ConvertPoint(RealPoint);
      end;
    vdSide:
      begin
        ScreenCoordinate := frameSideView.ConvertPoint(RealPoint);
      end;
  else
    Assert(False);
  end;
end;

procedure TfrmGoPhast.tbSelectClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbSelect.OnMouseDown(tbSelect, mbLeft, [ssLeft], 0, 0);
  end;

  if tbSelect.Down then
  begin
    if not FCreatingMainForm then
    begin
      // When you want to select screen objects, don't let any nodes be selected.
      ClearSelectedNodes;
    end;
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbSelect);
    // Set the cursors.
    SetZB_Cursors(crArrow);
    // Show a rectangle around the selected nodes.
    frameTopView.UpdateSelectRectangle;
    frameFrontView.UpdateSelectRectangle;
    frameSideView.UpdateSelectRectangle;
    CurrentTool := SelectScreenObjectTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.miHintDelayClick(Sender: TObject);
begin
  // Allow the user to adjust how long hints should be visible.
  ShowAForm(TfrmHintDelay);
end;

procedure TfrmGoPhast.miTimeSeriestoShapefileClick(Sender: TObject);
var
  FileName: string;
  AModel: TCustomModel;
begin
  inherited;
  FileName := ChangeFileExt(PhastModel.ModelFileName, '');
  if FileName = '' then
  begin
    FileName := IncludeTrailingPathDelimiter(GetCurrentDir)
      + StrTimeSeriesshp;
  end
  else
  begin
    FileName := FileName + '_' + StrTimeSeriesshp;
  end;
  sdShapefile.FileName := FileName;
  if sdShapefile.Execute then
  begin
    if FExportModpathShapeFileModelChoice > 0 then
    begin
      AModel := PhastModel.ChildModels[
        FExportModpathShapeFileModelChoice-1].ChildModel;
    end
    else
    begin
      AModel := PhastModel;
    end;
    AModel.TimeSeries.ExportShapefile(sdShapefile.FileName);
  end;
end;

procedure TfrmGoPhast.SetTopDiscretizationChanged(const Value: boolean);
begin
  FTopDiscretizationChanged := Value;
end;

procedure TfrmGoPhast.SetTopScreenObjectsChanged(const Value: boolean);
begin
  // When the top screen objects have changed redraw.
  FTopScreenObjectsChanged := Value;
  if FTopScreenObjectsChanged and not (csDestroying in ComponentState) then
  begin
    frameTopView.ZoomBox.InvalidateImage32;
  end;
end;

function TfrmGoPhast.ResetSelectedScreenObjects: boolean;
begin
  // Deselect all objects.
  result := PhastModel.ResetSelectedScreenObjects;
end;

procedure TfrmGoPhast.acExitExecute(Sender: TObject);
begin
  // Closing the main form (this one) closes the application.
  Close;
end;

procedure TfrmGoPhast.tbLassoClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbLasso.OnMouseDown(tbLasso, mbLeft, [ssLeft], 0, 0);
  end;

  if tbLasso.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbLasso);
    // Set the cursors.
    SetZB_Cursors(crArrow);
  end;
  CurrentTool := LassoTool;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.tbSelectPointClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbSelectPoint.OnMouseDown(tbSelectPoint, mbLeft, [ssLeft], 0, 0);
  end;

  if tbSelectPoint.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbSelectPoint);
    // Set the cursors.
    SetZB_Cursors(crSelectPoint);
    // don't draw a rectangle around the selected screen objects.
    CurrentTool := SelectPointTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.SetActionChecked(Sender: TObject);
var
  AnAction: Taction;
begin
  // toggle the checked state of an action or the action
  // associated with a control.
  AnAction := nil;
  if Sender is TAction then
  begin
    AnAction := TAction(Sender);
  end
  else if Sender is TMenuItem then
  begin
    AnAction := TMenuItem(Sender).Action as TAction;
  end
  else if Sender is TToolButton then
  begin
    AnAction := TToolButton(Sender).Action as TAction;
  end;
  if AnAction <> nil then
  begin
    AnAction.Checked := not AnAction.Checked;
  end;
end;

procedure TfrmGoPhast.tbInsertPointClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbInsertPoint.OnMouseDown(tbInsertPoint, mbLeft, [ssLeft], 0, 0);
  end;

  if tbInsertPoint.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbInsertPoint);
    // Set the cursors.
    SetZB_Cursors(crDisabledInsertPoint);
    CurrentTool := InsertPointTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.tbDeleteSegmentClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbDeleteSegment.OnMouseDown(tbDeleteSegment, mbLeft, [ssLeft], 0, 0);
  end;

  if tbDeleteSegment.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbDeleteSegment);
    // Set the cursors.
    SetZB_Cursors(crDisabledDeleteSegment);
    CurrentTool := DeleteSegmentTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.miRearrangeObjectsClick(Sender: TObject);
begin
  // Allow the user to change the order of objects.
  ShowAForm(TfrmRearrangeObjects);
end;

procedure TfrmGoPhast.miReverseSelectedObjectsClick(Sender: TObject);
begin
  inherited;
  UndoStack.Submit(TUndoReverseVerticies.Create);
end;

procedure TfrmGoPhast.tbPointMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ToolButton: TToolButton;
begin
  if Sender is TToolButton then
  begin
    ToolButton := TToolButton(Sender);
    if ToolButton.Down then
    begin
      // Make sure all buttons except the current one are up.
      SetButtonsUp(ToolButton);
    end;
  end;
end;

procedure TfrmGoPhast.tbGridAngleClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbGridAngle.OnMouseDown(tbGridAngle, mbLeft, [ssLeft], 0, 0);
  end;

  // Make sure all buttons except the current one are up.
  SetButtonsUp(tbGridAngle);

  // Set the cursors.
  SetZB_Cursors(crArrow);
  if frameTopView <> nil then
  begin
    frameTopView.ZoomBox.Cursor := crRotate;
    frameTopView.ZoomBox.Image32.Cursor := crRotate;
  end;

  CurrentTool := RotateGridTool;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.miSetSelectedColRowLayerClick(Sender: TObject);
begin
  ShowAForm(TfrmSelectColRowLayer);
end;

function TfrmGoPhast.DefaultVE: Real;
var
  LocalGrid: TCustomModelGrid;
  ZHeight: Real;
  XWidth: Real;
  FrameRatio: Real;
  E: Integer;
  D: Integer;
//  Mesh: TSutraMesh3D;
  Limits: TGridLimit;
  Mesh: IMesh3D;
  DrawMesh: IDrawMesh;
  procedure GetExag;
  begin
    result := XWidth / ZHeight;
    if (Mesh <> nil) and (Mesh is TSutraMesh3D)
      and (TSutraMesh3D(Mesh).MeshType = mtProfile) then
    begin
      if ((frameTopView.ZoomBox.Height > 10)
        and (frameTopView.ZoomBox.Width > 10)) then
      begin
        FrameRatio := frameTopView.ZoomBox.Width
          / frameTopView.ZoomBox.Height;
        result := result/FrameRatio;
      end
      else
      begin
        result := result/3;
      end;
    end
    else
    begin
      if ((frameFrontView.ZoomBox.Height > 10)
        and (frameFrontView.ZoomBox.Width > 10)) then
      begin
        FrameRatio := frameFrontView.ZoomBox.Width
          / frameFrontView.ZoomBox.Height;
        result := result/FrameRatio;
      end
      else
      begin
        result := result/3;
      end;
    end;
    E := Floor(Log10(result));
    D := ceil(result*Power(10, -E));
    result := D*Power(10, E);
  end;
begin
  result := 20;
  LocalGrid := Grid;
  Mesh := nil;
  if (LocalGrid <> nil)  then
  begin
    if (LocalGrid.LayerCount >= 1)
      and (LocalGrid.RowCount >= 1) and (LocalGrid.ColumnCount >= 1) then
    begin
      ZHeight := LocalGrid.HighestElevation - LocalGrid.LowestElevation;
      if ZHeight > 0 then
      begin
        XWidth := LocalGrid.ColumnPosition[LocalGrid.ColumnCount] - LocalGrid.ColumnPosition[0];
        XWidth := Abs(XWidth);
        if XWidth > 0 then
        begin
          GetExag;
        end;
      end;
    end;
  end
  else
  begin
    Mesh := PhastModel.Mesh3D;
    if Mesh <> nil then
    begin
      DrawMesh := PhastModel.DrawMesh;
      if Mesh.Is3DMesh then
      begin
        Limits := Mesh.MeshLimits(vdFront, DrawMesh.CrossSection.Angle);
        ZHeight := Limits.MaxZ - Limits.MinZ;
        if ZHeight > 0 then
        begin
          XWidth := Limits.MaxX - Limits.MinX;
          if XWidth > 0 then
          begin
            GetExag;
          end;
        end;
      end
      else
      begin
        Assert(Mesh is TSutraMesh3D);
        if TSutraMesh3D(Mesh).MeshType = mtProfile then
        begin
          Limits := Mesh.MeshLimits(vdTop, DrawMesh.CrossSection.Angle);
          ZHeight := Limits.MaxY - Limits.MinY;
          if ZHeight > 0 then
          begin
            XWidth := Limits.MaxX - Limits.MinX;
            if XWidth > 0 then
            begin
              GetExag;
            end;
          end;
        end
        else
        begin
          result := 1;
        end;
      end;
    end;
  end;
end;

procedure TfrmGoPhast.miDeleteImageClick(Sender: TObject);
begin
  inherited;
  if PhastModel.Bitmaps.Count  > 0 then
  begin
    if PhastModel.Bitmaps.Count = 1 then
    begin
      PhastModel.Bitmaps.Delete(0);
    end
    else
    begin
       ShowAForm(TfrmDeleteImage);
    end;
    InvalidateViewOfModel;
    ReDrawAllViews(nil);
    PhastModel.Invalidate(self);
  end;
  EnableDeleteImage;
end;

procedure TfrmGoPhast.DeleteLastPointInRuler;
begin
  if CurrentTool = RulerTool then
  begin
    RulerTool.DeleteLastSavedPoint;
  end;
end;

procedure TfrmGoPhast.DeleteLastPointInScreenObject;
var
  AScreenObject: TScreenObject;
begin
  AScreenObject := frameTopView.CurrentScreenObject;
  if AScreenObject = nil then
  begin
    AScreenObject := frameFrontView.CurrentScreenObject;
  end;
  if AScreenObject = nil then
  begin
    AScreenObject := frameSideView.CurrentScreenObject;
  end;
  if (AScreenObject <> nil) and (AScreenObject.Count > 0) then
  begin
    AScreenObject.Count := AScreenObject.Count -1;
    if AScreenObject.Count = 0 then
    begin
      (CurrentTool as TCustomCreateScreenObjectTool).RemoveScreenObject;
      (CurrentTool as TCustomCreateScreenObjectTool).CurrentScreenObject := nil;
//      TCustomCreateScreenObjectTool(CurrentTool).CurrentScreenObject := nil;
//      Assert(FCurrentUndo is TUndoCreateScreenObject);
//      frmGoPhast.PhastModel.RemoveScreenObject(AScreenObject);
//      FreeAndNil(FCurrentUndo);
    end;
  end
  else if AScreenObject = nil then
  begin
    DeleteSelectedNodesOrSelectedScreenObjects;
  end;
end;

procedure TfrmGoPhast.DeleteModelResults1Click(Sender: TObject);
begin
  inherited;
  UndoStack.Submit(TUndoDeleteModelResults.Create);
end;

procedure TfrmGoPhast.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
const
  Key_Escape = VK_ESCAPE; // = 27
  Key_Return = VK_RETURN; // = 13
  Key_Delete = VK_DELETE; // = 46
  Key_Left = VK_LEFT;     // = 37
  Key_Up = VK_UP;         //= 38
  Key_Right = VK_RIGHT;   // = 39
  Key_Down = VK_DOWN;     // = 40
  Key_PageUp = VK_PRIOR;  // = 33
  Key_PageDown = VK_NEXT; // = 34
var
  BigJump: boolean;
  ReverseFactor: integer;
begin
  if csDestroying in ComponentState then
  begin
    Exit;
  end;

  // This only works because frmGoPhast.KeyPreview is true.
  BigJump := ssShift in Shift;
  ReverseFactor := 1;
  case Key of
    Key_Left, Key_Right:
      begin
        if Grid <> nil then
        begin
          case Grid.ColumnDirection of
            cdWestToEast: ReverseFactor := 1;
            cdEastToWest: ReverseFactor := -1;
            else Assert(False);
          end;
        end;
      end;
    Key_Up, Key_Down:
      begin
        if Grid <> nil then
        begin
          case Grid.RowDirection of
            rdSouthToNorth: ReverseFactor := 1;
            rdNorthToSouth: ReverseFactor := -1;
            else Assert(False);
          end;
        end;
      end;
    Key_PageUp, Key_PageDown:
      begin
        if Grid <> nil then
        begin
          case Grid.LayerDirection of
            ldBottomToTop: ReverseFactor := 1;
            ldTopToBottom: ReverseFactor := -1;
            else Assert(False);
          end;
        end
        else
        begin
            ReverseFactor := -1;
        end;
      end;
    // else ignore it.
  end;
  case Key of
    Key_Escape:
      begin
        if CurrentTool = RulerTool then
        begin
          DeleteLastPointInRuler;
        end
        else if CurrentTool = FishnetTool then
        begin
          FishnetTool.DeleteLastNode;
        end
        else if CurrentTool = DrawElementTool then
        begin
          DrawElementTool.DeleteLastNode;
        end
        else if CurrentTool is TCustomCreateScreenObjectTool then
        begin
          DeleteLastPointInScreenObject;
        end;
      end;
    Key_Return:
      begin
        if CurrentTool is TAddLinePartTool then
        begin
          TAddLinePartTool(CurrentTool).FinishSection;
        end
        else
        begin
          frameTopView.FinishScreenObjects;
          frameFrontView.FinishScreenObjects;
          frameSideView.FinishScreenObjects;
        end;
      end;
    Key_Delete:
      begin
        if CurrentTool is TCustomCreateScreenObjectTool then
        begin
          DeleteLastPointInScreenObject;
        end
        else if CurrentTool = FishnetTool then
        begin
          FishnetTool.DeleteSelectedElement;
        end
        else if CurrentTool = MoveSutraNodesTool then
        begin
          MoveSutraNodesTool.DeleteSelectedNodesAndElements;
        end
        else
        begin
          DeleteSelectedNodesOrSelectedScreenObjects;
        end;
      end;
    Key_Left:
      begin
        if BigJump then
        begin
          PhastModel.SelectedColumn := PhastModel.SelectedColumn - ReverseFactor*10;
        end
        else
        begin
          PhastModel.SelectedColumn := PhastModel.SelectedColumn - ReverseFactor*1;
        end;
        frameSideView.DisplayItem;
      end;
    Key_Up:
      begin
        if BigJump then
        begin
          PhastModel.SelectedRow := PhastModel.SelectedRow + ReverseFactor*10;
        end
        else
        begin
          PhastModel.SelectedRow := PhastModel.SelectedRow + ReverseFactor*1;
        end;
        frameFrontView.DisplayItem;
      end;
    Key_Right:
      begin
        if BigJump then
        begin
          PhastModel.SelectedColumn := PhastModel.SelectedColumn + ReverseFactor*10;
        end
        else
        begin
          PhastModel.SelectedColumn := PhastModel.SelectedColumn + ReverseFactor*1;
        end;
        frameSideView.DisplayItem;
      end;
    Key_Down:
      begin
        if BigJump then
        begin
          PhastModel.SelectedRow := PhastModel.SelectedRow - ReverseFactor*10;
        end
        else
        begin
          PhastModel.SelectedRow := PhastModel.SelectedRow - ReverseFactor*1;
        end;
        frameFrontView.DisplayItem;
      end;
    Key_PageUp:
      begin
        if BigJump then
        begin
          PhastModel.SelectedLayer := PhastModel.SelectedLayer + ReverseFactor*10;
        end
        else
        begin
          PhastModel.SelectedLayer := PhastModel.SelectedLayer + ReverseFactor*1;
        end;
        frameTopView.DisplayItem;
      end;
    Key_PageDown:
      begin
        if BigJump then
        begin
          PhastModel.SelectedLayer := PhastModel.SelectedLayer - ReverseFactor*10;
        end
        else
        begin
          PhastModel.SelectedLayer := PhastModel.SelectedLayer - ReverseFactor*1;
        end;
        frameTopView.DisplayItem;
      end;
    else
    begin
//      Assert(PatchedVCLX = 3.9);
      // This shouldn't be required - CLX should take care of this.
      // This was broken with PatchedVCLX = 3.91.
      // If it is fixed, the following code and the
      // above assertion should be removed.

      // Respond to keyboard shortcuts.
      {for ControlIndex := 0 to ComponentCount -1 do
      begin
        if Components[ControlIndex] is TMenuItem then
        begin
          Item := TMenuItem(Components[ControlIndex]);
          ShortcutToKey(Item.ShortCut, MenuShortCutKey, MenuShortCutShift);
          if (MenuShortCutKey = Key)
            and (MenuShortCutShift = Shift)
            and Item.Enabled then
          begin
            Item.OnClick(Item);
            break;
          end;
        end;
      end;}
    end
  end;
end;

procedure TfrmGoPhast.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
const
  TenthOfASecond = 1/24/3600/10;
var
  ctrl : TWinControl;
  APoint: TPoint;
  Increment: Integer;
begin
  inherited;
  ctrl := FindVCLWindow(MousePos);
  if ctrl <> nil then
  begin
    if (ctrl.Parent is TQRbwZoomBox2) then
    begin
      APoint := ctrl.ScreenToClient(MousePos);
      Handled := True;
      if WheelDelta > 0 then
      begin
        ZoomInTool.MouseUp(Sender, mbLeft, Shift, APoint.X, APoint.Y);
      end
      else
      begin
        ZoomOutTool.MouseUp(Sender, mbLeft, Shift, APoint.X, APoint.Y);
      end;
      FishnetTool.HideEdits;
    end
    else
    begin
      if (FCubeControl <> nil) then
      begin
        if Now - FLastMoveTime < TenthOfASecond then
        begin
          Exit;
        end;
        FLastMoveTime := Now;
        if ssShift in Shift then
        begin
          Increment := 10;
        end
        else
        begin
          Increment := 1;
        end;
        case FCubeControl.SelectedFace of
          faTop:
            begin
              PhastModel.CombinedDisplayLayer :=
                PhastModel.CombinedDisplayLayer
                - Sign(WheelDelta)*Increment;
              sbMain.Panels[1].Text := Format(StrSelectedLayerD,
                [PhastModel.SelectedLayer + 1]);
            end;
          faFront:
            begin
              if ModelSelection  in SutraSelection then
              begin
                if Assigned(FCubeControl.OnMouseUp) then
                begin
                  if Sign(WheelDelta) > 0 then
                  begin
                    FCubeControl.OnMouseUp(FCubeControl, mbLeft, Shift,
                      FCubeControl.Height * 4 div 5, //44,
                      FCubeControl.Width div 5); // 10
                  end
                  else
                  begin
                    FCubeControl.OnMouseUp(FCubeControl, mbLeft, Shift,
                      FCubeControl.Height div 5, // 20,
                      FCubeControl.Width * 4 div 5); // 30
                  end;
                end;

              end
              else
              begin
                PhastModel.CombinedDisplayRow :=
                  PhastModel.CombinedDisplayRow
                  - Sign(WheelDelta)*Increment;
                sbMain.Panels[1].Text := Format(StrSelectedRowD,
                  [PhastModel.SelectedRow + 1]);
              end;
            end;
          faSide:
            begin
              PhastModel.CombinedDisplayColumn :=
                PhastModel.CombinedDisplayColumn
                + Sign(WheelDelta)*Increment;
              sbMain.Panels[1].Text := Format(StrSelectedColD,
                [PhastModel.SelectedColumn + 1]);
            end;
          else Assert(False);
        end;
      end;
    end;
  end

end;

procedure TfrmGoPhast.acSelectAllFrontExecute(Sender: TObject);
begin
  inherited;
  frameFrontView.SelectAll;
end;

procedure TfrmGoPhast.acSelectAllSideExecute(Sender: TObject);
begin
  inherited;
  frameSideView.SelectAll;
end;

procedure TfrmGoPhast.acSelectAllTopExecute(Sender: TObject);
begin
  inherited;
  frameTopView.SelectAll;
end;

procedure TfrmGoPhast.acSelectColRowLayExecute(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbSelectColRowLayer.OnMouseDown(tbSelectColRowLayer, mbLeft, [ssLeft], 0, 0);
  end;

  if tbSelectColRowLayer.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbSelectColRowLayer);

    // Set the cursors.
    SetZB_Cursors(crArrow);
  end
  else
  begin
    // Reset the cursors.
    SetZB_Cursors(crArrow);
  end;
  CurrentTool := ColRowLayerSelectorTool;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.acSetSpacingExecute(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbSpacing.OnMouseDown(tbSpacing, mbLeft, [ssLeft], 0, 0);
  end;

  if tbSpacing.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbSpacing);

    // Set the cursors.
    SetZB_Cursors(crSetWidth);
    if frameTopView <> nil then
    begin
      frameTopView.ZoomBox.Cursor := dcSetSpacing.Cursor;
      frameTopView.ZoomBox.Image32.Cursor := dcSetSpacing.Cursor;
    end;
  end
  else
  begin
    // Reset the cursors.
    SetZB_Cursors(crArrow);
  end;
  CurrentTool := SpacingGridTool;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.dcSetSpacingDrawCursor(Sender: TObject;
  const ABitMap, AMask: TBitmap);
begin
  // Draw the bitmaps for the SetSpacing cursor.
  if Grid <> nil then
  begin
    DrawSubdivideCursor(ABitMap, Grid.GridAngle, dcSetSpacing);
    DrawSubdivideCursor(ABitMap, Grid.GridAngle + Pi / 2, dcSetSpacing);
    AMask.Canvas.Pen.Width := 3;
    DrawSubdivideCursor(AMask, Grid.GridAngle, dcSetSpacing);
    DrawSubdivideCursor(AMask, Grid.GridAngle + Pi / 2, dcSetSpacing);
  end;
end;

procedure TfrmGoPhast.SetFrontDiscretizationChanged(const Value: boolean);
begin
  FFrontDiscretizationChanged := Value;
end;

procedure TfrmGoPhast.SetFrontScreenObjectsChanged(const Value: boolean);
begin
  FFrontScreenObjectsChanged := Value;
  if FFrontScreenObjectsChanged  and not (csDestroying in ComponentState) then
  begin
    frameFrontView.ZoomBox.InvalidateImage32;
  end;
end;

procedure TfrmGoPhast.SetModelSelection(const Value: TModelSelection);
begin
  if PhastModel.ModelSelection <> Value then
  begin
    PhastModel.ModelSelection := Value;
    case Value of
      msUndefined: Assert(False);
      msPhast, msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
        msModflowFmp, msModflowCfp, msSutra22, msSutra30, msFootPrint, msModflow2015:
        begin
          InvalidateViewOfModel;
          InvalidateAllViews;
        end;
      else Assert(False);
    end;
  end;
  case Value of
    msUndefined: Assert(False);
    msPhast:
      begin
        acPhastActive.Checked := True;
      end;
    msModflow:
      begin
        acModflowActive.Checked := True;
      end;
    msModflowLGR:
      begin
        acModflowLgrActive.Checked := True;
      end;
    msModflowLGR2:
      begin
        acModflowLgr2Active.Checked := True;
      end;
    msModflowNWT:
      begin
        acModflowNwtActive.Checked := True;
      end;
    msModflowFmp:
      begin
        acModflowFmpActive.Checked := True;
      end;
    msModflowCfp:
        acModflowCfpActive.Checked := True;
    msSutra22:
      begin
        acSutra22Active.Checked := True;
        if SutraMesh <> nil then
        begin
          SutraMesh.OnSelectedLayerChange := frameTopView.ItemChange;
        end;
      end;
    msSutra30:
      begin
        acSutra30Active.Checked := True;
        if SutraMesh <> nil then
        begin
          SutraMesh.OnSelectedLayerChange := frameTopView.ItemChange;
        end;
      end;
    msFootPrint:
      begin
        acFootPrintActive.Checked := True;
      end;
    msModflow2015:
      begin
        acModflow6Active.Checked := True;
      end;
    else Assert(False);
  end;
end;

procedure TfrmGoPhast.SetMt3dCaption;
begin
  if PhastModel.ModflowPackages.Mt3dBasic.Mt3dVersion = mvUSGS then
  begin
    acRunMt3dms.Caption := StrMT3DUSGSInputFile;
    acRunMt3dms.Hint := StrRunMT3DUSGS;
    miRunMt3dmsPopup.Caption := StrExportMT3DUSGSInp;
  end
  else
  begin
    Assert(PhastModel.ModflowPackages.Mt3dBasic.Mt3dVersion = mvMS);
    acRunMt3dms.Caption := StrMT3DMSInputFiles;
    acRunMt3dms.Hint := StrRunMT3DMS;
    miRunMt3dmsPopup.Caption := StrExportMT3DMSInput;
  end;
end;

procedure TfrmGoPhast.SetRulersVisible(const Value: Boolean);
begin
  FRulersVisible := Value;
  frameTopView.pnlTop.Visible := FRulersVisible;
  frameTopView.rulVertical.Visible := FRulersVisible;
  frameFrontView.pnlTop.Visible := FRulersVisible;
  frameFrontView.rulVertical.Visible := FRulersVisible;
  framesideView.pnlTop.Visible := FRulersVisible;
  framesideView.rulVertical.Visible := FRulersVisible;
end;

procedure TfrmGoPhast.miEditGlobalVariablesClick(Sender: TObject);
begin
  inherited;
  if frmDataSets <> nil then
  begin
    Beep;
    MessageDlg(StrYouMustCloseTheD, mtWarning, [mbOK], 0);
    Exit;
  end;
  if frmGlobalVariables = nil then
  begin
    frmGlobalVariables := TfrmGlobalVariables.Create(nil);
  end;
  frmGlobalVariables.Show;
end;

procedure TfrmGoPhast.EditScreenObjects;
var
  AList: TList;
  Index: Integer;
  AScreenObject: TScreenObject;
begin
  // This procedure allows the user to edit the properties of the selected
  // screen objects.
  if not CanEdit then Exit;
  CanEdit := False;
  try
    AList := TList.Create;
    try
      for Index := 0 to PhastModel.ScreenObjectCount - 1 do
      begin
        AScreenObject := PhastModel.ScreenObjects[Index];
        if AScreenObject.Selected then
        begin
          AList.Add(AScreenObject);
        end;
      end;
      if AList.Count > 0 then
      begin
        Assert(frmScreenObjectProperties <> nil);

        frmScreenObjectProperties.GetDataForMultipleScreenObjects(AList);
        frmScreenObjectProperties.ShowModal;
        BringToFront;
        BringFormsToFront(nil);
      end;
    finally
      AList.Free;
    end;
  finally
    CanEdit := True;
  end;
end;

procedure TfrmGoPhast.EnableInvertSelection;
var
  Index: Integer;
  ScreenObject: TScreenObject;
begin
  miInvertSelection.Enabled := False;
  for Index := 0 to PhastModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := PhastModel.ScreenObjects[Index];
    if ScreenObject.Selected then
    begin
      miInvertSelection.Enabled := True;
      break;
    end;
  end;
  ScreenObjectSelectionChange(nil);
end;

procedure TfrmGoPhast.UpdateDisplay(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
  begin
    TopScreenObjectsChanged := True;
    FrontScreenObjectsChanged := True;
    SideScreenObjectsChanged := True;
    Invalidate3DView(nil);
  end;
end;

procedure TfrmGoPhast.UpdateFrontCubeForMeshCrossSection(Sender: TObject);
var
//  Mesh: TSutraMesh3D;
  CrossSection: TMeshCrossSectionLine;
  Outline: TPolygon2D;
  APoint: TPoint2D;
  PointIndex: Integer;
  MinY: TFloat;
  MaxY: TFloat;
  Fraction: double;
  Mesh: IMesh3D;
  DrawMesh: IDrawMesh;
begin
  if (not (ModelSelection  in SutraSelection)) and (not DisvUsed) then
  begin
    Exit;
  end;
  Mesh := PhastModel.SelectedModel.Mesh3D;
  DrawMesh := PhastModel.SelectedModel.DrawMesh;

  if (not Mesh.Is3DMesh) or (Mesh.Mesh2DI.ElementCount = 0) then
  begin
    Exit;
  end;
  CrossSection := DrawMesh.CrossSection;
  Outline := Mesh.Mesh2DI.MeshOutline;
  APoint := CrossSection.StartPoint;
  if CrossSection.Angle <> 0 then
  begin
    APoint := DrawMesh.RotateFromRealWorldCoordinatesToMeshCoordinates(APoint);
    for PointIndex := 0 to Length(Outline) - 1 do
    begin
      Outline[PointIndex] :=
        DrawMesh.RotateFromRealWorldCoordinatesToMeshCoordinates(Outline[PointIndex]);
    end;
  end;
  MinY := Outline[0].y;
  MaxY := MinY;
  for PointIndex := 1 to Length(Outline) - 1 do
  begin
    if MinY > Outline[PointIndex].y then
    begin
      MinY := Outline[PointIndex].y;
    end
    else if MaxY < Outline[PointIndex].y then
    begin
      MaxY := Outline[PointIndex].y;
    end;
  end;
  if MinY = MaxY then
  begin
    Fraction := 0.5;
  end
  else
  begin
    Fraction := (APoint.y - MinY)/(MaxY-MinY);
    if Fraction < 0 then
    begin
      Fraction := 0;
    end
    else if Fraction > 1 then
    begin
      Fraction := 1;
    end;
  end;
//  Fraction := 1-Fraction;
  if ModelSelection  in SutraSelection then
  begin
    frameFrontView.ModelCube.Selection1 := Fraction;
    frameFrontView.ModelCube.Selection2 := Fraction;
  end
  else
  begin
    frameFrontView.ModelCube.Selection1 := 1-Fraction;
    frameFrontView.ModelCube.Selection2 := 1-Fraction;
  end;
end;

procedure TfrmGoPhast.SetSideDiscretizationChanged(const Value: boolean);
begin
  FSideDiscretizationChanged := Value;
end;

procedure TfrmGoPhast.SetSideScreenObjectsChanged(const Value: boolean);
begin
  FSideScreenObjectsChanged := Value;
  if FSideScreenObjectsChanged  and not (csDestroying in ComponentState) then
  begin
    frameSideView.ZoomBox.InvalidateImage32;
  end;
end;

procedure TfrmGoPhast.acEditCTSExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmContaminantTreatmentSystems);
end;

procedure TfrmGoPhast.acEditDataSetsExecute(Sender: TObject);
begin
  if frmGlobalVariables <> nil then
  begin
    Beep;
    MessageDlg(StrYouMustCloseTheG, mtWarning, [mbOK], 0);
    Exit;
  end;
  if frmDataSets = nil then
  begin
    frmDataSets := TfrmDataSets.Create(nil);
  end;
  frmDataSets.Show;
end;

procedure TfrmGoPhast.acEditFarmsExecute(Sender: TObject);
begin
  inherited;
  if PhastModel.FmpCrops.Count > 0 then
  begin
    ShowAForm(TfrmFarm);
  end
  else
  begin
    Beep;
    MessageDlg(StrYouMustDefineAtL, mtError, [mbOK], 0);
  end;
end;

procedure TfrmGoPhast.UpdateDataSetDimensions;
begin
  if PhastModel <> nil then
  begin
    PhastModel.UpdateDataSetDimensions;
  end;
  InvalidateImage32AllViews;
end;

procedure TfrmGoPhast.UpdateModelCubeBreaks;
var
  Breaks: TBreakCollection;
  LayerCount: Integer;
  LayerIndex: Integer;
  ABreak: TBreakPosition;
begin
  Breaks := frameTopView.ModelCube.Breaks;
  Breaks.Clear;
  if (ModelSelection in ModflowSelection)
    and (PhastModel.LayerStructure.Count > 0) then
  begin
    LayerCount := PhastModel.CombinedLayerCount;
    if PhastModel.LgrUsed then
    begin
      for LayerIndex := 0 to LayerCount - 1 do
      begin
        if not PhastModel.CombinedLayerSimulated(LayerIndex) then
        begin
          ABreak := Breaks.Add;
          ABreak.LowerFraction := (LayerCount - LayerIndex -1)/LayerCount;
          ABreak.HigherFraction := (LayerCount - LayerIndex)/LayerCount;
        end;
      end;
    end;
  end;
end;

procedure TfrmGoPhast.AdjustSutraBoundaries;
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  ABoundary: TSutraBoundary;
  BoundValues: TCustomSutraBoundaryCollection;
  ASchedule: TSutraTimeSchedule;
  procedure AdjustABoundary;
  begin
    if ABoundary.Used then
    begin
      BoundValues := ABoundary.Values as TCustomSutraBoundaryCollection;
      if BoundValues.ScheduleName <> '' then
      begin
        ASchedule := PhastModel.SutraTimeOptions.Schedules.GetScheduleByName(
          string(BoundValues.ScheduleName));
        if ASchedule <> nil then
        begin
          AdjustBoundaryValues(ASchedule, BoundValues);
        end;
      end;
    end;
  end;
begin
  for ScreenObjectIndex := 0 to PhastModel.ScreenObjectCount   - 1 do
  begin
    AScreenObject := PhastModel.ScreenObjects[ScreenObjectIndex];
    ABoundary := AScreenObject.SutraBoundaries.FluidSource;
    AdjustABoundary;
    ABoundary := AScreenObject.SutraBoundaries.MassEnergySource;
    AdjustABoundary;
    ABoundary := AScreenObject.SutraBoundaries.SpecifiedPressure;
    AdjustABoundary;
    ABoundary := AScreenObject.SutraBoundaries.SpecifiedConcTemp;
    AdjustABoundary;
  end;
end;

procedure TfrmGoPhast.ExportSutra(ShouldRunSutra: Boolean;
  const FileName: string);
var
  IcsWriter: TSutraInitialConditionsWriter;
  FluidSourceNodes: IBoundaryNodes;
  ObsWriter: TSutraObservationWriter;
  BatchFile: TStringList;
  SpecifiedPressureNodes: IBoundaryNodes;
  NOBS: Integer;
  MassEnergySourceNodes: IBoundaryNodes;
  TimeSchWriter: TSutraTimeScheduleWriter;
  BatchFileName: string;
  BoundaryWriter: TSutraBoundaryWriter;
  InputWriter: TSutraInputWriter;
  SpecifiedTempConcNodes: IBoundaryNodes;
  Schedules: TStringList;
  Observations: TStringList;
  LakeWriter: TSutraLakeWriter;
  GeoRefWriter: TGeoRefWriter;
  GeneralFlowNodes: TList<IGeneralFlowNodes>;
  GenFlowNodeLists: TObjectList<TList<IGeneralFlowNodes>>;
  GenFlowWriter: TSutraGeneralFlowWriter;
  GeneralTransportNodes: TList<IGeneralTransportNodes>;
  GeneralTransportList: TObjectList<TList<IGeneralTransportNodes>>;
  GenTransWriter: TSutraGeneralTransportWriter;
  ModelName: string;
  NetworkDrive: Boolean;
  ModelDirectory: string;
  HasLakes: Boolean;
  SutraFileName: string;
  BcsFileNames: TLakeInteractionStringLists;
  FluidSourceBcsNames: TLakeInteractionStringList;
  MassEnergyBcsNames: TLakeInteractionStringList;
  SpecifiedPressureBcsNames: TLakeInteractionStringList;
  SpecifiedTempConcBcsNames: TLakeInteractionStringList;
  LakeInteraction: TLakeBoundaryInteraction;
  GenFlowInteractionBcsNames: TGenFlowInteractionStringList;
  GenLakeInteractionType: TGeneralizedFlowInteractionType;
  GenTransportInteractionBcsNames: TGenTransportInteractionStringList;
  GenLakeTransInteractionType: TGeneralizedTransportInteractionType;
  SutraPestObsWriterWriter: TSutraPestObsWriterWriter;
  SutraNodDisWriter: TSutraNodDisWriter;
  SutraEleDisWriter: TSutraEleDisWriter;
  ParamEstBatFile: TStringList;
  ParamEstBatFileName: string;
  PIndex: Integer;
  AParam: TModflowSteadyParameter;
  SutraNod3DDisWriter: TSutraNod3DDisWriter;
  ParamEstBatchFile: TStringList;
  PLPROC_Location: string;
  DSIndex: Integer;
  ADataArray: TDataArray;
  INFLE: string;
  PestInputDataArrays: TDictionary<string, TDataArray>;
  PestDataArray: TDataArray;
  FileRoot: string;
  BackupParamEstBatFileName: string;
  BackupBatchFileName: string;
  procedure AddPestDataArraysToDictionary(InputPestDataArrays: TArray<TDataArray>);
  var
    DataArray: TDataArray;
  begin
    for DataArray in InputPestDataArrays do
    begin
      if not PestInputDataArrays.ContainsKey(UpperCase(DataArray.Name)) then
      begin
        PestInputDataArrays.Add(UpperCase(DataArray.Name), DataArray);
      end;
    end;
  end;

begin
  case ModelSelection of
    msSutra22:
      begin
        SutraFileName := PhastModel.ProgramLocations.Sutra22Location;
      end;
    msSutra30:
      begin
        SutraFileName := PhastModel.ProgramLocations.Sutra30Location;
      end;
    else
      Assert(False);
  end;

  if not TFile.Exists(SutraFileName) then
  begin
    Beep;
    acSutraProgramLocationsExecute(nil);
    case ModelSelection of
      msSutra22:
        begin
          SutraFileName := PhastModel.ProgramLocations.Sutra22Location;
        end;
      msSutra30:
        begin
          SutraFileName := PhastModel.ProgramLocations.Sutra30Location;
        end;
      else
        Assert(False);
    end;
    if not TFile.Exists(SutraFileName) then
    begin
      if MessageDlg(Format(StrSDoesNotExistSutra, [SutraFileName]), mtError,
        [mbYes, mbNo], 0) <> mrYes then
      begin
        Exit;
      end
      else
      begin
        ShouldRunSutra := False;
      end;
    end;
  end;

  PhastModel.ClearPval;
  PhastModel.PestTemplateLines.Clear;
  PhastModel.SutraPestScripts.Clear;
  PhastModel.KrigfactorsScriptLines.Clear;
  PhastModel.PilotPointData.Clear;
  PhastModel.ClearPestArrayFileNames;

  SutraNodDisWriter := TSutraNodDisWriter.Create(PhastModel, etExport);
  try
    SutraNodDisWriter.WriteFile(FileName);
  finally
    SutraNodDisWriter.Free;
  end;

  SutraNod3DDisWriter := TSutraNod3DDisWriter.Create(PhastModel, etExport);
  try
    SutraNod3DDisWriter.WriteFile(FileName);
  finally
    SutraNod3DDisWriter.Free;
  end;

  SutraEleDisWriter := TSutraEleDisWriter.Create(PhastModel, etExport);
  try
    SutraEleDisWriter.WriteFile(FileName);
  finally
    SutraEleDisWriter.Free;
  end;

  if frmProgressMM = nil then
  begin
    frmProgressMM := TfrmProgressMM.Create(nil);
  end;
  NetworkDrive := IsNetworkDrive(FileName);
  ModelDirectory := ExtractFileDir(FileName);
  PhastModel.ClearModelFiles;
  frmProgressMM.ShouldContinue := True;
  frmErrorsAndWarnings.RemoveWarningGroup(PhastModel, StrTheFollowingObjectNoCells);
  PestInputDataArrays := TDictionary<string, TDataArray>.Create;
  try
    try
      AdjustSutraBoundaries;

      SutraFileWriter := TSutraFileWriter.Create(PhastModel, FileName);
      FluidSourceNodes := TBoundaryNodes.Create;
      MassEnergySourceNodes := TBoundaryNodes.Create;
      SpecifiedPressureNodes := TBoundaryNodes.Create;
      SpecifiedTempConcNodes := TBoundaryNodes.Create;
      GenFlowNodeLists := TObjectList<TList<IGeneralFlowNodes>>.Create;
      GeneralTransportList := TObjectList<TList<IGeneralTransportNodes>>.Create;
      Schedules := TStringList.Create;
      Observations := TStringList.Create;
      BcsFileNames := TLakeInteractionStringLists.Create;
      try
        if not PhastModel.SutraLakesUsed then
        begin
          BcsFileNames.Add(nil);
          BoundaryWriter := TSutraBoundaryWriter.Create(PhastModel, etExport,
            sbtFluidSource);
          try
            BoundaryWriter.WriteFile(FileName, FluidSourceNodes, nil);
            AddPestDataArraysToDictionary(BoundaryWriter.GetUsedPestDataArrays)
//            InputPestDataArrays := BoundaryWriter.GetUsedPestDataArrays;
          finally
            BoundaryWriter.Free;
          end;
        end
        else
        begin
          FluidSourceBcsNames := TLakeInteractionStringList.Create;
          BcsFileNames.Add(FluidSourceBcsNames);
          for LakeInteraction := Low(TLakeBoundaryInteraction) to High(TLakeBoundaryInteraction) do
          begin
            FluidSourceBcsNames.LakeInteraction := LakeInteraction;
            BoundaryWriter := TSutraBoundaryWriter.Create(PhastModel, etExport,
              sbtFluidSource);
            try
              BoundaryWriter.WriteFile(FileName, FluidSourceNodes,
                FluidSourceBcsNames);
              AddPestDataArraysToDictionary(BoundaryWriter.GetUsedPestDataArrays)
            finally
              BoundaryWriter.Free;
            end;
          end;
        end;

        if not PhastModel.SutraLakesUsed then
        begin
          BcsFileNames.Add(nil);
          BoundaryWriter := TSutraBoundaryWriter.Create(PhastModel, etExport,
            sbtMassEnergySource);
          try
            BoundaryWriter.WriteFile(FileName, MassEnergySourceNodes, nil);
            AddPestDataArraysToDictionary(BoundaryWriter.GetUsedPestDataArrays)
          finally
            BoundaryWriter.Free;
          end;
        end
        else
        begin
          MassEnergyBcsNames := TLakeInteractionStringList.Create;
          BcsFileNames.Add(MassEnergyBcsNames);
          for LakeInteraction := Low(TLakeBoundaryInteraction) to High(TLakeBoundaryInteraction) do
          begin
            MassEnergyBcsNames.LakeInteraction := LakeInteraction;
            BoundaryWriter := TSutraBoundaryWriter.Create(PhastModel, etExport,
              sbtMassEnergySource);
            try
              BoundaryWriter.WriteFile(FileName, MassEnergySourceNodes,
                MassEnergyBcsNames);
              AddPestDataArraysToDictionary(BoundaryWriter.GetUsedPestDataArrays)
            finally
              BoundaryWriter.Free;
            end;
          end;
        end;

        if not PhastModel.SutraLakesUsed then
        begin
          BcsFileNames.Add(nil);
          BoundaryWriter := TSutraBoundaryWriter.Create(PhastModel, etExport,
            sbtSpecPress);
          try
            BoundaryWriter.WriteFile(FileName, SpecifiedPressureNodes, nil);
            AddPestDataArraysToDictionary(BoundaryWriter.GetUsedPestDataArrays)
          finally
            BoundaryWriter.Free;
          end;
        end
        else
        begin
          SpecifiedPressureBcsNames := TLakeInteractionStringList.Create;
          BcsFileNames.Add(SpecifiedPressureBcsNames);
          for LakeInteraction := Low(TLakeBoundaryInteraction) to High(TLakeBoundaryInteraction) do
          begin
            SpecifiedPressureBcsNames.LakeInteraction := LakeInteraction;
            BoundaryWriter := TSutraBoundaryWriter.Create(PhastModel, etExport,
              sbtSpecPress);
            try
              BoundaryWriter.WriteFile(FileName, SpecifiedPressureNodes,
                SpecifiedPressureBcsNames);
              AddPestDataArraysToDictionary(BoundaryWriter.GetUsedPestDataArrays)
            finally
              BoundaryWriter.Free;
            end;
          end;
        end;

        if not PhastModel.SutraLakesUsed then
        begin
          BcsFileNames.Add(nil);
          BoundaryWriter := TSutraBoundaryWriter.Create(PhastModel, etExport,
            sbtSpecConcTemp);
          try
            BoundaryWriter.WriteFile(FileName, SpecifiedTempConcNodes, nil);
            AddPestDataArraysToDictionary(BoundaryWriter.GetUsedPestDataArrays)
          finally
            BoundaryWriter.Free;
          end;
        end
        else
        begin
          SpecifiedTempConcBcsNames := TLakeInteractionStringList.Create;
          BcsFileNames.Add(SpecifiedTempConcBcsNames);
          for LakeInteraction := Low(TLakeBoundaryInteraction) to
            High(TLakeBoundaryInteraction) do
          begin
            SpecifiedTempConcBcsNames.LakeInteraction := LakeInteraction;
            BoundaryWriter := TSutraBoundaryWriter.Create(PhastModel, etExport,
              sbtSpecConcTemp);
            try
              BoundaryWriter.WriteFile(FileName, SpecifiedTempConcNodes,
                SpecifiedTempConcBcsNames);
              AddPestDataArraysToDictionary(BoundaryWriter.GetUsedPestDataArrays)
            finally
              BoundaryWriter.Free;
            end;
          end;
        end;

        TimeSchWriter := TSutraTimeScheduleWriter.Create(PhastModel);
        try
          TimeSchWriter.WriteFile(Schedules);
        finally
          TimeSchWriter.Free;
        end;
        // TSutraTimeScheduleWriter sets the export schedule name
        // for the observations so it must be evaluated before the observations.
        ObsWriter := TSutraObservationWriter.Create(PhastModel, etExport);
        try
          ObsWriter.WriteFile(FileName, Observations, NOBS);
        finally
          ObsWriter.Free;
        end;

        if not PhastModel.SutraLakesUsed then
        begin
          BcsFileNames.Add(nil);
          GeneralFlowNodes := TList<IGeneralFlowNodes>.Create;
          GenFlowNodeLists.Add(GeneralFlowNodes);

          GenFlowWriter := TSutraGeneralFlowWriter.Create(PhastModel, etExport);
          try
            GenFlowWriter.WriteFile(FileName, GeneralFlowNodes, nil);
            AddPestDataArraysToDictionary(GenFlowWriter.GetUsedPestDataArrays)
          finally
            GenFlowWriter.Free;
          end;
        end
        else
        begin
          GenFlowInteractionBcsNames := TGenFlowInteractionStringList.Create;
          BcsFileNames.Add(GenFlowInteractionBcsNames);
          for LakeInteraction := Low(TLakeBoundaryInteraction)
            to High(TLakeBoundaryInteraction) do
          begin
            GenFlowInteractionBcsNames.LakeInteraction := LakeInteraction;
            for GenLakeInteractionType := Low(TGeneralizedFlowInteractionType)
              to High(TGeneralizedFlowInteractionType) do
            begin
              GenFlowInteractionBcsNames.FlowInteraction := GenLakeInteractionType;

              GeneralFlowNodes := TList<IGeneralFlowNodes>.Create;
              GenFlowNodeLists.Add(GeneralFlowNodes);
              GenFlowWriter := TSutraGeneralFlowWriter.Create(PhastModel, etExport);
              try
                GenFlowWriter.WriteFile(FileName, GeneralFlowNodes,
                  GenFlowInteractionBcsNames);
                AddPestDataArraysToDictionary(GenFlowWriter.GetUsedPestDataArrays)
              finally
                GenFlowWriter.Free;
              end;
            end;
          end;
        end;

        if not PhastModel.SutraLakesUsed then
        begin
          BcsFileNames.Add(nil);
          GeneralTransportNodes := TList<IGeneralTransportNodes>.Create;
          GeneralTransportList.Add(GeneralTransportNodes);

          GenTransWriter := TSutraGeneralTransportWriter.Create(PhastModel, etExport);
          try
            GenTransWriter.WriteFile(FileName, GeneralTransportNodes, nil);
            AddPestDataArraysToDictionary(GenTransWriter.GetUsedPestDataArrays)
          finally
            GenTransWriter.Free;
          end;
        end
        else
        begin
          GenTransportInteractionBcsNames := TGenTransportInteractionStringList.Create;
          BcsFileNames.Add(GenTransportInteractionBcsNames);
          for LakeInteraction := Low(TLakeBoundaryInteraction)
            to High(TLakeBoundaryInteraction) do
          begin
            GenTransportInteractionBcsNames.LakeInteraction := LakeInteraction;
            for GenLakeTransInteractionType := Low(TGeneralizedTransportInteractionType)
              to High(TGeneralizedTransportInteractionType) do
            begin
              GenTransportInteractionBcsNames.TransportInteraction := GenLakeTransInteractionType;

              GeneralTransportNodes := TList<IGeneralTransportNodes>.Create;
              GeneralTransportList.Add(GeneralTransportNodes);

              GenTransWriter := TSutraGeneralTransportWriter.Create(PhastModel, etExport);
              try
                GenTransWriter.WriteFile(FileName, GeneralTransportNodes, GenTransportInteractionBcsNames);
                AddPestDataArraysToDictionary(GenTransWriter.GetUsedPestDataArrays)
              finally
                GenTransWriter.Free;
              end;
            end;
          end;
        end;

        LakeWriter := TSutraLakeWriter.Create(PhastModel, etExport);
        try
          LakeWriter.WriteFile(FileName, BcsFileNames);
          HasLakes := LakeWriter.HasLakes;
        finally
          LakeWriter.Free;
        end;

        InputWriter := TSutraInputWriter.Create(PhastModel);
        try
          InputWriter.HasLakes := HasLakes;

          for PestDataArray in PestInputDataArrays.Values do
          begin
            InputWriter.AddUsedPestDataArray(PestDataArray)
          end;

          InputWriter.WriteFile(FileName, FluidSourceNodes,
            MassEnergySourceNodes, SpecifiedPressureNodes,
            SpecifiedTempConcNodes, NOBS, Schedules, Observations,
            GenFlowNodeLists, GeneralTransportList);
        finally
          InputWriter.Free;
        end;

        IcsWriter := TSutraInitialConditionsWriter.Create(PhastModel);
        try
          IcsWriter.WriteFile(FileName);
        finally
          IcsWriter.Free;
        end;

        SutraPestObsWriterWriter := TSutraPestObsWriterWriter.Create(PhastModel);
        try
          SutraPestObsWriterWriter.WriteFile(FileName);
        finally
          SutraPestObsWriterWriter.Free;
        end;

        SutraFileWriter.WriteFile;
        BatchFileName := IncludeTrailingPathDelimiter(ExtractFileDir(FileName));
        FileRoot := ChangeFileExt(ExtractFileName(FileName), '.');
        ParamEstBatFileName := BatchFileName + StrRunModelBat;
        BackupParamEstBatFileName := BatchFileName + FileRoot + StrRunModelBat;
        BackupBatchFileName := BatchFileName + FileRoot + 'RunSutra.bat';
        BatchFileName := BatchFileName + 'RunSutra.bat';

        BatchFile := TStringList.Create;
        ParamEstBatFile := TStringList.Create;
        try
          if NetworkDrive then
          begin
            BatchFile.Add('pushd ' + ModelDirectory);
            ParamEstBatFile.Add('pushd ' + ModelDirectory);
          end;

          PhastModel.AddFilestToDeleteToBatchFile(ParamEstBatFile, ParamEstBatFileName);
          PLPROC_Location := GetPLPROC_Location(FileName, PhastModel);
          PLPROC_Location := Format('"%s" ', [PLPROC_Location]);
          for DSIndex := 0 to PhastModel.DataArrayManager.DataSetCount - 1 do
          begin
            ADataArray := PhastModel.DataArrayManager[DSIndex];
            if ADataArray.PestParametersUsed then
            begin
              INFLE := ExtractFileName(ChangeFileExt(FileName,
                '.' + ADataArray.Name + '.script' ));
              ParamEstBatFile.Add(PLPROC_Location + INFLE);
            end;
          end;

          BatchFile.AddStrings(PhastModel.KrigfactorsScriptLines);
          BatchFile.AddStrings(PhastModel.PestTemplateLines);
          ParamEstBatFile.AddStrings(PhastModel.PestTemplateLines);

          BatchFile.Add('"' + SutraFileName + '"');
          ParamEstBatFile.Add('"' + SutraFileName + '"');

          if PhastModel.PestUsed then
          begin
            BatchFile.Add(TCustomFileWriter.
              PestUtilityProgramPath(StrSutraObsExtractorex, FileName) + ' '
              + ChangeFileExt(ExtractFileName(FileName), StrSoei));
            ParamEstBatFile.Add(TCustomFileWriter.
              PestUtilityProgramPath(StrSutraObsExtractorex, FileName) + ' '
              + ChangeFileExt(ExtractFileName(FileName), StrSoeev));
          end;

          AddOpenListFileLine(ChangeFileExt(FileName, '.lst'), True, BatchFile,
            PhastModel.ProgramLocations);

          if NetworkDrive then
          begin
            BatchFile.Add('popd');
            ParamEstBatFile.Add('popd');
          end;
          BatchFile.Add('pause');
          BatchFile.SaveToFile(BatchFileName);
          ParamEstBatFile.SaveToFile(ParamEstBatFileName);
          TFile.Copy(ParamEstBatFileName, BackupParamEstBatFileName, True);
          TFile.Copy(BatchFileName, BackupBatchFileName, True);

          BatchFile.Clear;

          BatchFile.Add('if not exist "..\..\output\NUL" mkdir "..\..\output"');
          ModelName := ExtractFileName(ChangeFileExt(FileName, ''));
          BatchFile.Add(Format('if not exist "..\..\output\%0:s\NUL" mkdir "..\..\output\%0:s"', ['output.' + ModelName]));

          BatchFile.Add('..\..\bin\' + ExtractFilename(SutraFileName));
          BatchFile.Add('pause');
          BatchFile.SaveToFile(BatchFileName + ArchiveExt);

        finally
//          ParamEstBatFile.Free;
          BatchFile.Free;
          ParamEstBatFile.Free;
        end;
        PhastModel.AddModelInputFile(BatchFileName + ArchiveExt);
        PhastModel.AddModelInputFile(SutraFileName);
        if ShouldRunSutra then
        begin
          RunAProgram(BatchFileName);
        end;

        GeoRefWriter := TGeoRefWriter.Create(PhastModel, etExport);
        try
          GeoRefWriter.WriteFile(FileName, smtMain);
        finally
          GeoRefWriter.Free;
        end;

        PhastModel.SaveArchiveList(ChangeFileExt(FileName, '.axml'));

        if frmErrorsAndWarnings.HasMessages then
        begin
          frmErrorsAndWarnings.Show;
        end;

        if PhastModel.PestUsed then
        begin
          for PIndex := 0 to PhastModel.ModflowSteadyParameters.Count - 1 do
          begin
            AParam := PhastModel.ModflowSteadyParameters[PIndex];
            if AParam.ParameterType in SutraParamType then
            begin
              PhastModel.WritePValAndTemplate(AParam.ParameterName,
                AParam.Value, AParam);
            end;
          end;
        end;

        PhastModel.FinalizePvalAndTemplate(FileName);

        PhastModel.ExportPestInput(FileName, pecNone);
      finally
        Observations.Free;
        Schedules.Free;
        GenFlowNodeLists.Free;
        GeneralTransportList.Free;
        BcsFileNames.Free;
        FreeAndNil(SutraFileWriter);
      end;
    except
      on E: EFCreateError do
      begin
        Beep;
        MessageDlg(E.message, mtError, [mbOK], 0);
      end;
    end;
  finally
    frmProgressMM.ShouldContinue := False;
    frmProgressMM.btnAbort.Visible := False;
    frmProgressMM.Hide;
    if frmProgressMM.Owner = nil then
    begin
      FreeAndNil(frmProgressMM);
    end;
    PestInputDataArrays.Free;
  end;
end;

procedure TfrmGoPhast.UpdateModelSelection;
begin
  acModflowActive.Checked := (PhastModel.Grid <> nil)
    and (PhastModel.Grid = PhastModel.ModflowGrid);
  acPhastActive.Checked := (PhastModel.Grid <> nil)
    and (PhastModel.Grid = PhastModel.PhastGrid);
end;

procedure TfrmGoPhast.DeleteSelectedNodesOrSelectedScreenObjects;
var
  SelectedScreenObjects: TScreenObjectList;
  DeleteNodes: Boolean;
  Index: Integer;
  AScreenObject: TScreenObject;
  UndoDeleteNodes: TUndoDeleteVertices;
  UndoDeleteScreenObjects: TUndoDeleteScreenObjects;
begin
  SelectedScreenObjects := TScreenObjectList.Create;
  try
    SelectedScreenObjects.Capacity := PhastModel.ScreenObjectCount;
    // If any screen objects have selected nodes, delete the
    // selected nodes instead of the entire screen object.
    // (However, if all the nodes are selected, deleting
    // the selected nodes deletes the screen object instead.)
    DeleteNodes := False;
    for Index := PhastModel.ScreenObjectCount - 1 downto 0 do
    begin
      AScreenObject := PhastModel.ScreenObjects[Index] as TScreenObject;
      if AScreenObject.Selected then
      begin
        if AScreenObject.SelectedVertexCount > 0 then
        begin
          DeleteNodes := True;
        end;
        SelectedScreenObjects.Add(AScreenObject);
      end;
    end;
    if SelectedScreenObjects.Count > 0 then
    begin
      if DeleteNodes then
      begin
        UndoDeleteNodes := TUndoDeleteVertices.Create(SelectedScreenObjects);
        try
          UndoStack.Submit(UndoDeleteNodes);
          UndoDeleteNodes.SetPostSelection;
        except
          on EScreenObjectError do
          begin
            UndoDeleteNodes.Free;
            Beep;
            MessageDlg(StrSorryYouCantDel, mtError, [mbOK], 0);
          end;
        end;
      end
      else
      begin
        UndoDeleteScreenObjects := TUndoDeleteScreenObjects.Create(
          SelectedScreenObjects);
        UndoStack.Submit(UndoDeleteScreenObjects);
        UndoDeleteScreenObjects.SetPostSelection;
      end;
    end;
  finally
    SelectedScreenObjects.Free;
  end;
end;

procedure TfrmGoPhast.dlgSaveMt3dmsClose(Sender: TObject);
begin
  inherited;
  FRunMt3dms := FRunMt3dmsForm.cbRun.Checked;
  FRunMt3dModel := FRunMt3dmsForm.comboMt3dModelSelection.Items.Objects[FRunMt3dmsForm.comboMt3dModelSelection.ItemIndex] as TCustomModel;
  FRunMt3dmsForm.Free;
end;

procedure TfrmGoPhast.dlgSaveMt3dmsShow(Sender: TObject);
var
  ADialog: TSaveDialog;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  inherited;
  ADialog := Sender as TSaveDialog;
  FRunMt3dmsForm := TfrmRunMt3dms.createfordialog(ADialog);
  FRunMt3dmsForm.cbRun.Checked := FRunMt3dms;
  if PhastModel.ModflowPackages.Mt3dBasic.IsSelected then
  begin
    FRunMt3dmsForm.comboMt3dModelSelection.Items.AddObject(PhastModel.DisplayName, PhastModel);
  end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel.ModflowPackages.Mt3dBasic.IsSelected then
      begin
      FRunMt3dmsForm.comboMt3dModelSelection.Items.AddObject(
        ChildModel.DisplayName, ChildModel);
       end;
    end;
  Assert(FRunMt3dmsForm.comboMt3dModelSelection.Items.Count > 0);
  FRunMt3dmsForm.comboMt3dModelSelection.ItemIndex := 0;
end;

procedure TfrmGoPhast.dlgSavePestClose(Sender: TObject);
begin
  inherited;
  FRunPest := TPestExportChoice(FRunPestForm.rgRun.ItemIndex);
  FRunPestForm.Free;
end;

procedure TfrmGoPhast.dlgSavePestShow(Sender: TObject);
begin
  inherited;
  FRunPestForm := TfrmRunPest.createfordialog(dlgSavePest);
  FRunPestForm.rgRun.ItemIndex := Ord(FRunPest);
end;

procedure TfrmGoPhast.miGeneralClick(Sender: TObject);
begin
  ShowAForm(TfrmModflowOptions);
  if PhastModel.CheckWetting then
  begin
    frmErrorsAndWarnings.Show;
  end;
end;

function TfrmGoPhast.GetCanDraw: boolean;
begin
  result := FCanDraw and not FReadingFile
    and (PhastModel <> nil)
    and (PhastModel.DataSetUpdateCount = 0)
    and (FSupressDrawing = 0);
end;

procedure TfrmGoPhast.GetCurrentScreenObject(Sender: TObject;
  VD: TViewDirection; var ScreenObject: TScreenObject);
begin
  case VD of
    vdTop:
      begin
        ScreenObject := frameTopView.CurrentScreenObject;
      end;
    vdFront:
      begin
        ScreenObject := frameFrontView.CurrentScreenObject;
      end;
    vdSide:
      begin
        ScreenObject := frameSideView.CurrentScreenObject;
      end;
  else
    Assert(False);
    ScreenObject := nil;
  end;
end;

function TfrmGoPhast.GetDisvGrid: TModflowDisvGrid;
begin
  if PhastModel = nil then
  begin
    result := nil;
  end
  else
  begin
    result := PhastModel.DisvGrid;
  end
end;

function TfrmGoPhast.GetDisvUsed: Boolean;
begin
  if PhastModel = nil then
  begin
    result := False;
  end
  else
  begin
    result := PhastModel.DisvUsed;
  end;
end;

function TfrmGoPhast.GetFootPrintGrid: TFootprintGrid;
begin
  if PhastModel = nil then
  begin
    result := nil
  end
  else
  begin
    result := PhastModel.FootPrintGrid;
  end;
end;

function TfrmGoPhast.GetGrid: TCustomModelGrid;
begin
  if PhastModel = nil then
  begin
    result := nil;
  end
  else
  begin
    result := PhastModel.Grid;
  end;
end;

function TfrmGoPhast.GetHelpFormat: THelpFormat;
begin
  if FHelpFormat = hfUndefined then
  begin
    ShowAForm(TfrmHelpVersion)
  end;
  Result := FHelpFormat;
  case Result of
    hfLocal: miUseLocalHelp.Checked := True;
    hfWeb: miUseOnlineHelp.Checked := True;
    else
      Assert(False);
  end;
end;

function TfrmGoPhast.GetModelSelection: TModelSelection;
begin
  if PhastModel <> nil then
  begin
    result := PhastModel.ModelSelection;
  end
  else
  begin
    result := msUndefined;
  end;
end;

function TfrmGoPhast.GetModflowGrid: TModflowGrid;
begin
  if PhastModel = nil then
  begin
    result := nil;
  end
  else
  begin
    result := PhastModel.ModflowGrid;
  end
end;

function TfrmGoPhast.GetObservationFileName(SD: TSaveDialog): string;
begin
  result := FObservationFileName;
  if (result = '') and (PhastModel.ModelFileName <> '') then
  begin
    result := ChangeFileExt(PhastModel.ModelFileName,
      SD.DefaultExt);
  end;
  result := PhastModel.FixFileName(result);
end;

function TfrmGoPhast.IsOverStatusPanelDivider(const X: integer): boolean;
var
  Index: integer;
  Position: integer;
begin
  // This function returns True if X indicates that the cursor is over
  // one of the bars between the panels in sbMain.
  result := false;
  Position := 0;
  FMovingPanelIndex := -1;
  for Index := 0 to sbMain.Panels.Count - 2 do
  begin
    Position := Position + sbMain.Panels[Index].Width;
    result := (X >= Position) and (X <= Position + DividerWidth);
    if result then
    begin
      FMovingPanelIndex := Index;
      Exit;
    end;
    Position := Position + DividerWidth;
  end;
end;

function TfrmGoPhast.Mf2005UpToDate: boolean;
var
  WarningMessage: string;
begin
  result := ModelUpToDate(PhastModel.ProgramLocations.ModflowLocation, Mf2005Date);
  if not result then
  begin
    Beep;
    WarningMessage := Format(StrTheCurrentVersion, [StrMODFLOW2005]);
    result := (MessageDlg(WarningMessage, mtWarning, [mbYes, mbNo], 0) = mrYes);
  end;
end;

function TfrmGoPhast.Mf6UpToDate: boolean;
var
  WarningMessage: string;
begin
  result := ModelUpToDate(PhastModel.ProgramLocations.Modflow6Location, Mf6Date);
  if not result then
  begin
    Beep;
    WarningMessage := Format(StrTheCurrentVersion, [StrMODFLOW6]);
    result := (MessageDlg(WarningMessage, mtWarning, [mbYes, mbNo], 0) = mrYes);
  end;
end;

function TfrmGoPhast.FootprintUpToDate: boolean;
begin
  result := True;
  if FileExists(PhastModel.ProgramLocations.FootprintLocation) then
  begin
    result :=
      TFile.GetLastWriteTime(PhastModel.ProgramLocations.FootprintLocation)
       >= FootprintDate;
  end;

end;

//function TfrmGoPhast.ModelMateUpToDate: boolean;
//var
//  WarningMessage: string;
//begin
//  result := ModelUpToDate(PhastModel.ProgramLocations.ModelMateLocation, ModelMateDate);
//  if not result then
//  begin
//    Beep;
//    WarningMessage := Format(StrTheCurrentVersion, [StrMODFLOW2005]);
//    result := (MessageDlg(WarningMessage, mtWarning, [mbYes, mbNo], 0) = mrYes);
//  end;
//end;


function TfrmGoPhast.MfCfpUpToDate: boolean;
var
  WarningMessage: string;
begin
  result := ModelUpToDate(PhastModel.ProgramLocations.ModflowCfpLocation, MfCfpDate);
  if not result then
  begin
    Beep;
    WarningMessage := Format(StrTheCurrentVersion, [StrMODFLOWCFP]);
    result := (MessageDlg(WarningMessage, mtWarning, [mbYes, mbNo], 0) = mrYes);
  end;
end;

function TfrmGoPhast.MfLgrUpToDate: boolean;
var
  WarningMessage: string;
begin
  result := ModelUpToDate(PhastModel.ProgramLocations.ModflowLgrLocation, MfLgrDate);
  if not result then
  begin
    Beep;
    WarningMessage := Format(StrTheCurrentVersion, [StrMODFLOWLGR]);
    result := (MessageDlg(WarningMessage, mtWarning, [mbYes, mbNo], 0) = mrYes);
  end;
end;

function TfrmGoPhast.MfLgr2UpToDate: boolean;
var
  WarningMessage: string;
begin
  result := ModelUpToDate(PhastModel.ProgramLocations.ModflowLgr2Location, MfLgr2Date);
  if not result then
  begin
    Beep;
    WarningMessage := Format(StrTheCurrentVersion, [StrMODFLOWLGR2]);
    result := (MessageDlg(WarningMessage, mtWarning, [mbYes, mbNo], 0) = mrYes);
  end;
end;


function TfrmGoPhast.MfNwtUpToDate: boolean;
var
  WarningMessage: string;
//  MfNwt109UpToDate: Boolean;
  MfNwt110UpToDate: Boolean;
begin
  result := ModelUpToDate(PhastModel.ProgramLocations.ModflowNwtLocation, MfNwtDate);
  if not result then
  begin
    Beep;
//    MfNwt109UpToDate := ModelUpToDate(PhastModel.ProgramLocations.ModflowNwtLocation, MfNwtDateVersion1_0_9);
    MfNwt110UpToDate := ModelUpToDate(PhastModel.ProgramLocations.ModflowNwtLocation, MfNwtDateVersion1_1_0);
    if not MfNwt110UpToDate then
    begin
      WarningMessage := StrNwtVersion;
    end
    else
    begin
      WarningMessage := Format(StrTheCurrentVersion, [StrMODFLOWNWT]);
    end;
    result := (MessageDlg(WarningMessage, mtWarning, [mbYes, mbNo], 0) = mrYes);
  end;
end;

function TfrmGoPhast.MfOwhmUpToDate: boolean;
var
  WarningMessage: string;
begin
  result := ModelUpToDate(PhastModel.ProgramLocations.ModflowOwhmLocation, MfOwhmDate);
  if not result then
  begin
    Beep;
    WarningMessage := Format(StrTheCurrentVersion, [StrMODFLOWOWHM]);
    result := (MessageDlg(WarningMessage, mtWarning, [mbYes, mbNo], 0) = mrYes);
  end;
end;

procedure TfrmGoPhast.miMakeSelectedVerticesASeparateObjectClick(
  Sender: TObject);
begin
  inherited;
  UndoStack.Submit(TUndoMakeSelectedVerticesNewScreenObject.Create);
end;

procedure TfrmGoPhast.miManageFluxObservationsClick(Sender: TObject);
begin
  inherited;
//  if PhastModel.Mt3dmsIsSelected
//    and (PhastModel.MobileComponents.Count = 0)
//    and (PhastModel.ImmobileComponents.Count = 0) then
//  begin
//    Beep;
//    MessageDlg(StrYouMustDefineAtL, mtError, [mbOK], 0);
//    Exit;
//  end;
  ShowAForm(TfrmManageFluxObservations);
end;

procedure TfrmGoPhast.miManageHeadObservationsClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmManageHeadObservations);
end;

procedure TfrmGoPhast.miManageParametersClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmManageParameters);
end;

procedure TfrmGoPhast.miMergeObjectsClick(Sender: TObject);
var
  Undo: TUndoMergeObjects;
begin
  inherited;
  try
    Undo := TUndoMergeObjects.Create;
    if Undo.ShouldUse then
    begin
      UndoStack.Submit(Undo);
    end
    else
    begin
      Undo.Free;
    end;
  except on E: EIllegalMerge do
    begin
      Beep;
      MessageDlg(E.message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TfrmGoPhast.miLinkedRastersClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmLinkRaster);
end;

procedure TfrmGoPhast.miLinkSFRStreamsClick(Sender: TObject);
begin
  inherited;
  if frmLinkStreams = nil then
  begin
    Application.CreateForm(TfrmLinkStreams, frmLinkStreams);
  end;
  frmLinkStreams.Show;
end;

procedure TfrmGoPhast.miLockSelectedObjectsClick(Sender: TObject);
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  AList: TList;
  Undo : TUndoLockScreenObjects;
begin
  inherited;
  AList := TList.Create;
  try
    for ScreenObjectIndex := 0 to
      PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := PhastModel.ScreenObjects[ScreenObjectIndex];
      if not AScreenObject.Deleted and AScreenObject.Visible and
        AScreenObject.Selected then
      begin
        AList.Add(AScreenObject)
      end;
    end;
    if AList.Count > 0 then
    begin
      Undo := TUndoLockScreenObjects.Create(AList);
      UndoStack.Submit(Undo);
    end;
  finally
    AList.Free;
  end;
end;

procedure TfrmGoPhast.sbMainMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  { TODO : Put this functionality in a component. }
  // When the cursor is over one of the dividers between panels in
  // sbMain or when the divider is being moved, use crMoveColumn
  // as the cursor.  Otherwise, use crDefault.
  if FMovingStatusBarPanel or IsOverStatusPanelDivider(X) then
  begin
    sbMain.Cursor := crHSplit;
  end
  else
  begin
    sbMain.Cursor := crDefault;
  end;
end;

procedure TfrmGoPhast.sbMainDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
begin
  inherited;
  // The usual drawing of the text will truncate
  // the text after about 110 characters.
  StatusBar.Canvas.TextRect(Rect,Rect.Left+1,3,Panel.Text);
end;

procedure TfrmGoPhast.sbMainMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  // If the cursor is over one of the dividers between panels on
  // sbMain when the MouseDown event occurs, start moving the divider.
  FMovingStatusBarPanel := IsOverStatusPanelDivider(X);
end;

procedure TfrmGoPhast.sbMainMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  PriorWidths: integer;
  OldPosition, NewPosition: integer;
  Index: integer;
  OldWidth, NewWidth: integer;
  MaxPosition: integer;
begin
  { TODO : Put this functionality in a component. }
  // If the user is moving the dividers between two panels on sbMain,
  // move it on the MouseUp event.
  if FMovingStatusBarPanel then
  begin
    // Determine the widths of all the panels before the current one.
    // (All panels have a panel position of ppLeft and are visible.
    PriorWidths := 0;
    for Index := 0 to FMovingPanelIndex - 1 do
    begin
      PriorWidths := PriorWidths + sbMain.Panels[Index].Width + DividerWidth;
    end;
    // Find the middle of the divider between panels that the user
    // wants to move.
    OldPosition := PriorWidths + sbMain.Panels[FMovingPanelIndex].Width +
      DividerWidth div 2;
    // Store the width of the panel before the divider that the user
    // wants to move.
    OldWidth := sbMain.Panels[FMovingPanelIndex].Width;
    // Store the new width for that panel.
    NewWidth := sbMain.Panels[FMovingPanelIndex].Width + X - OldPosition;
    // Make sure the new width isn't too small.
    if NewWidth < 5 then
    begin
      NewWidth := 5;
    end;
    // Make sure the new width isn't too big.
    NewPosition := PriorWidths + NewWidth + DividerWidth div 2;
    if FMovingPanelIndex = sbMain.Panels.Count - 2 then
    begin
      MaxPosition := sbMain.Width - 20;
    end
    else
    begin
      MaxPosition := PriorWidths + OldWidth + sbMain.Panels[
        FMovingPanelIndex + 1].Width
    end;

    if NewPosition >= MaxPosition then
    begin
      NewPosition := MaxPosition;
      NewWidth := NewPosition - PriorWidths - DividerWidth div 2;
    end;

    // Set the new width of the panel.
    sbMain.Panels[FMovingPanelIndex].Width := NewWidth;

    // Set the width of the panel above the divider so that
    // its right edge doesn't move.
    NewWidth := sbMain.Panels[FMovingPanelIndex + 1].Width
      + OldWidth - NewWidth;
    if NewWidth < 5 then
    begin
      NewWidth := 5;
    end;
    sbMain.Panels[FMovingPanelIndex + 1].Width := NewWidth;
    // Stop moving the divider.
    FMovingStatusBarPanel := False;
  end;
end;

procedure TfrmGoPhast.InitializeView(ModelXWidth, ModelYWidth,
  ModelHeight: Real);
var
  LocalGrid: TCustomModelGrid;
//  Mesh: TSutraMesh3D;
  DisLimits: TGridLimit;
  X: Double;
  Mesh: IMesh3D;
  DrawMesh: IDrawMesh;
//  DisLimits: TGridLimit;
begin
  // Set the magnification so that the grid will fill most of the screen.
  frameTopView.ZoomBox.Magnification := 0.9 *
    Min(frameFrontView.ZoomBox.Width / ModelXWidth,
    frameTopView.ZoomBox.Height / ModelYWidth);
  // the following statement may not be required because the
  // magnification is set in SynchronizeViews.
  if (ModelHeight <> 0) and (frmGoPhast.ModelSelection <> msFootPrint) then
  begin
    frameFrontView.ZoomBox.Magnification := Min(
      frameTopView.ZoomBox.Magnification,
      0.9 *
      Min(frameFrontView.ZoomBox.Width / ModelXWidth,
      frameFrontView.ZoomBox.Height /
      (ModelHeight * frameFrontView.ZoomBox.Exaggeration)));
    frameTopView.ZoomBox.Magnification := frameFrontView.ZoomBox.Magnification;
  end;

  LocalGrid := Grid;
  if LocalGrid <> nil then
  begin
    // Make sure the grid is visible on the screen.
    if (Grid.ColumnCount > 0) and (Grid.RowCount > 0)
      and (Grid.LayerCount > 0) then
    begin
      DisLimits := PhastModel.DiscretizationLimits(vdFront);
      X := (DisLimits.MinX + DisLimits.MaxX)/2;
      SetFrontPosition(X, (DisLimits.MinZ + DisLimits.MaxZ)/2);


      DisLimits := PhastModel.DiscretizationLimits(vdTop);
      X := (DisLimits.MinX + DisLimits.MaxX)/2;
      SetTopPosition(X, (DisLimits.MinY + DisLimits.MaxY)/2);

//      MoveToTopCell(Grid, (Grid.ColumnCount - 1) div 2,
//        (Grid.RowCount - 1) div 2);
//      MoveToFrontCell(Grid, (Grid.ColumnCount - 1) div 2,
//        (Grid.LayerCount - 1) div 2);
    end;
  end
  else
  begin
    Mesh := PhastModel.Mesh3D;
    DrawMesh := PhastModel.DrawMesh;
    if Mesh.Is3DMesh then
    begin
      DisLimits := PhastModel.DiscretizationLimits(vdFront);
      X := (DisLimits.MinX + DisLimits.MaxX)/2;
      SetFrontPosition(X, (DisLimits.MinZ + DisLimits.MaxZ)/2);
    end;
    DisLimits := PhastModel.DiscretizationLimits(vdTop);
    X := (DisLimits.MinX + DisLimits.MaxX)/2;
    SetTopPosition(X, (DisLimits.MinY + DisLimits.MaxY)/2);
  end;

  SynchronizeViews(vdTop);
end;

procedure TfrmGoPhast.IntroductoryVideo1Click(Sender: TObject);
begin
  inherited;
  PlayIntroductoryVideo;
end;

procedure TfrmGoPhast.Invalidate3DView(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
  begin
    frame3DView.glWidModelView.Invalidate;
  end;
end;

procedure TfrmGoPhast.InvalidateAllViews;
begin
  InvalidateTop;
  InvalidateFront;
  InvalidateSide;
end;

procedure TfrmGoPhast.InvalidateDataSets;
var
  Index: integer;
  TimeList: TCustomTimeList;
begin
  PhastModel.DataArrayManager.InvalidateAllDataSets;
  for Index := 0 to PhastModel.TimeListCount - 1 do
  begin
    TimeList := PhastModel.TimeLists[Index];
    TimeList.Invalidate;
  end;
end;

procedure TfrmGoPhast.ScreenObjectsChanged(Sender: TObject);
begin
  if not (csDestroying in ComponentState)
    and ((Sender = nil) or not (Sender as TObserver).UpToDate) then
  begin
    PhastGrid.NeedToRecalculateCellColors;
    ModflowGrid.NeedToRecalculateCellColors;
    DisvGrid.MeshChanged;
    UpdateDisplay(Sender);
    Invalidate;
  end;
end;

procedure TfrmGoPhast.ScreenObjectSelectionChange(Sender: TObject);
var
  Index: Integer;
  AScreenObject: TScreenObject;
  AtLeastOneObjectSelected: Boolean;
  ExactlyOneObjectSelected: Boolean;
  MoreThanOneObjectSelected: Boolean;
begin
  if csDestroying in ComponentState then
  begin
    Exit;
  end;
  if ChangingSelection then
  begin
    Exit;
  end;
  AtLeastOneObjectSelected := (PhastModel <> nil)
    and (PhastModel.SelectedScreenObjectCount >= 1);
  ExactlyOneObjectSelected := (PhastModel <> nil)
    and (PhastModel.SelectedScreenObjectCount = 1);
  MoreThanOneObjectSelected := (PhastModel <> nil)
    and (PhastModel.SelectedScreenObjectCount > 1);

  acEditSelecteObjects.Enabled := AtLeastOneObjectSelected;
  acAddPolygonsToObject.Enabled := ExactlyOneObjectSelected;
  acAddLinesToObject.Enabled := ExactlyOneObjectSelected;
  acAddPointsToObject.Enabled := ExactlyOneObjectSelected;
  miScaleRotateorMoveObjects.Enabled := AtLeastOneObjectSelected;
  miMergeObjects.Enabled := MoreThanOneObjectSelected;
  miReverseSelectedObjects.Enabled := AtLeastOneObjectSelected;
  frameFrontView.miMergeObjects.Enabled := miMergeObjects.Enabled;
  frameTopView.miMergeObjects.Enabled := miMergeObjects.Enabled;
  frameSideView.miMergeObjects.Enabled := miMergeObjects.Enabled;
  miLockSelectedObjects.Enabled := AtLeastOneObjectSelected;
  miUnlockSelectedObjects.Enabled := AtLeastOneObjectSelected;
  acSimplifyScreenObjects.Enabled := AtLeastOneObjectSelected;

  miInvertSelectedVertices.Enabled := False;
  miMakeSelectedVerticesASeparateObject.Enabled := False;
  miSplitObjectAtSelectedVertices.Enabled := False;
  miSplitSelectedObjects.Enabled := False;
  if (PhastModel <> nil)
    and (PhastModel.SelectedScreenObjectCount >= 1) then
  begin
    for Index := 0 to PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := PhastModel.ScreenObjects[Index];
      if AScreenObject.Selected then
      begin
        if (AScreenObject.SelectedVertexCount > 0)
          and (AScreenObject.SelectedVertexCount < AScreenObject.Count) then
        begin
          miInvertSelectedVertices.Enabled := True;
          miMakeSelectedVerticesASeparateObject.Enabled := True;
          miSplitObjectAtSelectedVertices.Enabled := True;
          break;
        end;
      end;
    end;
    for Index := 0 to PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := PhastModel.ScreenObjects[Index];
      if AScreenObject.Selected and (AScreenObject.SectionCount > 1) then
      begin
        miSplitSelectedObjects.Enabled := True;
        break;
      end;
    end;
  end;
end;

procedure TfrmGoPhast.ReDrawAllViews(Sender: TObject);
var
  EnableTimer: Boolean;
begin
  if not frameTopView.Drawing
    and not frameFrontView.Drawing
    and not framesideView.Drawing then
  begin
    EnableTimer := timTimer.Enabled and FNeedFirstRedraw;
    try
      timTimer.Enabled := False;
      InvalidateImage32AllViews;
    finally
      timTimer.Enabled := EnableTimer;
    end;
  end;
end;

procedure TfrmGoPhast.ResizeZoomBoxes(Sender: TObject);
var
  EnableTimer: Boolean;
begin
  if not frameTopView.Drawing
    and not frameFrontView.Drawing
    and not framesideView.Drawing then
  begin
    EnableTimer := timTimer.Enabled and FNeedFirstRedraw;
    try
      timTimer.Enabled := False;
      if not frameTopView.ZoomBox.ImmediateResize then
      begin
        frameTopView.ZoomBox.ImmediateResize := True;
        frameTopView.IsResizing := False;
      end;
      if not frameFrontView.ZoomBox.ImmediateResize then
      begin
        frameFrontView.ZoomBox.ImmediateResize := True;
        frameFrontView.IsResizing := False;
      end;
      if not frameSideView.ZoomBox.ImmediateResize then
      begin
        frameSideView.ZoomBox.ImmediateResize := True;
        frameSideView.IsResizing := False;
      end;
    finally
      timTimer.Enabled := EnableTimer;
      if EnableTimer then
      begin
        timTimer.OnTimer := AllowDrawing;
      end;
    end;
  end;
end;

procedure TfrmGoPhast.RestoreDefault2DView1Click(Sender: TObject);
var
  ModelXWidth, ModelYWidth, ModelHeight: double;
  LocalGrid: TCustomModelGrid;
  MeshLimits: TGridLimit;
  DrawMesh: IDrawMesh;
  Mesh: IMesh3D;
  DisLimits: TGridLimit;
begin
  inherited;

  DisLimits := PhastModel.DiscretizationLimits(vdTop);
  ModelXWidth := DisLimits.MaxX - DisLimits.MinX;
  ModelYWidth := DisLimits.MaxY - DisLimits.MinY;

  LocalGrid := Grid;
  if LocalGrid <> nil then
  begin
    if (LocalGrid.ColumnCount >= 1) and (LocalGrid.RowCount >= 1)
      and (LocalGrid.LayerCount >= 1) then
    begin
//      ModelXWidth := Abs(LocalGrid.ColumnPosition[0]
//        - LocalGrid.ColumnPosition[LocalGrid.ColumnCount]);
//      ModelYWidth := Abs(LocalGrid.RowPosition[0]
//        - LocalGrid.RowPosition[LocalGrid.RowCount]);
      ModelHeight := Abs(LocalGrid.HighestElevation - LocalGrid.LowestElevation);
      InitializeView(ModelXWidth, ModelYWidth, ModelHeight);
    end;
  end
  else
  begin
    Mesh := PhastModel.Mesh3D;
    if Mesh <> nil then
    begin
      DrawMesh := PhastModel.DrawMesh;
      if Mesh.Mesh2DI.NodeCount > 0 then
      begin
//        MeshLimits := Mesh.MeshLimits(vdTop, 0);
//        ModelXWidth := MeshLimits.MaxX - MeshLimits.MinX;
//        ModelYWidth := MeshLimits.MaxY - MeshLimits.MinY;
        MeshLimits := Mesh.MeshLimits(vdFront, DrawMesh.CrossSection.Angle);
        ModelHeight := MeshLimits.MaxZ - MeshLimits.MinZ;
        if Mesh is TSutraMesh3D then
        begin
          if TSutraMesh3D(Mesh).MeshType = mtProfile then
          begin
            ModelYWidth := ModelYWidth * PhastModel.Exaggeration;
            ModelHeight := 0;
          end;
          if TSutraMesh3D(Mesh).MeshType = mt2D then
          begin
            ModelHeight := 0;
          end;
        end;
        InitializeView(ModelXWidth, ModelYWidth, ModelHeight);
      end;
    end;
  end;
end;

procedure TfrmGoPhast.miRulerFormatClick(Sender: TObject);
begin
  FClickedRuler := nil;
  ShowAForm(TfrmRulerOptions);
end;

procedure TfrmGoPhast.ToolButtonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ToolButton: TToolButton;
begin
  ToolButton := Sender as TToolButton;
  if (X < 0) or (Y < 0) or (X >= ToolButton.Width)
    or (Y >= ToolButton.Height) then
  begin
    // If the user clicked someplace that is not on the button,
    // act as if they didn't click the button.
    ToolButton.Down := False;
    SetZB_Cursors(crArrow);
    CurrentTool := nil;
  end;
end;

procedure TfrmGoPhast.SampleDEMData1Click(Sender: TObject);
begin
  inherited;
  with TfrmImportDEM.Create(nil) do
  begin
    try
      if GetData then
      begin
        ShowModal;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmGoPhast.SaveAFile(FileName: string);
var
  TempHeadFlux: TFluxObservationGroups;
  TempDrainFlux: TFluxObservationGroups;
  TempGhbFlux: TFluxObservationGroups;
  TempRivFlux: TFluxObservationGroups;
  BackUpName: string;
  FileStream: TFileStream;
  TempStreamFlux: TFluxObservationGroups;
//  FileSaved: Boolean;
begin
  FSaveTime := Now;
  if FileExists(FileName) then
  begin
    try
      FileStream := nil;
      try
        FileStream := TFileStream.Create(FileName,
          fmOpenReadWrite or fmShareDenyWrite, ReadWritePermissions);
      finally
        FileStream.Free;
      end;
    except on E: EFOpenError do
      begin
        MessageDlg(E.message, mtError, [mbOK], 0);
        Exit;
      end;
    end;
  end;
//  FileSaved := False;
  Screen.Cursor := crHourGlass;

//  ReclaimMemory;
  try
    AddMostRecentlyUsedFile(FileName);

    PhastModel.ModelFileName := FileName;

    TempHeadFlux := TFluxObservationGroups.Create(nil);
    TempDrainFlux := TFluxObservationGroups.Create(nil);
    TempGhbFlux := TFluxObservationGroups.Create(nil);
    TempRivFlux := TFluxObservationGroups.Create(nil);
    TempStreamFlux := TFluxObservationGroups.Create(nil);
    try
      TempHeadFlux.Assign(PhastModel.HeadFluxObservations);
      TempDrainFlux.Assign(PhastModel.DrainObservations);
      TempGhbFlux.Assign(PhastModel.GhbObservations);
      TempRivFlux.Assign(PhastModel.RiverObservations);
      TempStreamFlux.Assign(PhastModel.StreamObservations);

      PhastModel.HeadFluxObservations.EliminatedDeletedScreenObjects;
      PhastModel.DrainObservations.EliminatedDeletedScreenObjects;
      PhastModel.GhbObservations.EliminatedDeletedScreenObjects;
      PhastModel.RiverObservations.EliminatedDeletedScreenObjects;
      PhastModel.StreamObservations.EliminatedDeletedScreenObjects;

      PhastModel.DataArrayManager.StoreCachedData := true;

      try
        InternalSaveFile(FileName + '.tmp');
      except on E: Exception do
        begin
          Beep;
          MessageDlg(Format(StrThereWasAnErrorS, [E.message]), mtError, [mbOK], 0);
          Exit;
        end;
      end;

      Assert(FileExists(FileName + '.tmp'));
      if FileLength(FileName + '.tmp') > 0 then
      begin
        if FileExists(FileName) then
        begin
          BackUpName := ChangeFileExt(FileName, '.bak');
          if FileExists(BackUpName) then
          begin
            DeleteFile(BackUpName);
          end;
          RenameFile(FileName, BackUpName);
        end;
        RenameFile(FileName + '.tmp', FileName);
      end;
    finally
      PhastModel.HeadFluxObservations.Assign(TempHeadFlux);
      PhastModel.DrainObservations.Assign(TempDrainFlux);
      PhastModel.GhbObservations.Assign(TempGhbFlux);
      PhastModel.RiverObservations.Assign(TempRivFlux);
      PhastModel.StreamObservations.Assign(TempStreamFlux);
      TempHeadFlux.Free;
      TempDrainFlux.Free;
      TempGhbFlux.Free;
      TempRivFlux.Free;
      TempStreamFlux.Free;
    end;
    PhastModel.UpToDate := True;
  finally
    WriteIniFile;
    Screen.Cursor := crDefault;
  end;
  Application.Title := ExtractFileName(FileName) + ' ' + StrModelName;
  FSaveTime := Now;
end;

procedure TfrmGoPhast.acFileSaveExecute(Sender: TObject);
var
  Extension: string;
begin
  if sdSaveDialog.FileName = '' then
  begin
    acFileSaveAsExecute(Sender);
  end
  else
  begin
    try
      Extension := LowerCase(ExtractFileExt( sdSaveDialog.FileName));
      if (Extension = '.gpt') or (Extension = '.gpb')
        or (Extension = '.xml') or (Extension = '.mmzlib') then
      begin
//  TFileFormat = (ffAscii, ffBinary, ffXML, ffZLib);
        if Extension = '.gpt' then
        begin
          FileFormat := ffAscii;
        end
        else if Extension = '.gpb' then
        begin
          FileFormat := ffBinary;
        end
        else if Extension = '.xml' then
        begin
          FileFormat := ffXML;
        end
        else if Extension = '.mmzlib' then
        begin
          FileFormat := ffZLib;
        end;
	  
        SaveAFile(sdSaveDialog.FileName);
      end
      else
      begin
        Beep;
        MessageDlg(StrModelMuseFilesMust, mtError, [mbOK], 0);
        Exit;
      end;
    except on E: EFCreateError do
      begin
        Beep;
        MessageDlg(E.Message, mtWarning, [mbOK], 0);
        Exit;
      end;

    end;
    if not FCreateArchiveSet then
    begin
      CreateArchive := MessageDlg(StrDoYouWantToCreat,
        mtInformation, [mbYes, mbNo], 0) = mrYes;
    end;
    if CreateArchive then
    begin
      PhastModel.CreateArchive(frmGoPhast.PhastModel.ArchiveName,
        miFile.Caption + '|' + miFilesToArchive.Caption);
    end;
    ReadIniFile;
  end;
end;

procedure TfrmGoPhast.acFishnetExecute(Sender: TObject);
begin
  inherited;
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    btnFishnet.OnMouseDown(btnFishnet, mbLeft, [ssLeft], 0, 0);
  end;

  if btnFishnet.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(btnFishnet);
    // Set the cursors.
    SetZB_Cursors(crFishnet);
    CurrentTool := FishnetTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.AddMostRecentlyUsedFile(const FileName: string);
begin
  N5.Visible := True;
  N6.Visible := True;
  MostRecentlyUsed.Capacity := 5;
  MostRecentlyUsed.AddFileName(FileName);
  MostRecentlyUsed.FileToIgnore := FileName;
  N5.Visible := MostRecentlyUsed.MenuItemCount > 0;
  N6.Visible := N5.Visible;
end;

Procedure TfrmGoPhast.OnOpenFile(Sender: TObject);
const
  OneSecond = 1/24/3600;
begin
  if (Now - FStartTime) > OneSecond then
  begin
    FStartTime := Now;
    if not frmFileProgress.Visible then
    begin
      frmFileProgress.pbProgress.Max := 1000;
      frmFileProgress.Show;
    end;
    frmFileProgress.pbProgress.Position :=
      Round((1- (FFileSize - FFileStream.Position)/FFileSize)*1000);
    Application.ProcessMessages;
  end;
end;

function TfrmGoPhast.OpenAFile(const FileName: string): boolean;
var
  TempStream: TMemoryStream;
  NewTop, NewLeft: integer;
  Extension: string;
  DecompressionStream: TDecompressionStream;
  DataArray: TDataArray;
  Index: Integer;
  DataArrayManager: TDataArrayManager;
  WarningMessage: string;
  ChildIndex: integer;
  FootprintWithdrawalDataArray: TDataArray;
  ActiveDataArray: TDataArray;
  ChildModel: TChildModel;
  ADataSet: TDataArray;
begin
  Result := False;
  if not FileExists(FileName) then
  begin
    Beep;
    MessageDlg(Format(StrSDoesNotExist, [FileName]), mtError, [mbOK], 0);
    Exit;
  end
  else if FileLength(FileName) <= 0 then
  begin
    Beep;
    MessageDlg(Format(StrSDoesNotContain, [FileName]), mtError, [mbOK], 0);
    Exit;
  end;
  FSaveTime := Now;
  SetCurrentDir(ExtractFileDir(FileName));
  MenuItemsSetEnabled(False);
  try
    UpdateVerticalExaggeration(1);
    FreeAndNil(frmModflowPackages);
    FreeAndNil(frmDataSets);
    frmErrorsAndWarnings.Clear;
    Extension := LowerCase(ExtractFileExt(FileName));
    if Extension = '.gpt' then
    begin
      FileFormat := ffAscii;
    end
    else if Extension = '.gpb' then
    begin
      FileFormat := ffBinary;
    end
    else if Extension = '.xml' then
    begin
      FileFormat := ffXML;
    end
    else if Extension = '.mmzlib' then
    begin
      FileFormat := ffZLib;
    end
    else
    begin
      Beep;
      WarningMessage := Format(StrModelMuseFilesMust2, [FileName]);
      MessageDlg(WarningMessage, mtWarning, [mbOK], 0);
      Exit;
    end;

    FReadingFile := True;
    try

      ClearFileSaveDialogBoxNames;


      FObservationFileName := '';
      FPredictionFileName := '';
      AddMostRecentlyUsedFile(FileName);

      Screen.Cursor := crHourGlass;

      // Free forms that aren't destroyed automatically after
      // being shown.
      FreeAndNil(frmShowHideObjects);
      FreeAndNil(frmGridValue);
      FreeAndNil(frmLinkStreams);
      FreeAndNil(frmDisplayData);
      FreeAndNil(frmMeshInformation);
      FreeAndNil(frmCustomizeMesh);
      FreeAndNil(frmGlobalVariables);

      tbSelectClick(tbSelect);

      frmFileProgress := TfrmProgressMM.Create(nil);
      FFileStream := TBufferedFileStream.Create(FileName,
        fmOpenRead or fmShareDenyWrite, ReadWritePermissions);
      try
        FFileStream.Position := 0;
        FFileSize := FFileStream.Size;
        if FFileSize = 0 then
        begin
          Beep;
          MessageDlg(StrTheFileCanNotBe, mtError, [mbOK], 0);
          Exit;
        end;
        if frmScreenObjectProperties <> nil then
        begin
          frmScreenObjectProperties.Initialize;
        end;

        UndoStack.Clear;
        PhastModel.Clear;

        frameTopView.ZoomBox.Exaggeration := 1;
        frameFrontView.ZoomBox.Exaggeration := 1;
        frameSideView.ZoomBox.Exaggeration := 1;

        PhastModel.DataArrayManager.CreateInitialBoundaryDataSets;
        PhastModel.DataArrayManager.ClearDeletedDataSets;
        FPositionList.Clear;
        PhastModel.ClearExpressionsAndVariables;
        PhastModel.BeginScreenObjectUpdate;
        ClearFormulaErrors;
        frmFormulaErrors.DelayShowing := True;
        try
          DecompressionStream := nil;
          TempStream := TMemoryStream.Create;
          try
            case FileFormat of
              ffAscii:
                begin
                try
                  ObjectTextToBinary(FFileStream, TempStream);
                except
                  on E: EParserError do
                  begin
                    Beep;
                    result := False;
                    MessageDlg(Format(StrReadingTheFileFai2, [E.Message,
                      ChangeFileExt(FileName, '.bak')]), mtError, [mbOK], 0);
                    Exit;
                  end;
                end;
                end;
              ffBinary:
                begin
                  // do nothing
                end;
              ffXML:
                begin
                  try
                    rwObjectXMLToBinary(FFileStream, TempStream);
                  except
                    Beep;
                    MessageDlg(StrTheXmlFileYouAr, mtError, [mbOK], 0);
                    Exit;
                  end;
                end;
              ffZLib:
                begin
                  DecompressionStream := TDecompressionStream.Create(FFileStream);
                  DecompressionStream.OnProgress := OnOpenFile;
                end;
            else
              Assert(False);
            end;

            PhastModel.PhastGrid.Initialize;
            PhastModel.ModflowGrid.Initialize;
            try
              if FileFormat = ffBinary then
              begin
                FFileStream.ReadComponent(PhastModel);
                result := True;
              end
              else if FileFormat = ffZLib then
              begin
                FStartTime := Now;
                try
                DecompressionStream.ReadComponent(PhastModel);
                except on EZDecompressionError do
                  begin
                    Beep;
                    MessageDlg(Format(StrThereWasAnErrorO, [IModelVersion]),
                      mtError, [mbOK], 0);
                    Exit;
                  end;
                end;
                result := True;
              end
              else
              begin
                TempStream.Position := 0;
                TempStream.ReadComponent(PhastModel);
                result := True;
              end;
            except
              on E: EReadError do
              begin
                Beep;
                result := False;
                MessageDlg(Format(StrReadingTheFileFai, [E.Message]),
                  mtError, [mbOK], 0);
                Exit;
              end;
            end;
          finally
            TempStream.Free;
            DecompressionStream.Free;
          end;
          if PhastModel.ModelSelection = msUndefined then
          begin
            PhastModel.ModelSelection := msPhast
          end;
          PhastModel.CreateGlobalVariables;
          PhastModel.UpdateDataSets;
          PhastModel.ChildModels.Loaded;
          PhastModel.UpdateScreenObjects;
          PhastModel.SwrTabFiles.Loaded;

          PhastModel.SwrObservations.Loaded;
          PhastModel.Farms.Loaded;

          for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
          begin
            PhastModel.ChildModels[ChildIndex].ChildModel.SwrTabFiles.Loaded;
          end;
          PhastModel.LayerStructure.Loaded;
          PhastModel.SutraLayerStructure.Loaded;
          PhastModel.UpdateSutraTimeListNames;
          PhastModel.SutraMesh.Loading := True;
          PhastModel.SutraMesh.DeleteUnconnectedNodes;
          PhastModel.SutraMesh.SetCorrectElementOrientation;
          UpdateDataSetDimensions;
          PhastModel.SutraMesh.CheckUpdateElevations;
          PhastModel.SutraMesh.Loading := False;
          PhastModel.FishnetMeshGenerator.Loaded;
          PhastModel.CtsSystems.Loaded;


          // These steps ensure that the vectors will update properly
          // if the data they display changes.
          PhastModel.MaxVectors.Visible := PhastModel.MaxVectors.Visible;
          PhastModel.MidVectors.Visible := PhastModel.MidVectors.Visible;
          PhastModel.MinVectors.Visible := PhastModel.MinVectors.Visible;
          PhastModel.VelocityVectors.CheckDataSets;
          if PhastModel.VelocityVectors.SelectedItem >= 0 then
          begin
            PhastModel.VelocityVectors.BeginUpdate;
            PhastModel.VelocityVectors.EndUpdate;
          end;

          PhastModel.HeadFluxObservations.Loaded;
          PhastModel.DrainObservations.Loaded;
          PhastModel.GhbObservations.Loaded;
          PhastModel.RiverObservations.Loaded;
          PhastModel.StreamObservations.Loaded;

          PhastModel.Mt3dmsHeadMassFluxObservations.Loaded;
          PhastModel.Mt3dmsWellMassFluxObservations.Loaded;
          PhastModel.Mt3dmsDrnMassFluxObservations.Loaded;
          PhastModel.Mt3dmsRivMassFluxObservations.Loaded;
          PhastModel.Mt3dmsGhbMassFluxObservations.Loaded;
          PhastModel.Mt3dmsRchMassFluxObservations.Loaded;
          PhastModel.Mt3dmsEvtMassFluxObservations.Loaded;
          PhastModel.Mt3dmsMassLoadingMassFluxObservations.Loaded;
          PhastModel.Mt3dmsResMassFluxObservations.Loaded;
          PhastModel.Mt3dmsLakMassFluxObservations.Loaded;
          PhastModel.Mt3dmsDrtMassFluxObservations.Loaded;
          PhastModel.Mt3dmsEtsMassFluxObservations.Loaded;
          PhastModel.Mt3dmsStrMassFluxObservations.Loaded;
          PhastModel.Mt3dmsFhbHeadMassFluxObservations.Loaded;
          PhastModel.Mt3dmsFhbFlowMassFluxObservations.Loaded;

          PhastModel.UpdateOnPostInitialize;
          PhastModel.UpdateDataArrayParameterUsed;
          PhastModel.ModelFileName := FileName;
          PhastModel.FormulaManager.Pack;
          PhastModel.FormulaManager.FixSubscriptions;
          PhastModel.UpdateChildGrids;
          PhastModel.UpdateDataSetConnections;

          // Update the selected column, row, and layer for LGR models.
          PhastModel.CombinedDisplayColumn := PhastModel.CombinedDisplayColumn;
          PhastModel.CombinedDisplayRow := PhastModel.CombinedDisplayRow;
          PhastModel.CombinedDisplayLayer := PhastModel.CombinedDisplayLayer;

          miObservations.Checked := PhastModel.ObservationPurpose = ofObserved;
          miPredictions.Checked := PhastModel.ObservationPurpose = ofPredicted;

          DataArrayManager := PhastModel.DataArrayManager;
          DataArray := DataArrayManager.GetDataSetByName(rsActive);
          if Assigned(DataArray) and not Assigned(DataArray.OnUpToDateSet) then
          begin
            DataArray.OnUpToDateSet := PhastModel.OnActiveDataSetChanged;
          end;
          PhastModel.MobileComponents.Loaded;
          PhastModel.ImmobileComponents.Loaded;
          PhastModel.SutraFluxObs.Loaded;
          PhastModel.UpdateHobGroupNames;

          FootprintWithdrawalDataArray := DataArrayManager.GetDataSetByName(KWithdrawals);
          if FootprintWithdrawalDataArray <> nil then
          begin
            ActiveDataArray := DataArrayManager.GetDataSetByName(rsActive);
            if ActiveDataArray <> nil then
            begin
              ActiveDataArray.TalksTo(FootprintWithdrawalDataArray);
            end;
          end;

          DataArrayManager.Loaded;

//          for Index := DataArrayManager.DataSetCount - 1 downto 0 do
//          begin
//            ADataSet := DataArrayManager.DataSets[Index];
//            if ADataSet.PestParametersUsed then
//            begin
//              ADataSet.CreatePestParmNameDataSet;
//            end;
//          end;

          for Index := 0 to DataArrayManager.DataSetCount - 1 do
          begin
            ADataSet := DataArrayManager.DataSets[Index];
            ADataSet.RestoreUpToDateStatus;
          end;
          PhastModel.ModflowPackages.Loaded;

//          if FootprintWithdrawalDataArray <> nil then
//          begin
//            FootprintWithdrawalDataArray.UpToDate := False;
//          end;

          // invalidate data sets that depend on data sets that are constant.
          for Index := 0 to DataArrayManager.DataSetCount - 1 do
          begin
            DataArray := DataArrayManager.DataSets[Index];
            if not DataArray.UpToDate then
            begin
              DataArray.UpToDate := True;
              DataArray.UpToDate := False;
            end;
          end;

          UpdateFrontCubeForMeshCrossSection(Self);

          PhastModel.UpToDate := True;
        finally
          PhastModel.EndScreenObjectUpdate;
        end;

        UpdateDataSetDimensions;
        PhastModel.FixOldModel;
        PhastModel.ModflowPackages.RipPackage.UpdateCoverageTimeLists;
        ModelSelectionChange(nil);


        sdSaveDialog.FileName := FileName;
        Caption := StrModelName + ': ' + FileName;
      finally
        FreeAndNil(FFileStream);
        FreeAndNil(frmFileProgress);
        Screen.Cursor := crDefault;
        FCreateArchiveSet := False;
        FCreateArchive := True;
      end;
      UpdateModelCubeBreaks;
      frameTopView.AdjustScales;
      frameFrontView.AdjustScales;
      frameSideView.AdjustScales;
      ResetScreenObjectCount;
      frameTopView.ItemChange(nil);
      frameFrontView.ItemChange(nil);
      frameSideView.ItemChange(nil);
      frame3DView.SetDefaultOrientation;
      EnableInvertSelection;
      case PhastModel.ModelSelection of
        msUndefined: Assert(False);
        msPhast: acPhastActive.Checked := True;
        msModflow: acModflowActive.Checked := True;
        msModflowLGR: acModflowLgrActive.Checked := True;
        msModflowLGR2: acModflowLgr2Active.Checked := True;
        msModflowNWT: acModflowNwtActive.Checked := True;
        msModflowFmp: acModflowFmpActive.Checked := True;
        msModflowCFP: acModflowCfpActive.Checked := True;
        msSutra22: acSutra22Active.Checked := True;
        msSutra30: acSutra30Active.Checked := True;
        msFootPrint: acFootPrintActive.Checked := True;
        msModflow2015: acModflow6Active.Checked := True;
        else Assert(False);
      end;
      EnableModpathToShapefile;

      EnableExportHeadObs(nil);

      Application.Title := ExtractFileName(FileName) + ' ' + StrModelName;
      InvalidateImage32AllViews;

      TopDiscretizationChanged := True;
      FrontDiscretizationChanged := True;
      SideDiscretizationChanged := True;
      frameTopView.ZoomBoxResize(nil);
      frameFrontView.ZoomBoxResize(nil);
      frameSideView.ZoomBoxResize(nil);
      EnableLinkStreams;
      EnableManageFlowObservations;
      EnableManageHeadObservations;
      EnableHufMenuItems;
      EnableMt3dmsMenuItems;
      EnableMeshRenumbering;
      EnablePilotPointItems;
      if ModelSelection in SutraSelection then
      begin
        if SutraMesh <> nil then
        begin
          SutraMesh.OnSelectedLayerChange := frameTopView.ItemChange;
        end;
      end;

      if PhastModel.LgrUsed then
      begin
        for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
        begin
          ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
          ChildModel.ModflowGrid.UpdateCellElevations;
        end;
      end;

      if WindowState <> wsMaximized then
      begin

        NewTop := Top;
        if Top + Height > Screen.Height then
        begin
          NewTop := Screen.Height - Height;
        end;
        if NewTop < 0 then
        begin
          NewTop := 0;
        end;
        if NewTop <> Top then
        begin
          Top := NewTop;
          if Top + Height > Screen.Height then
          begin
            Height := Screen.Height - Top;
          end;
        end;

        NewLeft := Left;
        if Left + Width > Screen.Width then
        begin
          NewLeft := Screen.Width - Width;
        end;
        if NewLeft < 0 then
        begin
          NewLeft := 0;
        end;
        if NewLeft <> Left then
        begin
          Left := NewLeft;
          if Left + Width > Screen.Width then
          begin
            Width := Screen.Width - Left;
          end;
        end;
      end;
      EnableManageParameters;

    finally
      FReadingFile := False;
    end;
    // If Application.ProcessMessages is called here
    // and there is an error in a formula in the file,
    // frmFormulaErrors will not be displayed.
  //  Application.ProcessMessages;
    FPositionList.Clear;
    StoreInitalPosition;

    PhastModel.ProgramLocations.ReadFromIniFile(IniFile);

    EnableDeleteImage;
    EnableFarmMenuItems;
    EnableSwrActions;
    EnableCTS;

    WriteIniFile;
    acRestoreDefaultViewExecute(nil);
  finally
    MenuItemsSetEnabled(True);
  end;
  if frmGoPhast.Visible then
  begin
    frmFormulaErrors.DelayShowing := False;
    if frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.ShowAfterDelay;
    end;
  end;
  SetMt3dCaption;
  FSaveTime := Now;
end;

procedure TfrmGoPhast.acFileOpenExecute(Sender: TObject);
begin
  if CheckModel then
  begin
    if odOpenDialog.Execute then
    begin
      OpenAFile(odOpenDialog.FileName);
    end;
  end;
end;

procedure TfrmGoPhast.acFileSaveAsExecute(Sender: TObject);
begin
  if sdSaveDialog.Execute then
  begin
    if sdSaveDialog.FileName <> string(AnsiString(sdSaveDialog.FileName)) then
    begin
      Beep;
      MessageDlg(StrSorryTheFileName, mtError, [mbOK], 0);
      Exit;
    end;
    acFileSaveExecute(Sender);
    Caption := StrModelName + ': ' + sdSaveDialog.FileName;
    sdPhastInput.FileName := ChangeFileExt(sdSaveDialog.FileName,
      sdPhastInput.DefaultExt);
    sdModflowInput.FileName :=
      ChangeFileExt(sdSaveDialog.FileName, sdModflowInput.DefaultExt);
    FObservationFileName := sdModflowInput.FileName;
    FPredictionFileName := sdModflowInput.FileName;
    PhastModel.ModelFileName := sdSaveDialog.FileName;
    ClearFileSaveDialogBoxNames;
  end;
end;

function TfrmGoPhast.GetPhastGrid: TPhastGrid;
begin
  if PhastModel = nil then
  begin
    result := nil;
  end
  else
  begin
    result := PhastModel.PhastGrid;
  end
end;

function TfrmGoPhast.GetPredictionFileName(SD: TSaveDialog): string;
begin
  result := FPredictionFileName;
  if (result = '') and (PhastModel.ModelFileName <> '') then
  begin
    result := ChangeFileExt(PhastModel.ModelFileName,
      StrPred + SD.DefaultExt);
  end;
  result := PhastModel.FixFileName(result);
end;

procedure TfrmGoPhast.GetZoomBox(Sender: TObject; VD: TViewDirection;
  var ZoomBox: TQrbwZoomBox2);
begin
  // use the correct zoombox depending on which way you are viewing the
  // screen object from.
  case VD of
    vdTop:
      begin
        ZoomBox := frameTopView.ZoomBox;
      end;
    vdFront:
      begin
        ZoomBox := frameFrontView.ZoomBox;
      end;
    vdSide:
      begin
        ZoomBox := frameSideView.ZoomBox;
      end;
  else
    Assert(False);
    ZoomBox := nil;
  end;
end;

procedure TfrmGoPhast.miGriddedDataClick(Sender: TObject);
var
  LocalGrid: TCustomModelGrid;
  Mesh: TSutraMesh3D;
begin
  inherited;
  { TODO -cMODFLOW 6 : Modify to work with DISV grids }
  if ModelSelection in SutraSelection then
  begin
    Mesh := PhastModel.Mesh as TSutraMesh3D;
    if (Mesh <> nil) and (Mesh.Mesh2D.Nodes.Count > 0) then
    begin
      ShowAForm(TfrmImportGriddedData);
    end
    else
    begin
      Beep;
      MessageDlg(StrYouMustCreateTheMesh, mtError, [mbOK], 0);
    end;
  end
  else if DisvUsed then
  begin
    if (PhastModel <> nil) and (PhastModel.ColumnCount > 0)
      and (PhastModel.RowCount > 0) and (PhastModel.LayerCount > 0) then
    begin
      ShowAForm(TfrmImportGriddedData);
    end
    else
    begin
      Beep;
      MessageDlg(StrYouMustCreateTheDisv, mtError, [mbOK], 0);
    end;
  end
  else
  begin
    LocalGrid := Grid;
    if (LocalGrid <> nil) and (LocalGrid.ColumnCount > 0)
      and (LocalGrid.RowCount > 0) and (LocalGrid.LayerCount > 0) then
    begin
      ShowAForm(TfrmImportGriddedData);
    end
    else
    begin
      Beep;
      MessageDlg(StrYouMustCreateThe, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TfrmGoPhast.HideAllObjects1Click(Sender: TObject);
begin
  inherited;
  ShowOrHideAllScreenObjects(False);
end;

procedure TfrmGoPhast.acMoveToExecute(Sender: TObject);
begin
  ShowAForm(TfrmGoTo);
end;

procedure TfrmGoPhast.acOutlineToShapefileExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmExportModelOutline);
end;

procedure TfrmGoPhast.InvalidateViewOfModel;
begin
  frameTopView.ModelChanged := True;
  frameFrontView.ModelChanged := True;
  frameSideView.ModelChanged := True;
end;

procedure TfrmGoPhast.miInvertSelectedVerticesClick(Sender: TObject);
var
  Undo: TUndoChangeSelection;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  PointIndex: Integer;
begin
  Undo := TUndoChangeSelection.Create;
  for ScreenObjectIndex := 0 to
    PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := PhastModel.ScreenObjects[ScreenObjectIndex];
    if not AScreenObject.Deleted and AScreenObject.Visible and
      AScreenObject.Selected then
    begin
      for PointIndex := 0 to AScreenObject.Count - 1 do
      begin
        AScreenObject.SelectedVertices[PointIndex]
          := not AScreenObject.SelectedVertices[PointIndex];
      end;
    end;
  end;
  Undo.SetPostSelection;
  if Undo.SelectionChanged then
  begin
    UndoStack.Submit(Undo)
  end
  else
  begin
    Undo.Free;
  end;
end;

procedure TfrmGoPhast.miInvertSelectionClick(Sender: TObject);
var
  Index: integer;
  AScreenObject: TScreenObject;
  //  Update: boolean;
  UndoChangeSelection: TUndoChangeSelection;
  ViewDirection: TViewDirection;
  FoundSelected: boolean;
begin
  inherited;

  ViewDirection := vdTop;
  FoundSelected := false;
  for Index := 0 to PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := PhastModel.ScreenObjects[Index];
    if AScreenObject.Selected then
    begin
      ViewDirection := AScreenObject.ViewDirection;
      FoundSelected := True;
    end;
  end;
  if not FoundSelected then
  begin
    Exit;
  end;

  // Make sure no screen objects have selected nodes.
  UndoChangeSelection := TUndoChangeSelection.Create;

  for Index := 0 to PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := PhastModel.ScreenObjects[Index];
    if AScreenObject.Visible and
      (ViewDirection = AScreenObject.ViewDirection) then
    begin
      AScreenObject.Selected := not AScreenObject.Selected;
    end;
  end;

  UndoChangeSelection.SetPostSelection;
  if UndoChangeSelection.SelectionChanged then
  begin
    UndoStack.Submit(UndoChangeSelection);
  end
  else
  begin
    UndoChangeSelection.Free;
  end;
end;

procedure TfrmGoPhast.acPasteExecute(Sender: TObject);
var
  Undo: TUndoPasteScreenObjects;
  Index: Integer;
  List: TList;
begin
  inherited;
  Undo := TUndoPasteScreenObjects.Create;
  try
    for Index := 0 to PhastModel.ScreenObjectCount - 1 do
    begin
      PhastModel.ScreenObjects[Index].Selected := False;
    end;
    List := TList.Create;
    try
      PhastModel.PasteObjectsFromClipboard(List);
      if List.Count > 0 then
      begin
        Undo.StoreNewScreenObjects(List);
        UndoStack.Submit(Undo);
        Undo := nil;
      end;
    finally
      List.Free;
    end;
  finally
    begin
      Undo.Free;
    end;
  end;
end;

procedure TfrmGoPhast.acPESTExecute(Sender: TObject);
begin
  inherited;
//    frmPest.ShowModal;
  ShowAForm(TfrmPest);
end;

procedure TfrmGoPhast.acPhastActiveExecute(Sender: TObject);
begin
  if ModelSelection <> msPhast then
  begin
    UndoStack.Submit(TUndoModelSelectionChange.Create(msPhast));
  end;
end;

procedure TfrmGoPhast.acPositionBackwardExecute(Sender: TObject);
begin
  inherited;
  FPositionList.Undo;
end;

procedure TfrmGoPhast.acPositionForwardExecute(Sender: TObject);
begin
  inherited;
  FPositionList.Redo;
end;

procedure TfrmGoPhast.acQuadmeshExecute(Sender: TObject);
begin
  inherited;
  if ModelSelection = msModflow2015 then
  begin
    CanDraw := False;
    try
      PhastModel.Mf6GridType := mgtStructured;
//      PhastModel.DataArrayManager.InvalidateAllDataSets;
      PhastModel.DisvGrid.Assign(PhastModel.ModflowGrid);
      PhastModel.Mf6GridType := mgtLayered;
//      PhastModel.DataArrayManager.InvalidateAllDataSets;
    finally
      CanDraw := True;
    end;
    InvalidateGrid;
    InvalidateAllViews;
    frameTopView.ItemChange(nil);
    frameFrontView.ItemChange(nil);
    frameSideView.ItemChange(nil);
    UpdateFrontCubeForMeshCrossSection(nil);
  end;
end;

procedure TfrmGoPhast.miShowFormulaErrorsClick(Sender: TObject);
begin
  frmFormulaErrors.Handle;
  frmFormulaErrors.Show;
  if frmFormulaErrors.WindowState = wsMinimized then
  begin
    frmFormulaErrors.WindowState := wsNormal
  end;
end;

procedure TfrmGoPhast.acGenerateGridExecute(Sender: TObject);
var
  ErrorMessage: string;
begin
  SetButtonsUp(nil);
  SetZB_Cursors(crArrow);
  if ModelSelection in SutraSelection then
  begin
    if ShowAForm(TfrmMeshGenerationControlVariables) = mrOK then
    begin
      Screen.Cursor := crHourGlass;
      try
        PhastModel.GenerateSutraMesh(ErrorMessage);
        if ErrorMessage <> '' then
        begin
          Beep;
          MessageDlg(ErrorMessage, mtError, [mbOK], 0);
        end;
        if SutraMesh <> nil then
        begin
          SutraMesh.OnSelectedLayerChange := frameTopView.ItemChange;
          SutraMesh.CheckUpdateElevations;
        end;
        UpdateFrontCubeForMeshCrossSection(Self);
        TopScreenObjectsChanged := True;
        FrontScreenObjectsChanged := True;
        frame3DView.SetDefaultOrientation;
        InvalidateAllViews;
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  end
  else
  begin
    if (ModelSelection in ModflowSelection) and
      (PhastModel.LayerStructure.Count <= 1) then
    begin
      Beep;
      MessageDlg(StrYouMustDefineThe, mtError, [mbOK], 0);
    end
    else
    begin
      ShowAForm(TfrmGenerateGrid);
    end;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.acLayersExecute(Sender: TObject);
begin
  ShowAForm(TfrmLayers);
  if PhastModel.CheckWetting then
  begin
    frmErrorsAndWarnings.Show;
  end;
end;

procedure TfrmGoPhast.miGridOptionsClick(Sender: TObject);
begin
  ShowAForm(TfrmPhastGridOptions);
end;

procedure TfrmGoPhast.miFileClick(Sender: TObject);
begin
  inherited;
//  ReadIniFile;
end;

procedure TfrmGoPhast.miFreeSurfaceClick(Sender: TObject);
begin
  ShowAForm(TfrmFreeSurface);
end;

procedure TfrmGoPhast.miPrintFrequencyClick(Sender: TObject);
begin
  ShowAForm(TfrmPrintFrequency);
end;

procedure TfrmGoPhast.miPrintInitialClick(Sender: TObject);
begin
  ShowAForm(TfrmPrintInitial);
end;

procedure TfrmGoPhast.miSolutionMethodClick(Sender: TObject);
begin
  ShowAForm(TfrmSolutionMethod);
end;

procedure TfrmGoPhast.miSplitSelectedObjectsClick(Sender: TObject);
begin
  inherited;
  UndoStack.Submit(TUndoExplodeScreenObject.Create);
end;

procedure TfrmGoPhast.miTitleAndUnitsClick(Sender: TObject);
begin
  ShowAForm(TfrmUnits);
end;

procedure TfrmGoPhast.miUnlockSelectedObjectsClick(Sender: TObject);
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  AList: TList;
  Undo : TUndoUnlockScreenObjects;
begin
  inherited;
  AList := TList.Create;
  try
    for ScreenObjectIndex := 0 to
      PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := PhastModel.ScreenObjects[ScreenObjectIndex];
      if not AScreenObject.Deleted and AScreenObject.Visible and
        AScreenObject.Selected then
      begin
        AList.Add(AScreenObject)
      end;
    end;
    if AList.Count > 0 then
    begin
      Undo := TUndoUnlockScreenObjects.Create(AList);
      UndoStack.Submit(Undo);
    end;
  finally
    AList.Free;
  end;
end;

procedure TfrmGoPhast.miUseLocalHelpClick(Sender: TObject);
begin
  inherited;
  HelpFormat := hfLocal;
  miUseLocalHelp.Checked := True;
end;

procedure TfrmGoPhast.miUseOnlineHelpClick(Sender: TObject);
begin
  inherited;
  HelpFormat := hfWeb;
  miUseOnlineHelp.Checked := True;
end;

procedure TfrmGoPhast.miSteadyFlowClick(Sender: TObject);
begin
  ShowAForm(TfrmSteadyFlow);
end;

procedure TfrmGoPhast.miTimeClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmModflowTime);
end;

procedure TfrmGoPhast.miTimeControlClick(Sender: TObject);
begin
  ShowAForm(TfrmTimeControl);
end;

procedure TfrmGoPhast.SetFileFormat(const Value: TFileFormat);
var
  DefaultExtension: string;
begin
  Assert(Value in [Low(TFileFormat)..High(TFileFormat)]);
  if FFileFormat <> Value then
  begin
    FFileFormat := Value;
    case FileFormat of
      ffAscii:
        begin
          DefaultExtension := 'gpt'
        end;
      ffBinary:
        begin
          DefaultExtension := 'gpb'
        end;
      ffXML:
        begin
          DefaultExtension := 'xml'
        end;
      ffZLib:
        begin
          DefaultExtension := 'mmZLib'
        end;
    else
      Assert(False);
    end;
    if sdSaveDialog.FilterIndex <> Ord(FileFormat)+1 then
    begin
      sdSaveDialog.FilterIndex := Ord(FileFormat)+1;
    end;
    sdSaveDialog.DefaultExt := DefaultExtension;
  end;
end;

procedure TfrmGoPhast.sdFootprintClose(Sender: TObject);
begin
  inherited;
  FRunFootprint := FRunFootprintForm.cbRun.Checked;
  FRunFootprintForm.Free;
end;

procedure TfrmGoPhast.sdFootprintShow(Sender: TObject);
var
  ADialog: TSaveDialog;
begin
  inherited;
  ADialog := Sender as TSaveDialog;
  FRunFootprintForm := TfrmRunFootprint.createfordialog(ADialog);
  FRunFootprintForm.cbRun.Checked := FRunFootprint;
end;

procedure TfrmGoPhast.sdModelMateClose(Sender: TObject);
begin
  inherited;
  FRunModelMate := FRunModelMateForm.cbOpen.Checked;
  FRunModelMateForm.free;
end;

procedure TfrmGoPhast.sdModelMateShow(Sender: TObject);
begin
  inherited;
  FRunModelMateForm := TfrmRunModelMate.createfordialog(sdModelMate);
  FRunModelMateForm.cbOpen.Checked := FRunModelMate;
end;

procedure TfrmGoPhast.sdModflowInputClose(Sender: TObject);
begin
  inherited;
  FRunModflow := FRunModflowForm.cbRun.Checked;
  FRunModpath := FRunModflowForm.cbModpath.Checked;
  FCreateNewCompositeBudgetFile := FRunModflowForm.cbForceCBF.Checked;
  FRunZoneBudget := FRunModflowForm.cbExportZoneBudget.Checked;
  if FRunModflowForm.comboModelSelection.ItemIndex >= 0 then
  begin
    FRunModelSelection := FRunModflowForm.comboModelSelection.ItemIndex;
  end
  else
  begin
    FRunModelSelection := 0;
  end;
  FNewDescription := FRunModflowForm.memoComments.Text;

  FRunModflowForm.free;
end;

procedure TfrmGoPhast.sdModflowInputShow(Sender: TObject);
var
  ADialog: TSaveDialog;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  parentwnd: HWND;
  dlgclientrect: TRECT;
begin
  inherited;
  ADialog := Sender as TSaveDialog;
  FRunModflowForm := TfrmRunModflow.createfordialog(ADialog);

  parentwnd := GetParent( ADialog.handle );
  windows.GetClientRect( parentwnd, dlgclientrect );
  FRunModflowForm.Width := dlgclientrect.Right - dlgclientrect.Left;

  FRunModflowForm.cbRun.Checked := FRunModflow;

  FRunModflowForm.cbModpath.Enabled :=
    PhastModel.MODPATHIsSelected;
  FRunModflowForm.cbForceCBF.Enabled := FRunModflowForm.cbModpath.Enabled;
  FRunModflowForm.cbModpath.Checked := FRunModpath;
  FRunModflowForm.cbForceCBF.Checked := FCreateNewCompositeBudgetFile;

  FRunModflowForm.cbExportZoneBudget.Enabled :=
    PhastModel.ZoneBudgetIsSelected;
  FRunModflowForm.cbExportZoneBudget.Checked := FRunZoneBudget;
  FRunModflowForm.memoComments.Lines := PhastModel.ModflowOptions.Description;

  if PhastModel.LgrUsed then
  begin
    FRunModflowForm.comboModelSelection.Items.Add(StrCombinedModel);
    FRunModflowForm.comboModelSelection.Items.AddObject(
      PhastModel.DisplayName, PhastModel);
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      FRunModflowForm.comboModelSelection.Items.AddObject(
        ChildModel.DisplayName, ChildModel);
    end;
    FRunModflowForm.comboModelSelection.ItemIndex := FRunModelSelection;
  end
  else
  begin
    FRunModflowForm.comboModelSelection.Visible := False;
  end;
end;

procedure TfrmGoPhast.sdModpathInputClose(Sender: TObject);
begin
  inherited;
  FCreateNewCompositeBudgetFile := FRunModpathForm.cbForceCBF.Checked;
  FRunModpath := FRunModpathForm.cbRun.Checked;
  if FRunModpathForm.comboModelSelection.ItemIndex >= 0 then
  begin
    FRunModpathModelSelection := FRunModpathForm.comboModelSelection.ItemIndex;
  end
  else
  begin
    FRunModpathModelSelection := 0;
  end;
  FRunModpathForm.free;
end;

procedure TfrmGoPhast.sdModpathInputShow(Sender: TObject);
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  inherited;
  FRunModpathForm := TfrmRunModpath.createfordialog(sdModpathInput);
  FRunModpathForm.cbRun.Checked := FRunModpath;
  FRunModpathForm.cbForceCBF.Checked := FCreateNewCompositeBudgetFile;
  if PhastModel.LgrUsed then
  begin
//    FRunModpathForm.comboModelSelection.Items.Add('Combined model');
    FRunModpathForm.comboModelSelection.Items.AddObject(
      PhastModel.DisplayName, PhastModel);
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      FRunModpathForm.comboModelSelection.Items.AddObject(
        ChildModel.DisplayName, ChildModel);
    end;
    FRunModpathForm.comboModelSelection.ItemIndex := FRunModpathModelSelection;
  end
  else
  begin
    FRunModpathForm.comboModelSelection.Visible := False;
  end;
end;

procedure TfrmGoPhast.sdPhastInputClose(Sender: TObject);
begin
  inherited;
  FRunPhast := FRunPhastForm.cbRun.Checked;
  FRunPhastForm.Free;
end;

procedure TfrmGoPhast.sdPhastInputShow(Sender: TObject);
begin
  inherited;
  FRunPhastForm := TfrmRunPhast.createfordialog(sdPhastInput);
  FRunPhastForm.cbRun.Checked := FRunPhast;
end;

procedure TfrmGoPhast.sdSaveDialogClose(Sender: TObject);
begin
  inherited;
  CreateArchive := FSaveModelForm.cbSaveArchive.Checked;
  if FSaveModelForm.cbSaveDataSetValues.Checked then
  begin
    PhastModel.SaveDataSetValues := sdsvAlways;
  end
  else
  begin
    PhastModel.SaveDataSetValues := sdsvNever;
  end;
  FSaveModelForm.free;
end;

procedure TfrmGoPhast.sdSaveDialogFilterChange(Sender: TObject;
  NewIndex: Integer);
begin
  // Argh! FilterIndex is different on Linux and Windows;
  // The version in Windows is the one that is documented.  The other isn't.
  //
  // This is fixed with the Unofficial Visual CLX patch version 3.6

//{$IFDEF LINUX}
//  FileFormat := TFileFormat(NewIndex);
//{$ELSE}
//{$IFDEF MSWINDOWS}

  FileFormat := TFileFormat(NewIndex - 1);

//{$ELSE}
//  Assert(False);
//{$ENDIF}
//{$ENDIF}
end;

procedure TfrmGoPhast.sdSaveDialogShow(Sender: TObject);
begin
  inherited;
  FSaveModelForm := TfrmSaveArchive.createfordialog(sdSaveDialog);
  if FDefaultCreateArchive = dcaUnknown then
  begin
    FSaveModelForm.cbSaveArchive.Checked := CreateArchive;
  end
  else
  begin
    FSaveModelForm.cbSaveArchive.Checked := FDefaultCreateArchive = dcaSave;
  end;
  FSaveModelForm.cbSaveDataSetValues.Checked :=
    PhastModel.SaveDataSetValues = sdsvAlways;
end;

procedure TfrmGoPhast.sdSaveDialogTypeChange(Sender: TObject);
var
  Dialog: TOpenDialog;
  NewFileName: string;
  Extension: string;
begin
  inherited;
  Dialog := Sender as TOpenDialog;
  case Dialog.FilterIndex of
    1:
      begin
        FileFormat := ffAscii;
        if sdSaveDialog.FileName <> '' then
        begin
          NewFileName :=
            ChangeFileExt(sdSaveDialog.FileName, '.gpt');
        end;
      end;
    2:
      begin
        FileFormat := ffBinary;
        if sdSaveDialog.FileName <> '' then
        begin
          NewFileName :=
            ChangeFileExt(sdSaveDialog.FileName, '.gpb');
        end;
      end;
    3:
      begin
        FileFormat := ffXML;
        if sdSaveDialog.FileName <> '' then
        begin
          NewFileName :=
            ChangeFileExt(sdSaveDialog.FileName, '.xml');
        end;
      end;
    4:
      begin
        FileFormat := ffZLib;
        if sdSaveDialog.FileName <> '' then
        begin
          NewFileName :=
            ChangeFileExt(sdSaveDialog.FileName, '.mmZLib');
        end;
      end;
    5:
      begin
        Assert(Dialog = odOpenDialog);
        Extension := ExtractFileExt(Dialog.FileName);
        if Extension <> '' then
        begin
          Extension := LowerCase(Extension);
          if Extension = '.gpt' then
          begin
            FileFormat := ffAscii;
            if sdSaveDialog.FileName <> '' then
            begin
              NewFileName :=
                ChangeFileExt(sdSaveDialog.FileName, '.gpt');
            end;
          end
          else if Extension = '.gpb' then
          begin
            FileFormat := ffBinary;
            if sdSaveDialog.FileName <> '' then
            begin
              NewFileName :=
                ChangeFileExt(sdSaveDialog.FileName, '.gpb');
            end;
          end
          else if Extension = '.xml' then
          begin
            FileFormat := ffXML;
            if sdSaveDialog.FileName <> '' then
            begin
              NewFileName :=
                ChangeFileExt(sdSaveDialog.FileName, '.xml');
            end;
          end
          else if Extension = '.mmzlib' then
          begin
            FileFormat := ffZLib;
            if sdSaveDialog.FileName <> '' then
            begin
              NewFileName :=
                ChangeFileExt(sdSaveDialog.FileName, '.mmZLib');
            end;
          end
          else
          begin
            NewFileName := sdSaveDialog.FileName;
          end;
        end;
      end
    else
      begin
        FileFormat := ffAscii;
        if sdSaveDialog.FileName <> '' then
        begin
          NewFileName :=
            ChangeFileExt(sdSaveDialog.FileName, '.gpt');
        end;
      end;
  end;
  sdSaveDialog.FileName := NewFileName;
  if Sender = sdSaveDialog then
  begin
    UpdateDialogBoxFileName(sdSaveDialog, NewFileName);
//    SendMessage( GetParent(sdSaveDialog.Handle), CDM_SETCONTROLTEXT,
//      CB_FILENAME_ID, LongInt(Pchar(ExtractFileName(NewFileName))));
  end;
end;

procedure TfrmGoPhast.sdShapefileClose(Sender: TObject);
begin
  inherited;
  if FExportModpathShapefileForm.comboModelSelection.ItemIndex >= 0 then
  begin
    FExportModpathShapeFileModelChoice :=
      FExportModpathShapefileForm.comboModelSelection.ItemIndex;
  end
  else
  begin
    FExportModpathShapeFileModelChoice := 0;
  end;
  FExportModpathShapefileForm.free;
end;

procedure TfrmGoPhast.sdShapefileShow(Sender: TObject);
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  inherited;
  FExportModpathShapefileForm := TfrmExportModpathShapefile.
    createfordialog(sdShapefile);
  if PhastModel.LgrUsed then
  begin
    FExportModpathShapefileForm.comboModelSelection.Items.AddObject(
      PhastModel.DisplayName, PhastModel);
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      FExportModpathShapefileForm.comboModelSelection.Items.AddObject(
        ChildModel.DisplayName, ChildModel);
    end;
    FExportModpathShapefileForm.comboModelSelection.ItemIndex :=
      FExportModpathShapeFileModelChoice;
  end
  else
  begin
    FExportModpathShapefileForm.comboModelSelection.Visible := False;
  end;
end;

procedure TfrmGoPhast.sdSutraInputClose(Sender: TObject);
begin
  inherited;
  FRunSutra := FRunSutraFrom.cbRun.Checked;
  FRunSutraFrom.Free;
end;

procedure TfrmGoPhast.sdSutraInputShow(Sender: TObject);
begin
  inherited;
  FRunSutraFrom := TfrmRunPhast.createfordialog(sdSutraInput);
  FRunSutraFrom.cbRun.Checked := FRunSutra;

end;

procedure TfrmGoPhast.sdZonebudgetInputClose(Sender: TObject);
begin
  inherited;
  FRunZoneBudget := FRunZoneBudgetForm.cbRun.Checked;
  FRunZoneBudgetForm.free;
end;

procedure TfrmGoPhast.sdZonebudgetInputShow(Sender: TObject);
begin
  inherited;
  FRunZoneBudgetForm := TfrmRunZoneBudget.createfordialog(sdZonebudgetInput);
  FRunZoneBudgetForm.cbRun.Checked := FRunZoneBudget;
end;

procedure TfrmGoPhast.ExportFile(FileName: string; RunModel: boolean);
var
  FileDir: string;
begin
  Screen.Cursor := crHourGlass;
  CanDraw := False;
  try
    FileDir := IncludeTrailingPathDelimiter(ExtractFileDir(FileName));
    FileName := ExtractFileName(FileName);
    FileName := StringReplace(FileName, ' ', '', [rfReplaceAll, rfIgnoreCase]);
//    FileName := StringReplace(FileName, '(', '', [rfReplaceAll, rfIgnoreCase]);
//    FileName := StringReplace(FileName, ')', '', [rfReplaceAll, rfIgnoreCase]);
    FileName := FileDir+FileName;
    WritePhastInput(FileName, RunModel);
  finally
    Screen.Cursor := crDefault;
    CanDraw := True;
  end;
end;

procedure TfrmGoPhast.miExportShapefileClick(Sender: TObject);
var
  GridOK: boolean;
  MeshOK: Boolean;
begin
  inherited;
  { TODO -cMODFLOW 6 : Modify to work with DISV grids }
  GridOK := (Grid <> nil) and (Grid.ColumnCount > 0) and (Grid.RowCount > 0)
    and (Grid.LayerCount > 0);
  MeshOK := (PhastModel.Mesh3D <> nil) and (PhastModel.Mesh3D.Mesh2DI.NodeCount > 0);
  if MeshOK and (PhastModel.Mesh3D.Is3DMesh) then
  begin
    MeshOK := PhastModel.Mesh3D.ElementCount > 0;
  end;
  if GridOK or MeshOK then
  begin
    ShowAForm(TfrmExportShapefile)
  end
  else
  begin
    Beep;
    if (Grid <> nil) or DisvUsed then
    begin
      MessageDlg(StrYouMustCreateAGr, mtError, [mbOK], 0);
    end
    else
    begin
      MessageDlg(StrYouMustCreateAMe, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TfrmGoPhast.miExportModflowClick(Sender: TObject);
var
  FileName: string;
  AGrid: TCustomModelGrid;
  NameWriter: TNameFileWriter;
  ModflowVersionName: string;
  MPathLocation: string;
  SimNameWriter: IMf6_SimNameFileWriter;
  NewModelOptionsCollection: TModelOptionsCollection;
//  ModelIndex: Integer;
  ModelOptions: TModelOptions;
  DisvGrid: TModflowDisvGrid;
  ZoneBudgetLocation: string;
  PlProcLocation: string;
  MPathPackage: TModpathSelection;
begin
  inherited;
  if FExporting then
  begin
    Exit;
  end;
  try
    FExporting := True;
    if PhastModel.DisvUsed then
    begin
      DisvGrid := PhastModel.DisvGrid;
      if (DisvGrid = nil) or (DisvGrid.Layers.Count <= 0)
        or (DisvGrid.TwoDGrid.Cells.Count = 0) then
      begin
        Beep;
        MessageDlg(StrYouMustDefineTheDisv, mtError, [mbOK], 0);
        Exit;
      end;
    end
    else
    begin
      AGrid := Grid;
      if (AGrid = nil) or (AGrid.ColumnCount <= 0)
        or (AGrid.RowCount <= 0) or (AGrid.LayerCount <= 0) then
      begin
        Beep;
        MessageDlg(StrYouMustDefineThe2, mtError, [mbOK], 0);
        Exit;
      end;
    end;

    if (ModelSelection = msModflow2015) then
    begin
      MPathPackage := PhastModel.ModflowPackages.ModPath;
      if (MPathPackage.IsSelected) and (MPathPackage.MpathVersion <> mp7) then
      begin
        Beep;
        MessageDlg(StrOnlyMODPATH7CanB, mtError, [mbOK], 0);
        Exit;
      end;
    end;


    InitializeModflowInputDialog;
    if sdModflowInput.Execute then
    begin
      if PhastModel.ModflowPackages.Mt3dBasic.IsSelected then
      begin
        if Length(ExtractFileName(sdModflowInput.FileName)) > 50 then
        begin
          Beep;
          MessageDlg(StrSorryMT3DMSRestri, mtError, [mbOK], 0);
          Exit;
        end;
      end;
      if PhastModel.ModflowOptions.Description.Text <> FNewDescription then
      begin
        NewModelOptionsCollection := TModelOptionsCollection.Create(PhastModel);
        ModelOptions := NewModelOptionsCollection[0];
        ModelOptions.Description.Text := FNewDescription;
        UndoStack.Submit(TUndoGeneralOptions.Create(NewModelOptionsCollection,
          PhastModel.ModflowPackages.Mt3dBasic.MassUnit, PhastModel.UseGsflowFormat));
      end;

      if sdModflowInput.FileName <> string(AnsiString(sdModflowInput.FileName)) then
      begin
        Beep;
        MessageDlg(StrSorryTheFileName, mtError, [mbOK], 0);
        Exit;
      end;
      if sdModflowInput.FileName <>
        PhastModel.FixFileName(sdModflowInput.FileName) then
      begin
        Beep;
        MessageDlg(StrSpaceCharactersAre, mtError, [mbOK], 0);
        Exit;
      end;
      case PhastModel.ObservationPurpose of
        ofObserved: FObservationFileName := sdModflowInput.FileName;
        ofPredicted: FPredictionFileName := sdModflowInput.FileName;
        else Assert(False);
      end;
      // erase the list of model input files to be stored in the archive.
      PhastModel.ClearModelFiles;

      if not FileExists(PhastModel.ModflowLocation) then
      begin
        GetProgramLocations(PhastModel);
        if not FileExists(PhastModel.ModflowLocation) then
        begin
          Beep;
          case PhastModel.ModelSelection of
            msModflow: ModflowVersionName := StrMODFLOW;
            msModflowLGR: ModflowVersionName := StrMODFLOWLGR;
            msModflowLGR2: ModflowVersionName := StrMODFLOWLGR2;
            msModflowNWT: ModflowVersionName := StrMODFLOWNWT;
            msModflowFmp: ModflowVersionName := StrMODFLOWOWHM;
            msModflowCfp: ModflowVersionName := StrMODFLOWCFP;
            msModflow2015: ModflowVersionName := StrMODFLOW6;
            else Assert(False);
          end;
          if MessageDlg(Format(StrSDoesNotExistAt, [ModflowVersionName]),
            mtWarning, [mbYes, mbNo], 0) <> mrYes then
          begin
            Exit;
          end;
        end;
      end;

      if PhastModel.GncIsSelected and PhastModel.DisvUsed
        and (PhastModel.ModflowPackages.GncPackage.EquationFormulation = efImplicit)
        and not ((soLinLinearAcceleration in PhastModel.ModflowPackages.SmsPackage.SmsOverrides)
        or (PhastModel.ModflowPackages.SmsPackage.LinLinearAcceleration = sllaBiCgStab))
        then
      begin
        if MessageDlg(StrWhenTheGhostNode + sLineBreak + StrDoYouWantToConti,
          mtWarning, [mbYes, mbNo], 0) <> mrYes then
        begin
          Exit;
        end;
      end;

      if not ModflowUpToDate then
      begin
        Exit;
      end;
      if not TestCompatibleModflowMt3d then
      begin
        Exit;
      end;
      if PhastModel.MODPATHIsSelected and FRunModpath then
      begin
        if not TestModpathLocationOK(PhastModel) or not PhastModel.TestModpathOK(PhastModel)
          or not ModpathUpToDate(PhastModel) then
        begin
          Exit;
        end;
        MPathLocation := PhastModel.ModPathLocation;
        if FileExists(MPathLocation) then
        begin
          PhastModel.AddBinaryFile(MPathLocation);
        end;

      end;
      if PhastModel.ModflowPackages.ZoneBudget.IsSelected and FRunZoneBudget then
      begin
        if not TestZoneBudgetLocationOK(PhastModel) or not ZoneBudgetUpToDate then
        begin
          Exit;
        end;
        if ModelSelection = msModflow2015 then
        begin
          ZoneBudgetLocation := PhastModel.ProgramLocations.ZoneBudgetLocationMf6;
        end
        else
        begin
          ZoneBudgetLocation := PhastModel.ProgramLocations.ZoneBudgetLocation;
        end;
        if FileExists(ZoneBudgetLocation) then
        begin
          PhastModel.AddBinaryFile(ZoneBudgetLocation);
        end;
      end;
      if FileExists(PhastModel.ModflowLocation) then
      begin
        PhastModel.AddBinaryFile(PhastModel.ModflowLocation);
      end;

      if PhastModel.PestUsed then
      begin
        if not TDirectory.Exists(PhastModel.ProgramLocations.PestDirectory) then
        begin
          Beep;
          MessageDlg(Format(StrPESTIsActiveButT,
            [PhastModel.ProgramLocations.PestDirectory]), mtError, [mbOK], 0);
          Exit;
        end;
        PlProcLocation := GetPLPROC_Location(FileName, PhastModel);
        if not TFile.Exists(PlProcLocation) then
        begin
          Beep;
          MessageDlg(Format(StrPLPROCWasNotFound,
            [PhastModel.ProgramLocations.PestDirectory]), mtError, [mbOK], 0);
          Exit;
        end;
      end;

      frmErrorsAndWarnings.Clear;

      FileName := sdModflowInput.FileName;
      frmFormulaErrors.sgErrors.BeginUpdate;
      CanDraw := False;
      try
        NilDisplay;

        FileName := PhastModel.FixFileName(FileName);

        NameWriter := TNameFileWriter.Create(PhastModel, FileName, etExport);
        SimNameWriter := TMf6_SimNameFileWriter.Create(PhastModel);
        try
          PhastModel.NameFileWriter := NameWriter;
          PhastModel.SimNameWriter := SimNameWriter;
          PhastModel.ExportModflowModel(FileName, FRunModflow,
            PhastModel.MODPATHIsSelected and FRunModpath,
            PhastModel.MODPATHIsSelected and FCreateNewCompositeBudgetFile,
            PhastModel.ZoneBudgetIsSelected and FRunZoneBudget, True);
        finally
          NameWriter.Free;
          PhastModel.NameFileWriter := nil;
          SimNameWriter := nil;
  //        SimNameWriter.Free;
        end;
        PhastModel.SaveArchiveList(ChangeFileExt(FileName, '.axml'));

      finally
        CanDraw := True;
        frmFormulaErrors.sgErrors.EndUpdate;
      end;

      if frmErrorsAndWarnings.HasMessages then
      begin
        frmErrorsAndWarnings.Show;
      end;
    end;
  finally
    FExporting := False;
  end;
end;

procedure TfrmGoPhast.acExportImageExecute(Sender: TObject);
begin
  inherited;
  if frmDisplayData = nil then
  begin
    Application.CreateForm(TfrmDisplayData, frmDisplayData);
//    UpdateFrmGridColor(True);
  end;
    UpdateFrmDisplayData(True);
//  if frmGridColor = nil then
//  begin
//    frmGridColor := TfrmGridColor.Create(nil);
//    UpdateFrmGridColor(True);
//  end;
//  if frmContourData = nil then
//  begin
//    frmContourData := TfrmContourData.Create(nil);
//    UpdateFrmContourData(True);
//  end;
  if frmExportImage = nil then
  begin
    Application.CreateForm(TfrmExportImage, frmExportImage);
  end;
  frmExportImage.Show;
end;

procedure TfrmGoPhast.acExportModelMateExecute(Sender: TObject);
var
  TempProject: TProject;
  ModelFile: string;
  AppFile: string;
  Index: Integer;
  ModelPair: TModelIOPair;
  FoundPair: TModelIOPair;
  CurrentDir: string;
  InputFiles: TModelIOPairs;
  NameFile: string;
  CommandLine: string;
  ModflowLocation: string;
begin
  inherited;

  if sdSaveDialog.FileName = '' then
  begin
    Beep;
    MessageDlg(StrYouMustSaveYourM, mtError, [mbOK], 0);
    Exit;
  end;

  sdModelMate.FileName := PhastModel.ModelMateProjectFileName;
  if sdModelMate.Execute then
  begin
    if sdModelMate.FileName <> string(AnsiString(sdModelMate.FileName)) then
    begin
      Beep;
      MessageDlg(StrSorryTheFileName, mtError, [mbOK], 0);
      Exit;
    end;
    if not DirectoryExists(ExtractFileDir(sdModelMate.FileName)) then
    begin
      Beep;
      MessageDlg(StrTheDirectoryForTh, mtError, [mbOK], 0);
      Exit;
    end;

    if FRunModelMate then
    begin
      if not FileExists(PhastModel.ProgramLocations.ModelMateLocation) then
      begin
        GetProgramLocations(PhastModel);
      end;
      if not ModelMateUpToDate then
      begin
        Exit;
      end;
    end;

    PhastModel.ModelMateProjectFileName :=
//      ExtractRelativePath(sdSaveDialog.FileName, sdModelMate.FileName);
      ExtractFileName(sdModelMate.FileName);

    PhastModel.ModelMateProject := nil;
    TempProject := TProject.Create(nil);
    try
      TempProject.ModelID := midModflow2005;
      PhastModel.ModelMateProject := TempProject;
    finally
      TempProject.Free;
    end;
    InitializeModflowInputDialog;

    if FileExists(sdModelMate.FileName) then
    begin
      ReadModelMateProject(sdModelMate.FileName,
        PhastModel.ModelMateProject);
    end;

    CurrentDir := GetCurrentDir;
    try
      SetCurrentDir(ExtractFileDir(sdModelMate.FileName));
      if PhastModel.ModelMateProject.ProjName = '' then
      begin
        PhastModel.ModelMateProject.ProjName := ConvertString255(Copy(ChangeFileExt(
          ExtractFileName(sdModelMate.FileName), ''), 1, MaxString255));
      end;

//      NameFile := ExtractRelativePath(sdSaveDialog.FileName,
//        ChangeFileExt(sdSaveDialog.FileName, '.nam'));
      case PhastModel.ObservationPurpose of
        ofObserved:
          begin
            NameFile := ExtractRelativePath(sdModelMate.FileName,
              ObservationFileName[sdModflowInput]);
            PhastModel.ModelMateProject.ModflowNameFile := NameFile
          end;
        ofPredicted:
          begin
            NameFile := ExtractRelativePath(sdModelMate.FileName,
              PredictionFileName[sdModflowInput]);
            PhastModel.ModelMateProject.ModflowNameFilePred := NameFile
          end;
        ofInacative:
          begin
            NameFile := '';
          end;
        else Assert(False);
      end;

      NameFile := ExpandFileName(NameFile);

      ModelFile := //ExtractRelativePath(PhastModel.ModelFileName,
        ExtractFileName(ChangeFileExt(NameFile, StrPvalExt));
      AppFile := //ExtractRelativePath(PhastModel.ModelFileName,
        ExtractFileName(ChangeFileExt(NameFile, StrJtf));
      FoundPair := nil;

      InputFiles := nil;
      case PhastModel.ObservationPurpose of
        ofObserved: InputFiles := PhastModel.ModelMateProject.MIFiles;
        ofPredicted: InputFiles := PhastModel.ModelMateProject.MIFilesPred;
        ofInacative: ;
        else Assert(False);
      end;

      if InputFiles <> nil then
      begin
        for Index := 0 to InputFiles.Count - 1 do
        begin
          ModelPair := InputFiles.Items[Index];
          if SameText(ExtractFileExt(ModelPair.ModelFile), StrPvalExt) then
          begin
            FoundPair := ModelPair;
            break;
          end;
        end;
        if FoundPair = nil then
        begin
          FoundPair := InputFiles.Add as TModelIOPair;
        end;
        FoundPair.ModelFile := ModelFile;
        FoundPair.AppFile := AppFile;
      end;

      case ModelSelection of
        msModflow: ModflowLocation :=
          PhastModel.ProgramLocations.ModflowLocation;
        msModflowLGR: ModflowLocation :=
          PhastModel.ProgramLocations.ModflowLgrLocation;
        msModflowLGR2: ModflowLocation :=
          PhastModel.ProgramLocations.ModflowLgr2Location;
        msModflowNWT: ModflowLocation :=
          PhastModel.ProgramLocations.ModflowNwtLocation;
        msModflowFmp: ModflowLocation :=
          PhastModel.ProgramLocations.ModflowOwhmLocation;
        msModflowCfp: ModflowLocation :=
          PhastModel.ProgramLocations.ModflowCfpLocation;
        else
          Assert(False);
      end;
//      CommandLine := ExtractRelativePath(NameFile, ModflowLocation)
//        + ' ' + ExtractFileName(NameFile);
      CommandLine := StrRunModelBat;
      case PhastModel.ObservationPurpose of
        ofObserved:
          begin
            PhastModel.ModelMateProject.MCLForward := CommandLine;
          end;
        ofPredicted:
          begin
            PhastModel.ModelMateProject.MCLPred := CommandLine;
          end;
        else Assert(False);
      end;

      PhastModel.UpdateModelMateProject;

      SaveModelMateProject;
      if FRunModelMate then
      begin
        if FileExists(PhastModel.ProgramLocations.ModelMateLocation) then
        begin
          RunAProgram('"' + PhastModel.ProgramLocations.ModelMateLocation
            + '" "' + ExpandFileName(PhastModel.ModelMateProjectFileName)
            + '"');
        end;
      end;
    finally
      SetCurrentDir(CurrentDir);
    end;
  end;
end;

procedure TfrmGoPhast.acExportModpathExecute(Sender: TObject);
var
  FileName: string;
  UsedModel: TCustomModel;
  ChildModel: TChildModel;
begin
  inherited;
  if not PhastModel.MODPATHIsSelected then
  begin
    Beep;
    MessageDlg(StrYouMustActivateMO,
      mtWarning, [mbOK], 0);
    Exit;
  end;

  if (sdModpathInput.FileName = '') and (sdModflowInput.FileName <> '') then
  begin
    sdModpathInput.FileName := ChangeFileExt(sdModflowInput.FileName,
      sdModpathInput.DefaultExt);
  end;
  if (sdModpathInput.FileName = '') and (sdSaveDialog.FileName <> '') then
  begin
    sdModpathInput.FileName := ChangeFileExt(sdSaveDialog.FileName,
      sdModpathInput.DefaultExt);
  end;
  if sdModpathInput.Execute then
  begin
    FileName := sdModpathInput.FileName;

    if PhastModel.LgrUsed and (FRunModpathModelSelection > 0) then
    begin
      ChildModel := PhastModel.ChildModels[FRunModpathModelSelection-1].ChildModel;
      FileName := ChangeFileExt(FileName, '');
      FileName := FileName + '_' + ChildModel.ModelName;
      FileName := ChangeFileExt(FileName, sdModpathInput.DefaultExt);
      UsedModel := ChildModel;
    end
    else
    begin
      UsedModel := PhastModel;
    end;

    if (UsedModel.ModflowPackages.ModPath.MpathVersion = mp7)
      and (UsedModel.ModflowOutputControl.HeadOC.OutputFileType <> oftBinary) then
    begin
      Beep;
      MessageDlg(StrMODPATH7RequiresA, mtError, [mbOK], 0);
      Exit;
    end;

    if sdModpathInput.FileName <> string(AnsiString(sdModpathInput.FileName)) then
    begin
      Beep;
      MessageDlg(StrSorryTheFileName, mtError, [mbOK], 0);
      Exit;
    end;
    if not TestModpathLocationOK(UsedModel) or not UsedModel.TestModpathOK(UsedModel)
      or not ModpathUpToDate(UsedModel) then
    begin
      Exit;
    end;


    PhastModel.ClearModpathFiles;
    frmFormulaErrors.sgErrors.BeginUpdate;
    try
      UsedModel.ExportModpathModel(FileName, FRunModpath,
        FCreateNewCompositeBudgetFile);
    finally
      frmFormulaErrors.sgErrors.EndUpdate;
    end;
    FCreateNewCompositeBudgetFile := False;

     if frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.Show;
    end;
  end;
end;

procedure TfrmGoPhast.acExportParRepExecute(Sender: TObject);
begin
  inherited;
  if not PestVersionOK then
  begin
    Exit;
  end;

  if odRunParRep.Execute then
  begin
    PhastModel.ExportParRepInput(odRunParRep.FileName, FRunParRep, FRunParRep,
      True, 0)
  end;
end;

procedure TfrmGoPhast.acExportPhastInputFileExecute(Sender: TObject);
var
  FileName: string;
  AGrid: TCustomModelGrid;
begin
  AGrid := Grid;
  if (AGrid = nil) or (AGrid.ColumnCount <= 0)
    or (AGrid.RowCount <= 0) or (AGrid.LayerCount <= 0) then
  begin
    Beep;
    MessageDlg(StrYouMustDefineThe3, mtError, [mbOK], 0);
    Exit;
  end;
  if (sdPhastInput.FileName = '') and (sdSaveDialog.FileName <> '') then
  begin
    sdPhastInput.FileName := ChangeFileExt(sdSaveDialog.FileName,
      sdPhastInput.DefaultExt);
  end;
  if sdPhastInput.Execute then
  begin
    if sdPhastInput.FileName <> string(AnsiString(sdPhastInput.FileName)) then
    begin
      Beep;
      MessageDlg(StrSorryTheFileName, mtError, [mbOK], 0);
      Exit;
    end;
    // remove multiple file extensions
    FileName := ChangeFileExt(sdPhastInput.FileName, '');
    FileName := ChangeFileExt(FileName, '');
    FileName := ChangeFileExt(FileName, '');
    FileName := ChangeFileExt(FileName, '');

    FileName := ChangeFileExt(FileName, sdPhastInput.DefaultExt);
    sdPhastInput.FileName := FileName;
    frmErrorsAndWarnings.Clear;
    frmFormulaErrors.sgErrors.BeginUpdate;
    try
      NilDisplay;
      ExportFile(FileName, FRunPhast);
    finally
      frmFormulaErrors.sgErrors.EndUpdate;
    end;
  end;
end;

procedure TfrmGoPhast.MeshExportProgress(Sender: TObject);
begin
  Application.ProcessMessages;
  frmFileProgress.StepIt;
end;

procedure TfrmGoPhast.acExportSutra2DMeshExecute(Sender: TObject);
var
  Mesh2D: TSutraMesh2D;
begin
  inherited;
  if (sdSaveSutraMesh.FileName = '') and (PhastModel.ModelFileName <> '') then
  begin
    sdSaveSutraMesh.FileName := ChangeFileExt(PhastModel.ModelFileName, '.exp')
  end;
  if sdSaveSutraMesh.Execute then
  begin
    try
      Screen.Cursor := crHourGlass;
      frmFileProgress := TfrmProgressMM.Create(nil);
      frmFileProgress.ProgressLabelCaption := StrExportingSUTRA2DM;
      Mesh2D := PhastModel.SutraMesh.Mesh2D;
      frmFileProgress.pbProgress.Max := Mesh2D.Nodes.Count + Mesh2D.Elements.Count;
      frmFileProgress.ShouldContinue := True;
      frmFileProgress.btnAbort.Visible := True;
      frmFileProgress.Show;
      frmFileProgress.BringToFront;
      Export2DMesh(sdSaveSutraMesh.FileName, MeshExportProgress);
    finally
      FreeAndNil(frmFileProgress);
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmGoPhast.miChemistryOptionsClick(Sender: TObject);
begin
  ShowAForm(TfrmChemistryOptions);
end;

procedure TfrmGoPhast.miDisplayDataSetValuesClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmDataSetValues)
end;

//var
//  HelpHandle: HINST = 0;


procedure TfrmGoPhast.acHelpContentsExecute(Sender: TObject);
begin
//  if HelpHandle <> 0 then
//  begin
//    KillApp('ModelMuse Help');
////    if not KillApp('ModelMuse Help') then ShowMessage('App not closed') ;
////    CloseWindow(HelpHandle);
//  end;

  ShowHelp('', frmGoPhast.HelpFormat);
//  HelpHandle := ShellExecute(HelpHandle, 'open', 'HH', PChar(Application.HelpFile),
//    {nil,} nil, SW_SHOWNORMAL);

//  Application.HelpShowTableOfContents;
//  HelpRouter.HelpContent;
end;

procedure TfrmGoPhast.acImportGriddedDataFilesExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmImportMultipleGriddedDataFiles);
end;

procedure TfrmGoPhast.acImportMf6FeatureFromPestExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmImportModflow6FeatureModifiedByPest)
end;

procedure TfrmGoPhast.acImportModelMateExecute(Sender: TObject);
var
  Project: TProject;
begin
  inherited;
  if sdSaveDialog.FileName = '' then
  begin
    Beep;
    MessageDlg(StrYouMustSaveYourM2, mtError, [mbOK], 0);
    Exit;
  end;

  odModelMate.FileName := PhastModel.ModelMateProjectFileName;
  if odModelMate.Execute then
  begin
    Project := TProject.Create(nil);
    try
      ReadModelMateProject(odModelMate.FileName, Project);
      PhastModel.ImportFromModelMateProject(Project);
    finally
      Project.Free;
    end;
//    PhastModel.ModelMateProjectFileName := odModelMate.FileName;
    PhastModel.ModelMateProjectFileName :=
      ExtractRelativePath(sdSaveDialog.FileName, odModelMate.FileName);
  end;
end;

procedure TfrmGoPhast.acImportSutraFeaturesFromPestExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmImportSutraFeatures)
end;

procedure TfrmGoPhast.acImportSutraFilesExecute(Sender: TObject);
var
  Extension: string;
  DefaultFileName: string;
begin
  inherited;

  if (PhastModel.ModelFileName <> '') then
  begin
    DefaultFileName := ChangeFileExt(PhastModel.ModelFileName, '.14B');
    if TFile.Exists(DefaultFileName) then
    begin
      odSutraFiles.FileName := DefaultFileName;
    end
    else
    begin
      DefaultFileName := ChangeFileExt(PhastModel.ModelFileName, '.15B');
      if TFile.Exists(DefaultFileName) then
      begin
        odSutraFiles.FileName := DefaultFileName;
      end
      else
      begin
        DefaultFileName := ChangeFileExt(PhastModel.ModelFileName, '.PVEC');
        if TFile.Exists(DefaultFileName) then
        begin
          odSutraFiles.FileName := DefaultFileName;
        end
        else
        begin
          DefaultFileName := ChangeFileExt(PhastModel.ModelFileName, '.UVEC');
          if TFile.Exists(DefaultFileName) then
          begin
            odSutraFiles.FileName := DefaultFileName;
          end
          else
          begin
            odSutraFiles.FileName := ChangeFileExt(PhastModel.ModelFileName,
              odSutraFiles.DefaultExt);
          end;
        end;
      end;
    end;
  end;
  if odSutraFiles.Execute then
  begin
    Extension := UpperCase(ExtractFileExt(odSutraFiles.FileName));
    if Extension = '.14B' then
    begin
      ImportDataSet14B(odSutraFiles.FileName);
    end
    else if Extension = '.15B' then
    begin
      ImportDataSet15B(odSutraFiles.FileName);
    end
    else if (Extension = '.PVEC') or (Extension = '.UVEC') then
    begin
      ImportInitConditions(odSutraFiles.FileName);
    end
    else
    begin
      Beep;
      MessageDlg(Format(StrSWasNotARecog, [odSutraFiles.FileName]), mtWarning, [mbOK], 0);
    end;
  end;
end;

procedure TfrmGoPhast.acImportSutraMeshExecute(Sender: TObject);
begin
  inherited;
  if dlgOpenImportSutraMesh.Execute then
  begin
    ImportSutraMeshFromFile(dlgOpenImportSutraMesh.FileName);
  end;
end;

procedure TfrmGoPhast.acImportTprogsExecute(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmImportTprogs);
end;

procedure TfrmGoPhast.miImportDistributedDatabyZoneClick(Sender: TObject);
begin
  ShowAForm(TfrmImportDistributedData);
end;

procedure TfrmGoPhast.CheckInternet;
var
  WebIniFileName: string;
  WebIniFile: TMemIniFile;
begin
  if ParamCount > 1 then
  begin
    // don't display video's if running from the command line.
    Exit;
  end;

  ReadIniFile;

  WebIniFileName := InternetIniFileName(Handle, Application.ExeName);
  if not FileExists(WebIniFileName) then
  begin
    ReadIniFile
  end;
  Assert(FileExists(WebIniFileName));
  WebIniFile:= TMemInifile.Create(WebIniFileName);
  TCheckInternetThread.Create(PhastModel.Version, WebIniFile,
    miShowVideoTips.Checked);
end;

function TfrmGoPhast.CheckModel: boolean;
var
  ModalResult: integer;
  FileName: TFileName;
begin
  if PhastModel.UpToDate then
  begin
    result := True;
  end
  else
  begin
    FileName := sdSaveDialog.FileName;
    if FileName = '' then
    begin
      FileName := StrYourModel;
    end;
    ModalResult :=
      MessageDlg(Format(StrDoYouWantToSave, [FileName]),
      mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    if ModalResult = mrYes then
    begin
      acFileSaveExecute(nil);
      if not PhastModel.UpToDate then
      begin
        result := False;
        Exit;
      end;
//      acFileSave.Execute
    end;
    result := ModalResult in [mrYes, mrNo];
  end;
end;

function TfrmGoPhast.ModelUpToDate(const FileName: string;
  CorrectDate: TDateTime): boolean;
begin
  result := True;
  if FileExists(FileName) then
  begin
    result := TFile.GetLastWriteTime(FileName) >= CorrectDate;
  end;
end;

procedure TfrmGoPhast.CheckScreenObject(Sender: TObject;
  ScreenObject: TScreenObject; var IsACurrentScreenObject: boolean);
begin
  IsACurrentScreenObject :=
    (frameTopView.CurrentScreenObject = ScreenObject)
    or (frameFrontView.CurrentScreenObject = ScreenObject)
    or (frameSideView.CurrentScreenObject = ScreenObject);
end;

procedure TfrmGoPhast.miChildModelsClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmChildModels);
end;

procedure TfrmGoPhast.miClearUndoRedostackClick(Sender: TObject);
begin
  inherited;
  UndoStack.Clear;
end;

procedure TfrmGoPhast.FormActivate(Sender: TObject);
begin
  inherited;
  acCut.ShortCut := ShortCut(Word('X'), [ssCtrl]);
  acCopy.ShortCut := ShortCut(Word('C'), [ssCtrl]);
  acPaste.ShortCut := ShortCut(Word('V'), [ssCtrl]);
end;

procedure TfrmGoPhast.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if not CheckModel then
    Action := caNone;
end;

procedure TfrmGoPhast.InvalidateModel;
begin
  if PhastModel <> nil then
  begin
    { TODO -cRefactor : Consider replacing FModel with a TNotifyEvent or interface. }
    PhastModel.Invalidate(self);
  end;
end;

procedure TfrmGoPhast.miImportShapefileClick(Sender: TObject);
begin
  if ModelSelection = msUndefined then
  begin
    Beep;
    MessageDlg('Error. No model type is selected.', mtError, [mbOK], 0);
    Exit;
  end;
  with TfrmImportShapefile.Create(nil) do
  begin
    try
      if GetData then
      begin
        ShowModal;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmGoPhast.miImportDXFFileClick(Sender: TObject);
begin
  with TfrmImportDXF.Create(nil) do
  begin
    try
      if GetData then
      begin
        ShowModal;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmGoPhast.miImportBitmapClick(Sender: TObject);
begin
  ShowAForm(TfrmImportBitmap);
  EnableDeleteImage;
end;

procedure TfrmGoPhast.miEditBitmapsClick(Sender: TObject);
begin
  // Check that there is an image to edit.
  if PhastModel.Bitmaps.Count > 0 then
  begin
    // Check if there are more than one image that could be edited.
    if PhastModel.Bitmaps.Count = 1 then
    begin
      // There is only one image that can be edited.
      // Allow the user to edit it.
      with TfrmImportBitmap.Create(nil) do
      begin
        try
          GetData(PhastModel.Bitmaps.Items[0] as TCompressedBitmapItem);
          ShowModal;
        finally
          Free;
        end;
      end;
    end
    else
    begin
      // Let the user pick which image to edit.
      ShowAForm(TfrmSelectImage);
    end;
  end
  else
  begin
    // There aren't any images to edit.  Allow the user to import one instead.
    miImportBitmapClick(nil);
  end;
  EnableDeleteImage;
end;

procedure TfrmGoPhast.SelectDefaultButton;
var
  AList: TList;
  AllUp: boolean;
  Index: integer;
  Button: TToolButton;
begin
  AList := TList.Create;
  try
    FillButtonList(AList);
    AllUp := True;
    for Index := 0 to AList.Count - 1 do
    begin
      Button := AList[Index];
      if Button.Down then
      begin
        AllUp := False;
        Break;
      end;
    end;
    if AllUp then
    begin
      tbSelect.Down := True;
      tbSelectClick(tbSelect);
    end;
  finally
    AList.Free
  end;
end;

procedure TfrmGoPhast.miScaleRotateorMoveObjectsClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmScaleRotateMove);
end;

procedure TfrmGoPhast.miSearchForObjectClick(Sender: TObject);
begin
  // Allow the user to select an object based on what it does.
  ShowAForm(TfrmSearch);
end;

procedure TfrmGoPhast.miShowSelectedObjectsClick(Sender: TObject);
begin
  frmSelectedObjects.Show;
end;

procedure TfrmGoPhast.miShowVideoTipsClick(Sender: TObject);
begin
  inherited;
  miShowVideoTips.Checked := not miShowVideoTips.Checked;
  WriteIniFile;
end;

procedure TfrmGoPhast.tbShellClick(Sender: TObject);
begin
  Invalidate3DView(nil);
end;

procedure TfrmGoPhast.acShowGridShellExecute(Sender: TObject);
begin
  acShowGridShell.Checked := not acShowGridShell.Checked;
  tbShell.Down := acShowGridShell.Checked;
  Invalidate3DView(nil);
end;

procedure TfrmGoPhast.acShowTopGridExecute(Sender: TObject);
begin
  acShowTopGrid.Checked := not acShowTopGrid.Checked;
  tbTopGrid.Down := acShowTopGrid.Checked;
  Invalidate3DView(nil);
end;

procedure TfrmGoPhast.acShowTopMeshExecute(Sender: TObject);
begin
  inherited;
  acShowTopMesh.Checked := not acShowTopMesh.Checked;
  btnShowTopMesh.Down := acShowTopMesh.Checked;
  Invalidate3DView(nil);
end;

procedure TfrmGoPhast.acShowFrontGridExecute(Sender: TObject);
begin
  acShowFrontGrid.Checked := not acShowFrontGrid.Checked;
  tbFrontGrid.Down := acShowFrontGrid.Checked;
  Invalidate3DView(nil);
end;

procedure TfrmGoPhast.acShowFrontMeshExecute(Sender: TObject);
begin
  inherited;
  acShowFrontMesh.Checked := not acShowFrontMesh.Checked;
  btnShowFrontMesh.Down := acShowFrontMesh.Checked;
  Invalidate3DView(nil);
end;

procedure TfrmGoPhast.acShowSideGridExecute(Sender: TObject);
begin
  acShowSideGrid.Checked := not acShowSideGrid.Checked;
  tbSideGrid.Down := acShowSideGrid.Checked;
  Invalidate3DView(nil);
end;

procedure TfrmGoPhast.acRestoreDefaultViewExecute(Sender: TObject);
begin
  frame3DView.SetDefaultOrientation;
  Invalidate3DView(nil);
end;

procedure TfrmGoPhast.acMeasureExecute(Sender: TObject);
begin
  inherited;
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbMeasure.OnMouseDown(tbMeasure, mbLeft, [ssLeft], 0, 0);
  end;

  if tbMeasure.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbMeasure);
    // Set the cursors.
    SetZB_Cursors(crMeasure);
  end;
  CurrentTool := RulerTool;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.acRunFootprintExecute(Sender: TObject);
var
  AGrid: TCustomModelGrid;
begin
  inherited;
  // Check WellFootprint location
  AGrid := Grid;
  if (AGrid = nil) or (AGrid.ColumnCount <= 0)
    or (AGrid.RowCount <= 0) or (AGrid.LayerCount <= 0) then
  begin
    Beep;
    MessageDlg(StrYouMustDefineFootprintGrid, mtError, [mbOK], 0);
    Exit;
  end;

  sdFootprint.FileName := GetFootprintInputFileName;
  if sdFootprint.Execute then
  begin
    if sdFootprint.FileName <> string(AnsiString(sdFootprint.FileName)) then
    begin
      Beep;
      MessageDlg(StrSorryTheFileName, mtError, [mbOK], 0);
      Exit;
    end;
    if not FileExists(PhastModel.ProgramLocations.FootprintLocation) then
    begin
      acFootprintProgramLocationExecute(nil);
    end;
    if not FileExists(PhastModel.ProgramLocations.FootprintLocation) then
    begin
      Beep;
      MessageDlg(StrYouMustSpecifyFootprint, mtError, [mbOK], 0);
      Exit;
    end;

    if not FootprintUpToDate then
    begin
      if (MessageDlg(StrAMoreRecentVersionFootprint, mtWarning,
        [mbYes, mbNo], 0) <> mrYes) then
      begin
        Exit;
      end;
    end;

    FFootprintFileName := sdFootprint.FileName;

    PhastModel.ExportFootprintInput(sdFootprint.FileName, FRunFootprint);
  end;
//
//  if True then
//  begin
//
//  end;
//  sdFootprint.FileName :=
end;

procedure TfrmGoPhast.acRunModflow6Execute(Sender: TObject);
begin
  inherited;
  miExportModflowClick(Sender);
end;

procedure TfrmGoPhast.acRunModflowCfpExecute(Sender: TObject);
begin
  inherited;
  miExportModflowClick(Sender);
end;

procedure TfrmGoPhast.acRunModflowFmpExecute(Sender: TObject);
begin
  inherited;
  if PhastModel.ModflowOutputControl.PrintInputCellLists
    and (PhastModel.ModflowTransientParameters.Count > 0) then
  begin
    Beep;
    if (MessageDlg(StrMODFLOWOWHMMayCra, mtWarning, [mbYes, mbNo], 0) = mrYes) then
    begin
      PhastModel.ModflowOutputControl.PrintInputCellLists := False;
      MessageDlg(StrPrintingCellLists, mtInformation, [mbOK], 0);
    end;
  end;

  if PhastModel.LgrUsed then
  begin
    acRunModflowLgrExecute(Sender);
  end
  else
  begin
    miExportModflowClick(Sender);
  end;
end;

procedure TfrmGoPhast.acRunModflowLgrExecute(Sender: TObject);
var
  FileName: string;
  AGrid: TCustomModelGrid;
  NameWriter: TNameFileWriter;
  Index: Integer;
  ChildModel: TChildModel;
  ChildModelNameFile: string;
  UsedModel: TCustomModel;
  ModelFileExists: Boolean;
  ModelFile: string;
  NewModelOptionsCollection: TModelOptionsCollection;
  ModelOptions: TModelOptions;
begin
  if FExporting then
  begin
    Exit;
  end;
  try
    FExporting := True;
    inherited;
    AGrid := Grid;
    if (AGrid = nil) or (AGrid.ColumnCount <= 0)
      or (AGrid.RowCount <= 0) or (AGrid.LayerCount <= 0) then
    begin
      Beep;
      MessageDlg(StrYouMustDefineThe4, mtError, [mbOK], 0);
      Exit;
    end;
    for Index := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      AGrid := PhastModel.ChildModels[Index].ChildModel.Grid;
      if (AGrid = nil) or (AGrid.ColumnCount <= 0)
        or (AGrid.RowCount <= 0) or (AGrid.LayerCount <= 0) then
      begin
        Beep;
        MessageDlg(StrYouMustDefineThe4, mtError, [mbOK], 0);
        Exit;
      end;
    end;
    InitializeModflowLgrInputDialog;
    if sdModflowLgr.Execute then
    begin
      if PhastModel.ModflowPackages.Mt3dBasic.IsSelected then
      begin
        if Length(ExtractFileName(sdModflowLgr.FileName)) > 50 then
        begin
          Beep;
          MessageDlg(StrSorryMT3DMSRestri, mtError, [mbOK], 0);
          Exit;
        end;
      end;
      if PhastModel.ModflowOptions.Description.Text <> FNewDescription then
      begin
        NewModelOptionsCollection := TModelOptionsCollection.Create(PhastModel);
        ModelOptions := NewModelOptionsCollection[0];
        ModelOptions.Description.Text := FNewDescription;
        UndoStack.Submit(TUndoGeneralOptions.Create(NewModelOptionsCollection,
          PhastModel.ModflowPackages.Mt3dBasic.MassUnit, PhastModel.UseGsflowFormat));
      end;

      if sdModflowLgr.FileName <> string(AnsiString(sdModflowLgr.FileName)) then
      begin
        Beep;
        MessageDlg(StrSorryTheFileName, mtError, [mbOK], 0);
        Exit;
      end;
      if sdModflowLgr.FileName <>
        PhastModel.FixFileName(sdModflowLgr.FileName) then
      begin
        Beep;
        MessageDlg(StrSpaceCharactersAre, mtError, [mbOK], 0);
        Exit;
      end;
      case PhastModel.ObservationPurpose of
        ofObserved: FObservationFileName := sdModflowLgr.FileName;
        ofPredicted: FPredictionFileName := sdModflowLgr.FileName;
        else Assert(False);
      end;

      if ModelSelection in [msModflowLGR] then
      begin
        if not FileExists(PhastModel.ProgramLocations.ModflowLgrLocation) then
        begin
          case FRunModelSelection of
            0, 1:
              begin
                UsedModel := PhastModel;
              end;
            else
              begin
                UsedModel := PhastModel.ChildModels[FRunModelSelection-2].ChildModel;
              end;
          end;
          GetProgramLocations(UsedModel);
          if not FileExists(PhastModel.ProgramLocations.ModflowLgrLocation) then
          begin
            Beep;
            if MessageDlg(StrMODFLOWLGRDoesNot, mtWarning,
              [mbYes, mbNo], 0) <> mrYes then
            begin
              Exit;
            end;
          end;
        end;
        if not MfLgrUpToDate then
        begin
          Exit;
        end;
      end
      else
      begin
        Assert(ModelSelection in [msModflowLGR2, msModflowFmp]);
        ModelFileExists := False;
        case ModelSelection of
          msModflowLGR2:
            begin
              ModelFileExists := FileExists(PhastModel.ProgramLocations.ModflowLgr2Location);
            end;
          msModflowFmp:
            begin
              ModelFileExists := FileExists(PhastModel.ProgramLocations.ModflowOwhmLocation);
            end;
          else
            Assert(False);
        end;
        if not ModelFileExists then
        begin
          case FRunModelSelection of
            0, 1:
              begin
                UsedModel := PhastModel;
              end;
            else
              begin
                UsedModel := PhastModel.ChildModels[FRunModelSelection-2].ChildModel;
              end;
          end;
          GetProgramLocations(UsedModel);
          case ModelSelection of
            msModflowLGR2:
              begin
                ModelFileExists := FileExists(PhastModel.ProgramLocations.ModflowLgr2Location);
              end;
            msModflowFmp:
              begin
                ModelFileExists := FileExists(PhastModel.ProgramLocations.ModflowOwhmLocation);
              end;
            else
              Assert(False);
          end;
          if not ModelFileExists then
          begin
            Beep;
            if MessageDlg(StrMODFLOWLGRDoesNot, mtWarning,
              [mbYes, mbNo], 0) <> mrYes then
            begin
              Exit;
            end;
          end;
        end;
        case ModelSelection of
          msModflowLGR2:
            begin
              if not MfLgr2UpToDate then
              begin
                Exit;
              end;
            end;
          msModflowFmp:
            begin
              if not MfOwhmUpToDate then
              begin
                Exit;
              end;
            end
          else
            Assert(False);
          end;
      end;
      // erase the list of model input files to be stored in the archive.
      PhastModel.ClearModelFiles;
      if ModelSelection in [msModflowLGR] then
      begin
        if FileExists(PhastModel.ProgramLocations.ModflowLgrLocation) then
        begin
          PhastModel.AddModelInputFile(PhastModel.ProgramLocations.ModflowLgrLocation);
        end;
      end
      else
      begin
        Assert(ModelSelection in [msModflowLGR2, msModflowFmp]);
        case ModelSelection of
          msModflowLGR2:
            begin
              ModelFile := PhastModel.ProgramLocations.ModflowLgr2Location;
            end;
          msModflowFmp:
            begin
              ModelFile := PhastModel.ProgramLocations.ModflowOwhmLocation;
            end;
          else
            Assert(False);
        end;

        if FileExists(ModelFile) then
        begin
          PhastModel.AddModelInputFile(ModelFile);
        end;
      end;
      frmErrorsAndWarnings.Clear;

      FileName := sdModflowLgr.FileName;
      frmFormulaErrors.sgErrors.BeginUpdate;
      CanDraw := False;
      try
        NilDisplay;

        FileName := PhastModel.FixFileName(FileName);

        Assert(FRunModelSelection >= 0);
        case FRunModelSelection of
          0:
            begin
              NameWriter := TNameFileWriter.Create(PhastModel, FileName, etExport);
              try
                PhastModel.NameFileWriter := NameWriter;
                for Index := 0 to PhastModel.ChildModels.Count - 1 do
                begin
                  ChildModel := PhastModel.ChildModels[Index].ChildModel;
                  ChildModelNameFile := ChildModel.Child_NameFile_Name(FileName);
                  NameWriter := TNameFileWriter.Create(ChildModel,
                    ChildModelNameFile, etExport);
                  ChildModel.NameFileWriter := NameWriter;
                end;
                PhastModel.ExportModflowLgrModel(FileName, FRunModflow,
                  FRunModpath and PhastModel.MODPATHIsSelected,
                  FCreateNewCompositeBudgetFile, FRunZoneBudget
                  and PhastModel.ZoneBudgetIsSelected, True);
              finally
                PhastModel.NameFileWriter.Free;
                PhastModel.NameFileWriter := nil;
                for Index := 0 to PhastModel.ChildModels.Count - 1 do
                begin
                  ChildModel := PhastModel.ChildModels[Index].ChildModel;
                  ChildModel.NameFileWriter.Free;
                  ChildModel.NameFileWriter := nil;
                end;
              end;
            end;
          1:
            begin
              NameWriter := TNameFileWriter.Create(PhastModel, FileName, etExport);
              try
                PhastModel.NameFileWriter := NameWriter;
                PhastModel.ExportSeparateLgrModel(FileName, FRunModflow,
                  FRunModpath and PhastModel.ModflowPackages.ModPath.IsSelected,
                  FRunZoneBudget
                  and PhastModel.ModflowPackages.ZoneBudget.IsSelected, True);
              finally
                PhastModel.NameFileWriter.Free;
                PhastModel.NameFileWriter := nil;
              end;
            end;
          else
            begin
              ChildModel := PhastModel.ChildModels[FRunModelSelection-2].ChildModel;
              ChildModelNameFile := ChildModel.Child_NameFile_Name(FileName);
              NameWriter := TNameFileWriter.Create(ChildModel,
                ChildModelNameFile, etExport);
              try
                ChildModel.NameFileWriter := NameWriter;
                ChildModel.ExportSeparateLgrModel(FileName, FRunModflow,
                  FRunModpath and ChildModel.ModflowPackages.ModPath.IsSelected,
                  FRunZoneBudget
                  and ChildModel.ModflowPackages.ZoneBudget.IsSelected, True);
              finally
                ChildModel.NameFileWriter.Free;
                ChildModel.NameFileWriter := nil;
              end;
            end;
        end;
        PhastModel.SaveArchiveList(ChangeFileExt(FileName, '.axml'));
      finally
        CanDraw := True;
        frmFormulaErrors.sgErrors.EndUpdate;
      end;

      if frmErrorsAndWarnings.HasMessages then
      begin
        frmErrorsAndWarnings.Show;
      end;
    end;
  finally
    FExporting := False;
  end;
end;

procedure TfrmGoPhast.acRunModflowNwtExecute(Sender: TObject);
begin
  inherited;
  miExportModflowClick(Sender);
end;

procedure TfrmGoPhast.acRunMt3dmsExecute(Sender: TObject);
var
  FileName: string;
  NameWriter: TMt3dmsNameWriter;
  FileDir: string;
  Mt3dExtension: string;
begin
  inherited;
  if DisvUsed then
  begin
    Beep;
    MessageDlg(StrMT3DCanOnlyBeUse,
      mtWarning, [mbOK], 0);
    Exit;
  end;
  if not PhastModel.Mt3dmsIsSelected then
  begin
    Beep;
    MessageDlg(StrYouMustActivateMT,
      mtWarning, [mbOK], 0);
    Exit;
  end;
  if not TestCompatibleModflowMt3d then
  begin
    Exit;
  end;

  if (dlgSaveMt3dms.FileName = '') and (sdModflowInput.FileName <> '') then
  begin
    dlgSaveMt3dms.FileName := ChangeFileExt(sdModflowInput.FileName,
      dlgSaveMt3dms.DefaultExt);
  end;
  if (dlgSaveMt3dms.FileName = '') and (sdSaveDialog.FileName <> '') then
  begin
    dlgSaveMt3dms.FileName := ChangeFileExt(sdSaveDialog.FileName,
      dlgSaveMt3dms.DefaultExt);
  end;

  if dlgSaveMt3dms.Execute then
  begin
    if dlgSaveMt3dms.FileName <> string(AnsiString(dlgSaveMt3dms.FileName)) then
    begin
      Beep;
      MessageDlg(StrSorryTheFileName, mtError, [mbOK], 0);
      Exit;
    end;
    case FRunMt3dModel.ModflowPackages.Mt3dBasic.Mt3dVersion of
      mvUSGS:
        begin
          if not TestMt3dUsgsLocationOK(FRunMt3dModel) or not Mt3dUsgsUpToDate then
          begin
            Exit;
          end;
        end;
      mvMS:
        begin
          if not TestMt3dmsLocationOK(FRunMt3dModel) or not Mt3dUpToDate then
          begin
            Exit;
          end;
        end;
      else Assert(False);
    end;

    FileName := dlgSaveMt3dms.FileName;
    if FRunMt3dModel is TChildModel then
    begin
      Mt3dExtension := ExtractFileExt(FileName);
      FileName := TChildModel(FRunMt3dModel).Child_NameFile_Name(FileName);
      FileName := ChangeFileExt(FileName, Mt3dExtension);
    end;
    if Length(ExtractFileName(FileName)) > 50 then
    begin
      Beep;
      MessageDlg(StrSorryMT3DMSRestri, mtError, [mbOK], 0);
      Exit;
    end;

    FileDir := IncludeTrailingPathDelimiter(ExtractFileDir(FileName));
    FileName := ExtractFileName(FileName);
    FileName := StringReplace(FileName, ' ', '_', [rfReplaceAll, rfIgnoreCase]);
    FileName := FileDir+FileName;

    frmFormulaErrors.sgErrors.BeginUpdate;
    try
      FRunMt3dModel.Mt3dmsInputFiles.Clear;
      FRunMt3dModel.Mt3dmsOutputFiles.Clear;
//      PhastModel.ClearMt3dmsFiles;
      NameWriter := TMt3dmsNameWriter.Create(FRunMt3dModel, FileName, etExport);
      try
        FRunMt3dModel.NameFileWriter := NameWriter;
        FRunMt3dModel.ExportMt3dmsModel(FileName, FRunMt3dms, True);
      finally
        NameWriter.Free;
        FRunMt3dModel.NameFileWriter := nil;
      end;

    finally
      frmFormulaErrors.sgErrors.EndUpdate;
    end;

    if frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.Show;
    end;
  end;
end;

procedure TfrmGoPhast.acRunPestExecute(Sender: TObject);
var
  FileName: string;
begin
  inherited;
  if not PestVersionOK then
  begin
    Exit;
  end;

  FileName := '';
  if (PhastModel.ModelFileName <> '') then
  begin
    if PhastModel.ObservationPurpose = ofPredicted then
    begin
      FileName := ChangeFileExt(PhastModel.ModelFileName + StrPred,
        dlgSavePest.DefaultExt);
    end
    else
    begin
      FileName := ChangeFileExt(PhastModel.ModelFileName,
        dlgSavePest.DefaultExt);
    end;
  end;

  dlgSavePest.FileName := PhastModel.FixFileName(FileName);
  if dlgSavePest.Execute then
  begin
    PhastModel.ExportPestInput(dlgSavePest.FileName, FRunPest);
  end;
//
end;

procedure TfrmGoPhast.acRunSutraExecute(Sender: TObject);
var
  Options: TSutraOptions;
begin
  inherited;
  if (PhastModel.SutraMesh = nil)
    or (PhastModel.SutraMesh.Mesh2D.Elements.Count = 0) then
  begin
    Beep;
    MessageDlg(StrYouMustGenerateTh, mtError, [mbOK], 0);
    Exit;
  end
  else if (PhastModel.SutraMesh.MeshType = mt3D)
    and (PhastModel.SutraMesh.Elements.Count = 0) then
  begin
    Beep;
    MessageDlg(StrYouMustGenerate3D, mtError, [mbOK], 0);
    Exit;
  end;
  Options := PhastModel.SutraOptions;
  if (Options.StartType = stWarm) then
  begin
    if not FileExists(Options.FullRestartFileName) then
    begin
      Beep;
      MessageDlg(StrNoRestartFile, mtWarning, [mbOK], 0);
      Exit;
    end;
  end;


  sdSutraInput.FileName := ChangeFileExt(PhastModel.ModelFileName, '.inp');
  if sdSutraInput.Execute then
  begin
    if (Options.StartType = stWarm) then
    begin
      if UpperCase(sdSutraInput.FileName) = UpperCase(Options.FullRestartFileName) then
      begin
        Beep;
        MessageDlg(StrTheRestartFileUse, mtWarning, [mbOK], 0);
        Exit;
      end;
    end;

    NilDisplay;
    ExportSutra(FRunSutra, sdSutraInput.FileName);
  end;
end;

procedure TfrmGoPhast.acRunSvdaPrepExecute(Sender: TObject);
begin
  inherited;
  if not PestVersionOK then
  begin
    Exit;
  end;

  CheckSvdaActivated;
  if PhastModel.ModelFileName <> '' then
  begin
    PhastModel.SvdaPrepProperties.FileName := ChangeFileExt(PhastModel.ModelFileName, '.pst')
  end;
  frmSvdaPrepInput := TfrmSvdaPrepInput.Create(nil);
  try
    frmSvdaPrepInput.ShowModal;
    if frmSvdaPrepInput.ModalResult = mrOK then
    begin
      PhastModel.ExportSvdaPrep;
    end;
  finally
    frmSvdaPrepInput.Free;
  end;
end;

procedure TfrmGoPhast.tb3DColorsClick(Sender: TObject);
begin
  acColoredGrid.Checked := not acColoredGrid.Checked;
  tb3DColors.Down := acColoredGrid.Checked;
  PhastModel.Grid.GridChanged;
end;

procedure TfrmGoPhast.miShowHideObjectsClick(Sender: TObject);
begin
  if frmShowHideObjects = nil then
  begin
    frmShowHideObjects := TfrmShowHideObjects.Create(nil);
  end;
  frmShowHideObjects.Show;
  if frmShowHideObjects.WindowState = wsMinimized then
  begin
    frmShowHideObjects.WindowState := wsNormal;
  end;
end;

procedure TfrmGoPhast.mi3D_ColorsClick(Sender: TObject);
begin
  frmColors.Show;
end;

procedure TfrmGoPhast.tb3DObjectsClick(Sender: TObject);
begin
  acShow3DObjects.Checked := not acShow3DObjects.Checked;
  tb3DObjects.Down := acShow3DObjects.Checked;
  Invalidate3DView(nil);
  UpdateDisplay(nil);
end;

procedure TfrmGoPhast.BeginSuppressDrawing;
begin
  Inc(FSupressDrawing);
end;

procedure TfrmGoPhast.bhntMeasureRulerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  APoint: TPoint;
begin
  APoint := RulerTool.Control.ScreenToClient(Mouse.CursorPos);
  RulerTool.MouseMove(RulerTool.Control, Shift, APoint.X, APoint.Y);
end;

procedure TfrmGoPhast.BringFormsToFront(Sender: TObject);
begin
  if (csDestroying in ComponentState)
    or (csDestroying in Application.ComponentState) then
  begin
    Exit;
  end;
  if frmProgressMM <> nil then
  begin
    if frmProgressMM.Visible then
    begin
      frmProgressMM.BringToFront;
    end;
  end;
//  if frmSelectedObjects <> nil then
//  begin
    if frmSelectedObjects.Visible then
    begin
      frmSelectedObjects.BringToFront;
    end;
//  end;
  if frmColors <> nil then
  begin
    if frmColors.Visible then
    begin
      frmColors.BringToFront;
    end;
  end;

end;

procedure TfrmGoPhast.tbDrawElementClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbDrawElement.OnMouseDown(tbDrawElement, mbLeft, [ssLeft], 0, 0);
  end;

  if tbDrawElement.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbDrawElement);
    // Set the cursors.
    SetZB_Cursors(crMoveNode);
    // don't draw a rectangle around the selected screen objects.
    CurrentTool := DrawElementTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.tbRotateCrossSectionClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbRotateCrossSection.OnMouseDown(tbRotateCrossSection, mbLeft, [ssLeft], 0, 0);
  end;

  if tbRotateCrossSection.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbRotateCrossSection);
    // Set the cursors.
    SetZB_Cursors(crArrow);
    if frameTopView <> nil then
    begin
      frameTopView.ZoomBox.Cursor := crRotate;
      frameTopView.ZoomBox.Image32.Cursor := crRotate;
    end;

    // Show a rectangle around the selected nodes.
//    frameTopView.UpdateSelectRectangle;
//    frameFrontView.UpdateSelectRectangle;
//    frameSideView.UpdateSelectRectangle;
    CurrentTool := RotateCrossSectionTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.tbCrossSectionClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbCrossSection.OnMouseDown(tbCrossSection, mbLeft, [ssLeft], 0, 0);
  end;

  if tbCrossSection.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbCrossSection);
    // Set the cursors.
    SetZB_Cursors(crMoveCrossSection);
    // Show a rectangle around the selected nodes.
//    frameTopView.UpdateSelectRectangle;
//    frameFrontView.UpdateSelectRectangle;
//    frameSideView.UpdateSelectRectangle;
    CurrentTool := EditCrossSectionTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.miShowHideBitmapsClick(Sender: TObject);
var
  Item: TCompressedBitmapItem;
begin
  if PhastModel.Bitmaps.Count > 0 then
  begin
    if PhastModel.Bitmaps.Count = 1 then
    begin
      Item := PhastModel.Bitmaps.Items[0] as
        TCompressedBitmapItem;
      Item.Visible := not Item.Visible;
    end
    else
    begin
      ShowAForm(TfrmShowHideBitmaps);
    end;
  end
  else
  begin
    MessageDlg(StrThisModelDoesntH, mtError, [mbOK], 0);
  end;
end;

procedure TfrmGoPhast.miSelectObjectsByNameClick(Sender: TObject);
begin
  with TfrmSelectObjects.Create(nil) do
  begin
    try
      if tabTop.TabVisible or tabFront.TabVisible or tabSide.TabVisible then
      begin
        ShowModal;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmGoPhast.miSelectObjectsforEditingClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmSelectObjectsForEditing)
end;

procedure TfrmGoPhast.SetCanDraw(const Value: boolean);
begin
  FCanDraw := Value;
  if frame3DView <> nil then
  begin
    frame3DView.glWidModelView.Visible := FCanDraw;
  end;
end;

procedure TfrmGoPhast.SetChangingSelection(const Value: boolean);
begin
  if FChangingSelection <> Value then
  begin
    FChangingSelection := Value;
    if not FChangingSelection then
    begin
      self.ScreenObjectSelectionChange(nil);
    end;
  end;
end;

procedure TfrmGoPhast.SetCreateArchive(const Value: Boolean);
begin
  FCreateArchive := Value;
  FCreateArchiveSet := True;
end;

procedure TfrmGoPhast.SetCurrentTool(const Value: TCustomInteractiveTool);
begin
  if CurrentTool <> Value then
  begin
    if CurrentTool <> nil then
    begin
      CurrentTool.Deactivate;
    end;
    FCurrentTool := Value;
    if CurrentTool <> nil then
    begin
      CurrentTool.Activate;
    end;
  end;
end;

procedure TfrmGoPhast.miImportPointsClick(Sender: TObject);
begin
  ShowAForm(TfrmImportPoints);
end;

procedure TfrmGoPhast.miEditSelectedObjectsClick(Sender: TObject);
begin
  EditScreenObjects;
end;

procedure TfrmGoPhast.OpenMostRecentlyUsed(Sender: TObject);
var
  Item: TRecentFileMenuItem;
  FileName: string;
begin
  Item := Sender as TRecentFileMenuItem;
  FileName := Item.FileName;
  // Item might be destroyed during CheckModel so get the file name before
  // calling CheckModel.
  if CheckModel then
  begin
    OpenAFile(FileName);
  end;
end;

resourcestring
  StrRegnerateMesh = 'You must first generate the mesh before it can be renumbered.';

procedure TfrmGoPhast.miRenumberMeshClick(Sender: TObject);
var
  Mesh: TSutraMesh3D;
  AForm: TfrmRenumberingMethod;
  OldBandWidth: Integer;
  NewBandWidth: Integer;
  Undo: TUndoRenumberMesh;
  RenumberAlg: TRenumberingAlgorithm;
begin
  inherited;
  Mesh := PhastModel.SutraMesh;
  if (Mesh = nil) or (Mesh.Mesh2D.Nodes.Count = 0) then
  begin
    Beep;
    MessageDlg(StrRegnerateMesh, mtWarning, [mbOK], 0);
    Exit;
  end;
//  RenumberMesh(Mesh.Mesh2D);
//  if Mesh.MeshType = mt3D then
//  begin
    AForm := TfrmRenumberingMethod.Create(nil);
    try
      AForm.rgMethod.ItemIndex := FRenumberMethod;
      if AForm.ShowModal = mrOK then
      begin
//        NewBandWidth := -1;
        Screen.Cursor := crHourGlass;
        try
          FRenumberMethod := AForm.rgMethod.ItemIndex;
          OldBandWidth := Mesh.Bandwidth;
          Assert(FRenumberMethod in [0,1]);
          Undo := nil;
          try
            RenumberAlg := TRenumberingAlgorithm(
              AForm.rgRenumberingMethod.ItemIndex);

            Undo := TUndoRenumberMesh.Create(RenumberAlg);
            case FRenumberMethod of
              0:
                begin
                  case RenumberAlg of
                    raNone: ;
                    CuthillMcKee:
                      begin
                        CuthillMcKeeRenumbering.RenumberMesh(Mesh.Mesh2D);
//                        if Mesh.MeshType = mt3D then
                        begin
                          Mesh.SimpleRenumber;
                        end;
                      end;
                    raSloanRandolph:
                      begin
                        MeshRenumbering.RenumberMesh(Mesh.Mesh2D);
//                        if Mesh.MeshType = mt3D then
                        begin
                          Mesh.SimpleRenumber;
                        end;
                      end
                    else Assert(False);
                  end;
                end;
              1:
                begin
                  Assert(Mesh.MeshType = mt3D);
                  case RenumberAlg of
                    raNone: ;
                    CuthillMcKee:
                      begin
                        CuthillMcKeeRenumbering.RenumberMesh(Mesh);
                        Mesh.SimpleRenumber;
                      end;
                    raSloanRandolph:
                      begin
                        MeshRenumbering.RenumberMesh(Mesh);
                        Mesh.SimpleRenumber;
                      end
                    else Assert(False);
                  end;
                end
              else Assert(False);
            end;
            Mesh.Mesh2D.Nodes.SortByNodeNumber;
            Mesh.Mesh2D.Elements.SortByElementNumber;
            Undo.UpdateNumbers;
            UndoStack.Submit(Undo);
          except
            Undo.Free;
            raise
          end;
          NewBandWidth := Mesh.Bandwidth;
        finally
          Screen.Cursor := crDefault;
          frameTopView.MagnificationChanged := True;
          frameFrontView.MagnificationChanged := True;
          InvalidateAllViews;
        end;

        MessageDlg(Format(StrTheBandwidthHasCh, [OldBandWidth, NewBandWidth]),
          mtInformation, [mbOK], 0);
      end;
    finally
      AForm.Free;
    end;
//  end;
end;

procedure TfrmGoPhast.miObservationsClick(Sender: TObject);
begin
  inherited;
  if miObservations.Checked then
  begin
    PhastModel.ObservationPurpose := ofObserved;
  end
  else
  begin
    PhastModel.ObservationPurpose := ofPredicted;
  end;
end;

procedure TfrmGoPhast.miOutputControlClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmModflowOutputControl);
end;

procedure TfrmGoPhast.miPackagesClick(Sender: TObject);
begin
  inherited;
  if frmModflowPackages = nil then
  begin
    Application.CreateForm(TfrmModflowPackages, frmModflowPackages);
  end;
  frmModflowPackages.GetData;
  frmModflowPackages.ShowModal;
end;

procedure TfrmGoPhast.miPathlinestoShapefileClick(Sender: TObject);
var
  FileName: string;
  AModel: TCustomModel;
begin
  inherited;
  FileName := ChangeFileExt(PhastModel.ModelFileName, '');
  if FileName = '' then
  begin
    FileName := IncludeTrailingPathDelimiter(GetCurrentDir) + StrPathlineshp;
  end
  else
  begin
    FileName := FileName + '_' + StrPathlineshp;
  end;
  sdShapefile.FileName := FileName;
  if sdShapefile.Execute then
  begin
    if FExportModpathShapeFileModelChoice > 0 then
    begin
      AModel := PhastModel.ChildModels[
        FExportModpathShapeFileModelChoice-1].ChildModel;
    end
    else
    begin
      AModel := PhastModel;
    end;
    AModel.PathLines.ExportShapefile(sdShapefile.FileName);
  end;
end;

procedure TfrmGoPhast.miExamplesClick(Sender: TObject);
begin
  ShowHelp('Examples', frmGoPhast.HelpFormat);
//  Application.HelpJump('Examples');
//  HelpRouter.HelpJump('', 'Examples');
end;

procedure TfrmGoPhast.miHelpOnMainWindowClick(Sender: TObject);
begin
  ShowHelp(HelpKeyword, frmGoPhast.HelpFormat);
//  HelpHandle := ShellExecute(HelpHandle, 'open', 'HH',
//    PChar(Application.HelpFile + '::/' + HelpKeyword + '.htm'),
//    {nil,} nil, SW_SHOWNORMAL);


//  Application.HelpJump(HelpKeyword);
//  HelpRouter.HelpJump('', HelpKeyword);
end;

procedure TfrmGoPhast.ScreenOnActiveFormChange(Sender: TObject);
// http://delphi.about.com/od/windowsshellapi/a/delphi-mouse-snap-to-default-dialog-button.htm
var
  af : TCustomForm;
  cp : TPoint;

  function SnapMouseToDialogDefaultButton : boolean;
  var r: Bool;
  begin
    result := SystemParametersInfo(SPI_GETSNAPTODEFBUTTON, 0, @r, 0) AND r;
  end;
begin
  af := Screen.ActiveCustomForm;
  if (af = nil) OR (af.ActiveControl = nil) then Exit;
  if (fsModal in af.FormState) AND af.ActiveControl.InheritsFrom(TButton) AND SnapMouseToDialogDefaultButton then
  begin
    cp := af.ActiveControl.ClientOrigin;
    Inc(cp.X, af.ActiveControl.ClientWidth div 2);
    Inc(cp.Y, af.ActiveControl.ClientHeight div 2);
    Mouse.CursorPos := cp;
  end;
end;

{ TMenuItemHint }

constructor TMenuItemHint.Create(AOwner: TComponent);
begin
  inherited;

  showTimer := TTimer.Create(self);
  showTimer.Interval := Application.HintPause;

  hideTimer := TTimer.Create(self);
  hideTimer.Interval := Application.HintHidePause;
end; (*Create*)

destructor TMenuItemHint.Destroy;
begin
  hideTimer.OnTimer := nil;
  showTimer.OnTimer := nil;
  self.ReleaseHandle;
  inherited;
end; (*Destroy*)

procedure TMenuItemHint.DoActivateHint(menuItem: TMenuItem);
begin
  //force remove of the "old" hint window
  hideTime(self);

  if (menuItem = nil) or (menuItem.Hint = '') then
  begin
    activeMenuItem := nil;
    Exit;
  end;

  activeMenuItem := menuItem;

  showTimer.OnTimer := ShowTime;
  hideTimer.OnTimer := HideTime;
end; (*DoActivateHint*)

procedure TMenuItemHint.ShowTime(Sender: TObject);
var
  r : TRect;
  wdth : integer;
  hght : integer;
begin
  if activeMenuItem <> nil then
  begin
    // RBW begin modification.
    Color := Application.HintColor;
    // RBW end modification

    //position and resize
    wdth := Canvas.TextWidth(activeMenuItem.Hint);
    hght := Canvas.TextHeight(activeMenuItem.Hint);

    r.Left := Mouse.CursorPos.X + 16;
    r.Top := Mouse.CursorPos.Y + 16;
    r.Right := r.Left + wdth + 6;
    r.Bottom := r.Top + hght + 4;

    ActivateHint(r,activeMenuItem.Hint);
  end;

  showTimer.OnTimer := nil;
end; (*ShowTime*)

procedure TMenuItemHint.HideTime(Sender: TObject);
begin
  //hide (destroy) hint window
  self.ReleaseHandle;
  hideTimer.OnTimer := nil;
end; (*HideTime*)

{ TPositionList }

procedure TPositionList.Clear;
begin
  FList.Clear;
  FCurrentPosition := 0;
end;

constructor TPositionList.Create(MaxPositions: integer);
begin
  Assert(MaxPositions > 0);
  FMaxPositions := MaxPositions;
  FCurrentPosition := 0;
  FList:= TObjectList.Create;
end;

destructor TPositionList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TPositionList.GetCanRedo: boolean;
begin
  result := FCurrentPosition < FList.Count;
end;

function TPositionList.GetCanUndo: boolean;
begin
  result := (FCurrentPosition > 1) and (FList.Count > 0);
end;

procedure TPositionList.Redo;
var
  APosition: TPositionStorage;
begin
  if CanRedo then
  begin
    APosition := FList[FCurrentPosition];
    Inc(FCurrentPosition);
    if Assigned(FOnNewPosition) then
    begin
      FOnNewPosition(self, APosition);
    end;
  end;
end;

procedure TPositionList.Submit(NewPosition: TPositionStorage);
var
  Index: Integer;
begin
  for Index := FList.Count - 1 downto FCurrentPosition do
  begin
    FList.Delete(Index);
  end;

  FList.Add(NewPosition);
  if FCurrentPosition = FMaxPositions then
  begin
    FList.Delete(0);
  end
  else
  begin
    Inc(FCurrentPosition);
  end;
  if Assigned(FOnNewPosition) then
  begin
    FOnNewPosition(self, NewPosition);
  end;
end;

procedure TPositionList.Undo;
var
  APosition: TPositionStorage;
begin
  if CanUndo then
  begin
    Dec(FCurrentPosition);
    APosition := FList[FCurrentPosition-1];
    if Assigned(FOnNewPosition) then
    begin
      FOnNewPosition(self, APosition);
    end;
  end;
end;

initialization
  // see also StrModflowDefaultPath etc in PhastModelUnit.pas
  Mf2005DateVersion1_12 := EncodeDate(2017,2,2);
  Mf2005Date := EncodeDate(2017,2,2);
  MfNwtDateVersion1_0_9 := EncodeDate(2014,6,23);
  MfNwtDateVersion1_1_0 := EncodeDate(2016,7,21);
  MfNwtDate := EncodeDate(2020,3,3);
  Modpath6Date := EncodeDate(2012,8,28);
  Modpath7Date := EncodeDate(2017,12,22);
  MfLgr2Date := EncodeDate(2013, 9, 19);
  MfOwhmDate := EncodeDate(2016, 6, 15);
  MfCfpDate := EncodeDate(2011, 2, 23);
  ModelMateDate := EncodeDate(2013, 11, 19);
  Mf6Date := EncodeDate(2021, 7, 30);
  Mt3dUsgsDate := EncodeDate(2019, 3, 8);
  ZoneBudMf6Date := Mf6Date;
  FootprintDate := EncodeDate(2018,3,27);
  PestDate := EncodeDate(2021,6,29);

  {$IFDEF Win64}
  RegisterExpectedMemoryLeak(GR32_Blend.AlphaTable);
  {$ENDIF}


end.

