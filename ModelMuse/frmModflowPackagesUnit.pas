{
  @name defines the form on which the user activates or deactivates
  MODFLOW packages.

  To add a new package, add a new page to
  @link(TfrmModflowPackages.jvplPackages)
  and put a @link(TFramePackage) or one of its descendants on it.
  Add additional controls if required.
  Add a new package to @link(TCustomModel.ModflowPackages
  frmGoPhast.PhastModel.ModflowPackages).
  Modfify @link(TfrmModflowPackages.GetData) and
  @link(TfrmModflowPackages.SetData) to use the new package and
  @link(TFramePackage). Often this can be done by just modifying
  @link(AddPackagesToList).
  Update @link(TModflowPackages.Assign),
  @link(TModflowPackages.Create), @link(TModflowPackages.Destroy),
  @link(TModflowPackages.Reset), and
  @link(TModflowPackages.SelectedModflowPackageCount).
}
unit frmModflowPackagesUnit;

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, JvPageList, JvExControls, ExtCtrls,
  JvExExtCtrls, JvNetscapeSplitter, ComCtrls, StdCtrls,
  framePackageUnit, Buttons, UndoItems, ModflowPackageSelectionUnit,
  ArgusDataEntry, framePcgUnit, ModflowPackagesUnit, JvSpin,
  RbwController, Grids, RbwDataGrid4, ModflowParameterUnit,
  ModflowTransientListParameterUnit, OrderedCollectionUnit,
  frameListParameterDefinitionUnit, frameArrayParameterDefinitionUnit,
  framePackageTransientLayerChoiceUnit, frameEtsPackageUnit, ImgList,
  framePackageResUnit, PhastModelUnit, GoPhastTypes,
  framePackageLAK_Unit, framePackageSFRUnit,
  framePackageLayerChoiceUnit, framePackageUZFUnit, frameGmgUnit, frameSipUnit,
  frameDe4Unit, JvExComCtrls, JvComCtrls, RequiredDataSetsUndoUnit,
  framePackageHobUnit, framePackageLpfUnit, frameModpathSelectionUnit,
  framePackageHufUnit, HufDefinition, framePackageMnw2Unit, framePackageSubUnit,
  frameZoneBudgetUnit, framePackageSwtUnit, framePkgHydmodUnit,
  framePackageRCHUnit, framePackageUpwUnit, framePackageNwtUnit,
  frameMt3dBasicPkgUnit, frameMt3dmsGcgPackageUnit, frameMt3dmsAdvPkgUnit,
  frameMt3dmsDispersionPkgUnit, Mt3dmsChemSpeciesUnit,
  frameMt3dmsChemReactionPkgUnit, frameMt3dmsTransObsPkgUnit, Mt3dmsTimesUnit,
  framePackagePcgnUnit, framePackageWellUnit, framePackageStrUnit,
  framePackageFrmUnit, frameRadioGridUnit, framePackageCFPUnit,
  framePackageSwiUnit, framePackageSwrUnit, framePackageMnw1Unit,
  framePackageNpfUnit, framePkgStoUnit, framePkgSmsUnit, framePackageRipUnit,
  framePackageSfrMF6Unit,
  System.ImageList,
  framePackageMawUnit,
  framePackageGNC_Unit, framePackageMf6ObsUnit, framePackageLakMf6Unit,
  framePackageMvrUnit, framePackageUzfMf6Unit, frameMt3dLktPkgUnit,
  frameMt3dSftUnit, frameMt3dCtsPkgUnit, framePackageCsubUnit,
  PestParamGroupsUnit, frameGwtDspPackageUnit, frameGwtAdvPackageUnit,
  framePackageMSTUnit, framePackageIstUnit, frameChemSpeciesUnit,
  System.Generics.Collections, framePackageFmiUnit, framePackageFmp4Unit,
  framePackageFmp4SoilsUnit, framePackageFmp4ClimateUnit,
  framePackageFmp4SurfaceWaterUnit, framePackageFmp4WellsUnit,
  framePackageFmp4AllotmentsUnit, framePackageFmp4LandUseUnit,
  framePackageFmp4SalinityFlushUnit;

type

  TTempPackageItem = class(TCollectionItem)
  private
    FPackages: TModflowPackages;
  public
    property Packages: TModflowPackages read FPackages;
    Constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
  end;

  TTempPackageCollection = class(TCollection)
  private
    function GetItem(Index: integer): TTempPackageItem;
    procedure SetItem(Index: integer; const Value: TTempPackageItem);
  public
    constructor Create;
    function Add: TTempPackageItem;
    property Items[Index: integer]: TTempPackageItem read GetItem
      write SetItem; default;
  end;

  TFrameNodeLink = class(TObject)
  private
    FNode: TTreeNode;
    FFrame: TframePackage;
    procedure SetNode(const Value: TTreeNode);
    procedure SetFrame(const Value: TframePackage);
  public
    AlternateNode: TTreeNode;
    property Node: TTreeNode read FNode write SetNode;
    property Frame: TframePackage read FFrame write SetFrame;
  end;

  TfrmModflowPackages = class(TfrmCustomGoPhast)
    tvPackages: TTreeView;
    JvNetscapeSplitter1: TJvNetscapeSplitter;
    jvplPackages: TJvPageList;
    jvspLPF: TJvStandardPage;
    jvspCHD: TJvStandardPage;
    framePkgCHD: TframePackage;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    jvspPCG: TJvStandardPage;
    framePCG: TframePCG;
    jvspGHB: TJvStandardPage;
    framePkgGHB: TframePackage;
    jvspWEL: TJvStandardPage;
    framePkgWEL: TframePackageWell;
    jvspRIV: TJvStandardPage;
    framePkgRIV: TframePackage;
    jvspDRN: TJvStandardPage;
    framePkgDRN: TframePackage;
    jvspDRT: TJvStandardPage;
    framePkgDRT: TframePackage;
    rbwLpfParamCountController: TRbwController;
    frameLpfParameterDefinition: TframeArrayParameterDefinition;
    tvLpfParameterTypes: TTreeView;
    splitLprParameter: TJvNetscapeSplitter;
    frameChdParameterDefinition: TframeListParameterDefinition;
    frameDrnParameterDefinition: TframeListParameterDefinition;
    frameDrtParameterDefinition: TframeListParameterDefinition;
    frameGhbParameterDefinition: TframeListParameterDefinition;
    frameRivParameterDefinition: TframeListParameterDefinition;
    frameWelParameterDefinition: TframeListParameterDefinition;
    jvspRCH: TJvStandardPage;
    jvspEVT: TJvStandardPage;
    framePkgEVT: TframePackageTransientLayerChoice;
    frameEvtParameterDefinition: TframeListParameterDefinition;
    framePkgRCH: TframePackageRCH;
    frameRchParameterDefinition: TframeListParameterDefinition;
    jvspETS: TJvStandardPage;
    framePkgETS: TframeEtsPackage;
    frameEtsParameterDefinition: TframeListParameterDefinition;
    ilCheckImages: TImageList;
    jvspRES: TJvStandardPage;
    framePkgRES: TframePackageRes;
    jvspLAK: TJvStandardPage;
    framePkgLAK: TframePackageLAK;
    jvspSFR: TJvStandardPage;
    frameSFRParameterDefinition: TframeListParameterDefinition;
    framePkgSFR: TframePackageSFR;
    pcSFR: TJvPageControl;
    tabSfrGeneral: TTabSheet;
    tabSfrParameters: TTabSheet;
    jplSfrParameters: TJvPageList;
    splitSFR: TSplitter;
    jvspUZF: TJvStandardPage;
    framePkgUZF: TframePackageUZF;
    jvspGMG: TJvStandardPage;
    framePkgGMG: TframeGMG;
    jvspSIP: TJvStandardPage;
    framePkgSIP: TframeSIP;
    jvspDE4: TJvStandardPage;
    framePkgDE4: TframeDE4;
    jvspHOB: TJvStandardPage;
    framePkgHOB: TframePackageHob;
    jvspHFB: TJvStandardPage;
    framePkgHFB: TframePackage;
    frameHfbParameterDefinition: TframeListParameterDefinition;
    framePkgLPF: TframePackageLpf;
    jvspModpath: TJvStandardPage;
    frameModpath: TframeModpathSelection;
    jvspCHOB: TJvStandardPage;
    framePkgCHOB: TframePackage;
    jvspDROB: TJvStandardPage;
    jvspGBOB: TJvStandardPage;
    jvspRVOB: TJvStandardPage;
    framePkgDROB: TframePackage;
    framePkgGBOB: TframePackage;
    framePkgRVOB: TframePackage;
    JvNetscapeSplitter3: TJvNetscapeSplitter;
    jvspHUF: TJvStandardPage;
    framePkgHuf: TframePackageHuf;
    tvHufParameterTypes: TTreeView;
    JvNetscapeSplitter4: TJvNetscapeSplitter;
    JvNetscapeSplitter5: TJvNetscapeSplitter;
    frameHufParameterDefinition: TframeListParameterDefinition;
    rbwHufParamCountController: TRbwController;
    jvspMNW2: TJvStandardPage;
    framePkgMnw2: TframePackageMnw2;
    jvspBCF: TJvStandardPage;
    framePkgBCF: TframePackage;
    jvspSUB: TJvStandardPage;
    framePkgSUB: TframePackageSub;
    jvspZoneBudget: TJvStandardPage;
    frameZoneBudget: TframeZoneBudget;
    jvspSWT: TJvStandardPage;
    framePkgSwt: TframePackageSwt;
    jvspHydmod: TJvStandardPage;
    framePkgHydmod: TframePkgHydmod;
    pnlLeft: TPanel;
    pnlModel: TPanel;
    comboModel: TComboBox;
    lblModel: TLabel;
    jvspUPW: TJvStandardPage;
    framePkgUPW: TframePackageUpw;
    JvNetscapeSplitter6: TJvNetscapeSplitter;
    jvspNWT: TJvStandardPage;
    framePkgNwt: TframePackageNwt;
    jvspMt3dmsBasic: TJvStandardPage;
    framePkgMt3dBasic: TframeMt3dBasicPkg;
    jvspMt3dmsGCG: TJvStandardPage;
    frameMt3dmsGcgPackage: TframeMt3dmsGcgPackage;
    jvspMt3dmsAdv: TJvStandardPage;
    frameMt3dmsAdvPkg: TframeMt3dmsAdvPkg;
    jvspMt3dmsDsp: TJvStandardPage;
    frameMt3dmsDispersionPkg: TframeMt3dmsDispersionPkg;
    jvspMt3dmsSsm: TJvStandardPage;
    framePkgSSM: TframePackage;
    jvspMt3dmsRct: TJvStandardPage;
    framePkgMt3dmsRct: TframeMt3dmsChemReactionPkg;
    jvspMt3dmsTOB: TJvStandardPage;
    framePkgMt3dmsTob: TframeMt3dmsTransObsPkg;
    jvspPCGN: TJvStandardPage;
    framePackagePcgn: TframePackagePcgn;
    jvspSTR: TJvStandardPage;
    framePkgStr: TframePackageStr;
    frameStrParameterDefinition: TframeListParameterDefinition;
    jvspSTOB: TJvStandardPage;
    framePkgSTOB: TframePackage;
    jvspFHB: TJvStandardPage;
    framePkgFHB: TframePackage;
    jvspFMP: TJvStandardPage;
    frameFmpParameterDefinition: TframeListParameterDefinition;
    framePkgFrm: TframePkgFarm;
    jvspCFP: TJvStandardPage;
    framePkgCFP: TframePackageCFP;
    jvspSWI: TJvStandardPage;
    framePackageSWI: TframePackageSWI;
    jvspSWR: TJvStandardPage;
    framePkgSWR: TframePackageSwr;
    jvspMNW1: TJvStandardPage;
    framePkgMnw1: TframePackageMnw1;
    jvspNPF: TJvStandardPage;
    framePkgNpf: TframePackageNpf;
    jvspSTO: TJvStandardPage;
    framePkgSto: TframePkgSto;
    jvspIMS: TJvStandardPage;
    framePkgIMS: TframePkgSms;
    jvspRIP: TJvStandardPage;
    framePkgRip: TframePackageRip;
    jvspMt3dUZT: TJvStandardPage;
    framePkgMt3dUZT: TframePackage;
    jvspSfrMf6: TJvStandardPage;
    framePackageSfrMF6: TframePackageSfrMF6;
    jvspMAW: TJvStandardPage;
    framePkgMAW: TframePackageMaw;
    jvspGNC: TJvStandardPage;
    framePkgGNC: TframePackageGNC;
    jvspMf6Obs: TJvStandardPage;
    framePackageMf6Obs: TframePackageMf6Obs;
    jvspLakMf6: TJvStandardPage;
    framePackageLakMf6: TframePackageLakMf6;
    jvspMVR: TJvStandardPage;
    framePkgMVR: TframePackageMvr;
    jvspUzfMf6: TJvStandardPage;
    framePackageUzfMf6: TframePackageUzfMf6;
    jvspMt3dLkt: TJvStandardPage;
    frameMt3dLktPkg: TframeMt3dLktPkg;
    jvspMt3dSft: TJvStandardPage;
    frameMt3dSftPkg: TframeMt3dSftPkg;
    jvspMt3dCts: TJvStandardPage;
    frameMt3dCtsPkg: TframeMt3dCtsPkg;
    jvspCSUB: TJvStandardPage;
    framePackageCsub: TframePackageCsub;
    jvspGwtDisp: TJvStandardPage;
    frameGwtDsp: TframeGwtDspPackage;
    jvspGwtAdv: TJvStandardPage;
    frameGwtAdv: TframeGwtAdvPackage;
    jvspGwtSsm: TJvStandardPage;
    frameGwtSSM: TframePackage;
    jvspGwtCNC: TJvStandardPage;
    frameGwtCNC: TframePackage;
    jvspGwtSRC: TJvStandardPage;
    frameGwtSRC: TframePackage;
    jvspGwtProcess: TJvStandardPage;
    frameGwtProcess: TframePackageFmi;
    jvspChemSpecies: TJvStandardPage;
    frameChemSpecies: TframeChemSpecies;
    TimerBringToFront: TTimer;
    jvspFMP4: TJvStandardPage;
    framePkgFMP4: TframePackageFmp4;
    jvspFmp4Soil: TJvStandardPage;
    framePkgFmp4Soils: TframePackageFmp4Soils;
    jvspFmp4Climate: TJvStandardPage;
    framePkgFmp4Climate: TframePackageFmp4Climate;
    jvspFmp4SurfaceWater: TJvStandardPage;
    framePkgFmp4SurfaceWater: TframePackageFmp4SurfaceWater;
    jvspFmp4SupplyWells: TJvStandardPage;
    framePkgFmp4Wells: TframePackageFmp4Wells;
    jvspFmp4Allotments: TJvStandardPage;
    framePkgFmp4Allotments: TframePackageFmp4Allotments;
    jvspFmp4LandUse: TJvStandardPage;
    framePkgFmp4LandUse: TframePackageFmp4LandUse;
    jvspFmp4SalinityFlush: TJvStandardPage;
    framePkgFmp4SalinityFlush: TframePackageFmp4SalinityFlush;
    dlgOpenSelectExternalFile: TOpenDialog;
    procedure tvPackagesChange(Sender: TObject; Node: TTreeNode);
    procedure btnOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
    procedure tvLpfParameterTypesChange(Sender: TObject; Node: TTreeNode);
    procedure frameParameterDefinition_seNumberOfParametersChange(
      Sender: TObject);
    procedure frameParameterDefinition_btnDeleteClick(Sender: TObject);
    procedure jvplPackagesChange(Sender: TObject);
    procedure tvPackagesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure framePkgSFRrcSelectionControllerEnabledChange(Sender: TObject);
    procedure framePkgSFRcbSfrUnsatflowClick(Sender: TObject);
    procedure framePkgSFRcbSfrLpfHydraulicCondClick(Sender: TObject);
    procedure framePkgSFRrgSfr2ISFROPTClick(Sender: TObject);
    procedure frameSFRParameterDefinitionseNumberOfParametersChange(
      Sender: TObject);
    procedure frameSFRParameterDefinitionbtnDeleteClick(Sender: TObject);
    procedure frameSFRParameterDefinitiondgParametersSelectCell(Sender: TObject;
      ACol, ARow: Integer; var CanSelect: Boolean);
    procedure frameSFRParameterDefinitiondgParametersSetEditText(
      Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure FormResize(Sender: TObject);
    procedure frameModpathrcSelectionControllerEnabledChange(Sender: TObject);
    procedure framePkgEVTrcSelectionControllerEnabledChange(Sender: TObject);
    procedure framePkgRCHrcSelectionControllerEnabledChange(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure tvHufParameterTypesChange(Sender: TObject; Node: TTreeNode);
    procedure framePkgHufrcSelectionControllerEnabledChange(Sender: TObject);
    procedure framePkgUZFrcSelectionControllerEnabledChange(Sender: TObject);
    procedure framePkgBCFrcSelectionControllerEnabledChange(Sender: TObject);
    procedure comboModelChange(Sender: TObject);
    procedure framePkgLPFrcSelectionControllerEnabledChange(Sender: TObject);
    procedure framePkgUPWrcSelectionControllerEnabledChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure framePkgNwtpcNWTChange(Sender: TObject);
    procedure framePkgNwtrcSelectionControllerEnabledChange(Sender: TObject);
    procedure framePkgFrmrcSelectionControllerEnabledChange(Sender: TObject);
    procedure tvPackagesCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure frameCropConsumptiveUserdgGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure framePkgFrmjvplFarmChange(Sender: TObject);
    procedure framePkgCFPrcSelectionControllerEnabledChange(Sender: TObject);
    procedure framePkgCFPcbPipesClick(Sender: TObject);
    procedure framePkgSWRjvplSwrChange(Sender: TObject);
    procedure tvPackagesExpanded(Sender: TObject; Node: TTreeNode);
    procedure framePkgSFRcbSeepageLossClick(Sender: TObject);
    procedure framePkgMt3dBasiccomboVersionChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure jvspGMGShow(Sender: TObject);
    procedure jvspSUBShow(Sender: TObject);
    procedure jvspSWTShow(Sender: TObject);
    procedure jvspSWRShow(Sender: TObject);
    procedure frameZoneBudgetrcSelectionControllerEnabledChange(
      Sender: TObject);
    procedure framePkgLAKrcSelectionControllerEnabledChange(Sender: TObject);
    procedure framePkgMt3dBasicrcSelectionControllerEnabledChange(
      Sender: TObject);
    procedure framePackageUzfMf6rcSelectionControllerEnabledChange(
      Sender: TObject);
    procedure framePackageLakMf6rcSelectionControllerEnabledChange(
      Sender: TObject);
    procedure framePackageSfrMF6rcSelectionControllerEnabledChange(
      Sender: TObject);
    procedure framePkgSMSrcSelectionControllerEnabledChange(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure frameGridMobileseNumberChange(Sender: TObject);
    procedure frameGridImmobileseNumberChange(Sender: TObject);
    procedure frameGridMobileGridExit(Sender: TObject);
    procedure frameGridImmobileGridExit(Sender: TObject);
    procedure framePkgMt3dmsRctcomboKineticChoiceChange(Sender: TObject);
    procedure jvspNPFShow(Sender: TObject);
    procedure tvPackagesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure frameGridMobileGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure EnableMt3dTimeControls(Sender: TObject);
    procedure frameGwtProcessrcSelectionControllerEnabledChange(
      Sender: TObject);
    procedure frameGridMobileGridStateChange(Sender: TObject; ACol,
      ARow: Integer; const Value: TCheckBoxState);
    procedure frameGridMobileGridButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure frameGridImmobileGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure TimerBringToFrontTimer(Sender: TObject);
    procedure frameGridMobileGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure OwhmFrameButtonClick(Sender: TObject; ACol,
        ARow: Integer);
    procedure framePkgFmp4SoilsrdgSoilsSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
  private
    IsLoaded: boolean;
    CurrentParameterType: TParameterType;
    FSteadyParameters: TModflowSteadyParameters;
    FHufParameters: THufModflowParameters;
    FTransientListParameters: TModflowTransientListParameters;
    FPackageList: TList;
    FSfrParameterInstances: TSfrParamInstances;
    FNewPackages: TTempPackageCollection;
    FCurrentPackages: TModflowPackages;
    FFrameNodeLinks: TList;
    FLinkDictionary: TDictionary<TframePackage, TFrameNodeLink>;
    FSettingNumber: Boolean;
    FGwtParentNode: TTreeNode;
    FMstNode: TTreeNode;
    FframePackageMSTObjectList: TframePackageMSTObjectList;
    FIstNode: TTreeNode;
    FframePackageIstObjectList: TframePackageIstObjectList;
    FframePkgSmsObjectList: TframePkgSmsObjectList;
    FGwtImsNode: TTreeNode;
    FTransportNode: TTreeNode;
    FChemNode: TTreeNode;
    procedure AssignParameterToRow(ActiveGrid: TRbwDataGrid4; RowIndex: Integer;
      Parameter: TModflowParameter);
    procedure SetData;
    function NewParameterName: string;
    procedure FillLpfTree;
    function ParentFrame(Sender: TObject): TframeListParameterDefinition;
    procedure FillTransientGrids;
    procedure AddPackagesToList(Packages: TModflowPackages);
    procedure framePkgLPFSelectedChange(Sender: TObject);
    procedure EnableSfrParameters;
    procedure GetSfrParamInstances;
    procedure SetSfrParamInstances;
    procedure AdjustDroppedWidth(OwnerComponent: TComponent);
    // @seealso(StorePackages);
    procedure ReadPackages;
    procedure SetpcSFR_ClientBorderWidth;
    procedure CheckLpfParameters;
    function PackageUsed(const ID: string): boolean;
    procedure CheckSfrParameterInstances;
    procedure FillHfbGrid;
    procedure InitializeFrame(Frame: TframeListParameterDefinition);
    procedure EnableEvtModpathOption;
    procedure EnableRchModpathOption;
    procedure ChdSelectedChange(Sender: TObject);
    procedure DrnSelectedChange(Sender: TObject);
    procedure GhbSelectedChange(Sender: TObject);
    procedure RivSelectedChange(Sender: TObject);
    procedure FillHufTree;
    procedure UpdateFlowParamGrid(Node: TTreeNode;
      ParameterFrame: TframeListParameterDefinition;
      ParameterCollection: TCollection;
      ParameterFrameController: TRbwController);
    procedure ActivateHufReferenceChoice;
    procedure EnableUzfVerticalKSource;
    // @seealso(ReadPackages);
    procedure StorePackages;
    procedure SetCurrentPackages(const Value: TModflowPackages);
    procedure NwtSelectedChange(Sender: TObject);
    procedure UpwSelectedChange(Sender: TObject);
    procedure Mt3dmsGcgSelectedChange(Sender: TObject);
    procedure Fmp4SelectedChange(Sender: TObject);
    procedure StrSelectedChange(Sender: TObject);
    function DuplicateParameterNames: boolean;
    function NodeToFrame(Node: TTreeNode): TFramePackage;
    function SelectNodeOfChildSelectedPackage(Node: TTreeNode): Boolean;
    procedure UzfSelectedChange(Sender: TObject);
    function AreParameterZonesOK: Boolean;
    procedure CheckSmsLinearSolver;
    function CheckGmgNwt: Boolean;
    function PackageNodeChecked(const ID: string): boolean;
    procedure CheckXt3dGnc;
    procedure CheckIPHDRY;
    procedure UpdateSpeciesNames;
    function ValidModpathChoice: Boolean;
    function ValidMT3D: Boolean;
    property CurrentPackages: TModflowPackages read FCurrentPackages
      write SetCurrentPackages;
    procedure StorePackageDataInFrames(Packages: TModflowPackages);
    procedure StoreFrameDataInPackages(Packages: TModflowPackages);
    procedure EnableLpfParameterControls;
    procedure Mt3dmsBasicSelectedChange(Sender: TObject);
    procedure CheckMt3dChemSpeciesDefined;
    procedure EnableFarmPrintRouting;
    procedure EnableConduitRecharge;
    procedure NilNodes;
    procedure EnableUnsatTransport;
    procedure EnableLakeTransport;
    procedure EnableStreamTransport;
    procedure EnableContaminantTreatmentSystem;
    procedure EnableChemSpecies;
    procedure UpdateGwtFrames;
    function CreateMstFrame: TframePackageMST;
    function CreateIstFrame: TframePackageIst;
    function CreateImsGwtFrame: TframePkgSms;
    procedure ShowImsPage(Sender: TObject);
    procedure EnableGwtPackages;
    function CheckMt3dSaturation: Boolean;
    function CheckGwtSaturation: Boolean;
//    function CheckMf6LakeOutlet: Boolean;
    { Private declarations }
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetData;
    { Public declarations }
  end;

  // @name is used to reversibly change which packages are selected and
  // the properties of those packages.
  TUndoChangeLgrPackageSelection = class(TCustomUndoChangeParameters)
  private
    FOldPackages: TTempPackageCollection;
    FNewPackages: TTempPackageCollection;
    FOldHydroGeologicUnits: THydrogeologicUnits;
    FOldInterBlockTransmissivity: array of integer;
    FOldAquiferType: array of integer;
    FOldMobileComponents: TMobileChemSpeciesCollection;
    FNewMobileComponents: TMobileChemSpeciesCollection;
    FOldImmobileComponents: TChemSpeciesCollection;
    FNewImmobileComponents: TChemSpeciesCollection;
    FOldMt3dTimes: TMt3dmsTimeCollection;
    FComponentsSame: Boolean;
    procedure UpdateInterbedsInObjects;
    procedure UpdateLayerGroupProperties(BcfPackage: TModflowPackageSelection);
    procedure RecreateMt3dTimeLists;
    procedure SetMt3dCaption;
    procedure InvalidateActiveGrid;
  protected
    function Description: string; override;
  public
    Constructor Create(var NewSteadyParameters: TModflowSteadyParameters;
      var NewTransientParameters: TModflowTransientListParameters;
      var SfrParameterInstances: TSfrParamInstances;
      var NewHufModflowParameters: THufModflowParameters;
      var NewParamGroups: TPestParamGroups;
      var NewPackages: TTempPackageCollection);
    Destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
    procedure UpdateMt3dmsChemSpecies;
  end;

var
  frmModflowPackages: TfrmModflowPackages = nil;

implementation

uses Contnrs, JvListComb, frmGoPhastUnit, ScreenObjectUnit,

  frameSfrParamInstancesUnit, LayerStructureUnit, frmErrorsAndWarningsUnit,
  frmManageFluxObservationsUnit, Mt3dmsChemUnit,
  ModflowTimeUnit, ModflowDiscretizationWriterUnit, Mt3dUztRchUnit,
  Mt3dUztSatEtUnit, Mt3dUztUnsatEtUnit, Mt3dUzfSeepageUnit, Mt3dSftUnit,
  ModflowCsubUnit, System.Math, AbstractGridUnit,
  frmDisplayDataUnit;

resourcestring
  StrLPFParameters = 'LPF or NWT Parameters';
  rsChangePackages = 'change packages';
  StrOneOrMoreVKCBPar = 'One or more VKCB parameters are defined in the LPF ' +
  'or NWT package but they won''t be used because all the layers are simulat' +
  'ed.';
  StrOneOrMoreVKParam = 'One or more VK parameters are defined in the LPF or' +
  ' NWT package but they won''t be used because there is only one layer in t' +
  'he model.';
  StrOneOrMoreVKParam2 = 'One or more VK parameters are defined in the LPF or' +
  ' NWT package but they won''t be used because vertical anisotropy is used ' +
  'for all the layers. Check the MODFLOW Layers dialog box if you want to us' +
  'e vertical anisotropy.';
  StrOneOrMoreVANIPar = 'One or more VANI parameters are defined in the LPF ' +
  'or NWT package but they won''t be used because there is only one layer in' +
  ' the model.';
  StrOneOrMoreVANIPar2 = 'One or more VANI parameters are defined in the LPF' +
  ' or NWT package but they won''t be used because vertical hydraulic hydrau' +
  'lic conductivity is used for all the layers. Check the MODFLOW Layers dia' +
  'log box if you want to use vertical anisotropy.';
  StrOneOrMoreSSParam = 'One or more SS parameters are defined in the LPF or' +
  ' NWT package but they won''t be used because there are no transient stres' +
  's periods in the model.';
  StrOneOrMoreSYParam = 'One or more SY parameters are defined in the LPF or' +
  ' NWT package but they won''t be used because there are no transient stres' +
  's periods in the model.';
  StrOneOrMoreSYParam2 = 'One or more SY parameters are defined in the LPF o' +
  'r NWT package but they won''t be used because all of the layers are confi' +
  'ned.';
  StrHKHorizontalHydra = 'HK (horizontal hydraulic conductivity)';
  StrHANIHorizontalAni = 'HANI (horizontal anisotropy)';
  StrVKVerticalHydraul = 'VK (vertical hydraulic conductivity)';
  StrVANIVerticalAniso = 'VANI (vertical anisotropy)';
  StrSSSpecificStorage = 'SS (specific storage)';
  StrSYSpecificYield = 'SY (specific yield)';
  StrSYTPStorageCoeffi = 'SYTP (storage coefficient for the top active cell)';
  StrKDEPHydraulicCond = 'KDEP (hydraulic conductivity depth-dependence coef' +
  'ficient)';
  StrLVDAHorizontalAni = 'LVDA (horizontal anisotropy angle)';
  StrVKCBVerticalHydra = 'VKCB (vertical hydraulic conductivity of confining' +
  ' layer)';
  StrSFRParameters = 'SFR Parameters';
  StrTheSFRParameterNa = 'The SFR parameter named "%s" can''t be used becaus' +
  'e no parameter instances are defined for it.';
  StrNoChemicalSpecies = 'No chemical species defined';
  StrMT3DMSIsActiveBut = 'MT3DMS is active but no chemical species have been' +
  ' defined.';
  StrInOrderToGenerate = 'In order to generate the flow-transport link file ' +
  'required by MT3D, you will need to generate the MODFLOW input files and' +
  ' run MODFLOW again.';
  FormatStr = 'Number of %s parameters';
  StrSWasNotFound = '%s was not found.';
//  StrYouWillNeedToRun = 'You will need to run MODFLOW again one time before ' +
//  'running MODPATH.';
  StrFarmProcess = 'Farm Process';
  StrConduitFlowProcess = 'Conduit Flow Process';
  StrMODFLOWDoesNotAll = 'MODFLOW does not allow the MNW1 and MNW2 packages ' +
  'to both be used in the same model.';
  StrYouWillNeedToDef = 'You will need to define at least one transient stre' +
  'ss period for the storage properties to be used.';
  StrDuplicateParameter = 'Duplicate parameter names are not allowed even if' +
  ' they are in different packages. Please correct the following duplicate p' +
  'arameter names. '#13#10'%0:s';
  StrAtLeastTwoOfYour = 'At least two of your MODFLOW parameters are of the ' +
  'same type and for at least one of them, no zones are used. This is usuall' +
  'y a mistake. Parameters with no zones will apply to all cells in the mode' +
  'l. You can use zones to restrict a parameter to particular cells. Do you ' +
  'want to fix this before continuing?';
  StrTheGMGPackageIsN = 'The GMG package is not included in MODFLOW-NWT. Do ' +
  'you want to choose a different solver?';
  StrYouCanNotUseTheGnc = 'You can not use the GNC package and the XT3D opti' +
  'on in the NPF package in the same model. You should fix this before ' +
  'attempting to run MODFLOW.';
  StrBecauseYouAreUsin = 'Because you are using the Upstream Weighting packa' +
  'ge (UPW) along with one or more observation packages. You should disable ' +
  'the option in the UPW package to print HDRY in dry cells.';
  StrBecauseYouAreChan = 'Because you are changing the THICKSTRT option in t' +
  'he NPF package, you may wish to change the default formula for the Confin' +
  'ed data set too. When the THICKSTART option is used, an appropriate formu' +
  'la would be ' + slinebreak + '"CellType <> 0)".' + slinebreak +
  'When it is not used, an appropriate formu' +
  'la would be ' + slinebreak + '"CellType > 0)".';
  StrMonodAndFirstorde = 'Monod and first-order chain reactions are only ava' +
  'ilable with MT3D-USGS. You can change your selected version of MT3D in th' +
  'e MT3D Basic package.';
  StrFirstorderChainRe = 'First-order chain reactions require at least two c' +
  'hemical species in the MT3D Basic package.';
  StrMT3DCanOnlyBeUse = 'MT3D can only be used with structured grids.';
  StrMSTMobileStorage = 'MST: Mobile Storage and Transfer Packages';
  StrISTImmobileStorag = 'IST: Immobile Storage and Transfer Packages';
  StrImsGWT = 'IMS-GWT: Iterative Model Sovler Packages';
  StrIMSIterativeModel = 'IMS: Iterative Model Solver Packages';
  StrGroundwaterTranspor = 'Groundwater Transport';
  StrChemSpecies = 'Chem Species';
  StrChemSpeciesMT3DA = 'Chem Species (MT3D and GWT)';
  StrTheSaveSaturation = 'The Save Saturation option in the NPF package is i' +
  'ncompatible with MT3D. Do you want to fix this?';
  StrTheSaveSaturationGwt = 'The Save Saturation option in the NPF package i' +
  's required with GWT when the transport simulation is separate from the fl' +
  'ow simulation. Do you want to fix this?';
//  StrSurfaceWaterRouting = 'Surface-Water Routing';

{$R *.dfm}

type
  TParameterColumns = (pcName, pcValue, pcUseZone, pcUseMultiplier);

procedure TfrmModflowPackages.AssignParameterToRow(ActiveGrid: TRbwDataGrid4;
  RowIndex: Integer; Parameter: TModflowParameter);
begin
  ActiveGrid.Objects[0, RowIndex] := Parameter;
  ActiveGrid.Cells[Ord(pcName), RowIndex] := Parameter.ParameterName;
  ActiveGrid.Cells[Ord(pcValue), RowIndex] := FloatToStr(Parameter.Value);
  if (Parameter is TModflowSteadyParameter)
    and not (Parameter.ParameterType = ptHFB) then
  begin
    ActiveGrid.Checked[Ord(pcUseZone), RowIndex] :=
      TModflowSteadyParameter(Parameter).UseZone;
    ActiveGrid.Checked[Ord(pcUseMultiplier), RowIndex] :=
      TModflowSteadyParameter(Parameter).UseMultiplier;
  end;
end;

function TfrmModflowPackages.PackageUsed(const ID: string): boolean;
var
  PackageIndex : Integer;
  Package: TModflowPackageSelection;
begin
  result := false;
  for PackageIndex := 0 to FPackageList.Count - 1 do
  begin
    Package := FPackageList[PackageIndex];
    if Package.PackageIdentifier = ID then
    begin
      result := Package.IsSelected;
      Exit;
    end;
  end;
end;

function TfrmModflowPackages.PackageNodeChecked(const ID: string): boolean;
var
  PackageIndex : Integer;
  Package: TModflowPackageSelection;
  Node: TTreeNode;
  Frame: TframePackage;
begin
  result := false;
  for PackageIndex := 0 to FPackageList.Count - 1 do
  begin
    Package := FPackageList[PackageIndex];
    if Package.PackageIdentifier = ID then
    begin
      Node := Package.Node;
      Frame := NodeToFrame(Node);
      if Frame <> nil then
      begin
        result := Frame.Selected;
      end;
    end;
  end;
end;

procedure TfrmModflowPackages.CheckSfrParameterInstances;
var
  PageIndex: Integer;
  Page: TJvCustomPage;
  Frame: TframeSfrParamInstances;
  ParameterName: string;
  InstanceIndex: Integer;
  InstanceName: string;
  StartTime: double;
  EndTime: double;
  InvalidParameterNames: TStringList;
  IsInvalid: boolean;
  ParamNameIndex: Integer;
begin
  // Don't check SFR parameters if the SFR package isn't used.
  if not PackageUsed(StrSFR_Identifier) then Exit;
  // Don't check SFR parameters if SFR parameters can't be used.
  if framePkgSFR.CalculateISFROPT <> 0 then Exit;

  InvalidParameterNames := TStringList.Create;
  try
    for PageIndex := 0 to jplSfrParameters.PageCount - 1 do
    begin
      Page := jplSfrParameters.Pages[PageIndex];
      Assert(Page.ControlCount= 1);
      Page.Handle;
      Frame := Page.Controls[0] as TframeSfrParamInstances;
      ParameterName := frameSFRParameterDefinition.dgParameters.Cells[
        Ord(pcName), PageIndex+1];
      if ParameterName <> '' then
      begin
        IsInvalid := True;
        for InstanceIndex := 1 to Frame.seInstanceCount.AsInteger do
        begin
          InstanceName := Frame.rdgSfrParamInstances.Cells[
            Ord(sicInstanceName), InstanceIndex];

          if (InstanceName <> '')
            and TryStrToFloat(Frame.rdgSfrParamInstances.
              Cells[Ord(sicStartTime), InstanceIndex], StartTime)
            and TryStrToFloat(Frame.rdgSfrParamInstances.
              Cells[Ord(sicEndTime), InstanceIndex], EndTime) then
          begin
            IsInvalid := False;
            break;
          end;
        end;
        if IsInvalid then
        begin
          InvalidParameterNames.Add(ParameterName);
        end;
      end;
    end;

    frmErrorsAndWarnings.RemoveWarningGroup(frmGoPhast.PhastModel,
      StrSFRParameters);
    if InvalidParameterNames.Count > 0 then
    begin
      for ParamNameIndex := 0 to InvalidParameterNames.Count - 1 do
      begin
        frmErrorsAndWarnings.AddWarning(frmGoPhast.PhastModel, StrSFRParameters,
          Format(StrTheSFRParameterNa, [InvalidParameterNames[ParamNameIndex]]));
      end;
      frmErrorsAndWarnings.Show;
    end;
  finally
    InvalidParameterNames.Free;
  end;
end;

procedure TfrmModflowPackages.comboModelChange(Sender: TObject);
begin
  inherited;
  if comboModel.ItemIndex < 0 then
  begin
    CurrentPackages := nil;
  end
  else
  begin
    CurrentPackages := comboModel.Items.Objects[
      comboModel.ItemIndex] as TModflowPackages;
  end;
end;

constructor TfrmModflowPackages.Create(AOwner: TComponent);
begin
  inherited;
  FframePackageMSTObjectList := TframePackageMSTObjectList.Create;
  FframePackageIstObjectList := TframePackageIstObjectList.Create;
  FframePkgSmsObjectList := TframePkgSmsObjectList.Create;
end;

function TfrmModflowPackages.CreateImsGwtFrame: TframePkgSms;
var
  NewPage: TJvStandardPage;
  MemoWidth: Integer;
begin
  result := TframePkgSms.Create(nil);
  result.Selected := True;
  FframePkgSmsObjectList.Add(result);
  NewPage := TJvStandardPage.Create(self);
  NewPage.Name := '';
  NewPage.PageList := jvplPackages;
  NewPage.OnShow := ShowImsPage;
  result.Parent := NewPage;
  MemoWidth := result.MemoComments.Width;
  result.Align := alClient;
  // If this isn't done, the memo may be invisible because its width
  // ends up less than zero.
  // The page control is handled in ShowImsPage for the same reason.
  if result.Width <= 0 then
  begin
    result.MemoComments.Width := MemoWidth;
  end;
end;

function TfrmModflowPackages.CreateIstFrame: TframePackageIst;
var
  NewPage: TJvStandardPage;
  MemoWidth: Integer;
begin
  result := TframePackageIst.Create(nil);
  FframePackageISTObjectList.Add(result);
  NewPage := TJvStandardPage.Create(self);
  NewPage.Name := '';
  NewPage.PageList := jvplPackages;
  NewPage.HelpKeyword := 'IST-Immobile-Storage-and-Trasf';
  result.Parent := NewPage;
  MemoWidth := result.MemoComments.Width;
  result.Align := alClient;
  // If this isn't done, the memo may be invisible because its width
  // ends up less than zero.
  if result.Width <= 0 then
  begin
    result.MemoComments.Width := MemoWidth;
  end;
end;

procedure TfrmModflowPackages.CheckLpfParameters;
var
  ParamIndex: Integer;
  Param: TModflowSteadyParameter;
  VKCB_Defined: boolean;
  VK_Defined: boolean;
  VANI_Defined: boolean;
  SS_Defined: boolean;
  SY_Defined: boolean;
  Quasi3dUsed: boolean;
  VerticalHydraulicConductivityUsed: boolean;
  VerticalAnisotropyUsed: boolean;
  UnitIndex: Integer;
  LayerGroup: TLayerGroup;
  ShowErrors: boolean;
  SpecificStorageUsed: boolean;
  SpecificYieldUsed: boolean;
begin
  if not PackageUsed(StrLPF_Identifier)
    and not PackageUsed(StrUPW_Identifier) then
  begin
    Exit;
  end;

  VKCB_Defined := False;
  VK_Defined := False;
  VANI_Defined := False;
  SS_Defined := False;
  SY_Defined := False;
  for ParamIndex := 0 to FSteadyParameters.Count - 1 do
  begin
    Param := FSteadyParameters[ParamIndex];
    case Param.ParameterType of
      ptLPF_VK: VK_Defined := True;
      ptLPF_VANI: VANI_Defined := True;
      ptLPF_VKCB: VKCB_Defined := True;
      ptLPF_SS: SS_Defined := True;
      ptLPF_SY: SY_Defined := True;
    end;
  end;

  Quasi3dUsed := False;
  VerticalHydraulicConductivityUsed := False;
  VerticalAnisotropyUsed := False;
  if frmGoPhast.PhastModel.ModflowLayerCount > 1 then
  begin
    // Skip the top of the model: it doesn't count.
    for UnitIndex := 1 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
    begin
      LayerGroup := frmGoPhast.PhastModel.LayerStructure[UnitIndex];
      if LayerGroup.RunTimeSimulated then
      begin
        if LayerGroup.VerticalHydraulicConductivityMethod = 0 then
        begin
          VerticalHydraulicConductivityUsed := True;
        end
        else
        begin
          VerticalAnisotropyUsed := True;
        end;
      end
      else
      begin
        Quasi3dUsed := True;
      end;
    end;
  end;

  frmErrorsAndWarnings.RemoveWarningGroup(frmGoPhast.PhastModel, StrLPFParameters);
  SpecificStorageUsed := False;
  SpecificYieldUsed := False;
  if frmGoPhast.PhastModel.ModflowStressPeriods.TransientModel then
  begin
    SpecificStorageUsed := True;
// Skip the top of the model: it doesn't count.
    for UnitIndex := 1 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
    begin
      LayerGroup := frmGoPhast.PhastModel.LayerStructure[UnitIndex];
      if LayerGroup.RunTimeSimulated then
      begin
        if LayerGroup.AquiferType <> 0 then
        begin
          SpecificYieldUsed := True;
        end
      end;
    end;
  end;

  ShowErrors := False;
  if VKCB_Defined and not Quasi3dUsed then
  begin
    ShowErrors := True;
    frmErrorsAndWarnings.AddWarning(frmGoPhast.PhastModel, StrLPFParameters,
      StrOneOrMoreVKCBPar);
  end;

  if VK_Defined and not VerticalHydraulicConductivityUsed then
  begin
    ShowErrors := True;
    if frmGoPhast.PhastModel.ModflowLayerCount = 1 then
    begin
      frmErrorsAndWarnings.AddWarning(frmGoPhast.PhastModel, StrLPFParameters,
        StrOneOrMoreVKParam);
    end
    else
    begin
      frmErrorsAndWarnings.AddWarning(frmGoPhast.PhastModel, StrLPFParameters,
        StrOneOrMoreVKParam2);
    end;
  end;

  if VANI_Defined and not VerticalAnisotropyUsed then
  begin
    ShowErrors := True;
    if frmGoPhast.PhastModel.ModflowLayerCount = 1 then
    begin
      frmErrorsAndWarnings.AddWarning(frmGoPhast.PhastModel, StrLPFParameters,
        StrOneOrMoreVANIPar);
    end
    else
    begin
      frmErrorsAndWarnings.AddWarning(frmGoPhast.PhastModel, StrLPFParameters,
        StrOneOrMoreVANIPar2);
    end;
  end;

  if SS_Defined and not SpecificStorageUsed then
  begin
    ShowErrors := True;
    frmErrorsAndWarnings.AddWarning(frmGoPhast.PhastModel, StrLPFParameters,
      StrOneOrMoreSSParam);
  end;

  if SY_Defined and not SpecificYieldUsed then
  begin
    ShowErrors := True;
    if not frmGoPhast.PhastModel.ModflowStressPeriods.TransientModel then
    begin
      frmErrorsAndWarnings.AddWarning(frmGoPhast.PhastModel, StrLPFParameters,
        StrOneOrMoreSYParam);
    end
    else
    begin
      frmErrorsAndWarnings.AddWarning(frmGoPhast.PhastModel, StrLPFParameters,
        StrOneOrMoreSYParam2);
    end;
  end;

  if ShowErrors then
  begin
    frmErrorsAndWarnings.Show;
  end;

end;

//function TfrmModflowPackages.CheckMf6LakeOutlet: Boolean;
//var
//  ProblemOutlet: Integer;
//begin
//  result := True;
//  ProblemOutlet := -1;
//  if (frmGoPhast.ModelSelection = msModflow2015)
//    and framePackageLakMf6.Selected
//    and not framePackageLakMf6.LakeOutletsDefined(ProblemOutlet)
//    and not framePkgMVR.Selected then
//  begin
//    if (MessageDlg(Format(StrNoOutletLakeIsSp, [ProblemOutlet]), mtWarning, [mbYes, mbNo], 0)
//      in [mrNo, mrNone]) then
//    begin
//      Result := False;
//    end;
//  end;
//end;
//
procedure TfrmModflowPackages.CheckMt3dChemSpeciesDefined;
var
  Model: TPhastModel;
begin
  Model := frmGoPhast.PhastModel;
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrNoChemicalSpecies);
  if Model.Mt3dmsIsSelected then
  begin
    if (Model.MobileComponents.Count = 0)
      and (Model.ImmobileComponents.Count = 0) then
    begin
      frmErrorsAndWarnings.AddWarning(Model,
        StrNoChemicalSpecies, StrMT3DMSIsActiveBut);
    end;
  end;
end;

function TfrmModflowPackages.CheckMt3dSaturation: Boolean;
begin
  result := True;
  if (frmGoPhast.ModelSelection = msModflow2015) and framePkgMt3dBasic.Selected
    and framePkgNpf.SaturationSelected then
  begin
    if MessageDlg(StrTheSaveSaturation, mtWarning, [mbYes, mbNo], 0)
      = mrYes then
    begin
      Result := False;
    end;
  end;
end;

function TfrmModflowPackages.DuplicateParameterNames: boolean;
var
  ParamNames: TStringList;
  DupNames: TStringList;
  ParamIndex: Integer;
  Param: TModflowParameter;
  procedure CheckParam;
  begin
    if ParamNames.IndexOf(Param.ParameterName) >= 0 then
    begin
      if DupNames.IndexOf(Param.ParameterName) < 0 then
      begin
        DupNames.Add(Param.ParameterName)
      end;
    end;
    ParamNames.Add(Param.ParameterName);
  end;
begin
//  result := False;
  ParamNames := TStringList.Create;
  DupNames := TStringList.Create;
  try
    ParamNames.CaseSensitive := False;
    DupNames.CaseSensitive := False;
    for ParamIndex := 0 to FSteadyParameters.Count - 1 do
    begin
      Param := FSteadyParameters[ParamIndex];
      CheckParam;
    end;
    for ParamIndex := 0 to FHufParameters.Count - 1 do
    begin
      Param := FHufParameters[ParamIndex];
      CheckParam;
    end;
    for ParamIndex := 0 to FTransientListParameters.Count - 1 do
    begin
      Param := FTransientListParameters[ParamIndex];
      CheckParam;
    end;
    result := DupNames.Count > 0;
    if result then
    begin
      Beep;
      MessageDlg(Format(StrDuplicateParameter, [DupNames.Text]), mtError, [mbOK], 0);
    end;
  finally
    ParamNames.Free;
    DupNames.Free;
  end;
end;

procedure TfrmModflowPackages.CheckSmsLinearSolver;
begin
  if frmGoPhast.DisvUsed then
  begin
    if Packageused(StrGNCGhostNodeCorr)
      and (TEquationFormulation(framePkgGNC.rgFormulation.ItemIndex) = efImplicit)
      and (framePkgIMS.LineAccel <> sllaBiCgStab)
      then
    begin
      Beep;
      MessageDlg(StrWhenTheGhostNode, mtError, [mbOK], 0);
    end;
  end;
end;

function TfrmModflowPackages.CheckGmgNwt: Boolean;
var
  GmgPackage: TGmgPackageSelection;
begin
  GmgPackage := frmGoPhast.PhastModel.ModflowPackages.GmgPackage;
  result := (frmGoPhast.ModelSelection = msModflowNWT)
    and (PackageNodeChecked(GmgPackage.PackageIdentifier));
  if result then
  begin
    if not (MessageDlg(StrTheGMGPackageIsN, mtWarning, [mbYes, mbNo], 0)
      in [mrYes, mrNone]) then
    begin
      result := False;
    end;
  end;
end;

function TfrmModflowPackages.CheckGwtSaturation: Boolean;
begin
  result := True;
  if (frmGoPhast.ModelSelection = msModflow2015) and frameGwtProcess.Selected
    and frameGwtProcess.SeparateSimulations
    and not framePkgNpf.SaturationSelected then
  begin
    if (MessageDlg(StrTheSaveSaturationGwt, mtWarning, [mbYes, mbNo], 0)
      in [mrNo, mrNone]) then
    begin
      Result := False;
    end;
  end;
end;

procedure TfrmModflowPackages.CheckXt3dGnc;
var
  Packages: TModflowPackages;
begin
  if (frmGoPhast.ModelSelection = msModflow2015)
    and frmGoPhast.DisvUsed then
  begin
    Packages := frmGoPhast.PhastModel.ModflowPackages;
    if Packages.GncPackage.IsSelected
      and Packages.NpfPackage.IsSelected
      and Packages.NpfPackage.UseXT3D then
    begin
      Beep;
      MessageDlg(StrYouCanNotUseTheGnc, mtWarning, [mbOK], 0);
    end;
  end;
end;

procedure TfrmModflowPackages.CheckIPHDRY;
var
  Packages: TModflowPackages;
begin
  Packages := frmGoPhast.PhastModel.ModflowPackages;
  if Packages.UpwPackage.IsSelected
    and (Packages.UpwPackage.HDryPrintOption = hpoPrintHdry) then
  begin
    if Packages.HobPackage.IsSelected
      or Packages.ChobPackage.IsSelected
      or Packages.DrobPackage.IsSelected
      or Packages.GbobPackage.IsSelected
      or Packages.RvobPackage.IsSelected
      or Packages.StobPackage.IsSelected
      then
    begin
      Beep;
      MessageDlg(StrBecauseYouAreUsin, mtWarning, [mbOK], 0);
    end;
  end;
end;

procedure TfrmModflowPackages.btnCancelClick(Sender: TObject);
begin
  Handle;
  inherited;

end;

function TfrmModflowPackages.ValidMT3D: Boolean;
begin
  result := True;
  if frmGoPhast.DisvUsed and framePkgMt3dBasic.Selected then
  begin
    result := False;
    Beep;
    MessageDlg(StrMT3DCanOnlyBeUse, mtError, [mbOK], 0);
  end;
end;

function TfrmModflowPackages.ValidModpathChoice: Boolean;
begin
  result := True;
  if (frmGoPhast.ModelSelection = msModflow2015) and frameModpath.Selected
    and (frameModpath.rgModpathVersion.ItemIndex <> 2) then
  begin
    result := False;
    Beep;
    MessageDlg(StrOnlyMODPATH7CanB, mtError, [mbOK], 0);
  end;
end;

procedure TfrmModflowPackages.btnOKClick(Sender: TObject);
var
  NeedToDefineFluxObservations: Boolean;
  ModflowPackages: TModflowPackages;
  SubPackage: TSubPackageSelection;
  SwtPackage: TSwtPackageSelection;
  FarmProcess: TFarmProcess;
begin
  // calling Handle here MIGHT keep the form from being unresponsive when it is
  // left open for a long time.
  Handle;
  inherited;

  ValidMT3D;

  if not ValidModpathChoice then
  begin
    ModalResult := mrNone;
    Exit;
  end;

  if DuplicateParameterNames then
  begin
    ModalResult := mrNone;
    Exit;
  end;

  if not AreParameterZonesOK then
  begin
    ModalResult := mrNone;
    Exit;
  end;

  CheckLpfParameters;
  CheckSfrParameterInstances;
  CheckMt3dChemSpeciesDefined;
  CheckSmsLinearSolver;

  if CheckGmgNwt then
  begin
    ModalResult := mrNone;
    Exit;
  end;

  if not CheckMt3dSaturation then
  begin
    ModalResult := mrNone;
    Exit;
  end;

  if not CheckGwtSaturation then
  begin
    ModalResult := mrNone;
    Exit;
  end;


  ModflowPackages := frmGoPhast.PhastModel.ModflowPackages;
  NeedToDefineFluxObservations := False;
  if framePkgCHOB.Selected and
    not ModflowPackages.ChobPackage.IsSelected then
  begin
    NeedToDefineFluxObservations := True;
  end
  else if framePkgDROB.Selected and
    not ModflowPackages.DrobPackage.IsSelected then
  begin
    NeedToDefineFluxObservations := True;
  end
  else if framePkgGBOB.Selected and
    not ModflowPackages.GbobPackage.IsSelected then
  begin
    NeedToDefineFluxObservations := True;
  end
  else if framePkgRVOB.Selected and
    not ModflowPackages.RvobPackage.IsSelected then
  begin
    NeedToDefineFluxObservations := True;
  end
  else if framePkgStOB.Selected and
    not ModflowPackages.StobPackage.IsSelected then
  begin
    NeedToDefineFluxObservations := True;
  end
  else if framePkgMt3dmsTob.Selected and
    not ModflowPackages.Mt3dmsTransObs.IsSelected then
  begin
    NeedToDefineFluxObservations := True;
  end;

  if (frmGoPhast.ModelSelection = msModflow2015)
    and framePkgSto.Selected
    and (ModflowPackages.NpfPackage.UseSaturatedThickness
    <> framePkgNpf.rdgOptions.Checked[0, Ord(noThickStrt)]) then
  begin
    Beep;
    MessageDlg(StrBecauseYouAreChan, mtInformation, [mbOK], 0);
  end;

  SetData;

  if (frmGoPhast.ModelSelection = msModflow2015)
    and ModflowPackages.StoPackage.IsSelected
    and not frmGoPhast.PhastModel.ModflowStressPeriods.TransientModel then
  begin
    Beep;
    MessageDlg(StrYouWillNeedToDef, mtWarning, [mbOK], 0);
  end;

  CheckMt3dChemSpeciesDefined;
  CheckXt3dGnc;
  CheckIPHDRY;

  SubPackage := ModflowPackages.SubPackage;
  if SubPackage.IsSelected then
  begin
    SubPackage.PrintChoices.ReportErrors;
  end;

  SwtPackage := ModflowPackages.SwtPackage;
  if SwtPackage.IsSelected then
  begin
    SwtPackage.PrintChoices.ReportErrors;
  end;

  frmGoPhast.PhastModel.ModpathHeadWarning;

  if NeedToDefineFluxObservations then
  begin
    Hide;
//    frmGoPhast.miManageFluxObservationsClick(nil);
    ShowAForm(TfrmManageFluxObservations);
  end;

  if framePkgHuf.Selected
    and (frmGoPhast.PhastModel.HydrogeologicUnits.Count = 0) then
  begin
    frmGoPhast.miMF_HydrogeologicUnitsClick(nil);
  end;

  if (frmGoPhast.ModelSelection <> msModflow2015)
    and ((frmGoPhast.PhastModel.ModflowPackages.SubPackage.IsSelected
    and not frmGoPhast.PhastModel.LayerStructure.SubsidenceDefined)
    or (frmGoPhast.PhastModel.ModflowPackages.SwtPackage.IsSelected
    and not frmGoPhast.PhastModel.LayerStructure.SwtDefined)) then
  begin
    frmGoPhast.acLayersExecute(nil);
  end;

  if frmGoPhast.ModelSelection in [msModflow, msModflowNWT, msModflowFmp
          {$IFDEF OWHMV2}
          , msModflowOwhm2
          {$ENDIF}
  ] then
  begin
    if frmGoPhast.PhastModel.ModflowPackages.SwrPackage.IsSelected
      and (frmGoPhast.PhastModel.SwrReachGeometry.Count = 0) then
    begin
      frmGoPhast.acSWR_ReachGeometryExecute(nil);
    end;
  end;

  if frmGoPhast.PhastModel.RipIsSelected
    and (frmGoPhast.PhastModel.RipPlantGroups.Count = 0)
    and (frmGoPhast.ModelSelection in [msModflowFmp
          {$IFDEF OWHMV2}
          , msModflowOwhm2
          {$ENDIF}
    ]) then
  begin
    frmGoPhast.acRipPlantGroupsExecute(nil);
  end;

  frmErrorsAndWarnings.RemoveErrorGroup(frmGoPhast.PhastModel, StrInvalidSelectionOf);
  if (frmGoPhast.ModelSelection = msModflowFmp)
    and frmGoPhast.PhastModel.ModflowPackages.FarmProcess.IsSelected then
  begin
    FarmProcess := frmGoPhast.PhastModel.ModflowPackages.FarmProcess;
    if (FarmProcess.RootingDepth = rdCalculated)
      or (FarmProcess.ConsumptiveUse = cuCalculated) then
    begin
      if frmGoPhast.PhastModel.ModflowOptions.TimeUnit <> 4 then
      begin
        frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel, StrInvalidSelectionOf,
          StrTheFarmProcessReq)
      end;
    end;
    if (frmGoPhast.PhastModel.FmpCrops.Count = 0) then
    begin
      frmGoPhast.acFarmCropsExecute(nil);
    end;
    if (frmGoPhast.PhastModel.FmpSoils.Count = 0) then
    begin
      frmGoPhast.acFarmSoilsExecute(nil);
    end;
    if frmGoPhast.acFarmClimate.Enabled
      and (frmGoPhast.PhastModel.FmpClimate.Count = 0) then
    begin
      frmGoPhast.acFarmClimateExecute(nil);
    end;
    if frmGoPhast.acFarmAllotment.Enabled
      and (frmGoPhast.PhastModel.FmpAllotment.Count = 0) then
    begin
      frmGoPhast.acFarmAllotmentExecute(nil);
    end;
  end;

  if frmErrorsAndWarnings.HasMessages then
  begin
    frmErrorsAndWarnings.Show;
  end;
end;

procedure TfrmModflowPackages.FillHufTree;
var
  PriorBaseNode: TTreeNode;
  ChildNode: TTreeNode;
  HkNode: TTreeNode;
begin
  tvHufParameterTypes.Items.Clear;
  
  PriorBaseNode := nil;

  ChildNode := tvHufParameterTypes.Items.Add(PriorBaseNode,
    StrHKHorizontalHydra);
  ChildNode.Data := Pointer(ptHUF_HK);
  HkNode := ChildNode;

  ChildNode := tvHufParameterTypes.Items.Add(PriorBaseNode,
    StrHANIHorizontalAni);
  ChildNode.Data := Pointer(ptHUF_HANI);

  ChildNode := tvHufParameterTypes.Items.Add(PriorBaseNode,
    StrVKVerticalHydraul);
  ChildNode.Data := Pointer(ptHUF_VK);

  ChildNode := tvHufParameterTypes.Items.Add(PriorBaseNode,
    StrVANIVerticalAniso);
  ChildNode.Data := Pointer(ptHUF_VANI);

  ChildNode := tvHufParameterTypes.Items.Add(PriorBaseNode,
    StrSSSpecificStorage);
  ChildNode.Data := Pointer(ptHUF_SS);

  ChildNode := tvHufParameterTypes.Items.Add(PriorBaseNode,
    StrSYSpecificYield);
  ChildNode.Data := Pointer(ptHUF_SY);

  ChildNode := tvHufParameterTypes.Items.Add(PriorBaseNode,
    StrSYTPStorageCoeffi);
  ChildNode.Data := Pointer(ptHUF_SYTP);

  ChildNode := tvHufParameterTypes.Items.Add(PriorBaseNode,
    StrKDEPHydraulicCond);
  ChildNode.Data := Pointer(ptHUF_KDEP);

  ChildNode := tvHufParameterTypes.Items.Add(PriorBaseNode,
    StrLVDAHorizontalAni);
  ChildNode.Data := Pointer(ptHUF_LVDA);

  tvHufParameterTypes.Selected := HkNode;
end;

procedure TfrmModflowPackages.FillLpfTree;
var
  PriorBaseNode: TTreeNode;
  ChildNode: TTreeNode;
  HkNode: TTreeNode;
begin
  tvLpfParameterTypes.Items.Clear;

  PriorBaseNode := nil;

  ChildNode := tvLpfParameterTypes.Items.Add(PriorBaseNode,
    StrHKHorizontalHydra);
  ChildNode.Data := Pointer(ptLPF_HK);
  HkNode := ChildNode;

  ChildNode := tvLpfParameterTypes.Items.Add(PriorBaseNode,
    StrHANIHorizontalAni);
  ChildNode.Data := Pointer(ptLPF_HANI);

  ChildNode := tvLpfParameterTypes.Items.Add(PriorBaseNode,
    StrVKVerticalHydraul);
  ChildNode.Data := Pointer(ptLPF_VK);

  ChildNode := tvLpfParameterTypes.Items.Add(PriorBaseNode,
    StrVANIVerticalAniso);
  ChildNode.Data := Pointer(ptLPF_VANI);

  ChildNode := tvLpfParameterTypes.Items.Add(PriorBaseNode,
    StrSSSpecificStorage);
  ChildNode.Data := Pointer(ptLPF_SS);

  ChildNode := tvLpfParameterTypes.Items.Add(PriorBaseNode,
    StrSYSpecificYield);
  ChildNode.Data := Pointer(ptLPF_SY);

  ChildNode := tvLpfParameterTypes.Items.Add(PriorBaseNode,
    StrVKCBVerticalHydra);
  ChildNode.Data := Pointer(ptLPF_VKCB);

  tvLpfParameterTypes.Selected := HkNode;
end;

procedure TfrmModflowPackages.framePkgBCFrcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  EnableUzfVerticalKSource;
end;

procedure TfrmModflowPackages.framePkgCFPcbPipesClick(Sender: TObject);
begin
  inherited;
  framePkgCFP.cbPipesClick(Sender);
  EnableConduitRecharge;
end;

procedure TfrmModflowPackages.framePkgCFPrcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  framePkgCFP.rcSelectionControllerEnabledChange(Sender);
  EnableConduitRecharge;
end;

procedure TfrmModflowPackages.framePkgEVTrcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  EnableEvtModpathOption;
end;

procedure TfrmModflowPackages.framePkgFmp4SoilsrdgSoilsSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  framePkgFmp4Soils.rdgSoilsSelectCell(Sender, ACol, ARow, CanSelect);

end;

procedure TfrmModflowPackages.framePkgFrmjvplFarmChange(Sender: TObject);
begin
  inherited;
  HelpKeyword := framePkgFrm.jvplFarm.ActivePage.HelpKeyword;
end;

procedure TfrmModflowPackages.framePkgFrmrcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  framePkgFrm.rcSelectionControllerEnabledChange(Sender);
  EnableFarmPrintRouting;
end;

procedure TfrmModflowPackages.framePkgHufrcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  ActivateHufReferenceChoice;
end;

procedure TfrmModflowPackages.framePkgLAKrcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  EnableLakeTransport;
end;

procedure TfrmModflowPackages.framePkgLPFrcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  EnableLpfParameterControls;
end;

procedure TfrmModflowPackages.framePkgLPFSelectedChange(Sender: TObject);
begin
  framePkgSFR.LpfUsed := framePkgLPF.Selected;
end;

procedure TfrmModflowPackages.framePkgMt3dBasiccomboVersionChange(
  Sender: TObject);
begin
  inherited;
  EnableUnsatTransport;
  EnableLakeTransport;
  UpdateGwtFrames;
end;

procedure TfrmModflowPackages.framePkgMt3dBasicrcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  framePkgMt3dBasic.rcSelectionControllerEnabledChange(Sender);
  EnableUnsatTransport;
  EnableLakeTransport;
  EnableStreamTransport;
  EnableContaminantTreatmentSystem;
  EnableChemSpecies;
end;

procedure TfrmModflowPackages.framePkgMt3dmsRctcomboKineticChoiceChange(
  Sender: TObject);
var
  KineticChoice: TKineticChoice;
begin
  inherited;
  framePkgMt3dmsRct.comboKineticChoiceChange(Sender);
  if (framePkgMt3dmsRct.comboKineticChoice.ItemIndex >= 0) and IsLoaded then
  begin
    KineticChoice := TKineticChoice(framePkgMt3dmsRct.comboKineticChoice.ItemIndex);
    if (KineticChoice in [kcMonod, kcFirstOrderChain])
      and (framePkgMt3dBasic.comboVersion.ItemIndex <> 0) then
    begin
      Beep;
      MessageDlg(StrMonodAndFirstorde, mtWarning, [mbOK], 0);
    end;
    if (KineticChoice = kcFirstOrderChain)
      and (framePkgMt3dmsRct.NumberOfSpecies < 2) then
    begin
      Beep;
      MessageDlg(StrFirstorderChainRe, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TfrmModflowPackages.framePkgNwtpcNWTChange(Sender: TObject);
begin
  inherited;
  HelpKeyword := framePkgNwt.pcNWT.ActivePage.HelpKeyword;
end;

procedure TfrmModflowPackages.framePkgNwtrcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  framePkgNwt.rcSelectionControllerEnabledChange(Sender);
end;

procedure TfrmModflowPackages.ChdSelectedChange(Sender: TObject);
begin
  framePkgCHOB.CanSelect := framePkgCHD.Selected;
  if not framePkgCHOB.CanSelect then
  begin
    framePkgCHOB.Selected := False;
  end;
end;

destructor TfrmModflowPackages.Destroy;
begin
  FframePkgSmsObjectList.Free;
  FframePackageIstObjectList.Free;
  FframePackageMSTObjectList.Free;
  inherited;
end;

procedure TfrmModflowPackages.DrnSelectedChange(Sender: TObject);
begin
  framePkgDROB.CanSelect := framePkgDRN.Selected;
  if not framePkgDROB.CanSelect then
  begin
    framePkgDROB.Selected := False;
  end;
end;

procedure TfrmModflowPackages.GhbSelectedChange(Sender: TObject);
begin
  framePkgGBOB.CanSelect := framePkgGHB.Selected;
  if not framePkgGBOB.CanSelect then
  begin
    framePkgGBOB.Selected := False;
  end;
end;

procedure TfrmModflowPackages.RivSelectedChange(Sender: TObject);
begin
  framePkgRVOB.CanSelect := framePkgRIV.Selected;
  if not framePkgRVOB.CanSelect then
  begin
    framePkgRVOB.Selected := False;
  end;
end;

procedure TfrmModflowPackages.StrSelectedChange(Sender: TObject);
begin
  framePkgSTOB.CanSelect := framePkgSTR.Selected;
  if not framePkgSTOB.CanSelect then
  begin
    framePkgSTOB.Selected := False;
  end;
end;

procedure TfrmModflowPackages.TimerBringToFrontTimer(Sender: TObject);
begin
  inherited;
  BringToFront;
  TimerBringToFront.Enabled := False;
end;

procedure TfrmModflowPackages.NwtSelectedChange(Sender: TObject);
begin
  if framePkgNwt.Selected then
  begin
    if not framePkgUpw.Selected then
    begin
      framePkgUpw.Selected := True;
      framePkgLPF.Selected := False;
      framePkgBCF.Selected := False;
      framePkgHuf.Selected := False;
    end;
  end
  else if framePkgUpw.Selected then
  begin
    framePkgUpw.Selected := False;
    if not framePkgLPF.Selected and not framePkgBCF.Selected
      and not framePkgHuf.Selected then
    begin
      framePkgLpf.Selected := True;
    end;
  end;
end;

procedure TfrmModflowPackages.UpwSelectedChange(Sender: TObject);
begin
  if framePkgUpw.Selected then
  begin
    if not framePkgNwt.Selected then
    begin
      framePkgNwt.Selected := True;
      framePcg.Selected := False;
      framePkgSIP.Selected := False;
      framePkgGMG.Selected := False;
      framePkgDE4.Selected := False;
    end;
  end
  else if framePkgNwt.Selected then
  begin
    framePkgNwt.Selected := False;
    if not framePcg.Selected and not framePkgSIP.Selected
      and not framePkgGMG.Selected and not framePkgDE4.Selected then
    begin
      framePcg.Selected := True;
    end;
  end;

end;

procedure TfrmModflowPackages.framePkgRCHrcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  EnableRchModpathOption;
  EnableConduitRecharge;
end;

procedure TfrmModflowPackages.EnableSfrParameters;
var
  OldActivePage: TTabSheet;
begin
  frameSFRParameterDefinition.Enabled :=
    framePkgSFR.rcSelectionController.Enabled
    and (framePkgSFR.CalculateISFROPT = 0);
  OldActivePage := pcSFR.ActivePage;
  tabSfrParameters.TabVisible := frameSFRParameterDefinition.Enabled;
  pcSFR.ActivePage := OldActivePage;
  if not frameSFRParameterDefinition.Enabled then
  begin
    frameSFRParameterDefinition.seNumberOfParameters.OnEnter(
      frameSFRParameterDefinition.seNumberOfParameters);
    frameSFRParameterDefinition.seNumberOfParameters.Value := 0;
    frameSFRParameterDefinition.seNumberOfParameters.OnChange(
      frameSFRParameterDefinition.seNumberOfParameters);
  end;
  tabSfrGeneral.TabVisible := tabSfrParameters.TabVisible;
  if not tabSfrGeneral.TabVisible then
  begin
    pcSFR.ActivePage := tabSfrGeneral;
  end;
  SetpcSFR_ClientBorderWidth;
end;

procedure TfrmModflowPackages.EnableStreamTransport;
var
  CanSelect: Boolean;
begin
  CanSelect := framePkgMt3dBasic.rcSelectionController.Enabled
    and (framePkgMt3dBasic.comboVersion.ItemIndex = 0);
  if frmGoPhast.ModelSelection <> msModflowNWT then
  begin
    // SFT is not currently supported with MODFLOW 6
//    CanSelect := CanSelect and framePackageSfrMF6.rcSelectionController.Enabled;
    CanSelect := False;
  end
  else
  begin
    CanSelect := CanSelect and framePkgSFR.rcSelectionController.Enabled;
  end;
  frameMt3dSftPkg.CanSelect := CanSelect;
  if not frameMt3dSftPkg.CanSelect then
  begin
    frameMt3dSftPkg.Selected := False;
  end;
end;

procedure TfrmModflowPackages.SetpcSFR_ClientBorderWidth;
begin
  if tabSfrParameters.TabVisible then
  begin
    pcSFR.ClientBorderWidth := 4;
  end
  else
  begin
    pcSFR.ClientBorderWidth := 0;
  end;
end;

procedure TfrmModflowPackages.framePkgSFRcbSeepageLossClick(Sender: TObject);
begin
  inherited;
  framePkgSFR.cbSeepageLossClick(Sender);

end;

procedure TfrmModflowPackages.framePkgSFRcbSfrLpfHydraulicCondClick(
  Sender: TObject);
begin
  inherited;
  EnableSfrParameters;
end;

procedure TfrmModflowPackages.framePkgSFRcbSfrUnsatflowClick(Sender: TObject);
begin
  inherited;
  EnableSfrParameters;
  framePkgSFR.rcSelectionControllerEnabledChange(Sender);
end;

procedure TfrmModflowPackages.framePkgSFRrcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  framePkgSFR.rcSelectionControllerEnabledChange(Sender);
  EnableSfrParameters;
  EnableFarmPrintRouting;
  EnableStreamTransport;
end;

procedure TfrmModflowPackages.framePkgSFRrgSfr2ISFROPTClick(Sender: TObject);
begin
  inherited;
  EnableSfrParameters;
end;

procedure TfrmModflowPackages.framePkgSMSrcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  framePkgIMS.rcSelectionControllerEnabledChange(Sender);

end;

procedure TfrmModflowPackages.framePkgSWRjvplSwrChange(Sender: TObject);
begin
  inherited;
  HelpKeyword := framePkgSWR.jvplSwr.ActivePage.HelpKeyword;

end;

procedure TfrmModflowPackages.framePkgUPWrcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  EnableLpfParameterControls;
end;

procedure TfrmModflowPackages.framePkgUZFrcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  EnableUzfVerticalKSource;
end;

procedure TfrmModflowPackages.frameSFRParameterDefinitionbtnDeleteClick(
  Sender: TObject);
var
  PageIndex: integer;
  Page: TJvCustomPage;
  Frame: TframeSfrParamInstances;
begin
  inherited;
  frameParameterDefinition_btnDeleteClick(Sender);
  frameSFRParameterDefinition.btnDeleteClick(frameSFRParameterDefinition.btnDelete);
  PageIndex := frameSFRParameterDefinition.dgParameters.SelectedRow -1;
  frameSFRParameterDefinition.btnDeleteClick(frameSFRParameterDefinition.btnDelete);
  if (PageIndex >= 0) and (PageIndex < jplSfrParameters.PageCount) then
  begin
    Page := jplSfrParameters.Pages[PageIndex];
    Assert(Page.ControlCount= 1);
    Page.Handle;
    Frame := Page.Controls[0] as TframeSfrParamInstances;
    Frame.Free;
    Page.Free;
  end;
end;

procedure TfrmModflowPackages.frameSFRParameterDefinitiondgParametersSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  PageIndex: integer;
begin
  inherited;
  frameSFRParameterDefinition.dgParametersSelectCell(Sender, ACol, ARow, CanSelect);
  PageIndex := frameSFRParameterDefinition.dgParameters.SelectedRow -1;
  if PageIndex < jplSfrParameters.PageCount then
  begin
    jplSfrParameters.ActivePageIndex := PageIndex;
  end;
end;

procedure TfrmModflowPackages.frameSFRParameterDefinitiondgParametersSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
var
  PageIndex: Integer;
  Page: TJvCustomPage;
  Frame: TframeSfrParamInstances;
begin
  inherited;
  frameSFRParameterDefinition.dgParametersSetEditText(Sender, ACol, ARow, Value);
  PageIndex := ARow -1;
  if PageIndex < jplSfrParameters.PageCount then
  begin
    Page := jplSfrParameters.Pages[PageIndex];
    Assert(Page.ControlCount= 1);
    Page.Handle;
    Frame := Page.Controls[0] as TframeSfrParamInstances;
    Frame.pnlLabel.Caption := frameSFRParameterDefinition.
      dgParameters.Cells[Ord(pcName),PageIndex+1];
  end;
end;

procedure TfrmModflowPackages.frameSFRParameterDefinitionseNumberOfParametersChange(
  Sender: TObject);
var
  Page: TJvCustomPage;
  Frame: TframeSfrParamInstances;
  PageIndex: integer;
begin
  inherited;
  frameSFRParameterDefinition.seNumberOfParametersChange(Sender);
  frameParameterDefinition_seNumberOfParametersChange(Sender);
  while jplSfrParameters.PageCount <
    frameSFRParameterDefinition.seNumberOfParameters.AsInteger do
  begin
    Page := TJvCustomPage.Create(self);
    Page.PageList := jplSfrParameters;
    Frame := TframeSfrParamInstances.Create(self);
    Frame.Name := '';
    Frame.Parent := Page;
    Frame.pnlLabel.Caption := frameSFRParameterDefinition.
      dgParameters.Cells[Ord(pcName),jplSfrParameters.PageCount];
    Frame.btnDeleteFlowTableRow.Left := jplSfrParameters.Width
      - Frame.btnDeleteFlowTableRow.Width - 8;
    Frame.btnInsertFlowTableRow.Left := Frame.btnDeleteFlowTableRow.Left
      - Frame.btnInsertFlowTableRow.Width - 8;

    frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithStartTimes
      (Frame.rdgSfrParamInstances, Ord(sicStartTime));
    frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithEndTimes
      (Frame.rdgSfrParamInstances, Ord(sicEndTime));

    Frame.seInstanceCount.AsInteger := 0;
    Frame.FrameLoaded := True;
  end;
  while jplSfrParameters.PageCount >
    frameSFRParameterDefinition.seNumberOfParameters.AsInteger do
  begin
    Page := jplSfrParameters.Pages[jplSfrParameters.PageCount-1];
    Assert(Page.ControlCount= 1);
    Page.Handle;
    Frame := Page.Controls[0] as TframeSfrParamInstances;
    Frame.Free;
    Page.Free;
  end;
  if frameSFRParameterDefinition.dgParameters.SelectedRow >= 1 then
  begin
    PageIndex := frameSFRParameterDefinition.dgParameters.SelectedRow-1
  end
  else
  begin
    PageIndex := 0;
  end;
  if PageIndex < jplSfrParameters.PageCount then
  begin
    jplSfrParameters.ActivePageIndex := PageIndex;
  end;
end;

procedure TfrmModflowPackages.frameZoneBudgetrcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  frameZoneBudget.rcSelectionControllerEnabledChange(Sender);

end;

procedure TfrmModflowPackages.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Handle;
  inherited;
  IsLoaded := False;
end;

procedure TfrmModflowPackages.FormCreate(Sender: TObject);
begin
  inherited;
  FLinkDictionary := TDictionary<TframePackage, TFrameNodeLink>.Create;
  FFrameNodeLinks := TObjectList.Create;
  framePkgGMG.pcGMG.ActivePageIndex := 0;
  frameMt3dmsAdvPkg.pcAdvection.ActivePageIndex := 0;
end;

procedure TfrmModflowPackages.FormDestroy(Sender: TObject);
begin
  inherited;
  FSteadyParameters.Free;
  FHufParameters.Free;
  FTransientListParameters.Free;
  FPackageList.Free;
  FSfrParameterInstances.Free;
  FNewPackages.Free;
  FFrameNodeLinks.Free;
  FLinkDictionary.Free;
end;

procedure TfrmModflowPackages.FormResize(Sender: TObject);
begin
  inherited;
  AdjustDroppedWidth(self);
end;

procedure TfrmModflowPackages.FormShow(Sender: TObject);
begin
  inherited;
  EnableLpfParameterControls;
  jvplPackagesChange(nil);
  BringToFront;
  TimerBringToFront.Enabled := True;
end;

procedure TfrmModflowPackages.frameCropConsumptiveUserdgGridSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if (ARow = 2) and not framePkgUZF.Selected then
  begin
    CanSelect := False;
  end;
end;

procedure TfrmModflowPackages.UpdateSpeciesNames;
var
  SpeciesNames: TStringList;
  RowIndex: Integer;
  KineticChoice: TKineticChoice;
begin
  SpeciesNames := TStringList.Create;
  try
    SpeciesNames.Capacity :=
      frameChemSpecies.frameGridMobile.seNumber.AsInteger
      + frameChemSpecies.frameGridImmobile.seNumber.AsInteger;
    for RowIndex := 1 to frameChemSpecies.frameGridMobile.seNumber.AsInteger do
    begin
      SpeciesNames.Add(frameChemSpecies.frameGridMobile.Grid.Cells[0,RowIndex]);
    end;
    for RowIndex := 1 to frameChemSpecies.frameGridImmobile.seNumber.AsInteger do
    begin
      SpeciesNames.Add(frameChemSpecies.frameGridImmobile.Grid.Cells[0,RowIndex]);
    end;
    framePkgMt3dmsRct.SetSpeciesNames(SpeciesNames);

    if IsLoaded then
    begin
      KineticChoice := TKineticChoice(framePkgMt3dmsRct.comboKineticChoice.ItemIndex);
      if (KineticChoice = kcFirstOrderChain)
        and (framePkgMt3dmsRct.NumberOfSpecies < 2) then
      begin
        Beep;
        MessageDlg(StrFirstorderChainRe, mtError, [mbOK], 0);
      end;
    end;


  finally
    SpeciesNames.Free;
  end;

end;

procedure TfrmModflowPackages.frameGridImmobileGridExit(Sender: TObject);
begin
  inherited;
  UpdateSpeciesNames;
end;

procedure TfrmModflowPackages.frameGridImmobileGridSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  frameChemSpecies.frameGridImmobileGridSelectCell(Sender, ACol, ARow, CanSelect)
end;

procedure TfrmModflowPackages.frameGridImmobileseNumberChange(Sender: TObject);
begin
  inherited;
  frameChemSpecies.frameGridImmobile.seNumberChange(Sender);
  UpdateSpeciesNames;
end;

procedure TfrmModflowPackages.frameGridMobileGridButtonClick(Sender: TObject;
  ACol, ARow: Integer);
begin
  inherited;
  frameChemSpecies.frameGridSpeciesGridButtonClick(Sender, ACol, ARow);
end;

procedure TfrmModflowPackages.frameGridMobileGridExit(Sender: TObject);
begin
  inherited;
  UpdateSpeciesNames;
end;

procedure TfrmModflowPackages.frameGridMobileGridSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  frameChemSpecies.frameGridMobileGridSelectCell(Sender, ACol, ARow,
    CanSelect);
  if (ACol in [1,2]) and not framePkgMt3dBasic.Selected then
  begin
    CanSelect := False
  end;
end;

procedure TfrmModflowPackages.frameGridMobileGridSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  inherited;
  UpdateGwtFrames;
end;

procedure TfrmModflowPackages.frameGridMobileGridStateChange(Sender: TObject;
  ACol, ARow: Integer; const Value: TCheckBoxState);
begin
  inherited;
  frameChemSpecies.frameSpeciesGridStateChange(Sender, ACol, ARow, Value);
//
end;

procedure TfrmModflowPackages.frameGridMobileseNumberChange(Sender: TObject);
begin
  inherited;
  frameChemSpecies.frameGridMobile.seNumberChange(Sender);
  UpdateSpeciesNames;
  UpdateGwtFrames;
end;

procedure TfrmModflowPackages.frameGwtProcessrcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  frameGwtProcess.rcSelectionControllerEnabledChange(Sender);
  EnableChemSpecies;
  EnableGwtPackages;
end;

procedure TfrmModflowPackages.frameModpathrcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  frameModpath.rcSelectionControllerEnabledChange(nil);
  EnableEvtModpathOption;
  EnableRchModpathOption;
end;

procedure TfrmModflowPackages.framePackageLakMf6rcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  EnableLakeTransport;
end;

procedure TfrmModflowPackages.framePackageSfrMF6rcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  EnableStreamTransport;
end;

procedure TfrmModflowPackages.framePackageUzfMf6rcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  framePackageUzfMf6.rcSelectionControllerEnabledChange(Sender);
    EnableUnsatTransport;

end;

procedure TfrmModflowPackages.frameParameterDefinition_btnDeleteClick(
  Sender: TObject);
var
  ActiveFrame: TframeListParameterDefinition;
begin
  inherited;
  ActiveFrame := ParentFrame(Sender);
  ActiveFrame.btnDeleteClick(Sender);
  case CurrentParameterType of
    ptLPF_HK..ptLPF_VKCB, ptHFB, ptHUF_SYTP, ptHUF_LVDA:
      begin
        FSteadyParameters.Remove(ActiveFrame.CurrentParameter);
      end;
    ptCHD..ptDRT, ptSFR, ptRCH, ptEVT, ptETS, ptSTR, ptQMAX:
      begin
        FTransientListParameters.Remove(ActiveFrame.CurrentParameter);
      end;
    ptHUF_HK..ptHUF_SY, ptHUF_KDEP:
      begin
        FHufParameters.Remove(ActiveFrame.CurrentParameter);
      end
    else Assert(False);
  end;

end;

function TfrmModflowPackages.ParentFrame(Sender: TObject): TframeListParameterDefinition;
var
  Control: TControl;
begin
  result := nil;
  Control := Sender as TControl;
  while Control <> nil do
  begin
    if Control is TframeListParameterDefinition then
    begin
      result := TframeListParameterDefinition(Control);
      Exit;
    end;
    Control := Control.Parent;
  end;
end;

procedure TfrmModflowPackages.ActivateHufReferenceChoice;
begin
  framePkgHuf.rgElevationSurfaceChoice.Enabled :=
    framePkgHuf.rcSelectionController.Enabled
    and (FHufParameters.CountParameters([ptHUF_KDEP]) > 0);
end;

procedure TfrmModflowPackages.frameParameterDefinition_seNumberOfParametersChange(
  Sender: TObject);
var
  RowIndex: Integer;
  Parameter: TModflowParameter;
  ActiveGrid: TRbwDataGrid4;
  ActiveFrame: TframeListParameterDefinition;
begin
  inherited;
  if FSettingNumber then
  begin
    Exit;
  end;
  FSettingNumber := True;
  try
    ActiveFrame := ParentFrame(Sender);
    if Sender = ActiveFrame.btnDelete then
    begin
      ActiveFrame.PriorNumberOfParameters :=
        ActiveFrame.seNumberOfParameters.AsInteger;
    end;
    ActiveGrid := ActiveFrame.dgParameters;
    ActiveFrame.seNumberOfParametersChange(nil);
    if not IsLoaded then
    begin
      ActiveFrame.PriorNumberOfParameters := ActiveFrame.seNumberOfParameters.AsInteger;
      Exit;
    end;

    // Create new parameters.
    for RowIndex := ActiveFrame.PriorNumberOfParameters+1 to
      ActiveFrame.seNumberOfParameters.AsInteger do
    begin
      Parameter := nil;
      case CurrentParameterType of
        ptLPF_HK..ptLPF_VKCB, ptHFB, ptHUF_SYTP, ptHUF_LVDA:
          begin
            Parameter := FSteadyParameters.Add as TModflowParameter;
            if CurrentParameterType <> ptHFB then
            begin
              (Parameter as TModflowSteadyParameter).UseZone := True;
            end;
          end;
        ptCHD..ptSFR, ptRCH, ptEVT, ptETS, ptSTR, ptQMAX:
          begin
            Parameter := FTransientListParameters.Add as TModflowParameter;
          end;
        ptHUF_HK..ptHUF_SY, ptHUF_KDEP:
          begin
            Parameter := FHufParameters.Add as TModflowParameter;
          end;
        else Assert(False);
      end;
      Parameter.ParameterType := CurrentParameterType;
      Parameter.ParameterName := NewParameterName;
      AssignParameterToRow(ActiveGrid, RowIndex, Parameter);
    end;

    // Get rid of old parameters.
    for RowIndex := ActiveFrame.PriorNumberOfParameters downto
      ActiveFrame.seNumberOfParameters.AsInteger+1 do
    begin
      Parameter := ActiveGrid.Objects[0, RowIndex] as TModflowParameter;
      if Parameter <> nil then
      begin
      
        if Parameter is TModflowSteadyParameter then
        begin
          FSteadyParameters.Remove(Parameter);
        end
        else if Parameter is TModflowTransientListParameter then
        begin
          FTransientListParameters.Remove(Parameter);
        end
        else
        begin
          Assert(Parameter is THufParameter);
          FHufParameters.Remove(Parameter);
        end;
      end;
    end;

    ActiveFrame.PriorNumberOfParameters :=
      ActiveFrame.seNumberOfParameters.AsInteger;
    ActiveGrid.Invalidate;

    ActivateHufReferenceChoice;
  finally
    FSettingNumber := False;
  end;
end;

procedure TfrmModflowPackages.SetSfrParamInstances;
var
  PageIndex: integer;
  Page: TJvCustomPage;
  Frame: TframeSfrParamInstances;
  ParameterName: string;
  InstanceIndex: Integer;
  Item: TSfrParamInstance;
  InstanceName: string;
  StartTime, EndTime: double;
  ExistingInstances: TList;
  Index: integer;
  Position: integer;
begin
  ExistingInstances := TList.Create;
  try
    for PageIndex := 0 to jplSfrParameters.PageCount - 1 do
    begin
      Page := jplSfrParameters.Pages[PageIndex];
      Assert(Page.ControlCount= 1);
      Page.Handle;
      Frame := Page.Controls[0] as TframeSfrParamInstances;
      ParameterName := frameSFRParameterDefinition.dgParameters.
        Cells[Ord(pcName), PageIndex+1];
      if ParameterName <> '' then
      begin
        for InstanceIndex := 1 to Frame.seInstanceCount.AsInteger do
        begin
          InstanceName := Frame.rdgSfrParamInstances.Cells[Ord(sicInstanceName), InstanceIndex];

          if (InstanceName <> '')
            and TryStrToFloat(Frame.rdgSfrParamInstances.
              Cells[Ord(sicStartTime), InstanceIndex], StartTime)
            and TryStrToFloat(Frame.rdgSfrParamInstances.
              Cells[Ord(sicEndTime), InstanceIndex], EndTime) then
          begin
            Item := Frame.rdgSfrParamInstances.
              Objects[Ord(sicInstanceName), InstanceIndex] as TSfrParamInstance;
            if Item <> nil then
            begin
              ExistingInstances.Add(Item);
            end;
          end;
        end;
      end;
    end;
    for Index := FSfrParameterInstances.Count - 1 downto 0 do
    begin
      Item := FSfrParameterInstances.Items[Index];
      if ExistingInstances.IndexOf(Item) < 0 then
      begin
        FSfrParameterInstances.Delete(Index);
      end;
    end;

    Position := 0;
    for PageIndex := 0 to jplSfrParameters.PageCount - 1 do
    begin
      Page := jplSfrParameters.Pages[PageIndex];
      Assert(Page.ControlCount= 1);
      Page.Handle;
      Frame := Page.Controls[0] as TframeSfrParamInstances;
      ParameterName := frameSFRParameterDefinition.dgParameters.
        Cells[Ord(pcName), PageIndex+1];
      if ParameterName <> '' then
      begin
        for InstanceIndex := 1 to Frame.seInstanceCount.AsInteger do
        begin
          InstanceName := Frame.rdgSfrParamInstances.Cells[Ord(sicInstanceName), InstanceIndex];

          if (InstanceName <> '')
            and TryStrToFloat(Frame.rdgSfrParamInstances.
              Cells[Ord(sicStartTime), InstanceIndex], StartTime)
            and TryStrToFloat(Frame.rdgSfrParamInstances.
              Cells[Ord(sicEndTime), InstanceIndex], EndTime) then
          begin
            Item := Frame.rdgSfrParamInstances.
              Objects[Ord(sicInstanceName), InstanceIndex] as TSfrParamInstance;
            if Item = nil then
            begin
              Item := FSfrParameterInstances.Insert(Position) as TSfrParamInstance;
            end;
            Inc(Position);
            Item.ParameterName := ParameterName;
            Item.ParameterInstance := InstanceName;
            Item.StartTime := StartTime;
            Item.EndTime := EndTime;
          end;
        end;
      end;
    end;
  finally
    ExistingInstances.Free;
  end;
end;

procedure TfrmModflowPackages.ShowImsPage(Sender: TObject);
var
  APage: TJvStandardPage;
  ControlIndex: Integer;
  AFrame: TframePkgSms;
begin
  APage := Sender as TJvStandardPage;
  for ControlIndex := 0 to APage.ControlCount - 1 do
  begin
    if APage.Controls[ControlIndex] is TframePkgSms then
    begin
      AFrame := TframePkgSms(APage.Controls[ControlIndex]);
      AFrame.pgcControls.Align := alNone;
      AFrame.pgcControls.Align := alBottom;
    end;
  end;
end;

procedure TfrmModflowPackages.AdjustDroppedWidth(OwnerComponent: TComponent);
var
  Combo: TJvImageComboBox;
  AComponent: TComponent;
  ComponentIndex: Integer;
begin
  for ComponentIndex := 0 to OwnerComponent.ComponentCount - 1 do
  begin
    AComponent := OwnerComponent.Components[ComponentIndex];
    if AComponent is TJvImageComboBox then
    begin
      Combo := TJvImageComboBox(AComponent);
      Combo.DroppedWidth := Combo.Width;
    end
    else if AComponent is TFrame then
    begin
      AdjustDroppedWidth(AComponent);
    end;
  end;
end;

procedure TfrmModflowPackages.GetSfrParamInstances;
var
  ParameterNames: TStringList;
  Index: integer;
  SfrParameterInstances: TSfrParamInstances;
  Item: TSfrParamInstance;
  PageIndex: integer;
  Page: TJvCustomPage;
  Frame: TframeSfrParamInstances;
  Row: integer;
begin
  if not frmGoPhast.PhastModel.ModflowPackages.SfrPackage.IsSelected then
  begin
    FSfrParameterInstances.Free;
    FSfrParameterInstances := TSfrParamInstances.Create(nil);
    Exit;
  end;

  Assert(jplSfrParameters.PageCount =
    frameSFRParameterDefinition.seNumberOfParameters.AsInteger);
  ParameterNames := TStringList.Create;
  try
    ParameterNames.Assign(
      frameSFRParameterDefinition.dgParameters.Cols[Ord(pcName)]);
    ParameterNames.Delete(0);

    SfrParameterInstances :=
      frmGoPhast.PhastModel.ModflowPackages.SfrPackage.ParameterInstances;
    FSfrParameterInstances.Free;
    FSfrParameterInstances := TSfrParamInstances.Create(nil);
    FSfrParameterInstances.Assign(SfrParameterInstances);

//    if ParameterNames.Count = 0 then
//    begin
//      FSfrParameterInstances.Clear;
//    end;

    for Index := 0 to FSfrParameterInstances.Count - 1 do
    begin
      Item := FSfrParameterInstances.Items[Index];
      PageIndex := ParameterNames.IndexOf(Item.ParameterName);
      if PageIndex >= 0 then
      begin
        Assert(PageIndex >= 0);
        Page := jplSfrParameters.Pages[PageIndex];
        Assert(Page.ControlCount= 1);
        Page.Handle;
        Frame := Page.Controls[0] as TframeSfrParamInstances;
        Frame.seInstanceCount.AsInteger := Frame.seInstanceCount.AsInteger +1;
        Row := Frame.seInstanceCount.AsInteger;
        Frame.rdgSfrParamInstances.Cells[Ord(sicStartTime), Row] :=
          FloatToStr(Item.StartTime);
        Frame.rdgSfrParamInstances.Cells[Ord(sicEndTime), Row] :=
          FloatToStr(Item.EndTime);
        Frame.rdgSfrParamInstances.Cells[Ord(sicInstanceName), Row] :=
          Item.ParameterInstance;
        Frame.rdgSfrParamInstances.Objects[Ord(sicInstanceName), Row] := Item;
      end;
    end;
  finally
    ParameterNames.Free;
  end;
end;

procedure TfrmModflowPackages.ReadPackages;
var
  Index: Integer;
  APackage: TModflowPackageSelection;
  Frame: TframePackage;
  Page: TJvStandardPage;
  PriorNode: TTreeNode;
  ParentNode: TTreeNode;
  ChildNode: TTreeNode;
  NodeIndex: integer;
  AControl: TControl;
  FTreeNodeList: TStringList;
  Link: TFrameNodeLink;
  AltNodeIndex: Integer;
  AltParentNode: TTreeNode;
  AltChildNode: TTreeNode;
  FGwtSrcNode: TTreeNode;
//  MstIndex: Integer;
  procedure AddNode(const Key, Caption: string; var PriorNode: TTreeNode);
  begin
    PriorNode := tvPackages.Items.Add(PriorNode, Caption);
    FTreeNodeList.AddObject(Key, PriorNode);
  end;
  function AddChildNode(const Key, Caption: string; ParentNode: TTreeNode): TTreeNode;
  var
    ChildNode: TTreeNode;
  begin
    ChildNode := tvPackages.Items.AddChild(ParentNode, Caption);
    FTreeNodeList.AddObject(Key, ChildNode);
    result := ChildNode;
  end;
  function InsertSiblingNode(SiblingNode: TTreeNode; const Caption: string): TTreeNode;
  var
    ChildNode: TTreeNode;
  begin
    ChildNode := tvPackages.Items.Insert(SiblingNode, Caption);
    result := ChildNode;
  end;

begin
  frameSFRParameterDefinition.seNumberOfParameters.AsInteger := 0;
  frameSFRParameterDefinitionseNumberOfParametersChange(
    frameSFRParameterDefinition.seNumberOfParameters);

  FSteadyParameters.Assign(frmGoPhast.PhastModel.ModflowSteadyParameters);
  FTransientListParameters.Assign(frmGoPhast.PhastModel.ModflowTransientParameters);
  FHufParameters.Assign(frmGoPhast.PhastModel.HufParameters);

  AddPackagesToList(frmGoPhast.PhastModel.ModflowPackages);

  tvPackages.Items.Clear;
  FTreeNodeList := TStringList.Create;
  try

    PriorNode := nil;
    AddNode(StrFlowPackages, StrFlowPackages, PriorNode);
    AddNode(StrBoundaryCondition, StrBoundaryCondition, PriorNode);
    AddChildNode(BC_SpecHead, StrSpecifiedHeadPackages, PriorNode);
    AddChildNode(BC_SpecifiedFlux, StrSpecifiedFlux, PriorNode);
    AddChildNode(BC_HeadDependentFlux, StrHeaddependentFlux, PriorNode);
    AddNode(StrSolver, StrSolver, PriorNode);
    AddNode(StrSubSidence, StrSubSidence, PriorNode);
    AddNode(StrObservations, StrObservations, PriorNode);
    if frmGoPhast.ModelSelection <> msModflow2015 then
    begin
      AddNode(StrOutput, StrOutput, PriorNode);
    end;

    if frmGoPhast.ModelSelection in [msModflow, msModflowNWT, msModflowFMP
          {$IFDEF OWHMV2}
          , msModflowOwhm2
          {$ENDIF}
    ] then
    begin
      AddNode(StrSurfaceWaterRoutin, StrSurfaceWaterRoutin, PriorNode);
    end;

    if frmGoPhast.ModelSelection = msModflowCFP then
    begin
      AddNode(StrConduitFlowProcess, StrConduitFlowProcess, PriorNode);
    end;

    if frmGoPhast.ModelSelection in [msModflowFMP
    {$IFDEF OWHMV2}
    , msModflowOwhm2
    {$ENDIF}
    ] then
    begin
      AddNode(StrFarmProcess, StrFarmProcess, PriorNode);
    end;

    AddNode(StrPostProcessors, StrPostProcessors, PriorNode);

    AddNode(StrGroundwaterTranspor, StrGroundwaterTranspor, PriorNode);
    FTransportNode := PriorNode;

    FChemNode := AddChildNode(StrChemSpecies, StrChemSpeciesMT3DA,
      FTransportNode);
    FChemNode.Data := jvspChemSpecies;

    if frmGoPhast.ModelSelection = msModflow2015  then
    begin
      FGwtParentNode := AddChildNode(StrGwtClassification,
        StrGwtClassification, FTransportNode);

      FMstNode := AddChildNode(StrGwtMST, StrMSTMobileStorage,
        FGwtParentNode);
      FIstNode := AddChildNode(StrGwtIST, StrISTImmobileStorag,
        FGwtParentNode);
      FGwtImsNode := AddChildNode(StrGwtSolver, StrIMSIterativeModel,
        FGwtParentNode);
    end
    else
    begin
      FGwtParentNode := nil;
      FMstNode := nil;
      FIstNode := nil;
      FGwtImsNode := nil;
    end;

    AddChildNode(StrMt3dClassification,
      StrMt3dClassification, FTransportNode);

//    AddNode(StrMT3DMS_GWT_Classificaton, StrMT3DMS_GWT_Classificaton, PriorNode);

    NilNodes;

    FGwtSrcNode := nil;
    FFrameNodeLinks.Clear;
    FLinkDictionary.Clear;
    for Index := 0 to FPackageList.Count - 1 do
    begin
      APackage := FPackageList[Index];

      Frame := APackage.Frame;
      Assert(Frame <> nil);
      NodeIndex := FTreeNodeList.IndexOf(APackage.Classification);
      Assert(NodeIndex >= 0, Format(StrSWasNotFound, [APackage.Classification]));
      ParentNode := FTreeNodeList.Objects[NodeIndex] as TTreeNode;
      if APackage.SpeciesIndex >= 0 then
      begin
        ChildNode := tvPackages.Items.AddChild(ParentNode,
          frameChemSpecies.frameGridMobile.Grid.Cells[0,APackage.SpeciesIndex+1]);
        if Frame = frameGwtSRC then
        begin
          FGwtSrcNode := ChildNode;
        end;
      end
      else
      begin
        if (ParentNode = FGwtParentNode) and (FGwtParentNode <> nil) then
        begin
          Assert(FMstNode <> nil);
          ChildNode := InsertSiblingNode(FMstNode, APackage.PackageIdentifier)
        end
        else
        begin
          ChildNode := tvPackages.Items.AddChild(ParentNode, APackage.PackageIdentifier);
        end;
      end;

      if APackage.AlternativeClassification = '' then
      begin
        AltChildNode := nil;
      end
      else
      begin
        AltNodeIndex := FTreeNodeList.IndexOf(APackage.AlternativeClassification);
        Assert(AltNodeIndex >= 0, Format(StrSWasNotFound, [APackage.AlternativeClassification]));
        AltParentNode := FTreeNodeList.Objects[AltNodeIndex] as TTreeNode;
        AltChildNode := tvPackages.Items.AddChild(AltParentNode, APackage.PackageIdentifier);
      end;

      Link := TFrameNodeLink.Create;
      Link.Frame := Frame;
      Link.Node := ChildNode;
      Link.AlternateNode := AltChildNode;
      FFrameNodeLinks.Add(Link);
      FLinkDictionary.Add(Link.Frame, Link);

      AControl := Frame;
      Page := nil;
      while AControl.Parent <> nil do
      begin
        AControl := AControl.Parent;
        if AControl is TJvStandardPage then
        begin
          Page := TJvStandardPage(AControl);
        end;
      end;
      Assert(Page <> nil);
      ChildNode.Data := Page;
      if AltChildNode <> nil then
      begin
        AltChildNode.Data := Page;
      end;
      APackage.Node := ChildNode;
      APackage.AlternateNode := AltChildNode;
      Frame.GetData(APackage);
    end;
  finally
    FTreeNodeList.Free;
  end;

  if FGwtSrcNode <> nil then
  begin

  end;

  FillLpfTree;
  FillHufTree;
  FillTransientGrids;
  FillHfbGrid;

  GetSfrParamInstances;
end;

procedure TfrmModflowPackages.GetData;
begin
  framePkgMt3dBasic.OnEnableTimeControls := EnableMt3dTimeControls;
  frameChemSpecies.OnEnableTimeControls := EnableMt3dTimeControls;

  frameFmpParameterDefinition.Parent := framePkgFrm.jvspParameters;
  frameFmpParameterDefinition.Align := alClient;

  IsLoaded := False;
  try
    StorePackages;
    FPackageList.Free;
    FPackageList := TList.Create;

//    AddPackagesToList(frmGoPhast.PhastModel.ModflowPackages);
//    NilNodes;

    FSteadyParameters.Free;
    FSteadyParameters := TModflowSteadyParameters.Create(nil);
    FHufParameters.Free;
    FHufParameters := THufModflowParameters.Create(nil);
    FTransientListParameters.Free;
    FTransientListParameters := TModflowTransientListParameters.Create(nil);
    pcSFR.ActivePageIndex := 0;
    framePkgLPF.OnSelectedChange := framePkgLPFSelectedChange;
    framePkgCHD.OnSelectedChange := ChdSelectedChange;
    framePkgDRN.OnSelectedChange := DrnSelectedChange;
    framePkgGHB.OnSelectedChange := GhbSelectedChange;
    framePkgRIV.OnSelectedChange := RivSelectedChange;
    framePkgStr.OnSelectedChange := StrSelectedChange;
    framePkgNwt.OnSelectedChange := NwtSelectedChange;
    framePkgUpw.OnSelectedChange := UpwSelectedChange;
    framePkgFMP4.OnSelectedChange := Fmp4SelectedChange;
    framePkgCHOB.CanSelect := False;
    framePkgDROB.CanSelect := False;
    framePkgGBOB.CanSelect := False;
    framePkgRVOB.CanSelect := False;
    framePkgSTOB.CanSelect := False;
    framePkgMt3dBasic.OnSelectedChange :=  Mt3dmsBasicSelectedChange;
    framePkgUZF.OnSelectedChange := UzfSelectedChange;
    frameMt3dmsGcgPackage.CanSelect := False;
    frameMt3dmsGcgPackage.OnSelectedChange := Mt3dmsGcgSelectedChange;
    frameMt3dmsAdvPkg.CanSelect := False;
    frameMt3dmsDispersionPkg.CanSelect := False;
    FframePackageMSTObjectList.Clear;
    FframePackageIstObjectList.Clear;
    FframePkgSmsObjectList.Clear;
    ReadPackages;
    comboModel.ItemIndex := 0;
    comboModelChange(nil);
    if frmGoPhast.ModelSelection = msModflow2015  then
    begin
      jvplPackages.ActivePage := jvspNPF;
    end
    else
    begin
      if frmGoPhast.PhastModel.ModflowPackages.LpfPackage.IsSelected then
      begin
        jvplPackages.ActivePage := jvspLPF;
      end
      else if frmGoPhast.PhastModel.ModflowPackages.HufPackage.IsSelected then
      begin
        jvplPackages.ActivePage := jvspHUF;
      end
      else if frmGoPhast.PhastModel.ModflowPackages.BcfPackage.IsSelected then
      begin
        jvplPackages.ActivePage := jvspBCF;
      end
      else if frmGoPhast.PhastModel.ModflowPackages.UpwPackage.IsSelected then
      begin
        jvplPackages.ActivePage := jvspUPW;
      end
      else
      begin
        jvplPackages.ActivePage := jvspLPF;
      end;
    end;
    jvplPackagesChange(nil);
    HelpKeyword := 'MODFLOW_Packages_Dialog_Box';
    framePkgLPFSelectedChange(nil);
    ChdSelectedChange(nil);
    DrnSelectedChange(nil);
    GhbSelectedChange(nil);
    RivSelectedChange(nil);

    frameChemSpecies.GetMt3dmsChemSpecies(
      frmGoPhast.PhastModel.MobileComponents,
      frmGoPhast.PhastModel.ImmobileComponents);

    frameSFRParameterDefinitiondgParametersSetEditText(
      frameSFRParameterDefinition.dgParameters, 0, 1,
      frameSFRParameterDefinition.dgParameters.Cells[0, 1]);

    pnlModel.Visible := frmGoPhast.PhastModel.LgrUsed;
  finally
    IsLoaded := True;
  end;

  UpdateGwtFrames;
end;

function TfrmModflowPackages.CreateMstFrame: TframePackageMST;
var
  NewPage: TJvStandardPage;
  MemoWidth: Integer;
begin
  result := TframePackageMST.Create(nil);
  result.Selected := True;
  FframePackageMSTObjectList.Add(result);
  NewPage := TJvStandardPage.Create(self);
  NewPage.Name := '';
  NewPage.PageList := jvplPackages;
  NewPage.HelpKeyword := 'MST-Mobile-Storage-and-Transfe';
  result.Parent := NewPage;
  MemoWidth := Max(result.MemoComments.Width, 467);
  result.Align := alClient;
  // If this isn't done, the memo may be invisible because its width
  // ends up less than zero.
  if result.Width > 0 then
  begin
    MemoWidth := Max(result.Width - 21, 467);;
  end;
  result.MemoComments.Width := MemoWidth;
  result.MemoComments.Height := 89;
  result.rgPorosity.Width := MemoWidth;
  result.rgSorption.Width := MemoWidth;
  result.rgDecay.Width := MemoWidth;
end;

procedure TfrmModflowPackages.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
  Params.WndParent := 0;
end;

procedure TfrmModflowPackages.NilNodes;
var
  AFrame: TframePackage;
  FrameIndex: Integer;
  APackage: TModflowPackageSelection;
begin
  for FrameIndex := 0 to FPackageList.Count - 1 do
  begin
    APackage := FPackageList[FrameIndex];
    AFrame := APackage.Frame;
    AFrame.NilNode;
  end;
end;

procedure TfrmModflowPackages.EnableFarmPrintRouting;
begin
  framePkgFrm.frameRoutingInformationPrintFlag.Enabled :=
    framePkgFrm.rcSelectionController.Enabled
    and framePkgSFR.rcSelectionController.Enabled;
  if framePkgFrm.frameRoutingInformationPrintFlag.Enabled then
  begin
    framePkgFrm.frameRoutingInformationPrintFlag.rdgGrid.Color := clWindow;
  end
  else
  begin
    framePkgFrm.frameRoutingInformationPrintFlag.rdgGrid.Color := clBtnFace;
  end;
end;

procedure TfrmModflowPackages.EnableGwtPackages;
var
  CanSelect: Boolean;
  index: Integer;
  MstFrame: TframePackageMST;
  IstFrame: TframePackageIst;
  ImsFrame: TframePkgSms;
begin
  CanSelect := frameGwtProcess.rcSelectionController.Enabled;

  frameGwtAdv.CanSelect := CanSelect;
  if not frameGwtAdv.CanSelect then
  begin
    frameGwtAdv.Selected := False;
  end;

  frameGwtCNC.CanSelect := CanSelect;
  if not frameGwtCNC.CanSelect then
  begin
    frameGwtCNC.Selected := False;
  end;

  frameGwtDsp.CanSelect := CanSelect;
  if not frameGwtDsp.CanSelect then
  begin
    frameGwtDsp.Selected := False;
  end;

  frameGwtSRC.CanSelect := CanSelect;
  if not frameGwtSRC.CanSelect then
  begin
    frameGwtSRC.Selected := False;
  end;

  frameGwtSSM.CanSelect := CanSelect;
  if not frameGwtSSM.CanSelect then
  begin
    frameGwtSSM.Selected := False;
  end;

  for index := 0 to FframePackageMSTObjectList.Count - 1 do
  begin
    MstFrame := FframePackageMSTObjectList[index];
    MstFrame.CanSelect := CanSelect;
    if not MstFrame.CanSelect then
    begin
      MstFrame.Selected := False;
    end;
  end;

  for index := 0 to FframePackageIstObjectList.Count - 1 do
  begin
    IstFrame := FframePackageIstObjectList[index];
    IstFrame.CanSelect := CanSelect;
    if not IstFrame.CanSelect then
    begin
      IstFrame.Selected := False;
    end;
  end;

  for index := 0 to FframePkgSmsObjectList.Count - 1 do
  begin
    ImsFrame := FframePkgSmsObjectList[index];
    ImsFrame.CanSelect := CanSelect;
//    if not ImsFrame.CanSelect then
//    begin
//      ImsFrame.Selected := False;
//    end;
  end;
end;

procedure TfrmModflowPackages.EnableLakeTransport;
var
  CanSelect: Boolean;
begin
  CanSelect := framePkgMt3dBasic.rcSelectionController.Enabled
    and (framePkgMt3dBasic.comboVersion.ItemIndex = 0);
  if frmGoPhast.ModelSelection <> msModflowNWT then
  begin
    // LKT is not currently supported by MODFLOW 6.
//    CanSelect := CanSelect and framePackageLakMf6.rcSelectionController.Enabled;
    CanSelect := False;
  end
  else
  begin
    CanSelect := CanSelect and framePkgLAK.rcSelectionController.Enabled;
  end;
  frameMt3dLktPkg.CanSelect := CanSelect;
  if not frameMt3dLktPkg.CanSelect then
  begin
    frameMt3dLktPkg.Selected := False;
  end;
end;

procedure TfrmModflowPackages.EnableLpfParameterControls;
begin
  if framePkgLPF.rcSelectionController.Enabled
    and (jvplPackages.ActivePage = jvspLPF) then
  begin
    frameLpfParameterDefinition.Enabled := True;
  end
  else if framePkgUpw.rcSelectionController.Enabled
    and (jvplPackages.ActivePage = jvspUPW) then
  begin
    frameLpfParameterDefinition.Enabled := True;
  end
  else
  begin
    frameLpfParameterDefinition.Enabled := False;
  end;
  tvLpfParameterTypes.Enabled := frameLpfParameterDefinition.Enabled;
end;

procedure TfrmModflowPackages.EnableMt3dTimeControls(Sender: TObject);
var
  ShouldEnable: Boolean;
begin
  ShouldEnable := frameChemSpecies.Mt3dTimeControlsShouldBeEnabled;
  framePkgMt3dBasic.SetTimeControlsEnabled(ShouldEnable);
end;

procedure TfrmModflowPackages.StoreFrameDataInPackages(
  Packages: TModflowPackages);
var
  Index: Integer;
  APackage: TModflowPackageSelection;
  Frame: TframePackage;
  Link : TFrameNodeLink;
begin
  UpdateGwtFrames;
  AddPackagesToList(Packages);
  for Index := 0 to FPackageList.Count - 1 do
  begin
    APackage := FPackageList[Index];
    Frame := APackage.Frame;
    if not FLinkDictionary.TryGetValue(Frame, Link) then
    begin
      Assert(False);
    end;
//    Link := FFrameNodeLinks[Index];
    Assert(Frame = Link.Frame);
    APackage.Node := Link.Node;
    Frame.SetData(APackage);
  end;
end;

procedure TfrmModflowPackages.StorePackageDataInFrames(
  Packages: TModflowPackages);
var
  Index: Integer;
  APackage: TModflowPackageSelection;
  Frame: TframePackage;
  Link : TFrameNodeLink;
begin
  AddPackagesToList(Packages);
  for Index := 0 to FPackageList.Count - 1 do
  begin
    APackage := FPackageList[Index];
    Frame := APackage.Frame;
    if not FLinkDictionary.TryGetValue(Frame, Link) then
    begin
      Assert(False);
    end;
//    Link := FFrameNodeLinks[Index];
    Assert(Frame = Link.Frame);
    APackage.Node := Link.Node;
    APackage.AlternateNode := Link.AlternateNode;
    Frame.GetData(APackage);
  end;
end;

procedure TfrmModflowPackages.StorePackages;
var
  Item: TTempPackageItem;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  FCurrentPackages := nil;
  FNewPackages.Free;
  comboModel.Items.Clear;
  FNewPackages := TTempPackageCollection.Create;
  Item := FNewPackages.Add;
  Item.Packages.Assign(frmGoPhast.PhastModel.ModflowPackages);
  comboModel.AddItem(StrParentModel, Item.Packages);
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      Item := FNewPackages.Add;
      Item.Packages.Assign(ChildModel.ModflowPackages);
      comboModel.AddItem(ChildModel.ModelName, Item.Packages);
    end;
  end;
end;

procedure TfrmModflowPackages.EnableUnsatTransport;
begin
  if frmGoPhast.ModelSelection <> msModflowNWT then
  begin
    // UZT is not currently supported with MODFLOW 6
//    framePkgMt3dUZT.CanSelect := framePackageUzfMf6.rcSelectionController.Enabled
//      and framePkgMt3dBasic.rcSelectionController.Enabled
//      and (framePkgMt3dBasic.comboVersion.ItemIndex = 0);
    framePkgMt3dUZT.CanSelect := False;
  end
  else
  begin
    framePkgMt3dUZT.CanSelect := framePkgUZF.rcSelectionController.Enabled
      and framePkgMt3dBasic.rcSelectionController.Enabled
      and (framePkgMt3dBasic.comboVersion.ItemIndex = 0);
  end;
  if not framePkgMt3dUZT.CanSelect then
  begin
    framePkgMt3dUZT.Selected := False;
  end;
end;

procedure TfrmModflowPackages.EnableUzfVerticalKSource;
begin
  framePkgUZF.comboVerticalKSource.Enabled :=
    framePkgUZF.rcSelectionController.Enabled
    and not framePkgBcf.rcSelectionController.Enabled;
  framePkgUZF.lblVerticalKSource.Enabled :=
    framePkgUZF.comboVerticalKSource.Enabled;
  if not framePkgUZF.comboVerticalKSource.Enabled then
  begin
    framePkgUZF.comboVerticalKSource.ItemIndex := 0;
  end;
end;

procedure TfrmModflowPackages.UpdateFlowParamGrid(Node: TTreeNode;
  ParameterFrame: TframeListParameterDefinition;
  ParameterCollection: TCollection; ParameterFrameController: TRbwController);
var
  Parameter: TModflowParameter;
  Index: Integer;
  ParamList: TList;
  ALabel: TLabel;
  RowIndex: Integer;
  ActiveGrid: TRbwDataGrid4;
  ActiveFrame: TframeListParameterDefinition;
  seNumberOfParameters: TJvSpinEdit;
begin
  ActiveFrame := ParameterFrame;
  seNumberOfParameters := ActiveFrame.seNumberOfParameters;
  ActiveGrid := ParameterFrame.dgParameters;
  for RowIndex := 1 to ActiveGrid.RowCount - 1 do
  begin
    ActiveGrid.Objects[0, RowIndex] := nil;
  end;
  ALabel := ParameterFrame.lblNumParameters;
  CurrentParameterType := TParameterType(Node.Data);
  Assert(CurrentParameterType in [ptLPF_HK..ptLPF_VKCB, ptHUF_HK..ptHUF_LVDA]);
  case CurrentParameterType of
    ptLPF_HK:
      ALabel.Caption := Format(FormatStr, ['HK']);
    ptLPF_HANI:
      ALabel.Caption := Format(FormatStr, ['HANI']);
    ptLPF_VK:
      ALabel.Caption := Format(FormatStr, ['VK']);
    ptLPF_VANI:
      ALabel.Caption := Format(FormatStr, ['VANI']);
    ptLPF_SS:
      ALabel.Caption := Format(FormatStr, ['SS']);
    ptLPF_SY:
      ALabel.Caption := Format(FormatStr, ['SY']);
    ptLPF_VKCB:
      ALabel.Caption := Format(FormatStr, ['VKCB']);
    ptHUF_HK:
      ALabel.Caption := Format(FormatStr, ['HK']);
    ptHUF_HANI:
      ALabel.Caption := Format(FormatStr, ['HANI']);
    ptHUF_VK:
      ALabel.Caption := Format(FormatStr, ['VK']);
    ptHUF_VANI:
      ALabel.Caption := Format(FormatStr, ['VANI']);
    ptHUF_SS:
      ALabel.Caption := Format(FormatStr, ['SS']);
    ptHUF_SY:
      ALabel.Caption := Format(FormatStr, ['SY']);
    ptHUF_SYTP:
      ALabel.Caption := Format(FormatStr, ['SYTP']);
    ptHUF_KDEP:
      ALabel.Caption := Format(FormatStr, ['KDEP']);
    ptHUF_LVDA:
      ALabel.Caption := Format(FormatStr, ['LVDA']);
  else
    Assert(False);
  end;

  case CurrentParameterType of
    ptHUF_HK, ptHUF_HANI, ptHUF_VK, ptHUF_VANI,
      ptHUF_SS, ptHUF_SY, ptHUF_KDEP:
      begin
        ActiveGrid.ColCount := 2;
      end;
    ptHUF_SYTP, ptHUF_LVDA:
      begin
        ActiveGrid.ColCount := 4;
        ActiveGrid.Columns[Ord(pcUseZone)].Format := rcf4Boolean;
        ActiveGrid.Columns[Ord(pcUseMultiplier)].Format := rcf4Boolean;
        ParameterFrame.Loaded;
      end;
  end;

  ParameterFrameController.Enabled := CurrentParameterType <> ptUndefined;
  if ParameterFrameController.Enabled then
  begin
    ParamList := TList.Create;
    try
      if ParameterCollection <> nil then
      begin
        for Index := 0 to ParameterCollection.Count - 1 do
        begin
          Parameter := ParameterCollection.Items[Index] as TModflowParameter;
          if Parameter.ParameterType = CurrentParameterType then
          begin
            ParamList.Add(Parameter);
          end;
        end;
      end;
      ActiveFrame.PriorNumberOfParameters := ParamList.Count;
      Assert(ActiveGrid <> nil);
      FSettingNumber := True;
      try
        seNumberOfParameters.AsInteger := ParamList.Count;
        if ParamList.Count = 0 then
        begin
          ActiveGrid.RowCount := 2;
        end
        else
        begin
          ActiveGrid.RowCount := ParamList.Count + 1;
        end;
      finally
        FSettingNumber := False;
      end;
      for RowIndex := 1 to ParamList.Count do
      begin
        Parameter := ParamList[RowIndex - 1];
        AssignParameterToRow(ActiveGrid, RowIndex, Parameter);
      end;
      ParameterFrame.PriorNumberOfParameters := ParamList.Count;
      ParameterFrame.seNumberOfParameters.AsInteger := ParamList.Count;
      frameParameterDefinition_seNumberOfParametersChange(ParameterFrame.seNumberOfParameters);
    finally
      ParamList.Free;
    end;
  end;
end;

procedure TfrmModflowPackages.UpdateGwtFrames;
var
  SpeciesIndex: Integer;
  SpeciesName: string;
  ChildNode: TTreeNode;
  Link: TFrameNodeLink;
  MstPackage: TGwtMstPackage;
  IstPackage: TGwtIstPackage;
  AMstFrame: TframePackageMST;
  AnIstframe: TframePackageIst;
  ImsPackage: TSmsPackageSelection;
  AnImsframe: TframePkgSms;
  MemoWidth: Integer;
begin
  if not IsLoaded then
  begin
    Exit;
  end;
  if frameGwtProcess.Selected
    {and (framePkgMt3dBasic.comboVersion.ItemIndex = 2)} then
  begin
    if FCurrentPackages.GwtPackages.Count <
      frameChemSpecies.frameGridMobile.seNumber.AsInteger then
    begin
      FCurrentPackages.GwtPackages.Count :=
        frameChemSpecies.frameGridMobile.seNumber.AsInteger
    end;

    // MST package
    if FMstNode = nil then
    begin
      FMstNode := tvPackages.Items.AddChild(FGwtParentNode, StrMSTMobileStorage);
    end;
    ChildNode := FMstNode.GetFirstChild;
    for SpeciesIndex := 0 to frameChemSpecies.frameGridMobile.seNumber.AsInteger -1 do
    begin
      SpeciesName := frameChemSpecies.frameGridMobile.Grid.Cells[0, SpeciesIndex+1];

      if ChildNode = nil then
      begin
        ChildNode := tvPackages.Items.AddChild(FMstNode, SpeciesName);
      end
      else
      begin
        ChildNode.Text := SpeciesName;
      end;

      MstPackage := FCurrentPackages.GwtPackages[SpeciesIndex].GwtMst;
//      ChildNode.Data := MstPackage
                        ;
      if SpeciesIndex < FframePackageMSTObjectList.Count then
      begin
        AMstFrame := FframePackageMSTObjectList[SpeciesIndex];
      end
      else
      begin
        AMstFrame := CreateMstFrame;

        MstPackage.Node := ChildNode;
        MemoWidth := AMstFrame.MemoComments.Width;
        AMstFrame.GetData(MstPackage);
        AMstFrame.MemoComments.Width := MemoWidth;

        Link := TFrameNodeLink.Create;
        Link.Frame := AMstFrame;
        Link.Node := ChildNode;
        Link.AlternateNode := ChildNode;
        FFrameNodeLinks.Add(Link);
        FLinkDictionary.Add(Link.Frame, Link);
      end;
      ChildNode.Data := AMstFrame.Parent;

      ChildNode := ChildNode.GetnextSibling;
    end;

    // IST Package
    if FIstNode = nil then
    begin
      FIstNode := tvPackages.Items.AddChild(FGwtParentNode, StrISTImmobileStorag);
    end;

    ChildNode := FIstNode.GetFirstChild;
    for SpeciesIndex := 0 to frameChemSpecies.frameGridMobile.seNumber.AsInteger -1 do
    begin
      SpeciesName := frameChemSpecies.frameGridMobile.Grid.Cells[0, SpeciesIndex+1];

      if ChildNode = nil then
      begin
        ChildNode := tvPackages.Items.AddChild(FIstNode, SpeciesName);
      end
      else
      begin
        ChildNode.Text := SpeciesName;
      end;

      IstPackage := FCurrentPackages.GwtPackages[SpeciesIndex].GwtIst;
//      ChildNode.Data := IstPackage
                        ;
      if SpeciesIndex < FframePackageISTObjectList.Count then
      begin
        AnIstframe := FframePackageISTObjectList[SpeciesIndex];
      end
      else
      begin
        AnIstframe := CreateIstFrame;

        IstPackage.Node := ChildNode;
        AnIstframe.GetData(IstPackage);

        Link := TFrameNodeLink.Create;
        Link.Frame := AnIstframe;
        Link.Node := ChildNode;
        Link.AlternateNode := ChildNode;
        FFrameNodeLinks.Add(Link);
        FLinkDictionary.Add(Link.Frame, Link);

      end;
      ChildNode.Data := AnIstframe.Parent;

      ChildNode := ChildNode.GetnextSibling;
    end;

    // IMS Packages
    if FGwtImsNode = nil then
    begin
      FGwtImsNode := tvPackages.Items.AddChild(FGwtParentNode, StrIMSIterativeModel);
    end;

    ChildNode := FGwtImsNode.GetFirstChild;
    for SpeciesIndex := 0 to frameChemSpecies.frameGridMobile.seNumber.AsInteger -1 do
    begin
      SpeciesName := frameChemSpecies.frameGridMobile.Grid.Cells[0, SpeciesIndex+1];

      if ChildNode = nil then
      begin
        ChildNode := tvPackages.Items.AddChild(FGwtImsNode, SpeciesName);
      end
      else
      begin
        ChildNode.Text := SpeciesName;
      end;

      ImsPackage := FCurrentPackages.GwtPackages[SpeciesIndex].GwtIms;
//      ChildNode.Data := ImsPackage

      if SpeciesIndex < FframePkgSmsObjectList.Count then
      begin
        AnImsframe := FframePkgSmsObjectList[SpeciesIndex];
      end
      else
      begin
        AnImsframe := CreateImsGwtFrame;

        ImsPackage.Node := ChildNode;
        // New frames get their value from the flow model
        // IMS frame.
        AnImsframe.GetData(ImsPackage);
        AnImsframe.AssignFrame(framePkgIMS);

        Link := TFrameNodeLink.Create;
        Link.Frame := AnImsframe;
        Link.Node := ChildNode;
        Link.AlternateNode := ChildNode;
        FFrameNodeLinks.Add(Link);
        FLinkDictionary.Add(Link.Frame, Link);
      end;
      ChildNode.Data := AnImsframe.Parent;

      ChildNode := ChildNode.GetnextSibling;
    end
  end;
  EnableGwtPackages;
end;

procedure TfrmModflowPackages.EnableRchModpathOption;
begin
  frameModpath.comboRchSource.Enabled :=
    frameModpath.rcSelectionController.Enabled
    and framePkgRCH.rcSelectionController.Enabled
    and (frmGoPhast.ModelSelection <> msModflow2015);
end;

procedure TfrmModflowPackages.EnableChemSpecies;
begin
  frameChemSpecies.frameGridMobile.Enabled :=
    frameGwtProcess.rcSelectionController.Enabled
    or framePkgMt3dBasic.rcSelectionController.Enabled;
  frameChemSpecies.frameGridImmobile.Enabled :=
    framePkgMt3dBasic.rcSelectionController.Enabled;
  if IsLoaded and frameChemSpecies.frameGridMobile.Enabled then
  begin
    if (frameChemSpecies.frameGridMobile.seNumber.AsInteger = 0) then
    begin
      frameChemSpecies.frameGridMobile.seNumber.AsInteger := 1;
      if Assigned(frameChemSpecies.frameGridMobile.seNumber.OnChange) then
      begin
        frameChemSpecies.frameGridMobile.seNumber.OnChange(Self)
      end;
    end;
    if (frameChemSpecies.frameGridMobile.Grid.Cells[0,1] = '') then
    begin
      frameChemSpecies.frameGridMobile.Grid.Cells[0,1] := 'Chem';
      if Assigned(frameChemSpecies.frameGridMobile.Grid.OnSetEditText) then
      begin
        frameChemSpecies.frameGridMobile.Grid.OnSetEditText(Self, 0, 1,  'Chem');
      end;
    end;
    UpdateGwtFrames;
  end;
end;

procedure TfrmModflowPackages.EnableConduitRecharge;
begin
  framePkgCFP.cbConduitRecharge.Enabled :=
    framePkgCFP.rcSelectionController.Enabled
    and framePkgCFP.cbPipes.Checked
    and framePkgRCH.rcSelectionController.Enabled;
end;

procedure TfrmModflowPackages.EnableContaminantTreatmentSystem;
var
  CanSelect: Boolean;
begin
  CanSelect := framePkgMt3dBasic.rcSelectionController.Enabled
    and (framePkgMt3dBasic.comboVersion.ItemIndex = 0);
//  if frmGoPhast.ModelSelection = msModflow2015 then
//  begin
//    CanSelect := False;
//  end;
  frameMt3dCtsPkg.CanSelect := CanSelect;
  if not frameMt3dCtsPkg.CanSelect then
  begin
    frameMt3dCtsPkg.Selected := False;
  end;
end;

procedure TfrmModflowPackages.EnableEvtModpathOption;
begin
  frameModpath.comboEvtSink.Enabled :=
    frameModpath.rcSelectionController.Enabled
    and framePkgEVT.rcSelectionController.Enabled
    and (frmGoPhast.ModelSelection <> msModflow2015);
end;

procedure TfrmModflowPackages.jvplPackagesChange(Sender: TObject);
begin
  inherited;
  if jvplPackages.ActivePage = jvspNPF then
  begin
    framePkgNpf.rdgOptions.HideEditor;
  end;
  if (jvplPackages.ActivePage = jvspLPF) then
  begin
    framePkgLPF.rdgOptions.HideEditor;
  end;

  if (jvplPackages.ActivePage = jvspLPF) or (jvplPackages.ActivePage = jvspUPW) then
  begin
    tvLpfParameterTypes.Parent := jvplPackages.ActivePage;
    splitLprParameter.Parent := jvplPackages.ActivePage;
    frameLpfParameterDefinition.Parent := jvplPackages.ActivePage;
    EnableLpfParameterControls;
  end;
  if jvplPackages.ActivePage = jvspCHD then
  begin
    CurrentParameterType := ptCHD;
  end
  else if jvplPackages.ActivePage = jvspDRN then
  begin
    CurrentParameterType := ptDrn;
  end
  else if jvplPackages.ActivePage = jvspDRT then
  begin
    CurrentParameterType := ptDrt;
  end
  else if jvplPackages.ActivePage = jvspGHB then
  begin
    CurrentParameterType := ptGhb;
  end
  else if (jvplPackages.ActivePage = jvspLPF) or (jvplPackages.ActivePage = jvspUPW) then
  begin
    if tvLpfParameterTypes.Selected = nil then
    begin
      CurrentParameterType := ptUndefined;
//      Assert(False);
    end
    else
    begin
      CurrentParameterType := TParameterType(tvLpfParameterTypes.Selected.Data);
    end;
  end
  else if jvplPackages.ActivePage = jvspHUF then
  begin
    if tvHufParameterTypes.Selected = nil then
    begin
      CurrentParameterType := ptUndefined;
      Assert(False);
    end
    else
    begin
      CurrentParameterType := TParameterType(tvHufParameterTypes.Selected.Data);
    end;
  end
  else if jvplPackages.ActivePage = jvspPCG then
  begin
    CurrentParameterType := ptUndefined;
  end
  else if jvplPackages.ActivePage = jvspRIV then
  begin
    CurrentParameterType := ptRiv;
  end
  else if jvplPackages.ActivePage = jvspSTR then
  begin
    CurrentParameterType := ptStr;
  end
  else if jvplPackages.ActivePage = jvspWEL then
  begin
    CurrentParameterType := ptQ;
  end
  else if jvplPackages.ActivePage = jvspRCH then
  begin
    CurrentParameterType := ptRCH;
  end
  else if jvplPackages.ActivePage = jvspEVT then
  begin
    CurrentParameterType := ptEVT;
  end
  else if jvplPackages.ActivePage = jvspETS then
  begin
    CurrentParameterType := ptETS;
  end
  else if jvplPackages.ActivePage = jvspSFR then
  begin
    CurrentParameterType := ptSFR;
  end
  else if jvplPackages.ActivePage = jvspHFB then
  begin
    CurrentParameterType := ptHFB;
  end
  else if jvplPackages.ActivePage = jvspNWT then
  begin
    CurrentParameterType := ptUndefined;
  end
  else if jvplPackages.ActivePage = jvspPCGN then
  begin
    CurrentParameterType := ptUndefined;
  end
  else if jvplPackages.ActivePage = jvspFMP then
  begin
    CurrentParameterType := ptQMAX;
  end
  else
  begin
    CurrentParameterType := ptUndefined;
  end;

  HelpKeyword := jvplPackages.ActivePage.HelpKeyword;
end;

procedure TfrmModflowPackages.jvspGMGShow(Sender: TObject);
begin
  inherited;
  framePkgGMG.MoveControlsToTab(framePkgGMG.pcGMG);

end;

procedure TfrmModflowPackages.jvspNPFShow(Sender: TObject);
begin
  inherited;
  framePkgNpf.rdgOptions.HideEditor;
end;

procedure TfrmModflowPackages.jvspSUBShow(Sender: TObject);
begin
  inherited;
  framePkgSUB.MoveControlsToTab(framePkgSUB.pcSub);

end;

procedure TfrmModflowPackages.jvspSWRShow(Sender: TObject);
begin
  inherited;
  framePkgSWR.MoveControlsToTab(framePkgSWR.jvplSwr);

end;

procedure TfrmModflowPackages.jvspSWTShow(Sender: TObject);
begin
  inherited;
  framePkgSwt.MoveControlsToTab(framePkgSwt.pcSWT);

end;

procedure TfrmModflowPackages.Mt3dmsGcgSelectedChange(Sender: TObject);
begin
  if frameMt3dmsGcgPackage.Selected <> framePkgMt3dBasic.Selected then
  begin
    frameMt3dmsGcgPackage.Selected := framePkgMt3dBasic.Selected;
  end;
end;

procedure TfrmModflowPackages.UzfSelectedChange(Sender: TObject);
begin
  EnableUnsatTransport;
end;

procedure TfrmModflowPackages.Mt3dmsBasicSelectedChange(Sender: TObject);
begin
  ValidMt3D;
  frameMt3dmsGcgPackage.CanSelect := framePkgMt3dBasic.Selected;
  frameMt3dmsGcgPackage.Selected := framePkgMt3dBasic.Selected;

  frameMt3dmsAdvPkg.CanSelect := framePkgMt3dBasic.Selected;
  if not frameMt3dmsAdvPkg.CanSelect then
  begin
    frameMt3dmsAdvPkg.Selected := False;
  end;

  frameMt3dmsDispersionPkg.CanSelect := framePkgMt3dBasic.Selected;
  if not frameMt3dmsDispersionPkg.CanSelect then
  begin
    frameMt3dmsDispersionPkg.Selected := False;
  end;

  framePkgSSM.CanSelect := framePkgMt3dBasic.Selected;
  if not framePkgSSM.CanSelect then
  begin
    framePkgSSM.Selected := False;
  end;

  framePkgMt3dmsRct.CanSelect := framePkgMt3dBasic.Selected;
  if not framePkgMt3dmsRct.CanSelect then
  begin
    framePkgMt3dmsRct.Selected := False;
  end;

  framePkgMt3dmsTob.CanSelect := framePkgMt3dBasic.Selected;
  if not framePkgMt3dmsTob.CanSelect then
  begin
    framePkgMt3dmsTob.Selected := False;
  end;

  EnableUnsatTransport;
  EnableLakeTransport;
  EnableStreamTransport;
  EnableContaminantTreatmentSystem;

  UpdateGwtFrames;
end;

function TfrmModflowPackages.NewParameterName: string;
var
  Root: string;
  Index: Integer;
  Param: TModflowParameter;
//  UpRoot, ParamUpRoot: string;
//  CountString: string;
//  Count,
  MaxCount: integer;
begin
  case CurrentParameterType of
    ptUndefined: Assert(False);
    ptLPF_HK: Root := 'HK_Par';
    ptLPF_HANI: Root := 'HANI_Par';
    ptLPF_VK: Root := 'VK_Par';
    ptLPF_VANI: Root := 'VANI_Par';
    ptLPF_SS: Root := 'SS_Par';
    ptLPF_SY: Root := 'SY_Par';
    ptLPF_VKCB: Root := 'VKCB_Par';
    ptRCH: Root := 'RCH_Par';
    ptEVT: Root := 'EVT_Par';
    ptETS: Root := 'ETS_Par';
    ptCHD: Root := 'CHD_Par';
    ptGHB: Root := 'GHB_Par';
    ptQ: Root := 'Q_Par';
    ptRIV: Root := 'RIV_Par';
    ptDRN: Root := 'DRN_Par';
    ptDRT: Root := 'DRT_Par';
    ptSFR: Root := 'SFR_Par';
    ptHFB: Root := 'HFB_Par';
    ptHUF_HK: Root := 'HK_Par';
    ptHUF_HANI: Root := 'HANI_Par';
    ptHUF_VK: Root := 'VK_Par';
    ptHUF_VANI: Root := 'VANI_Par';
    ptHUF_SS: Root := 'SS_Par';
    ptHUF_SY: Root := 'SY_Par';
    ptHUF_SYTP: Root := 'SYTP_Par';
    ptHUF_KDEP: Root := 'KDEP_Par';
    ptHUF_LVDA: Root := 'LVDA_Par';
    ptSTR: Root := 'STR_Par';
    ptQMAX: Root := 'QMAX_Par';
    ptPEST: Root := 'PEST_Par';
    else Assert(False);
  end;
//  UpRoot := UpperCase(Root);
  MaxCount := 0;
  case CurrentParameterType of
    ptUndefined: Assert(False);
    ptLPF_HK..ptLPF_VKCB, ptHFB, ptHUF_SYTP, ptHUF_LVDA, ptPEST:
      begin
        for Index := 0 to FSteadyParameters.Count - 1 do
        begin
          Param := FSteadyParameters[Index];
          if Param.ParameterType = CurrentParameterType then
          begin
            Inc(MaxCount);
          end;
//          ParamUpRoot := UpperCase(Param.ParameterName);
//          if Pos(UpRoot, ParamUpRoot) > 0 then
//          begin
//            CountString := Copy(ParamUpRoot, Length(UpRoot)+1, MAXINT);
//            if TryStrToInt(CountString, Count) then
//            begin
//              if Count > MaxCount then
//              begin
//                MaxCount := Count;
//              end;
//            end;
//          end;
        end;
      end;
    ptCHD..ptSFR, ptRCH, ptEVT, ptETS, ptSTR, ptQMAX:
      begin
        for Index := 0 to FTransientListParameters.Count - 1 do
        begin
          Param := FTransientListParameters[Index];
          if Param.ParameterType = CurrentParameterType then
          begin
            Inc(MaxCount);
          end;
//          ParamUpRoot := UpperCase(Param.ParameterName);
//          if Pos(UpRoot, ParamUpRoot) > 0 then
//          begin
//            CountString := Copy(ParamUpRoot, Length(UpRoot)+1, MAXINT);
//            if TryStrToInt(CountString, Count) then
//            begin
//              if Count > MaxCount then
//              begin
//                MaxCount := Count;
//              end;
//            end;
//          end;
        end;
      end;
    ptHUF_HK..ptHUF_SY, ptHUF_KDEP:
      begin
        for Index := 0 to FHufParameters.Count - 1 do
        begin
          Param := FHufParameters.Items[Index] as TModflowParameter;
          if Param.ParameterType = CurrentParameterType then
          begin
            Inc(MaxCount);
          end;
//          ParamUpRoot := UpperCase(Param.ParameterName);
//          if Pos(UpRoot, ParamUpRoot) > 0 then
//          begin
//            CountString := Copy(ParamUpRoot, Length(UpRoot)+1, MAXINT);
//            if TryStrToInt(CountString, Count) then
//            begin
//              if Count > MaxCount then
//              begin
//                MaxCount := Count;
//              end;
//            end;
//          end;
        end;
      end;
    else Assert(False);
  end;
//  Inc(MaxCount);
  result := Root + IntToStr(MaxCount);

  While (Length(result) > MaxLengthModflowParameterName) and (Root <> '') do
  begin
    Root := Copy(Root, 1, Length(Root) -1);
    result := Root + IntToStr(MaxCount);
  end;
end;

procedure TfrmModflowPackages.SetCurrentPackages(const Value: TModflowPackages);
var
  PackageIndex: Integer;
  OtherPackages: TTempPackageItem;
begin
  if (FCurrentPackages <> Value) then
  begin
    if (FCurrentPackages <> nil) then
    begin
      StoreFrameDataInPackages(FCurrentPackages);
      for PackageIndex := 0 to FNewPackages.Count - 1 do
      begin
        OtherPackages := FNewPackages[PackageIndex];
        if OtherPackages.Packages <> FCurrentPackages then
        begin
          OtherPackages.Packages.LpfPackage.IsSelected
            := FCurrentPackages.LpfPackage.IsSelected;
          OtherPackages.Packages.BcfPackage.IsSelected
            := FCurrentPackages.BcfPackage.IsSelected;
          OtherPackages.Packages.HufPackage.IsSelected
            := FCurrentPackages.HufPackage.IsSelected;
          OtherPackages.Packages.UpwPackage.IsSelected
            := FCurrentPackages.UpwPackage.IsSelected;
        end;
      end;
    end;

    FCurrentPackages := Value;
    if (FCurrentPackages <> nil) then
    begin
      StorePackageDataInFrames(FCurrentPackages);
    end;
  end;
end;

function TfrmModflowPackages.AreParameterZonesOK: Boolean;
var
  Param: TModflowSteadyParameter;
  Params: array[TParameterType] of TModflowSteadyParameter;
  ParamIndex: Integer;
  PTypeIndex: TParameterType;
begin
  result := True;
  try
    for PTypeIndex := Low(TParameterType) to High(TParameterType) do
    begin
      Params[PTypeIndex] := nil;
    end;
    for ParamIndex := 0 to FSteadyParameters.Count - 1 do
    begin
      Param := FSteadyParameters[ParamIndex];
      if Param.ParameterType = ptPEST then
      begin
        Continue;
      end;
      if Params[Param.ParameterType] <> nil then
      begin
        result := Param.UseZone and Params[Param.ParameterType].UseZone;
        if not result then
        begin
          Exit;
        end;
      end
      else
      begin
        Params[Param.ParameterType] := Param
      end;
    end;
  finally
    if not result then
    begin
      Result := (MessageDlg(StrAtLeastTwoOfYour, mtWarning, [mbYes, mbNo], 0) <> mrYes);
    end;
  end;
end;

procedure TfrmModflowPackages.SetData;
var
  Undo: TUndoChangeLgrPackageSelection;
  ParamGroups: TPestParamGroups;
begin
  SetSfrParamInstances;

  CurrentPackages := nil;
  ParamGroups := nil;

  Undo := TUndoChangeLgrPackageSelection.Create(FSteadyParameters,
    FTransientListParameters, FSfrParameterInstances, FHufParameters,
    ParamGroups, FNewPackages);
  frameChemSpecies.SetMt3dmsChemSpecies(
    frmGoPhast.PhastModel.MobileComponents,
    frmGoPhast.PhastModel.ImmobileComponents);
  Undo.UpdateMt3dmsChemSpecies;

  frmGoPhast.UndoStack.Submit(Undo);
end;


procedure TfrmModflowPackages.tvPackagesChange(Sender: TObject;
  Node: TTreeNode);
var
  Page: TJvCustomPage;
begin
  inherited;
  if (Node <> nil) and (Node.Data <> nil) then
  begin
    Page := Node.Data;
    jvplPackages.ActivePage := Page;
  end;
end;

procedure TfrmModflowPackages.tvPackagesCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  inherited;
  if Node.Selected and not Sender.Focused then
  begin
    Sender.Canvas.Brush.Color := clMenuHighlight;
  end;
end;

function TfrmModflowPackages.SelectNodeOfChildSelectedPackage(Node: TTreeNode): Boolean;
var
  ChildNode: TTreeNode;
  AFrame: TframePackage;
begin
  result := False;
  if Node.Data = nil then
  begin
    ChildNode := Node.getFirstChild;
    while ChildNode <> nil do
    begin
      if ChildNode.Data <> nil then
      begin
        AFrame := NodeToFrame(ChildNode);
        if (AFrame <> nil) and AFrame.Selected then
        begin
          result := True;
          ChildNode.Selected := True;
          break;
        end;
      end
      else
      begin
        result := SelectNodeOfChildSelectedPackage(ChildNode);
        if result then
        begin
          break;
        end;
      end;
      ChildNode := Node.GetNextChild(ChildNode);
    end;
  end;
end;

procedure TfrmModflowPackages.tvPackagesExpanded(Sender: TObject;
  Node: TTreeNode);
begin
  inherited;
  SelectNodeOfChildSelectedPackage(Node);
end;

function TfrmModflowPackages.NodeToFrame(Node: TTreeNode): TFramePackage;
var
  Page, FramePage: TJvStandardPage;
  Frame: TFramePackage;
  Index: Integer;
  Component: TComponent;
  Control: TControl;
  MstIndex: Integer;
  IstIndex: Integer;
begin
  result := nil;
  if Node = FChemNode then
  begin
    Exit;
  end;
  Page := Node.Data;
  for Index := 0 to ComponentCount - 1 do
  begin
    Component := Components[Index];
    if (Component is TFramePackage) then
    begin
      Frame := TFramePackage(Component);
      if (Frame.Parent = Page) then
      begin
        result := Frame;
        Exit;
      end
      else if Frame = framePkgSFR then
      begin
        Control := Frame;
        while Control.Parent <> nil do
        begin
          Control := Control.Parent;
          if Control is TJvStandardPage then
          begin
            FramePage := TJvStandardPage(Control);
            if FramePage = Page then
            begin
              result := Frame;
              Exit;
            end;
          end;
        end;
      end;
    end;
  end;
  for MstIndex := 0 to FframePackageMSTObjectList.Count - 1 do
  begin
    Frame := FframePackageMSTObjectList[MstIndex];
    if (Frame.Parent = Page) then
    begin
      result := Frame;
      Exit;
    end
  end;
  for IstIndex := 0 to FframePackageIstObjectList.Count - 1 do
  begin
    Frame := FframePackageIstObjectList[IstIndex];
    if (Frame.Parent = Page) then
    begin
      result := Frame;
      Exit;
    end
  end;
  for IstIndex := 0 to FframePkgSmsObjectList.Count - 1 do
  begin
    Frame := FframePkgSmsObjectList[IstIndex];
    if (Frame.Parent = Page) then
    begin
      result := Frame;
      Exit;
    end
  end;
  Assert(result <> nil);
end;


procedure TfrmModflowPackages.tvPackagesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HitInfo: THitTests;
begin
  inherited;
  HitInfo := tvPackages.GetHitTestInfoAt(X, Y);
  if (htOnLabel in HitInfo) and (tvPackages.Selected <> nil)
    and (tvPackages.Selected.Count > 0) then
  begin
    tvPackages.Selected.Expanded := not tvPackages.Selected.Expanded ;
  end;
end;

procedure TfrmModflowPackages.tvPackagesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Frame, SelectedFrame: TFramePackage;
  Node, ParentNode, ChildNode: TTreeNode;
begin
  inherited;
  if htOnStateIcon in tvPackages.GetHitTestInfoAt(X, Y) then
  begin
    Node := tvPackages.GetNodeAt(X, Y);
    Frame := NodeToFrame(Node);
    if Frame = nil then
    begin
      Exit;
    end;
    case Frame.SelectionType of
      stCheckBox:
        begin
          Frame.Selected := not Frame.Selected and Frame.CanSelect;
          if ((Frame = framePkgMnw1) or (Frame = framePkgMnw2))
            and framePkgMnw1.Selected and framePkgMnw2.Selected then
          begin
            Beep;
            MessageDlg(StrMODFLOWDoesNotAll, mtWarning, [mbOK], 0);
          end;
        end;
      stRadioButton:
        begin
          if not Frame.Selected and Frame.CanSelect then
          begin
            ParentNode := Node.Parent;
            ChildNode := ParentNode.getFirstChild;
            while ChildNode <> nil do
            begin
              if ChildNode <> Node then
              begin
                SelectedFrame := NodeToFrame(ChildNode);
                if (SelectedFrame <> nil)
                  and (SelectedFrame.SelectionType = stRadioButton)
                  and SelectedFrame.Selected then
                begin
                  SelectedFrame.Selected := False;
                end;
              end;
              ChildNode := ChildNode.getNextSibling;
            end;
            Frame.Selected := True;
          end;
        end;
      else Assert(False);
    end;
  end;
end;

procedure TfrmModflowPackages.InitializeFrame(
  Frame: TframeListParameterDefinition);
begin
  Frame.seNumberOfParameters.AsInteger := 0;
  frameParameterDefinition_seNumberOfParametersChange(
    Frame.seNumberOfParameters);
  Frame.dgParameters.Objects[0,1] := nil;
end;

procedure TfrmModflowPackages.FillHfbGrid;
var
  ParamIndex: Integer;
  Param: TModflowSteadyParameter;
  ActiveFrame: TframeListParameterDefinition;
  ActiveGrid: TRbwDataGrid4;
begin
  InitializeFrame(frameHfbParameterDefinition);
  for ParamIndex := 0 to FSteadyParameters.Count - 1 do
  begin
    ActiveFrame := nil;
    Param := FSteadyParameters[ParamIndex];
    case Param.ParameterType of
      ptLPF_HK, ptLPF_HANI, ptLPF_VK,
        ptLPF_VANI, ptLPF_SS, ptLPF_SY, ptLPF_VKCB, ptHUF_SYTP, ptHUF_LVDA,
        ptSTR, ptQMAX, ptPEST: ;  // do nothing
      ptHFB: ActiveFrame := frameHFBParameterDefinition;
      else Assert(False);
    end;
    if ActiveFrame <> nil then
    begin
      ActiveGrid := ActiveFrame.dgParameters;
      ActiveFrame.seNumberOfParameters.AsInteger :=
        ActiveFrame.seNumberOfParameters.AsInteger + 1;
      if Assigned(ActiveFrame.seNumberOfParameters.OnChange) then
      begin
        ActiveFrame.seNumberOfParameters.OnChange(ActiveFrame.seNumberOfParameters);
      end;
      AssignParameterToRow(ActiveGrid, ActiveGrid.RowCount -1, Param);
    end;
  end;
end;

procedure TfrmModflowPackages.FillTransientGrids;
var
  ParamIndex: Integer;
  Param: TModflowTransientListParameter;
  ActiveFrame: TframeListParameterDefinition;
  ActiveGrid: TRbwDataGrid4;
begin
  InitializeFrame(frameChdParameterDefinition);
  InitializeFrame(frameGhbParameterDefinition);
  InitializeFrame(frameWelParameterDefinition);
  InitializeFrame(frameRivParameterDefinition);
  InitializeFrame(frameDrnParameterDefinition);
  InitializeFrame(frameDrtParameterDefinition);
  InitializeFrame(frameRchParameterDefinition);
  InitializeFrame(frameEvtParameterDefinition);
  InitializeFrame(frameEtsParameterDefinition);
  InitializeFrame(frameSfrParameterDefinition);
  InitializeFrame(frameStrParameterDefinition);
  InitializeFrame(frameFmpParameterDefinition);

  for ParamIndex := 0 to FTransientListParameters.Count - 1 do
  begin
    ActiveFrame := nil;
    Param := FTransientListParameters[ParamIndex];
    case Param.ParameterType of
      ptCHD: ActiveFrame := frameChdParameterDefinition;
      ptGHB: ActiveFrame := frameGhbParameterDefinition;
      ptQ: ActiveFrame := frameWelParameterDefinition;
      ptRIV: ActiveFrame := frameRivParameterDefinition;
      ptDRN: ActiveFrame := frameDrnParameterDefinition;
      ptDRT: ActiveFrame := frameDrtParameterDefinition;
      ptRCH: ActiveFrame := frameRchParameterDefinition;
      ptEVT: ActiveFrame := frameEvtParameterDefinition;
      ptETS: ActiveFrame := frameEtsParameterDefinition;
      ptSFR: ActiveFrame := frameSfrParameterDefinition;
      ptSTR: ActiveFrame := frameStrParameterDefinition;
      ptQMAX: ActiveFrame := frameFmpParameterDefinition;
      else Assert(False);
    end;
    ActiveGrid := ActiveFrame.dgParameters;
    ActiveFrame.seNumberOfParameters.AsInteger :=
      ActiveFrame.seNumberOfParameters.AsInteger + 1;
    if Assigned(ActiveFrame.seNumberOfParameters.OnChange) then
    begin
      ActiveFrame.seNumberOfParameters.OnChange(ActiveFrame.seNumberOfParameters);
    end;
    AssignParameterToRow(ActiveGrid, ActiveGrid.RowCount -1, Param);
  end;
end;

procedure TfrmModflowPackages.Fmp4SelectedChange(Sender: TObject);
begin
  if not framePkgFMP4.Selected then
  begin
    framePkgFmp4Soils.Selected := False;
    framePkgFmp4Soils.CanSelect := False;

    framePkgFmp4Climate.Selected := False;
    framePkgFmp4Climate.CanSelect := False;

    framePkgFmp4SurfaceWater.Selected := False;
    framePkgFmp4SurfaceWater.CanSelect := False;

    framePkgFmp4Wells.Selected := False;
    framePkgFmp4Wells.CanSelect := False;

    framePkgFmp4Allotments.Selected := False;
    framePkgFmp4Allotments.CanSelect := False;

    framePkgFmp4LandUse.Selected := False;
    framePkgFmp4LandUse.CanSelect := False;

    framePkgFmp4SalinityFlush.Selected := False;
    framePkgFmp4SalinityFlush.CanSelect := False;
  end
  else
  begin
    framePkgFmp4Soils.CanSelect := True;
    framePkgFmp4Climate.CanSelect := True;
    framePkgFmp4SurfaceWater.CanSelect := True;
    framePkgFmp4Wells.CanSelect := True;
    framePkgFmp4Allotments.CanSelect := True;
    framePkgFmp4LandUse.CanSelect := True;
    framePkgFmp4SalinityFlush.CanSelect := True;
  end;

end;

procedure TfrmModflowPackages.AddPackagesToList(Packages: TModflowPackages);
var
  MstIndex: Integer;
  AFrame: TframePackageMST;
  MstPackage: TGwtMstPackage;
  MstChildNode: TTreeNode;
  IstChildNode: TTreeNode;
  IstIndex: Integer;
  IstPackage: TGwtIstPackage;
  AnIstFrame: TframePackageIst;
  SpeciesName: string;
  ImsChildNode: TTreeNode;
  ImsIndex: Integer;
  ImsPackage: TSmsPackageSelection;
  AnImsFrame: TframePkgSms;
begin
  FPackageList.Clear;

  // add to list in in the order in which they should appear within
  // their group.

  if frmGoPhast.ModelSelection <> msModflow2015 then
  begin
    Packages.BcfPackage.Frame := framePkgBCF;
    FPackageList.Add(Packages.BcfPackage);
  end
  else
  begin
    framePkgBCF.NilNode;
  end;

  Packages.ChdBoundary.Frame := framePkgCHD;
  FPackageList.Add(Packages.ChdBoundary);

  Packages.DrnPackage.Frame := framePkgDRN;
  FPackageList.Add(Packages.DrnPackage);

  if frmGoPhast.ModelSelection <> msModflow2015 then
  begin
    Packages.DrtPackage.Frame := framePkgDRT;
    FPackageList.Add(Packages.DrtPackage);
  end
  else
  begin
    framePkgDRT.NilNode;
  end;

  Packages.ETSPackage.Frame := framePkgETS;
  FPackageList.Add(Packages.ETSPackage);

  if frmGoPhast.ModelSelection <> msModflow2015 then
  begin
    Packages.EVTPackage.Frame := framePkgEVT;
    FPackageList.Add(Packages.EVTPackage);
  end
  else
  begin
    framePkgEVT.NilNode;
  end;

  Packages.GhbBoundary.Frame := framePkgGHB;
  FPackageList.Add(Packages.GhbBoundary);

  if frmGoPhast.ModelSelection <> msModflow2015 then
  begin
    Packages.LakPackage.Frame := framePkgLAK;
    FPackageList.Add(Packages.LakPackage);
  end
  else
  begin
    framePkgLAK.NilNode;
  end;

  if frmGoPhast.ModelSelection = msModflow2015 then
  begin
    Packages.LakMf6Package.Frame := framePackageLakMf6;
    FPackageList.Add(Packages.LakMf6Package);
  end
  else
  begin
    framePackageLakMf6.NilNode;
  end;

  if frmGoPhast.ModelSelection <> msModflow2015 then
  begin
    Packages.LpfPackage.Frame := framePkgLPF;
    FPackageList.Add(Packages.LpfPackage);

    Packages.HufPackage.Frame := framePkgHuf;
    FPackageList.Add(Packages.HufPackage);
  end
  else
  begin
    framePkgLPF.NilNode;
    framePkgHuf.NilNode;
  end;

  if frmGoPhast.ModelSelection in [msModflowNWT, msModflowFmp
          {$IFDEF OWHMV2}
          , msModflowOwhm2
          {$ENDIF}
  ] then
  begin
    Packages.UpwPackage.Frame := framePkgUPW;
    FPackageList.Add(Packages.UpwPackage);
  end
  else
  begin
    framePkgUPW.NilNode;
  end;

  if frmGoPhast.ModelSelection = msModflow2015 then
  begin
    Packages.NpfPackage.Frame := framePkgNpf;
    FPackageList.Add(Packages.NpfPackage);

    Packages.StoPackage.Frame := framePkgSto;
    FPackageList.Add(Packages.StoPackage);
  end
  else
  begin
    framePkgNpf.NilNode;
    framePkgSto.NilNode;
  end;

  if frmGoPhast.DisvUsed then
  begin
    Packages.GncPackage.Frame := framePkgGnc;
    FPackageList.Add(Packages.GncPackage);
  end
  else
  begin
    framePkgGnc.NilNode;
  end;

  if frmGoPhast.ModelSelection <> msModflow2015 then
  begin
    Packages.PcgPackage.Frame := framePCG;
    FPackageList.Add(Packages.PcgPackage);

    Packages.PcgnPackage.Frame := framePackagePcgn;
    FPackageList.Add(Packages.PcgnPackage);

    Packages.GmgPackage.Frame := framePkgGMG;
    FPackageList.Add(Packages.GmgPackage);

    Packages.SipPackage.Frame := framePkgSIP;
    FPackageList.Add(Packages.SipPackage);

    Packages.De4Package.Frame := framePkgDE4;
    FPackageList.Add(Packages.De4Package);
  end
  else
  begin
    framePCG.NilNode;
    framePackagePcgn.NilNode;
    framePkgGMG.NilNode;
    framePkgSIP.NilNode;
    framePkgDE4.NilNode;
  end;

  if frmGoPhast.ModelSelection in [msModflowNWT, msModflowFmp
          {$IFDEF OWHMV2}
          , msModflowOwhm2
          {$ENDIF}
  ] then
  begin
    Packages.NwtPackage.Frame := framePkgNwt;
    FPackageList.Add(Packages.NwtPackage);
  end
  else
  begin
    framePkgNwt.NilNode;
  end;

  if frmGoPhast.ModelSelection = msModflow2015 then
  begin
    Packages.SmsPackage.Frame := framePkgIMS;
    FPackageList.Add(Packages.SmsPackage);

    Packages.MawPackage.Frame := framePkgMAW;
    FPackageList.Add(Packages.MawPackage);

    Packages.MvrPackage.Frame := framePkgMVR;
    FPackageList.Add(Packages.MvrPackage);
  end
  else
  begin
    framePkgIMS.NilNode;
    framePkgMAW.NilNode;
    framePkgMVR.NilNode;
  end;

  if frmGoPhast.ModelSelection <> msModflow2015 then
  begin
    Packages.Mnw1Package.Frame := framePkgMnw1;
    FPackageList.Add(Packages.Mnw1Package);

    Packages.Mnw2Package.Frame := framePkgMnw2;
    FPackageList.Add(Packages.Mnw2Package);
  end
  else
  begin
    framePkgMnw1.NilNode;
    framePkgMnw2.NilNode;
  end;

  Packages.RCHPackage.Frame := framePkgRCH;
  FPackageList.Add(Packages.RCHPackage);

  if frmGoPhast.ModelSelection <> msModflow2015 then
  begin
    Packages.ResPackage.Frame := framePkgRES;
    FPackageList.Add(Packages.ResPackage);
  end
  else
  begin
    framePkgRES.NilNode;
  end;

  if frmGoPhast.ModelSelection in [msModflowFMP
          {$IFDEF OWHMV2}
          , msModflowOwhm2
          {$ENDIF}
  ] then
  begin
    Packages.RipPackage.Frame := framePkgRIP;
    FPackageList.Add(Packages.RipPackage);
  end
  else
  begin
    framePkgRIP.NilNode;
  end;

  Packages.RivPackage.Frame := framePkgRIV;
  FPackageList.Add(Packages.RivPackage);

  if frmGoPhast.ModelSelection = msModflow2015 then
  begin
    Packages.SfrModflow6Package.Frame := framePackageSfrMF6;
    FPackageList.Add(Packages.SfrModflow6Package);
  end
  else
  begin
    framePackageSfrMF6.NilNode;
  end;

  if frmGoPhast.ModelSelection = msModflow2015 then
  begin
    Packages.UzfMf6Package.Frame := framePackageUzfMf6;
    FPackageList.Add(Packages.UzfMf6Package);
  end
  else
  begin
    framePackageUzfMf6.NilNode;
  end;

  Packages.WelPackage.Frame := framePkgWEL;
  FPackageList.Add(Packages.WelPackage);

  if frmGoPhast.ModelSelection <> msModflow2015 then
  begin
    Packages.SfrPackage.Frame := framePkgSFR;
    FPackageList.Add(Packages.SfrPackage);

    Packages.StrPackage.Frame := framePkgSTR;
    FPackageList.Add(Packages.StrPackage);
  end
  else
  begin
    framePkgSTR.NilNode;
    framePkgSFR.NilNode;
  end;

  Packages.HfbPackage.Frame := framePkgHFB;
  FPackageList.Add(Packages.HfbPackage);

  if frmGoPhast.ModelSelection <> msModflow2015 then
  begin
    Packages.UzfPackage.Frame := framePkgUZF;
    FPackageList.Add(Packages.UzfPackage);
  end
  else
  begin
    framePkgUZF.NilNode;
  end;

  if frmGoPhast.ModelSelection <> msModflow2015 then
  begin
    Packages.SubPackage.Frame := framePkgSub;
    FPackageList.Add(Packages.SubPackage);

    Packages.SwtPackage.Frame := framePkgSwt;
    FPackageList.Add(Packages.SwtPackage);
  end
  else
  begin
    framePkgSub.NilNode;
    framePkgSwt.NilNode;
  end;

  if frmGoPhast.ModelSelection = msModflow2015 then
  begin
    Packages.Mf6ObservationUtility.Frame := framePackageMf6Obs;
    FPackageList.Add(Packages.Mf6ObservationUtility);
  end
  else
  begin
    framePackageMf6Obs.NilNode;
  end;

  Packages.ModPath.Frame := frameModpath;
  FPackageList.Add(Packages.ModPath);

  if frmGoPhast.ModelSelection <> msModflow2015 then
  begin
    Packages.HobPackage.Frame := framePkgHOB;
    FPackageList.Add(Packages.HobPackage);

    Packages.ChobPackage.Frame := framePkgCHOB;
    FPackageList.Add(Packages.ChobPackage);

    Packages.DrobPackage.Frame := framePkgDROB;
    FPackageList.Add(Packages.DrobPackage);

    Packages.GbobPackage.Frame := framePkgGBOB;
    FPackageList.Add(Packages.GbobPackage);

    Packages.RvobPackage.Frame := framePkgRVOB;
    FPackageList.Add(Packages.RvobPackage);

    Packages.StobPackage.Frame := framePkgSTOB;
    FPackageList.Add(Packages.StobPackage);
  end
  else
  begin
    framePkgCHOB.NilNode;
    framePkgDROB.NilNode;
    framePkgGBOB.NilNode;
    framePkgRVOB.NilNode;
    framePkgSTOB.NilNode;
    framePkgHOB.NilNode;
  end;

  Packages.ZoneBudget.Frame := frameZoneBudget;
  FPackageList.Add(Packages.ZoneBudget);

  if frmGoPhast.ModelSelection <> msModflow2015 then
  begin
    Packages.HydmodPackage.Frame := framePkgHydmod;
    FPackageList.Add(Packages.HydmodPackage);
  end
  else
  begin
    framePkgHydmod.NilNode;
  end;

  Packages.Mt3dBasic.Frame := framePkgMt3dBasic;
  FPackageList.Add(Packages.Mt3dBasic);

  Packages.Mt3dmsAdvection.Frame := frameMt3dmsAdvPkg;
  FPackageList.Add(Packages.Mt3dmsAdvection);

  Packages.Mt3dmsDispersion.Frame := frameMt3dmsDispersionPkg;
  FPackageList.Add(Packages.Mt3dmsDispersion);

  Packages.Mt3dmsSourceSink.Frame := framePkgSSM;
  FPackageList.Add(Packages.Mt3dmsSourceSink);

  Packages.Mt3dmsChemReact.Frame := framePkgMt3dmsRct;
  FPackageList.Add(Packages.Mt3dmsChemReact);

  Packages.Mt3dmsGCGSolver.Frame := frameMt3dmsGcgPackage;
  FPackageList.Add(Packages.Mt3dmsGCGSolver);

  Packages.Mt3dmsTransObs.Frame := framePkgMt3dmsTob;
  FPackageList.Add(Packages.Mt3dmsTransObs);

  Packages.Mt3dCts.Frame := frameMt3dCtsPkg;
  FPackageList.Add(Packages.Mt3dCts);

  Packages.Mt3dLkt.Frame := frameMt3dLktPkg;
  FPackageList.Add(Packages.Mt3dLkt);

  Packages.Mt3dSft.Frame := frameMt3dSftPkg;
  FPackageList.Add(Packages.Mt3dSft);

  Packages.Mt3dUnsatTransport.Frame := framePkgMt3dUzt;
  FPackageList.Add(Packages.Mt3dUnsatTransport);

  if frmGoPhast.ModelSelection <> msModflow2015 then
  begin
    Packages.FhbPackage.Frame := framePkgFHB;
    FPackageList.Add(Packages.FhbPackage);
  end
  else
  begin
    framePkgFHB.NilNode;
  end;

  if frmGoPhast.ModelSelection = msModflowFMP then
  begin
    Packages.FarmProcess.Frame := framePkgFrm;
    FPackageList.Add(Packages.FarmProcess);
  end
  else
  begin
    framePkgFrm.NilNode;
  end;

          {$IFDEF OWHMV2}
  if frmGoPhast.ModelSelection = msModflowOwhm2 then
  begin
    Packages.FarmProcess4.Frame := framePkgFMP4;
    FPackageList.Add(Packages.FarmProcess4);

    Packages.FarmSoil4.Frame := framePkgFmp4Soils;
    FPackageList.Add(Packages.FarmSoil4);

    Packages.FarmClimate4.Frame := framePkgFmp4Climate;
    FPackageList.Add(Packages.FarmClimate4);

    Packages.FarmSurfaceWater4.Frame := framePkgFmp4SurfaceWater;
    FPackageList.Add(Packages.FarmSurfaceWater4);

    Packages.FarmWells4.Frame := framePkgFmp4Wells;
    FPackageList.Add(Packages.FarmWells4);

    Packages.FarmAllotments.Frame := framePkgFmp4Allotments;
    FPackageList.Add(Packages.FarmAllotments);

    Packages.FarmLandUse.Frame := framePkgFmp4LandUse;
    FPackageList.Add(Packages.FarmLandUse);

    Packages.FarmSalinityFlush.Frame := framePkgFmp4SalinityFlush;
    FPackageList.Add(Packages.FarmSalinityFlush);
  end
  else
  begin
    framePkgFMP4.NilNode;
    framePkgFmp4Soils.NilNode;
    framePkgFmp4Climate.NilNode;
    framePkgFmp4SurfaceWater.NilNode;
    framePkgFmp4Wells.NilNode;
    framePkgFmp4Allotments.NilNode;
    framePkgFmp4LandUse.NilNode;
    framePkgFmp4SalinityFlush.NilNode;
  end;
          {$ENDIF}

  if frmGoPhast.ModelSelection = msModflowCFP then
  begin
    Packages.ConduitFlowProcess.Frame := framePkgCFP;
    FPackageList.Add(Packages.ConduitFlowProcess);
  end
  else
  begin
    framePkgCFP.NilNode;
  end;

  if frmGoPhast.ModelSelection in [msModflow, msModflowNWT, msModflowFmp
          {$IFDEF OWHMV2}
          , msModflowOwhm2
          {$ENDIF}
  ] then
  begin
    Packages.SwiPackage.Frame := framePackageSWI;
    FPackageList.Add(Packages.SwiPackage);
  end
  else
  begin
    framePackageSWI.NilNode;
  end;

  if frmGoPhast.ModelSelection in [msModflow, msModflowNWT, msModflowFMP
          {$IFDEF OWHMV2}
          , msModflowOwhm2
          {$ENDIF}
  ] then
  begin
    Packages.SwrPackage.Frame := framePkgSWR;
    FPackageList.Add(Packages.SwrPackage);
  end
  else
  begin
    framePkgSWR.NilNode;
  end;

  if frmGoPhast.ModelSelection = msModflow2015 then
  begin
    Packages.CsubPackage.Frame := framePackageCsub;
    FPackageList.Add(Packages.CsubPackage);
  end
  else
  begin
    framePackageCsub.NilNode;
  end;

  if frmGoPhast.ModelSelection = msModflow2015 then
  begin
    Packages.GwtProcess.Frame := frameGwtProcess;
    FPackageList.Add(Packages.GwtProcess);
  end
  else
  begin
    frameGwtProcess.NilNode;
  end;

  if frmGoPhast.ModelSelection = msModflow2015 then
  begin
    Packages.GwtAdvectionPackage.Frame := frameGwtAdv;
    FPackageList.Add(Packages.GwtAdvectionPackage);
  end
  else
  begin
    frameGwtAdv.NilNode;
  end;

  if frmGoPhast.ModelSelection = msModflow2015 then
  begin
    Packages.GwtDispersionPackage.Frame := frameGwtDsp;
    FPackageList.Add(Packages.GwtDispersionPackage);
  end
  else
  begin
    frameGwtDsp.NilNode;
  end;

  if frmGoPhast.ModelSelection = msModflow2015 then
  begin
    Packages.GwtSsmPackage.Frame := frameGwtSSM;
    FPackageList.Add(Packages.GwtSsmPackage);
  end
  else
  begin
    frameGwtSSM.NilNode;
  end;

  if frmGoPhast.ModelSelection = msModflow2015 then
  begin
    Packages.GwtCncPackage.Frame := frameGwtCnc;
    FPackageList.Add(Packages.GwtCncPackage);
  end
  else
  begin
    frameGwtCnc.NilNode;
  end;

  if frmGoPhast.ModelSelection = msModflow2015 then
  begin
    Packages.GwtSrcPackage.Frame := frameGwtSrc;
    FPackageList.Add(Packages.GwtSrcPackage);
  end
  else
  begin
    frameGwtSrc.NilNode;
  end;

  if (frmGoPhast.ModelSelection = msModflow2015)
    and frameGwtProcess.Selected
    {and (framePkgMt3dBasic.comboVersion.ItemIndex = 2)} then
  begin
    if Packages.GwtPackages.Count < frameChemSpecies.frameGridMobile.seNumber.AsInteger then
    begin
      Packages.GwtPackages.Count := frameChemSpecies.frameGridMobile.seNumber.AsInteger
    end;
    // MST Package
    MstChildNode := FMstNode.GetFirstChild;
    for MstIndex := 0 to frameChemSpecies.frameGridMobile.seNumber.AsInteger - 1 do
    begin
      MstPackage := Packages.GwtPackages[MstIndex].GwtMst;
      if MstIndex < FframePackageMSTObjectList.Count then
      begin
        AFrame := FframePackageMSTObjectList[MstIndex];
      end
      else
      begin
        AFrame := CreateMstFrame;
      end;

      SpeciesName := frameChemSpecies.frameGridMobile.Grid.Cells[0, MstIndex+ 1];

      if MstChildNode = nil then
      begin
        MstChildNode := tvPackages.Items.AddChild(FMstNode, SpeciesName);
      end;

      MstPackage.Frame := AFrame;
      MstPackage.Node := MstChildNode;

      FPackageList.Add(MstPackage);

      MstChildNode := MstChildNode.GetNextSibling;
    end;

//    for MstIndex := frmGoPhast.PhastModel.MobileComponents.Count to
//      FframePackageMSTObjectList.Count - 1 do
//    begin
//      FframePackageMSTObjectList[MstIndex].NilNode;
//    end;

    // IST Package
    IstChildNode := FIstNode.GetFirstChild;
    for IstIndex := 0 to frameChemSpecies.frameGridMobile.seNumber.AsInteger - 1 do
    begin
      IstPackage := Packages.GwtPackages[IstIndex].GwtIst;
      if IstIndex < FframePackageIstObjectList.Count then
      begin
        AnIstFrame := FframePackageIstObjectList[IstIndex];
      end
      else
      begin
        AnIstFrame := CreateIstFrame;
      end;

      SpeciesName := frameChemSpecies.frameGridMobile.Grid.Cells[0, IstIndex+ 1];

      if IstChildNode = nil then
      begin
        IstChildNode := tvPackages.Items.AddChild(FIstNode, SpeciesName);
      end;

      IstPackage.Frame := AnIstFrame;
      IstPackage.Node := IstChildNode;

      FPackageList.Add(IstPackage);

      IstChildNode := IstChildNode.GetNextSibling;
    end;

//    for IstIndex := frmGoPhast.PhastModel.MobileComponents.Count to
//      FframePackageIstObjectList.Count - 1 do
//    begin
//      FframePackageIstObjectList[IstIndex].NilNode;
//    end;

    // IMS Package
    ImsChildNode := FGwtImsNode.GetFirstChild;
    for ImsIndex := 0 to frameChemSpecies.frameGridMobile.seNumber.AsInteger - 1 do
    begin
      ImsPackage := Packages.GwtPackages[ImsIndex].GwtIms;
      if ImsIndex < FframePkgSmsObjectList.Count then
      begin
        AnImsFrame := FframePkgSmsObjectList[ImsIndex];
      end
      else
      begin
        AnImsFrame := CreateImsGwtFrame;
      end;

      SpeciesName := frameChemSpecies.frameGridMobile.Grid.Cells[0, ImsIndex+ 1];

      if ImsChildNode = nil then
      begin
        ImsChildNode := tvPackages.Items.AddChild(FGwtImsNode, SpeciesName);
      end;

      ImsPackage.Frame := AnImsFrame;
      ImsPackage.Node := ImsChildNode;

      FPackageList.Add(ImsPackage);

      ImsChildNode := ImsChildNode.GetNextSibling;
    end;
  end
  else
  begin
    for MstIndex := 0 to FframePackageMSTObjectList.Count - 1 do
    begin
      FframePackageMSTObjectList[MstIndex].NilNode;
    end;
    for IstIndex := 0 to FframePackageIstObjectList.Count - 1 do
    begin
      FframePackageIstObjectList[IstIndex].NilNode;
    end;
    for IstIndex := 0 to FframePkgSmsObjectList.Count - 1 do
    begin
      FframePkgSmsObjectList[IstIndex].NilNode;
    end;
  end;

end;

procedure TfrmModflowPackages.OwhmFrameButtonClick(
    Sender: TObject; ACol, ARow: Integer);
var
  Grid: TRbwDataGrid4;
begin
  inherited;
  Grid := Sender as TRbwDataGrid4;
  dlgOpenSelectExternalFile.FileName := Grid.Cells[ACol, ARow];
  if dlgOpenSelectExternalFile.Execute then
  begin
    Grid.Cells[ACol, ARow] := dlgOpenSelectExternalFile.FileName;
  end;
end;

procedure TfrmModflowPackages.tvHufParameterTypesChange(Sender: TObject;
  Node: TTreeNode);
begin
  inherited;
  CurrentParameterType := TParameterType(Node.Data);
  if CurrentParameterType in [ptHUF_SYTP, ptHUF_LVDA] then
  begin
    UpdateFlowParamGrid(Node, frameHufParameterDefinition,
      FSteadyParameters, rbwHufParamCountController);
  end
  else
  begin
    UpdateFlowParamGrid(Node, frameHufParameterDefinition,
      FHufParameters, rbwHufParamCountController);
  end;
end;

procedure TfrmModflowPackages.tvLpfParameterTypesChange(Sender: TObject;
  Node: TTreeNode);
begin
  inherited;
  UpdateFlowParamGrid(Node, frameLpfParameterDefinition,
    FSteadyParameters, rbwLpfParamCountController);
end;

{ TTempPackageItem }

constructor TTempPackageItem.Create(Collection: TCollection);
begin
  inherited;
  FPackages := TModflowPackages.Create(nil);
end;

destructor TTempPackageItem.Destroy;
begin
  FPackages.Free;
  inherited;
end;

{ TTempPackageCollection }

function TTempPackageCollection.Add: TTempPackageItem;
begin
  result := inherited Add as TTempPackageItem
end;

constructor TTempPackageCollection.Create;
begin
  inherited Create(TTempPackageItem);
end;

function TTempPackageCollection.GetItem(Index: integer): TTempPackageItem;
begin
  result := inherited Items[Index] as TTempPackageItem
end;

procedure TTempPackageCollection.SetItem(Index: integer;
  const Value: TTempPackageItem);
begin
  inherited Items[Index] := Value;
end;

{ TUndoChangeLgrPackageSelection }

constructor TUndoChangeLgrPackageSelection.Create(
  var NewSteadyParameters: TModflowSteadyParameters;
  var NewTransientParameters: TModflowTransientListParameters;
  var SfrParameterInstances: TSfrParamInstances;
  var NewHufModflowParameters: THufModflowParameters;
  var NewParamGroups: TPestParamGroups;
  var NewPackages: TTempPackageCollection);
var
  Index: Integer;
  LayerGroup: TLayerGroup;
  TempParentPackages: TTempPackageItem;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  inherited Create(NewSteadyParameters, NewTransientParameters,
    NewHufModflowParameters, SfrParameterInstances, NewParamGroups);

  FOldMt3dTimes := TMt3dmsTimeCollection.Create(nil);
  FOldMt3dTimes.Assign(frmGoPhast.PhastModel.Mt3dmsTimes);

  // take ownership of NewPackages.
  FNewPackages := NewPackages;
  NewPackages := nil;

  SetLength(FOldInterBlockTransmissivity, frmGoPhast.PhastModel.LayerStructure.Count);
  SetLength(FOldAquiferType, frmGoPhast.PhastModel.LayerStructure.Count);
  for Index := 0 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
  begin
    LayerGroup := frmGoPhast.PhastModel.LayerStructure[Index];
    FOldInterBlockTransmissivity[Index] := LayerGroup.InterblockTransmissivityMethod;
    FOldAquiferType[Index] := LayerGroup.AquiferType;
  end;

  FOldHydroGeologicUnits := THydrogeologicUnits.Create(nil);
  FOldHydroGeologicUnits.Assign(frmGoPhast.PhastModel.HydrogeologicUnits);

  FOldPackages := TTempPackageCollection.Create;

  TempParentPackages := FOldPackages.Add;
  TempParentPackages.Packages.Assign(frmGoPhast.PhastModel.ModflowPackages);
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        TempParentPackages := FOldPackages.Add;
        TempParentPackages.Packages.Assign(ChildModel.ModflowPackages);
      end;
    end;
  end;

  FOldMobileComponents := TMobileChemSpeciesCollection.Create(nil);
  FOldMobileComponents.Assign(frmGoPhast.PhastModel.MobileComponents);
  FNewMobileComponents := TMobileChemSpeciesCollection.Create(nil);

  FOldImmobileComponents := TChemSpeciesCollection.Create(nil);
  FOldImmobileComponents.Assign(frmGoPhast.PhastModel.ImmobileComponents);
  FNewImmobileComponents := TChemSpeciesCollection.Create(nil);
end;

function TUndoChangeLgrPackageSelection.Description: string;
begin
  result := rsChangePackages;
end;

destructor TUndoChangeLgrPackageSelection.Destroy;
begin
  FOldHydroGeologicUnits.Free;
  FOldPackages.Free;
  FNewPackages.Free;
  FOldMobileComponents.Free;
  FNewMobileComponents.Free;
  FOldImmobileComponents.Free;
  FNewImmobileComponents.Free;
  FOldMt3dTimes.Free;
  inherited;
end;

procedure TUndoChangeLgrPackageSelection.DoCommand;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
  TimeItem: TMt3dmsTimeItem;
  PhastModel: TPhastModel;
  Mt3dmsNewlySelected: boolean;
  OldPackages: TModflowPackages;
  NewPackages: TModflowPackages;
  MfStressPeriods: TModflowStressPeriods;
  FirstStressPeriod: TModflowStressPeriod;
  GwtChanged: Boolean;
begin
  if (frmDisplayData <> nil) then
  begin
    frmDisplayData.BeginUpdate;
  end;
  try
  inherited;
  PhastModel := frmGoPhast.PhastModel;
  PhastModel.DataArrayManager.InvalidateHguFormulaDataSets;

  OldPackages := PhastModel.ModflowPackages;
  PhastModel.ModflowPackages.SfrPackage.AssignParameterInstances := False;
  try
    UpdateLayerGroupProperties(FNewPackages[0].Packages.BcfPackage);
    NewPackages := FNewPackages[0].Packages;

    GwtChanged := (PhastModel.ModelSelection = msModflow2015)
      and (PhastModel.GwtUsed <> NewPackages.GwtProcess.IsSelected);
//      and (NewPackages.Mt3dBasic.Mt3dVersion = mvMf6Gwt)));

    Mt3dmsNewlySelected := not OldPackages.Mt3dBasic.SimulateWithMt3D
      and NewPackages.Mt3dBasic.SimulateWithMt3D;
    PhastModel.ModflowPackages := NewPackages;
    if PhastModel.LgrUsed then
    begin
      Assert(PhastModel.ChildModels.Count = FNewPackages.Count -1);
      for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
      begin
        ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
        if ChildModel <> nil then
        begin
          OldPackages := ChildModel.ModflowPackages;
          NewPackages := FNewPackages[ChildIndex+1].Packages;
          if not OldPackages.Mt3dBasic.SimulateWithMt3D
            and NewPackages.Mt3dBasic.SimulateWithMt3D then
          begin
            Mt3dmsNewlySelected := True;
          end;
          ChildModel.ModflowPackages := FNewPackages[ChildIndex+1].Packages;
        end;
      end;
    end;
  finally
    PhastModel.ModflowPackages.SfrPackage.AssignParameterInstances := True;
  end;
  UpdateInterbedsInObjects;

  FComponentsSame := PhastModel.MobileComponents.IsSame(FNewMobileComponents)
    and PhastModel.ImmobileComponents.IsSame(FNewImmobileComponents);
  PhastModel.MobileComponents := FNewMobileComponents;
  PhastModel.ImmobileComponents := FNewImmobileComponents;

  if PhastModel.Mt3dmsIsSelected and
    (PhastModel.Mt3dmsTimes.Count = 0) then
  begin
    TimeItem := PhastModel.Mt3dmsTimes.Add;
    MfStressPeriods := PhastModel.ModflowStressPeriods;
    FirstStressPeriod := MfStressPeriods[0];
    TimeItem.StartTime := FirstStressPeriod.StartTime;
    TimeItem.StepSize := 0;
    TimeItem.TimeStepMultiplier := FirstStressPeriod.TimeStepMultiplier;
    TimeItem.MaxStepSize := 0;
    try
      TimeItem.MaxSteps := MfStressPeriods.NumberOfSteps * 1000;
    except on EIntOverflow do
      begin
        TimeItem.MaxSteps := MAXINT;
      end;
    end;
    TimeItem.EndTime := MfStressPeriods[MfStressPeriods.Count-1].EndTime;
  end;

  RecreateMt3dTimeLists;

  if not FComponentsSame then
  begin
    PhastModel.ModflowPackages.Mt3dSft.ChangeChemSpecies;
  end;

  if GwtChanged or not FComponentsSame then
  begin
    PhastModel.UpdateGwtConc;
  end;

  PhastModel.MobileComponents.UpdateAllDataArrays;

  inherited;

  InvalidateActiveGrid;

  frmGoPhast.EnableLinkStreams;
  frmGoPhast.EnableManageFlowObservations;
  frmGoPhast.EnableManageHeadObservations;
  frmGoPhast.EnableHufMenuItems;
  frmGoPhast.EnableMt3dmsMenuItems;
  frmGoPhast.EnableFarmMenuItems;
  frmGoPhast.EnableSwrActions;
  frmGoPhast.EnableCTS;
  SetMt3dCaption;

  if Mt3dmsNewlySelected then
  begin
    Beep;
    MessageDlg(StrInOrderToGenerate, mtInformation, [mbOK], 0);
  end;

  finally
    if (frmDisplayData <> nil) then
    begin
      frmDisplayData.EndUpdate;
    end;
  end;

end;

procedure TUndoChangeLgrPackageSelection.InvalidateActiveGrid;
var
  PackageIndex: Integer;
  OldPackages: TModflowPackages;
  NewPackages: TModflowPackages;
  ActiveChanged: Boolean;
  DrawingChoice: TGridLineDrawingChoice;
begin
  if frmGoPhast.PhastModel.ModflowGrid <> nil then
  begin
    DrawingChoice := frmGoPhast.PhastModel.ModflowGrid.GridLineDrawingChoice;
  end
  else if frmGoPhast.DisvUsed then
  begin
    DrawingChoice := frmGoPhast.PhastModel.DisvGrid.GridLineDrawingChoice;
  end
  else
  begin
    Exit;
  end;

  if DrawingChoice in [gldcActive, gldcActiveEdge] then
  begin
    ActiveChanged := False;
    for PackageIndex := 0 to Min(FOldPackages.Count, FNewPackages.Count) - 1 do
    begin
      OldPackages := FOldPackages[PackageIndex].Packages;
      NewPackages := FNewPackages[PackageIndex].Packages;
      if (OldPackages.LakPackage.IsSelected <> NewPackages.LakPackage.IsSelected)
        or (OldPackages.ChdBoundary.IsSelected <> NewPackages.ChdBoundary.IsSelected)
        or (OldPackages.LakMf6Package.IsSelected <> NewPackages.LakMf6Package.IsSelected) then
      begin
        ActiveChanged := True;
        break;
      end;
    end;
    if ActiveChanged then
    begin
      if frmGoPhast.PhastModel.ModflowGrid <> nil then
      begin
        frmGoPhast.PhastModel.ModflowGrid.GridChanged;
      end
      else if frmGoPhast.PhastModel.DisvUsed then
      begin
        frmGoPhast.PhastModel.DisvGrid.MeshChanged;
      end;
    end;
  end;
end;

procedure TUndoChangeLgrPackageSelection.RecreateMt3dTimeLists;
var
  LocalModel: TPhastModel;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ConcBoundary: TMt3dmsConcBoundary;
  Mt3dUzfRechConc: TMt3dUztRchConcBoundary;
  Mt3dUztSatEtConcBoundary: TMt3dUztSatEtConcBoundary;
  Mt3dUztUnsatEtConcBoundary: TMt3dUztUnsatEtConcBoundary;
  Mt3dUzSsmSinkConcBoundary: TMt3dUzSsmSinkConcBoundary;
  Mt3dSftConcBoundary: TMt3dSftBoundary;
begin
  LocalModel := frmGoPhast.PhastModel;
  if (LocalModel <> nil) and not (csDestroying in LocalModel.ComponentState)
    and not LocalModel.Clearing then
  begin
    for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
    begin
      ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];

      ConcBoundary := ScreenObject.Mt3dmsConcBoundary;
      if ConcBoundary <> nil then
      begin
        ConcBoundary.CreateTimeLists;
      end;

      Mt3dUzfRechConc := ScreenObject.Mt3dUzfRechConc;
      if Mt3dUzfRechConc <> nil then
      begin
        Mt3dUzfRechConc.CreateTimeLists;
      end;

      Mt3dUztSatEtConcBoundary := ScreenObject.Mt3dUztSatEtConcBoundary;
      if Mt3dUztSatEtConcBoundary <> nil then
      begin
        Mt3dUztSatEtConcBoundary.CreateTimeLists;
      end;

      Mt3dUztUnsatEtConcBoundary := ScreenObject.Mt3dUztUnsatEtConcBoundary;
      if Mt3dUztUnsatEtConcBoundary <> nil then
      begin
        Mt3dUztUnsatEtConcBoundary.CreateTimeLists;
      end;

      Mt3dUzSsmSinkConcBoundary := ScreenObject.Mt3dUzSsmSinkConcBoundary;
      if Mt3dUzSsmSinkConcBoundary <> nil then
      begin
        Mt3dUzSsmSinkConcBoundary.CreateTimeLists;
      end;

      Mt3dSftConcBoundary := ScreenObject.Mt3dSftConcBoundary;
      if Mt3dSftConcBoundary <> nil then
      begin
        Mt3dSftConcBoundary.CreateTimeLists;
      end;
    end;
  end;
end;

procedure TUndoChangeLgrPackageSelection.SetMt3dCaption;
begin
  frmGoPhast.SetMt3dCaption;
end;

procedure TUndoChangeLgrPackageSelection.Undo;
var
  Index: Integer;
  LayerGroup: TLayerGroup;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  PhastModel: TPhastModel;
  OldPackages: TModflowPackages;
  GwtChanged: Boolean;
begin
  if (frmDisplayData <> nil) then
  begin
    frmDisplayData.BeginUpdate;
  end;
  try
  frmGoPhast.PhastModel.Mt3dmsTimes := FOldMt3dTimes;
  PhastModel := frmGoPhast.PhastModel;
  PhastModel.DataArrayManager.InvalidateHguFormulaDataSets;

  frmGoPhast.PhastModel.ModflowPackages.SfrPackage.AssignParameterInstances := False;
  try
    for Index := 0 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
    begin
      LayerGroup := frmGoPhast.PhastModel.LayerStructure[Index];
      LayerGroup.InterblockTransmissivityMethod := FOldInterBlockTransmissivity[Index];
      LayerGroup.AquiferType := FOldAquiferType[Index];
    end;

    OldPackages := FOldPackages[0].Packages;
    GwtChanged := (PhastModel.ModelSelection = msModflow2015)
      and (PhastModel.GwtUsed <> OldPackages.GwtProcess.IsSelected);
//      and (OldPackages.Mt3dBasic.Mt3dVersion = mvMf6Gwt)));

    frmGoPhast.PhastModel.ModflowPackages := FOldPackages[0].Packages;
    if frmGoPhast.PhastModel.LgrUsed then
    begin
      Assert(frmGoPhast.PhastModel.ChildModels.Count = FOldPackages.Count -1);
      for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
      begin
        ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
        if ChildModel <> nil then
        begin
          ChildModel.ModflowPackages := FOldPackages[ChildIndex+1].Packages;
        end;
      end;
    end;
  finally
    frmGoPhast.PhastModel.ModflowPackages.SfrPackage.AssignParameterInstances := True;
  end;
  UpdateInterbedsInObjects;

  frmGoPhast.PhastModel.MobileComponents := FOldMobileComponents;
  frmGoPhast.PhastModel.ImmobileComponents := FOldImmobileComponents;
  RecreateMt3dTimeLists;

  if not FComponentsSame then
  begin
    frmGoPhast.PhastModel.ModflowPackages.Mt3dSft.ChangeChemSpecies;
  end;

  if GwtChanged or not FComponentsSame then
  begin
    frmGoPhast.PhastModel.UpdateGwtConc;
  end;

  PhastModel.MobileComponents.UpdateAllDataArrays;

  inherited;
  InvalidateActiveGrid;

  frmGoPhast.PhastModel.HydrogeologicUnits.Assign(FOldHydroGeologicUnits);
  frmGoPhast.EnableLinkStreams;
  frmGoPhast.EnableHufMenuItems;
  frmGoPhast.EnableMt3dmsMenuItems;
  frmGoPhast.EnableManageFlowObservations;
  frmGoPhast.EnableManageHeadObservations;
  frmGoPhast.EnableFarmMenuItems;
  frmGoPhast.EnableSwrActions;
  SetMt3dCaption;
  finally
    if (frmDisplayData <> nil) then
    begin
      frmDisplayData.EndUpdate;
    end;
  end;
end;

procedure TUndoChangeLgrPackageSelection.UpdateInterbedsInObjects;
var
//  Interbeds: TCSubInterbeds;
  LocalModel: TPhastModel;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  CSub: TCSubBoundary;
//  InterbedIndex: Integer;
//  CSubPkgData: TCSubPackageData;
begin
  LocalModel := frmGoPhast.PhastModel;
//  Interbeds := LocalModel.ModflowPackages.CSubPackage.Interbeds;
  for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
    CSub := AScreenObject.ModflowCSub;
    if CSub <> nil then
    begin
      CSub.Loaded;
    end;
  end;
end;

procedure TUndoChangeLgrPackageSelection.UpdateLayerGroupProperties(
  BcfPackage: TModflowPackageSelection);
var
  Index: Integer;
  LayerGroup: TLayerGroup;
begin
  if frmGoPhast.PhastModel.ModflowPackages.BcfPackage.IsSelected
    <> BcfPackage.IsSelected then
  begin
    if BcfPackage.IsSelected then
    begin
      for Index := 1 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
      begin
        LayerGroup := frmGoPhast.PhastModel.LayerStructure[Index];
        if LayerGroup.InterblockTransmissivityMethod >= 1 then
        begin
          LayerGroup.InterblockTransmissivityMethod :=
            LayerGroup.InterblockTransmissivityMethod + 1;
        end;
        if LayerGroup.AquiferType = 1 then
        begin
          LayerGroup.AquiferType := 3;
        end;
      end;
    end
    else
    begin
      for Index := 1 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
      begin
        LayerGroup := frmGoPhast.PhastModel.LayerStructure[Index];
        if LayerGroup.InterblockTransmissivityMethod >= 1 then
        begin
          LayerGroup.InterblockTransmissivityMethod :=
            LayerGroup.InterblockTransmissivityMethod - 1;
        end;
        if LayerGroup.AquiferType > 1 then
        begin
          LayerGroup.AquiferType := 1;
        end;
      end;
    end;
  end;
end;

procedure TUndoChangeLgrPackageSelection.UpdateMt3dmsChemSpecies;
begin
  FNewMobileComponents.Assign(frmGoPhast.PhastModel.MobileComponents);
  FNewImmobileComponents.Assign(frmGoPhast.PhastModel.ImmobileComponents);
end;

{ TFrameNodeLink }

procedure TFrameNodeLink.SetFrame(const Value: TframePackage);
begin
  FFrame := Value;
end;

procedure TFrameNodeLink.SetNode(const Value: TTreeNode);
begin
  FNode := Value;
end;

end.
