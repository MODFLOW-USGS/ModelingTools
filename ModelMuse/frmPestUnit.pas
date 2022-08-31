unit frmPestUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  JvPageList, JvExControls, Vcl.ComCtrls, JvExComCtrls, JvPageListTreeView,
  ArgusDataEntry, PestPropertiesUnit, Vcl.Buttons, Vcl.ExtCtrls, UndoItems,
  frameGridUnit, frameAvailableObjectsUnit, PestObsUnit, frameParentChildUnit,
  PestObsGroupUnit, Vcl.Mask, JvExMask, JvToolEdit,
  FluxObservationUnit, ModflowHobUnit, System.UITypes,
  System.Generics.Collections, Vcl.Grids, RbwDataGrid4, JvSpin,
  OrderedCollectionUnit, ModflowParameterUnit,
  ModflowTransientListParameterUnit, HufDefinition, JvExStdCtrls, JvHtControls;

type
  TPestObsGroupColumn = (pogcName, pogcRegularization, pogcUseTarget,
    pogcTarget, pogcFileName);
  TPilotPointColumns = (ppcX, ppcY);
  TPriorParmCol = (ppcName, ppcRegularization, ppcGroupName, ppcWeight);
  TParetoColumns = (pcName, pcReport);

  TUndoPestOptions = class(TCustomUndo)
  private
    FOldPestProperties: TPestProperties;
    FNewPestProperties: TPestProperties;
    FOldObsList: TObservationObjectList;
    FNewObsList: TObservationObjectList;
    OldPestLocation: string;
    NewPestLocation: string;
    FOldFluxObsList: TFluxObservationObjectList;
    FNewFluxObsList: TFluxObservationObjectList;
    FOldHobList: THobObjectList;
    FNewHobList: THobObjectList;
    FNewSteadyParameters: TModflowSteadyParameters;
    FOldSteadyParameters: TModflowSteadyParameters;
    FNewTransientParameters: TModflowTransientListParameters;
    FOldTransientParameters: TModflowTransientListParameters;
    FNewHufParameters: THufModflowParameters;
    FOldHufParameters: THufModflowParameters;
    procedure AssignParameters(SteadyParameters: TModflowSteadyParameters;
      HufParameters: THufModflowParameters;
      TransientListParameters: TModflowTransientListParameters);
  protected
    function Description: string; override;
    procedure UpdateProperties(PestProperties: TPestProperties;
      ObsList: TObservationObjectList; FluxObsList: TFluxObservationObjectList;
      HobList: THobObjectList);
  public
    constructor Create(var NewPestProperties: TPestProperties;
      var NewObsList: TObservationObjectList;
      var NewFluxObservationList: TFluxObservationObjectList;
      var NewHobList: THobObjectList;
      var NewSteadyParameters: TModflowSteadyParameters;
      var NewHufParameters: THufModflowParameters;
      var NewTransientListParameters: TModflowTransientListParameters;
      PestDirectory: String);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;

  end;

  TfrmPEST = class(TfrmCustomGoPhast)
    tvPEST: TJvPageListTreeView;
    plMain: TJvPageList;
    jvspBasic: TJvStandardPage;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    comboTemplateCharacter: TComboBox;
    lblTemplateCharacter: TLabel;
    comboFormulaMarker: TComboBox;
    lblFormulaMarker: TLabel;
    jvspControlDataMode: TJvStandardPage;
    cbSaveRestart: TCheckBox;
    lblPestMode: TLabel;
    comboPestMode: TComboBox;
    jvspInversionControls: TJvStandardPage;
    rdeInitialLambda: TRbwDataEntry;
    lblInitialLambda: TLabel;
    rdeLambdaAdj: TRbwDataEntry;
    comboLambdaAdj: TLabel;
    rdeIterationClosure: TRbwDataEntry;
    lblIterationClosure: TLabel;
    rdeLambdaTermination: TRbwDataEntry;
    lblLambdaTermination: TLabel;
    rdeMaxLambdas: TRbwDataEntry;
    lblMaxLambdas: TLabel;
    rdeJacobianUpdate: TRbwDataEntry;
    lblJacobianUpdate: TLabel;
    cbLamForgive: TCheckBox;
    cbDerForgive: TCheckBox;
    jvspParameterAdjustmentControls: TJvStandardPage;
    rdeMaxRelParamChange: TRbwDataEntry;
    lblMaxRelParamChange: TLabel;
    rdeMaxFacParamChange: TRbwDataEntry;
    lblMaxFacParamChange: TLabel;
    rdeFactorOriginal: TRbwDataEntry;
    lblFactorOriginal: TLabel;
    rdeBoundStick: TRbwDataEntry;
    lblBoundStick: TLabel;
    cbParameterBending: TCheckBox;
    jvspInversionControls2: TJvStandardPage;
    rdeSwitchCriterion: TRbwDataEntry;
    lblSwitchCriterion: TLabel;
    rdeSwitchCount: TRbwDataEntry;
    lblSwitchCount: TLabel;
    rdeSplitSlopeCriterion: TRbwDataEntry;
    lblSplitSlopeCriterion: TLabel;
    comboAutomaticUserIntervation: TComboBox;
    lblAutomaticUserIntervation: TLabel;
    cbSensitivityReuse: TCheckBox;
    cbBoundsScaling: TCheckBox;
    jvspIterationControls: TJvStandardPage;
    rdeMaxIterations: TRbwDataEntry;
    lblMaxIterations: TLabel;
    rdePhiReductionCriterion: TRbwDataEntry;
    lblPhiReductionCriterion: TLabel;
    rdePhiReductionCount: TRbwDataEntry;
    lblPhiReductionCount: TLabel;
    rdeNoReductionCount: TRbwDataEntry;
    lblNoReductionCount: TLabel;
    rdeSmallParameterReduction: TRbwDataEntry;
    rdeSmallParameterReductionCount: TRbwDataEntry;
    lblSmallParameterReduction: TLabel;
    lblrdeSmallParameterReductionCount: TLabel;
    rdePhiStoppingThreshold: TRbwDataEntry;
    lblPhiStoppingThreshold: TLabel;
    cbLastRun: TCheckBox;
    rdeAbandon: TRbwDataEntry;
    lblAbandon: TLabel;
    jvspOutputOptions: TJvStandardPage;
    jvspSingularValueDecomp: TJvStandardPage;
    cbWriteCov: TCheckBox;
    cbWriteCorrCoef: TCheckBox;
    cbWriteEigenvectors: TCheckBox;
    cbWriteResolution: TCheckBox;
    cbWriteJacobian: TCheckBox;
    cbWriteJacobianEveryIteration: TCheckBox;
    cbWriteVerboseRunRecord: TCheckBox;
    cbWriteIntermResidualForEveryIteration: TCheckBox;
    cbSaveParamValuesIteration: TCheckBox;
    cbSaveParamValuesModelRun: TCheckBox;
    splMain: TSplitter;
    comboSvdMode: TComboBox;
    lblSvdMode: TLabel;
    rdeMaxSingularValues: TRbwDataEntry;
    lblMaxSingularValues: TLabel;
    rdeEigenThreshold: TRbwDataEntry;
    lblEigenThreshold: TLabel;
    comboEigenWrite: TComboBox;
    lblEigenWrite: TLabel;
    jvspLqsr: TJvStandardPage;
    cbUseLqsr: TCheckBox;
    rdeMatrixTolerance: TRbwDataEntry;
    lblMatrixTolerance: TLabel;
    rdeRightHandSideTolerance: TRbwDataEntry;
    lblRightHandSideTolerance: TLabel;
    rdeConditionNumberLimit: TRbwDataEntry;
    lblConditionNumberLimit: TLabel;
    rdeMaxLqsrIterations: TRbwDataEntry;
    lblMaxLqsrIterations: TLabel;
    cbWriteLsqrOutput: TCheckBox;
    jvspObservationGroups: TJvStandardPage;
    frameObservationGroups: TframeGrid;
    dlgOpenCovarianceMatrixFile: TOpenDialog;
    jvspObsGroupAssignments: TJvStandardPage;
    frameParentObsGroups: TframeParentChild;
    diredPest: TJvDirectoryEdit;
    lblPestDirectory: TLabel;
    jvspPilotPoints: TJvStandardPage;
    rdePilotPointSpacing: TRbwDataEntry;
    lblPilotPointSpacing: TLabel;
    cbShowPilotPoints: TCheckBox;
    gbIndividualPilotPoints: TGroupBox;
    framePilotPoints: TframeGrid;
    btnImportShape: TButton;
    btnImportText: TButton;
    dlgOpenPilotPoints: TOpenDialog;
    btnBetweenObservations: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    gbBetweenPointObs: TGroupBox;
    Splitter1: TSplitter;
    rdgBetweenObs: TRbwDataGrid4;
    Panel3: TPanel;
    cbUseBetweenObs: TCheckBox;
    Panel4: TPanel;
    gbArray: TGroupBox;
    comboArrayPattern: TComboBox;
    lblArrayPattern: TLabel;
    lblPilotPointBuffer: TLabel;
    rdePilotPointBuffer: TRbwDataEntry;
    rdeMinSeparation: TRbwDataEntry;
    lblMinSeparation: TLabel;
    jvspRegularisation: TJvStandardPage;
    rdePhimLim: TRbwDataEntry;
    lblPhimLim: TLabel;
    rdePhimAccept: TRbwDataEntry;
    lblPhimAccept: TLabel;
    cbAutomaticallySetPHIMACCEPT: TCheckBox;
    lblFRACPHIM: TLabel;
    rdeFRACPHIM: TRbwDataEntry;
    cbMemSave: TCheckBox;
    rdeWFINIT: TRbwDataEntry;
    lblWFINIT: TLabel;
    rdeWFMIN: TRbwDataEntry;
    lblWFMIN: TLabel;
    rdeWFMAX: TRbwDataEntry;
    LBLWFMAX: TLabel;
    rdeWFFAC: TRbwDataEntry;
    lblWFFAC: TLabel;
    rdeWFTOL: TRbwDataEntry;
    lblWFTOL: TLabel;
    cbLINREG: TCheckBox;
    cbREGCONTINUE: TCheckBox;
    jvspRegularizationOption: TJvStandardPage;
    rgRegOption: TRadioGroup;
    rgGroupWeightMethod: TRadioGroup;
    rgIndividualAdjustmentMethod: TRadioGroup;
    cbRegApplyGroupWeight: TCheckBox;
    rdeIREGADJ: TRbwDataEntry;
    lblIREGADJ: TLabel;
    seNOPTREGADJ: TJvSpinEdit;
    lblNOPTREGADJ: TLabel;
    rdeREGWEIGHTRAT: TRbwDataEntry;
    lblREGWEIGHTRAT: TLabel;
    rdeREGSINGTHRESH: TRbwDataEntry;
    lblREGSINGTHRESH: TLabel;
    jvspPriorInfoObsGroups: TJvStandardPage;
    framePriorInfoObservationGroups: TframeGrid;
    jvspPriorInfoInitialValue: TJvStandardPage;
    rdgPriorInfoInitialValue: TRbwDataGrid4;
    Panel5: TPanel;
    cbInitialValue: TCheckBox;
    jvspPriorInfoHorizContinuity: TJvStandardPage;
    pnlPriorInfoContinuity: TPanel;
    cbPriorInfoHorizContinuity: TCheckBox;
    rdgPriorInfoHorizContinuity: TRbwDataGrid4;
    rdeSearchDistance: TRbwDataEntry;
    seMaxPilotPoints: TJvSpinEdit;
    lblSearchDistance: TLabel;
    lblMaxPilotPoints: TLabel;
    jvspPriorInfoVertContinuity: TJvStandardPage;
    Panel6: TPanel;
    cbPriorInfoVertContinuity: TCheckBox;
    rdgPriorInfoVertContinuity: TRbwDataGrid4;
    lblArrayMarker: TLabel;
    comboArrayMarker: TComboBox;
    jvspPrediction1: TJvStandardPage;
    comboPredMinMax: TComboBox;
    lblPredMinMax: TLabel;
    cbPredictiveNoise: TCheckBox;
    rdeTargetObjectiveFunction: TRbwDataEntry;
    lblTargetObjectiveFunction: TLabel;
    rdeAcceptedObjectiveFunction: TRbwDataEntry;
    lblAcceptedObjectiveFunction: TLabel;
    rdeTestLambdaPhi: TRbwDataEntry;
    lblTestLambdaPhi: TLabel;
    rdeAbsoluteLamdaCriterion: TRbwDataEntry;
    lblAbsoluteLamdaCriterion: TLabel;
    rdeRelativeLamdaCriterion: TRbwDataEntry;
    lblRelativeLamdaCriterion: TLabel;
    rdeInitialLineSearchFactor: TRbwDataEntry;
    lblInitialLineSearchFactor: TLabel;
    rdeUpdateLineSearchFactor: TRbwDataEntry;
    lblUpdateLineSearchFactor: TLabel;
    seLineSearchRuns: TJvSpinEdit;
    lblLineSearchRuns: TLabel;
    rdeAbsolutePredictionSwitch: TRbwDataEntry;
    lblAbsolutePredictionSwitch: TLabel;
    rdeRelativePredictionSwitch: TRbwDataEntry;
    lblRelativePredictionSwitch: TLabel;
    jvspPrediction2: TJvStandardPage;
    seMaxNoPredictionImprovmentRuns: TJvSpinEdit;
    rdeAbsoluteImprovementCriterion: TRbwDataEntry;
    lblMaxNoPredictionImprovmentRuns: TLabel;
    lblAbsoluteImprovementCriterion: TLabel;
    rdeRelativeImprovementCriterion: TRbwDataEntry;
    lblRelativeImprovementCriterion: TLabel;
    seNumberOfPredictionsToCompare: TJvSpinEdit;
    lblNumberOfPredictionsToCompare: TLabel;
    jvspPareto1: TJvStandardPage;
    comboParetoGroup: TComboBox;
    lblParetoGroup: TLabel;
    rdeInitialParetoWeight: TRbwDataEntry;
    lblInitialParetoWeight: TLabel;
    rdeFinalParetoWeight: TRbwDataEntry;
    lblFinalParetoWeight: TLabel;
    seParetoIncrements: TJvSpinEdit;
    lblParetoIncrements: TLabel;
    seInitialIterationCount: TJvSpinEdit;
    lblInitialIterationCount: TLabel;
    seIntermediateIterationCount: TJvSpinEdit;
    lblIntermediateIterationCount: TLabel;
    seFinalIterationCount: TJvSpinEdit;
    lblFinalIterationCount: TLabel;
    Panel7: TPanel;
    jsvpPareto2: TJvStandardPage;
    cbAltTerminationOption: TCheckBox;
    comboObservationName: TComboBox;
    lblObservationName: TLabel;
    comboAltDirection: TComboBox;
    lblAltDirection: TLabel;
    rdeAltThreshold: TRbwDataEntry;
    lblAltThreshold: TLabel;
    seAltIterations: TJvSpinEdit;
    lblAltIterations: TLabel;
    rdgObservationsToReport: TRbwDataGrid4;
    htlblZoneBudget6: TJvHTLabel;
    rdeMaxCompDim: TRbwDataEntry;
    rdeZeroLimit: TRbwDataEntry;
    lblMaxCompDim: TLabel;
    lblZeroLimit: TLabel;
    btnCheckAllInitialValue: TButton;
    Panel8: TPanel;
    btnMakeAllRegul: TButton;
    btnWithinLayerPrior: TButton;
    btnBetweenLayerPrior: TButton;
    comboPestStatus: TComboBox;
    lblPestStatus: TLabel;
    procedure FormCreate(Sender: TObject); override;
    procedure MarkerChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure cbUseLqsrClick(Sender: TObject);
    procedure comboSvdModeChange(Sender: TObject);
    procedure frameObservationGroupsGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure frameObservationGroupsGridButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure FormDestroy(Sender: TObject); override;
    procedure plMainChange(Sender: TObject);
    procedure frameObservationGroupssbDeleteClick(Sender: TObject);
    procedure frameObservationGroupssbInsertClick(Sender: TObject);
    procedure frameObservationGroupsseNumberChange(Sender: TObject);
    procedure frameObservationGroupsGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure diredPestChange(Sender: TObject);
    procedure rdeSwitchCriterionChange(Sender: TObject);
    procedure btnImportShapeClick(Sender: TObject);
    procedure btnImportTextClick(Sender: TObject);
    procedure btnBetweenObservationsClick(Sender: TObject);
    procedure rdePhimLimChange(Sender: TObject);
    procedure rdePhimAcceptChange(Sender: TObject);
    procedure cbAutomaticallySetPHIMACCEPTClick(Sender: TObject);
    procedure rdeFRACPHIMChange(Sender: TObject);
    procedure rdeWFINITChange(Sender: TObject);
    procedure rdeWFMINChange(Sender: TObject);
    procedure rdeWFMAXChange(Sender: TObject);
    procedure rdeWFFACChange(Sender: TObject);
    procedure rdeWFTOLChange(Sender: TObject);
    procedure UpdateIREGADJ_FromControls(Sender: TObject);
    procedure rdeIREGADJChange(Sender: TObject);
    procedure rdeREGWEIGHTRATChange(Sender: TObject);
    procedure rdeREGSINGTHRESHChange(Sender: TObject);
    procedure frameObservationGroupsGridBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure frameObservationGroupsGridStateChange(Sender: TObject; ACol,
      ARow: Integer; const Value: TCheckBoxState);
    procedure rdgPriorInfoInitialValueStateChange(Sender: TObject; ACol,
      ARow: Integer; const Value: TCheckBoxState);
    procedure rdgPriorInfoInitialValueSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure rdgPriorInfoInitialValueBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure framePriorInfoObservationGroupsGridBeforeDrawCell(Sender: TObject;
      ACol, ARow: Integer);
    procedure framePriorInfoObservationGroupsGridButtonClick(Sender: TObject;
      ACol, ARow: Integer);
    procedure framePriorInfoObservationGroupsGridSelectCell(Sender: TObject;
      ACol, ARow: Integer; var CanSelect: Boolean);
    procedure framePriorInfoObservationGroupsGridSetEditText(Sender: TObject;
      ACol, ARow: Integer; const Value: string);
    procedure framePriorInfoObservationGroupsGridStateChange(Sender: TObject;
      ACol, ARow: Integer; const Value: TCheckBoxState);
    procedure rdgPriorInfoHorizContinuityBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure rdgPriorInfoHorizContinuitySetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure rdgPriorInfoHorizContinuityStateChange(Sender: TObject; ACol,
      ARow: Integer; const Value: TCheckBoxState);
    procedure rdeSearchDistanceChange(Sender: TObject);
    procedure cbPriorInfoHorizContinuityClick(Sender: TObject);
    procedure framePriorInfoObservationGroupsGridExit(Sender: TObject);
    procedure rdgPriorInfoVertContinuityBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure rdgPriorInfoVertContinuitySetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure rdgPriorInfoVertContinuityStateChange(Sender: TObject; ACol,
      ARow: Integer; const Value: TCheckBoxState);
    procedure rgRegOptionClick(Sender: TObject);
    procedure rgGroupWeightMethodClick(Sender: TObject);
    procedure rgIndividualAdjustmentMethodClick(Sender: TObject);
    procedure cbRegApplyGroupWeightClick(Sender: TObject);
    procedure comboPestModeChange(Sender: TObject);
    procedure comboParetoGroupChange(Sender: TObject);
    procedure rdeTargetObjectiveFunctionChange(Sender: TObject);
    procedure rdeAcceptedObjectiveFunctionChange(Sender: TObject);
    procedure rdeTestLambdaPhiChange(Sender: TObject);
    procedure tvPESTMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdeLambdaAdjChange(Sender: TObject);
    procedure rdeMaxRelParamChangeChange(Sender: TObject);
    procedure rdeMaxFacParamChangeChange(Sender: TObject);
    procedure rdeFactorOriginalChange(Sender: TObject);
    procedure rdePhiReductionCriterionChange(Sender: TObject);
    procedure rdeSmallParameterReductionChange(Sender: TObject);
    procedure rdeAbandonChange(Sender: TObject);
    procedure btnCheckAllInitialValueClick(Sender: TObject);
    procedure btnMakeAllRegulClick(Sender: TObject);
    procedure btnWithinLayerPriorClick(Sender: TObject);
    procedure btnBetweenLayerPriorClick(Sender: TObject);
    procedure rdePilotPointSpacingChange(Sender: TObject);
  private
    FObsList: TObservationList;
    FNewObsList: TObservationObjectList;
    FFluxObservationList: TFluxObservationList;
    FNewFluxObservationList: TFluxObservationObjectList;
    FHobList: THobList;
    FNewHobList: THobObjectList;
    FLocalObsGroups: TPestObservationGroups;
    FLocalPriorInfoObsGroups: TPestObservationGroups;
    InvalidateModelEvent: TNotifyEvent;
    FGroupDictionary: TDictionary<TPestObservationGroup, TTreeNode>;
    FGroupNameDictionary: TDictionary<string, TPestObservationGroup>;
    FNoNameNode: TTreeNode;
    FNewSteadyParameters: TModflowSteadyParameters;
    FNewHufParameters: THufModflowParameters;
    FNewTransientListParameters: TModflowTransientListParameters;
    FSettingIREGADJ: Boolean;
    FChangingNumberOfGroups: Boolean;
    procedure GetData;
    procedure SetData;
    procedure FixObsGroupNames(ObsGridFrame: TframeGrid);
    procedure HandleGroupDeletion(Group: TPestObservationGroup);
    procedure HandleAddedGroup(TreeObsGroupFrame: TframeParentChild;
      ObsGroup: TPestObservationGroup);
    procedure CheckPestDirectory;
    procedure ImportPilotPoints(const FileName: string);
    procedure AutoSetPhimAccept;
    procedure SetrdePhimAcceptColor;
    procedure SetWfMaxVisibility;
    function GetIREGADJ: Integer;
    procedure SetIREGADJ(const Value: Integer);
    procedure GetUsedTypes(var UsedTypes: TParameterTypes);
    function PredictGroupOK: boolean;
    property IREGADJ: Integer read GetIREGADJ write SetIREGADJ;
    procedure InsertObsGroup(ObsGroupFrame: TframeGrid; Sender: TObject);
    procedure GetCovarianceFileName(ObsGridFrame: TframeGrid;
      ACol, ARow: Integer);
    procedure CanSelectObsGridCell(ObsGridFrame: TframeGrid;
      ARow, ACol: Integer; var CanSelect: Boolean);
    procedure ChangeObservationGroupName(Grid: TRbwDataGrid4;
      ARow, ACol: Integer; const Value: string);
    procedure ObsGroupDeleteButtonClick(ObsGroupFrame: TframeGrid;
      Sender: TObject);
    procedure GetObsGroups(ObsGroupFrame: TframeGrid;
      ObsGroups, EditedObsGroups: TPestObservationGroups);
    procedure ChangeObsGroupNumber(ObsNameGrid: TframeGrid;
      EditedObsGroups: TPestObservationGroups);
    procedure SetObsGroups(ObsGroups: TPestObservationGroups;
      ObsGroupFrame: TframeGrid; EditedObsGroups: TPestObservationGroups);
    procedure CheckObsGroupName(Grid: TRbwDataGrid4; ARow: Integer; ACol: Integer);
    procedure CheckObsGroupTarget(Grid: TRbwDataGrid4; ARow: Integer; ACol: Integer);
    procedure CheckPriorInfoGroupName(Grid: TRbwDataGrid4; ARow: Integer; ACol: Integer);
    procedure SetSearchDistanceColor;
    procedure AssignParetoObsReports(ParetoProperties: TParetoProperties);
    procedure CheckAllFirstCol(Grid: TRbwDataGrid4);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPEST: TfrmPEST;

const StrNone = '(none)';

implementation

uses
  frmGoPhastUnit, GoPhastTypes, JvComCtrls, PhastModelUnit,
  PointCollectionUnit, QuadTreeClass, ShapefileUnit, System.IOUtils, FastGEO,
  ModelMuseUtilities, ScreenObjectUnit, TriCP_Routines, TriPackRoutines,
  SutraPestObsUnit, System.Math, PestControlFileWriterUnit;

resourcestring
  StrObservationGroupNa = 'Observation Group Name (OBGNME)';
  StrUseGroupTargetGT = 'Use Group Target (GTARG)';
  StrGroupTargetGTARG = 'Group Target (GTARG)';
  StrCovarianceMatrixFi = 'Covariance Matrix File Name (optional) (COVFLE)';
  StrTheShapeHeaderFil = 'The shape header file "%s" could not be found.';
  StrLine0d1sM = 'Line %0:d ("%1:s") must contain at least two values separa' +
  'ted by a comma or space character.';
  StrRegularizationGroup = 'Regularization Group';
  StrX = 'X';
  StrY = 'Y';
  StrParameterName = 'Parameter name';
  StrDefinePriorInforma = 'Define Prior Information';
  StrWhenThePrediction = 'When the prediction analysis mode is used, there m' +
  'ust be an observation group named "predict" with exactly one observation ' +
  'assigned to it. This model fails that criterion. Do you want to continue ' +
  'in spite of this problem?';
  StrBasic = 'Basic';
  StrPilotPoints = 'Pilot Points';
  StrControlData = 'Control Data';
  StrMode = 'Mode and Dimensions';
//  StrDimensions = 'Dimensions';
  StrInversionControls1 = 'Inversion Controls 1';
  StrParameterAdjustment = 'Parameter Adjustment Controls';
  StrInversionControls2 = 'Inversion Controls 2';
  StrIterationControls = 'Iteration Controls';
  StrOutput = 'Output';
  StrSingularValueDecom = 'Singular Value Decomposition';
  StrLQSR = 'LQSR';
  StrObservations = 'Observation Groups';
  StrObservationGroups = 'Observation Group Properties';
  StrObservationGroupAs = 'Observation Group Assignments';
  StrPriorInformation = 'Prior Information';
  StrPriorInformationGr = 'Prior Information Group Properties';
  StrInitialValuePrior = 'Initial Value Prior Information';
  StrWithinLayerContinu = 'Within-Layer Continuity Prior Information';
  StrBetweenLayerContin = 'Between-Layer Continuity Prior Information';
  StrPredictionAnalysis = 'Prediction Analysis';
  StrPredictionAnalysisC = 'Prediction Analysis Controls';
  StrPredictionAnalysisT = 'Prediction Analysis Termination';
  StrRegularization = 'Regularization';
  StrRegularizationContr = 'Regularization Controls';
  StrRegularizationOptio = 'Regularization Options';
  StrPareto = 'Pareto';
  StrParetoControls = 'Pareto Controls';
  StrParetoAlternateTer = 'Pareto Alternate Termination';
  StrYouMustDefineAtL = 'You must define at least two point observations to ' +
  'define pilot points between observations.';
  StrNoObservationPoint = 'No observation points defined.';
  StrWeight = 'Weight';
//  StrObservationsToMoni = 'Observations to monitor (OBS_REPORT_[N])';

type
  TCheckedPointItem = class(TPointItem)
  private
    FChecked: Boolean;
  public
    FoundPoints: TQuadPointInRegionArray;
    constructor Create(Collection: TCollection); override;
    property Checked: Boolean read FChecked write FChecked;
  end;

  TCheckedPoints = class(TCollection)
  private
    function GetPoint(Index: Integer): TCheckedPointItem;
    procedure SetPoint(Index: Integer; const Value: TCheckedPointItem);
  public
    constructor Create;
    function Add: TCheckedPointItem;
    property Items[Index: Integer]: TCheckedPointItem read GetPoint
      write SetPoint; default;
  end;

{$R *.dfm}

procedure TfrmPEST.MarkerChange(Sender: TObject);
begin
  inherited;
  comboTemplateCharacter.Color := clWindow;
  comboFormulaMarker.Color := clWindow;
  comboArrayMarker.Color := clWindow;
  if comboTemplateCharacter.Text = comboFormulaMarker.Text then
  begin
    comboTemplateCharacter.Color := clRed;
    comboFormulaMarker.Color := clRed;
    Beep;
  end;
  if comboArrayMarker.Text = comboFormulaMarker.Text then
  begin
    comboArrayMarker.Color := clRed;
    comboFormulaMarker.Color := clRed;
    Beep;
  end;
  if comboTemplateCharacter.Text = comboArrayMarker.Text then
  begin
    comboTemplateCharacter.Color := clRed;
    comboArrayMarker.Color := clRed;
    Beep;
  end;
end;

procedure TfrmPEST.plMainChange(Sender: TObject);
const
  EmptyRect: TGridRect = (Left: 2; Top: 1; Right: 2; Bottom: 1);
var
  Grid: TRbwDataGrid4;
  RowIndex: Integer;
begin
  inherited;
  if plMain.ActivePage = jvspObservationGroups then
  begin
    FixObsGroupNames(frameObservationGroups);
    Grid := frameObservationGroups.Grid;
    for RowIndex := 1 to frameObservationGroups.seNumber.AsInteger do
    begin
      if Grid.Cells[Ord(pogcName), RowIndex] <> '' then
      begin
      end;
    end;
  end;
  if plMain.ActivePage = jvspPriorInfoInitialValue then
  begin
    rdgPriorInfoInitialValue.HideEditor
  end;
  if plMain.ActivePage = jvspPriorInfoHorizContinuity then
  begin
    rdgPriorInfoHorizContinuity.HideEditor;
  end;
  if plMain.ActivePage = jvspPriorInfoVertContinuity then
  begin
    rdgPriorInfoVertContinuity.HideEditor;
  end;
  if plMain.ActivePage = jvspPareto1 then
  begin
    rdgObservationsToReport.HideEditor;
  end;
  if plMain.ActivePage <> nil then
  begin
    HelpKeyWord := plMain.ActivePage.HelpKeyWord
  end;
end;

procedure TfrmPEST.rdeWFMAXChange(Sender: TObject);
begin
  inherited;
  SetWfMaxVisibility;
end;

procedure TfrmPEST.rdeWFFACChange(Sender: TObject);
var
  Value: Double;
begin
  inherited;
  Value := rdeWFFAC.RealValue;
  if Value = 1 then
  begin
    rdeWFFAC.Color := clRed;
  end
  else
  begin
    rdeWFFAC.Color := clWindow;
  end;
end;

procedure TfrmPEST.rdeWFINITChange(Sender: TObject);
var
  Value: Double;
begin
  inherited;
  Value := rdeWFINIT.RealValue;
  if Value = 0 then
  begin
    rdeWFINIT.Color := clRed;
  end
  else
  begin
    rdeWFINIT.Color := clWindow;
  end;
end;

procedure TfrmPEST.rdeWFMINChange(Sender: TObject);
var
  Value: Double;
begin
  inherited;
  Value := rdeWFMIN.RealValue;
  if Value = 0 then
  begin
    rdeWFMIN.Color := clRed;
  end
  else
  begin
    rdeWFMIN.Color := clWindow;
  end;
  SetWfMaxVisibility;
end;

procedure TfrmPEST.rdeWFTOLChange(Sender: TObject);
var
  Value: Double;
begin
  inherited;
  Value := rdeWFTOL.RealValue;
  if Value = 0 then
  begin
    rdeWFTOL.Color := clRed;
  end
  else
  begin
    rdeWFTOL.Color := clWindow;
  end;
end;

procedure TfrmPEST.rdgPriorInfoHorizContinuityBeforeDrawCell(Sender: TObject; ACol,
  ARow: Integer);
begin
  inherited;
  CheckPriorInfoGroupName(rdgPriorInfoHorizContinuity, ARow, ACol);
end;

procedure TfrmPEST.rdgPriorInfoHorizContinuitySetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  AParam: TModflowSteadyParameter;
  ObsGroupIndex: Integer;
begin
  inherited;
  if (ARow > 0) then
  begin
    if (ACol = Ord(ppcGroupName)) then
    begin
      ObsGroupIndex := framePriorInfoObservationGroups.Grid.Cols[Ord(pogcName)].
        IndexOf(rdgPriorInfoHorizContinuity.Cells[ACol, ARow]);
      if ObsGroupIndex >= 1 then
      begin
        rdgPriorInfoHorizContinuity.Objects[ACol, ARow] :=
          framePriorInfoObservationGroups.Grid.Cols[Ord(pogcName)].Objects[ObsGroupIndex];
      end;
      AParam := rdgPriorInfoHorizContinuity.Objects[Ord(ppcName), ARow] as TModflowSteadyParameter;
      if AParam <> nil then
      begin
        AParam.HorizontalSpatialContinuityGroupName := rdgPriorInfoHorizContinuity.Cells[ACol, ARow];
      end;
    end;
    if (ACol = Ord(ppcWeight)) then
    begin
      AParam := rdgPriorInfoHorizContinuity.Objects[Ord(ppcName), ARow] as TModflowSteadyParameter;
      if AParam <> nil then
      begin
        AParam.HorizontalSpatialContinuityPriorInfoWeight :=
          rdgPriorInfoHorizContinuity.RealValueDefault[ACol, ARow, 1];
      end;
    end;
  end;
end;

procedure TfrmPEST.rdgPriorInfoHorizContinuityStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
var
  AParam: TModflowSteadyParameter;
begin
  inherited;
  if (ACol = Ord(ppcRegularization)) and (ARow > 0) then
  begin
    AParam := rdgPriorInfoHorizContinuity.Objects[Ord(ppcName), ARow] as TModflowSteadyParameter;
    if AParam <> nil then
    begin
      AParam.UseHorizontalSpatialContinuityPriorInfo := rdgPriorInfoHorizContinuity.Checked[ACol, ARow];
    end;
  end;
end;

procedure TfrmPEST.rdgPriorInfoInitialValueBeforeDrawCell(Sender: TObject; ACol,
  ARow: Integer);
begin
  inherited;
  CheckPriorInfoGroupName(rdgPriorInfoInitialValue, ARow, ACol);
end;

procedure TfrmPEST.rdgPriorInfoInitialValueSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  AParam: TModflowParameter;
  ObsGroupIndex: Integer;
begin
  inherited;
  if (ARow > 0) then
  begin
    if (ACol = Ord(ppcGroupName)) then
    begin
      ObsGroupIndex := framePriorInfoObservationGroups.Grid.Cols[Ord(pogcName)].
        IndexOf(rdgPriorInfoInitialValue.Cells[ACol, ARow]);
      if ObsGroupIndex >= 1 then
      begin
        rdgPriorInfoInitialValue.Objects[ACol, ARow] :=
          framePriorInfoObservationGroups.Grid.Cols[Ord(pogcName)].Objects[ObsGroupIndex];
      end;
      AParam := rdgPriorInfoInitialValue.Objects[Ord(ppcName), ARow] as TModflowParameter;
      if AParam <> nil then
      begin
        AParam.RegularizationGroup := rdgPriorInfoInitialValue.Cells[ACol, ARow];
      end;
    end;
    if (ACol = Ord(ppcWeight)) then
    begin
      AParam := rdgPriorInfoInitialValue.Objects[Ord(ppcName), ARow] as TModflowParameter;
      if AParam <> nil then
      begin
        AParam.InitialValuePriorInfoWeight := rdgPriorInfoInitialValue.RealValueDefault[ACol, ARow, 1];
      end;
    end;
  end;
end;

procedure TfrmPEST.rdgPriorInfoInitialValueStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
var  
  AParam: TModflowParameter;
begin
  inherited;
  if (ACol = Ord(ppcRegularization)) and (ARow > 0) then
  begin
    AParam := rdgPriorInfoInitialValue.Objects[Ord(ppcName), ARow] as TModflowParameter;
    if AParam <> nil then
    begin
      AParam.UseInitialValuePriorInfo := rdgPriorInfoInitialValue.Checked[ACol, ARow];
    end;
  end;
end;

procedure TfrmPEST.rdgPriorInfoVertContinuityBeforeDrawCell(Sender: TObject;
  ACol, ARow: Integer);
begin
  inherited;
  CheckPriorInfoGroupName(rdgPriorInfoVertContinuity, ARow, ACol);
end;

procedure TfrmPEST.rdgPriorInfoVertContinuitySetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  AParam: TModflowSteadyParameter;
  ObsGroupIndex: Integer;
begin
  inherited;
  if (ARow > 0) then
  begin
    if (ACol = Ord(ppcGroupName)) then
    begin
      ObsGroupIndex := framePriorInfoObservationGroups.Grid.Cols[Ord(pogcName)].
        IndexOf(rdgPriorInfoVertContinuity.Cells[ACol, ARow]);
      if ObsGroupIndex >= 1 then
      begin
        rdgPriorInfoVertContinuity.Objects[ACol, ARow] :=
          framePriorInfoObservationGroups.Grid.Cols[Ord(pogcName)].Objects[ObsGroupIndex];
      end;
      AParam := rdgPriorInfoVertContinuity.Objects[Ord(ppcName), ARow] as TModflowSteadyParameter;
      if AParam <> nil then
      begin
        AParam.VertSpatialContinuityGroupName := rdgPriorInfoVertContinuity.Cells[ACol, ARow];
      end;
    end;
    if (ACol = Ord(ppcWeight)) then
    begin
      AParam := rdgPriorInfoVertContinuity.Objects[Ord(ppcName), ARow] as TModflowSteadyParameter;
      if AParam <> nil then
      begin
        AParam.VertSpatialContinuityPriorInfoWeight :=
          rdgPriorInfoVertContinuity.RealValueDefault[ACol, ARow, 1];
      end;
    end;
  end;
end;

procedure TfrmPEST.rdgPriorInfoVertContinuityStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
var
  AParam: TModflowSteadyParameter;
begin
  inherited;
    //TPriorParmCol = (ppcName, ppcRegularization, ppcGroupName);
  if (ACol = Ord(ppcRegularization)) and (ARow > 0) then
  begin
    AParam := rdgPriorInfoVertContinuity.Objects[Ord(ppcName), ARow] as TModflowSteadyParameter;
    if AParam <> nil then
    begin
      AParam.UseVertSpatialContinuityPriorInfo := rdgPriorInfoVertContinuity.Checked[ACol, ARow];
    end;
  end;
end;

procedure TfrmPEST.rgGroupWeightMethodClick(Sender: TObject);
begin
  inherited;
  rdeIREGADJ.IntegerValue := IREGADJ;
end;

procedure TfrmPEST.rgIndividualAdjustmentMethodClick(Sender: TObject);
begin
  inherited;
  rdeIREGADJ.IntegerValue := IREGADJ;
end;

procedure TfrmPEST.rgRegOptionClick(Sender: TObject);
begin
  inherited;
  rdeIREGADJ.IntegerValue := IREGADJ;
end;

procedure TfrmPEST.rdeAbandonChange(Sender: TObject);
begin
  inherited;
  if rdeAbandon.RealValue = 0 then
  begin
    rdeAbandon.Color := clRed;
  end
  else
  begin
    rdeAbandon.Color := clWindow;
  end;
end;

procedure TfrmPEST.rdeAcceptedObjectiveFunctionChange(Sender: TObject);
var
  PD0: Double;
  PD1: Double;
begin
  inherited;
  if ([csLoading, csReading] * ComponentState) <> [] then
  begin
    Exit;
  end;
  PD0 := rdeTargetObjectiveFunction.RealValue;
  PD1 := rdeAcceptedObjectiveFunction.RealValue;
  if PD1 > PD0 then
  begin
    rdeAcceptedObjectiveFunction.Color := clWindow;
  end
  else
  begin
    rdeAcceptedObjectiveFunction.Color := clRed;
  end;
  rdeTestLambdaPhiChange(nil);
end;

procedure TfrmPEST.rdeFactorOriginalChange(Sender: TObject);
begin
  inherited;
  if rdeFactorOriginal.RealValue = 0 then
  begin
    rdeFactorOriginal.Color := clRed;
  end
  else
  begin
    rdeFactorOriginal.Color := clWindow;
  end;
end;

procedure TfrmPEST.rdeFRACPHIMChange(Sender: TObject);
var
  Value: Double;
begin
  inherited;
  Value := rdeFRACPHIM.RealValue;
  if (Value = 0) or (Value = 1) then
  begin
    rdeFRACPHIM.Color := clRed;
  end
  else
  begin
    rdeFRACPHIM.Color := clWindow;
  end;
end;

procedure TfrmPEST.rdeIREGADJChange(Sender: TObject);
begin
  inherited;
  if rdeIREGADJ.Text <> '' then
  begin
    IREGADJ := rdeIREGADJ.IntegerValue;  
  end;
end;

procedure TfrmPEST.rdeLambdaAdjChange(Sender: TObject);
begin
  inherited;
  if Abs(rdeLambdaAdj.RealValue) <= 1 then
  begin
    rdeLambdaAdj.Color := clRed;
  end
  else
  begin
    rdeLambdaAdj.Color := clWindow;
  end;
end;

procedure TfrmPEST.rdeMaxFacParamChangeChange(Sender: TObject);
begin
  inherited;
  if rdeMaxFacParamChange.RealValue <= 1 then
  begin
    rdeMaxFacParamChange.Color := clRed;
  end
  else
  begin
    rdeMaxFacParamChange.Color := clWindow;
  end;
end;

procedure TfrmPEST.rdeMaxRelParamChangeChange(Sender: TObject);
begin
  inherited;
  if rdeMaxRelParamChange.RealValue = 0 then
  begin
    rdeMaxRelParamChange.Color := clRed;
  end
  else
  begin
    rdeMaxRelParamChange.Color := clWindow;
  end;
end;

procedure TfrmPEST.rdePhimAcceptChange(Sender: TObject);
begin
  inherited;
  SetrdePhimAcceptColor;
end;

procedure TfrmPEST.rdePhimLimChange(Sender: TObject);
var
  Value: Double;
begin
  inherited;
  Value := rdePhimLim.RealValue;
  if Value = 0 then
  begin
    rdePhimLim.Color := clRed;
  end
  else
  begin
    rdePhimLim.Color := clWindow;
  end;
  AutoSetPhimAccept;
  SetrdePhimAcceptColor;
end;

procedure TfrmPEST.rdePhiReductionCriterionChange(Sender: TObject);
begin
  inherited;
  if Abs(rdePhiReductionCriterion.RealValue) = 0 then
  begin
    rdePhiReductionCriterion.Color := clRed;
  end
  else
  begin
    rdePhiReductionCriterion.Color := clWindow;
  end;
end;

procedure TfrmPEST.rdePilotPointSpacingChange(Sender: TObject);
var
  AValue: Double;
begin
  inherited;
  if TryStrToFloat(rdePilotPointSpacing.Text, AValue)
    and (comboArrayPattern.ItemIndex = 0) then
  begin
    comboArrayPattern.ItemIndex := 1;
  end;
end;

procedure TfrmPEST.rdeREGSINGTHRESHChange(Sender: TObject);
var
  Value: Double;
begin
  inherited;
  Value := rdeREGSINGTHRESH.RealValue;
  if (Value = 0) or (Value = 1) then
  begin
    rdeREGSINGTHRESH.Color := clRed;
  end
  else
  begin
    rdeREGSINGTHRESH.Color := clWindow;
  end;
end;

procedure TfrmPEST.rdeREGWEIGHTRATChange(Sender: TObject);
begin
  inherited;
  if Abs(rdeREGWEIGHTRAT.RealValue) <= 1 then
  begin
    rdeREGWEIGHTRAT.Color := clRed;
  end
  else
  begin
    rdeREGWEIGHTRAT.Color := clWindow;
  end;
end;

procedure TfrmPEST.rdeSearchDistanceChange(Sender: TObject);
begin
  inherited;
  SetSearchDistanceColor;

end;

procedure TfrmPEST.rdeSmallParameterReductionChange(Sender: TObject);
begin
  inherited;
  if Abs(rdeSmallParameterReduction.RealValue) = 0 then
  begin
    rdeSmallParameterReduction.Color := clRed;
  end
  else
  begin
    rdeSmallParameterReduction.Color := clWindow;
  end;
end;

procedure TfrmPEST.rdeSwitchCriterionChange(Sender: TObject);
begin
  inherited;
  if rdeSwitchCriterion.RealValue = 0 then
  begin
    rdeSwitchCriterion.Color := clRed;
  end
  else
  begin
    rdeSwitchCriterion.Color := clWindow;
  end;
end;

procedure TfrmPEST.rdeTargetObjectiveFunctionChange(Sender: TObject);
var
  PD0: Double;
begin
  inherited;
  if ([csLoading, csReading] * ComponentState) <> [] then
  begin
    Exit;
  end;
  PD0 := rdeTargetObjectiveFunction.RealValue;
  if PD0 > 0 then
  begin
    rdeTargetObjectiveFunction.Color := clWindow;
  end
  else
  begin
    rdeTargetObjectiveFunction.Color := clRed;
  end;
  rdeAcceptedObjectiveFunctionChange(nil);
end;

procedure TfrmPEST.rdeTestLambdaPhiChange(Sender: TObject);
var
  PD1: Double;
  PD2: Double;
begin
  inherited;
  if ([csLoading, csReading] * ComponentState) <> [] then
  begin
    Exit;
  end;
  PD1 := rdeAcceptedObjectiveFunction.RealValue;
  PD2 := rdeTestLambdaPhi.RealValue;
  if PD2 > PD1 then
  begin
    rdeTestLambdaPhi.Color := clWindow;
  end
  else
  begin
    rdeTestLambdaPhi.Color := clRed;
  end;
end;

procedure TfrmPEST.btnBetweenObservationsClick(Sender: TObject);
var
  ScreenObjectIndex: Integer;
  PhastModel: TPhastModel;
  AScreenObject: TScreenObject;
  UsePoint: Boolean;
  QuadTree: TRbwQuadTree;
  DisLimits: TGridLimit;
  PointList: TList<TPoint2D>;
  ResultPointList: TList<TPoint2D>;
  XD: TRealArray;
  YD: TRealArray;
  NT: longint; 
  NL: longint;
  IPT: TIntArray; 
  IPL: TIntArray;
//  ResultPointList: TList<TPoint2D>;
  APoint2D: TPoint2D;
  DupPoint: TPoint2D;
  PointIndex: Integer;
  SutraStateObs: TSutraStateObservations;
  APointer: Pointer;
  TriangleIndex: Integer;
  APoint1: TPoint2D;
  APoint2: TPoint2D;
  APoint3: TPoint2D;
  FinalItems: array of TCheckedPointItem;
  CheckedPoints: TCheckedPoints;
  ACheckedPoint: TCheckedPointItem;
  SearchRadius: Double;
//  FoundPoints: TQuadPointInRegionArray;
  MaxCount: Integer;
//  InnerPointItem: Integer;
  RegionIndex: Integer;
  ARegion: TQuadPointInRegion;
  CheckPoint: TCheckedPointItem;
  AnotherCheckItem: TCheckedPointItem;
  TestItem: TCheckedPointItem;
  RowIndex: Integer;
  MaxItem: TCheckedPointItem;
  TestCount: Integer;
  procedure GetMidPoint(P1, P2: TPoint2D);
  begin
    APoint2D.x := (P1.x + P2.x)/2;
    APoint2D.y := (P1.y + P2.y)/2;
    if ResultPointList.Count > 0 then
    begin
      DupPoint := APoint2D;
      QuadTree.FirstNearestPoint(DupPoint.x, DupPoint.y, APointer);
      if (DupPoint.x <> APoint2D.x) or (DupPoint.y <> APoint2D.y) then
      begin
        ResultPointList.Add(APoint2D);
        QuadTree.AddPoint(APoint2D.x, APoint2D.y, nil);
      end;
    end
    else
    begin
      ResultPointList.Add(APoint2D);
      QuadTree.AddPoint(APoint2D.x, APoint2D.y, nil);
    end;
  end;
begin
  inherited;
  cbUseBetweenObs.Checked := True;
  //
  PhastModel := frmGoPhast.PhastModel;
  PointList := TList<TPoint2D>.Create;
  ResultPointList := TList<TPoint2D>.Create;
  QuadTree := TRbwQuadTree.Create(nil);
  try
    DisLimits := PhastModel.DiscretizationLimits(vdTop);
    QuadTree.XMax := DisLimits.MaxX;
    QuadTree.XMin := DisLimits.MinX;
    QuadTree.YMax := DisLimits.MaxY;
    QuadTree.YMin := DisLimits.MinY;
    for ScreenObjectIndex := 0 to PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := PhastModel.ScreenObjects[ScreenObjectIndex];
      if AScreenObject.Deleted or (AScreenObject.Count > 1) then
      begin
        Continue;
      end;

      UsePoint := False;
      if (PhastModel.ModelSelection in ModflowSelection) then
      begin
        if PhastModel.ModelSelection = msModflow2015 then
        begin
          if (AScreenObject.Modflow6Obs = nil)
            or not AScreenObject.Modflow6Obs.Used then
          begin
            Continue;
          end;
          UsePoint := AScreenObject.Modflow6Obs.CalibrationObservations.Count > 0;
        end
        else
        begin
          if PhastModel.ModflowPackages.Mnw2Package.IsSelected then
          begin
            if (AScreenObject.ModflowMnw2Boundary <> nil)
              and AScreenObject.ModflowMnw2Boundary.Used
              and (AScreenObject.ModflowMnw2Boundary.Observations.Count > 0) then
            begin
              UsePoint := True;
            end;
          end;

          if PhastModel.ModflowPackages.HobPackage.IsSelected then
          begin
            if (AScreenObject.ModflowHeadObservations <> nil)
              and AScreenObject.ModflowHeadObservations.Used then
            begin
              UsePoint := True;
            end;
          end;
        end;
      end;
      if (PhastModel.ModelSelection in SutraSelection) then
      begin
        SutraStateObs := AScreenObject.SutraBoundaries.SutraStateObs;
        if SutraStateObs.Used and (SutraStateObs.Count > 0) then
        begin
          UsePoint := True;
        end;
      end;

      if UsePoint then
      begin
        APoint2D := AScreenObject.Points[0];
        if PointList.Count = 0 then
        begin
          PointList.Add(APoint2D);
          QuadTree.AddPoint(APoint2D.x, APoint2D.y, nil);
        end
        else
        begin
          DupPoint := APoint2D;
          QuadTree.FirstNearestPoint(DupPoint.x, DupPoint.y, APointer);
          if (DupPoint.x <> APoint2D.x) or (DupPoint.y <> APoint2D.y) then
          begin
            PointList.Add(APoint2D);
            QuadTree.AddPoint(APoint2D.x, APoint2D.y, nil);
          end;
        end;
      end;
    end;
    
    if PointList.Count = 0 then
    begin
      Beep;
      MessageDlg(StrNoObservationPoint, mtError, [mbOK], 0);
    end;

    if PointList.Count < 4 then
    begin
      case PointList.Count of
        1:
          begin
            Beep;
            MessageDlg(StrYouMustDefineAtL, mtInformation, [mbOK], 0);
            Exit;
          end;
        2:
          begin
            APoint1 := PointList[0];
            APoint2 := PointList[1];
            GetMidPoint(APoint1, APoint2);
          end;
        3:
          begin
            APoint1 := PointList[0];
            APoint2 := PointList[1];
            APoint3 := PointList[2];
            GetMidPoint(APoint1, APoint2);
            GetMidPoint(APoint1, APoint3);
            GetMidPoint(APoint2, APoint3);
          end;
        else
          Assert(False);
      end;
    end
    else
    begin
      QuadTree.Clear;
      SetLength(XD, PointList.Count);
      SetLength(YD, PointList.Count);
      for PointIndex := 0 to PointList.Count -1 do
      begin
        APoint2D := PointList[PointIndex];
        XD[PointIndex] := APoint2D.x;
        YD[PointIndex] := APoint2D.y;
      end;
      SetLength(IPT, 6*PointList.Count-15);
      SetLength(IPL, 6*PointList.Count);
      IDTANG_Pascal(PointList.Count, XD,YD, NT, NL, IPT, IPL);

      for TriangleIndex := 0 to NT - 1 do
      begin
        APoint1 := PointList[IPT[TriangleIndex*3]];
        APoint2 := PointList[IPT[TriangleIndex*3+1]];
        APoint3 := PointList[IPT[TriangleIndex*3+2]];
        GetMidPoint(APoint1, APoint2);
        GetMidPoint(APoint1, APoint3);
        GetMidPoint(APoint2, APoint3);
      end;
    end;

    SearchRadius := rdeMinSeparation.RealValue;
    if SearchRadius = 0 then
    begin
      rdgBetweenObs.BeginUpdate;
      try
        rdgBetweenObs.RowCount := ResultPointList.Count+1;
        rdgBetweenObs.FixedRows := 1;
        for PointIndex := 0 to ResultPointList.count - 1 do
        begin
          APoint2D := ResultPointList[PointIndex];
          rdgBetweenObs.RealValue [Ord(ppcX), PointIndex +1] := APoint2D.x;
          rdgBetweenObs.RealValue [Ord(ppcY), PointIndex +1] := APoint2D.y;
        end;
      finally
        rdgBetweenObs.EndUpdate;
      end;
    end
    else
    begin
      QuadTree.Clear;
      SetLength(FinalItems, ResultPointList.Count);
      CheckedPoints := TCheckedPoints.Create;
      try
        for PointIndex := 0 to ResultPointList.Count - 1 do
        begin
          ACheckedPoint := CheckedPoints.Add;
          ACheckedPoint.Point2D := ResultPointList[PointIndex];
          FinalItems[PointIndex] := ACheckedPoint;
          QuadTree.AddPoint(ACheckedPoint.Point2D.x, ACheckedPoint.Point2D.y,
            ACheckedPoint);
        end;

        for PointIndex := 0 to ResultPointList.Count - 1 do
        begin
          CheckPoint := FinalItems[PointIndex];
          if (CheckPoint = nil) or CheckPoint.Checked then
          begin
            Continue;
          end;
          QuadTree.FindPointsInCircle(CheckPoint.Point2D.x,
            CheckPoint.Point2D.y, SearchRadius, CheckPoint.FoundPoints);
          MaxCount := Length(CheckPoint.FoundPoints);
          if MaxCount > 1 then
          begin
            MaxItem := CheckPoint;
            for RegionIndex := 0 to Length(CheckPoint.FoundPoints) - 1 do
            begin
              ARegion := CheckPoint.FoundPoints[RegionIndex];
              Assert(Length(ARegion.Data) = 1);
              AnotherCheckItem := ARegion.Data[0];
              QuadTree.FindPointsInCircle(AnotherCheckItem.Point2D.x,
                AnotherCheckItem.Point2D.y, SearchRadius,
                AnotherCheckItem.FoundPoints);
              TestCount := Length(AnotherCheckItem.FoundPoints);
              if TestCount > MaxCount then
              begin
                MaxCount := TestCount;
                MaxItem := AnotherCheckItem;
              end;
            end;

            MaxItem.Checked := True;
            for RegionIndex := 0 to MaxCount - 1 do
            begin
              ARegion := MaxItem.FoundPoints[RegionIndex];
              TestItem := ARegion.Data[0];
              QuadTree.RemovePoint(TestItem.Point2D.x, TestItem.Point2D.y,
                TestItem);
              if TestItem <> MaxItem then
              begin
                FinalItems[TestItem.Index] := nil;
              end;
            end;
          end;
        end;

        rdgBetweenObs.BeginUpdate;
        try
          RowIndex := 1;
          rdgBetweenObs.RowCount := Length(FinalItems)+1;
          rdgBetweenObs.FixedRows := 1;
          for PointIndex := 0 to Length(FinalItems) - 1 do
          begin
            CheckPoint := FinalItems[PointIndex];
            if CheckPoint <> nil then
            begin
              rdgBetweenObs.RealValue [Ord(ppcX), RowIndex] := CheckPoint.Point2D.x;
              rdgBetweenObs.RealValue [Ord(ppcY), RowIndex] := CheckPoint.Point2D.y;
              Inc(RowIndex);
            end;
          end;
          rdgBetweenObs.RowCount := RowIndex;
        finally
          rdgBetweenObs.EndUpdate;
        end;

      finally
        CheckedPoints.Free;
      end;
    end;
//    end;
  finally
    PointList.Free;
    ResultPointList.Free;
    QuadTree.Free;
  end;
end;

procedure TfrmPEST.btnCheckAllInitialValueClick(Sender: TObject);
begin
  inherited;
  cbInitialValue.Checked := True;
  CheckAllFirstCol(rdgPriorInfoInitialValue);
end;

procedure TfrmPEST.btnImportShapeClick(Sender: TObject);
begin
  inherited;
  dlgOpenPilotPoints.FilterIndex := 1;
  if dlgOpenPilotPoints.Execute then
  begin
    ImportPilotPoints(dlgOpenPilotPoints.FileName);
  end;
end;

procedure TfrmPEST.btnImportTextClick(Sender: TObject);
begin
  inherited;
  dlgOpenPilotPoints.FilterIndex := 2;
  if dlgOpenPilotPoints.Execute then
  begin
    ImportPilotPoints(dlgOpenPilotPoints.FileName);
  end;
end;

procedure TfrmPEST.btnMakeAllRegulClick(Sender: TObject);
begin
  inherited;
  CheckAllFirstCol(framePriorInfoObservationGroups.Grid);
end;

function TfrmPEST.PredictGroupOK: boolean;
var
  Grid: TRbwDataGrid4;
  PredictGroupFound: Boolean;
  ObsGroupIndex: Integer;
  GroupName: string;
  ANode: TTreeNode;
  ObsGroup: TPestObservationGroup;
  ChildNode: TTreeNode;
  PredictCount: Integer;
begin
  result := True;
  if comboPestMode.ItemIndex = Ord(pmPrediction) then
  begin
    Grid := frameObservationGroups.Grid;
    PredictGroupFound := False;
    for ObsGroupIndex := 0 to frameObservationGroups.seNumber.AsInteger - 1 do
    begin
      GroupName := LowerCase(Grid.Cells[Ord(pogcName), ObsGroupIndex + 1]);
      if GroupName = 'predict' then
      begin
        PredictGroupFound := True;
        break;
      end;
    end;

    if not PredictGroupFound then
    begin
      result := False;
      Exit;
    end;

    PredictCount := 0;
    ANode := FNoNameNode;
    while ANode <> nil do
    begin
      ObsGroup := ANode.Data;
      if (ObsGroup <> nil) and (LowerCase(ObsGroup.ObsGroupName) = 'predict') then
      begin
        ChildNode := ANode.getFirstChild;
        while ChildNode <> nil do
        begin
          Inc(PredictCount);
          ChildNode := ChildNode.GetNextSibling;
        end;
      end;

      ANode := ANode.GetNextSibling;
    end;
    result := PredictCount = 1;
  end;
end;

procedure TfrmPEST.btnOKClick(Sender: TObject);
begin
  inherited;
  if not PredictGroupOK then
  begin
    Beep;
    if MessageDlg(StrWhenThePrediction, mtError, [mbYes, mbNo], 0) <> mrYes then
    begin
      ModalResult := mrNone;
      Exit;
    end;
  end;
  SetData;
end;

procedure TfrmPEST.btnWithinLayerPriorClick(Sender: TObject);
begin
  inherited;
  cbPriorInfoHorizContinuity.Checked := True;
  CheckAllFirstCol(rdgPriorInfoHorizContinuity);
end;

procedure TfrmPEST.btnBetweenLayerPriorClick(Sender: TObject);
begin
  inherited;
  cbPriorInfoVertContinuity.Checked := True;
  CheckAllFirstCol(rdgPriorInfoVertContinuity);
end;

procedure TfrmPEST.cbAutomaticallySetPHIMACCEPTClick(Sender: TObject);
begin
  inherited;
  AutoSetPhimAccept;
  rdePhimAccept.Enabled := not cbAutomaticallySetPHIMACCEPT.Checked;
end;

procedure TfrmPEST.cbPriorInfoHorizContinuityClick(Sender: TObject);
begin
  inherited;
  SetSearchDistanceColor;
end;

procedure TfrmPEST.cbRegApplyGroupWeightClick(Sender: TObject);
begin
  inherited;
  rdeIREGADJ.IntegerValue := IREGADJ;
end;

procedure TfrmPEST.cbUseLqsrClick(Sender: TObject);
begin
  inherited;
  if cbUseLqsr.Checked then
  begin
    comboSvdMode.ItemIndex := 0;
  end;
end;


procedure TfrmPEST.comboParetoGroupChange(Sender: TObject);
var
  PestProperties: TPestProperties;
  ParetoProperties: TParetoProperties;
begin
  inherited;
  PestProperties := frmGoPhast.PhastModel.PestProperties;
  ParetoProperties := PestProperties.ParetoProperties;
  AssignParetoObsReports(ParetoProperties);
end;

procedure TfrmPEST.comboPestModeChange(Sender: TObject);
var
  ObsGroupIndex: Integer;
  Grid: TRbwDataGrid4;
  GroupName: string;
  PredictGroupFound: Boolean;
begin
  inherited;
  if comboPestMode.ItemIndex = Ord(pmPrediction) then
  begin
    Grid := frameObservationGroups.Grid;
    PredictGroupFound := False;
    for ObsGroupIndex := 0 to frameObservationGroups.seNumber.AsInteger - 1 do
    begin
      GroupName := LowerCase(Grid.Cells[Ord(pogcName), ObsGroupIndex + 1]);
      if GroupName = 'predict' then
      begin
        PredictGroupFound := True;
        break;
      end;
    end;
//    if not PredictGroupFound then
//    begin
//      Grid := framePriorInfoObservationGroups.Grid;
//      for ObsGroupIndex := 0 to framePriorInfoObservationGroups.seNumber.AsInteger - 1 do
//      begin
//        GroupName := LowerCase(Grid.Cells[Ord(pogcName), ObsGroupIndex + 1]);
//        if GroupName = 'predict' then
//        begin
//          PredictGroupFound := True;
//          break;
//        end;
//      end;
//    end;
    if not PredictGroupFound then
    begin
      Grid := frameObservationGroups.Grid;
      frameObservationGroups.seNumber.AsInteger :=
        frameObservationGroups.seNumber.AsInteger +1;
      Grid.Cells[Ord(pogcName), frameObservationGroups.seNumber.AsInteger] := 'predict';
      frameObservationGroupsGridSetEditText(Grid, Ord(pogcName),
        frameObservationGroups.seNumber.AsInteger, 'predict');
    end;
  end;
end;

procedure TfrmPEST.comboSvdModeChange(Sender: TObject);
begin
  inherited;
  if comboSvdMode.ItemIndex > 0 then
  begin
    cbUseLqsr.Checked := False;
  end;
end;

procedure TfrmPEST.diredPestChange(Sender: TObject);
begin
  inherited;
  CheckPestDirectory;
end;

procedure TfrmPEST.FormCreate(Sender: TObject);
var
  NewNode: TJvPageIndexNode;
  ControlDataNode: TJvPageIndexNode;
  ObservationNode: TJvPageIndexNode;
  RegularizationNode: TJvPageIndexNode;
  PriorInfoNode: TJvPageIndexNode;
  PredictionNode: TJvPageIndexNode;
  ParetoNode: TJvPageIndexNode;
begin
  inherited;
  FObsList := TObservationList.Create;
  FNewObsList := TObservationObjectList.Create;

  FFluxObservationList := TFluxObservationList.Create;
  FNewFluxObservationList := TFluxObservationObjectList.Create;

  FHobList :=  THobList.Create;
  FNewHobList := THobObjectList.Create;

  FNewSteadyParameters := TModflowSteadyParameters.Create(nil);
  FNewHufParameters := THufModflowParameters.Create(nil);
  FNewTransientListParameters := TModflowTransientListParameters.Create(nil);

  InvalidateModelEvent := nil;
  FLocalObsGroups := TPestObservationGroups.Create(nil);
  FLocalPriorInfoObsGroups := TPestObservationGroups.Create(nil);
  FGroupDictionary := TDictionary<TPestObservationGroup, TTreeNode>.Create;
  FGroupNameDictionary := TDictionary<string, TPestObservationGroup>.Create;

  NewNode := tvPEST.Items.AddChild(
    nil, StrBasic) as TJvPageIndexNode;
  NewNode.PageIndex := jvspBasic.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    nil, StrPilotPoints) as TJvPageIndexNode;
  NewNode.PageIndex := jvspPilotPoints.PageIndex;

  ControlDataNode := tvPEST.Items.AddChild(
    nil, StrControlData) as TJvPageIndexNode;
  ControlDataNode.PageIndex := -1;

  NewNode := tvPEST.Items.AddChild(
    ControlDataNode, StrMode) as TJvPageIndexNode;
  NewNode.PageIndex := jvspControlDataMode.PageIndex;

//  NewNode := tvPEST.Items.AddChild(
//    ControlDataNode, StrDimensions) as TJvPageIndexNode;
//  NewNode.PageIndex := jvspDimensions.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    ControlDataNode, StrInversionControls1) as TJvPageIndexNode;
  NewNode.PageIndex := jvspInversionControls.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    ControlDataNode, StrParameterAdjustment) as TJvPageIndexNode;
  NewNode.PageIndex := jvspParameterAdjustmentControls.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    ControlDataNode, StrInversionControls2) as TJvPageIndexNode;
  NewNode.PageIndex := jvspInversionControls2.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    ControlDataNode, StrIterationControls) as TJvPageIndexNode;
  NewNode.PageIndex := jvspIterationControls.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    ControlDataNode, StrOutput) as TJvPageIndexNode;
  NewNode.PageIndex := jvspOutputOptions.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    nil, StrSingularValueDecom) as TJvPageIndexNode;
  NewNode.PageIndex := jvspSingularValueDecomp.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    nil, StrLQSR) as TJvPageIndexNode;
  NewNode.PageIndex := jvspLqsr.PageIndex;

  ObservationNode := tvPEST.Items.AddChild(
    nil, StrObservations) as TJvPageIndexNode;
  ObservationNode.PageIndex := -1;

  NewNode := tvPEST.Items.AddChild(
    ObservationNode, StrObservationGroups) as TJvPageIndexNode;
  NewNode.PageIndex := jvspObservationGroups.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    ObservationNode, StrObservationGroupAs) as TJvPageIndexNode;
  NewNode.PageIndex := jvspObsGroupAssignments.PageIndex;

  PriorInfoNode := tvPEST.Items.AddChild(
    nil, StrPriorInformation) as TJvPageIndexNode;
  PriorInfoNode.PageIndex := -1;

  NewNode := tvPEST.Items.AddChild(
    PriorInfoNode, StrPriorInformationGr) as TJvPageIndexNode;
  NewNode.PageIndex := jvspPriorInfoObsGroups.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    PriorInfoNode, StrInitialValuePrior) as TJvPageIndexNode;
  NewNode.PageIndex := jvspPriorInfoInitialValue.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    PriorInfoNode, StrWithinLayerContinu) as TJvPageIndexNode;
  NewNode.PageIndex := jvspPriorInfoHorizContinuity.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    PriorInfoNode, StrBetweenLayerContin) as TJvPageIndexNode;
  NewNode.PageIndex := jvspPriorInfoVertContinuity.PageIndex;

  PredictionNode := tvPEST.Items.AddChild(
    nil, StrPredictionAnalysis) as TJvPageIndexNode;
  PredictionNode.PageIndex := -1;

  NewNode := tvPEST.Items.AddChild(
    PredictionNode, StrPredictionAnalysisC) as TJvPageIndexNode;
  NewNode.PageIndex := jvspPrediction1.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    PredictionNode, StrPredictionAnalysisT) as TJvPageIndexNode;
  NewNode.PageIndex := jvspPrediction2.PageIndex;

  RegularizationNode := tvPEST.Items.AddChild(
    nil, StrRegularization) as TJvPageIndexNode;
  RegularizationNode.PageIndex := -1;

  NewNode := tvPEST.Items.AddChild(
    RegularizationNode, StrRegularizationContr) as TJvPageIndexNode;
  NewNode.PageIndex := jvspRegularisation.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    RegularizationNode, StrRegularizationOptio) as TJvPageIndexNode;
  NewNode.PageIndex := jvspRegularizationOption.PageIndex;

  ParetoNode := tvPEST.Items.AddChild(
    nil, StrPareto) as TJvPageIndexNode;
  ParetoNode.PageIndex := -1;

  NewNode := tvPEST.Items.AddChild(
    ParetoNode, StrParetoControls) as TJvPageIndexNode;
  NewNode.PageIndex := jvspPareto1.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    ParetoNode, StrParetoAlternateTer) as TJvPageIndexNode;
  NewNode.PageIndex := jsvpPareto2.PageIndex;

  plMain.ActivePageIndex := 0;

  framePilotPoints.Grid.Cells[Ord(ppcX), 0] := StrX;
  framePilotPoints.Grid.Cells[Ord(ppcY), 0] := StrY;

  rdgBetweenObs.Cells[Ord(ppcX), 0] := StrX;
  rdgBetweenObs.Cells[Ord(ppcY), 0] := StrY;

  rdgPriorInfoInitialValue.Cells[Ord(ppcName), 0] := StrParameterName;
  rdgPriorInfoInitialValue.Cells[Ord(ppcRegularization), 0] := StrDefinePriorInforma;
  rdgPriorInfoInitialValue.Cells[Ord(ppcGroupName), 0] := StrObservationGroupNa;
  rdgPriorInfoInitialValue.Cells[Ord(ppcWeight), 0] := StrWeight;

  rdgPriorInfoHorizContinuity.Cells[Ord(ppcName), 0] := StrParameterName;
  rdgPriorInfoHorizContinuity.Cells[Ord(ppcRegularization), 0] := StrDefinePriorInforma;
  rdgPriorInfoHorizContinuity.Cells[Ord(ppcGroupName), 0] := StrObservationGroupNa;
  rdgPriorInfoHorizContinuity.Cells[Ord(ppcWeight), 0] := StrWeight;

  rdgPriorInfoVertContinuity.Cells[Ord(ppcName), 0] := StrParameterName;
  rdgPriorInfoVertContinuity.Cells[Ord(ppcRegularization), 0] := StrDefinePriorInforma;
  rdgPriorInfoVertContinuity.Cells[Ord(ppcGroupName), 0] := StrObservationGroupNa;
  rdgPriorInfoVertContinuity.Cells[Ord(ppcWeight), 0] := StrWeight;

  rdgObservationsToReport.Cells[Ord(pcName), 0] := 'Observations';
  rdgObservationsToReport.Cells[Ord(pcReport), 0] := 'Generate report (OBS_REPORT_(N)';
//  frameParetoParameters.Grid.Cells[0,0] := StrObservationsToMoni;
  GetData;
end;

procedure TfrmPEST.FormDestroy(Sender: TObject);
begin
  inherited;
  FNewSteadyParameters.Free;
  FNewHufParameters.Free;
  FNewTransientListParameters.Free;
  
  FGroupNameDictionary.Free;
  FGroupDictionary.Free;
  FLocalPriorInfoObsGroups.Free;
  FLocalObsGroups.Free;

  FHobList.Free;
  FNewHobList.Free;

  FFluxObservationList.Free;
  FNewFluxObservationList.Free;

  FObsList.Free;
  FNewObsList.Free;
end;

procedure TfrmPEST.frameObservationGroupsGridBeforeDrawCell(Sender: TObject;
  ACol, ARow: Integer);
begin
  inherited;
  CheckObsGroupName(frameObservationGroups.Grid, ARow, ACol);
  CheckObsGroupTarget(frameObservationGroups.Grid, ARow, ACol);
end;

procedure TfrmPEST.frameObservationGroupsGridButtonClick(Sender: TObject; ACol,
  ARow: Integer);
begin
  inherited;
  GetCovarianceFileName(frameObservationGroups, ACol, ARow);
end;

procedure TfrmPEST.frameObservationGroupsGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  CanSelectObsGridCell(frameObservationGroups, ARow, ACol, CanSelect);
end;

procedure TfrmPEST.frameObservationGroupsGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  ChangeObservationGroupName(frameObservationGroups.Grid, ARow, ACol, Value);
end;

procedure TfrmPEST.frameObservationGroupsGridStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
begin
  inherited;
  if (ACol = Ord(pogcRegularization)) and frameObservationGroups.Grid.Checked[ACol, ARow] then
  begin
    frameObservationGroups.Grid.Checked[Ord(pogcUseTarget), ARow] := False
  end;
  frameObservationGroups.Grid.Invalidate;
end;

procedure TfrmPEST.frameObservationGroupssbDeleteClick(Sender: TObject);
begin
  inherited;
  ObsGroupDeleteButtonClick(frameObservationGroups, Sender);
end;

procedure TfrmPEST.frameObservationGroupssbInsertClick(Sender: TObject);
begin
  inherited;
  InsertObsGroup(frameObservationGroups, Sender);

end;

procedure TfrmPEST.frameObservationGroupsseNumberChange(Sender: TObject);
begin
  inherited;
  ChangeObsGroupNumber(frameObservationGroups, FLocalObsGroups);
end;

procedure TfrmPEST.framePriorInfoObservationGroupsGridBeforeDrawCell(
  Sender: TObject; ACol, ARow: Integer);
begin
  inherited;
  CheckObsGroupName(framePriorInfoObservationGroups.Grid, ARow, ACol);
end;

procedure TfrmPEST.framePriorInfoObservationGroupsGridButtonClick(
  Sender: TObject; ACol, ARow: Integer);
begin
  inherited;
  GetCovarianceFileName(framePriorInfoObservationGroups, ACol, ARow);
end;

procedure TfrmPEST.framePriorInfoObservationGroupsGridExit(Sender: TObject);
var
  PickList: TStringList;
  procedure AssignPickList(Grid: TRbwDataGrid4);
  var
    RowIndex: Integer;
    ObsGroup: TPestObservationGroup;
    ObsIndex: Integer;
  begin
    Grid.Columns[Ord(ppcGroupName)].PickList := PickList;
    for RowIndex := 1 to Grid.RowCount - 1 do
    begin
      ObsGroup := Grid.Objects[Ord(ppcGroupName), RowIndex] as TPestObservationGroup;
      if (ObsGroup <> nil) then
      begin
        ObsIndex := PickList.IndexOfObject(ObsGroup);
        if (ObsIndex >= 0)
          and (PickList[ObsIndex] <> Grid.Cells[Ord(ppcGroupName), RowIndex]) then
        begin
          Grid.Cells[Ord(ppcGroupName), RowIndex] := PickList[ObsIndex];
          Assert(Assigned(Grid.OnSetEditText));
          Grid.OnSetEditText(Grid, Ord(ppcGroupName), RowIndex, PickList[ObsIndex]);
        end;
      end;
    end;
    Grid.Invalidate;
  end;
begin
  inherited;
  PickList := TStringList.Create;
  try
    PickList.AddStrings(framePriorInfoObservationGroups.Grid.Cols[Ord(pogcName)]);
    PickList.Delete(0);
    while PickList.Count > framePriorInfoObservationGroups.seNumber.AsInteger do
    begin
      PickList.Delete(PickList.Count-1);
    end;
    AssignPickList(rdgPriorInfoInitialValue);
    AssignPickList(rdgPriorInfoHorizContinuity);
    AssignPickList(rdgPriorInfoVertContinuity);
  finally
    PickList.Free;
  end;

end;

procedure TfrmPEST.framePriorInfoObservationGroupsGridSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  CanSelectObsGridCell(framePriorInfoObservationGroups, ARow, ACol, CanSelect);
end;

procedure TfrmPEST.framePriorInfoObservationGroupsGridSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
var
  NewValue: string;
  Obs: TPestObservationGroup;
begin
  inherited;
  if (Ord(pogcName) = ACol) and (ARow > 0) then
  begin
    NewValue := ValidObsGroupName(Value);
    if NewValue <> Value then
    begin
      framePriorInfoObservationGroups.Grid.Cells[ACol, ARow] := NewValue;
    end;
    Obs := framePriorInfoObservationGroups.Grid.Objects[ACol, ARow] as TPestObservationGroup;
    if Obs <> nil then
    begin
      Obs.ObsGroupName := NewValue;
    end;
  end;
end;

procedure TfrmPEST.framePriorInfoObservationGroupsGridStateChange(
  Sender: TObject; ACol, ARow: Integer; const Value: TCheckBoxState);
begin
  inherited;
  framePriorInfoObservationGroups.Grid.Invalidate;
end;

procedure TfrmPEST.GetData;
var
  PestProperties: TPestProperties;
  PestControlData: TPestControlData;
  SvdProperties: TSingularValueDecompositionProperties;
  LsqrProperties: TLsqrProperties;
  ItemIndex: Integer;
  ObsGroup: TPestObservationGroup;
  index: Integer;
  AnObs: TCustomObservationItem;
  ATempObs: TCustomObservationItem;
  Tree: TTreeView;
  NewNode: TTreeNode;
  GroupIndex: Integer;
  TreeNode: TTreeNode;
  Locations: TProgramLocations;
  FluxObs: TFluxObservationGroup;
  ATempFluxObs: TFluxObservationGroup;
  HobItem: THobItem;
  FNewHobItem: THobItem;
  PointItem: TPointItem;
  Regularization: TPestRegularization;
  EditedObsGroups: TPestObservationGroups;
  TreeFrame: TframeParentChild;
  UsedTypes: TParameterTypes;
  ParamList: TList<TModflowParameter>;
  ArrayParamList: TList<TModflowSteadyParameter>;
  PIndex: Integer;
  AParam: TModflowParameter;
  ASteadyParam: TModflowSteadyParameter;
  PickList: TStrings;
  ObsIndex: Integer;
  PredictionProperties: TPredictionProperties;
  ParetoProperties: TParetoProperties;
  ObsGroupIndex: Integer;
  ObsGroupNames: TStringList;
  procedure SetPriorInfoObsGroupPicklist(Grid: TRbwDataGrid4);
  var
    PickList: TStrings;
    ObsGroupIndex: Integer;
    ObsGroup: TPestObservationGroup;
  begin
    PickList := Grid.Columns[Ord(ppcGroupName)].PickList;
    PickList.Clear;
    for ObsGroupIndex := 0 to FLocalPriorInfoObsGroups.Count - 1 do
    begin
      ObsGroup := FLocalPriorInfoObsGroups[ObsGroupIndex];
      PickList.AddObject(ObsGroup.ObsGroupName, ObsGroup);
    end;
  end;
begin
  Locations := frmGoPhast.PhastModel.ProgramLocations;

  PestProperties := frmGoPhast.PhastModel.PestProperties;

  {$REGION 'PEST Basic'}
//  cbPEST.Checked := PestProperties.PestUsed;
  comboPestStatus.ItemIndex := Ord(PestProperties.PestStatus);
  comboTemplateCharacter.ItemIndex :=
    comboTemplateCharacter.Items.IndexOf(PestProperties.TemplateCharacter);

  comboFormulaMarker.ItemIndex :=
    comboFormulaMarker.Items.IndexOf(PestProperties.ExtendedTemplateCharacter);

  comboArrayMarker.ItemIndex :=
    comboArrayMarker.Items.IndexOf(PestProperties.ArrayTemplateCharacter);

  diredPest.Text := Locations.PestDirectory;
  CheckPestDirectory;
  {$ENDREGION}

  {$REGION 'Pilot Points'}
  cbShowPilotPoints.Checked := PestProperties.ShowPilotPoints;
  rdePilotPointBuffer.RealValue := PestProperties.PilotPointBuffer;
  comboArrayPattern.ItemIndex := Ord(PestProperties.ArrayPilotPointSelection);
  rdePilotPointSpacing.RealValue := PestProperties.PilotPointSpacing;
  ClearGrid(framePilotPoints.Grid);
  framePilotPoints.seNumber.AsInteger := PestProperties.SpecifiedPilotPoints.Count;
  framePilotPoints.Grid.BeginUpdate;
  try
    for ItemIndex := 0 to PestProperties.SpecifiedPilotPoints.Count - 1 do
    begin
      PointItem := PestProperties.SpecifiedPilotPoints.Items[ItemIndex] as TPointItem;
      framePilotPoints.Grid.RealValue[Ord(ppcX), ItemIndex+1] := PointItem.X;
      framePilotPoints.Grid.RealValue[Ord(ppcy), ItemIndex+1] := PointItem.Y;
    end;
  finally
    framePilotPoints.Grid.EndUpdate;
  end;
  framePilotPoints.seNumber.AsInteger := PestProperties.SpecifiedPilotPoints.Count;

  cbUseBetweenObs.Checked := PestProperties.UseBetweenObservationsPilotPoints;
  rdgBetweenObs.BeginUpdate;
  try
    rdgBetweenObs.RowCount :=
      PestProperties.BetweenObservationsPilotPoints.Count + 1;
    for ItemIndex := 0 to PestProperties.BetweenObservationsPilotPoints.Count - 1 do
    begin
      PointItem := PestProperties.BetweenObservationsPilotPoints.Items[ItemIndex] as TPointItem;
      rdgBetweenObs.RealValue[Ord(ppcX), ItemIndex+1] := PointItem.X;
      rdgBetweenObs.RealValue[Ord(ppcy), ItemIndex+1] := PointItem.Y;
    end;
  finally
    rdgBetweenObs.EndUpdate;
  end;
  {$ENDREGION}

  {$REGION 'Control Data'}
  PestControlData := PestProperties.PestControlData;
  cbSaveRestart.Checked := Boolean(PestControlData.PestRestart);
  comboPestMode.ItemIndex := Ord(PestControlData.PestMode);
  {$ENDREGION}

  {$REGION 'Dimensions'}
  rdeMaxCompDim.IntegerValue := PestControlData.MaxCompressionDimension;
  rdeZeroLimit.RealValue  := PestControlData.ZeroLimit;
  {$ENDREGION}

  {$REGION 'Inversion Controls'}
  rdeInitialLambda.RealValue  := PestControlData.InitalLambda;
  rdeLambdaAdj.RealValue  := PestControlData.LambdaAdjustmentFactor;
  rdeIterationClosure.RealValue  := PestControlData.PhiRatioSufficient;
  rdeLambdaTermination.RealValue  := PestControlData.PhiReductionLambda;
  rdeMaxLambdas.IntegerValue := PestControlData.NumberOfLambdas;
  rdeJacobianUpdate.IntegerValue := PestControlData.JacobianUpdate;
  cbLamForgive.Checked := Boolean(PestControlData.LambdaForgive);
  cbDerForgive.Checked := Boolean(PestControlData.DerivedForgive);
  {$ENDREGION}

  {$REGION 'Parameter Adjustment Controls'}
  rdeMaxRelParamChange.RealValue  := PestControlData.RelativeMaxParamChange;
  rdeMaxFacParamChange.RealValue  := PestControlData.FactorMaxParamChange;
  rdeFactorOriginal.RealValue  := PestControlData.FactorOriginal;
  rdeBoundStick.IntegerValue  := PestControlData.BoundStick;
  cbParameterBending.Checked := Boolean(PestControlData.UpgradeParamVectorBending);
  {$ENDREGION}

  {$REGION 'Inversion Controls 2'}
  rdeSwitchCriterion.RealValue := PestControlData.SwitchCriterion;
  rdeSwitchCriterionChange(nil);
  rdeSwitchCount.IntegerValue := PestControlData.OptSwitchCount;
  rdeSplitSlopeCriterion.RealValue := PestControlData.SplitSlopeCriterion;
  comboAutomaticUserIntervation.ItemIndex := Ord(PestControlData.AutomaticUserIntervation);
  cbSensitivityReuse.Checked := Boolean(PestControlData.SensitivityReuse);
  cbBoundsScaling.Checked := Boolean(PestControlData.Boundscaling);
  {$ENDREGION}

  {$REGION 'Iteration Controls'}
  rdeMaxIterations.IntegerValue := PestControlData.MaxIterations;
  rdePhiReductionCriterion.RealValue := PestControlData.SlowConvergenceCriterion;
  rdePhiReductionCount.IntegerValue := PestControlData.SlowConvergenceCountCriterion;
  rdeNoReductionCount.IntegerValue := PestControlData.ConvergenceCountCriterion;
  rdeSmallParameterReduction.RealValue := PestControlData.ParameterChangeConvergenceCriterion;
  rdeSmallParameterReductionCount.IntegerValue := PestControlData.ParameterChangeConvergenceCount;
  rdePhiStoppingThreshold.RealValue := PestControlData.ObjectiveCriterion;
  cbLastRun.Checked := Boolean(PestControlData.MakeFinalRun);
  rdeAbandon.RealValue := PestControlData.PhiAbandon;
  {$ENDREGION}

  {$REGION 'Output Controls'}
  cbWriteCov.Checked := Boolean(PestControlData.WriteCovariance);
  cbWriteCorrCoef.Checked := Boolean(PestControlData.WriteCorrelations);
  cbWriteEigenvectors.Checked := Boolean(PestControlData.WriteEigenVectors);
  cbWriteResolution.Checked := Boolean(PestControlData.SaveResolution);
  cbWriteJacobian.Checked := Boolean(PestControlData.SaveJacobian);
  cbWriteJacobianEveryIteration.Checked := Boolean(PestControlData.SaveJacobianIteration);
  cbWriteVerboseRunRecord.Checked := Boolean(PestControlData.VerboseRecord);
  cbWriteIntermResidualForEveryIteration.Checked := Boolean(PestControlData.SaveInterimResiduals);
  cbSaveParamValuesIteration.Checked := Boolean(PestControlData.SaveParamIteration);
  cbSaveParamValuesModelRun.Checked := Boolean(PestControlData.SaveParamRun);
  {$ENDREGION}

  {$REGION 'Singular Value Decomposition'}
  SvdProperties := PestProperties.SvdProperties;
  comboSvdMode.ItemIndex := Ord(SvdProperties.Mode);
  rdeMaxSingularValues.IntegerValue := SvdProperties.MaxSingularValues;
  rdeEigenThreshold.RealValue := SvdProperties.EigenThreshold;
  comboEigenWrite.ItemIndex := Ord(SvdProperties.EigenWrite);
  {$ENDREGION}

  {$REGION 'LQSR'}
  LsqrProperties := PestProperties.LsqrProperties;
  cbUseLqsr.Checked := Boolean(LsqrProperties.Mode);
  rdeMatrixTolerance.RealValue := LsqrProperties.MatrixTolerance;
  rdeRightHandSideTolerance.RealValue := LsqrProperties.RightHandSideTolerance;
  rdeConditionNumberLimit.RealValue := LsqrProperties.ConditionNumberLimit;
  rdeMaxLqsrIterations.IntegerValue := LsqrProperties.MaxIteration;
  cbWriteLsqrOutput.Checked := Boolean(LsqrProperties.LsqrWrite);
  {$ENDREGION}

  {$REGION 'Observation Groups'}
  GetObsGroups(frameObservationGroups, PestProperties.ObservationGroups,
    FLocalObsGroups);
  GetObsGroups(framePriorInfoObservationGroups,
    PestProperties.PriorInfoObservationGroups, FLocalPriorInfoObsGroups);
  {$ENDREGION}

  {$REGION 'Observation Group Assignments'}
  FGroupDictionary.Clear;
  FGroupNameDictionary.Clear;
  EditedObsGroups := FLocalObsGroups;

  TreeFrame := frameParentObsGroups;
  Tree := TreeFrame.tvTree;
  Tree.Items.Clear;
  FNoNameNode := Tree.Items.AddChild(nil, StrNone);
  for GroupIndex := 0 to EditedObsGroups.Count - 1 do
  begin
    ObsGroup := EditedObsGroups[GroupIndex];
    HandleAddedGroup(TreeFrame, ObsGroup);
  end;

  frmGoPhast.PhastModel.FillObsItemList(FObsList, True);
  FNewObsList.Capacity := FObsList.Count;
  for index := 0 to FObsList.Count - 1 do
  begin
    AnObs := FObsList[index];
    ATempObs := TCustomObservationItem.Create(nil);
    FNewObsList.Add(ATempObs);
    ATempObs.Assign(AnObs);
    if FGroupNameDictionary.TryGetValue(UpperCase(ATempObs.ObservationGroup), ObsGroup) then
    begin
      if FGroupDictionary.TryGetValue(ObsGroup, TreeNode) then
      begin
        NewNode := Tree.Items.AddChild(TreeNode, ATempObs.Name);
      end
      else
      begin
        NewNode := Tree.Items.AddChild(FNoNameNode, ATempObs.Name);
      end;
    end
    else
    begin
      NewNode := Tree.Items.AddChild(FNoNameNode, ATempObs.Name);
    end;
    NewNode.Data := ATempObs;
  end;

  frmGoPhast.PhastModel.FillFluxObsList(FFluxObservationList);
  FNewFluxObservationList.Capacity := FFluxObservationList.Count;
  for index := 0 to FFluxObservationList.Count - 1 do
  begin
    FluxObs := FFluxObservationList[index];
    ATempFluxObs := TFluxObservationGroup.Create(nil);
    FNewFluxObservationList.Add(ATempFluxObs);
    ATempFluxObs.Assign(FluxObs);
    if FGroupNameDictionary.TryGetValue(UpperCase(ATempFluxObs.ObservationGroup), ObsGroup) then
    begin
      if FGroupDictionary.TryGetValue(ObsGroup, TreeNode) then
      begin
        NewNode := Tree.Items.AddChild(TreeNode, ATempFluxObs.ObservationName);
      end
      else
      begin
        NewNode := Tree.Items.AddChild(FNoNameNode, ATempFluxObs.ObservationName);
      end;
    end
    else
    begin
      NewNode := Tree.Items.AddChild(FNoNameNode, ATempFluxObs.ObservationName);
    end;
    NewNode.Data := ATempFluxObs;
  end;

  frmGoPhast.PhastModel.FillHobList(FHobList);
  FNewHobList.Capacity := FHobList.Count;
  for index := 0 to FHobList.Count - 1 do
  begin
    HobItem := FHobList[index];
    FNewHobItem := THobItem.Create(nil);
    FNewHobList.Add(FNewHobItem);
    FNewHobItem.Assign(HobItem);
    if FGroupNameDictionary.TryGetValue(UpperCase(FNewHobItem.ObservationGroup), ObsGroup) then
    begin
      if FGroupDictionary.TryGetValue(ObsGroup, TreeNode) then
      begin
        NewNode := Tree.Items.AddChild(TreeNode, FNewHobItem.Name);
      end
      else
      begin
        NewNode := Tree.Items.AddChild(FNoNameNode, FNewHobItem.Name);
      end;
    end
    else
    begin
      NewNode := Tree.Items.AddChild(FNoNameNode, FNewHobItem.Name);
    end;
    NewNode.Data := FNewHobItem;
  end;
  {$ENDREGION}

  {$REGION 'Prior Information Observation Groups'}
  SetPriorInfoObsGroupPicklist(rdgPriorInfoInitialValue);
  SetPriorInfoObsGroupPicklist(rdgPriorInfoHorizContinuity);
  SetPriorInfoObsGroupPicklist(rdgPriorInfoVertContinuity);
 {$ENDREGION}

  PickList := rdgPriorInfoInitialValue.Columns[Ord(ppcGroupName)].PickList;

  {$REGION 'Prior Information'}
  cbInitialValue.Checked := PestProperties.UseInitialValuePriorInfo;

  cbPriorInfoHorizContinuity.Checked := PestProperties.UseHorizontalSpatialContinuityPriorInfo;
  rdeSearchDistance.RealValue := PestProperties.SeachDistance;
  seMaxPilotPoints.AsInteger := PestProperties.MaxPilotPointsInRange;

  cbPriorInfoVertContinuity.Checked := PestProperties.UseVertSpatialContinuityPriorInfo;

  GetUsedTypes(UsedTypes);
  FNewSteadyParameters.Assign(frmGoPhast.PhastModel.ModflowSteadyParameters);
  FNewHufParameters.Assign(frmGoPhast.PhastModel.HufParameters);
  FNewTransientListParameters.Assign(frmGoPhast.PhastModel.ModflowTransientParameters);
  ParamList := TList<TModflowParameter>.Create;
  ArrayParamList := TList<TModflowSteadyParameter>.Create;
  try
    for PIndex := 0 to FNewSteadyParameters.Count -1 do
    begin
      ASteadyParam := FNewSteadyParameters[PIndex];
      if (ASteadyParam.ParameterType in UsedTypes)
        and (ASteadyParam.Transform in [ptNoTransform, ptLog]) then
      begin
        ParamList.Add(ASteadyParam);
        if ASteadyParam.UsePilotPoints then
        begin
          ArrayParamList.Add(ASteadyParam);
        end;
      end;
    end;
    for PIndex := 0 to FNewHufParameters.Count -1 do
    begin
      AParam := FNewHufParameters[PIndex];
      if (AParam.ParameterType in UsedTypes)
        and (AParam.Transform in [ptNoTransform, ptLog]) then
      begin
        ParamList.Add(AParam);
      end;
    end;
    for PIndex := 0 to FNewTransientListParameters.Count -1 do
    begin
      AParam := FNewTransientListParameters[PIndex];
      if (AParam.ParameterType in UsedTypes)
        and (AParam.Transform in [ptNoTransform, ptLog]) then
      begin
        ParamList.Add(AParam);
      end;
    end;
    //TPriorParmCol = (ppcName, ppcRegularization, ppcGroupName);
    rdgPriorInfoInitialValue.RowCount := Max(2, ParamList.Count+1);
    for PIndex := 0 to ParamList.Count -1 do
    begin
      AParam := ParamList[PIndex];
      rdgPriorInfoInitialValue.Cells[Ord(ppcName), PIndex+1] := AParam.ParameterName;
      rdgPriorInfoInitialValue.Objects[Ord(ppcName), PIndex+1] := AParam;
      rdgPriorInfoInitialValue.Checked[Ord(ppcRegularization), PIndex+1] := AParam.UseInitialValuePriorInfo;
      rdgPriorInfoInitialValue.Cells[Ord(ppcGroupName), PIndex+1] := AParam.RegularizationGroup;
      rdgPriorInfoInitialValue.RealValue[Ord(ppcWeight), PIndex+1] := AParam.InitialValuePriorInfoWeight;
      ObsIndex := PickList.IndexOf(AParam.RegularizationGroup);
      if ObsIndex >= 0 then
      begin
        rdgPriorInfoInitialValue.Objects[Ord(ppcGroupName), PIndex+1] :=
          PickList.Objects[ObsIndex];
      end;
    end;

    rdgPriorInfoHorizContinuity.RowCount := Max(2, ArrayParamList.Count+1);
    for PIndex := 0 to ArrayParamList.Count -1 do
    begin
      ASteadyParam := ArrayParamList[PIndex];
      rdgPriorInfoHorizContinuity.Cells[Ord(ppcName), PIndex+1] := ASteadyParam.ParameterName;
      rdgPriorInfoHorizContinuity.Objects[Ord(ppcName), PIndex+1] := ASteadyParam;
      rdgPriorInfoHorizContinuity.Checked[Ord(ppcRegularization), PIndex+1] := ASteadyParam.UseHorizontalSpatialContinuityPriorInfo;
      rdgPriorInfoHorizContinuity.Cells[Ord(ppcGroupName), PIndex+1] := ASteadyParam.HorizontalSpatialContinuityGroupName;
      rdgPriorInfoHorizContinuity.RealValue[Ord(ppcWeight), PIndex+1] := ASteadyParam.HorizontalSpatialContinuityPriorInfoWeight;
      ObsIndex := PickList.IndexOf(ASteadyParam.HorizontalSpatialContinuityGroupName);
      if ObsIndex >= 0 then
      begin
        rdgPriorInfoHorizContinuity.Objects[Ord(ppcGroupName), PIndex+1] :=
          PickList.Objects[ObsIndex];
      end;
    end;

    rdgPriorInfoVertContinuity.RowCount := Max(2, ArrayParamList.Count+1);
    for PIndex := 0 to ArrayParamList.Count -1 do
    begin
      ASteadyParam := ArrayParamList[PIndex];
      rdgPriorInfoVertContinuity.Cells[Ord(ppcName), PIndex+1] := ASteadyParam.ParameterName;
      rdgPriorInfoVertContinuity.Objects[Ord(ppcName), PIndex+1] := ASteadyParam;
      rdgPriorInfoVertContinuity.Checked[Ord(ppcRegularization), PIndex+1] := ASteadyParam.UseVertSpatialContinuityPriorInfo;
      rdgPriorInfoVertContinuity.Cells[Ord(ppcGroupName), PIndex+1] := ASteadyParam.VertSpatialContinuityGroupName;
      rdgPriorInfoVertContinuity.RealValue[Ord(ppcWeight), PIndex+1] := ASteadyParam.VertSpatialContinuityPriorInfoWeight;
      ObsIndex := PickList.IndexOf(ASteadyParam.VertSpatialContinuityGroupName);
      if ObsIndex >= 0 then
      begin
        rdgPriorInfoVertContinuity.Objects[Ord(ppcGroupName), PIndex+1] :=
          PickList.Objects[ObsIndex];
      end;
    end;
  finally
    ParamList.Free;
    ArrayParamList.Free;
  end;
  {$ENDREGION}

  {$REGION 'Regularization'}
    Regularization := PestProperties.Regularization;
    rdePhimLim.RealValue := Regularization.PhiMLim;
    cbAutomaticallySetPHIMACCEPT.Checked := Regularization.AutoPhiMAccept;
    rdePhimAccept.RealValue := Regularization.PhiMAccept;
    rdeFRACPHIM.RealValue := Regularization.FracPhiM;
    cbMemSave.Checked := Boolean(Ord(Regularization.MemSave));
    rdeWFINIT.RealValue := Regularization.WFINIT;
    rdeWFMIN.RealValue := Regularization.WeightFactorMinimum;
    rdeWFMAX.RealValue := Regularization.WeightFactorMaximum;
    rdeWFFAC.RealValue := Regularization.WFFAC;
    rdeWFTOL.RealValue := Regularization.WeightFactorTolerance;
    cbLINREG.Checked := Boolean(Ord(Regularization.LinearRegression));
    cbREGCONTINUE.Checked := Boolean(Ord(Regularization.RegContinue));
    IREGADJ := Regularization.RegularizationOption;
    seNOPTREGADJ.AsInteger := Regularization.OptimizationInterval;
    rdeREGWEIGHTRAT.RealValue := Regularization.RegWeightRatio;
    rdeREGSINGTHRESH.RealValue := Regularization.RegularizationSingularValueThreshhold;
  {$ENDREGION}

  {$REGION 'Predictive analysis'}
  PredictionProperties := PestProperties.PredictionProperties;
  comboPredMinMax.ItemIndex := Ord(PredictionProperties.MinOrMax);
  cbPredictiveNoise.Checked := PredictionProperties.PredictiveNoise = pnUseNoise;
  rdeTargetObjectiveFunction.RealValue := PredictionProperties.TargetPhi;
  rdeAcceptedObjectiveFunction.RealValue := PredictionProperties.AcceptedPhi;
  rdeTestLambdaPhi.RealValue := PredictionProperties.TestLambdaPhi;
  rdeAbsoluteLamdaCriterion.RealValue := PredictionProperties.AbsoluteLamdaCriterion;
  rdeRelativeLamdaCriterion.RealValue := PredictionProperties.RelativeLamdaCriterion;
  rdeInitialLineSearchFactor.RealValue := PredictionProperties.InitialLineSearchFactor;
  rdeUpdateLineSearchFactor.RealValue := PredictionProperties.UpdateLineSearchFactor;
  seLineSearchRuns.AsInteger := PredictionProperties.LineSearchRuns;
  rdeAbsolutePredictionSwitch.RealValue := PredictionProperties.AbsolutePredictionSwitch;
  rdeRelativePredictionSwitch.RealValue := PredictionProperties.RelativePredictionSwitch;
  seMaxNoPredictionImprovmentRuns.AsInteger := PredictionProperties.MaxNoPredictionImprovmentRuns;
  rdeAbsoluteImprovementCriterion.RealValue := PredictionProperties.AbsoluteImprovementCriterion;
  rdeRelativeImprovementCriterion.RealValue := PredictionProperties.RelativeImprovementCriterion;
  seNumberOfPredictionsToCompare.AsInteger := PredictionProperties.NumberOfPredictionsToCompare;
  {$ENDREGION}

  {$REGION 'Pareto'}
  ParetoProperties := PestProperties.ParetoProperties;
  ObsGroupNames := TStringList.Create;
  try
    ObsGroupNames.CaseSensitive := False;
    for ObsGroupIndex := 0 to FLocalObsGroups.Count - 1 do
    begin
      ObsGroup := FLocalObsGroups[ObsGroupIndex];
      ObsGroupNames.AddObject(ObsGroup.ObsGroupName, ObsGroup);
    end;
    comboParetoGroup.Items.Assign(ObsGroupNames);
    comboParetoGroup.ItemIndex :=
      ObsGroupNames.IndexOf(ParetoProperties.ParetoGroupName);
    AssignParetoObsReports(ParetoProperties);
  finally
    ObsGroupNames.Free;
  end;
  rdeInitialParetoWeight.RealValue := ParetoProperties.InitialParetoWeight;
  rdeFinalParetoWeight.RealValue := ParetoProperties.FinalParetoWeight;
  seParetoIncrements.AsInteger := ParetoProperties.ParetoIncrements;
  seInitialIterationCount.AsInteger := ParetoProperties.InitialIterationCount;
  seIntermediateIterationCount.AsInteger := ParetoProperties.IntermediateIterationCount;
  seFinalIterationCount.AsInteger := ParetoProperties.FinalIterationCount;
  cbAltTerminationOption.Checked := Boolean(ParetoProperties.AltTerminationOption);
  comboAltDirection.ItemIndex := Ord(ParetoProperties.AltDirection);
  rdeAltThreshold.RealValue := ParetoProperties.AltThreshold;
  seAltIterations.AsInteger := ParetoProperties.AltIterations;
  {$ENDREGION}
end;

function TfrmPEST.GetIREGADJ: Integer;
begin
  result := 0;
  case rgRegOption.ItemIndex of
    0:
      begin
        result := 0;
      end;
    1:
      begin
        case rgGroupWeightMethod.ItemIndex of
          0:
            begin
              if cbRegApplyGroupWeight.Checked then
              begin
                result := 3;
              end
              else
              begin
                result := 1;
              end;
            end;
          1:
            begin
              result := 2;
            end;
          else
            Assert(False);
        end;
      end;
    2:
      begin
        case rgIndividualAdjustmentMethod.ItemIndex of
          0:
            begin
              result := 4;
            end;
          1:
            begin
              result := 5;
            end;
          else
            Assert(False);
        end;
      end;
    else
      Assert(False);
  end;
end;

procedure TfrmPEST.SetData;
var
  PestProperties: TPestProperties;
  InvalidateModelEvent: TNotifyEvent;
  PestControlData: TPestControlData;
  SvdProperties: TSingularValueDecompositionProperties;
  LsqrProperties: TLsqrProperties;
  Grid: TRbwDataGrid4;
  ANode: TTreeNode;
  ObsGroup: TPestObservationGroup;
  ChildNode: TTreeNode;
  AnObs: TCustomObservationItem;
  AnObject: TObject;
  FluxObs: TFluxObservationGroup;
  HobItem: THobItem;
  PointItem: TPointItem;
  ItemIndex: Integer;
  Regularization: TPestRegularization;
  PredProp: TPredictionProperties;
  ParetoProperties: TParetoProperties;
  ReportedObs: TStringList;
  RowIndex: Integer;
  AName: string;
begin
  InvalidateModelEvent := nil;
  PestProperties := TPestProperties.Create(nil);
  try

    {$REGION 'PEST Basics'}
//    PestProperties.PestUsed := cbPEST.Checked;
    PestProperties.PestStatus := TPestStatus(comboPestStatus.ItemIndex);
    if comboTemplateCharacter.Text <> '' then
    begin
      PestProperties.TemplateCharacter := comboTemplateCharacter.Text[1];
    end;
    if comboFormulaMarker.Text <> '' then
    begin
      PestProperties.ExtendedTemplateCharacter := comboFormulaMarker.Text[1];
    end;
    if comboArrayMarker.Text <> '' then
    begin
      PestProperties.ArrayTemplateCharacter := comboArrayMarker.Text[1];
    end;
    {$ENDREGION}

    {$REGION 'Pilot Points'}
    PestProperties.ShowPilotPoints := cbShowPilotPoints.Checked;
    PestProperties.PilotPointBuffer := rdePilotPointBuffer.RealValue;
    PestProperties.ArrayPilotPointSelection :=
      TArrayPilotPointSelection(comboArrayPattern.ItemIndex);
    PestProperties.PilotPointSpacing := rdePilotPointSpacing.RealValue;

    PestProperties.SpecifiedPilotPoints.Capacity :=
      framePilotPoints.seNumber.AsInteger;
    Grid := framePilotPoints.Grid;
    for ItemIndex := 0 to framePilotPoints.seNumber.AsInteger - 1 do
    begin
      if (Grid.Cells[Ord(ppcX), ItemIndex+1] <> '')
        and (Grid.Cells[Ord(ppcY), ItemIndex+1] <> '') then
      begin
        PointItem := PestProperties.SpecifiedPilotPoints.Add as TPointItem;
        PointItem.X := Grid.RealValue[Ord(ppcX), ItemIndex+1];
        PointItem.Y := Grid.RealValue[Ord(ppcY), ItemIndex+1];
      end;
    end;

    PestProperties.UseBetweenObservationsPilotPoints := cbUseBetweenObs.Checked;
    rdgBetweenObs.BeginUpdate;
    PestProperties.BetweenObservationsPilotPoints.Capacity :=
      rdgBetweenObs.RowCount-1;
    for ItemIndex := 1 to rdgBetweenObs.RowCount - 1 do
    begin
      if (rdgBetweenObs.Cells[Ord(ppcX), ItemIndex] <> '')
        and (rdgBetweenObs.Cells[Ord(ppcY), ItemIndex] <> '') then
      begin
        PointItem := PestProperties.BetweenObservationsPilotPoints.Add as TPointItem;
        PointItem.X := rdgBetweenObs.RealValue[Ord(ppcX), ItemIndex];
        PointItem.Y := rdgBetweenObs.RealValue[Ord(ppcY), ItemIndex];
      end;
    end;

    {$ENDREGION}

    {$REGION 'Control Data'}
    PestControlData := PestProperties.PestControlData;
    PestControlData.PestRestart := TPestRestart(cbSaveRestart.Checked);
    PestControlData.PestMode := TPestMode(comboPestMode.ItemIndex);
    {$ENDREGION}

    {$REGION 'Dimensions'}
    if rdeMaxCompDim.Text <> '' then
    begin
      PestControlData.MaxCompressionDimension := rdeMaxCompDim.IntegerValue;
    end;
    if rdeZeroLimit.Text <> '' then
    begin
      PestControlData.ZeroLimit := rdeZeroLimit.RealValue;
    end;
    {$ENDREGION}

    {$REGION 'Inversion Controls'}
    if rdeInitialLambda.Text <> '' then
    begin
      PestControlData.InitalLambda := rdeInitialLambda.RealValue;
    end;
    if rdeLambdaAdj.Text <> '' then
    begin
      PestControlData.LambdaAdjustmentFactor := rdeLambdaAdj.RealValue;
    end;
    if rdeIterationClosure.Text <> '' then
    begin
      PestControlData.PhiRatioSufficient := rdeIterationClosure.RealValue;
    end;
    if rdeLambdaTermination.Text <> '' then
    begin
      PestControlData.PhiReductionLambda := rdeLambdaTermination.RealValue;
    end;
    if rdeMaxLambdas.Text <> '' then
    begin
      PestControlData.NumberOfLambdas := rdeMaxLambdas.IntegerValue;
    end;
    if rdeJacobianUpdate.Text <> '' then
    begin
      PestControlData.JacobianUpdate := rdeJacobianUpdate.IntegerValue;
    end;
   PestControlData.LambdaForgive := TLambdaForgive(cbLamForgive.Checked);
   PestControlData.DerivedForgive := TDerivedForgive(cbDerForgive.Checked);
   {$ENDREGION}

    {$REGION 'Parameter Adjustment Controls'}
    if rdeMaxRelParamChange.Text <> '' then
    begin
      PestControlData.RelativeMaxParamChange :=  rdeMaxRelParamChange.RealValue;
    end;
    if rdeMaxFacParamChange.Text <> '' then
    begin
      PestControlData.FactorMaxParamChange :=  rdeMaxFacParamChange.RealValue;
    end;
    if rdeFactorOriginal.Text <> '' then
    begin
      PestControlData.FactorOriginal :=  rdeFactorOriginal.RealValue;
    end;
    if rdeBoundStick.Text <> '' then
    begin
      PestControlData.BoundStick :=  rdeBoundStick.IntegerValue;
    end;
   PestControlData.UpgradeParamVectorBending :=
     TUpgradeParamVectorBending(cbLamForgive.Checked);
   {$ENDREGION}

    {$REGION 'Inversion Controls 2'}
    if rdeSwitchCriterion.Text <> '' then
    begin
      PestControlData.SwitchCriterion := rdeSwitchCriterion.RealValue;
    end;
    if rdeSwitchCount.Text <> '' then
    begin
      PestControlData.OptSwitchCount := rdeSwitchCount.IntegerValue;
    end;
    if rdeSplitSlopeCriterion.Text <> '' then
    begin
      PestControlData.SplitSlopeCriterion := rdeSplitSlopeCriterion.RealValue;
    end;
    PestControlData.AutomaticUserIntervation := TAutomaticUserIntervation(comboAutomaticUserIntervation.ItemIndex);
    PestControlData.SensitivityReuse := TSensitivityReuse(cbSensitivityReuse.Checked);
    PestControlData.Boundscaling := TBoundsScaling(cbBoundsScaling.Checked);
    {$ENDREGION}

    {$REGION 'Iteration Controls'}
    if rdeMaxIterations.Text <> '' then
    begin
      PestControlData.MaxIterations := rdeMaxIterations.IntegerValue;
    end;
    if rdePhiReductionCriterion.Text <> '' then
    begin
      PestControlData.SlowConvergenceCriterion := rdePhiReductionCriterion.RealValue;
    end;
    if rdePhiReductionCount.Text <> '' then
    begin
      PestControlData.SlowConvergenceCountCriterion := rdePhiReductionCount.IntegerValue;
    end;
    if rdeNoReductionCount.Text <> '' then
    begin
      PestControlData.ConvergenceCountCriterion := rdeNoReductionCount.IntegerValue;
    end;
    if rdeSmallParameterReduction.Text <> '' then
    begin
      PestControlData.ParameterChangeConvergenceCriterion := rdeSmallParameterReduction.RealValue;
    end;
    if rdeSmallParameterReductionCount.Text <> '' then
    begin
      PestControlData.ParameterChangeConvergenceCount := rdeSmallParameterReductionCount.IntegerValue;
    end;
    if rdePhiStoppingThreshold.Text <> '' then
    begin
      PestControlData.ObjectiveCriterion := rdePhiStoppingThreshold.RealValue;
    end;
    PestControlData.MakeFinalRun := TMakeFinalRun(cbLastRun.Checked);
    if rdeAbandon.Text <> '' then
    begin
      PestControlData.PhiAbandon := rdeAbandon.RealValue;
    end;
    {$ENDREGION}

    {$REGION 'Output Options'}
    PestControlData.WriteCovariance := TWriteMatrix(cbWriteCov.Checked);
    PestControlData.WriteCorrelations := TWriteMatrix(cbWriteCorrCoef.Checked);
    PestControlData.WriteEigenVectors := TWriteMatrix(cbWriteEigenvectors.Checked);
    PestControlData.SaveResolution := TSaveResolution(cbWriteResolution.Checked);
    PestControlData.SaveJacobian := TSaveJacobian(cbWriteJacobian.Checked);
    PestControlData.SaveJacobianIteration :=
      TSaveJacobianIteration(cbWriteJacobianEveryIteration.Checked);
    PestControlData.VerboseRecord := TVerboseRecord(cbWriteVerboseRunRecord.Checked);
    PestControlData.SaveInterimResiduals :=
      TSaveInterimResiduals(cbWriteIntermResidualForEveryIteration.Checked);
    PestControlData.SaveParamIteration :=
      TSaveParamIteration(cbSaveParamValuesIteration.Checked);
    PestControlData.SaveParamRun :=
      TSaveParamRun(cbSaveParamValuesModelRun.Checked);
      {$ENDREGION}

    {$REGION 'Singular Value Decomposition'}
    SvdProperties := PestProperties.SvdProperties;
    SvdProperties.Mode := TSvdMode(comboSvdMode.ItemIndex);
    if rdeMaxSingularValues.Text <> '' then
    begin
      SvdProperties.MaxSingularValues := rdeMaxSingularValues.IntegerValue;
    end;
    if rdeEigenThreshold.Text <> '' then
    begin
      SvdProperties.EigenThreshold := rdeEigenThreshold.RealValue;
    end;
    SvdProperties.EigenWrite := TEigenWrite(comboEigenWrite.ItemIndex);
    {$ENDREGION}

    {$REGION 'LQSR'}
    LsqrProperties := PestProperties.LsqrProperties;
    LsqrProperties.Mode := TLsqrMode(cbUseLqsr.Checked);
    if rdeMatrixTolerance.Text <> '' then
    begin
      LsqrProperties.MatrixTolerance := rdeMatrixTolerance.RealValue;
    end;
    if rdeRightHandSideTolerance.Text <> '' then
    begin
      LsqrProperties.RightHandSideTolerance := rdeRightHandSideTolerance.RealValue;
    end;
    if rdeConditionNumberLimit.Text <> '' then
    begin
      LsqrProperties.ConditionNumberLimit := rdeConditionNumberLimit.RealValue;
    end;
    if rdeMaxLqsrIterations.Text <> '' then
    begin
      LsqrProperties.MaxIteration := rdeMaxLqsrIterations.IntegerValue;
    end;
    LsqrProperties.LsqrWrite := TLsqrWrite(cbWriteLsqrOutput.Checked);
    {$ENDREGION}

    {$REGION 'Observation Groups'}
//    ObsGroups := PestProperties.ObservationGroups;
//    ObsGroupFrame := frameObservationGroups;
//    EditedObsGroups := FLocalObsGroups;
    SetObsGroups(PestProperties.ObservationGroups, frameObservationGroups,
      FLocalObsGroups);
    SetObsGroups(PestProperties.PriorInfoObservationGroups,
      framePriorInfoObservationGroups, FLocalPriorInfoObsGroups);
    {$ENDREGION}

    {$REGION 'Prior Information'}
    PestProperties.UseInitialValuePriorInfo := cbInitialValue.Checked;
    PestProperties.UseHorizontalSpatialContinuityPriorInfo := cbPriorInfoHorizContinuity.Checked;
    PestProperties.SeachDistance := rdeSearchDistance.RealValue;
    PestProperties.MaxPilotPointsInRange := seMaxPilotPoints.AsInteger;
    PestProperties.UseVertSpatialContinuityPriorInfo := cbPriorInfoVertContinuity.Checked;
    {$ENDREGION}

    {$REGION 'Observation Group Assignments'}
    ANode := FNoNameNode;
    while ANode <> nil do
    begin
      ObsGroup := ANode.Data;
      ChildNode := ANode.getFirstChild;
      while ChildNode <> nil do
      begin
        AnObject := ChildNode.Data;
        if AnObject is TCustomObservationItem then
        begin
          AnObs := TCustomObservationItem(AnObject);
          if ObsGroup = nil then
          begin
            AnObs.ObservationGroup := '';
          end
          else
          begin
            AnObs.ObservationGroup := ObsGroup.ObsGroupName;
          end;
        end
        else if AnObject is TFluxObservationGroup then
        begin
          FluxObs := TFluxObservationGroup(AnObject);
          if ObsGroup = nil then
          begin
            FluxObs.ObservationGroup := '';
          end
          else
          begin
            FluxObs.ObservationGroup := ObsGroup.ObsGroupName;
          end;
        end
        else
        begin
          HobItem := AnObject as THobItem;
          if ObsGroup = nil then
          begin
            HobItem.ObservationGroup := '';
          end
          else
          begin
            HobItem.ObservationGroup := ObsGroup.ObsGroupName;
          end;
        end;
        ChildNode := ChildNode.GetNextSibling;
      end;

      ANode := ANode.GetNextSibling;
    end;
    {$ENDREGION}

    {$REGION 'Regularization'}
      Regularization := PestProperties.Regularization;
      Regularization.PhiMLim := rdePhimLim.RealValue;
      Regularization.AutoPhiMAccept := cbAutomaticallySetPHIMACCEPT.Checked;
      Regularization.PhiMAccept := rdePhimAccept.RealValue;
      Regularization.FracPhiM := rdeFRACPHIM.RealValue;
      Regularization.MemSave := TMemSave(cbMemSave.Checked);
      Regularization.WFINIT := rdeWFINIT.RealValue;
      Regularization.WeightFactorMinimum := rdeWFMIN.RealValue;
      Regularization.WeightFactorMaximum := rdeWFMAX.RealValue;
      Regularization.WFFAC := rdeWFFAC.RealValue;
      Regularization.WeightFactorTolerance := rdeWFTOL.RealValue;
      Regularization.LinearRegression := TLinRegression(cbLINREG.Checked);
      Regularization.RegContinue := TRegContinue(cbREGCONTINUE.Checked);
      Regularization.RegularizationOption := IREGADJ;
      Regularization.OptimizationInterval := seNOPTREGADJ.AsInteger;
      Regularization.RegWeightRatio := rdeREGWEIGHTRAT.RealValue;
      Regularization.RegularizationSingularValueThreshhold := rdeREGSINGTHRESH.RealValue;
    {$ENDREGION}

    {$REGION 'Predictive analysis'}
    PredProp := PestProperties.PredictionProperties;
    PredProp.MinOrMax := TMinOrMax(comboPredMinMax.ItemIndex);
    PredProp.PredictiveNoise :=
      TPredictiveNoise(cbPredictiveNoise.Checked);
    PredProp.TargetPhi :=
      rdeTargetObjectiveFunction.RealValueDefault(PredProp.TargetPhi);
    PredProp.AcceptedPhi :=
      rdeAcceptedObjectiveFunction.RealValueDefault(PredProp.AcceptedPhi);
    PredProp.TestLambdaPhi :=
      rdeTestLambdaPhi.RealValueDefault(PredProp.TestLambdaPhi);
    PredProp.AbsoluteLamdaCriterion :=
      rdeAbsoluteLamdaCriterion.RealValueDefault(PredProp.AbsoluteLamdaCriterion);
    PredProp.RelativeLamdaCriterion :=
      rdeRelativeLamdaCriterion.RealValueDefault(PredProp.RelativeLamdaCriterion);
    PredProp.InitialLineSearchFactor :=
      rdeInitialLineSearchFactor.RealValueDefault(PredProp.InitialLineSearchFactor);
    PredProp.UpdateLineSearchFactor :=
      rdeUpdateLineSearchFactor.RealValueDefault(PredProp.UpdateLineSearchFactor);
    PredProp.LineSearchRuns := seLineSearchRuns.AsInteger;
    PredProp.AbsolutePredictionSwitch :=
      rdeAbsolutePredictionSwitch.RealValueDefault(PredProp.AbsolutePredictionSwitch);
    PredProp.RelativePredictionSwitch :=
      rdeRelativePredictionSwitch.RealValueDefault(PredProp.AbsolutePredictionSwitch);
    PredProp.MaxNoPredictionImprovmentRuns :=
      seMaxNoPredictionImprovmentRuns.AsInteger;
    PredProp.AbsoluteImprovementCriterion :=
      rdeAbsoluteImprovementCriterion.RealValueDefault(PredProp.AbsoluteImprovementCriterion);
    PredProp.RelativeImprovementCriterion :=
      rdeRelativeImprovementCriterion.RealValueDefault(PredProp.RelativeImprovementCriterion);
    PredProp.NumberOfPredictionsToCompare :=
      seNumberOfPredictionsToCompare.AsInteger;
    {$ENDREGION}

    {$REGION 'Pareto'}
    ParetoProperties := PestProperties.ParetoProperties;
    ParetoProperties.ParetoGroupName := comboParetoGroup.Text;
    ParetoProperties.InitialParetoWeight :=
      rdeInitialParetoWeight.RealValueDefault(
      ParetoProperties.InitialParetoWeight);
    ParetoProperties.FinalParetoWeight :=
      rdeFinalParetoWeight.RealValueDefault(
      ParetoProperties.FinalParetoWeight);
    ParetoProperties.ParetoIncrements := seParetoIncrements.AsInteger;
    ParetoProperties.InitialIterationCount :=
      seInitialIterationCount.AsInteger;
    ParetoProperties.IntermediateIterationCount :=
      seIntermediateIterationCount.AsInteger;
    ParetoProperties.FinalIterationCount := seFinalIterationCount.AsInteger;
    ParetoProperties.AltTerminationOption :=
      TAltTerminationOption(cbAltTerminationOption.Checked);
    ParetoProperties.ObservationName := comboObservationName.Text;
    ParetoProperties.AltDirection := TAltDirection(comboAltDirection.ItemIndex);
    ParetoProperties.AltThreshold  :=
      rdeAltThreshold.RealValueDefault(
      ParetoProperties.AltThreshold);
    ParetoProperties.AltIterations := seAltIterations.AsInteger;
    ReportedObs := TStringList.Create;
    try
      for RowIndex := 1 to rdgObservationsToReport.RowCount - 1 do
      begin
        AName := rdgObservationsToReport.Cells[Ord(pcName), RowIndex];
        if (AName <> '')
          and rdgObservationsToReport.Checked[Ord(pcReport), RowIndex] then
        begin
          ReportedObs.Add(AName);
        end;
      end;
      ParetoProperties.ObservationsToReport := ReportedObs;
    finally
      ReportedObs.Free;
    end;
    {$ENDREGION}

    frmGoPhast.UndoStack.Submit(TUndoPestOptions.Create(PestProperties,
      FNewObsList, FNewFluxObservationList, FNewHobList, FNewSteadyParameters,
      FNewHufParameters, FNewTransientListParameters, diredPest.Text));
  finally
    PestProperties.Free
  end;

end;

procedure TfrmPEST.SetIREGADJ(const Value: Integer);
begin
  if (csLoading in ComponentState) then
  begin
    Exit;
  end;
  if FSettingIREGADJ then
  begin
    Exit;
  end;
  FSettingIREGADJ := True;
  try
    rgGroupWeightMethod.Enabled := False;
    cbRegApplyGroupWeight.Enabled := False;
    rgIndividualAdjustmentMethod.Enabled := False;
    seNOPTREGADJ.Enabled := False;
    rdeREGWEIGHTRAT.Enabled := False;
    rdeREGSINGTHRESH.Enabled := False;
    rdeIREGADJ.IntegerValue := Value;
    case Value of
      0:
        begin
          rgRegOption.ItemIndex := 0;
        end;
      1:
        begin
          rgRegOption.ItemIndex := 1;
          rgGroupWeightMethod.Enabled := True;
          rgGroupWeightMethod.ItemIndex := 0;
          cbRegApplyGroupWeight.Enabled := True;
          cbRegApplyGroupWeight.Checked := False;
        end;
      2:
        begin
          rgRegOption.ItemIndex := 1;
          rgGroupWeightMethod.Enabled := True;
          rgGroupWeightMethod.ItemIndex := 1;
        end;
      3:
        begin
          rgRegOption.ItemIndex := 1;
          rgGroupWeightMethod.Enabled := True;
          rgGroupWeightMethod.ItemIndex := 0;
          cbRegApplyGroupWeight.Enabled := True;
          cbRegApplyGroupWeight.Checked := True;
        end;
      4:
        begin
          rgRegOption.ItemIndex := 2;
          rgIndividualAdjustmentMethod.Enabled := True;
          rgIndividualAdjustmentMethod.ItemIndex := 0;
          seNOPTREGADJ.Enabled := True;
          rdeREGWEIGHTRAT.Enabled := True;
        end;
      5:
        begin
          rgRegOption.ItemIndex := 2;
          rgIndividualAdjustmentMethod.ItemIndex := 1;
          rgIndividualAdjustmentMethod.Enabled := True;
          seNOPTREGADJ.Enabled := True;
          rdeREGWEIGHTRAT.Enabled := True;
          rdeREGSINGTHRESH.Enabled := True;
        end;
//      else
//        Assert(False);
    end;
  finally
    FSettingIREGADJ := False;
  end;
end;

procedure TfrmPEST.InsertObsGroup(ObsGroupFrame: TframeGrid; Sender: TObject);
var
  Grid: TRbwDataGrid4;
  NewGroup: TPestObservationGroup;
begin
  Grid := ObsGroupFrame.Grid;
  NewGroup := nil;
  if Grid.SelectedRow >= Grid.FixedRows then
  begin
    NewGroup := FLocalObsGroups.Add;
  end;
  ObsGroupFrame.sbInsertClick(Sender);
  if NewGroup <> nil then
  begin
    Grid.Objects[Ord(pogcName), Grid.SelectedRow] := NewGroup;
    NewGroup.Index := Grid.SelectedRow - 1;
    HandleAddedGroup(frameParentObsGroups, NewGroup);
  end;
end;

procedure TfrmPEST.GetCovarianceFileName(ObsGridFrame: TframeGrid;
  ACol, ARow: Integer);
begin
  dlgOpenCovarianceMatrixFile.FileName := ObsGridFrame.Grid.Cells[ACol, ARow];
  if dlgOpenCovarianceMatrixFile.Execute then
  begin
    ObsGridFrame.Grid.Cells[ACol, ARow] := dlgOpenCovarianceMatrixFile.FileName;
  end;
end;

procedure TfrmPEST.CanSelectObsGridCell(ObsGridFrame: TframeGrid;
  ARow, ACol: Integer; var CanSelect: Boolean);
begin
  if (ARow > 0) then
  begin
    if (ACol = Ord(pogcTarget)) then
    begin
      CanSelect := ObsGridFrame.Grid.Checked[Ord(pogcUseTarget), ARow];
    end;
    if (ACol = Ord(pogcUseTarget)) then
    begin
      CanSelect := not ObsGridFrame.Grid.Checked[Ord(pogcRegularization), ARow];
    end;
  end;
end;

procedure TfrmPEST.ChangeObservationGroupName(Grid: TRbwDataGrid4;
  ARow, ACol: Integer; const Value: string);
var
  Group: TPestObservationGroup;
  OtherGroup: TPestObservationGroup;
  TreeNode: TTreeNode;
  OldGroupName: string;
  NewGroupName: string;
  ParetoGroupIndex: Integer;
  ExistingIndex: Integer;
begin
  OldGroupName := '';
  NewGroupName := '';
  if (ARow >= Grid.FixedRows) and (ACol = Ord(pogcName)) then
  begin
    Group := Grid.Objects[ACol, ARow] as TPestObservationGroup;
    if Group <> nil then
    begin
      OldGroupName := Group.ObsGroupName;
      if FGroupNameDictionary.TryGetValue(UpperCase(Group.ObsGroupName), OtherGroup) then
      begin
        if Group = OtherGroup then
        begin
          FGroupNameDictionary.Remove(UpperCase(Group.ObsGroupName));
        end;
      end;
      Group.ObsGroupName := ValidObsGroupName(Value);
      if Group.ObsGroupName <> '' then
      begin
        FGroupNameDictionary.AddOrSetValue(UpperCase(Group.ObsGroupName), Group);
      end;
      if FGroupDictionary.TryGetValue(Group, TreeNode) then
      begin
        TreeNode.Text := Group.ObsGroupName;
      end;
      ParetoGroupIndex := comboParetoGroup.Items.IndexOfObject(Group);
      if ParetoGroupIndex >=0 then
      begin
        ExistingIndex := comboParetoGroup.ItemIndex;
        comboParetoGroup.Items[ParetoGroupIndex] := Group.ObsGroupName;
        comboParetoGroup.ItemIndex := ExistingIndex;
      end;
    end;
  end;
end;

procedure TfrmPEST.ObsGroupDeleteButtonClick(ObsGroupFrame: TframeGrid;
  Sender: TObject);
var
  Grid: TRbwDataGrid4;
  Group: TPestObservationGroup;
begin
  Grid := ObsGroupFrame.Grid;
  if Grid.SelectedRow >= Grid.FixedRows then
  begin
    if Grid.Objects[Ord(pogcName), Grid.SelectedRow] <> nil then
    begin
      Group := Grid.Objects[Ord(pogcName), Grid.SelectedRow] as TPestObservationGroup;
      HandleGroupDeletion(Group);
      Group.Free;
      Grid.Objects[Ord(pogcName), Grid.SelectedRow] := nil;
    end;
  end;
  ObsGroupFrame.sbDeleteClick(Sender);
end;


procedure TfrmPEST.GetObsGroups(ObsGroupFrame: TframeGrid;
  ObsGroups, EditedObsGroups: TPestObservationGroups);
var
  Grid: TRbwDataGrid4;
  ItemIndex: Integer;
  ObsGroup: TPestObservationGroup;
begin
  Grid := ObsGroupFrame.Grid;
  Grid.BeginUpdate;
  try
    Grid.Cells[Ord(pogcName), 0] := StrObservationGroupNa;
    Grid.Cells[Ord(pogcRegularization), 0] := StrRegularizationGroup;
    Grid.Cells[Ord(pogcUseTarget), 0] := StrUseGroupTargetGT;
    Grid.Cells[Ord(pogcTarget), 0] := StrGroupTargetGTARG;
    Grid.Cells[Ord(pogcFileName), 0] := StrCovarianceMatrixFi;
    EditedObsGroups.Assign(ObsGroups);
    ObsGroupFrame.seNumber.AsInteger := EditedObsGroups.Count;
    for ItemIndex := 0 to EditedObsGroups.Count - 1 do
    begin
      ObsGroup := EditedObsGroups[ItemIndex];
      Grid.Objects[Ord(pogcName), ItemIndex + 1] := ObsGroup;
      Grid.Cells[Ord(pogcName), ItemIndex + 1] := ObsGroup.ObsGroupName;
      Grid.Checked[Ord(pogcRegularization), ItemIndex + 1] := ObsGroup.IsRegularizationGroup;
      Grid.Checked[Ord(pogcUseTarget), ItemIndex + 1] := ObsGroup.UseGroupTarget;
      Grid.RealValue[Ord(pogcTarget), ItemIndex + 1] := ObsGroup.GroupTarget;
      Grid.Cells[Ord(pogcFileName), ItemIndex + 1] := ObsGroup.AbsoluteCorrelationFileName;
    end;
  finally
    Grid.EndUpdate;
    if ObsGroups <> nil then
    begin
      ObsGroupFrame.seNumber.AsInteger := ObsGroups.Count;
    end;
  end;
end;

procedure TfrmPEST.ChangeObsGroupNumber(ObsNameGrid: TframeGrid;
  EditedObsGroups: TPestObservationGroups);
var
  Grid: TRbwDataGrid4;
  Names: TStrings;
  NewGroup: TPestObservationGroup;
  OldGroup: TPestObservationGroup;
  index: Integer;
begin
  if FChangingNumberOfGroups then
  begin
    Exit;
  end;
  FChangingNumberOfGroups := True;
  try
    Grid := ObsNameGrid.Grid;
    Names := Grid.Cols[Ord(pogcName)];
    ObsNameGrid.seNumberChange(nil);
    while ObsNameGrid.seNumber.AsInteger > EditedObsGroups.Count do
    begin
      NewGroup := EditedObsGroups.Add;
      Grid.Objects[Ord(pogcName), EditedObsGroups.Count] := NewGroup;
      NewGroup.ObsGroupName := ValidObsGroupName(Grid.Cells[Ord(pogcName), EditedObsGroups.Count]);
      HandleAddedGroup(frameParentObsGroups, NewGroup);
    end;
    while ObsNameGrid.seNumber.AsInteger < EditedObsGroups.Count do
    begin
      OldGroup := EditedObsGroups.Last as TPestObservationGroup;
      index := Names.IndexOfObject(OldGroup);
      HandleGroupDeletion(OldGroup);
      OldGroup.Free;
      if index >= 1 then
      begin
        Grid.Objects[Ord(pogcName), index] := nil;
      end;
    end;
  finally
    FChangingNumberOfGroups := False;
  end;
end;

procedure TfrmPEST.GetUsedTypes(var UsedTypes: TParameterTypes);
begin
  UsedTypes := [];
  if frmGoPhast.ModelSelection = msModflow2015 then
  begin
    UsedTypes := Mf15ParamType;
  end
  else if frmGoPhast.ModelSelection in Modflow2005Selection then
  begin
    UsedTypes := Mf2005ParamType;
  end
  else if frmGoPhast.ModelSelection in SutraSelection then
  begin
    UsedTypes := SutraParamType;
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TfrmPEST.SetObsGroups(ObsGroups: TPestObservationGroups;
  ObsGroupFrame: TframeGrid; EditedObsGroups: TPestObservationGroups);
var
  RowIndex: Integer;
  AnObsGroup: TPestObservationGroup;
  Grid: TRbwDataGrid4;
begin
  Grid := ObsGroupFrame.Grid;
  //    ObsIndex := 0;
  for RowIndex := 1 to ObsGroupFrame.seNumber.AsInteger do
  begin
    AnObsGroup := Grid.Objects[Ord(pogcName), RowIndex] as TPestObservationGroup;
    if (AnObsGroup = nil) and (Grid.Cells[Ord(pogcName), RowIndex] <> '') then
    begin
      AnObsGroup := EditedObsGroups.Add;
    end;
    if AnObsGroup <> nil then
    begin
      if Grid.Cells[Ord(pogcName), RowIndex] <> '' then
      begin
        AnObsGroup.ObsGroupName := Grid.Cells[Ord(pogcName), RowIndex];
      end;
      AnObsGroup.IsRegularizationGroup := Grid.Checked[Ord(pogcRegularization), RowIndex];
      AnObsGroup.UseGroupTarget := Grid.Checked[Ord(pogcUseTarget), RowIndex];
      AnObsGroup.GroupTarget := Grid.RealValueDefault[Ord(pogcTarget), RowIndex, 0];
      AnObsGroup.AbsoluteCorrelationFileName := Grid.Cells[Ord(pogcFileName), RowIndex];
      AnObsGroup.Collection := ObsGroups;
    end;
  end;
end;

procedure TfrmPEST.CheckObsGroupName(Grid: TRbwDataGrid4; ARow: Integer; ACol: Integer);
begin
  if (ARow > 0) and (ACol = Ord(pogcName)) then
  begin
    if Grid.Checked[Ord(pogcRegularization), ARow]
      and (Length(Grid.Cells[Ord(pogcName), ARow]) >
      AllowableGroupNameLength - Length(strRegul)) then
    begin
      Grid.Canvas.Brush.Color := clRed;
    end;
  end;
end;

procedure TfrmPEST.CheckObsGroupTarget(Grid: TRbwDataGrid4; ARow,
  ACol: Integer);
begin
  if (ARow > 0) and (ACol = Ord(pogcTarget)) then
  begin
    if Grid.Checked[Ord(pogcUseTarget), ARow] then
    begin
      if Grid.RealValueDefault[ACol, ARow, 0] <= 0 then
      begin
        Grid.Canvas.Brush.Color := clRed;
      end;
    end;
  end;
end;

procedure TfrmPEST.CheckPriorInfoGroupName(Grid: TRbwDataGrid4; ARow: Integer; ACol: Integer);
begin
  if (ARow > 0) and (ACol = Ord(ppcGroupName)) then
  begin
    if Grid.Checked[Ord(ppcRegularization), ARow] then
    begin
      if Grid.ItemIndex[ACol, ARow] < 0 then
      begin
        Grid.Canvas.Brush.Color := clRed;
      end;
    end;
  end;
end;

procedure TfrmPEST.SetSearchDistanceColor;
begin
  if cbPriorInfoHorizContinuity.Checked and (rdeSearchDistance.RealValue = 0) then
  begin
    rdeSearchDistance.Color := clRed;
  end
  else
  begin
    rdeSearchDistance.Color := clWindow;
  end;
end;

procedure TfrmPEST.AssignParetoObsReports(ParetoProperties: TParetoProperties);
var
  ParetoGroup: TCaption;
  ObsNames: TStringList;
  Index: Integer;
  ObsIndex: Integer;
  AnObs: TCustomObservationItem;
begin
  ParetoGroup := comboParetoGroup.Text;
  rdgObservationsToReport.BeginUpdate;
  ObsNames := TStringList.Create;
  try
    ObsNames.CaseSensitive := False;
    for Index := 0 to FObsList.Count - 1 do
    begin
      AnObs := FObsList[Index];
      ObsNames.AddObject(AnObs.Name, AnObs);
    end;
    rdgObservationsToReport.RowCount := Max(2, ObsNames.Count + 1);
    if ObsNames.Count = 0 then
    begin
      rdgObservationsToReport.Cells[Ord(pcName), 1] := '';
      rdgObservationsToReport.Checked[Ord(pcReport), 1] := False;
    end;
    for ObsIndex := 0 to ObsNames.Count - 1 do
    begin
      rdgObservationsToReport.Cells[Ord(pcName), ObsIndex + 1] :=
        ObsNames[ObsIndex];
      rdgObservationsToReport.Objects[Ord(pcName), ObsIndex + 1] :=
        ObsNames.Objects[ObsIndex];
      rdgObservationsToReport.Checked[Ord(pcReport), ObsIndex + 1] :=
        ParetoProperties.ObservationsToReport.IndexOf(ObsNames[ObsIndex]) >= 0;
    end;
    comboObservationName.Items := ObsNames;
    comboObservationName.ItemIndex :=
      ObsNames.IndexOf(ParetoProperties.ObservationName);
  finally
    ObsNames.Free;
    rdgObservationsToReport.EndUpdate;
  end;
end;

procedure TfrmPEST.CheckAllFirstCol(Grid: TRbwDataGrid4);
var
  RowIndex: Integer;
begin
  for RowIndex := 1 to Grid.RowCount - 1 do
  begin
    Grid.Checked[1, RowIndex] := True;
  end;
end;

procedure TfrmPEST.FixObsGroupNames(ObsGridFrame: TframeGrid);
var
  Grid: TRbwDataGrid4;
  RowIndex: Integer;
  ValidName: string;
begin
//  ObsGridFrame := frameObservationGroups;
  Grid := ObsGridFrame.Grid;
  for RowIndex := 1 to ObsGridFrame.seNumber.AsInteger do
  begin
    if Grid.Cells[Ord(pogcName), RowIndex] <> '' then
    begin
      ValidName := ValidObsGroupName(Grid.Cells[Ord(pogcName), RowIndex]);
      if ValidName <> Grid.Cells[Ord(pogcName), RowIndex] then
      begin
        Grid.Cells[Ord(pogcName), RowIndex] := ValidName;
      end;
    end;
  end;
end;

procedure TfrmPEST.HandleGroupDeletion(Group: TPestObservationGroup);
var
  OtherGroup: TPestObservationGroup;
  TreeNode: TTreeNode;
  ChildNode: TTreeNode;
  GroupIndex: Integer;
  ParetoGroupIndex: Integer;
  SelectedGroup: TObject;
begin
  GroupIndex := comboParetoGroup.Items.IndexOfObject(Group);
  if GroupIndex >= 0 then
  begin
    ParetoGroupIndex := comboParetoGroup.ItemIndex;
    if ParetoGroupIndex >= 0 then
    begin
      SelectedGroup := comboParetoGroup.Items.Objects[ParetoGroupIndex];
    end
    else
    begin
      SelectedGroup := nil;
    end;
    comboParetoGroup.Items.Delete(GroupIndex);
    if SelectedGroup <> nil then
    begin
      ParetoGroupIndex := comboParetoGroup.Items.IndexOfObject(SelectedGroup);
    end
    else
    begin
      ParetoGroupIndex := -1;
    end;
    comboParetoGroup.ItemIndex := ParetoGroupIndex;
  end;

  if FGroupNameDictionary.TryGetValue(
    UpperCase(Group.ObsGroupName), OtherGroup) then
  begin
    if Group = OtherGroup then
    begin
      FGroupNameDictionary.Remove(UpperCase(Group.ObsGroupName));
    end;
  end;
  if FGroupDictionary.TryGetValue(Group, TreeNode) then
  begin
    ChildNode := TreeNode.getFirstChild;
    while ChildNode <> nil do
    begin
      ChildNode.MoveTo(FNoNameNode, naAddChild);
      ChildNode := TreeNode.getFirstChild;
    end;
  end;
end;

procedure TfrmPEST.ImportPilotPoints(const FileName: string);
var
  Extension: string;
  DisLimits: TGridLimit;
  PointsQuadTree: TRbwQuadTree;
  ShapeHeaderFile: string;
  ShapeIndex: Integer;
  AShape: TShapeObject;
  ShapeFileReader: TShapefileGeometryReader;
  APoint: TShapePoint;
  APoint2D: TPoint2D;
  PointList: TList<TPoint2D>;
  Splitter: TStringList;
  PointIndex: Integer;
  TextReader: TStreamReader;
  ALine: string;
  LineIndex: Integer;
  Value: Extended;
  FirstRow: Int64;
  procedure HandleAPoint(APoint2D: TPoint2D);
  var
    DupPoint: TPoint2D;
    APointer: Pointer;
  begin
    if PointList.Count = 0 then
    begin
      PointList.Add(APoint2D);
      PointsQuadTree.AddPoint(APoint2D.x, APoint2D.y, nil);
    end
    else
    begin
      DupPoint := APoint2D;
      PointsQuadTree.FirstNearestPoint(DupPoint.x, DupPoint.y, APointer);
      if (DupPoint.x <> APoint2D.x) or (DupPoint.y <> APoint2D.y) then
      begin
        PointList.Add(APoint2D);
        PointsQuadTree.AddPoint(APoint2D.x, APoint2D.y, nil);
      end;
    end;
  end;
begin
  Assert(TFile.Exists(FileName));
  Extension := LowerCase(ExtractFileExt(FileName));
  PointsQuadTree := TRbwQuadTree.Create(nil);
  PointList := TList<TPoint2D>.Create;
  try
    DisLimits := frmGoPhast.PhastModel.DiscretizationLimits(vdTop);
    PointsQuadTree.XMax := DisLimits.MaxX;
    PointsQuadTree.XMin := DisLimits.MinX;
    PointsQuadTree.YMax := DisLimits.MaxY;
    PointsQuadTree.YMin := DisLimits.MinY;
    if Extension = '.shp' then
    begin
      ShapeFileReader := TShapefileGeometryReader.Create;
      try
        ShapeHeaderFile := ChangeFileExt(FileName, '.shx');
        if not TFile.Exists(ShapeHeaderFile) then
        begin
          Beep;
          MessageDlg(Format(StrTheShapeHeaderFil, [ShapeHeaderFile]), mtError,
            [mbOK], 0);
          Exit;
        end;
        ShapeFileReader.ReadFromFile(FileName, ShapeHeaderFile);
        for ShapeIndex := 0 to ShapeFileReader.Count - 1 do
        begin
          AShape := ShapeFileReader[ShapeIndex];
          for PointIndex := 0 to AShape.FNumPoints - 1 do
          begin
            APoint := AShape.FPoints[PointIndex];
            APoint2D.x := APoint.X;
            APoint2D.y := APoint.Y;
            HandleAPoint(APoint2D);
          end;
        end;
      finally
        ShapeFileReader.Free;
      end;
    end
    else  if (Extension = '.csv') or (Extension = '.txt') then
    begin
      Splitter := TStringList.Create;
      TextReader := TFile.OpenText(FileName);
      try
        LineIndex := 0;
        repeat
          ALine := TextReader.ReadLine;
          Inc(LineIndex);
          if (ALine = '') or (ALine[1] = '#') then
          begin
            Continue;
          end;
          Splitter.DelimitedText := ALine;
          if Splitter.Count >= 2 then
          begin
            if TryFortranStrToFloat(Splitter[0], Value) then
            begin
              APoint2D.x := Value;
            end
            else
            begin;
              Beep;
              MessageDlg(Format(
                'The %0:s value "%1:s" in line %2:d can not be converted to a real number.'
                , ['first', Splitter[0], LineIndex]), mtError, [mbOK], 0);
              Exit;
            end;

            if TryFortranStrToFloat(Splitter[1], Value) then
            begin
              APoint2D.y := Value;
            end
            else
            begin;
              Beep;
              MessageDlg(Format(
                'The %0:s value "%1:s" in line %2:d can not be converted to a real number.'
                , ['second', Splitter[1], LineIndex]), mtError, [mbOK], 0);
              Exit;
            end;
            HandleAPoint(APoint2D);
          end
          else
          begin
            Beep;
            MessageDlg(Format(StrLine0d1sM, [LineIndex, ALine]), mtError,
              [mbOK], 0);
            Exit;
          end;
        until (TextReader.EndOfStream);
      finally
        Splitter.Free;
        TextReader.Free;
      end;
    end
    else
    begin
      Assert(False);
    end;

    FirstRow := framePilotPoints.seNumber.AsInteger + 1;
    framePilotPoints.seNumber.AsInteger := framePilotPoints.seNumber.AsInteger
      + PointList.Count;
    for PointIndex := 0 to PointList.Count - 1 do
    begin
      APoint2D := PointList[PointIndex];
      framePilotPoints.Grid.RealValue[Ord(ppcX), FirstRow + PointIndex] := APoint2D.x;
      framePilotPoints.Grid.RealValue[Ord(ppcy), FirstRow + PointIndex] := APoint2D.y;
    end;
  finally
    PointsQuadTree.Free;
    PointList.Free;
  end;
end;

procedure TfrmPEST.AutoSetPhimAccept;
begin
  if (csLoading in ComponentState) then
  begin
    Exit;
  end;
  if cbAutomaticallySetPHIMACCEPT.Checked then
  begin
    rdePhimAccept.RealValue := rdePhimLim.RealValue * 1.05;
  end;
end;

procedure TfrmPEST.SetrdePhimAcceptColor;
var
  PHIMLIM: Double;
  PHIMACCEPT: Double;
begin
  if (csLoading in ComponentState) then
  begin
    Exit;
  end;
  PHIMLIM := rdePhimLim.RealValue;
  PHIMACCEPT := rdePhimAccept.RealValue;
  if PHIMACCEPT <= PHIMLIM then
  begin
    rdePhimAccept.Color := clRed;
  end
  else
  begin
    rdePhimAccept.Color := clWindow;
  end;
end;

procedure TfrmPEST.SetWfMaxVisibility;
var
  WFMIN: Double;
  WFMAX: Double;
begin
  if (csLoading in ComponentState) then
  begin
    Exit;
  end;
  WFMIN := rdeWFMIN.RealValue;
  WFMAX := rdeWFMAX.RealValue;
  if WFMAX <= WFMIN then
  begin
    rdeWFMAX.Color := clRed;
  end
  else
  begin
    rdeWFMAX.Color := clWindow;
  end;
end;

procedure TfrmPEST.tvPESTMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  SelectedNode: TJvPageIndexNode;
begin
  inherited;
  if not (htOnButton in tvPEST.GetHitTestInfoAt(X, Y)) then
  begin
    SelectedNode := tvPEST.GetNodeAt(X, Y) as TJvPageIndexNode;
    if (SelectedNode <> nil) and (SelectedNode.PageIndex < 0)
      and SelectedNode.HasChildren then
    begin
      if not SelectedNode.Expanded then
      begin
        tvPEST.Selected := SelectedNode.GetFirstChild
      end
      else
      begin
        SelectedNode.Expanded := False;
      end;
    end;
  end;
end;

procedure TfrmPEST.UpdateIREGADJ_FromControls(Sender: TObject);
begin
  rdeIREGADJ.IntegerValue := IREGADJ;
end;

procedure TfrmPEST.HandleAddedGroup(TreeObsGroupFrame: TframeParentChild;
  ObsGroup: TPestObservationGroup);
var
  NewNode: TTreeNode;
  ParetoGroupIndex: Integer;
begin
  NewNode := TreeObsGroupFrame.tvTree.Items.AddChild(nil,
    ObsGroup.ObsGroupName);
  NewNode.Data := ObsGroup;
  FGroupDictionary.AddOrSetValue(ObsGroup, NewNode);
  if ObsGroup.ObsGroupName <> '' then
  begin
    FGroupNameDictionary.AddOrSetValue(UpperCase(ObsGroup.ObsGroupName), ObsGroup);
  end;
  ParetoGroupIndex := comboParetoGroup.ItemIndex;
  comboParetoGroup.Items.AddObject(ObsGroup.ObsGroupName, ObsGroup);
  comboParetoGroup.ItemIndex := ParetoGroupIndex;
end;

procedure TfrmPEST.CheckPestDirectory;
begin
  if DirectoryExists(diredPest.Text) then
  begin
    diredPest.Color := clWindow;
  end
  else
  begin
    diredPest.Color := clRed;
  end;
end;

{ TUndoPestOptions }

constructor TUndoPestOptions.Create(var NewPestProperties: TPestProperties;
  var NewObsList: TObservationObjectList;
  var NewFluxObservationList: TFluxObservationObjectList;
  var NewHobList: THobObjectList;
  var NewSteadyParameters: TModflowSteadyParameters;
  var NewHufParameters: THufModflowParameters;
  var NewTransientListParameters: TModflowTransientListParameters;
  PestDirectory: String);
var
  InvalidateModelEvent: TNotifyEvent;
  TempList: TObservationList;
  index: Integer;
  AnObs: TCustomObservationItem;
  ATempObs: TCustomObservationItem;
  Locations: TProgramLocations;
  TempFluxObsList: TFluxObservationList;
  FluxObsIndex: Integer;
  FluxObs: TFluxObservationGroup;
  TempHobList: THobList;
  HobIndex: Integer;
  HobItem: THobItem;
begin
  Locations := frmGoPhast.PhastModel.ProgramLocations;
  OldPestLocation := Locations.PestDirectory;
  NewPestLocation := PestDirectory;

  InvalidateModelEvent := nil;
  FOldPestProperties := TPestProperties.Create(nil);
  FOldPestProperties.Assign(frmGoPhast.PhastModel.PestProperties);
  FNewPestProperties := NewPestProperties;
  NewPestProperties := nil;

  TempList := TObservationList.Create;
  try
    TempList.Capacity := NewObsList.Count;
    frmGoPhast.PhastModel.FillObsItemList(TempList, True);
    FOldObsList  := TObservationObjectList.Create;
    FOldObsList.Capacity := TempList.Count;
    for index := 0 to TempList.Count - 1 do
    begin
      AnObs := TempList[index];
      ATempObs := TCustomObservationItem.Create(nil);
      FOldObsList.Add(ATempObs);
      ATempObs.Assign(AnObs);
    end;
  finally
    TempList.Free;
  end;

  FNewObsList := NewObsList;
  NewObsList := nil;

  TempFluxObsList := TFluxObservationList.Create;
  try
    TempFluxObsList.Capacity := NewFluxObservationList.Count;
    frmGoPhast.PhastModel.FillFluxObsList(TempFluxObsList);
    FOldFluxObsList := TFluxObservationObjectList.Create;
    for FluxObsIndex := 0 to TempFluxObsList.Count - 1 do
    begin
      FluxObs := TFluxObservationGroup.Create(nil);
      FOldFluxObsList.Add(FluxObs);
      FluxObs.Assign(TempFluxObsList[FluxObsIndex]);
    end;
  finally
    TempFluxObsList.Free;
  end;

  FNewFluxObsList := NewFluxObservationList;
  NewFluxObservationList := nil;

  TempHobList := THobList.Create;
  try
    TempHobList.Capacity := NewHobList.Count;
    frmGoPhast.PhastModel.FillHobList(TempHobList);
    FOldHobList := THobObjectList.Create;
    for HobIndex := 0 to TempHobList.Count - 1 do
    begin
      HobItem := THobItem.Create(nil);
      FOldHobList.Add(HobItem);
      HobItem.Assign(TempHobList[HobIndex]);
    end;
  finally
    TempHobList.Free;
  end;

  FNewHobList := NewHobList;
  NewHobList := nil;
  
  FNewSteadyParameters := NewSteadyParameters;
  FNewHufParameters := NewHufParameters;
  FNewTransientParameters := NewTransientListParameters;
  NewSteadyParameters := nil;
  NewHufParameters := nil;
  NewTransientListParameters := nil;

  FOldSteadyParameters := TModflowSteadyParameters.Create(nil);
  FOldHufParameters := THufModflowParameters.Create(nil);
  FOldTransientParameters := TModflowTransientListParameters.Create(nil);
  FOldSteadyParameters.Assign(frmGoPhast.PhastModel.ModflowSteadyParameters);
  FOldHufParameters.Assign(frmGoPhast.PhastModel.HufParameters);
  FOldTransientParameters.Assign(frmGoPhast.PhastModel.ModflowTransientParameters);
end;

procedure TUndoPestOptions.AssignParameters(SteadyParameters: TModflowSteadyParameters;
  HufParameters: THufModflowParameters; TransientListParameters: TModflowTransientListParameters);
var
  ModelSteadyParameters: TModflowSteadyParameters;
  ParamIndex: Integer;
  ExistingParam: TModflowParameter;
  ModifiedParam: TModflowParameter;
  ModelHufParameters: THufModflowParameters;
  ModelTransientListParameters: TModflowTransientListParameters;
  ModifiedParamSteady: TModflowSteadyParameter;
  ExistingParamSteady: TModflowSteadyParameter;
begin
  ModelSteadyParameters := frmGoPhast.PhastModel.ModflowSteadyParameters;
  Assert(ModelSteadyParameters.Count = SteadyParameters.Count);
  for ParamIndex := 0 to SteadyParameters.Count -1 do
  begin
    ExistingParamSteady := ModelSteadyParameters[ParamIndex];
    ModifiedParamSteady := SteadyParameters[ParamIndex];
    ExistingParamSteady.UseInitialValuePriorInfo := ModifiedParamSteady.UseInitialValuePriorInfo;
    ExistingParamSteady.RegularizationGroup := ModifiedParamSteady.RegularizationGroup;
    ExistingParamSteady.InitialValuePriorInfoWeight := ModifiedParamSteady.InitialValuePriorInfoWeight;
    ExistingParamSteady.UseHorizontalSpatialContinuityPriorInfo := ModifiedParamSteady.UseHorizontalSpatialContinuityPriorInfo;
    ExistingParamSteady.HorizontalSpatialContinuityGroupName := ModifiedParamSteady.HorizontalSpatialContinuityGroupName;
    ExistingParamSteady.HorizontalSpatialContinuityPriorInfoWeight := ModifiedParamSteady.HorizontalSpatialContinuityPriorInfoWeight;
    ExistingParamSteady.UseVertSpatialContinuityPriorInfo := ModifiedParamSteady.UseVertSpatialContinuityPriorInfo;
    ExistingParamSteady.VertSpatialContinuityGroupName := ModifiedParamSteady.VertSpatialContinuityGroupName;
    ExistingParamSteady.VertSpatialContinuityPriorInfoWeight := ModifiedParamSteady.VertSpatialContinuityPriorInfoWeight;
  end;
  ModelHufParameters := frmGoPhast.PhastModel.HufParameters;
  Assert(ModelHufParameters.Count = HufParameters.Count);
  for ParamIndex := 0 to HufParameters.Count -1 do
  begin
    ExistingParam := ModelHufParameters[ParamIndex];
    ModifiedParam := HufParameters[ParamIndex];
    ExistingParam.UseInitialValuePriorInfo := ModifiedParam.UseInitialValuePriorInfo;
    ExistingParam.RegularizationGroup := ModifiedParam.RegularizationGroup;
    ExistingParam.InitialValuePriorInfoWeight := ModifiedParam.InitialValuePriorInfoWeight;
  end;
  ModelTransientListParameters := frmGoPhast.PhastModel.ModflowTransientParameters;
  Assert(ModelTransientListParameters.Count = TransientListParameters.Count);
  for ParamIndex := 0 to TransientListParameters.Count -1 do
  begin
    ExistingParam := ModelTransientListParameters[ParamIndex];
    ModifiedParam := TransientListParameters[ParamIndex];
    ExistingParam.UseInitialValuePriorInfo := ModifiedParam.UseInitialValuePriorInfo;
    ExistingParam.RegularizationGroup := ModifiedParam.RegularizationGroup;
    ExistingParam.InitialValuePriorInfoWeight := ModifiedParam.InitialValuePriorInfoWeight;
  end;
end;

function TUndoPestOptions.Description: string;
begin
  result := 'change PEST properties';
end;

destructor TUndoPestOptions.Destroy;
begin
  FNewSteadyParameters.Free;
  FNewHufParameters.Free;
  FNewTransientParameters.Free;
  FOldSteadyParameters.Free;
  FOldHufParameters.Free;
  FOldTransientParameters.Free;
  FNewFluxObsList.Free;
  FOldFluxObsList.Free;
  FOldObsList.Free;
  FNewObsList.Free;
  FOldPestProperties.Free;
  FNewPestProperties.Free;
  FOldHobList.Free;
  FNewHobList.Free;
  inherited;
end;

procedure TUndoPestOptions.DoCommand;
var
  Locations: TProgramLocations;
begin
  inherited;
//  frmGoPhast.PhastModel.PestProperties := FNewPestProperties;
  UpdateProperties(FNewPestProperties, FNewObsList, FNewFluxObsList, FNewHobList);
  AssignParameters(FNewSteadyParameters, FNewHufParameters, FNewTransientParameters);
  Locations := frmGoPhast.PhastModel.ProgramLocations;
  Locations.PestDirectory := NewPestLocation;
  frmGoPhast.PhastModel.SetMf2005ObsGroupNames;
  frmGoPhast.EnableManageParameters;
  frmGoPhast.UpdateControlsEnabledOrVisible;
end;

procedure TUndoPestOptions.Undo;
var
  Locations: TProgramLocations;
begin
  inherited;
  UpdateProperties(FOldPestProperties, FOldObsList, FOldFluxObsList, FOldHobList);
  AssignParameters(FOldSteadyParameters, FOldHufParameters, FOldTransientParameters);
  Locations := frmGoPhast.PhastModel.ProgramLocations;
  Locations.PestDirectory := OldPestLocation;
  frmGoPhast.PhastModel.SetMf2005ObsGroupNames;
  frmGoPhast.EnableManageParameters;
  frmGoPhast.UpdateControlsEnabledOrVisible;
end;

procedure TUndoPestOptions.UpdateProperties(PestProperties: TPestProperties;
  ObsList: TObservationObjectList; FluxObsList: TFluxObservationObjectList;
  HobList: THobObjectList);
var
  ShouldUpdateView: Boolean;
  TempList: TObservationList;
  AnObs: TCustomObservationItem;
  NewObs: TCustomObservationItem;
  ObsIndex: Integer;
  TempFluxObsList: TFluxObservationList;
  FluxObsIndex: Integer;
  FluxObs: TFluxObservationGroup;
  TempHobList: THobList;
  HobIndex: Integer;
  HobObs: THobItem;
begin
  ShouldUpdateView := frmGoPhast.PhastModel.PestProperties.ShouldDrawPilotPoints
    <> PestProperties.ShouldDrawPilotPoints;
  if PestProperties.ShouldDrawPilotPoints then
  begin
    if PestProperties.PilotPointSpacing
      <> frmGoPhast.PhastModel.PestProperties.PilotPointSpacing then
    begin
      ShouldUpdateView := True;
    end;
    if PestProperties.ArrayPilotPointSelection
      <> frmGoPhast.PhastModel.PestProperties.ArrayPilotPointSelection then
    begin
      ShouldUpdateView := True;
    end;
    if PestProperties.UseBetweenObservationsPilotPoints
      <> frmGoPhast.PhastModel.PestProperties.UseBetweenObservationsPilotPoints then
    begin
      ShouldUpdateView := True;
    end;
    if not ShouldUpdateView then
    begin
      ShouldUpdateView := not PestProperties.SpecifiedPilotPoints.IsSame(
        frmGoPhast.PhastModel.PestProperties.SpecifiedPilotPoints)
    end;
    if not ShouldUpdateView and PestProperties.UseBetweenObservationsPilotPoints then
    begin
      ShouldUpdateView := not PestProperties.BetweenObservationsPilotPoints.IsSame(
        frmGoPhast.PhastModel.PestProperties.BetweenObservationsPilotPoints)
    end;

  end;
  frmGoPhast.PhastModel.PestProperties := PestProperties;

  TempList := TObservationList.Create;
  try
    TempList.Capacity := ObsList.Count;
    frmGoPhast.PhastModel.FillObsItemList(TempList, True);
    Assert(TempList.Count = ObsList.Count);
    for ObsIndex := 0 to TempList.Count - 1 do
    begin
      AnObs := TempList[ObsIndex];
      NewObs := ObsList[ObsIndex];
      AnObs.Assign(NewObs);
    end;
  finally
    TempList.Free;
  end;

  TempFluxObsList := TFluxObservationList.Create;
  try
    TempFluxObsList.Capacity := FluxObsList.Count;
    frmGoPhast.PhastModel.FillFluxObsList(TempFluxObsList);
    for FluxObsIndex := 0 to TempFluxObsList.Count - 1 do
    begin
      FluxObs := TempFluxObsList[FluxObsIndex];
      FluxObs.Assign(FluxObsList[FluxObsIndex]);
    end;
  finally
    TempFluxObsList.Free;
  end;

  TempHobList := THobList.Create;
  try
    TempHobList.Capacity := HobList.Count;
    frmGoPhast.PhastModel.FillHobList(TempHobList);
    for HobIndex := 0 to TempHobList.Count - 1 do
    begin
      HobObs := TempHobList[HobIndex];
      HobObs.Assign(HobList[HobIndex]);
    end;
  finally
    TempHobList.Free;
  end;

  frmGoPhast.EnablePilotPointItems;

  if ShouldUpdateView then
  begin
    frmGoPhast.SynchronizeViews(vdTop);
  end;
end;

{ TCheckedPointItem }

constructor TCheckedPointItem.Create(Collection: TCollection);
begin
  inherited;
  FoundPoints := nil;
  FChecked := False;
end;


{ TCheckedPoints }

function TCheckedPoints.Add: TCheckedPointItem;
begin
  result := inherited Add as TCheckedPointItem;
end;

constructor TCheckedPoints.Create;
begin
  inherited Create(TCheckedPointItem);
end;

function TCheckedPoints.GetPoint(Index: Integer): TCheckedPointItem;
begin
  result := inherited Items[index] as TCheckedPointItem;
end;

procedure TCheckedPoints.SetPoint(Index: Integer;
  const Value: TCheckedPointItem);
begin
  inherited Items[index] := Value;
end;

end.
