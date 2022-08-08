unit frmSelectResultToImportUnit;

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, CheckLst, JvExCheckLst,
  JvCheckListBox, Buttons, JvDialogs, IntListUnit, ReadModflowArrayUnit,
  DataSetUnit, ScreenObjectUnit, StrUtils, UndoItems, Contnrs, RealListUnit,
  ModflowGridUnit, ExtCtrls, EdgeDisplayUnit, GoPhastTypes, Grids, RbwDataGrid4,
  PhastModelUnit, Types, ModflowSwrWriterUnit, ReadSwrOutputUnit,
  JvExControls, JvxCheckListBox, RbwEdit, ModflowIrregularMeshUnit;

type
  TModflowResultFormat = (mrBinary, mrAscii, mrFlux, mrHufAscii, mrHufBinary,
    mrHufFlux, mfSubBinary, mfMt3dConc, mfSwrStageAscii, mfSwrStageBinary,
    mfSwrReachExchangeAscii, mfSwrReachExchangeBinary,
    mfSwrReachGroupBudgetAscii, mfSwrReachGroupBudgetBinary, mfCSubBinary);

  TModelColumns = (mcModelName, mcUse, mcFileName);

  TDataArrayForm = (dafLayer, dafSystem, dafSubsidence, dafWaterTable, dafSwrStage);

  EHufReadError = class(Exception);

  TUndoImportModelResults = class(TCustomUndo)
  private
    FNewTopDataSet: TDataArray;
    FNewFrontDataSet: TDataArray;
    FNewSideDataSet: TDataArray;
    FNew3DDataSet: TDataArray;
    FNewTopContourDataSet: TDataArray;
    FNewFrontContourDataSet: TDataArray;
    FNewSideContourDataSet: TDataArray;
    FNew3DContourDataSet: TDataArray;
    FNewEdgeDisplay: TCustomModflowGridEdgeDisplay;

    FNewThreeDTimeList: TCustomTimeList;
    FNewTopTimeList: TCustomTimeList;
    FNewFrontTimeList: TCustomTimeList;
    FNewSideTimeList: TCustomTimeList;

    FOldTopDataSet: TDataArray;
    FOldFrontDataSet: TDataArray;
    FOldSideDataSet: TDataArray;
    FOld3DDataSet: TDataArray;
    FOldTopContourDataSet: TDataArray;
    FOldFrontContourDataSet: TDataArray;
    FOldSideContourDataSet: TDataArray;
    FOld3DContourDataSet: TDataArray;
    FOldEdgeDisplay: TCustomModflowGridEdgeDisplay;

    FOldThreeDTimeList: TCustomTimeList;
    FOldTopTimeList: TCustomTimeList;
    FOldFrontTimeList: TCustomTimeList;
    FOldSideTimeList: TCustomTimeList;

    FContainedUndos: TList;
    FNewDataSets: TList;
    FOldComments: TStringList;
    FNewComments: TStringList;
    FModel: TCustomModel;
    procedure SetComments(Comments: TStringList);
  protected
    function Description: string; override;
  public
    Constructor Create(NewDataSets: TList;
      DataSetNames, OldComments: TStringList;
      DisplayDataSet: TDataArray; DisplayChoice: TDisplayChoice;
      AModel: TCustomModel);
    Destructor Destroy; override;
    // @name does the command for the first time.
    procedure DoCommand; override;
    // @name undoes the command.
    procedure Undo; override;
  end;

type
  TFormulaAssigner = class(TObject)
  strict private
    FDataArray: TDataArray;
    FFormulas: TStringList;
    FModels: TList;
  public
    Constructor Create(ADataArray: TDataArray);
    Destructor Destroy; override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure AddFormula(AFormula: string; AModel: TBaseModel);
    procedure AssignFinalFormula;
    property DataArray: TDataArray read FDataArray;
  end;

  TFormulaAssignerList = class(TObject)
  strict private
    FList: TList;
    function GetFormulaAssigner(ADataArray: TDataArray): TFormulaAssigner;
  public
    Constructor Create;
    Destructor Destroy; override;
    property FormulaAssigners[ADataArray: TDataArray]: TFormulaAssigner
      read GetFormulaAssigner; default;
    procedure AssignFinalFormulas;
  end;

  TfrmSelectResultToImport = class(TfrmCustomGoPhast)
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    odSelectFiles: TJvOpenDialog;
    comboColorGrid: TComboBox;
    lblColorGrid: TLabel;
    btnSelectAll: TButton;
    btnSelectNone: TButton;
    rgDisplayChoice: TRadioGroup;
    pnlSelections: TPanel;
    rdgModels: TRbwDataGrid4;
    clData: TJvCheckListBox;
    splitData: TSplitter;
    pnlMultiSelect: TPanel;
    splMultiSelect: TSplitter;
    spl1: TSplitter;
    clTime: TJvxCheckListBox;
    clDescription: TJvxCheckListBox;
    comboClassification: TComboBox;
    lblClassification: TLabel;
    edPrefix: TRbwEdit;
    lblPrefix: TLabel;
    procedure clDataClickCheck(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure odSelectFilesTypeChange(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnSelectNoneClick(Sender: TObject);
    procedure rdgModelsBeforeDrawCell(Sender: TObject; ACol, ARow: Integer);
    procedure rdgModelsButtonClick(Sender: TObject; ACol, ARow: Integer);
    procedure FormShow(Sender: TObject);
    procedure clDescriptionStateChange(Sender: TObject; Index: Integer);
    procedure clTimeStateChange(Sender: TObject; Index: Integer);
    procedure comboClassificationChange(Sender: TObject);
  private
    FPeriods: TIntegerList;
    FSteps: TIntegerList;
    FTransportSteps: TIntegerList;
    FSwrSteps: TIntegerList;
    FDescriptions: TStringList;
    FFileStream: TFileStream;
    FFileVariable: TFileVariable;
    FResultFormat: TModflowResultFormat;
    FMaxPeriod: Integer;
    FMaxTrans: integer;
    FMaxStep: Integer;
    FMaxLayer: Integer;
    FGrid: TModflowGrid;
    FDisvGrid: TModflowDisvGrid;
    FNewDataSetNames: TStringList;
    FNewDefaultDataSetNames: TStringList;
    FFormulaAssigners: TFormulaAssignerList;
    FModifiedParentDataSets: TList;
    FSettingAll: boolean;

    KSTP: Integer;
    KPER: Integer;
    NTRANS: Integer;
    TOTIM: TModflowDouble;
    SwrTimeStep: integer;
    MinValues: TRealList;
    MaxValues: TRealList;
    NewDataSets: TList;
    DataSetNames: TStringList;
    ScreenObjectsToDelete: TScreenObjectList;
    NewCreateScreenObjects: TList;
    ValuesToIgnore: TOneDRealArray;

    FItemDescriptions: TStringList;
    FItemTimes: TStringList;


    FSPeciesName: string;
    function DefaultFileName(AModel: TCustomModel; Directory: string = ''): string;
    function OpenResultFile(AFileName: string;out Precision: TModflowPrecision;
      out HufFormat: boolean): boolean;
    procedure ReadArray(var AnArray: TModflowDoubleArray;
      var EndReached: Boolean; var NTRANS, KPER, KSTP, ILAY: Integer;
      var TOTIM: TModflowDouble;
      var Description: string; Precision: TModflowPrecision;
      ShouldReadArray: boolean);
    procedure CreateOrRetrieveLayerDataSet(const Description: string;
      ILAY: integer;
      out LayerData: TDataArray; out OldComment: string;
      FileNames: string;
      AModel: TCustomModel; DataArrayType: TDataArrayType; Precision: TModflowPrecision;
      DataArrayForm: TDataArrayForm = dafLayer);
    procedure CreateScreenObject(LayerIndex: integer; AModel: TCustomModel;
      out ScreenObject: TScreenObject; FileName: string);
    procedure AssignValues(LayerIndex: integer; ScreenObject: TScreenObject;
      LayerData: TDataArray; AnArray: TModflowDoubleArray;
      ValuesToIgnore: TOneDRealArray; AModel: TCustomModel;
      out MinMaxAssigned: boolean);
    procedure CreateOrRetrieve3DDataSet(Description: string;
      NTRANS, KPER, KSTP: integer; TOTIM: TModflowDouble;
      LayerNumbers: TIntegerList; LayerDataSets: TList;
      out New3DArray: TDataArray; out OldComment: string; FluxData: boolean;
      NewDataSets: TList; FileNames: string; AModel: TCustomModel;
      Precision: TModflowPrecision);
    procedure CloseFiles;
    procedure Read3DArray(var NLAY: Integer; var EndReached: Boolean;
      var KPER, KSTP: Integer; var TOTIM: TModflowDouble;
      var Description: string; var A3DArray: T3DTModflowArray; var AuxArray: TAuxArrays;
      Precision: TModflowPrecision; HufFormat: boolean;
      ShouldReadArray: boolean; AModel: TCustomModel;
      var ModelName1, ModelName2, PackageName1, PackageName2: string);
    procedure Assign3DValues(ScreenObject: TScreenObject; LayerData: TDataArray;
      AnArray: T3DTModflowArray; ModflowLayerIndex, DataSetLayerIndex: integer; CheckAllLayers: boolean;
      ValuesToIgnore: TOneDRealArray; AModel: TCustomModel);
    procedure SetData;
    procedure AssignLimits(MinValues, MaxValues: TRealList;
      New3DArray: TDataArray; ValuesToIgnore: TOneDRealArray);
    procedure AssignObjectName(var ScreenObject: TScreenObject; LayerData: TDataArray);
    procedure UpdateCombo;
    procedure GetShouldIgnore(ValuesToIgnore: TOneDRealArray;
      Temp: TModflowFloat; var ShouldIgnore: Boolean);
    function SubsidenceDescription(DESC: String; ILAY: integer): string;
    // In the label for data sets, TOTIM will be measured from the end of
    // the first stress period if there are more than one stress period and
    // the first stress period is a steady-state stress period.
    procedure AdjustTotalTime(var TOTIM: TModflowDouble);
    procedure AssignWaterTableArray(
      var WaterTableArray: TModflowDoubleArray;
      ILAY: Integer;
      AnArray: TModflowDoubleArray;
      ValuesToIgnore: TOneDRealArray;
      const Description: string);
    procedure AssignWaterTable(OldComments: TStringList;
      DataSetNames: TStringList;
      NewCreateScreenObjects: TList;
      WaterTableArray: TModflowDoubleArray;
      const Description: string; ValuesToIgnore: TOneDRealArray;
      FileNames: string; AModel: TCustomModel; Precision: TModflowPrecision);
    procedure SetDefaultDisplayOption;
    { Private declarations }
    function ReadDataHeadings(AModel: TCustomModel; AFileName: string): boolean;
    procedure AddModelRow(AModel: TCustomModel; ARow: integer;
      AFileName: string);
    procedure UpdateOldComments(OldComments: TStringList;
      ADataArray: TDataArray; OldComment: string);
    procedure AdjustSwiNames(var NewName: string);
    function WriteLabel(var Description: string; AModel: TCustomModel;
      ILAY, KPER, KSTP, NTRANS, SwrTimeStep: integer; TOTIM: TModflowDouble;
      Precision: TModflowPrecision;
      ModelName1, ModelName2, PackageName1, PackageName2: String): string;
    procedure CreateSwrStageScreenObject(AModel: TCustomModel;
      out ScreenObject: TScreenObject; Root: string; AFileName: string);
    procedure CreateSwrReachGroupFlowScreenObject(AModel: TCustomModel;
      out ScreenObject: TScreenObject; Root: string;
      SwrWriter: TModflowSwrWriter; FileName: string);
    procedure CreateSwrReachExchangeScreenObject(AModel: TCustomModel;
      var ScreenObject: TScreenObject; Root: string;
      SwrWriter: TModflowSwrWriter; NewCreateScreenObjects: TList;
      AReachExchange, PriorReachExchange: TReachExchange; FileName: string);
    procedure AssignSwrStageValues(ScreenObject: TScreenObject;
      LayerData: TDataArray;
      SwrData: TDoubleDynArray; AModel: TCustomModel;
      out MinMaxAssigned: boolean);
    procedure AssignSwrReachExchangeValues(ScreenObject: TScreenObject;
      LayerData: TDataArray; ValuesToIgnore: TOneDRealArray;
      SwrData: TReachExchange; AModel: TCustomModel;
      out MinMaxAssigned: boolean; SwrDataType: integer);
    procedure AssignSwrWaterBudgetValues(ScreenObject: TScreenObject;
      LayerData: TDataArray; ValuesToIgnore: TOneDRealArray;
      SwrData: TReachGroupWaterBudget; AModel: TCustomModel;
      out MinMaxAssigned: boolean; SwrDataType: integer;
      SwrWriter: TModflowSwrWriter);
    function ImportSwrStages(AModel: TCustomModel; AFileName: string;
      ILAY: Integer; FileNames: string; LastItem: Integer;
      OldComments: TStringList; Precision: TModflowPrecision): Boolean;
    function ImportSwrReachExchange(AModel: TCustomModel; AFileName: string;
      ILAY: Integer; FileNames: string; LastItem: Integer;
      OldComments: TStringList; Precision: TModflowPrecision): boolean;
    procedure ImportSwrReachGroupFlows(AModel: TCustomModel; AFileName: string;
      ILAY: Integer; FileNames: string; LastItem: Integer;
      OldComments: TStringList; Precision: TModflowPrecision);
    function GetClassificationGroup: string;
    function GetPrefix: string;
  public
    function ShowDataSets(AFileName: string): boolean;
    function SelectFiles: boolean;
    { Public declarations }
  end;

var
  frmSelectResultToImport: TfrmSelectResultToImport;
  // @name records how many times the user has chosen to color the grid,
  // contour the data, or do nothing.  The most frequent choice is then
  // selected at the default when the @link(TfrmSelectResultToImport) is
  // created.
  DisplayChoices : array[TDisplayChoice] of integer = (0, 0, 0);
const
  MaxDisplayChoiceCount = 10;

resourcestring
  StrLayerData = '|Layer Data';
  StrThreeDData = '|3D Data';
  KSystem = '|System';
  KWaterTable = '|Water Table';
  KSwrStage = '|SWR_Data';

implementation

uses Math, frmGoPhastUnit, RbwParser,
  GIS_Functions, ValueArrayStorageUnit, ModelMuseUtilities,
  frmUpdateDataSetsUnit, UndoItemsScreenObjects,
  InterpolationUnit, HufDefinition, ModflowTimeUnit,
  frmGridValueUnit, shlobj, activex, AnsiStrings, frmDisplayDataUnit,
  Mt3dmsChemSpeciesUnit, frmExportImageUnit, IOUtils,
  SwrReachObjectUnit, frmProgressUnit, Generics.Collections,
  frmBudgetPrecisionQueryUnit, ModflowBoundaryDisplayUnit, VectorDisplayUnit;

resourcestring
  StrHead = 'Head';
  StrTheFileCouldNotB = 'The file could not be read.' + sLineBreak + '"%s"';
  StrImportModelResults = 'import model results';
  StrTheNumberOfRowsOrColumns = 'The number of rows or columns in the data s' +
  'et doesn''t match the number of rows or columns in the grid for data set "%s".';
  StrFileIsEmpty = 'File is empty.';
  StrTheNumberOfLayersOrCol = 'The number of layers or columns in the data s' +
  'et doesn''t match the number of layers or columns in the grid for data set "%s".';
  StrTheNumberOfRows = 'The number of layers, rows, or columns in the data s' +
  'et doesn''t match the number of rows, columns, or layers in the grid for data set "%s".';
  StrTheNumberOfHydrogeologic = 'The number of rows, columns, or hydrogeolog' +
  'ic units in the data set doesn''t match the number of rows, columns, or h' +
  'ydrogeologic units in the grid.';
  StrReadFrom0sOnStress = 'read from: "%0:s" on %1:s'
    + sLineBreak + 'Stress Period: %2:d'
    + sLineBreak + 'Time Step: %3:d'
    + sLineBreak + 'Elapsed Time: %4:g'
    + sLineBreak + 'File last modified on: %5:s';
  StrLayer = 'Layer: ';
  StrSystem = 'System: ';
  StrChildLayer = 'Child Layer: ';
  StrChildSystem = 'Child System: ';
  StrAtLeastOneOfThe = 'At least one of the result files does not exist.';
  StrTheFileYouAreTry = 'The file you are trying to read appears to have mor' +
  'e simulated layers than does your model. Aborting data import.';
  StrMinimumValueG = 'Minimum value: %g';
  StrMaximumValueG = 'Maximum value: %g';
  StrModel = 'Model';
  StrImportData = 'Import Data';
  StrFileName = 'FileName';
  StrFormattedHeadFiles = 'Formatted head files';
  StrFormattedDrawdownF = 'Formatted drawdown files';
  StrBinaryHeadFiles = 'Binary head files';
  StrBinaryDrawdownFile = 'Binary drawdown files';
  StrBinaryFlowFiles = 'Binary flow files';
  StrFormattedHUFHeadF = 'Formatted HUF head files';
  StrBinaryHUFHeadFile = 'Binary HUF head files';
  StrHUFFlowFiles = 'HUF flow files';
  StrMT3DMSConcentration = 'MT3DMS Concentration file';
  StrCombinedSUBOutput = 'Combined SUB output file';
  StrCombinedSWTOutput = 'Combined SWT output file';
  StrSUBSubsidence = 'SUB Subsidence';
  StrSUBCompactionByMo = 'SUB Compaction by model layer';
  StrSUBCompactionByIn = 'SUB Compaction by interbed system';
  StrSUBVerticalDisplac = 'SUB Vertical displacement';
  StrSUBCriticalHeadFo = 'SUB Critical head for no-delay interbeds';
  StrSUBCriticalHeadFoDelay = 'SUB Critical head for delay interbeds';
  StrSWTSubsidence = 'SWT Subsidence';
  StrSWTCompactionByMo = 'SWT Compaction by model layer';
  StrSWTCompactionByIn = 'SWT Compaction by interbed system';
  StrSWTVerticalDisplac = 'SWT Vertical displacement';
  StrSWTPreconsolidation = 'SWT Preconsolidation stress';
  StrSWTChangeInPrecon = 'SWT Change in preconsolidation stress';
  StrSWTGeostaticStress = 'SWT Geostatic stress';
  StrSWTChangeInGeosta = 'SWT Change in geostatic stress';
  StrSWTEffectiveStress = 'SWT Effective stress';
  StrSWTChangeInEffect = 'SWT Change in effective stress';
  StrSWTVoidRatio = 'SWT Void ratio';
  StrSWTThicknessOfCom = 'SWT Thickness of compressible sediments';
  StrSWTLayercenterEle = 'SWT Layer-center elevation';
  StrCommonSupportedFil = 'Common supported file types|*';
  StrSubsidenceFiles = '|Subsidence files|*';
  Str0s1sPeriod2 = '%0:s%1:s: Period: %2:d; Step: %3:d';
  Str0sTransportStep = '%0:s; Transport Step: %1:d';
  Str0sTotalTime1 = '%0:s; Total Time: %1:g';
  StrSDoesNotExist = '%s does not exist.';
  StrSIsEmpty = '%s is empty.';
  StrErrorReadingS = 'Error reading %s.';
  StrWaterTable = 'Water Table';
  StrObject = '_Object';
  StrTheFileTypeMustB = 'The file type must be one of the file types recogni' +
  'zed by ModelMuse. The recognized file types are displayed in the "files o' +
  'f type" combo box in the "Open File" dialog box.';
  StrSWIZetaFiles = 'SWI Zeta Files';
  StrSWRASCIIStageFile = 'SWR ASCII Stage Files';
  StrSWRBinaryStageFil = 'SWR Binary Stage Files';
//  StrTheNumberOfReache = 'The number of reaches in the file does not match t' +
//  'he number of reaches in the model.';
  StrSWRTimeStep = 'SWR Time Step: ';
  StrSWRStage = 'SWR Stage';
  StrSWRBottomElevation = 'SWR Bottom Elevation';
  StrSWRDepth = 'SWR Reach Depth';
  StrSWRGroundwaterHead = 'SWR Groundwater Head';
  StrSWRWettedPerimeter = 'SWR Wetted Perimeter';
  StrSWRConductance = 'SWR Conductance';
  StrSWRCalculatedHead = 'SWR Calculated Head Difference';
  StrSWRAquiferReachFl = 'SWR Aquifer Reach Flow';
  StrSWRReachExchange = 'SWR_Reach_Exchange';
  StrSWRStageName = 'SWR_Stage';
  StrSWRReachGroupFlow = 'SWR_Reach_Group_Flows';
  StrSWRReachGroupStag = 'SWR Reach Group Stage';
  StrSWRReachGroupInfl = 'SWR Reach Group Inflow to Connected Reaches';
  StrSWRReachGroupLate = 'SWR Reach Group Lateral Flow';
  StrSWRReachGroupUZF = 'SWR Reach Group UZF Inflow';
  StrSWRReachGroupRain = 'SWR Reach Group Rain';
  StrSWRReachGroupEvap = 'SWR Reach Group Evaporation';
  StrSWRReachGroupAqui = 'SWR Reach Group Aquifer Reach Flow';
  StrSWRReachGroupOutf = 'SWR Reach Group Outflow to Connected Reaches';
  StrSWRReachGroupExte = 'SWR Reach Group External Flow';
  StrSWRReachGroupStru = 'SWR Reach Group Structure Flow';
  StrSWRReachGroupCons = 'SWR Reach Group Constant Reach Flow';
  StrSWRReachGroupVoluChange = 'SWR Reach Group Volume Change';
  StrSWRReachGroupFlowDisc = 'SWR Reach Group Flow Discrepancy';
  StrSWRReachGroupVolu = 'SWR Reach Group Volume';
  StrTheNumberOfReache = 'The number of reaches in the file isn''t the same ' +
  'as the number of reaches in the model.';
  StrUsuallyTheFileWil = 'Usually the file will be single precision so that ' +
  'is what we''ll try. However, if you encounter an error, the file may be ' +
  'double precision.';
  StrSinglePrecisionMea = 'Single precision means that each real number in t' +
  'he file is stored using 4 bytes. Double precision means that each real nu' +
  'mber is stored using 8 bytes. There are two versions of MODFLOW-2005 dist' +
  'ributed by the USGS. Mf2005.exe saves results in single precision format. ' +
  'Mf2005dbl.exe saves results in double precision format.';
  StrTheFileYouSelecte = 'The file you selected is empty.';
  StrThereWasAnErrorR = 'There was an error reading the file, the error mess' +
  'age was "%s".';
  StrThereWasAnErrorO = 'There was an error opening the file. The error mess' +
  'age was "%s".';
  StrInvalidData = 'There was an error reading some data from your resu' +
  'lts file.  The error message was "%s". Typically, this means that there i' +
  's an error in your model that you will need to fix.';
  StrSWRReachExchangeText = 'SWR Reach Exchange Text File';
  StrSWRReachExchangeBin = 'SWR Reach Exchange Binary File';
  StrSWRReachGroupWateText = 'SWR Reach Group Water Budget Text File';
  StrSWRReachGroupWateBin = 'SWR Reach Group Water Budget Binary File';
  StrElasticCompactionML = 'Elastic compaction by model layer';
  StrInelasticCompactionML = 'Inelastic compaction by model layer';
  StrElasticCompactionIB = 'Elastic compaction by interbed system';
  StrInelasticCompactionIb = 'Inelastic compaction by interbed system';
  StrErrorAttemptingTo = 'Error attempting to read HUF output when no HUF un' +
  'its have been defined.';
  StrTheFileYouAreTryFewer = 'The file you are trying to read appears to hav' +
  'e fewer simulated layers than does your model. Aborting data import.';
  StrTheFileYouAreTryDifferent = 'The file you are trying to read appears to have a d' +
  'ifferent number of simulated layers than does your model. Aborting data i' +
  'mport.';
  StrUZFRecharge = 'UZF Recharge';
  StrUZFDischarge = 'UZF Discharge';
  StrCSUBCompaction = 'CSUB Compaction';
  StrCSUBElasticCompact = 'CSUB Elastic Compaction';
  StrCSUBInlasticCompac = 'CSUB Inlastic Compaction';
  StrCSUBInterbedCompac = 'CSUB Interbed Compaction';
  StrCSUBCoarseCompacti = 'CSUB Coarse Compaction';
  StrCSUBZDisplacement = 'CSUB Z Displacement';
  StrImportedOnS = ' imported on %s';
  StrMT3DConcentrations = 'MT3D Concentrations can only be imported if MT3D ' +
  'is selected.';
  StrGWTConcentrationFi = 'GWT concentration files';

{$R *.dfm}

Function PaddedIntToStr(Value, MaxValue: integer): string;
var
  Index: Integer;
begin
  result := IntToStr(Value);
  for Index := Trunc(Log10(Value)) to Trunc(Log10(MaxValue)) - 1 do
  begin
    result := '0' + result;
  end;
end;

function PaddedFloatToStr(Value: double): string;
begin
  result := FloatToStr(Value);
  while Length(result) < 15 do
  begin
    result := '_' + result;
  end;
end;

{ TfrmSelectResultToImport }

function TfrmSelectResultToImport.GetPrefix: string;
begin
  if comboClassification.ItemIndex = 0 then
  begin
    result := '';
  end
  else
  begin
    result := Trim(edPrefix.Text);
  end;
end;


function TfrmSelectResultToImport.GetClassificationGroup: string;
begin
  case comboClassification.ItemIndex of
    0:
      begin
        result := StrModelResults;
      end;
    1:
      begin
        result := strDefaultClassification;
      end;
    else
      begin
        if Trim(comboClassification.Text) = '' then
        begin
          result := StrModelResults;
        end
        else
        begin
          result := Trim(comboClassification.Text);
        end;
      end;
  end;
end;

procedure TfrmSelectResultToImport.CreateOrRetrieveLayerDataSet(
  const Description: string; ILAY: integer;
  out LayerData: TDataArray; out OldComment: string;
  FileNames: string;
  AModel: TCustomModel; DataArrayType: TDataArrayType; Precision: TModflowPrecision;
  DataArrayForm: TDataArrayForm = dafLayer);
var
  NewName: string;
  CreateNewDataSet: Boolean;
  Index: Integer;
  ScreenObject: TScreenObject;
  NewDataSetPosition: Integer;
  DefaultName: string;
  ParentLayerData: TDataArray;
  Splitter: TStringList;
  FileDates: string;
  ClassificationGroup: string;
begin
  Splitter := TStringList.Create;
  try
    Splitter.Text := FileNames;
    for Index := 0 to Splitter.Count - 1 do
    begin
      Splitter[Index] := DateTimeToStr(TFile.GetLastWriteTime(Splitter[Index]))
    end;
    FileDates := Splitter.Text;
  finally
    Splitter.Free;
  end;
  NewName := GetPrefix + TitleCase(Description);
  NewName := NewName + '_P' + PaddedIntToStr(KPER, FMaxPeriod) +
    '_S' + PaddedIntToStr(KSTP, FMaxStep);
  if (FResultFormat = mfMt3dConc) and (Precision <> mpMf6Double) then
  begin
    NewName := NewName + '_TS' + PaddedIntToStr(NTRANS, FMaxTrans);
  end;
  NewName := ValidName(NewName);
  case DataArrayForm of
    dafLayer:
      begin
        NewName := NewName + '_L' + PaddedIntToStr(ILAY, FMaxLayer);
      end;
    dafSystem:
      begin
        NewName := NewName + '_Sys' + PaddedIntToStr(ILAY, FMaxLayer);
      end;
    dafSubsidence, dafWaterTable:
      begin
        // do nothing
      end;
    dafSwrStage:
      begin
        NewName := NewName + '_SWR_Step' + IntToStr(SwrTimeStep);
      end
    else
      Assert(False);
  end;
  DefaultName := NewName;
  NewDataSetPosition := FNewDefaultDataSetNames.IndexOf(NewName);
  if NewDataSetPosition >= 0 then
  begin
    NewName := FNewDataSetNames[NewDataSetPosition];
    CreateNewDataSet := False;
  end
  else
  begin
    CreateNewDataSet := True;
    if AModel.DataArrayManager.GetDataSetByName(NewName) <> nil then
    begin
      CreateNewDataSet := AskUserIfNewDataSet;
      if CreateNewDataSet then
      begin
        NewName := GenerateNewName(NewName, nil, '_');
      end;
    end;
  end;
  if CreateNewDataSet then
  begin
    FNewDefaultDataSetNames.Add(DefaultName);
    FNewDataSetNames.Add(NewName);
    LayerData := DataArrayType.Create(frmGoPhast.PhastModel);
    if LayerData is TModflowBoundaryDisplayDataArray then
    begin
      TModflowBoundaryDisplayDataArray(LayerData).AddMethod := vamAveragedDelayed;
    end;
    NewDataSets.Add(LayerData);
    LayerData.UpDateWithName(NewName);
//    LayerData.OnNameChange := frmGoPhast.PhastModel.DataArrayNameChange;
    frmGoPhast.PhastModel.AddDataSet(LayerData);
    LayerData.DataType := rdtDouble;
    LayerData.Orientation := dsoTop;
    if FResultFormat = mfMt3dConc then
    begin
      LayerData.Formula :=
        FortranFloatToStr(AModel.ModflowPackages.Mt3dBasic.InactiveConcentration);
    end
    else
    begin
      LayerData.Formula :=
        FortranFloatToStr(AModel.ModflowOptions.HNoFlow);
    end;
    LayerData.TwoInterpolatorClass := TLinearSfrpackInterpolator.ClassName;

    frmGoPhast.PhastModel.UpdateDataArrayDimensions(LayerData);
    ClassificationGroup := GetClassificationGroup;
    LayerData.EvaluatedAt := eaBlocks;
    case DataArrayForm of
      dafLayer: LayerData.Classification := ClassificationGroup + StrLayerData;
      dafSystem: LayerData.Classification := ClassificationGroup+ KSystem;
      dafSubsidence: LayerData.Classification := ClassificationGroup + StrLayerData;
      dafWaterTable: LayerData.Classification := ClassificationGroup + KWaterTable;
      dafSwrStage: LayerData.Classification := ClassificationGroup + KSwrStage;
      else Assert(False);
    end;

    LayerData.OnDataSetUsed := frmGoPhast.PhastModel.ModelResultsRequired;
    frmGoPhast.PhastModel.CreateVariables(LayerData);
    LayerData := AModel.DataArrayManager.GetDataSetByName(NewName);
//    Grid := AModel.ModflowGrid;
    AModel.UpdateDataArrayDimensions(LayerData);
//    LayerData.UpdateDimensions(Grid.LayerCount, Grid.RowCount,
//      Grid.ColumnCount);
  end
  else
  begin
    LayerData := AModel.DataArrayManager.GetDataSetByName(NewName);
//    Grid := AModel.ModflowGrid;
    AModel.UpdateDataArrayDimensions(LayerData);
//    LayerData.UpdateDimensions(Grid.LayerCount, Grid.RowCount,
//      Grid.ColumnCount);
    for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
    begin
      ScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
      if (ScreenObject.IndexOfDataSet(LayerData) >= 0)
        and ScreenObject.UsedModels.UsesModel(AModel) then
      begin
        ScreenObject.Deleted := True;
        ScreenObjectsToDelete.Add(ScreenObject);
      end;
    end;
  end;
  AdjustTotalTime(TOTIM);
  if LayerData.Model = frmGoPhast.PhastModel then
  begin
    ParentLayerData := LayerData;
  end
  else
  begin
    ParentLayerData := frmGoPhast.PhastModel.DataArrayManager.
      GetDataSetByName(LayerData.Name);
  end;
  OldComment := ParentLayerData.Comment;
  if LayerData = ParentLayerData then
  begin
    ParentLayerData.Comment := Format(StrReadFrom0sOnStress,
      [FileNames, DateTimeToStr(Now), KPER, KSTP, TOTIM,
      FileDates]);
    if FResultFormat = mfMt3dConc then
    begin
      ParentLayerData.Comment := ParentLayerData.Comment
        + sLineBreak + StrTransportStep + IntToStr(NTRANS)
    end;
    case DataArrayForm of
      dafLayer:
        begin
          ParentLayerData.Comment := ParentLayerData.Comment
            + sLineBreak + StrLayer + IntToStr(ILAY);
        end;
      dafSystem:
        begin
          ParentLayerData.Comment := ParentLayerData.Comment
            + sLineBreak + StrSystem + IntToStr(ILAY);
        end;
      dafSubsidence, dafWaterTable, dafSwrStage:
        begin
          // do nothing
        end;
      else
        Assert(False);
    end;
  end
  else
  begin
    if ParentLayerData.Comment = '' then
    begin
      ParentLayerData.Comment := Format(StrReadFrom0sOnStress,
        [FileNames, DateTimeToStr(Now), KPER, KSTP, TOTIM,
        FileDates]);
      if FResultFormat = mfMt3dConc then
      begin
        ParentLayerData.Comment := ParentLayerData.Comment
          + sLineBreak + StrTransportStep + IntToStr(NTRANS)
      end;
    end;
    case DataArrayForm of
      dafLayer:
        begin
          ParentLayerData.Comment := ParentLayerData.Comment
            + sLineBreak + StrChildLayer + IntToStr(ILAY);
        end;
      dafSystem:
        begin
          ParentLayerData.Comment := ParentLayerData.Comment
            + sLineBreak + StrChildSystem + IntToStr(ILAY);
        end;
      dafSubsidence, dafWaterTable:
        begin
          // do nothing
        end;
      dafSwrStage:
        begin
          ParentLayerData.Comment := ParentLayerData.Comment
            + sLineBreak + StrSWRTimeStep + IntToStr(SwrTimeStep);
        end;

      else
        Assert(False);
    end;
  end;
  if FModifiedParentDataSets.IndexOf(ParentLayerData) < 0 then
  begin
    FModifiedParentDataSets.Add(ParentLayerData);
  end;
end;

procedure TfrmSelectResultToImport.CreateSwrReachExchangeScreenObject(
  AModel: TCustomModel; var ScreenObject: TScreenObject; Root: string;
  SwrWriter: TModflowSwrWriter; NewCreateScreenObjects: TList;
  AReachExchange, PriorReachExchange: TReachExchange; FileName: string);
var
  Grid: TModflowGrid;
  ExistingObjectCount: Integer;
  NeedToCreate: Boolean;
  ItemIndex: Integer;
  UndoCreateScreenObject: TCustomUndo;
  ReachIndex: Integer;
  ReachNumber: Integer;
  AReach: TReachObject;
  AReachScreenObject: TScreenObject;
  ALayer: Integer;
  Elevation: double;
  ElevValues: TValueArrayStorage;
  UndoCreateObject: TUndoCreateScreenObject;
begin
  if PriorReachExchange = nil then
  begin
    NeedToCreate := True;
  end
  else
  begin
    NeedToCreate := AReachExchange.ReachNumbers.Count <> PriorReachExchange.ReachNumbers.Count;
    if not NeedToCreate then
    begin
      for ItemIndex := 0 to AReachExchange.ReachNumbers.Count - 1 do
      begin
        NeedToCreate :=
          (AReachExchange.ReachNumbers[ItemIndex] <> PriorReachExchange.ReachNumbers[ItemIndex])
          or (AReachExchange.ModelLayers[ItemIndex] <> PriorReachExchange.ModelLayers[ItemIndex]);
        if NeedToCreate then
        begin
          break;
        end;
      end;
    end;
  end;
  if NeedToCreate then
  begin
    Grid := AModel.ModflowGrid;
    ScreenObject := TScreenObject.CreateWithViewDirection(
      frmGoPhast.PhastModel, vdTop,
      UndoCreateScreenObject, False);
    ScreenObject.Comment := 'Imported from ' + FileName +' on ' + DateTimeToStr(Now);
    frmGoPhast.PhastModel.AddScreenObject(ScreenObject);
    ScreenObject.ElevationCount := ecOne;
    ScreenObject.SetValuesOfIntersectedCells := True;

    Root := Root + StrObject;
    ExistingObjectCount := frmGoPhast.PhastModel.
      NumberOfLargestScreenObjectsStartingWith(Root);
    Inc(ExistingObjectCount);
    ScreenObject.Name := Root + IntToStr(ExistingObjectCount);

    if frmGoPhast.PhastModel.LgrUsed then
    begin
      ScreenObject.UsedModels.UsedWithAllModels := False;
      ScreenObject.UsedModels.AddModel(AModel);
    end;

    ScreenObject.EvaluatedAt := eaBlocks;
    ScreenObject.Visible := False;
    ScreenObject.Capacity := AReachExchange.ReachNumbers.Count;
    ElevValues := TValueArrayStorage.Create;
    try
      ElevValues.Count := AReachExchange.ReachNumbers.Count;


      for ReachIndex := 0 to AReachExchange.ReachNumbers.Count - 1 do
      begin
        ReachNumber := AReachExchange.ReachNumbers[ReachIndex];
        AReach := SwrWriter.Reaches[ReachNumber];
        AReachScreenObject := AReach.FReachData.ScreenObject as TScreenObject;
        if AReachScreenObject.Count = 1 then
        begin
          ScreenObject.AddPoint(AReachScreenObject.Points[0], True);
        end
        else
        begin
          ScreenObject.AddPoint(Grid.TwoDElementCenter(
            AReach.FReachData.Cell.Column, AReach.FReachData.Cell.Row), True);
        end;

        ALayer := AReachExchange.ModelLayers[ReachIndex];
        ALayer := AModel.ModflowLayerToDataSetLayer(ALayer);
        Elevation := Grid.LayerCenter(AReach.FReachData.Cell.Column,
          AReach.FReachData.Cell.Row, ALayer);
        ElevValues.RealValues[ReachIndex] := Elevation;

      end;
      ScreenObject.ImportedSectionElevations := ElevValues;
      ScreenObject.ImportedSectionElevations.CacheData;
      ScreenObject.ElevationFormula := rsObjectImportedValuesR
        + '("' + StrImportedElevations + '")';
    finally
      ElevValues.Free;
    end;
    UndoCreateObject := TUndoCreateScreenObject.Create(ScreenObject);
    UndoCreateObject.UpdateObservations;
    NewCreateScreenObjects.Add(UndoCreateObject);
  end;
end;

procedure TfrmSelectResultToImport.CreateSwrReachGroupFlowScreenObject(
  AModel: TCustomModel; out ScreenObject: TScreenObject; Root: string;
  SwrWriter: TModflowSwrWriter; FileName: string);
var
  ReachIndex: Integer;
  AReach: TReachObject;
  UndoCreateScreenObject: TCustomUndo;
  AReachScreenObject: TScreenObject;
  Grid: TModflowGrid;
  ExistingObjectCount: Integer;
begin
  Grid := AModel.ModflowGrid;
  ScreenObject := TScreenObject.CreateWithViewDirection(
    frmGoPhast.PhastModel, vdTop,
    UndoCreateScreenObject, False);
  ScreenObject.Comment := 'Imported from ' + FileName +' on ' + DateTimeToStr(Now);
  frmGoPhast.PhastModel.AddScreenObject(ScreenObject);
  ScreenObject.ElevationCount := ecZero;
  ScreenObject.SetValuesOfIntersectedCells := True;

  Root := Root + StrObject;
  ExistingObjectCount := frmGoPhast.PhastModel.
    NumberOfLargestScreenObjectsStartingWith(Root);
  Inc(ExistingObjectCount);
  ScreenObject.Name := Root + IntToStr(ExistingObjectCount);


  if frmGoPhast.PhastModel.LgrUsed then
  begin
    ScreenObject.UsedModels.UsedWithAllModels := False;
    ScreenObject.UsedModels.AddModel(AModel);
  end;

  // Don't SetValuesByInterpolation because it causes
  // inactive cells to be assigned inappropriate values.
  // This causes such cells to be colored when the grid is colored
  // with the data set.
//  ScreenObject.SetValuesByInterpolation := True;
  ScreenObject.EvaluatedAt := eaBlocks;
  ScreenObject.Visible := False;
  ScreenObject.Capacity := SwrWriter.ReachCount;
  for ReachIndex := 0 to SwrWriter.ReachCount - 1 do
  begin
    AReach := SwrWriter.Reaches[ReachIndex];
    AReachScreenObject := AReach.FReachData.ScreenObject as TScreenObject;
    if AReachScreenObject.Count = 1 then
    begin
      ScreenObject.AddPoint(AReachScreenObject.Points[0], True);
    end
    else
    begin
      ScreenObject.AddPoint(Grid.TwoDElementCenter(
        AReach.FReachData.Cell.Column, AReach.FReachData.Cell.Row), True);
    end;
  end;
end;

procedure TfrmSelectResultToImport.CreateSwrStageScreenObject(AModel: TCustomModel;
  out ScreenObject: TScreenObject; Root: string; AFileName: string);
var
  SwrWriter: TModflowSwrWriter;
begin
  SwrWriter := TModflowSwrWriter.Create(AModel, etDisplay);
  try
    frmProgressMM.ShouldContinue := True;
    SwrWriter.UpdateReachNumberDisplay;

    CreateSwrReachGroupFlowScreenObject(AModel, ScreenObject, Root, SwrWriter, AFileName);

  finally
    SwrWriter.Free;
  end;
end;

procedure TfrmSelectResultToImport.CreateScreenObject(LayerIndex: integer;
  AModel: TCustomModel; out ScreenObject: TScreenObject; FileName: string);
var
  RowIndex: Integer;
  ColIndex: Integer;
  UndoCreateScreenObject: TCustomUndo;
  Grid: TModflowGrid;
  ActiveDataSet: TDataArray;
  LI: Integer;
  DisvGrid: TModflowDisvGrid;
  RowCount: Integer;
  ColumnCount: Integer;
begin
  if FResultFormat = mfMt3dConc then
  begin
    if not AModel.Mt3dMS_StrictUsed(nil) then
    begin
      Beep;
      MessageDlg(StrMT3DConcentrations, mtError, [mbOK], 0);
      Exit;
    end;
    ActiveDataSet := AModel.DataArrayManager.GetDataSetByName(StrMT3DMSActive);
  end
  else
  begin
    ActiveDataSet := AModel.DataArrayManager.GetDataSetByName(rsActive);
  end;
//  ActiveDataSet := AModel.DataArrayManager.GetDataSetByName(rsActive);
  ActiveDataSet.Initialize;

  if AModel.DisvUsed then
  begin
    Grid := nil;
    DisvGrid := AModel.DisvGrid;
  end
  else
  begin
    Grid := AModel.ModflowGrid;
    DisvGrid := nil;
  end;
  ScreenObject := TScreenObject.CreateWithViewDirection(
    frmGoPhast.PhastModel, vdTop,
    UndoCreateScreenObject, False);
  ScreenObject.Comment := 'Imported from ' + FileName +' on ' + DateTimeToStr(Now);
  frmGoPhast.PhastModel.AddScreenObject(ScreenObject);
  ScreenObject.ElevationCount := ecZero;
  ScreenObject.SetValuesOfIntersectedCells := True;

  if frmGoPhast.PhastModel.LgrUsed then
  begin
    ScreenObject.UsedModels.UsedWithAllModels := False;
    ScreenObject.UsedModels.AddModel(AModel);
  end;

  // Don't SetValuesByInterpolation because it causes
  // inactive cells to be assigned inappropriate values.
  // This causes such cells to be colored when the grid is colored
  // with the data set.
//  ScreenObject.SetValuesByInterpolation := True;
  ScreenObject.EvaluatedAt := eaBlocks;
  ScreenObject.Visible := False;
  if Grid <> nil then
  begin
    RowCount := Grid.RowCount;
    ColumnCount := Grid.ColumnCount;
  end
  else
  begin
    RowCount := DisvGrid.RowCount;
    ColumnCount := DisvGrid.ColumnCount;
  end;
  ScreenObject.Capacity := RowCount * ColumnCount;
  for RowIndex := 0 to RowCount - 1 do
  begin
    for ColIndex := 0 to ColumnCount - 1 do
    begin
      if LayerIndex >= 0 then
      begin
        if ActiveDataSet.BooleanData[LayerIndex, RowIndex, ColIndex] then
        begin
          if Grid <> nil then
          begin
            ScreenObject.AddPoint(
              Grid.TwoDElementCenter(ColIndex, RowIndex), True);
          end
          else
          begin
            ScreenObject.AddPoint(
              DisvGrid.TwoDElementCenter(ColIndex, RowIndex), True);
          end;
        end;
      end
      else
      begin
        for LI := 0 to ActiveDataSet.LayerCount - 1 do
        begin
          if ActiveDataSet.BooleanData[LI, RowIndex, ColIndex] then
          begin
            if Grid <> nil then
            begin
              ScreenObject.AddPoint(
                Grid.TwoDElementCenter(ColIndex, RowIndex), True);
            end
            else
            begin
              ScreenObject.AddPoint(
                DisvGrid.TwoDElementCenter(ColIndex, RowIndex), True);
            end;
            break;
          end;
        end;
      end;
    end;
  end;
end;

function TfrmSelectResultToImport.DefaultFileName(AModel: TCustomModel; Directory: string = ''): string;
var
  ModelDir: string;
//var
//  ModelDir: String;
begin
  Assert(frmGoPhast.PhastModel.ModelSelection in ModflowSelection);
  result := AModel.DefaultModflowOutputFileName;
  if Directory <> '' then
  begin
    ModelDir := SysUtils.ExtractFileDir(result);
    if ModelDir <> Directory then
    begin
      result := IncludeTrailingPathDelimiter(Directory) + SysUtils.ExtractFileName(result)
    end;
  end;
  if not FileExists(result) then
  begin
    result := '';
  end;
end;

procedure TfrmSelectResultToImport.AddModelRow(AModel: TCustomModel; ARow: integer; AFileName: string);
begin
  rdgModels.Objects[Ord(mcModelName), ARow] := AModel;
  rdgModels.Cells[Ord(mcModelName), ARow] := AModel.DisplayName;
  rdgModels.Cells[Ord(mcFileName), ARow] := AFileName;
  rdgModels.Checked[Ord(mcUse), ARow] := FileExists(AFileName);
end;

function TfrmSelectResultToImport.ShowDataSets(AFileName: string): boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
  Extension: string;
  ADir: string;
begin
  FMaxTrans := -1;
  try
    Screen.Cursor := crHourGlass;
    rdgModels.BeginUpdate;
    try
      FItemDescriptions.Clear;
      FItemTimes.Clear;

      if frmGoPhast.PhastModel.LgrUsed then
      begin
        rdgModels.RowCount := 2 + frmGoPhast.PhastModel.ChildModels.Count;
      end
      else
      begin
        rdgModels.RowCount := 2;
        rdgModels.Visible := False;
        splitData.Visible := False;
      end;
      ADir := SysUtils.ExtractFileDir(AFileName);
      AddModelRow(frmGoPhast.PhastModel, 1,  AFileName);
      result := ReadDataHeadings(frmGoPhast.PhastModel, {1,} AFileName);
      if result and frmGoPhast.PhastModel.LgrUsed then
      begin
        Extension := ExtractFileExt(AFileName);
        for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
        begin
          ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
          AFileName := DefaultFileName(ChildModel, ADir);
          if AFileName <> '' then
          begin
            AFileName := ChangeFileExt(AFileName, Extension);
          end;
          AddModelRow(ChildModel, ChildIndex + 2,  AFileName);
          if FileExists(AFileName) then
          begin
            result := ReadDataHeadings(ChildModel, {ChildIndex + 2,}  AFileName);
            if not result then
            begin
              Exit;
            end;
          end
          else
          begin

          end;
        end;
      end;
      if (FItemDescriptions.Count = 1) or (FItemTimes.Count <= 1) then
      begin
        pnlMultiSelect.Visible := False;
        splMultiSelect.Visible := False;
      end
      else
      begin
        clDescription.Items := FItemDescriptions;
        clTime.Items := FItemTimes;
      end;

    finally
      rdgModels.EndUpdate;
      Screen.Cursor := crDefault;
    end;
  except on E: EInOutError do
    begin
      result := False;
      Beep;
      MessageDlg(Format(StrTheFileCouldNotB,
        [E.message]), mtError, [mbOK], 0);
    end;
  end
end;


function TfrmSelectResultToImport.SelectFiles: boolean;
var
  AFileName: string;
begin
  odSelectFiles.FileName := DefaultFileName(frmGoPhast.PhastModel);
  SetCurrentDir(SysUtils.ExtractFileDir(odSelectFiles.FileName));
  result := odSelectFiles.Execute;
  if result then
  begin
    AFileName := odSelectFiles.FileName;
    try
      result := ShowDataSets(AFileName);
    except
      on E: EInOutError do
      begin
        result := False;
        Beep;
        MessageDlg(Format(StrTheFileCouldNotB,
          [E.message]), mtError, [mbOK], 0);
      end;
      on E: EHufReadError do
      begin
        result := False;
        Beep;
        MessageDlg(Format(StrTheFileCouldNotB,
          [E.message]), mtError, [mbOK], 0);
      end;
    end;
  end;
end;

function TfrmSelectResultToImport.ImportSwrStages(AModel: TCustomModel;
  AFileName: string; ILAY: Integer; FileNames: string; LastItem: Integer;
  OldComments: TStringList; Precision: TModflowPrecision): Boolean;
var
  SwrStages: TSwrTimeStages;
  SwrItem: TSwrTimeStage;
  SwrFileType: TSwrFileType;
  UndoCreateObject: TUndoCreateScreenObject;
  SwrIndex: Integer;
  OldComment: string;
  Count: Integer;
  ScreenObject: TScreenObject;
  LayerData: TDataArray;
  MinMaxAssigned: Boolean;
begin
  Result := True;
  CreateSwrStageScreenObject(AModel, ScreenObject, StrSWRStageName, AFileName);
  SwrStages := TSwrTimeStages.Create;
  try
    if FResultFormat = mfSwrStageAscii then
    begin
      SwrFileType := srtAscii;
    end
    else
    begin
      Assert(FResultFormat = mfSwrStageBinary);
      SwrFileType := srtBinary;
    end;
    ReadSwrTimeStageData(AFileName, SwrFileType, SwrStages);
    if SwrStages.NumberOfReaches <> ScreenObject.Count then
    begin
      Result := False;
      Exit;
    end;
    Count := 0;
    for SwrIndex := 0 to LastItem do
    begin
      if clData.Checked[SwrIndex] then
      begin
        SwrItem := SwrStages[SwrIndex];
        KPER := SwrItem.StressPeriod;
        KSTP := SwrItem.ModflowTimeStep;
        SwrTimeStep := SwrItem.SwrTimeStep;
        TOTIM := SwrItem.TotalTime;
        CreateOrRetrieveLayerDataSet(StrSWRStage, ILAY, LayerData, OldComment,
          FileNames, AModel, TModflowBoundaryDisplayDataArray, Precision, dafSwrStage);
        AssignSwrStageValues(ScreenObject, LayerData, SwrItem.Stages, AModel,
          MinMaxAssigned);
        UpdateOldComments(OldComments, LayerData, OldComment);
        DataSetNames.AddObject(LayerData.Name, LayerData);
        if MinMaxAssigned then
        begin
          MinValues.Add(LayerData.Limits.LowerLimit.RealLimitValue);
          MaxValues.Add(LayerData.Limits.UpperLimit.RealLimitValue);
        end;
        Inc(Count);
        comboColorGrid.Items.Objects[Count] := LayerData;
      end;
    end;
    UndoCreateObject := TUndoCreateScreenObject.Create(ScreenObject);
    UndoCreateObject.UpdateObservations;
    NewCreateScreenObjects.Add(UndoCreateObject);
  finally
    SwrStages.Free;
  end;
end;

procedure TfrmSelectResultToImport.ImportSwrReachGroupFlows(AModel: TCustomModel;
  AFileName: string; ILAY: Integer; FileNames: string; LastItem: Integer;
  OldComments: TStringList; Precision: TModflowPrecision);
const
  MaxSwrReachReachGroupFlowsDataTypes = 14;
var
  Count: Integer;
  SwrWriter: TModflowSwrWriter;
  ScreenObject: TScreenObject;
  SwrFileType: TSwrFileType;
  SwrIndex: Integer;
  ReachGroupWaterBudgets: TReachGroupWaterBudgets;
  AWaterBudget: TReachGroupWaterBudget;
  SwrDataType: Integer;
  Description: string;
  LayerData: TDataArray;
  OldComment: string;
  MinMaxAssigned: boolean;
begin
  Count := 0;
  SwrWriter := TModflowSwrWriter.Create(AModel, etDisplay);
  try
    frmProgressMM.ShouldContinue := True;
    SwrWriter.UpdateReachNumberDisplay;

    CreateSwrReachGroupFlowScreenObject(AModel,
      ScreenObject, StrSWRReachGroupFlow,
      SwrWriter, AFileName);

    if FResultFormat = mfSwrReachGroupBudgetAscii then
    begin
      SwrFileType := srtAscii;
    end
    else
    begin
      Assert(FResultFormat = mfSwrReachGroupBudgetBinary);
      SwrFileType := srtBinary;
    end;

    ReachGroupWaterBudgets := TReachGroupWaterBudgets.Create;
    try
      ReadSwrReachGroupWaterBudgetData(AFileName, SwrFileType,
        ReachGroupWaterBudgets);

      for SwrIndex := 0 to LastItem do
      begin
        if clData.Checked[SwrIndex] then
        begin
          AWaterBudget := ReachGroupWaterBudgets[
            SwrIndex div MaxSwrReachReachGroupFlowsDataTypes];

          KPER := AWaterBudget.StressPeriod;
          KSTP := AWaterBudget.ModflowTimeStep;
          SwrTimeStep := AWaterBudget.SwrTimeStep;
          TOTIM := AWaterBudget.TotalTime;

          SwrDataType := SwrIndex mod MaxSwrReachReachGroupFlowsDataTypes;
          case SwrDataType of
            0: Description := StrSWRReachGroupStag;
            1: Description := StrSWRReachGroupInfl;
            2: Description := StrSWRReachGroupLate;
            3: Description := StrSWRReachGroupUZF;
            4: Description := StrSWRReachGroupRain;
            5: Description := StrSWRReachGroupEvap;
            6: Description := StrSWRReachGroupAqui;
            7: Description := StrSWRReachGroupOutf;
            8: Description := StrSWRReachGroupExte;
            9: Description := StrSWRReachGroupStru;
            10: Description := StrSWRReachGroupCons;
            11: Description := StrSWRReachGroupVoluChange;
            12: Description := StrSWRReachGroupFlowDisc;
            13: Description := StrSWRReachGroupVolu;
            else Assert(False);
          end;

          CreateOrRetrieveLayerDataSet(Description, ILAY, LayerData, OldComment,
            FileNames, AModel, TModflowBoundaryDisplayDataArray, Precision, dafSwrStage);

          AssignSwrWaterBudgetValues(ScreenObject, LayerData,
            ValuesToIgnore, AWaterBudget,
            AModel, MinMaxAssigned,
            SwrDataType, SwrWriter);

          UpdateOldComments(OldComments, LayerData, OldComment);
          DataSetNames.AddObject(LayerData.Name, LayerData);
          if MinMaxAssigned then
          begin
            MinValues.Add(LayerData.Limits.LowerLimit.RealLimitValue);
            MaxValues.Add(LayerData.Limits.UpperLimit.RealLimitValue);
          end;
          Inc(Count);
          comboColorGrid.Items.Objects[Count] := LayerData;
        end;
      end;

    finally
      ReachGroupWaterBudgets.Free;
    end;
  finally
    SwrWriter.Free;
  end;

end;

function TfrmSelectResultToImport.ImportSwrReachExchange(AModel: TCustomModel;
  AFileName: string; ILAY: Integer; FileNames: string; LastItem: Integer;
  OldComments: TStringList; Precision: TModflowPrecision): boolean;
const
  MaxSwrReachExchangeDataTypes = 8;
var
  Count: Integer;
  AReachExchange: TReachExchange;
  SwrWriter: TModflowSwrWriter;
  PriorReachExchange: TReachExchange;
  SwrDataType: Integer;
  ReachExchanges: TReachExchanges;
  SwrIndex: Integer;
  OldComment: string;
  SwrFileType: TSwrFileType;
  ScreenObject: TScreenObject;
  LayerData: TDataArray;
  Description: string;
  MinMaxAssigned: Boolean;
  ReachNumberIndex: Integer;
  AReachNumber: Integer;
begin
  Count := 0;
  SwrWriter := TModflowSwrWriter.Create(AModel, etDisplay);
  try
    frmProgressMM.ShouldContinue := True;
    SwrWriter.UpdateReachNumberDisplay;
    if FResultFormat = mfSwrReachExchangeAscii then
    begin
      SwrFileType := srtAscii;
    end
    else
    begin
      Assert(FResultFormat = mfSwrReachExchangeBinary);
      SwrFileType := srtBinary;
    end;
    ReachExchanges := TReachExchanges.Create;
    try
      ReadSwrReachExchangeData(AFileName, SwrFileType, ReachExchanges);
      Assert(ReachExchanges.Count > 0);
      AReachExchange := ReachExchanges[0];
      result := True;
      for ReachNumberIndex := 0 to AReachExchange.ReachNumbers.Count - 1 do
      begin
        AReachNumber := AReachExchange.ReachNumbers[ReachNumberIndex];
        if AReachNumber >= SwrWriter.ReachCount then
        begin
          result := false;
          Exit;
        end;
      end;

      PriorReachExchange := nil;
      ScreenObject := nil;
      for SwrIndex := 0 to LastItem do
      begin
        if clData.Checked[SwrIndex] then
        begin
          AReachExchange := ReachExchanges[SwrIndex div MaxSwrReachExchangeDataTypes];
          CreateSwrReachExchangeScreenObject(AModel, ScreenObject,
            StrSWRReachExchange, SwrWriter, NewCreateScreenObjects,
            AReachExchange, PriorReachExchange, AFileName);
          KPER := AReachExchange.StressPeriod;
          KSTP := AReachExchange.ModflowTimeStep;
          SwrTimeStep := AReachExchange.SwrTimeStep;
          TOTIM := AReachExchange.TotalTime;
          SwrDataType := SwrIndex mod MaxSwrReachExchangeDataTypes;
          case SwrDataType of
            0:
              Description := StrSWRBottomElevation;
            1:
              Description := StrSWRStage;
            2:
              Description := StrSWRDepth;
            3:
              Description := StrSWRGroundwaterHead;
            4:
              Description := StrSWRWettedPerimeter;
            5:
              Description := StrSWRConductance;
            6:
              Description := StrSWRCalculatedHead;
            7:
              Description := StrSWRAquiferReachFl;
          else
            Assert(False);
          end;
          CreateOrRetrieveLayerDataSet(Description, ILAY, LayerData, OldComment,
            FileNames, AModel, TModflowBoundaryDisplayDataArray, Precision, dafSwrStage);
          LayerData.Orientation := dso3D;
          LayerData.TwoInterpolatorClass := '';
          AModel.UpdateDataArrayDimensions(LayerData);
          AssignSwrReachExchangeValues(ScreenObject, LayerData, ValuesToIgnore,
            AReachExchange, AModel, MinMaxAssigned, SwrDataType);
          UpdateOldComments(OldComments, LayerData, OldComment);
          DataSetNames.AddObject(LayerData.Name, LayerData);
          if MinMaxAssigned then
          begin
            MinValues.Add(LayerData.Limits.LowerLimit.RealLimitValue);
            MaxValues.Add(LayerData.Limits.UpperLimit.RealLimitValue);
          end;
          Inc(Count);
          comboColorGrid.Items.Objects[Count] := LayerData;
          PriorReachExchange := AReachExchange;
        end;
      end;
    finally
      ReachExchanges.Free;
    end;
  finally
    SwrWriter.Free;
  end;
end;

procedure TfrmSelectResultToImport.AdjustSwiNames(var NewName: string);
begin
  if NewName = 'Swiaddtoch' then
  begin
    NewName := 'SwiAddToCH';
  end
  else if NewName = 'Swiaddtoflf' then
  begin
    NewName := 'SwiAddToFLF';
  end
  else if NewName = 'Swiaddtofrf' then
  begin
    NewName := 'SwiAddToFRF';
  end
  else if NewName = 'Swiaddtofff' then
  begin
    NewName := 'SwiAddToFFF';
  end;
end;

procedure TfrmSelectResultToImport.UpdateOldComments(OldComments: TStringList;
  ADataArray: TDataArray; OldComment: string);
var
  ParentDataArray: TDataArray;
begin
  if ADataArray.Model = frmGoPhast.PhastModel then
  begin
    ParentDataArray := ADataArray;
  end
  else
  begin
    ParentDataArray := frmGoPhast.PhastModel.
      DataArrayManager.GetDataSetByName(ADataArray.Name);
  end;
  if OldComments.IndexOfObject(ParentDataArray) < 0 then
  begin
    OldComments.AddObject(OldComment, ParentDataArray);
  end;
end;

procedure TfrmSelectResultToImport.AssignSwrWaterBudgetValues(
  ScreenObject: TScreenObject; LayerData: TDataArray;
  ValuesToIgnore: TOneDRealArray; SwrData: TReachGroupWaterBudget;
  AModel: TCustomModel; out MinMaxAssigned: boolean;
  SwrDataType: integer; SwrWriter: TModflowSwrWriter);
var
  DataSetIndex: Integer;
  ParentDataSet: TDataArray;
  ImportedValues: TValueArrayItem;
  MinValue: double;
  MaxValue: double;
  Temp: double;
  ItemIndex: Integer;
  ParentLayerData: TDataArray;
  IgnoreIndex: Integer;
  SkipReal: TSkipReal;
  ReachGroup: Integer;
  ReachIndex: Integer;
  AReach: TReachObject;
begin
  if AModel is TPhastModel then
  begin
    DataSetIndex := ScreenObject.AddDataSet(LayerData);
  end
  else
  begin
    ParentDataSet := frmGoPhast.PhastModel.DataArrayManager.
      GetDataSetByName(LayerData.Name);
    DataSetIndex := ScreenObject.AddDataSet(ParentDataSet);
  end;
  ScreenObject.DataSetFormulas[DataSetIndex] := rsObjectImportedValuesR
    + '("' + LayerData.Name + '")';

  ScreenObject.ImportedValues.Add;
  ImportedValues := ScreenObject.ImportedValues.Items[ScreenObject.ImportedValues.Count-1];
  ImportedValues.Values.DataType := rdtDouble;
  ImportedValues.Values.Count := ScreenObject.Count;
  ImportedValues.Name := LayerData.Name;
  MinValue := 0;
  MaxValue := 0;
  MinMaxAssigned := False;

  for ItemIndex := 0 to SwrData.ReachGroupNumbers.Count - 1 do
  begin

    Temp := SwrData.Value[SwrDataType, ItemIndex];
    ReachGroup := SwrData.ReachGroupNumbers[ItemIndex];

    for ReachIndex := 0 to SwrWriter.ReachCount - 1 do
    begin
      AReach := SwrWriter.Reaches[ReachIndex];
      if AReach.FReachData.ReachGroup = ReachGroup then
      begin
        ImportedValues.Values.RealValues[ReachIndex] := Temp;
        if not MinMaxAssigned then
        begin
          MinValue := Temp;
          MaxValue := Temp;
          MinMaxAssigned := True;
        end
        else
        begin
          if MinValue > Temp then
          begin
            MinValue := Temp;
          end
          else if MaxValue < Temp then
          begin
            MaxValue := Temp;
          end;
        end;
      end;
    end;

  end;

  ImportedValues.CacheData;
  ParentLayerData := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(LayerData.Name);
  if ParentLayerData = LayerData then
  begin
    ParentLayerData.Limits.LowerLimit.UseLimit := True;
    ParentLayerData.Limits.LowerLimit.RealLimitValue := MinValue;
    ParentLayerData.Limits.UpperLimit.UseLimit := True;
    ParentLayerData.Limits.UpperLimit.RealLimitValue := MaxValue;
    ParentLayerData.Limits.Update;
    ParentLayerData.Limits.RealValuesToSkip.Clear;
    for IgnoreIndex := 0 to Length(ValuesToIgnore) - 1 do
    begin
      SkipReal := ParentLayerData.Limits.RealValuesToSkip.Add as TSkipReal;
      SkipReal.RealValue := ValuesToIgnore[IgnoreIndex];
    end;
    ParentLayerData.ContourLimits := ParentLayerData.Limits;
  end
  else
  begin
    LayerData.Limits.LowerLimit.RealLimitValue := MinValue;
    LayerData.Limits.UpperLimit.RealLimitValue := MaxValue;
    ParentLayerData.Limits.LowerLimit.RealLimitValue :=
      Min(MinValue, ParentLayerData.Limits.LowerLimit.RealLimitValue);
    ParentLayerData.Limits.UpperLimit.RealLimitValue :=
      Max(MaxValue, ParentLayerData.Limits.UpperLimit.RealLimitValue);
    for IgnoreIndex := 0 to Length(ValuesToIgnore) - 1 do
    begin
      if ParentLayerData.Limits.RealValuesToSkip.IndexOf(ValuesToIgnore[IgnoreIndex]) < 0 then
      begin
        SkipReal := ParentLayerData.Limits.RealValuesToSkip.Add as TSkipReal;
        SkipReal.RealValue := ValuesToIgnore[IgnoreIndex];
      end;
    end;
    ParentLayerData.ContourLimits := ParentLayerData.Limits;
  end;
end;

procedure TfrmSelectResultToImport.AssignSwrReachExchangeValues(
  ScreenObject: TScreenObject; LayerData: TDataArray;
  ValuesToIgnore: TOneDRealArray; SwrData: TReachExchange;
  AModel: TCustomModel; out MinMaxAssigned: boolean;
  SwrDataType: integer);
var
  DataSetIndex: Integer;
  ParentDataSet: TDataArray;
  ImportedValues: TValueArrayItem;
  MinValue: double;
  MaxValue: double;
  Temp: double;
  ItemIndex: Integer;
  ParentLayerData: TDataArray;
  IgnoreIndex: Integer;
  SkipReal: TSkipReal;
begin
  if AModel is TPhastModel then
  begin
    DataSetIndex := ScreenObject.AddDataSet(LayerData);
  end
  else
  begin
    ParentDataSet := frmGoPhast.PhastModel.DataArrayManager.
      GetDataSetByName(LayerData.Name);
    DataSetIndex := ScreenObject.AddDataSet(ParentDataSet);
  end;
  ScreenObject.DataSetFormulas[DataSetIndex] := rsObjectImportedValuesR
    + '("' + LayerData.Name + '")';

  ScreenObject.ImportedValues.Add;
  ImportedValues := ScreenObject.ImportedValues.Items[ScreenObject.ImportedValues.Count-1];
  ImportedValues.Values.DataType := rdtDouble;
  ImportedValues.Values.Count := SwrData.ReachNumbers.Count;
  ImportedValues.Name := LayerData.Name;
  MinValue := 0;
  MaxValue := 0;
  MinMaxAssigned := False;

  for ItemIndex := 0 to SwrData.ReachNumbers.Count - 1 do
  begin
    Temp := SwrData.Value[SwrDataType, ItemIndex];
    ImportedValues.Values.RealValues[ItemIndex] := Temp;
    if not MinMaxAssigned then
    begin
      MinValue := Temp;
      MaxValue := Temp;
      MinMaxAssigned := True;
    end
    else
    begin
      if MinValue > Temp then
      begin
        MinValue := Temp;
      end
      else if MaxValue < Temp then
      begin
        MaxValue := Temp;
      end;
    end;
  end;

  ImportedValues.CacheData;
  ParentLayerData := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(LayerData.Name);
  if ParentLayerData = LayerData then
  begin
    ParentLayerData.Limits.LowerLimit.UseLimit := True;
    ParentLayerData.Limits.LowerLimit.RealLimitValue := MinValue;
    ParentLayerData.Limits.UpperLimit.UseLimit := True;
    ParentLayerData.Limits.UpperLimit.RealLimitValue := MaxValue;
    ParentLayerData.Limits.Update;
    ParentLayerData.Limits.RealValuesToSkip.Clear;
    for IgnoreIndex := 0 to Length(ValuesToIgnore) - 1 do
    begin
      SkipReal := ParentLayerData.Limits.RealValuesToSkip.Add as TSkipReal;
      SkipReal.RealValue := ValuesToIgnore[IgnoreIndex];
    end;
    ParentLayerData.ContourLimits := ParentLayerData.Limits;
  end
  else
  begin
    LayerData.Limits.LowerLimit.RealLimitValue := MinValue;
    LayerData.Limits.UpperLimit.RealLimitValue := MaxValue;
    ParentLayerData.Limits.LowerLimit.RealLimitValue :=
      Min(MinValue, ParentLayerData.Limits.LowerLimit.RealLimitValue);
    ParentLayerData.Limits.UpperLimit.RealLimitValue :=
      Max(MaxValue, ParentLayerData.Limits.UpperLimit.RealLimitValue);
    for IgnoreIndex := 0 to Length(ValuesToIgnore) - 1 do
    begin
      if ParentLayerData.Limits.RealValuesToSkip.IndexOf(ValuesToIgnore[IgnoreIndex]) < 0 then
      begin
        SkipReal := ParentLayerData.Limits.RealValuesToSkip.Add as TSkipReal;
        SkipReal.RealValue := ValuesToIgnore[IgnoreIndex];
      end;
    end;
    ParentLayerData.ContourLimits := ParentLayerData.Limits;
  end;
end;

procedure TfrmSelectResultToImport.AssignSwrStageValues(
  ScreenObject: TScreenObject; LayerData: TDataArray;
  SwrData: TDoubleDynArray;
  AModel: TCustomModel; out MinMaxAssigned: boolean);
var
  ImportedValues: TValueArrayItem;
  ItemIndex: Integer;
  DataSetIndex: Integer;
  ParentDataSet: TDataArray;
  MinValue: double;
  MaxValue: double;
  Temp: double;
  ParentLayerData: TDataArray;
  IgnoreIndex: Integer;
  SkipReal: TSkipReal;
begin
  if AModel is TPhastModel then
  begin
    DataSetIndex := ScreenObject.AddDataSet(LayerData);
  end
  else
  begin
    ParentDataSet := frmGoPhast.PhastModel.DataArrayManager.
      GetDataSetByName(LayerData.Name);
    DataSetIndex := ScreenObject.AddDataSet(ParentDataSet);
  end;
  ScreenObject.DataSetFormulas[DataSetIndex] := rsObjectImportedValuesR
    + '("' + LayerData.Name + '")';

  ScreenObject.ImportedValues.Add;
  ImportedValues := ScreenObject.ImportedValues.Items[ScreenObject.ImportedValues.Count-1];
  ImportedValues.Values.DataType := rdtDouble;
  ImportedValues.Values.Count := Length(SwrData);
  ImportedValues.Name := LayerData.Name;
  MinValue := 0;
  MaxValue := 0;
  MinMaxAssigned := False;
  for ItemIndex := 0 to Length(SwrData) - 1 do
  begin
    Temp := SwrData[ItemIndex];
    ImportedValues.Values.RealValues[ItemIndex] := Temp;
    if not MinMaxAssigned then
    begin
      MinValue := Temp;
      MaxValue := Temp;
      MinMaxAssigned := True;
    end
    else
    begin
      if MinValue > Temp then
      begin
        MinValue := Temp;
      end
      else if MaxValue < Temp then
      begin
        MaxValue := Temp;
      end;
    end;
  end;

  ImportedValues.CacheData;
  ParentLayerData := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(LayerData.Name);
  if ParentLayerData = LayerData then
  begin
    ParentLayerData.Limits.LowerLimit.UseLimit := True;
    ParentLayerData.Limits.LowerLimit.RealLimitValue := MinValue;
    ParentLayerData.Limits.UpperLimit.UseLimit := True;
    ParentLayerData.Limits.UpperLimit.RealLimitValue := MaxValue;
    ParentLayerData.Limits.Update;
    ParentLayerData.Limits.RealValuesToSkip.Clear;
    for IgnoreIndex := 0 to Length(ValuesToIgnore) - 1 do
    begin
      SkipReal := ParentLayerData.Limits.RealValuesToSkip.Add as TSkipReal;
      SkipReal.RealValue := ValuesToIgnore[IgnoreIndex];
    end;
    ParentLayerData.ContourLimits := ParentLayerData.Limits;
  end
  else
  begin
    LayerData.Limits.LowerLimit.RealLimitValue := MinValue;
    LayerData.Limits.UpperLimit.RealLimitValue := MaxValue;
    ParentLayerData.Limits.LowerLimit.RealLimitValue :=
      Min(MinValue, ParentLayerData.Limits.LowerLimit.RealLimitValue);
    ParentLayerData.Limits.UpperLimit.RealLimitValue :=
      Max(MaxValue, ParentLayerData.Limits.UpperLimit.RealLimitValue);
    for IgnoreIndex := 0 to Length(ValuesToIgnore) - 1 do
    begin
      if ParentLayerData.Limits.RealValuesToSkip.IndexOf(ValuesToIgnore[IgnoreIndex]) < 0 then
      begin
        SkipReal := ParentLayerData.Limits.RealValuesToSkip.Add as TSkipReal;
        SkipReal.RealValue := ValuesToIgnore[IgnoreIndex];
      end;
    end;
    ParentLayerData.ContourLimits := ParentLayerData.Limits;
  end;
end;

procedure TfrmSelectResultToImport.AssignValues(LayerIndex: integer;
  ScreenObject: TScreenObject; LayerData: TDataArray;
  AnArray: TModflowDoubleArray; ValuesToIgnore: TOneDRealArray;
  AModel: TCustomModel; out MinMaxAssigned: boolean);
var
  RowIndex: Integer;
  ColIndex: Integer;
  DataSetIndex: Integer;
  ImportedValues: TValueArrayItem;
  PointIndex: Integer;
  Grid: TModflowGrid;
  ActiveDataSet: TDataArray;
  Temp: TModflowFloat;
  MinValue, MaxValue: TModflowFloat;
  ShouldCheck: Boolean;
  LI: Integer;
  ShouldIgnore: Boolean;
  IgnoreIndex: Integer;
  SkipReal: TSkipReal;
  ParentDataSet: TDataArray;
  ParentLayerData: TDataArray;
  DisvGrid: TModflowDisvGrid;
  RowCount: Integer;
  ColumnCount: Integer;
begin
  if FResultFormat = mfMt3dConc then
  begin
    if not AModel.Mt3dMS_StrictUsed(nil) then
    begin
      Beep;
      MessageDlg(StrMT3DConcentrations, mtError, [mbOK], 0);
      Exit;
    end;
    ActiveDataSet := AModel.DataArrayManager.GetDataSetByName(StrMT3DMSActive);
  end
  else
  begin
    ActiveDataSet := AModel.DataArrayManager.GetDataSetByName(rsActive);
  end;
  ActiveDataSet.Initialize;

  if AModel.DisvUsed then
  begin
//    Grid := nil;
    DisvGrid := AModel.DisvGrid;
    Assert(DisvGrid <> nil);
    RowCount := DisvGrid.RowCount;
    ColumnCount := DisvGrid.ColumnCount;
  end
  else
  begin
    Grid := AModel.ModflowGrid;
    Assert(Grid <> nil);
//    DisvGrid := nil;
    RowCount := Grid.RowCount;
    ColumnCount := Grid.ColumnCount;
  end;
  if AModel is TPhastModel then
  begin
    DataSetIndex := ScreenObject.AddDataSet(LayerData);
  end
  else
  begin
    ParentDataSet := frmGoPhast.PhastModel.DataArrayManager.
      GetDataSetByName(LayerData.Name);
    DataSetIndex := ScreenObject.AddDataSet(ParentDataSet);
  end;
  ScreenObject.DataSetFormulas[DataSetIndex] := rsObjectImportedValuesR
    + '("' + LayerData.Name + '")';

  AssignObjectName(ScreenObject, LayerData);

  ScreenObject.ImportedValues.Add;
  ImportedValues := ScreenObject.ImportedValues.Items[0];
  ImportedValues.Values.DataType := rdtDouble;
  ImportedValues.Values.Count := RowCount * ColumnCount;
  ImportedValues.Name := LayerData.Name;
  PointIndex := 0;
  MinValue := 0;
  MaxValue := 0;
  MinMaxAssigned := False;
  Assert(AnArray <> nil);
  Assert(Length(AnArray) = RowCount);
  Assert(Length(AnArray[0]) = ColumnCount);
  for RowIndex := 0 to RowCount - 1 do
  begin
    for ColIndex := 0 to ColumnCount - 1 do
    begin
      ShouldCheck := False;
      if LayerIndex >= 0 then
      begin
        ShouldCheck := ActiveDataSet.BooleanData[LayerIndex, RowIndex, ColIndex];
      end
      else
      begin
        for LI := 0 to ActiveDataSet.LayerCount - 1 do
        begin
          if ActiveDataSet.BooleanData[LI, RowIndex, ColIndex] then
          begin
            ShouldCheck := True;
            break;
          end;
        end;
      end;
      if ShouldCheck then
      begin
        Temp:= AnArray[RowIndex, ColIndex];
        ImportedValues.Values.RealValues[PointIndex] := Temp;
        Inc(PointIndex);
        GetShouldIgnore(ValuesToIgnore, Temp, ShouldIgnore);

        if not ShouldIgnore then
        begin
          if not MinMaxAssigned then
          begin
            MinValue := Temp;
            MaxValue := Temp;
            MinMaxAssigned := True;
          end
          else
          begin
            if MinValue > Temp then
            begin
              MinValue := Temp;
            end
            else if MaxValue < Temp then
            begin
              MaxValue := Temp;
            end;
          end;
        end;
      end;
    end;
  end;
  ImportedValues.Values.Count := PointIndex;
  ImportedValues.CacheData;
  ParentLayerData := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(LayerData.Name);
  if ParentLayerData = LayerData then
  begin
    ParentLayerData.Limits.LowerLimit.UseLimit := True;
    ParentLayerData.Limits.LowerLimit.RealLimitValue := MinValue;
    ParentLayerData.Limits.UpperLimit.UseLimit := True;
    ParentLayerData.Limits.UpperLimit.RealLimitValue := MaxValue;
    ParentLayerData.Limits.Update;
    ParentLayerData.Limits.RealValuesToSkip.Clear;
    for IgnoreIndex := 0 to Length(ValuesToIgnore) - 1 do
    begin
      SkipReal := ParentLayerData.Limits.RealValuesToSkip.Add as TSkipReal;
      SkipReal.RealValue := ValuesToIgnore[IgnoreIndex];
    end;
    ParentLayerData.ContourLimits := ParentLayerData.Limits;
  end
  else
  begin
    LayerData.Limits.LowerLimit.RealLimitValue := MinValue;
    LayerData.Limits.UpperLimit.RealLimitValue := MaxValue;
    ParentLayerData.Limits.LowerLimit.RealLimitValue :=
      Min(MinValue, ParentLayerData.Limits.LowerLimit.RealLimitValue);
    ParentLayerData.Limits.UpperLimit.RealLimitValue :=
      Max(MaxValue, ParentLayerData.Limits.UpperLimit.RealLimitValue);
    for IgnoreIndex := 0 to Length(ValuesToIgnore) - 1 do
    begin
      if ParentLayerData.Limits.RealValuesToSkip.IndexOf(ValuesToIgnore[IgnoreIndex]) < 0 then
      begin
        SkipReal := ParentLayerData.Limits.RealValuesToSkip.Add as TSkipReal;
        SkipReal.RealValue := ValuesToIgnore[IgnoreIndex];
      end;
    end;
    ParentLayerData.ContourLimits := ParentLayerData.Limits;
  end;
end;

procedure TfrmSelectResultToImport.Assign3DValues(ScreenObject: TScreenObject;
  LayerData: TDataArray; AnArray: T3DTModflowArray; ModflowLayerIndex, DataSetLayerIndex: integer;
  CheckAllLayers: boolean; ValuesToIgnore: TOneDRealArray; AModel: TCustomModel);
var
  RowIndex: Integer;
  ColIndex: Integer;
  DataSetIndex: Integer;
  ImportedValues: TValueArrayItem;
  PointIndex: Integer;
  Grid: TModflowGrid;
  MinMaxAssigned: boolean;
  Temp: TModflowFloat;
  MinValue, MaxValue: TModflowFloat;
  ActiveDataSet: TDataArray;
  ShouldCheck: Boolean;
  LI: Integer;
  ShouldIgnore: Boolean;
  ParentLayerData: TDataArray;
  DisvGrid: TModflowDisvGrid;
  RowCount: Integer;
  ColumnCount: Integer;
begin
  AssignObjectName(ScreenObject, LayerData);

  ActiveDataSet := AModel.DataArrayManager.GetDataSetByName(rsActive);
  ActiveDataSet.Initialize;

  if AModel.DisvUsed then
  begin
//    Grid := nil;
    DisvGrid := AModel.DisvGrid;
    RowCount := DisvGrid.RowCount;
    ColumnCount := DisvGrid.ColumnCount;
  end
  else
  begin
//    DisvGrid := nil;
    Grid := AModel.ModflowGrid;
    RowCount := Grid.RowCount;
    ColumnCount := Grid.ColumnCount;
  end;
  DataSetIndex := ScreenObject.AddDataSet(LayerData);
  ScreenObject.DataSetFormulas[DataSetIndex] := rsObjectImportedValuesR
    + '("' + LayerData.Name + '")';
  ScreenObject.ImportedValues.Add;
  ImportedValues := ScreenObject.ImportedValues.Items[0];
  ImportedValues.Values.DataType := rdtDouble;
  ImportedValues.Values.Count := RowCount * ColumnCount;
  ImportedValues.Name := LayerData.Name;
  PointIndex := 0;
  MinValue := 0;
  MaxValue := 0;
  MinMaxAssigned := False;
  for RowIndex := 0 to RowCount - 1 do
  begin
    for ColIndex := 0 to ColumnCount - 1 do
    begin
      ShouldCheck := False;
      if not CheckAllLayers then
      begin
        ShouldCheck := ActiveDataSet.BooleanData[DataSetLayerIndex, RowIndex, ColIndex];
      end
      else
      begin
        for LI := 0 to ActiveDataSet.LayerCount - 1 do
        begin
          if ActiveDataSet.BooleanData[LI, RowIndex, ColIndex] then
          begin
            ShouldCheck := True;
            break;
          end;
        end;
      end;
      if ShouldCheck then
      begin
        Temp := AnArray[ModflowLayerIndex, RowIndex, ColIndex];
        ImportedValues.Values.RealValues[PointIndex] := Temp;
        Inc(PointIndex);
        GetShouldIgnore(ValuesToIgnore, Temp, ShouldIgnore);

        if not ShouldIgnore then
        begin
          if not MinMaxAssigned then
          begin
            MinValue := Temp;
            MaxValue := Temp;
            MinMaxAssigned := True;
          end
          else
          begin
            if MinValue > Temp then
            begin
              MinValue := Temp;
            end
            else if MaxValue < Temp then
            begin
              MaxValue := Temp;
            end;
          end;
        end;
      end;
    end;
  end;
  ImportedValues.Values.Count := PointIndex;
  ImportedValues.CacheData;
  LayerData.Limits.LowerLimit.UseLimit := True;
  LayerData.Limits.LowerLimit.RealLimitValue := MinValue;
  LayerData.Limits.UpperLimit.UseLimit := True;
  LayerData.Limits.UpperLimit.RealLimitValue := MaxValue;
  ParentLayerData := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(LayerData.Name);
  if ParentLayerData <> LayerData then
  begin
    ParentLayerData.Limits.LowerLimit.RealLimitValue :=
      Min(MinValue, ParentLayerData.Limits.LowerLimit.RealLimitValue);
    ParentLayerData.Limits.UpperLimit.RealLimitValue := MaxValue;
      Max(MaxValue, ParentLayerData.Limits.UpperLimit.RealLimitValue);
  end;
  ParentLayerData.Limits.Update;
end;

procedure TfrmSelectResultToImport.CreateOrRetrieve3DDataSet(Description: string;
  NTRANS, KPER, KSTP: integer; TOTIM: TModflowDouble; LayerNumbers: TIntegerList;
  LayerDataSets: TList; out New3DArray: TDataArray; out OldComment: string; FluxData: boolean;
  NewDataSets: TList; FileNames: string; AModel: TCustomModel; Precision: TModflowPrecision);
var
  NewName: string;
  NewFormula: string;
  LayerIndex: Integer;
  LayerPosition: Integer;
  DataArray: TDataArray;
  Grid: TModflowGrid;
  CreateNewDataSet: Boolean;
  DefaultName: string;
  NamePosition: Integer;
  Assigner: TFormulaAssigner;
  Parent3DArray: TDataArray;
  Splitter: TStringList;
  FileTimes: string;
  index: Integer;
begin
  Splitter := TStringList.Create;
  try
    Splitter.Text := FileNames;
    for index := 0 to Splitter.Count - 1 do
    begin
      Splitter[index] := DateTimeToStr(TFile.GetLastWriteTime(Splitter[index]))
    end;
    FileTimes := Splitter.Text;
  finally
    Splitter.Free;
  end;
  NewName := GetPrefix + TitleCase(Description);
  AdjustSwiNames(NewName);
  NewName := NewName + '_P' + PaddedIntToStr(KPER, FMaxPeriod) +
    '_S' + PaddedIntToStr(KSTP, FMaxStep);
  if (FResultFormat = mfMt3dConc) and (Precision <> mpMf6Double) then
  begin
    NewName := NewName + '_TS' + PaddedIntToStr(NTRANS, FMaxTrans);
  end;
  NewName := ValidName(NewName);

  DefaultName := NewName;
  NamePosition := FNewDefaultDataSetNames.IndexOf(DefaultName);
  if NamePosition >= 0 then
  begin
    NewName := FNewDataSetNames[NamePosition];
    CreateNewDataSet := False;
  end
  else
  begin
    CreateNewDataSet := True;
    if AModel.DataArrayManager.GetDataSetByName(NewName) <> nil then
    begin
      CreateNewDataSet := AskUserIfNewDataSet;
      if CreateNewDataSet then
      begin
        NewName := GenerateNewName(NewName, nil, '_');
      end;
    end;
  end;
//  Grid := AModel.ModflowGrid;
  if CreateNewDataSet then
  begin
    FNewDefaultDataSetNames.Add(DefaultName);
    FNewDataSetNames.Add(NewName);
    New3DArray := TDataArray.Create(frmGoPhast.PhastModel);
    NewDataSets.Add(New3DArray);
    New3DArray.UpDateWithName(NewName);
//    New3DArray.OnNameChange := frmGoPhast.PhastModel.DataArrayNameChange;
    New3DArray.DataType := rdtDouble;
    New3DArray.Orientation := dso3D;
    frmGoPhast.PhastModel.AddDataSet(New3DArray);
//    Grid := frmGoPhast.PhastModel.ModflowGrid;
    frmGoPhast.PhastModel.UpdateDataArrayDimensions(New3DArray);
//    New3DArray.UpdateDimensions(Grid.LayerCount, Grid.RowCount,
//      Grid.ColumnCount);
    New3DArray.EvaluatedAt := eaBlocks;
    New3DArray.Classification := GetClassificationGroup + StrThreeDData;
    New3DArray.OnDataSetUsed := AModel.ModelResultsRequired;
    frmGoPhast.PhastModel.CreateVariables(New3DArray);

    New3DArray := AModel.DataArrayManager.GetDataSetByName(NewName);
    Grid := AModel.ModflowGrid;
    AModel.UpdateDataArrayDimensions(New3DArray);
//    New3DArray.UpdateDimensions(Grid.LayerCount, Grid.RowCount,
//      Grid.ColumnCount);
  end
  else
  begin
    New3DArray := AModel.DataArrayManager.GetDataSetByName(NewName);
    New3DArray.Orientation := dso3D;
    Grid := AModel.ModflowGrid;
    AModel.UpdateDataArrayDimensions(New3DArray);
//    New3DArray.UpdateDimensions(Grid.LayerCount, Grid.RowCount,
//      Grid.ColumnCount);
  end;
  AdjustTotalTime(TOTIM);
  if New3DArray.Model = frmGoPhast.PhastModel then
  begin
    Parent3DArray := New3DArray;
  end
  else
  begin
    Parent3DArray := frmGoPhast.PhastModel.DataArrayManager.
      GetDataSetByName(New3DArray.Name);
  end;
  OldComment := Parent3DArray.Comment;
  if (Parent3DArray = New3DArray) or (Parent3DArray.Comment = '') then
  begin
    Parent3DArray.Comment := Format(StrReadFrom0sOnStress,
      [FileNames, DateTimeToStr(Now), KPER, KSTP, TOTIM,
      FileTimes]);
    if FResultFormat = mfMt3dConc then
    begin
      Parent3DArray.Comment := Parent3DArray.Comment
        + sLineBreak + StrTransportStep + IntToStr(NTRANS)
    end;
  end;
  if Grid.LayerCount = 1 then
  begin
    LayerPosition := LayerNumbers.IndexOf(1);
    Assert(LayerPosition = 0);
    DataArray := LayerDataSets[LayerPosition];
    NewFormula := DataArray.Name;
  end
  else
  begin
    NewFormula := 'CaseR(Layer, ';
    for LayerIndex := 1 to Grid.LayerCount do
    begin
      LayerPosition := LayerNumbers.IndexOf(LayerIndex);
      if LayerPosition >= 0 then
      begin
        DataArray := LayerDataSets[LayerPosition];
        NewFormula := NewFormula + DataArray.Name;
        if LayerIndex < Grid.LayerCount then
        begin
          NewFormula := NewFormula + ', ';
        end;
      end
      else
      begin
        if FluxData then
        begin
          NewFormula := NewFormula + '0, ';
        end
        else
        begin
          LayerPosition := LayerNumbers.IndexOf(LayerIndex-1);
          Assert(LayerPosition >= 0);
          DataArray := LayerDataSets[LayerPosition];
          NewFormula := NewFormula + '((' + DataArray.Name + ' + ';

          LayerPosition := LayerNumbers.IndexOf(LayerIndex+1);
          Assert(LayerPosition >= 0);
          DataArray := LayerDataSets[LayerPosition];
          NewFormula := NewFormula + DataArray.Name + ') / 2.), ';
        end;
      end;
    end;
    NewFormula := NewFormula + ')';
    NewFormula := 'If(((Layer > 0) and (Layer <= ' + IntToStr(Grid.LayerCount)
      + ')), ' + NewFormula + ', 0)'
  end;
  Assigner := FFormulaAssigners[Parent3DArray];
  Assigner.AddFormula(NewFormula, AModel);
//  New3DArray.Formula := NewFormula;
  LayerDataSets.Clear;
  LayerNumbers.Clear;
  if FModifiedParentDataSets.IndexOf(Parent3DArray) < 0 then
  begin
    FModifiedParentDataSets.Add(Parent3DArray);
  end;
end;

procedure TfrmSelectResultToImport.btnOKClick(Sender: TObject);
var
  RowIndex: Integer;
  DisplayChoice: TDisplayChoice;
begin
  inherited;
  for RowIndex := 1 to rdgModels.RowCount - 1 do
  begin
    if rdgModels.Checked[Ord(mcUse), RowIndex]
      and not FileExists(rdgModels.Cells[Ord(mcFileName), RowIndex]) then
    begin
      Beep;
      MessageDlg(StrAtLeastOneOfThe, mtError, [mbOK], 0);
      ModalResult := mrNone;
      rdgModels.Row := RowIndex;
      rdgModels.Col := Ord(mcUse);
      Exit;
    end;
  end;

  Screen.Cursor := crHourGlass;
  try
    SetData;

    DisplayChoice := TDisplayChoice(rgDisplayChoice.ItemIndex);
    Inc(DisplayChoices[DisplayChoice]);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmSelectResultToImport.btnSelectAllClick(Sender: TObject);
begin
  inherited;
  FSettingAll := True;
  try
    clData.CheckAll;
    clDescription.ApplyState(cbChecked, False);
    clTime.ApplyState(cbChecked, False);
  finally
    FSettingAll := False;
  end;
  UpdateCombo;
end;

procedure TfrmSelectResultToImport.btnSelectNoneClick(Sender: TObject);
begin
  inherited;
  FSettingAll := True;
  try
    clData.UnCheckAll;
    clDescription.ApplyState(cbUnChecked, False);
    clTime.ApplyState(cbUnChecked, False);
  finally
    FSettingAll := False;
  end;
  UpdateCombo;
end;

procedure TfrmSelectResultToImport.AssignLimits( MinValues, MaxValues: TRealList;
  New3DArray: TDataArray; ValuesToIgnore: TOneDRealArray);
var
  IgnoreIndex: Integer;
  SkipReal: TSkipReal;
  ParentArray: TDataArray;
begin
  MinValues.Sort;
  MaxValues.Sort;
  ParentArray := frmGoPhast.PhastModel.DataArrayManager.
    GetDataSetByName(New3DArray.Name);
  if ParentArray = New3DArray then
  begin
    if MinValues.Count > 0 then
    begin
      ParentArray.Limits.LowerLimit.UseLimit := True;
      ParentArray.Limits.LowerLimit.RealLimitValue := MinValues[0];
      ParentArray.Limits.UpperLimit.UseLimit := True;
      ParentArray.Limits.UpperLimit.RealLimitValue :=
        MaxValues[MaxValues.Count -1];
    end;
    ParentArray.Limits.RealValuesToSkip.Clear;
    for IgnoreIndex := 0 to Length(ValuesToIgnore) - 1 do
    begin
      SkipReal := ParentArray.Limits.RealValuesToSkip.Add as TSkipReal;
      SkipReal.RealValue := ValuesToIgnore[IgnoreIndex];
    end;
    ParentArray.ContourLimits := ParentArray.Limits;
  end
  else
  begin
    if MinValues.Count > 0 then
    begin
      if ParentArray.Limits.LowerLimit.UseLimit then
      begin
        ParentArray.Limits.LowerLimit.RealLimitValue :=
          Min(MinValues[0], ParentArray.Limits.LowerLimit.RealLimitValue);
      end
      else
      begin
        ParentArray.Limits.LowerLimit.UseLimit := True;
        ParentArray.Limits.LowerLimit.RealLimitValue := MinValues[0];
      end;
      if ParentArray.Limits.UpperLimit.UseLimit then
      begin
        ParentArray.Limits.UpperLimit.RealLimitValue :=
          Max(MaxValues[MaxValues.Count -1], ParentArray.Limits.UpperLimit.RealLimitValue);
      end
      else
      begin
        ParentArray.Limits.UpperLimit.UseLimit := True;
        ParentArray.Limits.UpperLimit.RealLimitValue := MaxValues[MaxValues.Count -1];
      end;
    end;
    for IgnoreIndex := 0 to Length(ValuesToIgnore) - 1 do
    begin
      if ParentArray.Limits.RealValuesToSkip.IndexOf(ValuesToIgnore[IgnoreIndex]) < 0 then
      begin
        SkipReal := ParentArray.Limits.RealValuesToSkip.Add as TSkipReal;
        SkipReal.RealValue := ValuesToIgnore[IgnoreIndex];
      end;
    end;
    ParentArray.ContourLimits := ParentArray.Limits;
  end;
  ParentArray.Limits.Update;
  MinValues.Clear;
  MaxValues.Clear;
end;

procedure MoveArrayLeft(var A3DArray: T3DTModflowArray);
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  FirstCol: Integer;
begin
  for LayerIndex := 0 to Length(A3DArray) - 1 do
  begin
    for RowIndex := 0 to Length(A3DArray[0]) - 1 do
    begin
      for ColIndex := Length(A3DArray[0][0]) - 2 downto 0 do
      begin
        A3DArray[LayerIndex,RowIndex,ColIndex+1] := A3DArray[LayerIndex,RowIndex,ColIndex];
      end;
    end;
  end;
  FirstCol := 0;
  for LayerIndex := 0 to Length(A3DArray) - 1 do
  begin
    for RowIndex := 0 to Length(A3DArray[0]) - 1 do
    begin
        A3DArray[LayerIndex,RowIndex,FirstCol] := 0;
    end;
  end;
end;

procedure MoveArrayForward(var A3DArray: T3DTModflowArray);
var
  LayerIndex: Integer;
  RowIndex: Integer;
  FirstRow: Integer;
  ColIndex: Integer;
begin
  for LayerIndex := 0 to Length(A3DArray) - 1 do
  begin
    for RowIndex := Length(A3DArray[0]) - 2 downto 0 do
    begin
      A3DArray[LayerIndex,RowIndex+1] := A3DArray[LayerIndex,RowIndex];
    end;
    SetLength(A3DArray[LayerIndex][0], Length(A3DArray[0][0]));
  end;
  FirstRow := 0;
  for LayerIndex := 0 to Length(A3DArray) - 1 do
  begin
    for ColIndex := 0 to Length(A3DArray[0][0]) - 1 do
    begin
      A3DArray[LayerIndex,FirstRow,ColIndex] := 0;
    end;
  end;
end;

procedure MoveArrayUp(var A3DArray: T3DTModflowArray);
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  FirstLayer: Integer;
begin
  for LayerIndex := Length(A3DArray) - 2 downto 0 do
  begin
    A3DArray[LayerIndex+1] := A3DArray[LayerIndex];
  end;
  FirstLayer := 0;
  SetLength(A3DArray[FirstLayer] , Length(A3DArray[0]), Length(A3DArray[0][0]));
  for RowIndex := 0 to Length(A3DArray[0]) - 1 do
  begin
    for ColIndex := 0 to Length(A3DArray[0][0]) -1 do
    begin
      A3DArray[FirstLayer,RowIndex,ColIndex] := 0;
    end;
  end;
end;

procedure NegateArray(var A3DArray: T3DTModflowArray);
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  for LayerIndex := 0 to Length(A3DArray) - 1 do
  begin
    for RowIndex := 0 to Length(A3DArray[0]) - 1 do
    begin
      for ColIndex := 0 to Length(A3DArray[0][0]) - 1 do
      begin
        A3DArray[LayerIndex,RowIndex,ColIndex] := -A3DArray[LayerIndex,RowIndex,ColIndex]
      end;
    end;
  end;
end;

procedure TfrmSelectResultToImport.SetData;
var
  Index: Integer;
  AnArray: TModflowDoubleArray;
  WaterTableArray: TModflowDoubleArray;
  LayerArray: TModflowDoubleArray;
  EndReached: Boolean;
  LayerData: TDataArray;
  Description: string;
  ScreenObject: TScreenObject;
  LayerNumbers: TIntegerList;
  AuxLayerNumbersList: TObjectList<TIntegerList>;
  LayerDataSets: TList;
  AuxLayerDataSetsList: TObjectList<TList>;
  Count: integer;
  New3DArray: TDataArray;
  A3DArray: T3DTModflowArray;
  LayerIndex: Integer;
  NLAY: Integer;
  UndoImportResults : TUndoImportModelResults;
  UndoChangeDataSets: TUndoChangeDataSets;
  DeletedDataSets: TList;
  NewDataSetProperties : TObjectList;
  DataArray: TDataArray;
  DataStorage: TPhastDataSetStorage;
  UndoDeleteScreenObjects: TUndoDeleteScreenObjects;
  OldComments: TStringList;
  OldComment: string;
  ColIndex: Integer;
  Precision: TModflowPrecision;
  HufFormat: boolean;
  HGU: THydrogeologicUnit;
  LayerDescription: string;
  MinMaxAssigned: Boolean;
  DisplayChoice: TDisplayChoice;
  AModel: TCustomModel;
  AFileName: string;
  LastItem: Integer;
  RowIndex: Integer;
  ParentArray: TDataArray;
  FileNames: string;
  AParentArray: TDataArray;
  UndoCreateObject: TUndoCreateScreenObject;
  Mt3dComponentName: string;
  LastCharIndex: Integer;
  CharIndex: Integer;
  ALabel: string;
  CheckIndex: integer;
  ILAY: Integer;
  PriorILay: Integer;
  LayerCount: Integer;
  AuxArray: TAuxArrays;
  AuxIndex: Integer;
  AuxDescription: string;
  AuxLayerNumbers: TIntegerList;
  AuxLayerDataSets: TList;
  AuxMinValuesList: TObjectList<TRealList>;
  AuxMaxValuesList: TObjectList<TRealList>;
  AuxMinValues: TRealList;
  AuxMaxValues: TRealList;
  AltDescription: string;
  AltMinValues: TRealList;
  AltMaxValues: TRealList;
  AltOldComments: TStringList;
  AltDataSetNames: TStringList;
  AltLayerNumbers: TIntegerList;
  AltLayerDataSets: TList;
  Alt3DArray: T3DTModflowArray;
  Vectors: TVectorCollection;
  VectorItem: TVectorItem;
  UndoItem: TCustomUndo;
  ModelName1: string;
  ModelName2: string;
  PackageName1: string;
  PackageName2: string;
begin
  inherited;
  ModelName1 :='';
  ModelName2 :='';
  PackageName1 :='';
  PackageName2 :='';
  ILAY := 0;
  NTRANS := 0;
  SwrTimeStep := 0;
  FileNames := '';
  Mt3dComponentName := '';
  FModifiedParentDataSets.Clear;
  MinValues := TRealList.Create;
  AltMinValues := TRealList.Create;
  MaxValues := TRealList.Create;
  AltMaxValues := TRealList.Create;
  NewDataSets := TList.Create;
  OldComments := TStringList.Create;
  AltOldComments := TStringList.Create;
  DataSetNames := TStringList.Create;
  AltDataSetNames  := TStringList.Create;
  ScreenObjectsToDelete := TScreenObjectList.Create;
  NewCreateScreenObjects := TList.Create;
  for RowIndex := 1 to rdgModels.RowCount - 1 do
  begin
    if rdgModels.Checked[Ord(mcUse), RowIndex] then
    begin
      AFileName := rdgModels.Cells[Ord(mcFileName), RowIndex];
      Assert(FileExists(AFileName));
      if FileNames <> '' then
      begin
        FileNames := FileNames + sLineBreak;
      end;
      FileNames := FileNames + AFileName;
      if (FResultFormat = mfMt3dConc) and (Mt3dComponentName = '') then
      begin
        AFileName := ChangeFileExt(SysUtils.ExtractFileName(AFileName), '');
        LastCharIndex := Length(AFileName);
        if (LastCharIndex > 1) and (Copy(AFileName, LastCharIndex-1, 2) = '_S') then
        begin
          LastCharIndex := LastCharIndex -2;
        end;
        for CharIndex := LastCharIndex downto 1 do
        begin
          if AFileName[CharIndex] = '_' then
          begin
            Mt3dComponentName := ' ' + Copy(AFileName, CharIndex+1, MAXINT);
            break;
          end;
        end;
      end;
    end;
  end;
  try
    try
      FMaxLayer := 0;
      for RowIndex := 1 to rdgModels.RowCount - 1 do
      begin
        if rdgModels.Checked[Ord(mcUse), RowIndex] then
        begin
          AModel := rdgModels.Objects[Ord(mcModelName), RowIndex] as TCustomModel;
          FMaxLayer := Max(FMaxLayer, AModel.ModflowLayerCount);
        end;
      end;
      for RowIndex := 1 to rdgModels.RowCount - 1 do
      begin
        if rdgModels.Checked[Ord(mcUse), RowIndex] then
        begin
          AFileName := rdgModels.Cells[Ord(mcFileName), RowIndex];
          Assert(FileExists(AFileName));

          LayerNumbers := TIntegerList.Create;
          AltLayerNumbers := TIntegerList.Create;
          AuxLayerNumbersList := TObjectList<TIntegerList>.Create;
          LayerDataSets := TList.Create;
          AltLayerDataSets := TList.Create;
          AuxLayerDataSetsList := TObjectList<TList>.Create;
          AuxMinValuesList:= TObjectList<TRealList>.Create;
          AuxMaxValuesList := TObjectList<TRealList>.Create;
          try

            AModel := rdgModels.Objects[Ord(mcModelName), RowIndex] as TCustomModel;
            Vectors := AModel.VelocityVectors;
            if AModel.DisvUsed then
            begin
              FGrid := nil;
              FDisvGrid := AModel.DisvGrid;
            end
            else
            begin
              FDisvGrid := nil;
              FGrid := AModel.ModflowGrid;
            end;
            FFileStream := nil;
            FFileVariable := nil;
            FMaxPeriod := Max(frmGoPhast.PhastModel.ModflowStressPeriods.Count,
              frmGoPhast.PhastModel.ModflowFullStressPeriods.Count);
            FMaxStep := frmGoPhast.PhastModel.ModflowStressPeriods.MaxStepsInAnyStressPeriod;
  //          FMaxLayer := AModel.ModflowLayerCount;
            OpenResultFile(AFileName, Precision, HufFormat);
            EndReached := False;
            KPER := -1;
            KSTP := -1;
            Description := '';
            Count := 0;
            NLAY := 1;
            if FResultFormat = mrFlux then
            begin
              SetLength(ValuesToIgnore, 0);
            end
            else if FResultFormat = mfMt3dConc then
            begin
              SetLength(ValuesToIgnore, 1);
              ValuesToIgnore[0] := AModel.ModflowPackages.Mt3dBasic.InactiveConcentration;
            end
            else
            begin
              if AModel.ModelSelection = msModflow2015 then
              begin
                SetLength(ValuesToIgnore, 3);
                ValuesToIgnore[0] := 1.0e30;
                ValuesToIgnore[1] := -1.0e30;
                ValuesToIgnore[2] := 3.0e30;
              end
              else
              begin
                SetLength(ValuesToIgnore, 2);
                ValuesToIgnore[0] := AModel.ModflowOptions.HDry;
                ValuesToIgnore[1] := AModel.ModflowOptions.HNoFlow;
              end;
            end;
            LastItem := 0;
            for Index := clData.Items.Count - 1 downto 0 do
            begin
              if clData.Checked[Index] then
              begin
                LastItem := Index;
                break;
              end;
            end;
            if FResultFormat in [mfSwrStageAscii, mfSwrStageBinary] then
            begin
              if not ImportSwrStages(AModel, AFileName, ILAY, FileNames,
                LastItem, OldComments, Precision) then
              begin
                Beep;
                MessageDlg(StrTheNumberOfReache, mtError, [mbOK], 0);
                Exit;
              end;
            end
            else if FResultFormat in
              [mfSwrReachExchangeAscii, mfSwrReachExchangeBinary] then
            begin
              if not ImportSwrReachExchange(AModel, AFileName, ILAY, FileNames,
                LastItem, OldComments, Precision) then
              begin
                Beep;
                MessageDlg(StrTheNumberOfReache, mtError, [mbOK], 0);
                Exit;
              end;
            end
            else if FResultFormat in
              [mfSwrReachGroupBudgetAscii, mfSwrReachGroupBudgetBinary] then
            begin
              ImportSwrReachGroupFlows(AModel, AFileName, ILAY, FileNames,
                LastItem, OldComments, Precision);
            end
            else
            begin
              if FGrid <> nil then
              begin
                LayerCount := FGrid.LayerCount;
              end
              else
              begin
                LayerCount := FDisvGrid.LayerCount;
              end;

              PriorILay := 1;
              for Index := 0 to clData.Items.Count - 1 do
              begin
                case FResultFormat of
                  mrBinary, mrAscii, mfMt3dConc, mfCSubBinary:
                    begin
                      if Index > LastItem then
                      begin
                        Break;
                      end;
                      if Index = 0 then
                      begin
                        ReadArray(AnArray, EndReached,
                          NTRANS, KPER, KSTP, ILAY, TOTIM, Description, Precision,
                          clData.Checked[Index]);
                      end;
                      Description := Description;
                      While (KPER = FPeriods[Index])
                        and (KSTP = FSteps[Index])
                        and (NTRANS = FTransportSteps[Index])
                        and (Description + FSpeciesName = FDescriptions[Index])
                        and not EndReached do
                      begin
                        if ILAY < 0 then
                        begin
                          // cross section
                          for LayerIndex := 1 to AModel.ModflowLayerCount do
                          begin
                            ILAY := LayerIndex;
                            ILAY := AModel.ModflowLayerToDataSetLayer(ILAY)+1;
                            if clData.Checked[Index] then
                            begin
                              SetLength(LayerArray, 1, Length(AnArray[0]));
                              for ColIndex := 0 to Length(AnArray[0]) - 1 do
                              begin
                                LayerArray[0,ColIndex] := AnArray[LayerIndex-1,ColIndex];
                              end;
                              AssignWaterTableArray(WaterTableArray,
                                ILAY, LayerArray, ValuesToIgnore, Description);
                              CreateOrRetrieveLayerDataSet(Description, ILAY,
                                LayerData, OldComment,
                                FileNames, AModel, TDataArray, Precision);
                              CreateScreenObject(ILAY-1, AModel, ScreenObject, AFileName);
                              AssignValues(ILAY-1, ScreenObject, LayerData, LayerArray,
                                ValuesToIgnore, AModel, MinMaxAssigned);
                              LayerNumbers.Add(ILAY);
                              LayerDataSets.Add(LayerData);
                              Assert(FModifiedParentDataSets.Count> 0);
                              UpdateOldComments(OldComments, LayerData, OldComment);
                              DataSetNames.AddObject(LayerData.Name, LayerData);
                              if MinMaxAssigned then
                              begin
                                MinValues.Add(LayerData.Limits.LowerLimit.RealLimitValue);
                                MaxValues.Add(LayerData.Limits.UpperLimit.RealLimitValue);
                              end;

                              if ILAY = FGrid.LayerCount then
                              begin
                                Inc(Count);
                                CreateOrRetrieve3DDataSet(Description, NTRANS, KPER, KSTP, TOTIM,
                                  LayerNumbers, LayerDataSets, New3DArray, OldComment,
                                  False, NewDataSets, FileNames, AModel, Precision);
                                UpdateOldComments(OldComments, New3DArray, OldComment);
                                DataSetNames.AddObject(New3DArray.Name, New3DArray);
                                ParentArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(New3DArray.Name);
                                comboColorGrid.Items.Objects[Count] := ParentArray;

                                AssignLimits(MinValues, MaxValues, New3DArray,
                                  ValuesToIgnore);
                              end;
                              UndoCreateObject := TUndoCreateScreenObject.Create(ScreenObject);
                              UndoCreateObject.UpdateObservations;
                              NewCreateScreenObjects.Add(UndoCreateObject);
                            end
                          end;
                          if clData.Checked[Index] then
                          begin
                            AssignWaterTable(OldComments, DataSetNames,
                              NewCreateScreenObjects,
                              WaterTableArray, Description, ValuesToIgnore,
                              FileNames, AModel, Precision);
                          end;
                        end
                        else
                        begin
                          Assert(ILAY > 0);
                          if ILAY > FMaxLayer then
                          begin
                            Beep;
                            MessageDlg(StrTheFileYouAreTry, mtError, [mbOK], 0);
                            Exit;
                          end;
                          // not a cross section
                          ILAY := AModel.ModflowLayerToDataSetLayer(ILAY)+1;
                          if clData.Checked[Index] then
                          begin
                            AssignWaterTableArray(WaterTableArray, ILAY, AnArray,
                              ValuesToIgnore, Description);
                            CreateOrRetrieveLayerDataSet(
                              Description+Mt3dComponentName, ILAY,
                              LayerData, OldComment,
                              FileNames, AModel, TDataArray, Precision);
                            CreateScreenObject(ILAY-1, AModel, ScreenObject, AFileName);
                            AssignValues(ILAY-1, ScreenObject, LayerData, AnArray,
                              ValuesToIgnore, AModel, MinMaxAssigned);
                            LayerNumbers.Add(ILAY);
                            LayerDataSets.Add(LayerData);
                            UpdateOldComments(OldComments, LayerData, OldComment);
                            DataSetNames.AddObject(LayerData.Name, LayerData);
                            if MinMaxAssigned then
                            begin
                              MinValues.Add(LayerData.Limits.LowerLimit.RealLimitValue);
                              MaxValues.Add(LayerData.Limits.UpperLimit.RealLimitValue);
                            end;

                            if ILAY = LayerCount then
                            begin
                              Inc(Count);
                              CreateOrRetrieve3DDataSet(
                                Description+Mt3dComponentName, NTRANS, KPER, KSTP, TOTIM,
                                LayerNumbers, LayerDataSets, New3DArray, OldComment,
                                False, NewDataSets, FileNames, AModel, Precision);
                              UpdateOldComments(OldComments, New3DArray, OldComment);
                              DataSetNames.AddObject(New3DArray.Name, New3DArray);
                              ParentArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(New3DArray.Name);
                              comboColorGrid.Items.Objects[Count] := ParentArray;

                              AssignLimits(MinValues, MaxValues, New3DArray,
                                ValuesToIgnore);

                            end;
                            UndoCreateObject := TUndoCreateScreenObject.Create(ScreenObject);
                            UndoCreateObject.UpdateObservations;
                            NewCreateScreenObjects.Add(UndoCreateObject);
    //                        NewCreateScreenObjects.Add(
    //                          TUndoCreateScreenObject.Create(ScreenObject));
                            if ILAY = LayerCount then
                            begin
                              AssignWaterTable(OldComments, DataSetNames,
                                NewCreateScreenObjects,
                                WaterTableArray, Description, ValuesToIgnore,
                                FileNames, AModel, Precision);
                            end;
                          end;
                        end;

                        // read next array
                        if ILAY = LayerCount then
                        begin
                          ReadArray(AnArray, EndReached,
                            NTRANS, KPER, KSTP, ILAY, TOTIM, Description, Precision,
                            (LastItem <> Index) and clData.Checked[Index+1])
                        end
                        else
                        begin
                          ReadArray(AnArray, EndReached,
                            NTRANS, KPER, KSTP, ILAY, TOTIM, Description, Precision,
                            clData.Checked[Index]);
                          if PriorILay + 1 <> ILAY then
                          begin
                            Beep;
                            MessageDlg(StrTheFileYouAreTryFewer, mtError, [mbOK], 0);
                            Exit;
                          end;
                        end;
                        PriorILay := ILAY;
                      end;
                    end;
                  mrFlux:
                    begin
                      ILAY := 0;
                      NTRANS := 0;
                      Read3DArray(NLAY, EndReached, KPER, KSTP, TOTIM, Description,
                        A3DArray, AuxArray, Precision, HufFormat, True, AModel,
                        ModelName1, ModelName2, PackageName1, PackageName2);
                      if Description = 'FLOW-JA-FACE' then
                      begin
                        Read3DArray(NLAY, EndReached, KPER, KSTP, TOTIM, Description,
                          A3DArray, AuxArray, Precision, HufFormat, True, AModel,
                          ModelName1, ModelName2, PackageName1, PackageName2);
                      end;
                      if Trim(ModelName1) = 'MODFLOW' then
                      begin
                        ModelName1 := '';
                      end;
                      if Trim(ModelName2) = 'MODFLOW' then
                      begin
                        ModelName2 := '';
                      end;
                      if Trim(PackageName1) = 'MODFLOW' then
                      begin
                        PackageName1 := '';
                      end;
                      if Trim(PackageName2) = 'MODFLOW' then
                      begin
                        PackageName2 := '';
                      end;
                      if ModelName1 = ModelName2 then
                      begin
                        ModelName2 :='';
                      end;
                      if PackageName1 = PackageName2 then
                      begin
                        PackageName2 :='';
                      end;
                      ALabel := WriteLabel(Description, AModel, ILAY, KPER,
                        KSTP, NTRANS, SwrTimeStep, TOTIM, Precision,
                        ModelName1, ModelName2, PackageName1, PackageName2);
                      CheckIndex := clData.Items.IndexOf(ALabel);
                      Assert(CheckIndex >= 0);
                      if CheckIndex > LastItem then
                      begin
                        Break;
                      end;
                      if clData.Checked[CheckIndex] and not EndReached then
                      begin
                        if (Pos('data_spdis', Lowercase(Description)) = 1)
                          or (Pos('data-spdis', Lowercase(Description)) = 1) then
                        begin
                          AuxLayerNumbersList.Clear;
                          AuxLayerDataSetsList.Clear;
                          AuxMinValuesList.Clear;
                          AuxMaxValuesList.Clear;
                          for AuxIndex := 0 to Length(AuxArray) - 1 do
                          begin
                            AuxLayerNumbersList.Add(TIntegerList.Create);
                            AuxLayerDataSetsList.Add(TList.Create);
                            AuxMinValuesList.Add(TRealList.Create);
                            AuxMaxValuesList.Add(TRealList.Create);
                          end;
                        end;
                        for LayerIndex := 0 to Abs(NLAY) - 1 do
                        begin
                          ILAY := AModel.ModflowLayerToDataSetLayer(LayerIndex+1)+1;

                          if (Pos('data_spdis', Lowercase(Description)) = 1)
                            or (Pos('data-spdis', Lowercase(Description)) = 1) then
                          begin
                            for AuxIndex := 0 to Length(AuxArray) - 1 do
                            begin
                              AuxLayerNumbers := AuxLayerNumbersList[AuxIndex];
                              AuxLayerDataSets := AuxLayerDataSetsList[AuxIndex];
                              AuxMinValues := AuxMinValuesList[AuxIndex];
                              AuxMaxValues := AuxMaxValuesList[AuxIndex];
                              AuxDescription := Description + '_' + Trim(AuxArray[AuxIndex].Name);
                              CreateOrRetrieveLayerDataSet(AuxDescription, ILAY,
                                LayerData, OldComment,
                                FileNames, AModel, TDataArray, Precision);
                              CreateScreenObject(ILAY-1, AModel, ScreenObject, AFileName);
                              Assign3DValues(ScreenObject, LayerData,
                                AuxArray[AuxIndex].Values, LayerIndex, ILAY-1,
                                False, ValuesToIgnore, AModel);
                              AuxLayerNumbers.Add(ILAY);
                              AuxLayerDataSets.Add(LayerData);
                              UpdateOldComments(OldComments, LayerData, OldComment);
                              DataSetNames.AddObject(LayerData.Name, LayerData);
                              AuxMinValues.Add(LayerData.Limits.LowerLimit.RealLimitValue);
                              AuxMaxValues.Add(LayerData.Limits.UpperLimit.RealLimitValue);
                              UndoCreateObject := TUndoCreateScreenObject.Create(ScreenObject);
                              UndoCreateObject.UpdateObservations;
                              NewCreateScreenObjects.Add(UndoCreateObject);
                            end;
                          end
                          else
                          begin
                            CreateOrRetrieveLayerDataSet(Description, ILAY,
                              LayerData, OldComment,
                              FileNames, AModel, TDataArray, Precision);
                            CreateScreenObject(ILAY-1, AModel, ScreenObject, AFileName);
                            Assign3DValues(ScreenObject, LayerData, A3DArray,
                              LayerIndex, ILAY-1,
                              False, ValuesToIgnore, AModel);
                            LayerNumbers.Add(ILAY);
                            LayerDataSets.Add(LayerData);
                            UpdateOldComments(OldComments, LayerData, OldComment);
                            DataSetNames.AddObject(LayerData.Name, LayerData);
                            MinValues.Add(LayerData.Limits.LowerLimit.RealLimitValue);
                            MaxValues.Add(LayerData.Limits.UpperLimit.RealLimitValue);
                            UndoCreateObject := TUndoCreateScreenObject.Create(ScreenObject);
                            UndoCreateObject.UpdateObservations;
                            NewCreateScreenObjects.Add(UndoCreateObject);

                            if Description = 'Flow Right Face' then
                            begin
                              if LayerIndex = 0 then
                              begin
                                Alt3DArray := A3DArray;
                                SetLength(Alt3DArray, Length(Alt3DArray),
                                  Length(Alt3DArray[0]), Length(Alt3DArray[0][0]));
                                MoveArrayLeft(Alt3DArray);
                                NegateArray(Alt3DArray);
                              end;
                              AltDescription := 'Flow Left Face';
                              CreateOrRetrieveLayerDataSet(AltDescription, ILAY,
                                LayerData, OldComment,
                                FileNames, AModel, TDataArray, Precision);
                              CreateScreenObject(ILAY-1, AModel, ScreenObject, AFileName);
                              Assign3DValues(ScreenObject, LayerData, Alt3DArray,
                                LayerIndex, ILAY-1,
                                False, ValuesToIgnore, AModel);
                              AltLayerNumbers.Add(ILAY);
                              AltLayerDataSets.Add(LayerData);
                              UpdateOldComments(AltOldComments, LayerData, OldComment);
                              AltDataSetNames.AddObject(LayerData.Name, LayerData);
                              AltMinValues.Add(LayerData.Limits.LowerLimit.RealLimitValue);
                              AltMaxValues.Add(LayerData.Limits.UpperLimit.RealLimitValue);
                              UndoCreateObject := TUndoCreateScreenObject.Create(ScreenObject);
                              UndoCreateObject.UpdateObservations;
                              NewCreateScreenObjects.Add(UndoCreateObject);
                            end
                            else if Description = 'Flow Front Face' then
                            begin
                              if LayerIndex = 0 then
                              begin
                                Alt3DArray := A3DArray;
                                SetLength(Alt3DArray, Length(Alt3DArray),
                                  Length(Alt3DArray[0]), Length(Alt3DArray[0][0]));
                                MoveArrayForward(Alt3DArray);
                                NegateArray(Alt3DArray);
                              end;
                              AltDescription := 'Flow Back Face';
                              CreateOrRetrieveLayerDataSet(AltDescription, ILAY,
                                LayerData, OldComment,
                                FileNames, AModel, TDataArray, Precision);
                              CreateScreenObject(ILAY-1, AModel, ScreenObject, AFileName);
                              Assign3DValues(ScreenObject, LayerData, Alt3DArray,
                                LayerIndex, ILAY-1,
                                False, ValuesToIgnore, AModel);
                              AltLayerNumbers.Add(ILAY);
                              AltLayerDataSets.Add(LayerData);
                              UpdateOldComments(AltOldComments, LayerData, OldComment);
                              AltDataSetNames.AddObject(LayerData.Name, LayerData);
                              AltMinValues.Add(LayerData.Limits.LowerLimit.RealLimitValue);
                              AltMaxValues.Add(LayerData.Limits.UpperLimit.RealLimitValue);
                              UndoCreateObject := TUndoCreateScreenObject.Create(ScreenObject);
                              UndoCreateObject.UpdateObservations;
                              NewCreateScreenObjects.Add(UndoCreateObject);
                            end
                            else if Description = 'Flow Lower Face' then
                            begin
                              if LayerIndex = 0 then
                              begin
                                Alt3DArray := A3DArray;
                                SetLength(Alt3DArray, Length(Alt3DArray),
                                  Length(Alt3DArray[0]), Length(Alt3DArray[0][0]));
                                MoveArrayUp(Alt3DArray);
                                NegateArray(Alt3DArray);
                              end;
                              AltDescription := 'Flow Upper Face';
                              CreateOrRetrieveLayerDataSet(AltDescription, ILAY,
                                LayerData, OldComment,
                                FileNames, AModel, TDataArray, Precision);
                              CreateScreenObject(ILAY-1, AModel, ScreenObject, AFileName);
                              Assign3DValues(ScreenObject, LayerData, Alt3DArray,
                                LayerIndex, ILAY-1,
                                False, ValuesToIgnore, AModel);
                              AltLayerNumbers.Add(ILAY);
                              AltLayerDataSets.Add(LayerData);
                              UpdateOldComments(AltOldComments, LayerData, OldComment);
                              AltDataSetNames.AddObject(LayerData.Name, LayerData);
                              AltMinValues.Add(LayerData.Limits.LowerLimit.RealLimitValue);
                              AltMaxValues.Add(LayerData.Limits.UpperLimit.RealLimitValue);
                              UndoCreateObject := TUndoCreateScreenObject.Create(ScreenObject);
                              UndoCreateObject.UpdateObservations;
                              NewCreateScreenObjects.Add(UndoCreateObject);
                            end;
                          end;
                        end;
                        Inc(Count);
                        ParentArray := nil;
                        if (Pos('data_spdis', Lowercase(Description)) = 1)
                          or (Pos('data-spdis', Lowercase(Description)) = 1) then
                        begin
                          if Length(AuxArray) > 0 then
                          begin
                            VectorItem := Vectors.Add;
                          end;
                          for AuxIndex := 0 to Length(AuxArray) - 1 do
                          begin
                            AuxLayerNumbers := AuxLayerNumbersList[AuxIndex];
                            AuxLayerDataSets := AuxLayerDataSetsList[AuxIndex];
                            AuxMinValues := AuxMinValuesList[AuxIndex];
                            AuxMaxValues := AuxMaxValuesList[AuxIndex];
                            AuxDescription := Description + '_' + Trim(AuxArray[AuxIndex].Name);
                            CreateOrRetrieve3DDataSet(AuxDescription, NTRANS, KPER, KSTP, TOTIM,
                              AuxLayerNumbers, AuxLayerDataSets, New3DArray, OldComment, True,
                              NewDataSets, FileNames, AModel, Precision);

                            if Pos('qx_', New3DArray.Name) > 0 then
                            begin
                              VectorItem.Description :=
                                ReplaceText(New3DArray.Name, 'qx_', '')
                                + Format(StrImportedOnS, [DateToStr(Now)]);
                              VectorItem.Vectors.XVelocityName := New3DArray.Name;
                            end
                            else if Pos('qy_', New3DArray.Name) > 0 then
                            begin
                              VectorItem.Description := ReplaceText(New3DArray.Name, 'qy_', '')
                                + Format(StrImportedOnS, [DateToStr(Now)]);
                              VectorItem.Vectors.YVelocityName := New3DArray.Name;
                            end
                            else if Pos('qz_', New3DArray.Name) > 0 then
                            begin
                              VectorItem.Description := ReplaceText(New3DArray.Name, 'qz_', '')
                                + Format(StrImportedOnS, [DateToStr(Now)]);
                              VectorItem.Vectors.ZVelocityName := New3DArray.Name;
                            end;

                            UpdateOldComments(OldComments, New3DArray, OldComment);
                            DataSetNames.AddObject(New3DArray.Name, New3DArray);
                            ParentArray := frmGoPhast.PhastModel.DataArrayManager.
                              GetDataSetByName(New3DArray.Name);

                            AssignLimits(AuxMinValues, AuxMaxValues, New3DArray,
                              ValuesToIgnore);
                          end;
                        end
                        else
                        begin
                          CreateOrRetrieve3DDataSet(Description, NTRANS, KPER, KSTP, TOTIM,
                            LayerNumbers, LayerDataSets, New3DArray, OldComment, True,
                            NewDataSets, FileNames, AModel, Precision);
                          UpdateOldComments(OldComments, New3DArray, OldComment);
                          DataSetNames.AddObject(New3DArray.Name, New3DArray);
                          ParentArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(New3DArray.Name);

                          AssignLimits(MinValues, MaxValues, New3DArray, ValuesToIgnore);

                          if Description = 'Flow Right Face' then
                          begin
                            AltDescription := 'Flow Left Face';
                            CreateOrRetrieve3DDataSet(AltDescription, NTRANS, KPER, KSTP, TOTIM,
                              AltLayerNumbers, AltLayerDataSets, New3DArray, OldComment, True,
                              NewDataSets, FileNames, AModel, Precision);
                            UpdateOldComments(AltOldComments, New3DArray, OldComment);
                            AltDataSetNames.AddObject(New3DArray.Name, New3DArray);
//                            ParentArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(New3DArray.Name);

                            AssignLimits(AltMinValues, AltMaxValues, New3DArray, ValuesToIgnore);
                          end
                          else if Description = 'Flow Front Face' then
                          begin
                            AltDescription := 'Flow Back Face';
                            CreateOrRetrieve3DDataSet(AltDescription, NTRANS, KPER, KSTP, TOTIM,
                              AltLayerNumbers, AltLayerDataSets, New3DArray, OldComment, True,
                              NewDataSets, FileNames, AModel, Precision);
                            UpdateOldComments(AltOldComments, New3DArray, OldComment);
                            AltDataSetNames.AddObject(New3DArray.Name, New3DArray);
//                            ParentArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(New3DArray.Name);

                            AssignLimits(AltMinValues, AltMaxValues, New3DArray, ValuesToIgnore);
                          end
                          else if Description = 'Flow Lower Face' then
                          begin
                            AltDescription := 'Flow Upper Face';
                            CreateOrRetrieve3DDataSet(AltDescription, NTRANS, KPER, KSTP, TOTIM,
                              AltLayerNumbers, AltLayerDataSets, New3DArray, OldComment, True,
                              NewDataSets, FileNames, AModel, Precision);
                            UpdateOldComments(AltOldComments, New3DArray, OldComment);
                            AltDataSetNames.AddObject(New3DArray.Name, New3DArray);
//                            ParentArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(New3DArray.Name);

                            AssignLimits(AltMinValues, AltMaxValues, New3DArray, ValuesToIgnore);
                          end;
                        end;
                        comboColorGrid.Items.Objects[Count] := ParentArray;
                      end;

                    end;
                  mrHufAscii, mrHufBinary:
                    begin
                      if Index > LastItem then
                      begin
                        break;
                      end;
                      ReadArray(AnArray, EndReached,
                        NTRANS, KPER, KSTP, ILAY, TOTIM, Description, Precision,
                        clData.Checked[Index]);
                      Assert((KPER = FPeriods[Index])
                        and (KSTP = FSteps[Index])
                        and (Description = FDescriptions[Index]));
                      Assert( ILAY > 0);
                      // not a cross section
                      if clData.Checked[Index] then
                      begin
                        HGU := AModel.HydrogeologicUnits[ILAY-1];
                        Description := Description + ' ' + HGU.HufName;
                        CreateOrRetrieveLayerDataSet(Description, ILAY,
                          LayerData, OldComment,
                          FileNames, AModel, TDataArray, Precision);
                        CreateScreenObject(-1, AModel, ScreenObject, AFileName);
                        AssignValues(-1, ScreenObject, LayerData, AnArray,
                          ValuesToIgnore, AModel, MinMaxAssigned);
                        UpdateOldComments(OldComments, LayerData, OldComment);
                        DataSetNames.AddObject(LayerData.Name, LayerData);
                        Inc(Count);
                        ParentArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(LayerData.Name);
                        comboColorGrid.Items.Objects[Count] := ParentArray;
                        UndoCreateObject := TUndoCreateScreenObject.Create(ScreenObject);
                        UndoCreateObject.UpdateObservations;
                        NewCreateScreenObjects.Add(UndoCreateObject);
    //                    NewCreateScreenObjects.Add(
    //                      TUndoCreateScreenObject.Create(ScreenObject))
                      end;
                    end;
                  mrHufFlux:
                    begin
                      if Index > LastItem then
                      begin
                        break;
                      end;
                      if (Index = 0) or ((Index mod NLAY) = 0) then
                      begin
                        Read3DArray(NLAY, EndReached, KPER, KSTP, TOTIM,
                          Description, A3DArray, AuxArray, Precision, HufFormat,
                          clData.Checked[Index], AModel,
                          ModelName1, ModelName2, PackageName1, PackageName2);
                      end;

                      if clData.Checked[Index] then
                      begin
                        LayerIndex := Index mod NLAY;
                        ILAY := LayerIndex+1;
                        HGU := AModel.HydrogeologicUnits[ILAY-1];
                        LayerDescription := Description + ' ' + HGU.HufName;
                        CreateOrRetrieveLayerDataSet(LayerDescription, ILAY,
                          LayerData, OldComment,
                          FileNames, AModel, TDataArray, Precision);
                        CreateScreenObject(-1, AModel, ScreenObject, AFileName);
                        Assign3DValues(ScreenObject, LayerData, A3DArray, LayerIndex, ILAY-1,
                          True, ValuesToIgnore, AModel);
                        UpdateOldComments(OldComments, LayerData, OldComment);
                        DataSetNames.AddObject(LayerData.Name, LayerData);
                        Inc(Count);
                        ParentArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(LayerData.Name);
                        comboColorGrid.Items.Objects[Count] := ParentArray;
                        UndoCreateObject := TUndoCreateScreenObject.Create(ScreenObject);
                        UndoCreateObject.UpdateObservations;
                        NewCreateScreenObjects.Add(UndoCreateObject);
    //                    NewCreateScreenObjects.Add(
    //                      TUndoCreateScreenObject.Create(ScreenObject))
                      end;
                    end;
                  mfSubBinary:
                    begin
                      if Index > LastItem then
                      begin
                        break;
                      end;
                      ReadArray(AnArray, EndReached,
                        NTRANS, KPER, KSTP, ILAY, TOTIM, Description, Precision,
                        clData.Checked[Index]);
                      Description := SubsidenceDescription(Description, ILAY);
                      if clData.Checked[Index] then
                      begin
                        Inc(Count);
                        CreateOrRetrieveLayerDataSet(Description, ILAY,
                          LayerData, OldComment,
                          FileNames, AModel, TDataArray, Precision, dafSubsidence);
                        ParentArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(LayerData.Name);
                        comboColorGrid.Items.Objects[Count] := ParentArray;
                        CreateScreenObject(ILAY-1, AModel, ScreenObject, AFileName);
                        AssignValues(ILAY-1, ScreenObject, LayerData, AnArray,
                          ValuesToIgnore, AModel, MinMaxAssigned);
                        LayerNumbers.Add(ILAY);
                        LayerDataSets.Add(LayerData);
                        UpdateOldComments(OldComments, LayerData, OldComment);
                        DataSetNames.AddObject(LayerData.Name, LayerData);
                        if MinMaxAssigned then
                        begin
                          MinValues.Add(LayerData.Limits.LowerLimit.RealLimitValue);
                          MaxValues.Add(LayerData.Limits.UpperLimit.RealLimitValue);
                        end;
                      end;

                    end;
                  else Assert(False);
                end;
              end;
            end;
          finally
            CloseFiles;
            LayerNumbers.Free;
            AltLayerNumbers.Free;
            AuxLayerNumbersList.Free;
            LayerDataSets.Free;
            AltLayerDataSets.Free;
            AuxLayerDataSetsList.Free;
            AuxMinValuesList.Free;
            AuxMaxValuesList.Free;
          end;
        end;
      end;
      New3DArray := comboColorGrid.Items.Objects[comboColorGrid.ItemIndex]
        as TDataArray;
      DisplayChoice := TDisplayChoice(rgDisplayChoice.ItemIndex);
      if New3DArray <> nil then
      begin
        case DisplayChoice of
          dcColor:
            begin
              frmGoPhast.acColoredGrid.Enabled := True;
              frmGoPhast.acColoredGrid.Checked := True;
              frmGoPhast.tb3DColors.Down := True;
            end;
          dcContour, dcNone:
            begin
              // do nothing
            end;
          else Assert(False);
        end;
      end;
      for Index := 0 to FModifiedParentDataSets.Count - 1 do
      begin
        AParentArray := FModifiedParentDataSets[Index];
        AParentArray.Comment := AParentArray.Comment
          + sLineBreak + Format(StrMinimumValueG,
          [AParentArray.Limits.LowerLimit.RealLimitValue])
          + sLineBreak + Format(StrMaximumValueG,
          [AParentArray.Limits.UpperLimit.RealLimitValue]);
      end;
      FFormulaAssigners.AssignFinalFormulas;
      UndoImportResults := TUndoImportModelResults.Create(NewDataSets,
        DataSetNames, OldComments, New3DArray, DisplayChoice, frmGoPhast.PhastModel);
      DeletedDataSets := TList.Create;
      NewDataSetProperties := TObjectList.Create;
      try
        for Index := 0 to NewDataSets.Count - 1 do
        begin
          DataArray := NewDataSets[Index];
          DataStorage := TPhastDataSetStorage.Create;
          DataStorage.Assign(DataArray);
          NewDataSetProperties.Add(DataStorage);
        end;
        UndoChangeDataSets := TUndoChangeDataSets.Create(
          DeletedDataSets, NewDataSets, NewDataSetProperties);
        UndoImportResults.FContainedUndos.Add(UndoChangeDataSets);
      finally
        DeletedDataSets.Free;
        NewDataSetProperties.Free;
      end;

      if ScreenObjectsToDelete.Count > 0 then
      begin
        UndoDeleteScreenObjects:=
          TUndoDeleteScreenObjects.Create(ScreenObjectsToDelete);
        UndoImportResults.FContainedUndos.Add(UndoDeleteScreenObjects);
      end;

      for Index := 0 to NewCreateScreenObjects.Count - 1 do
      begin
        UndoImportResults.FContainedUndos.Add(NewCreateScreenObjects[Index]);
      end;

      frmGoPhast.BeginSuppressDrawing;
      try
        frmGoPhast.UndoStack.Submit(UndoImportResults);
      finally
        frmGoPhast.EndSupressDrawing;
      end;
    except
      on E: EConvertError do
      begin
        Beep;
        MessageDlg(Format(StrInvalidData, [E.message]), mtError, [mbOK], 0);
      end;
      on E: EAbortingImport do
      begin
        for Index := 0 to NewCreateScreenObjects.Count - 1 do
        begin
          UndoItem := NewCreateScreenObjects[Index];
          UndoItem.Undo;
          UndoItem.Free;
        end;

        frmGoPhast.PhastModel.DataArrayManager.HandleDeletedDataArrays(NewDataSets);
        Beep;
        MessageDlg(E.message, mtInformation, [mbOK], 0);
      end;

    end;
  finally
    NewDataSets.Free;
    ScreenObjectsToDelete.Free;
    NewCreateScreenObjects.Free;
    MinValues.Free;
    AltMinValues.Free;
    MaxValues.Free;
    AltMaxValues.Free;
    OldComments.Free;
    AltOldComments.Free;
    DataSetNames.Free;
    AltDataSetNames.Free;
  end;

end;

function TfrmSelectResultToImport.SubsidenceDescription(DESC: String;
  ILAY: integer): string;
begin
  result := string(Trim(DESC));
  if SameText(result, 'SUBSIDENCE') then
  begin
    Exit;
  end
  else if SameText(result, 'NDSYS COMPACTION')
    or SameText(result, 'ND CRITICAL HEAD')
    or SameText(result, 'DSYS COMPACTION')
    or SameText(result, 'D CRITICAL HEAD')
    or SameText(result, 'SYSTM COMPACTION') then
  begin
    result := result + ' System ' + IntToStr(ILAY);
  end
  else
  begin
    result := result + ' Layer ' + IntToStr(ILAY);
  end;
end;

procedure TfrmSelectResultToImport.clDataClickCheck(Sender: TObject);
begin
  inherited;
  UpdateCombo;
end;

procedure TfrmSelectResultToImport.clDescriptionStateChange(Sender: TObject;
  Index: Integer);
var
  ItemList: TList<Integer>;
  ItemIndex: Integer;
  Checked: Boolean;
begin
  inherited;
  if FSettingAll then
  begin
    Exit;
  end;
  ItemList := FItemDescriptions.Objects[Index] as TList<Integer>;
  Checked := clDescription.Checked[Index];
  for ItemIndex := 0 to ItemList.Count - 1 do
  begin
    clData.Checked[ItemList[ItemIndex]] := Checked;
  end;
  clDataClickCheck(clData);
end;

procedure TfrmSelectResultToImport.FormCreate(Sender: TObject);
var
  FilterDescriptions: TStringList;
  FileExtensions: TStringList;
  index: Integer;
  SubsidenceDescriptions: TStringList;
  SubsidenceExtensions: TStringList;
begin
  inherited;
  comboClassification.Items.Add(StrModelResults);
  comboClassification.Items.Add(StrUserDefined);
  comboClassification.ItemIndex := 0;

  FModifiedParentDataSets:= TList.Create;
  FFormulaAssigners := TFormulaAssignerList.Create;

  FItemDescriptions := TStringList.Create;
  FItemTimes := TStringList.Create;
  FItemDescriptions.OwnsObjects := True;
  FItemTimes.OwnsObjects := True;

  rdgModels.Cells[Ord(mcModelName), 0] := StrModel;
  rdgModels.Cells[Ord(mcUse), 0] := StrImportData;
  rdgModels.Cells[Ord(mcFileName), 0] := StrFileName;

  FNewDataSetNames:= TStringList.Create;
  FNewDefaultDataSetNames:= TStringList.Create;

  FPeriods := TIntegerList.Create;
  FSteps := TIntegerList.Create;
  FTransportSteps := TIntegerList.Create;
  FSwrSteps := TIntegerList.Create;

  FDescriptions := TStringList.Create;
//  FAskedUser := False;
  SetDefaultDisplayOption;

  FilterDescriptions := TStringList.Create;
  FileExtensions := TStringList.Create;
  SubsidenceDescriptions := TStringList.Create;
  SubsidenceExtensions := TStringList.Create;
  try
    FilterDescriptions.Add(StrFormattedHeadFiles);
    FileExtensions.Add(StrFhd);

    FilterDescriptions.Add(StrFormattedDrawdownF);
    FileExtensions.Add(StrFdn);

    FilterDescriptions.Add(StrBinaryHeadFiles);
    FileExtensions.Add(StrBhd);

    FilterDescriptions.Add(StrBinaryDrawdownFile);
    FileExtensions.Add(StrBdn);

    FilterDescriptions.Add(StrBinaryFlowFiles);
    FileExtensions.Add(StrCbcExt);

    FilterDescriptions.Add(StrGWTConcentrationFi);
    FileExtensions.Add(StrConc);

    FilterDescriptions.Add(StrFormattedHUFHeadF);
    FileExtensions.Add(StrHuffhd);

    FilterDescriptions.Add(StrBinaryHUFHeadFile);
    FileExtensions.Add(StrHufbhd);

    FilterDescriptions.Add(StrHUFFlowFiles);
    FileExtensions.Add(StrHufflow);

    FilterDescriptions.Add(StrSWIZetaFiles);
    FileExtensions.Add(strZeta);

    FilterDescriptions.Add(StrSWRASCIIStageFile);
    FileExtensions.Add(StrSwrReachStageA);

    FilterDescriptions.Add(StrSWRBinaryStageFil);
    FileExtensions.Add(StrSwrReachStageB);

    FilterDescriptions.Add(StrSWRReachExchangeText);
    FileExtensions.Add(StrSwrReachExchangeA);

    FilterDescriptions.Add(StrSWRReachExchangeBin);
    FileExtensions.Add(StrSwrReachExchangeB);

    FilterDescriptions.Add(StrSWRReachGroupWateText);
    FileExtensions.Add(StrSwrReachGroupFlowsA);

    FilterDescriptions.Add(StrSWRReachGroupWateBin);
    FileExtensions.Add(StrSwrReachGroupFlowsB);

    FilterDescriptions.Add(StrMT3DMSConcentration);
    FileExtensions.Add(StrMt3dConcFile);

    FilterDescriptions.Add(StrUZFRecharge);
    FileExtensions.Add(StrUzfRch);

    FilterDescriptions.Add(StrUZFDischarge);
    FileExtensions.Add(StrUzfDisch);

    SubsidenceDescriptions.Add(StrCombinedSUBOutput);
    SubsidenceExtensions.Add(StrSubOut);

    SubsidenceDescriptions.Add(StrCombinedSWTOutput);
    SubsidenceExtensions.Add(StrSwtOut);

    SubsidenceDescriptions.Add(StrSUBSubsidence);
    SubsidenceExtensions.Add(StrSubSubOut);

    SubsidenceDescriptions.Add(StrSUBCompactionByMo);
    SubsidenceExtensions.Add(StrSubComMlOut);

    SubsidenceDescriptions.Add(StrSUBCompactionByIn);
    SubsidenceExtensions.Add(StrSubComIsOut);

    SubsidenceDescriptions.Add(StrSUBVerticalDisplac);
    SubsidenceExtensions.Add(StrSubVdOut);

    SubsidenceDescriptions.Add(StrSUBCriticalHeadFo);
    SubsidenceExtensions.Add(StrSubNdCritHeadOut);

    SubsidenceDescriptions.Add(StrSUBCriticalHeadFoDelay);
    SubsidenceExtensions.Add(StrSubDCritHeadOut);




    SubsidenceDescriptions.Add(StrElasticCompactionML);
    SubsidenceExtensions.Add(StrSubElasCompMLOut);

    SubsidenceDescriptions.Add(StrInelasticCompactionML);
    SubsidenceExtensions.Add(StrSubInelasCompMLOut);

    SubsidenceDescriptions.Add(StrElasticCompactionIB);
    SubsidenceExtensions.Add(StrSubElasCompIBOut);

    SubsidenceDescriptions.Add(StrInelasticCompactionIb);
    SubsidenceExtensions.Add(StrSubInlasCompIBOut);





    SubsidenceDescriptions.Add(StrSWTSubsidence);
    SubsidenceExtensions.Add(StrSwtSubOut);

    SubsidenceDescriptions.Add(StrSWTCompactionByMo);
    SubsidenceExtensions.Add(StrSwtComMLOut);

    SubsidenceDescriptions.Add(StrSWTCompactionByIn);
    SubsidenceExtensions.Add(StrSwtComIsOut);

    SubsidenceDescriptions.Add(StrSWTVerticalDisplac);
    SubsidenceExtensions.Add(StrSwtVDOut);

    SubsidenceDescriptions.Add(StrSWTPreconsolidation);
    SubsidenceExtensions.Add(StrSwtDeltaPreConStrOu);

    SubsidenceDescriptions.Add(StrSWTChangeInPrecon);
    SubsidenceExtensions.Add(StrSwtDeltaPreConStrOu);

    SubsidenceDescriptions.Add(StrSWTGeostaticStress);
    SubsidenceExtensions.Add(StrSwtGeoStatOut);

    SubsidenceDescriptions.Add(StrSWTChangeInGeosta);
    SubsidenceExtensions.Add(StrSwtDeltaGeoStatOut);

    SubsidenceDescriptions.Add(StrSWTEffectiveStress);
    SubsidenceExtensions.Add(StrSwtEffStressOut);

    SubsidenceDescriptions.Add(StrSWTChangeInEffect);
    SubsidenceExtensions.Add(StrSwtDeltaEffStressOu);

    SubsidenceDescriptions.Add(StrSWTVoidRatio);
    SubsidenceExtensions.Add(StrSwtVoidRatioOut);

    SubsidenceDescriptions.Add(StrSWTThicknessOfCom);
    SubsidenceExtensions.Add(StrSwtThickCompSedOut);

    SubsidenceDescriptions.Add(StrSWTLayercenterEle);
    SubsidenceExtensions.Add(StrSwtLayerCentElevOut);

    SubsidenceDescriptions.Add(StrCSUBCompaction);
    SubsidenceExtensions.Add(StrCsubcmpct);

    SubsidenceDescriptions.Add(StrCSUBElasticCompact);
    SubsidenceExtensions.Add(StrCsubelstcmpct);

    SubsidenceDescriptions.Add(StrCSUBInlasticCompac);
    SubsidenceExtensions.Add(StrCsubinelstcmpct);

    SubsidenceDescriptions.Add(StrCSUBInterbedCompac);
    SubsidenceExtensions.Add(StrCsubintrbdcmpct);

    SubsidenceDescriptions.Add(StrCSUBCoarseCompacti);
    SubsidenceExtensions.Add(StrCsubcrscmpct);

    SubsidenceDescriptions.Add(StrCSUBZDisplacement);
    SubsidenceExtensions.Add(StrCsubzdis);




    odSelectFiles.Filter := StrCommonSupportedFil + Trim(FileExtensions[0]);
    for index := 1 to FileExtensions.Count - 1 do
    begin
      odSelectFiles.Filter := odSelectFiles.Filter
        + ';*' + Trim(FileExtensions[index]);
    end;

    odSelectFiles.Filter := odSelectFiles.Filter + StrSubsidenceFiles + SubsidenceExtensions[0];
    for index := 1 to SubsidenceExtensions.Count - 1 do
    begin
      odSelectFiles.Filter := odSelectFiles.Filter
        + ';*' + Trim(SubsidenceExtensions[index]);
    end;

    Assert(FileExtensions.Count = FilterDescriptions.Count);
    for index := 0 to FileExtensions.Count - 1 do
    begin
      odSelectFiles.Filter := odSelectFiles.Filter
        + '|' + FilterDescriptions[index]
        + '(*' + Trim(FileExtensions[index]) + ')|*' + Trim(FileExtensions[index]);
    end;

    Assert(SubsidenceExtensions.Count = SubsidenceDescriptions.Count);
    for index := 0 to SubsidenceExtensions.Count - 1 do
    begin
      odSelectFiles.Filter := odSelectFiles.Filter
        + '|' + SubsidenceDescriptions[index]
        + '(*' + Trim(SubsidenceExtensions[index]) + ')|*' + Trim(SubsidenceExtensions[index]);
    end;
  finally
    FilterDescriptions.Free;
    FileExtensions.Free;
    SubsidenceDescriptions.Free;
    SubsidenceExtensions.Free;
  end;

end;

procedure TfrmSelectResultToImport.FormDestroy(Sender: TObject);
begin
  inherited;
  FPeriods.Free;
  FSteps.Free;
  FSwrSteps.Free;
  FTransportSteps.Free;
  FDescriptions.Free;
  FNewDataSetNames.Free;
  FNewDefaultDataSetNames.Free;
  FFormulaAssigners.Free;
  FModifiedParentDataSets.Free;
  FItemDescriptions.Free;
  FItemTimes.Free;
end;

procedure TfrmSelectResultToImport.FormShow(Sender: TObject);
begin
  inherited;
  rdgModels.Column := Ord(mcFileName);
//  rdgModels.Options := rdgModels.Options + [goAlwaysShowEditor];
end;

procedure TfrmSelectResultToImport.odSelectFilesTypeChange(Sender: TObject);
var
  Dialog: TOpenDialog;
  NewFileName: string;
  Extension: string;
  Index: Integer;
  Position: integer;
  AChemSpecies: TChemSpeciesItem;
begin
  inherited;
  Dialog := Sender as TOpenDialog;
  if (Dialog.FilterIndex > 2) and (Dialog.FileName <> '') then
  begin
    Position := 1;
    for Index := 0 to Dialog.FilterIndex*2 - 2 do
    begin
      Position := PosEx('|', Dialog.Filter, Position+1);
    end;
    Extension := Copy(Dialog.Filter, Position+2,MAXINT);
    Position := Pos('|', Extension);
    if Position >= 1 then
    begin
      Extension := Copy(Extension, 1, Position-1);
    end;
    Position := Pos(';', Extension);
    if Position >= 1 then
    begin
      Extension := Copy(Extension, 1, Position-1);
    end;

    if SameText(Extension, StrMt3dConcFile) then
    begin
      NewFileName :=
        ChangeFileExt(Dialog.FileName, '');
      if frmGoPhast.PhastModel.MobileComponents.Count > 0 then
      begin
        AChemSpecies := frmGoPhast.PhastModel.MobileComponents[0];
      end
      else if frmGoPhast.PhastModel.ImmobileComponents.Count > 0 then
      begin
        AChemSpecies := frmGoPhast.PhastModel.ImmobileComponents[0];
      end
      else
      begin
        AChemSpecies := nil;
      end;
      if AChemSpecies <> nil then
      begin
        NewFileName := NewFileName + '_' + AChemSpecies.Name;
      end;
      NewFileName :=
        ChangeFileExt(NewFileName, Extension);
    end
    else
    begin
      NewFileName :=
        ChangeFileExt(Dialog.FileName, Extension);
    end;

    Dialog.FileName := NewFileName;
    UpdateDialogBoxFileName(Dialog, NewFileName);
  end;
end;

function TfrmSelectResultToImport.WriteLabel(var Description: string;
  AModel: TCustomModel; ILAY, KPER, KSTP, NTRANS, SwrTimeStep: integer;
  TOTIM: TModflowDouble; Precision: TModflowPrecision;
  ModelName1, ModelName2, PackageName1, PackageName2: String): string;
var
  HGU: THydrogeologicUnit;
  HufName: string;
begin
  Description := Trim(Description);
  if (Length(Description) >= 4)
    and (Copy(Description, 1, 3) = 'NET')
    and (Description[4] <> ' ') then
  begin
    Description := 'NET ' + Copy(Description,4, MAXINT);
  end;
  Assert(Length(Description) > 0);
  Description := TitleCase(Description);
  AdjustSwiNames(Description);
  if FResultFormat in [mrHufAscii, mrHufBinary, mrHufFlux] then
  begin
    if AModel.HydrogeologicUnits.Count = 0 then
    begin
      raise EHufReadError.Create(StrErrorAttemptingTo);
    end;
    HGU := AModel.HydrogeologicUnits[ILAY-1];
    HufName := ' ' + HGU.HufName;
  end
  else
  begin
    HufName := '';
  end;

  if ModelName1 <> '' then
  begin
    Description := Format('%0:s_%1:s', [Description, Trim(ModelName1)]);
  end;
  if PackageName1 <> '' then
  begin
    Description := Format('%0:s_%1:s', [Description, Trim(PackageName1)]);
  end;
  if ModelName2 <> '' then
  begin
    Description := Format('%0:s_%1:s', [Description, Trim(ModelName2)]);
  end;
  if PackageName2 <> '' then
  begin
    Description := Format('%0:s_%1:s', [Description, Trim(PackageName2)]);
  end;

  Description := StringReplace(Description, '-', '_', [rfReplaceAll, rfIgnoreCase]);

  result := Format(Str0s1sPeriod2,
    [Description, HufName, KPER, KSTP]);
  if (FResultFormat = mfMt3dConc) and (Precision <> mpMf6Double) then
  begin
    result := Format(Str0sTransportStep, [result, NTRANS]);
  end;
  if FResultFormat in [mfSwrStageAscii, mfSwrStageBinary] then
  begin
    result := Format('%0:s; SWR Step: %1:d', [result, SwrTimeStep]);
  end;
  if TOTIM >= 0 then
  begin
    result := Format(Str0sTotalTime1, [result, TOTIM]);
  end;

  //ModelName1, ModelName2, PackageName1, PackageName2
//  if ModelName1 <> '' then
//  begin
//    result := Format('%0:s_%1:s', [result, Trim(ModelName1)]);
//  end;
//  if PackageName1 <> '' then
//  begin
//    result := Format('%0:s_%1:s', [result, Trim(PackageName1)]);
//  end;
//  if ModelName2 <> '' then
//  begin
//    result := Format('%0:s_%1:s', [result, Trim(ModelName2)]);
//  end;
//  if PackageName2 <> '' then
//  begin
//    result := Format('%0:s_%1:s', [result, Trim(PackageName2)]);
//  end;
end;


function TfrmSelectResultToImport.ReadDataHeadings(AModel: TCustomModel;
  AFileName: string): boolean;
var
  KSTP: Integer;
  AnArray: TModflowDoubleArray;
  A3DArray: T3DTModflowArray;
  KPER: Integer;
  NTRANS: integer;
  PERTIM: TModflowDouble;
  TOTIM: TModflowDouble;
  DESC: TModflowDesc;
  NCOL: Integer;
  NROW: Integer;
  ILAY: Integer;
  DESC2: TModflowDesc2;
  NLAY: Integer;
  Precision: TModflowPrecision;
  HufFormat: boolean;
  LayerIndex: Integer;
  Description: String;
  AFileStream: TFileStream;
  SwrStages: TSwrTimeStages;
  SwrFileType: TSwrFileType;
  SWR_TimeStep: integer;
  SwrIndex: Integer;
  SwrItem: TSwrTimeStage;
  ReachExchanges: TReachExchanges;
  AReachExchange: TReachExchange;
  ReachGroupWaterBudgets: TReachGroupWaterBudgets;
  ABudget: TReachGroupWaterBudget;
  LayerCount: Integer;
  PriorCount: Integer;
  AuxArray: TAuxArrays;
  Mf6Description: string;
  ModelName1, ModelName2, PackageName1, PackageName2: string;
  procedure RecordItem(Description: String;
    ModelName1, ModelName2, PackageName1, PackageName2: String);
  var
    TrimmedDescription: string;
    ItemIndex: Integer;
    ItemPosition: integer;
    ItemList: TList<Integer>;
    Item: string;
  begin
    TrimmedDescription := Trim(Description);
    if (Length(TrimmedDescription) >= 4)
      and (Copy(TrimmedDescription, 1, 3) = 'NET')
      and (TrimmedDescription[4] <> ' ') then
    begin
      TrimmedDescription := 'NET ' + Copy(TrimmedDescription,4, MAXINT);
    end;
    TrimmedDescription := TrimmedDescription + FSpeciesName;
    Item := WriteLabel(TrimmedDescription, AModel, ILAY, KPER, KSTP,
      NTRANS, SWR_TimeStep, TOTIM, Precision,
      ModelName1, ModelName2, PackageName1, PackageName2);
    if clData.Items.IndexOf(Item) < 0 then
    begin
      FPeriods.Add(KPER);
      FSteps.Add(KSTP);
      FTransportSteps.Add(NTRANS);
      FSwrSteps.Add(SWR_TimeStep);
      FDescriptions.Add(TitleCase(TrimmedDescription));
      ItemIndex := clData.Items.Add(Item);

      ItemPosition := FItemDescriptions.IndexOf(Description);
      if ItemPosition >= 0 then
      begin
        ItemList := FItemDescriptions.Objects[ItemPosition] as TList<Integer>;
      end
      else
      begin
        ItemList := TList<Integer>.Create;
        FItemDescriptions.AddObject(Description, ItemList);
      end;
      ItemList.Add(ItemIndex);

      Item := SysUtils.StringReplace(Item, TrimmedDescription, '', []);
      while (Length(Item) > 0) and CharInSet(Item[1], ['_', ':', ' ']) do
      begin
        Item := Copy(Item, 2, MaxInt);
      end;
      if Length(Item) = 0 then
      begin
        Exit;
      end;

      ItemPosition := FItemTimes.IndexOf(Item);
      if ItemPosition >= 0 then
      begin
        ItemList := FItemTimes.Objects[ItemPosition] as TList<Integer>;
      end
      else
      begin
        ItemList := TList<Integer>.Create;
        FItemTimes.AddObject(Item, ItemList);
      end;
      ItemList.Add(ItemIndex);
    end;
  end;
begin
  ILAY := 0;
  result := True;
  NTRANS := 0;
  SWR_TimeStep := 0;
  try
    try
      if not FileExists(AFileName) then
      begin
        result := False;
        Beep;
        MessageDlg(Format(StrSDoesNotExist, [AFileName]), mtError, [mbOK], 0);
        Exit;
      end;

      AFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
      try
        if AFileStream.Size = 0 then
        begin
          result := False;
          Beep;
          MessageDlg(Format(StrSIsEmpty, [AFileName]), mtError, [mbOK], 0);
          Exit;
        end;
      finally
        AFileStream.Free;
      end;

      FFileStream := nil;
      FFileVariable := nil;
      try
        if not OpenResultFile(AFileName, Precision, HufFormat) then
        begin
          result := False;
          Exit;
        end;
      except on EPrecisionReadError do
        begin
          result := False;
          Beep;
          MessageDlg(Format(StrErrorReadingS, [AFileName]), mtError, [mbOK], 0);
          Exit;
        end;
      end;

      case FResultFormat of
        mfMt3dConc:
          begin
            while FFileStream.Position < FFileStream.Size do
            begin
              try
                case Precision of
                  mpSingle:
                    ReadSinglePrecisionMt3dmsBinaryRealArray(FFileStream, NTRANS,
                      KSTP, KPER, TOTIM, DESC, NCOL, NROW, ILAY, AnArray, False);
                  mpDouble:
                    ReadDoublePrecisionMt3dmsBinaryRealArray(FFileStream, NTRANS,
                      KSTP, KPER, TOTIM, DESC, NCOL, NROW, ILAY, AnArray, False);
                  mpMf6Double:
                    ReadDoublePrecisionModflowBinaryRealArray(FFileStream, KSTP,
                      KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray, False);
                  else Assert(False);
                end;
              except on E: EEndOfModflowFileError do
                begin
                  Beep;
                  MessageDlg(E.message, mtError, [mbOK], 0);
                  result := False;
                  Exit;
                end;
              end;
              RecordItem(string(DESC),
                ModelName1, ModelName2, PackageName1, PackageName2);
              if (AModel.ModflowGrid.RowCount <> NROW)
                or (AModel.ModflowGrid.ColumnCount <> NCOL) then
              begin
                Beep;
                MessageDlg(Format(StrTheNumberOfRowsOrColumns, [Trim(string(DESC))]),
                  mtError, [mbOK], 0);
                result := False;
                break;
              end;
            end;
          end;
        mrBinary, mfCSubBinary:
          begin
            LayerCount := AModel.ModflowLayerCount;
            PriorCount := LayerCount;
            while FFileStream.Position < FFileStream.Size do
            begin
              try
                case Precision of
                  mpSingle:
                    ReadSinglePrecisionModflowBinaryRealArray(FFileStream, KSTP,
                      KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray, False);
                  mpDouble:
                    ReadDoublePrecisionModflowBinaryRealArray(FFileStream, KSTP,
                      KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray, False);
                  else Assert(False);
                end;
                if (ILAY = 1) and (PriorCount <> LayerCount) then
                begin
                  Beep;
                  MessageDlg(StrTheFileYouAreTryDifferent, mtError, [mbOK], 0);
                  result := False;
                  Exit;
                end;
                PriorCount := ILAY;
              except on E: EEndOfModflowFileError do
                begin
                  Beep;
                  MessageDlg(E.message, mtError, [mbOK], 0);
                  result := False;
                  Exit;
                end;
              end;
              RecordItem(string(DESC),
                ModelName1, ModelName2, PackageName1, PackageName2);

              if AModel.Grid <> nil then
              begin
                if (AModel.ModflowGrid.RowCount = 1) then
                begin
                  if ((AModel.ModflowLayerCount <> NROW)
                    and (AModel.ModflowGrid.RowCount <> NROW))
                    or (AModel.ModflowGrid.ColumnCount <> NCOL) then
                  begin
                    Beep;
                    MessageDlg(Format(StrTheNumberOfLayersOrCol, [Trim(string(DESC))]),
                      mtError, [mbOK], 0);
                    result := False;
                    break;
                  end;
                end
                else
                begin
                  if (AModel.ModflowGrid.RowCount <> NROW)
                    or (AModel.ModflowGrid.ColumnCount <> NCOL) then
                  begin
                    Beep;
                    MessageDlg(Format(StrTheNumberOfRowsOrColumns, [Trim(string(DESC))]),
                      mtError, [mbOK], 0);
                    result := False;
                    break;
                  end;
                end;
              end
              else
              begin
                Assert(AModel.DisvUsed);
                if (AModel.DisvGrid.RowCount <> NROW)
                  or (AModel.DisvGrid.ColumnCount <> NCOL) then
                begin
                  Beep;
                  MessageDlg(Format(StrTheNumberOfRowsOrColumns, [Trim(string(DESC))]),
                    mtError, [mbOK], 0);
                  result := False;
                  break;
                end;
              end;
            end;
          end;
        mrAscii:
          begin
            while not EOF(FFileVariable.AFile) do
            begin
            try
              ReadModflowAsciiRealArray(FFileVariable, KSTP, KPER,
                PERTIM, TOTIM, DESC2, NCOL, NROW, ILAY, AnArray, False);
            except on E: EEndOfModflowFileError  do
              begin
                Beep;
                MessageDlg(E.message, mtError, [mbOK], 0);
                result := False;
                Exit;
              end;
            end;
              RecordItem(string(DESC2),
                ModelName1, ModelName2, PackageName1, PackageName2);
              if AModel.ModflowGrid.RowCount = 1 then
              begin
                if ((AModel.ModflowLayerCount <> NROW)
                  and (AModel.ModflowGrid.RowCount <> NROW))
                  or (AModel.ModflowGrid.ColumnCount <> NCOL) then
                begin
                  Beep;
                  MessageDlg(Format(StrTheNumberOfLayersOrCol, [Trim(string(DESC2))]),
                    mtError, [mbOK], 0);
                  result := False;
                  break;
                end;
              end
              else
              begin
                if (AModel.ModflowGrid.RowCount <> NROW)
                  or (AModel.ModflowGrid.ColumnCount <> NCOL) then
                begin
                  Beep;
                  MessageDlg(Format(StrTheNumberOfRowsOrColumns, [Trim(string(DESC2))]),
                    mtError, [mbOK], 0);
                  result := False;
                  break;
                end;
              end;
            end;
          end;
        mrFlux:
          begin
            while FFileStream.Position < FFileStream.Size do
            begin
              case Precision of
                mpSingle:
                  ReadModflowSinglePrecFluxArray(FFileStream, KSTP, KPER,
                    PERTIM, TOTIM, DESC, NCOL, NROW, NLAY, A3DArray, AuxArray, HufFormat,
                    False);
                mpDouble:
                  begin
                    ReadModflowDoublePrecFluxArray(FFileStream, KSTP, KPER,
                      PERTIM, TOTIM, DESC, NCOL, NROW, NLAY, A3DArray, AuxArray,
                      AModel.LayerCount, AModel.RowCount, AModel.ColumnCount,
                      HufFormat,
                      ModelName1, ModelName2, PackageName1, PackageName2, False);
                    if Trim(ModelName1) = 'MODFLOW' then
                    begin
                      ModelName1 := '';
                    end;
                    if Trim(ModelName2) = 'MODFLOW' then
                    begin
                      ModelName2 := '';
                    end;
                    if Trim(PackageName1) = 'MODFLOW' then
                    begin
                      PackageName1 := '';
                    end;
                    if Trim(PackageName2) = 'MODFLOW' then
                    begin
                      PackageName2 := '';
                    end;
                    if ModelName1 = ModelName2 then
                    begin
                      ModelName2 :='';
                    end;
                    if PackageName1 = PackageName2 then
                    begin
                      PackageName2 :='';
                    end;
                  end;
                else Assert(False);
              end;
              Mf6Description := Trim(string(DESC));
              if Mf6Description = 'FLOW-JA-FACE' then
              begin
                Continue;
              end;
              RecordItem(string(DESC),
                ModelName1, ModelName2, PackageName1, PackageName2);
              if (frmGoPhast.ModelSelection = msModflow2015) then
              begin
                Mf6Description := Trim(string(DESC));
                if (Mf6Description = 'CSUB-ELASTIC')
                  or (Mf6Description = 'CSUB-INELASTIC') then
                begin
                  Continue;
                end;
                if AModel.DisvUsed then
                begin
                  if (AModel.ModflowLayerCount * AModel.DisvGrid.RowCount
                    * AModel.DisvGrid.ColumnCount)
                    <> (Abs(NLAY) * NROW * NCOL) then
                  begin
                    Beep;
                    MessageDlg(Format(StrTheNumberOfLayersOrCol, [Trim(string(DESC))]),
                      mtError, [mbOK], 0);
                    result := False;
                    break;
                  end;
                end
                else
                begin
                  if (AModel.ModflowLayerCount * AModel.ModflowGrid.RowCount
                    * AModel.ModflowGrid.ColumnCount)
                    <> (Abs(NLAY) * NROW * NCOL) then
                  begin
                    Beep;
                    MessageDlg(Format(StrTheNumberOfRowsOrColumns, [Trim(string(DESC))]),
                      mtError, [mbOK], 0);
                    result := False;
                    break;
                  end;
                end;
              end
              else
              begin
                if (AModel.ModflowGrid.RowCount <> NROW)
                  or (AModel.ModflowGrid.ColumnCount <> NCOL)
                  or (AModel.ModflowLayerCount <> Abs(NLAY)) then
                begin
                  Beep;
                  MessageDlg(Format(StrTheNumberOfRows, [Trim(string(DESC))]),
                    mtError, [mbOK], 0);
                  result := False;
                  break;
                end;
              end;
            end;
          end;
        mrHufAscii:
          begin
            while not EOF(FFileVariable.AFile) do
            begin
              try
                ReadModflowAsciiRealArray(FFileVariable, KSTP, KPER,
                  PERTIM, TOTIM, DESC2, NCOL, NROW, ILAY, AnArray, False);
              except on E: EEndOfModflowFileError  do
                begin
                  Beep;
                  MessageDlg(E.message, mtError, [mbOK], 0);
                  result := False;
                  Exit;
                end;
              end;
              RecordItem(string(DESC2), ModelName1, ModelName2, PackageName1, PackageName2);
              if (AModel.ModflowGrid.RowCount <> NROW)
                or (AModel.ModflowGrid.ColumnCount <> NCOL) then
              begin
                Beep;
                MessageDlg(Format(StrTheNumberOfRowsOrColumns, [Trim(string(DESC2))]),
                  mtError, [mbOK], 0);
                result := False;
                break;
              end;
            end
          end;
        mrHufBinary:
          begin
            while FFileStream.Position < FFileStream.Size do
            begin
              case Precision of
                mpSingle:
                  ReadSinglePrecisionModflowBinaryRealArray(FFileStream, KSTP, KPER,
                    PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray, False);
                mpDouble:
                  ReadDoublePrecisionModflowBinaryRealArray(FFileStream, KSTP, KPER,
                    PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray, False);
                else Assert(False);
              end;
              RecordItem(string(DESC),
                ModelName1, ModelName2, PackageName1, PackageName2);
              if (AModel.ModflowGrid.RowCount <> NROW)
                or (AModel.ModflowGrid.ColumnCount <> NCOL) then
              begin
                Beep;
                MessageDlg(Format(StrTheNumberOfRowsOrColumns, [Trim(string(DESC))]),
                  mtError, [mbOK], 0);
                result := False;
                break;
              end;
            end;
          end;
        mrHufFlux:
          begin
            while FFileStream.Position < FFileStream.Size do
            begin
              case Precision of
                mpSingle:
                  ReadModflowSinglePrecFluxArray(FFileStream, KSTP, KPER,
                    PERTIM, TOTIM, DESC, NCOL, NROW, NLAY, A3DArray, AuxArray, HufFormat,
                    False);
                mpDouble:
                  begin
                    ReadModflowDoublePrecFluxArray(FFileStream, KSTP, KPER,
                      PERTIM, TOTIM, DESC, NCOL, NROW, NLAY, A3DArray, AuxArray,
                      AModel.LayerCount, AModel.RowCount, AModel.ColumnCount,
                      HufFormat,
                      ModelName1, ModelName2, PackageName1, PackageName2, False);
                    if Trim(ModelName1) = 'MODFLOW' then
                    begin
                      ModelName1 := '';
                    end;
                    if Trim(ModelName2) = 'MODFLOW' then
                    begin
                      ModelName2 := '';
                    end;
                    if Trim(PackageName1) = 'MODFLOW' then
                    begin
                      PackageName1 := '';
                    end;
                    if Trim(PackageName2) = 'MODFLOW' then
                    begin
                      PackageName2 := '';
                    end;
                  end;
                else Assert(False);
              end;
              for LayerIndex := 0 to Abs(NLAY) - 1 do
              begin
                ILAY := LayerIndex+1;
                RecordItem(string(DESC),
                  ModelName1, ModelName2, PackageName1, PackageName2);
              end;
              if (AModel.ModflowGrid.RowCount <> NROW)
                or (AModel.ModflowGrid.ColumnCount <> NCOL)
                or (AModel.HydrogeologicUnits.Count <> Abs(NLAY)) then
              begin
                Beep;
                MessageDlg(StrTheNumberOfHydrogeologic,
                  mtError, [mbOK], 0);
                result := False;
                break;
              end;
            end;
          end;
        mfSubBinary:
          begin
            while FFileStream.Position < FFileStream.Size do
            begin
              case Precision of
                mpSingle:
                  ReadSinglePrecisionModflowBinaryRealArray(FFileStream, KSTP,
                    KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray,
                    False);
                mpDouble:
                  ReadDoublePrecisionModflowBinaryRealArray(FFileStream, KSTP,
                    KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray,
                    False);
                else Assert(False);
              end;
              Description := SubsidenceDescription(string(DESC), ILAY);
              RecordItem(Description,
                ModelName1, ModelName2, PackageName1, PackageName2);
              if (AModel.ModflowGrid.RowCount <> NROW)
                or (AModel.ModflowGrid.ColumnCount <> NCOL) then
              begin
                Beep;
                MessageDlg(Format(StrTheNumberOfRowsOrColumns, [Description]),
                  mtError, [mbOK], 0);
                result := False;
                break;
              end;
            end;
          end;
        mfSwrStageAscii, mfSwrStageBinary:
          begin
            if FResultFormat = mfSwrStageAscii then
            begin
              SwrFileType := srtAscii;
            end
            else
            begin
              Assert(FResultFormat = mfSwrStageBinary);
              SwrFileType := srtBinary;
            end;
            SwrStages := TSwrTimeStages.Create;
            try
              ReadSwrTimeStageData(AFileName, SwrFileType, SwrStages);
              for SwrIndex := 0 to SwrStages.Count - 1 do
              begin
                SwrItem := SwrStages[SwrIndex];
                KPER := SwrItem.StressPeriod;
                KSTP := SwrItem.ModflowTimeStep;
                SWR_TimeStep := SwrItem.SwrTimeStep;
                TOTIM := SwrItem.TotalTime;
                RecordItem(StrSWRStage,
                  ModelName1, ModelName2, PackageName1, PackageName2);
              end;
            finally
              SwrStages.Free;
            end;
          end;
        mfSwrReachExchangeAscii, mfSwrReachExchangeBinary:
          begin
            if FResultFormat = mfSwrReachExchangeAscii then
            begin
              SwrFileType := srtAscii;
            end
            else
            begin
              Assert(FResultFormat = mfSwrReachExchangeBinary);
              SwrFileType := srtBinary;
            end;
            ReachExchanges := TReachExchanges.Create;
            try
              ReadSwrReachExchangeData(AFileName, SwrFileType, ReachExchanges);
              for SwrIndex := 0 to ReachExchanges.Count - 1 do
              begin
                AReachExchange := ReachExchanges[SwrIndex];
                KPER := AReachExchange.StressPeriod;
                KSTP := AReachExchange.ModflowTimeStep;
                SWR_TimeStep := AReachExchange.SwrTimeStep;
                TOTIM := AReachExchange.TotalTime;
                RecordItem(StrSWRBottomElevation,
                  ModelName1, ModelName2, PackageName1, PackageName2);
                RecordItem(StrSWRStage,
                  ModelName1, ModelName2, PackageName1, PackageName2);
                RecordItem(StrSWRDepth,
                  ModelName1, ModelName2, PackageName1, PackageName2);
                RecordItem(StrSWRGroundwaterHead,
                  ModelName1, ModelName2, PackageName1, PackageName2);
                RecordItem(StrSWRWettedPerimeter,
                  ModelName1, ModelName2, PackageName1, PackageName2);
                RecordItem(StrSWRConductance,
                  ModelName1, ModelName2, PackageName1, PackageName2);
                RecordItem(StrSWRCalculatedHead,
                  ModelName1, ModelName2, PackageName1, PackageName2);
                RecordItem(StrSWRAquiferReachFl,
                  ModelName1, ModelName2, PackageName1, PackageName2);
              end;
            finally
              ReachExchanges.Free;
            end;
          end;
        mfSwrReachGroupBudgetAscii, mfSwrReachGroupBudgetBinary:
          begin
            if FResultFormat = mfSwrReachGroupBudgetAscii then
            begin
              SwrFileType := srtAscii;
            end
            else
            begin
              Assert(FResultFormat = mfSwrReachGroupBudgetBinary);
              SwrFileType := srtBinary;
            end;
            ReachGroupWaterBudgets := TReachGroupWaterBudgets.Create;
            try
              ReadSwrReachGroupWaterBudgetData(AFileName, SwrFileType,
                ReachGroupWaterBudgets);
              for SwrIndex := 0 to ReachGroupWaterBudgets.Count - 1 do
              begin
                ABudget := ReachGroupWaterBudgets[SwrIndex];
                KPER := ABudget.StressPeriod;
                KSTP := ABudget.ModflowTimeStep;
                SWR_TimeStep := ABudget.SwrTimeStep;
                TOTIM := ABudget.TotalTime;

                RecordItem(StrSWRReachGroupStag,
                  ModelName1, ModelName2, PackageName1, PackageName2);
                RecordItem(StrSWRReachGroupInfl,
                  ModelName1, ModelName2, PackageName1, PackageName2);
                RecordItem(StrSWRReachGroupLate,
                  ModelName1, ModelName2, PackageName1, PackageName2);
                RecordItem(StrSWRReachGroupUZF,
                  ModelName1, ModelName2, PackageName1, PackageName2);
                RecordItem(StrSWRReachGroupRain,
                  ModelName1, ModelName2, PackageName1, PackageName2);
                RecordItem(StrSWRReachGroupEvap,
                  ModelName1, ModelName2, PackageName1, PackageName2);
                RecordItem(StrSWRReachGroupAqui,
                  ModelName1, ModelName2, PackageName1, PackageName2);
                RecordItem(StrSWRReachGroupOutf,
                  ModelName1, ModelName2, PackageName1, PackageName2);
                RecordItem(StrSWRReachGroupExte,
                  ModelName1, ModelName2, PackageName1, PackageName2);
                RecordItem(StrSWRReachGroupStru,
                  ModelName1, ModelName2, PackageName1, PackageName2);
                RecordItem(StrSWRReachGroupCons,
                  ModelName1, ModelName2, PackageName1, PackageName2);
                RecordItem(StrSWRReachGroupVoluChange,
                  ModelName1, ModelName2, PackageName1, PackageName2);
                RecordItem(StrSWRReachGroupFlowDisc,
                  ModelName1, ModelName2, PackageName1, PackageName2);
                RecordItem(StrSWRReachGroupVolu,
                  ModelName1, ModelName2, PackageName1, PackageName2);
              end;
            finally
              ReachGroupWaterBudgets.Free;
            end;
          end;
        else Assert(False);
      end;

    except
      on E: EFOpenError do
      begin
        result := False;
        Beep;
        MessageDlg(Format(StrThereWasAnErrorO, [E.message]), mtError, [mbOK], 0);
        Exit;
      end;
      on E: EInOutError do
      begin
        result := False;
        Beep;
        MessageDlg(Format(StrThereWasAnErrorR, [E.message]), mtError, [mbOK], 0);
        Exit;
      end;
    end;
  finally
    CloseFiles;
  end;
  if clData.Items.Count = 0 then
  begin
    Beep;
    MessageDlg(StrFileIsEmpty, mtError, [mbOK], 0);
    result := False;
  end;
  if clData.Items.Count >= 1 then
  begin
    clData.Checked[clData.Items.Count-1] := True;
    clDataClickCheck(clData);
  end;
end;

procedure TfrmSelectResultToImport.SetDefaultDisplayOption;
var
  Choice: TDisplayChoice;
  DefaultChoice: TDisplayChoice;
begin
  DefaultChoice := dcColor;
  for Choice := Low(TDisplayChoice) to High(TDisplayChoice) do
  begin
    if DisplayChoices[Choice] > DisplayChoices[DefaultChoice] then
    begin
      DefaultChoice := Choice;
    end;
  end;
  rgDisplayChoice.ItemIndex := Ord(DefaultChoice);
end;

procedure TfrmSelectResultToImport.AssignWaterTable(
  OldComments: TStringList; DataSetNames: TStringList;
  NewCreateScreenObjects: TList;
  WaterTableArray: TModflowDoubleArray;
  const Description: string; ValuesToIgnore: TOneDRealArray;
  FileNames: string; AModel: TCustomModel; Precision: TModflowPrecision);
var
  WaterTableData: TDataArray;
  OldComment: string;
  ScreenObject: TScreenObject;
  MinMaxAssigned: Boolean;
  UndoCreateObject: TUndoCreateScreenObject;
begin
  if Description = StrHead then
  begin
    SwrTimeStep := 0;
    CreateOrRetrieveLayerDataSet(StrWaterTable, -1,
      WaterTableData, OldComment, FileNames,
      AModel, TDataArray, Precision, dafWaterTable);
    CreateScreenObject(-1, AModel, ScreenObject, FileNames);
    AssignValues(-1, ScreenObject, WaterTableData, WaterTableArray,
      ValuesToIgnore, AModel, MinMaxAssigned);
    UpdateOldComments(OldComments, WaterTableData, OldComment);
    DataSetNames.AddObject(WaterTableData.Name, WaterTableData);
    UndoCreateObject := TUndoCreateScreenObject.Create(ScreenObject);
    UndoCreateObject.UpdateObservations;
    NewCreateScreenObjects.Add(UndoCreateObject);
//    NewCreateScreenObjects.Add(TUndoCreateScreenObject.Create(ScreenObject));
  end;
end;

procedure TfrmSelectResultToImport.AssignWaterTableArray(
  var WaterTableArray: TModflowDoubleArray;
  ILAY: Integer;
  AnArray: TModflowDoubleArray;
  ValuesToIgnore: TOneDRealArray; const Description: string);
var
  AValue: single;
  ValueIndex: Integer;
  ValueOK: Boolean;
  RowIndex: Integer;
  ColIndex: Integer;
  WaterTableValue: single;
begin
  if (Description = StrHead) then
  begin
    if ILAY = 1 then
    begin
      WaterTableArray := AnArray;
      if Length(WaterTableArray) > 0 then
      begin
        SetLength(WaterTableArray, Length(WaterTableArray),
          Length(WaterTableArray[0]));
      end
      else
      begin
        SetLength(WaterTableArray, Length(WaterTableArray));
      end;
    end
    else
    begin
      for RowIndex := 0 to Length(WaterTableArray) - 1 do
      begin
        for ColIndex := 0 to Length(WaterTableArray[0]) - 1 do
        begin
          ValueOK := True;
          for ValueIndex := 0 to Length(ValuesToIgnore) - 1 do
          begin
            AValue := ValuesToIgnore[ValueIndex];
            WaterTableValue := WaterTableArray[RowIndex, ColIndex];
            if IsNan(WaterTableValue) or  (WaterTableValue = AValue) then
            begin
              ValueOK := False;
              break;
            end;
          end;
          if not ValueOK then
          begin
            WaterTableArray[RowIndex, ColIndex] := AnArray[RowIndex, ColIndex];
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmSelectResultToImport.AdjustTotalTime(var TOTIM: TModflowDouble);
var
  FirstStressPeriod: TModflowStressPeriod;
  ShouldAdjust: Boolean;
  SP_Index: Integer;
  StressPeriod: TModflowStressPeriod;
begin
  if frmGoPhast.PhastModel.ModflowStressPeriods.Count > 1 then
  begin
    FirstStressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods[0];
    if FirstStressPeriod.StressPeriodType = sptSteadyState then
    begin
      ShouldAdjust := True;
      for SP_Index := 1 to frmGoPhast.PhastModel.ModflowStressPeriods.Count - 1 do
      begin
        StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods[SP_Index];
        if StressPeriod.StressPeriodType = sptSteadyState then
        begin
          ShouldAdjust := False;
          break;
        end;
      end;
      if ShouldAdjust then
      begin
        TOTIM := TOTIM - FirstStressPeriod.PeriodLength;
      end;
    end;
  end;
end;

procedure TfrmSelectResultToImport.GetShouldIgnore(
  ValuesToIgnore: TOneDRealArray; Temp: TModflowFloat;
  var ShouldIgnore: Boolean);
var
  Delta: Double;
  IgnoreIndex: Integer;
  IgnoreValue: Double;
const
  Epsilon = 1E-07;
begin
  if IsNan(Temp) then
  begin
    ShouldIgnore := True;
    Exit;
  end;

  ShouldIgnore := False;
  for IgnoreIndex := 0 to Length(ValuesToIgnore) - 1 do
  begin
    IgnoreValue := ValuesToIgnore[IgnoreIndex];
    Delta := Abs(IgnoreValue) * Epsilon;
    if (IgnoreValue - Delta <= Temp) and (IgnoreValue + Delta >= Temp) then
    begin
      ShouldIgnore := True;
      Break;
    end;
  end;
end;

procedure TfrmSelectResultToImport.UpdateCombo;
var
  Index: Integer;
  CurrentItem: string;
  ColorItems: TStringList;
  CheckCount: Integer;
begin
  CheckCount := 0;
  ColorItems := TStringList.Create;
  try
    ColorItems.Add('none');
    CurrentItem := comboColorGrid.Text;
    btnOK.Enabled := False;
    for Index := 0 to clData.Items.Count - 1 do
    begin
      if clData.Checked[Index] then
      begin
        Inc(CheckCount);
        btnOK.Enabled := True;
        ColorItems.Add(clData.Items[Index]);
      end;
    end;
    comboColorGrid.Items := ColorItems;
    comboColorGrid.ItemIndex := ColorItems.IndexOf(CurrentItem);
    if comboColorGrid.ItemIndex < 0 then
    begin
      comboColorGrid.ItemIndex := 0;
    end;
    if (CheckCount = 1) then
    begin
      comboColorGrid.ItemIndex := 1;
    end;
  finally
    ColorItems.Free;
  end;
end;

procedure TfrmSelectResultToImport.AssignObjectName(
  var ScreenObject: TScreenObject; LayerData: TDataArray);
var
  Root: string;
  ExistingObjectCount: Integer;
begin
  Root := LayerData.Name + StrObject;
  ExistingObjectCount := frmGoPhast.PhastModel.
    NumberOfLargestScreenObjectsStartingWith(Root);
  Inc(ExistingObjectCount);
  ScreenObject.Name := Root + IntToStr(ExistingObjectCount);
end;

procedure TfrmSelectResultToImport.Read3DArray(var NLAY: Integer;
   var EndReached: Boolean; var KPER, KSTP: Integer; var TOTIM: TModflowDouble;
   var Description: string; var A3DArray: T3DTModflowArray; var AuxArray: TAuxArrays;
   Precision: TModflowPrecision; HufFormat: boolean; ShouldReadArray: boolean;
   AModel: TCustomModel;
   var ModelName1, ModelName2, PackageName1, PackageName2: string
   );
var
  PERTIM: TModflowDouble;
  DESC: TModflowDesc;
  NCOL: Integer;
  NROW: Integer;
begin
  ModelName1 := '';
  ModelName2 := '';
  PackageName1 := '';
  PackageName2 := '';
  if FFileStream.Position < FFileStream.Size then
  begin
    case Precision of
      mpSingle:
        ReadModflowSinglePrecFluxArray(FFileStream, KSTP, KPER, PERTIM, TOTIM, DESC,
          NCOL, NROW, NLAY, A3DArray, AuxArray, HufFormat, ShouldReadArray);
      mpDouble:
        begin
        ReadModflowDoublePrecFluxArray(FFileStream, KSTP, KPER, PERTIM, TOTIM, DESC,
          NCOL, NROW, NLAY, A3DArray, AuxArray,
          AModel.LayerCount, AModel.RowCount, AModel.ColumnCount,
          HufFormat, ModelName1, ModelName2, PackageName1, PackageName2, ShouldReadArray);

          if Trim(ModelName1) = 'MODFLOW' then
          begin
            ModelName1 := '';
          end;
          if Trim(ModelName2) = 'MODFLOW' then
          begin
            ModelName2 := '';
          end;
          if Trim(PackageName1) = 'MODFLOW' then
          begin
            PackageName1 := '';
          end;
          if Trim(PackageName2) = 'MODFLOW' then
          begin
            PackageName2 := '';
          end;
        end
      else Assert(False);
    end;
    Description := string(Trim(DESC));
    EndReached:= False;
  end
  else
  begin
    EndReached := True;
  end;
end;

procedure TfrmSelectResultToImport.CloseFiles;
begin
  FFileStream.Free;
  if FFileVariable <> nil then
  begin
    CloseFile(FFileVariable.AFile);
  end;
  FFileVariable.Free;
end;

procedure TfrmSelectResultToImport.clTimeStateChange(Sender: TObject;
  Index: Integer);
var
  ItemList: TList<Integer>;
  ItemIndex: Integer;
  Checked: Boolean;
begin
  inherited;
  if FSettingAll then
  begin
    Exit;
  end;
  ItemList := FItemTimes.Objects[Index] as TList<Integer>;
  Checked := clTime.Checked[Index];
  for ItemIndex := 0 to ItemList.Count - 1 do
  begin
    clData.Checked[ItemList[ItemIndex]] := Checked;
  end;
  clDataClickCheck(clData);
end;

procedure TfrmSelectResultToImport.comboClassificationChange(Sender: TObject);
begin
  inherited;
  edPrefix.Enabled := comboClassification.ItemIndex <> 0;
end;

procedure TfrmSelectResultToImport.ReadArray(var AnArray: TModflowDoubleArray;
  var EndReached: Boolean; var NTRANS, KPER, KSTP, ILAY: Integer;
  var TOTIM: TModflowDouble;
  var Description: string; Precision: TModflowPrecision; ShouldReadArray: boolean);
var
  NROW: Integer;
  DESC2: TModflowDesc2;
  DESC: TModflowDesc;
  PERTIM: TModflowDouble;
  NCOL: Integer;
begin
  try
    NTRANS := 0;
    case FResultFormat of
      mfMt3dConc:
        begin
          if FFileStream.Position < FFileStream.Size then
          begin
            case Precision of
              mpSingle:
                ReadSinglePrecisionMt3dmsBinaryRealArray(FFileStream, NTRANS, KSTP,
                  KPER, TOTIM, DESC, NCOL, NROW, ILAY, AnArray,
                  ShouldReadArray);
              mpDouble:
                ReadDoublePrecisionMt3dmsBinaryRealArray(FFileStream, NTRANS, KSTP,
                  KPER, TOTIM, DESC, NCOL, NROW, ILAY, AnArray,
                  ShouldReadArray);
              mpMf6Double:
                ReadDoublePrecisionModflowBinaryRealArray(FFileStream, KSTP,
                  KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray,
                  ShouldReadArray);
              else Assert(False);
            end;
            Description := string(Trim(DESC));
            if NTRANS > FMaxTrans then
            begin
              FMaxTrans := NTRANS;
            end;
          end
          else
          begin
            EndReached := True;
          end;
        end;
      mrBinary, mrHufBinary, mfSubBinary, mfCSubBinary:
        begin
          if FFileStream.Position < FFileStream.Size then
          begin
            case Precision of
              mpSingle:
                ReadSinglePrecisionModflowBinaryRealArray(FFileStream, KSTP,
                  KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray,
                  ShouldReadArray);
              mpDouble:
                ReadDoublePrecisionModflowBinaryRealArray(FFileStream, KSTP,
                  KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray,
                  ShouldReadArray);
              else Assert(False);
            end;
            Description := string(Trim(DESC));
          end
          else
          begin
            EndReached := True;
          end;
        end;
      mrAscii, mrHufAscii:
        begin
          if not EOF(FFileVariable.AFile) then
          begin
            try
              ReadModflowAsciiRealArray(FFileVariable, KSTP, KPER, PERTIM, TOTIM,
                DESC2, NCOL, NROW, ILAY, AnArray, ShouldReadArray);
            except on E: EEndOfModflowFileError  do
              begin
                Beep;
                MessageDlg(E.message, mtError, [mbOK], 0);
                raise;
              end;
            end;
            Description := string(Trim(DESC2));
          end
          else
          begin
            EndReached := True;
          end;
        end;
      else Assert(False);
    end;
    Assert(Length(Description) > 0);
    Description := TitleCase(Description);
  except
    on E: EFOpenError do
    begin
      EndReached := True;
      Beep;
      MessageDlg(Format(StrThereWasAnErrorO, [E.message]), mtError, [mbOK], 0);
      Exit;
    end;
    on E: EInOutError do
    begin
      EndReached := True;
      Beep;
      MessageDlg(Format(StrThereWasAnErrorR, [E.message]), mtError, [mbOK], 0);
      Exit;
    end;
  end;
end;

function TfrmSelectResultToImport.OpenResultFile(AFileName: string;
  out Precision: TModflowPrecision; out HufFormat: boolean): boolean;
var
  Extension: string;
  frmBudgetPrecisionQuery: TfrmBudgetPrecisionQuery;
  AFileStream: TFileStream;
  function GetspeciesName: string;
  begin
    result := ExtractFileName(AFileName);
    result := ChangeFileExt(result, '');
    result := ExtractFileExt(result);
    result := ' ' + Copy(result, 2, MAXINT);
  end;
begin
  result := True;
  Precision := mpSingle;
  Extension := ExtractFileExt(AFileName);
  FSPeciesName := '';
  if (SameText(Extension, StrBdn))
    or (SameText(Extension, StrBhd))
    or (SameText(Extension, StrConc))
    then
  begin
    FResultFormat := mrBinary;
    if SameText(Extension, StrConc) then
    begin
      FSPeciesName := GetspeciesName;
    end;
  end
  else if (SameText(Extension, StrFdn))
    or (SameText(Extension, StrFhd)) then
  begin
    FResultFormat := mrAscii;
  end
  else if (SameText(Extension, StrCbcExt))
    or (SameText(Extension, StrZeta))
    or (SameText(Extension, StrUzfRch))
    or (SameText(Extension, StrUzfDisch))
    then
  begin
    FResultFormat := mrFlux;
  end
  else if (SameText(Extension, StrHuffhd)) then
  begin
    FResultFormat := mrHufAscii;
  end
  else if (SameText(Extension, StrHufbhd)) then
  begin
    FResultFormat := mrHufBinary;
  end
  else if (SameText(Extension, StrHufflow)) then
  begin
    FResultFormat := mrHufFlux;
  end
  else if (SameText(Extension, StrSubOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSubSubOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSubComMlOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSubComIsOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSubVdOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSubNdCritHeadOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSubDCritHeadOut)) then
  begin
    FResultFormat := mfSubBinary;
  end

  else if (SameText(Extension, StrSubElasCompMLOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSubInelasCompMLOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSubElasCompIBOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSubInlasCompIBOut)) then
  begin
    FResultFormat := mfSubBinary;
  end

  else if (SameText(Extension, StrSwtSubOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtComMLOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtComIsOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtVDOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtPreConStrOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtDeltaPreConStrOu)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtGeoStatOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtDeltaGeoStatOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtEffStressOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtDeltaEffStressOu)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtVoidRatioOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtThickCompSedOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtLayerCentElevOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrMt3dConcFile)) then
  begin
    FResultFormat := mfMt3dConc;
    if not frmGoPhast.PhastModel.Mt3dMS_StrictUsed(nil) then
    begin
      result := False;
      Beep;
      MessageDlg(StrMT3DConcentrations, mtError, [mbOK], 0);
      Exit;
    end;
  end
  else if (SameText(Extension, StrSwrReachStageA)) then
  begin
    FResultFormat := mfSwrStageAscii;
  end
  else if (SameText(Extension, StrSwrReachStageB)) then
  begin
    FResultFormat := mfSwrStageBinary;
  end
  else if (SameText(Extension, StrSwrReachExchangeA)) then
  begin
    FResultFormat := mfSwrReachExchangeAscii;
  end
  else if (SameText(Extension, StrSwrReachExchangeB)) then
  begin
    FResultFormat := mfSwrReachExchangeBinary;
  end
  else if (SameText(Extension, StrSwrReachGroupFlowsA)) then
  begin
    FResultFormat := mfSwrReachGroupBudgetAscii;
  end
  else if (SameText(Extension, StrSwrReachGroupFlowsB)) then
  begin
    FResultFormat := mfSwrReachGroupBudgetBinary;
  end

  else if SameText(Extension, StrCsubcmpct)
    or SameText(Extension, StrCsubelstcmpct)
    or SameText(Extension, StrCsubinelstcmpct)
    or SameText(Extension, StrCsubintrbdcmpct)
    or SameText(Extension, StrCsubcrscmpct)
    or SameText(Extension, StrCsubzdis)
    then
  begin
    FResultFormat := mfCSubBinary;
  end

  else
  begin
    result := False;
    Beep;
    MessageDlg(StrTheFileTypeMustB, mtError, [mbOK], 0);
    Exit;
  end;

  HufFormat:= false;
  case FResultFormat of
    mrBinary, mrHufBinary, mfSubBinary:
      begin
        FFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
        if FFileStream.Size > 0 then
        begin
          Precision := CheckArrayPrecision(FFileStream);
        end
        else
        begin
          result := False;
          Beep;
          MessageDlg(StrTheFileYouSelecte, mtError, [mbOK], 0);
          Exit;
        end;
      end;
    mfMt3dConc:
      begin
        FFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
        if FFileStream.Size > 0 then
        begin
          Precision := CheckMt3dmsArrayPrecision(FFileStream);
        end
        else
        begin
          result := False;
          Beep;
          MessageDlg(StrTheFileYouSelecte, mtError, [mbOK], 0);
          Exit;
        end;
      end;
    mrFlux, mrHufFlux:
      begin
        FFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
        if FFileStream.Size > 0 then
        begin
          try
            Precision := CheckBudgetPrecision(FFileStream, HufFormat,
              frmGoPhast.ModelSelection = msModflow2015);
          except on EPrecisionReadError do
            begin
              frmBudgetPrecisionQuery := TfrmBudgetPrecisionQuery.Create(nil);
              try
                frmBudgetPrecisionQuery.ShowModal;
                case frmBudgetPrecisionQuery.rgBudgetPrecision.ItemIndex of
                  0:
                    begin
                      Precision := mpSingle;
                    end;
                  1:
                    begin
                      Precision := mpDouble;
                    end;
                  2:
                    begin
                      MessageDlg(StrUsuallyTheFileWil, mtInformation, [mbOK], 0);
                      Precision := mpSingle;
                    end;
                  3:
                    begin
                      MessageDlg(StrSinglePrecisionMea + StrUsuallyTheFileWil, mtInformation, [mbOK], 0);
                      Precision := mpSingle;
                    end;
                end;
              finally
                frmBudgetPrecisionQuery.Free;
              end;
            end;
          end;
        end
        else
        begin
          result := False;
          Beep;
          MessageDlg(StrTheFileYouSelecte, mtError, [mbOK], 0);
          Exit;
        end;
      end;
    mrAscii, mrHufAscii:
      begin
        FFileVariable := TFileVariable.Create;
        AssignFile(FFileVariable.AFile, AFileName);
        Reset(FFileVariable.AFile);
        if FileSize(FFileVariable.AFile) > 0 then
        begin
          Precision := mpDouble;
        end
        else
        begin
          result := False;
          Beep;
          MessageDlg(StrTheFileYouSelecte, mtError, [mbOK], 0);
          Exit;
        end;
      end;
    mfSwrStageAscii, mfSwrStageBinary,
      mfSwrReachExchangeAscii, mfSwrReachExchangeBinary,
      mfSwrReachGroupBudgetAscii, mfSwrReachGroupBudgetBinary:
      begin
        AFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
        try
          if AFileStream.Size > 0 then
          begin
            Precision := mpDouble;
          end
          else
          begin
            result := False;
            Beep;
            MessageDlg(StrTheFileYouSelecte, mtError, [mbOK], 0);
            Exit;
          end;
        finally
          AFileStream.Free;
        end;
      end;
    mfCSubBinary:
      begin
        Precision := mpDouble;
        FFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
      end;
    else Assert(False);
  end;
end;

procedure TfrmSelectResultToImport.rdgModelsBeforeDrawCell(Sender: TObject;
  ACol, ARow: Integer);
begin
  inherited;
  if (ARow >= rdgModels.FixedRows) and (ACol = Ord(mcFileName)) then
  begin
    if (rdgModels.Cells[ACol, ARow] <> '')
      and rdgModels.Checked[Ord(mcUse), ARow]
      and not FileExists(rdgModels.Cells[ACol, ARow]) then
    begin
      rdgModels.Canvas.Brush.Color := clRed;
    end;
  end;
end;

procedure TfrmSelectResultToImport.rdgModelsButtonClick(Sender: TObject; ACol,
  ARow: Integer);
var
  AModel: TCustomModel;
  AFileName: string;
  Extension: string;
begin
  inherited;
  FFileVariable := nil;
  Assert(ACol = Ord(mcFileName));
  AModel := rdgModels.Objects[Ord(mcModelName), ARow] as TCustomModel;
  Assert(AModel <> nil);
  if FileExists(rdgModels.Cells[ACol, ARow]) then
  begin
    odSelectFiles.FileName := rdgModels.Cells[ACol, ARow];
  end
  else
  begin
    AFileName := DefaultFileName(AModel);
    if (ARow > 2) and FileExists(rdgModels.Cells[ACol, 2]) then
    begin
      Extension := ExtractFileExt(rdgModels.Cells[ACol, 2]);
      AFileName := ChangeFileExt(AFileName, Extension);
    end;
    odSelectFiles.FileName := AFileName;
  end;
  if odSelectFiles.Execute then
  begin
    ReadDataHeadings(AModel, {ARow,} odSelectFiles.FileName);
    AddModelRow(AModel, ARow,  odSelectFiles.FileName);
  end;
end;

{ TUndoImportModelResults }

constructor TUndoImportModelResults.Create(NewDataSets: TList;
  DataSetNames, OldComments: TStringList;
  DisplayDataSet: TDataArray; DisplayChoice: TDisplayChoice;
  AModel: TCustomModel);
var
  DSIndex: Integer;
  DataArray: TDataArray;
  Position: Integer;
begin
  inherited Create;

  FModel := AModel;
  FNewEdgeDisplay := FModel.EdgeDisplay;
  FOldEdgeDisplay := FModel.EdgeDisplay;
  FNew3DDataSet := FModel.ThreeDDataSet;
  FOld3DDataSet := FModel.ThreeDDataSet;
  FNewThreeDTimeList := FModel.ThreeDTimeList;
  FOldThreeDTimeList := FModel.ThreeDTimeList;
  FNewTopDataSet := FModel.TopDataSet;
  FOldTopDataSet := FModel.TopDataSet;
  FNewTopTimeList := FModel.TopTimeList;
  FOldTopTimeList := FModel.TopTimeList;
  FNewFrontDataSet := FModel.FrontDataSet;
  FOldFrontDataSet := FModel.FrontDataSet;
  FNewFrontTimeList := FModel.FrontTimeList;
  FOldFrontTimeList := FModel.FrontTimeList;
  FNewSideDataSet := FModel.SideDataSet;
  FOldSideDataSet := FModel.SideDataSet;
  FNewSideTimeList := FModel.SideTimeList;
  FOldSideTimeList := FModel.SideTimeList;
  FNewTopContourDataSet := FModel.TopContourDataSet;
  FOldTopContourDataSet := FModel.TopContourDataSet;
  FNewFrontContourDataSet := FModel.FrontContourDataSet;
  FOldFrontContourDataSet := FModel.FrontContourDataSet;
  FNewSideContourDataSet := FModel.SideContourDataSet;
  FOldSideContourDataSet := FModel.SideContourDataSet;
  FNew3DContourDataSet := FModel.ThreeDContourDataSet;
  FOld3DContourDataSet := FModel.ThreeDContourDataSet;

  if DisplayDataSet <> nil then
  begin
    case DisplayChoice of
      dcColor:
        begin
          FNewEdgeDisplay := nil;

          FNew3DDataSet := DisplayDataSet;
          FNewThreeDTimeList := nil;

          FNewTopDataSet := DisplayDataSet;
          FNewTopTimeList := nil;

          if DisplayDataSet.Orientation = dso3D then
          begin
            FNewFrontDataSet := DisplayDataSet;
            FNewFrontTimeList := nil;

            FNewSideDataSet := DisplayDataSet;
            FNewSideTimeList := nil;
          end
          else
          begin
            FNewFrontDataSet := nil;
            FNewFrontTimeList := nil;

            FNewSideDataSet := nil;
            FNewSideTimeList := nil;
          end;
        end;
      dcContour:
        begin
          FNew3DContourDataSet := DisplayDataSet;
          FNewTopContourDataSet := DisplayDataSet;

          if DisplayDataSet.Orientation = dso3D then
          begin
            FNewFrontContourDataSet := DisplayDataSet;
            FNewSideContourDataSet := DisplayDataSet;
          end
          else
          begin
            FNewFrontContourDataSet := nil;
            FNewSideContourDataSet := nil;
          end;
        end;
      dcNone:
        begin
          // do nothing
        end;
      else Assert(False);
    end;
//    if frmDisplayData = nil then
//    begin
//      Application.CreateForm(TfrmDisplayData, frmDisplayData);
//    end;
//    UpdateFrmDisplayData(True);
  end;

  FContainedUndos := TObjectList.Create;
  FNewDataSets := TList.Create;
  FNewDataSets.Assign(NewDataSets);

  FOldComments:= TStringList.Create;
  FNewComments:= TStringList.Create;

  for DSIndex := 0 to NewDataSets.Count - 1 do
  begin
    DataArray := NewDataSets[DSIndex];
    Position := DataSetNames.IndexOf(DataArray.Name);
    if Position >= 0 then
    begin
      DataSetNames.Delete(Position);
      Position := OldComments.IndexOfObject(DataArray);
      if Position >= 0 then
      begin
        OldComments.Delete(Position);
      end;
    end;
  end;
  FOldComments.Assign(OldComments);
  FNewComments.Capacity := DataSetNames.Count;
  for DSIndex := 0 to DataSetNames.Count - 1 do
  begin
    DataArray := DataSetNames.Objects[DSIndex] as TDataArray;
    FNewComments.AddObject(DataArray.Comment, DataArray);
  end;

end;

function TUndoImportModelResults.Description: string;
begin
  result := StrImportModelResults;
end;

destructor TUndoImportModelResults.Destroy;
begin
  FNewDataSets.Free;
  FContainedUndos.Free;
  FOldComments.Free;
  FNewComments.Free;
  inherited;
end;

procedure TUndoImportModelResults.DoCommand;
var
  Index: Integer;
  AnUndo: TCustomUndo;
  DataSet: TDataArray;
  DataArrayManager: TDataArrayManager;
begin
  inherited;
  frmGoPhast.PhastModel.BeginDataSetUpdate;
  DisallowChildGridUpdates;
  try
    DataArrayManager := FModel.DataArrayManager;
    DataArrayManager.HandleAddedDataArrays(FNewDataSets);

    for Index := 0 to FContainedUndos.Count - 1 do
    begin
      AnUndo := FContainedUndos[Index];
      AnUndo.DoCommand;
    end;
    for Index := 0 to DataArrayManager.DataSetCount - 1 do
    begin
      DataSet := DataArrayManager.DataSets[Index];
      if FNewDataSets.IndexOf(DataSet) < 0 then
      begin
        FModel.CreateVariables(DataSet);
      end;
    end;

    SetComments(FNewComments);

    FModel.EdgeDisplay := FNewEdgeDisplay;
    FModel.ThreeDDataSet := FNew3DDataSet;
    FModel.ThreeDTimeList := FNewThreeDTimeList;
    FModel.TopDataSet := FNewTopDataSet;
    FModel.TopTimeList := FNewTopTimeList;
    FModel.FrontDataSet := FNewFrontDataSet;
    FModel.FrontTimeList := FNewFrontTimeList;
    FModel.SideDataSet := FNewSideDataSet;
    FModel.SideTimeList := FNewSideTimeList;
    FModel.TopContourDataSet := FNewTopContourDataSet;
    FModel.FrontContourDataSet := FNewFrontContourDataSet;
    FModel.SideContourDataSet := FNewSideContourDataSet;
    FModel.ThreeDContourDataSet := FNew3DContourDataSet;
    FModel.DiscretizationChanged;
  finally
    AllowChildGridUpdates;
    frmGoPhast.PhastModel.EndDataSetUpdate;
  end;

  UpdateFrmDisplayData;
//  UpdateFrmContourData;
  UpdateFrmGridValue;

end;

procedure TUndoImportModelResults.Undo;
var
  Index: Integer;
  AnUndo: TCustomUndo;
  DataArrayManager: TDataArrayManager;
begin
  inherited;
  DisallowChildGridUpdates;
  try
    for Index := FContainedUndos.Count - 1 downto 0 do
    begin
      AnUndo := FContainedUndos[Index];
      AnUndo.Undo;
    end;
    SetComments(FOldComments);

    FModel.EdgeDisplay := FOldEdgeDisplay;
    FModel.ThreeDDataSet := FOld3DDataSet;
    FModel.ThreeDTimeList := FOldThreeDTimeList;
    FModel.TopDataSet := FOldTopDataSet;
    FModel.TopTimeList := FOldTopTimeList;
    FModel.FrontDataSet := FOldFrontDataSet;
    FModel.FrontTimeList := FOldFrontTimeList;
    FModel.SideDataSet := FOldSideDataSet;
    FModel.SideTimeList := FOldSideTimeList;
    FModel.TopContourDataSet := FOldTopContourDataSet;
    FModel.FrontContourDataSet := FOldFrontContourDataSet;
    FModel.SideContourDataSet := FOldSideContourDataSet;
    FModel.ThreeDContourDataSet := FOld3DContourDataSet;
    FModel.DiscretizationChanged;


    DataArrayManager := FModel.DataArrayManager;
    DataArrayManager.HandleDeletedDataArrays(FNewDataSets);
    finally
    AllowChildGridUpdates;

  end;
  UpdateFrmDisplayData;
//  UpdateFrmContourData;
  UpdateFrmGridValue;
end;

procedure TUndoImportModelResults.SetComments(Comments: TStringList);
var
  Index: Integer;
  DataArray: TDataArray;
begin
  for Index := 0 to Comments.Count - 1 do
  begin
    DataArray := Comments.Objects[Index] as TDataArray;
    DataArray.Comment := Comments[Index];
  end;
end;

{ TFormulaAssigner }

procedure TFormulaAssigner.AddFormula(AFormula: string; AModel: TBaseModel);
begin
  FFormulas.Add(AFormula);
  FModels.Add(AModel);
end;

procedure TFormulaAssigner.AssignFinalFormula;
var
  FinalFormula: string;
  ModelPosition: Integer;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  Assert(FFormulas.Count = FModels.Count);
  Assert(FFormulas.Count >= 1);
  if FFormulas.Count = 1 then
  begin
    FDataArray.Formula := FFormulas[0];
  end
  else
  begin
    Assert(frmGoPhast.PhastModel.LgrUsed);
    FinalFormula := 'If('+ StrGridNumber + ' <= '
      + IntToStr(frmGoPhast.PhastModel.ChildModels.Count+1) + ', Case(' + StrGridNumber;
    ModelPosition := FModels.IndexOf(frmGoPhast.PhastModel);
    if ModelPosition >= 0 then
    begin
      FinalFormula := FinalFormula + ', ' + FFormulas[ModelPosition];
    end
    else
    begin
      FinalFormula := FinalFormula + ', 0'
    end;
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      ModelPosition := FModels.IndexOf(ChildModel);
      if ModelPosition >= 0 then
      begin
        FinalFormula := FinalFormula + ', ' + FFormulas[ModelPosition];
      end
      else
      begin
        FinalFormula := FinalFormula + ', 0'
      end;
    end;
    FinalFormula := FinalFormula + '0), 0)';
    FDataArray.Formula := FinalFormula;
  end;
end;

constructor TFormulaAssigner.Create(ADataArray: TDataArray);
begin
  FDataArray := ADataArray;
  FFormulas := TStringList.Create;
  FModels:= TList.Create;
end;

destructor TFormulaAssigner.Destroy;
begin
  FFormulas.Free;
  FModels.Free;
  inherited;
end;

{ TFormulaAssignerList }

procedure TFormulaAssignerList.AssignFinalFormulas;
var
  Index: Integer;
  AFormulaAssigner: TFormulaAssigner;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    AFormulaAssigner := FList[Index];
    AFormulaAssigner.AssignFinalFormula;
  end;
end;

constructor TFormulaAssignerList.Create;
begin
  FList := TObjectList.Create;
end;

destructor TFormulaAssignerList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFormulaAssignerList.GetFormulaAssigner(
  ADataArray: TDataArray): TFormulaAssigner;
var
  Index: Integer;
  AFormulaAssigner: TFormulaAssigner;
begin
  Assert(ADataArray <> nil);
  Assert(ADataArray.Model = frmGoPhast.PhastModel);
  for Index := 0 to FList.Count - 1 do
  begin
    AFormulaAssigner := FList[Index];
    if AFormulaAssigner.DataArray = ADataArray then
    begin
      result := AFormulaAssigner;
      Exit;
    end;
  end;
  result := TFormulaAssigner.Create(ADataArray);
  FList.Add(result);
end;

end.
