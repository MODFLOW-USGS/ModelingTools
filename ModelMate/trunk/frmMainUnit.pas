unit frmMainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, StdCtrls, ExtCtrls,
  ToolWin, ActnList, ImgList, IniFiles, Math,
  ModelMateUtilities, frmAddGroupUnit, Grids,  frmConfigureParTablesUnit,
  frmUcodeSettingsUnit, frmParEstSettingsUnit, frmHelpUcodeMethodsUnit,
  frmAddParOrDepUnit, DB,
  GlobalBasicData, GlobalData, GlobalTypesUnit, JupiterUnit, UcodeUnit,
  DBClient, DataGrid, ModelMateClassesUnit, frmAbout, frmProjectName,
  frmModelCommandsUnit, frmModelIO, frmModelSettings, frmOutput,
  DependentsUnit, frmObservations, frmPredictions, Utilities,
  JclSysUtils, frmProgramLocationsUnit, ModflowUnit, Modflow2005Unit,
  SyncObjs, JvExStdCtrls, JvMemo, JvEdit,
  Link_Jupiter, frmEditor, frmStartUp, frmUcodeBasicSettingsUnit,
  frmPriorInfo, PriorInfoUnit, JvCheckBox, JvExExtCtrls, JvNetscapeSplitter,
  sskutils, frmDerivedParameters, frmParallelControl, frmParallelRunners,
  JvExComCtrls, JvProgressBar, RbwDataGrid4, Buttons,
  frmPriorInfoControl, ParallelControlUnit, frmRenameGroup, JvExControls,
  JvPageList, frmModelDirectories,
  IniFileUtilities, JvBaseDlg, JvProgressDialog, System.Actions,
  System.ImageList;

type
  TFormMain = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    NewProject1: TMenuItem;
    OpenProject1: TMenuItem;
    SaveProject1: TMenuItem;
    SaveProjectAs1: TMenuItem;
    ExampleProjects1: TMenuItem;
    RunProject1: TMenuItem;
    N1: TMenuItem;
    FileEditor1: TMenuItem;
    NewFile1: TMenuItem;
    ExistingFile1: TMenuItem;
    N2: TMenuItem;
    ExitModelMate: TMenuItem;
    Model1: TMenuItem;
    ModelOutputAndInstructionFiles: TMenuItem;
    View1: TMenuItem;
    File2: TMenuItem;
    ApplicationOutput1: TMenuItem;
    Help1: TMenuItem;
    Help2: TMenuItem;
    AboutModelMate1: TMenuItem;
    GroupBox1: TGroupBox;
    pcMain: TPageControl;
    tsParams: TTabSheet;
    tsObs: TTabSheet;
    Commands1: TMenuItem;
    NameandUnits1: TMenuItem;
    ModelCalculatedDerivatives1: TMenuItem;
    StatusBar1: TStatusBar;
    tbarMain: TToolBar;
    tbNew: TToolButton;
    tbOpen: TToolButton;
    tbSave: TToolButton;
    tbSaveAs: TToolButton;
    tbRevert: TToolButton;
    ImageList1: TImageList;
    ActionList1: TActionList;
    actFileNew: TAction;
    actFileOpen: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    RevertProject: TMenuItem;
    actRevertProject: TAction;
    GroupBox2: TGroupBox;
    CloseAllChildWindows1: TMenuItem;
    actCloseChildWin: TAction;
    N3: TMenuItem;
    InvokeModelViewer1: TMenuItem;
    sdProject: TSaveDialog;
    UCODEHelp1: TMenuItem;
    odProject: TOpenDialog;
    tsPreds: TTabSheet;
    Project1: TMenuItem;
    Rename1: TMenuItem;
    actProjectName: TAction;
    actUcodeFiles: TAction;
    ModelInputandTemplateFiles1: TMenuItem;
    actModelInput: TAction;
    actModelOutput: TAction;
    actImportUcodeMainFile: TAction;
    actImportObservationDataBlock: TAction;
    actImportParameterDataBlock: TAction;
    btnObsForm: TButton;
    ProgramLocations1: TMenuItem;
    CheckProject1: TMenuItem;
    actImportModflow2005Parameters: TAction;
    actImportModflow2005Observations: TAction;
    N6: TMenuItem;
    SupportedModels1: TMenuItem;
    actCheckProject: TAction;
    P1: TMenuItem;
    ModelFit1: TMenuItem;
    Sensitivity1: TMenuItem;
    Regression1: TMenuItem;
    StartGWChart1: TMenuItem;
    actExit: TAction;
    dgObsGpSummary: TEcDataGrid;
    Label8: TLabel;
    Label9: TLabel;
    jvmNumObsGps: TJvMemo;
    jvmNumObs: TJvMemo;
    odUcode: TOpenDialog;
    jvmNumPred: TJvMemo;
    jvmNumPredGps: TJvMemo;
    Label1: TLabel;
    Label10: TLabel;
    dgPredGpSummary: TEcDataGrid;
    btnPredForm: TButton;
    actOpenEditor: TAction;
    OpenEditor: TMenuItem;
    ViewUcodeOutput: TMenuItem;
    ViewUcodeInputFile: TMenuItem;
    N7: TMenuItem;
    PredictiveModelInputandTemplateFiles1: TMenuItem;
    PredictiveModelOutputandInstructionFiles1: TMenuItem;
    N8: TMenuItem;
    actModelInputPred: TAction;
    actModelOutputPred: TAction;
    actImportModflow2005Predictions: TAction;
    actInvokeGW_Chart: TAction;
    tbGWChart: TToolButton;
    pnlUpper: TPanel;
    lblParTable: TLabel;
    btnAddParameter: TButton;
    btnDeleteSelectedParameters: TButton;
    pnlLower: TPanel;
    lblParGPSTable: TLabel;
    btnAddGroup: TButton;
    btnDeleteSelParGps: TButton;
    JvNetscapeSplitter1: TJvNetscapeSplitter;
    actResidualAnalysis: TAction;
    Residualanalysis1: TMenuItem;
    Label2: TLabel;
    Label11: TLabel;
    actImport_dm: TAction;
    actImport_paopt: TAction;
    actImport_pasub: TAction;
    actRefresh: TAction;
    Refresh1: TMenuItem;
    actConfigParamTables: TAction;
    actResidualAnalysisAdv: TAction;
    ResidualAnalysisAdv1: TMenuItem;
    actParallelControl: TAction;
    actParallelRunners: TAction;
    miParallelProcessing: TMenuItem;
    ParallelControl1: TMenuItem;
    ParallelRunners1: TMenuItem;
    actStartLocalRunners: TAction;
    StartLocalRunners1: TMenuItem;
    Import: TMenuItem;
    actImportUcodeMainFile1: TMenuItem;
    N10: TMenuItem;
    actImportModflow2005ParamsAndObs: TAction;
    actImportModflow2005ParamsAndObs1: TMenuItem;
    ImportParametersfromMODFLOW20051: TMenuItem;
    ImportObservationsMODFLOW20051: TMenuItem;
    ImportMODFLOW2005Predictions1: TMenuItem;
    N11: TMenuItem;
    ParametersandObservationsFromMODFLOW20001: TMenuItem;
    ParametersFromMODFLOW20002: TMenuItem;
    ObservationsFromMODFLOW20002: TMenuItem;
    N4: TMenuItem;
    rbwdgParams: TRbwDataGrid4;
    rbwdgParamGps: TRbwDataGrid4;
    LinearUncertainty1: TMenuItem;
    N5: TMenuItem;
    OptimizedParameterValuesfrompaoptFile1: TMenuItem;
    actCreateInstructionFilesForModelMuseObs: TAction;
    CreateInstructionFilesForModelMuseObs1: TMenuItem;
    actCreateInstructionFilesForModelMusePreds: TAction;
    CreateInstructionFilesForPredictionsDefinedInModelMuse1: TMenuItem;
    miUCODE: TMenuItem;
    miUcodeFileNames: TMenuItem;
    miUcodeSettings: TMenuItem;
    miParamEstSettings: TMenuItem;
    miDerivedParams: TMenuItem;
    miPriorInfo: TMenuItem;
    cmbAppChooser: TComboBox;
    Label3: TLabel;
    plApp: TJvPageList;
    pgUCODE: TJvStandardPage;
    pgPest: TJvStandardPage;
    GroupBox5: TGroupBox;
    Button3: TButton;
    Button4: TButton;
    btnConfigParTables: TButton;
    btnRenameGp: TButton;
    GroupBox3: TGroupBox;
    rbForward: TRadioButton;
    rbSenAnal: TRadioButton;
    rbParEst: TRadioButton;
    rbTestLin: TRadioButton;
    rbPred: TRadioButton;
    rbAdvTestLin: TRadioButton;
    rbNonLinUnc: TRadioButton;
    rbInvObjFn: TRadioButton;
    Button1: TButton;
    btnRunUcode: TButton;
    miModelDirectories: TMenuItem;
    actModelDir: TAction;
    JvProgressDialog1: TJvProgressDialog;
    procedure AboutModelMate1Click(Sender: TObject);
    procedure actCheckProjectExecute(Sender: TObject);
    procedure actCloseChildWinExecute(Sender: TObject);
    procedure actConfigParamTablesExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actFileNewExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actImport_dmExecute(Sender: TObject);
    procedure actImport_paoptExecute(Sender: TObject);
    procedure actImport_pasubExecute(Sender: TObject);
    procedure actImportModflow2005ObservationsExecute(Sender: TObject);
    procedure actImportModflow2005ParametersExecute(Sender: TObject);
    procedure actImportModflow2005ParamsAndObsExecute(Sender: TObject);
    procedure actImportModflow2005PredictionsExecute(Sender: TObject);
    procedure actImportObservation(Sender: TObject);
    procedure actImportParameterDataBlockExecute(Sender: TObject);
    procedure actImportUcodeMainFileExecute(Sender: TObject);
    procedure actInvokeGW_ChartExecute(Sender: TObject);
    procedure actModelInputExecute(Sender: TObject);
    procedure actModelInputPredExecute(Sender: TObject);
    procedure actModelOutputExecute(Sender: TObject);
    procedure actModelOutputPredExecute(Sender: TObject);
    procedure actOpenEditorExecute(Sender: TObject);
    procedure actParallelControlExecute(Sender: TObject);
    procedure actParallelRunnersExecute(Sender: TObject);
    procedure actProjectNameExecute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actResidualAnalysisAdvExecute(Sender: TObject);
    procedure actResidualAnalysisExecute(Sender: TObject);
    procedure actRevertProjectExecute(Sender: TObject);
    procedure actStartLocalRunnersExecute(Sender: TObject);
    procedure btnAddGroupClick(Sender: TObject);
    procedure btnAddParameterClick(Sender: TObject);
    procedure btnDeleteSelectedParametersClick(Sender: TObject);
    procedure btnDeleteSelParGpsClick(Sender: TObject);
    procedure btnDerivedParamsClick(Sender: TObject);
    procedure btnExportUcodeClick(Sender: TObject);
    procedure btnHelpMethodsClick(Sender: TObject);
    procedure btnMoreUcodeClick(Sender: TObject);
    procedure btnObsFormClick(Sender: TObject);
    procedure btnParallelClick(Sender: TObject);
    procedure btnParEstWindowClick(Sender: TObject);
    procedure btnPredFormClick(Sender: TObject);
    procedure btnPriorFormClick(Sender: TObject);
    procedure btnRunUcodeClick(Sender: TObject);
    procedure Commands1Click(Sender: TObject);
    procedure EnableSaveRevert;
    procedure FormActivate(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gpbxModeClick(Sender: TObject);
    procedure GroupBox1Click(Sender: TObject);
    procedure GroupBox2Click(Sender: TObject);
    function OKToCloseProject: boolean;
    function OpenAFile(FileName: string): boolean;
    procedure ParametersAndObservationsFromMODFLOW2005Click(Sender: TObject);
    procedure pcAppChanging(Sender: TObject; var AllowChange: Boolean);
    procedure PopulateControls(Prj: TProject);
    procedure ProgramLocations1Click(Sender: TObject);
    procedure rbParEstClick(Sender: TObject);
    procedure rbForwardClick(Sender: TObject);
    procedure rbSenAnalClick(Sender: TObject);
    procedure rbTestLinClick(Sender: TObject);
    procedure rbPredClick(Sender: TObject);
    procedure rbNonLinUncClick(Sender: TObject);
    procedure rbInvObjFnClick(Sender: TObject);
    procedure rbAdvTestLinClick(Sender: TObject);
    procedure SaveProjectFile(FileName: string);
    procedure StatusBar1Click(Sender: TObject);
    procedure StatusClear;
    procedure SupportedModels1Click(Sender: TObject);
    procedure tsObsShow(Sender: TObject);
    procedure rbwdgParamsAfterDelete(Sender: TObject);
    procedure tsPredsShow(Sender: TObject);
    procedure ViewUcodeOutputClick(Sender: TObject);
    procedure ViewUcodeInputFileClick(Sender: TObject);
    procedure StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure popmenuUcodePopup(Sender: TObject);
    procedure actCreateInstructionFilesForModelMuseObsExecute(Sender: TObject);
    procedure actCreateInstructionFilesForModelMusePredsExecute(
      Sender: TObject);
    //
    // rbwdgParams
    procedure rbwdgParamsColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure rbwdgParamsEndUpdate(Sender: TObject);
    procedure rbwdgParamsEnter(Sender: TObject);
    procedure rbwdgParamsExit(Sender: TObject);
    procedure rbwdgParamsGetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
    procedure rbwdgParamsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rbwdgParamsPopulate(psSource: TParamSet);
    procedure rbwdgParamsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rbwdgParamsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rbwdgParamsStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    //
    // rbwdgParamGps
    procedure rbwdgParamGpsColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure rbwdgParamGpsEndUpdate(Sender: TObject);
    procedure rbwdgParamGpsExit(Sender: TObject);
    procedure rbwdgParamGpsGetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
    procedure rbwdgParamGpsPopulate(const psSource: TParamSet);
    procedure rbwdgParamGpsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rbwdgParamGpsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rbwdgParamGpsStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure miUcodeFileNamesClick(Sender: TObject);
    procedure miUcodeSettingsClick(Sender: TObject);
    procedure miParamEstSettingsClick(Sender: TObject);
    procedure miDerivedParamsClick(Sender: TObject);
    procedure miPriorInfoClick(Sender: TObject);
    procedure rbwdgParamGpsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rbwdgParamGpsEnter(Sender: TObject);
    procedure cmbAppChooserChange(Sender: TObject);
    procedure btnRenameGpClick(Sender: TObject);
    procedure JvNetscapeSplitter1Moved(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure actModelDirExecute(Sender: TObject);
    procedure tsParamsShow(Sender: TObject);
  private
    { Private declarations }
    FreezeParamNames: boolean;
    IgnoreGpsStateChange: boolean;
    IgnoreParStateChange: boolean;
    UseCurrentNameFile: boolean;
    NumRunnersStarted: integer;
    ParamCellChanged, ParamGpCellChanged: boolean;
    ParamGpCellCurrent: GridCell;
    ParamCellText, ParamGpCellText: string;
    ParamGpSelected: string;
    ProjFileTemp: string;
    LastCol, LastRow: integer;
    FormIsOpen: boolean;
    FIniFile: TMemInifile;
    procedure AssignCurrent;
    procedure AssignParameterGroups;
    function AssignParameters: boolean;
    procedure ClearAllData;
    procedure CountParLines(const RbwGrid: TRbwDataGrid4; var NLines: integer; var NNames: integer;
                            var FirstBlankRow: integer);
    function ExportUcode(var ExportErrMsg: string): integer;
    function GetOptParamSet(var psOpt: TParamSet;
                            const fnPaopt: TFileName): boolean;
    procedure GetParametersFromPackageFiles(NameFile: TFileName;
                                            psPkg: TParamSet);
    procedure GetParametersFromPvalFile(PVFile: TFileName; psPval: TParamSet;
                                        ModelUse: TModelUse);
    procedure ReplaceParAttExpl(var TestString: string; PatTemp: TParamAttType);
    procedure ReplaceParAttValue(var TestString: string; PatTemp: TParamAttType);
    procedure RunUcode;
    procedure ShowObsForm;
    procedure ShowPredForm;
    procedure ShowPriForm;
    procedure PopulateObsTabSheet(const Project: TProject);
    procedure PopulatePredTabSheet(const Project: TProject);
    procedure ProcessProjectFile(ProjectFile: string);
    procedure OpenProgramLocations;
    procedure SetCaption;
    procedure ViewFile(FName: TFileName);
    function SelectPaoptFile(var fnPaopt: TFileName): boolean;
    procedure UpdateMainForm;
    procedure InitializeMemoryObjects;
    procedure SaveParamGpsChanges;
    procedure CheckParamGpNames;
    procedure CreateInstructionFilesForModelMuseDeps(const ModelUse: TModelUse);
    procedure ColorRadioButtons(ActiveButton: TRadioButton);
    procedure AssignProjectDirectory(NewProjectPath: string; OrigProjDir: string);
  public
    { Public declarations }
    property IniFile: TMemInifile read FIniFile;
    procedure ReadIniFile;
    procedure WriteIniFile;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

var
  KeepCurrent: boolean;
  ParamSetLocal: TParamSet;
  PreventDataLoss: boolean = True;
  NeedPvalTemplate: boolean = False;
  ParamCellSelected: boolean = False;
  ParamGpsChanged: boolean = False;
  LoadingDone: boolean = True;

procedure TFormMain.FormActivate(Sender: TObject);
begin
  EnableSaveRevert;
end;

procedure TFormMain.FormClick(Sender: TObject);
begin
  StatusClear;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Idum := 1;
  FormIsOpen := False;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := OKToCloseProject;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FormIsOpen := False;
  CenterForm(self);
  if (GlobalBasicData.FileToBeOpened = '') or GlobalBasicData.StartNewProject then
  begin
    FormStartUp.ShowModal;
  end;
  FormStartUp.Free;
  // Assign global variables.
  FormatSettings.DecimalSeparator := '.';
  KeepCurrent := False;
  UCParEstSetWinOpen := False;
  // Create some objects.
  ParamSetLocal := TParamSet.Create;
  FormEditorAppInput := TFormEditor.Create(Self);
  FormEditorAppInput.Left := 40;
  FormEditorAppInput.Top := 60;
  FormEditorAppOutput := TFormEditor.Create(Self);
  FormEditorAppOutput.Left := 10;
  FormEditorAppOutput.Top := 90;
  cmbAppChooser.ItemIndex := 0;
  plApp.ActivePage := plApp.Pages[0];
  FreezeParamNames := True;
  PCurrent := TProject.Create(Self);
  ReadIniFile;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  //WriteIniFile;
  IniFile.Free;
  ParamSetLocal.Free;
  PCurrent.Free;
  PDefault.Free;
  PLastSaved.Free;
  ParameterSetupDefault.Free;
  ParameterSetupCurrent.Free;
  ParameterSetupLastSaved.Free;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  if FormIsOpen then
    SetCaption;
end;

procedure TFormMain.FormShow(Sender: TObject);
var
  DirLocal, DirSep, Str: string;
begin
  if OpenMainform then
    begin
      FormIsOpen := True;
      IgnoreGpsStateChange := False;
      IgnoreParStateChange := False;
      LoadingDone := False;
      PDefault := TProject.Create(Self);
      PLastSaved := TProject.Create(Self);
      ParameterSetupDefault := TParameterSetup.Create(Self);
      ParameterSetupCurrent := TParameterSetup.Create(Self);
      ParameterSetupLastSaved := TParameterSetup.Create(Self);
      ParamSetCurrent := TParamSet.Create;
      ParamGpsDefault := TParamSet.Create;
      ParamGpsDefault.Add;
      ParamGpsDefault.Items[0].Initialize('ParamDefault','ParamDefault');
      ParamGpsCurrent := TParamSet.Create;
      ParamGpsCurrent.Add;
      ParamGpsCurrent.Items[0].Initialize('ParamDefault','ParamDefault');
      ParamGpsLastSaved := TParamSet.Create;
      KeepCurrent := False;
      PopulateControls(PDefault);
      PLastSaved.Assign(PDefault);
      KeepCurrent := True;
      rbwdgParamGpsPopulate(ParamGpsCurrent);
      rbwdgParamsPopulate(ParamSetCurrent);
      UseCurrentNameFile := False;
      if GlobalBasicData.FileToBeOpened <> '' then
        begin
          ProjectDirectory := ExtractFileDir(FileToBeOpened);
          if GlobalBasicData.StartNewProject then
            // user has entered a non-existent filename;
            // define ProjName and save new project to specified filename
            begin
              PCurrent.FileName := FileToBeOpened;
              Str := FileBaseName(PCurrent.FileName);
              if PCurrent.NameLikeProjectFile then
                begin
                  PCurrent.ProjName := ConvertString255(FileBaseName(PCurrent.FileName));
                end;
              DirLocal := ExtractFileDir(PCurrent.FileName);
              DirSep := PathDelimiter;
              PCurrent.UcProject.MainInputFileName := DirLocal + DirSep + 'Default_Ucode_main.in';
              PCurrent.UcProject.MainInputFileNamePred := DirLocal + DirSep + 'Default_Ucode_pred.in';
              PCurrent.UcProject.OutputPrefix := 'Default_Ucode_main_out';
              PCurrent.UcProject.OutputPrefixPred := 'Default_Ucode_pred_out';
              actFileSaveExecute(self);
              ProjChanged := False;
            end
          else
            // open existing project file
            begin
      JvProgressDialog1.InitValues(0,100,1,0,'Progress','Opening project file');
      JvProgressDialog1.Smooth := True;
      JvProgressDialog1.ShowCancel := False;
      JvProgressDialog1.Show;
      JvProgressDialog1.Position := 10;
              Str := FileBaseName(FileToBeOpened);
              PCurrent.FileName := FileToBeOpened;
              if OpenAFile(FileToBeOpened) then;
                begin
                  JvProgressDialog1.Position := 40;
                  ProcessProjectFile(FileToBeOpened);
                end;
            end;
          FileToBeOpened := '';
        end;
      JvProgressDialog1.Position := 80;
      PLastSaved.Assign(PCurrent);
      JvProgressDialog1.Position := 90;
      UCChanged := False;
      ParamGpsChanged := False;
      ParamGpSelected := '';
      ParamCellText := '';
      ParamGpCellText := '';
      ParamCellChanged := False;
      ParamGpCellChanged := False;
      UpdateMainForm;
      JvProgressDialog1.Position := 95;
      ProjChanged := False;
      EnableSaveRevert;
      LoadingDone := True;
      JvProgressDialog1.Hide;
      SetCaption;
    end
  else
    Close;
end;

procedure TFormMain.gpbxModeClick(Sender: TObject);
begin
  StatusClear;
end;

procedure TFormMain.GroupBox1Click(Sender: TObject);
begin
  StatusClear;
end;

procedure TFormMain.GroupBox2Click(Sender: TObject);
begin
  StatusClear;
end;

function TFormMain.OKToCloseProject: boolean;
var
  MsgText: string;
  ModRes: integer;
begin
  if ProjChanged and PreventDataLoss then
    begin
      MsgText := 'Project contains unsaved changes.  Save changes to ' + PCurrent.FileName + '?';
      ModRes := MessageDlg(MsgText,mtConfirmation,[mbYes,mbNo,mbCancel],0,mbYes);
      case ModRes of
        mrYes:   // Save project data
          begin
            actFileSaveExecute(nil);
            result := True;
          end;
        mrNo:   // Exit anyway
          begin
            PreventDataLoss := False;
            result := True;
          end;
        mrCancel: result := False; // Cancel
        else
          result := False;
      end;
    end
  else
    begin
      result := True;
    end;
end;

function TFormMain.OpenAFile(FileName: string): boolean;
var
  FileStream: TFileStream;     // Text file defining one or more objects
  MemStream: TMemoryStream;    // Temporarily hold an object
  TextStream: TMemoryStream;   // Text form of a stream
  TempProject: TProject;
  Status: string;
  FileBase: string;
begin
  result := False;
  TempProject := TProject.Create(Self);
  MemStream := TMemoryStream.Create;
  TextStream := TMemoryStream.Create;
  Status := 'Problem encountered while opening the file: ' + FileName;
  try
    FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    FileStream.Position := 0;
    TextStream.CopyFrom(FileStream, FileStream.Size);
    JvProgressDialog1.Position := 15;
    TextStream.Position := 0;
    ObjectTextToBinary(TextStream, MemStream);
    JvProgressDialog1.Position := 20;
    MemStream.Position := 0;
    try
      Status := 'Problem encountered while opening the file "' + FileName +
                '".  File structure may be obsolete.';
      MemStream.ReadComponent(TempProject);
      JvProgressDialog1.Position := 25;
      FileBase := FileBaseName(FileName);
      PCurrent.Assign(TempProject);
      JvProgressDialog1.Position := 30;
      PCurrent.FileName := FileName;
      if PCurrent.NameLikeProjectFile then
        PCurrent.ProjName := ConvertString255(ChangeFileExt(ExtractFileName(PCurrent.FileName),''));
      
      StatusBar1.Panels[0].Text := ' Project Name: ' + ConvertString(PCurrent.ProjName);
      Status := 'Project read from file: ' + FileName;
      SetCaption;
      result := True;
    except
      on EReadError do ShowMessage(Status);
      on EFOpenError do ShowMessage(Status);
      on Exception do ShowMessage(Status);
    end;
  finally
    FreeAndNil(FileStream);
    FreeAndNil(TempProject);
    FreeAndNil(MemStream);
    FreeAndNil(TextStream);
    StatusBar1.Panels[1].Text := Status;
  end;
end;

procedure TFormMain.miDerivedParamsClick(Sender: TObject);
var
  ModRes: integer;
begin
//  AssignParameters;
  ModRes := FormDerivedParameters.ShowModal;
  if (ModRes = mrOK) and (ProjChanged) then
    begin
      PCurrent.ParamSet.Assign(ParamSetCurrent);
    end;
  ParamSetCurrent.Assign(PCurrent.ParamSet);
  PopulateControls(PCurrent);
  EnableSaveRevert;
end;

procedure TFormMain.miParamEstSettingsClick(Sender: TObject);
begin
  frmParEstSettings.ShowModal;
  EnableSaveRevert;
end;

procedure TFormMain.miPriorInfoClick(Sender: TObject);
var
  ModRes: integer;
begin
  FormPriorInfoControl.PriSetControl.Assign(PriSetCurrent);
  FormPriorInfoControl.PriGpsControl.Assign(PriGpsCurrent);
  FormPriorInfoControl.PriorSetupControl.Assign(PriorSetupCurrent);
  FormPriorInfoControl.UsePriorInfoControl:= PCurrent.UsePriorInfo;
  ModRes := FormPriorInfoControl.ShowModal;
  if (ModRes = mrOK) and (FormPriorInfoControl.PriorDataControlChanged) then
    begin
      PriSetCurrent.Assign(FormPriorInfoControl.PriSetControl);
      PriGpsCurrent.Assign(FormPriorInfoControl.PriGpsControl);
      PriorSetupCurrent.Assign(FormPriorInfoControl.PriorSetupControl);
      PCurrent.UsePriorInfo := FormPriorInfoControl.UsePriorInfoControl;
      ProjChanged := True;
      EnableSaveRevert;
    end;
end;

procedure TFormMain.ParametersAndObservationsFromMODFLOW2005Click(Sender: TObject);
begin
  UseCurrentNameFile := False;
  actImportModflow2005ParametersExecute(self);
  UseCurrentNameFile := True;
  actImportModflow2005ObservationsExecute(self);
end;

procedure TFormMain.pcAppChanging(Sender: TObject; var AllowChange: Boolean);
// Prevent user from changing tab selection if a tab-specific window is open
begin
  AllowChange := True;
  if UCParEstSetWinOpen then
    begin
      ShowMessage('The Parameter-Estimation Settings window must be closed before selecting another application.');
      AllowChange := False;
    end;
  if UcodeSetWinOpen then
    begin
      ShowMessage('The UCODE Settings window must be closed before selecting another application.');
      AllowChange := False;
    end;
end;


procedure TFormMain.popmenuUcodePopup(Sender: TObject);
begin
  if AssignParameters then
    UpdateCurrentProject;
end;

procedure TFormMain.PopulateControls(Prj: TProject);
begin
  // Populate controls on Form1 with appropriate values from project Prj.
  KeepCurrent := False;
  with FormMain do
  begin
    // check one radio button in Mode group box.
    case Prj.UcProject.UcMode of
      umFwd: rbForward.Checked := True;
      umSensAnal: rbSenAnal.Checked := True;
      umParEst: begin
           rbParEst.Checked := True;
         end;
      umTestLin: rbTestLin.Checked := True;
      umPred: rbPred.Checked := True;
      umAdvTestLin: begin
           rbAdvTestLin.Checked := True;
          end;
      umNonLinUncert: rbNonLinUnc.Checked := True;
      umInvObjFunc: begin
           rbInvObjFn.Checked := True;
         end;
    end;
    rbwdgParamGpsPopulate(Prj.ParGpSet);
    rbwdgParamsPopulate(Prj.ParamSet);
    PopulateObsTabSheet(Prj);
    PopulatePredTabSheet(Prj);
    StatusBar1.Panels[0].Text := ' Project Name: ' + ConvertString(Prj.ProjName);
    EnableSaveRevert;
  end;
  KeepCurrent := True;
end;

procedure TFormMain.PopulateObsTabSheet(const Project: TProject);
var
  AttPos, I, J, K, NObs, NObsGps: integer;
  GName: string;
begin
  //
  if Assigned(Project) then
    begin
      AttPos := DepAttPos(datGroupName);
      NObs := Project.ObsSet.Count;
      NObsGps := Project.ObsGpSet.Count;
      jvmNumObs.Lines.Clear;
      jvmNumObs.Lines.Add(IntToStr(NObs));
      jvmNumObs.CurrentLine := 0;
      jvmNumObsGps.Lines.Clear;
      jvmNumObsGps.Lines.Add(IntToStr(NObsGps));
      jvmNumObsGps.CurrentLine := 0;
      dgObsGpSummary.RowCount := 1 + NObsGps;
      if NObsGps > 0 then
        begin
          // Loop through observation groups
          for I := 1 to NObsGps do
            begin
              GName := ConvertString(Project.ObsGpSet.Items[I-1].Name);
              dgObsGpSummary.Cells[0,I] := GName;
              // Count number of observations in this group
              K := 0;
              for J := 0 to NObs - 1 do
                begin
                  if AnsiSameText(Project.ObsSet.Items[J].AllAtts.Items[AttPos].Text, GName) then
                    begin
                      K := K + 1;
                    end;
                end;
              dgObsGpSummary.Cells[1,I] := IntToStr(K);
            end;
        end;
    end;
end;

procedure TFormMain.PopulatePredTabSheet(const Project: TProject);
var
  AttPos, I, J, K, NPred, NPredGps: integer;
  GName: string;
begin
  //
  if Assigned(Project) then
    begin
      AttPos := DepAttPos(datGroupName);
      NPred := Project.PredSet.Count;
      NPredGps := Project.PredGpSet.Count;
      jvmNumPred.Lines.Clear;
      jvmNumPred.Lines.Add(IntToStr(NPred));
      jvmNumPred.CurrentLine := 0;
      jvmNumPredGps.Lines.Clear;
      jvmNumPredGps.Lines.Add(IntToStr(NPredGps));
      jvmNumPredGps.CurrentLine := 0;
      dgPredGpSummary.RowCount := 1 + NPredGps;
      if NPredGps > 0 then
        begin
          // Loop through prediction groups
          for I := 1 to NPredGps do
            begin
              GName := ConvertString(Project.PredGpSet.Items[I-1].Name);
              dgPredGpSummary.Cells[0,I] := GName;
              // Count number of predictions in this group
              K := 0;
              for J := 0 to NPred - 1 do
                begin
                  if AnsiSameText(Project.PredSet.Items[J].AllAtts.Items[AttPos].Text, GName) then
                    begin
                      K := K + 1;
                    end;
                end;
              dgPredGpSummary.Cells[1,I] := IntToStr(K);
            end;
        end;
    end;
end;

procedure TFormMain.ProgramLocations1Click(Sender: TObject);
begin
  OpenProgramLocations;
end;

procedure TFormMain.btnMoreUcodeClick(Sender: TObject);
begin
  if AssignParameters then
    begin
      UpdateCurrentProject;
      if UcodeSetWinOpen then
        begin
          frmUcodeSettings.SetFocus;
        end
      else
        begin
          frmUcodeSettings := TfrmUcodeSettings.Create(Self);
          frmUcodeSettings.Show;
          if ProjChanged then
            begin
              PopulateControls(PCurrent);
            end;
        end;
    end;
end;

procedure TFormMain.btnObsFormClick(Sender: TObject);
begin
  ShowObsForm;
  PopulateObsTabSheet(PCurrent);
  If ProjChanged then EnableSaveRevert;
  Invalidate;
end;

procedure TFormMain.btnPriorFormClick(Sender: TObject);
begin
  ShowPriForm;
end;

procedure TFormMain.btnAddGroupClick(Sender: TObject);
var
  NGpsOld: integer;
begin
  NGpsOld := ParamGpsCurrent.Count;
  frmAddGroup := TfrmAddGroup.Create(Self);
  frmAddGroup.GpUse := guParGroup;
  frmAddGroup.ShowModal;
  FreeAndNil(frmAddGroup);
  if NGpsOld <> ParamGpsCurrent.Count then
    begin
      rbwdgParamGpsPopulate(ParamGpsCurrent);
      rbwdgParamsPopulate(ParamSetCurrent);
      UpdateCurrentProject;
      ProjChanged := True;
      AssignCurrent;
    end;
end;

procedure TFormMain.btnAddParameterClick(Sender: TObject);
var
  NParOld: integer;
begin
  NParOld := ParamSetCurrent.Count;
  frmAddParOrDep := TfrmAddParOrDep.Create(Self);
  frmAddParOrDep.PDUse := pduParameter;
  frmAddParOrDep.ShowModal;
  FreeAndNil(frmAddParOrDep);
  if NParOld <> ParamSetCurrent.Count then
    begin
      rbwdgParamsPopulate(ParamSetCurrent);
      UpdateCurrentProject;
      ProjChanged := True;
      AssignCurrent;
      // If it's a MODFLOW-2005 project, set flag to recreate PVAL template file.
      if PCurrent.ModelID = midModflow2005 then
        begin
          if PCurrent.LinkTemplateToParamsTable then NeedPvalTemplate := True;
        end;
    end;
  Invalidate;
end;

procedure TFormMain.btnExportUcodeClick(Sender: TObject);
var
  ExportErrMsg, Line: string;
  ExportCode: integer;
  OrigCursor: TCursor;
begin
  OrigCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourglass;
    if AssignParameters then
      begin
        UpdateCurrentProject;
        if ProjChanged then actFileSaveExecute(self);
        ExportCode := ExportUcode(ExportErrMsg);
        if ExportCode = 0 then
          begin
            Line := 'UCODE file(s) successfully exported';
          end
        else
          if ExportCode > 0 then
            begin
              Line := 'Error(s) encountered in exporting UCODE file(s): '
                + ExportErrMsg + ' After correcting error(s) please click "Create UCODE Input Files" again.';
            end
          else
            Line := 'Export cancelled';
      end
    else
      begin
        Line := '';
      end;
  finally
    Screen.Cursor := OrigCursor;
  end;
  if Line <> '' then ShowMessage(Line);
  EnableSaveRevert;
end;

procedure TFormMain.btnHelpMethodsClick(Sender: TObject);
begin
  frmHelpUcodeMethods := TfrmHelpUcodeMethods.Create(Self);
  frmHelpUcodeMethods.Show;
end;

procedure TFormMain.btnParallelClick(Sender: TObject);
begin
// Open Parallel Processing form (modal) when clicked.
  actParallelControl.Execute;
end;

procedure TFormMain.btnParEstWindowClick(Sender: TObject);
begin
  if UCParEstSetWinOpen then
    begin
      frmParEstSettings.SetFocus;
    end
  else
    begin
      frmParEstSettings := TfrmParEstSettings.Create(Self);
      frmParEstSettings.Show;
    end;
end;

procedure TFormMain.btnPredFormClick(Sender: TObject);
begin
  ShowPredForm;
  PopulatePredTabSheet(PCurrent);
end;

procedure TFormMain.btnRenameGpClick(Sender: TObject);
var
  I, ModRes, SelRowFirst, SelRowLast: integer;
  Messg, NewGpName, OldGpName: string;
begin
  ClearAllSelections(rbwdgParams);
  rbwdgParams.EditorMode := False;
  SelRowFirst := rbwdgParamGps.Selection.Top;
  SelRowLast := rbwdgParamGps.Selection.Bottom;
  if (SelRowFirst = SelRowLast) and (SelRowFirst > 0) then
    begin
      I := SelRowFirst;
      OldGpName := rbwdgParamGps.Cells[0,I];
      ParamGpCellCurrent.Row := I;
      ParamGpCellCurrent.Column := 0;
      ParamGpCellCurrent.TextOld := OldGpName;
      ParamGpCellCurrent.TextNew := OldGpName;
      FormRenameGroup.CellData := ParamGpCellCurrent;
      FormRenameGroup.GpUse := guParGroup;
      FormRenameGroup.edtNewGroup.MaxLength := MaxLenGpName;
      ModRes := FormRenameGroup.ShowModal;
      if ModRes = mrOk then
        begin
          NewGpName := FormRenameGroup.CellData.TextNew;
          rbwdgParamGps.Cells[ParamGpCellCurrent.Column,ParamGpCellCurrent.Row]
                := NewGpName;
          ParamSetCurrent.ChangeGroupNames(OldGpName,NewGpName);
          rbwdgParamsPopulate(ParamSetCurrent);
          ClearAllSelections(rbwdgParamGps);
          AssignParameterGroups;
          UpdateCurrentProject;
          ProjChanged := True;
          AssignCurrent;
          ParamGpsChanged := True;
          EnableSaveRevert;
        end
      else
        begin
          rbwdgParamGps.Cells[ParamGpCellCurrent.Column,ParamGpCellCurrent.Row]
                := ParamGpCellCurrent.TextOld;
        end;
      ParamGpCellChanged := False;
    end
  else if SelRowFirst <= 0 then
    begin
      Messg := 'Select a parameter group to be renamed';
      ShowMessage(Messg);
    end;
end;

procedure TFormMain.btnRunUcodeClick(Sender: TObject);
var
  ExportErrMsg, ErrMsg, Line: string;
  ExportCode, ModRes, NFiles, NDir: integer;
  OK: boolean;
begin
  ModRes := MessageDlg('Start a UCODE run?',mtConfirmation,[mbYes,mbCancel],0,mbCancel);
  if ModRes = mrYes then
    begin
      if AssignParameters then
        begin
          UpdateCurrentProject;
          if ProjChanged then actFileSaveExecute(self);
          // TODO 2 : Support option to run Ucode without exporting a Ucode input file
          ExportCode := ExportUcode(ExportErrMsg);
          if ExportCode = 0 then
            begin
              OK := True;
              if PCurrent.UcProject.ParallelControl.AutoPopRunnerDirs then
                begin
                  OK := PCurrent.UcProject.PopulateRunnerDirectories(ErrMsg, NFiles, NDir);
                end;
              if OK then
                begin
                  if (PCurrent.UcProject.ModeIsParallelizable) and
                     (PCurrent.UcProject.ParallelControl.Parallel) and
                     (PCurrent.UcProject.ParallelControl.AutoStartLocalRunners) then
                    begin
                      // Start local runners
                      NumRunnersStarted := PCurrent.UcProject.StartLocalRunners;
                      if NumRunnersStarted = 0 then
                        begin
                          Line := 'Failed to start local runners';
                          ShowMessage(Line);
                        end
                      else
                        begin
                          RunUcode;
                        end;
                    end
                  else
                    begin
                      RunUcode;
                    end;
                end
              else
                begin
                  Line := 'Error encountered in populating runner directories: ' + ErrMsg;
                  ShowMessage(Line);
                end;
            end
          else
            if ExportCode > 0 then
              begin
                Line := 'Error encountered in exporting UCODE file(s): '
                      + ExportErrMsg + ' After correcting error(s), please click "Run UCODE" again.';
                ShowMessage(Line);
              end;
        end;
    end;
end;

procedure TFormMain.btnDeleteSelectedParametersClick(Sender: TObject);
var
  DelRow, I, J, SelRowFirst, SelRowLast: integer;
  TempParamSet: TParamSet;
  Messg: string;
begin
  SelRowFirst := rbwdgParams.Selection.Top;
  SelRowLast := rbwdgParams.Selection.Bottom;
  if SelRowFirst > 0 then
    begin
      DelRow := 0;
      for I := rbwdgParams.RowCount - 1 downto 1 do
        begin
          if (RowContainsSelectedCell(rbwdgParams,I))
                or ((I >= SelRowFirst) and (I <= SelRowLast)) then
            begin
              if rbwdgParams.RowCount > 2 then
                begin
                  rbwdgParams.DeleteRow(I);
                end
              else
                begin
                  rbwdgParams.Cells[1,1] := '';
                  for J := 2 to rbwdgParams.ColCount-1 do
                    begin
                      rbwdgParams.Cells[J,1] := '';
                    end;
                  TempParamSet := TParamSet.Create;
                  rbwdgParamsPopulate(TempParamSet);
                  TempParamSet.Free;
                end;
              DelRow := I;
              // Delete corresponding item in ParamSetCurrent.
              if ParamSetCurrent.Count > 0 then ParamSetCurrent.Delete(I-1);
              NeedPvalTemplate := True;
            end;
        end;
      if DelRow > 0 then
        begin
          if (DelRow < rbwdgParams.RowCount - 1) then
            begin
              rbwdgParams.Row := DelRow;
            end;
          ClearAllSelections(rbwdgParams);
          AssignParameters;
          UpdateCurrentProject;
          ProjChanged := True;
          AssignCurrent;
        end;
    end
  else
    begin
      Messg := 'Select parameter(s) to be deleted';
      ShowMessage(Messg);
    end;
end;

procedure TFormMain.btnDeleteSelParGpsClick(Sender: TObject);
// Delete selected parameter groups, but only if group is unused
var
  DelRow, I, K, SelRowFirst, SelRowLast: integer;
  GpName: string;
  Messg: string;
begin
  ClearAllSelections(rbwdgParams);
  rbwdgParams.EditorMode := False;
  if (rbwdgParamGps.RowCount > 2) and
      (CountSelectedRows(rbwdgParamGps) < rbwdgParamGps.RowCount-1) then
    begin
      SelRowFirst := rbwdgParamGps.Selection.Top;
      SelRowLast := rbwdgParamGps.Selection.Bottom;
      if SelRowFirst > 0 then
        begin
          DelRow := 0;
          for I := rbwdgParamGps.RowCount - 1 downto 1 do
            begin
              if (RowContainsSelectedCell(rbwdgParamGps,I))
                    or ((I >= SelRowFirst) and (I <= SelRowLast)) then
                begin
                  GpName := rbwdgParamGps.Cells[0,I];
                  K := ParamSetCurrent.NumParsByGroup(GpName);
                  if K = 0 then
                    begin
                      if rbwdgParamGps.RowCount > 2 then
                        begin
                          rbwdgParamGps.DeleteRow(I);
                        end;
                      DelRow := I;
                      // Delete corresponding item in ParamGpsCurrent.
                      if ParamGpsCurrent.Count > 0 then ParamGpsCurrent.Delete(I-1);
                    end
                  else
                    begin
                      Messg := 'Group in use may not be deleted: ' + GpName;
                      ShowMessage(Messg);
                    end;
                end;
            end;
          if DelRow > 0 then
            begin
              ClearAllSelections(rbwdgParamGps);
              AssignParameterGroups;
              UpdateCurrentProject;
              ProjChanged := True;
              AssignCurrent;
            end;
        end
      else
        begin
          Messg := 'Select parameter group(s) to be deleted';
          ShowMessage(Messg);
        end;
    end
  else
    begin
      Messg := 'At least one group must be defined.  Group(s) not deleted';
      ShowMessage(Messg);
    end;
end;

procedure TFormMain.btnDerivedParamsClick(Sender: TObject);
var
  ModRes: integer;
begin
  if AssignParameters then
    begin
      UpdateCurrentProject;
      ModRes := FormDerivedParameters.ShowModal;
      if (ModRes = mrOK) and (ProjChanged) then
        begin
          PCurrent.ParamSet.Assign(ParamSetCurrent);
          PopulateControls(PCurrent);
        end;
    end;
end;

procedure TFormMain.ClearAllData;
begin
  FormObservations.NeedObsPopulate := true;
  PCurrent.Assign(PDefault);
  ParameterSetupCurrent.Assign(ParameterSetupDefault);
  ParamSetCurrent.Clear;
  ParamGpsCurrent.Clear;
  ParallelControlGlobal.Free;
  ParallelControlGlobal := TParallelControl.Create;
  ParallelRunnersGlobal.Clear;
  RunnerFilesGlobal.Clear;
  ParamSetLocal.Clear;
  //
  PopulateControls(PCurrent);
end;

procedure TFormMain.cmbAppChooserChange(Sender: TObject);
begin
  plApp.ActivePage := plApp.Pages[cmbAppChooser.ItemIndex];
end;

procedure TFormMain.CheckParamGpNames;
var
  IRowCurr: Integer;
  OldGroup: string;
  K: Integer;
  Messg: string;
  NewGroup: string;
begin
  for IRowCurr := 1 to rbwdgParamGps.RowCount - 1 do
  begin
    OldGroup := ConvertString(ParamGpsCurrent.Items[IRowCurr - 1].Name);
    NewGroup := rbwdgParamGps.Cells[0,IRowCurr];
    if NewGroup <> OldGroup then
      begin
        // If group name has changed, ensure that old group name is unused.
        K := ParamSetCurrent.NumParsByGroup(OldGroup);
        if K > 0 then
        // Old group name is in use.
          begin
            Messg := 'Group name in use may not be changed: ' + OldGroup;
            ShowMessage(Messg);
            rbwdgParamGps.Cells[0, IRowCurr] := OldGroup;
          end
        else
        // Old group name not in use and may be changed
          begin
            // Check validity of new parameter group name
            NewGroup := rbwdgParamGps.Cells[0, IRowCurr];
            if J_Valid_Name(NewGroup,MaxLenGpName) then
              begin
                UpdateCurrentProject;
              end
            else
              begin
                if NewGroup <> '' then
                  begin
                    Messg := 'Invalid name: ' + NewGroup;
                    ShowMessage(Messg);
                  end;
                rbwdgParamGps.Cells[0, IRowCurr] := OldGroup;
              end;
          end;
      end;
  end;
  AssignCurrent;
end;

procedure TFormMain.CreateInstructionFilesForModelMuseDeps(const ModelUse: TModelUse);
// Create instruction file(s) for observations or predictions contained in PCurrent.
// Correct functioning assumes that model is MODFLOW-2005 and only standard group names
// (Heads, Head_Changes, DRN_flows, RIV_flows, GHB_flows, CHOB_flows) are used.
//
// Within each group, it is further assumed that simulated values are listed
// in the MODFLOW-2005 output file in the order in which they are listed in
// PCurrent.PredSet.  Also, the unit numbers in the name file for the package
// output files must be those assigned by ModelMuse.
var
  ObsPkgs: array[0..5] of string;
  GpName: array[1..5] of string;
  Iuobsv: array[0..5] of Integer;
  NumDeps: array[0..5] of Integer;
  NTotalDeps: Integer;
  I: Integer;
  Msg: string;
begin
  // Use order used internally by Modflow-2005
  ObsPkgs[0] := 'HOB';
  ObsPkgs[1] := 'DROB';
  ObsPkgs[2] := 'RVOB';
  ObsPkgs[3] := 'GBOB';
  ObsPkgs[4] := 'CHOB';
  ObsPkgs[5] := 'STOB';
  GpName[1] := 'DRN_flows';
  GpName[2] := 'RIV_flows';
  GpName[3] := 'GHB_flows';
  GpName[4] := 'CHOB_flows';
  GpName[5] := 'STR_flows';
  // Following are unit numbers assigned by ModelMuse
  Iuobsv[0] := 42; // IUHOBSV.
  Iuobsv[1] := 45; // IUDROBSV.
  Iuobsv[2] := 49; // IURVOBSV.
  Iuobsv[3] := 47; // IUGBOBSV.
  Iuobsv[4] := 51; // IUCHOBSV.
  Iuobsv[5] := 145; // IUSTOBSV.
  // Store number of predictions of each type (group name)
  case ModelUse of
    muCalib: NumDeps[0] := PCurrent.ObsSet.NumDepByGroup('Heads') + PCurrent.ObsSet.NumDepByGroup('Head_Changes');
    muPred: NumDeps[0] := PCurrent.PredSet.NumDepByGroup('Heads') + PCurrent.PredSet.NumDepByGroup('Head_Changes');
  end;
  NTotalDeps := NumDeps[0];
  for I := 1 to 5 do
  begin
    case ModelUse of
      muCalib: NumDeps[I] := PCurrent.ObsSet.NumDepByGroup(GpName[I]);
      muPred: NumDeps[I] := PCurrent.PredSet.NumDepByGroup(GpName[I]);
    end;
    NTotalDeps := NTotalDeps + NumDeps[I];
  end;
  if NTotalDeps > 0 then
  begin
    if BuildMF2005InstructFiles(PCurrent, Iuobsv, NumDeps, ModelUse) then
    begin
      Msg := 'Instruction file(s) successfully created';
      ShowMessage(Msg);
      ProjChanged := True;
      EnableSaveRevert;
    end
    else
    begin
      Msg := 'Error encountered.  Unable to create instruction file(s)';
      ShowMessage(Msg);
    end;
  end
  else
  begin
    case ModelUse of
      muCalib: Msg := 'Project contains no observations in supported groups.  No instruction file(s) created.';
      muPred: Msg := 'Project contains no predictions in supported groups.  No instruction file(s) created.';
    end;
    ShowMessage(Msg);
  end;
end;

procedure TFormMain.ColorRadioButtons(ActiveButton: TRadioButton);
begin
  rbForward.Color := clBtnFace;
  rbSenAnal.Color := clBtnFace;
  rbParEst.Color := clBtnFace;
  rbTestLin.Color := clBtnFace;
  rbPred.Color := clBtnFace;
  rbAdvTestLin.Color := clBtnFace;
  rbNonLinUnc.Color := clBtnFace;
  rbInvObjFn.Color := clBtnFace;
  ActiveButton.Color := clInfoBk;
end;

procedure TFormMain.Commands1Click(Sender: TObject);
var
  MCFold, MCFDold, MCPold, MCPDold: string;
begin
  MCFold := PCurrent.MCLForward;
  MCFDold := PCurrent.MCLFwdDeriv;
  MCPold := PCurrent.MCLPred;
  MCPDold := PCurrent.MCLPredDeriv;
  FormModelCommands.NeedProgLoc := False;
  FormModelCommands.ShowModal;
  if FormModelCommands.NeedProgLoc then
    begin
      OpenProgramLocations;
    end
  else
    begin
      if (PCurrent.MCLForward <> MCFold) or (PCurrent.MCLFwdDeriv <> MCFDold)
        or (PCurrent.MCLPred <> MCPold) or (PCurrent.MCLPredDeriv <> MCPDold) then
        begin
          ProjChanged := True;
          AssignCurrent;
        end;
    end;
end;

procedure TFormMain.CountParLines(const RbwGrid: TRbwDataGrid4;
    var NLines: integer; var NNames: integer;
    var FirstBlankRow: integer);
var
  I, J, NCol, NGridRows: integer;
begin
  NGridRows := RbwGrid.RowCount - 1;
  NCol := RbwGrid.ColCount;
  NLines := 0;
  NNames := 0;
  FirstBlankRow := 0;
  for I := 1 to NGridRows do
    begin
      if IsNonBlank(RbwGrid.Cells[0,I]) then
        begin
          NNames := NNames + 1;
        end
      else
        begin
          if FirstBlankRow = 0 then
            FirstBlankRow := I;
        end;
      for J := 0 to NCol - 1 do
        begin
          if RbwGrid.Cells[J,I] <> '' then
            begin
              NLines := NLines + 1;
              // exit inner loop
              break;
            end;
        end;
    end;
end;

procedure TFormMain.actCreateInstructionFilesForModelMuseObsExecute(Sender: TObject);
begin
  CreateInstructionFilesForModelMuseDeps(muCalib);
end;

procedure TFormMain.actCreateInstructionFilesForModelMusePredsExecute(
  Sender: TObject);
begin
  CreateInstructionFilesForModelMuseDeps(muPred);
end;

procedure TFormMain.rbwdgParamGpsColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  Invalidate;
  Repaint;
end;

procedure TFormMain.rbwdgParamGpsEndUpdate(Sender: TObject);
begin
  if ParamGpCellChanged then
    begin
      if ParamGpCellCurrent.Column = 0 then
        begin
          rbwdgParamGps.Cells[ParamGpCellCurrent.Column,ParamGpCellCurrent.Row]
                    := ParamGpCellCurrent.TextOld;
          ParamGpCellChanged := False;
        end
      else
        begin
          ProjChanged := True;
          EnableSaveRevert;
        end;
    end;
  StatusBar1.Panels[1].Text := '';
end;

procedure TFormMain.rbwdgParamGpsEnter(Sender: TObject);
begin
  rbwdgParamGpsPopulate(ParamGpsCurrent);
end;

procedure TFormMain.rbwdgParamGpsExit(Sender: TObject);
var
  FirstBlankParGpRow, NParGpLines, NParGpNames: integer;
  OK: boolean;
begin
  // Check parameter group table entries
  // Count lines not filled with blanks in parameter groups table
  // If NParGpNames < NParGpLines, there's at least one blank group name
  CountParLines(rbwdgParamGps,NParGpLines,NParGpNames,FirstBlankParGpRow);
  if NParGpLines = NParGpNames then
    begin
      OK := True;
    end
  else
    begin
      ShowMessage('Error in Parameter Groups Table: At least one group is unnamed');
      rbwdgParamGps.SetFocus;
      rbwdgParamGps.Column := 0;
      rbwdgParamGps.Row := FirstBlankParGpRow;
      OK := False;
    end;
  if OK then
    begin
    CheckParamGpNames;
    AssignParameterGroups;
    end;
  StatusBar1.Panels[1].Text := '';
end;

procedure TFormMain.rbwdgParamGpsGetEditText(Sender: TObject; ACol,
  ARow: Integer; var Value: string);
begin
  ParamGpCellText := Value;
  ParamGpCellChanged := False;
  if ACol = 0 then
    begin
      ParamGpSelected := Value;
    end;
  StatusClear;
end;

procedure TFormMain.rbwdgParamGpsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ssCtrl in Shift then
    begin
      SetColorSelectedRow(rbwdgParamGps,False);
    end
  else
    begin
      SetColorSelectedRow(rbwdgParamGps,True);
    end;
  if ssShift in Shift then
    begin
      AllowEditing(rbwdgParamGps,False);
      SetColorSelectedRow(rbwdgParamGps,False);
    end
  else
    begin
      AllowEditing(rbwdgParamGps,True);
    end;
  StatusClear;
end;

procedure TFormMain.rbwdgParamGpsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  Hint: string;
  LocalRect: TGridRect;
begin
  if (ACol = rbwdgParamGps.Column) and (ACol >= 0) and (ARow >= 0) then
    begin
      Hint := GetParHint(rbwdgParamGps, ACol);
      StatusBar1.Panels[1].Text := ' ' + Hint;
      StatusBar1.Panels[2].Width := 0;
      if ACol = 0 then
        begin
          LocalRect.Left := 0;
          LocalRect.Right := rbwdgParamGps.ColCount - 1;
          LocalRect.Top := ARow;
          LocalRect.Bottom := ARow;
          rbwdgParamGps.Selection := LocalRect;
        end;
    end;
end;

procedure TFormMain.rbwdgParamGpsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  if Value <> ParamGpCellText then
    begin
      ParamGpCellCurrent.Row := ARow;
      ParamGpCellCurrent.Column := ACol;
      ParamGpCellCurrent.Checked := rbwdgParamGps.Checked[ACol,ARow];
      ParamGpCellCurrent.TextOld := ParamGpCellText;
      ParamGpCellCurrent.TextNew := Value;
      ParamGpCellChanged := True;
      ParamGpsChanged := True;
    end;
  StatusClear;
end;

procedure TFormMain.rbwdgParamGpsStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
begin
  if not IgnoreGpsStateChange then
    begin
      AssignParameterGroups;
      ParamGpsChanged := True;
      ProjChanged := True;
      EnableSaveRevert;
      PCurrent.ParGpSet.Assign(ParamGpsCurrent);
    end;
end;

procedure TFormMain.rbwdgParamsAfterDelete(Sender: TObject);
begin
  // Revise parameter set to include only visible parameters.
  AssignParameters;
  UpdateCurrentProject;
  ProjChanged := True;
  AssignCurrent;
end;

procedure TFormMain.rbwdgParamsPopulate(psSource: TParamSet);
// Populate data grid dgParams
var
  I, IA, IP1, J, NRows, NumCols: integer;
  BlankString, TempString: string;
  setATDisplayPar: set of TParamAttType;
  PatTemp: TParamAttType;
  GpList: TStringList;
  ParValTemp: double;
  ErrMsg, ParValText: string;
  TempBool: boolean;
begin
  ParamSetLocal.Assign(psSource);
  GpList := TStringList.Create;
  try
  //AssignParameterGroups;
  for I := 0 to ParamGpsCurrent.Count - 1 do
    GpList.Add(ConvertString(ParamGpsCurrent.Items[I].Name));
  BlankString := StringOfChar(' ', 256); // Ensures that all characters are erased
  BlankString := '';
  case PCurrent.ActiveApp of
  aaUcode:
    begin
      setATDisplayPar := [patGroupName, patDerived, patStartValue,
                      patAdjustable, patConstrain,
                      patLowerBound, patUpperBound,
                      patLowerValue, patUpperValue,
                      patMaxChange, patPerturbAmt, patScalePval, patSenMethod,
                      patTolPar, patTransform];
    end;
  aaPest:
    begin
      setATDisplayPar := [patGroupName, patDerived, patStartValue, patAdjustable,
                      patLowerBound, patUpperBound, patTransform];
    end;
  aaApp3:
    begin
      setATDisplayPar := [patGroupName, patStartValue, patAdjustable,
                      patLowerBound, patUpperBound, patTransform];
    end;
  aaApp4:
    begin
      setATDisplayPar := [patGroupName, patStartValue, patAdjustable,
                      patLowerBound, patUpperBound, patTransform];
    end;
  end;
  // Count number of columns needed.
  NumCols := 1;
  for I := 0 to NumParAttributes - 1 do
    if (ParameterSetupCurrent.ParAttributes[I].ParamAttType in setATDisplayPar) and
       (ParameterSetupCurrent.ParAttributes[I].ControlMethod = cmByItem) then
            NumCols := NumCols + 1;
  rbwdgParams.ColCount := NumCols;
  if FreezeParamNames then
    begin
      rbwdgParams.FixedCols := 1;
    end
  else
    begin
      rbwdgParams.FixedCols := 0;
    end;
  // Clear table headings and cells.
  NRows := Max(psSource.Count+1, 2);
  rbwdgParams.RowCount := NRows;
  for I := 0 to rbwdgParams.ColCount - 1 do
  begin
    rbwdgParams.Columns[I].CaptionAlignment := taLeftJustify;
    rbwdgParams.Cells[I,0] := BlankString;
    rbwdgParams.Columns[I].AutoAdjustColWidths := True;
    for J := 1 to rbwdgParams.RowCount - 1 do
      rbwdgParams.Cells[I, J] := BlankString;
  end;
  // After all cells are cleared, assign ColWidths to minimal width.
  for I := 0 to rbwdgParams.ColCount - 1 do
    begin
      rbwdgParams.Columns[I].ComboUsed := False;
      rbwdgParams.ColWidths[I] := rbwdgParams.DefaultColWidth;
      rbwdgParams.Columns[I].AutoAdjustColWidths := True;
    end;
  // Populate column headings and assign properties for selected columns.
  J := 0;
  rbwdgParams.Cells[J,0] := 'Parameter Name';
  for I := 0 to NumParAttributes - 1 do
    if (ParameterSetupCurrent.ParAttributes[I].ControlMethod = cmByItem) and
       (ParameterSetupCurrent.ParAttributes[I].ParamAttType in setATDisplayPar) then
    begin
      case ParameterSetupCurrent.ParAttributes[I].ParamAttType of
        patGroupName: begin
            J := J + 1;
            rbwdgParams.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParams.Columns[J].ComboUsed := True;
            rbwdgParams.Columns[J].Format := rcf4String;
            rbwdgParams.Columns[J].PickList := GpList;
            rbwdgParams.Columns[J].LimitToList := True;
            rbwdgParams.Columns[J].CheckMin := False;
          end;
        patDerived: begin
            J := J + 1;
            rbwdgParams.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParams.Columns[J].Format := rcf4Boolean;
            rbwdgParams.Columns[J].CheckMin := False;
            rbwdgParams.Columns[J].LimitToList := False;
          end;
        patStartValue: begin
            J := J + 1;
            rbwdgParams.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParams.Columns[J].Format := rcf4Real;
            rbwdgParams.Columns[J].CheckMin := False;
            rbwdgParams.Columns[J].LimitToList := False;
          end;
        patAdjustable: begin
            J := J + 1;
            rbwdgParams.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParams.Columns[J].Format := rcf4Boolean;
            rbwdgParams.Columns[J].CheckMin := False;
            rbwdgParams.Columns[J].LimitToList := False;
          end;
        patConstrain: begin
            J := J + 1;
            rbwdgParams.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParams.Columns[J].Format := rcf4Boolean;
            rbwdgParams.Columns[J].CheckMin := False;
            rbwdgParams.Columns[J].LimitToList := False;
          end;
        patLowerBound: begin
            J := J + 1;
            rbwdgParams.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParams.Columns[J].Format := rcf4Real;
            rbwdgParams.Columns[J].CheckMin := False;
            rbwdgParams.Columns[J].LimitToList := False;
          end;
        patUpperBound: begin
            J := J + 1;
            rbwdgParams.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParams.Columns[J].Format := rcf4Real;
            rbwdgParams.Columns[J].CheckMin := False;
            rbwdgParams.Columns[J].LimitToList := False;
          end;
        patMaxChange: begin
            J := J + 1;
            rbwdgParams.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParams.Columns[J].Format := rcf4Real;
            rbwdgParams.Columns[J].CheckMin := False;
            rbwdgParams.Columns[J].LimitToList := False;
          end;
            //atNonLinearInterval:
        patPerturbAmt: begin
            J := J + 1;
            rbwdgParams.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParams.Columns[J].Format := rcf4Real;
            rbwdgParams.Columns[J].CheckMin := False;
            rbwdgParams.Columns[J].LimitToList := False;
          end;
        patLowerValue: begin
            J := J + 1;
            rbwdgParams.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParams.Columns[J].Format := rcf4Real;
            rbwdgParams.Columns[J].CheckMin := False;
            rbwdgParams.Columns[J].LimitToList := False;
          end;
        patUpperValue: begin
            J := J + 1;
            rbwdgParams.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParams.Columns[J].Format := rcf4Real;
            rbwdgParams.Columns[J].CheckMin := False;
            rbwdgParams.Columns[J].LimitToList := False;
          end;
        patScalePval: begin
            J := J + 1;
            rbwdgParams.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParams.Columns[J].Format := rcf4Real;
            rbwdgParams.Columns[J].CheckMin := True;
            rbwdgParams.Columns[J].Min := 0;
            rbwdgParams.Columns[J].LimitToList := False;
          end;
        patSenMethod: begin
            J := J + 1;
            rbwdgParams.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParams.Columns[J].Format := rcf4String;
            rbwdgParams.Columns[J].LimitToList := True;
            rbwdgParams.Columns[J].ComboUsed := True;
            rbwdgParams.Columns[J].PickList := slSenMethod;
            rbwdgParams.Columns[J].CheckMin := False;
          end;
        patSosIncrement: begin
            J := J + 1;
            rbwdgParams.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParams.Columns[J].Format := rcf4Real;
            rbwdgParams.Columns[J].CheckMin := False;
            rbwdgParams.Columns[J].LimitToList := False;
          end;
        patTolPar: begin
            J := J + 1;
            rbwdgParams.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParams.Columns[J].Format := rcf4Real;
            rbwdgParams.Columns[J].CheckMin := False;
            rbwdgParams.Columns[J].LimitToList := False;
          end;
        patTransform: begin
            J := J + 1;
            rbwdgParams.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParams.Columns[J].Format := rcf4Boolean;
            rbwdgParams.Columns[J].CheckMin := False;
            rbwdgParams.Columns[J].LimitToList := False;
          end;
      end;
    end;
  // Populate table cells
  // Loop through groups; each iteration populates a row
  for I := 0 to psSource.Count - 1 do
  begin
    IP1 := I + 1;
    rbwdgParams.Cells[0,IP1] := ConvertString(psSource.Items[I].Name);
    J := 0;
    for IA := 0 to NumParAttributes - 1 do
    begin
      if (ParameterSetupCurrent.ParAttributes[IA].ControlMethod = cmByItem) and
         (ParameterSetupCurrent.ParAttributes[IA].ParamAttType in setATDisplayPar) then
        begin
          J := J + 1;
{ TODO 2 -cParameters : For parameter values, format text in a minimal length }
          PatTemp := psSource.Items[I].AllAtts[IA].ParamAttType;
          // Parameter attribute type patStartValue does not have a default value.
          if (PatTemp = patStartValue) then
            begin
              ParValText :=  psSource.Items[I].AllAtts[IA].Text;
              if (ParValText <> '') and (ParValText <> ' ') then
                try
                  if psSource.Items[I].AllAtts[IA].Text = ' ' then
                    begin
                      TempString := '';
                    end
                  else
                    begin
                      ParValTemp := StrToFloat(psSource.Items[I].AllAtts[IA].Text);
                      TempString := FloatToStr(ParValTemp);
                    end;
                except
                  on EConvertError do
                    begin
                      ErrMsg := 'Error converting "' + psSource.Items[I].AllAtts[IA].Text
                          + '" to a floating-point number';
                      ShowMessage(ErrMsg);
                      TempString := '';
                      Continue;
                    end;
                end
              else
                begin
                  TempString := '';
                end;
            end
          else
            begin
              TempString := psSource.Items[I].AllAtts[IA].Text;
            end;
          if TempString = '' then
            begin
              TempString := psSource.Items[I].AllAtts.Items[IA].DefaultText;
            end;
          ReplaceParAttExpl(TempString,PatTemp);
          if (PatTemp = patDerived) or (PatTemp = patAdjustable)
               or (PatTemp = patTransform) or (PatTemp = patConstrain) then
            begin
              TempBool := YesOrNoToBoolean(psSource.Items[I].AllAtts[IA].Text);
              IgnoreParStateChange := True;
              rbwdgParams.Checked[J,IP1] := TempBool;
              IgnoreParStateChange := False;
            end
          else
            begin
              rbwdgParams.Cells[J,IP1] := TempString;
            end;
        end;
    end;
  end;
  rbwdgParams.Invalidate;
  ParamCellSelected := False;
  finally
    GpList.Free;
  end;
end; // procedure TForm1.dgParamsPopulate.

procedure TFormMain.ReplaceParAttExpl(var TestString: string;
  PatTemp: TParamAttType);
var
  K: Integer;
begin
  if PatTemp = patSenMethod then
    begin
      for K := 0 to length(spSenMethod) - 1 do
        begin
          if spSenMethod[K].Value = TestString then
            TestString := spSenMethod[K].Explanation;
        end;
    end;
end;

function TFormMain.ExportUcode(var ExportErrMsg: string): integer;
var
  Dir, ErrMess, Messg : string;
  BatchFileLocation, AbsMIF, OutPrefix: string;
  Errors, ReturnCode: integer;
  ModelUseLocal: TModelUse;
begin
  Errors := 0;
  ReturnCode := 0;
  ExportErrMsg := '';
  case PCurrent.UcProject.UcMode of
    umPred:
      begin
        ModelUseLocal := muPred;
        OutPrefix := PCurrent.UcProject.OutputPrefixPred;
      end
  else
    ModelUseLocal := muCalib;
    OutPrefix := PCurrent.UcProject.OutputPrefix;
  end;
  JvProgressDialog1.InitValues(0,100,1,0,'Progress','Exporting UCODE file');
  JvProgressDialog1.Smooth := True;
  JvProgressDialog1.ShowCancel := False;
  JvProgressDialog1.Position := 0;
  if PCurrent.UcProject.FindMainInputFileName = '' then
    begin
      if sdUcode.Execute then
        begin
          case PCurrent.UcProject.UcMode of
            umPred:
              begin
                PCurrent.UcProject.MainInputFileNamePred := sdUcode.FileName;
              end
          else
            begin
              PCurrent.UcProject.MainInputFileName := sdUcode.FileName;
            end;
          end;
          ProjChanged := True
        end
      else
        ReturnCode := -1;
    end;
  JvProgressDialog1.Show;
  if ProjChanged then AssignCurrent;
  JvProgressDialog1.Position := 5;
  if ReturnCode = 0 then
    begin
      // If model is a MODFLOW-2005 model and a template for the PVAL file
      // needs to be created or recreated, do it here.
      if (PCurrent.ModelID = midModflow2005) and NeedPvalTemplate then
        begin
          if BuildMF2005PvalTemplate(PCurrent, Messg, ModelUseLocal) then
            begin
              NeedPvalTemplate := False;
            end
          else
            begin
              ShowMessage(Messg);
              if PCurrent.LinkTemplateToParamsTable then NeedPvalTemplate := True;
              Errors := Errors + 1;
              ExportErrMsg := Messg;
            end;
        end;
      JvProgressDialog1.Position := 10;

      if pCurrent.ExportUcodeFile(ModelUseLocal,JvProgressDialog1) then
        begin
          JvProgressDialog1.Position := 80;
          //result := True;
        end
      else
        begin
          Errors := Errors + 1;
          JvProgressDialog1.Hide;
          ExportErrMsg := 'Export of UCODE file failed.';
        end;
      if PCurrent.UcProject.OmitVals.Count > 0 then
        begin
          // Build a fn.omit file (see p 62 in TM6-A11).
          Dir := PCurrent.AbsAppDirectory(ModelUseLocal);
          PCurrent.UcProject.ExportOmitFile(Dir);
        end;
      if ProjChanged then actFileSaveExecute(self);
      JvProgressDialog1.Position := 85;
    end;
    if FileExists(GlobalProgramLocations.Ucode2005Location) then
    begin
      AbsMIF := PCurrent.UcProject.AbsMainInputFileName(ModelUseLocal);
      if AbsMIF <> '' then
        begin
          if not PCurrent.MakeUcodeBatchFile(ModelUseLocal,BatchFileLocation,ErrMess) then
            begin
              Errors := Error + 1;
              JvProgressDialog1.Hide;
              ShowMessage(ErrMess);
              ExportErrMsg := ErrMess;
            end;
        end
      else
        begin
          Errors := Errors + 1;
          JvProgressDialog1.Hide;
          Messg := 'Error: UCODE main input file is not defined.';
          ShowMessage(Messg);
          ExportErrMsg := Messg;
        end;
    end
  else
    begin
      Errors := Errors + 1;
      JvProgressDialog1.Hide;
      Messg := 'Error: File "' + GlobalProgramLocations.Ucode2005Location + '" not found.  Please correct location of UCODE executable.';
      ShowMessage(Messg);
      OpenProgramLocations;
      ExportErrMsg := 'If location of UCODE executable has been corrected, please continue.';
    end;
  //.
  JvProgressDialog1.Hide;
  if ReturnCode = 0 then
    begin
      if Errors > 0 then
        begin
          result := Errors;
        end
      else
        begin
          result := 0;
        end;
    end
  else
    result := ReturnCode;
end;

procedure TFormMain.ReplaceParAttValue(var TestString: string; PatTemp: TParamAttType);
var
  K: Integer;
begin
  if PatTemp = patSenMethod then
    begin
      for K := 0 to length(spSenMethod) - 1 do
        begin
          if spSenMethod[K].Explanation = TestString then
            TestString := spSenMethod[K].Value;
        end;
    end;
end;

procedure TFormMain.RunUcode;
var
  BatchFileLocation, Line, AppDir, OrigDir, Q1: string;
  AbsMIF, ErrMess, FNameNew, FNameOld, OutPrefix: string;
  BatchFileLocationTemp: AnsiSTring;
  slOutput: TStringList;
  ModelUseLocal: TModelUse;
begin
  Q1 := #039; //  #039 is ASCII for single quote.
  OrigDir := GetCurrentDir;
  // Write a batch file to invoke Ucode, then use WinExec to run the batch file
  slOutput := TStringList.Create;
  case PCurrent.UcProject.UcMode of
    umPred:
      begin
        ModelUseLocal := muPred;
        OutPrefix := PCurrent.UcProject.OutputPrefixPred;
        // Check for _dm, _paopt, _pc, and _mv file.
        // IF files do not exist, copy the Calibration versions
        // of those files to the new output prefix.  But first
        // ensure that calibration versions of _dm, _paopt, _pc,
        // and _mv exist.
        FNameOld := PCurrent.UcProject.OutputPrefix + '._dm';
        FNameNew := OutPrefix + '._dm';
          begin
            if FileExists(FNameOld) then
              {$IfDef VER180} CopyFile(PAnsiChar(FNameOld),PAnsiChar(FNameNew),False); {$EndIf}
              {$IfDef VER220} CopyFile(PWideChar(FNameOld),PWideChar(FNameNew),False); {$EndIf}
          end;
        FNameOld := PCurrent.UcProject.OutputPrefix + '._paopt';
        FNameNew := OutPrefix + '._paopt';
          begin
            if FileExists(FNameOld) then
              {$IfDef VER180} CopyFile(PAnsiChar(FNameOld),PAnsiChar(FNameNew),False); {$EndIf}
              {$IfDef VER220} CopyFile(PWideChar(FNameOld),PWideChar(FNameNew),False); {$EndIf}
          end;
        FNameOld := PCurrent.UcProject.OutputPrefix + '._pc';
        FNameNew := OutPrefix + '._pc';
          begin
            if FileExists(FNameOld) then
              {$IfDef VER180} CopyFile(PAnsiChar(FNameOld),PAnsiChar(FNameNew),False); {$EndIf}
              {$IfDef VER220} CopyFile(PWideChar(FNameOld),PWideChar(FNameNew),False); {$EndIf}
          end;
        FNameOld := PCurrent.UcProject.OutputPrefix + '._mv';
        FNameNew := OutPrefix + '._mv';
          begin
            if FileExists(FNameOld) then
              {$IfDef VER180} CopyFile(PAnsiChar(FNameOld),PAnsiChar(FNameNew),False); {$EndIf}
              {$IfDef VER220} CopyFile(PWideChar(FNameOld),PWideChar(FNameNew),False); {$EndIf}
          end;
      end
  else
    begin
      ModelUseLocal := muCalib;
      OutPrefix := PCurrent.UcProject.OutputPrefix;
    end;
  end;
  try
    if FileExists(GlobalProgramLocations.Ucode2005Location) then
      begin
        // TODO 2: Refactor to extract procedure.
        AbsMIF := PCurrent.UcProject.AbsMainInputFileName(ModelUseLocal);
        if AbsMIF <> '' then
          begin
            if PCurrent.MakeUcodeBatchFile(ModelUseLocal,BatchFileLocation,ErrMess) then
              begin
                AppDir := ExtractFileDir(AbsMIF);
                SetCurrentDir(AppDir);
//                WinExec(PAnsiChar('"' + BatchFileLocation + '"'),SW_SHOW);
//                WinExec(PAnsiChar(BatchFileLocation),SW_SHOW);
                BatchFileLocationTemp := AnsiString('"' + BatchFileLocation + '"');
                WinExec(PAnsiChar(BatchFileLocationTemp),SW_SHOW);
//                BatchFileLocationTemp := Q1 + BatchFileLocation + Q1;
//                WinExec(PAnsiChar(BatchFileLocationTemp),SW_SHOW);
              end
            else
              begin
                ShowMessage(ErrMess);
              end;
          end
        else
          begin
            Line := 'UCODE was not invoked because name for UCODE input' +
                    ' file has not been defined';
            ShowMessage(Line);
          end;
      end
    else
      begin
        ShowMessage('Location of UCODE executable needs to be defined.');
        OpenProgramLocations;
      end;
  finally
    slOutput.Free;
    SetCurrentDir(OrigDir);
  end;
end;

function TFormMain.GetOptParamSet(var psOpt: TParamSet; const fnPaopt: TFileName): boolean;
// Read _paopt file generated by Ucode, read parameters from it
// to populate psOpt.
var
  slPaopt: TStringList;
  I: integer;
  ParNameTemp, ParValueTemp: string;
begin
  result := False;
  if FileExists(fnPaopt) then
    begin
      slPaopt := TStringList.Create;
      try
        slPaopt.LoadFromFile(fnPaopt);
        psOpt.Clear;
        for I := 1 to slPaopt.Count - 1 do
          begin
            ParNameTemp := ParseByBlanks(slPaopt.Strings[I],1);
            if ParNameTemp <> '' then
              begin
                ParValueTemp := ParseByBlanks(slPaopt.Strings[I],2);
                psOpt.Add;
                psOpt.Items[I-1].CreateNameValue(psOpt,ConvertString12(ParNameTemp),
                                     'UnknownGroup',ParValueTemp);
              end;
          end;
        if psOpt.Count > 0 then result := True;
      finally
        slPaopt.Free;
      end;
    end;
end;

procedure TFormMain.GetParametersFromPackageFiles(NameFile: TFileName; psPkg: TParamSet);
var
  Line: string;
        { TODO 3 -cparameters :
If PVAL is not listed in name file, define a parameter set here,
based on parameters defined in Modflow-2005 package input files }
begin
  // Populate psPkg here
  Line := 'Getting parameters from MODFLOW-2005 package input files not yet' +
          ' supported.  Parameters must be listed in PVAL file';
  ShowMessage(Line);
end;

procedure TFormMain.ShowObsForm;
var
  ModRes: integer;
begin
  FormObservations.ObsSetLocal.Assign(PCurrent.ObsSet);
  FormObservations.ObsGpsLocal.Assign(PCurrent.ObsGpSet);
  FormObservations.ObservationSetupLocal.Assign(PCurrent.ObservationSetup);
  ModRes := FormObservations.ShowModal;
  if ModRes = mrOK then
    begin
      if FormObservations.ObservationDataChanged then
        begin
          PCurrent.ObsSet.Assign(FormObservations.ObsSetLocal);
          PCurrent.ObsGpSet.Assign(FormObservations.ObsGpsLocal);
          PCurrent.ObservationSetup.Assign(FormObservations.ObservationSetupLocal);
          ObsSetCurrent.Assign(FormObservations.ObsSetLocal);
          ObsGpsCurrent.Assign(FormObservations.ObsGpsLocal);
          ObservationSetupCurrent.Assign(FormObservations.ObservationSetupLocal);
          ProjChanged := True;
          EnableSaveRevert;
        end;
    end;
end;

procedure TFormMain.ShowPredForm;
var
  ModRes: integer;
begin
  FormPredictions.PredSetLocal.Assign(PCurrent.PredSet);
  FormPredictions.PredGpsLocal.Assign(PCurrent.PredGpSet);
  FormPredictions.PredictionSetupLocal.Assign(PCurrent.PredictionSetup);
  ModRes := FormPredictions.ShowModal;
  if ModRes = mrOK then
    begin
      if FormPredictions.PredictionDataChanged then
        begin
          PCurrent.PredSet.Assign(FormPredictions.PredSetLocal);
          PCurrent.PredGpSet.Assign(FormPredictions.PredGpsLocal);
          PCurrent.PredictionSetup.Assign(FormPredictions.PredictionSetupLocal);
          PredSetCurrent.Assign(FormPredictions.PredSetLocal);
          PredGpsCurrent.Assign(FormPredictions.PredGpsLocal);
          PredictionSetupCurrent.Assign(FormPredictions.PredictionSetupLocal);
          ProjChanged := True;
          EnableSaveRevert;
        end;
    end;
end;

procedure TFormMain.ShowPriForm;
var
  ModRes: integer;
begin
  ModRes := FormPriorInfo.ShowModal;
  if ModRes = mrOK then
    begin
      if FormPriorInfo.PriorDataChanged then
        begin
          PCurrent.PriSet.Assign(FormPriorInfo.PriSetLocal);
          PriSetCurrent.Assign(FormPriorInfo.PriSetLocal);
          PCurrent.PriGpSet.Assign(FormPriorInfo.PriGpsLocal);
          PriGpsCurrent.Assign(FormPriorInfo.PriGpsLocal);
          ProjChanged := True;
          EnableSaveRevert;
        end;
    end;
end;

procedure TFormMain.ProcessProjectFile(ProjectFile: string);
// Assumes PCurrent has been populated from ProjectFile prior
// to invocation of ProcessProjectFile.
// TODO 2 : Arrange so that assumption is unnecessary.
var
  Dir: string;
begin
  UCChanged := False;
  ProjChanged := False;
  PopulateControls(PCurrent);
  JvProgressDialog1.Position := 50;
  KeepCurrent := True;
  EnableSaveRevert;
  JvProgressDialog1.Position := 60;
  case PCurrent.ActiveApp of
    aaUcode:
      plApp.ActivePage := plApp.Pages[0];
    aaPest:
      plApp.ActivePage := plApp.Pages[1];
    aaApp3:
      plApp.ActivePage := plApp.Pages[0];
    aaApp4:
      plApp.ActivePage := plApp.Pages[0];
  end;
  AssignCurrentDataObjects(PCurrent);
  JvProgressDialog1.Position := 70;
  // Ensure that current directory is directory where project file resides
  Dir := ExtractFileDir(ProjectFile);
  SetCurrentDir(Dir);
  if Dir <> ProjectDirectory then
  // ProjDirectory, ModelDirectory and AppDirectory are no longer valid
  begin
    ProjectDirectory := Dir;
  end;
end;

procedure TFormMain.OpenProgramLocations;
begin
  ReadIniFile;
  FormProgramLocations.ShowModal;
  if FormProgramLocations.LocalChange then
    begin
      AssignCurrent;
      WriteIniFile;
    end;
end;

procedure TFormMain.GetParametersFromPvalFile(PVFile: TFileName;
                           psPval: TParamSet; Modeluse: TModelUse);
var
  slParams: TStringList;
  IndexPF: Integer;
  aGroup, Char, ErrMsg, Line, Path: string;
  I, NPars: Integer;
  PName: string;
  PVal: string;
  psDefault: TParamSet;
  OrigDir, ModelDir: string;
  OK: boolean;
begin
  OK := True;
  psPval.Clear;
  OrigDir := GetCurrentDir;
  // Find default group name.
  psDefault := TParamSet.Create;
  try
    psDefault.Add;
    psDefault.Items[0].Initialize('DefName','');
    aGroup := psDefault.Items[0].AllAtts[ParAttPos(patGroupName)].Text;
  finally
    psDefault.Free;
  end;
  //
  slParams := TStringList.Create;
  try
    ModelDir := PCurrent.AbsModelDirectory(ModelUse);
    SetCurrentDir(ModelDir);
    try
      slParams.LoadFromFile(PVFile);
    except
      Path := ModelDir + PathDelimiter + PVFile;
      ErrMsg := 'Error opening file: ' + Path;
      ShowMessage(ErrMsg);
      OK := False;
    end;
    // Find number of parameters listed in PVAL file (NPars)
    // and populate psModflow with parameter names and values
    if OK then
      begin
        IndexPF := 0;
        NPars := -1;
        while NPars < 0 do
          begin
            if IndexPF < slParams.Count then
              begin
                Line := slParams.Strings[IndexPF];
                if Line <> ' ' then
                  begin
                    Char := Copy(Line, 0, 1);
                    if Char <> '#' then
                    begin
                      NPars := StrToInt(GetFirstWord(Line));
                    end;
                  end;
                  IndexPF := IndexPF + 1;
              end
            else
              begin
                NPars := 0;
              end;
          end;
        if NPars > 0 then
          begin
            for I := 0 to NPars - 1 do
            begin
              Line := slParams.Strings[IndexPF];
              PName := ParseByBlanks(Line, 1);
              PVal := ParseByBlanks(Line, 2);
              psPval.Add;
              psPval.Items[I].CreateNameValue(psPval, ConvertString12(PName), ConvertString12(aGroup), PVal);
              IndexPF := IndexPF + 1;
            end;
          end;
      end;
  finally
    slParams.Free;
    SetCurrentDir(OrigDir);
  end;
end;

procedure TFormMain.rbwdgParamGpsPopulate(const psSource: TParamSet);
// Populate data grid rbwdgParamGps
var
  I, IA, IGP, IP1, J, NRows, NumCols: integer;
  BlankString, TempString: string;
  setATDisplayGps: set of TParamAttType;
  PatTemp: TParamAttType;
  TempBool: boolean;
begin
  BlankString := '';
  if PCurrent.ParGpSet.Count = 0 then
    begin
      PCurrent.ParGpSet.SetGpDefault;
    end;

  case PCurrent.ActiveApp of
    aaUcode:
      begin
        setATDisplayGps := [patAdjustable, patConstrain, patLowerBound,
                      patUpperBound, patLowerValue, patUpperValue,
                      patMaxChange, patPerturbAmt, patScalePval,
                      patSenMethod, patTolPar, patTransform];
      end;
    aaPest:
      begin
        setATDisplayGps := [patAdjustable, patLowerBound,
                      patUpperBound, patTransform];
      end;
    aaApp3:
      begin
        setATDisplayGps := [patAdjustable, patLowerBound,
                      patUpperBound, patTransform];
      end;
    aaApp4:
      begin
        setATDisplayGps := [patAdjustable, patLowerBound,
                      patUpperBound, patTransform];
      end;
  end;
  // Later: atNonLinearInterval, atSosIncrement,
  // Count number of columns needed.
  NumCols := 1;
  for I := 0 to NumParAttributes - 1 do
    if (ParameterSetupCurrent.ParAttributes[I].ParamAttType in setATDisplayGps) and
       (ParameterSetupCurrent.ParAttributes[I].ControlMethod = cmByGroup) then
            NumCols := NumCols + 1;
  rbwdgParamGps.ColCount := NumCols;
  if rbwdgParamGps.ColCount > 1 then
    begin
      rbwdgParamGps.FixedCols := 1;
    end;
  // Clear table headings and cells
  rbwdgParamGps.DefaultColWidth := 50;
  rbwdgParamGps.Columns[0].AutoAdjustColWidths := True;
  for I := 1 to rbwdgParamGps.ColCount - 1 do
    begin
      rbwdgParamGps.Columns[I].AutoAdjustColWidths := False;
      rbwdgParamGps.Columns[I].CaptionAlignment := taLeftJustify;
      rbwdgParamGps.Cells[I,0] := BlankString;
      for J := 1 to rbwdgParamGps.RowCount - 1 do
        begin
          try
            rbwdgParamGps.Cells[I, J] := BlankString;
          except
            On E: EListError do
              ShowMessage('Caught EListError exception');
          end;
        end;
    end;
  // After all cells are cleared, assign ColWidths to minimal width.
  for I := 0 to rbwdgParamGps.ColCount - 1 do
    begin
      rbwdgParamGps.Columns[I].ComboUsed := False;
      rbwdgParamGps.ColWidths[I] := rbwdgParamGps.DefaultColWidth;
      rbwdgParamGps.Columns[I].AutoAdjustColWidths := True;
    end;
  // Populate column headings and assign properties for selected columns
  NRows := psSource.Count +1;
  if NRows < 2 then NRows := 2;
  rbwdgParamGps.RowCount := NRows;
  J := 0;
  rbwdgParamGps.Cells[J,0] := 'Group Name';
  for I := 0 to NumParAttributes - 1 do
  begin
    if (ParameterSetupCurrent.ParAttributes[I].ControlMethod = cmByGroup) and
       (ParameterSetupCurrent.ParAttributes[I].ParamAttType in setATDisplayGps) then
    begin
      case ParameterSetupCurrent.ParAttributes[I].ParamAttType of
        patAdjustable: begin
            J := J + 1;
            rbwdgParamGps.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParamGps.Columns[J].Format := rcf4Boolean;
            rbwdgParamGps.Columns[J].LimitToList := False;
          end;
        patConstrain: begin
            J := J + 1;
            rbwdgParamGps.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParamGps.Columns[J].Format := rcf4Boolean;
            rbwdgParamGps.Columns[J].LimitToList := False;
          end;
        patLowerBound: begin
            J := J + 1;
            rbwdgParamGps.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParamGps.Columns[J].Format := rcf4Real;
            rbwdgParamGps.Columns[J].LimitToList := False;
          end;
        patUpperBound: begin
            J := J + 1;
            rbwdgParamGps.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParamGps.Columns[J].Format := rcf4Real;
            rbwdgParamGps.Columns[J].LimitToList := False;
          end;
        patMaxChange: begin
            J := J + 1;
            rbwdgParamGps.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParamGps.Columns[J].Format := rcf4Real;
            rbwdgParamGps.Columns[J].LimitToList := False;
          end;
        patPerturbAmt: begin
            J := J + 1;
            rbwdgParamGps.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParamGps.Columns[J].Format := rcf4Real;
            rbwdgParamGps.Columns[J].LimitToList := False;
          end;
        patLowerValue: begin
            J := J + 1;
            rbwdgParamGps.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParamGps.Columns[J].Format := rcf4Real;
            rbwdgParamGps.Columns[J].LimitToList := False;
          end;
        patUpperValue: begin
            J := J + 1;
            rbwdgParamGps.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParamGps.Columns[J].Format := rcf4Real;
            rbwdgParamGps.Columns[J].LimitToList := False;
          end;
        patScalePval: begin
            J := J + 1;
            rbwdgParamGps.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParamGps.Columns[J].Format := rcf4Real;
            rbwdgParamGps.Columns[J].LimitToList := False;
          end;
        patSenMethod: begin
            J := J + 1;
            rbwdgParamGps.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParamGps.Columns[J].Format := rcf4String;
            rbwdgParamGps.Columns[J].LimitToList := True;
            rbwdgParamGps.Columns[J].PickList := slSenMethod;
            rbwdgParamGps.Columns[J].ComboUsed := True;
          end;
        patSosIncrement: begin
            J := J + 1;
            rbwdgParamGps.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParamGps.Columns[J].Format := rcf4Real;
            rbwdgParamGps.Columns[J].LimitToList := False;
          end;
        patTolPar: begin
            J := J + 1;
            rbwdgParamGps.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParamGps.Columns[J].Format := rcf4Real;
            rbwdgParamGps.Columns[J].LimitToList := False;
          end;
        patTransform: begin
            J := J + 1;
            rbwdgParamGps.Cells[J,0] :=
                 ParameterSetupCurrent.ParAttributes[I].Caption;
            rbwdgParamGps.Columns[J].Format := rcf4Boolean;
            rbwdgParamGps.Columns[J].LimitToList := False;
          end;
      end;
    end;
  end;
  // Populate table cells
  // Loop through groups; each iteration populates a row
  for IGP := 0 to psSource.Count - 1 do
  begin
    IP1 := IGP + 1;
    rbwdgParamGps.Cells[0,IP1] := ConvertString(psSource.Items[IGP].Name);
    J := 0;
    for IA := 0 to NumParAttributes - 1 do
    begin
      PatTemp := ParameterSetupCurrent.ParAttributes[IA].ParamAttType;
      if (ParameterSetupCurrent.ParAttributes[IA].ControlMethod = cmByGroup) and
         (PatTemp in setATDisplayGps) then
        begin
          J := J + 1;
          TempString := psSource.Items[IGP].AllAtts[IA].Text;
          if TempString = '' then
            TempString := psSource.Items[IGP].AllAtts.Items[IA].DefaultText;
          PatTemp := psSource.Items[IGP].AllAtts[IA].ParamAttType;
          if ((PatTemp = patDerived) or (PatTemp = patTransform)
              or (PatTemp = patAdjustable) or (PatTemp = patConstrain)) then
            begin
              // Check or uncheck box depending on boolean value.
              TempBool := YesOrNoToBoolean(TempString);
              IgnoreGpsStateChange := True;
              rbwdgParamGps.Checked[J,IP1] := TempBool;
              IgnoreGpsStateChange := False;
            end
          else if PatTemp = patScalePval then               
            begin
              if AnsiSameText(TempString,'Starting parameter value / 100') then
                begin
                  rbwdgParamGps.Cells[J,IP1] := '1.0E-10'; // Only positive numeric values are valid.
                end;
              rbwdgParamGps.Cells[J,IP1] := TempString;
            end
          else
            begin
              ReplaceParAttExpl(TempString,PatTemp);
              rbwdgParamGps.Cells[J,IP1] := TempString;
            end;
        end;
    end;
  end;
  rbwdgParamGps.Invalidate;
end;

procedure TFormMain.EnableSaveRevert;
begin
  if ProjChanged then
    begin
      tbSave.Enabled := True;
      tbSave.ImageIndex := 2;
      tbRevert.Enabled := True;
      tbRevert.ImageIndex := 4;
    end
  else
    begin
      tbSave.Enabled := False;
      tbSave.ImageIndex := 6;
      tbRevert.Enabled := False;
      tbRevert.ImageIndex := 5;
    end;
end;

procedure TFormMain.rbForwardClick(Sender: TObject);
begin
  ColorRadioButtons(rbForward);
  if PCurrent.UcProject.UcMode <> umFwd then
  begin
    PCurrent.UcProject.AssignMode(umFwd);
    UCChanged := True;
    ProjChanged := True;
    AssignCurrent;
  end;
end;

procedure TFormMain.rbSenAnalClick(Sender: TObject);
begin
  ColorRadioButtons(rbSenAnal);
  if PCurrent.UcProject.UcMode <> umSensAnal then
  begin
    PCurrent.UcProject.AssignMode(umSensAnal);
    UCChanged := True;
    ProjChanged := True;
    AssignCurrent;
  end;
end;

procedure TFormMain.rbParEstClick(Sender: TObject);
begin
  ColorRadioButtons(rbParEst);
  if PCurrent.UcProject.UcMode <> umParEst then
  begin
    PCurrent.UcProject.AssignMode(umParEst);
    UCChanged := True;
    ProjChanged := True;
    AssignCurrent;
  end;
end;

procedure TFormMain.rbTestLinClick(Sender: TObject);
begin
  ColorRadioButtons(rbTestLin);
  if PCurrent.UcProject.UcMode <> umTestLin then
  begin
    PCurrent.UcProject.AssignMode(umTestLin);
    UCChanged := True;
    ProjChanged := True;
    AssignCurrent;
  end;
end;

procedure TFormMain.rbwdgParamsColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  Invalidate;
  Repaint;
end;

procedure TFormMain.rbwdgParamsEndUpdate(Sender: TObject);
begin
  if ParamCellChanged then
    begin
      ProjChanged := True;
      EnableSaveRevert;
    end;
//  StatusBar1.Panels[1].Text := '';
  StatusClear;
end;

procedure TFormMain.rbwdgParamsEnter(Sender: TObject);
begin
  rbwdgParamsPopulate(ParamSetCurrent);
end;

procedure TFormMain.rbwdgParamsExit(Sender: TObject);
var
  ErrRow, FirstBlankParRow, NParLines, NParNames: integer;
  OK: boolean;
  ErrMsg, ErrName: string;
begin
  // Check parameter table entries
  // Count lines not filled with blanks in parameters table
  // If NParNames < NParLines, there's at least one blank parameter name
  CountParLines(rbwdgParams,NParLines,NParNames,FirstBlankParRow);
  if (NParLines = NParNames) then
    begin
      OK := True;
    end
  else
    begin
      ShowMessage('Error in Parameters Table: At least one parameter is unnamed');
      rbwdgParams.SetFocus;
      rbwdgParams.Column := 1;
      rbwdgParams.Row := FirstBlankParRow;
      OK := False;
    end;
  if OK then
    begin
      // Check names in Parameters table
      OK := CheckNamesInColumn(rbwdgParams,0,MaxLenParName,ErrName,ErrRow);
      if not OK then
        begin
          if (rbwdgParams.RowCount > 2) or (IsNonBlank(rbwdgParams.Cells[0,1])) then
            begin
              ErrMsg := 'Invalid parameter name ' + ErrName;
              ShowMessage(ErrMsg);
              rbwdgParams.Col := 0;
              rbwdgParams.Row := ErrRow;
              rbwdgParams.SetFocus;
            end;
        end;
    end;
  if OK then
    begin
      // Check names in Parameter Groups table
      OK := CheckNamesInColumn(rbwdgParamGps,0,MaxLenGpName,ErrName,ErrRow);
      if not OK then
        begin
          if (rbwdgParamGps.RowCount > 2) or (IsNonBlank(rbwdgParamGps.Cells[0,1])) then
            begin
              ErrMsg := 'Invalid parameter group name ' + ErrName;
              ShowMessage(ErrMsg);
              rbwdgParamGps.Col := 0;
              rbwdgParamGps.Row := ErrRow;
              rbwdgParamGps.SetFocus;
            end;
        end;
    end;
  if OK then
    AssignParameters;
  StatusBar1.Panels[1].Text := '';
end;

procedure TFormMain.rbwdgParamsGetEditText(Sender: TObject; ACol, ARow: Integer;
  var Value: string);
begin
  ParamCellText := Value;
  ParamCellChanged := False;
  StatusClear;
end;

procedure TFormMain.rbwdgParamsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssCtrl in Shift then
    begin
      SetColorSelectedRow(rbwdgParams,False);
    end
  else
    begin
      SetColorSelectedRow(rbwdgParams,True);
    end;
  if ssShift in Shift then
    begin
      AllowEditing(rbwdgParams,False);
      SetColorSelectedRow(rbwdgParams,False);
    end
  else
    begin
      AllowEditing(rbwdgParams,True);
    end;
  StatusClear;
end;

procedure TFormMain.rbwdgParamsSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  if (ACol = rbwdgParams.Column) and (ACol >= 0) and (ARow >= 0) then
    begin
      StatusBar1.Panels[1].Text := ' ' + GetParHint(rbwdgParams, ACol);
      if Arow > 0 then
        ParamCellSelected := True
      else
        ParamCellSelected := False;
    end;
end;

procedure TFormMain.rbwdgParamsSetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
begin
  LastCol := ACol;
  LastRow := ARow;
  if Value <> ParamCellText then
    begin
      ParamCellChanged := True;
      ParamCellText := Value;
    end;
  StatusClear;
end;

procedure TFormMain.rbwdgParamsStateChange(Sender: TObject; ACol, ARow: Integer;
  const Value: TCheckBoxState);
begin
  if not IgnoreParStateChange then
    begin
      ProjChanged := True;
      AssignParameters;
      PCurrent.ParamSet.Assign(ParamSetCurrent);
      EnableSaveRevert;
    end;

  // TODO 2: Does this code need to be used when a parameter name or other data is changed?
  // It would go into another procedure--this one is invoked only when a checkbox is changed.
//  IgnoreChange := False;
//  ColChgd := rbwdgParams.Col; // Selected column.
//  // Ensure that a parameter is present to be edited.
//  if ParamSetCurrent <> nil then
//    begin
//      // Loop through rows (parameters).
//      for IRow := 1 to rbwdgParams.RowCount - 1 do
//        begin
//          IPar := IRow - 1;
//          // Get parameter name from column 0 of current row.
//          // Check validity of parameter name.
//          TempName := TrimLeadingAndTrailingBlanks(rbwdgParams.Cells[0, IRow]);
//          rbwdgParams.Cells[0, IRow] := TempName;
//          if J_Valid_Name(TempName) then
//            begin
//              if ParamSetCurrent.Count < IRow then
//                begin
//                  ParamSetCurrent.Add;
//                  ParamSetCurrent.Items[IPar].Name := TempName;
//                  for ICol := 1 to rbwdgParams.ColCount - 1 do
//                    begin
//                      TempCaption := rbwdgParams.Cells[ICol,0];
//                      // Find index of parameter attribute that has this caption.
//                      IPA := PosCap(TempCaption);
//                      // Assign data grid value to parameter attribute.
//                      patTemp := ParamSetCurrent.Items[IPar].AllAtts[IPA].ParamAttType;
//                      TempString := ParamSetCurrent.Items[IPar].AllAtts[IPA].DefaultText;
//                      ReplaceParAttValue(TempString, patTemp);
//                      rbwdgParams.Cells[ICol,IRow] := TempString;
//                      if (patTemp = patGroupName) and (TempString = 'ParamDefault') then
//                        // Ensure that 'ParamDefault' is included in parameter groups.
//                        begin
//                          ParamDefaultFound := False;
//                          for I := 0 to PCurrent.ParGpSet.Count - 1 do
//                            begin
//                              if AnsiSameText(PCurrent.ParGpSet.Items[I].GetAttributeByType(patGroupName),'ParamDefault') then
//                                ParamDefaultFound := True;
//                            end;
//                          if not ParamDefaultFound then
//                            begin
//                              I := PCurrent.ParGpSet.Count;
//                              PCurrent.ParGpSet.Add;
//                              PCurrent.ParGpSet.Items[I].SetAttributeByType(patGroupName,'ParamDefault');
//                            end;
//                        end;
//                    end;
//                end
//              else
//                begin
//                  // If parameter is being renamed, change name in ParamSetCurrent.
//                  if ColChgd = 0 then
//                    begin
//                      ParamSetCurrent.Items[IPar].Name := TempName;
//                      UpdateCurrentProject;
//                    end;
//                end;
//            end
//          else
//            begin
//              if TempName <> '' then
//                begin
//                  Messg := 'Invalid name: ' + TempName;
//                  ShowMessage(Messg);
//                end;
//              if IRow <= ParamSetLocal.Count then
//                rbwdgParams.Cells[0, IRow] := ParamSetLocal.Items[IRow-1].Name;
//            end;
//        end;
//    end;
//  if not IgnoreChange then
//    begin
//      ProjChanged := True;
//      EnableSaveRevert;
//      //AssignCurrent;
//    end;
//  Invalidate;
end;

procedure TFormMain.rbPredClick(Sender: TObject);
begin
  ColorRadioButtons(rbPred);
  if PCurrent.UcProject.UcMode <> umPred then
  begin
    PCurrent.UcProject.AssignMode(umPred);
    UCChanged := True;
    ProjChanged := True;
    AssignCurrent;
  end;
end;

procedure TFormMain.rbAdvTestLinClick(Sender: TObject);
begin
  ColorRadioButtons(rbAdvTestLin);
  if PCurrent.UcProject.UcMode <> umAdvTestLin then
  begin
    PCurrent.UcProject.AssignMode(umAdvTestLin);
    UCChanged := True;
    ProjChanged := True;
    AssignCurrent;
  end;
end;

procedure TFormMain.rbNonLinUncClick(Sender: TObject);
begin
  ColorRadioButtons(rbNonLinUnc);
  if PCurrent.UcProject.UcMode <> umNonlinUncert then
  begin
    PCurrent.UcProject.AssignMode(umNonlinUncert);
    UCChanged := True;
    ProjChanged := True;
    AssignCurrent;
  end;
end;

procedure TFormMain.rbInvObjFnClick(Sender: TObject);
begin
  ColorRadioButtons(rbInvObjFn);
  if PCurrent.UcProject.UcMode <> umInvObjFunc then
    begin
      PCurrent.UcProject.AssignMode(umInvObjFunc);
      UCChanged := True;
      ProjChanged := True;
      AssignCurrent;
    end;
end;

procedure TFormMain.AboutModelMate1Click(Sender: TObject);
begin
  // Open About form.
  FormAbout.ShowModal;
end;

procedure TFormMain.actCheckProjectExecute(Sender: TObject);
begin
  // TODO 3 : Write code to check project settings
  // Write code to check that:
  // Program locations are valid
  // Command(s) to invoke model is(are) valid
  // Model input and template files are valid
  // All parameters are listed in at least one template file
  // (unless parameter is use to define a derived parameter)
  // Model output and instruction files are valid
  // All non-derived dependents are listed exactly once in extraction instructions
  // No dependent is listed more than once in extraction instructions
  // All variables required to define observation weights are assigned and valid
  // If model is a Modflow model, that Name file is valid
end;

procedure TFormMain.actCloseChildWinExecute(Sender: TObject);
begin
  if UCParEstSetWinOpen then
    begin
      frmParEstSettings.Close;
    end;
  if UcodeSetWinOpen then
    begin
      frmUcodeSettings.Close;
    end;
  FormEditorAppInput.Close;
  FormEditorAppOutput.Close;
end;

procedure TFormMain.actExitExecute(Sender: TObject);
begin
  if OKToCloseProject then Close;
end;

procedure TFormMain.actFileNewExecute(Sender: TObject);
var
  Msg: string;
  ModalResult: integer;
  OK: boolean;
begin
  OK := True;
  // Save current project?
  if ProjChanged then
    begin
      Msg := 'Current project has not been saved.  Save now?';
      ModalResult := MessageDlg(Msg, mtConfirmation, [mbYes,mbNo,mbCancel], 0);
      case ModalResult of
        mrYes: actFileSaveExecute(Sender);
        mrNo: OK := True;
        mrCancel: OK := False;
      end;
    end;
  if OK then
    begin
      PLastSaved.Assign(PCurrent);
      InitializeMemoryObjects;
      ClearAllData;
      // Load default project
      PCurrent.Assign(PDefault);
      AssignCurrentParamObjects(PCurrent);
      UCChanged := False;
      StatusBar1.Panels[1].Text := 'New Project';
      UpdateMainForm;
      ProjChanged := False;
      EnableSaveRevert;
    end;
end;

procedure TFormMain.actFileOpenExecute(Sender: TObject);
var
  ProjectFile, Str: string;
begin
  // Save current project?
  // Do something like this: actFileSaveExecute(Sender);
  if OKToCloseProject then
    begin
      if ProjChanged then actFileSaveExecute(Sender);
      // Open project file into PCurrent    need code
      JvProgressDialog1.InitValues(0,100,1,0,'Progress','Opening project file');
      JvProgressDialog1.Smooth := True;
      JvProgressDialog1.ShowCancel := False;
      odProject.Title := 'Open Project';
      if odProject.Execute then
        begin
          JvProgressDialog1.Show;
          ProjectFile := odProject.FileName;
          Str := FileBaseName(ProjectFile);
          PCurrent.FileName := ProjectFile;
          LoadingDone := False;
          JvProgressDialog1.Position := 10;
          if OpenAFile(ProjectFile) then;
            begin
              JvProgressDialog1.Position := 40;
              ProcessProjectFile(ProjectFile);
            end;
        end;
      JvProgressDialog1.Position := 80;
      PLastSaved.Assign(PCurrent);
      JvProgressDialog1.Position := 85;
      UCChanged := False;
      ParamGpsChanged := False;
      UpdateMainForm;
      JvProgressDialog1.Position := 90;
      ProjChanged := False;
      EnableSaveRevert;
      LoadingDone := True;
      JvProgressDialog1.Hide;
    end;
end;

procedure TFormMain.actFileSaveAsExecute(Sender: TObject);
var
  OrigProjDir, FNTemp, Messg: string;
  OK: boolean;
  MR: integer;
  UcodeFileTemp: string;
begin
  sdProject.Title := 'Save Project As';
  OrigProjDir := ProjectDirectory;
  if ProjectDirectory <> PathDelimiter then
    begin
      if DirectoryExists(ProjectDirectory) then
        begin
          sdProject.InitialDir := ProjectDirectory;
        end;
    end;
  if PCurrent.FileName <> '' then
    FNTemp := PCurrent.FileName
  else if ProjFileTemp <> '' then
    FNTemp := ProjFileTemp
  else
    FNTemp := sdProject.FileName;
  ProjFileTemp := '';
  sdProject.FileName := FNTemp;
  if sdProject.Execute then
    begin
      OK := True;
      // If file exists, ask user if OK to overwrite
      UcodeFileTemp := SdProject.FileName;
      if (FileExists(UcodeFileTemp)) and (UcodeFileTemp <> PCurrent.FileName) then
        begin
          Messg := UcodeFileTemp + ' already exists.  Do you want to replace it?';
          MR := MessageDlg(Messg,mtConfirmation,[mbYes,mbNo],0);
          OK := MR = mrYes;
        end;
      if OK then
        begin
          AssignProjectDirectory(UcodeFileTemp, OrigProjDir);
          //
          if PCurrent.NameLikeProjectFile then
            begin
              PCurrent.ProjName := ConvertString255(FileBaseName(PCurrent.FileName));
            end;
          actFileSaveExecute(Sender);
          SetCaption;
          PopulateControls(PCurrent);
        end;
    end;
end;

procedure TFormMain.actFileSaveExecute(Sender: TObject);
var
  TempDir: TFileName;
begin
  // Save current project
  if (PCurrent.FileName = '') then
    begin
      actFileSaveAsExecute(Sender);
    end
  else
    begin
      JvProgressDialog1.InitValues(0,100,1,0,'Progress','Saving project file');
      JvProgressDialog1.Smooth := True;
      JvProgressDialog1.ShowCancel := False;
      JvProgressDialog1.Show;
      if ParamGpsChanged then SaveParamGpsChanges;
      JvProgressDialog1.Position := 5;
      if AssignParameters then
        begin
          JvProgressDialog1.Position := 10;
          UpdateCurrentProject;
          JvProgressDialog1.Position := 15;
          AssignCurrentDataObjects(PCurrent);
          JvProgressDialog1.Position := 20;
          PCurrent.FileName := ChangeFileExt(PCurrent.FileName, '.mtc');
          JvProgressDialog1.Position := 25;
          SaveProjectFile(PCurrent.FileName);
          // Ensure that PCurrent.FileName is a pathname, not just a filename
          JvProgressDialog1.Position := 80;
          TempDir := ExtractFileDir(PCurrent.FileName);
          if TempDir <> '' then
            ProjectDirectory := TempDir
          else
            // Assign PCurrent.FileName as ProjectDirectory + PCurrent.FileName.
            begin
              TempDir := RelDirToAbsDir(ProjectDirectory, PathDelimiter);
              PCurrent.FileName := DirFilToAbsPath(TempDir, PCurrent.FileName);
            end;
          JvProgressDialog1.Position := 85;
          PLastSaved.Assign(PCurrent);
          JvProgressDialog1.Position := 90;
          UCChanged := False;
          ProjChanged := False;
          ParamCellChanged := False;
          ParamGpsChanged := False;
          ObservationsChanged := False;
          PredictionsChanged := False;
          PriorChanged := False;
          ParameterSetupChanged := False;
          ObservationSetupChanged := False;
          PredictionSetupChanged := False;
          PriorSetupChanged := False;
          JvProgressDialog1.Position := 95;
          EnableSaveRevert;
        end;
      JvProgressDialog1.Hide;
    end;
  SetCaption;
end;

procedure TFormMain.actImportModflow2005ObservationsExecute(Sender: TObject);
var
  FName, Line, Messg, NameFilePath, OCount: string;
  AbsModelDir, OrigDir : string;
  I, IAtt, Index, MessageResponse, NObsFiles, PlotSymbol: integer;
  HOBDRY: string;
  OK: boolean;
  slNames: TStringList;
  dsModflow, dsTemp: TDepSet;
  NObsPkg: integer;
  NumObs: array [0..4] of integer;
  ObsPkgs: array [0..4] of string;
  ObsFiles: array [0..4] of TFileName;
  Iuobsv: array [0..4] of integer;
  GpName: array [1..4] of string12;
begin
  NObsFiles := 0;
  NObsPkg := 5;  // Use order used internally by Modflow-2005
  ObsPkgs[0] := 'HOB';
  ObsPkgs[1] := 'DROB';
  ObsPkgs[2] := 'RVOB';
  ObsPkgs[3] := 'GBOB';
  ObsPkgs[4] := 'CHOB';
  GpName[1] := 'DRN_flows';
  GpName[2] := 'RIV_flows';
  GpName[3] := 'GHB_flows';
  GpName[4] := 'CHOB_flows';
  OrigDir := GetCurrentDir;
  for I := 0 to NObsPkg - 1 do
    begin
      ObsFiles[I] := '';
      Iuobsv[I] := 0;
      NumObs[I] := 0;
    end;
  dsModflow := TDepSet.Create;
  dsTemp := TDepSet.Create;
  try
  OK := True;
  if OK then
    begin
      if UseCurrentNameFile then
        begin
          if PCurrent.ModflowNameFile = '' then
            OK := PCurrent.LocateModflowNameFile(muCalib)
          else
            // ModflowNameFile is not blank.
            if not FileExists(PCurrent.AbsModflowNameFile(muCalib)) then
              OK := PCurrent.LocateModflowNameFile(muCalib);
          UseCurrentNameFile := False;
        end
      else
        OK := PCurrent.LocateModflowNameFile(muCalib);
    end;
  //
  if OK then
    begin
      PCurrent.ModelDirectory := ExtractFileDir(PCurrent.ModflowNameFile);
      if PCurrent.ObsSet.Count > 0 then
        begin
          OCount := IntToStr(PCurrent.ObsSet.Count);
          Messg := 'This action will delete ' + OCount + ' existing observations.  Continue?';
          MessageResponse := MessageDlg(Messg,mtConfirmation,[mbOk, mbCancel],0);
          if MessageResponse = MrCancel then OK := False;
        end;
    end;
  //
  if OK then
    begin
      PCurrent.ModelID := midModflow2005;
      // Process name file and store names of Obs process input files
      slNames := TStringList.Create;
      try
      NameFilePath := PathToAbsPath(ProjectDirectory, PCurrent.ModflowNameFile);
      slNames.LoadFromFile(NameFilePath);
      for Index := 0 to slNames.Count - 1 do
        begin
          Line := slNames.Strings[Index];
          FName := GetFirstWord(Line);
          for I := 0 to NObsPkg - 1 do
            begin
              if SameText(FName,ObsPkgs[I]) then
                begin
                  ObsFiles[I] := ParseByBlanks(Line,3);
                  NObsFiles := NObsFiles + 1;
                end;
            end;
        end;
      finally
        slNames.Free;
      end;
      if NObsFiles > 0 then
        begin
          AbsModelDir := PCurrent.AbsModelDirectory(muCalib);
          SetCurrentDir(AbsModelDir);
          PlotSymbol := 1;
          // Set up progress bar
          JvProgressDialog1.ShowCancel := False;
          JvProgressDialog1.InitValues(0,100,1,0,'Progress','Reading Observation Data');
          JvProgressDialog1.Smooth := True;
          JvProgressDialog1.Show;
          //
          // Get observations from each package's observation input file
          for I := 0 to NObsPkg - 1 do
            begin
              if ObsFiles[I] <> '' then
                begin
                  dsTemp.Clear;
                  case I of
                    // Order is that used internally by Modflow-2005
                    0:
                      begin
                        GetMF2005HeadObs(ObsFiles[I], dsTemp, PlotSymbol,
                                        Iuobsv[I], HOBDRY, NumObs[I],
                                        JvProgressDialog1);
                        if NumObs[I] > 0 then
                          begin
                            PCurrent.UcProject.OmitVals.Add(HOBDRY)  ;
                          end;
                      end;
                    1: GetMF2005FlowObs(ObsFiles[I], dsTemp, PlotSymbol,
                                        GpName[I], Iuobsv[I], NumObs[I]);
                    2: GetMF2005FlowObs(ObsFiles[I], dsTemp, PlotSymbol,
                                        GpName[I], Iuobsv[I], NumObs[I]);
                    3: GetMF2005FlowObs(ObsFiles[I], dsTemp, PlotSymbol,
                                        GpName[I], Iuobsv[I], NumObs[I]);
                    4: GetMF2005FlowObs(ObsFiles[I], dsTemp, PlotSymbol,
                                        GpName[I], Iuobsv[I], NumObs[I]);
                  end;

                  if dsTemp.Count > 0 then
                    begin
                      dsModflow.Append(dsTemp);
                    end;
                end;
            end;
          SetCurrentDir(OrigDir);
        end;
      PCurrent.ObsSet.Assign(dsModflow);
      FormObservations.NeedObsPopulate := True;
      PCurrent.DefineObsGroups;
      PCurrent.DefineObsSetup;
      PCurrent.UseObsGps := True;
      IAtt := PosDepCap('Group Name');
      PCurrent.ObservationSetup.ObsAttributes.Items[IAtt].ControlMethod := cmByItem;
      ObsSetCurrent.Assign(PCurrent.ObsSet);
      ObsGpsCurrent.Assign(PCurrent.ObsGpSet);
      ObservationSetupCurrent.Assign(PCurrent.ObservationSetup);
      ProjChanged := True;
      AssignCurrent;
      PCurrent.MOFiles.Clear;
      if not BuildMF2005InstructFiles(PCurrent, Iuobsv, NumObs, muCalib) then
        begin
          OK := False;
          Line := 'Error encountered trying to build instruction files.  ' +
                  'Correct problem and reimport observations.';
          ShowMessage(Line);
        end;
      if OK then
        begin
          // Open Observations form so user can define attributes for observation groups
          ShowObsForm;
          PopulateObsTabSheet(PCurrent);
        end;
    end;
  finally
      dsModflow.Free;
      dsTemp.Free;
  end;
end;

procedure TFormMain.actImportModflow2005ParametersExecute(Sender: TObject);
var
  NameFile, PValFile: TFileName;
  ModelDir, Messg, PCount: string;
  MessageResponse, UnitNum: integer;
  OK: boolean;
  psModflow: TParamSet;
begin
{ TODO 4 -cparameters : To add support for reading parameters
from an MF2K SEN file, copy this procedure }
  //
  psModflow := TParamSet.Create;
  OK := True;
  try
  ModelDir := '';
  if DirectoryExists(PCurrent.AbsModelDirectory(muCalib)) then ModelDir := PCurrent.ModelDirectory;
  if OK then
    begin
      if UseCurrentNameFile then
        begin
          if PCurrent.ModflowNameFile = '' then
            OK := PCurrent.LocateModflowNameFile(muCalib)
          else
            // ModflowNameFile is not blank.
            if not FileExists(PCurrent.AbsModflowNameFile(muCalib)) then
              OK := PCurrent.LocateModflowNameFile(muCalib);
          UseCurrentNameFile := False;
        end
      else
        OK := PCurrent.LocateModflowNameFile(muCalib);
    end;
  //
  if OK then
    begin
      if PCurrent.ParamSet.Count > 0 then
        begin
          PCount := IntToStr(PCurrent.ParamSet.Count);
          Messg := 'This action will delete ' + PCount + ' existing parameters.  Continue?';
          MessageResponse := MessageDlg(Messg,mtConfirmation,[mbOk, mbCancel],0);
          if MessageResponse = MrCancel then OK := False;
          if OK then PCurrent.ParamSet.Clear;
        end;
      if OK then
        begin
          PCurrent.ModelID := midModflow2005;
          NameFile := PathToAbsPath(ProjectDirectory, PCurrent.ModflowNameFile);
          if ModflowUnit.GetNameFileEntry(NameFile,'PVAL',UnitNum,PValFile) then
            begin
              GetParametersFromPvalFile(PValFile,psModflow,muCalib);
            end
          else
            begin
              GetParametersFromPackageFiles(NameFile, psModflow);
            end;
          if psModflow.Count > 0 then
            begin
              PCurrent.ParamSet.Assign(psModflow);
              if PCurrent.ParGpSet.Count = 0 then PCurrent.ParGpSet.SetGpDefault;
              if BuildMF2005PvalTemplate(PCurrent, Messg, muCalib) then
                begin
                  NeedPvalTemplate := False;
                end
              else
                begin
                  ShowMessage(Messg);
                  if PCurrent.LinkTemplateToParamsTable then NeedPvalTemplate := True;
                end;
              ProjChanged := True;
              AssignCurrent;
              AssignCurrentParamObjects(PCurrent);
              rbwdgParamGpsPopulate(ParamGpsCurrent);
              rbwdgParamsPopulate(ParamSetCurrent);
            end;
        end;
      ProjChanged := True;
      AssignCurrent;
    end;
  finally
    psModflow.Free;
  end;
end;

procedure TFormMain.actImportModflow2005ParamsAndObsExecute(Sender: TObject);
begin
  UseCurrentNameFile := False;
  actImportModflow2005ParametersExecute(self);
  UseCurrentNameFile := True;
  actImportModflow2005ObservationsExecute(self);
end;

procedure TFormMain.actImportModflow2005PredictionsExecute(Sender: TObject);
var
  FName, Line, Messg, NameFilePath, PrCount: string;
  AbsModelDir, OrigDir: string;
  I, IAtt, Index, MessageResponse, NPredFiles, PlotSymbol: integer;
  HOBDRY: string;
  OK: boolean;
  slNames: TStringList;
  psModflow, psTemp: TDepSet;
  NPredPkg: integer;
  NumPred: array [0..4] of integer;
  PredPkgs: array [0..4] of string;
  PredFiles: array [0..4] of TFileName;
  Iuobsv: array [0..4] of integer;
  GpName: array [1..4] of string12;
begin
  NPredFiles := 0;
  NPredPkg := 5;  // Use order used internally by Modflow-2005.
  PredPkgs[0] := 'HOB';
  PredPkgs[1] := 'DROB';
  PredPkgs[2] := 'RVOB';
  PredPkgs[3] := 'GBOB';
  PredPkgs[4] := 'CHOB';
  GpName[1] := 'DRN_flows';
  GpName[2] := 'RIV_flows';
  GpName[3] := 'GHB_flows';
  GpName[4] := 'CHOB_flows';
  OrigDir := GetCurrentDir;
  for I := 0 to NPredPkg - 1 do
    begin
      PredFiles[I] := '';
      Iuobsv[I] := 0;
      NumPred[I] := 0;
    end;
  psModflow := TDepSet.Create;
  psTemp := TDepSet.Create;
  try
  OK := True;
  if OK then
    begin
      if UseCurrentNameFile then
        begin
          if PCurrent.ModflowNameFilePred = '' then
            OK := PCurrent.LocateModflowNameFile(muPred)
          else
            // ModflowNameFilePred is not blank.
            if not FileExists(PCurrent.AbsModflowNameFile(muPred)) then
              OK := PCurrent.LocateModflowNameFile(muPred);
          UseCurrentNameFile := False;
        end
      else
        OK := PCurrent.LocateModflowNameFile(muPred);
    end;
  //
  if OK then
    begin
      PCurrent.ModelDirectoryPred := ExtractFileDir(PCurrent.ModflowNameFilePred);
      if PCurrent.PredSet.Count > 0 then
        begin
          PrCount := IntToStr(PCurrent.PredSet.Count);
          Messg := 'This action will delete ' + PrCount + ' existing predictions.  Continue?';
          MessageResponse := MessageDlg(Messg,mtConfirmation,[mbOk, mbCancel],0);
          if MessageResponse = MrCancel then OK := False;
        end;
    end;
  //
  if OK then
    begin
      PCurrent.ModelID := midModflow2005;
      // Process name file and store names of Obs process input files.
      slNames := TStringList.Create;
      try
      NameFilePath := PathToAbsPath(ProjectDirectory, PCurrent.ModflowNameFilePred);
      slNames.LoadFromFile(NameFilePath);
      for Index := 0 to slNames.Count - 1 do
        begin
          Line := slNames.Strings[Index];
          FName := GetFirstWord(Line);
          for I := 0 to NPredPkg - 1 do
            begin
              if SameText(FName,PredPkgs[I]) then
                begin
                  PredFiles[I] := ParseByBlanks(Line,3);
                  NPredFiles := NPredFiles + 1;
                end;
            end;
        end;
      finally
        slNames.Free;
      end;
      if NPredFiles > 0 then
        begin
          AbsModelDir := PCurrent.AbsModelDirectory(muPred);
          SetCurrentDir(AbsModelDir);
          PlotSymbol := 1;
          // Get predictions from each package's observation input file.
          for I := 0 to NPredPkg - 1 do
            begin
              if PredFiles[I] <> '' then
                begin
                  psTemp.Clear;
                  case I of
                    // Order is that used internally by Modflow-2005.
                    0:
                      begin
                        GetMF2005HeadObs(PredFiles[I], psTemp, PlotSymbol,
                                        Iuobsv[I], HOBDRY, NumPred[I],
                                        JvProgressDialog1);
                        if NumPred[I] > 0 then
                          begin
                            PCurrent.UcProject.OmitVals.Add(HOBDRY)  ;
                          end;
                      end;
                    1: GetMF2005FlowObs(PredFiles[I], psTemp, PlotSymbol,
                                        GpName[I], Iuobsv[I], NumPred[I]);
                    2: GetMF2005FlowObs(PredFiles[I], psTemp, PlotSymbol,
                                        GpName[I], Iuobsv[I], NumPred[I]);
                    3: GetMF2005FlowObs(PredFiles[I], psTemp, PlotSymbol,
                                        GpName[I], Iuobsv[I], NumPred[I]);
                    4: GetMF2005FlowObs(PredFiles[I], psTemp, PlotSymbol,
                                        GpName[I], Iuobsv[I], NumPred[I]);
                  end;

                  if psTemp.Count > 0 then
                    begin
                      psModflow.Append(psTemp);
                    end;
                end;
            end;
          SetCurrentDir(OrigDir);
        end;
      PCurrent.PredSet.Assign(psModflow);
      PCurrent.DefinePredGroups;
      PCurrent.DefinePredSetup;
      PCurrent.UsePredGps := True;
      IAtt := PosDepCap('Group Name');
      PCurrent.PredictionSetup.PredAttributes.Items[IAtt].ControlMethod := cmByItem;
      PredictionSetupCurrent.Assign(PCurrent.PredictionSetup);
      PredGpsCurrent.Assign(PCurrent.PredGpSet);
      PredSetCurrent.Assign(PCurrent.PredSet);
      PopulatePredTabSheet(PCurrent);
      ProjChanged := True;
      AssignCurrent;
      PCurrent.MOFilesPred.Clear;
      if not BuildMF2005InstructFiles(PCurrent, Iuobsv, NumPred, muPred) then
        begin
          OK := False;
          Line := 'Error encountered trying to build instruction files.  ' +
                  'Correct problem and reimport observations.';
          ShowMessage(Line);
        end;
      if OK then
        begin
          // Open Predictions form so user can define attributes for prediction groups.
          ShowPredForm;
        end;
    end;
  finally
      psModflow.Free;
      psTemp.Free;
  end;
end;

procedure TFormMain.actImportObservation(Sender: TObject);
begin
  { TODO 3 -cObservations : Write code to import an Observation_Data input block }
  // Store data in PCurrent
end;

procedure TFormMain.actImportParameterDataBlockExecute(Sender: TObject);
begin
  { TODO 3 -cParameters : Write code to import a Parameter_Data input block }
  // Store data in PCurrent.  This may call a JupiterUnit procedure?
end;

procedure TFormMain.actImportUcodeMainFileExecute(Sender: TObject);
var
  Ifail: LongInt;
  NewProjectPath, OrigProjDir, UcodeFileDir, UcodeFilePath, Str: string;
  Iverb, LenStr1: LongInt;
  Deriv_Interface, UcFile, Path2MergedFile: string255;
  LinAdv, ModName, ModLenUnit, ModMassUnit, ModTimeUnit: string255;
  SosSurf, SosFil, StrtRes, IntRes, FinRes: string255;
  StrtSens, IntSens, FinSens: string255;
  CrInitFiles, DataEx, EigenVal, Lin: LongInt;
  WriteDerPars, WritePriInfo: LongInt;
  NonLinInt, Optim, Pred, Sens, StdErr1: LongInt;
  MaxIt,LQuasiNewton,IQNIter,IOmitDefault,LStats_On_Nonconverge,
        LOmitInsensitive,IConsecMax: LongInt;
  DTolPar,DTolSosc,DMaxChg,DMqrtDir,DMqrtFac,DMqrtInc,DQNSosr,DMinSenRat,
        DReincSenRat,DTolParWtos,DMaxStep: double;
  CMaxChgRealm, CTrustReg, TempCommand, TempPurpose: string255;
  NCommands: LongInt;
  NParGps, NPars, NObsGps, NObs, NPredGps, NPreds,
        NPriGps, NPri, NMIFiles, NMOFiles: LongInt;
  I, J: integer;
  IAutoStopRunners, IParallel, IVar, KCtrl, NDerPars, NumRunners : LongInt;
  TempIntVal, VerboseRunner: LongInt;
  ErrMsg, Keyword, ParamGpName, TempParName, TempStr: string255;
  TempValue, TimeoutFac, Wait, WaitRunners: double;
  OK: boolean;
  ReasonableRangeControlMethod, StatisticControlMethod: TControlMethod;
  Status0Text, StatusMsg: string;
  Incr: integer;
  Gap, DI, DN: double;
  OrigCursor: TCursor;
begin
  OK := true;
  TempCommand := ConvertString255(StringOfChar(' ',255));
  TempPurpose := ConvertString255(StringOfChar(' ',255));
  UcFile := ConvertString255(StringOfChar(' ',255));
  CMaxChgRealm := ConvertString255(StringOfChar(' ',255));
  CTrustReg := ConvertString255(StringOfChar(' ',255));
  Keyword := ConvertString255(StringOfChar(' ',255));
  ParamGpName := ConvertString255(StringOfChar(' ',255));
  { Import a UCODE main input file }
  // For each input block in main input file, call Execute procedure
  // for corresponding action.
  // Select a UCODE input file using an Open dialog.
  odUcode.FileName := '';
  odUcode.Title := 'Open UCODE main input file';
  odUcode.Filter := 'UCODE files (*.in) | *.in|All files (*.*)|*.*';
  OrigProjDir := ProjectDirectory;
  if odUcode.Execute then
    begin
      actFileNewExecute(self);
      UcodeFilePath := odUcode.FileName;
      UcodeFileDir := ExtractFileDir(UcodeFilePath);
      NewProjectPath := ChangeFileExt(UcodeFilePath, '.mtc');
      AssignProjectDirectory(NewProjectPath, OrigProjDir);
      UcFile := ConvertString255(UcodeFilePath);
      Str := FileBaseName(UcodeFilePath);
      PCurrent.ProjName := ConvertString255(Str);
      ProjFileTemp := Str + '.mtc';
      Invalidate;
      Status0Text := StatusBar1.Panels[0].Text;
      Repaint;
      // Set up progress bar
      JvProgressDialog1.InitValues(0,100,1,0,'Progress','Importing UCODE input file');
      JvProgressDialog1.ShowCancel := False;
      JvProgressDialog1.Smooth := True;
      JvProgressDialog1.Show;
    end
  else
    begin
      OK := False;
      StatusMsg := 'Import cancelled';
    end;
  OrigCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  LenStr1 := 255;
  Deriv_Interface := ConvertString255(StringOfChar(' ',255));
  try
    if OK then
      begin
        // Change current directory to directory where Ucode file resides
        SetCurrentDir(UcodeFileDir);
        // Initialize the accjupiter dll.
        aj_ini(UcFile, Ifail, LenStr1);
        if Ifail <> 0 then
          begin
            ShowMessage('Import of UCODE file failed on initialization');
            StatusMsg := 'Import failed';
            OK := False;
          end;
        JvProgressDialog1.Position := 2;
      end;
    // Invoke routines in DLL to read input blocks
    // and populate PCurrent.UcProject.
    //
    // Read Options input block.
    if OK then
      begin
        aj_bas_ini_getoptions(Iverb, Deriv_Interface, Path2MergedFile, IFail,
                              LenStr1, LenStr1);
        if IFail = 0 then
          begin
            PCurrent.UcProject.Verbose := Iverb;
            PCurrent.UcProject.DerivativesInterface := ConvertString(Deriv_Interface);
            PCurrent.UcProject.PathToMergedFile := ConvertString(Path2MergedFile);
            JvProgressDialog1.Position := 4;
          end
        else
          begin
            ShowMessage('Import of UCODE file failed on reading options');
            StatusMsg := 'Import failed';
            OK := False;
          end;
      end;
    //
    // Read UCODE_Control_Data input block.
    if OK  then
      begin
        self.Invalidate;
        aj_ini_ucode_control_data(Pred, Optim, Sens, Lin, NonLinInt,
             StdErr1, WriteDerPars, WritePriInfo, EigenVal, DataEx, CrInitFiles,
             LinAdv, ModName, ModLenUnit, ModMassUnit, ModTimeUnit,
             SosSurf, SosFil, StrtRes, IntRes, FinRes, StrtSens,
             IntSens, FinSens, IFail, ErrMsg, LenStr1,
             LenStr1, LenStr1, LenStr1, LenStr1,
             LenStr1, LenStr1, LenStr1, LenStr1,
             LenStr1, LenStr1, LenStr1, LenStr1,
             LenStr1);
        if IFail = 0 then
          begin
            PCurrent.UcProject.Optimize := LongInt2Bool(Optim);
            PCurrent.UcProject.Sensitivities := LongInt2Bool(Sens);
            PCurrent.UcProject.Linearity := LongInt2Bool(Lin);
            PCurrent.UcProject.Prediction := LongInt2Bool(Pred);
            PCurrent.UcProject.StdErrOne := LongInt2Bool(StdErr1);
            PCurrent.UcProject.WriteDerivedParams := LongInt2Bool(WriteDerPars);
            PCurrent.UcProject.WritePriorInfo := LongInt2Bool(WritePriInfo);
            PCurrent.UcProject.EigenValues := LongInt2Bool(EigenVal);
            PCurrent.UcProject.DataExchange := LongInt2Bool(DataEx);
            PCurrent.UcProject.CreateInitFiles := LongInt2Bool(CrInitFiles);
            PCurrent.UcProject.LinearityAdv := LinAdv;
            PCurrent.UcProject.NonLinearIntervals := LongInt2Bool(NonLinInt);
            PCurrent.UcProject.ModelName := ModName;
            PCurrent.UcProject.ModelLengthUnits := ModLenUnit;
            PCurrent.UcProject.ModelTimeUnits := ModTimeUnit;
            PCurrent.UcProject.ModelMassUnits := ModMassUnit;
            PCurrent.UcProject.SosSurface := SosSurf;
            PCurrent.UcProject.SosFile := SosFil;
            PCurrent.UcProject.StartRes := StrtRes;
            PCurrent.UcProject.IntermedRes := IntRes;
            PCurrent.UcProject.FinalRes := FinRes;
            PCurrent.UcProject.StartSens := ConvertString(StrtSens);
            PCurrent.UcProject.IntermedSens := ConvertString(IntSens);
            PCurrent.UcProject.FinalSens := ConvertString(FinSens);
            PCurrent.UcProject.UpdateMode();
            JvProgressDialog1.Position := 6;
          end
        else
          begin
            ShowMessage(ConvertString(ErrMsg));
            StatusMsg := 'Import failed';
            OK := False;
          end;
      end;
    //
    // Read Reg_GN_Controls input block.
    if OK then
      begin
        self.Invalidate;
        aj_ini_reg_gn_controls(MaxIt,LQuasiNewton,IQNIter,IOmitDefault,
              LStats_On_Nonconverge,LOmitInsensitive,IConsecMax,DTolPar,DTolSosc,DMaxChg,
              DMqrtDir,DMqrtFac,DMqrtInc,DQNSosr,DMinSenRat,DReincSenRat,
              DTolParWtos,DMaxStep,CMaxChgRealm,CTrustReg,IFail,LenStr1,LenStr1);
        if IFail = 0 then
          begin
            PCurrent.UcProject.Tolpar := DTolpar;
            PCurrent.UcProject.TolSOSC := DTolSosc;
            PCurrent.UcProject.MaxIter := MaxIt;
            PCurrent.UcProject.MaxChange := DMaxChg;
            PCurrent.UcProject.MaxChangeRealm := CMaxChgRealm;
            PCurrent.UcProject.MqrtDirection := DMqrtDir;
            PCurrent.UcProject.MqrtFactor := DMqrtFac;
            PCurrent.UcProject.MqrtIncrement := DMqrtInc;
            PCurrent.UcProject.QuasiNewton := LongInt2Bool(LQuasiNewton);
            PCurrent.UcProject.QNiter := IQNIter;
            PCurrent.UcProject.QNsosr := DQNSosr;
            // TODO 2 : IOmitDefault should be used to flag need for a fn.omit file.
            PCurrent.UcProject.OmitDefault:= IOmitDefault;
            PCurrent.UcProject.StatsOnNonconverge := LongInt2Bool(LStats_On_Nonconverge);
            PCurrent.UcProject.OmitInsensitive := LongInt2Bool(LOmitInsensitive);
            PCurrent.UcProject.MinimumSensRatio := DMinSenRat;
            PCurrent.UcProject.ReincludeSensRatio := DReincSenRat;
            PCurrent.UcProject.TolParWtOS := DTolParWtos;
            PCurrent.UcProject.TrustRegion := CTrustReg;
            PCurrent.UcProject.MaxStep := DMaxStep;
            PCurrent.UcProject.ConsecMax := IConsecMax;
            JvProgressDialog1.Position := 8;
          end
        else
          begin
            ShowMessage('Import of UCODE file failed on reading Reg_GN_Controls input block');
            StatusMsg := 'Import failed';
            OK := False;
          end;
      end;
    //
    // Read Model_Command_Lines input block and populate appropriate fields of TProject.
    NCommands := 0;
    if OK then
      begin
        self.Invalidate;
        aj_ini_model_command_lines(NCommands);
        if NCommands > 0 then
          begin
            TempPurpose := 'Forward';
            aj_get_model_command_by_purpose(TempPurpose, TempCommand, LenStr1, LenStr1);
            PCurrent.MCLForward := ConvertString(TempCommand);
            TempPurpose := 'Forward&Der';
            aj_get_model_command_by_purpose(TempPurpose, TempCommand, LenStr1, LenStr1);
            PCurrent.MCLFwdDeriv := ConvertString(TempCommand);
          end;
        JvProgressDialog1.Position := 10;
      end;
    //
    // Read Parameter_Groups input block.
    NParGps := 0;
    ReasonableRangeControlMethod := cmByDefault;
    if OK then
      begin
        self.Invalidate;
        aj_ini_parameter_groups(NParGps);
        if NParGps > 0 then
          begin
            ParamGpsCurrent.Clear;
            for I := 0 to NParGps - 1 do
              begin
                IVar := I;
                ParamGpsCurrent.Add;
                aj_get_parameter_group_name(IVar, ParamGpName, LenStr1);
                ParamGpsCurrent.Items[I].Initialize(ParamGpName, ParamGpName);
                // Doubles.
                Keyword := 'GPLOWERVALUE';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                TempStr := ConvertString255(FloatToStr(TempValue));
                ParamGpsCurrent.Items[I].SetAttributeByType(patLowerValue,ConvertString(TempStr));
                if TempStr <> '-1E39' then
                  begin
                    ReasonableRangeControlMethod := cmByGroup;
                  end;
                Keyword := 'GPUPPERVALUE';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                TempStr := ConvertString255(FloatToStr(TempValue));
                ParamGpsCurrent.Items[I].SetAttributeByType(patUpperValue,ConvertString(TempStr));
                if TempStr <> '1E39' then
                  begin
                    ReasonableRangeControlMethod := cmByGroup;
                  end;
                Keyword := 'GPLOWERCONSTRAINT';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                ParamGpsCurrent.Items[I].SetAttributeByType(patLowerBound,FloatToStr(TempValue));
                Keyword := 'GPUPPERCONSTRAINT';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                ParamGpsCurrent.Items[I].SetAttributeByType(patUpperBound,FloatToStr(TempValue));
                Keyword := 'GPPERTURBAMT';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                ParamGpsCurrent.Items[I].SetAttributeByType(patPerturbAmt,FloatToStr(TempValue));
                Keyword := 'GPTOLPAR';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                ParamGpsCurrent.Items[I].SetAttributeByType(patTolPar,FloatToStr(TempValue));
                Keyword := 'GPMAXCHANGE';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                ParamGpsCurrent.Items[I].SetAttributeByType(patMaxChange,FloatToStr(TempValue));
                Keyword := 'GPSCALEPVAL';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                ParamGpsCurrent.Items[I].SetAttributeByType(patScalePval,FloatToStr(TempValue));
                // Booleans.
                Keyword := 'GPCONSTRAIN';
                aj_get_logical_by_keyword(Keyword, IVar, TempIntVal, LenStr1);
                ParamGpsCurrent.Items[I].SetAttributeByType(patConstrain,BooleanToYesOrNo(LongInt2Bool(TempIntVal)));
                Keyword := 'GPADJUSTABLE';
                aj_get_logical_by_keyword(Keyword, IVar, TempIntVal, LenStr1);
                ParamGpsCurrent.Items[I].SetAttributeByType(patAdjustable,BooleanToYesOrNo(LongInt2Bool(TempIntVal)));
                Keyword := 'GPTRANSFORM';
                aj_get_logical_by_keyword(Keyword, IVar, TempIntVal, LenStr1);
                ParamGpsCurrent.Items[I].SetAttributeByType(patTransform,BooleanToYesOrNo(LongInt2Bool(TempIntVal)));
                Keyword := 'GPNONLINEARINTERVAL';
                aj_get_logical_by_keyword(Keyword, IVar, TempIntVal, LenStr1);
                ParamGpsCurrent.Items[I].SetAttributeByType(patNonLinearInterval,BooleanToYesOrNo(LongInt2Bool(TempIntVal)));
                // Integers.
                Keyword := 'GPSENMETHOD';
                aj_get_integer_by_keyword(Keyword, IVar, TempIntVal, LenStr1);
                ParamGpsCurrent.Items[I].SetAttributeByType(patSenMethod,IntToStr(TempIntVal));
                Keyword := 'GPSOSINCREMENT';
                aj_get_integer_by_keyword(Keyword, IVar, TempIntVal, LenStr1);
                ParamGpsCurrent.Items[I].SetAttributeByType(patSosIncrement,IntToStr(TempIntVal));
              end;
            PCurrent.ParGpSet.Assign(ParamGpsCurrent);
            PCurrent.ParameterSetup.SetControlMethod(patGroupName,cmByItem);
          end;
        JvProgressDialog1.Position := 12;
      end;
    //
    // Read Parameter_Data and Parameter_Values input blocks.
    NPars := 0;
    if OK then
      begin
        self.Invalidate;
        aj_ini_parameter_data(NPars);
        if NPars > 0 then
          begin
            ParamSetCurrent.Clear;
            for I := 0 to NPars - 1 do
              begin
                IVar := I;
                ParamSetCurrent.Add;
                // Strings.
                Keyword := 'PARAMNAME';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                ParamSetCurrent.Items[I].SetAttributeByType(patParamName,ConvertString(TempStr));
                Keyword := 'PARGROUPNAME';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                ParamSetCurrent.Items[I].SetAttributeByType(patGroupName,ConvertString(TempStr));
                // Doubles.
                Keyword := 'STARTVALUE';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                ParamSetCurrent.Items[I].SetAttributeByType(patStartValue,FloatToStr(TempValue));
                Keyword := 'LOWERVALUE';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                TempStr := ConvertString255(FloatToStr(TempValue));
                ParamSetCurrent.Items[I].SetAttributeByType(patLowerValue,ConvertString(TempStr));
                if TempStr <> '-1E39' then
                  begin
                    ReasonableRangeControlMethod := cmByItem;
                  end;
                Keyword := 'UPPERVALUE';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                TempStr := ConvertString255(FloatToStr(TempValue));
                ParamSetCurrent.Items[I].SetAttributeByType(patUpperValue,ConvertString(TempStr));
                if TempStr <> '1E39' then
                  begin
                    ReasonableRangeControlMethod := cmByItem;
                  end;
                Keyword := 'LOWERCONSTRAINT';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                ParamSetCurrent.Items[I].SetAttributeByType(patLowerBound,FloatToStr(TempValue));
                Keyword := 'UPPERCONSTRAINT';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                ParamSetCurrent.Items[I].SetAttributeByType(patUpperBound,FloatToStr(TempValue));
                Keyword := 'PERTURBAMT';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                ParamSetCurrent.Items[I].SetAttributeByType(patPerturbAmt,FloatToStr(TempValue));
                Keyword := 'TOLPAR';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                ParamSetCurrent.Items[I].SetAttributeByType(patTolPar,FloatToStr(TempValue));
                Keyword := 'MAXCHANGE';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                ParamSetCurrent.Items[I].SetAttributeByType(patMaxChange,FloatToStr(TempValue));
                Keyword := 'SCALEPVAL';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                ParamSetCurrent.Items[I].SetAttributeByType(patScalePval,FloatToStr(TempValue));
                // Booleans.
                Keyword := 'CONSTRAIN';
                aj_get_logical_by_keyword(Keyword, IVar, TempIntVal, LenStr1);
                ParamSetCurrent.Items[I].SetAttributeByType(patConstrain,BooleanToYesOrNo(LongInt2Bool(TempIntVal)));
                Keyword := 'ADJUSTABLE';
                aj_get_logical_by_keyword(Keyword, IVar, TempIntVal, LenStr1);
                ParamSetCurrent.Items[I].SetAttributeByType(patAdjustable,BooleanToYesOrNo(LongInt2Bool(TempIntVal)));
                Keyword := 'TRANSFORM';
                aj_get_logical_by_keyword(Keyword, IVar, TempIntVal, LenStr1);
                ParamSetCurrent.Items[I].SetAttributeByType(patTransform,BooleanToYesOrNo(LongInt2Bool(TempIntVal)));
                Keyword := 'NONLINEARINTERVAL';
                aj_get_logical_by_keyword(Keyword, IVar, TempIntVal, LenStr1);
                ParamSetCurrent.Items[I].SetAttributeByType(patNonLinearInterval,BooleanToYesOrNo(LongInt2Bool(TempIntVal)));
                // Integers.
                Keyword := 'SENMETHOD';
                aj_get_integer_by_keyword(Keyword, IVar, TempIntVal, LenStr1);
                ParamSetCurrent.Items[I].SetAttributeByType(patSenMethod,IntToStr(TempIntVal));
                Keyword := 'SOSINCREMENT';
                aj_get_integer_by_keyword(Keyword, IVar, TempIntVal, LenStr1);
                ParamSetCurrent.Items[I].SetAttributeByType(patSosIncrement,IntToStr(TempIntVal));
              end;
            ParameterSetupCurrent.SetControlMethod(patReasRange, ReasonableRangeControlMethod);
            ParameterSetupCurrent.SetCombinedMethods;
            PCurrent.ParameterSetup.Assign(ParameterSetupCurrent);
            PCurrent.ParamSet.Assign(ParamSetCurrent);
          end;
        JvProgressDialog1.Position := 14;
      end;
    //
    // Read Derived_Parameters input block.
    NDerPars := 0;
    if OK then
      begin
        self.Invalidate;
        aj_ini_derived_parameters(NDerPars);
        if NDerPars > 0 then
          begin
            for I := 0 to NDerPars - 1 do
              begin
                IVar := I;
                J := ParamSetCurrent.Count;
                ParamSetCurrent.Add;
                Keyword := 'DERPARNAME';
                aj_get_character_by_keyword(Keyword, IVar, TempParName, LenStr1, LenStr1);
                ParamSetCurrent.Items[J].SetAttributeByType(patParamName,ConvertString(TempParName));
                Keyword := 'DERPAREQN';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                ParamSetCurrent.Items[J].DerivEqn := ConvertString(TempStr);
                ParamSetCurrent.Items[J].SetAttributeByType(patDerived,'Yes');
                ParamSetCurrent.Items[J].SetAttributeByType(patStartValue,'0.0');
              end;
            PCurrent.ParamSet.Assign(ParamSetCurrent);
          end;
        JvProgressDialog1.Position := 16;
      end;
    //
    // Read Observation_Groups input block.
    NObsGps := 0;
    if OK then
      begin
        self.Invalidate;
        aj_ini_observation_groups(NObsGps);
        if NObsGps > 0 then
          begin
            ObsGpsCurrent.Clear;
            for I := 0 to NObsGps - 1 do
              begin
                IVar := I;
                ObsGpsCurrent.Add;
                // Strings.
                Keyword := 'OBSGROUPNAME';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                ObsGpsCurrent.Items[I].SetAttributeByType(datGroupName,ConvertString(TempStr));
                Keyword := 'GPSTATFLAG';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                ObsGpsCurrent.Items[I].SetAttributeByType(datStatFlag,ConvertString(TempStr));
                Keyword := 'COVMATRIX';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                ObsGpsCurrent.Items[I].SetAttributeByType(datCovMatrix,ConvertString(TempStr));
                // Doubles.
                Keyword := 'WTMULTIPLIER';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                ObsGpsCurrent.Items[I].SetAttributeByType(datWtMultiplier,FloatToStr(TempValue));
                Keyword := 'GPSTATISTIC';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                ObsGpsCurrent.Items[I].SetAttributeByType(datStatistic,FloatToStr(TempValue));
                Keyword := 'GPWTOSCONSTANT';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                ObsGpsCurrent.Items[I].SetAttributeByType(datWtOSConstant,FloatToStr(TempValue));
                // Integers.
                Keyword := 'PLOTSYMBOL';
                aj_get_integer_by_keyword(Keyword, IVar, TempIntVal, LenStr1);
                ObsGpsCurrent.Items[I].SetAttributeByType(datPlotSymbol,IntToStr(TempIntVal));
                // Booleans.
                Keyword := 'USEFLAG';
                aj_get_logical_by_keyword(Keyword, IVar, TempIntVal, LenStr1);
                ObsGpsCurrent.Items[I].SetAttributeByType(datUseFlag,BooleanToYesOrNo(LongInt2Bool(TempIntVal)));
                Keyword := 'GPNONDETECT';
                aj_get_logical_by_keyword(Keyword, IVar, TempIntVal, LenStr1);
                ObsGpsCurrent.Items[I].SetAttributeByType(datNonDetect,BooleanToYesOrNo(LongInt2Bool(TempIntVal)));
              end;
            PCurrent.ObsGpSet.Assign(ObsGpsCurrent);
            PCurrent.UseObsGps := True;
            ObservationSetupCurrent.SetControlMethod(datGroupName,cmByItem);
          end;
        JvProgressDialog1.Position := 18;
        Repaint;
      end;
    //
    // Read Observation_Data input block.
    NObs := 0;
    if OK then
      begin
       // self.Invalidate;
        aj_ini_observation_data(NObs);
        if NObs > 0 then
          begin
            ObsSetCurrent.Clear;
            StatisticControlMethod := cmByGroup;
            for I := 0 to NObs - 1 do
              begin
                IVar := I;
                ObsSetCurrent.Add;
                // Strings.
                Keyword := 'OBSNAME';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                ObsSetCurrent.Items[I].SetAttributeByType(datObsName,ConvertString(TempStr));
                Keyword := 'STATFLAG';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                ObsSetCurrent.Items[I].SetAttributeByType(datStatFlag,ConvertString(TempStr));
                if TempStr = 'UNKNOWN' then
                  begin
                    ObservationSetupCurrent.SetControlMethod(datStatFlag,cmByGroup);
                  end;
                Keyword := 'GROUPNOBS';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                ObsSetCurrent.Items[I].SetAttributeByType(datGroupName,ConvertString(TempStr));
                Keyword := 'DEROBSEQN';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                ObsSetCurrent.Items[I].SetAttributeByType(datEquation,ConvertString(TempStr));
                // Doubles.
                Keyword := 'OBSVALUE';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                ObsSetCurrent.Items[I].SetAttributeByType(datObsValue,FloatToStr(TempValue));
                Keyword := 'STATISTIC';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                TempStr := ConvertString255(FloatToStr(TempValue));
                ObsSetCurrent.Items[I].SetAttributeByType(datStatistic,ConvertString(TempStr));
                if TempStr <> '1E39' then
                  begin
                    StatisticControlMethod := cmByItem;
                  end;
                Keyword := 'WTOSCONSTANT';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                ObsSetCurrent.Items[I].SetAttributeByType(datWtOSConstant,FloatToStr(TempValue));
                // Integers.
                // Booleans.
                Keyword := 'NONDETECT';
                aj_get_logical_by_keyword(Keyword, IVar, TempIntVal, LenStr1);
                ObsSetCurrent.Items[I].SetAttributeByType(datNonDetect,BooleanToYesOrNo(LongInt2Bool(TempIntVal)));
                DI := I + 1;
                DN := NObs;
                Gap := 68.0;
                Incr := Trunc(Gap * ( DI / DN ));
                JvProgressDialog1.Position := 18 + Incr;
              end;
            ObservationSetupCurrent.SetControlMethod(datStatistic,StatisticControlMethod);
            PCurrent.ObservationSetup.Assign(ObservationSetupCurrent);
            PCurrent.ObsSet.Assign(ObsSetCurrent);
            Invalidate;
            Repaint;
          end;
      end;
    //
    // TODO 3 : Read Derived_Observations input block (later).
    //
    // Read Prediction_Groups input block.
    NPredGps := 0;
    if OK then
      begin
        aj_ini_prediction_groups(NPredGps);
        if NPredGps > 0 then
          begin
            PredGpsCurrent.Clear;
            for I := 0 to NPredGps - 1 do
              begin
                IVar := I;
                PredGpsCurrent.Add;
                // Strings.
                Keyword := 'PREDGROUPNAME';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                PredGpsCurrent.Items[I].SetAttributeByType(datGroupName,ConvertString(TempStr));
                Keyword := 'GPMEASSTATFLAG';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                PredGpsCurrent.Items[I].SetAttributeByType(datMeasStatFlag,ConvertString(TempStr));
                // Doubles.
                Keyword := 'GPMEASSTATISTIC';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                PredGpsCurrent.Items[I].SetAttributeByType(datMeasStatistic,FloatToStr(TempValue));
                // Integers.
                Keyword := 'PREDPLOTSYMBOL';
                aj_get_integer_by_keyword(Keyword, IVar, TempIntVal, LenStr1);
                PredGpsCurrent.Items[I].SetAttributeByType(datPlotSymbol,IntToStr(TempIntVal));
                // Booleans.
                Keyword := 'PREDUSEFLAG';
                aj_get_logical_by_keyword(Keyword, IVar, TempIntVal, LenStr1);
                PredGpsCurrent.Items[I].SetAttributeByType(datUseFlag,BooleanToYesOrNo(LongInt2Bool(TempIntVal)));
              end;
            PCurrent.PredGpSet.Assign(PredGpsCurrent);
            PCurrent.UsePredGps := True;
            PCurrent.PredictionSetup.SetControlMethod(datGroupName,cmByItem);
          end;
        JvProgressDialog1.Position := 88;
        Repaint;
      end;
    //
    // Read Prediction_Data input block.
    NPreds := 0;
    if OK then
      begin
        aj_ini_prediction_data(NPreds);
        if NPreds > 0 then
          begin
            PredSetCurrent.Clear;
            StatisticControlMethod := cmByGroup;
            for I := 0 to NPreds - 1 do
              begin
                IVar := I;
                PredSetCurrent.Add;
                // Strings.
                Keyword := 'PREDNAME';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                PredSetCurrent.Items[I].SetAttributeByType(datPredName,ConvertString(TempStr));
                Keyword := 'MEASSTATFLAG';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                PredSetCurrent.Items[I].SetAttributeByType(datMeasStatFlag,ConvertString(TempStr));
                if TempStr = 'UNKNOWN' then
                  begin
                    PredictionSetupCurrent.SetControlMethod(datMeasStatFlag,cmByGroup);
                  end;
                Keyword := 'GROUPNPRED';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                PredSetCurrent.Items[I].SetAttributeByType(datGroupName,ConvertString(TempStr));
                Keyword := 'DERPREDEQN';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                PredSetCurrent.Items[I].SetAttributeByType(datEquation,ConvertString(TempStr));
                // Doubles.
                Keyword := 'REFVALUE';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                PredSetCurrent.Items[I].SetAttributeByType(datRefValue,FloatToStr(TempValue));
                Keyword := 'MEASSTATISTIC';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                TempStr := ConvertString255(FloatToStr(TempValue));
                PredSetCurrent.Items[I].SetAttributeByType(datMeasStatistic,ConvertString(TempStr));
                if TempStr <> '1E39' then
                  begin
                    StatisticControlMethod := cmByItem;
                  end;
                // Integers.
                // Booleans.
              end;
            PredictionSetupCurrent.SetControlMethod(datMeasStatistic,StatisticControlMethod);
            PCurrent.PredictionSetup.Assign(PredictionSetupCurrent);
            PCurrent.PredSet.Assign(PredSetCurrent);
          end;
        JvProgressDialog1.Position := 90;
        Repaint;
      end;
    //
    // TODO 3 : Read Derived_Predictions input block (later).
    //
    // Read Prior_Information_Groups input block.
    NPriGps := 0;
    if OK then
      begin
        aj_ini_prior_information_groups(NPriGps);
        if NPriGps > 0 then
          begin
            PriGpsCurrent.Clear;
            for I := 0 to NPriGps - 1 do
              begin
                IVar := I;
                PriGpsCurrent.Add;
                // Strings.
                Keyword := 'PRIGROUPNAME';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                PriGpsCurrent.Items[I].SetAttributeByType(piatGroupName,ConvertString(TempStr));
                Keyword := 'GPPRISTATFLAG';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                PriGpsCurrent.Items[I].SetAttributeByType(piatStatFlag,ConvertString(TempStr));
                Keyword := 'PRICOVMATRIX';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                PriGpsCurrent.Items[I].SetAttributeByType(piatCovMatrix,ConvertString(TempStr));
                Keyword := 'GPPRIEQN';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                PriGpsCurrent.Items[I].SetAttributeByType(piatEquation,ConvertString(TempStr));
                // Doubles.
                Keyword := 'PRIWTMULTIPLIER';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                PriGpsCurrent.Items[I].SetAttributeByType(piatWtMultiplier,FloatToStr(TempValue));
                Keyword := 'GPPRIORINFOVALUE';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                PriGpsCurrent.Items[I].SetAttributeByType(piatPriValue,FloatToStr(TempValue));
                Keyword := 'GPPRISTATISTIC';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                PriGpsCurrent.Items[I].SetAttributeByType(piatStatistic,FloatToStr(TempValue));
                // Integers.
                Keyword := 'PRIPLOTSYMBOL';
                aj_get_integer_by_keyword(Keyword, IVar, TempIntVal, LenStr1);
                PriGpsCurrent.Items[I].SetAttributeByType(piatPlotSymbol,IntToStr(TempIntVal));
                // Booleans.
                Keyword := 'PRIUSEFLAG';
                aj_get_logical_by_keyword(Keyword, IVar, TempIntVal, LenStr1);
                PriGpsCurrent.Items[I].SetAttributeByType(piatUseFlag,BooleanToYesOrNo(LongInt2Bool(TempIntVal)));
              end;
            PCurrent.PriGpSet.Assign(PriGpsCurrent);
            PCurrent.UsePriGps := True;
            PCurrent.PriorSetup.SetControlMethod(piatGroupName,cmByItem) ;
          end;
        JvProgressDialog1.Position := 92;
        Repaint;
      end;
    //
    // Read Linear_Prior_Information input block.
    NPri := 0;
    if OK then
      begin
        aj_ini_linear_prior_information(NPri);
        if NPri > 0 then
          begin
            PriSetCurrent.Clear;
            StatisticControlMethod := cmByGroup;
            for I := 0 to NPri - 1 do
              begin
                IVar := I;
                PriSetCurrent.Add;
                // Strings.
                Keyword := 'PRIORNAME';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                PriSetCurrent.Items[I].SetAttributeByType(piatPriorName,ConvertString(TempStr));
                Keyword := 'PRISTATFLAG';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                PriSetCurrent.Items[I].SetAttributeByType(piatStatFlag,ConvertString(TempStr));
                if TempStr = 'UNKNOWN' then
                  begin
                    PriorSetupCurrent.SetControlMethod(piatStatFlag,cmByGroup);
                  end;
                Keyword := 'PRIEQN';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                PriSetCurrent.Items[I].SetAttributeByType(piatEquation,ConvertString(TempStr));
                Keyword := 'GROUPNPRI';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                PriSetCurrent.Items[I].SetAttributeByType(piatGroupName,ConvertString(TempStr));
                // Doubles.
                Keyword := 'PRIORINFOVALUE';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                PriSetCurrent.Items[I].SetAttributeByType(piatPriValue,FloatToStr(TempValue));
                Keyword := 'PRISTATISTIC';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                TempStr := ConvertString255(FloatToStr(TempValue));
                PriSetCurrent.Items[I].SetAttributeByType(piatStatistic,ConvertString(TempStr));
                if TempStr <> '1E39' then
                  begin
                    StatisticControlMethod := cmByItem;
                  end;
                // Integers.
                // Booleans.
              end;
            PriorSetupCurrent.SetControlMethod(piatStatistic,StatisticControlMethod);
            PCurrent.PriorSetup.Assign(PriorSetupCurrent);
            PCurrent.PriSet.Assign(PriSetCurrent);
          end;
        JvProgressDialog1.Position := 94;
        Repaint;
      end;
    //
    // TODO 3 : Read Matrix_Files input block (later).
    //
    // Read Model_Input_Files input block.
    NMIFiles := 0;
    if OK then
      begin
        self.Invalidate;
        aj_ini_model_input_files(NMIFiles);
        if NMIFiles > 0 then
          begin
            PCurrent.MIFiles.Clear;
            for I := 0 to NMIFiles - 1 do
              begin
                IVar := I;
                PCurrent.MIFiles.Add;
                // Strings.
                Keyword := 'MODINFILE';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                PCurrent.MIFiles.Items[I].ModelFile := ConvertString(TempStr);
                Keyword := 'TEMPLATEFILE';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                PCurrent.MIFiles.Items[I].AppFile := ConvertString(TempStr);
              end;
          end;
        JvProgressDialog1.Position := 96;
        Repaint;
      end;
    //
    // Read Model_Output_Files input block.
    NMOFiles := 0;
    if OK then
      begin
        self.Invalidate;
        aj_ini_model_output_files(NMOFiles);
        if NMOFiles > 0 then
          begin
            PCurrent.MOFiles.Clear;
            for I := 0 to NMOFiles - 1 do
              begin
                IVar := I;
                PCurrent.MOFiles.Add;
                // Strings.
                Keyword := 'MODOUTFILE';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                PCurrent.MOFiles.Items[I].ModelFile := ConvertString(TempStr);
                Keyword := 'INSTRUCTIONFILE';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                PCurrent.MOFiles.Items[I].AppFile := ConvertString(TempStr);
              end;
          end;
        JvProgressDialog1.Position := 98;
        Repaint;
      end;
    //
    // Read Parallel_Control and Parallel_Runners input blocks.
    if OK then
      begin
        self.Invalidate;
        aj_ini_parallel(KCtrl, NumRunners, IParallel, Wait, WaitRunners,
                        VerboseRunner, IAutoStopRunners, TimeoutFac);
        if KCtrl > 0 then
          begin
            PCurrent.UcProject.ParallelControl.Parallel := LongInt2Bool(IParallel);
            PCurrent.UcProject.ParallelControl.Wait := Wait;
            PCurrent.UcProject.ParallelControl.WaitRunners := WaitRunners;
            PCurrent.UcProject.ParallelControl.VerboseRunner := VerboseRunner;
            PCurrent.UcProject.ParallelControl.AutoStopRunners := LongInt2Bool(IAutoStopRunners);
            PCurrent.UcProject.ParallelControl.TimeOutFactor := TimeoutFac;
          end;
        if NumRunners > 0 then
          begin
            PCurrent.UcProject.ParallelRunners.Clear;
            PCurrent.UcProject.ParallelControl.NumRunnersToUse := 0;
            for I := 0 to NumRunners - 1 do
              begin
                IVar := I;
                PCurrent.UcProject.ParallelRunners.Add;
                // Strings.
                Keyword := 'RUNNERNAME';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                PCurrent.UcProject.ParallelRunners.Items[I].Name := ConvertString(TempStr);
                Keyword := 'RUNNERDIR';
                aj_get_character_by_keyword(Keyword, IVar, TempStr, LenStr1, LenStr1);
                PCurrent.UcProject.ParallelRunners.Items[I].Directory := ConvertString(TempStr);
                Keyword := 'RUNTIME';
                aj_get_double_by_keyword(Keyword, IVar, TempValue, LenStr1);
                PCurrent.UcProject.ParallelRunners.Items[I].ExpectedRunTime := TempValue;
                PCurrent.UcProject.ParallelRunners.Items[I].Use := True;
                PCurrent.UcProject.ParallelControl.NumRunnersToUse :=
                     PCurrent.UcProject.ParallelControl.NumRunnersToUse + 1;
              end;
          end;
        JvProgressDialog1.Position := 100;
        Repaint;
      end;
    //
    // Deallocate memory in DLL.
    aj_cln();
        // TODO 2 : When IOmitDefault > 0, read fn.omit and use contents to populate
        // box at bottom of Parameter Estimation Settings form.
    // Populate PCurrent fields from data in DLL.
    //  PopulateControls(PCurrent);
    TempUcProject.Assign(PCurrent.UcProject);
  except
    OK := False;
    ErrMsg := 'Exception occurred during import.';
    ShowMessage(ConvertString(ErrMsg));
    StatusMsg := ConvertString(ErrMsg);
  end;
  // Return Status bar to text state.
  StatusBar1.Panels[0].Alignment := taLeftJustify;
  StatusBar1.Panels[0].Text := Status0Text;
  if OK then
    begin
      StatusBar1.Panels[1].Text := 'Import completed';
    end
  else
    begin
      StatusBar1.Panels[1].Text := StatusMsg;
    end;
  JvProgressDialog1.Hide;
  //
  ProjChanged := True;
  UpdateMainForm;
  Screen.Cursor := OrigCursor;
end;

procedure TFormMain.actImport_dmExecute(Sender: TObject);
// Import Model Information from _dm file.
begin
  PCurrent.UcProject.GetModelInfo(UcModelInfo);
end;

procedure TFormMain.actImport_pasubExecute(Sender: TObject);
// Import estimated parameters by iteration from _pasub file.
begin
//
end;

procedure TFormMain.actImport_paoptExecute(Sender: TObject);
// Import optimized parameter values from a _paopt file.
var
  OK: boolean;
  psTemp: TParamSet;
  fnPaopt: TFileName;
  Dir, DirSep, Msg, StrTemp: string;
  MR: integer;
  AttPos, I: Integer;
begin
  AttPos := ParAttPos(patStartValue);
  DirSep := PathDelimiter;
  Dir := ExtractFileDir(PCurrent.UcProject.AbsMainInputFileName(muCalib));
  StrTemp := Dir + DirSep + PCurrent.UcProject.OutputPrefix;
  fnPaopt := ChangeFileExt(StrTemp,'._paopt');
  OK := False;
  if FileExists(fnPaopt) then
    begin
      Msg := 'Replace current parameter values with optimized values in file '
             + fnPaopt + '?  Click No to choose another file.';
      MR := MessageDlg(Msg,mtConfirmation,[mbYes,mbNo,mbCancel],0,mbYes);
      case MR of
        mrYes: OK := True;
        mrNo:
          begin
            OK := SelectPaoptFile(fnPaopt);
            if not FileExists(fnPaopt) then
              begin
                Msg := 'File ' + fnPaopt + ' does not exist';
                ShowMessage(Msg);
                OK := False;
              end;
          end;
        mrCancel: OK := False;
      end;
    end
  else
    begin
      Msg := 'File ' + fnPaopt + ' does not exist';
      ShowMessage(Msg);
      OK := SelectPaoptFile(fnPaopt);
      if not FileExists(fnPaopt) then
        begin
          Msg := 'File ' + fnPaopt + ' does not exist';
          ShowMessage(Msg);
          OK := False;
        end;
    end;
  if OK then
    begin
      psTemp := TParamSet.Create;
      try
        if GetOptParamSet(psTemp,fnPaopt) then
          begin
            // Check # and names of parameters: if OK, replace all parameter values.
            if psTemp.Count = ParamSetCurrent.Count then
              begin
                I := 0;
                while (I < psTemp.Count) and OK do
                  begin
                    OK := AnsiSameText(ConvertString(psTemp.Items[I].Name),
                                ConvertString(ParamSetCurrent.Items[I].Name));
                    I := I + 1;
                  end;
              end
            else
              begin
                OK := False;
              end;
            if OK then
              begin
                for I := 0 to psTemp.Count - 1 do
                  begin
                    ParamSetCurrent.Items[I].AllAtts.Items[AttPos].Text :=
                      psTemp.Items[I].AllAtts.Items[AttPos].Text;
                  end;
                rbwdgParamsPopulate(ParamSetCurrent);
                ProjChanged := True;
                EnableSaveRevert;
              end
            else
              begin
                Msg := 'Error: Number or names of parameters do not match';
                ShowMessage(Msg);
              end;
          end;
      finally
        psTemp.Free;
      end;
    end;
end;

procedure TFormMain.actInvokeGW_ChartExecute(Sender: TObject);
var
  AbsDir, Cmd, FName, Line, RelPath: string;
  ModelUseLocal: TModelUse;
  UcModeTemp: TUcodeMode;
  FoundFile: boolean;
begin
  FoundFile := False;
  if FileExists(GlobalProgramLocations.GWChartLocation) then
    begin
      UcModeTemp := PCurrent.UcProject.UcMode;
      case UcModeTemp of
        umPred:
          begin
            AbsDir := PCurrent.AbsAppDirectory(muPred);
            RelPath := ChangeFileExt(PCurrent.UcProject.OutputMainFile, '._sppp');
            FName := PathToAbsPath(AbsDir,RelPath);
            if FileExists(FName) then FoundFile := True;
            ModelUseLocal := muPred;
          end;
        else
          ModelUseLocal := muCalib;
      end;
      if not FoundFile then
        begin
          AbsDir := PCurrent.AbsAppDirectory(ModelUseLocal);
          PCurrent.UcProject.UcMode := umFwd;
          RelPath := ChangeFileExt(PCurrent.UcProject.OutputMainFile, '._os');
          PCurrent.UcProject.UcMode := UcModeTemp;
          FName := PathToAbsPath(AbsDir,RelPath);
        end;
      if FileExists(FName) then
        begin
          if DirectoryExists(AbsDir) then
            begin
              // Invoke GW_Chart here.
              SetCurrentDir(AbsDir);
              Cmd := BuildCommand(GlobalProgramLocations.GWChartLocation, FName, False);
              ExecuteCommand(Cmd, False);
            end;
        end
      else
        begin
          Line := 'File not found: ' + FName;
          ShowMessage(Line);
        end;
    end
  else
    begin
      Line := 'Location of GW_Chart executable is invalid';
      ShowMessage(Line);
      OpenProgramLocations;
    end;
end;

procedure TFormMain.actModelDirExecute(Sender: TObject);
begin
  FormModelDir.ShowModal;
  EnableSaveRevert;
end;

procedure TFormMain.actModelInputExecute(Sender: TObject);
begin
  ModelIOFormUse := fuInput;
  FormModelIO.ModelUse := muCalib;
  FormModelIO.ShowModal;
  if ProjChanged then
    begin
      NeedPvalTemplate := True;
      AssignCurrent;
    end;
end;

procedure TFormMain.actModelInputPredExecute(Sender: TObject);
begin
  ModelIOFormUse := fuInput;
  FormModelIO.ModelUse := muPred;
  FormModelIO.ShowModal;
  if ProjChanged then AssignCurrent;
end;

procedure TFormMain.actModelOutputExecute(Sender: TObject);
begin
  ModelIOFormUse := fuOutput;
  FormModelIO.ModelUse := muCalib;
  FormModelIO.ShowModal;
  if ProjChanged then AssignCurrent;
end;

procedure TFormMain.actModelOutputPredExecute(Sender: TObject);
begin
  ModelIOFormUse := fuOutput;
  FormModelIO.ModelUse := muPred;
  FormModelIO.ShowModal;
  if ProjChanged then AssignCurrent;
end;

procedure TFormMain.actConfigParamTablesExecute(Sender: TObject);
var
  UseGps: boolean;
  I, ModRes: integer;
begin
  FormConfigureParTables.FreezeNames := FreezeParamNames;
  ModRes := FormConfigureParTables.ShowModal();
  if ModRes = mrOK then
    begin
      FreezeParamNames := FormConfigureParTables.FreezeNames;
    end;
  rbwdgParamGpsPopulate(ParamGpsCurrent);
  rbwdgParamsPopulate(ParamSetCurrent);
  // Determine if parameter groups are needed
  UseGps := False;
  for I := 0 to NumParAttributes - 1 do
    begin
      if ParameterSetupCurrent.ParAttributes.Items[I].ControlMethod
             = cmByGroup then
        UseGps := True;
    end;
  PCurrent.UseParGps := UseGps;
  if ProjChanged then AssignCurrent;
end;

procedure TFormMain.actOpenEditorExecute(Sender: TObject);
begin
//  FormEditor.Show;
end;

procedure TFormMain.actParallelRunnersExecute(Sender: TObject);
var
  ModRes: integer;
begin
  ParallelRunnersGlobal.Assign(PCurrent.UcProject.ParallelRunners);
  ModRes := FormParallelRunners.ShowModal;
  if ModRes = mrOK then
    begin
      if not PCurrent.UcProject.ParallelRunners.SameAs(ParallelRunnersGlobal) then
        begin
          PCurrent.UcProject.ParallelRunners.Assign(ParallelRunnersGlobal);
          ProjChanged := True;
          AssignCurrent;
        end;
    end;
end;

procedure TFormMain.actParallelControlExecute(Sender: TObject);
var
  ModRes: integer;
begin
  ParallelControlGlobal.Assign(PCurrent.UcProject.ParallelControl);
  FormParallelControl.NumUsableRunners := PCurrent.UcProject.ParallelRunners.NumUsable;
  ParallelRunnersGlobal.Assign(PCurrent.UcProject.ParallelRunners);
  ModRes := FormParallelControl.ShowModal;
  if ModRes = mrOK then
    begin
      if not PCurrent.UcProject.ParallelControl.SameAs(ParallelControlGlobal) then
        begin
          PCurrent.UcProject.ParallelControl.Assign(ParallelControlGlobal);
          ProjChanged := True;
        end;
    end;
  if not PCurrent.UcProject.ParallelRunners.SameAs(ParallelRunnersGlobal) then
    begin
      PCurrent.UcProject.ParallelRunners.Assign(ParallelRunnersGlobal);
      ProjChanged := True;
    end;
  if ProjChanged then
    AssignCurrent;
end;

procedure TFormMain.actProjectNameExecute(Sender: TObject);
begin
  frmProjName := TfrmProjName.Create(Self);
  frmProjName.ShowModal;
  if ProjChanged then
    begin
     PopulateControls(PCurrent);
    end;
end;

procedure TFormMain.actRefreshExecute(Sender: TObject);
begin
  PopulateControls(PCurrent);
  Invalidate;
  Repaint;
end;

procedure TFormMain.actResidualAnalysisAdvExecute(Sender: TObject);
// Invoke residual_analysis_adv.
var
  AbsDir, cmd, DirSep, Line: string;
  FName: TFileName;
begin
  DirSep := PathDelimiter;
  // Residual_analysis_adv requires command line of form:
  //  path\residual_analysis_adv fn, where fn is filename prefix used for UCODE run.
  if FileExists(GlobalProgramLocations.ResidAnalysisAdvLocation) then
    begin
      AbsDir := PCurrent.AbsAppDirectory(muCalib);
      SetCurrentDir(AbsDir);
      Cmd := BuildCommand(GlobalProgramLocations.ResidAnalysisAdvLocation,
                          PCurrent.UcProject.OutputPrefix, False);
      // Invoke Residual_analysis_adv here.
      ExecuteCommand(Cmd, True);
      FName := PCurrent.AbsAppDirectory(muCalib) + DirSep +
               PCurrent.UcProject.OutputPrefix + '.#resanadv';
      ViewFile(FName);
    end
  else
    begin
      Line := 'Location of Residual_analysis_adv executable is invalid';
      ShowMessage(Line);
      OpenProgramLocations;
    end;

end;

procedure TFormMain.actResidualAnalysisExecute(Sender: TObject);
// Invoke residual_analysis.
var
  AbsDir, cmd, DirSep, Line: string;
  FName: TFileName;
//  ModelUseLocal: TModelUse;
begin
  DirSep := PathDelimiter;
  // Residual_analysis requires command line of form:
  //  path\residual_analysis fn, where fn is filename prefix used for UCODE run.
  // TODO 3 : Revise to open form to define non-default input for residual_analysis.
  //  This will require creating a .rs file.
  if FileExists(GlobalProgramLocations.ResidAnalysisLocation) then
    begin
      AbsDir := PCurrent.AbsAppDirectory(muCalib);
      SetCurrentDir(AbsDir);
      Cmd := BuildCommand(GlobalProgramLocations.ResidAnalysisLocation,
                          PCurrent.UcProject.OutputPrefix, False);
      // Invoke Residual_analysis here.
      ExecuteCommand(Cmd, True);
      FName := PCurrent.AbsAppDirectory(muCalib) + DirSep +
               PCurrent.UcProject.OutputPrefix + '.#resan';
      ViewFile(FName);
    end
  else
    begin
      Line := 'Location of Residual_analysis executable is invalid';
      ShowMessage(Line);
      OpenProgramLocations;
    end;
end;

procedure TFormMain.actRevertProjectExecute(Sender: TObject);
begin
    // Revert ALL project settings to those last saved.
  actCloseChildWinExecute(Self);
  PCurrent.Assign(PLastSaved);
  ParameterSetupCurrent.Assign(PLastSaved.ParameterSetup);
  ParamSetCurrent.Assign(PLastSaved.ParamSet);
  ParamGpsCurrent.Assign(PLastSaved.ParGpSet);
  ObsSetCurrent.Assign(PLastSaved.ObsSet);
  ObsGpsCurrent.Assign(PLastSaved.ObsGpSet);
  ObservationSetupCurrent.Assign(PLastSaved.ObservationSetup);
  PredSetCurrent.Assign(PLastSaved.PredSet);
  PredGpsCurrent.Assign(PLastSaved.PredGpSet);
  PredictionSetupCurrent.Assign(PLastSaved.PredictionSetup);
  PriSetCurrent.Assign(PLastSaved.PriSet);
  PriGpsCurrent.Assign(PLastSaved.PriGpSet);
  PriorSetupCurrent.Assign(PLastSaved.PriorSetup);
  UCChanged := False;
  ProjChanged := False;
  ParamCellChanged := False;
  ParamGpsChanged := False;
  StatusClear;
  UpdateMainForm;
  ProjChanged := False;
  EnableSaveRevert;
end;

procedure TFormMain.actStartLocalRunnersExecute(Sender: TObject);
var
  Msg: string;
begin
  if (PCurrent.UcProject.ModeIsParallelizable) then
    begin
      NumRunnersStarted := PCurrent.UcProject.StartLocalRunners;
      Msg := 'Started ' + IntToStr(NumRunnersStarted) + ' runners on local machine';
      ShowMessage(Msg);
    end
  else
    begin
      Msg := 'Runners not started because selected mode does not make parallel model runs.';
      ShowMessage(Msg);
    end;
end;

procedure TFormMain.AssignCurrent;
// Assign control settings to PCurrent.UcProject
var
  Vold: LongInt;
  Mold: TUcodeMode;
begin
  { Assign control values to current [UCODE?] project }
  StatusClear;
  if KeepCurrent then
    begin
      Mold := GlobalData.PCurrent.UcProject.UcMode;
      Vold := GlobalData.PCurrent.UcProject.Verbose;
      with GlobalData.PCurrent.UcProject do
        begin
          if rbForward.Checked then UcMode := umFwd;
          if rbSenAnal.Checked then UcMode := umSensAnal;
          if rbParEst.Checked then UcMode := umParEst;
          if rbTestLin.Checked then UcMode := umTestLin;
          if rbPred.Checked then UcMode := umPred;
          if rbAdvTestLin.Checked then UcMode := umAdvTestLin;
          if rbNonLinUnc.Checked then UcMode := umNonlinUncert;
          if rbInvObjFn.Checked then UcMode := umInvObjFunc;
        end;
      if (GlobalData.PCurrent.UcProject.UcMode <> Mold) or
           (GlobalData.PCurrent.UcProject.Verbose <> Vold) then
        begin
          UCChanged := True;
          ProjChanged := True;
          StatusBar1.Panels[1].Text :=
              'Project settings have been updated';
        end;
      tbSave.Enabled := True;
    end;
  EnableSaveRevert;
end;

procedure TFormMain.AssignParameterGroups;
// Assign parameter groups and attributes in dgParamGps to ParamGpsCurrent
var
  ICol, IPA, IParGp, IRow, NParGps: integer;
  TempCaption, TempString: string;
  PatTemp: TParamAttType;
  ClearedLocalGps: boolean;
begin
  ClearedLocalGps := False;
  NParGps := rbwdgParamGps.RowCount - 1;
  if (not (ParamGpsCurrent.Count = NParGps)) then
    begin
      ParamGpsCurrent.Clear;
      ClearedLocalGps := True;
    end;
  if NParGps > 0 then
    begin
      for IParGp := 0 to NParGps - 1 do
        begin
          if ClearedLocalGps then ParamGpsCurrent.Add;
          IRow := IParGp + 1;
          TempString := rbwdgParamGps.Cells[0,IRow];
          ParamGpsCurrent.Items[IParGp].Name := ConvertString12(TempString);
          // Assign attributes to this parameter group, as defined in table.
          // Loop through columns (parameter-group attributes).
          for ICol := 1 to rbwdgParamGps.ColCount - 1 do
            begin
              TempCaption := rbwdgParamGps.Cells[ICol,0];
              // Find index of parameter attribute that has this caption.
              IPA := PosCap(TempCaption);
              // Assign data grid value to parameter attribute.
              PatTemp := ParamGpsCurrent.Items[IParGp].AllAtts[IPA].ParamAttType;
              if (PatTemp = patAdjustable) or (PatTemp = patTransform) or
                 (PatTemp = patConstrain) then
                begin
                  ParamGpsCurrent.Items[IParGp].AllAtts[IPA].Text :=
                      BooleanToYesOrNo(rbwdgParamGps.Checked[ICol,IRow]);
                end
              else
                begin
                  TempString := rbwdgParamGps.Cells[ICol, IRow];
                  ReplaceParAttValue(TempString, PatTemp);
                  ParamGpsCurrent.Items[IParGp].AllAtts[IPA].Text := TempString;
                end;
            end;
        end;
      if ClearedLocalGps then
        begin
          TempString := 'Need to add code in TFormMain.AssignParameterGroups'
                        + ' to restore hidden attributes';
          ShowMessage(TempString);
        end;
    end;
end;

function TFormMain.AssignParameters: boolean;
// Assign parameters and attributes in dgParams to ParamSetCurrent.
var
  ErrRow, I, IPar, ICol, IPA, IRow, NPar, PosDerived: integer;
  FirstBlankParRow, NParLines, NParNames: integer;
  PatTemp: TParamAttType;
  ErrMsg, ErrName, TempCaption, TempName, TempString: string;
  PSetTemp: TParamSet;
  ClearedParamSetCurrent, DerivedVisible, FoundMatch, OK: boolean;
begin
  DerivedVisible := False;
  ClearedParamSetCurrent := False;
  OK := True;
  if (rbwdgParams.RowCount > 1) then
    begin
      if (rbwdgParams.RowCount > 2) or (IsNonBlank(rbwdgParams.Cells[0,1])) then
        begin
          OK := CheckNamesInColumn(rbwdgParams,0,MaxLenParName,ErrName,ErrRow);
          if not OK then
            begin
              ErrMsg := 'Invalid parameter name: ' + ErrName;
              ShowMessage(ErrMsg);
              rbwdgParams.Col := 0;
              rbwdgParams.Row := ErrRow;
              rbwdgParams.SetFocus;
            end;
          if OK then
            begin
              // Count lines not filled with blanks in parameters table
              // If NParNames < NParLines, there's at least one blank parameter name
              CountParLines(rbwdgParams,NParLines,NParNames,FirstBlankParRow);
              if NParLines = NParNames then
                begin
                  PSetTemp := TParamSet.Create;
                  try
                    PosDerived := ParAttPos(patDerived);
                    PSetTemp.Assign(ParamSetCurrent); // Save DerivEqn strings and other attributes.
                    NPar := rbwdgParams.RowCount-1;
                    if (not (ParamSetCurrent.Count = NPar)) then
                      begin
                        ParamSetCurrent.Clear;
                        ClearedParamSetCurrent := True;
                      end;
                    if NPar > 0 then
                      begin
                        for IPar := 0 to NPar-1 do
                          begin
                            if rbwdgParams.Cells[0,IPar+1] <> '' then
                              begin
                                if ClearedParamSetCurrent then ParamSetCurrent.Add;
                                IRow := IPar+1;
                                TempName := rbwdgParams.Cells[0,IRow];
                                ParamSetCurrent.Items[IPar].Name := ConvertString12(TempName);
                                // Assign attributes to this parameter, as defined in table.
                                // Loop through columns (parameter attributes).
                                for ICol := 1 to rbwdgParams.ColCount - 1 do
                                  begin
                                    TempCaption := rbwdgParams.Cells[ICol,0];
                                    // Find index of parameter attribute that has this caption.
                                    IPA := PosCap(TempCaption);
                                    // Assign data grid value to parameter attribute.
                                    PatTemp := ParamSetCurrent.Items[IPar].AllAtts[IPA].ParamAttType;
                                    if (PatTemp = patDerived) or (PatTemp = patAdjustable) or
                                       (PatTemp = patTransform) or (PatTemp = patConstrain) then
                                      begin
                                        if PatTemp = patDerived then
                                          DerivedVisible := True;
                                        ParamSetCurrent.Items[IPar].AllAtts[IPA].Text :=
                                            BooleanToYesOrNo(rbwdgParams.Checked[ICol,IRow]);
                                      end
                                    else
                                      begin
                                        TempString := rbwdgParams.Cells[ICol, IRow];
                                        ReplaceParAttValue(TempString, PatTemp);
                                        ParamSetCurrent.Items[IPar].AllAtts[IPA].Text := TempString;
                                      end;
                                  end;
                                // Find matching name in PSetTemp and restore DerivEqn and,
                                // if Derived is not visible, the value of Derived.
                                I := 0;
                                FoundMatch := False;
                                while (I < PSetTemp.Count) and (not FoundMatch) do
                                  begin
                                    if AnsiSameText(ConvertString(PsetTemp.Items[I].Name),TempName) then
                                      begin
                                        ParamSetCurrent.Items[IPar].DerivEqn := PSetTemp.Items[I].DerivEqn;
                                        if not DerivedVisible then
                                          begin
                                            ParamSetCurrent.Items[IPar].AllAtts.Items[PosDerived].Text :=
                                                PSetTemp.Items[I].AllAtts.Items[PosDerived].Text;
                                          end;
                                        FoundMatch := True;
                                      end;
                                    I := I + 1;
                                  end;
                              end;
                          end;
                        // If # of parameters or parameter names have changed, need new template file.
                        if PSetTemp.Count = ParamSetCurrent.Count then
                          begin
                            for I := 0 to PSetTemp.Count - 1 do
                              begin
                                if ParamSetCurrent.Items[I].Name <> PSetTemp.Items[I].Name then
                                  begin
                                    if PCurrent.LinkTemplateToParamsTable then NeedPvalTemplate := True;
                                  end;
                              end;
                          end
                        else
                          begin
                            if PCurrent.LinkTemplateToParamsTable then NeedPvalTemplate := True;
                          end;
                      end;
                  finally
                    PSetTemp.Free;
                  end;
                end
              else
                begin
                  ShowMessage('Error in Parameters Table: At least one parameter is unnamed');
                  OK := False;
                end;
            end;
        end
      else
        begin
          if ParamSetCurrent.Count > 0 then
            ParamSetCurrent.Clear;
        end;
    end;
  result := OK;
end;

procedure TFormMain.SaveProjectFile(FileName: string);
var
  FileStream: TFileStream;     // Text file defining one or more objects.
  MemStream: TMemoryStream;    // Temporarily hold an object.
  TextStream: TMemoryStream;   // Text form of a stream.
begin
  if AssignParameters then
    begin
      JvProgressDialog1.Position := 30;
      UpdateCurrentProject;
      JvProgressDialog1.Position := 35;
      PCurrent.AssignVersion;
      JvProgressDialog1.Position := 40;
      MemStream := TMemoryStream.Create;
      JvProgressDialog1.Position := 45;
      TextStream := TMemoryStream.Create;
      JvProgressDialog1.Position := 50;
      //
      // Open a text file.
      FileStream := TFileStream.Create(FileName, fmCreate);
      JvProgressDialog1.Position := 55;
      try
        //
        // Write the TProject data to the memory stream.
        MemStream.WriteComponent(PCurrent);
        JvProgressDialog1.Position := 60;
        //MemStream.WriteComponent(PCurrent.UcProject);
        MemStream.Position := 0;
        JvProgressDialog1.Position := 65;
        // Convert the memory stream to a text stream.
        ObjectBinaryToText(MemStream, TextStream);
        JvProgressDialog1.Position := 70;
        // write the TProject text stream to the text file.
        TextStream.Position := 0;
        FileStream.CopyFrom(TextStream, TextStream.Size);
        JvProgressDialog1.Position := 75;
        //
      finally
        // Free all streams.
        FreeAndNil(FileStream);
        FreeAndNil(MemStream);
        FreeAndNil(TextStream);
      end;
    end;
end;

procedure TFormMain.SetCaption;
var
  AbsPath: TFileName;
  TempCaption: string;
begin
  AbsPath := ExpandFileName(PCurrent.FileName);
  TempCaption := 'ModelMate: [' + AbsPath + ']';
  if Length(TempCaption) > (self.Width-80)/7 then
    TempCaption := 'ModelMate: [' + ExtractFileName(PCurrent.FileName) + ']';
  Caption := TempCaption;
end;

procedure TFormMain.StatusBar1Click(Sender: TObject);
begin
  if (StatusBar1.Panels[1].Text <> '') then
    begin
      ShowMessage(StatusBar1.Panels[1].Text);
    end;
end;

procedure TFormMain.StatusBar1DrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
begin
  if Panel = StatusBar.Panels[1] then
    begin
      StatusBar.Canvas.TextRect(Rect,Rect.Left+1,5,Panel.Text);
    end;
end;

procedure TFormMain.StatusClear;
begin
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Invalidate;
  StatusBar1.Repaint;
end;


procedure TFormMain.SupportedModels1Click(Sender: TObject);
begin
  FormModflow.ShowModal;
  if ProjChanged then
    begin
      AssignCurrent;
    end;
end;

procedure TFormMain.tsObsShow(Sender: TObject);
// Populate controls on Observations tab sheet.
begin
  StatusBar1.Panels[1].Text := '';
  PopulateObsTabSheet(PCurrent);
end;

procedure TFormMain.tsParamsShow(Sender: TObject);
begin
  StatusBar1.Panels[1].Text := '';
end;

procedure TFormMain.tsPredsShow(Sender: TObject);
// Populate controls on Predictions tab sheet
begin
  StatusBar1.Panels[1].Text := '';
  PopulatePredTabSheet(PCurrent);
end;

procedure TFormMain.ViewFile(FName: TFileName);
begin
  FormEditorAppOutput.CurrentEditorFile := FName;
  FormEditorAppOutput.ShowViewCurrentEditorFile;
end;

function TFormMain.SelectPaoptFile(var fnPaopt: TFileName): boolean;
var
  OK: boolean;
begin
  odUcode.FileName := '';
  odUcode.Title := 'Open a UCODE Optimized Parameters file';
  odUcode.Filter := 'UCODE Optimized Parameters Files (*._paopt)|*._paopt';
  if odUcode.Execute then
    begin
      fnPaopt := odUcode.FileName;
      if FileExists(fnPaopt) then
        OK := True
      else
        OK := False;
    end
  else
    OK := False;
  result := OK;
end;

procedure TFormMain.miUcodeFileNamesClick(Sender: TObject);
begin
  FormUcodeFileNames.MainInputFile := PCurrent.UcProject.AbsMainInputFileName(muCalib);
  FormUcodeFileNames.OutputPrefix := PCurrent.UcProject.OutputPrefix;
  FormUcodeFileNames.MainInputFilePred := PCurrent.UcProject.AbsMainInputFileName(muPred);
  FormUcodeFileNames.OutputPrefixPred := PCurrent.UcProject.OutputPrefixPred;
  FormUcodeFileNames.ShowModal;
  if ProjChanged then
    begin
      PCurrent.UcProject.MainInputFileName := FormUcodeFileNames.MainInputFile;
      PCurrent.UcProject.OutputPrefix := FormUcodeFileNames.OutputPrefix;
      PCurrent.UcProject.MainInputFileNamePred := FormUcodeFileNames.MainInputFilePred;
      PCurrent.UcProject.OutputPrefixPred := FormUcodeFileNames.OutputPrefixPred;
    end;
  PopulateControls(PCurrent);
end;

procedure TFormMain.miUcodeSettingsClick(Sender: TObject);
begin
  frmUcodeSettings.ShowModal;
  PopulateControls(PCurrent);
end;

procedure TFormMain.UpdateMainForm;
begin
  SetCaption;
  PopulateControls(PCurrent);
  EnableSaveRevert;
  Invalidate;
end;

procedure TFormMain.InitializeMemoryObjects;
begin
  ParamSetLocal.Clear;
  InitializeGlobalMemoryObjects;
  InitializeDependentsMemoryObjects;
  InitializePriorInfoMemoryObjects;
end;

procedure TFormMain.JvNetscapeSplitter1Moved(Sender: TObject);
begin
  ClearAllSelections(rbwdgParams);
  ClearAllSelections(rbwdgParamGps);
end;

procedure TFormMain.SaveParamGpsChanges;
var
  IRowCurr: Integer;
  IPGold: Integer;
  NewGroup: string;
  IRow: Integer;
  GpName: string;
  IPG: Integer;
  J: Integer;
  ICol: Integer;
  TempCaption: string;
  IPA: Integer;
  PatTemp: TParamAttType;
  TempString: string;
begin
  CheckParamGpNames;
  IRowCurr := rbwdgParamGps.Row;
  IPGold := IRowCurr - 1;
  //
  // Loop through rows (parameter groups)
  for IRow := 1 to rbwdgParamGps.RowCount - 1 do
  begin
    // Get parameter group name from column 0 of current row
    GpName := rbwdgParamGps.Cells[0, IRow];
    // Find index of this parameter group in fAllGps array
    IPG := -1;
    for J := 0 to ParamGpsCurrent.Count - 1 do
    begin
      if ConvertString(ParamGpsCurrent.Items[J].Name) = GpName then
        IPG := J;
    end;
    // Assign attributes to this parameter group, as defined in table
    // Loop through columns (parameter attributes)
    if IPG > -1 then
    begin
      for ICol := 1 to rbwdgParamGps.ColCount - 1 do
      begin
        TempCaption := rbwdgParamGps.Cells[ICol, 0];
        // Find index of parameter attribute that has this caption
        IPA := PosCap(TempCaption);
        // Assign data grid value to parameter attribute
        PatTemp := ParamGpsCurrent.Items[IPG].AllAtts[IPA].ParamAttType;
        if PatTemp in SetATBoolean then
          TempString := BooleanToYesOrNo(rbwdgParamGps.Checked[ICol, IRow])
        else
          TempString := rbwdgParamGps.Cells[ICol, IRow];
        ReplaceParAttValue(TempString, PatTemp);
        ParamGpsCurrent.Items[IPG].AllAtts[IPA].Text := TempString;
      end;
    end
    else
    begin
      IPG := IPGold;
      for ICol := 1 to rbwdgParamGps.ColCount - 1 do
      begin
        TempCaption := rbwdgParamGps.Cells[ICol, 0];
        // Find index of parameter attribute that has this caption
        IPA := PosCap(TempCaption);
        // Assign data grid value to parameter attribute
        PatTemp := ParamGpsCurrent.Items[IPG].AllAtts[IPA].ParamAttType;
        TempString := rbwdgParamGps.Cells[ICol, IRow];
        ReplaceParAttValue(TempString, PatTemp);
        ParamGpsCurrent.Items[IPG].AllAtts[IPA].Text := TempString;
        ParamGpsCurrent.Items[IPG].Name := ConvertString12(NewGroup);
      end;
    end;
  end;
  AssignParameterGroups;
  UpdateCurrentProject;
end;

procedure TFormMain.ViewUcodeInputFileClick(Sender: TObject);
var
  FullPath: TFileName;
  ModelUseLocal: TModelUse;
begin
  case PCurrent.UcProject.UcMode of
    umPred: ModelUseLocal := muPred;
  else
    ModelUseLocal := muCalib;
  end;
  FullPath := PCurrent.UcProject.AbsMainInputFileName(ModelUseLocal);
  FormEditorAppInput.CurrentEditorFile := FullPath;
  FormEditorAppInput.ShowViewCurrentEditorFile;
end;

procedure TFormMain.ViewUcodeOutputClick(Sender: TObject);
var
  DirSep: string;
  ModelUse: TModelUse;
begin
  case PCurrent.UcProject.UcMode of
    umPred: ModelUse := muPred;
  else
    ModelUse := muCalib;
  end;
  DirSep := PathDelimiter;
  FormEditorAppOutput.CurrentEditorFile := PCurrent.AbsAppDirectory(ModelUse)
                        + DirSep + PCurrent.UcProject.OutputMainFile;
  FormEditorappOutput.ShowViewCurrentEditorFile;
end;

procedure TFormMain.ReadIniFile;
var
  IniFName: string;
begin
  FIniFile.Free;
  IniFName := IniFileName(Handle, Application.ExeName);
  FIniFile:= TMemInifile.Create(IniFName);
    GlobalProgramLocations.ReadFromIniFile(IniFile);
end;

procedure TFormMain.WriteIniFile;
begin
  GlobalProgramLocations.WriteToIniFile(IniFile);
  try
    IniFile.UpdateFile;
  except on EFileStreamError do
    begin
      // Ignore errors saving ini files.
    end;
  end;
end;

procedure TFormMain.AssignProjectDirectory(NewProjectPath:
                                           string; OrigProjDir: string);
begin
  ProjectDirectory := ExtractFileDir(NewProjectPath);
  PCurrent.FileName := NewProjectPath;
  // If directory has changed, correct relative paths
  // of Model IO and other files.
  if OrigProjDir <> ProjectDirectory then
    PCurrent.CorrectRelativePaths(OrigProjDir);
  //
  if OrigProjDir = PathDelimiter then
  begin
    // Convert any absolute paths to relative paths.
    PCurrent.NormalizePaths;
  end;
end;

end.
