unit frmUcodeSettingsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls,
  GlobalBasicData, GlobalData, GlobalTypesUnit,
  ModelMateClassesUnit, Buttons, UcodeUnit, Utilities,
  frmDerivedParameters, frmPriorInfoControl;

type
  TfrmUcodeSettings = class(TForm)
    btnUcodeResetDefaults: TButton;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    GroupBox1: TGroupBox;
    lblModelName: TLabel;
    edtModelName: TEdit;
    lblModelLenUnit: TLabel;
    cmbModelLenUnit: TComboBox;
    lblModelTimeUnit: TLabel;
    cmbModelTimeUnit: TComboBox;
    lblModelMassUnit: TLabel;
    cmbModelMassUnit: TComboBox;
    GroupBox2: TGroupBox;
    lblStdErrOne: TLabel;
    cmbStdErrOne: TComboBox;
    gpbxCalcSens: TGroupBox;
    ckbxModelDeriv: TCheckBox;
    btnDerivInterface: TButton;
    gpbxInvObjFn: TGroupBox;
    lblSosFile: TLabel;
    lblSosMethod: TLabel;
    btnBrowseSOS: TSpeedButton;
    edtSosFile: TEdit;
    cmbSosMethod: TComboBox;
    gpbxLinAdv: TGroupBox;
    lblConfPred: TLabel;
    ComboBox1: TComboBox;
    gpbxPred: TGroupBox;
    ckbxPredSen: TCheckBox;
    GroupBox3: TGroupBox;
    lblOutputVerbosity: TLabel;
    cmbVerbose: TComboBox;
    gpbxObs: TGroupBox;
    ckbxStartRes: TCheckBox;
    ckbxIntRes: TCheckBox;
    ckbxFinalRes: TCheckBox;
    gpbxSen: TGroupBox;
    lblStartSens: TLabel;
    lblIntSens: TLabel;
    lblFinalSens: TLabel;
    cmbStartSens: TComboBox;
    cmbIntSens: TComboBox;
    cmbFinalSens: TComboBox;
    gpbxDataEx: TGroupBox;
    ckbxDataEx: TCheckBox;
    ckbxCreateInit: TCheckBox;
    ckbxEigenValues: TCheckBox;
    cbxWriteDerivedParams: TCheckBox;
    cbxWritePriorInfo: TCheckBox;
    procedure AssignCurrent;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PopulateControls(UcPrj: TUcProject);
    procedure ckbxModelDerivClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnUcodeResetDefaultsClick(Sender: TObject);
    procedure edtModelNameChange(Sender: TObject);
    procedure cmbModelLenUnitChange(Sender: TObject);
    procedure cmbModelTimeUnitChange(Sender: TObject);
    procedure cmbModelMassUnitChange(Sender: TObject);
    procedure cmbStdErrOneChange(Sender: TObject);
    procedure cmbSosMethodChange(Sender: TObject);
    procedure edtSosFileChange(Sender: TObject);
    procedure ckbxStartResClick(Sender: TObject);
    procedure ckbxIntResClick(Sender: TObject);
    procedure ckbxFinalResClick(Sender: TObject);
    procedure ckbxEigenValuesClick(Sender: TObject);
    procedure cmbStartSensChange(Sender: TObject);
    procedure cmbIntSensChange(Sender: TObject);
    procedure cmbFinalSensChange(Sender: TObject);
    procedure ckbxDataExClick(Sender: TObject);
    procedure ckbxCreateInitClick(Sender: TObject);
    procedure edtOutputPrefixChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ckbxPredSenClick(Sender: TObject);
    procedure cmbVerboseChange(Sender: TObject);
    procedure cbxWriteDerivedParamsClick(Sender: TObject);
    procedure cbxWritePriorInfoClick(Sender: TObject);
  private
    { Private declarations }
    LocalUcProject: TUcProject;
  public
    { Public declarations }
  end;

var
  frmUcodeSettings: TfrmUcodeSettings;

implementation

{$R *.dfm}

var
  KeepCurrent: boolean;

procedure TfrmUcodeSettings.AssignCurrent;
// Use control settings to update current project
begin
  if KeepCurrent then
    begin
      with LocalUcProject do
      begin
        ModelName := ConvertString12(edtModelName.Text);
        ModelLengthUnits := ConvertString12(cmbModelLenUnit.Text);
        ModelTimeUnits := ConvertString12(cmbModelTimeUnit.Text);
        ModelMassUnits := ConvertString12(cmbModelMassUnit.Text);
        Verbose := cmbVerbose.ItemIndex;
        case cmbStdErrOne.ItemIndex of
          0: StdErrOne := False;
          1: StdErrOne := True;
        end;
        UseModelDerivatives := ckbxModelDeriv.Checked;
        // Insert code here to support Investigate Objective Function mode.
        PredictionSensitivities := ckbxPredSen.Checked;
        if ckbxStartRes.Checked then
          StartRes := 'yes'
        else
          StartRes := 'no';
        if ckbxIntRes.Checked then
          IntermedRes := 'yes'
        else
          IntermedRes := 'no';
        if ckbxFinalRes.Checked then
          FinalRes := 'yes'
        else
          FinalRes := 'no';
        WriteDerivedParams := cbxWriteDerivedParams.Checked;
        WritePriorInfo := cbxWritePriorInfo.Checked;
        EigenValues := ckbxEigenValues.Checked;
        StartSens := SensOptions[cmbStartSens.ItemIndex];
        IntermedSens := SensOptions[cmbIntSens.ItemIndex];
        FinalSens := SensOptions[cmbFinalSens.ItemIndex];
        DataExchange := ckbxDataEx.Checked;
        CreateInitFiles := ckbxCreateInit.Checked;
      end;
    end;
end;

procedure TfrmUcodeSettings.btnOKClick(Sender: TObject);
begin
  if PCurrent.UcProject <> LocalUcProject then
    begin
      PCurrent.UcProject.Assign(LocalUcProject);
      UCChanged := True;
      ProjChanged := True;
    end;
end;

procedure TfrmUcodeSettings.btnUcodeResetDefaultsClick(Sender: TObject);
begin
  KeepCurrent := False;
  PopulateControls(PDefault.UcProject);
  KeepCurrent := True;
  AssignCurrent;
end;

procedure TfrmUcodeSettings.cbxWriteDerivedParamsClick(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmUcodeSettings.cbxWritePriorInfoClick(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmUcodeSettings.ckbxCreateInitClick(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmUcodeSettings.ckbxDataExClick(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmUcodeSettings.ckbxEigenValuesClick(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmUcodeSettings.ckbxFinalResClick(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmUcodeSettings.ckbxIntResClick(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmUcodeSettings.ckbxModelDerivClick(Sender: TObject);
begin
  if ckbxModelDeriv.Checked then
    begin
      btnDerivInterface.Enabled := True;
    end
  else
    begin
      btnDerivInterface.Enabled := False;
    end;
  AssignCurrent;
end;

procedure TfrmUcodeSettings.ckbxPredSenClick(Sender: TObject);
begin
  PCurrent.UcProject.PredictionSensitivities := ckbxPredSen.Checked;
  AssignCurrent;
end;

procedure TfrmUcodeSettings.ckbxStartResClick(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmUcodeSettings.cmbFinalSensChange(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmUcodeSettings.cmbIntSensChange(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmUcodeSettings.cmbModelLenUnitChange(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmUcodeSettings.cmbModelMassUnitChange(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmUcodeSettings.cmbModelTimeUnitChange(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmUcodeSettings.cmbSosMethodChange(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmUcodeSettings.cmbStartSensChange(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmUcodeSettings.cmbStdErrOneChange(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmUcodeSettings.cmbVerboseChange(Sender: TObject);
begin
  LocalUcProject.Verbose := cmbVerbose.ItemIndex;
end;

procedure TfrmUcodeSettings.edtModelNameChange(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmUcodeSettings.edtOutputPrefixChange(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmUcodeSettings.edtSosFileChange(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmUcodeSettings.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  UcodeSetWinOpen := False;
end;

procedure TfrmUcodeSettings.FormCreate(Sender: TObject);
begin
  CenterForm(self);
  KeepCurrent := False;
  LocalUcProject := TUcProject.Create(self);
end;

procedure TfrmUcodeSettings.FormDestroy(Sender: TObject);
begin
  LocalUcProject.Free;
end;

procedure TfrmUcodeSettings.FormShow(Sender: TObject);
begin
  UcodeSetWinOpen := True;
  KeepCurrent := False;
  LocalUcProject.Assign(PCurrent.UcProject);
  PopulateControls(LocalUcProject);
  KeepCurrent := True;
end;

procedure TfrmUcodeSettings.PopulateControls(UcPrj: TUcProject);
// Use values from project Prj to populate values on controls
begin
  edtModelName.Text := ConvertString(UcPrj.ModelName);
  cmbModelLenUnit.Text := ConvertString(UcPrj.ModelLengthUnits);
  cmbModelTimeUnit.Text := ConvertString(UcPrj.ModelTimeUnits);
  cmbModelMassUnit.Text := ConvertString(UcPrj.ModelMassUnits);
  cmbVerbose.ItemIndex := UcPrj.Verbose;

  case UcPrj.StdErrOne of
    False: cmbStdErrOne.ItemIndex := 0;
    True: cmbStdErrOne.ItemIndex := 1;
  end;
  //
  if UcPrj.UseModelDerivatives then
    begin
      ckbxModelDeriv.Checked := True;
      btnDerivInterface.Enabled := True;
    end
  else
    begin
      ckbxModelDeriv.Checked := False;
      btnDerivInterface.Enabled := False;
    end;
  //
  if UcPrj.UcMode = umAdvTestLin then
    // Mode is Advanced Test Model Linearity
    begin
      gpbxLinAdv.Enabled := True;
      gpbxLinAdv.Font.Color := clWindowText;
    end
  else
    // Mode is NOT Advanced Test Model Linearity
    begin
      gpbxLinAdv.Enabled := False;
      gpbxLinAdv.Font.Color := DisabledGray;
    end;
  //
  if UcPrj.UcMode = umInvObjFunc then
    // Mode is Investigate Objective Function
    begin
      gpbxInvObjFn.Enabled := True;
      gpbxInvObjFn.Font.Color := clWindowText;
      if UcPrj.SosMethod = smKeywords then
        // Use parameter constraints and SOSIncrement to generate parameter values
        begin
          cmbSosMethod.ItemIndex := 0;
          lblSosFile.Font.Color := DisabledGray;
          edtSosFile.Enabled := False;
          btnBrowseSos.Enabled := False;
        end
      else
        // SosMethod = smFile; get parameter values from SOSFile.
        begin
          cmbSosMethod.ItemIndex := 1;
          lblSosFile.Font.Color := clWindowText;
          edtSosFile.Enabled := True;
          btnBrowseSos.Enabled := True;
        end
    end
  else
    // Mode is NOT Investigate Objective Function.
    begin
      cmbSosMethod.ItemIndex := 0;
      lblSosFile.Font.Color := DisabledGray;
      edtSosFile.Enabled := False;
      btnBrowseSos.Enabled := False;
      gpbxInvObjFn.Enabled := False;
      gpbxInvObjFn.Font.Color := DisabledGray;
    end;
  //
  // Prediction mode group box.
  ckbxPredSen.Checked := UcPrj.PredictionSensitivities;
  if UcPrj.UcMode = umPred then
    begin
      ckbxPredSen.Enabled := True;
      gpbxPred.Enabled := True;
      gpbxPred.Font.Color := clWindowText;
    end
  else
    begin
      ckbxPredSen.Enabled := False;
      gpbxPred.Enabled := False;
      gpbxPred.Font.Color := DisabledGray;
    end;
  //
  if UcPrj.StartRes = 'yes' then
    ckbxStartRes.Checked := True
  else
    ckbxStartRes.Checked := False;
  if UcPrj.IntermedRes = 'yes' then
    ckbxIntRes.Checked := True
  else
    ckbxIntRes.Checked := False;
  if UcPrj.FinalRes = 'yes' then
    ckbxFinalRes.Checked := True
  else
    ckbxFinalRes.Checked := False;
  cbxWriteDerivedParams.Checked := UcPrj.WriteDerivedParams;
  cbxWritePriorInfo.Checked := UcPrj.WritePriorInfo;
  ckbxEigenValues.Checked := UcPrj.EigenValues;
  cmbStartSens.ItemIndex := U_GetSensOptPos(UcPrj.StartSens);
  cmbIntSens.ItemIndex := U_GetSensOptPos(UcPrj.IntermedSens);
  cmbFinalSens.ItemIndex := U_GetSensOptPos(UcPrj.FinalSens);
  ckbxDataEx.Checked := UcPrj.DataExchange;
  ckbxCreateInit.Checked := UcPrj.CreateInitFiles;
end;

end.
