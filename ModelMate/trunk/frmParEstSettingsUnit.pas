unit frmParEstSettingsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Spin,
  GlobalBasicData, GlobalData, GlobalTypesUnit,
  ModelMateClassesUnit, ExtCtrls, UcodeUnit,
  frmParEstAdvancedUnit, Grids, DataGrid, Utilities, Buttons;

type
  TfrmParEstSettings = class(TForm)
    edtTolPar: TEdit;
    lblTolpar: TLabel;
    lblTolSOSC: TLabel;
    edtTolSOSC: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    edtMaxChange: TEdit;
    Label4: TLabel;
    cmbMaxChangeRealm: TComboBox;
    btnUcodeParEstResetDefaults: TButton;
    StatusBar1: TStatusBar;
    gbTrustRegion: TGroupBox;
    cbTrustRegion: TCheckBox;
    rbDogleg: TRadioButton;
    rbHookStep: TRadioButton;
    edtMaxStep: TEdit;
    seConsecmax: TSpinEdit;
    lblMaxStep: TLabel;
    lblConsecmax: TLabel;
    cbUseMaxStep: TCheckBox;
    lblDoglegOptions: TLabel;
    cbStatsOnNonConverge: TCheckBox;
    cbOmitInsensitive: TCheckBox;
    edtMinSensRatio: TEdit;
    edtReincludeSensRatio: TEdit;
    edtTolParWtOS: TEdit;
    lblMSR: TLabel;
    lblRSR: TLabel;
    Label8: TLabel;
    btnAdvanced: TButton;
    dgOmitVals: TEcDataGrid;
    lblOmitVals: TLabel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    spedMaxIter: TSpinEdit;
    procedure FormClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnAdvancedClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnUcodeParEstResetDefaultsClick(Sender: TObject);
    procedure cbTrustRegionClick(Sender: TObject);
    procedure cbUseMaxStepClick(Sender: TObject);
    procedure cmbMaxChangeRealmChange(Sender: TObject);
    procedure cmbMaxChangeRealmClick(Sender: TObject);
    procedure edtTolParChange(Sender: TObject);
    procedure edtTolSOSCChange(Sender: TObject);
    procedure edtMaxChangeChange(Sender: TObject);
    procedure edtMqrtDirChange(Sender: TObject);
    procedure edtMqrtFacChange(Sender: TObject);
    procedure edtMqrtIncChange(Sender: TObject);
    procedure edtQNiterChange(Sender: TObject);
    procedure edtQNsosrChange(Sender: TObject);
    procedure edtTolParClick(Sender: TObject);
    procedure edtTolSOSCClick(Sender: TObject);
    procedure edtMaxChangeClick(Sender: TObject);
    procedure edtMqrtDirClick(Sender: TObject);
    procedure edtMqrtFacClick(Sender: TObject);
    procedure edtMqrtIncClick(Sender: TObject);
    procedure edtQNiterClick(Sender: TObject);
    procedure edtQNsosrClick(Sender: TObject);
    procedure gpbxQuasiNewtonClick(Sender: TObject);
    procedure gpbxMqrtClick(Sender: TObject);
    procedure rbDoglegClick(Sender: TObject);
    procedure rbHookStepClick(Sender: TObject);
    procedure StatusBar1Click(Sender: TObject);
    procedure cbOmitInsensitiveClick(Sender: TObject);
    procedure dgOmitValsExit(Sender: TObject);
    procedure spedMaxIterChange(Sender: TObject);
    procedure spedMaxIterClick(Sender: TObject);
 private
    { Private declarations }
    procedure AssignCurrent;
    procedure PopulateControls(UcPrj: TUcProject);
    procedure StatusClear;
    procedure SetupTrustRegion(UcPrj: TUcProject);
  public
    { Public declarations }
  end;

var
  frmParEstSettings: TfrmParEstSettings;

implementation

{$R *.dfm}

var
  KeepCurrent: boolean;
  LocalUcProject: TUcProject;

procedure TfrmParEstSettings.FormClick(Sender: TObject);
begin
  StatusClear;
end;

procedure TfrmParEstSettings.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  LocalUcProject.Free;
  UCParEstSetWinOpen := False;
end;

procedure TfrmParEstSettings.FormCreate(Sender: TObject);
begin
  CenterForm(self);
  KeepCurrent := False;
end;

procedure TfrmParEstSettings.FormShow(Sender: TObject);
begin
  LocalUcProject := TUcProject.Create(self);
  LocalUcProject.Assign(PCurrent.UcProject);
  UCParEstSetWinOpen := True;
  StatusClear;
  KeepCurrent := False;
  PopulateControls(LocalUcProject);
  KeepCurrent := True;
end;

procedure TfrmParEstSettings.gpbxMqrtClick(Sender: TObject);
begin
  StatusClear;
end;

procedure TfrmParEstSettings.gpbxQuasiNewtonClick(Sender: TObject);
begin
  StatusClear;
end;

procedure TfrmParEstSettings.PopulateControls(UcPrj: TUcProject);
{ Populate controls on Par. Est. Settings form
  with appropriate values from project Prj}
var
  I: Integer;
begin
  StatusClear;
  I := UcPrj.MaxIter;
  spedMaxIter.Text := IntToStr(I);
  edtTolPar.Text := FloatToStr(UcPrj.Tolpar);
  edtTolSOSC.Text := FloatToStr(UcPrj.TolSOSC);
  edtMaxChange.Text := FloatToStr(UcPrj.MaxChange);
  if AnsiSameText(ConvertString(UcPrj.MaxChangeRealm),'Native') then
    cmbMaxChangeRealm.ItemIndex := 0
  else
    if AnsiSameText(ConvertString(UcPrj.MaxChangeRealm),'Regression') then
      cmbMaxChangeRealm.ItemIndex := 1
    else
      StatusBar1.Panels[1].Text := 'Error assigning MaxChangeRealm';
  cbStatsOnNonConverge.Checked := UcPrj.StatsOnNonConverge;
  cbOmitInsensitive.Checked := UcPrj.OmitInsensitive;
  edtMinSensRatio.Enabled := cbOmitInsensitive.Checked;
  lblMSR.Enabled := cbOmitInsensitive.Checked;
  edtReincludeSensRatio.Enabled := cbOmitInsensitive.Checked;
  lblRSR.Enabled := cbOmitInsensitive.Checked;
  edtMinSensRatio.Text := FloatToStr(UcPrj.MinimumSensRatio);
  edtReincludeSensRatio.Text := FloatToStr(UcPrj.ReincludeSensRatio);
  edtTolParWtOS.Text := FloatToStr(UcPrj.TolParWtOS);
  // Trust Region panel
  SetupTrustRegion(UcPrj);
  // Data grid dgOmitVals
  dgOmitVals.RowCount := 1;
  dgOmitVals.Cells[0,0] := '';
  if UcPrj.OmitVals.Count > 0 then
    begin
      dgOmitVals.RowCount := UcPrj.OmitVals.Count;
      for I := 0 to UcPrj.OmitVals.Count - 1 do
        begin
          dgOmitVals.Cells[0,I] := UcPrj.OmitVals.Strings[I];
        end;
    end;
  StatusClear;
end;

procedure TfrmParEstSettings.rbDoglegClick(Sender: TObject);
begin
  if rbDogleg.Checked then
    begin
      TempUcProject.TrustRegion := 'dogleg';
    end
  else
    begin
      TempUcProject.TrustRegion := 'hookstep';
    end;
  SetupTrustRegion(TempUcProject);
end;

procedure TfrmParEstSettings.rbHookStepClick(Sender: TObject);
begin
  if rbHookStep.Checked then
    begin
      TempUcProject.TrustRegion := 'hookstep';
    end
  else
    begin
      TempUcProject.TrustRegion := 'dogleg';
    end;
  SetupTrustRegion(TempUcProject);
end;

procedure TfrmParEstSettings.AssignCurrent;
{ Assign control values to LocalUcProject }
var
  I: integer;
  S: string20;
  OK: boolean;
begin
  OK := True;
  StatusClear;
  if KeepCurrent then
  begin
    with LocalUcProject do
    begin
      S := 'start';
      S := ConvertString20(spedMaxIter.Text);
      if (not IsNumber(spedMaxIter.Text)) then OK := False;
      if OK then
        begin
          MaxIter := StrToInt(spedMaxIter.Text);
          S := ConvertString20(edtTolPar.Text);
          TolPar := StrToFloat(edtTolPar.Text);
          TolSOSC := StrToFloat(edtTolSOSC.Text);
          MaxChange := StrToFloat(edtMaxChange.Text);
          MaxChangeRealm := ConvertString12(cmbMaxChangeRealm.Text);
          StatsOnNonConverge := cbStatsOnNonconverge.Checked;
          OmitInsensitive := cbOmitInsensitive.Checked;
          MinimumSensRatio := StrToFloat(edtMinSensRatio.Text);
          ReincludeSensRatio := StrToFloat(edtReincludeSensRatio.Text);
          TolParWtOS := StrToFloat(edtTolParWtOS.Text);
          // Trust Region panel
          if cbTrustRegion.Checked then
            begin
              if rbDogLeg.Checked then TrustRegion := 'dogleg';
              if rbHookStep.Checked then TrustRegion := 'hookstep';
              UseMaxStep := cbUsemaxStep.Checked;
              if UseMaxStep then
                begin
                  MaxStep := StrToFloat(edtMaxStep.Text);
                  ConsecMax := StrToInt(seConsecMax.Text);
                end;
            end
          else
            begin
              TrustRegion := 'no';
            end;
          // Data grid dgOmitVals
          OmitVals.Clear;
          for I := 0 to dgOmitVals.RowCount - 1 do
            begin
              if dgOmitVals.Cells[0,I] <> '' then
                begin
                  OmitVals.Add(dgOmitVals.Cells[0,I]);
                end;
            end;
        end;
    end;
    if ((OK) and (UCChanged)) then
    begin
      StatusBar1.Panels[1].Text :=
          'Parameter-Estimation settings have been updated';
    end;
  end;
end;

procedure TfrmParEstSettings.btnAdvancedClick(Sender: TObject);
begin
  AssignCurrent; // Assigns control contents to LocalUcProject
  TempUcProject.Assign(LocalUcProject);
  FormParEstAdvanced.ShowModal;
  LocalUcProject.Assign(TempUcProject);
end;

procedure TfrmParEstSettings.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmParEstSettings.btnOKClick(Sender: TObject);
begin
  AssignCurrent; // Assigns control contents to LocalUcProject
  TempUcProject.Assign(LocalUcProject); // Assigns settings to UcodeUnit.TempUcProject
  if TempUcProject <> PCurrent.UcProject then
    begin
      PCurrent.UcProject.Assign(TempUcProject);
      UCChanged := True;
      ProjChanged := True;
    end;
end;

procedure TfrmParEstSettings.btnUcodeParEstResetDefaultsClick(Sender: TObject);
begin
  KeepCurrent := False;
  PopulateControls(PDefault.UcProject);
  KeepCurrent := True;
  AssignCurrent;
end;

procedure TfrmParEstSettings.cbOmitInsensitiveClick(Sender: TObject);
begin
  edtMinSensRatio.Enabled := cbOmitInsensitive.Checked;
  lblMSR.Enabled := cbOmitInsensitive.Checked;
  edtReincludeSensRatio.Enabled := cbOmitInsensitive.Checked;
  lblRSR.Enabled := cbOmitInsensitive.Checked;
end;

procedure TfrmParEstSettings.cbTrustRegionClick(Sender: TObject);
begin
  if cbTrustRegion.Checked then
    begin
      if rbDogLeg.Checked then TempUcProject.TrustRegion := 'dogleg';
      if rbHookStep.Checked then TempUcProject.TrustRegion := 'hookstep';
    end
  else
    TempUcProject.TrustRegion := 'no';
  SetupTrustRegion(TempUcProject);
end;

procedure TfrmParEstSettings.cbUseMaxStepClick(Sender: TObject);
begin
  TempUcProject.UseMaxStep := cbUseMaxStep.Checked;
  SetupTrustRegion(TempUcProject);
end;


procedure TfrmParEstSettings.cmbMaxChangeRealmChange(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmParEstSettings.cmbMaxChangeRealmClick(Sender: TObject);
begin
  StatusClear;
end;

procedure TfrmParEstSettings.dgOmitValsExit(Sender: TObject);
var
  I: integer;
  Msg: string;
  Selection: TGridRect;
begin
      for I := 0 to dgOmitVals.RowCount - 1 do
        begin
          if dgOmitVals.Cells[0,I] <> '' then
            begin
              if not IsNumber(dgOmitVals.Cells[0,I]) then
                begin
                  Msg := 'Value to be ignored must be a number';
                  ShowMessage(Msg);
                  Selection.Left := 0;
                  Selection.Right := 0;
                  Selection.Top := I;
                  Selection.Bottom := I;
                  dgOmitVals.Selection := Selection;
                  dgOmitVals.TopRow := I;
                  dgOmitVals.SetFocus;
                  break;
                end;
            end;
        end;
end;

procedure TfrmParEstSettings.edtMaxChangeChange(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmParEstSettings.edtMaxChangeClick(Sender: TObject);
begin
  StatusClear;
end;

procedure TfrmParEstSettings.edtMqrtDirChange(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmParEstSettings.edtMqrtDirClick(Sender: TObject);
begin
  StatusClear;
end;

procedure TfrmParEstSettings.edtMqrtFacChange(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmParEstSettings.edtMqrtFacClick(Sender: TObject);
begin
  StatusClear;
end;

procedure TfrmParEstSettings.edtMqrtIncChange(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmParEstSettings.edtMqrtIncClick(Sender: TObject);
begin
  StatusClear;
end;

procedure TfrmParEstSettings.edtQNiterChange(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmParEstSettings.edtQNiterClick(Sender: TObject);
begin
  StatusClear;
end;

procedure TfrmParEstSettings.edtQNsosrChange(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmParEstSettings.edtQNsosrClick(Sender: TObject);
begin
  StatusClear;
end;

procedure TfrmParEstSettings.edtTolParChange(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmParEstSettings.edtTolParClick(Sender: TObject);
begin
  StatusClear;
end;

procedure TfrmParEstSettings.edtTolSOSCChange(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmParEstSettings.edtTolSOSCClick(Sender: TObject);
begin
  StatusClear;
end;

procedure TfrmParEstSettings.StatusBar1Click(Sender: TObject);
begin
  StatusClear;
end;

procedure TfrmParEstSettings.SetupTrustRegion(UcPrj: TUcProject);
begin
  // Trust Region panel
  cbUseMaxStep.Checked := UcPrj.UseMaxStep;
  edtMaxStep.Text := FloatToStr(UcPrj.MaxStep);
  seConsecMax.Text := IntToStr(UcPrj.ConsecMax);
  if SameText(ConvertString(UcPrj.TrustRegion), 'no') then
    begin
      cbTrustRegion.Checked := False;
      rbDogLeg.Enabled := False;
      rbHookStep.Enabled := False;
      cbUseMaxStep.Enabled := False;
      edtMaxStep.Enabled := False;
      seConsecMax.Enabled := False;
      lblDoglegOptions.Font.Color := DisabledGray;
      lblMaxStep.Font.Color := DisabledGray;
      lblConsecMax.Font.Color := DisabledGray;
    end
  else
    begin
      rbDogLeg.Enabled := True;
      rbHookStep.Enabled := True;
      if SameText(ConvertString(UcPrj.TrustRegion), 'dogleg') then
        begin
          rbDogLeg.Checked := True;
          rbHookStep.Checked := False;
          lblDoglegOptions.Font.Color := clWindowText;
          cbUseMaxStep.Enabled := True;
          if cbUseMaxStep.Checked then
            begin
              edtMaxStep.Enabled := True;
              seConsecMax.Enabled := True;
              //lblDoglegOptions.Font.Color := clWindowText;
              lblMaxStep.Font.Color := clWindowText;
              lblConsecMax.Font.Color := clWindowText;
            end
          else
            begin
              edtMaxStep.Enabled := False;
              seConsecMax.Enabled := False;
              //lblDoglegOptions.Font.Color := DisabledGray;
              lblMaxStep.Font.Color := DisabledGray;
              lblConsecMax.Font.Color := DisabledGray;
            end;
        end;
      if SameText(ConvertString(UcPrj.TrustRegion), 'hookstep') then
        begin
          rbDogLeg.Checked := False;
          rbHookStep.Checked := True;
          lblDoglegOptions.Font.Color := DisabledGray;
          cbUseMaxStep.Enabled := False;
          edtMaxStep.Enabled := False;
          seConsecMax.Enabled := False;
          lblMaxStep.Font.Color := DisabledGray;
          lblConsecMax.Font.Color := DisabledGray;
        end;
      cbTrustRegion.Checked := True;
    end;
end;

procedure TfrmParEstSettings.spedMaxIterChange(Sender: TObject);
begin
  AssignCurrent;
end;

procedure TfrmParEstSettings.spedMaxIterClick(Sender: TObject);
begin
  StatusClear;
end;

procedure TfrmParEstSettings.StatusClear;
begin
  StatusBar1.Panels[1].Text := ' ';
end;
end.

