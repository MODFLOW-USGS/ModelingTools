unit frmParEstAdvancedUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  GlobalBasicData, GlobalData, GlobalTypesUnit, UcodeUnit, Utilities, Buttons,
  Spin;

type
  TFormParEstAdvanced = class(TForm)
    lblExpl: TLabel;
    gpbxMqrt: TGroupBox;
    Label1: TLabel;
    Label7: TLabel;
    lblMqrtInc: TLabel;
    edtMqrtDir: TEdit;
    edtMqrtFac: TEdit;
    edtMqrtInc: TEdit;
    GroupBox1: TGroupBox;
    lblQNiter: TLabel;
    lblQNsosr: TLabel;
    ckbxQuasiNewton: TCheckBox;
    edtQNsosr: TEdit;
    btnResetDefault: TButton;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    spedQNiter: TSpinEdit;
    procedure ckbxQuasiNewtonClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnResetDefaultClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure AssignCurrent;
    procedure PopulateControls(const UcPrj: TUcProject);
  public
    { Public declarations }
  end;

var
  FormParEstAdvanced: TFormParEstAdvanced;

implementation

{$R *.dfm}

var
  //KeepCurrent: boolean;
  LocalUcProject: TUcProject;

procedure TFormParEstAdvanced.AssignCurrent;
{ Assign control values to LocalUcProject }
begin
  with LocalUcProject do
    begin
      // Quasi-Newton panel
      QuasiNewton := ckbxQuasiNewton.Checked;
      QNiter := spedQNiter.Value;
      QNsosr := StrToFloat(edtQNsosr.Text);
      // Marquardt Parameter panel
      MqrtDirection := StrToFloat(edtMqrtDir.Text);
      MqrtFactor := StrToFloat(edtMqrtFac.Text);
      MqrtIncrement := StrToFloat(edtMqrtInc.Text);
    end;
end;

procedure TFormParEstAdvanced.PopulateControls(const UcPrj: TUcProject);
{ Populate controls on Par. Est. Advanced Settings form
  with appropriate values from project UcPrj}
var
  I: Integer;
begin
  // Quasi-Newton panel
  I := UcPrj.QNiter;
  spedQNiter.Value := I;
  edtQNsosr.Text := FloatToStr(UcPrj.QNsosr);
  ckbxQuasiNewton.Checked := UcPrj.QuasiNewton;
  // Marquardt Parameter panel
  edtMqrtDir.Text := FloatToStr(UcPrj.MqrtDirection);
  edtMqrtFac.Text := FloatToStr(UcPrj.MqrtFactor);
  edtMqrtInc.Text := FloatToStr(UcPrj.MqrtIncrement);
end;

procedure TFormParEstAdvanced.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormParEstAdvanced.btnOKClick(Sender: TObject);
begin
  AssignCurrent; // Assign control values to LocalUcProject
  TempUcProject.Assign(LocalUcProject);
end;

procedure TFormParEstAdvanced.btnResetDefaultClick(Sender: TObject);
begin
  LocalUcProject.Assign(PDefault.UcProject);
  PopulateControls(LocalUcProject);
end;

procedure TFormParEstAdvanced.ckbxQuasiNewtonClick(Sender: TObject);
begin
  if ckbxQuasiNewton.Checked then
    begin
      lblQNiter.Font.Color := clWindowText;
      spedQNiter.Enabled := True;
      lblQNsosr.Font.Color := clWindowText;
      edtQNsosr.Enabled := True;
    end
  else
    begin
      lblQNiter.Font.Color := DisabledGray;
      spedQNiter.Enabled := False;
      lblQNsosr.Font.Color := DisabledGray;
      edtQNsosr.Enabled := False;
    end;
  UCChanged := True;
  ProjChanged := True;
  //AssignCurrent;
end;


procedure TFormParEstAdvanced.FormCreate(Sender: TObject);
begin
  CenterForm(self);
end;

procedure TFormParEstAdvanced.FormShow(Sender: TObject);
begin
  LocalUcProject := TUcProject.Create(self);
  LocalUcProject.Assign(TempUcProject);
  PopulateControls(LocalUcProject);
end;

end.
