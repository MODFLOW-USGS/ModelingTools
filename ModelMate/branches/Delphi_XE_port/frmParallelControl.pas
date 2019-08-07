unit frmParallelControl;

interface

uses
  Buttons, Classes, Controls, Dialogs, Forms, Graphics, Mask, Math, Messages,
  StdCtrls, SysUtils, Variants, Windows,
  JvExStdCtrls, JvEdit, JvValidateEdit, JvExMask, JvSpin,
  GlobalData, ParallelControlUnit, frmParallelRunners, GlobalBasicData,
  frmRunnerFiles, Spin;

type
  TFormParallelControl = class(TForm)
    ckbxUseParallelProcessing: TCheckBox;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    GroupBox1: TGroupBox;
    cbAutoPopRunDirs: TCheckBox;
    btnFilesForRunners: TSpeedButton;
    btnPopulateRunnerDirectories: TSpeedButton;
    GroupBox2: TGroupBox;
    ckbxAutoStart: TCheckBox;
    ckbxAutoStop: TCheckBox;
    GroupBox3: TGroupBox;
    btnRunnerDirectories: TSpeedButton;
    edNumRun: TEdit;
    Label5: TLabel;
    edNumUseTrue: TEdit;
    Label6: TLabel;
    Label1: TLabel;
    GroupBox4: TGroupBox;
    cmbVerboseRunner: TComboBox;
    Label2: TLabel;
    veWait: TJvValidateEdit;
    Label3: TLabel;
    veTimeOutFactor: TJvValidateEdit;
    Label4: TLabel;
    spedNumToUse: TSpinEdit;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnRunnerDirectoriesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnFilesForRunnersClick(Sender: TObject);
    procedure btnPopulateRunnerDirectoriesClick(Sender: TObject);
  private
    procedure StoreFormData;
    procedure PopulateForm;
    { Private declarations }
  public
    { Public declarations }
    NumUsableRunners: integer;
  end;

var
  FormParallelControl: TFormParallelControl;
  ParallelControlLocal: TParallelControl;

implementation

{$R *.dfm}

procedure TFormParallelControl.btnFilesForRunnersClick(Sender: TObject);
var
  ModRes: integer;
begin
  RunnerFilesGlobal.Assign(PCurrent.UcProject.RunnerFiles);
  ModRes := FormRunnerFiles.ShowModal;
  if ModRes = mrOK then
    begin
      if not PCurrent.UcProject.RunnerFiles.SameAs(RunnerFilesGlobal) then
        begin
          PCurrent.UcProject.RunnerFiles.Assign(RunnerFilesGlobal);
          ProjChanged := True;
        end;
    end;
end;

procedure TFormParallelControl.btnOKClick(Sender: TObject);
begin
  StoreFormData;
  ParallelControlGlobal.Assign(ParallelControlLocal);
end;

procedure TFormParallelControl.btnPopulateRunnerDirectoriesClick(
  Sender: TObject);
var
  ErrMsg, Msg: string;
  NFiles, NDir: integer;
begin
  if PCurrent.UcProject.PopulateRunnerDirectories(ErrMsg, NFiles, NDir) then
    begin
      Msg := IntToStr(NDir) + ' runner directories are up-to-date.';
      ShowMessage(Msg);
    end
  else
    begin
      ShowMessage(ErrMsg);
    end;
end;

procedure TFormParallelControl.StoreFormData;
begin
  ParallelControlLocal.Parallel := self.ckbxUseParallelProcessing.Checked;
  ParallelControlLocal.Wait := self.veWait.Value;
  ParallelControlLocal.TimeOutFactor := StrToFloat(self.veTimeOutFactor.Value);
  ParallelControlLocal.NumRunnersToUse := self.spedNumToUse.Value;
  ParallelControlLocal.AutoStopRunners := self.ckbxAutoStop.Checked;
  ParallelControlLocal.AutoStartLocalRunners := self.ckbxAutoStart.Checked;
  ParallelControlLocal.VerboseRunner := cmbVerboseRunner.ItemIndex;
  ParallelControlLocal.AutoPopRunnerDirs := cbAutoPopRunDirs.Checked;
end;

procedure TFormParallelControl.PopulateForm;
begin
  self.spedNumToUse.MaxValue := NumUsableRunners;
  self.ckbxUseParallelProcessing.Checked := ParallelControlLocal.Parallel;
  self.spedNumToUse.Value := Min(ParallelControlLocal.NumRunnersToUse, NumUsableRunners);
  self.ckbxAutoStart.Checked := ParallelControlLocal.AutoStartLocalRunners;
  self.ckbxAutoStop.Checked := ParallelControlLocal.AutoStopRunners;
  self.veWait.Value := ParallelControlLocal.Wait;
  self.veTimeOutFactor.Value := FloatToStr(ParallelControlLocal.TimeOutFactor);
  self.cmbVerboseRunner.ItemIndex := ParallelControlLocal.VerboseRunner;
  self.cbAutoPopRunDirs.Checked := ParallelControlLocal.AutoPopRunnerDirs;
  edNumRun.Text := IntToStr(ParallelRunnersGlobal.Count);
  edNumUseTrue.Text := IntToStr(ParallelRunnersGlobal.NumUsable);
end;

procedure TFormParallelControl.btnRunnerDirectoriesClick(Sender: TObject);
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
          PopulateForm;
        end;
      NumUsableRunners := FormParallelRunners.NumUsableRunners;
      spedNumToUse.MaxValue := NumUsableRunners;
      spedNumToUse.Value := NumUsableRunners;
    end;
end;

procedure TFormParallelControl.FormCreate(Sender: TObject);
begin
  FormParallelControl.NumUsableRunners := 0;
end;

procedure TFormParallelControl.FormShow(Sender: TObject);
begin
  ParallelControlLocal.Assign(ParallelControlGlobal);
  PopulateForm;
end;

initialization
  ParallelControlLocal := TParallelControl.Create;

finalization
  ParallelControlLocal.Free;

end.
