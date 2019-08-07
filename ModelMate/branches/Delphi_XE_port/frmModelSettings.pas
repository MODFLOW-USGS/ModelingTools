unit frmModelSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvExMask, JvToolEdit, GlobalData, GlobalTypesUnit,
  GlobalBasicData, Utilities, Buttons;

type
  TFormModflow = class(TForm)
    edtNameFile: TJvFilenameEdit;
    lblNameFile: TLabel;
    rbModflow2005: TRadioButton;
    rbModflow2000: TRadioButton;
    lblExpl: TLabel;
    lblNameFilePred: TLabel;
    edtNameFilePred: TJvFilenameEdit;
    rbGeneric: TRadioButton;
    cbLinkTemplate: TCheckBox;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
    procedure edtNameFileChange(Sender: TObject);
    procedure rbModflow2005Click(Sender: TObject);
    procedure rbModflow2000Click(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure edtNameFilePredChange(Sender: TObject);
    procedure rbGenericClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetupForm;
    procedure SaveSettings;
    procedure ValidatePaths;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormModflow: TFormModflow;
  LocalModelID: TModelID;

implementation

{$R *.dfm}

procedure TFormModflow.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormModflow.btnOKClick(Sender: TObject);
begin
  SaveSettings;
end;

procedure TFormModflow.edtNameFileChange(Sender: TObject);
var
  NameFilePath: string;
begin
  NameFilePath := edtNameFile.Text;
  {
  PCurrent.ModelDirectory := ExtractFileDir(NameFilePath);
  PCurrent.ModflowNameFile := MyExtractRelativePath(ProjectDirectory, NameFilePath);
  ProjChanged := True;
  }
end;

procedure TFormModflow.edtNameFilePredChange(Sender: TObject);
var
  NameFilePath: string;
begin
  NameFilePath := edtNameFilePred.Text;
end;

procedure TFormModflow.FormCreate(Sender: TObject);
begin
  CenterForm(self);
end;

procedure TFormModflow.FormShow(Sender: TObject);
begin
  LocalModelID := PCurrent.ModelID;
  edtNameFile.Text := PCurrent.AbsModflowNameFile(muCalib);
  edtNameFilePred.Text := PCurrent.AbsModflowNameFile(muPred);
  cbLinkTemplate.Checked := PCurrent.LinkTemplateToParamsTable;
  SetupForm;
end;

procedure TFormModflow.rbGenericClick(Sender: TObject);
begin
  if rbGeneric.Checked then
    begin
      LocalModelID := midGeneric;
    end;
  SetupForm;
  ProjChanged := True;
end;

procedure TFormModflow.rbModflow2000Click(Sender: TObject);
begin
      if rbModflow2000.Checked then
        begin
          LocalModelID := midModflow2000;
        end;
      SetupForm;
      ProjChanged := True;
end;

procedure TFormModflow.rbModflow2005Click(Sender: TObject);
begin
      if rbModflow2005.Checked then
        begin
          LocalModelID := midModflow2005;
        end;
      SetupForm;
      ProjChanged := True;
end;

procedure TFormModflow.SetupForm;
begin
  case LocalModelID of
    midModflow2005:
      begin
        rbModflow2005.Checked := True;
        rbModflow2000.Checked := False;
        rbGeneric.Checked := False;
        lblNameFile.Font.Color := clWindowText;
        lblNameFilePred.Font.Color := clWindowText;
        edtNameFile.Enabled := True;
        edtNameFilePred.Enabled := True;
        cbLinkTemplate.Enabled := True;
      end;
    midModflow2000:
      begin
        rbModflow2005.Checked := False;
        rbModflow2000.Checked := True;
        rbGeneric.Checked := False;
        lblNameFile.Font.Color := clWindowText;
        lblNameFilePred.Font.Color := clWindowText;
        edtNameFile.Enabled := False;
        edtNameFilePred.Enabled := False;
        cbLinkTemplate.Enabled := False;
      end;
    midGeneric:
      begin
        rbModflow2005.Checked := False;
        rbModflow2000.Checked := False;
        rbGeneric.Checked := True;
        lblNameFile.Font.Color := clGrayText;
        lblNameFilePred.Font.Color := clGrayText;
        edtNameFile.Enabled := False;
        edtNameFilePred.Enabled := False;
        cbLinkTemplate.Enabled := False;
      end;
    else
      begin
        rbModflow2005.Checked := False;
        rbModflow2000.Checked := False;
        rbGeneric.Checked := False;
        edtNameFile.Enabled := False;
        edtNameFilePred.Enabled := False;
        cbLinkTemplate.Enabled := False;
      end;
  end;
end;

procedure TFormModflow.SaveSettings;
begin
  PCurrent.ModelID := LocalModelID;
  case PCurrent.ModelID of
    midModflow2000: ;
    midModflow2005: ValidatePaths;
    midGeneric: ;
  end;
  if PCurrent.LinkTemplateToParamsTable <> cbLinkTemplate.Checked then
    begin
      PCurrent.LinkTemplateToParamsTable := cbLinkTemplate.Checked;
      ProjChanged := True;
    end;
end;

procedure TFormModflow.ValidatePaths;
var
  Line: string;
  NameFileDir: string;
  NameFilePath: string;
begin
  NameFilePath := edtNameFile.Text;
  // an absolute path.
  NameFileDir := ExtractFileDir(NameFilePath);
  // an absolute path.
  if (PCurrent.AbsModelDirectory(muCalib) <> NameFileDir) or (PCurrent.AbsModflowNameFile(muCalib) <> NameFilePath) then
  begin
    if NameFilePath = '' then
    begin
      // Revert to defaults.
      PCurrent.ModelDirectory := '\';
      PCurrent.ModflowNameFile := '';
      ProjChanged := True;
    end
    else
    begin
      // NameFilePath is not ''.
      if FileExists(NameFilePath) then
      begin
        PCurrent.ModelDirectory := NameFileDir;
        // setter converts to relative path.
        PCurrent.ModflowNameFile := NameFilePath;
        // setter converts to relative path.
        ProjChanged := True;
      end
      else
      begin
        // User has entered a nonexistent file name.
        Line := 'Error: File "' + NameFilePath + '" does not exist';
        ShowMessage(Line);
      end;
    end;
  end;
  // Predictive model.
  NameFilePath := edtNameFilePred.Text;
  // an absolute path.
  NameFileDir := ExtractFileDir(NameFilePath);
  // an absolute path.
  if (PCurrent.AbsModelDirectory(muPred) <> NameFileDir) or (PCurrent.AbsModflowNameFile(muPred) <> NameFilePath) then
  begin
    if NameFilePath = '' then
    begin
      // Revert to defaults.
      PCurrent.ModelDirectoryPred := '\';
      PCurrent.ModflowNameFilePred := '';
      ProjChanged := True;
    end
    else
    begin
      // NameFilePath is not ''.
      if FileExists(NameFilePath) then
      begin
        PCurrent.ModelDirectoryPred := NameFileDir;
        // setter converts to relative path.
        PCurrent.ModflowNameFilePred := NameFilePath;
        // setter converts to relative path.
        ProjChanged := True;
      end
      else
      begin
        // User has entered a nonexistent file name.
        Line := 'Error: File "' + NameFilePath + '" does not exist';
        ShowMessage(Line);
      end;
    end;
  end;
end;

end.
