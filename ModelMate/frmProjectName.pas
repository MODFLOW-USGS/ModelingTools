unit frmProjectName;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  GlobalBasicData, GlobalData, GlobalTypesUnit, Utilities, Buttons, 
  ModelMateUtilities;

type
  TfrmProjName = class(TForm)
    edtName: TEdit;
    edtTitle: TEdit;
    lblName: TLabel;
    lblTitle: TLabel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    ckbxNameLikeFile: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ckbxNameLikeFileClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmProjName: TfrmProjName;

implementation

{$R *.dfm}

procedure TfrmProjName.btnOKClick(Sender: TObject);
begin
  if ConvertString(PCurrent.ProjName) <> edtName.Text then
    begin
      PCurrent.ProjName := ConvertString255(edtName.Text);
      ProjChanged := True;
    end;
  if ConvertString(PCurrent.Title) <> edtTitle.Text then
    begin
      PCurrent.Title := ConvertString255(edtTitle.Text);
      ProjChanged := True;
    end;
  if PCurrent.NameLikeProjectFile <> ckbxNameLikeFile.Checked then
    begin
      PCurrent.NameLikeProjectFile := ckbxNameLikeFile.Checked;
      ProjChanged := True;
    end;
end;

procedure TfrmProjName.ckbxNameLikeFileClick(Sender: TObject);
begin
  edtName.Enabled := not ckbxNameLikeFile.Checked;
  if ckbxNameLikeFile.Checked then
    edtName.Text := FileBaseName(PCurrent.FileName);
end;

procedure TfrmProjName.FormCreate(Sender: TObject);
begin
  CenterForm(self);
end;

procedure TfrmProjName.FormShow(Sender: TObject);
begin
  ckbxNameLikeFile.Checked := PCurrent.NameLikeProjectFile;
  edtName.Enabled := not ckbxNameLikeFile.Checked;
  edtName.Text := ConvertString(PCurrent.ProjName);
  edtTitle.Text := ConvertString(PCurrent.Title);
end;

end.
