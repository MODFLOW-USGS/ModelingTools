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
  if PCurrent.ProjName <> edtName.Text then
    begin
      PCurrent.ProjName := edtName.Text;
      ProjChanged := True;
    end;
  if PCurrent.Title <> edtTitle.Text then
    begin
      PCurrent.Title := edtTitle.Text;
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
  edtName.Text := PCurrent.ProjName;
  edtTitle.Text := PCurrent.Title;
end;

end.
