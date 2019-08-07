unit frmRenameGroup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  GlobalTypesUnit, StdCtrls, Buttons, Utilities, GlobalBasicData,
  ModelMateUtilities, frmNamingConvention;

type
  TFormRenameGroup = class(TForm)
    lblOldGroup: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    edtNewGroup: TEdit;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    CellData: GridCell;
    GpUse: TGrpUse;
  end;

var
  FormRenameGroup: TFormRenameGroup;

implementation

{$R *.dfm}

procedure TFormRenameGroup.btnOKClick(Sender: TObject);
var
  ErrMsg, GroupName: string;
begin
  GroupName := edtNewGroup.Text;
  if J_Valid_Name(GroupName,MaxLenGpName) then
    begin
      if UniqueGroupName(GroupName,GpUse) then
        begin
          CellData.TextNew := GroupName;
          ModalResult := mrOK;
        end
      else
        begin
          ErrMsg := 'Group name already in use';
          ShowMessage(ErrMsg);
          ModalResult := mrNo;
        end;
    end
  else
    begin
      ErrMsg := 'Invalid Name: ' + GroupName;
      ShowMessage(ErrMsg);
      FormNamingConvention.ShowModal;
      ModalResult := mrNo;
    end;
end;

procedure TFormRenameGroup.FormShow(Sender: TObject);
begin
  lblOldGroup.Caption := CellData.TextOld;
  edtNewGroup.Text := CellData.TextNew;
  edtNewGroup.SetFocus;
end;

end.
