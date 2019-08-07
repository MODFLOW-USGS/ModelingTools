unit frmAddModelOrClassUnit;

interface

uses Winapi.Windows,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit, FMX.StdCtrls,
  FMX.Controls.Presentation, System.IOUtils;

type
  TfrmModelOrClass = class(TForm)
    edtName: TEdit;
    btnCancel: TButton;
    btnOK: TButton;
    lblName: TLabel;
    edModelDirectory: TEdit;
    btnSelectDirectory: TButton;
    lblModelDirectory: TLabel;
    procedure btnSelectDirectoryClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmModelOrClass: TfrmModelOrClass;

implementation

{$R *.fmx}

procedure TfrmModelOrClass.btnOKClick(Sender: TObject);
begin
  if edtName.Text = '' then
  begin
    Beep;
    MessageDlg('No name has been specified.', TMsgDlgType.mtError,
      [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbNo], 0);
    btnOK.ModalResult := mrNone;
    Exit;
  end;
  if edModelDirectory.Visible and not DirectoryExists(edModelDirectory.Text) then
  begin
    Beep;
    MessageDlg('The specified directory does not exist.', TMsgDlgType.mtError,
      [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbNo], 0);
    btnOK.ModalResult := mrNone;
    Exit;
  end;
  btnOK.ModalResult := mrOK;
end;

procedure TfrmModelOrClass.btnSelectDirectoryClick(Sender: TObject);
var
  Directory: string;
  OldDir: string;
begin
  Directory := edModelDirectory.Text;
  if TDirectory.Exists(Directory) then
  begin
    OldDir := Directory;
//  end
//  else
//  begin
//    OldDir := GetCurrentDir;
  end;
  if SelectDirectory('Model Files Directory', OldDir, Directory) then
  begin
    edModelDirectory.Text := IncludeTrailingPathDelimiter(Directory);
  end;
end;

procedure TfrmModelOrClass.FormResize(Sender: TObject);
var
  APosition: TPosition;
begin
  APosition := btnSelectDirectory.Position;
  APosition.X := edModelDirectory.Position.X + edModelDirectory.Size.Width;
  btnSelectDirectory.Position := APosition;
end;

end.


