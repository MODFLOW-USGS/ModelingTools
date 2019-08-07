unit frmUpdatePathsUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation;

type
  TfrmUpdatePaths = class(TForm)
    lblModelNewDirectory: TLabel;
    edNewModelDirectory: TEdit;
    btnSelectNewDirectory: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    lblModelOldDirectory: TLabel;
    edOldModelDirectory: TEdit;
    btnSelectOldDirectory: TButton;
    procedure btnSelectOldDirectoryClick(Sender: TObject);
    procedure btnSelectNewDirectoryClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmUpdatePaths: TfrmUpdatePaths;

implementation

uses
  System.IOUtils;

{$R *.fmx}

procedure TfrmUpdatePaths.btnSelectNewDirectoryClick(Sender: TObject);
var
  Directory: string;
  PriorDir: string;
begin
  Directory := edNewModelDirectory.Text;
  if TDirectory.Exists(Directory) then
  begin
    PriorDir := Directory;
  end;
  if SelectDirectory('New Model Files Directory', PriorDir, Directory) then
  begin
    edNewModelDirectory.Text := IncludeTrailingPathDelimiter(Directory);
  end;
end;

procedure TfrmUpdatePaths.btnSelectOldDirectoryClick(Sender: TObject);
var
  Directory: string;
  PriorDir: string;
begin
  Directory := edOldModelDirectory.Text;
  if TDirectory.Exists(Directory) then
  begin
    PriorDir := Directory;
  end;
  if SelectDirectory('Previous Model Files Directory', PriorDir, Directory) then
  begin
    edOldModelDirectory.Text := IncludeTrailingPathDelimiter(Directory);
  end;
end;

end.
