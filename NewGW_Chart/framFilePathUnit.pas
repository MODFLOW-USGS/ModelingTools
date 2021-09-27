unit framFilePathUnit;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TframFilePath = class(TFrame)
    edFilePath: TEdit;
    lblFileType: TLabel;
    OpenDialogPath: TOpenDialog;
    btnBrowse: TButton;
    procedure btnBrowseClick(Sender: TObject);
    procedure edFilePathChange(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure SetEnabled(Value: Boolean); override;
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TframFilePath.btnBrowseClick(Sender: TObject);
var
  Dir : string;
begin
  Dir := GetCurrentDir;
  try
    if FileExists(edFilePath.Text) then
    begin
      OpenDialogPath.FileName := edFilePath.Text;
    end;
    If OpenDialogPath.Execute then
    begin
      edFilePath.Text := OpenDialogPath.FileName;
    end;
  finally
    SetCurrentDir(Dir);
  end;
  edFilePathChange(Sender);
end;

procedure TframFilePath.edFilePathChange(Sender: TObject);
begin
  if not Enabled then
  begin
    edFilePath.Color := clBtnFace;
  end
  else if FileExists(edFilePath.Text) then
  begin
    edFilePath.Color := clWindow;
  end
  else
  begin
    edFilePath.Color := clRed;
  end;
end;

procedure TframFilePath.SetEnabled(Value: Boolean);
begin
  inherited;
  edFilePath.Enabled := Value;
  btnBrowse.Enabled := Value;
  edFilePathChange(nil);
end;

end.
