unit UcodeSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  GlobalData;

type
  TfrmUcodeSettings = class(TForm)
    Label1: TLabel;
    StatusBar1: TStatusBar;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmUcodeSettings: TfrmUcodeSettings;

implementation

{$R *.dfm}

procedure TfrmUcodeSettings.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  UcodeSetWinOpen := False;
  Destroy;
end;

procedure TfrmUcodeSettings.FormShow(Sender: TObject);
begin
  UcodeSetWinOpen := True;
end;

end.
