unit frmNewVideoUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Buttons;

type
  TfrmNewVideos = class(TfrmCustomGoPhast)
    lblNewVideo: TLabel;
    memoNewVideos: TMemo;
    pnlBottom: TPanel;
    btnVideoPage: TButton;
    btnClose: TBitBtn;
    procedure btnVideoPageClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmNewVideos: TfrmNewVideos;

implementation

uses
  frmGoPhastUnit, RbwInternetUtilities;

{$R *.dfm}

procedure TfrmNewVideos.btnVideoPageClick(Sender: TObject);
var
  Browser: string;
begin
  inherited;
  Browser := '';
  LaunchURL(Browser, VideoUrl);
end;

end.
