unit frmAboutUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  VersInfo, ASLink, StdCtrls, ExtCtrls, Buttons;

type
  TfrmAbout = class(TForm)
    ImageLogo: TImage;
    lblName: TLabel;
    ASLinkEmail: TASLink;
    Label1: TLabel;
    lblVersion: TLabel;
    ASLinkWebPage: TASLink;
    VersionInfo1: TVersionInfo;
    btnOK: TBitBtn;
    Memo1: TMemo;
    memoDisclaimer: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

uses
  DisclaimerTextUnit;

{$R *.DFM}

procedure TfrmAbout.FormCreate(Sender: TObject);
var
  FileCheck: array[0..255] of char;
begin
  GetModuleFileName(HInstance, Filecheck, 255);
  VersionInfo1.FileName := String(Filecheck);
  lblVersion.Caption := VersionInfo1.FileVersion;
  memoDisclaimer.Lines.Add(DisclaimerString);
end;

procedure TfrmAbout.btnOKClick(Sender: TObject);
begin
  Close;
end;

end.
