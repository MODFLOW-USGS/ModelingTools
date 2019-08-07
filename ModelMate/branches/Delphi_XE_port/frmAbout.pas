unit frmAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, sskutils, Utilities, JvExStdCtrls, JvHtControls;

type
  TFormAbout = class(TForm)
    btnOK: TButton;
    Label1: TLabel;
    lblSSKVer: TLabel;
    Memo1: TMemo;
    JvHTLabel1: TJvHTLabel;
    JvHTLabel2: TJvHTLabel;
    Label2: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.dfm}

procedure TFormAbout.btnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  CenterForm(self);
end;

procedure TFormAbout.FormShow(Sender: TObject);
var
  FullVersion, ShortVersion, Build: string;
begin
  FullVersion := getMyFileVersion;
  ParseVersion(FullVersion,ShortVersion,Build);
  lblSSKVer.Caption := ShortVersion;
end;

end.
