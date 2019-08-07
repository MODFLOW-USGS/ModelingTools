unit frmWait;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GlobalData, StdCtrls;

type
  TFormWait = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormWait: TFormWait;

implementation

{$R *.dfm}

procedure TFormWait.FormActivate(Sender: TObject);
begin
  Button1.Visible := False;
  EShowCompleted.SetEvent;
end;

procedure TFormWait.FormShow(Sender: TObject);
begin
  //EShowCompleted.SetEvent;
end;

end.
