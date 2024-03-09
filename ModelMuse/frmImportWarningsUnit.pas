unit frmImportWarningsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ExtCtrls;

type
  TfrmImportWarnings = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnClose: TBitBtn;
    memoWarnings: TMemo;
    tmr1: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tmr1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmImportWarnings: TfrmImportWarnings = nil;

implementation

{$R *.dfm}

procedure TfrmImportWarnings.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
//  Release;
end;

procedure TfrmImportWarnings.tmr1Timer(Sender: TObject);
begin
  inherited;
  tmr1.Enabled := False;
  BringToFront;
end;

end.
