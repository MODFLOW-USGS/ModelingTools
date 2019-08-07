unit frmHelpVersionUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, StdCtrls,
  Buttons, ExtCtrls;

type
  TfrmHelpVersion = class(TfrmCustomGoPhast)
    rgHelpVersion: TRadioGroup;
    pnl1: TPanel;
    btnClose: TBitBtn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmHelpVersion: TfrmHelpVersion;

implementation

uses
  frmGoPhastUnit;

{$R *.dfm}

procedure TfrmHelpVersion.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  frmGoPhast.HelpFormat := THelpFormat(rgHelpVersion.ItemIndex + 1);
end;

end.
