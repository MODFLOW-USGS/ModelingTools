unit frmConsoleLinesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, StdCtrls,
  Buttons, ExtCtrls;

type
  TfrmConsoleLines = class(TfrmCustomGoPhast)
    memoConsoleLines: TMemo;
    pnlBottom: TPanel;
    btnClose: TBitBtn;
    pnlTop: TPanel;
    lblMessage: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmConsoleLines: TfrmConsoleLines;

implementation

{$R *.dfm}

end.
