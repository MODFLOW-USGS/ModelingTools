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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmImportWarnings: TfrmImportWarnings;

implementation

{$R *.dfm}

end.
