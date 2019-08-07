unit frmRunPhastUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CustomExtendedDialogForm;

type
  TfrmRunPhast = class(TCustomExtendedDialog)
    cbRun: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmRunPhast: TfrmRunPhast;

implementation

{$R *.dfm}

end.
