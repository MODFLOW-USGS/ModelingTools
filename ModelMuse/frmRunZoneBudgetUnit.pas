unit frmRunZoneBudgetUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CustomExtendedDialogForm, StdCtrls;

type
  TfrmRunZoneBudget = class(TCustomExtendedDialog)
    cbRun: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmRunZoneBudget: TfrmRunZoneBudget;

implementation

{$R *.dfm}

end.
