unit frmBudgetPrecisionQueryUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, StdCtrls,
  ExtCtrls, Buttons;

type
  TfrmBudgetPrecisionQuery = class(TfrmCustomGoPhast)
    rgBudgetPrecision: TRadioGroup;
    pnlBottom: TPanel;
    btnClose: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

//var
//  frmBudgetPrecisionQuery: TfrmBudgetPrecisionQuery;

implementation

{$R *.dfm}

end.
