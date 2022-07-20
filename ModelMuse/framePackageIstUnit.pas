unit framePackageIstUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, frameRowGridUnit;

type
  TIbsRow = (IrName, irSaveBuddget, irBudgetText, irSorption, irDecay, irSaveConc, IrPrintCols, irWidth, IrDigits, irFormat);

  TframePackageIst = class(TframePackage)
    frameRowGrid1: TframeRowGrid;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  framePackageIst: TframePackageIst;

implementation

{$R *.dfm}

end.
