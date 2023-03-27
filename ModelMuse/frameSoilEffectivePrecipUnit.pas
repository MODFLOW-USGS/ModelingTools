unit frameSoilEffectivePrecipUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameFormulaGridUnit, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Grids, RbwDataGrid4, Vcl.Mask, JvExMask, JvSpin, Vcl.Buttons;

type
  TframeSoilEffectivePrecip = class(TframeFormulaGrid)
    comboInterpolation: TComboBox;
    lblMethod: TLabel;
    lblSoil: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frameSoilEffectivePrecip: TframeSoilEffectivePrecip;

implementation

{$R *.dfm}

end.
