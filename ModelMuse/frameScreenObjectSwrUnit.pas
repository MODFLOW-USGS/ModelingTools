unit frameScreenObjectSwrUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameScreenObjectNoParamUnit, Grids,
  RbwDataGrid4, StdCtrls, ArgusDataEntry, Buttons, Mask, JvExMask,
  JvSpin, ExtCtrls;

type
  TframeScreenObjectSwr = class(TframeScreenObjectNoParam)
    lblConductanceInterpretation: TLabel;
    comboFormulaInterp: TComboBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frameScreenObjectSwr: TframeScreenObjectSwr;

implementation

{$R *.dfm}

end.
