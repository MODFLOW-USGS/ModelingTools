unit frameScreenObjectCondParamUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frameScreenObjectParamUnit, StdCtrls, JvExControls, JvxCheckListBox,
  Buttons, Mask, JvExMask, JvSpin, ExtCtrls, Grids, RbwDataGrid4, ArgusDataEntry;

type
  TframeScreenObjectCondParam = class(TframeScreenObjectParam)
    lblConductanceInterpretation: TLabel;
    comboFormulaInterp: TComboBox;
  private
    { Private declarations }
  public
    function ParamColumnCaption(NameIndex: integer): string; override;
    function ConductanceCaption(DirectCaption: string): string; override;
    { Public declarations }
  end;

implementation

uses
  ModflowBoundaryUnit, GoPhastTypes;

{$R *.dfm}

{ TframeScreenObjectCondParam }

function TframeScreenObjectCondParam.ConductanceCaption(
  DirectCaption: string): string;
var
  FormulaInterp: TFormulaInterpretation;
begin
  if comboFormulaInterp.ItemIndex < 0 then
  begin
    FormulaInterp := fiDirect
  end
  else
  begin
    FormulaInterp := TFormulaInterpretation(comboFormulaInterp.ItemIndex);
  end;
  result := DirectCaption;
  case FormulaInterp of
    fiSpecific: result := Format(StrSPerUnitLength, [result]);
    fiDirect: ;
    fiTotal: result := Format(StrTotalSPerLayer, [LowerCase(result)]);
    else Assert(False);
  end;
end;

function TframeScreenObjectCondParam.ParamColumnCaption(NameIndex: integer): string;
var
  FormulaInterp: TFormulaInterpretation;
begin
  result := inherited ParamColumnCaption(NameIndex);
  if NameIndex <> ConductanceColumn then
  begin
    Exit;
  end;
  if comboFormulaInterp.ItemIndex < 0 then
  begin
    FormulaInterp := fiDirect
  end
  else
  begin
    FormulaInterp := TFormulaInterpretation(comboFormulaInterp.ItemIndex);
  end;
  case FormulaInterp of
    fiSpecific: result := Format(StrSPerUnitLength, [result]);
    fiDirect: ;
    fiTotal: result := Format(StrTotalS, [result]);
    else Assert(False);
  end;
end;

end.
