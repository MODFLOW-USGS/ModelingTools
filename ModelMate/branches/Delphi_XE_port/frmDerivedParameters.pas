unit frmDerivedParameters;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, RbwDataGrid4, ModelMateClassesUnit, GlobalData, StdCtrls,
  Buttons, Math, GlobalBasicData, GlobalTypesUnit, Utilities;

type
  TFormDerivedParameters = class(TForm)
    dgDerParams: TRbwDataGrid4;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure dgDerParamsButtonClick(Sender: TObject; ACol, ARow: Integer);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormDerivedParameters: TFormDerivedParameters;
  ParamSetLocal: TParamSet;
  DerParsChanged: boolean;

implementation

{$R *.dfm}

procedure TFormDerivedParameters.btnOKClick(Sender: TObject);
var
  I: integer;
  IATderived: integer;
  Derived: boolean;
begin
  IATderived := ParAttPos(patDerived);
  if ParamSetLocal.Count > 0 then
    begin
      for I := 0 to ParamSetLocal.Count -1 do
        begin
          Derived := StrToBoolean(ParamSetLocal.Items[I].AllAtts.Items[IATderived].Text);
          if Derived <> dgDerParams.Checked[1,I+1] then
            begin
              DerParsChanged := True;
              ParamSetLocal.Items[I].AllAtts.Items[IATderived].Text := BooleanToYesOrNo(dgDerParams.Checked[1,I+1]);
            end;
          if ParamSetLocal.Items[I].DerivEqn <> dgDerParams.Cells[2,I+1] then
            begin
              DerParsChanged := True;
              ParamSetLocal.Items[I].DerivEqn := dgDerParams.Cells[2,I+1];
            end;
        end;
    end;
  if DerParsChanged then
    begin
      ParamSetCurrent.Assign(ParamSetLocal);
      ProjChanged := True;
    end;
end;

procedure TFormDerivedParameters.dgDerParamsButtonClick(Sender: TObject; ACol,
  ARow: Integer);
//var
//  ModRes: integer;
begin
// TODO 2 : Call Equation Builder here,
// and then populate dgDerParams.Cells[ACol,ARow]
//  ModRes := FormFormula.ShowModal;
end;

procedure TFormDerivedParameters.FormResize(Sender: TObject);
begin
  dgDerParams.ColWidths[2] := self.ClientWidth - dgDerParams.ColWidths[0]
                              - dgDerParams.ColWidths[1] - 25;
end;

procedure TFormDerivedParameters.FormShow(Sender: TObject);
var
  I, IATderived, J: integer;
begin
  IATderived := ParAttPos(patDerived);
  ParamSetLocal.Assign(ParamSetCurrent);
  dgDerParams.ColWidths[2] := self.ClientWidth - dgDerParams.ColWidths[0]
                              - dgDerParams.ColWidths[1] - 25;
  dgDerParams.Cells[0,0] := 'Parameter Name';
  dgDerParams.Cells[1,0] := 'Derived';
  dgDerParams.Cells[2,0] := 'Equation';
  dgDerParams.RowCount := Max(2,PCurrent.ParamSet.Count+1);
  // Clear data rows.
  for I := 1 to dgDerParams.RowCount - 1 do
    for J := 0 to 2 do  // for each of 3 columns.
      begin
        dgDerParams.Cells[J,I] := '';
      end;
  // Populate table.
  if ParamSetCurrent.Count > 0 then
    begin
      for I := 0 to PCurrent.ParamSet.Count -1 do
        begin
          dgDerParams.Cells[0,I+1] := ParamSetCurrent.Items[I].Name;
          dgDerParams.Checked[1,I+1] := StrToBoolean(ParamSetCurrent.Items[I].AllAtts.Items[IATderived].Text);
          dgDerParams.Cells[2,I+1] := ParamSetCurrent.Items[I].DerivEqn;
        end;
    end;
  DerParsChanged := False;
end;

initialization
  ParamSetLocal := TParamSet.Create;

finalization
  ParamSetLocal.Free;

end.
