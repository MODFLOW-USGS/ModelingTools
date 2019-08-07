{ Abstract(@name defines @link(TframeListParameterDefinition).
@link(TframeListParameterDefinition) is used to define the parameters
in packages that have a list of boundary locations such as the Well package.)
@seealso(TframeArrayParameterDefinition).
@author(Richard B. Winston <rbwinst@usgs.gov>)
}
unit frameListParameterDefinitionUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, RbwDataGrid4, StdCtrls, Mask, JvExMask, JvSpin, Buttons,
  ExtCtrls, OrderedCollectionUnit, ModflowParameterUnit;

type
  TParameterColumns = (pcName, pcValue, pcUseZone, pcUseMultiplier);

   {
   @member(dgParametersStateChange is used to turn on or off the use of
   multiplier or zone arrays for a parameter.)
   }
  TframeListParameterDefinition = class(TFrame)
    pnlParameterCount: TPanel;
    lblNumParameters: TLabel;
    btnDelete: TBitBtn;
    seNumberOfParameters: TJvSpinEdit;
    dgParameters: TRbwDataGrid4;
    procedure seNumberOfParametersEnter(Sender: TObject);
    procedure seNumberOfParametersChange(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure dgParametersBeforeDrawCell(Sender: TObject; ACol, ARow: Integer);
    procedure dgParametersSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure dgParametersSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure dgParametersExit(Sender: TObject);
    procedure dgParametersStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure dgParametersEndUpdate(Sender: TObject);
  private
    { Private declarations }
    procedure ClearFirstRow;
  protected
    procedure SetEnabled(Value: boolean); override;
  public
    CurrentParameter: TModflowParameter;
    PriorNumberOfParameters: Integer;
    procedure Loaded; override;

    { Public declarations }
  end;

implementation

{$R *.dfm}

resourcestring
  StrName = 'Name';
  StrValue = 'Value';
  StrUseZone = 'Use Zone';
  StrUseMultiplier = 'Use Multiplier';

procedure TframeListParameterDefinition.btnDeleteClick(Sender: TObject);
begin
  if seNumberOfParameters.AsInteger in [0,1] then
  begin
    CurrentParameter := dgParameters.Objects[0, 1] as TModflowParameter;
    PriorNumberOfParameters := 0;
    seNumberOfParameters.AsInteger := 0;
    dgParameters.Objects[0, 1] := nil;
  end
  else
  begin
    CurrentParameter := dgParameters.Objects[0, dgParameters.SelectedRow] as TModflowParameter;
    dgParameters.Objects[0, dgParameters.SelectedRow] := nil;
    dgParameters.DeleteRow(dgParameters.SelectedRow);
    Dec(PriorNumberOfParameters);
    seNumberOfParameters.AsInteger := seNumberOfParameters.AsInteger - 1;
  end;
end;

procedure TframeListParameterDefinition.ClearFirstRow;
begin
  dgParameters.Cells[0,1] := '';
  dgParameters.Cells[1,1] := '';
  if dgParameters.ColCount > 2 then
  begin
    dgParameters.Checked[2,1] := False;
    dgParameters.Checked[3,1] := False;
  end;
end;

procedure TframeListParameterDefinition.dgParametersBeforeDrawCell(
  Sender: TObject; ACol, ARow: Integer);
var
  Names: TStringList;
  ParamRow: integer;
begin
  if (ARow >= dgParameters.FixedRows)
    and (TParameterColumns(ACol) in [pcName, pcValue]) then
  begin
    if (seNumberOfParameters.AsInteger > 0)
      and (Trim(dgParameters.Cells[ACol, ARow]) = '') then
    begin
      dgParameters.Canvas.Brush.Color := clRed;
    end;
  end;
  if (ARow > dgParameters.FixedRows)
    and (TParameterColumns(ACol) = pcName) then
  begin
    Names := TStringList.Create;
    try
      Names.AddStrings(dgParameters.Cols[ACol]);
      Names.Delete(0);
      ParamRow := Names.IndexOf(dgParameters.Cells[ACol, ARow])+1;
      if ParamRow <> ARow then
      begin
        dgParameters.Canvas.Brush.Color := clRed;
      end;
    finally
      Names.Free;
    end;
  end;

end;

procedure TframeListParameterDefinition.dgParametersEndUpdate(Sender: TObject);
var
  NumParam: Integer;
begin
  if (seNumberOfParameters <> nil) then
  begin
    NumParam := dgParameters.RowCount -1;
    if NumParam = 1 then
    begin
      if (dgParameters.Cells[0,1] = '') and (dgParameters.Cells[1,1] = '') then
      begin
        NumParam := 0;
      end;
    end;
    seNumberOfParameters.AsInteger := NumParam;
    if Assigned(seNumberOfParameters.OnChange) then
    begin
      seNumberOfParameters.OnChange(seNumberOfParameters);
    end;
  end;
end;

procedure TframeListParameterDefinition.dgParametersExit(Sender: TObject);
var
  RowIndex: Integer;
  Parameter: TModflowParameter;
begin
  for RowIndex := 1 to seNumberOfParameters.AsInteger - 1 do
  begin
    Parameter := dgParameters.Objects[0,RowIndex] as TModflowParameter;
    if Parameter <> nil then
    begin
      dgParameters.Cells[0,RowIndex] := Parameter.ParameterName;
    end;
  end;
end;

procedure TframeListParameterDefinition.dgParametersSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  CanSelect := ((ARow <> 1) or (seNumberOfParameters.AsInteger > 0))
    and Enabled;
end;

procedure TframeListParameterDefinition.dgParametersSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
var
  Parameter: TModflowParameter;
  NewValue: double;
  NewStringValue: AnsiString;
begin
  inherited;
  Parameter := dgParameters.Objects[0,ARow] as TModflowParameter;
  if Parameter = nil then
  begin

    if (ARow > 0) and (TParameterColumns(ACol) = pcName) then
    begin
      NewStringValue := AnsiString(Value);
      if string(NewStringValue) <> Value then
      begin
        dgParameters.Cells[ACol,ARow] := string(NewStringValue);
      end;
    end;
    Exit;
  end;
  
  case TParameterColumns(ACol) of
    pcName:
      begin
        Parameter.ParameterName := string(AnsiString(Value));
        if Parameter.ParameterName <> Value then
        begin
          dgParameters.Cells[0,ARow] := Parameter.ParameterName;
        end;
      end;
    pcValue:
      begin
        if not TryStrToFloat(Value, NewValue) then
        begin
          NewValue := 0;
        end;
        Parameter.Value := NewValue;
      end;
  end;
end;

procedure TframeListParameterDefinition.Loaded;
begin
  inherited;
  dgParameters.Cells[Ord(pcName),0] := StrName;
  dgParameters.Cells[Ord(pcValue),0] := StrValue;
  if dgParameters.ColCount > 2 then
  begin
    dgParameters.Cells[Ord(pcUseZone),0] := StrUseZone;
    dgParameters.Cells[Ord(pcUseMultiplier),0] := StrUseMultiplier;

    dgParameters.ColWidths[Ord(pcUseZone)] :=
      dgParameters.ColWidths[Ord(pcUseZone)] + 40;
    dgParameters.ColWidths[Ord(pcUseMultiplier)] :=
      dgParameters.ColWidths[Ord(pcUseMultiplier)] + 40;
  end;

end;

procedure TframeListParameterDefinition.seNumberOfParametersChange(
  Sender: TObject);
begin
  if seNumberOfParameters.AsInteger = 0 then
  begin
    dgParameters.RowCount := 2;
    ClearFirstRow;
  end
  else
  begin
    dgParameters.RowCount := seNumberOfParameters.AsInteger +1;
  end;

  btnDelete.Enabled := seNumberOfParameters.Enabled and
    (seNumberOfParameters.AsInteger > 0);
  dgParameters.Enabled := btnDelete.Enabled;

end;

procedure TframeListParameterDefinition.seNumberOfParametersEnter(
  Sender: TObject);
begin
  PriorNumberOfParameters := seNumberOfParameters.AsInteger;
end;

procedure TframeListParameterDefinition.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);
  if not (csDestroying in ComponentState) then
  begin
    seNumberOfParameters.Enabled := Value;
    seNumberOfParametersChange(seNumberOfParameters);
    dgParameters.Invalidate;
  end;
end;

procedure TframeListParameterDefinition.dgParametersStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
var
  Parameter: TModflowSteadyParameter;
  Par: TModflowParameter;
begin
  inherited;
  Par := dgParameters.Objects[0,ARow] as TModflowParameter;
  if (Par = nil) or not (Par is TModflowSteadyParameter) then
  begin
    Exit;
  end;
  Parameter := TModflowSteadyParameter(Par);

  case TParameterColumns(ACol) of
    pcUseZone:
      begin
        Parameter.UseZone := Value = cbChecked;
      end;
    pcUseMultiplier:
      begin
        Parameter.UseMultiplier := Value = cbChecked;
      end;
  end;
end;

end.
