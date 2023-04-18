unit ModflowFmpAllotmentUnit;

interface

uses
  ModflowFmpBaseClasses, ModflowBoundaryUnit, SysUtils, FormulaManagerInterfaceUnit;

type
  TAllotmentItem = class(TCustomZeroFarmItem)
  private
    function GetAllotment: string;
    procedure SetAllotment(const Value: string);
  protected
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    function BoundaryFormulaCount: integer; override;
  published
    property Allotment: string read GetAllotment write SetAllotment;
  end;

  TAllotmentRecord = record
    StartingTime: double;
    EndingTime: double;
    Allotment: double;
  end;

  TAllotmentArray = array of TAllotmentRecord;

  TAllotmentCollection = class(TCustomFarmCollection)
  private
    FTimeValues: TAllotmentArray;
    function GetAllotmentTimeValues(Index: integer): TAllotmentRecord;
    function GetItems(Index: Integer): TAllotmentItem;
    procedure SetAllotmentTimeValues(Index: integer; const Value: TAllotmentRecord);
    procedure SetItems(Index: Integer; const Value: TAllotmentItem);
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    procedure EvaluateAllotment;
    property AllotmentTimeValues[Index: integer]: TAllotmentRecord
      read GetAllotmentTimeValues write SetAllotmentTimeValues;
    function GetAllotmentTimeValuesFromTime(StartTime: double): TAllotmentRecord;
    property Items[Index: Integer]: TAllotmentItem read GetItems
      write SetItems; default;
    function Add: TAllotmentItem;
  end;


implementation

uses
  frmErrorsAndWarningsUnit, frmGoPhastUnit, RbwParser, PhastModelUnit,
  frmFormulaErrorsUnit;

resourcestring
  StrTimeG = 'Time: %g.';
  StrIncompleteAllotment = 'Incomplete Allotment data';

const
  AllotmentPosition = 0;

{ TAllotmentItem }

function TAllotmentItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

function TAllotmentItem.GetAllotment: string;
begin
  Result := FFormulaObjects[AllotmentPosition].Formula;
  ResetItemObserver(AllotmentPosition);
end;

function TAllotmentItem.GetBoundaryFormula(Index: integer): string;
begin
  if Index = AllotmentPosition then
  begin
    result := Allotment;
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TAllotmentItem.SetAllotment(const Value: string);
begin
  if FFormulaObjects[AllotmentPosition].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, AllotmentPosition, FFormulaObjects[AllotmentPosition]);
  end;
end;

procedure TAllotmentItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  if Index = AllotmentPosition then
  begin
    Allotment := Value;
  end
  else
  begin
    Assert(False);
  end;
end;

{ TAllotmentCollection }

function TAllotmentCollection.Add: TAllotmentItem;
begin
  result := inherited Add as TAllotmentItem;
end;

procedure TAllotmentCollection.EvaluateAllotment;
var
  CurrentRecord: TAllotmentRecord;
  CurrentItem: TAllotmentItem;
  Compiler: TRbwParser;
  PhastModel: TPhastModel;
  Formula: string;
  Expression: TExpression;
  Index: integer;
begin
  PhastModel := Model as TPhastModel;
  SetLength(FTimeValues, Count);
  Compiler := PhastModel.rpThreeDFormulaCompiler;
  for Index := 0 to Count - 1 do
  begin
    CurrentItem := Items[Index] as TAllotmentItem;
    CurrentRecord.StartingTime := CurrentItem.StartTime;
    CurrentRecord.EndingTime := CurrentItem.EndTime;

//    Expression := nil;
    Formula := CurrentItem.Allotment;
    Expression := nil;
    try
      Compiler.Compile(Formula);
      Expression := Compiler.CurrentExpression;
      // only global variables are used so there should be no need
      // to update the variables.
      Expression.Evaluate;
    except on E: ERbwParserError do
      begin
        frmFormulaErrors.AddFormulaError('',
          'Allotment in the Farm Process',
          Formula, E.Message);

        CurrentItem.Allotment := '0.';
        Formula := CurrentItem.Allotment;
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        Expression.Evaluate;
      end;
    end;
    CurrentRecord.Allotment := Expression.DoubleResult;

    FTimeValues[Index] := CurrentRecord;
  end;
end;

function TAllotmentCollection.GetAllotmentTimeValues(
  Index: integer): TAllotmentRecord;
begin
  Assert((Index >= 0) and (Index < Length(FTimeValues)));
  result := FTimeValues[Index];
end;

function TAllotmentCollection.GetAllotmentTimeValuesFromTime(
  StartTime: double): TAllotmentRecord;
var
  Index: integer;
  ErrorMessage: string;
begin
  Assert(Length(FTimeValues) > 0);
  result := FTimeValues[0];
  for Index := 0 to Length(FTimeValues) - 1 do
  begin
    if (FTimeValues[Index].StartingTime <= StartTime) then
    begin
      result := FTimeValues[Index];
      if (FTimeValues[Index].EndingTime > StartTime) then
      begin
        Exit;
      end;
    end;
  end;
  ErrorMessage := Format(StrTimeG, [StartTime]);
  frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel,
    StrIncompleteAllotment, ErrorMessage);
end;

function TAllotmentCollection.GetItems(Index: Integer): TAllotmentItem;
begin
  result := inherited Items[Index] as TAllotmentItem;
end;

class function TAllotmentCollection.ItemClass: TBoundaryItemClass;
begin
  result := TAllotmentItem;
end;

procedure TAllotmentCollection.SetAllotmentTimeValues(Index: integer;
  const Value: TAllotmentRecord);
begin
  Assert((Index >= 0) and (Index < Length(FTimeValues)));
  FTimeValues[Index] := Value;
end;

procedure TAllotmentCollection.SetItems(Index: Integer;
  const Value: TAllotmentItem);
begin
  inherited Items[Index] := Value;
end;

end.
