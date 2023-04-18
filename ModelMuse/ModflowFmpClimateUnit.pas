unit ModflowFmpClimateUnit;

interface

uses
  OrderedCollectionUnit, ModflowBoundaryUnit, SysUtils, Classes,
  ModflowFmpBaseClasses, FormulaManagerInterfaceUnit;

type
  TClimateItem = class(TCustomZeroFarmItem)
  private
    function GetETref: string;
    function GetMaxT: string;
    function GetMinT: string;
    function GetPrecip: string;
    procedure SetETref(const Value: string);
    procedure SetMaxT(const Value: string);
    procedure SetMinT(const Value: string);
    procedure SetPrecip(const Value: string);
  protected
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    function BoundaryFormulaCount: integer; override;
  published
    property MaxT: string read GetMaxT write SetMaxT;
    property MinT: string read GetMinT write SetMinT;
    property Precip: string read GetPrecip write SetPrecip;
    property ETref: string read GetETref write SetETref;
  end;

  TClimateRecord = record
    StartingTime: double;
    EndingTime: double;
    MaxT: double;
    MinT: double;
    Precip: double;
    ETref: double;
  end;

  TClimateArray = array of TClimateRecord;

  TClimateCollection = class(TCustomFarmCollection)
  private
    FTimeValues: TClimateArray;
    function GetClimateTimeValues(Index: integer): TClimateRecord;
    function GetItems(Index: Integer): TClimateItem;
    procedure SetClimateTimeValues(Index: integer; const Value: TClimateRecord);
    procedure SetItems(Index: Integer; const Value: TClimateItem);
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    procedure EvaluateClimate;
    property ClimateTimeValues[Index: integer]: TClimateRecord
      read GetClimateTimeValues write SetClimateTimeValues;
    function GetClimateTimeValuesFromTime(StartTime: double): TClimateRecord;
    property Items[Index: Integer]: TClimateItem read GetItems
      write SetItems; default;
  end;

implementation

uses
  RbwParser, PhastModelUnit, frmFormulaErrorsUnit, frmErrorsAndWarningsUnit,
  frmGoPhastUnit;

resourcestring
  StrMaximumTemperature = 'Maximum Temperature in the Farm Process';
  StrMinimumTemperature = 'Minimum Temperature in the Farm Process';
  StrPrecipiationInThe = 'Precipiation in the Farm Process';
  StrReferenceEvapotrans = 'Reference Evapotranspiration in the Farm Process';
  IDError = 'Time: %g.';
  StrIncompleteClimateD = 'Incomplete Climate data';

const
  MaxTPostion = 0;
  MinTPostion = 1;
  PrecipPostion = 2;
  ETrefPostion = 3;

{ TClimateItem }

function TClimateItem.BoundaryFormulaCount: integer;
begin
  result := 4
end;

function TClimateItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    MaxTPostion:
      result := MaxT;
    MinTPostion:
      result := MinT;
    PrecipPostion:
      result := Precip;
    ETrefPostion:
      result := ETref;
    else Assert(False);
  end;
end;

function TClimateItem.GetETref: string;
begin
  Result := FFormulaObjects[ETrefPostion].Formula;
  ResetItemObserver(ETrefPostion);
end;

function TClimateItem.GetMaxT: string;
begin
  Result := FFormulaObjects[MaxTPostion].Formula;
  ResetItemObserver(MaxTPostion);
end;

function TClimateItem.GetMinT: string;
begin
  Result := FFormulaObjects[MinTPostion].Formula;
  ResetItemObserver(MinTPostion);
end;

function TClimateItem.GetPrecip: string;
begin
  Result := FFormulaObjects[PrecipPostion].Formula;
  ResetItemObserver(PrecipPostion);
end;

procedure TClimateItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    MaxTPostion:
      MaxT := Value;
    MinTPostion:
      MinT := Value;
    PrecipPostion:
      Precip := Value;
    ETrefPostion:
      ETref := Value;
    else Assert(False);
  end;
end;

procedure TClimateItem.SetETref(const Value: string);
begin
  if FFormulaObjects[ETrefPostion].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, ETrefPostion, FFormulaObjects[ETrefPostion]);
  end;
end;

procedure TClimateItem.SetMaxT(const Value: string);
begin
  if FFormulaObjects[MaxTPostion].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, MaxTPostion, FFormulaObjects[MaxTPostion]);
  end;
end;

procedure TClimateItem.SetMinT(const Value: string);
begin
  if FFormulaObjects[MinTPostion].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, MinTPostion, FFormulaObjects[MinTPostion]);
  end;
end;

procedure TClimateItem.SetPrecip(const Value: string);
begin
  if FFormulaObjects[PrecipPostion].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, PrecipPostion, FFormulaObjects[PrecipPostion]);
  end;
end;

{ TClimateCollection }

procedure TClimateCollection.EvaluateClimate;
var
  CurrentRecord: TClimateRecord;
  CurrentItem: TClimateItem;
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
    CurrentItem := Items[Index] as TClimateItem;
    CurrentRecord.StartingTime := CurrentItem.StartTime;
    CurrentRecord.EndingTime := CurrentItem.EndTime;

    Expression := nil;
    Formula := CurrentItem.MaxT;
    try
      Compiler.Compile(Formula);
      Expression := Compiler.CurrentExpression;
      // only global variables are used so there should be no need
      // to update the variables.
      Expression.Evaluate;
    except on E: ERbwParserError do
      begin
        frmFormulaErrors.AddFormulaError('',
          StrMaximumTemperature,
          Formula, E.Message);

        CurrentItem.MaxT := '0.';
        Formula := CurrentItem.MaxT;
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        Expression.Evaluate;
      end;
    end;
    CurrentRecord.MaxT := Expression.DoubleResult;

    Expression := nil;
    Formula := CurrentItem.MinT;
    try
      Compiler.Compile(Formula);
      Expression := Compiler.CurrentExpression;
      // only global variables are used so there should be no need
      // to update the variables.
      Expression.Evaluate;
    except on E: ERbwParserError do
      begin
        frmFormulaErrors.AddFormulaError('',
          StrMinimumTemperature,
          Formula, E.Message);

        CurrentItem.MinT := '0.';
        Formula := CurrentItem.MinT;
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        Expression.Evaluate;
      end;
    end;
    CurrentRecord.MinT := Expression.DoubleResult;

    Expression := nil;
    Formula := CurrentItem.Precip;
    try
      Compiler.Compile(Formula);
      Expression := Compiler.CurrentExpression;
      // only global variables are used so there should be no need
      // to update the variables.
      Expression.Evaluate;
    except on E: ERbwParserError do
      begin
        frmFormulaErrors.AddFormulaError('',
          StrPrecipiationInThe,
          Formula, E.Message);

        CurrentItem.Precip := '0.';
        Formula := CurrentItem.Precip;
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        Expression.Evaluate;
      end;
    end;
    CurrentRecord.Precip := Expression.DoubleResult;

    Expression := nil;
    Formula := CurrentItem.ETref;
    try
      Compiler.Compile(Formula);
      Expression := Compiler.CurrentExpression;
      // only global variables are used so there should be no need
      // to update the variables.
      Expression.Evaluate;
    except on E: ERbwParserError do
      begin
        frmFormulaErrors.AddFormulaError('',
          StrReferenceEvapotrans,
          Formula, E.Message);

        CurrentItem.ETref := '0.';
        Formula := CurrentItem.ETref;
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        Expression.Evaluate;
      end;
    end;
    CurrentRecord.ETref := Expression.DoubleResult;

    FTimeValues[Index] := CurrentRecord;
  end;
end;

function TClimateCollection.GetClimateTimeValues(
  Index: integer): TClimateRecord;
begin
  Assert((Index >= 0) and (Index < Length(FTimeValues)));
  result := FTimeValues[Index];
end;

function TClimateCollection.GetClimateTimeValuesFromTime(
  StartTime: double): TClimateRecord;
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
  ErrorMessage := Format(IDError, [StartTime]);
  frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel,
    StrIncompleteClimateD, ErrorMessage);
end;

function TClimateCollection.GetItems(Index: Integer): TClimateItem;
begin
  result := inherited Items[Index] as TClimateItem;
end;

class function TClimateCollection.ItemClass: TBoundaryItemClass;
begin
  result := TClimateItem;
end;

procedure TClimateCollection.SetClimateTimeValues(Index: integer;
  const Value: TClimateRecord);
begin
  Assert((Index >= 0) and (Index < Length(FTimeValues)));
  FTimeValues[Index] := Value;
end;

procedure TClimateCollection.SetItems(Index: Integer;
  const Value: TClimateItem);
begin
  inherited Items[Index] := Value;
end;

end.
