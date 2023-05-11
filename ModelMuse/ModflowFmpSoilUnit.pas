unit ModflowFmpSoilUnit;

interface

uses
  OrderedCollectionUnit, ModflowBoundaryUnit, SysUtils, Classes,
  ModflowFmpBaseClasses, GoPhastTypes, FormulaManagerInterfaceUnit;

type
  TSoilType = (stSand, stSandyLoam, stSilt, stSiltyClay, stOther);
  TSoilMethod = (smConstant, smInterpolate, smStep, smNearest);

  TLookupItem = class(TCustomZeroFarmItem)
  private
    const
    LookupPosition = 0;
    ReturnPostion  = 1;
  protected
    function BoundaryFormulaCount: integer; override;
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
  public
//    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function IsSame(OtherItem: TOrderedItem): Boolean; override;
  published
    property LookupValue: string index LookupPosition read GetBoundaryFormula write SetBoundaryFormula;
    property ReturnValue: string index ReturnPostion read GetBoundaryFormula write SetBoundaryFormula;
  end;

  TLookUpTable = class(TCustomFarmCollection)
  private
    FMethod: TSoilMethod;
    procedure SetMethod(const Value: TSoilMethod);
    function GetItem(Index: Integer): TLookupItem;
    procedure SetItem(Index: Integer; const Value: TLookupItem);
  protected
    class function ItemClass: TBoundaryItemClass; override;
    function ShouldDeleteItemsWithZeroDuration: Boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]:TLookupItem  read GetItem write SetItem; default;
    function IsSame(OtherTable: TOrderedCollection): Boolean; override;
  published
    property Method: TSoilMethod read FMethod write SetMethod;
  end;

  TSoilItem = class(TCustomZeroFarmItem)
  private
    const
    CapillaryFringePosition =  0;
    ACoeffPosition = 1;
    BCoeffPosition = 2;
    CCoeffPosition = 3;
    DCoeffPosition = 4;
    ECoeffPosition = 5;
    SurfVKPosition = 6;
    var
    FSoilName: string;
    FSoilType: TSoilType;
    FLookUpTable: TLookUpTable;
    procedure SetSoilName(Value: string);
    procedure SetSoilType(const Value: TSoilType);
    procedure SetLookUpTable(const Value: TLookUpTable);
  protected
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    function BoundaryFormulaCount: integer; override;
    // @name checks whether AnotherItem is the same as the current @classname.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure SetIndex(Value: Integer); override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property SoilName: string read FSoilName write SetSoilName;
    property SoilType: TSoilType read FSoilType write SetSoilType;
    property CapillaryFringe: string index CapillaryFringePosition
      read GetBoundaryFormula write SetBoundaryFormula;
    property ACoeff: string index ACoeffPosition read GetBoundaryFormula
      write SetBoundaryFormula;
    property BCoeff: string index BCoeffPosition read GetBoundaryFormula
      write SetBoundaryFormula;
    property CCoeff: string index CCoeffPosition read GetBoundaryFormula
      write SetBoundaryFormula;
    property DCoeff: string index DCoeffPosition read GetBoundaryFormula
      write SetBoundaryFormula;
    property ECoeff: string index ECoeffPosition read GetBoundaryFormula
      write SetBoundaryFormula;
    property SurfVK: string index SurfVKPosition read GetBoundaryFormula
      write SetBoundaryFormula
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
      ;
    property LookUpTable: TLookUpTable read FLookUpTable write SetLookUpTable
    {$IFNDEF OWHMV2}
      stored False
    {$ENDIF}
      ;
  end;

  TSoilRecord = record
    SoilID: integer;
    SoilType: TSoilType;
    CapillaryFringe: double;
    ACoeff: double;
    BCoeff: double;
    CCoeff: double;
    DCoeff: double;
    ECoeff: double;
  end;

  TSoilArray = array of TSoilRecord;

  TSoilCollection = class(TCustomFarmCollection)
  private
    FSoilArray: TSoilArray;
    function GetItems(Index: Integer): TSoilItem;
    procedure SetItems(Index: Integer; const Value: TSoilItem);
  protected
    class function ItemClass: TBoundaryItemClass; override;
    procedure DeleteItemsWithZeroDuration; override;
  public
    property SoilArray: TSoilArray read FSoilArray;
    property Items[Index: Integer]: TSoilItem read GetItems write SetItems; default;
    procedure EvaluateSoils;
  end;

implementation

uses
  PhastModelUnit, ModflowPackageSelectionUnit, RbwParser, frmFormulaErrorsUnit,
  LockedGlobalVariableChangers;

resourcestring
  StrSoilVariable = 'Soil Variable';

{ TSoilItem }

procedure TSoilItem.Assign(Source: TPersistent);
var
  SourceItem: TSoilItem;
begin
  if Source is TSoilItem then
  begin
    SourceItem := TSoilItem(Source);
    SoilName := SourceItem.SoilName;
    SoilType := SourceItem.SoilType;
    LookUpTable := SourceItem.LookUpTable;
  end;
  inherited;
end;

function TSoilItem.BoundaryFormulaCount: integer;
begin
  result := 7
end;

constructor TSoilItem.Create(Collection: TCollection);
begin
  inherited;
  FLookUpTable := TLookUpTable.Create(Model as TCustomModel);
end;

destructor TSoilItem.Destroy;
var
  Unlocker: TDefineGlobalIntegerObject;
begin
  FLookUpTable.Free;
  if (Model <> nil) and (SoilName <> '')  then
  begin
    if ([csLoading, csDestroying] * (Model as TComponent).ComponentState) = [] then
    begin
      Unlocker := TDefineGlobalIntegerObject.Create(Model as TCustomModel, FSoilName, FSoilName,
        StrSoilVariable);
      try
        Unlocker.Locked := False;
      finally
        Unlocker.Free;
      end;
    end;
  end;
  inherited;
end;

//function TSoilItem.GetACoeff: string;
//begin
//  Result := FFormulaObjects[ACoeffPosition].Formula;
//  ResetItemObserver(ACoeffPosition);
//end;
//
//function TSoilItem.GetBCoeff: string;
//begin
//  Result := FFormulaObjects[BCoeffPosition].Formula;
//  ResetItemObserver(BCoeffPosition);
//end;

function TSoilItem.GetBoundaryFormula(Index: integer): string;
begin
  Result := FFormulaObjects[Index].Formula;
  ResetItemObserver(Index);

//  case Index of
//    CapillaryFringePosition:
//      result := CapillaryFringe;
//    ACoeffPosition:
//      result := ACoeff;
//    BCoeffPosition:
//      result := BCoeff;
//    CCoeffPosition:
//      result := CCoeff;
//    DCoeffPosition:
//      result := DCoeff;
//    ECoeffPosition:
//      result := ECoeff;
//    else Assert(False);
//  end;
end;

//function TSoilItem.GetCapillaryFringe: string;
//begin
//  Result := FFormulaObjects[CapillaryFringePosition].Formula;
//  ResetItemObserver(CapillaryFringePosition);
//end;
//
//function TSoilItem.GetCCoeff: string;
//begin
//  Result := FFormulaObjects[CCoeffPosition].Formula;
//  ResetItemObserver(CCoeffPosition);
//end;
//
//function TSoilItem.GetDCoeff: string;
//begin
//  Result := FFormulaObjects[DCoeffPosition].Formula;
//  ResetItemObserver(DCoeffPosition);
//end;
//
//function TSoilItem.GetECoeff: string;
//begin
//  Result := FFormulaObjects[ECoeffPosition].Formula;
//  ResetItemObserver(ECoeffPosition);
//end;

function TSoilItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  OtherItem: TSoilItem;
begin
  Result := (AnotherItem is TSoilItem) and inherited IsSame(AnotherItem);
  if Result then
  begin
    OtherItem := TSoilItem(AnotherItem);
    result := (SoilName = OtherItem.SoilName)
      and (SoilType = OtherItem.SoilType)
      and (LookUpTable.IsSame(OtherItem.LookUpTable));
  end;
end;

procedure TSoilItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  if FFormulaObjects[Index].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, Index, FFormulaObjects[Index]);
  end;
end;

procedure TSoilItem.SetIndex(Value: Integer);
var
  ChangeGlobals: TDefineGlobalIntegerObject;
begin
  if (Model <> nil) and (FSoilName <> '') then
  begin
    ChangeGlobals := TDefineGlobalIntegerObject.Create(Model as TCustomModel, FSoilName, FSoilName,
      StrSoilVariable);
    try
      ChangeGlobals.SetValue(Value+1);
    finally
      ChangeGlobals.Free;
    end;
  end;
  inherited;

end;

procedure TSoilItem.SetLookUpTable(const Value: TLookUpTable);
begin
  FLookUpTable.Assign(Value);
end;

procedure TSoilItem.SetSoilName(Value: string);
var
  ChangeGlobals: TDefineGlobalIntegerObject;
begin
  if (FSoilName <> Value) and (Model <> nil)
    and not (csReading in (Model as TComponent).ComponentState) then
  begin
    Value := GenerateNewName(Value, nil, '_');
  end;
  ChangeGlobals := TDefineGlobalIntegerObject.Create(Model as TCustomModel, FSoilName, Value,
    StrSoilVariable);
  try
    if FSoilName <> Value then
    begin
      if (Model <> nil) and (Value <> '') then
      begin
        ChangeGlobals.Rename;
      end;
      FSoilName := Value;
      InvalidateModel;
    end;
    if (Model <> nil) and (FSoilName <> '') then
    begin
      ChangeGlobals.SetValue(Index+1);
    end;
  finally
    ChangeGlobals.Free;
  end;
end;

procedure TSoilItem.SetSoilType(const Value: TSoilType);
begin
  if FSoilType <> Value then
  begin
    FSoilType := Value;
    InvalidateModel;
  end;
end;

{ TSoilCollection }

procedure TSoilCollection.DeleteItemsWithZeroDuration;
begin
//  inherited;
  // Don't delete based on duration.
end;

procedure TSoilCollection.EvaluateSoils;
var
  CurrentRecord: TSoilRecord;
  Compiler: TRbwParser;
  PhastModel: TPhastModel;
  Formula: string;
  Expression: TExpression;
  Index: integer;
  CurrentItem: TSoilItem;
  FarmProcess: TFarmProcess;
begin
  PhastModel := Model as TPhastModel;
  FarmProcess := PhastModel.ModflowPackages.FarmProcess;
  SetLength(FSoilArray, Count);
  Compiler := PhastModel.rpThreeDFormulaCompiler;
  for Index := 0 to Count - 1 do
  begin
    CurrentItem := Items[Index];
    CurrentRecord.SoilID := Index+1;

    Expression := nil;
    Formula := CurrentItem.CapillaryFringe;
    try
      Compiler.Compile(Formula);
      Expression := Compiler.CurrentExpression;
      // only global variables are used so there should be no need
      // to update the variables.
      Expression.Evaluate;
    except on E: ERbwParserError do
      begin
        frmFormulaErrors.AddFormulaError('', Format(
          'Error in Capillary Fringe for "%s" in the Farm Process', [CurrentItem.SoilName]),
          Formula, E.Message);

        CurrentItem.CapillaryFringe := '0.';
        Formula := CurrentItem.CapillaryFringe;
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        Expression.Evaluate;
      end;
    end;
    CurrentRecord.CapillaryFringe := Expression.DoubleResult;

    CurrentRecord.SoilType := CurrentItem.SoilType;

    if (FarmProcess.CropConsumptiveConcept = cccConcept1)
      and (CurrentItem.SoilType = stOther) then
    begin
      Expression := nil;
      Formula := CurrentItem.ACoeff;
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', Format(
            'Error in A-Coeff for "%s" in the Farm Process', [CurrentItem.SoilName]),
            Formula, E.Message);

          CurrentItem.ACoeff := '0.';
          Formula := CurrentItem.ACoeff;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.ACoeff := Expression.DoubleResult;

      Expression := nil;
      Formula := CurrentItem.BCoeff;
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', Format(
            'Error in B-Coeff for "%s" in the Farm Process', [CurrentItem.SoilName]),
            Formula, E.Message);

          CurrentItem.BCoeff := '0.';
          Formula := CurrentItem.BCoeff;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.BCoeff := Expression.DoubleResult;

      Expression := nil;
      Formula := CurrentItem.CCoeff;
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', Format(
            'Error in C-Coeff for "%s" in the Farm Process', [CurrentItem.SoilName]),
            Formula, E.Message);

          CurrentItem.CCoeff := '0.';
          Formula := CurrentItem.CCoeff;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.CCoeff := Expression.DoubleResult;

      Expression := nil;
      Formula := CurrentItem.DCoeff;
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', Format(
            'Error in D-Coeff for "%s" in the Farm Process', [CurrentItem.SoilName]),
            Formula, E.Message);

          CurrentItem.DCoeff := '0.';
          Formula := CurrentItem.DCoeff;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.DCoeff := Expression.DoubleResult;

      Expression := nil;
      Formula := CurrentItem.ECoeff;
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', Format(
            'Error in E-Coeff for "%s" in the Farm Process', [CurrentItem.SoilName]),
            Formula, E.Message);

          CurrentItem.ECoeff := '0.';
          Formula := CurrentItem.ECoeff;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.ECoeff := Expression.DoubleResult;

    end;

    FSoilArray[Index] := CurrentRecord;
  end;
end;

function TSoilCollection.GetItems(Index: Integer): TSoilItem;
begin
  result := inherited Items[Index] as TSoilItem;
end;

class function TSoilCollection.ItemClass: TBoundaryItemClass;
begin
  result := TSoilItem;
end;

procedure TSoilCollection.SetItems(Index: Integer; const Value: TSoilItem);
begin
  inherited Items[Index] := Value;
end;

{ TLookupItem }

procedure TLookupItem.Assign(Source: TPersistent);
var
  LuSource: TLookupItem;
begin
  if Source is TLookupItem then
  begin
    LuSource := TLookupItem(Source);
    LookupValue := LuSource.LookupValue;
    ReturnValue := LuSource.ReturnValue;
  end
  else
  begin
    inherited;
  end;
end;

function TLookupItem.BoundaryFormulaCount: integer;
begin
  result := 2;
end;

//constructor TLookupItem.Create(Collection: TCollection);
//begin
//  inherited;
////  FStoredLookupValue := TRealStorage.Create(OnInvalidateModel);
////  FStoredReturnValue := TRealStorage.Create(OnInvalidateModel);
//  LookupValue := 0;
//  ReturnValue := 0;
//end;

function TLookupItem.GetBoundaryFormula(Index: integer): string;
begin
  Result := FFormulaObjects[Index].Formula;
  ResetItemObserver(Index);
end;

function TLookupItem.IsSame(OtherItem: TOrderedItem): Boolean;
begin
  result := (OtherItem is TLookupItem) and inherited;

//  (LookupValue = OtherItem.LookupValue)
//    and (ReturnValue = OtherItem.ReturnValue)
end;

procedure TLookupItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  if FFormulaObjects[Index].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, Index, FFormulaObjects[Index]);
  end;
end;

{ TLookUpTable }

//constructor TLookUpTable.Create(Model: TBaseModel);
//begin
//  inherited Create(TLookupItem, Model);
//end;

procedure TLookUpTable.Assign(Source: TPersistent);
begin
  if Source is TLookUpTable then
  begin
    Method := TLookUpTable(Source).Method;
  end;
  inherited;
end;

function TLookUpTable.GetItem(Index: Integer): TLookupItem;
begin
  result := inherited Items[index] as TLookupItem;
end;

function TLookUpTable.IsSame(OtherTable: TOrderedCollection): Boolean;
var
  index: Integer;
begin
  result := (OtherTable is TLookUpTable)
    and (Method = TLookUpTable(OtherTable).Method)
    and inherited IsSame(OtherTable);
end;

class function TLookUpTable.ItemClass: TBoundaryItemClass;
begin
  result := TLookupItem;
end;

procedure TLookUpTable.SetItem(Index: Integer; const Value: TLookupItem);
begin
  inherited Items[index] := Value;
end;

procedure TLookUpTable.SetMethod(const Value: TSoilMethod);
begin
  if FMethod <> Value then
  begin
    FMethod := Value;
    InvalidateModel;
  end;
end;

function TLookUpTable.ShouldDeleteItemsWithZeroDuration: Boolean;
begin
  result := False;
end;

end.
