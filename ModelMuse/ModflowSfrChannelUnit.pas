unit ModflowSfrChannelUnit;

interface

uses SysUtils, Classes, RbwParser, OrderedCollectionUnit, ModflowBoundaryUnit,
  FormulaManagerUnit, FormulaManagerInterfaceUnit,
  SubscriptionUnit, GoPhastTypes;

type
  TSfrChannelRecord = record
    ChannelRoughness: double;
    BankRoughness: double;
    StartingTime: double;
    EndingTime: double;
    ChannelRoughnessAnnotation: string;
    BankRoughnessAnnotation: string;
    X: array[0..7] of double;
    Z: array[0..7] of double;
    XAnnotation: array[0..7] of string;
    ZAnnotation: array[0..7] of string;
  end;

  // @name represents a MODFLOW Streamflow Routing boundary for one time interval.
  // @name is stored by @link(TSfrCollection).
  TSfrChannelItem = class(TCustomModflowBoundaryItem)
  private
    FX: array[0..7] of IFormulaObject;
    FZ: array[0..7] of IFormulaObject;
    FChannelRoughness: IFormulaObject;
    FBankRoughness: IFormulaObject;
    procedure SetBankRoughness(const Value: string);
    procedure SetChannelRoughness(const Value: string);
    function GetX(Index: integer): string;
    function GetZ(Index: integer): string;
    procedure SetX(Index: integer; const Value: string);
    procedure SetZ(Index: integer; const Value: string);
    procedure ReadX(Reader: TReader);
    procedure ReadZ(Reader: TReader);
    procedure WriteX(Writer: TWriter);
    procedure WriteZ(Writer: TWriter);
    function GetBankRoughness: string;
    function GetChannelRoughness: string;
    procedure InvalidateBankRoughnessData(Sender: TObject);
    procedure InvalidateChannelRoughnessData(Sender: TObject);
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    // @name checks whether AnotherItem is the same as the current @classname.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure DefineProperties(Filer: TFiler); override;
    function BoundaryFormulaCount: integer; override;
  public
    property X[Index: integer]: string read GetX write SetX;
    property Z[Index: integer]: string read GetZ write SetZ;
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
    procedure AssignGeometry(Sfr: TSfrChannelItem);
  published
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    property ChannelRoughness: string read GetChannelRoughness
      write SetChannelRoughness;
    property BankRoughness: string read GetBankRoughness
      write SetBankRoughness;
  end;

  // @name represents MODFLOW Streamflow Routing boundaries
  // for a series of time intervals.
  TSfrChannelCollection = class(TCustomNonSpatialBoundColl)
  private
    FTimeValues: array of TSfrChannelRecord;
    function GetChannelTimeValues(Index: integer): TSfrChannelRecord;
    procedure SetChannelTimeValues(Index: integer;
      const Value: TSfrChannelRecord);
    function GetItem(Index: Integer): TSfrChannelItem;
    procedure SetItem(Index: Integer; const Value: TSfrChannelItem);
  protected
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
  public
    procedure EvaluateBoundaries(AModel: TBaseModel);
    property ChannelTimeValues[Index: integer]: TSfrChannelRecord
      read GetChannelTimeValues write SetChannelTimeValues;
    function GetChannelTimeValuesFromTime(AModel: TBaseModel;
      StartTime: double): TSfrChannelRecord;
    property Items[Index: Integer]: TSfrChannelItem read GetItem write SetItem; default;
    function GetItemByStartTime(StartTime: double): TSfrChannelItem;
  end;

resourcestring
  ChannelRoughnessError = 'SFR Channel roughness is less than or equal to zero.';
  BankRoughnessError = 'SFR Bank roughness is less than or equal to zero.';
  IDError = 'Object: %0:s; Time: %1:g.';

implementation

uses ScreenObjectUnit, PhastModelUnit,
  ModflowSfrUnit, frmFormulaErrorsUnit, frmErrorsAndWarningsUnit,
  frmGoPhastUnit;

resourcestring
  StrChannelRougnnessF = '(Channel rougnness for the SFR package)';
  StrBankRoughnessFor = '(bank roughness for the SFR package)';
  StrCrossSectionXFor = '(cross section X for the SFR package)';
  StrCrossSectionZFor = '(cross section Z for the SFR package)';
  StrChannelRoughnessNo = 'Channel Roughness not used';

const
  ChannelRoughnessPosition = 0;
  BankRoughnessPosition = 1;

{ TSfrChannelItem }

procedure TSfrChannelItem.Assign(Source: TPersistent);
var
  Sfr: TSfrChannelItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TSfrChannelItem then
  begin
    Sfr := TSfrChannelItem(Source);
    ChannelRoughness := Sfr.ChannelRoughness;
    BankRoughness := Sfr.BankRoughness;
    AssignGeometry(Sfr);
  end;
  inherited;
end;

procedure TSfrChannelItem.AssignGeometry(Sfr: TSfrChannelItem);
var
  Index: Integer;
begin
  for Index := 0 to 7 do
  begin
    X[Index] := Sfr.X[Index];
    Z[Index] := Sfr.Z[Index];
  end;
end;

procedure TSfrChannelItem.InvalidateChannelRoughnessData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if (Sender = nil) or not (Sender as TObserver).UpToDate then
  begin
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil) and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateMfSfrChannelRoughness(self);
      end;
    end;
  end;
end;

procedure TSfrChannelItem.InvalidateBankRoughnessData(Sender: TObject);
var
  ScreenObj: TObject;
  PhastModel: TPhastModel;
begin
  if (Sender = nil) or not (Sender as TObserver).UpToDate then
  begin
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil) and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateMfSfrBankRoughness(self);
      end;
    end;
  end;
end;

procedure TSfrChannelItem.ReadX(Reader: TReader);
var
  Index: integer;
begin
  with Reader do
  begin
    ReadListBegin;
    Index := 0;
    while not EndOfList do
    begin
      X[Index] := ReadString;
      Inc(Index);
    end;
    Assert(Index = 8);
    ReadListEnd;
  end;
end;

procedure TSfrChannelItem.ReadZ(Reader: TReader);
var
  Index: integer;
begin
  with Reader do
  begin
    ReadListBegin;
    Index := 0;
    while not EndOfList do
    begin
      Z[Index] := ReadString;
      Inc(Index);
    end;
    Assert(Index = 8);
    ReadListEnd;
  end;
end;

procedure TSfrChannelItem.RemoveFormulaObjects;
var
  Index: integer;
begin
  for Index := Length(FZ)-1 downto 0 do
  begin
    frmGoPhast.PhastModel.FormulaManager.Remove(FZ[Index],
      GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  end;
  for Index := Length(FX)-1 downto 0 do
  begin
    frmGoPhast.PhastModel.FormulaManager.Remove(FX[Index],
      GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  end;
  frmGoPhast.PhastModel.FormulaManager.Remove(FBankRoughness,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FChannelRoughness,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TSfrChannelItem.WriteX(Writer: TWriter);
var
  Index: integer;
begin
  with Writer do
  begin
    WriteListBegin;
    for Index := 0 to 7 do
    begin
      WriteString(X[Index]);
    end;
    WriteListEnd;
  end;
end;

procedure TSfrChannelItem.WriteZ(Writer: TWriter);
var
  Index: integer;
begin
  with Writer do
  begin
    WriteListBegin;
    for Index := 0 to 7 do
    begin
      WriteString(Z[Index]);
    end;
    WriteListEnd;
  end;
end;

procedure TSfrChannelItem.AssignObserverEvents(Collection: TCollection);
var
  BankRoughnessObserver: TObserver;
  ChannelRoughnessObserver: TObserver;
begin
  BankRoughnessObserver := FObserverList[BankRoughnessPosition];
  BankRoughnessObserver.OnUpToDateSet := InvalidateBankRoughnessData;
  ChannelRoughnessObserver := FObserverList[ChannelRoughnessPosition];
  ChannelRoughnessObserver.OnUpToDateSet := InvalidateChannelRoughnessData;
end;

function TSfrChannelItem.BoundaryFormulaCount: integer;
begin
  result := 2;
  // need to fix this for the case when bank roughness is used.
end;

constructor TSfrChannelItem.Create(Collection: TCollection);
begin
  inherited;
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FX[0], '0.', frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FX[1], '2.', frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FX[2], '4.', frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FX[3], '6.', frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FX[4], '8.', frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FX[5], '10.', frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FX[6], '12.', frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FX[7], '14.', frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FZ[0], '6.', frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FZ[1], '4.5', frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FZ[2], '3.5', frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FZ[3], '0.', frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FZ[4], '0.3', frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FZ[5], '3.5', frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FZ[6], '4.5', frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FZ[7], '6.', frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FChannelRoughness, '3.5E-7',
    frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FBankRoughness, '3.5E-7',
    frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TSfrChannelItem.CreateFormulaObjects;
var
  Index: integer;
begin
  FChannelRoughness := CreateFormulaObject(dso3D);
  FBankRoughness := CreateFormulaObject(dso3D);
  for Index := 0 to Length(FX) - 1 do
  begin
    FX[Index] := CreateFormulaObject(dso3D);
  end;
  for Index := 0 to Length(FZ) - 1 do
  begin
    FZ[Index] := CreateFormulaObject(dso3D);
  end;
end;

procedure TSfrChannelItem.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('X', ReadX, WriteX, True);
  Filer.DefineProperty('Z', ReadZ, WriteZ, True);
end;

destructor TSfrChannelItem.Destroy;
var
  LocalScreenObject: TScreenObject;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) then
  begin
    ChannelRoughness := '0';
    BankRoughness := '0';
  end;
  inherited;
end;

function TSfrChannelItem.GetBankRoughness: string;
begin
  Result := FBankRoughness.Formula;
  ResetItemObserver(BankRoughnessPosition);
end;

function TSfrChannelItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    ChannelRoughnessPosition:
      result := ChannelRoughness;
    BankRoughnessPosition:
      result := BankRoughness;
    else
      begin
        Dec(Index,2);
        if Odd(Index) then
        begin
          Index := index div 2;
          result := Z[Index];
        end
        else
        begin
          Index := index div 2;
          result := X[Index];
        end;
      end;
  end;
end;

function TSfrChannelItem.GetChannelRoughness: string;
begin
  Result := FChannelRoughness.Formula;
  ResetItemObserver(ChannelRoughnessPosition);
end;

procedure TSfrChannelItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FChannelRoughness as TObject then
  begin
    List.Add(FObserverList[ChannelRoughnessPosition]);
  end;
  if Sender = FBankRoughness as TObject then
  begin
    List.Add(FObserverList[BankRoughnessPosition]);
  end;
end;

function TSfrChannelItem.GetX(Index: integer): string;
begin
  Assert((Index >= 0) and  (Index <=7));
  result := FX[Index].Formula;
end;

function TSfrChannelItem.GetZ(Index: integer): string;
begin
  Assert((Index >= 0) and  (Index <=7));
  result := FZ[Index].Formula;
end;

function TSfrChannelItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  SfrChannel: TSfrChannelItem;
  Index: integer;
begin
  result := (AnotherItem is TSfrChannelItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    SfrChannel := TSfrChannelItem(AnotherItem);
    result := (ChannelRoughness = SfrChannel.ChannelRoughness)
      and (BankRoughness = SfrChannel.BankRoughness);
    if result then
    begin
      for Index := 0 to 7 do
      begin
        result := (X[Index] = SfrChannel.X[Index])
          and (Z[Index] = SfrChannel.Z[Index]);
        if not result then
        begin
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TSfrChannelItem.SetBankRoughness(const Value: string);
begin
  if FBankRoughness.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, BankRoughnessPosition, FBankRoughness);
    InvalidateBankRoughnessData(nil);
  end;
end;

procedure TSfrChannelItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    ChannelRoughnessPosition:
      ChannelRoughness := Value;
    BankRoughnessPosition:
      BankRoughness := Value;
    else
      begin
        Dec(Index,2);
        if Odd(Index) then
        begin
          Index := index div 2;
          Z[Index] := Value;
        end
        else
        begin
          Index := index div 2;
          X[Index] := Value;
        end;
      end;
  end;
end;

procedure TSfrChannelItem.SetChannelRoughness(const Value: string);
begin
  if FChannelRoughness.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, ChannelRoughnessPosition, FChannelRoughness);
    InvalidateChannelRoughnessData(nil);
  end;
end;

procedure TSfrChannelItem.SetX(Index: integer; const Value: string);
begin
  Assert((Index >= 0) and  (Index <=7));
  if FX[Index].Formula <> Value then
  begin
    InvalidateModel;
    frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
      FX[Index], Value, frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
      GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  end;
end;

procedure TSfrChannelItem.SetZ(Index: integer; const Value: string);
begin
  Assert((Index >= 0) and  (Index <=7));
  if FZ[Index].Formula <> Value then
  begin
    InvalidateModel;
    frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
      FZ[Index], Value, frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
      GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  end;
end;

{ TSfrChannelCollection }

procedure TSfrChannelCollection.EvaluateBoundaries(AModel: TBaseModel);
var
  CurrentRecord: TSfrChannelRecord;
  CurrentItem: TSfrChannelItem;
  Compiler: TRbwParser;
  PhastModel: TPhastModel;
  SfrBound: TSfrBoundary;
  ICalc: integer;
  Formula: string;
  Expression: TExpression;
  ScrObj: TScreenObject;
  XSIndex: Integer;
  Index: integer;
begin
  SfrBound := BoundaryGroup as TSfrBoundary;
  if (SfrBound.ParamIcalc.IcalcSet * [1,2] = []) then
  begin
    Exit;
  end;
  ScrObj := ScreenObject as TScreenObject;
  PhastModel := Model as TPhastModel;
  SetLength(FTimeValues, Count);
  Compiler := PhastModel.rpThreeDFormulaCompiler;
  for Index := 0 to Count - 1 do
  begin
    CurrentItem := Items[Index];
    CurrentRecord.StartingTime := CurrentItem.StartTime;
    CurrentRecord.EndingTime := CurrentItem.EndTime;
    Assert(ScrObj.ModflowSfrBoundary <> nil);
    ICALC := ScrObj.ModflowSfrBoundary.ParamIcalc.ICalc(CurrentItem.StartTime);

    if ICALC in [1,2] then
    begin
      Formula := CurrentItem.ChannelRoughness;
      CurrentRecord.ChannelRoughnessAnnotation := Format(StrAssignedBy0sWit,
        [ScrObj.Name, Formula]);
    end
    else
    begin
      Formula := '0';
      CurrentRecord.ChannelRoughnessAnnotation := StrChannelRoughnessNo;
    end;
    Expression := nil;
    try
      Compiler.Compile(Formula);
      Expression := Compiler.CurrentExpression;
      // only global variables are used so there should be no need
      // to update the variables.
      Expression.Evaluate;
    except on E: ERbwParserError do
      begin
        frmFormulaErrors.AddFormulaError(ScrObj.Name,
          StrChannelRougnnessF,
          Formula, E.Message);

        CurrentItem.ChannelRoughness := '0.';
        Formula := CurrentItem.ChannelRoughness;
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        Expression.Evaluate;
      end;
    end;
    CurrentRecord.ChannelRoughness := Expression.DoubleResult;
    if CurrentRecord.ChannelRoughness <= 0 then
    begin
      frmErrorsAndWarnings.AddError(AModel, ChannelRoughnessError,
        Format(IDError, [ScrObj.Name, CurrentItem.StartTime]), ScrObj);

    end;
    if ICALC = 2 then
    begin
      Formula := CurrentItem.BankRoughness;
      CurrentRecord.BankRoughnessAnnotation := Format(StrAssignedBy0sWit,
        [ScrObj.Name, Formula]);
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError(ScrObj.Name,
            StrBankRoughnessFor,
            Formula, E.Message);

          CurrentItem.BankRoughness := '0.';
          Formula := CurrentItem.BankRoughness;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.BankRoughness := Expression.DoubleResult;
      if CurrentRecord.BankRoughness <= 0 then
      begin
        frmErrorsAndWarnings.AddError(AModel, BankRoughnessError,
          Format(IDError, [ScrObj.Name, CurrentItem.StartTime]), ScrObj);
      end;

      for XSIndex := 0 to 7 do
      begin
        Formula := CurrentItem.X[XSIndex];
        CurrentRecord.XAnnotation[XSIndex] := Format(StrAssignedBy0sWit,
        [ScrObj.Name, Formula]);
        try
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          // only global variables are used so there should be no need
          // to update the variables.
          Expression.Evaluate;
        except on E: ERbwParserError do
          begin
            frmFormulaErrors.AddFormulaError(ScrObj.Name,
              StrCrossSectionXFor,
              Formula, E.Message);

            CurrentItem.X[XSIndex] := '0.';
            Formula := CurrentItem.X[XSIndex];
            Compiler.Compile(Formula);
            Expression := Compiler.CurrentExpression;
            Expression.Evaluate;
          end;
        end;
        CurrentRecord.X[XSIndex] := Expression.DoubleResult;

        Formula := CurrentItem.Z[XSIndex];
        CurrentRecord.ZAnnotation[XSIndex] := Format(StrAssignedBy0sWit,
          [ScrObj.Name, Formula]);
        try
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          // only global variables are used so there should be no need
          // to update the variables.
          Expression.Evaluate;
        except on E: ERbwParserError do
          begin
            frmFormulaErrors.AddFormulaError(ScrObj.Name,
              StrCrossSectionZFor,
              Formula, E.Message);

            CurrentItem.Z[XSIndex] := '0.';
            Formula := CurrentItem.Z[XSIndex];
            Compiler.Compile(Formula);
            Expression := Compiler.CurrentExpression;
            Expression.Evaluate;
          end;
        end;
        CurrentRecord.Z[XSIndex] := Expression.DoubleResult;

      end;
    end;
    FTimeValues[Index] := CurrentRecord;
  end;
end;

function TSfrChannelCollection.GetChannelTimeValues(
  Index: integer): TSfrChannelRecord;
begin
  Assert((Index >= 0) and (Index < Length(FTimeValues)));
  result := FTimeValues[Index];
end;

function TSfrChannelCollection.GetChannelTimeValuesFromTime(AModel: TBaseModel;
  StartTime: double): TSfrChannelRecord;
var
  Index: Integer;
  ScreenObjectName: string;
  ErrorMessage: string;
begin
  if Length(FTimeValues) > 0 then
  begin
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
  end;
  ScreenObjectName := (ScreenObject as TScreenObject).Name;
  ErrorMessage := Format(IDError, [ScreenObjectName, StartTime]);
//  ErrorMessage := 'Object = ' + ScreenObjectName
//    + '; Time = ' + FloatToStr(StartTime);
  frmErrorsAndWarnings.AddError(AModel, StrIncompleteSFRData, ErrorMessage,
    ScreenObject as TScreenObject);

end;

function TSfrChannelCollection.GetItem(Index: Integer): TSfrChannelItem;
begin
  result := inherited Items[Index] as TSfrChannelItem;
end;

function TSfrChannelCollection.GetItemByStartTime(
  StartTime: double): TSfrChannelItem;
var
  Index: Integer;
  Item: TSfrChannelItem;
begin
  result := nil;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TSfrChannelItem;
    if Item.StartTime = StartTime then
    begin
      result := Item;
      Exit;
    end;
    if Item.StartTime < StartTime then
    begin
      result := Item;
    end;
    if Item.EndTime > StartTime then
    begin
      Exit;
    end;
  end;
end;

class function TSfrChannelCollection.ItemClass: TBoundaryItemClass;
begin
  result := TSfrChannelItem;
end;

procedure TSfrChannelCollection.SetChannelTimeValues(Index: integer;
  const Value: TSfrChannelRecord);
begin
  Assert((Index >= 0) and (Index < Length(FTimeValues)));
  FTimeValues[Index] := Value;
end;

procedure TSfrChannelCollection.SetItem(Index: Integer;
  const Value: TSfrChannelItem);
begin
  inherited Items[Index] := Value;
end;

end.
