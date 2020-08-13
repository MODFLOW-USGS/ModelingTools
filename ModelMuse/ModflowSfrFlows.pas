unit ModflowSfrFlows;

interface

uses SysUtils, Classes, RbwParser, OrderedCollectionUnit, ModflowCellUnit,
  ModflowBoundaryUnit, FormulaManagerUnit, SubscriptionUnit;

type
  TSfrSegmentFlowRecord = record
    Flow: double;
    Precipitation: double;
    Evapotranspiration: double;
    Runnoff: double;
    StartingTime: double;
    EndingTime: double;
    FlowAnnotation: string;
    PrecipitationAnnotation: string;
    EvapotranspirationAnnotation: string;
    RunnoffAnnotation: string;
  end;

  // @name represents a MODFLOW Streamflow Routing boundary for one time interval.
  // @name is stored by @link(TSfrCollection).
  TSfrSegmentFlowItem = class(TCustomModflowBoundaryItem)
  private
    FFlow: TFormulaObject;
    FPrecipitation: TFormulaObject;
    FEvapotranspiration: TFormulaObject;
    FRunnoff: TFormulaObject;
    procedure SetPrecipitation(const Value: string);
    procedure SetFlow(const Value: string);
    procedure SetEvapotranspiration(const Value: string);
    procedure SetRunnoff(const Value: string);
    function GetEvapotranspiration: string;
    function GetFlow: string;
    function GetPrecipitation: string;
    function GetRunnoff: string;
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
    function BoundaryFormulaCount: integer; override;
  public
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
  published
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    property Flow: string read GetFlow write SetFlow;
    property Precipitation: string read GetPrecipitation write SetPrecipitation;
    property Evapotranspiration: string read GetEvapotranspiration
      write SetEvapotranspiration;
    property Runnoff: string read GetRunnoff write SetRunnoff;
  end;

  // @name represents MODFLOW Streamflow Routing boundaries
  // for a series of time intervals.
  TSfrSegmentFlowCollection = class(TCustomNonSpatialBoundColl)
  private
    FTimeValues: array of TSfrSegmentFlowRecord;
    function GetFlowTimeValues(Index: integer): TSfrSegmentFlowRecord;
    procedure SetFlowTimeValues(Index: integer;
      const Value: TSfrSegmentFlowRecord);
  protected
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
  public
    procedure EvaluateBoundaries;
    property FlowTimeValues[Index: integer]: TSfrSegmentFlowRecord
      read GetFlowTimeValues write SetFlowTimeValues;
    function GetFlowValuesFromTime(StartTime: double): TSfrSegmentFlowRecord;
    function GetItemByStartTime(StartTime: double): TSfrSegmentFlowItem;
  end;

implementation

uses Contnrs, DataSetUnit, ScreenObjectUnit, ModflowTimeUnit, PhastModelUnit,
  ModflowSfrUnit, frmFormulaErrorsUnit, frmErrorsAndWarningsUnit, GoPhastTypes, 
  frmGoPhastUnit, ModflowSfrChannelUnit;

resourcestring
  StrFlowForTheSFRPa = '(flow for the SFR package)';
  StrPrecipitationForT = '(precipitation for the SFR package)';
  StrEvaptotranspiration = '(evaptotranspiration for the SFR package)';
  StrRunoffForTheSFR = '(runoff for the SFR package)';

const
  FlowPosition = 0;
  PrecipitationPosition = 1;
  EvapotranspirationPosition = 2;
  RunnoffPosition = 3;

{ TSfrSegmentFlowItem }

procedure TSfrSegmentFlowItem.Assign(Source: TPersistent);
var
  Sfr: TSfrSegmentFlowItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TSfrSegmentFlowItem then
  begin
    Sfr := TSfrSegmentFlowItem(Source);
    Flow := Sfr.Flow;
    Precipitation := Sfr.Precipitation;
    Evapotranspiration := Sfr.Evapotranspiration;
    Runnoff := Sfr.Runnoff;
  end;
  inherited;
end;

procedure TSfrSegmentFlowItem.AssignObserverEvents(Collection: TCollection);
begin
// do nothing
end;

function TSfrSegmentFlowItem.BoundaryFormulaCount: integer;
begin
  result := 4;
end;

constructor TSfrSegmentFlowItem.Create(Collection: TCollection);
begin
  inherited;
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FFlow, '0.', frmGoPhast.PhastModel.rpTopFormulaCompiler,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FPrecipitation, '0.', frmGoPhast.PhastModel.rpTopFormulaCompiler,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FEvapotranspiration, '0.', frmGoPhast.PhastModel.rpTopFormulaCompiler,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FRunnoff, '0.', frmGoPhast.PhastModel.rpTopFormulaCompiler,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TSfrSegmentFlowItem.CreateFormulaObjects;
begin
  FFlow := CreateFormulaObject(dso3D);
  FPrecipitation := CreateFormulaObject(dso3D);
  FEvapotranspiration := CreateFormulaObject(dso3D);
  FRunnoff := CreateFormulaObject(dso3D);
end;

destructor TSfrSegmentFlowItem.Destroy;
var
  Index: integer;
begin
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    BoundaryFormula[Index] := '0';
  end;
  inherited;
end;

function TSfrSegmentFlowItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    FlowPosition:
      result := Flow;
    PrecipitationPosition:
      result := Precipitation;
    EvapotranspirationPosition:
      result := Evapotranspiration;
    RunnoffPosition:
      result := Runnoff;
    else
      Assert(False);
  end;
end;

function TSfrSegmentFlowItem.GetEvapotranspiration: string;
begin
  Result := FEvapotranspiration.Formula;
  ResetItemObserver(EvapotranspirationPosition);
end;

function TSfrSegmentFlowItem.GetFlow: string;
begin
  Result := FFlow.Formula;
  ResetItemObserver(FlowPosition);
end;

function TSfrSegmentFlowItem.GetPrecipitation: string;
begin
  Result := FPrecipitation.Formula;
  ResetItemObserver(PrecipitationPosition);
end;

procedure TSfrSegmentFlowItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FFlow then
  begin
    List.Add(FObserverList[FlowPosition]);
  end;
  if Sender = FPrecipitation then
  begin
    List.Add(FObserverList[PrecipitationPosition]);
  end;
  if Sender = FEvapotranspiration then
  begin
    List.Add(FObserverList[EvapotranspirationPosition]);
  end;
  if Sender = FRunnoff then
  begin
    List.Add(FObserverList[RunnoffPosition]);
  end;
end;

function TSfrSegmentFlowItem.GetRunnoff: string;
begin
  Result := FRunnoff.Formula;
  ResetItemObserver(RunnoffPosition);
end;

function TSfrSegmentFlowItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  SfrChannel: TSfrSegmentFlowItem;
begin
  result := (AnotherItem is TSfrSegmentFlowItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    SfrChannel := TSfrSegmentFlowItem(AnotherItem);
    result := (Flow = SfrChannel.Flow)
      and (Precipitation = SfrChannel.Precipitation)
      and (Evapotranspiration = SfrChannel.Evapotranspiration)
      and (Runnoff = SfrChannel.Runnoff)
  end;
end;

procedure TSfrSegmentFlowItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FRunnoff,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FEvapotranspiration,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FPrecipitation,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FFlow,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TSfrSegmentFlowItem.SetPrecipitation(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FPrecipitation.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, PrecipitationPosition, FPrecipitation);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateMfSfrPrecipitation(self);
      end;
    end;
  end;
end;

procedure TSfrSegmentFlowItem.SetRunnoff(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FRunnoff.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, RunnoffPosition, FRunnoff);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateMfSfrRunoff(self);
      end;
    end;
  end;
end;

procedure TSfrSegmentFlowItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    FlowPosition:
      Flow := Value;
    PrecipitationPosition:
      Precipitation := Value;
    EvapotranspirationPosition:
      Evapotranspiration := Value;
    RunnoffPosition:
      Runnoff := Value;
    else
      Assert(False);
  end;
end;

procedure TSfrSegmentFlowItem.SetEvapotranspiration(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FEvapotranspiration.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, EvapotranspirationPosition, FEvapotranspiration);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateMfSfrEvapotranspiration(self);
      end;
    end;
  end;
end;

procedure TSfrSegmentFlowItem.SetFlow(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FFlow.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, FlowPosition, FFlow);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateMfSfrFlow(self);
      end;
    end;
  end;
end;

{ TSfrSegmentFlowCollection }

procedure TSfrSegmentFlowCollection.EvaluateBoundaries;
var
  CurrentRecord: TSfrSegmentFlowRecord;
  CurrentItem: TSfrSegmentFlowItem;
  Compiler: TRbwParser;
  PhastModel: TPhastModel;
  Formula: string;
  Expression: TExpression;
  ScrObj: TScreenObject;
  Index: integer;
begin
  ScrObj := ScreenObject as TScreenObject;
  PhastModel := Model as TPhastModel;
  SetLength(FTimeValues, Count);
  Compiler := PhastModel.rpThreeDFormulaCompiler;
  for Index := 0 to Count - 1 do
  begin
    CurrentItem := Items[Index] as TSfrSegmentFlowItem;
    CurrentRecord.StartingTime := CurrentItem.StartTime;
    CurrentRecord.EndingTime := CurrentItem.EndTime;

    Expression := nil;
    Formula := CurrentItem.Flow;
    CurrentRecord.FlowAnnotation := Format(StrAssignedBy0sWit,
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
          StrFlowForTheSFRPa,
          Formula, E.Message);

        CurrentItem.Flow := '0.';
        Formula := CurrentItem.Flow;
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        Expression.Evaluate;
      end;
    end;
    CurrentRecord.Flow := Expression.DoubleResult;

    Formula := CurrentItem.Precipitation;
    CurrentRecord.PrecipitationAnnotation := Format(StrAssignedBy0sWit,
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
          StrPrecipitationForT,
          Formula, E.Message);

        CurrentItem.Precipitation := '0.';
        Formula := CurrentItem.Precipitation;
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        Expression.Evaluate;
      end;
    end;
    CurrentRecord.Precipitation := Expression.DoubleResult;

    Formula := CurrentItem.Evapotranspiration;
    CurrentRecord.EvapotranspirationAnnotation := Format(StrAssignedBy0sWit,
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
          StrEvaptotranspiration,
          Formula, E.Message);

        CurrentItem.Evapotranspiration := '0.';
        Formula := CurrentItem.Evapotranspiration;
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        Expression.Evaluate;
      end;
    end;
    CurrentRecord.Evapotranspiration := Expression.DoubleResult;

    Formula := CurrentItem.Runnoff;
    CurrentRecord.RunnoffAnnotation := Format(StrAssignedBy0sWit,
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
          StrRunoffForTheSFR,
          Formula, E.Message);

        CurrentItem.Runnoff := '0.';
        Formula := CurrentItem.Runnoff;
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        Expression.Evaluate;
      end;
    end;
    CurrentRecord.Runnoff := Expression.DoubleResult;

    FTimeValues[Index] := CurrentRecord;
  end;
end;

function TSfrSegmentFlowCollection.GetFlowTimeValues(
  Index: integer): TSfrSegmentFlowRecord;
begin
  Assert((Index >= 0) and (Index < Length(FTimeValues)));
  result := FTimeValues[Index];
end;

function TSfrSegmentFlowCollection.GetFlowValuesFromTime(
  StartTime: double): TSfrSegmentFlowRecord;
var
  Index: Integer;
  FoundResult: boolean;
  ScreenObjectName: string;
  ErrorMessage: string;
begin
  FoundResult := False;
  Assert(Length(FTimeValues) > 0);
  result := FTimeValues[0];
  for Index := 0 to Length(FTimeValues) - 1 do
  begin
    if FTimeValues[Index].StartingTime = StartTime then
    begin
      result := FTimeValues[Index];
      Exit;
    end;
    if FTimeValues[Index].StartingTime < StartTime then
    begin
      result := FTimeValues[Index];
      FoundResult := True;
    end;
    if FTimeValues[Index].EndingTime > StartTime then
    begin
      Exit;
    end;
//    if (FTimeValues[Index].StartingTime <= StartTime)
//      and (FTimeValues[Index].EndingTime > StartTime) then
//    begin
//      result := FTimeValues[Index];
//      Exit;
//    end;
  end;
  if not FoundResult then
  begin
    ScreenObjectName := (ScreenObject as TScreenObject).Name;
    ErrorMessage := Format(IDError, [ScreenObjectName, StartTime]);
//    ErrorMessage := 'Object = ' + ScreenObjectName
//      + '; Time = ' + FloatToStr(StartTime);
    frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel,
      StrIncompleteSFRData, ErrorMessage, ScreenObject);
  end;
end;

function TSfrSegmentFlowCollection.GetItemByStartTime(
  StartTime: double): TSfrSegmentFlowItem;
var
  Index: Integer;
  Item: TSfrSegmentFlowItem;
begin
  result := nil;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TSfrSegmentFlowItem;
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

class function TSfrSegmentFlowCollection.ItemClass: TBoundaryItemClass;
begin
  result := TSfrSegmentFlowItem;
end;

procedure TSfrSegmentFlowCollection.SetFlowTimeValues(Index: integer;
  const Value: TSfrSegmentFlowRecord);
begin
  Assert((Index >= 0) and (Index < Length(FTimeValues)));
  FTimeValues[Index] := Value;
end;

end.
