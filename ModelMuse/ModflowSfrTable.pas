unit ModflowSfrTable;

interface

uses SysUtils, Classes, RbwParser, GoPhastTypes, OrderedCollectionUnit,
  ModflowBoundaryUnit, FormulaManagerUnit, SubscriptionUnit,
  OrderedCollectionInterfaceUnit;

type
  TSfrFlowTableItemRecord = record
    Flow: double;
    Depth: double;
    Width: double;
    FlowAnnotation: string;
    DepthAnnotation: string;
    WidthAnnotation: string;
  end;

  TSfrFlowTableArray = array of TSfrFlowTableItemRecord;

  TSfrFlowTableRecord = record
    SfrFlowTableArray: TSfrFlowTableArray;
    StartingTime: double;
    EndingTime: double;
  end;

  TSfrTable = class;

  TSfrTableRowItem = class(TFormulaOrderedItem)
  private
    const
      FlowPosition = 0;
      DepthPosition = 1;
      WidthPosition = 2;
    var
    FObserverList: TList;
    FFormulaList: TList;
    FWidth: TFormulaObject;
    FFlow: TFormulaObject;
    FDepth: TFormulaObject;
    FFlowObserver: TObserver;
    FWidthObserver: TObserver;
    FDepthObserver: TObserver;
    procedure SetDepth(const Value: string);
    procedure SetFlow(const Value: string);
    procedure SetWidth(const Value: string);
    function GetDepth: string;
    function GetFlow: string;
    function GetWidth: string;
    function SfrTable: TSfrTable;
    function CreateFormulaObject: TFormulaObject;
    procedure ResetItemObserver(FormulaObject: TFormulaObject);
  protected
    procedure GetPropertyObserver(Sender: TObject; List: TList);
    procedure RemoveSubscription(Sender: TObject; const AName: string);
    procedure RestoreSubscription(Sender: TObject; const AName: string);
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function GetObserver(Index: Integer): TObserver; override;
    function GetScreenObject: TObject; override;
  public
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Flow: string read GetFlow write SetFlow;
    property Depth: string read GetDepth write SetDepth;
    property Width: string read GetWidth write SetWidth;
  end;

  TSfrTable = class(TCustomObjectOrderedCollection)
  private
//    FScreenObject: TObject;
    function GetItems(Index: integer): TSfrTableRowItem;
    procedure SetItems(Index: integer; const Value: TSfrTableRowItem);
  public
    constructor Create(Model: ICustomModelInterfaceForTOrderedCollection; ScreenObject: TObject);
    property Items[Index: integer]: TSfrTableRowItem read GetItems
      write SetItems; default;
  end;

  // @name represents a MODFLOW Streamflow Routing boundary for one time interval.
  // @name is stored by @link(TSfrCollection).
  TSfrTablelItem = class(TCustomModflowBoundaryItem)
  private
    FSfrTable: TSfrTable;
    procedure SetSfrTable(const Value: TSfrTable);
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
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
    procedure AssignTable(Source: TPersistent);
  published
    property SfrTable: TSfrTable read FSfrTable write SetSfrTable;
  end;

  // @name represents MODFLOW Streamflow Routing boundaries
  // for a series of time intervals.
  TSfrTableCollection = class(TCustomNonSpatialBoundColl)
  private
    FTimeValues: array of TSfrFlowTableRecord;
    function GetTableTimeValues(Index: integer): TSfrFlowTableRecord;
    procedure SetTableTimeValues(Index: integer;
      const Value: TSfrFlowTableRecord);
    function GetItem(Index: Integer): TSfrTablelItem;
    procedure SetItem(Index: Integer; const Value: TSfrTablelItem);
  protected
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
  public
    procedure EvaluateBoundaries;
    property TableTimeValues[Index: integer]: TSfrFlowTableRecord
      read GetTableTimeValues write SetTableTimeValues;
    function GetRecordForTime(StartTime: double): TSfrFlowTableRecord;
    property Items[Index: Integer]: TSfrTablelItem read GetItem write SetItem; default;
  end;

procedure TableRowRemoveSubscription(Sender: TObject; Subject: TObject;
  const AName: string);

procedure TableRowRestoreSubscription(Sender: TObject; Subject: TObject;
  const AName: string);

implementation

uses Contnrs, ScreenObjectUnit, PhastModelUnit,
  ModflowSfrUnit, frmFormulaErrorsUnit, frmErrorsAndWarningsUnit,
  frmGoPhastUnit, ModflowSfrChannelUnit;

resourcestring
  StrFlowTableFlowFor = '(flow table flow for the SFR package)';
  StrFlowTableDepthFo = '(flow table depth for the SFR package)';
  StrFlowTableWidthFo = '(flow table width for the SFR package)';

//const
//  FlowPosition = 0;
//  DepthPosition = 1;
//  WidthPosition = 2;


{ TSfrTablelItem }

procedure TSfrTablelItem.Assign(Source: TPersistent);
begin
  AssignTable(Source);
  inherited;
end;


procedure TSfrTablelItem.AssignObserverEvents(Collection: TCollection);
begin
  // do nothing
end;

function TSfrTablelItem.BoundaryFormulaCount: integer;
begin
  if (SfrTable <> nil) and (SfrTable.Count = 0) then
  begin
    result := 0;
  end
  else
  begin
    result := 3;
  end;
end;

constructor TSfrTablelItem.Create(Collection: TCollection);
begin
  inherited;
  FSfrTable := TSfrTable.Create(Model as TCustomModel, ScreenObject);
end;

procedure TSfrTablelItem.CreateFormulaObjects;
begin
  // do nothing
end;

destructor TSfrTablelItem.Destroy;
begin
  FSfrTable.Free;
  inherited;
end;

procedure TSfrTablelItem.AssignTable(Source: TPersistent);
var
  Sfr: TSfrTablelItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TSfrTablelItem then
  begin
    Sfr := TSfrTablelItem(Source);
    SfrTable := Sfr.SfrTable;
  end;
end;

function TSfrTablelItem.GetBoundaryFormula(Index: integer): string;
var
  ItemIndex: integer;
  Item: TSfrTableRowItem;
begin
  ItemIndex := Index div 3;
  if ItemIndex >= SfrTable.Count then
  begin
    result := '';
    Exit;
  end;
  Item := SfrTable.Items[ItemIndex];
  Index := Index mod 3;

  case Index of
    TSfrTableRowItem.FlowPosition:
      result := Item.Flow;
    TSfrTableRowItem.DepthPosition:
      result := Item.Depth;
    TSfrTableRowItem.WidthPosition:
      result := Item.Width;
    else
      Assert(False);
  end;
end;


procedure TSfrTablelItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  // do nothing
end;

function TSfrTablelItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  SfrTableItem: TSfrTablelItem;
begin
  result := (AnotherItem is TSfrTablelItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    SfrTableItem := TSfrTablelItem(AnotherItem);
    result := SfrTable.IsSame(SfrTableItem.SfrTable);
  end;
end;

procedure TSfrTablelItem.RemoveFormulaObjects;
begin
  // do nothing
end;

procedure TSfrTablelItem.SetBoundaryFormula(Index: integer;
  const Value: string);
var
  ItemIndex: integer;
  Item: TSfrTableRowItem;
begin
  if SfrTable = nil then
  begin
    Exit;
  end;
  ItemIndex := Index div 3;
  Item := SfrTable.Items[ItemIndex];
  Index := Index mod 3;

  case Index of
    TSfrTableRowItem.FlowPosition:
      Item.Flow := Value;
    TSfrTableRowItem.DepthPosition:
      Item.Depth := Value;
    TSfrTableRowItem.WidthPosition:
      Item.Width := Value;
    else
      Assert(False);
  end;
end;


procedure TSfrTablelItem.SetSfrTable(const Value: TSfrTable);
begin
  FSfrTable.Assign(Value);
end;

{ TSfrTableCollection }

procedure TSfrTableCollection.EvaluateBoundaries;
var
  CurrentRecord: TSfrFlowTableItemRecord;
  CurrentItem: TSfrTablelItem;
  Compiler: TRbwParser;
  PhastModel: TPhastModel;
  SfrBound: TSfrBoundary;
  Formula: string;
  Expression: TExpression;
  ScrObj: TScreenObject;
  Index: integer;
  RowIndex: integer;
  CurrentRowItem: TSfrTableRowItem;
begin
  SfrBound := BoundaryGroup as TSfrBoundary;
  if not (4 in SfrBound.ParamIcalc.IcalcSet) then
  begin
    Exit;
  end;
  ScrObj := ScreenObject as TScreenObject;
  PhastModel := Model as TPhastModel;
  SetLength(FTimeValues, Count);
  Compiler := PhastModel.rpThreeDFormulaCompiler;
  for Index := 0 to Count - 1 do
  begin
    CurrentItem := Items[Index] as TSfrTablelItem;

    SetLength(FTimeValues[Index].SfrFlowTableArray, CurrentItem.SfrTable.Count);
    for RowIndex := 0 to CurrentItem.SfrTable.Count - 1 do
    begin
      CurrentRowItem := CurrentItem.SfrTable.Items[RowIndex];
      Expression := nil;
      Formula := CurrentRowItem.Flow;
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
            StrFlowTableFlowFor,
            Formula, E.Message);

          CurrentRowItem.Flow := '0.';
          Formula := CurrentRowItem.Flow;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.Flow := Expression.DoubleResult;

      Formula := CurrentRowItem.Depth;
      CurrentRecord.DepthAnnotation := Format(StrAssignedBy0sWit,
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
            StrFlowTableDepthFo,
            Formula, E.Message);

          CurrentRowItem.Depth := '0.';
          Formula := CurrentRowItem.Depth;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.Depth := Expression.DoubleResult;

      Formula := CurrentRowItem.Width;
      CurrentRecord.WidthAnnotation := Format(StrAssignedBy0sWit,
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
            StrFlowTableWidthFo,
            Formula, E.Message);

          CurrentRowItem.Width := '0.';
          Formula := CurrentRowItem.Width;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.Width := Expression.DoubleResult;

      FTimeValues[Index].SfrFlowTableArray[RowIndex] := CurrentRecord;
      FTimeValues[Index].StartingTime := CurrentItem.StartTime;
      FTimeValues[Index].EndingTime := CurrentItem.EndTime;
    end;
  end;
end;

function TSfrTableCollection.GetItem(Index: Integer): TSfrTablelItem;
begin
  result := inherited Items[index] as TSfrTablelItem;
end;

function TSfrTableCollection.GetRecordForTime(
  StartTime: double): TSfrFlowTableRecord;
var
  Index: integer;
  ScreenObjectName: string;
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
        exit;
      end;
    end;
  end;
  ScreenObjectName := (ScreenObject as TScreenObject).Name;
  ErrorMessage := Format(IDError, [ScreenObjectName, StartTime]);
//  ErrorMessage := 'Object = ' + ScreenObjectName
//    + '; Time = ' + FloatToStr(StartTime);
  frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel,
    StrIncompleteSFRData, ErrorMessage, ScreenObject);
end;

function TSfrTableCollection.GetTableTimeValues(
  Index: integer): TSfrFlowTableRecord;
begin
  Assert((Index >= 0) and (Index < Length(FTimeValues)));
  result := FTimeValues[Index];
end;

class function TSfrTableCollection.ItemClass: TBoundaryItemClass;
begin
  result := TSfrTablelItem;
end;

procedure TSfrTableCollection.SetItem(Index: Integer;
  const Value: TSfrTablelItem);
begin
  inherited Items[index] := Value;
end;

procedure TSfrTableCollection.SetTableTimeValues(Index: integer;
  const Value: TSfrFlowTableRecord);
begin
  Assert((Index >= 0) and (Index < Length(FTimeValues)));
  FTimeValues[Index] := Value;
end;

{ TSfrTableRowItem }

procedure TSfrTableRowItem.Assign(Source: TPersistent);
var
  SfrTableRowItem: TSfrTableRowItem;
begin
  // if Assign is updated, update IsSame too.
  if (Source is TSfrTableRowItem) then
  begin
    SfrTableRowItem := TSfrTableRowItem(Source);
    Flow := SfrTableRowItem.Flow;
    Depth := SfrTableRowItem.Depth;
    Width := SfrTableRowItem.Width;
  end;
  inherited;
end;

procedure TableRowRemoveSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TSfrTableRowItem).RemoveSubscription(Sender, AName);
end;

procedure TableRowRestoreSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TSfrTableRowItem).RestoreSubscription(Sender, AName);
end;

function TSfrTableRowItem.CreateFormulaObject: TFormulaObject;
begin
  result := frmGoPhast.PhastModel.FormulaManager.Add;
  result.Parser := frmGoPhast.PhastModel.rpTopFormulaCompiler;
  result.AddSubscriptionEvents(TableRowRemoveSubscription,
  TableRowRestoreSubscription, self);
end;

constructor TSfrTableRowItem.Create(Collection: TCollection);
var
  ScreenObject: TScreenObject;
begin
  inherited;
  FFlowObserver := TObserver.Create(nil);
  FWidthObserver := TObserver.Create(nil);
  FDepthObserver := TObserver.Create(nil);

  FObserverList := TObjectList.Create;

  FObserverList.Add(FFlowObserver);
  FObserverList.Add(FDepthObserver);
  FObserverList.Add(FWidthObserver);

  FFlow := CreateFormulaObject;
  FDepth := CreateFormulaObject;
  FWidth := CreateFormulaObject;

  FFormulaList := TList.Create;
  FFormulaList.Add(FFlow);
  FFormulaList.Add(FDepth);
  FFormulaList.Add(FWidth);

  ScreenObject := SfrTable.ScreenObject as TScreenObject;
  if (ScreenObject <> nil) and ScreenObject.CanInvalidateModel then
  begin
    ScreenObject.TalksTo(FFlowObserver);
    ScreenObject.TalksTo(FDepthObserver);
    ScreenObject.TalksTo(FWidthObserver);
  end;

  OnRemoveSubscription := TableRowRemoveSubscription;
  OnRestoreSubscription := TableRowRestoreSubscription;

  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FFlow, '0.', frmGoPhast.PhastModel.rpTopFormulaCompiler,
    TableRowRemoveSubscription, TableRowRestoreSubscription, self);
  FFormulaList[FlowPosition] := FFlow;
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FDepth, '0.', frmGoPhast.PhastModel.rpTopFormulaCompiler,
    TableRowRemoveSubscription, TableRowRestoreSubscription, self);
  FFormulaList[DepthPosition] := FDepth;
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FWidth, '0.', frmGoPhast.PhastModel.rpTopFormulaCompiler,
    TableRowRemoveSubscription, TableRowRestoreSubscription, self);
  FFormulaList[WidthPosition] := FWidth;
end;

destructor TSfrTableRowItem.Destroy;
begin
  Flow := '0';
  Depth := '0';
  Width := '0';
  frmGoPhast.PhastModel.FormulaManager.Remove(FWidth,
    TableRowRemoveSubscription, TableRowRestoreSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FDepth,
    TableRowRemoveSubscription, TableRowRestoreSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FFlow,
    TableRowRemoveSubscription, TableRowRestoreSubscription, self);
  FFormulaList.Free;
  FObserverList.Free;
  inherited;
end;

function TSfrTableRowItem.GetDepth: string;
begin
  Result := FDepth.Formula;
  ResetItemObserver(FDepth);
end;

function TSfrTableRowItem.GetFlow: string;
begin
  Result := FFlow.Formula;
  ResetItemObserver(FFlow);
end;

function TSfrTableRowItem.GetObserver(Index: Integer): TObserver;
begin
  result := FObserverList[Index];
end;

procedure TSfrTableRowItem.GetPropertyObserver(Sender: TObject; List: TList);
var
  Index: integer;
begin
  for Index := 0 to FFormulaList.Count - 1 do
  begin
    if FFormulaList[Index] = Sender then
    begin
      List.Add(FObserverList[Index])
    end;
  end;
end;

function TSfrTableRowItem.GetScreenObject: TObject;
begin
  result := SfrTable.ScreenObject;
end;

function TSfrTableRowItem.GetWidth: string;
begin
  Result := FWidth.Formula;
  ResetItemObserver(FWidth);
end;

function TSfrTableRowItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  SfrTableRowItem: TSfrTableRowItem;
begin
  result := (AnotherItem is TSfrTableRowItem);
  if result then
  begin
    SfrTableRowItem := TSfrTableRowItem(AnotherItem);
    result := (Flow = SfrTableRowItem.Flow)
      and (Depth = SfrTableRowItem.Depth)
      and (Width = SfrTableRowItem.Width);
  end;
end;

procedure TSfrTableRowItem.RemoveSubscription(Sender: TObject;
  const AName: string);
var
  Observer: TObserver;
  DS: TObserver;
  Observers: TList;
  ObserverIndex: Integer;
begin
  Observers := TList.Create;
  try
    GetPropertyObserver(Sender, Observers);
    for ObserverIndex := 0 to Observers.Count - 1 do
    begin
      Observer:= Observers[ObserverIndex];
      DS := frmGoPhast.PhastModel.GetObserverByName(AName);
      DS.StopsTalkingTo(Observer);
    end;
  finally
    Observers.Free;
  end;
end;

procedure TSfrTableRowItem.ResetItemObserver(FormulaObject: TFormulaObject);
var
  List: TList;
  Observer: TObserver;
  ObserverIndex: Integer;
begin
  List := TList.Create;
  try
    GetPropertyObserver(FormulaObject, List);
    for ObserverIndex := 0 to List.Count - 1 do
    begin
      Observer := List[ObserverIndex];
      Observer.UpToDate := True;
    end;
  finally
    List.Free;
  end;
end;

procedure TSfrTableRowItem.RestoreSubscription(Sender: TObject;
  const AName: string);
var
  Observer: TObserver;
  DS: TObserver;
  Observers: TList;
  ObserverIndex: Integer;
begin
  Observers := TList.Create;
  try
    GetPropertyObserver(Sender, Observers);
    for ObserverIndex := 0 to Observers.Count - 1 do
    begin
      Observer := Observers[ObserverIndex];
      DS := frmGoPhast.PhastModel.GetObserverByName(AName);
      DS.TalksTo(Observer);
      Observer.UpToDate := False;
    end;
  finally
    Observers.Free;
  end;
end;

procedure TSfrTableRowItem.SetDepth(const Value: string);
begin
  UpdateFormulaBlocks(Value, DepthPosition, FDepth);
end;

procedure TSfrTableRowItem.SetFlow(const Value: string);
begin
  UpdateFormulaBlocks(Value, FlowPosition, FFlow);
end;

procedure TSfrTableRowItem.SetWidth(const Value: string);
begin
  UpdateFormulaBlocks(Value, WidthPosition, FWidth);
end;

function TSfrTableRowItem.SfrTable: TSfrTable;
begin
  result := Collection as TSfrTable;
end;

//procedure TSfrTableRowItem.UpdateFormula(Value: string;  Observer: TObserver;
//  var FormulaObject: TFormulaObject);
//var
//  ParentModel: TPhastModel;
//  Compiler: TRbwParser;
//begin
//  if FormulaObject.Formula <> Value then
//  begin
//    ParentModel := Model as TPhastModel;
//    if ParentModel <> nil then
//    begin
//      Compiler := ParentModel.rpThreeDFormulaCompiler;
//      UpdateFormulaDependencies(FormulaObject.Formula, Value, Observer, Compiler);
//    end;
//    InvalidateModel;
//    frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
//      FormulaObject, Value, frmGoPhast.PhastModel.rpTopFormulaCompiler,
//      TableRowRemoveSubscription, TableRowRestoreSubscription, self);
//  end;
//end;

//procedure TSfrTableRowItem.UpdateFormulaDependencies(OldFormula: string;
//  var NewFormula: string; Observer: TObserver; Compiler: TRbwParser);
//var
//  OldUses: TStringList;
//  NewUses: TStringList;
//  Position: Integer;
//  DS: TObserver;
//  ParentScreenObject: TScreenObject;
//  Index: integer;
//  procedure CompileFormula(var AFormula: string;
//    UsesList: TStringList);
//  begin
//    if AFormula <> '' then
//    begin
//      try
//        Compiler.Compile(AFormula);
//        UsesList.Assign(Compiler.CurrentExpression.VariablesUsed);
//      except on E: ERbwParserError do
//        begin
//        end;
//      end;
//    end;
//  end;
//begin
//  OldFormula := Trim(OldFormula);
//  NewFormula := Trim(NewFormula);
//  if OldFormula = NewFormula then
//  begin
//    Exit;
//  end;
//  ParentScreenObject := SfrTable.FScreenObject as TScreenObject;
//  if (ParentScreenObject = nil)
//    or not ParentScreenObject.CanInvalidateModel then
//  begin
//    Exit;
//  end;
//  OldUses := TStringList.Create;
//  NewUses := TStringList.Create;
//  try
//    CompileFormula(OldFormula, OldUses);
//    CompileFormula(NewFormula, NewUses);
//    for Index := OldUses.Count - 1 downto 0 do
//    begin
//      Position := NewUses.IndexOf(OldUses[Index]);
//      if Position >= 0 then
//      begin
//        OldUses.Delete(Index);
//        NewUses.Delete(Position);
//      end;
//    end;
//    for Index := 0 to OldUses.Count - 1 do
//    begin
//      DS := frmGoPhast.PhastModel.GetObserverByName(OldUses[Index]);
//      Assert(DS <> nil);
//      DS.StopsTalkingTo(Observer);
//    end;
//    for Index := 0 to NewUses.Count - 1 do
//    begin
//      DS := frmGoPhast.PhastModel.GetObserverByName(NewUses[Index]);
//      Assert(DS <> nil);
//      DS.TalksTo(Observer);
//    end;
//  finally
//    NewUses.Free;
//    OldUses.Free;
//  end;
//end;

{ TSfrTable }

constructor TSfrTable.Create(Model: ICustomModelInterfaceForTOrderedCollection; ScreenObject: TObject);
begin
  inherited Create(TSfrTableRowItem, Model, ScreenObject);
//  FScreenObject := ScreenObject;
end;

function TSfrTable.GetItems(Index: integer): TSfrTableRowItem;
begin
  result := inherited Items[Index] as TSfrTableRowItem;
end;

procedure TSfrTable.SetItems(Index: integer;
  const Value: TSfrTableRowItem);
begin
  inherited Items[Index] := Value;
end;

end.
