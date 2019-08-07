unit SutraGenTransBoundUnit;

interface

uses
  System.Classes, ModflowBoundaryUnit, FormulaManagerUnit, GoPhastTypes,
  OrderedCollectionUnit, RbwParser, SutraBoundaryUnit,
  System.Generics.Collections;

type
  TSutraGenTransportItem = class(TCustomBoundaryItem)
  private
    // PBG11
    FLowerUFormula: TFormulaObject;
    // QPBG11
    FLowerFlowUFormula: TFormulaObject;
    // PBG21
    FHigherUFormula: TFormulaObject;
    // QPBG21
    FHigherFlowUFormula: TFormulaObject;
    FUsed: Boolean;
    FUsedFormulaObject: TFormulaObject;
    function GetHigherFlowUFormula: string;
    function GetHigherUFormula: string;
    function GetLowerFlowUFormula: string;
    function GetLowerUFormula: string;
//    function GetUInFormula: string;
//    function GetUoutFormula: string;
    procedure SetUsed(const Value: Boolean);
    procedure SetHigherFlowUFormula(const Value: string);
    procedure SetHigherUFormula(const Value: string);
    procedure SetLowerFlowUFormula(const Value: string);
    procedure SetLowerUFormula(const Value: string);
    function GetUsedFormula: string;
    procedure SetUsedFormula(const Value: string);
  protected
    function CreateFormulaObject(Orientation:
      TDataSetOrientation): TFormulaObject; override;
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
    procedure InvalidateModel; override;
    function BoundaryFormulaCount: integer; override;
  public
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
  published
    property Used: Boolean read FUsed write SetUsed
    {$IFDEF SutraUsedFormulas}
      Stored False
    {$ENDIF}
      ;
    { $IFDEF SutraUsedFormulas}
    property UsedFormula: string read GetUsedFormula write SetUsedFormula;
    { $ENDIF}
    // PBG11
    property LowerUFormula: string read GetLowerUFormula
      write SetLowerUFormula;
    // QPBG11
    property LowerFlowUFormula: string read GetLowerFlowUFormula
      write SetLowerFlowUFormula;
    // PBG21
    property HigherUFormula: string read GetHigherUFormula
      write SetHigherUFormula;
    // QPBG21
    property HigherFlowUFormula: string read GetHigherFlowUFormula
      write SetHigherFlowUFormula;
  end;

  // @name is only used to record the names of the data to be edited.
  TSutraGeneralFlowTimeLink = class(TTimeListsModelLink)
  private
    FLowUTimeList: TModflowTimeList;
    FLowFlowRateUTimeList: TModflowTimeList;
    FHighUTimeList: TModflowTimeList;
    FHighFlowRateUTimeList: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  TSutraGeneralTransportCollection = class(TAbstractSutraBoundaryCollection)
  protected
    procedure LowerUChangeHandler(Sender: TObject);
    procedure LowerFlowUChangeHandler(Sender: TObject);
    procedure HigherUChangeHandler(Sender: TObject);
    procedure HigherFlowUChangeHandler(Sender: TObject);
    procedure UsedFormulaChangeHandler(Sender: TObject);
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject); override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
      override;
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    function CanInvalidate: boolean;
    class function ItemClass: TBoundaryItemClass; override;
    procedure Changed; override;
  public
    procedure Assign(Source: TPersistent); override;
  end;

  TSutraGeneralTransportBoundary = class(TSutraBoundary)
  protected
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    class function BoundaryCollectionClass: TMF_BoundCollClass;
      override;
  public
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel); override;
  end;

  TSutraGeneralTransBoundaryList = TList<TSutraGeneralTransportBoundary>;

const
{$IFDEF SutraUsedFormulas}
  UsedFormulaPosition = 0;
  LowerUPosition = 1;
  LowerFlowUPosition = 2;
  HigherUPosition = 3;
  HigherFlowUPosition = 4;
{$ELSE}
  UsedFormulaPosition = -1000;
  LowerUPosition = 0;
  LowerFlowUPosition = 1;
  HigherUPosition = 2;
  HigherFlowUPosition = 3;
{$ENDIF}

implementation

uses
  frmGoPhastUnit, SubscriptionUnit, PhastModelUnit, ScreenObjectUnit;

  { TSutraGenTransportItem }

procedure TSutraGenTransportItem.Assign(Source: TPersistent);
var
  SourceItem: TSutraGenTransportItem;
begin
  if Source is TSutraGenTransportItem then
  begin
    SourceItem := TSutraGenTransportItem(Source);
  {$IFDEF SutraUsedFormulas}
    UsedFormula := SourceItem.UsedFormula;
  {$ELSE}
    Used := SourceItem.Used;
  {$ENDIF}
    LowerUFormula := SourceItem.LowerUFormula;
    LowerFlowUFormula := SourceItem.LowerFlowUFormula;
    HigherUFormula := SourceItem.HigherUFormula;
    HigherFlowUFormula := SourceItem.HigherFlowUFormula;
  end;
  inherited;
end;

procedure TSutraGenTransportItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TSutraGeneralTransportCollection;
  U1Observer: TObserver;
  QU1Observer: TObserver;
  U2Observer: TObserver;
  QU2Observer: TObserver;
  UsedFormulaObserver: TObserver;
begin
  ParentCollection := Collection as TSutraGeneralTransportCollection;

{$IFDEF SutraUsedFormulas}
  UsedFormulaObserver := FObserverList[UsedFormulaPosition];
  UsedFormulaObserver.OnUpToDateSet := ParentCollection.UsedFormulaChangeHandler;
{$ENDIF}

  U1Observer := FObserverList[LowerUPosition];
  U1Observer.OnUpToDateSet := ParentCollection.LowerUChangeHandler;

  QU1Observer := FObserverList[LowerFlowUPosition];
  QU1Observer.OnUpToDateSet := ParentCollection.LowerFlowUChangeHandler;

  U2Observer := FObserverList[HigherUPosition];
  U2Observer.OnUpToDateSet := ParentCollection.HigherUChangeHandler;

  QU2Observer := FObserverList[HigherFlowUPosition];
  QU2Observer.OnUpToDateSet := ParentCollection.HigherFlowUChangeHandler;
end;

function TSutraGenTransportItem.BoundaryFormulaCount: integer;
begin
{$IFDEF SutraUsedFormulas}
  result := 5;
{$ELSE}
  result := 4;
{$ENDIF}
end;

function TSutraGenTransportItem.CreateFormulaObject(
  Orientation: TDataSetOrientation): TFormulaObject;
begin
  Assert(Orientation = dso3D);
  result := frmGoPhast.PhastModel.FormulaManager.Add;
  result.Parser := frmGoPhast.PhastModel.rpThreeDFormulaCompilerNodes;
  result.AddSubscriptionEvents(
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TSutraGenTransportItem.CreateFormulaObjects;
begin
  inherited;
  {$IFDEF SutraUsedFormulas}
  FUsedFormulaObject := CreateFormulaObject(dso3D);
  {$ENDIF}
  FLowerUFormula := CreateFormulaObject(dso3D);
  FLowerFlowUFormula := CreateFormulaObject(dso3D);
  FHigherUFormula := CreateFormulaObject(dso3D);
  FHigherFlowUFormula := CreateFormulaObject(dso3D);

end;

destructor TSutraGenTransportItem.Destroy;
begin
  LowerUFormula := '0';
  LowerFlowUFormula := '0';
  HigherUFormula := '0';
  HigherFlowUFormula := '0';
  {$IFDEF SutraUsedFormulas}
  UsedFormula := 'False';
  {$ENDIF}
  inherited;
end;

function TSutraGenTransportItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
  {$IFDEF SutraUsedFormulas}
    UsedFormulaPosition: result := UsedFormula;
  {$ENDIF}
    LowerUPosition: result := LowerUFormula;
    LowerFlowUPosition: result := LowerFlowUFormula;
    HigherUPosition: result := HigherUFormula;
    HigherFlowUPosition: result := HigherFlowUFormula;
    else
      Assert(False);
  end;
end;

function TSutraGenTransportItem.GetHigherFlowUFormula: string;
begin
  Result := FHigherFlowUFormula.Formula;
  ResetItemObserver(HigherFlowUPosition);
end;

function TSutraGenTransportItem.GetHigherUFormula: string;
begin
  Result := FHigherUFormula.Formula;
  ResetItemObserver(HigherUPosition);
end;

function TSutraGenTransportItem.GetLowerFlowUFormula: string;
begin
  Result := FLowerFlowUFormula.Formula;
  ResetItemObserver(LowerFlowUPosition);
end;

function TSutraGenTransportItem.GetLowerUFormula: string;
begin
  Result := FLowerUFormula.Formula;
  ResetItemObserver(LowerUPosition);
end;

procedure TSutraGenTransportItem.GetPropertyObserver(Sender: TObject;
  List: TList);
begin
  if Sender = FLowerUFormula then
  begin
    List.Add(FObserverList[LowerUPosition]);
  end
  else if Sender = FLowerFlowUFormula then
  begin
    List.Add(FObserverList[LowerFlowUPosition]);
  end
  else if Sender = FHigherUFormula then
  begin
    List.Add(FObserverList[HigherUPosition]);
  end
  else if Sender = FHigherFlowUFormula then
  begin
    List.Add(FObserverList[HigherFlowUPosition]);
  end
{$IFDEF SutraUsedFormulas}
  else if Sender = FUsedFormulaObject then
  begin
    List.Add(FObserverList[UsedFormulaPosition]);
  end
{$ENDIF}
  else
  begin
    Assert(False);
  end;
end;

function TSutraGenTransportItem.GetUsedFormula: string;
begin
{$IFDEF SutraUsedFormulas}
  Result := FUsedFormulaObject.Formula;
  ResetItemObserver(UsedFormulaPosition);
{$ELSE}
  result := 'False';
{$ENDIF}
end;

procedure TSutraGenTransportItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
//    PhastModel.InvalidateMfWellPumpage(self);
  end;
end;

function TSutraGenTransportItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TSutraGenTransportItem;
begin
  result := (AnotherItem is TSutraGenTransportItem)
    and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TSutraGenTransportItem(AnotherItem);
    result := (Item.LowerUFormula = LowerUFormula)
      and (Item.LowerFlowUFormula = LowerFlowUFormula)
      and (Item.HigherUFormula = HigherUFormula)
      and (Item.HigherFlowUFormula = HigherFlowUFormula)
  {$IFNDEF SutraUsedFormulas}
      and (Item.Used = Used);
  {$ELSE}
      and (Item.UsedFormula = UsedFormula);
  {$ENDIF}
  end;
end;

procedure TSutraGenTransportItem.RemoveFormulaObjects;
begin
{$IFDEF SutraUsedFormulas}
  frmGoPhast.PhastModel.FormulaManager.Remove(FUsedFormulaObject,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
{$ENDIF}
  frmGoPhast.PhastModel.FormulaManager.Remove(FLowerUFormula,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FLowerFlowUFormula,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FHigherUFormula,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FHigherFlowUFormula,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  inherited;

end;

procedure TSutraGenTransportItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    UsedFormulaPosition:
      UsedFormula := Value;
    LowerUPosition:
      LowerUFormula := Value;
    LowerFlowUPosition:
      LowerFlowUFormula := Value;
    HigherUPosition:
      HigherUFormula := Value;
    HigherFlowUPosition:
      HigherFlowUFormula := Value;
    else
      Assert(False);
  end;
end;

procedure TSutraGenTransportItem.SetHigherFlowUFormula(const Value: string);
begin
  UpdateFormula(Value, HigherFlowUPosition, FHigherFlowUFormula);
end;

procedure TSutraGenTransportItem.SetHigherUFormula(const Value: string);
begin
  UpdateFormula(Value, HigherUPosition, FHigherUFormula);
end;

procedure TSutraGenTransportItem.SetLowerFlowUFormula(const Value: string);
begin
  UpdateFormula(Value, LowerFlowUPosition, FLowerFlowUFormula);
end;

procedure TSutraGenTransportItem.SetLowerUFormula(const Value: string);
begin
  UpdateFormula(Value, LowerUPosition, FLowerUFormula);
end;

procedure TSutraGenTransportItem.SetUsed(const Value: Boolean);
begin
  if FUsed <> Value then
  begin
    FUsed := Value;
    InvalidateModel;
    if FUsed then
    begin
      UsedFormula := 'True';
    end
    else
    begin
      UsedFormula := 'False';
    end;
  end;
end;

procedure TSutraGenTransportItem.SetUsedFormula(const Value: string);
begin
{$IFDEF SutraUsedFormulas}
  UpdateFormula(Value, UsedFormulaPosition, FUsedFormulaObject);
{$ENDIF}
end;

{ TSutraGeneralTransportCollection }

procedure TSutraGeneralTransportCollection.AddSpecificBoundary(
  AModel: TBaseModel);
begin
  inherited;
  // this needs to be changed?
  Assert(False);
end;

function TSutraGeneralTransportCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
begin
  // this needs to be changed?
  Assert(False);
end;

procedure TSutraGeneralTransportCollection.Assign(Source: TPersistent);
var
  SourceBoundary: TSutraGeneralTransportCollection;
begin
  if Source is TSutraGeneralTransportCollection then
  begin
    SourceBoundary := TSutraGeneralTransportCollection(Source);
    ScheduleName := SourceBoundary.ScheduleName;
  end;
  inherited;

end;

procedure TSutraGeneralTransportCollection.AssignCellList(
  Expression: TExpression; ACellList: TObject;
  BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
  Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject);
begin
  inherited;
  // this needs to be changed?
  Assert(False);
end;

procedure TSutraGeneralTransportCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
begin
  inherited;
  // this needs to be changed?
  Assert(False);
end;

function TSutraGeneralTransportCollection.CanInvalidate: boolean;
begin
  result := (Model <> nil) and (ScreenObject <> nil)
    and (ScreenObject as TScreenObject).CanInvalidateModel;
end;

procedure TSutraGeneralTransportCollection.Changed;
begin
  LowerUChangeHandler(self);
  LowerFlowUChangeHandler(self);
  HigherUChangeHandler(self);
  HigherFlowUChangeHandler(self);
end;

function TSutraGeneralTransportCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TSutraGeneralFlowTimeLink;
end;

procedure TSutraGeneralTransportCollection.HigherFlowUChangeHandler(
  Sender: TObject);
begin
  InvalidateModel;
  if CanInvalidate then
  begin
    (Model as TCustomModel).InvalidateSutraGenTransQU2(Sender);
  end;
end;

procedure TSutraGeneralTransportCollection.HigherUChangeHandler(
  Sender: TObject);
begin
  InvalidateModel;
  if CanInvalidate then
  begin
    (Model as TCustomModel).InvalidateSutraGenTransU2(Sender);
  end;
end;

class function TSutraGeneralTransportCollection.ItemClass: TBoundaryItemClass;
begin
  result := TSutraGenTransportItem
end;

procedure TSutraGeneralTransportCollection.LowerFlowUChangeHandler(
  Sender: TObject);
begin
  InvalidateModel;
  if CanInvalidate then
  begin
    (Model as TCustomModel).InvalidateSutraGenTransQU1(Sender);
  end;
end;

procedure TSutraGeneralTransportCollection.LowerUChangeHandler(Sender: TObject);
begin
  InvalidateModel;
  if CanInvalidate then
  begin
    (Model as TCustomModel).InvalidateSutraGenTransU1(Sender);
  end;
end;

procedure TSutraGeneralTransportCollection.UsedFormulaChangeHandler(
  Sender: TObject);
begin
  LowerUChangeHandler(Sender);
  LowerFlowUChangeHandler(Sender);
  HigherUChangeHandler(Sender);
  HigherFlowUChangeHandler(Sender);
end;

{ TSutraGeneralTransportBoundary }

procedure TSutraGeneralTransportBoundary.AssignCells(
  BoundaryStorage: TCustomBoundaryStorage; ValueTimeList: TList;
  AModel: TBaseModel);
begin
  inherited;
  // does this need to change?
  Assert(False);
end;

class function TSutraGeneralTransportBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TSutraGeneralTransportCollection;
end;

procedure TSutraGeneralTransportBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
begin
  inherited;
  // does this need to change?
  Assert(False);
end;

{ TSutraGeneralFlowTimeLink }

procedure TSutraGeneralFlowTimeLink.CreateTimeLists;
begin
//  inherited;
//  inherited;
  FLowUTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FLowUTimeList.NonParamDescription := 'Lower temperature or concentration value';
  FLowUTimeList.ParamDescription := 'Lower temperature or concentration value';
  AddTimeList(FLowUTimeList);
//  if Model <> nil then
//  begin
//    FLowPressTimeList.OnInvalidate := (Model as TCustomModel).InvalidateMfRchRate;
//  end;
  FLowFlowRateUTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FLowFlowRateUTimeList.NonParamDescription := 'Mass or energy flow rate at lower temperature or concentration';
  FLowFlowRateUTimeList.ParamDescription := 'Mass or energy flow rate at lower temperature or concentration';
  AddTimeList(FLowFlowRateUTimeList);

  FHighUTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FHighUTimeList.NonParamDescription := 'Higher temperature or concentration value';
  FHighUTimeList.ParamDescription := 'Higher temperature or concentration value';
  AddTimeList(FHighUTimeList);          //  if Model <> nil then
//  begin
//    FLowPressTimeList.OnInvalidate := (Model as TCustomModel).InvalidateMfRchRate;
//  end;
  FHighFlowRateUTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FHighFlowRateUTimeList.NonParamDescription := 'Mass or energy flow rate at higher temperature or concentration';
  FHighFlowRateUTimeList.ParamDescription := 'Mass or energy flow rate at higher temperature or concentration';
  AddTimeList(FHighFlowRateUTimeList);

end;

destructor TSutraGeneralFlowTimeLink.Destroy;
begin
  FLowUTimeList.Free;
  FLowFlowRateUTimeList.Free;
  FHighUTimeList.Free;
  FHighFlowRateUTimeList.Free;
  inherited;
end;

end.
