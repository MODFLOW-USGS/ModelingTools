unit SutraGenTransBoundUnit;

interface

uses
  System.Classes, ModflowBoundaryUnit, FormulaManagerUnit, GoPhastTypes,
  OrderedCollectionUnit, RbwParser, SutraBoundaryUnit,
  System.Generics.Collections, SutraOptionsUnit;

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
    // @name is only retained for backwards compatibility. Use
    // @link(UsedFormula) instead.
    property Used: Boolean read FUsed write SetUsed Stored False;
    // @name is used to determine whether a @classname is used at a
    // particular node.
    property UsedFormula: string read GetUsedFormula write SetUsedFormula;
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
      Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject;
      PestName: string; PestSeriesName: string;
      PestSeriesMethod: TPestParamMethod); override;
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
  private
    FLakeInteractionType: TGeneralizedTransportInteractionType;
    procedure SetLakeInteractionType(
      const Value: TGeneralizedTransportInteractionType);
  protected
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    class function BoundaryCollectionClass: TMF_BoundCollClass;
      override;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel); override;
  published
    property LakeInteractionType: TGeneralizedTransportInteractionType
      read FLakeInteractionType write SetLakeInteractionType default gtitUseDefaults;
  end;

  TSutraGeneralTransBoundaryList = TList<TSutraGeneralTransportBoundary>;

const
  UsedFormulaPosition = 0;
  LowerUPosition = 1;
  LowerFlowUPosition = 2;
  HigherUPosition = 3;
  HigherFlowUPosition = 4;

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
    UsedFormula := SourceItem.UsedFormula;
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

  UsedFormulaObserver := FObserverList[UsedFormulaPosition];
  UsedFormulaObserver.OnUpToDateSet := ParentCollection.UsedFormulaChangeHandler;

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
  result := 5;
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
  FUsedFormulaObject := CreateFormulaObject(dso3D);
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
  UsedFormula := 'False';
  inherited;
end;

function TSutraGenTransportItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    UsedFormulaPosition: result := UsedFormula;
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
  else if Sender = FUsedFormulaObject then
  begin
    List.Add(FObserverList[UsedFormulaPosition]);
  end
  else
  begin
    Assert(False);
  end;
end;

function TSutraGenTransportItem.GetUsedFormula: string;
begin
  Result := FUsedFormulaObject.Formula;
  ResetItemObserver(UsedFormulaPosition);
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
      and (Item.UsedFormula = UsedFormula);
  end;
end;

procedure TSutraGenTransportItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FUsedFormulaObject,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
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
  UpdateFormulaNodes(Value, HigherFlowUPosition, FHigherFlowUFormula);
end;

procedure TSutraGenTransportItem.SetHigherUFormula(const Value: string);
begin
  UpdateFormulaNodes(Value, HigherUPosition, FHigherUFormula);
end;

procedure TSutraGenTransportItem.SetLowerFlowUFormula(const Value: string);
begin
  UpdateFormulaNodes(Value, LowerFlowUPosition, FLowerFlowUFormula);
end;

procedure TSutraGenTransportItem.SetLowerUFormula(const Value: string);
begin
  UpdateFormulaNodes(Value, LowerUPosition, FLowerUFormula);
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
  UpdateFormulaNodes(Value, UsedFormulaPosition, FUsedFormulaObject);
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
  Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject;
  PestName: string; PestSeriesName: string; PestSeriesMethod: TPestParamMethod);
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

procedure TSutraGeneralTransportBoundary.Assign(Source: TPersistent);
begin
  if Source is TSutraGeneralTransportBoundary then
  begin
    LakeInteractionType := TSutraGeneralTransportBoundary(Source).LakeInteractionType
  end;
  inherited;

end;

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

constructor TSutraGeneralTransportBoundary.Create(Model: TBaseModel;
  ScreenObject: TObject);
begin
  inherited;
  FLakeInteractionType := gtitUseDefaults;
end;

procedure TSutraGeneralTransportBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
begin
  inherited;
  // does this need to change?
  Assert(False);
end;

procedure TSutraGeneralTransportBoundary.SetLakeInteractionType(
  const Value: TGeneralizedTransportInteractionType);
begin
  if FLakeInteractionType <> Value then
  begin
    FLakeInteractionType := Value;
    InvalidateModel;
  end;
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
