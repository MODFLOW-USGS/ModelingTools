unit SutraGeneralBoundaryUnit;

interface

uses
  System.Classes, ModflowBoundaryUnit, FormulaManagerUnit, GoPhastTypes,
  OrderedCollectionUnit, RbwParser, SutraBoundaryUnit,
  System.Generics.Collections, SutraOptionsUnit, SubscriptionUnit,
  FormulaManagerInterfaceUnit;

type
  TSutraGeneralFlowItem = class(TCustomBoundaryItem)
  private
    // PBG11
    FLowerPressureFormula: IFormulaObject;
    // QPBG11
    FLowerFlowRateFormula: IFormulaObject;
    // PBG21
    FHigherPressureFormula: IFormulaObject;
    // QPBG21
    FHigherFlowRateFormula: IFormulaObject;
    // UPBGI1
    // Temperature or solute concentration of any external fluid that
    // enters the model
    FUInFormula: IFormulaObject;
    // UPBGO1
    // value U+UBPGO1, where U is the temperature or concentration computed
    // at the node.
    FUoutFormula: IFormulaObject;
    FExitSpecMethod: TSutraExitSpecificationMethod;
    FLowerLimitType: TSutraLimitType;
    FUpperLimitType: TSutraLimitType;
    FUsed: Boolean;
    FUsedFormulaObject: IFormulaObject;
    function GetHigherFlowRateFormula: string;
    function GetHigherPressureFormula: string;
    function GetLowerFlowRateFormula: string;
    function GetLowerPressureFormula: string;
    function GetUInFormula: string;
    function GetUoutFormula: string;
    procedure SetUsed(const Value: Boolean);
    procedure SetExitSpecMethod(const Value: TSutraExitSpecificationMethod);
    procedure SetHigherFlowRateFormula(const Value: string);
    procedure SetHigherPressureFormula(const Value: string);
    procedure SetLowerFlowRateFormula(const Value: string);
    procedure SetLowerLimitType(const Value: TSutraLimitType);
    procedure SetLowerPressureFormula(const Value: string);
    procedure SetUInFormula(const Value: string);
    procedure SetUoutFormula(const Value: string);
    procedure SetUpperLimitType(const Value: TSutraLimitType);
    function GetUsedFormula: string;
    procedure SetUsedFormula(const Value: string);
    function GetUsed: Boolean;
  protected
    function CreateFormulaObject(Orientation:
      TDataSetOrientation): IFormulaObject; override;
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
    // @name is retained only for backwards compatibility.
    // Use @link(UsedFormula) instead.
    property Used: Boolean read GetUsed write SetUsed Stored False;
    // @name is used to determine whether or not this item is used at a
    // particular node.
    property UsedFormula: string read GetUsedFormula write SetUsedFormula;
    // CPQL11
    property LowerLimitType: TSutraLimitType read FLowerLimitType
      write SetLowerLimitType;
    // CPQL21
    property UpperLimitType: TSutraLimitType read FUpperLimitType
      write SetUpperLimitType;
    // CUPBGO1
    property ExitSpecMethod: TSutraExitSpecificationMethod read FExitSpecMethod
      write SetExitSpecMethod;
    // PBG11
    property LowerPressureFormula: string read GetLowerPressureFormula
      write SetLowerPressureFormula;
    // QPBG11
    property LowerFlowRateFormula: string read GetLowerFlowRateFormula
      write SetLowerFlowRateFormula;
    // PBG21
    property HigherPressureFormula: string read GetHigherPressureFormula
      write SetHigherPressureFormula;
    // QPBG21
    property HigherFlowRateFormula: string read GetHigherFlowRateFormula
      write SetHigherFlowRateFormula;
    // UPBGI1
    // Temperature or solute concentration of any external fluid that
    // enters the model
    property UInFormula: string  read GetUInFormula write SetUInFormula;
    // UPBGO1
    // value U+UBPGO1, where U is the temperature or concentration computed
    // at the node.
    property UoutFormula: string read GetUoutFormula write SetUoutFormula;
  end;

  // @name is only used to record the names of the data to be edited.
  TSutraGeneralFlowTimeLink = class(TTimeListsModelLink)
  private
    FLowPressTimeList: TModflowTimeList;
    FLowFlowRateTimeList: TModflowTimeList;
    FHighPressTimeList: TModflowTimeList;
    FHighFlowRateTimeList: TModflowTimeList;
    FInTimeList: TModflowTimeList;
    FOutTimeList: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  TSutraGeneralFlowCollection = class(TAbstractSutraBoundaryCollection)
  protected
    procedure LowerPressureChangeHandler(Sender: TObject);
    procedure LowerFlowRateChangeHandler(Sender: TObject);
    procedure HigherPressureChangeHandler(Sender: TObject);
    procedure HigherFlowRateChangeHandler(Sender: TObject);
    procedure UInChangeHandler(Sender: TObject);
    procedure UOutChangeHandler(Sender: TObject);
    procedure UsedFormulaChangeHandler(Sender: TObject);
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(CellAssignmentData: TCellAssignmentData); override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
      override;
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    function CanInvalidate: boolean;
    class function ItemClass: TBoundaryItemClass; override;
    procedure Changed; override;
  public
    procedure Assign(Source: TPersistent); override;
  end;

  TSutraGeneralFlowBoundary = class(TSutraBoundary)
  private
    FLakeInteractionType: TGeneralizedFlowInteractionType;
    FPestHigherPressureMethod: TPestParamMethod;
    FPestLowerFlowRateMethod: TPestParamMethod;
    FPestHigherFlowRateMethod: TPestParamMethod;
    FPestUOutMethod: TPestParamMethod;
    FPestLowerFlowRateFormula: IFormulaObject;
    FPestHigherPressureFormula: IFormulaObject;
    FPestHigherFlowRateFormula: IFormulaObject;
    FPestUOutFormula: IFormulaObject;
    FPestHigherFlowRateObserver: TObserver;
    FPestHigherPressureObserver: TObserver;
    FPestLowerFlowRateObserver: TObserver;
    FPestUOutObserver: TObserver;
    procedure SetLakeInteractionType(
      const Value: TGeneralizedFlowInteractionType);
    function GetPestHigherFlowRateFormula: string;
    function GetPestHigherFlowRateObserver: TObserver;
    function GetPestHigherPressureFormula: string;
    function GetPestHigherPressureObserver: TObserver;
    function GetPestLowerFlowRateFormula: string;
    function GetPestLowerFlowRateObserver: TObserver;
    function GetPestUOutFormula: string;
    function GetPestUOutObserver: TObserver;
    procedure SetPestHigherFlowRateFormula(const Value: string);
    procedure SetPestHigherFlowRateMethod(const Value: TPestParamMethod);
    procedure SetPestHigherPressureFormula(const Value: string);
    procedure SetPestHigherPressureMethod(const Value: TPestParamMethod);
    procedure SetPestLowerFlowRateFormula(const Value: string);
    procedure SetPestLowerFlowRateMethod(const Value: TPestParamMethod);
    procedure SetPestUOutFormula(const Value: string);
    procedure SetPestUOutMethod(const Value: TPestParamMethod);
  protected
    function BoundaryObserverPrefix: string; override;
    // PEST
    procedure PQChangeHandler(Sender: TObject); override;
    procedure UChangeHandler(Sender: TObject); override;
    procedure LowerFlowRateChangeHandler(Sender: TObject);
    procedure HigherPressureChangeHandler(Sender: TObject);
    procedure HigherFlowRateChangeHandler(Sender: TObject);
    procedure UOutChangeHandler(Sender: TObject);
    procedure CreateFormulaObjects; override;
    procedure CreateObservers; override;
    function GetPestBoundaryFormula(FormulaIndex: integer): string; override;
    procedure SetPestBoundaryFormula(FormulaIndex: integer;
      const Value: string); override;
    function GetPestBoundaryMethod(FormulaIndex: integer): TPestParamMethod; override;
    procedure SetPestBoundaryMethod(FormulaIndex: integer;
      const Value: TPestParamMethod); override;
    property PestLowerFlowRateObserver: TObserver read GetPestLowerFlowRateObserver;
    property PestHigherPressureObserver: TObserver read GetPestHigherPressureObserver;
    property PestHigherFlowRateObserver: TObserver read GetPestHigherFlowRateObserver;
    property PestUOutObserver: TObserver read GetPestUOutObserver;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    class function BoundaryCollectionClass: TMF_BoundCollClass;
      override;
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    procedure Loaded;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
  published
    property LakeInteractionType: TGeneralizedFlowInteractionType
      read FLakeInteractionType write SetLakeInteractionType
      default gfitUseDefaults;
    // PestBoundaryValueFormula is used with LowerPressure, PBG11
    // PestAssociatedValueFormula is used with InflowU

    // QPBG11
    property PestLowerFlowRateFormula: string read GetPestLowerFlowRateFormula
      write SetPestLowerFlowRateFormula;
    property PestLowerFlowRateMethod: TPestParamMethod
      read FPestLowerFlowRateMethod
      write SetPestLowerFlowRateMethod;
    property PestHigherPressureFormula: string read GetPestHigherPressureFormula
      write SetPestHigherPressureFormula;
    property PestHigherPressureMethod: TPestParamMethod
      read FPestHigherPressureMethod
      write SetPestHigherPressureMethod;
    property PestHigherFlowRateFormula: string read GetPestHigherFlowRateFormula
      write SetPestHigherFlowRateFormula;
    property PestHigherFlowRateMethod: TPestParamMethod
      read FPestHigherFlowRateMethod
      write SetPestHigherFlowRateMethod;
    property PestUOutFormula: string read GetPestUOutFormula
      write SetPestUOutFormula;
    property PestUOutMethod: TPestParamMethod read FPestUOutMethod
      write SetPestUOutMethod;
  end;

  TSutraGeneralFlowBoundaryList = TList<TSutraGeneralFlowBoundary>;

const
  UsedFormulaPosition = 0;
  LowerPressurePosition = 1;
  LowerFlowRatePosition = 2;
  HigherPressurePosition = 3;
  HigherFlowRatePosition = 4;
  UInPosition = 5;
  UOutPosition = 6;
  SutraGenOffset = 2;

implementation

uses
  frmGoPhastUnit, PhastModelUnit, ScreenObjectUnit;

{ TSutraGeneralFlowItem }

procedure TSutraGeneralFlowItem.Assign(Source: TPersistent);
var
  SourceItem: TSutraGeneralFlowItem;
begin
  if Source is TSutraGeneralFlowItem then
  begin
    SourceItem := TSutraGeneralFlowItem(Source);
    UsedFormula := SourceItem.UsedFormula;
//    Used := SourceItem.Used;
    LowerLimitType := SourceItem.LowerLimitType;
    UpperLimitType := SourceItem.UpperLimitType;
    ExitSpecMethod := SourceItem.ExitSpecMethod;
    LowerPressureFormula := SourceItem.LowerPressureFormula;
    LowerFlowRateFormula := SourceItem.LowerFlowRateFormula;
    HigherPressureFormula := SourceItem.HigherPressureFormula;
    HigherFlowRateFormula := SourceItem.HigherFlowRateFormula;
    UInFormula := SourceItem.UInFormula;
    UoutFormula := SourceItem.UoutFormula;
  end;
  inherited;

end;

procedure TSutraGeneralFlowItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TSutraGeneralFlowCollection;
  LowerPressureObserver: TObserver;
  LowerFlowRateObserver: TObserver;
  HigherPressureObserver: TObserver;
  HigherFlowRateObserver: TObserver;
  UInObserver: TObserver;
  UIoutObserver: TObserver;
  UsedFormulaObserver: TObserver;
begin
  ParentCollection := Collection as TSutraGeneralFlowCollection;

  UsedFormulaObserver := FObserverList[UsedFormulaPosition];
  UsedFormulaObserver.OnUpToDateSet := ParentCollection.UsedFormulaChangeHandler;

  LowerPressureObserver := FObserverList[LowerPressurePosition];
  LowerPressureObserver.OnUpToDateSet := ParentCollection.LowerPressureChangeHandler;

  LowerFlowRateObserver := FObserverList[LowerFlowRatePosition];
  LowerFlowRateObserver.OnUpToDateSet := ParentCollection.LowerFlowRateChangeHandler;

  HigherPressureObserver := FObserverList[HigherPressurePosition];
  HigherPressureObserver.OnUpToDateSet := ParentCollection.HigherPressureChangeHandler;

  HigherFlowRateObserver := FObserverList[HigherFlowRatePosition];
  HigherFlowRateObserver.OnUpToDateSet := ParentCollection.HigherFlowRateChangeHandler;

  UInObserver := FObserverList[UInPosition];
  UInObserver.OnUpToDateSet := ParentCollection.UInChangeHandler;

  UIoutObserver := FObserverList[UOutPosition];
  UIoutObserver.OnUpToDateSet := ParentCollection.UOutChangeHandler;
end;

function TSutraGeneralFlowItem.BoundaryFormulaCount: integer;
begin
  result := 7;
end;

function TSutraGeneralFlowItem.CreateFormulaObject(
  Orientation: TDataSetOrientation): IFormulaObject;
begin
  Assert(Orientation = dso3D);
  result := frmGoPhast.PhastModel.FormulaManager.Add;
  result.Parser := frmGoPhast.PhastModel.rpThreeDFormulaCompilerNodes;
  result.AddSubscriptionEvents(
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TSutraGeneralFlowItem.CreateFormulaObjects;
begin
  inherited;
  FUsedFormulaObject := CreateFormulaObject(dso3D);
  FLowerPressureFormula := CreateFormulaObject(dso3D);
  FLowerFlowRateFormula := CreateFormulaObject(dso3D);
  FHigherPressureFormula := CreateFormulaObject(dso3D);
  FHigherFlowRateFormula := CreateFormulaObject(dso3D);
  FUInFormula := CreateFormulaObject(dso3D);
  FUoutFormula := CreateFormulaObject(dso3D);
end;

destructor TSutraGeneralFlowItem.Destroy;
begin
  LowerPressureFormula := '0.';
  LowerFlowRateFormula := '0.';
  HigherPressureFormula := '0.';
  HigherFlowRateFormula := '0.';
  UInFormula := '0.';
  UoutFormula := '0.';
  UsedFormula := 'False';
  inherited;
end;

function TSutraGeneralFlowItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    UsedFormulaPosition: result := UsedFormula;
    LowerPressurePosition: result := LowerPressureFormula;
    LowerFlowRatePosition: result := LowerFlowRateFormula;
    HigherPressurePosition: result := HigherPressureFormula;
    HigherFlowRatePosition: result := HigherFlowRateFormula;
    UInPosition: result := UInFormula;
    UOutPosition: result := UoutFormula;
    else
      Assert(False);
  end;
end;

function TSutraGeneralFlowItem.GetHigherFlowRateFormula: string;
begin
  Result := FHigherFlowRateFormula.Formula;
  ResetItemObserver(HigherFlowRatePosition);
end;

function TSutraGeneralFlowItem.GetHigherPressureFormula: string;
begin
  Result := FHigherPressureFormula.Formula;
  ResetItemObserver(HigherPressurePosition);
end;

function TSutraGeneralFlowItem.GetLowerFlowRateFormula: string;
begin
  Result := FLowerFlowRateFormula.Formula;
  ResetItemObserver(LowerFlowRatePosition);
end;

function TSutraGeneralFlowItem.GetLowerPressureFormula: string;
begin
  Result := FLowerPressureFormula.Formula;
  ResetItemObserver(LowerPressurePosition);
end;

procedure TSutraGeneralFlowItem.GetPropertyObserver(Sender: TObject;
  List: TList);
begin
  if Sender = FLowerPressureFormula as TObject then
  begin
    List.Add(FObserverList[LowerPressurePosition]);
  end
  else if Sender = FLowerFlowRateFormula as TObject then
  begin
    List.Add(FObserverList[LowerFlowRatePosition]);
  end
  else if Sender = FHigherPressureFormula as TObject then
  begin
    List.Add(FObserverList[HigherPressurePosition]);
  end
  else if Sender = FHigherFlowRateFormula as TObject then
  begin
    List.Add(FObserverList[HigherFlowRatePosition]);
  end
  else if Sender = FUInFormula as TObject then
  begin
    List.Add(FObserverList[UInPosition]);
  end
  else if Sender = FUoutFormula as TObject then
  begin
    List.Add(FObserverList[UOutPosition]);
  end
  else if Sender = FUsedFormulaObject as TObject then
  begin
    List.Add(FObserverList[UsedFormulaPosition]);
  end
  else
  begin
    Assert(False);
  end;

end;

function TSutraGeneralFlowItem.GetUInFormula: string;
begin
  Result := FUInFormula.Formula;
  ResetItemObserver(UInPosition);
end;

function TSutraGeneralFlowItem.GetUoutFormula: string;
begin
  Result := FUoutFormula.Formula;
  ResetItemObserver(UOutPosition);
end;

function TSutraGeneralFlowItem.GetUsed: Boolean;
begin
  result := FUsed;
end;

function TSutraGeneralFlowItem.GetUsedFormula: string;
begin
  Result := FUsedFormulaObject.Formula;
  ResetItemObserver(UsedFormulaPosition);
end;

procedure TSutraGeneralFlowItem.InvalidateModel;
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

function TSutraGeneralFlowItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TSutraGeneralFlowItem;
begin
  result := (AnotherItem is TSutraGeneralFlowItem)
    and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TSutraGeneralFlowItem(AnotherItem);
    result := (Item.LowerPressureFormula = LowerPressureFormula)
      and (Item.LowerFlowRateFormula = LowerFlowRateFormula)
      and (Item.HigherPressureFormula = HigherPressureFormula)
      and (Item.HigherFlowRateFormula = HigherFlowRateFormula)
      and (Item.UInFormula = UInFormula)
      and (Item.UoutFormula = UoutFormula)
      and (Item.LowerLimitType = LowerLimitType)
      and (Item.UpperLimitType = UpperLimitType)
      and (Item.ExitSpecMethod = ExitSpecMethod)
      and (Item.UsedFormula = UsedFormula);
  end;
end;

procedure TSutraGeneralFlowItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FUsedFormulaObject,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FLowerPressureFormula,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FLowerFlowRateFormula,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FHigherPressureFormula,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FHigherFlowRateFormula,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FUInFormula,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FUoutFormula,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  inherited;
end;

procedure TSutraGeneralFlowItem.SetUsed(const Value: Boolean);
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

procedure TSutraGeneralFlowItem.SetUsedFormula(const Value: string);
begin
  UpdateFormulaNodes(Value, UsedFormulaPosition, FUsedFormulaObject);
end;

procedure TSutraGeneralFlowItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    UsedFormulaPosition:
      UsedFormula := Value;
    LowerPressurePosition:
      LowerPressureFormula := Value;
    LowerFlowRatePosition:
      LowerFlowRateFormula := Value;
    HigherPressurePosition:
      HigherPressureFormula := Value;
    HigherFlowRatePosition:
      HigherFlowRateFormula := Value;
    UInPosition:
      UInFormula := Value;
    UOutPosition:
      UoutFormula := Value;
    else
      Assert(False);
  end;
end;

procedure TSutraGeneralFlowItem.SetExitSpecMethod(
  const Value: TSutraExitSpecificationMethod);
begin
  if FExitSpecMethod <> Value then
  begin
    FExitSpecMethod := Value;
    InvalidateModel;
  end;
end;

procedure TSutraGeneralFlowItem.SetHigherFlowRateFormula(const Value: string);
begin
  UpdateFormulaNodes(Value, HigherFlowRatePosition, FHigherFlowRateFormula);
end;

procedure TSutraGeneralFlowItem.SetHigherPressureFormula(const Value: string);
begin
  UpdateFormulaNodes(Value, HigherPressurePosition, FHigherPressureFormula);
end;

procedure TSutraGeneralFlowItem.SetLowerFlowRateFormula(const Value: string);
begin
  UpdateFormulaNodes(Value, LowerFlowRatePosition, FLowerFlowRateFormula);
end;

procedure TSutraGeneralFlowItem.SetLowerLimitType(const Value: TSutraLimitType);
begin
  if FLowerLimitType <> Value then
  begin
    FLowerLimitType := Value;
    InvalidateModel;
  end;
end;

procedure TSutraGeneralFlowItem.SetLowerPressureFormula(const Value: string);
begin
  UpdateFormulaNodes(Value, LowerPressurePosition, FLowerPressureFormula);
end;

procedure TSutraGeneralFlowItem.SetUInFormula(const Value: string);
begin
  UpdateFormulaNodes(Value, UInPosition, FUInFormula);
end;

procedure TSutraGeneralFlowItem.SetUoutFormula(const Value: string);
begin
  UpdateFormulaNodes(Value, UOutPosition, FUoutFormula);
end;

procedure TSutraGeneralFlowItem.SetUpperLimitType(const Value: TSutraLimitType);
begin
  if FUpperLimitType <> Value then
  begin
    FUpperLimitType := Value;
    InvalidateModel;
  end;
end;

{ TSutraGeneralFlowCollection }

procedure TSutraGeneralFlowCollection.AddSpecificBoundary(
  AModel: TBaseModel);
begin
  inherited;
  // this needs to be changed?
  Assert(False);
end;

function TSutraGeneralFlowCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
begin
  // this needs to be changed?
  Assert(False);
end;

procedure TSutraGeneralFlowCollection.Assign(Source: TPersistent);
var
  SourceBoundary: TSutraGeneralFlowCollection;
begin
  if Source is TSutraGeneralFlowCollection then
  begin
    SourceBoundary := TSutraGeneralFlowCollection(Source);
    ScheduleName := SourceBoundary.ScheduleName;
  end;
  inherited;
end;

procedure TSutraGeneralFlowCollection.AssignCellList(CellAssignmentData: TCellAssignmentData);
begin
  inherited;
  // this needs to be changed?
  Assert(False);
end;

procedure TSutraGeneralFlowCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
begin
  inherited;
  // this needs to be changed?
  Assert(False);
end;

function TSutraGeneralFlowCollection.CanInvalidate: boolean;
begin
  result := (Model <> nil) and (ScreenObject <> nil)
    and (ScreenObject as TScreenObject).CanInvalidateModel;
end;

procedure TSutraGeneralFlowCollection.Changed;
begin
  LowerPressureChangeHandler(self);
  LowerFlowRateChangeHandler(self);
  HigherPressureChangeHandler(self);
  HigherFlowRateChangeHandler(self);
  UInChangeHandler(self);
  UOutChangeHandler(self);

end;

class function TSutraGeneralFlowCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TSutraGeneralFlowTimeLink;
end;

procedure TSutraGeneralFlowCollection.HigherFlowRateChangeHandler(
  Sender: TObject);
begin
  InvalidateModel;
  if CanInvalidate then
  begin
    (Model as TCustomModel).InvalidateSutraGenFlowRate2(Sender);
  end;
end;

procedure TSutraGeneralFlowCollection.HigherPressureChangeHandler(
  Sender: TObject);
begin
  InvalidateModel;
  if CanInvalidate then
  begin
    (Model as TCustomModel).InvalidateSutraGenFlowPress2(Sender);
  end;
end;

class function TSutraGeneralFlowCollection.ItemClass: TBoundaryItemClass;
begin
  result := TSutraGeneralFlowItem
end;

procedure TSutraGeneralFlowCollection.LowerFlowRateChangeHandler(
  Sender: TObject);
begin
  InvalidateModel;
  if CanInvalidate then
  begin
    (Model as TCustomModel).InvalidateSutraGenFlowRate1(Sender);
  end;
end;

procedure TSutraGeneralFlowCollection.LowerPressureChangeHandler(
  Sender: TObject);
begin
  InvalidateModel;
  if CanInvalidate then
  begin
    (Model as TCustomModel).InvalidateSutraGenFlowPress1(Sender);
  end;
end;

procedure TSutraGeneralFlowCollection.UInChangeHandler(
  Sender: TObject);
begin
  InvalidateModel;
  if CanInvalidate then
  begin
    (Model as TCustomModel).InvalidateSutraGenFlowU1(Sender);
  end;
end;

procedure TSutraGeneralFlowCollection.UOutChangeHandler(
  Sender: TObject);
begin
  InvalidateModel;
  if CanInvalidate then
  begin
    (Model as TCustomModel).InvalidateSutraGenFlowU2(Sender);
  end;
end;

procedure TSutraGeneralFlowCollection.UsedFormulaChangeHandler(Sender: TObject);
begin
  LowerPressureChangeHandler(Sender);
  LowerFlowRateChangeHandler(Sender);
  HigherPressureChangeHandler(Sender);
  HigherFlowRateChangeHandler(Sender);
  UInChangeHandler(Sender);
  UOutChangeHandler(Sender);
end;

{ TSutraGeneralFlowBoundary }

procedure TSutraGeneralFlowBoundary.Assign(Source: TPersistent);
var
  GenFlowSource: TSutraGeneralFlowBoundary;
begin
  if Source is TSutraGeneralFlowBoundary then
  begin
    GenFlowSource := TSutraGeneralFlowBoundary(Source);
    LakeInteractionType := GenFlowSource.LakeInteractionType;
    PestLowerFlowRateFormula := GenFlowSource.PestLowerFlowRateFormula;
    PestLowerFlowRateMethod := GenFlowSource.PestLowerFlowRateMethod;
    PestHigherPressureFormula := GenFlowSource.PestHigherPressureFormula;
    PestHigherPressureMethod := GenFlowSource.PestHigherPressureMethod;
    PestHigherFlowRateFormula := GenFlowSource.PestHigherFlowRateFormula;
    PestHigherFlowRateMethod := GenFlowSource.PestHigherFlowRateMethod;
    PestUOutFormula := GenFlowSource.PestUOutFormula;
    PestUOutMethod := GenFlowSource.PestUOutMethod;
  end;
  inherited;
end;

procedure TSutraGeneralFlowBoundary.AssignCells(
  BoundaryStorage: TCustomBoundaryStorage; ValueTimeList: TList;
  AModel: TBaseModel);
begin
  inherited;
  // does this need to change?
  Assert(False);
end;

class function TSutraGeneralFlowBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TSutraGeneralFlowCollection;
end;

function TSutraGeneralFlowBoundary.BoundaryObserverPrefix: string;
begin
  result := 'PestGeneralizedFlow_';
end;

constructor TSutraGeneralFlowBoundary.Create(Model: TBaseModel;
  ScreenObject: TObject);
var
  PestIndex: Integer;
begin
  inherited;
  FLakeInteractionType := gfitUseDefaults;

  for PestIndex := LowerPressurePosition to UoutPosition do
  begin
    PestBoundaryFormula[PestIndex] := '';
    PestBoundaryMethod[PestIndex] := DefaultBoundaryMethod(PestIndex);
  end;
end;

procedure TSutraGeneralFlowBoundary.CreateFormulaObjects;
begin
  inherited;
  FPestLowerFlowRateFormula := CreateFormulaObjectNodes(dso3D);
  FPestHigherPressureFormula := CreateFormulaObjectNodes(dso3D);
  FPestHigherFlowRateFormula := CreateFormulaObjectNodes(dso3D);
  FPestUOutFormula := CreateFormulaObjectNodes(dso3D);
end;

procedure TSutraGeneralFlowBoundary.CreateObservers;
begin
  inherited;
  if ScreenObject <> nil then
  begin
    FObserverList.Add(PestLowerFlowRateObserver);
    FObserverList.Add(PestHigherPressureObserver);
    FObserverList.Add(PestHigherFlowRateObserver);
    FObserverList.Add(PestUOutObserver);
  end;
end;

class function TSutraGeneralFlowBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    LowerPressurePosition:
      begin
        result := ppmAdd;
      end;
    LowerFlowRatePosition:
      begin
        result := ppmMultiply;
      end;
    HigherPressurePosition:
      begin
        result := ppmAdd;
      end;
    HigherFlowRatePosition:
      begin
        result := ppmMultiply;
      end;
    UInPosition:
      begin
        result := ppmMultiply;
      end;
    UOutPosition:
      begin
        result := ppmMultiply;
      end;
    else
      begin
        result := inherited;
//        Assert(False);
      end;
  end;
end;

destructor TSutraGeneralFlowBoundary.Destroy;
var
  PestIndex: Integer;
begin
  for PestIndex := LowerPressurePosition to UoutPosition do
  begin
    PestBoundaryFormula[PestIndex] := '';
  end;
  inherited;
end;

procedure TSutraGeneralFlowBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
begin
  inherited;
  // does this need to change?
  Assert(False);
end;

function TSutraGeneralFlowBoundary.GetPestBoundaryFormula(
  FormulaIndex: integer): string;
begin
  case FormulaIndex of
    UsedFormulaPosition:
      begin
        result := inherited;
      end;
    LowerPressurePosition:
      begin
        result := PestBoundaryValueFormula;
      end;
    LowerFlowRatePosition:
      begin
        result := PestLowerFlowRateFormula;
      end;
    HigherPressurePosition:
      begin
        result := PestHigherPressureFormula;
      end;
    HigherFlowRatePosition:
      begin
        result := PestHigherFlowRateFormula;
      end;
    UInPosition:
      begin
        result := PestAssociatedValueFormula;
      end;
    UOutPosition:
      begin
        result := PestUOutFormula;
      end;
    else
      begin
        result := inherited;
        Assert(False);
      end;
  end;
end;

function TSutraGeneralFlowBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    UsedFormulaPosition:
      begin
        result := inherited;
      end;
    LowerPressurePosition:
      begin
        result := PestBoundaryValueMethod;
      end;
    LowerFlowRatePosition:
      begin
        result := PestLowerFlowRateMethod;
      end;
    HigherPressurePosition:
      begin
        result := PestHigherPressureMethod;
      end;
    HigherFlowRatePosition:
      begin
        result := PestHigherFlowRateMethod;
      end;
    UInPosition:
      begin
        result := PestAssociatedValueMethod;
      end;
    UOutPosition:
      begin
        result := PestUOutMethod;
      end;
    else
      begin
        result := inherited;
        Assert(False);
      end;
  end;
end;

function TSutraGeneralFlowBoundary.GetPestHigherFlowRateFormula: string;
begin
  Result := FPestHigherFlowRateFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(HigherFlowRatePosition+SutraGenOffset);
  end;
end;

function TSutraGeneralFlowBoundary.GetPestHigherFlowRateObserver: TObserver;
begin
  if FPestHigherFlowRateObserver = nil then
  begin
    CreateObserver('PestHigherFlowRate_', FPestHigherFlowRateObserver, nil);
    FPestHigherFlowRateObserver.OnUpToDateSet := HigherFlowRateChangeHandler;
  end;
  result := FPestHigherFlowRateObserver;
end;

function TSutraGeneralFlowBoundary.GetPestHigherPressureFormula: string;
begin
  Result := FPestHigherPressureFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(HigherPressurePosition+SutraGenOffset);
  end;
end;

function TSutraGeneralFlowBoundary.GetPestHigherPressureObserver: TObserver;
begin
//  if FPestHigherPressureObserver = nil then
  begin
    CreateObserver('PestHigherPressure_', FPestHigherPressureObserver, nil);
    FPestHigherPressureObserver.OnUpToDateSet := HigherPressureChangeHandler;
  end;
  result := FPestHigherPressureObserver;
end;

function TSutraGeneralFlowBoundary.GetPestLowerFlowRateFormula: string;
begin
  Result := FPestLowerFlowRateFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(LowerFlowRatePosition+SutraGenOffset);
  end;
end;

function TSutraGeneralFlowBoundary.GetPestLowerFlowRateObserver: TObserver;
begin
  if FPestLowerFlowRateObserver = nil then
  begin
    CreateObserver('PestLowerFlowRate_', FPestLowerFlowRateObserver, nil);
    FPestLowerFlowRateObserver.OnUpToDateSet := LowerFlowRateChangeHandler;
  end;
  result := FPestLowerFlowRateObserver;
end;

function TSutraGeneralFlowBoundary.GetPestUOutFormula: string;
begin
  Result := FPestUOutFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(UOutPosition+SutraGenOffset);
  end;
end;

function TSutraGeneralFlowBoundary.GetPestUOutObserver: TObserver;
begin
  if FPestUOutObserver = nil then
  begin
    CreateObserver('PestUOut_', FPestUOutObserver, nil);
    FPestUOutObserver.OnUpToDateSet := UOutChangeHandler;
  end;
  result := FPestUOutObserver;
end;

procedure TSutraGeneralFlowBoundary.HigherFlowRateChangeHandler(
  Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateSutraGenFlowRate2(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateSutraGenFlowRate2(self);
      end;
    end;
  end;
end;

procedure TSutraGeneralFlowBoundary.HigherPressureChangeHandler(
  Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateSutraGenFlowPress2(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateSutraGenFlowPress2(self);
      end;
    end;
  end;
end;

procedure TSutraGeneralFlowBoundary.Loaded;
var
  SutraBoundaryCollection: TSutraGeneralFlowCollection;
  Index: Integer;
  AnItem: TSutraGeneralFlowItem;
begin
  SutraBoundaryCollection := Values as TSutraGeneralFlowCollection;
  for Index := 0 to SutraBoundaryCollection.Count - 1 do
  begin
    AnItem := SutraBoundaryCollection[Index] as TSutraGeneralFlowItem;
    if (AnItem.UsedFormula = '') or (AnItem.UsedFormula = '0') then
    begin
      AnItem.UsedFormula := 'False';
    end;
  end;
end;

procedure TSutraGeneralFlowBoundary.LowerFlowRateChangeHandler(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateSutraGenFlowRate1(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateSutraGenFlowRate1(self);
      end;
    end;
  end;
end;

procedure TSutraGeneralFlowBoundary.PQChangeHandler(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateSutraGenFlowPress1(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateSutraGenFlowPress1(self);
      end;
    end;
  end;
end;

procedure TSutraGeneralFlowBoundary.SetLakeInteractionType(
  const Value: TGeneralizedFlowInteractionType);
begin
  if FLakeInteractionType <> Value then
  begin
    FLakeInteractionType := Value;
    InvalidateModel;
  end;
end;

procedure TSutraGeneralFlowBoundary.SetPestBoundaryFormula(
  FormulaIndex: integer; const Value: string);
begin
  case FormulaIndex of
    LowerPressurePosition:
      begin
        PestBoundaryValueFormula := Value;
      end;
    LowerFlowRatePosition:
      begin
        PestLowerFlowRateFormula := Value;
      end;
    HigherPressurePosition:
      begin
        PestHigherPressureFormula := Value;
      end;
    HigherFlowRatePosition:
      begin
        PestHigherFlowRateFormula := Value;
      end;
    UInPosition:
      begin
        PestAssociatedValueFormula := Value;
      end;
    UOutPosition:
      begin
        PestUOutFormula := Value;
      end;
    else
      begin
        inherited;
        Assert(False);
      end;
  end;
end;

procedure TSutraGeneralFlowBoundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
begin
  case FormulaIndex of
    LowerPressurePosition:
      begin
        PestBoundaryValueMethod := Value;
      end;
    LowerFlowRatePosition:
      begin
        PestLowerFlowRateMethod := Value;
      end;
    HigherPressurePosition:
      begin
        PestHigherPressureMethod := Value;
      end;
    HigherFlowRatePosition:
      begin
        PestHigherFlowRateMethod := Value;
      end;
    UInPosition:
      begin
        PestAssociatedValueMethod := Value;
      end;
    UOutPosition:
      begin
        PestUOutMethod := Value;
      end;
    else
      begin
        inherited;
        Assert(False);
      end;
  end;
end;

procedure TSutraGeneralFlowBoundary.SetPestHigherFlowRateFormula(
  const Value: string);
begin
  UpdateFormulaNodes(Value, HigherFlowRatePosition+SutraGenOffset,
    FPestHigherFlowRateFormula);
end;

procedure TSutraGeneralFlowBoundary.SetPestHigherFlowRateMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestHigherFlowRateMethod, Value);
end;

procedure TSutraGeneralFlowBoundary.SetPestHigherPressureFormula(
  const Value: string);
begin
  UpdateFormulaNodes(Value, HigherPressurePosition+SutraGenOffset,
    FPestHigherPressureFormula);
end;

procedure TSutraGeneralFlowBoundary.SetPestHigherPressureMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestHigherPressureMethod, Value);
end;

procedure TSutraGeneralFlowBoundary.SetPestLowerFlowRateFormula(
  const Value: string);
begin
  UpdateFormulaNodes(Value, LowerFlowRatePosition+SutraGenOffset,
    FPestLowerFlowRateFormula);
end;

procedure TSutraGeneralFlowBoundary.SetPestLowerFlowRateMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestLowerFlowRateMethod, Value);
end;

procedure TSutraGeneralFlowBoundary.SetPestUOutFormula(const Value: string);
begin
  UpdateFormulaNodes(Value, UoutPosition+SutraGenOffset,
    FPestUOutFormula);
end;

procedure TSutraGeneralFlowBoundary.SetPestUOutMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestUOutMethod, Value);
end;

procedure TSutraGeneralFlowBoundary.UChangeHandler(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateSutraGenFlowU1(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateSutraGenFlowU1(self);
      end;
    end;
  end;
end;

procedure TSutraGeneralFlowBoundary.UOutChangeHandler(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateSutraGenFlowU2(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateSutraGenFlowU2(self);
      end;
    end;
  end;
end;

{ TSutraGeneralFlowTimeLink }


{ TSutraGeneralFlowTimeLink }

procedure TSutraGeneralFlowTimeLink.CreateTimeLists;
begin
//  inherited;
  FLowPressTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FLowPressTimeList.NonParamDescription := 'Lower pressure or head value';
  FLowPressTimeList.ParamDescription := 'Lower pressure or head value';
  AddTimeList(FLowPressTimeList);
//  if Model <> nil then
//  begin
//    FLowPressTimeList.OnInvalidate := (Model as TCustomModel).InvalidateMfRchRate;
//  end;
  FLowFlowRateTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FLowFlowRateTimeList.NonParamDescription := 'Flow rate at lower pressure or head';
  FLowFlowRateTimeList.ParamDescription := 'Flow rate at lower pressure or head';
  AddTimeList(FLowFlowRateTimeList);

  FHighPressTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FHighPressTimeList.NonParamDescription := 'Higher pressure or head value';
  FHighPressTimeList.ParamDescription := 'Higher pressure or head value';
  AddTimeList(FHighPressTimeList);          //  if Model <> nil then
//  begin
//    FLowPressTimeList.OnInvalidate := (Model as TCustomModel).InvalidateMfRchRate;
//  end;
  FHighFlowRateTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FHighFlowRateTimeList.NonParamDescription := 'Flow rate at higher pressure or head';
  FHighFlowRateTimeList.ParamDescription := 'Flow rate at higher pressure or head';
  AddTimeList(FHighFlowRateTimeList);

  FInTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FInTimeList.NonParamDescription := 'Inflow concentration or temperature';
  FInTimeList.ParamDescription := 'Inflow concentration or temperature';
  AddTimeList(FInTimeList);
  //  Inflowif Model <> nil then
//  begin
//    FLowPressTimeList.OnInvalidate := (Model as TCustomModel).InvalidateMfRchRate;
//  end;
  FOutTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FOutTimeList.NonParamDescription := 'Outflow concentration or temperature';
  FOutTimeList.ParamDescription := 'Outflow concentration or temperature';
  AddTimeList(FOutTimeList);
end;

destructor TSutraGeneralFlowTimeLink.Destroy;
begin
  FLowPressTimeList.Free;
  FLowFlowRateTimeList.Free;
  FHighPressTimeList.Free;
  FHighFlowRateTimeList.Free;
  FInTimeList.Free;
  FOutTimeList.Free;
  inherited;
end;

end.
