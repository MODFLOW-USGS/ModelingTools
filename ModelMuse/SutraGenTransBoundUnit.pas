unit SutraGenTransBoundUnit;

interface

uses
  System.Classes, ModflowBoundaryUnit, FormulaManagerUnit, GoPhastTypes,
  OrderedCollectionUnit, RbwParser, SutraBoundaryUnit,
  System.Generics.Collections, SutraOptionsUnit, SubscriptionUnit;

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
      PestSeriesMethod: TPestParamMethod; TimeSeriesName: string); override;
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
    FPestHigherFlowUMethod: TPestParamMethod;
    FPestHigherUMethod: TPestParamMethod;
    FPestLowerFlowUMethod: TPestParamMethod;
    FPestHigherUFormula: TFormulaObject;
    FPestLowerFlowUFormula: TFormulaObject;
    FPestHigherFlowUFormula: TFormulaObject;
    FPestHigherFlowUObserver: TObserver;
    FPestHigherUObserver: TObserver;
    FPestLowerFlowUObserver: TObserver;
//    FPestLowerUObserver: TObserver;
    procedure SetLakeInteractionType(
      const Value: TGeneralizedTransportInteractionType);
    function GetPestHigherFlowUFormula: string;
    function GetPestHigherFlowUObserver: TObserver;
    function GetPestHigherUFormula: string;
    function GetPestHigherUObserver: TObserver;
    function GetPestLowerFlowUFormula: string;
    function GetPestLowerFlowUObserver: TObserver;
    procedure SetPestHigherFlowUFormula(const Value: string);
    procedure SetPestHigherFlowUMethod(const Value: TPestParamMethod);
    procedure SetPestHigherUFormula(const Value: string);
    procedure SetPestHigherUMethod(const Value: TPestParamMethod);
    procedure SetPestLowerFlowUFormula(const Value: string);
    procedure SetPestLowerFlowUMethod(const Value: TPestParamMethod);
  protected
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    class function BoundaryCollectionClass: TMF_BoundCollClass;
      override;
    // PEST
    procedure PQChangeHandler(Sender: TObject); override;
    procedure UChangeHandler(Sender: TObject); override;
    function BoundaryObserverPrefix: string; override;
    procedure HigherUChangeHandler(Sender: TObject);
    procedure LowerFlowUChangeHandler(Sender: TObject);
    procedure HigherFlowUChangeHandler(Sender: TObject);
    procedure CreateFormulaObjects; override;
    procedure CreateObservers; override;
    function GetPestBoundaryFormula(FormulaIndex: integer): string; override;
    procedure SetPestBoundaryFormula(FormulaIndex: integer;
      const Value: string); override;
    function GetPestBoundaryMethod(FormulaIndex: integer): TPestParamMethod; override;
    procedure SetPestBoundaryMethod(FormulaIndex: integer;
      const Value: TPestParamMethod); override;
    property PestHigherUObserver: TObserver read GetPestHigherUObserver;
    property PestLowerFlowUObserver: TObserver read GetPestLowerFlowUObserver;
    property PestHigherFlowUObserver: TObserver read GetPestHigherFlowUObserver;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
  published
    property LakeInteractionType: TGeneralizedTransportInteractionType
      read FLakeInteractionType write SetLakeInteractionType default gtitUseDefaults;
    // PestAssociatedValueFormula is used with LowerUFormula,UBG1

    property PestHigherUFormula: string read GetPestHigherUFormula
      write SetPestHigherUFormula
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestHigherUMethod: TPestParamMethod read FPestHigherUMethod
      write SetPestHigherUMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestLowerFlowUFormula: string read GetPestLowerFlowUFormula
      write SetPestLowerFlowUFormula
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestLowerFlowUMethod: TPestParamMethod read FPestLowerFlowUMethod
      write SetPestLowerFlowUMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestHigherFlowUFormula: string read GetPestHigherFlowUFormula
      write SetPestHigherFlowUFormula
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestHigherFlowUMethod: TPestParamMethod read FPestHigherFlowUMethod
      write SetPestHigherFlowUMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
  end;

  TSutraGeneralTransBoundaryList = TList<TSutraGeneralTransportBoundary>;

const
  UsedFormulaPosition = 0;
  LowerUPosition = 1;
  LowerFlowUPosition = 2;
  HigherUPosition = 3;
  HigherFlowUPosition = 4;
  SutraGenTransOffset = 2;

implementation

uses
  frmGoPhastUnit, PhastModelUnit, ScreenObjectUnit;

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
  PestName: string; PestSeriesName: string; PestSeriesMethod: TPestParamMethod;
  TimeSeriesName: string);
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
var
  GenTranSource: TSutraGeneralTransportBoundary;
begin
  if Source is TSutraGeneralTransportBoundary then
  begin
    GenTranSource := TSutraGeneralTransportBoundary(Source);
    LakeInteractionType := GenTranSource.LakeInteractionType;

    PestHigherUFormula := GenTranSource.PestHigherUFormula;
    PestHigherUMethod := GenTranSource.PestHigherUMethod;
    PestLowerFlowUFormula := GenTranSource.PestLowerFlowUFormula;
    PestLowerFlowUMethod := GenTranSource.PestLowerFlowUMethod;
    PestHigherFlowUFormula := GenTranSource.PestHigherFlowUFormula;
    PestHigherFlowUMethod := GenTranSource.PestHigherFlowUMethod;
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

function TSutraGeneralTransportBoundary.BoundaryObserverPrefix: string;
begin
  result := 'PestGeneralizedTransport_';
end;

constructor TSutraGeneralTransportBoundary.Create(Model: TBaseModel;
  ScreenObject: TObject);
var
  PestIndex: Integer;
begin
  inherited;
  FLakeInteractionType := gtitUseDefaults;

  for PestIndex := LowerUPosition to HigherFlowUPosition do
  begin
    PestBoundaryFormula[PestIndex] := '';
    PestBoundaryMethod[PestIndex] := DefaultBoundaryMethod(PestIndex);
  end;
end;

procedure TSutraGeneralTransportBoundary.CreateFormulaObjects;
begin
  inherited;
  FPestLowerFlowUFormula := CreateFormulaObjectNodes(dso3D);
  FPestHigherUFormula := CreateFormulaObjectNodes(dso3D);
  FPestHigherFlowUFormula := CreateFormulaObjectNodes(dso3D);
end;

procedure TSutraGeneralTransportBoundary.CreateObservers;
begin
  inherited;
  if ScreenObject <> nil then
  begin
    FObserverList.Add(PestLowerFlowUObserver);
    FObserverList.Add(PestHigherUObserver);
    FObserverList.Add(PestHigherFlowUObserver);
  end;
end;

class function TSutraGeneralTransportBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    UsedFormulaPosition:
      begin
        result := inherited;
      end;
    LowerUPosition:
      begin
        result := ppmMultiply
      end;
    LowerFlowUPosition:
      begin
        result := ppmMultiply
      end;
    HigherUPosition:
      begin
        result := ppmMultiply
      end;
    HigherFlowUPosition:
      begin
        result := ppmMultiply
      end;
    else
      begin
        result := inherited;
        Assert(False);
      end;
  end;
end;

destructor TSutraGeneralTransportBoundary.Destroy;
var
  PestIndex: Integer;
begin
  for PestIndex := LowerUPosition to HigherFlowUPosition do
  begin
    PestBoundaryFormula[PestIndex] := '';
  end;
  inherited;
end;

procedure TSutraGeneralTransportBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
begin
  inherited;
  // does this need to change?
  Assert(False);
end;

function TSutraGeneralTransportBoundary.GetPestBoundaryFormula(
  FormulaIndex: integer): string;
begin
  case FormulaIndex of
    UsedFormulaPosition:
      begin
        result := inherited;
      end;
    LowerUPosition:
      begin
        result := PestAssociatedValueFormula
      end;
    LowerFlowUPosition:
      begin
        result := PestLowerFlowUFormula
      end;
    HigherUPosition:
      begin
        result := PestHigherUFormula
      end;
    HigherFlowUPosition:
      begin
        result := PestHigherFlowUFormula
      end;
    else
      begin
        result := inherited;
        Assert(False);
      end;
  end;
end;

function TSutraGeneralTransportBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    UsedFormulaPosition:
      begin
        result := inherited;
      end;
    LowerUPosition:
      begin
        result := PestAssociatedValueMethod
      end;
    LowerFlowUPosition:
      begin
        result := PestLowerFlowUMethod
      end;
    HigherUPosition:
      begin
        result := PestHigherUMethod
      end;
    HigherFlowUPosition:
      begin
        result := PestHigherFlowUMethod
      end;
    else
      begin
        result := inherited;
        Assert(False);
      end;
  end;
end;

function TSutraGeneralTransportBoundary.GetPestHigherFlowUFormula: string;
begin
  Result := FPestHigherFlowUFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(HigherFlowUPosition+SutraGenTransOffset);
  end;
end;

function TSutraGeneralTransportBoundary.GetPestHigherFlowUObserver: TObserver;
begin
  if FPestHigherFlowUObserver = nil then
  begin
    CreateObserver('PestHigherFlowU_', FPestHigherFlowUObserver, nil);
    FPestHigherFlowUObserver.OnUpToDateSet := HigherFlowUChangeHandler;
  end;
  result := FPestHigherFlowUObserver;
end;

function TSutraGeneralTransportBoundary.GetPestHigherUFormula: string;
begin
  Result := FPestHigherUFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(HigherUPosition+SutraGenTransOffset);
  end;
end;

function TSutraGeneralTransportBoundary.GetPestHigherUObserver: TObserver;
begin
  if FPestHigherUObserver = nil then
  begin
    CreateObserver('PestHigherU_', FPestHigherUObserver, nil);
    FPestHigherUObserver.OnUpToDateSet := HigherUChangeHandler;
  end;
  result := FPestHigherUObserver;
end;

function TSutraGeneralTransportBoundary.GetPestLowerFlowUFormula: string;
begin
  Result := FPestLowerFlowUFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(LowerUPosition+SutraGenTransOffset);
  end;
end;

function TSutraGeneralTransportBoundary.GetPestLowerFlowUObserver: TObserver;
begin
  if FPestLowerFlowUObserver = nil then
  begin
    CreateObserver('PestLowerFlowU_', FPestLowerFlowUObserver, nil);
    FPestLowerFlowUObserver.OnUpToDateSet := LowerFlowUChangeHandler;
  end;
  result := FPestLowerFlowUObserver;
end;

procedure TSutraGeneralTransportBoundary.HigherFlowUChangeHandler(
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
    PhastModel.InvalidateSutraGenTransQU2(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateSutraGenTransQU2(self);
    end;
  end;
end;

procedure TSutraGeneralTransportBoundary.HigherUChangeHandler(Sender: TObject);
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
    PhastModel.InvalidateSutraGenTransU2(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateSutraGenTransU2(self);
    end;
  end;
end;

procedure TSutraGeneralTransportBoundary.LowerFlowUChangeHandler(
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
    PhastModel.InvalidateSutraGenTransQU1(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateSutraGenTransQU1(self);
    end;
  end;
end;

procedure TSutraGeneralTransportBoundary.PQChangeHandler(Sender: TObject);
begin
//  inherited;

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

procedure TSutraGeneralTransportBoundary.SetPestBoundaryFormula(
  FormulaIndex: integer; const Value: string);
begin
  case FormulaIndex of
    UsedFormulaPosition:
      begin
        inherited;
      end;
    LowerUPosition:
      begin
        PestAssociatedValueFormula := Value;
      end;
    LowerFlowUPosition:
      begin
        PestLowerFlowUFormula := Value;
      end;
    HigherUPosition:
      begin
        PestHigherUFormula := Value;
      end;
    HigherFlowUPosition:
      begin
        PestHigherFlowUFormula := Value;
      end;
    else
      begin
        inherited;
        Assert(False);
      end;
  end;
end;

procedure TSutraGeneralTransportBoundary.SetPestBoundaryMethod(
  FormulaIndex: integer; const Value: TPestParamMethod);
begin
  case FormulaIndex of
    UsedFormulaPosition:
      begin
        inherited;
      end;
    LowerUPosition:
      begin
        PestAssociatedValueMethod := Value;
      end;
    LowerFlowUPosition:
      begin
        PestLowerFlowUMethod := Value;
      end;
    HigherUPosition:
      begin
        PestHigherUMethod := Value;
      end;
    HigherFlowUPosition:
      begin
        PestHigherFlowUMethod := Value;
      end;
    else
      begin
        inherited;
        Assert(False);
      end;
  end;
end;

procedure TSutraGeneralTransportBoundary.SetPestHigherFlowUFormula(
  const Value: string);
begin
  UpdateFormulaNodes(Value, HigherFlowUPosition+SutraGenTransOffset,
    FPestHigherFlowUFormula);
end;

procedure TSutraGeneralTransportBoundary.SetPestHigherFlowUMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestHigherFlowUMethod, Value);
end;

procedure TSutraGeneralTransportBoundary.SetPestHigherUFormula(
  const Value: string);
begin
  UpdateFormulaNodes(Value, HigherUPosition+SutraGenTransOffset,
    FPestHigherUFormula);
end;

procedure TSutraGeneralTransportBoundary.SetPestHigherUMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestHigherUMethod, Value);
end;

procedure TSutraGeneralTransportBoundary.SetPestLowerFlowUFormula(
  const Value: string);
begin
  UpdateFormulaNodes(Value, LowerFlowUPosition+SutraGenTransOffset,
    FPestLowerFlowUFormula);
end;

procedure TSutraGeneralTransportBoundary.SetPestLowerFlowUMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestLowerFlowUMethod, Value);
end;

procedure TSutraGeneralTransportBoundary.UChangeHandler(Sender: TObject);
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
    PhastModel.InvalidateSutraGenTransU1(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateSutraGenTransU1(self);
    end;
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
