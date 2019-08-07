unit ModflowSfrParamIcalcUnit;

interface

uses Classes, Math, GoPhastTypes, OrderedCollectionUnit, ModflowBoundaryUnit,
  SubscriptionUnit;

type
  TSfrParamIcalcItem = class(TCustomModflowBoundaryItem)
  private
    FICalc: integer;
    FParam: string;
    FParamInstance: string;
    FIPRIOR: integer;
    FOutflowSegment: integer;
    FDiversionSegment: integer;
    procedure SetICalc(const Value: integer);
    procedure SetParam(const Value: string);
    procedure SetParamInstance(const Value: string);
    function GetICalc: integer;
    function GetParam: string;
    function GetParamInstance: string;
    procedure SetDiversionSegment(const Value: integer);
    procedure SetIPRIOR(const Value: integer);
    procedure SetOutflowSegment(const Value: integer);
    procedure InvalidateSegmentNumberArray;
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
  published
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    property Param: string read GetParam write SetParam;
    property ParamInstance: string read GetParamInstance write SetParamInstance;
    property ICalc: integer read GetICalc write SetICalc;
    property OutflowSegment: integer read FOutflowSegment write SetOutflowSegment;
    property DiversionSegment: integer read FDiversionSegment write SetDiversionSegment;
    property IPRIOR: integer read FIPRIOR write SetIPRIOR;
  end;

  // @name represents MODFLOW Streamflow Routing boundaries
  // for a series of time intervals.
  TSfrParamIcalcCollection = class(TCustomNonSpatialBoundColl)
  private
    function GetItems(Index: integer): TSfrParamIcalcItem;
    procedure SetItems(Index: integer; const Value: TSfrParamIcalcItem);
  protected
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
  public
    function IcalcSet: TByteSet;
    function ICalc(Time: double): integer;
    function ParameterUsed(const ParamName: string): boolean;
    property Items[Index: integer]: TSfrParamIcalcItem read GetItems
      write SetItems;
    function GetItemByStartTime(StartTime: double): TSfrParamIcalcItem;
    procedure Assign(Source: TPersistent);override;
  end;


implementation

uses PhastModelUnit, ModflowSfrUnit, ScreenObjectUnit;

{ TSfrParamIcalcItem }

procedure TSfrParamIcalcItem.Assign(Source: TPersistent);
var
  Sfr : TSfrParamIcalcItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TSfrParamIcalcItem then
  begin
    Sfr := TSfrParamIcalcItem(Source);
    Param := Sfr.Param;
    ParamInstance := Sfr.ParamInstance;
    ICalc := Sfr.ICalc;
    OutflowSegment := Sfr.OutflowSegment;
    DiversionSegment := Sfr.DiversionSegment;
    IPRIOR := Sfr.IPRIOR;
  end;
  inherited;
end;

procedure TSfrParamIcalcItem.InvalidateSegmentNumberArray;
var
  BoundaryGroup: TSfrBoundary;
begin
  BoundaryGroup := (Collection as TSfrParamIcalcCollection).BoundaryGroup as TSfrBoundary;
  if BoundaryGroup <> nil then
  begin
    BoundaryGroup.InvalidateSegmentNumberArray;
  end;
end;

procedure TSfrParamIcalcItem.AssignObserverEvents(Collection: TCollection);
begin
  // do nothing
end;

function TSfrParamIcalcItem.BoundaryFormulaCount: integer;
begin
  result := 0;
end;

procedure TSfrParamIcalcItem.CreateFormulaObjects;
begin
  // do nothing
end;

function TSfrParamIcalcItem.GetBoundaryFormula(Index: integer): string;
begin
  Assert(False);
  result := '';
end;

function TSfrParamIcalcItem.GetICalc: integer;
var
  FirstValue: integer;
begin
  if (Model <> nil) and
    ((Model as TPhastModel).ModflowPackages.SfrPackage.Isfropt > 1)
    and (Index > 0) then
  begin
    FirstValue := ((Collection as TSfrParamIcalcCollection).Items[0]
      as TSfrParamIcalcItem).FICalc;
    if (FirstValue in [1,2]) or (FICalc in [1,2]) then
    begin
      result := FirstValue;
    end
    else
    begin
      result := FICalc;
    end;
  end
  else
  begin
    result := FICalc
  end;
end;

function TSfrParamIcalcItem.GetParam: string;
var
  PhastModel: TPhastModel;
begin
  PhastModel := Model as TPhastModel;
  if PhastModel = nil then
  begin
    result := FParam;
  end
  else if (PhastModel.ModflowPackages.SfrPackage.Isfropt > 0) then
  begin
    result := '';
  end
  else if PhastModel.ModflowPackages.SfrPackage.ParameterInstances.
      ParameterInstanceExists(FParam, FParamInstance) then
  begin
    result := FParam;
  end
  else
  begin
    result := '';
  end;
end;

function TSfrParamIcalcItem.GetParamInstance: string;
var
  PhastModel: TPhastModel;
begin
  PhastModel := Model as TPhastModel;
  if PhastModel = nil then
  begin
    result := FParamInstance;
  end
  else if (PhastModel.ModflowPackages.SfrPackage.Isfropt > 0) then
  begin
    result := '';
  end
  else if PhastModel.ModflowPackages.SfrPackage.ParameterInstances.
      ParameterInstanceExists(FParam, FParamInstance) then
  begin
    result := FParamInstance;
  end
  else
  begin
    result := '';
  end;
end;

procedure TSfrParamIcalcItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  // do nothing
end;

function TSfrParamIcalcItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Sfr: TSfrParamIcalcItem;
begin
  result := (AnotherItem is TSfrParamIcalcItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Sfr := TSfrParamIcalcItem(AnotherItem);
    result := (Param = Sfr.Param)
      and (ParamInstance = Sfr.ParamInstance)
      and (ICalc = Sfr.ICalc)
      and (OutflowSegment = Sfr.OutflowSegment)
      and (DiversionSegment = Sfr.DiversionSegment)
      and (IPRIOR = Sfr.IPRIOR);
  end;
end;

procedure TSfrParamIcalcItem.RemoveFormulaObjects;
begin
  // do nothing
end;

procedure TSfrParamIcalcItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  Assert(False);
end;

procedure TSfrParamIcalcItem.SetDiversionSegment(const Value: integer);
begin
  if FDiversionSegment <> Value then
  begin
    InvalidateModel;
    InvalidateSegmentNumberArray;
    FDiversionSegment := Value;
  end;
end;

procedure TSfrParamIcalcItem.SetICalc(const Value: integer);
var
  PhastModel : TPhastModel;
begin
  if FICalc <> Value then
  begin
    InvalidateModel;
    if (Model <> nil) then
    begin
      PhastModel := Model as TPhastModel;
      PhastModel.InvalidateMfSfrSegmentReachAndIcalc(self);
    end;
    FICalc := Value;
  end;
end;

procedure TSfrParamIcalcItem.SetIPRIOR(const Value: integer);
var
  LocalScreenObject: TScreenObject;
begin
  if FIPRIOR <> Value then
  begin
    InvalidateModel;
    LocalScreenObject := ScreenObject as TScreenObject;
    if (LocalScreenObject <> nil)
      and (LocalScreenObject as TScreenObject).CanInvalidateModel
      and (Model <> nil) then
    begin
      (Model as TPhastModel).InvalidateMfSfrIprior(self);
    end;
//    InvalidateSegmentNumberArray;
    if Value > 0 then
    begin
      // versions of ModelMuse prior to 2/23/2009 stored
      // positive values for IPRIOR when they should have stored
      // negative values.
      FIPRIOR := -Value;
    end
    else
    begin
      FIPRIOR := Value;
    end;

  end;
end;

procedure TSfrParamIcalcItem.SetOutflowSegment(const Value: integer);
begin
  if FOutflowSegment <> Value then
  begin
    InvalidateModel;
    InvalidateSegmentNumberArray;
    FOutflowSegment := Value;
  end;
end;

procedure TSfrParamIcalcItem.SetParam(const Value: string);
begin
  if FParam <> Value then
  begin
    InvalidateModel;
    FParam := Value;
  end;
end;

procedure TSfrParamIcalcItem.SetParamInstance(const Value: string);
begin
  if FParamInstance <> Value then
  begin
    InvalidateModel;
    FParamInstance := Value;
  end;
end;

{ TSfrParamIcalcCollection }
function CompareItems(Item1, Item2: Pointer): integer;
var
  Param1, Param2: TSfrParamIcalcItem;
begin
  Param1 := Item1;
  Param2 := Item2;
  result := Sign(Param1.StartTime - Param2.StartTime);
end;


procedure TSfrParamIcalcCollection.Assign(Source: TPersistent);
var
  List: TList;
  Index: Integer;
  Item: TSfrParamIcalcItem;
begin
  inherited;
  List := TList.Create;
  try
    for Index := 0 to Count - 1 do
    begin
      List.Add(Items[Index])
    end;
    List.Sort(CompareItems);
    for Index := 0 to List.Count - 1 do
    begin
      Item := List[Index];
      Item.Index := Index;
    end;
  finally
    List.Free;
  end;
end;

function TSfrParamIcalcCollection.GetItemByStartTime(
  StartTime: double): TSfrParamIcalcItem;
var
  Index: Integer;
  Item: TSfrParamIcalcItem;
begin
  result := nil;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
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

function TSfrParamIcalcCollection.GetItems(Index: integer): TSfrParamIcalcItem;
begin
  result := inherited Items[Index] as TSfrParamIcalcItem
end;

function TSfrParamIcalcCollection.ICalc(Time: double): integer;
var
  Index: Integer;
  Item: TSfrParamIcalcItem;
begin
  result := 0;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    if Item.StartTime > Time then
    begin
      Exit;
    end
    else
    begin
      result := Item.ICalc;
    end;
    if Time < Item.EndTime then
    begin
      Exit;
    end;
  end;
end;

function TSfrParamIcalcCollection.IcalcSet: TByteSet;
var
  AByte: Byte;
  Index: integer;
  Item: TSfrParamIcalcItem;
begin
  result := [];
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    Assert(Item.ICalc >= Low(Byte));
    Assert(Item.ICalc <= High(Byte));
    AByte := Item.ICalc;
    Include(result, AByte);
  end;
end;

class function TSfrParamIcalcCollection.ItemClass: TBoundaryItemClass;
begin
  result := TSfrParamIcalcItem;
end;

function TSfrParamIcalcCollection.ParameterUsed(
  const ParamName: string): boolean;
var
  Index: Integer;
  Item: TSfrParamIcalcItem;
begin
  result := false;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    result := Item.Param = ParamName;
    if result then
    begin
      Exit;
    end;
  end;
end;

procedure TSfrParamIcalcCollection.SetItems(Index: integer;
  const Value: TSfrParamIcalcItem);
begin
  inherited Items[Index] := Value;
end;

end.

