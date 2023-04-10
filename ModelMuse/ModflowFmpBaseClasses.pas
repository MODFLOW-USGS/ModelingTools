unit ModflowFmpBaseClasses;

interface

uses
ModflowBoundaryUnit, FormulaManagerUnit, Classes,
  OrderedCollectionUnit, SysUtils, GoPhastTypes, RealListUnit;

type
  TCustomFarmItem = class(TCustomModflowBoundaryItem)
  protected
    FFormulaObjects: array of TFormulaObject;
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    // @name checks whether AnotherItem is the same as the current @classname.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function GetBoundaryFormula(Index: integer): string; override;
    procedure SetBoundaryFormula(Index: integer; const Value: string);
      override;
  public
    // @name copies Source to this @classname.
    procedure CreateFormulaObjects; override;
    procedure Assign(Source: TPersistent); override;
  end;

  // descendents need to override BoundaryFormulaCount
  // and specify the properties
  TCustomDefaultFormulaItem = class(TCustomFarmItem)
  protected
    class function DefaultFormula: string; virtual;
    procedure SetFormulasToDefault;
  public
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
  end;

  TCustomZeroFarmItem = TCustomDefaultFormulaItem;

  TCustomFarmCollection = class(TCustomNonSpatialBoundColl)
  public
    constructor Create(Model: TBaseModel); reintroduce; virtual;
  end;

  // @name is used to define list properties for MODFLOW OWHM version 2.
  TOwhmItem = class(TCustomDefaultFormulaItem)
  private
  const
    OwhmValuePosition = 0;
  protected
    function BoundaryFormulaCount: integer; override;
  published
    property OwhmValue: string index OwhmValuePosition read GetBoundaryFormula
      write SetBoundaryFormula;
  end;

  // @name is used to define list properties for MODFLOW OWHM version 2.
  TOwhmCollection = class(TCustomFarmCollection)
  private
    FOwhmNames: TStrings;
    function GetItem(Index: Integer): TOwhmItem;
    procedure SetItem(Index: Integer; const Value: TOwhmItem);
    procedure SetOwhmNames(const Value: TStrings);
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    constructor Create(Model: TBaseModel); override;
    destructor Destroy; override;
    function IsSame(AnOrderedCollection: TOrderedCollection): boolean; override;
    property Items[Index: Integer]: TOwhmItem read GetItem
      write SetItem;  default;
    function First: TOwhmItem;
    Property OwhmNames: TStrings read FOwhmNames write SetOwhmNames;
 end;

  TCustomOneFarmItem = class(TOwhmItem)
  protected
    class function DefaultFormula: string; override;
  end;

  TBoolFarmItem = class(TOwhmItem)
  protected
    class function DefaultFormula: string; override;
  published
  end;

  TBoolFarmCollection = class(TOwhmCollection)
  protected
    class function ItemClass: TBoundaryItemClass; override;
  end;

implementation

uses
  frmGoPhastUnit, PhastModelUnit, RbwParser;

{ TCustomFarmItem }

procedure TCustomFarmItem.Assign(Source: TPersistent);
var
  Index: integer;
  OtherItem: TCustomFarmItem;
begin
  if Source is TCustomFarmItem then
  begin
    OtherItem := TCustomFarmItem(Source);
    for Index := 0 to BoundaryFormulaCount - 1 do
    begin
      BoundaryFormula[Index] := OtherItem.BoundaryFormula[Index];
    end;
  end;
  inherited;
end;

procedure TCustomFarmItem.AssignObserverEvents(Collection: TCollection);
begin
  // do nothing
end;

procedure TCustomFarmItem.CreateFormulaObjects;
var
  Index: integer;
begin
  SetLength(FFormulaObjects, BoundaryFormulaCount);
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    FFormulaObjects[Index] := CreateFormulaObject(dso3D);
  end;
end;

function TCustomFarmItem.GetBoundaryFormula(Index: integer): string;
begin
  Assert(Index >= 0);
  Assert(Index < BoundaryFormulaCount);
  Result := FFormulaObjects[Index].Formula;
  ResetItemObserver(Index);
end;

procedure TCustomFarmItem.GetPropertyObserver(Sender: TObject; List: TList);
var
  Index: integer;
begin
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    if Sender = FFormulaObjects[Index] then
    begin
      List.Add(FObserverList[Index]);
      Break;
    end;
  end;
end;

function TCustomFarmItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  OtherItem: TCustomFarmItem;
  Index: integer;
begin
  result := (AnotherItem is TCustomFarmItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    OtherItem := TCustomFarmItem(AnotherItem);
    for Index := 0 to BoundaryFormulaCount - 1 do
    begin
      result := BoundaryFormula[Index] = OtherItem.BoundaryFormula[Index];
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TCustomFarmItem.RemoveFormulaObjects;
var
  Index: integer;
begin
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    frmGoPhast.PhastModel.FormulaManager.Remove(FFormulaObjects[Index],
      GlobalRemoveModflowBoundaryItemSubscription,
      GlobalRestoreModflowBoundaryItemSubscription, self);
  end;
end;

procedure TCustomFarmItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  Assert(Index >= 0);
  Assert(Index < BoundaryFormulaCount);
  if FFormulaObjects[Index].Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, Index, FFormulaObjects[Index]);
  end;
end;

{ TCustomZeroFarmItem }


{ TCustomFarmCollection }

constructor TCustomFarmCollection.Create(Model: TBaseModel);
begin
  inherited Create(nil, Model, nil);
end;
{ TOwhmItem }

function TOwhmItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

{ TOwhmCollection }

constructor TOwhmCollection.Create(Model: TBaseModel);
begin
  inherited;
  FOwhmNames := TStringList.Create;
end;

destructor TOwhmCollection.Destroy;
begin
  FOwhmNames.Free;
  inherited;
end;

function TOwhmCollection.First: TOwhmItem;
begin
  result := inherited First as TOwhmItem;
end;

function TOwhmCollection.GetItem(Index: Integer): TOwhmItem;
begin
  result := inherited Items[index] as TOwhmItem;
end;

function TOwhmCollection.IsSame(
  AnOrderedCollection: TOrderedCollection): boolean;
begin
  result := (AnOrderedCollection is TOwhmCollection)
    and inherited IsSame(AnOrderedCollection);
end;

class function TOwhmCollection.ItemClass: TBoundaryItemClass;
begin
  result := TOwhmItem;
end;

procedure TOwhmCollection.SetItem(Index: Integer; const Value: TOwhmItem);
begin
  inherited Items[index] := Value;
end;

procedure TOwhmCollection.SetOwhmNames(const Value: TStrings);
begin
  FOwhmNames.Assign(Value);
end;

{ TCustomDefaultFormulaItem }

constructor TCustomDefaultFormulaItem.Create(Collection: TCollection);
begin
  inherited;
  SetFormulasToDefault;
end;

class function TCustomDefaultFormulaItem.DefaultFormula: string;
begin
  result := '0';
end;

destructor TCustomDefaultFormulaItem.Destroy;
begin
  SetFormulasToDefault;
  inherited;
end;

procedure TCustomDefaultFormulaItem.SetFormulasToDefault;
var
  Index: integer;
begin
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    BoundaryFormula[Index] := DefaultFormula;
  end;
end;

{ TCustomOneFarmItem }

class function TCustomOneFarmItem.DefaultFormula: string;
begin
  result := '1';
end;

{ TBoolFarmItem }

class function TBoolFarmItem.DefaultFormula: string;
begin
  result := 'False';
end;

{ TBoolFarmCollection }

class function TBoolFarmCollection.ItemClass: TBoundaryItemClass;
begin
  result := TBoolFarmItem;
end;

end.
