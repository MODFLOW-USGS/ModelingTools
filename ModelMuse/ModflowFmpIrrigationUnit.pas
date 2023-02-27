unit ModflowFmpIrrigationUnit;

interface

uses
  System.Classes, OrderedCollectionUnit, GoPhastTypes;

type
  TIrrigationItem = class(TOrderedItem)
  private
    FName: string;
    procedure SetName(const Value: string);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Name: string read FName write SetName;
  end;

  TIrrigationCollection = class(TEnhancedOrderedCollection)
  private
    function GetItems(Index: Integer): TIrrigationItem;
    procedure SetItems(Index: Integer; const Value: TIrrigationItem);
  public
    constructor Create(Model: TBaseModel);
    property Items[Index: Integer]: TIrrigationItem read GetItems
      write SetItems; default;
  end;

implementation

{ TIrrigationItem }

procedure TIrrigationItem.Assign(Source: TPersistent);
begin
  if Source is TIrrigationItem then
  begin
    Name := TIrrigationItem(Source).Name;
  end;
  inherited;
end;

function TIrrigationItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := (AnotherItem is TIrrigationItem)
    and (Name  = TIrrigationItem(AnotherItem).Name);
end;

procedure TIrrigationItem.SetName(const Value: string);
begin
  SetCaseSensitiveStringProperty(FName, Value);
end;

{ TIrrigationCollection }

constructor TIrrigationCollection.Create(Model: TBaseModel);
begin
  inherited Create(TIrrigationItem, Model);
end;

function TIrrigationCollection.GetItems(Index: Integer): TIrrigationItem;
begin
  result := inherited Items[index] as TIrrigationItem
end;

procedure TIrrigationCollection.SetItems(Index: Integer;
  const Value: TIrrigationItem);
begin
  inherited Items[index] := Value;
end;

end.
