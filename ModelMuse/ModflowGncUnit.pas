unit ModflowGncUnit;

interface

uses
  System.Classes, GoPhastTypes;

type
  TCellId = class(TCollectionItem)
  private
    FCell: Integer;
    procedure SetCell(const Value: Integer);
  public
    procedure Assign(Source: TPersistent); override;
  published
    // @name starts at 0.
    property Cell: Integer read FCell write SetCell;
  end;

  TWeightedCellId = class(TCellId)
  private
    FStoredWeight: TRealStorage;
    procedure SetStoredWeight(const Value: TRealStorage);
    function GetWeight: double;
    procedure SetWeight(const Value: double);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Weight: double read GetWeight write SetWeight;
  published
    property StoredWeight: TRealStorage read FStoredWeight write SetStoredWeight;
  end;

  TCellIdCollection = class(TCollection)
  private
    function GetItems(Index: Integer): TWeightedCellId;
    procedure SetItems(Index: Integer; const Value: TWeightedCellId);
  public
    constructor Create;
    function Add: TWeightedCellId;
    property Items[Index: Integer]: TWeightedCellId read GetItems
      write SetItems; default;
    function GetCellByID(ID: integer): TWeightedCellId;
  end;

  TGhostNode = class(TCollectionItem)
  private
    FLinkedCell: TCellId;
    FContainingCell: TCellId;
    FCellWeights: TCellIdCollection;
    procedure SetContainingCell(const Value: TCellId);
    procedure SetLinkedCell(const Value: TCellId);
    procedure SetCellWeights(const Value: TCellIdCollection);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ContainingCell: TCellId read FContainingCell write SetContainingCell;
    property LinkedCell: TCellId read FLinkedCell write SetLinkedCell;
    property CellWeights: TCellIdCollection read FCellWeights write SetCellWeights;
  end;

  TGhostNodeArray = array of TGhostNode;

  TGhostNodes = class(TCollection)
  private
    FGhostNodeByCell: array of TGhostNodeArray;
    function GetItems(Index: Integer): TGhostNode;
    procedure SetItems(Index: Integer; const Value: TGhostNode);
    function GetGhostNodesByCell(CellID: Integer): TGhostNodeArray;
  public
    procedure Clear;
    constructor Create;
    function Add: TGhostNode;
    property Items[Index: Integer]: TGhostNode read GetItems write SetItems; default;
    property GhostNodesByCell[CellID: Integer]: TGhostNodeArray
      read GetGhostNodesByCell;
  end;

implementation

{ TCellId }

procedure TCellId.Assign(Source: TPersistent);
var
  SourceID: TCellId;
begin
  if Source is TCellId then
  begin
    SourceID := TCellId(Source);
    Cell := SourceID.Cell;
  end
  else
  begin
    inherited;
  end;

end;

procedure TCellId.SetCell(const Value: Integer);
begin
  FCell := Value;
end;

{ TCellIdCollection }

function TCellIdCollection.Add: TWeightedCellId;
begin
  result := inherited Add as TWeightedCellId;
end;

constructor TCellIdCollection.Create;
begin
  inherited Create(TWeightedCellId);
end;

function TCellIdCollection.GetCellByID(ID: integer): TWeightedCellId;
var
  CellIndex: Integer;
begin
  Assert(ID >= 0);
  result := nil;
  for CellIndex := 0 to Count - 1 do
  begin
    if Items[CellIndex].Cell = ID then
    begin
      result := Items[CellIndex];
      break;
    end;
  end;
end;

function TCellIdCollection.GetItems(Index: Integer): TWeightedCellId;
begin
  result := inherited Items[Index] as TWeightedCellId
end;

procedure TCellIdCollection.SetItems(Index: Integer;
  const Value: TWeightedCellId);
begin
  inherited Items[Index] := Value;
end;

{ TWeightedCellId }

procedure TWeightedCellId.Assign(Source: TPersistent);
begin
  if Source is TWeightedCellId then
  begin
    Weight := TWeightedCellId(Source).Weight;
  end;
  inherited;
end;

constructor TWeightedCellId.Create(Collection: TCollection);
begin
  inherited;
  FStoredWeight := TRealStorage.Create;
end;

destructor TWeightedCellId.Destroy;
begin
  FStoredWeight.Free;
  inherited;
end;

function TWeightedCellId.GetWeight: double;
begin
  result := StoredWeight.Value;
end;

procedure TWeightedCellId.SetStoredWeight(const Value: TRealStorage);
begin
  FStoredWeight.Assign(Value);
end;

procedure TWeightedCellId.SetWeight(const Value: double);
begin
  StoredWeight.Value := Value;
end;

{ TGhostNode }

procedure TGhostNode.Assign(Source: TPersistent);
var
  GhostSource: TGhostNode;
begin
  if Source is TGhostNode then
  begin
    GhostSource := TGhostNode(Source);
    ContainingCell := GhostSource.ContainingCell;
    LinkedCell := GhostSource.LinkedCell;
    CellWeights := GhostSource.CellWeights;
  end
  else
  begin
    inherited;
  end;
end;

constructor TGhostNode.Create(Collection: TCollection);
begin
  inherited;
  FContainingCell := TCellId.Create(nil);
  FLinkedCell := TCellId.Create(nil);
  FCellWeights := TCellIdCollection.Create;
end;

destructor TGhostNode.Destroy;
begin
  FCellWeights.Free;
  FLinkedCell.Free;
  FContainingCell.Free;
  inherited;
end;

procedure TGhostNode.SetCellWeights(const Value: TCellIdCollection);
begin
  FCellWeights.Assign(Value);
end;

procedure TGhostNode.SetContainingCell(const Value: TCellId);
begin
  FContainingCell.Assign(Value);
end;

procedure TGhostNode.SetLinkedCell(const Value: TCellId);
begin
  FLinkedCell.Assign(Value);
end;

{ TGhostNodes }

function TGhostNodes.Add: TGhostNode;
begin
  result := inherited Add as TGhostNode;
end;

procedure TGhostNodes.Clear;
begin
  inherited;
  SetLength(FGhostNodeByCell, 0);
end;

constructor TGhostNodes.Create;
begin
  inherited Create(TGhostNode)
end;

function TGhostNodes.GetGhostNodesByCell(CellID: Integer): TGhostNodeArray;
var
  ItemIndex: Integer;
  AGhostNode: TGhostNode;
  MaxCellID: Integer;
  ArrayCount: Integer;
begin
  if Length(FGhostNodeByCell) = 0 then
  begin
    MaxCellID := 0;
    for ItemIndex := 0 to Count - 1 do
    begin
      AGhostNode := Items[ItemIndex];
      if AGhostNode.FContainingCell.Cell > MaxCellID then
      begin
        MaxCellID := AGhostNode.FContainingCell.Cell;
      end;
    end;
    SetLength(FGhostNodeByCell, MaxCellID+1);
    for ItemIndex := 0 to MaxCellID do
    begin
      FGhostNodeByCell[ItemIndex] := nil;
    end;

    for ItemIndex := 0 to Count - 1 do
    begin
      AGhostNode := Items[ItemIndex];
      ArrayCount := Length(FGhostNodeByCell[AGhostNode.FContainingCell.Cell]);
      SetLength(FGhostNodeByCell[AGhostNode.FContainingCell.Cell], ArrayCount+1);
      FGhostNodeByCell[AGhostNode.FContainingCell.Cell, ArrayCount] := AGhostNode;
    end;
  end;
  result := nil;
  if CellID < Length(FGhostNodeByCell) then
  begin
    result := FGhostNodeByCell[CellID];
  end;

end;

function TGhostNodes.GetItems(Index: Integer): TGhostNode;
begin
  result := inherited Items[Index] as TGhostNode;
end;

procedure TGhostNodes.SetItems(Index: Integer; const Value: TGhostNode);
begin
  inherited Items[Index] := Value;
end;

end.

