unit RbwBinomialPriorityQueueUnit;

interface

uses
  LinkedListUnit, System.Generics.Collections;

type
  TRbwBinomialPriorityQueue<T> = class(TDoubleLinkedList<T>)
  private
    FChildren: TRbwBinomialPriorityQueue<T>;
    FOrder: Integer;
    FParent: TRbwBinomialPriorityQueue<T>;
    function NextQueue: TRbwBinomialPriorityQueue<T>;
    function PriorQueue: TRbwBinomialPriorityQueue<T>;
    function GetCount: Integer;
    procedure SetChildren(const NewChild: TRbwBinomialPriorityQueue<T>);
    procedure SetOrder(const Value: Integer);
    procedure MergeWithNeighbors;
  protected
    {
    @name returns > 0 if Left has a higher priority than Right.
    @name returns 0 if Left has an equal priority to Right.
    @name returns < 0 if Left has a lower priority than Right.
    }
    function Compare (Left, Right: T): Integer; virtual; abstract;
    property Children: TRbwBinomialPriorityQueue<T> read FChildren write SetChildren;
    property Order: Integer read FOrder write SetOrder;
    procedure MergeTree(p, q: TRbwBinomialPriorityQueue<T>);
    procedure AddSubTree(Child: TRbwBinomialPriorityQueue<T>);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Enqueue(Item: T);
    property Count: Integer read GetCount;
    function Dequeue: T;
    function Peek: T;
  end;

  TRbwBinomialTree<T> = class(TObject)
  private
    FData: T;
    FChildren: TList<TRbwBinomialTree<T>>;
    FRank: Integer;
    procedure Link(OtherTree: TRbwBinomialTree<T>);
  protected
    function Compare (Left, Right: T): Integer; virtual; abstract;
  public
    constructor Create(Value: T);
    destructor Destroy; override;
    property Data: T read FData;
    property Rank: Integer read FRank;
  end;

  TRbwBinomialQueue = class(TObject)

  end;

implementation

{ TRbwBinomialPriorityQueue<T> }

procedure TRbwBinomialPriorityQueue<T>.AddSubTree(
  Child: TRbwBinomialPriorityQueue<T>);
begin
  if FChildren = nil then
  begin
    Children := Child;
  end
  else
  begin

  end;
end;

procedure TRbwBinomialPriorityQueue<T>.Clear;
begin

end;

constructor TRbwBinomialPriorityQueue<T>.Create;
begin

end;

function TRbwBinomialPriorityQueue<T>.Dequeue: T;
begin

end;

destructor TRbwBinomialPriorityQueue<T>.Destroy;
begin

  inherited;
end;

procedure TRbwBinomialPriorityQueue<T>.Enqueue(Item: T);
begin

end;

function TRbwBinomialPriorityQueue<T>.GetCount: Integer;
begin

end;

procedure TRbwBinomialPriorityQueue<T>.MergeTree(p,
  q: TRbwBinomialPriorityQueue<T>);
begin

end;

function TRbwBinomialPriorityQueue<T>.NextQueue: TRbwBinomialPriorityQueue<T>;
begin
  Result := Next as TRbwBinomialPriorityQueue<T>;
end;

function TRbwBinomialPriorityQueue<T>.Peek: T;
begin

end;

procedure TRbwBinomialPriorityQueue<T>.MergeWithNeighbors;
var
  NQueue: TRbwBinomialPriorityQueue<T>;
  PQueue: TRbwBinomialPriorityQueue<T>;
  NeedToUpdateParent: Boolean;
begin
  NeedToUpdateParent := (FParent <> nil) and (FParent.Children = Self);
  while Next <> nil do
  begin
    NQueue := NextQueue;
    if NQueue.Order < Order then
    begin
      PQueue := PriorQueue;
      if PQueue <> nil then
      begin
        PQueue.Next := NQueue;
        Next := NQueue.Next;
        NQueue.Next := self;
      end;
    end
    else if NQueue.Order = Order then
    begin
      MergeTree(NQueue, Self);
      Order := Order + 1;
      Break;
    end
    else
    begin
      Break;
    end;
  end;
  if (Next = nil) and (FParent <> nil) then
  begin
    FParent.Order := Order + 1;
  end;
  if True then
  begin

  end;
end;

function TRbwBinomialPriorityQueue<T>.PriorQueue: TRbwBinomialPriorityQueue<T>;
begin
  Result := Previous as TRbwBinomialPriorityQueue<T>;
end;

procedure TRbwBinomialPriorityQueue<T>.SetChildren(
  const NewChild: TRbwBinomialPriorityQueue<T>);
begin
  if FChildren <> nil then
  begin
    NewChild.Next := FChildren;
  end;
  FChildren := NewChild;
  if FChildren <> nil then
  begin
    FChildren.MergeWithNeighbors;
  end;
end;

procedure TRbwBinomialPriorityQueue<T>.SetOrder(const Value: Integer);
begin
  FOrder := Value;
  MergeWithNeighbors;
end;

{ TRbwBinomialTree<T> }

constructor TRbwBinomialTree<T>.Create(Value: T);
begin
  FData := Value;
  FRank := 0;
  FChildren := TList<TRbwBinomialTree<T>>.Create;
end;

destructor TRbwBinomialTree<T>.Destroy;
begin
  FChildren.Free;
  inherited;
end;

procedure TRbwBinomialTree<T>.Link(OtherTree: TRbwBinomialTree<T>);
begin
  Assert(Rank = OtherTree.Rank);
  Assert(Compare(Data, OtherTree.Data) >= 0);
  FChildren.Add(OtherTree);
  Inc(FRank);
end;

end.
