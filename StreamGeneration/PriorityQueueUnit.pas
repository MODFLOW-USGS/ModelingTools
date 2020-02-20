{This unit implements a generic priority queue. It is in the public domain.
Author: Richard B. Winston
email: rbwinst@usgs.gov
The logic is comparable to the priority queue implemented in
"The Tomes of Delphi: Algorithms and Data Structures" by Julian Bucknall.
}
unit PriorityQueueUnit;

interface

uses
  System.Generics.Collections, System.Generics.Defaults;

type
  // @name is a binary priority queue.
  TRbwPriorityQueue<T> = class(TObject)
  private
    FList: TList<T>;
    FComparer: IComparer<T>;
    FCount: Integer;
    function GetCount: Integer;
  protected
    {
    @name returns > 0 if Left has a higher priority than Right.
    @name returns 0 if Left has an equal priority to Right.
    @name returns < 0 if Left has a lower priority than Right.
    }
//    function Compare (Left, Right: T): Integer; virtual; abstract;
    procedure BubbleUp(FromIndex: Integer);
    procedure TrickleDown;
  public
    constructor Create(Comparer: IComparer<T>);
    destructor Destroy; override;
    procedure Clear;
    procedure Enqueue(Item: T);
    property Count: Integer read GetCount;
    function Dequeue: T;
    function Peek: T;
  end;

implementation

{ TRbwPriorityQueue<T> }

procedure TRbwPriorityQueue<T>.BubbleUp(FromIndex: Integer);
var
  Item: T;
  ParentIndex: Integer;
begin
  Item := FList[FromIndex];
  ParentIndex := (FromIndex-1) div 2;
  while (FromIndex > 0) and (FComparer.Compare(Item, FList[ParentIndex]) > 0) do
  begin
    FList[FromIndex] := FList[ParentIndex];
    FromIndex := ParentIndex;
    ParentIndex := (FromIndex-1) div 2;
  end;
  FList[FromIndex] := Item;
end;

procedure TRbwPriorityQueue<T>.Clear;
begin
  FCount := 0;
  FList.Clear;
end;

constructor TRbwPriorityQueue<T>.Create(Comparer: IComparer<T>);
begin
  FList := TList<T>.Create;
  FComparer := Comparer;
end;

function TRbwPriorityQueue<T>.Dequeue: T;
begin
  result := FList[0];
  if Count <= 2 then
  begin
    FList.Delete(0);
    Dec(FCount);
  end
  else
  begin
    FList[0] := FList[FCount-1];
    Dec(FCount);
    TrickleDown;
  end;
end;

destructor TRbwPriorityQueue<T>.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TRbwPriorityQueue<T>.Enqueue(Item: T);
begin
  if FList.Count = FCount then
  begin
    FList.Add(Item);
  end
  else
  begin
    FList[FCount] := Item;
  end;
  Inc(FCount);
  BubbleUp(Pred(FCount));
end;

function TRbwPriorityQueue<T>.GetCount: Integer;
begin
  result := FCount;// FList.Count;
end;

function TRbwPriorityQueue<T>.Peek: T;
begin
  result := FList[0];
end;

procedure TRbwPriorityQueue<T>.TrickleDown;
var
  FromIndex: Integer;
  Item: T;
  MaxIndex: Integer;
  ChildIndex: Integer;
begin
  FromIndex := 0;
  Item := FList[0];
  MaxIndex := Pred(FCount);
  ChildIndex := (FromIndex * 2) + 1;
  while ChildIndex <= MaxIndex do
  begin
    if (Succ(ChildIndex) <= MaxIndex) and
      (FComparer.Compare(FList[ChildIndex], FList[Succ(ChildIndex)]) < 0) then
    begin
      Inc(ChildIndex);
    end;
    FList[FromIndex] := FList[ChildIndex];
    FromIndex := ChildIndex;
    ChildIndex := (FromIndex * 2) + 1;
  end;
  FList[FromIndex] := Item;
  BubbleUp(FromIndex);
end;

end.
