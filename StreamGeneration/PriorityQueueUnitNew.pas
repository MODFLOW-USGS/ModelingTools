unit PriorityQueueUnitNew;

interface

uses
  System.Classes, System.Generics.Collections, System.Generics.Defaults;

type
  TPriorityQueue<T> = class(TObject)
  private
    FData: TList<T>;
    FComparer: IComparer<T>;
    procedure PushUp();
    function Left(a:Integer):Integer;inline;
    function Right(a:Integer):Integer;inline;
    procedure Heapify(position:Integer);
    function Parent(a:Integer):Integer;inline;
  public
    constructor Create(Comparer: IComparer<T>);
    destructor Destroy; override;
    function Top:T;inline;
    procedure Pop;inline;
    procedure Push(value:T);inline;
    function Count:Integer;inline;
    function IsEmpty:boolean;inline;
    procedure Enqueue(Item: T);
    function Dequeue: T;
    function Peek: T;
    procedure Clear;
  end;

implementation

{ TPriorityQueue<T> }

constructor TPriorityQueue<T>.Create(Comparer: IComparer<T>);
begin
  FComparer := Comparer;
  FData := TList<T>.Create;
end;

function TPriorityQueue<T>.Dequeue: T;
begin
  Result := Top;
  Pop;
end;

destructor TPriorityQueue<T>.Destroy;
begin
  FData.Free;
  inherited;
end;


procedure TPriorityQueue<T>.Enqueue(Item: T);
begin
  Push(Item)
end;

procedure TPriorityQueue<T>.Heapify(position: Integer);
var
  mpos,l,r:Integer;
  temp:T;
begin
  while(true) do
  begin
    mpos:=position;
    l:=Left(position);
    r:=Right(position);
    if (l<FData.Count) AND (FComparer.compare(FData[mpos],FData[l]) > 0) then
      mpos:=l;
    if (r<FData.Count) AND (FComparer.compare(FData[mpos],FData[r]) > 0) then
      mpos:=r;
    if mpos = position then break;

    temp:=FData[position];
    FData[position]:=FData[mpos];
    FData[mpos]:=temp;
    position:=mpos;
  end;

end;

function TPriorityQueue<T>.IsEmpty: boolean;
begin
  result:=FData.Count=0;
end;

function TPriorityQueue<T>.Left(a: Integer): Integer;
begin
  Result := ((a+1)shl 1)-1;
end;

function TPriorityQueue<T>.Parent(a: Integer): Integer;
begin
  result:=(a-1)shr 1;
end;

function TPriorityQueue<T>.Peek: T;
begin
  result := Top;
end;

procedure TPriorityQueue<T>.Pop;
begin
  if not IsEmpty then
  begin
    FData[0]:=FData.Last;
    FData.Delete(FData.Count-1);
    Heapify(0);
  end;
end;

procedure TPriorityQueue<T>.Push(value: T);
begin
  FData.Add(value);
  PushUp;
end;

procedure TPriorityQueue<T>.PushUp;
var
  position, np: Integer;
  temp:T;
begin
  position:=FData.Count-1;
  while(position>0) do
  begin
    np := Parent(position);
    if(FComparer.Compare(FData[np],FData[position]) > 0) then
    begin
      temp:=FData[np];
      FData[np]:=FData[position];
      FData[position]:=temp;
      position:=np;
    end else
      break;
  end;

end;

function TPriorityQueue<T>.Right(a: Integer): Integer;
begin
  result:=(a+1) shl 1;
end;

procedure TPriorityQueue<T>.Clear;
begin
  FData.Clear;
end;

function TPriorityQueue<T>.Count: Integer;
begin
  result := FData.Count;
end;

function TPriorityQueue<T>.Top: T;
begin
  result:=FData[0];
end;

end.
