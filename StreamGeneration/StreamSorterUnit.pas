unit StreamSorterUnit;

interface

uses
  System.Classes, System.Generics.Collections, System.Generics.Defaults;

type
  TSortStream<T> = class(TObject)
  private
    FStream: TStream;
    FItemSize: Integer;
    procedure QuickSort(const Comparer: IComparer<T>; L, R: Int64);
    function GetValue(Index: Int64): T;
    procedure SetValue(Index: Int64; const Value: T);
    function GetStreamPositionFromIndex(Index: Int64): Int64;
    function GetCount: Int64;
  public
    constructor Create(AStream: TStream);
    procedure Sort(const AComparer: IComparer<T>);
    property Values[Index: Int64]: T read GetValue write SetValue;
    property Count: Int64 read GetCount;
  end;

implementation

{ TSortStream }

constructor TSortStream<T>.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
  FItemSize := SizeOf(T);
end;

function TSortStream<T>.GetValue(Index: Int64): T;
begin
  FStream.Position := GetStreamPositionFromIndex(Index);
  FStream.Read(Result, FItemSize);
end;

procedure TSortStream<T>.QuickSort(const Comparer: IComparer<T>; L,
  R: Int64);
var
  I, J: Int64;
  pivot, temp: T;
begin
  if (FStream.Size = 0) or ((R - L) <= 0) then
    Exit;
  repeat
    I := L;
    J := R;
    pivot := Values[L + (R - L) shr 1];
    repeat
      while Comparer.Compare(Values[I], pivot) < 0 do
        Inc(I);
      while Comparer.Compare(Values[J], pivot) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          temp := Values[I];
          Values[I] := Values[J];
          Values[J] := temp;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(Comparer, L, J);
    L := I;
  until I >= R;
end;

procedure TSortStream<T>.SetValue(Index: Int64; const Value: T);
begin
  FStream.Position := GetStreamPositionFromIndex(Index);
  FStream.Write(Value, FItemSize)
end;

procedure TSortStream<T>.Sort(const AComparer: IComparer<T>);
begin
  QuickSort(AComparer, 0, Count-1);
end;

function TSortStream<T>.GetCount: Int64;
begin
  result := FStream.Size div FItemSize;
end;

function TSortStream<T>.GetStreamPositionFromIndex(Index: Int64): Int64;
begin
  result := Index * FItemSize;
  Assert(result >= 0);
  Assert(result < FStream.Size);
end;

end.
