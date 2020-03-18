{@abstract(@name defines @link(TRealList) which is used to store
 a series of doubles.)}

{$IFDEF FPC}
  {$mode Delphi}
{$ENDIF}

unit RealListUnit;

interface

uses Classes, contnrs;

type
  // @abstract(@name is similar to TList except that it stores doubles instead
  // of pointers.)
  TRealList = class(TObject)
  private
    // @name: TList;
    // @name is the internal list used to store the real numbers.
    // It actually stores instances of TRealClass (defined in the
    // implementation.  Each of these stores one of the real numbers
    // stored by @classname.
    FList: TList;
    // @name: boolean;
    FSorted: boolean;
    // See @link(Capacity).
    function GetCapacity: integer;
    // See @link(Count).
    function GetCount: integer;
    // See @link(Items).
    function GetItem(Index: integer): double;
    // See @link(Capacity).
    procedure SetCapacity(ACapacity: Integer);
    // See @link(Count).
    procedure SetCount(Value: integer);
    // See @link(Items).
    procedure SetItem(Index: integer; const AReal: double);
    // See @link(Sorted).
    procedure SetSorted(const Value: boolean);
  public
    // @name increases the count of the list by one.  If the list is sorted,
    // AReal is inserted at the appropriate place.  Otherwise it is added at
    // the end.
    function Add(const AReal: double): integer;
    // @name adds AReal to the list if it isn't already in the list.
    // otherwise it returns the position of AReal in the list.
    function AddUnique(const AReal: double): integer;
    // @name copies the contents of ARealList
    // into the @classname that calls @name.
    procedure Assign(ARealList: TRealList);
    // @name is the number of numbers that the @classname can hold without
    // reallocating memory.  If you plan to add a large number of items to
    // the @classname, setting @name to a large enough number first can
    // improve performance.
    property Capacity: integer read GetCapacity write SetCapacity;
    // @name removes all real numbers from the list.
    procedure Clear;
    // @name is the number of real numbers in the TRealList.
    // Setting @name to a number larger than the current @name results
    // in 0's being added to the end of @classname.
    property Count: integer read GetCount write SetCount;
    // see inherited Create.
    constructor Create;
    // @name removes the item at the position designated by Index
    procedure Delete(const Index: Integer);
    // see inherited Destroy.
    destructor Destroy; override;
    // @name generate an error message.
    // See TList.Error.
    class procedure Error(const Msg: string; Data: Integer); virtual;
    // @name exchanges the real numbers at positions Index1 and Index2.
    procedure Exchange(Index1, Index2: Integer);
    // @name returns the first real number in the list.
    function First: double;
    // @name returns the position of AReal in the list. If AReal is not in
    // the list, IndexOf returns -1.
    function IndexOf(const AReal: double): integer;
    // @name, returns the position of the number in the list that
    // is closest to AReal.
    function IndexOfClosest(const AReal: double): integer;
    // @name inserts AReal at Index.
    procedure Insert(Index: Integer; AReal: double);
    // @name is used to set or retrieve real numbers from the @classname.
    // Use @Link(Count) to determine how many real
    // numbers are in the @classname.
    property Items[Index: integer]: double read GetItem write SetItem; default;
    // @name returns the last real number in the @classname.
    function Last: double;
    // @name moves the number at CurIndex to NewIndex.
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Reverse;
    // @name sorts the numbers in the @classname in ascending order and
    // sets @Link(Sorted) to true.
    procedure Sort;
    // If @name is true, the numbers in the list are in ascending order and
    // when a new number is added (see @Link(Add) ) to the list, it will be
    // inserted in the correct position to keep the list sorted.
    property Sorted: boolean read FSorted write SetSorted;
  end;

implementation


type
  TRealClass = class(TObject)
  private
    AReal: double;
  end;

function SortReals(Item1, Item2: Pointer): Integer;
var
  AReal1: TRealClass;
  AReal2: TRealClass;
  Difference: double;
begin
  AReal1 := Item1;
  AReal2 := Item2;
  Difference := AReal1.AReal - AReal2.AReal;
  if Difference < 0 then
  begin
    result := -1;
  end
  else if Difference = 0 then
  begin
    result := 0;
  end
  else
  begin
    result := 1;
  end;
end;

constructor TRealList.Create;
begin
  inherited;
  FList := TObjectList.Create;
end;

destructor TRealList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TRealList.Add(const AReal: double): integer;
var
  ARealClass: TRealClass;
  Top, Bottom, Middle: integer;
begin
  ARealClass := TRealClass.Create;
  ARealClass.AReal := AReal;
  if FSorted then
  begin
    if FList.Count > 0 then
    begin
      if (TRealClass(FList.Items[0]).AReal > AReal) then
      begin
        FList.Insert(0, ARealClass);
        result := 0;
      end
      else if (TRealClass(FList.Items[FList.Count - 1]).AReal < AReal) then
      begin
        result := FList.Add(ARealClass);
      end
      else
      begin
        Top := FList.Count - 1;
        Bottom := 0;
        while Top - Bottom > 1 do
        begin
          Middle := (Top + Bottom) div 2;
          if TRealClass(FList.Items[Middle]).AReal < AReal then
          begin
            Bottom := Middle;
          end
          else
          begin
            Top := Middle;
          end;
        end; // While Top - Bottom > 1 do
        FList.Insert(Top, ARealClass);
        result := Top;
      end;
    end
    else // if FList.Count > 0 then
    begin
      result := FList.Add(ARealClass);
    end;
  end
  else // if FSorted then
  begin
    Result := FList.Add(ARealClass);
  end;
end;

procedure TRealList.Delete(const Index: Integer);
begin
  FList.Delete(Index);
end;

class procedure TRealList.Error(const Msg: string; Data: Integer);
begin
  TList.Error(Msg, Data);
end;

procedure TRealList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
  FSorted := False;
end;

function TRealList.First: double;
var
  ARealClass: TRealClass;
begin
  ARealClass := FList.First;
  result := ARealClass.AReal;
end;

procedure TRealList.Insert(Index: Integer; AReal: double);
var
  ARealClass: TRealClass;
begin
  ARealClass := TRealClass.Create;
  ARealClass.AReal := AReal;
  FList.Insert(Index, ARealClass);
  FSorted := False;
end;

function TRealList.Last: double;
var
  ARealClass: TRealClass;
begin
  ARealClass := FList.Last;
  result := ARealClass.AReal;
end;

procedure TRealList.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
  FSorted := False;
end;

procedure TRealList.Reverse;
var
  Index: Integer;
begin
  for Index := 0 to (Count div 2)-1 do
  begin
    FList.Exchange(Index, Count-Index-1);
  end;
end;

procedure TRealList.SetItem(Index: integer; const AReal: double);
var
  ARealClass: TRealClass;
begin
  ARealClass := FList.Items[Index];
  if not (ARealClass.AReal = AReal) then
  begin
    ARealClass.AReal := AReal;
    FSorted := False;
  end;
end;

function TRealList.GetItem(Index: integer): double;
var
  ARealClass: TRealClass;
begin
  ARealClass := FList.Items[Index];
  result := ARealClass.AReal;
end;

function TRealList.GetCount: integer;
begin
  result := FList.Count
end;

procedure TRealList.SetCapacity(ACapacity: Integer);
begin
  if not (FList.Capacity = ACapacity) then
  begin
    FList.Capacity := ACapacity
  end;
end;

function TRealList.GetCapacity: integer;
begin
  result := FList.Capacity
end;

procedure TRealList.Assign(ARealList: TRealList);
var
  Index: integer;
begin
  Count := ARealList.Count;
  for Index := 0 to Count - 1 do
  begin
    Items[Index] := ARealList.Items[Index];
  end;
  FSorted := ARealList.Sorted;
end;

procedure TRealList.SetCount(Value: integer);
var
  OldCount: integer;
  Index: integer;
  ARealClass: TRealClass;
begin
  if FList.Count <> Value then
  begin
    if FList.Count < Value then
    begin
      FSorted := False;
    end;
    OldCount := FList.Count;
    FList.Count := Value;
    for Index := OldCount to Value - 1 do
    begin
      ARealClass := TRealClass.Create;
      ARealClass.AReal := 0;
      FList[Index] := ARealClass;
    end;
  end;
end;

procedure TRealList.Clear;
begin
  FList.Clear;
end;

function TRealList.IndexOf(const AReal: double): integer;
var
  Index, Top, Bottom, Middle: integer;
begin
  if FSorted then
  begin
    if FList.Count = 0 then
    begin
      result := -1;
    end
    else
    begin
      if (TRealClass(FList.Items[0]).AReal > AReal) or
        (TRealClass(FList.Items[FList.Count - 1]).AReal < AReal) then
      begin
        result := -1;
      end
      else
      begin
        Top := FList.Count - 1;
        Bottom := 0;
        while Top - Bottom > 1 do
        begin
          Middle := (Top + Bottom) div 2;
          if TRealClass(FList.Items[Middle]).AReal < AReal then
          begin
            Bottom := Middle;
          end
          else
          begin
            Top := Middle;
          end;
        end; // While Top - Bottom > 1 do
        if TRealClass(FList.Items[Bottom]).AReal = AReal then
        begin
          result := Bottom;
        end
        else if TRealClass(FList.Items[Top]).AReal = AReal then
        begin
          result := Top;
        end
        else
        begin
          result := -1;
        end;
      end;
    end;
  end
  else
  begin
    result := -1;
    for Index := 0 to FList.Count - 1 do
    begin
      if TRealClass(FList.Items[Index]).AReal = AReal then
      begin
        result := Index;
        break;
      end;
    end;
  end;
end;

procedure TRealList.Sort;
begin
  FList.Sort(SortReals);
  FSorted := True;
end;

procedure TRealList.SetSorted(const Value: boolean);
begin
  if Value then
  begin
    Sort;
  end
  else
  begin
    FSorted := False;
  end;
end;

function TRealList.AddUnique(const AReal: double): Integer;
begin
  result := IndexOf(AReal);
  if result = -1 then
  begin
    result := Add(AReal);
  end;
end;

function TRealList.IndexOfClosest(const AReal: double): integer;
var
  Index, Top, Bottom, Middle: integer;
  MinDistance, Test: double;
begin
  if FSorted then
  begin
    if FList.Count = 0 then
    begin
      result := -1;
    end
    else
    begin
      if (TRealClass(FList.Items[0]).AReal > AReal) then
      begin
        result := 0
      end
      else if (TRealClass(FList.Items[FList.Count - 1]).AReal < AReal) then
      begin
        result := FList.Count - 1;
      end
      else
      begin
        Top := FList.Count - 1;
        Bottom := 0;
        while Top - Bottom > 1 do
        begin
          Middle := (Top + Bottom) div 2;
          if TRealClass(FList.Items[Middle]).AReal < AReal then
          begin
            Bottom := Middle;
          end
          else
          begin
            Top := Middle;
          end;
        end; // While Top - Bottom > 1 do
        if TRealClass(FList.Items[Bottom]).AReal = AReal then
        begin
          result := Bottom;
        end
        else if TRealClass(FList.Items[Top]).AReal = AReal then
        begin
          result := Top;
        end
        else if Abs(TRealClass(FList.Items[Bottom]).AReal - AReal) <
          Abs(TRealClass(FList.Items[Top]).AReal - AReal) then
        begin
          result := Bottom;
        end
        else
        begin
          result := Top;
        end;
      end;
    end;
  end
  else
  begin
    if FList.Count = 0 then
    begin
      result := -1;
    end
    else
    begin
      result := 0;
      MinDistance := Abs(TRealClass(FList.Items[0]).AReal - AReal);
      for Index := 1 to FList.Count - 1 do
      begin
        Test := Abs(TRealClass(FList.Items[Index]).AReal - AReal);
        if Test < MinDistance then
        begin
          result := Index;
          MinDistance := Test;
        end;
      end;
    end;
  end;
end;

end.

