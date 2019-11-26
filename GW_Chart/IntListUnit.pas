unit IntListUnit;

interface

uses Classes;

type
  TIntegerList = class(TObject)
  // TIntegerList acts much like TList except that it stores integers
  // rather than pointers.
    private
      FList : TList;
      FSorted: boolean;
      procedure SetItem (Index: integer; const AnInteger : integer);
      function GetItem (Index: integer): integer;
      function GetCount : integer;
      procedure SetCapacity(ACapacity : Integer);
      function GetCapacity : integer;
      Procedure SetSorted(AValue : boolean);
    public
      function Add(const AnInteger : integer): integer;
      procedure AddUnique(const AnInteger: integer);
      procedure Clear;
      constructor Create;
      destructor Destroy; Override;
      procedure Delete(Index: Integer);
      class procedure Error(const Msg: string; Data: Integer); virtual;
      procedure Exchange(Index1, Index2: Integer);
      function First: Integer;
      procedure Insert(Index: Integer; AnInteger : integer);
      function Last: integer;
      procedure Move(CurIndex, NewIndex: Integer);
      procedure Pack;
      Property Items[Index: integer] : integer  read GetItem write SetItem; default;
      property Count : integer read GetCount;
      property Capacity : integer read GetCapacity write SetCapacity;
      property Sorted : boolean read FSorted write SetSorted;
      procedure Sort;
      function IndexOf(AnInteger: integer): integer;
    end;

  TInt64List = class(TObject)
  // TIntegerList acts much like TList except that it stores integers
  // rather than pointers.
    private
      FList : TList;
      procedure SetItem (Index: integer; const AnInteger : Int64);
      function GetItem (Index: integer): Int64;
      function GetCount : integer;
      procedure SetCapacity(ACapacity : Integer);
      function GetCapacity : integer;
    public
      function Add(const AnInteger : Int64): integer;
      procedure Clear;
      constructor Create;
      destructor Destroy; Override;
      procedure Delete(Index: Integer);
//      class procedure Error(const Msg: string; Data: Integer); virtual;
      procedure Exchange(Index1, Index2: Integer);
      function First: Int64;
      procedure Insert(Index: Integer; AnInteger : Int64);
      function Last: Int64;
      procedure Move(CurIndex, NewIndex: Integer);
//      procedure Pack;
      Property Items[Index: integer] : Int64  read GetItem write SetItem; default;
      property Count : integer read GetCount;
      property Capacity : integer read GetCapacity write SetCapacity;
    end;

  TIntListList = class(TObject)
    private
      FList : TList;
      procedure SetItem (Index: integer; const AnIntList : TIntegerList);
      function GetItem (Index: integer): TIntegerList;
      function GetCount : integer;
      procedure SetCapacity(ACapacity : Integer);
      function GetCapacity : integer;
    public
      procedure Add(const AnIntList : TIntegerList);
      procedure Clear;
      constructor Create;
      destructor Destroy; Override;
      Property Items[Index: integer] : TIntegerList  read GetItem write SetItem; default;
      property Count : integer read GetCount;
      property Capacity : integer read GetCapacity write SetCapacity;
      function IndexOf(AnIntList : TIntegerList): integer;
    end;

implementation

uses
  Contnrs;

type
  TIntegerClass = class(TObject)
    AnInteger : integer;
    end;

  TInt64Object = class(TObject)
    AnInt64: Int64;
  end;

constructor TIntegerList.Create;
begin
  inherited;
  FList := TList.Create;
  FSorted := False;
end;

destructor TIntegerList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;
procedure TIntegerList.AddUnique(const AnInteger : integer);
begin
  if IndexOf(AnInteger) = -1 then
  begin
    Add(AnInteger);
  end;
end;

function TIntegerList.Add(const AnInteger : integer): integer;
var
  AnIntegerClass : TIntegerClass;
  Top,Bottom, Middle : integer;
begin
  AnIntegerClass := TIntegerClass.Create;
  AnIntegerClass.AnInteger := AnInteger;

  if FSorted then
  begin
    if FList.Count > 0 then
    begin
      if (TIntegerClass(FList.Items[0]).AnInteger > AnInteger) then
      begin
        FList.Insert(0,AnIntegerClass);
        result := 0;
      end
      else if (TIntegerClass(FList.Items[FList.Count-1]).AnInteger < AnInteger) then
      begin
        result := FList.Add(AnIntegerClass);
      end
      else
      begin
        Top := FList.Count-1;
        Bottom := 0;
        While Top - Bottom > 1 do
        begin
          Middle := (Top+Bottom) div 2;
          if TIntegerClass(FList.Items[Middle]).AnInteger < AnInteger then
          begin
            Bottom := Middle;
          end
          else
          begin
            Top := Middle;
          end;
        end; // While Top - Bottom > 1 do
        FList.Insert(Top,AnIntegerClass);
        result := Top;
      end;
    end
    else // if FList.Count > 0 then
    begin
      result := FList.Add(AnIntegerClass);
    end;
  end
  else // if FSorted then
  begin
    result := FList.Add(AnIntegerClass);
    FSorted := False;
  end;
end;

procedure TIntegerList.Delete(Index: Integer);
var
  AnIntegerClass : TIntegerClass;
begin
  AnIntegerClass := FList.Items[Index];
  FList.Delete(Index);
  AnIntegerClass.Free;
end;

procedure TIntegerList.Clear;
var
  AnIntegerClass : TIntegerClass;
  index : integer;
begin
  for index := FList.Count -1 downto 0 do
  begin
    AnIntegerClass := FList.Items[Index];
    AnIntegerClass.Free;
  end;
  FList.Clear;
//  FSorted := False;
end;

class procedure TIntegerList.Error(const Msg: string; Data: Integer);
begin
  TList.Error(Msg, Data);
end;

procedure TIntegerList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
  FSorted := False;
end;

function TIntegerList.First: Integer;
var
  AnIntegerClass : TIntegerClass;
begin
  AnIntegerClass := FList.First;
  result := AnIntegerClass.AnInteger;
end;

procedure TIntegerList.Insert(Index: Integer; AnInteger : integer);
var
  AnIntegerClass : TIntegerClass;
begin
  AnIntegerClass := TIntegerClass.Create;
  AnIntegerClass.AnInteger := AnInteger;
  FList.Insert(Index, AnIntegerClass);
  FSorted := False;
end;

function TIntegerList.Last: integer;
var
  AnIntegerClass : TIntegerClass;
begin
  AnIntegerClass := FList.Last;
  result := AnIntegerClass.AnInteger;
end;

procedure TIntegerList.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
  FSorted := False;
end;

procedure TIntegerList.Pack;
begin
  FList.Pack
end;

procedure TIntegerList.SetItem (Index: integer; const  AnInteger : integer);
var
  AnIntegerClass : TIntegerClass;
begin
  AnIntegerClass := FList.Items[Index];
  if not ( AnIntegerClass.AnInteger = AnInteger) then
  begin
    AnIntegerClass.AnInteger := AnInteger;
  end;
  FSorted := False;
end;

function TIntegerList.GetItem (Index: integer): integer;
var
  AnIntegerClass : TIntegerClass;
begin
  AnIntegerClass := FList.Items[Index];
  result := AnIntegerClass.AnInteger;
end;

function TIntegerList.GetCount : integer;
begin
  result := FList.Count
end;

procedure TIntegerList.SetCapacity(ACapacity : Integer);
begin
  if not (FList.Capacity = ACapacity) then
  begin
    FList.Capacity := ACapacity
  end;
end;

function TIntegerList.GetCapacity : integer;
begin
  result := FList.Capacity
end;

function SortFunction(Item1, Item2: Pointer): Integer;
Var
  Int1, Int2 : TIntegerClass;
begin
  Int1 := TIntegerClass(Item1);
  Int2 := TIntegerClass(Item2);
  if Int1.AnInteger < Int2.AnInteger then
  begin
    result := -1;
  end
  else if Int1.AnInteger = Int2.AnInteger then
  begin
    result := 0;
  end
  else
  begin
    result := 1;
  end;
end;


procedure TIntegerList.Sort;
begin
  FList.Sort(SortFunction);
  FSorted := True;
end;

function TIntegerList.IndexOf(AnInteger: integer): integer;
var
  Index, Top, Bottom, Middle : integer;
begin
  if FSorted then
  begin
    if FList.Count = 0 then
    begin
      result := -1;
    end
    else
    begin
      if (TIntegerClass(FList.Items[0]).AnInteger > AnInteger) or
         (TIntegerClass(FList.Items[FList.Count-1]).AnInteger < AnInteger) then
      begin
        result := -1;
      end
      else
      begin
        Top := FList.Count-1;
        Bottom := 0;
        While Top - Bottom > 1 do
        begin
          Middle := (Top+Bottom) div 2;
          if TIntegerClass(FList.Items[Middle]).AnInteger < AnInteger then
          begin
            Bottom := Middle;
          end
          else
          begin
            Top := Middle;
          end;
        end; // While Top - Bottom > 1 do
        if TIntegerClass(FList.Items[Bottom]).AnInteger = AnInteger then
        begin
          result := Bottom;
        end
        else if TIntegerClass(FList.Items[Top]).AnInteger = AnInteger then
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
    for Index := 0 to FList.Count -1 do
    begin
      if TIntegerClass(FList.Items[Index]).AnInteger = AnInteger then
      begin
        result := Index;
        break;
      end;
    end;
  end;
end;

{ TIntListList }

procedure TIntListList.Add(const AnIntList: TIntegerList);
begin
  FList.Add(AnIntList);
end;

procedure TIntListList.Clear;
begin
  FList.Clear;
end;

constructor TIntListList.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TIntListList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TIntListList.GetCapacity: integer;
begin
  result := FList.Capacity;
end;

function TIntListList.GetCount: integer;
begin
  result := FList.Count;
end;

function TIntListList.GetItem(Index: integer): TIntegerList;
begin
  Result := FList[Index];
end;

function TIntListList.IndexOf(AnIntList: TIntegerList): integer;
begin
  result := FList.IndexOf(AnIntList);
end;

procedure TIntListList.SetCapacity(ACapacity: Integer);
begin
  FList.Capacity := ACapacity;
end;

procedure TIntListList.SetItem(Index: integer;
  const AnIntList: TIntegerList);
begin
  FList[Index] := AnIntList;
end;

procedure TIntegerList.SetSorted(AValue: boolean);
begin
  if AValue then
  begin
    Sort;
  end
  else
  begin
    FSorted := False;
  end;
end;

{ TInt64List }

function TInt64List.Add(const AnInteger: Int64): integer;
var
  AnIntegerObj : TInt64Object;
begin
  AnIntegerObj := TInt64Object.Create;
  AnIntegerObj.AnInt64 := AnInteger;
  result := FList.Add(AnIntegerObj);
end;

procedure TInt64List.Clear;
begin
  FList.Clear;
end;

constructor TInt64List.Create;
begin
  FList := TObjectList.Create;
end;

procedure TInt64List.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

destructor TInt64List.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TInt64List.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

function TInt64List.First: Int64;
var
  AnIntegerObj : TInt64Object;
begin
  AnIntegerObj := FList.First;
  result := AnIntegerObj.AnInt64;
end;

function TInt64List.GetCapacity: integer;
begin
  result := FList.Capacity
end;

function TInt64List.GetCount: integer;
begin
  result := FList.Count
end;

function TInt64List.GetItem(Index: integer): Int64;
var
  AnIntegerObj : TInt64Object;
begin
  AnIntegerObj := FList.Items[Index];
  result := AnIntegerObj.AnInt64;
end;

procedure TInt64List.Insert(Index: Integer; AnInteger: Int64);
var
  AnIntegerObj : TInt64Object;
begin
  AnIntegerObj := TInt64Object.Create;
  AnIntegerObj.AnInt64 := AnInteger;
  FList.Insert(Index, AnIntegerObj);
end;

function TInt64List.Last: Int64;
var
  AnIntegerObj : TInt64Object;
begin
  AnIntegerObj := FList.Last;
  result := AnIntegerObj.AnInt64;
end;

procedure TInt64List.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

procedure TInt64List.SetCapacity(ACapacity: Integer);
begin
  if not (FList.Capacity = ACapacity) then
  begin
    FList.Capacity := ACapacity
  end;
end;

procedure TInt64List.SetItem(Index: integer; const AnInteger: Int64);
var
  AnIntegerObj : TInt64Object;
begin
  AnIntegerObj := FList.Items[Index];
  if not ( AnIntegerObj.AnInt64 = AnInteger) then
  begin
    AnIntegerObj.AnInt64 := AnInteger;
  end;
end;

end.
