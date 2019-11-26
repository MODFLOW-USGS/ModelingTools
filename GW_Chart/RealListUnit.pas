unit RealListUnit;

interface

uses Classes, contnrs;

type
  TRealList = class(TObject)
  // TRealList is similar to TList except that it stores doubles instead
  // of pointers.
    private
      FList : TObjectList;
      FSorted: boolean;
      function GetCapacity : integer;
      function GetCount : integer;
      function GetItem (Index: integer): double;
      procedure SetCapacity(ACapacity : Integer);
      procedure SetCount(Value : integer);
      procedure SetItem (Index: integer; const AReal : double);
    procedure SetSorted(const Value: boolean);
    public
      Procedure Assign(ARealList : TRealList);
      function Add(const AReal : double) : integer;
      procedure AddUnique(const AReal : double);
      constructor Create;
      destructor Destroy; Override;
      procedure Delete(Index: Integer);
      class procedure Error(const Msg: string; Data: Integer); virtual;
      procedure Exchange(Index1, Index2: Integer);
      function First: double;
      procedure Insert(Index: Integer; AReal : double);
      function Last: double;
      procedure Move(CurIndex, NewIndex: Integer);
      procedure Pack;
      Property Items[Index: integer] : double  read GetItem write SetItem; default;
      property Count : integer read GetCount write SetCount;
      property Capacity : integer read GetCapacity write SetCapacity;
      procedure Clear;
      function IndexOf(Const AReal : double) : integer;
      property Sorted : boolean read FSorted write SetSorted;
      procedure Sort;
      function IndexOfClosest(const AReal: double): integer;
    end;

implementation

type
  TRealClass = Class(TObject)
    AReal : double;
    end;

function SortReals(Item1, Item2: Pointer): Integer;
var
  AReal1 : TRealClass;
  AReal2 : TRealClass;
  Difference : double;
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

function TRealList.Add(const AReal : double) : integer;
var
  ARealClass : TRealClass;
  Top,Bottom, Middle : integer;
begin
  ARealClass := TRealClass.Create;
  ARealClass.AReal := AReal;
  if FSorted then
  begin
    if FList.Count > 0 then
    begin
      if (TRealClass(FList.Items[0]).AReal > AReal) then
      begin
        FList.Insert(0,ARealClass);
        result := 0;
      end
      else if (TRealClass(FList.Items[FList.Count-1]).AReal < AReal) then
      begin
        result := FList.Add(ARealClass);
      end
      else
      begin
        Top := FList.Count-1;
        Bottom := 0;
        While Top - Bottom > 1 do
        begin
          Middle := (Top+Bottom) div 2;
          if TRealClass(FList.Items[Middle]).AReal < AReal then
          begin
            Bottom := Middle;
          end
          else
          begin
            Top := Middle;
          end;
        end; // While Top - Bottom > 1 do
        FList.Insert(Top,ARealClass);
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

procedure TRealList.Delete(Index: Integer);
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
  ARealClass : TRealClass;
begin
  ARealClass := FList.First {$IFDEF VER150} as TRealClass {$ENDIF};
  result := ARealClass.AReal;
end;

procedure TRealList.Insert(Index: Integer; AReal : double);
var
  ARealClass : TRealClass;
begin
  ARealClass := TRealClass.Create;
  ARealClass.AReal := AReal;
  FList.Insert(Index, ARealClass);
  FSorted := False;
end;

function TRealList.Last: double;
var
  ARealClass : TRealClass;
begin
  ARealClass := FList.Last {$IFDEF VER150} as TRealClass {$ENDIF};
  result := ARealClass.AReal;
end;

procedure TRealList.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
  FSorted := False;
end;

procedure TRealList.Pack;
begin
  FList.Pack
end;

procedure TRealList.SetItem (Index: integer; const  AReal : double);
var
  ARealClass : TRealClass;
begin
  ARealClass := FList.Items[Index] as TRealClass;
  if not ( ARealClass.AReal = AReal) then
  begin
    ARealClass.AReal := AReal;
    FSorted := False;
  end;
end;

function TRealList.GetItem (Index: integer): double;
var
  ARealClass : TRealClass;
begin
  ARealClass := FList.Items[Index] as TRealClass;
  result := ARealClass.AReal;
end;

function TRealList.GetCount : integer;
begin
  result := FList.Count
end;

procedure TRealList.SetCapacity(ACapacity : Integer);
begin
  if not (FList.Capacity = ACapacity) then
  begin
    FList.Capacity := ACapacity
  end;
end;

function TRealList.GetCapacity : integer;
begin
  result := FList.Capacity
end;

procedure TRealList.Assign(ARealList: TRealList);
var
  Index : integer;
begin
  Count := ARealList.Count;
  for Index := 0 to Count -1 do
  begin
    Items[Index] := ARealList.Items[Index];
  end;
  Sorted := ARealList.Sorted;
end;

procedure TRealList.SetCount(Value: integer);
var
  OldCount : integer;
  Index : integer;
  ARealClass : TRealClass;
begin
  if FList.Count <> Value then
  begin
    OldCount := FList.Count;
    FList.Count := Value;
    for Index := OldCount to Value -1 do
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

function TRealList.IndexOfClosest(const AReal: double): integer;
var
  Index, Top, Bottom, Middle : integer;
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
      else if (TRealClass(FList.Items[FList.Count-1]).AReal < AReal) then
      begin
        result := FList.Count-1;
      end
      else
      begin
        Top := FList.Count-1;
        Bottom := 0;
        While Top - Bottom > 1 do
        begin
          Middle := (Top+Bottom) div 2;
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
      for Index := 1 to FList.Count -1 do
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

function TRealList.IndexOf(const AReal: double): integer;
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
      if (TRealClass(FList.Items[0]).AReal > AReal) or
         (TRealClass(FList.Items[FList.Count-1]).AReal < AReal) then
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
    for Index := 0 to FList.Count -1 do
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

procedure TRealList.AddUnique(const AReal: double);
begin
  if IndexOf(AReal) = -1 then
  begin
    Add(AReal);
  end;
end;

end.
