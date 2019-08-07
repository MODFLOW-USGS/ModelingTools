unit HashListUnit;


interface

uses SysUtils;

type
  PHashRecord = ^THashRecord;
  THashRecord = record
    Key: string;
    Value: Pointer;
    Next: PHashRecord;
  end;

  THashArray = array of PHashRecord;

  EHashException = class(Exception);

  THashList = class(TObject)
  private
    CurrentRecord: PHashRecord;
    CurrentIndex: integer;
    FMaxChain: integer;
    HashArray: THashArray;
    procedure SetSize(const NewLength: integer);
  protected
    procedure AddTo(var AHashArray: THashArray;
      const Key: string; const Value: Pointer; const ShouldGrow: boolean);
    procedure ClearAnArray(var AHashArray: THashArray);
    procedure GetPosition(const Key: string; out Index, Count: integer;
      out HashRecord: PHashRecord; const AHashArray: THashArray);
    procedure Grow;
    function HashIndex(const Key: string; const AHashArray: THashArray):
      integer;
  public
    procedure Add(const Key: string; const Value: Pointer);
    // "Add" adds a new entry (Value) to the THashList using Key to determine
    // the location at which it should be added.
    procedure AddOrReplace(const Key: string; const Value: Pointer);
    // "Add" adds a new entry (Value) to the THashList using Key to determine
    // the location at which it should be added.
    procedure Clear;
    constructor Create(const IntialSize, MaxChain: integer);
    destructor Destroy; override;
    function First: PHashRecord;
    function Get(const Key: string): Pointer;
    function Next: PHashRecord;
    function Remove(const Key: string): pointer;
    procedure UpdateCapacity(const NewLength: integer);
  end;

implementation

uses HashUnit;

{ THashList }

procedure THashList.Add(const Key: string; const Value: Pointer);
begin
  AddTo(HashArray, Key, Value, True);
end;

procedure THashList.AddOrReplace(const Key: string; const Value: Pointer);
var
  Index, Count: integer;
  HashRecord: PHashRecord;
begin
  if Key = '' then
  begin
    Exit;
  end;
  GetPosition(Key, Index, Count, HashRecord, HashArray);
  if HashRecord = nil then
  begin
    Add(Key, Value);
  end
  else
  begin
    HashRecord^.Value := Value;
    if Count > FMaxChain then
    begin
      Grow;
    end;
  end;
end;

procedure THashList.AddTo(var AHashArray: THashArray;
  const Key: string; const Value: Pointer; const ShouldGrow: boolean);
var
  Index, Count: integer;
  HashRecord: PHashRecord;
begin
  GetPosition(Key, Index, Count, HashRecord, AHashArray);
  if (HashRecord <> nil) then
  begin
    raise EHashException.Create('HashList Error: Attempted to add '
      + 'duplicate record: ' + Key);
  end;
  if ShouldGrow and (Count > FMaxChain) then
  begin
    Grow;
    Index := HashIndex(Key, AHashArray);
    Assert((Index >= 0) and (Index < Length(AHashArray)));
  end;
  New(HashRecord);
  HashRecord^.Key := Key;
  HashRecord^.Value := Value;
  HashRecord^.Next := AHashArray[Index];
  AHashArray[Index] := HashRecord;
end;

procedure THashList.Clear;
begin
  ClearAnArray(HashArray);
end;

procedure THashList.ClearAnArray(var AHashArray: THashArray);
var
  Index: integer;
  Value: PHashRecord;
  NextValue: PHashRecord;
begin
  for Index := 0 to Length(AHashArray) - 1 do
  begin
    Value := AHashArray[Index];
    while Value <> nil do
    begin
      NextValue := Value.Next;
      Dispose(Value);
      Value := NextValue;
    end;
    AHashArray[Index] := nil;
  end;
end;

constructor THashList.Create(const IntialSize, MaxChain: integer);
begin
  inherited Create;
  FMaxChain := MaxChain;
  Assert(IntialSize >= 2);
  SetLength(HashArray, IntialSize);
  CurrentRecord := nil;
  CurrentIndex := -1;
end;

destructor THashList.Destroy;
begin
  ClearAnArray(HashArray);
  SetLength(HashArray, 0);
  HashArray := nil;
  inherited;
end;

function THashList.First: PHashRecord;
begin
  CurrentRecord := nil;
  CurrentIndex := -1;
  result := Next;
end;

function THashList.Get(const Key: string): Pointer;
var
  HashRecord: PHashRecord;
  Index, Count: integer;
begin
  GetPosition(Key, Index, Count, HashRecord, HashArray);
  if HashRecord = nil then
  begin
    result := nil;
  end
  else
  begin
    result := HashRecord^.Value;
  end;
  if Count > FMaxChain then
  begin
    Grow;
  end;
end;

procedure THashList.GetPosition(const Key: string; out Index, Count: integer;
  out HashRecord: PHashRecord; const AHashArray: THashArray);
var
  TempHashRecord: PHashRecord;
begin
  HashRecord := nil;
  Count := 0;
  Index := HashIndex(Key, AHashArray);
  Assert((Index >= 0) and (Index < Length(AHashArray)));
  TempHashRecord := AHashArray[Index];
  while TempHashRecord <> nil do
  begin
    if TempHashRecord^.Key = Key then
    begin
      HashRecord := TempHashRecord;
      Exit;
    end
    else
    begin
      TempHashRecord := TempHashRecord^.Next;
      Inc(Count);
    end;
  end;
end;

procedure THashList.SetSize(const NewLength: integer);
var
  NewHashArray: THashArray;
  HashRecord: PHashRecord;
begin
  SetLength(NewHashArray, NewLength);
  HashRecord := First;
  while HashRecord <> nil do
  begin
    AddTo(NewHashArray, HashRecord^.Key, HashRecord^.Value, False);
    HashRecord := Next;
  end;
  ClearAnArray(HashArray);
  SetLength(HashArray, 0);
  HashArray := nil;
  HashArray := NewHashArray;
end;

procedure THashList.Grow;
var
  NewLength: integer;
  NewHashArray: THashArray;
  HashRecord: PHashRecord;
begin
  NewLength := Length(HashArray) * 2;
  SetSize(NewLength);
{  SetLength(NewHashArray, NewLength);
  HashRecord := First;
  while HashRecord <> nil do
  begin
    AddTo(NewHashArray, HashRecord^.Key, HashRecord^.Value, False);
    HashRecord := Next;
  end;
  ClearAnArray(HashArray);
  SetLength(HashArray, 0);
  HashArray := nil;
  HashArray := NewHashArray; }
end;

function THashList.HashIndex(const Key: string; const AHashArray: THashArray):
  integer;
begin
  result := Hash(Key, Length(AHashArray) {- 1});
end;

function THashList.Next: PHashRecord;
begin
  result := nil;
  while CurrentIndex < Length(HashArray) do
  begin
    if CurrentRecord <> nil then
    begin
      CurrentRecord := CurrentRecord^.Next;
      if CurrentRecord <> nil then
      begin
        result := CurrentRecord;
        Exit;
      end;
    end;
    Inc(CurrentIndex);
    if CurrentIndex < Length(HashArray) then
    begin
      CurrentRecord := HashArray[CurrentIndex];
      if CurrentRecord <> nil then
      begin
        result := CurrentRecord;
        Exit;
      end;
    end;
  end;
end;

function THashList.Remove(const Key: string): pointer;
var
  Index, Count: integer;
  HashRecord: PHashRecord;
  PriorRecord: PHashRecord;
begin
  result := nil;
  try
    GetPosition(Key, Index, Count, HashRecord, HashArray);
    if HashRecord <> nil then
    begin
      PriorRecord := HashArray[Index];
      if PriorRecord = HashRecord then
      begin
        result := HashRecord^.Value;
        HashArray[Index] := HashRecord^.Next;
        if HashRecord = CurrentRecord then
        begin
          CurrentRecord := nil;
          CurrentIndex := -1;
        end;
        Dispose(HashRecord);
        Exit;
      end
      else
      begin
        while PriorRecord^.Next <> nil do
        begin
          if PriorRecord^.Next = HashRecord then
          begin
            result := HashRecord^.Value;
            PriorRecord^.Next := HashRecord^.Next;
            if HashRecord = CurrentRecord then
            begin
              CurrentRecord := nil;
              CurrentIndex := -1;
            end;
            Dispose(HashRecord);
            Exit;
          end;
          PriorRecord := PriorRecord^.Next;
        end;
      end;
      Assert(False);
    end;
  finally
    if Count > FMaxChain + 1 then
    begin
      Grow;
    end;
  end;
end;

procedure THashList.UpdateCapacity(const NewLength: integer);
begin
  if NewLength > Length(HashArray) then
  begin
    SetSize(NewLength);
  end;
end;

end.


