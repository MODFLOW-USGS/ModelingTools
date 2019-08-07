unit SearchTrie;

interface

type
  TSearchTrie<TData> = class(TObject)
  private
    FData: TData;
    FHasData: boolean;
    SubKeys: array [Low(AnsiChar) .. High(AnsiChar)] of TSearchTrie<TData>;
    function Find(Key: PAnsiChar; var Data: TData; var KeyLength: integer;
      var FoundData: boolean): boolean; overload;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddKey(Key: PAnsiChar; Data: TData);
    function Find(Key: PAnsiChar; var Data: TData; var KeyLength: integer)
      : boolean; overload;
  end;

implementation

{ TSearchTrie<TData> }

procedure TSearchTrie<TData>.AddKey(Key: PAnsiChar; Data: TData);
var
  AChar: AnsiChar;
  SubTrie: TSearchTrie<TData>;
begin
  // if Key^ <> AnsiChar(0) then
  if Key^ = AnsiChar(0) then
  begin
    FData := Data;
    FHasData := True;
  end
  else
  begin
    AChar := Key^;
    Inc(Key);
    begin
      if not Assigned(SubKeys[AChar]) then
      begin
        SubKeys[AChar] := TSearchTrie<TData>.Create;
      end;
      SubTrie := SubKeys[AChar];
      SubTrie.AddKey(Key, Data);
    end;
  end;
end;

constructor TSearchTrie<TData>.Create;
var
  index: AnsiChar;
begin
  for index := Low(AnsiChar) to High(AnsiChar) do
  begin
    SubKeys[index] := nil;
  end;
end;

destructor TSearchTrie<TData>.Destroy;
var
  index: AnsiChar;
begin
  for index := Low(AnsiChar) to High(AnsiChar) do
  begin
    SubKeys[index].Free;
  end;
  inherited;
end;

function TSearchTrie<TData>.Find(Key: PAnsiChar; var Data: TData;
  var KeyLength: integer; var FoundData: boolean): boolean;
var
  AChar: AnsiChar;
  SubTrie: TSearchTrie<TData>;
begin
  SubTrie := Self;
  Assert(Key^ <> AnsiChar(0));
  repeat
    if SubTrie.FHasData then
    begin
      Data := SubTrie.FData;
      FoundData := True;
      // Don't exit immediately because
      // some identifiers are subsets of other identifiers such as
      // 'RECHARGE' and 'RECHARGE"'.
      // At present, this procedure will return the last TData that is found.
      // An array of TData could be returned instead but that is not required
      // for use in ListingAnalyst.

//      result := True;
//      Exit;
    end;
    AChar := Key^;
    if Key^ = AnsiChar(0) then
    begin
      result := FoundData;
      Exit;
    end
    else if Assigned(SubTrie.SubKeys[AChar]) then
    begin
      SubTrie := SubTrie.SubKeys[AChar];
      Inc(KeyLength);
    end
    else
    begin
      result := FoundData;
      Exit;
    end;
    Inc(Key);
  until False;
end;

function TSearchTrie<TData>.Find(Key: PAnsiChar; var Data: TData;
  var KeyLength: integer): boolean;
var
  FoundData: boolean;
begin
  if (Key^ = AnsiChar(0)) or (Key = nil) then
  begin
    result := False;
  end
  else
  begin
    FoundData := False;
    KeyLength := 1;
    result := Find(Key, Data, KeyLength, FoundData);
  end;
end;

end.
