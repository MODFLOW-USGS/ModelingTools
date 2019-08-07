unit HashTableFacadeUnit;

interface

uses Generics.Collections;

type

  THashTableFacade = class(TObject)
  private
    FDictionary: TDictionary<string, Pointer>;
    FCaseSensitive: Boolean;
    function GetIgnoreCase: boolean;
    procedure SetIgnoreCase(const Value: boolean);
  public
    constructor Create(TableSize: Integer = 0);
    destructor Destroy; override;
    procedure Delete(const aKey : string);
    procedure Insert(const aKey : string; aData : pointer);
    function Search(const aKey : string; var aData : pointer) : boolean;
    property IgnoreCase : boolean read GetIgnoreCase write SetIgnoreCase;
  end;

implementation

uses
  SysUtils;

type
  TPointerStorage = class(TObject)
    Data: Pointer;
  end;

function GetClosestPrime(N : longint) : longint;
{$I EZPrimes.inc}
const
  Forever = true;
var
  L, R, M : integer;
  RootN   : longint;
  IsPrime : boolean;
  DivisorIndex : integer;
begin
  {treat 2 as a special case}
  if (N = 2) then begin
    Result := N;
    Exit;
  end;
  {make the result equal to N, and if it's even, the next odd number}
  if Odd(N) then
    Result := N
  else
    Result := succ(N);
  {if the result is within our prime number table, use binary search
   to find the equal or next highest prime number}
  if (Result <= MaxPrime) then begin
    L := 0;
    R := pred(PrimeCount);
    while (L <= R) do begin
      M := (L + R) div 2;
      if (Result = Primes[M]) then
        Exit
      else if (Result < Primes[M]) then
        R := pred(M)
      else
        L := succ(M);
    end;
    Result := Primes[L];
    Exit;
  end;
  {the result is outside our prime number table range, use the
   standard method for testing primality (do any of the primes up to
   the root of the number divide it exactly?) and continue
   incrementing the result by 2 until it is prime}
  if (Result <= (MaxPrime * MaxPrime)) then begin
    while Forever do begin
      RootN := round(Sqrt(Result));
      DivisorIndex := 1; {ignore the prime number 2}
      IsPrime := true;
      while (DivisorIndex < PrimeCount) and (RootN > Primes[DivisorIndex]) do begin
        if ((Result div Primes[DivisorIndex]) * Primes[DivisorIndex] = Result) then begin
          IsPrime := false;
          Break;
        end;
        inc(DivisorIndex);
      end;
      if IsPrime then
        Exit;
      inc(Result, 2);
    end;
  end;
end;

{ THashTableFacade }

constructor THashTableFacade.Create(TableSize: Integer = 0);
begin
  FDictionary:= TDictionary<string, Pointer>.Create(TableSize);
  IgnoreCase := False;
end;

procedure THashTableFacade.Delete(const aKey: string);
begin
  if IgnoreCase then
  begin
    FDictionary.Remove(UpperCase(aKey));
  end
  else
  begin
    FDictionary.Remove(aKey);
  end;
end;

destructor THashTableFacade.Destroy;
begin
  FDictionary.Free;
  inherited;
end;

function THashTableFacade.GetIgnoreCase: boolean;
begin
  result := FCaseSensitive;
end;

procedure THashTableFacade.Insert(const aKey: string; aData: pointer);
begin
  if IgnoreCase then
  begin
    FDictionary.Add(UpperCase(aKey), aData);
  end
  else
  begin
    FDictionary.Add(aKey, aData);
  end;

end;

procedure THashTableFacade.SetIgnoreCase(const Value: boolean);
begin
  Assert(FDictionary.Count = 0);
  FCaseSensitive := Value;
end;

function THashTableFacade.Search(const aKey: string;
  var aData: pointer): boolean;
begin
  if IgnoreCase then
  begin
    Result := FDictionary.ContainsKey(UpperCase(aKey));
    if Result then
    begin
      aData := FDictionary[UpperCase(aKey)];
    end
    else
    begin
      aData := nil;
    end;
  end
  else
  begin
    Result := FDictionary.ContainsKey(aKey);
    if Result then
    begin
      aData := FDictionary[aKey];
    end
    else
    begin
      aData := nil;
    end;
  end;
end;

end.
