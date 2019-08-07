unit HashUnit;

interface
{modified from:
Parsons, Thomas W., 1995. Introduction to Algorithms in Pascal,
John Wiley & Sons, Inc., New York, 447 p.
}

function Hash(const Key: string; const n: integer): integer;
{Mulplicative hashing}

implementation

var
  c: real;

function Hash(const Key: string; const n: integer): integer;
{var
  i, sum: integer;
begin
  sum := 0;
  for i := 1 to Length(key) do
  begin
    sum := sum + ord(key[i])
      //Add ASCII codes.
  end;
  result := trunc((n + 1) * frac(c * sum));
end; }
var
  G: longint;
  i: integer;
  HashValue: longint;
begin
  HashValue := 0;
  for i := 1 to Length(Key) do
  begin
    HashValue := (HashValue shl 4) + Ord(Key[i]);
    G := HashValue and $F0000000;
    if G <> 0 then
    begin
      HashValue := HashValue xor (G shr 24) xor G;
    end;
  end;
  result := HashValue mod n;
end;

initialization
  c := (Sqrt(5) - 1) / 2;
end.

