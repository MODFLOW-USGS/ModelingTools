{ ******************************************************************
  Marsaglia's Multiply-With-Carry random number generator
  ****************************************************************** }

unit uranmwc;

interface

uses
  utypes;

procedure InitMWC(Seed : Integer);
{ ------------------------------------------------------------------
  Initializes the 'Multiply with carry' random number generator.
  ------------------------------------------------------------------ }

function IRanMWC : Integer;
{ ------------------------------------------------------------------
  Returns a 32 bit random number in [-2^31 ; 2^31-1]
  ------------------------------------------------------------------ }

implementation

var
  X1, X2 : Integer;  { Uniform random integers }
  C1, C2 : Integer;  { Carries }

procedure InitMWC(Seed : Integer);
begin
  X1 := Seed shr 16;
  X2 := Seed and 65535;
  C1 := 0;
  C2 := 0;
end;

function IRanMWC : Integer;
var
  Y1, Y2 : Integer;
begin
  Y1 := 18000 * X1 + C1;
  X1 := Y1 and 65535;
  C1 := Y1 shr 16;
  Y2 := 30903 * X2 + C2;
  X2 := Y2 and 65535;
  C2 := Y2 shr 16;
  IRanMWC := (X1 shl 16) + (X2 and 65535);
end;

end.

