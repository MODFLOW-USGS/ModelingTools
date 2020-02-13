unit WellFunctionUnit;

interface

function W(u: Double): double;

const
  // http://en.wikipedia.org/wiki/Euler%E2%80%93Mascheroni_constant
  Euler_Mascheroni = 0.57721566490153286060651209008240243104215933593992;

implementation

uses
  fmath;

function W(u: Double): double;
const
  Tolerance = 1e-20;
var
  term: double;
  n: Integer;
begin
  if u >= 20 then
  begin
    result := 0;
  end
  else
  begin
    term := -u;
    result := -Euler_Mascheroni - Ln(u) + u;
    n:= 1;
    while Abs(term) > Tolerance do
    begin
      term := term * (-u*n/(n+1)/(n+1));
      result := result-term;
      Inc(n);
    end;
  end;
end;



end.
