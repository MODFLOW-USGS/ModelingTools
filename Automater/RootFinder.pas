unit RootFinder;

interface

uses
  SysUtils;

type
  TFuncEval = function(X: Double; Values: pointer): Double;

  ERootError = class(Exception);

function FindRoot(X1, X2, tol: Double; Values: Pointer;
  AFunc: TFuncEval): double;

implementation

function FindRoot(X1, X2, tol: Double; Values: Pointer;
  AFunc: TFuncEval): double;
const
  itmax = 100;
  eps = 3e-8;
var
  a, b, c, d, e: Double;
  min1, min2, min: double;
  fa, fb, fc, p, q, r: double;
  s, tol1, xm: Double;
  iter: integer;
begin
  a := X1;
  b := X2;
  fa := AFunc(a, Values);
  fb := AFunc(b, Values);
  if fa * fb > 0 then
  begin
    raise ERootError.Create('Root must be bracketed');
  end;
  fc := fb;
  for iter := 1 to itmax do
  begin
    if fb*fc > 0 then
    begin
      c := a;
      fc := fa;
      d := b-a;
      e := d;
    end;
    if Abs(fc) < Abs(fb) then
    begin
      a := b;
      b := c;
      c := a;
      fa := fb;
      fb := fc;
      fc := fa;
    end;
    tol1 := 2*eps*abs(b)+ 0.5*tol;
    xm := 0.5*(c-b);
    if (Abs(xm) <= tol1) or (fb = 0) then
    begin
      result := b;
      Exit;
    end;
    if (Abs(e) >= tol1) and (Abs(fa) > Abs(fb)) then
    begin
      s := fb/fa;
      if a = c then
      begin
        p := 2*xm*s;
        q := 1 - s;
      end
      else
      begin
        q := fa/fc;
        r := fb/fc;
        p := s*(s*xm*q*(q-r)-(b-1)*(r-1));
        q := (q-1)*(r-1)*(s-1);
      end;
      if p > 0 then
      begin
        q := -q;
      end;
      p := Abs(p);
      min1 := 3*xm*q-abs(tol1*q);
      min2 := Abs(e*q);
      if min1 < min2 then
      begin
        min := min1
      end
      else
      begin
        min := min2;
      end;
      if 2*p < min then
      begin
        e := d;
        d := p/q;
      end
      else
      begin
        d := xm;
        e := d;
      end;
    end
    else
    begin
      d := xm;
      e := d;
    end;
    a := b;
    fa := fb;
    if Abs(d) > tol1 then
    begin
      b := b+d;
    end
    else
    begin
      if xm >= 0 then
      begin
        b := b + Abs(tol1);
      end
      else
      begin
        b := b - Abs(tol1);
      end;
    end;
    fb := AFunc(b, Values);
  end;
  result := b;
  raise ERootError.Create('Maximumn number of iterations exceeded.');
end;

end.
