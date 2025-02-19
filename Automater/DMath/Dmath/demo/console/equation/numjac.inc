{ ******************************************************************
  Numerical jacobian
  ****************************************************************** }

procedure Jacobian(X : TVector; D : TMatrix);
const
  EtaMin = 1E-6;  { Relative increment used to compute derivatives }
var
  I, J    : Integer;
  R, Temp : Float;
  Eta     : Float;
  Delta   : TVector;  { Increment }
  Xminus  : TVector;  { X - Delta }
  Xplus   : TVector;  { X + Delta }
  Fminus  : TVector;  { F(X - Delta) }
  Fplus   : TVector;  { F(X + Delta) }

begin
  DimVector(Delta, Nvar);
  DimVector(Xminus, Nvar);
  DimVector(Xplus, Nvar);
  DimVector(Fminus, Nvar);
  DimVector(Fplus, Nvar);

  Eta := Sqrt(MachEp);
  if Eta < EtaMin then Eta := EtaMin;

  for I := 1 to Nvar do
    begin
      if X[I] <> 0 then
        Delta[I] := Eta * Abs(X[I])
      else
        Delta[I] := Eta;
      Xplus[I] := X[I] + Delta[I];
      Xminus[I] := X[I] - Delta[I]
    end;

  for J := 1 to Nvar do
    begin
      Temp := X[J];

      X[J] := Xminus[J];
      Equations(X, Fminus);

      X[J] := Xplus[J];
      Equations(X, Fplus);

      R := 1.0 / (2.0 * Delta[J]);

      for I := 1 to Nvar do
        D[I,J] := R * (Fplus[I] - Fminus[I]);

      X[J] := Temp;
    end;

end;
