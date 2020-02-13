unit HantushUnit;

interface

//function HantushRectangular(InitialHead, RechargeRate, Kx, AquiferThickness,
//  SpecificYield, RechargeTime, SimulationTime, HalfLength, HalfWidth, X, Y: double): double;

function IterateHantushRectangular(InitialHead, RechargeRate, Kx, AquiferThickness,
  SpecificYield, RechargeTime, SimulationTime, HalfLength, HalfWidth, X, Y: double): double;

function HantushCircular(InitialHead, RechargeRate, Kx, AquiferThickness,
  SpecificYield, RechargeTime, SimulationTime, InifiltrationRadius,
  Radius: Double; var CriteriaMet: Boolean): double;

function IntS(Alpha, Beta: Double; Precision: Double = 1e-8): double;

//function i2erfc(x: Double): double;

implementation

uses
  fmath, WellFunctionUnit {$IFNDEF NoInterpolate}, RealListUnit
  , S_StarValues, RootFinder {$ENDIF};

{$IFNDEF NoInterpolate}
var
  KeyValues: TRealList;
{$ENDIF}

function i2erfc(x: Double): double;
begin
  // Return the second integral of the complementary error function.
  // http://www.mhtl.uwaterloo.ca/courses/me755/web_chap2.pdf
  result := ((1 + 2*Sqr(x))*erfc(x) - 2/Sqrt(PI)*x*Exp(-Sqr(x)))/4
end;

function S(Alpha, Beta, Tau: Double): Double;
begin
  if Tau = 0 then
  begin
    Result := 1;
  end
  else
  begin
    Result := Erf(Alpha/Sqrt(Tau))*Erf(Beta/Sqrt(Tau));
  end;
end;

function IntegralS(Alpha, Beta: Double; NumberOfIntervals: Integer): Double;
var
  Count: Integer;
  Index: Integer;
  Tau: Extended;
  a: double;
  b: double;
  h: double;
begin
  a := 0;
  b := 1;
  Count := NumberOfIntervals*2;

  h := (b-a)/Count;

  result := S(Alpha, Beta, 0);
  for Index := 1 to Count - 1 do
  begin
    Tau := Index/Count;
    if Odd(Index) then
    begin
      result := result + 4*S(Alpha, Beta, Tau);
    end
    else
    begin
      result := result + 2*S(Alpha, Beta, Tau);
    end;
  end;
  result := result + S(Alpha, Beta, 1);
  result := result * h/3;
end;

{$IFNDEF NoInterpolate}
function InterpolateS(Alpha, Beta: double): double;
  procedure GetValues(const AValue: Double;
  var LowerIndex, HigherIndex: Integer;
  var Fraction: double);
  begin
    LowerIndex := KeyValues.IndexOfClosest(Abs(AValue));
    if KeyValues[LowerIndex] < Abs(AValue) then
    begin
      HigherIndex := LowerIndex+1;
    end
    else
    begin
      HigherIndex := LowerIndex;
      LowerIndex := LowerIndex-1;
    end;
    Fraction := (Abs(AValue)-KeyValues[LowerIndex])/
      (KeyValues[HigherIndex]-KeyValues[LowerIndex]);
  end;
  function Interpolate(LowV, HighV, Fraction: double): Double;
  begin
    Result := (HighV-LowV)*Fraction + LowV;
  end;
var
  ALowIndex: Integer;
  AHighIndex: Integer;
  AFraction: Double;
  BLowIndex: Integer;
  BHighIndex: Integer;
  BFraction: Double;
  AlphaLow: Double;
  AlphaHigh: Double;
begin
  GetValues(Alpha, ALowIndex, AHighIndex, AFraction);
  GetValues(Beta, BLowIndex, BHighIndex, BFraction);
  AlphaLow := Interpolate(SValues[ALowIndex,BLowIndex],
    SValues[AHighIndex,BLowIndex],AFraction);
  AlphaHigh := Interpolate(SValues[ALowIndex,BHighIndex],
    SValues[AHighIndex,BHighIndex],AFraction);
  result := Interpolate(AlphaLow, AlphaHigh, BFraction);
  if (Alpha > 0) <> (Beta > 0) then
  begin
    result := -result;
  end;
end;
{$ENDIF}

function IntS(Alpha, Beta: Double; Precision: Double = 1e-8): double;
{$IFDEF NoInterpolate}
var
  NumInterval: Integer;
  TestResult: Double;
{$ENDIF}
begin
  if (Alpha = 0) or (Beta = 0) then
  begin
    Result := 0;
  end
  else if Abs(Alpha) >= 3 then
  begin
    if Abs(Beta) >= 3 then
    begin
      if (Alpha > 0) = (Beta > 0) then
      begin
        result := 1;
      end
      else
      begin
        Result := -1;
      end;
    end
    else
    begin
      result := 1-4*i2erfc(Abs(Beta));
      if (Alpha > 0) <> (Beta > 0) then
      begin
        result := -result;
      end;
    end;
  end
  else if Abs(Beta) >= 3 then
  begin
    result := 1-4*i2erfc(Abs(Alpha));
    if (Alpha > 0) <> (Beta > 0) then
    begin
      result := -result;
    end;
  end
//  else if Sqr(Alpha) + Sqr(Beta) <= 0.1 then
//  begin
//    AlphaDivBeta := Alpha/Beta;
//    BetaDivAlpha := Beta/Alpha;
//    result := 4/Pi*Alpha*Beta*(3 + W(Sqr(Alpha) + Sqr(Beta))
//      - (AlphaDivBeta*ArcTan(BetaDivAlpha) + BetaDivAlpha*ArcTan(AlphaDivBeta)));
//  end
  else
  begin
    {$IFNDEF NoInterpolate}
    result := InterpolateS(Alpha, Beta);

    {$ELSE}
    NumInterval := 8;
    result := IntegralS(Alpha, Beta, NumInterval);
    repeat
      TestResult := result;
      NumInterval := NumInterval*2;
      result := IntegralS(Alpha, Beta, NumInterval);
    until (Abs(result-TestResult)<Precision);
    {$ENDIF}
  end;
end;

function HantushRectangular(InitialHead, RechargeRate, Kx, AquiferThickness,
  SpecificYield, RechargeTime, SimulationTime, HalfLength, HalfWidth, X, Y: double): double;
var
  Diffusivity: Extended;
  Alpha1: Extended;
  Alpha2: Extended;
  Beta1: Extended;
  Beta2: Extended;
  ZRecharge: double;
  ZDischarge: double;
  SimTime: Double;
begin

  Diffusivity := Kx*AquiferThickness/SpecificYield;
  Alpha1 := (HalfLength+X)/Sqrt(4*Diffusivity*SimulationTime);
  Alpha2 := (HalfLength-X)/Sqrt(4*Diffusivity*SimulationTime);
  Beta1 := (HalfWidth+Y)/Sqrt(4*Diffusivity*SimulationTime);
  Beta2 := (HalfWidth-Y)/Sqrt(4*Diffusivity*SimulationTime);
  ZRecharge := RechargeRate/(2*Kx)*Diffusivity*SimulationTime
    *(IntS(Alpha1, Beta1) + IntS(Alpha1, Beta2)
    + IntS(Alpha2, Beta1) + IntS(Alpha2, Beta2));

  if SimulationTime <= RechargeTime then
  begin
    ZDischarge := 0;
  end
  else
  begin
    SimTime := SimulationTime-RechargeTime;
    Alpha1 := (HalfLength+X)/Sqrt(4*Diffusivity*SimTime);
    Alpha2 := (HalfLength-X)/Sqrt(4*Diffusivity*SimTime);
    Beta1 := (HalfWidth+Y)/Sqrt(4*Diffusivity*SimTime);
    Beta2 := (HalfWidth-Y)/Sqrt(4*Diffusivity*SimTime);

    ZDischarge := RechargeRate/(2*Kx)*Diffusivity*(SimTime)
      *(IntS(Alpha1, Beta1) + IntS(Alpha1, Beta2)
      + IntS(Alpha2, Beta1) + IntS(Alpha2, Beta2));
  end;
  if ZDischarge > ZRecharge then
  begin
    result := InitialHead;
  end
  else
  begin
    result := Sqrt(ZRecharge-ZDischarge + Sqr(InitialHead));
  end;
end;

type
  THantushValues = record
    InitialHead: double;
    RechargeRate: double;
    Kx: double;
    AquiferThickness: double;
    SpecificYield: double;
    RechargeTime: double;
    SimulationTime: double;
    HalfLength: double;
    HalfWidth: double;
    X: double;
    Y: double;
  end;
  PHantushValues = ^THantushValues;

function RootHantushRect(Value: double; values: Pointer): double;
var
  P: PHantushValues;
begin
  P := values;
  result := HantushRectangular(P.InitialHead, P.RechargeRate, P.Kx,
    (P.AquiferThickness + Value)/2, P.SpecificYield, P.RechargeTime, P.SimulationTime,
    P.HalfLength, P.HalfWidth, P.X, P.Y) - Value;
end;

function IterateHantushRectangular(InitialHead, RechargeRate, Kx, AquiferThickness,
  SpecificYield, RechargeTime, SimulationTime, HalfLength, HalfWidth,
  X, Y: double): double;
var
  Values: THantushValues;
  X2: double;
const
  IterationControl = 1e-5;
begin
  Values.InitialHead := InitialHead;
  Values.RechargeRate := RechargeRate;
  Values.Kx := Kx;
  Values.AquiferThickness := AquiferThickness;
  Values.SpecificYield := SpecificYield;
  Values.RechargeTime := RechargeTime;
  Values.SimulationTime := SimulationTime;
  Values.HalfLength := HalfLength;
  Values.HalfWidth := HalfWidth;
  Values.X := X;
  Values.Y := Y;
  X2 := 2*HantushRectangular(Values.InitialHead, Values.RechargeRate, Values.Kx,
    Values.AquiferThickness, Values.SpecificYield, Values.RechargeTime, Values.SimulationTime,
    Values.HalfLength, Values.HalfWidth, Values.X, Values.Y);
  while RootHantushRect(X2, @Values) > 0 do
  begin
    X2 := X2 * 2;
  end;

  Result := FindRoot(0, X2, IterationControl, @Values, RootHantushRect);
end;

function HantushCircular(InitialHead, RechargeRate, Kx, AquiferThickness,
  SpecificYield, RechargeTime, SimulationTime, InifiltrationRadius,
  Radius: Double; var CriteriaMet: Boolean): double;
var
  Diffusivity: Extended;
  UZero: double;
  FirstTerm: double;
  ZRecharge: Double;
  EUZero: double;
  u: double;
  ZDischarge: Double;
  SimTme: Double;
begin
//  result := 0;
  Diffusivity := Kx*AquiferThickness/SpecificYield;
  if Radius < InifiltrationRadius then
  begin
    CriteriaMet := SimulationTime >= 0.5*Sqr(Radius)/Diffusivity;
  end
  else
  begin
    CriteriaMet := (SimulationTime >= 0.5*Sqr(InifiltrationRadius)/Diffusivity);
  end;
//  if not CriteriaMet then
//  begin
//    Exit;
//  end;
  UZero := Sqr(InifiltrationRadius)/4/Diffusivity/SimulationTime;
  // The Pi factor in V in the numerator
  // cancels pi factor in denoninator.
  FirstTerm := RechargeRate*Sqr(InifiltrationRadius)/2/Kx;
  if Radius < InifiltrationRadius then
  begin
    EUZero := Exp(-UZero);
    ZRecharge := FirstTerm*(W(UZero) - Sqr(Radius/InifiltrationRadius)*EUZero
      + 1/UZero*(1-EUZero));
  end
  else
  begin
//    if not CriteriaMet then
//    begin
//      Exit;
//    end;
    u := Sqr(Radius)/4/Diffusivity/SimulationTime;
    ZRecharge := FirstTerm*(W(u) + 0.5*UZero*Exp(-u));
  end;
  if SimulationTime <= RechargeTime then
  begin
    ZDischarge := 0;
  end
  else
  begin
    SimTme := SimulationTime-RechargeTime;
    UZero := Sqr(InifiltrationRadius)/4/Diffusivity/(SimTme);
    // The Pi factor in V in the numerator
    // cancels pi factor in denoninator.
    FirstTerm := RechargeRate*Sqr(InifiltrationRadius)/2/Kx;
    if Radius < InifiltrationRadius then
    begin
      EUZero := Exp(-UZero);
      ZDischarge := FirstTerm*(W(UZero) - Sqr(Radius/InifiltrationRadius)*EUZero
        + 1/UZero*(1-EUZero));
    end
    else
    begin
      u := Sqr(Radius)/4/Diffusivity/SimTme;
      ZDischarge := FirstTerm*(W(u) + 0.5*UZero*Exp(-u));
    end;
  end;
  if ZDischarge > ZRecharge then
  begin
    result := InitialHead;
  end
  else
  begin
    result := Sqrt(ZRecharge-ZDischarge+Sqr(InitialHead));
  end;
end;

{$IFNDEF NoInterpolate}
procedure InitialKeyValues;
var
  index: Integer;
begin
  KeyValues := TRealList.Create;
  KeyValues.Capacity := Length(Values);
  for index := 0 to Length(Values) - 1 do
  begin
    KeyValues.Add(Values[index]);
  end;
  KeyValues.Sorted := True;
end;
{$ENDIF}


{$IFNDEF NoInterpolate}
initialization
  InitialKeyValues;
{$ENDIF}

{$IFNDEF NoInterpolate}
finalization
  KeyValues.Free;
{$ENDIF}

end.
