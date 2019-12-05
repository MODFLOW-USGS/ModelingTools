unit BasisFunctionUnit;

interface

uses
  FastGEO, GoPhastTypes;

const
  niI = 0;
  niJ = 1;
  niK = 2;

  rnI = 0;
  rnJ = 1;
  rnK = 2;
  rnL = 3;

type
  TLinearNodes = (liI, liJ);
  TLinearElement = array[TLinearNodes] of double;
  TLinearNodeValues = array[TLinearNodes] of double;

  TElement = array of TPoint2D;

//  TTriangularNodes = (niI, niJ, niK);
  TTriangularElement = TElement;

//  TTriangularElement = array[TTriangularNodes] of TPoint2D;
  TTriangularNodeValues = array of double;
//  TTriangularNodeValues = array[TTriangularNodes] of double;

//  TRectangularNodes = (rnI, rnJ, rnK, rnL);
  TRectangularElement = TElement;
  TQuadrilateralElement = TElement;
//  TRectangularElement = array[TRectangularNodes] of TPoint2D;
  TRectangularNodeValues = array of double;
//  TRectangularNodeValues = array[TRectangularNodes] of double;

//  TElement = array of TPoint2D;
  T2DDirection = (dirX, dirY);

function LinearBasisFunction(const Element: TLinearElement;
  const NodeValues: TLinearNodeValues; const ALocation: double): double;

function TriangularBasisFunction(const Element: TTriangularElement;
  const NodeValues: TTriangularNodeValues; const ALocation: TPoint2D): double;

function RectangularBasisFunction(const Element: TRectangularElement;
  const NodeValues: TRectangularNodeValues; ALocation: TPoint2D): double;

procedure GetBasisFunctionFractions(Element: TElement; const ALocation: TPoint2D;
  out Fractions: TOneDRealArray; Dir: T2DDirection);

procedure MapRealWorldCoordToRectangularElementCoord(
  Element: TQuadrilateralElement; const ALocation: TPoint2D;
  out MappedLocation: TPoint2D);

function QuadrilateralBasisFunction(const Element: TQuadrilateralElement;
  const NodeValues: TRectangularNodeValues; ALocation: TPoint2D): double;

implementation

function LinearBasisFunction(const Element: TLinearElement;
  const NodeValues: TLinearNodeValues; const ALocation: double): double;
var
  Fraction: double;
begin
  Fraction := (ALocation - Element[liI])/(Element[liJ] - Element[liI]);
  result := NodeValues[liI] + Fraction * (NodeValues[liJ] - NodeValues[liI])
end;

procedure CalculateTriangularBasisFactors(const Element: TTriangularElement;
  const ALocation: TPoint2D; out WeightFactors: TOneDRealArray);
var
  CrossProducts: array[0..2] of double;
  AreaTimes2: double;
  AValue: double;
begin
  Assert(Length(Element) = 3);

  CrossProducts[niI] := Element[niJ].x*Element[niK].y
    - Element[niK].x*Element[niJ].y;
  CrossProducts[niJ] := Element[niK].x*Element[niI].y
    - Element[niI].x*Element[niK].y;
  CrossProducts[niK] := Element[niI].x*Element[niJ].y
    - Element[niJ].x*Element[niI].y;

  AreaTimes2 := 0;
  for AValue in CrossProducts do
  begin
    AreaTimes2 := AreaTimes2 + AValue;
  end;

  SetLength(WeightFactors, 3);
  WeightFactors[niI] := (CrossProducts[niI]
    + (Element[niJ].y-Element[niK].y) * ALocation.x
    + (Element[niK].x-Element[niJ].x) * ALocation.y)
    / AreaTimes2;
  WeightFactors[niJ] := (CrossProducts[niJ]
    + (Element[niK].y-Element[niI].y) * ALocation.x
    + (Element[niI].x-Element[niK].x) * ALocation.y)
    / AreaTimes2;
  WeightFactors[niK] := (CrossProducts[niK]
    + (Element[niI].y-Element[niJ].y) * ALocation.x
    + (Element[niJ].x-Element[niI].x) * ALocation.y)
    / AreaTimes2;
end;

function TriangularBasisFunction(const Element: TTriangularElement;
  const NodeValues: TTriangularNodeValues; const ALocation: TPoint2D): double;
var
  WeightFactors: TOneDRealArray;
begin
  CalculateTriangularBasisFactors(Element, ALocation, WeightFactors);

  result := WeightFactors[niI]*NodeValues[niI]
    + WeightFactors[niJ]*NodeValues[niJ]
    + WeightFactors[niK]*NodeValues[niK]
end;

procedure CalculateRectangularBasisFactors(const Element: TRectangularElement;
  ALocation: TPoint2D; out WeightFactors: TOneDRealArray);
var
  HalfXLength: double;
  HalfYLength: double;
begin
  Assert(Element[rnI].x = Element[rnL].x);
  Assert(Element[rnJ].x = Element[rnK].x);
  Assert(Element[rnI].y = Element[rnJ].y);
  Assert(Element[rnK].y = Element[rnL].y);

  HalfXLength := (Element[rnJ].x - Element[rnI].x)/2;
  HalfYLength := (Element[rnK].y - Element[rnJ].y)/2;

  ALocation.x := ALocation.x - (Element[rnI].x + HalfXLength);
  ALocation.y := ALocation.y - (Element[rnI].y + HalfYLength);

  SetLength(WeightFactors, 4);
  WeightFactors[rnI] := (1 - ALocation.x/HalfXLength)
    * (1 - ALocation.y/HalfYLength) /4;
  WeightFactors[rnJ] := (1 + ALocation.x/HalfXLength)
    * (1 - ALocation.y/HalfYLength) /4;
  WeightFactors[rnK] := (1 + ALocation.x/HalfXLength)
    * (1 + ALocation.y/HalfYLength) /4;
  WeightFactors[rnL] := (1 - ALocation.x/HalfXLength)
    * (1 + ALocation.y/HalfYLength) /4;
end;

function RectangularBasisFunction(const Element: TRectangularElement;
  const NodeValues: TRectangularNodeValues; ALocation: TPoint2D): double;
var
  WeightFactors: TOneDRealArray;
  Node: integer;
begin
  CalculateRectangularBasisFactors(Element, ALocation, WeightFactors);

  result := 0;
  for Node := 0 to 3 do
  begin
    result := result + WeightFactors[Node] * NodeValues[Node];
  end;
end;

procedure GetBasisFunctionFractions(Element: TElement; const ALocation: TPoint2D;
  out Fractions: TOneDRealArray; Dir: T2DDirection);
begin
  SetLength(Fractions,Length(Element));
  case Length(Element) of
    2:
      begin
        case Dir of
          dirX: Fractions[0] := Abs((Element[0].x-ALocation.x)/(Element[0].x-Element[1].x));
          dirY: Fractions[0] := Abs((Element[0].y-ALocation.y)/(Element[0].y-Element[1].y));
        end;
        Fractions[1] := 1-Fractions[0];
      end;
    3:
      begin
        CalculateTriangularBasisFactors(Element, ALocation, Fractions);
      end;
    4:
      begin
        CalculateRectangularBasisFactors(Element, ALocation, Fractions);
      end;
    else
      Assert(False);
  end;
end;

procedure MapRealWorldCoordToRectangularElementCoord(
  Element: TQuadrilateralElement; const ALocation: TPoint2D;
  out MappedLocation: TPoint2D);
var
  a1: Double;
  a2: double;
  b1: Double;
  b2: double;
  c1: Double;
  c2: double;
  d1: Double;
  d2: double;
  ab: double;
  ac: double;
  SolutionCase: integer;
  cb: Double;
  da: double;
  dc: Double;
  A, B, C: Double;
  X1, X2: double;
  ad: double;
  ba: double;
  db: double;
  bc: double;
  NodeIndex: Integer;
  bd: Double;
begin
  // Based on a method presented by
  // Hua, Chongyu, 1990. An inverse transformation for quadrilateral
  // isoparametric elements, Finite Elements in Analysis and Design 7:159-166.

// Node Numbering
//  4----3
//  |    |
//  |    |
//  1----2

// Note: The lemma that Hua asserts does not hold for the following element
// coordinates

//  1125702.266	201012.603
//  1125646.98	201179.7051
//  1125799.601	201179.7051
//  1125732.79	201012.603

// For these coordinates a2 = c2 = 0

  Assert(Length(Element)=4);

  d1 := 4*ALocation.x;
  d2 := 4*ALocation.y;
  for NodeIndex := 0 to Length(Element) - 1 do
  begin
    d1 := d1 - Element[NodeIndex].x;
    d2 := d2 - Element[NodeIndex].y;
  end;

  a1 :=  Element[0].x - Element[1].x + Element[2].x - Element[3].x;
  a2 :=  Element[0].y - Element[1].y + Element[2].y - Element[3].y;
  b1 := -Element[0].x + Element[1].x + Element[2].x - Element[3].x;
  b2 := -Element[0].y + Element[1].y + Element[2].y - Element[3].y;
  c1 := -Element[0].x - Element[1].x + Element[2].x + Element[3].x;
  c2 := -Element[0].y - Element[1].y + Element[2].y + Element[3].y;

  ab := a1*b2 - a2*b1;
  ac := a1*c2 - a2*c1;

  if a1*a2*ab*ac <> 0 then
  begin
    SolutionCase := 1;
  end
  else if (a1 = 0) and (a2*c1 <> 0) then
  begin
    SolutionCase := 2;
  end
  else if (a2 = 0) and (a1*b2 <> 0) then
  begin
    SolutionCase := 3;
  end
  else if (a1*a2 <> 0) and (ab = 0) then
  begin
    SolutionCase := 4;
  end
  else if (a1*a2 <> 0) and (ac = 0) then
  begin
    SolutionCase := 5;
  end
  else
  begin
    SolutionCase := 6;
  end;

  case SolutionCase of
    1,2,3:
      begin
        // X coordinate is the solution of a quadratic equation
        // A*Sqr(X) + B*X + C
        // where A = ab
        // B = cb + da
        // C = dc
        cb := c1*b2 - c2*b1;
        da := d1*a2 - d2*a1;
        dc := d1*c2 - d2*c1;
        A := ab;
        B := cb + da;
        C := dc;
        if A = 0 then
        begin
          MappedLocation.x := -B/C;
        end
        else
        begin
          X1 := (-B + Sqrt(Sqr(B)- 4*A*C))/(2*A);
          X2 := (-B - Sqrt(Sqr(B)- 4*A*C))/(2*A);
          if (-1 <= X1) and (X1 <= 1) then
          begin
            MappedLocation.x := X1;
          end
          else
          begin
            Assert((-1 <= X2) and (X2 <= 1));
            MappedLocation.x := X2;
          end;
        end;

        ad := -da;
        ba := -ab;
        if ac <> 0 then
        begin
          // Hua formula
          MappedLocation.y := (ad + ba*MappedLocation.x)/ac;
        end
        else
        begin
          // formula to use in the case where the lemma does not hold.
          MappedLocation.y := (b1*MappedLocation.x -d1)/(-c1 -a1*MappedLocation.x);
        end;
      end;
    4:
      begin
        dc := d1*c2 - d2*c1;
        da := d1*a2 - d2*a1;
        ad := -da;
        MappedLocation.x := (a1*dc)/(b1*ac +a1*ad);
        MappedLocation.y := ad/ac;
      end;
    5:
      begin
        da := d1*a2 - d2*a1;
        ad := -da;
        db := d1*b2 - d2*b1;
        MappedLocation.x := ad/ab;
        MappedLocation.y := (a1*db)/(c1*ab +a1*ad);
      end;
    6:
      begin
        dc := d1*c2 - d2*c1;
        bc := b1*c2 - b2*c1;
        bd := b1*d2 - b2*d1;
        MappedLocation.x := dc/(a1*d2 + bc);
        MappedLocation.y := bd/(a2*d1 + bc);
      end;
    else
      Assert(False);
  end;
end;

function QuadrilateralBasisFunction(const Element: TQuadrilateralElement;
  const NodeValues: TRectangularNodeValues; ALocation: TPoint2D): double;
var
  MappedLocation: TPoint2D;
  RectangularElement: TRectangularElement;
begin
  MapRealWorldCoordToRectangularElementCoord(Element, ALocation, MappedLocation);
  SetLength(RectangularElement, 4);
  RectangularElement[0].x := -1;
  RectangularElement[0].y := -1;
  RectangularElement[1].x := 1;
  RectangularElement[1].y := -1;
  RectangularElement[2].x := 1;
  RectangularElement[2].y := 1;
  RectangularElement[3].x := -1;
  RectangularElement[3].y := 1;
  result := RectangularBasisFunction(RectangularElement, NodeValues, MappedLocation)
end;

end.
