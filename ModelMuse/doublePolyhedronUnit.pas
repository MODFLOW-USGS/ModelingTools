unit doublePolyhedronUnit;

interface

{
  Much of this code is adapted from Chapter 7 of
  O'Rourke, Joseph. 1998. "Computational Geometry in C"
  (Second Edition). Cambridge University Press 376 p.
}

uses System.Types, Classes, SysUtils, Math, SolidGeom, {S_Stream,} IntListUnit, OctTreeClass;

const
  X = 0;
  Y = 1;
  Z = 2;
  BoxMax = 2;

var
  Epsilon: extended;
  VI_array: array of integer;

type
  TPointi = array [0 .. DIM - 1] of integer;
  TPointdArray = array [0 .. BoxMax - 1] of TPointd;
  // integer point //adapted from "Computational Geometry in C"

  TInPolyhedronResult = (ipVertex, ipEdge, ipFace, ipInterior, ipExterior);
  TSegPlaneIntersectResult = (spiWithin, spiQOnPlane, spiROnPlane, spiOffPlane,
    spiIntersects);
  TInTriangleResult = (itVertex, itEdge, itInterior, itExterior);
  TSegTriangleIntersectionResult = (stiVertex, stiEdge, stiFace, stiMisses,
    stiVertexOnSegment, stiEdgeOnSegment, stiFaceOnSegment, stiDegenerate);
  TBoxTestResult = (btOutside, btUnknown);

  TSegmentObject = class(TObject)
    StartLoc, EndLoc: TPointd;
    function Length: extended;
    function TwoDIntersection(ASegment: TSegmentObject;
      var Intersection: TPointd): boolean;
    function Direction: TPointd;
    function Parallel(const ASegment: TSegmentObject): boolean;
    function Colinear(const ASegment: TSegmentObject): boolean;
    function Contains(const ASegment: TSegmentObject): boolean;
  end;

  TSegmentList = Class(TObject)
  private
    FList: TList;
    function Getitems(Index: integer): TSegmentObject;
  public
    function Count: integer;
    Constructor Create;
    Destructor Destroy; override;
    property items[Index: integer]: TSegmentObject read Getitems; default;
    function IndexOf(ASegment: TSegmentObject): integer;
    function Add(ASegment: TSegmentObject): integer;
  end;

  TPolyhedron = class(TObject)
  private
    // number of faces
    FF: integer;
    // number of vertices.
    FV: integer;
    // adapted from "Computational Geometry in C"
    FVertices: array of TPointd;
    // adapted from "Computational Geometry in C"
    FFaces: array of TPointi;
    // adapted from "Computational Geometry in C"
    FBox: array of TPointdArray;
    FOctTree: TRbwOctTree;
    function GetF: integer;
    function GetV: integer;
    property F: integer read GetF write FF;
    property V: integer read GetV write FV;
    // adapted from "Computational Geometry in C"
    function Inbox(q: TPointd): boolean;
    // adapted from "Computational Geometry in C"
    procedure RandomRay(var ray: TPointd);
    // adapted from "Computational Geometry in C"
    function BoxTest(n: integer; a, b: TPointd): TBoxTestResult;
    // adapted from "Computational Geometry in C"
    procedure AddVec(q: TPointd; var ray: TPointd);
    // adapted from "Computational Geometry in C"
    procedure NormalVec(a, b, c: TPointd; var n: TPointd);
    // adapted from "Computational Geometry in C"
    function Dot(a: TPointd; b: TPointd): extended;
    // adapted from "Computational Geometry in C"
    function PlaneCoeff(T: TPointi; var n: TPointd; var D: extended): integer;
    // adapted from "Computational Geometry in C"
    procedure SubVec(a, b: TPointd; var c: TPointd);
    // adapted from "Computational Geometry in C"
    function SegPlaneInt(T: TPointi; q, r: TPointd; var p: TPointd;
      var m: integer): TSegPlaneIntersectResult;
    // adapted from "Computational Geometry in C"
    function AreaSign(a, b, c: TPointd): integer;
    // adapted from "Computational Geometry in C"
    function InTri2D(Tp: array of TPointd; pp: TPointd): TInTriangleResult;
    // adapted from "Computational Geometry in C"
    function InTri3D(T: TPointi; m: integer; p: TPointd): TInTriangleResult;
    // adapted from "Computational Geometry in C"
    function InPlane(T: TPointi; m: integer; q, r: TPointd; p: TPointd)
      : TSegTriangleIntersectionResult;
    // adapted from "Computational Geometry in C"
    function VolumeSign(a, b, c, D: TPointd): integer;
    // adapted from "Computational Geometry in C"
    function SegTriCross(T: TPointi; q, r: TPointd)
      : TSegTriangleIntersectionResult;
    // adapted from "Computational Geometry in C"
    function SegTriInt(T: TPointi; q, r: TPointd; var p: TPointd)
      : TSegTriangleIntersectionResult;
    function FacesSame(FaceIndex1, FaceIndex2: integer): boolean;
    Function GetVertex(const VIndex: integer): TPointd;
    Function GetFace(const FIndex: integer): TPointi;
    Function GetBox(const BIndex: integer): TPointdArray;
    procedure SetVertexValue(const VIndex, DimIndex: integer;
      const Value: extended);
    procedure SetVertex(const VIndex: integer; const Value: TPointd);
    procedure SetFace(const FIndex: integer; const Value: TPointi);
    procedure SetFaceValue(const FIndex, DimIndex: integer;
      const Value: integer);
    procedure SetBox(const BIndex: integer; const Value: TPointdArray);
    procedure SetBoxPointd(const BIndex, CornerIndex: integer;
      const Value: TPointd);
    procedure SetBoxValue(const BIndex, CornerIndex, DimIndex: integer;
      const Value: extended);
  public
    bmin, bmax: TPointd;
    radius: extended;
    StorageIndex: integer;
    property OctTree: TRbwOctTree read FOctTree;
    property Vertices[const VIndex: integer]: TPointd read GetVertex
      write SetVertex;
    property VertexValue[const VIndex, DimIndex: integer]: extended
      write SetVertexValue;
    // adapted from "Computational Geometry in C"
    property Faces[const FIndex: integer]: TPointi read GetFace write SetFace;
    Property FaceValue[const FIndex, DimIndex: integer]: integer
      write SetFaceValue;
    // adapted from "Computational Geometry in C"
    property Box[const BIndex: integer]: TPointdArray read GetBox write SetBox;
    property BoxPointd[const BIndex, CornerIndex: integer]: TPointd
      write SetBoxPointd;
    property BoxValue[const BIndex, CornerIndex, DimIndex: integer]: extended
      write SetBoxValue;
    // adapted from "Computational Geometry in C"
    constructor Create(VertexCount, FaceCount: integer);
    destructor Destroy; override;
    function InPolyHedron(q: TPointd): TInPolyhedronResult;
    // adapted from "Computational Geometry in C"
    procedure ComputePolyhedronBox;
    function ComputeBox: extended;
    // adapted from "Computational Geometry in C"
    function SegmentIntersection(Const ASegment: TSegmentObject): TSegmentList;
    Procedure GetProps(Var Vol, Area: extended);
    function PolygonIntersect(APolygon: TXYZPlane): TXYZPlaneList;
    function VertexIndex(Avertex: TPointd; const LastVertex: integer;
      EpsilonX: double = 1E-6; EpsilonY: double = 1E-6; EpsilonZ: double = 1E-6)
      : integer; overload;
    function VertexIndex(Avertex: TPointd): integer; overload;
    Procedure EliminateInternalFaces(LastFace, LastVertex: integer);
    Property FaceCount: integer read GetF;
    Property VertexCount: integer read GetV;
    procedure FreeArrays;
    procedure SetArrayLengths(LastFace, LastVertex: integer);
  end;

function SortPointObjects(Item1, Item2: Pointer): integer;
function WrongEndsMatch(FirstSegment, SecondSegment: TSegmentObject): boolean;
function EndsMatch(FirstSegment, SecondSegment: TSegmentObject): boolean;

var
  PolyhedronPositions: TIntegerList;

implementation

uses SolidUnit, RealListUnit;

type
  PInteger = ^integer;

function SortPointObjects(Item1, Item2: Pointer): integer;
{
  SortPointObjects sorts TPointObject's in order by their X, Y, and Z
  coordinates.
}
var
  Point1, Point2: TPointObject;
begin
  Point1 := Item1;
  Point2 := Item2;
  if Point1.Location[X] < Point2.Location[X] then
  begin
    result := -1;
  end
  else if Point1.Location[X] > Point2.Location[X] then
  begin
    result := 1;
  end
  else if Point1.Location[Y] < Point2.Location[Y] then
  begin
    result := -1;
  end
  else if Point1.Location[Y] > Point2.Location[Y] then
  begin
    result := 1;
  end
  else if Point1.Location[Z] < Point2.Location[Z] then
  begin
    result := -1;
  end
  else if Point1.Location[Z] > Point2.Location[Z] then
  begin
    result := 1;
  end
  else
  begin
    result := 0;
  end;
end;

function TPolyhedron.Inbox(q: TPointd): boolean;
{
  Inbox tests whether q is inside the smallest box with sides parallel
  to the principle coordinate axes in which the polygon will fit.

  Bmin, and bmax must already have been assigned before calling Inbox.

}
begin
  result := ((bmin[X] <= q[X]) and (q[X] <= bmax[X]) and (bmin[Y] <= q[Y]) and
    (q[Y] <= bmax[Y]) and (bmin[Z] <= q[Z]) and (q[Z] <= bmax[Z]))
end;

procedure TPolyhedron.RandomRay(var ray: TPointd);
{
  Generate a random point on a sphere with a radius of "radius".
  the sphere is sliced at z, and a random point at angle t
  generated on the circle of intersection.

  "Radius" must already have been set before calling RandomRay.
}
var
  x_l, y_l, z_l, w, T: extended;
begin

  z_l := random;
  T := Pi * random;
  w := sqrt(1 - Sqr(z_l));
  x_l := w * cos(T);
  y_l := w * sin(T);

  ray[X] := radius * x_l;
  ray[Y] := radius * y_l;
  ray[Z] := radius * z_l;

end;

function TPolyhedron.BoxTest(n: integer; a, b: TPointd): TBoxTestResult;
{
  Test whether the line segment a,b intersects the box around face n

  Box must have been set before calling BoxTest.
}
var
  i: integer; // * Coordinate index */
  w: extended;
begin
  for i := 0 to DIM - 1 do
  begin
    w := Box[n][0, i]; // * min: lower left */
    if ((a[i] < w) and (b[i] < w)) then
    begin
      result := btOutside;
      Exit;
    end;
    w := Box[n][1][i]; // * max: upper right */
    if ((a[i] > w) and (b[i] > w)) then
    begin
      result := btOutside;
      Exit;
    end;
  end;
  result := btUnknown;

end;

procedure TPolyhedron.AddVec(q: TPointd; var ray: TPointd);
{
  Add two vectors.
}
var
  i: integer;
begin
  for i := 0 to DIM - 1 do
  begin
    ray[i] := q[i] + ray[i]
  end;
end;

procedure TPolyhedron.NormalVec(a, b, c: TPointd; var n: TPointd);
{
  Calculate the normal N to the plane defined by a, by, c.
}
begin
  n[X] := (c[Z] - a[Z]) * (b[Y] - a[Y]) - (b[Z] - a[Z]) * (c[Y] - a[Y]);
  n[Y] := (b[Z] - a[Z]) * (c[X] - a[X]) - (b[X] - a[X]) * (c[Z] - a[Z]);
  n[Z] := (b[X] - a[X]) * (c[Y] - a[Y]) - (b[Y] - a[Y]) * (c[X] - a[X]);
end;

destructor TPolyhedron.Destroy;
begin
  FOctTree.Free;
  inherited;
end;

function TPolyhedron.Dot(a: TPointd; b: TPointd): extended;
{
  Calculate the dot product of a and b.
}
var
  i: integer;
begin
  result := 0.0;

  for i := 0 to DIM - 1 do
  begin
    result := result + a[i] * b[i];
  end;
end;

function TPolyhedron.PlaneCoeff(T: TPointi; var n: TPointd;
  var D: extended): integer;
{
  Compute the normal (N) to Face T and the dot product of N and the first
  point in T. Also determine the largest component of N.
}
var
  i: integer;
  t_l: extended; // * Temp storage */
  biggest: extended; // * Largest component of normal vector. */
  m: integer; // * Index of largest component. */
begin
  biggest := 0;
  m := 0;

  NormalVec(Vertices[T[0]], Vertices[T[1]], Vertices[T[2]], n);
  D := Dot(Vertices[T[0]], n);

  // * Find the largest component of N. */

  for i := 0 to DIM - 1 do
  begin
    t_l := Abs(n[i]);
    if (t_l > biggest) then
    begin
      biggest := t_l;
      m := i;
    end;
  end;

  result := m;

end;

procedure TPolyhedron.SubVec(a, b: TPointd; var c: TPointd);
{
  Subtract two vectors.
}
var
  i: integer;
begin
  for i := 0 to DIM - 1 do
  begin
    c[i] := a[i] - b[i];
  end;
end;

function TPolyhedron.SegPlaneInt(T: TPointi; q, r: TPointd; var p: TPointd;
  var m: integer): TSegPlaneIntersectResult;
{
  Determine the point of intersection (p) between the face T and the the line
  segment defined by q and r. Also determine the largest component of the Normal
  to T.
}
var
  n: TPointd;
  D: extended;
  rq: TPointd;
  num, denom, t_l: extended;
  i: integer;
  // Epsilon, Temp : extended;

  // volQ, volR : integer;
begin
  { Epsilon := bmax[0] - bmin[0];
    for i := 1 to Dim -1 do
    begin
    Temp := bmax[i] - bmin[i];
    if Temp < Epsilon then
    begin
    Epsilon := Temp;
    end;
    end;
    Epsilon := Epsilon/1000; }
  {
    volQ := VolumeSign( q, Vertices[T[0]], Vertices[T[1]], Vertices[T[2]]);
    volR := VolumeSign( r, Vertices[T[0]], Vertices[T[1]], Vertices[T[2]]);
    if (volQ = 0) and (volR = 0) then
    begin
    result := spiWithin;
    p := q;
    Exit;
    end
    else if (volQ = 0) then
    begin
    result := spiQOnPlane;
    p := q;
    Exit;
    end
    else if (volR = 0) then
    begin
    result := spiROnPlane;
    p := r;
    Exit;
    end; }

  for i := 0 to DIM - 1 do
  begin
    n[i] := 0;
  end;
  D := 0;
  m := PlaneCoeff(T, n, D);
  num := D - Dot(q, n);
  SubVec(r, q, rq);
  denom := Dot(rq, n);

  if NearlyTheSame(denom, 0.0, Epsilon) then
  // * Segment is parallel to plane. */
  begin
    if NearlyTheSame(num, 0.0, Epsilon) then // * q is on plane. */
    begin
      result := spiWithin;
      Exit;
    end
    else
    begin
      result := spiOffPlane;
      Exit;
    end;
  end
  else
  begin
    t_l := num / denom;
  end;

  for i := 0 to DIM - 1 do
  begin
    p[i] := q[i] + t_l * (r[i] - q[i]);
  end;

  if ((0.0 + Epsilon < t_l) and (t_l < 1.0 - Epsilon)) or
    NearlyTheSame(0.0 + Epsilon, t_l, Epsilon) or
    NearlyTheSame(1.0 - Epsilon, t_l, Epsilon) then
  begin
    result := spiIntersects;
  end
  else if NearlyTheSame(num, 0.0, Epsilon) then // * t_l == 0 */
  begin
    result := spiQOnPlane
  end
  else if NearlyTheSame(num, denom, Epsilon) then // * t_l == 1 */
  begin
    result := spiROnPlane;
  end
  else
  begin
    result := spiOffPlane;
  end;
end;

function TPolyhedron.AreaSign(a, b, c: TPointd): integer;
var
  area2: extended;
begin

  area2 := (b[0] - a[0]) * (c[1] - a[1]) - (c[0] - a[0]) * (b[1] - a[1]);

  if (area2 > Epsilon) then
  begin
    result := 1;
  end
  else if (area2 < -Epsilon) then
  begin
    result := -1;
  end
  else
  begin
    result := 0;
  end;
end;

function TPolyhedron.InTri2D(Tp: array of TPointd; pp: TPointd)
  : TInTriangleResult;
{
  Determine whether pp is in Tp. Only the first two members of each tPointd
  are used.
}
var
  area0, area1, area2: integer;
begin

  // * compute three AreaSign() values for pp
  // with respect to each edge of the face in 2D */
  area0 := AreaSign(pp, Tp[0], Tp[1]);
  area1 := AreaSign(pp, Tp[1], Tp[2]);
  area2 := AreaSign(pp, Tp[2], Tp[0]);

  if ((area0 = 0) and (area1 > 0) and (area2 > 0) or (area1 = 0) and (area0 > 0)
    and (area2 > 0) or (area2 = 0) and (area0 > 0) and (area1 > 0)) then
  begin
    result := itEdge;
  end

  else if ((area0 = 0) and (area1 < 0) and (area2 < 0) or (area1 = 0) and
    (area0 < 0) and (area2 < 0) or (area2 = 0) and (area0 < 0) and (area1 < 0))
  then
  begin
    result := itEdge;
  end

  else if ((area0 > 0) and (area1 > 0) and (area2 > 0) or (area0 < 0) and
    (area1 < 0) and (area2 < 0)) then
  begin
    result := itInterior;
  end

  else if ((area0 = 0) and (area1 = 0) and (area2 = 0)) then
  begin
    Assert(False);
    result := itExterior;
  end

  else if ((area0 = 0) and (area1 = 0) or (area0 = 0) and (area2 = 0) or
    (area1 = 0) and (area2 = 0)) then
  begin
    result := itVertex;
  end

  else
  begin
    result := itExterior;
  end;
end;

function TPolyhedron.InTri3D(T: TPointi; m: integer; p: TPointd)
  : TInTriangleResult;
{
  determine whether p lies in T.
}
var
  i: integer; // * Index for X,Y,Z           */
  j: integer; // * Index for X,Y             */
  k: integer; // * Index for triangle vertex */
  pp: TPointd; // * projected p */
  Tp: array [0 .. 2] of TPointd; // * projected T: three new vertices */
begin

  // * Project out coordinate m in both p and the triangular face */
  j := 0;
  for i := 0 to DIM - 1 do
  begin
    if (i <> m) then // * skip largest coordinate */
    begin
      pp[j] := p[i];
      for k := 0 to 2 do
      begin
        Tp[k][j] := Vertices[T[k]][i];
      end;
      Inc(j);
    end;
  end;
  result := InTri2D(Tp, pp);
end;

function TPolyhedron.InPlane(T: TPointi; m: integer; q, r: TPointd; p: TPointd)
  : TSegTriangleIntersectionResult;
begin
  // * NOT IMPLEMENTED */
  result := stiDegenerate;
  // return 'p';
end;

function TPolyhedron.VolumeSign(a, b, c, D: TPointd): integer;
var
  Vol: extended;
  ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz: extended;
  bxdx, bydy, bzdz, cxdx, cydy, czdz: extended;
begin
  ax := a[X];
  ay := a[Y];
  az := a[Z];
  bx := b[X];
  by := b[Y];
  bz := b[Z];
  cx := c[X];
  cy := c[Y];
  cz := c[Z];
  dx := D[X];
  dy := D[Y];
  dz := D[Z];

  bxdx := bx - dx;
  bydy := by - dy;
  bzdz := bz - dz;
  cxdx := cx - dx;
  cydy := cy - dy;
  czdz := cz - dz;
  Vol := (az - dz) * (bxdx * cydy - bydy * cxdx) + (ay - dy) *
    (bzdz * cxdx - bxdx * czdz) + (ax - dx) * (bydy * czdz - bzdz * cydy);

  // This may not work for floating point values.
  if (Vol > Epsilon) then
  begin
    result := 1;
  end
  else if (Vol < -Epsilon) then
  begin
    result := -1;
  end
  else
  begin
    result := 0;
  end;
end;

function TPolyhedron.SegTriCross(T: TPointi; q, r: TPointd)
  : TSegTriangleIntersectionResult;
var
  vol0, vol1, vol2: integer;
begin
  if VertexIndex(q) > -1 then
  begin
    result := stiVertex;
    Exit;
  end;

  vol0 := VolumeSign(q, Vertices[T[0]], Vertices[T[1]], r);
  vol1 := VolumeSign(q, Vertices[T[1]], Vertices[T[2]], r);
  vol2 := VolumeSign(q, Vertices[T[2]], Vertices[T[0]], r);

  // * Same sign: segment intersects interior of triangle. */
  if (((vol0 > 0) and (vol1 > 0) and (vol2 > 0)) or
    ((vol0 < 0) and (vol1 < 0) and (vol2 < 0))) then
  begin
    result := stiFaceOnSegment;
  end

  // * Opposite sign: no intersection between segment and triangle */
  else if (((vol0 > 0) or (vol1 > 0) or (vol2 > 0)) and
    ((vol0 < 0) or (vol1 < 0) or (vol2 < 0))) then
  begin
    result := stiMisses
  end

  else if ((vol0 = 0) and (vol1 = 0) and (vol2 = 0)) then
  begin
    Assert(False);
    result := stiMisses
  end

  // * Two zeros: segment intersects vertex. */
  else if (((vol0 = 0) and (vol1 = 0)) or ((vol0 = 0) and (vol2 = 0)) or
    ((vol1 = 0) and (vol2 = 0))) then
  begin
    result := stiVertex
  end

  // */ One zero: segment intersects edge. */
  else if ((vol0 = 0) or (vol1 = 0) or (vol2 = 0)) then
  begin
    result := stiEdge
  end

  else
  begin
    Assert(False);
    result := stiMisses;
  end;
end;

function TPolyhedron.SegTriInt(T: TPointi; q, r: TPointd; var p: TPointd)
  : TSegTriangleIntersectionResult;
var
  code: TSegPlaneIntersectResult;
  m: integer;
begin
  m := -1;

  code := SegPlaneInt(T, q, r, p, m);

  case code of
    spiOffPlane:
      begin
        result := stiMisses;
      end;
    spiQOnPlane:
      begin
        case InTri3D(T, m, q) of
          itVertex:
            begin
              result := stiVertex;
            end;
          itEdge:
            begin
              result := stiEdge;
            end;
          itInterior:
            begin
              result := stiFace;
            end;
          itExterior:
            begin
              result := stiMisses;
            end
        else
          begin
            Assert(False);
            result := stiMisses;
          end
        end;
      end;
    spiROnPlane:
      begin
        case InTri3D(T, m, r) of
          itVertex:
            begin
              result := stiVertex;
            end;
          itEdge:
            begin
              result := stiEdge;
            end;
          itInterior:
            begin
              result := stiFace;
            end;
          itExterior:
            begin
              result := stiMisses;
            end
        else
          begin
            Assert(False);
            result := stiMisses;
          end
        end;
      end;
    spiWithin:
      begin
        result := InPlane(T, m, q, r, p);
      end;
    spiIntersects:
      begin
        result := SegTriCross(T, q, r);
      end;
  else
    begin
      Assert(False);
      result := stiMisses;
    end
  end;
end;

function TPolyhedron.InPolyHedron(q: TPointd): TInPolyhedronResult;
var
  r: TPointd; // ray endpoint
  p: TPointd; // intersection point; not used
  f_l: integer;
  crossings: integer;
  code: TSegTriangleIntersectionResult;
  VertexIndex { , DimIndex } : integer;
  // Same : boolean;
label Loop;
begin
  if not Inbox(q) then
  begin
    result := ipExterior;
    Exit;
  end;

  for VertexIndex := 0 to V - 1 do
  begin
    { Same := True;
      for DimIndex := 0 to DIM-1 do
      begin
      Same := Same and
      NearlyTheSame(q[DimIndex], Vertices[VertexIndex][DimIndex],Epsilon);
      if not Same then
      begin
      break;
      end;
      end; }
    if NearlyTheSame(q, Vertices[VertexIndex], Epsilon) then
    begin
      result := ipVertex;
      Exit;
    end;
  end;

//  crossings := 0;
Loop:
  While True do
  begin
    crossings := 0;
    RandomRay(r);
    AddVec(q, r);
    for f_l := 0 to F - 1 do // * Begin check each face */
    begin
      if (BoxTest(f_l, q, r) = btOutside) then
      begin
        code := stiMisses;
      end
      else
      begin
        code := SegTriInt(Faces[f_l], q, r, p)
      end;
      case code of
        stiDegenerate, stiVertexOnSegment, stiEdgeOnSegment:
          begin
            Goto Loop;
            // degenerate, start over
          end;
        stiFaceOnSegment:
          begin
            Inc(crossings);
          end;
        stiVertex:
          begin
            result := ipVertex;
            Exit;
          end;
        stiEdge:
          begin
            result := ipEdge;
            Exit;
          end;
        stiFace:
          begin
            result := ipFace;
            Exit;
          end;
        stiMisses:
          begin
            // do nothing.
          end;
      else
        begin
          Assert(False);
        end;
      end;

    end;
    break;

  end;
  if Odd(crossings) then
  begin
    result := ipInterior;
  end
  else
  begin
    result := ipExterior;
  end;

end;

constructor TPolyhedron.Create(VertexCount, FaceCount: integer);
var
  Index, InnerIndex: integer;
begin
  inherited Create;
  FOctTree := TRbwOctTree.Create(nil);
  V := VertexCount;
  SetLength(FVertices, VertexCount);
  F := FaceCount;
  SetLength(FFaces, FaceCount);
  SetLength(FBox, FaceCount);
  for Index := 0 to FV - 1 do
  begin
    for InnerIndex := 0 to DIM - 1 do
    begin
      VertexValue[Index, InnerIndex] := 0;
    end;
  end;
  for Index := 0 to FF - 1 do
  begin
    for InnerIndex := 0 to DIM - 1 do
    begin
      FaceValue[Index, InnerIndex] := 0;
    end;
  end;
end;

procedure TPolyhedron.ComputePolyhedronBox;
var
  i, j: integer;
begin

  for i := 0 to DIM - 1 do
  begin
    bmin[i] := Vertices[0][i];
    bmax[i] := Vertices[0][i];
  end;

  for i := 0 to V - 1 do
  begin
    for j := 0 to DIM - 1 do
    begin
      if (Vertices[i][j] < bmin[j]) then
      begin
        bmin[j] := Vertices[i][j];
      end;
      if (Vertices[i][j] > bmax[j]) then
      begin
        bmax[j] := Vertices[i][j];
      end;
    end;
  end;
end;

function TPolyhedron.ComputeBox: extended;
var
  // i, j: integer;
  FaceIndex: integer;
  AFace: TPointi;
  Avertex: TPointd;
  VertexIndex, InnerVertexIndex: integer;
begin
  ComputePolyhedronBox;
  { for i := 0 to DIM-1 do
    begin
    bmin[i] := Vertices[0][i];
    bmax[i] := Vertices[0][i];
    end;

    for i := 0 to V-1 do
    begin
    for j := 0 to DIM-1 do
    begin
    if( Vertices[i][j] < bmin[j] ) then
    begin
    bmin[j] := Vertices[i][j];
    end;
    if( Vertices[i][j] > bmax[j] ) then
    begin
    bmax[j] := Vertices[i][j];
    end;
    end;
    end; }

  radius := sqrt(Sqr(bmax[X] - bmin[X]) + Sqr(bmax[Y] - bmin[Y]) +
    Sqr(bmax[Z] - bmin[Z])) * 2;

  for FaceIndex := 0 to F - 1 do
  begin
    AFace := Faces[FaceIndex];
    Avertex := Vertices[AFace[0]];
    BoxPointd[FaceIndex, 0] := Avertex;
    BoxPointd[FaceIndex, 1] := Avertex;
    for VertexIndex := 1 to DIM - 1 do
    begin
      Avertex := Vertices[AFace[VertexIndex]];
      for InnerVertexIndex := 0 to DIM - 1 do
      begin
        if Avertex[InnerVertexIndex] < Box[FaceIndex][0, InnerVertexIndex] then
        begin
          BoxValue[FaceIndex, 0, InnerVertexIndex] := Avertex[InnerVertexIndex];
        end;
        if Avertex[InnerVertexIndex] > Box[FaceIndex][1, InnerVertexIndex] then
        begin
          BoxValue[FaceIndex, 1, InnerVertexIndex] := Avertex[InnerVertexIndex];
        end;
      end;

    end;
  end;

  result := radius + 1;
end;

function TPolyhedron.SegmentIntersection(Const ASegment: TSegmentObject)
  : TSegmentList;
var
  VertexList, OrderList: TList;
  StartPoint, EndPoint: TPointObject;
  FaceIndex: integer;
  IntersectionLocation: TPointd;
  code: TSegTriangleIntersectionResult;
  IntermediatePoint: TPointObject;
  FirstPointOK: boolean;
  Index: integer;
  Point1, Point2: TPointObject;
  NewSegment: TSegmentObject;
  PointIndex: integer;
  Edge, Line: TSegmentObject;
  EdgeList: TList;
  EdgeIndex: integer;
  fraction: double;
  DimIndex: integer;
begin
  result := TSegmentList.Create;
  if ((ASegment.StartLoc[X] > bmax[X] + Epsilon) and
    (ASegment.EndLoc[X] > bmax[X] + Epsilon)) or
    ((ASegment.StartLoc[Y] > bmax[Y] + Epsilon) and
    (ASegment.EndLoc[Y] > bmax[Y] + Epsilon)) or
    ((ASegment.StartLoc[Z] > bmax[Z] + Epsilon) and
    (ASegment.EndLoc[Z] > bmax[Z] + Epsilon)) or
    ((ASegment.StartLoc[X] < bmin[X] - Epsilon) and
    (ASegment.EndLoc[X] < bmin[X] - Epsilon)) or
    ((ASegment.StartLoc[Y] < bmin[Y] - Epsilon) and
    (ASegment.EndLoc[Y] < bmin[Y] - Epsilon)) or
    ((ASegment.StartLoc[Z] < bmin[Z] - Epsilon) and
    (ASegment.EndLoc[Z] < bmin[Z] - Epsilon)) then
  begin
    Exit;
  end;

  VertexList := TList.Create;
  OrderList := TList.Create;
  StartPoint := TPointObject.Create;
  EndPoint := TPointObject.Create;
  try
    OrderList.Capacity := 2;
    StartPoint.Location := ASegment.StartLoc;
    EndPoint.Location := ASegment.EndLoc;
    OrderList.Add(StartPoint);
    OrderList.Add(EndPoint);
    VertexList.Add(StartPoint);
    if InPolyHedron(StartPoint.Location) <> ipExterior then
    begin
      FirstPointOK := True;
    end
    else
    begin
      FirstPointOK := False;
    end;
    if InPolyHedron(EndPoint.Location) <> ipExterior then
    begin
      VertexList.Add(EndPoint);
    end;
    for FaceIndex := 0 to F - 1 do
    begin
      code := SegTriInt(Faces[FaceIndex], StartPoint.Location,
        EndPoint.Location, IntersectionLocation);
      if code = stiDegenerate then
      begin
        EdgeList := TList.Create;
        Line := TSegmentObject.Create;
        try
          EdgeList.Capacity := DIM;
          Line.StartLoc := StartPoint.Location;
          Line.EndLoc := EndPoint.Location;

          for PointIndex := 0 to DIM - 2 do
          begin
            Edge := TSegmentObject.Create;
            Edge.StartLoc := Vertices[Faces[FaceIndex][PointIndex]];
            Edge.EndLoc := Vertices[Faces[FaceIndex][PointIndex + 1]];
            EdgeList.Add(Edge);
          end;

          Edge := TSegmentObject.Create;
          Edge.StartLoc := Vertices[Faces[FaceIndex][DIM - 1]];
          Edge.EndLoc := Vertices[Faces[FaceIndex][0]];
          EdgeList.Add(Edge);

          for EdgeIndex := 0 to EdgeList.Count - 1 do
          begin
            Edge := EdgeList[EdgeIndex];
            if Line.Contains(Edge) then
            begin
              IntermediatePoint := TPointObject.Create;
              IntermediatePoint.Location := Edge.StartLoc;
              VertexList.Add(IntermediatePoint);

              IntermediatePoint := TPointObject.Create;
              IntermediatePoint.Location := Edge.EndLoc;
              VertexList.Add(IntermediatePoint);
            end
            else
            begin
              if Line.TwoDIntersection(Edge, IntersectionLocation) then
              begin
                fraction := sqrt(Sqr(IntersectionLocation[X] - Line.StartLoc[X])
                  + Sqr(IntersectionLocation[Y] - Line.StartLoc[Y])) /
                  sqrt(Sqr(Line.StartLoc[X] - Line.EndLoc[X]) +
                  Sqr(Line.StartLoc[Y] - Line.EndLoc[Y]));

                IntersectionLocation[Z] := Line.StartLoc[Z] + fraction *
                  (Line.EndLoc[Z] - Line.StartLoc[Z]);

                IntermediatePoint := TPointObject.Create;
                IntermediatePoint.Location := IntersectionLocation;
                VertexList.Add(IntermediatePoint);
              end;
            end;
          end;

        finally
          for EdgeIndex := 0 to EdgeList.Count - 1 do
          begin
            Edge := EdgeList[EdgeIndex];
            Edge.Free;
          end;

          EdgeList.Free;
          Line.Free;
        end;

      end
      else if (code <> stiMisses) then
      begin
        IntermediatePoint := TPointObject.Create;
        IntermediatePoint.Location := IntersectionLocation;
        VertexList.Add(IntermediatePoint);
      end;
    end;
    VertexList.Sort(SortPointObjects);
    OrderList.Sort(SortPointObjects);
    if OrderList.IndexOf(StartPoint) <> 0 then
    begin
      for Index := 0 to (VertexList.Count - 1) div 2 do
      begin
        VertexList.Exchange(Index, VertexList.Count - 1 - Index);
      end;
    end;
    if not FirstPointOK then
    begin
      VertexList.Remove(StartPoint);
    end;
    if VertexList.Count > 1 then
    begin
      for Index := 0 to VertexList.Count - 2 do
      begin
        Point1 := VertexList[Index];
        Point2 := VertexList[Index + 1];
        if not NearlyTheSame(Point1.Location, Point2.Location, Epsilon) { or
          not NearlyTheSame(Point1.Location[Y], Point2.Location[Y],
          Epsilon) or
          not NearlyTheSame(Point1.Location[Z], Point2.Location[Z],
          Epsilon) } then
        begin
          for DimIndex := X to Z do
          begin
            IntersectionLocation[DimIndex] :=
              (Point1.Location[DimIndex] + Point2.Location[DimIndex]) / 2;
          end;
          if InPolyHedron(IntersectionLocation) <> ipExterior then
          begin
            NewSegment := TSegmentObject.Create;
            NewSegment.StartLoc := Point1.Location;
            NewSegment.EndLoc := Point2.Location;
            result.FList.Add(NewSegment);
          end;
        end;
      end;

    end;

  finally
    if VertexList.IndexOf(StartPoint) < 0 then
    begin
      StartPoint.Free;
    end;
    if VertexList.IndexOf(EndPoint) < 0 then
    begin
      EndPoint.Free;
    end;
    for Index := 0 to VertexList.Count - 1 do
    begin
      TPointObject(VertexList[Index]).Free;
    end;
    VertexList.Free;
    OrderList.Free;
  end;

end;

{ TSegmentList }

function TSegmentList.Add(ASegment: TSegmentObject): integer;
begin
  result := FList.Add(ASegment);
end;

function TSegmentList.Count: integer;
begin
  result := FList.Count;
end;

constructor TSegmentList.Create;
begin
  inherited;
  FList := TList.Create;

end;

destructor TSegmentList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TSegmentList.Getitems(Index: integer): TSegmentObject;
begin
  result := FList[Index];
end;

function TSegmentList.IndexOf(ASegment: TSegmentObject): integer;
begin
  result := FList.IndexOf(ASegment);
end;

procedure TPolyhedron.GetProps(var Vol, Area: extended);
var
  ASolid: TSolid;
  Xs, Ys, Zs: TRealList;
  FaceLists: TIntListList;
  Index: integer;
  AFaceList: TIntegerList;
  VertexIndex: integer;
begin
  Xs := TRealList.Create;
  Ys := TRealList.Create;
  Zs := TRealList.Create;
  FaceLists := TIntListList.Create;
  try
    Xs.Capacity := V;
    Ys.Capacity := V;
    Zs.Capacity := V;
    FaceLists.Capacity := F;
    begin
      for Index := 0 to V - 1 do
      begin
        Xs.Add(Vertices[Index][X]);
        Ys.Add(Vertices[Index][Y]);
        Zs.Add(Vertices[Index][Z]);
      end;
      for Index := 0 to F - 1 do
      begin
        AFaceList := TIntegerList.Create;
        FaceLists.Add(AFaceList);
        AFaceList.Add(Index);
        for VertexIndex := DIM - 1 downto 0 do
        begin
          AFaceList.Add(Faces[Index][VertexIndex]);
        end;
      end;

      ASolid := TSolid.Create(Xs, Ys, Zs, FaceLists);
      try
        ASolid.GetProps(Vol, Area);
        Vol := Abs(Vol);
      finally
        ASolid.Free;
      end;
    end
  finally
    begin
      Xs.Free;
      Ys.Free;
      Zs.Free;
      for Index := 0 to FaceLists.Count - 1 do
      begin
        TIntegerList(FaceLists[Index]).Free;
      end;
      FaceLists.Free;
      // FreeArrays;
    end;
  end;

end;

function WrongEndsMatch(FirstSegment, SecondSegment: TSegmentObject): boolean;
begin
  result := NearlyTheSame(FirstSegment.EndLoc, SecondSegment.EndLoc, 1E-7);

  { (NearlyTheSame(FirstSegment.EndLoc[0],SecondSegment.EndLoc[0],
    Epsilon))
    and (NearlyTheSame(FirstSegment.EndLoc[1], SecondSegment.EndLoc[1],
    Epsilon))
    and (NearlyTheSame(FirstSegment.EndLoc[2], SecondSegment.EndLoc[2],
    Epsilon)); }
end;

function EndsMatch(FirstSegment, SecondSegment: TSegmentObject): boolean;
begin
  result := NearlyTheSame(FirstSegment.EndLoc, SecondSegment.StartLoc, 1E-7);
  { result := (NearlyTheSame(FirstSegment.EndLoc[0], SecondSegment.StartLoc[0],
    Epsilon))
    and (NearlyTheSame(FirstSegment.EndLoc[1], SecondSegment.StartLoc[1],
    Epsilon))
    and (NearlyTheSame(FirstSegment.EndLoc[2], SecondSegment.StartLoc[2],
    Epsilon)); }
end;

function FirstEndsMatch(FirstSegment, SecondSegment: TSegmentObject): boolean;
begin
  result := NearlyTheSame(FirstSegment.StartLoc, SecondSegment.StartLoc, 1E-7);
  { result := (NearlyTheSame(FirstSegment.StartLoc[0], SecondSegment.StartLoc[0],
    Epsilon))
    and (NearlyTheSame(FirstSegment.StartLoc[1], SecondSegment.StartLoc[1],
    Epsilon))
    and (NearlyTheSame(FirstSegment.StartLoc[2], SecondSegment.StartLoc[2],
    Epsilon)); }
end;

function ReversedEndsMatch(FirstSegment, SecondSegment: TSegmentObject)
  : boolean;
begin
  result := NearlyTheSame(FirstSegment.StartLoc, SecondSegment.EndLoc, 1E-7);
  { result := (NearlyTheSame(FirstSegment.StartLoc[0], SecondSegment.EndLoc[0],
    Epsilon))
    and (NearlyTheSame(FirstSegment.StartLoc[1], SecondSegment.EndLoc[1],
    Epsilon))
    and (NearlyTheSame(FirstSegment.StartLoc[2], SecondSegment.EndLoc[2],
    Epsilon)); }
end;

function TPolyhedron.PolygonIntersect(APolygon: TXYZPlane): TXYZPlaneList;
  Function GetNextSegment(AList: TList; ASegment: TSegmentObject)
    : TSegmentObject;
  var
    AnotherSegment: TSegmentObject;
    Index: integer;
    ALocation: TPointd;
  begin
    result := nil;
    for Index := 0 to AList.Count - 1 do
    begin
      AnotherSegment := AList[Index];
      if (ASegment <> AnotherSegment) then
      begin
        if EndsMatch(ASegment, AnotherSegment) then
        begin
          result := AnotherSegment;
          break;
        end
        else if WrongEndsMatch(ASegment, AnotherSegment) then
        begin
          ALocation := AnotherSegment.StartLoc;
          AnotherSegment.StartLoc := AnotherSegment.EndLoc;
          AnotherSegment.EndLoc := ALocation;
          result := AnotherSegment;
          break;
        end
      end;
    end;

  end;
  Function GetPreviousSegment(AList: TList; ASegment: TSegmentObject)
    : TSegmentObject;
  var
    AnotherSegment: TSegmentObject;
    Index: integer;
    ALocation: TPointd;
  begin
    result := nil;
    for Index := 0 to AList.Count - 1 do
    begin
      AnotherSegment := AList[Index];
      if (ASegment <> AnotherSegment) then
      begin
        if ReversedEndsMatch(ASegment, AnotherSegment) then
        begin
          result := AnotherSegment;
          break;
        end
        else if FirstEndsMatch(ASegment, AnotherSegment) then
        begin
          ALocation := AnotherSegment.StartLoc;
          AnotherSegment.StartLoc := AnotherSegment.EndLoc;
          AnotherSegment.EndLoc := ALocation;
          result := AnotherSegment;
          break;
        end
      end;
    end;
  end;

var
  Converter: TPointObject;
  ASegment, AnotherSegment: TSegmentObject;
  SegmentList: TSegmentList;
  AList, AnotherList: TList;
  Index: integer;
  InnerIndex: integer;
  AnotherPolygon: TXYZPlane;
  FacePointer: TPointi;
  VertexIndex, FaceIndex: integer;
  Procedure CreatePolygon;
  var
    Index: integer;
    APlane: TXYZPlane;
    NodeIndex, InnerNodeIndex: integer;
  begin
    if AnotherList.Count > 2 then
    begin
      APlane := TXYZPlane.Create(AnotherList.Count);
      result.Add(APlane);
      for Index := 0 to AnotherList.Count - 1 do
      begin
        ASegment := AnotherList[Index];
        Converter.Location := ASegment.StartLoc;
        APlane.Outline[Index] := Converter.XYZLocation;
      end;
      Converter.Location := ASegment.EndLoc;
      if ((NearlyTheSame(APlane.Outline[0].X, APlane.Outline[APlane.Count - 1]
        .X, Epsilon)) and (NearlyTheSame(APlane.Outline[0].Y,
        APlane.Outline[APlane.Count - 1].Y, Epsilon)) and
        (NearlyTheSame(APlane.Outline[0].Z, APlane.Outline[APlane.Count - 1].Z,
        Epsilon))) then
      begin
        if APlane.Count = 3 then
        begin
          result.Remove(APlane);
          APlane.Free;
          APlane := nil;
        end
        else
        begin
          APlane.Count := APlane.Count - 1;
        end;
        if APlane <> nil then
        begin
          for NodeIndex := APlane.Count - 1 downto 1 do
          begin
            if NearlyTheSame(APlane.Outline[NodeIndex].X,
              APlane.Outline[NodeIndex - 1].X, Epsilon) and
              NearlyTheSame(APlane.Outline[NodeIndex].Y,
              APlane.Outline[NodeIndex - 1].Y, Epsilon) and
              NearlyTheSame(APlane.Outline[NodeIndex].Z,
              APlane.Outline[NodeIndex - 1].Z, Epsilon) then
            begin
              if APlane.Count = 3 then
              begin
                result.Remove(APlane);
                APlane.Free;
              end
              else
              begin
                for InnerNodeIndex := NodeIndex to APlane.Count - 2 do
                begin
                  APlane.Outline[NodeIndex] := APlane.Outline[NodeIndex + 1]
                end;
                APlane.Count := APlane.Count - 1;
              end;
            end;
          end;
        end;
      end;
    end;
    for Index := 0 to AnotherList.Count - 1 do
    begin
      TSegmentObject(AnotherList[Index]).Free;
    end;
    AnotherList.Clear;
    if AList.Count > 0 then
    begin
      ASegment := AList[0];
      AnotherList.Add(ASegment);
      AList.Remove(ASegment);
    end;
  end;

var
  DimIndex: integer;
  TempSegList: TList;
  TempSegIndex: integer;
  SegmentIndex, InnerSegmentIndex: integer;
begin
  result := TXYZPlaneList.Create;
  AList := TList.Create;
  AnotherList := TList.Create;
  Converter := TPointObject.Create;
  try
    for DimIndex := 0 to DIM - 1 do
    begin
      if (APolygon.bmin[DimIndex] > bmax[DimIndex]) or
        (APolygon.bmax[DimIndex] < bmin[DimIndex]) then
      begin
        Exit;
      end;
    end;

    // Get the parts of the outline of the APolygon that are in the
    // polyhedron.
    for Index := 0 to APolygon.Count - 2 do
    begin
      ASegment := TSegmentObject.Create;
      try
        Converter.XYZLocation := APolygon.Outline[Index];
        ASegment.StartLoc := Converter.Location;
        Converter.XYZLocation := APolygon.Outline[Index + 1];
        ASegment.EndLoc := Converter.Location;
        SegmentList := SegmentIntersection(ASegment);
        try
          for InnerIndex := 0 to SegmentList.Count - 1 do
          begin
            AList.Add(SegmentList[InnerIndex]);
          end;
        finally
          SegmentList.Free;
        end;
      finally
        ASegment.Free;
      end;
    end;
    ASegment := TSegmentObject.Create;
    try
      Converter.XYZLocation := APolygon.Outline[APolygon.Count - 1];
      ASegment.StartLoc := Converter.Location;
      Converter.XYZLocation := APolygon.Outline[0];
      ASegment.EndLoc := Converter.Location;
      SegmentList := SegmentIntersection(ASegment);
      try
        for InnerIndex := 0 to SegmentList.Count - 1 do
        begin
          AList.Add(SegmentList[InnerIndex]);
        end;
      finally
        SegmentList.Free;
      end;
    finally
      ASegment.Free;
    end;

    // Get the intersection between each face of the polyhedron and APolygon
    for FaceIndex := 0 to F - 1 do
    begin
      FacePointer := Faces[FaceIndex];
      AnotherPolygon := TXYZPlane.Create(DIM);
      try
        for VertexIndex := 0 to DIM - 1 do
        begin
          Converter.Location := Vertices[FacePointer[VertexIndex]];
          AnotherPolygon.Outline[VertexIndex] := Converter.XYZLocation;
        end;
        TempSegList := TList.Create;
        try
          if (APolygon.PlaneIntersection(AnotherPolygon, TempSegList)
            = piOutlinesCross) then
          begin
            if TempSegList.Count > 0 then
            begin
              AList.Capacity := AList.Count + TempSegList.Count;
              for TempSegIndex := 0 to TempSegList.Count - 1 do
              begin
                ASegment := TempSegList[TempSegIndex];
                AList.Add(ASegment);
              end;
            end;

          end;
        finally
          TempSegList.Free;
        end;

      finally
        AnotherPolygon.Free;
      end;

    end;

    for SegmentIndex := AList.Count - 2 downto 0 do
    begin
      ASegment := AList[SegmentIndex];
      for InnerSegmentIndex := AList.Count - 1 downto SegmentIndex + 1 do
      begin
        AnotherSegment := AList[InnerSegmentIndex];
        if (FirstEndsMatch(ASegment, AnotherSegment) and
          WrongEndsMatch(AnotherSegment, ASegment)) or
          (ReversedEndsMatch(ASegment, AnotherSegment) and
          ReversedEndsMatch(AnotherSegment, ASegment)) or
          ASegment.Contains(AnotherSegment) then
        begin
          AnotherSegment.Free;
          AList.Delete(InnerSegmentIndex);
        end
        else if AnotherSegment.Contains(ASegment) then
        begin
          ASegment.Free;
          AList.Delete(SegmentIndex);
          break;
        end
      end;
    end;

    // arrange the segments.
    if AList.Count > 0 then
    begin
      ASegment := AList[0];
      AnotherList.Add(ASegment);
      AList.Remove(ASegment);
      While AList.Count > 0 do
      begin
        ASegment := GetNextSegment(AList, ASegment);
        if ASegment = nil then
        begin
          ASegment := AnotherList[0];
          While AList.Count > 0 do
          begin
            ASegment := GetPreviousSegment(AList, ASegment);

            if ASegment = nil then
            begin
              CreatePolygon;
            end
            else
            begin
              AnotherList.Insert(0, ASegment);
              AList.Remove(ASegment);
            end;
          end;
        end
        else
        begin
          AnotherList.Add(ASegment);
          AList.Remove(ASegment);
        end;
      end;
    end;
    // create the resulting polygon
    CreatePolygon;
  finally
    Converter.Free;

    for Index := 0 to AList.Count - 1 do
    begin
      TSegmentObject(AList[Index]).Free;
    end;
    AList.Free;

    for Index := 0 to AnotherList.Count - 1 do
    begin
      TSegmentObject(AnotherList[Index]).Free;
    end;
    AnotherList.Free;
  end;

end;

function TPolyhedron.VertexIndex(Avertex: TPointd; Const LastVertex: integer;
  EpsilonX: double = 1E-6; EpsilonY: double = 1E-6;
  EpsilonZ: double = 1E-6): integer;
var
  Index: integer;
  pResult: PInteger;
  AnotherVertex: TPointd;
begin
  // set VI_ARRAY here
  if Length(VI_array) < VertexCount then
  begin
    SetLength(VI_array, VertexCount);
    for Index := 0 to VertexCount - 1 do
    begin
      VI_array[Index] := Index;
    end;
  end;

  Assert(LastVertex < V);
  result := -1;
  if FOctTree.Count = 0 then
  begin
    Exit;
  end;
  pResult := FOctTree.NearestPointsFirstData(Avertex[X], Avertex[Y], Avertex[Z]);
  result := pResult^;
  AnotherVertex := Vertices[result];
  // if not NearlyTheSame(Avertex,AnotherVertex,1e-6) then
  if not NearlyTheSame(Avertex[X], AnotherVertex[X], EpsilonX) or
    not NearlyTheSame(Avertex[Y], AnotherVertex[Y], EpsilonY) or
    not NearlyTheSame(Avertex[Z], AnotherVertex[Z], EpsilonZ) then
  begin
    result := -1;
  end;

  { for Index := 0 to LastVertex do
    begin
    AnotherVertex := Vertices[Index];
    if NearlyTheSame(Avertex,AnotherVertex,1e-6) then
    begin
    result := Index;
    break;
    end;
    end; }
end;

procedure TPolyhedron.SetArrayLengths(LastFace, LastVertex: integer);
begin
  // eliminate parts of the arrays that weren't used.
  if V <> LastVertex + 1 then
  begin
    V := LastVertex + 1;
    SetLength(FVertices, V);
  end;

  if F <> LastFace + 1 then
  begin
    F := LastFace + 1;
    SetLength(FFaces, F);
  end;
end;

procedure TPolyhedron.EliminateInternalFaces(LastFace, LastVertex: integer);
var
  FaceIndex1, FaceIndex2: integer;
  FaceIndex: integer;
  DuplicateFound: boolean;
  NodeIndex: integer;
  NodeUsed: array of boolean;
  // InnerNodeIndex : integer;
  EliminatedNodes: integer;
  NodeIndicies: array of integer;
  APoint: TPointd;
begin
  // eliminate parts of the arrays that weren't used.
  // SetArrayLengths(LastFace, LastVertex);
  { if V <> LastVertex+1 then
    begin
    V := LastVertex+1;
    Setlength(FVertices,V);
    end;

    if F <> LastFace+1 then
    begin
    F := LastFace+1;
    Setlength(FFaces,F);
    end; }
  // Get rid of duplicate faces because they are internal faces.
  repeat
    DuplicateFound := False;
    for FaceIndex1 := F - 2 downto 0 do
    begin
      for FaceIndex2 := F - 1 downto FaceIndex1 + 1 do
      begin
        if FacesSame(FaceIndex1, FaceIndex2) then
        begin
          DuplicateFound := True;
          for FaceIndex := FaceIndex1 + 1 to FaceIndex2 - 1 do
          begin
            Faces[FaceIndex - 1] := Faces[FaceIndex];
          end;
          // Dec(FaceIndex2);
          for FaceIndex := FaceIndex2 to F - 2 do
          begin
            Faces[FaceIndex - 1] := Faces[FaceIndex + 1];
          end;
          Dec(FF, 2);
          SetLength(FFaces, F);
          break;
        end;
      end;
      if DuplicateFound then
      begin
        break;
      end;
    end;
  until not DuplicateFound;

  SetLength(FBox, F);

  // initialize NodeUsed array
  SetLength(NodeUsed, V);
  for NodeIndex := 0 to V - 1 do
  begin
    NodeUsed[NodeIndex] := False;
  end;

  // check which nodes were used.
  for FaceIndex1 := 0 to F - 1 do
  begin
    for FaceIndex2 := 0 to DIM - 1 do
    begin
      NodeUsed[Faces[FaceIndex1][FaceIndex2]] := True;
    end;
  end;

  // eliminate nodes that were not used.
  SetLength(NodeIndicies, V);

  EliminatedNodes := 0;
  for NodeIndex := 0 to V - 1 do
  begin
    if (EliminatedNodes > 0) { and (NodeIndex+EliminatedNodes < V) } then
    begin
      Vertices[NodeIndex - EliminatedNodes] := Vertices[NodeIndex];
    end;
    if not NodeUsed[NodeIndex] then
    begin
      Inc(EliminatedNodes);
    end;
    NodeIndicies[NodeIndex] := NodeIndex - EliminatedNodes;
  end;
  if EliminatedNodes > 0 then
  begin
    V := V - EliminatedNodes;
    SetLength(FVertices, V);
    for FaceIndex1 := 0 to F - 1 do
    begin
      for FaceIndex2 := 0 to DIM - 1 do
      begin
        FaceValue[FaceIndex1, FaceIndex2] :=
          NodeIndicies[Faces[FaceIndex1][FaceIndex2]];
      end;
    end;
    FOctTree.Clear;
    for NodeIndex := 0 to V - 1 do
    begin
      APoint := Vertices[NodeIndex];
      FOctTree.AddPoint(APoint[X], APoint[Y], APoint[Z], @VI_array[NodeIndex]);
    end;

  end;

end;

function TPolyhedron.FacesSame(FaceIndex1, FaceIndex2: integer): boolean;
var
  Face1, Face2: TPointi;
begin
  Face1 := Faces[FaceIndex1];
  Face2 := Faces[FaceIndex2];
  result := False;
  if Face1[X] = Face2[X] then
  begin
    if Face1[Y] = Face2[Y] then
    begin
      result := (Face1[Z] = Face2[Z]);
    end
    else if Face1[Y] = Face2[Z] then
    begin
      result := (Face1[Z] = Face2[Y]);
    end;
  end
  else if Face1[X] = Face2[Y] then
  begin
    if Face1[Y] = Face2[X] then
    begin
      result := (Face1[Z] = Face2[Z]);
    end
    else if Face1[Y] = Face2[Z] then
    begin
      result := (Face1[Z] = Face2[X]);
    end;
  end
  else if Face1[X] = Face2[Z] then
  begin
    if Face1[Y] = Face2[X] then
    begin
      result := (Face1[Z] = Face2[Y]);
    end
    else if Face1[Y] = Face2[Y] then
    begin
      result := (Face1[Z] = Face2[X]);
    end;
  end
end;

{ TSegmentObject }

function TSegmentObject.Colinear(const ASegment: TSegmentObject): boolean;
var
  // L1, L2 : extended;
  // TempSegment : TSegmentObject;
  // Index : integer;
  // TempDirection : TPointD;
  APlane: TXYZPlane;
  Converter: TPointObject;

begin
  APlane := TXYZPlane.Create(3);
  try
    Converter := TPointObject.Create;
    try
      Converter.Location := StartLoc;
      APlane.Outline[0] := Converter.XYZLocation;

      Converter.Location := EndLoc;
      APlane.Outline[1] := Converter.XYZLocation;

      Converter.Location := ASegment.StartLoc;
      APlane.Outline[2] := Converter.XYZLocation;

      result := NearlyTheSame(0, APlane.Area, Epsilon);
      if not result then
        Exit;

      Converter.Location := ASegment.EndLoc;
      APlane.Outline[2] := Converter.XYZLocation;

      result := NearlyTheSame(0, APlane.Area, Epsilon);
      // if not result then Exit;
    finally
      Converter.Free;
    end;

  finally
    APlane.Free;
  end;

  {
    L1 := Length;
    L2 := ASegment.Length;
    if L1 < L2 then
    begin
    result := ASegment.Colinear(self);
    end
    else if (L1 = 0) and (L2 = 0) then
    begin
    result := True;
    end
    else if (L1 = 0) or (L2 = 0) then
    begin
    result := False;
    end
    else
    begin
    result := Parallel(ASegment);
    if result then
    begin
    TempSegment := TSegmentObject.Create;
    try
    for Index := 0 to Dim-1 do
    begin
    TempSegment.StartLoc[Index] := StartLoc[Index];
    TempSegment.EndLoc[Index] := ASegment.StartLoc[Index];
    end;
    TempDirection := TempSegment.Direction;
    finally
    TempSegment.Free;
    end;
    result := NearlyTheSame(Direction, TempDirection, Epsilon);
    if not result then
    begin
    for Index := 0 to Dim-1 do
    begin
    TempDirection[Index] := - TempDirection[Index]
    end;
    result := NearlyTheSame(Direction, TempDirection, Epsilon);
    end;
    end;
    end; }
end;

function TSegmentObject.Contains(const ASegment: TSegmentObject): boolean;
var
  Higher, Lower: TPointd;
  Index: integer;
begin
  result := Colinear(ASegment);
  if result then
  begin
    for Index := 0 to DIM - 1 do
    begin
      Higher[Index] := Max(StartLoc[Index], EndLoc[Index]);
      Lower[Index] := Min(StartLoc[Index], EndLoc[Index]);
    end;
    for Index := 0 to DIM - 1 do
    begin
      result := ((Higher[Index] >= ASegment.StartLoc[Index]) or
        NearlyTheSame(Higher[Index], ASegment.StartLoc[Index], Epsilon)) and
        ((Higher[Index] >= ASegment.EndLoc[Index]) or
        NearlyTheSame(Higher[Index], ASegment.EndLoc[Index], Epsilon)) and
        ((Lower[Index] <= ASegment.StartLoc[Index]) or
        NearlyTheSame(Lower[Index], ASegment.StartLoc[Index], Epsilon)) and
        ((Lower[Index] <= ASegment.EndLoc[Index]) or NearlyTheSame(Lower[Index],
        ASegment.EndLoc[Index], Epsilon));
      if not result then
        Exit;
    end;

  end;
end;

function TSegmentObject.Direction: TPointd;
var
  Index: integer;
begin
  for Index := 0 to DIM - 1 do
  begin
    result[Index] := EndLoc[Index] - StartLoc[Index];
  end;
end;

function TSegmentObject.Length: extended;
begin
  result := sqrt(Sqr(StartLoc[X] - EndLoc[X]) + Sqr(StartLoc[Y] - EndLoc[Y]) +
    Sqr(StartLoc[Z] - EndLoc[Z]));
end;

procedure SetEpsilon;
begin
  { Epsilon := 1;
    while 1 + Epsilon/2 > 1 do
    begin
    Epsilon := Epsilon/2;
    end;
    Epsilon := Epsilon*1000000000; }
  Epsilon := 1E-7
end;

function TSegmentObject.Parallel(const ASegment: TSegmentObject): boolean;
var
  Dir1, Dir2: TPointd;
  DotProduct: extended;
  Index: integer;
begin
  Dir1 := Direction;
  Dir2 := ASegment.Direction;
  DotProduct := 0;
  for Index := 0 to DIM - 1 do
  begin
    DotProduct := DotProduct + Dir1[Index] * Dir2[Index];
  end;
  result := NearlyTheSame(Abs(DotProduct), Length * ASegment.Length, Epsilon);
end;

function TSegmentObject.TwoDIntersection(ASegment: TSegmentObject;
  var Intersection: TPointd): boolean;
var
  Lower1, Higher1, Lower2, Higher2: extended;
  DimIndex: integer;
  num, denom: double;
  s, T: double;
  a, b, c, D: TPointd;
begin
  // result := True;
  for DimIndex := 0 to 1 do
  begin
    if StartLoc[DimIndex] < EndLoc[DimIndex] then
    begin
      Lower1 := StartLoc[DimIndex];
      Higher1 := EndLoc[DimIndex];
    end
    else
    begin
      Higher1 := StartLoc[DimIndex];
      Lower1 := EndLoc[DimIndex];
    end;
    if ASegment.StartLoc[DimIndex] < ASegment.EndLoc[DimIndex] then
    begin
      Lower2 := ASegment.StartLoc[DimIndex];
      Higher2 := ASegment.EndLoc[DimIndex];
    end
    else
    begin
      Higher2 := ASegment.StartLoc[DimIndex];
      Lower2 := ASegment.EndLoc[DimIndex];
    end;
    result := (Higher1 >= Lower2 - Epsilon) and (Higher2 >= Lower1 - Epsilon);
    if not result then
    begin
      Exit;
    end;
  end;
  if result then
  begin
    a := StartLoc;
    b := EndLoc;
    c := ASegment.StartLoc;
    D := ASegment.EndLoc;
    denom := a[X] * (D[Y] - c[Y]) + b[X] * (c[Y] - D[Y]) + D[X] * (b[Y] - a[Y])
      + c[X] * (a[Y] - b[Y]);
    result := (denom <> 0);
    if not result then
    begin
      Exit;
    end;

    num := a[X] * (D[Y] - c[Y]) + c[X] * (a[Y] - D[Y]) + D[X] * (c[Y] - a[Y]);
    s := num / denom;

    num := -(a[X] * (c[Y] - b[Y]) + b[X] * (a[Y] - c[Y]) + c[X] *
      (b[Y] - a[Y]));
    T := num / denom;

    Intersection[X] := a[X] + s * (b[X] - a[X]);
    Intersection[Y] := a[Y] + s * (b[Y] - a[Y]);
    Intersection[Z] := a[Z] + s * (b[Z] - a[Z]);

    result := (0 <= s) and (s <= 1) and (0 <= T) and (T <= 1);

  end;
end;

function TPolyhedron.VertexIndex(Avertex: TPointd): integer;
begin
  result := VertexIndex(Avertex, V - 1);
end;

function TPolyhedron.GetBox(const BIndex: integer): TPointdArray;
begin
  // if Length(FBox) = 0 then
  // begin
  // ReadBoxesFromFile;
  // end;
  result := FBox[BIndex];
end;

function TPolyhedron.GetFace(const FIndex: integer): TPointi;
begin
  // if Length(FFaces) = 0 then
  // begin
  // ReadFacesFromFile;
  // end;
  result := FFaces[FIndex];
end;

function TPolyhedron.GetVertex(const VIndex: integer): TPointd;
begin
  // if Length(FVertices) = 0 then
  // begin
  // ReadVerticesFromFile;
  // end;
  result := FVertices[VIndex];
end;

procedure TPolyhedron.SetBox(const BIndex: integer; const Value: TPointdArray);
begin
  FBox[BIndex] := Value;
end;

procedure TPolyhedron.SetFace(const FIndex: integer; const Value: TPointi);
begin
  FFaces[FIndex] := Value;
end;

procedure TPolyhedron.SetVertexValue(const VIndex, DimIndex: integer;
  const Value: extended);
begin
  FVertices[VIndex, DimIndex] := Value;
end;

procedure TPolyhedron.SetFaceValue(const FIndex, DimIndex, Value: integer);
begin
  FFaces[FIndex, DimIndex] := Value;
end;

procedure TPolyhedron.SetVertex(const VIndex: integer; const Value: TPointd);
begin
  FVertices[VIndex] := Value;
end;

procedure TPolyhedron.SetBoxPointd(const BIndex, CornerIndex: integer;
  const Value: TPointd);
begin
  FBox[BIndex, CornerIndex] := Value;
end;

procedure TPolyhedron.SetBoxValue(const BIndex, CornerIndex, DimIndex: integer;
  const Value: extended);
begin
  FBox[BIndex, CornerIndex, DimIndex] := Value;
end;

procedure TPolyhedron.FreeArrays;
begin
  F := 0;
  V := 0;
  SetLength(FVertices, 0);
  SetLength(FFaces, 0);
  SetLength(FBox, 0);
end;

function TPolyhedron.GetF: integer;
begin
  // if FF = 0 then ReadSizes;
  result := FF;
end;

function TPolyhedron.GetV: integer;
begin
  // if FV = 0 then ReadSizes;
  result := FV;
end;

Initialization

SetEpsilon;

Finalization

end.
