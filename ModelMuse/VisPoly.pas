{
  By Richard B. Winston, rbwinst@usgs.gov
  This unit is in the public domain.
}
unit VisPoly;

interface

uses
  FastGEO, Generics.Collections;

{
  @param(BoundaryPolygon is a simple (non self-intersecting) polygon.
  The first and last points in BoundaryPolygon are different from one another.
  An edge of the polygon connects each adjacent point in BoundaryPolygon and
  also the first and last points in BoundaryPolygon.)

  @param(ViewPointIndex is the index of one of the points in BoundaryPolygon for
  which the visibility polygon will be computed.)

  The result of @name is the visibility polygon for the point designated
  by ViewPointIndex. The point designated by ViewPointIndex will be the
  first point in the result.

  @param(Indices will contain the indices of the points in BoundaryPolygon that
  correspond to each point in the result. For points in the result that are
  not points of BoundaryPolygon, Indices will have a value of -1.)

  Based on Joe, B, and Simpson, R.B., 1987. Algorithms and Correctness
  Proofs for Visibility Polygon Computations. Department of Computer Science,
  Research Report CS-87-03.
  https://cs.uwaterloo.ca/research/tr/1985/CS-85-38.pdf

  See also:  Joe, Barry; Simpson, R. B. (1987). "Corrections to Lee's
  visibility polygon algorithm". BIT Numerical Mathematics 27 (4): 458–473.
  doi:10.1007/BF01937271
}
function VisibilityPolygon(const BoundaryPolygon: TPolygon2D; ViewPointIndex: integer;
  out Indices: TArray<Integer>):
  TPolygon2D;

implementation

uses
  Math;

type
  TRay2D = array [1..2] of TPoint2D;

  TVisStack<T> = class(TObject)
  private
    FList: TList<T>;
    function GetCapacity: Integer;
    function GetCount: Integer;
    procedure SetCapacity(const Value: Integer);
    function GetItem(index: Integer): T;
  public
    function NextToLastItem: T;
    procedure Push(AnItem: T);
    procedure Pop;
    function Peek: T;
    constructor Create;
    destructor Destroy; override;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property Items[index: Integer]: T read GetItem; default;
  end;

  TPointStack = TVisStack<TPoint2D>;
  TIntegerStack = TVisStack<integer>;

  TPointList = TList<TPoint2D>;
  TIntegerList = TList<Integer>;

  TNextOp = (noUnknown, noLeft, noRight, noScanA, noScanB, noScanc, noScanD, noFinish);

var
  Epsilon: double;

function IsEqual(const Val1,Val2:TFloat):Boolean;
begin
  Result := FastGEO.IsEqual(Val1,Val2,Epsilon);
end;

function RobustOrientation(const x1,y1,x2,y2,Px,Py:TFloat):Integer;
var
  Orin : TFloat;
begin
  (* Linear determinant of the 3 points *)
  Orin := (x2 - x1) * (py - y1) - (px - x1) * (y2 - y1);

  (*
    Calculation Policy:
    if |Orin - Orin`| < Epsilon then Orin` is assumed to be equal to zero.
    Where:
     Orin : is the "real" mathematically precise orientation value, using infinite
            precision arithmetic (hypothetical)
     Orin`: is the calculated imprecise orientation value, using finite precision arithmetic
  *)

  if IsEqual(Orin,Zero) then
    Result := 0                (* Orientaion is neutral aka collinear  *)
  else if Orin < Zero then
    Result := RightHandSide    (* Orientaion is to the right-hand side *)
  else
    Result := LeftHandSide;    (* Orientaion is to the left-hand side  *)
end;


function Orientation(const Point1,Point2,Point3:TPoint2D):Integer;
begin
  Result := RobustOrientation(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y);
end;

function Intersect(Ray: TRay2D; Segment: TSegment2D;
  var IntersectionLocation: TPoint2D): boolean; overload
var
  MinValue: TFloat;
  MaxValue: TFloat;
//  P1: TPoint2D;
//  P2: TPoint2D;
//  P1Distance: TFloat;
//  P2Distance: TFloat;
//  P1OK: Boolean;
//  P2OK: Boolean;
  function TestPoint: boolean;
  begin
      if Segment[1].x > Segment[2].x then
      begin
        MinValue := Segment[2].x;
        MaxValue := Segment[1].x;
      end
      else
      begin
        MinValue := Segment[1].x;
        MaxValue := Segment[2].x;
      end;

      Result := (MaxValue >= IntersectionLocation.x-Epsilon)
        and (IntersectionLocation.x+Epsilon >= MinValue);
      if result then
      begin
        if Segment[1].y > Segment[2].y then
        begin
          MinValue := Segment[2].y;
          MaxValue := Segment[1].y;
        end
        else
        begin
          MinValue := Segment[1].y;
          MaxValue := Segment[2].y;
        end;
        Result := (MaxValue >= IntersectionLocation.y-Epsilon)
          and (IntersectionLocation.y+Epsilon >= MinValue);
      end;
      if result then
      begin
        if Ray[1].x <> Ray[2].x then
        begin
          if Ray[2].x > Ray[1].x then
          begin
            result := IntersectionLocation.x >= Ray[1].x-Epsilon;
          end
          else
          begin
            result := IntersectionLocation.x <= Ray[1].x+Epsilon;
          end;
        end
        else
        begin
          if Ray[2].y > Ray[1].y then
          begin
            result := IntersectionLocation.y >= Ray[1].y-Epsilon;
          end
          else
          begin
            result := IntersectionLocation.y <= Ray[1].y+Epsilon;
          end;
        end;
      end;
  end;
begin
  if Collinear(Ray[1], Segment[1], Segment[2])
    and Collinear(Ray[2], Segment[1], Segment[2]) then
  begin
    if Ray[2].x <> Ray[1].x then
    begin
      Result := (Ray[2].x > Ray[1].x) =
        ((Segment[1].x >= Ray[1].x) or (Segment[2].x >= Ray[1].x));
      if Result then
      begin
        if (Ray[2].x > Ray[1].x) = (Segment[1].x >= Ray[1].x) then
        begin
          IntersectionLocation := Segment[1];
        end
        else
        begin
          IntersectionLocation := Segment[2];
        end;
      end;
    end
    else if Ray[2].y <> Ray[1].y then
    begin
      Result := (Ray[2].y > Ray[1].y) =
        ((Segment[1].y >= Ray[1].y) or (Segment[2].y >= Ray[1].y));
      if Result then
      begin
        if (Ray[2].y > Ray[1].y) = (Segment[1].y >= Ray[1].y) then
        begin
          IntersectionLocation := Segment[1];
        end
        else
        begin
          IntersectionLocation := Segment[2];
        end;
      end;
    end
    else
    begin
      result := ((Ray[1].x = Segment[1].x) and (Ray[1].y = Segment[1].y))
        or ((Ray[1].x = Segment[2].x) and (Ray[1].y = Segment[2].y));
      IntersectionLocation := Ray[1];
    end;
  end
  else
  begin
    if Orientation(Ray[1], Ray[2], Segment[1]) = CollinearOrientation then
    begin
      result := True;
      IntersectionLocation := Segment[1];
    end
    else if Orientation(Ray[1], Ray[2], Segment[2]) = CollinearOrientation then
    begin
      result := True;
      IntersectionLocation := Segment[2];
    end
    else
    begin
      result := (CartesianAngle(Ray[1].x - Ray[2].x, Ray[1].y - Ray[2].y)
        <> CartesianAngle(Segment[1].x - Segment[2].x, Segment[1].y - Segment[2].y));
      if result then
      begin
        IntersectionLocation := IntersectionPoint(EquateLine(Ray[1], Ray[2]),
          EquateLine(Segment[1], Segment[2]));
      end;
    end;
    if result then
    begin
      result := TestPoint;
//      if not result then
//      begin
//        P1 := ClosestPointOnLineFromPoint(EquateLine(Ray[1], Ray[2]), Segment[1]);
//        P2 := ClosestPointOnLineFromPoint(EquateLine(Ray[1], Ray[2]), Segment[2]);
//        P1Distance := Distance(P1, Segment[1]);
//        P2Distance := Distance(P2, Segment[2]);
//        P1OK := P1Distance < Epsilon;
//        P2OK := P2Distance < Epsilon;
//        if P1OK then
//        begin
//          IntersectionLocation := P1;
//          P1OK := TestPoint;
//        end;
//        if P2OK then
//        begin
//          IntersectionLocation := P2;
//          P2OK := TestPoint;
//        end;
//        result := P1OK or P2OK;
//        if P1OK then
//        begin
//          IntersectionLocation := P1;
//          if P2OK and (P2Distance < P1Distance) then
//          begin
//            IntersectionLocation := P2;
//          end;
//        end;
//      end;
//      if Segment[1].x > Segment[2].x then
//      begin
//        MinValue := Segment[2].x;
//        MaxValue := Segment[1].x;
//      end
//      else
//      begin
//        MinValue := Segment[1].x;
//        MaxValue := Segment[2].x;
//      end;
//
//      Result := (MaxValue >= IntersectionLocation.x-Epsilon)
//        and (IntersectionLocation.x+Epsilon >= MinValue);
//      if result then
//      begin
//        if Segment[1].y > Segment[2].y then
//        begin
//          MinValue := Segment[2].y;
//          MaxValue := Segment[1].y;
//        end
//        else
//        begin
//          MinValue := Segment[1].y;
//          MaxValue := Segment[2].y;
//        end;
//        Result := (MaxValue >= IntersectionLocation.y-Epsilon)
//          and (IntersectionLocation.y+Epsilon >= MinValue);
//      end;
//      if result then
//      begin
//        if Ray[1].x <> Ray[2].x then
//        begin
//          if Ray[2].x > Ray[1].x then
//          begin
//            result := IntersectionLocation.x >= Ray[1].x-Epsilon;
//          end
//          else
//          begin
//            result := IntersectionLocation.x <= Ray[1].x+Epsilon;
//          end;
//        end
//        else
//        begin
//          if Ray[2].y > Ray[1].y then
//          begin
//            result := IntersectionLocation.y >= Ray[1].y-Epsilon;
//          end
//          else
//          begin
//            result := IntersectionLocation.y <= Ray[1].y+Epsilon;
//          end;
//        end;
//      end;
    end;
  end;
end;

procedure Left(const Points: TPointList; var proc: TNextOp;
  var i, t: Integer; S: TPointStack; var w: TPoint2D; const z: TPoint2D;
  IndexList: TIntegerList; IndexStack: TIntegerStack);
var
  TurnDirection: Integer;
  Vi: TPoint2D;
  ViPlus1: TPoint2D;
  Sj: TPoint2D;
begin
  if i = Points.Count-1 then
  begin
    proc := noFinish;
    w := Points.Last;
  end
  else
  begin
    Vi := Points[i];
    ViPlus1 := Points[i+1];
    TurnDirection := Orientation(z, Vi, ViPlus1);
    if TurnDirection = CollinearOrientation then
    begin
      Sj := S.Peek;
      if (Sj.x <> Vi.x) or (Sj.y <> Vi.y) then
      begin
        S.push(Vi);
        IndexStack.Push(IndexList[i]);
        Inc(t);
      end;
    end;
    if (TurnDirection = LeftHandSide) or (TurnDirection = CollinearOrientation) then
    begin
      proc := noLeft;
      S.Push(ViPlus1);
      IndexStack.Push(IndexList[i+1]);
      Inc(i);
      Inc(t);
      w := ViPlus1;
    end
    else
    begin
      TurnDirection := Orientation(S.NextToLastItem, Vi, ViPlus1);
      if TurnDirection = RightHandSide then
      begin
        proc := noScanA;
        Inc(i);
        w := ViPlus1;
      end
      else
      begin
        proc := noRight;
        w := Vi;
        Inc(i);
      end;
    end;
  end;
end;

type
  TRightType = (rtRA, rtRB, rtRC);

procedure Right(const Points: TPointList; var proc: TNextOp;
  var i, t: Integer; S: TPointStack; var w: TPoint2D; const z: TPoint2D;
  IndexList: TIntegerList; IndexStack: TIntegerStack);
var
  PriorSeg: TSegment2D;
  Vi: TPoint2D;
  SJ: TPoint2D;
  RightExit: TRightType;
  SjMinus1: TPoint2D;
  ViPlus1: TPoint2D;
  ViMinus1: TPoint2D;
  u: TPoint2D;
  Ray: TRay2D;
  SjOrientation: Integer;
  ViMinus1Orientation: Integer;
begin
  Vi := Points[i];
  ViMinus1 := Points[i-1];
  PriorSeg[1] := ViMinus1;
  PriorSeg[2] := Vi;

  while True do
  begin
    Sj := S.Peek;
    SjMinus1 := S.NextToLastItem;
    SjOrientation := Orientation(z,Sj,Vi);
    ViMinus1Orientation := Orientation(z,Sj,ViMinus1);
    if (Vi.x = z.x) and (Vi.y = z.y)
      and ((ViMinus1Orientation = RightHandSide)
      or (ViMinus1Orientation = CollinearOrientation))
      and (Orientation(z,SjMinus1,ViMinus1) = LeftHandSide)  then
    begin
      RightExit := rtRC;
      break;
    end
    else if ((SjOrientation = RightHandSide) or (SjOrientation = CollinearOrientation))
      and (Orientation(z,SjMinus1,Vi) = LeftHandSide) then
    begin
      RightExit := rtRA;
      break;
    end
    else if (Orientation(z,SjMinus1,Sj) = CollinearOrientation)
      and (Abs(Distance(z,SjMinus1) + Distance(SjMinus1,Sj) - Distance(z,SjMinus1))
       < epsilon) then
//    else if Intersect(PriorSeg, EquateSegment(Sj,SjMinus1)) then
    begin
      u := IntersectionPoint(PriorSeg, EquateSegment(Sj,SjMinus1));
//      if Distance(z, SjMinus1) < Distance(z, Sj) then
      begin
        RightExit := rtRB;
        break;
      end;
    end;
    S.Pop;
    IndexStack.Pop;
    Dec(t);
  end;

  ViPlus1 := Points[i+1];
  if RightExit in [rtRA,rtRc] then
  begin
    Ray[1] := z;
//    Ray[2] := Vi;

    if RightExit = rtRA then
    begin
      Ray[2] := Vi;
    end
    else
    begin
      Ray[2] := ViMinus1;
    end;

    Assert(Intersect(Ray, EquateSegment(SJ, SjMinus1), u));
    S.Pop;
    S.Push(u);
    if (u.x <> SJ.x) or (u.y <> SJ.y) then
    begin
      IndexStack.Pop;
      IndexStack.Push(-1);
    end;

    if (Orientation(z,Vi,ViPlus1) = RightHandSide) then
    begin
      proc := noRight;
      Inc(i);
      t := S.Count-1;
      w := Vi;
    end
    else
    begin
      if Orientation(ViMinus1,Vi,ViPlus1) = RightHandSide then
      begin
        proc := noLeft;
        S.Push(Vi);
        S.Push(ViPlus1);
        IndexStack.Push(IndexList[i]);
        IndexStack.Push(IndexList[i+1]);
        Inc(i);
        t := S.Count-1;
        w := ViPlus1;
      end
      else
      begin
        proc := noScanc;
        Inc(i);
        t := S.Count-1;
        w := Vi;
      end;
    end;
  end
  else
  begin
//    u := IntersectionPoint(PriorSeg, EquateSegment(Sj,SjMinus1));
    S.Pop;
    IndexStack.Pop;
    proc := noScanD;
    t := S.Count-1;
    w := u;
  end;
end;

procedure ScanA(const Points: TPointList; var proc: TNextOp;
  var i, t: Integer; S: TPointStack; var w: TPoint2D; const z: TPoint2D;
  IndexList: TIntegerList; IndexStack: TIntegerStack);
var
  IntersectRay: TRay2D;
  LastDistance: TFloat;
  Vi: TPoint2D;
  ViPlus1: TPoint2D;
  u: TPoint2D;
  TurnDirection: Integer;
  IntersectDist: TFloat;
begin
  IntersectRay[1] := z;
  IntersectRay[2] := S.Peek;
  LastDistance := Distance(Z, S.Peek);
  while True do
  begin
    Vi := Points[i];
    ViPlus1 := Points[i+1];
    if Intersect(IntersectRay, EquateSegment(Vi, ViPlus1), u) then
    begin
      TurnDirection := Orientation(z,Vi,ViPlus1);
      IntersectDist := Distance(Z, u);
      if TurnDirection = RightHandSide then
      begin
        if IntersectDist < LastDistance then
        begin
          // A1
          proc := noRight;
          w := u;
          Inc(i);
        end
        else
        begin
          // A2
          proc := noScanD;
          w := u;
          Inc(i);
        end;
      end
      else
      begin
        if IntersectDist > LastDistance then
        begin
          // A3
          proc := noLeft;
          S.Push(u);
          S.Push(ViPlus1);
          IndexStack.Push(-1);
          IndexStack.Push(IndexList[i+1]);
          Inc(i);
          t := S.Count-1;
          w := ViPlus1;
        end
        else
        begin
          // A4
          Assert(False);
        end;
      end;
      Exit;
    end;
    Inc(i);
  end;
end;

procedure ScanB(const Points: TPointList; var proc: TNextOp;
  var i, t: Integer; S: TPointStack; var w: TPoint2D; const z: TPoint2D;
  IndexList: TIntegerList; IndexStack: TIntegerStack);
var
  IntersectSeg: TSegment2D;
  Vi: TPoint2D;
  ViPlus1: TPoint2D;
  u: TPoint2D;
  Intersects: Boolean;
begin
  IntersectSeg[1] := S.Peek;
  IntersectSeg[2] := Points.Last;
  while True do
  begin
    Vi := Points[i];
    ViPlus1 := Points[i+1];
    if Intersect(IntersectSeg, EquateSegment(Vi, ViPlus1)) then
    begin
      u := IntersectionPoint(IntersectSeg, EquateSegment(Vi,ViPlus1));
      Intersects := True;
    end
    else
    begin
      u := IntersectionPoint(IntersectSeg, EquateSegment(Vi,ViPlus1));
      Intersects := (Abs(u.x-ViPlus1.x) < Epsilon)
        and (Abs(u.y-ViPlus1.y) < Epsilon);
      if Intersects then
      begin
        Intersects := True;
      end;
    end;
    if Intersects then
    begin
      if i+1 = Points.Count-1 then
      begin
        proc := noFinish;
        S.Push(ViPlus1);
        IndexStack.Push(IndexList[I+1]);
        t := S.Count-1;
        w := ViPlus1;
        Inc(i);
      end
      else
      begin
        proc := noRight;
        Inc(i);
        w := u;
      end;
      Exit;
    end;
    Inc(i);
  end;
end;

procedure ScanC(const Points: TPointList; var proc: TNextOp;
  var i, t: Integer; S: TPointStack; var w: TPoint2D; const z: TPoint2D;
  IndexList: TIntegerList; IndexStack: TIntegerStack);
var
  IntersectSeg: TSegment2D;
  Vi: TPoint2D;
  ViPlus1: TPoint2D;
  u: TPoint2D;
  MaxX: double;
  MinX: double;
  MaxY: double;
  MinY: double;
begin
  IntersectSeg[1] := S.Peek;
  IntersectSeg[2] := w;
  MaxX := Max(IntersectSeg[1].x, IntersectSeg[2].x);
  MinX := Max(IntersectSeg[1].x, IntersectSeg[2].x);
  MaxY := Max(IntersectSeg[1].y, IntersectSeg[2].y);
  MinY := Max(IntersectSeg[1].y, IntersectSeg[2].y);
  while True do
  begin
    Vi := Points[i];
    ViPlus1 := Points[i+1];
    if Intersect(IntersectSeg, EquateSegment(Vi, ViPlus1)) then
    begin
      u := IntersectionPoint(IntersectSeg, EquateSegment(Vi,ViPlus1));
      t := S.Count-1;
      Inc(i);
      proc := noRight;
      w := u;
      Exit;
    end
    else if Collinear(IntersectSeg[1], IntersectSeg[2], Vi)
      and (MaxX >= Vi.x) and (Vi.x >= MinX)
      and (MaxY >= Vi.y) and (Vi.y >= MinY) then
    begin
      u := Vi;
      t := S.Count-1;
      Inc(i);
      proc := noRight;
      w := u;
      Exit;
    end
    else if Collinear(IntersectSeg[1], IntersectSeg[2], ViPlus1) then
    begin
      u := ViPlus1;
      t := S.Count-1;
      Inc(i);
      proc := noRight;
      w := u;
      Exit;
    end;
    Inc(i);
  end;
end;

procedure ScanD(const Points: TPointList; var proc: TNextOp;
  var i, t: Integer; S: TPointStack; var w: TPoint2D; const z: TPoint2D;
  IndexList: TIntegerList; IndexStack: TIntegerStack);
var
  IntersectSeg: TSegment2D;
  Vi: TPoint2D;
  ViPlus1: TPoint2D;
  u: TPoint2D;
begin
  IntersectSeg[1] := S.Peek;
  IntersectSeg[2] := w;
  while True do
  begin
    Vi := Points[i];
    ViPlus1 := Points[i+1];
    if Intersect(IntersectSeg, EquateSegment(Vi, ViPlus1)) then
    begin
      u := IntersectionPoint(IntersectSeg, EquateSegment(Vi,ViPlus1));
      proc := noLeft;
      S.Push(u);
      S.Push(ViPlus1);
      IndexStack.Push(-1);
      IndexStack.Push(IndexList[i+1]);
      Inc(i);
      t := S.Count-1;
      w := ViPlus1;
      Exit;
    end;
    Inc(i)
  end;
end;

function VisibilityPolygon(const BoundaryPolygon: TPolygon2D; ViewPointIndex: integer;
  out Indices: TArray<Integer>): TPolygon2D;
var
  PointStack: TPointStack;
  FirstOrientation: integer;
  Proc: TNextOp;
  PointList: TPointList;
  PointIndex: Integer;
  i, t: Integer;
  w: TPoint2D;
  z: TPoint2D;
  Index: Integer;
  PolyIndex: Integer;
  LastRay: TRay2D;
  u: TPoint2D;
  Sj: TPoint2D;
  SjMinus1: TPoint2D;
  MinX, MaxX, MinY, MaxY: double;
//  Epsilon: double;
  IndexList: TIntegerList;
  IndexStack: TIntegerStack;
  VertexAngleSj: TFloat;
  VertexAngleSjMinus1: TFloat;
begin
  Assert(Length(BoundaryPolygon) >= 3);
  PointStack := TPointStack.Create;
  IndexStack := TIntegerStack.Create;
  PointList := TPointList.Create;
  IndexList := TIntegerList.Create;
  try
    MinX := BoundaryPolygon[0].x;
    MinY := BoundaryPolygon[0].Y;
    MaxX := MinX;
    MaxY := MinY;
    for PointIndex := 1 to Length(BoundaryPolygon) - 1 do
    begin
      if BoundaryPolygon[PointIndex].x < MinX then
      begin
        MinX := BoundaryPolygon[PointIndex].x
      end
      else if BoundaryPolygon[PointIndex].x > MaxX then
      begin
        MaxX := BoundaryPolygon[PointIndex].x
      end;
      if BoundaryPolygon[PointIndex].y < MinY then
      begin
        MinY := BoundaryPolygon[PointIndex].y
      end
      else if BoundaryPolygon[PointIndex].y > MaxY then
      begin
        MaxY := BoundaryPolygon[PointIndex].y
      end;
    end;
    Epsilon := Min((MaxX-MinX),(MaxY-MinY))/1e6;

    PointList.Capacity := Length(BoundaryPolygon)+1;
    IndexList.Capacity := PointList.Capacity;
    for PointIndex := ViewPointIndex+1 to Length(BoundaryPolygon) - 1 do
    begin
      PointList.Add(BoundaryPolygon[PointIndex]);
      IndexList.Add(PointIndex);
    end;
    for PointIndex := 0 to ViewPointIndex do
    begin
      PointList.Add(BoundaryPolygon[PointIndex]);
      IndexList.Add(PointIndex);
    end;
    z := PointList.Last;
    PointList.Add(PointList.First);
    IndexList.Add(IndexList.First);

    LastRay[1] := z;
    LastRay[2] := PointList.Last;

    Proc := noUnknown;
    i := 1;
    w := PointList[0];
    PointStack.Push(PointList[0]);
    IndexStack.Push(IndexList[0]);
    FirstOrientation := Orientation(z, PointList[0], PointList[1]);
    if (FirstOrientation = LeftHandSide) or (FirstOrientation = CollinearOrientation) then
    begin
      t:= 1;
      PointStack.Push(PointList[1]);
      IndexStack.Push(IndexList[1]);
      Left(PointList, Proc, i, t, PointStack, w, z, IndexList, IndexStack);
    end
    else
    begin
      t:= 0;
      ScanA(PointList, Proc, i, t, PointStack, w, z, IndexList, IndexStack);
    end;

    repeat
      case Proc of
        noUnknown:
          begin
            Assert(False);
          end;
        noLeft:
          begin
            Left(PointList, Proc, i, t, PointStack, w, z, IndexList, IndexStack);
          end;
        noRight:
          begin
            Right(PointList, Proc, i, t, PointStack, w, z, IndexList, IndexStack);
          end;
        noScanA:
          begin
            ScanA(PointList, Proc, i, t, PointStack, w, z, IndexList, IndexStack);
          end;
        noScanB:
          begin
            ScanB(PointList, Proc, i, t, PointStack, w, z, IndexList, IndexStack);
          end;
        noScanc:
          begin
            ScanC(PointList, Proc, i, t, PointStack, w, z, IndexList, IndexStack);
          end;
        noScanD:
          begin
            ScanD(PointList, Proc, i, t, PointStack, w, z, IndexList, IndexStack);
          end;
        noFinish:
          begin
            // do nothing;
          end;
      end;

      if (Proc = noLeft) then
      begin
        Sj := PointStack.Peek;
        SjMinus1 := PointStack.NextToLastItem;
        VertexAngleSj := VertexAngle(LastRay[1], LastRay[2], Sj);
        VertexAngleSjMinus1 := VertexAngle(LastRay[1], LastRay[2], SjMinus1);
        if (((VertexAngleSj <> 180) and (VertexAngleSj <> 0))
        or (VertexAngleSjMinus1 <> 180) and (VertexAngleSjMinus1 <> 0))
          and Intersect(LastRay, EquateSegment(Sj, SjMinus1), u) then
        begin
          if ((Abs(u.x - Sj.x) > Epsilon) or (Abs(u.y - Sj.Y)> Epsilon))
            and ((Abs(u.x - SjMinus1.x) > Epsilon) or (Abs(u.y - SjMinus1.Y)> Epsilon)) then
          begin
            PointStack.Pop;
            IndexStack.Pop;
            PointStack.Push(u);
            IndexStack.Push(-1);
            Proc := noScanB;
          end;
        end;
      end;

    until (Proc = noFinish);

    Assert(PointStack.Count = IndexStack.Count);
    SetLength(result, PointStack.Count-1);
    SetLength(Indices, PointStack.Count-1);
    PolyIndex := 0;
    result[PolyIndex] := PointStack[PointStack.Count - 2];
    Indices[PolyIndex] := IndexStack[PointStack.Count - 2];

    for Index := 0 to PointStack.Count - 3 do
    begin
      Inc(PolyIndex);
      result[PolyIndex] := PointStack[index];
      Indices[PolyIndex] := IndexStack[index];
    end;

  finally
    IndexList.Free;
    PointStack.Free;
    IndexStack.Free;
    PointList.Free;
  end;
end;

{ TVisStack<T> }

constructor TVisStack<T>.Create;
begin
  FList := TList<T>.Create;
end;

destructor TVisStack<T>.Destroy;
begin
  FList.Free;
  inherited;
end;

function TVisStack<T>.GetCapacity: Integer;
begin
  result := FList.Capacity;
end;

function TVisStack<T>.GetCount: Integer;
begin
  result := FList.Count;
end;

function TVisStack<T>.GetItem(index: Integer): T;
begin
  result := FList[index];
end;

function TVisStack<T>.NextToLastItem: T;
begin
  result := FList[Count-2];
end;

function TVisStack<T>.Peek: T;
begin
  result := FList.Last;
end;

procedure TVisStack<T>.Pop;
begin
  FList.Delete(Count-1);
end;

procedure TVisStack<T>.Push(AnItem: T);
begin
  FList.Add(AnItem);
end;

procedure TVisStack<T>.SetCapacity(const Value: Integer);
begin
  FList.Capacity := Value;
end;

end.
