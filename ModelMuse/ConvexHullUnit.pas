unit ConvexHullUnit;

{
References:

de Berg, Mark; van Kreveld, Mark; Overmars, Mark; Schwarzkopf, Otfried.
  2000. Computational Geometry, Algorithms and Applications. Second Edition.
  Springer, Berlin. 367 p.

}

interface

uses Classes, FastGEO;

// @name generates the convex hull of the points defined in InputPoints.
// OutputPoints will contain all points that intersect
// the convex hull, not just the minimal set required to
// define the convex hull.
procedure ConvexHull(const InputPoints: TPolygon2D;
  out OutputPoints : TPolygon2D; const Epsilon: TFloat = 0);

// @name is like @link(ConvexHull) except that InputOrientation will
// be set to FastGeo.CollinearOrientation, FastGeo.CounterClockwise, or
// FastGeo.Clockwise depending on the orientation of InputPoints.
// InputPoints must represent a polygon and not a random set of points
// for InputOrientation to be meaningful.
procedure ConvexHull2(const InputPoints: TPolygon2D;
  out InputOrientation: integer; out OutputPoints : TPolygon2D;
  const Epsilon: TFloat = 0);

implementation

uses Math;

function ComparePoints (Item1, Item2: Pointer): Integer;
var
  Point1, Point2 : TPoint2DPtr;
begin
  Point1 := Item1;
  Point2 := Item2;
  result := Sign(Point1.X - Point2.X);
  if result = 0 then
  begin
    result := Sign(Point1.Y - Point2.Y);
  end;
end;

function EpsilonOrientation(const x1,y1,x2,y2,Px,Py: TFloat;
  const Epsilon: TFloat = 0):Integer; overload;
var
  Orin : TFloat;
begin
  (* Determinant of the 3 points *)
  Orin := (x2 - x1) * (py - y1) - (px - x1) * (y2 - y1);

  if Orin > Epsilon then
    Result := LeftHandSide          (* Orientaion is to the left-hand side  *)
  else if Orin < -Epsilon then
    Result := RightHandSide         (* Orientaion is to the right-hand side *)
  else
    Result := CollinearOrientation; (* Orientaion is neutral aka collinear  *)
end;

function EpsilonOrientation(const Point1,Point2,Point3:TPoint2D;
  const Epsilon: TFloat = 0):Integer; overload;
begin
  Result := EpsilonOrientation(Point1.x,Point1.y,Point2.x,Point2.y,
    Point3.x,Point3.y, Epsilon);
end;


function RightTurnOrStraight(const HullList : TList;
  const Epsilon: TFloat = 0) : boolean;
var
  Point1, Point2, Point3: TPoint2DPtr;
begin
  Assert(HullList.Count >= 3);
  Point1 := HullList[HullList.Count-3];
  Point2 := HullList[HullList.Count-2];
  Point3 := HullList[HullList.Count-1];
  result := EpsilonOrientation(Point1^, Point2^, Point3^,
    Epsilon) <> RightHandSide;
end;

function DuplicatePoint(HullList: TList; const Epsilon: TFloat = 0): boolean;
var
  Point1, Point2: TPoint2DPtr;
begin
  if HullList.Count < 2 then
  begin
    result := False;
  end
  else
  begin
    Point1 := HullList[HullList.Count-2];
    Point2 := HullList[HullList.Count-1];
    result := (Abs(Point1.x - Point2.x) <= Epsilon)
      and (Abs(Point1.y - Point2.y) <= Epsilon)
  end;
end;

procedure CreateHull(const InputPoints: TPolygon2D;
  out OutputPoints: TPolygon2D; InputList, OutputList: TList;
  const Epsilon: TFloat = 0); overload;
var
  Index: Integer;
  APoint: TPoint2DPtr;
  Point1, Point2, Point3: TPoint2DPtr;
  Procedure UpdateHull;
  begin
    // add the current point to the OutputList.
    OutputList.Add(InputList[Index]);
    if DuplicatePoint(OutputList) then
    begin
      OutputList.Delete(OutputList.Count -1);
    end
    else
    begin
      while (OutputList.Count > 2) and not RightTurnOrStraight(OutputList) do
      begin
        // If you didn't make a right turn, the previous point was not
        // on the maximal convex hull.
        OutputList.Delete(OutputList.Count -2);
      end;
      if DuplicatePoint(OutputList) then
      begin
        OutputList.Delete(OutputList.Count -1);
      end;
    end;

  end;
begin
  OutputList.Capacity := Length(InputPoints);
  InputList.Capacity := Length(InputPoints);
  for Index := 0 to Length(InputPoints) - 1 do
  begin
    InputList.Add(Addr(InputPoints[Index]));
  end;
  // Sort the input list from lowest to highest X.
  // In the event of ties, sort from lowest to highest Y.
  InputList.Sort(ComparePoints);

  // Loop over the InputList and update the convex hull for each point.
  for Index := 0 to InputList.Count - 1 do
  begin
    UpdateHull;
  end;
  // Now we have the upper half of the convex hull.
  // Loop over the InputList and update the convex hull for each point.
  // However, skip the last point in the InputList;
  // it has already been added.
  // Don't skip the first point despite the fact that it has already
  // been added.
  for Index := InputList.Count - 2 downto 0 do
  begin
    UpdateHull;
  end;

  // Delete middle points that are on a straight line.
  for Index := OutputList.Count - 1 downto 2 do
  begin
    Point1 := OutputList[Index-2];
    Point2 := OutputList[Index-1];
    Point3 := OutputList[Index];
    if EpsilonOrientation(Point1^, Point2^, Point3^,
      Epsilon) = CollinearOrientation then
    begin
      if ((Point1.x < Point2.x) = (Point2.x < Point3.x))
        and ((Point1.y < Point2.y) = (Point2.y < Point3.y)) then
      begin
        OutputList.Delete(Index-1);
      end;
    end;
  end;

  // Copy OutputList into OutputPoints.
  //
  // The first point has been added twice; once at the beginning and
  // once at the end.  The second copy was needed to check that points
  // prior to it weren't on the convex hull but we don't want two copies
  // of it in OutputPoints.
  if OutputList.Count > 1 then
  begin
    SetLength(OutputPoints, OutputList.Count - 1);
  end
  else
  begin
    // All the points were identical.
    SetLength(OutputPoints, OutputList.Count);
  end;
  for Index := 0 to Length(OutputPoints) - 1 do
  begin
    APoint := OutputList[Index];
    OutputPoints[Index] := APoint^;
  end;
end;

procedure ConvexHull(const InputPoints: TPolygon2D;
  out OutputPoints : TPolygon2D; const Epsilon: TFloat = 0);
var
  OutputList: TList;
  InputList: TList;
begin
  // InputPoints can represent a random set of points.

  // Take care of a trivial case
  if Length(InputPoints) < 3 then
  begin
    OutputPoints := Copy(InputPoints);
    Exit;
  end;

  OutputList := TList.Create;
  InputList := TList.Create;
  try
    CreateHull(InputPoints, OutputPoints, InputList,
      OutputList, Epsilon);
  finally
    InputList.Free;
    OutputList.Free;
  end;
end;

procedure ConvexHull2(const InputPoints: TPolygon2D;
  out InputOrientation: integer; out OutputPoints : TPolygon2D;
  const Epsilon: TFloat = 0); 
var
  OutputList: TList;
  InputList: TList;
  RightCount: Integer;
  LeftCount: Integer;
  Index: Integer;
  PriorPosition: Integer;
  NewPosition: Integer;
  PositionList: TList;
begin
  // InputPoints must represent a polygon and not a random set of points
  // for InputOrientation to be meaningful.

  // Take care of a trivial case
  if Length(InputPoints) < 3 then
  begin
    OutputPoints := Copy(InputPoints);
    InputOrientation := CollinearOrientation;
    Exit;
  end;

  OutputList := TList.Create;
  InputList := TList.Create;
  PositionList := TList.Create;
  try
    PositionList.Capacity := Length(InputPoints);
    for Index := 0 to Length(InputPoints) - 1 do
    begin
      PositionList.Add(Addr(InputPoints[Index]));
    end;
    CreateHull(InputPoints, OutputPoints, InputList,
      OutputList, Epsilon);
    if (OutputList.Count < 4) then
    begin
      InputOrientation := CollinearOrientation;
    end
    else
    begin
      RightCount := 0;
      LeftCount := 0;
      PriorPosition := PositionList.IndexOf(OutputList[0]);
      for Index := 1 to OutputList.Count - 1 do
      begin
        NewPosition := PositionList.IndexOf(OutputList[Index]);
        if (NewPosition = 0) and (Index = OutputList.Count-1) then
        begin
          NewPosition := -1;
        end;
        if (PriorPosition - NewPosition) < 0 then
        begin
          Inc(RightCount);
          if RightCount >= 2 then
          begin
            InputOrientation := CounterClockwise;
            break;
          end;
        end
        else
        begin
          Inc(LeftCount);
          if LeftCount >= 2 then
          begin
            InputOrientation := Clockwise;
            break;
          end;
        end;
        PriorPosition := NewPosition;
      end;
    end;
  finally
    InputList.Free;
    OutputList.Free;
    PositionList.Free;
  end;
end;

end.
