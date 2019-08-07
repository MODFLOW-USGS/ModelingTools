unit VisTest;

// http://geomalgorithms.com/a14-_extreme_pts.html
// http://geomalgorithms.com/a15-_tangents.html#tangent_PointPolyC%28%29

interface

uses
  FastGEO, System.Types, System.Generics.Collections, System.Generics.Defaults;

// Input: Points is the array of points to be triangulated by the line-sweep
// algorithm
// Output: The result of the function is an array of integers.
// The indicies of the points that make up a triangle are defined by
// the points i*3, i*3+1 and i*3+2 in Points;
function TriangulatePoints(Points: array of TPoint2D): TIntegerDynArray;

implementation

uses
  System.Math;

type
  TOrderedPoint = class(TObject)
    FLocation: TPoint2D;
    FOriginalOrder: integer;
  Private
    constructor Create(ALocation: TPoint2D; Order: Integer);
    property Location: TPoint2D read FLocation;
    property OriginalOrder: integer read FOriginalOrder;
    property X : double read FLocation.X;
    property Y : double read FLocation.Y;
  end;

  TOrderedPointList = TList<TOrderedPoint>;
  TOrderedPointObjectList = TObjectList<TOrderedPoint>;

  TPointPositionComparer = TComparer<TOrderedPoint>;

// isLeft(): test if a point is Left|On|Right of an infinite line.
//    Input:  three points P0, P1, and P2
//    Return: >0 for P2 left of the line through P0 and P1
//            =0 for P2 on the line
//            <0 for P2 right of the line
//    See: Algorithm 1 on Area of Triangles

function isLeft( P0, P1, P2:  TOrderedPoint): double;
begin
    result := (P1.x - P0.x)*(P2.y - P0.y) - (P2.x - P0.x)*(P1.y - P0.y);
end;

// tests for polygon vertex ordering relative to a fixed point P
function above(P,Vi,Vj: TOrderedPoint): Boolean;
begin
  result := (isLeft(P,Vi,Vj) > 0)   // true if Vi is above Vj
end;

function below(P,Vi,Vj: TOrderedPoint): Boolean;
begin
  result := (isLeft(P,Vi,Vj) < 0)   // true if Vi is below Vj
end;

// Rtangent_PointPolyC(): binary search for convex polygon right tangent
//    Input:  P = a 2D point (exterior to the polygon)
//            n = number of polygon vertices
//            V = array of vertices for a 2D convex polygon with V[n] = V[0]
//    Return: index "i" of rightmost tangent point V[i]
function Rtangent_PointPolyC( P: TOrderedPoint; n: Integer;
  V: TOrderedPointList): Integer;
var
         a, b, c: integer;            // indices for edge chain endpoints
         upA, dnC: boolean;           // test for up direction of edges a and c
begin
    // use binary search for large convex polygons

    // rightmost tangent = maximum for the isLeft() ordering
    // test if V[0] is a local maximum
    if (below(P, V[1], V[0]) and not above(P, V[n-1], V[0])) then
    begin
        result := 0;               // V[0] is the maximum tangent point
        Exit;
    end;

    a := 0;
    b := n;
    repeat
              // start chain = [0,n] with V[n]=V[0]
        c := (a + b) div 2;        // midpoint of [a,b], and 0<c<n
        dnC := below(P, V[c+1], V[c]);
        if (dnC and not above(P, V[c-1], V[c])) then
        begin
            Result := c;          // V[c] is the maximum tangent point
            Exit;
        end;

        // no max yet, so continue with the binary search
        // pick one of the two subchains [a,c] or [c,b]
        upA := above(P, V[a+1], V[a]);
        if (upA) then
        begin                       // edge a points up
            if (dnC) then                        // edge c points down
            begin
                 b := c                           // select [a,c]
            end
            else
            begin                           // edge c points up
                 if (above(P, V[a], V[c])) then    // V[a] above V[c]
                 begin
                     b := c;                       // select [a,c]
                 end
                 else                          // V[a] below V[c]
                 begin
                     a := c;                       // select [c,b]
                 end;
            end;
        end
        else
        begin                           // edge a points down
            if (not dnC) then                       // edge c points up
            begin
                 a := c;                           // select [c,b]
            end
            else
            begin                           // edge c points down
                 if (below(P, V[a], V[c])) then    // V[a] below V[c]
                 begin
                     b := c;                       // select [a,c]
                 end
                 else                          // V[a] above V[c]
                 begin
                     a := c;                       // select [c,b]
                 end;
            end;
        end;
    until False;
end;



// Ltangent_PointPolyC(): binary search for convex polygon left tangent
//    Input:  P = a 2D point (exterior to the polygon)
//            n = number of polygon vertices
//            V = array of vertices for a 2D convex polygon with V[n]=V[0]
//    Return: index "i" of leftmost tangent point V[i]

function Ltangent_PointPolyC( P: TOrderedPoint; n: Integer;
  V: TOrderedPointList): integer;
var
         a, b, c: integer;            // indices for edge chain endpoints
         dnA, dnC: boolean;           // test for down direction of edges a and c
begin
    // use binary search for large convex polygons

    // leftmost tangent = minimum for the isLeft() ordering
    // test if V[0] is a local minimum
    if (above(P, V[n-1], V[0]) and not below(P, V[1], V[0])) then
    begin
        result := 0;               // V[0] is the minimum tangent point
        Exit;
    end;

    a := 0;
    b := n;
    repeat
              // start chain = [0,n] with V[n] = V[0]
        c := (a + b) div 2;        // midpoint of [a,b], and 0<c<n
        dnC := below(P, V[c+1], V[c]);
        if (above(P, V[c-1], V[c]) and not dnC) then
        begin
            Result := c;          // V[c] is the minimum tangent point
            Exit;
        end;

        // no min yet, so continue with the binary search
        // pick one of the two subchains [a,c] or [c,b]
        dnA := below(P, V[a+1], V[a]);
        if (dnA) then
        begin                       // edge a points down
            if (not dnC)  then                      // edge c points up
            begin
                 b := c;                           // select [a,c]
            end
            else
            begin                           // edge c points down
                 if (below(P, V[a], V[c])) then    // V[a] below V[c]
                 begin
                     b := c;                       // select [a,c]
                 end
                 else                          // V[a] above V[c]
                 begin
                     a := c;                       // select [c,b]
                 end;
            end;
        end
        else
        begin                           // edge a points up

            if (dnC)then                         // edge c points down
            begin
                 a := c;                           // select [c,b]
            end
            else
            begin                           // edge c points up
                 if (above(P, V[a], V[c])) then    // V[a] above V[c]
                 begin
                     b := c;                       // select [a,c]
                 end
                 else                          // V[a] below V[c]
                 begin
                     a := c;                       // select [c,b]
                 end;
            end;
        end;
    until False;
end;
//===================================================================




// tangent_PointPolyC(): fast binary search for tangents to a convex polygon
//    Input:  P = a 2D point (exterior to the polygon)
//            n = number of polygon vertices
//            V = array of vertices for a 2D convex polygon with V[n] = V[0]
//    Output: *rtan = index of rightmost tangent point V[*rtan]
//            *ltan = index of leftmost tangent point V[*ltan]
procedure tangent_PointPolyC(P: TOrderedPoint; n: Integer;
  V: TOrderedPointList; var rtan, ltan: integer);
begin
    rtan := Rtangent_PointPolyC(P, n, V);
    ltan := Ltangent_PointPolyC(P, n, V);
end;

function TriangulatePoints(Points: array of TPoint2D): TIntegerDynArray;
const
  PiDiv2 = Pi/2;
  TwoPi = Pi * 2;
var
  Locations: TOrderedPointObjectList;
  Index: Integer;
  Boundary: TOrderedPointList;
  TriangleList: TList<Integer>;
  PointIndex: Integer;
  APoint: TOrderedPoint;
  StartIndex: Integer;
  EndIndex: Integer;
  TriangleIndex: Integer;
  InnerPointIndex: Integer;
begin
  result := nil;
  // triangulate using line sweep.
  Assert(Length(Points) >= 3);
  Locations := TOrderedPointObjectList.Create;
  try
    for Index := 0 to Length(Points) - 1 do
    begin
      Locations.Add(TOrderedPoint.Create(Points[Index], Index));
    end;

    // Sort points in ascending order Left to right
    Locations.Sort(TPointPositionComparer.Construct(
      function (const L, R: TOrderedPoint): integer
       begin
         result := Sign(L.Location.x - R.Location.x);
         if result = 0 then
         begin
           result := Sign(L.Location.y - R.Location.y);
         end;
       end
       ));

    // Boundary is a polygon that surrounds all the points in Locations
    // that have been processed so far.
    Boundary := TOrderedPointList.Create;
    TriangleList := TList<Integer>.Create;
    try
      Boundary.Capacity := Locations.Count;
      Boundary.Add(Locations[0]);
      Boundary.Add(Locations[1]);
      Boundary.Add(Locations[2]);
      // Adding the first point agian makes it a closed polygon
      // and prevents out of range errors in
      // tangent_PointPolyC
      Boundary.Add(Locations[0]);

      // Make sure the boundary is oriented counterclockwise.
      if Orientation(Boundary[0].Location, Boundary[1].Location,
        Boundary[2].Location) = Clockwise then
      begin
        Boundary.Reverse
      end;
      TriangleList.Add(Boundary[0].OriginalOrder);
      TriangleList.Add(Boundary[1].OriginalOrder);
      TriangleList.Add(Boundary[2].OriginalOrder);

      for PointIndex := 3 to Locations.Count - 1 do
      begin
        APoint := Locations[PointIndex];

        // Get tangents of APoint to Boundary
        tangent_PointPolyC(APoint, Boundary.Count-1, Boundary,
          EndIndex, StartIndex);
        if EndIndex < StartIndex then
        begin
          EndIndex := Boundary.Count-1;
        end;

        // Add new triangles.
        for TriangleIndex := StartIndex to EndIndex - 1 do
        begin
          TriangleList.Add(Boundary[TriangleIndex].OriginalOrder);
          TriangleList.Add(Boundary[TriangleIndex+1].OriginalOrder);
          TriangleList.Add(APoint.OriginalOrder);
        end;

        // delete points that are no longer on the convex hull
        for InnerPointIndex := EndIndex-1 downto StartIndex + 1 do
        begin
          Boundary.Delete(InnerPointIndex);
        end;

        // Insert APoint into Boundary
        Boundary.Insert(StartIndex+1, APoint);
      end;

      // report results.
      SetLength(result, TriangleList.Count);
      for index := 0 to TriangleList.Count - 1 do
      begin
        result[index] := TriangleList[index];
      end;
    finally
      Boundary.Free;
      TriangleList.Free;
    end;

  finally
    Locations.Free;
  end;
end;

{ TOrderedPoint }

constructor TOrderedPoint.Create(ALocation: TPoint2D; Order: Integer);
begin
  FLocation := ALocation;
  FOriginalOrder := Order;
end;

end.
