{@abstract(@name is used to define @link(TLine) which is used to select
  objects on the screen by drawing a "lasso" around them.)}
unit SelectUnit;

interface

uses
  // TPositionedLayer is declared in GR32_Layers.
  GR32, // TBitmap32, and TFloatRect are declared in GR32.
  Types, SysUtils, Math, Graphics, Classes, Contnrs, GoPhastTypes, FastGEO,
  ZoomBox2;

type
  {@name defines how two line segments intersect with each other.}
  TIntersectResult = (irDoIntersect, irDontIntersect, irColinear);

  TSimpleLine = class (TObject)
  private
    // See @link(Count).
    FCount: integer;
    // See @link(Points)
    FPoints: TPolyLine2D;
    FZoomBox: TQRbwZoomBox2;
    // See @link(Points).
    function GetPoint(const Index: integer): TPoint2D;
    procedure SetPoint(const Index: integer; const Value: TPoint2D);
  public
    Constructor Create(AZoomBox: TQRbwZoomBox2);
    // @name adds a new point to the end of @classname
    procedure AddPoint(const APoint: TPoint2D);
    // @name is the number of locations in @link(Points)
    // that is used by this @classname.
    property Count: integer read FCount write FCount;
    // @name draws @classname on BitMap as a polygon.
    procedure Draw(const BitMap: TBitmap32);
    //property Points: TPointArray read FPoints write SetPoints;
    property Points[const Index: integer]: TPoint2D read GetPoint write SetPoint;
    function LineLength: double;
    property ZoomBox: TQRbwZoomBox2 read FZoomBox;
    procedure DeleteNextToLastPoint;
  end;

  {@abstract(@link(TLine)  is used to select
  objects on the screen by drawing a "lasso" around them.
  See @link(InteractiveTools.TLassoTool).)
  }
  TLine = class(TObject)
  private
    // @name indicates whether or not the length of @link(FPoints) can
    // be changed. FCanAdjustBounds is set to @False in @link(AssignPoints)
    // and @link(CopyPoints). It is used to prevent sublines
    // (@link(FFirstSubLine) and @link(FSecondSubLine)) from
    // adding points.
    FCanAdjustBounds: boolean;
    // See @link(Count).
    FCount: integer;
    // @name is used to handle the first
    // half of the points in its parent TLine.
    // Its last point is shared with @link(FSecondSubLine).
    FFirstSubLine: TLine;
    // The coordinates of @name are equal to the highest X and Y coordinates
    // of any of the TPoints in @link(Points) between Start and Start+Count-1.
    FMaxP: TPoint;
    // The coordinates of @name are equal to the lowest X and Y coordinates
    // of any of the TPoints in @link(Points) between Start and Start+Count-1.
    FMinP: TPoint;
    // See @link(Points) and @link(AssignPoints).
    FPoints: TPointArray;
    // @name is used to handle the second
    // half of the points in its parent TLine.
    // Its first point is shared with @link(FFirstSubLine).
    FSecondSubLine: TLine;
    // See @link(Start).
    FStart: integer;
    // @name indicates whether @link(FMaxP) and @link(FMinP) are up-to-date.
    FUpdateBounds: boolean;
    // @name creates @link(FFirstSubLine) and @link(FSecondSubLine) and
    // half of its points to each of them.  They will share one point.
    procedure CreateSubLines;
    // See @link(Points).
    function GetPoint(const Index: integer): TPoint;
    // @name computes how ALine and the current line intersect.
    // both ALine and the current line must have a Count of 2.
    function GetIntersection(const ALine: TLine): TIntersectResult;
    // @name returns true if the point (X,Y) is inside the
    // box defined by (FMinP, FMaxP).
    function InsideBox(const X, Y: integer): boolean;
    // @name sets @link(FMinP) and (FMaxP).
    procedure SetBounds;
    procedure CopyPoints(const Value: TPointArray);
  public
    // @name adds a new point to the end of @classname
    procedure AddPoint(const APoint: TPoint);
    // @name copies Value.  @link(AddPoint) can not be called after @name.
    procedure AssignPoints(const Value: TPointArray);
    // @name is the number of locations in @link(Points)
    // that is used by this @classname.
    procedure Clear;
    property Count: integer read FCount write FCount;
    // @name creates a new instance of @classname.
    // If capacity is set greater than 0, @link(Points) is allocated
    constructor Create(const Capacity: integer);
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name draws @classname on BitMap as a polygon.
    procedure Draw(const BitMap: TBitmap32);
    // @name returns true if the point (X,Y) is inside the polygon
    // defined by @classname.
    function Inside(const X, Y: integer): boolean; overload;
    // @name returns true if the point APoint is inside the polygon
    // defined by @classname.
    function Inside(const APoint: TPoint): boolean; overload;
    // @name returns @true if ALine intersects @classname.
    function Intersect(const ALine: TLine): boolean;
    // Call @name to indicate that the @classname has changed.
    procedure Invalidate;
    //property Points: TPointArray read FPoints write SetPoints;
    property Points[const Index: integer]: TPoint read GetPoint;
    // @name sets @link(FMinP) and (FMaxP) to the box
    // defined by Corner1 and Corner2.
    // The order of Corner1 and Corner2 doesn't matter.
    procedure SetBox(const Corner1, Corner2: TPoint);
    // @name is the index of the first location in @link(Points)
    // that is used by this @classname.
    property Start: integer read FStart write FStart;
  end;

  TLines = class(TObject)
  private
    // @name is actually a TObjectList.
    FLines: TList;
    FCapacity: integer;
    function GetLines(Index: integer): TLine;
    procedure SetCapacity(const Value: integer);
    function GetCount: integer;
  public
    function Add(ALine: TLine): integer;
    Procedure Clear;
    Constructor Create;
    Destructor Destroy; override;
    // @name returns true if the point (X,Y) is inside the polygon
    // defined by @classname.
    function Inside(const X, Y: integer): boolean; overload;
    // @name returns true if the point APoint is inside the polygon
    // defined by @classname.
    function Inside(const APoint: TPoint): boolean; overload;
    // @name returns @true if ALine intersects @classname.
    function Intersect(const ALine: TLine): boolean;
    property Count: integer read GetCount;
    property Capacity: integer read FCapacity write SetCapacity;
    property Lines[Index: integer]: TLine read GetLines; default;
  end;

implementation

uses BigCanvasMethods;

resourcestring
  StrCantAddPoint = 'Can''t add point';

{ TLine }

procedure TLine.AddPoint(const APoint: TPoint);
begin
  if not FCanAdjustBounds then
  begin
    raise Exception.Create(StrCantAddPoint);
  end;
  if Length(FPoints) <= Count then
  begin
    SetLength(FPoints, Count * 2 + 1);
  end;
  FPoints[Count] := APoint;
  Inc(FCount);
  Invalidate;
end;

constructor TLine.Create(const Capacity: integer);
begin
  Start := 0;
  FCount := 0;
  if Capacity > 0 then
  begin
    SetLength(FPoints, Capacity);
    FCanAdjustBounds := True;
  end;
  FUpdateBounds := True;
end;

destructor TLine.Destroy;
begin
  FFirstSubLine.Free;
  FSecondSubLine.Free;
  inherited;
end;

procedure TLine.Draw(const BitMap: TBitmap32);
begin
  DrawBigPolyline32(BitMap, clBlack32, 1,
    FPoints, {False,} True, True, Start, Count);
end;

function TLine.GetIntersection(const ALine: TLine): TIntersectResult;
{
http://www1.acm.org/pubs/tog/GraphicsGems/gemsii/xlines.c
AUTHOR: Mukesh Prasad
}
  function SameSign(const A, B: integer): boolean;
  begin
    result := (A >= 0) = (B >= 0);
    {if A >= 0 then
    begin
      result := (B >= 0)
    end
    else
    begin
      result := (B < 0)
    end;  }
  end;
var
  x1, y1, x2, y2, x3, y3, x4, y4: integer;
  a1, a2, b1, b2, c1, c2: integer; { Coefficients of line eqns. }
  r1, r2, r3, r4: integer; { 'Sign' values }
  //  denom, offset, num: integer;      { Intermediate values }
begin
  x1 := FPoints[Start].X;
  y1 := FPoints[Start].Y;
  x2 := FPoints[Start + 1].X;
  y2 := FPoints[Start + 1].Y;
  x3 := ALine.FPoints[ALine.Start].X;
  y3 := ALine.FPoints[ALine.Start].Y;
  x4 := ALine.FPoints[ALine.Start + 1].X;
  y4 := ALine.FPoints[ALine.Start + 1].Y;

  { Compute a1, b1, c1, where line joining points 1 and 2
   * is "a1 x  +  b1 y  +  c1  =  0".
   }

  a1 := y2 - y1;
  b1 := x1 - x2;
  c1 := x2 * y1 - x1 * y2;

  { Compute r3 and r4.
   }

  r3 := a1 * x3 + b1 * y3 + c1;
  r4 := a1 * x4 + b1 * y4 + c1;

  { Check signs of r3 and r4.  If both point 3 and point 4 lie on
    same side of line 1, the line segments do not intersect.
   }

  if (r3 <> 0) and
    (r4 <> 0) and
    SameSign(r3, r4) then
  begin
    result := irDontIntersect;
    Exit;
  end;

  { Compute a2, b2, c2 }

  a2 := y4 - y3;
  b2 := x3 - x4;
  c2 := x4 * y3 - x3 * y4;

  { Compute r1 and r2 }

  r1 := a2 * x1 + b2 * y1 + c2;
  r2 := a2 * x2 + b2 * y2 + c2;

  { Check signs of r1 and r2.  If both point 1 and point 2 lie
    on same side of second line segment, the line segments do
    not intersect.
   }

  if (r1 <> 0) and
    (r2 <> 0) and
    SameSign(r1, r2) then
  begin
    result := irDontIntersect;
    Exit;
  end;

  { Line segments intersect: compute intersection point.
   }

{
  denom := a1 * b2 - a2 * b1;
  if ( denom = 0 ) then
  begin
    result := irColinear;
    Exit;
  end;   }

{
  if denom < 0 then
  begin
    offset := -denom div 2;
  end
  else
  begin
    offset := denom div 2;
  end; }

  { The denom/2 is to get rounding instead of truncating.  It
    is added or subtracted to the numerator, depending upon the
    sign of the numerator.
   }

{  num := b1 * c2 - b2 * c1;
  if num < 0 then
  begin
    Intersection.X := (num - offset) div denom;
  end
  else
  begin
    Intersection.X := (num + offset) div denom;
  end;

  num := a2 * c1 - a1 * c2;

  if num < 0 then
  begin
    Intersection.Y := (num - offset) div denom;
  end
  else
  begin
    Intersection.Y := (num + offset) div denom;
  end;  }
  result := irDoIntersect;
end;

function TLine.Inside(const APoint: TPoint): boolean;
begin
  result := Inside(APoint.X, APoint.Y);
end;

function TLine.Inside(const X, Y: integer): boolean;
var
  APoint, AnotherPoint: TPoint;
  procedure EvaluateSubLines(const ASubLine: TLine);
  var
    VertexIndex: integer;
  begin
    if (Y < ASubLine.FMinP.Y) or (Y > ASubLine.FMaxP.Y) then
      Exit;
    if ASubLine.FFirstSubLine <> nil then
    begin
      EvaluateSubLines(ASubLine.FFirstSubLine);
      EvaluateSubLines(ASubLine.FSecondSubLine);
    end
    else
    begin
      for VertexIndex := 0 to ASubLine.Count - 2 do
      begin
        APoint := FPoints[ASubLine.Start + VertexIndex];
        AnotherPoint := FPoints[ASubLine.Start + VertexIndex + 1];
        if ((Y <= APoint.Y) = (Y > AnotherPoint.Y)) and
          (X - APoint.X - (Y - APoint.Y) *
          (AnotherPoint.X - APoint.X) /
          (AnotherPoint.Y - APoint.Y) < 0) then
        begin
          result := not result;
        end;
      end;
    end;
  end;
begin
  result := false;
  if Count < 4 then
  begin
    Exit;
    // The first and last points must be at the same location for a
    // screen object to be closed.  Thus a triangle would have four points;
    // not three.
  end;

  APoint := FPoints[0];
  AnotherPoint := FPoints[Count - 1];
  if (APoint.X <> AnotherPoint.X) or
    (APoint.Y <> AnotherPoint.Y) then
  begin
    // If the screen object is not closed, nothing can be inside it.
    Exit;
  end;

  // Make a quick check to make sure it might possibly be inside
  // the screen object.
  if not InsideBox(X, Y) then
  begin
    Exit;
  end;

  if FFirstSubLine = nil then
  begin
    CreateSubLines;
  end;

  EvaluateSubLines(FFirstSubLine);
  EvaluateSubLines(FSecondSubLine);
end;

procedure TLine.SetBox(const Corner1, Corner2: TPoint);
begin
  FMaxP.X := Max(Corner1.X, Corner2.X);
  FMaxP.Y := Max(Corner1.Y, Corner2.Y);
  FMinP.X := Min(Corner1.X, Corner2.X);
  FMinP.Y := Min(Corner1.Y, Corner2.Y);
  FUpdateBounds := False;
end;

procedure TLine.Clear;
begin
  FCount := 0;
end;

procedure TLine.CopyPoints(const Value: TPointArray);
begin
  FCanAdjustBounds := False;
  FPoints := Value;
end;

function TLine.InsideBox(const X, Y: integer): boolean;
begin
  SetBounds;
  result := (X <= FMaxP.X) and (X >= FMinP.X)
    and (Y <= FMaxP.Y) and (Y >= FMinP.Y);
end;

function TLine.Intersect(const ALine: TLine): boolean;
begin
  result := False;
  if (Count < 2) or (ALine.Count < 2) then
  begin
    Exit;
  end;
  SetBounds;
  ALine.SetBounds;
  if (ALine.FMaxP.X < FMinP.X) or (ALine.FMaxP.Y < FMinP.Y)
    or (FMaxP.X < ALine.FMinP.X) or (FMaxP.Y < ALine.FMinP.Y) then
  begin
    Exit;
  end;
  if (Count > 2) or (ALine.Count > 2) then
  begin
    if Count > ALine.Count then
    begin
      if (FFirstSubLine = nil) or (FSecondSubLine = nil) then
      begin
        CreateSublines;
      end;
      result := FFirstSubLine.Intersect(ALine)
        or FSecondSubLine.Intersect(ALine);
    end
    else
    begin
      if (ALine.FFirstSubLine = nil) or (ALine.FSecondSubLine = nil) then
      begin
        ALine.CreateSublines;
      end;
      result := Intersect(ALine.FFirstSubLine)
        or Intersect(ALine.FSecondSubLine);
    end;
  end
  else
  begin
    result := GetIntersection(ALine) <> irDontIntersect;
  end;
end;

procedure TLine.Invalidate;
begin
  FUpdateBounds := True;
  FreeAndNil(FFirstSubLine);
  FreeAndNil(FSecondSubLine);
end;

procedure TLine.CreateSubLines;
begin
  FFirstSubLine.Free;
  FSecondSubLine.Free;
  FFirstSubLine := TLine.Create(0);
  FSecondSubLine := TLine.Create(0);
  FFirstSubLine.CopyPoints(FPoints);
  FSecondSubLine.CopyPoints(FPoints);
  FFirstSubLine.Start := Start;
  FFirstSubLine.FCount := Count div 2 + 1;
  FSecondSubLine.Start := Start + FFirstSubLine.Count - 1;
  FSecondSubLine.FCount := Count - FFirstSubLine.Count + 1;
  FFirstSubLine.SetBounds;
  FSecondSubLine.SetBounds;
end;

procedure TLine.SetBounds;
var
  Point1, Point2: TPoint;
begin
  if not FUpdateBounds then
    Exit;
  if Count > 2 then
  begin
    CreateSubLines;
    FMaxP.X := Max(FFirstSubLine.FMaxP.X, FSecondSubLine.FMaxP.X);
    FMaxP.Y := Max(FFirstSubLine.FMaxP.Y, FSecondSubLine.FMaxP.Y);
    FMinP.X := Min(FFirstSubLine.FMinP.X, FSecondSubLine.FMinP.X);
    FMinP.Y := Min(FFirstSubLine.FMinP.Y, FSecondSubLine.FMinP.Y);
  end
  else if Count = 2 then
  begin
    Point1 := FPoints[Start];
    Point2 := FPoints[Start + 1];
    FMaxP.X := Max(Point1.X, Point2.X);
    FMaxP.Y := Max(Point1.Y, Point2.Y);
    FMinP.X := Min(Point1.X, Point2.X);
    FMinP.Y := Min(Point1.Y, Point2.Y);
  end;
  FUpdateBounds := False;
end;

procedure TLine.AssignPoints(const Value: TPointArray);
begin
  CopyPoints(Value);
  SetLength(FPoints, Length(FPoints));
end;

function TLine.GetPoint(const Index: integer): TPoint;
begin
  result := FPoints[Index];
end;

{ TLines }

function TLines.Add(ALine: TLine): integer;
begin
  result := FLines.Add(ALine);
end;

procedure TLines.Clear;
begin
  FLines.Clear;
end;

constructor TLines.Create;
begin
  FLines := TObjectList.Create
end;

destructor TLines.Destroy;
begin
  FLines.Free;
  inherited;
end;

function TLines.GetCount: integer;
begin
  result := FLines.Count;
end;

function TLines.GetLines(Index: integer): TLine;
begin
  result := FLines[Index];
end;

function TLines.Inside(const X, Y: integer): boolean;
var
  Index: integer;
begin
  result := False;
  for Index := 0 to Count - 1 do
  begin
    result := Lines[Index].Inside(X, Y);
    if result then
    begin
      Exit;
    end;
  end;
end;

function TLines.Inside(const APoint: TPoint): boolean;
begin
  result := Inside(APoint.X, APoint.Y);
end;

function TLines.Intersect(const ALine: TLine): boolean;
var
  Index: integer;
begin
  result := False;
  for Index := 0 to Count - 1 do
  begin
    result := Lines[Index].Intersect(ALine);
    if result then
    begin
      Exit;
    end;
  end;
end;

procedure TLines.SetCapacity(const Value: integer);
begin
  FLines.Capacity := Value;
end;

{ TSimpleLine }

procedure TSimpleLine.AddPoint(const APoint: TPoint2D);
begin
  if Length(FPoints) = 0 then
  begin
    SetLength(FPoints, 4);
  end;
  if Length(FPoints) <= Count then
  begin
    SetLength(FPoints, Count * 2 + 1);
  end;
  FPoints[Count] := APoint;
  Inc(FCount);
end;

constructor TSimpleLine.Create(AZoomBox: TQRbwZoomBox2);
begin
  FZoomBox := AZoomBox;
end;

procedure TSimpleLine.DeleteNextToLastPoint;
begin
  if Count > 2 then
  begin
    FPoints[Count-2] := FPoints[Count-1];
    Dec(FCount);
  end;
end;

procedure TSimpleLine.Draw(const BitMap: TBitmap32);
var
  PointArray: TPointArray;
  PointIndex: Integer;
begin
  if Count > 1 then
  begin
    SetLength(PointArray, Count);
    for PointIndex := 0 to Count - 1 do
    begin
      PointArray[PointIndex].X := FZoomBox.XCoord(FPoints[PointIndex].x);
      PointArray[PointIndex].Y := FZoomBox.YCoord(FPoints[PointIndex].y);
    end;
    DrawBigPolyline32(BitMap, clBlack32, 1,
      PointArray, True, True, 0, Count);
  end;
end;

function TSimpleLine.GetPoint(const Index: integer): TPoint2D;
begin
  result := FPoints[Index];
end;

function TSimpleLine.LineLength: double;
var
  PointIndex: Integer;
begin
  result := 0;
  for PointIndex := 0 to Count - 2 do
  begin
    result := result + Distance(FPoints[PointIndex], FPoints[PointIndex+1]);
  end;
end;

procedure TSimpleLine.SetPoint(const Index: integer; const Value: TPoint2D);
begin
  FPoints[Index] := Value;
end;

end.

