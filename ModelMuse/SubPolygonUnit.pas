unit SubPolygonUnit;

interface

uses
{$IFDEF ModelMuse}
  GoPhastTypes,
{$ENDIF}
  FastGEO, Classes, Generics.Collections;

type

{$IFNDEF ModelMuse}
  TRealPointArray = TPolygon2D;//array of TPoint2D;
{$ENDIF}

  // @abstract(@name is used to make determining whether a point is inside
  // a @link(TScreenObject) faster and to make finding the nearest point
  // on a line faster.)
  // @Seealso(TScreenObject.EvaluateSubPolygon)
  // @Seealso(TScreenObject.IsAnyPointCloser)
  TSubPolygon = class(TObject)
  private
    FOriginalCount: integer;
    // @name is the number of points used by the @classname.
    FCount: integer;
    // @name is the maximum X value of any
    // of the points used by the @classname.
    FMaxX: real;
    // @name is the maximum Y value of any
    // of the points used by the @classname.
    FMaxY: real;
    // @name is the minimum X value of any
    // of the points used by the @classname.
    FMinX: real;
    // @name is the minimum Y value of any
    // of the points used by the @classname.
    FMinY: real;
    // @name is the index of the first point used by the @classname.
    FStart: integer;
    // @name represents the @classname used to
    // process the first half of the points if
    // the number of points exceeds a threshold.
    FSubPolygon1: TSubPolygon;
    // @name represents the @classname used to
    // process the second half of the points if
    // the number of points exceeds a threshold.
    FSubPolygon2: TSubPolygon;
    FPoints: TRealPointArray;
    FSectionIndex: integer;
    procedure CreateSubPolygons(const Points: TRealPointArray;
      const Count, Start, Section: Integer);
    procedure SetMaxAndMinWhenNoSubPolygons(const Count, Start: Integer;
      const Points: TRealPointArray);
    procedure SetMaxAndMinFromSubPolygons;
    procedure InternalBoxIntersect(SubPolygons: TList;
      const BoxMinX, BoxMaxX, BoxMinY, BoxMaxY: Double);
    function GetPoint(Index: Integer): TPoint2D;
    procedure SetPoint(Index: Integer; const Value: TPoint2D);
  public
    procedure EvaluateSubPolygon(
      const X, Y: real; var IsInside: boolean);
    // @name creates an instance of TSubPolygon.
    // If Count is large enough, it will create @link(FSubPolygon1)
    // and @link(FSubPolygon2) to handle what it needs to do.
    // @param(Points is the array of TPoint2Ds
    // to be used by @classname. The first and last points in Points should
    // be the same point.)
    // @param(Count is the number of TPoint2Ds
    // in Points to be used by @classname.)
    // @param(Start is the index of the  first TPoint2Ds
    // in Points to be used by @classname.)
    // @param(Section indicates the part of the @link(TScreenObject) that
    // this @name represents.)
    //

    constructor Create(const Points: TRealPointArray;
      const Count, Start, Section: integer);
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
    procedure GrowByOne;
    Procedure BoxIntersect(const Point1, Point2: TPoint2D; SubPolygons: TList);
    // If @name is created for a part of a @link(TScreenObject),
    // @name indicates which part of the @link(TScreenObject) it represents.
    property SectionIndex: integer read FSectionIndex;
    function IsPointInside(const X, Y: real): boolean; overload;
    function IsPointInside(const APoint: TPoint2D): boolean; overload;
    function Intersect(ASubPolygon: TSubPolygon): Boolean;
    property Count: Integer read FCount;
    property Start: Integer read FStart;
    property MinX: Real read FMinX;
    property MaxX: Real read FMaxX;
    property MinY: Real read FMinY;
    property MaxY: Real read FMaxY;
    property Points[Index: Integer]: TPoint2D read GetPoint write SetPoint;
    property SubPolygon1: TSubPolygon read FSubPolygon1;
    property SubPolygon2: TSubPolygon read FSubPolygon2;
  end;

  // @name is an abstract base class.
  // define a method to add items to @link(FPolygons) in descendants.
  TOutline = class(TObject)
  private
    function GetPolygon(Index: Integer): TSubPolygon;
    function GetPolygonCount: Integer;
  protected
    FPolygons: TObjectList<TSubPolygon>;
  public
    function PointInside(APoint: TPoint2D): boolean; overload;
    function PointInside(X, Y: Real): boolean; overload;
    constructor Create;
    destructor Destroy; override;
    property PolygonCount: Integer read GetPolygonCount;
    property Polygons[Index: Integer]: TSubPolygon read GetPolygon;
  end;

const
  MaxPointsInSubPolygon = 4;

implementation

uses
  Math;


{ TSubPolygon }

procedure TSubPolygon.BoxIntersect(const Point1, Point2: TPoint2D;
  SubPolygons: TList);
var
  BoxMaxX, BoxMinX, BoxMaxY, BoxMinY: double;
begin
  if Point1.x > Point2.x then
  begin
    BoxMaxX := Point1.x;
    BoxMinX := Point2.x;
  end
  else
  begin
    BoxMaxX := Point2.x;
    BoxMinX := Point1.x;
  end;
  if Point1.Y > Point2.Y then
  begin
    BoxMaxY := Point1.Y;
    BoxMinY := Point2.Y;
  end
  else
  begin
    BoxMaxY := Point2.Y;
    BoxMinY := Point1.Y;
  end;
  InternalBoxIntersect(SubPolygons, BoxMinX, BoxMaxX, BoxMinY, BoxMaxY);
end;

constructor TSubPolygon.Create(const Points: TRealPointArray; const Count,
  Start, Section: integer);
begin
  FSectionIndex := Section;
  FOriginalCount := Count;
  Assert(Start + Count -1 < Length(Points));
  // Each subpolygon must store where in the array its data starts,
  // how many points it has and the Maximum Y value, Minimum Y value and
  // Maximum X value.
  // Store it's starting point and count.
  FStart := Start;
  FCount := Count;
  FPoints := Points;
  if Count > MaxPointsInSubPolygon then
  begin
    CreateSubPolygons(Points, Count, Start, Section);
  end
  else
  begin
    SetMaxAndMinWhenNoSubPolygons(Count, Start, Points);
  end;
end;

procedure TSubPolygon.CreateSubPolygons(const Points: TRealPointArray;
  const Count, Start, Section: Integer);
begin
  // If the number of points is too big, create additional subpolygons
  // that each store half the points.  The two subpolygons overlap at
  // one vertext.
  FSubPolygon1 := TSubPolygon.Create(Points, Count div 2 + 1, Start, Section);
  FSubPolygon2 := TSubPolygon.Create(Points, Count - FSubPolygon1.FCount + 1,
    Start + FSubPolygon1.FCount - 1, Section);
  SetMaxAndMinFromSubPolygons;
end;

destructor TSubPolygon.Destroy;
begin
  // Destroy the subpolygons if they exist.
  FSubPolygon1.Free;
  FSubPolygon2.Free;
  inherited;
end;

procedure TSubPolygon.EvaluateSubPolygon(const X, Y: real;
  var IsInside: boolean);
var
  VertexIndex: integer;
  APoint, AnotherPoint: TPoint2D;
begin
  if (Y < FMinY) or (Y > FMaxY)
    or (X > FMaxX) then
    Exit;
  if FSubPolygon1 <> nil then
  begin
    FSubPolygon1.EvaluateSubPolygon(X, Y, IsInside);
    FSubPolygon2.EvaluateSubPolygon(X, Y, IsInside);
  end
  else
  begin
    for VertexIndex := 0 to self.FCount - 2 do
    begin
      APoint := FPoints[self.FStart + VertexIndex];
      AnotherPoint := FPoints[self.FStart + VertexIndex + 1];
      if ((Y <= APoint.Y) = (Y > AnotherPoint.Y)) and
        (X - APoint.X - (Y - APoint.Y) *
        (AnotherPoint.X - APoint.X) /
        (AnotherPoint.Y - APoint.Y) < 0) then
      begin
        IsInside := not IsInside;
      end;
    end;
  end;
end;

function TSubPolygon.GetPoint(Index: Integer): TPoint2D;
begin
  result := FPoints[Index];
end;

procedure TSubPolygon.GrowByOne;
begin
  Inc(FCount);
  if (FCount > MaxPointsInSubPolygon) then
  begin
    if FSubPolygon2 = nil then
    begin
      FOriginalCount := FCount;
      CreateSubPolygons(FPoints, FCount, FStart, FSectionIndex);
    end
    else if (FCount > 2*FOriginalCount)
      or (FSubPolygon1.FCount*2 < FSubPolygon2.FCount) then
    begin
      FSubPolygon1.Free;
      FSubPolygon2.Free;
      FOriginalCount := FCount;
      CreateSubPolygons(FPoints, FCount, FStart, FSectionIndex);
    end
    else
    begin
      FSubPolygon2.GrowByOne;
      SetMaxAndMinFromSubPolygons;
    end;
  end
  else
  begin
    SetMaxAndMinWhenNoSubPolygons(FCount, FStart, FPoints);
  end;
end;

procedure TSubPolygon.InternalBoxIntersect(SubPolygons: TList; const BoxMinX,
  BoxMaxX, BoxMinY, BoxMaxY: Double);
begin
  if (BoxMaxX >= FMinX) and (BoxMinX <= FMaxX)
    and (BoxMaxY >= FMinY) and (BoxMinY <= FMaxY) then
  begin
    if FSubPolygon1 = nil then
    begin
      SubPolygons.Add(self);
    end
    else
    begin
      FSubPolygon1.InternalBoxIntersect(SubPolygons,
        BoxMinX, BoxMaxX, BoxMinY, BoxMaxY);
      FSubPolygon2.InternalBoxIntersect(SubPolygons,
        BoxMinX, BoxMaxX, BoxMinY, BoxMaxY);
    end;
  end;
end;

function TSubPolygon.Intersect(ASubPolygon: TSubPolygon): Boolean;
var
  Segment1: TSegment2D;
  Segment2: TSegment2D;
  OuterIndex: Integer;
  InnerIndex: Integer;
begin
  result := False;
  if (FMaxX < ASubPolygon.FMinX) or (ASubPolygon.FMaxX < FMinX)
    or (FMaxY < ASubPolygon.FMinY) or (ASubPolygon.FMaxY < FMinY) then
  begin
    Exit;
  end;
  if (SubPolygon1 = nil) then
  begin
    Assert(SubPolygon2 = nil);
    if (ASubPolygon.SubPolygon1 = nil) then
    begin
      for OuterIndex := 1 to Count - 1 do
      begin
        Segment1[1] := FPoints[FStart+OuterIndex-1];
        Segment1[2] := FPoints[FStart+OuterIndex];
        for InnerIndex := 1 to ASubPolygon.Count - 1 do
        begin
          Segment2[1] := ASubPolygon.FPoints[ASubPolygon.FStart+InnerIndex-1];
          Segment2[2] := ASubPolygon.FPoints[ASubPolygon.FStart+InnerIndex];
          result := FastGEO.Intersect(Segment1,Segment2);
          if Result then
          begin
            Exit;
          end;
        end;
      end;
    end
    else
    begin
      Result := ASubPolygon.Intersect(self);
    end;
  end
  else
  begin
    if ASubPolygon.SubPolygon1 = nil then
    begin
      Result := SubPolygon1.Intersect(ASubPolygon)
        or SubPolygon2.Intersect(ASubPolygon)
    end
    else
    begin
      if Count > ASubPolygon.Count then
      begin
        Result := SubPolygon1.Intersect(ASubPolygon)
          or SubPolygon2.Intersect(ASubPolygon)
      end
      else
      begin
        Result := ASubPolygon.SubPolygon1.Intersect(self)
          or ASubPolygon.SubPolygon2.Intersect(self)
      end;
    end;
  end;
end;

function TSubPolygon.IsPointInside(const X, Y: real): boolean;
begin
  Result := False;
  if (FPoints[0].x <> FPoints[Length(FPoints)-1].x)
    or (FPoints[0].y <> FPoints[Length(FPoints)-1].y) then
  begin
    Exit;
  end;
  EvaluateSubPolygon(X, Y, Result);
end;

function TSubPolygon.IsPointInside(const APoint: TPoint2D): boolean; 
begin
  result := IsPointInside(APoint.x, APoint.y);
 end;


procedure TSubPolygon.SetMaxAndMinFromSubPolygons;
begin
  // Determine the Maximum Y value, Minimum Y value and
  // Maximum X value by comparing those of the two subpolygons.
  FMaxY := Max(FSubPolygon1.FMaxY, FSubPolygon2.FMaxY);
  FMinY := Min(FSubPolygon1.FMinY, FSubPolygon2.FMinY);
  FMaxX := Max(FSubPolygon1.FMaxX, FSubPolygon2.FMaxX);
  FMinX := Min(FSubPolygon1.FMinX, FSubPolygon2.FMinX);
end;

procedure TSubPolygon.SetMaxAndMinWhenNoSubPolygons(const Count, Start: Integer;
  const Points: TRealPointArray);
var
  Index: Integer;
  Temp: Real;
begin
  // If the subpolygons are small enough, determine the Maximum Y value,
  // Minimum Y value and Maximum X values directly.
  FMaxX := Points[Start].X;
  FMinX := FMaxX;
  FMaxY := Points[Start].Y;
  FMinY := FMaxY;
  for Index := 1 to Count - 1 do
  begin
    Temp := Points[Start + Index].Y;
    if Temp > FMaxY then
    begin
      FMaxY := Temp;
    end
    else if Temp < FMinY then
    begin
      FMinY := Temp;
    end;
    Temp := Points[Start + Index].X;
    if Temp > FMaxX then
    begin
      FMaxX := Temp;
    end
    else if Temp < FMinX then
    begin
      FMinX := Temp;
    end;
  end;
end;

procedure TSubPolygon.SetPoint(Index: Integer; const Value: TPoint2D);
begin
  FPoints[Index] := Value;
end;

{ TOutline }


constructor TOutline.Create;
begin
  FPolygons :=  TObjectList<TSubPolygon>.Create;
end;

destructor TOutline.Destroy;
begin
  FPolygons.Free;
  inherited;
end;

function TOutline.GetPolygon(Index: Integer): TSubPolygon;
begin
  result := FPolygons[Index];
end;

function TOutline.GetPolygonCount: Integer;
begin
  result := FPolygons.Count;
end;

function TOutline.PointInside(APoint: TPoint2D): boolean;
begin
  result := PointInside(APoint.x, APoint.y);
end;

function TOutline.PointInside(X, Y: Real): boolean;
var
  SubPolygon: TSubPolygon;
  index: Integer;
begin
  result := False;
  for index := 0 to FPolygons.Count - 1 do
  begin
    SubPolygon := FPolygons[index];
    if SubPolygon.IsPointInside(X, Y) then
    begin
      result := not result;
    end;
  end;
end;


end.
