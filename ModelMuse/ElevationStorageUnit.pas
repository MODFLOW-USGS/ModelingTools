{@name defines @link(TElevationStorage).}
unit ElevationStorageUnit;

interface

uses Classes, GoPhastTypes, FastGEO;

type
  {
   @name is intended to store a series of points that represent
   upper or lower "elevations" of a @link(TScreenObject)
   perpendicular to a @link(TScreenObject) whose
   @link(TScreenObject.ViewDirection) is @link(TViewDirection vdFront)
   or @link(TViewDirection vdSide).

   @classname is used in MODFLOW models to define the "upper" and "lower" surfaces
   of 3D objects drawn on the front or side views of the model.  It is needed
   on those views because the layers may not be flat.  Both row and column
   boundaries are vertical planes so @classname isn't need for the top view.
  }
  TElevationStorage = class(TObject)
  strict private
    // @name is the maximum size of the first dimension of @link(Elevations).
    // @name would normally be the number of rows for the front view
    // or the number of columns for the side view.
    FFirstCapacity: integer;
    // @name is the maximum size of the second dimension of @link(Elevations).
    // @name would normally be
    // the number of columns times the number of layers for the front view
    // or the number of rows times the number of layers for the side view.
    FSecondCapacity: integer;
    // @name records how many items are stored for each row or column in
    // FElevations
    FSecondCount: array of integer;
    // @name is used to store the elevations internally.
    // See @link(Elevations);
    FElevations: T3DRealPointArray2;
    // @name is used to report the elevations to consumers.
    // The elevations are sorted before being reported.
    // See @link(Elevations);
    FCachedElevations: T3DRealPointArray2;
    // @name is used to record whether @link(FCachedElevations) are up-to-date
    // or whether they need to be recalculated.
    FElevationsUpToDate: boolean;
  private
    // See @link(Elevations)
    function GetElevations: T3DRealPointArray2;
    // @name tests whether OtherPoint and Point are different.
    function PointsAreDifferent(const OtherPoint, Point: TPoint3D): boolean;
    // @name updates @link(FCachedElevations).
    procedure UpdateCache;
  public
    // @name adds a point on the front or side view of the @link(TScreenObject).
    // @param(FirstIndex is the
    // row or column number of the location being added.)
    // @param(Point is the location being added.)
    procedure AddPoint(const FirstIndex: integer; const Point: T3DRealPoint);
    // @name clears all the elevations in the @classname.
    procedure Clear;
    // @name are the "elevations" on the front or side views.
    // The first index is a row or column number. The points in each
    // row or column will be sorted in ascending order.
    // @name will be recalculated if required.
    property Elevations: T3DRealPointArray2 read GetElevations;
    // @name sets the maximum size of @link(Elevations).
    procedure SetCapacities(const FirstCapacity, SecondCapacity: integer);
  end;

implementation

uses Math;

procedure TElevationStorage.AddPoint(const FirstIndex: integer; const Point: T3DRealPoint);
var
  SecondIndex: integer;
  PointsDifferent: boolean;
  OtherPoint: T3DRealPoint;
begin
  FElevationsUpToDate := False;
  Assert((FirstIndex >= 0) and (FirstIndex < FFirstCapacity));
  SecondIndex := FSecondCount[FirstIndex];
  Assert((SecondIndex >= 0) and (FirstIndex < FSecondCapacity));
  if SecondIndex > 0 then
  begin
    OtherPoint := FElevations[FirstIndex,SecondIndex-1];
    PointsDifferent := PointsAreDifferent(OtherPoint, Point);
  end
  else
  begin
    PointsDifferent := true;
  end;
  if PointsDifferent then
  begin
    FElevations[FirstIndex,SecondIndex] := Point;
    Inc(FSecondCount[FirstIndex]);
  end;
end;

function TElevationStorage.PointsAreDifferent(const OtherPoint, Point: T3DRealPoint): boolean;
begin
  result := (Point.X <> OtherPoint.X)
    or (Point.Y <> OtherPoint.Y)
    or (Point.Z <> OtherPoint.Z);
end;

procedure TElevationStorage.Clear;
var
  Index: Integer;
begin
  FElevationsUpToDate := False;
  for Index := 0 to FFirstCapacity - 1 do
  begin
    FSecondCount[Index] := 0;
  end;
end;

function TElevationStorage.GetElevations: T3DRealPointArray2;
begin
  if not FElevationsUpToDate then
  begin
    UpdateCache;
  end;
  result := FCachedElevations;
end;

procedure TElevationStorage.SetCapacities(const FirstCapacity, SecondCapacity: integer);
begin
  FElevationsUpToDate := False;
  FFirstCapacity := FirstCapacity;
  FSecondCapacity := SecondCapacity;
  SetLength(FSecondCount, FFirstCapacity);
  SetLength(FElevations, FFirstCapacity, FSecondCapacity);
end;

function ZPointCompare(Item1, Item2: Pointer): Integer;
var
  PPoint1, PPoint2: P3DRealPoint;
begin
  PPoint1 := Item1;
  PPoint2 := Item2;
  result := Sign(PPoint1^.Z - PPoint2^.Z);
end;

procedure TElevationStorage.UpdateCache;
var
  PointSortList: TList;
  Index: Integer;
  PointIndex: Integer;
  Point1: T3DRealPoint;
  Point2: T3DRealPoint;
  CacheIndex: Integer;
  TempPoints: T3DRealPointArray1;
begin
  PointSortList := TList.Create;
  try
    for Index := 0 to FFirstCapacity - 1 do
    begin
      PointSortList.Clear;
      for PointIndex := 0 to FSecondCount[Index] - 1 do
      begin
        PointSortList.Add(Addr(FElevations[Index,PointIndex]));
      end;
      if PointSortList.Count > 0 then
      begin
        PointSortList.Sort(ZPointCompare);
        for PointIndex := PointSortList.Count - 1 downto 1 do
        begin
          Point1 := P3DRealPoint(PointSortList[PointIndex])^;
          Point2 := P3DRealPoint(PointSortList[PointIndex-1])^;
          if not PointsAreDifferent(Point1, Point2) then
          begin
            PointSortList.Delete(PointIndex);
          end;
        end;
        SetLength(TempPoints, PointSortList.Count);
        for PointIndex := 0 to PointSortList.Count - 1 do
        begin
          TempPoints[PointIndex] := P3DRealPoint(PointSortList[PointIndex])^;
        end;
        FElevations[Index] := TempPoints;
        FSecondCount[Index] := PointSortList.Count;
      end;
    end;
    CacheIndex := 0;
    SetLength(FCachedElevations,FFirstCapacity);
    for Index := 0 to FFirstCapacity - 1 do
    begin
      if FSecondCount[Index] > 0 then
      begin
        FCachedElevations[CacheIndex] := FElevations[Index];
        SetLength(FCachedElevations[CacheIndex], FSecondCount[Index]);
        Inc(CacheIndex);
      end;
    end;
    SetLength(FCachedElevations,CacheIndex);
    FElevationsUpToDate := True;
  finally
    PointSortList.Free;
  end;
end;

end.
