{ This unit provides two classes to perform Natural Neighbor
  interpolation on a set of points.
  @author(Richard B. Winston <rbwinst@usgs.gov>)

  See @html(<a href="http://en.wikipedia.org/wiki/Natural_neighbor">http://en.wikipedia.org/wiki/Natural_neighbor</a>)
  for an explanation of Natural Neighbor interpolation.

  @link(TNaturalNeighborInterpolatorTripack) is faster
  than @link(TNaturalNeighborInterpolator) because it uses a more faster
  (but more complex) algorithm for constructing the Delaunay triangulation.

  Dec. 9, 2010.
  This unit is in the public domain.
 }

unit NatNeigh;

interface

uses Classes, Delaunay, GPC_Classes, Types, QuadTreeClass, TripackTypes,
  SysUtils, Dialogs, FastGEO;

type
  {@name performs Natural Neighbor interpolation on a set of points.

  Usage: The user must first determine the limits in the X and Y directions
  over which interpolation will be performed.  Those limits are passed to
  the constructor along with the X and Y coordinates of the points used for
  interpolation and the values associated with those points.  If the locations
  of the points defines the limits over which interpolation will be performed,
  the second overloaded version of @link(Create) can be used in which only
  the coordinates and the values are passed to the constructor.  @name will
  determine the limits of the points and pass those to the first version of
  @link(Create).  Call @link(interpolate) to find the interpolated value.

  @name is slower
  than @link(TNaturalNeighborInterpolatorTripack).}
  TNaturalNeighborInterpolator = class(TObject)
  private
    FVoronoiPolygons: TList;
    FStartingPoints: array of integer;
    FVoronoi: TVoronoiDiagram;
    FValues: TFloatArray;
    FQuadTree: TRbwQuadTree;
    FEpsilonX: Double;
    FEpsilonY: Double;
    function GetGpcPolygon(DelaunayNodeIndex: integer): TGpcPolygonClass;
    procedure SetTestPolygonGeometry(VertexPositions: TLongIntList;
      Vertices: TVertexList; var TestPolygon: TGpcPolygonClass);
  protected
    procedure FillVoronoiDiagram(const X, Y: TFloatArray;
      XHigh, XLow, YHigh, YLow: Extended); virtual;
    property GpcPolygon[Index: integer]: TGpcPolygonClass read GetGpcPolygon;
  public
    constructor Create(const X, Y, Values: TFloatArray; XLow, YLow, XHigh, YHigh: real); overload;
    constructor Create(const X, Y, Values: TFloatArray); overload;
    destructor Destroy; override;
    function Interpolate(const X, Y: double): double;
    property EpsilonX: Double read FEpsilonX write FEpsilonX;
    property EpsilonY: Double read FEpsilonY write FEpsilonY;
  end;

  {@name is used exactly like @link(TNaturalNeighborInterpolator).

  @name is faster
  than @link(TNaturalNeighborInterpolator).}
  TNaturalNeighborInterpolatorTripack = class(TNaturalNeighborInterpolator)
  protected
    procedure FillVoronoiDiagram(const X, Y: TFloatArray;
      XHigh, XLow, YHigh, YLow: Extended); override;
  end;

procedure GetHighAndLowValuesFromArray(const AnArray: TDoubleDynArray; var HighValue: Extended; var LowValue: Extended); overload;
procedure GetHighAndLowValuesFromArray(const AnArray: TFloatArray; var HighValue: TFloat; var LowValue: TFloat); overload;
implementation

uses
  Contnrs, gpc;

procedure GetHighAndLowValuesFromArray(const AnArray: TDoubleDynArray;
  var HighValue, LowValue: Extended); overload;
var
  Index: Integer;
begin
  Assert(Length(AnArray) > 0);
  LowValue := AnArray[0];
  HighValue := AnArray[0];
  for Index := 1 to Length(AnArray) - 1 do
  begin
    if LowValue > AnArray[Index] then
    begin
      LowValue := AnArray[Index];
    end
    else if HighValue < AnArray[Index] then
    begin
      HighValue := AnArray[Index];
    end;
  end;
end;

procedure GetHighAndLowValuesFromArray(const AnArray: TFloatArray;
  var HighValue: TFloat; var LowValue: TFloat); overload;
var
  Index: Integer;
begin
  Assert(Length(AnArray) > 0);
  LowValue := AnArray[0];
  HighValue := AnArray[0];
  for Index := 1 to Length(AnArray) - 1 do
  begin
    if LowValue > AnArray[Index] then
    begin
      LowValue := AnArray[Index];
    end
    else if HighValue < AnArray[Index] then
    begin
      HighValue := AnArray[Index];
    end;
  end;
end;


{ TNaturalNeighborInterpolator }

constructor TNaturalNeighborInterpolator.Create(
  const X, Y, Values: TFloatArray);
var
  XHigh: TFloat;
  XLow: TFloat;
  YHigh: TFloat;
  YLow: TFloat;
begin
  Assert(Length(X) = Length(Y));
  Assert(Length(X) = Length(Values));
  Assert(Length(X) >= 3);
  GetHighAndLowValuesFromArray(X, XHigh, XLow);
  GetHighAndLowValuesFromArray(Y, YHigh, YLow);
  Create(X, Y, Values, XLow, YLow, XHigh, YHigh);
end;

destructor TNaturalNeighborInterpolator.Destroy;
begin
  FVoronoiPolygons.Free;
  FVoronoi.Free;
  FQuadTree.Free;
  inherited;
end;

constructor TNaturalNeighborInterpolator.Create(const X, Y, Values: TFloatArray;
  XLow, YLow, XHigh, YHigh: real);
var
  VIndex: Integer;
  Vertex: TVoronoiVertex;
  NIndex: Integer;
  Index: Integer;
  NodeNumber: Integer;
  StartingPointCount: Integer;
begin
  FVoronoi := nil;
  Assert(Length(X) = Length(Y));
  Assert(Length(X) = Length(Values));
  Assert(Length(X) >= 3);
  FValues := Values;
  SetLength(FValues, Length(FValues));

  // store the values associated with each point in a QuadTree.
  FQuadTree := TRbwQuadTree.Create(nil);
  FQuadTree.XMax := XHigh;
  FQuadTree.XMin := XLow;
  FQuadTree.YMax := YHigh;
  FQuadTree.YMin := YLow;
  for Index := 0 to Length(X) - 1 do
  begin
    FQuadTree.AddPoint(X[Index], Y[Index], Addr(FValues[Index]));
  end;
  FillVoronoiDiagram(X, Y, XHigh, XLow, YHigh, YLow);
  FVoronoiPolygons := TObjectList.Create;
  FVoronoiPolygons.Capacity := FVoronoi.PointCount;
  SetLength(FStartingPoints, FVoronoi.PointCount);
  for NIndex := 0 to FVoronoi.PointCount - 1 do
  begin
    FStartingPoints[NIndex] := -1;
  end;
  StartingPointCount := 0;
  for VIndex := 0 to FVoronoi.VertexCount - 1 do
  begin
    Vertex := FVoronoi.VoronoiVertices[VIndex];
    for NIndex := 0 to 2 do
    begin
      NodeNumber := Vertex.FormingDelaunayPointIndices[NIndex];
      if (NodeNumber >= 0) and (FStartingPoints[NodeNumber] < 0) then
      begin
        FStartingPoints[NodeNumber] := VIndex;
        Inc(StartingPointCount);
      end;
    end;
    if StartingPointCount = FVoronoi.PointCount then
    begin
      Break;
    end;
  end;
end;

function TNaturalNeighborInterpolator.GetGpcPolygon(
  DelaunayNodeIndex: integer): TGpcPolygonClass;
var
  VList: TVertexList;
  TestedVerticies: TLongIntList;
  StartingPointIndex: Integer;
  AVertex: TVoronoiVertex;
  FoundAPoint: Boolean;
  VIndex: integer;
  NeighborIndex: longint;
  NeighborVertex: TVoronoiVertex;
  NIndex: integer;
  GpcVertex: Tgpc_vertex;
begin
  Assert(DelaunayNodeIndex >= 0);
  while DelaunayNodeIndex >= FVoronoiPolygons.Count do
  begin
    FVoronoiPolygons.Add(nil);
  end;
  result := FVoronoiPolygons[DelaunayNodeIndex];
  if result = nil then
  begin
    VList := TVertexList.Create;
    try
      StartingPointIndex := FStartingPoints[DelaunayNodeIndex];
      AVertex := FVoronoi.VoronoiVertices[StartingPointIndex];
      TestedVerticies := TLongIntList.Create;
      try
        VList.Add(AVertex);
        TestedVerticies.Add(StartingPointIndex);
        repeat
          FoundAPoint := False;
          for VIndex := 0 to 2 do
          begin
            NeighborIndex := AVertex.NeighborVoronoiVertexIndices[VIndex];
            if (NeighborIndex >= 0)
              and (TestedVerticies.IndexOf(NeighborIndex) < 0) then
            begin
              NeighborVertex := FVoronoi.VoronoiVertices[NeighborIndex];
              for NIndex := 0 to 2 do
              begin
                if NeighborVertex.FormingDelaunayPointIndices[NIndex] = DelaunayNodeIndex then
                begin
                  FoundAPoint := True;
                  AVertex := NeighborVertex;
                  VList.Add(AVertex);
                  break;
                end;
              end;
              TestedVerticies.Add(NeighborIndex);
              if FoundAPoint then
              begin
                break;
              end;
            end;
          end;
        until (not FoundAPoint);
      finally
        TestedVerticies.Free;
      end;
      result := TGpcPolygonClass.Create;
      FVoronoiPolygons[DelaunayNodeIndex] := result;
      result.NumberOfContours := 1;
      result.VertexCount[0] := VList.Count;
      result.Holes[0] := False;
      for VIndex := 0 to VList.Count - 1 do
      begin
        AVertex := VList.Items[VIndex];
        GpcVertex.x := AVertex.x;
        GpcVertex.y := AVertex.y;
        result.Vertices[0,VIndex] := GpcVertex;
      end;
    finally
      VList.Free;
    end;
  end;
end;

function TNaturalNeighborInterpolator.Interpolate(const X, Y: double): double;
var
  APoint: TDelaunayPoint;
  VertexPositions: TLongIntList;
  Vertices: TVertexList;
  VoronoiVertex: TVoronoiVertex;
  TestPolygon: TGpcPolygonClass;
  VIndex: Integer;
  TestedPoints: TLongIntList;
  DIndex: Integer;
  PointIndex: Integer;
  Sum: Double;
  WeightedSum: Double;
  PointPolygon: TGpcPolygonClass;
  IntersectionPolygon: TGpcPolygonClass;
  Area: double;
  ContourIndex: Integer;
  NewX: Double;
  NewY: Double;
  ValuePointers: TPointerArray;
  function NearlyTheSame(const A, B, Epsilon: real): boolean;
  begin
    result := A = B;
    if not result then
    begin
      result := Abs(A - B) < Epsilon;
    end;
  end;
begin
  NewX := X;
  NewY := Y;
  FQuadTree.FindClosestPointsData(NewX, NewY, ValuePointers);
  if NearlyTheSame(X, NewX, EpsilonX) and NearlyTheSame(Y, NewY, EpsilonY) then
  begin
    // If the interpolation point is at the same location
    // as one of the original points, use the value associated with
    // the original points.
    result := PDouble(ValuePointers[0])^;
    Exit;
  end;

  VertexPositions := TLongIntList.Create;
  Vertices := TVertexList.Create;
  try
    APoint.x := X;
    APoint.y := Y;
    FVoronoi.CheckNewPoint(APoint, VertexPositions, Vertices);

    TestPolygon := TGpcPolygonClass.Create;
    try
      SetTestPolygonGeometry(VertexPositions, Vertices, TestPolygon);

      Sum := 0;
      WeightedSum := 0;
      TestedPoints := TLongIntList.Create;
      try
        TestedPoints.Add(FVoronoi.PointCount);
        for VIndex := 0 to Vertices.Count - 1 do
        begin
          VoronoiVertex := Vertices[VIndex];
          for DIndex := 0 to 2 do
          begin
            PointIndex := VoronoiVertex.FormingDelaunayPointIndices[DIndex];
            if (PointIndex >= 4) and (TestedPoints.IndexOf(PointIndex) < 0) then
            begin
              PointPolygon := GpcPolygon[PointIndex];
              IntersectionPolygon := TGpcPolygonClass.
                CreateFromOperation(GPC_INT, TestPolygon, PointPolygon);
              try
                Area := 0;
                for ContourIndex := 0 to IntersectionPolygon.NumberOfContours - 1 do
                begin
                  if IntersectionPolygon.Holes[ContourIndex] then
                  begin
                    Area := Area - Abs(IntersectionPolygon.ContourArea(ContourIndex));
                  end
                  else
                  begin
                    Area := Area + Abs(IntersectionPolygon.ContourArea(ContourIndex));
                  end;
                end;
                Sum := Sum + Area;
                WeightedSum := WeightedSum + Area * FValues[PointIndex-4];
              finally
                IntersectionPolygon.Free;
              end;
              TestedPoints.Add(PointIndex);
            end;
          end;
        end;
      finally
        TestedPoints.Free;
      end;
      if Sum = 0 then
      begin
        result := 0
      end
      else
      begin
        result := WeightedSum/Sum;
      end;
    finally
      TestPolygon.Free;
    end;
  finally
    Vertices.Free;
    VertexPositions.Free;
  end;
end;

procedure TNaturalNeighborInterpolator.FillVoronoiDiagram(
  const X, Y: TFloatArray; XHigh, XLow, YHigh, YLow: Extended);
var
  Index: Integer;
  Delta: Extended;
  APoint: TDelaunayPoint;
  Epsilon: double;
begin
  Delta := XHigh - XLow;
  XLow := XLow - Delta;
  XHigh := XHigh + Delta;
  Delta := YHigh - YLow;
  YLow := YLow - Delta;
  YHigh := YHigh + Delta;
  Epsilon := GetEpsilon(Y, X);

  FVoronoi := TVoronoiDiagram.Create(XLow, YLow, XHigh, YHigh, Epsilon);
  for Index := 0 to Length(X) - 1 do
  begin
    APoint.x := X[Index];
    APoint.Y := Y[Index];
    FVoronoi.AddPoint(APoint);
  end;
end;

procedure TNaturalNeighborInterpolator.SetTestPolygonGeometry(
  VertexPositions: TLongIntList; Vertices: TVertexList;
  var TestPolygon: TGpcPolygonClass);
var
  StartIndex: Integer;
  VoronoiVertex: TVoronoiVertex;
  GpcVertex: Tgpc_vertex;
  TestedIndices: TLongIntList;
  PositionIndex: Integer;
  FoundPoint: Boolean;
  VIndex: Integer;
  VertexIndex: Integer;
  VPosition: Integer;
begin
  TestPolygon.NumberOfContours := 1;
  TestPolygon.VertexCount[0] := VertexPositions.Count;
  TestPolygon.Holes[0] := False;
  StartIndex := VertexPositions[0];
  VoronoiVertex := Vertices[0];
  GpcVertex.x := VoronoiVertex.x;
  GpcVertex.y := VoronoiVertex.y;
  TestPolygon.Vertices[0, 0] := GpcVertex;
  TestedIndices := TLongIntList.Create;
  try
    TestedIndices.Add(StartIndex);
    PositionIndex := 0;
    repeat
      FoundPoint := False;
      for VIndex := 0 to 2 do
      begin
        VertexIndex := VoronoiVertex.NeighborVoronoiVertexIndices[VIndex];
        if TestedIndices.IndexOf(VertexIndex) < 0 then
        begin
          VPosition := VertexPositions.IndexOf(VertexIndex);
          if VPosition >= 0 then
          begin
            FoundPoint := True;
            VoronoiVertex := Vertices[VPosition];
            TestedIndices.Add(VertexIndex);
            GpcVertex.x := VoronoiVertex.x;
            GpcVertex.y := VoronoiVertex.y;
            Inc(PositionIndex);
            Assert(PositionIndex < VertexPositions.Count);
            TestPolygon.Vertices[0, PositionIndex] := GpcVertex;
            break;
          end;
        end;
      end;
    until not FoundPoint;
    Assert(PositionIndex = VertexPositions.Count - 1);
  finally
    TestedIndices.Free;
  end;
end;

{ TNaturalNeighborInterpolatorTripack }

procedure TNaturalNeighborInterpolatorTripack.FillVoronoiDiagram(const X,
  Y: TFloatArray; XHigh, XLow, YHigh, YLow: Extended);
var
  Delta: Extended;
  XArray, YArray: TNmaxSingleArray;
  Index: Integer;
begin
//  inherited;
  Delta := XHigh - XLow;
  XLow := XLow - Delta;
  XHigh := XHigh + Delta;
  Delta := YHigh - YLow;
  YLow := YLow - Delta;
  YHigh := YHigh + Delta;
  SetLength(XArray, Length(X) + 4);
  SetLength(YArray, Length(Y) + 4);
  XArray[0] := XLow;
  XArray[1] := XHigh;
  XArray[2] := XHigh;
  XArray[3] := XLow;
  YArray[0] := YLow;
  YArray[1] := YLow;
  YArray[2] := YHigh;
  YArray[3] := YHigh;
  for Index := 0 to Length(X) - 1 do
  begin
    XArray[Index+4] := X[Index];
    YArray[Index+4] := Y[Index];
  end;
  GetVoronoiDiagram(FVoronoi, YArray, XArray);
end;

end.
