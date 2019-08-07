unit RasterValuesAlongSegmentsUnit;

interface

uses
  FastGEO, QuadtreeClass;

type
  ISegments = interface(IUnknown)
    function Count: Integer;
    function GetSegment(Index: Integer): TSegment2D;
    property Segments[Index: Integer]: TSegment2D read GetSegment;
  end;

  IRaster = interface(IUnknown)
    function GetLowerLeft: TPoint2D;
    function GetXCount: integer;
    function GetYCount: integer;
    function GetXSpacing: Double;
    function GetYSpacing: Double;
    function GetZ(XIndex, YIndex: Integer): Double;
    function GetIgnore(XIndex, YIndex: Integer): Boolean;
    // @name is the lower left corner of the grid. The data point for the
    // lower left cells is at
    // (LowerLeft.x + (XSpacing/2), LowerLeft.y + (YSpacing/2)).
    // ESRI ASCII grid files follow this format. Surfer Grid files need to
    // be adjusted because they give the coordinates of the cell centers
    // directly.
    property LowerLeft: TPoint2D read GetLowerLeft;
    property XCount: Integer read GetXCount;
    property YCount: Integer read GetYCount;
    property XSpacing: Double read GetXSpacing;
    property YSpacing: Double read GetYSpacing;
    // The coordinate of the point associated with Z[XIndex, YIndex]
    // is LowerLeft.x - XSpacing/2 + XSpacing*XIndex,
    // LowerLeft.Y - YSpacing/2 + YSpacing*YIndex,
    // Which places it at the center of a rectanglar cell.
    // ESRI ASCII grid files start at the upper left instead of the
    // lower left and therefore need to be adjusted to follow this format.
    property Z[XIndex, YIndex: Integer]: double read GetZ;
    property Ignore[XIndex, YIndex: Integer]: Boolean read GetIgnore;
  end;

  IRasterFlush = interface(IRaster )
    ['{E138A3E0-4DA5-43C9-B246-6F8BCB1DAEB4}']
    procedure Flush;
    procedure SetZ(XIndex, YIndex: integer; const Value: Double);
    property Z[XIndex, YIndex: Integer]: double read GetZ write SetZ;
  end;

  IRasterFile = interface(IRasterFlush )
    ['{D0217423-5A0D-4C69-8488-D58E6C602BDB}']
    function GetFileName: string;
    property FileName: string read GetFileName;
  end;



type
  TSegmentValues = array of Double;


function GetLineValueFromRaster(ALine: TPolyLine2D; Raster: IRaster): double;
function GetSegmentValueFromRaster(ASegment: TSegment2D; Raster: IRaster): double;

function GetSegmentSlopeFromRaster(ASegment: TSegment2D; Raster: IRaster): double;

function GetRasterValuesAlongSegments(Segments: ISegments;
  Raster: IRaster; out SegmentValues: TSegmentValues): boolean;

implementation

uses
  System.Math;

var QuadTree: TRbwQuadTree;

function GetIndex(const X, MixX, MaxX: double; const Count: integer): Integer;
begin
  if X < MixX then
  begin
    result := 0
  end
  else if X > MaxX then
  begin
    result := Count-1;
  end
  else
  begin
    result := Trunc((X - MixX)/(MaxX-MixX)*Count);
    if result >= Count then
    begin
      result := Count-1;
    end;
  end;
end;

procedure LowestFirst(var Val1, Val2: Integer);
var
  Temp: Integer;
begin
  if Val2 < Val1 then
  begin
    Temp := Val2;
    Val2 := Val1;
    Val1 := Temp;
  end;
end;

function GetSegmentSlopeFromRaster(ASegment: TSegment2D; Raster: IRaster): double;
var
  UpperRight: TPoint2D;
  StartXIndex: Integer;
  EndXIndex: Integer;
  StartYIndex: Integer;
  EndYIndex: Integer;
  Value: double;
  Cell: TRectangle;
  XIndex: Integer;
  YIndex: Integer;
  TestAll: Boolean;
  StartValue: Double;
  EndValue: Double;
  SegmentLength: double;
  Values: array of array of Double;
  DoublePointer : PDouble;
begin
  SegmentLength := Distance(ASegment[1], ASegment[2]);
  if SegmentLength = 0 then
  begin
    result := 0;
    Exit;
  end;

  UpperRight.x := Raster.LowerLeft.x + Raster.XSpacing*Raster.XCount;
  UpperRight.y := Raster.LowerLeft.y + Raster.YSpacing*Raster.YCount;

  Cell[1] := Raster.LowerLeft;
  Cell[2] := UpperRight;
  if not Intersect(ASegment, Cell) then
  begin
    result := 0;
    Exit;
  end;

  StartXIndex := GetIndex(ASegment[1].X, Raster.LowerLeft.x,
    UpperRight.x, Raster.XCount);
  EndXIndex := GetIndex(ASegment[2].X, Raster.LowerLeft.x,
    UpperRight.x, Raster.XCount);

  StartYIndex := GetIndex(ASegment[1].y, Raster.LowerLeft.y,
    UpperRight.y, Raster.YCount);
  EndYIndex := GetIndex(ASegment[2].y, Raster.LowerLeft.y,
    UpperRight.y, Raster.YCount);

  Cell[1].x := Raster.LowerLeft.x + Raster.XSpacing*StartXIndex;
  Cell[2].x := Raster.LowerLeft.x + Raster.XSpacing*(StartXIndex+1);
  Cell[1].y := Raster.LowerLeft.y + Raster.YSpacing*StartYIndex;
  Cell[2].y := Raster.LowerLeft.y + Raster.YSpacing*(StartYIndex+1);

  StartValue := 0;
  TestAll := False;
  if Intersect(ASegment, Cell) then
  begin
    if Raster.Ignore[StartXIndex,StartYIndex] then
    begin
      TestAll := True;
    end
    else
    begin
      StartValue := Raster.Z[StartXIndex,StartYIndex];
    end;
  end;

  Cell[1].x := Raster.LowerLeft.x + Raster.XSpacing*EndXIndex;
  Cell[2].x := Raster.LowerLeft.x + Raster.XSpacing*(EndXIndex+1);
  Cell[1].y := Raster.LowerLeft.y + Raster.YSpacing*EndYIndex;
  Cell[2].y := Raster.LowerLeft.y + Raster.YSpacing*(EndYIndex+1);

  EndValue := 0;
  if Intersect(ASegment, Cell) then
  begin
    if Raster.Ignore[EndXIndex,EndYIndex] then
    begin
      TestAll := True;
    end
    else
    begin
      EndValue := Raster.Z[EndXIndex,EndYIndex];
    end;
  end;

  if not TestAll then
  begin
    Result := (StartValue-EndValue)/SegmentLength;
    Exit;
  end;

  LowestFirst(StartXIndex,EndXIndex);
  Assert(StartXIndex >= 0);
  Assert(EndXIndex <= Raster.XCount);

  LowestFirst(StartYIndex,EndYIndex);
  Assert(StartYIndex >= 0);
  Assert(EndYIndex <= Raster.YCount);

  QuadTree.Clear;
  QuadTree.XMin := Min(ASegment[1].x, ASegment[2].x);
  QuadTree.XMax := Max(ASegment[1].x, ASegment[2].x);
  QuadTree.YMin := Min(ASegment[1].y, ASegment[2].y);
  QuadTree.YMax := Max(ASegment[1].y, ASegment[2].y);

  SetLength(Values, EndXIndex-StartXIndex+1, EndYIndex-StartYIndex+1);

  for XIndex := StartXIndex to EndXIndex do
  begin
    Cell[1].x := Raster.LowerLeft.x + Raster.XSpacing*XIndex;
    Cell[2].x := Raster.LowerLeft.x + Raster.XSpacing*(XIndex+1);
    for YIndex := StartYIndex to EndYIndex do
    begin
      Cell[1].y := Raster.LowerLeft.y + Raster.YSpacing*YIndex;
      Cell[2].y := Raster.LowerLeft.y + Raster.YSpacing*(YIndex+1);
      if Intersect(ASegment, Cell) then
      begin
        if not Raster.Ignore[XIndex,YIndex] then
        begin
          Value := Raster.Z[XIndex,YIndex];
          Values[XIndex-StartXIndex, YIndex-StartYIndex] := Value;
          QuadTree.AddPoint((Cell[1].x + Cell[2].x)/2,(Cell[1].y + Cell[2].y)/2,
            Addr(Values[XIndex-StartXIndex, YIndex-StartYIndex]));
        end;
      end;
    end;
  end;

  if QuadTree.Count = 0 then
  begin
    result := 0;
    Exit;
  end;

  DoublePointer := QuadTree.NearestPointsFirstData(ASegment[1].x, ASegment[1].y);
  StartValue := DoublePointer^;
  DoublePointer := QuadTree.NearestPointsFirstData(ASegment[2].x, ASegment[2].y);
  EndValue := DoublePointer^;
  Result := (StartValue-EndValue)/SegmentLength;

end;

function GetLineValueFromRaster(ALine: TPolyLine2D; Raster: IRaster): double;
var
  UpperRight: TPoint2D;
  StartXIndex: Integer;
  EndXIndex: Integer;
  StartYIndex: Integer;
  EndYIndex: Integer;
  Count: Integer;
  Value: double;
  Cell: TRectangle;
  XIndex: Integer;
  YIndex: Integer;
  IgnoreValue: double;
  PointIndex: Integer;
  function Intersect(ALine: TPolyLine2D; Cell: TRectangle): boolean;
  var
    PointIndex: integer;
  begin
    result := False;
    for PointIndex := 0 to Length(ALine) - 2 do
    begin
      Result := FastGEO.Intersect(EquateRectangle(ALine[PointIndex],
        ALine[PointIndex+1]), Cell);
      if result then
      begin
        break
      end;
    end;
  end;
begin
  if Length(ALine) = 0 then
  begin
    result := 0;
    Exit;
  end;
  UpperRight.x := Raster.LowerLeft.x + Raster.XSpacing*Raster.XCount;
  UpperRight.y := Raster.LowerLeft.y + Raster.YSpacing*Raster.YCount;

  if Length(ALine) = 1 then
  begin
    if (ALine[0].x < Raster.LowerLeft.x)
      or (ALine[0].x > UpperRight.x)
      or (ALine[0].y < Raster.LowerLeft.y)
      or (ALine[0].y > UpperRight.y) then
    begin
      result := 0;
      Exit;
    end;
  end;
  IgnoreValue := 0;

  StartXIndex := GetIndex(ALine[0].x, Raster.LowerLeft.x,
    UpperRight.x, Raster.XCount);
  EndXIndex := StartXIndex;
  StartYIndex := GetIndex(ALine[0].y, Raster.LowerLeft.y,
    UpperRight.y, Raster.YCount);
  EndYIndex := StartYIndex;
  for PointIndex := 1 to Length(ALine) - 1 do
  begin
    XIndex := GetIndex(ALine[PointIndex].X, Raster.LowerLeft.x,
      UpperRight.x, Raster.XCount);
    StartXIndex := Min(StartXIndex, XIndex);
    EndXIndex := Max(EndXIndex, XIndex);
    YIndex := GetIndex(ALine[PointIndex].y, Raster.LowerLeft.y,
      UpperRight.y, Raster.YCount);
    StartYIndex := Min(StartYIndex, YIndex);
    EndYIndex := Max(EndYIndex, YIndex);
  end;

  if Length(ALine) = 1 then
  begin
    result := Raster.Z[StartXIndex,StartYIndex];
    Exit;
  end;

  Count := 0;
  Value := 0;
  Cell[1] := Raster.LowerLeft;
  Cell[2] := UpperRight;
  if Intersect(ALine, Cell) then
  begin
    for XIndex := StartXIndex to EndXIndex do
    begin
      Cell[1].x := Raster.LowerLeft.x + Raster.XSpacing*XIndex;
      Cell[2].x := Raster.LowerLeft.x + Raster.XSpacing*(XIndex+1);
      for YIndex := StartYIndex to EndYIndex do
      begin
        Cell[1].y := Raster.LowerLeft.y + Raster.YSpacing*YIndex;
        Cell[2].y := Raster.LowerLeft.y + Raster.YSpacing*(YIndex+1);
        if Intersect(ALine, Cell) then
        begin
          if Raster.Ignore[XIndex,YIndex] then
          begin
            IgnoreValue := Raster.Z[XIndex,YIndex];
          end
          else
          begin
            Value := Value + Raster.Z[XIndex,YIndex];
            Inc(Count);
          end;
        end;
      end;
    end;
    if Count > 0 then
    begin
      Value := Value/Count;
    end
    else
    begin
      Value := IgnoreValue;
    end;
  end;
  result := Value;
end;

function GetSegmentValueFromRaster(ASegment: TSegment2D; Raster: IRaster): double;
var
  UpperRight: TPoint2D;
  StartXIndex: Integer;
  EndXIndex: Integer;
  StartYIndex: Integer;
  EndYIndex: Integer;
  Count: Integer;
  Value: double;
  Cell: TRectangle;
  XIndex: Integer;
  YIndex: Integer;
  IgnoreValue: double;
begin
  UpperRight.x := Raster.LowerLeft.x + Raster.XSpacing*Raster.XCount;
  UpperRight.y := Raster.LowerLeft.y + Raster.YSpacing*Raster.YCount;
  IgnoreValue := 0;

  StartXIndex := GetIndex(ASegment[1].X, Raster.LowerLeft.x,
    UpperRight.x, Raster.XCount);
  EndXIndex := GetIndex(ASegment[2].X, Raster.LowerLeft.x,
    UpperRight.x, Raster.XCount);
  LowestFirst(StartXIndex,EndXIndex);
  Assert(StartXIndex >= 0);
  Assert(EndXIndex <= Raster.XCount);

  StartYIndex := GetIndex(ASegment[1].y, Raster.LowerLeft.y,
    UpperRight.y, Raster.YCount);
  EndYIndex := GetIndex(ASegment[2].y, Raster.LowerLeft.y,
    UpperRight.y, Raster.YCount);
  LowestFirst(StartYIndex,EndYIndex);
  Assert(StartYIndex >= 0);
  Assert(EndYIndex <= Raster.YCount);

  Count := 0;
  Value := 0;
  Cell[1] := Raster.LowerLeft;
  Cell[2] := UpperRight;
  if Intersect(ASegment, Cell) then
  begin
    for XIndex := StartXIndex to EndXIndex do
    begin
      Cell[1].x := Raster.LowerLeft.x + Raster.XSpacing*XIndex;
      Cell[2].x := Raster.LowerLeft.x + Raster.XSpacing*(XIndex+1);
      for YIndex := StartYIndex to EndYIndex do
      begin
        Cell[1].y := Raster.LowerLeft.y + Raster.YSpacing*YIndex;
        Cell[2].y := Raster.LowerLeft.y + Raster.YSpacing*(YIndex+1);
        if Intersect(ASegment, Cell) then
        begin
          if Raster.Ignore[XIndex,YIndex] then
          begin
            IgnoreValue := Raster.Z[XIndex,YIndex];
          end
          else
          begin
            Value := Value + Raster.Z[XIndex,YIndex];
            Inc(Count);
          end;
        end;
      end;
    end;
    if Count > 0 then
    begin
      Value := Value/Count;
    end
    else
    begin
      Value := IgnoreValue;
    end;
  end;
  result := Value;
end;

function GetRasterValuesAlongSegments(Segments: ISegments;
  Raster: IRaster; out SegmentValues: TSegmentValues): boolean;
var
  SegmentIndex: Integer;
  ASegment: TSegment2D;
  UpperRight: TPoint2D;
  StartXIndex: Integer;
  EndXIndex: Integer;
  XIndex: Integer;
  Count: Integer;
  Value: double;
  YIndex: Integer;
  StartYIndex: Integer;
  EndYIndex: Integer;
  Cell: TRectangle;
  IgnoreValue: double;
begin
  Result := True;
  IgnoreValue := 0;
  SetLength(SegmentValues, Segments.Count);
  UpperRight.x := Raster.LowerLeft.x + Raster.XSpacing*Raster.XCount;
  UpperRight.y := Raster.LowerLeft.y + Raster.XSpacing*Raster.YCount;
  for SegmentIndex := 0 to Segments.Count - 1 do
  begin
    ASegment := Segments.Segments[SegmentIndex];
    StartXIndex := GetIndex(ASegment[1].X, Raster.LowerLeft.x,
      UpperRight.x, Raster.XCount);
    EndXIndex := GetIndex(ASegment[2].X, Raster.LowerLeft.x,
      UpperRight.x, Raster.XCount);
    LowestFirst(StartXIndex,EndXIndex);

    StartYIndex := GetIndex(ASegment[1].y, Raster.LowerLeft.y,
      UpperRight.y, Raster.YCount);
    EndYIndex := GetIndex(ASegment[2].y, Raster.LowerLeft.y,
      UpperRight.y, Raster.YCount);
    LowestFirst(StartYIndex,EndYIndex);

    Count := 0;
    Value := 0;
    for XIndex := StartXIndex to EndXIndex do
    begin
      Cell[1].x := Raster.LowerLeft.x + Raster.XSpacing*XIndex;
      Cell[2].x := Raster.LowerLeft.x + Raster.XSpacing*(XIndex+1);
      for YIndex := StartYIndex to EndXIndex do
      begin
        Cell[1].y := Raster.LowerLeft.y + Raster.YSpacing*YIndex;
        Cell[2].y := Raster.LowerLeft.y + Raster.YSpacing*(YIndex+1);
        if Intersect(ASegment, Cell) then
        begin
          if Raster.Ignore[XIndex,YIndex] then
          begin
            IgnoreValue := Raster.Z[XIndex,YIndex];
          end
          else
          begin
            Value := Value + Raster.Z[XIndex,YIndex];
            Inc(Count);
          end;
        end;
      end;
    end;
    if Count > 0 then
    begin
      Value := Value/Count;
    end
    else
    begin
      result := False;
      Value := IgnoreValue;
    end;
    SegmentValues[SegmentIndex] := Value;
  end;
end;

initialization
  QuadTree := TRbwQuadTree.Create(nil);

finalization
  QuadTree.Free;

end.
