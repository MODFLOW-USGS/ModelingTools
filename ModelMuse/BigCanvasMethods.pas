{
@abstract(@name provides methods that can be used to draw on a TBitmap32
when the coordinates used for drawing are outside the range of a smallint.)
}
unit BigCanvasMethods;


interface

uses
  Types, SysUtils, Classes, Graphics,
  GR32, // TBitmap32 and TFloatRect are declared in GR32.
  GoPhastTypes, GR32_Polygons;

{@abstract(@name draws a polyline on BitMap.)
 Points holds the locations for drawing
 the polyline.  StartIndex is the position within Points where the line
 should be started.  NumPts is the number of points to include in the polyline.
 If NumPts is less than zero, the polyline will include all points from
 StartIndex to the end of the Points array.
 As currently used, FirstLine is always called with a value of @True.
 If UseStipple is @true and LineThickness is 1, the polyline will be dashed.}
procedure DrawBigPolyline32Old(const BitMap: TBitmap32; const Color: TColor32;
  const LineThickness: single;
  const Points: array of TPoint; FirstLine: boolean; UseStipple: boolean = False;
  StartIndex: Integer = 0; NumPts: Integer = -1);
procedure DrawPolylineCanvas(const Canvas: TCanvas; const Color: TColor;
  const Points: array of TPoint; StartIndex: Integer = 0; NumPts: Integer = -1);
procedure DrawBigPolyline32(const Graphic: TPersistent; const Color: TColor32;
  const LineThickness: single;
  const Points: array of TPoint; FirstLine: boolean; UseStipple: boolean = False;
  StartIndex: Integer = 0; NumPts: Integer = -1);

procedure DrawBigTextBitmap32(const BitMap32: TBitmap32; const Location: TPoint;
  const Text: string; const Font: TFont);
procedure DrawBigTextCanvas(const Canvas: TCanvas; const Location: TPoint;
  const Text: string; const Font: TFont);
procedure DrawBigText(const Graphic: TPersistent; const Location: TPoint;
  const Text: string; Font: TFont = nil);


{@abstract(@name draws a polygon on BitMap.)
 Points holds the locations for drawing
 the polygon.  StartIndex is the position within Points where the line
 should be started.  NumPts is the number of points to include in the polygon.
 If NumPts is less than zero, the polyline will include all points from
 StartIndex to the end of the Points array.
 If UseStipple is @true and LineThickness is 1, the polyline will be dashed.

 If the polygon consists of multiple parts, each part is drawn with a call to
 @name and for the final part, PolygonComplete should be set to @true.
 if MultiplePolygons is @true, Polygon will be created if it is nil and returned
 to the calling routine for use in the next call to @name.  The calling routine
 is responsible for destroying Polygon after the last call to @name for that
 polygon.}
procedure DrawBigPolygon32Old(const BitMap: TBitmap32;
  const OutlineColor, FillColor: TColor32; const LineThickness: single;
  Points: TPointArray; var Polygon: TPolygon32;
  const MultiplePolygons, PolygonComplete: boolean;
  UseStipple: boolean = false;
  StartIndex: Integer = 0; NumPts: Integer = -1); overload;
procedure DrawCanvasePolygon(const Canvas: TCanvas;
  const OutlineColor, FillColor: TColor;
  Points: TPointArray;
  StartIndex: Integer = 0; NumPts: Integer = -1);
procedure DrawBigPolygon32(const Graphic: TPersistent;
  const OutlineColor, FillColor: TColor32; const LineThickness: single;
  Points: TPointArray; var Polygon: TPolygon32;
  const MultiplePolygons, PolygonComplete: boolean;
  UseStipple: boolean = false;
  StartIndex: Integer = 0; NumPts: Integer = -1); overload;

// @abstract(@name draws a rectangle with opposite corners
// defined by (X1,Y1) and (X2,Y2).)
procedure DrawBigRectangle32Old(const BitMap: TBitmap32;
  const OutlineColor, FillColor: TColor32; const LineThickness: single;
  X1, Y1, X2, Y2: Integer; UseStipple: boolean = False); overload;
procedure DrawCanvasRectangle(const Canvas: TCanvas;
  const OutlineColor, FillColor: TColor;
  X1, Y1, X2, Y2: Integer); overload;
procedure DrawBigRectangle32(const Graphic: TPersistent;
  const OutlineColor, FillColor: TColor32; const LineThickness: single;
  X1, Y1, X2, Y2: Integer; UseStipple: boolean = False); overload;

  // @abstract(@name draws a rectangle defined by ARect.)
procedure DrawBigRectangle32Old(const BitMap: TBitmap32;
  const OutlineColor, FillColor: TColor32; const LineThickness: single;
  const ARect: TRect; UseStipple: boolean = False); overload;
procedure DrawCanvasRectangle(const Canvas: TCanvas;
  const OutlineColor, FillColor: TColor;
  const ARect: TRect); overload;
procedure DrawBigRectangle32(const Graphic: TPersistent;
  const OutlineColor, FillColor: TColor32; const LineThickness: single;
  const ARect: TRect; UseStipple: boolean = False); overload;

implementation

uses Math, Forms;

const
  BottomRange = 0;
var
  Bounds: TRect;
  TopRange: integer;

function Color32ToWinColor(AColor32: TColor32): TColor;
var
  R: Byte;
  G: Byte;
  B: Byte;
begin
  Color32ToRGB(AColor32, R, G, B);
  result := (B shl 16) + (G shl 8) + R;
end;

procedure SetTopRange;
begin
  if Screen <> nil then
  begin
    TopRange := Max(Screen.Width, Screen.Height) * 3 div 2;
  end
  else
  begin
    TopRange := 1;
  end;
  Bounds := Rect(BottomRange, BottomRange, TopRange, TopRange);
end;

function SortRightUp(Item1, Item2: Pointer): Integer;
begin
  result := PPoint(Item1)^.X - PPoint(Item2)^.X;
  if result = 0 then
  begin
    result := PPoint(Item1)^.Y - PPoint(Item2)^.Y;
  end;
end;

function SortRightDown(Item1, Item2: Pointer): Integer;
begin
  result := PPoint(Item1)^.X - PPoint(Item2)^.X;
  if result = 0 then
  begin
    result := PPoint(Item2)^.Y - PPoint(Item1)^.Y;
  end;
end;

function SortLeftUp(Item1, Item2: Pointer): Integer;
begin
  result := PPoint(Item2)^.X - PPoint(Item1)^.X;
  if result = 0 then
  begin
    result := PPoint(Item1)^.Y - PPoint(Item2)^.Y;
  end;
end;

function SortLeftDown(Item1, Item2: Pointer): Integer;
begin
  result := PPoint(Item2)^.X - PPoint(Item1)^.X;
  if result = 0 then
  begin
    result := PPoint(Item2)^.Y - PPoint(Item1)^.Y;
  end;
end;

procedure InterpolateArray(const Points: array of TPoint;
  var NewPoints: TPointArray; StartIndex: Integer = 0;
  NumPts: Integer = -1);
var
  TempPoints: array[0..3] of TPoint;
  Index: integer;
  Count, OuterCount: integer;
  InnerIndex: integer;
  Sorter: TList;
  Point1, Point2, Temp: TPoint;
  X1, X2, Y1, Y2: real;
begin
  if NumPts = -1 then
  begin
    NumPts := Length(Points) - StartIndex;
  end;
  if NumPts <= 0 then
  begin
    Exit;
  end;
  SetLength(NewPoints, (NumPts - 1) * 5 + 1);
  NewPoints[0] := Points[StartIndex];
  OuterCount := 1;
  Sorter := TList.Create;
  try
    for Index := StartIndex + 1 to StartIndex + NumPts - 1 do
    begin
      Point1 := Points[Index - 1];
      Point2 := Points[Index];
      // convert to real numbers to prevent integer overflow.
      X1 := Point1.X;
      X2 := Point2.X;
      Y1 := Point1.Y;
      Y2 := Point2.Y;
      Count := 0;
      if ((Point1.X > TopRange) and (Point2.X < TopRange))
        or ((Point1.X < TopRange) and (Point2.X > TopRange)) then
      begin
        Temp.X := TopRange;
        Temp.Y := Round((TopRange - X1) / (X2 - X1) * (Y2 - Y1) + Y1);
        TempPoints[Count] := Temp;
        Inc(Count);
      end;
      if ((Point1.X > BottomRange) and (Point2.X < BottomRange))
        or ((Point1.X < BottomRange) and (Point2.X > BottomRange)) then
      begin
        Temp.X := BottomRange;
        Temp.Y := Round((BottomRange - X1) / (X2 - X1) * (Y2 - Y1) + Y1);
        TempPoints[Count] := Temp;
        Inc(Count);
      end;

      if ((Point1.Y > TopRange) and (Point2.Y < TopRange))
        or ((Point1.Y < TopRange) and (Point2.Y > TopRange)) then
      begin
        Temp.Y := TopRange;
        Temp.X := Round((TopRange - Y1) / (Y2 - Y1) * (X2 - X1) + X1);
        TempPoints[Count] := Temp;
        Inc(Count);
      end;
      if ((Point1.Y > BottomRange) and (Point2.Y < BottomRange))
        or ((Point1.Y < BottomRange) and (Point2.Y > BottomRange)) then
      begin
        Temp.Y := BottomRange;
        Temp.X := Round((BottomRange - Y1) / (Y2 - Y1) * (X2 - X1) + X1);
        TempPoints[Count] := Temp;
        Inc(Count);
      end;
      if Count > 0 then
      begin
        Sorter.Clear;
        Sorter.Capacity := Count;

        for InnerIndex := 0 to Count - 1 do
        begin
          Sorter.Add(@TempPoints[InnerIndex]);
        end;

        if Count > 1 then
        begin
          if Point2.X > Point1.X then
          begin
            if Point2.Y > Point1.Y then
            begin
              Sorter.Sort(SortRightUp);
            end
            else
            begin
              Sorter.Sort(SortRightDown);
            end;
          end
          else
          begin
            if Point2.Y > Point1.Y then
            begin
              Sorter.Sort(SortLeftUp);
            end
            else
            begin
              Sorter.Sort(SortLeftDown);
            end;
          end;
        end;
        for InnerIndex := 0 to Sorter.Count - 1 do
        begin
          NewPoints[OuterCount] := PPoint(Sorter[InnerIndex])^;
          Inc(OuterCount);
        end;
      end;
      NewPoints[OuterCount] := Point2;
      Inc(OuterCount);
    end;
    SetLength(NewPoints, OuterCount);
  finally
    Sorter.Free;
  end;
end;

procedure DrawPolylineCanvas(const Canvas: TCanvas; const Color: TColor;
  const Points: array of TPoint; StartIndex: Integer = 0; NumPts: Integer = -1);
var
  PointIndex: Integer;
  EndIndex: Integer;
begin
  if Canvas = nil then Exit;
  if Length(Points) = 0 then Exit;

  Canvas.Pen.Color := Color;

  if NumPts < 0 then
  begin
    EndIndex := Length(Points);
  end
  else
  begin
    EndIndex := StartIndex + NumPts;
  end;

  Canvas.MoveTo(Points[StartIndex].X, Points[StartIndex].Y);
  for PointIndex := StartIndex+1 to EndIndex - 1 do
  begin
    Canvas.LineTo(Points[PointIndex].X, Points[PointIndex].Y);
  end;

end;

procedure DrawBigPolyline32(const Graphic: TPersistent; const Color: TColor32;
  const LineThickness: single;
  const Points: array of TPoint; FirstLine: boolean; UseStipple: boolean = False;
  StartIndex: Integer = 0; NumPts: Integer = -1);
var
  Bitmap: TBitmap32;
  Canvas: TCanvas;
begin
  if Graphic = nil then
  begin
    Exit;
  end;
  try
    if Graphic is TBitmap32 then
    begin
      Bitmap := TBitmap32(Graphic);
      DrawBigPolyline32Old(Bitmap, Color, LineThickness, Points, FirstLine,
        UseStipple, StartIndex, NumPts);
    end
    else
    begin
      Canvas := Graphic as TCanvas;
      DrawPolylineCanvas(Canvas, Color32ToWinColor(Color), Points,
        StartIndex, NumPts);
    end;
  except on EZeroDivide do
    begin

    end;
  end;
end;

procedure DrawBigPolyline32Old(const BitMap: TBitmap32; const Color: TColor32;
  const LineThickness: single;
  const Points: array of TPoint; FirstLine: boolean; UseStipple: boolean = False;
  StartIndex: Integer = 0; NumPts: Integer = -1);
var
  NewPoints: TPointArray;
  Index: integer;
  Count: integer;
  APoint: TPoint;
  MyStartIndex: integer;
  Temp: TPointArray;
  Polygon: TPolygon32;
  PointIndex: integer;
  TmpPoly, Outline : TPolygon32;
begin
  if BitMap = nil then Exit;

  if LineThickness <> 1 then
  begin
    UseStipple := False;
  end;
  if UseStipple then
  begin
    BitMap.SetStipple([0, 0, 0, 0, 0,
    Color, Color, Color, Color, Color]);
  end;
//  UseStipple := False;

//  FirstLine := True;
  Polygon := TPolygon32.Create;
  try
    Polygon.Antialiased := True;
    Polygon.AntialiasMode := am16times;
    Polygon.Closed := False;
    SetTopRange;
    InterpolateArray(Points, NewPoints, StartIndex, NumPts);
    Count := 0;
    MyStartIndex := 0;
    for Index := 0 to Length(NewPoints) - 1 do
    begin
      APoint := NewPoints[Index];
      if (APoint.X < BottomRange) or (APoint.X > TopRange)
        or (APoint.Y < BottomRange) or (APoint.Y > TopRange) then
      begin
        if Count > 0 then
        begin
          SetLength(Temp, Count);
          Move( NewPoints[MyStartIndex], Temp[0], Count*SizeOf(TPoint));
          if UseStipple then
          begin
            BitMap.MoveToF(Temp[0].X, Temp[0].Y);
            for PointIndex := 1 to Length(Temp) - 1 do
            begin
              BitMap.LineToFSP(Temp[PointIndex].X, Temp[PointIndex].Y);
            end;
          end
          else
          begin
            if not FirstLine  then
            begin
              Polygon.NewLine;
              FirstLine := True;
            end;
            for PointIndex := 0 to Length(Temp) - 1 do
            begin
              Polygon.Add(FixedPoint(Temp[PointIndex]));
            end;
          end;
        end;
        Count := 0;
        MyStartIndex := Index + 1;
        FirstLine := False;
      end
      else
      begin
        Inc(Count);
      end;

    end;
    if Count > 0 then
    begin
      SetLength(Temp, Count);
      Move( NewPoints[MyStartIndex], Temp[0], Count*SizeOf(TPoint));

      if UseStipple then
      begin
        BitMap.MoveToF(Temp[0].X, Temp[0].Y);
        for PointIndex := 1 to Length(Temp) - 1 do
        begin
          BitMap.LineToFSP(Temp[PointIndex].X, Temp[PointIndex].Y);
        end;
      end
      else
      begin
        if not FirstLine  then
        begin
          Polygon.NewLine;
        end;
        for PointIndex := 0 to Length(Temp) - 1 do
        begin
          Polygon.Add(FixedPoint(Temp[PointIndex]));
        end;
      end;
    end;

    if not UseStipple then
    begin
      if (LineThickness = 1)  then
      begin
        Polygon.DrawEdge(BitMap, Color);
      end
      else
      begin
        TmpPoly := Polygon.Outline;
        try
          Outline := TmpPoly.Grow(Fixed(LineThickness / 4), 0.99);
          try
            Outline.FillMode := pfWinding;
            Outline.Draw(BitMap, Color, Color);
          finally
            Outline.Free
          end;
        finally
          TmpPoly.Free;
        end;
      end;
    end;
  finally
    Polygon.Free;
  end;
end;

procedure DrawCanvasePolygon(const Canvas: TCanvas;
  const OutlineColor, FillColor: TColor;
  Points: TPointArray;
  StartIndex: Integer = 0; NumPts: Integer = -1);
var
  NewPoints: TPointArray;
  EndIndex: Integer;
begin
  if Canvas = nil then Exit;
  if Length(Points) = 0 then Exit;

  Canvas.Pen.Color := OutlineColor;
  Canvas.Brush.Color := FillColor;

  if (StartIndex=0) and (NumPts=-1) then
  begin
    Canvas.Polygon(Points);
  end
  else
  begin
    if NumPts < 0 then
    begin
      EndIndex := Length(Points);
    end
    else
    begin
      EndIndex := StartIndex + NumPts;
    end;
    NumPts := EndIndex-StartIndex-1;
    SetLength(NewPoints, NumPts);
    Move(Points[StartIndex], NewPoints[0], NumPts*SizeOf(TPoint));
    Canvas.Polygon(NewPoints);
  end;
end;

procedure DrawBigPolygon32Old(const BitMap: TBitmap32;
  const OutlineColor, FillColor: TColor32;
  const LineThickness: single;
  Points: TPointArray; var Polygon: TPolygon32;
  const MultiplePolygons, PolygonComplete: boolean;
  UseStipple: boolean = false;
  StartIndex: Integer = 0; NumPts: Integer = -1);
var
  TempPoints: TPointArray;
  NewPoints: TPointArray;
  Index: integer;
  I1, I3: integer;
  P1, P2, P3: TPoint;
  SkipPoint: boolean;
  SkipFirstPoint: boolean;
  I2: integer;
//  Polygon: TPolygon32;
  TmpPoly, Outline: TPolygon32;
  ArrayChanged: boolean;
  procedure DeleteSomePoints(Source: TPointArray; var Destination: TPointArray;
    var Changed: boolean);
  var
    Index: integer;
  begin
    Changed := False;
    setLength(Destination, Length(Source));
    I1 := Length(Source) - 3;
    I2 := 0;
    I3 := 0;
    SkipFirstPoint := True;
    for Index := 0 to Length(Source) - 2 do
    begin
      P2 := Source[Index];

      Inc(I1);
      if I1 = Length(Source) - 1 then
      begin
        I1 := 0;
      end;

      Inc(I3);
      if I3 = Length(Source) - 1 then
      begin
        I3 := 0;
      end;
      P1 := Source[I1];
      P3 := Source[I3];
      SkipPoint := False;
      if (P1.Y = P2.Y) and (P2.Y = P3.Y) then
      begin
        if (P2.X > P1.X) and (P2.X > P3.X) then
        begin
          SkipPoint := True;

        end
        else if (P2.X < P1.X) and (P2.X < P3.X) then
        begin
          SkipPoint := True;
        end;
      end;
      if (P1.X = P2.X) and (P2.X = P3.X) then
      begin
        if (P2.Y > P1.Y) and (P2.Y > P3.Y) then
        begin
          SkipPoint := True;
        end
        else if (P2.Y < P1.Y) and (P2.Y < P3.Y) then
        begin
          SkipPoint := True;
        end;
      end;
      if (P1.X = P2.X) and (P1.Y = P2.Y) then
      begin
        SkipPoint := True;
      end;

      if not SkipPoint then
      begin
        if (I2 < 2) then
        begin
          Destination[I2] := P2;
          Inc(I2);
        end
        else
        begin
          P1 := Destination[I2 - 2];
          if (P1.X <> P2.X) or (P1.Y <> P2.Y) then
          begin
            Destination[I2] := P2;
            Inc(I2);
          end
          else
          begin
            Dec(I2, 2);
          end;
        end
      end;
      if Index = 0 then
      begin
        SkipFirstPoint := SkipPoint;
      end;
      if SkipPoint then
      begin
        Changed := True;
      end;
    end;
    if SkipFirstPoint then
    begin
      if I2 > 0 then
      begin
        Destination[I2] := Destination[0];
        Inc(I2);
      end;
    end
    else
    begin
      Destination[I2] := Source[0];
      Inc(I2);
    end;
  end;
begin
  if BitMap = nil then Exit;
  if LineThickness <> 1 then
  begin
    UseStipple := False;
  end;
  if UseStipple then
  begin
    BitMap.SetStipple([0, 0, 0, 0, 0,
    OutlineColor, OutlineColor, OutlineColor, OutlineColor, OutlineColor]);
  end;
  SetTopRange;
  if NumPts <= 0 then
  begin
    NumPts := Length(Points) - StartIndex;
  end;
  TempPoints := Points;
  SetLength(TempPoints, StartIndex + NumPts + 1);
  TempPoints[StartIndex + NumPts] := TempPoints[StartIndex];
  InterpolateArray(TempPoints, NewPoints, StartIndex, NumPts + 1);
  for Index := 0 to Length(NewPoints) - 1 do
  begin
    if NewPoints[Index].X > TopRange then
    begin
      NewPoints[Index].X := TopRange
    end
    else if NewPoints[Index].X < BottomRange then
    begin
      NewPoints[Index].X := BottomRange
    end;

    if NewPoints[Index].Y > TopRange then
    begin
      NewPoints[Index].Y := TopRange
    end
    else if NewPoints[Index].Y < BottomRange then
    begin
      NewPoints[Index].Y := BottomRange
    end;
  end;
  ArrayChanged := False;
  repeat
    DeleteSomePoints(NewPoints, TempPoints, ArrayChanged);
    if ArrayChanged then
    begin
      SetLength(TempPoints, I2);
      NewPoints := TempPoints;
    end;
  until not ArrayChanged;
{  setLength(TempPoints, Length(NewPoints));
  I1 := Length(NewPoints) - 3;
  I2 := 0;
  I3 := 0;
  SkipFirstPoint := True;
  for Index := 0 to Length(NewPoints) - 2 do
  begin
    P2 := NewPoints[Index];

    Inc(I1);
    if I1 = Length(NewPoints) - 1 then
    begin
      I1 := 0;
    end;

    Inc(I3);
    if I3 = Length(NewPoints) - 1 then
    begin
      I3 := 0;
    end;
    P1 := NewPoints[I1];
    P3 := NewPoints[I3];
    SkipPoint := False;
    if (P1.Y = P2.Y) and (P2.Y = P3.Y) then
    begin
      if (P2.X > P1.X) and (P2.X > P3.X) then
      begin
        SkipPoint := True;
      end
      else if (P2.X < P1.X) and (P2.X < P3.X) then
      begin
        SkipPoint := True;
      end;
    end;
    if (P1.X = P2.X) and (P2.X = P3.X) then
    begin
      if (P2.Y > P1.Y) and (P2.Y > P3.Y) then
      begin
        SkipPoint := True;
      end
      else if (P2.Y < P1.Y) and (P2.Y < P3.Y) then
      begin
        SkipPoint := True;
      end;
    end;
    if (P1.X = P2.X) and (P1.Y = P2.Y) then
    begin
      SkipPoint := True;
    end;

    if not SkipPoint then
    begin
      if (I2 < 2) then
      begin
        TempPoints[I2] := P2;
        Inc(I2);
      end
      else
      begin
        P1 := TempPoints[I2 - 2];
        if (P1.X <> P2.X) or (P1.Y <> P2.Y) then
        begin
          TempPoints[I2] := P2;
          Inc(I2);
        end
        else
        begin
          Dec(I2, 2);
        end;
      end
    end;
    if Index = 0 then
    begin
      SkipFirstPoint := SkipPoint;
    end;
  end;
  if SkipFirstPoint then
  begin
    if I2 > 0 then
    begin
      TempPoints[I2] := TempPoints[0];
      Inc(I2);
    end;
  end
  else
  begin
    TempPoints[I2] := NewPoints[0];
    Inc(I2);
  end;      }
  if (I2 > 3) or (MultiplePolygons and PolygonComplete) then
  begin
    SetLength(TempPoints, I2);

    if MultiplePolygons then
    begin
      if Polygon = nil then
      begin
        Polygon := TPolygon32.Create;
        Polygon.Antialiased := True;
        Polygon.AntialiasMode := am16times;
        Polygon.FillMode := pfAlternate;
      end
      else
      begin
        if (I2 > 3) then
        begin
          Polygon.NewLine;
        end;
      end;
    end
    else
    begin
      Assert(Polygon = nil);
      Polygon := TPolygon32.Create;
      Polygon.Antialiased := True;
      Polygon.AntialiasMode := am16times;
    end;
    try
      if (I2 > 3) then
      begin
        for Index := 0 to I2 - 1 do
        begin
          Polygon.Add(FixedPoint(TempPoints[Index]));
        end;
      end;
      if PolygonComplete then
      begin
        if (LineThickness = 1) or UseStipple then
        begin
          if UseStipple then
          begin
            Polygon.Draw(BitMap, clTransparent32, FillColor);
            BitMap.MoveToF(TempPoints[0].X, TempPoints[0].Y);
            for Index := 1 to Length(TempPoints) - 1 do
            begin
              BitMap.LineToFSP(TempPoints[Index].X, TempPoints[Index].Y);
            end;
          end
          else
          begin
            if MultiplePolygons then
            begin
              Polygon.DrawFill(BitMap, FillColor);
              Polygon.DrawEdge(BitMap, OutlineColor);
            end
            else
            begin
              Polygon.Draw(BitMap, OutlineColor, FillColor);
            end;
          end;
  //        Polygon.DrawEdge(BitMap, Color);
        end
        else
        begin
          Polygon.DrawFill(BitMap, FillColor);
          TmpPoly := Polygon.Outline;
          try
            Outline := TmpPoly.Grow(Fixed(LineThickness / 4), 0.99);
            try
              Outline.FillMode := pfWinding;
              Outline.Draw(BitMap, OutlineColor, OutlineColor);
            finally
              Outline.Free
            end;
          finally
            TmpPoly.Free;
          end;
        end;
      end;
    finally
      if not MultiplePolygons then
      begin
        FreeAndNil(Polygon);
      end;
    end;
  end;
end;

procedure DrawBigPolygon32(const Graphic: TPersistent;
  const OutlineColor, FillColor: TColor32; const LineThickness: single;
  Points: TPointArray; var Polygon: TPolygon32;
  const MultiplePolygons, PolygonComplete: boolean;
  UseStipple: boolean = false;
  StartIndex: Integer = 0; NumPts: Integer = -1); overload;
var
  Bitmap: TBitmap32;
  Canvas: TCanvas;
begin
  if Graphic = nil then
  begin
    Exit;
  end;
  if Graphic is TBitmap32 then
  begin
    Bitmap := TBitmap32(Graphic);
    DrawBigPolygon32Old(Bitmap, OutlineColor, FillColor, LineThickness,
      Points, Polygon, MultiplePolygons, PolygonComplete, UseStipple,
      StartIndex, NumPts);
  end
  else
  begin
    Canvas := Graphic as TCanvas;
    DrawCanvasePolygon(Canvas, Color32ToWinColor(OutlineColor),
      Color32ToWinColor(FillColor), Points, StartIndex, NumPts);
  end;
end;

procedure DrawCanvasRectangle(const Canvas: TCanvas;
  const OutlineColor, FillColor: TColor;
  X1, Y1, X2, Y2: Integer); overload;
var
  temp: integer;
begin
  if Canvas = nil then Exit;
  if X2 < X1 then
  begin
    temp := X2;
    X2 := X1;
    X1 := Temp;
  end;
  if Y2 < Y1 then
  begin
    temp := Y2;
    Y2 := Y1;
    Y1 := Temp;
  end;
  DrawCanvasRectangle(Canvas, OutlineColor, FillColor, Rect(X1, Y1, X2, Y2));
end;

procedure DrawBigRectangle32Old(const BitMap: TBitmap32;
  const OutlineColor, FillColor: TColor32; const LineThickness: single;
  X1, Y1, X2, Y2: Integer; UseStipple: boolean = False); overload;
var
  temp: integer;
begin
  if BitMap = nil then Exit;
  if X2 < X1 then
  begin
    temp := X2;
    X2 := X1;
    X1 := Temp;
  end;
  if Y2 < Y1 then
  begin
    temp := Y2;
    Y2 := Y1;
    Y1 := Temp;
  end;
  DrawBigRectangle32(BitMap, OutlineColor, FillColor, LineThickness, Rect(X1, Y1, X2, Y2), UseStipple);
end;

procedure DrawBigRectangle32(const Graphic: TPersistent;
  const OutlineColor, FillColor: TColor32; const LineThickness: single;
  X1, Y1, X2, Y2: Integer; UseStipple: boolean = False); overload;
var
  Bitmap: TBitmap32;
  Canvas: TCanvas;
begin
  if Graphic = nil then
  begin
    Exit;
  end;

  if Graphic is TBitmap32 then
  begin
    Bitmap := TBitmap32(Graphic);
    if (X1 < BitMap.Width) and (X2 >= 0) and (Y1 < Bitmap.Height) and (Y2 >= 0) then
    begin
      DrawBigRectangle32Old(Bitmap, OutlineColor, FillColor, LineThickness, X1, Y1, X2, Y2,
        UseStipple);
    end;
  end
  else
  begin
    Canvas := Graphic as TCanvas;
    DrawCanvasRectangle(Canvas, Color32ToWinColor(OutlineColor),
      Color32ToWinColor(FillColor), X1, Y1, X2, Y2)
  end;
end;

procedure DrawBigRectangle32(const Graphic: TPersistent;
  const OutlineColor, FillColor: TColor32; const LineThickness: single;
  const ARect: TRect; UseStipple: boolean = False); overload;
var
  Bitmap: TBitmap32;
  Canvas: TCanvas;
  OldWidth: Integer;
begin
  if Graphic = nil then
  begin
    Exit;
  end;
  if Graphic is TBitmap32 then
  begin
    Bitmap := TBitmap32(Graphic);
    DrawBigRectangle32Old(Bitmap, OutlineColor, FillColor, LineThickness, ARect,
      UseStipple);
  end
  else
  begin
    Canvas := Graphic as TCanvas;
    OldWidth := Canvas.Pen.Width;
    try
      Canvas.Pen.Width := Round(LineThickness);
      DrawCanvasRectangle(Canvas, Color32ToWinColor(OutlineColor),
        Color32ToWinColor(FillColor), ARect)
    finally
      Canvas.Pen.Width := OldWidth;
    end;
  end;
end;

procedure DrawCanvasRectangle(const Canvas: TCanvas;
  const OutlineColor, FillColor: TColor;
  const ARect: TRect); overload;
var
  Intersection: TRect;
  Points: TPointArray;
//  Polygon: TPolygon32;
//  MultiplePolygons: boolean;
begin
  if Canvas = nil then Exit;
  SetLength(Points, 5);
  SetTopRange;
  if Gr32.IntersectRect(Intersection, ARect, Bounds) then
  begin
    Points[0] := Intersection.TopLeft;
    Points[1].X := Intersection.Left;
    Points[1].Y := Intersection.Bottom;
    Points[2] := Intersection.BottomRight;
    Points[3].X := Intersection.Right;
    Points[3].Y := Intersection.Top;
    Points[4] := Intersection.TopLeft;
//    Polygon := nil;
//    MultiplePolygons := False;
    DrawCanvasePolygon(Canvas, OutlineColor, FillColor, Points);
  end;
end;

procedure DrawBigRectangle32Old(const BitMap: TBitmap32;
  const OutlineColor, FillColor: TColor32; const LineThickness: single;
  const ARect: TRect; UseStipple: boolean = False); overload;
var
  Intersection: TRect;
  Points: TPointArray;
  Polygon: TPolygon32;
  MultiplePolygons: boolean;
begin
  if BitMap = nil then Exit;
  SetLength(Points, 5);
  SetTopRange;
  if Gr32.IntersectRect(Intersection, ARect, Bounds) then
  begin
    Points[0] := Intersection.TopLeft;
    Points[1].X := Intersection.Left;
    Points[1].Y := Intersection.Bottom;
    Points[2] := Intersection.BottomRight;
    Points[3].X := Intersection.Right;
    Points[3].Y := Intersection.Top;
    Points[4] := Intersection.TopLeft;
    Polygon := nil;
    MultiplePolygons := False;
    DrawBigPolygon32(BitMap, OutlineColor, FillColor, LineThickness, Points,
      Polygon, MultiplePolygons, True, UseStipple);
  end;
end;

procedure DrawBigTextBitmap32(const BitMap32: TBitmap32; const Location: TPoint;
  const Text: string; const Font: TFont);
begin
  if BitMap32 <> nil then
  begin
    Bitmap32.Textout(Location.X, Location.Y, Text);
  end;
end;

procedure DrawBigTextCanvas(const Canvas: TCanvas; const Location: TPoint;
  const Text: string; const Font: TFont);
begin
  if Canvas <> nil then
  begin
    Canvas.Textout(Location.X, Location.Y, Text);
  end;
end;

procedure DrawBigText(const Graphic: TPersistent; const Location: TPoint;
  const Text: string; Font: TFont = nil);
var
  Bitmap: TBitmap32;
  Canvas: TCanvas;
begin
  if Graphic = nil then
  begin
    Exit;
  end;

  if Graphic is TBitmap32 then
  begin
    Bitmap := TBitmap32(Graphic);
    if Font = nil then
    begin
      Font := Bitmap.Font;
    end;
    DrawBigTextBitmap32(Bitmap, Location, Text, Font);
  end
  else
  begin
    Canvas := Graphic as TCanvas;
    if Font = nil then
    begin
      Font := Canvas.Font;
    end;
    DrawBigTextCanvas(Canvas, Location, Text, Font);
  end;
end;

end.

