unit Point2DRegressionUnit;

interface

uses
  FastGEO, System.SysUtils, JmTypes;

type
  T2D_DynDoubleArray = array of TDynDoubleArray;

// See https://www.ilikebigbits.com/2015_03_04_plane_from_points.html
// Given a set of (x,y) points and corresponding Z values, @name returns
// the coefficients of an equation describing the best fit plane through
// the points.
// Z = Coefficients[0] + Coefficients[1]X + Coefficient[2]Y
// There must be at least 3 non-colinear points in PointData.

procedure Regress2DPoints(Const PointData: TPoint2DArray;
  const ZValues: TDynDoubleArray; var Coefficients: TDynDoubleArray);

// Given a set of (x1,x2, x3, ... xn) points and corresponding Z values, 
// @name returns
// the coefficients of an equation describing the best fit through
// the points.
// Z = Coefficients[0] + Coefficients[1]x1 + Coefficient[2]x2 + ... + Coefficient[n]xn
// There must be at least 3 non-colinear points in XData.
procedure Regression(PointData: T2D_DynDoubleArray; const ZValues: TDynDoubleArray;
  var Coefficients: TDynDoubleArray);

procedure Regress2DPointsExpanded(Const PointData: TPoint2DArray;
  const ZValues: TDynDoubleArray; var Coefficients: TDynDoubleArray);

implementation

uses
  JmFloatVector, JmFloatMatrix;

procedure Regression(PointData: T2D_DynDoubleArray; const ZValues: TDynDoubleArray;
  var Coefficients: TDynDoubleArray);
var
  Data: IJmFloatMatrix;
  resultVector: IJmFloatVector;
  Zs: IJmFloatVector;
  RowIndex: Integer;
  DataTranspose: IJmFloatMatrix;
  DataTranspose2: IJmFloatMatrix;
  XIndex: Integer;
begin
  Assert(Length(PointData) = Length(ZValues));

  Data := Matrix(Length(PointData),Length(PointData[0])+1);
  Zs := Vector(Length(ZValues));
  resultVector := Vector(3);

  for RowIndex := 1 to Length(PointData) do
  begin
    Data[RowIndex,1] := 1;
    for XIndex := 1 to Length(PointData[0]) do
    begin
      Data[RowIndex,XIndex+1]:= PointData[RowIndex-1, XIndex-1];
    end;
    Zs[RowIndex] := ZValues[RowIndex-1];
  end;

  DataTranspose := Data.Transposed;

  DataTranspose2 := Matrix(1,1);
  DataTranspose2.Copy(DataTranspose);

  DataTranspose.MultWithMatrix(Data);

  DataTranspose.Invert;

  DataTranspose.MultWithMatrix(DataTranspose2);

  DataTranspose.ProdWithVector(Zs, resultVector);

  SetLength(Coefficients, resultVector.Dim);
  for RowIndex := 1 to resultVector.Dim do
  begin
    Coefficients[RowIndex-1] := resultVector[RowIndex]
  end;
end;


procedure Regress2DPoints(Const PointData: TPoint2DArray;
  const ZValues: TDynDoubleArray; var Coefficients: TDynDoubleArray);
var
  Data: IJmFloatMatrix;
  resultVector: IJmFloatVector;
  Zs: IJmFloatVector;
  RowIndex: Integer;
  DataTranspose: IJmFloatMatrix;
  DataTranspose2: IJmFloatMatrix;
begin
  Assert(Length(PointData) = Length(ZValues));

  Data := Matrix(Length(PointData),3);
  Zs := Vector(Length(ZValues));
  resultVector := Vector(3);

  for RowIndex := 1 to Length(PointData) do
  begin
    Data[RowIndex,1] := 1;
    Data[RowIndex,2]:= PointData[RowIndex-1].x;
    Data[RowIndex,3]:= PointData[RowIndex-1].y;
    Zs[RowIndex] := ZValues[RowIndex-1];
  end;

  DataTranspose := Data.Transposed;

  DataTranspose2 := Matrix(1,1);
  DataTranspose2.Copy(DataTranspose);

  DataTranspose.MultWithMatrix(Data);

  DataTranspose.Invert;

  DataTranspose.MultWithMatrix(DataTranspose2);

  DataTranspose.ProdWithVector(Zs, resultVector);

  SetLength(Coefficients, resultVector.Dim);
  for RowIndex := 1 to resultVector.Dim do
  begin
    Coefficients[RowIndex-1] := resultVector[RowIndex]
  end;
end;

procedure Regress2DPointsExpanded(Const PointData: TPoint2DArray;
  const ZValues: TDynDoubleArray; var Coefficients: TDynDoubleArray);
var
  IsColinear: Boolean;
  Index: Integer;
  procedure SetWithCollinearPoints;
  var
    NewPointData: TPoint2DArray;
    NewZValues: TDynDoubleArray;
    PointIndex: Integer;
    XOffset: double;
    YOffset: double;
    OffSet: Extended;
  begin
    NewPointData := PointData;
    NewZValues := ZValues;
    SetLength(NewPointData, Length(NewPointData)*2);
    SetLength(NewZValues, Length(NewZValues)*2);
    for PointIndex := 0 to Length(PointData) - 1 do
    begin
      NewPointData[PointIndex + Length(PointData)] := NewPointData[PointIndex];
      NewZValues[PointIndex + Length(ZValues)] := NewZValues[PointIndex];
    end;

    XOffset := PointData[1].y - PointData[0].y;
    YOffset := PointData[0].x - PointData[1].x;
    OffSet := Sqrt(Sqr(XOffset) + Sqr(YOffset));
    Assert(OffSet <> 0);
    if PointData[0].x = PointData[1].x then
    begin
      for PointIndex := 0 to Length(PointData) - 1 do
      begin
        NewPointData[PointIndex + Length(PointData)].x := 
          NewPointData[PointIndex].x -OffSet;
      end;
    end
    else if PointData[0].y = PointData[1].y then
    begin
      for PointIndex := 0 to Length(PointData) - 1 do
      begin
        NewPointData[PointIndex + Length(PointData)].y := 
          NewPointData[PointIndex].y -OffSet;
      end;
    end
    else
    begin
      // Offset points perpendicular to the original line.
      for PointIndex := 0 to Length(PointData) - 1 do
      begin
        NewPointData[PointIndex + Length(PointData)].x := 
          NewPointData[PointIndex].x + XOffset;
        NewPointData[PointIndex + Length(PointData)].y := 
          NewPointData[PointIndex].y + YOffset;
      end;
    end;
    Regress2DPoints(NewPointData, NewZValues, Coefficients);
  end;
begin
  Assert(Length(PointData) = Length(ZValues));
  SetLength(Coefficients,3);
  if Length(PointData) = 0 then
  begin
    Coefficients[0] := 0;
    Coefficients[1] := 0;
    Coefficients[2] := 0;
  end
  else if Length(PointData) = 1 then
  begin
    Coefficients[0] := ZValues[0];
    Coefficients[1] := 0;
    Coefficients[2] := 0;
  end
  else if Length(PointData) = 2 then
  begin
    SetWithCollinearPoints;
  end
  else
  begin
    IsColinear := True;
    for Index := 2 to Length(PointData) -1 do
    begin
      if not Collinear(PointData[0], PointData[1], PointData[Index]) then
      begin
        IsColinear := False;
        break;
      end;
    end;
    if IsColinear then
    begin
      SetWithCollinearPoints;
    end
    else
    begin
      Regress2DPoints(PointData, ZValues, Coefficients);
    end;
  end;
end;

end.
