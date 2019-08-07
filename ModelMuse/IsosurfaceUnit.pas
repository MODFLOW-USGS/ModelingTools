unit IsosurfaceUnit;

{ Modified from Metaballs project on http://www.sulaco.co.za/opengl3.htm
  by Jan Horn.
}

interface

uses OpenGL;

type
  TGLCoord = Record
    X, Y, Z : glFLoat;
  end;

  TGLCoordArray = array of TGLCoord;

  TGridPoint = record
    P : TGLCoord;
    Value : glFLoat;
  end;
  PGridPoint = ^TGridPoint;
  TGridCube = record
    GridPoint : Array [0..7] of PGridPoint; // Points to 8 grid points (cube)
  end;

  TGrid = array of array of array of TGridPoint;
  TCubes = array of array of array of TGridCube;

procedure DrawIsoSurface(Grid: TGrid; Value: double);

implementation

uses LookUpTable;

function Interpolate(const C1, C2 : TGLCoord; Val1, Val2 : glFloat; ContourValue: glFloat) : TGLCoord;
var mu : glFLoat;
begin
  if (Val1 = ContourValue) then
    Result := C1
  else
  if (Val2 = ContourValue) then
    Result := C2
  else
  begin
    mu := (ContourValue - Val1) / (Val2 - Val1);
    Result.x := C1.x + mu * (C2.x - C1.x);
    Result.y := C1.y + mu * (C2.y - C1.y);
    Result.z := C1.z + mu * (C2.z - C1.z);
  end;
end;

{------------------------------------------------------------}
{  Calculate the triangles required to draw a Cube.          }
{  Draws the triangles that makes up a Cube                  }
{------------------------------------------------------------}
procedure CreateCubeTriangles(const GridCube : TGridCube; value: double);
var I : Integer;
    CubeIndex: Integer;
    VertList : Array[0..11] of TGLCoord;
begin
  // Determine the index into the edge table which tells
  // us which vertices are inside/outside the metaballs
  CubeIndex := 0;
  if GridCube.GridPoint[0]^.Value < 1 then CubeIndex := CubeIndex or 1;
  if GridCube.GridPoint[1]^.Value < 1 then CubeIndex := CubeIndex or 2;
  if GridCube.GridPoint[2]^.Value < 1 then CubeIndex := CubeIndex or 4;
  if GridCube.GridPoint[3]^.Value < 1 then CubeIndex := CubeIndex or 8;
  if GridCube.GridPoint[4]^.Value < 1 then CubeIndex := CubeIndex or 16;
  if GridCube.GridPoint[5]^.Value < 1 then CubeIndex := CubeIndex or 32;
  if GridCube.GridPoint[6]^.Value < 1 then CubeIndex := CubeIndex or 64;
  if GridCube.GridPoint[7]^.Value < 1 then CubeIndex := CubeIndex or 128;

  // Check if the cube is entirely in/out of the surface
  if edgeTable[CubeIndex] = 0 then
    Exit;

  // Find the vertices where the surface intersects the cube.
  with GridCube do
  begin
    if (edgeTable[CubeIndex] and 1) <> 0 then
      VertList[0] := Interpolate(GridPoint[0]^.P, GridPoint[1]^.P,
        GridPoint[0]^.Value, GridPoint[1]^.Value, Value);
    if (edgeTable[CubeIndex] and 2) <> 0 then
      VertList[1] := Interpolate(GridPoint[1]^.P, GridPoint[2]^.P,
        GridPoint[1]^.Value, GridPoint[2]^.Value, Value);
    if (edgeTable[CubeIndex] and 4) <> 0 then
      VertList[2] := Interpolate(GridPoint[2]^.P, GridPoint[3]^.P,
        GridPoint[2]^.Value, GridPoint[3]^.Value, Value);
    if (edgeTable[CubeIndex] and 8) <> 0 then
      VertList[3] := Interpolate(GridPoint[3]^.P, GridPoint[0]^.P,
        GridPoint[3]^.Value, GridPoint[0]^.Value, Value);
    if (edgeTable[CubeIndex] and 16) <> 0 then
      VertList[4] := Interpolate(GridPoint[4]^.P, GridPoint[5]^.P,
        GridPoint[4]^.Value, GridPoint[5]^.Value, Value);
    if (edgeTable[CubeIndex] and 32) <> 0 then
      VertList[5] := Interpolate(GridPoint[5]^.P, GridPoint[6]^.P,
        GridPoint[5]^.Value, GridPoint[6]^.Value, Value);
    if (edgeTable[CubeIndex] and 64) <> 0 then
      VertList[6] := Interpolate(GridPoint[6]^.P, GridPoint[7]^.P,
        GridPoint[6]^.Value, GridPoint[7]^.Value, Value);
    if (edgeTable[CubeIndex] and 128) <> 0 then
      VertList[7] := Interpolate(GridPoint[7]^.P, GridPoint[4]^.P,
        GridPoint[7]^.Value, GridPoint[4]^.Value, Value);
    if (edgeTable[CubeIndex] and 256) <> 0 then
      VertList[8] := Interpolate(GridPoint[0]^.P, GridPoint[4]^.P,
        GridPoint[0]^.Value, GridPoint[4]^.Value, Value);
    if (edgeTable[CubeIndex] and 512) <> 0 then
      VertList[9] := Interpolate(GridPoint[1]^.P, GridPoint[5]^.P,
        GridPoint[1]^.Value, GridPoint[5]^.Value, Value);
    if (edgeTable[CubeIndex] and 1024) <> 0 then
      VertList[10] := Interpolate(GridPoint[2]^.P, GridPoint[6]^.P,
        GridPoint[2]^.Value, GridPoint[6]^.Value, Value);
    if (edgeTable[CubeIndex] and 2048) <> 0 then
      VertList[11] := Interpolate(GridPoint[3]^.P, GridPoint[7]^.P,
        GridPoint[3]^.Value, GridPoint[7]^.Value, Value);
  end;

  // Draw the triangles for this cube
  I := 0;
  while TriangleTable[CubeIndex, i] <> -1 do
  begin
//    SetColor(VertList[TriangleTable[CubeIndex][i]]);
    glVertex3fv(@VertList[TriangleTable[CubeIndex][i]]);

//    SetColor(VertList[TriangleTable[CubeIndex][i+1]]);
    glVertex3fv(@VertList[TriangleTable[CubeIndex][i+1]]);

//    if SmoothShading then
//      SetColor(VertList[TriangleTable[CubeIndex][i+2]]);
    glVertex3fv(@VertList[TriangleTable[CubeIndex][i+2]]);

//    Inc(TessTriangles);
    Inc(i, 3);
  end;
end;


procedure DrawIsoSurface(Grid: TGrid; Value: double);
var
  GridSize1: Integer;
  GridSize2: Integer;
  GridSize3: Integer;
  Cubes: TCubes;
  cx: Integer;
  cy: Integer;
  cz: Integer;
begin
//  glShadeModel(GL_SMOOTH);                 // Enables Smooth Color Shading
  GridSize1 := Length(Grid);
  if GridSize1 > 0 then
  begin
    GridSize2 := Length(Grid[0]);
    if GridSize2 > 0 then
    begin
      GridSize3 := Length(Grid[0,0]);
      if GridSize3 > 0 then
      begin
        SetLength(Cubes, GridSize1-1, GridSize2-1, GridSize3-1);
        for cx := 0 to GridSize1-2 do
        begin
          for cy := 0 to GridSize2-2 do
          begin
            for cz := 0 to GridSize3-2 do
            begin
              Cubes[cx,cy,cz].GridPoint[0] := @Grid[cx,   cy,   cz  ];
              Cubes[cx,cy,cz].GridPoint[1] := @Grid[cx+1, cy,   cz  ];
              Cubes[cx,cy,cz].GridPoint[2] := @Grid[cx+1, cy,   cz+1];
              Cubes[cx,cy,cz].GridPoint[3] := @Grid[cx,   cy,   cz+1];
              Cubes[cx,cy,cz].GridPoint[4] := @Grid[cx,   cy+1, cz  ];
              Cubes[cx,cy,cz].GridPoint[5] := @Grid[cx+1, cy+1, cz  ];
              Cubes[cx,cy,cz].GridPoint[6] := @Grid[cx+1, cy+1, cz+1];
              Cubes[cx,cy,cz].GridPoint[7] := @Grid[cx,   cy+1, cz+1];
            end;
          end;
        end;

        glBegin(GL_TRIANGLES);
        For cx := 0 to GridSize1-2 do
        begin
          for cy := 0 to GridSize2-2 do
          begin
            for cz := 0 to GridSize3-2 do
            begin
              CreateCubeTriangles(Cubes[cx, cy, cz], Value);
            end;
          end;
        end;
        glEnd;
      end;
    end;
  end;
end;

end.
