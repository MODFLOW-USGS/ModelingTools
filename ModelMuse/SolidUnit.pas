unit SolidUnit;

interface

{
  Adapted from Messner, A.M., and Taylor G.Q, 1980. Algorithm 550, Solid
  Polyhedron Measures[Z], Collected Algorithms from ACM.

  ACM Transactions on Mathematical Software, Vol. 6, No. 1, March 1980. p121-130.
}

uses Classes, SysUtils, IntListUnit, RealListUnit, Dialogs;

type
  TSolid = class(TObject)
    Verticies: TList;
    Faces: TList;
    Constructor Create(Xs, Ys, Zs: TRealList; FaceLists: TIntListList);
      overload;
    // VertexStrings is a series of strings each of which represents one
    // vertex. The X, Y, and Z coordinates of the vertex are in the string
    // in that order and separated by tab characters.
    // FaceStrings is a series of strings each of which represents one facelist.
    // The first integer in the string is the face number. Faces must be
    // consecutively ordered beginning at 0. The numbers in the string are
    // separated by tabs. Each successive number is the number of a vertex
    // in the face. The vertices are numbered beginning with 0. Vertices are
    // A single face may have one or more facelists. If a face has a hole
    // in it, a second facelist will be required to describe the face.
    Constructor Create(VertexStrings, FaceStrings: TStrings); overload;
    {
      C FACE                                                                      3440
      C   A FACE IS REPRESENTED BY THE SET OF VERTEX POINTS OF THE                3450
      C   POLYGONAL SURFACE PATCH, ORDERED BY STARTING WITH AN                    3460
      C   ARBITRARY VERTEX, AND PROCEEDING POINT BY POINT AROUND                  3470
      C   THE BOUNDARY UNTIL THE FIRST POINT IS REACHED.                          3480
      C                                                                           3490
      C FACE LIST                                                                 3500
      C   THE LIST OF THE SET OF VERTEX NUMBERS, ORDERED AS ABOVE                 3510
      C   FOR A FACE, IS CALLED A FACE LIST, WHICH IS THE DATA                    3520
      C   REPRESENTATION OF A FACE.  A FACE LIST HAS THE FORM                     3530
      C        NSF,NA,NB,NC,...,NI,...,NA                                         3540
      C   WHERE NSF IS THE SURFACE NUMBER AND NA,NB,NC,NI ARE                     3550
      C   VERTEX NUMBERS.  NOTE THAT THE FACE LIST IS A CLOSED                    3560
      C   LIST, STARTING AND ENDING WITH THE FIRST POINT SELECTED.                3570
      C   FOR ANY SURFACE WITH ONE OR MORE HOLES IN                               3580
      C   IT, ANY INTERIOR BOUNDARY (HOLE) IS REPRESENTED BY A                    3590
      C   FACE LIST WITH VERTICES ENUMERATED IN A SEQUENCE                        3600
      C   OPPOSITE TO THAT GIVEN FOR AN EXTERIOR BOUNDARY.                        3610
      C
      " If one were to stand on the face with the left foot on the edge of the
      face and the right foot on the interior of the face and then traverse
      forward around the perimeter, vertices would be encountered in the
      required order."
      The vertices on any face are assumed to be coplanar. The faces are assumed
      to completely separate the interior from the exterior. No check is performed
      on either of these assumptions.
    }
    Destructor Destroy; override;
    Procedure GetProps(Var Vol, Area: extended);
    // GetProps is modified from TOMS 550. (Messner and Taylor, 1980.)

  end;

implementation

uses doublePolyhedronUnit, SolidGeom;

// array of weights used for Gaussian quadrature
const
  W: array [1 .. 4] of extended = (-27 / 48, 25 / 48, 25 / 48, 25 / 48);

Type
  { TRealPoint = class(TObject)
    X, Y, Z : extended;
    end; }

  TFace = class(TObject)
  private
    // Perimeters contains lists of TPoints. Each list within Perimeters
    // defines a FACE LIST.
    Perimeters: TList;
    AX, AY, AZ: extended;
  public
    function Area: extended;
    constructor Create;
    Destructor Destroy; override;
    Procedure IntegrateSurface(var Vol: extended);
  end;

  { TFace }

function TFace.Area: extended;
var
  Vol: extended;
begin
  IntegrateSurface(Vol);
  result := Sqrt(Sqr(AX) + Sqr(AY) + Sqr(AZ));
end;

constructor TFace.Create;
begin
  inherited;
  Perimeters := TList.Create;
end;

destructor TFace.Destroy;
var
  Index: integer;
begin
  for Index := 0 to Perimeters.Count - 1 do
  begin
    TList(Perimeters[Index]).Free;
  end;
  Perimeters.Free;
  inherited;
end;

procedure TFace.IntegrateSurface(var Vol: extended);
var
  ListIndex, VertexIndex: integer;
  VertexList: TList;
  Point1, Point2, Point3: TPointObject;
  X, Y, Z: array [1 .. 3] of extended;
  PX, PY, PZ: array [1 .. 4] of extended;
  DX, DY, DZ: extended;
  Index, K: integer;
  ANX, ANY, ANZ, CX, CY, CZ: extended;
  Weight: extended;
begin
  // initialize surface area vector components
  AX := 0;
  AY := 0;
  AZ := 0;
  // loop over FACE LISTS
  for ListIndex := 0 to Perimeters.Count - 1 do
  begin
    VertexList := Perimeters[ListIndex];
    if VertexList.Count > 1 then
    begin
      // Get the first point in the list.
      Point1 := VertexList[0];
      // Get the second point in the list.
      Point2 := VertexList[1];
      // initialize first elements of X,Y, and Z.
      X[1] := Point1.X;
      Y[1] := Point1.Y;
      Z[1] := Point1.Z;
      for VertexIndex := 2 to VertexList.Count - 1 do
      begin
        // Get Third point
        Point3 := VertexList[VertexIndex];

        // initialize remaining elements of X,Y, and Z.
        X[2] := Point2.X;
        Y[2] := Point2.Y;
        Z[2] := Point2.Z;
        X[3] := Point3.X;
        Y[3] := Point3.Y;
        Z[3] := Point3.Z;

        // Get Center point of triangle.
        PX[1] := (Point1.X + Point2.X + Point3.X) / 3;
        PY[1] := (Point1.Y + Point2.Y + Point3.Y) / 3;
        PZ[1] := (Point1.Z + Point2.Z + Point3.Z) / 3;

        // Get other points used for Guassian quadrature
        DX := 0.6 * PX[1];
        DY := 0.6 * PY[1];
        DZ := 0.6 * PZ[1];

        for Index := 1 to 3 do
        begin
          K := Index + 1;
          PX[K] := DX + 0.4 * X[Index];
          PY[K] := DY + 0.4 * Y[Index];
          PZ[K] := DZ + 0.4 * Z[Index];
        end;

        // Calculate area vectors.
        ANX := 0.5 * (Z[1] * (Y[2] - Y[3]) + Z[2] * (Y[3] - Y[1]) + Z[3] *
          (Y[1] - Y[2]));
        ANY := 0.5 * (X[1] * (Z[2] - Z[3]) + X[2] * (Z[3] - Z[1]) + X[3] *
          (Z[1] - Z[2]));
        ANZ := 0.5 * (Y[1] * (X[2] - X[3]) + Y[2] * (X[3] - X[1]) + Y[3] *
          (X[1] - X[2]));

        AX := AX + ANX;
        AY := AY + ANY;
        AZ := AZ + ANZ;

        // calculate volume by Gaussian quadrature
        for Index := 1 to 4 do
        begin
          Weight := W[Index];
          CX := ANX * PX[Index] * Weight;
          CY := ANY * PY[Index] * Weight;
          CZ := ANZ * PZ[Index] * Weight;
          Vol := Vol + (CX + CY + CZ);
        end;

        // make Third point the new second point.
        Point2 := Point3;
      end;
    end;

  end;

end;

{ TSolid }

constructor TSolid.Create(VertexStrings, FaceStrings: TStrings);
var
  Index: integer;
  AVertex: TPointObject;
  AFace: TFace;
  AString: String;
  FaceIndex: integer;
  AList: TList;
  function GetFloat(var AString: String): extended;
  var
    TabPosition: integer;
  begin
    TabPosition := Pos(Chr(9), AString);
    if TabPosition > 0 then
    begin
      result := StrToFloat(Copy(AString, 1, TabPosition - 1));
      AString := Copy(AString, TabPosition + 1, Length(AString));
    end
    else
    begin
      result := StrToFloat(AString);
      AString := '';
    end;
  end;
  function GetInteger(var AString: String): integer;
  var
    TabPosition: integer;
  begin
    TabPosition := Pos(Chr(9), AString);
    if TabPosition > 0 then
    begin
      result := StrToInt(Copy(AString, 1, TabPosition - 1));
      AString := Copy(AString, TabPosition + 1, Length(AString));
    end
    else
    begin
      result := StrToInt(AString);
      AString := '';
    end;
  end;

begin
  inherited Create;
  Verticies := TList.Create;
  Verticies.Capacity := VertexStrings.Count;
  Faces := TList.Create;
  for Index := 0 to VertexStrings.Count - 1 do
  begin
    AVertex := TPointObject.Create;
    Verticies.Add(AVertex);
    AString := VertexStrings[Index];
    AVertex.X := GetFloat(AString);
    AVertex.Y := GetFloat(AString);
    AVertex.Z := GetFloat(AString);
  end;
  for Index := 0 to FaceStrings.Count - 1 do
  begin
    AString := FaceStrings[Index];
    FaceIndex := GetInteger(AString);
    if FaceIndex < Faces.Count then
    begin
      AFace := Faces[FaceIndex];
    end
    else
    begin
      AFace := TFace.Create;
      Faces.Add(AFace);
    end;
    AList := TList.Create;
    AFace.Perimeters.Add(AList);
    while AString <> '' do
    begin
      AVertex := Verticies[GetInteger(AString)];
      AList.Add(AVertex);
    end;
  end;

end;

{ constructor TSolid.Create(VertexCount: integer;
  FaceLists, Verts: TList);
  var
  Index, VertexIndex : integer;
  AFaceList : TIntegerList;
  AVertex : TPoint;
  AFace : TFace;
  FaceIndex : integer;
  AList : TList;
  AVert : TPointArray;
  begin
  Verticies := TList.Create;
  Faces := TList.Create;
  for Index := 0 to VertexCount -1 do
  begin
  AVertex := TPoint.Create;
  Verticies.Add(AVertex);
  AVertex.X := Verts[Index,0];
  AVertex.Y := Verts[Index,1];
  AVertex.Z := Verts[Index,2];
  end;
  for Index := 0 to FaceLists.Count -1 do
  begin
  AFaceList := FaceLists[Index];
  FaceIndex := AFaceList[0];
  if FaceIndex < Faces.Count then
  begin
  AFace := Faces[FaceIndex];
  end
  else
  begin
  AFace := TFace.Create;
  Faces.Add(AFace);
  end;
  AList := TList.Create;
  AFace.Perimeters.Add(AList);
  for VertexIndex := 1 to AFaceList.Count -1 do
  begin
  AList.Add(Verticies[VertexIndex]);
  end;

  end;

  end; }

constructor TSolid.Create(Xs, Ys, Zs: TRealList; FaceLists: TIntListList);
var
  Index, VertexIndex: integer;
  AFaceList: TIntegerList;
  AVertex: TPointObject;
  AFace: TFace;
  FaceIndex: integer;
  AList: TList;
begin
  inherited Create;
  Verticies := TList.Create;
  Verticies.Capacity := Xs.Count;
  Faces := TList.Create;
  for Index := 0 to Xs.Count - 1 do
  begin
    AVertex := TPointObject.Create;
    Verticies.Add(AVertex);
    AVertex.X := Xs[Index];
    AVertex.Y := Ys[Index];
    AVertex.Z := Zs[Index];
  end;
  for Index := 0 to FaceLists.Count - 1 do
  begin
    AFaceList := FaceLists[Index];
    FaceIndex := AFaceList[0];
    if FaceIndex < Faces.Count then
    begin
      AFace := Faces[FaceIndex];
    end
    else
    begin
      AFace := TFace.Create;
      Faces.Add(AFace);
    end;
    AList := TList.Create;
    AList.Capacity := AFaceList.Count - 1;
    AFace.Perimeters.Add(AList);
    for VertexIndex := 1 to AFaceList.Count - 1 do
    begin
      AList.Add(Verticies[AFaceList[VertexIndex]]);
    end;

  end;

end;

destructor TSolid.Destroy;
var
  Index: integer;
begin
  for Index := 0 to Faces.Count - 1 do
  begin
    TFace(Faces[Index]).Free;
  end;
  Faces.Free;

  for Index := 0 to Verticies.Count - 1 do
  begin
    TPointObject(Verticies[Index]).Free;
  end;
  Verticies.Free;
  inherited;
end;

procedure TSolid.GetProps(var Vol, Area: extended);
var
  Index: integer;
  AFace: TFace;
begin
  Area := 0;
  Vol := 0;
  for Index := 0 to Faces.Count - 1 do
  begin
    AFace := Faces[Index];
    AFace.IntegrateSurface(Vol);
    Area := Area + Sqrt(Sqr(AFace.AX) + Sqr(AFace.AY) + Sqr(AFace.AZ));
  end;
  Vol := Vol / 3;
end;

end.
