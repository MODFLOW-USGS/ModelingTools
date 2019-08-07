unit Math3D;

{$IF CompilerVersion>=23}
{$EXCESSPRECISION OFF}
{$IFEND}

// Vector and matrix math library for OpenGL
// by LI Qingrui
// Feel free to use and modify.

// Note: Matrice used here is row-majored. This means a left-to-right rule
// of matrix concatenation. And there is no need to transpose before calling
// glLoadMatrixf or glMultMatrixf.

interface

// TVector2x

type
  TVector2i = array[0..1] of Integer;
  TVector2f = array[0..1] of Single;
  TVector2d = array[0..1] of Double;
  TVector2p = array[0..1] of Pointer;
const
  NullVector2i: TVector2i = (0,0);
  NullVector2f: TVector2f = (0,0);
  NullVector2d: TVector2d = (0,0);
  NullVector2p: TVector2p = (nil,nil);
  XVector2i: TVector2i = (1,0);
  XVector2f: TVector2f = (1,0);
  XVector2d: TVector2d = (1,0);
  YVector2i: TVector2i = (0,1);
  YVector2f: TVector2f = (0,1);
  YVector2d: TVector2d = (0,1);

// TVector3x

type
  TVector3i = array[0..2] of Integer;
  TVector3f = array[0..2] of Single;
  TVector3d = array[0..2] of Double;
  TVector3p = array[0..2] of Pointer;
  PVector3i = ^TVector3i;
  PVector3f = ^TVector3f;
  PVector3d = ^TVector3d;

  TVector3fDynArray = array of TVector3f;
const
  NullVector3i: TVector3i = (0,0,0);
  NullVector3f: TVector3f = (0,0,0);
  NullVector3d: TVector3d = (0,0,0);
  NullVector3p: TVector3p = (nil,nil,nil);
  XVector3i: TVector3i = (1,0,0);
  XVector3f: TVector3f = (1,0,0);
  XVector3d: TVector3d = (1,0,0);
  YVector3i: TVector3i = (0,1,0);
  YVector3f: TVector3f = (0,1,0);
  YVector3d: TVector3d = (0,1,0);
  ZVector3i: TVector3i = (0,0,1);
  ZVector3f: TVector3f = (0,0,1);
  ZVector3d: TVector3d = (0,0,1);

// TVector4x

type
  TVector4i = array[0..3] of Integer;
  TVector4f = array[0..3] of Single;
  TVector4d = array[0..3] of Double;
  TVector4p = array[0..3] of Pointer;
const
  NullVector4i: TVector4i = (0,0,0,0);
  NullVector4f: TVector4f = (0,0,0,0);
  NullVector4d: TVector4d = (0,0,0,0);
  NullVector4p: TVector4p = (nil,nil,nil,nil);

// Common used

type
  TQuaternion = TVector4f;

// Matrix Types

type
  TMatrix3i = array[0..2] of TVector3i;
  TMatrix3f = array[0..2] of TVector3f;
  TMatrix3d = array[0..2] of TVector3d;
  PMatrix3i = ^TMatrix3i;
  PMatrix3f = ^TMatrix3f;
  PMatrix3d = ^TMatrix3d;
  TMatrix4i = array[0..3] of TVector4i;
  TMatrix4f = array[0..3] of TVector4f;
  TMatrix4d = array[0..3] of TVector4d;
  PMatrix4i = ^TMatrix4i;
  PMatrix4f = ^TMatrix4f;
  PMatrix4d = ^TMatrix4d;

const
  NullMatrix4f: TMatrix4f = (
    (0,0,0,0),
    (0,0,0,0),
    (0,0,0,0),
    (0,0,0,0)
  );
  IdentityMatrix4f: TMatrix4f = (
    (1,0,0,0),
    (0,1,0,0),
    (0,0,1,0),
    (0,0,0,1)
  );

function Vector4f(x, y, z, w: Single): TVector4f; overload;
function Vector4f(const v3: TVector3f; w: Single = 1.0): TVector4f; overload;
function Vector3f(x, y, z: Single): TVector3f; overload;
function Vector3f(const v4: TVector4f): TVector3f; overload;
function VectorAdd(const V1, V2: TVector4f): TVector4f; overload;
function VectorAdd(const V1, V2: TVector3f): TVector3f; overload;
function VectorSubtract(const V1, V2: TVector4f): TVector4f; overload;
function VectorSubtract(const V1, V2: TVector3f): TVector3f; overload;
function VectorScale(const V: TVector3f; Factor: Single): TVector3f;
function VectorLength(const V: TVector3f): Single;
function VectorNormalize(var V: TVector3f): Single;
function VectorDot(const V1, V2: TVector3f): Single;
function VectorCross(const V1, V2: TVector3f): TVector3f;
function VectorLerp(const V1, V2: TVector3f; t: Single): TVector3f;
function NormalLerp(const n1, n2: TVector3f; t: single): TVector3f;
function VectorTransform(const V: TVector4f; M: TMatrix4f): TVector4f; overload;
function VectorTransform(const V: TVector3f; const M: TMatrix4f): TVector3f; overload;
function NormalTransform(const V: TVector3f; const M: TMatrix4f): TVector3f;
function VectorMaximize(const V1, V2: TVector3f): TVector3f;
function VectorMinimize(const V1, V2: TVector3f): TVector3f;
function VectorPerpendicular(const V, N: TVector3f): TVector3f;
function VectorReflect(const V, N: TVector3f): TVector3f;
function Quaternion(Angle: Single; Axis: TVector3f): TQuaternion;
function QuaternionConjugate(const Q: TQuaternion): TQuaternion;
function QuaternionImagePart(const Q: TQuaternion): TVector3f;
function QuaternionMultiply(const qL, qR: TQuaternion): TQuaternion;
function QuaternionSlerp(const QStart, QEnd: TQuaternion; t: Single): TQuaternion;
function QuaternionToMatrix(const Q: TQuaternion): TMatrix4f;
function MatrixMultiply(const M1, M2: TMatrix4f): TMatrix4f;
procedure MatrixTranspose(var M: TMatrix4f);
//procedure MatrixInvert(var M: TMatrix4f);
function MatrixInvert(const M: TMatrix4f): TMatrix4f;
procedure MatrixAdjoint(var M: TMatrix4f);
function MatrixDeterminant(const M: TMatrix4f): Single;
procedure MatrixScale(var M: TMatrix4f; s: Single);
function MatrixScaling(sx, sy, sz: single): TMatrix4f;
function MatrixTranslation(sx, sy, sz: single): TMatrix4f;
function MatrixRotation(Angle: Single; Axis: TVector3f): TMatrix4f;
function MatrixRotationX(Angle: Single): TMatrix4f;
function MatrixRotationY(Angle: Single): TMatrix4f;
function MatrixRotationZ(Angle: Single): TMatrix4f;
function MatrixShearingZ(dx, dy: single): TMatrix4f;
function MatrixShadow(const plane: TVector4f; const lightpos: TVector3f): TMatrix4f; overload;
function MatrixShadow(const plane, lightpos: TVector4f): TMatrix4f; overload;
function MatrixProjection(fov, aspect, nearplane, farplane: single): TMatrix4f;
function MatrixView(const From, At, Worldup: TVector3f): TMatrix4f;

procedure VectorArrayTranslate(var va: array of TVector3f; x, y, z: single);
function VectorArrayLerp(const v1, v2: TVector3fDynArray; t: single): TVector3fDynArray;
function NormalArrayLerp(const n1, n2: TVector3fDynArray; t: single): TVector3fDynArray;
procedure GenerateNormals(out normals: array of TVector3f;
    const vertices: array of TVector3f; const indices: array of TVector3i);

implementation

uses Math;

const
  // some very small numbers
  EPSILON  = 1e-100;
  EPSILON2 = 1e-50;

function Vector4f(x, y, z, w: Single): TVector4f;
begin
  Result[0] := x;
  Result[1] := y;
  Result[2] := z;
  Result[3] := w;
end;

function Vector4f(const v3: TVector3f; w: Single): TVector4f;
begin
  result[0] := v3[0];
  result[1] := v3[1];
  result[2] := v3[2];
  result[3] := w;
end;

function Vector3f(x, y, z: Single): TVector3f;
begin
  Result[0] := x;
  Result[1] := y;
  Result[2] := z;
end;

function Vector3f(const v4: TVector4f): TVector3f;
begin
  result[0] := v4[0] / v4[3];
  result[1] := v4[1] / v4[3];
  result[2] := v4[2] / v4[3];
end;

function VectorAdd(const V1, V2: TVector4f): TVector4f;
begin
  Result[0] := V1[0] + V2[0];
  Result[1] := V1[1] + V2[1];
  Result[2] := V1[2] + V2[2];
  Result[3] := V1[3] + V2[3];
end;

function VectorAdd(const V1, V2: TVector3f): TVector3f;
begin
  Result[0] := V1[0] + V2[0];
  Result[1] := V1[1] + V2[1];
  Result[2] := V1[2] + V2[2];
end;

function VectorSubtract(const V1, V2: TVector4f): TVector4f;
begin
  Result[0] := V1[0] - V2[0];
  Result[1] := V1[1] - V2[1];
  Result[2] := V1[2] - V2[2];
  Result[3] := V1[3] - V2[3];
end;

function VectorSubtract(const V1, V2: TVector3f): TVector3f;
begin
  Result[0] := V1[0] - V2[0];
  Result[1] := V1[1] - V2[1];
  Result[2] := V1[2] - V2[2];
end;

function VectorScale(const V: TVector3f; Factor: Single): TVector3f;
begin
  result[0] := V[0] * Factor;
  result[1] := V[1] * Factor;
  result[2] := V[2] * Factor;
end;

function VectorLength(const V: TVector3f): Single;
begin
  Result := Sqrt(Sqr(V[0]) + Sqr(V[1]) + Sqr(V[2]));
end;

function VectorNormalize(var V: TVector3f): Single;
begin
  Result := Sqrt(Sqr(V[0]) + Sqr(V[1]) + Sqr(V[2]));
  V[0] := V[0] / Result;
  V[1] := V[1] / Result;
  V[2] := V[2] / Result;
end;

function VectorDot(const V1, V2: TVector3f): Single;
begin
  result := V1[0]*V2[0] + V1[1]*V2[1] + V1[2]*V2[2];
end;

function VectorCross(const V1, V2: TVector3f): TVector3f;
begin
  Result[0] := V1[1] * V2[2]-V1[2] * V2[1];
  Result[1] := V1[2] * V2[0]-V1[0] * V2[2];
  Result[2] := V1[0] * V2[1]-V1[1] * V2[0];
end;

function VectorLerp(const V1, V2: TVector3f; t: Single): TVector3f;
begin
  Result[0] := V1[0] + (V2[0] - V1[0]) * t;
  Result[1] := V1[1] + (V2[1] - V1[1]) * t;
  Result[2] := V1[2] + (V2[2] - V1[2]) * t;
end;

function  VectorTransform(const V: TVector3f; const M: TMatrix4f): TVector3f;
// Transforms a vector3 by a given matrix, projecting the result back into w = 1.
var W: Single;
begin
  W := V[0] * M[0, 3] + V[1] * M[1, 3] + V[2] * M[2, 3] + M[3, 3];
  Result[0] := (V[0] * M[0, 0] + V[1] * M[1, 0] + V[2] * M[2, 0] + M[3, 0]) / W;
  Result[1] := (V[0] * M[0, 1] + V[1] * M[1, 1] + V[2] * M[2, 1] + M[3, 1]) / W;
  Result[2] := (V[0] * M[0, 2] + V[1] * M[1, 2] + V[2] * M[2, 2] + M[3, 2]) / W;
end;

function NormalTransform(const V: TVector3f; const M: TMatrix4f): TVector3f;
begin
  Result[0] := V[0] * M[0, 0] + V[1] * M[1, 0] + V[2] * M[2, 0];
  Result[1] := V[0] * M[0, 1] + V[1] * M[1, 1] + V[2] * M[2, 1];
  Result[2] := V[0] * M[0, 2] + V[1] * M[1, 2] + V[2] * M[2, 2];
end;

function VectorTransform(const V: TVector4f; M: TMatrix4f): TVector4f;
// transforms a homogeneous vector by multiplying it with a matrix
begin
  Result[0] := V[0] * M[0, 0] + V[1] * M[1, 0] + V[2] * M[2, 0] + V[3] * M[3, 0];
  Result[1] := V[0] * M[0, 1] + V[1] * M[1, 1] + V[2] * M[2, 1] + V[3] * M[3, 1];
  Result[2] := V[0] * M[0, 2] + V[1] * M[1, 2] + V[2] * M[2, 2] + V[3] * M[3, 2];
  Result[3] := V[0] * M[0, 3] + V[1] * M[1, 3] + V[2] * M[2, 3] + V[3] * M[3, 3];
end;

function VectorMaximize(const V1, V2: TVector3f): TVector3f;
begin
  Result[0] := Max(V1[0], V2[0]);
  Result[1] := Max(V1[1], V2[1]);
  Result[2] := Max(V1[2], V2[2]);
end;

function VectorMinimize(const V1, V2: TVector3f): TVector3f;
begin
  Result[0] := Min(V1[0], V2[0]);
  Result[1] := Min(V1[1], V2[1]);
  Result[2] := Min(V1[2], V2[2]);
end;

function VectorPerpendicular(const V, N: TVector3f): TVector3f;
// calculates a vector perpendicular to N (N is assumed to be of unit length)
// subtract out any component parallel to N
var Dot: Single;
begin
  Dot := VectorDot(V, N);
  Result[0] := V[0]-Dot * N[0];
  Result[1] := V[1]-Dot * N[1];
  Result[2] := V[2]-Dot * N[2];
end;

function VectorReflect(const V, N: TVector3f): TVector3f;
// reflects vector V against N (assumes N is normalized)
var Dot: Single;
begin
  Dot := VectorDot(V, N);
  Result[0] := V[0]-2 * Dot * N[0];
  Result[1] := V[1]-2 * Dot * N[1];
  Result[2] := V[2]-2 * Dot * N[2];
end;

function VectorCosAngle(const V1, V2: TVector3f): Single;
// calculates the cosine of the angle between Vector1 and Vector2
begin
  Result := VectorDot(V1, V2) / (VectorLength(V1) * VectorLength(V2));
end;

function BezierCVCurvePoint(t: single; p0, p1, p2, p3: single): single;
begin
  result := (1-t)*(1-t)*(1-t)*p0 + (1-t)*(1-t)*t*p1 + (1-t)*t*t*p2 + t*t*t*p3;
end;



function Quaternion(Angle: Single; Axis: TVector3f): TQuaternion;
begin
  VectorNormalize(Axis);
  result[0] := sin(Angle/2) * Axis[0];
  result[1] := sin(Angle/2) * Axis[1];
  result[2] := sin(Angle/2) * Axis[2];
  result[3] := cos(Angle/2);
end;

function QuaternionConjugate(const Q: TQuaternion): TQuaternion;
begin
  result[0] := -Q[0];
  result[1] := -Q[1];
  result[2] := -Q[2];
  result[3] := Q[3];
end;

function QuaternionMultiply(const qL, qR: TQuaternion): TQuaternion;
begin
  Result[3] := qL[3] * qR[3] - qL[0] * qR[0] - qL[1] * qR[1] - qL[2] * qR[2];
  Result[0] := qL[3] * qR[0] + qL[0] * qR[3] + qL[1] * qR[2] - qL[2] * qR[1];
  Result[1] := qL[3] * qR[1] + qL[1] * qR[3] + qL[2] * qR[0] - qL[0] * qR[2];
  Result[2] := qL[3] * qR[2] + qL[2] * qR[3] + qL[0] * qR[1] - qL[1] * qR[0];
end;

function QuaternionImagePart(const Q: TQuaternion): TVector3f;
begin
  Result[0] := Q[0];
  Result[1] := Q[1];
  Result[2] := Q[2];
end;

function QuaternionSlerp(const QStart, QEnd: TQuaternion; t: Single): TQuaternion;
// spherical linear interpolation of unit quaternions with spins
// QStart, QEnd - start and end unit quaternions
// t            - interpolation parameter (0 to 1)
var beta,                   // complementary interp parameter
    theta,                  // Angle between A and B
    sint, cost: Single;     // sine, cosine of theta
    bflip: Boolean;         // use negativ t?
begin
  // cosine theta
  cost := VectorCosAngle(QuaternionImagePart(QStart), QuaternionImagePart(QEnd));

  // if QEnd is on opposite hemisphere from QStart, use -QEnd instead
  if cost < 0 then
  begin
    cost := -cost;
    bflip := True;
  end
  else bflip := False;

  // if QEnd is (within precision limits) the same as QStart,
  // just linear interpolate between QStart and QEnd.
  // Can't do spins, since we don't know what direction to spin.
  if (1 - cost) < EPSILON then beta := 1 - t
  else begin // normal case
    theta := arccos(cost);
    sint := sin(theta);
    beta := sin(theta - t * theta) / sint;
    t := sin(t * theta) / sint;
  end;

  if bflip then t := -t;
  // interpolate
  Result[0] := beta * QStart[0] + t * QEnd[0];
  Result[1] := beta * QStart[1] + t * QEnd[1];
  Result[2] := beta * QStart[2] + t * QEnd[2];
  Result[3] := beta * QStart[3] + t * QEnd[3];
end;

function QuaternionToMatrix(const Q: TQuaternion): TMatrix4f;
// Constructs rotation matrix from (possibly non-unit) quaternion.
// Assumes matrix is used to multiply column vector on the left:
// vnew = mat vold.  Works correctly for right-handed coordinate system
// and right-handed rotations.
var
  V: TVector3f;
  SinA, CosA, A, B, C: Extended;
begin
  V := QuaternionImagePart(Q);
  VectorNormalize(V);
  SinCos(Q[3] / 2, SinA, CosA);
  A := V[0] * SinA;
  B := V[1] * SinA;
  C := V[2] * SinA;

  Result := IdentityMatrix4f;
  Result[0, 0] := 1 - 2 * B * B - 2 * C * C;
  Result[0, 1] := 2 * A * B - 2 * CosA * C;
  Result[0, 2] := 2 * A * C + 2 * CosA * B;

  Result[1, 0] := 2 * A * B + 2 * CosA * C;
  Result[1, 1] := 1 - 2 * A * A - 2 * C * C;
  Result[1, 2] := 2 * B * C - 2 * CosA * A;

  Result[2, 0] := 2 * A * C - 2 * CosA * B;
  Result[2, 1] := 2 * B * C + 2 * CosA * A;
  Result[2, 2] := 1 - 2 * A * A - 2 * B * B;
end;

function MatrixMultiply(const M1, M2: TMatrix4f): TMatrix4f;
var I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do
      Result[I, J] := M1[I, 0] * M2[0, J] +
                  M1[I, 1] * M2[1, J] +
                  M1[I, 2] * M2[2, J] +
                  M1[I, 3] * M2[3, J];
end;

function MatrixScaling(sx, sy, sz: single): TMatrix4f;
begin
  Result := IdentityMatrix4f;
  Result[0, 0] := sx;
  Result[1, 1] := sy;
  Result[2, 2] := sz;
end;

function MatrixTranslation(sx, sy, sz: single): TMatrix4f;
begin
  Result := IdentityMatrix4f;
  Result[3, 0] := sx;
  Result[3, 1] := sy;
  Result[3, 2] := sz;
end;

function MatrixRotation(Angle: Single; Axis: TVector3f): TMatrix4f;
var
  cosine, sine, Len, one_minus_cosine: Extended;
begin
  SinCos(Angle, Sine, Cosine);
  one_minus_cosine := 1 - cosine;
  Len := VectorNormalize(Axis);
  if Len = 0 then Result := IdentityMatrix4f
  else begin
    Result[0, 0] := (one_minus_cosine * Sqr(Axis[0])) + Cosine;
    Result[0, 1] := (one_minus_cosine * Axis[0] * Axis[1]) - (Axis[2] * Sine);
    Result[0, 2] := (one_minus_cosine * Axis[2] * Axis[0]) + (Axis[1] * Sine);
    Result[0, 3] := 0;

    Result[1, 0] := (one_minus_cosine * Axis[0] * Axis[1]) + (Axis[2] * Sine);
    Result[1, 1] := (one_minus_cosine * Sqr(Axis[1])) + Cosine;
    Result[1, 2] := (one_minus_cosine * Axis[1] * Axis[2]) - (Axis[0] * Sine);
    Result[1, 3] := 0;

    Result[2, 0] := (one_minus_cosine * Axis[2] * Axis[0]) - (Axis[1] * Sine);
    Result[2, 1] := (one_minus_cosine * Axis[1] * Axis[2]) + (Axis[0] * Sine);
    Result[2, 2] := (one_minus_cosine * Sqr(Axis[2])) + Cosine;
    Result[2, 3] := 0;

    Result[3, 0] := 0;
    Result[3, 1] := 0;
    Result[3, 2] := 0;
    Result[3, 3] := 1;
  end;
end;

function MatrixRotationX(Angle: Single): TMatrix4f;
// creates matrix for rotation about x-axis
var Sine, Cosine: Extended;
begin
  SinCos(Angle, Sine, Cosine);
  Result := IdentityMatrix4f;
  Result[1, 1] := Cosine;
  Result[1, 2] := Sine;
  Result[2, 1] := -Sine;
  Result[2, 2] := Cosine;
end;

function MatrixRotationY(Angle: Single): TMatrix4f;
// creates matrix for rotation about y-axis
var Sine, Cosine: Extended;
begin
  SinCos(Angle, Sine, Cosine);
  Result := IdentityMatrix4f;
  Result[0, 0] := Cosine;
  Result[0, 2] := -Sine;
  Result[2, 0] := Sine;
  Result[2, 2] := Cosine;
end;

function MatrixRotationZ(Angle: Single): TMatrix4f;
// creates matrix for rotation about z-axis
var Sine, Cosine: Extended;
begin
  SinCos(Angle, Sine, Cosine);
  Result := IdentityMatrix4f;
  Result[0, 0] := Cosine;
  Result[0, 1] := Sine;
  Result[1, 0] := -Sine;
  Result[1, 1] := Cosine;
end;

function MatrixDetInternal(a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single;
// internal version for the determinant of a 3x3 matrix
begin
  Result := a1 * (b2 * c3 - b3 * c2) -
            b1 * (a2 * c3 - a3 * c2) +
            c1 * (a2 * b3 - a3 * b2);
end;

function MatrixDeterminant(const M: TMatrix4f): Single;
var a1, a2, a3, a4,
    b1, b2, b3, b4,
    c1, c2, c3, c4,
    d1, d2, d3, d4  : Single;
begin
  a1 := M[0, 0];  b1 := M[0, 1];  c1 := M[0, 2];  d1 := M[0, 3];
  a2 := M[1, 0];  b2 := M[1, 1];  c2 := M[1, 2];  d2 := M[1, 3];
  a3 := M[2, 0];  b3 := M[2, 1];  c3 := M[2, 2];  d3 := M[2, 3];
  a4 := M[3, 0];  b4 := M[3, 1];  c4 := M[3, 2];  d4 := M[3, 3];

  Result := a1 * MatrixDetInternal(b2, b3, b4, c2, c3, c4, d2, d3, d4) -
            b1 * MatrixDetInternal(a2, a3, a4, c2, c3, c4, d2, d3, d4) +
            c1 * MatrixDetInternal(a2, a3, a4, b2, b3, b4, d2, d3, d4) -
            d1 * MatrixDetInternal(a2, a3, a4, b2, b3, b4, c2, c3, c4);
end;

procedure MatrixScale(var M: TMatrix4f; s: Single);
var I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do M[I, J] := M[I, J] * s;
end;

procedure MatrixAdjoint(var M: TMatrix4f); register;
// Adjoint of a 4x4 matrix - used in the computation of the inverse of matrix
var a1, a2, a3, a4,
    b1, b2, b3, b4,
    c1, c2, c3, c4,
    d1, d2, d3, d4: Single;
begin
    a1 :=  M[0, 0]; b1 :=  M[0, 1];
    c1 :=  M[0, 2]; d1 :=  M[0, 3];
    a2 :=  M[1, 0]; b2 :=  M[1, 1];
    c2 :=  M[1, 2]; d2 :=  M[1, 3];
    a3 :=  M[2, 0]; b3 :=  M[2, 1];
    c3 :=  M[2, 2]; d3 :=  M[2, 3];
    a4 :=  M[3, 0]; b4 :=  M[3, 1];
    c4 :=  M[3, 2]; d4 :=  M[3, 3];

    // row column labeling reversed since we transpose rows & columns
    M[0, 0] :=  MatrixDetInternal(b2, b3, b4, c2, c3, c4, d2, d3, d4);
    M[1, 0] := -MatrixDetInternal(a2, a3, a4, c2, c3, c4, d2, d3, d4);
    M[2, 0] :=  MatrixDetInternal(a2, a3, a4, b2, b3, b4, d2, d3, d4);
    M[3, 0] := -MatrixDetInternal(a2, a3, a4, b2, b3, b4, c2, c3, c4);

    M[0, 1] := -MatrixDetInternal(b1, b3, b4, c1, c3, c4, d1, d3, d4);
    M[1, 1] :=  MatrixDetInternal(a1, a3, a4, c1, c3, c4, d1, d3, d4);
    M[2, 1] := -MatrixDetInternal(a1, a3, a4, b1, b3, b4, d1, d3, d4);
    M[3, 1] :=  MatrixDetInternal(a1, a3, a4, b1, b3, b4, c1, c3, c4);

    M[0, 2] :=  MatrixDetInternal(b1, b2, b4, c1, c2, c4, d1, d2, d4);
    M[1, 2] := -MatrixDetInternal(a1, a2, a4, c1, c2, c4, d1, d2, d4);
    M[2, 2] :=  MatrixDetInternal(a1, a2, a4, b1, b2, b4, d1, d2, d4);
    M[3, 2] := -MatrixDetInternal(a1, a2, a4, b1, b2, b4, c1, c2, c4);

    M[0, 3] := -MatrixDetInternal(b1, b2, b3, c1, c2, c3, d1, d2, d3);
    M[1, 3] :=  MatrixDetInternal(a1, a2, a3, c1, c2, c3, d1, d2, d3);
    M[2, 3] := -MatrixDetInternal(a1, a2, a3, b1, b2, b3, d1, d2, d3);
    M[3, 3] :=  MatrixDetInternal(a1, a2, a3, b1, b2, b3, c1, c2, c3);
end;
{
procedure MatrixInvert(var M: TMatrix4f);
var Det: Single;
begin
  Det := MatrixDeterminant(M);
  if Abs(Det) < EPSILON then M := IdentityMatrix4f
                        else
  begin
    MatrixAdjoint(M);
    MatrixScale(M, 1 / Det);
  end;
end;
}
function MatrixInvert(const M: TMatrix4f): TMatrix4f;
var DetInv: Single;
begin
  if (abs(M[3, 3] - 1) > 0.001) or
     (abs(M[0, 3]) > 0.001) or
     (abs(M[1, 3]) > 0.001) or
     (abs(M[2, 3]) > 0.001)
  then raise EInvalidArgument.Create('arg not valid for this MatrixInvert function');
  DetInv := 1 /( M[0, 0] * (M[1, 1] * M[2, 2] - M[1, 2] * M[2, 1]) -
                 M[0, 1] * (M[1, 0] * M[2, 2] - M[1, 2] * M[2, 0]) +
                 M[0, 2] * (M[1, 0] * M[2, 1] - M[1, 1] * M[2, 0]) );

  result[0, 0] :=  DetInv * (M[1, 1] * M[2, 2] - M[1, 2] * M[2, 1]);
  result[0, 1] := -DetInv * (M[0, 1] * M[2, 2] - M[0, 2] * M[2, 1]);
  result[0, 2] :=  DetInv * (M[0, 1] * M[1, 2] - M[0, 2] * M[1, 1]);
  result[0, 3] := 0;

  result[1, 0] := -DetInv * (M[1, 0] * M[2, 2] - M[1, 2] * M[2, 0]);
  result[1, 1] :=  DetInv * (M[0, 0] * M[2, 2] - M[0, 2] * M[2, 0]);
  result[1, 2] := -DetInv * (M[0, 0] * M[1, 2] - M[0, 2] * M[1, 0]);
  result[1, 3] := 0;

  result[2, 0] :=  DetInv * (M[1, 0] * M[2, 1] - M[1, 1] * M[2, 0]);
  result[2, 1] := -DetInv * (M[0, 0] * M[2, 1] - M[0, 1] * M[2, 0]);
  result[2, 2] :=  DetInv * (M[0, 0] * M[1, 1] - M[0, 1] * M[1, 0]);
  result[2, 3] := 0;

  result[3, 0] := -(M[3, 0] * result[0, 0] + M[3, 1] * result[1, 0] + M[3, 2] * result[2, 0]);
  result[3, 1] := -(M[3, 0] * result[0, 1] + M[3, 1] * result[1, 1] + M[3, 2] * result[2, 1]);
  result[3, 2] := -(M[3, 0] * result[0, 2] + M[3, 1] * result[1, 2] + M[3, 2] * result[2, 2]);
  result[3, 3] := 1;
end;

procedure MatrixTranspose(var M: TMatrix4f);
var
  I, J: Integer;
  t: Single;
begin
  for I := 0 to 2 do
    for J := I+1 to 3 do
    begin
      t := M[I, J];
      M[I, J] := M[J, I];
      M[J, I] := t;
    end;
end;

function MatrixShearingZ(dx, dy: single): TMatrix4f;
begin
  result := IdentityMatrix4f;
  result[0, 2] := dx;
  result[1, 2] := dy;
end;

procedure VectorArrayTranslate(var va: array of TVector3f; x, y, z: single);
var
  i: integer;
  t: TVector3f;
begin
  t := Vector3f(x, y, z);
  for i := 0 to High(va) do
    va[i] := VectorAdd(va[i], t);
end;

function VectorArrayLerp(const v1, v2: TVector3fDynArray; t: single): TVector3fDynArray;
var i, len: integer;
begin
  Assert(Length(v1) = Length(v2));
  len := Length(v1);
  if t = 0 then result := v1
  else if t = 1 then result := v2
  else begin
    SetLength(result, len);
    for i := 0 to len-1 do
      result[i] := VectorLerp(v1[i], v2[i], t);
  end;
end;

function NormalLerp(const n1, n2: TVector3f; t: single): TVector3f;
begin
  result := VectorLerp(n1, n2, t);
  VectorNormalize(result);
end;

function NormalArrayLerp(const n1, n2: TVector3fDynArray; t: single): TVector3fDynArray;
var i, len: integer;
begin
  Assert(Length(n1) = Length(n2));
  if t = 0 then result := n1
  else if t = 1 then result := n2
  else begin
    len := Length(n1);
    SetLength(result, len);
    for i := 0 to len-1 do
    begin
      result[i] := VectorLerp(n1[i], n2[i], t);
      VectorNormalize(result[i]);
    end;
  end;
end;

function MatrixShadow(const plane, lightpos: TVector4f): TMatrix4f;
var dot: single;
begin
  dot := plane[0] * lightpos[0] + plane[1] * lightpos[1] +
    plane[2] * lightpos[2] + plane[3] * lightpos[3];
  result[0, 0] := dot - lightpos[0] * plane[0];
  result[1, 0] :=     - lightpos[0] * plane[1];
  result[2, 0] :=     - lightpos[0] * plane[2];
  result[3, 0] :=     - lightpos[0] * plane[3];

  result[0, 1] :=     - lightpos[1] * plane[0];
  result[1, 1] := dot - lightpos[1] * plane[1];
  result[2, 1] :=     - lightpos[1] * plane[2];
  result[3, 1] :=     - lightpos[1] * plane[3];

  result[0, 2] :=     - lightpos[2] * plane[0];
  result[1, 2] :=     - lightpos[2] * plane[1];
  result[2, 2] := dot - lightpos[2] * plane[2];
  result[3, 2] :=     - lightpos[2] * plane[3];

  result[0, 3] :=     - lightpos[3] * plane[0];
  result[1, 3] :=     - lightpos[3] * plane[1];
  result[2, 3] :=     - lightpos[3] * plane[2];
  result[3, 3] := dot - lightpos[3] * plane[3];
end;

function MatrixShadow(const plane: TVector4f; const lightpos: TVector3f): TMatrix4f;
begin
  result := MatrixShadow(plane, Vector4f(lightpos));
end;

function MatrixProjection(fov, aspect, nearplane, farplane: single): TMatrix4f;
var
  w, h, q: Single;
begin
  h := (cos(FOV/2)/sin(FOV/2));
  w := Aspect * h;
  Q := FarPlane / (FarPlane - NearPlane);
  result := NullMatrix4f;
  result[0, 0] := w;
  result[1, 1] := h;
  result[2, 2] := Q;
  result[2, 3] := 1.0;
  result[3, 2] := -Q * NearPlane;
end;

function MatrixView(const From, At, Worldup: TVector3f): TMatrix4f;
var
  View: TVector3f;
  DotProduct: Single;
  Up, Right: TVector3f;
begin
  // Get the z basis vector, which points straight ahead. This is the
  // difference from the eyepoint to the lookat point.
  View := VectorSubtract(At, From);
  // Normalize the z basis vector
  VectorNormalize(View);
  // Get the dot product, and calculate the projection of the z basis
  // vector onto the up vector. The projection is the y basis vector.
  DotProduct := VectorDot(WorldUp, View);
  Up := VectorSubtract(WorldUp, VectorScale(View, DotProduct));
  // Normalize the y basis vector
  VectorNormalize(Up);
  // The x basis vector is found simply with the cross product of the y
  // and z basis vectors
  Right := VectorCross(Up, View);
  // Start building the matrix. The first three rows contains the basis
  // vectors used to rotate the view to point at the lookat point
  Result[0, 0] := Right[0];  Result[0, 1] := Up[0];  Result[0, 2] := View[0];  Result[0, 3] := 0;
  Result[1, 0] := Right[1];  Result[1, 1] := Up[1];  Result[1, 2] := View[1];  Result[1, 3] := 0;
  Result[2, 0] := Right[2];  Result[2, 1] := Up[2];  Result[2, 2] := View[2];  Result[2, 3] := 0;
  // Do the translation values (rotations are still about the eyepoint)
  Result[3, 0] := - VectorDot(From, Right);
  Result[3, 1] := - VectorDot(From, Up);
  Result[3, 2] := - VectorDot(From, View);
  Result[3, 3] := 1;
end;

procedure GenerateNormals(out normals: array of TVector3f;
    const vertices: array of TVector3f; const indices: array of TVector3i);
var
  i: integer;
  v: TVector3f;
begin
  FillChar(normals, Length(normals) * sizeof(TVector3f), 0);
  for i := 0 to Length(indices) - 1 do
  begin
    v := VectorCross(VectorSubtract(vertices[indices[i, 2]], vertices[indices[i, 1]]),
        VectorSubtract(vertices[indices[i, 0]], vertices[indices[i, 1]]));
    VectorNormalize(v);
    normals[indices[i, 0]] := VectorAdd(normals[indices[i, 0]], v);
    normals[indices[i, 1]] := VectorAdd(normals[indices[i, 1]], v);
    normals[indices[i, 2]] := VectorAdd(normals[indices[i, 2]], v);
  end;
  for i := 0 to length(normals) - 1 do
    VectorNormalize(normals[i]);
end;

end.
