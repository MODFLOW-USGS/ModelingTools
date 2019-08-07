{******************************************************************************}
{                                                                              }
{ Unit name: JmFloaTJmFloatVector.pas                                          }
{      for the Jedi Math Alpha 1.04 release                                    }
{ Project JEDI Math  http://sourceforge.net/projects/jedimath/                 }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{ or see the file MPL-1.1.txt included in this package.                        }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This is the original code with parts taken and derived from Patrick Van      }
{ Laake's original JmMath.pas.                                                 }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This unit contains interface oriented vector representation and manipulation.}
{                                                                              }
{ Unit owner:  Patrick van Laake                                               }
{ Last modified:                                                               }
{      10.06.2005 by Ralph K. Muench (ralphkmuench@users.sourceforge.net)      }
{      for the Jedi Math Alpha 1.04 prerelease                                 }
{ History:                                                                     }
{      This file was added to the Jedi Math project by Patrick van Laake on    }
{      the 8th Feb. 2003 but was left empty. In the procedure of updating the  }
{      JmMatrix unit for 1.03, many functions and procedures from the old      }
{      JmMatrix were copied here. Also new procedures have been introduced.    }
{                                                                              }
{******************************************************************************}

unit JmFloatVector;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
    Dialogs,
  {$ELSE}
    {$IFDEF WIN32}
      Dialogs,
    {$ENDIF}
    {$IFDEF WIN64}
      Dialogs,
    {$ENDIF}
    {$IFDEF LINUX}
      QDialogs,
    {$ENDIF}
  {$ENDIF}
  SysUtils, Classes, Math, JmTypes;

resourcestring
  rsCantGetVectorFromStrings = 'Can''t get vector from strings.';
  rsDimensionNotAllowed = 'Dimension < 1 not allowed';
  rsDimensionTJmFloatVectorBoundsNotAllowed = 'Dimension > TJmFloatVectorBounds'
    +' not allowed';
  rsElementOutOfBounds = 'Element %d out of bounds';
  rsFalseStringFormat = 'False string format';
  rsVectorDimensionsMustBeEqualForProd = 'Vector dimensions must be equal for '
    +'Prod';
  rsVectorDimensionsMustBeEqualForSum = 'Vector dimensions must be equal for '
    +'Sum';

const
  TJmFloatVectorMax = MaxInt div 8;  //Value of the maximum dimension of a vector

type
  EJmFloatVector = class(Exception);
  {An exception class specially for raising vector exceptions}

  TJmFloatVectorBounds = 1..MaxInt div 8;
  {Range of values for the vector dimension:
  smallest 1, largest MaxInt div 8}

  // TJmFloatArray = array[1..MaxInt div 8] of TJmFloat;
  TJmFloatArray = array[1..MaxInt div 10] of TJmFloat;
  {Dynamically allocated memory for the contents of a
  vector}
  PJmFloatArray = ^TJmFloatArray;
  {Pointer to the memory of a vector}

  TVElementFunc = function(n: TJmFloatVectorBounds): TJmFloat;
  {Any function of this type can be passed to a vector
      and the vector fills it's elements according to the
      function result}

  TVElementPFunc = function(n: TJmFloatVectorBounds; p: Pointer): TJmFloat;
  {The same again, but the function parameters contain a
      pointer - this allows more flexible programming}

  IJmFloatVector = interface['{BFFF5F20-8B92-11D8-B069-000374890932}']
    {This is the GUID string for IJmFloatVector}

    // Memory management for dimensioning
    function GetDim: TJmFloatVectorBounds;
    procedure SetDim(n: TJmFloatVectorBounds);
    property Dim: TJmFloatVectorBounds Read GetDim Write SetDim;

    // Memory management for elements
    function Data: PJmFloatArray;
    function GetElement(n: TJmFloatVectorBounds): TJmFloat;
    procedure SetElement(n: TJmFloatVectorBounds; r: TJmFloat);
    property Element[n: TJmFloatVectorBounds]: TJmFloat Read GetElement Write SetElement; default;

    // Input / Output
    function AsString: string;
    function AsStringF(Format: TFloatFormat; Precision, Digits: integer): string;
    function AsStringMathematica(Precision, Digits: integer): string;
    procedure GetFromString(s: string);
    procedure LoadFromStream(var strm: TStream);
    procedure LoadFromStrings(L: TStrings; Position: longint);
    procedure SaveToStream(var strm: TStream);
    procedure SaveToStrings(L: TStrings);
    procedure SaveToStringsF(L: TStrings; Format: TFloatFormat; Precision, Digits: integer);

    // Calculations
    {The contents of self do not change when these functions are called}
    function AsPolynomeAt(x: TJmFloat): TJmFloat;
    function Mean: TJmFloat;
    function Norm: TJmFloat;
    function ProdWithFloat(x: TJmFloat): IJmFloatVector;
    function Sorted: IJmFloatVector;
    function SortedByMag: IJmFloatVector;
    function StdDev: TJmFloat;
    function SumWithFloat(x: TJmFloat): IJmFloatVector;
    function Variance: TJmFloat;

    // Self manipulations
    procedure AddWithFloat(x: TJmFloat);
    procedure Copy(v: IJmFloatVector);
    procedure MultWithFloat(x: TJmFloat);
    procedure SetElements(f: TVElementFunc);
    procedure SetElementsP(f: TVElementPFunc; P: Pointer);
    procedure SetToGaussian(xMean, stdDev: TJmFloat);
    procedure SetToNull;
    procedure SetToUniform(x: TJmFloat);
    procedure Sort;
    procedure SortByMag;
  end;

// Helper functions
function CopyOf(v: IJmFloatVector): IJmFloatVector; overload;
function Prod(const v1, v2: IJmFloatVector): TJmFloat; overload;
function Sum(const v1, v2: IJmFloatVector): IJmFloatVector; overload;
function Sum(const vectors: array of IJmFloatVector): IJmFloatVector; overload;
function Vector(n: TJmFloatVectorBounds): IJmFloatVector;
function VectorFromStream(var strm: TStream): IJmFloatVector;
function VectorFromString(s: string): IJmFloatVector;
function VectorFromStrings(L: TStrings; Position: longint): IJmFloatVector;


implementation

(************************************************************************)
(************************************************************************)
(************************************************************************)
                       { TJmFloatVector Type }
(************************************************************************)
(************************************************************************)
(************************************************************************)

type
  TJmFloatVector = class(TInterfacedObject, IJmFloatVector)
    FDim:  TJmFloatVectorBounds;
    FData: PJmFloatArray;

    // Miscellaneous

    constructor Create(n: TJmFloatVectorBounds); overload;
    destructor Destroy; override;

    // Memory management
    function Data: PJmFloatArray;
    function GetDim: TJmFloatVectorBounds;
    function GetElement(n: TJmFloatVectorBounds): TJmFloat;
    procedure CheckBounds(const n: TJmFloatVectorBounds);
    procedure SetDim(n: TJmFloatVectorBounds);
    procedure SetElement(n: TJmFloatVectorBounds; r: TJmFloat);
    property Dim: TJmFloatVectorBounds Read GetDim Write SetDim;
    property Element[n: TJmFloatVectorBounds]: TJmFloat Read GetElement Write SetElement; default;

    // Input / Output
    function AsString: string;
    function AsStringF(Format: TFloatFormat; Precision, Digits: integer): string;
    function AsStringMathematica(Precision, Digits: integer): string;
    procedure GetFromString(s: string);
    procedure LoadFromStream(var strm: TStream);
    procedure LoadFromStrings(L: TStrings; Position: longint);
    procedure SaveToStream(var strm: TStream);
    procedure SaveToStrings(L: TStrings);
    procedure SaveToStringsF(L: TStrings; Format: TFloatFormat; Precision, Digits: integer);

    // Calculations
    function AsPolynomeAt(x: TJmFloat): TJmFloat;
    function Mean: TJmFloat;
    function Norm: TJmFloat;
    function ProdWithFloat(x: TJmFloat): IJmFloatVector;
    function Sorted: IJmFloatVector;
    function SortedByMag: IJmFloatVector;
    function StdDev: TJmFloat;
    function SumWithFloat(x: TJmFloat): IJmFloatVector;
    function Variance: TJmFloat;

    // Self manipulations
    procedure AddWithFloat(x: TJmFloat);
    procedure Copy(v: IJmFloatVector);
    procedure MultWithFloat(x: TJmFloat);
    procedure SetElements(f: TVElementFunc);
    procedure SetElementsP(f: TVElementPFunc; P: Pointer);
    procedure SetToGaussian(xMean, stdDev: TJmFloat);
    procedure SetToNull;
    procedure SetToUniform(x: TJmFloat);
    procedure Sort;
    procedure SortByMag;
  end;

(************************************************************************)
(************************************************************************)
(************************************************************************)
                       { HELPER FUNCITONS }
(************************************************************************)
(************************************************************************)
(************************************************************************)

{New for Release 1.03}
function Vector(n: TJmFloatVectorBounds): IJmFloatVector;
begin
  Result := TJmFloatVector.Create(n);
end;

{New for Release 1.03}
function VectorFromString(s: string): IJmFloatVector;
begin
  Result := Vector(1);
  Result.GetFromString(s);
end;

{New for Release 1.03}
function VectorFromStrings(L: TStrings; Position: longint): IJmFloatVector;
begin
  Result := Vector(1);
  Result.LoadFromStrings(L, Position);
end;

{New for Release 1.03}
function VectorFromStream(var strm: TStream): IJmFloatVector;
begin
  Result := Vector(1);
  Result.LoadFromStream(strm);
end;

{New for Release 1.03}
function CopyOf(v: IJmFloatVector): IJmFloatVector;
begin
  Result := Vector(v.Dim);
  Result.Copy(v);
end;

{New for Release 1.03}
function Prod(const v1, v2: IJmFloatVector): TJmFloat; overload {$IFDEF FPC} ; {$ENDIF}
var
  i: longint;
begin
  if v1.Dim <> v2.Dim then
    raise Exception.Create(rsVectorDimensionsMustBeEqualForProd);
  Result := 0;
  for i := 1 to v1.Dim do
    Result := Result + v1[i] * v2[i];
end;

{New for Release 1.03}
function Sum(const v1, v2: IJmFloatVector): IJmFloatVector;
var
  i: longint;
begin
  if v1.Dim <> v2.Dim then
    raise Exception.Create(rsVectorDimensionsMustBeEqualForSum);
  Result := Vector(v1.Dim);
  for i := 1 to v1.Dim do
    Result[i] := v1[i] + v2[i];
end;

{New for Release 1.03}
function Sum(const vectors: array of IJmFloatVector): IJmFloatVector; overload;
var
  i, j: longint;
  Sum:  TJmFloat;
begin
  for j := 0 to Length(vectors) - 2 do
    if vectors[j].Dim <> vectors[j + 1].Dim then
      raise Exception.Create(rsVectorDimensionsMustBeEqualForSum);
  Result := Vector(vectors[0].Dim);
  for i := 1 to vectors[0].Dim do
  begin
    Sum := 0;
    for j := 0 to Length(vectors) - 1 do
      Sum := Sum + vectors[j][i];
    Result[i] := Sum;
  end;
end;

(************************************************************************)
(************************************************************************)
(************************************************************************)
                       { Implementation of TJmFloatVector }
(************************************************************************)
(************************************************************************)
(************************************************************************)

{Taken from Patrick Van Laake's original JmMath.pas}
constructor TJmFloatVector.Create(n: TJmFloatVectorBounds);
begin
  if n < 1 then
    raise EJmFloatVector.Create(rsDimensionNotAllowed);
  if n > TJmFloatVectorMax then
    raise EJmFloatVector.Create(rsDimensionTJmFloatVectorBoundsNotAllowed);
  inherited Create;
  FDim  := n;
  FData := AllocMem(n * SizeOf(TJmFloat));
end;

{Taken from Patrick Van Laake's original JmMath.pas}
destructor TJmFloatVector.Destroy;
begin
  FreeMem(FData);
  inherited;
end;

{Taken from Patrick Van Laake's original JmMath.pas}
procedure TJmFloatVector.CheckBounds(const n: TJmFloatVectorBounds);
begin
  if n > FDim then
    raise EJmFloatVector.Create(Format(rsElementOutOfBounds, [n]));
  if n < 1 then
    raise EJmFloatVector.Create(Format(rsElementOutOfBounds, [n]));
end;

(************************************************************************)
(************************************************************************)
(************************************************************************)
                       { MEMORY MANAGEMENT }
(************************************************************************)
(************************************************************************)
(************************************************************************)

{Derived from Patrick Van Laake's original JmMath.pas}
procedure TJmFloatVector.SetDim(n: TJmFloatVectorBounds);
var
  NewData:  PJmFloatArray;
  MoveSize: longint;
begin
  if n = FDim then
    Exit;
  NewData := AllocMem(n * SizeOf(TJmFloat));
  if n < FDim then
    MoveSize := n * SizeOf(TJmFloat)
  else
    MoveSize := FDim * SizeOf(TJmFloat);
  Move(FData^, NewData^, MoveSize);
  FreeMem(FData);
  FData := NewData;
  FDim  := n;
end;

{Derived from Patrick Van Laake's original JmMath.pas}
function TJmFloatVector.GetDim: TJmFloatVectorBounds;
begin
  Result := FDim;
end;

{Derived from Patrick Van Laake's original JmMath.pas}
function TJmFloatVector.GetElement(n: TJmFloatVectorBounds): TJmFloat;
begin
  CheckBounds(n);
  Result := FData^[n];
end;

{Derived from Patrick Van Laake's original JmMath.pas}
procedure TJmFloatVector.SetElement(n: TJmFloatVectorBounds; r: TJmFloat);
begin
  CheckBounds(n);
  FData^[n] := r;
end;

{New for Release 1.03}
function TJmFloatVector.Data: PJmFloatArray;
begin
  Result := FData;
end;

(************************************************************************)
(************************************************************************)
(************************************************************************)
                       { INPUT / OUTPUT }
(************************************************************************)
(************************************************************************)
(************************************************************************)

 {New for Release 1.03}
 // Format: "{ v[1], v[2], ..... , v[Dim] }"
function TJmFloatVector.AsString: string;
var
  i: longint;
begin
  Result := '{ ' + FloatToStr(Self[1]);
  for i := 2 to FDim do
    Result := Result + ', ' + FloatToStr(Self[i]);
  Result := Result + ' }';
end;

{New for Release 1.03}
function TJmFloatVector.AsStringF(Format: TFloatFormat;
  Precision, Digits: integer): string;
var
  i: longint;
  // The ToStr function is just cosmetic so that positive and negative
  // numbers both have an equal amount of characters.
  function ToStr(r: TJmFloat): string;
  begin
    Result := FloatToStrF(r, Format, Precision, Digits);
    if r > 0 then
      Result := ' ' + Result;
  end;

begin
  Result := '{ ' + ToStr(Self[1]);
  for i := 2 to FDim do
    Result := Result + ', ' + ToStr(Self[i]);
  Result := Result + ' }';
end;

{New for Release 1.03}
// Gives back a string representing the vector in Mathematika
// format - i.e. numbers with the format
// 4.31597291518653244*10^+001
// instead of
// 4.31597291518653244E001
function TJmFloatVector.AsStringMathematica(Precision, Digits: integer): string;
var
  i: longint;
begin
  Result := '{ ' + FloatToStrF(Self[1], FFExponent, Precision, Digits);
  for i := 2 to FDim do
    Result := Result + ', ' + FloatToStrF(Self[i], FFExponent, Precision, Digits);
  Result := Result + ' }';
  while Pos('E', Result) > 0 do
    Result := System.Copy(Result, 1, Pos('E', Result) - 1) + '*10^' +
      System.Copy(Result, Pos('E', Result) + 1, Length(Result));

end;

{New for Release 1.03}
procedure RemoveBrackets(var s: string);
var
  Position: longint;
begin
  while Pos('{', s) > 0 do
  begin
    Position := Pos('{', s);
    s := Copy(s, 1, Position - 1) + Copy(s, Position + 1, Length(s));
  end;
  while Pos('}', s) > 0 do
  begin
    Position := Pos('}', s);
    s := Copy(s, 1, Position - 1) + Copy(s, Position + 1, Length(s));
  end;
end;

 {New for Release 1.03}
 //Returns the first number of the string and deletes
 //it from s. This only works if the numbers are
 //seperated by commas.
function FirstNumber(var s: string): string;
var
  Position: longint;
begin
  if Pos(',', s) > 0 then
  begin
    Position := Pos(',', s);
    Result := Copy(s, 1, Position - 1);
    s := Copy(s, Position + 1, Length(s));
  end
  else
  begin
    Result := s;
    s      := '';
  end;
end;

 {New for Release 1.03}
 // Assumes this Format: "{ v[1], v[2], ..... , v[Dim] }"
procedure TJmFloatVector.GetFromString(s: string);
var
  i, Dimension: longint;
begin
  RemoveBrackets(s);
  Dimension := 1;
  for i := 1 to Length(s) do
    if s[i] = ',' then
      Inc(Dimension);
  Self.Dim := Dimension;
  // Get values
  try
    for i := 1 to FDim do
      Self[i] := StrToFloat(FirstNumber(s));
  except
    raise EJmFloatVector.Create(rsFalseStringFormat);
  end;
end;

 {New for Release 1.03}
 // Writes the dimension into the first line
 // and the data for the elements each into a new line.
procedure TJmFloatVector.SaveToStrings(L: TStrings);
var
  i: longint;
begin
  L.Add(IntToStr(FDim));
  for i := 1 to FDim do
    L.Add(FloatToStr(FData^[i]));
end;

 {New for Release 1.03}
 // Writes the dimension into the first line
 // and the data for the elements each into a new line.
procedure TJmFloatVector.SaveToStringsF(L: TStrings; Format: TFloatFormat;
  Precision, Digits: integer);
var
  i: longint;
begin
  L.Add(IntToStr(FDim));
  for i := 1 to FDim do
    L.Add(FloatToStrF(FData^[i], Format, Precision, Digits));
end;

 {New for Release 1.03}
 // Assumes that the first line is the dimension of the vector
 // and each following line is a seperate number.
procedure TJmFloatVector.LoadFromStrings(L: TStrings; Position: longint);
var
  i: longint;
begin
  try
    Dim := StrToInt(L[Position]);
    for i := 1 to Position do
      FData^[i] := StrToFloat(L[i + Position]);
  except
//    ShowMessage('List of strings had at least one non numeric expression.');
    Dim := 1;
    raise EJmFloatVector.Create(rsCantGetVectorFromStrings);
  end;
end;

 {Derived from Patrick Van Laake's original JmMath.pas}
 // Writes the size of the vector followed by the data.
procedure TJmFloatVector.SaveToStream(var strm: TStream);
begin
  strm.Write(FDim, SizeOf(FDim));
  strm.Write(FData^, FDim * SizeOf(TJmFloat));
end;

 {Derived from Patrick Van Laake's original JmMath.pas}
 // LoadFromStream loads the data for the vector from the strm parameter. The
 // size of the vector should be contained fisrt in the stream folowed
 // by the data.
procedure TJmFloatVector.LoadFromStream(var strm: TStream);
begin
  strm.Read(FDim, SizeOf(FDim));
  FreeMem(FData);
  FData := AllocMem(FDim * SizeOf(TJmFloat));
  strm.Read(FData^, FDim * SizeOf(TJmFloat));
end;

(************************************************************************)
(************************************************************************)
(************************************************************************)
                       { CALCULATIONS }
(************************************************************************)
(************************************************************************)
(************************************************************************)

{New for Release 1.03}
function TJmFloatVector.SumWithFloat(x: TJmFloat): IJmFloatVector;
var
  i: longint;
begin
  Result := Vector(FDim);
  for i := 1 to FDim do
    Result.Data^[i] := x + FData^[i];
end;

{New for Release 1.03}
function TJmFloatVector.ProdWithFloat(x: TJmFloat): IJmFloatVector;
var
  i: longint;
begin
  Result := Vector(FDim);
  for i := 1 to FDim do
    Result.Data^[i] := x * FData^[i];
end;

{New for Release 1.03}
function TJmFloatVector.AsPolynomeAt(x: TJmFloat): TJmFloat;
var
  i: longint;
begin
  Result := 0;
  for i := 1 to FDim do
    Result := Result + FData^[i] * Power(x, i - 1);
end;

{New for Release 1.03}
function TJmFloatVector.Mean: TJmFloat;
var
  i: longint;
begin
  Result := 0;
  for i := 1 to FDim do
    Result := Result + FData^[i];
  Result := Result / FDim;
end;

{New for Release 1.03}
function TJmFloatVector.StdDev: TJmFloat;
begin
  Result := Sqrt(Variance);
end;

{New for Release 1.03}
function TJmFloatVector.Variance: TJmFloat;
var
  i:    longint;
  Temp: TJmFloat;
begin
  Temp   := Mean;
  Result := 0;
  for i := 1 to FDim do
    Result := Result + Sqr(FData^[i] - Temp);
  Result := Result / (FDim - 1);
end;

{New for Release 1.03}
function TJmFloatVector.Sorted: IJmFloatVector;
begin
  Result := Vector(FDim);
  Result.Copy(Self);
  Result.Sort;
end;

{New for Release 1.03}
function TJmFloatVector.SortedByMag: IJmFloatVector;
begin
  Result := Vector(FDim);
  Result.Copy(Self);
  Result.SortByMag;
end;

{New for Release 1.03}
function TJmFloatVector.Norm: TJmFloat;
var
  i: longint;
begin
  Result := 0;
  for i := 1 to FDim do
    Result := Result + Sqr(FData^[i]);
  Result := Result;
end;

(************************************************************************)
(************************************************************************)
(************************************************************************)
                       { SELF MANIPULATIONS }
(************************************************************************)
(************************************************************************)
(************************************************************************)

{New for Release 1.03}
procedure TJmFloatVector.AddWithFloat(x: TJmFloat);
var
  i: longint;
begin
  for i := 1 to FDim do
    FData^[i] := x + FData^[i];
end;

{New for Release 1.03}
procedure TJmFloatVector.MultWithFloat(x: TJmFloat);
var
  i: longint;
begin
  for i := 1 to FDim do
    FData^[i] := x * FData^[i];
end;

{Derived from Patrick Van Laake's original JmMath.pas}
procedure TJmFloatVector.Copy(v: IJmFloatVector);
begin
  Self.Dim := v.Dim;
  Move(v.Data^, FData^, FDim * SizeOf(TJmFloat));
end;

 {New for Release 1.03}
 // Very slow - needs faster method.
 // Smallest items first.
procedure BubbleSort(SortList: PJmFloatArray; L, R: integer);
var
  I, J:   integer;
  LF, RF: TJmFloat;
begin
  for i := L to R - 1 do
    for j := i + 1 to R do
    begin
      LF := SortList^[i];
      RF := SortList^[j];
      if LF > RF then
      begin
        SortList^[i] := RF;
        SortList^[j] := LF;
      end;
    end;
end;

{New for Release 1.03}
procedure TJmFloatVector.Sort;
begin
  BubbleSort(FData, 1, FDim);
end;

 {New for Release 1.03}
 // Very slow - needs faster method.
 // Smallest items first.
procedure BubbleSortMag(SortList: PJmFloatArray; L, R: integer);
var
  I, J:   integer;
  LF, RF: TJmFloat;
begin
  for i := L to R - 1 do
    for j := i + 1 to R do
    begin
      LF := SortList^[i];
      RF := SortList^[j];
      if Abs(LF) > Abs(RF) then
      begin
        SortList^[i] := RF;
        SortList^[j] := LF;
      end;
    end;
end;

{New for Release 1.03}
procedure TJmFloatVector.SortByMag;
begin
  BubbleSortMag(FData, 1, FDim);
end;

{New for Release 1.03}
procedure TJmFloatVector.SetToGaussian(xMean, stdDev: TJmFloat);
var
  i: longint;
begin
  for i := 1 to FDim do
    FData^[i] := RandG(xMean, stdDev);
end;

{New for Release 1.03}
procedure TJmFloatVector.SetElements(f: TVElementFunc);
var
  i: longint;
begin
  for i := 1 to FDim do
    FData^[i] := f(i);
end;

{New for Release 1.03}
procedure TJmFloatVector.SetElementsP(f: TVElementPFunc; P: Pointer);
var
  i: longint;
begin
  for i := 1 to FDim do
    FData^[i] := f(i, P);
end;

{New for Release 1.03}
procedure TJmFloatVector.SetToUniform(x: TJmFloat);
var
  i: longint;
begin
  for i := 1 to FDim do
    FData^[i] := x;
end;

{New for Release 1.03}
procedure TJmFloatVector.SetToNull;
var
  i: longint;
begin
  for i := 1 to FDim do
    FData^[i] := 0;
end;

end.
