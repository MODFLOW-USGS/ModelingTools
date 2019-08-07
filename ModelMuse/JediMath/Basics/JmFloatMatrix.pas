{******************************************************************************}
{                                                                              }
{ Unit name: JmMatrix.pas                                                      }
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
{ The Original Code is JmMatrix.pas from the alpha releases                    }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This unit contains interface oriented matrix                                 }
{ representation and manipulation.                                             }
{                                                                              }
{ Unit owner:  Patrick Van Laake                                               }
{ Last modified:                                                               }
{      03.06.2006 by Ralph K. Muench (ralphkmuench@users.sourceforge.net)      }
{      for the Jedi Math Alpha 1.04 prerelease                                 }
{      June 25, 2004 by Ralph K. Muench and Peter Huesser in                   }
{      collaboration with Patrick Van Laake and commuinication with            }
{      Robert Rossmair for the Jedi Math Alpha 1.03 release.                   }
{      The unit has been totally revised in this version leaving many          }
{      Classes from the original code away and introducing interface based     }
{      Matrices.                                                               }
{                                                                              }
{******************************************************************************}

unit JmFloatMatrix;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

{$i ../JmJedi.inc}

interface

uses
  {$IFDEF FPC}
    Dialogs, Grids, LCLProc,
  {$ELSE}
    {$IFDEF WIN32}
      Dialogs, Grids,
    {$ENDIF}
    {$IFDEF WIN64}
      Dialogs, Grids,
    {$ENDIF}
    {$IFDEF LINUX}
      QDialogs, QGrids,
    {$ENDIF}
  {$ENDIF}
  SysUtils, Classes, JmTypes, JmFloatVector, Math, JmBasics;

resourcestring
  rsC1IsOutOfBounds = 'c1 is out of bounds';
  rsC2IsOutOfBounds = 'c2 is out of bounds';
  rsCannotAddMatricesOfDifferentSizes =
    'Cannot add matrices of different sizes';
  rsCannotCreateMatrixWithXColumns = 'Cannot create matrix with %d columns';
  rsCannotCreateMatrixWithXRows = 'Cannot create matrix with %d rows';
  rsCantGetMatrixFromStrings = 'Can''t get matrix from strings.';
  rsColCountNotAllowed = 'ColCount < 1 not allowed';
  rsColCountOfM1MustBeEqualToRowCountOfM2 = 'ColCount of M1 must be equal to '
    +'RowCount of M2';
  rsColCountTJmFloatMatrixBoundsNotAllowed = 'ColCount > TJmFloatMatrixBounds '
    +'not allowed';
  rsColumnOutOfBounds = 'Column %d out of bounds';
  rsDetCalledOnNonSquareMatrix = 'Det called on non square matrix';
  rsDimensionOfMatricesDoesNotMatchForMultiplication = 'Dimension of matrices '
    +'does not match for multiplication';
  rsDimensionOfResultDoesNotMatchRowCount = 'Dimension of result does not '
    +'match RowCount';
  rsDimensionOfVDoesNotMatchColCount = 'Dimension of v does not match ColCount';
  rsDimensionOfVDoesNotMatchRowCount = 'Dimension of v does not match RowCount';
  rsDimensionsOfBothMatricesMustBeIdenticalForDiff = 'Dimensions of both '
    +'matrices must be identical for diff';
  rsDimensionsOfBothMatricesMustBeIdenticalForSum = 'Dimensions of both '
    +'matrices must be identical for sum';
  rsInvertCalledOnNonSquareMatrix = 'Invert called on non square matrix';
  rsInvertSingularMatrix = 'Invert: Singular matrix';
  rsListOfStringsHadAtLeastOneNonNumericExpression = 'List of strings had at '
    +'least one non numeric expression.';
  rsMatrixFromFileFileTooSmall = 'MatrixFromFile: File too small.';
  rsMatrixFromFileFirstTwoLinesMustBeRowCountColCount = 'MatrixFromFile: '
    +'First two lines must be RowCount, ColCount.';
  rsMatrixFromFileFileDoesNotContainEnoughLinesForRowC = 'MatrixFromFile: '
    +'File does not contain enough lines for RowCount.';
  rsMatrixTooLargeToDisplayInStringgrid = 'Matrix too large to display in '
    +'Stringgrid';
  rsR1IsOutOfBounds = 'r1 is out of bounds';
  rsR2IsOutOfBounds = 'r2 is out of bounds';
  rsRowCountNotAllowed = 'RowCount < 1 not allowed';
  rsRowCountTJmFloatMatrixBoundsNotAllowed = 'RowCount > TJmFloatMatrixBounds '
    +'not allowed';
  rsRowOutOfBounds = 'Row %d out of bounds';
  rsSetColFailedDimensionOfVMustBe = 'SetCol failed: Dimension of v must be %d';
  rsSetRowFailedDimensionOfVMustBe = 'SetRow failed: Dimension of v must be %d';

const
  TJmMatrixMax     = MaxInt div 8;
  TJmStringGridMax = 100;
  AlmostZero: TJmFloat = 1.0e-320;

type
  EJmFloatMatrix = class(Exception);
  TJmFloatMatrixBounds = 1..MaxInt div 8;
  TMElementFunc = function(r, c: TJmFloatMatrixBounds): TJmFloat;
  TMElementPFunc = function(r, c: TJmFloatMatrixBounds; p: Pointer): TJmFloat;
     {The same again, but the function parameters contain a
      pointer - this allows more flexible programming}

  IJmFloatMatrix = interface
    ['{DE69EE80-8B92-11D8-B069-000374890932}']
    // This is the GUID string for IJmFloatMatrix

    // Memory management for dimensioning
    function Data: PJmFloatArray;
    function GetColCount: TJmFloatMatrixBounds;
    function GetRowCount: TJmFloatMatrixBounds;
    function GetRowPtr(r: longint): PJmFloatArray;
    procedure Redim(r, c: TJmFloatMatrixBounds);
    procedure SetColCount(c: TJmFloatMatrixBounds);
    procedure SetRowCount(r: TJmFloatMatrixBounds);
    property ColCount: TJmFloatMatrixBounds Read GetColCount Write SetColCount;
    property RowCount: TJmFloatMatrixBounds Read GetRowCount Write SetRowCount;

    // Memory management for Elements
    function GetValue(r, c: TJmFloatMatrixBounds): TJmFloat;
    procedure SetValue(r, c: TJmFloatMatrixBounds; x: TJmFloat);
    property Element[r, c: TJmFloatMatrixBounds]: TJmFloat Read GetValue Write SetValue; default;

    // Memory management for rows and colums
    function GetCol(c: TJmFloatMatrixBounds): IJmFloatVector;
    function GetRow(r: TJmFloatMatrixBounds): IJmFloatVector;
    procedure SetCol(c: TJmFloatMatrixBounds; v: IJmFloatVector);
    procedure SetRow(r: TJmFloatMatrixBounds; const v: IJmFloatVector);
    // setting v to const saves plenty of time. 5'000'000 calls
    // to SetRow on a matrix sized 10x10 took less than 3.1 s
    // on a Pentium II, 400 MHz with const - without const it
    // took 11.9 s.
    property Col[r: TJmFloatMatrixBounds]: IJmFloatVector Read GetCol Write SetCol;
    property Row[r: TJmFloatMatrixBounds]: IJmFloatVector Read GetRow Write SetRow;

    // Input and Output
    function AsString: string;
    function AsStringF(Format: TFloatFormat; Precision, Digits: integer): string;
    procedure GetFromString(s: string);
    procedure LoadFromStream(var strm: TStream);
    procedure LoadFromStrings(L: TStrings; Position: longint);
    procedure SaveToFile(FileName: string);
    procedure SaveToFileF(FileName: string; Format: TFloatFormat; Precision, Digits: integer);
    procedure SaveToFileMathematica(FileName: string; Precision, Digits: integer; MatrixName: string);
    procedure SaveToStream(var strm: TStream);
    procedure SaveToStrings(L: TStrings);
    procedure SaveToStringsF(L: TStrings; Format: TFloatFormat; Precision, Digits: integer);
    procedure ShowInStringGrid(SG: TStringGrid);

    // Element manipulations
    procedure AddToElement(r, c: TJmFloatMatrixBounds; const x: TJmFloat);
    procedure MultWithElement(r, c: TJmFloatMatrixBounds; const x: TJmFloat);
    procedure SwapElements(r1, c1, r2, c2: TJmFloatMatrixBounds);

    // Row and col manipulations
    function ProdWithCol(c: TJmFloatMatrixBounds; v: IJmFloatVector): TJmFloat;
    function ProdWithRow(r: TJmFloatMatrixBounds; v: IJmFloatVector): TJmFloat;
    function SumWithCol(c: TJmFloatMatrixBounds; v: IJmFloatVector): IJmFloatVector;
    function SumWithRow(r: TJmFloatMatrixBounds; v: IJmFloatVector): IJmFloatVector;
    procedure AddProdOfCol1ToCol2(c1: TJmFloatMatrixBounds; Factor: TJmFloat; c2: TJmFloatMatrixBounds);
    procedure AddProdOfRow1ToRow2(r1: TJmFloatMatrixBounds; Factor: TJmFloat; r2: TJmFloatMatrixBounds);
    procedure AddToCol(c: TJmFloatMatrixBounds; v: IJmFloatVector);
    procedure AddToRow(r: TJmFloatMatrixBounds; v: IJmFloatVector);
    procedure SwapCols(c1, c2: TJmFloatMatrixBounds);
    procedure SwapRows(r1, r2: TJmFloatMatrixBounds);

    // Matrix manipulations
    function Norm: TJmFLoat;
    function ProdWithFloat(const x: TJmFLoat): IJmFloatMatrix;
    function ProdWithMatrix(const A: IJmFloatMatrix): IJmFloatMatrix;
    function ProdWithVector(const v: IJmFloatVector): IJmFloatVector; overload;
    function SumWithMatrix(const A: IJmFloatMatrix): IJmFloatMatrix;
    procedure AddMatrix(const A: IJmFloatMatrix);
    procedure Copy(const A: IJmFloatMatrix);
    procedure MultWithFloat(const x: TJmFLoat);
    procedure MultWithMatrix(const A: IJmFloatMatrix);
    procedure MultWithVector(const v: IJmFloatVector);
    procedure ProdWithVector(const v: IJmFloatVector; const Result: IJmFloatVector); overload;
    procedure SubMatrix(const A: IJmFloatMatrix);
    // setting M to const saves time. 1'000'000 calls
    // to AddMatrix on a matrix sized 10x10 took less than 4.4 s
    // on a Pentium II, 400 MHz with const - without const it
    // took 6.6 s.

    // Matrix self manipulations
    function Transposed: IJmFloatMatrix;
    procedure SetElements(f: TMElementFunc);
    procedure SetElementsP(f: TMElementPFunc; P: Pointer);
    procedure SetToGaussian(xMean, stdDev: TJmFloat);
    procedure SetToOne;
    procedure SetToUniformMatrix(x: TJmFLoat);
    procedure SetToZero;
    procedure Transpose;

    // Sophisticated manipulations
    function Det: TJmFloat;
    function GetToRowEschelonForm(var T: IJmFloatMatrix): IJmFloatMatrix;
    function Inverted: IJmFloatMatrix;
    procedure Invert;
    procedure SetToRowEschelonForm(var T: IJmFloatMatrix);
    // Yet to be included:
    // Procedure GaussJordan( var p : IJmFloatMatrix );
  end;

// Helper functions
function CopyOf(M: IJmFloatMatrix): IJmFloatMatrix; overload;
function Diff(const M1, M2: IJmFloatMatrix): IJmFloatMatrix; overload;
function Matrix(r, c: TJmFloatMatrixBounds): IJmFloatMatrix;
function MatrixFromFile(FileName: string): IJmFloatMatrix;
function MatrixFromStream(var strm: TStream): IJmFloatMatrix;
function MatrixFromString(s: string): IJmFloatMatrix;
function MatrixFromStrings(L: TStrings; Position: longint): IJmFloatMatrix;
function Prod(const M1, M2: IJmFloatMatrix): IJmFloatMatrix; overload;
function Sum(const M1, M2: IJmFloatMatrix): IJmFloatMatrix; overload;
//function Sum(const Matrices: array of IJmFloatMatrix): IJmFloatMatrix; overload;
//function Prod(const Matrices: array of IJmFloatMatrix): IJmFloatMatrix; overload

implementation

type
  TMatrix = class(TInterfacedObject, IJmFloatMatrix)
    FCols: TJmFloatMatrixBounds;
    FData: PJmFloatArray;
    FRows: TJmFloatMatrixBounds;

    constructor Create(r, c: TJmFloatMatrixBounds); overload;
    destructor Destroy; override;
    procedure CheckBounds(const r, c: TJmFloatMatrixBounds);

    // Memory management for dimensioning
    function GetColCount: TJmFloatMatrixBounds;
    function GetRowCount: TJmFloatMatrixBounds;
    procedure Redim(r, c: TJmFloatMatrixBounds);
    procedure SetColCount(c: TJmFloatMatrixBounds);
    procedure SetRowCount(r: TJmFloatMatrixBounds);
    property ColCount: TJmFloatMatrixBounds Read GetColCount Write SetColCount;
    property RowCount: TJmFloatMatrixBounds Read GetRowCount Write SetRowCount;

    // Pointer to the vector data
    function Data: PJmFloatArray;
    function GetRowPtr(r: longint): PJmFloatArray;

    // Memory management for Elements
    function GetValue(r, c: TJmFloatMatrixBounds): TJmFloat;
    procedure SetValue(r, c: TJmFloatMatrixBounds; x: TJmFloat);
    property Element[r, c: TJmFloatMatrixBounds]: TJmFloat Read GetValue Write SetValue; default;

    // Memory management for rows and colums
    function GetCol(c: TJmFloatMatrixBounds): IJmFloatVector;
    function GetRow(r: TJmFloatMatrixBounds): IJmFloatVector;
    procedure SetCol(c: TJmFloatMatrixBounds; v: IJmFloatVector);
    procedure SetRow(r: TJmFloatMatrixBounds; const v: IJmFloatVector);
    property Col[r: TJmFloatMatrixBounds]: IJmFloatVector Read GetCol Write SetCol;
    property Row[r: TJmFloatMatrixBounds]: IJmFloatVector Read GetRow Write SetRow;

    // Input and Output
    function AsString: string;
    function AsStringF(Format: TFloatFormat; Precision, Digits: integer): string;
    procedure GetFromString(s: string);
    procedure LoadFromStream(var strm: TStream);
    procedure LoadFromStrings(L: TStrings; Position: longint);
    procedure SaveToFile(FileName: string);
    procedure SaveToFileF(FileName: string; Format: TFloatFormat; Precision, Digits: integer);
    procedure SaveToFileMathematica(FileName: string; Precision, Digits: integer; MatrixName: string);
    procedure SaveToStream(var strm: TStream);
    procedure SaveToStrings(L: TStrings);
    procedure SaveToStringsF(L: TStrings; Format: TFloatFormat; Precision, Digits: integer);
    procedure ShowInStringGrid(SG: TStringGrid);

    // Element manipulations
    procedure AddToElement(r, c: TJmFloatMatrixBounds; const x: TJmFloat);
    procedure MultWithElement(r, c: TJmFloatMatrixBounds; const x: TJmFloat);
    procedure SwapElements(r1, c1, r2, c2: TJmFloatMatrixBounds);

    // Row and col manipulations
    function ProdWithCol(c: TJmFloatMatrixBounds; v: IJmFloatVector): TJmFloat;
    function ProdWithRow(r: TJmFloatMatrixBounds; v: IJmFloatVector): TJmFloat;
    function SumWithCol(c: TJmFloatMatrixBounds; v: IJmFloatVector): IJmFloatVector;
    function SumWithRow(r: TJmFloatMatrixBounds; v: IJmFloatVector): IJmFloatVector;
    procedure AddProdOfCol1ToCol2(c1: TJmFloatMatrixBounds; Factor: TJmFloat; c2: TJmFloatMatrixBounds);
    procedure AddProdOfRow1ToRow2(r1: TJmFloatMatrixBounds; Factor: TJmFloat; r2: TJmFloatMatrixBounds);
    procedure AddToCol(c: TJmFloatMatrixBounds; v: IJmFloatVector);
    procedure AddToRow(r: TJmFloatMatrixBounds; v: IJmFloatVector);
    procedure SwapCols(c1, c2: TJmFloatMatrixBounds);
    procedure SwapRows(r1, r2: TJmFloatMatrixBounds);

    // Matrix manipulations
    function Norm: TJmFLoat;
    function ProdWithFloat(const x: TJmFLoat): IJmFloatMatrix;
    function ProdWithMatrix(const A: IJmFloatMatrix): IJmFloatMatrix;
    function ProdWithVector(const v: IJmFloatVector): IJmFloatVector; overload;
    function SumWithMatrix(const A: IJmFloatMatrix): IJmFloatMatrix;
    procedure AddMatrix(const A: IJmFloatMatrix);
    procedure Copy(const A: IJmFloatMatrix);
    procedure MultWithFloat(const x: TJmFLoat);
    procedure MultWithMatrix(const A: IJmFloatMatrix);
    procedure MultWithVector(const v: IJmFloatVector);
    procedure ProdWithVector(const v: IJmFloatVector; const Result: IJmFloatVector); overload;
    procedure SubMatrix(const A: IJmFloatMatrix);

    // Matrix self manipulations
    function Transposed: IJmFloatMatrix;
    procedure SetElements(f: TMElementFunc);
    procedure SetElementsP(f: TMElementPFunc; P: Pointer);
    procedure SetToGaussian(xMean, stdDev: TJmFloat);
    procedure SetToOne;
    procedure SetToUniformMatrix(x: TJmFLoat);
    procedure SetToZero;
    procedure Transpose;

    // Sophisticated manipulations
    function Det: TJmFloat;
    function GetToRowEschelonForm(var T: IJmFloatMatrix): IJmFloatMatrix;
    function Inverted: IJmFloatMatrix;
    procedure Invert;
    procedure SetToRowEschelonForm(var T: IJmFloatMatrix);
    // Procedure GaussJordan( var p : IJmFloatMatrix );
  end;

(************************************************************************)
(************************************************************************)
(************************************************************************)
                       { HELPER FUNCTIONS }
(************************************************************************)
(************************************************************************)
(************************************************************************)

{New for Release 1.03}
function Matrix(r, c: TJmFloatMatrixBounds): IJmFloatMatrix;
begin
  Result := TMatrix.Create(r, c);
end;

{New for Release 1.03}
function MatrixFromString(s: string): IJmFloatMatrix;
var
  L: TStringList;
  v: IJmFloatVector;
  i: longint;
begin
  L := TStringList.Create;
  while Pos('},', s) > 0 do
  begin
    L.Add(Copy(s, 1, Pos('},', s)));
    s := Copy(s, Pos('},', s) + 2, Length(s));
  end;
  L.Add(s);
  v      := VectorFromString(L[0]);
  Result := Matrix(L.Count, v.Dim);
  Result.SetRow(1, v);
  for i := 2 to Result.RowCount do
  begin
    v := VectorFromString(L[i - 1]);
    Result.SetRow(i, v);
  end;
  L.Free;
end;

{New for Release 1.03}
function MatrixFromStrings(L: TStrings; Position: longint): IJmFloatMatrix;
begin
  Result := TMatrix.Create(1, 1);
  Result.LoadFromStrings(L, Position);
end;

{New for Release 1.03}
function MatrixFromStream(var strm: TStream): IJmFloatMatrix;
begin
  Result := TMatrix.Create(1, 1);
  Result.LoadFromStream(strm);
end;

{New for Release 1.03}
function MatrixFromFile(FileName: string): IJmFloatMatrix;
var
  L, Expanded, L2: TStringList;
  i:    longint;
  R, C: longint;
  v:    IJmFloatVector;
begin
  L := TStringList.Create;
  try
    L.LoadFromFile(FileName);
    if L.Count < 3 then
      raise Exception.Create(rsMatrixFromFileFileTooSmall);
    try
      R := StrToInt(L[0]);
      C := StrToInt(L[1]);
    except
      raise Exception.Create(rsMatrixFromFileFirstTwoLinesMustBeRowCountColCount
        );
    end;
    if L.Count < 2 + R then
      raise Exception.Create(
        rsMatrixFromFileFileDoesNotContainEnoughLinesForRowC);
    Result := Matrix(R, C);
    for i := 1 to R do
    begin
      v := VectorFromString(L[1 + i]);
      Result.Row[i] := v;
    end;
  finally
    L.Free;
  end;
end;

{New for Release 1.03}
function CopyOf(M: IJmFloatMatrix): IJmFloatMatrix;
begin
  Result := Matrix(M.RowCount, M.ColCount);
  Result.Copy(M);
end;

{New for Release 1.03}
function Sum(const M1, M2: IJmFloatMatrix): IJmFloatMatrix;
begin
  if (M1.ColCount <> M2.ColCount) or (M1.RowCount <> M2.RowCount) then
    raise EJmFloatMatrix.Create(rsDimensionsOfBothMatricesMustBeIdenticalForSum
      );
  Result := CopyOf(M1);
  Result.AddMatrix(M2);
end;

{New for Release 1.03}
function Diff(const M1, M2: IJmFloatMatrix): IJmFloatMatrix; overload;
begin
  if (M1.ColCount <> M2.ColCount) or (M1.RowCount <> M2.RowCount) then
    raise EJmFloatMatrix.Create(rsDimensionsOfBothMatricesMustBeIdenticalForDiff
      );
  Result := CopyOf(M1);
  Result.SubMatrix(M2);
end;

{ $note remove commented code}
{New for Release 1.03}
{
function Sum( const Matrices: array of IJmFloatMatrix): IJmFloatMatrix;
begin
end;
}

{New for Release 1.03}
function Prod(const M1, M2: IJmFloatMatrix): IJmFloatMatrix;
var
  R, C, k:    longint;
  TheElement: TJmFloat;
begin
  if M1.ColCount <> M2.RowCount then
    raise EJmFloatMatrix.Create(rsColCountOfM1MustBeEqualToRowCountOfM2);
  Result := Matrix(M1.RowCount, M2.ColCount);
  for R := 1 to M1.RowCount do
    for C := 1 to M2.ColCount do
    begin
      TheElement := 0;
      for k := 1 to M1.ColCount do
        TheElement := TheElement + M1[R, k] * M2[k, C];
      Result[R, C] := TheElement;
    end;
end;

{ $note remove commented code}
{New for Release 1.03}
// function Prod( const Matrices: array of IJmFloatMatrix): IJmFloatMatrix;
// begin
// end;

(************************************************************************)
(************************************************************************)
(************************************************************************)
                       { MISELLANEOUS FUNCTIONS }
(************************************************************************)
(************************************************************************)
(************************************************************************)

{Taken from Patrick Van Laake's original JmMath.pas}
constructor TMatrix.Create(r, c: TJmFloatMatrixBounds);
begin
  if r < 1 then
    raise EJmFloatMatrix.Create(rsRowCountNotAllowed);
  if r > TJmMatrixMax then
    raise EJmFloatMatrix.Create(rsRowCountTJmFloatMatrixBoundsNotAllowed);
  if c < 1 then
    raise EJmFloatMatrix.Create(rsColCountNotAllowed);
  if c > TJmMatrixMax then
    raise EJmFloatMatrix.Create(rsColCountTJmFloatMatrixBoundsNotAllowed);
  inherited Create;
  FCols := c;
  FRows := r;
  FData := AllocMem(r * c * SizeOf(TJmFloat));
end;

{Taken from Patrick Van Laake's original JmMath.pas}
destructor TMatrix.Destroy;
begin
  FreeMem(FData);
  inherited;
end;

{Derived from Patrick Van Laake's original JmMath.pas}
procedure TMatrix.CheckBounds(const r, c: TJmFloatMatrixBounds);
begin
  if r > FRows then
    raise EJmFloatMatrix.Create(Format(rsRowOutOfBounds, [r]));
  if c > FCols then
    raise EJmFloatMatrix.Create(Format(rsColumnOutOfBounds, [c]));
  if r < 1 then
    raise EJmFloatMatrix.Create(Format(rsRowOutOfBounds, [r]));
  if c < 1 then
    raise EJmFloatMatrix.Create(Format(rsColumnOutOfBounds, [c]));
end;

{New for Release 1.03}
function TMatrix.Norm: TJmFLoat;
var
  i, j: longint;
begin
  Result := 0;
  for i := 1 to FRows do
    for j := 1 to FCols do
      Result := Result + Sqr(Self[i, j]);
end;

(************************************************************************)
(************************************************************************)
(************************************************************************)
                 { Memory management for dimensioning }
(************************************************************************)
(************************************************************************)
(************************************************************************)

{Taken from Patrick Van Laake's original JmMath.pas}
procedure TMatrix.SetRowCount(r: TJmFloatMatrixBounds);
var
  NewData:  PJmFloatArray;
  // RowSize : Longint;
  MoveSize: longint;
begin
  if r = FRows then
    Exit;
  if (r < 1) or (r > TJmMatrixMax) then
    raise EJmFloatMatrix.CreateFmt(rsCannotCreateMatrixWithXRows, [r]);

 { $note implement this code block}
 {
 // Patrick suggested this, but I could not get it to work:
 // The ReallocMem function preserves the matrix, so the
 // (expensive) Move is not required. The ZeroMemory
 // function is only necessary to set any new rows to 0.
 RowSize := FCols * SizeOf(TJmFloat);
 ReallocMem(FData, r * RowSize);
 If r > FRows then
  ZeroMemory(pointer(cardinal(FData) + FRows * rowSize), (r – FRows) * rowSize);
  FRows := r;
 }

  NewData := AllocMem(r * FCols * SizeOf(TJmFloat));
  if r > FRows then
    MoveSize := FRows * FCols * SizeOf(TJmFloat)
  else
    MoveSize := r * FCols * SizeOf(TJmFloat);
  Move(FData^, NewData^, MoveSize);
  FreeMem(FData);
  FData := NewData;
  FRows := r;
end;

{Taken from Patrick Van Laake's original JmMath.pas}
function TMatrix.GetRowCount: TJmFloatMatrixBounds;
begin
  Result := FRows;
end;

{ $note remove this commented code}
(*
// This is my original source:
procedure TMatrix.SetColCount( c : TJmFloatMatrixBounds);
var NewData : PJmFloatArray;
    MaxCol, MoveSize : Longint;
    i, j : Longint;
    M : IJmFloatMatrix;
begin
 If c = FCols then Exit;
 If c > TJmMatrixMax then raise EJmFloatMatrix.Create(Format('Row %d out of bounds', [c]));
 If c < 1 then raise EJmFloatMatrix.Create(Format('Row %d out of bounds', [c]));
 { New Matrix with the size it should have }
 M := Matrix( FRows, c );
 If c > FCols then
  MaxCol := FCols
 else
  MaxCol := c;
 For i := 1 to FRows do
  For j := 1 to MaxCol do
   M[ i, j ] := Self[ i, j ];

 FreeMem( FData );
 FData := AllocMem(FRows * c * SizeOf(TJmFloat));
 FCols := c;
 For i := 1 to FRows do
  For j := 1 to MaxCol do
   Self[ i, j ] := M[ i, j ];
end;
*)

{Derived from Patrick Van Laake's original JmMath.pas}
// Patrick suggested this version to be faster...
// Similarly with SetColCount. Creating a new matrix and copying
// the data over twice is very wasteful. Instead, I allocate a
// new block of memory, move the existing data over a row at a
// time, and dispose of the old block of memory.
procedure TMatrix.SetColCount(c: TJmFloatMatrixBounds);
var
  NewData: PJmFloatArray;
  r, MoveSize, oldRowSize, newRowSize: TJmFloatMatrixBounds;
  oldPtr, newPtr: pointer;
begin
  if c = FCols then
    Exit;
  if (c < 1) or (c > TJmMatrixMax) then
    raise EJmFloatMatrix.CreateFmt(rsCannotCreateMatrixWithXColumns, [c]);
  oldRowSize := FCols * SizeOf(TJmFloat);
  newRowSize := c * SizeOf(TJmFloat);
  if FCols < c then
  begin
    NewData  := AllocMem(c * FRows * SizeOf(TJmFloat)); // initialized to 0
    moveSize := oldRowSize;
  end
  else
  begin
    GetMem(NewData, c * FRows * SizeOf(TJmFloat));     // uninitialized, faster
    moveSize := newRowSize;
  end;
  oldPtr := FData;
  newPtr := NewData;
  for r := 1 to FRows do
  begin
    Move(oldPtr^, newPtr^, moveSize);
    oldPtr := pointer(cardinal(oldPtr) + oldRowSize);
    newPtr := pointer(cardinal(newPtr) + newRowSize);
  end;
  FCols := c;
  Dispose(FData);
  FData := NewData;
end;

{Taken from Patrick Van Laake's original JmMath.pas}
function TMatrix.GetColCount: TJmFloatMatrixBounds;
begin
  Result := FCols;
end;

{New for Release 1.03}
procedure TMatrix.Redim(r, c: TJmFloatMatrixBounds);
begin
  // This should really be implemented in a faster way.
  if (FRows = r) and (FCols = c) then
    Exit;

  RowCount := r;
  ColCount := c;
end;

{New for Release 1.03}
function TMatrix.Data: PJmFloatArray;
begin
  Result := FData;
end;

function TMatrix.GetRowPtr(r: longint): PJmFloatArray;
begin
  Result := @(FData^[(r - 1) * FCols + 1]);
end;

(************************************************************************)
(************************************************************************)
(************************************************************************)
                 { Memory management for Elements }
(************************************************************************)
(************************************************************************)
(************************************************************************)

{Taken from Patrick Van Laake's original JmMath.pas}
function TMatrix.GetValue(r, c: TJmFloatMatrixBounds): TJmFloat;
begin
  CheckBounds(r, c);
  Result := FData^[(r - 1) * FCols + c];
end;

{Taken from Patrick Van Laake's original JmMath.pas}
procedure TMatrix.SetValue(r, c: TJmFloatMatrixBounds; x: TJmFloat);
begin
  CheckBounds(r, c);
  FData^[(r - 1) * FCols + c] := x;
end;

(************************************************************************)
(************************************************************************)
(************************************************************************)
                 { Memory management for rows and cols }
(************************************************************************)
(************************************************************************)
(************************************************************************)

{Taken from Patrick Van Laake's original JmMath.pas}
function TMatrix.GetRow(r: TJmFloatMatrixBounds): IJmFloatVector;
var
  MoveSize: longint;
  P: PJmFloatArray;
begin
  if r > FRows then
    raise EJmFloatMatrix.Create(Format(rsRowOutOfBounds, [r]));
  if r < 1 then
    raise EJmFloatMatrix.Create(Format(rsRowOutOfBounds, [r]));
  MoveSize := FCols * SizeOf(TJmFloat);
  Result := Vector(TJmFloatVectorBounds(FCols));
  P := @FData^[(r - 1) * FCols + 1];
  Move(P^, Result.Data^, MoveSize);
end;

{Taken from Patrick Van Laake's original JmMath.pas}
procedure TMatrix.SetRow(r: TJmFloatMatrixBounds; const v: IJmFloatVector);
var
  MoveSize: longint;
  P: PJmFloatArray;
begin
  if FCols <> v.Dim then
    raise EJmFloatMatrix.Create(Format(rsSetRowFailedDimensionOfVMustBe, [FRows]
      ));
  if r > FRows then
    raise EJmFloatMatrix.Create(Format(rsRowOutOfBounds, [r]));
  if r < 1 then
    raise EJmFloatMatrix.Create(Format(rsRowOutOfBounds, [r]));
  MoveSize := FCols * SizeOf(TJmFloat);
  P := @FData^[(r - 1) * FCols + 1];
  Move(v.Data^, P^, MoveSize);
end;

{New for Release 1.03}
function TMatrix.GetCol(c: TJmFloatMatrixBounds): IJmFloatVector;
var
  r: longint;
begin
  if c > FCols then
    raise EJmFloatMatrix.Create(Format(rsColumnOutOfBounds, [c]));
  if c < 1 then
    raise EJmFloatMatrix.Create(Format(rsColumnOutOfBounds, [c]));
  Result := Vector(FRows);
  for r := 1 to FRows do
    Result[r] := Self.Element[r, c];
end;

{New for Release 1.03}
procedure TMatrix.SetCol(c: TJmFloatMatrixBounds; v: IJmFloatVector);
var
  r: longint;
begin
  if FRows <> v.Dim then
    raise EJmFloatMatrix.Create(Format(rsSetColFailedDimensionOfVMustBe, [FCols]
      ));
  if c > FCols then
    raise EJmFloatMatrix.Create(Format(rsColumnOutOfBounds, [c]));
  if c < 1 then
    raise EJmFloatMatrix.Create(Format(rsColumnOutOfBounds, [c]));
  for r := 1 to FRows do
    Self.Element[r, c] := v[r];
end;

(************************************************************************)
(************************************************************************)
(************************************************************************)
                 { Input and output }
(************************************************************************)
(************************************************************************)
(************************************************************************)

{New for Release 1.03}
// Writes this Format: "{{ M[1,1], M[1,2], ..... , M[1,ColCount] },

 //                       ....
 //                      { M[RowCount,1], ..... , M[RowCount,ColCount] } }"
function TMatrix.AsString: string;
var
  RowNum: longint;
begin
  Result := '{ ' + Self.Row[1].AsString;
  for RowNum := 2 to FRows do
    Result := Result + ', ' + Self.Row[RowNum].AsString;
  Result := Result + ' }';
end;

{New for Release 1.03}
// Writes this Format: "{{ M[1,1], M[1,2], ..... , M[1,ColCount] },
//                       ....
//                      { M[RowCount,1], ..... , M[RowCount,ColCount] } }"
function TMatrix.AsStringF(Format: TFloatFormat;
  Precision, Digits: integer): string;
var
  RowNum: longint;
begin
  Result := '{ ' + Self.Row[1].AsStringF(Format, Precision, Digits);
  for RowNum := 2 to FRows do
    Result := Result + ', ' + Self.Row[RowNum].AsStringF(Format, Precision, Digits);
  Result := Result + ' }';
end;

{New for Release 1.03}
// Assumes this Format: "{{ M[1,1], M[1,2], ..... , M[1,ColCount] },
//                       ....
//                       { M[RowCount,1], ..... , M[RowCount,ColCount] } }"
procedure TMatrix.GetFromString(s: string);
begin
end;

{New for Release 1.03}
// Writes the RowCount into the first line then the ColCount
// and the data for the elements each into a new line.
procedure TMatrix.SaveToStrings(L: TStrings);
var
  i: longint;
begin
  L.Add(IntToStr(FRows));
  L.Add(IntToStr(FCols));
  for i := 1 to FRows * FCols do
    L.Add(FloatToStr(FData^[i]));
end;

{New for Release 1.03}
// Writes the RowCount into the first line then the ColCount
// and the data for the elements each into a new line.
procedure TMatrix.SaveToStringsF(L: TStrings; Format: TFloatFormat;
  Precision, Digits: integer);
var
  i: longint;
begin
  L.Add(IntToStr(FRows));
  L.Add(IntToStr(FCols));
  for i := 1 to FRows * FCols do
    L.Add(FloatToStrF(FData^[i], Format, Precision, Digits));
end;

{New for Release 1.03}
// Writes the RowCount into the first line then the ColCount
// and the data for the rows each into a new line.
procedure TMatrix.SaveToFile(FileName: string);
var
  L: TStringList;
  i: longint;
begin
  L := TStringList.Create;
  L.Add(IntToStr(FRows));
  L.Add(IntToStr(FCols));
  for i := 1 to FRows do
    L.Add(Self.Row[i].AsString);
  L.SaveToFile(FileName);
  L.Free;
end;

{New for Release 1.03}
// Writes the RowCount into the first line then the ColCount
// and the data for the rows each into a new line.
procedure TMatrix.SaveToFileF(FileName: string; Format: TFloatFormat;
  Precision, Digits: integer);
var
  L: TStringList;
  i: longint;
begin
  L := TStringList.Create;
  L.Add(IntToStr(FRows));
  L.Add(IntToStr(FCols));
  for i := 1 to FRows do
    L.Add(Self.Row[i].AsStringF(Format, Precision, Digits));
  L.SaveToFile(FileName);
  L.Free;
end;

{New for Release 1.03}
// Writes the Matrix name into the first line then matrix date in the
// following lines in Mathematica format. The file can then be read
// into Mathematica directly.
procedure TMatrix.SaveToFileMathematica(FileName: string;
  Precision, Digits: integer; MatrixName: string);
var
  L: TStringList;
  i: longint;
begin
  L := TStringList.Create;
  L.Add(MatrixName + '=');
  L.Add('{');
  for i := 1 to FRows - 1 do
    L.Add(Self.Row[i].AsStringMathematica(Precision, Digits) + ',');
  L.Add(Self.Row[FRows].AsStringMathematica(Precision, Digits));
  L.Add('}');
  L.SaveToFile(FileName);
  L.Free;
end;

{New for Release 1.03}
// Assumes that the first line is the RowCount, the
// second line the ColCount and that the following
// lines each contain seperate number for the data.
procedure TMatrix.LoadFromStrings(L: TStrings; Position: longint);
var
  RNew, CNew: TJmFloatMatrixBounds;
  i: longint;
begin
  try
    RNew := StrToInt(L[Position]);
    CNew := StrToInt(L[Position + 1]);
    if (RNew * CNew) <> (FRows * FCols) then
    begin
      FreeMem(FData);
      FData := AllocMem(RNew * CNew * SizeOf(TJmFloat));
    end;
    FRows := RNew;
    FCols := CNew;
    for i := 1 to FRows * FCols do
      FData^[i] := StrToFloat(L[i + Position + 1]);
  except
//    ShowMessage(rsListOfStringsHadAtLeastOneNonNumericExpression);
    raise EJmFloatMatrix.Create(rsCantGetMatrixFromStrings);
  end;
end;

{Derived from Patrick Van Laake's original JmMath.pas}
// Writes the RowCount and ColCount of the matrix followed by the data.
procedure TMatrix.SaveToStream(var strm: TStream);
begin
  strm.Write(FRows, SizeOf(FRows));
  strm.Write(FCols, SizeOf(FCols));
  strm.Write(FData^, FRows * FCols * SizeOf(TJmFloat));
end;

{Derived from Patrick Van Laake's original JmMath.pas}
// LoadFromStream loads the data for the matrix from the strm parameter. The
// size of the matrix is read first followed the stream data.
procedure TMatrix.LoadFromStream(var strm: TStream);
var
  RNew, CNew: TJmFloatMatrixBounds;
begin
  strm.Read(RNew, SizeOf(FRows));
  strm.Read(CNew, SizeOf(FCols));
  if (RNew * CNew) <> (FRows * FCols) then
  begin
    FreeMem(FData);
    FData := AllocMem(RNew * CNew * SizeOf(TJmFloat));
  end;
  FRows := RNew;
  FCols := CNew;
  strm.Read(FData^, FRows * FCols * SizeOf(TJmFloat));
end;

{New for Release 1.03}
procedure OptimumWidth_SG(SG: TStringGrid);
var
  i, j:  longint;
  Width: longint;
begin
  for j := 0 to SG.ColCount - 1 do
  begin
    Width := 0;
    for i := 0 to SG.RowCount - 1 do
      if SG.Canvas.TextWidth(SG.Cells[j, i]) > Width then
        Width := SG.Canvas.TextWidth(SG.Cells[j, i]);
    SG.ColWidths[j] := Width + 30;
  end;
end;

{New for Release 1.03}
procedure TMatrix.ShowInStringGrid(SG: TStringGrid);
var
  i, j: longint;
begin
  if (FRows > TJmStringGridMax) or (FCols > TJmStringGridMax) then
    raise Exception.Create(rsMatrixTooLargeToDisplayInStringgrid);
  SG.ColCount := ColCount + 1;
  SG.RowCount := RowCount + 1;
  for i := 1 to RowCount do
    SG.Cells[0, i] := IntToStr(i);
  for j := 1 to ColCount do
    SG.Cells[j, 0] := IntToStr(j);
  for i := 1 to RowCount do
    for j := 1 to ColCount do
      SG.Cells[j, i] := FloatToStr(Self[i, j]);
  OptimumWidth_SG(SG);
end;

(************************************************************************)
(************************************************************************)
(************************************************************************)
                 { Element manipulations }
(************************************************************************)
(************************************************************************)
(************************************************************************)

{New for Release 1.03}
procedure TMatrix.AddToElement(r, c: TJmFloatMatrixBounds; const x: TJmFloat);
begin
  Self[r, c] := Self[r, c] + x;
end;

{New for Release 1.03}
procedure TMatrix.MultWithElement(r, c: TJmFloatMatrixBounds; const x: TJmFloat);
begin
  Self[r, c] := Self[r, c] * x;
end;

{New for Release 1.03}
procedure TMatrix.SwapElements(r1, c1, r2, c2: TJmFloatMatrixBounds);
var
  Temp: TJmFLoat;
begin
  Temp := Self[r1, c1];
  Self[r1, c1] := Self[r2, c2];
  Self[r2, c2] := Temp;
end;

(************************************************************************)
(************************************************************************)
(************************************************************************)
                 { Row and col manipulations }
(************************************************************************)
(************************************************************************)
(************************************************************************)

{New for Release 1.03}
function TMatrix.SumWithRow(r: TJmFloatMatrixBounds;
  v: IJmFloatVector): IJmFloatVector;
var
  i: longint;
begin
  if v.Dim <> FCols then
    raise Exception.Create(rsDimensionOfVDoesNotMatchColCount);
  Result := Vector(v.Dim);
  for i := 1 to FCols do
    Result[i] := Self[r, i] + v[i];
end;

{New for Release 1.03}
procedure TMatrix.AddToRow(r: TJmFloatMatrixBounds; v: IJmFloatVector);
var
  i: longint;
begin
  if v.Dim <> FCols then
    raise Exception.Create(rsDimensionOfVDoesNotMatchColCount);
  for i := 1 to FCols do
    Self[r, i] := Self[r, i] + v[i];
end;

{Derived from Patrick Van Laake's original JmMath.pas}
function TMatrix.ProdWithRow(r: TJmFloatMatrixBounds; v: IJmFloatVector): TJmFloat;
var
  i: longint;
begin
  if v.Dim <> FCols then
    raise Exception.Create(rsDimensionOfVDoesNotMatchColCount);
  Result := 0;
  for i := 1 to FCols do
    Result := Result + Self[r, i] * v[i];
end;

{New for Release 1.03}
function TMatrix.SumWithCol(c: TJmFloatMatrixBounds;
  v: IJmFloatVector): IJmFloatVector;
var
  i: longint;
begin
  if v.Dim <> FRows then
    raise Exception.Create(rsDimensionOfVDoesNotMatchRowCount);
  Result := Vector(v.Dim);
  for i := 1 to FRows do
    Result[i] := Self[i, c] + v[i];
end;

{New for Release 1.03}
procedure TMatrix.AddToCol(c: TJmFloatMatrixBounds; v: IJmFloatVector);
var
  i: longint;
begin
  if v.Dim <> FRows then
    raise Exception.Create(rsDimensionOfVDoesNotMatchRowCount);
  for i := 1 to FRows do
    Self[i, c] := Self[i, c] + v[i];
end;

{New for Release 1.03}
function TMatrix.ProdWithCol(c: TJmFloatMatrixBounds; v: IJmFloatVector): TJmFloat;
var
  i: longint;
begin
  if v.Dim <> FRows then
    raise Exception.Create(rsDimensionOfVDoesNotMatchRowCount);
  Result := 0;
  for i := 1 to FRows do
    Result := Result + Self[i, c] * v[i];
end;

{Derived from Patrick Van Laake's original JmMath.pas}
procedure TMatrix.SwapRows(r1, r2: TJmFloatMatrixBounds);
var
  tmpRow:   PJmFloatArray;
  dataSize: integer; {
    i : Longint;
    Temp : TJmFLoat;    }
begin
  if (r1 < 1) or (r1 > FRows) then
    raise Exception.Create(rsR1IsOutOfBounds);
  if (r2 < 1) or (r2 > FRows) then
    raise Exception.Create(rsR2IsOutOfBounds);
{ $note remove commented code}
{
 For i := 1 to FCols do
  begin
   Temp := Self[ r1, i ];
   Self[ r1, i ] := Self[ r2, i ];
   Self[ r2, i ] := Temp;
  end;
}
  dataSize := FCols * SizeOf(TJmFloat);
  GetMem(tmpRow, dataSize);
  Move(FData^[(r1 - 1) * FCols + 1], tmpRow^, dataSize);
  Move(FData^[(r2 - 1) * FCols + 1], FData^[(r1 - 1) * FCols + 1], dataSize);
  Move(tmpRow^, FData^[(r2 - 1) * FCols + 1], dataSize);
  FreeMem(tmpRow);
end;

{New for Release 1.03}
procedure TMatrix.SwapCols(c1, c2: TJmFloatMatrixBounds);
var
  i:    longint;
  Temp: TJmFLoat;
begin
  if (c1 < 1) or (c1 > FCols) then
    raise Exception.Create(rsC1IsOutOfBounds);
  if (c2 < 1) or (c2 > FCols) then
    raise Exception.Create(rsC2IsOutOfBounds);
  for i := 1 to FRows do
  begin
    Temp := Self[i, c1];
    Self[i, c1] := Self[i, c2];
    Self[i, c2] := Temp;
  end;
end;

{New for Release 1.03}
procedure TMatrix.AddProdOfRow1ToRow2(r1: TJmFloatMatrixBounds;
  Factor: TJmFloat;
  r2: TJmFloatMatrixBounds);
var
  i: longint;
begin
  if (r1 < 1) or (r1 > FRows) then
    raise Exception.Create(rsR1IsOutOfBounds);
  if (r2 < 1) or (r2 > FRows) then
    raise Exception.Create(rsR2IsOutOfBounds);
  for i := 1 to FCols do
    Self[r2, i] := Self[r2, i] + Self[r1, i] * Factor;
end;

{New for Release 1.03}
procedure TMatrix.AddProdOfCol1ToCol2(c1: TJmFloatMatrixBounds;
  Factor: TJmFloat;
  c2: TJmFloatMatrixBounds);
var
  i: longint;
begin
  if (c1 < 1) or (c1 > FCols) then
    raise Exception.Create(rsC1IsOutOfBounds);
  if (c2 < 1) or (c2 > FCols) then
    raise Exception.Create(rsC2IsOutOfBounds);
  for i := 1 to FRows do
    Self[i, c2] := Self[i, c2] + Self[i, c1] * Factor;
end;

(************************************************************************)
(************************************************************************)
(************************************************************************)
                 { Matrix manipulations }
(************************************************************************)
(************************************************************************)
(************************************************************************)

{New for Release 1.03}
function TMatrix.ProdWithFloat(const x: TJmFLoat): IJmFloatMatrix;
var
  r, c: longint;
begin
  Result := Matrix(FRows, FCols);
  for r := 1 to FRows do
    for c := 1 to FCols do
      Result[r, c] := Self[r, c] * x;
end;

{New for Release 1.03}
procedure TMatrix.MultWithFloat(const x: TJmFLoat);
var
  r, c: longint;
begin
  for r := 1 to FRows do
    for c := 1 to FCols do
      Self[r, c] := Self[r, c] * x;
end;

{New for Release 1.03}
function TMatrix.ProdWithVector(const v: IJmFloatVector): IJmFloatVector;
var
  r, c: longint;
  Temp: TJmFloat;
begin
  if v.Dim <> FCols then
    raise Exception.Create(rsDimensionOfVDoesNotMatchColCount);
  Result := Vector(FRows);
  for r := 1 to FRows do
  begin
    Temp := 0;
    for c := 1 to FCols do
      Temp := Temp + v[c] * Self[r, c];
    Result[r] := Temp;
  end;
end;

{New for Release 1.03}
procedure TMatrix.ProdWithVector(const v: IJmFloatVector;
  const Result: IJmFloatVector);
var
  r, c: longint;
  Temp: TJmFloat;
begin
  if v.Dim <> FCols then
    raise Exception.Create(rsDimensionOfVDoesNotMatchColCount);
  if FRows <> Result.Dim then
    raise Exception.Create(rsDimensionOfResultDoesNotMatchRowCount);
  for r := 1 to FRows do
  begin
    Temp := 0;
    for c := 1 to FCols do
      Temp := Temp + v[c] * Self[r, c];
    Result[r] := Temp;
  end;
end;

{New for Release 1.03}
procedure TMatrix.MultWithVector(const v: IJmFloatVector);
var
  r, c: longint;
  Temp: TJmFloat;
  VNew: IJmFloatVector;
begin
  if v.Dim <> FCols then
    raise Exception.Create(rsDimensionOfVDoesNotMatchColCount);
  VNew := Vector(FRows);
  for r := 1 to FRows do
  begin
    Temp := 0;
    for c := 1 to FCols do
      Temp := Temp + v[c] * Self[r, c];
    VNew[r] := Temp;
  end;
  Redim(VNew.Dim, 1);
  for r := 1 to FRows do
    Self[r, 1] := v[r];
end;

{Derived from Patrick Van Laake's original JmMath.pas}
procedure TMatrix.Copy(const A: IJmFloatMatrix);
var
  DataSize: longint;
begin
  Redim(A.RowCount, A.ColCount);
  dataSize := FRows * FCols * SizeOf(TJmFloat);
  Move(A.Data^, FData^, dataSize);
end;

{New for Release 1.03}
function TMatrix.SumWithMatrix(const A: IJmFloatMatrix): IJmFloatMatrix;
var
  r: integer;
  selfData, bData, rData: PJmFloatArray;
begin
  if (A.RowCount <> FRows) or (A.ColCount <> FCols) then
    raise EJmFloatMatrix.Create(rsCannotAddMatricesOfDifferentSizes);
  Result   := Matrix(FRows, FCols);
  selfData := FData;
  bData    := A.Data;
  rData    := Result.Data;
  for r := 1 to FRows * FCols do
    rData^[r] := selfData^[r] + bData^[r];
end;

{New for Release 1.03}
procedure TMatrix.AddMatrix(const A: IJmFloatMatrix);
var
  r: integer;
  selfData, bData: PJmFloatArray;
begin
  if (A.RowCount <> FRows) or (A.ColCount <> FCols) then
    raise EJmFloatMatrix.Create(rsCannotAddMatricesOfDifferentSizes);
  selfData := FData;
  bData    := A.Data;
  for r := 1 to FRows * FCols do
    selfData^[r] := selfData^[r] + bData^[r];
end;

{New for Release 1.03}
procedure TMatrix.SubMatrix(const A: IJmFloatMatrix);
var
  r: integer;
  selfData, bData: PJmFloatArray;
begin
  if (A.RowCount <> FRows) or (A.ColCount <> FCols) then
    raise EJmFloatMatrix.Create(rsCannotAddMatricesOfDifferentSizes);
  selfData := FData;
  bData    := A.Data;
  for r := 1 to FRows * FCols do
    selfData^[r] := selfData^[r] - bData^[r];
end;

{Bug fixed and more speed optimised for Release 1.03a}
function TMatrix.ProdWithMatrix(const A: IJmFloatMatrix): IJmFloatMatrix;
var
  r, c, k: integer;
  sum:     TJmFloat;
begin
  if A.RowCount <> FCols then
    EJmFloatMatrix.Create(rsDimensionOfMatricesDoesNotMatchForMultiplication);
  Result := Matrix(FRows, A.ColCount);
  for r := 1 to FRows do
    for c := 1 to A.ColCount do
    begin
      sum := 0;
      for k := 1 to FCols do
        sum := sum + FData^[(r - 1) * FCols + k] * FData^[(k - 1) * FCols + c];
      { $note remove commented code}
      // This was the source for release 3.0
      // sum := sum + Self[ r, k ] * A[ k, c ];
      // and this is the source for GetElement
      // FData^[(r-1)*FCols+c]
      Result[r, c] := sum;
    end;
end;

{Bug fixed and more speed optimised for Release 1.03a}
procedure TMatrix.MultWithMatrix(const A: IJmFloatMatrix);
var
  r, c, k: integer;
  sum:     TJmFloat;
  Temp:    IJmFloatMatrix;
begin
  if A.RowCount <> FCols then
    EJmFloatMatrix.Create(rsDimensionOfMatricesDoesNotMatchForMultiplication);
  Temp := Matrix(FRows, A.ColCount);
  for r := 1 to FRows do
    for c := 1 to A.ColCount do
    begin
      sum := 0;
      for k := 1 to FCols do
//        sum := sum + FData^[(r - 1) * FCols + k] * FData^[(k - 1) * FCols + c];
      { $note remove commented code}
      // This was the source for release 3.0
       sum := sum + Self[ r, k ] * A[ k, c ];
      // and this is the source for GetElement
      // FData^[(r-1)*FCols+c]
      Temp[r, c] := sum;
    end;
  Copy(Temp);
end;

(************************************************************************)
(************************************************************************)
(************************************************************************)
                 { Matrix self manipulations }
(************************************************************************)
(************************************************************************)
(************************************************************************)

{New for Release 1.03}
function TMatrix.Transposed: IJmFloatMatrix;
var
  r, c: integer;
begin
  Result := Matrix(FCols, FRows);
  for r := 1 to FRows do
    for c := 1 to FCols do
      Result.Element[c, r] := FData^[(r - 1) * FCols + c];
end;

{Derived from Patrick Van Laake's original JmMath.pas}
procedure TMatrix.Transpose;
var
  r, c: integer;
  Temp: IJmFloatMatrix;
begin
  Temp := Matrix(FCols, FRows);
  for r := 1 to FRows do
    for c := 1 to FCols do
      Temp[c, r] := FData^[(r - 1) * FCols + c];
  Copy(Temp);
end;

{New for Release 1.03}
procedure TMatrix.SetToGaussian(xMean, stdDev: TJmFloat);
var
  i: longint;
begin
  for i := 1 to FRows * FCols do
    FData^[i] := RandG(xMean, stdDev);
end;

{New for Release 1.03}
procedure TMatrix.SetElements(f: TMElementFunc);
var
  r, c: integer;
begin
  for r := 1 to FRows do
    for c := 1 to FCols do
      FData^[(r - 1) * FCols + c] := f(r, c);
end;

{New for Release 1.03}
procedure TMatrix.SetElementsP(f: TMElementPFunc; P: Pointer);
var
  r, c: integer;
begin
  for r := 1 to FRows do
    for c := 1 to FCols do
      FData^[(r - 1) * FCols + c] := f(r, c, P);
end;

{New for Release 1.03}
procedure TMatrix.SetToUniformMatrix(x: TJmFLoat);
var
  r, c: integer;
begin
  for r := 1 to FRows do
    for c := 1 to FCols do
      FData^[(r - 1) * FCols + c] := x;
end;

{New for Release 1.03}
procedure TMatrix.SetToZero;
begin
  SetToUniformMatrix(0);
end;

{New for Release 1.03}
procedure TMatrix.SetToOne;
var
  i: longint;
begin
  SetToUniformMatrix(0);
  if FRows < FCols then
    for i := 1 to FRows do
      Self[i, i] := 1
  else
    for i := 1 to FCols do
      Self[i, i] := 1;
end;

(************************************************************************)
(************************************************************************)
(************************************************************************)
                 { Sophisticated manipulations }
(************************************************************************)
(************************************************************************)
(************************************************************************)

{New for Release 1.03}
function TMatrix.Inverted: IJmFloatMatrix;
begin
  Result := CopyOf(Self);
  Result.Invert;
end;

{New for Release 1.03}
{The Gauss-Jordan elimination with partial pivoting - this is not the
fastest method but it is straight forward, easy to understand and
numerically stable. Full pivoting does not improve the algorithm much.
Added by Ralph K. Muench, 11. May 2004}
procedure TMatrix.Invert;
var
  col, i, j, pivrow: integer;
  big, factor: TJmFloat;
  One: IJmFloatMatrix;
begin
  {$IFDEF DEBUG}
    {$IFDEF FPC}
    DebugLn('TMatrix.Invert: Self = ', Self.AsString);
    {$ENDIF}
  {$ENDIF}

  if FRows <> FCols then
    raise EJmFloatMatrix.Create(rsInvertCalledOnNonSquareMatrix);
  One := CopyOf(Self);
  Self.SetToOne;
  pivrow := 0;
  // Self is now one - We'll make One to One and transform self
  for col := 1 to FCols do
  begin
    // ShowMessage( 'Start' );
    // ShowMessage( One.AsString );
    // Find max element in col
    big := 0;
    for i := col to FRows do
      if Abs(One[i, col]) > Abs(big) then
      begin
        big    := One[i, col];
        pivrow := i;
      end;

    if Abs(big) < ThreeFloatEpsilon then
      raise EJmFloatMatrix.Create(rsInvertSingularMatrix);
    // Swap rows if necessary
    if col <> pivrow then
    begin
      One.SwapRows(col, pivrow);
      Self.SwapRows(col, pivrow);
    end;
    // ShowMessage( 'Swapped rows' );
    // ShowMessage( One.AsString );

    // Reduce the current row
    for i := 1 to FCols do
    begin
      One[col, i]  := One[col, i] / big;
      Self[col, i] := Self[col, i] / big;
    end;
    // ShowMessage( 'Reduced row' );
    // ShowMessage( One.AsString );

    // Reduce the other rows
    for i := 1 to FRows do
    begin
      if i = col then
        Continue; // don't reduce the pivot row
      Factor := One[i, col];
      for j := 1 to FCols do
        One[i, j] := One[i, j] - One[col, j] * Factor;
      for j := 1 to FCols do
        Self[i, j] := Self[i, j] - Self[col, j] * Factor;
    end;
    // ShowMessage( 'Reduced rows' );
    // ShowMessage( One.AsString );
  end;
end;

{Taken from Harley Flanders Matrix Project}
procedure TMatrix.SetToRowEschelonForm(var T: IJmFloatMatrix);
begin
end;

{Taken from Harley Flanders Matrix Project}
function TMatrix.GetToRowEschelonForm(var T: IJmFloatMatrix): IJmFloatMatrix;
begin
end;

 {New for Release 1.03}
 // This is a very slow, but understandable DET.
 // Direct implementation of the definition.
function TMatrix.Det: TJmFloat;
var
  MSub: IJmFloatMatrix;
  f:    TJmFloat;
  i, TheCol, NewCol, TheRow: longint;
begin
  if FRows <> FCols then
    raise EJmFloatMatrix.Create(rsDetCalledOnNonSquareMatrix);
  Result := Self[1, 1];
  if FRows = 1 then
    Exit;
  Result := 0;
  for i := 1 to FRows do
  begin
    MSub := Matrix(FRows - 1, FCols - 1);
    for TheCol := 1 to FCols do
    begin
      NewCol := TheCol;
      if TheCol = i then
        Continue;
      if TheCol > i then
        NewCol := TheCol - 1;
      for TheRow := 2 to FRows do
        MSub[TheRow - 1, NewCol] := Self[TheRow, TheCol];
    end;
    f := Self[1, i] * MSub.Det;
    if not (Odd(i)) then
      f := -1 * f;
    Result := Result + f;
  end;
end;

{ $note implement TJmMatrixSquare.GaussJordan}
{Miscellaneous from Patrick Van Laake's original JmMath.pas}
{Has yet to be built in}
(*
// Gauss-Jordan linear algebraic equation solution with full pivoting
//                        self dot x = p
// Self will be converted to its inverse. p is a matrix with
// an equal number of rows as self and any number of columns (simultaneous
// equation systems). p will be converted to the solution x. If p = nil then
// only self will be inverted.
procedure TJmMatrixSquare.GaussJordan(p: TJmMatrix);
var colNdx, rowNdx, pivot: PIntegerArray;
    rowPtr, pivPtr: PJmFloatArray;
    big, v: TJmFloat;
    i, c, r, col, row, rowSize: integer;
begin
  if Assigned(p) and (p.Rows < FRows) then
    raise EJmFloatMatrix.Create('GaussJordan: Solution vector sub-dimensional');

  rowSize := FRows * SizeOf(integer);
  GetMem(colNdx, rowSize);
  try
    GetMem(rowNdx, rowSize);
    try
      pivot := AllocMem(rowSize);  // Assign + initialize to 0
      try

        //Entering the main loop
        col := 0; row := 0; // avoid compiler warnings
        for i := 1 to FRows do begin
          // Finding the pivot element
          big := 0.0;
          for r := 1 to FRows do begin
            rowPtr := GetRow(r);
            if pivot^[r] <> 1 then
              for c := 1 to FCols do
                if pivot^[c] = 0 then begin
                  v := Abs(rowPtr^[c]);
                  if v > big then begin
                    big := v;
                    row := r;
                    col := c;
                  end;
                end else
                  if pivot^[c] > 1 then
                    raise EJmFloatMatrix.Create('GaussJordan: Singular matrix');
          end;
          Inc(pivot^[col]);

          // Interchange rows
          if row <> col then begin
            SwapRows(row, col);
            if p <> nil then
              p.SwapRows(row, col);
          end;

          // Divide the pivot row by the pivot element
          rowNdx^[i] := row;
          colNdx^[i] := col;
          rowPtr := GetRow(col);
          if big < AlmostZero then
            raise EJmFloatMatrix.Create('GaussJordan: Singular Matrix');
          v := 1 / big;
          rowPtr^[col] := 1.0;
          for c := 1 to FCols do
            rowPtr^[c] := rowPtr^[c] * v;
          if p <> nil then begin
            rowPtr := p.Row[col];
            for c := 1 to p.Columns do
              rowPtr^[c] := rowPtr^[c] * v;
          end;

          // Reduce the rows
          for r := 1 to FRows do
            if r <> col then begin
              rowPtr := GetRow(r);
              pivPtr := GetRow(col);
              v := rowPtr^[col];
              rowPtr^[col] := 0.0;
              for c := 1 to FCols do
                rowPtr^[c] := rowPtr^[c] - pivPtr^[c] * v;
              if p <> nil then begin
                rowPtr := p.Row[r];
                pivPtr := p.Row[col];
                for c := 1 to p.Columns do
                  rowPtr^[c] := rowPtr^[c] - pivPtr^[c] * v;
              end;
            end;
        end;  // Main loop

        // Unscramble the result
        for c := FCols downto 1 do
          if rowNdx^[c] <> colNdx^[c] then
            for r := 1 to FRows do begin
              rowPtr := GetRow(r);
              v := rowPtr^[rowNdx^[c]];
              rowPtr^[rowNdx^[c]] := rowPtr^[colNdx^[c]];
              rowPtr^[colNdx^[c]] := v;
            end;

      finally
        FreeMem(pivot);
      end;
    finally
      FreeMem(rowNdx);
    end;
  finally
    FreeMem(colNdx);
  end;

  DataChanged;
end;
*)

{ $note implement TJmMatrixSquare.SolveVector}
(*
function TJmMatrixSquare.SolveVector(b: TJmVector; iter: TJmIterBounds = 0): TJmVector;
begin
  if b.Rows <> FRows then
    raise EJmFloatMatrix.Create('Incompatible matrix sizes');
  Result := TJmVector.Copy(b);
  if not Assigned(FLU) then Decompose;
  (FLU as TJmMatrixDecompose).Solve(Result);
  if iter > 0 then
    (FLU as TJmMatrixDecompose).Improve(self, b, Result, iter);
end;
*)

end.
