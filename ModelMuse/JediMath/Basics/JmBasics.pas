{******************************************************************************}
{                                                                              }
{ Unit name: JmBasics.pas                                                      }
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
{ Pieces of source taken from jclMath.                                         }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ Based upon the jclMath unit, this unit contains:                             }
{ resource strings for exception messages,                                     }
{ logarithmic, transcendental, hyperbolic and exponential functions,           }
{ as well as some unit conversions and miscellaneous functions.                }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ Unit owner:    Chris Eyre                                                    }
{ Last modified:                                                               }
{      28.11.2005 by Ralph K. Muench (ralphkmuench@users.sourceforge.net)      }
{      for the Jedi Math Alpha 1.04 prerelease                                 }
{                                                                              }
{******************************************************************************}

unit JmBasics;

{$IFDEF FPC}
  {$mode objfpc}
{$ENDIF}

interface

{$I ../JediMath.inc}

uses
  {$IFDEF WIN32}
    Windows,
  {$ENDIF}
  {$IFDEF DELPHI6_UP}
    Types,
  {$ENDIF}
  Classes, SysUtils, JmTypes;

resourcestring
  RsDivByZero  = 'Division by zero';
  RsEmptyArray = 'Empty array is not allowed as input parameter';
  RsInvalidRational = 'Invalid rational number';
  RsMathDomainError = 'Domain check failure in JEDI Math';
  RsNaNSignal  = 'NaN signaling %d';
  RsNaNTagError = 'NaN Tag value %d out of range';
  RsNoNaN      = 'NaN expected';
  RsNonPositiveArray = 'Input array contains non-positive or zero values';
  RsRangeError = 'Cannot merge range';
  RsRationalDivByZero = 'Rational division by zero';
  RsUnexpectedDataType = 'Unexpected data type';
  RsUnexpectedValue = 'Unexpected data value';

var
  FloatEpsilon:      TJmFloat;
  ThreeFloatEpsilon: TJmFloat;

{ Unit conversion }

{ $note obsolete function needs to be removed}
function DegMinSecToFloat(const Degs, Mins, Secs: TJmFloat): TJmFloat;
// obsolete (see JclUnitConv)

{ $note obsolete function needs to be removed}
procedure FloatToDegMinSec(const X: TJmFloat; var Degs, Mins, Secs: TJmFloat);
// obsolete (see JclUnitConv)

{ Miscellaneous routines }
function FloatsEqual(const X, Y, Epsilon: TJmFloat): boolean;
function MaxFloat(const X, Y: TJmFloat): TJmFloat;
function MinFloat(const X, Y: TJmFloat): TJmFloat;
procedure DomainCheck(Err: boolean);
procedure SwapFloats(var X, Y: TJmFloat);
procedure SwapLongints(var X, Y: longint);

implementation

//===================================================================================================
// Coordinate conversion
//===================================================================================================

function DegMinSecToFloat(const Degs, Mins, Secs: TJmFloat): TJmFloat; // obsolete
begin
  Result := Degs + (Mins / 60.0) + (Secs / 3600.0);
end;

//--------------------------------------------------------------------------------------------------

procedure FloatToDegMinSec(const X: TJmFloat; var Degs, Mins, Secs: TJmFloat);
// obsolete
var
  Y: TJmFloat;
begin
  Degs := System.Int(X);
  Y    := Frac(X) * 60;
  Mins := System.Int(Y);
  Secs := Frac(Y) * 60;
end;

//===================================================================================================
// Miscellaneous routines
//===================================================================================================

procedure DomainCheck(Err: boolean);
begin
  if Err then
    raise EJmMathError.CreateResRec(@RsMathDomainError);
end;

//--------------------------------------------------------------------------------------------------

function FloatsEqual(const X, Y, Epsilon: TJmFloat): boolean;
begin
  Result := Abs(X - Y) <= Epsilon;
end;

//--------------------------------------------------------------------------------------------------

function MaxFloat(const X, Y: TJmFloat): TJmFloat;
begin
  if X < Y then
    Result := Y
  else
    Result := X;
end;

//--------------------------------------------------------------------------------------------------

function MinFloat(const X, Y: TJmFloat): TJmFloat;
begin
  if X > Y then
    Result := Y
  else
    Result := X;
end;

//--------------------------------------------------------------------------------------------------

procedure SwapFloats(var X, Y: TJmFloat);
var
  T: TJmFloat;
begin
  T := X;
  X := Y;
  Y := T;
end;

//--------------------------------------------------------------------------------------------------

procedure SwapLongints(var X, Y: longint);
var
  Temp: longint;
begin
  Temp := X;
  X    := Y;
  Y    := Temp;
end;

//--------------------------------------------------------------------------------------------------

procedure CalcMachineEpsilon;
var
  One: TJmFloat;
  T:   TJmFloat;
begin
  One := 1.0;
  FloatEpsilon := One;
  repeat
    FloatEpsilon := 0.5 * FloatEpsilon;
    T := One + FloatEpsilon;
  until One = T;
  FloatEpsilon      := 2.0 * FloatEpsilon;
  ThreeFloatEpsilon := 3.0 * FloatEpsilon;
end;

//--------------------------------------------------------------------------------------------------

initialization
    CalcMachineEpsilon;

end.
