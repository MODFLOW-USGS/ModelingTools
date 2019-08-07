{******************************************************************************}
{                                                                              }
{ Unit name: JmTypes.pas                                                       }
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
{  The Original Code is JmTypes.pas.                                           }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This unit contains various type declarations for the most common types used  }
{ in the JEDI Math project. The types include:                                 }
{ Number types such as TJmFloat and TJmInteger,                                }
{ an exception class EJmMathError,                                             }
{ array and function types.                                                    }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ Unit owner:    Chris Eyre                                                    }
{ Last modified:                                                               }
{      10.06.2005 by Ralph K. Muench (ralphkmuench@users.sourceforge.net)      }
{      for the Jedi Math Alpha 1.04 prerelease                                 }
{                                                                              }
{******************************************************************************}

unit JmTypes;

{$IFDEF FPC}
  {$mode objfpc}
{$ENDIF}

{$I ../JediMath.inc}

interface

uses
  {$IFDEF WIN32}
    Windows,
  {$ENDIF}
  {$IFDEF DELPHI6_UP}
    Types,
  {$ENDIF}
  Classes,
  SysUtils;

type
  {$ifdef jmDoublePrecision}
    TJmFloat = double;
  {$endif jmDoublePrecision}
  {$ifdef jmSinglePrecision}
    TJmFloat = single;
  {$endif jmSinglePrecision}
  {$ifdef jmExtendedPrecision}
    TJmFloat = extended;
  {$endif jmExtendedPrecision}

  TJmInteger = longint;

  PLargeInteger = ^TLargeInteger;
  TLargeInteger = record
    case integer of
      0: (
        LowPart: longword;
        HighPart: longint);
      1: (
        QuadPart: int64);
  end;

  { $warning remove comment from code}
  {
// The following declaration is identical to
// the above and will be romeved.
  PULargeInteger = ^TULargeInteger;
  TULargeInteger = record
    case Integer of
    0: (
      LowPart: LongWord;
      HighPart: LongWord);
    1: (
      QuadPart: Int64);
  end;
       }

  TDynByteArray     = array of byte;
  TDynCardinalArray = array of cardinal;
  TDynDoubleArray   = array of double;
  TDynExtendedArray = array of extended;
  TDynFloatArray    = array of TJmFloat;
  TDynInt64Array    = array of int64;
  TDynIntegerArray  = array of integer;
  TDynLongintArray  = array of longint;
  TDynPointerArray  = array of Pointer;
  TDynShortintArray = array of shortint;
  TDynSingleArray   = array of single;
  TDynSmallintArray = array of smallint;
  TDynStringArray   = array of string;
  TDynWordArray     = array of word;

  { $warning remove comment from code}
 // TPrimalityTestMethod is no longer in use:
 // Will not be available in future versisions if
 // there is no demand for this type.
{
type
  TPrimalityTestMethod = (ptTrialDivision, ptRabinMiller);
}

  EJmMathError = class(Exception)
  public
    constructor CreateResRec(ResStringRec: PResStringRec);
    constructor CreateResRecFmt(ResStringRec: PResStringRec; const Args: array of const);
  end;

implementation

{ EJmMathError }

constructor EJmMathError.CreateResRec(ResStringRec: PResStringRec);
begin
  {$IFDEF FPC}
    inherited Create(ResStringRec^);
  {$ELSE FPC}
    inherited Create(LoadResString(ResStringRec));
  {$ENDIF FPC}
end;

constructor EJmMathError.CreateResRecFmt(ResStringRec: PResStringRec;
  const Args: array of const);
begin
  {$IFDEF FPC}
    inherited CreateFmt(ResStringRec^, Args);
  {$ELSE FPC}
    inherited CreateFmt(LoadResString(ResStringRec), Args);
  {$ENDIF FPC}
end;

end.
