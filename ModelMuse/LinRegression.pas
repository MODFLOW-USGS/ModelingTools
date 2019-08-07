

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower SysTools
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* SysTools: StStat.pas 4.03                             *}
{*********************************************************}
{* SysTools: Statistical math functions modeled on       *}
{*           those in Excel                              *}
{*********************************************************}

// Modified by Richard B. Winston to include only code for linear regression
// and to make comments compatible with TPasDoc.

{@abstract(@name is used to perform linear regression.)}
unit LinRegression;

interface

uses SysUtils;

const
  {ststat errors}

  {unequal or bad counts of array elements}
  stscStatBadCount   = 170;

  {invalid parameter}
  stscStatBadParam   = 171;

  {invalid data point in array}
  stscStatBadData    = 172;

  {no convergence in numerical routine}
  stscStatNoConverge = 173;

{.Z+}
  StMaxBlockSize = MaxLongInt;
{.Z-}

type
{.Z+}
  PDouble = ^Double;
  TDoubleArray = array[0..(stMaxBlockSize div SizeOf(Double))-1] of Double;
  PDoubleArray = ^TDoubleArray;
  TIntArray = array[0..(StMaxBlockSize div SizeOf(Integer))-1] of Integer;
  PIntArray = ^TIntArray;
{.Z-}

type
  {@abstract(ancestor to all SysTools exceptions)}
  EStException = class(Exception)
    protected {private}
      FErrorCode : Longint;

    public
      constructor CreateResTP(Ident : LongInt; Dummy : Word);
      constructor CreateResFmtTP(Ident : Longint; const Args : array of const;
                                 Dummy : Word);
      property ErrorCode : LongInt
        read FErrorCode
        write FErrorCode;
  end;
  EStExceptionClass = class of EStException;
  {@abstract(statistics exceptions)}
  EStStatError = class(EStException);


type
  {full statistics for a linear regression}
  TStLinEst = record
    {model coefficients}
    B0, B1     : Double;
    {standard error of model coefficients}
    seB0, seB1 : Double;
    {coefficient of determination}
    R2         : Double;
    {standard error of regression}
    sigma      : Double;
    {elements for ANOVA table}
    SSr, SSe   : Double;
    {F-statistic to test B1=0}
    F0         : Double;
    {denominator degrees of freedom for F-statistic}
    df         : Integer;
  end;

{LINEST}
  {-Performs linear fit to data and returns coefficients and error
    statistics.
      KnownY is the dependent array of known data points.
      KnownX is the independent array of known data points.
      NData must be greater than 2.
      If ErrorStats is FALSE, only B0 and B1 are computed; the other fields
        of TStLinEst are set to 0.0.
      See declaration of TStLinEst for returned data.

  }
procedure LinEst(const KnownY : array of Double;
  const KnownX : array of Double; var LF : TStLinEst; ErrorStats : Boolean);
  {-Performs linear fit to data and returns coefficients and error
    statistics.
      KnownY is the dependent array of known data points.
      KnownX is the independent array of known data points.
      NData must be greater than 2.
      If ErrorStats is FALSE, only B0 and B1 are computed; the other fields
        of TStLinEst are set to 0.0.
      See declaration of TStLinEst for returned data.

  }
procedure LinEst16(const KnownY; const KnownX; NData : Integer;
  var LF : TStLinEst; ErrorStats : Boolean);


implementation

const
  stscStatBadCountS               = 'Unequal or bad counts of array elements';
  stscStatBadParamS               = 'Invalid parameter';
  stscStatBadDataS                = 'Invalid data point in array';
  stscStatNoConvergeS             = 'no convergence in numerical routine';

type
  StStrRec = record
    ID: Integer;
    Str: string;
  end;

const
  SysToolsStrArray : array [0..3] of StStrRec = (

  {ststat errors}
 (ID: stscStatBadCount; Str: stscStatBadCountS),  {unequal or bad counts of array elements}
 (ID: stscStatBadParam; Str: stscStatBadParamS),  {invalid parameter}
 (ID: stscStatBadData; Str: stscStatBadDataS),  {invalid data point in array}
 (ID: stscStatNoConverge; Str: stscStatNoConvergeS)  {no convergence in numerical routine}

 );

function SysToolsStr(Index : Integer) : string;
var
  i : Integer;
begin
  for i := Low(SysToolsStrArray) to High(SysToolsStrArray) do
    if SysToolsStrArray[i].ID = Index then
      Result := SysToolsStrArray[i].Str;
end;


procedure RaiseStatError(Code : LongInt);
  {-Generate a statistics exception}
var
  E : EStStatError;
begin
  E := EStStatError.CreateResTP(Code, 0);
  E.ErrorCode := Code;
  raise E;
end;

procedure LinEst(const KnownY: array of Double;
  const KnownX: array of Double; var LF : TStLinEst; ErrorStats : Boolean);
begin
  if (High(KnownY) <> High(KnownX)) then
    RaiseStatError(stscStatBadCount);
  LinEst16(KnownY, KnownX, High(KnownY)+1, LF, ErrorStats);
end;

procedure LinEst16(const KnownY; const KnownX; NData : Integer;
  var LF : TStLinEst; ErrorStats : Boolean);
var
  i : Integer;
  sx, sy, xmean, ymean, sxx, sxy, syy, x, y : Extended;
begin
  // RBW: I have changed 2 to 1 in the following line.
  // This only works because I never want Error Statistics.
  if (NData <= 1) then
    RaiseStatError(stscStatBadCount);

  Assert(not ErrorStats);

  {compute basic sums}
  sx := 0.0;
  sy := 0.0;
  sxx := 0.0;
  sxy := 0.0;
  syy := 0.0;
  for i := 0 to NData-1 do begin
    x := TDoubleArray(KnownX)[i];
    y := TDoubleArray(KnownY)[i];
    sx := sx+x;
    sy := sy+y;
    sxx := sxx+x*x;
    syy := syy+y*y;
    sxy := sxy+x*y;
  end;
  xmean := sx/NData;
  ymean := sy/NData;
  sxx := sxx-NData*xmean*xmean;
  syy := syy-NData*ymean*ymean;
  sxy := sxy-NData*xmean*ymean;

  {check for zero variance}
  if (sxx <= 0.0) or (syy <= 0.0) then
    RaiseStatError(stscStatBadData);

  {initialize returned parameters}
  fillchar(LF, sizeof(LF), 0);

  {regression coefficients}
  LF.B1 := sxy/sxx;
  LF.B0 := ymean-LF.B1*xmean;

  {error statistics}
  if (ErrorStats) then begin
    LF.ssr := LF.B1*sxy;
    LF.sse := syy-LF.ssr;
    LF.R2 := LF.ssr/syy;
    LF.df := NData-2;
    LF.sigma := sqrt(LF.sse/LF.df);
    if LF.sse = 0.0 then
      {pick an arbitrarily large number for perfect fit}
      LF.F0 := 1.7e+308
    else
      LF.F0 := (LF.ssr*LF.df)/LF.sse;
    LF.seB1 := LF.sigma/sqrt(sxx);
    LF.seB0 := LF.sigma*sqrt((1.0/NData)+(xmean*xmean/sxx));
  end;
end;




{ EStException }

constructor EStException.CreateResFmtTP(Ident: Integer;
  const Args: array of const; Dummy: Word);
begin
  inherited CreateFmt(SysToolsStr(Ident), Args);
end;

constructor EStException.CreateResTP(Ident: Integer; Dummy: Word);
begin
  inherited Create(SysToolsStr(Ident));

end;

end.
 
