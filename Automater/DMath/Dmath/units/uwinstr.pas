{ ******************************************************************
  String routines for DELPHI
  ****************************************************************** }

unit uwinstr;

interface

{$ifdef CONDITIONALEXPRESSIONS}
  {$if CompilerVersion>=23}
    {$DEFINE Delphi_XE2_UP}
  {$ifend}
{$endif}

uses
  utypes, ustrings,
  {$IFDEF Delphi_XE2_UP}
  VCL.StdCtrls,
  {$ELSE}
  StdCtrls,
  {$ENDIF}
  SysUtils;

function StrDec(S : String) : String;
{ Replaces commas or decimal points by
  the decimal separator defined in SysUtils }

function IsNumeric(var S : String; var X : Float) : Boolean;
{ Replaces in string S the decimal comma by a point,
  tests if the resulting string represents a number.
  If so, returns this number in X }

function ReadNumFromEdit(Edit : TEdit) : Float;
{ Reads a floating point number from an Edit control }

procedure WriteNumToFile(var F : Text; X : Float);
{ Writes a floating point number in a text file,
  forcing the use of a decimal point }

implementation

{$IFDEF Delphi_XE2_UP}
// The DecimalSeparator variable was depricated in Delphi XE and
// removed in Delphi XE6 according to a comment in
// https://stackoverflow.com/questions/25109497/decimalseparator-in-sysutils-and-system-sysutils.
var
  DecimalSeparator: Char;
{$ENDIF}


var
  BadChar : Char;  { Decimal separator to be replaced }

function StrDec(S : String) : String;
begin
  StrDec := Replace(S, BadChar, DecimalSeparator);
end;

function IsNumeric(var S : String; var X : Float) : Boolean;
var
  ErrCode : Integer;
begin
  if DecimalSeparator = ',' then
    S := Replace(S, ',', '.');
  Val(S, X, ErrCode);
  IsNumeric := (ErrCode = 0);
end;

function ReadNumFromEdit(Edit : TEdit) : Float;
var
  S : String;
  X : Float;
begin
  S := Edit.Text;
  if IsNumeric(S, X) then
    ReadNumFromEdit := X
  else
    ReadNumFromEdit := 0.0;
end;

procedure WriteNumToFile(var F : Text; X : Float);
begin
  Write(F, ' ', Replace(FloatToStr(X), ',', '.'));
end;

begin
  if DecimalSeparator = '.' then BadChar := ',' else BadChar := '.';
end.

Initialization

{$IFDEF Delphi_XE2_UP}
var
  DecimalSeparator: Char;
{$ENDIF}


end;
