unit FootPrintUtilities;

interface

uses
  System.SysUtils;

type
  TOneDRealArray = array of Double;
  TTwoDRealArray = array of TOneDRealArray;

  TOneDIntArray = array of Integer;
  TTwoDIntArray = array of TOneDIntArray;

  TOneDStringArray = array of string;
  TTwoDStringArray = array of TOneDStringArray;

  TOneDBooleanArray = array of Boolean;
  TTwoDBooleanArray = array of TOneDBooleanArray;

function FortranFloatToStr(Value: Extended): string;

function FortranStrToFloat(AString: string): Extended;

function TryFortranStrToFloat(AString: string; var value: Extended): Boolean;

function CharArrayToString(const a: array of Char): string;

procedure WriteLine(const ALine: string);

implementation

uses
  System.Math, System.StrUtils;

function FortranFloatToStr(Value: Extended): string;
var
  OldDecimalSeparator: Char;
begin
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := '.';
    result := FloatToStr(Value);
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;

function FortranStrToFloat(AString: string): Extended;
var
  OldDecimalSeparator: Char;
  SignPos: Integer;
begin
  AString := Trim(AString);
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := '.';
    AString := StringReplace(AString, ',', '.', [rfReplaceAll, rfIgnoreCase]);
    AString := StringReplace(AString, 'd', 'e', [rfReplaceAll, rfIgnoreCase]);
    SignPos := Max(PosEx('+', AString, 2), PosEx('-', AString, 2));
    if SignPos > 0 then
    begin
      if not CharInSet(AString[SignPos-1], ['e', 'E']) then
      begin
        Insert('E', AString, SignPos);
      end;
    end;
    result := StrToFloat(AString);
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;

function TryFortranStrToFloat(AString: string; var value: Extended): Boolean;
var
  OldDecimalSeparator: Char;
  SignPos: Integer;
begin
  Value := 0;
  AString := Trim(AString);
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := '.';
    AString := StringReplace(AString, ',', '.', [rfReplaceAll, rfIgnoreCase]);
    AString := StringReplace(AString, 'd', 'e', [rfReplaceAll, rfIgnoreCase]);
    SignPos := Max(PosEx('+', AString, 2), PosEx('-', AString, 2));
    if SignPos > 0 then
    begin
      if not CharInSet(AString[SignPos-1], ['e', 'E']) then
      begin
        Insert('E', AString, SignPos);
      end;
    end;
    result := TryStrToFloat(AString, Value);
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;

function CharArrayToString(const a: array of Char): string;
begin
  if Length(a)>0 then
    SetString(Result, PChar(@a[0]), Length(a))
  else
    Result := '';
end;

procedure WriteLine(const ALine: string);
begin
  Write(ALine);
end;
end.
