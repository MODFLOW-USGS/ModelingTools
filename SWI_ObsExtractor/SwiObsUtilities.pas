unit SwiObsUtilities;

interface

uses SysUtils, StrUtils;

function TryFortranStrToFloat(AString: string; var value: Extended): Boolean;
function FortranStrToFloat(AString: string): Extended;

implementation

uses
  Math;

function TryFortranStrToFloat(AString: string; var value: Extended): Boolean;
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
    result := TryStrToFloat(AString, Value);
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

end.
