{*********************************************************}
{* BMSearch                                              *}
{* Copyright (c) Julian M Bucknall 1998                  *}
{* All rights reserved.                                  *}
{*********************************************************}
{* Boyer-Moore search routines                           *}
{*********************************************************}
{Note: this unit is released as freeware. In other words, you are free
       to use this unit in your own applications, however I retain all
       copyright to the code. JMB}
{$IFNDEF Win32}
//!! Error: Sorry this unit will only compile in Delphi 2, 3, or 4
{$ENDIF}

// Modified by Richard B. Winston

unit BMSearch;
interface
uses
  SysUtils, AnsiStrings;
function BMPos(const aPattern : AnsiString;
               const aText : AnsiString;
{$IFDEF VER120} {ie, Delphi 4}
               aNoCase : boolean = false;
               aStartPos : integer = 1) : integer;
{$ELSE}
               aNoCase : boolean;
               aStartPos : integer) : integer;
{$ENDIF}
function BMPosSimple(const aPattern : AnsiString;
                     const aText : AnsiString) : integer;

function BMPosSimpleEx(const aPattern : AnsiString;
                     const aText : AnsiString; Offset: integer = 1) : integer;

implementation
uses
  Windows;
type
  PBoyerMooreSkips = ^TBoyerMooreSkips;
  TBoyerMooreSkips = array [AnsiChar] of byte;
function BMPos(const aPattern : AnsiString;
               const aText : AnsiString;
{$IFDEF VER120} {ie, Delphi 4}
               aNoCase : boolean = false;
               aStartPos : integer = 1) : integer;
{$ELSE}
               aNoCase : boolean;
               aStartPos : integer) : integer;
{$ENDIF}
var
  TextInx         : integer;
  NewTextInx      : integer;
  PatInx          : integer;
  PatLen, TextLen : integer;
  SkipValue       : integer;
  WorkPattern     : AnsiString;
  AllChars        : AnsiString;
  LowerChars      : AnsiString;
  Skips           : TBoyerMooreSkips;
  LastChar        : AnsiChar;
  Matched         : boolean;
begin
  {quick easy checks}
  PatLen := length(aPattern);
  TextLen := length(aText);
  if (aPattern = '') or (PatLen > TextLen) then begin
    Result := 0;
    Exit;
  end;
  if (PatLen > 255) then
    raise Exception.Create('Pattern is too long');
  {get the lowercased pattern if required}
  if aNoCase then begin
    WorkPattern := AnsiLowerCase(aPattern);
    SetLength(AllChars, 256);
    for TextInx := 1 to 255 do
      AllChars[TextInx] := AnsiChar(TextInx);
    LowerChars := AnsiLowerCase(AllChars);
    AllChars := '';
  end
  else
    WorkPattern := aPattern;
  {generate the skip values}
  FillChar(Skips, sizeof(Skips), byte(PatLen));
  SkipValue := pred(PatLen);
  for PatInx := 1 to pred(PatLen) do begin
    Skips[WorkPattern[PatInx]] := SkipValue;
    dec(SkipValue);
  end;
  {start looking for the last character of the pattern}
  LastChar := WorkPattern[PatLen];
  if (aStartPos <= 1) then
    TextInx := PatLen
  else
    TextInx := PatLen + aStartPos - 1;
  while TextInx <= TextLen do begin
    {calc the skip value, based on the current text character}
    SkipValue := Skips[aText[TextInx]];
    {if we don't have a match on the last character, skip}
    if ((not aNoCase) and (LastChar <> aText[TextInx])) or
       (aNoCase and (LastChar <> LowerChars[ord(aText[TextInx])])) then
      inc(TextInx, SkipValue)
    {if we do have a match on the last AnsiChar, try matching the rest}
    else begin
      Matched := true;
      Result := TextInx;
      for PatInx := pred(PatLen) downto 1 do begin
        dec(Result);
        if ((not aNoCase) and
            (WorkPattern[PatInx] <> aText[Result])) or
           (aNoCase and
            (WorkPattern[PatInx] <> LowerChars[ord(aText[Result])])) then begin
          NewTextInx := Result + Skips[aText[Result]];
          inc(TextInx, SkipValue);
          if (TextInx < NewTextInx) then
            TextInx := NewTextInx;
          Matched := false;
          Break;
        end;
      end;
      if Matched then
        Exit;
    end;
  end;
  Result := 0;
end;
function BMPosSimple(const aPattern : AnsiString;
                     const aText : AnsiString) : integer;
var
  TextInx         : integer;
  NewTextInx      : integer;
  PatInx          : integer;
  PatLen, TextLen : integer;
  SkipValue       : integer;
  Skips           : TBoyerMooreSkips;
  LastChar        : AnsiChar;
  Matched         : boolean;
begin
  {quick easy checks}
  PatLen := length(aPattern);
  TextLen := length(aText);
  if (aPattern = '') or (PatLen > TextLen) then begin
    Result := 0;
    Exit;
  end;
  if (PatLen > 255) then
    raise Exception.Create('Pattern is too long');
  {generate the skip values}
  FillChar(Skips, sizeof(Skips), byte(PatLen));
  SkipValue := pred(PatLen);
  for PatInx := 1 to pred(PatLen) do begin
    Skips[aPattern[PatInx]] := SkipValue;
    dec(SkipValue);
  end;
  {start looking for the last character of the pattern}
  LastChar := aPattern[PatLen];
  TextInx := PatLen;
  while TextInx <= TextLen do begin
    {calc the skip value, based on the current text character}
    SkipValue := Skips[aText[TextInx]];
    {if we don't have a match on the last character, skip}
    if (LastChar <> aText[TextInx]) then
      inc(TextInx, SkipValue)
    {if we do have a match on the last AnsiChar, try matching the rest}
    else begin
      Matched := true;
      Result := TextInx;
      for PatInx := pred(PatLen) downto 1 do begin
        dec(Result);
        if (aPattern[PatInx] <> aText[Result]) then begin
          NewTextInx := Result + Skips[aText[Result]];
          inc(TextInx, SkipValue);
          if (TextInx < NewTextInx) then
            TextInx := NewTextInx;
          Matched := false;
          Break;
        end;
      end;
      if Matched then
        Exit;
    end;
  end;
  Result := 0;
end;

function BMPosSimpleEx(const aPattern : AnsiString;
                     const aText : AnsiString; Offset: integer = 1) : integer;
var
  TextInx         : integer;
  NewTextInx      : integer;
  PatInx          : integer;
  PatLen, TextLen : integer;
  SkipValue       : integer;
  Skips           : TBoyerMooreSkips;
  LastChar        : AnsiChar;
  Matched         : boolean;
begin
  {quick easy checks}
  if Offset < 1 then
  begin
    Result := 0;
    Exit;
  end;
  PatLen := length(aPattern);
  TextLen := length(aText);
  if (aPattern = '') or (PatLen > TextLen) then begin
    Result := 0;
    Exit;
  end;
  if (PatLen > 255) then
    raise Exception.Create('Pattern is too long');
  {generate the skip values}
  FillChar(Skips, sizeof(Skips), byte(PatLen));
  SkipValue := pred(PatLen);
  for PatInx := 1 to pred(PatLen) do begin
    Skips[aPattern[PatInx]] := SkipValue;
    dec(SkipValue);
  end;
  {start looking for the last character of the pattern}
  LastChar := aPattern[PatLen];
  TextInx := PatLen + Offset -1;
  while TextInx <= TextLen do begin
    {calc the skip value, based on the current text character}
    SkipValue := Skips[aText[TextInx]];
    {if we don't have a match on the last character, skip}
    if (LastChar <> aText[TextInx]) then
      inc(TextInx, SkipValue)
    {if we do have a match on the last AnsiChar, try matching the rest}
    else begin
      Matched := true;
      Result := TextInx;
      for PatInx := pred(PatLen) downto 1 do begin
        dec(Result);
        if (aPattern[PatInx] <> aText[Result]) then begin
          NewTextInx := Result + Skips[aText[Result]];
          inc(TextInx, SkipValue);
          if (TextInx < NewTextInx) then
            TextInx := NewTextInx;
          Matched := false;
          Break;
        end;
      end;
      if Matched then
        Exit;
    end;
  end;
  Result := 0;
end;

end.

