{@abstract(@name contains a variety of miscellaneous routines involving
  OpenGL, math, and strings. )}
unit ModelMuseUtilities;

interface

uses System.UITypes,
  Windows, SysUtils, Classes, Graphics, OpenGL, GoPhastTypes, ColorSchemes,
  System.ConvUtils, System.StdConvs, FastGEO, System.Character;

type
  TSupportedLengthConv = (slcCm, slcMeter, slcKilometer, slcFeet, slcMile);

resourcestring
  StrSorryItLooksLike = 'Sorry. It looks like some other program has locked ' +
  '%s. You will need to close the other program.';

// @abstract(@name gets the red, green, and blue components from a TColor
// in a form suitable for use with OpenGL.)
procedure ExtractColorComponents(const AColor: TColor;
  out Red, Green, Blue: GLubyte);

// @name extracts the file name without the drive, directory or extension.
function ExtractFileRoot(const FileName: string): string;
{
   @name converts a fraction between 0 and 1 to
   a color using the selected ColorSchemeIndex.

  @param(ColorSchemeIndex
0: @link(FracToSpectrum);

1: @link(FracToGreenMagenta) (reversed);

2: @link(FracToBlueRed) (reversed);

3: @link(FracToBlueDarkOrange) (reversed);

4: @link(FracToBlueGreen) (reversed);

5: @link(FracToBrownBlue) (reversed);

6: @link(FracToBlueGray) (reversed);

7: @link(FracToBlueOrange) (reversed);

8: @link(FracToBlue_OrangeRed) (reversed);

9: @link(FracToLightBlue_DarkBlue) (reversed);

10: @link(ModifiedSpectralScheme) (reversed); )
}

function FracAndSchemeToColor(ColorSchemeIndex: integer;
  Fraction, ColorAdjustmentFactor: real; const Cycles: integer): TColor;

// @abstract(@name calculates the normal
// of the plane defined by v1, v2, and v3.)
procedure Normal(const v1, v2, v3: T3DRealPoint; out result: T3DRealPoint);

// @abstract(@name subtracts v2 from v1.)
procedure SubtractVectors(const v1, v2: T3DRealPoint; out result: T3DRealPoint);

function FortranFloatToStr(Value: Extended): string;

function FortranStrToFloat(AString: string): Extended;
function FortranStrToFloatDef(AString: string; Value: Extended): Extended;
function TryFortranStrToFloat(AString: string; out Value: Extended): Boolean;

function TitleCase(AString: string): string;
function AnsiTitleCase(AString: AnsiString): AnsiString;

// @name converts Value to a string that includes the thousands separator
// if appropriate.
function IntToStrFormatted(Value: integer): string;

procedure DSiTrimWorkingSet;

function QuoteFileName(AName: string): string;
function ArchiveQuoteFileName(AName: string): string;

function FixShapeFileFieldName(FieldName: AnsiString; Fields: TStringList): AnsiString;

procedure RunAProgram(const CommandLine: string);

function FileLength(fileName : string) : Int64;

function IsWOW64: Boolean;

procedure CantOpenFileMessage(AFileName: string);

// @name is a function suitable for use in TStringList.CustomSort so that
// strings that have numbers at the end are sorted in numerical order if the
// parts before that are identical.
function CompareNames(List: TStringList; Index1, Index2: Integer): Integer;
//function DiskInDrive(Drive: AnsiChar): Boolean;

function ConvertLength(Value: double; FromUnits, ToUnits: TSupportedLengthConv): double;
function ConvertPoint2D(APoint: TPoint2D; FromUnits, ToUnits: TSupportedLengthConv): TPoint2D;

function PointInConcavePolygon(const Point:TPoint2D; const Polygon:TPolygon2D):Boolean;

procedure MM_ObjectBinaryToText(const Input, Output: TStream);

function PestObsName(const ObsName: string): string;

{
@name removes comments from a MODFLOW input file line
and converts it to upper case.
If the entire line is a comment, @name returns an empty string.
}
function ExtractNonCommentLine(const ALine: string): string;
{@name checks a MODFLOW 6 input line that has been proccessed by
@link(ExtractNonCommentLine) and returns @True if it marks the beginning
of a section. If it does mark the beginning of a section, Section identifies
the section.}
function IsBeginningOfSection(const ALine: string;
  out Section: string): Boolean;
function IsEndOfSection(const ALine: string): Boolean;

function Interpolate(X, StartX, EndX, StartY, EndY: double): double;

function RoundSigDigits(ANumber: Extended; SignificantDigits: Integer): Extended;

var
  StartTimeCmdLines: TStringList;
  ShowElapsedTimeCmdLines: TStringList;

implementation

uses AnsiStrings, StrUtils, Dialogs, Math, frmGoPhastUnit,
  IdGlobal, IOUtils, System.RTLConsts, System.TypInfo, System.ZLib, TempFiles,
  JvCreateProcess;

resourcestring
  StrBadProcessHandle = 'Bad process handle';

var
  ColorParameters: TColorParameters;

function FileLength(fileName : string) : Int64;
 var
   sr : TSearchRec;
 begin
   if FindFirst(fileName, faAnyFile, sr ) = 0 then
   begin
      result := sr.Size;
   end
   else
   begin
      result := -1;
   end;

   FindClose(sr) ;
 end;

procedure RunAProgram(const CommandLine: string);
var
  Runner: TJvCreateProcess;
begin
  Runner := TJvCreateProcess.Create(nil);
  try
    Runner.CommandLine := CommandLine;
    try
      Runner.Run;
    except on E: EOSError do
      begin
        Beep;
        MessageDlg(E.message, mtError, [mbOK], 0);
      end;
    end;
  finally
    Runner.Free;
  end;
end;

procedure ExtractColorComponents(const AColor: TColor;
  out Red, Green, Blue: GLubyte);
var
  Value: integer;
  v: longword;
begin
  Value := ColorToRGB(AColor);
  Assert(Value >= 0);
  v := Value;
  Red := (v shl 24) shr 24;
  v := v shr 8;
  Green := (v shl 24) shr 24;
  v := v shr 8;
  Blue := (v shl 24) shr 24;
end;

procedure CrossProduct(const v1, v2: T3DRealPoint; out result: T3DRealPoint);
begin
  result.X := v1.Y * v2.Z - v1.Z * v2.Y;
  result.Y := v1.Z * v2.X - v1.X * v2.Z;
  result.Z := v1.X * v2.Y - v1.Y * v2.X;
end;

procedure SubtractVectors(const v1, v2: T3DRealPoint; out result: T3DRealPoint);
begin
  result.X := v1.X - v2.X;
  result.Y := v1.Y - v2.Y;
  result.Z := v1.Z - v2.Z;
end;

procedure Normal(const v1, v2, v3: T3DRealPoint; out result: T3DRealPoint);
var
  Diff1, Diff2: T3DRealPoint;
  d: double;
begin
  SubtractVectors(v1, v2, Diff1);
  SubtractVectors(v2, v3, Diff2);
  CrossProduct(Diff1, Diff2, result);
  d := Sqrt(sqr(result.X) + sqr(result.Y) + sqr(result.Z));

  result.X := result.X / d;
  result.Y := result.Y / d;
  result.Z := result.Z / d;
end;

function FracAndSchemeToColor(ColorSchemeIndex: integer;
  Fraction, ColorAdjustmentFactor: real; const Cycles: integer): TColor;
var
  ColorScheme: TUserDefinedColorSchemeItem;
begin
  if ColorSchemeIndex <= MaxColorScheme then
  begin
    if ColorSchemeIndex < 0 then
    begin
      result := clWhite;
      Exit;
    end;
    if Fraction <> 1 then
    begin
      Fraction := Frac(Fraction*Cycles);
    end;

    case ColorSchemeIndex of
      0: result := FracToSpectrum(Fraction, ColorAdjustmentFactor);
      1: result := FracToGreenMagenta(1 - Fraction, ColorAdjustmentFactor);
      2: result := FracToBlueRed(1 - Fraction, ColorAdjustmentFactor);
      3: result := FracToBlueDarkOrange(1 - Fraction, ColorAdjustmentFactor);
      4: result := FracToBlueGreen(1 - Fraction, ColorAdjustmentFactor);
      5: result := FracToBrownBlue(1 - Fraction, ColorAdjustmentFactor);
      6: result := FracToBlueGray(1 - Fraction, ColorAdjustmentFactor);
      7: result := FracToBlueOrange(1 - Fraction, ColorAdjustmentFactor);
      8: result := FracToBlue_OrangeRed(1 - Fraction, ColorAdjustmentFactor);
      9: result := FracToLightBlue_DarkBlue(1 - Fraction, ColorAdjustmentFactor);
      10: result := ModifiedSpectralScheme(1 - Fraction, ColorAdjustmentFactor);
      11: result := SteppedSequential(1 - Fraction, ColorAdjustmentFactor);
    else
      result := clWhite;
      Assert(False);
    end;
  end
  else
  begin
    ColorSchemeIndex := ColorSchemeIndex-MaxColorScheme-1;
    if ColorSchemeIndex <= frmGoPhast.PhastModel.ColorSchemes.Count then
    begin
      ColorParameters.ColorCycles := Cycles;
      ColorParameters.ColorExponent := ColorAdjustmentFactor;
      ColorScheme:= frmGoPhast.PhastModel.ColorSchemes[ColorSchemeIndex];
      result := ColorParameters.FracToColor(Fraction, ColorScheme)
    end
    else
    begin
      result := clWhite;
    end;
  end;
end;

function ExtractFileRoot(const FileName: string): string;
var
  DotPos: integer;
begin
  result := ExtractFileName(FileName);
  DotPos := Pos('.', result);
  If DotPos > 0 then
  begin
    result := Copy(result, 1, DotPos-1);
  end;
end;

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

function TitleCase(AString: string): string;
var
  Index: integer;
begin
  result := LowerCase(AString);
  if Length(result) > 0 then
  begin
    Result[1] := UpperCase(Result)[1];
    for Index := 1 to Length(result) - 1 do
    begin
      if (result[Index] = ' ') or (result[Index] = '-') then
      begin
        result[Index+1] := UpperCase(result[Index+1])[1];
      end;
    end;
  end;
end;

function AnsiTitleCase(AString: AnsiString): AnsiString;
var
  Index: integer;
begin
  result := AnsiLowerCase(AString);
  if Length(result) > 0 then
  begin
    Result[1] := AnsiUpperCase(Result)[1];
    for Index := 1 to Length(result) - 1 do
    begin
      if result[Index] = ' ' then
      begin
        result[Index+1] := AnsiUpperCase(result[Index+1])[1];
      end;
    end;
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

function FortranStrToFloatDef(AString: string; Value: Extended): Extended;
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
    result := StrToFloatDef(AString, Value);
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;

function TryFortranStrToFloat(AString: string; out Value: Extended): Boolean;
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
    Result := TryStrToFloat(AString, Value);
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;

function IntToStrFormatted(Value: integer): string;
var
  Negative: Boolean;
  Digits: Integer;
  DigitString: string;
begin
  if Abs(Value) < 10000 then
  begin
    result := IntToStr(Value);
    Exit;
  end;
  Negative := Value < 0;
  result := '';
  Value := Abs(Value);
  while Value > 0 do
  begin
    Digits := Value mod 1000;
    Value := Value div 1000;
    if Value > 0 then
    begin
      DigitString := Format('%.3d', [Digits]);
      result := FormatSettings.ThousandSeparator + DigitString + result;
    end
    else
    begin
      DigitString := Format('%d', [Digits]);
      result := DigitString + result;
    end;
  end;
  if Negative then
  begin
    result := '-' + result;
  end;
end;

// http://stackoverflow.com/questions/2031577/can-memory-be-cleaned-up/2033393#2033393
procedure DSiTrimWorkingSet;
var
  hProcess: THandle;
begin
//  Exit;
  hProcess := OpenProcess(PROCESS_SET_QUOTA, false, GetCurrentProcessId);
  try
    SetProcessWorkingSetSize(hProcess, $FFFFFFFF, $FFFFFFFF);
  finally CloseHandle(hProcess); end;
end;

function QuoteFileName(AName: string): string;
var
  AlphaNumeric: Boolean;
  AChar: Char;
  TempFile: TFileStream;
  function HasWhiteSpace(const AText: string): Boolean;
  var
    AChar: Char;
  begin
    result := False;
    for AChar in AText do
    begin
      if AChar.IsWhiteSpace then
      begin
        result := True;
        break;
      end;
    end;
  end;
begin
  if AName = '' then
  begin
    result := AName;
    Exit;
  end;

  AlphaNumeric := True;
  for AChar in AName do
  begin
    if not CharInSet(AChar, ['.', ':', '\', ' ', '_', '-', '~']) and not IsAlphaNumeric(AChar) then
    begin
      AlphaNumeric := False;
    end;
  end;
  if AlphaNumeric then
  begin
    if (AName[1] <> '"')
      and ((Pos(' ', AName) > 0) or HasWhiteSpace(AName)) then
    begin
      result := '"' + AName + '"';
    end
    else
    begin
      result := AName;
    end;
  end
  else
  begin
    if not FileExists(AName) then
    begin
      if DirectoryExists(ExtractFileDir(AName)) then
      begin
        TempFile := TFile.Create(AName);
        try
          AName := ExtractShortPathName(AName);
        finally
          TempFile.Free;
        end;
        DeleteFile(AName)
      end;
    end
    else
    begin
      AName := ExtractShortPathName(AName);
    end;
    result := AName;
  end;
end;

function ArchiveQuoteFileName(AName: string): string;
var
  AlphaNumeric: Boolean;
  AChar: Char;
  TempFile: TFileStream;
begin
  if AName = '' then
  begin
    result := AName;
    Exit;
  end;

  AlphaNumeric := True;
  for AChar in AName do
  begin
    if not CharInSet(AChar, ['.', ':', '\', ' ', '_', '-', '~']) and not IsAlphaNumeric(AChar) then
    begin
      AlphaNumeric := False;
    end;
  end;
  if AlphaNumeric then
  begin
    if (AName[1] <> '"')
      and (Pos(' ', AName) > 0) then
    begin
      result := '"' + AName + '"';
    end
    else
    begin
      result := AName;
    end;
  end
  else
  begin
    if not FileExists(AName) then
    begin
      if DirectoryExists(ExtractFileDir(AName)) then
      begin
        TempFile := TFile.Create(AName);
        try
          AName := ExtractShortPathName(AName);
        finally
          TempFile.Free;
        end;
        DeleteFile(AName)
      end;
    end
    else
    begin
      AName := ExtractShortPathName(AName);
    end;
    result := ExtractFileName(AName);
  end;
end;

function FixShapeFileFieldName(FieldName: AnsiString; Fields: TStringList): AnsiString;
const
  MaximumFieldNameLength = 10;
var
//  SuffixIndex: Integer;
  Root: AnsiString;
  SuffixValue: Integer;
  Suffix: AnsiString;
begin
  While (Length(FieldName) > 0) and (FieldName[Length(FieldName)] = '_') do
  begin
    SetLength(FieldName, Length(FieldName) -1);
  end;

  if Fields.IndexOf(string(FieldName)) >= 0 then
  begin
    Root := FieldName;
    SuffixValue := 0;
    repeat
      Inc(SuffixValue);
      Suffix := AnsiString(IntToStr(SuffixValue));
      if Length(Root) + Length(Suffix) > MaximumFieldNameLength then
      begin
        SetLength(Root, MaximumFieldNameLength - Length(Suffix));
      end;
      FieldName := Root + Suffix;
//      FieldName := FixShapeFileFieldName(FieldName);
    until (Fields.IndexOf(string(FieldName)) < 0);
  end;

  result := FieldName;
end;

function IsWOW64: Boolean;
// from http://www.delphipages.com/forum/showthread.php?t=206540
type
  TIsWow64Process = function(
    Handle: THandle;
    var Res: BOOL
  ): BOOL; stdcall;
var
  IsWow64Result: BOOL;
  IsWow64Process: TIsWow64Process;
begin
  IsWow64Process := GetProcAddress(
    GetModuleHandle('kernel32'), 'IsWow64Process'
  );
  if Assigned(IsWow64Process) then
  begin
    if not IsWow64Process(GetCurrentProcess, IsWow64Result) then
      raise Exception.Create(StrBadProcessHandle);
    Result := IsWow64Result;
  end
  else
    Result := False;
end;

procedure CantOpenFileMessage(AFileName: string);
begin
  Beep;
  MessageDlg(Format(StrSorryItLooksLike, [AFileName]), mtError, [mbOK], 0);
end;

function CompareNames(List: TStringList; Index1, Index2: Integer): Integer;
  procedure ProcessName(var AName, ANumber: string);
  const
    Digits = ['0'..'9'];
  var
    Index: integer;
    LastDigit: integer;
  begin
    LastDigit := Length(AName) + 1;
    for Index := Length(AName) downto 1 do
    begin
      if CharInSet(AName[Index], Digits) then
      begin
        LastDigit := Index;
      end
      else
      begin
        break;
      end;
    end;
    ANumber := Copy(AName, LastDigit, MAXINT);
    SetLength(AName, LastDigit-1);
  end;
var
  Name1, Name2: string;
  Number1, Number2: string;
  N1, N2: Int64;
begin
  Name1 :=  List[Index1];
  Name2 :=  List[Index2];
  ProcessName(Name1, Number1);
  ProcessName(Name2, Number2);
  result := AnsiCompareText(Name1, Name2);
  if result = 0 then
  begin
    if Number1 = Number2 then
    begin
      Exit;
    end
    else if Number1 = '' then
    begin
      result := -1
    end
    else if Number2 = '' then
    begin
      result := 1
    end
    else
    begin
      result := Length(Number1) - Length(Number2);
      if result = 0 then
      begin
        while (not TryStrToInt64(Number1, N1))
          or (not TryStrToInt64(Number2, N2)) do
        begin
          N1 := StrToInt64(Copy(Number1, 1, 15));
          N2 := StrToInt64(Copy(Number2, 1, 15));
          result := Sign(N1 - N2);
          if Result = 0 then
          begin
            Number1 := Copy(Number1, 16, MAXINT);
            Number2 := Copy(Number2, 16, MAXINT);
            if Number1 = Number2 then
            begin
              Result := 0;
              Exit;
            end
            else if Number1 = '' then
            begin
              result := -1;
              Exit;
            end
            else if Number2 = '' then
            begin
              result := 1;
              Exit;
            end
          end
          else
          begin
            Exit;
          end;
        end;
        result := Sign(N1 - N2);
      end;
    end;
  end
end;

function ConvertLength(Value: double; FromUnits, ToUnits: TSupportedLengthConv): double;
//var
//  FromConv: TConvType;
//  ToConv: TConvType;
  function UnitToConvType(Units: TSupportedLengthConv): TConvType;
  begin
    Result := duMeters;
    case Units of
      slcCm: Result := duCentimeters;
      slcMeter: Result := duMeters;
      slcKilometer: Result := duKilometers;
      slcFeet: Result := duFeet;
      slcMile: Result := duMiles;
      else
        Assert(False);
    end;
  end;
begin
  result := Convert(Value, UnitToConvType(FromUnits), UnitToConvType(ToUnits));
end;

function ConvertPoint2D(APoint: TPoint2D; FromUnits, ToUnits: TSupportedLengthConv): TPoint2D;
begin
  result.x := ConvertLength(APoint.x, FromUnits, ToUnits);
  result.y := ConvertLength(APoint.y, FromUnits, ToUnits);
end;

function PointInConcavePolygon(const Point:TPoint2D; const Polygon:TPolygon2D):Boolean;
var
  PriorIndex: Integer;
  VertexIndex: Integer;
  APoint: TPoint2D;
  AnotherPoint: TPoint2D;
begin
  Result := False;
  PriorIndex := Length(Polygon) - 1;
  for VertexIndex := 0 to Length(Polygon) - 1 do
  begin
    APoint := Polygon[VertexIndex];
    AnotherPoint := Polygon[PriorIndex];
    if ((Point.Y <= APoint.Y) = (Point.Y > AnotherPoint.Y)) and
      (Point.X - APoint.X - (Point.Y - APoint.Y) *
      (AnotherPoint.X - APoint.X) /
      (AnotherPoint.Y - APoint.Y) < 0) then
    begin
      result := not result;
    end;
    PriorIndex := VertexIndex;
  end;
end;

// From https://www.experts-exchange.com/questions/26419234/Drivecombobox-Delphi-what's-the-proper-way-to-test-if-removeable-media-is-present.html#answer33492618
//function DiskInDrive(Drive: AnsiChar): Boolean;
//var
//  ErrorMode: Word;
//begin
//  { make it upper case }
//  if Drive in ['a'..'z'] then Dec(Drive, $20);
//  { make sure it's a letter }
//  if not (Drive in ['A'..'Z']) then
//    raise EConvertError.Create('Not a valid drive ID');
//
//  { turn off critical errors }
//  ErrorMode := SetErrorMode(SEM_FailCriticalErrors);
//  try
//    { drive 1 = a, 2 = b, 3 = c, etc. }
//    if DiskSize(Ord(Drive) - $40) = -1 then
//      Result := False
//    else
//      Result := True;
//  finally
//    { Restore old error mode }
//    SetErrorMode(ErrorMode);
//  end;
//
//end;

procedure MM_ObjectBinaryToText(const Input, Output: TStream);
var
  NestingLevel: Integer;
  Reader: TReader;
  Writer: TWriter;
  ObjectName, PropName: string;
  UTF8Idents: Boolean;
//  MemoryStream: TMemoryStream;
  LFormatSettings: TFormatSettings;
//  AFileName: string;
  procedure WriteIndent;
  var
    Buf: TBytes;
    I: Integer;
  begin
    Buf := TBytes.Create($20, $20);
    for I := 1 to NestingLevel do Writer.Write(Buf, Length(Buf));
  end;

  procedure WriteTBytes(S: TBytes);
  begin
    Writer.Write(S, Length(S));
  end;

  procedure WriteAsciiStr(const S: String);
  var
    Buf: TBytes;
    I: Integer;
  begin
    SetLength(Buf, S.Length);
    for I := Low(S) to High(S) do
      Buf[I-Low(S)] := Byte(S[I]);
    Writer.Write(Buf, Length(Buf));
  end;

  procedure WriteByte(const B: Byte);
  begin
    Writer.Write(B, 1);
  end;

  procedure WriteUTF8Str(const S: string);
  var
    Ident: TBytes; // UTF8String;
  begin
    Ident := TEncoding.UTF8.GetBytes(S);

    if not UTF8Idents and (Length(Ident) > S.Length) then
      UTF8Idents := True;
    WriteTBytes(Ident);
  end;

  procedure NewLine;
  begin
    WriteAsciiStr(sLineBreak);
    WriteIndent;
  end;

  procedure ConvertValue; forward;

  procedure ConvertHeader;
  var
    ClassName: string;
    Flags: TFilerFlags;
    Position: Integer;
  begin
    Reader.ReadPrefix(Flags, Position);
    ClassName := Reader.ReadStr;
    ObjectName := Reader.ReadStr;
    WriteIndent;
    if ffInherited in Flags then
      WriteAsciiStr('inherited ')
    else if ffInline in Flags then
      WriteAsciiStr('inline ')
    else
      WriteAsciiStr('object ');
    if ObjectName <> '' then
    begin
      WriteUTF8Str(ObjectName);
      WriteAsciiStr(': ');
    end;
    WriteUTF8Str(ClassName);
    if ffChildPos in Flags then
    begin
      WriteAsciiStr(' [');
      WriteAsciiStr(IntToStr(Position));
      WriteAsciiStr(']');
    end;

    if ObjectName = '' then
      ObjectName := ClassName;  // save for error reporting

    WriteAsciiStr(sLineBreak);
  end;

  procedure ConvertBinary;
  const
    BytesPerLine = 32;
  var
    MultiLine: Boolean;
    I: Integer;
    Count: Integer;
    Buffer: TBytes; // array[0..BytesPerLine - 1] of AnsiChar;
    Text: TBytes; // array[0..BytesPerLine * 2 - 1] of AnsiChar;
  begin
    SetLength(Buffer, BytesPerLine);
    SetLength(Text, BytesPerLine*2+1);

    Reader.ReadValue;
    WriteAsciiStr('{');
    Inc(NestingLevel);
    Reader.Read(Count, SizeOf(Count));
    MultiLine := Count >= BytesPerLine;
    while Count > 0 do
    begin
      if MultiLine then NewLine;
      if Count >= 32 then I := 32 else I := Count;
      Reader.Read(Buffer, I);
      BinToHex(Buffer, 0, Text, 0, I);
      Writer.Write(Text, I * 2);
      Dec(Count, I);
    end;
    Dec(NestingLevel);
    WriteAsciiStr('}');
  end;

  procedure ConvertProperty; forward;

  procedure ConvertValue;
  const
    LineLength = 64;
  var
    I, J, K, L: Integer;
    S: String;
    W: String;
    LineBreak: Boolean;
  begin
    case Reader.NextValue of
      vaList:
        begin
          Reader.ReadValue;
          WriteAsciiStr('(');
          Inc(NestingLevel);
          while not Reader.EndOfList do
          begin
            NewLine;
            ConvertValue;
          end;
          Reader.ReadListEnd;
          Dec(NestingLevel);
          WriteAsciiStr(')');
        end;
      vaInt8, vaInt16, vaInt32:
        WriteAsciiStr(IntToStr(Reader.ReadInteger));
      vaExtended, vaDouble:
        WriteAsciiStr(FloatToStrF(Reader.ReadFloat, ffFixed, 16, 18, LFormatSettings));
      vaSingle:
        WriteAsciiStr(FloatToStr(Reader.ReadSingle, LFormatSettings) + 's');
      vaCurrency:
        WriteAsciiStr(FloatToStr(Reader.ReadCurrency * 10000, LFormatSettings) + 'c');
      vaDate:
        WriteAsciiStr(FloatToStr(Reader.ReadDate, LFormatSettings) + 'd');
      vaWString, vaUTF8String:
        begin
          W := Reader.ReadString;
          L := High(W);
          if L = High('') then WriteAsciiStr('''''') else
          begin
            I := Low(W);
            Inc(NestingLevel);
            try
              if L > LineLength then NewLine;
              K := I;
              repeat
                LineBreak := False;
                if (W[I] >= ' ') and (W[I] <> '''') and (Ord(W[i]) <= 127) then
                begin
                  J := I;
                  repeat
                    Inc(I)
                  until (I > L) or (W[I] < ' ') or (W[I] = '''') or
                    ((I - K) >= LineLength) or (Ord(W[i]) > 127);
                  if ((I - K) >= LineLength) then LineBreak := True;
                  WriteAsciiStr('''');
                  while J < I do
                  begin
                    WriteByte(Byte(W[J]));
                    Inc(J);
                  end;
                  WriteAsciiStr('''');
                end else
                begin
                  WriteAsciiStr('#');
                  WriteAsciiStr(IntToStr(Ord(W[I])));
                  Inc(I);
                  if ((I - K) >= LineLength) then LineBreak := True;
                end;
                if LineBreak and (I <= L) then
                begin
                  WriteAsciiStr(' +');
                  NewLine;
                  K := I;
                end;
              until I > L;
            finally
              Dec(NestingLevel);
            end;
          end;
        end;
      vaString, vaLString:
        begin
          S := Reader.ReadString;
          L := High(S);
          if L = High('') then WriteAsciiStr('''''') else
          begin
            I := Low(S);
            Inc(NestingLevel);
            try
              if L > LineLength then NewLine;
              K := I;
              repeat
                LineBreak := False;
                if (S[I] >= ' ') and (S[I] <> '''') then
                begin
                  J := I;
                  repeat
                    Inc(I)
                  until (I > L) or (S[I] < ' ') or (S[I] = '''') or
                    ((I - K) >= LineLength);
                  if ((I - K) >= LineLength) then
                  begin
                    LIneBreak := True;

//                    if ByteType(S, I) = mbTrailByte then Dec(I);
                  end;
                  WriteAsciiStr('''');

                  WriteAsciiStr(S.Substring(J-Low(S), I-J));
                  WriteAsciiStr('''');
                end else
                begin
                  WriteAsciiStr('#');
                  WriteAsciiStr(IntToStr(Ord(S[I])));
                  Inc(I);
                  if ((I - K) >= LineLength) then LineBreak := True;
                end;
                if LineBreak and (I <= L) then
                begin
                  WriteAsciiStr(' +');
                  NewLine;
                  K := I;
                end;
              until I > L;
            finally
              Dec(NestingLevel);
            end;
          end;
        end;
      vaIdent, vaFalse, vaTrue, vaNil, vaNull:
        WriteUTF8Str(Reader.ReadIdent);
      vaBinary:
        ConvertBinary;
      vaSet:
        begin
          Reader.ReadValue;
          WriteAsciiStr('[');
          I := 0;
          while True do
          begin
            S := Reader.ReadStr;
            if S = '' then Break;
            if I > 0 then WriteAsciiStr(', ');
            WriteUtf8Str(S);
            Inc(I);
          end;
          WriteAsciiStr(']');
        end;
      vaCollection:
        begin
          Reader.ReadValue;
          WriteAsciiStr('<');
          Inc(NestingLevel);
          while not Reader.EndOfList do
          begin
            NewLine;
            WriteAsciiStr('item');
            if Reader.NextValue in [vaInt8, vaInt16, vaInt32] then
            begin
              WriteAsciiStr(' [');
              ConvertValue;
              WriteAsciiStr(']');
            end;
            WriteAsciiStr(sLineBreak);
            Reader.CheckValue(vaList);
            Inc(NestingLevel);
            while not Reader.EndOfList do
              ConvertProperty;
            Reader.ReadListEnd;
            Dec(NestingLevel);
            WriteIndent;
            WriteAsciiStr('end');
          end;
          Reader.ReadListEnd;
          Dec(NestingLevel);
          WriteAsciiStr('>');
        end;
      vaInt64:
        WriteAsciiStr(IntToStr(Reader.ReadInt64));
    else
      raise EReadError.CreateResFmt(@sPropertyException,
        [ObjectName, DotSep, PropName, IntToStr(Ord(Reader.NextValue))]);
    end;
  end;

  procedure ConvertProperty;
  begin
    WriteIndent;
    PropName := Reader.ReadStr;  // save for error reporting
    WriteUTF8Str(PropName);
    WriteAsciiStr(' = ');
    ConvertValue;
    WriteAsciiStr(sLineBreak);
  end;

  procedure ConvertObject;
  begin
    ConvertHeader;
    Inc(NestingLevel);
    while not Reader.EndOfList do
      ConvertProperty;
    Reader.ReadListEnd;
    while not Reader.EndOfList do
      ConvertObject;
    Reader.ReadListEnd;
    Dec(NestingLevel);
    WriteIndent;
    WriteAsciiStr('end' + sLineBreak);
  end;
const
  BufferSize = 4096;
//var
//  CompressionStream: TCompressionStream;
//  DeCompressionStream: TDeCompressionStream;
//  AFileStream: TFileStream;
//  Data: TBytes;
//  Count: LongInt;
//  AFileName: String;
begin
  NestingLevel := 0;
  UTF8Idents := False;
  Reader := TReader.Create(Input, 4096);
  LFormatSettings := TFormatSettings.Create('en-US'); // do not localize
  LFormatSettings.DecimalSeparator := '.';
  try
    if UTF8Idents then
      Output.Write(TEncoding.UTF8.GetPreamble[0], 3);
    Writer := TWriter.Create(Output, 4096);
    try
      Reader.ReadSignature;
      ConvertObject;
    finally
      Writer.Free;
    end;
  finally
    Reader.Free;
  end;
end;

function PestObsName(const ObsName: string): string;
begin
  result := StringReplace(ObsName, ' ', '_', [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '[', '_', [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, ']', '_', [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '(', '_', [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, ')', '_', [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '!', '_', [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '@', '_', [rfReplaceAll, rfIgnoreCase]);
end;

function ExtractNonCommentLine(const ALine: string): string;
var
  CommentMarkerPosition: Integer;
begin
  result := Trim(ALine);

  if (Pos('#', result) = 1) or (Pos('!', result) = 1)  or (Pos('//', result) = 1) then
  begin
    result := '';
    Exit;
  end;
  CommentMarkerPosition := Pos('#', result);
  if CommentMarkerPosition > 1 then
  begin
    result := Copy(result, 1, CommentMarkerPosition-1);
  end;
  CommentMarkerPosition := Pos('!', result);
  if CommentMarkerPosition > 1 then
  begin
    result := Copy(result, 1, CommentMarkerPosition-1);
  end;
  CommentMarkerPosition := Pos('//', result);
  if CommentMarkerPosition > 1 then
  begin
    result := Copy(result, 1, CommentMarkerPosition-1);
  end;
  result := UpperCase(result);
end;

function IsBeginningOfSection(const ALine: string;
  out Section: string): Boolean;
const
  strBegin = 'BEGIN ';
var
  BeginPosition: Integer;
begin
  BeginPosition := Pos(strBegin, UpperCase(ALine));
  result := BeginPosition = 1;
  if result then
  begin
    Section := Copy(ALine, Length(strBegin)+1, MAXINT);
  end
  else
  begin
    Section := '';
  end;
end;

function IsEndOfSection(const ALine: string): Boolean;
begin
  result := Pos('END', UpperCase(ALine)) = 1;
end;

function Interpolate(X, StartX, EndX, StartY, EndY: double): double;
begin
  if X = StartX then
  begin
    result := StartY;
  end
  else if X = EndX then
  begin
    result := EndY;
  end
  else if StartX = EndX then
  begin
    result := (StartY+EndY)/2;
  end
  else
  begin
    result := StartY + (EndY-StartY)/(EndX-StartX)*(X-StartX);
  end;

end;

function RoundSigDigits(ANumber: Extended; SignificantDigits: Integer): Extended;
var
  NumDigits: Integer;
  Factor: Extended;
begin
  if ANumber = 0 then
  begin
    result := ANumber;
  end
  else
  begin
    NumDigits := Ceil(Log10(Abs(ANumber)));
    NumDigits := SignificantDigits - NumDigits;
    Factor := Power(10,NumDigits);
    result := Round(ANumber*Factor)/Factor;
  end;
end;

initialization
  ColorParameters := TColorParameters.Create;
  StartTimeCmdLines := TStringList.Create;
  ShowElapsedTimeCmdLines := TStringList.Create;

  StartTimeCmdLines.Add('@echo off');
  StartTimeCmdLines.Add('rem Get start time:');
  StartTimeCmdLines.Add('for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (');
  StartTimeCmdLines.Add('   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"');
  StartTimeCmdLines.Add(')');
  StartTimeCmdLines.Add('@echo on');

  ShowElapsedTimeCmdLines.Add('@echo off');
  ShowElapsedTimeCmdLines.Add('rem Get end time:');
  ShowElapsedTimeCmdLines.Add('for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (');
  ShowElapsedTimeCmdLines.Add('   set /A "end=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"');
  ShowElapsedTimeCmdLines.Add(')');
  ShowElapsedTimeCmdLines.Add('');
  ShowElapsedTimeCmdLines.Add('rem Get elapsed time:');
  ShowElapsedTimeCmdLines.Add('set /A elapsed=end-start');
  ShowElapsedTimeCmdLines.Add('');
  ShowElapsedTimeCmdLines.Add('rem Show elapsed time:');
  ShowElapsedTimeCmdLines.Add('set /A hh=elapsed/(60*60*100), rest=elapsed%%(60*60*100), mm=rest/(60*100), rest%%=60*100, ss=rest/100, cc=rest%%100');
  ShowElapsedTimeCmdLines.Add('if %mm% lss 10 set mm=0%mm%');
  ShowElapsedTimeCmdLines.Add('if %ss% lss 10 set ss=0%ss%');
  ShowElapsedTimeCmdLines.Add('if %cc% lss 10 set cc=0%cc%');
  ShowElapsedTimeCmdLines.Add('echo elapsed time: %hh% hours, %mm% minutes %ss%.%cc% seconds');
  ShowElapsedTimeCmdLines.Add('');

finalization
  ColorParameters.Free;
  StartTimeCmdLines.Free;
  ShowElapsedTimeCmdLines.Free;

end.

