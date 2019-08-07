unit Utilities;

interface

  uses Classes, Dialogs, SysUtils, Windows, StdCtrls, Forms, StrUtils,
       GlobalBasicData, RbwDataGrid4, Grids;

  procedure AllowEditing(RbwGrid: TRbwDataGrid4; Allow: boolean);
  procedure SetColorSelectedRow(RbwGrid: TRbwDataGrid4; Allow: boolean);
  function BooleanToYesOrNo(const Bool: boolean): string;
  function BuildCommand(const ProgName, ArgList: string; const Quote: boolean): string;
  procedure CenterForm(aForm: TForm);
  function ChangeRelPath(OldBaseDir, NewBaseDir, OldRelPath: string): string;
  function CheckNamesInColumn(RbwGrid: TRbwDataGrid4; Column: integer;
               MaxLen: integer; var ErrName: string; var ErrRow: integer): boolean;
  procedure ClearAllSelections(RbwGrid: TRbwDataGrid4);
  function ComputerName: String;
  function CountSelectedRows(const RbwGrid: TRbwDataGrid4): integer;
  function DirectoryIsLocal(const Dir: string): boolean;
  function DirFilToAbsPath(const Dir, FName: string): string;
  function ExclTrailingBackslash(const SourceDir: string): string;
  function ExecuteCommand(const CommandLine: String; const Wait: Boolean): boolean;
  function ExtractServer(const UNCFileName: string): string;
  function FortranDecimal(NumberString: string): string;
  function FreeFormattedReal(const Value: double): string;
  function GetFirstWord(const Str: string): string;
  function GetNextDataString(const slSource: TStringList; var Index: integer): string;
  function GetNextString(const slSource: TStringList; var Index: integer): string;
  function GetQuotedString(const Str: string; var StrRem: string): string;
  function IsBlank(const Str: string): boolean;
  function IsNonBlank(const Str: string): boolean;
  function IsUNC(const Directory: string): boolean;
  function IsNumber(const Str: string): boolean;
  function J_Valid_Name(Name: string; MaxLen: integer): boolean;
  function LastPos(const Str: string; const SubStr: string): integer;
  function LongInt2Bool(const IBool: LongInt): boolean;
  function MyExtractRelativePath(const BaseName, DestName: string): string;
  function ParentDir(const SourceDir: string; var Parent: string): boolean;
  function ParseByBlanks(const S: string; const Count: Integer): string;
  procedure ParseVersion(const FullVersion: string; var ShortVersion, Build: string);
  function PathToAbsPath(const SourceDir, RelPath: string): string;
  function PathWithoutSpaces(const ProgName: string): string;
  function PosLastNonBlank(const Str: string): integer;
  function QuoteIfNeeded(const Str: string): string;
  function RelativePath(const Value: string): string;
  function RelDirToAbsDir(const SourceDir, RelDir: string): string;
  function RowContainsSelectedCell(const RbwGrid: TRbwDataGrid4; IRow: integer): boolean;
  function StrToBoolean(const Str: string): boolean;
  function TrimLeadingAndTrailingBlanks(const Str: string): string;
  function TrimLeadingBlanks(const Str: string): string;
  function TrimTrailingBlanks(const Str: string): string;
  function WriteBatchFile(ProgramLocation, CmdLineOption, BatchName: string): string;
  function WriteCustomBatchFile(ProgramLocation, AbsMIF, OutPrefix, OutPrefixPred,
                                 BatchName, ModelDir: string): string;
  function YesOrNoToBoolean(const Str: string): boolean;


implementation

//###################################################################

procedure AllowEditing(RbwGrid: TRbwDataGrid4; Allow: boolean);
begin
  if Allow then
    begin
      if not (goEditing in RbwGrid.Options) then
        begin
          RbwGrid.Options := RbwGrid.Options + [goEditing];
        end;
    end
  else
    begin
      if goEditing in RbwGrid.Options then
        begin
          RbwGrid.Options := RbwGrid.Options - [goEditing];
        end;
    end;
end;

//###################################################################

function BooleanToYesOrNo(const Bool: boolean): string;
begin
  if Bool then
    result := 'Yes'
  else
    result := 'No';
end;

//###################################################################

function PathWithoutSpaces(const ProgName: string): string;
var
  TempName: string;
begin
  TempName := TrimLeadingAndTrailingBlanks(ProgName);
  if AnsiPos(' ', TempName) > 0 then
    begin
      Result := ExtractShortPathName(TempName);
    end
  else
    begin
      Result := TempName;
    end;
end;

//###################################################################

function BuildCommand(const ProgName, ArgList: string;
                      const Quote: boolean): string;
var
  Q1, Q2: string;
  ProgNameLocal: string;
begin
  ProgNameLocal := PathWithoutSpaces(ProgName);
  Q1 := #039; //  #039 is ASCII for single quote.
  Q2 := '"';
  if True then
    if Quote then
      begin
        result := Q1 + ProgNameLocal + ' ' + Q2 + ArgList + Q2 + Q1;
      end
    else
      begin
        result := ProgNameLocal + ' ' + Q2 + ArgList + Q2;
      end;
end;

//###################################################################

procedure CenterForm(aForm: TForm);
var
  h, w, x, y: integer;
begin
  // Center horizontally.
  w := (Screen.Width - aForm.Width) div 2;
  x := w;
  if x < 0 then
    x := 0
  else
    if x + w > Screen.Width then
      x := Screen.Width - aForm.Width;
  aForm.Left := x;
  // Center vertically.
  h := (Screen.Height - aForm.Height) div 2;
  y := h;
  if y < 0 then
    y := 0
  else
    if y + h > Screen.Height then
      y := Screen.Height - aForm.Height;
  aForm.Top := y;
end;

//###################################################################

function ChangeRelPath(OldBaseDir, NewBaseDir, OldRelPath: string): string;
var
  AbsPath: string;
begin
  AbsPath := PathToAbsPath(OldBaseDir, OldRelPath);
  result := MyExtractRelativePath(NewBaseDir,AbsPath);
end;

//###################################################################

function CheckNamesInColumn(RbwGrid: TRbwDataGrid4; Column: integer; MaxLen: integer;
                            var ErrName: string; var ErrRow: integer): boolean;
var
  I: Integer;
begin
  // Validate contents of one column of a TRbwDataGrid4 as valid JUPITER names
  result := True;
  ErrName := '';
  ErrRow := -1;
  for I := 1 to RbwGrid.RowCount - 1 do
  begin
    if not J_Valid_Name(RbwGrid.Cells[Column, I],MaxLen) then
    begin
      ErrName := RbwGrid.Cells[Column, I];
      ErrRow := I;
      result := False;
      Break;
    end;
  end;
end;

//###################################################################

procedure ClearAllSelections(RbwGrid: TRbwDataGrid4);
var
  DummyRect: TGridRect;
begin
  // Set selections to be invisible and invalid
  RbwGrid.ClearSelection;  // Affects range selection.
  DummyRect.Left := -1;
  DummyRect.Top := -1;
  DummyRect.Right := -1;
  DummyRect.Bottom := -1;
  RbwGrid.Selection := DummyRect;
end;

//###################################################################

function ComputerName: String;
var
  buffer: array[0..Max_ComputerName_Length-1] of char;
  size: dword;
begin
  size := Max_ComputerName_Length;
  if GetComputerName(buffer, size) then
    Result := buffer
  else
    Result := ''
end;

//###################################################################

function CountSelectedRows(const RbwGrid: TRbwDataGrid4): integer;
var
  I, J: integer;
begin
  result := 0;
  for I := 0 to RbwGrid.RowCount - 1 do
    begin
      for J := 0 to RbwGrid.ColCount - 1 do
        begin
          if RbwGrid.IsSelectedCell(J,I) then
            begin
              result := result + 1;
              Break;
            end;
        end;
    end;
end;

//###################################################################

function DirectoryIsLocal(const Dir: string): boolean;
var
  Server, UNCFileName: string;
begin
  result := False;
  UNCFileName := ExpandUNCFileName(Dir);
  Server := ExtractServer(UNCFileName);
  if (Server = '') or (AnsiSameText(Server,LocalComputerName)) then
    begin
      result := True;
    end;
end;

//###################################################################

function DirFilToAbsPath(const Dir, FName: string): string;
var
  AbsDir, RelDir, ResultTemp, Separator: string;
  Done: boolean;
begin
  Separator := '\';
  Done := False;
  //
  if length(Dir) > 1 then
    begin
      if Dir[2] = ':' then
        begin
          // Dir already is an absolute directory.
          AbsDir := ExcludeTrailingPathDelimiter(Dir);
          ResultTemp := AbsDir + Separator + ExtractFileName(FName);
          Done := True;
        end;
    end;
  if not Done then
    begin
      RelDir := '.';
      AbsDir := ExcludeTrailingPathDelimiter(RelDirToAbsDir(Dir, RelDir));
      ResultTemp := AbsDir + Separator + ExtractFileName(FName);
    end;
  result := ResultTemp;
end;

//###################################################################

function ExclTrailingBackslash(const SourceDir: string): string;
var
  Len: integer;
  BName: string;
begin
  Len := length(SourceDir);
  if SourceDir[Len] = '\' then
    begin
      BName := LeftStr(SourceDir, Len-1);
    end
  else
    begin
      BName := SourceDir;
    end;
  result := BName;
end;

//###################################################################

function ExecuteCommand(const CommandLine: String; const Wait: Boolean): boolean;
// Obtained from http://www.delphicorner.f9.co.uk/articles/wapi4.htm,
//   where it is documented as ExecNewProcess.
// CommandLine must start with a fully qualified path name for an
//   executable file, and it may contain command-line parameters.
// Wait controls whether procedure waits for process to end before
//   returning control to caller.
var
  StartInfo : TStartupInfo;
  ProcInfo : TProcessInformation;
  CreateOK : Boolean;
begin
  result := True;
    { fill with known state }
  FillChar(StartInfo,SizeOf(TStartupInfo),#0);
  FillChar(ProcInfo,SizeOf(TProcessInformation),#0);
  StartInfo.cb := SizeOf(TStartupInfo);
  CreateOK := CreateProcess(nil, PChar(CommandLine), nil, nil,False,
              CREATE_NEW_PROCESS_GROUP+NORMAL_PRIORITY_CLASS,
              nil, nil, StartInfo, ProcInfo);
    { check to see if successful }
  if CreateOK then
    begin
        //may or may not be needed. Usually wait for child processes
      if Wait then
        WaitForSingleObject(ProcInfo.hProcess, INFINITE);
    end
  else
    begin
      result := False;
      ShowMessage('Unable to run '+CommandLine);
     end;
  CloseHandle(ProcInfo.hProcess);
  CloseHandle(ProcInfo.hThread);
end;

//###################################################################

function ExtractServer(const UNCFileName: string): string;
var
  LenString, PosSlash: integer;
  RightString, BSlash: string;
begin
  result := '';
  BSlash := '\';
  LenString := Length(UNCFileName) - 2;
  if LenString > 0 then
    begin
      RightString := RightStr(UNCFileName,LenString);
      PosSlash := Pos(BSlash,RightString);
      if PosSlash > 0 then
        begin
          result := LeftStr(RightString,PosSlash-1);
        end;
    end;
end;

//###################################################################

function FortranDecimal(NumberString: string): string;
begin
  if DecimalSeparator = '.' then
    begin
      result := NumberString;
    end
  else
    begin
      result := StringReplace(NumberString, DecimalSeparator, '.',
        [rfReplaceAll]);
    end;
end; // function FortranDecimal

//###################################################################

function FreeFormattedReal(const Value: double): string;
begin
  // Need maximum precision for reproducible results
  //result := FortranDecimal(Format('%.15e ', [Value]));
  // try g (general) format
  result := FortranDecimal(Format('%.15g ', [Value]));
end; // function FreeFormattedReal

//###################################################################

function GetFirstWord(const Str: string): string;
// Return first string of non-blank characters in a string.
// Leading blanks are ignored.
var
  I, L: integer;
  StrLocal: string;
begin
  StrLocal := Str;
  L := Length(StrLocal);
  I := Pos(' ',StrLocal);
  if (I = 0) or (StrLocal = ' ') then
    // Str is either all non-blanks or all blanks.
    result := StrLocal
  else
    begin
      // Str contains blanks and non-blanks.
      if I = 1 then
        while (I = 1) and (L > 0) do
          begin
            Delete(StrLocal,1,1);
            I := Pos(' ',StrLocal);
            L := L-1;
          end;
      if I = 0 then
        // All leading blanks have been removed, Str contains no other blanks.
        result := StrLocal
      else
        result := Copy(StrLocal,1,I-1);
    end;
end; // function GetFirstWord.

//###################################################################

function GetNextDataString(const slSource: TStringList; var Index: integer): string;
// Index is incremented to point to line following the line returned in result
var
  Char: string;
begin
  result := '';
  Char := '#';
  while (Char = '#') and (Index < slSource.Count) do
    begin
      Char := Copy(slSource.Strings[Index], 0, 1);
      Char := Copy(slSource.Strings[Index], 0, 1);
      if Char <> '#' then result := slSource.Strings[Index];
      Index := Index + 1;
    end;
end;

//###################################################################

function GetNextString(const slSource: TStringList; var Index: integer): string;
// Index is incremented to point to line following the line returned in result
begin
  result := '';
  if Index < slSource.Count then
    begin
      result := slSource.Strings[Index];
      Index := Index + 1;
    end;
end;

//###################################################################

function GetQuotedString(const Str: string; var StrRem: string): string;
// StrRem is Str with quoted string removed; recognizes double quotes
// If Str does not contain two double-quote characters, an empty string
// is returned.
var
  Blank, LocStr, Q2, QStr: string;
  PosQ1, PosQ2, StrLen: integer;
begin
  QStr := '';
  Blank := ' ';
  Q2 := '"';
  LocStr := Str;
  PosQ1 := AnsiPos(Q2, LocStr);
  if PosQ1 > 0 then
    begin
      LocStr := StringReplace(LocStr,Q2,Blank,[]);
      PosQ2 := AnsiPos(Q2, LocStr);
      if PosQ2 > 0 then
        begin
          StrLen := PosQ2 - PosQ1 - 1;
          LocStr := StringReplace(LocStr,Q2,Blank,[]);
          Qstr := AnsiMidStr(LocStr,PosQ1+1,StrLen);
          StrRem := StringReplace(LocStr,QStr,Blank,[]);
        end;
    end;
  result := QStr;
end;

//###################################################################

function IsBlank(const Str: string): boolean;
begin
  result := not IsNonBlank(Str);
end;

//###################################################################

function IsNonBlank(const Str: string): boolean;
var
  S: string;
begin
  S := TrimLeadingAndTrailingBlanks(Str);
  result := (S <> '');
end;

//###################################################################

function IsUNC(const Directory: string): boolean;
begin
  if LeftStr(Directory,2) = '\\' then
    result := True
  else
    result := False;
end;

//###################################################################

function IsNumber(const Str: string): boolean;
var
  X: double;
begin
  try
    X := StrToFloat(Str);
    result := True;
  except
    result := False;
  end;
end;

//###################################################################

function J_Valid_Name(Name: string; MaxLen: integer): boolean;
// Check Name argument for conformance with JUPITER naming convention
type
  setChar = set of Char;
var
  Ch: Char;
  I, LenName: integer;
  Letters, Digits, Symbols, JChars: setChar;
begin
  // Initialize character sets
  Letters := [ 'A' .. 'Z', 'a' .. 'z' ];
  Digits := [ '0' .. '9' ];
  Symbols := [ '_', '.', ':', '&', '#', '@' ];
  JChars := Letters + Digits + Symbols;
  //
  result := True;
  LenName := Length(Name);
  if (LenName > 0) and (LenName <= MaxLen) then
    begin
      Ch := Name[1];  //bad when LenName=0
      if Ch in Letters then
        begin
          for I := 2 to LenName do
            begin
              Ch := Name[I];
              if not (Ch in JChars) then
                result := False;
            end;
        end
      else
        result := False;
    end
  else
    begin
      result := False;
    end;
end; // function J_Valid_Name

//###################################################################

function LastPos(const Str: string; const SubStr: string): integer;
// Starting from the end of Str and proceeding to the beginning, find
// position of substring matching SubStr.  If not found, return 0.
var
  I, LSS: integer;
  TestStr: string;
begin
  result := 0;
  if SubStr <> '' then
    begin
      LSS := length(SubStr);
      I := length(Str) - LSS + 1;
      result := 0;
      while (I > 0) and (result = 0) do
        begin
          TestStr := MidStr(Str, I, LSS);
          if TestStr = SubStr then
            result := I;
          I := I - 1;
        end;
    end;
end;

//###################################################################

  function LongInt2Bool(const IBool: LongInt): boolean;
  // Interpret 1 as True, otherwise False
  begin
    if IBool = 1 then
      result := True
    else
      result := False;
  end;

//###################################################################

function MyExtractRelativePath(const BaseName, DestName: string): string;
var
  Len: integer;
  BName, Dot: string;
begin
  Dot := '.';
  if (BaseName = DestName) then
    begin
      result := Dot;
    end
  else
    begin
      Len := length(BaseName);
      if BaseName[Len] <> '\' then
        begin
          BName := BaseName + '\';
        end
      else
        begin
          BName := BaseName;
        end;
      result := ExtractRelativePath(BName, DestName);
    end;
end;

//###################################################################

function ParentDir(const SourceDir: string; var Parent: string): boolean;
// Return parent directory name without a trailing backslash
var
  L: integer;
  SrcDir, DSep: string;
begin
  result := False;
  DSep := '\';
  SrcDir := ExclTrailingBackslash(SourceDir);
  L := LastPos(SrcDir, DSep) - 1;
  if L > 0 then
    begin
      Parent := LeftStr(SrcDir, L);
      result := True;
    end;
end;

//###################################################################

function ParseByBlanks(const S: string; const Count: Integer): string;
// Parse a string.
var
  I, Pos1, Pos2: Integer;
  Blank, C, Temp: string;
begin
  result := '';
  if S <> Blank then
    begin
      Temp := S;
      Pos1 := 1;
      Pos2 := 1;
      Blank := ' ';
      if Temp[Length(Temp)] <> Blank then
        Temp := Temp + Blank;
      for I := 1 to Count do
        begin
          // Find Pos1, position of first non-blank character in this string
          C := Copy(Temp, Pos1, 1);
          while C = Blank do
            begin
              Pos1 := Pos1 + 1;
              C := Copy(Temp, Pos1, 1);
            end;
          // Find Pos2, position of first blank character following this string
          Pos2 := Pos1 + 1;
          C := Copy(Temp, Pos2, 1);
          while C <> Blank do
            begin
              Pos2 := Pos2 + 1;
              C := Copy(Temp, Pos2, 1);
            end;
          if I < Count then Pos1 := Pos2;
        end;
      result := Copy(Temp, Pos1, Pos2-Pos1);
    end;
end;

//###################################################################

procedure ParseVersion(const FullVersion: string; var ShortVersion,
                       Build: string);
var
  Len, LenFull, LPos: integer;
begin
  LenFull := Length(FullVersion);
  LPos := LastPos(FullVersion,'.');
  ShortVersion := Copy(FullVersion,1,LPos-1);
  Len := LenFull - LPos;
  Build := Copy(FullVersion,LPos+1,Len);
end;

//###################################################################

function PathToAbsPath(const SourceDir, RelPath: string): string;
var
  AbsDir, Dot, FName, RelDir, ResultTemp, Separator: string;
  Done: boolean;
begin
  Dot := '.';
  FName := '';
  Separator := '\';
  Done := False;
  //
  if length(RelPath) > 1 then
    begin
      if RelPath[2] = ':' then
        begin
          // RelPath already is an absolute path.
          ResultTemp := RelPath;
          Done := True;
        end;
    end;
  if not Done then
    begin
      RelDir := ExtractFileDir(RelPath);
      AbsDir := ExcludeTrailingPathDelimiter(RelDirToAbsDir(SourceDir, RelDir));
      if RelPath <> Dot then
        FName := ExtractFileName(RelPath);
      ResultTemp := AbsDir + Separator + FName;
    end;
  result := ExcludeTrailingPathDelimiter(ResultTemp);
end;

//###################################################################

function PosLastNonBlank(const Str: string): integer;
// Starting from the end of Str and proceeding to the beginning, find
// position of last non-blank character.  If all characters are blanks,
// return 0.
var
  I: integer;
  FoundNonBlank: boolean;
//  TestStr: string;
begin
  FoundNonBlank := False;
  result := 0;
  I := Length(Str);
  while (I > 0) and not FoundNonBlank  do
    begin
      if Str[I] <> ' ' then
        begin
          FoundNonBlank := True;
          result := I;
        end
      else
        I := I - 1;
    end;
end;

//###################################################################

function QuoteIfNeeded(const Str: string): string;
var
  Done: boolean;
  Len: integer;
  StrLocal, Q1: string;
begin
  Q1 := #039; // single quote
  StrLocal := trim(Str);
  Len := length(StrLocal);
  Done := False;
  if StrLocal = '' then
    begin
      result := '';
      Done := True;
    end;
  if not Done then
    begin
      if (StrLocal[1] = '"') and (StrLocal[Len] = '"') then
        begin
          result := StrLocal;
          Done := True;
        end;
      if (StrLocal[1] = Q1) and (StrLocal[Len] = Q1) then
        begin
          result := StrLocal;
          Done := True;
        end;
    end;
  if not Done then
    begin
      if PosEx(' ', MidStr(StrLocal,1,Len)) = 0 then
        begin
          result := StrLocal;
        end
      else
        begin
          result := '"' + StrLocal + '"';
        end;
    end;
end;

//###################################################################

function RelativePath(const Value: string): string;
var
  Done: Boolean;
  Blank, Q1, Q2, TempStr: string;
  PosQ: integer;
begin
  Blank := '';
  Q1 := #39;
  Q2 := '"';
  Done := False;
  if length(Value) > 1 then
    begin
      if Value[2] = ':' then
        begin
          // Value is an absolute path; convert it to a relative path.
          TempStr := MyExtractRelativePath(ProjectDirectory, Value);
          Done := True;
        end;
    end;
  if not Done then
    begin
      TempStr := Value;
    end;
  // Ensure that result does not contain any single- or double-quote characters
  PosQ := AnsiPos(Q1, TempStr);
  if PosQ > 0 then
    begin
      TempStr := StringReplace(TempStr,Q1,Blank,[rfReplaceAll]);
    end;
  PosQ := AnsiPos(Q2, TempStr);
  if PosQ > 0 then
    begin
      TempStr := StringReplace(TempStr,Q2,Blank,[rfReplaceAll]);
    end;
  result := TempStr;
end;

//###################################################################

function RelDirToAbsDir(const SourceDir, RelDir: string): string;
var
  Len, PosColon, PSepR, PSepS: integer;
  Colon, Dot, SrcDirStr, RelDirName, RelDirStr, DSep, ResultTemp: string;
  Done: boolean;
begin
  Done := False;
  Colon := ':';
  Dot := '.';
  DSep := '\';
  if SourceDir = DSep then
    begin
      ResultTemp := RelDir;
      Done := True;
    end
  else if RelDir = DSep then
    begin
      ResultTemp := SourceDir;
      Done := True;
    end;
  //
  if not Done then
    begin
      SrcDirStr := ExcludeTrailingPathDelimiter(SourceDir);
      RelDirStr := RelDir;
      PosColon := PosEx(Colon, RelDirStr);
      PSepR := PosEx(DSep, RelDirStr); // Position of the directory separator in RelDirStr
      if PosColon = 2 then
        // RelDir starts with drive letter and colon, so it already is an absolute directory.
        begin
          ResultTemp := RelDirStr;
        end
      else
        begin
          if (PSepR = 0) and (PosEx(Dot,RelDirStr)>0) then
            begin
              RelDirStr := RelDirStr + DSep;
              PSepR := PosEx(DSep, RelDirStr); // Position of the directory separator in RelDirStr
            end;
          if PSepR > 1 then
            begin
              RelDirName := LeftStr(RelDirStr, PSepR-1);
              while (RelDirName = '.') or (RelDirName = '..') do
                begin
                  if SrcDirStr = '.' then
                    begin

                    end;
                  if RelDirName = '..' then
                    begin
                      PSepS := LastPos(SrcDirStr, DSep); // Position of dir. separator in SrcDirStr
                      SrcDirStr := LeftStr(SrcDirStr, PSepS-1);
                    end;
                  PSepR := PosEx(DSep, RelDirStr); // Position of the dir. separator in RelDirStr
                  Len := length(RelDirStr) - PSepR;
                  RelDirStr := RightStr(RelDirStr, Len);
                  PSepR := PosEx(DSep, RelDirStr); // Position of the dir. separator in RelDirStr
                  if PSepR > 1 then
                    begin
                      RelDirName := LeftStr(RelDirStr, PSepR-1);
                    end
                  else
                    begin
                      RelDirName := RelDirStr;
                    end;
                end;
            end;
          ResultTemp := SrcDirStr + DSep + RelDirStr;
        end;
    end;
  result := ExcludeTrailingPathDelimiter(ResultTemp);
end;

//###################################################################

function RowContainsSelectedCell(const RbwGrid: TRbwDataGrid4; IRow: integer): boolean;
var
  J: integer;
begin
  result := False;
  if (IRow >= 0) and (IRow < RbwGrid.RowCount) then
    begin
      for J := 0 to RbwGrid.ColCount - 1 do
        begin
          if RbwGrid.IsSelectedCell(J,IRow) then
            begin
              result := True;
              Exit;
            end;
        end;
    end;
end;

//###################################################################

procedure SetColorSelectedRow(RbwGrid: TRbwDataGrid4; Allow: boolean);
begin
  if Allow then
    begin
      RbwGrid.ColorSelectedRow := True;
    end
  else
    begin
      RbwGrid.ColorSelectedRow := False;
    end;
end;

//###################################################################

function StrToBoolean(const Str: string): boolean;
// Interpret string Str as a boolean.
var
  StrTemp: string;
begin
  StrTemp := AnsiLowerCase(Str);
  result := False;
  if StrTemp = 'yes' then result := True;
  if StrTemp = 'true' then result := True;
  if StrTemp = 'y' then result := True;
  if StrTemp = 't' then result := True;
end;

//###################################################################

function TrimLeadingAndTrailingBlanks(const Str: string): string;
var
  StrLocal: string;
begin
  StrLocal := TrimLeadingBlanks(Str);
  result := TrimTrailingBlanks(StrLocal);
end;

//###################################################################

function TrimLeadingBlanks(const Str: string): string;
var
  I, L: integer;
  StrLocal: string;
begin
  StrLocal := Str;
  L := Length(StrLocal);
  I := Pos(' ',StrLocal);
  if (I = 0) or (StrLocal = ' ') then
    // Str is either all non-blanks or all blanks.
    result := StrLocal
  else
    begin
      // Str contains blanks and non-blanks.
      if I = 1 then
        // First character is a blank.
        while (I = 1) and (L > 0) do
          begin
            Delete(StrLocal,1,1);
            I := Pos(' ',StrLocal);
            L := L-1;
          end;
      // All leading blanks have been removed, Str contains no other blanks.
      result := StrLocal
    end;
end; // function TrimLeadingBlanks.

//###################################################################

function TrimTrailingBlanks(const Str: string): string;
var
  L: integer;
begin
  L := PosLastNonBlank(Str);
  if L = 0 then
    // Str is all blanks
    begin
      result := '';
    end
  else
    begin
      result := Copy(Str, 1, L);
    end;
end;

//###################################################################

function WriteBatchFile(ProgramLocation, CmdLineOption,
                        BatchName: string): string;
// This is a generic version of R. Winston's WriteModflowBatchFile function
var
  BatchFile: TStringList;
begin
  if (Length(ProgramLocation) > 0) and (ProgramLocation[1] <> '"') then
  begin
    ProgramLocation := '"' + ProgramLocation + '"';
  end;
  result := ExtractFileDir(CmdLineOption);
  result := IncludeTrailingPathDelimiter(result)
    + BatchName + '.bat';

  CmdLineOption := ExtractFileName(CmdLineOption);
  BatchFile := TStringList.Create;
  try
    BatchFile.Add(ProgramLocation + ' ' + CmdLineOption + ' /wait');
    BatchFile.Add('pause');
    BatchFile.SaveToFile(result);
  finally
    BatchFile.Free;
  end;
end;

//###################################################################

function WriteCustomBatchFile(ProgramLocation, AbsMIF, OutPrefix, OutPrefixPred,
                               BatchName, ModelDir: string): string;
var
  BatchFile: TStringList;
  AbsAppDir, AbsPrefix, AbsPrefixPred, Path: string;
begin
  if (Length(ProgramLocation) > 0) and (ProgramLocation[1] <> '"') then
  begin
    ProgramLocation := '"' + ProgramLocation + '"';
  end;
  Path := IncludeTrailingPathDelimiter(ExtractFileDir(AbsMIF));
  if Path = '\' then Path := '';
  result := Path + BatchName + '.bat';
  AbsAppDir := ExtractFileDir(AbsMIF);
  AbsPrefix := AbsAppDir + '\' + OutPrefix;
  AbsPrefixPred := AbsAppDir + '\' + OutPrefixPred;
  BatchFile := TStringList.Create;
  try
    BatchFile.Add('@echo off');
    BatchFile.Add('REM  CD to directory from which process model is invoked');
    BatchFile.Add('cd ' + ModelDir);
    BatchFile.Add('');
    BatchFile.Add('REM  Invoke analysis application');
    BatchFile.Add(ProgramLocation + ' ' + AbsMIF + ' ' + AbsPrefix + ' ' + AbsPrefixPred + ' /wait');
    BatchFile.Add('');
    BatchFile.Add('REM  Return to directory where analysis-application files reside');
    BatchFile.Add('cd ' + AbsAppDir);
    BatchFile.Add('');
    BatchFile.Add('pause');
    BatchFile.SaveToFile(result);
  finally
    BatchFile.Free;
  end;
end;

//###################################################################

function YesOrNoToBoolean(const Str: string): boolean;
begin
  if (AnsiSameText(Str,'yes')) or (AnsiSameText(Str,'y')) then
    begin
      result := True;
    end
  else
    begin
      result := False;
    end;
end;

//###################################################################

end.
