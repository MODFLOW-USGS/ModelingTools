unit ModflowUnit;

{ This unit is for procedures that apply to both MODFLOW-2000 and MODFLOW-2005 }

interface

  uses Classes, SysUtils,
       Utilities;

  function GetNameFileEntry(const aNameFile: TFileName;
               const FileType: string; var UnitNum: integer;
               var FileName: TFileName): boolean;
  function GetNameForUnit(const slNameFile: TStringList; const UnitNum: integer;
               var FileName: TFileName): boolean;
  function GetNextNameFileEntry(const slNameFile: TStringList;
               var LineNum: integer; var FileType: string;
               var UnitNum: integer; var FileName: TFileName): boolean;

implementation

//###################################################################

function GetNameFileEntry(const aNameFile: TFileName; const FileType: string;
             var UnitNum: integer; var FileName: TFileName): boolean;
var
  slFileName: TStringList;
  Line, Word: string;
  I: integer;
begin
  result := False;
  FileName := '';
  // If the file specified in ModflowNameFile does not exist, return False
  if FileExists(aNameFile) then
    begin
      slFileName := TStringList.Create;
      try
        slFileName.LoadFromFile(aNameFile);
        I := 0;
        while (not result) and (I < slFileName.count) do
          begin
            Line := slFileName.Strings[I];
            Word := GetFirstWord(Line);
            if SameText(Word, FileType) then
              begin
                // Specified file type is on this line
                Word := ParseByBlanks(Line, 2);
                UnitNum := StrToInt(Word);
                FileName := ParseByBlanks(Line, 3);
                result := True;
              end;
            I := I + 1;
          end;
      finally
        slFileName.Free;
      end;
    end;
end;

//###################################################################

function GetNameForUnit(const slNameFile: TStringList; const UnitNum: integer;
               var FileName: TFileName): boolean;
var
  I: integer;
  Line, UnitStr: string;
begin
  result := False;
  if UnitNum > 0 then
    begin
      UnitStr := IntToStr(UnitNum);
      I := 0;
      while (I < slNameFile.Count) and not result do
        begin
          Line := GetNextDataString(slNamefile,I); // increments I.
          if ParseByBlanks(Line,2) = UnitStr then
            begin
              FileName := ParseByBlanks(Line,3);
              result := True;
            end;
        end;
    end;
end;
//###################################################################

function GetNextNameFileEntry(const slNameFile: TStringList;
               var LineNum: integer; var FileType: string;
               var UnitNum: integer; var FileName: TFileName): boolean;
// Starting at line LineNum (zero-based), read first line in StringList
// slNameFile that does not start with "#", return file type, unit number, and
// file name from that line, then increment LineNume to point to next line.
// Return False if EOF is encountered.
var
  Line: string;
begin
  result := False;
  if LineNum < 0 then LineNum := 0;
  Line := GetNextDataString(slNameFile, LineNum);
  if Line <> '' then
    begin
      // Get file type, unit number, file name from Line
      FileType := GetFirstWord(Line);
      UnitNum := StrToInt(ParseByBlanks(Line,2));
      FileName := ParseByBlanks(Line,3);
      result := True;
    end;
end;

end.
