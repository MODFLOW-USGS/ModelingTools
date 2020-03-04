unit Mf6ObsUtilOutputReaderUnit;

interface

uses
  Classes, SysUtils;

type
  TFileType = (ftText, ftBinary);

  TDoubleArray = array of Double;

  EMf6ReadError = class(Exception);

  TMf6ObsOutputFile = class(TObject)
  private
    FFileName: string;
    FNOBS: Integer;
    FFileType: TFileType;
    FBinaryFile: TFileStream;
    FBinaryFileSize: Int64;
    FTextFile: TextFile;
    FTime: double;
    FValues: TDoubleArray;
    FObsNames: TStringList;
    FSuccess: Boolean;
    procedure ReadHeader;
    procedure ReadTimeAndValues;
    function GetObsName(Index: Integer): String;
  public
    constructor Create(FileName: string; FileType: TFileType);
    destructor Destroy; override;
    property NumberOfObservations: Integer read FNOBS;
    property ObsName[Index: Integer]: String read GetObsName;
    function ReadNextData(out Time: Double; out Values: TDoubleArray): Boolean;
  end;

implementation

constructor TMf6ObsOutputFile.Create(FileName: string; FileType: TFileType);
begin
  FSuccess := True;
  FObsNames := TStringList.Create;
  FFileName := FileName;
  FFileType := FileType;
  if FileType = ftBinary then
  begin
    FBinaryFile := TFileStream.Create(FileName, fmOpenRead);
    FBinaryFileSize := FBinaryFile.Size;
  end
  else
  begin
    AssignFile(FTextFile, FileName);
    reset(FTextFile);
  end;
  ReadHeader;
//  ReadTimeAndValues;
//  ReadTimeAndValues;
end;

destructor TMf6ObsOutputFile.Destroy;
begin
  if FFileType = ftText then
  begin
    CloseFile(FTextFile);
  end;
  FBinaryFile.Free;
  FObsNames.Free;
  inherited Destroy;
end;

function TMf6ObsOutputFile.GetObsName(Index: Integer): String;
begin
  result := FObsNames[index];
end;

procedure TMf6ObsOutputFile.ReadHeader;
var
  ALine: string;
  Splitter: TStringList;
//  FileID: TFileId;
  Index: Integer;
  ID: String;
  ObsTypeArray: array[0..3] of AnsiChar;
  AnAnsiChar: AnsiChar;
  PrecisionArray: array[0..5] of AnsiChar;
  LENOBSNAME_Array: array[0..3] of AnsiChar;
  LENOBSNAME: Integer;
  DummyArray: array[0..84] of AnsiChar;
  ObsNameIndex: Integer;
  ObsNameArray:  array of AnsiChar;
  procedure AddID;
  begin
    FObsNames.Add(ID);
  end;
begin
  if FFileType = ftBinary then
  begin
    FBinaryFile.read(ObsTypeArray[0], Length(ObsTypeArray)*SizeOf(AnAnsiChar));
    if string(ObsTypeArray) <> 'cont' then
    begin
      raise EMf6ReadError.Create(Format('Error reading the header of %s.',
        [FFileName]));
    end;
    FBinaryFile.read(AnAnsiChar, SizeOf(AnAnsiChar));
    FBinaryFile.read(PrecisionArray[0],
      Length(PrecisionArray)*SizeOf(AnAnsiChar));
    if string(PrecisionArray) <> 'double' then
    begin
      raise EMf6ReadError.Create(Format('Error reading the header of %s.',
        [FFileName]));
    end;
    FBinaryFile.read(LENOBSNAME_Array,
      Length(LENOBSNAME_Array)*SizeOf(AnAnsiChar));
    LENOBSNAME := StrToInt(Trim(string(LENOBSNAME_Array)));
    FBinaryFile.read(DummyArray[0], Length(DummyArray)*SizeOf(AnAnsiChar));
    FBinaryFile.read(FNOBS, SizeOf(FNOBS));
    FObsNames.Capacity := FNOBS;
    SetLength(ObsNameArray, LENOBSNAME);
    for ObsNameIndex := 0 to Pred(FNOBS) do
    begin
//      Index := ObsNameIndex+1;
      FBinaryFile.read(ObsNameArray[0],
        Length(ObsNameArray)*SizeOf(AnAnsiChar));
      ID := UpperCase(Trim(string(ObsNameArray)));
      AddID;
    end;
  end
  else
  begin
    Readln(FTextFile, ALine);
    Splitter := TStringList.Create;
    try
      Splitter.DelimitedText := ALine;
      FNOBS := Splitter.Count - 1;
      FObsNames.Capacity := FNOBS;

//      FIdLocations.Capacity := FIdLocations.Count + Splitter.Count;
      for Index := 1 to Splitter.Count -1 do
      begin
        ID := UpperCase(Splitter[Index]);
        AddID;
      end;
    finally
      Splitter.Free;
    end;
  end;
end;

function TMf6ObsOutputFile.ReadNextData(out Time: Double;
  out Values: TDoubleArray): Boolean;
begin
  ReadTimeAndValues;
  Time := FTime;
  Values := FValues;
  Result := FSuccess;
end;

procedure TMf6ObsOutputFile.ReadTimeAndValues;
var
  Splitter: TStringList;
  ALine: string;
  Index: Integer;
begin
  SetLength(FValues, FNOBS);
  if FFileType = ftBinary then
  begin
    if FBinaryFile.Position = FBinaryFileSize then
    begin
      FSuccess := False;
      FTime := FTime - 1;
    end
    else
    begin
      FBinaryFile.read(FTime, SizeOf(FTime));
      FBinaryFile.read(FValues[0], Length(FValues)*SizeOf(double));
    end;
  end
  else
  begin
    Readln(FTextFile, ALine);
    if ALine = '' then
    begin
      FSuccess := False;
      FTime := FTime - 1;
    end
    else
    begin
      Splitter := TStringList.Create;
      try
        Splitter.DelimitedText := ALine;
        Assert(FNOBS = Splitter.Count - 1,
          Format('In the line "%0:s", the number of observation values is %1:d instead of %2:d',
          [ALine, Splitter.Count - 1, FNOBS]));
        FTime := StrToFloat(Splitter[0]);
        for Index := 1 to Splitter.Count -1 do
        begin
          FValues[Index-1] := StrToFloat(Splitter[Index]);
        end;
      finally
        Splitter.Free;
      end;
    end;
  end;
//  if FFirstValues = nil then
//  begin
//    FFirstValues := FValues;
//    FFirstTime := FTime;
//  end
//  else
//  begin
//    if FSecondValues <> nil then
//    begin
//      FFirstValues := FSecondValues;
//      FFirstTime := FSecondTime;
//    end;
//    FSecondValues := FValues;
//    FSecondTime := FTime;
//  end;
end;


end.
