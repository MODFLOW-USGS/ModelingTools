unit OutputFileReader;

{$mode objfpc}{$H+}
{$ASSERTIONS ON}

interface

uses
  Classes, SysUtils, CustomOutputFileReader;

type
  { TOutputFile }

  TOutputFile = class(TCustomOutputFile)
  private
    FNOBS: Integer;
  protected
    procedure ReadHeader; override;
  public
    procedure ReadTimeAndValues; override;
  end;

implementation

resourcestring
  rsTheIdentifie = 'The identifier %0:s in %1:s duplicates another identifier '
    +'in %2:s';

{ TOutputFile }

procedure TOutputFile.ReadHeader;
var
  ALine: string;
  Splitter: TStringList;
  FileID: TFileId;
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
    if FIdLocations.TryGetValue(ID, FileID) then
    begin
      raise EReadOutputError.Create(Format(rsTheIdentifie, [ID, FFileName,
        FileID.OutputFile.FileName]));
    end
    else
    begin
      FileID.OutputFile := self;
      FileID.Key := ID;
      FileID.Position := Index;
      FIdLocations.Add(ID, FileID);
    end;
  end;

begin
  if FFileType = ftBinary then
  begin
    FBinaryFile.read(ObsTypeArray[0], Length(ObsTypeArray)*SizeOf(AnAnsiChar));
    if string(ObsTypeArray) <> 'cont' then
    begin
      raise EReadOutputError.Create(Format('Error reading the header of %s.',
        [FFileName]));
    end;
    FBinaryFile.read(AnAnsiChar, SizeOf(AnAnsiChar));
    FBinaryFile.read(PrecisionArray[0],
      Length(PrecisionArray)*SizeOf(AnAnsiChar));
    if string(PrecisionArray) <> 'double' then
    begin
      raise EReadOutputError.Create(Format('Error reading the header of %s.',
        [FFileName]));
    end;
    FBinaryFile.read(LENOBSNAME_Array,
      Length(LENOBSNAME_Array)*SizeOf(AnAnsiChar));
    LENOBSNAME := StrToInt(Trim(string(LENOBSNAME_Array)));
    FBinaryFile.read(DummyArray[0], Length(DummyArray)*SizeOf(AnAnsiChar));
    FBinaryFile.read(FNOBS, SizeOf(FNOBS));
    SetLength(ObsNameArray, LENOBSNAME);
    for ObsNameIndex := 0 to Pred(FNOBS) do
    begin
      Index := ObsNameIndex+1;
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
      FIdLocations.Capacity := FIdLocations.Count + Splitter.Count;
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

procedure TOutputFile.ReadTimeAndValues;
var
  Splitter: TStringList;
  ALine: string;
  Index: Integer;
  ATime: double;
  FValues: array of double;
begin
  SetLength(FValues, FNOBS);
  if FFileType = ftBinary then
  begin
    if FBinaryFile.Position = FBinaryFileSize then
    begin
      ATime := -1;
    end
    else
    begin
      FBinaryFile.read(ATime, SizeOf(ATime));
      FBinaryFile.read(FValues[0], Length(FValues)*SizeOf(double));
    end;
  end
  else
  begin
    Readln(FTextFile, ALine);
    if ALine = '' then
    begin
      ATime := -1;
    end
    else
    begin
      Splitter := TStringList.Create;
      try
        Splitter.DelimitedText := ALine;
        Assert(FNOBS = Splitter.Count - 1, Format('In the line "%0:s", the number of observation values is %1:d instead of %2:d', [ALine, FIdLocations.Count - 1, FNOBS]));
        ATime := StrToFloat(Splitter[0]);
        for Index := 1 to Splitter.Count -1 do
        begin
          FValues[Index-1] := StrToFloat(Splitter[Index]);
        end;
      finally
        Splitter.Free;
      end;
    end;
  end;
  if FFirstValues = nil then
  begin
    FFirstValues := FValues;
    FFirstTime := ATime;
  end
  else
  begin
    if FSecondValues <> nil then
    begin
      FFirstValues := FSecondValues;
      FFirstTime := FSecondTime;
    end;
    FSecondValues := FValues;
    FSecondTime := ATime;
  end;
end;

end.

