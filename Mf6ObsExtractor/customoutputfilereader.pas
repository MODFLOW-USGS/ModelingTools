unit CustomOutputFileReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type

  TFileType = (ftText, ftBinary);
  TCustomOutputFile = class;

  TFileId = record
    Key: string;
    OutputFile: TCustomOutputFile;
    Position: Integer;
  end;

  TObservationDictionary = specialize TDictionary<string, TFileId>;

  { TCustomOutputFile }

  TCustomOutputFile = class(TObject)
  private
    //FNOBS: Integer;
    function GetFirstValue(Index: integer): double;
    function GetSecondValue(Index: integer): double;
  protected
    FIdLocations: TObservationDictionary;
    FFileName: string;
    FFileType: TFileType;
    FTextFile: TextFile;
    FBinaryFile: TFileStream;
    FBinaryFileSize: Int64;
    FFirstValues: array of double;
    FSecondValues: array of double;
    FFirstTime: double;
    FSecondTime: double;
    procedure ReadHeader; virtual; abstract;
  public
    constructor Create(FileName: string; FileType: TFileType;
      IdLocations: TObservationDictionary);
    destructor Destroy; override;
    property FileName: string read FFileName;
    procedure ReadTimeAndValues; virtual; abstract;
    property FirstTime: double read FFirstTime;
    property SecondTime: double read FSecondTime;
    property FirstValue[Index: integer]: double read GetFirstValue;
    property SecondValue[Index: integer]: double read GetSecondValue;
  end;

  TOutputFileObjectList = specialize TObjectList<TCustomOutputFile>;

  EReadOutputError = class(Exception);


implementation

{ TCustomOutputFile }

function TCustomOutputFile.GetFirstValue(Index: integer): double;
begin
  result := FFirstValues[Index];
end;

function TCustomOutputFile.GetSecondValue(Index: integer): double;
begin
  result := FSecondValues[Index];
end;

constructor TCustomOutputFile.Create(FileName: string; FileType: TFileType;
  IdLocations: TObservationDictionary);
begin
  //FTime := 0;
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
  FIdLocations:= IdLocations;
  ReadHeader;
  ReadTimeAndValues;
  ReadTimeAndValues;
end;

destructor TCustomOutputFile.Destroy;
begin
  if FFileType = ftText then
  begin
    CloseFile(FTextFile);
  end;
  FBinaryFile.Free;
  inherited Destroy;
end;

end.

