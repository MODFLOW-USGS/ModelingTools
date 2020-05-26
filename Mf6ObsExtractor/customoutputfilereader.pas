unit CustomOutputFileReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TDoubleArray = array of double;

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
    FFirstValues: TDoubleArray;
    FSecondValues: TDoubleArray;
    FFirstTime: double;
    FSecondTime: double;
    FFileType: TFileType;
    FFileName: string;
    function GetFirstValue(Index: integer): double;
    function GetSecondValue(Index: integer): double;
  protected
    FObservationDictionary: TObservationDictionary;
    FTextFile: TextFile;
    FBinaryFile: TFileStream;
    FBinaryFileSize: Int64;
    procedure ReadHeader; virtual; abstract;
    procedure UpdateStoredValues(const ATime: double; const Values: TDoubleArray);
  public
    constructor Create(AFileName: string; AFileType: TFileType;
      ObservationDictionary: TObservationDictionary);
    destructor Destroy; override;
    property FileName: string read FFileName;
    procedure ReadTimeAndValues; virtual; abstract;
    property FirstTime: double read FFirstTime;
    property SecondTime: double read FSecondTime;
    property FirstValue[Index: integer]: double read GetFirstValue;
    property SecondValue[Index: integer]: double read GetSecondValue;
    property FileType: TFileType read FFileType;
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

procedure TCustomOutputFile.UpdateStoredValues(const ATime: double;
  const Values: TDoubleArray);
begin
  if FFirstValues = nil then
  begin
    FFirstValues := Values;
    FFirstTime := ATime;
  end
  else
  begin
    if FSecondValues <> nil then
    begin
      FFirstValues := FSecondValues;
      FFirstTime := FSecondTime;
    end;
    FSecondValues := Values;
    FSecondTime := ATime;
  end;
end;

constructor TCustomOutputFile.Create(AFileName: string; AFileType: TFileType;
  ObservationDictionary: TObservationDictionary);
begin
  //FTime := 0;
  FFileName := AFileName;
  FFileType := AFileType;
  if AFileType = ftBinary then
  begin
    FBinaryFile := TFileStream.Create(AFileName, fmOpenRead);
    FBinaryFileSize := FBinaryFile.Size;
  end
  else
  begin
    AssignFile(FTextFile, AFileName);
    reset(FTextFile);
  end;
  FObservationDictionary:= ObservationDictionary;
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

