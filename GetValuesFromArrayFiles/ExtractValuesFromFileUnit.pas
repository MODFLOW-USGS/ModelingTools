unit ExtractValuesFromFileUnit;

interface

uses
  System.IOUtils, System.SysUtils, System.Generics.Collections, System.Classes;

type
  TDoubleList = TList<Double>;

  TArrayFileHandler = class(TObject)
  private
    FArrayFileName: string;
    FInputFileName: string;
    FOutputFileName: string;
    FValues: TDoubleList;
    procedure SetArrayFileName(const Value: string);
    procedure SetInputFileName(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    property ArrayFileName: string read FArrayFileName write SetArrayFileName;
    property InputFileName: string read FInputFileName write SetInputFileName;
    procedure ConvertValues;
  end;

implementation

{ TArrayFileHandler }

procedure TArrayFileHandler.ConvertValues;
var
  ArrayFile: TStreamReader;
  Splitter: TStringList;
  ALine: string;
  ValueIndex: Integer;
  InputFile: TStreamReader;
  ValueName: string;
  Sum: Extended;
  OutputFile: TStreamWriter;
  index: Integer;
//  ObsValues: TDoubleList;
begin
  Assert(TFile.Exists(FArrayFileName));
  Assert(TFile.Exists(FInputFileName));
  FOutputFileName := ChangeFileExt(FInputFileName, '.arrayvalues');
  FormatSettings.DecimalSeparator := '.';
  Splitter := TStringList.Create;
  try
    ArrayFile := TFile.OpenText(FArrayFileName);
    try
      while not ArrayFile.EndOfStream do
      begin
        ALine := ArrayFile.ReadLine;
        Splitter.DelimitedText := ALine;
        for ValueIndex := 0 to Splitter.Count - 1 do
        begin
          ALine := Splitter[ValueIndex];
          FValues.Add(StrToFloat(ALine));
        end;
      end;
    finally
      ArrayFile.Free;
    end;

    OutputFile := TFile.CreateText(FOutputFileName);
    InputFile := TFile.OpenText(FInputFileName);
    try
      while not InputFile.EndOfStream do
      begin
        ALine := InputFile.ReadLine;
        Splitter.DelimitedText := ALine;
        if Splitter.Count > 1 then
        begin
          Sum := 0.0;
          ValueName := Splitter[0];
          for ValueIndex := 0 to Splitter.Count - 1 do
          begin
            ALine := Splitter[ValueIndex];
            index := StrToInt(ALine) - 1;
            Assert(index >= 0);
            Assert(index < FValues.Count);
            Sum := Sum + FValues[index];
          end;
          OutputFile.Write(Sum/(Splitter.Count-1));
          OutputFile.Write(' ' + ValueName);
        end;
      end;
    finally
      InputFile.Free;
      OutputFile.Free;
    end;

  finally
    Splitter.Free;
  end;
end;

constructor TArrayFileHandler.Create;
begin
  inherited;
  FValues := TDoubleList.Create;
end;

destructor TArrayFileHandler.Destroy;
begin
  FValues.Free;
  inherited;
end;

procedure TArrayFileHandler.SetArrayFileName(const Value: string);
begin
  FArrayFileName := Value;
end;

procedure TArrayFileHandler.SetInputFileName(const Value: string);
begin
  FInputFileName := Value;
end;

end.
