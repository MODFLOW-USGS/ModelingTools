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
  public
    constructor Create;
    destructor Destroy; override;
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
  SumProducts: Extended;
  OutputFile: TStreamWriter;
  index: Integer;
  SumWeights: Extended;
  Weight: Extended;
begin
  FInputFileName := ExpandFileName(ParamStr(1));
  Assert(TFile.Exists(FInputFileName));
  FOutputFileName := ChangeFileExt(FInputFileName, '.arrayvalues');
  FormatSettings.DecimalSeparator := '.';
  Splitter := TStringList.Create;
  try
    OutputFile := TFile.CreateText(FOutputFileName);
    InputFile := TFile.OpenText(FInputFileName);
    try
      FArrayFileName := ExpandFileName(InputFile.ReadLine);
      Assert(TFile.Exists(FArrayFileName));
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

      index := -1;
      Weight := -1;
      while not InputFile.EndOfStream do
      begin
        ALine := InputFile.ReadLine;
        Splitter.DelimitedText := ALine;
        if Splitter.Count > 1 then
        begin
          Assert(Odd(Splitter.Count));
          SumProducts := 0.0;
          SumWeights := 0.0;
          ValueName := Splitter[0];
          for ValueIndex := 1 to Splitter.Count - 1 do
          begin
            ALine := Splitter[ValueIndex];
            if Odd(ValueIndex) then
            begin
              index := StrToInt(ALine) - 1;
              Assert(index >= 0);
              Assert(index < FValues.Count);
            end
            else
            begin
              Weight := StrToFloat(ALine);
              Assert(Weight > 0);
              SumProducts := SumProducts + FValues[index]*Weight;
              SumWeights := SumWeights + Weight
            end;
          end;
          if SumWeights <> 0 then
          begin
            OutputFile.Write(SumProducts/SumWeights);
          end
          else
          begin
            OutputFile.Write(0.0);
          end;
          OutputFile.WriteLine(' ' + ValueName);
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

end.
