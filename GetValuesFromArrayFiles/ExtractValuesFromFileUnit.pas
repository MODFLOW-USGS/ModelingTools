unit ExtractValuesFromFileUnit;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses
{$IFDEF FPC}
  fgl,
  SimpleTextWriter,
{$ELSE}
  System.IOUtils,
  System.Generics.Collections,
{$ENDIF}
  SysUtils, Classes;

type
{$IFDEF FPC}
TDoubleList = TFPGList<Double>;
{$ELSE}
   TDoubleList = TList<Double>;
{$ENDIF}

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
  Splitter: TStringList;
  ALine: string;
  ValueIndex: Integer;
  ValueName: string;
  SumProducts: Extended;
  index: Integer;
  SumWeights: Extended;
  Weight: Extended;
  Value: double;
{$IFDEF FPC}
  InputFile: TSimpleStreamReader;
  OutputFile: TSimpleTextWriter;
  ArrayFile: TSimpleStreamReader;
{$ELSE}
  InputFile: TStreamReader;
  OutputFile: TStreamWriter;
  ArrayFile: TStreamReader;
{$ENDIF}
begin
  FInputFileName := ExpandFileName(ParamStr(1));
  Assert(FileExists(FInputFileName));
  FOutputFileName := ChangeFileExt(FInputFileName, '.arrayvalues');
  FormatSettings.DecimalSeparator := '.';
  Splitter := TStringList.Create;
  try
    Splitter.StrictDelimiter := False;
    {$IFDEF FPC}
    OutputFile := TSimpleTextWriter.Create(FOutputFileName);
    InputFile := TSimpleStreamReader.Create(FInputFileName);
    {$ELSE}
    OutputFile := TFile.CreateText(FOutputFileName);
    InputFile := TFile.OpenText(FInputFileName);
    {$ENDIF}
    try
      FArrayFileName := ExpandFileName(InputFile.ReadLine);
      Assert(FileExists(FArrayFileName));
      {$IFDEF FPC}
      ArrayFile := TSimpleStreamReader.Create(FArrayFileName);
      {$ELSE}
      ArrayFile := TFile.OpenText(FArrayFileName);
      {$ENDIF}
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
            Value := SumProducts/SumWeights;
          end
          else
          begin
            Value := 0;
          end;
          OutputFile.Write(Value);
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
