unit CustomFileWriterUnit;
  { This unit borrows much from unit CustomModflowWriterUnit
    of ModelMuse by RB Winston }

interface

  uses Classes, Sysutils;

  type
    TFileOption = (foNone, foInput, foOutput);
    TCustomFileWriter = class(TObject)
    private
      fFileStream: TFileStream;
    protected
      procedure CloseFile;
      class function FortranDecimal(NumberString: string): string;
      class function FixedFormattedInteger(const Value, Width: integer): string;
      class function FixedFormattedReal(const Value: double;
                     const Width: integer): string;
      class function FreeFormattedReal(const Value : double) : string;
      procedure OpenFile(const FileName: string);
      class function FileName(const AFileName: string): string;
      class function Extension: string; virtual; abstract;
      procedure WriteCommentLines(const Lines: TStrings);
    public
      constructor Create(); virtual;
      procedure NewLine;
      procedure WriteCommentLine(const Comment: string);
      procedure WriteFloat(const Value: double);
      procedure WriteInteger(Const Value: integer);
      procedure WriteString(const Value: string);
      class procedure WriteToFile(const Ftype: string;
        const UnitNumber: integer; FileName: string;
        const Option: TFileOption);
      class procedure AddComment(const Comment: string);
    end;

implementation

var
  Outputfile: TStringList;

{ TCustomFileWriter }

class procedure TCustomFileWriter.AddComment(const Comment: string);
begin
  OutputFile.Add('# ' + Comment);
end;

procedure TCustomFileWriter.CloseFile;
begin
  FreeAndNil(fFileStream);
end;

constructor TCustomFileWriter.Create();
begin
  inherited;
end;

class function TCustomFileWriter.FileName(const AFileName: string): string;
begin
  result := ChangeFileExt(AFileName, Extension);
end;

class function TCustomFileWriter.FixedFormattedInteger(const Value,
  Width: integer): string;
var
  Index : integer;
  PadIndex : integer;
begin
  for Index := Width downto 1 do
  begin
    result := Format(' %.*d', [Index, Value]);
    if Value < 0 then
    begin
      while (Length(result) > 3) and (result[3] = '0') do
      begin
        Delete(result, 3, 1);
      end;
    end
    else
    begin
      while (Length(result) > 2) and (result[2] = '0') do
      begin
        Delete(result, 2, 1);
      end;
    end;

    if Length(result) <= Width then
    begin
      for PadIndex := 0 to Width - Length(result) -1 do
      begin
        result := ' ' + result;
      end;
      break;
    end;
  end;
end;

class function TCustomFileWriter.FixedFormattedReal(const Value: double;
  const Width: integer): string;
var
  Index : integer;
  PadIndex : integer;
begin
  for Index := Width downto 1 do
  begin
    result := Format(' %.*g', [Index, Value]);
    if Length(result) <= Width then
    begin
      for PadIndex := 0 to Width - Length(result) -1 do
      begin
        result := ' ' + result;
      end;
      break;
    end;
  end;
  result := FortranDecimal(result);
end;

class function TCustomFileWriter.FortranDecimal(NumberString: string): string;
begin
  if FormatSettings.DecimalSeparator = '.' then
  begin
    result := NumberString;
  end
  else
  begin
    result := StringReplace(NumberString, FormatSettings.DecimalSeparator, '.',
      [rfReplaceAll]);
  end;
end;

class function TCustomFileWriter.FreeFormattedReal
               (const Value: double): string;
begin
  //result := FortranDecimal(Format('%.13e ', [Value]));
  // Need maximum precision for perturbation sensitivities
  //result := FortranDecimal(Format('%.15e ', [Value]));
  // try g (general) format
  result := FortranDecimal(Format('%.15g ', [Value]));
end;

procedure TCustomFileWriter.NewLine;
begin
  WriteString(#13#10);
end;

procedure TCustomFileWriter.OpenFile(const FileName: string);
begin
  fFileStream:= TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
end;

procedure TCustomFileWriter.WriteCommentLine(const Comment: string);
begin
  WriteString('# ' + Comment);
  NewLine;
end;

procedure TCustomFileWriter.WriteCommentLines(const Lines: TStrings);
var
  LineIndex: Integer;
begin
  for LineIndex := 0 to Lines.Count - 1 do
  begin
    WriteCommentLine(Lines[LineIndex]);
  end;
end;

procedure TCustomFileWriter.WriteFloat(const Value: double);
begin
  WriteString(' ' + FreeFormattedReal(Value));
end;

procedure TCustomFileWriter.WriteInteger(const Value: integer);
var
  ValueAsString: string;
begin
  ValueAsString := ' ' + IntToStr(Value);
  WriteString(ValueAsString);
end;

procedure TCustomFileWriter.WriteString(const Value: string);
begin
  if Length(Value) > 0 then
  begin
    FFileStream.Write(Value[1], Length(Value));
  end;
end;

class procedure TCustomFileWriter.WriteToFile(const Ftype: string;
  const UnitNumber: integer; FileName: string; const Option: TFileOption);
var
  Line: string;
begin
  // Not sure I need this, if I can manage without using FileStreams
  FileName := ExtractFileName(FileName);
  Line := Ftype + ' ' + IntToStr(UnitNumber) + ' ' + FileName;
  case Option of
    foNone: ;// do nothing
    foInput:
      begin
        Line := Line + ' ' + 'OLD';
      end;
    foOutput:
      begin
        Line := Line + ' ' + 'REPLACE';
      end;
  end;
  OutputFile.Add(Line);
end;

end.
