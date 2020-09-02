unit ProcessTemplateUnit;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses Classes, Generics.Collections, SysUtils, RbwParser,
  Math
{$IFDEF FPC}
  , SimpleTextWriter
{$ENDIF}
  ;

type
  TParameter = record
    ParameterName: string;
    ParameterValue: double;
  end;

  TParameterDictionary = TDictionary<string, TParameter>;

  EEnhanceTemplateError = class(Exception);

  EPvalError = class(EEnhanceTemplateError);
  EReadParamCountError = class(EPvalError);
  EReadParamError = class(EPvalError);

  ETemplateError = class(EEnhanceTemplateError);
  EBadFileName = class(ETemplateError);
  EBadDelimiter = class(ETemplateError);
  EUnmatchedDelimiter = class(ETemplateError);
  ENotEnoughPrecision = class(ETemplateError);
  EBadParamName = class(ETemplateError);
  ECompileError = class(ETemplateError);


  TParameterProcessor = class(TObject)
  private
    FTemplateFileName: string;
    FPvalFileName: string;
{$IFDEF FPC}
  FTemplateFile: TSimpleStreamReader;
  FPValFile: TSimpleStreamReader;
  FModelOutputFile: TSimpleStreamWriter;
{$ELSE}
  FTemplateFile: TStreamReader;
  FPValFile: TStreamReader;
  FModelOutputFile: TStreamWriter;
{$ENDIF}
    FParameters: TParameterDictionary;
    FParser: TRbwParser;
    FParameterDelimiter: Char;
    FFormulaDelimiter: Char;
    procedure WriteUsage;
    procedure WriteParameters;
    procedure GetFileNames;
    procedure OpenFiles;
    procedure CloseFiles;
    Procedure ReadPValFile;
    procedure ReadAndProcessTemplateLines;
    procedure ReadTemplateHeader;
    procedure ProcessTemplateLine(ALine: string);
  private
    procedure ProcessTemplate;
  public
    Constructor Create;
    destructor Destroy; override;
  end;

procedure ProcessTemplate;

implementation

uses
  StrUtils;

resourcestring
  StrUnableToReadTheN = 'Unable to read the number of parameters from the PV' +
  'AL file';
  StrInThePVALFile = 'In the PVAL file, "%s" does not have exactly two value' +
  's separated by commas or whitespace.';
  StrInThePVALFileNoConvert = 'In the PVAL file, unable to convert "%0:s" to' +
  ' a number in "1:s".';
  StrErrorReadingPVALF = 'Error reading PVAL file. Only %0:d read instead of' +
  ' %1:d.';
  StrErrorReadingThePa = 'Error Reading the %s delimiter. Parameter d' +
  'elimiters must be one character in length.';
  StrErrorReadingTheFo = 'Error reading the formula delimiter from the templ' +
  'ate file. The line for the formula delimiter must begin with "etf " follo' +
  'wed by a single character.';
  StrUnmatchedDelimiter = 'There is an unmatched %0:s delimiter in "%1:s".';
  StrNotEnoughSpace = 'There was not enough space to replace %0:s with the p' +
  'arameter value in "%1:s. The available space starts at position %2:d and ends at position %3:d.';
  StrNoParameterNamed = 'No parameter named %s was defined in the PVAL file.';
  StrDuplicateDelimiters = 'Both the parameter delimiter and formula delimit' +
  'er are set to "%s". The two must be different from each other.';
  StrConversionProblem = 'There was an error converting "%0:s" to a real number in the lined %1:s';

procedure ProcessTemplate;
var
  Processor: TParameterProcessor;
begin
  Processor := TParameterProcessor.Create;
  try
    Processor.ProcessTemplate;
  finally
    Processor.Free;
  end;
end;

function MaxPrecisionFloatToStr(value: double; AvailableWidth: Integer): string;
var
  Exponent: Integer;
  ExponentStr: string;
  ExponentLength: Integer;
  DecimalPostion: Integer;
begin
  result := FloatToStr(Value);
  if Length(result) > AvailableWidth then
  begin
    Exponent := Trunc(Log10(Abs(Value)));
    result := FloatToStrF(Value, ffGeneral, AvailableWidth,
      Max(0, AvailableWidth-Exponent-1));
    if Length(result) > AvailableWidth then
    begin
      DecimalPostion :=  Pos('.', result);
      if (Pos('E', result) = 0) and
        ((DecimalPostion <= AvailableWidth) and (DecimalPostion > 0)) then
      begin
        result := Copy(result, 1, AvailableWidth);
      end
      else
      begin
        ExponentStr := 'E' + IntToStr(Exponent);
        ExponentLength := Length(ExponentStr);
        if ExponentLength < AvailableWidth then
        begin
          result := FloatToStrF(Value, ffExponent, AvailableWidth,
            Max(0, AvailableWidth-Exponent-1));
          result := Copy(result, 1, AvailableWidth-ExponentLength) + ExponentStr
        end;
      end;
    end;
  end;
end;

function PadLeft(AString: string; AvailableWidth: Integer): string;
begin
  result := AString;
  while Length(result) < AvailableWidth do
  begin
    result := ' ' + result;
  end;
end;

function PadRight(AString: string; AvailableWidth: Integer): string;
begin
  result := AString;
  while Length(result) < AvailableWidth do
  begin
    result := result + ' ';
  end;
end;

procedure TParameterProcessor.WriteUsage;
begin
  WriteLn('Usage');
  WriteLn('  EnhancedTemplateProcessor <template file name>');
  WriteLn('  EnhancedTemplateProcessor <template file name> <PVAL file name>');
  WriteLn('A PVAL is not required if formulas do not require parameter value substitution.');
  WriteLn('File names that include white space must be enclosed in quotation marks');
end;

procedure TParameterProcessor.WriteParameters;
var
  ParamIndex: Integer;
begin
  WriteLn('File names on the command line');
  for ParamIndex := 0 to ParamCount do
  begin
    WriteLn(ParamStr(ParamIndex));
  end;
end;

procedure TParameterProcessor.CloseFiles;
begin
  FTemplateFile.Free;
  FPValFile.Free;
  FModelOutputFile.Free;
end;

constructor TParameterProcessor.Create;
begin
  FParser := TRbwParser.Create(nil);
  FParameters := TParameterDictionary.Create;
  FormatSettings.DecimalSeparator := '.';
end;

destructor TParameterProcessor.Destroy;
begin
  FParameters.Free;
  FParser.Free;
  inherited;
end;

procedure TParameterProcessor.GetFileNames;
begin
  FTemplateFileName := ParamStr(1);
  FTemplateFileName := ExpandFileName(FTemplateFileName);
  if ParamCount = 2 then
  begin
    FPvalFileName := ParamStr(2);
    FPvalFileName := ExpandFileName(FPvalFileName);
  end
  else
  begin
    FPvalFileName := '';
  end;
end;

procedure TParameterProcessor.OpenFiles;
begin
  if ExtractFileExt(FTemplateFileName) = '' then
  begin
    raise EBadFileName.Create('Template files must have an extension');
  end;
  {$IFDEF FPC}
  FTemplateFile := TSimpleStreamReader.Create(FTemplateFileName);
  {$ELSE}
  FTemplateFile := TStreamReader.Create(FTemplateFileName);
  {$ENDIF}
  if FPvalFileName <> '' then
  begin
    {$IFDEF FPC}
    FPValFile := TSimpleStreamReader.Create(FPvalFileName);
    {$ELSE}
    FPValFile := TStreamReader.Create(FPvalFileName);
    {$ENDIF}
  end
  else
  begin
    FPValFile := nil;
  end;
  {$IFDEF FPC}
  FModelOutputFile := TSimpleStreamWriter.Create(
    ChangeFileExt(FTemplateFileName, ''));
  {$ELSE}
  FModelOutputFile := TStreamWriter.Create(
    ChangeFileExt(FTemplateFileName, ''));
  {$ENDIF}
end;

procedure TParameterProcessor.ProcessTemplate;
begin
  if (ParamCount = 0) then
  begin
    WriteUsage;
    Exit;
  end;

  if (ParamCount > 2) then
  begin
    WriteUsage;
    WriteParameters;
    Exit;
  end;

  if ParamStr(1) = '?' then
  begin
    WriteUsage;
    Exit;
  end;

  try
    GetFileNames;
    OpenFiles;
    ReadPValFile;
    ReadAndProcessTemplateLines;
  finally
    CloseFiles;
  end;
end;

procedure TParameterProcessor.ProcessTemplateLine(ALine: string);
var
  StartPosition: Integer;
  EndPos: Integer;
  OriginalLine: string;
  ParameterName: string;
  Parameter: TParameter;
  ReplacementString: string;
  AvailableLength: Integer;
  AValue: double;
  TemplateParameterName: string;
  Formula: string;
  StartFormula: Integer;
  OriginalFormula: string;
  function CountFormulaDelimiterBefore(APosition: Integer): integer;
  var
    FormulaDelimiterPosition: Integer;
  begin
    result := 0;
    FormulaDelimiterPosition := Pos (FFormulaDelimiter, ALine);
    while FormulaDelimiterPosition > 0 do
    begin
      Inc(result);
      FormulaDelimiterPosition := PosEx (FFormulaDelimiter, ALine,
        Succ(FormulaDelimiterPosition));
      if FormulaDelimiterPosition > APosition then
      begin
        break;
      end;
    end;

  end;

begin
  OriginalLine := ALine;
  if FParameterDelimiter <> ' ' then
  begin
    StartPosition := Pos (FParameterDelimiter, ALine);
    while StartPosition > 0 do
    begin
      EndPos := PosEx(FParameterDelimiter, ALine, Succ(StartPosition));
      if EndPos = 0 then
      begin
        raise EUnmatchedDelimiter.Create(Format(StrUnmatchedDelimiter,
          ['parameter', OriginalLine]));
      end;

      TemplateParameterName := Copy(ALine, Succ(StartPosition), EndPos-StartPosition-1);
      ParameterName := UpperCase(Trim(TemplateParameterName));

      if FParameters.TryGetValue(ParameterName, Parameter) then
      begin
        AvailableLength := EndPos-StartPosition+1;
        ReplacementString := MaxPrecisionFloatToStr(Parameter.ParameterValue,
          AvailableLength);
        if Odd(CountFormulaDelimiterBefore(StartPosition)) then
        begin
          ReplacementString := PadRight(ReplacementString, AvailableLength);
        end
        else
        begin
          ReplacementString := PadLeft(ReplacementString, AvailableLength);
        end;
        if Length(ReplacementString) > AvailableLength then
        begin
          raise ENotEnoughPrecision.Create(Format(StrNotEnoughSpace,
            [TemplateParameterName, OriginalLine, StartPosition, EndPos]));
        end
        else if not TryStrToFloat(Trim(ReplacementString), AValue) then
        begin
          raise EConvertError.Create(Format(StrConversionProblem,
            [ReplacementString, OriginalLine]));
        end;
        ALine := Copy(ALine, 1, Pred(StartPosition)) + ReplacementString
          + Copy(ALine, Succ(EndPos), MaxInt);
        StartPosition := Pos (FParameterDelimiter, ALine);
      end
      else
      begin
        raise EBadParamName.Create(Format(StrNoParameterNamed,
          [Trim(TemplateParameterName)]));
      end;
    end;
  end;
  StartPosition := Pos (FFormulaDelimiter, ALine);
  while StartPosition > 0 do
  begin
    EndPos := PosEx(FFormulaDelimiter, ALine, Succ(StartPosition));
    if EndPos = 0 then
    begin
      raise EUnmatchedDelimiter.Create(Format(StrUnmatchedDelimiter,
        ['formula', OriginalLine]));
    end;

    Formula := Trim(Copy(ALine, Succ(StartPosition), EndPos-StartPosition-1));
    OriginalFormula := Formula;
    StartFormula := PosEx(Formula, ALine, Succ(StartPosition));
    // The number should end where the formula begins.
    AvailableLength := StartFormula - StartPosition;
    try
      FParser.Compile(Formula);
      AValue := FParser.CurrentExpression.DoubleResult;
    except on E: Exception do
      begin
        raise ECompileError.Create(Format('Unable to evaluate the formula "%0:s" in "%1:s." The error message was "%:2s"',
          [OriginalFormula, OriginalLine, E.Message]));
      end;
    end;
    ReplacementString := MaxPrecisionFloatToStr(AValue, AvailableLength);
    ReplacementString := PadLeft(ReplacementString, AvailableLength);
    if Length(ReplacementString) > AvailableLength then
    begin
      raise ENotEnoughPrecision.Create(Format(StrNotEnoughSpace,
        [TemplateParameterName, OriginalLine, StartPosition, StartFormula]));
    end
    else if not TryStrToFloat(Trim(ReplacementString), AValue) then
    begin
      raise EConvertError.Create(Format(StrConversionProblem,
        [ReplacementString, OriginalLine]));
    end;
    ALine := Copy(ALine, 1, Pred(StartPosition)) + ReplacementString
      + Copy(ALine, Succ(EndPos), MaxInt);
    StartPosition := Pos (FFormulaDelimiter, ALine);
  end;
  FModelOutputFile.WriteLine(ALine);
end;

procedure TParameterProcessor.ReadAndProcessTemplateLines;
var
  ALine: string;
begin
  ReadTemplateHeader;
  while not FTemplateFile.EndOfStream do
  begin
    ALine := FTemplateFile.ReadLine;
    ProcessTemplateLine(ALine);
  end;

end;

procedure TParameterProcessor.ReadPValFile;
var
  ALine: string;
  ParameterCount: Integer;
  Splitter: TStringList;
  AParameter: TParameter;
  Value: Double;
begin
  if FPValFile <> nil then
  begin
    while Not FPValFile.EndOfStream do
    begin
      ALine := Trim(FPValFile.ReadLine);
      if (Aline = '') or (Aline[1] = '#') then
      begin
        Continue;
      end;
      if TryStrToInt(Aline, ParameterCount) then
      begin
        FParameters.Capacity := ParameterCount;
        break;
      end
      else
      begin
        raise EReadParamCountError.Create(StrUnableToReadTheN)
      end;
    end;

    Splitter := TStringList.Create;
    try
      Splitter.StrictDelimiter := False;
      while Not FPValFile.EndOfStream do
      begin
        ALine := Trim(FPValFile.ReadLine);
        if (Aline = '') or (Aline[1] = '#') then
        begin
          Continue;
        end;
        Splitter.DelimitedText := ALine;
        if Splitter.Count <> 2 then
        begin
          raise EReadParamError.Create(Format(StrInThePVALFile, [ALine]));
        end;
        AParameter.ParameterName := Splitter[0];
        if TryStrToFloat(Splitter[1], Value) then
        begin
          AParameter.ParameterValue := Value;
          FParameters.Add(UpperCase(AParameter.ParameterName), AParameter);
          if FParameters.Count = ParameterCount then
          begin
            break;
          end;
        end
        else
        begin
          raise EReadParamError.Create(Format(StrInThePVALFileNoConvert,
            [Splitter[1], ALine]));
        end;
      end;
    finally
      Splitter.Free;
    end;

    if FParameters.Count <> ParameterCount then
    begin
      raise EReadParamError.Create(Format(StrErrorReadingPVALF,
        [FParameters.Count, ParameterCount]));
    end;

  end;
end;

procedure TParameterProcessor.ReadTemplateHeader;
var
  ALine: string;
  ID: string;
  TemplateString: string;
  procedure ReadFormulaDelimiter;
  begin
    if (ID = 'etf ') then
    begin
      TemplateString := Trim(Copy(ALine,5, MAXINT));
      if Length(TemplateString) = 1 then
      begin
        FFormulaDelimiter := TemplateString[1];
      end
      else
      begin
        raise EBadDelimiter.Create(Format(StrErrorReadingThePa, ['formula']));
      end;
    end
    else
    begin
      raise EBadDelimiter.Create(StrErrorReadingTheFo);
    end;
  end;
begin
  ALine := FTemplateFile.ReadLine;
  ID := Copy(ALine, 1, 4);
  if (ID = 'ptf ') or (ID = 'jtf ') then
  begin
    TemplateString := Trim(Copy(ALine,5, MAXINT));
    if Length(TemplateString) = 1 then
    begin
      FParameterDelimiter := TemplateString[1];
      ALine := FTemplateFile.ReadLine;
      ID := Copy(ALine, 1, 4);
      ReadFormulaDelimiter;
    end
    else
    begin
      raise EBadDelimiter.Create(Format(StrErrorReadingThePa, ['parameter']));
    end;
  end
  else
  begin
    FParameterDelimiter := ' ';
    ReadFormulaDelimiter;
  end;
  if FParameterDelimiter = FFormulaDelimiter then
  begin
    raise EBadDelimiter.Create(Format(StrDuplicateDelimiters,
      [FParameterDelimiter]));
  end;
end;

end.
