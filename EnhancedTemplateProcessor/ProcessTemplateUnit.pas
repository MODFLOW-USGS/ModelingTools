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

  TNamedArray = record
    ArrayName: string;
    Values: array of array of array of double;
  end;

  TArrayDictionary = TDictionary<string, TNamedArray>;

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
  EReadArrayError = class(ETemplateError);

  { TParameterProcessor }

  TParameterProcessor = class(TObject)
  private
    FTemplateFileName: string;
    FPvalFileName: string;
    FArrayNamesFile: string;
{$IFDEF FPC}
  FTemplateFile: TSimpleStreamReader;
  FPValFile: TSimpleStreamReader;
  FArrayNameReader: TSimpleStreamReader;
  FModelOutputFile: TSimpleStreamWriter;
{$ELSE}
  FTemplateFile: TStreamReader;
  FPValFile: TStreamReader;
  FArrayNameReader: TStreamReader;
  FModelOutputFile: TStreamWriter;
{$ENDIF}
    FParameters: TParameterDictionary;
    FParser: TRbwParser;
    FParameterDelimiter: Char;
    FFormulaDelimiter: Char;
    FArrayDictionary: TArrayDictionary;
    procedure WriteUsage;
    procedure WriteParameters;
    procedure GetFileNames;
    procedure OpenFiles;
    procedure CloseFiles;
    Procedure ReadPValFile;
    procedure ReadAndProcessTemplateLines;
    procedure ReadTemplateHeader;
    procedure ProcessTemplateLine(ALine: string);
    procedure ReadArrayFiles;
    procedure ProcessArrayLine(ALine: string);
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
  rsWhileAttempt = 'While attempting to read an array from %0:s, no "%1:s" '
    +'character was found in the line "%2:s".';
  rsWhileAttempt2 = 'While attempting to read an array from %0:s, the "[" '
    +'character was after the "]" character in "%1:s".';
  rsWhileReading = 'While reading %0:s, no array name was found in "%1:s".';
  rsWhileReading2 = 'While reading %0:s, no file name was found in "%1:s".';
  rsWhileReading3 = 'While reading %0:s, the file name "%1:s" in "%1:s" does '
    +'not exist.';
  rsWhileReading4 = 'While reading the dimensions of the array "%0:s" in the '
    +'line "%1:s" of "%2:s", the number of dimensions was not equal to 3.';
  rsUnableToRead = 'Unable to read the number of %0:s in the array "%1:s" in '
    +'the line "%2:s" from the file "%3:s" because "%4:s" could not be '
    +'converted to an integer.';
  rsTheArrayName = 'The array named "%0:s" has been defined more than once in '
    +'"%:1s".';
  rsThereAreTooM = 'There are too many numbers for the array "%0:s" being read'
    +' from the file "1:s".';
  rsThereAreNotE = 'There are not enough numbers for the array "%0:s" being '
    +'read from the file "1:s".';

const
  ValidArrayNameCharacters = ('A'..'Z', 'a'..'z', '0'..'9', '_');

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
  WriteLn('  EnhancedTemplateProcessor <template file name> <PVAL file name> <Arrays file name>');
  WriteLn('A PVAL file is not required if formulas do not require parameter value substitution.');
  WriteLn('An Arrays file is not required if formulas do not require array value substitution.');
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
  FArrayNameReader.Free;
  FModelOutputFile.Free;
  FPValFile.Free;
  FTemplateFile.Free;
end;

constructor TParameterProcessor.Create;
begin
  FParser := TRbwParser.Create(nil);
  FParameters := TParameterDictionary.Create;
  FArrayDictionary := TArrayDictionary.Create;
  FormatSettings.DecimalSeparator := '.';
end;

destructor TParameterProcessor.Destroy;
begin
  FArrayDictionary.Free;
  FParameters.Free;
  FParser.Free;
  inherited;
end;

procedure TParameterProcessor.GetFileNames;
begin
  FTemplateFileName := ParamStr(1);
  FTemplateFileName := ExpandFileName(FTemplateFileName);
  if ParamCount >= 2 then
  begin
    FPvalFileName := ParamStr(2);
    FPvalFileName := ExpandFileName(FPvalFileName);
    if ParamCount = 3 then
    begin
      FArrayNamesFile := ParamStr(3);
      FArrayNamesFile := ExpandFileName(FArrayNamesFile);
    end
    else
    begin
      FArrayNamesFile := '';
    end;
  end
  else
  begin
    FPvalFileName := '';
    FArrayNamesFile := '';
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

  if FArrayNamesFile <> '' then
  begin
    {$IFDEF FPC}
    FArrayNameReader := TSimpleStreamReader.Create(FArrayNamesFile);
    {$ELSE}
    FArrayNameReader := TStreamReader.Create(FArrayNamesFile);
    {$ENDIF}
  end
  else
  begin
    FArrayNameReader := nil;
  end;
end;

procedure TParameterProcessor.ProcessTemplate;
begin
  if (ParamCount = 0) then
  begin
    WriteUsage;
    Exit;
  end;

  if (ParamCount > 3) then
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
  procedure SubstituteArrayValues(var ALine: string);
  var
    OpenBracePosition: Integer;
    CloseBracePosition: Integer;
  begin
    OpenBracePosition := Pos('[', ALine);
    While (OpenBracePostion > 0 do
    begin
      CloseBracePosition := Pos(']', ALine);



      OpenBracePosition := Pos('[', ALine);
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

procedure TParameterProcessor.ReadArrayFiles;
var
  ALine: String;
begin
  if FArrayNameReader <> nil then
  begin
    while not FArrayNameReader.EndOfStream do
    begin
      ALine := FArrayNameReader.ReadLine;
      ProcessArrayLine(ALine);
    end;
  end;
end;

procedure TParameterProcessor.ProcessArrayLine(ALine: string);
var
  OpenBracePosition: Integer;
  CloseBracePosition, LayerIndex, RowIndex, ColIndex, NumberIndex: Integer;
  ArrayName: string;
  FileName: string;
  Splitter: TStringList;
  LayerCount, RowCount, ColumnCount: Longint;
  NamedArray: TNamedArray;
  {$IFDEF FPC}
  ArrayFile: TSimpleStreamReader;
  {$ELSE}
  ArrayFile: TStreamReader;
  {$ENDIF}
  ANumber: Double;
begin
  if (ALine = '') or (ALine[1] = '#') then
  begin
    Exit;
  end;
  // The line is defines an array as follows
  // ArrayName[LayerCount, RowCount, ColumnCount] FileName
  // If the file name has any white space, it must be enclosed in double quotes.
  // Example:
  // HK[3, 9, 12]  "HK Array.txt"
  OpenBracePosition := Pos('[', ALine);
  if OpenBracePosition < 2 then
  begin
    raise EReadArrayError.Create(Format(rsWhileAttempt,
      [FArrayNamesFile, '[', ALine]));
  end;
  CloseBracePosition := Pos(']', ALine);
  if CloseBracePosition < 3 then
  begin
    raise EReadArrayError.Create(Format(rsWhileAttempt,
      [FArrayNamesFile, ']', ALine]));
  end;
  if OpenBracePosition > CloseBracePosition then
  begin
    raise EReadArrayError.Create(Format(rsWhileAttempt2,
      [FArrayNamesFile, ALine]));
  end;
  ArrayName := Trim(Copy(ALine, OpenBracePosition -1));
  if ArrayName = '' then
  begin
    raise EReadArrayError.Create(Format(rsWhileReading ,
      [FArrayNamesFile, ALine]));
  end;
  if FArrayDictionary.ContainsKey(UpperCase(ArrayName)) then
  begin
    raise EReadArrayError.Create(Format(rsTheArrayName ,
      [ArrayName, FArrayNamesFile]));
  end;
  FileName := Trim(Copy(ALine, CloseBracePosition+1, MAXINT));
  if FileName = '' then
  begin
    raise EReadArrayError.Create(Format(rsWhileReading2,
      [FArrayNamesFile, ALine]));
  end;
  if not FileExists(FileName) then
  begin
    raise EReadArrayError.Create(Format(rsWhileReading3,
      [FArrayNamesFile, FileName, ALine]));
  end;
  Splitter := TStringList.Create;
  try
    Splitter.DelimitedText := Copy(ALine, OpenBracePosition+1,
      CloseBracePosition-OpenBracePosition -1);
    if Splitter.Count <> 3 then
    begin
      raise EReadArrayError.Create(Format(rsWhileReading4,
        [ArrayName, ALine, FArrayNamesFile]));
    end;
    if not TryStrToInt(Splitter[0], LayerCount) then
    begin
      raise EReadArrayError.Create(Format(rsUnableToRead,
        ['layers', ArrayName, ALine, FArrayNamesFile, Splitter[0]]));
    end;
    if not TryStrToInt(Splitter[1], RowCount) then
    begin
      raise EReadArrayError.Create(Format(rsUnableToRead,
        ['rows', ArrayName, ALine, FArrayNamesFile, Splitter[1]]));
    end;
    if not TryStrToInt(Splitter[2], ColumnCount) then
    begin
      raise EReadArrayError.Create(Format(rsUnableToRead,
        ['columns', ArrayName, ALine, FArrayNamesFile, Splitter[2]]));
    end;
    NamedArray.ArrayName:= ArrayName;
    SetLength(NamedArray.Values, LayerCount, RowCount, ColumnCount);

    {$IFDEF FPC}
    ArrayFile := TSimpleStreamReader.Create(FileName);
    {$ELSE}
    ArrayFile := TStreamReader.Create(FileName);
    {$ENDIF}
    try
      LayerIndex := 0;
      RowIndex := 0;
      ColIndex := 0;
      while not ArrayFile.EndOfStream do
      begin
        Splitter.DelimitedText := ArrayFile.ReadLine;
        for NumberIndex := 0 to Pred(Splitter.Count) do
        begin
          if not TryStrToFloat(Splitter[NumberIndex], ANumber) then
          begin
            if LayerIndex >= LayerCount then
            begin
              raise EReadArrayError.Create(Format(rsThereAreTooM,
                [ArrayName, FileName]));
            end;
            NamedArray.Values[LayerIndex, RowIndex, ColIndex] := ANumber;
            Inc(ColIndex);
            if ColIndex = ColumnCount then
            begin
              ColIndex := 0;
              Inc(RowIndex);
              if RowIndex = RowCount then
              begin
                RowIndex := 0;
                Inc(LayerIndex);
              end;
            end;
          end;
        end;
      end;
      if LayerIndex <> LayerCount then
      begin
        raise EReadArrayError.Create(Format(rsThereAreNotE,
          [ArrayName, FileName]));
      end;
    finally
      ArrayFile.Free;
    end;

    FArrayDictionary.Add(UpperCase(NamedArray.ArrayName), NamedArray);
  finally
    Splitter.Free;
  end;
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
  ValueString: String;
  DPos: Integer;
  PestParam: Boolean;
  MF2005ParamCount: Integer;
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
      MF2005ParamCount := 0;
      while Not FPValFile.EndOfStream do
      begin
        PestParam := False;
        ALine := Trim(FPValFile.ReadLine);
        if (Aline = '') then
        begin
          Continue;
        end;
        if (Aline[1] = '#') then
        begin
          if Copy(ALine, 1, 3) = '#--' then
          begin
            PestParam := True;
            ALine := Trim(Copy(ALine, 4, MAXINT));
          end
          else
          begin
            Continue;
          end;
        end;

        Splitter.DelimitedText := ALine;
        if Splitter.Count <> 2 then
        begin
          raise EReadParamError.Create(Format(StrInThePVALFile, [ALine]));
        end;
        AParameter.ParameterName := Splitter[0];
        ValueString := Splitter[1];
        DPos := Pos('D', ValueString);
        if DPos >= 1 then
        begin
          ValueString[DPos] := 'E';
        end;
        DPos := Pos('d', ValueString);
        if DPos >= 1 then
        begin
          ValueString[DPos] := 'e';
        end;
        if TryStrToFloat(ValueString, Value) then
        begin
          AParameter.ParameterValue := Value;
          if (MF2005ParamCount < ParameterCount) or PestParam then
          begin
            FParameters.Add(UpperCase(AParameter.ParameterName), AParameter);
          end;
          if not PestParam then
          begin
            Inc(MF2005ParamCount);
          end;
          //if FParameters.Count = ParameterCount then
          //begin
          //  break;
          //end;
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

    if MF2005ParamCount <> ParameterCount then
    begin
      raise EReadParamError.Create(Format(StrErrorReadingPVALF,
        [MF2005ParamCount, ParameterCount]));
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
