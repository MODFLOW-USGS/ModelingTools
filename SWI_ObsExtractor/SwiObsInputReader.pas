unit SwiObsInputReader;

{$IFDEF FPC}
  {$mode Delphi}{$H+}
{$ENDIF}  

{#BACKUP SimpleTextWriter.pas}
{#BACKUP SwiObsExtractor.lpi}
{#BACKUP SwiObsExtractor.lpr}

interface

uses
  Classes, 
  SysUtils, 
  Generics.Collections, 
{$IFDEF FPC}
  SimpleTextWriter,
  streamex,
{$ENDIF}  
  SwiObsReaderUnit;

//procedure ExtractSwiObservations;

type
  TInstructionFileFormat = (iffUcode, IffPest);
  TDataToWrite = (dtwObservedValues, dtwInstructions);
  TDataToWriteSet = set of TDataToWrite;

  TSwiBinaryObsID = class(TObject)
    Fraction: double;
    ObsNumber: integer;
  end;

  TSwiBinaryObsIDObjList = TObjectList<TSwiBinaryObsID>;

  TSwiAsciiObsID = class(TSwiBinaryObsID)
    Name: string;
  end;

  TSwiAsciiObsIDObjList = TObjectList<TSwiAsciiObsID>;

  { TInterpolatedSwiObs }

  TInterpolatedSwiObs = class(TObject)
    Name: string;
    Time: double;
    ZetaSurface: Integer;
    ObservedValue: double;
    FAsciiObs: TSwiAsciiObsIDObjList;
    FBinaryObs: TSwiBinaryObsIDObjList;
    constructor Create;
    destructor Destroy; override;
    function InterpolatedValue(SwiObs: TSwiObs; ZoneCount: Integer): double;
  end;

  TInterpolatedSwiObsObjList = TObjectList<TInterpolatedSwiObs>;

  { TSwiObservationExtractor }

  TSwiObservationExtractor = class(TObject)
  private
    const
      KVersion = '2.0';
    var
    FOutputFileName: string;
    FSwiObsFileName: string;
    FSwiFileFormat: TSwiFileFormat;
    FZoneCount: integer;
    FFileOptionsRead: Boolean;
    FObs: TInterpolatedSwiObsObjList;
    FCurrentObs: TInterpolatedSwiObs;
    FObservationCount: integer;
    FSwiObs: TSwiObs;
    FLinesToSkip: integer;
    FInstructionFileFormat: TInstructionFileFormat;
    FDataToWriteSet: TDataToWriteSet;
  {$IFDEF FPC}
    FOutputFile: TSimpleTextWriter;
    FInstructionFile: TSimpleTextWriter;
  {$ELSE}
    FOutputFile: TStreamWriter;
    FInstructionFile: TStreamWriter;
  {$ENDIF}
    FOldDecimalSeparator: Char;
    procedure ReadInputOptions(Input: TStringList);
    procedure ReadFileOptions(Input: TStringList; var LineIndex: integer);
    procedure ReadObservationIDs(Input: TStringList; var LineIndex: integer);
    procedure ReadAsciiObservationIDs(InputLine: TStringList);
    procedure ReadBinaryObservationIDs(InputLine: TStringList);
    function ShouldSkipLine(const ALine: string): Boolean;
    procedure ReadSwiAsciiObservations;
    procedure ReadSwiBinaryObservations;
    procedure WriteLine; overload;
    procedure WriteLine(Value: Integer); overload;
//    procedure WriteLine(Value: double); overload;
    procedure WriteLine(Value: string); overload;
    procedure Write(Value: string); overload;
    procedure Write(Value: double); overload;
    procedure Write(Value: integer); overload;
    procedure WriteInterpolatedValuesAndTemplate;
    procedure WriteObsDefinition;
    procedure WriteInstructionHeader;
    procedure WriteInstruction(AnObs: TInterpolatedSwiObs);
  public
    function Version: string;
    constructor Create(InputFile: string);
    destructor Destroy; override;
  end;

implementation

uses
{$IFNDEF FPC}
  System.IOUtils, 
  System.StrUtils, 
{$ENDIF}

  SwiObsUtilities, 
  InterpolatedObsResourceUnit,
  DisclaimerTextUnit;

resourcestring
  StrIfTheSWIObserivat = 'If the SWI Obserivation file is binary, you must s' +
  'pecify whether the precision is "single" or "double".';
  StrUnknownSWIObservat = 'Unknown SWI Observation file format.';
  StrForBinarySWIObser = 'For binary SWI Observation files, you must specify' +
  ' the number of zones.';
  StrErrorReadingNumber = 'Error reading number of zones';
  StrUnknownSWIObsPrecison = 'Unknown SWI Observation file precision';
  StrUnrecognizedTagIn = 'Unrecognized tag in file options: %s';
  StrTheFileOptionsMus = 'The file options must be specified before specifyi' +
  'ng the observations';
  StrForBinaryFilesObsCount = 'For binary files, you must specify the total ' +
  'number of observations.';
  //StrZetaError = 'Error convertng "%0:s" to an integer in the observation wi' +
  //'th the name "%1:s"';
  StrErrorConvertingtoNumber = 'Error converting "%s" to a number';
  //StrErrorConvertngFraction = 'Error convertng "%0:s" to an real number in t' +
  //'he observation with the name "%1:s"';
  StrUnrecognizedObsTab = 'Unrecognized tag in observations: "%S';
  StrSWIASCIIObservatio = 'SWI ASCII observations must include the observati' +
  'on name, zeta surface and Fraction. This was not found in %s';
  StrEachObservationMus = 'Each observation must start with "BEGIN_OBSERVATI' +
  'ON".';
  StrANameMustBeSpeci = 'A name must be specified for each observations';
  StrTheTimeForTheObs = 'The time for the observation named %s must be great' +
  'er then or equal to zero';
  StrErrorConvertngBinConvert = 'Error convertng "%0:s" to an integer in an ' +
  'SWI observation definition';
  StrErrorConvertngBinReal = 'Error convertng "%0:s" to an real number';
  StrErrorReadingTotal = 'Error reading total number of observations';
  StrTheNameOfTheFile = 'The name of the file from which observations will b' +
  'e read must always be specified even if interpolated values will not be c' +
  'alculated.';
  StrErrorReadingTheVa = 'Error reading the value for DATA_TO_WRITE. The val' +
  'ue must be VALUES or INSTRUCTIONS.';
  StrErrorReadingInstru = 'Error reading instruction file format. The format' +
  ' must be UCODE or PEST.';
  StrTheInstructionFileUcode = 'The instruction file will be written in the ' +
  'format required by UCODE.';
  StrTheInstructionFilePest = 'The instruction file will be written in the f' +
  'ormat required by PEST.';


{ TSwiObservationExtractor }

constructor TSwiObservationExtractor.Create(InputFile: string);
var
  AStringList: TStringList;
begin
  FOldDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';

  FLinesToSkip := 0;
  FObs := TInterpolatedSwiObsObjList.Create;
  FOutputFile := nil;
  try
    FSwiObs := TSwiObs.Create;
    Assert(FileExists(InputFile));
    AStringList := TStringList.Create;
    try
      AStringList.LoadFromFile(InputFile);
      ReadInputOptions(AStringList);
    finally
      AStringList.Free;
    end;

    if dtwObservedValues in FDataToWriteSet then
    begin
      case FSwiFileFormat of
        sffAscii: ReadSwiAsciiObservations;
        sffBinarySingle, sffBinaryDouble: ReadSwiBinaryObservations;
        else Assert(False);
      end;
    end;

    WriteInterpolatedValuesAndTemplate;
  except on E: Exception do
    begin
      if FOutputFile <> nil then
      begin
        FOutputFile.WriteLine(E.Message);
      end;

      raise
    end;
  end;
end;

destructor TSwiObservationExtractor.Destroy;
begin
  FSwiObs.Free;
  FOutputFile.Free;
  FObs.Free;
  FormatSettings.DecimalSeparator := FOldDecimalSeparator;

  inherited Destroy;
end;

procedure TSwiObservationExtractor.WriteInstruction(AnObs: TInterpolatedSwiObs);
begin
  case FInstructionFileFormat of
    iffUcode:
      begin
        FInstructionFile.WriteLine(AnObs.Name);
      end;
    IffPest:
      begin
        FInstructionFile.WriteLine(Format('l1 !%s!', [AnObs.Name]));
      end;
    else
      Assert(False);
  end;
end;

procedure TSwiObservationExtractor.WriteInstructionHeader;
begin
  case FInstructionFileFormat of
    iffUcode:
      begin
        FInstructionFile.WriteLine('jif @');
        FInstructionFile.Write('StandardFile ');
        FInstructionFile.Write(FLinesToSkip);
        FInstructionFile.Write(' 1 ');
        FInstructionFile.WriteLine(FObs.Count);
      end;
    IffPest:
      begin
        FInstructionFile.WriteLine('pif @');
        FInstructionFile.WriteLine(Format('l%d', [FLinesToSkip]));
      end;
    else
      Assert(False);
  end;
end;

procedure TSwiObservationExtractor.WriteObsDefinition;
var
  SWI_Index: Integer;
  AscObs: TSwiAsciiObsID;
  BinObs: TSwiBinaryObsID;
begin
  Write('"');
  Write(FCurrentObs.Name);
  Write('"');
  Write(FCurrentObs.Time);
  Write(FCurrentObs.ZetaSurface);
  Write(FCurrentObs.ObservedValue);
  WriteLine(' # Name, Time, Zeta Surface, Observed Value');
  for SWI_Index := 0 to FCurrentObs.FAsciiObs.Count - 1 do
  begin
    AscObs := FCurrentObs.FAsciiObs[SWI_Index];
    Write('  ');
    Write(AscObs.ObsNumber);
    Write(AscObs.Fraction);
    Write(' ');
    Write(AscObs.Name);
    WriteLine(' # Observation Number, Fraction, Name');
  end;
  for SWI_Index := 0 to FCurrentObs.FBinaryObs.Count - 1 do
  begin
    BinObs := FCurrentObs.FBinaryObs[SWI_Index];
    Write('  ');
    Write(BinObs.ObsNumber);
    Write(BinObs.Fraction);
    WriteLine(' # Observation Number, Fraction');
  end;
  WriteLine;
end;

procedure TSwiObservationExtractor.ReadSwiAsciiObservations;
var
  SwiObsFile: TStreamReader;
begin
  Assert(FSwiObsFileName <> '');
{$IFDEF FPC}
  SwiObsFile := TSimpleStreamReader.Create(FSwiObsFileName);
{$ELSE}
  SwiObsFile := TFile.OpenText(FSwiObsFileName);
{$ENDIF}
  try
    FSwiObs.ReadAsciiSwiObs(SwiObsFile);
  finally
    SwiObsFile.Free;
  end;
end;

procedure TSwiObservationExtractor.ReadSwiBinaryObservations;
var
  SwiObsFile: TFileStream;
begin
  Assert(FSwiObsFileName <> '');
{$IFDEF FPC}
  SwiObsFile := TFileStream.Create(FSwiObsFileName, fmOpenRead or fmShareDenyWrite);
{$ELSE}
  SwiObsFile := TFile.OpenRead(FSwiObsFileName);
{$ENDIF}
  try
    FSwiObs.ReadBinarySwiObs(SwiObsFile, FZoneCount*FObservationCount,
      FSwiFileFormat);
  finally
    SwiObsFile.Free;
  end;
end;

procedure TSwiObservationExtractor.ReadAsciiObservationIDs(
  InputLine: TStringList);
var
  Tag: string;
  AValue: extended;
  ObsName: string;
  Fraction: double;
//  Zeta: integer;
  AnObs: TSwiAsciiObsID;
  ObsNumber: integer;
  ObsZeta: Integer;
begin
  if InputLine.Count > 0 then
  begin
    Tag := UpperCase(InputLine[0]);
    if Tag = StrBEGINOBSERVATION then
    begin
      FCurrentObs := TInterpolatedSwiObs.Create;
      FObs.Add(FCurrentObs);
    end
    else
    begin
      Assert(FCurrentObs <> nil, StrEachObservationMus);
      if Tag = StrENDOBSERVATION then
      begin
        Assert(FCurrentObs.Name <> '', StrANameMustBeSpeci);
        Assert(FCurrentObs.Time >= 0, Format(StrTheTimeForTheObs, [FCurrentObs.Name]));
        WriteObsDefinition;

        FCurrentObs := nil;
      end
      else if Tag = StrZetaNAME then
      begin
        FCurrentObs.Name := InputLine[1];
      end
      else if Tag = StrZetaTIME then
      begin
        if TryFortranStrToFloat(InputLine[1], AValue) then
        begin
          FCurrentObs.Time := AValue;
        end
        else
        begin
          Assert(False, Format(StrErrorConvertingtoNumber, [InputLine[1]]));
        end;
      end
      else if Tag = StrZETASURFACENUMBER then
      begin
        if TryStrToInt(InputLine[1], ObsZeta) then
        begin
          FCurrentObs.ZetaSurface := ObsZeta;
        end
        else
        begin
          Assert(False, Format(StrErrorConvertingtoNumber, [InputLine[1]]));
        end;
      end
      else if Tag = StrOBSERVEDVALUE then
      begin
        if TryFortranStrToFloat(InputLine[1], AValue) then
        begin
          FCurrentObs.ObservedValue := AValue;
        end
        else
        begin
          Assert(False, Format(StrErrorConvertingtoNumber, [InputLine[1]]));
        end;
      end
      else if Tag = StrSWIOBSERVATION then
      begin
        if InputLine.Count >= 3 then
        begin
          Fraction := 0;
          if not TryStrToInt(InputLine[1], ObsNumber) then
          begin
            Assert(False, Format(StrErrorConvertngBinConvert, [InputLine[1]]));
          end;

          if TryFortranStrToFloat(InputLine[2], AValue)  then
          begin
            Fraction := AValue;
          end
          else
          begin
            Assert(False, Format(StrErrorConvertngBinReal, [InputLine[2]]));
          end;

          if InputLine.Count >= 4 then
          begin
            ObsName := InputLine[3];
          end
          else
          begin
            ObsName:= '';
          end;
          AnObs := TSwiAsciiObsID.Create;
          FCurrentObs.FAsciiObs.Add(AnObs);
          AnObs.Name := ObsName;
//          AnObs.ZetaSurface := Zeta;
          AnObs.Fraction := Fraction;
          AnObs.ObsNumber := ObsNumber;
        end
        else
        begin
          Assert(False, Format(StrSWIASCIIObservatio, [InputLine.Text]));
        end;
      end
      else
      begin
        Assert(False, Format(StrUnrecognizedObsTab, [InputLine[0]]));
      end;
    end;
  end;
end;

procedure TSwiObservationExtractor.ReadBinaryObservationIDs(
  InputLine: TStringList);
var
  Tag: string;
  AValue: extended;
  Fraction: double;
  AnObs: TSwiBinaryObsID;
  ObsNumber: integer;
  ObsZeta: Integer;
begin
  if InputLine.Count > 0 then
  begin
    Tag := UpperCase(InputLine[0]);
    if Tag = StrBEGINOBSERVATION then
    begin
      FCurrentObs := TInterpolatedSwiObs.Create;
      FObs.Add(FCurrentObs);
    end
    else
    begin
      Assert(FCurrentObs <> nil, StrEachObservationMus);
      if Tag = StrENDOBSERVATION then
      begin
        Assert(FCurrentObs.Name <> '', StrANameMustBeSpeci);
        Assert(FCurrentObs.Time >= 0, Format(StrTheTimeForTheObs, [FCurrentObs.Name]));

        WriteObsDefinition;

        FCurrentObs := nil;
      end
      else if Tag = StrZetaNAME then
      begin
        FCurrentObs.Name := InputLine[1];
      end
      else if Tag = StrZetaTIME then
      begin
        if TryFortranStrToFloat(InputLine[1], AValue) then
        begin
          FCurrentObs.Time := AValue;
        end
        else
        begin
          Assert(False, Format(StrErrorConvertingtoNumber, [InputLine[1]]));
        end;
      end
      else if Tag = StrZETASURFACENUMBER then
      begin
        if TryStrToInt(InputLine[1], ObsZeta) then
        begin
          FCurrentObs.ZetaSurface := ObsZeta;
        end
        else
        begin
          Assert(False, Format(StrErrorConvertingtoNumber, [InputLine[1]]));
        end;
      end
      else if Tag = StrOBSERVEDVALUE then
      begin
        if TryFortranStrToFloat(InputLine[1], AValue) then
        begin
          FCurrentObs.ObservedValue := AValue;
        end
        else
        begin
          Assert(False, Format(StrErrorConvertingtoNumber, [InputLine[1]]));
        end;
      end
      else if Tag = StrSWIOBSERVATION then
      begin
        if InputLine.Count > 3 then
        begin
          Fraction := 0;
          if not TryStrToInt(InputLine[1], ObsNumber) then
          begin
            Assert(False, Format(StrErrorConvertngBinConvert, [InputLine[1]]));
          end;
//          if not TryStrToInt(InputLine[2], Zeta) then
//          begin
//            Assert( Format(StrErrorConvertngBinConvert, [InputLine[2]]));
//          end;
          if TryFortranStrToFloat(InputLine[2], AValue)  then
          begin
            Fraction := AValue;
          end
          else
          begin
            Assert(False, Format(StrErrorConvertngBinReal, [InputLine[2]]));
          end;
          AnObs := TSwiBinaryObsID.Create;
          FCurrentObs.FBinaryObs.Add(AnObs);
          AnObs.ObsNumber := ObsNumber;
//          AnObs.ZetaSurface := Zeta;
          AnObs.Fraction := Fraction;
        end
        else
        begin
          Assert(False, Format(StrSWIASCIIObservatio, [InputLine.Text]));
        end;
      end
      else
      begin
        Assert(False, Format(StrUnrecognizedObsTab, [InputLine[0]]));
      end;
    end;
  end;
end;

procedure TSwiObservationExtractor.ReadFileOptions(Input: TStringList;
  var LineIndex: integer);
var
  Splitter: TStringList;
  ALine: string;
  Tag: string;
  SwiFileFormat: string;
  SwiPrecision: string;
  DisclaimerIndex: Integer;
begin
  FInstructionFileFormat := iffUcode;
  FDataToWriteSet := [];
  FZoneCount := 0;
  FObservationCount := 0;
  FFileOptionsRead := True;
  Splitter := TStringList.Create;
  try
    Splitter.Delimiter := ' ';
    repeat
      Inc(LineIndex);
      ALine := Input[LineIndex];

      // Skip comments and blank lines
      if ShouldSkipLine(ALine) then
      begin
        Continue;
      end;

      if Trim(UpperCase(ALine)) = StrENDFILEOPTIONS then
      begin
        Exit;
      end;

      Splitter.DelimitedText := Trim(ALine);
      if Splitter.Count > 1 then
      begin
        Tag := UpperCase(Splitter[0]);
        if Tag = StrOUTPUTFILE then
        begin
          FOutputFileName := Splitter[1];
          {$IFDEF FPC}
          FOutputFile := TSimpleTextWriter.Create(FOutputFileName);
          {$ELSE}
          FOutputFile := TFile.CreateText(FOutputFileName);
          {$ENDIF}
          WriteLine('SWI Observation Interpolator Version ' + Version);

          WriteLine;
          for DisclaimerIndex := 0 to Disclaimer.Count - 1 do
          begin
            WriteLine(Disclaimer[DisclaimerIndex]);
          end;
          WriteLine;

          WriteLine('INPUT OPTIONS');
          Write('Output File: ');
          WriteLine(FOutputFileName);
        end
        else if Tag = StrSWIOBSFILE then
        begin
          FSwiObsFileName := Splitter[1];
          Write('SWI Observation File: ');
          WriteLine(FSwiObsFileName);
        end
        else if Tag = StrSWIOBSFORMAT then
        begin
          SwiFileFormat := UpperCase(Splitter[1]);
          Write('SWI Observation File Format: ');
          if SwiFileFormat = StrASCII then
          begin
            FSwiFileFormat := sffAscii;
            WriteLine('ASCII');
          end
          else if SwiFileFormat = StrBINARY then
          begin
            Assert(Splitter.Count > 2, StrIfTheSWIObserivat);
            SwiPrecision := UpperCase(Splitter[2]);
            Write('BINARY ');
            if SwiPrecision = StrSINGLE then
            begin
              FSwiFileFormat := sffBinarySingle;
              WriteLine('single precision');
            end
            else if SwiPrecision = StrDOUBLE then
            begin
              FSwiFileFormat := sffBinaryDouble;
              WriteLine('double precision');
            end
            else
            begin
              Assert(False, StrUnknownSWIObsPrecison);
            end;
          end
          else
          begin
            Assert(False, StrUnknownSWIObservat);
          end;
        end
        else if Tag = StrTOTALNUMBEROFOBSE then
        begin
          if not TryStrToInt(Splitter[1], FObservationCount) then
          begin
            Assert(False, StrErrorReadingTotal);
          end;
          Write('Total number of observations: ');
          WriteLine(FObservationCount);
        end
        else if Tag = StrNUMBEROFZETASURFA then
        begin
          if not TryStrToInt(Splitter[1], FZoneCount) then
          begin
            Assert(False, StrErrorReadingNumber);
          end;
          Write('Total number of zeta surfaces: ');
          WriteLine(FZoneCount);
        end
        else if Tag = 'INSTRUCTION_FILE_FORMAT' then
        begin
          if UpperCase(Splitter[1]) = 'UCODE' then
          begin
            FInstructionFileFormat := iffUcode;
            WriteLine(StrTheInstructionFileUcode);
          end
          else if UpperCase(Splitter[1]) = 'PEST' then
          begin
            FInstructionFileFormat := IffPest;
            WriteLine(StrTheInstructionFilePest);
          end
          else
          begin
            Assert(False, StrErrorReadingInstru);
          end;
        end
        else if Tag = 'DATA_TO_WRITE' then
        begin
          if UpperCase(Splitter[1]) = 'VALUES' then
          begin
            Include(FDataToWriteSet, dtwObservedValues);
            WriteLine('Interpolated values will be written to the output file');
          end
          else if UpperCase(Splitter[1]) = 'INSTRUCTIONS' then
          begin
            Include(FDataToWriteSet, dtwInstructions);
            WriteLine('An instruction file will be written');
          end
          else
          begin
            Assert(False, StrErrorReadingTheVa);
          end;
        end
        else
        begin
          Assert(False, Format(StrUnrecognizedTagIn, [Splitter[0]]));
        end;
      end;
    until (False);
  finally
    if FDataToWriteSet = [] then
    begin
      FDataToWriteSet := [dtwObservedValues, dtwInstructions];
      WriteLine('Interpolated values will be written to the output file');
      WriteLine('An instruction file will be written');
    end;
    Splitter.Free;
  end;
  if FSwiFileFormat <> sffAscii then
  begin
    Assert(FZoneCount > 0, StrForBinarySWIObser);
    Assert(FObservationCount > 0, StrForBinaryFilesObsCount );
  end;
  Assert(FSwiObsFileName <> '', StrTheNameOfTheFile);
  WriteLine;
end;

procedure TSwiObservationExtractor.ReadInputOptions(Input: TStringList);
var
  LineIndex: Integer;
  ALine: string;
begin
  LineIndex := -1;
  While LineIndex < Input.Count-1 do
  begin
    Inc(LineIndex);
    ALine := Input[LineIndex];

    // Skip comments
    if ShouldSkipLine(ALine) then
    begin
      Continue;
    end;

    if Trim(UpperCase(ALine)) = StrBEGINFILEOPTIONS then
    begin
      ReadFileOptions(Input, LineIndex);
      Continue;
    end;

    if Trim(UpperCase(ALine)) = StrBEGINOBSERVATIONS then
    begin
      ReadObservationIDs(Input, LineIndex);
    end;
  end;
end;

procedure TSwiObservationExtractor.ReadObservationIDs(Input: TStringList;
  var LineIndex: integer);
var
  Splitter: TStringList;
  ALine: string;
begin
  Write('OBSERVATION DEFINITIONS');
  WriteLine;
  Assert(FFileOptionsRead, StrTheFileOptionsMus);
  Splitter := TStringList.Create;
  try
    Splitter.Delimiter := ' ';
    while LineIndex < Input.Count do
    begin
      Inc(LineIndex);
      ALine := Input[LineIndex];
      if Trim(UpperCase(ALine)) = StrENDOBSERVATIONS then
      begin
        Exit;
      end;

      // Skip comments and blank lines
      if ShouldSkipLine(ALine) then
      begin
        Continue;
      end;

      Splitter.DelimitedText := ALine;

      case FSwiFileFormat of
        sffAscii: ReadAsciiObservationIDs(Splitter);
        sffBinarySingle, sffBinaryDouble: ReadBinaryObservationIDs(Splitter);
        else Assert(False);
      end;
    end;
  finally
    Splitter.Free;
  end;
end;

function TSwiObservationExtractor.ShouldSkipLine(const ALine: string): Boolean;
begin
  result := (Trim(ALine) = '') or (ALine[1] = '#');
end;

function TSwiObservationExtractor.Version: string;
begin
  result := KVersion;
end;

procedure TSwiObservationExtractor.Write(Value: string);
begin
  if FOutputFile <> nil then
  begin
    FOutputFile.Write(Value);
  end;
end;

procedure TSwiObservationExtractor.Write(Value: double);
begin
  if FOutputFile <> nil then
  begin
    FOutputFile.Write(' ');
    FOutputFile.Write(Value);
  end;
end;

procedure TSwiObservationExtractor.WriteLine(Value: string);
begin
  if FOutputFile <> nil then
  begin
{$IFDEF FPC}
    FOutputFile.Write(Value);
    WriteLine;
{$ELSE}
    FOutputFile.WriteLine(Value);
    Inc(FLinesToSkip);
{$ENDIF}
  end;
end;

procedure TSwiObservationExtractor.WriteLine(Value: Integer);
begin
  if FOutputFile <> nil then
  begin
    FOutputFile.Write(' ');
{$IFDEF FPC}
    FOutputFile.Write(Value);
    WriteLine;
{$ELSE}
    FOutputFile.WriteLine(Value);
    Inc(FLinesToSkip);
{$ENDIF}
  end;
end;

procedure TSwiObservationExtractor.WriteLine;
begin
  if FOutputFile <> nil then
  begin
{$IFDEF FPC}
    FOutputFile.Write(LineEnding);
{$ELSE}
    FOutputFile.WriteLine;
{$ENDIF}
    Inc(FLinesToSkip);
  end;
end;

procedure TSwiObservationExtractor.Write(Value: integer);
begin
  if FOutputFile <> nil then
  begin
    FOutputFile.Write(' ');
    FOutputFile.Write(Value);
  end;
end;

procedure TSwiObservationExtractor.WriteInterpolatedValuesAndTemplate;
var
  ObsIndex: Integer;
  AnObs: TInterpolatedSwiObs;
  InstructionFileName: string;
begin
  if dtwInstructions in FDataToWriteSet then
  begin
    case FInstructionFileFormat of
      iffUcode: InstructionFileName := FOutputFileName + '.jif';
      IffPest: InstructionFileName := FOutputFileName + '.ins';
      else Assert(False, 'Programming error: unspecified instruction file format.');
    end;
  {$IFDEF FPC}
    FInstructionFile := TSimpleTextWriter.Create(InstructionFileName);
  {$ELSE}
    FInstructionFile := TFile.CreateText(InstructionFileName);
  {$ENDIF}
  end
  else
  begin
    FInstructionFile := nil;
  end;
  try
    WriteLine('OBSERVATIONS');
    WriteLine('Simulated Value, Observed Value, Name');

    if dtwInstructions in FDataToWriteSet then
    begin
      WriteInstructionHeader;
    end;

    for ObsIndex := 0 to FObs.Count - 1 do
    begin
      AnObs := FObs[ObsIndex];
      if dtwObservedValues in FDataToWriteSet then
      begin
        Write(AnObs.InterpolatedValue(FSwiObs, FZoneCount));
        Write(AnObs.ObservedValue);
        Write(' "');
        Write(AnObs.Name);
        WriteLine('"');
      end;

      if dtwInstructions in FDataToWriteSet then
      begin
        WriteInstruction(AnObs);
      end;
    end;
  finally
    FInstructionFile.Free;
  end;
end;

//procedure TSwiObservationExtractor.WriteLine(Value: double);
//begin
//  if FOutputFile <> nil then
//  begin
//    FOutputFile.Write(' ');
//    FOutputFile.WriteLine(Value);
//    Inc(FLinesToSkip);
//  end;
//end;

{ TInterpolatedSwiObs }

constructor TInterpolatedSwiObs.Create;
begin
  inherited;
  FAsciiObs := TSwiAsciiObsIDObjList.Create;
  FBinaryObs := TSwiBinaryObsIDObjList.Create;
end;

destructor TInterpolatedSwiObs.Destroy;
begin
  FBinaryObs.Free;
  FAsciiObs.Free;
  inherited Destroy;
end;

function TInterpolatedSwiObs.InterpolatedValue(SwiObs: TSwiObs;
  ZoneCount: Integer): double;
var
  ObsIndex: Integer;
  AnSwiObs: TSwiAsciiObsID;
  ABinaryObs: TSwiBinaryObsID;
  TimeIndex: integer;
  BeforeTimeIndex: Integer;
  SwiTime: Double;
  AfterTimeIndex: Integer;
  DeltaT: Double;
  ObsName: string;
  ObsZeta: Integer;
  TotalFraction: double;
  BeforeValue: double;
  AfterValue: double;
  Fraction: Double;
  ObsPosition: integer;
const
  Epsilon = 0.01;
  procedure HandleSwiObs(AnObs: TSwiBinaryObsID);
  var
    ObsIndex: integer;
    AValue: Double;
  begin
    TotalFraction := TotalFraction + AnObs.Fraction;
    ObsIndex := (AnObs.ObsNumber-1)*ZoneCount + ZetaSurface -1;
    AValue := SwiObs.ObservationValue[BeforeTimeIndex, ObsIndex];
    BeforeValue := BeforeValue + AValue*AnObs.Fraction;
    if BeforeTimeIndex <> AfterTimeIndex then
    begin
      AValue := SwiObs.ObservationValue[AfterTimeIndex, ObsIndex];
      AfterValue := AfterValue + AValue*AnObs.Fraction;
    end;
  end;
begin
  Assert((FAsciiObs.Count = 0) or (FBinaryObs.Count = 0));
  Assert((FAsciiObs.Count > 0) or (FBinaryObs.Count > 0));
  Assert(SwiObs.TimeCount > 0);
  TimeIndex := SwiObs.IndexOfClosestTime(Time);
  SwiTime := SwiObs.Times[TimeIndex];
  if SwiTime = Time then
  begin
    BeforeTimeIndex := TimeIndex;
    AfterTimeIndex := TimeIndex;
  end
  else if(Time < SwiTime) then
  begin
    BeforeTimeIndex := TimeIndex-1;
    AfterTimeIndex := TimeIndex;
  end
  else
  begin
    BeforeTimeIndex := TimeIndex;
    AfterTimeIndex := TimeIndex+1;
  end;

  if BeforeTimeIndex < 0 then
  begin
    BeforeTimeIndex := 0;
  end;
  if AfterTimeIndex >= SwiObs.TimeCount then
  begin
    AfterTimeIndex := SwiObs.TimeCount -1;
  end;

  if BeforeTimeIndex <> AfterTimeIndex then
  begin
    DeltaT := SwiObs.Times[AfterTimeIndex] - SwiObs.Times[BeforeTimeIndex];
    if Abs(Time - SwiTime)/DeltaT < Epsilon then
    begin
      BeforeTimeIndex := TimeIndex;
      AfterTimeIndex := TimeIndex;
    end;
  end;

  TotalFraction := 0;
  BeforeValue := 0;
  AfterValue := 0;
  if FAsciiObs.Count > 0 then
  begin
    for ObsIndex := 0 to FAsciiObs.Count - 1 do
    begin
      AnSwiObs := FAsciiObs[ObsIndex];
      ObsPosition := (AnSwiObs.ObsNumber-1)*ZoneCount + ZetaSurface -1;
      ObsName := SwiObs.ObsName[ObsPosition];
      ObsZeta := StrToInt(RightStr(ObsName, 3));
      Assert(ObsZeta = ZetaSurface);
      ObsName := Copy(ObsName, 1, Length(ObsName)-3);
      Assert(UpperCase(AnSwiObs.Name) = UpperCase(ObsName));
      HandleSwiObs(AnSwiObs);
    end;
  end
  else
  begin
    for ObsIndex := 0 to FBinaryObs.Count - 1 do
    begin
      ABinaryObs := FBinaryObs[ObsIndex];
      HandleSwiObs(ABinaryObs);
    end;
  end;
  Assert(TotalFraction > 0);
  if BeforeTimeIndex = AfterTimeIndex then
  begin
    result := BeforeValue/TotalFraction;
  end
  else
  begin
    BeforeValue := BeforeValue/TotalFraction;
    AfterValue := AfterValue/TotalFraction;
    DeltaT := SwiObs.Times[AfterTimeIndex] - SwiObs.Times[BeforeTimeIndex];
    Fraction := (Time - SwiObs.Times[BeforeTimeIndex])/DeltaT;
    result := BeforeValue + Fraction*(AfterValue-BeforeValue);
  end;
end;

end.
