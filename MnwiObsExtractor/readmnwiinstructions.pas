unit ReadMnwiInstructions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ReadMnwiOutput, ObExtractorTypes;

type

  { TMnwiObsProcessor }

  TMnwiObsProcessor = class(TObject)
  private
    FLineIndex: integer;
    FInputFile: TStringList;
    FObsList: TCustomObsValueObjectList;
    FObsDictionary: TCustomObsValueDictionary;
    FListingFileName: string;
    FObservationsFileName: string;
    FListingFile: TStringList;
    FObservationsFile: TStringList;
    procedure HandleSimpleObservations;
    procedure HandleDerivedObservations;
    procedure GetFileNames;
    procedure WriteFiles;
  public
    Constructor Create;
    destructor Destroy; override;
    procedure ProcessInstructionFile(InstructionFileName: string);
  end;


implementation

const
  IVersion = '0.1.0.0';

resourcestring
  rsDERIVED_OBSE = 'DERIVED_OBSERVATIONS';
  rsFILENAMES = 'FILENAMES';
  rsERRORSNotFou = 'ERROR: %s not found among the direct observations';
  rsOnLine0D1SIs = 'On line %0:d, "%1:s" is a duplicate of a previous '
    +'observation.';

  { TMnwiObsProcessor }

procedure TMnwiObsProcessor.HandleSimpleObservations;
var
  ALine: string;
  Splitter: TStringList;
  OutputFileName: string;
  ObsExtractor: TMnwiObsExtractor;
  ObsName: String;
  ObsTypeIndex: Integer;
  ObsTime: double;
  ObsTypes: TStringList;
  Obs: TMnwiObsValue;
  PrintString: string;
  ErrorMessage: string;
  ObservedValue: double;
  Weight: double;
  PackageType: String;
  procedure ProcessObsFile;
  begin
    if ObsExtractor <> nil then
    begin
      try
        ObsExtractor.ExtractSimulatedValues;
      finally
        FreeAndNil(ObsExtractor);
      end;
    end;
  end;
begin
  ObsExtractor := nil;
  ObsTypes := TStringList.Create;
  Splitter := TStringList.Create;
  try
    ObsTypes.Add(UpperCase('Qin'));
    ObsTypes.Add(UpperCase('Qout'));
    ObsTypes.Add(UpperCase('Qnet'));
    ObsTypes.Add(UpperCase('QCumu'));
    ObsTypes.Add(UpperCase('hwell'));
    ObsTypes.CaseSensitive := False;

    Splitter.Delimiter := ' ';
    While FLineIndex < FInputFile.Count do
    begin
      ALine := Trim(FInputFile[FLineIndex]);
      Inc(FLineIndex);
      if (ALine = '') or (ALine[1] = '#') then
      begin
        if Length(ALine) > 0 then
        begin
          FListingFile.Add(ALine);
        end;
        Continue;
      end;

      Splitter.DelimitedText := ALine;
      if Splitter.Count = 2 then
      begin
        if (UpperCase(Splitter[0]) = 'END')
          and (UpperCase(Splitter[1]) = 'OBSERVATIONS') then
        begin
          ProcessObsFile;
          FListingFile.Add('END READING OBSERVATIONS');
          FListingFile.Add('');
          Exit;
        end
        else if UpperCase(Splitter[0]) = 'FILENAME' then
        begin
          ProcessObsFile;
          OutputFileName := RemoveQuotes(Splitter[1]);
          Assert(FileExists(OutputFileName), Format('The MNWI output file "%0:s" specified on line %1:d does not exist', [OutputFileName, FLineIndex]));
          FListingFile.Add(Format('Observations will be read from "%s"',
            [OutputFileName]));
          FListingFile.Add(UpperCase('Observation_Name, Observation_Type, Observation_Time, Observed_Value, Weight, Observation_Print'));
          ObsExtractor := TMnwiObsExtractor.Create;
          ObsExtractor.OutputFileName := OutputFileName;
        end
        else
        begin
          Assert(False);
        end;
      end
      else if Splitter.Count = 3 then
      begin
        if UpperCase(Splitter[0]) = 'FILENAME' then
        begin
          ProcessObsFile;
          PackageType := RemoveQuotes(Splitter[1]);
          OutputFileName := RemoveQuotes(Splitter[2]);
          Assert(FileExists(OutputFileName), Format('The MNWI output file "%0:s" specified on line %1:d does not exist', [OutputFileName, FLineIndex]));
          FListingFile.Add(Format('Observations will be read from "%s"',
            [OutputFileName]));
          FListingFile.Add(UpperCase('Observation_Name, Observation_Type, Observation_Time, Observed_Value, Weight, Observation_Print'));
          ObsExtractor := TMnwiObsExtractor.Create;
          ObsExtractor.OutputFileName := OutputFileName;
        end
        else
        begin
          Assert(False);
        end;
      end
      else if Splitter.Count in [6,7] then
      begin
        Assert(UpperCase(Splitter[0]) = 'OBSERVATION');
        Assert(ObsExtractor <> nil, 'No MNWI output file has been specified for processing.');
        ObsName := Splitter[1];
        ObsTypeIndex := ObsTypes.IndexOf(UpperCase(Splitter[2]));
        Assert(ObsTypeIndex >= 0);
        ObsTime := StrToFloat(Splitter[3]);
        ObservedValue := StrToFloat(Splitter[4]);
        Weight := StrToFloat(Splitter[5]);
        Obs := TMnwiObsValue.Create;
        FObsList.Add(Obs);
        Obs.ObsName := ObsName;
        Obs.ObsType := TMnwiObsType(ObsTypeIndex);
        Obs.ObsTime := ObsTime;
        Obs.ObservedValue := ObservedValue;
        Obs.Weight := Weight;
        if (Splitter.Count = 7) then
        begin
          if (UpperCase(Splitter[6]) = 'PRINT') then
          begin
            Obs.Print := True;
          end
          else if (UpperCase(Splitter[6]) = 'NO_PRINT') then
          begin
            Obs.Print := False;
          end
          else
          begin
            Assert(False);
          end;
        end
        else
        begin
          Obs.Print := True;
        end;
        ObsExtractor.AddObs(Obs);
        try
          FObsDictionary.Add(UpperCase(Obs.ObsName), Obs);
        except on E: Exception do
          begin
            FListingFile.Add(E.Message);
            ErrorMessage := Format(rsOnLine0D1SIs, [FLineIndex, Obs.ObsName]);
            Raise Exception.Create(ErrorMessage);
          end;
        end;
        if Obs.Print then
        begin
          PrintString := 'Print';
        end
        else
        begin
          PrintString := 'Do not Print';

        end;
        FListingFile.Add(Format('%0:s, %1:s, %2:g, %3:g, %4:g %5:s,',
          [Obs.ObsName, ObsTypes[ObsTypeIndex], Obs.ObsTime, Obs.ObservedValue,
          Obs.Weight, PrintString]));
      end
      else
      begin
        Assert(False);
      end;
    end;

  finally
    Splitter.Free;
    ObsTypes.Free;
    ObsExtractor.Free;
  end;
end;

procedure TMnwiObsProcessor.HandleDerivedObservations;
var
  ALine: string;
  Splitter: TStringList;
  FirstValue: TCustomObsValue;
  SecondValue: TCustomObsValue;
  Obs: TMnwiObsValue;
  ObsName: string;
  FirstName: string;
  SecondName: string;
  ErrorMessage: string;
begin
  FListingFile.Add(UpperCase('Derived_Observation_Name, Formula, Observed_Value, Weight, Print'));
  Splitter := TStringList.Create;
  try
    Splitter.Delimiter := ' ';
    While FLineIndex < FInputFile.Count do
    begin
      ALine := Trim(FInputFile[FLineIndex]);
      Inc(FLineIndex);
      if (ALine = '') or (ALine[1] = '#') then
      begin
        if Length(ALine) > 0 then
        begin
          FListingFile.Add(ALine);
        end;
        Continue;
      end;

      Splitter.DelimitedText := ALine;
      if (Splitter.Count = 2)
        and (UpperCase(Splitter[0]) = 'END')
        and (UpperCase(Splitter[1]) = rsDERIVED_OBSE)
        then
      begin
        FListingFile.Add('END READING DERIVED OBSERVATIONS');
        Exit;
      end
      else if (Splitter.Count in [6,7]) and
        ((UpperCase(Splitter[0]) = 'DIFFERENCE')
        or (UpperCase(Splitter[0]) = 'SUM'))
        then
      begin
        ObsName := Splitter[1];
        Obs := TMnwiObsValue.Create;
        Obs.ObsName := ObsName;
        try
          FObsDictionary.Add(UpperCase(Obs.ObsName), Obs);
        except on E: Exception do
          begin
            FListingFile.Add(E.Message);
            ErrorMessage := Format(rsOnLine0D1SIs, [FLineIndex, Obs.ObsName]);
            Raise Exception.Create(ErrorMessage);
          end;
        end;
        if (Splitter.Count = 7) then
        begin
          if (UpperCase(Splitter[6]) = 'PRINT') then
          begin
            Obs.Print := True;
          end
          else if (UpperCase(Splitter[6]) = 'NO_PRINT') then
          begin
            Obs.Print := False;
          end
          else
          begin
            Assert(False);
          end;
        end
        else
        begin
          Obs.Print := True;
        end;
        FObsList.Add(Obs);
        Obs.SimulatedValue := MissingValue;
        FirstName := Splitter[2];
        SecondName := Splitter[3];
        Obs.ObservedValue := StrToFloat(Splitter[4]);
        Obs.Weight := StrToFloat(Splitter[5]);
        if (UpperCase(Splitter[0]) = 'DIFFERENCE') then
        begin
          FListingFile.Add(Format('%0:s, %1:s - %2:s, %3:g, %4:g, PRINT', [Obs.ObsName, FirstName, SecondName, Obs.ObservedValue, Obs.Weight]));
        end
        else
        begin
          FListingFile.Add(Format('%0:s, %1:s + %2:s, %3:g, %4:g, PRINT', [Obs.ObsName, FirstName, SecondName, Obs.ObservedValue, Obs.Weight]));
        end;
        if not FObsDictionary.TryGetValue(UpperCase(FirstName), FirstValue) then
        begin
          FirstValue := nil;
          FListingFile.Add(Format(rsERRORSNotFou, [FirstName]));
        end;
        if not FObsDictionary.TryGetValue(UpperCase(SecondName), SecondValue) then
        begin
          SecondValue := nil;
          FListingFile.Add(Format(rsERRORSNotFou, [FirstName]));
        end;
        if (FirstValue <> nil) and (SecondValue <> nil) then
        begin
          if (FirstValue.SimulatedValue <> MissingValue)
            or (SecondValue.SimulatedValue <> MissingValue) then
          begin
            if (UpperCase(Splitter[0]) = 'DIFFERENCE') then
            begin
              Obs.SimulatedValue :=
                FirstValue.SimulatedValue - SecondValue.SimulatedValue;
            end
            else
            begin
              Assert(UpperCase(Splitter[0]) = 'SUM');
              Obs.SimulatedValue :=
                FirstValue.SimulatedValue + SecondValue.SimulatedValue;
            end
          end;
        end;
      end
      else
      begin
        Assert(False);
      end;
    end;

  finally
    Splitter.Free;
  end;
end;

procedure TMnwiObsProcessor.GetFileNames;
var
  Splitter: TStringList;
  ALine: string;
begin
  Splitter := TStringList.Create;
  try
    Splitter.Delimiter := ' ';
    While FLineIndex < FInputFile.Count do
    begin
      ALine := Trim(FInputFile[FLineIndex]);
      Inc(FLineIndex);
      if (ALine = '') or (ALine[1] = '#') then
      begin
        if Length(ALine) > 0 then
        begin
          FListingFile.Add(ALine);
        end;
        Continue;
      end;

      Splitter.DelimitedText := ALine;
      if (Splitter.Count = 2)then
      begin
        if (UpperCase(Splitter[0]) = 'LISTING_FILE') then
        begin
          FListingFileName := RemoveQuotes(Splitter[1]);
        end
        else if (UpperCase(Splitter[0]) = 'OBSERVATIONS_FILE') then
        begin
          FObservationsFileName := RemoveQuotes(Splitter[1]);
          FListingFile.Add('Observations file = ' + FObservationsFileName);
        end
        else if (UpperCase(Splitter[0]) = 'END')
          and (UpperCase(Splitter[1]) = rsFILENAMES) then
        begin
          FListingFile.Add(Format('Observations file name = %s', [FObservationsFileName]));
          FListingFile.Add('END OUTPUT FILE NAMES');
          FListingFile.Add('');
          Assert(FListingFileName <> '');
          Assert(FObservationsFileName <> '');
          Exit;
        end
        else
        begin
          Assert(False);
        end;
      end
      else
      begin
        Assert(False);
      end;
    end;
  finally
    Splitter.Free;
  end;
end;

procedure TMnwiObsProcessor.WriteFiles;
var
  Index: Integer;
  Obs: TCustomObsValue;// TMnwiObsValue;
  ErrorMessage: string;
  ObsPrinted: Boolean;
begin
  Assert(FObsList.Count > 0, 'No observations specified');
  ObsPrinted := False;
  FListingFile.Add('');
  FListingFile.Add('Observation_Name, Simulated_Value, Observed_Value, Weight');
  FObservationsFile.Add('Observation_Name, Simulated_Value, Observed_Value, Weight');
  for Index := 0 to Pred(FObsList.Count) do
  begin
    Obs := FObsList[Index];
    if Obs.Print then
    begin
      ObsPrinted := True;
      FObservationsFile.Add(
        Format('"%0:s", %1:g, %2:g, %3:g', [Obs.ObsName, Obs.SimulatedValue, Obs.ObservedValue, Obs.Weight]));
    end;
    FListingFile.Add(
      Format('"%0:s", %1:g, %2:g, %3:g', [Obs.ObsName, Obs.SimulatedValue, Obs.ObservedValue, Obs.Weight]));
  end;
  FListingFile.Add('');
  Assert(ObsPrinted, 'No observations printed');
  try
    FObservationsFile.SaveToFile(FObservationsFileName);
  except on E: Exception do
    begin
      FListingFile.Add(E.Message);
      if FObservationsFileName = '' then
      begin
        ErrorMessage := 'Error saving observations file because no file name specified.';
      end
      else
      begin
        ErrorMessage := Format('Error saving observation file "%s".', [FObservationsFileName])
      end;
      WriteLn(ErrorMessage);
      FListingFile.Add(ErrorMessage);
    end;
  end;
  try
    FListingFile.Add('normal termination');
    FListingFile.SaveToFile(FListingFileName);
  except  on E: Exception do
    begin
      if FListingFileName = '' then
      begin
        WriteLn('Error saving listing file because no file name specified.');
      end
      else
      begin
        WriteLn(Format('Error saving listing file "%s".', [FListingFileName]));
      end;
    end;
  end;
  Writeln('normal termination');
end;

constructor TMnwiObsProcessor.Create;
begin
  FObsList := TCustomObsValueObjectList.Create;
  FObsDictionary := TCustomObsValueDictionary.Create;
  //FObsDictionary.Duplicates := dupError;
  //FObsDictionary.Sorted := True;
  FListingFile := TStringList.Create;
  FObservationsFile := TStringList.Create;
  FListingFile.Add('MNWI Observation Extractor');
  FListingFile.Add('Version ' + IVersion);
  FListingFile.Add('');
end;

destructor TMnwiObsProcessor.Destroy;
begin
  FObsDictionary.Free;
  FObsList.Free;
  FListingFile.Free;
  FObservationsFile.Free;
  inherited Destroy;
end;

procedure TMnwiObsProcessor.ProcessInstructionFile(InstructionFileName: string);
var
  ALine: string;
  FileNamesFound: Boolean;
  ObservationsFound: Boolean;
  ErrorMessage: string;
begin
  FileNamesFound := False;
  ObservationsFound := False;
  FInputFile := TStringList.Create;
  try
    try
        FInputFile.LoadFromFile(InstructionFileName);
        FLineIndex := 0;
        While FLineIndex < FInputFile.Count do
        begin
          ALine := Trim(FInputFile[FLineIndex]);
          Inc(FLineIndex);
          if (ALine = '') or (ALine[1] = '#') then
          begin
            if Length(ALine) > 0 then
            begin
              FListingFile.Add(ALine);
            end;
            Continue;
          end;
          ALine := UpperCase(ALine);
          if Pos('BEGIN', ALine) = 1 then
          begin
            ALine := Trim(Copy(ALine, 7, MAXINT));
            if ALine = rsFILENAMES then
            begin
              Assert(not FileNamesFound);
              FListingFile.Add('');
              FListingFile.Add(UpperCase('Reading output file names'));
              GetFileNames;
              FileNamesFound := True;
            end
            else if ALine = 'OBSERVATIONS' then
            begin
              Assert(not ObservationsFound);
              FListingFile.Add('');
              FListingFile.Add(UpperCase('Reading observations'));
              Assert(FileNamesFound);
              HandleSimpleObservations;
              ObservationsFound := True;
            end
            else if ALine = rsDERIVED_OBSE then
            begin
              FListingFile.Add('');
              FListingFile.Add(UpperCase('Reading derived observations'));
              Assert(FileNamesFound);
              Assert(ObservationsFound);
              HandleDerivedObservations;
              Exit;
            end;
          end
          else
          begin
            Assert(False);
          end;
        end;
        Assert(FileNamesFound, 'No output file names were specified');
        Assert(ObservationsFound, 'No observations were specified');
    except on E: Exception do
      begin
        Writeln(E.message);
        FListingFile.Add(E.message);

        ErrorMessage := Format('Error processing line %0:d of %1:s',
          [FLineIndex, InstructionFileName]);
        Writeln(ErrorMessage);
        FListingFile.Add(ErrorMessage);
        if (FLineIndex > 0) and (FLineIndex <= FInputFile.Count) then
        begin
          FListingFile.Add(FInputFile[FLineIndex-1]);
        end;
      end;
    end;
  finally
    WriteFiles;
    FInputFile.Free;
  end;
end;

end.

