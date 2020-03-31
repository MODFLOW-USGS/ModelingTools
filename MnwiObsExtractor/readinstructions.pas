unit readinstructions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ReadMnwiOutput, ObExtractorTypes, ReadNameFile,
    Generics.Collections, Generics.Defaults;

type

  { TObsProcessor }

  TObsProcessor = class(TObject)
  private
    FLineIndex: integer;
    FInputFile: TStringList;
    FObsList: TCustomObsValueObjectList;
    FObsDictionary: TCustomObsValueDictionary;
    //FListingFileName: string;
    //FObservationsFileName: string;
    FListingFile: TStringList;
    FObservationsFile: TStringList;
    FFileLink: TInputFileLink;
    FGenerateInstructionFile: Boolean;
    procedure HandleSimpleObservations;
    //procedure GetFileNames;
  public
    Constructor Create(FileLink: TInputFileLink; GenerateInstructionFile: Boolean);
    destructor Destroy; override;
    procedure ProcessInstructionFile;
    procedure HandleDerivedObservations;
    procedure WriteFiles;
    property ListingFile: TStringList read FListingFile write FListingFile;
    property ObservationsFile: TStringList read FObservationsFile
      write FObservationsFile;
    property ObsDictionary: TCustomObsValueDictionary read FObsDictionary
      write FObsDictionary;
  end;

  TObsProcessorList = specialize TObjectList<TObsProcessor>;

implementation

uses readgageoutput;

resourcestring
  rsDERIVED_OBSE = 'DERIVED_OBSERVATIONS';
  rsERRORSNotFou = 'ERROR: %s not found among the direct observations';
  rsOnLine0D1SIs = 'On line %0:d of %1:s, "%2:s" is a duplicate of a previous '
    +'observation.';

  { TObsProcessor }

procedure TObsProcessor.HandleSimpleObservations;
var
  ALine: string;
  Splitter: TStringList;
  OutputFileName: string;
  ObsExtractor: TCustomObsExtractor;
  ObsName: String;
  ObsTypeIndex: Integer;
  ObsTime: double;
  ObsTypes: TStringList;
  Obs: TCustomObsValue;
  PrintString: string;
  ErrorMessage: string;
  ObservedValue: double;
  Weight: double;
  ObsTypeName: string;
  //PackageType: String;
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
    case FFileLink.FileType of
      iftMNW2:
        begin
          ObsTypes.Add(UpperCase('Qin'));
          ObsTypes.Add(UpperCase('Qout'));
          ObsTypes.Add(UpperCase('Qnet'));
          ObsTypes.Add(UpperCase('QCumu'));
          ObsTypes.Add(UpperCase('hwell'));
        end;
      iftLAK:
        begin
          ObsTypes.Assign(LakeGageOutputTypes)
        end;
      iftSFR:
        begin
          ObsTypes.Assign(StreamGageOutputTypes)
        end;
    else Assert(false, 'programming error');
    end;
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
          Assert(FileExists(OutputFileName), Format('The output file "%0:s" specified on line %1:d does not exist', [OutputFileName, FLineIndex]));
          FListingFile.Add(Format('Observations will be read from "%s"',
            [OutputFileName]));
          FListingFile.Add(UpperCase('Observation_Name, Observation_Type, Observation_Time, Observed_Value, Weight, Observation_Print'));
          if not FGenerateInstructionFile then
          begin
            case FFileLink.FileType of
              iftMNW2:
                begin
                  ObsExtractor := TMnwiObsExtractor.Create;
                end;
              iftLAK:
                begin
                  ObsExtractor := TLakeGageObsExtractor.Create;
                end;
              iftSFR:
                begin
                  ObsExtractor := TSfrGageObsExtractor.Create;
                end;
            else Assert(False);
            end;
            ObsExtractor.OutputFileName := OutputFileName;
          end;
        end
        else
        begin
          Assert(False);
        end;
      end
      //else if Splitter.Count = 2 then
      //begin
      //  if UpperCase(Splitter[0]) = 'FILENAME' then
      //  begin
      //    ProcessObsFile;
      //    //PackageType := RemoveQuotes(Splitter[1]);
      //    OutputFileName := RemoveQuotes(Splitter[1]);
      //    Assert(FileExists(OutputFileName), Format('The MNWI output file "%0:s" specified on line %1:d does not exist', [OutputFileName, FLineIndex]));
      //    FListingFile.Add(Format('Observations will be read from "%s"',
      //      [OutputFileName]));
      //    FListingFile.Add(UpperCase('Observation_Name, Observation_Type, Observation_Time, Observed_Value, Weight, Observation_Print'));
      //    ObsExtractor := TMnwiObsExtractor.Create;
      //    ObsExtractor.OutputFileName := OutputFileName;
      //  end
      //  else
      //  begin
      //    Assert(False);
      //  end;
      //end
      else if Splitter.Count in [6,7] then
      begin
        Assert(UpperCase(Splitter[0]) = 'OBSERVATION');
        if not FGenerateInstructionFile then
        begin
          Assert(ObsExtractor <> nil, 'No MNWI output file has been specified for processing.');
        end;
        ObsName := Splitter[1];
        ObsTypeName := UpperCase(Splitter[2]);
        ObsTypeIndex := ObsTypes.IndexOf(ObsTypeName);
        Assert(ObsTypeIndex >= 0);
        ObsTime := StrToFloat(Splitter[3]);
        ObservedValue := StrToFloat(Splitter[4]);
        Weight := StrToFloat(Splitter[5]);
        case FFileLink.FileType of
          iftMNW2:
            begin
              Obs := TMnwiObsValue.Create;
              TMnwiObsValue(Obs).ObsType := TMnwiObsType(ObsTypeIndex);
            end;
          iftLAK, iftSFR:
            begin
              Obs := TGageObsValue.Create;
              TGageObsValue(Obs).ObsType := ObsTypeName;
            end;
          else Assert(False);
        end;
        FObsList.Add(Obs);
        Obs.ObsName := ObsName;
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
        if not FGenerateInstructionFile then
        begin
          ObsExtractor.AddObs(Obs);
        end;
        try
          FObsDictionary.Add(UpperCase(Obs.ObsName), Obs);
        except on E: Exception do
          begin
            FListingFile.Add(E.Message);
            ErrorMessage := Format(rsOnLine0D1SIs, [FLineIndex, FFileLink.FileName, Obs.ObsName]);
            FListingFile.Add(ErrorMessage);
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

procedure TObsProcessor.HandleDerivedObservations;
var
  ALine: string;
  Splitter: TStringList;
  FirstValue: TCustomObsValue;
  SecondValue: TCustomObsValue;
  Obs: TCustomObsValue;
  ObsName: string;
  FirstName: string;
  SecondName: string;
  ErrorMessage: string;
begin
  Assert(ListingFile <> nil, 'programming error');
  Assert(ObservationsFile <> nil, 'programming error');
  Assert(ObsDictionary <> nil, 'programming error');

  if FLineIndex < FInputFile.Count then
  begin
    FListingFile.Add('');
    FListingFile.Add(UpperCase(Format('Reading derived observations from "%s"', [FFileLink.FileName])));
    FListingFile.Add('');
    FListingFile.Add(UpperCase('Derived_Observation_Name, Formula, Observed_Value, Weight, Print'));
  end
  else
  begin
    Exit;
  end;
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
        Obs := TCustomObsValue.Create;
        Obs.ObsName := ObsName;
        try
          FObsDictionary.Add(UpperCase(Obs.ObsName), Obs);
        except on E: Exception do
          begin
            FListingFile.Add(E.Message);
            ErrorMessage := Format(rsOnLine0D1SIs, [FLineIndex, FFileLink.FileName, Obs.ObsName]);
            FListingFile.Add(ErrorMessage);
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

//procedure TObsProcessor.GetFileNames;
//var
//  Splitter: TStringList;
//  ALine: string;
//begin
//  Splitter := TStringList.Create;
//  try
//    Splitter.Delimiter := ' ';
//    While FLineIndex < FInputFile.Count do
//    begin
//      ALine := Trim(FInputFile[FLineIndex]);
//      Inc(FLineIndex);
//      if (ALine = '') or (ALine[1] = '#') then
//      begin
//        if Length(ALine) > 0 then
//        begin
//          FListingFile.Add(ALine);
//        end;
//        Continue;
//      end;
//
//      Splitter.DelimitedText := ALine;
//      if (Splitter.Count = 2)then
//      begin
//        if (UpperCase(Splitter[0]) = 'LISTING_FILE') then
//        begin
//          FListingFileName := RemoveQuotes(Splitter[1]);
//        end
//        else if (UpperCase(Splitter[0]) = 'OBSERVATIONS_FILE') then
//        begin
//          FObservationsFileName := RemoveQuotes(Splitter[1]);
//          FListingFile.Add('Observations file = ' + FObservationsFileName);
//        end
//        else if (UpperCase(Splitter[0]) = 'END')
//          and (UpperCase(Splitter[1]) = rsFILENAMES) then
//        begin
//          FListingFile.Add(Format('Observations file name = %s', [FObservationsFileName]));
//          FListingFile.Add('END OUTPUT FILE NAMES');
//          FListingFile.Add('');
//          Assert(FListingFileName <> '');
//          Assert(FObservationsFileName <> '');
//          Exit;
//        end
//        else
//        begin
//          Assert(False);
//        end;
//      end
//      else
//      begin
//        Assert(False);
//      end;
//    end;
//  finally
//    Splitter.Free;
//  end;
//end;

procedure TObsProcessor.WriteFiles;
var
  Index: Integer;
  Obs: TCustomObsValue;// TMnwiObsValue;
  //ErrorMessage: string;
  ObsPrinted: Boolean;
begin
  Assert(FObsList.Count > 0, 'No observations specified');
  ObsPrinted := False;
  FListingFile.Add('');
  FListingFile.Add('Observation_Name, Simulated_Value, Observed_Value, Weight');
  if not FGenerateInstructionFile and (FObservationsFile.Count = 0) then
  begin
    FObservationsFile.Add('Observation_Name, Simulated_Value, Observed_Value, Weight');
  end;
  for Index := 0 to Pred(FObsList.Count) do
  begin
    Obs := FObsList[Index];
    if Obs.Print then
    begin
      ObsPrinted := True;
      if FGenerateInstructionFile then
      begin
        FObservationsFile.Add(Format('l1 @"%0:s",@ w !%0:s! @,@', [Obs.ObsName]));
      end
      else
      begin
        FObservationsFile.Add(
          Format('"%0:s", %1:g, %2:g, %3:g', [Obs.ObsName, Obs.SimulatedValue, Obs.ObservedValue, Obs.Weight]));
      end;
    end;
    if FGenerateInstructionFile then
    begin
      FListingFile.Add(Format('l1 @"%0:s",@ w !%0:s! @,@', [Obs.ObsName]));
    end
    else
    begin
      FListingFile.Add(
        Format('"%0:s", %1:g, %2:g, %3:g', [Obs.ObsName, Obs.SimulatedValue, Obs.ObservedValue, Obs.Weight]));
    end;
  end;
  FListingFile.Add('');
  Assert(ObsPrinted, 'No observations printed');
end;

constructor TObsProcessor.Create(FileLink: TInputFileLink;
  GenerateInstructionFile: Boolean);
begin
  FFileLink := FileLink;
  FGenerateInstructionFile := GenerateInstructionFile;
  FObsList := TCustomObsValueObjectList.Create;
  //FObsDictionary := TCustomObsValueDictionary.Create;
  //FObsDictionary.Duplicates := dupError;
  //FObsDictionary.Sorted := True;
  //FListingFile := TStringList.Create;
  //FObservationsFile := TStringList.Create;
  //FListingFile.Add('MNWI Observation Extractor');
  //FListingFile.Add('Version ' + IVersion);
  //FListingFile.Add('');
end;

destructor TObsProcessor.Destroy;
begin
  //FObsDictionary.Free;
  FObsList.Free;
  //FListingFile.Free;
  //FObservationsFile.Free;
  FInputFile.Free;
  inherited Destroy;
end;

procedure TObsProcessor.ProcessInstructionFile;
var
  ALine: string;
  //FileNamesFound: Boolean;
  ObservationsFound: Boolean;
  ErrorMessage: string;
  InstructionFileName: string;
begin
  Assert(ListingFile <> nil, 'programming error');
  Assert(ObservationsFile <> nil, 'programming error');
  Assert(ObsDictionary <> nil, 'programming error');

  InstructionFileName := FFileLink.FileName;
  //FileNamesFound := False;
  ObservationsFound := False;
  FInputFile := TStringList.Create;
  //try
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
          if ALine = 'OBSERVATIONS' then
          begin
            Assert(not ObservationsFound);
            FListingFile.Add('');
            FListingFile.Add(UpperCase('Reading observations'));
            //Assert(FileNamesFound);
            HandleSimpleObservations;
            ObservationsFound := True;
          end
          else if ALine = rsDERIVED_OBSE then
          begin
            //Assert(FileNamesFound);
            //Assert(ObservationsFound);
            //HandleDerivedObservations;
            Exit;
          end;
        end
        else
        begin
          Assert(False);
        end;
      end;
      //Assert(FileNamesFound, 'No output file names were specified');
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
  //finally
  //  //WriteFiles;
  //  FInputFile.Free;
  //end;
end;

end.

