unit InputFileReader;

{$mode objfpc}{$H+}
{$ASSERTIONS ON}

interface

uses
  Classes, SysUtils, Generics.Defaults, CustomOutputFileReader,
  OutputFileReader, RbwParser, FastGeo, CustomInputReader;

type
  { TInputHandler }

  TInputHandler = class(TCustomInputHandler)
  private
    //FPriorProcessStatus: TProcessStatus;
    FOutputFileName: string;
    FListingFileName: string;
    FInstructionFileName: string;
    //FObsFileList: TOutputFileObjectList;
    FObservationDictionary: TObservationDictionary;
    FID: string;
  protected
    procedure ClearFileObs; override;
    function CreateObsFile(const FileType: TFileType;
      const FileName: string): TCustomOutputFile; override;
    procedure HandleOption;
    //procedure HandleObservationFiles;
    procedure InitializeObsFiles;
    procedure HandleIdentifiers;
    procedure InterpolateInTime;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadAndProcessInputFile(const FileName: string);
  end;

implementation

resourceString
  rsBEGIN = 'BEGIN';
  rsOPTIONS = 'OPTIONS';
  rsOBSERVATION_Files = 'OBSERVATION_FILES';
  rsIDENTIFIERS = 'IDENTIFIERS';
  rsOutputFileS = '  Output file = %s';
  rsInstructionF = '  Instruction file = %s';
  rsNotExactlyTwoItems = 'In line %0:d, "%1:s", there were not exactly two '
    +'items listed.';
  rsLISTING = 'LISTING';
  rsTheListingFi = 'The listing file was already set to "0:s".';
  rsMODFLOW6Obse = 'MODFLOW 6 Observation Extractor';
  rsBeginOPTIONS = 'Begin OPTIONS block';
  rsListingFileS = '  Listing file = %s';
  rsOUTPUT = 'OUTPUT';
  rsTheOutputFil = 'The output file was already set to "0:s".';
  rsINSTRUCTION = 'INSTRUCTION';
  rsTheInstructi = 'The instruction file was already set to "0:s".';
  rsBEGINOPTIONS2 = 'BEGIN OPTIONS must be paired with END OPTIONS in line %0:'
    +'d, "%1:s".';
  rsEndOfOPTIONS = 'End of OPTIONS block';
  rsUnrecognized = 'Unrecognized option in line %0:d, ""%1:s.';
  //rsFILENAME = 'FILENAME';
  //rsTheObservati = 'The observation file "%s" does not exist';
  //rsBINARY = 'BINARY';
  //rsTEXT = 'TEXT';
  //rsTheFileForma = 'The file format in line %0:d, "%1:s" must be "BINARY" or "'
    //+'TEXT".';
  //rsObservationF = '  Observation file = %s';
  //rsFormatTextFi = '  Format = text file';
  //rsFormatBinary = '  Format = binary file';
  rsMustBeTwo = 'In line %0:d, "%1:s", there must be exactly two items listed.';
  //rsBEGINOBSERVA = 'BEGIN OBSERVATION_FILES must be paired with END '
  //  +'OBSERVATION_FILES in line %0:d, "%1:s".';
  //rsEndOfOBSERVA = 'End of OBSERVATION_FILES Block';
  rsBeginOBSERVA2 = 'Begin OBSERVATION_FILES Block';
  rsID = 'ID';
  rsMustStartWithID = 'In line %0:d, "%1:s" must start with "ID".';
  rsLOCATION = 'LOCATION';
  rsStartWithObsOrLocation = 'In line %0:d, "%1:s" must start with "LOCATION" '
    +'or "OBSNAME".';
  rsStartWithObsname = 'In line %0:d, "%1:s" must start with "OBSNAME".';
  rsBEGINIDENTIF2 = 'BEGIN IDENTIFIERS must be paired with END IDENTIFIERS in '
    +'line %0:d, "%1:s".';
  rsMustStartWithObsnameOrEnd = 'In line %0:d, "%1:s" must start with "OBSNAME'
    +'" or "END".';
  rsIDInObservat = '  ID in observation file = %s';
  rsIMustBeThree = 'In line %0:d, "%1:s", there must be exactly three items '
    +'listed.';
  rsErrorConvert = 'Error converting X or Y coordinate on line %0:d, "%1:s".';
  rsLocationInOb = '  Location in observation file = (%0:g, %1:g)';
  rsMustBeThreeOrFor = 'In line %0:d, "%1:s", there must be exactly three or '
    +'four items listed.';
  rsErrorConvert2 = 'Error converting time on line %0:d, "%1:s".';
  rsIMustBePrint = 'In line %0:d, "%1:s", the fourth item, if present, must be'
    +' "PRINT".';
  rsObservationN = '  Observation Name = %s.';
  rsObservationT = '  Observation time = %g.';
  rsProgrammingE = 'Programming error in TInputHandler.HandleIdentifiers';
  rsBeginExtract =
    'Begin extracting observation values interpolated in time.';
  rsEndExtract =
    'End of extracting observation values interpolated in time.';
  rsTheObservati2 = 'The observation %s is not found in any of the observation'
    +' output files.';
  rsObservationN2 = '  Observation Name = %s';
  rsObservationT2 = '  Observation time = %g';
  rsObservationV = '  Observation Value = %g';
  rsPestOrUcode = 'In line %0:d, "%1:s", the third item must be either "PEST" '
    +'or "UCODE".';
  rsStandardFile = 'StandardFile, 0, 2, %d';
  rsProcessingOB = 'Processing OBSERVATION_FILES Block';
  rsEndOfIDENTIF = 'End of IDENTIFIERS Block';

{ TInputHandler }

procedure TInputHandler.ClearFileObs;
begin
  FObsFileList.Clear;
  FObservationDictionary.Clear;
end;

function TInputHandler.CreateObsFile(const FileType: TFileType;
  const FileName: string): TCustomOutputFile;
begin
  result := TOutputFile.Create(FileName, FileType, FObservationDictionary);
end;

procedure TInputHandler.HandleOption;

  procedure RecordOutputFile;
  begin
    if (FOutputFileName <> '') and (FListingFile <> nil) then
    begin
      FListingFile.Add(Format(rsOutputFileS, [FOutputFileName]));
    end;
  end;

  procedure RecordInstructionFile;
  begin
    if (FInstructionFileName <> '') and (FListingFile <> nil) then
    begin
      FListingFile.Add(Format(rsInstructionF, [FInstructionFileName]));
      case FInstructionFileMode of
        ifmPest:
          begin
            FListingFile.Add('  Instruction file format = PEST');
          end;
        ifmUCODE:
          begin
            FListingFile.Add('  Instruction file format = UCODE');
          end;
      end;
    end;
  end;

begin
  Assert(FSplitter.Count in [2, 3], Format(rsExactlyTwoOrThree, [FLineIndex+1,
    FInputFileLines[FLineIndex]]));
  if UpperCase(FSplitter[0]) = rsLISTING then
  begin
    Assert(FSplitter.Count = 2, Format(rsNotExactlyTwoItems, [FLineIndex+1,
      FInputFileLines[FLineIndex]]));
    Assert(FListingFileName = '', Format(rsTheListingFi, [FListingFileName]));
    FListingFileName := FSplitter[1];
    FListingFile := TStringList.Create;
    FListingFile.Add(rsMODFLOW6Obse);
    FListingFile.Add('');
    FListingFile.Add(rsBeginOPTIONS);
    FListingFile.Add(Format(rsListingFileS, [FListingFileName]));

    RecordOutputFile;
    RecordInstructionFile;
  end
  else if UpperCase(FSplitter[0]) = rsOUTPUT then
  begin
    Assert(FSplitter.Count = 2, Format(rsNotExactlyTwoItems, [FLineIndex+1,
      FInputFileLines[FLineIndex]]));
    Assert(FOutputFileName = '', Format(rsTheOutputFil, [FOutputFileName]));
    FOutputFileName := FSplitter[1];
    FOutputFile := TStringList.Create;
    RecordOutputFile;
  end
  else if UpperCase(FSplitter[0]) = rsINSTRUCTION then
  begin
    Assert(FInstructionFileName = '', Format(rsTheInstructi, [
      FInstructionFileName]));
    FInstructionFileName := FSplitter[1];
    if FSplitter.Count = 3 then
    begin
      if UpperCase(FSplitter[2]) = 'UCODE' then
      begin
        FInstructionFileMode := ifmUCODE;
      end
      else if UpperCase(FSplitter[2]) = 'PEST' then
      begin
        FInstructionFileMode := ifmPest;
      end
      else
      begin
        Assert(False, Format(rsPestOrUcode, [FLineIndex+1,
          FInputFileLines[FLineIndex]]));
      end;
    end
    else
    begin
      FInstructionFileMode := ifmPest;
    end;
    FInstructionFile := TStringList.Create;
    case FInstructionFileMode of
      ifmPest:
        begin
          FInstructionFile.Add('pif @');
        end;
      ifmUCODE:
        begin
          FInstructionFile.Add('jtf @');
        end;
    end;
    RecordInstructionFile;
  end
  else if UpperCase(FSplitter[0]) = rsEND then
  begin
    Assert(UpperCase(FSplitter[1]) = rsOPTIONS, Format(rsBEGINOPTIONS2,
      [FLineIndex+1, FInputFileLines[FLineIndex]]));
    FCurrentProcessStatus := psNone;
    FPriorProcessStatus := psOptions;
    if FListingFile <> nil then
    begin
      FListingFile.Add(rsEndOfOPTIONS);
      FListingFile.Add('');
    end;
  end
  else
  begin
    Assert(False, Format(rsUnrecognized, [FLineIndex+1, FInputFileLines[
      FLineIndex]]));
  end;
end;

//procedure TInputHandler.HandleObservationFiles;
//var
//  FileName: string;
//  FileType: TFileType;
//  ObsFile: TCustomOutputFile;
//begin
//  Assert(FSplitter.Count in [2, 3], Format(rsExactlyTwoOrThree, [FLineIndex+1,
//    FInputFileLines[FLineIndex]]));
//  if UpperCase(FSplitter[0]) = rsFILENAME then
//  begin
//    FileName := FSplitter[1];
//    Assert(FileExists(FileName), Format(rsTheObservati, [FileName]));
//    if FSplitter.Count = 3 then
//    begin
//      if UpperCase(FSplitter[2]) = rsBINARY then
//      begin
//        FileType := ftBinary
//      end
//      else if UpperCase(FSplitter[2]) = rsTEXT then
//      begin
//        FileType := ftText
//      end
//      else
//      begin
//        Assert(False, Format(rsTheFileForma, [FLineIndex+1,
//          FInputFileLines[FLineIndex]]));
//      end;
//    end
//    else
//    begin
//      FileType := ftBinary
//    end;
//    if FListingFile <> nil then
//    begin
//      FListingFile.Add(Format(rsObservationF, [FileName]));
//      if FileType  = ftText then
//      begin
//        FListingFile.Add(rsFormatTextFi);
//      end
//      else
//      begin
//        FListingFile.Add(rsFormatBinary);
//      end;
//      FListingFile.Add('');
//    end;
//    ObsFile := CreateObsFile(FileType, FileName);
//    FObsFileList.Add(ObsFile);
//  end
//  else if UpperCase(FSplitter[0]) = rsEND then
//  begin
//    Assert(FSplitter.Count = 2, Format(rsMustBeTwo, [FLineIndex+1,
//      FInputFileLines[FLineIndex]]));
//    Assert(UpperCase(FSplitter[1]) = rsOBSERVATION_Files, Format(
//      rsBEGINOBSERVA, [FLineIndex+1, FInputFileLines[FLineIndex]]));
//    FCurrentProcessStatus := psNone;
//    FPriorProcessStatus := psObsFiles;
//    if FListingFile <> nil then
//    begin
//      FListingFile.Add(rsEndOfOBSERVA);
//      FListingFile.Add('');
//    end;
//  end
//  else
//  begin
//    Assert(False, Format(rsUnrecognized, [FLineIndex+1, FInputFileLines[
//      FLineIndex]]));
//  end;
//end;

procedure TInputHandler.InitializeObsFiles;
begin
  FIdentifiersRead := False;
  FCurrentProcessStatus := psObsFiles;
  ClearAllObservations;

  if FListingFile <> nil then
  begin
    FListingFile.Add(rsBeginOBSERVA2);
  end;
  WriteLn(rsProcessingOB);
end;

procedure TInputHandler.HandleIdentifiers;
var
  LocationID: TLocationID;
  DerivedObs: TDerivedObs;
begin
  case FIdStatus of
    isNone:
      begin
        if UpperCase(FSplitter[0]) = rsID then
        begin
          FIdStatus := isID
        end
        else
        begin
          Assert(False, Format(rsMustStartWithID, [FLineIndex+1,
            FInputFileLines[FLineIndex]]));
        end;
      end;
    isID:
      begin
        if UpperCase(FSplitter[0]) = rsLOCATION then
        begin
          FIdStatus := isLocation
        end
        else if UpperCase(FSplitter[0]) = rsOBSNAME then
        begin
          FIdStatus := isTime
        end
        else
        begin
          Assert(False, Format(rsStartWithObsOrLocation, [FLineIndex+1,
            FInputFileLines[FLineIndex]]));
        end;
      end;
    isLocation:
      begin
        if UpperCase(FSplitter[0]) = rsOBSNAME then
        begin
          FIdStatus := isTime
        end
        else
        begin
          Assert(False, Format(rsStartWithObsname, [FLineIndex+1,
            FInputFileLines[FLineIndex]]));
        end;
      end;
    isTime:
      begin
        if UpperCase(FSplitter[0]) = rsOBSNAME then
        begin
          FIdStatus := isTime
        end
        else if UpperCase(FSplitter[0]) = rsID then
        begin
          FIdStatus := isID
        end
        else if UpperCase(FSplitter[0]) = rsEND then
        begin
          FIdStatus := isNone;
          FPriorProcessStatus := psIdentifiers;
          FCurrentProcessStatus := psNone;
          Assert(FSplitter.Count = 2, Format(rsMustBeTwo, [FLineIndex+1,
            FInputFileLines[FLineIndex]]));
          Assert(UpperCase(FSplitter[1]) = rsIDENTIFIERS, Format(
            rsBEGINIDENTIF2, [FLineIndex+1, FInputFileLines[FLineIndex]]));
          if FListingFile <> nil then
          begin
            FListingFile.Add(rsEndOfIDENTIF);
          end;
          InterpolateInTime;
        end
        else
        begin
          Assert(False, Format(rsMustStartWithObsnameOrEnd, [FLineIndex+1,
            FInputFileLines[FLineIndex]]));
        end;
      end;
  else Assert(False);
  end;
  case FIdStatus of
    isNone:
      begin
      end;
    isID:
      begin
        Assert(FSplitter.Count = 2, Format(rsMustBeTwo, [FLineIndex+1,
          FInputFileLines[FLineIndex]]));
        FID := FSplitter[1];
        if FListingFile <> nil then
        begin
          FListingFile.Add('');
          FListingFile.Add(Format(rsIDInObservat, [FID]));
        end;
      end;
    isLocation:
      begin
        Assert(FSplitter.Count = 3, Format(rsIMustBeThree, [FLineIndex+1,
          FInputFileLines[FLineIndex]]));
        LocationID.ID := FID;
        try
          LocationID.APoint.X := StrToFloat(FSplitter[1]);
          LocationID.APoint.Y := StrToFloat(FSplitter[2]);
        except on EConvertError do
          begin
            raise EInputException.Create(Format(rsErrorConvert, [FLineIndex+1,
              FInputFileLines[FLineIndex]]));
          end;
        end;
        AddLocationToDictionary(LocationID);
        if FListingFile <> nil then
        begin
          FListingFile.Add(Format(rsLocationInOb, [LocationID.APoint.X,
            LocationID.APoint.Y]));
        end;
      end;
    isTime:
      begin
        Assert(FSplitter.Count in [3, 4], Format(rsMustBeThreeOrFor, [FLineIndex
          +1, FInputFileLines[FLineIndex]]));
        DerivedObs := TDerivedObs.Create;
        DerivedObs.ID := FID;
        DerivedObs.Obsname := FSplitter[1];
        try
          DerivedObs.Time:= StrToFloat(FSplitter[2]);
        except on EConvertError do
          begin
            DerivedObs.Free;
            raise EInputException.Create(Format(rsErrorConvert2, [FLineIndex+1,
              FInputFileLines[FLineIndex]]));
          end;
        end;
        DerivedObs.TimeAssigned := True;
        if FSplitter.Count = 4 then
        begin
          Assert(UpperCase(FSplitter[3]) = rsPRINT, Format(rsIMustBePrint, [
            FLineIndex+1, FInputFileLines[FLineIndex]]));
          DerivedObs.Print := True;
        end
        else
        begin
          DerivedObs.Print := False;
        end;
        DerivedObs.Value := 0;
        FDerivedObsList.Add(DerivedObs);
        if FListingFile <> nil then
        begin
          FListingFile.Add(Format(rsObservationN, [DerivedObs.Obsname]));
          FListingFile.Add(Format(rsObservationT, [DerivedObs.Time]));
          if DerivedObs.Print then
          begin
            FListingFile.Add(rsPrintTrue);
          end
          else
          begin
            FListingFile.Add(rsPrintFalse);
          end;
        end;
      end;
  else Assert(False, rsProgrammingE);
  end;
end;

procedure TInputHandler.InterpolateInTime;
var
  ObsIndex: Integer;
  AnObs: TDerivedObs;
  ObsFile : TFileId;
  FirstValue: double;
  SecondValue: double;
  FirstTime: double;
  SecondTime: double;
begin
  if FListingFile <> nil then
  begin
    FListingFile.Add('');
    FListingFile.Add(rsBeginExtract);
  end;
  WriteLn(rsBeginExtract);
  FDerivedObsList.Sort;
  for ObsIndex := 0 to Pred(FDerivedObsList.Count) do
  begin
    AnObs := FDerivedObsList[ObsIndex];
    Assert(FObservationDictionary.TryGetValue(UpperCase(AnObs.ID), ObsFile),
      Format(rsTheObservati2, [AnObs.ID]));
    if  ObsFile.OutputFile.FirstTime < ObsFile.OutputFile.SecondTime then
    begin
      while AnObs.Time > ObsFile.OutputFile.SecondTime do
      begin
        ObsFile.OutputFile.ReadTimeAndValues;
        if ObsFile.OutputFile.FirstTime > ObsFile.OutputFile.SecondTime then
        begin
          break;
        end;
      end;
      if ObsFile.OutputFile.FirstTime > ObsFile.OutputFile.SecondTime then
      begin
        AnObs.Value := ObsFile.OutputFile.FirstValue[ObsFile.Position];
      end
      else
      begin
        FirstValue := ObsFile.OutputFile.FirstValue[ObsFile.Position];
        SecondValue := ObsFile.OutputFile.SecondValue[ObsFile.Position];
        FirstTime := ObsFile.OutputFile.FirstTime;
        SecondTime := ObsFile.OutputFile.SecondTime;
        AnObs.Value := FirstValue + (SecondValue-FirstValue)
          * (AnObs.Time - FirstTime)
          / (SecondTime - FirstTime);
      end;
      AnObs.TimeAssigned := True;
      AddObsToDictionary(AnObs);

      FParser.CreateVariable(AnObs.ObsName, '', AnObs.Value, AnObs.ObsName);
      PrintToOutputFile(AnObs);
      if FListingFile <> nil then
      begin
        FListingFile.Add(Format(rsObservationN2, [AnObs.Obsname]));
        FListingFile.Add(Format(rsObservationT2, [AnObs.Time]));
        FListingFile.Add(Format(rsObservationV, [AnObs.Value]));
        FListingFile.Add('');
      end;
    end;
  end;
  if FListingFile <> nil then
  begin
    FListingFile.Add('');
    FListingFile.Add(rsEndExtract);
  end;
end;

constructor TInputHandler.Create;
begin
  inherited;
  //FCurrentProcessStatus := psNone;
  FPriorProcessStatus := psNone;
  //FObsFileList := TOutputFileObjectList.Create;
  FObservationDictionary := TObservationDictionary.Create;

  FListingFileName := '';
  FOutputFileName := '';
  FInstructionFileName := '';
end;

destructor TInputHandler.Destroy;
begin
  if FListingFileName <> '' then
  begin
    FListingFile.SaveToFile(FListingFileName);
  end;
  if FOutputFileName <> '' then
  begin
    FOutputFile.SaveToFile(FOutputFileName);
  end;
  if FInstructionFileName <> '' then
  begin
    if FInstructionFileMode = ifmUCODE then
    begin
      FInstructionFile.Insert(1, Format(rsStandardFile,
        [FInstructionFile.Count-1]));
    end;
    FInstructionFile.SaveToFile(FInstructionFileName);
  end;
  FObservationDictionary.Free;
  //FObsFileList.Free;
  inherited Destroy;
end;

procedure TInputHandler.ReadAndProcessInputFile(const FileName: string);
var
  Index: Integer;
  ALine: String;
begin
  try
    try
      FInputFileLines.LoadFromFile(FileName);
      for Index := 0 to Pred(FInputFileLines.Count) do
      begin
        FLineIndex := Index;
        ALine := FInputFileLines[Index];
        ALine := Trim(ALine);
        if ALine = '' then
        begin
          Continue;
        end;
        if ALine[1] = '#' then
        begin
          Continue;
        end;
        FSplitter.DelimitedText := ALine;
        case FCurrentProcessStatus of
          psNone:
          begin
            Assert(FSplitter.Count = 2, Format(rsNotExactlyTwoItems,
              [FLineIndex+1, FInputFileLines[FLineIndex]]));
            Assert(UpperCase(FSplitter[0]) = rsBEGIN, Format('In line %0:d, "%1:s", the first word must be "BEGIN".', [FLineIndex+1, FInputFileLines[FLineIndex]]));
            case FPriorProcessStatus of
              psNone:
              begin
                Assert(UpperCase(FSplitter[1]) = rsOPTIONS);
                InitializeOptions;
              end;
              psOptions:
              begin
                Assert(UpperCase(FSplitter[1]) = rsOBSERVATION_Files);
                InitializeObsFiles;
              end;
              psObsFiles:
              begin
                Assert(UpperCase(FSplitter[1]) = rsIDENTIFIERS);
                InitializeIdentifiers;
              end;
              psIdentifiers, psDerivedObs:
              begin
                if UpperCase(FSplitter[1]) = rsDERIVED_OBSE then
                begin
                  InitializeDerivedObs;
                end
                else if UpperCase(FSplitter[1]) = rsOBSERVATION_Files then
                begin
                  InitializeObsFiles
                end
                else if UpperCase(FSplitter[1]) = rsIDENTIFIERS then
                begin
                  InitializeIdentifiers;
                end
                else
                begin
                  Assert(False);
                end;
              end;
              else
                Assert(False);
            end;
          end;
          psOptions:
          begin
            HandleOption;
          end;
          psObsFiles:
          begin
            HandleObservationFiles;
          end;
          psIdentifiers:
          begin
            HandleIdentifiers;
          end;
          psDerivedObs:
          begin
            HandleDerivedObs
          end;
          else
            Assert(False);
        end;
      end;
    except on E: Exception do
      begin
        if FListingFile <> nil then
        begin
          FListingFile.Add(E.Message);
        end;
        raise;
      end;
    end;
  finally
  end;
end;

end.

