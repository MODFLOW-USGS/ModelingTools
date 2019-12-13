unit InputFileReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Generics.Collections, Generics.Defaults,
  OutputFileReader, RbwParser;

type

  TProcessStatus = (psNone, psOptions, psObsFiles, psIdentifiers, psDerivedObs);
  TIdStatus = (isNone, isID, isLocation, isTime);

  TLocationID = record
    ID: string;
    X: double;
    Y: double;
  end;

  TLocationDictionary = specialize TDictionary<string, TLocationID>;

  TDerivedObs = class
    ID: string;
    Obsname: string;
    Time: double;
    Print: boolean;
    Value: double;
  end;

  { TDerivedObsCompare }

  TDerivedObsCompare = class(specialize TComparer<TDerivedObs>)
    function Compare(constref Left, Right: TDerivedObs): Integer; override;
  end;

  { TDerivedObsObjectList }

  TDerivedObsObjectList = class(specialize TObjectList<TDerivedObs>)
  public
    procedure Sort;
  end;

  TDerivedObsDictionary = specialize TDictionary<string, TDerivedObs>;

  { TInputHandler }

  TInputHandler = class(TObject)
  private
    FInputFileLines: TStringList;
    FCurrentProcessStatus: TProcessStatus;
    FPriorProcessStatus: TProcessStatus;
    FIdStatus: TIdStatus;
    FSplitter: TStringList;
    FOutputFile: TStringList;
    FOutputFileName: string;
    FLineIndex: Integer;
    FListingFileName: string;
    FListingFile: TStringList;
    FInstructionFileName: string;
    FInstructionFile: TStringList;
    FObsFileList: TOutputFileObjectList;
    FObservationDictionary: TObservationDictionary;
    FParser: TRbwParser;
    FID: string;
    FLocationDictionary: TLocationDictionary;
    FDerivedObsList: TDerivedObsObjectList;
    FDerivedObsDictionary: TDerivedObsDictionary;
    procedure HandleOption;
    procedure HandleObservationFiles;
    procedure ClearAllObservations;
    procedure InitializeIdentifiers;
    procedure InitializeObsFiles;
    procedure HandleIdentifiers;
    procedure InterpolateInTime;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadAndProcessInputFile(const FileName: string);
  end;

  EInputException = class(Exception);

implementation

resourceString
  rsBEGIN = 'BEGIN';
  rsOPTIONS = 'OPTIONS';
  rsOBSERVATION_Files = 'OBSERVATION_FILES';
  rsIDENTIFIERS = 'IDENTIFIERS';
  rsDERIVED_OBSE = 'DERIVED_OBSERVATIONS';

{ TDerivedObsCompare }

function TDerivedObsCompare.Compare(constref Left, Right: TDerivedObs
  ): Integer;
begin
  Result := Sign(Left.Time - Right.Time);
end;

{ TDerivedObsObjectList }

procedure TDerivedObsObjectList.Sort;
begin
  inherited Sort(TDerivedObsCompare.Create);
end;

{ TInputHandler }

procedure TInputHandler.HandleOption;

  procedure RecordOutputFile;
  begin
    if (FOutputFileName <> '') and (FListingFile <> nil) then
    begin
      FListingFile.Add(Format('Output file = %s', [FOutputFileName]));
    end;
  end;

  procedure RecordInstructionFile;
  begin
    if (FInstructionFileName <> '') and (FListingFile <> nil) then
    begin
      FListingFile.Add(Format('Instruction file = %s', [FInstructionFileName]));
    end;
  end;

begin
  Assert(FSplitter.Count = 2, Format('In line %0:d, "%1:s", there were not exactly two items listed.', [FLineIndex+1, FSplitter[FLineIndex]]));
  if UpperCase(FSplitter[0]) = 'LISTING' then
  begin
    Assert(FListingFileName = '', Format('The listing file was already set to "0:s".', [FListingFileName]));
    FListingFileName := FSplitter[1];
    FListingFile := TStringList.Create;
    FListingFile.Add('MODFLOW 6 Observation Extractor');
    FListingFile.Add('');
    FListingFile.Add(Format('Listing file = %s', [FListingFileName]));
    RecordOutputFile;
    RecordInstructionFile;
  end
  else if UpperCase(FSplitter[0]) = 'OUTPUT' then
  begin
    Assert(FOutputFileName = '', Format('The output file was already set to "0:s".', [FOutputFileName]));
    FOutputFileName := FSplitter[1];
    FOutputFile := TStringList.Create;
    RecordOutputFile;
  end
  else if UpperCase(FSplitter[0]) = 'INSTRUCTION' then
  begin
    Assert(FInstructionFileName = '', Format('The instruction file was already set to "0:s".', [FInstructionFileName]));
    FInstructionFileName := FSplitter[1];
    FInstructionFile := TStringList.Create;
    RecordInstructionFile;
  end
  else if UpperCase(FSplitter[0]) = 'END' then
  begin
    Assert(UpperCase(FSplitter[1]) = 'OPTIONS', Format('BEGIN OPTIONS must be paired with END OPTIONS in line %0:d, "%1:s".', [FLineIndex+1, FSplitter[FLineIndex]]));
    FCurrentProcessStatus := psNone;
    FPriorProcessStatus := psOptions;
  end
  else
  begin
    Assert(False, Format('Unrecognized option in line %0:d, ""%1:s.', [FLineIndex+1, FSplitter[FLineIndex]]));
  end;
end;

procedure TInputHandler.HandleObservationFiles;
var
  FileName: string;
  FileType: TFileType;
  ObsFile: TOutputFile;
begin
  Assert(FSplitter.Count in [2, 3], Format('In line %0:d, "%1:s", there must be exactly two or three items listed.', [FLineIndex+1, FSplitter[FLineIndex]]));
  if UpperCase(FSplitter[0]) = 'FILENAME' then
  begin
    FileName := FSplitter[1];
    Assert(FileExists(FileName), Format('The observation file "%s" does not exist', [FileName]));
    if FSplitter.Count = 3 then
    begin
      if UpperCase(FSplitter[2]) = 'BINARY' then
      begin
        FileType := ftBinary
      end
      else if UpperCase(FSplitter[2]) = 'TEXT' then
      begin
        FileType := ftText
      end
      else
      begin
        Assert(False, Format('The file format in line %0:d, "%1:s" must be "BINARY" or "TEXT".', [FLineIndex+1, FSplitter[FLineIndex]]));
      end;
    end
    else
    begin
      FileType := ftBinary
    end;
    ObsFile := TOutputFile.Create(FileName, FileType, FObservationDictionary);
    FObsFileList.Add(ObsFile);
  end
  else if UpperCase(FSplitter[0]) = 'END' then
  begin
    Assert(FSplitter.Count = 2, Format('In line %0:d, "%1:s", there must be exactly two items listed.', [FLineIndex+1, FSplitter[FLineIndex]]));
    Assert(UpperCase(FSplitter[1]) = 'OBSERVATION_FILES', Format('BEGIN OBSERVATION_FILES must be paired with END OBSERVATION_FILES in line %0:d, "%1:s".', [FLineIndex+1, FSplitter[FLineIndex]]));
    FCurrentProcessStatus := psNone;
    FPriorProcessStatus := psObsFiles;
  end
  else
  begin
    Assert(False, Format('Unrecognized option in line %0:d, ""%1:s.', [FLineIndex+1, FSplitter[FLineIndex]]));
  end;
end;

procedure TInputHandler.ClearAllObservations;
begin
  FParser.ClearExpressions;
  FParser.ClearVariables;
  FObsFileList.Clear;
  FObservationDictionary.Clear;
  FLocationDictionary.Clear;
  FDerivedObsList.Clear;
  FDerivedObsDictionary.Clear;
end;

procedure TInputHandler.InitializeIdentifiers;
begin
  FCurrentProcessStatus := psIdentifiers;
  FIdStatus := isNone;
end;

procedure TInputHandler.InitializeObsFiles;
begin
  FCurrentProcessStatus := psObsFiles;
  ClearAllObservations;
end;

procedure TInputHandler.HandleIdentifiers;
var
  LocationID: TLocationID;
  DerivedObs: TDerivedObs;
begin
  case FIdStatus of
    isNone:
      begin
        if UpperCase(FSplitter[0]) = 'ID' then
        begin
          FIdStatus := isID
        end
        else
        begin
          Assert(False, Format('In line %0:d, "%1:s" must start with "ID".', [FLineIndex+1, FSplitter[FLineIndex]]));
        end;
      end;
    isID:
      begin
        if UpperCase(FSplitter[0]) = 'LOCATION' then
        begin
          FIdStatus := isLocation
        end
        else if UpperCase(FSplitter[0]) = 'OBSNAME' then
        begin
          FIdStatus := isTime
        end
        else
        begin
          Assert(False, Format('In line %0:d, "%1:s" must start with "LOCATION" or "OBSNAME".', [FLineIndex+1, FSplitter[FLineIndex]]));
        end;
      end;
    isLocation:
      begin
        if UpperCase(FSplitter[0]) = 'OBSNAME' then
        begin
          FIdStatus := isTime
        end
        else
        begin
          Assert(False, Format('In line %0:d, "%1:s" must start with "OBSNAME".', [FLineIndex+1, FSplitter[FLineIndex]]));
        end;
      end;
    isTime:
      begin
        if UpperCase(FSplitter[0]) = 'OBSNAME' then
        begin
          FIdStatus := isTime
        end
        else if UpperCase(FSplitter[0]) = 'END' then
        begin
          FIdStatus := isNone;
          FPriorProcessStatus := psIdentifiers;
          FCurrentProcessStatus := psNone;
          Assert(FSplitter.Count = 2, Format('In line %0:d, "%1:s", there must be exactly two items listed.', [FLineIndex+1, FSplitter[FLineIndex]]));
          Assert(UpperCase(FSplitter[1]) = 'IDENTIFIERS', Format('BEGIN IDENTIFIERS must be paired with END IDENTIFIERS in line %0:d, "%1:s".', [FLineIndex+1, FSplitter[FLineIndex]]));
        end
        else
        begin
          Assert(False, Format('In line %0:d, "%1:s" must start with "OBSNAME" or "END".', [FLineIndex+1, FSplitter[FLineIndex]]));
        end;
      end;
  else Assert(False);
  end;
  case FIdStatus of
    isNone:
      begin
        InterpolateInTime;
      end;
    isID:
      begin
        Assert(FSplitter.Count = 2, Format('In line %0:d, "%1:s", there must be exactly two items listed.', [FLineIndex+1, FSplitter[FLineIndex]]));
        FID := FSplitter[1];
      end;
    isLocation:
      begin
        Assert(FSplitter.Count = 3, Format('In line %0:d, "%1:s", there must be exactly three items listed.', [FLineIndex+1, FSplitter[FLineIndex]]));
        LocationID.ID := FID;
        try
          LocationID.X := StrToFloat(FSplitter[1]);
          LocationID.Y := StrToFloat(FSplitter[2]);
        except on EConvertError do
          begin
            raise EInputException.Create(Format('Error converting X or Y coordinate on line %0:d, "%1:s".', [FLineIndex+1, FSplitter[FLineIndex]]));
          end;
        end;
        FLocationDictionary.Add(LocationID.ID, LocationID);
      end;
    isTime:
      begin
        Assert(FSplitter.Count in [3, 4], Format('In line %0:d, "%1:s", there must be exactly three or four items listed.', [FLineIndex+1, FSplitter[FLineIndex]]));
        DerivedObs := TDerivedObs.Create;
        DerivedObs.ID := FID;
        DerivedObs.Obsname := FSplitter[1];
        try
          DerivedObs.Time:= StrToFloat(FSplitter[2]);
        except on EConvertError do
          begin
            DerivedObs.Free;
            raise EInputException.Create(Format('Error converting time on line %0:d, "%1:s".', [FLineIndex+1, FSplitter[FLineIndex]]));
          end;
        end;
        if FSplitter.Count = 4 then
        begin
          Assert(UpperCase(FSplitter[3]) = 'PRINT', Format('In line %0:d, "%1:s", the fourth item, if present, must be "PRINT".', [FLineIndex+1, FSplitter[FLineIndex]]));
          DerivedObs.Print := True;
        end
        else
        begin
          DerivedObs.Print := False;
        end;
        DerivedObs.Value := 0;
        FDerivedObsList.Add(DerivedObs);
      end;
  else Assert(False, 'Programming error in TInputHandler.HandleIdentifiers');
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
  FDerivedObsList.Sort;
  for ObsIndex := 0 to Pred(FDerivedObsList.Count) do
  begin
    AnObs := FDerivedObsList[ObsIndex];
    Assert(FObservationDictionary.TryGetValue(UpperCase(AnObs.ID), ObsFile), Format('The observation %s is not found in any of the observation output files.', [AnObs.ID]));
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
      FDerivedObsDictionary.Add(UpperCase(AnObs.Obsname), AnObs);
    end;
  end;
end;

constructor TInputHandler.Create;
begin
  FCurrentProcessStatus := psNone;
  FPriorProcessStatus := psNone;
  FInputFileLines := TStringList.Create;
  FSplitter := TStringList.Create;
  FObsFileList := TOutputFileObjectList.Create;
  FObservationDictionary := TObservationDictionary.Create;
  FParser := TRbwParser.Create(nil);
  FLocationDictionary := TLocationDictionary.Create;
  FDerivedObsList := TDerivedObsObjectList.Create;
  FDerivedObsDictionary := TDerivedObsDictionary.Create;

  FListingFileName := '';
  FOutputFileName := '';
  FInstructionFileName := '';
end;

destructor TInputHandler.Destroy;
begin
  FLocationDictionary.Free;
  FParser.Free;
  FObservationDictionary.Free;
  FObsFileList.Free;
  FInstructionFile.Free;
  FListingFile.Free;
  FOutputFile.Free;
  FSplitter.Free;
  FInputFileLines.Free;
  inherited Destroy;
end;

procedure TInputHandler.ReadAndProcessInputFile(const FileName: string);
var
  Index: Integer;
  ALine: String;
begin
  try
    FInputFileLines.LoadFromFile(FileName);
    for Index := 0 to Pred(FInputFileLines.Count) do
    begin
      FLineIndex := Index;
      ALine := Trim(FInputFileLines[Index]);
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
          Assert(FSplitter.Count = 2);
          Assert(UpperCase(FSplitter[0]) = rsBEGIN);
          case FPriorProcessStatus of
            psNone:
            begin
              Assert(UpperCase(FSplitter[1]) = rsOPTIONS);
              FCurrentProcessStatus := psOptions;
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
                FCurrentProcessStatus := psDerivedObs;
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

        end;
        else
          Assert(False);
      end;
    end;
  finally
  end;
end;

end.
psOptions, psObsFiles, psIdentifiers, psDerivedObs
