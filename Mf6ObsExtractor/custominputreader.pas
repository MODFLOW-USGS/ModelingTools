unit CustomInputReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Generics.Collections, Generics.Defaults, RbwParser,
  FastGeo, CustomOutputFileReader;

type
  TProcessStatus = (psNone, psOptions, psObsFiles, psIdentifiers, psDerivedObs);
  TIdStatus = (isNone, isID, isLocation, isTime);
  TDerivedObsStatus = (dosNone, dosObsName, dosInterpolate, dosFormula);
  TInstructionFileMode = (ifmPest, ifmUCODE);

  TLocationID = record
    ID: string;
    APoint: TPoint2D;
  end;

  TLocationList = specialize TList<TLocationID>;
  TLocationDictionary = specialize TDictionary<string, TLocationID>;

  TDerivedObs = class
    ID: string;
    Obsname: string;
    Time: double;
    Print: boolean;
    Value: double;
    TimeAssigned: Boolean;
  end;

  TDerivedObsList = specialize TList<TDerivedObs>;

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

  { TCustomInputHandler }

  TCustomInputHandler = class(TObject)
  private
    FObsName: string;
    FPrint: Boolean;
    FLocationDictionary: TLocationDictionary;
    FDerivedObsDictionary: TDerivedObsDictionary;
    FPriorDerivedObsStatus: TDerivedObsStatus;
    FInstructionFileMode: TInstructionFileMode;
    FOutputFile: TStringList;
    FInstructionFile: TStringList;
    FIdentifiersRead: Boolean;
    FParser: TRbwParser;
    FObsFileList: TOutputFileObjectList;
    FOutputFileName: string;
    FInstructionFileName: string;
    FListingFileName: string;
    procedure AssignInactiveObs(var NewLocation: TLocationID);
    procedure RecordObs(const AnObs: TDerivedObs);
    procedure InterpOnePoint(DerivedObs: TDerivedObs; NewLocation: TLocationID);
    procedure InterpTwoPoints(DerivedObss: TDerivedObsList;
      Locations: TLocationList; NewLocation: TLocationID);
    procedure InterpThreePoints(DerivedObservations: TDerivedObsList;
      Locations: TLocationList; NewLocation: TLocationID);
    procedure InterpFourPoints(DerivedObservations: TDerivedObsList;
      Locations: TLocationList; NewLocation: TLocationID);
    procedure ClearFileObs;
    procedure ClearAllObservations;
    procedure AddObsToDictionary(AnObs: TDerivedObs);
    procedure PrintToOutputFile(const AnObs: TDerivedObs);
  protected
    FObservationDictionary: TObservationDictionary;
    FIdStatus: TIdStatus;
    FSplitter: TStringList;
    FLineIndex: Integer;
    FInputFileLines: TStringList;
    FPriorProcessStatus: TProcessStatus;
    FCurrentProcessStatus: TProcessStatus;
    FListingFile: TStringList;
    FDerivedObsList: TDerivedObsObjectList;
    function CreateObsFile(const FileType: TFileType;
      const FileName: string): TCustomOutputFile; virtual; abstract;
    function ApplicationTitle: string; virtual; abstract;
    procedure InterpolateInTime;
    procedure AddLocationToDictionary(Location: TLocationID);
    procedure InitializeOptions;
    procedure InitializeObsFiles;
    procedure InitializeIdentifiers;
    procedure InitializeDerivedObs;
    procedure HandleOption;
    procedure HandleObservationFiles;
    procedure HandleDerivedObs;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  EInputException = class(Exception);

resourcestring
  rsExactlyTwoOrThree = 'In line %0:d, "%1:s", there must be exactly two or '
    +'three items listed.';
  rsEND = 'END';
  rsOBSNAME = 'OBSNAME';
  rsPRINT = 'PRINT';
  rsPrintTrue = '  Print = True';
  rsPrintFalse = '  Print = False';
  rsDERIVED_OBSE = 'DERIVED_OBSERVATIONS';

implementation

uses SubPolygonUnit, BasisFunctionUnit;

const
  InactiveValue = -1E30;
  Epsilon = 1E-6;

resourcestring
  rsTheIdentifie =
    'The identifier "%s" is a duplicate of an earlier identifier';
  rsTheIdentifie2 = 'The identifier "%s" is a duplicate of an earlier '
    +'oservation name';
  rsAssignedValu = '  assigned value = (%0:g)';
  rsPrintObsValue = '%0:s %1:g';
  rsPrintPestObsInstruction = 'l1 @%0:s@ !%0:s!';
  rsBeginDERIVED = 'Begin DERIVED_OBSERVATIONS Block';
  rsProcessingDE = 'Processing DERIVED_OBSERVATIONS Block';
  rsSecondIdentifiers = 'In line %0:d, "%1:s", a second IDENTIFIERS block was '
    +'started without being preceeded by an OBSERVATION_FILES block.';
  rsBeginIDENTIF = 'Begin IDENTIFIERS Block';
  rsProcessingID = 'Processing IDENTIFIERS Block';
  rsRemovingAllO = 'Removing all observations from memory.';
  rsBeginOPTIONS3 = 'Processing OPTIONS Block';
  rsNoLocation = 'In line %0:d, "%1:s", there is no location associated with %'
    +'2:s.';
  rsNoLocationForObs = 'In line %0:d, "%1:s", there is no observation '
    +'associated with %2:s.';
  rsINTERPOLATE = 'INTERPOLATE';
  rsFORMULA = 'FORMULA';
  rsLineMustStartObs = 'In line %0:d, "%1:s", the line does not begin with "'
    +'OBSNAME".';
  rsNoFormula = 'In line %0:d, "%1:s", the line does not begin with "FORMULA" '
    +'OR "INTERPOLATE".';
  rsUnmatchedEndDerObs = 'In line %0:d, "%1:s", the DERIVED_OBSERVATIONS block'
    +' must end with "END DERIVED_OBSERVATIONS".';
  rsEndDERIVED_O = 'End DERIVED_OBSERVATIONS Block';
  rsMustBePrint = 'In line %0:d, "%1:s", if a third item is listed, it must be'
    +' "PRINT".';
  rsObservationN3 = '  Observation name = %s';
  rsValueAssigne = '  Value assigned by spatial interpolation';
  rsMustBe4to7 = 'In line %0:d, "%1:s", there must be from four to seven items'
    +' listed.';
  rsErrorConvert = 'Error converting X or Y coordinate on line %0:d, "%1:s".';
  rsLocation0G1G = '  Location = (%0:g, %1:g)';
  rsObservation0 = '    Observation %0:s = %1:g';
  rsLocatedAt0G1 = '    Located at ( %0:g, %1:g';
  rsWARNINGBecau = 'WARNING: Because Observation %0:s = %1:g, it will be '
    +'skipped.';
  rsProgrammingE5 = 'Programming error in TInputHandler.HandleDerivedObs';
  rsValueAssigne2 = '  Value assigned by formula';
  rsFormulaS = '  Formula = %s';
  rsFormulaEvalu = '  Formula evaluated as = %s';
  rsInvalidFormula = 'In line %0:d, "%1:s", The formula "%2:s" is invalid '
    +'because it doesn''t evaluate to a number.';
  rsInvalidFormula2 = 'In line %0:d, "%1:s", the formula "%2:s" is invalid. The'
    +' error message is "%3:s".';
  rsErrorInHandleDerivedObs = 'In line %0:d, "%1:s", Error in TInputHandler.'
    +'HandleDerivedObs';
  rsProgrammingE2 = 'Programming error in TInputHandler.InterpTwoPoints';
  rsDistanceZero = 'In line %0:d, "%1:s", the distance between %2:s and %3:s '
    +'is zero.';
  rsProgrammingE3 = 'Programming error in TInputHandler.InterpThreePoints';
  rsProgrammingE4 = 'Programming error in TInputHandler.InterpFourPoints';
  rsFILENAME = 'FILENAME';
  rsTheObservati = 'The observation file "%s" does not exist';
  rsBINARY = 'BINARY';
  rsTEXT = 'TEXT';
  rsTheFileForma = 'The file format in line %0:d, "%1:s" must be "BINARY" or "'
    +'TEXT".';
  rsObservationF = '  Observation file = %s';
  rsFormatTextFi = '  Format = text file';
  rsFormatBinary = '  Format = binary file';
  rsMustBeTwo = 'In line %0:d, "%1:s", there must be exactly two items listed.';
  rsBEGINOBSERVA = 'BEGIN OBSERVATION_FILES must be paired with END '
    +'OBSERVATION_FILES in line %0:d, "%1:s".';
  rsEndOfOBSERVA = 'End of OBSERVATION_FILES Block';
  rsOBSERVATION_Files = 'OBSERVATION_FILES';
  rsUnrecognized = 'Unrecognized option in line %0:d, ""%1:s.';
  rsOutputFileS = '  Output file = %s';
  rsInstructionF = '  Instruction file = %s';
  rsLISTING = 'LISTING';
  rsNotExactlyTwoItems = 'In line %0:d, "%1:s", there were not exactly two '
    +'items listed.';
  rsTheListingFi = 'The listing file was already set to "0:s".';
  rsBeginOPTIONS = 'Begin OPTIONS block';
  rsListingFileS = '  Listing file = %s';
  rsOUTPUT = 'OUTPUT';
  rsTheOutputFil = 'The output file was already set to "0:s".';
  rsINSTRUCTION = 'INSTRUCTION';
  rsTheInstructi = 'The instruction file was already set to "0:s".';
  rsPestOrUcode = 'In line %0:d, "%1:s", the third item must be either "PEST" '
    +'or "UCODE".';
  rsOPTIONS = 'OPTIONS';
  rsBEGINOPTIONS2 = 'BEGIN OPTIONS must be paired with END OPTIONS in line %0:'
    +'d, "%1:s".';
  rsEndOfOPTIONS = 'End of OPTIONS block';
  rsStandardFile = 'StandardFile, 0, 2, %d';
  rsBeginExtract =
    'Begin extracting observation values interpolated in time.';
  rsTheObservati2 = 'The observation %s is not found in any of the observation'
    +' output files.';
  rsObservationN2 = '  Observation Name = %s';
  rsObservationT2 = '  Observation time = %g';
  rsObservationV = '  Observation Value = %g';
  rsEndExtract =
    'End of extracting observation values interpolated in time.';
  rsBeginOBSERVA2 = 'Begin OBSERVATION_FILES Block';
  rsProcessingOB = 'Processing OBSERVATION_FILES Block';

  function NearlyTheSame(A, B, Epsilon: double): boolean;
  begin
    result := (A = B)
      or (Abs(A-B)/Abs(A + B) < Epsilon)
  end;

{ TCustomInputHandler }

procedure TCustomInputHandler.AssignInactiveObs(var NewLocation: TLocationID);
var
  AnObs: TDerivedObs;
begin
  AnObs := TDerivedObs.Create;
  AnObs.ID := '';
  AnObs.Obsname := FObsName;
  AnObs.Print := FPrint;
  AnObs.Value := InactiveValue;
  AnObs.Time := 0;
  AnObs.TimeAssigned := False;
  AddLocationToDictionary(NewLocation);
  RecordObs(AnObs);
end;

procedure TCustomInputHandler.RecordObs(const AnObs: TDerivedObs);
begin
  AddObsToDictionary(AnObs);

  FDerivedObsList.Add(AnObs);
  FParser.CreateVariable(AnObs.ObsName, '', AnObs.Value, AnObs.ObsName);
  PrintToOutputFile(AnObs);
  if FListingFile <> nil then
  begin
    FListingFile.Add(Format(rsAssignedValu, [AnObs.Value]));
  end;
end;

procedure TCustomInputHandler.AddLocationToDictionary(Location: TLocationID);
begin
  try
    FLocationDictionary.Add(UpperCase(Location.ID), Location);

  except on EListError do
    begin
      Assert(False, Format(rsTheIdentifie, [Location.ID]));
    end;
  end;
end;

procedure TCustomInputHandler.AddObsToDictionary(AnObs: TDerivedObs);
begin
  try
    FDerivedObsDictionary.Add(UpperCase(AnObs.Obsname), AnObs);

  except on EListError do
    begin
      Assert(False, Format(rsTheIdentifie2, [AnObs.Obsname]));
    end;

  end;
end;

procedure TCustomInputHandler.PrintToOutputFile(const AnObs: TDerivedObs);
begin
  if AnObs.Print then
  begin
    FOutputFile.Add(Format(rsPrintObsValue, [AnObs.ObsName, AnObs.Value]));
    if FInstructionFile <> nil then
    begin
      case FInstructionFileMode of
        ifmPest:
          begin
            FInstructionFile.Add(Format(rsPrintPestObsInstruction, [AnObs.ObsName]));
          end;
        ifmUCODE:
          begin
            FInstructionFile.Add(AnObs.ObsName);
          end;
      end;
    end;
  end;
end;

procedure TCustomInputHandler.InitializeDerivedObs;
begin
  FCurrentProcessStatus := psDerivedObs;
  FPriorDerivedObsStatus := dosNone;
  if FListingFile <> nil then
  begin
    FListingFile.Add(rsBeginDERIVED);
  end;
  WriteLn(rsProcessingDE);
end;

procedure TCustomInputHandler.InitializeIdentifiers;
begin
  Assert(not FIdentifiersRead, Format(rsSecondIdentifiers, [FLineIndex+1,
    FInputFileLines[FLineIndex]]));
  FCurrentProcessStatus := psIdentifiers;
  FIdStatus := isNone;
  FIdentifiersRead := True;

  if FListingFile <> nil then
  begin
    FListingFile.Add(rsBeginIDENTIF);
  end;
  WriteLn(rsProcessingID);
end;

procedure TCustomInputHandler.ClearAllObservations;
begin
  if FListingFile <> nil then
  begin
    FListingFile.Add('');
    FListingFile.Add(rsRemovingAllO);
    FListingFile.Add('');
  end;
  FParser.ClearExpressions;
  FParser.ClearVariables;
  ClearFileObs;
  FLocationDictionary.Clear;
  FDerivedObsList.Clear;
  FDerivedObsDictionary.Clear;
end;

procedure TCustomInputHandler.InitializeOptions;
begin
  FCurrentProcessStatus := psOptions;
  WriteLn(rsBeginOPTIONS3);
end;

procedure TCustomInputHandler.HandleDerivedObs;
var
  CurrentStatus: TDerivedObsStatus;
  DerivedObs: TDerivedObs;
  NewLocation: TLocationID;
  LocationList: TLocationList;
  DerivedObsList: TDerivedObsList;
  LocationIndex: Integer;
  ALine: String;
  Formula: string;
  AnObs: TDerivedObs;
  ALocation: TLocationID;
  WarningMessage: string;
  function GetLocation(DerivedObs1: TDerivedObs): TLocationID;
  var
    ALocationID: TLocationID;
  begin
    if FLocationDictionary.TryGetValue(UpperCase(DerivedObs1.ID), ALocationID)then
    begin
      result := ALocationID
    end
    else if FLocationDictionary.TryGetValue(UpperCase(DerivedObs1.ID), ALocationID)then
    begin
      result := ALocationID
    end
    else
    begin
      Assert(False, Format(rsNoLocation, [FLineIndex+1, FInputFileLines[
        FLineIndex], DerivedObs1.ID]));
    end;
  end;
  function GetObs(ObsName: string): TDerivedObs;
  var
    ADerivedObs: TDerivedObs;
  begin
    if FDerivedObsDictionary.TryGetValue(UpperCase(ObsName), ADerivedObs) then
    begin
      result := ADerivedObs;
    end
    else
    begin
      Assert(False, Format(rsNoLocationForObs, [FLineIndex+1, FInputFileLines[
        FLineIndex], ObsName]));
    end;
  end;

begin
  LocationList := TLocationList.Create;
  DerivedObsList := TDerivedObsList.Create;
  try
    case FPriorDerivedObsStatus of
      dosNone:
      begin
        if UpperCase(FSplitter[0]) = rsOBSNAME then
        begin
          CurrentStatus := dosObsName;
        end
        else
        begin
          Assert(False, Format(rsLineMustStartObs, [FLineIndex+1,
            FInputFileLines[FLineIndex]]));
        end;
      end;
      dosObsName:
      begin
        if UpperCase(FSplitter[0]) = rsINTERPOLATE then
        begin
          CurrentStatus := dosInterpolate;
        end
        else if UpperCase(FSplitter[0]) = rsFORMULA then
        begin
          CurrentStatus := dosFormula;
        end
        else
        begin
          Assert(False, Format(rsNoFormula, [FLineIndex+1, FInputFileLines[
            FLineIndex]]));
        end;
      end;
      dosInterpolate, dosFormula:
      begin
        if UpperCase(FSplitter[0]) = rsEND then
        begin
          Assert(UpperCase(FSplitter[1]) = rsDERIVED_OBSE,
            Format(rsUnmatchedEndDerObs, [FLineIndex+1,
            FInputFileLines[FLineIndex]]));
          CurrentStatus := dosNone;
          FCurrentProcessStatus := psNone;
          if FListingFile <> nil then
          begin
            FListingFile.Add(rsEndDERIVED_O);
          end;
        end
        else
        begin
          CurrentStatus := dosObsName
        end;
      end;
    else Assert(False);
    end;
    FPriorDerivedObsStatus := CurrentStatus;
    case CurrentStatus of
      dosNone:
        begin
        end;
      dosObsName:
        begin
          Assert(FSplitter.Count in [2,3], Format(rsExactlyTwoOrThree,
            [FLineIndex+1, FInputFileLines[FLineIndex]]));
          FObsName := FSplitter[1];
          if FSplitter.Count = 2 then
          begin
            FPrint := False;
          end
          else if UpperCase(FSplitter[2]) = rsPRINT then
          begin
            FPrint := True;
          end
          else
          begin
            Assert(False, Format(rsMustBePrint, [FLineIndex+1, FInputFileLines[
              FLineIndex]]));
          end;
          if FListingFile <> nil then
          begin
            FListingFile.Add('');
            FListingFile.Add(Format(rsObservationN3, [FObsName]));
            if FPrint then
            begin
              FListingFile.Add(rsPrintTrue);
            end
            else
            begin
              FListingFile.Add(rsPrintFalse);
            end;
          end;
        end;
      dosInterpolate:
        begin
          if FListingFile <> nil then
          begin
            FListingFile.Add(rsValueAssigne);
          end;

          NewLocation.ID := FObsName;
          Assert(FSplitter.Count in [4..7], Format(rsMustBe4to7, [FLineIndex+1,
            FInputFileLines[FLineIndex]]));
          try
            NewLocation.APoint.X := StrToFloat(FSplitter[1]);
            NewLocation.APoint.Y := StrToFloat(FSplitter[2]);
          except on EConvertError do
            begin
              raise EInputException.Create(Format(rsErrorConvert, [FLineIndex+1,
                FInputFileLines[FLineIndex]]));
            end;
          end;
          if FListingFile <> nil then
          begin
            FListingFile.Add(Format(rsLocation0G1G, [NewLocation.APoint.X,
              NewLocation.APoint.Y]));
          end;
          for LocationIndex := 3 to Pred(FSplitter.Count) do
          begin
            DerivedObs := GetObs(UpperCase(FSplitter[LocationIndex]));
            if FListingFile <> nil then
            begin
              FListingFile.Add(Format(rsObservation0, [DerivedObs.ObsName,
                DerivedObs.Value]))
            end;
            if not NearlyTheSame(DerivedObs.Value, InactiveValue, Epsilon) then
            begin
              DerivedObsList.Add(DerivedObs);
              ALocation := GetLocation(DerivedObs);
              if FListingFile <> nil then
              begin
                FListingFile.Add(Format(rsLocatedAt0G1, [ALocation.APoint.X,
                  ALocation.APoint.Y]))
              end;
              LocationList.Add(ALocation);
            end
            else
            begin
              WarningMessage := Format(rsWARNINGBecau, [DerivedObs.ObsName,
                DerivedObs.Value]);
              WriteLn(WarningMessage);
              if FListingFile <> nil then
              begin
                FListingFile.Add(WarningMessage);
              end;
            end;
          end;
          case DerivedObsList.Count of
            0:
              begin
                AssignInactiveObs(NewLocation);
              end;
            1:
              begin
                InterpOnePoint(DerivedObsList[0], NewLocation);
              end;
            2:
              begin
                InterpTwoPoints(DerivedObsList, LocationList, NewLocation);
              end;
            3:
              begin
                InterpThreePoints(DerivedObsList, LocationList, NewLocation);
              end;
            4:
              begin
                InterpFourPoints(DerivedObsList, LocationList, NewLocation);
              end;
            else Assert(False, rsProgrammingE5);
          end;
          if FListingFile <> nil then
          begin
          end;
        end;
      dosFormula:
        begin
          if FListingFile <> nil then
          begin
            FListingFile.Add(rsValueAssigne2);
          end;
          ALine := FInputFileLines[FLineIndex];
          ALine := Trim(ALine);
          Formula := Copy(ALine, Length(rsFORMULA)+1, MAXINT);
          Formula := Trim(Formula);
          if FListingFile <> nil then
          begin
            FListingFile.Add(Format(rsFormulaS, [Formula]));
          end;
          try
            FParser.Compile(Formula);
            FListingFile.Add(Format(rsFormulaEvalu, [Formula]));
            if not (FParser.CurrentExpression.ResultType in
              [rdtDouble, rdtInteger]) then
            begin
              Assert(False, Format(rsInvalidFormula, [FLineIndex+1,
                FInputFileLines[FLineIndex], Formula]));
            end;

            AnObs := TDerivedObs.Create;
            AnObs.ID := '';
            AnObs.Obsname := FObsName;
            AnObs.Print := FPrint;
            FParser.CurrentExpression.Evaluate;
            AnObs.Value := FParser.CurrentExpression.DoubleResult;
            AnObs.Time := 0;
            AnObs.TimeAssigned := False;
            RecordObs(AnObs);
          except on E: ERbwParserError do
            begin
              raise EInputException.Create(Format(rsInvalidFormula2, [FLineIndex+
                1, FInputFileLines[FLineIndex], Formula, E.Message]));
            end;
          end;
        end;
    else
      Assert(False, Format(rsErrorInHandleDerivedObs, [FLineIndex+1,
        FInputFileLines[FLineIndex], Formula]));
    end;

  finally
    LocationList.Free;
    DerivedObsList.Free;
  end;
end;

procedure TCustomInputHandler.HandleObservationFiles;
var
  FileName: string;
  FileType: TFileType;
  ObsFile: TCustomOutputFile;
begin
  Assert(FSplitter.Count in [2, 3], Format(rsExactlyTwoOrThree, [FLineIndex+1,
    FInputFileLines[FLineIndex]]));
  if UpperCase(FSplitter[0]) = rsFILENAME then
  begin
    FileName := FSplitter[1];
    Assert(FileExists(FileName), Format(rsTheObservati, [FileName]));
    if FSplitter.Count = 3 then
    begin
      if UpperCase(FSplitter[2]) = rsBINARY then
      begin
        FileType := ftBinary
      end
      else if UpperCase(FSplitter[2]) = rsTEXT then
      begin
        FileType := ftText
      end
      else
      begin
        Assert(False, Format(rsTheFileForma, [FLineIndex+1,
          FInputFileLines[FLineIndex]]));
      end;
    end
    else
    begin
      FileType := ftBinary
    end;
    if FListingFile <> nil then
    begin
      FListingFile.Add(Format(rsObservationF, [FileName]));
      if FileType  = ftText then
      begin
        FListingFile.Add(rsFormatTextFi);
      end
      else
      begin
        FListingFile.Add(rsFormatBinary);
      end;
      FListingFile.Add('');
    end;
    ObsFile := CreateObsFile(FileType, FileName);
    FObsFileList.Add(ObsFile);
  end
  else if UpperCase(FSplitter[0]) = rsEND then
  begin
    Assert(FSplitter.Count = 2, Format(rsMustBeTwo, [FLineIndex+1,
      FInputFileLines[FLineIndex]]));
    Assert(UpperCase(FSplitter[1]) = rsOBSERVATION_Files, Format(
      rsBEGINOBSERVA, [FLineIndex+1, FInputFileLines[FLineIndex]]));
    FCurrentProcessStatus := psNone;
    FPriorProcessStatus := psObsFiles;
    if FListingFile <> nil then
    begin
      FListingFile.Add(rsEndOfOBSERVA);
      FListingFile.Add('');
    end;
  end
  else
  begin
    Assert(False, Format(rsUnrecognized, [FLineIndex+1, FInputFileLines[
      FLineIndex]]));
  end;
end;

procedure TCustomInputHandler.HandleOption;
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
    FListingFile.Add(ApplicationTitle);
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

procedure TCustomInputHandler.InterpolateInTime;
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

procedure TCustomInputHandler.InitializeObsFiles;
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

procedure TCustomInputHandler.InterpOnePoint(DerivedObs: TDerivedObs;
  NewLocation: TLocationID);
var
  AnObs: TDerivedObs;
begin
  AnObs := TDerivedObs.Create;
  AnObs.ID := '';
  AnObs.Obsname := FObsName;
  AnObs.Print := FPrint;
  AnObs.Value := DerivedObs.Value;
  AnObs.Time := DerivedObs.Time;
  AnObs.TimeAssigned := DerivedObs.TimeAssigned;
  AddLocationToDictionary(NewLocation);
  RecordObs(AnObs);
end;

procedure TCustomInputHandler.InterpTwoPoints(DerivedObss: TDerivedObsList;
  Locations: TLocationList; NewLocation: TLocationID);
var
  DerivedObs1: TDerivedObs;
  Location1: TLocationID;
  DerivedObs2: TDerivedObs;
  Location2: TLocationID;
  ClosestPoint: TPoint2D;
  SegmentLength: double;
  PointDistance: double;
  AnObs: TDerivedObs;
begin
  Assert(DerivedObss.Count = 2, rsProgrammingE2);
  Assert(Locations.Count = 2, rsProgrammingE2);
  DerivedObs1 := DerivedObss[0];
  DerivedObs2 := DerivedObss[1];
  Location1 := Locations[0];
  Location2 := Locations[1];
  ClosestPoint := ClosestPointOnSegmentFromPoint(
    EquateSegment(Location1.APoint, Location2.APoint),
    NewLocation.APoint);
  SegmentLength := Distance(Location1.APoint, Location2.APoint);
  if SegmentLength > 0 then
  begin
    PointDistance := Distance(Location1.APoint, ClosestPoint);

    AnObs := TDerivedObs.Create;
    AnObs.ID := '';
    AnObs.Obsname := FObsName;
    AnObs.Print := FPrint;
    AnObs.Time := DerivedObs1.Time;
    AnObs.TimeAssigned := DerivedObs1.TimeAssigned and DerivedObs2.TimeAssigned
      and (DerivedObs1.Time = DerivedObs2.Time);

    AnObs.Value := DerivedObs1.Value
      + (DerivedObs2.Value - DerivedObs1.Value)
      * PointDistance/SegmentLength;

    AddLocationToDictionary(NewLocation);
    RecordObs(AnObs);
  end
  else
  begin
    Assert(False, Format(rsDistanceZero, [FLineIndex+1, FInputFileLines[
      FLineIndex], DerivedObs1.Obsname, DerivedObs2.Obsname]));
  end;
end;

procedure TCustomInputHandler.InterpThreePoints(
  DerivedObservations: TDerivedObsList; Locations: TLocationList;
  NewLocation: TLocationID);
var
  DerivedObs1: TDerivedObs;
  Location1: TLocationID;
  DerivedObs2: TDerivedObs;
  Location2: TLocationID;
  DerivedObs3: TDerivedObs;
  Location3: TLocationID;
  ClosestPoint: TPoint2D;
  AnObs: TDerivedObs;
  Corners: TTriangularElement;
  Triangle: TSimplePolygon;
  NodeValues: TTriangularNodeValues;
  Distance1: double;
  Distance2: double;
  Distance3: double;
begin
  Assert(DerivedObservations.Count = 3, rsProgrammingE3);
  Assert(Locations.Count = 3, rsProgrammingE3);
  DerivedObs1 := DerivedObservations[0];
  DerivedObs2 := DerivedObservations[1];
  DerivedObs3 := DerivedObservations[2];
  Location1 := Locations[0];
  Location2 := Locations[1];
  Location3 := Locations[2];
  SetLength(Corners, 4);
  Corners[0] := Location1.APoint;
  Corners[2] := Location2.APoint;
  Corners[3] := Location3.APoint;
  Corners[4] := Location1.APoint;
  Triangle := TSimplePolygon.Create(Corners);
  try
    if Triangle.PointInside(NewLocation.APoint) then
    begin
      AnObs := TDerivedObs.Create;
      AnObs.ID := '';
      AnObs.Obsname := FObsName;
      AnObs.Print := FPrint;
      AnObs.Time := DerivedObs1.Time;
      AnObs.TimeAssigned := DerivedObs1.TimeAssigned
        and DerivedObs2.TimeAssigned and DerivedObs3.TimeAssigned
        and (DerivedObs1.Time = DerivedObs2.Time)
        and (DerivedObs1.Time = DerivedObs3.Time);

      SetLength(Corners, 3);
      SetLength(NodeValues, 3);
      NodeValues[0] := DerivedObs1.Value;
      NodeValues[1] := DerivedObs2.Value;
      NodeValues[2] := DerivedObs3.Value;
      AnObs.Value := TriangularBasisFunction(Corners, NodeValues, NewLocation.APoint);

      AddLocationToDictionary(NewLocation);

      RecordObs(AnObs);
  end
    else
    begin
      ClosestPoint := ClosestPointOnSegmentFromPoint(
        EquateSegment(Location1.APoint, Location2.APoint),
        NewLocation.APoint);
      Distance1 := Distance(ClosestPoint, NewLocation.APoint);
      ClosestPoint := ClosestPointOnSegmentFromPoint(
        EquateSegment(Location1.APoint, Location3.APoint),
        NewLocation.APoint);
      Distance2 := Distance(ClosestPoint, NewLocation.APoint);
      ClosestPoint := ClosestPointOnSegmentFromPoint(
        EquateSegment(Location2.APoint, Location3.APoint),
        NewLocation.APoint);
      Distance3 := Distance(ClosestPoint, NewLocation.APoint);
      if (Distance1 <= Distance2) and (Distance1 <= Distance3) then
      begin
        DerivedObservations.Delete(2);
        Locations.Delete(2);
      end
      else if (Distance2 <= Distance1) and (Distance2 <= Distance3) then
      begin
        DerivedObservations.Delete(1);
        Locations.Delete(1);
      end
      else if (Distance3 <= Distance1) and (Distance3 <= Distance2) then
      begin
        DerivedObservations.Delete(0);
        Locations.Delete(0);
      end
      else
      begin
        Assert(False, rsProgrammingE3);
      end;
      InterpTwoPoints(DerivedObservations, Locations, NewLocation);
    end;
  finally
    Triangle.Free;
  end;
end;

procedure TCustomInputHandler.InterpFourPoints(
  DerivedObservations: TDerivedObsList; Locations: TLocationList;
  NewLocation: TLocationID);
var
  DerivedObs1: TDerivedObs;
  Location1: TLocationID;
  DerivedObs2: TDerivedObs;
  Location2: TLocationID;
  DerivedObs3: TDerivedObs;
  Location3: TLocationID;
  DerivedObs4: TDerivedObs;
  Location4: TLocationID;
  ClosestPoint: TPoint2D;
  AnObs: TDerivedObs;
  Corners: TTriangularElement;
  Quadrilateral: TSimplePolygon;
  NodeValues: TTriangularNodeValues;
  Distance1: double;
  Distance2: double;
  Distance3: double;
  Distance4: double;
begin
  Assert(DerivedObservations.Count = 4, rsProgrammingE4);
  Assert(Locations.Count = 4, rsProgrammingE4);
  DerivedObs1 := DerivedObservations[0];
  DerivedObs2 := DerivedObservations[1];
  DerivedObs3 := DerivedObservations[2];
  DerivedObs4 := DerivedObservations[3];
  Location1 := Locations[0];
  Location2 := Locations[1];
  Location3 := Locations[2];
  Location4 := Locations[3];
  SetLength(Corners, 5);
  Corners[0] := Location1.APoint;
  Corners[2] := Location2.APoint;
  Corners[3] := Location3.APoint;
  Corners[4] := Location4.APoint;
  Corners[5] := Location1.APoint;
  Quadrilateral := TSimplePolygon.Create(Corners);
  try
    if Quadrilateral.PointInside(NewLocation.APoint) then
    begin
      AnObs := TDerivedObs.Create;
      AnObs.ID := '';
      AnObs.Obsname := FObsName;
      AnObs.Print := FPrint;
      AnObs.Time := DerivedObs1.Time;
      AnObs.TimeAssigned := DerivedObs1.TimeAssigned
        and DerivedObs2.TimeAssigned and DerivedObs3.TimeAssigned
        and DerivedObs4.TimeAssigned
        and (DerivedObs1.Time = DerivedObs2.Time)
        and (DerivedObs1.Time = DerivedObs3.Time)
        and (DerivedObs4.Time = DerivedObs3.Time);

      SetLength(Corners, 4);
      SetLength(NodeValues, 4);
      NodeValues[0] := DerivedObs1.Value;
      NodeValues[1] := DerivedObs2.Value;
      NodeValues[2] := DerivedObs3.Value;
      NodeValues[3] := DerivedObs4.Value;
      AnObs.Value := QuadrilateralBasisFunction(Corners, NodeValues, NewLocation.APoint);

      AddLocationToDictionary(NewLocation);

      RecordObs(AnObs);
    end
    else
    begin
      ClosestPoint := ClosestPointOnSegmentFromPoint(
        EquateSegment(Location1.APoint, Location2.APoint),
        NewLocation.APoint);
      Distance1 := Distance(ClosestPoint, NewLocation.APoint);
      ClosestPoint := ClosestPointOnSegmentFromPoint(
        EquateSegment(Location2.APoint, Location3.APoint),
        NewLocation.APoint);
      Distance2 := Distance(ClosestPoint, NewLocation.APoint);
      ClosestPoint := ClosestPointOnSegmentFromPoint(
        EquateSegment(Location3.APoint, Location4.APoint),
        NewLocation.APoint);
      Distance3 := Distance(ClosestPoint, NewLocation.APoint);
      ClosestPoint := ClosestPointOnSegmentFromPoint(
        EquateSegment(Location4.APoint, Location1.APoint),
        NewLocation.APoint);
      Distance4 := Distance(ClosestPoint, NewLocation.APoint);
      if (Distance1 <= Distance2) and (Distance1 <= Distance3)
        and (Distance1 <= Distance4) then
      begin
        DerivedObservations.Delete(3);
        Locations.Delete(3);
        DerivedObservations.Delete(2);
        Locations.Delete(2);
      end
      else if (Distance2 <= Distance1) and (Distance2 <= Distance3)
        and (Distance2 <= Distance4) then
      begin
        DerivedObservations.Delete(3);
        Locations.Delete(3);
        DerivedObservations.Delete(0);
        Locations.Delete(0);
      end
      else if (Distance3 <= Distance1) and (Distance3 <= Distance2)
        and (Distance3 <= Distance4) then
      begin
        DerivedObservations.Delete(1);
        Locations.Delete(1);
        DerivedObservations.Delete(0);
        Locations.Delete(0);
      end
      else if (Distance4 <= Distance1) and (Distance4 <= Distance2)
        and (Distance4 <= Distance3) then
      begin
        DerivedObservations.Delete(2);
        Locations.Delete(2);
        DerivedObservations.Delete(1);
        Locations.Delete(1);
      end
      else
      begin
        Assert(False, rsProgrammingE4);
      end;
      InterpTwoPoints(DerivedObservations, Locations, NewLocation);
    end;
  finally
    Quadrilateral.Free;
  end;
end;

procedure TCustomInputHandler.ClearFileObs;
begin
  FObsFileList.Clear;
  FObservationDictionary.Clear;
end;

constructor TCustomInputHandler.Create;
begin
  inherited;
  FListingFileName := '';
  FOutputFileName := '';
  FInstructionFileName := '';
  FCurrentProcessStatus := psNone;
  FPriorProcessStatus := psNone;
  FParser := TRbwParser.Create(nil);
  FLocationDictionary := TLocationDictionary.Create;
  FDerivedObsDictionary := TDerivedObsDictionary.Create;
  FDerivedObsList := TDerivedObsObjectList.Create;
  FInputFileLines := TStringList.Create;
  FSplitter := TStringList.Create;
  FObsFileList := TOutputFileObjectList.Create;
  FObservationDictionary := TObservationDictionary.Create;
end;

destructor TCustomInputHandler.Destroy;
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
  FObsFileList.Free;
  FSplitter.Free;
  FInputFileLines.Free;
  FDerivedObsList.Free;
  FDerivedObsDictionary.Free;
  FLocationDictionary.Free;
  FParser.Free;
  FListingFile.Free;
  FOutputFile.Free;
  FInstructionFile.Free;
  inherited Destroy;
end;

{ TDerivedObsObjectList }

procedure TDerivedObsObjectList.Sort;
begin
  inherited Sort(TDerivedObsCompare.Create);
end;

{ TDerivedObsCompare }

function TDerivedObsCompare.Compare(constref Left, Right: TDerivedObs): Integer;
begin
  Result := Sign(Left.Time - Right.Time);
end;

end.

