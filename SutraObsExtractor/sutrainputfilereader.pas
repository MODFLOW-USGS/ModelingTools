unit SutraInputFileReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustomInputReader, CustomOutputFileReader,
    SutraOutputFileReader;

type
  TSutraFileType = (sftOBC, sftLKST, sftBCOP, sftBCOF, sftBCOU, sftBCOPG, sftBCOUG);

  { TSutraInputHandler }

  TSutraInputHandler = class(TCustomInputHandler)
  protected
    FNodeNumber: Integer;
    FSutraFileType: TSutraFileType;
    function CreateObsFile(const FileName: string): TCustomOutputFile; override;
    function ApplicationTitle: string; override;
    procedure ChangeStatusFromID; override;
    procedure ReadAndRecordFileType(var FileName: string); override;
    procedure SpecifyID; override;
  end;


implementation

resourcestring
  rsInLine0D1SCa = 'In line %0:d, "%1:s", "LOCATION" is invalid. The location '
    +'is read directly from OBC files and is not needed for lake stage files.';
  rsTheFileForma = 'The file format in line %0:d, "%1:s" must be one of the '
    + 'following: "OBC", "LKST", "BCOP", "BCOF", "BCOU", "BCOPG",or "BCOUG".';
  rsMustBeThree = 'In line %0:d, "%1:s", there must be exactly three items listed.';

{ TSutraInputHandler }

function TSutraInputHandler.CreateObsFile(const FileName: string): TCustomOutputFile;
begin
  case FSutraFileType of
    sftOBC:
      begin
        result := TSutraObsOutputFile.Create(FileName,
          FObservationDictionary, LocationDictionary);
      end;
    sftLKST:
      begin
        result := TSutraLakeStageOutputFile.Create(FileName,
          FObservationDictionary);
      end;
    sftBCOP:
      begin
        result := TSutraSpecifiedPressureOutputFile.Create(FileName,
          FObservationDictionary);
      end;
    sftBCOF:
      begin
        result := TSutraFluidSourceSinkOutputFile.Create(FileName,
          FObservationDictionary);
      end;
    sftBCOU:
      begin
        result := TSutraSpecifiedConcentrationOutputFile.Create(FileName,
          FObservationDictionary);
      end;
    sftBCOPG:
      begin
        result := TSutraGeneralizedFlowOutputFile.Create(FileName,
          FObservationDictionary);
      end;
    sftBCOUG:
      begin
        result := TSutraGeneralizedTransportOutputFile.Create(FileName,
          FObservationDictionary);
      end;
  else
    Assert(False);
  end;
end;

function TSutraInputHandler.ApplicationTitle: string;
begin
  result := 'SUTRA Observation Extractor';
end;

procedure TSutraInputHandler.ChangeStatusFromID;
begin
  if UpperCase(FSplitter[0]) = rsLOCATION then
  begin
    //FIdStatus := isLocation;
    //if FSutraFileType = sftOBC then
    //begin
      Assert(False, Format(rsInLine0D1SCa, [FLineIndex+1,
        FInputFileLines[FLineIndex]]));
    //end;
  end
  else if UpperCase(FSplitter[0]) = rsOBSNAME then
  begin
    FIdStatus := isTime
  end
  else
  begin
    Assert(False, Format(rsInLine0D1SMu, [FLineIndex+1,
      FInputFileLines[FLineIndex]]));
  end;
end;

procedure TSutraInputHandler.ReadAndRecordFileType(var FileName: string);
begin
  FFileType := ftText;
  if FSplitter.Count = 3 then
  begin
    if UpperCase(FSplitter[2]) = 'OBC' then
    begin
      FSutraFileType := sftOBC
    end
    else if UpperCase(FSplitter[2]) = 'LKST' then
    begin
      FSutraFileType := sftLKST
    end
    else if UpperCase(FSplitter[2]) = 'BCOP' then
    begin
      FSutraFileType := sftBCOP
    end
    else if UpperCase(FSplitter[2]) = 'BCOF' then
    begin
      FSutraFileType := sftBCOF
    end
    else if UpperCase(FSplitter[2]) = 'BCOU' then
    begin
      FSutraFileType := sftBCOU
    end
    else if UpperCase(FSplitter[2]) = 'BCOPG' then
    begin
      FSutraFileType := sftBCOPG
    end
    else if UpperCase(FSplitter[2]) = 'BCOUG' then
    begin
      FSutraFileType := sftBCOUG
    end
    else
    begin
      Assert(False, Format(rsTheFileForma, [FLineIndex+1,
        FInputFileLines[FLineIndex]]));
    end;
  end
  else
  begin
    FSutraFileType := sftOBC
  end;
  if FListingFile <> nil then
  begin
    FListingFile.Add(Format(rsObservationF, [FileName]));
    case FSutraFileType of
      sftOBC:
        begin
          FListingFile.Add('  Format = observation output file');
        end;
      sftLKST:
        begin
          FListingFile.Add('  Format = lake stage file');
        end;
      sftBCOP:
        begin
          FListingFile.Add('  Format = specified-pressure output file');
        end;
      sftBCOF:
        begin
          FListingFile.Add('  Format = fluid source/sink output file');
        end;
      sftBCOU:
        begin
          FListingFile.Add('  Format = specified concentration/temperature output file');
        end;
      sftBCOPG:
        begin
          FListingFile.Add('  Format = generalized flow output file');
        end;
      sftBCOUG:
        begin
          FListingFile.Add('  Format = generalized transport output file');
        end;
    else
      Assert(False);
    end;
    FListingFile.Add('');
  end;
end;

procedure TSutraInputHandler.SpecifyID;
var
  ObservationType: string;
  ErrorMessage: string;
begin
  Assert(FSplitter.Count in [3,4], Format(rsMustBeThree, [FLineIndex+1,
    FInputFileLines[FLineIndex]]));
  FID := FSplitter[1] + '_' + UpperCase(FSplitter[2]);
  ObservationType := FSplitter[2];
  if FSplitter.Count = 4 then
  begin
    FNodeNumber := StrToInt(FSplitter[3]);
    FID := FID + '_' + FSplitter[3]
  end;
  if FListingFile <> nil then
  begin
    FListingFile.Add('');
    FListingFile.Add(Format(rsIDInObservat, [FSplitter[1]]));
    FListingFile.Add(Format('  Observation type = %s', [ObservationType]));
    if ObservationType = 'P' then
    begin
      FListingFile.Add('  Pressure in OBC file');
    end
    else if ObservationType = 'U' then
    begin
      FListingFile.Add('  Temperature or concentration in OBC file');
    end
    else if ObservationType = 'S' then
    begin
      FListingFile.Add('  Saturation in OBC file');
    end
    else if ObservationType = 'LKST' then
    begin
      FListingFile.Add('  Lake Stage in LKST file');
    end
    else if ObservationType = 'PF' then
    begin
      FListingFile.Add('  Resultant source/sink(+/-) of fluid in BCOP file');
    end
    else if ObservationType = 'PU' then
    begin
      FListingFile.Add('  Solute conc/temperature of fluid source/sink in BCOP file');
    end
    else if ObservationType = 'PR' then
    begin
      FListingFile.Add('  Resultant source/sink(+/-) of mass/energy in BCOP file');
    end
    else if ObservationType = 'FF' then
    begin
      FListingFile.Add('  Specified flow rate in BCOF file');
    end
    else if ObservationType = 'FU' then
    begin
      FListingFile.Add('  Solute conc/temperature of fluid source/sink in BCOF file');
    end
    else if ObservationType = 'FR' then
    begin
      FListingFile.Add('  Resultant source/sink(+/-) of mass/energy in BCOF file');
    end
    else if ObservationType = 'UR' then
    begin
      FListingFile.Add('  Resultant source/sink(+/-) of mass/energy in BCOU file');
    end
    else if ObservationType = 'PGF' then
    begin
      FListingFile.Add('  Resultant source/sink(+/-) of fluid in BCOPG file');
    end
    else if ObservationType = 'PGU' then
    begin
      FListingFile.Add('  Solute conc/temperature of fluid source/sink in BCOPG file');
    end
    else if ObservationType = 'PGR' then
    begin
      FListingFile.Add('  Resultant source/sink(+/-) of mass/energy in BCOPG file');
    end
    else if ObservationType = 'UGR' then
    begin
      FListingFile.Add('  Resultant source/sink(+/-) of mass/energy in BCOUG file');
    end
    else if ObservationType = 'UGU' then
    begin
      FListingFile.Add('  Computed conc/temperature in BCOUG file');
    end
    else
    begin
      ErrorMessage := Format('On line %0:d, "%1:s" is an invalid observation type',
        [FLineIndex + 1, ObservationType]);
      FListingFile.Add(ErrorMessage);
      Assert(False, ErrorMessage);
    end;

    if FSplitter.Count = 4 then
    begin
      FListingFile.Add('  Node Number = ' + FSplitter[3]);
    end;
  end;
end;

end.

