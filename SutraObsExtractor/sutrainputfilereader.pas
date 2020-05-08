unit SutraInputFileReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustomInputReader, CustomOutputFileReader,
    SutraOutputFileReader;

type
  TSutraFileType = (sftOBC, sftLKST);

  { TSutraInputHandler }

  TSutraInputHandler = class(TCustomInputHandler)
  protected
    FSutraFileType: TSutraFileType;
    function CreateObsFile(const FileName: string): TCustomOutputFile; override;
    function ApplicationTitle: string; override;
    procedure ChangeStatusFromID; override;
    procedure ReadAndRecordFileType(var FileName: string); override;
  end;


implementation

{ TSutraInputHandler }

function TSutraInputHandler.CreateObsFile(const FileName: string): TCustomOutputFile;
begin
  if FSutraFileType = sftOBC then
  begin
  result := TSutraObsOutputFile.Create(FileName,
    FObservationDictionary, LocationDictionary);
  end
  else
  begin
    result := TSutraLakeStageOutputFile.Create(FileName,
      FObservationDictionary);
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
    FIdStatus := isLocation;
    if FSutraFileType = sftOBC then
    begin
      Assert(False, Format(rsInLine0D1SCa, [FLineIndex+1,
        FInputFileLines[FLineIndex]]));
    end;
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
    else
    begin
      Assert(False, Format('The file format in line %0:d, "%1:s" must be "OBC" or "LKST".', [FLineIndex+1,
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
    if FSutraFileType  = sftOBC then
    begin
      FListingFile.Add('  Format = observation output file');
    end
    else
    begin
      FListingFile.Add('  Format = lake stage file');
    end;
    FListingFile.Add('');
  end;
end;

end.

