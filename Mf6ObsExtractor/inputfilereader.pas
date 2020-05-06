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
    FID: string;
  protected
    function CreateObsFile(const FileType: TFileType;
      const FileName: string): TCustomOutputFile; override;
    procedure HandleIdentifiers;
    function ApplicationTitle: string; override;
  public
    procedure ReadAndProcessInputFile(const FileName: string);
  end;

implementation

resourceString
  rsBEGIN = 'BEGIN';
  rsOPTIONS = 'OPTIONS';
  rsOBSERVATION_Files = 'OBSERVATION_FILES';
  rsIDENTIFIERS = 'IDENTIFIERS';
  rsNotExactlyTwoItems = 'In line %0:d, "%1:s", there were not exactly two '
    +'items listed.';
  rsMODFLOW6Obse = 'MODFLOW 6 Observation Extractor';
  rsMustBeTwo = 'In line %0:d, "%1:s", there must be exactly two items listed.';
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
  rsEndOfIDENTIF = 'End of IDENTIFIERS Block';

{ TInputHandler }

function TInputHandler.CreateObsFile(const FileType: TFileType;
  const FileName: string): TCustomOutputFile;
begin
  result := TOutputFile.Create(FileName, FileType, FObservationDictionary);
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

function TInputHandler.ApplicationTitle: string;
begin
  result := rsMODFLOW6Obse;
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
end;

end.

