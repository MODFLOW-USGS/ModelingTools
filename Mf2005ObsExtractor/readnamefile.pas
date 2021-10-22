unit ReadNameFile;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults, ObExtractorTypes;

type
  TInputFileType = (iftMNW2, iftLAK, iftSFR, iftSUB, iftSWT, iftSWI,
    iftHOB, iftFlow, iftDerived);
  //TOutputFileType = (oftList, oftObs);

  TInputFileLink = record
    FileType: TInputFileType;
    FileName: string;
  end;

{$IFDEF FPC}
  TInputFileLinks = specialize TList<TInputFileLink>;
{$ELSE}
  TInputFileLinks = TList<TInputFileLink>;
{$ENDIF}


  { TNameFileReader }

  TNameFileReader = class(TObject)
  private
    FGenerateInstructionFile: Boolean;
    FListFileName: string;
    FListingFile: TStringList;
    FOutputFile: TStringList;
    FOutputFileName: string;
    FInputFileLinks: TInputFileLinks;
    FLineIndex: Integer;
    FNameFile: TStringList;
    FNameFileName: string;
    FObsDictionary: TCustomObsValueDictionary;
    function GetInputFile(Index: integer): TInputFileLink;
    function GetInputFileCount: Integer;
    procedure ReadOutputFileNames;
    procedure ReadInputFileNames;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadNameFile(const FileName: string);
    procedure RunScripts;
    property ListingFile: TStringList read FListingFile;
    // Ultimately @name will contain either the observed values or
    // an instruction file for reading the observed values.
    property OutputFile: TStringList read FOutputFile;
    property InputFileCount: Integer read GetInputFileCount;
    property InputFiles[Index: integer]: TInputFileLink read GetInputFile; default;
    property GenerateInstructionFile: Boolean Read FGenerateInstructionFile;
  end;

const
  IVersion = '0.1.0.0';

implementation

uses readinstructions;

resourcestring
  rsOnLine0DOf1S = 'On line %0:d of %1:s, the following line was not recognized'
    +'. "%2:s"';
  rsOnLine0DOf1S2Listing = 'On line %0:d of %1:s, a duplicate listing file was'
    +' listed';
  rsOnLine0DOf1S2Output = 'On line %0:d of %1:s, a duplicate output or '
    +'instruction file was listed';
  rsNoOutputFile = 'No output files read before reading input files.';


{ TNameFileReader }

function TNameFileReader.GetInputFile(Index: integer): TInputFileLink;
begin
  result := FInputFileLinks[Index];
end;

function TNameFileReader.GetInputFileCount: Integer;
begin
  result := FInputFileLinks.Count;
end;

procedure TNameFileReader.ReadOutputFileNames;
var
  ALine: string;
  Splitter: TStringList;
begin
  Splitter := TStringList.Create;
  try
    Splitter.Delimiter := ' ';
    While FLineIndex < FNameFile.Count do
    begin
      ALine := Trim(FNameFile[FLineIndex]);
      Inc(FLineIndex);
      if (Length(ALine) > 0) and (ALine[1] = '#') then
      begin
        FListingFile.Add(ALine);
        Continue;
      end;
      if (Length(ALine) = 0) then
      begin
        Continue;
      end;
      Splitter.DelimitedText := ALine;
      if Splitter.Count = 2 then
      begin
        if UpperCase(Splitter[0]) = 'LIST' then
        begin
          Assert(FListFileName = '', Format(rsOnLine0DOf1S2Listing, [
            FLineIndex, FNameFileName]));
          FListFileName := Splitter[1];
        end
        else if UpperCase(Splitter[0]) = 'OBSERVATIONS_FILE' then
        begin
          Assert(FOutputFileName = '', Format(rsOnLine0DOf1S2Output, [
            FLineIndex, FNameFileName]));
          FGenerateInstructionFile := False;
          FOutputFileName := Splitter[1];
        end
        else if UpperCase(Splitter[0]) = 'INSTRUCTION_FILE' then
        begin
          Assert(FOutputFileName = '', Format(rsOnLine0DOf1S2Output, [
            FLineIndex, FNameFileName]));
          FGenerateInstructionFile := True;
          FOutputFileName := Splitter[1];
          FOutputFile.Add('pif @');
          FOutputFile.Add('l1');
        end
        else if (UpperCase(Splitter[0]) = 'END')
          and (UpperCase(Splitter[1]) = 'OUTPUT_FILES') then
        begin
          FListingFile.Add('');
          FListingFile.Add(Format('Listing File = "%s"', [FListFileName]));

          if FGenerateInstructionFile then
          begin
            FListingFile.Add(Format('Instruction File = "%s"', [FOutputFileName]));
          end
          else
          begin
            FListingFile.Add(Format('Observations Output File = "%s"', [FOutputFileName]));
          end;

          Exit;
        end
        else
        begin
          Assert(False, Format(rsOnLine0DOf1S, [FLineIndex, FNameFileName, ALine]));
        end;
      end
      else
      begin
        Assert(False, Format(rsOnLine0DOf1S, [FLineIndex, FNameFileName, ALine]));
      end;
    end;
  finally
    Splitter.Free;
  end;
end;

procedure TNameFileReader.ReadInputFileNames;
var
  ALine: string;
  Splitter: TStringList;
  InputFile: TInputFileLink;
begin
  FListingFile.Add('');
  Splitter := TStringList.Create;
  try
    Splitter.Delimiter := ' ';
    While FLineIndex < FNameFile.Count do
    begin
      ALine := Trim(FNameFile[FLineIndex]);
      Inc(FLineIndex);
      if (Length(ALine) > 0) and (ALine[1] = '#') then
      begin
        FListingFile.Add(ALine);
        Continue;
      end;
      if (Length(ALine) = 0) then
      begin
        Continue;
      end;

      Splitter.DelimitedText := ALine;
      if Splitter.Count = 2 then
      begin
        if UpperCase(Splitter[0]) = 'MNW2' then
        begin
          InputFile.FileType := iftMNW2;
          InputFile.FileName := Splitter[1];
          FInputFileLinks.Add(InputFile);
          FListingFile.Add(Format('MNW2 Instruction File = "%s"', [InputFile.FileName]));
        end
        else if UpperCase(Splitter[0]) = 'LAK' then
        begin
          InputFile.FileType := iftLAK;
          InputFile.FileName := Splitter[1];
          FInputFileLinks.Add(InputFile);
          FListingFile.Add(Format('LAK Instruction File = "%s"', [InputFile.FileName]));
        end
        else if UpperCase(Splitter[0]) = 'SFR' then
        begin
          InputFile.FileType := iftSFR;
          InputFile.FileName := Splitter[1];
          FInputFileLinks.Add(InputFile);
          FListingFile.Add(Format('SFR Instruction File = "%s"', [InputFile.FileName]));
        end
        else if UpperCase(Splitter[0]) = 'SUB' then
        begin
          InputFile.FileType := iftSUB;
          InputFile.FileName := Splitter[1];
          FInputFileLinks.Add(InputFile);
          FListingFile.Add(Format('SUB Instruction File = "%s"', [InputFile.FileName]));
        end
        else if UpperCase(Splitter[0]) = 'SWT' then
        begin
          InputFile.FileType := iftSWT;
          InputFile.FileName := Splitter[1];
          FInputFileLinks.Add(InputFile);
          FListingFile.Add(Format('SWT Instruction File = "%s"', [InputFile.FileName]));
        end
        else if UpperCase(Splitter[0]) = 'SWI' then
        begin
          InputFile.FileType := iftSWI;
          InputFile.FileName := Splitter[1];
          FInputFileLinks.Add(InputFile);
          FListingFile.Add(Format('SWI Instruction File = "%s"', [InputFile.FileName]));
        end
        else if UpperCase(Splitter[0]) = 'HOB' then
        begin
          InputFile.FileType := iftHOB;
          InputFile.FileName := Splitter[1];
          FInputFileLinks.Add(InputFile);
          FListingFile.Add(Format('HOB output File = "%s"', [InputFile.FileName]));
        end
        else if UpperCase(Splitter[0]) = 'CHOB' then
        begin
          InputFile.FileType := iftFlow;
          InputFile.FileName := Splitter[1];
          FInputFileLinks.Add(InputFile);
          FListingFile.Add(Format('CHOB output File = "%s"', [InputFile.FileName]));
        end
        else if UpperCase(Splitter[0]) = 'DROB' then
        begin
          InputFile.FileType := iftFlow;
          InputFile.FileName := Splitter[1];
          FInputFileLinks.Add(InputFile);
          FListingFile.Add(Format('DROB output File = "%s"', [InputFile.FileName]));
        end
        else if UpperCase(Splitter[0]) = 'RVOB' then
        begin
          InputFile.FileType := iftFlow;
          InputFile.FileName := Splitter[1];
          FInputFileLinks.Add(InputFile);
          FListingFile.Add(Format('RVOB output File = "%s"', [InputFile.FileName]));
        end
        else if UpperCase(Splitter[0]) = 'GBOB' then
        begin
          InputFile.FileType := iftFlow;
          InputFile.FileName := Splitter[1];
          FInputFileLinks.Add(InputFile);
          FListingFile.Add(Format('GBOB output File = "%s"', [InputFile.FileName]));
        end
        else if UpperCase(Splitter[0]) = 'STOB' then
        begin
          InputFile.FileType := iftFlow;
          InputFile.FileName := Splitter[1];
          FInputFileLinks.Add(InputFile);
          FListingFile.Add(Format('STOB output File = "%s"', [InputFile.FileName]));
        end
        else if UpperCase(Splitter[0]) = 'DERIVED' then
        begin
          InputFile.FileType := iftDerived;
          InputFile.FileName := Splitter[1];
          FInputFileLinks.Add(InputFile);
          FListingFile.Add(Format('DERIVED Instruction File = "%s"', [InputFile.FileName]));
        end
        else if (UpperCase(Splitter[0]) = 'END')
          and (UpperCase(Splitter[1]) = 'INPUT_FILES') then
        begin
          Exit;
        end
        else
        begin
          Assert(False, Format(rsOnLine0DOf1S, [FLineIndex, FNameFileName, ALine]));
        end;
      end
      else
      begin
        Assert(False, Format(rsOnLine0DOf1S, [FLineIndex, FNameFileName, ALine]));
      end;
    end;
  finally
    Splitter.Free;
  end;
end;

constructor TNameFileReader.Create;
begin
  FListingFile := TStringList.Create;
  FOutputFile := TStringList.Create;
  FInputFileLinks := TInputFileLinks.Create;

  FListingFile.Add('MODFLOW Observation Extractor');
  FListingFile.Add('Version ' + IVersion);
  FListingFile.Add('');

  FObsDictionary := TCustomObsValueDictionary.Create;
end;

destructor TNameFileReader.Destroy;
var
  ErrorMessage: string;
begin
  try
    try
      FOutputFile.SaveToFile(FOutputFileName);
    except on E: Exception do
      begin
        FListingFile.Add(E.Message);
        if FOutputFileName = '' then
        begin
          ErrorMessage := 'Error saving observations file because no file name specified.';
        end
        else
        begin
          ErrorMessage := Format('Error saving observation file "%s".', [FOutputFileName])
        end;
        WriteLn(ErrorMessage);
        FListingFile.Add(ErrorMessage);
        raise;
      end;
    end;
    try
      FListingFile.Add('normal termination');
      FListingFile.SaveToFile(FListFileName);
    except  on E: Exception do
      begin
        if FListFileName = '' then
        begin
          WriteLn('Error saving listing file because no file name specified.');
        end
        else
        begin
          WriteLn(Format('Error saving listing file "%s".', [FListFileName]));
        end;
        raise;
      end;
    end;

  finally
    FObsDictionary.Free;
    FInputFileLinks.Free;
    FOutputFile.Free;
    FListingFile.Free;
    inherited Destroy;
  end;
end;

procedure TNameFileReader.ReadNameFile(const FileName: string);
var
  Splitter: TStringList;
  ALine: string;
  FOutputRead: Boolean;
begin
  FNameFileName := FileName;
  FOutputRead := False;
  FNameFile := TStringList.Create;
  Splitter := TStringList.Create;
  try
    Splitter.Delimiter := ' ';
    FNameFile.LoadFromFile(FileName);
    FLineIndex := 0;
    While FLineIndex < FNameFile.Count do
    begin
      ALine := Trim(FNameFile[FLineIndex]);
      Inc(FLineIndex);
      if (Length(ALine) > 0) and (ALine[1] = '#') then
      begin
        FListingFile.Add(ALine);
        Continue;
      end;
      if (Length(ALine) = 0) then
      begin
        Continue;
      end;
      Splitter.DelimitedText := ALine;
      if (Splitter.Count = 2) and (UpperCase(Splitter[0]) = 'BEGIN') then
      begin
        if (UpperCase(Splitter[1]) = 'OUTPUT_FILES') then
        begin
          ReadOutputFileNames;
          FOutputRead := True;
        end
        else if (UpperCase(Splitter[1]) = 'INPUT_FILES') then
        begin
          Assert(FOutputRead, rsNoOutputFile);
          ReadInputFileNames;
          Exit;
        end
        else
        begin
          Assert(False, Format(rsOnLine0DOf1S, [FLineIndex, FileName, ALine]));
        end;
      end
      else
      begin
        Assert(False, Format(rsOnLine0DOf1S, [FLineIndex, FileName, ALine]));
      end;
    end;
  finally
    FNameFile.Free;
    Splitter.Free;
  end;
end;

procedure TNameFileReader.RunScripts;
var
  //NewPosition: Integer;
  ItemIndex : Integer;
  //SortedInputFileLinks: TInputFileLinks;
  ObsProcessorList: TObsProcessorList;
  ObsProcessor: TObsProcessor;
begin
  ObsProcessorList := TObsProcessorList.Create;
  try
    ObsProcessorList.Capacity := FInputFileLinks.Count;

    for ItemIndex := 0 to Pred(FInputFileLinks.Count) do
    begin
      ObsProcessor := TObsProcessor.Create(FInputFileLinks[ItemIndex],
        FGenerateInstructionFile);
      ObsProcessorList.Add(ObsProcessor);
      ObsProcessor.ListingFile := ListingFile;
      ObsProcessor.ObservationsFile := OutputFile;
      ObsProcessor.ObsDictionary := FObsDictionary;
    end;

    for ItemIndex := 0 to Pred(FInputFileLinks.Count) do
    begin
      ObsProcessor := ObsProcessorList[ItemIndex];
      ObsProcessor.ProcessInstructionFile;
    end;

    for ItemIndex := 0 to Pred(FInputFileLinks.Count) do
    begin
      ObsProcessor := ObsProcessorList[ItemIndex];
      ObsProcessor.HandleDerivedObservations;
      ObsProcessor.WriteFiles;
    end;
    Writeln('normal termination');

  finally
    //SortedInputFileLinks.Free;
    ObsProcessorList.Free;
  end;
end;

end.

