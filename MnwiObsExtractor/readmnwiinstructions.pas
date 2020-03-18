unit ReadMnwiInstructions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ReadMnwiOutput;

type

  { TMnwiObsProcessor }

  TMnwiObsProcessor = class(TObject)
  private
    FLineIndex: integer;
    FInputFile: TStringList;
    FObsList: TMnwiObsValueObjectList;
    FObsDictionary: TMnwiObsValueDictionary;
    procedure HandleSimpleObservations;
    procedure HandleDerivedObservations;
  public
    Constructor Create;
    destructor Destroy; override;
    procedure ProcessInstructionFile(InstructionFileName: string);
  end;


implementation

resourcestring
  rsDERIVED_OBSE = 'DERIVED_OBSERVATIONS';

{ TMnwiObsProcessor }

procedure TMnwiObsProcessor.HandleSimpleObservations;
var
  ALine: string;
  Splitter: TStringList;
  MnwiOutputFileName: string;
  ObsExtractor: TObsExtractor;
  ObsName: String;
  ObsTypeIndex: Integer;
  ObsTime: double;
  ObsTypes: TStringList;
  Obs: TMnwiObsValue;
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
  function RemoveQuotes(AString: string): string;
  begin
    Assert(Length(AString) > 0);
    if (AString[1] = '"') and (AString[Length(AString)] = '"') then
    begin
      AString := Copy(AString, 2, Length(AString)-2);
    end
    else if (AString[1] = '''') and (AString[Length(AString)] = '''') then
    begin
      AString := Copy(AString, 2, Length(AString)-2);
    end;
    result := AString;
  end;
begin
  ObsExtractor := nil;
  ObsTypes := TStringList.Create;
  Splitter := TStringList.Create;
  try
    ObsTypes.Add(UpperCase('Qin'));
    ObsTypes.Add(UpperCase('Qout'));
    ObsTypes.Add(UpperCase('Qnet'));
    ObsTypes.Add(UpperCase('Cum.Vol.'));
    ObsTypes.Add(UpperCase('hwell'));
    ObsTypes.CaseSensitive := False;

    Splitter.Delimiter := ' ';
    While FLineIndex < FInputFile.Count do
    begin
      ALine := Trim(FInputFile[FLineIndex]);
      Inc(FLineIndex);
      if (ALine = '') or (ALine[1] = '#') then
      begin
        Continue;
      end;
      Splitter.DelimitedText := ALine;
      if Splitter.Count = 2 then
      begin
        if (UpperCase(Splitter[0]) = 'END')
          and (UpperCase(Splitter[1]) = 'OBSERVATIONS') then
        begin
          ProcessObsFile;
          Exit;
        end
        else if UpperCase(Splitter[0]) = 'FILENAME' then
        begin
          ProcessObsFile;
          MnwiOutputFileName := RemoveQuotes(Splitter[1]);
          ObsExtractor := TObsExtractor.Create;
          ObsExtractor.MnwiOutputFileName := MnwiOutputFileName;
        end;
      end
      else if Splitter.Count in [4,5] then
      begin
        Assert(UpperCase(Splitter[0]) = 'OBSERVATION');
        Assert(ObsExtractor <> nil);
        ObsName := Splitter[1];
        ObsTypeIndex := ObsTypes.IndexOf(UpperCase(Splitter[2]));
        Assert(ObsTypeIndex >= 0);
        ObsTime := StrToFloat(Splitter[3]);
        Obs := TMnwiObsValue.Create;
        FObsList.Add(Obs);
        Obs.ObsName := ObsName;
        Obs.ObsType := TMnwiObsType(ObsTypeIndex);
        Obs.ObsTime := ObsTime;
        if (Splitter.Count = 5) and (UpperCase(Splitter[4]) = 'PRINT') then
        begin
          Obs.Print := True;
        end
        else
        begin
          Obs.Print := False;
        end;
        ObsExtractor.AddObs(Obs);
        FObsDictionary.Add(UpperCase(Obs.ObsName), Obs);
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
  FirstValue: TMnwiObsValue;
  SecondValue: TMnwiObsValue;
  Obs: TMnwiObsValue;
  ObsName: string;
  FirstName: string;
  SecondName: string;
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
        Continue;
      end;

      Splitter.DelimitedText := ALine;
      if (Splitter.Count = 2)
        and (UpperCase(Splitter[0]) = 'END')
        and (UpperCase(Splitter[1]) = rsDERIVED_OBSE)
        then
      begin
        Exit;
      end
      else if (Splitter.Count = 4) and
        (UpperCase(Splitter[0]) = 'DIFFERENCE') then
      begin
        ObsName := Splitter[1];
        Obs := TMnwiObsValue.Create;
        Obs.ObsName := ObsName;
        FObsList.Add(Obs);
        FirstName := Splitter[2];
        SecondName := Splitter[3];
        if not FObsDictionary.TryGetData(UpperCase(FirstName), FirstValue) then
        begin
          FirstValue := nil
        end;
        if not FObsDictionary.TryGetData(UpperCase(SecondName), SecondValue) then
        begin
          SecondValue := nil
        end;
        if (FirstValue <> nil) and (SecondValue <> nil) then
        begin
          if (FirstValue.SimulatedValue = MissingValue)
            or (SecondValue.SimulatedValue = MissingValue)
            then
          begin
            Obs.SimulatedValue :=
              FirstValue.SimulatedValue - SecondValue.SimulatedValue;
          end
          else
          begin
            Obs.SimulatedValue := MissingValue;
          end;
        end
        else
        begin
          Obs.SimulatedValue := MissingValue;
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

constructor TMnwiObsProcessor.Create;
begin
  FObsList := TMnwiObsValueObjectList.Create;
  FObsDictionary := TMnwiObsValueDictionary.Create;
  FObsDictionary.Duplicates := dupError;
end;

destructor TMnwiObsProcessor.Destroy;
begin
  FObsDictionary.Free;
  FObsList.Free;
  inherited Destroy;
end;

procedure TMnwiObsProcessor.ProcessInstructionFile(InstructionFileName: string);
var
  ALine: string;
begin
  FInputFile := TStringList.Create;
  try
    FInputFile.LoadFromFile(InstructionFileName);
    FLineIndex := 0;
    While FLineIndex < FInputFile.Count do
    begin
      ALine := Trim(FInputFile[FLineIndex]);
      Inc(FLineIndex);
      if (ALine = '') or (ALine[1] = '#') then
      begin
        Continue;
      end;
      ALine := UpperCase(ALine);
      if Pos('BEGIN', ALine) = 1 then
      begin
        ALine := Trim(Copy(ALine, 7, MAXINT));
        if ALine = 'OBSERVATIONS' then
        begin
          HandleSimpleObservations;
        end
        else if ALine = rsDERIVED_OBSE then
        begin
          HandleDerivedObservations;
          Exit;
        end;
      end
      else
      begin
        Assert(False);
      end;
    end;
  finally
    FInputFile.Free;
  end;
end;

end.

