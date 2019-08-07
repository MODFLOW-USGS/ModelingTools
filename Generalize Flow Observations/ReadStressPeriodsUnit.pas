unit ReadStressPeriodsUnit;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.IOUtils;

type
  EDisFileReader = class(Exception);

  TStressPeriodType = (sptSteady, sptTransient);

  TStressPeriod = class(TObject)
    PERLEN: double;
    NSTP: integer;
    TSMULT: double;
    StressPeriodType: TStressPeriodType;
    TotalTime: double;
    StartTime: double;
  end;

  TStressPeriods = TObjectList<TStressPeriod>;

  TDisFileReader = class(TObject)
  private
    FDisFile: TStreamReader;
    FLineSplitter: TStringList;
    FOutputFile: TStreamWriter;
    procedure ReadComments(var ALine: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadDiscretization(const DiscretizationFileName: string;
      var NLAY, NROW, NCOL: Integer; StressPeriods: TStressPeriods;
      OutputFile: TStreamWriter);
  end;

implementation

constructor TDisFileReader.Create;
begin
  FLineSplitter := TStringList.Create;
  FLineSplitter.Delimiter := ' ';
end;

destructor TDisFileReader.Destroy;
begin
  FLineSplitter.Free;
  inherited;
end;

procedure TDisFileReader.ReadComments(var ALine: string);
begin
  while not FDisFile.EndOfStream do
  begin
    ALine := FDisFile.ReadLine;
    if (ALine = '') or (ALine[1] = '#') then
    begin
      FOutputFile.WriteLine(ALine);
    end
    else
    begin
      Exit;
    end;
  end;
end;

procedure TDisFileReader.ReadDiscretization(
  const DiscretizationFileName: string; var NLAY, NROW, NCOL: Integer;
  StressPeriods: TStressPeriods; OutputFile: TStreamWriter);
var
  ALine: string;
  SP: TStressPeriod;
  NPER: integer;
  PeriodIndex: Integer;
  OutputLine: TStringBuilder;
  Blanks: TCharArray;
  index: Integer;
begin
  SetLength(Blanks, 15);
  for index := 0 to Length(Blanks) - 1 do
  begin
    Blanks[index] := ' ';
  end;
  Assert(OutputFile <> nil);
  FOutputFile := OutputFile;
  StressPeriods.Clear;
  FDisFile := nil;
  try
    OutputLine := TStringBuilder.Create;
    try
      FDisFile := TFile.OpenText(DiscretizationFileName);
      ReadComments(ALine);

      OutputFile.WriteLine('');

      FLineSplitter.DelimitedText := ALine;
      if FLineSplitter.Count < 6 then
      begin
        raise EDisFileReader.Create(
          'Error reading data set 1 of Discretization file. Too few variables.');
      end;

      if not TryStrToInt(FLineSplitter[0], NLAY) then
      begin
        raise EDisFileReader.Create(
          'Error reading NLAY in data set 1 of Discretization file.');
      end
      else
      begin
        OutputFile.WriteLine(Format('NLAY = %d', [NLAY]));
      end;
      if not TryStrToInt(FLineSplitter[1], NROW) then
      begin
        raise EDisFileReader.Create(
          'Error reading NROW in data set 1 of Discretization file.');
      end
      else
      begin
        OutputFile.WriteLine(Format('NROW = %d', [NROW]));
      end;
      if not TryStrToInt(FLineSplitter[2], NCOL) then
      begin
        raise EDisFileReader.Create(
          'Error reading NCOL in data set 1 of Discretization file.');
      end
      else
      begin
        OutputFile.WriteLine(Format('NCOL = %d', [NCOL]));
      end;
      if not TryStrToInt(FLineSplitter[3], NPER) then
      begin
        raise EDisFileReader.Create(
          'Error reading NPER in data set 1 of Discretization file.');
      end
      else
      begin
        OutputFile.WriteLine(Format('NPER = %d', [NPER]));
      end;
      OutputFile.WriteLine('');

      while not FDisFile.EndOfStream do
      begin
        ALine := UpperCase(FDisFile.ReadLine);
        if Pos('SS', ALine) > 0 then
        begin
          FLineSplitter.DelimitedText := ALine;
          if (FLineSplitter.Count >= 4) and (FLineSplitter[3] = 'SS') then
          begin
            break;
          end;
        end
        else if Pos('TR', ALine) > 0 then
        begin
          FLineSplitter.DelimitedText := ALine;
          if (FLineSplitter.Count >= 4) and (FLineSplitter[3] = 'TR') then
          begin
            break;
          end;
        end;
      end;

      OutputFile.WriteLine('Stress                        Number');
      OutputFile.WriteLine('Period         Length         of steps       Multiplier     SS/TR');
      OutputFile.WriteLine('-------------  -------------  -------------  -------------  ------------');


      OutputLine.Append(1);
      OutputLine.Append(Blanks, 0, 15 - OutputLine.Length);
      SP := TStressPeriod.Create;
      StressPeriods.Add(SP);
      if not TryStrToFloat(FLineSplitter[0], SP.PERLEN) then
      begin
        raise EDisFileReader.Create(Format(
          'Error reading PERLEN data in Stress Period %d of Discretization file.', [1]));
      end;
      OutputLine.Append(SP.PERLEN);
      OutputLine.Append(Blanks, 0, 30 - OutputLine.Length);
      if not TryStrToInt(FLineSplitter[1], SP.NSTP) then
      begin
        raise EDisFileReader.Create(Format(
          'Error reading NSTP data in Stress Period %d of Discretization file.',
          [1]));
      end;
      OutputLine.Append(SP.NSTP);
      OutputLine.Append(Blanks, 0, 45 - OutputLine.Length);
      if not TryStrToFloat(FLineSplitter[2], SP.TSMULT) then
      begin
        raise EDisFileReader.Create(Format(
          'Error reading TSMULT data in Stress Period %d of Discretization file.',
          [1]));
      end;
      OutputLine.Append(SP.TSMULT);
      OutputLine.Append(Blanks, 0, 60 - OutputLine.Length);
      if UpperCase(FLineSplitter[3]) = 'SS' then
      begin
        SP.StressPeriodType := sptSteady;
        OutputLine.Append('Steady');
      end
      else if UpperCase(FLineSplitter[3]) = 'TR' then
      begin
        SP.StressPeriodType := sptTransient;
        OutputLine.Append('Transient');
      end
      else
      begin
        raise EDisFileReader.Create(Format(
          'Error reading SS/TR data in Stress Period %d of Discretization file.',
          [1]));
      end;
      OutputFile.WriteLine(OutputLine.ToString);

      for PeriodIndex := 2 to NPER do
      begin
        OutputLine.Clear;
        OutputLine.Append(PeriodIndex);
        OutputLine.Append(Blanks, 0, 15 - OutputLine.Length);
        FLineSplitter.DelimitedText := FDisFile.ReadLine;
        SP := TStressPeriod.Create;
        StressPeriods.Add(SP);
        if not TryStrToFloat(FLineSplitter[0], SP.PERLEN) then
        begin
          raise EDisFileReader.Create(Format(
            'Error reading PERLEN data in Stress Period %d of Discretization file.',
            [PeriodIndex]));
        end;
        OutputLine.Append(SP.PERLEN);
        OutputLine.Append(Blanks, 0, 30 - OutputLine.Length);
        if not TryStrToInt(FLineSplitter[1], SP.NSTP) then
        begin
          raise EDisFileReader.Create(Format(
            'Error reading NSTP data in Stress Period %d of Discretization file.',
            [PeriodIndex]));
        end;
        OutputLine.Append(SP.NSTP);
        OutputLine.Append(Blanks, 0, 45 - OutputLine.Length);
        if not TryStrToFloat(FLineSplitter[2], SP.TSMULT) then
        begin
          raise EDisFileReader.Create(Format(
            'Error reading TSMULT data in Stress Period %d of Discretization file.',
            [PeriodIndex]));
        end;
        OutputLine.Append(SP.TSMULT);
        OutputLine.Append(Blanks, 0, 60 - OutputLine.Length);
        if UpperCase(FLineSplitter[3]) = 'SS' then
        begin
          SP.StressPeriodType := sptSteady;
          OutputLine.Append('Steady');
        end
        else if UpperCase(FLineSplitter[3]) = 'TR' then
        begin
          SP.StressPeriodType := sptTransient;
          OutputLine.Append('Transient');
        end
        else
        begin
          raise EDisFileReader.Create(Format(
            'Error reading SS/TR data in Stress Period %d of Discretization file.',
            [PeriodIndex]));
        end;
        OutputFile.WriteLine(OutputLine.ToString);
      end;

    finally
      FDisFile.Free;
      OutputLine.Free;
    end;
  except on E: Exception do
    begin
      FOutputFile.WriteLine(E.Message);
      raise;
    end;
  end;
end;

end.
