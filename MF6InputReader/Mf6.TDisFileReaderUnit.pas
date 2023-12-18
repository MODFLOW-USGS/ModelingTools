unit Mf6.TDisFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections, Mf6.AtsFileReaderUnit;

type
  TTDisOptions = class(TCustomMf6Persistent)
  public
    TimeUnits: string;
    StartDate: string;
    ATS6_FileName: string;
    procedure Initialize; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  end;

  TTDisDimensions = class(TCustomMf6Persistent)
  public
    NPER: Integer;
    procedure Initialize; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  end;

  TPeriod = class(TObject)
    PerLen: Extended;
    NStp: Integer;
    TSMult: Extended;
  end;

  TPeriodList = TObjectList<TPeriod>;

  TTDisPeriodData = class(TCustomMf6Persistent)
  private
    FPeriods: TPeriodList;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  end;

  TTDis = class(TCustomMf6Persistent)
  private
    FOptions: TTDisOptions;
    FDimensions: TTDisDimensions;
    FPeriodData: TTDisPeriodData;
    FAts: TAts;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    procedure ReadInput(Unhandled: TStreamWriter);
  end;

implementation

uses
  ModelMuseUtilities;

resourcestring
  StrUnrecognizedTDISOp = 'Unrecognized TDIS option in the following line.';
  StrUnrecognizedTDISDI = 'Unrecognized TDIS DIMENSION option in the followi' +
  'ng line.';
  StrNPERDInTheTD = 'NPER, "%d" in the TDIS DIMENSIONS block does not match ' +
  'the number of stress periods listed in the PERIODDATA block "%d".';

{ TDisOptions }

procedure TTDisOptions.Initialize;
begin
  inherited;
  TimeUnits := 'UNKNOWN';
  StartDate := '';
  ATS6_FileName := '';
end;

procedure TTDisOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  ValidUnits: TStringList;
begin
  Initialize;
  ValidUnits := TStringList.Create;
  try
    ValidUnits.Add('UNKNOWN');
    ValidUnits.Add('SECONDS');
    ValidUnits.Add('MINUTES');
    ValidUnits.Add('HOURS');
    ValidUnits.Add('DAYS');
    ValidUnits.Add('YEARS');

    try
      while not Stream.EndOfStream do
      begin
        ALine := Stream.ReadLine;
        if Stream.EndOfStream and (FOriginalStream <> nil) then
        begin
          Stream.Free;
          Stream := FOriginalStream;
          FOriginalStream := nil;
        end;
        ErrorLine := ALine;
        ALine := StripFollowingComments(ALine);
        if ALine = '' then
        begin
          Continue;
        end;



        ALine := UpperCase(ALine);
        if ReadEndOfSection(ALine, ErrorLine, 'OPTIONS', Unhandled) then
        begin
          Exit
        end;

        if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'OPTIONS') then
        begin
          // do nothing
        end
        else if FSplitter.Count >= 2 then
        begin
          if FSplitter[0] = 'TIME_UNITS' then
          begin
            TimeUnits := FSplitter[1];
          end
          else if FSplitter[0] = 'START_DATE_TIME' then
          begin
            StartDate := FSplitter[1];
          end
          else if FSplitter.Count >= 3 then
          begin
            if (FSplitter[0] = 'ATS6') and (FSplitter[1] = 'FILEIN') then
            begin
              ATS6_FileName := FSplitter[2];
            end
            else
            begin
              Unhandled.WriteLine(StrUnrecognizedTDISOp);
              Unhandled.WriteLine(ErrorLine);
            end;
          end
          else
          begin
            Unhandled.WriteLine(StrUnrecognizedTDISOp);
            Unhandled.WriteLine(ErrorLine);
          end;
        end
        else
        begin
          Unhandled.WriteLine(StrUnrecognizedTDISOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end;
    finally
      if ValidUnits.IndexOf(TimeUnits) < 0 then
      begin
        Unhandled.WriteLine(Format('Unrecognized time unit "%s".', [TimeUnits]));
      end;
    end;
  finally
    ValidUnits.Free;
  end;
end;

{ TDisDimensions }

procedure TTDisDimensions.Initialize;
begin
  inherited;
  NPER := 0;
end;

procedure TTDisDimensions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
begin
  Initialize;
  while not Stream.EndOfStream do
  begin
    ALine := Stream.ReadLine;
    if Stream.EndOfStream and (FOriginalStream <> nil) then
    begin
      Stream.Free;
      Stream := FOriginalStream;
      FOriginalStream := nil;
    end;
    ErrorLine := ALine;
    ALine := StripFollowingComments(ALine);
    if ALine = '' then
    begin
      Continue;
    end;


    ALine := UpperCase(ALine);
    if ReadEndOfSection(ALine, ErrorLine, 'DIMENSIONS', Unhandled) then
    begin
      Exit
    end;

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'DIMENSIONS') then
    begin
      // do nothing
    end
    else if FSplitter.Count >= 2 then
    begin
      if FSplitter[0] = 'NPER' then
      begin
        if not TryStrToInt(FSplitter[1], NPER) then
        begin
          Unhandled.WriteLine(StrUnrecognizedTDISDI);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine(StrUnrecognizedTDISDI);
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine(StrUnrecognizedTDISDI);
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TDisPeriodData }

constructor TTDisPeriodData.Create(PackageType: string);
begin
  FPeriods := TPeriodList.Create;
  inherited;

end;

destructor TTDisPeriodData.Destroy;
begin
  FPeriods.Free;
  inherited;
end;

procedure TTDisPeriodData.Initialize;
begin
  inherited;
  FPeriods.Clear;
end;

procedure TTDisPeriodData.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  SectionName: string;
  APeriod: TPeriod;
begin
  Initialize;
  while not Stream.EndOfStream do
  begin
    ALine := Stream.ReadLine;
    if Stream.EndOfStream and (FOriginalStream <> nil) then
    begin
      Stream.Free;
      Stream := FOriginalStream;
      FOriginalStream := nil;
    end;
    ErrorLine := ALine;
    ALine := StripFollowingComments(ALine);
    if ALine = '' then
    begin
      Continue;
    end;

    SectionName := 'PERIODDATA';
    if ReadEndOfSection(ALine, ErrorLine, SectionName, Unhandled) then
    begin
      Exit;
    end;

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, SectionName) then
    begin
      // do nothing
    end
    else if FSplitter.Count >= 3 then
    begin
      APeriod := TPeriod.Create;
      FPeriods.Add(APeriod);

      if not TryFortranStrToFloat(FSplitter[0], APeriod.PerLen) then
      begin
        Unhandled.WriteLine(Format('Unable to convert %s to a floating point number in the following Line',
          [FSplitter[0]]));
        Unhandled.WriteLine(ErrorLine);
      end;
      if not TryStrToInt(FSplitter[1], APeriod.NStp) then
      begin
        Unhandled.WriteLine(Format('Unable to convert %s to an integer in the following Line',
          [FSplitter[1]]));
        Unhandled.WriteLine(ErrorLine);
      end;
      if not TryFortranStrToFloat(FSplitter[2], APeriod.TSMult) then
      begin
        Unhandled.WriteLine(Format('Unable to convert %s to a floating point number in the following Line',
          [FSplitter[2]]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine('Unrecognized PERIODDATA option in the following line');
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TTDis }

constructor TTDis.Create(PackageType: string);
begin
  FOptions := TTDisOptions.Create(PackageType);
  FDimensions := TTDisDimensions.Create(PackageType);
  FPeriodData := TTDisPeriodData.Create(PackageType);

  inherited;

end;

destructor TTDis.Destroy;
begin
  FOptions.Free;
  FDimensions.Free;
  FPeriodData.Free;
  FAts.Free;
  inherited;
end;

procedure TTDis.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
begin
  try
    while not Stream.EndOfStream do
    begin
      ALine := Stream.ReadLine;
      ErrorLine := ALine;
      ALine := StripFollowingComments(ALine);
      if ALine = '' then
      begin
        Continue;
      end;

      ALine := UpperCase(ALine);
      if Pos('BEGIN', ALine) = 1 then
      begin
        if Trim(Copy(ALine,Length('BEGIN')+1,1)) <> '' then
        begin
          Unhandled.WriteLine(StrUnrecognizedTDISOp);
          Unhandled.WriteLine(ErrorLine);
          Continue;
        end;
        ALine := Trim(Copy(ALine, Length('BEGIN')+1, MaxInt)) ;
        if Pos('OPTIONS', ALine) = 1 then
        begin
          if Trim(Copy(ALine,Length('OPTIONS')+1,1)) <> '' then
          begin
            Unhandled.WriteLine(StrUnrecognizedTDISOp);
            Unhandled.WriteLine(ErrorLine);
            Continue;
          end;
          FOptions.Read(Stream, Unhandled)
        end
        else if Pos('DIMENSIONS', ALine) = 1 then
        begin
          if Trim(Copy(ALine,Length('DIMENSIONS')+1,1)) <> '' then
          begin
            Unhandled.WriteLine(StrUnrecognizedTDISOp);
            Unhandled.WriteLine(ErrorLine);
            Continue;
          end;
          FDimensions.Read(Stream, Unhandled)
        end
        else if Pos('PERIODDATA', ALine) = 1 then
        begin
          if Trim(Copy(ALine,Length('PERIODDATA')+1,1)) <> '' then
          begin
            Unhandled.WriteLine(StrUnrecognizedTDISOp);
            Unhandled.WriteLine(ErrorLine);
            Continue;
          end;
          FPeriodData.Read(Stream, Unhandled);
        end
        else
        begin
          Unhandled.WriteLine(StrUnrecognizedTDISOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end;
    end;
  finally
    if FDimensions.NPER <> FPeriodData.FPeriods.Count then
    begin
      Unhandled.WriteLine(Format(StrNPERDInTheTD,
        [FDimensions.NPER, FPeriodData.FPeriods.Count]));
    end;
  end;
end;

procedure TTDis.ReadInput(Unhandled: TStreamWriter);
var
  AtsFile: TStreamReader;
begin
  if FOptions.ATS6_FileName <> '' then
  begin
    if TFile.Exists(FOptions.ATS6_FileName) then
    begin
      try
        AtsFile := TFile.OpenText(FOptions.ATS6_FileName);
        try
          try
            FAts := TAts.Create('ATS');
            FAts.Read(AtsFile, Unhandled);
          except on E: Exception do
            begin
              Unhandled.WriteLine('ERROR');
              Unhandled.WriteLine(E.Message);
            end;
          end;
        finally
          AtsFile.Free;
        end;
      except on E: Exception do
        begin
          Unhandled.WriteLine('ERROR');
          Unhandled.WriteLine(E.Message);
        end;
      end
    end
    else
    begin
      Unhandled.WriteLine(Format('Unable to open %s because it does not exist.',
        [FOptions.ATS6_FileName]));
    end;
  end;
end;

end.
