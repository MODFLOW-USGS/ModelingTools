unit Mf6.AtsFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TAtsDimensions = class(TCustomMf6Persistent)
  public
    MAXATS: Integer;
    procedure Initialize; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  end;

  TAtsPeriod = class(TObject)
    iperats: Integer;
    dt0: Extended;
    dtmin: Extended;
    dtmax: Extended;
    dtadj: Extended;
    dtfailadj: Extended;
  end;

  TAtsPeriodList = TObjectList<TAtsPeriod>;

  TAtsPeriodData = class(TCustomMf6Persistent)
  private
    FPeriods: TAtsPeriodList;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  end;

  TAts = class(TCustomMf6Persistent)
  private
    FDimensions: TAtsDimensions;
    FPeriodData: TAtsPeriodData;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  end;

implementation

uses
  ModelMuseUtilities;

resourcestring
  StrUnrecognizedATSDIM = 'Unrecognized ATS DIMENSION option in the followin' +
  'g line.';
  StrMAXATSDInThe = 'MAXATS, "%d" in the ATS DIMENSIONS block does not match' +
  ' the number of stress periods listed in the PERIODDATA block "%d".';
  StrUnrecognizedAdsOp = 'Unrecognized ADS option in the following line.';

{ TAtsDimensions }

procedure TAtsDimensions.Initialize;
begin
  inherited;
  MAXATS := 0;
end;

procedure TAtsDimensions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
begin
  Initialize;
  while not Stream.EndOfStream do
  begin
    ALine := Stream.ReadLine;
    RestoreStream(Stream);
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
      if FSplitter[0] = 'MAXATS' then
      begin
        if not TryStrToInt(FSplitter[1], MAXATS) then
        begin
          Unhandled.WriteLine(StrUnrecognizedATSDIM);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine(StrUnrecognizedATSDIM);
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine(StrUnrecognizedATSDIM);
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;


{ TAts }

constructor TAts.Create(PackageType: string);
begin
  inherited;
  FDimensions := TAtsDimensions.Create(PackageType);
  FPeriodData := TAtsPeriodData.Create(PackageType);
end;

destructor TAts.Destroy;
begin
  FDimensions.Free;
  FPeriodData.Free;
  inherited;
end;

procedure TAts.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
          Unhandled.WriteLine(StrUnrecognizedAdsOp);
          Unhandled.WriteLine(ErrorLine);
          Continue;
        end;
        ALine := Trim(Copy(ALine, Length('BEGIN')+1, MaxInt)) ;
        if Pos('DIMENSIONS', ALine) = 1 then
        begin
          if Trim(Copy(ALine,Length('DIMENSIONS')+1,1)) <> '' then
          begin
            Unhandled.WriteLine(StrUnrecognizedAdsOp);
            Unhandled.WriteLine(ErrorLine);
            Continue;
          end;
          FDimensions.Read(Stream, Unhandled)
        end
        else if Pos('PERIODDATA', ALine) = 1 then
        begin
          if Trim(Copy(ALine,Length('PERIODDATA')+1,1)) <> '' then
          begin
            Unhandled.WriteLine(StrUnrecognizedAdsOp);
            Unhandled.WriteLine(ErrorLine);
            Continue;
          end;
          FPeriodData.Read(Stream, Unhandled);
        end
        else
        begin
          Unhandled.WriteLine(StrUnrecognizedAdsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end;
    end;
  finally
    if FDimensions.MAXATS <> FPeriodData.FPeriods.Count then
    begin
      Unhandled.WriteLine(Format(StrMAXATSDInThe,
        [FDimensions.MAXATS, FPeriodData.FPeriods.Count]));
    end;
  end;
end;

{ TAtsPeriodData }

constructor TAtsPeriodData.Create(PackageType: string);
begin
  FPeriods := TAtsPeriodList.Create;
  inherited;

end;

destructor TAtsPeriodData.Destroy;
begin
  FPeriods.Free;
  inherited;
end;

procedure TAtsPeriodData.Initialize;
begin
  inherited;
  FPeriods.Clear;
end;

procedure TAtsPeriodData.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  SectionName: string;
  APeriod: TAtsPeriod;
begin
  Initialize;
  while not Stream.EndOfStream do
  begin
    ALine := Stream.ReadLine;
    RestoreStream(Stream);
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
    else if FSplitter.Count >= 6 then
    begin
      APeriod := TAtsPeriod.Create;
      FPeriods.Add(APeriod);

      if not TryStrToInt(FSplitter[0], APeriod.iperats) then
      begin
        Unhandled.WriteLine(Format('Unable to convert %s to an integer in the following Line',
          [FSplitter[0]]));
        Unhandled.WriteLine(ErrorLine);
      end;
      if not TryFortranStrToFloat(FSplitter[1], APeriod.dt0) then
      begin
        Unhandled.WriteLine(Format('Unable to convert %s to a floating point number in the following Line',
          [FSplitter[1]]));
        Unhandled.WriteLine(ErrorLine);
      end;
      if not TryFortranStrToFloat(FSplitter[2], APeriod.dtmin) then
      begin
        Unhandled.WriteLine(Format('Unable to convert %s to a floating point number in the following Line',
          [FSplitter[2]]));
        Unhandled.WriteLine(ErrorLine);
      end;
      if not TryFortranStrToFloat(FSplitter[3], APeriod.dtmax) then
      begin
        Unhandled.WriteLine(Format('Unable to convert %s to a floating point number in the following Line',
          [FSplitter[3]]));
        Unhandled.WriteLine(ErrorLine);
      end;
      if not TryFortranStrToFloat(FSplitter[4], APeriod.dtadj) then
      begin
        Unhandled.WriteLine(Format('Unable to convert %s to a floating point number in the following Line',
          [FSplitter[4]]));
        Unhandled.WriteLine(ErrorLine);
      end;
      if not TryFortranStrToFloat(FSplitter[5], APeriod.dtfailadj) then
      begin
        Unhandled.WriteLine(Format('Unable to convert %s to a floating point number in the following Line',
          [FSplitter[5]]));
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

end.
