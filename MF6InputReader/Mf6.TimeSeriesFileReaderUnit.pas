unit Mf6.TimeSeriesFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TTsMethod = (tmStepWise, tmLinear, tmLinearEnd, tsUndefined);
  TTsMethodList = TList<TTsMethod>;

  TTsAttributes = class(TCustomMf6Persistent)
  private
    FNames: TStringList;
    FMethods: TTsMethodList;
    FSFACS: TDoubleList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    function GetMethod(Index: Integer): TTsMethod;
    function GetMethodCount: Integer;
    function GetName(Index: Integer): string;
    function GetNameCount: Integer;
    function GetSfac(Index: Integer): double;
    function GetSfacCount: Integer;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property NameCount: Integer read GetNameCount;
    property Names[Index: Integer]: string read GetName;
    property MethodCount: Integer read GetMethodCount;
    property Methods[Index: Integer]: TTsMethod read GetMethod;
    property SfacCount: Integer read GetSfacCount;
    property SFacs[Index: Integer]: double read GetSfac;
  end;

  TTimeValues = TObjectList<TDoubleList>;

  TTsTimeSeries = class(TCustomMf6Persistent)
  private
    FNumberOfTimeSeries: Integer;
    FTimes: TDoubleList;
    FValues: TTimeValues;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    function GetNumberOfTimeSeries: Integer;
    function GetTime(Index: Integer): double;
    function GetTimeCount: Integer;
    function GetTimeSeriesValues(Index: integer): TDoubleList;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property TimeCount: Integer read GetTimeCount;
    property Times[Index: Integer]: double read GetTime;
    property NumberOfTimeSeries: Integer read GetNumberOfTimeSeries;
    property TimeSeriesValues[Index: integer]: TDoubleList read GetTimeSeriesValues;
  end;

  TTimeSeries = class(TPackageReader)
  private
    FAttributes: TTsAttributes;
    FTimeSeries: TTsTimeSeries;
  public
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer); override;
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Attributes: TTsAttributes read FAttributes;
    property TimeSeries: TTsTimeSeries read FTimeSeries;
  end;

implementation

uses
  ModelMuseUtilities;

{ TTimeSeriesOptions }

constructor TTsAttributes.Create(PackageType: string);
begin
  FNames := TStringList.Create;
  FMethods := TTsMethodList.Create;
  FSFACS := TDoubleList.Create;
  inherited;

end;

destructor TTsAttributes.Destroy;
begin
  FNames.Free;
  FMethods.Free;
  FSFACS.Free;
  inherited;
end;

function TTsAttributes.GetMethod(Index: Integer): TTsMethod;
begin
  result := FMethods[Index];
end;

function TTsAttributes.GetMethodCount: Integer;
begin
  result := FMethods.Count;
end;

function TTsAttributes.GetName(Index: Integer): string;
begin
  result := FNames[Index];
end;

function TTsAttributes.GetNameCount: Integer;
begin
  result := FNames.Count;
end;

function TTsAttributes.GetSfac(Index: Integer): double;
begin
  result := FSFACS[Index];
end;

function TTsAttributes.GetSfacCount: Integer;
begin
  result := FSFACS.Count;
end;

procedure TTsAttributes.Initialize;
begin
  inherited;
  FNames.Clear;
  FMethods.Clear;
  FSFACS.Clear;
end;

procedure TTsAttributes.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
  Tag: string;
  ValidMethods: TStringList;
  Index: Integer;
  ItemIndex: Integer;
  Method: TTsMethod;
  AValue: Extended;
begin
  ValidMethods := TStringList.Create;
  try
    ValidMethods.Capacity := 3;
    ValidMethods.Add('STEPWISE');
    ValidMethods.Add('LINEAR');
    ValidMethods.Add('LINEAREND');
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
      if ReadEndOfSection(ALine, ErrorLine, 'ATTRIBUTES', Unhandled) then
      begin
        Assert(FMethods.Count >= FNames.Count);
        if FSFACS.Count > 0 then
        begin
          Assert(FSFACS.Count >= FNames.Count);
        end;
        Exit
      end;

      CaseSensitiveLine := ALine;
      if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'ATTRIBUTES') then
      begin
        // do nothing
      end
      else
      begin
        FSplitter.DelimitedText := CaseSensitiveLine;
        Tag := UpperCase(FSplitter[0]);
        if (Tag = 'NAMES') or (Tag = 'NAME') then
        begin
          FSplitter.Delete(0);
          FNames.Assign(FSplitter);
          if (FMethods.Count > 0) and (FMethods.Count < FNames.Count) then
          begin
            Method := FMethods.Last;
            while FMethods.Count < FNames.Count do
            begin
              FMethods.Add(Method);
            end;
          end;
        end
        else if (Tag = 'METHODS') then
        begin
          ALine := UpperCase(ALine);
          FSplitter.DelimitedText := ALine;
          FSplitter.Delete(0);
          FMethods.Capacity := FSplitter.Count;
          for Index := 0 to FSplitter.Count - 1 do
          begin
            ItemIndex := ValidMethods.IndexOf(FSplitter[Index]);
            if ItemIndex >= 0 then
            begin
              FMethods.Add(TTsMethod(ItemIndex));
            end
            else
            begin
              FMethods.Add(tsUndefined);
            end;
          end;
        end
        else if (Tag = 'METHOD') then
        begin
          ALine := UpperCase(ALine);
          FSplitter.DelimitedText := ALine;
          FSplitter.Delete(0);
          ItemIndex := ValidMethods.IndexOf(FSplitter[0]);
          if ItemIndex >= 0 then
          begin
            Method := TTsMethod(ItemIndex);
          end
          else
          begin
            Method := tsUndefined;
          end;
          FMethods.Capacity := FNames.Count;
          for Index := 0 to FNames.Count - 1 do
          begin
            FMethods.Add(Method);
          end;
          if FMethods.Count = 0 then
          begin
            FMethods.Add(Method);
          end;
        end
        else if (Tag = 'SFACS') then
        begin
          FSplitter.Delete(0);
          FSFACS.Capacity := FSplitter.Count;
          for Index := 0 to FSplitter.Count - 1 do
          begin
            if TryFortranStrToFloat(FSplitter[Index], AValue) then
            begin
              FSFACS.Add(AValue)
            end
            else
            begin
              if FSFACS.Count < FNames.Count then
              begin
                Unhandled.WriteLine('Insufficient number of scale factors in the following line.');
                Unhandled.WriteLine(ErrorLine);
              end;
              Break;
            end;
          end;
        end
        else if (Tag = 'SFAC') then
        begin
          FSplitter.Delete(0);
          FSFACS.Capacity := FNames.Count;
          if TryFortranStrToFloat(FSplitter[0], AValue) then
          begin
            for Index := 0 to FNames.Count - 1 do
            begin
              FSFACS.Add(AValue);
            end;
          end;
        end
        else
        begin
          Unhandled.WriteLine('Unrecognized Time Series option in the following line.');
          Unhandled.WriteLine(ErrorLine);
        end;
      end;
    end
  finally
    ValidMethods.Free;
  end;
end;

{ TTsTimeSeries }

constructor TTsTimeSeries.Create(PackageType: string);
begin
  FTimes := TDoubleList.Create;
  FValues := TTimeValues.Create;
  inherited Create(PackageType);
end;

destructor TTsTimeSeries.Destroy;
begin
  FTimes.Free;
  FValues.Free;
  inherited;
end;

function TTsTimeSeries.GetNumberOfTimeSeries: Integer;
begin
  result := FValues.Count;
end;

function TTsTimeSeries.GetTime(Index: Integer): double;
begin
  result := FTimes[Index];
end;

function TTsTimeSeries.GetTimeCount: Integer;
begin
  result := FTimes.Count;
end;

function TTsTimeSeries.GetTimeSeriesValues(Index: integer): TDoubleList;
begin
  result := FValues[Index];
end;

procedure TTsTimeSeries.Initialize;
begin
  FTimes.Clear;
  FValues.Clear;
  inherited;
end;

procedure TTsTimeSeries.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  TimeValues: TDoubleList;
  AValue: Extended;
  Index: Integer;
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
    if ReadEndOfSection(ALine, ErrorLine, 'TIMESERIES', Unhandled) then
    begin
      Exit
    end;

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'TIMESERIES') then
    begin
      Continue;
    end;

    if TryFortranStrToFloat(FSplitter[0], AValue) then
    begin
      FTimes.Add(AValue);
    end
    else
    begin
      Unhandled.WriteLine('Invalid time value in a time series in the following line.');
      Unhandled.WriteLine(ErrorLine);
    end;

    TimeValues := nil;
    for Index := 1 to FSplitter.Count - 1 do
    begin
      if TryFortranStrToFloat(FSplitter[Index], AValue) then
      begin
        if Index - 1 < FValues.Count then
        begin
          TimeValues := FValues[Index - 1];
        end
        else
        begin
          TimeValues := TDoubleList.Create;
          FValues.Add(TimeValues);
        end;
        TimeValues.Add(AValue);
      end
      else
      begin
        Break;
      end;
    end;
    if (TimeValues = nil) or (TimeValues.Count < FNumberOfTimeSeries) then
    begin
      Unhandled.WriteLine('Invalid or missing tsr-value in a time series in the following line.');
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TTimeSeries }

constructor TTimeSeries.Create(PackageType: string);
begin
  FAttributes := TTsAttributes.Create(PackageType);
  FTimeSeries := TTsTimeSeries.Create(PackageType);
  inherited;
end;

destructor TTimeSeries.Destroy;
begin
  FAttributes.Free;
  FTimeSeries.Free;
  inherited;
end;

procedure TTimeSeries.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
var
  ALine: string;
  ErrorLine: string;
begin
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading time-series');
  end;
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
    FSplitter.DelimitedText := ALine;
    if FSplitter[0] = 'BEGIN' then
    begin
      if FSplitter[1] ='ATTRIBUTES' then
      begin
        FAttributes.Read(Stream, Unhandled);
      end
      else if FSplitter[1] ='TIMESERIES' then
      begin
        FTimeSeries.FNumberOfTimeSeries := FAttributes.FNames.Count;
        FTimeSeries.Read(Stream, Unhandled);
      end
      else
      begin
        Unhandled.WriteLine('Unrecognized Times series data in the following line.');
        Unhandled.WriteLine(ErrorLine);
      end;
    end;
  end;
end;

end.
