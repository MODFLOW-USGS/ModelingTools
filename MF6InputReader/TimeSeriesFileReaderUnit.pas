unit TimeSeriesFileReaderUnit;

interface
uses
  System.Classes, System.IOUtils, System.SysUtils, CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TTsMethod = (tmStepWise, tmLinear, tmLinearEnd, tsUndefined);
  TTsMethodList = TList<TTsMethod>;

  TTsAttributes = class(TCustomMf6Persistent)
  private
    Names: TStringList;
    Methods: TTsMethodList;
    SFACS: TDoubleList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TTimeValues = TObjectList<TDoubleList>;

  TTsTimeSeries = class(TCustomMf6Persistent)
  private
    FNumberOfTimeSeries: Integer;
    Times: TDoubleList;
    Values: TTimeValues;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TTimeSeries = class(TPackageReader)
    FAttributes: TTsAttributes;
    FTimeSeries: TTsTimeSeries;
  public
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TTimeSeriesOptions }

constructor TTsAttributes.Create;
begin
  Names := TStringList.Create;
  Methods := TTsMethodList.Create;
  SFACS := TDoubleList.Create;
  inherited;

end;

destructor TTsAttributes.Destroy;
begin
  Names.Free;
  Methods.Free;
  SFACS.Free;
  inherited;
end;

procedure TTsAttributes.Initialize;
begin
  inherited;
  Names.Clear;
  Methods.Clear;
  SFACS.Clear;
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
  AValue: Double;
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
      ErrorLine := ALine;
      ALine := StripFollowingComments(ALine);
      if ALine = '' then
      begin
        Continue;
      end;
      if ReadEndOfSection(ALine, ErrorLine, 'ATTRIBUTES', Unhandled) then
      begin
        Assert(Methods.Count >= Names.Count);
        if SFACS.Count > 0 then
        begin
          Assert(SFACS.Count >= Names.Count);
        end;
        Exit
      end;

      FSplitter.DelimitedText := ALine;
      Assert(FSplitter.Count > 0);
      Tag := UpperCase(FSplitter[0]);
      if (Tag = 'NAMES') or (Tag = 'NAME') then
      begin
        FSplitter.Delete(0);
        Names.Assign(FSplitter);
        if (Methods.Count > 0) and (Methods.Count < Names.Count) then
        begin
          Method := Methods.Last;
          while Methods.Count < Names.Count do
          begin
            Methods.Add(Method);
          end;
        end;
      end
      else if (Tag = 'METHODS') then
      begin
        ALine := UpperCase(ALine);
        FSplitter.DelimitedText := ALine;
        FSplitter.Delete(0);
        Methods.Capacity := FSplitter.Count;
        for Index := 0 to FSplitter.Count - 1 do
        begin
          ItemIndex := ValidMethods.IndexOf(FSplitter[Index]);
          if ItemIndex >= 0 then
          begin
            Methods.Add(TTsMethod(ItemIndex));
          end
          else
          begin
            Methods.Add(tsUndefined);
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
        Methods.Capacity := Names.Count;
        for Index := 0 to Names.Count - 1 do
        begin
          Methods.Add(Method);
        end;
        if Methods.Count = 0 then
        begin
          Methods.Add(Method);
        end;
      end
      else if (Tag = 'SFACS') then
      begin
        FSplitter.Delete(0);
        SFACS.Capacity := FSplitter.Count;
        for Index := 0 to FSplitter.Count - 1 do
        begin
          if TryStrToFloat(FSplitter[Index], AValue) then
          begin
            SFACS.Add(AValue)
          end
          else
          begin
            if SFACS.Count < Names.Count then
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
        SFACS.Capacity := Names.Count;
        if TryStrToFloat(FSplitter[0], AValue) then
        begin
          for Index := 0 to Names.Count - 1 do
          begin
            SFACS.Add(AValue);
          end;
        end;
      end
      else
      begin
        Unhandled.WriteLine('Unrecognized Time Series option in the following line.');
        Unhandled.WriteLine(ErrorLine);
      end;
    end
  finally
    ValidMethods.Free;
  end;
end;

{ TTsTimeSeries }

constructor TTsTimeSeries.Create;
begin
  Times := TDoubleList.Create;
  Values := TTimeValues.Create;
  inherited Create;
end;

destructor TTsTimeSeries.Destroy;
begin
  Times.Free;
  Values.Free;
  inherited;
end;

procedure TTsTimeSeries.Initialize;
begin
  Times.Clear;
  Values.Clear;
  inherited;
end;

procedure TTsTimeSeries.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  TimeValues: TDoubleList;
  AValue: Double;
  Index: Integer;
begin
  Initialize;
  while not Stream.EndOfStream do
  begin
    ALine := Stream.ReadLine;
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

    FSplitter.DelimitedText := ALine;
    TimeValues := TDoubleList.Create;
    Values.Add(TimeValues);

    if TryStrToFloat(FSplitter[0], AValue) then
    begin
      Times.Add(AValue);
    end
    else
    begin
      Unhandled.WriteLine('Invalid time value in a time series in the following line.');
      Unhandled.WriteLine(ErrorLine);
    end;

    for Index := 1 to FSplitter.Count - 1 do
    begin
      if TryStrToFloat(FSplitter[Index], AValue) then
      begin
        TimeValues.Add(AValue);
      end
      else
      begin
        Break;
      end;
    end;
    if TimeValues.Count < FNumberOfTimeSeries then
    begin
      Unhandled.WriteLine('Invalid or missing tsr-value in a time series in the following line.');
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TTimeSeries }

constructor TTimeSeries.Create;
begin
  FAttributes := TTsAttributes.Create;
  FTimeSeries := TTsTimeSeries.Create;
  inherited;
end;

destructor TTimeSeries.Destroy;
begin
  FAttributes.Free;
  FTimeSeries.Free;
  inherited;
end;

procedure TTimeSeries.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
begin
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
        FTimeSeries.FNumberOfTimeSeries := FAttributes.Names.Count;
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
