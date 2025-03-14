unit Mf6.SpcFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TSpcOptions = class(TCustomMf6Persistent)
  private
    PRINT_INPUT: Boolean;
    READASARRAYS: Boolean;
    TS6: TStringList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TSpcDimensions = class(TCustomMf6Persistent)
  private
    MAXBOUND: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TSpcTimeItem = record
    bndno: Integer;
    spcsetting: TMf6BoundaryValue;
    procedure Initialize;
  end;

  TSpcTimeItemList = TList<TSpcTimeItem>;
  TSpcDictionary = TDictionary<Integer, TSpcTimeItem>;
  TSpcDictionaries = TObjectList<TSpcDictionary>;
  TSpcTimeItemLists = TObjectList<TSpcTimeItemList>;
  TListOfSpcTimeItemLists = TObjectList<TSpcTimeItemLists>;

  TSpcPeriod = class(TCustomMf6Persistent)
  private
    iper: Integer;
    FItems: TSpcTimeItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      Dimensions: TDimensions; READASARRAYS: Boolean);
    function GetCount: Integer;
    function GetItem(Index: Integer): TSpcTimeItem;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Period: Integer read iper;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TSpcTimeItem read GetItem; default;
  end;

  TSpcPeriodList = TObjectList<TSpcPeriod>;
  TSpcPeriodArray = TArray<TSpcPeriod>;

  TSpc = class(TDimensionedPackageReader)
  private
    FOptions: TSpcOptions;
    FSpcDimensions: TSpcDimensions;
    FPeriods: TSpcPeriodList;
    FTimeSeriesPackages: TPackageList;
    function GetPeriod(Index: Integer): TSpcPeriod;
    function GetPeriodCount: Integer;
    function GetTimeSeries(Index: Integer): TPackage;
    function GetTimeSeriesCount: Integer;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      const NPER: Integer); override;
    property PeriodCount: Integer read GetPeriodCount;
    property Periods[Index: Integer]: TSpcPeriod read GetPeriod; default;
    property TimeSeriesCount: Integer read GetTimeSeriesCount;
    property TimeSeries[Index: Integer]: TPackage read GetTimeSeries;
  end;

  TSpcList = TList<TSpc>;

implementation

uses
  Mf6.TimeSeriesFileReaderUnit, ModelMuseUtilities;

{ TSpcOptions }

constructor TSpcOptions.Create(PackageType: string);
begin
  TS6 := TStringList.Create;
  inherited;

end;

destructor TSpcOptions.Destroy;
begin
  TS6.Free;
  inherited;
end;

procedure TSpcOptions.Initialize;
begin
  inherited;
  TS6.Clear;
  PRINT_INPUT := False;
  READASARRAYS := False;
end;

procedure TSpcOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
  TS6_FileName: string;
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
    if ReadEndOfSection(ALine, ErrorLine, 'OPTIONS', Unhandled) then
    begin
      Exit
    end;

    CaseSensitiveLine := ALine;
    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'OPTIONS') then
    begin
      // do nothing
    end
    else if FSplitter[0] = 'PRINT_INPUT' then
    begin
      PRINT_INPUT := True;
    end
    else if FSplitter[0] = 'READASARRAYS' then
    begin
      READASARRAYS := True;
    end
    else if (FSplitter[0] = 'TS6')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEIN') then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      TS6_FileName := FSplitter[2];
      TS6.Add(TS6_FileName);
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TSpcDimensions }

procedure TSpcDimensions.Initialize;
begin
  inherited;
  MAXBOUND := 0;
end;

procedure TSpcDimensions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    if ReadEndOfSection(ALine, ErrorLine, 'DIMENSIONS', Unhandled) then
    begin
      Exit
    end;

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'DIMENSIONS') then
    begin
      // do nothing
    end
    else if (FSplitter[0] = 'MAXBOUND') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], MAXBOUND) then
    begin
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TSpcTimeItem }

procedure TSpcTimeItem.Initialize;
begin
  bndno := 0;
  spcsetting.Initialize;
end;

{ TSpcPeriod }

constructor TSpcPeriod.Create(PackageType: string);
begin
  FItems := TSpcTimeItemList.Create;
  inherited;

end;

destructor TSpcPeriod.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TSpcPeriod.GetCount: Integer;
begin
  result := FItems.Count;
end;

function TSpcPeriod.GetItem(Index: Integer): TSpcTimeItem;
begin
  result := FItems[Index];
end;

procedure TSpcPeriod.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TSpcPeriod.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  Dimensions: TDimensions; READASARRAYS: Boolean);
var
  SpcTimeItem: TSpcTimeItem;
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
  NumberOfItems: Integer;
  Concentration: TArrayItem;
  bndno: Integer;
  LocalDim: TDimensions;
  Double2DDReader: TDouble2DArrayReader;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  NumberOfItems := 3;
  Initialize;

  if READASARRAYS then
  begin
    LocalDim := Dimensions;
    LocalDim.NLay := 1;
    Concentration.Initialize;
    try
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

        if ReadEndOfSection(ALine, ErrorLine, 'PERIOD', Unhandled) then
        begin
          Exit;
        end;

        CaseSensitiveLine := ALine;
        if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'PERIOD') then
        begin
          // do nothing
        end
        else if AnsiSametext(FSplitter[0], 'CONCENTRATION') then
        begin
          if (FSplitter.Count >= 3) and AnsiSametext(FSplitter[1], 'TIMEARRAYSERIES') then
          begin
            FSplitter.DelimitedText := CaseSensitiveLine;
            CONCENTRATION.TimeArraySeries := FSplitter[2]
          end
          else
          begin
            Double2DDReader := TDouble2DArrayReader.Create(LocalDim, FPackageType);
            try
              Double2DDReader.Read(Stream, Unhandled);
              CONCENTRATION.value := Double2DDReader.FData;
            finally
              Double2DDReader.Free;
            end;
          end;
        end
      end;
    finally
      bndno := 0;
      for RowIndex := 0 to LocalDim.NRow - 1 do
      begin
        for ColIndex := 0 to LocalDim.NCol - 1 do
        begin
          SpcTimeItem.Initialize;
          Inc(bndno);
          SpcTimeItem.bndno := bndno;
          if Concentration.Value <> nil then
          begin
            SpcTimeItem.spcsetting.NumericValue := CONCENTRATION.Value[RowIndex, ColIndex];
            SpcTimeItem.spcsetting.ValueType := vtNumeric;
          end
          else
          begin
            SpcTimeItem.spcsetting.ValueType := vtString;
            SpcTimeItem.spcsetting.StringValue := CONCENTRATION.TimeArraySeries + '_'
              + IntToStr(RowIndex + 1) + '_' + IntToStr(ColIndex + 1)
          end;

          FItems.Add(SpcTimeItem);
        end;
      end;
      Concentration.Initialize;
    end;
  end
  else
  begin
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

      if ReadEndOfSection(ALine, ErrorLine, 'PERIOD', Unhandled) then
      begin
        Exit;
      end;
      SpcTimeItem.Initialize;
      if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'PERIOD') then
      begin
        // do nothing
      end
      else if FSplitter.Count >= NumberOfItems then
      begin
        if TryStrToInt(FSplitter[0], SpcTimeItem.bndno)
          and (FSplitter[1] = 'CONCENTRATION')
          then
        begin
          if TryFortranStrToFloat(FSplitter[2], SpcTimeItem.spcsetting.NumericValue) then
          begin
            SpcTimeItem.spcsetting.ValueType := vtNumeric;
          end
          else
          begin
            SpcTimeItem.spcsetting.ValueType := vtString;
            FSplitter.DelimitedText := CaseSensitiveLine;
            SpcTimeItem.spcsetting.StringValue := FSplitter[2];
          end;
          FItems.Add(SpcTimeItem);
        end
        else
        begin
          Unhandled.WriteLine(Format(StrUnrecognizedSPERI, [FPackageType]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSPERI, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end;
  end;
end;

{ TSpc }

constructor TSpc.Create(PackageType: string);
begin
  inherited;
  FOptions := TSpcOptions.Create(PackageType);
  FSpcDimensions := TSpcDimensions.Create(PackageType);
  FPeriods := TSpcPeriodList.Create;
  FTimeSeriesPackages := TPackageList.Create;
end;

destructor TSpc.Destroy;
begin
  FOptions.Free;
  FSpcDimensions.Free;
  FPeriods.Free;
  FTimeSeriesPackages.Free;
  inherited;
end;

function TSpc.GetPeriod(Index: Integer): TSpcPeriod;
begin
  result := FPeriods[Index];
end;

function TSpc.GetPeriodCount: Integer;
begin
  result := FPeriods.Count;
end;

function TSpc.GetTimeSeries(Index: Integer): TPackage;
begin
  result := FTimeSeriesPackages[Index];
end;

function TSpc.GetTimeSeriesCount: Integer;
begin
  result := FTimeSeriesPackages.Count;
end;

procedure TSpc.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
var
  ALine: string;
  ErrorLine: string;
  IPER: Integer;
  APeriod: TSpcPeriod;
  TsPackage: TPackage;
  PackageIndex: Integer;
  TsReader: TTimeSeries;
begin
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading SPC package');
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
      if FSplitter[1] ='OPTIONS' then
      begin
        FOptions.Read(Stream, Unhandled);
      end
      else if FSplitter[1] ='DIMENSIONS' then
      begin
        FSpcDimensions.Read(Stream, Unhandled);
      end
      else if (FSplitter[1] ='PERIOD') and (FSplitter.Count >= 3) then
      begin
        if TryStrToInt(FSplitter[2], IPER) then
        begin
          if IPER > NPER then
          begin
            break;
          end;
          APeriod := TSpcPeriod.Create(FPackageType);
          FPeriods.Add(APeriod);
          APeriod.IPer := IPER;
          APeriod.Read(Stream, Unhandled, FDimensions, FOptions.READASARRAYS);
        end
        else
        begin
          Unhandled.WriteLine(Format(StrUnrecognizedSData, [FPackageType]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSData, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSData, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
  for PackageIndex := 0 to FOptions.TS6.Count - 1 do
  begin
    TsPackage := TPackage.Create;
    FTimeSeriesPackages.Add(TsPackage);
    TsPackage.FileType := FPackageType;
    TsPackage.FileName := FOptions.TS6[PackageIndex];
    TsPackage.PackageName := '';

    TsReader := TTimeSeries.Create(FPackageType);
    TsPackage.Package := TsReader;
    TsPackage.ReadPackage(Unhandled, NPER);
  end;
end;

end.
