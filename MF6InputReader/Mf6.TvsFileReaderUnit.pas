unit Mf6.TvsFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TTvsOptions = class(TCustomMf6Persistent)
  private
    DISABLE_STORAGE_CHANGE_INTEGRATION: Boolean;
    PRINT_INPUT: Boolean;
    TS6_FileNames: TStringList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TTvsPeriodData = class(TCustomMf6Persistent)
  private
    IPer: Integer;
    FCells: TTimeVariableCellList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TTvsPeriodList = TObjectList<TTvsPeriodData>;

  TTvs = class(TDimensionedPackageReader)
  private
    FOptions: TTvsOptions;
    FPeriods: TTvsPeriodList;
    FTimeSeriesPackages: TPackageList;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
  end;


implementation

uses
  ModelMuseUtilities, Mf6.TimeSeriesFileReaderUnit;

{ TTvsOptions }

constructor TTvsOptions.Create(PackageType: string);
begin
  TS6_FileNames := TStringList.Create;
  inherited;

end;

destructor TTvsOptions.Destroy;
begin
  TS6_FileNames.Free;
  inherited;
end;

procedure TTvsOptions.Initialize;
begin
  inherited;
  DISABLE_STORAGE_CHANGE_INTEGRATION := False;
  PRINT_INPUT := False;
  TS6_FileNames.Clear;
end;

procedure TTvsOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    Assert(FSplitter.Count > 0);
    if FSplitter[0] = 'PRINT_INPUT' then
    begin
      PRINT_INPUT := True;
    end
    else if FSplitter[0] = 'DISABLE_STORAGE_CHANGE_INTEGRATION' then
    begin
      DISABLE_STORAGE_CHANGE_INTEGRATION := True;
    end
    else if (FSplitter[0] = 'TS6')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEIN') then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      TS6_FileName := FSplitter[2];
      TS6_FileNames.Add(TS6_FileName);
    end
    else
    begin
      Unhandled.WriteLine('Unrecognized TVS option in the following line.');
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TTvsPeriodData }

constructor TTvsPeriodData.Create(PackageType: string);
begin
  FCells := TTimeVariableCellList.Create;
  inherited;

end;

destructor TTvsPeriodData.Destroy;
begin
  FCells.Free;
  inherited;
end;

procedure TTvsPeriodData.Initialize;
begin
  inherited;
  FCells.Clear;
end;

procedure TTvsPeriodData.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  Dimensions: TDimensions);
var
  DimensionCount: Integer;
  Cell: TTimeVariableCell;
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
begin
  DimensionCount := Dimensions.DimensionCount;
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

    if ReadEndOfSection(ALine, ErrorLine, 'PERIOD', Unhandled) then
    begin
      Exit;
    end;

    Cell.Initialize;
    CaseSensitiveLine := ALine;
    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    if FSplitter.Count >= DimensionCount + 2 then
    begin
      if ReadCellID(Cell.CellId, 0, DimensionCount) then
      begin
        Cell.VariableName := FSplitter[DimensionCount];
        if TryFortranStrToFloat(FSplitter[DimensionCount+1], Cell.NumericValue) then
        begin
          Cell.ValueType := vtNumeric;
        end
        else
        begin
          Cell.ValueType := vtString;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Cell.StringValue := FSplitter[DimensionCount+1];
        end;
        FCells.Add(Cell);
      end
      else
      begin
        Unhandled.WriteLine('Unrecognized TVS PERIOD data in the following line.');
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine('Unrecognized TVS PERIOD data in the following line.');
      Unhandled.WriteLine(ErrorLine);
    end;
  end;

end;

{ TTvs }

constructor TTvs.Create(PackageType: string);
begin
  inherited;
  FOptions := TTvsOptions.Create(PackageType);
  FPeriods := TTvsPeriodList.Create;
  FTimeSeriesPackages := TPackageList.Create;
end;

destructor TTvs.Destroy;
begin
  FTimeSeriesPackages.Free;
  FOptions.Free;
  FPeriods.Free;
  inherited;
end;

procedure TTvs.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  IPER: Integer;
  APeriod: TTvsPeriodData;
  TsPackage: TPackage;
  PackageIndex: Integer;
  TsReader: TTimeSeries;
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
      if FSplitter[1] ='OPTIONS' then
      begin
        FOptions.Read(Stream, Unhandled);
      end
      else if (FSplitter[1] ='PERIOD') and (FSplitter.Count >= 3) then
      begin
        if TryStrToInt(FSplitter[2], IPER) then
        begin
          APeriod := TTvsPeriodData.Create(FPackageType);
          FPeriods.Add(APeriod);
          APeriod.IPer := IPER;
          APeriod.Read(Stream, Unhandled, FDimensions);
        end
        else
        begin
          Unhandled.WriteLine('Unrecognized TVS data in the following line.');
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine('Unrecognized TVS data in the following line.');
        Unhandled.WriteLine(ErrorLine);
      end;
    end;
  end;
  for PackageIndex := 0 to FOptions.TS6_FileNames.Count - 1 do
  begin
    TsPackage := TPackage.Create;
    FTimeSeriesPackages.Add(TsPackage);
    TsPackage.FileType := 'Time Series';
    TsPackage.FileName := FOptions.TS6_FileNames[PackageIndex];
    TsPackage.PackageName := '';

    TsReader := TTimeSeries.Create(FPackageType);
    TsPackage.Package := TsReader;
    TsPackage.ReadPackage(Unhandled);
  end;
end;

end.
