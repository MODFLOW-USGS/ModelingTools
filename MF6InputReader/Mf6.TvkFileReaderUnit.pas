unit Mf6.TvkFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TTvkOptions = class(TCustomMf6Persistent)
  private
    PRINT_INPUT: Boolean;
    TS6_FileNames: TStringList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TTvkPeriodData = class(TCustomMf6Persistent)
  private
    IPer: Integer;
    FCells: TTimeVariableCellList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; Dimensions: TDimensions);
    function GetCell(Index: Integer): TTimeVariableCell;
    function GetCount: Integer;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Period: Integer read IPer;
    property Count: Integer read GetCount;
    property Cell[Index: Integer]: TTimeVariableCell read GetCell; default;
  end;

  TTvkPeriodList = TObjectList<TTvkPeriodData>;

  TTvk = class(TDimensionedPackageReader)
  private
    FOptions: TTvkOptions;
    FPeriods: TTvkPeriodList;
    FTimeSeriesPackages: TPackageList;
    function GetCount: Integer;
    function GetPeriod(Index: Integer): TTvkPeriodData;
    function GetTimeSeriesPackage(Index: Integer): TPackage;
    function GetTimeSeriesPackageCount: Integer;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer); override;
    property Count: Integer read GetCount;
    property Periods[Index: Integer]: TTvkPeriodData read GetPeriod; default;
    property TimeSeriesPackageCount: Integer read GetTimeSeriesPackageCount;
    property TimeSeriesPackages[Index: Integer]: TPackage read GetTimeSeriesPackage;
  end;

implementation

uses
  Mf6.TimeSeriesFileReaderUnit, ModelMuseUtilities;

{ TTvkOptions }

constructor TTvkOptions.Create(PackageType: string);
begin
  TS6_FileNames := TStringList.Create;
  inherited;
end;

destructor TTvkOptions.Destroy;
begin
  TS6_FileNames.Free;
  inherited;
end;

procedure TTvkOptions.Initialize;
begin
  inherited;
  PRINT_INPUT := False;
  TS6_FileNames.Clear;
end;

procedure TTvkOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
      Unhandled.WriteLine('Unrecognized TVK option in the following line.');
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TTvkPeriodData }

constructor TTvkPeriodData.Create(PackageType: string);
begin
  FCells := TTimeVariableCellList.Create;
  inherited;

end;

destructor TTvkPeriodData.Destroy;
begin
  FCells.Free;
  inherited;
end;

function TTvkPeriodData.GetCell(Index: Integer): TTimeVariableCell;
begin
  result := FCells[Index];
end;

function TTvkPeriodData.GetCount: Integer;
begin
  result := FCells.Count;
end;

procedure TTvkPeriodData.Initialize;
begin
  inherited;
  FCells.Clear;
end;

procedure TTvkPeriodData.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
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

    Cell.Initialize;
    CaseSensitiveLine := ALine;
    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'PERIOD') then
    begin
      // do nothing
    end
    else if FSplitter.Count >= DimensionCount + 2 then
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
        Unhandled.WriteLine('Unrecognized TVK PERIOD data in the following line.');
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine('Unrecognized TVK PERIOD data in the following line.');
      Unhandled.WriteLine(ErrorLine);
    end;
  end;

end;

{ TTvk }

constructor TTvk.Create(PackageType: string);
begin
  inherited;
  FOptions := TTvkOptions.Create(PackageType);
  FPeriods := TTvkPeriodList.Create;
  FTimeSeriesPackages := TPackageList.Create;
end;

destructor TTvk.Destroy;
begin
  FTimeSeriesPackages.Free;
  FOptions.Free;
  FPeriods.Free;
  inherited;
end;

function TTvk.GetCount: Integer;
begin
  result := FPeriods.Count;
end;

function TTvk.GetPeriod(Index: Integer): TTvkPeriodData;
begin
  result := FPeriods[Index];
end;

function TTvk.GetTimeSeriesPackage(Index: Integer): TPackage;
begin
  Result := FTimeSeriesPackages[Index];
end;

function TTvk.GetTimeSeriesPackageCount: Integer;
begin
  result := FTimeSeriesPackages.Count;
end;

procedure TTvk.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
var
  ALine: string;
  ErrorLine: string;
  IPER: Integer;
  APeriod: TTvkPeriodData;
  TsPackage: TPackage;
  PackageIndex: Integer;
  TsReader: TTimeSeries;
begin
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading TVK package');
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
      else if (FSplitter[1] ='PERIOD') and (FSplitter.Count >= 3) then
      begin
        if TryStrToInt(FSplitter[2], IPER) then
        begin
          if IPER > NPER then
          begin
            break;
          end;
          APeriod := TTvkPeriodData.Create(FPackageType);
          FPeriods.Add(APeriod);
          APeriod.IPer := IPER;
          APeriod.Read(Stream, Unhandled, FDimensions);
        end
        else
        begin
          Unhandled.WriteLine('Unrecognized TVK data in the following line.');
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine('Unrecognized TVK data in the following line.');
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
    TsPackage.ReadPackage(Unhandled, NPER);
  end;
end;

end.
