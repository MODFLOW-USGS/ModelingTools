unit Mf6.CncFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections, System.Generics.Defaults;

type
  TCncOptions = class(TCustomMf6Persistent)
  private
    FAUXILIARY: TStringList;
    FAUXMULTNAME: string;
    BOUNDNAMES: Boolean;
    PRINT_INPUT: Boolean;
    PRINT_FLOWS: Boolean;
    SAVE_FLOWS: Boolean;
    TS6_FileNames: TStringList;
    Obs6_FileNames: TStringList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    function GetAUXILIARY(Index: Integer): string;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property AUXILIARY[Index: Integer]: string read GetAUXILIARY;
    property AUXMULTNAME: string read FAUXMULTNAME;
    function IndexOfAUXILIARY(const AName: string): Integer;
  end;

  TCncDimensions = class(TCustomMf6Persistent)
  private
    MAXBOUND: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TCncTimeItem = class(TObject)
  private
    Fcellid: TMfCellId;
    Fconc: TMf6BoundaryValue;
    Faux: TList<TMf6BoundaryValue>;
    Fboundname: string;
    function GetAux(Index: Integer): TMf6BoundaryValue;
  public
    constructor Create;
    destructor Destroy; override;
    property cellid: TMfCellId read Fcellid;
    property conc: TMf6BoundaryValue read Fconc;
    property boundname: string read Fboundname;
    property Aux[Index: Integer]: TMf6BoundaryValue read GetAux; default;
  end;

  TCncTimeItemList = class(TObjectList<TCncTimeItem>)
    procedure Sort;
    function SameCells(OtherList: TCncTimeItemList): Boolean;
  end;

  TCncPeriod = class(TCustomMf6Persistent)
  private
    IPer: Integer;
    FCells: TCncTimeItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      Dimensions: TDimensions; naux: Integer; BOUNDNAMES: Boolean);
    function GetCell(Index: Integer): TCncTimeItem;
    function GetCount: Integer;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Period: Integer read IPer;
    property Count: Integer read GetCount;
    property Cells[Index: Integer]: TCncTimeItem read GetCell; default;
  end;

  TCncPeriodList = TObjectList<TCncPeriod>;

  TCnc = class(TDimensionedPackageReader)
  private
    FOptions: TCncOptions;
    FCncDimensions: TCncDimensions;
    FPeriods: TCncPeriodList;
    FTimeSeriesPackages: TPackageList;
    FObservationsPackages: TPackageList;
    function GetObservation(Index: Integer): TPackage;
    function GetObservationCount: Integer;
    function GetPeriod(Index: Integer): TCncPeriod;
    function GetPeriodCount: Integer;
    function GetTimeSeries(Index: Integer): TPackage;
    function GetTimeSeriesCount: Integer;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      const NPER: Integer); override;
    property Options: TCncOptions read FOptions;
    property PeriodCount: Integer read GetPeriodCount;
    property Periods[Index: Integer]: TCncPeriod read GetPeriod;
    property TimeSeriesCount: Integer read GetTimeSeriesCount;
    property TimeSeries[Index: Integer]: TPackage read GetTimeSeries;
    property ObservationCount: Integer read GetObservationCount;
    property Observations[Index: Integer]: TPackage read GetObservation;
  end;

implementation

uses
  ModelMuseUtilities, Mf6.TimeSeriesFileReaderUnit, Mf6.ObsFileReaderUnit;

{ TCncOptions }

constructor TCncOptions.Create(PackageType: string);
begin
  FAUXILIARY := TStringList.Create;
  TS6_FileNames := TStringList.Create;
  Obs6_FileNames := TStringList.Create;
  inherited;

end;

destructor TCncOptions.Destroy;
begin
  FAUXILIARY.Free;
  TS6_FileNames.Free;
  Obs6_FileNames.Free;
  inherited;
end;

function TCncOptions.GetAUXILIARY(Index: Integer): string;
begin
  result := FAUXILIARY[Index];
end;

function TCncOptions.IndexOfAUXILIARY(const AName: string): Integer;
begin
  result := FAUXILIARY.IndexOf(AName)
end;

procedure TCncOptions.Initialize;
begin
  inherited;
  FAUXILIARY.Clear;
  TS6_FileNames.Clear;
  Obs6_FileNames.Clear;
  BOUNDNAMES := False;
  PRINT_INPUT := False;
  PRINT_FLOWS := False;
  SAVE_FLOWS := False;
end;

procedure TCncOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
  TS6_FileName: string;
  Obs_FileName: string;
  AUXILIARY_Name: string;
  AuxIndex: Integer;
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
    else if FSplitter[0] = 'BOUNDNAMES' then
    begin
      BOUNDNAMES := True;
    end
    else if FSplitter[0] = 'PRINT_INPUT' then
    begin
      PRINT_INPUT := True;
    end
    else if FSplitter[0] = 'PRINT_FLOWS' then
    begin
      PRINT_FLOWS := True;
    end
    else if FSplitter[0] = 'SAVE_FLOWS' then
    begin
      SAVE_FLOWS := True;
    end
    else if (FSplitter[0] = 'AUXILIARY')
      and (FSplitter.Count >= 2) then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      for AuxIndex := 1 to FSplitter.Count - 1 do
      begin
        AUXILIARY_Name := FSplitter[AuxIndex];
        FAUXILIARY.Add(AUXILIARY_Name);
      end;
    end
    else if (FSplitter[0] = 'AUXMULTNAME')
      and (FSplitter.Count >= 2) then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      FAUXMULTNAME := FSplitter[1];
    end
    else if (FSplitter[0] = 'TS6')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEIN') then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      TS6_FileName := FSplitter[2];
      TS6_FileNames.Add(TS6_FileName);
    end
    else if (FSplitter[0] = 'OBS6')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEIN') then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      Obs_FileName := FSplitter[2];
      Obs6_FileNames.Add(Obs_FileName);
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TCncTimeItem }

constructor TCncTimeItem.Create;
begin
  Fcellid.Initialize;
  Fconc.Initialize;
  Faux := TList<TMf6BoundaryValue>.Create;
  Fboundname := '';
end;

destructor TCncTimeItem.Destroy;
begin
  Faux.Free;
  inherited;
end;

function TCncTimeItem.GetAux(Index: Integer): TMf6BoundaryValue;
begin
  result := Faux[Index];
end;

{ TCncDimensions }

procedure TCncDimensions.Initialize;
begin
  inherited;
  MAXBOUND := 0;
end;

procedure TCncDimensions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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

{ TCncPeriod }

constructor TCncPeriod.Create(PackageType: string);
begin
  FCells := TCncTimeItemList.Create;
  inherited;
end;

destructor TCncPeriod.Destroy;
begin
  FCells.Free;
  inherited;
end;

function TCncPeriod.GetCell(Index: Integer): TCncTimeItem;
begin
  result := FCells[Index];
end;

function TCncPeriod.GetCount: Integer;
begin
  result := FCells.Count;
end;

procedure TCncPeriod.Initialize;
begin
  inherited;
  FCells.Clear;
end;

procedure TCncPeriod.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  Dimensions: TDimensions; naux: Integer; BOUNDNAMES: Boolean);
var
  DimensionCount: Integer;
  Cell: TCncTimeItem;
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
  Aux: TMf6BoundaryValue;
  StartIndex: Integer;
  AuxIndex: Integer;
  NumberOfItems: Integer;
begin
  DimensionCount := Dimensions.DimensionCount;
  NumberOfItems := DimensionCount + 1 + naux;
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

    Cell := TCncTimeItem.Create;
    try
      CaseSensitiveLine := ALine;
      if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'PERIOD') then
      begin
        // do nothing
      end
      else if FSplitter.Count >= NumberOfItems then
      begin
        if ReadCellID(Cell.Fcellid, 0, DimensionCount) then
        begin
          if TryFortranStrToFloat(FSplitter[DimensionCount], Cell.Fconc.NumericValue) then
          begin
            Cell.Fconc.ValueType := vtNumeric;
          end
          else
          begin
            Cell.Fconc.ValueType := vtString;
            FSplitter.DelimitedText := CaseSensitiveLine;
            Cell.Fconc.StringValue := FSplitter[DimensionCount];
          end;
          StartIndex := DimensionCount + 1;
          for AuxIndex := 0 to naux - 1 do
          begin
            Aux.Initialize;
            if TryFortranStrToFloat(FSplitter[StartIndex], Aux.NumericValue) then
            begin
              Aux.ValueType := vtNumeric;
            end
            else
            begin
              Aux.ValueType := vtString;
              FSplitter.DelimitedText := CaseSensitiveLine;
              Aux.StringValue := FSplitter[StartIndex];
            end;
            Inc(StartIndex);
            Cell.Faux.Add(Aux);
          end;
          if BOUNDNAMES and (FSplitter.Count >= NumberOfItems+1) then
          begin
            Cell.Fboundname := FSplitter[StartIndex];
          end;
          FCells.Add(Cell);
          Cell:= nil;
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
    finally
      Cell.Free;
    end;
  end;

end;

{ TCnc }

constructor TCnc.Create(PackageType: string);
begin
  inherited;
  FOptions := TCncOptions.Create(PackageType);
  FCncDimensions := TCncDimensions.Create(PackageType);
  FPeriods := TCncPeriodList.Create;
  FTimeSeriesPackages := TPackageList.Create;
  FObservationsPackages := TPackageList.Create;

end;

destructor TCnc.Destroy;
begin
  FOptions.Free;
  FCncDimensions.Free;
  FPeriods.Free;
  FTimeSeriesPackages.Free;
  FObservationsPackages.Free;
  inherited;
end;

function TCnc.GetObservation(Index: Integer): TPackage;
begin
  result := FObservationsPackages[Index];
end;

function TCnc.GetObservationCount: Integer;
begin
  result := FObservationsPackages.Count;
end;

function TCnc.GetPeriod(Index: Integer): TCncPeriod;
begin
  result := FPeriods[Index];
end;

function TCnc.GetPeriodCount: Integer;
begin
  result := FPeriods.Count;
end;

function TCnc.GetTimeSeries(Index: Integer): TPackage;
begin
  result := FTimeSeriesPackages[Index];
end;

function TCnc.GetTimeSeriesCount: Integer;
begin
  result := FTimeSeriesPackages.Count;
end;

procedure TCnc.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
var
  ALine: string;
  ErrorLine: string;
  IPER: Integer;
  APeriod: TCncPeriod;
  TsPackage: TPackage;
  PackageIndex: Integer;
  TsReader: TTimeSeries;
  ObsReader: TObs;
  ObsPackage: TPackage;
begin
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading CNC package');
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
        FCncDimensions.Read(Stream, Unhandled);
      end
      else if (FSplitter[1] ='PERIOD') and (FSplitter.Count >= 3) then
      begin
        if TryStrToInt(FSplitter[2], IPER) then
        begin
          if IPER > NPER then
          begin
            break;
          end;
          APeriod := TCncPeriod.Create(FPackageType);
          FPeriods.Add(APeriod);
          APeriod.IPer := IPER;
          APeriod.Read(Stream, Unhandled, FDimensions, FOptions.FAUXILIARY.Count,
            FOptions.BOUNDNAMES);
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
  for PackageIndex := 0 to FOptions.TS6_FileNames.Count - 1 do
  begin
    TsPackage := TPackage.Create;
    FTimeSeriesPackages.Add(TsPackage);
    TsPackage.FileType := FPackageType;
    TsPackage.FileName := FOptions.TS6_FileNames[PackageIndex];
    TsPackage.PackageName := '';

    TsReader := TTimeSeries.Create(FPackageType);
    TsPackage.Package := TsReader;
    TsPackage.ReadPackage(Unhandled, NPER);
  end;
  for PackageIndex := 0 to FOptions.Obs6_FileNames.Count - 1 do
  begin
    ObsPackage := TPackage.Create;
    FObservationsPackages.Add(ObsPackage);
    ObsPackage.FileType := FPackageType;
    ObsPackage.FileName := FOptions.Obs6_FileNames[PackageIndex];
    ObsPackage.PackageName := '';

    ObsReader := TObs.Create(FPackageType);
    ObsReader.Dimensions := FDimensions;
    ObsPackage.Package := ObsReader;
    ObsPackage.ReadPackage(Unhandled, NPER);
  end;
end;

{ TCncTimeItemList }

function TCncTimeItemList.SameCells(OtherList: TCncTimeItemList): Boolean;
begin
  Result := Count = OtherList.Count;
  if Result then
  begin
    for var index := 0 to Count - 1 do
    begin
      Result := Items[index].Fcellid.SameLocation(OtherList.Items[index].Fcellid);
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TCncTimeItemList.Sort;
begin
  inherited Sort(
    TComparer<TCncTimeItem>.Construct(
      function(const Left, Right: TCncTimeItem): Integer
      begin
        Result := AnsiCompareText(Left.Fboundname, Right.Fboundname);
        if Result = 0 then
        begin
          result := Left.Fcellid.Layer - Right.Fcellid.Layer;
          if Result = 0 then
          begin
            result := Left.Fcellid.Row - Right.Fcellid.Row;
            if Result = 0 then
            begin
              result := Left.Fcellid.column - Right.Fcellid.column;
            end;
          end;
        end;
      end
    ));
end;

end.
