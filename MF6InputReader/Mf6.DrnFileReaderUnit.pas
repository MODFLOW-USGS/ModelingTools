unit Mf6.DrnFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections, System.Generics.Defaults;

type
  TDrnOptions = class(TCustomMf6Persistent)
  private
    FAUXILIARY: TStringList;
    FAUXMULTNAME: string;
    FAUXDEPTHNAME: string;
    BOUNDNAMES: Boolean;
    PRINT_INPUT: Boolean;
    PRINT_FLOWS: Boolean;
    SAVE_FLOWS: Boolean;
    TS6_FileNames: TStringList;
    Obs6_FileNames: TStringList;
    MOVER: Boolean;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    function GetAUXILIARY(Index: Integer): string;
    function GetCount: Integer;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property AUXILIARY[Index: Integer]: string read GetAUXILIARY;
    property AUXMULTNAME: string read FAUXMULTNAME;
    property AUXDEPTHNAME: string read FAUXDEPTHNAME;
    function IndexOfAUXILIARY(const AName: string): Integer;
  end;

  TDrnDimensions = class(TCustomMf6Persistent)
  private
    MAXBOUND: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TDrnTimeItem = class(TObject)
  private
    Fcellid: TCellId;
    Felev: TMf6BoundaryValue;
    Fcond: TMf6BoundaryValue;
    Faux: TList<TMf6BoundaryValue>;
    Fboundname: string;
    FId: Integer;
    function GetAux(Index: Integer): TMf6BoundaryValue;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Keystring: string;
    property cellid: TCellId read Fcellid;
    property elev: TMf6BoundaryValue read Felev;
    property cond: TMf6BoundaryValue read Fcond;
    property BoundName: string read Fboundname;
    property Count: integer read GetCount;
    property Aux[Index: Integer]: TMf6BoundaryValue read GetAux; default;
    // Id is used in the MVR package;
    property Id: Integer read FId;
  end;

  TDrnTimeItemList = class(TObjectList<TDrnTimeItem>)
    procedure Sort;
    function SameCells(OtherList: TDrnTimeItemList): Boolean;
  end;

  TDrnPeriod = class(TCustomMf6Persistent)
  private
    IPer: Integer;
    FCells: TDrnTimeItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      Dimensions: TDimensions; naux: Integer; BOUNDNAMES: Boolean);
    function GetCell(Index: Integer): TDrnTimeItem;
    function GetCount: Integer;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Period: Integer read IPer;
    property Count: Integer read GetCount;
    property Cells[Index: Integer]: TDrnTimeItem read GetCell; default;
  end;

  TDrnPeriodList = TObjectList<TDrnPeriod>;

  TDrn = class(TDimensionedPackageReader)
  private
    FOptions: TDrnOptions;
    FDrnDimensions: TDrnDimensions;
    FPeriods: TDrnPeriodList;
    FTimeSeriesPackages: TPackageList;
    FObservationsPackages: TPackageList;
    function GetObservation(Index: Integer): TPackage;
    function GetObservationCount: Integer;
    function GetPeriod(Index: Integer): TDrnPeriod;
    function GetPeriodCount: Integer;
    function GetTimeSeries(Index: Integer): TPackage;
    function GetTimeSeriesCount: Integer;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer); override;
    property Options: TDrnOptions read FOptions;
    property PeriodCount: Integer read GetPeriodCount;
    property Periods[Index: Integer]: TDrnPeriod read GetPeriod;
    property TimeSeriesCount: Integer read GetTimeSeriesCount;
    property TimeSeries[Index: Integer]: TPackage read GetTimeSeries;
    property ObservationCount: Integer read GetObservationCount;
    property Observations[Index: Integer]: TPackage read GetObservation;
  end;


implementation

uses
  ModelMuseUtilities, Mf6.TimeSeriesFileReaderUnit, Mf6.ObsFileReaderUnit;

{ TDrnOptions }

constructor TDrnOptions.Create(PackageType: string);
begin
  FAUXILIARY := TStringList.Create;
  FAUXILIARY.CaseSensitive := False;
  TS6_FileNames := TStringList.Create;
  Obs6_FileNames := TStringList.Create;
  inherited;

end;

destructor TDrnOptions.Destroy;
begin
  FAUXILIARY.Free;
  TS6_FileNames.Free;
  Obs6_FileNames.Free;
  inherited;
end;

function TDrnOptions.GetAUXILIARY(Index: Integer): string;
begin
  result := FAUXILIARY[Index]
end;

function TDrnOptions.GetCount: Integer;
begin
  result := FAUXILIARY.Count
end;

function TDrnOptions.IndexOfAUXILIARY(const AName: string): Integer;
begin
  result := FAUXILIARY.IndexOf(AName)
end;

procedure TDrnOptions.Initialize;
begin
  inherited;
  FAUXILIARY.Clear;
  TS6_FileNames.Clear;
  Obs6_FileNames.Clear;
  BOUNDNAMES := False;
  PRINT_INPUT := False;
  PRINT_FLOWS := False;
  SAVE_FLOWS := False;
  MOVER := False;
end;

procedure TDrnOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    else if FSplitter[0] = 'MOVER' then
    begin
      MOVER := True;
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
      if FAUXMULTNAME = '' then
      begin
        FAUXMULTNAME := FSplitter[1];
      end;
    end
    else if (FSplitter[0] = 'AUXDEPTHNAME')
      and (FSplitter.Count >= 2) then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      if FAUXDEPTHNAME = '' then
      begin
        FAUXDEPTHNAME := FSplitter[1];
      end;
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

{ TChdDTimeItem }

constructor TDrnTimeItem.Create;
begin
  Fcellid.Initialize;
  Felev.Initialize;
  Fcond.Initialize;
  Faux := TList<TMf6BoundaryValue>.Create;
  Fboundname := '';
end;

destructor TDrnTimeItem.Destroy;
begin
  Faux.Free;
  inherited;
end;

function TDrnTimeItem.GetAux(Index: Integer): TMf6BoundaryValue;
begin
  result := Faux[Index];
end;

function TDrnTimeItem.GetCount: integer;
begin
  result := Faux.Count;
end;

function TDrnTimeItem.Keystring: string;
var
  AuxIndex: Integer;
  AnAux: TMf6BoundaryValue;
begin
  result := '';
  if elev.ValueType = vtNumeric then
  begin
    result := result + ' Num';
  end
  else
  begin
    result := result + UpperCase(elev.StringValue);
  end;
  if cond.ValueType = vtNumeric then
  begin
    result := result + ' Num';
  end
  else
  begin
    result := result + UpperCase(cond.StringValue);
  end;
  for AuxIndex := 0 to Faux.Count - 1 do
  begin
    AnAux := Faux[AuxIndex];
    if AnAux.ValueType = vtNumeric then
    begin
      result := result + ' Num';
    end
    else
    begin
      result := result + UpperCase(AnAux.StringValue);
    end;
  end;
end;

{ TDrnDimensions }

procedure TDrnDimensions.Initialize;
begin
  inherited;
  MAXBOUND := 0;
end;

procedure TDrnDimensions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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

{ TDrnPeriod }

constructor TDrnPeriod.Create(PackageType: string);
begin
  FCells := TDrnTimeItemList.Create;
  inherited;
end;

destructor TDrnPeriod.Destroy;
begin
  FCells.Free;
  inherited;
end;

function TDrnPeriod.GetCell(Index: Integer): TDrnTimeItem;
begin
  result := FCells[Index];
end;

function TDrnPeriod.GetCount: Integer;
begin
  result := FCells.Count;
end;

procedure TDrnPeriod.Initialize;
begin
  inherited;
  FCells.Clear;
end;

procedure TDrnPeriod.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  Dimensions: TDimensions; naux: Integer; BOUNDNAMES: Boolean);
var
  DimensionCount: Integer;
  Cell: TDrnTimeItem;
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
  Aux: TMf6BoundaryValue;
  StartIndex: Integer;
  AuxIndex: Integer;
  NumberOfColumns: Integer;
begin
  DimensionCount := Dimensions.DimensionCount;
  NumberOfColumns := DimensionCount + 2 + naux;
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

    Cell := TDrnTimeItem.Create;
    try
      CaseSensitiveLine := ALine;
      if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'PERIOD') then
      begin
        // do nothing
      end
      else if FSplitter.Count >= NumberOfColumns then
      begin
        if ReadCellID(Cell.Fcellid, 0, DimensionCount) then
        begin
          if TryFortranStrToFloat(FSplitter[DimensionCount], Cell.Felev.NumericValue) then
          begin
            Cell.Felev.ValueType := vtNumeric;
          end
          else
          begin
            Cell.Felev.ValueType := vtString;
            FSplitter.DelimitedText := CaseSensitiveLine;
            Cell.Felev.StringValue := FSplitter[DimensionCount];
          end;
          if TryFortranStrToFloat(FSplitter[DimensionCount+1], Cell.Fcond.NumericValue) then
          begin
            Cell.Fcond.ValueType := vtNumeric;
          end
          else
          begin
            Cell.Fcond.ValueType := vtString;
            FSplitter.DelimitedText := CaseSensitiveLine;
            Cell.Fcond.StringValue := FSplitter[DimensionCount+1];
          end;
          StartIndex := DimensionCount + 2;
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
          if BOUNDNAMES and (FSplitter.Count >= NumberOfColumns+1) then
          begin
            Cell.Fboundname := FSplitter[StartIndex];
          end;
          FCells.Add(Cell);
          Cell.FId := FCells.Count;
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

{ TDrn }

constructor TDrn.Create(PackageType: string);
begin
  inherited;
  FOptions := TDrnOptions.Create(PackageType);
  FDrnDimensions := TDrnDimensions.Create(PackageType);
  FPeriods := TDrnPeriodList.Create;
  FTimeSeriesPackages := TPackageList.Create;
  FObservationsPackages := TPackageList.Create;

end;

destructor TDrn.Destroy;
begin
  FOptions.Free;
  FDrnDimensions.Free;
  FPeriods.Free;
  FTimeSeriesPackages.Free;
  FObservationsPackages.Free;
  inherited;
end;

function TDrn.GetObservation(Index: Integer): TPackage;
begin
  result := FObservationsPackages[Index]
end;

function TDrn.GetObservationCount: Integer;
begin
  result := FObservationsPackages.Count;
end;

function TDrn.GetPeriod(Index: Integer): TDrnPeriod;
begin
  result := FPeriods[Index];
end;

function TDrn.GetPeriodCount: Integer;
begin
  result := FPeriods.Count;
end;

function TDrn.GetTimeSeries(Index: Integer): TPackage;
begin
  result := FTimeSeriesPackages[Index];
end;

function TDrn.GetTimeSeriesCount: Integer;
begin
  result := FTimeSeriesPackages.Count;
end;

procedure TDrn.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
var
  ALine: string;
  ErrorLine: string;
  IPER: Integer;
  APeriod: TDrnPeriod;
  TsPackage: TPackage;
  PackageIndex: Integer;
  TsReader: TTimeSeries;
  ObsReader: TObs;
  ObsPackage: TPackage;
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
      else if FSplitter[1] ='DIMENSIONS' then
      begin
        FDrnDimensions.Read(Stream, Unhandled);
      end
      else if (FSplitter[1] ='PERIOD') and (FSplitter.Count >= 3) then
      begin
        if TryStrToInt(FSplitter[2], IPER) then
        begin
          if IPER > NPER then
          begin
            break;
          end;
          APeriod := TDrnPeriod.Create(FPackageType);
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

{ TDrnTimeItemList }

function TDrnTimeItemList.SameCells(OtherList: TDrnTimeItemList): Boolean;
var
  index: Integer;
begin
  Result := Count = OtherList.Count;
  if Result then
  begin
    for index := 0 to Count - 1 do
    begin
      Result := Items[index].Fcellid.SameLocation(OtherList.Items[index].Fcellid);
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TDrnTimeItemList.Sort;
begin
  inherited Sort(
    TComparer<TDrnTimeItem>.Construct(
      function(const Left, Right: TDrnTimeItem): Integer
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
