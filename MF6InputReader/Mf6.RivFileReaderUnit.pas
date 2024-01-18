unit Mf6.RivFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections, System.Generics.Defaults;

type
  TRivOptions = class(TCustomMf6Persistent)
  private
    FAUXILIARY: TStringList;
    FAUXMULTNAME: string;
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
    function IndexOfAUXILIARY(const AName: string): Integer;
  end;

  TRivDimensions = class(TCustomMf6Persistent)
  private
    MAXBOUND: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TRivTimeItem = class(TObject)
  private
    Fcellid: TCellId;
    Fstage: TMf6BoundaryValue;
    Fcond: TMf6BoundaryValue;
    Frbot: TMf6BoundaryValue;
    Faux: TList<TMf6BoundaryValue>;
    Fboundname: string;
    FId: Integer;
    function GetAux(Index: Integer): TMf6BoundaryValue;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Keystring: string;
    property CellId: TCellId read Fcellid;
    property Stage: TMf6BoundaryValue read Fstage;
    property Cond: TMf6BoundaryValue read Fcond;
    property RBot: TMf6BoundaryValue read Frbot;
    property Count: integer read GetCount;
    property Aux[Index: Integer]: TMf6BoundaryValue read GetAux; default;
    property BoundName: string read Fboundname;
    // Id is used in the MVR package;
    property Id: Integer read FId;
  end;

  TRivTimeItemList = class(TObjectList<TRivTimeItem>)
    procedure Sort;
    function SameCells(OtherList: TRivTimeItemList): Boolean;
  end;

  TRivPeriod = class(TCustomMf6Persistent)
  private
    IPer: Integer;
    FCells: TRivTimeItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      Dimensions: TDimensions; naux: Integer; BOUNDNAMES: Boolean);
    function GetCell(Index: Integer): TRivTimeItem;
    function GetCount: Integer;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Period: Integer read IPer;
    property Count: Integer read GetCount;
    property Cells[Index: Integer]: TRivTimeItem read GetCell; default;
  end;

  TRivPeriodList = TObjectList<TRivPeriod>;

  TRiv = class(TDimensionedPackageReader)
  private
    FOptions: TRivOptions;
    FChdDimensions: TRivDimensions;
    FPeriods: TRivPeriodList;
    FTimeSeriesPackages: TPackageList;
    FObservationsPackages: TPackageList;
    function GetObservation(Index: Integer): TPackage;
    function GetObservationCount: Integer;
    function GetPeriod(Index: Integer): TRivPeriod;
    function GetPeriodCount: Integer;
    function GetTimeSeries(Index: Integer): TPackage;
    function GetTimeSeriesCount: Integer;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer); override;
    property Options: TRivOptions read FOptions;
    property PeriodCount: Integer read GetPeriodCount;
    property Periods[Index: Integer]: TRivPeriod read GetPeriod;
    property TimeSeriesCount: Integer read GetTimeSeriesCount;
    property TimeSeries[Index: Integer]: TPackage read GetTimeSeries;
    property ObservationCount: Integer read GetObservationCount;
    property Observations[Index: Integer]: TPackage read GetObservation;
  end;


implementation

uses
  ModelMuseUtilities, Mf6.TimeSeriesFileReaderUnit, Mf6.ObsFileReaderUnit;

{ TRivOptions }

constructor TRivOptions.Create(PackageType: string);
begin
  FAUXILIARY := TStringList.Create;
  FAUXILIARY.CaseSensitive := False;
  TS6_FileNames := TStringList.Create;
  Obs6_FileNames := TStringList.Create;
  inherited;

end;

destructor TRivOptions.Destroy;
begin
  FAUXILIARY.Free;
  TS6_FileNames.Free;
  Obs6_FileNames.Free;
  inherited;
end;

function TRivOptions.GetAUXILIARY(Index: Integer): string;
begin
  Result := FAUXILIARY[Index];
end;

function TRivOptions.GetCount: Integer;
begin
  Result := FAUXILIARY.Count;
end;

function TRivOptions.IndexOfAUXILIARY(const AName: string): Integer;
begin
  Result := FAUXILIARY.IndexOf(AName)
end;

procedure TRivOptions.Initialize;
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

procedure TRivOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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

{ TChdDTimeItem }

constructor TRivTimeItem.Create;
begin
  Fcellid.Initialize;
  Fstage.Initialize;
  Fcond.Initialize;
  Frbot.Initialize;
  Faux := TList<TMf6BoundaryValue>.Create;
  Fboundname := '';
end;

destructor TRivTimeItem.Destroy;
begin
  Faux.Free;
  inherited;
end;

function TRivTimeItem.GetAux(Index: Integer): TMf6BoundaryValue;
begin
  result := Faux[Index];
end;

function TRivTimeItem.GetCount: integer;
begin
  result := Faux.Count;
end;

function TRivTimeItem.Keystring: string;
var
  AuxIndex: Integer;
  AnAux: TMf6BoundaryValue;
begin
  result := '';
  if Fstage.ValueType = vtNumeric then
  begin
    result := result + ' Num';
  end
  else
  begin
    result := result + UpperCase(Fstage.StringValue);
  end;

  if Fcond.ValueType = vtNumeric then
  begin
    result := result + ' Num';
  end
  else
  begin
    result := result + UpperCase(Fcond.StringValue);
  end;

  if Frbot.ValueType = vtNumeric then
  begin
    result := result + ' Num';
  end
  else
  begin
    result := result + UpperCase(Frbot.StringValue);
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

{ TRivDimensions }

procedure TRivDimensions.Initialize;
begin
  inherited;
  MAXBOUND := 0;
end;

procedure TRivDimensions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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

{ TRivPeriod }

constructor TRivPeriod.Create(PackageType: string);
begin
  FCells := TRivTimeItemList.Create;
  inherited;
end;

destructor TRivPeriod.Destroy;
begin
  FCells.Free;
  inherited;
end;

function TRivPeriod.GetCell(Index: Integer): TRivTimeItem;
begin
  result := FCells[Index];
end;

function TRivPeriod.GetCount: Integer;
begin
  result := FCells.Count;
end;

procedure TRivPeriod.Initialize;
begin
  inherited;
  FCells.Clear;
end;

procedure TRivPeriod.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  Dimensions: TDimensions; naux: Integer; BOUNDNAMES: Boolean);
var
  DimensionCount: Integer;
  Cell: TRivTimeItem;
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
  Aux: TMf6BoundaryValue;
  StartIndex: Integer;
  AuxIndex: Integer;
  NumberOfColumns: Integer;
begin
  DimensionCount := Dimensions.DimensionCount;
  NumberOfColumns := DimensionCount + 3 + naux;
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

    Cell := TRivTimeItem.Create;;
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
          if TryFortranStrToFloat(FSplitter[DimensionCount], Cell.Fstage.NumericValue) then
          begin
            Cell.Fstage.ValueType := vtNumeric;
          end
          else
          begin
            Cell.Fstage.ValueType := vtString;
            FSplitter.DelimitedText := CaseSensitiveLine;
            Cell.Fstage.StringValue := FSplitter[DimensionCount];
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

          if TryFortranStrToFloat(FSplitter[DimensionCount+2], Cell.Frbot.NumericValue) then
          begin
            Cell.Frbot.ValueType := vtNumeric;
          end
          else
          begin
            Cell.Frbot.ValueType := vtString;
            FSplitter.DelimitedText := CaseSensitiveLine;
            Cell.Frbot.StringValue := FSplitter[DimensionCount+2];
          end;

          StartIndex := DimensionCount + 3;
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

{ TRiv }

constructor TRiv.Create(PackageType: string);
begin
  inherited;
  FOptions := TRivOptions.Create(PackageType);
  FChdDimensions := TRivDimensions.Create(PackageType);
  FPeriods := TRivPeriodList.Create;
  FTimeSeriesPackages := TPackageList.Create;
  FObservationsPackages := TPackageList.Create;

end;

destructor TRiv.Destroy;
begin
  FOptions.Free;
  FChdDimensions.Free;
  FPeriods.Free;
  FTimeSeriesPackages.Free;
  FObservationsPackages.Free;
  inherited;
end;

function TRiv.GetObservation(Index: Integer): TPackage;
begin
  result := FObservationsPackages[Index];
end;

function TRiv.GetObservationCount: Integer;
begin
  result := FObservationsPackages.Count;
end;

function TRiv.GetPeriod(Index: Integer): TRivPeriod;
begin
  result := FPeriods[Index];
end;

function TRiv.GetPeriodCount: Integer;
begin
  result := FPeriods.Count;
end;

function TRiv.GetTimeSeries(Index: Integer): TPackage;
begin
  Result := FTimeSeriesPackages[Index];
end;

function TRiv.GetTimeSeriesCount: Integer;
begin
  Result := FTimeSeriesPackages.Count;
end;

procedure TRiv.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
var
  ALine: string;
  ErrorLine: string;
  IPER: Integer;
  APeriod: TRivPeriod;
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
        FChdDimensions.Read(Stream, Unhandled);
      end
      else if (FSplitter[1] ='PERIOD') and (FSplitter.Count >= 3) then
      begin
        if TryStrToInt(FSplitter[2], IPER) then
        begin
          if IPER > NPER then
          begin
            break;
          end;
          APeriod := TRivPeriod.Create(FPackageType);
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

{ TRivTimeItemList }

function TRivTimeItemList.SameCells(OtherList: TRivTimeItemList): Boolean;
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

procedure TRivTimeItemList.Sort;
begin
  inherited Sort(
    TComparer<TRivTimeItem>.Construct(
      function(const Left, Right: TRivTimeItem): Integer
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
