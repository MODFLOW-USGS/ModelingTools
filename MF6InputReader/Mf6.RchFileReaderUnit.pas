unit Mf6.RchFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections, System.Generics.Defaults;

type
  TRchOptions = class(TCustomMf6Persistent)
  private
    FFIXED_CELL: Boolean;
    FAUXILIARY: TStringList;
    FAUXMULTNAME: string;
    BOUNDNAMES: Boolean;
    PRINT_INPUT: Boolean;
    PRINT_FLOWS: Boolean;
    SAVE_FLOWS: Boolean;
    TS6_FileNames: TStringList;
    TAS6_FileNames: TStringList;
    Obs6_FileNames: TStringList;
    READASARRAYS: Boolean;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    function GetAUXILIARY(Index: Integer): string;
    function GetCount: Integer;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property FIXED_CELL: Boolean read FFIXED_CELL;
    property Count: Integer read GetCount;
    property AUXILIARY[Index: Integer]: string read GetAUXILIARY;
    property AUXMULTNAME: string read FAUXMULTNAME;
    function IndexOfAUXILIARY(const AName: string): Integer;
  end;

  TRchDimensions = class(TCustomMf6Persistent)
  private
    MAXBOUND: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TRchTimeItem = class(TObject)
  private
    FId: Integer;
    Fcellid: TMfCellId;
    Frecharge: TMf6BoundaryValue;
    Faux: TList<TMf6BoundaryValue>;
    Fboundname: string;
    function GetAux(Index: Integer): TMf6BoundaryValue;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Keystring: string;
    property CellId: TMfCellId read Fcellid;
    property Recharge: TMf6BoundaryValue read Frecharge;
    property Count: integer read GetCount;
    property Aux[Index: Integer]: TMf6BoundaryValue read GetAux; default;
    property BoundName: string read Fboundname;
    property ID: Integer read FId;
  end;

  TRchTimeItemList = class(TObjectList<TRchTimeItem>)
    procedure Sort;
    function SameCells(OtherList: TRchTimeItemList): Boolean;
  end;

  TRchPeriod = class(TCustomMf6Persistent)
  private
    IPer: Integer;
    FCells: TRchTimeItemList;
    IRCH: TIArray3D;
    RECHARGE: TArrayItem;
    AuxList: TArrayItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      Dimensions: TDimensions; naux: Integer; BOUNDNAMES: Boolean;
      READASARRAYS: Boolean; PriorPeriod: TRchPeriod);
    function GetCell(Index: Integer): TRchTimeItem;
    function GetCount: Integer;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Period: Integer read IPer;
    property Count: Integer read GetCount;
    property Cells[Index: Integer]: TRchTimeItem read GetCell; default;
  end;

  TRchPeriodList = TObjectList<TRchPeriod>;

  TRch = class(TDimensionedPackageReader)
  private
    FOptions: TRchOptions;
    FRchDimensions: TRchDimensions;
    FPeriods: TRchPeriodList;
    FTimeSeriesPackages: TPackageList;
    FTimeSeriesArrayPackages: TPackageList;
    FObservationsPackages: TPackageList;
    function GetObservation(Index: Integer): TPackage;
    function GetObservationCount: Integer;
    function GetPeriod(Index: Integer): TRchPeriod;
    function GetPeriodCount: Integer;
    function GetTimeSeries(Index: Integer): TPackage;
    function GetTimeSeriesCount: Integer;
    function GetTimeSeriesArray(Index: Integer): TPackage;
    function GetTimeSeriesArrayCount: Integer;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer); override;
    property Options: TRchOptions read FOptions;
    property PeriodCount: Integer read GetPeriodCount;
    property Periods[Index: Integer]: TRchPeriod read GetPeriod;
    property TimeSeriesCount: Integer read GetTimeSeriesCount;
    property TimeSeries[Index: Integer]: TPackage read GetTimeSeries;
    property ObservationCount: Integer read GetObservationCount;
    property Observations[Index: Integer]: TPackage read GetObservation;
    property TimeSeriesArrayCount: Integer read GetTimeSeriesArrayCount;
    property TimeSeriesArray[Index: Integer]: TPackage read GetTimeSeriesArray;
  end;


implementation

uses
  ModelMuseUtilities, Mf6.TimeSeriesFileReaderUnit, Mf6.ObsFileReaderUnit,
  Mf6.TimeArraySeriesFileReaderUnit;

{ TRchOptions }

constructor TRchOptions.Create(PackageType: string);
begin
  FAUXILIARY := TStringList.Create;
  FAUXILIARY.CaseSensitive := False;
  TS6_FileNames := TStringList.Create;
  TAS6_FileNames := TStringList.Create;
  Obs6_FileNames := TStringList.Create;
  inherited;

end;

destructor TRchOptions.Destroy;
begin
  FAUXILIARY.Free;
  TS6_FileNames.Free;
  TAS6_FileNames.Free;
  Obs6_FileNames.Free;
  inherited;
end;

function TRchOptions.GetAUXILIARY(Index: Integer): string;
begin
  result := FAUXILIARY[Index];
end;

function TRchOptions.GetCount: Integer;
begin
  result := FAUXILIARY.Count;
end;

function TRchOptions.IndexOfAUXILIARY(const AName: string): Integer;
begin
  result := FAUXILIARY.IndexOf(AName);
end;

procedure TRchOptions.Initialize;
begin
  inherited;
  FFIXED_CELL := False;
  FAUXILIARY.Clear;
  TS6_FileNames.Clear;
  TAS6_FileNames.Clear;
  Obs6_FileNames.Clear;
  BOUNDNAMES := False;
  PRINT_INPUT := False;
  PRINT_FLOWS := False;
  SAVE_FLOWS := False;
  READASARRAYS := False;
end;

procedure TRchOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
  TS6_FileName: string;
  Obs_FileName: string;
  AUXILIARY_Name: string;
  TAS6_FileName: string;
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
    else if FSplitter[0] = 'FIXED_CELL' then
    begin
      FFIXED_CELL := True;
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
    else if FSplitter[0] = 'READASARRAYS' then
    begin
      READASARRAYS := True;
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
    else if (FSplitter[0] = 'TAS6')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEIN') then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      TAS6_FileName := FSplitter[2];
      TAS6_FileNames.Add(TAS6_FileName);
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

constructor TRchTimeItem.Create;
begin
  Fcellid.Initialize;
  Frecharge.Initialize;
  Faux := TList<TMf6BoundaryValue>.Create;
  Fboundname := '';
end;

destructor TRchTimeItem.Destroy;
begin
  Faux.Free;
  inherited;
end;

function TRchTimeItem.GetAux(Index: Integer): TMf6BoundaryValue;
begin
  result := Faux[Index];
end;

function TRchTimeItem.GetCount: integer;
begin
  result := Faux.Count;
end;

function TRchTimeItem.Keystring: string;
var
  AuxIndex: Integer;
  AnAux: TMf6BoundaryValue;
begin
  result := '';
  if FRecharge.ValueType = vtNumeric then
  begin
    result := result + ' Num';
  end
  else
  begin
    result := result + UpperCase(FRecharge.StringValue);
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

{ TRchDimensions }

procedure TRchDimensions.Initialize;
begin
  inherited;
  MAXBOUND := 0;
end;

procedure TRchDimensions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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

{ TRchPeriod }

constructor TRchPeriod.Create(PackageType: string);
begin
  FCells := TRchTimeItemList.Create;
  AuxList := TArrayItemList.Create;
  inherited;
end;

destructor TRchPeriod.Destroy;
begin
  AuxList.Free;
  FCells.Free;
  inherited;
end;

function TRchPeriod.GetCell(Index: Integer): TRchTimeItem;
begin
  Result := FCells[Index];
end;

function TRchPeriod.GetCount: Integer;
begin
  Result := FCells.Count;
end;

procedure TRchPeriod.Initialize;
begin
  inherited;
  FCells.Clear;
end;

procedure TRchPeriod.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  Dimensions: TDimensions; naux: Integer; BOUNDNAMES: Boolean;
  READASARRAYS: Boolean; PriorPeriod: TRchPeriod);
var
  DimensionCount: Integer;
  Cell: TRchTimeItem;
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
  Aux: TMf6BoundaryValue;
  StartIndex: Integer;
  AuxIndex: Integer;
  LocalDim: TDimensions;
  Layered: Boolean;
  IntThreeDReader: TInteger3DArrayReader;
  Double2DDReader: TDouble2DArrayReader;
  AuxArray: TArrayItem;
  RowIndex: Integer;
  ColIndex: Integer;
  NumberOfColumns: Integer;
  ID: Integer;
begin
  ID := 0;
  DimensionCount := Dimensions.DimensionCount;
  Initialize;
  NumberOfColumns := DimensionCount + 1 + naux;
  if READASARRAYS then
  begin
    LocalDim := Dimensions;
    LocalDim.NLay := 1;
    if PriorPeriod = nil then
    begin
      IRCH := nil;
      RECHARGE.Initialize;
    end
    else
    begin
      IRCH := PriorPeriod.IRCH;
      if IRCH <> nil then
      begin
        SetLength(IRCH, Length(IRCH), Length(IRCH[0]), Length(IRCH[0,0]));
        RECHARGE.Assign(PriorPeriod.RECHARGE);
        for AuxIndex := 0 to PriorPeriod.AuxList.Count - 1 do
        begin
          AuxArray.Assign(PriorPeriod.AuxList[AuxIndex]);
          AuxList.Add(AuxArray);
        end;
      end;
    end;
//    AuxList := TArrayItemList.Create;
    try
      AuxIndex := 0;
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
        else if FSplitter[0] = 'IRCH' then
        begin
          Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
          IntThreeDReader := TInteger3DArrayReader.Create(LocalDim, Layered, FPackageType);
          try
            IntThreeDReader.Read(Stream, Unhandled);
            IRCH := IntThreeDReader.FData;
          finally
            IntThreeDReader.Free;
          end;
        end
        else if FSplitter[0] = 'RECHARGE' then
        begin
          if (FSplitter.Count >= 3) and (FSplitter[1] = 'TIMEARRAYSERIES') then
          begin
            FSplitter.DelimitedText := CaseSensitiveLine;
            RECHARGE.TimeArraySeries := FSplitter[2]
          end
          else
          begin
            Double2DDReader := TDouble2DArrayReader.Create(LocalDim, FPackageType);
            try
              Double2DDReader.Read(Stream, Unhandled);
              RECHARGE.value := Double2DDReader.FData;
            finally
              Double2DDReader.Free;
            end;
          end;
        end
        else if FSplitter[0] = 'AUX' then
        begin
          AuxArray.Initialize;
          if (FSplitter.Count >= 3) and (FSplitter[1] = 'TIMEARRAYSERIES') then
          begin
            FSplitter.DelimitedText := CaseSensitiveLine;
            AuxArray.TimeArraySeries := FSplitter[2]
          end
          else
          begin
            Double2DDReader := TDouble2DArrayReader.Create(LocalDim, FPackageType);
            try
              Double2DDReader.Read(Stream, Unhandled);
              AuxArray.Value := Double2DDReader.FData;
              SetLength(AuxArray.Value, Length(AuxArray.Value), Length(AuxArray.Value[0]));
            finally
              Double2DDReader.Free;
            end;
          end;
          if PriorPeriod = nil then
          begin
            AuxList.Add(AuxArray)
          end
          else
          begin
            AuxList[AuxIndex].Assign(AuxArray);
            Inc(AuxIndex);
          end;
        end
      end;
    finally
      for RowIndex := 0 to LocalDim.NRow - 1 do
      begin
        for ColIndex := 0 to LocalDim.NCol - 1 do
        begin
          Cell := TRchTimeItem.Create;
          Inc(ID);
          Cell.FId := ID;
          if IRCH = nil then
          begin
            Cell.Fcellid.Layer := 1;
          end
          else
          begin
            Cell.Fcellid.Layer := IRCH[0, RowIndex, ColIndex];
          end;
          Cell.Fcellid.Row := RowIndex + 1;
          Cell.Fcellid.Column := ColIndex + 1;
          if RECHARGE.Value <> nil then
          begin
            Cell.Frecharge.NumericValue := RECHARGE.Value[RowIndex, ColIndex];
            Cell.Frecharge.ValueType := vtNumeric;
          end
          else
          begin
            Cell.Frecharge.ValueType := vtString;
            Cell.Frecharge.StringValue := RECHARGE.TimeArraySeries + '_'
              + IntToStr(RowIndex + 1) + '_' + IntToStr(ColIndex + 1)
          end;
          for AuxIndex := 0 to AuxList.Count - 1 do
          begin
            AuxArray := AuxList[AuxIndex];
            Aux.Initialize;
            if AuxArray.Value <> nil then
            begin
              Aux.ValueType := vtNumeric;
              Aux.NumericValue := AuxArray.Value[RowIndex, ColIndex];
            end
            else
            begin
              Aux.ValueType := vtString;
              Aux.StringValue := AuxArray.TimeArraySeries + '_'
                + IntToStr(RowIndex + 1) + '_' + IntToStr(ColIndex + 1)
            end;
          end;
          FCells.Add(Cell);
        end;
      end;
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

      Cell := TRchTimeItem.Create;
      try
        Inc(ID);
        Cell.FId := ID;
        CaseSensitiveLine := ALine;
        if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'PERIOD') then
        begin
          // do nothing
        end
        else if FSplitter.Count >= NumberOfColumns then
        begin
          if ReadCellID(Cell.Fcellid, 0, DimensionCount) then
          begin
            if TryFortranStrToFloat(FSplitter[DimensionCount], Cell.Frecharge.NumericValue) then
            begin
              Cell.Frecharge.ValueType := vtNumeric;
            end
            else
            begin
              Cell.Frecharge.ValueType := vtString;
              FSplitter.DelimitedText := CaseSensitiveLine;
              Cell.Frecharge.StringValue := FSplitter[DimensionCount];
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
            if BOUNDNAMES and (FSplitter.Count >= NumberOfColumns+1) then
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
        if Cell <> nil then
        begin
          Dec(ID);
        end;
        Cell.Free;
      end;
    end;
  end;

end;

{ TRch }

constructor TRch.Create(PackageType: string);
begin
  inherited;
  FOptions := TRchOptions.Create(PackageType);
  FRchDimensions := TRchDimensions.Create(PackageType);
  FPeriods := TRchPeriodList.Create;
  FTimeSeriesPackages := TPackageList.Create;
  FObservationsPackages := TPackageList.Create;
  FTimeSeriesArrayPackages := TPackageList.Create;

  FRchDimensions.Initialize;
end;

destructor TRch.Destroy;
begin
  FOptions.Free;
  FRchDimensions.Free;
  FPeriods.Free;
  FTimeSeriesPackages.Free;
  FObservationsPackages.Free;
  FTimeSeriesArrayPackages.Free;
  inherited;
end;

function TRch.GetObservation(Index: Integer): TPackage;
begin
  result := FObservationsPackages[Index];
end;

function TRch.GetObservationCount: Integer;
begin
    result := FObservationsPackages.Count;
end;

function TRch.GetPeriod(Index: Integer): TRchPeriod;
begin
  result := FPeriods[Index];
end;

function TRch.GetPeriodCount: Integer;
begin
  result := FPeriods.Count;
end;

function TRch.GetTimeSeries(Index: Integer): TPackage;
begin
  Result := FTimeSeriesPackages[Index];
end;

function TRch.GetTimeSeriesArray(Index: Integer): TPackage;
begin
  result := FTimeSeriesArrayPackages[Index];
end;

function TRch.GetTimeSeriesArrayCount: Integer;
begin
  result := FTimeSeriesArrayPackages.Count;
end;

function TRch.GetTimeSeriesCount: Integer;
begin
  Result := FTimeSeriesPackages.Count;
end;

procedure TRch.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
var
  ALine: string;
  ErrorLine: string;
  IPER: Integer;
  APeriod: TRchPeriod;
  TsPackage: TPackage;
  PackageIndex: Integer;
  TsReader: TTimeSeries;
  ObsReader: TObs;
  ObsPackage: TPackage;
  TasPackage: TPackage;
  TasReader: TTimeArraySeries;
  PriorPeriod: TRchPeriod;
begin
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading RCH package');
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
        FRchDimensions.Read(Stream, Unhandled);
      end
      else if (FSplitter[1] ='PERIOD') and (FSplitter.Count >= 3) then
      begin
        if TryStrToInt(FSplitter[2], IPER) then
        begin
          if IPER > NPER then
          begin
            break;
          end;
          if FPeriods.Count > 0 then
          begin
            PriorPeriod := FPeriods.Last;
          end
          else
          begin
            PriorPeriod := nil;
          end;
          APeriod := TRchPeriod.Create(FPackageType);
          FPeriods.Add(APeriod);
          APeriod.IPer := IPER;
          APeriod.Read(Stream, Unhandled, FDimensions, FOptions.FAUXILIARY.Count,
            FOptions.BOUNDNAMES, FOptions.READASARRAYS, PriorPeriod);
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
  for PackageIndex := 0 to FOptions.TAS6_FileNames.Count - 1 do
  begin
    TasPackage := TPackage.Create;
    FTimeSeriesArrayPackages.Add(TasPackage);
    TasPackage.FileType := FPackageType;
    TasPackage.FileName := FOptions.TAS6_FileNames[PackageIndex];
    TasPackage.PackageName := '';

    TasReader := TTimeArraySeries.Create(FPackageType);
    TasPackage.Package := TasReader;
    TasPackage.ReadPackage(Unhandled, NPER);
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

{ TRchTimeItemList }

function TRchTimeItemList.SameCells(OtherList: TRchTimeItemList): Boolean;
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

procedure TRchTimeItemList.Sort;
begin
  inherited Sort(
    TComparer<TRchTimeItem>.Construct(
      function(const Left, Right: TRchTimeItem): Integer
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
