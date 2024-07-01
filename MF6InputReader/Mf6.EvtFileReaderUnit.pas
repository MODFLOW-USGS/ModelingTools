unit Mf6.EvtFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections, System.Generics.Defaults;

type
  TEvtOptions = class(TCustomMf6Persistent)
  private
    FFIXED_CELL: Boolean;
    FAUXILIARY: TStringList;
    FAUXMULTNAME: string;
    FBOUNDNAMES: Boolean;
    PRINT_INPUT: Boolean;
    PRINT_FLOWS: Boolean;
    SAVE_FLOWS: Boolean;
    TS6_FileNames: TStringList;
    TAS6_FileNames: TStringList;
    Obs6_FileNames: TStringList;
    READASARRAYS: Boolean;
    FSURF_RATE_SPECIFIED: Boolean;
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
    property SURF_RATE_SPECIFIED: Boolean read FSURF_RATE_SPECIFIED;
  end;

  TEvtDimensions = class(TCustomMf6Persistent)
  private
    MAXBOUND: Integer;
    FNSEG: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    property NSEG: Integer read FNSEG;
  end;

  TEvtTimeItem = class(TObject)
  private
    Fcellid: TMfCellId;
    Fsurf: TMf6BoundaryValue;
    Frate: TMf6BoundaryValue;
    Fdepth: TMf6BoundaryValue;
    Fpxdp: TBoundaryValueList;
    Fpetm: TBoundaryValueList;
    Fpetm0: TMf6BoundaryValue;
    Faux: TList<TMf6BoundaryValue>;
    Fboundname: string;
    function GetAux(Index: Integer): TMf6BoundaryValue;
    function GetCount: integer;
    function GetPetm(Index: Integer): TMf6BoundaryValue;
    function GetPxdp(Index: Integer): TMf6BoundaryValue;
  public
    constructor Create;
    destructor Destroy; override;
    function Keystring(Map: TimeSeriesMap): string;
    property CellId: TMfCellId read Fcellid;
    property Surf: TMf6BoundaryValue read Fsurf;
    property Rate: TMf6BoundaryValue read Frate;
    property Depth: TMf6BoundaryValue read Fdepth;
    property Pxdp[Index: Integer]: TMf6BoundaryValue read GetPxdp;
    property Petm[Index: Integer]: TMf6BoundaryValue read GetPetm;
    property Count: integer read GetCount;
    property Aux[Index: Integer]: TMf6BoundaryValue read GetAux; default;
    property BoundName: string read Fboundname;
    property Petm0: TMf6BoundaryValue read Fpetm0;
  end;

  TEvtTimeItemList = class(TObjectList<TEvtTimeItem>)
    procedure Sort;
    function SameCells(OtherList: TEvtTimeItemList): Boolean;
  end;

  TEvtPeriod = class(TCustomMf6Persistent)
  private
    IPer: Integer;
    FCells: TEvtTimeItemList;
    SURFACE: TArrayItem;
    RATE: TArrayItem;
    DEPTH: TArrayItem;
    IEVT: TIArray2D;
    AuxList: TArrayItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      Dimensions: TDimensions; naux: Integer; BOUNDNAMES: Boolean;
      READASARRAYS: Boolean; NSEG: Integer; SURF_RATE_SPECIFIED: Boolean;
      PriorPeriod: TEvtPeriod; Options: TEvtOptions);
    function GetCell(Index: Integer): TEvtTimeItem;
    function GetCount: Integer;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Period: Integer read IPer;
    property Count: Integer read GetCount;
    property Cells[Index: Integer]: TEvtTimeItem read GetCell; default;
  end;

  TEvtPeriodList = TObjectList<TEvtPeriod>;

  TEvt = class(TDimensionedPackageReader)
  private
    FOptions: TEvtOptions;
    FEvtDimensions: TEvtDimensions;
    FPeriods: TEvtPeriodList;
    FTimeSeriesPackages: TPackageList;
    FTimeSeriesArrayPackages: TPackageList;
    FObservationsPackages: TPackageList;
    function GetObservation(Index: Integer): TPackage;
    function GetObservationCount: Integer;
    function GetPeriod(Index: Integer): TEvtPeriod;
    function GetPeriodCount: Integer;
    function GetTimeSeries(Index: Integer): TPackage;
    function GetTimeSeriesCount: Integer;
    function GetTimeSeriesArray(Index: Integer): TPackage;
    function GetTimeSeriesArrayCount: Integer;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer); override;
    property Options: TEvtOptions read FOptions;
    property EvtDimensions: TEvtDimensions read FEvtDimensions;
    property PeriodCount: Integer read GetPeriodCount;
    property Periods[Index: Integer]: TEvtPeriod read GetPeriod;
    property TimeSeriesCount: Integer read GetTimeSeriesCount;
    property TimeSeries[Index: Integer]: TPackage read GetTimeSeries;
    property TimeSeriesArrayCount: Integer read GetTimeSeriesArrayCount;
    property TimeSeriesArray[Index: Integer]: TPackage read GetTimeSeriesArray;
    property ObservationCount: Integer read GetObservationCount;
    property Observations[Index: Integer]: TPackage read GetObservation;
  end;


implementation

uses
  ModelMuseUtilities, Mf6.TimeSeriesFileReaderUnit, Mf6.ObsFileReaderUnit,
  Mf6.TimeArraySeriesFileReaderUnit;

{ TEvtOptions }

constructor TEvtOptions.Create(PackageType: string);
begin
  FAUXILIARY := TStringList.Create;
  FAUXILIARY.CaseSensitive := False;
  TS6_FileNames := TStringList.Create;
  TAS6_FileNames := TStringList.Create;
  Obs6_FileNames := TStringList.Create;
  inherited;

end;

destructor TEvtOptions.Destroy;
begin
  FAUXILIARY.Free;
  TS6_FileNames.Free;
  TAS6_FileNames.Free;
  Obs6_FileNames.Free;
  inherited;
end;

function TEvtOptions.GetAUXILIARY(Index: Integer): string;
begin
  result := FAUXILIARY[Index];
end;

function TEvtOptions.GetCount: Integer;
begin
  result := FAUXILIARY.Count;
end;

function TEvtOptions.IndexOfAUXILIARY(const AName: string): Integer;
begin
  result := FAUXILIARY.IndexOf(AName)
end;

procedure TEvtOptions.Initialize;
begin
  inherited;
  FFIXED_CELL := False;
  FAUXILIARY.Clear;
  TS6_FileNames.Clear;
  TAS6_FileNames.Clear;
  Obs6_FileNames.Clear;
  FBOUNDNAMES := False;
  PRINT_INPUT := False;
  PRINT_FLOWS := False;
  SAVE_FLOWS := False;
  READASARRAYS := False;
  FSURF_RATE_SPECIFIED := False;
end;

procedure TEvtOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
      FBOUNDNAMES := True;
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
    else if FSplitter[0] = 'SURF_RATE_SPECIFIED' then
    begin
      FSURF_RATE_SPECIFIED := True;
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

constructor TEvtTimeItem.Create;
begin
  Fcellid.Initialize;
  Fsurf.Initialize;
  Frate.Initialize;
  Fdepth.Initialize;
  Fpxdp := TBoundaryValueList.Create;
  Fpetm := TBoundaryValueList.Create;
  Fpetm0.Initialize;
  Faux := TList<TMf6BoundaryValue>.Create;
  Fboundname := '';
end;

destructor TEvtTimeItem.Destroy;
begin
  Fpxdp.Free;
  Fpetm.Free;
  Faux.Free;
  inherited;
end;

function TEvtTimeItem.GetAux(Index: Integer): TMf6BoundaryValue;
begin
  result := Faux[Index];
end;

function TEvtTimeItem.GetCount: integer;
begin
  result := Faux.Count
end;

function TEvtTimeItem.GetPetm(Index: Integer): TMf6BoundaryValue;
begin
  result := Fpetm[Index];
end;

function TEvtTimeItem.GetPxdp(Index: Integer): TMf6BoundaryValue;
begin
  result := Fpxdp[Index];
end;

function TEvtTimeItem.Keystring(Map: TimeSeriesMap): string;
var
  AuxIndex: Integer;
  AnAux: TMf6BoundaryValue;
  Index: Integer;
  BoundValue: TMf6BoundaryValue;
  TsName: string;
begin
  result := '';
  if Fsurf.ValueType = vtNumeric then
  begin
    result := result + ' Num';
  end
  else
  begin
    if Map.TryGetValue(UpperCase(Fsurf.StringValue), TsName) then
    begin
      result := result + TsName;
    end
    else
    begin
      result := result + UpperCase(Fsurf.StringValue);
    end;

  end;

  if Frate.ValueType = vtNumeric then
  begin
    result := result + ' Num';
  end
  else
  begin
    if Map.TryGetValue(UpperCase(Frate.StringValue), TsName) then
    begin
      result := result + TsName;
    end
    else
    begin
      result := result + UpperCase(Frate.StringValue);
    end;
  end;

  if Fdepth.ValueType = vtNumeric then
  begin
    result := result + ' Num';
  end
  else
  begin
    if Map.TryGetValue(UpperCase(Fdepth.StringValue), TsName) then
    begin
      result := result + TsName;
    end
    else
    begin
      result := result + UpperCase(Fdepth.StringValue);
    end;
  end;

  for Index := 0 to Fpxdp.Count - 1 do
  begin
    BoundValue := Fpxdp[Index];
    if BoundValue.ValueType = vtNumeric then
    begin
      result := result + ' Num';
    end
    else
    begin
      if Map.TryGetValue(UpperCase(BoundValue.StringValue), TsName) then
      begin
        result := result + TsName;
      end
      else
      begin
        result := result + UpperCase(BoundValue.StringValue);
      end;
    end;
  end;

  for Index := 0 to Fpetm.Count - 1 do
  begin
    BoundValue := Fpetm[Index];
    if BoundValue.ValueType = vtNumeric then
    begin
      result := result + ' Num';
    end
    else
    begin
      if Map.TryGetValue(UpperCase(BoundValue.StringValue), TsName) then
      begin
        result := result + TsName;
      end
      else
      begin
        result := result + UpperCase(BoundValue.StringValue);
      end;
    end;
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
      if Map.TryGetValue(UpperCase(AnAux.StringValue), TsName) then
      begin
        result := result + TsName;
      end
      else
      begin
        result := result + UpperCase(AnAux.StringValue);
      end;
    end;
  end;
end;

{ TEvtDimensions }

procedure TEvtDimensions.Initialize;
begin
  inherited;
  MAXBOUND := 0;
  FNSEG := 1;
end;

procedure TEvtDimensions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    else if (FSplitter[0] = 'NSEG') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], FNSEG) then
    begin
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TEvtPeriod }

constructor TEvtPeriod.Create(PackageType: string);
begin
  FCells := TEvtTimeItemList.Create;
  AuxList := TArrayItemList.Create;
  inherited;
end;

destructor TEvtPeriod.Destroy;
begin
  FCells.Free;
  AuxList.Free;
  inherited;
end;

function TEvtPeriod.GetCell(Index: Integer): TEvtTimeItem;
begin
  Result := FCells[Index];
end;

function TEvtPeriod.GetCount: Integer;
begin
  Result := FCells.Count;
end;

procedure TEvtPeriod.Initialize;
begin
  inherited;
  FCells.Clear;
end;

procedure TEvtPeriod.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  Dimensions: TDimensions; naux: Integer; BOUNDNAMES: Boolean;
  READASARRAYS: Boolean; NSEG: Integer; SURF_RATE_SPECIFIED: Boolean;
  PriorPeriod: TEvtPeriod; Options: TEvtOptions);
var
  DimensionCount: Integer;
  Cell: TEvtTimeItem;
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
  Aux: TMf6BoundaryValue;
  StartIndex: Integer;
  AuxIndex: Integer;
  LocalDim: TDimensions;
  IntTwoDReader: TInteger2DArrayReader;
  Double2DDReader: TDouble2DArrayReader;
  AuxArray: TArrayItem;
  RowIndex: Integer;
  ColIndex: Integer;
  SegIndex: Integer;
  ABoundValue: TMf6BoundaryValue;
  NumberOfColumns: Integer;
begin
  DimensionCount := Dimensions.DimensionCount;
  NumberOfColumns := DimensionCount + 3 + (NSEG-1)*2 + naux;
  if SURF_RATE_SPECIFIED then
  begin
    Inc(NumberOfColumns);
  end;
  Initialize;
  if READASARRAYS then
  begin
    LocalDim := Dimensions;
    LocalDim.NLay := 1;
    if PriorPeriod = nil then
    begin
      SURFACE.Initialize;
      RATE.Initialize;
      DEPTH.Initialize;
      IEVT := nil;
    end
    else
    begin
      SURFACE.Assign(PriorPeriod.SURFACE);
      RATE.Assign(PriorPeriod.RATE);
      DEPTH.Assign(PriorPeriod.DEPTH);
      for AuxIndex := 0 to PriorPeriod.AuxList.Count - 1 do
      begin
        AuxArray.Assign(PriorPeriod.AuxList[AuxIndex]);
        AuxList.Add(AuxArray);
      end;
      IEVT := PriorPeriod.IEVT;
      if IEVT <> nil then
      begin
        SetLength(IEVT, Length(IEVT), Length(IEVT[0]));
      end;
      PriorPeriod.SURFACE.Initialize;
      PriorPeriod.RATE.Initialize;
      PriorPeriod.DEPTH.Initialize;
      PriorPeriod.IEVT := nil;
    end;
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

//        IEVT := nil;
        CaseSensitiveLine := ALine;
        if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'PERIOD') then
        begin
          // do nothing
        end
        else if AnsiSametext(FSplitter[0], 'IEVT') then
        begin
          IntTwoDReader := TInteger2DArrayReader.Create(LocalDim, FPackageType);
          try
            IntTwoDReader.Read(Stream, Unhandled);
            IEVT := IntTwoDReader.FData;
          finally
            IntTwoDReader.Free;
          end;
        end
        else if AnsiSametext(FSplitter[0], 'SURFACE') then
        begin
          if (FSplitter.Count >= 3) and AnsiSametext(FSplitter[1], 'TIMEARRAYSERIES') then
          begin
            FSplitter.DelimitedText := CaseSensitiveLine;
            SURFACE.TimeArraySeries := FSplitter[2];
            SURFACE.value := nil;
          end
          else
          begin
            Double2DDReader := TDouble2DArrayReader.Create(LocalDim, FPackageType);
            try
              Double2DDReader.Read(Stream, Unhandled);
              SURFACE.value := Double2DDReader.FData;
              SURFACE.TimeArraySeries := '';
            finally
              Double2DDReader.Free;
            end;
          end;
        end
        else if AnsiSametext(FSplitter[0], 'RATE') then
        begin
          if (FSplitter.Count >= 3) and AnsiSametext(FSplitter[1], 'TIMEARRAYSERIES') then
          begin
            FSplitter.DelimitedText := CaseSensitiveLine;
            RATE.TimeArraySeries := FSplitter[2];
            RATE.value := nil;
          end
          else
          begin
            Double2DDReader := TDouble2DArrayReader.Create(LocalDim, FPackageType);
            try
              Double2DDReader.Read(Stream, Unhandled);
              RATE.value := Double2DDReader.FData;
              RATE.TimeArraySeries :='';
            finally
              Double2DDReader.Free;
            end;
          end;
        end
        else if AnsiSametext(FSplitter[0], 'DEPTH') then
        begin
          if (FSplitter.Count >= 3) and AnsiSametext(FSplitter[1], 'TIMEARRAYSERIES') then
          begin
            FSplitter.DelimitedText := CaseSensitiveLine;
            DEPTH.TimeArraySeries := FSplitter[2];
            DEPTH.value := nil;
          end
          else
          begin
            Double2DDReader := TDouble2DArrayReader.Create(LocalDim, FPackageType);
            try
              Double2DDReader.Read(Stream, Unhandled);
              DEPTH.value := Double2DDReader.FData;
              DEPTH.TimeArraySeries := '';
            finally
              Double2DDReader.Free;
            end;
          end;
        end
        else
        begin
          AuxIndex := Options.FAUXILIARY.IndexOf(FSplitter[0]);
          if AuxIndex >= 0 then
          begin
            AuxArray.Initialize;
            if (FSplitter.Count >= 3) and AnsiSametext(FSplitter[1], 'TIMEARRAYSERIES') then
            begin
              FSplitter.DelimitedText := CaseSensitiveLine;
              AuxArray.TimeArraySeries := FSplitter[2];
              AuxArray.value := nil;
            end
            else
            begin
              Double2DDReader := TDouble2DArrayReader.Create(LocalDim, FPackageType);
              try
                Double2DDReader.Read(Stream, Unhandled);
                AuxArray.Value := Double2DDReader.FData;
                SetLength(AuxArray.Value, Length(AuxArray.Value), Length(AuxArray.Value[0]));
                AuxArray.TimeArraySeries := '';
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
            end;
          end
          else
          begin
            Unhandled.WriteLine(Format(StrUnrecognizedSPERI, [FPackageType]));
            Unhandled.WriteLine(ErrorLine);
          end;
        end;
      end;
    finally
      for RowIndex := 0 to LocalDim.NRow - 1 do
      begin
        for ColIndex := 0 to LocalDim.NCol - 1 do
        begin
          Cell := TEvtTimeItem.Create;
          if IEVT = nil then
          begin
            Cell.Fcellid.Layer := 1;
          end
          else
          begin
            Cell.Fcellid.Layer := IEVT[RowIndex, ColIndex];
          end;
          Cell.Fcellid.Row := RowIndex + 1;
          Cell.Fcellid.Column := ColIndex + 1;
          if SURFACE.Value <> nil then
          begin
            Cell.Fsurf.NumericValue := SURFACE.Value[RowIndex, ColIndex];
            Cell.Fsurf.ValueType := vtNumeric;
          end
          else
          begin
            Cell.Fsurf.ValueType := vtString;
            Cell.Fsurf.StringValue := SURFACE.TimeArraySeries + '_'
              + IntToStr(RowIndex + 1) + '_' + IntToStr(ColIndex + 1)
          end;
          if RATE.Value <> nil then
          begin
            Cell.Frate.NumericValue := RATE.Value[RowIndex, ColIndex];
            Cell.Frate.ValueType := vtNumeric;
            if Cell.Frate.NumericValue = 0 then
            begin
              Cell.Free;
              Continue;
            end;
          end
          else
          begin
            Cell.Frate.ValueType := vtString;
            Cell.Frate.StringValue := RATE.TimeArraySeries + '_'
              + IntToStr(RowIndex + 1) + '_' + IntToStr(ColIndex + 1)
          end;
          if DEPTH.Value <> nil then
          begin
            Cell.Fdepth.NumericValue := DEPTH.Value[RowIndex, ColIndex];
            Cell.Fdepth.ValueType := vtNumeric;
          end
          else
          begin
            Cell.Fdepth.ValueType := vtString;
            Cell.Fdepth.StringValue := DEPTH.TimeArraySeries + '_'
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
            Cell.Faux.Add(Aux);
          end;
          FCells.Add(Cell);
        end;
      end;

//      AuxList.Free;
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

      Cell := TEvtTimeItem.Create;
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
            if TryFortranStrToFloat(FSplitter[DimensionCount], Cell.Fsurf.NumericValue) then
            begin
              Cell.Fsurf.ValueType := vtNumeric;
            end
            else
            begin
              Cell.Fsurf.ValueType := vtString;
              FSplitter.DelimitedText := CaseSensitiveLine;
              Cell.Fsurf.StringValue := FSplitter[DimensionCount];
            end;
            StartIndex := DimensionCount + 1;

            if TryFortranStrToFloat(FSplitter[StartIndex], Cell.Frate.NumericValue) then
            begin
              Cell.Frate.ValueType := vtNumeric;
            end
            else
            begin
              Cell.Frate.ValueType := vtString;
              FSplitter.DelimitedText := CaseSensitiveLine;
              Cell.Frate.StringValue := FSplitter[StartIndex];
            end;
            Inc(StartIndex);

            if TryFortranStrToFloat(FSplitter[StartIndex], Cell.Fdepth.NumericValue) then
            begin
              Cell.Fdepth.ValueType := vtNumeric;
            end
            else
            begin
              Cell.Fdepth.ValueType := vtString;
              FSplitter.DelimitedText := CaseSensitiveLine;
              Cell.Fdepth.StringValue := FSplitter[StartIndex];
            end;
            Inc(StartIndex);

            for SegIndex := 0 to nseg - 2 do
            begin
              ABoundValue.Initialize;
              if TryFortranStrToFloat(FSplitter[StartIndex], ABoundValue.NumericValue) then
              begin
                ABoundValue.ValueType := vtNumeric;
              end
              else
              begin
                ABoundValue.ValueType := vtString;
                FSplitter.DelimitedText := CaseSensitiveLine;
                ABoundValue.StringValue := FSplitter[StartIndex];
              end;
              Cell.Fpxdp.Add(ABoundValue);
              Inc(StartIndex);
            end;

            for SegIndex := 0 to nseg - 2 do
            begin
              ABoundValue.Initialize;
              if TryFortranStrToFloat(FSplitter[StartIndex], ABoundValue.NumericValue) then
              begin
                ABoundValue.ValueType := vtNumeric;
              end
              else
              begin
                ABoundValue.ValueType := vtString;
                FSplitter.DelimitedText := CaseSensitiveLine;
                ABoundValue.StringValue := FSplitter[StartIndex];
              end;
              Cell.Fpetm.Add(ABoundValue);
              Inc(StartIndex);
            end;

            if SURF_RATE_SPECIFIED then
            begin
              if TryFortranStrToFloat(FSplitter[StartIndex], Cell.Fpetm0.NumericValue) then
              begin
                Cell.Fpetm0.ValueType := vtNumeric;
              end
              else
              begin
                Cell.Fpetm0.ValueType := vtString;
                FSplitter.DelimitedText := CaseSensitiveLine;
                Cell.Fpetm0.StringValue := FSplitter[StartIndex];
              end;
              Inc(StartIndex);
            end;

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
        Cell.Free;
      end;
    end;
  end;
end;

{ Evt }

constructor TEvt.Create(PackageType: string);
begin
  inherited;
  FOptions := TEvtOptions.Create(PackageType);
  FEvtDimensions := TEvtDimensions.Create(PackageType);
  FPeriods := TEvtPeriodList.Create;
  FTimeSeriesPackages := TPackageList.Create;
  FObservationsPackages := TPackageList.Create;
  FTimeSeriesArrayPackages := TPackageList.Create;

  FEvtDimensions.Initialize;
end;

destructor TEvt.Destroy;
begin
  FOptions.Free;
  FEvtDimensions.Free;
  FPeriods.Free;
  FTimeSeriesPackages.Free;
  FObservationsPackages.Free;
  FTimeSeriesArrayPackages.Free;
  inherited;
end;

function TEvt.GetObservation(Index: Integer): TPackage;
begin
  result := FObservationsPackages[Index];
end;

function TEvt.GetObservationCount: Integer;
begin
  result := FObservationsPackages.Count;
end;

function TEvt.GetPeriod(Index: Integer): TEvtPeriod;
begin
  result := FPeriods[Index];
end;

function TEvt.GetPeriodCount: Integer;
begin
  result := FPeriods.Count;
end;

function TEvt.GetTimeSeries(Index: Integer): TPackage;
begin
  Result := FTimeSeriesPackages[Index];
end;

function TEvt.GetTimeSeriesArray(Index: Integer): TPackage;
begin
  result := FTimeSeriesArrayPackages[Index];
end;

function TEvt.GetTimeSeriesArrayCount: Integer;
begin
  result := FTimeSeriesArrayPackages.Count
end;

function TEvt.GetTimeSeriesCount: Integer;
begin
  Result := FTimeSeriesPackages.Count;
end;

procedure TEvt.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
var
  ALine: string;
  ErrorLine: string;
  IPER: Integer;
  APeriod: TEvtPeriod;
  TsPackage: TPackage;
  PackageIndex: Integer;
  TsReader: TTimeSeries;
  ObsReader: TObs;
  ObsPackage: TPackage;
  TasPackage: TPackage;
  TasReader: TTimeArraySeries;
  PriorPeriod: TEvtPeriod;
begin
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading EVT package');
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
        FEvtDimensions.Read(Stream, Unhandled);
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
          APeriod := TEvtPeriod.Create(FPackageType);
          FPeriods.Add(APeriod);
          APeriod.IPer := IPER;
          APeriod.Read(Stream, Unhandled, FDimensions, FOptions.FAUXILIARY.Count,
            FOptions.FBOUNDNAMES, FOptions.READASARRAYS, FEvtDimensions.FNSEG,
            FOptions.FSURF_RATE_SPECIFIED, PriorPeriod, FOptions);
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
    TasReader.Dimensions := FDimensions;
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

{ TEvtTimeItemList }

function TEvtTimeItemList.SameCells(OtherList: TEvtTimeItemList): Boolean;
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

procedure TEvtTimeItemList.Sort;
begin
  inherited Sort(
    TComparer<TEvtTimeItem>.Construct(
      function(const Left, Right: TEvtTimeItem): Integer
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
