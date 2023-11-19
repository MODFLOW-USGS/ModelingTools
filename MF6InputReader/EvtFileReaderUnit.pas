unit EvtFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TEvtOptions = class(TCustomMf6Persistent)
  private
    FIXED_CELL: Boolean;
    AUXILIARY: TStringList;
    AUXMULTNAME: TStringList;
    BOUNDNAMES: Boolean;
    PRINT_INPUT: Boolean;
    PRINT_FLOWS: Boolean;
    SAVE_FLOWS: Boolean;
    TS6_FileNames: TStringList;
    TAS6_FileNames: TStringList;
    Obs6_FileNames: TStringList;
    READASARRAYS: Boolean;
    SURF_RATE_SPECIFIED: Boolean;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TEvtDimensions = class(TCustomMf6Persistent)
  private
    MAXBOUND: Integer;
    NSEG: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TEvtTimeItem = class(TObject)
    cellid: TCellId;
    surf: TBoundaryValue;
    rate: TBoundaryValue;
    depth: TBoundaryValue;
    pxdp: TBoundaryValueList;
    petm: TBoundaryValueList;
    petm0: TBoundaryValue;
    aux: TList<TBoundaryValue>;
    boundname: string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TEvtTimeItemList = TList<TEvtTimeItem>;

  TEvtPeriod = class(TCustomMf6Persistent)
  private
    IPer: Integer;
    FCells: TEvtTimeItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      Dimensions: TDimensions; naux: Integer; BOUNDNAMES: Boolean;
      READASARRAYS: Boolean; NSEG: Integer; SURF_RATE_SPECIFIED: Boolean);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
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
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
  end;


implementation

uses
  ModelMuseUtilities, TimeSeriesFileReaderUnit, ObsFileReaderUnit,
  TimeArraySeriesFileReaderUnit;

{ TEvtOptions }

constructor TEvtOptions.Create(PackageType: string);
begin
  AUXILIARY := TStringList.Create;
  AUXMULTNAME := TStringList.Create;
  TS6_FileNames := TStringList.Create;
  TAS6_FileNames := TStringList.Create;
  Obs6_FileNames := TStringList.Create;
  inherited;

end;

destructor TEvtOptions.Destroy;
begin
  AUXILIARY.Free;
  AUXMULTNAME.Free;
  TS6_FileNames.Free;
  TAS6_FileNames.Free;
  Obs6_FileNames.Free;
  inherited;
end;

procedure TEvtOptions.Initialize;
begin
  inherited;
  FIXED_CELL := False;
  AUXILIARY.Clear;
  AUXMULTNAME.Clear;
  TS6_FileNames.Clear;
  TAS6_FileNames.Clear;
  Obs6_FileNames.Clear;
  BOUNDNAMES := False;
  PRINT_INPUT := False;
  PRINT_FLOWS := False;
  SAVE_FLOWS := False;
  READASARRAYS := False;
  SURF_RATE_SPECIFIED := False;
end;

procedure TEvtOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
  TS6_FileName: string;
  Obs_FileName: string;
  AUXILIARY_Name: string;
  AUXMULTNAME_Name: string;
  TAS6_FileName: string;
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
    if FSplitter[0] = 'FIXED_CELL' then
    begin
      FIXED_CELL := True;
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
    else if FSplitter[0] = 'SURF_RATE_SPECIFIED' then
    begin
      SURF_RATE_SPECIFIED := True;
    end
    else if (FSplitter[0] = 'AUXILIARY')
      and (FSplitter.Count >= 2) then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      AUXILIARY_Name := FSplitter[1];
      AUXILIARY.Add(AUXILIARY_Name);
    end
    else if (FSplitter[0] = 'AUXMULTNAME')
      and (FSplitter.Count >= 2) then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      AUXMULTNAME_Name := FSplitter[1];
      AUXMULTNAME.Add(AUXMULTNAME_Name);
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
      Unhandled.WriteLine(Format(StrUnrecognizedOCOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TChdDTimeItem }

constructor TEvtTimeItem.Create;
begin
  cellid.Initialize;
  surf.Initialize;
  rate.Initialize;
  depth.Initialize;
  pxdp := TBoundaryValueList.Create;
  petm := TBoundaryValueList.Create;
  petm0.Initialize;
  aux := TList<TBoundaryValue>.Create;
  boundname := '';
end;

destructor TEvtTimeItem.Destroy;
begin
  pxdp.Free;
  petm.Free;
  aux.Free;
  inherited;
end;

{ TEvtDimensions }

procedure TEvtDimensions.Initialize;
begin
  inherited;
  MAXBOUND := 0;
  NSEG := 0;
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

    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    Assert(FSplitter.Count > 0);
    if (FSplitter[0] = 'MAXBOUND') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], MAXBOUND) then
    begin
    end
    else if (FSplitter[0] = 'NSEG') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], NSEG) then
    begin
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOCOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TEvtPeriod }

constructor TEvtPeriod.Create(PackageType: string);
begin
  FCells := TEvtTimeItemList.Create;
  inherited;
end;

destructor TEvtPeriod.Destroy;
begin
  FCells.Free;
  inherited;
end;

procedure TEvtPeriod.Initialize;
begin
  inherited;
  FCells.Clear;
end;

procedure TEvtPeriod.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  Dimensions: TDimensions; naux: Integer; BOUNDNAMES: Boolean;
  READASARRAYS: Boolean; NSEG: Integer; SURF_RATE_SPECIFIED: Boolean);
var
  DimensionCount: Integer;
  Cell: TEvtTimeItem;
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
  Aux: TBoundaryValue;
  StartIndex: Integer;
  AuxIndex: Integer;
  LocalDim: TDimensions;
  Layered: Boolean;
  IntThreeDReader: TInteger3DArrayReader;
  IEVT: TIArray3D;
  Double2DDReader: TDouble2DArrayReader;
  SURFACE: TArrayItem;
  RATE: TArrayItem;
  DEPTH: TArrayItem;
  AuxArray: TArrayItem;
  AuxList: TArrayItemList;
  RowIndex: Integer;
  ColIndex: Integer;
  SegIndex: Integer;
  ABoundValue: TBoundaryValue;
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
    SURFACE.Initialize;
    RATE.Initialize;
    DEPTH.Initialize;
    AuxList := TArrayItemList.Create;
    try
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

        IEVT := nil;
        CaseSensitiveLine := ALine;
        ALine := UpperCase(ALine);
        FSplitter.DelimitedText := ALine;
        if FSplitter[0] = 'IEVT' then
        begin
          Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
          IntThreeDReader := TInteger3DArrayReader.Create(LocalDim, Layered, FPackageType);
          try
            IntThreeDReader.Read(Stream, Unhandled);
            IEVT := IntThreeDReader.FData;
          finally
            IntThreeDReader.Free;
          end;
        end
        else if FSplitter[0] = 'RECHARGE' then
        begin
          if (FSplitter.Count >= 3) and (FSplitter[1] = 'TIMEARRAYSERIES') then
          begin
            FSplitter.DelimitedText := CaseSensitiveLine;
            SURFACE.TimeArraySeries := FSplitter[2]
          end
          else
          begin
            Double2DDReader := TDouble2DArrayReader.Create(LocalDim, FPackageType);
            try
              Double2DDReader.Read(Stream, Unhandled);
              SURFACE.value := Double2DDReader.FData;
            finally
              Double2DDReader.Free;
            end;
          end;
        end
        else if FSplitter[0] = 'RATE' then
        begin
          if (FSplitter.Count >= 3) and (FSplitter[1] = 'TIMEARRAYSERIES') then
          begin
            FSplitter.DelimitedText := CaseSensitiveLine;
            RATE.TimeArraySeries := FSplitter[2]
          end
          else
          begin
            Double2DDReader := TDouble2DArrayReader.Create(LocalDim, FPackageType);
            try
              Double2DDReader.Read(Stream, Unhandled);
              RATE.value := Double2DDReader.FData;
            finally
              Double2DDReader.Free;
            end;
          end;
        end
        else if FSplitter[0] = 'DEPTH' then
        begin
          if (FSplitter.Count >= 3) and (FSplitter[1] = 'TIMEARRAYSERIES') then
          begin
            FSplitter.DelimitedText := CaseSensitiveLine;
            DEPTH.TimeArraySeries := FSplitter[2]
          end
          else
          begin
            Double2DDReader := TDouble2DArrayReader.Create(LocalDim, FPackageType);
            try
              Double2DDReader.Read(Stream, Unhandled);
              DEPTH.value := Double2DDReader.FData;
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
          AuxList.Add(AuxArray)
        end
      end;
    finally
      for RowIndex := 0 to LocalDim.NRow - 1 do
      begin
        for ColIndex := 0 to LocalDim.NCol - 1 do
        begin
          Cell := TEvtTimeItem.Create;
          if IEVT = nil then
          begin
            Cell.cellid.Layer := 1;
          end
          else
          begin
            Cell.cellid.Layer := IEVT[0, RowIndex, ColIndex];
          end;
          Cell.cellid.Row := RowIndex + 1;
          Cell.cellid.Column := ColIndex + 1;
          if SURFACE.Value <> nil then
          begin
            Cell.surf.NumericValue := SURFACE.Value[RowIndex, ColIndex];
            Cell.surf.ValueType := vtNumeric;
          end
          else
          begin
            Cell.surf.ValueType := vtString;
            Cell.surf.StringValue := SURFACE.TimeArraySeries + '_'
              + IntToStr(RowIndex + 1) + '_' + IntToStr(ColIndex + 1)
          end;
          if RATE.Value <> nil then
          begin
            Cell.rate.NumericValue := RATE.Value[RowIndex, ColIndex];
            Cell.rate.ValueType := vtNumeric;
          end
          else
          begin
            Cell.rate.ValueType := vtString;
            Cell.rate.StringValue := RATE.TimeArraySeries + '_'
              + IntToStr(RowIndex + 1) + '_' + IntToStr(ColIndex + 1)
          end;
          if DEPTH.Value <> nil then
          begin
            Cell.depth.NumericValue := DEPTH.Value[RowIndex, ColIndex];
            Cell.depth.ValueType := vtNumeric;
          end
          else
          begin
            Cell.depth.ValueType := vtString;
            Cell.depth.StringValue := DEPTH.TimeArraySeries + '_'
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

      AuxList.Free;
    end;
  end
  else
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

      if ReadEndOfSection(ALine, ErrorLine, 'PERIOD', Unhandled) then
      begin
        Exit;
      end;

      Cell := TEvtTimeItem.Create;
      try
        CaseSensitiveLine := ALine;
        ALine := UpperCase(ALine);
        FSplitter.DelimitedText := ALine;
        if FSplitter.Count >= NumberOfColumns then
        begin
          if ReadCellID(Cell.CellId, 0, DimensionCount) then
          begin
            if TryFortranStrToFloat(FSplitter[DimensionCount], Cell.surf.NumericValue) then
            begin
              Cell.surf.ValueType := vtNumeric;
            end
            else
            begin
              Cell.surf.ValueType := vtString;
              FSplitter.DelimitedText := CaseSensitiveLine;
              Cell.surf.StringValue := FSplitter[DimensionCount];
            end;
            StartIndex := DimensionCount + 1;

            if TryFortranStrToFloat(FSplitter[StartIndex], Cell.rate.NumericValue) then
            begin
              Cell.rate.ValueType := vtNumeric;
            end
            else
            begin
              Cell.rate.ValueType := vtString;
              FSplitter.DelimitedText := CaseSensitiveLine;
              Cell.rate.StringValue := FSplitter[StartIndex];
            end;
            Inc(StartIndex);

            if TryFortranStrToFloat(FSplitter[StartIndex], Cell.depth.NumericValue) then
            begin
              Cell.rate.ValueType := vtNumeric;
            end
            else
            begin
              Cell.rate.ValueType := vtString;
              FSplitter.DelimitedText := CaseSensitiveLine;
              Cell.rate.StringValue := FSplitter[StartIndex];
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
              Cell.pxdp.Add(ABoundValue);
              Inc(StartIndex);

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
              Cell.petm.Add(ABoundValue);
              Inc(StartIndex);
            end;

            if SURF_RATE_SPECIFIED then
            begin
              if TryFortranStrToFloat(FSplitter[StartIndex], Cell.petm0.NumericValue) then
              begin
                Cell.petm0.ValueType := vtNumeric;
              end
              else
              begin
                Cell.petm0.ValueType := vtString;
                FSplitter.DelimitedText := CaseSensitiveLine;
                Cell.petm0.StringValue := FSplitter[StartIndex];
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
              Cell.aux.Add(Aux);
            end;
            if BOUNDNAMES and (FSplitter.Count >= NumberOfColumns+1) then
            begin
              Cell.boundname := FSplitter[StartIndex];
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

procedure TEvt.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
        FEvtDimensions.Read(Stream, Unhandled);
      end
      else if (FSplitter[1] ='PERIOD') and (FSplitter.Count >= 3) then
      begin
        if TryStrToInt(FSplitter[2], IPER) then
        begin
          APeriod := TEvtPeriod.Create(FPackageType);
          FPeriods.Add(APeriod);
          APeriod.IPer := IPER;
          APeriod.Read(Stream, Unhandled, FDimensions, FOptions.AUXILIARY.Count,
            FOptions.BOUNDNAMES, FOptions.READASARRAYS, FEvtDimensions.NSEG,
            FOptions.SURF_RATE_SPECIFIED);
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
    TsPackage.ReadPackage(Unhandled);
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
    TasPackage.ReadPackage(Unhandled);
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
    ObsPackage.ReadPackage(Unhandled);
  end;
end;

end.
