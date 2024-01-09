unit Mf6.RchFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TRchOptions = class(TCustomMf6Persistent)
  private
    FIXED_CELL: Boolean;
    AUXILIARY: TStringList;
    AUXMULTNAME: string;
    BOUNDNAMES: Boolean;
    PRINT_INPUT: Boolean;
    PRINT_FLOWS: Boolean;
    SAVE_FLOWS: Boolean;
    TS6_FileNames: TStringList;
    TAS6_FileNames: TStringList;
    Obs6_FileNames: TStringList;
    READASARRAYS: Boolean;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TRchDimensions = class(TCustomMf6Persistent)
  private
    MAXBOUND: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TRchTimeItem = class(TObject)
    cellid: TCellId;
    recharge: TMf6BoundaryValue;
    aux: TList<TMf6BoundaryValue>;
    boundname: string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TRchTimeItemList = TObjectList<TRchTimeItem>;

  TRchPeriod = class(TCustomMf6Persistent)
  private
    IPer: Integer;
    FCells: TRchTimeItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      Dimensions: TDimensions; naux: Integer; BOUNDNAMES: Boolean; READASARRAYS: Boolean);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
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
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
  end;


implementation

uses
  ModelMuseUtilities, Mf6.TimeSeriesFileReaderUnit, Mf6.ObsFileReaderUnit,
  Mf6.TimeArraySeriesFileReaderUnit;

{ TRchOptions }

constructor TRchOptions.Create(PackageType: string);
begin
  AUXILIARY := TStringList.Create;
  AUXILIARY.CaseSensitive := False;
  TS6_FileNames := TStringList.Create;
  TAS6_FileNames := TStringList.Create;
  Obs6_FileNames := TStringList.Create;
  inherited;

end;

destructor TRchOptions.Destroy;
begin
  AUXILIARY.Free;
  TS6_FileNames.Free;
  TAS6_FileNames.Free;
  Obs6_FileNames.Free;
  inherited;
end;

procedure TRchOptions.Initialize;
begin
  inherited;
  FIXED_CELL := False;
  AUXILIARY.Clear;
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
    else if (FSplitter[0] = 'AUXILIARY')
      and (FSplitter.Count >= 2) then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      for AuxIndex := 1 to FSplitter.Count - 1 do
      begin
        AUXILIARY_Name := FSplitter[AuxIndex];
        AUXILIARY.Add(AUXILIARY_Name);
      end;
    end
    else if (FSplitter[0] = 'AUXMULTNAME')
      and (FSplitter.Count >= 2) then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      AUXMULTNAME := FSplitter[1];
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
  cellid.Initialize;
  recharge.Initialize;
  aux := TList<TMf6BoundaryValue>.Create;
  boundname := '';
end;

destructor TRchTimeItem.Destroy;
begin
  aux.Free;
  inherited;
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
  inherited;
end;

destructor TRchPeriod.Destroy;
begin
  FCells.Free;
  inherited;
end;

procedure TRchPeriod.Initialize;
begin
  inherited;
  FCells.Clear;
end;

procedure TRchPeriod.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  Dimensions: TDimensions; naux: Integer; BOUNDNAMES: Boolean; READASARRAYS: Boolean);
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
  IRCH: TIArray3D;
  Double2DDReader: TDouble2DArrayReader;
  RECHARGE: TArrayItem;
  AuxArray: TArrayItem;
  AuxList: TArrayItemList;
  RowIndex: Integer;
  ColIndex: Integer;
  NumberOfColumns: Integer;
begin
  DimensionCount := Dimensions.DimensionCount;
  Initialize;
  NumberOfColumns := DimensionCount + 1 + naux;
  if READASARRAYS then
  begin
    LocalDim := Dimensions;
    LocalDim.NLay := 1;
    RECHARGE.Initialize;
    AuxList := TArrayItemList.Create;
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

        IRCH := nil;
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
          AuxList.Add(AuxArray)
        end
      end;
    finally
      for RowIndex := 0 to LocalDim.NRow - 1 do
      begin
        for ColIndex := 0 to LocalDim.NCol - 1 do
        begin
          Cell := TRchTimeItem.Create;
          if IRCH = nil then
          begin
            Cell.cellid.Layer := 1;
          end
          else
          begin
            Cell.cellid.Layer := IRCH[0, RowIndex, ColIndex];
          end;
          Cell.cellid.Row := RowIndex + 1;
          Cell.cellid.Column := ColIndex + 1;
          if RECHARGE.Value <> nil then
          begin
            Cell.recharge.NumericValue := RECHARGE.Value[RowIndex, ColIndex];
            Cell.recharge.ValueType := vtNumeric;
          end
          else
          begin
            Cell.recharge.ValueType := vtString;
            Cell.recharge.StringValue := RECHARGE.TimeArraySeries + '_'
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
        CaseSensitiveLine := ALine;
        if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'PERIOD') then
        begin
          // do nothing
        end
        else if FSplitter.Count >= NumberOfColumns then
        begin
          if ReadCellID(Cell.CellId, 0, DimensionCount) then
          begin
            if TryFortranStrToFloat(FSplitter[DimensionCount], Cell.recharge.NumericValue) then
            begin
              Cell.recharge.ValueType := vtNumeric;
            end
            else
            begin
              Cell.recharge.ValueType := vtString;
              FSplitter.DelimitedText := CaseSensitiveLine;
              Cell.recharge.StringValue := FSplitter[DimensionCount];
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

procedure TRch.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
        FRchDimensions.Read(Stream, Unhandled);
      end
      else if (FSplitter[1] ='PERIOD') and (FSplitter.Count >= 3) then
      begin
        if TryStrToInt(FSplitter[2], IPER) then
        begin
          APeriod := TRchPeriod.Create(FPackageType);
          FPeriods.Add(APeriod);
          APeriod.IPer := IPER;
          APeriod.Read(Stream, Unhandled, FDimensions, FOptions.AUXILIARY.Count,
            FOptions.BOUNDNAMES, FOptions.READASARRAYS);
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
