unit CSubFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TCSubOptions = class(TCustomMf6Persistent)
  private
    BOUNDNAMES: Boolean;
    PRINT_INPUT: Boolean;
    SAVE_FLOWS: Boolean;
    GAMMAW: TRealOption;
    BETA: TRealOption;
    HEAD_BASED: Boolean;
    INITIAL_PRECONSOLIDATION_HEAD: Boolean;
    NDELAYCELLS: TIntegerOption;
    COMPRESSION_INDICES: Boolean;
    UPDATE_MATERIAL_PROPERTIES: Boolean;
    CELL_FRACTION: Boolean;
    SPECIFIED_INITIAL_INTERBED_STATE: Boolean;
    SPECIFIED_INITIAL_PRECONSOLIDATION_STRESS: Boolean;
    SPECIFIED_INITIAL_DELAY_HEAD: Boolean;
    EFFECTIVE_STRESS_LAG: Boolean;
    STRAIN_CSV_INTERBED: Boolean;
    STRAIN_CSV_COARSE: Boolean;
    COMPACTION: Boolean;
    COMPACTION_ELASTIC: Boolean;
    COMPACTION_INELASTIC: Boolean;
    COMPACTION_INTERBED: Boolean;
    COMPACTION_COARSE: Boolean;
    ZDISPLACEMENT: Boolean;
    PACKAGE_CONVERGENCE: Boolean;
    TS6_FileNames: TStringList;
    Obs6_FileNames: TStringList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TCSubDimensions = class(TCustomMf6Persistent)
  private
    NINTERBEDS: Integer;
    MAXSIG0: TIntegerOption;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TCSubGridData = class(TCustomMf6Persistent)
  private
    CG_SKE_CR: TDArray3D;
    CG_THETA: TDArray3D;
    SGM: TDArray3D;
    SGS: TDArray3D;
    FDimensions: TDimensions;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
  end;

  TCSubItem = record
    icsubno: Integer;
    cellid: TCellId;
    cdelay: string;
    pcs0: Extended;
    thick_frac: Extended;
    rnb: Extended;
    ssv_cc: Extended;
    sse_cr: Extended;
    theta: Extended;
    kv: Extended;
    h0: Extended;
    boundname: TStringOption;
    procedure Initialize;
  end;

  TCSubItemList = TList<TCSubItem>;

  TCSubPackageData = class(TCustomMf6Persistent)
  private
    FItems: TCSubItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;


  TCSubTimeItem = record
    cellid: TCellId;
    ValueType: TValueType;
    sig0: Extended;
    StringValue: string;
    procedure Initialize;
  end;

  TCSubTimeItemList = TList<TCSubTimeItem>;

  TCSubPeriod = class(TCustomMf6Persistent)
  private
    IPer: Integer;
    FCells: TCSubTimeItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TCSubPeriodList = TObjectList<TCSubPeriod>;

  TCSub = class(TDimensionedPackageReader)
  private
    FOptions: TCSubOptions;
    FCSubDimensions: TCSubDimensions;
    FGridData: TCSubGridData;
    FPackageData: TCSubPackageData;
    FPeriods: TCSubPeriodList;
    FTimeSeriesPackages: TPackageList;
    FObservationsPackages: TPackageList;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
  end;

implementation

uses
  ModelMuseUtilities, TimeSeriesFileReaderUnit, ObsFileReaderUnit;

{ TCSubOptions }

constructor TCSubOptions.Create(PackageType: string);
begin
  TS6_FileNames := TStringList.Create;
  Obs6_FileNames := TStringList.Create;
  inherited;
end;

destructor TCSubOptions.Destroy;
begin
  TS6_FileNames.Free;
  Obs6_FileNames.Free;
  inherited;
end;

procedure TCSubOptions.Initialize;
begin
  inherited;
  TS6_FileNames.Clear;
  Obs6_FileNames.Clear;

  BOUNDNAMES := False;
  PRINT_INPUT := False;
  SAVE_FLOWS := False;
  GAMMAW.Initialize;
  BETA.Initialize;
  HEAD_BASED := False;
  INITIAL_PRECONSOLIDATION_HEAD := False;
  NDELAYCELLS.Initialize;
  COMPRESSION_INDICES := False;
  UPDATE_MATERIAL_PROPERTIES := False;
  CELL_FRACTION := False;
  SPECIFIED_INITIAL_INTERBED_STATE := False;
  SPECIFIED_INITIAL_PRECONSOLIDATION_STRESS := False;
  SPECIFIED_INITIAL_DELAY_HEAD := False;
  EFFECTIVE_STRESS_LAG := False;
  STRAIN_CSV_INTERBED := False;
  STRAIN_CSV_COARSE := False;
  COMPACTION := False;
  COMPACTION_ELASTIC := False;
  COMPACTION_INELASTIC := False;
  COMPACTION_INTERBED := False;
  COMPACTION_COARSE := False;
  ZDISPLACEMENT := False;
  PACKAGE_CONVERGENCE := False;
end;

procedure TCSubOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
  TS6_FileName: string;
  Obs_FileName: string;
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
    if FSplitter[0] = 'BOUNDNAMES' then
    begin
      BOUNDNAMES := True;
    end
    else if FSplitter[0] = 'PRINT_INPUT' then
    begin
      PRINT_INPUT := True;
    end
    else if FSplitter[0] = 'SAVE_FLOWS' then
    begin
      SAVE_FLOWS := True;
    end
    else if (FSplitter[0] = 'GAMMAW') and (FSplitter.Count >= 2)
      and TryFortranStrToFloat(FSplitter[1], GAMMAW.Value) then
    begin
      GAMMAW.Used := True;
    end
    else if (FSplitter[0] = 'BETA') and (FSplitter.Count >= 2)
      and TryFortranStrToFloat(FSplitter[1], BETA.Value) then
    begin
      BETA.Used := True;
    end
    else if FSplitter[0] = 'HEAD_BASED' then
    begin
      HEAD_BASED := True;
    end
    else if FSplitter[0] = 'INITIAL_PRECONSOLIDATION_HEAD' then
    begin
      INITIAL_PRECONSOLIDATION_HEAD := True;
    end
    else if (FSplitter[0] = 'NDELAYCELLS') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], NDELAYCELLS.Value) then
    begin
      NDELAYCELLS.Used := True;
    end
    else if FSplitter[0] = 'COMPRESSION_INDICES' then
    begin
      COMPRESSION_INDICES := True;
    end
    else if FSplitter[0] = 'UPDATE_MATERIAL_PROPERTIES' then
    begin
      UPDATE_MATERIAL_PROPERTIES := True;
    end
    else if FSplitter[0] = 'CELL_FRACTION' then
    begin
      CELL_FRACTION := True;
    end
    else if FSplitter[0] = 'SPECIFIED_INITIAL_INTERBED_STATE' then
    begin
      SPECIFIED_INITIAL_INTERBED_STATE := True;
    end
    else if FSplitter[0] = 'SPECIFIED_INITIAL_PRECONSOLIDATION_STRESS' then
    begin
      SPECIFIED_INITIAL_PRECONSOLIDATION_STRESS := True;
    end
    else if FSplitter[0] = 'SPECIFIED_INITIAL_DELAY_HEAD' then
    begin
      SPECIFIED_INITIAL_DELAY_HEAD := True;
    end
    else if FSplitter[0] = 'EFFECTIVE_STRESS_LAG' then
    begin
      EFFECTIVE_STRESS_LAG := True;
    end
    else if FSplitter[0] = 'STRAIN_CSV_INTERBED' then
    begin
      STRAIN_CSV_INTERBED := True;
    end
    else if FSplitter[0] = 'STRAIN_CSV_COARSE' then
    begin
      STRAIN_CSV_COARSE := True;
    end
    else if FSplitter[0] = 'COMPACTION' then
    begin
      COMPACTION := True;
    end
    else if FSplitter[0] = 'COMPACTION_ELASTIC' then
    begin
      COMPACTION_ELASTIC := True;
    end
    else if FSplitter[0] = 'COMPACTION_INELASTIC' then
    begin
      COMPACTION_INELASTIC := True;
    end
    else if FSplitter[0] = 'COMPACTION_INTERBED' then
    begin
      COMPACTION_INTERBED := True;
    end
    else if FSplitter[0] = 'COMPACTION_INTERBED' then
    begin
      COMPACTION_INTERBED := True;
    end
    else if FSplitter[0] = 'COMPACTION_COARSE' then
    begin
      COMPACTION_COARSE := True;
    end
    else if FSplitter[0] = 'ZDISPLACEMENT' then
    begin
      ZDISPLACEMENT := True;
    end
    else if FSplitter[0] = 'PACKAGE_CONVERGENCE' then
    begin
      PACKAGE_CONVERGENCE := True;
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
  end
end;

{ TCSubDimensions }

procedure TCSubDimensions.Initialize;
begin
  inherited;
  NINTERBEDS := 0;
  MAXSIG0.Initialize;
end;

procedure TCSubDimensions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    if (FSplitter[0] = 'NINTERBEDS') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], NINTERBEDS) then
    begin
    end
    else if (FSplitter[0] = 'MAXSIG0') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], MAXSIG0.Value) then
    begin
      MAXSIG0.Used := True;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TCSubGridData }

constructor TCSubGridData.Create(PackageType: string);
begin
  FDimensions.Initialize;
  inherited;

end;

procedure TCSubGridData.Initialize;
begin
  SetLength(CG_SKE_CR, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
  SetLength(CG_THETA, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
  SetLength(SGM, 0);
  SetLength(SGS, 0);
  inherited;
end;

procedure TCSubGridData.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  Dimensions: TDimensions);
var
  ALine: string;
  ErrorLine: string;
  Layered: Boolean;
  DoubleThreeDReader: TDouble3DArrayReader;
begin
  FDimensions := Dimensions;
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

    if ReadEndOfSection(ALine, ErrorLine, 'GRIDDATA', Unhandled) then
    begin
      Exit;
    end;

    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    if FSplitter[0] = 'CG_SKE_CR' then
    begin
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        CG_SKE_CR := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'CG_THETA' then
    begin
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        CG_THETA := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'SGM' then
    begin
      SetLength(SGM, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        SGM := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'SGS' then
    begin
      SetLength(SGS, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        SGS := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else
    begin
      Unhandled.WriteLine('Unrecognized CSUB GRIDDATA in the following line');
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TCSubItem }

procedure TCSubItem.Initialize;
begin
  icsubno := 0;
  cellid.Initialize;
  cdelay := '';
  pcs0 := 0;
  thick_frac := 0;
  rnb := 0;
  ssv_cc := 0;
  sse_cr := 0;
  theta := 0;
  kv := 0;
  h0 := 0;
  boundname.Initialize;
end;

{ TCSubPackageData }

constructor TCSubPackageData.Create(PackageType: string);
begin
  FItems := TCSubItemList.Create;
  inherited;

end;

destructor TCSubPackageData.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TCSubPackageData.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TCSubPackageData.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  Dimensions: TDimensions);
var
  ALine: string;
  ErrorLine: string;
  DimensionCount: Integer;
  Item: TCSubItem;
  ItemStart: Integer;
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

    if ReadEndOfSection(ALine, ErrorLine, 'PACKAGEDATA', Unhandled) then
    begin
      Exit;
    end;

    Item.Initialize;

    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    if TryStrToInt(FSplitter[0],Item.icsubno) and (FSplitter.Count >= 10 + DimensionCount)  then
    begin
      if ReadCellID(Item.cellid, 1, DimensionCount) then
      begin
        ItemStart := DimensionCount + 1;
        Item.cdelay := FSplitter[ItemStart];
        Inc(ItemStart);
        if TryFortranStrToFloat(FSplitter[ItemStart], Item.pcs0) then
        begin
          Inc(ItemStart);
          if TryFortranStrToFloat(FSplitter[ItemStart], Item.thick_frac) then
          begin
            Inc(ItemStart);
            if TryFortranStrToFloat(FSplitter[ItemStart], Item.rnb) then
            begin
              Inc(ItemStart);
              if TryFortranStrToFloat(FSplitter[ItemStart], Item.ssv_cc) then
              begin
                Inc(ItemStart);
                if TryFortranStrToFloat(FSplitter[ItemStart], Item.sse_cr) then
                begin
                  Inc(ItemStart);
                  if TryFortranStrToFloat(FSplitter[ItemStart], Item.theta) then
                  begin
                    Inc(ItemStart);
                    if TryFortranStrToFloat(FSplitter[ItemStart], Item.kv) then
                    begin
                      Inc(ItemStart);
                      if TryFortranStrToFloat(FSplitter[ItemStart], Item.h0) then
                      begin
                        Inc(ItemStart);
                        if FSplitter.Count > ItemStart then
                        begin
                          Item.boundname.Value := FSplitter[ItemStart];
                          Item.boundname.Used := True;
                          FItems.Add(Item);
                        end;
                      end
                      else
                      begin
                        Unhandled.WriteLine(Format(StrUnrecognizedSPACK, [FPackageType]));
                        Unhandled.WriteLine(ErrorLine);
                      end;
                    end
                    else
                    begin
                      Unhandled.WriteLine(Format(StrUnrecognizedSPACK, [FPackageType]));
                      Unhandled.WriteLine(ErrorLine);
                    end;
                  end
                  else
                  begin
                    Unhandled.WriteLine(Format(StrUnrecognizedSPACK, [FPackageType]));
                    Unhandled.WriteLine(ErrorLine);
                  end;
                end
                else
                begin
                  Unhandled.WriteLine(Format(StrUnrecognizedSPACK, [FPackageType]));
                  Unhandled.WriteLine(ErrorLine);
                end;
              end
              else
              begin
                Unhandled.WriteLine(Format(StrUnrecognizedSPACK, [FPackageType]));
                Unhandled.WriteLine(ErrorLine);
              end;
            end
            else
            begin
              Unhandled.WriteLine(Format(StrUnrecognizedSPACK, [FPackageType]));
              Unhandled.WriteLine(ErrorLine);
            end;
          end
          else
          begin
            Unhandled.WriteLine(Format(StrUnrecognizedSPACK, [FPackageType]));
            Unhandled.WriteLine(ErrorLine);
          end;
        end
        else
        begin
          Unhandled.WriteLine(Format(StrUnrecognizedSPACK, [FPackageType]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSPACK, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSPACK, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TCSubPeriodData }

constructor TCSubPeriod.Create(PackageType: string);
begin
  FCells := TCSubTimeItemList.Create;
  inherited;
end;

destructor TCSubPeriod.Destroy;
begin
  FCells.Free;
  inherited;
end;

procedure TCSubPeriod.Initialize;
begin
  inherited;
  FCells.Clear;
end;

procedure TCSubPeriod.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  Dimensions: TDimensions);
var
  DimensionCount: Integer;
  Cell: TCSubTimeItem;
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
    if FSplitter.Count >= DimensionCount + 1 then
    begin
      if ReadCellID(Cell.CellId, 0, DimensionCount) then
      begin
        if TryFortranStrToFloat(FSplitter[DimensionCount], Cell.sig0) then
        begin
          Cell.ValueType := vtNumeric;
        end
        else
        begin
          Cell.ValueType := vtString;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Cell.StringValue := FSplitter[DimensionCount];
        end;
        FCells.Add(Cell);
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

{ TCSubTimeItem }

procedure TCSubTimeItem.Initialize;
begin
  cellid.Initialize;
  sig0 := 0;
end;

{ TCSub }

constructor TCSub.Create(PackageType: string);
begin
  FOptions := TCSubOptions.Create(PackageType);
  FCSubDimensions := TCSubDimensions.Create(PackageType);
  FGridData := TCSubGridData.Create(PackageType);
  FPackageData := TCSubPackageData.Create(PackageType);
  FPeriods := TCSubPeriodList.Create;
  FTimeSeriesPackages := TPackageList.Create;
  FObservationsPackages := TPackageList.Create;
  inherited;
end;

destructor TCSub.Destroy;
begin
  FOptions.Free;
  FCSubDimensions.Free;
  FGridData.Free;
  FPackageData.Free;
  FPeriods.Free;
  FTimeSeriesPackages.Free;
  FObservationsPackages.Free;
  inherited;
end;

procedure TCSub.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  IPER: Integer;
  APeriod: TCSubPeriod;
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
        FCSubDimensions.Read(Stream, Unhandled);
      end
      else if FSplitter[1] ='GRIDDATA' then
      begin
        FGridData.Read(Stream, Unhandled, FDimensions);
      end
      else if FSplitter[1] ='PACKAGEDATA' then
      begin
        FPackageData.Read(Stream, Unhandled, FDimensions);
      end
      else if (FSplitter[1] ='PERIOD') and (FSplitter.Count >= 3) then
      begin
        if TryStrToInt(FSplitter[2], IPER) then
        begin
          APeriod := TCSubPeriod.Create(FPackageType);
          FPeriods.Add(APeriod);
          APeriod.IPer := IPER;
          APeriod.Read(Stream, Unhandled, FDimensions);
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
