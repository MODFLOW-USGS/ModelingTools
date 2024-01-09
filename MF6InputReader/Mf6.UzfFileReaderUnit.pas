unit Mf6.UzfFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections, System.Generics.Defaults;

type
  TUzfOptions = class(TCustomMf6Persistent)
  private
    AUXILIARY: TStringList;
    AUXMULTNAME: string;
    BOUNDNAMES: Boolean;
    PRINT_INPUT: Boolean;
    PRINT_FLOWS: Boolean;
    SAVE_FLOWS: Boolean;
    WATER_CONTENT: Boolean;
    BUDGET: Boolean;
    BUDGETCSV: Boolean;
    PACKAGE_CONVERGENCE: Boolean;
    TS6: TStringList;
    OBS6: TStringList;
    MOVER: Boolean;
    SIMULATE_ET: Boolean;
    LINEAR_GWET: Boolean;
    SQUARE_GWET: Boolean;
    SIMULATE_GWSEEP: Boolean;
    UNSAT_ETWC: Boolean;
    UNSAT_ETAE: Boolean;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TUzfDimensions = class(TCustomMf6Persistent)
  private
    NUZFCELLS: Integer;
    NTRAILWAVES: Integer;
    NWAVESETS: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TUzfPackageItem = record
  private
    iuzno: Integer;
    cellid: TCellId;
    landflag: Integer;
    ivertcon: Integer;
    surfdep: Extended;
    vks: Extended;
    thtr: Extended;
    thts: Extended;
    thti: Extended;
    eps: Extended;
    boundname: string;
    procedure Initialize;
  end;

  TUzfPackageItemList= TList<TUzfPackageItem>;

  TUzfPackageData = class(TCustomMf6Persistent)
  private
    FItems: TUzfPackageItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      BOUNDNAMES: Boolean; Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TUzfPeriodItem = class(TCustomMf6Persistent)
  private
    iuzno: Integer;
    finf: TMf6BoundaryValue;
    pet: TMf6BoundaryValue;
    extdp: TMf6BoundaryValue;
    extwc: TMf6BoundaryValue;
    ha: TMf6BoundaryValue;
    hroot: TMf6BoundaryValue;
    rootact: TMf6BoundaryValue;
    aux: TBoundaryValueList;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TUzfPeriodItemList = TObjectList<TUzfPeriodItem>;

  TUzfPeriod = class(TCustomMf6Persistent)
  private
    IPER: Integer;
    FItems: TUzfPeriodItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; Naux: Integer);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TUzfPeriodList = TObjectList<TUzfPeriod>;

  TUzf = class(TDimensionedPackageReader)
    FOptions: TUzfOptions;
    FuzfDimensions: TUzfDimensions;
    FPackageData: TUzfPackageData;
    FPeriods: TUzfPeriodList;
    FTimeSeriesPackages: TPackageList;
    FObservationsPackages: TPackageList;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
  end;

implementation

uses
  ModelMuseUtilities, Mf6.TimeSeriesFileReaderUnit, Mf6.ObsFileReaderUnit;

{ TUzfOptions }

constructor TUzfOptions.Create(PackageType: string);
begin
  AUXILIARY := TStringList.Create;
  AUXILIARY.CaseSensitive := False;
  TS6 := TStringList.Create;
  OBS6 := TStringList.Create;
  inherited;

end;

destructor TUzfOptions.Destroy;
begin
  AUXILIARY.Free;
  TS6.Free;
  OBS6.Free;
  inherited;
end;

procedure TUzfOptions.Initialize;
begin
  inherited;
  AUXILIARY.Clear;
  AUXMULTNAME := '';
  BOUNDNAMES := False;
  PRINT_INPUT := False;
  PRINT_FLOWS := False;
  SAVE_FLOWS := False;
  WATER_CONTENT := False;
  BUDGET := False;
  BUDGETCSV := False;
  TS6.Clear;
  OBS6.Clear;
  MOVER := False;
  SIMULATE_ET := False;
  LINEAR_GWET := False;
  SQUARE_GWET := False;
  SIMULATE_GWSEEP := False;
  UNSAT_ETWC := False;
  UNSAT_ETAE := False;
end;

procedure TUzfOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
  TS6_FileName: string;
  Obs_FileName: string;
  AuxIndex: Integer;
  AUXILIARY_Name: string;
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
    else if (FSplitter[0] = 'WATER_CONTENT')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEOUT') then
    begin
      WATER_CONTENT := True;
    end
    else if (FSplitter[0] = 'BUDGET')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEOUT') then
    begin
      BUDGET := True;
    end
    else if (FSplitter[0] = 'BUDGETCSV')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEOUT') then
    begin
      BUDGETCSV := True;
    end
    else if (FSplitter[0] = 'PACKAGE_CONVERGENCE')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEOUT') then
    begin
      PACKAGE_CONVERGENCE := True;
    end
    else if (FSplitter[0] = 'TS6')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEIN') then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      TS6_FileName := FSplitter[2];
      TS6.Add(TS6_FileName);
    end
    else if (FSplitter[0] = 'OBS6')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEIN') then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      Obs_FileName := FSplitter[2];
      Obs6.Add(Obs_FileName);
    end
    else if FSplitter[0] = 'MOVER' then
    begin
      MOVER := True;
    end
    else if FSplitter[0] = 'SIMULATE_ET' then
    begin
      SIMULATE_ET := True;
    end
    else if FSplitter[0] = 'LINEAR_GWET' then
    begin
      LINEAR_GWET := True;
    end
    else if FSplitter[0] = 'SQUARE_GWET' then
    begin
      SQUARE_GWET := True;
    end
    else if FSplitter[0] = 'SIMULATE_GWSEEP' then
    begin
      SIMULATE_GWSEEP := True;
    end
    else if FSplitter[0] = 'UNSAT_ETWC' then
    begin
      UNSAT_ETWC := True;
    end
    else if FSplitter[0] = 'UNSAT_ETAE' then
    begin
      UNSAT_ETAE := True;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TUzfDimensions }

procedure TUzfDimensions.Initialize;
begin
  inherited;
  NUZFCELLS := 0;
  NTRAILWAVES := 0;
  NWAVESETS := 0;

end;

procedure TUzfDimensions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    else if (FSplitter[0] = 'NUZFCELLS') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], NUZFCELLS) then
    begin
    end
    else if (FSplitter[0] = 'NTRAILWAVES') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], NTRAILWAVES) then
    begin
    end
    else if (FSplitter[0] = 'NWAVESETS') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], NWAVESETS) then
    begin
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TUzfPackageItem }

procedure TUzfPackageItem.Initialize;
begin
  iuzno := 0;
  cellid.Initialize;
  landflag := 0;
  ivertcon := 0;
  surfdep := 0;
  vks := 0;
  thtr := 0;
  thts := 0;
  thti := 0;
  eps := 0;
  boundname := '';
end;

{ TUzfPackageData }

constructor TUzfPackageData.Create(PackageType: string);
begin
  FItems := TUzfPackageItemList.Create;
  inherited;

end;

destructor TUzfPackageData.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TUzfPackageData.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TUzfPackageData.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  BOUNDNAMES: Boolean; Dimensions: TDimensions);
var
  ALine: string;
  ErrorLine: string;
  Item: TUzfPackageItem;
  ItemStart: Integer;
  CaseSensitiveLine: string;
  NumberOfItems: Integer;
  DimensionCount: Integer;
begin
  DimensionCount := Dimensions.DimensionCount;
  NumberOfItems := 9 + DimensionCount;
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

    if ReadEndOfSection(ALine, ErrorLine, 'PACKAGEDATA', Unhandled) then
    begin
      FItems.Sort(
        TComparer<TUzfPackageItem>.Construct(
          function(const Left, Right: TUzfPackageItem): Integer
          begin
            Result := Left.iuzno - Right.iuzno;
          end
        ));
      Exit;
    end;

    CaseSensitiveLine := ALine;
    Item.Initialize;
    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'PACKAGEDATA') then
    begin
      // do nothing
    end
    else if (FSplitter.Count >= NumberOfItems)
      and TryStrToInt(FSplitter[0],Item.iuzno)
      and ReadCellID(Item.cellid, 1, DimensionCount)
      and TryStrToInt(FSplitter[1+DimensionCount],Item.landflag)
      and TryStrToInt(FSplitter[2+DimensionCount],Item.ivertcon)
      and TryFortranStrToFloat(FSplitter[3+DimensionCount],Item.surfdep)
      and TryFortranStrToFloat(FSplitter[4+DimensionCount],Item.vks)
      and TryFortranStrToFloat(FSplitter[5+DimensionCount],Item.thtr)
      and TryFortranStrToFloat(FSplitter[6+DimensionCount],Item.thts)
      and TryFortranStrToFloat(FSplitter[7+DimensionCount],Item.thti)
      and TryFortranStrToFloat(FSplitter[8+DimensionCount],Item.eps)
      then
    begin
      ItemStart := 9+DimensionCount;
      if BOUNDNAMES and (FSplitter.Count >= NumberOfItems+1) then
      begin
        FSplitter.DelimitedText := CaseSensitiveLine;
        Item.boundname := FSplitter[ItemStart];
      end;
      FItems.Add(Item);
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSPACK, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TUzfPeriod }

constructor TUzfPeriodItem.Create(PackageType: string);
begin
  aux := TBoundaryValueList.Create;
  inherited;
end;

destructor TUzfPeriodItem.Destroy;
begin
  aux.Free;
  inherited;
end;

procedure TUzfPeriodItem.Initialize;
begin
  inherited;
  iuzno := 0;
  finf.Initialize;
  pet.Initialize;
  extdp.Initialize;
  extwc.Initialize;
  ha.Initialize;
  hroot.Initialize;
  rootact.Initialize;
  aux.Clear;
end;

{ TUzfPeriod }

constructor TUzfPeriod.Create(PackageType: string);
begin
  FItems := TUzfPeriodItemList.Create;
  inherited;

end;

destructor TUzfPeriod.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TUzfPeriod.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TUzfPeriod.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  Naux: Integer);
var
  NumberOfItems: Integer;
  UzfItem: TUzfPeriodItem;
  ALine: string;
  ErrorLine: string;
  AuxIndex: Integer;
  StartIndex: Integer;
  AuxValue: TMf6BoundaryValue;
begin
  NumberOfItems := 8 + Naux;
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

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'PERIOD') then
    begin
      Continue;
    end;
    UzfItem := TUzfPeriodItem.Create(FPackageType);
    try
      if FSplitter.Count >= NumberOfItems then
      begin
        if TryStrToInt(FSplitter[0], UzfItem.iuzno) then
        begin
          if not TryFortranStrToFloat(FSplitter[1], UzfItem.finf.NumericValue) then
          begin
            UzfItem.finf.ValueType := vtString;
            UzfItem.finf.StringValue := FSplitter[1]
          end;
          if not TryFortranStrToFloat(FSplitter[2], UzfItem.pet.NumericValue) then
          begin
            UzfItem.pet.ValueType := vtString;
            UzfItem.pet.StringValue := FSplitter[2]
          end;
          if not TryFortranStrToFloat(FSplitter[3], UzfItem.extdp.NumericValue) then
          begin
            UzfItem.extdp.ValueType := vtString;
            UzfItem.extdp.StringValue := FSplitter[3]
          end;
          if not TryFortranStrToFloat(FSplitter[4], UzfItem.extwc.NumericValue) then
          begin
            UzfItem.extwc.ValueType := vtString;
            UzfItem.extwc.StringValue := FSplitter[4]
          end;
          if not TryFortranStrToFloat(FSplitter[5], UzfItem.ha.NumericValue) then
          begin
            UzfItem.ha.ValueType := vtString;
            UzfItem.ha.StringValue := FSplitter[5]
          end;
          if not TryFortranStrToFloat(FSplitter[6], UzfItem.hroot.NumericValue) then
          begin
            UzfItem.hroot.ValueType := vtString;
            UzfItem.hroot.StringValue := FSplitter[6]
          end;
          if not TryFortranStrToFloat(FSplitter[7], UzfItem.rootact.NumericValue) then
          begin
            UzfItem.rootact.ValueType := vtString;
            UzfItem.rootact.StringValue := FSplitter[7]
          end;
          StartIndex := 8;
          for AuxIndex := 0 to NAUX - 1 do
          begin
            AuxValue.Initialize;
            if not TryFortranStrToFloat(FSplitter[StartIndex], AuxValue.NumericValue) then
            begin
              AuxValue.ValueType := vtString;
              AuxValue.StringValue := FSplitter[StartIndex]
            end;
            UzfItem.aux.Add(AuxValue);
            Inc(StartIndex);
          end;
          FItems.Add(UzfItem);
          UzfItem := nil;
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
      UzfItem.Free;
    end;
  end;

end;

{ TUzf }

constructor TUzf.Create(PackageType: string);
begin
  inherited;
  FOptions := TUzfOptions.Create(PackageType);
  FuzfDimensions := TUzfDimensions.Create(PackageType);
  FPackageData := TUzfPackageData.Create(PackageType);
  FPeriods := TUzfPeriodList.Create;
  FTimeSeriesPackages := TPackageList.Create;
  FObservationsPackages := TPackageList.Create;

end;

destructor TUzf.Destroy;
begin
  FOptions.Free;
  FuzfDimensions.Free;
  FPackageData.Free;
  FPeriods.Free;
  FTimeSeriesPackages.Free;
  FObservationsPackages.Free;
  inherited;
end;

procedure TUzf.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  IPER: Integer;
  APeriod: TUzfPeriod;
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
        FUzfDimensions.Read(Stream, Unhandled);
      end
      else if FSplitter[1] ='PACKAGEDATA' then
      begin
        FPackageData.Read(Stream, Unhandled,
          FOptions.BOUNDNAMES, FDimensions);
      end
      else if (FSplitter[1] ='PERIOD') and (FSplitter.Count >= 3) then
      begin
        if TryStrToInt(FSplitter[2], IPER) then
        begin
          APeriod := TUzfPeriod.Create(FPackageType);
          FPeriods.Add(APeriod);
          APeriod.IPer := IPER;
          APeriod.Read(Stream, Unhandled, FOptions.AUXILIARY.Count);
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
  for PackageIndex := 0 to FOptions.TS6.Count - 1 do
  begin
    TsPackage := TPackage.Create;
    FTimeSeriesPackages.Add(TsPackage);
    TsPackage.FileType := FPackageType;
    TsPackage.FileName := FOptions.TS6[PackageIndex];
    TsPackage.PackageName := '';

    TsReader := TTimeSeries.Create(FPackageType);
    TsPackage.Package := TsReader;
    TsPackage.ReadPackage(Unhandled);
  end;
  for PackageIndex := 0 to FOptions.Obs6.Count - 1 do
  begin
    ObsPackage := TPackage.Create;
    FObservationsPackages.Add(ObsPackage);
    ObsPackage.FileType := FPackageType;
    ObsPackage.FileName := FOptions.Obs6[PackageIndex];
    ObsPackage.PackageName := '';

    ObsReader := TObs.Create(FPackageType);
    ObsReader.Dimensions := FDimensions;
    ObsPackage.Package := ObsReader;
    ObsPackage.ReadPackage(Unhandled);
  end;
end;

end.
