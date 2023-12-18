unit Mf6.LakFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TLakOptions = class(TCustomMf6Persistent)
  private
    AUXILIARY: TStringList;
    BOUNDNAMES: Boolean;
    PRINT_INPUT: Boolean;
    PRINT_STAGE: Boolean;
    PRINT_FLOWS: Boolean;
    SAVE_FLOWS: Boolean;
    STAGE: Boolean;
    BUDGET: Boolean;
    BUDGETCSV: Boolean;
    PACKAGE_CONVERGENCE: Boolean;
    TS6_FileNames: TStringList;
    Obs6_FileNames: TStringList;
    MOVER: Boolean;
    SURFDEP: TRealOption;
    MAXIMUM_ITERATIONS: TIntegerOption;
    MAXIMUM_STAGE_CHANGE: TRealOption;
    LENGTH_CONVERSION: TRealOption;
    TIME_CONVERSION: TRealOption;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TLakDimensions = class(TCustomMf6Persistent)
  private
    NLAKES: Integer;
    NOUTLETS: Integer;
    NTABLES: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TLakPackageItem = class(TObject)
  private
    lakeno: Integer;
    strt: Extended;
    nlakeconn: Integer;
    aux: TBoundaryValueList;
    boundname: string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TLakPackageItemList= TObjectList<TLakPackageItem>;

  TLakPackageData = class(TCustomMf6Persistent)
  private
    FItems: TLakPackageItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; naux: Integer;
      BOUNDNAMES: Boolean);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TLakConnectionItem = record
  private
    lakeno: Integer;
    iconn: Integer;
    cellid: TCellId;
    claktype: string;
    bedleak: string;
    belev: Extended;
    telev: Extended;
    connlen: Extended;
    connwidth: Extended;
    procedure Initialize;
  end;

  TLakConnectionItemList = TList<TLakConnectionItem>;

  TLakConnections = class(TCustomMf6Persistent)
  private
    FItems: TLakConnectionItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TLakeTableItem = record
    lakeno: Integer;
    tab6_filename: string;
    procedure Initialize;
  end;

  TLakeTableItemList = TList<TLakeTableItem>;

  TLakTables = class(TCustomMf6Persistent)
  private
    FItems: TLakeTableItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TLakOutletItem = record
    outletno: Integer;
    lakein: Integer;
    lakeout: Integer;
    couttype: string;
    invert: TBoundaryValue;
    width: TBoundaryValue;
    rough: TBoundaryValue;
    slope: TBoundaryValue;
    procedure Initialize;
  end;

  TLakOutletItemList = TList<TLakOutletItem>;

  TLakOutlets = class(TCustomMf6Persistent)
  private
    FItems: TLakOutletItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TLakPeriod = class(TCustomMf6Persistent)
  private
    IPER: Integer;
    FItems: TNumberedItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TSfrPeriodList = TObjectList<TLakPeriod>;

  TLak = class(TDimensionedPackageReader)
  private
    FOptions: TLakOptions;
    FLakDimensions: TLakDimensions;
    FPackageData: TLakPackageData;
    FLakTables: TLakTables;
    FConnections: TLakConnections;
    FLakOutlets: TLakOutlets;
    FPeriods: TSfrPeriodList;
    FTimeSeriesPackages: TPackageList;
    FObservationsPackages: TPackageList;
    FTabFilePackages: TPackageList;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
  end;


implementation

uses
  ModelMuseUtilities, System.Generics.Defaults, Mf6.TimeSeriesFileReaderUnit,
  Mf6.ObsFileReaderUnit, Mf6.LakeTableFileReaderUnit;

resourcestring
  StrUnrecognizedSCROS = 'Unrecognized %s CROSSSECTIONS data in the followin' +
  'g line';

{ TLakOptions }

constructor TLakOptions.Create(PackageType: string);
begin
  AUXILIARY := TStringList.Create;
  TS6_FileNames := TStringList.Create;
  Obs6_FileNames := TStringList.Create;
  inherited;

end;

destructor TLakOptions.Destroy;
begin
  AUXILIARY.Free;
  TS6_FileNames.Free;
  Obs6_FileNames.Free;
  inherited;
end;

procedure TLakOptions.Initialize;
begin
  inherited;
  AUXILIARY.Clear;
  BOUNDNAMES := False;
  PRINT_INPUT := False;
  PRINT_STAGE := False;
  PRINT_FLOWS := False;
  SAVE_FLOWS := False;
  STAGE := False;
  BUDGET := False;
  BUDGETCSV := False;
  PACKAGE_CONVERGENCE := False;
  TS6_FileNames.Clear;
  Obs6_FileNames.Clear;
  MOVER := False;
  SURFDEP.Initialize;
  MAXIMUM_ITERATIONS.Initialize;
  MAXIMUM_STAGE_CHANGE.Initialize;
  LENGTH_CONVERSION.Initialize;
  TIME_CONVERSION.Initialize;
end;

procedure TLakOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    if (FSplitter[0] = 'AUXILIARY')
      and (FSplitter.Count >= 2) then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      for AuxIndex := 1 to FSplitter.Count - 1 do
      begin
        AUXILIARY_Name := FSplitter[AuxIndex];
        AUXILIARY.Add(AUXILIARY_Name);
      end;
    end
    else if FSplitter[0] = 'BOUNDNAMES' then
    begin
      BOUNDNAMES := True;
    end
    else if FSplitter[0] = 'PRINT_INPUT' then
    begin
      PRINT_INPUT := True;
    end
    else if FSplitter[0] = 'PRINT_STAGE' then
    begin
      PRINT_STAGE := True;
    end
    else if FSplitter[0] = 'PRINT_FLOWS' then
    begin
      PRINT_FLOWS := True;
    end
    else if FSplitter[0] = 'SAVE_FLOWS' then
    begin
      SAVE_FLOWS := True;
    end
    else if (FSplitter[0] = 'STAGE')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEOUT') then
    begin
      STAGE := True;
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
    else if FSplitter[0] = 'MOVER' then
    begin
      MOVER := True;
    end
    else if (FSplitter[0] = 'SURFDEP') and (FSplitter.Count >= 2)
      and TryFortranStrToFloat(FSplitter[1], SURFDEP.Value) then
    begin
      SURFDEP.Used := True;
    end
    else if (FSplitter[0] = 'MAXIMUM_ITERATIONS') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], MAXIMUM_ITERATIONS.Value) then
    begin
      MAXIMUM_ITERATIONS.Used := True;
    end
    else if (FSplitter[0] = 'MAXIMUM_STAGE_CHANGE') and (FSplitter.Count >= 2)
      and TryFortranStrToFloat(FSplitter[1], MAXIMUM_STAGE_CHANGE.Value) then
    begin
      MAXIMUM_STAGE_CHANGE.Used := True;
    end

    else if (FSplitter[0] = 'LENGTH_CONVERSION') and (FSplitter.Count >= 2)
      and TryFortranStrToFloat(FSplitter[1], LENGTH_CONVERSION.Value) then
    begin
      LENGTH_CONVERSION.Used := True;
    end
    else if (FSplitter[0] = 'TIME_CONVERSION') and (FSplitter.Count >= 2)
      and TryFortranStrToFloat(FSplitter[1], TIME_CONVERSION.Value) then
    begin
      TIME_CONVERSION.Used := True;
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

{ TLakDimensions }

procedure TLakDimensions.Initialize;
begin
  inherited;
  NLAKES := 0;
  NOUTLETS := 0;
  NTABLES := 0;
end;

procedure TLakDimensions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    if (FSplitter[0] = 'NLAKES') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], NLAKES) then
    begin
    end
    else if (FSplitter[0] = 'NOUTLETS') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], NOUTLETS) then
    begin
    end
    else if (FSplitter[0] = 'NTABLES') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], NTABLES) then
    begin
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TLakPackageItem }

constructor TLakPackageItem.Create;
begin
  lakeno := 0;
  strt := 0;
  nlakeconn := 0;
  aux := TBoundaryValueList.Create;
  boundname := ''
end;

destructor TLakPackageItem.Destroy;
begin
  aux.Free;
  inherited;
end;

{ TLakPackageData }

constructor TLakPackageData.Create(PackageType: string);
begin
  FItems := TLakPackageItemList.Create;
  inherited;

end;

destructor TLakPackageData.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TLakPackageData.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TLakPackageData.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  naux: Integer; BOUNDNAMES: Boolean);
var
  ALine: string;
  ErrorLine: string;
  Item: TLakPackageItem;
  ItemStart: Integer;
  AuxIndex: Integer;
  AValue: TBoundaryValue;
  CaseSensitiveLine: string;
  NumberOfItems: Integer;
begin
  NumberOfItems := 3 + naux;
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
      FItems.Sort(
        TComparer<TLakPackageItem>.Construct(
          function(const Left, Right: TLakPackageItem): Integer
          begin
            Result := Left.lakeno - Right.lakeno;
          end
        ));
      Exit;
    end;

    CaseSensitiveLine := ALine;
    Item := TLakPackageItem.Create;
    try
      ALine := UpperCase(ALine);
      FSplitter.DelimitedText := ALine;
      if (FSplitter.Count >= NumberOfItems)
        and TryStrToInt(FSplitter[0],Item.lakeno)
        and TryFortranStrToFloat(FSplitter[1],Item.strt)
        and TryStrToInt(FSplitter[2],Item.nlakeconn)
        then
      begin
        ItemStart := 3;
        for AuxIndex := 0 to naux - 1 do
        begin
          AValue.Initialize;
          if TryFortranStrToFloat(FSplitter[ItemStart],AValue.NumericValue) then
          begin
            AValue.ValueType := vtNumeric
          end
          else
          begin
            FSplitter.DelimitedText := CaseSensitiveLine;
            AValue.ValueType := vtString;
            AValue.StringValue := FSplitter[ItemStart]
          end;
          Item.aux.Add(AVAlue);
          Inc(ItemStart);
        end;
        if BOUNDNAMES and (FSplitter.Count >= NumberOfItems+1) then
        begin
          Item.boundname := FSplitter[ItemStart];
        end;
        FItems.Add(Item);
        Item := nil;
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSPACK, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    finally
      Item.Free
    end;
  end;
end;

{ TTabFileItem }

procedure TLakeTableItem.Initialize;
begin
  lakeno := 0;
  tab6_filename := '';
end;

{ TLakTables }

constructor TLakTables.Create(PackageType: string);
begin
  FItems := TLakeTableItemList.Create;
  inherited;

end;

destructor TLakTables.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TLakTables.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TLakTables.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  Item: TLakeTableItem;
  CaseSensitiveLine: string;
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

    if ReadEndOfSection(ALine, ErrorLine, 'TABLES', Unhandled) then
    begin
      Exit;
    end;

    Item.Initialize;

    CaseSensitiveLine := ALine;
    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    if (FSplitter.Count >= 4)
      and TryStrToInt(FSplitter[0],Item.lakeno)
      and (FSplitter[1] = 'TAB6')
      and (FSplitter[2] = 'FILEIN')
      then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      Item.tab6_filename := FSplitter[3];
      FItems.Add(Item);
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSCROS, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TLakConnectionItem }

procedure TLakConnectionItem.Initialize;
begin
  lakeno := 0;
  iconn := 0;
  cellid.Initialize;
  claktype := '';
  bedleak := '';
  belev := 0;
  telev := 0;
  connlen := 0;
  connwidth := 0;
end;

{ TLakConnections }

constructor TLakConnections.Create(PackageType: string);
begin
  FItems := TLakConnectionItemList.Create;
  inherited;

end;

destructor TLakConnections.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TLakConnections.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TLakConnections.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  Dimensions: TDimensions);
var
  ALine: string;
  ErrorLine: string;
  Item: TLakConnectionItem;
  DimensionCount: Integer;
  ItemCount: Integer;
begin
  DimensionCount := Dimensions.DimensionCount;
  ItemCount := 8 + DimensionCount;
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

    if ReadEndOfSection(ALine, ErrorLine, 'CONNECTIONDATA', Unhandled) then
    begin
      Exit;
    end;

    Item.Initialize;
    FSplitter.DelimitedText := ALine;
    if (FSplitter.Count >= ItemCount)
      and TryStrToInt(FSplitter[0],Item.lakeno)
      and TryStrToInt(FSplitter[1],Item.iconn)
      and ReadCellId(Item.cellid, 2, DimensionCount)
      and TryFortranStrToFloat(FSplitter[4+DimensionCount], Item.belev)
      and TryFortranStrToFloat(FSplitter[5+DimensionCount], Item.telev)
      and TryFortranStrToFloat(FSplitter[6+DimensionCount], Item.connlen)
      and TryFortranStrToFloat(FSplitter[7+DimensionCount], Item.connwidth)
      then
    begin
      Item.claktype := FSplitter[2+DimensionCount];
      Item.bedleak := FSplitter[3+DimensionCount];
      FItems.Add(Item);
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSCONN, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TLakOutletItem }

procedure TLakOutletItem.Initialize;
begin
  outletno := 0;
  lakein := 0;
  lakeout := 0;
  couttype := '';
  invert.Initialize;
  width.Initialize;
  rough.Initialize;
  slope.Initialize;
end;

{ TLakOutlets }

constructor TLakOutlets.Create(PackageType: string);
begin
  FItems := TLakOutletItemList.Create;
  inherited;

end;

destructor TLakOutlets.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TLakOutlets.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TLakOutlets.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  Item: TLakOutletItem;
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

    if ReadEndOfSection(ALine, ErrorLine, 'OUTLETS', Unhandled) then
    begin
      Exit;
    end;

    Item.Initialize;
    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    if (FSplitter.Count >- 4)
      and TryStrToInt(FSplitter[0],Item.outletno)
      and TryStrToInt(FSplitter[1],Item.lakein)
      and TryStrToInt(FSplitter[2],Item.lakeout)

      then
    begin
      Item.couttype := FSplitter[3];
      if not TryFortranStrToFloat(FSplitter[4],Item.invert.NumericValue) then
      begin
        Item.invert.StringValue := FSplitter[4];
        Item.invert.ValueType := vtString;
      end;
      if not TryFortranStrToFloat(FSplitter[5],Item.width.NumericValue) then
      begin
        Item.width.StringValue := FSplitter[5];
        Item.width.ValueType := vtString;
      end;
      if not TryFortranStrToFloat(FSplitter[6],Item.rough.NumericValue) then
      begin
        Item.rough.StringValue := FSplitter[6];
        Item.rough.ValueType := vtString;
      end;
      if not TryFortranStrToFloat(FSplitter[7],Item.slope.NumericValue) then
      begin
        Item.slope.StringValue := FSplitter[7];
        Item.slope.ValueType := vtString;
      end;
      FItems.Add(Item);
    end
    else
    begin
      Unhandled.WriteLine(Format('Unrecognized %s OUTLETS in the following line', [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TLakPeriod }

constructor TLakPeriod.Create(PackageType: string);
begin
  FItems := TNumberedItemList.Create;
  inherited;

end;

destructor TLakPeriod.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TLakPeriod.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TLakPeriod.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  SfrItem: TNumberedItem;
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
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

    if ReadEndOfSection(ALine, ErrorLine, 'PERIOD', Unhandled) then
    begin
      Exit;
    end;

    SfrItem.Initialize;
    CaseSensitiveLine := ALine;
    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    if FSplitter.Count >= 3 then
    begin
      if TryStrToInt(FSplitter[0], SfrItem.IdNumber) then
      begin
        SfrItem.Name := FSplitter[1];
        if SfrItem.Name = 'STATUS' then
        begin
          // lakes
          SfrItem.StringValue := FSplitter[2];
        end
        else if
          // lakes
          (SfrItem.Name = 'STAGE')
          or (SfrItem.Name = 'RAINFALL')
          or (SfrItem.Name = 'EVAPORATION')
          or (SfrItem.Name = 'RUNOFF')
          or (SfrItem.Name = 'INFLOW')
          or (SfrItem.Name = 'WITHDRAWAL')
          or (SfrItem.Name = 'RATE')
          // Outlets
          or (SfrItem.Name = 'INVERT')
          or (SfrItem.Name = 'WIDTH')
          or (SfrItem.Name = 'SLOPE')
          or (SfrItem.Name = 'ROUGH')
          then
        begin
          if not TryFortranStrToFloat(FSplitter[2], SfrItem.FloatValue) then
          begin
            SfrItem.StringValue := FSplitter[2]
          end;
        end
        else if (SfrItem.Name = 'AUXILIARY')
          and TryFortranStrToFloat(FSplitter[3], SfrItem.FloatValue)
          then
        begin
          FSplitter.DelimitedText := CaseSensitiveLine;
          SfrItem.AuxName := FSplitter[2];
        end
        else
        begin
          Unhandled.WriteLine(Format(StrUnrecognizedSPERI, [FPackageType]));
          Unhandled.WriteLine(ErrorLine);
        end;
        FItems.Add(SfrItem);
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

{ TLak }

constructor TLak.Create(PackageType: string);
begin
  inherited;
  FOptions := TLakOptions.Create(PackageType);
  FLakDimensions := TLakDimensions.Create(PackageType);
  FPackageData := TLakPackageData.Create(PackageType);
  FLakTables := TLakTables.Create(PackageType);
  FConnections := TLakConnections.Create(PackageType);
  FLakOutlets := TLakOutlets.Create(PackageType);
  FPeriods := TSfrPeriodList.Create;
  FTimeSeriesPackages := TPackageList.Create;
  FObservationsPackages := TPackageList.Create;
  FTabFilePackages := TPackageList.Create;

end;

destructor TLak.Destroy;
begin
  FOptions.Free;
  FLakDimensions.Free;
  FPackageData.Free;
  FLakTables.Free;
  FConnections.Free;
  FLakOutlets.Free;
  FPeriods.Free;
  FTimeSeriesPackages.Free;
  FObservationsPackages.Free;
  FTabFilePackages.Free;
  inherited;
end;

procedure TLak.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  IPER: Integer;
  APeriod: TLakPeriod;
  TsPackage: TPackage;
  PackageIndex: Integer;
  TsReader: TTimeSeries;
  ObsReader: TObs;
  ObsPackage: TPackage;
  LakeTablePackage: TPackage;
  LakeTableReader: TLakeTable;
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
        FLakDimensions.Read(Stream, Unhandled);
      end
      else if FSplitter[1] ='PACKAGEDATA' then
      begin
        FPackageData.Read(Stream, Unhandled, FOptions.AUXILIARY.Count,
          FOptions.BOUNDNAMES);
      end
      else if FSplitter[1] ='CONNECTIONDATA' then
      begin
        FConnections.Read(Stream, Unhandled, FDimensions);
      end
      else if FSplitter[1] ='TABLES' then
      begin
        FLakTables.Read(Stream, Unhandled);
      end
      else if FSplitter[1] ='OUTLETS' then
      begin
        FLakOutlets.Read(Stream, Unhandled);
      end
      else if (FSplitter[1] ='PERIOD') and (FSplitter.Count >= 3) then
      begin
        if TryStrToInt(FSplitter[2], IPER) then
        begin
          APeriod := TLakPeriod.Create(FPackageType);
          FPeriods.Add(APeriod);
          APeriod.IPer := IPER;
          APeriod.Read(Stream, Unhandled);
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
  for PackageIndex := 0 to FLakTables.FItems.Count - 1 do
  begin
    LakeTablePackage := TPackage.Create;
    FObservationsPackages.Add(LakeTablePackage);
    LakeTablePackage.FileType := FPackageType;
    LakeTablePackage.FileName := FLakTables.FItems[PackageIndex].tab6_filename;
    LakeTablePackage.PackageName := '';

    LakeTableReader := TLakeTable.Create(FPackageType);
    LakeTablePackage.Package := LakeTableReader;
    LakeTablePackage.ReadPackage(Unhandled);
  end;
end;

end.
