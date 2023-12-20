unit Mf6.MawFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TMawOptions = class(TCustomMf6Persistent)
  private
    AUXILIARY: TStringList;
    BOUNDNAMES: Boolean;
    PRINT_INPUT: Boolean;
    PRINT_HEAD: Boolean;
    PRINT_FLOWS: Boolean;
    SAVE_FLOWS: Boolean;
    HEAD: Boolean;
    BUDGET: Boolean;
    BUDGETCSV: Boolean;
    NO_WELL_STORAGE: Boolean;
    FLOW_CORRECTION: Boolean;
    FLOWING_WELLS: Boolean;
    SHUTDOWN_THETA: TRealOption;
    SHUTDOWN_KAPPA: TRealOption;
    MAW_FLOW_REDUCE_CSV: Boolean;
    TS6_FileNames: TStringList;
    Obs6_FileNames: TStringList;
    MOVER: Boolean;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TMawDimensions = class(TCustomMf6Persistent)
  private
    NMAWWELLS: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TMawPackageItem = class(TObject)
  private
    wellno: Integer;
    radius: Extended;
    bottom: Extended;
    strt: Extended;
    condeqn: string;
    ngwfnodes: Integer;
    aux: TBoundaryValueList;
    boundname: string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TMawPackageItemList = TObjectList<TMawPackageItem>;

  TMawPackageData = class(TCustomMf6Persistent)
  private
    FItems: TMawPackageItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; naux: Integer;
      BOUNDNAMES: Boolean);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TMawConnectionItem = record
    wellno: Integer;
    icon: Integer;
    cellid: TCellId;
    scrn_top: Extended;
    scrn_bot: Extended;
    hk_skin: Extended;
    radius_skin: Extended;
    procedure Initialize;
  end;

  TMawConnectionItemList = TList<TMawConnectionItem>;

  TMawConnectionData = class(TCustomMf6Persistent)
  private
    FItems: TMawConnectionItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TMawPeriod = class(TCustomMf6Persistent)
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

  TMawPeriodList = TObjectList<TMawPeriod>;

  TMaw = class(TDimensionedPackageReader)
  private
    FOptions: TMawOptions;
    FMawDimensions: TMawDimensions;
    FConnections: TMawConnectionData;
    FPackageData: TMawPackageData;
    FPeriods: TMawPeriodList;
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

{ TMawOptions }

constructor TMawOptions.Create(PackageType: string);
begin
  AUXILIARY := TStringList.Create;
  TS6_FileNames := TStringList.Create;
  Obs6_FileNames := TStringList.Create;
  inherited;
end;

destructor TMawOptions.Destroy;
begin
  AUXILIARY.Free;
  TS6_FileNames.Free;
  Obs6_FileNames.Free;
  inherited;
end;

procedure TMawOptions.Initialize;
begin
  inherited;
  AUXILIARY.Clear;
  BOUNDNAMES := False;
  PRINT_INPUT := False;
  PRINT_HEAD := False;
  PRINT_FLOWS := False;
  SAVE_FLOWS := False;
  HEAD := False;
  BUDGET := False;
  BUDGETCSV := False;
  NO_WELL_STORAGE := False;
  FLOW_CORRECTION := False;
  FLOWING_WELLS := False;
  SHUTDOWN_THETA.Initialize;
  SHUTDOWN_KAPPA.Initialize;
  MAW_FLOW_REDUCE_CSV := False;
  TS6_FileNames.Clear;
  Obs6_FileNames.Clear;
  MOVER := False;
end;

procedure TMawOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    else if FSplitter[0] = 'BOUNDNAMES' then
    begin
      BOUNDNAMES := True;
    end
    else if FSplitter[0] = 'PRINT_INPUT' then
    begin
      PRINT_INPUT := True;
    end
    else if FSplitter[0] = 'PRINT_HEAD' then
    begin
      PRINT_HEAD := True;
    end
    else if FSplitter[0] = 'PRINT_FLOWS' then
    begin
      PRINT_FLOWS := True;
    end
    else if FSplitter[0] = 'SAVE_FLOWS' then
    begin
      SAVE_FLOWS := True;
    end
    else if (FSplitter[0] = 'HEAD')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEOUT') then
    begin
      HEAD := True;
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
    else if FSplitter[0] = 'NO_WELL_STORAGE' then
    begin
      NO_WELL_STORAGE := True;
    end
    else if FSplitter[0] = 'FLOW_CORRECTION' then
    begin
      FLOW_CORRECTION := True;
    end
    else if FSplitter[0] = 'FLOWING_WELLS' then
    begin
      FLOWING_WELLS := True;
    end
    else if (FSplitter[0] = 'SHUTDOWN_THETA') and (FSplitter.Count >= 2)
      and TryFortranStrToFloat(FSplitter[1], SHUTDOWN_THETA.Value) then
    begin
      SHUTDOWN_THETA.Used := True;
    end
    else if (FSplitter[0] = 'SHUTDOWN_KAPPA') and (FSplitter.Count >= 2)
      and TryFortranStrToFloat(FSplitter[1], SHUTDOWN_KAPPA.Value) then
    begin
      SHUTDOWN_KAPPA.Used := True;
    end
    else if (FSplitter[0] = 'MAW_FLOW_REDUCE_CSV')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEOUT') then
    begin
      MAW_FLOW_REDUCE_CSV := True;
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
    else if FSplitter[0] = 'MOVER' then
    begin
      MOVER := True;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TMawDimensions }

procedure TMawDimensions.Initialize;
begin
  inherited;
  NMAWWELLS := 0;
end;

procedure TMawDimensions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    else if (FSplitter[0] = 'NMAWWELLS') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], NMAWWELLS) then
    begin
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TMawPackageItem }

constructor TMawPackageItem.Create;
begin
  aux := TBoundaryValueList.Create;
  wellno := 0;
  radius := 0;
  bottom := 0;
  strt := 0;
  condeqn := '';
  ngwfnodes := 0;
  boundname := '';
end;

destructor TMawPackageItem.Destroy;
begin
  aux.Free;
  inherited;
end;

{ TMawPackageData }

constructor TMawPackageData.Create(PackageType: string);
begin
  FItems := TMawPackageItemList.Create;
  inherited;

end;

destructor TMawPackageData.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TMawPackageData.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TMawPackageData.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  naux: Integer; BOUNDNAMES: Boolean);
var
  ALine: string;
  ErrorLine: string;
  Item: TMawPackageItem;
  ItemStart: Integer;
  AuxIndex: Integer;
  AValue: TBoundaryValue;
  CaseSensitiveLine: string;
  NumberOfItems: Integer;
begin
  Initialize;
  NumberOfItems := 6 + naux;
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
      Exit;
    end;

    CaseSensitiveLine := ALine;
    Item := TMawPackageItem.Create;
    try
      if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'PACKAGEDATA') then
      begin
        // do nothing
      end
      else if (FSplitter.Count >= NumberOfItems)
        and TryStrToInt(FSplitter[0],Item.wellno)
        and TryFortranStrToFloat(FSplitter[1],Item.radius)
        and TryFortranStrToFloat(FSplitter[2],Item.bottom)
        and TryFortranStrToFloat(FSplitter[3],Item.strt)
        and TryStrToInt(FSplitter[5],Item.ngwfnodes)
        then
      begin
        Item.condeqn := FSplitter[4];
        ItemStart := 6;
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

{ TMawConnectionData }

constructor TMawConnectionData.Create(PackageType: string);
begin
  FItems := TMawConnectionItemList.Create;
  inherited;

end;

destructor TMawConnectionData.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TMawConnectionData.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TMawConnectionData.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter; Dimensions: TDimensions);
var
  ALine: string;
  ErrorLine: string;
  DimensionCount: Integer;
  Item: TMawConnectionItem;
  CaseSensitiveLine: string;
begin
  DimensionCount := Dimensions.DimensionCount;
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

    if ReadEndOfSection(ALine, ErrorLine, 'CONNECTIONDATA', Unhandled) then
    begin
      Exit;
    end;

    CaseSensitiveLine := ALine;
    Item.Initialize;;
    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'CONNECTIONDATA') then
    begin
      // do nothing
    end
    else if (FSplitter.Count >= 6 + DimensionCount)
      and TryStrToInt(FSplitter[0],Item.wellno)
      and TryStrToInt(FSplitter[1],Item.icon)
      and ReadCellID(Item.cellid, 2, DimensionCount)
      and TryFortranStrToFloat(FSplitter[2+DimensionCount],Item.scrn_top)
      and TryFortranStrToFloat(FSplitter[3+DimensionCount],Item.scrn_bot)
      and TryFortranStrToFloat(FSplitter[4+DimensionCount],Item.hk_skin)
      and TryFortranStrToFloat(FSplitter[5+DimensionCount],Item.radius_skin)
      then
    begin
      FItems.Add(Item);
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSCONN, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TMawConnectionItem }

procedure TMawConnectionItem.Initialize;
begin
  wellno := 0;
  icon := 0;
  cellid.Initialize;
  scrn_top := 0;
  scrn_bot := 0;
  hk_skin := 0;
  radius_skin := 0;
end;

{ TMawPeriod }

constructor TMawPeriod.Create(PackageType: string);
begin
  FItems := TNumberedItemList.Create;
  inherited;

end;

destructor TMawPeriod.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TMawPeriod.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TMawPeriod.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  MawItem: TNumberedItem;
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
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

    if ReadEndOfSection(ALine, ErrorLine, 'PERIOD', Unhandled) then
    begin
      Exit;
    end;

    MawItem.Initialize;
    CaseSensitiveLine := ALine;
    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'PERIOD') then
    begin
      // do nothing
    end
    else if FSplitter.Count >= 3 then
    begin
      if TryStrToInt(FSplitter[0], MawItem.IdNumber) then
      begin
        MawItem.Name := FSplitter[1];
        if MawItem.Name = 'STATUS' then
        begin
          MawItem.StringValue := FSplitter[2];
        end
        else if (MawItem.Name = 'FLOWING_WELL') and (FSplitter.Count >= 5)  then
        begin
          SetLength(MawItem.FloatValues, 3);
          if TryFortranStrToFloat(FSplitter[2], MawItem.FloatValues[0])
            and TryFortranStrToFloat(FSplitter[3], MawItem.FloatValues[1])
            and TryFortranStrToFloat(FSplitter[4], MawItem.FloatValues[2])
            then
          begin

          end
          else
          begin
            Unhandled.WriteLine(Format(StrUnrecognizedSPERI, [FPackageType]));
            Unhandled.WriteLine(ErrorLine);
          end;
        end
        else if (MawItem.Name = 'RATE')
          or  (MawItem.Name = 'WELL_HEAD')
          or (MawItem.Name = 'AUXILIARY')
          then
        begin
          if TryFortranStrToFloat(FSplitter[2], MawItem.FloatValue) then
          begin

          end
          else
          begin
            MawItem.StringValue := FSplitter[2]
          end;
        end
        else if MawItem.Name = 'HEAD_LIMIT' then
        begin
          if TryFortranStrToFloat(FSplitter[2], MawItem.FloatValue) then
          begin

          end
          else
          begin
            Unhandled.WriteLine(Format(StrUnrecognizedSPERI, [FPackageType]));
            Unhandled.WriteLine(ErrorLine);
          end;
        end
        else if (MawItem.Name = 'SHUT_OFF')
          or (MawItem.Name = 'RATE_SCALING') then
        begin
          SetLength(MawItem.FloatValues, 2);
          if TryFortranStrToFloat(FSplitter[2], MawItem.FloatValues[0])
            and TryFortranStrToFloat(FSplitter[3], MawItem.FloatValues[1])
            then
          begin

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
        FItems.Add(MawItem);
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

{ TMaw }

constructor TMaw.Create(PackageType: string);
begin
  inherited;
  FOptions := TMawOptions.Create(PackageType);
  FMawDimensions := TMawDimensions.Create(PackageType);
  FConnections := TMawConnectionData.Create(PackageType);
  FPackageData := TMawPackageData.Create(PackageType);
  FPeriods := TMawPeriodList.Create;
  FTimeSeriesPackages := TPackageList.Create;
  FObservationsPackages := TPackageList.Create;
end;

destructor TMaw.Destroy;
begin
  FOptions.Free;
  FMawDimensions.Free;
  FConnections.Free;
  FPackageData.Free;
  FPeriods.Free;
  FTimeSeriesPackages.Free;
  FObservationsPackages.Free;
  inherited;
end;

procedure TMaw.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  IPER: Integer;
  APeriod: TMawPeriod;
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
        FMawDimensions.Read(Stream, Unhandled);
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
      else if (FSplitter[1] ='PERIOD') and (FSplitter.Count >= 3) then
      begin
        if TryStrToInt(FSplitter[2], IPER) then
        begin
          APeriod := TMawPeriod.Create(FPackageType);
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
end;

end.
