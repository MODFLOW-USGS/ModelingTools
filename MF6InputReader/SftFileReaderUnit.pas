unit SftFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TSftOptions = class(TCustomMf6Persistent)
  private
    FLOW_PACKAGE_NAME: string;
    AUXILIARY: TStringList;
    FLOW_PACKAGE_AUXILIARY_NAME: string;
    BOUNDNAMES: Boolean;
    PRINT_INPUT: Boolean;
    PRINT_CONCENTRATION: Boolean;
    PRINT_FLOWS: Boolean;
    SAVE_FLOWS: Boolean;
    CONCENTRATION: Boolean;
    BUDGET: Boolean;
    BUDGETCSV: Boolean;
    TS6_FileNames: TStringList;
    Obs6_FileNames: TStringList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TSftPackageItem = class(TObject)
  private
    rno: Integer;
    strt: TBoundaryValue;
    aux: TBoundaryValueList;
    boundname: string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TSftPackageItemList= TObjectList<TSftPackageItem>;

  TSftPackageData = class(TCustomMf6Persistent)
  private
    FItems: TSftPackageItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; naux: Integer;
      BOUNDNAMES: Boolean);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TSftPeriod = class(TCustomMf6Persistent)
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

  TSftPeriodList = TObjectList<TSftPeriod>;

  TSft = class(TDimensionedPackageReader)
  private
    FOptions: TSftOptions;
    FPackageData: TSftPackageData;
    FPeriods: TSftPeriodList;
    FTimeSeriesPackages: TPackageList;
    FObservationsPackages: TPackageList;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
  end;


implementation

uses
  System.Generics.Defaults, ModelMuseUtilities, TimeSeriesFileReaderUnit,
  ObsFileReaderUnit;

{ TSftOptions }

constructor TSftOptions.Create(PackageType: string);
begin
  AUXILIARY := TStringList.Create;
  TS6_FileNames := TStringList.Create;
  Obs6_FileNames := TStringList.Create;
  inherited;
end;

destructor TSftOptions.Destroy;
begin
  AUXILIARY.Free;
  TS6_FileNames.Free;
  Obs6_FileNames.Free;
  inherited;
end;

procedure TSftOptions.Initialize;
begin
  inherited;
  FLOW_PACKAGE_NAME := '';
  AUXILIARY.Clear;
  FLOW_PACKAGE_AUXILIARY_NAME := '';
  BOUNDNAMES := False;
  PRINT_INPUT := False;
  PRINT_CONCENTRATION := False;
  PRINT_FLOWS := False;
  SAVE_FLOWS := False;
  CONCENTRATION := False;
  BUDGET := False;
  BUDGETCSV := False;
  TS6_FileNames.Clear;
  Obs6_FileNames.Clear;
end;

procedure TSftOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    if (FSplitter[0] = 'FLOW_PACKAGE_NAME')
      and (FSplitter.Count >= 2) then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      FLOW_PACKAGE_NAME := FSplitter[1];
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
    else if (FSplitter[0] = 'FLOW_PACKAGE_AUXILIARY_NAME')
      and (FSplitter.Count >= 2) then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      FLOW_PACKAGE_AUXILIARY_NAME := FSplitter[1];
    end
    else if FSplitter[0] = 'BOUNDNAMES' then
    begin
      BOUNDNAMES := True;
    end
    else if FSplitter[0] = 'PRINT_INPUT' then
    begin
      PRINT_INPUT := True;
    end
    else if FSplitter[0] = 'PRINT_CONCENTRATION' then
    begin
      PRINT_CONCENTRATION := True;
    end
    else if FSplitter[0] = 'PRINT_FLOWS' then
    begin
      PRINT_FLOWS := True;
    end
    else if FSplitter[0] = 'SAVE_FLOWS' then
    begin
      SAVE_FLOWS := True;
    end
    else if (FSplitter[0] = 'CONCENTRATION')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEOUT') then
    begin
      CONCENTRATION := True;
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

{ TSftPackageItem }

constructor TSftPackageItem.Create;
begin
  rno := 0;
  strt.Initialize;
  aux := TBoundaryValueList.Create;
  boundname := ''
end;

destructor TSftPackageItem.Destroy;
begin
  aux.Free;
  inherited;
end;

{ TSftPackageData }

constructor TSftPackageData.Create(PackageType: string);
begin
  FItems := TSftPackageItemList.Create;
  inherited;

end;

destructor TSftPackageData.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TSftPackageData.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TSftPackageData.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  naux: Integer; BOUNDNAMES: Boolean);
var
  ALine: string;
  ErrorLine: string;
  Item: TSftPackageItem;
  ItemStart: Integer;
  AuxIndex: Integer;
  AValue: TBoundaryValue;
  CaseSensitiveLine: string;
  NumberOfItems: Integer;
begin
  NumberOfItems := 2 + naux;
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
        TComparer<TSftPackageItem>.Construct(
          function(const Left, Right: TSftPackageItem): Integer
          begin
            Result := Left.rno - Right.rno;
          end
        ));
      Exit;
    end;

    CaseSensitiveLine := ALine;
    Item := TSftPackageItem.Create;
    try
      ALine := UpperCase(ALine);
      FSplitter.DelimitedText := ALine;
      if (FSplitter.Count >= NumberOfItems)
        and TryStrToInt(FSplitter[0],Item.rno)
        then
      begin
        if TryFortranStrToFloat(FSplitter[1],Item.strt.NumericValue) then
        begin
          Item.strt.ValueType := vtNumeric;
        end
        else
        begin
          Item.strt.ValueType := vtString;
          Item.strt.StringValue := FSplitter[1];
        end;

        ItemStart := 2;
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
          FSplitter.DelimitedText := CaseSensitiveLine;
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

{ TSftPeriod }

constructor TSftPeriod.Create(PackageType: string);
begin
  FItems := TNumberedItemList.Create;
  inherited;
end;

destructor TSftPeriod.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TSftPeriod.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TSftPeriod.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  SftItem: TNumberedItem;
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

    SftItem.Initialize;
    CaseSensitiveLine := ALine;
    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    if FSplitter.Count >= 3 then
    begin
      if TryStrToInt(FSplitter[0], SftItem.IdNumber) then
      begin
        SftItem.Name := FSplitter[1];
        if SftItem.Name = 'STATUS' then
        begin
          SftItem.StringValue := FSplitter[2];
        end
        else if (SftItem.Name = 'CONCENTRATION')
          or (SftItem.Name = 'RAINFALL')
          or (SftItem.Name = 'EVAPORATION')
          or (SftItem.Name = 'RUNOFF')
          or (SftItem.Name = 'INFLOW')
          then
        begin
          if TryFortranStrToFloat(FSplitter[2], SftItem.FloatValue) then
          begin

          end
          else
          begin
            SftItem.StringValue := FSplitter[2]
          end;
        end
        else if (SftItem.Name = 'AUXILIARY')
          then
        begin
          if TryFortranStrToFloat(FSplitter[3], SftItem.FloatValue) then
          begin

          end
          else
          begin
            FSplitter.DelimitedText := CaseSensitiveLine;
            SftItem.StringValue := FSplitter[3]
          end;
          FSplitter.DelimitedText := CaseSensitiveLine;
          SftItem.AuxName := FSplitter[2];
        end
        else
        begin
          Unhandled.WriteLine(Format(StrUnrecognizedSPERI, [FPackageType]));
          Unhandled.WriteLine(ErrorLine);
        end;
        FItems.Add(SftItem);
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

{ TSft }

constructor TSft.Create(PackageType: string);
begin
  inherited;
  FOptions := TSftOptions.Create(PackageType);
  FPackageData := TSftPackageData.Create(PackageType);
  FPeriods := TSftPeriodList.Create;
  FTimeSeriesPackages := TPackageList.Create;
  FObservationsPackages := TPackageList.Create;
end;

destructor TSft.Destroy;
begin
  FOptions.Free;
  FPackageData.Free;
  FPeriods := TSftPeriodList.Create;
  FTimeSeriesPackages := TPackageList.Create;
  FObservationsPackages := TPackageList.Create;
  inherited;
end;

procedure TSft.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  IPER: Integer;
  APeriod: TSftPeriod;
  TsPackage: TPackage;
  PackageIndex: Integer;
  TsReader: TTimeSeries;
  ObsReader: TObs;
  ObsPackage: TPackage;
  CrossSectionPackage: TPackage;
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
      else if FSplitter[1] ='PACKAGEDATA' then
      begin
        FPackageData.Read(Stream, Unhandled, FOptions.AUXILIARY.Count,
          FOptions.BOUNDNAMES);
      end
      else if (FSplitter[1] ='PERIOD') and (FSplitter.Count >= 3) then
      begin
        if TryStrToInt(FSplitter[2], IPER) then
        begin
          APeriod := TSftPeriod.Create(FPackageType);
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
