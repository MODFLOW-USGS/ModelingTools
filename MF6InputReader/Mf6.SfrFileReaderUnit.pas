unit Mf6.SfrFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TSfrOptions = class(TCustomMf6Persistent)
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
    MAXIMUM_PICARD_ITERATIONS: TIntegerOption;
    MAXIMUM_ITERATIONS: TIntegerOption;
    LENGTH_CONVERSION: TRealOption;
    TIME_CONVERSION: TRealOption;
    UNIT_CONVERSION: TRealOption;
    MAXIMUM_DEPTH_CHANGE: TRealOption;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TSfrDimensions = class(TCustomMf6Persistent)
  private
    NREACHES: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TSfrPackageItem = class(TObject)
  private
    rno: Integer;
    cellid: TCellId;
    rlen: Extended;
    rwid: Extended;
    rgrd: Extended;
    rtp: Extended;
    rbth: Extended;
    rhk: Extended;
    man: TMf6BoundaryValue;
    ncon: Integer;
    ustrf: TMf6BoundaryValue;
    ndv: Integer;
    aux: TBoundaryValueList;
    boundname: string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TSfrPackageItemList= TObjectList<TSfrPackageItem>;

  TSfrPackageData = class(TCustomMf6Persistent)
  private
    FItems: TSfrPackageItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; naux: Integer;
      BOUNDNAMES: Boolean; Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TCrossSectionItem = record
    rno: Integer;
    tab6_filename: string;
    procedure Initialize;
  end;

  TCrossSectionItemList = TList<TCrossSectionItem>;

  TSfrCrossSections = class(TCustomMf6Persistent)
  private
    FItems: TCrossSectionItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TSfrConnectionItem = record
  private
    rno: Integer;
    ic: array of Integer;
    procedure Initialize;
  end;

  TSfrConnectionItemList = TList<TSfrConnectionItem>;

  TSfrConnections = class(TCustomMf6Persistent)
  private
    FItems: TSfrConnectionItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      PackageItems: TSfrPackageItemList);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TSfrDiversionItem = record
    rno: Integer;
    idv: Integer;
    iconr: Integer;
    cprior: string;
    procedure Initialize;
  end;

  TSfrDiversionItemList = TList<TSfrDiversionItem>;

  TSfrDiversions = class(TCustomMf6Persistent)
  private
    FItems: TSfrDiversionItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TSfrPeriod = class(TCustomMf6Persistent)
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

  TSfrPeriodList = TObjectList<TSfrPeriod>;

  TSfr = class(TDimensionedPackageReader)
  private
    FOptions: TSfrOptions;
    FSfrDimensions: TSfrDimensions;
    FPackageData: TSfrPackageData;
    FSfrCrossSections: TSfrCrossSections;
    FConnections: TSfrConnections;
    FSfrDiversions: TSfrDiversions;
    FPeriods: TSfrPeriodList;
    FTimeSeriesPackages: TPackageList;
    FObservationsPackages: TPackageList;
    FTabFilePackages: TPackageList;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer); override;
  end;


implementation

uses
  ModelMuseUtilities, System.Generics.Defaults, Mf6.TimeSeriesFileReaderUnit,
  Mf6.ObsFileReaderUnit, Mf6.CrossSectionFileReaderUnit;

resourcestring
  StrUnrecognizedSCROS = 'Unrecognized %s CROSSSECTIONS data in the followin' +
  'g line';

{ TSfrOptions }

constructor TSfrOptions.Create(PackageType: string);
begin
  AUXILIARY := TStringList.Create;
  AUXILIARY.CaseSensitive := False;
  TS6_FileNames := TStringList.Create;
  Obs6_FileNames := TStringList.Create;
  inherited;

end;

destructor TSfrOptions.Destroy;
begin
  AUXILIARY.Free;
  TS6_FileNames.Free;
  Obs6_FileNames.Free;
  inherited;
end;

procedure TSfrOptions.Initialize;
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
  MAXIMUM_PICARD_ITERATIONS.Initialize;
  MAXIMUM_ITERATIONS.Initialize;
  LENGTH_CONVERSION.Initialize;
  TIME_CONVERSION.Initialize;
  MAXIMUM_DEPTH_CHANGE.Initialize;
  UNIT_CONVERSION.Initialize;
end;

procedure TSfrOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    else if (FSplitter[0] = 'MAXIMUM_PICARD_ITERATIONS') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], MAXIMUM_PICARD_ITERATIONS.Value) then
    begin
      MAXIMUM_PICARD_ITERATIONS.Used := True;
    end
    else if (FSplitter[0] = 'MAXIMUM_ITERATIONS') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], MAXIMUM_ITERATIONS.Value) then
    begin
      MAXIMUM_ITERATIONS.Used := True;
    end
    else if (FSplitter[0] = 'MAXIMUM_DEPTH_CHANGE') and (FSplitter.Count >= 2)
      and TryFortranStrToFloat(FSplitter[1], MAXIMUM_DEPTH_CHANGE.Value) then
    begin
      MAXIMUM_DEPTH_CHANGE.Used := True;
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
    else if (FSplitter[0] = 'UNIT_CONVERSION') and (FSplitter.Count >= 2)
      and TryFortranStrToFloat(FSplitter[1], UNIT_CONVERSION.Value) then
    begin
      UNIT_CONVERSION.Used := True;
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

{ TSfrDimensions }

procedure TSfrDimensions.Initialize;
begin
  inherited;
  NREACHES := 0;
end;

procedure TSfrDimensions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    else if (FSplitter[0] = 'NREACHES') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], NREACHES) then
    begin
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TSfrPackageItem }

constructor TSfrPackageItem.Create;
begin
  rno := 0;
  cellid.Initialize;
  rlen := 0;
  rwid := 0;
  rgrd := 0;
  rtp := 0;
  rbth := 0;
  rhk := 0;
  man.Initialize;
  ncon := 0;
  ustrf.Initialize;
  ndv := 0;
  aux := TBoundaryValueList.Create;
  boundname := ''
end;

destructor TSfrPackageItem.Destroy;
begin
  aux.Free;
  inherited;
end;

{ TSfrPackageData }

constructor TSfrPackageData.Create(PackageType: string);
begin
  FItems := TSfrPackageItemList.Create;
  inherited;

end;

destructor TSfrPackageData.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TSfrPackageData.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TSfrPackageData.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  naux: Integer; BOUNDNAMES: Boolean; Dimensions: TDimensions);
var
  ALine: string;
  ErrorLine: string;
  Item: TSfrPackageItem;
  ItemStart: Integer;
  AuxIndex: Integer;
  AValue: TMf6BoundaryValue;
  CaseSensitiveLine: string;
  DimensionCount: Integer;
  NumberOfItems: Integer;
begin
  DimensionCount := Dimensions.DimensionCount;
  NumberOfItems := 11 + naux + DimensionCount;
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
        TComparer<TSfrPackageItem>.Construct(
          function(const Left, Right: TSfrPackageItem): Integer
          begin
            Result := Left.rno - Right.rno;
          end
        ));
      Exit;
    end;

    CaseSensitiveLine := ALine;
    Item := TSfrPackageItem.Create;
    try
      if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'PACKAGEDATA') then
      begin
        // do nothing
      end
      else if (FSplitter.Count >= 12) and (FSplitter[1] = 'NONE') then
      begin
        if TryStrToInt(FSplitter[0],Item.rno)
          and TryFortranStrToFloat(FSplitter[2],Item.rlen)
          and TryFortranStrToFloat(FSplitter[3],Item.rwid)
          and TryFortranStrToFloat(FSplitter[4],Item.rgrd)
          and TryFortranStrToFloat(FSplitter[5],Item.rtp)
          and TryFortranStrToFloat(FSplitter[6],Item.rbth)
          and TryFortranStrToFloat(FSplitter[7],Item.rhk)
          and TryStrToInt(FSplitter[9],Item.ncon)
          and TryStrToInt(FSplitter[11],Item.ndv)
          then
        begin
          Item.cellid.Layer := -1;
          Item.cellid.Row := -1;
          Item.cellid.Column := -1;
          if TryFortranStrToFloat(FSplitter[8],Item.man.NumericValue) then
          begin
            Item.man.ValueType := vtNumeric;
          end
          else
          begin
            Item.man.ValueType := vtString;
            Item.man.StringValue := FSplitter[8];
          end;
          if TryFortranStrToFloat(FSplitter[10],Item.ustrf.NumericValue) then
          begin
            Item.ustrf.ValueType := vtNumeric;
          end
          else
          begin
            Item.ustrf.ValueType := vtString;
            Item.ustrf.StringValue := FSplitter[10];
          end;
          ItemStart := 12;
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
      end
      else if (FSplitter.Count >= NumberOfItems)
        and TryStrToInt(FSplitter[0],Item.rno)
        and ReadCellID(Item.cellid, 1, DimensionCount)
        and TryFortranStrToFloat(FSplitter[DimensionCount+1],Item.rlen)
        and TryFortranStrToFloat(FSplitter[DimensionCount+2],Item.rwid)
        and TryFortranStrToFloat(FSplitter[DimensionCount+3],Item.rgrd)
        and TryFortranStrToFloat(FSplitter[DimensionCount+4],Item.rtp)
        and TryFortranStrToFloat(FSplitter[DimensionCount+5],Item.rbth)
        and TryFortranStrToFloat(FSplitter[DimensionCount+6],Item.rhk)
        and TryStrToInt(FSplitter[DimensionCount+8],Item.ncon)
        and TryStrToInt(FSplitter[DimensionCount+10],Item.ndv)
        then
      begin
        if TryFortranStrToFloat(FSplitter[DimensionCount+7],Item.man.NumericValue) then
        begin
          Item.man.ValueType := vtNumeric;
        end
        else
        begin
          Item.man.ValueType := vtString;
          Item.man.StringValue := FSplitter[DimensionCount+7];
        end;
        if TryFortranStrToFloat(FSplitter[DimensionCount+9],Item.ustrf.NumericValue) then
        begin
          Item.ustrf.ValueType := vtNumeric;
        end
        else
        begin
          Item.ustrf.ValueType := vtString;
          Item.ustrf.StringValue := FSplitter[DimensionCount+7];
        end;

        ItemStart := DimensionCount+11;
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

{ TTabFileItem }

procedure TCrossSectionItem.Initialize;
begin
  rno := 0;
  tab6_filename := '';
end;

{ TSfrCrossSections }

constructor TSfrCrossSections.Create(PackageType: string);
begin
  FItems := TCrossSectionItemList.Create;
  inherited;

end;

destructor TSfrCrossSections.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TSfrCrossSections.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TSfrCrossSections.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  Item: TCrossSectionItem;
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

    if ReadEndOfSection(ALine, ErrorLine, 'CROSSSECTIONS', Unhandled) then
    begin
      Exit;
    end;

    Item.Initialize;

    CaseSensitiveLine := ALine;
    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'CROSSSECTIONS') then
    begin
      // do nothing
    end
    else if (FSplitter.Count >= 4)
      and TryStrToInt(FSplitter[0],Item.rno)
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

{ TSfrConnectionItem }

procedure TSfrConnectionItem.Initialize;
begin
  rno := 0;
  SetLength(ic, 0);
end;

{ TSfrConnections }

constructor TSfrConnections.Create(PackageType: string);
begin
  FItems := TSfrConnectionItemList.Create;
  inherited;

end;

destructor TSfrConnections.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TSfrConnections.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TSfrConnections.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  PackageItems: TSfrPackageItemList);
var
  ALine: string;
  ErrorLine: string;
  Item: TSfrConnectionItem;
  IcIndex: Integer;
  ncon: Integer;
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

    if ReadEndOfSection(ALine, ErrorLine, 'CONNECTIONDATA', Unhandled) then
    begin
      Exit;
    end;

    Item.Initialize;;
    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'CONNECTIONDATA') then
    begin
      // do nothing
    end
    else if TryStrToInt(FSplitter[0],Item.rno) then
    begin

      if (Item.rno >= 1) and (Item.rno <= PackageItems.Count) then
      begin
        ncon := PackageItems[Item.rno-1].ncon;
        if FSplitter.Count >= ncon+1 then
        begin
          SetLength(Item.ic, ncon);
          for IcIndex := 0 to Length(Item.ic) - 1 do
          begin
            Item.ic[IcIndex] := 0;
          end;
          for IcIndex := 0 to Length(Item.ic) - 1 do
          begin
            if not TryStrToInt(FSplitter[IcIndex+1], Item.ic[IcIndex]) then
            begin
              Unhandled.WriteLine(Format(StrUnrecognizedSCONN, [FPackageType]));
              Unhandled.WriteLine(ErrorLine);
              Break;
            end;
          end;
          FItems.Add(Item);
        end
        else
        begin
          Unhandled.WriteLine(Format(StrUnrecognizedSCONN, [FPackageType]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSCONN, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSCONN, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TSfrDiversionItem }

procedure TSfrDiversionItem.Initialize;
begin
  rno := 0;
  idv := 0;
  iconr := 0;
  cprior := '';
end;

{ TSfrDiversions }

constructor TSfrDiversions.Create(PackageType: string);
begin
  FItems := TSfrDiversionItemList.Create;
  inherited;

end;

destructor TSfrDiversions.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TSfrDiversions.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TSfrDiversions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  Item: TSfrDiversionItem;
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

    if ReadEndOfSection(ALine, ErrorLine, 'DIVERSIONS', Unhandled) then
    begin
      Exit;
    end;

    Item.Initialize;
    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'DIVERSIONS') then
    begin
      // do nothing
    end
    else if (FSplitter.Count >- 4)
      and TryStrToInt(FSplitter[0],Item.rno)
      and TryStrToInt(FSplitter[1],Item.idv)
      and TryStrToInt(FSplitter[2],Item.iconr)
      then
    begin
      Item.cprior := FSplitter[3];
      FItems.Add(Item);
    end
    else
    begin
      Unhandled.WriteLine(Format('Unrecognized %s DIVERSIONS in the following line', [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TSfrPeriod }

constructor TSfrPeriod.Create(PackageType: string);
begin
  FItems := TNumberedItemList.Create;
  inherited;

end;

destructor TSfrPeriod.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TSfrPeriod.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TSfrPeriod.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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

    SfrItem.Initialize;
    CaseSensitiveLine := ALine;
    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'PERIOD') then
    begin
      // do nothing
    end
    else if FSplitter.Count >= 3 then
    begin
      if TryStrToInt(FSplitter[0], SfrItem.IdNumber) then
      begin
        SfrItem.Name := FSplitter[1];
        if SfrItem.Name = 'STATUS' then
        begin
          SfrItem.StringValue := FSplitter[2];
        end
        else if (SfrItem.Name = 'MANNING')
          or  (SfrItem.Name = 'STAGE')
          or (SfrItem.Name = 'INFLOW')
          or (SfrItem.Name = 'RAINFALL')
          or (SfrItem.Name = 'EVAPORATION')
          or (SfrItem.Name = 'RUNOFF')
          or (SfrItem.Name = 'UPSTREAM_FRACTION')
          then
        begin
          if TryFortranStrToFloat(FSplitter[2], SfrItem.FloatValue) then
          begin

          end
          else
          begin
            SfrItem.StringValue := FSplitter[2]
          end;
        end
        else if (SfrItem.Name = 'DIVERSION')
          and (FSplitter.Count >= 4)
          then
        begin
          if TryStrToInt(FSplitter[2], SfrItem.IntValue) then
          begin
            if TryFortranStrToFloat(FSplitter[3], SfrItem.FloatValue) then
            begin

            end
            else
            begin
              SfrItem.StringValue := FSplitter[3]
            end;
          end
          else
          begin
            Unhandled.WriteLine(Format(StrUnrecognizedSPERI, [FPackageType]));
            Unhandled.WriteLine(ErrorLine);
          end;
        end
        else if (SfrItem.Name = 'CROSS_SECTION')
          and (FSplitter.Count >= 5)
          and (FSplitter[2] = 'TAB6')
          and (FSplitter[3] = 'FILEIN')
          then
        begin
          FSplitter.DelimitedText := CaseSensitiveLine;
          SfrItem.StringValue := FSplitter[4]
        end
        else if (SfrItem.Name = 'AUXILIARY')
          then
        begin
          if TryFortranStrToFloat(FSplitter[3], SfrItem.FloatValue) then
          begin

          end
          else
          begin
            FSplitter.DelimitedText := CaseSensitiveLine;
            SfrItem.StringValue := FSplitter[3]
          end;
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

{ TSfr }

constructor TSfr.Create(PackageType: string);
begin
  inherited;
  FOptions := TSfrOptions.Create(PackageType);
  FSfrDimensions := TSfrDimensions.Create(PackageType);
  FPackageData := TSfrPackageData.Create(PackageType);
  FSfrCrossSections := TSfrCrossSections.Create(PackageType);
  FConnections := TSfrConnections.Create(PackageType);
  FSfrDiversions := TSfrDiversions.Create(PackageType);
  FPeriods := TSfrPeriodList.Create;
  FTimeSeriesPackages := TPackageList.Create;
  FObservationsPackages := TPackageList.Create;
  FTabFilePackages := TPackageList.Create;

end;

destructor TSfr.Destroy;
begin
  FOptions.Free;
  FSfrDimensions.Free;
  FPackageData.Free;
  FSfrCrossSections.Free;
  FConnections.Free;
  FSfrDiversions.Free;
  FPeriods.Free;
  FTimeSeriesPackages.Free;
  FObservationsPackages.Free;
  FTabFilePackages.Free;
  inherited;
end;

procedure TSfr.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
var
  ALine: string;
  ErrorLine: string;
  IPER: Integer;
  APeriod: TSfrPeriod;
  TsPackage: TPackage;
  PackageIndex: Integer;
  TsReader: TTimeSeries;
  ObsReader: TObs;
  ObsPackage: TPackage;
  CrossSectionPackage: TPackage;
  CrossSectionReader: TCrossSection;
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
        FSfrDimensions.Read(Stream, Unhandled);
      end
      else if FSplitter[1] ='PACKAGEDATA' then
      begin
        FPackageData.Read(Stream, Unhandled, FOptions.AUXILIARY.Count,
          FOptions.BOUNDNAMES, FDimensions);
      end
      else if FSplitter[1] ='CROSSSECTIONS' then
      begin
        FSfrCrossSections.Read(Stream, Unhandled);
      end
      else if FSplitter[1] ='CONNECTIONDATA' then
      begin
        FConnections.Read(Stream, Unhandled, FPackageData.FItems);
      end
      else if FSplitter[1] ='DIVERSIONS' then
      begin
        FSfrDiversions.Read(Stream, Unhandled);
      end
      else if (FSplitter[1] ='PERIOD') and (FSplitter.Count >= 3) then
      begin
        if TryStrToInt(FSplitter[2], IPER) then
        begin
          if IPER > NPER then
          begin
            break;
          end;
          APeriod := TSfrPeriod.Create(FPackageType);
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
  for PackageIndex := 0 to FSfrCrossSections.FItems.Count - 1 do
  begin
    CrossSectionPackage := TPackage.Create;
    FObservationsPackages.Add(CrossSectionPackage);
    CrossSectionPackage.FileType := FPackageType;
    CrossSectionPackage.FileName := FSfrCrossSections.FItems[PackageIndex].tab6_filename;
    CrossSectionPackage.PackageName := '';

    CrossSectionReader := TCrossSection.Create(FPackageType);
    CrossSectionPackage.Package := CrossSectionReader;
    CrossSectionPackage.ReadPackage(Unhandled, NPER);
  end;
end;

end.
