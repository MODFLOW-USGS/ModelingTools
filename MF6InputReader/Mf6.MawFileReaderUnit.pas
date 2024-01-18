unit Mf6.MawFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections, System.Generics.Defaults;

type
  TMawOptions = class(TCustomMf6Persistent)
  private
    FAUXILIARY: TStringList;
    BOUNDNAMES: Boolean;
    PRINT_INPUT: Boolean;
    FPRINT_HEAD: Boolean;
    PRINT_FLOWS: Boolean;
    SAVE_FLOWS: Boolean;
    FHEAD: Boolean;
    FBUDGET: Boolean;
    FBUDGETCSV: Boolean;
    FNO_WELL_STORAGE: Boolean;
    FFLOW_CORRECTION: Boolean;
    FFLOWING_WELLS: Boolean;
    FSHUTDOWN_THETA: TRealOption;
    FSHUTDOWN_KAPPA: TRealOption;
    FMAW_FLOW_REDUCE_CSV: Boolean;
    TS6_FileNames: TStringList;
    Obs6_FileNames: TStringList;
    MOVER: Boolean;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    function GetAUXILIARY(Index: Integer): string;
    function GetCount: Integer;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property AUXILIARY[Index: Integer]: string read GetAUXILIARY;
    function IndexOfAUXILIARY(const AName: string): Integer;
    property HEAD: Boolean read FHEAD;
    property PRINT_HEAD: Boolean read FPRINT_HEAD;
    property BUDGET: Boolean read FBUDGET;
    property BUDGETCSV: Boolean read FBUDGETCSV;
    property NO_WELL_STORAGE: Boolean read FNO_WELL_STORAGE;
    property FLOW_CORRECTION: Boolean read FFLOW_CORRECTION;
    property FLOWING_WELLS: Boolean read FFLOWING_WELLS;
    property SHUTDOWN_THETA: TRealOption read FSHUTDOWN_THETA;
    property SHUTDOWN_KAPPA: TRealOption read FSHUTDOWN_KAPPA;
    property MAW_FLOW_REDUCE_CSV: Boolean read FMAW_FLOW_REDUCE_CSV;
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
    Fwellno: Integer;
    Fradius: Extended;
    Fbottom: Extended;
    Fstrt: Extended;
    Fcondeqn: string;
    Fngwfnodes: Integer;
    Faux: TBoundaryValueList;
    Fboundname: string;
    function GetAux(Index: Integer): TMf6BoundaryValue;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property wellno: Integer read Fwellno;
    property radius: Extended read Fradius;
    property bottom: Extended read Fbottom;
    property strt: Extended read Fstrt;
    property condeqn: string read Fcondeqn;
    property ngwfnodes: Integer read Fngwfnodes;
    property Count: Integer read GetCount;
    property Aux[Index: Integer]: TMf6BoundaryValue read GetAux;
    property Boundname: string read Fboundname;
  end;

  TMawPackageItemList = Class(TObjectList<TMawPackageItem>)
    procedure Sort;
  End;

  TMawPackageData = class(TCustomMf6Persistent)
  private
    FItems: TMawPackageItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; naux: Integer;
      BOUNDNAMES: Boolean);
    function GetCount: Integer;
    function GetItem(Index: Integer): TMawPackageItem;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TMawPackageItem read GetItem; default;
  end;

  TMawConnectionItem = record
  private
    Fwellno: Integer;
    Ficon: Integer;
    Fcellid: TCellId;
    Fscrn_top: Extended;
    Fscrn_bot: Extended;
    Fhk_skin: Extended;
    Fradius_skin: Extended;
    procedure Initialize;
  public
    property wellno: Integer read Fwellno;
    property icon: Integer read Ficon;
    property CellID: TCellId read Fcellid;
    property scrn_top: Extended read Fscrn_top;
    property scrn_bot: Extended read Fscrn_bot;
    property hk_skin: Extended read Fhk_skin;
    property radius_skin: Extended read Fradius_skin;
  end;

  TMawConnectionItemList = class(TList<TMawConnectionItem>)
    procedure Sort;
  end;

  TMawConnectionData = class(TCustomMf6Persistent)
  private
    FItems: TMawConnectionItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      Dimensions: TDimensions);
    function GetCount: Integer;
    function GetItem(Index: Integer): TMawConnectionItem;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TMawConnectionItem read GetItem; default;
  end;

  TMawPeriod = class(TCustomMf6Persistent)
  private
    IPER: Integer;
    FItems: TNumberedItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    function GetCount: Integer;
    function GetItem(Index: Integer): TNumberedItem;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Period: Integer read IPER;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TNumberedItem read GetItem; default;
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
    function GetObservation(Index: Integer): TPackage;
    function GetObservationCount: Integer;
    function GetPeriod(Index: Integer): TMawPeriod;
    function GetPeriodCount: Integer;
    function GetTimeSeries(Index: Integer): TPackage;
    function GetTimeSeriesCount: Integer;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
    property Options: TMawOptions read FOptions;
    property Connections: TMawConnectionData read FConnections;
    property PackageData: TMawPackageData read FPackageData;
    property PeriodCount: Integer read GetPeriodCount;
    property Periods[Index: Integer]: TMawPeriod read GetPeriod;
    property TimeSeriesCount: Integer read GetTimeSeriesCount;
    property TimeSeries[Index: Integer]: TPackage read GetTimeSeries;
    property ObservationCount: Integer read GetObservationCount;
    property Observations[Index: Integer]: TPackage read GetObservation;
  end;

implementation

uses
  ModelMuseUtilities, Mf6.TimeSeriesFileReaderUnit, Mf6.ObsFileReaderUnit;

{ TMawOptions }

constructor TMawOptions.Create(PackageType: string);
begin
  FAUXILIARY := TStringList.Create;
  FAUXILIARY.CaseSensitive := False;
  TS6_FileNames := TStringList.Create;
  Obs6_FileNames := TStringList.Create;
  inherited;
end;

destructor TMawOptions.Destroy;
begin
  FAUXILIARY.Free;
  TS6_FileNames.Free;
  Obs6_FileNames.Free;
  inherited;
end;

function TMawOptions.GetAUXILIARY(Index: Integer): string;
begin
  result := FAUXILIARY[Index];
end;

function TMawOptions.GetCount: Integer;
begin
  result := FAUXILIARY.Count;
end;

function TMawOptions.IndexOfAUXILIARY(const AName: string): Integer;
begin
  result := FAUXILIARY.IndexOf(AName)
end;

procedure TMawOptions.Initialize;
begin
  inherited;
  FAUXILIARY.Clear;
  BOUNDNAMES := False;
  PRINT_INPUT := False;
  FPRINT_HEAD := False;
  PRINT_FLOWS := False;
  SAVE_FLOWS := False;
  FHEAD := False;
  FBUDGET := False;
  FBUDGETCSV := False;
  FNO_WELL_STORAGE := False;
  FFLOW_CORRECTION := False;
  FFLOWING_WELLS := False;
  FSHUTDOWN_THETA.Initialize;
  FSHUTDOWN_KAPPA.Initialize;
  FMAW_FLOW_REDUCE_CSV := False;
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
      FPRINT_HEAD := True;
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
      FHEAD := True;
    end
    else if (FSplitter[0] = 'BUDGET')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEOUT') then
    begin
      FBUDGET := True;
    end
    else if (FSplitter[0] = 'BUDGETCSV')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEOUT') then
    begin
      FBUDGETCSV := True;
    end
    else if FSplitter[0] = 'NO_WELL_STORAGE' then
    begin
      FNO_WELL_STORAGE := True;
    end
    else if FSplitter[0] = 'FLOW_CORRECTION' then
    begin
      FFLOW_CORRECTION := True;
    end
    else if FSplitter[0] = 'FLOWING_WELLS' then
    begin
      FFLOWING_WELLS := True;
    end
    else if (FSplitter[0] = 'SHUTDOWN_THETA') and (FSplitter.Count >= 2)
      and TryFortranStrToFloat(FSplitter[1], FSHUTDOWN_THETA.Value) then
    begin
      FSHUTDOWN_THETA.Used := True;
    end
    else if (FSplitter[0] = 'SHUTDOWN_KAPPA') and (FSplitter.Count >= 2)
      and TryFortranStrToFloat(FSplitter[1], FSHUTDOWN_KAPPA.Value) then
    begin
      FSHUTDOWN_KAPPA.Used := True;
    end
    else if (FSplitter[0] = 'MAW_FLOW_REDUCE_CSV')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEOUT') then
    begin
      FMAW_FLOW_REDUCE_CSV := True;
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
  Faux := TBoundaryValueList.Create;
  Fwellno := 0;
  Fradius := 0;
  Fbottom := 0;
  Fstrt := 0;
  Fcondeqn := '';
  Fngwfnodes := 0;
  Fboundname := '';
end;

destructor TMawPackageItem.Destroy;
begin
  Faux.Free;
  inherited;
end;

function TMawPackageItem.GetAux(Index: Integer): TMf6BoundaryValue;
begin
  result := Faux[Index];
end;

function TMawPackageItem.GetCount: Integer;
begin
  result := Faux.Count;
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

function TMawPackageData.GetCount: Integer;
begin
  result := FItems.Count;
end;

function TMawPackageData.GetItem(Index: Integer): TMawPackageItem;
begin
  result := FItems[Index];
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
  AValue: TMf6BoundaryValue;
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
      FItems.Sort;
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
        and TryStrToInt(FSplitter[0],Item.Fwellno)
        and TryFortranStrToFloat(FSplitter[1],Item.Fradius)
        and TryFortranStrToFloat(FSplitter[2],Item.Fbottom)
        and TryFortranStrToFloat(FSplitter[3],Item.Fstrt)
        and TryStrToInt(FSplitter[5],Item.Fngwfnodes)
        then
      begin
        Item.Fcondeqn := FSplitter[4];
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
          Item.Faux.Add(AVAlue);
          Inc(ItemStart);
        end;
        if BOUNDNAMES and (FSplitter.Count >= NumberOfItems+1) then
        begin
          Item.Fboundname := FSplitter[ItemStart];
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

function TMawConnectionData.GetCount: Integer;
begin
  result := FItems.Count;
end;

function TMawConnectionData.GetItem(Index: Integer): TMawConnectionItem;
begin
  result := FItems[Index];
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
      FItems.Sort;
      Exit;
    end;

    CaseSensitiveLine := ALine;
    Item.Initialize;;
    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'CONNECTIONDATA') then
    begin
      // do nothing
    end
    else if (FSplitter.Count >= 6 + DimensionCount)
      and TryStrToInt(FSplitter[0],Item.Fwellno)
      and TryStrToInt(FSplitter[1],Item.Ficon)
      and ReadCellID(Item.Fcellid, 2, DimensionCount)
      and TryFortranStrToFloat(FSplitter[2+DimensionCount],Item.Fscrn_top)
      and TryFortranStrToFloat(FSplitter[3+DimensionCount],Item.Fscrn_bot)
      and TryFortranStrToFloat(FSplitter[4+DimensionCount],Item.Fhk_skin)
      and TryFortranStrToFloat(FSplitter[5+DimensionCount],Item.Fradius_skin)
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
  Fwellno := 0;
  Ficon := 0;
  Fcellid.Initialize;
  Fscrn_top := 0;
  Fscrn_bot := 0;
  Fhk_skin := 0;
  Fradius_skin := 0;
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

function TMawPeriod.GetCount: Integer;
begin
  result := FItems.Count;
end;

function TMawPeriod.GetItem(Index: Integer): TNumberedItem;
begin
  result := FItems[Index];
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
      FItems.Sort;
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

function TMaw.GetObservation(Index: Integer): TPackage;
begin
  result := FObservationsPackages[Index];
end;

function TMaw.GetObservationCount: Integer;
begin
  result := FObservationsPackages.Count;
end;

function TMaw.GetPeriod(Index: Integer): TMawPeriod;
begin
  result := FPeriods[Index];
end;

function TMaw.GetPeriodCount: Integer;
begin
  result := FPeriods.Count;
end;

function TMaw.GetTimeSeries(Index: Integer): TPackage;
begin
  result := FTimeSeriesPackages[Index];
end;

function TMaw.GetTimeSeriesCount: Integer;
begin
  result := FTimeSeriesPackages.Count;
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
        FPackageData.Read(Stream, Unhandled, FOptions.FAUXILIARY.Count,
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

{ TMawPackageItemList }

procedure TMawPackageItemList.Sort;
begin
  inherited Sort(
    TComparer<TMawPackageItem>.Construct(
      function(const Left, Right: TMawPackageItem): Integer
      begin
        result := Left.Fwellno - Right.Fwellno;
      end
    ));
end;

{ TMawConnectionItemList }

procedure TMawConnectionItemList.Sort;
begin
  inherited Sort(
    TComparer<TMawConnectionItem>.Construct(
      function(const Left, Right: TMawConnectionItem): Integer
      begin
        result := Left.Fwellno - Right.Fwellno;
        if result = 0 then
        begin
          result := Left.Ficon - Right.Ficon;
        end;
      end
    ));
end;

end.
