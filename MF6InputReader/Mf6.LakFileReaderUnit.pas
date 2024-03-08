unit Mf6.LakFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TLakOptions = class(TCustomMf6Persistent)
  private
    FAUXILIARY: TStringList;
    BOUNDNAMES: Boolean;
    PRINT_INPUT: Boolean;
    FPRINT_STAGE: Boolean;
    PRINT_FLOWS: Boolean;
    SAVE_FLOWS: Boolean;
    FSTAGE: Boolean;
    FBUDGET: Boolean;
    FBUDGETCSV: Boolean;
    FPACKAGE_CONVERGENCE: Boolean;
    TS6_FileNames: TStringList;
    Obs6_FileNames: TStringList;
    MOVER: Boolean;
    FSURFDEP: TRealOption;
    FMAXIMUM_ITERATIONS: TIntegerOption;
    FMAXIMUM_STAGE_CHANGE: TRealOption;
    LENGTH_CONVERSION: TRealOption;
    TIME_CONVERSION: TRealOption;
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
    property STAGE: Boolean read FSTAGE;
    property PRINT_STAGE: Boolean read FPRINT_STAGE;
    property BUDGET: Boolean read FBUDGET;
    property BUDGETCSV: Boolean read FBUDGETCSV;
    property PACKAGE_CONVERGENCE: Boolean read FPACKAGE_CONVERGENCE;
    property SURFDEP: TRealOption read FSURFDEP;
    property MAXIMUM_ITERATIONS: TIntegerOption read FMAXIMUM_ITERATIONS;
    property MAXIMUM_STAGE_CHANGE: TRealOption read FMAXIMUM_STAGE_CHANGE;
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
    Flakeno: Integer;
    Fstrt: Extended;
    nlakeconn: Integer;
    Faux: TBoundaryValueList;
    Fboundname: string;
    function GetAux(Index: Integer): TMf6BoundaryValue;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property lakeno: Integer read Flakeno;
    property strt: Extended read Fstrt;
    property boundname: string read Fboundname;
    property Count: Integer read GetCount;
    property aux[Index: Integer]: TMf6BoundaryValue read GetAux;
  end;

  TLakPackageItemList= TObjectList<TLakPackageItem>;

  TLakPackageData = class(TCustomMf6Persistent)
  private
    FItems: TLakPackageItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; naux: Integer;
      BOUNDNAMES: Boolean);
    function GetItem(Index: Integer): TLakPackageItem;
    function GetCount: integer;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Count: integer read GetCount;
    property Items[Index: Integer]: TLakPackageItem read GetItem; default;
  end;

  TLakConnectionItem = record
  private
    Flakeno: Integer;
    Ficonn: Integer;
    Fcellid: TMfCellId;
    Fclaktype: string;
    Fbedleak: Extended;
    Fbelev: Extended;
    Ftelev: Extended;
    Fconnlen: Extended;
    Fconnwidth: Extended;
  public
    procedure Initialize;
    property lakeno: Integer read Flakeno;
    property iconn: Integer read Ficonn;
    property cellid: TMfCellId read Fcellid;
    property claktype: string read Fclaktype;
    property bedleak: Extended read Fbedleak;
    property belev: Extended read Fbelev;
    property telev: Extended read Ftelev;
    property connlen: Extended read Fconnlen;
    property connwidth: Extended read Fconnwidth;
  end;

  TLakConnectionItemList = TList<TLakConnectionItem>;

  TLakConnections = class(TCustomMf6Persistent)
  private
    FItems: TLakConnectionItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      Dimensions: TDimensions);
    function GetCount: integer;
    function GetItem(Index: Integer): TLakConnectionItem;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Count: integer read GetCount;
    property Items[Index: Integer]: TLakConnectionItem read GetItem; default;
  end;

  TMf6LakeTableItem = record
  private
    Flakeno: Integer;
    Ftab6_filename: string;
    procedure Initialize;
  public
    property lakeno: Integer read Flakeno;
    property tab6_filename: string read Ftab6_filename;
  end;

  TLakeTableItemList = TList<TMf6LakeTableItem>;

  TLakTables = class(TCustomMf6Persistent)
  private
    FItems: TLakeTableItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    function GetCount: Integer;
    function GetItem(Index: Integer): TMf6LakeTableItem;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TMf6LakeTableItem read GetItem; default;
  end;

  TMf6LakOutletItem = record
  private
    Foutletno: Integer;
    Flakein: Integer;
    Flakeout: Integer;
    Fcouttype: string;
    Finvert: TMf6BoundaryValue;
    Fwidth: TMf6BoundaryValue;
    Frough: TMf6BoundaryValue;
    Fslope: TMf6BoundaryValue;
    procedure Initialize;
  public
    property outletno: Integer read Foutletno;
    property lakein: Integer read Flakein;
    property lakeout: Integer read Flakeout;
    property couttype: string read Fcouttype;
    property invert: TMf6BoundaryValue read Finvert;
    property width: TMf6BoundaryValue read Fwidth;
    property rough: TMf6BoundaryValue read Frough;
    property slope: TMf6BoundaryValue read Fslope;
  end;

  TLakOutletItemList = TList<TMf6LakOutletItem>;

  TLakOutlets = class(TCustomMf6Persistent)
  private
    FItems: TLakOutletItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    function GetCount: integer;
    function GetItem(Index: Integer): TMf6LakOutletItem;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Count: integer read GetCount;
    property Items[Index: Integer]: TMf6LakOutletItem read GetItem; default;
  end;

  TLakPeriod = class(TCustomMf6Persistent)
  private
    IPER: Integer;
    FItems: TNumberedItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    function GetCount: integer;
    function GetItem(Index: Integer): TNumberedItem;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Period: Integer read IPER;
    property Count: integer read GetCount;
    property Items[Index: Integer]: TNumberedItem read GetItem; default;
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
    FTabFileDictionary: TDictionary<string, TPackage>;
    function GetObservation(Index: Integer): TPackage;
    function GetObservationCount: Integer;
    function GetPeriod(Index: Integer): TLakPeriod;
    function GetPeriodCount: Integer;
    function GetTimeSeries(Index: Integer): TPackage;
    function GetTimeSeriesCount: Integer;
    function GetTableCount: Integer;
    function GetTableItem(Index: Integer): TMf6LakeTableItem;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      const NPER: Integer); override;
    property Options: TLakOptions read FOptions;
    property PackageData: TLakPackageData read FPackageData;
    property Connections: TLakConnections read FConnections;
    property LakOutlets: TLakOutlets read FLakOutlets;
    property PeriodCount: Integer read GetPeriodCount;
    property Periods[Index: Integer]: TLakPeriod read GetPeriod;
    property TimeSeriesCount: Integer read GetTimeSeriesCount;
    property TimeSeries[Index: Integer]: TPackage read GetTimeSeries;
    property ObservationCount: Integer read GetObservationCount;
    property Observations[Index: Integer]: TPackage read GetObservation;
    property TableCount: Integer read GetTableCount;
    property Tables[Index: Integer]: TMf6LakeTableItem read GetTableItem;
    function GetTabFilePackage(FileName: string): TPackage;
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
  FAUXILIARY := TStringList.Create;
  FAUXILIARY.CaseSensitive := False;
  TS6_FileNames := TStringList.Create;
  Obs6_FileNames := TStringList.Create;
  inherited;

end;

destructor TLakOptions.Destroy;
begin
  FAUXILIARY.Free;
  TS6_FileNames.Free;
  Obs6_FileNames.Free;
  inherited;
end;

function TLakOptions.GetAUXILIARY(Index: Integer): string;
begin
  result := FAUXILIARY[Index];
end;

function TLakOptions.GetCount: Integer;
begin
  result := FAUXILIARY.Count;
end;

function TLakOptions.IndexOfAUXILIARY(const AName: string): Integer;
begin
  result := FAUXILIARY.IndexOf(AName);
end;

procedure TLakOptions.Initialize;
begin
  inherited;
  FAUXILIARY.Clear;
  BOUNDNAMES := False;
  PRINT_INPUT := False;
  FPRINT_STAGE := False;
  PRINT_FLOWS := False;
  SAVE_FLOWS := False;
  FSTAGE := False;
  FBUDGET := False;
  FBUDGETCSV := False;
  FPACKAGE_CONVERGENCE := False;
  TS6_FileNames.Clear;
  Obs6_FileNames.Clear;
  MOVER := False;
  FSURFDEP.Initialize;
  FMAXIMUM_ITERATIONS.Initialize;
  FMAXIMUM_STAGE_CHANGE.Initialize;
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
        FAUXILIARY.Add(AUXILIARY_Name);
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
      FPRINT_STAGE := True;
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
      FSTAGE := True;
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
    else if (FSplitter[0] = 'PACKAGE_CONVERGENCE')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEOUT') then
    begin
      FPACKAGE_CONVERGENCE := True;
    end
    else if FSplitter[0] = 'MOVER' then
    begin
      MOVER := True;
    end
    else if (FSplitter[0] = 'SURFDEP') and (FSplitter.Count >= 2)
      and TryFortranStrToFloat(FSplitter[1], FSURFDEP.Value) then
    begin
      FSURFDEP.Used := True;
    end
    else if (FSplitter[0] = 'MAXIMUM_ITERATIONS') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], FMAXIMUM_ITERATIONS.Value) then
    begin
      FMAXIMUM_ITERATIONS.Used := True;
    end
    else if (FSplitter[0] = 'MAXIMUM_STAGE_CHANGE') and (FSplitter.Count >= 2)
      and TryFortranStrToFloat(FSplitter[1], FMAXIMUM_STAGE_CHANGE.Value) then
    begin
      FMAXIMUM_STAGE_CHANGE.Used := True;
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
    else if (FSplitter[0] = 'NLAKES') and (FSplitter.Count >= 2)
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
  Flakeno := 0;
  Fstrt := 0;
  nlakeconn := 0;
  Faux := TBoundaryValueList.Create;
  Fboundname := ''
end;

destructor TLakPackageItem.Destroy;
begin
  Faux.Free;
  inherited;
end;

function TLakPackageItem.GetAux(Index: Integer): TMf6BoundaryValue;
begin
  Result := Faux[Index];
end;

function TLakPackageItem.GetCount: Integer;
begin
  Result := Faux.Count;
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

function TLakPackageData.GetCount: integer;
begin
  result := FItems.Count;
end;

function TLakPackageData.GetItem(Index: Integer): TLakPackageItem;
begin
  result := FItems[index]
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
  AValue: TMf6BoundaryValue;
  CaseSensitiveLine: string;
  NumberOfItems: Integer;
begin
  NumberOfItems := 3 + naux;
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
        TComparer<TLakPackageItem>.Construct(
          function(const Left, Right: TLakPackageItem): Integer
          begin
            Result := Left.Flakeno - Right.Flakeno;
          end
        ));
      Exit;
    end;

    CaseSensitiveLine := ALine;
    Item := TLakPackageItem.Create;
    try
      if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'PACKAGEDATA') then
      begin
        // do nothing
      end
      else if (FSplitter.Count >= NumberOfItems)
        and TryStrToInt(FSplitter[0],Item.Flakeno)
        and TryFortranStrToFloat(FSplitter[1],Item.Fstrt)
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


{ TTabFileItem }

procedure TMf6LakeTableItem.Initialize;
begin
  Flakeno := 0;
  Ftab6_filename := '';
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

function TLakTables.GetCount: Integer;
begin
  result := FItems.Count;
end;

function TLakTables.GetItem(Index: Integer): TMf6LakeTableItem;
begin
  result := FItems[Index];
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
  Item: TMf6LakeTableItem;
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

    if ReadEndOfSection(ALine, ErrorLine, 'TABLES', Unhandled) then
    begin
      Exit;
    end;

    Item.Initialize;

    CaseSensitiveLine := ALine;
    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'TABLES') then
    begin
      // do nothing
    end
    else if (FSplitter.Count >= 4)
      and TryStrToInt(FSplitter[0],Item.Flakeno)
      and (FSplitter[1] = 'TAB6')
      and (FSplitter[2] = 'FILEIN')
      then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      Item.Ftab6_filename := FSplitter[3];
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
  Flakeno := 0;
  Ficonn := 0;
  Fcellid.Initialize;
  Fclaktype := '';
  Fbedleak := 3.0E30;
  Fbelev := 0;
  Ftelev := 0;
  Fconnlen := 0;
  Fconnwidth := 0;
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

function TLakConnections.GetCount: integer;
begin
  result := FItems.Count
end;

function TLakConnections.GetItem(Index: Integer): TLakConnectionItem;
begin
  result := FItems[Index];
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

    Item.Initialize;
    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'CONNECTIONDATA') then
    begin
      // do nothing
    end
    else if (FSplitter.Count >= ItemCount)
      and TryStrToInt(FSplitter[0],Item.Flakeno)
      and TryStrToInt(FSplitter[1],Item.Ficonn)
      and ReadCellId(Item.Fcellid, 2, DimensionCount)
      and TryFortranStrToFloat(FSplitter[4+DimensionCount], Item.Fbelev)
      and TryFortranStrToFloat(FSplitter[5+DimensionCount], Item.Ftelev)
      and TryFortranStrToFloat(FSplitter[6+DimensionCount], Item.Fconnlen)
      and TryFortranStrToFloat(FSplitter[7+DimensionCount], Item.Fconnwidth)
      then
    begin
      Item.Fclaktype := FSplitter[2+DimensionCount];
      if AnsiSameText(FSplitter[3+DimensionCount], 'NONE') then
      begin
        Item.Fbedleak := 3E30;
        FItems.Add(Item);
      end
      else
      begin
        if TryFortranStrToFloat(FSplitter[3+DimensionCount], Item.Fbedleak) then
        begin
          FItems.Add(Item);
        end
        else
        begin
          Unhandled.WriteLine(Format(StrUnrecognizedSCONN, [FPackageType]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSCONN, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TLakOutletItem }

procedure TMf6LakOutletItem.Initialize;
begin
  Foutletno := 0;
  Flakein := 0;
  Flakeout := 0;
  Fcouttype := '';
  Finvert.Initialize;
  Fwidth.Initialize;
  Frough.Initialize;
  Fslope.Initialize;
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

function TLakOutlets.GetCount: integer;
begin
  result := FItems.Count;
end;

function TLakOutlets.GetItem(Index: Integer): TMf6LakOutletItem;
begin
  result := FItems[Index];
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
  Item: TMf6LakOutletItem;
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

    if ReadEndOfSection(ALine, ErrorLine, 'OUTLETS', Unhandled) then
    begin
      FItems.Sort(
        TComparer<TMf6LakOutletItem>.Construct(
          function(const Left, Right: TMf6LakOutletItem): Integer
          begin
            Result := Left.Foutletno - Right.Foutletno;
          end
        ));
      Exit;
    end;

    Item.Initialize;
    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'OUTLETS') then
    begin
      // do nothing
    end
    else if (FSplitter.Count >- 4)
      and TryStrToInt(FSplitter[0],Item.Foutletno)
      and TryStrToInt(FSplitter[1],Item.Flakein)
      and TryStrToInt(FSplitter[2],Item.Flakeout)

      then
    begin
      Item.Fcouttype := FSplitter[3];
      if not TryFortranStrToFloat(FSplitter[4],Item.Finvert.NumericValue) then
      begin
        Item.Finvert.StringValue := FSplitter[4];
        Item.Finvert.ValueType := vtString;
      end;
      if not TryFortranStrToFloat(FSplitter[5],Item.Fwidth.NumericValue) then
      begin
        Item.Fwidth.StringValue := FSplitter[5];
        Item.Fwidth.ValueType := vtString;
      end;
      if not TryFortranStrToFloat(FSplitter[6],Item.Frough.NumericValue) then
      begin
        Item.Frough.StringValue := FSplitter[6];
        Item.Frough.ValueType := vtString;
      end;
      if not TryFortranStrToFloat(FSplitter[7],Item.Fslope.NumericValue) then
      begin
        Item.Fslope.StringValue := FSplitter[7];
        Item.Fslope.ValueType := vtString;
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

function TLakPeriod.GetCount: integer;
begin
  result := FItems.Count
end;

function TLakPeriod.GetItem(Index: Integer): TNumberedItem;
begin
  result := FItems[Index];
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
          // Outlets
          or (SfrItem.Name = 'RATE')
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
  FTabFileDictionary := TDictionary<string, TPackage>.Create;

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
  FTabFileDictionary.Free;
  inherited;
end;

function TLak.GetObservation(Index: Integer): TPackage;
begin
  result := FObservationsPackages[Index];
end;

function TLak.GetObservationCount: Integer;
begin
  result := FObservationsPackages.Count;
end;

function TLak.GetPeriod(Index: Integer): TLakPeriod;
begin
  result := FPeriods[Index];
end;

function TLak.GetPeriodCount: Integer;
begin
  result := FPeriods.Count;
end;

function TLak.GetTabFilePackage(FileName: string): TPackage;
begin
  if not FTabFileDictionary.TryGetValue(UpperCase(FileName), result) then
  begin
    result := nil;
  end;
end;

function TLak.GetTableCount: Integer;
begin
  result := FLakTables.Count;
end;

function TLak.GetTableItem(Index: Integer): TMf6LakeTableItem;
begin
  result := FLakTables[Index];
end;

function TLak.GetTimeSeries(Index: Integer): TPackage;
begin
  result := FTimeSeriesPackages[Index];
end;

function TLak.GetTimeSeriesCount: Integer;
begin
  result := FTimeSeriesPackages.Count;
end;

procedure TLak.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  const NPER: Integer);
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
  AFilename: string;
begin
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading LAK package');
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
        FLakDimensions.Read(Stream, Unhandled);
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
          if IPER > NPER then
          begin
            break;
          end;
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
  for PackageIndex := 0 to FLakTables.FItems.Count - 1 do
  begin
    AFilename := FLakTables.FItems[PackageIndex].Ftab6_filename;
    if not FTabFileDictionary.ContainsKey(UpperCase(AFileName)) then
    begin
      LakeTablePackage := TPackage.Create;
      FTabFilePackages.Add(LakeTablePackage);
      LakeTablePackage.FileType := FPackageType;
      LakeTablePackage.FileName := AFilename;
      LakeTablePackage.PackageName := '';

      LakeTableReader := TLakeTable.Create(FPackageType);
      LakeTablePackage.Package := LakeTableReader;
      LakeTablePackage.ReadPackage(Unhandled, NPER);

      FTabFileDictionary.Add(UpperCase(AFileName), LakeTablePackage)
    end;
  end;
end;

end.
