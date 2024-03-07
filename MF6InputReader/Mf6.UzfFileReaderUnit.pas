unit Mf6.UzfFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections, System.Generics.Defaults;

type
  TUzfOptions = class(TCustomMf6Persistent)
  private
    FAUXILIARY: TStringList;
    AUXMULTNAME: string;
    BOUNDNAMES: Boolean;
    PRINT_INPUT: Boolean;
    PRINT_FLOWS: Boolean;
    SAVE_FLOWS: Boolean;
    FWATER_CONTENT: Boolean;
    FBUDGET: Boolean;
    FBUDGETCSV: Boolean;
    FPACKAGE_CONVERGENCE: Boolean;
    TS6: TStringList;
    OBS6: TStringList;
    MOVER: Boolean;
    FSIMULATE_ET: Boolean;
    FLINEAR_GWET: Boolean;
    FSQUARE_GWET: Boolean;
    FSIMULATE_GWSEEP: Boolean;
    FUNSAT_ETWC: Boolean;
    FUNSAT_ETAE: Boolean;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    function GetAUXILIARY(Index: Integer): string;
    function GetCount: Integer;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property AUXILIARY[Index: Integer]: string read GetAUXILIARY; default;
    property WATER_CONTENT: Boolean read FWATER_CONTENT;
    property BUDGET: Boolean read FBUDGET;
    property BUDGETCSV: Boolean read FBUDGETCSV;
    property PACKAGE_CONVERGENCE: Boolean read FPACKAGE_CONVERGENCE;
    property SIMULATE_ET: Boolean read FSIMULATE_ET;
    property LINEAR_GWET: Boolean read FLINEAR_GWET;
    property SQUARE_GWET: Boolean read FSQUARE_GWET;
    property SIMULATE_GWSEEP: Boolean read FSIMULATE_GWSEEP;
    property UNSAT_ETWC: Boolean read FUNSAT_ETWC;
    property UNSAT_ETAE: Boolean read FUNSAT_ETAE;
    function IndexOfAux(AuxName: string): Integer;
  end;

  TUzfDimensions = class(TCustomMf6Persistent)
  private
    FNUZFCELLS: Integer;
    FNTRAILWAVES: Integer;
    FNWAVESETS: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    property NUZFCELLS: Integer read FNUZFCELLS;
    property NTRAILWAVES: Integer read FNTRAILWAVES;
    property NWAVESETS: Integer read FNWAVESETS;
  end;

  TUzfPackageItem = record
  private
    Fiuzno: Integer;
    Fcellid: TMfCellId;
    Flandflag: Integer;
    Fivertcon: Integer;
    Fsurfdep: Extended;
    Fvks: Extended;
    Fthtr: Extended;
    Fthts: Extended;
    Fthti: Extended;
    Feps: Extended;
    Fboundname: string;
    procedure Initialize;
  public
    property iuzno: Integer read Fiuzno;
    property cellid: TMfCellId read Fcellid;
    property landflag: Integer read Flandflag;
    property ivertcon: Integer read Fivertcon;
    property surfdep: Extended read Fsurfdep;
    property vks: Extended read Fvks;
    property thtr: Extended read Fthtr;
    property thts: Extended read Fthts;
    property thti: Extended read Fthti;
    property eps: Extended read Feps;
    property boundname: string read Fboundname;
  end;

  TUzfPackageItemList= TList<TUzfPackageItem>;

  TUzfPackageData = class(TCustomMf6Persistent)
  private
    FItems: TUzfPackageItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      BOUNDNAMES: Boolean; Dimensions: TDimensions);
    function GetCount: Integer;
    function GetItem(Index: Integer): TUzfPackageItem;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TUzfPackageItem read GetItem; default;
  end;

  TUzfPeriodItem = class(TCustomMf6Persistent)
  private
    Fiuzno: Integer;
    Ffinf: TMf6BoundaryValue;
    Fpet: TMf6BoundaryValue;
    Fextdp: TMf6BoundaryValue;
    Fextwc: TMf6BoundaryValue;
    Fha: TMf6BoundaryValue;
    Fhroot: TMf6BoundaryValue;
    Frootact: TMf6BoundaryValue;
    Faux: TBoundaryValueList;
    function GetAux(Index: Integer): TMf6BoundaryValue;
    function GetCount: Integer;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property iuzno: Integer read Fiuzno;
    property finf: TMf6BoundaryValue read Ffinf;
    property pet: TMf6BoundaryValue read Fpet;
    property extdp: TMf6BoundaryValue read Fextdp;
    property extwc: TMf6BoundaryValue read Fextwc;
    property ha: TMf6BoundaryValue read Fha;
    property hroot: TMf6BoundaryValue read Fhroot;
    property rootact: TMf6BoundaryValue read Frootact;
    property Count: Integer read GetCount;
    property Aux[Index: Integer]: TMf6BoundaryValue read GetAux; default;
  end;

  TUzfPeriodItemObjectList = TObjectList<TUzfPeriodItem>;

  TUzfPeriod = class(TCustomMf6Persistent)
  private
    IPER: Integer;
    FItems: TUzfPeriodItemObjectList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; Naux: Integer);
    function GetCount: Integer;
    function GetItem(Index: Integer): TUzfPeriodItem;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Period: Integer read IPER;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TUzfPeriodItem read GetItem; default;
  end;

  TUzfPeriodList = TObjectList<TUzfPeriod>;

  TUzf = class(TDimensionedPackageReader)
  private
    FOptions: TUzfOptions;
    FUzfDimensions: TUzfDimensions;
    FPackageData: TUzfPackageData;
    FPeriods: TUzfPeriodList;
    FTimeSeriesPackages: TPackageList;
    FObservationsPackages: TPackageList;
    function GetPeriod(Index: Integer): TUzfPeriod;
    function GetPeriodCount: Integer;
    function GetTimeSeriesPackage(Index: Integer): TPackage;
    function GetTimeSeriesPackageCount: Integer;
    function GetObservation(Index: Integer): TPackage;
    function GetObservationCount: Integer;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      const NPER: Integer); override;
    property Options: TUzfOptions read FOptions;
    property UzfDimensions: TUzfDimensions read FUzfDimensions;
    property PackageData: TUzfPackageData read FPackageData;
    property PeriodCount: Integer read GetPeriodCount;
    property Periods[Index: Integer]: TUzfPeriod read GetPeriod; default;
    property TimeSeriesPackageCount: Integer read GetTimeSeriesPackageCount;
    property TimeSeriesPackages[Index: Integer]: TPackage read GetTimeSeriesPackage;
    property ObservationCount: Integer read GetObservationCount;
    property Observations[Index: Integer]: TPackage read GetObservation;
  end;

implementation

uses
  ModelMuseUtilities, Mf6.TimeSeriesFileReaderUnit, Mf6.ObsFileReaderUnit;

{ TUzfOptions }

constructor TUzfOptions.Create(PackageType: string);
begin
  FAUXILIARY := TStringList.Create;
  FAUXILIARY.CaseSensitive := False;
  TS6 := TStringList.Create;
  OBS6 := TStringList.Create;
  inherited;

end;

destructor TUzfOptions.Destroy;
begin
  FAUXILIARY.Free;
  TS6.Free;
  OBS6.Free;
  inherited;
end;

function TUzfOptions.GetAUXILIARY(Index: Integer): string;
begin
  Result := FAUXILIARY[Index];
end;

function TUzfOptions.GetCount: Integer;
begin
  Result := FAUXILIARY.Count;
end;

function TUzfOptions.IndexOfAux(AuxName: string): Integer;
begin
  Result := FAUXILIARY.IndexOf(AuxName);
end;

procedure TUzfOptions.Initialize;
begin
  inherited;
  FAUXILIARY.Clear;
  AUXMULTNAME := '';
  BOUNDNAMES := False;
  PRINT_INPUT := False;
  PRINT_FLOWS := False;
  SAVE_FLOWS := False;
  FWATER_CONTENT := False;
  FBUDGET := False;
  FBUDGETCSV := False;
  TS6.Clear;
  OBS6.Clear;
  MOVER := False;
  FSIMULATE_ET := False;
  FLINEAR_GWET := False;
  FSQUARE_GWET := False;
  FSIMULATE_GWSEEP := False;
  FUNSAT_ETWC := False;
  FUNSAT_ETAE := False;
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
        FAUXILIARY.Add(AUXILIARY_Name);
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
      FWATER_CONTENT := True;
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
      FSIMULATE_ET := True;
    end
    else if FSplitter[0] = 'LINEAR_GWET' then
    begin
      FLINEAR_GWET := True;
    end
    else if FSplitter[0] = 'SQUARE_GWET' then
    begin
      FSQUARE_GWET := True;
    end
    else if FSplitter[0] = 'SIMULATE_GWSEEP' then
    begin
      FSIMULATE_GWSEEP := True;
    end
    else if FSplitter[0] = 'UNSAT_ETWC' then
    begin
      FUNSAT_ETWC := True;
    end
    else if FSplitter[0] = 'UNSAT_ETAE' then
    begin
      FUNSAT_ETAE := True;
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
  FNUZFCELLS := 0;
  FNTRAILWAVES := 0;
  FNWAVESETS := 0;

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
      and TryStrToInt(FSplitter[1], FNUZFCELLS) then
    begin
    end
    else if (FSplitter[0] = 'NTRAILWAVES') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], FNTRAILWAVES) then
    begin
    end
    else if (FSplitter[0] = 'NWAVESETS') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], FNWAVESETS) then
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
  Fiuzno := 0;
  Fcellid.Initialize;
  Flandflag := 0;
  Fivertcon := 0;
  Fsurfdep := 0;
  Fvks := 0;
  Fthtr := 0;
  Fthts := 0;
  Fthti := 0;
  Feps := 0;
  Fboundname := '';
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

function TUzfPackageData.GetCount: Integer;
begin
  result := FItems.Count;
end;

function TUzfPackageData.GetItem(Index: Integer): TUzfPackageItem;
begin
  result := FItems[Index];
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
            Result := Left.Fiuzno - Right.Fiuzno;
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
      and TryStrToInt(FSplitter[0],Item.Fiuzno)
      and ReadCellID(Item.Fcellid, 1, DimensionCount)
      and TryStrToInt(FSplitter[1+DimensionCount],Item.Flandflag)
      and TryStrToInt(FSplitter[2+DimensionCount],Item.Fivertcon)
      and TryFortranStrToFloat(FSplitter[3+DimensionCount],Item.Fsurfdep)
      and TryFortranStrToFloat(FSplitter[4+DimensionCount],Item.Fvks)
      and TryFortranStrToFloat(FSplitter[5+DimensionCount],Item.Fthtr)
      and TryFortranStrToFloat(FSplitter[6+DimensionCount],Item.Fthts)
      and TryFortranStrToFloat(FSplitter[7+DimensionCount],Item.Fthti)
      and TryFortranStrToFloat(FSplitter[8+DimensionCount],Item.Feps)
      then
    begin
      ItemStart := 9+DimensionCount;
      if BOUNDNAMES and (FSplitter.Count >= NumberOfItems+1) then
      begin
        FSplitter.DelimitedText := CaseSensitiveLine;
        Item.Fboundname := FSplitter[ItemStart];
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
  Faux := TBoundaryValueList.Create;
  inherited;
end;

destructor TUzfPeriodItem.Destroy;
begin
  Faux.Free;
  inherited;
end;

function TUzfPeriodItem.GetAux(Index: Integer): TMf6BoundaryValue;
begin
  result := Faux[Index];
end;

function TUzfPeriodItem.GetCount: Integer;
begin
  result := Faux.Count;
end;

procedure TUzfPeriodItem.Initialize;
begin
  inherited;
  Fiuzno := 0;
  Ffinf.Initialize;
  Fpet.Initialize;
  Fextdp.Initialize;
  Fextwc.Initialize;
  Fha.Initialize;
  Fhroot.Initialize;
  Frootact.Initialize;
  Faux.Clear;
end;

{ TUzfPeriod }

constructor TUzfPeriod.Create(PackageType: string);
begin
  FItems := TUzfPeriodItemObjectList.Create;
  inherited;

end;

destructor TUzfPeriod.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TUzfPeriod.GetCount: Integer;
begin
  result := FItems.Count;
end;

function TUzfPeriod.GetItem(Index: Integer): TUzfPeriodItem;
begin
  result := FItems[Index];
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
        if TryStrToInt(FSplitter[0], UzfItem.Fiuzno) then
        begin
          if not TryFortranStrToFloat(FSplitter[1], UzfItem.Ffinf.NumericValue) then
          begin
            UzfItem.Ffinf.ValueType := vtString;
            UzfItem.Ffinf.StringValue := FSplitter[1]
          end;
          if not TryFortranStrToFloat(FSplitter[2], UzfItem.Fpet.NumericValue) then
          begin
            UzfItem.Fpet.ValueType := vtString;
            UzfItem.Fpet.StringValue := FSplitter[2]
          end;
          if not TryFortranStrToFloat(FSplitter[3], UzfItem.Fextdp.NumericValue) then
          begin
            UzfItem.Fextdp.ValueType := vtString;
            UzfItem.Fextdp.StringValue := FSplitter[3]
          end;
          if not TryFortranStrToFloat(FSplitter[4], UzfItem.Fextwc.NumericValue) then
          begin
            UzfItem.Fextwc.ValueType := vtString;
            UzfItem.Fextwc.StringValue := FSplitter[4]
          end;
          if not TryFortranStrToFloat(FSplitter[5], UzfItem.Fha.NumericValue) then
          begin
            UzfItem.Fha.ValueType := vtString;
            UzfItem.Fha.StringValue := FSplitter[5]
          end;
          if not TryFortranStrToFloat(FSplitter[6], UzfItem.Fhroot.NumericValue) then
          begin
            UzfItem.Fhroot.ValueType := vtString;
            UzfItem.Fhroot.StringValue := FSplitter[6]
          end;
          if not TryFortranStrToFloat(FSplitter[7], UzfItem.Frootact.NumericValue) then
          begin
            UzfItem.Frootact.ValueType := vtString;
            UzfItem.Frootact.StringValue := FSplitter[7]
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
            UzfItem.Faux.Add(AuxValue);
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

function TUzf.GetObservation(Index: Integer): TPackage;
begin
  result := FObservationsPackages[Index];
end;

function TUzf.GetObservationCount: Integer;
begin
  result := FObservationsPackages.Count;
end;

function TUzf.GetPeriod(Index: Integer): TUzfPeriod;
begin
  result := FPeriods[Index];
end;

function TUzf.GetPeriodCount: Integer;
begin
  result := FPeriods.Count;
end;

function TUzf.GetTimeSeriesPackage(Index: Integer): TPackage;
begin
  result := FTimeSeriesPackages[Index];
end;

function TUzf.GetTimeSeriesPackageCount: Integer;
begin
  result := FTimeSeriesPackages.Count;
end;

procedure TUzf.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
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
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading UZF package');
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
          if IPER > NPER then
          begin
            break;
          end;
          APeriod := TUzfPeriod.Create(FPackageType);
          FPeriods.Add(APeriod);
          APeriod.IPer := IPER;
          APeriod.Read(Stream, Unhandled, FOptions.FAUXILIARY.Count);
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
    TsPackage.ReadPackage(Unhandled, NPER);
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
    ObsPackage.ReadPackage(Unhandled, NPER);
  end;
end;

end.
