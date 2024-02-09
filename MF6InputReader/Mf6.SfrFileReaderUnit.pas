unit Mf6.SfrFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TSfrOptions = class(TCustomMf6Persistent)
  private
    FAUXILIARY: TStringList;
    BOUNDNAMES: Boolean;
    PRINT_INPUT: Boolean;
    FPRINT_STAGE: Boolean;
    FPRINT_FLOWS: Boolean;
    FSAVE_FLOWS: Boolean;
    FSTAGE: Boolean;
    FBUDGET: Boolean;
    FBUDGETCSV: Boolean;
    FPACKAGE_CONVERGENCE: Boolean;
    TS6_FileNames: TStringList;
    Obs6_FileNames: TStringList;
    MOVER: Boolean;
    FMAXIMUM_PICARD_ITERATIONS: TIntegerOption;
    FMAXIMUM_ITERATIONS: TIntegerOption;
    FLENGTH_CONVERSION: TRealOption;
    FTIME_CONVERSION: TRealOption;
    UNIT_CONVERSION: TRealOption;
    FMAXIMUM_DEPTH_CHANGE: TRealOption;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property PRINT_STAGE: Boolean read FPRINT_STAGE;
    property PRINT_FLOWS: Boolean read FPRINT_FLOWS;
    property SAVE_FLOWS: Boolean read FSAVE_FLOWS;
    property STAGE: Boolean read FSTAGE;
    property BUDGET: Boolean read FBUDGET;
    property BUDGETCSV: Boolean read FBUDGETCSV;
    property PACKAGE_CONVERGENCE: Boolean read FPACKAGE_CONVERGENCE;
    property MAXIMUM_PICARD_ITERATIONS: TIntegerOption read FMAXIMUM_PICARD_ITERATIONS;
    property MAXIMUM_ITERATIONS: TIntegerOption read FMAXIMUM_ITERATIONS;
    property LENGTH_CONVERSION: TRealOption read FLENGTH_CONVERSION;
    property TIME_CONVERSION: TRealOption read FTIME_CONVERSION;
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
    Frno: Integer;
    Fcellid: TCellId;
    Frlen: Extended;
    Frwid: Extended;
    Frgrd: Extended;
    Frtp: Extended;
    Frbth: Extended;
    Frhk: Extended;
    Fman: TMf6BoundaryValue;
    Fncon: Integer;
    Fustrf: TMf6BoundaryValue;
    Fndv: Integer;
    Faux: TBoundaryValueList;
    Fboundname: string;
    function GetAux(Index: Integer): TMf6BoundaryValue;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property rno: Integer read Frno;
    property cellid: TCellId read Fcellid;
    property rlen: Extended read Frlen;
    property rwid: Extended read Frwid;
    property rgrd: Extended read Frgrd;
    property rtp: Extended read Frtp;
    property rbth: Extended read Frbth;
    property rhk: Extended read Frhk;
    property man: TMf6BoundaryValue read Fman;
    property ncon: Integer read Fncon;
    property ustrf: TMf6BoundaryValue read Fustrf;
    property ndv: Integer read Fndv;
    property Count: Integer read GetCount;
    property Aux[Index: Integer]: TMf6BoundaryValue read GetAux;
    property Boundname: string read Fboundname;


  end;

  TSfrPackageItemList= TList<TSfrPackageItem>;
  TSfrPackageItemObjectList= TObjectList<TSfrPackageItem>;

  TSfrPackageData = class(TCustomMf6Persistent)
  private
    FItems: TSfrPackageItemObjectList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; naux: Integer;
      BOUNDNAMES: Boolean; Dimensions: TDimensions);
    function GetCount: Integer;
    function GetItem(Index: Integer): TSfrPackageItem;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TSfrPackageItem read GetItem; default;
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
    function GetCount: Integer;
    function GetItem(index: Integer): TCrossSectionItem;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[index: Integer]: TCrossSectionItem read GetItem; default;
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
      PackageItems: TSfrPackageItemObjectList);
    function GetCount: Integer;
    function GetItem(index: Integer): TSfrConnectionItem;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[index: Integer]: TSfrConnectionItem read GetItem; default;
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
    function GetCount: Integer;
    function GetItem(index: Integer): TSfrDiversionItem;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[index: Integer]: TSfrDiversionItem read GetItem; default;
  end;

  TSfrPeriod = class(TCustomMf6Persistent)
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
    FCosssSectionDictionary: TDictionary<string, TPackage>;
    FCosssSectionPackages: TPackageList;
    function GetObservation(Index: Integer): TPackage;
    function GetObservationCount: Integer;
    function GetPeriod(Index: Integer): TSfrPeriod;
    function GetPeriodCount: Integer;
    function GetTimeSeries(Index: Integer): TPackage;
    function GetTimeSeriesCount: Integer;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer); override;
    property Options: TSfrOptions read FOptions;
    property Connections: TSfrConnections read FConnections;
    property PackageData: TSfrPackageData read FPackageData;
    property PeriodCount: Integer read GetPeriodCount;
    property Periods[Index: Integer]: TSfrPeriod read GetPeriod;
    property TimeSeriesCount: Integer read GetTimeSeriesCount;
    property TimeSeries[Index: Integer]: TPackage read GetTimeSeries;
    property ObservationCount: Integer read GetObservationCount;
    property Observations[Index: Integer]: TPackage read GetObservation;
    function GetCrossSection(FileName: string): TPackage;
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
  FAUXILIARY := TStringList.Create;
  FAUXILIARY.CaseSensitive := False;
  TS6_FileNames := TStringList.Create;
  Obs6_FileNames := TStringList.Create;
  inherited;

end;

destructor TSfrOptions.Destroy;
begin
  FAUXILIARY.Free;
  TS6_FileNames.Free;
  Obs6_FileNames.Free;
  inherited;
end;

procedure TSfrOptions.Initialize;
begin
  inherited;
  FAUXILIARY.Clear;
  BOUNDNAMES := False;
  PRINT_INPUT := False;
  FPRINT_STAGE := False;
  FPRINT_FLOWS := False;
  FSAVE_FLOWS := False;
  FSTAGE := False;
  FBUDGET := False;
  FBUDGETCSV := False;
  FPACKAGE_CONVERGENCE := False;
  TS6_FileNames.Clear;
  Obs6_FileNames.Clear;
  MOVER := False;
  FMAXIMUM_PICARD_ITERATIONS.Initialize;
  FMAXIMUM_ITERATIONS.Initialize;
  FLENGTH_CONVERSION.Initialize;
  FTIME_CONVERSION.Initialize;
  FMAXIMUM_DEPTH_CHANGE.Initialize;
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
      FPRINT_FLOWS := True;
    end
    else if FSplitter[0] = 'SAVE_FLOWS' then
    begin
      FSAVE_FLOWS := True;
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
    else if (FSplitter[0] = 'MAXIMUM_PICARD_ITERATIONS') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], FMAXIMUM_PICARD_ITERATIONS.Value) then
    begin
      FMAXIMUM_PICARD_ITERATIONS.Used := True;
    end
    else if (FSplitter[0] = 'MAXIMUM_ITERATIONS') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], FMAXIMUM_ITERATIONS.Value) then
    begin
      FMAXIMUM_ITERATIONS.Used := True;
    end
    else if (FSplitter[0] = 'MAXIMUM_DEPTH_CHANGE') and (FSplitter.Count >= 2)
      and TryFortranStrToFloat(FSplitter[1], FMAXIMUM_DEPTH_CHANGE.Value) then
    begin
      FMAXIMUM_DEPTH_CHANGE.Used := True;
    end
    else if (FSplitter[0] = 'LENGTH_CONVERSION') and (FSplitter.Count >= 2)
      and TryFortranStrToFloat(FSplitter[1], FLENGTH_CONVERSION.Value) then
    begin
      FLENGTH_CONVERSION.Used := True;
    end
    else if (FSplitter[0] = 'TIME_CONVERSION') and (FSplitter.Count >= 2)
      and TryFortranStrToFloat(FSplitter[1], FTIME_CONVERSION.Value) then
    begin
      FTIME_CONVERSION.Used := True;
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
  Frno := 0;
  Fcellid.Initialize;
  Frlen := 0;
  Frwid := 0;
  Frgrd := 0;
  Frtp := 0;
  Frbth := 0;
  Frhk := 0;
  Fman.Initialize;
  Fncon := 0;
  Fustrf.Initialize;
  Fndv := 0;
  Faux := TBoundaryValueList.Create;
  Fboundname := ''
end;

destructor TSfrPackageItem.Destroy;
begin
  Faux.Free;
  inherited;
end;

function TSfrPackageItem.GetAux(Index: Integer): TMf6BoundaryValue;
begin
  result := Faux[Index];
end;

function TSfrPackageItem.GetCount: Integer;
begin
  result := Faux.Count;
end;

{ TSfrPackageData }

constructor TSfrPackageData.Create(PackageType: string);
begin
  FItems := TSfrPackageItemObjectList.Create;
  inherited;

end;

destructor TSfrPackageData.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TSfrPackageData.GetCount: Integer;
begin
  result := FItems.Count;
end;

function TSfrPackageData.GetItem(Index: Integer): TSfrPackageItem;
begin
  result := FItems[Index];
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
            Result := Left.Frno - Right.Frno;
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
        if TryStrToInt(FSplitter[0],Item.Frno)
          and TryFortranStrToFloat(FSplitter[2],Item.Frlen)
          and TryFortranStrToFloat(FSplitter[3],Item.Frwid)
          and TryFortranStrToFloat(FSplitter[4],Item.Frgrd)
          and TryFortranStrToFloat(FSplitter[5],Item.Frtp)
          and TryFortranStrToFloat(FSplitter[6],Item.Frbth)
          and TryFortranStrToFloat(FSplitter[7],Item.Frhk)
          and TryStrToInt(FSplitter[9],Item.Fncon)
          and TryStrToInt(FSplitter[11],Item.Fndv)
          then
        begin
          Item.Fcellid.Layer := -1;
          Item.Fcellid.Row := -1;
          Item.Fcellid.Column := -1;
          if TryFortranStrToFloat(FSplitter[8],Item.Fman.NumericValue) then
          begin
            Item.Fman.ValueType := vtNumeric;
          end
          else
          begin
            Item.Fman.ValueType := vtString;
            Item.Fman.StringValue := FSplitter[8];
          end;
          if TryFortranStrToFloat(FSplitter[10],Item.Fustrf.NumericValue) then
          begin
            Item.Fustrf.ValueType := vtNumeric;
          end
          else
          begin
            Item.Fustrf.ValueType := vtString;
            Item.Fustrf.StringValue := FSplitter[10];
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
            Item.Faux.Add(AVAlue);
            Inc(ItemStart);
          end;
          if BOUNDNAMES and (FSplitter.Count >= NumberOfItems+1) then
          begin
            FSplitter.DelimitedText := CaseSensitiveLine;
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
      end
      else if (FSplitter.Count >= NumberOfItems)
        and TryStrToInt(FSplitter[0],Item.Frno)
        and ReadCellID(Item.Fcellid, 1, DimensionCount)
        and TryFortranStrToFloat(FSplitter[DimensionCount+1],Item.Frlen)
        and TryFortranStrToFloat(FSplitter[DimensionCount+2],Item.Frwid)
        and TryFortranStrToFloat(FSplitter[DimensionCount+3],Item.Frgrd)
        and TryFortranStrToFloat(FSplitter[DimensionCount+4],Item.Frtp)
        and TryFortranStrToFloat(FSplitter[DimensionCount+5],Item.Frbth)
        and TryFortranStrToFloat(FSplitter[DimensionCount+6],Item.Frhk)
        and TryStrToInt(FSplitter[DimensionCount+8],Item.Fncon)
        and TryStrToInt(FSplitter[DimensionCount+10],Item.Fndv)
        then
      begin
        if TryFortranStrToFloat(FSplitter[DimensionCount+7],Item.Fman.NumericValue) then
        begin
          Item.Fman.ValueType := vtNumeric;
        end
        else
        begin
          Item.Fman.ValueType := vtString;
          Item.Fman.StringValue := FSplitter[DimensionCount+7];
        end;
        if TryFortranStrToFloat(FSplitter[DimensionCount+9],Item.Fustrf.NumericValue) then
        begin
          Item.Fustrf.ValueType := vtNumeric;
        end
        else
        begin
          Item.Fustrf.ValueType := vtString;
          Item.Fustrf.StringValue := FSplitter[DimensionCount+7];
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
          Item.Faux.Add(AVAlue);
          Inc(ItemStart);
        end;
        if BOUNDNAMES and (FSplitter.Count >= NumberOfItems+1) then
        begin
          FSplitter.DelimitedText := CaseSensitiveLine;
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

function TSfrCrossSections.GetCount: Integer;
begin
  result := FItems.Count;
end;

function TSfrCrossSections.GetItem(index: Integer): TCrossSectionItem;
begin
  result := FItems[Index];
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

function TSfrConnections.GetCount: Integer;
begin
  result := FItems.Count;
end;

function TSfrConnections.GetItem(index: Integer): TSfrConnectionItem;
begin
  result := FItems[index];
end;

procedure TSfrConnections.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TSfrConnections.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  PackageItems: TSfrPackageItemObjectList);
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
        ncon := PackageItems[Item.rno-1].Fncon;
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

function TSfrDiversions.GetCount: Integer;
begin
  result := FItems.Count;
end;

function TSfrDiversions.GetItem(index: Integer): TSfrDiversionItem;
begin
  result := FItems[index];
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

function TSfrPeriod.GetCount: Integer;
begin
  result := FItems.Count;
end;

function TSfrPeriod.GetItem(Index: Integer): TNumberedItem;
begin
  result := FItems[index];
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
  FCosssSectionDictionary := TDictionary<string, TPackage>.Create;
  FCosssSectionPackages := TPackageList.Create;
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
  FCosssSectionDictionary.Free;
  FCosssSectionPackages.Free;
  inherited;
end;

function TSfr.GetCrossSection(FileName: string): TPackage;
begin
  if not FCosssSectionDictionary.TryGetValue(FileName, result) then
  begin
    result := nil;
  end;
end;

function TSfr.GetObservation(Index: Integer): TPackage;
begin
  result := FObservationsPackages[Index];
end;

function TSfr.GetObservationCount: Integer;
begin
  result := FObservationsPackages.Count;
end;

function TSfr.GetPeriod(Index: Integer): TSfrPeriod;
begin
  result := FPeriods[Index];
end;

function TSfr.GetPeriodCount: Integer;
begin
  result := FPeriods.Count;
end;

function TSfr.GetTimeSeries(Index: Integer): TPackage;
begin
  result := FTimeSeriesPackages[Index];
end;

function TSfr.GetTimeSeriesCount: Integer;
begin
  result := FTimeSeriesPackages.Count;
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
  CrossFileNames: TStringList;
  AFileName: string;
begin
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading SFR package');
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
        FSfrDimensions.Read(Stream, Unhandled);
      end
      else if FSplitter[1] ='PACKAGEDATA' then
      begin
        FPackageData.Read(Stream, Unhandled, FOptions.FAUXILIARY.Count,
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
  CrossFileNames := TStringList.Create;
  try
    for PackageIndex := 0 to FSfrCrossSections.FItems.Count - 1 do
    begin
      AFileName := FSfrCrossSections.FItems[PackageIndex].tab6_filename;
      if CrossFileNames.IndexOf(AFileName) < 0 then
      begin
        CrossFileNames.Add(AFileName);
        CrossSectionPackage := TPackage.Create;
        FCosssSectionPackages.Add(CrossSectionPackage);
        FCosssSectionDictionary.Add(AFileName, CrossSectionPackage);
        CrossSectionPackage.FileType := FPackageType;
        CrossSectionPackage.FileName := AFileName;
        CrossSectionPackage.PackageName := '';

        CrossSectionReader := TCrossSection.Create(FPackageType);
        CrossSectionPackage.Package := CrossSectionReader;
        CrossSectionPackage.ReadPackage(Unhandled, NPER);
      end;
    end;
  finally
    CrossFileNames.Free;
  end;
end;

end.
