unit Mf6.CSubFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TCSubOptions = class(TCustomMf6Persistent)
  private
    FBOUNDNAMES: Boolean;
    FPRINT_INPUT: Boolean;
    FSAVE_FLOWS: Boolean;
    FGAMMAW: TRealOption;
    FBETA: TRealOption;
    FHEAD_BASED: Boolean;
    FINITIAL_PRECONSOLIDATION_HEAD: Boolean;
    FNDELAYCELLS: TIntegerOption;
    FCOMPRESSION_INDICES: Boolean;
    FUPDATE_MATERIAL_PROPERTIES: Boolean;
    FCELL_FRACTION: Boolean;
    FSPECIFIED_INITIAL_INTERBED_STATE: Boolean;
    FSPECIFIED_INITIAL_PRECONSOLIDATION_STRESS: Boolean;
    FSPECIFIED_INITIAL_DELAY_HEAD: Boolean;
    FEFFECTIVE_STRESS_LAG: Boolean;
    FSTRAIN_CSV_INTERBED: Boolean;
    FSTRAIN_CSV_COARSE: Boolean;
    FCOMPACTION: Boolean;
    FCOMPACTION_ELASTIC: Boolean;
    FCOMPACTION_INELASTIC: Boolean;
    FCOMPACTION_INTERBED: Boolean;
    FCOMPACTION_COARSE: Boolean;
    FZDISPLACEMENT: Boolean;
    FPACKAGE_CONVERGENCE: Boolean;
    TS6_FileNames: TStringList;
    Obs6_FileNames: TStringList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property BOUNDNAMES: Boolean read FBOUNDNAMES;
    property PRINT_INPUT: Boolean read FPRINT_INPUT;
    property SAVE_FLOWS: Boolean read FSAVE_FLOWS;
    property GAMMAW: TRealOption read FGAMMAW;
    property BETA: TRealOption read FBETA;
    property HEAD_BASED: Boolean read FHEAD_BASED;
    property INITIAL_PRECONSOLIDATION_HEAD: Boolean read FINITIAL_PRECONSOLIDATION_HEAD;
    property NDELAYCELLS: TIntegerOption read FNDELAYCELLS;
    property COMPRESSION_INDICES: Boolean read FCOMPRESSION_INDICES;
    property UPDATE_MATERIAL_PROPERTIES: Boolean read FUPDATE_MATERIAL_PROPERTIES;
    property CELL_FRACTION: Boolean read FCELL_FRACTION;
    property SPECIFIED_INITIAL_INTERBED_STATE: Boolean read FSPECIFIED_INITIAL_INTERBED_STATE;
    property SPECIFIED_INITIAL_PRECONSOLIDATION_STRESS: Boolean read FSPECIFIED_INITIAL_PRECONSOLIDATION_STRESS;
    property SPECIFIED_INITIAL_DELAY_HEAD: Boolean read FSPECIFIED_INITIAL_DELAY_HEAD;
    property EFFECTIVE_STRESS_LAG: Boolean read FEFFECTIVE_STRESS_LAG;
    property STRAIN_CSV_INTERBED: Boolean read FSTRAIN_CSV_INTERBED;
    property STRAIN_CSV_COARSE: Boolean read FSTRAIN_CSV_COARSE;
    property COMPACTION: Boolean read FCOMPACTION;
    property COMPACTION_ELASTIC: Boolean read FCOMPACTION_ELASTIC;
    property COMPACTION_INELASTIC: Boolean read FCOMPACTION_INELASTIC;
    property COMPACTION_INTERBED: Boolean read FCOMPACTION_INTERBED;
    property COMPACTION_COARSE: Boolean read FCOMPACTION_COARSE;
    property ZDISPLACEMENT: Boolean read FZDISPLACEMENT;
    property PACKAGE_CONVERGENCE: Boolean read FPACKAGE_CONVERGENCE;
  end;

  TCSubDimensions = class(TCustomMf6Persistent)
  private
    FNINTERBEDS: Integer;
    FMAXSIG0: TIntegerOption;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
    property NINTERBEDS: Integer read FNINTERBEDS;
    property MAXSIG0: TIntegerOption read FMAXSIG0;
  end;

  TCSubGridData = class(TCustomMf6Persistent)
  private
    FCG_SKE_CR: TDArray3D;
    FCG_THETA: TDArray3D;
    FSGM: TDArray3D;
    FSGS: TDArray3D;
    FDimensions: TDimensions;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    property CG_SKE_CR: TDArray3D read FCG_SKE_CR;
    property CG_THETA: TDArray3D read FCG_THETA;
    property SGM: TDArray3D read FSGM;
    property SGS: TDArray3D read FSGS;
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
    function GetCount: Integer;
    function GetItem(Index: Integer): TCSubItem;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TCSubItem read GetItem; default;
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
    function GetCell(Index: Integer): TCSubTimeItem;
    function GetCount: Integer;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Period: Integer read IPer;
    property Count: Integer read GetCount;
    property Cells[Index: Integer]: TCSubTimeItem read GetCell; default;
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
    function GetPeriod(Index: Integer): TCSubPeriod;
    function GetPeriodCount: Integer;
    function GetTimeSeriesCount: Integer;
    function GetPackage(Index: Integer): TPackage;
    function GetObservation(Index: Integer): TPackage;
    function GetObservationCount: Integer;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
    property Options: TCSubOptions read FOptions;
    property CSubDimensions: TCSubDimensions read FCSubDimensions;
    property GridData: TCSubGridData read FGridData;
    property PackageData: TCSubPackageData read FPackageData;
    property PeriodCount: Integer read GetPeriodCount;
    property Periods[Index: Integer]: TCSubPeriod read GetPeriod; default;
    property TimeSeriesCount: Integer read GetTimeSeriesCount;
    property TimeSeries[Index: Integer]: TPackage read GetPackage;
    property ObservationCount: Integer read GetObservationCount;
    property Observations[Index: Integer]: TPackage read GetObservation;
  end;

implementation

uses
  ModelMuseUtilities, Mf6.TimeSeriesFileReaderUnit, Mf6.ObsFileReaderUnit;

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

  FBOUNDNAMES := False;
  FPRINT_INPUT := False;
  FSAVE_FLOWS := False;
  FGAMMAW.Initialize;
  FBETA.Initialize;
  FHEAD_BASED := False;
  FINITIAL_PRECONSOLIDATION_HEAD := False;
  FNDELAYCELLS.Initialize;
  FCOMPRESSION_INDICES := False;
  FUPDATE_MATERIAL_PROPERTIES := False;
  FCELL_FRACTION := False;
  FSPECIFIED_INITIAL_INTERBED_STATE := False;
  FSPECIFIED_INITIAL_PRECONSOLIDATION_STRESS := False;
  FSPECIFIED_INITIAL_DELAY_HEAD := False;
  FEFFECTIVE_STRESS_LAG := False;
  FSTRAIN_CSV_INTERBED := False;
  FSTRAIN_CSV_COARSE := False;
  FCOMPACTION := False;
  FCOMPACTION_ELASTIC := False;
  FCOMPACTION_INELASTIC := False;
  FCOMPACTION_INTERBED := False;
  FCOMPACTION_COARSE := False;
  FZDISPLACEMENT := False;
  FPACKAGE_CONVERGENCE := False;
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
      FBOUNDNAMES := True;
    end
    else if FSplitter[0] = 'PRINT_INPUT' then
    begin
      FPRINT_INPUT := True;
    end
    else if FSplitter[0] = 'SAVE_FLOWS' then
    begin
      FSAVE_FLOWS := True;
    end
    else if (FSplitter[0] = 'GAMMAW') and (FSplitter.Count >= 2)
      and TryFortranStrToFloat(FSplitter[1], FGAMMAW.Value) then
    begin
      FGAMMAW.Used := True;
    end
    else if (FSplitter[0] = 'BETA') and (FSplitter.Count >= 2)
      and TryFortranStrToFloat(FSplitter[1], FBETA.Value) then
    begin
      FBETA.Used := True;
    end
    else if FSplitter[0] = 'HEAD_BASED' then
    begin
      FHEAD_BASED := True;
    end
    else if FSplitter[0] = 'INITIAL_PRECONSOLIDATION_HEAD' then
    begin
      FINITIAL_PRECONSOLIDATION_HEAD := True;
    end
    else if (FSplitter[0] = 'NDELAYCELLS') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], FNDELAYCELLS.Value) then
    begin
      FNDELAYCELLS.Used := True;
    end
    else if FSplitter[0] = 'COMPRESSION_INDICES' then
    begin
      FCOMPRESSION_INDICES := True;
    end
    else if FSplitter[0] = 'UPDATE_MATERIAL_PROPERTIES' then
    begin
      FUPDATE_MATERIAL_PROPERTIES := True;
    end
    else if FSplitter[0] = 'CELL_FRACTION' then
    begin
      FCELL_FRACTION := True;
    end
    else if FSplitter[0] = 'SPECIFIED_INITIAL_INTERBED_STATE' then
    begin
      FSPECIFIED_INITIAL_INTERBED_STATE := True;
    end
    else if FSplitter[0] = 'SPECIFIED_INITIAL_PRECONSOLIDATION_STRESS' then
    begin
      FSPECIFIED_INITIAL_PRECONSOLIDATION_STRESS := True;
    end
    else if FSplitter[0] = 'SPECIFIED_INITIAL_DELAY_HEAD' then
    begin
      FSPECIFIED_INITIAL_DELAY_HEAD := True;
    end
    else if FSplitter[0] = 'EFFECTIVE_STRESS_LAG' then
    begin
      FEFFECTIVE_STRESS_LAG := True;
    end
    else if FSplitter[0] = 'STRAIN_CSV_INTERBED' then
    begin
      FSTRAIN_CSV_INTERBED := True;
    end
    else if FSplitter[0] = 'STRAIN_CSV_COARSE' then
    begin
      FSTRAIN_CSV_COARSE := True;
    end
    else if FSplitter[0] = 'COMPACTION' then
    begin
      FCOMPACTION := True;
    end
    else if FSplitter[0] = 'COMPACTION_ELASTIC' then
    begin
      FCOMPACTION_ELASTIC := True;
    end
    else if FSplitter[0] = 'COMPACTION_INELASTIC' then
    begin
      FCOMPACTION_INELASTIC := True;
    end
    else if FSplitter[0] = 'COMPACTION_INTERBED' then
    begin
      FCOMPACTION_INTERBED := True;
    end
    else if FSplitter[0] = 'COMPACTION_INTERBED' then
    begin
      FCOMPACTION_INTERBED := True;
    end
    else if FSplitter[0] = 'COMPACTION_COARSE' then
    begin
      FCOMPACTION_COARSE := True;
    end
    else if FSplitter[0] = 'ZDISPLACEMENT' then
    begin
      FZDISPLACEMENT := True;
    end
    else if FSplitter[0] = 'PACKAGE_CONVERGENCE' then
    begin
      FPACKAGE_CONVERGENCE := True;
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
  FNINTERBEDS := 0;
  FMAXSIG0.Initialize;
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
    else if (FSplitter[0] = 'NINTERBEDS') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], FNINTERBEDS) then
    begin
    end
    else if (FSplitter[0] = 'MAXSIG0') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], FMAXSIG0.Value) then
    begin
      FMAXSIG0.Used := True;
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
  SetLength(FCG_SKE_CR, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
  SetLength(FCG_THETA, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
  SetLength(FSGM, 0);
  SetLength(FSGS, 0);
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
    RestoreStream(Stream);
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

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'GRIDDATA') then
    begin
      // do nothing
    end
    else if FSplitter[0] = 'CG_SKE_CR' then
    begin
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FCG_SKE_CR := DoubleThreeDReader.FData;
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
        FCG_THETA := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'SGM' then
    begin
      SetLength(FSGM, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FSGM := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'SGS' then
    begin
      SetLength(FSGS, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FSGS := DoubleThreeDReader.FData;
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

function TCSubPackageData.GetCount: Integer;
begin
  result := FItems.Count;
end;

function TCSubPackageData.GetItem(Index: Integer): TCSubItem;
begin
  result := FItems[Index];
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

    Item.Initialize;

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'PACKAGEDATA') then
    begin
      // do nothing
    end
    else if TryStrToInt(FSplitter[0],Item.icsubno) and (FSplitter.Count >= 10 + DimensionCount)  then
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

function TCSubPeriod.GetCell(Index: Integer): TCSubTimeItem;
begin
  result := FCells[Index];
end;

function TCSubPeriod.GetCount: Integer;
begin
  result := FCells.Count;
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

    Cell.Initialize;
    CaseSensitiveLine := ALine;
    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'PERIOD') then
    begin
      // do nothing
    end
    else if FSplitter.Count >= DimensionCount + 1 then
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

function TCSub.GetTimeSeriesCount: Integer;
begin
  result := FTimeSeriesPackages.Count;
end;

function TCSub.GetObservation(Index: Integer): TPackage;
begin
  result := FObservationsPackages[Index];
end;

function TCSub.GetObservationCount: Integer;
begin
  result := FObservationsPackages.Count;
end;

function TCSub.GetPackage(Index: Integer): TPackage;
begin
  result := FTimeSeriesPackages[Index];
end;

function TCSub.GetPeriod(Index: Integer): TCSubPeriod;
begin
  result := FPeriods[Index];
end;

function TCSub.GetPeriodCount: Integer;
begin
  result := FPeriods.Count;
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
