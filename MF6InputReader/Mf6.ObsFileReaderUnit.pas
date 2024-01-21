unit Mf6.ObsFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TObsOptions = class(TCustomMf6Persistent)
  private
    FDigits: Integer;
    PrintInput: Boolean;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    property Digits: Integer read FDigits;
  end;

  TIdType = (itCell, itNumber, itFloat, itName, itAbsent);

  TObservation = record
    ObsName: string;
    ObsType: string;
    IdType1: TIdType;
    IdType2: TIdType;
    CellId1: TCellId;
    CellId2: TCellId;
    Num1: Integer;
    Num2: Integer;
    FloatNum1: Extended;
    FloatNum2: Extended;
    Name1: string;
    Name2: string;
    procedure Initialize;
  end;

  TObservationList = TList<TObservation>;

  TObsFile = class(TCustomMf6Persistent)
  private
    FObservations: TObservationList;
    FOutputFileName: string;
    FBinary: Boolean;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; Dimensions: TDimensions);
    function GetCount: Integer;
    function GetObservation(Index: Integer): TObservation;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Binary: Boolean read FBinary;
    property Count: Integer read GetCount;
    property Observations[Index: Integer]: TObservation read GetObservation; default;
  end;

  TObsFileList = TObjectList<TObsFile>;

  TObs = class(TDimensionedPackageReader)
  private
    FOptions: TObsOptions;
    FObsFiles: TObsFileList;
    function GetFileCount: Integer;
    function GetObsFile(Index: Integer): TObsFile;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer); override;
    property Options: TObsOptions read FOptions;
    property FileCount: Integer read GetFileCount;
    property ObsFiles[Index: Integer]: TObsFile read GetObsFile; default;
  end;

implementation

uses
  ModelMuseUtilities;

resourcestring
  StrUnrecognizedSObse = 'Unrecognized %s Observation in the following line.';
  StrErrorReadingSObs = 'Error reading %s observation in the following line';

{ TObsOptions }

procedure TObsOptions.Initialize;
begin
  FDigits := -1;
  PrintInput := False;
  inherited;
end;

procedure TObsOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  SectionName: string;
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

    SectionName := 'OPTIONS';
    if ReadEndOfSection(ALine, ErrorLine, SectionName, Unhandled) then
    begin
      Exit;
    end;

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, SectionName) then
    begin
      // do nothing
    end
    else if (FSplitter[0] = 'DIGITS') and (FSplitter.Count >= 2) then
    begin
      if not TryStrToInt(FSplitter[1], FDigits) then
      begin
        Unhandled.WriteLine('Unrecogized Observation Utility option in the following line.');
        Unhandled.WriteLine(ErrorLine);
      end
    end
    else if FSplitter[0] = 'PRINT_INPUT' then
    begin
      PrintInput := True;
    end
    else
    begin
      Unhandled.WriteLine('Unrecogized Observation Utility option in the following line.');
      Unhandled.WriteLine(ErrorLine);
    end;
  end

end;

{ TObsFile }

constructor TObsFile.Create(PackageType: string);
begin
  FObservations := TObservationList.Create;
  inherited;

end;

destructor TObsFile.Destroy;
begin
  FObservations.Free;
  inherited;
end;

function TObsFile.GetCount: Integer;
begin
  result := FObservations.Count;
end;

function TObsFile.GetObservation(Index: Integer): TObservation;
begin
  result := FObservations[Index];
end;

procedure TObsFile.Initialize;
begin
  FObservations.Clear;
  inherited;
end;

procedure TObsFile.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  Dimensions: TDimensions);
var
  ALine: string;
  ErrorLine: string;
  DimensionCount: Integer;
  Observation: TObservation;
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
    if ReadEndOfSection(ALine, ErrorLine, 'CONTINUOUS', Unhandled) then
    begin
      Exit
    end;

    CaseSensitiveLine := ALine;
    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'CONTINUOUS') then
    begin
      Continue;
    end;

    Observation.Initialize;
    Observation.ObsName := FSplitter[0];

    ALine := LowerCase(ALine);
    FSplitter.DelimitedText := ALine;

    Observation.ObsType := FSplitter[1];
//    ALine := LowerCase(ALine);
//    FSplitter.DelimitedText := ALine;
    if FPackagetype = 'OBS6' then
    begin
      if (Observation.ObsType = 'head')
        or (Observation.ObsType = 'drawdown')
        or (Observation.ObsType = 'concentration')
        then
      begin
        if ReadCellID(Observation.CellId1, 2, DimensionCount) then
        begin
          Observation.IdType1:= itCell;
          FObservations.Add(Observation);
        end
        else
        begin
          Unhandled.WriteLine(Format(StrErrorReadingSObs, [FPackageType]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if Observation.ObsType = 'flow-ja-face' then
      begin
        if ReadCellID(Observation.CellId1, 2, DimensionCount) then
        begin
          Observation.IdType1:= itCell;
          if ReadCellID(Observation.CellId2, 2+DimensionCount, DimensionCount) then
          begin
            Observation.IdType2:= itCell;
            FObservations.Add(Observation);
          end
          else
          begin
            Unhandled.WriteLine(Format(StrErrorReadingSObs, [FPackageType]));
            Unhandled.WriteLine(ErrorLine);
          end;
        end
        else
        begin
          Unhandled.WriteLine(Format(StrErrorReadingSObs, [FPackageType]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSObse, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else if FPackagetype = 'CSUB6' then
    begin
      if (Observation.ObsType = 'csub')
        or (Observation.ObsType = 'inelastic-csub')
        or (Observation.ObsType = 'elastic-csub')
        or (Observation.ObsType = 'sk')
        or (Observation.ObsType = 'ske')
        or (Observation.ObsType = 'interbed-compaction')
        or (Observation.ObsType = 'inelastic-compaction')
        or (Observation.ObsType = 'elastic-compaction')
        or (Observation.ObsType = 'thickness')
        or (Observation.ObsType = 'theta') // The mf6.4.2 says ID of theta can only by a icsubno but one of the examples uses a bound name.
        then
      begin
        if TryStrToInt(FSplitter[2], Observation.Num1) then
        begin
          Observation.IdType1 := itNumber;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2]
        end;
        FObservations.Add(Observation);
      end
      else if (Observation.ObsType = 'coarse-csub')
        or (Observation.ObsType = 'csub-cell')
        or (Observation.ObsType = 'wcomp-csub-cell')
        or (Observation.ObsType = 'sk-cell')
        or (Observation.ObsType = 'ske-cell')
        or (Observation.ObsType = 'estress-cell')
        or (Observation.ObsType = 'gstress-cell')
        or (Observation.ObsType = 'coarse-compaction')
        or (Observation.ObsType = 'inelastic-compaction-cell')
        or (Observation.ObsType = 'elastic-compaction-cell')
        or (Observation.ObsType = 'compaction-cell')
        or (Observation.ObsType = 'coarse-thickness')
        or (Observation.ObsType = 'thickness-cell')
        or (Observation.ObsType = 'coarse-theta')
        or (Observation.ObsType = 'theta-cell')
        or (Observation.ObsType = 'preconstress-cell')
        then
      begin
        if ReadCellID(Observation.CellId1, 2, DimensionCount) then
        begin
          Observation.IdType1:= itCell;
          FObservations.Add(Observation);
        end
        else
        begin
          Unhandled.WriteLine(Format(StrErrorReadingSObs, [FPackageType]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if // (Observation.ObsType = 'theta')
         (Observation.ObsType = 'delay-flowtop')
        or (Observation.ObsType = 'delay-flowbot')
        then
      begin
        if TryStrToInt(FSplitter[2], Observation.Num1) then
        begin
          Observation.IdType1 := itNumber;
          FObservations.Add(Observation);
        end
        else
        begin
          Unhandled.WriteLine(Format(StrErrorReadingSObs, [FPackageType]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if (Observation.ObsType = 'delay-head')
        or (Observation.ObsType = 'delay-gstress')
        or (Observation.ObsType = 'delay-estress')
        or (Observation.ObsType = 'delay-preconstress')
        or (Observation.ObsType = 'delay-compaction')
        or (Observation.ObsType = 'delay-thickness')
        or (Observation.ObsType = 'delay-theta')
        then
      begin
        if TryStrToInt(FSplitter[2], Observation.Num1)
          and TryStrToInt(FSplitter[3], Observation.Num2) then
        begin
          Observation.IdType1 := itNumber;
          Observation.IdType2 := itNumber;
          FObservations.Add(Observation);
        end
        else
        begin
          Unhandled.WriteLine(Format(StrErrorReadingSObs, [FPackageType]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSObse, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else if FPackagetype = 'CHD6' then
    begin
      if (Observation.ObsType = 'chd') then
      begin
        if ReadCellID(Observation.CellId1, 2, DimensionCount) then
        begin
          Observation.IdType1:= itCell;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2];
        end;
        FObservations.Add(Observation);
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSObse, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else if FPackagetype = 'DRN6' then
    begin
      if (Observation.ObsType = 'drn')
        or (Observation.ObsType = 'to-mvr') then
      begin
        if ReadCellID(Observation.CellId1, 2, DimensionCount) then
        begin
          Observation.IdType1:= itCell;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2];
        end;
        FObservations.Add(Observation);
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSObse, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else if FPackagetype = 'EVT6' then
    begin
      if (Observation.ObsType = 'evt') then
      begin
        if ReadCellID(Observation.CellId1, 2, DimensionCount) then
        begin
          Observation.IdType1:= itCell;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2];
        end;
        FObservations.Add(Observation);
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSObse, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else if FPackagetype = 'GHB6' then
    begin
      if (Observation.ObsType = 'ghb')
        or (Observation.ObsType = 'to-mvr') then
      begin
        if ReadCellID(Observation.CellId1, 2, DimensionCount) then
        begin
          Observation.IdType1:= itCell;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2];
        end;
        FObservations.Add(Observation);
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSObse, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else if FPackagetype = 'RCH6' then
    begin
      if (Observation.ObsType = 'rch') then
      begin
        if ReadCellID(Observation.CellId1, 2, DimensionCount) then
        begin
          Observation.IdType1:= itCell;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2];
        end;
        FObservations.Add(Observation);
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSObse, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else if FPackagetype = 'RIV6' then
    begin
      if (Observation.ObsType = 'riv')
        or (Observation.ObsType = 'to-mvr') then
      begin
        if ReadCellID(Observation.CellId1, 2, DimensionCount) then
        begin
          Observation.IdType1:= itCell;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2];
        end;
        FObservations.Add(Observation);
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSObse, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else if FPackagetype = 'WEL6' then
    begin
      if (Observation.ObsType = 'wel')
        or (Observation.ObsType = 'to-mvr')
        or (Observation.ObsType = 'wel-reduction')
        then
      begin
        if ReadCellID(Observation.CellId1, 2, DimensionCount) then
        begin
          Observation.IdType1:= itCell;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2];
        end;
        FObservations.Add(Observation);
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSObse, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else if FPackagetype = 'LAK6' then
    begin
      if (Observation.ObsType = 'stage')
        or  (Observation.ObsType = 'ext-inflow')
        or  (Observation.ObsType = 'outlet-inflow')
        or  (Observation.ObsType = 'inflow')
        or  (Observation.ObsType = 'from-mvr')
        or  (Observation.ObsType = 'rainfall')
        or  (Observation.ObsType = 'runoff')
        or  (Observation.ObsType = 'withdrawal')
        or  (Observation.ObsType = 'evaporation')
        or  (Observation.ObsType = 'ext-outflow')
        or  (Observation.ObsType = 'to-mvr')
        or  (Observation.ObsType = 'storage')
        or  (Observation.ObsType = 'constant')
        or  (Observation.ObsType = 'outlet')
        or  (Observation.ObsType = 'volume')
        or  (Observation.ObsType = 'surface-area')
        then
      begin
        if TryStrToInt(FSplitter[2], Observation.Num1) then
        begin
          Observation.IdType1 := itNumber;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2]
        end;
        FObservations.Add(Observation);
      end
      else if (Observation.ObsType = 'lak')
        or (Observation.ObsType = 'wetted-area')
        or (Observation.ObsType = 'conductance')
        then
      begin
        if TryStrToInt(FSplitter[2], Observation.Num1) then
        begin
          Observation.IdType1 := itNumber;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2]
        end;
        if (FSplitter.Count >= 4) and TryStrToInt(FSplitter[3], Observation.Num2) then
        begin
          Observation.IdType2 := itNumber;
        end;
        FObservations.Add(Observation);
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSObse, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else if FPackagetype = 'MAW6' then
    begin
      if (Observation.ObsType = 'head')
        or  (Observation.ObsType = 'from-mvr')
        or  (Observation.ObsType = 'rate')
        or  (Observation.ObsType = 'rate-to-mvr')
        or  (Observation.ObsType = 'fw-rate')
        or  (Observation.ObsType = 'fw-to-mvr')
        or  (Observation.ObsType = 'storage')
        or  (Observation.ObsType = 'constant')
        or  (Observation.ObsType = 'fw-conductance')
        then
      begin
        if TryStrToInt(FSplitter[2], Observation.Num1) then
        begin
          Observation.IdType1 := itNumber;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2]
        end;
        FObservations.Add(Observation);
      end
      else if (Observation.ObsType = 'maw')
        or  (Observation.ObsType = 'conductance')
        then
      begin
        if TryStrToInt(FSplitter[2], Observation.Num1) then
        begin
          Observation.IdType1 := itNumber;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2]
        end;
        if (FSplitter.Count >= 4) and  TryStrToInt(FSplitter[3], Observation.Num2) then
        begin
          Observation.IdType2 := itNumber;
        end;
        FObservations.Add(Observation);
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSObse, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else if FPackagetype = 'SFR6' then
    begin
      if (Observation.ObsType = 'stage')
        or  (Observation.ObsType = 'ext-inflow')
        or  (Observation.ObsType = 'inflow')
        or  (Observation.ObsType = 'from-mvr')
        or  (Observation.ObsType = 'rainfall')
        or  (Observation.ObsType = 'runoff')
        or  (Observation.ObsType = 'sfr')
        or  (Observation.ObsType = 'evaporation')
        or  (Observation.ObsType = 'outflow')
        or  (Observation.ObsType = 'ext-outflow')
        or  (Observation.ObsType = 'to-mvr')
        or  (Observation.ObsType = 'upstream-flow')
        or  (Observation.ObsType = 'downstream-flow')
        or  (Observation.ObsType = 'depth')
        or  (Observation.ObsType = 'wet-perimeter')
        or  (Observation.ObsType = 'wet-area')
        or  (Observation.ObsType = 'wet-width')
//          or  (Observation.ObsType = 'inflow')
//          or  (Observation.ObsType = 'inflow')
//          or  (Observation.ObsType = 'inflow')
        then
      begin
        if TryStrToInt(FSplitter[2], Observation.Num1) then
        begin
          Observation.IdType1 := itNumber;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2]
        end;
        FObservations.Add(Observation);
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSObse, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else if FPackagetype = 'UZF6' then
    begin
      if (Observation.ObsType = 'uzf-gwrch')
        or  (Observation.ObsType = 'uzf-gwd')
        or  (Observation.ObsType = 'uzf-gwd-to-mvr')
        or  (Observation.ObsType = 'uzf-gwet')
        or  (Observation.ObsType = 'infiltration')
        or  (Observation.ObsType = 'from-mvr')
        or  (Observation.ObsType = 'rej-inf')
        or  (Observation.ObsType = 'rej-inf-to-mvr')
        or  (Observation.ObsType = 'uzet')
        or  (Observation.ObsType = 'storage')
        or  (Observation.ObsType = 'net-infiltration')
        then
      begin
        if TryStrToInt(FSplitter[2], Observation.Num1) then
        begin
          Observation.IdType1 := itNumber;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2]
        end;
        FObservations.Add(Observation);
      end
      else if (Observation.ObsType = 'water-content') then
      begin
        if TryStrToInt(FSplitter[2], Observation.Num1) then
        begin
          Observation.IdType1 := itNumber;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2]
        end;
        if TryFortranStrToFloat(FSplitter[3], Observation.FloatNum2) then
        begin
          Observation.IdType1 := itFloat;
          FObservations.Add(Observation);
        end
        else
        begin
          Unhandled.WriteLine(Format(StrUnrecognizedSObse, [FPackageType]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSObse, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else if FPackagetype = 'GW6F-GWF6' then
    begin
      if (Observation.ObsType = 'flow-ja-face')
        then
      begin
        if TryStrToInt(FSplitter[2], Observation.Num1) then
        begin
          Observation.IdType1 := itNumber;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2]
        end;
        FObservations.Add(Observation);
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSObse, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else if FPackagetype = 'GWT6' then
    begin
      if (Observation.ObsType = 'concentration')
        then
      begin
        if ReadCellID(Observation.CellId1, 2, DimensionCount) then
        begin
          Observation.IdType1:= itCell;
          FObservations.Add(Observation);
        end
        else
        begin
          Unhandled.WriteLine(Format(StrErrorReadingSObs, [FPackageType]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if Observation.ObsType = 'flow-ja-face' then
      begin
        if ReadCellID(Observation.CellId1, 2, DimensionCount) then
        begin
          Observation.IdType1:= itCell;
          if ReadCellID(Observation.CellId2, 2+DimensionCount, DimensionCount) then
          begin
            Observation.IdType2:= itCell;
            FObservations.Add(Observation);
          end
          else
          begin
            Unhandled.WriteLine(Format(StrErrorReadingSObs, [FPackageType]));
            Unhandled.WriteLine(ErrorLine);
          end;
        end
        else
        begin
          Unhandled.WriteLine(Format(StrErrorReadingSObs, [FPackageType]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSObse, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else if FPackagetype = 'CNC6' then
    begin
      if (Observation.ObsType = 'cnc')
        then
      begin
        if ReadCellID(Observation.CellId1, 2, DimensionCount) then
        begin
          Observation.IdType1:= itCell;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2];
        end;
        FObservations.Add(Observation);
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSObse, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else if FPackagetype = 'SRC6' then
    begin
      if (Observation.ObsType = 'src')
        then
      begin
        if ReadCellID(Observation.CellId1, 2, DimensionCount) then
        begin
          Observation.IdType1:= itCell;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2];
        end;
        FObservations.Add(Observation);
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSObse, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else if FPackagetype = 'SFT6' then
    begin
      if (Observation.ObsType = 'concentration')
        or (Observation.ObsType = 'storage')
        or (Observation.ObsType = 'constant')
        or (Observation.ObsType = 'from-mvr')
        or (Observation.ObsType = 'to-mvr')
        or (Observation.ObsType = 'sft')
        or (Observation.ObsType = 'rainfall')
        or (Observation.ObsType = 'evaporation')
        or (Observation.ObsType = 'runoff')
        or (Observation.ObsType = 'ext-inflow')
        or (Observation.ObsType = 'ext-outflow')
        then
      begin
        if TryStrToInt(FSplitter[2], Observation.Num1) then
        begin
          Observation.IdType1:= itNumber;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2];
        end;
        FObservations.Add(Observation);
      end
      else if Observation.ObsType = 'flow-ja-face' then
      begin
        if TryStrToInt(FSplitter[2], Observation.Num1) then
        begin
          Observation.IdType1:= itNumber;
          if TryStrToInt(FSplitter[3], Observation.Num2) then
          begin
            Observation.IdType2:= itNumber;
            FObservations.Add(Observation);
          end
          else
          begin
            Unhandled.WriteLine(Format(StrErrorReadingSObs, [FPackageType]));
            Unhandled.WriteLine(ErrorLine);
          end;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2];
        end;
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSObse, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else if FPackagetype = 'LKT6' then
    begin
      if (Observation.ObsType = 'concentration')
        or (Observation.ObsType = 'storage')
        or (Observation.ObsType = 'constant')
        or (Observation.ObsType = 'from-mvr')
        or (Observation.ObsType = 'to-mvr')
        or (Observation.ObsType = 'lkt')
        or (Observation.ObsType = 'rainfall')
        or (Observation.ObsType = 'evaporation')
        or (Observation.ObsType = 'runoff')
        or (Observation.ObsType = 'ext-inflow')
        or (Observation.ObsType = 'withdrawal')
        or (Observation.ObsType = 'ext-outflow')
        then
      begin
        if TryStrToInt(FSplitter[2], Observation.Num1) then
        begin
          Observation.IdType1:= itNumber;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2];
        end;
        FObservations.Add(Observation);
      end
      else if Observation.ObsType = 'flow-ja-face' then
      begin
        if TryStrToInt(FSplitter[2], Observation.Num1) then
        begin
          Observation.IdType1:= itNumber;
          if TryStrToInt(FSplitter[3], Observation.Num2) then
          begin
            Observation.IdType2:= itNumber;
            FObservations.Add(Observation);
          end
          else
          begin
            Unhandled.WriteLine(Format(StrErrorReadingSObs, [FPackageType]));
            Unhandled.WriteLine(ErrorLine);
          end;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2];
        end;
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSObse, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else if FPackagetype = 'MWT6' then
    begin
      if (Observation.ObsType = 'concentration')
        or (Observation.ObsType = 'storage')
        or (Observation.ObsType = 'constant')
        or (Observation.ObsType = 'from-mvr')
        or (Observation.ObsType = 'to-mvr')
        or (Observation.ObsType = 'rate')
        or (Observation.ObsType = 'fw-rate')
        or (Observation.ObsType = 'rate-to-mvr')
        or (Observation.ObsType = 'fw-rate-to-mvr')
        or (Observation.ObsType = 'withdrawal')
        or (Observation.ObsType = 'ext-outflow')
        then
      begin
        if TryStrToInt(FSplitter[2], Observation.Num1) then
        begin
          Observation.IdType1:= itNumber;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2];
        end;
        FObservations.Add(Observation);
      end
      else if Observation.ObsType = 'mwt' then
      begin
        if TryStrToInt(FSplitter[2], Observation.Num1) then
        begin
          Observation.IdType1:= itNumber;
          if TryStrToInt(FSplitter[3], Observation.Num2) then
          begin
            Observation.IdType2:= itNumber;
            FObservations.Add(Observation);
          end
          else
          begin
            Unhandled.WriteLine(Format(StrErrorReadingSObs, [FPackageType]));
            Unhandled.WriteLine(ErrorLine);
          end;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2];
        end;
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSObse, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else if FPackagetype = 'UZT6' then
    begin
      if (Observation.ObsType = 'concentration')
        or (Observation.ObsType = 'storage')
        or (Observation.ObsType = 'constant')
        or (Observation.ObsType = 'from-mvr')
        or (Observation.ObsType = 'uzt')
//        or (Observation.ObsType = 'to-mvr')
        or (Observation.ObsType = 'infiltration')
        or (Observation.ObsType = 'rej-inf')
        or (Observation.ObsType = 'uzet')
        or (Observation.ObsType = 'rej-inf-to-mvr')
        then
      begin
        if TryStrToInt(FSplitter[2], Observation.Num1) then
        begin
          Observation.IdType1:= itNumber;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2];
        end;
        FObservations.Add(Observation);
      end
      else if Observation.ObsType = 'flow-ja-face' then
      begin
        if TryStrToInt(FSplitter[2], Observation.Num1) then
        begin
          Observation.IdType1:= itNumber;
          if TryStrToInt(FSplitter[3], Observation.Num2) then
          begin
            Observation.IdType2:= itNumber;
            FObservations.Add(Observation);
          end
          else
          begin
            Unhandled.WriteLine(Format(StrErrorReadingSObs, [FPackageType]));
            Unhandled.WriteLine(ErrorLine);
          end;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2];
        end;
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSObse, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else if FPackagetype = 'GWT6-GWT6' then
    begin
      if (Observation.ObsType = 'flow-ja-face')
        then
      begin
        if TryStrToInt(FSplitter[2], Observation.Num1) then
        begin
          Observation.IdType1:= itNumber;
        end
        else
        begin
          Observation.IdType1 := itName;
          FSplitter.DelimitedText := CaseSensitiveLine;
          Observation.Name1 := FSplitter[2];
        end;
        FObservations.Add(Observation);
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSObse, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSObse, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;


{ TObservation }

procedure TObservation.Initialize;
begin
  ObsName := '';
  ObsType := '';
  IdType1 := itAbsent;
  IdType2 := itAbsent;
  CellId1.Initialize;
  CellId2.Initialize;
  Num1 := 0;
  Num2 := 0;
  FloatNum1 := 0;
  FloatNum2 := 0;
  Name1 := '';
  Name2 := '';
end;

{ TObs }

constructor TObs.Create(PackageType: string);
begin
  inherited;
  FOptions := TObsOptions.Create(PackageType);
  FObsFiles := TObsFileList.Create;
end;

destructor TObs.Destroy;
begin
  FOptions.Free;
  FObsFiles.Free;
  inherited;
end;

function TObs.GetFileCount: Integer;
begin
  result := FObsFiles.Count;
end;

function TObs.GetObsFile(Index: Integer): TObsFile;
begin
  result := FObsFiles[Index];
end;

procedure TObs.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
var
  ALine: string;
  ErrorLine: string;
  ObsFile: TObsFile;
begin
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading OBS package');
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
    if FSplitter.Count >= 2 then
    begin
      if FSplitter[0] = 'BEGIN' then
      begin
        if FSplitter[1] = 'OPTIONS' then
        begin
          FOptions.Read(Stream, Unhandled)
        end
        else if (FSplitter.Count >= 3)
          and (FSplitter[1] = 'CONTINUOUS')
          and (FSplitter[2] = 'FILEOUT') then
        begin
          ObsFile := TObsFile.Create(FPackageType);
          FObsFiles.Add(ObsFile);
          ObsFile.FOutputFileName := FSplitter[3];
          ObsFile.FBinary := (FSplitter.Count >= 5) and (FSplitter[4] = 'BINARY');
          ObsFile.Read(Stream, Unhandled, FDimensions)
        end
        else
        begin
          Unhandled.WriteLine('Unrecognized section in the following line.');
          Unhandled.WriteLine(ErrorLine);
        end;
      end;
    end
    else
    begin
      Unhandled.WriteLine('Unrecognized section in the following line.');
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

end.
