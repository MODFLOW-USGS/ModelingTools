unit TvkFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TvkOptions = class(TCustomMf6Persistent)
  private
    PRINT_INPUT: Boolean;
    TS6_FileName: string;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TValueType = (vtNumeric, vtString);

  TvkCell = record
    ValueType: TValueType;
    CellId: TCellId;
    VariableName: string;
    StringValue: string;
    NumericValue: Double;
    procedure Initialize;
  end;

  TvkCellList = TList<TvkCell>;

  TvkPeriodData = class(TCustomMf6Persistent)
  private
    IPer: Integer;
    FCells: TvkCellList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TPeriodList = TObjectList<TvkPeriodData>;

  Tvk = class(TDimensionedPackageReader)
  private
    FOptions: TvkOptions;
    FPeriods: TPeriodList;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
  end;

implementation

{ TvkOptions }

procedure TvkOptions.Initialize;
begin
  inherited;
  PRINT_INPUT := False;
  TS6_FileName := '';
end;

procedure TvkOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    if ReadEndOfSection(ALine, ErrorLine, 'OPTIONS', Unhandled) then
    begin
      Exit
    end;

    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    Assert(FSplitter.Count > 0);
    if FSplitter[0] = 'PRINT_INPUT' then
    begin
      PRINT_INPUT := True;
    end
    else if (FSplitter[0] = 'TS6')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEIN') then
    begin
      TS6_FileName := FSplitter[2];
    end
    else
    begin
      Unhandled.WriteLine('Unrecognized TVK option in the following line.');
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TvkCell }

procedure TvkCell.Initialize;
begin
  ValueType := vtNumeric;
  CellId.Initialize;
  VariableName := '';
  StringValue := '';
  NumericValue := 0;
end;

{ TvkPeriodData }

constructor TvkPeriodData.Create;
begin
  FCells := TvkCellList.Create;
  inherited;

end;

destructor TvkPeriodData.Destroy;
begin
  FCells.Free;
  inherited;
end;

procedure TvkPeriodData.Initialize;
begin
  inherited;
  FCells.Clear;
end;

procedure TvkPeriodData.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  Dimensions: TDimensions);
var
  DimensionCount: Integer;
  Cell: TvkCell;
  ALine: string;
  ErrorLine: string;
begin
  DimensionCount := Dimensions.DimensionCount;
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

    Cell.Initialize;
    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    if FSplitter.Count >= DimensionCount + 2 then
    begin
      if ReadCellID(Cell.CellId, 0, DimensionCount) then
      begin
        Cell.VariableName := FSplitter[DimensionCount];
        if TryStrToFloat(FSplitter[DimensionCount+1], Cell.NumericValue) then
        begin
          Cell.ValueType := vtNumeric;
        end
        else
        begin
          Cell.ValueType := vtString;
          Cell.StringValue := FSplitter[DimensionCount+1];
        end;
        FCells.Add(Cell);
      end
      else
      begin
        Unhandled.WriteLine('Unrecognized TVK PERIOD data in the following line.');
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine('Unrecognized TVK PERIOD data in the following line.');
      Unhandled.WriteLine(ErrorLine);
    end;
  end;

end;

{ Tvk }

constructor Tvk.Create;
begin
  inherited;
  FOptions := TvkOptions.Create;
  FPeriods := TPeriodList.Create;
end;

destructor Tvk.Destroy;
begin
  FOptions.Free;
  FPeriods.Free;
  inherited;
end;

procedure Tvk.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  IPER: Integer;
  APeriod: TvkPeriodData;
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
      else if FSplitter[1] ='PERIOD' then
      begin
        if TryStrToInt(FSplitter[2], IPER) then
        begin
          APeriod := TvkPeriodData.Create;
          FPeriods.Add(APeriod);
          APeriod.IPer := IPER;
          APeriod.Read(Stream, Unhandled, FDimensions);
        end
        else
        begin
          Unhandled.WriteLine('Unrecognized TVK data in the following line.');
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine('Unrecognized TVK data in the following line.');
        Unhandled.WriteLine(ErrorLine);
      end;
    end;
  end;
end;

end.
