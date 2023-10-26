unit ObsFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TObsOptions = class(TCustomMf6Persistent)
  private
    Digits: Integer;
    PrintInput: Boolean;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TObservation = record
    ObsName: string;
    ObsType: string;
    Id1: TCellId;
    Id2: TCellId;
    Id2Used: Boolean;
    procedure Initialize;
  end;

  TObservationList = TList<TObservation>;

  TObsFile = class(TCustomMf6Persistent)
  private
    FObservations: TObservationList;
    FOutputFileName: string;
    FBinary: Boolean;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TObsFileList = TObjectList<TObsFile>;

  TObs = class(TDimensionedPackageReader)
  private
    FOptions: TObsOptions;
    FObsFiles: TObsFileList;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
  end;

implementation

{ TObsOptions }

procedure TObsOptions.Initialize;
begin
  Digits := -1;
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

    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    if (FSplitter[0] = 'DIGITS') and (FSplitter.Count >= 2) then
    begin
      if not TryStrToInt(FSplitter[0], Digits) then
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

constructor TObsFile.Create;
begin
  FObservations := TObservationList.Create;
  inherited;

end;

destructor TObsFile.Destroy;
begin
  FObservations.Free;
  inherited;
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
  StartIndex: Integer;
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
    if ReadEndOfSection(ALine, ErrorLine, 'CONTINUOUS', Unhandled) then
    begin
      Exit
    end;

    FSplitter.DelimitedText := ALine;
    if FSplitter.Count >= 2 + DimensionCount then
    begin
      Observation.Initialize;
      Observation.ObsName := FSplitter[0];
    end;
    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    if FSplitter.Count >= 2 + DimensionCount then
    begin
      Observation.ObsType := FSplitter[1];
      if ReadCellID(Observation.Id1, 2, DimensionCount) then
      begin
        ReadCellID(Observation.Id2, 2+DimensionCount, DimensionCount);
        FObservations.Add(Observation);
      end
      else
      begin
        Unhandled.WriteLine('Unrecognized observation in the following line.');
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine('Unrecognized observation in the following line.');
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;


{ TObservation }

procedure TObservation.Initialize;
begin
  ObsName := '';
  ObsType := '';
  Id1.Initialize;
  Id2.Initialize;
  Id2Used := False;
end;

{ TObs }

constructor TObs.Create;
begin
  inherited;
  FOptions := TObsOptions.Create;
  FObsFiles := TObsFileList.Create;
end;

destructor TObs.Destroy;
begin
  FOptions.Free;
  FObsFiles.Free;
  inherited;
end;

procedure TObs.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  ObsFile: TObsFile;
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
          ObsFile := TObsFile.Create;
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
