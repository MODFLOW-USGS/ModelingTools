unit IcFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TIcGridData = class(TCustomMf6Persistent)
  private
    STRT: TDArray3D;
    FDimensions: TDimensions;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
  end;

  TIc = class(TDimensionedPackageReader)
  private
    FGridData: TIcGridData;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;

  end;

implementation

resourcestring
  StrUnrecognizedICOpti = 'Unrecognized IC option in the following line.';

{ TIcGridData }

constructor TIcGridData.Create(PackageType: string);
begin
  FDimensions.Initialize;
  inherited;

end;

procedure TIcGridData.Initialize;
begin
  SetLength(STRT, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
  inherited;
end;

procedure TIcGridData.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  Dimensions: TDimensions);
var
  ALine: string;
  ErrorLine: string;
  SectionName: string;
  Layered: Boolean;
  ThreeDReader: TDouble3DArrayReader;
begin
  FDimensions := Dimensions;
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

    SectionName := 'GRIDDATA';
    if ReadEndOfSection(ALine, ErrorLine, SectionName, Unhandled) then
    begin
      Exit;
    end;

    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    if FSplitter[0] = 'STRT' then
    begin
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      ThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        ThreeDReader.Read(Stream, Unhandled);
        STRT := ThreeDReader.FData;
      finally
        ThreeDReader.Free;
      end;
    end
    else
    begin
      Unhandled.WriteLine('Unrecognized IC GRIDDATA in the following line');
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TIc }

constructor TIc.Create(PackageType: string);
begin
  FGridData := TIcGridData.Create(PackageType);
  inherited;

end;

destructor TIc.Destroy;
begin
  FGridData.Free;
  inherited;
end;

procedure TIc.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
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
      if FSplitter[1] ='GRIDDATA' then
      begin
        FGridData.Read(Stream, Unhandled, FDimensions);
      end
      else
      begin
        Unhandled.WriteLine(StrUnrecognizedICOpti);
        Unhandled.WriteLine(ErrorLine);
      end;
    end;
  end;
end;

end.
