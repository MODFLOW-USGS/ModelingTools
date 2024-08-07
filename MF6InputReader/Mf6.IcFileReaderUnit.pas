unit Mf6.IcFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TIcGridData = class(TCustomMf6Persistent)
  private
    FSTRT: TDArray3D;
    FDimensions: TDimensions;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    property STRT: TDArray3D read FSTRT;
  end;

  TIc = class(TDimensionedPackageReader)
  private
    FGridData: TIcGridData;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer); override;
    property GridData: TIcGridData read FGridData;
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
  SetLength(FSTRT, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
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
    RestoreStream(Stream);
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

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, SectionName) then
    begin
      // do nothing
    end
    else if FSplitter[0] = 'STRT' then
    begin
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      ThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        ThreeDReader.Read(Stream, Unhandled);
        FSTRT := ThreeDReader.FData;
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

procedure TIc.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
var
  ALine: string;
  ErrorLine: string;
begin
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading IC package');
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
        // Do nothing
        // An empty OPTIONS block is no longer required for IC.
      end
      else if FSplitter[1] ='GRIDDATA' then
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
