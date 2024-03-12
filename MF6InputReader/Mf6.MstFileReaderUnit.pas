unit Mf6.MstFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TMstOptions = class(TCustomMf6Persistent)
  private
    SAVE_FLOWS: Boolean;
    FFIRST_ORDER_DECAY: Boolean;
    FZERO_ORDER_DECAY: Boolean;
    FSORPTION: TStringOption;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    property FIRST_ORDER_DECAY: Boolean read FFIRST_ORDER_DECAY;
    property ZERO_ORDER_DECAY: Boolean read FZERO_ORDER_DECAY;
    property SORPTION: TStringOption read FSORPTION;
  end;

  TMstGridData = class(TCustomMf6Persistent)
  private
    FPOROSITY: TDArray3D;
    FDECAY: TDArray3D;
    FDECAY_SORBED: TDArray3D;
    FBULK_DENSITY: TDArray3D;
    FDISTCOEF: TDArray3D;
    FSP2: TDArray3D;
    FDimensions: TDimensions;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    property POROSITY: TDArray3D read FPOROSITY;
    property DECAY: TDArray3D read FDECAY;
    property DECAY_SORBED: TDArray3D read FDECAY_SORBED;
    property BULK_DENSITY: TDArray3D read FBULK_DENSITY;
    property DISTCOEF: TDArray3D read FDISTCOEF;
    property SP2: TDArray3D read FSP2;
  end;

  TMst = class(TDimensionedPackageReader)
  private
    FOptions: TMstOptions;
    FGridData: TMstGridData;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      const NPER: Integer); override;
    property Options: TMstOptions read FOptions;
    property GridData: TMstGridData read FGridData;
  end;

implementation

{ TMstOptions }

procedure TMstOptions.Initialize;
begin
  inherited;
  SAVE_FLOWS := False;
  FFIRST_ORDER_DECAY := False;
  FZERO_ORDER_DECAY := False;
  FSORPTION.Initialize;
end;

procedure TMstOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    if ReadEndOfSection(ALine, ErrorLine, 'OPTIONS', Unhandled) then
    begin
      Exit
    end;

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'OPTIONS') then
    begin
      // do nothing
    end
    else if FSplitter[0] = 'SAVE_FLOWS' then
    begin
      SAVE_FLOWS := True;
    end
    else if FSplitter[0] = 'FIRST_ORDER_DECAY' then
    begin
      FFIRST_ORDER_DECAY := True;
    end
    else if FSplitter[0] = 'ZERO_ORDER_DECAY' then
    begin
      FZERO_ORDER_DECAY := True;
    end
    else if ((FSplitter[0] = 'SORPTION') or (FSplitter[0] = 'SORBTION'))
      and (FSplitter.Count >= 2) then
    begin
      FSORPTION.Value := FSplitter[1];
      FSORPTION.Used := True;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TMstGridData }

constructor TMstGridData.Create(PackageType: string);
begin
  inherited;
  FDimensions.Initialize;
end;

procedure TMstGridData.Initialize;
begin
  inherited;
  SetLength(FPOROSITY, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
  SetLength(FDECAY, 0);
  SetLength(FDECAY_SORBED, 0);
  SetLength(FBULK_DENSITY, 0);
  SetLength(FDISTCOEF, 0);
  SetLength(FSP2, 0);
end;

procedure TMstGridData.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
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
    else if FSplitter[0] = 'POROSITY' then
    begin
//      SetLength(POROSITY, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FPOROSITY := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'DECAY' then
    begin
      SetLength(FDECAY, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FDECAY := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'DECAY_SORBED' then
    begin
      SetLength(FDECAY_SORBED, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FDECAY_SORBED := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'BULK_DENSITY' then
    begin
      SetLength(FBULK_DENSITY, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FBULK_DENSITY := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'DISTCOEF' then
    begin
      SetLength(FDISTCOEF, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FDISTCOEF := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'SP2' then
    begin
      SetLength(FSP2, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FSP2 := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSGRID, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TMst }

constructor TMst.Create(PackageType: string);
begin
  inherited;
  FOptions := TMstOptions.Create(PackageType);
  FGridData := TMstGridData.Create(PackageType);
end;

destructor TMst.Destroy;
begin
  FOptions.Free;
  FGridData.Free;
  inherited;
end;

procedure TMst.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
var
  ALine: string;
  ErrorLine: string;
begin
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading MST package');
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
      else if FSplitter[1] ='GRIDDATA' then
      begin
        FGridData.Read(Stream, Unhandled, FDimensions);
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSSect, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSSect, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

end.
