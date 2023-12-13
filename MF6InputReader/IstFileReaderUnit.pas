unit IstFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TIstOptions = class(TCustomMf6Persistent)
  private
    SAVE_FLOWS: Boolean;
    BUDGET: Boolean;
    BUDGETCSV: Boolean;
    SORPTION: Boolean;
    FIRST_ORDER_DECAY: Boolean;
    ZERO_ORDER_DECAY: Boolean;
    CIM: Boolean;
    CIM_PRINT_FORMAT: TPrintFormat;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TIstGridData = class(TCustomMf6Persistent)
  private
    POROSITY: TDArray3D;
    VOLFRAC: TDArray3D;
    ZETAIM: TDArray3D;
    CIM: TDArray3D;
    DECAY: TDArray3D;
    DECAY_SORBED: TDArray3D;
    BULK_DENSITY: TDArray3D;
    DISTCOEF: TDArray3D;
    FDimensions: TDimensions;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
  end;

  TIst = class(TDimensionedPackageReader)
  private
    FOptions: TIstOptions;
    FGridData: TIstGridData;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
  end;

implementation

{ TIstOptions }

procedure TIstOptions.Initialize;
begin
  inherited;
  SAVE_FLOWS := False;
  BUDGET := False;
  BUDGETCSV := False;
  SORPTION := False;
  FIRST_ORDER_DECAY := False;
  ZERO_ORDER_DECAY := False;
  CIM := False;
  CIM_PRINT_FORMAT.Initialize;
end;

procedure TIstOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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

//    CaseSensitiveLine := ALine;
    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    Assert(FSplitter.Count > 0);
    if FSplitter[0] = 'SAVE_FLOWS' then
    begin
      SAVE_FLOWS := True;
    end
    else if (FSplitter[0] = 'BUDGET')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEOUT') then
    begin
      BUDGET := True;
    end
    else if (FSplitter[0] = 'BUDGETCSV')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEOUT') then
    begin
      BUDGETCSV := True;
    end
    else if (FSplitter[0] = 'SORPTION') then
    begin
      SORPTION := True;
    end
    else if FSplitter[0] = 'FIRST_ORDER_DECAY' then
    begin
      FIRST_ORDER_DECAY := True;
    end
    else if FSplitter[0] = 'ZERO_ORDER_DECAY' then
    begin
      ZERO_ORDER_DECAY := True;
    end
    else if (FSplitter[0] = 'CIM')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEOUT') then
    begin
      CIM := True;
    end
    else if (FSplitter[0] = 'CIM')
      and (FSplitter.Count >= 8)
      and (FSplitter[1] = 'PRINT_FORMAT') then
    begin
      ReadPrintFormat(ErrorLine, Unhandled, FPackageType, CIM_PRINT_FORMAT);
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TIstGridData }

constructor TIstGridData.Create(PackageType: string);
begin
  inherited;
  FDimensions.Initialize;
end;

procedure TIstGridData.Initialize;
begin
  inherited;
  SetLength(POROSITY, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
  SetLength(VOLFRAC, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
  SetLength(ZETAIM, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
  SetLength(CIM, 0);
  SetLength(DECAY, 0);
  SetLength(DECAY_SORBED, 0);
  SetLength(BULK_DENSITY, 0);
  SetLength(DISTCOEF, 0);
end;

procedure TIstGridData.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
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

    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    if FSplitter[0] = 'POROSITY' then
    begin
//      SetLength(POROSITY, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        POROSITY := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'VOLFRAC' then
    begin
//      SetLength(VOLFRAC, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        VOLFRAC := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'ZETAIM' then
    begin
//      SetLength(ZETAIM, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        ZETAIM := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'CIM' then
    begin
      SetLength(CIM, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        CIM := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'DECAY' then
    begin
      SetLength(DECAY, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        DECAY := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'DECAY_SORBED' then
    begin
      SetLength(DECAY_SORBED, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        DECAY_SORBED := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'BULK_DENSITY' then
    begin
      SetLength(BULK_DENSITY, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        BULK_DENSITY := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'DISTCOEF' then
    begin
      SetLength(DISTCOEF, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        DISTCOEF := DoubleThreeDReader.FData;
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

{ TIst }

constructor TIst.Create(PackageType: string);
begin
  inherited;
  FOptions := TIstOptions.Create(PackageType);
  FGridData := TIstGridData.Create(PackageType);
end;

destructor TIst.Destroy;
begin
  FOptions.Free;
  FGridData.Free;
  inherited;
end;

procedure TIst.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
