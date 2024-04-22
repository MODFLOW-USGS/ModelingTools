unit Mf6.IstFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TIstOptions = class(TCustomMf6Persistent)
  private
    FSAVE_FLOWS: Boolean;
    FBUDGET: Boolean;
    FBUDGETCSV: Boolean;
    FSORPTION: Boolean;
    FFIRST_ORDER_DECAY: Boolean;
    FZERO_ORDER_DECAY: Boolean;
    FCIM: Boolean;
    FCIM_PRINT_FORMAT: TPrintFormat;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    property SAVE_FLOWS: Boolean read FSAVE_FLOWS;
    property BUDGET: Boolean read FBUDGET;
    property BUDGETCSV: Boolean read FBUDGETCSV;
    property SORPTION: Boolean read FSORPTION;
    property FIRST_ORDER_DECAY: Boolean read FFIRST_ORDER_DECAY;
    property ZERO_ORDER_DECAY: Boolean read FZERO_ORDER_DECAY;
    property CIM: Boolean read FCIM;
    property CIM_PRINT_FORMAT: TPrintFormat read FCIM_PRINT_FORMAT;
  end;

  TIstGridData = class(TCustomMf6Persistent)
  private
    FPOROSITY: TDArray3D;
    FVOLFRAC: TDArray3D;
    FZETAIM: TDArray3D;
    FCIM: TDArray3D;
    FDECAY: TDArray3D;
    FDECAY_SORBED: TDArray3D;
    FBULK_DENSITY: TDArray3D;
    FDISTCOEF: TDArray3D;
    FDimensions: TDimensions;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    property POROSITY: TDArray3D read FPOROSITY;
    property VOLFRAC: TDArray3D read FVOLFRAC;
    property ZETAIM: TDArray3D read FZETAIM;
    property CIM: TDArray3D read FCIM;
    property DECAY: TDArray3D read FDECAY;
    property DECAY_SORBED: TDArray3D read FDECAY_SORBED;
    property BULK_DENSITY: TDArray3D read FBULK_DENSITY;
    property DISTCOEF: TDArray3D read FDISTCOEF;
  end;

  TIst = class(TDimensionedPackageReader)
  private
    FOptions: TIstOptions;
    FGridData: TIstGridData;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      const NPER: Integer); override;
    property Options: TIstOptions read FOptions;
    property GridData: TIstGridData read FGridData;
  end;

implementation

{ TIstOptions }

procedure TIstOptions.Initialize;
begin
  inherited;
  FSAVE_FLOWS := False;
  FBUDGET := False;
  FBUDGETCSV := False;
  FSORPTION := False;
  FFIRST_ORDER_DECAY := False;
  FZERO_ORDER_DECAY := False;
  FCIM := False;
  FCIM_PRINT_FORMAT.Initialize;
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
      FSAVE_FLOWS := True;
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
    else if (FSplitter[0] = 'SORPTION') then
    begin
      FSORPTION := True;
    end
    else if FSplitter[0] = 'FIRST_ORDER_DECAY' then
    begin
      FFIRST_ORDER_DECAY := True;
    end
    else if FSplitter[0] = 'ZERO_ORDER_DECAY' then
    begin
      FZERO_ORDER_DECAY := True;
    end
    else if (FSplitter[0] = 'CIM')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEOUT') then
    begin
      FCIM := True;
    end
    else if (FSplitter[0] = 'CIM')
      and (FSplitter.Count >= 8)
      and (FSplitter[1] = 'PRINT_FORMAT') then
    begin
      ReadPrintFormat(ErrorLine, Unhandled, FPackageType, FCIM_PRINT_FORMAT);
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
  SetLength(FPOROSITY, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
  SetLength(FVOLFRAC, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
  SetLength(FZETAIM, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
  SetLength(FCIM, 0);
  SetLength(FDECAY, 0);
  SetLength(FDECAY_SORBED, 0);
  SetLength(FBULK_DENSITY, 0);
  SetLength(FDISTCOEF, 0);
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
    else if FSplitter[0] = 'VOLFRAC' then
    begin
//      SetLength(VOLFRAC, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FVOLFRAC := DoubleThreeDReader.FData;
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
        FZETAIM := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'CIM' then
    begin
      SetLength(FCIM, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FCIM := DoubleThreeDReader.FData;
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

procedure TIst.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
var
  ALine: string;
  ErrorLine: string;
begin
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading IST package');
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
