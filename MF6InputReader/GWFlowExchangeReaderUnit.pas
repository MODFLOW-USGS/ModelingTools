unit GWFlowExchangeReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, CustomMf6PersistentUnit,
  System.Generics.Collections, AtsFileReaderUnit;

Type
  TCustomExchangeOptions = class(TCustomMf6Persistent)
  private
    FAuxiliary: TStringList;
    BOUNDNAMES: Boolean;
    PRINT_INPUT: Boolean;
    PRINT_FLOWS: Boolean;
    SAVE_FLOWS: Boolean;
    obs6_filename: string;
  protected
    procedure Initialize; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); virtual; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;

  end;

  TGWFlowExchangeOptions = class(TCustomExchangeOptions)
  private
    CELL_AVERAGING: string;
    VARIABLECV: Boolean;
    DEWATERED: Boolean;
    NEWTON: Boolean;
    XT3D: Boolean;
    gnc6_filename: string;
    mvr6_filename: string;
  protected
    procedure Initialize; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
  end;

  TGWTransportExchangeOptions = class(TCustomExchangeOptions)
  private
    GWFMODELNAME1: string;
    GWFMODELNAME2: string;
    ADV_SCHEME: string;
    DSP_XT3D_OFF: Boolean;
    DSP_XT3D_RHS: Boolean;
    mvt6_filename: string;
  protected
    procedure Initialize; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
  end;

  TGWFlowExchangeDimensions = class(TCustomMf6Persistent)
  private
    NEXG: Integer;
      protected
procedure Initialize; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  end;

  TGwFlowExchangeItem = class(TObject)
  private
    CellIdM1: TCellId;
    CellIdM2: TCellId;
    IHC: Integer;
    cl1: Extended;
    cl2: Extended;
    hwva: Extended;
    AUX: TExtendedList;
    BoundName: string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TGwFlowExchangeList = TObjectList<TGwFlowExchangeItem>;

  TGWFlowExchangeData = class(TCustomMf6Persistent)
  private
    FData: TGwFlowExchangeList;
    FDimensionsM1: TDimensions;
    FDimensionsM2: TDimensions;
    FAuxiliary: TStringList;
    FBOUNDNAMES: Boolean;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(DimensionsM1, DimensionsM2: TDimensions;
      Auxiliary: TStringList); reintroduce;
    destructor Destroy; override;
  end;

  TCustomExchange = class(TCustomMf6Persistent)
  private
    FOptions: TCustomExchangeOptions;
    FDimensions: TGWFlowExchangeDimensions;
    FData: TGWFlowExchangeData;
    FDimensionsM1: TDimensions;
    FDimensionsM2: TDimensions;
  public
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    constructor Create(DimensionsM1, DimensionsM2: TDimensions); reintroduce; virtual;
    destructor Destroy; override;
  end;

  TGWFlowExchange = class(TCustomExchange)
  public
    constructor Create(DimensionsM1, DimensionsM2: TDimensions); override;
  end;

  TGWTransportwExchange = class(TCustomExchange)
  public
    constructor Create(DimensionsM1, DimensionsM2: TDimensions); override;
  end;

implementation

uses
  ModelMuseUtilities;

resourcestring
  StrUnrecognizedExchOp = 'Unrecognized Exchange option in the following line.';
  StrUnrecognizedExchDI = 'Unrecognized Exchange dimension in the following line.';
  StrErrorReadingSOf = 'Error reading %s of an exchange in the following line:';
  StrNEXGDInTheGW = 'NEXG, "%d" in the DIMENSIONS block does not mat' +
  'ch the number of exchanges listed in the EXCHANGEDATA block "%d".';

{ TGWFlowExchangeOptions }

constructor TCustomExchangeOptions.Create;
begin
  FAuxiliary := TStringList.Create;
  inherited;
end;

destructor TCustomExchangeOptions.Destroy;
begin
  FAuxiliary.Free;
  inherited;
end;

procedure TGWFlowExchangeOptions.Initialize;
begin
  inherited;
  CELL_AVERAGING := '';
  VARIABLECV := False;
  DEWATERED := False;
  NEWTON := False;
  XT3D := False;
  obs6_filename := '';
end;

procedure TGWFlowExchangeOptions.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  ValidOptions: TStringList;
begin
  Initialize;
  ValidOptions := TStringList.Create;
  try
    ValidOptions.Add('CELL_AVERAGING');
    ValidOptions.Add('HARMONIC');
    ValidOptions.Add('LOGARITHMIC');
    ValidOptions.Add('MT-LMK');

    try
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
        if ReadEndOfSection(ALine, ErrorLine, 'OPTIONS', Unhandled) then
        begin
          Exit
        end;

        FSplitter.DelimitedText := ALine;
        Assert(FSplitter.Count > 0);
        if FSplitter.Count >= 1 then
        begin
          if FSplitter[0] = 'BOUNDNAMES' then
          begin
            BOUNDNAMES := True;
          end
          else if FSplitter[0] = 'PRINT_INPUT' then
          begin
            PRINT_INPUT := True;
          end
          else if FSplitter[0] = 'PRINT_FLOWS' then
          begin
            PRINT_FLOWS := True;
          end
          else if FSplitter[0] = 'SAVE_FLOWS' then
          begin
            SAVE_FLOWS := True;
          end
          else if FSplitter[0] = 'VARIABLECV' then
          begin
            VARIABLECV := True;
            if (FSplitter.Count >= 2) then
            begin
              if FSplitter[1] = 'DEWATERED' then
              begin
                DEWATERED := True;
              end
              else
              begin
                Unhandled.WriteLine(StrUnrecognizedExchOp);
                Unhandled.WriteLine(ErrorLine);
              end;
            end;
          end
          else if FSplitter[0] = 'NEWTON' then
          begin
            NEWTON := True;
          end
          else if FSplitter[0] = 'XT3D' then
          begin
            XT3D := True;
          end
          else if FSplitter.Count >= 2 then
          begin
            if (FSplitter[0] = 'CELL_AVERAGING') then
            begin
              CELL_AVERAGING := FSplitter[1];
              if ValidOptions.IndexOf(CELL_AVERAGING) < 0 then
              begin
                Unhandled.WriteLine(Format('Unrecognized cell averaging method "%s".', [CELL_AVERAGING]));
              end;
            end
            else if FSplitter.Count >= 3 then
            begin
              if (FSplitter[0] = 'GNC6') and (FSplitter[1] = 'FILEIN') then
              begin
                gnc6_filename := FSplitter[2];
              end
              else if (FSplitter[0] = 'MVR6') and (FSplitter[1] = 'FILEIN') then
              begin
                mvr6_filename := FSplitter[2];
              end
              else if (FSplitter[0] = 'OBS6') and (FSplitter[1] = 'FILEIN') then
              begin
                obs6_filename := FSplitter[2];
              end
              else
              begin
                Unhandled.WriteLine(StrUnrecognizedExchOp);
                Unhandled.WriteLine(ErrorLine);
              end;
            end
            else
            begin
              Unhandled.WriteLine(StrUnrecognizedExchOp);
              Unhandled.WriteLine(ErrorLine);
            end;
          end
        end
        else
        begin
          Unhandled.WriteLine(StrUnrecognizedExchOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end;
    finally
    end;
  finally
    ValidOptions.Free;
  end;
end;

{ TGWFlowExchangeDimensions }

procedure TGWFlowExchangeDimensions.Initialize;
begin
  NEXG := 0;
  inherited;

end;

procedure TGWFlowExchangeDimensions.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter);
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
    ALine := UpperCase(ALine);
    if ReadEndOfSection(ALine, ErrorLine, 'DIMENSIONS', Unhandled) then
    begin
      Exit
    end;

    FSplitter.DelimitedText := ALine;
    Assert(FSplitter.Count > 0);
    if FSplitter.Count >= 2 then
    begin
      if FSplitter[0] = 'NEXG' then
      begin
        if not TryStrToInt(FSplitter[0], NEXG) then
        begin
          Unhandled.WriteLine(StrUnrecognizedExchDI);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
    end
    else
    begin
      Unhandled.WriteLine(StrUnrecognizedExchDI);
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TGWFlowExchangeData }

constructor TGWFlowExchangeData.Create(DimensionsM1, DimensionsM2: TDimensions;
  Auxiliary: TStringList);
begin
  FData := TGwFlowExchangeList.Create;
  FDimensionsM1 := DimensionsM1;
  FDimensionsM2 := DimensionsM2;
  FAuxiliary := Auxiliary;
  inherited Create;

end;

destructor TGWFlowExchangeData.Destroy;
begin
  FData.Free;
  inherited;
end;

procedure TGWFlowExchangeData.Initialize;
begin
  inherited;
  FData.Clear;
end;

procedure TGWFlowExchangeData.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  SectionName: string;
  Exchange: TGwFlowExchangeItem;
  Index: Integer;
  AuxIndex: Integer;
  Value: Extended;
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

    SectionName := 'EXCHANGEDATA';
    if ReadEndOfSection(ALine, ErrorLine, SectionName, Unhandled) then
    begin
      Exit;
    end;

    Exchange := TGwFlowExchangeItem.Create;
    FData.Add(Exchange);
    FSplitter.DelimitedText := ALine;
    Index := 0;
    case FDimensionsM1.DimensionCount of
      1:
        begin
          if not TryStrToInt(FSplitter[Index], Exchange.CellIdM1.Column) then
          begin
            Unhandled.WriteLine(Format(StrErrorReadingSOf, ['cellidm1']));
            Unhandled.WriteLine(ErrorLine);
            Continue;
          end;
          Inc(Index);
        end;
      2:
        begin
          if not TryStrToInt(FSplitter[Index], Exchange.CellIdM1.Layer) then
          begin
            Unhandled.WriteLine(Format(StrErrorReadingSOf, ['cellidm1']));
            Unhandled.WriteLine(ErrorLine);
            Continue;
          end;
          Inc(Index);

          if not TryStrToInt(FSplitter[Index], Exchange.CellIdM1.Layer) then
          begin
            Unhandled.WriteLine(Format(StrErrorReadingSOf, ['cellidm1']));
            Unhandled.WriteLine(ErrorLine);
            Continue;
          end;
          Inc(Index);
        end;
      3:
        begin
          if not TryStrToInt(FSplitter[Index], Exchange.CellIdM1.Layer) then
          begin
            Unhandled.WriteLine(Format(StrErrorReadingSOf, ['cellidm1']));
            Unhandled.WriteLine(ErrorLine);
            Continue;
          end;
          Inc(Index);

          if not TryStrToInt(FSplitter[Index], Exchange.CellIdM1.Row) then
          begin
            Unhandled.WriteLine(Format(StrErrorReadingSOf, ['cellidm1']));
            Unhandled.WriteLine(ErrorLine);
            Continue;
          end;
          Inc(Index);

          if not TryStrToInt(FSplitter[Index], Exchange.CellIdM1.Layer) then
          begin
            Unhandled.WriteLine(Format(StrErrorReadingSOf, ['cellidm1']));
            Unhandled.WriteLine(ErrorLine);
            Continue;
          end;
          Inc(Index);
        end;
      else
        begin
          Unhandled.WriteLine(Format(StrErrorReadingSOf, ['cellidm1']));
          Unhandled.WriteLine(ErrorLine);
          Continue;
        end;
    end;

    case FDimensionsM2.DimensionCount of
      1:
        begin
          if not TryStrToInt(FSplitter[Index], Exchange.CellIdM2.Column) then
          begin
            Unhandled.WriteLine(Format(StrErrorReadingSOf, ['cellidm2']));
            Unhandled.WriteLine(ErrorLine);
            Continue;
          end;
          Inc(Index);
        end;
      2:
        begin
          if not TryStrToInt(FSplitter[Index], Exchange.CellIdM2.Layer) then
          begin
            Unhandled.WriteLine(Format(StrErrorReadingSOf, ['cellidm2']));
            Unhandled.WriteLine(ErrorLine);
            Continue;
          end;
          Inc(Index);

          if not TryStrToInt(FSplitter[Index], Exchange.CellIdM2.Layer) then
          begin
            Unhandled.WriteLine(Format(StrErrorReadingSOf, ['cellidm2']));
            Unhandled.WriteLine(ErrorLine);
            Continue;
          end;
          Inc(Index);
        end;
      3:
        begin
          if not TryStrToInt(FSplitter[Index], Exchange.CellIdM2.Layer) then
          begin
            Unhandled.WriteLine(Format(StrErrorReadingSOf, ['cellidm2']));
            Unhandled.WriteLine(ErrorLine);
            Continue;
          end;
          Inc(Index);

          if not TryStrToInt(FSplitter[Index], Exchange.CellIdM2.Row) then
          begin
            Unhandled.WriteLine(Format(StrErrorReadingSOf, ['cellidm2']));
            Unhandled.WriteLine(ErrorLine);
            Continue;
          end;
          Inc(Index);

          if not TryStrToInt(FSplitter[Index], Exchange.CellIdM2.Layer) then
          begin
            Unhandled.WriteLine(Format(StrErrorReadingSOf, ['cellidm2']));
            Unhandled.WriteLine(ErrorLine);
            Continue;
          end;
          Inc(Index);
        end;
      else
        begin
          Unhandled.WriteLine(Format(StrErrorReadingSOf, ['cellidm2']));
          Unhandled.WriteLine(ErrorLine);
          Continue;
        end;
    end;

    if not TryStrToInt(FSplitter[Index], Exchange.IHC) then
    begin
      Unhandled.WriteLine(Format(StrErrorReadingSOf, ['ihc']));
      Unhandled.WriteLine(ErrorLine);
      Continue;
    end;
    Inc(Index);

    if not TryFortranStrToFloat(FSplitter[Index], Exchange.cl1) then
    begin
      Unhandled.WriteLine(Format(StrErrorReadingSOf, ['cl1']));
      Unhandled.WriteLine(ErrorLine);
      Continue;
    end;
    Inc(Index);

    if not TryFortranStrToFloat(FSplitter[Index], Exchange.cl2) then
    begin
      Unhandled.WriteLine(Format(StrErrorReadingSOf, ['cl2']));
      Unhandled.WriteLine(ErrorLine);
      Continue;
    end;
    Inc(Index);

    if not TryFortranStrToFloat(FSplitter[Index], Exchange.hwva) then
    begin
      Unhandled.WriteLine(Format(StrErrorReadingSOf, ['hwva']));
      Unhandled.WriteLine(ErrorLine);
      Continue;
    end;
    Inc(Index);

    for AuxIndex := 0 to FAuxiliary.Count - 1 do
    begin
      if TryFortranStrToFloat(FSplitter[Index], Value) then
      begin
        Exchange.AUX.Add(Value)
      end
      else
      begin
        Unhandled.WriteLine(Format(StrErrorReadingSOf, ['hwva']));
        Unhandled.WriteLine(ErrorLine);
        break;
      end;
      Inc(Index);
    end;
    if FBOUNDNAMES then
    begin
      Exchange.BoundName := FSplitter[Index];
    end;
  end;
end;

{ TExchangeItem }

constructor TGwFlowExchangeItem.Create;
begin
  inherited;
  CellIdM1.Initialize;
  CellIdM2.Initialize;
  IHC := 0;
  cl1 := 0;
  cl2 := 0;
  hwva := 0;
  AUX := TExtendedList.Create;
  BoundName := '';
end;

destructor TGwFlowExchangeItem.Destroy;
begin
  AUX.Free;
  inherited;
end;

{ TGWFlowExchange }

constructor TGWFlowExchange.Create(DimensionsM1, DimensionsM2: TDimensions);
begin
  FOptions := TGWFlowExchangeOptions.Create;
  inherited Create(DimensionsM1, DimensionsM2);
end;


procedure TCustomExchange.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
begin
  try
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
      if Pos('BEGIN', ALine) = 1 then
      begin
        if Trim(Copy(ALine,Length('BEGIN')+1,1)) <> '' then
        begin
          Unhandled.WriteLine(StrUnrecognizedExchOp);
          Unhandled.WriteLine(ErrorLine);
          Continue;
        end;
        ALine := Trim(Copy(ALine, Length('BEGIN')+1, MaxInt)) ;
        if Pos('OPTIONS', ALine) = 1 then
        begin
          if Trim(Copy(ALine,Length('OPTIONS')+1,1)) <> '' then
          begin
            Unhandled.WriteLine(StrUnrecognizedExchOp);
            Unhandled.WriteLine(ErrorLine);
            Continue;
          end;
          FOptions.Read(Stream, Unhandled)
        end
        else if Pos('DIMENSIONS', ALine) = 1 then
        begin
          if Trim(Copy(ALine,Length('DIMENSIONS')+1,1)) <> '' then
          begin
            Unhandled.WriteLine(StrUnrecognizedExchOp);
            Unhandled.WriteLine(ErrorLine);
            Continue;
          end;
          FDimensions.Read(Stream, Unhandled)
        end
        else if Pos('EXCHANGEDATA', ALine) = 1 then
        begin
          FData.FBOUNDNAMES := FOptions.BOUNDNAMES;
          if Trim(Copy(ALine,Length('EXCHANGEDATA')+1,1)) <> '' then
          begin
            Unhandled.WriteLine(StrUnrecognizedExchOp);
            Unhandled.WriteLine(ErrorLine);
            Continue;
          end;
          FData.Read(Stream, Unhandled);
        end
        else
        begin
          Unhandled.WriteLine(StrUnrecognizedExchOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end;
    end;
  finally
    if FDimensions.NEXG <> FData.FData.Count then
    begin
      Unhandled.WriteLine(Format(StrNEXGDInTheGW,
        [FDimensions.NEXG, FData.FData.Count]));
    end;
  end;
end;

{ TCustomExchangeOptions }

procedure TCustomExchangeOptions.Initialize;
begin
  inherited;
  FAuxiliary.Clear;
  BOUNDNAMES := False;
  PRINT_INPUT := False;
  PRINT_FLOWS := False;
  SAVE_FLOWS := False;
  obs6_filename := '';
end;

{ TGWTransportwExchange }

constructor TGWTransportwExchange.Create(DimensionsM1,
  DimensionsM2: TDimensions);
begin
  FOptions := TGWTransportExchangeOptions.Create;
  inherited Create(DimensionsM1, DimensionsM2);
end;


{ TGWTransportExchangeOptions }

procedure TGWTransportExchangeOptions.Initialize;
begin
  inherited;
  GWFMODELNAME1 := '';
  GWFMODELNAME2 := '';
  ADV_SCHEME := '';
  DSP_XT3D_OFF := False;
  DSP_XT3D_RHS := False;
  mvt6_filename := '';
end;

procedure TGWTransportExchangeOptions.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  ValidOptions: TStringList;
begin
  Initialize;
  ValidOptions := TStringList.Create;
  try
    ValidOptions.Add('UPSTREAM');
    ValidOptions.Add('CENTRAL');
    ValidOptions.Add('TVD');

    try
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
        if ReadEndOfSection(ALine, ErrorLine, 'OPTIONS', Unhandled) then
        begin
          Exit
        end;

        FSplitter.DelimitedText := ALine;
        Assert(FSplitter.Count > 0);
        if FSplitter.Count >= 1 then
        begin
          if FSplitter[0] = 'BOUNDNAMES' then
          begin
            BOUNDNAMES := True;
          end
          else if FSplitter[0] = 'PRINT_INPUT' then
          begin
            PRINT_INPUT := True;
          end
          else if FSplitter[0] = 'PRINT_FLOWS' then
          begin
            PRINT_FLOWS := True;
          end
          else if FSplitter[0] = 'SAVE_FLOWS' then
          begin
            SAVE_FLOWS := True;
          end
          else if FSplitter[0] = 'DSP_XT3D_OFF' then
          begin
            DSP_XT3D_OFF := True;
          end
          else if FSplitter[0] = 'DSP_XT3D_RHS' then
          begin
            DSP_XT3D_RHS := True;
          end
          else if FSplitter.Count >= 2 then
          begin
            if (FSplitter[0] = 'ADV_SCHEME') then
            begin
              ADV_SCHEME := FSplitter[1];
              if ValidOptions.IndexOf(ADV_SCHEME) < 0 then
              begin
                Unhandled.WriteLine(Format('Unrecognized advection scheme "%s".', [ADV_SCHEME]));
              end;
            end
            else if FSplitter.Count >= 3 then
            begin
              if (FSplitter[0] = 'MVT6') and (FSplitter[1] = 'FILEIN') then
              begin
                mvt6_filename := FSplitter[2];
              end
              else if (FSplitter[0] = 'OBS6') and (FSplitter[1] = 'FILEIN') then
              begin
                obs6_filename := FSplitter[2];
              end
              else
              begin
                Unhandled.WriteLine(StrUnrecognizedExchOp);
                Unhandled.WriteLine(ErrorLine);
              end;
            end
            else
            begin
              Unhandled.WriteLine(StrUnrecognizedExchOp);
              Unhandled.WriteLine(ErrorLine);
            end;
          end
        end
        else
        begin
          Unhandled.WriteLine(StrUnrecognizedExchOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end;
    finally
    end;
  finally
    ValidOptions.Free;
  end;
end;

{ TCustomExchange }

constructor TCustomExchange.Create(DimensionsM1, DimensionsM2: TDimensions);
begin
  FDimensionsM1 := DimensionsM1;
  FDimensionsM2 := DimensionsM2;

  FDimensions := TGWFlowExchangeDimensions.Create;
  FData := TGWFlowExchangeData.Create(DimensionsM1, DimensionsM2,
    FOptions.FAuxiliary);
  inherited Create;
end;

destructor TCustomExchange.Destroy;
begin
  FOptions.Free;
  FDimensions.Free;
  FData.Free;

  inherited;
end;

end.
