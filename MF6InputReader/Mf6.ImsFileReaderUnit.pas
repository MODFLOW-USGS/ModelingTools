unit Mf6.ImsFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections, Mf6.AtsFileReaderUnit;

type
  TImsOptions = class(TCustomMf6Persistent)
  private
    FPRINT_OPTION: string;
    FCOMPLEXITY: string;
    FCSV_OUTER_OUTPUT: string;
    FCSV_INNER_OUTPUT: string;
    FNO_PTC: string;
    Fno_ptc_option: string;
    FATS_OUTER_MAXIMUM_FRACTION: Extended;
  public
    procedure Initialize; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    property PRINT_OPTION: string read FPRINT_OPTION;
    property COMPLEXITY: string read FCOMPLEXITY;
    property CSV_OUTER_OUTPUT: string read FCSV_OUTER_OUTPUT;
    property CSV_INNER_OUTPUT: string read FCSV_INNER_OUTPUT;
    property NO_PTC: string read FNO_PTC;
    property no_ptc_option: string read Fno_ptc_option;
    property ATS_OUTER_MAXIMUM_FRACTION: Extended read FATS_OUTER_MAXIMUM_FRACTION;
  end;

  TImsNonLinear = class(TCustomMf6Persistent)
  private
    FOUTER_DVCLOSE: Extended;
    FOUTER_MAXIMUM: Integer;
    FUNDER_RELAXATION: string;
    FUNDER_RELAXATION_GAMMA: Extended;
    FUNDER_RELAXATION_THETA: Extended;
    FUNDER_RELAXATION_KAPPA: Extended;
    FUNDER_RELAXATION_MOMENTUM: Extended;
    FBACKTRACKING_NUMBER: Integer;
    FBACKTRACKING_TOLERANCE: Extended;
    FBACKTRACKING_REDUCTION_FACTOR: Extended;
    FBACKTRACKING_RESIDUAL_LIMIT: Extended;
  protected
    procedure Initialize; override;
  public
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    property OUTER_DVCLOSE: Extended read FOUTER_DVCLOSE;
    property OUTER_MAXIMUM: Integer read FOUTER_MAXIMUM;
    property UNDER_RELAXATION: string read FUNDER_RELAXATION;
    property UNDER_RELAXATION_GAMMA: Extended read FUNDER_RELAXATION_GAMMA;
    property UNDER_RELAXATION_THETA: Extended read FUNDER_RELAXATION_THETA;
    property UNDER_RELAXATION_KAPPA: Extended read FUNDER_RELAXATION_KAPPA;
    property UNDER_RELAXATION_MOMENTUM: Extended read FUNDER_RELAXATION_MOMENTUM;
    property BACKTRACKING_NUMBER: Integer read FBACKTRACKING_NUMBER;
    property BACKTRACKING_TOLERANCE: Extended read FBACKTRACKING_TOLERANCE;
    property BACKTRACKING_REDUCTION_FACTOR: Extended read FBACKTRACKING_REDUCTION_FACTOR;
    property BACKTRACKING_RESIDUAL_LIMIT: Extended read FBACKTRACKING_RESIDUAL_LIMIT;
  end;

  TImsLinear = class(TCustomMf6Persistent)
  private
    FINNER_MAXIMUM: Integer;
    FINNER_DVCLOSE: Extended;
    FINNER_RCLOSE: Extended;
    Frclose_option: string;
    FLINEAR_ACCELERATION: string;
    FRELAXATION_FACTOR: Extended;
    FPRECONDITIONER_LEVELS: Integer;
    FPRECONDITIONER_DROP_TOLERANCE: Extended;
    FNUMBER_ORTHOGONALIZATIONS: Integer;
    FSCALING_METHOD: string;
    FREORDERING_METHOD: string;
  protected
    procedure Initialize; override;
  public
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    property INNER_MAXIMUM: Integer read FINNER_MAXIMUM;
    property INNER_DVCLOSE: Extended read FINNER_DVCLOSE;
    property INNER_RCLOSE: Extended read FINNER_RCLOSE;
    property rclose_option: string read Frclose_option;
    property LINEAR_ACCELERATION: string read FLINEAR_ACCELERATION;
    property RELAXATION_FACTOR: Extended read FRELAXATION_FACTOR;
    property PRECONDITIONER_LEVELS: Integer read FPRECONDITIONER_LEVELS;
    property PRECONDITIONER_DROP_TOLERANCE: Extended read FPRECONDITIONER_DROP_TOLERANCE;
    property NUMBER_ORTHOGONALIZATIONS: Integer read FNUMBER_ORTHOGONALIZATIONS;
    property SCALING_METHOD: string read FSCALING_METHOD;
    property REORDERING_METHOD: string read FREORDERING_METHOD;
  end;

  TIms = class(TCustomMf6Persistent)
  private
    FOptions: TImsOptions;
    FNonLinear: TImsNonLinear;
    FLinear: TImsLinear;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    property Options: TImsOptions read FOptions;
    property NonLinear: TImsNonLinear read FNonLinear;
    property Linear: TImsLinear read FLinear;
  end;

implementation

uses
  ModelMuseUtilities;

resourcestring
  StrUnrecognizedImsOp = 'Unrecognized IMS option in the following line.';

{ TImsOptions }

procedure TImsOptions.Initialize;
begin
  inherited;
  FPRINT_OPTION := '';
  FCOMPLEXITY := '';
  FCSV_OUTER_OUTPUT := '';
  FCSV_INNER_OUTPUT := '';
  FNO_PTC := '';
  Fno_ptc_option := '';
  FATS_OUTER_MAXIMUM_FRACTION := -1;
end;

procedure TImsOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    else if FSplitter.Count >= 1 then
    begin
      ALine := UpperCase(ALine);
      FSplitter.DelimitedText := ALine;
      if FSplitter[0] = 'NO_PTC' then
      begin
        FNO_PTC := FSplitter[0];
      end
      else if FSplitter.Count >= 2 then
      begin
        ALine := UpperCase(ALine);
        FSplitter.DelimitedText := ALine;
        if FSplitter[0] = 'PRINT_OPTION' then
        begin
          FPRINT_OPTION := FSplitter[1];
          if (FPRINT_OPTION <> 'NONE') and (FPRINT_OPTION <> 'SUMMARY') and (FPRINT_OPTION <> 'ALL') then
          begin
            Unhandled.WriteLine(StrUnrecognizedImsOp);
          end;
        end
        else if FSplitter[0] = 'COMPLEXITY' then
        begin
          FCOMPLEXITY := FSplitter[1];
          if (FCOMPLEXITY <> 'SIMPLE') and (FCOMPLEXITY <> 'MODERATE') and (FCOMPLEXITY <> 'COMPLEX') then
          begin
            Unhandled.WriteLine(StrUnrecognizedImsOp);
          end;
        end
        else if FSplitter[0] = 'NO_PTC' then
        begin
          FNO_PTC := FSplitter[0];
          Fno_ptc_option := FSplitter[1];
          if (Fno_ptc_option <> 'FIRST') and (Fno_ptc_option <> 'ALL') then
          begin
            Unhandled.WriteLine(StrUnrecognizedImsOp);
          end;
        end
        else if FSplitter[0] = 'ATS_OUTER_MAXIMUM_FRACTION' then
        begin
          if not TryFortranStrToFloat(FSplitter[1], FATS_OUTER_MAXIMUM_FRACTION) then
          begin
            Unhandled.WriteLine(StrUnrecognizedImsOp);
          end;
        end
        else if FSplitter.Count >= 3 then
        begin
          if (UpperCase(FSplitter[0]) = 'CSV_OUTER_OUTPUT')
            or (UpperCase(FSplitter[0]) = 'CSV_OUTPUT')
            then
          begin
            if (UpperCase(FSplitter[1]) = 'FILEOUT') then
            begin
              FCSV_OUTER_OUTPUT := FSplitter[2];
            end
            else
            begin
              Unhandled.WriteLine(StrUnrecognizedImsOp);
              Unhandled.WriteLine(ErrorLine);
            end;
          end
          else if (UpperCase(FSplitter[0]) = 'CSV_INNER_OUTPUT') then
          begin
            if (UpperCase(FSplitter[1]) = 'FILEOUT') then
            begin
              FCSV_INNER_OUTPUT := FSplitter[2];
            end
            else
            begin
              Unhandled.WriteLine(StrUnrecognizedImsOp);
              Unhandled.WriteLine(ErrorLine);
            end;
          end
          else
          begin
            Unhandled.WriteLine(StrUnrecognizedImsOp);
            Unhandled.WriteLine(ErrorLine);
          end;
        end
        else
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine(StrUnrecognizedImsOp);
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine(StrUnrecognizedImsOp);
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TImsNonLinear }

procedure TImsNonLinear.Initialize;
begin
  inherited;
  FOUTER_DVCLOSE := -1;
  FOUTER_MAXIMUM := -1;
  FUNDER_RELAXATION := '';
  FUNDER_RELAXATION_GAMMA := -1;
  FUNDER_RELAXATION_THETA := -1;
  FUNDER_RELAXATION_KAPPA := -1;
  FUNDER_RELAXATION_MOMENTUM := -1;
  FBACKTRACKING_NUMBER := -1;
  FBACKTRACKING_TOLERANCE := -1;
  FBACKTRACKING_REDUCTION_FACTOR := -1;
  FBACKTRACKING_RESIDUAL_LIMIT := -1;

end;

procedure TImsNonLinear.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    if ReadEndOfSection(ALine, ErrorLine, 'NONLINEAR', Unhandled) then
    begin
      Exit
    end;

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'NONLINEAR') then
    begin
      // do nothing
    end
    else if FSplitter.Count >= 2 then
    begin
      if (FSplitter[0] = 'OUTER_DVCLOSE') or (FSplitter[0] = 'OUTER_HCLOSE') then
      begin
        if not TryFortranStrToFloat(FSplitter[1], FOUTER_DVCLOSE) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'OUTER_MAXIMUM' then
      begin
        if not TryStrToInt(FSplitter[1], FOUTER_MAXIMUM) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'UNDER_RELAXATION' then
      begin
        FUNDER_RELAXATION := FSplitter[1];
        if (FUNDER_RELAXATION <> 'NONE') and (FUNDER_RELAXATION <> 'SIMPLE')
          and (FUNDER_RELAXATION <> 'COOLEY') and (FUNDER_RELAXATION <> 'DBD') then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'UNDER_RELAXATION_GAMMA' then
      begin
        if not TryFortranStrToFloat(FSplitter[1], FUNDER_RELAXATION_GAMMA) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'UNDER_RELAXATION_THETA' then
      begin
        if not TryFortranStrToFloat(FSplitter[1], FUNDER_RELAXATION_THETA) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'UNDER_RELAXATION_KAPPA' then
      begin
        if not TryFortranStrToFloat(FSplitter[1], FUNDER_RELAXATION_KAPPA) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'UNDER_RELAXATION_MOMENTUM' then
      begin
        if not TryFortranStrToFloat(FSplitter[1], FUNDER_RELAXATION_MOMENTUM) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'BACKTRACKING_NUMBER' then
      begin
        if not TryStrToInt(FSplitter[1], FBACKTRACKING_NUMBER) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'BACKTRACKING_TOLERANCE' then
      begin
        if not TryFortranStrToFloat(FSplitter[1], FBACKTRACKING_TOLERANCE) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'BACKTRACKING_REDUCTION_FACTOR' then
      begin
        if not TryFortranStrToFloat(FSplitter[1], FBACKTRACKING_REDUCTION_FACTOR) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'BACKTRACKING_RESIDUAL_LIMIT' then
      begin
        if not TryFortranStrToFloat(FSplitter[1], FBACKTRACKING_RESIDUAL_LIMIT) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'OUTER_RCLOSEBND' then
      begin
        // ignore. This option is deprecated.
      end
      else
      begin
        Unhandled.WriteLine(StrUnrecognizedImsOp);
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine(StrUnrecognizedImsOp);
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TImsLinear }

procedure TImsLinear.Initialize;
begin
  inherited;
  FINNER_MAXIMUM := -1;
  FINNER_DVCLOSE := -1;
  FINNER_RCLOSE := -1;
  Frclose_option := '';
  FLINEAR_ACCELERATION := '';
  FRELAXATION_FACTOR := -1;
  FPRECONDITIONER_LEVELS := -1;
  FPRECONDITIONER_DROP_TOLERANCE := -1;
  FNUMBER_ORTHOGONALIZATIONS := -1;
  FSCALING_METHOD := '';
  FREORDERING_METHOD := '';
end;

procedure TImsLinear.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    if ReadEndOfSection(ALine, ErrorLine, 'LINEAR', Unhandled) then
    begin
      Exit
    end;

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'LINEAR') then
    begin
      // do nothing
    end
    else if FSplitter.Count >= 2 then
    begin
      if FSplitter[0] = 'INNER_MAXIMUM' then
      begin
        if not TryStrToInt(FSplitter[1], FINNER_MAXIMUM) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if (FSplitter[0] = 'INNER_DVCLOSE') or (FSplitter[0] = 'INNER_HCLOSE') then
      begin
        if not TryFortranStrToFloat(FSplitter[1], FINNER_DVCLOSE) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'INNER_RCLOSE' then
      begin
        if not TryFortranStrToFloat(FSplitter[1], FINNER_RCLOSE) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'LINEAR_ACCELERATION' then
      begin
        FLINEAR_ACCELERATION := FSplitter[1];
        if (FLINEAR_ACCELERATION <> 'CG') and (FLINEAR_ACCELERATION <> 'BICGSTAB') then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'RELAXATION_FACTOR' then
      begin
        if not TryFortranStrToFloat(FSplitter[1], FRELAXATION_FACTOR) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'PRECONDITIONER_LEVELS' then
      begin
        if not TryStrToInt(FSplitter[1], FPRECONDITIONER_LEVELS) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'PRECONDITIONER_DROP_TOLERANCE' then
      begin
        if not TryFortranStrToFloat(FSplitter[1], FPRECONDITIONER_DROP_TOLERANCE) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'NUMBER_ORTHOGONALIZATIONS' then
      begin
        if not TryStrToInt(FSplitter[1], FNUMBER_ORTHOGONALIZATIONS) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'SCALING_METHOD' then
      begin
        FSCALING_METHOD := FSplitter[1];
        if (FSCALING_METHOD <> 'NONE') and (FSCALING_METHOD <> 'DIAGONAL') and (FSCALING_METHOD <> 'L2NORM') then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'REORDERING_METHOD' then
      begin
        FREORDERING_METHOD := FSplitter[1];
        if (FREORDERING_METHOD <> 'NONE') and (FREORDERING_METHOD <> 'RCM') and (FREORDERING_METHOD <> 'MD') then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter.Count >= 3 then
      begin
        if FSplitter[0] = 'INNER_RCLOSE' then
        begin
          if not TryFortranStrToFloat(FSplitter[1], FINNER_RCLOSE) then
          begin
            Unhandled.WriteLine(StrUnrecognizedImsOp);
            Unhandled.WriteLine(ErrorLine);
          end;
          Frclose_option := FSplitter[2];
          if (Frclose_option <> 'STRICT') then
          begin
            Unhandled.WriteLine(StrUnrecognizedImsOp);
            Unhandled.WriteLine(ErrorLine);
          end;
        end
      end
      else
      begin
        Unhandled.WriteLine(StrUnrecognizedImsOp);
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine(StrUnrecognizedImsOp);
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TIms }

constructor TIms.Create(PackageType: string);
begin
  FOptions := TImsOptions.Create(PackageType);
  FNonLinear := TImsNonLinear.Create(PackageType);
  FLinear := TImsLinear.Create(PackageType);
  inherited;

end;

destructor TIms.Destroy;
begin
  FOptions.Free;
  FNonLinear.Free;
  FLinear.Free;
  inherited;
end;

procedure TIms.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
begin
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading IMS package');
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
    if Pos('BEGIN', ALine) = 1 then
    begin
      if Trim(Copy(ALine,Length('BEGIN')+1,1)) <> '' then
      begin
        Unhandled.WriteLine(StrUnrecognizedImsOp);
        Unhandled.WriteLine(ErrorLine);
        Continue;
      end;
      ALine := Trim(Copy(ALine, Length('BEGIN')+1, MaxInt)) ;
      if Pos('OPTIONS', ALine) = 1 then
      begin
        if Trim(Copy(ALine,Length('OPTIONS')+1,1)) <> '' then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
          Continue;
        end;
        FOptions.Read(Stream, Unhandled)
      end
      else if Pos('NONLINEAR', ALine) = 1 then
      begin
        if Trim(Copy(ALine,Length('NONLINEAR')+1,1)) <> '' then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
          Continue;
        end;
        FNonLinear.Read(Stream, Unhandled)
      end
      else if Pos('LINEAR', ALine) = 1 then
      begin
        if Trim(Copy(ALine,Length('LINEAR')+1,1)) <> '' then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
          Continue;
        end;
        FLinear.Read(Stream, Unhandled);
      end
      else
      begin
        Unhandled.WriteLine(StrUnrecognizedImsOp);
        Unhandled.WriteLine(ErrorLine);
      end;
    end;
  end;
end;

end.
