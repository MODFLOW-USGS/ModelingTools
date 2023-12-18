unit Mf6.ImsFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections, Mf6.AtsFileReaderUnit;

type
  TImsOptions = class(TCustomMf6Persistent)
  private
    PRINT_OPTION: string;
    COMPLEXITY: string;
    CSV_OUTER_OUTPUT: string;
    CSV_INNER_OUTPUT: string;
    NO_PTC: string;
    no_ptc_option: string;
    ATS_OUTER_MAXIMUM_FRACTION: Extended;
  public
    procedure Initialize; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  end;

  TImsNonLinear = class(TCustomMf6Persistent)
  private
    OUTER_DVCLOSE: Extended;
    OUTER_MAXIMUM: Integer;
    UNDER_RELAXATION: string;
    UNDER_RELAXATION_GAMMA: Extended;
    UNDER_RELAXATION_THETA: Extended;
    UNDER_RELAXATION_KAPPA: Extended;
    UNDER_RELAXATION_MOMENTUM: Extended;
    BACKTRACKING_NUMBER: Integer;
    BACKTRACKING_TOLERANCE: Extended;
    BACKTRACKING_REDUCTION_FACTOR: Extended;
    BACKTRACKING_RESIDUAL_LIMIT: Extended;
  protected
    procedure Initialize; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  end;

  TImsLinear = class(TCustomMf6Persistent)
  private
    INNER_MAXIMUM: Integer;
    INNER_DVCLOSE: Extended;
    INNER_RCLOSE: Extended;
    rclose_option: string;
    LINEAR_ACCELERATION: string;
    RELAXATION_FACTOR: Extended;
    PRECONDITIONER_LEVELS: Integer;
    PRECONDITIONER_DROP_TOLERANCE: Extended;
    NUMBER_ORTHOGONALIZATIONS: Integer;
    SCALING_METHOD: string;
    REORDERING_METHOD: string;
  protected
    procedure Initialize; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
  PRINT_OPTION := '';
  COMPLEXITY := '';
  CSV_OUTER_OUTPUT := '';
  CSV_INNER_OUTPUT := '';
  NO_PTC := '';
  no_ptc_option := '';
  ATS_OUTER_MAXIMUM_FRACTION := -1;
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

//    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    Assert(FSplitter.Count > 0);
    if FSplitter.Count >= 1 then
    begin
      ALine := UpperCase(ALine);
      FSplitter.DelimitedText := ALine;
      if FSplitter[0] = 'NO_PTC' then
      begin
        NO_PTC := FSplitter[0];
      end
      else if FSplitter.Count >= 2 then
      begin
        ALine := UpperCase(ALine);
        FSplitter.DelimitedText := ALine;
        if FSplitter[0] = 'PRINT_OPTION' then
        begin
          PRINT_OPTION := FSplitter[1];
          if (PRINT_OPTION <> 'NONE') and (PRINT_OPTION <> 'SUMMARY') and (PRINT_OPTION <> 'ALL') then
          begin
            Unhandled.WriteLine(StrUnrecognizedImsOp);
          end;
        end
        else if FSplitter[0] = 'COMPLEXITY' then
        begin
          COMPLEXITY := FSplitter[1];
          if (COMPLEXITY <> 'SIMPLE') and (COMPLEXITY <> 'MODERATE') and (COMPLEXITY <> 'COMPLEX') then
          begin
            Unhandled.WriteLine(StrUnrecognizedImsOp);
          end;
        end
        else if FSplitter[0] = 'NO_PTC' then
        begin
          NO_PTC := FSplitter[0];
          no_ptc_option := FSplitter[1];
          if (no_ptc_option <> 'FIRST') and (no_ptc_option <> 'ALL') then
          begin
            Unhandled.WriteLine(StrUnrecognizedImsOp);
          end;
        end
        else if FSplitter[0] = 'ATS_OUTER_MAXIMUM_FRACTION' then
        begin
          if not TryFortranStrToFloat(FSplitter[1], ATS_OUTER_MAXIMUM_FRACTION) then
          begin
            Unhandled.WriteLine(StrUnrecognizedImsOp);
          end;
        end
        else if FSplitter.Count >= 3 then
        begin
          if (UpperCase(FSplitter[0]) = 'CSV_OUTER_OUTPUT') then
          begin
            if (UpperCase(FSplitter[1]) = 'FILEOUT') then
            begin
              CSV_OUTER_OUTPUT := FSplitter[2];
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
              CSV_INNER_OUTPUT := FSplitter[2];
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
  OUTER_DVCLOSE := -1;
  OUTER_MAXIMUM := -1;
  UNDER_RELAXATION := '';
  UNDER_RELAXATION_GAMMA := -1;
  UNDER_RELAXATION_THETA := -1;
  UNDER_RELAXATION_KAPPA := -1;
  UNDER_RELAXATION_MOMENTUM := -1;
  BACKTRACKING_NUMBER := -1;
  BACKTRACKING_TOLERANCE := -1;
  BACKTRACKING_REDUCTION_FACTOR := -1;
  BACKTRACKING_RESIDUAL_LIMIT := -1;

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

    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    Assert(FSplitter.Count > 0);
    if FSplitter.Count >= 2 then
    begin
      if (FSplitter[0] = 'OUTER_DVCLOSE') or (FSplitter[0] = 'OUTER_HCLOSE') then
      begin
        if not TryFortranStrToFloat(FSplitter[1], OUTER_DVCLOSE) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'OUTER_MAXIMUM' then
      begin
        if not TryStrToInt(FSplitter[1], OUTER_MAXIMUM) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'UNDER_RELAXATION' then
      begin
        UNDER_RELAXATION := FSplitter[1];
        if (UNDER_RELAXATION <> 'NONE') and (UNDER_RELAXATION <> 'SIMPLE')
          and (UNDER_RELAXATION <> 'COOLEY') and (UNDER_RELAXATION <> 'DBD') then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'UNDER_RELAXATION_GAMMA' then
      begin
        if not TryFortranStrToFloat(FSplitter[1], UNDER_RELAXATION_GAMMA) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'UNDER_RELAXATION_THETA' then
      begin
        if not TryFortranStrToFloat(FSplitter[1], UNDER_RELAXATION_THETA) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'UNDER_RELAXATION_KAPPA' then
      begin
        if not TryFortranStrToFloat(FSplitter[1], UNDER_RELAXATION_KAPPA) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'UNDER_RELAXATION_MOMENTUM' then
      begin
        if not TryFortranStrToFloat(FSplitter[1], UNDER_RELAXATION_MOMENTUM) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'BACKTRACKING_NUMBER' then
      begin
        if not TryStrToInt(FSplitter[1], BACKTRACKING_NUMBER) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'BACKTRACKING_TOLERANCE' then
      begin
        if not TryFortranStrToFloat(FSplitter[1], BACKTRACKING_TOLERANCE) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'BACKTRACKING_REDUCTION_FACTOR' then
      begin
        if not TryFortranStrToFloat(FSplitter[1], BACKTRACKING_REDUCTION_FACTOR) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'BACKTRACKING_RESIDUAL_LIMIT' then
      begin
        if not TryFortranStrToFloat(FSplitter[1], BACKTRACKING_RESIDUAL_LIMIT) then
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

{ TImsLinear }

procedure TImsLinear.Initialize;
begin
  inherited;
  INNER_MAXIMUM := -1;
  INNER_DVCLOSE := -1;
  INNER_RCLOSE := -1;
  rclose_option := '';
  LINEAR_ACCELERATION := '';
  RELAXATION_FACTOR := -1;
  PRECONDITIONER_LEVELS := -1;
  PRECONDITIONER_DROP_TOLERANCE := -1;
  NUMBER_ORTHOGONALIZATIONS := -1;
  SCALING_METHOD := '';
  REORDERING_METHOD := '';
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

    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    Assert(FSplitter.Count > 0);
    if FSplitter.Count >= 2 then
    begin
      if FSplitter[0] = 'INNER_MAXIMUM' then
      begin
        if not TryStrToInt(FSplitter[1], INNER_MAXIMUM) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if (FSplitter[0] = 'INNER_DVCLOSE') or (FSplitter[0] = 'INNER_HCLOSE') then
      begin
        if not TryFortranStrToFloat(FSplitter[1], INNER_DVCLOSE) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'INNER_RCLOSE' then
      begin
        if not TryFortranStrToFloat(FSplitter[1], INNER_RCLOSE) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'LINEAR_ACCELERATION' then
      begin
        LINEAR_ACCELERATION := FSplitter[1];
        if (LINEAR_ACCELERATION <> 'CG') and (LINEAR_ACCELERATION <> 'BICGSTAB') then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'RELAXATION_FACTOR' then
      begin
        if not TryFortranStrToFloat(FSplitter[1], RELAXATION_FACTOR) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'PRECONDITIONER_LEVELS' then
      begin
        if not TryStrToInt(FSplitter[1], PRECONDITIONER_LEVELS) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'PRECONDITIONER_DROP_TOLERANCE' then
      begin
        if not TryFortranStrToFloat(FSplitter[1], PRECONDITIONER_DROP_TOLERANCE) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'NUMBER_ORTHOGONALIZATIONS' then
      begin
        if not TryStrToInt(FSplitter[1], NUMBER_ORTHOGONALIZATIONS) then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'SCALING_METHOD' then
      begin
        SCALING_METHOD := FSplitter[1];
        if (SCALING_METHOD <> 'NONE') and (SCALING_METHOD <> 'DIAGONAL') and (SCALING_METHOD <> 'L2NORM') then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'REORDERING_METHOD' then
      begin
        REORDERING_METHOD := FSplitter[1];
        if (REORDERING_METHOD <> 'NONE') and (REORDERING_METHOD <> 'RCM') and (REORDERING_METHOD <> 'MD') then
        begin
          Unhandled.WriteLine(StrUnrecognizedImsOp);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter.Count >= 3 then
      begin
        if FSplitter[0] = 'INNER_RCLOSE' then
        begin
          if not TryFortranStrToFloat(FSplitter[1], INNER_RCLOSE) then
          begin
            Unhandled.WriteLine(StrUnrecognizedImsOp);
            Unhandled.WriteLine(ErrorLine);
          end;
          rclose_option := FSplitter[2];
          if (rclose_option <> 'STRICT') then
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
