unit SfrInterpolatorUnit;

interface

uses FastGeo, TripackTypes, SysUtils;


type
  ESfrError = class(Exception);

  TSfrInterpolator = class(TObject)
  private
    // @name holds the X-coordinates of the data points.
    // @name is a copy of the value passed in @link(Initialize).
    FX: TFloatArray;
    // @name holds the Y-coordinates of the data points.
    // @name is a copy of the value passed in @link(Initialize).
    FY: TFloatArray;
    // @name holds the Z-values at the data points.
    // @name is a copy of the value passed in @link(Initialize).
    FZ: TFloatArray;
    // number of points.
    N: integer;
    // LIST is a work-array with Length 6N-12.
    LIST: TIntArray;
    // // LPTR is a work-array with Length 6N-12.
    LPTR: TIntArray;
    // LEND is a work-array with Length N.
    LEND: TIntArray;
    // @name is used as a starting point in @link(Interpolate1) and
    // @link(Interpolate2).
    IST: longint;
    // GRAD is a work-array (with Length 2N?).
    GRAD: TFloatArray;
    // SIGMA is a work-array (with Length 6N-12?).
    //       SIGMA = Uniform tension factor (IFLGS <= 0), or
    //               array containing tension factors associated
    //               with arcs in one-to-one correspondence with
    //               LIST entries (IFLGS >= 1).  Refer to Sub-
    //               routines GETSIG, SIG0, SIG1, and SIG2.
    SIGMA: TFloatArray;
  public
    // @name stores the data used by @link(Interpolate1) and
    // @link(Interpolate2).  Each point described by X and Y
    // must be unique; duplicates are not allowed.
    // @param(X X holds the X-coordinates of the data points.)
    // @param(Y Y holds the Y-coordinates of the data points.)
    // @param(Z Z holds the data values of the data points.)
    procedure Initialize(X, Y, Z: TFloatArray);
    // Given a value of X, and Y, @name returns an interpolated value
    // at that location based on the data stored in @link(Initialize).
    // Linear interpolation is used.
    // @link(Initialize) must be called before @name.
    function Interpolate1(X, Y: TFloat): TFloat;
    // Given a value of X, and Y, @name returns an interpolated value
    // at that location based on the data stored in @link(Initialize).
    // Curvilinear interpolation is used.
    // @link(Initialize) must be called before @name.
    function Interpolate2(X, Y: TFloat): TFloat;
    procedure Finalize;
  end;

implementation

uses TripackProcedures, SfrProcedures;

resourcestring
  StrAtLeastThreePoint = 'At least three points must be defined.';
  StrErrorInTRMESHThe = 'Error in TRMESH: the first three nodes are collinea' +
  'r.';
  StrErrorInTRMESHInv = 'Error in TRMESH: invalid triangulation.';
  StrErrorInTRMESHDup = 'Error in TRMESH: duplicate nodes encountered.';
  StrErrorInZGRADGIE = 'Error in ZGRADG:  IER = %0:d, NIT = %1:d';
  StrErrorInINTRCOIER = 'Error in INTRCO, IER = %0:d X = %1:g Y = %2:g';
  StrErrorInINTRC1IER = 'Error in INTRC1, IER = %0:d X = %1:g Y = %2:g';

procedure TSfrInterpolator.Finalize;
begin
  SetLength(FX, 0);
  SetLength(FY, 0);
  SetLength(FZ, 0);
  SetLength(LIST, 0);
  SetLength(LPTR, 0);
  SetLength(LEND, 0);
  SetLength(GRAD, 0);
  SetLength(SIGMA, 0);
end;

procedure TSfrInterpolator.Initialize(X, Y, Z: TFloatArray);
var
  NODES: TIntArray;  // NODES is a work-array with Length 2N.
  LNEW: integer; // Work variable.
  IER: integer; // Error indicator.
  TOL, DSM: TFloat;
  Index: Integer;
  MAXITZ: Integer;
  DZMAX: TFloat;
  MAXITG: Integer;
  DGMAX: TFloat;
  IMAX: Integer;
  I: Integer;
  IFLGS: Integer;
  NCC: Integer;
  NITZ: Integer;
  DZMX: TFloat;
  NITG: Integer;
  DGMX: TFloat;
  LCC: TNcmaxIntArray;
  Temp: TFloat;
begin
  IST := 1;
  SetLength(LCC, 0);
  N := Length(X);
  if N < 3 then
  begin
    raise ESfrError.Create(StrAtLeastThreePoint);
  end;

  // If possible, make sure the first three points are not colinear.
  for Index := 2 to N - 1 do
  begin
    if not LEFT(X[1],Y[1],X[0],Y[0],X[Index],Y[Index])
      or not LEFT(X[0],Y[0],X[1],Y[1],X[Index],Y[Index]) then
    begin
      if Index > 2 then
      begin
        Temp := X[Index];
        X[Index] := X[2];
        X[2] := Temp;

        Temp := Y[Index];
        Y[Index] := Y[2];
        Y[2] := Temp;

        Temp := Z[Index];
        Z[Index] := Z[2];
        Z[2] := Temp;
      end;
      break;
    end;
  end;

  // make copies of arrays.
  FX := X;
  SetLength(FX, N);

  Assert(Length(Y) = N);
  FY := Y;
  SetLength(FY, N);

  Assert(Length(Z) = N);
  FZ := Z;
  SetLength(FZ, N);

  // Create work arrays.
  SetLength(LEND, N);
  SetLength(LIST, 6*N-12);
  SetLength(LPTR, 6*N-12);
  SetLength(NODES, 2*N);
  SetLength(SIGMA, 6*N-12);
  SetLength(GRAD, 2*N);

  // Construct the triangulation;
  TRMESH (N,X,Y, LIST,LPTR,LEND,LNEW,NODES, NODES,GRAD,IER);

  IF (IER = -2) THEN
  begin
    raise ESfrError.Create(StrErrorInTRMESHThe);
  end
  ELSE IF (IER = -4) THEN
  begin
    raise ESfrError.Create(StrErrorInTRMESHInv);
  end
  ELSE IF (IER > 0) THEN
  begin
    raise ESfrError.Create(StrErrorInTRMESHDup);
  END
  else if (IER <> 0) then
  begin
    raise ESfrError.Create(
      'Unknown error in TRMESH.');
  end;

  for Index := 0 to Length(SIGMA) - 1 do
  begin
    SIGMA[Index] := 0;
  end;

//C The global gradient estimates depend on the tension
//C   factors, and the optimal tension factors depend on the
//C   gradient estimates.  This requires an iteration around
//C   the calls to ZGRADG/GRADG and GETSIG.  An appropriate
//C   convergence test would involve an upper bound on the
//C   relative change in the gradients between iterations.
//C   However, a few iterations is usually sufficient to
//C   produce good results.
//C
//C Parameters:
//C
//C   MAXITZ = Maximum number of Gauss-Seidel iterations to be
//C            used by ZGRADG.
//C   DZMAX = Tolerance defining convergence in ZGRADG.
//C   MAXITG = Maximum number of Gauss-Seidel iterations to be
//C            used by GRADG.
//C   DGMAX = Tolerance defining convergence in GRADG.
//C   TOL = Tolerance for GETSIG.
//C   IMAX = Number of ZGRADG/GETSIG iterations.
//C   IFLGS = Tension factor option:  0 means uniform tension.
//C
  MAXITZ := 10;
  DZMAX := 1.0E-3;
  MAXITG := 10;
  DGMAX := 1.0E-3;
  TOL := 1.0E-3;
  IMAX := 3;
  NCC := 0;

  for I := 1 to IMAX do
  begin
    IFLGS := 0;
    IF (I > 1) THEN IFLGS := 1;
  //C
  //C Compute global gradient estimates and constraint node
  //C   values (ZGRADG).
  //C
    IF (NCC > 0)  AND  (MAXITZ > 0) THEN
    BEGIN
      NITZ := MAXITZ;
      DZMX := DZMAX;
      ZGRADG (NCC,LCC,N,X,Y,LIST,LPTR,LEND,
                  IFLGS,SIGMA, NITZ,DZMX,Z,
                  GRAD, IER);
      IF (IER < 0) THEN
      BEGIN
        raise ESfrError.Create(Format(StrErrorInZGRADGIE, [IER, NITZ]));
      END;
    end
    ELSE
    begin
      NITZ := 0;
    END;
  //C
  //C Improve the gradient estimates (GRADG).
  //C
    IF (MAXITG > 0) THEN
    begin
      NITG := MAXITG;
      DGMX := DGMAX;
      GRADG (NCC,LCC,N,X,Y,Z,LIST,LPTR,LEND,
                 IFLGS,SIGMA, NITG,DGMX,GRAD, IER);
    end
    ELSE
    begin
      NITG := 0;
    END;
  //C Compute tension factors SIGMA.
  //C
    GETSIG (N,X,Y,Z,LIST,LPTR,LEND,GRAD, TOL, SIGMA, DSM,IER);
  end

end;

function TSfrInterpolator.Interpolate1(X, Y: TFloat): TFloat;
var
  IER: longint;
begin
  INTRC0 (X, Y, 0, nil, N, FX, FY, FZ, LIST,LPTR, LEND, IST,result, IER);
  if IER < 0 then
  begin
    raise ESfrError.Create(Format(StrErrorInINTRCOIER, [IER, X, Y]));
  end;
end;

function TSfrInterpolator.Interpolate2(X, Y: TFloat): TFloat;
var
  IER: longint;
  PZX,PZY: TFloat;
begin
  INTRC1(X,Y, 0, nil, N, FX,FY,FZ, LIST,LPTR, LEND, 1,
    SIGMA, GRAD, False, IST, result ,PZX,PZY, IER);
  if IER < 0 then
  begin
    raise ESfrError.Create(Format(StrErrorInINTRC1IER, [IER, X, Y]));
  end;
end;

end.
