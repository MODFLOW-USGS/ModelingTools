unit SfrProcedures;

interface

{$IFNDEF MyDebug}
  {$HINTS OFF}
  {$WARNINGS OFF}
{$ENDIF}

uses FastGeo, TripackTypes, SysUtils;

type
  ESfrProcedureException = class(Exception);

procedure ARCINT (const B,X1,X2,Y1,Y2,H1,H2,HX1,HX2,HY1,
  HY2,SIGMA : TFloat; const DFLAG: longbool; var HP,HXP,HYP: TFloat; var IER: longint);

procedure CNTOUR (const NX,NY: longint; const X,Y: TNiSingleArray;
  const Z: TNi_NjSingleArray; const CVAL: TFloat; const LC,NCMAX: longint;
  var IWK: TLwkIntArray; var XC, YC: TNmaxSingleArray; var ILC: TLwkIntArray;
  var NC,IER: longint; const IWK_Offset: longint);

procedure COORDS (const XP,YP,X1,X2,X3,Y1,Y2,Y3: TFloat; var B1,B2, B3: TFloat; var IER: longint);

procedure CRPLOT (const LUN: longint; const PLTSIZ: TFloat; const NX,NY: longint;
  const PX,PY: TNiSingleArray; const PZ: TNi_NjSingleArray; const NCON: longint;
  var IWK: TLwkIntArray; var XC,YC: TNmaxSingleArray; var IER: longint);

procedure FVAL (const XP,YP,X1,X2,X3,Y1,Y2,Y3,F1,F2,F3,
  FX1,FX2,FX3,FY1,FY2,FY3,SIG1,SIG2,
  SIG3: TFloat; var FP: TFloat; var IER: longint);

procedure GETSIG (const N: longint; const X,Y,H: TNmaxSingleArray;
  const LIST,LPTR: TN6IntArray; const LEND: TNmaxIntArray; const HXHY: TNtnxSingleArray;
  const TOL: TFloat; var SIGMA: TN6SingleArray; var DSMAX: TFloat; var IER: longint);

procedure GIVENS (var A,B, C,S: TFloat);

procedure GRADG (const NCC: longint; const LCC: TNcmaxIntArray; const N: longint;
  const X,Y,Z: TNmaxSingleArray; const LIST,LPTR: TN6IntArray; const LEND: TNmaxIntArray;
  const IFLGS: longint; const SIGMA: TN6SingleArray; var NIT: longint;
  var DGMAX: TFloat; var GRAD: TNtnxSingleArray; var IER: longint);

procedure GRADL (const K,NCC: longint; const LCC: TNcmaxIntArray; const N: longint;
  const X,Y,Z: TNmaxSingleArray; const LIST,LPTR: TN6IntArray; const LEND: TNmaxIntArray;
  var DX,DY: TFloat; var IER: longint);

procedure GRCOEF (const SIGMA,DCUB: TFloat; var D,SD: TFloat);

procedure INTRC0 (const PX,PY: TFloat; const NCC: longint;
  const LCC: TNcmaxIntArray; const N: longint;
  const X,Y,Z: TNmaxSingleArray; const LIST,LPTR: TN6IntArray; const LEND: TNmaxIntArray;
  var IST: longint; var PZ: TFloat; var IER: longint);

procedure INTRC1 (const PX,PY: TFloat; const NCC: longint;
  const LCC: TNcmaxIntArray; const N: longint; const X,Y,Z: TNmaxSingleArray;
  const LIST,LPTR: TN6IntArray; const LEND: TNmaxIntArray; const IFLGS: longint;
  const SIGMA: TN6SingleArray; const GRAD: TNtnxSingleArray; const DFLAG: longbool;
  var IST :longint; var PZ,PZX,PZY: TFloat; var IER: longint);

procedure ROTATE (const N: longint; const C,S: TFloat; var X,Y: array of TFloat;
  const XOffset, YOffset: longint );

procedure SETRO1 (const XK,YK,ZK,XI,YI,ZI,S1,S2,W: TFloat;
  var ROW: array of TFloat);

procedure SGPRNT (const N,LUNIT: longint; const LIST,LPTR: TN6IntArray;
  const LEND: TNmaxIntArray; const SIGMA: TN6SingleArray);
  
procedure SNHCSH (const X: TFloat; var SINHM,COSHM,COSHMM: TFloat);

procedure TVAL (const X,Y,X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,ZX1,
  ZX2,ZX3,ZY1,ZY2,ZY3: TFloat; const DFLAG: longbool; var F,FX,FY: TFloat;
  var IER: longint);

FUNCTION TRVOL (const X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3: TFloat): TFloat;

procedure UNIF (const NCC: longint; const LCC: TNcmaxIntArray; const N: longint;
  const X,Y,Z: TNmaxSingleArray; const GRAD: TNtnxSingleArray;
  const LIST,LPTR: TN6IntArray; const LEND: TNmaxIntArray; const IFLGS: longint;
  const SIGMA: TN6SingleArray; const NROW,NX,NY: longint; const PX,PY: TNiSingleArray;
  const SFLAG: longbool; var SVAL: TFloat; var ZZ: TNi_NjSingleArray; var IER: longint);

FUNCTION VOLUME (const NCC: longint; const LCC: TNcmaxIntArray; const N: longint;
  const X,Y,Z: TNmaxSingleArray; const LIST,LPTR: TN6IntArray;
  const LEND: TNmaxIntArray): TFloat;

procedure ZGRADG (const NCC: longint; const LCC: TNcmaxIntArray; const N: longint;
  const X,Y: TNmaxSingleArray; const LIST,LPTR: TN6IntArray; const LEND: TNmaxIntArray;
  const IFLGS: longint; const SIGMA: TN6SingleArray; var NIT: longint;
  var DZMAX : TFloat; var Z: TNmaxSingleArray;
  var GRAD: TNtnxSingleArray; var IER: longint);

procedure ZINIT (const NCC: longint; const LCC: TNcmaxIntArray; const N: longint;
  const X,Y: TNmaxSingleArray; const LIST,LPTR: TN6IntArray; const LEND: TNmaxIntArray;
  var Z: TNmaxSingleArray; var IER: longint);
  
implementation

uses
{$IFDEF UseSfrMessages}
  SfrMessages,
{$ENDIF}  
  Math, TripackProcedures;

function IWK_Index(const firstIndex, secondIndex: integer): integer; inline;
begin
  result := firstIndex*2 + secondIndex;
end;

function FortranSign(A, B: TFloat): TFloat; //overload;
begin
  if B > 0 then
  begin
    result := Abs(A);
  end
  else
  begin
    result := -Abs(A);
  end;
end;

procedure ARCINT (const B,X1,X2,Y1,Y2,H1,H2,HX1,HX2,HY1,
  HY2,SIGMA : TFloat; const DFLAG: longbool; var HP,HXP,HYP: TFloat; var IER: longint);
//      SUBROUTINE ARCINT (B,X1,X2,Y1,Y2,H1,H2,HX1,HX2,HY1,
//     .                   HY2,SIGMA,DFLAG, HP,HXP,HYP,IER)
//      INTEGER IER
//      LOGICAL DFLAG
//      REAL    B, X1, X2, Y1, Y2, H1, H2, HX1, HX2, HY1,
//     .        HY2, SIGMA, HP, HXP, HYP
//C
//C***********************************************************
//C
//C                                               From SRFPACK
//C                                            Robert J. Renka
//C                                  Dept. of Computer Science
//C                                       Univ. of North Texas
//C                                           renka@cs.unt.edu
//C                                                   11/18/96
//C
//C   Gigen a line segment P1-P2 containing a point P =
//C (XP,YP), along with function values and partial deriva-
//C tives at the endpoints, this subroutine computes an
//C interpolated value and, optionally, a gradient at P.  The
//C value and tangential gradient component at P are taken to
//C be the value and derivative of the Hermite interpolatory
//C tension spline H defined by the endpoint values and tan-
//C gential gradient components.  The normal gradient compo-
//C nent at P is obtained by linear interpolation applied to
//C the normal components at the endpoints.
//C
//C On input:
//C
//C       B = Local coordinate of P with respect to P1-P2:
//C           P = B*P1 + (1-B)*P2.  Note that B may be comput-
//C           ed from the coordinates of P as <P2-P1,P2-P>/
//C           <P2-P1,P2-P1>.
//C
//C       X1,X2,Y1,Y2 = Coordinates of a pair of distinct
//C                     points P1 and P2.
//C
//C       H1,H2 = Values of the interpolant H at P1 and P2,
//C               respectively.
//C
//C       HX1,HX2,HY1,HY2 = x and y partial derivatives of H
//C                         at P1 and P2.
//C
//C       SIGMA = Tension factor associated with P1-P2.
//C
//C       DFLAG = Logical flag which specifies whether first
//C               partial derivatives at P are to be computed:
//C               DFLAG = .TRUE. if and only if partials are
//C               to be returned.
//C
//C Input parameters are not altered by this routine.
//C
//C On output:
//C
//C       HP = Interpolated value at P unless IER < 0, in
//C            which case HP is not defined.
//C
//C       HXP,HYP = x and y partial derivatives at P unless
//C                 DFLAG = FALSE or IER < 0.
//C
//C       IER = Error indicator:
//C             IER = 0 if no errors were encountered.
//C             IER = 1 if B < 0 or B > 1 and thus HP is an
//C                     extrapolated value.
//C             IER = -1 if P1 and P2 coincide.
//C
//C SRFPACK module required by ARCINT:  SNHCSH
//C
//C Intrinsic functions called by ARCINT:  ABS, EXP
//C
//C***********************************************************
//C
var
       B1, B2, CM, CM2, CMM, D1, D2, DS, DUMMY, DX, DY,
          E, E1, E2, EMS, GN, GT, S, S1, S2, SB1, SB2,
          SBIG, SIG, SINH2, SM, SM2, TM, TM1, TM2, TP1,
          TP2, TS: TFloat;
//      REAL B1, B2, CM, CM2, CMM, D1, D2, DS, DUMMY, DX, DY,
//     .     E, E1, E2, EMS, GN, GT, S, S1, S2, SB1, SB2,
//     .     SBIG, SIG, SINH2, SM, SM2, TM, TM1, TM2, TP1,
//     .     TP2, TS
begin
  SBIG := 85;
//      DATA SBIG/85./
//C
      DX := X2 - X1;
      DY := Y2 - Y1;
      DS := DX*DX + DY*DY;
      IF (DS = 0.0) then
      begin
  //C P1 and P2 coincide.
  //C
        IER := -1;
        Exit;
  //    1 IER = -1
  //      RETURN
      end;
      IER := 0;
//      DX = X2 - X1
//      DY = Y2 - Y1
//      DS = DX*DX + DY*DY
//      IF (DS .EQ. 0.) GO TO 1
//      IER = 0
//C
//C Compute local coordinates B1 and B2, tangential deriva-
//C   tives S1 and S2, slope S, and second differences D1 and
//C   D2.  S1, S2, S, D1, and D2 are scaled by the separation
//C   D between P1 and P2.
//C
      B1 := B;
      B2 := 1. - B1;
      IF (B1 < 0.0)  OR  (B2 < 0.0) then IER := 1;
      S1 := HX1*DX + HY1*DY;
      S2 := HX2*DX + HY2*DY;
      S := H2 - H1;
      D1 := S - S1;
      D2 := S2 - S;
//      B1 = B
//      B2 = 1. - B1
//      IF (B1 .LT. 0.  .OR.  B2 .LT. 0.) IER = 1
//      S1 = HX1*DX + HY1*DY
//      S2 = HX2*DX + HY2*DY
//      S = H2 - H1
//      D1 = S - S1
//      D2 = S2 - S
//C
//C Compute HP and, if required, the scaled tangential grad-
//C   ient component GT.
//C
      SIG := ABS(SIGMA);
//      SIG = ABS(SIGMA)
      IF (SIG < 1.0E-9) THEN
      begin
//      IF (SIG .LT. 1.E-9) THEN
//C
//C SIG = 0:  use Hermite cubic interpolation.
//C
        HP := H1 + B2*(S1 + B2*(D1 + B1*(D1 - D2)));
        IF (NOT DFLAG) then Exit;
        GT := S1 + B2*(D1 + D2 + 3.*B1*(D1 - D2));
//        HP = H1 + B2*(S1 + B2*(D1 + B1*(D1 - D2)))
//        IF (.NOT. DFLAG) RETURN
//        GT = S1 + B2*(D1 + D2 + 3.*B1*(D1 - D2))
      end
      else if (SIG <= 0.5) THEN
      begin
//      ELSEIF (SIG .LE. .5) THEN
//C
//C 0 .LT. SIG .LE. .5:  use approximations designed to avoid
//C   cancellation error in the hyperbolic functions.
//C
        SB2 := SIG*B2;
        SNHCSH (SIG, SM,CM,CMM);
        SNHCSH (SB2, SM2,CM2,DUMMY);
        E := SIG*SM - CMM - CMM;
        HP := H1 + B2*S1 + ((CM*SM2-SM*CM2)*(D1+D2) + SIG*
                          (CM*CM2-(SM+SIG)*SM2)*D1)/(SIG*E);
        IF (NOT DFLAG) then Exit;
        SINH2 := SM2 + SB2;
        GT := S1 + ((CM*CM2-SM*SINH2)*(D1+D2) + SIG*
                  (CM*SINH2-(SM+SIG)*CM2)*D1)/E;
//        SB2 = SIG*B2
//        CALL SNHCSH (SIG, SM,CM,CMM)
//        CALL SNHCSH (SB2, SM2,CM2,DUMMY)
//        E = SIG*SM - CMM - CMM
//        HP = H1 + B2*S1 + ((CM*SM2-SM*CM2)*(D1+D2) + SIG*
//     .                     (CM*CM2-(SM+SIG)*SM2)*D1)/(SIG*E)
//        IF (.NOT. DFLAG) RETURN
//        SINH2 = SM2 + SB2
//        GT = S1 + ((CM*CM2-SM*SINH2)*(D1+D2) + SIG*
//     .             (CM*SINH2-(SM+SIG)*CM2)*D1)/E
      end
      else
      begin
//      ELSE
//C
//C SIG > .5:  use negative exponentials in order to avoid
//C   overflow.  Note that EMS = EXP(-SIG).  In the case of
//C   extrapolation (negative B1 or B2), H is approximated
//C   by a linear function if -SIG*B1 or -SIG*B2 is large.
//C
        SB1 := SIG*B1;
        SB2 := SIG - SB1;
//        SB1 = SIG*B1
//        SB2 = SIG - SB1
        IF (-SB1 > SBIG)  OR  (-SB2 > SBIG) THEN
        begin
          HP := H1 + B2*S;
          IF (NOT DFLAG) then Exit;
          GT := S;
        end
        ELSE
        begin
          E1 := EXP(-SB1);
          E2 := EXP(-SB2);
          EMS := E1*E2;
          TM := 1.0 - EMS;
          TS := TM*TM;
          TM1 := 1.0 - E1;
          TM2 := 1.0 - E2;
          E := TM*(SIG*(1.0+EMS) - TM - TM);
          HP := H1 + B2*S + (TM*TM1*TM2*(D1+D2) + SIG*
                           ((E2*TM1*TM1-B1*TS)*D1 +
                            (E1*TM2*TM2-B2*TS)*D2))/(SIG*E);
          IF (NOT DFLAG) then Exit;
          TP1 := 1.0 + E1;
          TP2 := 1.0 + E2;
          GT := S + (TM1*(TM*TP2-SIG*E2*TP1)*D1 -
                   TM2*(TM*TP1-SIG*E1*TP2)*D2)/E;
        END;
//        IF (-SB1 .GT. SBIG  .OR.  -SB2 .GT. SBIG) THEN
//          HP = H1 + B2*S
//          IF (.NOT. DFLAG) RETURN
//          GT = S
//        ELSE
//          E1 = EXP(-SB1)
//          E2 = EXP(-SB2)
//          EMS = E1*E2
//          TM = 1. - EMS
//          TS = TM*TM
//          TM1 = 1. - E1
//          TM2 = 1. - E2
//          E = TM*(SIG*(1.+EMS) - TM - TM)
//          HP = H1 + B2*S + (TM*TM1*TM2*(D1+D2) + SIG*
//     .                      ((E2*TM1*TM1-B1*TS)*D1 +
//     .                       (E1*TM2*TM2-B2*TS)*D2))/(SIG*E)
//          IF (.NOT. DFLAG) RETURN
//          TP1 = 1. + E1
//          TP2 = 1. + E2
//          GT = S + (TM1*(TM*TP2-SIG*E2*TP1)*D1 -
//     .              TM2*(TM*TP1-SIG*E1*TP2)*D2)/E
//        ENDIF
      END;
//      ENDIF
//C
//C Compute the gradient at P, (HXP,HYP) = (GT/D)T + (GN/D)N,
//C   where T = (DX,DY)/D (unit tangent vector), N = (-DY,DX)/
//C   D (unit normal), and the scaled normal component is GN =
//C   B1<(HX1,HY1),N> + B2<(HX2,HY2),N>.
//C
      GN := B1*(HY1*DX-HX1*DY) + B2*(HY2*DX-HX2*DY);
      HXP := (GT*DX - GN*DY)/DS;
      HYP := (GT*DY + GN*DX)/DS;
//      GN = B1*(HY1*DX-HX1*DY) + B2*(HY2*DX-HX2*DY)
//      HXP = (GT*DX - GN*DY)/DS
//      HYP = (GT*DY + GN*DX)/DS
//      RETURN
//C
//C P1 and P2 coincide.
//C
//    1 IER = -1
//      RETURN
//      END
end;

procedure CNTOUR (const NX,NY: longint; const X,Y: TNiSingleArray;
  const Z: TNi_NjSingleArray; const CVAL: TFloat; const LC,NCMAX: longint;
  var IWK: TLwkIntArray; var XC, YC: TNmaxSingleArray; var ILC: TLwkIntArray;
  var NC,IER: longint; const IWK_Offset: longint);
//      SUBROUTINE CNTOUR (NX,NY,X,Y,Z,CVAL,LC,NCMAX,IWK, XC,
//     .                   YC,ILC,NC,IER,ifortran)
//      dll_import Cntour100,  Cntour110
//      INTEGER NX, NY, LC, NCMAX, IWK(NX,*), ILC(NCMAX), NC,
//     .        IER
//      REAL    X(NX), Y(NY), Z(NX,NY), CVAL, XC(LC), YC(LC)
//C
//C***********************************************************
//C
//C                                               From SRFPACK
//C                                            Robert J. Renka
//C                                  Dept. of Computer Science
//C                                       Univ. of North Texas
//C                                           renka@cs.unt.edu
//C                                                   04/28/90
//C
//C   Given a set of function values Z = F(X,Y) at the verti-
//C ces of an NX by NY rectangular grid, this subroutine de-
//C termines a set of contour lines associated with F = CVAL.
//C A contour line is specified by an ordered sequence of
//C points (XC,YC), each lying on a grid edge and computed
//C from the linear interpolant of the function values at the
//C endpoints of the edge.  The accuracy of the contour lines
//C is thus directly related to the number of grid points.  If
//C a contour line forms a closed curve, the first point coin-
//C cides with the last point.  Otherwise, the first and last
//C points lie on the grid boundary.
//C
//C   Note that the problem is ill-conditioned in the vicinity
//C of a double zero of F-CVAL.  Thus, if a grid cell is
//C crossed by two contour lines (all four sides intersected),
//C three different configurations are possible, corresponding
//C to a local minimum, a local maximum, or a saddle point.
//C It is arbitrarily assumed in this case that the contour
//C lines intersect, representing a saddle point.  Also, in
//C order to treat the case of F = CVAL at a vertex in a con-
//C sistent manner, this case is always treated as F > CVAL.
//C Hence, if F takes on the same value at both ends of an
//C edge, it is assumed that no contour line intersects that
//C edge.  In particular, a constant function, including
//C F = CVAL, results in no contour lines.
//C
//C On input:
//C
//C       NX = Number of grid points in the x direction.
//C            NX .GE. 2.
//C
//C       NY = Number of grid points in the y direction.
//C            NY .GE. 2.
//C
//C       X = Array of length NX containing a strictly in-
//C           creasing sequence of values.
//C
//C       Y = Array of length NY containing a strictly in-
//C           creasing sequence of values.
//C
//C       Z = Array of function values at the vertices of the
//C           rectangular grid.  Z(I,J) = F(X(I),Y(J)) for
//C           I = 1,...,NX and J = 1,...,NY.
//C
//C       CVAL = Constant function value defining a contour
//C              line as the set of points (X,Y) such that
//C              F(X,Y) = CVAL.
//C
//C       LC = Length of arrays XC and YC, and maximum allow-
//C            able number of points defining contour lines.
//C            LC = 2(NX-1)(NY-1) + (NX*NY+1)/2 is (probably
//C            more than) sufficient.  LC .GE. 2.
//C
//C       NCMAX = Length of array ILC, and maximum allowable
//C               number of contour lines.  NCMAX = (NX*NY+1)/
//C               2 is sufficient.  NCMAX .GE. 1.
//C
//C The above parameters are not altered by this routine.
//C
//C       IWK = Integer array of length .GE. NX*(NY-1) to be
//C             used as work space.
//C
//C       XC,YC = Arrays of length LC.
//C
//C       ILC = Integer array of length NCMAX.
//C
//C On output:
//C
//C       XC,YC = Arrays containing the coordinates of NC con-
//C               tour lines.  For K = 1,...,NC, contour line
//C               K is defined by the sequence of points with
//C               indexes ILC(K-1)+1,...,ILC(K) where ILC(0) =
//C               0.
//C
//C       ILC = Array containing the indexes (to XC and YC)
//C             associated with the terminal point of contour
//C             line K in position K for K = 1,...,NC (if NC
//C             .GT. 0).
//C
//C       NC = Number of contour lines whose points are stored
//C            in XC and YC.
//C
//C       IER = Error indicator:
//C             IER = 0 if no errors were encountered and all
//C                     contour lines were found.
//C             IER = 1 if NX, NY, LC, or NCMAX is outside its
//C                     valid range.  NC = 0 and XC, YC, and
//C                     ILC are not altered in this case.
//C             IER = 2 if X or Y is not strictly increasing.
//C                     NC = 0 and XC, YC, and ILC are not
//C                     altered in this case.
//C             IER = K for K > LC, where K is the required
//C                     length of XC and YC, if more storage
//C                     space is required to complete the
//C                     specification of contour line NC and/
//C                     or additional contour lines up to a
//C                     total of NCMAX.  NC .GE. 1 and ILC(NC)
//C                     = LC in this case.
//C            IER = -1 if more than NCMAX contour lines are
//C                     present (more space is required in
//C                     ILC).  NC = NCMAX, and LC may or may
//C                     not be sufficient for the additional
//C                     contour lines in this case.  (This is
//C                     not determined.)
//C
//C   In the unlikely event of an internal failure, a message
//C is printed on logical unit LUN (specified in the DATA
//C statement below).  IER may be 0 in this case.
//C
//C Modules required by CNTOUR:  None
//C
//C***********************************************************
//C
var
       I, I1, I2, IB, IN_Var, IND, ISID, ISIDB, ISIDN,
             J, J1, J2, JB, JN, K, LCON, LMX, LUN, NCMX,
             NCON, NI, NIM1, NJ, NJM1: longint;
       BDRY: longbool;
          CV, W, XF, XN, XP, YF, YN, YP, Z1, Z2: TFloat;
       IFORTRAN: longint;
       GoTo8: boolean;
  GoTo9, GoTo12, GoTo13: Boolean;
//      INTEGER I, I1, I2, IB, IN, IND, ISID, ISIDB, ISIDN,
//     .        J, J1, J2, JB, JN, K, LCON, LMX, LUN, NCMX,
//     .        NCON, NI, NIM1, NJ, NJM1
//      LOGICAL BDRY
//      REAL    CV, W, XF, XN, XP, YF, YN, YP, Z1, Z2
//      DATA    LUN/0/
//      integer IFORTRAN
//C
//C Store parameters in local variables.
  function IWK_Index(const firstIndex, secondIndex: integer): integer; 
  begin
    result := firstIndex*(NY-1) + secondIndex;
  end;
begin
  LUN := 0;
//C
      NI := NX;
      NJ := NY;
      NIM1 := NI - 1;
      NJM1 := NJ - 1;
      CV := CVAL;
      LMX := LC;
      NCMX := NCMAX;
      NC := 0;
//      NI = NX
//      NJ = NY
//      NIM1 = NI - 1
//      NJM1 = NJ - 1
//      CV = CVAL
//      LMX = LC
//      NCMX = NCMAX
//      NC = 0
//C
//C Test for invalid input parameters.
//C
      IER := 1;
      IF (NI < 2)  OR  (NJ < 2)  OR  (LMX < 2)  OR
         (NCMX < 1) then Exit;
//      IER = 1
//      IF (NI .LT. 2  .OR.  NJ .LT. 2  .OR.  LMX .LT. 2  .OR.
//     .    NCMX .LT. 1) RETURN
//C
//C Test for nonincreasing values of X or Y.
//C
      IER := 2;
      for I := 2 to NI do
      begin
        IF (X[I-1] <= X[I-2]) then Exit;
      end;       
      for J := 2 to NJ do
      begin
        IF (Y[J-1] <= Y[J-2]) then Exit;
      end;
//      IER = 2
//      DO 1 I = 2,NI
//        IF (X(I) .LE. X(I-1)) RETURN
//    1   CONTINUE
//      DO 2 J = 2,NJ
//        IF (Y(J) .LE. Y(J-1)) RETURN
//    2   CONTINUE
//C
//C Loop on grid cells, initializing edge indicators (stored
//C   in IWK) to zeros.  For each cell, the indicator IND is a
//C   4-bit integer with each bit corresponding to an edge of
//C   the cell, and having value 1 iff the edge has been pro-
//C   cessed.  Note that two IND values must be adjusted when
//C   an interior edge is processed.  The cell sides (edges)
//C   are numbered (1,2,4,8) in counterclockwise order start-
//C   ing from the bottom.  This corresponds to an ordering of
//C   the weighted IND bits from low order to high order.
//C   Grid cells are identified with their lower left corners.
//C
      for J := 1 to NJM1 do
      begin
        for I := 1 to NIM1 do
        begin
          IWK[IWK_Index(J-1,I-1)+IWK_Offset] := 0;
        end;
      end;
//      DO 4 J = 1,NJM1
//        DO 3 I = 1,NIM1
//          IWK(I,J) = 0
//    3     CONTINUE
//    4   CONTINUE
//C
//C First determine open contours by looping on boundary edges
//C   in counterclockwise order starting from the lower left.
//C   For each unprocessed boundary edge intersected by a con-
//C   tour line, the contour line is determined and IWK is up-
//C   dated to reflect the edges intersected.  The boundary
//C   cell (lower left corner) is indexed by (IB,JB) and the
//C   boundary edge is specified by ISIDB.  NCON and LCON are
//C   local variables containing the number of contour lines
//C   encountered and the current length of XC and YC.
//C
      NCON := 0;
      LCON := 0;
      ISIDB := 1;
      IB := 1;
      JB := 1;
//      NCON = 0
//      LCON = 0
//      ISIDB = 1
//      IB = 1
//      JB = 1
//C
//C Top of loop on boundary edges.  The edge has been
//C   processed iff IND/ISIDB is odd.
//C
      repeat
        IND := IWK[IWK_Index(JB-1,IB-1)+IWK_Offset];
//        IF (IND/ISIDB = 2*((IND/ISIDB)/2)) then
        IF not Odd(IND div ISIDB) then
        begin
    //    5 IND = IWK(IB,JB)
    //      IF (IND/ISIDB .NE. 2*((IND/ISIDB)/2)) GO TO 9
    //C
    //C Update the edge indicator and store the vertex indexes of
    //C   the endpoints of the edge.
    //C
          IWK[IWK_Index(JB-1,IB-1)+IWK_Offset] := IND + ISIDB;
          IF (ISIDB = 1) THEN
          begin
            I1 := IB;
            J1 := JB;
            I2 := IB + 1;
            J2 := JB;
          end
          ELSE IF (ISIDB = 2) THEN
          begin
            I1 := IB + 1;
            J1 := JB;
            I2 := IB + 1;
            J2 := JB + 1;
          end
          ELSE IF (ISIDB = 4) THEN
          begin
            I1 := IB + 1;
            J1 := JB + 1;
            I2 := IB;
            J2 := JB + 1;
          end
          ELSE
          begin
            I1 := IB;
            J1 := JB + 1;
            I2 := IB;
            J2 := JB;
          END;
    //      IWK(IB,JB) = IND + ISIDB
    //      IF (ISIDB .EQ. 1) THEN
    //        I1 = IB
    //        J1 = JB
    //        I2 = IB + 1
    //        J2 = JB
    //      ELSEIF (ISIDB .EQ. 2) THEN
    //        I1 = IB + 1
    //        J1 = JB
    //        I2 = IB + 1
    //        J2 = JB + 1
    //      ELSEIF (ISIDB .EQ. 4) THEN
    //        I1 = IB + 1
    //        J1 = JB + 1
    //        I2 = IB
    //        J2 = JB + 1
    //      ELSE
    //        I1 = IB
    //        J1 = JB + 1
    //        I2 = IB
    //        J2 = JB
    //      ENDIF
    //C
    //C Proceed to the next edge if there is no intersection.
    //C
          Z1 := Z[J1-1,I1-1];
          Z2 := Z[J2-1,I2-1];
    //      Z1 = Z(I1,J1)
    //      Z2 = Z(I2,J2)
          IF not (((Z1 < CV)  AND  (Z2 < CV))  OR
             ((Z1 >= CV)  AND  (Z2 >= CV))) then
          begin
    //      IF ((Z1 .LT. CV  .AND.  Z2 .LT. CV)  .OR.
    //     .    (Z1 .GE. CV  .AND.  Z2 .GE. CV)) GO TO 9
    //C
    //C Store the zero of the linear interpolant of Z1-CV and
    //C   Z2-CV as the first point of an open contour unless
    //C   NCMAX contour lines have been found or there is in-
    //C   sufficient space reserved for XC and YC.
    //C
          IF (NCON = NCMX) THEN
          begin
            IER := -1;
    //C Test for insufficient storage reserved for XC and YC.
    //C
            IF (LCON > LMX) then IER := LCON;
            NC := NCON;
            Exit;
    //   16 IF (LCON .GT. LMX) IER = LCON
    //      NC = NCON
    //      RETURN
          END;
          NCON := NCON + 1;
          LCON := LCON + 1;
          W := (CV-Z1)/(Z2-Z1);
          XP := X[I1-1] + W*(X[I2-1]-X[I1-1]);
          YP := Y[J1-1] + W*(Y[J2-1]-Y[J1-1]);
          IF (LCON <= LMX) THEN
          begin
            XC[LCON-1] := XP;
            YC[LCON-1] := YP;
          END;
    //      IF (NCON .EQ. NCMX) THEN
    //        IER = -1
    //        GO TO 16
    //      ENDIF
    //      NCON = NCON + 1
    //      LCON = LCON + 1
    //      W = (CV-Z1)/(Z2-Z1)
    //      XP = X(I1) + W*(X(I2)-X(I1))
    //      YP = Y(J1) + W*(Y(J2)-Y(J1))
    //      IF (LCON .LE. LMX) THEN
    //        XC(LCON) = XP
    //        YC(LCON) = YP
    //      ENDIF
    //C
    //C Initialize for loop on cells intersected by the open
    //C   contour line.
    //C
          I := IB;
          J := JB;
          ISID := ISIDB;
    //      I = IB
    //      J = JB
    //      ISID = ISIDB
    //C
    //C Traverse the contour line.  Cell (I,J) was entered on side
    //C   ISID = (I1,J1)->(I2,J2).  Find an exit edge E (unproces-
    //C   sed edge intersected by the contour) by looping on the
    //C   remaining three sides, starting with the side opposite
    //C   ISID.
    //C
          repeat
            IND := IWK[IWK_Index(J-1,I-1)+IWK_Offset];
      //    6 IND = IWK(I,J)
            GoTo8 := False;
            for K := 1 to 3 do
            begin
              ISID := 2*ISID;
              IF (K <> 2) then ISID := 2*ISID;
              IF (ISID > 15) then ISID := ISID div 16;
              IF (ISID = 1) THEN
              begin
                I1 := I;
                J1 := J;
                I2 := I + 1;
                J2 := J;
              end
              ELSE IF (ISID = 2) THEN
              begin
                I1 := I + 1;
                J1 := J;
                I2 := I + 1;
                J2 := J + 1;
              end
              ELSE IF (ISID = 4) THEN
              begin
                I1 := I + 1;
                J1 := J + 1;
                I2 := I;
                J2 := J + 1;
              end
              ELSE
              begin
                I1 := I;
                J1 := J + 1;
                I2 := I;
                J2 := J;
              END;
              
      //      DO 7 K = 1,3
      //        ISID = 2*ISID
      //        IF (K .NE. 2) ISID = 2*ISID
      //        IF (ISID .GT. 15) ISID = ISID/16
      //        IF (ISID .EQ. 1) THEN
      //          I1 = I
      //          J1 = J
      //          I2 = I + 1
      //          J2 = J
      //        ELSEIF (ISID .EQ. 2) THEN
      //          I1 = I + 1
      //          J1 = J
      //          I2 = I + 1
      //          J2 = J + 1
      //        ELSEIF (ISID .EQ. 4) THEN
      //          I1 = I + 1
      //          J1 = J + 1
      //          I2 = I
      //          J2 = J + 1
      //        ELSE
      //          I1 = I
      //          J1 = J + 1
      //          I2 = I
      //          J2 = J
      //        ENDIF
      //C
      //C Test for a 1 in bit position ISID of cell (I,J) and bypass
      //C   the edge if it has been previously encountered.
      //C
              IF Odd(IND div ISID) then Continue;
//              IF (IND/ISID <> 2*((IND/ISID)/2)) then Continue;
      //        IF (IND/ISID .NE. 2*((IND/ISID)/2)) GO TO 7
      //C
      //C Update IWK for edge E = (I1,J1)->(I2,J2).  (IN,JN) indexes
      //C   the cell which shares E with cell (I,J), and ISIDN is
      //C   the side number of E in (IN,JN).  BDRY is true iff E is
      //C   a boundary edge (with no neighboring cell).
      //C
              IWK[IWK_Index(J-1,I-1)+IWK_Offset] := IWK[IWK_Index(J-1,I-1)+IWK_Offset] + ISID;
              IF (ISID <= 2) THEN
              begin
                IN_Var := I1;
                JN := J2 - 1;
                ISIDN := 4*ISID;
              end
              ELSE
              begin
                IN_Var := I1 - 1;
                JN := J2;
                ISIDN := ISID div 4;
              END;
              BDRY := (IN_Var = 0)  OR  (IN_Var = NI)  OR
                    (JN = 0)  OR  (JN = NJ);
              IF (NOT BDRY) then IWK[Iwk_Index(JN-1,IN_Var-1)+IWK_Offset] :=
                IWK[Iwk_Index(JN-1,IN_Var-1)+IWK_Offset] + ISIDN;
      //        IWK(I,J) = IWK(I,J) + ISID
      //        IF (ISID .LE. 2) THEN
      //          IN = I1
      //          JN = J2 - 1
      //          ISIDN = 4*ISID
      //        ELSE
      //          IN = I1 - 1
      //          JN = J2
      //          ISIDN = ISID/4
      //        ENDIF
      //        BDRY = IN .EQ. 0  .OR.  IN .EQ. NI  .OR.
      //     .         JN .EQ. 0  .OR.  JN .EQ. NJ
      //        IF (.NOT. BDRY) IWK(IN,JN) = IWK(IN,JN) + ISIDN
      //C
      //C Exit the loop on sides if E is intersected by the contour.
      //C
              Z1 := Z[J1-1,I1-1];
              Z2 := Z[J2-1,I2-1];
              IF (((Z1 < CV)  AND  (Z2 >= CV))  OR
                 ((Z1 >= CV)  AND  (Z2 < CV))) then
              begin
                GoTo8 := True;
                break;
              end;
      //        Z1 = Z(I1,J1)
      //        Z2 = Z(I2,J2)
      //        IF ((Z1 .LT. CV  .AND.  Z2 .GE. CV)  .OR.
      //     .      (Z1 .GE. CV  .AND.  Z2 .LT. CV)) GO TO 8
      //    7   CONTINUE
            end;
            GoTo9 := False;
            if not GoTo8 then
            begin
      //C*
      //C Error -- No exit point found.  Print a message and exit
      //C          the contour traversal loop.
      //C
      //!      WRITE (LUN,100) NCON
{$IFDEF UseSfrMessages}
              Cntour100(IFORTRAN, NCON);
{$ENDIF}
      //      call Cntour100(IFORTRAN, NCON)
      //!  100 FORMAT (///5X,'Error in CNTOUR:  Contour line L ',
      //!     .        'begins on the boundary'/5X,'and terminates ',
      //!     .        'in the interior for L =',I4/)
              ILC[NCON-1] := LCON;
              GoTo9 := True;
              break;
      //      ILC(NCON) = LCON
      //      GO TO 9
              
            end;
      //C*
      //C Add the intersection point (XN,YN) to the list unless it
      //C   coincides with the previous point (XP,YP) or there is
      //C   not enough space in XC and YC.
      //C
            W := (CV-Z1)/(Z2-Z1);
            XN := X[I1-1] + W*(X[I2-1]-X[I1-1]);
            YN := Y[J1-1] + W*(Y[J2-1]-Y[J1-1]);
            IF (XN <> XP) OR (YN <> YP) THEN
            begin
              LCON := LCON + 1;
              XP := XN;
              YP := YN;
              IF (LCON <= LMX) THEN
              begin
                XC[LCON-1] := XN;
                YC[LCON-1] := YN;
              END;
            END;
      //    8 W = (CV-Z1)/(Z2-Z1)
      //      XN = X(I1) + W*(X(I2)-X(I1))
      //      YN = Y(J1) + W*(Y(J2)-Y(J1))
      //      IF (XN .NE. XP  .OR.  YN .NE. YP) THEN
      //        LCON = LCON + 1
      //        XP = XN
      //        YP = YN
      //        IF (LCON .LE. LMX) THEN
      //          XC(LCON) = XN
      //          YC(LCON) = YN
      //        ENDIF
      //      ENDIF
      //C
      //C Bottom of contour traversal loop.  If E is not a boundary
      //C   edge, reverse the edge direction (endpoint indexes) and
      //C   update the cell index and side number.
      //C
            IF (NOT BDRY) THEN
            begin
              I := I1;
              J := J1;
              I1 := I2;
              J1 := J2;
              I2 := I;
              J2 := J;
              I := IN_Var;
              J := JN;
              ISID := ISIDN;
              Continue;
            END;
      //      IF (.NOT. BDRY) THEN
      //        I = I1
      //        J = J1
      //        I1 = I2
      //        J1 = J2
      //        I2 = I
      //        J2 = J
      //        I = IN
      //        J = JN
      //        ISID = ISIDN
      //        GO TO 6
      //      ENDIF
            break;
          until False;
    //C
    //C Update ILC with a pointer to the end of the contour line.
    //C
          ILC[NCON-1] := LCON;
    //      ILC(NCON) = LCON
    //C
          end;
        end;
  //C Bottom of loop on boundary edges.  Update the boundary
  //C   cell index and side number, and test for termination.
  //C
        IF (ISIDB = 1) THEN
        begin
          IF (IB < NIM1) THEN
          begin
            IB := IB + 1;
          end
          ELSE
          begin
            ISIDB := 2;
          end;
        end
        ELSE IF (ISIDB = 2) THEN
        begin
          IF (JB < NJM1) THEN
          begin
            JB := JB + 1        ;
          end
          ELSE
          begin
            ISIDB := 4;
          end;
        end
        ELSE IF (ISIDB = 4) THEN
        begin
          IF (IB > 1) THEN
          begin
            IB := IB - 1;
          end
          ELSE
          begin
            ISIDB := 8;
          end;
        end
        ELSE
        begin
          IF (JB > 1) THEN
          begin
            JB := JB - 1
          end
          ELSE
          begin
            ISIDB := 16
          end;
        end;
  //    9 IF (ISIDB .EQ. 1) THEN
  //        IF (IB .LT. NIM1) THEN
  //          IB = IB + 1
  //        ELSE
  //          ISIDB = 2
  //        ENDIF
  //      ELSEIF (ISIDB .EQ. 2) THEN
  //        IF (JB .LT. NJM1) THEN
  //          JB = JB + 1
  //        ELSE
  //          ISIDB = 4
  //        ENDIF
  //      ELSEIF (ISIDB .EQ. 4) THEN
  //        IF (IB .GT. 1) THEN
  //          IB = IB - 1
  //        ELSE
  //          ISIDB = 8
  //        ENDIF
  //      ELSE
  //        IF (JB .GT. 1) THEN
  //          JB = JB - 1
  //        ELSE
  //          ISIDB = 16
  //        ENDIF
  //      ENDIF
  //      IF (ISIDB .LT. 16) GO TO 5
      until ISIDB >= 16;
//C
//C Determine closed contours by looping on interior edges --
//C   the first two sides (bottom and right) of each cell,
//C   excluding boundary edges.  The beginning cell is indexed
//C   by (IB,JB), and the beginning side number is ISIDB.
//C
      for JB := 1 to NJM1 do
      begin
        for IB := 1 to NIM1 do
        begin
          for ISIDB := 1 to 2 do
          begin
            
      //      DO 15 JB = 1,NJM1
      //      DO 14 IB = 1,NIM1
      //      DO 13 ISIDB = 1,2
              IF (JB = 1)  AND  (ISIDB = 1) then Continue;
              IF (IB = NIM1)  AND  (ISIDB = 2) then Continue;
      //        IF (JB .EQ. 1  .AND.  ISIDB .EQ. 1) GO TO 13
      //        IF (IB .EQ. NIM1  .AND.  ISIDB .EQ. 2) GO TO 13
      //C
      //C Bypass the edge if it was previously encountered
      //C   (IND/ISIDB odd).
      //C
              IND := IWK[IWK_Index(JB-1,IB-1)+IWK_Offset];
//              IF (IND/ISIDB <> 2*((IND/ISIDB)/2)) then Continue;
              IF Odd(IND div ISIDB) then Continue;
      //        IND = IWK(IB,JB)
      //        IF (IND/ISIDB .NE. 2*((IND/ISIDB)/2)) GO TO 13
      //C
      //C Determine the endpoint indexes of the beginning edge E =
      //C   (I1,J1)->(I2,J2), find the index (I,J) and side number
      //C   ISID of the cell which shares E with (IB,JB), and up-
      //C   date IWK.
      //C
              IF (ISIDB = 1) THEN
              begin
                I1 := IB;
                J1 := JB;
                I2 := IB + 1;
                J2 := JB;
                I := IB;
                J := JB - 1;
                ISID := 4;
              end
              ELSE
              begin
                I1 := IB + 1;
                J1 := JB;
                I2 := IB + 1;
                J2 := JB + 1;
                I := I1;
                J := J1;
                ISID := 8;
              END;
              IWK[IWK_Index(JB-1,IB-1)+IWK_Offset] := IND + ISIDB;
              IWK[IWK_Index(J-1,I-1)+IWK_Offset] := IWK[IWK_Index(J-1,I-1)+IWK_Offset] + ISID;
      //        IF (ISIDB .EQ. 1) THEN
      //          I1 = IB
      //          J1 = JB
      //          I2 = IB + 1
      //          J2 = JB
      //          I = IB
      //          J = JB - 1
      //          ISID = 4
      //        ELSE
      //          I1 = IB + 1
      //          J1 = JB
      //          I2 = IB + 1
      //          J2 = JB + 1
      //          I = I1
      //          J = J1
      //          ISID = 8
      //        ENDIF
      //        IWK(IB,JB) = IND + ISIDB
      //        IWK(I,J) = IWK(I,J) + ISID
      //C
      //C Proceed to the next interior edge if there is no
      //C   intersection.
      //C
              Z1 := Z[J1-1,I1-1];
              Z2 := Z[J2-1,I2-1];
              IF (((Z1 < CV ) AND  (Z2 < CV))  OR
                 ((Z1 >= CV)  AND  (Z2 >= CV))) then Continue;
      //        Z1 = Z(I1,J1)
      //        Z2 = Z(I2,J2)
      //        IF ((Z1 .LT. CV  .AND.  Z2 .LT. CV)  .OR.
      //     .      (Z1 .GE. CV  .AND.  Z2 .GE. CV)) GO TO 13
      //C
      //C Store the intersection point as the first point of a
      //C   closed contour unless NCMAX contour lines have been
      //C   found or there is insufficient space in XC and YC.
      //C
              IF (NCON = NCMX) THEN
              begin
                IER := -1;
                IF (LCON > LMX) then IER := LCON;
                NC := NCON;
                Exit;
          //   16 IF (LCON .GT. LMX) IER = LCON
          //      NC = NCON
          //      RETURN
              END;
              NCON := NCON + 1;
              LCON := LCON + 1;
              W := (CV-Z1)/(Z2-Z1);
              XP := X[I1-1] + W*(X[I2-1]-X[I1-1]);
              YP := Y[J1-1] + W*(Y[J2-1]-Y[J1-1]);
              IF (LCON <= LMX) THEN
              begin
                XC[LCON-1] := XP;
                YC[LCON-1] := YP;
              END;
              XF := XP;
              YF := YP;
      //        IF (NCON .EQ. NCMX) THEN
      //          IER = -1
      //          GO TO 16
      //        ENDIF
      //        NCON = NCON + 1
      //        LCON = LCON + 1
      //        W = (CV-Z1)/(Z2-Z1)
      //        XP = X(I1) + W*(X(I2)-X(I1))
      //        YP = Y(J1) + W*(Y(J2)-Y(J1))
      //        IF (LCON .LE. LMX) THEN
      //          XC(LCON) = XP
      //          YC(LCON) = YP
      //        ENDIF
      //        XF = XP
      //        YF = YP
      //C
      //C Traverse the contour line.  Cell (I,J) was entered on side
      //C   ISID = edge (I2,J2)->(I1,J1).  Reverse the edge direc-
      //C   tion.
              GoTo13 := False;
              repeat
      //C
                IN_Var := I1;
                JN := J1;
                I1 := I2;
                J1 := J2;
                I2 := IN_Var;
                J2 := JN;
                IND := IWK[IWK_Index(J-1,I-1)+IWK_Offset];
      //   10   IN = I1
      //        JN = J1
      //        I1 = I2
      //        J1 = J2
      //        I2 = IN
      //        J2 = JN
      //        IND = IWK(I,J)
      //C
      //C Find an exit edge E by looping on the remaining three
      //C   sides, starting with the side opposite ISID.
      //C
                GoTo12 := False;
                for K := 1 to 3 do
                begin
      //        DO 11 K = 1,3
                  ISID := 2*ISID;
                  IF (K <> 2) then ISID := 2*ISID;
                  IF (ISID > 15) then ISID := ISID div 16;
                  IF (ISID = 1) THEN
                  begin
                    I1 := I;
                    J1 := J;
                    I2 := I + 1;
                    J2 := J;
                  end
                  ELSE IF (ISID = 2) THEN
                  begin
                    I1 := I + 1;
                    J1 := J;
                    I2 := I + 1;
                    J2 := J + 1;
                  end
                  ELSE IF (ISID = 4) THEN
                  begin
                    I1 := I + 1;
                    J1 := J + 1;
                    I2 := I;
                    J2 := J + 1;
                  end
                  ELSE
                  begin
                    I1 := I;
                    J1 := J + 1;
                    I2 := I;
                    J2 := J;
                  END;
      //          ISID = 2*ISID
      //          IF (K .NE. 2) ISID = 2*ISID
      //          IF (ISID .GT. 15) ISID = ISID/16
      //          IF (ISID .EQ. 1) THEN
      //            I1 = I
      //            J1 = J
      //            I2 = I + 1
      //            J2 = J
      //          ELSEIF (ISID .EQ. 2) THEN
      //            I1 = I + 1
      //            J1 = J
      //            I2 = I + 1
      //            J2 = J + 1
      //          ELSEIF (ISID .EQ. 4) THEN
      //            I1 = I + 1
      //            J1 = J + 1
      //            I2 = I
      //            J2 = J + 1
      //          ELSE
      //            I1 = I
      //            J1 = J + 1
      //            I2 = I
      //            J2 = J
      //          ENDIF
      //C
      //C Bypass the edge if it has been previously encountered.
      //C
                IF Odd(IND div ISID) then Continue;
//                IF (IND/ISID <> 2*((IND/ISID)/2)) then Continue;
      //          IF (IND/ISID .NE. 2*((IND/ISID)/2)) GO TO 11
      //C
      //C Determine the index (IN,JN) and side number ISIDN of the
      //C   cell which shares edge E = (I1,J1)->(I2,J2) with cell
      //C   (I,J), and update IWK.
      //C
                IF (ISID <= 2) THEN
                begin
                  IN_Var := I1;
                  JN := J2 - 1;
                  ISIDN := 4*ISID;
                end
                ELSE
                begin
                  IN_Var := I1 - 1;
                  JN := J2;
                  ISIDN := ISID div 4;
                END;
                IWK[IWK_Index(J-1,I-1)+IWK_Offset] := IWK[IWK_Index(J-1,I-1)+IWK_Offset] + ISID;
                IWK[IWK_Index(JN-1,IN_Var-1)+IWK_Offset] := IWK[IWK_Index(JN-1,IN_Var-1)+IWK_Offset] + ISIDN;
      //          IF (ISID .LE. 2) THEN
      //            IN = I1
      //            JN = J2 - 1
      //            ISIDN = 4*ISID
      //          ELSE
      //            IN = I1 - 1
      //            JN = J2
      //            ISIDN = ISID/4
      //          ENDIF
      //          IWK(I,J) = IWK(I,J) + ISID
      //          IWK(IN,JN) = IWK(IN,JN) + ISIDN
      //C
      //C Exit the loop on sides if E is intersected.
      //C
                Z1 := Z[J1-1,I1-1];
                Z2 := Z[J2-1,I2-1];
      //          Z1 = Z(I1,J1)
      //          Z2 = Z(I2,J2)
                IF (((Z1 < CV)  AND  (Z2 >= CV)) OR
                   ((Z1 >= CV)  AND  (Z2 < CV))) then
                begin
                   GoTo12 := True;
                   break;
                end;
      //          IF ((Z1 .LT. CV  .AND.  Z2 .GE. CV)  .OR.
      //     .        (Z1 .GE. CV  .AND.  Z2 .LT. CV)) GO TO 12
      //   11     CONTINUE
                end;
                if not GoTo12 then
                begin
          //C*
          //C Error -- No exit point found.  Print a message and exit
          //C          the contour traversal loop.
          //C
{$IFDEF UseSfrMessages}
                  Cntour110(IFORTRAN, NCON);
{$ENDIF}
          //      call Cntour110(IFORTRAN, NCON)
          //!        WRITE (LUN,110) NCON
          //!  110   FORMAT (///5X,'Error in CNTOUR:  Contour line L ',
          //!     .          'is open but'/5X,'does not intersect the ',
          //!     .          'boundary for L =',I4/)
                  ILC[NCON-1] := LCON;
                  GoTo13 := True;
                  break;
          //        ILC(NCON) = LCON
          //        GO TO 13
          //C*
          //C Add the intersection point to the list unless it coincides
          //C   with the previous point or there is not enough space in
          //C   XC and YC.
          //C
                  
                end;
                W := (CV-Z1)/(Z2-Z1);
                XN := X[I1-1] + W*(X[I2-1]-X[I1-1]);
                YN := Y[J1-1] + W*(Y[J2-1]-Y[J1-1]);
                IF (XN <> XP)  OR  (YN <> YP) THEN
                begin
                  LCON := LCON + 1;
                  XP := XN;
                  YP := YN;
                  IF (LCON <= LMX) THEN
                  begin
                    XC[LCON-1] := XN;
                    YC[LCON-1] := YN;
                  END;
                END;
      //   12   W = (CV-Z1)/(Z2-Z1)
      //        XN = X(I1) + W*(X(I2)-X(I1))
      //        YN = Y(J1) + W*(Y(J2)-Y(J1))
      //        IF (XN .NE. XP  .OR.  YN .NE. YP) THEN
      //          LCON = LCON + 1
      //          XP = XN
      //          YP = YN
      //          IF (LCON .LE. LMX) THEN
      //            XC(LCON) = XN
      //            YC(LCON) = YN
      //          ENDIF
      //        ENDIF
      //C
      //C Bottom of contour traversal loop.  If the next cell is not
      //C   the beginning cell, update the cell index and side num-
      //C   ber.
      //C
              IF (IN_Var <> IB)  OR  (JN <> JB) THEN
              begin
                I := IN_Var;
                J := JN;
                ISID := ISIDN;
                Continue;
              END;
      //        IF (IN .NE. IB  .OR.  JN .NE. JB) THEN
      //          I = IN
      //          J = JN
      //          ISID = ISIDN
      //          GO TO 10
      //        ENDIF
                break;
              until False;
              if GoTo13 then
              begin
                Continue;
              end;
      //C
      //C Add the first point as the last point (unless the first
      //C   and last points already coincide), and update ILC.
      //C
              IF (XP <> XF)  OR  (YP <> YF) THEN
              begin
                LCON := LCON + 1;
                IF (LCON <= LMX) THEN
                begin
                  XC[LCON-1] := XF;
                  YC[LCON-1] := YF;
                END;
              END;
              ILC[NCON-1] := LCON;
      //        IF (XP .NE. XF  .OR.  YP .NE. YF) THEN
      //          LCON = LCON + 1
      //          IF (LCON .LE. LMX) THEN
      //            XC(LCON) = XF
      //            YC(LCON) = YF
      //          ENDIF
      //        ENDIF
      //        ILC(NCON) = LCON
      //C
      //C Bottom of loop on interior edges.
      //C
      //   13   CONTINUE
      //   14   CONTINUE
      //   15   CONTINUE
          end;
        end;
      end;
      IER := 0;
//      IER = 0
//C
//C Test for insufficient storage reserved for XC and YC.
//C
      IF (LCON > LMX) then IER := LCON;
      NC := NCON;
//   16 IF (LCON .GT. LMX) IER = LCON
//      NC = NCON
//      RETURN
//      END
end;

procedure COORDS (const XP,YP,X1,X2,X3,Y1,Y2,Y3: TFloat; var  B1,B2, B3: TFloat; var IER: longint);
//      SUBROUTINE COORDS (XP,YP,X1,X2,X3,Y1,Y2,Y3, B1,B2,
//     .                   B3, IER)
//      INTEGER IER
//      REAL    XP, YP, X1, X2, X3, Y1, Y2, Y3, B1, B2, B3
//C
//C***********************************************************
//C
//C                                               From SRFPACK
//C                                            Robert J. Renka
//C                                  Dept. of Computer Science
//C                                       Univ. of North Texas
//C                                           renka@cs.unt.edu
//C                                                   09/01/88
//C
//C   This subroutine computes the barycentric (areal) coordi-
//C nates B1, B2, and B3 of a point P with respect to the tri-
//C angle with vertices P1, P2, and P3:  the solution to the
//C linear system defined by B1 + B2 + B3 = 1 and B1*P1 +
//C B2*P2 + B3*P3 = P.  Note that B1 is a linear function
//C of P which satisfies B1 = 1 at P = P1 and B1 = 0 on
//C the triangle side P2-P3.  Also, B1 < 0 if and only if
//C P is to the right of P2->P3 (and thus exterior to the
//C triangle).  B2 and B3 satisfy similar properties.
//C
//C On input:
//C
//C       XP,YP = Cartesian coordinates of P.
//C
//C       X1,X2,X3,Y1,Y2,Y3 = Coordinates of the vertices of
//C                           the triangle P1, P2, and P3.
//C
//C Input parameters are not altered by this routine.
//C
//C On output:
//C
//C       B1,B2,B3 = Barycentric coordinates unless IER = 1,
//C                  in which case the coordinates are not
//C                  defined.
//C
//C       IER = Error indicator:
//C             IER = 0 if no errors were encountered.
//C             IER = 1 if the vertices of the triangle are
//C                     collinear.
//C
//C Modules required by COORDS:  None
//C
//C***********************************************************
//C
var
       A, PX, PY, XP1, XP2, XP3, YP1, YP2, YP3: TFloat;
//      REAL A, PX, PY, XP1, XP2, XP3, YP1, YP2, YP3
begin
//C
      PX := XP;
      PY := YP;
//      PX = XP
//      PY = YP
//C
//C Compute components of the vectors P->P1, P->P2, and P->P3.
//C
      XP1 := X1 - PX;
      YP1 := Y1 - PY;
      XP2 := X2 - PX;
      YP2 := Y2 - PY;
      XP3 := X3 - PX;
      YP3 := Y3 - PY;
//      XP1 = X1 - PX
//      YP1 = Y1 - PY
//      XP2 = X2 - PX
//      YP2 = Y2 - PY
//      XP3 = X3 - PX
//      YP3 = Y3 - PY
//C
//C Compute subtriangle areas B1 = P->P2 X P->P3, B2 = P->P3 X
//C   P->P1, and B3 = P->P1 X P->P2.
//C
      B1 := XP2*YP3 - XP3*YP2;
      B2 := XP3*YP1 - XP1*YP3;
      B3 := XP1*YP2 - XP2*YP1;
//      B1 = XP2*YP3 - XP3*YP2
//      B2 = XP3*YP1 - XP1*YP3
//      B3 = XP1*YP2 - XP2*YP1
//C
//C Compute twice the signed area of the triangle.
//C
      A := B1 + B2 + B3;
      IF (A = 0.0) then
      begin
//C The vertices are collinear.
//C
        IER := -1;
        Exit;
//    1 IER = -1
//      RETURN
      end;
//      A = B1 + B2 + B3
//      IF (A .EQ. 0.) GO TO 1
//C
//C Normalize the coordinates.
//C
      B1 := B1/A;
      B2 := B2/A;
      B3 := B3/A;
      IER := 0;
//      B1 = B1/A
//      B2 = B2/A
//      B3 = B3/A
//      IER = 0
//      RETURN
//C
//C The vertices are collinear.
//C
//    1 IER = -1
//      RETURN
//      END
end;

procedure CRPLOT (const LUN: longint; const PLTSIZ: TFloat; const NX,NY: longint;
  const PX,PY: TNiSingleArray; const PZ: TNi_NjSingleArray; const NCON: longint;
  var IWK: TLwkIntArray; var XC,YC: TNmaxSingleArray; var IER: longint);
//      SUBROUTINE CRPLOT (LUN,PLTSIZ,NX,NY,PX,PY,PZ,NCON,IWK,
//     .                   XC,YC, IER,ifortran)
//      dll_import Crplot100, Crplot110, Crplot120, Crplot130, Crplot140,
//     .  Crplot150, Crplot160, Crplot170, Crplot180, Crplot200, Crplot210
//      INTEGER LUN, NX, NY, NCON, IWK(*), IER
//      REAL    PLTSIZ, PX(NX), PY(NY), PZ(NX,NY),
//     .        XC(*), YC(*)
//C
//C***********************************************************
//C
//C                                               From SRFPACK
//C                                            Robert J. Renka
//C                                  Dept. of Computer Science
//C                                       Univ. of North Texas
//C                                           renka@cs.unt.edu
//C                                                   04/12/97
//C
//C   Given a set of function values PZ = F(X,Y) at the ver-
//C tices of an NX by NY rectangular grid, this subroutine
//C creates a level-2 Encapsulated PostScript (EPS) file
//C containing a contour plot of the piecewise bilinear inter-
//C polant of the function values.
//C
//C   The accuracy of the contour lines increases with the
//C number of grid points.  Refer to Subroutine CNTOUR for
//C further details.
//C
//C
//C On input:
//C
//C       LUN = Logical unit number in the range 0 to 99.
//C             The unit should be opened with an appropriate
//C             file name before the call to this routine.
//C
//C       PLTSIZ = Plot size in inches.  A window containing
//C                the plot is mapped, with aspect ratio
//C                preserved, to a rectangular viewport with
//C                maximum side-length PLTSIZ.  The viewport
//C                is centered on the 8.5 by 11 inch page, and
//C                its boundary is drawn.  1.0 .LE. PLTSIZ
//C                .LE. 7.5.
//C
//C       NX = Number of grid points in the x direction.
//C            NX .GE. 2.
//C
//C       NY = Number of grid points in the y direction.
//C            NY .GE. 2.
//C
//C       PX = Array of length NX containing a strictly in-
//C            creasing sequence of values.
//C
//C       PY = Array of length NY containing a strictly in-
//C            creasing sequence of values.
//C
//C       PZ = Array of function values at the vertices of the
//C            rectangular grid.  PZ(I,J) = F(PX(I),PY(J)) for
//C            I = 1,...,NX and J = 1,...,NY.
//C
//C       NCON = Number of contour values.  The contour values
//C              are uniformly distributed over the range of
//C              PZ values.  NCON .GE. 1.
//C
//C The above parameters are not altered by this routine.
//C
//C       IWK = Integer array of length at least 1.5*NX*NY to
//C             be used as work space.
//C
//C       XC,YC = Real arrays of length at least 2.5*NX*NY to
//C               be used as work space.
//C
//C On output:
//C
//C       IER = Error indicator:
//C             IER = 0 if no errors were encountered.
//C             IER = 1 if LUN, PLTSIZ, NX, NY, or NCON is
//C                     outside its valid range.
//C             IER = 2 if PX or PY is not strictly
//C                     increasing.
//C             IER = 3 if the range of PZ values has zero
//C                     width (F is constant).
//C             IER = 4 if an error was encountered in writing
//C                     to unit LUN.
//C             IER = 5 if an unexpected error flag was re-
//C                     turned by Subroutine CNTOUR.  This
//C                     should not occur.
//C
//C   In the unlikely event of an internal failure, a message
//C is printed on the standard output device.  IER may be 0
//C in this case.
//C
//C Module required by CRPLOT:  CNTOUR
//C
//C Intrinsic functions called by CRPLOT:  CHAR, REAL
//C
//C***********************************************************
//C
var
       I, IC, IERR, IH, IPX1, IPX2, IPY1, IPY2, IW,
             J, K, KV, LC, NC, NCMAX: longint;
          CVAL, DX, DY, DZ, PZIJ, R, SFX, SFY, T,
             TX, TY, ZMAX, ZMIN: TFloat;
//      INTEGER I, IC, IERR, IH, IPX1, IPX2, IPY1, IPY2, IW,
//     .        J, K, KV, LC, NC, NCMAX
//      REAL    CVAL, DX, DY, DZ, PZIJ, R, SFX, SFY, T,
//     .        TX, TY, ZMAX, ZMIN
//C
//C Local parameters:
//C
//C CVAL =      Contour value between ZMIN and ZMAX
//C DX =        Window width PX(NX)-PX(1)N
//C DY =        Window height PY(NY)-PY(1)
//C DZ =        Interval between contour values:
//C               (ZMAX-ZMIN)/(NCON+1)
//C I,J =       Row and column indexes for PZ
//C IC =        Index (for IWK) of a contour line associated
//C               with contour value CVAL:  1 to NC
//C IERR =      Error flag for calls to CNTOUR
//C IH =        Height of the bounding box (viewport) in
//C               points
//C IPX1,IPY1 = X and y coordinates (in points) of the lower
//C               left corner of the bounding box
//C IPX2,IPY2 = X and y coordinates (in points) of the upper
//C               right corner of the bounding box
//C IW =        Width of the bounding box in points
//C K =         Index (for XC and YC) of a point on a contour
//C               line
//C KV =        DO-loop index for loop on contour values
//C LC =        Length of arrays XC and YC
//C NC =        Number of contour lines associated with
//C               contour value CVAL
//C NCMAX =     Maximum allowable value of NC
//C PZIJ =      PZ(I,J)
//C R =         Aspect ratio DX/DY
//C SFX,SFY =   Scale factors for mapping window coordinates
//C               to viewport coordinates
//C T =         Temporary variable
//C TX,TY =     Translation vector for mapping window coordi-
//C               nates to viewport coordinates
//C ZMIN,ZMAX = Minimum and maximum of the PZ values
//C
       IFORTRAN: longint;
  Char4: Char;
//      integer IFORTRAN
begin
  IFORTRAN := 4;
//C
//C Test for error 1.
//C
      IF (LUN < 0) or (LUN > 99) OR
         (PLTSIZ < 1.0) or (PLTSIZ > 7.5) OR
         (NX < 2) or (NY < 2) or (NCON < 1) then
      begin
  //C Invalid input parameter.
  //C
        IER := 1;
        Exit;
  //   11 IER = 1
  //      RETURN
      end;
//      IF (LUN .LT. 0  .OR.  LUN .GT. 99  .OR.
//     .    PLTSIZ .LT. 1.0  .OR.  PLTSIZ .GT. 7.5  .OR.
//     .    NX .LT. 2  .OR.  NY .LT. 2  .OR.  NCON .LT. 1)
//     .  GO TO 11
//C
//C Compute the aspect ratio of the window.
//C
      DX := PX[NX-1] - PX[0];
      DY := PY[NY-1] - PY[0];
      IF (DX = 0.0)  OR  (DY = 0.0) then
      begin
  //C PX or PY is not strictly increasing.
  //C
        IER := 2;
        Exit;
  //   12 IER = 2
  //      RETURN
      end;
      R := DX/DY;
//      DX = PX(NX) - PX(1)
//      DY = PY(NY) - PY(1)
//      IF (DX .EQ. 0.0  .OR.  DY .EQ. 0.0) GO TO 12
//      R = DX/DY
//C
//C Compute the range of PZ values and the interval between
//C   contour values.
//C
      ZMIN := PZ[0,0];
      ZMAX := ZMIN;
      for J := 1 to NY do
      begin
        for I := 1 to NX do
        begin
          PZIJ := PZ[J-1,I-1];
          IF (PZIJ < ZMIN) then ZMIN := PZIJ;
          IF (PZIJ > ZMAX) then ZMAX := PZIJ;
        end;
      end;
      DZ := (ZMAX-ZMIN)/(NCON+1);
      IF (DZ <= 0.0) then
      begin
  //C DZ = 0.
  //C
        IER := 3;
        Exit;
  //   13 IER = 3
  //      RETURN
      end;
//      ZMIN = PZ(1,1)
//      ZMAX = ZMIN
//      DO 2 J = 1,NY
//        DO 1 I = 1,NX
//          PZIJ = PZ(I,J)
//          IF (PZIJ .LT. ZMIN) ZMIN = PZIJ
//          IF (PZIJ .GT. ZMAX) ZMAX = PZIJ
//    1     CONTINUE
//    2   CONTINUE
//      DZ = (ZMAX-ZMIN)/REAL(NCON+1)
//      IF (DZ .LE. 0.0) GO TO 13
//C
//C Compute the lower left (IPX1,IPY1) and upper right
//C   (IPX2,IPY2) corner coordinates of the bounding box
//C   (the viewport).  The coordinates, specified in default
//C   user space units (points, at 72 points/inch with origin
//C   at the lower left corner of the page), are chosen to
//C   preserve the aspect ratio R, and to center the plot on
//C   the 8.5 by 11 inch page.  The center of the page is
//C   (306,396), and T = PLTSIZ/2 in points.
//C
      T := 36.0*PLTSIZ;
      IF (R >= 1.0) THEN
      begin
        IPX1 := 306 - Round(T);
        IPX2 := 306 + Round(T);
        IPY1 := 396 - Round(T/R);
        IPY2 := 396 + Round(T/R);
      end
      ELSE
      begin
        IPX1 := 306 - Round(T*R);
        IPX2 := 306 + Round(T*R);
        IPY1 := 396 - Round(T);
        IPY2 := 396 + Round(T);
      END;
//      T = 36.0*PLTSIZ
//      IF (R .GE. 1.0) THEN
//        IPX1 = 306 - NINT(T)
//        IPX2 = 306 + NINT(T)
//        IPY1 = 396 - NINT(T/R)
//        IPY2 = 396 + NINT(T/R)
//      ELSE
//        IPX1 = 306 - NINT(T*R)
//        IPX2 = 306 + NINT(T*R)
//        IPY1 = 396 - NINT(T)
//        IPY2 = 396 + NINT(T)
//      ENDIF
//C
//C Output header comments.
//C
{$IFDEF UseSfrMessages}
      Crplot100(IFORTRAN, IPX1, IPY1, IPX2, IPY2);
//      CALL Crplot100(IFORTRAN, IPX1, IPY1, IPX2, IPY2)
//!      WRITE (LUN,100,ERR=14) IPX1, IPY1, IPX2, IPY2
//!  100 FORMAT ('%!PS-Adobe-3.0 EPSF-3.0'/
//!     .        '%%BoundingBox:',4I4/
//!     .        '%%Title:  Contour Plot'/
//!     .        '%%Creator:  SRFPACK'/
//!     .        '%%EndComments')
//C
//C Draw the bounding box.
//C
      Crplot110(IFORTRAN, IPX1, IPY1);
      Crplot120(IFORTRAN, IPX1, IPY2);
      Crplot120(IFORTRAN, IPX2, IPY2);
      Crplot120(IFORTRAN, IPX2, IPY1);
      Crplot130(IFORTRAN);
      Crplot140(IFORTRAN);
{$ENDIF}
//      CALL Crplot110(IFORTRAN, IPX1, IPY1)
//      CALL Crplot120(IFORTRAN, IPX1, IPY2)
//      CALL Crplot120(IFORTRAN, IPX2, IPY2)
//      CALL Crplot120(IFORTRAN, IPX2, IPY1)
//      CALL Crplot130(IFORTRAN)
//      CALL Crplot140(IFORTRAN)
//!      WRITE (LUN,110,ERR=14) IPX1, IPY1
//!      WRITE (LUN,120,ERR=14) IPX1, IPY2
//!      WRITE (LUN,120,ERR=14) IPX2, IPY2
//!      WRITE (LUN,120,ERR=14) IPX2, IPY1
//!      WRITE (LUN,130,ERR=14)
//!      WRITE (LUN,140,ERR=14)
//!  110 FORMAT (2I4,' moveto')
//!  120 FORMAT (2I4,' lineto')
//!  130 FORMAT ('closepath')
//!  140 FORMAT ('stroke')
//C
//C Set up a mapping from the window to the viewport.
//C
      IW := IPX2 - IPX1;
      IH := IPY2 - IPY1;
      SFX := IW/DX;
      SFY := IH/DY;
      TX := IPX1 - SFX*PX[0];
      TY := IPY1 - SFY*PY[0];
{$IFDEF UseSfrMessages}
      Crplot150(IFORTRAN, TX, TY, SFX, SFY);
{$ENDIF}
//      IW = IPX2 - IPX1
//      IH = IPY2 - IPY1
//      SFX = REAL(IW)/DX
//      SFY = REAL(IH)/DY
//      TX = IPX1 - SFX*PX(1)
//      TY = IPY1 - SFY*PY(1)
//      CALL Crplot150(IFORTRAN, TX, TY, SFX, SFY)
//!      WRITE (LUN,150,ERR=14) TX, TY, SFX, SFY
//!  150 FORMAT (2F12.6,' translate'/
//!     .        2F12.6,' scale')
//C
//C Set the line thickness to 2 points.  (Since the scale
//C   factors are applied to everything, the width must be
//C   specified in world coordinates.)
//C
{$IFDEF UseSfrMessages}
      T := 4.0/(SFX+SFY);
      Crplot160(IFORTRAN, T);
{$ENDIF}
//      T = 4.0/(SFX+SFY)
//      CALL Crplot160(IFORTRAN, T)
//!      WRITE (LUN,160,ERR=14) T
//!  160 FORMAT (F12.6,' setlinewidth')
//C
//C Compute parameters for CNTOUR:
//C
//C   NCMAX = Maximum allowable number of contour lines
//C           associated with each contour value.
//C   LC = Length of arrays XC and YC and maximum allowable
//C        number of points defining all the contour lines
//C        associated with a contour value.
//C
      NCMAX := (NX*NY+1) div 2;
      LC := 2*(NX-1)*(NY-1) + NCMAX;
//      NCMAX = (NX*NY+1)/2
//      LC = 2*(NX-1)*(NY-1) + NCMAX
//C
//C Loop on contour values CVAL uniformly spaced in the open
//C   interval (ZMIN,ZMAX).
//C
      CVAL := ZMIN;
//      CVAL = ZMIN
      for KV := 1 to NCON do
      begin
//      DO 5 KV = 1,NCON
        CVAL := CVAL + DZ;
//        CVAL = CVAL + DZ
//C
//C Compute a sequence of NC contour lines associated with
//C   F = CVAL.  For IC = 1 to NC, IWK(IC) is the index (for
//C   XC and YC) of the last point of contour IC.
//C
        CNTOUR (NX,NY,PX,PY,PZ,CVAL,LC,NCMAX,
                    IWK, XC,YC,IWK,NC,IERR, NCMAX);
        IF (IERR = 2) then
        begin
    //C PX or PY is not strictly increasing.
    //C
          IER := 2;
          Exit;
    //   12 IER = 2
    //      RETURN
        end;
        IF (IERR<> 0) then
        begin
    //C Error flag returned by CNTOUR.
    //C
          IER := 5;
          Exit;
    //   15 IER = 5
    //      RETURN
        end;
//        CALL CNTOUR (NX,NY,PX,PY,PZ,CVAL,LC,NCMAX,
//     .               IWK(NCMAX+1), XC,YC,IWK,NC,IERR,ifortran)
//        IF (IERR .EQ. 2) GO TO 12
//        IF (IERR .NE. 0) GO TO 15
//C
//C Draw the NC contours.
//C
        IC := 0;
        K := 0;
//        IC = 0
//        K = 0
        repeat
          IC := IC + 1;
          K := K + 1;
//    3   IC = IC + 1
//          K = K + 1
//C
//C   Create a path consisting of contour IC.
//C
{$IFDEF UseSfrMessages}
          Crplot170(IFORTRAN, XC[K-1], YC[K-1]);
{$ENDIF}
//          CALL Crplot170(IFORTRAN, XC(K), YC(K))
//!          WRITE (LUN,170,ERR=14) XC(K), YC(K)
//!  170     FORMAT (2F12.6,' moveto')
          repeat
            K := K + 1;
{$IFDEF UseSfrMessages}
            Crplot180(IFORTRAN, XC[K-1], YC[K-1]);
{$ENDIF}
//    4     K = K + 1
//            CALL Crplot180(IFORTRAN, XC(K), YC(K))
//!            WRITE (LUN,180,ERR=14) XC(K), YC(K)
//!  180       FORMAT (2F12.6,' lineto')
//            IF (K .NE. IWK(IC)) GO TO 4

          until K = IWK[IC-1];
//C
//C   Paint the path.
//C
{$IFDEF UseSfrMessages}
          Crplot140(IFORTRAN);
{$ENDIF}
//          CALL Crplot140(IFORTRAN)
//!          WRITE (LUN,140,ERR=14)
//          IF (IC .NE. NC) GO TO 3

        until IC = NC;
//    5   CONTINUE
      end;
//C
//C Output the showpage command and end-of-file indicator.
//C
{$IFDEF UseSfrMessages}
      Crplot200(IFORTRAN);
{$ENDIF}
//      CALL Crplot200(IFORTRAN)
//!      WRITE (LUN,200,ERR=14)
//!  200 FORMAT ('showpage'/
//!     .        '%%EOF')
//C
//C HP's interpreters require a one-byte End-of-PostScript-Job
//C   indicator (to eliminate a timeout error message):
//C   ASCII 4.
//C
      Char4 := Char(4);
{$IFDEF UseSfrMessages}
      Crplot210(IFORTRAN, Char4);
{$ENDIF}
//      CALL Crplot210(IFORTRAN, CHAR(4))
//!      WRITE (LUN,210,ERR=14) CHAR(4)
//  210 FORMAT (A1)
//C
//C No error encountered.
//C
      IER := 0;
//      IER = 0
//      RETURN
//C
//C Invalid input parameter.
//C
//   11 IER = 1
//      RETURN
//C
//C PX or PY is not strictly increasing.
//C
//   12 IER = 2
//      RETURN
//C
//C DZ = 0.
//C
//   13 IER = 3
//      RETURN
//C
//C Error writing to unit LUN.
//C
//   14 IER = 4
//      RETURN
//C
//C Error flag returned by CNTOUR.
//C
//   15 IER = 5
//      RETURN
//      END
end;

procedure FVAL (const XP,YP,X1,X2,X3,Y1,Y2,Y3,F1,F2,F3,
  FX1,FX2,FX3,FY1,FY2,FY3,SIG1,SIG2,
  SIG3: TFloat; var  FP: TFloat; var IER: longint);
//      SUBROUTINE FVAL (XP,YP,X1,X2,X3,Y1,Y2,Y3,F1,F2,F3,
//     .                 FX1,FX2,FX3,FY1,FY2,FY3,SIG1,SIG2,
//     .                 SIG3, FP,IER)
//      INTEGER IER
//      REAL    XP, YP, X1, X2, X3, Y1, Y2, Y3, F1, F2,
//     .        F3, FX1, FX2, FX3, FY1, FY2, FY3, SIG1,
//     .        SIG2, SIG3, FP
//C
//C***********************************************************
//C
//C                                               From SRFPACK
//C                                            Robert J. Renka
//C                                  Dept. of Computer Science
//C                                       Univ. of North Texas
//C                                           renka@cs.unt.edu
//C                                                   03/18/90
//C
//C   Given function values and gradients at the three ver-
//C tices of a triangle containing a point P, this routine
//C computes the value of F at P where F interpolates the ver-
//C tex data.  Along the triangle arcs, the interpolatory
//C function F is the Hermite interpolatory tension spline de-
//C fined by the values and tangential gradient components at
//C the endpoints, and the derivative in the direction normal
//C to the arc varies linearly between the normal gradient
//C components at the endpoints.  A first-order C-1 blending
//C method is used to extend F to the interior of the trian-
//C gle.  Thus, since values and gradients on an arc depend
//C only on the vertex data, the method results in C-1 contin-
//C uity when used to interpolate over a triangulation.
//C
//C   The blending method consists of taking F(P) to be the
//C weighted sum of the values at P of the three univariate
//C Hermite interpolatory tension splines defined on the line
//C segments which join the vertices to the opposite sides and
//C pass through P.  The tension factors for these splines are
//C obtained by linear interpolation between the pair of ten-
//C sion factors associated with the triangle sides which join
//C at the appropriate vertex.
//C
//C   A tension factor SIGMA associated with a Hermite interp-
//C olatory tension spline is a nonnegative parameter which
//C determines the curviness of the spline.  SIGMA = 0 results
//C in a cubic spline, and the spline approaches the linear
//C interpolant as SIGMA increases.
//C
//C On input:
//C
//C       XP,YP = Coordinates of a point P at which an interp-
//C               olated value is to be computed.
//C
//C       X1,X2,X3,Y1,Y2,Y3 = Coordinates of the vertices of a
//C                           triangle (V1,V2,V3) containing
//C                           P.  V3 is strictly to the left
//C                           of V1->V2.
//C
//C       F1,F2,F3 = Values of the interpolatory function at
//C                  the vertices.
//C
//C       FX1,FX2,FX3 = x components of the gradients of F at
//C                     the vertices.
//C
//C       FY1,FY2,FY3 = y components of the gradients of F at
//C                     the vertices.
//C
//C       SIG1,SIG2,SIG3 = Tension factors associated with the
//C                        arcs opposite V1, V2, and V3, re-
//C                        spectively.
//C
//C Input parameters are not altered by this routine.
//C
//C On output:
//C
//C       FP = Interpolated value at P unless IER < 0, in
//C            which case FP is not defined.
//C
//C       IER = Error indicator:
//C             IER = 0 if no errors were encountered.
//C             IER = 1 if P is not contained in the triangle.
//C                     This may result from roundoff error
//C                     when P lies near an arc, and the int-
//C                     erpolated value FP is valid in that
//C                     case.
//C             IER = -1 if the triangle vertices are
//C                      collinear.
//C
//C SRFPACK modules required by FVAL:  ARCINT, COORDS, SNHCSH
//C
//C***********************************************************
//C
var
       IERR: longint;
          B, B1, B2, B3, C1, C2, C3, DUM, FQ, FXQ, FYQ,
             H1, H2, H3, PX, PY, SIG, SUM, XQ, YQ: TFloat;
  DFLAG_Temp: longbool;
//      INTEGER IERR
//      REAL    B, B1, B2, B3, C1, C2, C3, DUM, FQ, FXQ, FYQ,
//     .        H1, H2, H3, PX, PY, SIG, SUM, XQ, YQ
begin
//C
      PX := XP;
      PY := YP;
//      PX = XP
//      PY = YP
//C
//C F(P) = C1*H1(P) + C2*H2(P) + C3*H3(P) where C1, C2, and C3
//C   are weight functions which sum to 1, and H1, H2, and H3
//C   are Hermite interpolatory tension splines on the line
//C   segments which join vertices to opposite sides and con-
//C   tain P.
//C
//C Compute barycentric coordinates of P with respect to the
//C   triangle.
//C
      COORDS (PX,PY,X1,X2,X3,Y1,Y2,Y3, B1,B2,B3,IER);
      IF (IER <> 0) Then Exit;
      IF (B1 < 0.0)  OR  (B2 < 0.0)  OR  (B3 < 0.0) then
      begin
        IER := 1;
      end;
//      CALL COORDS (PX,PY,X1,X2,X3,Y1,Y2,Y3, B1,B2,B3,IER)
//      IF (IER .NE. 0) RETURN
//      IF (B1 .LT. 0.  .OR.  B2 .LT. 0.  .OR.  B3 .LT. 0.)
//     .   IER = 1
//C
//C Compute the coefficients of the partial interpolants.
//C   C1 = 1 on the side opposite V1, and C1 = 0 on the other
//C   arcs.  Similarly for C2 and C3.
//C
      C1 := B2*B3;
      C2 := B3*B1;
      C3 := B1*B2;
      SUM := C1 + C2 + C3;
//      C1 = B2*B3
//      C2 = B3*B1
//      C3 = B1*B2
//      SUM = C1 + C2 + C3
      IF (SUM = 0.0) THEN
      begin
//      IF (SUM .EQ. 0.) THEN
//C
//C P coincides with a vertex.
//C
        FP := B1*F1 + B2*F2 + B3*F3;
        Exit;
      END;
//        FP = B1*F1 + B2*F2 + B3*F3
//        RETURN
//      ENDIF
//C
//C Normalize the coefficients.
//C
      C1 := C1/SUM;
      C2 := C2/SUM;
      C3 := C3/SUM;
//      C1 = C1/SUM
//      C2 = C2/SUM
//      C3 = C3/SUM
//C
//C For each vertex Vi, compute the intersection Q of the side
//C   opposite Vi with the line defined by Vi and P, the value
//C   and gradient at Q, and the partial interpolant value Hi
//C   at P.
//C
//C   Side opposite V1:
//C
      B := B2/(B2+B3);
      XQ := B*X2 + (1.0-B)*X3;
      YQ := B*Y2 + (1.0-B)*Y3;
      SIG := B*SIG3 + (1.0-B)*SIG2;
      DFLAG_Temp := True;
      ARCINT (B,X2,X3,Y2,Y3,F2,F3,FX2,FX3,FY2,FY3,SIG1,
                  DFLAG_Temp, FQ,FXQ,FYQ,IERR);
      DFLAG_Temp := False;
      ARCINT (B1,X1,XQ,Y1,YQ,F1,FQ,FX1,FXQ,FY1,FYQ,SIG,
                  DFLAG_Temp, H1,DUM,DUM,IERR);
//      B = B2/(B2+B3)
//      XQ = B*X2 + (1.-B)*X3
//      YQ = B*Y2 + (1.-B)*Y3
//      SIG = B*SIG3 + (1.-B)*SIG2
//      CALL ARCINT (B,X2,X3,Y2,Y3,F2,F3,FX2,FX3,FY2,FY3,SIG1,
//     .             .TRUE., FQ,FXQ,FYQ,IERR)
//      CALL ARCINT (B1,X1,XQ,Y1,YQ,F1,FQ,FX1,FXQ,FY1,FYQ,SIG,
//     .             .FALSE., H1,DUM,DUM,IERR)
//C
//C   Side opposite V2:
//C
      B := B3/(B3+B1);
      XQ := B*X3 + (1.-B)*X1;
      YQ := B*Y3 + (1.-B)*Y1;
      SIG := B*SIG1 + (1.-B)*SIG3;
      DFLAG_Temp := True;
      ARCINT (B,X3,X1,Y3,Y1,F3,F1,FX3,FX1,FY3,FY1,SIG2,
                  DFLAG_Temp, FQ,FXQ,FYQ,IERR);
      DFLAG_Temp := False;
      ARCINT (B2,X2,XQ,Y2,YQ,F2,FQ,FX2,FXQ,FY2,FYQ,SIG,
                  DFLAG_Temp, H2,DUM,DUM,IERR);
//      B = B3/(B3+B1)
//      XQ = B*X3 + (1.-B)*X1
//      YQ = B*Y3 + (1.-B)*Y1
//      SIG = B*SIG1 + (1.-B)*SIG3
//      CALL ARCINT (B,X3,X1,Y3,Y1,F3,F1,FX3,FX1,FY3,FY1,SIG2,
//     .             .TRUE., FQ,FXQ,FYQ,IERR)
//      CALL ARCINT (B2,X2,XQ,Y2,YQ,F2,FQ,FX2,FXQ,FY2,FYQ,SIG,
//     .             .FALSE., H2,DUM,DUM,IERR)
//C
//C   Side opposite V3:
//C
      B := B1/(B1+B2);
      XQ := B*X1 + (1.-B)*X2;
      YQ := B*Y1 + (1.-B)*Y2;
      SIG := B*SIG2 + (1.-B)*SIG1;
      DFLAG_Temp := True;
      ARCINT (B,X1,X2,Y1,Y2,F1,F2,FX1,FX2,FY1,FY2,SIG3,
                  DFLAG_Temp, FQ,FXQ,FYQ,IERR);
      DFLAG_Temp := False;
      ARCINT (B3,X3,XQ,Y3,YQ,F3,FQ,FX3,FXQ,FY3,FYQ,SIG,
                  DFLAG_Temp, H3,DUM,DUM,IERR);
//      B = B1/(B1+B2)
//      XQ = B*X1 + (1.-B)*X2
//      YQ = B*Y1 + (1.-B)*Y2
//      SIG = B*SIG2 + (1.-B)*SIG1
//      CALL ARCINT (B,X1,X2,Y1,Y2,F1,F2,FX1,FX2,FY1,FY2,SIG3,
//     .             .TRUE., FQ,FXQ,FYQ,IERR)
//      CALL ARCINT (B3,X3,XQ,Y3,YQ,F3,FQ,FX3,FXQ,FY3,FYQ,SIG,
//     .             .FALSE., H3,DUM,DUM,IERR)
//C
//C Accumulate the partial interpolant values.
//C
      FP := C1*H1 + C2*H2 + C3*H3;
//      FP = C1*H1 + C2*H2 + C3*H3
//      RETURN
//      END
end;

procedure GETSIG (const N: longint; const X,Y,H: TNmaxSingleArray;
  const LIST,LPTR: TN6IntArray; const LEND: TNmaxIntArray; const HXHY: TNtnxSingleArray;
  const TOL: TFloat; var SIGMA: TN6SingleArray; var DSMAX: TFloat; var IER: longint);
//      SUBROUTINE GETSIG (N,X,Y,H,LIST,LPTR,LEND,HXHY,
//     .                   TOL, SIGMA, DSMAX,IER,ifortran)
//      dll_import Getsig100, Getsig110, Getsig120, Getsig130, Getsig140
//      INTEGER N, LIST(*), LPTR(*), LEND(N), IER
//      REAL    X(N), Y(N), H(N), HXHY(2,N), TOL, SIGMA(*),
//     .        DSMAX
//C
//C***********************************************************
//C
//C                                               From SRFPACK
//C                                            Robert J. Renka
//C                                  Dept. of Computer Science
//C                                       Univ. of North Texas
//C                                           renka@cs.unt.edu
//C                                                   07/03/98
//C
//C   Given a triangulation of a set of nodes in the plane,
//C along with data values H and gradients (HX,HY) at the
//C nodes, this subroutine determines, for each triangulation
//C arc, the smallest (nonnegative) tension factor SIGMA such
//C that the Hermite interpolatory tension spline H(T), de-
//C fined by SIGMA and the endpoint values and directional
//C derivatives, preserves local shape properties of the data.
//C In order to define the shape properties on an arc, it is
//C convenient to map the arc to an interval (T1,T2).  Then,
//C denoting the endpoint data values by H1,H2 and the deriva-
//C tives by HP1,HP2, and letting S = (H2-H1)/(T2-T1), the
//C data properties are
//C
//C       Monotonicity:  S, HP1, and HP2 are nonnegative or
//C                        nonpositive,
//C   and
//C
//C       Convexity:     HP1 .LE. S .LE. HP2  or  HP1 .GE. S
//C                        .GE. HP2.
//C
//C The corresponding properties of H are constant sign of the
//C first and second derivatives, respectively.  Note that,
//C unless HP1 = S = HP2, infinite tension is required (and H
//C is linear on the interval) if S = 0 in the case of mono-
//C tonicity, or if HP1 = S or HP2 = S in the case of
//C convexity.
//C
//C   Note that if gradients are to be computed by Subroutine
//C GRADG or function values and gradients are computed by
//C SMSURF, it may be desirable to alternate those computa-
//C tions (which require tension factors) with calls to this
//C subroutine.  This iterative procedure should terminate
//C with a call to GETSIG in order to ensure that the shape
//C properties are preserved, and convergence can be achieved
//C (at the cost of optimality) by allowing only increases in
//C tension factors (refer to the parameter descriptions for
//C SIGMA, DSMAX, and IER).
//C
//C   Refer to functions SIG0, SIG1, and SIG2 for means of
//C selecting minimum tension factors to preserve more general
//C properties.
//C
//C On input:
//C
//C       N = Number of nodes in the triangulation.  N .GE. 3.
//C
//C       X,Y = Arrays of length N containing the Cartesian
//C             coordinates of the nodes.
//C
//C       H = Array of length N containing data values at the
//C           nodes.  H(I) is associated with (X(I),Y(I)).
//C
//C       LIST,LPTR,LEND = Data structure defining the trian-
//C                        gulation.  Refer to TRIPACK
//C                        Subroutine TRMESH.
//C
//C       HXHY = Array dimensioned 2 by N whose columns con-
//C              tain partial derivatives at the nodes (X
//C              partials in the first row).  Refer to Subrou-
//C              tines GRADC, GRADG, GRADL, and SMSURF.
//C
//C       TOL = Tolerance whose magnitude determines how close
//C             each tension factor is to its optimal value
//C             when nonzero finite tension is necessary and
//C             sufficient to satisfy the constraint --
//C             abs(TOL) is an upper bound on the magnitude
//C             of the smallest (nonnegative) or largest (non-
//C             positive) value of the first or second deriva-
//C             tive of H in the interval.  Thus, the con-
//C             straint is satisfied, but possibly with more
//C             tension than necessary.
//C
//C The above parameters are not altered by this routine.
//C
//C       SIGMA = Array of length 2*NA = 6*(N-1)-2*NB, where
//C               NA and NB are the numbers of arcs and boun-
//C               dary nodes, respectively, containing minimum
//C               values of the tension factors.  The tension
//C               factors are associated with arcs in one-to-
//C               one correspondence with LIST entries.  Note
//C               that each arc N1-N2 has two LIST entries and
//C               thus, the tension factor is stored in both
//C               SIGMA(I) and SIGMA(J) where LIST(I) = N2 (in
//C               the adjacency list for N1) and LIST(J) = N1
//C               (in the list associated with N2).  SIGMA
//C               should be set to all zeros if minimal ten-
//C               sion is desired, and should be unchanged
//C               from a previous call in order to ensure con-
//C               vergence of the iterative procedure describ-
//C               ed in the header comments.
//C
//C On output:
//C
//C       SIGMA = Array containing tension factors for which
//C               H(T) preserves the local data properties on
//C               each triangulation arc, with the restriction
//C               that SIGMA(I) .LE. 85 for all I (unless the
//C               input value is larger).  The factors are as
//C               small as possible (within the tolerance) but
//C               not less than their input values.  If infin-
//C               ite tension is required on an arc, the cor-
//C               responding factor is SIGMA(I) = 85 (and H
//C               is an approximation to the linear inter-
//C               polant on the arc), and if neither property
//C               is satisfied by the data, then SIGMA(I) = 0
//C               (assuming its input value is 0), and thus H
//C               is cubic on the arc.
//C
//C       DSMAX = Maximum increase in a component of SIGMA
//C               from its input value.
//C
//C       IER = Error indicator and information flag:
//C             IER = I if no errors were encountered and I
//C                     components of SIGMA were altered from
//C                     their input values for I .GE. 0.
//C             IER = -1 if N < 3.  SIGMA is not altered in
//C                      this case.
//C             IER = -2 if duplicate nodes were encountered.
//C
//C TRIPACK modules required by GETSIG:  LSTPTR, STORE
//C
//C SRFPACK module required by GETSIG:  SNHCSH
//C
//C Intrinsic functions called by GETSIG:  ABS, EXP, MAX, MIN,
//C                                          SIGN, SQRT
//C
//C***********************************************************
//C
//      INTEGER LSTPTR
//      REAL    STORE
var
       ICNT, LP1, LP2, LPL, LUN, N1, N2, NIT, NM1: longint;
          A, C1, C2, COSHM, COSHMM, D0, D1, D1D2, D1PD2,
             D2, DMAX, DSIG, DSM, DT, DX, DY, E, EMS, EMS2,
             F, F0, FMAX, FNEG, FP, FTOL, RTOL, S, S1, S2,
             SBIG, SCM, SGN, SIG, SIGIN, SINHM, SSINH, SSM,
             STOL, T, T0, T1, T2, TM, TP1: TFloat;
//      INTEGER ICNT, LP1, LP2, LPL, LUN, N1, N2, NIT, NM1
//      REAL    A, C1, C2, COSHM, COSHMM, D0, D1, D1D2, D1PD2,
//     .        D2, DMAX, DSIG, DSM, DT, DX, DY, E, EMS, EMS2,
//     .        F, F0, FMAX, FNEG, FP, FTOL, RTOL, S, S1, S2,
//     .        SBIG, SCM, SGN, SIG, SIGIN, SINHM, SSINH, SSM,
//     .        STOL, T, T0, T1, T2, TM, TP1
  IFORTRAN: longint;
  RtolPlus : TFloat;
  N1_Temp: longint;
  GoTo8, GoTo6: Boolean;
//C
begin
  IFORTRAN := 2;
  SBIG := 85;
  LUN := -1;
//      DATA SBIG/85./,  LUN/-1/
//      integer IFORTRAN
      NM1 := N - 1;
//      NM1 = N - 1
      IF (NM1 < 2) then
      begin
  //C N < 3
  //C
        DSMAX := 0.0;
        IER := -1;
        Exit;
  //   11 DSMAX = 0.
  //      IER = -1
  //      RETURN
      end;
//      IF (NM1 .LT. 2) GO TO 11
//C
//C Compute an absolute tolerance FTOL = abs(TOL) and a
//C   relative tolerance RTOL = 100*Macheps.
//C
      FTOL := ABS(TOL);
      RTOL := 1.;
      repeat
        RTOL := RTOL/2.0;
        RtolPlus := RTOL+1.0;
      until RtolPlus <= 1.0;
      RTOL := RTOL*200.;
//      FTOL = ABS(TOL)
//      RTOL = 1.
//    1 RTOL = RTOL/2.
//        IF (STORE(RTOL+1.) .GT. 1.) GO TO 1
//      RTOL = RTOL*200.
//C
//C Print a heading.
//C
//!      IF (LUN .GE. 0) WRITE (LUN,100) N, FTOL
{$IFDEF UseSfrMessages}
      IF (LUN >= 0) then Getsig100P(IFORTRAN, N, FTOL);
{$ENDIF}
//      IF (LUN .GE. 0) CALL Getsig100(IFORTRAN, N, FTOL)
//!  100 FORMAT (///13X,'GETSIG:  N =',I4,', TOL = ',E10.3//)
//C
//C Initialize change counter ICNT and maximum change DSM for
//C   the loop on arcs.
//C
      ICNT := 0;
      DSM := 0.0;
//      ICNT = 0
//      DSM = 0.
//C
//C Loop on arcs N1-N2 for which N2 > N1.  LPL points to the
//C   last neighbor of N1.
//C
      for N1 := 1 to NM1 do
      begin
//      DO 10 N1 = 1,NM1
        LPL := LEND[N1-1];
        LP1 := LPL;
//        LPL = LEND(N1)
//        LP1 = LPL
//C
//C   Top of loop on neighbors N2 of N1.
//C
        repeat
          LP1 := LPTR[LP1-1];
          N2 := ABS(LIST[LP1-1]);
          IF (N2 <= N1) then Continue;
//    2   LP1 = LPTR(LP1)
//        N2 = ABS(LIST(LP1))
//        IF (N2 .LE. N1) GO TO 9
//C
//C Print a message and compute parameters for the arc:  DT =
//C   arc length and SIGIN = input SIGMA value.
//C
//!        IF (LUN .GE. 0) WRITE (LUN,110) N1, N2
          N1_Temp := N1;
{$IFDEF UseSfrMessages}
          IF (LUN >= 0) then Getsig110(IFORTRAN, N1_Temp, N2);
{$ENDIF}
//        IF (LUN .GE. 0) CALL Getsig110(IFORTRAN, N1, N2)
//!  110   FORMAT (/1X,'Arc',I4,' -',I4)
          DX := X[N2-1] - X[N1-1];
          DY := Y[N2-1] - Y[N1-1];
          DT := SQRT(DX*DX + DY*DY);
          IF (DT = 0.0) then
          begin
      //C Nodes N1 and N2 coincide.
      //C
            DSMAX := DSM;
            IER := -2;
            Exit;
      //   12 DSMAX = DSM
      //      IER = -2
      //      RETURN
          end;
          SIGIN := SIGMA[LP1-1];
          IF (SIGIN >= SBIG) then Continue;
//        DX = X(N2) - X(N1)
//        DY = Y(N2) - Y(N1)
//        DT = SQRT(DX*DX + DY*DY)
//        IF (DT .EQ. 0.) GO TO 12
//        SIGIN = SIGMA(LP1)
//        IF (SIGIN .GE. SBIG) GO TO 9
//C
//C Compute scaled directional derivatives S1,S2 at the end-
//C   points (for the direction N1->N2), first difference S,
//C   and second differences D1,D2.
//C
        S1 := HXHY[IWK_Index(N1-1,0)]*DX + HXHY[IWK_Index(N1-1,1)]*DY;
        S2 := HXHY[IWK_Index(N2-1,0)]*DX + HXHY[IWK_Index(N2-1,1)]*DY;
        S := H[N2-1] - H[N1-1];
        D1 := S - S1;
        D2 := S2 - S;
        D1D2 := D1*D2;
//        S1 = HXHY(1,N1)*DX + HXHY(2,N1)*DY
//        S2 = HXHY(1,N2)*DX + HXHY(2,N2)*DY
//        S = H(N2) - H(N1)
//        D1 = S - S1
//        D2 = S2 - S
//        D1D2 = D1*D2
//C
//C Test for infinite tension required to satisfy either
//C   property.
//C
        SIG := SBIG;
        IF not (((D1D2 = 0.0)  AND  (S1 <> S2))  OR
           ((S = 0.0)  AND  (S1*S2 > 0.0))) then
        begin
  //        SIG = SBIG
  //        IF ((D1D2 .EQ. 0.  .AND.  S1 .NE. S2)  .OR.
  //     .      (S .EQ. 0.  .AND.  S1*S2 .GT. 0.)) GO TO 8
  //C
  //C Test for SIGMA = 0 sufficient.  The data satisfies convex-
  //C   ity iff D1D2 .GE. 0, and D1D2 = 0 implies S1 = S = S2.
  //C
          SIG := 0.0;
          GoTo8 := False;
          IF (D1D2 >= 0.0) then
          begin
    //        SIG = 0.
    //        IF (D1D2 .LT. 0.) GO TO 4
            IF (D1D2 = 0.0) then
            begin
              GoTo8 := True;
            end
            else
            begin
      //        IF (D1D2 .EQ. 0.) GO TO 8
              T := MAX(D1/D2,D2/D1);
      //        T = MAX(D1/D2,D2/D1)
              IF (T <= 2.0) then
              begin
                GoTo8 := True;
              end
              else
              begin
        //        IF (T .LE. 2.) GO TO 8
                TP1 := T + 1.0;
        //        TP1 = T + 1.
        //C
        //C Convexity:  find a zero of F(SIG) = SIG*COSHM(SIG)/
        //C   SINHM(SIG) - TP1.
        //C
        //C   F(0) = 2-T < 0, F(TP1) .GE. 0, the derivative of F
        //C     vanishes at SIG = 0, and the second derivative of F is
        //C     .2 at SIG = 0.  A quadratic approximation is used to
        //C     obtain a starting point for the Newton method.
        //C
                SIG := SQRT(10.0*T-20.0);
                NIT := 0;
        //        SIG = SQRT(10.*T-20.)
        //        NIT = 0
        //C
        //C   Top of loop:
                repeat
        //C
                IF (SIG <= 0.5) THEN
                begin
                  SNHCSH (SIG, SINHM,COSHM,COSHMM);
                  T1 := COSHM/SINHM;
                  FP := T1 + SIG*(SIG/SINHM - T1*T1 + 1.0);
                end
                ELSE
                begin
        //    3   IF (SIG .LE. .5) THEN
        //          CALL SNHCSH (SIG, SINHM,COSHM,COSHMM)
        //          T1 = COSHM/SINHM
        //          FP = T1 + SIG*(SIG/SINHM - T1*T1 + 1.)
        //        ELSE
        //C
        //C   Scale SINHM and COSHM by 2*exp(-SIG) in order to avoid
        //C     overflow with large SIG.
        //C
                  EMS := EXP(-SIG);
                  SSM := 1.0 - EMS*(EMS+SIG+SIG);
                  T1 := (1.0-EMS)*(1.0-EMS)/SSM;
                  FP := T1 + SIG*(2.0*SIG*EMS/SSM - T1*T1 + 1.0);
                END;
        //          EMS = EXP(-SIG)
        //          SSM = 1. - EMS*(EMS+SIG+SIG)
        //          T1 = (1.-EMS)*(1.-EMS)/SSM
        //          FP = T1 + SIG*(2.*SIG*EMS/SSM - T1*T1 + 1.)
        //        ENDIF
        //C
                F := SIG*T1 - TP1;
        //        F = SIG*T1 - TP1
        //!        IF (LUN .GE. 0) WRITE (LUN,120) SIG, F, FP
{$IFDEF UseSfrMessages}
                IF (LUN >= 0) then Getsig120(IFORTRAN, SIG, F, FP);
{$ENDIF}
        //        IF (LUN .GE. 0) CALL Getsig120(IFORTRAN, SIG, F, FP)
        //!  120   FORMAT (1X,'Convexity:  SIG = ',E15.8,
        //!     .          ', F(SIG) = ',E15.8/1X,35X,'FP(SIG) = ',
        //!     .          E15.8)
                NIT := NIT + 1;
        //        NIT = NIT + 1
        //C
        //C   Test for convergence.
        //C
                IF (FP <= 0.0) then
                begin
                  GoTo8 := True;
                  break;
                end;
                DSIG := -F/FP;
        //        IF (FP .LE. 0.) GO TO 8
        //        DSIG = -F/FP
                IF ((ABS(DSIG) <= RTOL*SIG)  OR  ((F >= 0.0)  AND
                   (F <= FTOL))  OR  (ABS(F) <= RTOL)) then
                begin
                  GoTo8 := True;
                  break;
                end;
        //        IF (ABS(DSIG) .LE. RTOL*SIG  .OR.  (F .GE. 0.  .AND.
        //     .      F .LE. FTOL)  .OR.  ABS(F) .LE. RTOL) GO TO 8
        //C
        //C   Update SIG.
        //C
                SIG := SIG + DSIG;
        //        SIG = SIG + DSIG
        //        GO TO 3

                until False;
        //C
              end;
            end;
          end;
          if not GoTo8 then
          begin
    //C Convexity cannot be satisfied.  Monotonicity can be satis-
    //C   fied iff S1*S .GE. 0 and S2*S .GE. 0 since S .NE. 0.
    //C
            IF not (S1*S < 0.0)  OR  (S2*S < 0.0) then
            begin
      //    4   IF (S1*S .LT. 0.  .OR.  S2*S .LT. 0.) GO TO 8
              T0 := 3.*S - S1 - S2;
              D0 := T0*T0 - S1*S2;
      //        T0 = 3.*S - S1 - S2
      //        D0 = T0*T0 - S1*S2
      //C
      //C SIGMA = 0 is sufficient for monotonicity iff S*T0 .GE. 0
      //C   or D0 .LE. 0.
      //C
              IF not ((D0 <= 0.0)  OR  (S*T0 >= 0.0)) then
              begin
        //        IF (D0 .LE. 0.  .OR.  S*T0 .GE. 0.) GO TO 8
        //C
        //C Monotonicity:  find a zero of F(SIG) = sign(S)*HP(R),
        //C   where HPP(R) = 0 and HP, HPP denote derivatives of H.
        //C   F has a unique zero, F(0) < 0, and F approaches
        //C   abs(S) as SIG increases.
        //C
        //C   Initialize parameters for the secant method.  The method
        //C     uses three points:  (SG0,F0), (SIG,F), and
        //C     (SNEG,FNEG), where SG0 and SNEG are defined implicitly
        //C     by DSIG = SIG - SG0 and DMAX = SIG - SNEG.
        //C
                SGN := FortranSign(1.0,S);
                SIG := SBIG;
                FMAX := SGN*(SIG*S-S1-S2)/(SIG-2.0);
                IF (FMAX > 0.0) then
                begin
                  STOL := RTOL*SIG;
                  F := FMAX;
                  F0 := SGN*D0/(3.*(D1-D2));
                  FNEG := F0;
                  DSIG := SIG;
                  DMAX := SIG;
                  D1PD2 := D1 + D2;
                  NIT := 0;
          //        SGN = SIGN(1.,S)
          //        SIG = SBIG
          //        FMAX = SGN*(SIG*S-S1-S2)/(SIG-2.)
          //        IF (FMAX .LE. 0.) GO TO 8
          //        STOL = RTOL*SIG
          //        F = FMAX
          //        F0 = SGN*D0/(3.*(D1-D2))
          //        FNEG = F0
          //        DSIG = SIG
          //        DMAX = SIG
          //        D1PD2 = D1 + D2
          //        NIT = 0
          //C
          //C   Top of loop:  compute the change in SIG by linear
          //C     interpolation.
          //C
                  repeat
                    DSIG := -F*DSIG/(F-F0);
            //    5   DSIG = -F*DSIG/(F-F0)
            //!        IF (LUN .GE. 0) WRITE (LUN,130) DSIG
{$IFDEF UseSfrMessages}
                    IF (LUN >= 0) then Getsig130(IFORTRAN, DSIG);
{$ENDIF}
            //        IF (LUN .GE. 0) CALL Getsig130(IFORTRAN, DSIG)
            //!  130   FORMAT (1X,'Monotonicity:  DSIG = ',E15.8)
                    IF ( ABS(DSIG) <= ABS(DMAX))  and
                        (DSIG*DMAX <= 0.0 ) then
                    begin
              //        IF ( ABS(DSIG) .GT. ABS(DMAX)  .OR.
              //     .       DSIG*DMAX .GT. 0. ) GO TO 7
              //C
              //C   Restrict the step-size such that abs(DSIG) .GE. STOL/2.
              //C     Note that DSIG and DMAX have opposite signs.
              //C
                      IF (ABS(DSIG) < STOL/2.0) then
                      begin
                        DSIG := -FortranSign(STOL/2.0, DMAX);
                      end;
              //        IF (ABS(DSIG) .LT. STOL/2.) DSIG = -SIGN(STOL/2.,
              //     .                              DMAX)
              //C
              //C   Update SIG, F0, and F.
              //C
                      SIG := SIG + DSIG;
                      F0 := F;
              //        SIG = SIG + DSIG
              //        F0 = F
                      GoTo6 := False;
                      IF (SIG <= 0.5) THEN
                      begin
              //        IF (SIG .LE. .5) THEN
              //C
              //C   Use approximations to the hyperbolic functions designed
              //C     to avoid cancellation error with small SIG.
              //C
                        SNHCSH (SIG, SINHM,COSHM,COSHMM);
                        C1 := SIG*COSHM*D2 - SINHM*D1PD2;
                        C2 := SIG*(SINHM+SIG)*D2 - COSHM*D1PD2;
                        A := C2 - C1;
                        E := SIG*SINHM - COSHMM - COSHMM;
              //          CALL SNHCSH (SIG, SINHM,COSHM,COSHMM)
              //          C1 = SIG*COSHM*D2 - SINHM*D1PD2
              //          C2 = SIG*(SINHM+SIG)*D2 - COSHM*D1PD2
              //          A = C2 - C1
              //          E = SIG*SINHM - COSHMM - COSHMM
                      end
                      ELSE
                      begin
              //        ELSE
              //C
              //C   Scale SINHM and COSHM by 2*exp(-SIG) in order to avoid
              //C     overflow with large SIG.
              //C
                        EMS := EXP(-SIG);
                        EMS2 := EMS + EMS;
                        TM := 1.0 - EMS;
                        SSINH := TM*(1.0+EMS);
                        SSM := SSINH - SIG*EMS2;
                        SCM := TM*TM;
                        C1 := SIG*SCM*D2 - SSM*D1PD2;
                        C2 := SIG*SSINH*D2 - SCM*D1PD2;
              //          EMS = EXP(-SIG)
              //          EMS2 = EMS + EMS
              //          TM = 1. - EMS
              //          SSINH = TM*(1.+EMS)
              //          SSM = SSINH - SIG*EMS2
              //          SCM = TM*TM
              //          C1 = SIG*SCM*D2 - SSM*D1PD2
              //          C2 = SIG*SSINH*D2 - SCM*D1PD2
              //C
              //C   R is in (0,1) and well-defined iff HPP(T1)*HPP(T2) < 0.
              //C
                        F := FMAX;
                        IF (C1*(SIG*SCM*D1 - SSM*D1PD2) >= 0.0) then
                        begin
                          GoTo6 := True;
                        end
                        else
                        begin
                          A := EMS2*(SIG*TM*D2 + (TM-SIG)*D1PD2);
                          IF (A*(C2+C1) < 0.0) then
                          begin
                            GoTo6 := True;
                          end
                          else
                          begin
                            E := SIG*SSINH - SCM - SCM;
                          end;
                        end;
                      END;
                      if not GoTo6 then
                      begin

              //          F = FMAX
              //          IF (C1*(SIG*SCM*D1 - SSM*D1PD2) .GE. 0.) GO TO 6
              //          A = EMS2*(SIG*TM*D2 + (TM-SIG)*D1PD2)
              //          IF (A*(C2+C1) .LT. 0.) GO TO 6
              //          E = SIG*SSINH - SCM - SCM
              //        ENDIF
              //C
                        F := (SGN*(E*S2-C2) + SQRT(A*(C2+C1)))/E;
              //        F = (SGN*(E*S2-C2) + SQRT(A*(C2+C1)))/E
                      end;
              //C
              //C   Update the number of iterations NIT.
              //C
                      NIT := NIT + 1;
              //    6   NIT = NIT + 1
              //!        IF (LUN .GE. 0) WRITE (LUN,140) NIT, SIG, F
{$IFDEF UseSfrMessages}
                      IF (LUN >= 0) then Getsig140(IFORTRAN, NIT, SIG, F);
{$ENDIF}
              //        IF (LUN .GE. 0) CALL Getsig140(IFORTRAN, NIT, SIG, F)
              //!  140   FORMAT (1X,11X,I2,' -- SIG = ',E15.8,', F = ',
              //!     .          E15.8)
              //C
              //C   Test for convergence.
              //C
                      STOL := RTOL*SIG;
                      IF ((ABS(DMAX) <= STOL)  OR  ((F >= 0.0)  AND
                         (F <= FTOL))  OR  (ABS(F) <= RTOL)) then
                      begin
                        break;
                      end
                      else
                      begin
                //        STOL = RTOL*SIG
                //        IF (ABS(DMAX) .LE. STOL  .OR.  (F .GE. 0.  .AND.
                //     .      F .LE. FTOL)  .OR.  ABS(F) .LE. RTOL) GO TO 8
                        DMAX := DMAX + DSIG;
                //        DMAX = DMAX + DSIG
                        IF (F0*F <= 0.0)  or  (ABS(F) < ABS(F0)) then
                        begin
                //        IF (F0*F .GT. 0.  .AND.  ABS(F) .GE. ABS(F0))
                //     .     GO TO 7
                        IF (F0*F <= 0.0) THEN
                        begin
                //        IF (F0*F .LE. 0.) THEN
                //C
                //C   F and F0 have opposite signs.  Update (SNEG,FNEG) to
                //C     (SG0,F0) so that F and FNEG always have opposite
                //C     signs.  If SIG is closer to SNEG than SG0 and abs(F)
                //C     < abs(FNEG), then swap (SNEG,FNEG) with (SG0,F0).
                //C
                          T1 := DMAX;
                          T2 := FNEG;
                          DMAX := DSIG;
                          FNEG := F0;
                          IF ( ABS(DSIG) > ABS(T1))  AND
                              (ABS(F) < ABS(T2) ) THEN
                          begin
                            DSIG := T1;
                            F0 := T2;
                          END;
                        END;
                        Continue;
                //          T1 = DMAX
                //          T2 = FNEG
                //          DMAX = DSIG
                //          FNEG = F0
                //          IF ( ABS(DSIG) .GT. ABS(T1)  .AND.
                //     .         ABS(F) .LT. ABS(T2) ) THEN
                //C
                //            DSIG = T1
                //            F0 = T2
                //          ENDIF
                //        ENDIF
                //        GO TO 5
                //C
                        end;
                      end;
                    end;
              //C   Bottom of loop:  F0*F > 0 and the new estimate would
              //C     be outside of the bracketing interval of length
              //C     abs(DMAX).  Reset (SG0,F0) to (SNEG,FNEG).
              //C
                    DSIG := DMAX;
                    F0 := FNEG;
              //    7   DSIG = DMAX
              //        F0 = FNEG
              //        GO TO 5
                  until False;
                end;
              end;
            end;
          end;
        end;
  //C
  //C  Update SIGMA, ICNT, and DSM if necessary.
  //C
        SIG := MIN(SIG,SBIG);
        IF (SIG > SIGIN) THEN
        begin
          SIGMA[LP1-1] := SIG;
          LP2 := LSTPTR(LEND[N2-1],N1,LIST,LPTR);
          SIGMA[LP2-1] := SIG;
          ICNT := ICNT + 1;
          DSM := MAX(DSM,SIG-SIGIN);
        END;
//    8   SIG = MIN(SIG,SBIG)
//        IF (SIG .GT. SIGIN) THEN
//          SIGMA(LP1) = SIG
//          LP2 = LSTPTR(LEND(N2),N1,LIST,LPTR)
//          SIGMA(LP2) = SIG
//          ICNT = ICNT + 1
//          DSM = MAX(DSM,SIG-SIGIN)
//        ENDIF
//C
//C Bottom of loop on neighbors N2 of N1.
//C
//    9   IF (LP1 .NE. LPL) GO TO 2
        until LP1 = LPL;
//   10   CONTINUE
      end;
//C
//C No errors encountered.
//C
      DSMAX := DSM;
      IER := ICNT;
//      DSMAX = DSM
//      IER = ICNT
//      RETURN
//C
//C N < 3
//C
//   11 DSMAX = 0.
//      IER = -1
//      RETURN
//C
//C Nodes N1 and N2 coincide.
//C
//   12 DSMAX = DSM
//      IER = -2
//      RETURN
//      END
end;

procedure GIVENS (var A,B, C,S: TFloat);
//      SUBROUTINE GIVENS ( A,B, C,S)
//      REAL A, B, C, S
//C
//C***********************************************************
//C
//C                                               From SRFPACK
//C                                            Robert J. Renka
//C                                  Dept. of Computer Science
//C                                       Univ. of North Texas
//C                                           renka@cs.unt.edu
//C                                                   09/01/88
//C
//C   This subroutine constructs the Givens plane rotation,
//C
//C           ( C  S)
//C       G = (     ) , where C*C + S*S = 1,
//C           (-S  C)
//C
//C which zeros the second component of the vector (A,B)**T
//C (transposed).  Subroutine ROTATE may be called to apply
//C the transformation to a 2 by N matrix.
//C
//C   This routine is identical to Subroutine SROTG from the
//C LINPACK BLAS (Basic Linear Algebra Subroutines).
//C
//C On input:
//C
//C       A,B = Components of the vector defining the rota-
//C             tion.  These are overwritten by values R
//C             and Z (described below) which define C and S.
//C
//C On output:
//C
//C       A = Signed Euclidean norm R of the input vector:
//C           R = +/-SQRT(A*A + B*B)
//C
//C       B = Value Z such that:
//C             C = SQRT(1-Z*Z) and S=Z if ABS(Z) .LE. 1, and
//C             C = 1/Z and S = SQRT(1-C*C) if ABS(Z) > 1.
//C
//C       C = +/-(A/R) or 1 if R = 0.
//C
//C       S = +/-(B/R) or 0 if R = 0.
//C
//C Modules required by GIVENS:  None
//C
//C Intrinsic functions called by GIVENS:  ABS, SQRT
//C
//C***********************************************************
//C
var
       AA, BB, R, U, V: TFloat;
//      REAL AA, BB, R, U, V
//C
//C Local parameters:
//C
//C AA,BB = Local copies of A and B
//C R =     C*A + S*B = +/-SQRT(A*A+B*B)
//C U,V =   Variables used to scale A and B for computing R
//C
begin
      AA := A;
      BB := B;
//      AA = A
//      BB = B
      IF (ABS(AA) > ABS(BB)) then
      begin
//      IF (ABS(AA) .LE. ABS(BB)) GO TO 1
//C
//C ABS(A) > ABS(B).
//C
      U := AA + AA;
      V := BB/U;
      R := SQRT(0.25 + V*V) * U;
      C := AA/R;
      S := V * (C + C);
//      U = AA + AA
//      V = BB/U
//      R = SQRT(.25 + V*V) * U
//      C = AA/R
//      S = V * (C + C)
//C
//C Note that R has the sign of A, C > 0, and S has
//C   SIGN(A)*SIGN(B).
//C
      B := S;
      A := R;
//      B = S
//      A = R
//      RETURN
//C
      end
      else if BB <> 0.0 then
      begin
//C ABS(A) .LE. ABS(B).
//C
//    1 IF (BB .EQ. 0.) GO TO 2
      U := BB + BB;
      V := AA/U;
//      U = BB + BB
//      V = AA/U
//C
//C Store R in A.
//C
      A := SQRT(0.25 + V*V) * U;
      S := BB/A;
      C := V * (S + S);
//      A = SQRT(.25 + V*V) * U
//      S = BB/A
//      C = V * (S + S)
//C
//C Note that R has the sign of B, S > 0, and C has
//C   SIGN(A)*SIGN(B).
//C
      B := 1.0;
      IF (C <> 0.0) then B := 1.0/C;
//      B = 1.
//      IF (C .NE. 0.) B = 1./C
//      RETURN
      end
      else
      begin
//C
//C A = B = 0.
//C
      C := 1.0;
      S := 0.0;
//    2 C = 1.
//      S = 0.
//      RETURN
      end;
//      END
end;

procedure GRADG (const NCC: longint; const LCC: TNcmaxIntArray; const N: longint;
  const X,Y,Z: TNmaxSingleArray; const LIST,LPTR: TN6IntArray; const LEND: TNmaxIntArray;
  const IFLGS: longint; const SIGMA: TN6SingleArray; var NIT: longint;
  var DGMAX: TFloat; var GRAD: TNtnxSingleArray; var IER: longint);
//      SUBROUTINE GRADG (NCC,LCC,N,X,Y,Z,LIST,LPTR,LEND,
//     .                  IFLGS,SIGMA, NIT,DGMAX,GRAD, IER)
//      INTEGER NCC, LCC(*), N, LIST(*), LPTR(*), LEND(N),
//     .        IFLGS, NIT, IER
//      REAL    X(N), Y(N), Z(N), SIGMA(*), DGMAX, GRAD(2,N)
//C
//C***********************************************************
//C
//C                                               From SRFPACK
//C                                            Robert J. Renka
//C                                  Dept. of Computer Science
//C                                       Univ. of North Texas
//C                                           renka@cs.unt.edu
//C                                                   11/12/94
//C
//C   Given a triangulation of N nodes in the plane, along
//C with data values at the nodes and tension factors associ-
//C ated with the arcs, this subroutine employs a global
//C method to compute estimated gradients at the nodes.  The
//C method consists of minimizing a quadratic functional Q(G)
//C over vectors G of length 2N (gradient components), where
//C Q is an approximation to the linearized curvature over the
//C triangulation of a C-1 bivariate function F(X,Y) which
//C interpolates the nodal values and gradients.
//C
//C   The restriction of F to an arc of the triangulation is
//C taken to be the Hermite interpolatory tension spline
//C defined by the data values and tangential gradient compo-
//C nents at the endpoints of the arc, and Q is the sum over
//C the triangulation arcs, excluding interior constraint
//C arcs, of the linearized curvatures of F along the arcs --
//C the integrals over the arcs of D2F(T)**2, where D2F(T) is
//C the second derivative of F with respect to distance T
//C along the arc.
//C
//C   Subroutines INTRC1 and UNIF may be called to evaluate F
//C at arbitrary points.  The interpolant F is further de-
//C scribed in Subroutines FVAL and TVAL, and Q is identical
//C to the functional Q1 described in Subroutine SMSURF.
//C
//C   The minimization problem corresponds to an order 2N
//C symmetric positive definite sparse linear system which is
//C solved for the X and Y partial derivatives by the block
//C Gauss-Seidel method with N blocks of order 2.
//C
//C   If constraints are present and data values at the con-
//C straint nodes are not known, Subroutine ZGRADG, which
//C computes approximate data values at constraint nodes
//C along with the gradients, should be called in place of
//C this routine.
//C
//C   An alternative method, Subroutine GRADC or GRADL, com-
//C putes a local approximation to the partials at a single
//C node and may be more accurate, depending on the data
//C values and distribution of nodes (neither method emerged
//C as superior in tests for accuracy).  If all gradients are
//C required and a uniform tension factor SIGMA = 0 is used,
//C GRADG is significantly faster than either GRADC or GRADL.
//C
//C On input:
//C
//C       NCC = Number of constraint curves (refer to TRIPACK
//C             Subroutine ADDCST).  NCC .GE. 0.
//C
//C       LCC = Array of length NCC (or dummy array of length
//C             1 if NCC = 0) containing the index of the
//C             first node of constraint I in LCC(I).  For I =
//C             1 to NCC, LCC(I+1)-LCC(I) .GE. 3, where
//C             LCC(NCC+1) = N+1.
//C
//C       N = Number of nodes in the triangulation.  N .GE. 3.
//C
//C       X,Y = Arrays of length N containing the coordinates
//C             of the nodes with non-constraint nodes in the
//C             first LCC(1)-1 locations, followed by NCC se-
//C             quences of constraint nodes.
//C
//C       Z = Array of length N containing data values at the
//C           nodes.  Z(I) is associated with (X(I),Y(I)).
//C
//C       LIST,LPTR,LEND = Data structure defining the trian-
//C                        gulation.  Refer to TRIPACK
//C                        Subroutine TRMESH.
//C
//C       IFLGS = Tension factor option:
//C               IFLGS .LE. 0 if a single uniform tension
//C                            factor is to be used.
//C               IFLGS .GE. 1 if variable tension is desired.
//C
//C       SIGMA = Uniform tension factor (IFLGS .LE. 0), or
//C               array containing tension factors associated
//C               with arcs in one-to-one correspondence with
//C               LIST entries (IFLGS .GE. 1).  Refer to Sub-
//C               routines GETSIG, SIG0, SIG1, and SIG2.
//C
//C The above parameters are not altered by this routine.
//C
//C       NIT = Maximum number of Gauss-Seidel iterations to
//C             be employed.  This maximum will likely be
//C             achieved if DGMAX is smaller than the machine
//C             precision.  Note that complete convergence is
//C             not necessary to achieve maximum accuracy of
//C             the interpolant.  For SIGMA = 0, optimal ef-
//C             ficiency was achieved in testing with DGMAX =
//C             0, and NIT = 3 or 4.  NIT > 0.
//C
//C       DGMAX = Nonnegative convergence criterion.  The
//C               method is terminated when the maximum change
//C               in a gradient between iterations is at most
//C               DGMAX.  The change in a gradient is taken to
//C               be the Euclidean norm of the difference rel-
//C               ative to 1 plus the norm of the old value.
//C               DGMAX = 1.E-3 is sufficient for effective
//C               convergence.
//C
//C       GRAD = 2 by N array whose columns contain initial
//C              estimates of the partial derivatives.  Zero
//C              vectors are sufficient.
//C
//C On output:
//C
//C       NIT = Number of Gauss-Seidel iterations employed.
//C
//C       DGMAX = Maximum relative change in a gradient at the
//C               last iteration.
//C
//C       GRAD = Estimated X and Y partial derivatives at the
//C              nodes with X partials in the first row.  Grad
//C              is not altered if IER = -1.
//C
//C       IER = Error indicator:
//C             IER = 0 if no errors were encountered and the
//C                     convergence criterion was achieved.
//C             IER = 1 if no errors were encountered but con-
//C                     vergence was not achieved within NIT
//C                     iterations.
//C             IER = -1 if NCC, an LCC entry, N, NIT, or
//C                      DGMAX is outside its valid range on
//C                      input.
//C             IER = -2 if all nodes are collinear or the
//C                      triangulation data structure is in-
//C                      valid.
//C             IER = -3 if duplicate nodes were encountered.
//C
//C SRFPACK modules required by GRADG:  GRCOEF, SNHCSH
//C
//C Intrinsic functions called by GRADG:  ABS, MAX, SQRT
//C
//C***********************************************************
//C
var
       I, IFL, IFRST, ILAST, ITER, J, K, KBAK, KFOR,
            LCC1, LP, LPJ, LPL, MAXIT, NB, NN: longint;
        A11, A12, A22, D, DCUB, DELX, DELXS, DELY,
            DELYS, DET, DF, DGMX, DSQ, DZX, DZY, R1, R2,
            SDF, SIG, T, TOL, XK, YK, ZK, ZXK, ZYK: TFloat;
  GoTo5: Boolean;
  GoTo6: Boolean;
//      INTEGER I, IFL, IFRST, ILAST, ITER, J, K, KBAK, KFOR,
//     .        LCC1, LP, LPJ, LPL, MAXIT, NB, NN
//      REAL    A11, A12, A22, D, DCUB, DELX, DELXS, DELY,
//     .        DELYS, DET, DF, DGMX, DSQ, DZX, DZY, R1, R2,
//     .        SDF, SIG, T, TOL, XK, YK, ZK, ZXK, ZYK
begin
//C
      NN := N;
      IFL := IFLGS;
      MAXIT := NIT;
      TOL := DGMAX;
//      NN = N
//      IFL = IFLGS
//      MAXIT = NIT
//      TOL = DGMAX
//C
//C Test for errors in input parameters.
//C
      IF (NCC < 0)  OR  (MAXIT < 1)  OR  (TOL < 0.0) then
      begin
        NIT := 0;
        DGMAX := 0.0;
        IER := -1;
        Exit;
//    9 NIT = 0
//      DGMAX = 0.
//      IER = -1
//      RETURN
      end;
//      IF (NCC .LT. 0  .OR.  MAXIT .LT. 1  .OR.  TOL .LT. 0.)
//     .  GO TO 9
      LCC1 := NN+1;
      IF (NCC = 0) THEN
      begin
        IF (NN < 3) then
        begin
          NIT := 0;
          DGMAX := 0.0;
          IER := -1;
          Exit;
  //    9 NIT = 0
  //      DGMAX = 0.
  //      IER = -1
  //      RETURN
        end;
      end
      ELSE
      begin
        for I := NCC downto 1 do
        begin
          IF (LCC1-LCC[I-1] < 3) then
          begin
            NIT := 0;
            DGMAX := 0.0;
            IER := -1;
            Exit;
    //    9 NIT = 0
    //      DGMAX = 0.
    //      IER = -1
    //      RETURN
          end;
          LCC1 := LCC[I-1];
        end;
        IF (LCC1 < 1) then
        begin
          NIT := 0;
          DGMAX := 0.0;
          IER := -1;
          Exit;
  //    9 NIT = 0
  //      DGMAX = 0.
  //      IER = -1
  //      RETURN
        end;
      END;
//      LCC1 = NN+1
//      IF (NCC .EQ. 0) THEN
//        IF (NN .LT. 3) GO TO 9
//      ELSE
//        DO 1 I = NCC,1,-1
//          IF (LCC1-LCC(I) .LT. 3) GO TO 9
//          LCC1 = LCC(I)
//    1     CONTINUE
//        IF (LCC1 .LT. 1) GO TO 9
//      ENDIF
//C
//C Initialize iteration count and SIG (overwritten if
//C   IFLGS > 0).
//C
      ITER := 0;
      SIG := SIGMA[0];
//      ITER = 0
//      SIG = SIGMA(1)
//C
//C Top of iteration loop:  If K is a constraint node, I
//C   indexes the constraint containing node K, IFRST and
//C   ILAST are the first and last nodes of constraint I,
//C   and (KBAK,K,KFOR) is a subsequence of constraint I.
//C
      repeat
        IF (ITER = MAXIT) then
        begin
          DGMAX := 0;
          IER := 1;
          Exit;
  //    8 DGMAX = DGMX
  //      IER = 1
  //      RETURN
        end;
        DGMX := 0.0;
        I := 0;
        IFRST := 1;
        ILAST := LCC1-1;
        KBAK := 0;
        KFOR := 0;
//    2 IF (ITER .EQ. MAXIT) GO TO 8
//      DGMX = 0.
//      I = 0
//      IFRST = 1
//      ILAST = LCC1-1
//      KBAK = 0
//      KFOR = 0
//C
//C Loop on nodes.
//C
        for K := 1 to NN do
        begin
//      DO 7 K = 1,NN
          IF (K >= LCC1) THEN
          begin
            IF (K > ILAST) THEN
            begin
              I := I + 1;
              IFRST := K;
              IF (I < NCC) THEN
              begin
                ILAST := LCC[I] - 1;
              end
              ELSE
              begin
                ILAST := NN;
              END;
              KBAK := ILAST;
              KFOR := K + 1;
            end
            ELSE
            begin
              KBAK := K - 1;
              IF (K < ILAST) THEN
              begin
                KFOR := K + 1;
              end
              ELSE
              begin
                KFOR := IFRST;
              END;
            END;
          END;
//        IF (K .GE. LCC1) THEN
//          IF (K .GT. ILAST) THEN
//            I = I + 1
//            IFRST = K
//            IF (I .LT. NCC) THEN
//              ILAST = LCC(I+1) - 1
//            ELSE
//              ILAST = NN
//            ENDIF
//            KBAK = ILAST
//            KFOR = K + 1
//          ELSE
//            KBAK = K - 1
//            IF (K .LT. ILAST) THEN
//              KFOR = K + 1
//            ELSE
//              KFOR = IFRST
//            ENDIF
//          ENDIF
//        ENDIF
        XK := X[K-1];
        YK := Y[K-1];
        ZK := Z[K-1];
//        XK = X(K)
//        YK = Y(K)
//        ZK = Z(K)
        ZXK := GRAD[IWK_Index(K-1,0)];
        ZYK := GRAD[IWK_Index(K-1,1)];
//        ZXK = GRAD(1,K)
//        ZYK = GRAD(2,K)
//C
//C   Initialize components of the order 2 system for the
//C     change (DZX,DZY) in the K-th solution components
//C     (symmetric matrix in A and residual in R).
//C
        A11 := 0.0;
        A12 := 0.0;
        A22 := 0.0;
        R1 := 0.0;
        R2 := 0.0;
//        A11 = 0.
//        A12 = 0.
//        A22 = 0.
//        R1 = 0.
//        R2 = 0.
//C
//C   Loop on neighbors J of node K.
//C
        LPL := LEND[K-1];
        LPJ := LPL;
//        LPL = LEND(K)
//        LPJ = LPL
        repeat
          LPJ := LPTR[LPJ-1];
          J := ABS(LIST[LPJ-1]);
//    3   LPJ = LPTR(LPJ)
//          J = ABS(LIST(LPJ))
//C
//C   Arc K-J lies in a constraint region and is bypassed iff
//C     K and J are nodes in the same constraint and J follows
//C     KFOR and precedes KBAK as a neighbor of K.
//C
          GoTo5 := False;
          IF (K < LCC1)  OR  (J < IFRST)  OR
             (J > ILAST) then
          begin
            GoTo5 := True;
          end;
          IF (J = KBAK) OR (J = KFOR) then
          begin
            GoTo5 := True;
          end;
//          IF (K .LT. LCC1  .OR.  J .LT. IFRST  .OR.
//     .        J .GT. ILAST) GO TO 5
//          IF (J .EQ. KBAK  .OR.  J .EQ. KFOR) GO TO 5
          GoTo6 := False;
          if not GoTo5 then
          begin
            LP := LPJ;
//          LP = LPJ
//C
            repeat
              LP := LPTR[LP-1];
              NB := ABS(LIST[LP-1]);
              IF (NB = KBAK) then
              begin
                GoTo6 := True;
                break;
              end;
//    4     LP = LPTR(LP)
//            NB = ABS(LIST(LP))
//            IF (NB .EQ. KBAK) GO TO 6
//            IF (NB .NE. KFOR) GO TO 4
            until (NB = KFOR);
          end;
          if GoTo6 then
          begin
            Continue;
          end;
//C
//C   Compute parameters associated with edge
//C     K->J, and test for duplicate nodes.
//C
          DELX := X[J-1] - XK;
          DELY := Y[J-1] - YK;
          DELXS := DELX*DELX;
          DELYS := DELY*DELY;
          DSQ := DELXS + DELYS;
          D := SQRT(DSQ);
          DCUB := D*DSQ;
          IF (D = 0.0) then
          begin
//      C Nodes K and J coincide.
//      C
            NIT := 0;
            DGMAX := DGMX;
            IER := -3;
            Exit;
      //C Nodes K and J coincide.
      //C
      //   11 NIT = 0
      //      DGMAX = DGMX
      //      IER = -3
      //      RETURN
          end;
          IF (IFL >= 1) then SIG := SIGMA[LPJ-1];
          GRCOEF (SIG,DCUB, DF,SDF);
//    5     DELX = X(J) - XK
//          DELY = Y(J) - YK
//          DELXS = DELX*DELX
//          DELYS = DELY*DELY
//          DSQ = DELXS + DELYS
//          D = SQRT(DSQ)
//          DCUB = D*DSQ
//          IF (D .EQ. 0.) GO TO 11
//          IF (IFL .GE. 1) SIG = SIGMA(LPJ)
//          CALL GRCOEF (SIG,DCUB, DF,SDF)
//C
//C   Update the system components for node J.  The contribu-
//C     tion from edge K->J is weighted by 1/D, where D is
//C     the arc length.
//C
          A11 := A11 + DF*DELXS/D;
          A12 := A12 + DF*DELX*DELY/D;
          A22 := A22 + DF*DELYS/D;
          T := ((DF+SDF)*(Z[J-1]-ZK) - DF*(ZXK*DELX + ZYK*DELY)
               - SDF*(GRAD[IWK_Index(J-1,0)]*DELX + GRAD[IWK_Index(J-1,1)]*DELY))/D;
          R1 := R1 + T*DELX;
          R2 := R2 + T*DELY;
//          A11 = A11 + DF*DELXS/D
//          A12 = A12 + DF*DELX*DELY/D
//          A22 = A22 + DF*DELYS/D
//          T = ((DF+SDF)*(Z(J)-ZK) - DF*(ZXK*DELX + ZYK*DELY)
//     .          - SDF*(GRAD(1,J)*DELX + GRAD(2,J)*DELY))/D
//          R1 = R1 + T*DELX
//          R2 = R2 + T*DELY
//C
//C   Bottom of loop on neighbors.
//C
//    6     IF (LPJ .NE. LPL) GO TO 3
        until (LPJ = LPL);
//C
//C   Solve the system associated with the K-th block.
//C
        DET := A11*A22 - A12*A12;
        IF (DET = 0.0)  OR  (A11 = 0.0) then
        begin
//    C Node K and its neighbors are collinear, resulting in a
//    C   singular system.
//    C
          NIT := 0;
          DGMAX := DGMX;
          IER := -2;
          Exit;
    //C Node K and its neighbors are collinear, resulting in a
    //C   singular system.
    //C
    //   10 NIT = 0
    //      DGMAX = DGMX
    //      IER = -2
    //      RETURN
        end;
        DZY := (A11*R2 - A12*R1)/DET;
        DZX := (R1 - A12*DZY)/A11;
//        DET = A11*A22 - A12*A12
//        IF (DET .EQ. 0.  .OR.  A11 .EQ. 0.) GO TO 10
//        DZY = (A11*R2 - A12*R1)/DET
//        DZX = (R1 - A12*DZY)/A11
//C
//C   Update the partials at node K and the maximum relative
//C     change DGMX.
//C
        GRAD[IWK_Index(K-1,0)] := ZXK + DZX;
        GRAD[IWK_Index(K-1,1)] := ZYK + DZY;
        DGMX := MAX(DGMX,SQRT(DZX*DZX+DZY*DZY)/
                  (1.0+SQRT(ZXK*ZXK+ZYK*ZYK)));
//        GRAD(1,K) = ZXK + DZX
//        GRAD(2,K) = ZYK + DZY
//        DGMX = MAX(DGMX,SQRT(DZX*DZX+DZY*DZY)/
//     .             (1.+SQRT(ZXK*ZXK+ZYK*ZYK)))
//    7   CONTINUE
        end;
//C
//C   Increment ITER and test for convergence.
//C
//      ITER = ITER + 1
//      IF (DGMX .GT. TOL) GO TO 2
      until (DGMX <= TOL) or IsNan(DGMX);
      if IsNan(DGMX) then
      begin
        raise ESfrProcedureException.Create('Error determing gradients. Check that values are not too extreme.');
      end;
//C
//C Method converged.
//C
//      NIT = ITER
//      DGMAX = DGMX
//      IER = 0
//      RETURN
//C
//C Method failed to converge within NIT iterations.
//C
//    8 DGMAX = DGMX
//      IER = 1
//      RETURN
//C
//C Invalid input parameter.
//C
//    9 NIT = 0
//      DGMAX = 0.
//      IER = -1
//      RETURN
//C
//C Node K and its neighbors are collinear, resulting in a
//C   singular system.
//C
//   10 NIT = 0
//      DGMAX = DGMX
//      IER = -2
//      RETURN
//C
//C Nodes K and J coincide.
//C
//   11 NIT = 0
//      DGMAX = DGMX
//      IER = -3
//      RETURN
//      END
end;

procedure GRADL (const K,NCC: longint; const LCC: TNcmaxIntArray; const N: longint;
  const X,Y,Z: TNmaxSingleArray; const LIST,LPTR: TN6IntArray; const LEND: TNmaxIntArray;
  var DX,DY: TFloat; var IER: longint);
//      SUBROUTINE GRADL (K,NCC,LCC,N,X,Y,Z,LIST,LPTR,
//     .                  LEND, DX,DY,IER)
//      INTEGER K, NCC, LCC(*), N, LIST(*), LPTR(*),
//     .        LEND(N), IER
//      REAL    X(N), Y(N), Z(N), DX, DY
//C
//C***********************************************************
//C
//C                                               From SRFPACK
//C                                            Robert J. Renka
//C                                  Dept. of Computer Science
//C                                       Univ. of North Texas
//C                                           renka@cs.unt.edu
//C                                                   02/22/97
//C
//C   Given a Delaunay triangulation of N points in the plane
//C with associated data values Z, this subroutine estimates
//C X and Y partial derivatives at node K.  The derivatives
//C are taken to be the partials at K of a quadratic function
//C which interpolates Z(K) and fits the data values at a set
//C of nearby nodes in a weighted least squares sense. A Mar-
//C quardt stabilization factor is used if necessary to ensure
//C a well-conditioned system.  Thus, a unique solution exists
//C if there are at least 6 noncollinear nodes.
//C
//C   The triangulation may include constraints introduced by
//C Subroutine ADDCST, in which case the gradient estimates
//C are influenced by the nonconvex geometry of the domain.
//C Refer to Subroutine GETNP.  If data values at the con-
//C straint nodes are not known, Subroutine ZGRADL, which
//C computes approximate data values at constraint nodes along
//C with gradients, should be called in place of this routine.
//C
//C   Subroutine GRADC uses a cubic polynomial instead of the
//C quadratic and is generally more accurate than this routine
//C if the nodal distribution is sufficiently dense.  Another
//C alternative routine, GRADG, employs a global method to
//C compute the partial derivatives at all of the nodes at
//C once.  That method is usually more efficient (when all
//C partials are needed) and may be more accurate, depending
//C on the data.
//C
//C On input:
//C
//C       K = Index of the node at which derivatives are to be
//C           estimated.  1 .LE. K .LE. N.
//C
//C       NCC = Number of constraint curves (refer to TRIPACK
//C             Subroutine ADDCST).  NCC .GE. 0.
//C
//C       LCC = Array of length NCC (or dummy array of length
//C             1 if NCC = 0) containing the index of the
//C             first node of constraint I in LCC(I).  For I =
//C             1 to NCC, LCC(I+1)-LCC(I) .GE. 3, where
//C             LCC(NCC+1) = N+1.
//C
//C       N = Number of nodes in the triangulation.  N .GE. 6.
//C
//C       X,Y = Arrays of length N containing the coordinates
//C             of the nodes with non-constraint nodes in the
//C             first LCC(1)-1 locations, followed by NCC se-
//C             quences of constraint nodes.
//C
//C       Z = Array of length N containing data values associ-
//C           ated with the nodes.
//C
//C       LIST,LPTR,LEND = Data structure defining the trian-
//C                        gulation.  Refer to TRIPACK
//C                        Subroutine TRMESH.
//C
//C Input parameters are not altered by this routine.
//C
//C On output:
//C
//C       DX,DY = Estimated partial derivatives at node K
//C               unless IER < 0.
//C
//C       IER = Error indicator:
//C             IER = L > 0 if no errors were encountered and
//C                         L nodes (including node K) were
//C                         employed in the least squares fit.
//C             IER = -1 if K, NCC, an LCC entry, or N is
//C                      outside its valid range on input.
//C             IER = -2 if all nodes are collinear.
//C
//C TRIPACK modules required by GRADL:  GETNP, INTSEC
//C
//C SRFPACK modules required by GRADL:  GIVENS, ROTATE, SETRO1
//C
//C Intrinsic functions called by GRADL:  ABS, MIN, REAL, SQRT
//C
//C***********************************************************
//C
const
      LMN: longint =10;
      LMX =30;
var
         I, IERR, J, JP1, KK, L, LMAX, LMIN, LM1,
               LNP, NP: longint;
             C, DMIN, DS, {DTOL,} RIN,
               RS, {RTOL,} S, SF, SFS, STF, SUM, W, XK, YK,
               ZK: TFloat;
   A: array[0..5, 0..5] of TFloat;
   NPTS: TLwkIntArray;
   DIST: TNmaxSingleArray;
  LNP_Temp: Integer;
  GoTo4: Boolean;
  GoTo12: Boolean;
const
            RTOL: TFloat = 1.0E-5;
            DTOL: TFloat = 0.01;
//      INTEGER   LMN, LMX
//      PARAMETER (LMN=10,  LMX=30)
//      INTEGER   I, IERR, J, JP1, KK, L, LMAX, LMIN, LM1,
//     .          LNP, NP, NPTS(LMX)
//      REAL      A(6,6), C, DIST(LMX), DMIN, DS, DTOL, RIN,
//     .          RS, RTOL, S, SF, SFS, STF, SUM, W, XK, YK,
//     .          ZK
//      DATA      RTOL/1.E-5/, DTOL/.01/
//C
//C Local parameters:
//C
//C A =         Transpose of the augmented regression matrix
//C C =         First component of the plane rotation deter-
//C               mined by Subroutine GIVENS
//C DIST =      Array containing the distances between K and
//C               the elements of NPTS (refer to GETNP)
//C DMIN =      Minimum of the magnitudes of the diagonal
//C               elements of the regression matrix after
//C               zeros are introduced below the diagonal
//C DS =        Squared distance between nodes K and NPTS(LNP)
//C DTOL =      Tolerance for detecting an ill-conditioned
//C               system.  The system is accepted when DMIN/W
//C               .GE. DTOL
//C I =         DO-loop index
//C IERR =      Error flag for calls to GETNP
//C J =         DO-loop index
//C JP1 =       J+1
//C KK =        Local copy of K
//C L =         Number of columns of A**T to which a rotation
//C               is applied
//C LMAX,LMIN = Min(LMX,N), Min(LMN,N)
//C LMN,LMX =   Minimum and maximum values of LNP for N
//C               sufficiently large.  In most cases LMN-1
//C               nodes are used in the fit.  4 .LE. LMN .LE.
//C               LMX.
//C LM1 =       LMIN-1 or LNP-1
//C LNP =       Length of NPTS
//C NP =        Element of NPTS to be added to the system
//C NPTS =      Array containing the indexes of a sequence of
//C               nodes ordered by distance from K.  NPTS(1)=K
//C               and the first LNP-1 elements of NPTS are
//C               used in the least squares fit.  Unless LNP
//C               exceeds LMAX, NPTS(LNP) determines R.
//C RIN =       Inverse of the distance R between node K and
//C               NPTS(LNP) or some point further from K than
//C               NPTS(LMAX) if NPTS(LMAX) is used in the fit.
//C               R is a radius of influence which enters into
//C               the weight W.
//C RS =        R*R
//C RTOL =      Tolerance for determining R.  If the relative
//C               change in DS between two elements of NPTS is
//C               not greater than RTOL, they are treated as
//C               being the same distance from node K
//C S =         Second component of the plane rotation deter-
//C               mined by Subroutine GIVENS
//C SF =        Scale factor for the linear terms (columns 4
//C               and 5) in the least squares fit -- inverse
//C               of the root-mean-square distance between K
//C               and the nodes (other than K) in the least
//C               squares fit.
//C SFS =       Scale factor for the quadratic terms (first 3
//C               columns) in the least squares fit -- SF*SF.
//C STF =       Marquardt stabilization factor used to damp
//C               out the first 3 solution components (second
//C               partials of the quadratic) when the system
//C               is ill-conditioned.  As STF increases, the
//C               fitting function approaches a linear
//C SUM =       Sum of squared distances between node K and
//C               the nodes used in the least squares fit
//C W =         Weight associated with a row of the augmented
//C               regression matrix -- 1/R - 1/D, where D < R
//C               and D is the distance between K and a node
//C               entering into the least squares fit.
//C XK,YK,ZK =  Coordinates and data value associated with K
begin
  SetLength(NPTS, LMX);
  SetLength(DIST, LMX);
//C
      KK := K;
//      KK = K
//C
//C Test for errors and initialize LMIN and LMAX.
//C
      IF (KK < 1)  OR  (KK > N)  OR  (NCC < 0)
         OR  (N < 6) then
      begin
  //C Invalid input parameter.
  //C
        IER := -1;
        Exit;
  //   13 IER = -1
  //      RETURN
      end;
//      IF (KK .LT. 1  .OR.  KK .GT. N  .OR.  NCC .LT. 0
//     .    .OR.  N .LT. 6) GO TO 13
      LMIN := MIN(LMN,N);
      LMAX := MIN(LMX,N);
//      LMIN = MIN(LMN,N)
//      LMAX = MIN(LMX,N)
//C
//C Compute NPTS, DIST, LNP, SF, SFS, and RIN --
//C
//C   Set NPTS to the closest LMIN-1 nodes to K.
//C
      SUM := 0.0;
      NPTS[0] := KK;
      DIST[0] := 0.0;
      LM1 := LMIN - 1;
//      SUM = 0.
//      NPTS(1) = KK
//      DIST(1) = 0.
//      LM1 = LMIN - 1
      for LNP := 2 to LM1 do
      begin
//      DO 1 LNP = 2,LM1
        LNP_Temp := LNP;
        GETNP (NCC,LCC,N,X,Y,LIST,LPTR,LEND,
                   LNP_Temp, NPTS,DIST, IERR);
        IF (IERR <> 0) then
        begin
    //C Invalid input parameter.
    //C
          IER := -1;
          Exit;
    //   13 IER = -1
    //      RETURN
        end;
        DS := Sqr(DIST[LNP-1]);
        SUM := SUM + DS;
//        CALL GETNP (NCC,LCC,N,X,Y,LIST,LPTR,LEND,
//     .              LNP, NPTS,DIST, IERR)
//        IF (IERR .NE. 0) GO TO 13
//        DS = DIST(LNP)**2
//        SUM = SUM + DS
//    1   CONTINUE
      end;
//C
//C Add additional nodes to NPTS until the relative increase
//C   in DS is at least RTOL.
//C
      GoTo4 := False;
      for LNP := LMIN to LMAX do
      begin
        LNP_Temp := LNP;
        GETNP (NCC,LCC,N,X,Y,LIST,LPTR,LEND,
                   LNP_Temp, NPTS,DIST, IERR);
        RS := Sqr(DIST[LNP-1]);
        IF ((RS-DS)/DS > RTOL) then
        begin
          IF (LNP > 6) then
          begin
            GoTo4 := True;
            break;
          end;
        end;
        SUM := SUM + RS;
      end;
//      DO 3 LNP = LMIN,LMAX
//        CALL GETNP (NCC,LCC,N,X,Y,LIST,LPTR,LEND,
//     .              LNP, NPTS,DIST, IERR)
//        RS = DIST(LNP)**2
//        IF ((RS-DS)/DS .LE. RTOL) GO TO 2
//        IF (LNP .GT. 6) GO TO 4
//    2   SUM = SUM + RS
//    3   CONTINUE
//C
//C Use all LMAX nodes in the least squares fit.  RS is
//C   arbitrarily increased by 10 per cent.
      if not GoTo4 then
      begin
        RS := 1.1*RS;
        LNP := LMAX + 1;
//C
//      RS = 1.1*RS
//      LNP = LMAX + 1
      end;
//C
//C There are LNP-2 equations corresponding to nodes NPTS(2),
//C   ...,NPTS(LNP-1).
//C
      SFS := (LNP-2)/SUM;
      SF := SQRT(SFS);
      RIN := 1.0/SQRT(RS);
      XK := X[KK-1];
      YK := Y[KK-1];
      ZK := Z[KK-1];
//    4 SFS = REAL(LNP-2)/SUM
//      SF = SQRT(SFS)
//      RIN = 1./SQRT(RS)
//      XK = X(KK)
//      YK = Y(KK)
//      ZK = Z(KK)
//C
//C A Q-R decomposition is used to solve the least squares
//C   system.  The transpose of the augmented regression
//C   matrix is stored in A with columns (rows of A) defined
//C   as follows:  1-3 are the quadratic terms, 4 and 5 are
//C   the linear terms with coefficients DX and DY, and the
//C   last column is the right hand side.
//C
//C Set up the first 5 equations and zero out the lower tri-
//C   angle with Givens rotations.
//C
      for I := 1 to 5 do
      begin
//      DO 6 I = 1,5
        NP := NPTS[I];
        W := 1.0/DIST[I] - RIN;
        SETRO1 (XK,YK,ZK,X[NP-1],Y[NP-1],Z[NP-1],SF,SFS,
                    W, A[I-1]);
//        SETRO1_ext (XK,YK,ZK,X[NP-1],Y[NP-1],Z[NP-1],SF,SFS,
//                    W, A[I-1,0]);
//        NP = NPTS(I+1)
//        W = 1./DIST(I+1) - RIN
//        CALL SETRO1 (XK,YK,ZK,X(NP),Y(NP),Z(NP),SF,SFS,
//     .               W, A(1,I))
//        IF (I .EQ. 1) GO TO 6
        IF (I <> 1) then
        begin
          for J := 1 to I - 1 do
          begin
//        DO 5 J = 1,I-1
            JP1 := J + 1;
            L := 6 - J;
            GIVENS (A[J-1,J-1],A[I-1, J-1],C,S);
            ROTATE (L,C,S,A[J-1],A[I-1], JP1-1, JP1-1);
//            ROTATE_ext (L,C,S,A[J-1,JP1-1],A[I-1,JP1-1]);
//          JP1 = J + 1
//          L = 6 - J
//          CALL GIVENS (A(J,J),A(J,I),C,S)
//          CALL ROTATE (L,C,S,A(JP1,J),A(JP1,I))
          end;
//    5     CONTINUE
        end;
//    6   CONTINUE
      end;
//C
//C Add the additional equations to the system using
//C   the last column of A.  I .LE. LNP.
//C
      I := 7;
//      I = 7
      GoTo12 := False;
      repeat
        while I < LNP do
        begin
  //    7   IF (I .LT. LNP) THEN
            NP := NPTS[I-1];
            W := 1.0/DIST[I-1] - RIN;
            SETRO1 (XK,YK,ZK,X[NP-1],Y[NP-1],Z[NP-1],SF,SFS,
                        W, A[5]);
//            SETRO1_ext (XK,YK,ZK,X[NP-1],Y[NP-1],Z[NP-1],SF,SFS,
//                        W, A[5,0]);
  //          NP = NPTS(I)
  //          W = 1./DIST(I) - RIN
  //          CALL SETRO1 (XK,YK,ZK,X(NP),Y(NP),Z(NP),SF,SFS,
  //     .                 W, A(1,6))
            for J := 1 to 6 do
            begin
              JP1 := J + 1;
              L := 6 - J;
              GIVENS (A[J-1,J-1],A[5,J-1],C,S);
              ROTATE (L,C,S,A[J-1],A[5],JP1-1,JP1-1);
//              ROTATE_ext (L,C,S,A[J-1,JP1-1],A[5,JP1-1]);
            end;
  //          DO 8 J = 1,5
  //            JP1 = J + 1
  //            L = 6 - J
  //            CALL GIVENS (A(J,J),A(J,6),C,S)
  //            CALL ROTATE (L,C,S,A(JP1,J),A(JP1,6))
  //    8       CONTINUE
            I := I + 1;
  //          I = I + 1
  //          GO TO 7
  //        ENDIF
        end;
  //C
  //C Test the system for ill-conditioning.
  //C
        DMIN := MIN( ABS(A[0,0]),ABS(A[1,1]));
        DMIN := MIN( DMIN,ABS(A[2,2]));
        DMIN := MIN( DMIN,ABS(A[3,3]));
        DMIN := MIN( DMIN,ABS(A[4,4]));
  //      DMIN = MIN( ABS(A(1,1)),ABS(A(2,2)),ABS(A(3,3)),
  //     .            ABS(A(4,4)),ABS(A(5,5)) )
        GoTo12 := False;
        IF (DMIN/W >= DTOL) then
        begin
          GoTo12 := True;
          break;
        end;
  //      IF (DMIN/W .GE. DTOL) GO TO 12
        IF (LNP <= LMAX) THEN
        begin
  //      IF (LNP .LE. LMAX) THEN
  //C
  //C   Add another node to the system and increase R.  Note
  //C     that I = LNP.
  //C
          LNP := LNP + 1;
          IF (LNP <= LMAX) THEN
          begin
            GETNP (NCC,LCC,N,X,Y,LIST,LPTR,LEND,
                       LNP, NPTS,DIST, IERR);
            RS := Sqr(DIST[LNP-1]);
          END;
          RIN := 1.0/SQRT(1.1*RS);
          Continue;
        END;
  //        LNP = LNP + 1
  //        IF (LNP .LE. LMAX) THEN
  //          CALL GETNP (NCC,LCC,N,X,Y,LIST,LPTR,LEND,
  //     .                LNP, NPTS,DIST, IERR)
  //          RS = DIST(LNP)**2
  //        ENDIF
  //        RIN = 1./SQRT(1.1*RS)
  //        GO TO 7
  //      ENDIF
        break;
      until False;
      if not GoTo12 then
      begin

//C
//C Stabilize the system by damping second partials -- add
//C   multiples of the first three unit vectors to the first
//C   three equations.
//C
      STF := W;
//      STF = W
      for I := 1 to 3 do
      begin
//      DO 11 I = 1,3
        A[5,I-1] := STF;
//        A(I,6) = STF
        for J := I+1 to 6 do
        begin
//        DO 9 J = I+1,6
          A[5,J-1] := 0.0;
//          A(J,6) = 0.
        end;
//    9     CONTINUE
        for J := 1 to 5 do
        begin
//        DO 10 J = I,5
          JP1 := J + 1;
          L := 6 - J;
          GIVENS (A[J-1,J-1],A[5,J-1],C,S);
          ROTATE (L,C,S,A[J-1],A[5],JP1-1,JP1-1);
//          ROTATE_ext (L,C,S,A[J-1,JP1-1],A[5,JP1-1]);
//          JP1 = J + 1
//          L = 6 - J
//          CALL GIVENS (A(J,J),A(J,6),C,S)
//          CALL ROTATE (L,C,S,A(JP1,J),A(JP1,6))
        end;
//   10     CONTINUE
//   11   CONTINUE
      end;
//C
//C Test the damped system for ill-conditioning.
//C
      DMIN := MIN( ABS(A[3,3]),ABS(A[4,4]) );
      IF (DMIN/W < DTOL) then
      begin
//C No unique solution due to collinear nodes.
//C
        IER := -2;
        Exit;
  //   14 IER = -2
  //      RETURN
      end;
//      DMIN = MIN( ABS(A(4,4)),ABS(A(5,5)) )
//      IF (DMIN/W .LT. DTOL) GO TO 14
//C
      end;
//C Solve the 2 by 2 triangular system for the partial
//C   derivatives.
//C
      DY := A[4,5]/A[4,4];
      DX := SF*(A[3,5] - A[3,4]*DY)/A[3,3];
      DY := SF*DY;
      IER := LNP - 1;
//   12 DY = A(6,5)/A(5,5)
//      DX = SF*(A(6,4) - A(5,4)*DY)/A(4,4)
//      DY = SF*DY
//      IER = LNP - 1
//      RETURN
//C
//C Invalid input parameter.
//C
//   13 IER = -1
//      RETURN
//C
//C No unique solution due to collinear nodes.
//C
//   14 IER = -2
//      RETURN
//      END
end;

procedure GRCOEF (const SIGMA,DCUB: TFloat; var D,SD: TFloat);
//      SUBROUTINE GRCOEF (SIGMA,DCUB, D,SD)
//      REAL SIGMA, DCUB, D, SD
//C
//C***********************************************************
//C
//C                                               From SRFPACK
//C                                            Robert J. Renka
//C                                  Dept. of Computer Science
//C                                       Univ. of North Texas
//C                                           renka@cs.unt.edu
//C                                                   11/18/96
//C
//C   This subroutine computes factors involved in the linear
//C system solved by Subroutines GRADG and SMSGS.
//C
//C On input:
//C
//C       SIGMA = Nonnegative tension factor associated with a
//C               triangulation arc.
//C
//C       DCUB = Cube of the positive arc length.
//C
//C Input parameters are not altered by this routine.
//C
//C On output:
//C
//C       D = Diagonal factor.  D = SIG*(SIG*COSHM(SIG) -
//C           SINHM(SIG))/(E*DCUB), where E = SIG*SINH(SIG) -
//C           2*COSHM(SIG).  D > 0.
//C
//C       SD = Off-diagonal factor.  SD = SIG*SINHM(SIG)/
//C            (E*DCUB).  SD > 0.
//C
//C SRFPACK module required by GRCOEF:  SNHCSH
//C
//C Intrinsic function called by GRCOEF:  EXP
//C
//C***********************************************************
//C
var
       COSHM, COSHMM, E, EMS, SCM, SIG, SINHM, SSINH,
          SSM: TFloat;
//      REAL COSHM, COSHMM, E, EMS, SCM, SIG, SINHM, SSINH,
//     .     SSM
//C
begin
      SIG := SIGMA;
      IF (SIG < 1.0E-9) THEN
      begin
//      SIG = SIGMA
//      IF (SIG .LT. 1.E-9) THEN
//C
//C SIG = 0:  cubic interpolant.
//C
        D := 4.0/DCUB;
        SD := 2.0/DCUB;
//        D = 4./DCUB
//        SD = 2./DCUB
      end
      else if SIG <= 0.5 THEN
      begin
//      ELSEIF (SIG .LE. .5) THEN
//C
//C 0 .LT. SIG .LE. .5:  use approximations designed to avoid
//C                      cancellation error in the hyperbolic
//C                      functions when SIGMA is small.
//C
        SNHCSH (SIG, SINHM,COSHM,COSHMM);
        E := (SIG*SINHM - COSHMM - COSHMM)*DCUB;
        D := SIG*(SIG*COSHM-SINHM)/E;
        SD := SIG*SINHM/E;
//        CALL SNHCSH (SIG, SINHM,COSHM,COSHMM)
//        E = (SIG*SINHM - COSHMM - COSHMM)*DCUB
//        D = SIG*(SIG*COSHM-SINHM)/E
//        SD = SIG*SINHM/E
      end
      ELSE
      begin
//      ELSE
//C
//C SIG > .5:  scale SINHM and COSHM by 2*EXP(-SIG) in order
//C            to avoid overflow when SIGMA is large.
//C
        EMS := EXP(-SIG);
        SSINH := 1.0 - EMS*EMS;
        SSM := SSINH - 2.0*SIG*EMS;
        SCM := (1.0-EMS)*(1.0-EMS);
        E := (SIG*SSINH - SCM - SCM)*DCUB;
        D := SIG*(SIG*SCM-SSM)/E;
        SD := SIG*SSM/E;
//        EMS = EXP(-SIG)
//        SSINH = 1. - EMS*EMS
//        SSM = SSINH - 2.*SIG*EMS
//        SCM = (1.-EMS)*(1.-EMS)
//        E = (SIG*SSINH - SCM - SCM)*DCUB
//        D = SIG*(SIG*SCM-SSM)/E
//        SD = SIG*SSM/E
      end;
//      ENDIF
//      RETURN
//      END
end;

procedure INTRC0 (const PX,PY: TFloat; const NCC: longint;
  const LCC: TNcmaxIntArray; const N: longint;
  const X,Y,Z: TNmaxSingleArray; const LIST,LPTR: TN6IntArray; const LEND: TNmaxIntArray;
  var IST: longint; var PZ: TFloat; var IER: longint);
//      SUBROUTINE INTRC0 (PX,PY,NCC,LCC,N,X,Y,Z,LIST,LPTR,
//     .                   LEND, IST, PZ,IER)
//      INTEGER NCC, LCC(*), N, LIST(*), LPTR(*), LEND(N),
//     .        IST, IER
//      REAL    PX, PY, X(N), Y(N), Z(N), PZ
//C
//C***********************************************************
//C
//C                                               From SRFPACK
//C                                            Robert J. Renka
//C                                  Dept. of Computer Science
//C                                       Univ. of North Texas
//C                                           renka@cs.unt.edu
//C                                                   07/03/98
//C
//C   Given a triangulation of a set of nodes in the plane,
//C along with data values at the nodes, this subroutine com-
//C putes the value at P = (PX,PY) of the piecewise linear
//C function which interpolates the data values.  The surface
//C is extended in a continuous fashion beyond the boundary of
//C the triangulation, allowing extrapolation.
//C
//C On input:
//C
//C       PX,PY = Coordinates of the point P at which the sur-
//C               face is to be evaluated.
//C
//C       NCC = Number of constraint curves (refer to TRIPACK
//C             Subroutine ADDCST).  NCC .GE. 0.
//C
//C       LCC = Array of length NCC (or dummy array of length
//C             1 if NCC = 0) containing the index of the
//C             first node of constraint I in LCC(I).  For I =
//C             1 to NCC, LCC(I+1)-LCC(I) .GE. 3, where
//C             LCC(NCC+1) = N+1.
//C
//C       N = Number of nodes in the triangulation.  N .GE. 3.
//C
//C       X,Y = Arrays of length N containing the coordinates
//C             of the nodes with non-constraint nodes in the
//C             first LCC(1)-1 locations, followed by NCC se-
//C             quences of constraint nodes.
//C
//C       Z = Array of length N containing data values at the
//C           nodes.  Refer to Subroutine ZGRADL.
//C
//C       LIST,LPTR,LEND = Data structure defining the trian-
//C                        gulation.  Refer to TRIPACK
//C                        Subroutine TRMESH.
//C
//C The above parameters are not altered by this routine.
//C
//C       IST = Index of the starting node in the search for a
//C             triangle containing P.  1 .LE. IST .LE. N.
//C             The output value of IST from a previous call
//C             may be a good choice.
//C
//C On output:
//C
//C       IST = Index of one of the vertices of the triangle
//C             containing P (or a boundary node which is vis-
//C             ible from P) unless IER < 0.
//C
//C       PZ = Value of the interpolatory surface at P, or
//C            zero if IER < 0.
//C
//C       IER = Error indicator:
//C             IER = 0 if no errors were encountered and P is
//C                     contained in a triangle but not in a
//C                     constraint region.
//C             IER = 1 if no errors were encountered and P
//C                     lies in a constraint region triangle.
//C                     PZ is effectively an extrapolated
//C                     value in this case.
//C             IER = 2 if no errors were encountered and P is
//C                     exterior to the triangulation.  PZ is
//C                     an extrapolated value in this case.
//C             IER = -1 if NCC, N, or IST is outside its
//C                      valid range on input.  LCC is not
//C                      tested for validity.
//C             IER = -2 if the nodes are collinear.
//C
//C TRIPACK modules required by INTRC0:  CRTRI, JRAND, LEFT,
//C                                        LSTPTR, TRFIND
//C
//C SRFPACK module required by INTRC0:  COORDS
//C
//C***********************************************************
//C
var
       I1, I2, I3, IERR, LPL, N1, N2: longint;
          B1, B2, B3, DP, X1, X2, XP, Y1, Y2, YP: TFloat;
//      LOGICAL CRTRI
//      INTEGER I1, I2, I3, IERR, LPL, N1, N2
//      REAL    B1, B2, B3, DP, X1, X2, XP, Y1, Y2, YP
begin
//C
      XP := PX;
      YP := PY;
      PZ := 0.0;
//      XP = PX
//      YP = PY
//      PZ = 0.
//C
//C Test for invalid input parameters.
//C
      IF (NCC < 0)  OR  (N < 3)  OR  (IST < 1) OR  (IST > N) THEN
      begin
        IER := -1;
        Exit;
      END;
//      IF (NCC .LT. 0  .OR.  N .LT. 3  .OR.  IST .LT. 1
//     .    .OR.  IST .GT. N) THEN
//        IER = -1
//        RETURN
//      ENDIF
//C
//C Find a triangle (I1,I2,I3) containing P, or a pair of
//C   visible boundary nodes I1 and I2.
//C
      TRFIND (IST,XP,YP,N,X,Y,LIST,LPTR,LEND, I1,I2,I3);
      IF (I1 = 0) THEN
      begin
        IER := -2;
        Exit;
      END;
      IST := I1;
      IF (I3 <> 0) then
      begin

//      CALL TRFIND (IST,XP,YP,N,X,Y,LIST,LPTR,LEND, I1,I2,I3)
//      IF (I1 .EQ. 0) THEN
//        IER = -2
//        RETURN
//      ENDIF
//      IST = I1
//      IF (I3 .EQ. 0) GO TO 1
  //C
  //C P is in a triangle.  Compute its barycentric coordinates.
  //C
         COORDS (XP,YP,X[I1-1],X[I2-1],X[I3-1],Y[I1-1],Y[I2-1],
                    Y[I3-1], B1,B2,B3,IERR);
        IF (IERR <> 0) THEN
        begin
          IER := -2;
          Exit;
        END;
  //      CALL COORDS (XP,YP,X(I1),X(I2),X(I3),Y(I1),Y(I2),
  //     .             Y(I3), B1,B2,B3,IERR)
  //      IF (IERR .NE. 0) THEN
  //        IER = -2
  //        RETURN
  //      ENDIF
  //C
  //C Compute an interpolated value.
  //C
        PZ := B1*Z[I1-1] + B2*Z[I2-1] + B3*Z[I3-1];
        IER := 0;
        IF (CRTRI(NCC,LCC,I1,I2,I3)) THEN
        begin
          IER := 1;
        end
        ELSE
        begin
          IER := 0;
        END;
        Exit;
  //      PZ = B1*Z(I1) + B2*Z(I2) + B3*Z(I3)
  //      IER = 0
  //C
  //      IF (CRTRI(NCC,LCC,I1,I2,I3)) THEN
  //        IER = 1
  //      ELSE
  //        IER = 0
  //      ENDIF
  //      RETURN
  //C
      end;
//C P is exterior to the triangulation.  Extrapolate to P by
//C   extending the interpolatory surface as a constant
//C   beyond the boundary:  PZ is the function value at Q
//C   where Q is the closest boundary point to P.
//C
//C Determine Q by traversing the boundary starting from the
//C   rightmost visible node I1.
//C
      IER := 2;
      N2 := I1;
//    1 IER = 2
//      N2 = I1
//C
//C Top of loop:
//C
//C   Set N1 to the last neighbor of N2, and compute the dot
//C     product DP = (N2->N1,N2->P).  P FORWARD N2->N1 iff
//C     DP > 0.
//C
      repeat
        LPL := LEND[N2-1];
        N1 := -LIST[LPL-1];
        X1 := X[N1-1];
        Y1 := Y[N1-1];
        X2 := X[N2-1];
        Y2 := Y[N2-1];
        DP := (X1-X2)*(XP-X2) + (Y1-Y2)*(YP-Y2);

  //    2 LPL = LEND(N2)
  //      N1 = -LIST(LPL)
  //      X1 = X(N1)
  //      Y1 = Y(N1)
  //      X2 = X(N2)
  //      Y2 = Y(N2)
  //      DP = (X1-X2)*(XP-X2) + (Y1-Y2)*(YP-Y2)
        IF (DP <= 0.0) THEN
        begin
  //      IF (DP .LE. 0.) THEN
  //C
  //C   N2 is the closest boundary point to P.
  //C
          PZ := Z[N2-1];
          Exit;
        END;
  //        PZ = Z(N2)
  //        RETURN
  //      ENDIF
  //C
  //C   P FORWARD N2->N1.  Test for P FORWARD N1->N2.
  //C
        IF ((XP-X1)*(X2-X1) + (YP-Y1)*(Y2-Y1) > 0.0) THEN
        begin
  //      IF ((XP-X1)*(X2-X1) + (YP-Y1)*(Y2-Y1) .GT. 0.) THEN
  //C
  //C   The closest boundary point to P lies on N2-N1.  Compute
  //C     its local coordinates with respect to N2-N1.
  //C
          B1 := DP/( Sqr(X2-X1) + Sqr(Y2-Y1) );
          B2 := 1.0 - B1;
          PZ := B1*Z[N1-1] + B2*Z[N2-1];
          Exit;
        END;
  //        B1 = DP/( (X2-X1)**2 + (Y2-Y1)**2 )
  //        B2 = 1. - B1
  //        PZ = B1*Z(N1) + B2*Z(N2)
  //        RETURN
  //      ENDIF
  //C
  //C   Bottom of boundary traversal loop.
  //C
        N2 := N1;
  //      N2 = N1
      until False;
//      GO TO 2
//      END
end;

procedure INTRC1 (const PX,PY: TFloat; const NCC: longint;
  const LCC: TNcmaxIntArray; const N: longint; const X,Y,Z: TNmaxSingleArray;
  const LIST,LPTR: TN6IntArray; const LEND: TNmaxIntArray; const IFLGS: longint;
  const SIGMA: TN6SingleArray; const GRAD: TNtnxSingleArray; const DFLAG: longbool;
  var IST :longint; var PZ,PZX,PZY: TFloat; var IER: longint);
//      SUBROUTINE INTRC1 (PX,PY,NCC,LCC,N,X,Y,Z,LIST,LPTR,
//     .                   LEND,IFLGS,SIGMA,GRAD,
//     .                   DFLAG, IST, PZ,PZX,PZY,IER)
//      INTEGER NCC, LCC(*), N, LIST(*), LPTR(*), LEND(N),
//     .        IFLGS, IST, IER
//      LOGICAL DFLAG
//      REAL    PX, PY, X(N), Y(N), Z(N), SIGMA(*), GRAD(2,N),
//     .        PZ, PZX, PZY
//C
//C***********************************************************
//C
//C                                               From SRFPACK
//C                                            Robert J. Renka
//C                                  Dept. of Computer Science
//C                                       Univ. of North Texas
//C                                           renka@cs.unt.edu
//C                                                   07/03/98
//C
//C   Given a triangulation of a set of nodes in the plane,
//C along with data values and estimated gradients at the
//C nodes, this subroutine computes the value and, optionally,
//C the first partial derivatives at P = (PX,PY) of a C-1
//C (once-continuously differentiable) function F which int-
//C erpolates the data values and gradients.  Extrapolation to
//C a point exterior to the triangulation is accomplished by
//C extending the surface in such a way that F is C-1 over the
//C entire plane.
//C
//C   Subroutine FVAL is used to evaluate an interpolatory
//C surface under tension, while Subroutine TVAL is called in
//C the case of no tension (IFLGS .LE. 0 and SIGMA(1) = 0).
//C However, the surface under tension is well-defined with
//C SIGMA = 0, and, in this case, the two interpolants are
//C identical on triangulation arcs and outside the triangula-
//C tion.  The use of FVAL with no tension can be forced (at
//C a cost in efficiency) by setting IFLGS = 1 and storing
//C zeros in all components of SIGMA.  Note, however, that
//C first partial derivatives are only available from TVAL
//C (and at points outside the triangulation);  i.e., a proce-
//C dure for differentiating the surface under tension has not
//C been implemented.
//C
//C   A set of interpolated values at the vertices of a rec-
//C tangular grid can be obtained by a single call to
//C Subroutine UNIF.  Subroutine INTRC0 provides for evalua-
//C tion of the piecewise linear interpolatory surface.
//C
//C On input:
//C
//C       PX,PY = Coordinates of the point P at which the sur-
//C               face is to be evaluated.
//C
//C       NCC = Number of constraint curves (refer to TRIPACK
//C             Subroutine ADDCST).  NCC .GE. 0.
//C
//C       LCC = Array of length NCC (or dummy array of length
//C             1 if NCC = 0) containing the index of the
//C             first node of constraint I in LCC(I).  For I =
//C             1 to NCC, LCC(I+1)-LCC(I) .GE. 3, where
//C             LCC(NCC+1) = N+1.
//C
//C       N = Number of nodes in the triangulation.  N .GE. 3.
//C
//C       X,Y = Arrays of length N containing the coordinates
//C             of the nodes with non-constraint nodes in the
//C             first LCC(1)-1 locations, followed by NCC se-
//C             quences of constraint nodes.
//C
//C       Z = Array of length N containing data values at the
//C           nodes.  Refer to Subroutines ZGRADG and ZGRADL.
//C
//C       LIST,LPTR,LEND = Data structure defining the trian-
//C                        gulation.  Refer to TRIPACK
//C                        Subroutine TRMESH.
//C
//C       IFLGS = Tension factor option:
//C               IFLGS .LE. 0 if a single uniform tension
//C                            factor is to be used.
//C               IFLGS .GE. 1 if variable tension is desired.
//C
//C       SIGMA = Uniform tension factor (IFLGS .LE. 0), or
//C               array containing tension factors associated
//C               with arcs in one-to-one correspondence with
//C               LIST entries (IFLGS .GE. 1).  Refer to Sub-
//C               routines FVAL, GETSIG, SIG0, SIG1, and SIG2.
//C
//C       GRAD = 2 by N array whose columns contain estimated
//C              gradients at the nodes with X partial deriva-
//C              tives in the first row and Y partials in the
//C              second.  Refer to Subroutines GRADC, GRADG,
//C              GRADL, SMSURF, ZGRADG, and ZGRADL.
//C
//C       DFLAG = Logical flag which specifies whether first
//C               partial derivatives at P are to be computed:
//C               DFLAG = TRUE if and only if partials are
//C               to be computed by TVAL.  This option is only
//C               valid for IFLGS .LE. 0 and SIGMA(1) = 0 (and
//C               for points outside the triangulation).
//C
//C The above parameters are not altered by this routine.
//C
//C       IST = Index of the starting node in the search for a
//C             triangle containing P.  1 .LE. IST .LE. N.
//C             The output value of IST from a previous call
//C             may be a good choice.
//C
//C On output:
//C
//C       IST = Index of one of the vertices of the triangle
//C             containing P (or a boundary node which is vis-
//C             ible from P) unless IER = -1 or IER = -2.
//C
//C       PZ = Value of the interpolatory surface at P, or
//C            zero if IER < 0.
//C
//C       PZX,PZY = X and Y partials at P if DFLAG = .TRUE.
//C                 and IER .GE. 0, unaltered otherwise.
//C
//C       IER = Error indicator:
//C             IER = 0 if no errors were encountered and P is
//C                     contained in a triangle but not in a
//C                     constraint region.
//C             IER = 1 if no errors were encountered and P
//C                     lies in a constraint region triangle.
//C                     PZ is effectively an extrapolated
//C                     value in this case.
//C             IER = 2 if no errors were encountered and P is
//C                     exterior to the triangulation.  PZ is
//C                     an extrapolated value in this case.
//C             IER = -1 if NCC, N, or IST is outside its
//C                      valid range on input.  LCC is not
//C                      tested for validity.
//C             IER = -2 if the nodes are collinear.
//C             IER = -3 if P is contained in a triangle and
//C                      DFLAG = TRUE, but IFLGS > 0 or
//C                      SIGMA(1) .NE. 0.
//C
//C TRIPACK modules required by INTRC1:  CRTRI, JRAND, LEFT,
//C                                        LSTPTR, TRFIND
//C
//C SRFPACK modules required by INTRC1:  ARCINT, COORDS, FVAL,
//C                                        SNHCSH, TVAL
//C
//C Intrinsic function called by INTRC1:  SQRT
//C
//C***********************************************************
//C
//      INTEGER LSTPTR
//      LOGICAL CRTRI
var
       I1, I2, I3, IERR, LP, LPL, N1, N2, N3: longint;
       TENSN: longbool;
          A1, A2, B1, B2, C1, C2, D, D1, D2, D3, DP,
             DP1, DP3, F1, F2, R1, R12, R2, SIG, SIG1,
             SIG2, SIG3, T, T1, T2, X1, X2, X3, X12, X23,
             X2P, XP, XQ, XQP, Y1, Y2, Y3, Y12, Y23, Y2P,
             YP, YQ, YQP, Z1, Z2, Z3, ZQ, ZX1, ZX2, ZX3,
             ZXQ, ZY1, ZY2, ZY3, ZYQ: TFloat;
//      INTEGER I1, I2, I3, IERR, LP, LPL, N1, N2, N3
//      LOGICAL TENSN
//      REAL    A1, A2, B1, B2, C1, C2, D, D1, D2, D3, DP,
//     .        DP1, DP3, F1, F2, R1, R12, R2, SIG, SIG1,
//     .        SIG2, SIG3, T, T1, T2, X1, X2, X3, X12, X23,
//     .        X2P, XP, XQ, XQP, Y1, Y2, Y3, Y12, Y23, Y2P,
//     .        YP, YQ, YQP, Z1, Z2, Z3, ZQ, ZX1, ZX2, ZX3,
//     .        ZXQ, ZY1, ZY2, ZY3, ZYQ
  TrueTemp: longbool;
begin
//C
      XP := PX;
      YP := PY;
      PZ := 0.0;
//      XP = PX
//      YP = PY
//      PZ = 0.
//C
//C Test for invalid input parameters.
//C
      IF (NCC < 0)  OR  (N < 3)  OR  (IST < 1)
         OR  (IST > N) THEN
      begin
        IER := -1;
        Exit;
      END;
//      IF (NCC .LT. 0  .OR.  N .LT. 3  .OR.  IST .LT. 1
//     .    .OR.  IST .GT. N) THEN
//        IER = -1
//        RETURN
//      ENDIF
//C
//C Find a triangle (I1,I2,I3) containing P, or a pair of
//C   visible boundary nodes I1 and I2.
//C
      TRFIND (IST,XP,YP,N,X,Y,LIST,LPTR,LEND, I1,I2,I3);
      IF (I1 = 0) THEN
      begin
        IER := -2;
        Exit;
      END;
      IST := I1;
      TENSN := (IFLGS >= 1)  OR  (SIGMA[0] <> 0);
      IF (I3 <> 0) then
      begin
        IF (DFLAG  AND  TENSN) THEN
        begin
          IER := -3;
          Exit;
        END;
  //      CALL TRFIND (IST,XP,YP,N,X,Y,LIST,LPTR,LEND, I1,I2,I3)
  //      IF (I1 .EQ. 0) THEN
  //        IER = -2
  //        RETURN
  //      ENDIF
  //      IST = I1
  //      TENSN = IFLGS .GE. 1  .OR.  SIGMA(1) .NE. 0
  //      IF (I3 .EQ. 0) GO TO 1
  //      IF (DFLAG  .AND.  TENSN) THEN
  //        IER = -3
  //        RETURN
  //      ENDIF
  //C
  //C P is in a triangle.  Store local parameters for the
  //C   call to FVAL or TVAL.
  //C
        X1 := X[I1-1];
        Y1 := Y[I1-1];
        X2 := X[I2-1];
        Y2 := Y[I2-1];
        X3 := X[I3-1];
        Y3 := Y[I3-1];
        Z1 := Z[I1-1];
        Z2 := Z[I2-1];
        Z3 := Z[I3-1];
        ZX1 := GRAD[IWK_Index(I1-1,0)];
        ZX2 := GRAD[IWK_Index(I2-1,0)];
        ZX3 := GRAD[IWK_Index(I3-1,0)];
        ZY1 := GRAD[IWK_Index(I1-1,1)];
        ZY2 := GRAD[IWK_Index(I2-1,1)];
        ZY3 := GRAD[IWK_Index(I3-1,1)];
  //      X1 = X(I1)
  //      Y1 = Y(I1)
  //      X2 = X(I2)
  //      Y2 = Y(I2)
  //      X3 = X(I3)
  //      Y3 = Y(I3)
  //      Z1 = Z(I1)
  //      Z2 = Z(I2)
  //      Z3 = Z(I3)
  //      ZX1 = GRAD(1,I1)
  //      ZX2 = GRAD(1,I2)
  //      ZX3 = GRAD(1,I3)
  //      ZY1 = GRAD(2,I1)
  //      ZY2 = GRAD(2,I2)
  //      ZY3 = GRAD(2,I3)
        IF (TENSN) THEN
        begin
  //      IF (TENSN) THEN
  //C
  //C Set SIG1, SIG2, and SIG3 to the tension factors associated
  //C   with the sides opposite I1, I2, and I3, respectively,
  //C   and compute a value from FVAL.
  //C
          IF (IFLGS <= 0) THEN
          begin
            SIG1 := SIGMA[0];
            SIG2 := SIG1;
            SIG3 := SIG1;
          end
          ELSE
          begin
            LP := LSTPTR(LEND[I2-1],I3,LIST,LPTR);
            SIG1 := SIGMA[LP-1];
            LP := LSTPTR(LEND[I3-1],I1,LIST,LPTR);
            SIG2 := SIGMA[LP-1];
            LP := LSTPTR(LEND[I1-1],I2,LIST,LPTR);
            SIG3 := SIGMA[LP-1];
          END;
          FVAL (XP,YP,X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,ZX1,ZX2,
                    ZX3,ZY1,ZY2,ZY3,SIG1,SIG2,SIG3, PZ,IERR);
          IF (IERR < 0) THEN
          begin
            IER := -2;
            Exit;
          END;
  //        IF (IFLGS .LE. 0) THEN
  //          SIG1 = SIGMA(1)
  //          SIG2 = SIG1
  //          SIG3 = SIG1
  //        ELSE
  //          LP = LSTPTR(LEND(I2),I3,LIST,LPTR)
  //          SIG1 = SIGMA(LP)
  //          LP = LSTPTR(LEND(I3),I1,LIST,LPTR)
  //          SIG2 = SIGMA(LP)
  //          LP = LSTPTR(LEND(I1),I2,LIST,LPTR)
  //          SIG3 = SIGMA(LP)
  //        ENDIF
  //        CALL FVAL (XP,YP,X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,ZX1,ZX2,
  //     .             ZX3,ZY1,ZY2,ZY3,SIG1,SIG2,SIG3, PZ,IERR)
  //        IF (IERR .LT. 0) THEN
  //          IER = -2
  //          RETURN
  //        ENDIF
        end
        else
        begin
  //      ELSE
  //C
  //C Compute an interpolated value from TVAL for no tension.
  //C
          TVAL (XP,YP,X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,ZX1,ZX2,
                    ZX3,ZY1,ZY2,ZY3,DFLAG, PZ,PZX,PZY,IERR);
          IF (IERR <> 0) THEN
          begin
            IER := -2;
            Exit;
          END;
  //        CALL TVAL (XP,YP,X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,ZX1,ZX2,
  //     .             ZX3,ZY1,ZY2,ZY3,DFLAG, PZ,PZX,PZY,IERR)
  //        IF (IERR .NE. 0) THEN
  //          IER = -2
  //          RETURN
  //        ENDIF
  //      ENDIF
        end;
  //C
        IF (CRTRI(NCC,LCC,I1,I2,I3)) THEN
        begin
          IER := 1;
        end
        ELSE
        begin
          IER := 0
        END;
        Exit;
  //      IF (CRTRI(NCC,LCC,I1,I2,I3)) THEN
  //        IER = 1
  //      ELSE
  //        IER = 0
  //      ENDIF
  //      RETURN
      end;
//C
//C P is exterior to the triangulation.  Extrapolate to P by
//C   passing a linear function of one variable through the
//C   value and directional derivative (in the direction
//C   Q->P) of the interpolatory surface F at Q, where Q is
//C   the closest boundary point to P.
//C
//C Determine Q by traversing the boundary starting from the
//C   rightmost visible node I1.
//C
      IER := 2;
      N2 := I1;
//    1 IER = 2
//      N2 = I1
//C
//C Top of loop:
//C
//C   Set N1 to the last neighbor of N2, and compute the dot
//C     product DP = (N2->N1,N2->P).  P FORWARD N2->N1 iff
//C     DP > 0.
//C
      repeat
        LPL := LEND[N2-1];
        N1 := -LIST[LPL-1];
        X1 := X[N1-1];
        Y1 := Y[N1-1];
        X2 := X[N2-1];
        Y2 := Y[N2-1];
        DP := (X1-X2)*(XP-X2) + (Y1-Y2)*(YP-Y2);
  //    2 LPL = LEND(N2)
  //      N1 = -LIST(LPL)
  //      X1 = X(N1)
  //      Y1 = Y(N1)
  //      X2 = X(N2)
  //      Y2 = Y(N2)
  //      DP = (X1-X2)*(XP-X2) + (Y1-Y2)*(YP-Y2)
        IF (DP <= 0.0) THEN
        begin
  //      IF (DP .LE. 0.) THEN
  //C
  //C   N2 is the closest boundary point to P:  P lies in a
  //C     wedge with sides orthogonal to N1-N2 and N2-N3, where
  //C     N3 is the first neighbor of N2.  The linear interpo-
  //C     lant must be modified by a correction term which
  //C     provides for continuity of the derivative across the
  //C     sides of the wedge.
  //C
          LP := LPTR[LPL-1];
          N3 := LIST[LP-1];
          ZX1 := GRAD[IWK_Index(N1-1,0)];
          ZX2 := GRAD[IWK_Index(N2-1,0)];
          ZX3 := GRAD[IWK_Index(N3-1,0)];
          ZY1 := GRAD[IWK_Index(N1-1,1)];
          ZY2 := GRAD[IWK_Index(N2-1,1)];
          ZY3 := GRAD[IWK_Index(N3-1,1)];
          X12 := X2-X1;
          Y12 := Y2-Y1;
          X23 := X[N3-1]-X2;
          Y23 := Y[N3-1]-Y2;
          X2P := XP-X2;
          Y2P := YP-Y2;
          DP1 := -DP;
          DP3 := X23*X2P + Y23*Y2P;
          D2 := X2P*X2P + Y2P*Y2P;
          D1 := SQRT((X12*X12 + Y12*Y12)*D2);
          D3 := SQRT((X23*X23 + Y23*Y23)*D2);
          D := Sqr(X12*Y23 - Y12*X23);
          T1 := DP3*( X12*(ZY2-ZY1)-Y12*(ZX2-ZX1) )/D1;
          T2 := DP1*( X23*(ZY3-ZY2)-Y23*(ZX3-ZX2) )/D3;
          T := DP1*DP3*(T1+T2);
          PZ := Z[N2-1] + X2P*ZX2 + Y2P*ZY2;
          IF (D <> 0.0) then PZ := PZ - T/D;
          IF (DFLAG) THEN
          begin
            T := T/D2;
            D1 := DP3*(T1+T2+T2);
            D2 := DP1*(T1+T1+T2);
            PZX := ZX2;
            IF (D <> 0.0) then PZX := PZX + (T*X2P - D1*X12 -
                                       D2*X23)/D;
            PZY := ZY2;
            IF (D <> 0.0) then PZY := PZY + (T*Y2P - D1*Y12 -
                                       D2*Y23)/D;
          END;
          Exit;
  //        LP = LPTR(LPL)
  //        N3 = LIST(LP)
  //        ZX1 = GRAD(1,N1)
  //        ZX2 = GRAD(1,N2)
  //        ZX3 = GRAD(1,N3)
  //        ZY1 = GRAD(2,N1)
  //        ZY2 = GRAD(2,N2)
  //        ZY3 = GRAD(2,N3)
  //        X12 = X2-X1
  //        Y12 = Y2-Y1
  //        X23 = X(N3)-X2
  //        Y23 = Y(N3)-Y2
  //        X2P = XP-X2
  //        Y2P = YP-Y2
  //        DP1 = -DP
  //        DP3 = X23*X2P + Y23*Y2P
  //        D2 = X2P*X2P + Y2P*Y2P
  //        D1 = SQRT((X12*X12 + Y12*Y12)*D2)
  //        D3 = SQRT((X23*X23 + Y23*Y23)*D2)
  //        D = (X12*Y23 - Y12*X23)**2
  //        T1 = DP3*( X12*(ZY2-ZY1)-Y12*(ZX2-ZX1) )/D1
  //        T2 = DP1*( X23*(ZY3-ZY2)-Y23*(ZX3-ZX2) )/D3
  //        T = DP1*DP3*(T1+T2)
  //        PZ = Z(N2) + X2P*ZX2 + Y2P*ZY2
  //        IF (D .NE. 0.) PZ = PZ - T/D
  //        IF (DFLAG) THEN
  //          T = T/D2
  //          D1 = DP3*(T1+T2+T2)
  //          D2 = DP1*(T1+T1+T2)
  //          PZX = ZX2
  //          IF (D .NE. 0.) PZX = PZX + (T*X2P - D1*X12 -
  //     .                                D2*X23)/D
  //          PZY = ZY2
  //          IF (D .NE. 0.) PZY = PZY + (T*Y2P - D1*Y12 -
  //     .                                D2*Y23)/D
  //        ENDIF
  //        RETURN
        END;
  //      ENDIF
  //C
  //C   P FORWARD N2->N1.  Test for P FORWARD N1->N2.
  //C
        IF ((XP-X1)*(X2-X1) + (YP-Y1)*(Y2-Y1) <= 0.0) THEN
        begin
  //      IF ((XP-X1)*(X2-X1) + (YP-Y1)*(Y2-Y1) .LE. 0.) THEN
  //C
  //C   Bottom of boundary traversal loop.
  //C
          N2 := N1;
          Continue;
        END;
  //        N2 = N1
  //        GO TO 2
  //      ENDIF
        break;
      until False;
//C
//C The closest boundary point Q lies on N2-N1.  Store par-
//C   tials at N1 and N2, and compute Q and its barycentric
//C   coordinates R1 and R2.
//C
      ZX1 := GRAD[IWK_Index(N1-1,0)];
      ZY1 := GRAD[IWK_Index(N1-1,1)];
      ZX2 := GRAD[IWK_Index(N2-1,0)];
      ZY2 := GRAD[IWK_Index(N2-1,1)];
      X12 := X2-X1;
      Y12 := Y2-Y1;
      D2 := X12*X12 + Y12*Y12;
      R1 := DP/D2;
      R2 := 1.0 - R1;
      XQ := R1*X1 + R2*X2;
      YQ := R1*Y1 + R2*Y2;
//      ZX1 = GRAD(1,N1)
//      ZY1 = GRAD(2,N1)
//      ZX2 = GRAD(1,N2)
//      ZY2 = GRAD(2,N2)
//      X12 = X2-X1
//      Y12 = Y2-Y1
//      D2 = X12*X12 + Y12*Y12
//      R1 = DP/D2
//      R2 = 1. - R1
//      XQ = R1*X1 + R2*X2
//      YQ = R1*Y1 + R2*Y2
      IF (TENSN) THEN
      begin
//      IF (TENSN) THEN
//C
//C   Set SIG to the tension factor associated with N1-N2 and
//C     compute an interpolated value ZQ at Q from FVAL.
//C
        IF (IFLGS <= 0) THEN
        begin
          SIG := SIGMA[0];
        end
        ELSE
        begin
          SIG := SIGMA[LPL-1];
        END;
        TrueTemp := True;
        ARCINT (R1,X1,X2,Y1,Y2,Z[N1-1],Z[N2-1],ZX1,ZX2,
                    ZY1,ZY2,SIG,TrueTemp, ZQ,ZXQ,ZYQ,IERR);
//        IF (IFLGS .LE. 0) THEN
//          SIG = SIGMA(1)
//        ELSE
//          SIG = SIGMA(LPL)
//        ENDIF
//        CALL ARCINT (R1,X1,X2,Y1,Y2,Z(N1),Z(N2),ZX1,ZX2,
//     .               ZY1,ZY2,SIG,.TRUE., ZQ,ZXQ,ZYQ,IERR)
//C
//C   Compute the extrapolated value at P.
//C
        XQP := XP-XQ;
        YQP := YP-YQ;
        PZ := ZQ + ZXQ*XQP + ZYQ*YQP;
        IF (DFLAG) THEN
        begin
          T := ((ZX2-ZX1)*XQP + (ZY2-ZY1)*YQP)/D2;
          PZX := ZXQ + X12*T;
          PZY := ZYQ + Y12*T;
        END;
//        XQP = XP-XQ
//        YQP = YP-YQ
//        PZ = ZQ + ZXQ*XQP + ZYQ*YQP
//        IF (DFLAG) THEN
//          T = ((ZX2-ZX1)*XQP + (ZY2-ZY1)*YQP)/D2
//          PZX = ZXQ + X12*T
//          PZY = ZYQ + Y12*T
//        ENDIF
      end
      ELSE
      begin
//      ELSE
//C
//C   Compute the cardinal function values and interpolated
//C     value at Q associated with TVAL.
//C
        R12 := R1*R2;
        F1 := R1*R12;
        F2 := R2*R12;
        A1 := R1 + (F1-F2);
        A2 := R2 - (F1-F2);
        B1 := X12*F1;
        B2 := -X12*F2;
        C1 := Y12*F1;
        C2 := -Y12*F2;
        ZQ := A1*Z[N1-1] + A2*Z[N2-1] + B1*ZX1 + B2*ZX2 +
            C1*ZY1 + C2*ZY2;
//        R12 = R1*R2
//        F1 = R1*R12
//        F2 = R2*R12
//        A1 = R1 + (F1-F2)
//        A2 = R2 - (F1-F2)
//        B1 = X12*F1
//        B2 = -X12*F2
//        C1 = Y12*F1
//        C2 = -Y12*F2
//        ZQ = A1*Z(N1) + A2*Z(N2) + B1*ZX1 + B2*ZX2 +
//     .       C1*ZY1 + C2*ZY2
//C
//C   Compute the extrapolated value at P.
//C
        XQP := XP-XQ;
        YQP := YP-YQ;
        T1 := R1*ZX1 + R2*ZX2;
        T2 := R1*ZY1 + R2*ZY2;
        PZ := ZQ + T1*XQP + T2*YQP;
        IF (DFLAG) THEN
        begin
          T := (3.0*R12*(2.0*(Z[N2-1]-Z[N1-1]) - X12*(ZX1+ZX2) -
              Y12*(ZY1+ZY2)) + (ZX2-ZX1)*XQP +
              (ZY2-ZY1)*YQP)/D2;
          PZX := T1 + X12*T;
          PZY := T2 + Y12*T;
        END;
//        XQP = XP-XQ
//        YQP = YP-YQ
//        T1 = R1*ZX1 + R2*ZX2
//        T2 = R1*ZY1 + R2*ZY2
//        PZ = ZQ + T1*XQP + T2*YQP
//        IF (DFLAG) THEN
//          T = (3.*R12*(2.*(Z(N2)-Z(N1)) - X12*(ZX1+ZX2) -
//     .         Y12*(ZY1+ZY2)) + (ZX2-ZX1)*XQP +
//     .         (ZY2-ZY1)*YQP)/D2
//          PZX = T1 + X12*T
//          PZY = T2 + Y12*T
//        ENDIF
      END;
//      ENDIF
//      RETURN
//      END
end;

procedure ROTATE (const N: longint; const C,S: TFloat; var X,Y: array of TFloat;
  const XOffset, YOffset: longint );
//      SUBROUTINE ROTATE (N,C,S, X,Y )
//      INTEGER N
//      REAL    C, S, X(N), Y(N)
//C
//C***********************************************************
//C
//C                                               From SRFPACK
//C                                            Robert J. Renka
//C                                  Dept. of Computer Science
//C                                       Univ. of North Texas
//C                                           renka@cs.unt.edu
//C                                                   09/01/88
//C
//C                                                ( C  S)
//C   This subroutine applies the Givens rotation  (     )  to
//C                                                (-S  C)
//C                    (X(1) ... X(N))
//C the 2 by N matrix  (             ) .
//C                    (Y(1) ... Y(N))
//C
//C   This routine is identical to Subroutine SROT from the
//C LINPACK BLAS (Basic Linear Algebra Subroutines).
//C
//C On input:
//C
//C       N = Number of columns to be rotated.
//C
//C       C,S = Elements of the Givens rotation.  Refer to
//C             Subroutine GIVENS.
//C
//C The above parameters are not altered by this routine.
//C
//C       X,Y = Arrays of length .GE. N containing the compo-
//C             nents of the vectors to be rotated.
//C
//C On output:
//C
//C       X,Y = Arrays containing the rotated vectors (not
//C             altered if N < 1).
//C
//C Modules required by ROTATE:  None
//C
//C***********************************************************
//C
var
       I: longint;
          XI, YI: TFloat;

//      INTEGER I
//      REAL    XI, YI
begin
//C
      for I := 1 to N do
      begin
//      DO 1 I = 1,N
        XI := X[I-1+XOffset];
        YI := Y[I-1+XOffset];
        X[I-1+XOffset] := C*XI + S*YI;
        Y[I-1+XOffset] := -S*XI + C*YI;
//        XI = X(I)
//        YI = Y(I)
//        X(I) = C*XI + S*YI
//        Y(I) = -S*XI + C*YI
//    1   CONTINUE
      end;
//      RETURN
//      END
end;

procedure SETRO1 (const XK,YK,ZK,XI,YI,ZI,S1,S2,W: TFloat;
  var ROW: array of TFloat);
//      SUBROUTINE SETRO1 (XK,YK,ZK,XI,YI,ZI,S1,S2,W, ROW)
//      REAL XK, YK, ZK, XI, YI, ZI, S1, S2, W, ROW(6)
//C
//C***********************************************************
//C
//C                                               From SRFPACK
//C                                            Robert J. Renka
//C                                  Dept. of Computer Science
//C                                       Univ. of North Texas
//C                                           renka@cs.unt.edu
//C                                                   09/01/88
//C
//C   This subroutine sets up the I-th row of an augmented re-
//C gression matrix for a weighted least squares fit of a
//C quadratic function Q(X,Y) to a set of data values Z, where
//C Q(XK,YK) = ZK.  The first three columns (quadratic terms)
//C are scaled by S2, and the fourth and fifth columns (lin-
//C ear terms) are scaled by S1.
//C
//C On input:
//C
//C       XK,YK = Coordinates of node K.
//C
//C       ZK = Data value at node K to be interpolated by Q.
//C
//C       XI,YI,ZI = Coordinates and data value at node I.
//C
//C       S1,S2 = Scale factors.
//C
//C       W = Weight associated with node I.
//C
//C The above parameters are not altered by this routine.
//C
//C       ROW = Array of length 6.
//C
//C On output:
//C
//C       ROW = Array containing a row of the augmented re-
//C             gression matrix.
//C
//C Modules required by SETRO1:  None
//C
//C***********************************************************
//C
var
       DX, DY, W1, W2: TFloat;
//      REAL DX, DY, W1, W2
//C
begin
      DX := XI - XK;
      DY := YI - YK;
      W1 := S1*W;
      W2 := S2*W;
      ROW[0] := DX*DX*W2;
      ROW[1] := DX*DY*W2;
      ROW[2] := DY*DY*W2;
      ROW[3] := DX*W1;
      ROW[4] := DY*W1;
      ROW[5] := (ZI - ZK)*W;
//      DX = XI - XK
//      DY = YI - YK
//      W1 = S1*W
//      W2 = S2*W
//      ROW(1) = DX*DX*W2
//      ROW(2) = DX*DY*W2
//      ROW(3) = DY*DY*W2
//      ROW(4) = DX*W1
//      ROW(5) = DY*W1
//      ROW(6) = (ZI - ZK)*W
//      RETURN
//      END
end;

procedure SGPRNT (const N,LUNIT: longint; const LIST,LPTR: TN6IntArray;
  const LEND: TNmaxIntArray; const SIGMA: TN6SingleArray);
//      SUBROUTINE SGPRNT (N,LUNIT,LIST,LPTR,LEND,SIGMA,ifortran)
//      dll_import Sgprnt100, Sgprnt110, Sgprnt120, Sgprnt130,
//     .  Sgprnt140, Sgprnt200, Sgprnt210, Sgprnt220
//      INTEGER N, LUNIT, LIST(*), LPTR(*), LEND(N)
//      REAL    SIGMA(*)
//C
//C***********************************************************
//C
//C                                               From SRFPACK
//C                                            Robert J. Renka
//C                                  Dept. of Computer Science
//C                                       Univ. of North Texas
//C                                           renka@cs.unt.edu
//C                                                   07/03/98
//C
//C   Given a triangulation of a set of nodes in the plane,
//C along with an array of tension factors associated with the
//C triangulation arcs, this subroutine prints the list of
//C arcs (with tension factors) ordered by endpoint nodal in-
//C dexes.  An arc is identified with its smaller endpoint
//C index:  N1-N2, where N1 < N2.
//C
//C On input:
//C
//C       N = Number of nodes in the triangulation.  3 .LE. N
//C           .LE. 9999.
//C
//C       LUNIT = Logical unit for output.  0 .LE. LUNIT .LE.
//C               99.  Output is printed on unit 6 if LUNIT is
//C               outside its valid range.
//C
//C       LIST,LPTR,LEND = Data structure defining the trian-
//C                        gulation.  Refer to TRIPACK
//C                        Subroutine TRMESH.
//C
//C       SIGMA = Array of length 2*NA = 6*(N-1)-2*NB, where
//C               NA and NB are the numbers of arcs and boun-
//C               dary nodes, respectively, containing tension
//C               factors associated with arcs in one-to-one
//C               correspondence with LIST entries.  Note that
//C               each arc N1-N2 has two LIST entries and
//C               thus, SIGMA(I) and SIGMA(J) should be iden-
//C               tical, where LIST(I) = N2 (in the adjacency
//C               list for N1) and LIST(J) = N1 (in the list
//C               associated with N2).  Both SIGMA(I) and
//C               SIGMA(J) are printed if they are not iden-
//C               tical.
//C
//C None of the parameters are altered by this routine.
//C
//C TRIPACK module required by SGPRNT:  LSTPTR
//C
//C Intrinsic function called by SGPRNT:  ABS
//C
//C***********************************************************
//C
//      INTEGER LSTPTR
var
{$IFDEF UseSfrMessages}
  LUN, NAT, LP2,
{$ENDIF}
       LP1, LPL, N1, N2, NA, NB, NE,
             NL, NLMAX, NM1, NMAX: longint;
          SIG: TFloat;
{$IFDEF UseSfrMessages}
       ERROR: longbool;
       IFORTRAN: longint;
  N1_Temp: longint;
{$ENDIF}
//      INTEGER LP1, LP2, LPL, LUN, N1, N2, NA, NAT, NB, NE,
//     .        NL, NLMAX, NM1, NMAX
//      LOGICAL ERROR
//      REAL    SIG
//      integer IFORTRAN
begin
  NMAX := 9999;
  NLMAX := 60;
{$IFDEF UseSfrMessages}
  Ifortran := 2;
//      DATA NMAX/9999/,  NLMAX/60/
//C
      LUN := LUNIT;
      IF (LUN < 0)  OR  (LUN > 99) then LUN := 6;
{$ENDIF}
//      LUN = LUNIT
//      IF (LUN .LT. 0  .OR.  LUN .GT. 99) LUN = 6
//C
//C Print a heading, test for invalid N, and initialize coun-
//C   ters:
//C
//C NL = Number of lines printed on the current page
//C NA = Number of arcs encountered
//C NE = Number of errors in SIGMA encountered
//C NB = Number of boundary nodes encountered
//C
//!      WRITE (LUN,100) N
{$IFDEF UseSfrMessages}
      Sgprnt100P(IFORTRAN, N);
{$ENDIF}
      IF (N < 3)  OR  (N > NMAX) then
      begin
//C N is outside its valid range.
//C
{$IFDEF UseSfrMessages}
        Sgprnt220(IFORTRAN, NMAX);
{$ENDIF}
        Exit;
//    4 CALL Sgprnt220(IFORTRAN, NMAX)
//      RETURN
      end;
      NL := 6;
      NA := 0;
      NE := 0;
      NB := 0;
//      CALL Sgprnt100(IFORTRAN, N)
//      IF (N .LT. 3  .OR.  N .GT. NMAX) GO TO 4
//      NL = 6
//      NA = 0
//      NE = 0
//      NB = 0
//C
//C Outer loop on nodes N1.  LPL points to the last neighbor
//C   of N1.
//C
      NM1 := N - 1;
//      NM1 = N - 1
      for N1 := 1 to NM1 do
      begin
//      DO 3 N1 = 1,NM1
        LPL := LEND[N1-1];
        IF (LIST[LPL-1] < 0) then NB := NB + 1;
        LP1 := LPL;
//        LPL = LEND(N1)
//        IF (LIST(LPL) .LT. 0) NB = NB + 1
//        LP1 = LPL
//C
//C Inner loop on neighbors N2 of N1 such that N1 < N2.
//C
        repeat
         LP1 := LPTR[LP1-1];
          N2 := ABS(LIST[LP1-1]);
          IF (N2 < N1) then Continue;
          NA := NA + 1;
          SIG := SIGMA[LP1-1];
//    1   LP1 = LPTR(LP1)
//          N2 = ABS(LIST(LP1))
//          IF (N2 .LT. N1) GO TO 2
//          NA = NA + 1
//          SIG = SIGMA(LP1)
//C
//C   Test for an invalid SIGMA entry.
//C
{$IFDEF UseSfrMessages}
          N1_Temp := N1;
          LP2 := LSTPTR (LEND[N2-1],N1_Temp,LIST,LPTR);
          ERROR := SIGMA[LP2-1] <> SIG;
          IF (ERROR) then NE := NE + 1;
//          LP2 = LSTPTR (LEND(N2),N1,LIST,LPTR)
//          ERROR = SIGMA(LP2) .NE. SIG
//          IF (ERROR) NE = NE + 1
//C
//C   Print a line and update the counters.
//C
//!          IF (.NOT. ERROR) WRITE (LUN,110) N1, N2, SIG
          N1_Temp := N1;
          IF (NOT ERROR) then Sgprnt110(IFORTRAN, N1_Temp, N2, SIG);
{$ENDIF}
//          IF (.NOT. ERROR) CALL Sgprnt110(IFORTRAN, N1, N2, SIG)
//!          IF (ERROR) WRITE (LUN,120) N1, N2, SIG, SIGMA(LP2)
{$IFDEF UseSfrMessages}
          IF (ERROR) then Sgprnt120(IFORTRAN, N1_Temp, N2, SIG, SIGMA[LP2-1]);
{$ENDIF}
//          IF (ERROR) call Sgprnt120(IFORTRAN, N1, N2, SIG, SIGMA(LP2))
          NL := NL + 1;
          IF (NL >= NLMAX) THEN
          begin
{$IFDEF UseSfrMessages}
            Sgprnt130(IFORTRAN);
{$ENDIF}
            NL := 1;
          END;
//          NL = NL + 1
//          IF (NL .GE. NLMAX) THEN
//!            WRITE (LUN,130)
//            CALL Sgprnt130(IFORTRAN)
//            NL = 1
//          ENDIF
//C
//C Bottom of loop on neighbors N2 of N1.
//C
//    2     IF (LP1 .NE. LPL) GO TO 1
        until LP1 = LPL;
//    3   CONTINUE
      end;
{$IFDEF UseSfrMessages}
      LPL := LEND[N-1];
      IF (LIST[LPL-1] < 0) then NB := NB + 1;
//      LPL = LEND(N)
//      IF (LIST(LPL) .LT. 0) NB = NB + 1
//C
//C Test for errors in SIGMA.
//C
//!      IF (NE .GT. 0) WRITE (LUN,200) NE
      IF (NE > 0) then Sgprnt200(IFORTRAN, NE);
{$ENDIF}
//      IF (NE .GT. 0) CALL Sgprnt200(IFORTRAN, NE)
//C
//C Print NA and test for an invalid triangulation.
//C
//!      WRITE (LUN,140) NA
{$IFDEF UseSfrMessages}
      Sgprnt140(IFORTRAN, NA);
      NAT := 3*NM1 - NB;
      IF (NAT <> NA) then Sgprnt210(IFORTRAN, NAT);
{$ENDIF}
//      CALL Sgprnt140(IFORTRAN, NA)
//      NAT = 3*NM1 - NB
//!      IF (NAT .NE. NA) WRITE (LUN,210) NAT
//      IF (NAT .NE. NA) CALL Sgprnt210(IFORTRAN, NAT)
//      RETURN
//C
//C N is outside its valid range.
//C
//!    4 WRITE (LUN,220) NMAX
//    4 CALL Sgprnt220(IFORTRAN, NMAX)
//      RETURN
//C
//C Print formats:
//C
//!  100 FORMAT (///14X,'Tension Factors,  N =',I5,
//!     .        ' Nodes'//1X,18X,'N1',5X,'N2',8X,'Tension'//)
//!  110 FORMAT (1X,16X,I4,3X,I4,5X,F12.8)
//!  120 FORMAT (1X,16X,I4,3X,I4,5X,F12.8,3X,F12.8,' *')
//!  130 FORMAT (///)
//!  140 FORMAT (//1X,10X,'NA =',I5,' Arcs')
//C
//C Error messages:
//C
//!  200 FORMAT (//1X,10X,'*',I5,' Errors in SIGMA')
//!  210 FORMAT (/1X,10X,'*** Error in triangulation:  ',
//!     .        '3N-NB-3 = ',I5,' ***')
//!  220 FORMAT (1X,10X,'*** N is outside its valid range:  ',
//!     .        'NMAX = ',I4,' ***')
//      END
end;

procedure SNHCSH (const X: TFloat; var SINHM,COSHM,COSHMM: TFloat);
//      SUBROUTINE SNHCSH (X, SINHM,COSHM,COSHMM)
//      REAL X, SINHM, COSHM, COSHMM
//C
//C***********************************************************
//C
//C                                               From SRFPACK
//C                                            Robert J. Renka
//C                                  Dept. of Computer Science
//C                                       Univ. of North Texas
//C                                           renka@cs.unt.edu
//C                                                   03/18/90
//C
//C   This subroutine computes approximations to the modified
//C hyperbolic functions defined below with relative error
//C bounded by 4.7E-12 for a floating point number system with
//C sufficient precision.  For IEEE standard single precision,
//C the relative error is less than 1.E-5 for all x.
//C
//C   Note that the 13-digit constants in the data statements
//C below may not be acceptable to all compilers.
//C
//C On input:
//C
//C       X = Point at which the functions are to be
//C           evaluated.
//C
//C X is not altered by this routine.
//C
//C On output:
//C
//C       SINHM = sinh(X) - X.
//C
//C       COSHM = cosh(X) - 1.
//C
//C       COSHMM = cosh(X) - 1 - X*X/2.
//C
//C Modules required by SNHCSH:  None
//C
//C Intrinsic functions called by SNHCSH:  ABS, EXP
//C
//C***********************************************************
//C
var
       AX, {C1, C2, C3, C4,} EXPX, F, XC, XS, XSD2, XSD4: TFloat;
//      REAL AX, C1, C2, C3, C4, EXPX, F, XC, XS, XSD2, XSD4
const
  C1 = 0.1666666666659E0;
  C2 = 0.8333333431546E-2;
  C3 = 0.1984107350948E-3;
  C4 = 0.2768286868175E-5;
//C
//      DATA C1/.1666666666659E0/,
//     .     C2/.8333333431546E-2/,
//     .     C3/.1984107350948E-3/,
//     .     C4/.2768286868175E-5/
begin
      AX := ABS(X);
      XS := AX*AX;
//      AX = ABS(X)
//      XS = AX*AX
      IF (AX <= 0.5) THEN
      begin
//      IF (AX .LE. .5) THEN
//C
//C Approximations for small X:
//C
        XC := X*XS;
        SINHM := XC*(((C4*XS+C3)*XS+C2)*XS+C1);
        XSD4 := 0.25*XS;
        XSD2 := XSD4 + XSD4;
        F := (((C4*XSD4+C3)*XSD4+C2)*XSD4+C1)*XSD4;
        COSHMM := XSD2*F*(F+2.0);
        COSHM := COSHMM + XSD2;
      end
      ELSE
      begin
//        XC = X*XS
//        SINHM = XC*(((C4*XS+C3)*XS+C2)*XS+C1)
//        XSD4 = .25*XS
//        XSD2 = XSD4 + XSD4
//        F = (((C4*XSD4+C3)*XSD4+C2)*XSD4+C1)*XSD4
//        COSHMM = XSD2*F*(F+2.)
//        COSHM = COSHMM + XSD2
//      ELSE
//C
//C Approximations for large X:
//C
        EXPX := EXP(AX);
        SINHM := -(((1.0/EXPX+AX)+AX)-EXPX)/2.0;
        IF (X < 0.0) then SINHM := -SINHM;
        COSHM := ((1.0/EXPX-2.0)+EXPX)/2.0;
        COSHMM := COSHM - XS/2.0;
      END;
//        EXPX = EXP(AX)
//        SINHM = -(((1./EXPX+AX)+AX)-EXPX)/2.
//        IF (X .LT. 0.) SINHM = -SINHM
//        COSHM = ((1./EXPX-2.)+EXPX)/2.
//        COSHMM = COSHM - XS/2.
//      ENDIF
//      RETURN
//      END
end;

FUNCTION TRVOL (const X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3: TFloat): TFloat;
//      REAL FUNCTION TRVOL (X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3)
//      REAL X1, X2, X3, Y1, Y2, Y3, Z1, Z2, Z3
//C
//C***********************************************************
//C
//C                                               From SRFPACK
//C                                            Robert J. Renka
//C                                  Dept. of Computer Science
//C                                       Univ. of North Texas
//C                                           renka@cs.unt.edu
//C                                                   09/01/88
//C
//C   This function computes the integral over a triangle of
//C the linear (planar) surface which interpolates data
//C values at the vertices.
//C
//C On input:
//C
//C       X1,X2,X3 = X coordinates of the vertices of the tri-
//C                  angle in counterclockwise order.
//C
//C       Y1,Y2,Y3 = Y coordinates of the vertices of the tri-
//C                  angle in one-to-one correspondence with
//C                  X1, X2, and X3.
//C
//C       Z1,Z2,Z3 = Data values at the vertices (X1,Y1),
//C                  (X2,Y2), (X3,Y3), respectively.
//C
//C Input parameters are not altered by this function.
//C
//C On output:
//C
//C       TRVOL = Integral over the triangle of the linear
//C               interpolant.  Note that TRVOL will have
//C               the wrong sign if the vertices are speci-
//C               fied in clockwise order.
//C
//C Modules required by TRVOL:  None
//C
//C***********************************************************
//C
//      REAL AREA
  function Area: TFloat;
  begin
    result := (X2-X1)*(Y3-Y1) - (X3-X1)*(Y2-Y1);
  end;
begin
//C
//      AREA = (X2-X1)*(Y3-Y1) - (X3-X1)*(Y2-Y1)
//C
//C AREA is twice the (signed) area of the triangle.
//C TRVOL is the mean of the data values times the area of the
//C   triangle.
//C
      result := (Z1 + Z2 + Z3)*AREA/6.0;
//      TRVOL = (Z1 + Z2 + Z3)*AREA/6.
//      RETURN
//      END
end;

procedure TVAL (const X,Y,X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,ZX1,
  ZX2,ZX3,ZY1,ZY2,ZY3: TFloat; const DFLAG: longbool; var F,FX,FY: TFloat;
  var IER: longint);
//      SUBROUTINE TVAL (X,Y,X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,ZX1,
//     .                 ZX2,ZX3,ZY1,ZY2,ZY3,DFLAG, F,FX,FY,
//     .                 IER)
//      INTEGER IER
//      LOGICAL DFLAG
//      REAL    X, Y, X1, X2, X3, Y1, Y2, Y3, Z1, Z2, Z3,
//     .        ZX1, ZX2, ZX3, ZY1, ZY2, ZY3, F, FX, FY
//C
//C***********************************************************
//C
//C                                               From SRFPACK
//C                                            Robert J. Renka
//C                                  Dept. of Computer Science
//C                                       Univ. of North Texas
//C                                           renka@cs.unt.edu
//C                                                   06/12/90
//C
//C   Given function values and first partial derivatives at
//C the vertices of a triangle, along with a point P in the
//C triangle, this subroutine computes an interpolated value
//C F(P) and, optionally, the first partial derivatives of F
//C at P.
//C
//C   The interpolant F of the vertex values and gradients is
//C the Clough-Tocher finite element.  F is cubic in each of
//C the three subtriangles of equal area obtained by joining
//C the vertices to the barycenter, but has only quadratic
//C precision (exact for values and partials from a quadratic
//C polynomial).  Along each triangle side, F is the Hermite
//C cubic interpolant of the endpoint values and tangential
//C gradient components, and the normal gradient component of
//C F varies linearly between the interpolated endpoint nor-
//C mal components.  Thus, since values and first partials on
//C a triangle side depend only on the endpoint data, the
//C method results in a C-1 interpolant over a triangulation.
//C Second derivatives are discontinuous across subtriangle
//C boundaries.
//C
//C   The computational procedure, due to Charles Lawson, has
//C the following operation counts:  62 adds, 54 multiplies,
//C 8 divides, and 6 compares for an interpolated value, and
//C 170 adds, 142 multiplies, 14 divides, and 6 compares for
//C both a value and a pair of first partial derivatives.
//C
//C On input:
//C
//C       X,Y = Coordinates of the point P at which F is to
//C             be evaluated.
//C
//C       X1,X2,X3 = X coordinates of the vertices of the tri-
//C                  angle in counterclockwise order.
//C
//C       Y1,Y2,Y3 = Y coordinates of the vertices of the tri-
//C                  angle in one-to-one correspondence with
//C                  X1, X2, and X3.
//C
//C       Z1,Z2,Z3 = Data values at the vertices (X1,Y1),
//C                  (X2,Y2), (X3,Y3), respectively.
//C
//C       ZX1,ZX2,ZX3 = X-derivative values at the vertices.
//C
//C       ZY1,ZY2,ZY3 = Y-derivative values at the vertices.
//C
//C       DFLAG = Logical flag which specifies whether first
//C               partial derivatives at P are to be computed:
//C               DFLAG = .TRUE. if and only if partials are
//C               to be returned.
//C
//C Input parameters are not altered by this routine.
//C
//C On output:
//C
//C       F = Value of the interpolatory function at P if
//C           IER = 0, or zero if IER = 1.  Note that, if
//C           P is not contained in the triangle, F is an
//C           extrapolated value.
//C
//C       FX,FY = Partial derivatives of F at P if DFLAG =
//C               .TRUE. and IER = 0, unaltered otherwise.
//C
//C       IER = Error indicator:
//C             IER = 0 if no errors were encountered.
//C             IER = 1 if the vertices of the triangle are
//C                     collinear.
//C
//C Modules required by TVAL:  None
//C
//C***********************************************************
//C
var
       I, IP1, IP2, IP3: longint;
//      INTEGER I, IP1, IP2, IP3
          A, AX, AY, B, BX, BY,
             C, CX, CY, FF, G,
             GX, GY, P, PHI, PHIX, PHIY,
             PX, PY, Q, QX, QY, R,
             RO, ROX, ROY, RX, RY, SL,
             U, V: array[0..2] of TFloat;
         AREA, C1, C2, RMIN, XP, YP: TFloat;
//      REAL    A(3), AREA, AX(3), AY(3), B(3), BX(3), BY(3),
//     .        C(3), CX(3), CY(3), C1, C2, FF(3), G(3),
//     .        GX(3), GY(3), P(3), PHI(3), PHIX(3), PHIY(3),
//     .        PX(3), PY(3), Q(3), QX(3), QY(3), R(3), RMIN,
//     .        RO(3), ROX(3), ROY(3), RX(3), RY(3), SL(3),
//     .        U(3), V(3), XP, YP
//C
//C Local parameters:
//C
//C A(K) =            Cardinal function whose coefficient is
//C                     Z(K)
//C AREA =            Twice the area of the triangle
//C AX(K),AY(K) =     X,Y partials of A(K) -- cardinal
//C                     functions for FX and FY
//C B(K) =            Twice the cardinal function whose
//C                     coefficient is ZX(K)
//C BX(K),BY(K) =     X,Y partials of B(K)
//C C(K) =            Twice the cardinal function whose
//C                     coefficient is ZY(K)
//C CX(K),CY(K) =     X,Y partials of C(K)
//C C1,C2 =           Factors for computing RO
//C FF(K) =           Factors for computing G, GX, and GY --
//C                     constant
//C G(K) =            Factors for computing the cardinal
//C                     functions -- cubic
//C GX(K),GY(K) =     X,Y partials of G(K)
//C I =               DO-loop index
//C IP1,IP2,IP3 =     Permuted indexes for computing RO, ROX,
//C                     and ROY
//C P(K) =            G(K) + PHI(K)
//C PHI(K)            R(K-1)*R(K+1) -- quadratic
//C PHIX(K),PHIY(K) = X,Y partials of PHI(K)
//C PX(K),PY(K) =     X,Y partials of P(K)
//C Q(K) =            G(K) - PHI(K)
//C QX(K),QY(K) =     X,Y partials of Q(K)
//C R(K) =            K-th barycentric coordinate
//C RMIN =            Min(R1,R2,R3)
//C RO(K) =           Factors for computing G -- cubic
//C                     correction terms
//C ROX(K),ROY(K) =   X,Y partials of RO(K)
//C RX(K),RY(K) =     X,Y partial derivatives of R(K)
//C SL(K) =           Square of the length of the side
//C                     opposite vertex K
//C U(K) =            X-component of the vector representing
//C                     the side opposite vertex K
//C V(K) =            Y-component of the vector representing
//C                     the side opposite vertex K
//C XP,YP =           X-X1, Y-Y1
//C
begin
      U[0] := X3 - X2;
      U[1] := X1 - X3;
      U[2] := X2 - X1;
//      U(1) = X3 - X2
//      U(2) = X1 - X3
//      U(3) = X2 - X1
//C
      V[0] := Y3 - Y2;
      V[1] := Y1 - Y3;
      V[2] := Y2 - Y1;
//      V(1) = Y3 - Y2
//      V(2) = Y1 - Y3
//      V(3) = Y2 - Y1
//C
      for I := 1 to 3 do
      begin
        SL[I-1] := U[I-1]*U[I-1] + V[I-1]*V[I-1]
      end;
//      DO 1 I = 1,3
//        SL(I) = U(I)*U(I) + V(I)*V(I)
//    1   CONTINUE
//C
//C AREA = 3->1 X 3->2.
//C
      AREA := U[0]*V[1] - U[1]*V[0];
      IF (AREA = 0.0) then
      begin
  //C The vertices are collinear.
  //C
        IER := 1;
        F := 0.0;
        Exit;
  //    9 IER = 1
  //      F = 0.
  //      RETURN
      end;
      IER := 0;
//      AREA = U(1)*V(2) - U(2)*V(1)
//      IF (AREA .EQ. 0.) GO TO 9
//      IER = 0
//C
//C R(1) = (2->3 X 2->P)/AREA, R(2) = (1->P X 1->3)/AREA,
//C   R(3) = (1->2 X 1->P)/AREA.
//C
      R[0] := (U[0]*(Y-Y2) - V[0]*(X-X2))/AREA;
      XP := X - X1;
      YP := Y - Y1;
      R[1] := (U[1]*YP - V[1]*XP)/AREA;
      R[2] := (U[2]*YP - V[2]*XP)/AREA;
//      R(1) = (U(1)*(Y-Y2) - V(1)*(X-X2))/AREA
//      XP = X - X1
//      YP = Y - Y1
//      R(2) = (U(2)*YP - V(2)*XP)/AREA
//      R(3) = (U(3)*YP - V(3)*XP)/AREA
//C
      PHI[0] := R[1]*R[2];
      PHI[1] := R[2]*R[0];
      PHI[2] := R[0]*R[1];
//      PHI(1) = R(2)*R(3)
//      PHI(2) = R(3)*R(1)
//      PHI(3) = R(1)*R(2)
//C
      IF (R[0] <= R[1])  and  (R[0] <= R[2]) then
      begin
//      IF (R(1) .GT. R(2)  .OR.  R(1) .GT. R(3)) GO TO 3
        RMIN := R[0];
        IP1 := 1;
        IP2 := 2;
        IP3 := 3;
//      RMIN = R(1)
//      IP1 = 1
//      IP2 = 2
//      IP3 = 3
//      GO TO 5
      end
      else IF (R[1] <= R[2]) then
      begin
//    3 IF (R(2) .GT. R(3)) GO TO 4
        RMIN := R[1];
        IP1 := 2;
        IP2 := 3;
        IP3 := 1;
//      RMIN = R(2)
//      IP1 = 2
//      IP2 = 3
//      IP3 = 1
//      GO TO 5
      end
      else
      begin
        RMIN := R[2];
        IP1 := 3;
        IP2 := 1;
        IP3 := 2;
//    4 RMIN = R(3)
//      IP1 = 3
//      IP2 = 1
//      IP3 = 2
      end;
//C
      C1 := RMIN*RMIN/2.0;
      C2 := RMIN/3.0;
      RO[IP1-1] := (PHI[IP1-1] + 5.0*C1/3.0)*R[IP1-1] - C1;
      RO[IP2-1] := C1*(R[IP3-1] - C2);
      RO[IP3-1] := C1*(R[IP2-1] - C2);
//    5 C1 = RMIN*RMIN/2.
//      C2 = RMIN/3.
//      RO(IP1) = (PHI(IP1) + 5.*C1/3.)*R(IP1) - C1
//      RO(IP2) = C1*(R(IP3) - C2)
//      RO(IP3) = C1*(R(IP2) - C2)
//C
      FF[0] := 3.0*(SL[1]-SL[2])/SL[0];
      FF[1] := 3.0*(SL[2]-SL[0])/SL[1];
      FF[2] := 3.0*(SL[0]-SL[1])/SL[2];
//      FF(1) = 3.*(SL(2)-SL(3))/SL(1)
//      FF(2) = 3.*(SL(3)-SL(1))/SL(2)
//      FF(3) = 3.*(SL(1)-SL(2))/SL(3)
//C
      G[0] := (R[1]-R[2])*PHI[0] + FF[0]*RO[0] - RO[1]+RO[2];
      G[1] := (R[2]-R[0])*PHI[1] + FF[1]*RO[1] - RO[2]+RO[0];
      G[2] := (R[0]-R[1])*PHI[2] + FF[2]*RO[2] - RO[0]+RO[1];
//      G(1) = (R(2)-R(3))*PHI(1) + FF(1)*RO(1) - RO(2)+RO(3)
//      G(2) = (R(3)-R(1))*PHI(2) + FF(2)*RO(2) - RO(3)+RO(1)
//      G(3) = (R(1)-R(2))*PHI(3) + FF(3)*RO(3) - RO(1)+RO(2)
//C
      for I := 1 to 3 do
      begin
        P[I-1] := G[I-1] + PHI[I-1];
        Q[I-1] := G[I-1] - PHI[I-1];
      end;
//      DO 6 I = 1,3
//        P(I) = G(I) + PHI(I)
//        Q(I) = G(I) - PHI(I)
//    6   CONTINUE
//C
      A[0] := R[0] + G[2] - G[1];
      A[1] := R[1] + G[0] - G[2];
      A[2] := R[2] + G[1] - G[0];
//      A(1) = R(1) + G(3) - G(2)
//      A(2) = R(2) + G(1) - G(3)
//      A(3) = R(3) + G(2) - G(1)
//C
      B[0] := U[2]*P[2] + U[1]*Q[1];
      B[1] := U[0]*P[0] + U[2]*Q[2];
      B[2] := U[1]*P[1] + U[0]*Q[0];
//      B(1) = U(3)*P(3) + U(2)*Q(2)
//      B(2) = U(1)*P(1) + U(3)*Q(3)
//      B(3) = U(2)*P(2) + U(1)*Q(1)
//C
      C[0] := V[2]*P[2] + V[1]*Q[1];
      C[1] := V[0]*P[0] + V[2]*Q[2];
      C[2] := V[1]*P[1] + V[0]*Q[0];
//      C(1) = V(3)*P(3) + V(2)*Q(2)
//      C(2) = V(1)*P(1) + V(3)*Q(3)
//      C(3) = V(2)*P(2) + V(1)*Q(1)
//C
//C F is a linear combination of the cardinal functions.
//C
      F := A[0]*Z1 + A[1]*Z2 + A[2]*Z3 + (B[0]*ZX1 + B[1]*ZX2
         + B[2]*ZX3 + C[0]*ZY1 + C[1]*ZY2 + C[2]*ZY3)/2.0;
      IF (NOT DFLAG) then Exit;
//      F = A(1)*Z1 + A(2)*Z2 + A(3)*Z3 + (B(1)*ZX1 + B(2)*ZX2
//     .    + B(3)*ZX3 + C(1)*ZY1 + C(2)*ZY2 + C(3)*ZY3)/2.
//      IF (.NOT. DFLAG) RETURN
//C
//C Compute FX and FY.
//C
      for I := 1 to 3 do
      begin
        RX[I-1] := -V[I-1]/AREA;
        RY[I-1] := U[I-1]/AREA;
      end;
//      DO 7 I = 1,3
//        RX(I) = -V(I)/AREA
//        RY(I) = U(I)/AREA
//    7   CONTINUE
//C
      PHIX[0] := R[1]*RX[2] + RX[1]*R[2];
      PHIY[0] := R[1]*RY[2] + RY[1]*R[2];
      PHIX[1] := R[2]*RX[0] + RX[2]*R[0];
      PHIY[1] := R[2]*RY[0] + RY[2]*R[0];
      PHIX[2] := R[0]*RX[1] + RX[0]*R[1];
      PHIY[2] := R[0]*RY[1] + RY[0]*R[1];
//      PHIX(1) = R(2)*RX(3) + RX(2)*R(3)
//      PHIY(1) = R(2)*RY(3) + RY(2)*R(3)
//      PHIX(2) = R(3)*RX(1) + RX(3)*R(1)
//      PHIY(2) = R(3)*RY(1) + RY(3)*R(1)
//      PHIX(3) = R(1)*RX(2) + RX(1)*R(2)
//      PHIY(3) = R(1)*RY(2) + RY(1)*R(2)
//C
      ROX[IP1-1] := RX[IP1-1]*(PHI[IP1-1] + 5.*C1) +
                R[IP1-1]*(PHIX[IP1-1] - RX[IP1-1]);
      ROY[IP1-1] := RY[IP1-1]*(PHI[IP1-1] + 5.*C1) +
                R[IP1-1]*(PHIY[IP1-1] - RY[IP1-1]);
      ROX[IP2-1] := RX[IP1-1]*(PHI[IP2-1] - C1) + C1*RX[IP3-1];
      ROY[IP2-1] := RY[IP1-1]*(PHI[IP2-1] - C1) + C1*RY[IP3-1];
      ROX[IP3-1] := RX[IP1-1]*(PHI[IP3-1] - C1) + C1*RX[IP2-1];
      ROY[IP3-1] := RY[IP1-1]*(PHI[IP3-1] - C1) + C1*RY[IP2-1];
//      ROX(IP1) = RX(IP1)*(PHI(IP1) + 5.*C1) +
//     .           R(IP1)*(PHIX(IP1) - RX(IP1))
//      ROY(IP1) = RY(IP1)*(PHI(IP1) + 5.*C1) +
//     .           R(IP1)*(PHIY(IP1) - RY(IP1))
//      ROX(IP2) = RX(IP1)*(PHI(IP2) - C1) + C1*RX(IP3)
//      ROY(IP2) = RY(IP1)*(PHI(IP2) - C1) + C1*RY(IP3)
//      ROX(IP3) = RX(IP1)*(PHI(IP3) - C1) + C1*RX(IP2)
//      ROY(IP3) = RY(IP1)*(PHI(IP3) - C1) + C1*RY(IP2)
//C
      GX[0] := (RX[1] - RX[2])*PHI[0] + (R[1] - R[2])*PHIX[0]
             + FF[0]*ROX[0] - ROX[1] + ROX[2];
      GY[0] := (RY[1] - RY[2])*PHI[0] + (R[1] - R[2])*PHIY[0]
             + FF[0]*ROY[0] - ROY[1] + ROY[2];
      GX[1] := (RX[2] - RX[0])*PHI[1] + (R[2] - R[0])*PHIX[1]
             + FF[1]*ROX[1] - ROX[2] + ROX[0];
      GY[1] := (RY[2] - RY[0])*PHI[1] + (R[2] - R[0])*PHIY[1]
             + FF[1]*ROY[1] - ROY[2] + ROY[0];
      GX[2] := (RX[0] - RX[1])*PHI[2] + (R[0] - R[1])*PHIX[2]
             + FF[2]*ROX[2] - ROX[0] + ROX[1];
      GY[2] := (RY[0] - RY[1])*PHI[2] + (R[0] - R[1])*PHIY[2]
             + FF[2]*ROY[2] - ROY[0] + ROY[1];
//      GX(1) = (RX(2) - RX(3))*PHI(1) + (R(2) - R(3))*PHIX(1)
//     .        + FF(1)*ROX(1) - ROX(2) + ROX(3)
//      GY(1) = (RY(2) - RY(3))*PHI(1) + (R(2) - R(3))*PHIY(1)
//     .        + FF(1)*ROY(1) - ROY(2) + ROY(3)
//      GX(2) = (RX(3) - RX(1))*PHI(2) + (R(3) - R(1))*PHIX(2)
//     .        + FF(2)*ROX(2) - ROX(3) + ROX(1)
//      GY(2) = (RY(3) - RY(1))*PHI(2) + (R(3) - R(1))*PHIY(2)
//     .        + FF(2)*ROY(2) - ROY(3) + ROY(1)
//      GX(3) = (RX(1) - RX(2))*PHI(3) + (R(1) - R(2))*PHIX(3)
//     .        + FF(3)*ROX(3) - ROX(1) + ROX(2)
//      GY(3) = (RY(1) - RY(2))*PHI(3) + (R(1) - R(2))*PHIY(3)
//     .        + FF(3)*ROY(3) - ROY(1) + ROY(2)
//C
      for I := 1 to 3 do
      begin
        PX[I-1] := GX[I-1] + PHIX[I-1];
        PY[I-1] := GY[I-1] + PHIY[I-1];
        QX[I-1] := GX[I-1] - PHIX[I-1];
        QY[I-1] := GY[I-1] - PHIY[I-1];
      end;
//      DO 8 I = 1,3
//        PX(I) = GX(I) + PHIX(I)
//        PY(I) = GY(I) + PHIY(I)
//        QX(I) = GX(I) - PHIX(I)
//        QY(I) = GY(I) - PHIY(I)
//    8   CONTINUE
//C
      AX[0] := RX[0] + GX[2] - GX[1];
      AY[0] := RY[0] + GY[2] - GY[1];
      AX[1] := RX[1] + GX[0] - GX[2];
      AY[1] := RY[1] + GY[0] - GY[2];
      AX[2] := RX[2] + GX[1] - GX[0];
      AY[2] := RY[2] + GY[1] - GY[0];
//      AX(1) = RX(1) + GX(3) - GX(2)
//      AY(1) = RY(1) + GY(3) - GY(2)
//      AX(2) = RX(2) + GX(1) - GX(3)
//      AY(2) = RY(2) + GY(1) - GY(3)
//      AX(3) = RX(3) + GX(2) - GX(1)
//      AY(3) = RY(3) + GY(2) - GY(1)
//C
      BX[0] := U[2]*PX[2] + U[1]*QX[1];
      BY[0] := U[2]*PY[2] + U[1]*QY[1];
      BX[1] := U[0]*PX[0] + U[2]*QX[2];
      BY[1] := U[0]*PY[0] + U[2]*QY[2];
      BX[2] := U[1]*PX[1] + U[0]*QX[0];
      BY[2] := U[1]*PY[1] + U[0]*QY[0];
//      BX(1) = U(3)*PX(3) + U(2)*QX(2)
//      BY(1) = U(3)*PY(3) + U(2)*QY(2)
//      BX(2) = U(1)*PX(1) + U(3)*QX(3)
//      BY(2) = U(1)*PY(1) + U(3)*QY(3)
//      BX(3) = U(2)*PX(2) + U(1)*QX(1)
//      BY(3) = U(2)*PY(2) + U(1)*QY(1)
//C
      CX[0] := V[2]*PX[2] + V[1]*QX[1];
      CY[0] := V[2]*PY[2] + V[1]*QY[1];
      CX[1] := V[0]*PX[0] + V[2]*QX[2];
      CY[1] := V[0]*PY[0] + V[2]*QY[2];
      CX[2] := V[1]*PX[1] + V[0]*QX[0];
      CY[2] := V[1]*PY[1] + V[0]*QY[0];
//      CX(1) = V(3)*PX(3) + V(2)*QX(2)
//      CY(1) = V(3)*PY(3) + V(2)*QY(2)
//      CX(2) = V(1)*PX(1) + V(3)*QX(3)
//      CY(2) = V(1)*PY(1) + V(3)*QY(3)
//      CX(3) = V(2)*PX(2) + V(1)*QX(1)
//      CY(3) = V(2)*PY(2) + V(1)*QY(1)
//C
//C FX and FY are linear combinations of the cardinal
//C   functions.
//C
      FX := AX[0]*Z1 + AX[1]*Z2 + AX[2]*Z3 + (BX[0]*ZX1 +
          BX[1]*ZX2 + BX[2]*ZX3 + CX[0]*ZY1 + CX[1]*ZY2 +
          CX[2]*ZY3)/2.0;
      FY := AY[0]*Z1 + AY[1]*Z2 + AY[2]*Z3 + (BY[0]*ZX1 +
          BY[1]*ZX2 + BY[2]*ZX3 + CY[0]*ZY1 + CY[1]*ZY2 +
          CY[2]*ZY3)/2.0;
//      FX = AX(1)*Z1 + AX(2)*Z2 + AX(3)*Z3 + (BX(1)*ZX1 +
//     .     BX(2)*ZX2 + BX(3)*ZX3 + CX(1)*ZY1 + CX(2)*ZY2 +
//     .     CX(3)*ZY3)/2.
//      FY = AY(1)*Z1 + AY(2)*Z2 + AY(3)*Z3 + (BY(1)*ZX1 +
//     .     BY(2)*ZX2 + BY(3)*ZX3 + CY(1)*ZY1 + CY(2)*ZY2 +
//     .     CY(3)*ZY3)/2.
//      RETURN
//C
//C The vertices are collinear.
//C
//    9 IER = 1
//      F = 0.
//      RETURN
//      END
end;

procedure UNIF (const NCC: longint; const LCC: TNcmaxIntArray; const N: longint;
  const X,Y,Z: TNmaxSingleArray; const GRAD: TNtnxSingleArray;
  const LIST,LPTR: TN6IntArray; const LEND: TNmaxIntArray; const IFLGS: longint;
  const SIGMA: TN6SingleArray; const NROW,NX,NY: longint; const PX,PY: TNiSingleArray;
  const SFLAG: longbool; var SVAL: TFloat; var ZZ: TNi_NjSingleArray; var IER: longint);
//      SUBROUTINE UNIF (NCC,LCC,N,X,Y,Z,GRAD,LIST,LPTR,LEND,
//     .                 IFLGS,SIGMA,NROW,NX,NY,PX,PY,SFLAG,
//     .                 SVAL, ZZ,IER)
//      INTEGER NCC, LCC(*), N, LIST(*), LPTR(*), LEND(N),
//     .        IFLGS, NROW, NX, NY, IER
//      LOGICAL SFLAG
//      REAL    X(N), Y(N), Z(N), GRAD(2,N), SIGMA(*), PX(NX),
//     .        PY(NY), SVAL, ZZ(NROW,NY)
//C
//C***********************************************************
//C
//C                                               From SRFPACK
//C                                            Robert J. Renka
//C                                  Dept. of Computer Science
//C                                       Univ. of North Texas
//C                                           renka@cs.unt.edu
//C                                                   02/22/97
//C
//C   Given a Delaunay triangulation of a set of points in the
//C plane with associated data values and gradients, this sub-
//C routine interpolates the data to a set of rectangular grid
//C points for such applications as contouring.  Extrapolation
//C is performed at grid points exterior to the triangulation,
//C and the interpolant is once-continuously differentiable
//C over the entire plane.  Refer to Subroutine INTRC1 for
//C further details.
//C
//C On input:
//C
//C       NCC = Number of constraint curves (refer to TRIPACK
//C             Subroutine ADDCST).  NCC .GE. 0.
//C
//C       LCC = Array of length NCC (or dummy array of length
//C             1 if NCC = 0) containing the index of the
//C             first node of constraint I in LCC(I).  For I =
//C             1 to NCC, LCC(I+1)-LCC(I) .GE. 3, where
//C             LCC(NCC+1) = N+1.
//C
//C       N = Number of nodes in the triangulation.  N .GE. 3.
//C
//C       X,Y = Arrays of length N containing the coordinates
//C             of the nodes with non-constraint nodes in the
//C             first LCC(1)-1 locations, followed by NCC se-
//C             quences of constraint nodes.
//C
//C       Z = Array of length N containing data values at the
//C           nodes.  Refer to Subroutines ZGRADG and ZGRADL.
//C
//C       GRAD = 2 by N array whose columns contain estimated
//C              gradients at the nodes with X partial deriva-
//C              tives in the first row and Y partials in the
//C              second.  Refer to Subroutines GRADC, GRADG,
//C              GRADL, SMSURF, ZGRADG, and ZGRADL.
//C
//C       LIST,LPTR,LEND = Data structure defining the trian-
//C                        gulation.  Refer to TRIPACK
//C                        Subroutine TRMESH.
//C
//C       IFLGS = Tension factor option:
//C               IFLGS .LE. 0 if a single uniform tension
//C                            factor is to be used.
//C               IFLGS .GE. 1 if variable tension is desired.
//C
//C       SIGMA = Uniform tension factor (IFLGS .LE. 0), or
//C               array containing tension factors associated
//C               with arcs in one-to-one correspondence with
//C               LIST entries (IFLGS .GE. 1).  Refer to Sub-
//C               routines FVAL, GETSIG, SIG0, SIG1, and SIG2.
//C
//C       NROW = Number of rows in the dimension statement of
//C              ZZ.
//C
//C       NX,NY = Number of rows and columns, respectively, in
//C               the rectangular grid.  1 .LE. NX .LE. NROW,
//C               and 1 .LE. NY.
//C
//C       PX,PY = Arrays of length NX and NY, respectively,
//C               containing the coordinates of the grid
//C               lines.
//C
//C       SFLAG = Special value flag:
//C               SFLAG = .FALSE. if special values are not to
//C                               be used (ZZ contains only
//C                               interpolated or extrapolated
//C                               values.
//C               SFLAG = .TRUE. if SVAL is to be stored in ZZ
//C                              elements corresponding to
//C                              grid points which lie in a
//C                              constraint region.
//C
//C       SVAL = Special value for grid points lying in a con-
//C              straint region, or dummy parameter if SFLAG =
//C              .FALSE.
//C
//C The above parameters are not altered by this routine.
//C
//C       ZZ = NROW by NCOL array for some NCOL .GE. NY.
//C
//C On output:
//C
//C       ZZ = Interpolated values at the grid points (or
//C            special values) if IER .GE. 0.  ZZ(I,J) =
//C            F(PX(I),PY(J)) for I = 1,...,NX and J = 1,...,
//C            NY, where F is the interpolatory surface.
//C
//C       IER = Error indicator:
//C             IER .GE. 0 if no errors were encountered.
//C                        IER contains the number of grid
//C                        points exterior to the triangula-
//C                        tion or contained in a constraint
//C                        region triangle (extrapolated
//C                        values).
//C             IER = -1 if NCC, N, NROW, NX, or NY is
//C                      outside its valid range on input.
//C                      LCC is not tested for validity.
//C             IER = -2 if the nodes are collinear or the
//C                      triangulation is invalid.
//C
//C TRIPACK modules required by UNIF:  CRTRI, JRAND, LEFT,
//C                                      LSTPTR, TRFIND
//C
//C SRFPACK modules required by UNIF:  ARCINT, COORDS, FVAL,
//C                                      INTRC1, SNHCSH, TVAL
//C
//C***********************************************************
//C
var
       I, IERR, IST, J, NEX, NI, NJ, NST: longint;
       DFLAG, SFL: longbool;
          DUM: TFloat;
//      INTEGER I, IERR, IST, J, NEX, NI, NJ, NST
//      LOGICAL DFLAG, SFL
//      REAL    DUM
begin
  DFLAG := False;
  NST := 1;
//      DATA    DFLAG/.FALSE./,  NST/1/
//C
//C Local parameters:
//C
//C DFLAG = Derivative flag for INTRC1
//C DUM =   Dummy INTRC1 parameter
//C I,J =   DO-loop indexes
//C IERR =  Error flag for calls to INTRC1
//C IST =   Parameter for INTRC1
//C NEX =   Number of grid points exterior to the triangula-
//C           tion boundary (number of extrapolated values)
//C NI,NJ = Local copies of NX and NY
//C NST =   Initial value for IST
//C SFL =   Local copy of SFLAG
//C
      NI := NX;
      NJ := NY;
      IF (NCC < 0)  OR  (N < 3)  OR  (NI < 1)  OR
         (NI > NROW)  OR  (NJ < 1) then
      begin
  //C Invalid input parameter.
  //C
        IER := -1;
        Exit;
  //    3 IER = -1
  //      RETURN
      end;
      SFL := SFLAG;
      IST := NST;
//      NI = NX
//      NJ = NY
//      IF (NCC .LT. 0  .OR.  N .LT. 3  .OR.  NI .LT. 1  .OR.
//     .    NI .GT. NROW  .OR.  NJ .LT. 1) GO TO 3
//      SFL = SFLAG
//      IST = NST
//C
//C Compute interpolated values.
//C
      NEX := 0;
//      NEX = 0
      for J := 1 to NJ do
      begin
        for I := 1 to NI do
        begin
           INTRC1 (PX[I-1],PY[J-1],NCC,LCC,N,X,Y,Z,LIST,
                      LPTR,LEND,IFLGS,SIGMA,GRAD,
                      DFLAG, IST, ZZ[J-1,I-1],DUM,DUM,IERR);
          IF (IERR < 0) then
          begin
      //C Triangulation nodes are collinear.
      //C
            IER := -2;
            Exit;
      //    4 IER = -2
      //      RETURN
          end;
          IF (IERR > 0) then NEX := NEX + 1;
          IF (SFL  AND (IERR = 1)) then ZZ[J-1,I-1] := SVAL;
        end;
      end;
//      DO 2 J = 1,NJ
//        DO 1 I = 1,NI
//          CALL INTRC1 (PX(I),PY(J),NCC,LCC,N,X,Y,Z,LIST,
//     .                 LPTR,LEND,IFLGS,SIGMA,GRAD,
//     .                 DFLAG, IST, ZZ(I,J),DUM,DUM,IERR)
//          IF (IERR .LT. 0) GO TO 4
//          IF (IERR .GT. 0) NEX = NEX + 1
//          IF (SFL  .AND. IERR .EQ. 1) ZZ(I,J) = SVAL
//    1     CONTINUE
//    2   CONTINUE
      IER := NEX;
//      IER = NEX
//      RETURN
//C
//C Invalid input parameter.
//C
//    3 IER = -1
//      RETURN
//C
//C Triangulation nodes are collinear.
//C
//    4 IER = -2
//      RETURN
//      END
end;

FUNCTION VOLUME (const NCC: longint; const LCC: TNcmaxIntArray; const N: longint;
  const X,Y,Z: TNmaxSingleArray; const LIST,LPTR: TN6IntArray;
  const LEND: TNmaxIntArray): TFloat;
//      REAL FUNCTION VOLUME (NCC,LCC,N,X,Y,Z,LIST,LPTR,LEND)
//      INTEGER NCC, LCC(*), N, LIST(*), LPTR(*), LEND(N)
//      REAL    X(N), Y(N), Z(N)
//C
//C***********************************************************
//C
//C                                               From SRFPACK
//C                                            Robert J. Renka
//C                                  Dept. of Computer Science
//C                                       Univ. of North Texas
//C                                           renka@cs.unt.edu
//C                                                   08/26/91
//C
//C   Given a triangulation of a set of N nodes, along with
//C data values at the nodes, this function computes the int-
//C egral over a region R of the piecewise linear interpolant
//C of the data values.  R is the convex hull of the nodes
//C with constraint regions excluded.
//C
//C On input:
//C
//C       NCC = Number of constraint curves (refer to TRIPACK
//C             Subroutine ADDCST).  NCC .GE. 0.
//C
//C       LCC = Array of length NCC (or dummy array of length
//C             1 if NCC = 0) containing the index of the
//C             first node of constraint I in LCC(I).  For I =
//C             1 to NCC, LCC(I+1)-LCC(I) .GE. 3, where
//C             LCC(NCC+1) = N+1.
//C
//C       N = Number of nodes in the triangulation.  N .GE. 3.
//C
//C       X,Y = Arrays of length N containing the coordinates
//C             of the nodes with non-constraint nodes in the
//C             first LCC(1)-1 locations, followed by NCC se-
//C             quences of constraint nodes.
//C
//C       Z = Array of length N containing data values at the
//C           nodes.  Refer to Subroutine ZGRADL.
//C
//C       LIST,LPTR,LEND = Data structure defining the trian-
//C                        gulation.  Refer to TRIPACK
//C                        Subroutine TRMESH.
//C
//C Input parameters are not altered by this function.
//C
//C On output:
//C
//C       VOLUME = Sum of the volumes of the linear interpo-
//C                lants on the non-constraint triangles, or
//C                zero if a parameter is outside its valid
//C                range on input.
//C
//C SRFPACK module required by VOLUME:  TRVOL
//C
//C Intrinsic function called by VOLUME:  ABS
//C
//C***********************************************************
//C
//      REAL    TRVOL
var
       I, ILAST, LCC1, LP2, LP3, LPL, N1, N2, N3,
             NM2, NN: longint;
          SUM, XN1, YN1, ZN1: TFloat;
//      INTEGER I, ILAST, LCC1, LP2, LP3, LPL, N1, N2, N3,
//     .        NM2, NN
//      REAL    SUM, XN1, YN1, ZN1
//C
//C Test for invalid input parameters.
begin
//C
      IF (NCC < 0) then
      begin
        result := 0;
        Exit;
      end;
//      IF (NCC .LT. 0) GO TO 5
      NN := N;
      LCC1 := NN+1;
//      NN = N
//      LCC1 = NN+1
      IF (NCC = 0) THEN
      begin
        IF (NN < 3) then
        begin
          result := 0;
          Exit;
        end;
      end
      ELSE
      begin
        for I := NCC downto 1 do
        begin
          IF (LCC1-LCC[I-1] < 3) then
          begin
            result := 0;
            Exit;
          end;
          LCC1 := LCC[I-1];
        end;
        IF (LCC1 < 1) then
        begin
          result := 0;
          Exit;
        end;
      END;
//      IF (NCC .EQ. 0) THEN
//        IF (NN .LT. 3) GO TO 5
//      ELSE
//        DO 1 I = NCC,1,-1
//          IF (LCC1-LCC(I) .LT. 3) GO TO 5
//          LCC1 = LCC(I)
//    1     CONTINUE
//        IF (LCC1 .LT. 1) GO TO 5
//      ENDIF
//C
//C Initialize for loop on triangles (N1,N2,N3) such that N2
//C   and N3 have larger indexes than N1.  SUM contains the
//C   accumulated volume, I is the index of the constraint
//C   containing N1 if N1 is a constraint node, and ILAST is
//C   the last node of constraint I.
//C
      I := 0;
      ILAST := LCC1 - 1;
      SUM := 0.0;
      NM2 := NN - 2;
//      I = 0
//      ILAST = LCC1 - 1
//      SUM = 0.
//      NM2 = NN - 2
      for N1 := 1 to NM2 do
      begin
//      DO 4 N1 = 1,NM2
        XN1 := X[N1-1];
        YN1 := Y[N1-1];
        ZN1 := Z[N1-1];
//        XN1 = X(N1)
//        YN1 = Y(N1)
//        ZN1 = Z(N1)
        IF (N1 > ILAST) THEN
        begin
          I := I + 1;
          IF (I < NCC) THEN
          begin
            ILAST := LCC[I] - 1;
          end
          ELSE
          begin
            ILAST := NN
          END;
        END;
//        IF (N1 .GT. ILAST) THEN
//          I = I + 1
//          IF (I .LT. NCC) THEN
//            ILAST = LCC(I+1) - 1
//          ELSE
//            ILAST = NN
//          ENDIF
//        ENDIF
//C
//C Top of loop on neighbors of N1.
//C
        LPL := LEND[N1-1];
        LP2 := LPL;
//        LPL = LEND(N1)
//        LP2 = LPL
        repeat
          LP2 := LPTR[LP2-1];
          N2 := LIST[LP2-1];
          LP3 := LPTR[LP2-1];
          N3 := ABS(LIST[LP3-1]);
          IF (N2 < N1)  OR  (N3 < N1) then
          begin
            Continue;
          end;

//    2   LP2 = LPTR(LP2)
//          N2 = LIST(LP2)
//          LP3 = LPTR(LP2)
//          N3 = ABS(LIST(LP3))
//          IF (N2 .LT. N1  .OR.  N3 .LT. N1) GO TO 3
//C
//C   (N1,N2,N3) lies in a constraint region iff the vertices
//C     are nodes of the same constraint and N2 < N3.
//C

          IF (N1 < LCC1)  OR  (N2 > N3)  OR
             (N3 > ILAST) THEN
          begin
            SUM := SUM + TRVOL(XN1,X[N2-1],X[N3-1],YN1,Y[N2-1],
                             Y[N3-1],ZN1,Z[N2-1],Z[N3-1]);
          END;
//          IF (N1 .LT. LCC1  .OR.  N2 .GT. N3  .OR.
//     .        N3 .GT. ILAST) THEN
//            SUM = SUM + TRVOL(XN1,X(N2),X(N3),YN1,Y(N2),
//     .                        Y(N3),ZN1,Z(N2),Z(N3))
//          ENDIF
//C
//C   Bottom of loop on neighbors.
//C
//    3     IF (LP2 .NE. LPL) GO TO 2

        until LP2 = LPL;
//    4   CONTINUE
      end;

//C
      result := Sum;
//      VOLUME = SUM
//      RETURN
//C
//C Invalid input parameter.
//C
//    5 VOLUME = 0.
//      RETURN
//      END
end;

procedure ZGRADG (const NCC: longint; const LCC: TNcmaxIntArray; const N: longint;
  const X,Y: TNmaxSingleArray; const LIST,LPTR: TN6IntArray; const LEND: TNmaxIntArray;
  const IFLGS: longint; const SIGMA: TN6SingleArray; var NIT: longint;
  var DZMAX : TFloat; var Z: TNmaxSingleArray;
  var GRAD: TNtnxSingleArray; var IER: longint);
//      SUBROUTINE ZGRADG (NCC,LCC,N,X,Y,LIST,LPTR,LEND,
//     .                   IFLGS,SIGMA, NIT,DZMAX,Z,
//     .                   GRAD, IER)
//      INTEGER NCC, LCC(*), N, LIST(*), LPTR(*), LEND(N),
//     .        IFLGS, NIT, IER
//      REAL    X(N), Y(N), SIGMA(*), DZMAX, Z(N), GRAD(2,N)
//C
//C***********************************************************
//C
//C                                               From SRFPACK
//C                                            Robert J. Renka
//C                                  Dept. of Computer Science
//C                                       Univ. of North Texas
//C                                           renka@cs.unt.edu
//C                                                   02/22/97
//C
//C   Given a triangulation of N nodes, along with data values
//C at non-constraint nodes, this subroutine employs a global
//C method to compute estimated gradients at the nodes and
//C approximate data values at the constraint nodes.  For
//C NCN = N-LCC(1)+1 constraint nodes, the method consists of
//C minimizing a quadratic functional Q(U) over vectors U of
//C length 2N+NCN containing gradients and values at con-
//C straint nodes.  Q is taken to be the sum over the
//C triangulation arcs, excluding interior constraint arcs,
//C of the linearized curvature (integral of squared second
//C derivative) of the Hermite interpolatory tension spline
//C defined by the data values and tangential gradient comp-
//C onents at the endpoints of the arc.
//C
//C   This minimization problem corresponds to a symmetric
//C positive definite sparse linear system which is solved by
//C a block Gauss-Seidel method with N blocks of order 2, or
//C order 3 for constraint nodes.
//C
//C   An alternative method (Subroutine ZGRADL) computes a
//C local approximation to the gradient and data value (if not
//C specified) at a single node.  The relative speed and
//C accuracy of the two methods depends on the distribution
//C of the nodes.  Relative accuracy also depends on the data
//C values.
//C
//C   Note that the call to ZGRADG can be followed by a call
//C to GRADG in order to compute improved gradient estimates
//C with fixed data values.  This is recommended for improved
//C efficiency.
//C
//C On input:
//C
//C       NCC = Number of constraint curves (refer to TRIPACK
//C             Subroutine ADDCST).  NCC > 0.
//C
//C       LCC = Array of length NCC containing the index of
//C             the first node of constraint I in LCC(I).  For
//C             I = 1 to NCC, LCC(I+1)-LCC(I) .GE. 3, where
//C             LCC(NCC+1) = N+1, and LCC(1) .GE. 4.
//C
//C       N = Number of nodes in the triangulation.
//C
//C       X,Y = Arrays of length N containing the coordinates
//C             of the nodes with non-constraint nodes in the
//C             first LCC(1)-1 locations, followed by NCC se-
//C             quences of constraint nodes.
//C
//C       LIST,LPTR,LEND = Data structure defining the trian-
//C                        gulation.  Refer to TRIPACK
//C                        Subroutine TRMESH.
//C
//C       IFLGS = Tension factor option:
//C               IFLGS .LE. 0 if a single uniform tension
//C                            factor is to be used.
//C               IFLGS .GE. 1 if variable tension is desired.
//C
//C       SIGMA = Uniform tension factor (IFLGS .LE. 0), or
//C               array containing tension factors associated
//C               with arcs in one-to-one correspondence with
//C               LIST entries (IFLGS .GE. 1).  Refer to Sub-
//C               routines GETSIG, SIG0, SIG1, and SIG2.
//C
//C The above parameters are not altered by this routine.
//C
//C       NIT = Maximum number of Gauss-Seidel iterations to
//C             be employed.  This maximum will likely be
//C             achieved if DZMAX is smaller than the machine
//C             precision.  Note that complete convergence is
//C             not necessary to achieve maximum accuracy of
//C             the interpolant.  NIT > 0.
//C
//C       DZMAX - Nonnegative convergence criterion.  The
//C               method is terminated when the maximum change
//C               in a solution Z-component between iterations
//C               is at most DZMAX.  The change in a solution
//C               component is taken to be the magnitude of
//C               the difference relative to 1 plus the magni-
//C               tude of the previous value.
//C
//C       Z = Array of length N containing data values in the
//C           first LCC(1)-1 locations and initial solution
//C           estimates in the remaining locations.  Zeros are
//C           sufficient, but Subroutine ZINIT may be called
//C           to provide better initial estimates.
//C
//C       GRAD = 2 by N array whose columns contain initial
//C              estimates of the gradients with X partial
//C              derivatives in the first row, Y partials in
//C              the second.  Zeros are sufficient.
//C
//C On output:
//C
//C       NIT = Number of Gauss-Seidel iterations employed.
//C
//C       DZMAX = Maximum relative change in a solution Z-
//C               component at the last iteration.
//C
//C       Z = Array updated with approximate data values in
//C           the last NCN = N-LCC(1)+1 locations if IER .GE.
//C           0.  Z is not altered if IER = -1.
//C
//C       GRAD = Estimated gradients at the nodes if IER .GE.
//C              0.  GRAD is not altered if IER = -1.
//C
//C       IER = Error indicator:
//C             IER = 0 if no errors were encountered and the
//C                     convergence criterion was achieved.
//C             IER = 1 if no errors were encountered but con-
//C                     vergence was not achieved within NIT
//C                     iterations.
//C             IER = -1 if NCC, an LCC entry, N, NIT, or
//C                      DZMAX is outside its valid range
//C                      on input.
//C             IER = -2 if all nodes are collinear or the
//C                      triangulation data structure is in-
//C                      valid.
//C             IER = -3 if duplicate nodes were encountered.
//C
//C SRFPACK modules required by ZGRADG:  GRCOEF, SNHCSH
//C
//C Intrinsic functions called by ZGRADG:  ABS, MAX, SQRT
//C
//C***********************************************************
//C
var
       I, IFL, IFRST, ILAST, ITER, J, JN, K, KBAK,
             KFOR, LCC1, LP, LPF, LPJ, LPL, LPN, MAXIT, NB,
             NN: longint;
          A11, A12, A13, A22, A23, A33, AREAJ, AREAN,
             AREAP, D, DCUB, DF, DSQ, DX, DXS, DY, DYS, DZ,
             DZJ, DZK, DZMX, DZX, DZY, R1, R2, R3, SDF,
             SIG, T, TOL, W, XK, YK, ZK, ZXK, ZYK: TFloat;
  GoTo5, GoT06: Boolean;
//      INTEGER I, IFL, IFRST, ILAST, ITER, J, JN, K, KBAK,
//     .        KFOR, LCC1, LP, LPF, LPJ, LPL, LPN, MAXIT, NB,
//     .        NN
//      REAL    A11, A12, A13, A22, A23, A33, AREAJ, AREAN,
//     .        AREAP, D, DCUB, DF, DSQ, DX, DXS, DY, DYS, DZ,
//     .        DZJ, DZK, DZMX, DZX, DZY, R1, R2, R3, SDF,
//     .        SIG, T, TOL, W, XK, YK, ZK, ZXK, ZYK
begin
//C
      NN := N;
      IFL := IFLGS;
      MAXIT := NIT;
      TOL := DZMAX;
//      NN = N
//      IFL = IFLGS
//      MAXIT = NIT
//      TOL = DZMAX
//C
//C Test for errors in input parameters.
//C
      IF (NCC <= 0)  OR  (MAXIT < 1)  OR  (TOL < 0.0) then
      begin
  //C Invalid input parameter.
  //C
        NIT := 0;
        DZMAX := 0.0;
        IER := -1;
        Exit;
  //    9 NIT = 0
  //      DZMAX = 0.
  //      IER = -1
  //      RETURN
      end;
      LCC1 := NN+1;
      for I := NCC downto 1 do
      begin
        IF (LCC1-LCC[I-1] < 3) then
        begin
    //C Invalid input parameter.
    //C
          NIT := 0;
          DZMAX := 0.0;
          IER := -1;
          Exit;
    //    9 NIT = 0
    //      DZMAX = 0.
    //      IER = -1
    //      RETURN
        end;
        LCC1 := LCC[I-1];
      end;

      IF (LCC1 < 4) then
      begin
  //C Invalid input parameter.
  //C
        NIT := 0;
        DZMAX := 0.0;
        IER := -1;
        Exit;
  //    9 NIT = 0
  //      DZMAX = 0.
  //      IER = -1
  //      RETURN
      end;
//      IF (NCC .LE. 0  .OR.  MAXIT .LT. 1  .OR.  TOL .LT. 0.)
//     .  GO TO 9
//      LCC1 = NN+1
//      DO 1 I = NCC,1,-1
//        IF (LCC1-LCC(I) .LT. 3) GO TO 9
//        LCC1 = LCC(I)
//    1   CONTINUE
//      IF (LCC1 .LT. 4) GO TO 9
//C
//C Initialize iteration count and SIG (overwritten if
//C   IFLGS > 0).
//C
      ITER := 0;
      SIG := SIGMA[0];
//      ITER = 0
//      SIG = SIGMA(1)
//C
//C Top of iteration loop:  If K is a constraint node, I
//C   indexes the constraint containing node K, IFRST and
//C   ILAST are the first and last nodes of constraint I, and
//C   (KBAK,K,KFOR) is a subsequence of constraint I.
//C
      repeat
        IF (ITER = MAXIT) then
        begin
  //C Method failed to converge within NIT iterations.
  //C
          DZMAX := DZMX ;
          IER := 1;
          Exit;
  //    8 DZMAX = DZMX
  //      IER = 1
  //      RETURN
        end;
  //    2 IF (ITER .EQ. MAXIT) GO TO 8
        DZMX := 0.0;
        I := 0;
        ILAST := LCC1-1;
        KBAK := 0;
        KFOR := 0;
  //      DZMX = 0.
  //      I = 0
  //      ILAST = LCC1-1
  //      KBAK = 0
  //      KFOR = 0
  //C
  //C Loop on nodes.
  //C
        for K := 1 to NN do
        begin
  //      DO 7 K = 1,NN
          IF (K >= LCC1) THEN
          begin
            IF (K > ILAST) THEN
            begin
              I := I + 1;
              IFRST := K;
              IF (I < NCC) THEN
              begin
                ILAST := LCC[I] - 1;
              end
              ELSE
              begin
                ILAST := NN;
              END;
              KBAK := ILAST;
              KFOR := K + 1;
            end
            ELSE
            begin
              KBAK := K-1;
              IF (K < ILAST) THEN
              begin
                KFOR := K+1;
              end
              ELSE
              begin
                KFOR := IFRST;
              END;
            END;
          END;
          XK := X[K-1];
          YK := Y[K-1];
          ZK := Z[K-1];
          ZXK := GRAD[IWK_Index(K-1,0)];
          ZYK := GRAD[IWK_Index(K-1,1)];
  //        IF (K .GE. LCC1) THEN
  //          IF (K .GT. ILAST) THEN
  //            I = I + 1
  //            IFRST = K
  //            IF (I .LT. NCC) THEN
  //              ILAST = LCC(I+1) - 1
  //            ELSE
  //              ILAST = NN
  //            ENDIF
  //            KBAK = ILAST
  //            KFOR = K + 1
  //          ELSE
  //            KBAK = K-1
  //            IF (K .LT. ILAST) THEN
  //              KFOR = K+1
  //            ELSE
  //              KFOR = IFRST
  //            ENDIF
  //          ENDIF
  //        ENDIF
  //        XK = X(K)
  //        YK = Y(K)
  //        ZK = Z(K)
  //        ZXK = GRAD(1,K)
  //        ZYK = GRAD(2,K)
  //C
  //C Initialize components of the 2 by 2 (or 3 by 3) block --
  //C   symmetric matrix in A and residual in R.  The unknowns
  //C   are ordered (DZX,DZY,DZ).
  //C
          A11 := 0.0;
          A12 := 0.0;
          A13 := 0.0;
          A22 := 0.0;
          A23 := 0.0;
          A33 := 0.0;
          R1 := 0.0;
          R2 := 0.0;
          R3 := 0.0;
  //        A11 = 0.
  //        A12 = 0.
  //        A13 = 0.
  //        A22 = 0.
  //        A23 = 0.
  //        A33 = 0.
  //        R1 = 0.
  //        R2 = 0.
  //        R3 = 0.
  //C
  //C Loop on neighbors J of node K.  The equation associated
  //C   with K->J (and hence its contribution to the functional)
  //C   is weighted by AREAJ/D, where AREAJ is twice the sum of
  //C   the areas of the triangles containing K-J (excluding
  //C   those which lie in a constraint region) and D is the arc
  //C   length.  JN is the neighbor of K following J.  AREAP is
  //C   to the right of K->J and AREAN is to the left.
  //C
          LPL := LEND[K-1];
          J := LIST[LPL-1];
          LPF := LPTR[LPL-1];
          JN := LIST[LPF-1];
          AREAN := 0.0;
          IF (J > 0) then AREAN := (X[J-1]-XK)*(Y[JN-1]-YK) -
                               (Y[J-1]-YK)*(X[JN-1]-XK);
          LPN := LPF;
  //        LPL = LEND(K)
  //        J = LIST(LPL)
  //        LPF = LPTR(LPL)
  //        JN = LIST(LPF)
  //        AREAN = 0.
  //        IF (J .GT. 0) AREAN = (X(J)-XK)*(Y(JN)-YK) -
  //     .                        (Y(J)-YK)*(X(JN)-XK)
  //        LPN = LPF
  //C
  //C Top of loop:  LPF and LPL point to the first and last
  //C   neighbors of K, and LPN points to JN.
  //C
          repeat

            LPJ := LPN;
            LPN := LPTR[LPN-1];
            J := JN;
            AREAP := AREAN;
            JN := ABS(LIST[LPN-1]);
    //    3   LPJ = LPN
    //          LPN = LPTR(LPN)
    //          J = JN
    //          AREAP = AREAN
    //          JN = ABS(LIST(LPN))
    //C
    //C Arc K-J lies in a constraint region and is bypassed iff K
    //C   and J are nodes in the same constraint and J follows
    //C   KFOR and precedes KBAK as a neighbor of K.
    //C
            GoTo5 := False;
            IF (K < LCC1)  OR  (J < IFRST)  OR
               (J > ILAST) then
            begin
              GoTo5 := True;
            end
            else
            begin
              IF (J = KBAK) then AREAP := 0.0;
              IF (J = KBAK)  OR  (J = KFOR) then
              begin
                GoTo5 := True;
              end;
            end;
            GoT06 := False;
            if not GoTo5 then
            begin
              
    //          IF (K .LT. LCC1  .OR.  J .LT. IFRST  .OR.
    //     .        J .GT. ILAST) GO TO 5
    //          IF (J .EQ. KBAK) AREAP = 0.
    //          IF (J .EQ. KBAK  .OR.  J .EQ. KFOR) GO TO 5
    //C
              LP := LPN;
              GoT06 := False;
              repeat
                NB := ABS(LIST[LP-1]);
                IF (NB = KFOR) then
                begin
                  break;
                end;
                IF (NB = KBAK) then
                begin
                  GoT06 := True;
                  break;
                end;
                LP := LPTR[LP-1];
              until False;
    //          LP = LPN
    //    4     NB = ABS(LIST(LP))
    //            IF (NB .EQ. KFOR) GO TO 5
    //            IF (NB .EQ. KBAK) GO TO 6
    //            LP = LPTR(LP)
    //            GO TO 4
    //C
            end;
            if GoT06 then Continue;

              
    //C   Compute parameters associated with the edge K->J, and
    //C     test for duplicate nodes.  Note that AREAJ = 0 and
    //C     K->J is bypassed if K-J is both a constraint arc and
    //C     a boundary arc of the triangulation.
    //C
            DX := X[J-1] - XK;
            DY := Y[J-1] - YK;
            AREAN := 0.0;
            IF (LIST[LPL-1] <> -J)  AND  (J <> KFOR) then
            begin
              AREAN := DX*(Y[JN-1]-YK) - DY*(X[JN-1]-XK);
            end;
            AREAJ := AREAP + AREAN;
            IF (AREAJ = 0.0) then Continue;
            DXS := DX*DX;
            DYS := DY*DY;
            DSQ := DXS + DYS;
            D := SQRT(DSQ);
            DCUB := D*DSQ;
            IF (D = 0.0) then
            begin
        //C Nodes J and K coincide.
        //C
              NIT := 0;
              DZMAX := DZMX;
              IER := -3;
              Exit;
        //   11 NIT = 0
        //      DZMAX = DZMX
        //      IER = -3
        //      RETURN
            end;
            IF (IFL >= 1) then SIG := SIGMA[LPJ-1];
            GRCOEF (SIG,DCUB, DF,SDF);
            W := AREAJ/D;
    //    5     DX = X(J) - XK
    //          DY = Y(J) - YK
    //          AREAN = 0.
    //          IF (LIST(LPL) .NE. -J  .AND.  J .NE. KFOR) AREAN =
    //     .      DX*(Y(JN)-YK) - DY*(X(JN)-XK)
    //          AREAJ = AREAP + AREAN
    //          IF (AREAJ .EQ. 0.) GO TO 6
    //          DXS = DX*DX
    //          DYS = DY*DY
    //          DSQ = DXS + DYS
    //          D = SQRT(DSQ)
    //          DCUB = D*DSQ
    //          IF (D .EQ. 0.) GO TO 11
    //          IF (IFL .GE. 1) SIG = SIGMA(LPJ)
    //          CALL GRCOEF (SIG,DCUB, DF,SDF)
    //          W = AREAJ/D
    //C
    //C   Update the 2 by 2 system components for node J.
    //C
              A11 := A11 + DF*DXS*W;
              A12 := A12 + DF*DX*DY*W;
              A22 := A22 + DF*DYS*W;
              DZ := Z[J-1] - ZK;
              DZJ := GRAD[IWK_Index(J-1,0)]*DX + GRAD[IWK_Index(J-1,1)]*DY;
              DZK := ZXK*DX + ZYK*DY;
              T := ( (DF+SDF)*DZ - SDF*DZJ - DF*DZK )*W;
              R1 := R1 + T*DX;
              R2 := R2 + T*DY;
    //          A11 = A11 + DF*DXS*W
    //          A12 = A12 + DF*DX*DY*W
    //          A22 = A22 + DF*DYS*W
    //          DZ = Z(J) - ZK
    //          DZJ = GRAD(1,J)*DX + GRAD(2,J)*DY
    //          DZK = ZXK*DX + ZYK*DY
    //          T = ( (DF+SDF)*DZ - SDF*DZJ - DF*DZK )*W
    //          R1 = R1 + T*DX
    //          R2 = R2 + T*DY
              IF (K >= LCC1) THEN
              begin
    //          IF (K .GE. LCC1) THEN
    //C
    //C   K is a constraint node.  Update the remaining components.
    //C
                W := (DF+SDF)*W;
                A13 := A13 + DX*W;
                A23 := A23 + DY*W;
                A33 := A33 + 2.0*W;
                R3 := R3 + (2.0*DZ - DZJ - DZK)*W;
              END;
    //            W = (DF+SDF)*W
    //            A13 = A13 + DX*W
    //            A23 = A23 + DY*W
    //            A33 = A33 + 2.0*W
    //            R3 = R3 + (2.0*DZ - DZJ - DZK)*W
    //          ENDIF
    //C
    //C   Bottom of loop on J.
    //C
    //    6     IF (LPN .NE. LPF) GO TO 3
          until (LPN = LPF);
  //C
  //C Solve the linear system associated with the K-th block.
  //C
          A22 := A11*A22 - A12*A12;
          R2 := A11*R2 - A12*R1;
          IF (A11 = 0.0)  OR  (A22 = 0.0) then
          begin
      //C Node K and its neighbors are collinear, resulting in a
      //C   singular system.
      //C
            NIT := 0;
            DZMAX := DZMX;
            IER := -2;
            Exit;
      //   10 NIT = 0
      //      DZMAX = DZMX
      //      IER = -2
      //      RETURN
          end;
          IF (K >= LCC1) THEN
          begin
            A23 := A11*A23 - A12*A13;
            A33 := A22*(A11*A33 - A13*A13) - A23*A23;
            R3 := A22*(A11*R3 - A13*R1) - A23*R2;
            IF (A33 = 0.0) then
            begin
        //C Node K and its neighbors are collinear, resulting in a
        //C   singular system.
        //C
              NIT := 0;
              DZMAX := DZMX;
              IER := -2;
              Exit;
        //   10 NIT = 0
        //      DZMAX = DZMX
        //      IER = -2
        //      RETURN
            end;
            DZ := R3/A33
          END;
          DZY := (R2 - A23*DZ)/A22;
          DZX := (R1 - A12*DZY - A13*DZ)/A11;
  //        A22 = A11*A22 - A12*A12
  //        R2 = A11*R2 - A12*R1
  //        IF (A11 .EQ. 0.  .OR.  A22 .EQ. 0.) GO TO 10
  //        IF (K .GE. LCC1) THEN
  //          A23 = A11*A23 - A12*A13
  //          A33 = A22*(A11*A33 - A13*A13) - A23*A23
  //          R3 = A22*(A11*R3 - A13*R1) - A23*R2
  //          IF (A33 .EQ. 0.) GO TO 10
  //          DZ = R3/A33
  //        ENDIF
  //        DZY = (R2 - A23*DZ)/A22
  //        DZX = (R1 - A12*DZY - A13*DZ)/A11
  //C
  //C Update the solution components for node K and the maxi-
  //C   mum relative change DZMX.
  //C
          GRAD[IWK_Index(K-1,0)] := ZXK + DZX;
          GRAD[IWK_Index(K-1,1)] := ZYK + DZY;
          IF (K >= LCC1) THEN
          begin
            Z[K-1] := ZK + DZ;
            DZMX := MAX(DZMX,ABS(DZ)/(1.0+ABS(ZK)));
          END;
  //        GRAD(1,K) = ZXK + DZX
  //        GRAD(2,K) = ZYK + DZY
  //        IF (K .GE. LCC1) THEN
  //          Z(K) = ZK + DZ
  //          DZMX = MAX(DZMX,ABS(DZ)/(1.+ABS(ZK)))
  //        ENDIF
  //    7   CONTINUE
        end;
  //C
  //C Increment ITER and test for convergence.
  //C
        ITER := ITER + 1;
  //      ITER = ITER + 1
  //      IF (DZMX .GT. TOL) GO TO 2
      until DZMX <= TOL;
//C
//C Method converged.
//C
      NIT := ITER;
      DZMAX := DZMX;
      IER := 0;
//      NIT = ITER
//      DZMAX = DZMX
//      IER = 0
//      RETURN
//C
//C Method failed to converge within NIT iterations.
//C
//    8 DZMAX = DZMX
//      IER = 1
//      RETURN
//C
//C Invalid input parameter.
//C
//    9 NIT = 0
//      DZMAX = 0.
//      IER = -1
//      RETURN
//C
//C Node K and its neighbors are collinear, resulting in a
//C   singular system.
//C
//   10 NIT = 0
//      DZMAX = DZMX
//      IER = -2
//      RETURN
//C
//C Nodes J and K coincide.
//C
//   11 NIT = 0
//      DZMAX = DZMX
//      IER = -3
//      RETURN
//      END
end;

procedure ZINIT (const NCC: longint; const LCC: TNcmaxIntArray; const N: longint;
  const X,Y: TNmaxSingleArray; const LIST,LPTR: TN6IntArray; const LEND: TNmaxIntArray;
  var Z: TNmaxSingleArray; var IER: longint);
//      SUBROUTINE ZINIT (NCC,LCC,N,X,Y,LIST,LPTR,
//     .                  LEND, Z, IER)
//      INTEGER NCC, LCC(*), N, LIST(*), LPTR(*), LEND(N),
//     .        IER
//      REAL    X(N), Y(N), Z(N)
//C
//C***********************************************************
//C
//C                                               From SRFPACK
//C                                            Robert J. Renka
//C                                  Dept. of Computer Science
//C                                       Univ. of North Texas
//C                                           renka@cs.unt.edu
//C                                                   08/27/91
//C
//C   Given a triangulation of N nodes, along with data values
//C at non-constraint nodes, this subroutine computes approxi-
//C mate data values at the constraint nodes.  The approximate
//C values are intended only to serve as initial estimates for
//C Subroutine ZGRADG which computes refined estimates.
//C
//C   For each subsequence (KM2,KM1,K) of a constraint, the
//C approximate value at node KM1 is taken to be the closest-
//C point value (data value at the closest non-constraint
//C node) at KM1 averaged with the value at KM1 of the linear
//C interpolant (along the constraint boundary) of the approx-
//C imate value at KM2 and the closest-point value at K.
//C
//C On input:
//C
//C       NCC = Number of constraint curves (refer to TRIPACK
//C             Subroutine ADDCST).  NCC .GE. 0.
//C
//C       LCC = Array of length NCC (or dummy array of length
//C             1 if NCC = 0) containing the index of the
//C             first node of constraint I in LCC(I).  For I =
//C             1 to NCC, LCC(I+1)-LCC(I) .GE. 3, where
//C             LCC(NCC+1) = N+1, and LCC(1) .GE. 4.
//C
//C       N = Number of nodes in the triangulation.  N .GE. 3.
//C
//C       X,Y = Arrays of length N containing the coordinates
//C             of the nodes with non-constraint nodes in the
//C             first LCC(1)-1 locations, followed by NCC se-
//C             quences of constraint nodes.
//C
//C       LIST,LPTR,LEND = Data structure defining the trian-
//C                        gulation.  Refer to TRIPACK
//C                        Subroutine TRMESH.
//C
//C The above parameters are not altered by this routine.
//C
//C       Z = Array of length N containing data values in the
//C           first LCC(1)-1 locations.
//C
//C On output:
//C
//C       Z = Array updated with approximate data values in
//C           the last N-LCC(1)+1 locations if IER = 0.
//C
//C       IER = Error indicator:
//C             IER = 0 if no errors were encountered.
//C             IER = 1 if NCC, N, or an LCC entry is outside
//C                     its valid range on input.
//C
//C TRIPACK modules required by ZINIT:  GETNP, INTSEC
//C
//C Intrinsic functions called by ZINIT:  ABS, SQRT
//C
//C***********************************************************
//C
const
  LMAX = 12;
//      INTEGER   LMAX
//      PARAMETER (LMAX=12)
var
         I, IERR, IFRST, ILAST, ILSTM1, K, KM1, KM2,
               KN, LCC1, LNP, LP, LPL: longint;
         NPTS: TLwkIntArray;
         D, DMIN, H1, H2, XK, YK, ZN: TFloat;
         DS: TNmaxSingleArray;
//      INTEGER   I, IERR, IFRST, ILAST, ILSTM1, K, KM1, KM2,
//     .          KN, LCC1, LNP, LP, LPL, NPTS(LMAX)
//      REAL      D, DMIN, DS(LMAX), H1, H2, XK, YK, ZN
begin
  SetLength(NPTS, LMAX);
  SetLength(DS, LMAX);
//C
//C Test for errors in input parameters.  (LCC is tested by
//C   Subroutine GETNP.)
//C
      IER := 1;
      IF (NCC > 0) THEN
      begin
        LCC1 := LCC[0];
      end
      ELSE
      begin
        LCC1 := N+1;
      END;
      IF (NCC < 0)  OR  (LCC1 < 4) then Exit;
//      IER = 1
//      IF (NCC .GT. 0) THEN
//        LCC1 = LCC(1)
//      ELSE
//        LCC1 = N+1
//      ENDIF
//      IF (NCC .LT. 0  .OR.  LCC1 .LT. 4) RETURN
//C
//C Outer loop on constraint I with first and last nodes IFRST
//C   and ILAST.
//C
      for I := 1 to NCC do
      begin
//      DO 6 I = 1,NCC
        IFRST := LCC[I-1];
        IF (I < NCC) THEN
        begin
          ILAST := LCC[I] - 1
        end
        ELSE
        begin
          ILAST := N;
        END;
//        IFRST = LCC(I)
//        IF (I .LT. NCC) THEN
//          ILAST = LCC(I+1) - 1
//        ELSE
//          ILAST = N
//        ENDIF
//C
//C Initialize Z(ILAST) with the data value at the closest
//C   non-constraint node to ILAST.  Unless the LMAX closest
//C   nodes to ILAST (including ILAST) are all constraint
//C   nodes, NPTS is set to the closest LNP nodes (with
//C   distance measured in the non-constraint region), where
//C   LNP is the smallest integer such that NPTS contains a
//C   non-constraint node.  The value at LCC(1)-1 is used if
//C   LMAX is too small.
//C
        LNP := 1;
        NPTS[0] := ILAST;
        DS[0] := 0.0;
//        LNP = 1
//        NPTS(1) = ILAST
//        DS(1) = 0.
        repeat
          LNP := LNP + 1;
          GETNP (NCC,LCC,N,X,Y,LIST,LPTR,LEND,
                     LNP, NPTS,DS, IERR);
          IF (IERR <> 0) then Exit;
          KN := NPTS[LNP-1];
//    1   LNP = LNP + 1
//          CALL GETNP (NCC,LCC,N,X,Y,LIST,LPTR,LEND,
//     .                LNP, NPTS,DS, IERR)
//          IF (IERR .NE. 0) RETURN
//          KN = NPTS(LNP)
//          IF (KN .GE. LCC1  .AND.  LNP .LT. LMAX) GO TO 1
        until (KN < LCC1) or (LNP >= LMAX);
        IF (KN >= LCC1) then KN := LCC1-1;
        Z[ILAST-1] := Z[KN-1];
//        IF (KN .GE. LCC1) KN = LCC1-1
//        Z(ILAST) = Z(KN)
//C
//C Loop on constraint nodes K.  LPL points to the last
//C   neighbor of K.  At each step, Z(K) is set to the
//C   closest-point value at K, and Z(KM1) is set to the
//C   (final) approximate data value at KM1 (except when
//C   K = IFRST).
//C
        KM1 := ILAST;
        ILSTM1 := ILAST - 1;
//        KM1 = ILAST
//        ILSTM1 = ILAST - 1
        for K := IFRST to ILSTM1 do
        begin
//        DO 5 K = IFRST,ILSTM1
          XK := X[K-1];
          YK := Y[K-1];
          LPL := LEND[K-1];
//          XK = X(K)
//          YK = Y(K)
//          LPL = LEND(K)
//C
//C   Set LP to point to KM1 as a neighbor of K.
//C
          LP := LPL;
          repeat
            LP := LPTR[LP-1];
          until ABS(LIST[LP-1]) = KM1;
//          LP = LPL
//    2     LP = LPTR(LP)
//            IF (ABS(LIST(LP)) .NE. KM1) GO TO 2
//C
//C   Initialize for loop on non-constraint node neighbors of
//C     K.  If K has no such neighbors, the closest non-
//C     constraint node to K is (implicitly) taken to be the
//C     closest non-constraint node to KM1.
//C
          DMIN := -1.0;
          ZN := Z[KM1-1];
//          DMIN = -1.
//          ZN = Z(KM1)
          repeat
            LP := LPTR[LP-1];
            KN := ABS(LIST[LP-1]);
            IF (KN = K+1) then break;
            IF (KN = LCC1) then Continue;
            D := Sqr(X[KN-1]-XK) + Sqr(Y[KN-1]-YK);
            IF (DMIN >= 0.0) AND (DMIN < D) then Continue;
            DMIN := D;
            ZN := Z[KN-1];
//    3     LP = LPTR(LP)
//            KN = ABS(LIST(LP))
//            IF (KN .EQ. K+1) GO TO 4
//            IF (KN .GE. LCC1) GO TO 3
//            D = (X(KN)-XK)**2 + (Y(KN)-YK)**2
//            IF (DMIN .GE. 0.  .AND.  DMIN .LT. D) GO TO 3
//            DMIN = D
//            ZN = Z(KN)
//            GO TO 3

          until False;
//C
//C   ZN is the closest-point value at K.  Set H2 to the arc
//C     length of KM1-K, and compute Z(KM1) if K > IFRST.
//C     (H1 is the arc length of KM2-KM1).
//C
          H2 := SQRT( Sqr(XK-X[KM1-1]) + Sqr(YK-Y[KM1-1]) );
          IF (K <> IFRST) then Z[KM1-1] := 0.5*( Z[KM1-1] +
            (H1*ZN+H2*Z[KM2-1])/(H1+H2) );
          Z[K-1] := ZN;
//    4     H2 = SQRT( (XK-X(KM1))**2 + (YK-Y(KM1))**2 )
//          IF (K .NE. IFRST) Z(KM1) = .5*( Z(KM1) +
//     .      (H1*ZN+H2*Z(KM2))/(H1+H2) )
//          Z(K) = ZN
//C
//C   Bottom of loop on K.
//C
          H1 := H2;
          KM2 := KM1;
          KM1 := K;
//          H1 = H2
//          KM2 = KM1
//          KM1 = K
//    5     CONTINUE
        end;
//C
//C For K = ILAST, the closest-point value has already been
//C   computed.
//C
        H2 := SQRT ( Sqr(X[ILAST-1]-X[ILSTM1-1]) +
                   Sqr(Y[ILAST-1]-Y[ILSTM1-1]) );
        Z[ILSTM1-1] := 0.5*( Z[ILSTM1-1] + (H1*Z[ILAST-1]+H2*Z[KM2-1])
                                   /(H1+H2) );
//        H2 = SQRT ( (X(ILAST)-X(ILSTM1))**2 +
//     .              (Y(ILAST)-Y(ILSTM1))**2 )
//        Z(ILSTM1) = .5*( Z(ILSTM1) + (H1*Z(ILAST)+H2*Z(KM2))
//     .                              /(H1+H2) )
//C
//C Compute the final value at ILAST.
//C
        H1 := H2;
        H2 := SQRT ( Sqr(X[IFRST-1]-X[ILAST-1]) +
                   Sqr(Y[IFRST-1]-Y[ILAST-1]) );
        Z[ILAST-1] := 0.5*(Z[ILAST-1] + (H1*Z[IFRST-1]+H2*Z[ILSTM1-1])
                                /(H1+H2));
//        H1 = H2
//        H2 = SQRT ( (X(IFRST)-X(ILAST))**2 +
//     .              (Y(IFRST)-Y(ILAST))**2 )
//        Z(ILAST) = .5*(Z(ILAST) + (H1*Z(IFRST)+H2*Z(ILSTM1))
//     .                           /(H1+H2))
//    6   CONTINUE
      end;
//C
//C No errors encountered.
//C
      IER := 0;
//      IER = 0
//      RETURN
//      END
end;

{$IFNDEF MyDebug}
  {$HINTS ON}
  {$WARNINGS ON}
{$ENDIF}

end.
