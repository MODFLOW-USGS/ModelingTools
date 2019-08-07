
      MODULE GMRESMODULE
      IMPLICIT NONE
      DOUBLE PRECISION, SAVE, POINTER :: Stop_tol_gmres
      INTEGER, SAVE, POINTER :: IorderILU, Idir, Msdr
      INTEGER, SAVE, POINTER :: Ilu_method, Lev_fill
      INTEGER, SAVE, POINTER :: Maxitr_gmres, Istor_gmres
      DOUBLE PRECISION, SAVE, POINTER :: DROP_TOL
!      DOUBLE PRECISION, SAVE, DIMENSION(:), POINTER :: Alu
!      INTEGER, SAVE, DIMENSION(:), POINTER :: Jlu, Ju
      END MODULE GMRESMODULE
!C------------------------------------------------------------------
      SUBROUTINE GMRES7AR(IN)
!rgn------REVISION NUMBER CHANGED TO BE CONSISTENT WITH NWT RELEASE
!rgn------NEW VERSION NUMBER 1.0.5:  APRIL 5, 2012

      USE GLOBAL, ONLY: IOUT,STRT,IBOUND
      USE GMRESMODULE
!      USE GWFNWTMODULE, ONLY: IPRNWT,NUMACTIVE,IA,JA,NJA,IFDPARAM
      USE GWFNWTMODULE, ONLY: IPRNWT,NUMACTIVE,NJA,IFDPARAM
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
      INTRINSIC INT
      EXTERNAL URDCOM, URWORD
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
      INTEGER IN
!     ------------------------------------------------------------------
!     LOCAL VARIABLES
!     ------------------------------------------------------------------
      INTEGER lloc, istart, istop, i, n 
      CHARACTER(LEN=300) line
      REAL Stop_toldum, R
!     ------------------------------------------------------------------
!
!1------IDENTIFY PACKAGE AND INITIALIZE.
!      WRITE(IOUT,1)IN
!    1 FORMAT(1X,'GMRES -- LINEAR SOLUTION BY GMRES PACKAGE ',  &
!      /1X,'    adapted for HYDROTHERM by Kipp and others (2008)',  &
!      /1X,'    using software originally developed by Saad (1990)',I3,  &
!      /1X,'    using algorithms described in Saad (2003)',I3)
! ALLOCATE GMRES data
      ALLOCATE (Stop_tol_gmres,IorderILU, Idir, Msdr,Ilu_method)
      ALLOCATE (Lev_fill,Maxitr_gmres, Istor_gmres, DROP_TOL)
           
!-----GMRES INPUT

!IorderILU is type of ILU(0) factorization for preconditioner
!         1 - Standard ILU
!         2 - Modified ILU; Row-sum preserved.      
      IorderILU = 2
! ILU Method of ILU factorization for preconditioner. 
!         1 - ILU with drop tolerance and fill limit. Fill-in terms less than
!             drop tolerance times the diagonal are discarded. The number
!             of fill in terms in each row of L and U is limited to the fill limit.
!             The fill-limit largest elements are kept in the L and U factors.
!         2 - ILU(k), Order k incomplete LU factorization. Fill-in terms of higher
!             order than k in the factorization are discarded.
      Ilu_method = 2
! Lev_fill - the level of fill for method 2, the fill limit for method 1.
!           Recommended values: 5-10 for method 1, 0-2 for method 2.
      Lev_fill = 2
! Drop_tol - the drop tolerance for method 1. Default is 0.001      
      drop_tol = 0.001
! Msdr   - number of iterations between restarts of the gmres
      Msdr = 10
!         solution algorithm. Default is 5 but 10 or 20 might work
!         better for large problems.
! Stop_tol_gmres - Tolerance for convergence of the iterative solver
!         This is the residual of the linear equations scaled by
!         the norm of the rhs.
!         Usually 10^-8 to 10^-12 works ok.  
      Stop_toldum = 1.0D0-10
! Idir  - index for reordering direction permutation [1-6]
!        the actual order is not needed to be known by the
!        user. But the convergence rate will depend on the 
!        reordering for strong anisotropy in conductance or
!       cell dimensions, especially for the red-black reordering.
!        The d4zigzag is much less sensitive, which is why we
!        invented it. 
      Idir = 1
!-----GMRES INPUT
      IF ( IFDPARAM.EQ.4 )CALL URDCOM(In, Iout, line)
      lloc = 1
      i = 1
      IF ( IFDPARAM.EQ.4 ) THEN
      CALL URWORD(line, lloc, istart, istop, 2, Maxitr_gmres, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, Ilu_method, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, Lev_fill, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 3, I, Stop_toldum, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, Msdr, r, Iout, In)
      WRITE(IOUT,*)'MAXITINNER,ILUMETHOD,LEVFILL,STOPTOL,MSDR:'
      WRITE(IOUT,*)Maxitr_gmres,Ilu_method,Lev_fill,Stop_toldum,Msdr
      ELSEIF ( IFDPARAM.EQ.1 ) THEN
        Maxitr_gmres = 50
        Ilu_method = 2
        Lev_fill = 1
        Stop_toldum = 1.0e-10
        Msdr = 5
      ELSEIF ( IFDPARAM.EQ.2 ) THEN
        Maxitr_gmres = 50
        Ilu_method = 2
        Lev_fill = 1
        Stop_toldum = 1.0e-10
        Msdr = 10
      ELSEIF ( IFDPARAM.EQ.3 ) THEN
        Maxitr_gmres = 50
        Ilu_method = 2
        Lev_fill = 1
        Stop_toldum = 1.0e-10
        Msdr = 15
      END IF
 !     
      Stop_tol_gmres = Stop_toldum
      Istor_gmres = 0
! Allocate GMRES arrays.
      Istor_gmres = 4
!      Istor_gmres = Istor_gmres*NJA
!      ALLOCATE (Alu(Istor_gmres), Jlu(Istor_gmres), Ju(Istor_gmres))
!      Jlu = 0
!      Ju = 0
!      Alu = 0
      RETURN
      END 
    

!   SUBROUTINE gmres(n,msdr,rhs,sol,stop_tol,maxits,aa,ja,ia,alu,jlu,ju,iierr,n_iter,r_norm)
  !                 *** ILU - Preconditioned GMRES ***                  
  !----------------------------------------------------------------------
  ! This is a simple version of the ILU preconditioned GMRES algorithm. 
  ! GMRES uses the L and U matrices generated 
  ! from the subroutine ILU to precondition the GMRES algorithm.        
  ! The preconditioning is applied to the right. The stopping criterion  
  ! utilized is based simply on reducing the relative residual norm to stop_tol
  !     absolute stop_tol (eps_a) is used to handle small initial rhs.
  !                                                                      
  ! USAGE: first call ILUT or ILUK to set up preconditioner and 
  !    then call gmres.                                                    
  !----------------------------------------------------------------------
  ! adapted from  Y. Saad - 5/90
  ! see also chp.9.3.2 of Saad (2003) book for algorithm 9.5
  !----------------------------------------------------------------------
  ! subroutines called :                                           
  ! amux   : SPARSKIT routine to do the matrix*vector multiplication 
  ! lusol : combined forward and backward solves from preconditioning
  ! several BLAS1 routines                                                
  !----------------------------------------------------------------------
!
!
      SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)
!     .. Scalar Arguments ..
      DOUBLE PRECISION DA
      INTEGER INCX,INCY,N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION DX(*),DY(*)
!     ..
!
!  Purpose
!  =======
!
!     constant times a vector plus a vector.
!     uses unrolled loops for increments equal to one.
!     jack dongarra, linpack, 3/11/78.
!     modified 12/3/93, array(1) declarations changed to array(*)
!
!
!     .. Local Scalars ..
      INTEGER I,IX,IY,M,MP1
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC MOD
!     ..
      IF (N.LE.0) RETURN
      IF (DA.EQ.0.0d0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) GO TO 20
!
!        code for unequal increments or equal increments
!          not equal to 1
!
      IX = 1
      IY = 1
      IF (INCX.LT.0) IX = (-N+1)*INCX + 1
      IF (INCY.LT.0) IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
          DY(IY) = DY(IY) + DA*DX(IX)
          IX = IX + INCX
          IY = IY + INCY
   10 CONTINUE
      RETURN
!
!        code for both increments equal to 1
!
!
!        clean-up loop
!
   20 M = MOD(N,4)
      IF (M.EQ.0) GO TO 40
      DO 30 I = 1,M
          DY(I) = DY(I) + DA*DX(I)
   30 CONTINUE
      IF (N.LT.4) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
          DY(I) = DY(I) + DA*DX(I)
          DY(I+1) = DY(I+1) + DA*DX(I+1)
          DY(I+2) = DY(I+2) + DA*DX(I+2)
          DY(I+3) = DY(I+3) + DA*DX(I+3)
   50 CONTINUE
      RETURN
      END
!
!
      DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)
!     .. Scalar Arguments ..
      INTEGER INCX,INCY,N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION DX(*),DY(*)
!     ..
!
!  Purpose
!  =======
!
!     forms the dot product of two vectors.
!     uses unrolled loops for increments equal to one.
!     jack dongarra, linpack, 3/11/78.
!     modified 12/3/93, array(1) declarations changed to array(*)
!
!
!    .. Local Scalars ..
      DOUBLE PRECISION DTEMP
      INTEGER I,IX,IY,M,MP1
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC MOD
!     ..
      DDOT = 0.0d0
      DTEMP = 0.0d0
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) GO TO 20
!
!        code for unequal increments or equal increments
!          not equal to 1
!
      IX = 1
      IY = 1
      IF (INCX.LT.0) IX = (-N+1)*INCX + 1
      IF (INCY.LT.0) IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
          DTEMP = DTEMP + DX(IX)*DY(IY)
          IX = IX + INCX
          IY = IY + INCY
   10 CONTINUE
      DDOT = DTEMP
      RETURN
!
!        code for both increments equal to 1
!
!
!        clean-up loop
!
   20 M = MOD(N,5)
      IF (M.EQ.0) GO TO 40
      DO 30 I = 1,M
          DTEMP = DTEMP + DX(I)*DY(I)
   30 CONTINUE
      IF (N.LT.5) GO TO 60
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
          DTEMP = DTEMP + DX(I)*DY(I) + DX(I+1)*DY(I+1) + DX(I+2)*DY(I+2) + DX(I+3)*DY(I+3) + DX(I+4)*DY(I+4)
   50 CONTINUE
   60 DDOT = DTEMP
      RETURN
      END
 !
 !
       DOUBLE PRECISION FUNCTION DNRM2(N,X,INCX)
!     .. Scalar Arguments ..
      INTEGER INCX,N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION X(*)
!     ..
!
!  Purpose
!  =======
!
!  DNRM2 returns the euclidean norm of a vector via the function
!  name, so that
!
!     DNRM2 := sqrt( x'*x )
!
!
!  -- This version written on 25-October-1982.
!     Modified on 14-October-1993 to inline the call to DLASSQ.
!     Sven Hammarling, Nag Ltd.
!
!
!     .. Parameters ..
      DOUBLE PRECISION ONE,ZERO
      PARAMETER (ONE=1.0D+0,ZERO=0.0D+0)
!     ..
!     .. Local Scalars ..
      DOUBLE PRECISION ABSXI,NORM,SCALE,SSQ
      INTEGER IX
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC ABS,SQRT
!     ..
      IF (N.LT.1 .OR. INCX.LT.1) THEN
          NORM = ZERO
      ELSE IF (N.EQ.1) THEN
          NORM = ABS(X(1))
      ELSE
          SCALE = ZERO
          SSQ = ONE
!        The following loop is equivalent to this call to the LAPACK
!        auxiliary routine:
!        CALL DLASSQ( N, X, INCX, SCALE, SSQ )
!
          DO 10 IX = 1,1 + (N-1)*INCX,INCX
              IF (X(IX).NE.ZERO) THEN
                  ABSXI = ABS(X(IX))
                  IF (SCALE.LT.ABSXI) THEN
                      SSQ = ONE + SSQ* (SCALE/ABSXI)**2
                      SCALE = ABSXI
                  ELSE
                      SSQ = SSQ + (ABSXI/SCALE)**2
                  END IF
              END IF
   10     CONTINUE
          NORM = SCALE*SQRT(SSQ)
      END IF
!
      DNRM2 = NORM
      RETURN
!
!     End of DNRM2.
!
      END
!
     SUBROUTINE GMRES7DA(IGRID)
!  DEALLOCATE GLOBAL DATA
      USE GMRESMODULE
      INTEGER ALLOC_ERR
!      DEALLOCATE(Alu, STAT = ALLOC_ERR)
!      DEALLOCATE(Jlu, STAT = ALLOC_ERR)
!      DEALLOCATE(Ju, STAT = ALLOC_ERR)
      RETURN
      END





