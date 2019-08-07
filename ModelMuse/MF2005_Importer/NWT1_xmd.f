
c      for XMD package for NWT solver (NWT version 1.06; 12-05-12)
c
c                     M. Ibaraki
c
c             Thu Oct 29 10:03:25 EDT 2009
c
c     variable definitions:
c
c      iacl               choice of acceleration method
c                         = 0; conjugate gradient
c                         = 1; ORTHOMIN
c                         = 2; CGSTAB
c      n                  number of unknowns
c      norder             = 0; original ordering
c                         = 1; RCM ordering
c                         = 2; Minimum Degree ordering
c      nja                size of ja, a, arrays
c      njaf               size of af, jaf arrays
c      level              level of ILU
c      itmax              number of maximum allowable iterations
c      north              number of orthogonalization
c      liwrk              size of integer work array
c      lrwrk              size of real work array
c
c      ia(n+1),ja(nja)    usual ia, ja arrays for coefficient matrix
c      lorder(n)          ordering vector: lorder( new_order ) = old_order
c      iwork(liwrk)      temporary work array
c
c      dptol              flag for the drop tolerance
c                         =.true. perform the drop tolerance
c                         =.false. do NOT perform the drop tolerance
c
c      epsrn              drop tolerance
c      ctol               absolute convergence criteria
c      rrctol             residual reduction convergence criteria
c
c      rwork(lrwrk)       temporary work array
c      a(nja)             matrix stored as linear array
c      af(njaf)           factored matrix (each row of af contains a row L\U)
c                         where A = LU
c      b(n)               right hand side vector
c      x(n)               solution vector
c
c
c      nx,ny,nz           graph of matrix is regular rectangular grid
c                         of size nx * ny * nz
c
cmi
c      MODULE XMDMODULE
c      IMPLICIT NONE
c      LOGICAL, SAVE, POINTER ::  REDSYS,LDCOMB
c      DOUBLE PRECISION, SAVE, POINTER ::  EPSRN,RRCTOL,HCLOSEXMD
c      INTEGER, SAVE, POINTER ::  MXITERXMD
c      DOUBLE PRECISION, SAVE, DIMENSION(:), POINTER ::  RWORK,AF,DGSCAL
c      INTEGER, SAVE, DIMENSION(:), POINTER ::  LORDER,IWORK,MSINDX
c      INTEGER, SAVE, POINTER :: IACL,NORDER,NJAF,LEVEL,NORTH,LIWRK,
c     *  LRWRK,IDROPTOL,NBLACK,IERR,IDSCALE,MBLACK
c      END MODULE XMDMODULE
cmi


      MODULE XMDMODULE
      IMPLICIT NONE
      LOGICAL, SAVE, POINTER ::  REDSYS,LDCOMB
      DOUBLE PRECISION, SAVE, POINTER ::  EPSRN,RRCTOL,HCLOSEXMD
      INTEGER, SAVE, POINTER ::  MXITERXMD
      INTEGER, SAVE, POINTER :: IACL,NORDER,LEVEL,NORTH,IDROPTOL,
     [                          IERR
      END MODULE XMDMODULE
C------------------------------------------------------------------
      SUBROUTINE XMD7AR(IN)

      USE GLOBAL, ONLY: IOUT
      USE XMDMODULE
!      USE GWFNWTMODULE, ONLY: IPRNWT,NUMACTIVE,IA,JA,NJA,IFDPARAM
      USE GWFNWTMODULE, ONLY: IPRNWT,NUMACTIVE,NJA,IFDPARAM
cmi
      use xmdcmn
cmi
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
      INTRINSIC INT
      EXTERNAL URDCOM, URWORD
cmi
c     include 'xmdcmn.com'
cmi
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
      INTEGER IN,NODES
!     ------------------------------------------------------------------
!     LOCAL VARIABLES
!     ------------------------------------------------------------------
      INTEGER lloc, istart, istop, i, n,ISTORXMD,IREDSYS, Icomb
      CHARACTER(LEN=300) line
      REAL R,RRCTOLS,EPSRNS,HCLOSEXMDDUM
!     LOCAL VARIABLES FOR XMD SOLVER
!     ------------------------------------------------------------------
!
!1------IDENTIFY PACKAGE AND INITIALIZE.
!      WRITE(IOUT,1)IN
!    1 FORMAT(1X,'XMD -- LINEAR SOLUTION BY XMD PACKAGE VERSION',
!     &       1X,'1.30',
!     & /1X,'    BY MOTOMU IBARAKI, OHIO STATE UNIVERSITY, COLOMBUS, OH',
!     & /1X,'                INPUT READ FROM UNIT',I3)
C
cmi
c      ALLOCATE (IACL,NORDER,NJAF,LEVEL,NORTH,LIWRK,LRWRK,IDROPTOL,
c     *  NBLACK,IERR,IDSCALE,MBLACK,HCLOSEXMD,MXITERXMD)
c      ALLOCATE (EPSRN,RRCTOL)
c      ALLOCATE (REDSYS,LDCOMB)
cmi
      ALLOCATE (IACL,NORDER,LEVEL,NORTH,IDROPTOL,
     *  IERR,HCLOSEXMD,MXITERXMD)
      ALLOCATE (EPSRN,RRCTOL)
      ALLOCATE (REDSYS,LDCOMB)
!-----XMD INPUT
      IF ( IFDPARAM.EQ.4 )CALL URDCOM(In, Iout, line)
      lloc = 1
      i = 1
      IF ( IFDPARAM.EQ.4 ) THEN
      CALL URWORD(line, lloc, istart, istop, 2, IACL, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, NORDER, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, LEVEL, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, NORTH, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, IREDSYS, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 3, I, RRCTOLS, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, IDROPTOL, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 3, I, EPSRNS, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 3, I,HCLOSEXMDDUM,Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, MXITERXMD,r,Iout,In)  !---added this 12/29/10
      WRITE(IOUT,*) 'IACL,NORDER,LEVEL,NORTH,IREDSYS,RRCTOLS,IDROPTOL,
     1EPSRNS,HCLOSEXMDDUM,MXITERXMD:'
      WRITE(IOUT,*) IACL,NORDER,LEVEL,NORTH,IREDSYS,RRCTOLS,IDROPTOL,
     1  EPSRNS,HCLOSEXMDDUM,MXITERXMD
      ELSEIF ( IFDPARAM.EQ.1 ) THEN
        IACL = 1
        NORDER = 0
        LEVEL = 3
        NORTH = 5
        IREDSYS = 1
        RRCTOLS = 0.0
        IDROPTOL = 1
        EPSRNS = 1.0e-3
        HCLOSEXMDDUM = 1.0e-4
        MXITERXMD = 50
      ELSEIF ( IFDPARAM.EQ.2 ) THEN
        IACL = 2
        NORDER = 0
        LEVEL = 5
        NORTH = 5
        IREDSYS = 1
        RRCTOLS = 0.0
        IDROPTOL = 1
        EPSRNS = 1.0e-4
        HCLOSEXMDDUM = 1.0e-4
        MXITERXMD = 50
      ELSEIF ( IFDPARAM.EQ.3 ) THEN
        IACL = 2
        NORDER = 1
        LEVEL = 5
        NORTH = 7
        IREDSYS = 1
        RRCTOLS = 0.0
        IDROPTOL = 1
        EPSRNS = 1.0e-5
        HCLOSEXMDDUM = 1.0e-5
        MXITERXMD = 50
      END IF    
      HCLOSEXMD = dble(HCLOSEXMDDUM)
!
!
!
c      read (*,*) IACL, NORDER, LEVEL, NORTH, IREDSYS, IDROPTOL, Icomb,
c     [            EPSRNS
c
c      write (*,1192) IACL, NORDER, LEVEL, NORTH, IREDSYS, IDROPTOL,
c     [               Icomb, EPSRNS
c
c 1192 format(7i8, 1pe10.3)
C
C
      RETURN
      END
C-----------------------------------------------------------------------------------
      SUBROUTINE XMD7DA(IGRID)
C  DEALLOCATE GLOBAL DATA
      use xmdcmn
      use xmdmatrix
      INTEGER ALLOC_ERR
C
      DEALLOCATE(icolour, STAT = ALLOC_ERR)
      DEALLOCATE(RBorder, STAT = ALLOC_ERR)
      DEALLOCATE(iblackend, STAT = ALLOC_ERR)
      DEALLOCATE(lorder, STAT = ALLOC_ERR)
      DEALLOCATE(iaf, STAT = ALLOC_ERR)
      DEALLOCATE(idiagf, STAT = ALLOC_ERR)
      RETURN
      END



