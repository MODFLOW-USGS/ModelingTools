      MODULE GWFSWIMODULE
        TYPE TSWIDE4
          INTEGER  :: MXITER,NODES,NHALFU,NHALFL,NBWGRD
          INTEGER  :: MXUP,MXLOW,MXEQ,MXBW,ITMX,ID4DIR
          INTEGER  :: NITERDE4,IFREQ,ID4DIM
          INTEGER  :: NBWL,NUPL,NLOWL,NLOW,NEQ,NUP,NBW
          REAL     :: ACCLDE4,HCLOSEDE4,DELTL
          INTEGER,          ALLOCATABLE, DIMENSION(:,:)   :: IUPPNT
          INTEGER,          ALLOCATABLE, DIMENSION(:,:,:) :: IEQPNT
          REAL,             ALLOCATABLE, DIMENSION(:,:)   :: AU
          REAL,             ALLOCATABLE, DIMENSION(:,:)   :: AL
          REAL,             ALLOCATABLE, DIMENSION(:)     :: D4B
          REAL,             ALLOCATABLE, DIMENSION(:)     :: HDCGDE4
          INTEGER,          ALLOCATABLE, DIMENSION(:,:)   :: LRCHDE4
        END TYPE TSWIDE4

        TYPE TSWIPCG
          INTEGER :: MXITER,NODES
          INTEGER :: ITER1,NPCOND,NBPOL,NITER
          REAL    :: ZCLOSEPCG,RCLOSEPCG,RELAXPCG,DAMPPCG
          REAL    :: DAMPPCGT
          INTEGER :: IHCOFADD = 1
          DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:) :: VPCG
          DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:) :: SS
          DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:) :: P
          DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:) :: HPCG
          REAL,             ALLOCATABLE, DIMENSION(:,:,:) :: CD
          REAL,             ALLOCATABLE, DIMENSION(:,:,:) :: HCSV
          INTEGER,          ALLOCATABLE, DIMENSION(:,:)   :: LHCH
          REAL,             ALLOCATABLE, DIMENSION(:)     :: HCHG
          INTEGER,          ALLOCATABLE, DIMENSION(:,:)   :: LRCHPCG
          REAL,             ALLOCATABLE, DIMENSION(:)     :: RCHG
          INTEGER,          ALLOCATABLE, DIMENSION(:)     :: IT1
        END TYPE TSWIPCG

        TYPE TSWIOBS
          CHARACTER (LEN=12) :: OBSNAM
          INTEGER :: KLAY
          INTEGER :: IROW
          INTEGER :: JCOL
        END TYPE TSWIOBS
C         SWI DIMENSIONS
        INTEGER, SAVE, POINTER :: NSRF,ISTRAT,NSWIOPT,NZONES
C         SWI ADAPTIVE TIME STEP
        INTEGER, SAVE, POINTER :: NADPTFLG
        INTEGER, SAVE, POINTER :: NADPTMX
        INTEGER, SAVE, POINTER :: NADPTMN
        REAL, SAVE, POINTER    :: ADPTFCT
        INTEGER, SAVE, POINTER :: IADPT
        INTEGER, SAVE, POINTER :: IADPTMOD
        REAL, SAVE, POINTER    :: ADPTVAL
        REAL, SAVE, POINTER    :: SWIDELT
C---------STORAGE FOR ADAPTIVE SWI TIME STEP SUMMARY
        INTEGER, SAVE, DIMENSION(:), POINTER :: NADPTSUM
        REAL, SAVE, DIMENSION(:), POINTER :: RADPTSUM
        INTEGER, SAVE, POINTER :: IADPTSUM
C         SWI OUTPUT
        INTEGER, SAVE, POINTER :: NOBS
        INTEGER, SAVE, POINTER :: IOBSHEADER
        INTEGER, SAVE, POINTER :: ISWIZT,ISWICB,ISWIOBS
        INTEGER, SAVE, POINTER :: NLAYSWI
C         SOLVER
        INTEGER, SAVE, POINTER :: NSOLVER
        INTEGER, SAVE, POINTER :: IPRSOL
        INTEGER, SAVE, POINTER :: MUTSOL
C         SWI PARAMETERS
        REAL, SAVE, POINTER    :: TOESLOPE,TIPSLOPE,ALPHA,BETA
        INTEGER, SAVE, DIMENSION(:), POINTER :: ICONV
        INTEGER, SAVE, DIMENSION(:,:), POINTER :: IBO
        REAL, SAVE, DIMENSION(:,:), POINTER :: SWIHCOF
        REAL, SAVE, DIMENSION(:,:), POINTER :: SWISOLCR
        REAL, SAVE, DIMENSION(:,:), POINTER :: SWISOLCC
        REAL, SAVE, DIMENSION(:,:), POINTER :: SWISOLCV
        REAL, SAVE, DIMENSION(:,:,:,:), POINTER :: ZETA
        REAL, SAVE, DIMENSION(:,:,:,:), POINTER :: ZETAOLD
        REAL, SAVE, DIMENSION(:,:,:,:), POINTER :: ZETASWITS0
        REAL, SAVE, DIMENSION(:,:,:,:), POINTER :: ZONECHG1
        REAL, SAVE, DIMENSION(:,:,:,:), POINTER :: ZONECHG2
        REAL, SAVE, DIMENSION(:,:,:,:), POINTER :: ZONEIMIX
        REAL, SAVE, DIMENSION(:,:,:), POINTER :: SSZ
        REAL, SAVE, DIMENSION(:), POINTER :: EPS
        REAL, SAVE, DIMENSION(:), POINTER :: NUS
        REAL, SAVE, DIMENSION(:), POINTER :: DELNUS
        REAL, SAVE, DIMENSION(:), POINTER :: NUSRF
        REAL, SAVE, DIMENSION(:,:,:,:), POINTER :: SWICR
        REAL, SAVE, DIMENSION(:,:,:,:), POINTER :: SWICC
        REAL, SAVE, DIMENSION(:,:,:,:), POINTER :: SWICUMCR
        REAL, SAVE, DIMENSION(:,:,:,:), POINTER :: SWICUMCC
        REAL, SAVE, DIMENSION(:,:,:), POINTER :: NUTOP
        REAL, SAVE, DIMENSION(:,:,:), POINTER :: NUBOT
        REAL, SAVE, DIMENSION(:,:,:), POINTER :: QLEXTRA
        REAL, SAVE, DIMENSION(:,:,:), POINTER :: QREXTRA
        REAL, SAVE, DIMENSION(:,:,:), POINTER :: QFEXTRA
        REAL, SAVE, DIMENSION(:,:,:), POINTER :: QLEXTRACUM
        REAL, SAVE, DIMENSION(:,:,:), POINTER :: QREXTRACUM
        REAL, SAVE, DIMENSION(:,:,:), POINTER :: QFEXTRACUM
        REAL, SAVE, DIMENSION(:,:,:), POINTER :: BRHS
        DOUBLE PRECISION, SAVE, DIMENSION(:,:), POINTER :: DUM
        REAL, SAVE, DIMENSION(:,:,:), POINTER :: RHSPRESWI
        INTEGER, SAVE, DIMENSION(:,:,:,:), POINTER :: IPLPOS
        INTEGER, SAVE, DIMENSION(:,:,:), POINTER :: IZONENR
C---------STORAGE FOR BUDGET DATA
        INTEGER,SAVE,POINTER                :: NBDITEMS
        REAL,SAVE,DIMENSION(:,:,:), POINTER :: CUMBD
        REAL,SAVE,DIMENSION(:,:,:), POINTER :: INCBD
        DOUBLEPRECISION,SAVE,DIMENSION(:), POINTER  :: RRATIN
        DOUBLEPRECISION,SAVE,DIMENSION(:), POINTER  :: RRATOUT
C---------POINTERS WITH STORAGE VALUES FROM BCF, LPF, HUF, OR UPW
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::SC1
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::SC2
C---------STORAGE FOR OBSERVATION DATA
        TYPE (TSWIOBS), SAVE, DIMENSION(:), POINTER :: SWIOBS
C---------STORAGE FOR SOLVERS
        TYPE (TSWIDE4),  SAVE, POINTER :: SWIDE4
        TYPE (TSWIPCG),  SAVE, POINTER :: SWIPCG

        TYPE GWFSWITYPE
C           SWI DIMENSIONS
          INTEGER, POINTER :: NSRF,ISTRAT,NSWIOPT,NZONES
C           SWI ADAPTIVE TIME STEP
          INTEGER, POINTER :: NADPTFLG
          INTEGER, POINTER :: NADPTMX
          INTEGER, POINTER :: NADPTMN
          REAL, POINTER    :: ADPTFCT
          INTEGER, POINTER :: IADPT
          INTEGER, POINTER :: IADPTMOD
          REAL, POINTER    :: ADPTVAL
          REAL, POINTER    :: SWIDELT
C---------STORAGE FOR ADAPTIVE SWI TIME STEP SUMMARY
          INTEGER, DIMENSION(:), POINTER :: NADPTSUM
          REAL, DIMENSION(:), POINTER :: RADPTSUM
          INTEGER, POINTER :: IADPTSUM
C           SWI OUTPUT
          INTEGER, POINTER :: NOBS
          INTEGER, POINTER :: IOBSHEADER
          INTEGER, POINTER :: ISWIZT,ISWICB,ISWIOBS
          INTEGER, POINTER :: NLAYSWI
C           SOLVER
          INTEGER, POINTER :: NSOLVER
          INTEGER, POINTER :: IPRSOL
          INTEGER, POINTER :: MUTSOL
C           SWI PARAMETERS
          REAL, POINTER    :: TOESLOPE,TIPSLOPE,ALPHA,BETA
          INTEGER, DIMENSION(:), POINTER :: ICONV
          INTEGER, DIMENSION(:,:), POINTER :: IBO
          REAL, DIMENSION(:,:), POINTER :: SWIHCOF
          REAL, DIMENSION(:,:), POINTER :: SWISOLCR
          REAL, DIMENSION(:,:), POINTER :: SWISOLCC
          REAL, DIMENSION(:,:), POINTER :: SWISOLCV
          REAL, DIMENSION(:,:,:,:), POINTER :: ZETA
          REAL, DIMENSION(:,:,:,:), POINTER :: ZETAOLD
          REAL, DIMENSION(:,:,:,:), POINTER :: ZETASWITS0
          REAL, DIMENSION(:,:,:,:), POINTER :: ZONECHG1
          REAL, DIMENSION(:,:,:,:), POINTER :: ZONECHG2
          REAL, DIMENSION(:,:,:,:), POINTER :: ZONEIMIX
          REAL, DIMENSION(:,:,:), POINTER :: SSZ
          REAL, DIMENSION(:), POINTER :: EPS
          REAL, DIMENSION(:), POINTER :: NUS
          REAL, DIMENSION(:), POINTER :: DELNUS
          REAL, DIMENSION(:), POINTER :: NUSRF
          REAL, DIMENSION(:,:,:,:), POINTER :: SWICR
          REAL, DIMENSION(:,:,:,:), POINTER :: SWICC
          REAL, DIMENSION(:,:,:,:), POINTER :: SWICUMCR
          REAL, DIMENSION(:,:,:,:), POINTER :: SWICUMCC
          REAL, DIMENSION(:,:,:), POINTER :: NUTOP
          REAL, DIMENSION(:,:,:), POINTER :: NUBOT
          REAL, DIMENSION(:,:,:), POINTER :: QLEXTRA
          REAL, DIMENSION(:,:,:), POINTER :: QREXTRA
          REAL, DIMENSION(:,:,:), POINTER :: QFEXTRA
          REAL, DIMENSION(:,:,:), POINTER :: QLEXTRACUM
          REAL, DIMENSION(:,:,:), POINTER :: QREXTRACUM
          REAL, DIMENSION(:,:,:), POINTER :: QFEXTRACUM
          REAL, DIMENSION(:,:,:), POINTER :: BRHS
          DOUBLE PRECISION, DIMENSION(:,:), POINTER :: DUM
          REAL, DIMENSION(:,:,:), POINTER :: RHSPRESWI
          INTEGER, DIMENSION(:,:,:,:), POINTER :: IPLPOS
          INTEGER, DIMENSION(:,:,:), POINTER :: IZONENR
C-----------STORAGE FOR BUDGET DATA
          INTEGER,POINTER                :: NBDITEMS
          REAL,DIMENSION(:,:,:), POINTER :: CUMBD
          REAL,DIMENSION(:,:,:), POINTER :: INCBD
          DOUBLEPRECISION,DIMENSION(:), POINTER  :: RRATIN
          DOUBLEPRECISION,DIMENSION(:), POINTER  :: RRATOUT
C---------POINTERS WITH STORAGE VALUES FROM BCF, LPF, HUF, OR UPW
          REAL,      POINTER, DIMENSION(:,:,:) ::SC1
          REAL,      POINTER, DIMENSION(:,:,:) ::SC2
C-----------STORAGE FOR OBSERVATION DATA
          TYPE (TSWIOBS), DIMENSION(:), POINTER :: SWIOBS
C-----------STORAGE FOR SOLVERS
          TYPE (TSWIDE4),  POINTER :: SWIDE4
          TYPE (TSWIPCG),  POINTER :: SWIPCG
        END TYPE
        TYPE(GWFSWITYPE), SAVE:: GWFSWIDAT(10)
      END MODULE GWFSWIMODULE
C
C
      SUBROUTINE GWF2SWI2AR(In,Ibcf,Ilpf,Ihuf,Iupw,Igrid)
C
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR SEA WATER INTRUSION (SWI2) PACKAGE
C     ******************************************************************
C
C     SPECIFICATIONS:
        USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,NPER,IFREFM,
     2                      NSTP,LBOTM,BOTM
        USE GWFBCFMODULE, ONLY:LCB=>LAYCON,SC1B=>SC1,SC2B=>SC2
        USE GWFLPFMODULE, ONLY:LCL=>LAYTYP,SC1L=>SC1,SC2L=>SC2
        USE GWFHUFMODULE, ONLY:LCH=>LTHUF,SC1H=>SC1
        USE GWFUPWMODULE, ONLY:LCU=>LAYTYPUPW,SC1U=>SC1,SC2U=>SC2UPW
        USE GWFSWIMODULE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: In
        INTEGER, INTENT(IN) :: Ibcf
        INTEGER, INTENT(IN) :: Ilpf
        INTEGER, INTENT(IN) :: Ihuf
        INTEGER, INTENT(IN) :: Iupw
        INTEGER, INTENT(IN) :: Igrid
C       + + + LOCAL DEFINITIONS + + +
        CHARACTER*200 :: line
        INTEGER :: lloc, istart, istop
        INTEGER :: iadptflg
        INTEGER :: ierr
        INTEGER :: i, j, k, n
        INTEGER :: iz, kk
        INTEGER :: itmem
        INTEGER :: ic
        REAL :: r
        REAL :: d
        REAL :: bbot, ttop, z
        CHARACTER*40, DIMENSION(2) :: csolver
        CHARACTER*24, DIMENSION(4) :: ANAME
        CHARACTER*24 :: ZETANAME
C       + + + DATA + + +
        DATA csolver /'                     DIRECT SOLVER (DE4)',
     2                ' PRECONDITIONED CONJUGATE GRADIENT (PCG)'/

        DATA ANAME(1) /'                  NUZONE'/
        DATA ANAME(2) /'                   NUSRF'/
        DATA ANAME(3) /'                     SSZ'/
        DATA ANAME(4) /'                 IZONENR'/
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
!    1   FORMAT(//1X,'SWI2 -- SWI PACKAGE, VERSION 2.0.0, 07/22/2013',
!     2          /1X,50('-'),
!     3          /1X,'SWI2 INPUT READ FROM UNIT',I3,//)
    7 FORMAT(//1X,'SWI2 ERROR: ',
     2       'THE TOTAL NUMBER OF SURFACES LESS THAN 1')
!02200   FORMAT(//1X,'SWI2 DATASET 1',/,1X,52('-'),
!     2    /1X,'NUMBER OF SURFACES (NSRF):                    ',1X,I5,
!     3    /1X,'TOTAL NUMBER OF ZONES:                        ',1X,I5,
!     4    /1X,'DENSITY DISTRIBUTION OPTION (ISTRAT):         ',1X,I5,
!     5    /1X,'  VARIABLE DENSITY FLOW -- ISTRAT = 0',
!     6    /1X,'  STRATIFIED FLOW       -- ISTRAT = 1',
!     7    /1X,'NUMBER OF SWI OBSERVATIONS (NOBS):            ',1X,I5,
!     8    /1X,'ZETA OUTPUT FILE UNIT NUMBER (ISWIZT):        ',1X,I5,
!     9    /1X,'BUDGET OUTPUT FILE UNIT NUMBER (ISWICB):      ',1X,I5,
!     X    /1X,'OBSERVATION OUTPUT FILE UNIT NUMBER (ISWIOBS):',1X,I5,
!     1    /1X,52('-'))
!02300   FORMAT(//1X,'SWI2 DATASET 1 KEYWORD OPTIONS',/,1X,52('-'))
!02310   FORMAT(1X,A50)
!02320   FORMAT(1X,52('-'))
  111   FORMAT('        ZETA SURFACE ',I2)
!02100   FORMAT(//1X,'SWI2 PROCESS REQUIRES USE OF THE BCF, LPF,'
!     2           1X,'OR HUF FLOW PACKAGES',//)
!02110   FORMAT(//1X,16X,'SWI2 OBSERVATION LOCATIONS',
!     2          /1X,' OBSERVATION',
!     3           1X,'     LAYER',1X,'       ROW',1X,'    COLUMN',
!     4           1X,'      OBSNAM',
!     5          /1X,58('-'))
!02120   FORMAT(1X,I12,3(1X,I10),1X,A12)
!02140   FORMAT(//1X,13X,'SWI2 INVALID OBSERVATION LOCATIONS',
!     2          /1X,59('-'))
02150   FORMAT(1X,' OBSERVATION',1X,I5,1X,A6,1X,I5,
     2         1X,'NOT BETWEEN',1X,I5,1X,'AND',1X,I5)
!02160   FORMAT(1X,'        NONE')
C     ------------------------------------------------------------------
C
C       + + + CODE + + +
C
C---------ALLOCATE VARIABLES - INITIALIZE IF POSSIBLE
        ALLOCATE(NSRF,ISTRAT,NZONES)
        ALLOCATE(NADPTFLG,NADPTMX,NADPTMN,ADPTFCT)
        ALLOCATE(IADPT,IADPTMOD,ADPTVAL,SWIDELT)
        ALLOCATE(NOBS,IOBSHEADER)
        ALLOCATE(ISWIZT,ISWICB,ISWIOBS)
        ALLOCATE(NSWIOPT)
        ALLOCATE(NLAYSWI)
        ALLOCATE(NSOLVER,IPRSOL,MUTSOL)
        ALLOCATE(TOESLOPE,TIPSLOPE,ALPHA,BETA)

        IOBSHEADER  = 0
        iadptflg    = 0
        NSWIOPT     = 0
C
C---------IDENTIFY PACKAGE AND INITIALIZE
        WRITE(IOUT,*) 'SWI2:'
!        WRITE(IOUT,1) In
C
C---------READ DATASET 1
        CALL URDCOM(In, IOUT, line)
        lloc = 1
        CALL URWORD(line, lloc, istart, istop, 2,   NSRF, r, IOUT, In)
        CALL URWORD(line, lloc, istart, istop, 2, ISTRAT, r, IOUT, In)
        CALL URWORD(line, lloc, istart, istop, 2,   NOBS, r, IOUT, In)
        CALL URWORD(line, lloc, istart, istop, 2, ISWIZT, r, IOUT, In)
        CALL URWORD(line, lloc, istart, istop, 2, ISWICB, r, IOUT, In)
        CALL URWORD(line, lloc, istart, istop, 2,ISWIOBS, r, IOUT, In)
C         TEST FOR KEYWORD ARGUMENTS
        DO
          CALL URWORD(line,lloc,istart,istop,1,n,r,IOUT,In)
          SELECT CASE ( line(istart:istop) )
            CASE ( 'ADAPTIVE' )
              iadptflg = 1
            CASE ( 'FSSSOPT' )
              NSWIOPT = 1
            CASE ( '0', '' )
              EXIT
!            CASE DEFAULT
!              IF ( LEN(line(istart:istop)).EQ.0 ) EXIT
!              WRITE (IOUT,'(1X,A,1X,A)') 
!     2          'UNRECOGNIZED KEYWORD:', line(istart:istop)
          END SELECT
        END DO
        Write(IOUT, *) 'iadptflg:'
        Write(IOUT, *) iadptflg

C
C---------CHECK TO SEE THAT NUMBER OF SURFACES IS AT LEAST 1
        IF ( NSRF.LT.1 ) THEN
          WRITE (IOUT,7)
          CALL USTOP('SWI2 ERROR: NUMBER OF SURFACES LESS THAN 1')
        ENDIF
C
C---------CALCULATE THE TOTAL NUMBER OF ZONES
        NZONES = NSRF + 1
        NLAYSWI = 1
C
C---------WRITE DATASET 1
        WRITE (IOUT,*) 
     1   'NSRF, NZONES, ISTRAT, NOBS, ISWIZT, ISWICB, ISWIOBS:'
        WRITE (IOUT,*) NSRF, NZONES, ISTRAT, NOBS,
     2                    ISWIZT, ISWICB, ISWIOBS
C         DATASET 1 OPTIONS
        IF ( NSWIOPT.NE.0 .OR. iadptflg.NE.0 ) THEN
!          WRITE (IOUT,2300)
          IF ( NSWIOPT.NE.0 ) THEN
!            WRITE (IOUT,2310) 
!     2        'DEBUG OPTION - ADDITION DATA DURING TIP/TOE     '
          END IF
          IF ( iadptflg.NE.0 ) THEN
!            WRITE (IOUT,2310) 
!     2        'SWI2 ADAPTIVE TIME STEP OPTION (ADAPTIVE)       '
          END IF
!          WRITE (IOUT,2320)
        END IF
C
C---------READ DATASET 2A - SOLVER DATA FOR BOTH SOLVERS
        CALL URDCOM(In, IOUT, line)
        lloc = 1
        CALL URWORD(line, lloc, istart, istop, 2,NSOLVER, r, IOUT, In)
        CALL URWORD(line, lloc, istart, istop, 2, IPRSOL, r, IOUT, In)
        CALL URWORD(line, lloc, istart, istop, 2, MUTSOL, r, IOUT, In)
        WRITE(IOUT, *) 'NSOLVER,IPRSOL,MUTSOL:'
        WRITE(IOUT, *) NSOLVER,IPRSOL,MUTSOL
        IF ( NSOLVER.LT.1 .OR. NSOLVER.GT.2 ) THEN
          WRITE (IOUT,2210) (i,csolver(i),i=1,3)
          CALL USTOP('SWI2 ERROR: INVALID NSOLVER SPECIFIED')
        END IF
        IF ( IPRSOL.LT.1 ) IPRSOL = 999
        IF ( MUTSOL.LT.0 .OR. MUTSOL.GT.3 ) MUTSOL=0
C
C---------WRITE DATASET 2A
!        WRITE (IOUT,2220) NSOLVER, csolver(NSOLVER),
!     2                    IPRSOL, MUTSOL

02210   FORMAT(//1X,'SWI2 DATASET 2A',/,1X,52('-'),
     2    /1X,'ERROR SPECIFYING NSOLVER - VALID VALUES ARE',
     3    100(:/1X,I2,1X,'=',A40))
!02220   FORMAT(//1X,'SWI2 DATASET 2A',/,1X,52('-'),
!     2    /1X,'SOLVER (NSOLVER):                             ',1X,I5,
!     3    /1X,'  SOLVER =',1X,A40,
!     4    /1X,'SOLVER PRINTOUT INTERVAL (IPRSOL):            ',1X,I5,
!     5    /1X,'SOLVER OUTPUT FLAG (MUTSOL):                  ',1X,I5,
!     6    /1X,'  0 = PRINTING EVERY ITERATION',
!     7    /1X,'  1 = LIMITED (TOTAL NUMBER OF ITERATIONS)',
!     8    /1X,'  2 = NO PRINTING',
!     9    /1X,'  3 = ONLY IF CONVERGENCE FAILS',
!     1    /1X,52('-'))
C
C---------ALLOCATE STORAGE FOR SOLVER DATA
        ALLOCATE(SWIDE4,SWIPCG)
C
C---------READ DATASET 2B IF NECESSARY
        SELECT CASE (NSOLVER)
          CASE (2)
            CALL URDCOM(In, IOUT, line)
            lloc = 1
            CALL URWORD(line, lloc, istart, istop, 2, i, r, IOUT, In)
            SWIPCG%MXITER = i
            CALL URWORD(line, lloc, istart, istop, 2, i, r, IOUT, In)
            SWIPCG%ITER1  = i
            CALL URWORD(line, lloc, istart, istop, 2, i, r, IOUT, In)
            SWIPCG%NPCOND = i
            CALL URWORD(line, lloc, istart, istop, 3, i, r, IOUT, In)
            SWIPCG%ZCLOSEPCG = r
            CALL URWORD(line, lloc, istart, istop, 3, i, r, IOUT, In)
            SWIPCG%RCLOSEPCG = r
            CALL URWORD(line, lloc, istart, istop, 3, i, r, IOUT, In)
            SWIPCG%RELAXPCG = r
            CALL URWORD(line, lloc, istart, istop, 2, i, r, IOUT, In)
            SWIPCG%NBPOL = i
            CALL URWORD(line, lloc, istart, istop, 3, i, r, IOUT, In)
            SWIPCG%DAMPPCG = ABS(r)
            IF ( r.LT.0 ) THEN
              CALL URWORD(line, lloc, istart, istop, 3, i, r, IOUT, In)
              SWIPCG%DAMPPCGT = r
            ELSE
              SWIPCG%DAMPPCGT = SWIPCG%DAMPPCG
            END IF
C             WRITE DATASET 2B FOR PCG SOLVER
            WRITE (IOUT,*) 
     1 'MXITER, ITER1,NPCOND,ZCLOSE, RCLOSE,RELAX, NBPOL,DAMP, DAMPT:'
            WRITE (IOUT,*) SWIPCG%MXITER, SWIPCG%ITER1,
     2                        SWIPCG%NPCOND,
     3                        SWIPCG%ZCLOSEPCG, SWIPCG%RCLOSEPCG,
     4                        SWIPCG%RELAXPCG, SWIPCG%NBPOL,
     5                        SWIPCG%DAMPPCG, SWIPCG%DAMPPCGT
        END SELECT
!02230   FORMAT(//1X,'SWI2 DATASET 2B',
!     2    /1X,'PRECONDITIONED CONJUGATE GRADIENT PARAMETERS',
!     3    /1X,52('-'),
!     4    /1X,'MAXIMUM NUMBER OF CALLS TO PCG (MXITER):      ',1X,I5,
!     5    /1X,'MAXIMUM ITERATIONS PER CALL TO PCG (ITER1):   ',1X,I5,
!     6    /1X,'MATRIX PRECONDITIONING TYPE (NPCOND):         ',1X,I5,
!     7    /1X,'  1 = MODIFIED INCOMPLETE CHOLESKY',
!     8    /1X,'  2 = NEUMAN POLYNOMIAL - THE MATRIX WILL BE SCALED',
!     9    /1X,'ZETA CLOSURE CRITERION (ZCLOSE):    ',1X,G15.5,
!     X    /1X,'RESIDUAL CLOSURE CRITERION (RCLOSE):',1X,G15.5,
!     1    /1X,'RELAXATION FACTOR (RELAX):          ',1X,G15.5,
!     2    /1X,'  ONLY USED WITH NPCOND = 1',
!     3    /1X,'POLYNOMIAL PRECONDITIONER PARAMETER (NBPOL):  ',1X,I5,
!     4    /1X,'  INTERNALLY CALCULATED IF NPCOND.NE.2',
!     5    /1X,'STEADY-STATE DAMPING (DAMPPCG):     ',1X,G15.5,
!     6    /1X,'TRANSIENT DAMPING (DAMPPCGT):       ',1X,G15.5,
!     7    /1X,52('-'))
C
C---------ALLOCATE ARRAYS
        IF ( NOBS.GT.0 ) THEN
          ALLOCATE(SWIOBS(NOBS))
        ELSE
          ALLOCATE(SWIOBS(1))
        END IF
        ALLOCATE(ZETA(NCOL,NROW,NLAY,NZONES+1))
        ALLOCATE(SSZ(NCOL,NROW,NLAY))
        ALLOCATE(IZONENR(NCOL,NROW,NLAY))
C---------ALLOCATE AND INITIALIZE SWI ZONE BUDGET DATA
        ALLOCATE(NBDITEMS)
        NBDITEMS = 5
        ALLOCATE(CUMBD(2,NBDITEMS,NZONES),INCBD(2,NBDITEMS,NZONES))
        ALLOCATE(RRATIN(NBDITEMS),RRATOUT(NBDITEMS))
        DO i = 1, NBDITEMS
          RRATIN(i)  = 0.0D0
          RRATOUT(i) = 0.0D0
          DO iz = 1, NZONES
            DO j = 1, 2
              CUMBD(j,i,iz) = 0.0
              INCBD(j,i,iz) = 0.0
            END DO
          END DO
        END DO
C---------ALLOCATE SOLUTION DATA
        ALLOCATE(NUS(NZONES))
        ALLOCATE(NUSRF(NZONES+1))
        ALLOCATE(DELNUS(NZONES))
        ALLOCATE(EPS(NZONES))

        ALLOCATE(IPLPOS(NCOL,NROW,NLAY,NZONES))
        ALLOCATE(RHSPRESWI(NCOL,NROW,NLAY))
        ALLOCATE(SWICR(NCOL,NROW,NLAY,NZONES))
        ALLOCATE(SWICC(NCOL,NROW,NLAY,NZONES))
        ALLOCATE(SWICUMCR(NCOL,NROW,NLAY,NZONES))
        ALLOCATE(SWICUMCC(NCOL,NROW,NLAY,NZONES))
        ALLOCATE(NUTOP(NCOL,NROW,NLAY),NUBOT(NCOL,NROW,NLAY))
        ALLOCATE(QLEXTRA(NCOL,NROW,NLAY))
        ALLOCATE(QREXTRA(NCOL,NROW,NLAY))
        ALLOCATE(QFEXTRA(NCOL,NROW,NLAY))
        ALLOCATE(QLEXTRACUM(NCOL,NROW,NLAY))
        ALLOCATE(QREXTRACUM(NCOL,NROW,NLAY))
        ALLOCATE(QFEXTRACUM(NCOL,NROW,NLAY))

        ALLOCATE(BRHS(NCOL,NROW,NZONES))
        ALLOCATE(ZETAOLD(NCOL,NROW,NLAY,NZONES+1))
        ALLOCATE(ZETASWITS0(NCOL,NROW,NLAY,NZONES+1))
        ALLOCATE(ZONECHG1(NCOL,NROW,NLAY,NZONES+1))
        ALLOCATE(ZONECHG2(NCOL,NROW,NLAY,NZONES+1))
        ALLOCATE(ZONEIMIX(NCOL,NROW,NLAY,NZONES+1))
        ALLOCATE(ICONV(NLAY))
        ALLOCATE(IBO(NCOL,NROW),SWIHCOF(NCOL,NROW))
        ALLOCATE(SWISOLCR(NCOL,NROW))
        ALLOCATE(SWISOLCC(NCOL,NROW))
        ALLOCATE(SWISOLCV(NCOL,NROW))
        ALLOCATE(DUM(NCOL,NROW))
C
C-------READ DATASET 3A PARAMETERS
        CALL URDCOM(In, IOUT, line)
        lloc = 1
        CALL URWORD(line, lloc, istart, istop, 3, i, TOESLOPE, IOUT, In)
        CALL URWORD(line, lloc, istart, istop, 3, i, TIPSLOPE, IOUT, In)
        CALL URWORD(line, lloc, istart, istop, 3, i,    ALPHA,-IOUT, In)
        IF ( ALPHA.NE.0.0 ) THEN
          CALL URWORD(line, lloc, istart, istop, 3, i,  BETA, IOUT, In)
        ELSE
          ALPHA = 0.1
          BETA  = 0.1
        END IF
        WRITE(IOUT,*) 'TOESLOPE, TIPSLOPE, ALPHA, BETA:'
        WRITE(IOUT,*) TOESLOPE, TIPSLOPE, ALPHA, BETA
C
C-------CONFIRM THAT VALID ALPHA AND BETA VALUES ARE SPECIFIED
        IF ( ALPHA.LE.0.0 .OR. ALPHA.GT.1.0 ) THEN
          WRITE (IOUT,2240) 'ALPHA',ALPHA
          CALL USTOP('SWI2 ERROR: ALPHA OUT OF BOUNDS (0.0,1.0]')
        END IF
        IF ( BETA.LE.0.0  .OR. BETA.GT.1.0  ) THEN
          WRITE (IOUT,2240) 'BETA',BETA
          CALL USTOP('SWI2 ERROR: BETA OUT OF BOUNDS (0.0,1.0]')
        END IF
02240   FORMAT(//1X,'SWI2 ERROR:',1X,A,1X,'(',G10.3,')',1X,
     2              'MUST BE GREATER THAN 0.0 AND LESS THAN OR ',
     3              'EQUAL TO 1.0')
C
C-------READ DATASET 3B PARAMETERS
        NADPTMX  = 1
        NADPTMN  = 1
        ADPTFCT  = 1.0
        IF ( iadptflg.NE.0 ) THEN        
          CALL URDCOM(In, IOUT, line)
          lloc = 1
          CALL URWORD(line,lloc,istart,istop,2,  NADPTMX, r,-IOUT, In)
          CALL URWORD(line,lloc,istart,istop,2,NADPTMN, r, IOUT, In)
          CALL URWORD(line,lloc,istart,istop,3,i,ADPTFCT,IOUT,In)
          IF ( NADPTMN.GT.NADPTMX ) THEN
            WRITE (IOUT,2245) NADPTMN, NADPTMX
            CALL USTOP('SWI2 ERROR: NADPTMN EXCEEDS NADPTMX')
          END IF
          IF ( ADPTFCT.LE.0.0 ) THEN
            WRITE (IOUT,2250) ADPTFCT
            CALL USTOP('SWI2 ERROR: ADPTFCT MUST BE > ZERO')
          END IF
        END IF
02245   FORMAT(//1X,'SWI2 ERROR: NADPTMN (',I10,')',1X,
     2              'EXCEEDS NAPTMX (',I10,')')
02250   FORMAT(//1X,'SWI2 ERROR: ADPTFCT (',G10.3,')',1X,
     2              'MUST BE GREATER THAN ZERO')

        IADPT      = NADPTMN
        IADPTMOD   = -1
        ADPTVAL    = 1.0

        NADPTFLG = 0
        IF ( NADPTMX.GT.1 ) NADPTFLG = 1

!        WRITE (IOUT,2255) TOESLOPE,TIPSLOPE,ALPHA,BETA
        IF ( iadptflg.NE.0 ) THEN
          WRITE (IOUT,*) 'NADPTMN,NADPTMX,ADPTFCT:'
          WRITE (IOUT,*) NADPTMN,NADPTMX,ADPTFCT
        END IF

!02255   FORMAT(//1X,'SWI2 DATASET 3A',/,1X,52('-'),
!     2    /1X,'MAXIMUM TOE SURFACE SLOPE (TOESLOPE):    ',1X,G10.3,
!     3    /1X,'MAXIMUM TIP SURFACE SLOPE (TIPSLOPE):    ',1X,G10.3,
!     4    /1X,'EXCESS SURFACE SLOPE FRACTION (ALPHA):   ',1X,G10.3,
!     5    /1X,'MINIMUM ZONE THICKNESS FRACTION (BETA):  ',1X,G10.3,
!     6    /1X,52('-'))
!02260   FORMAT(//1X,'SWI2 DATASET 3B',/,1X,52('-'),
!     1    /1X,'MINIMUM SWI SUB-TIME STEPS  (NADPTMN):   ',1X,I5,
!     2    /1X,'MAXIMUM SWI SUB-TIME STEPS  (NADPTMX):   ',1X,I5,
!     3    /1X,'MAXIMUM ZETA SLOPE FRACTION (ADPTFCT):   ',1X,G10.3,
!     4    /1X,52('-'))
02270   FORMAT(1X,'Invalid ISOURCE value (',I4,
     2    ') at cell (layer,row,col)',I4,',',I4,',',I4)
C
C---------ALLOCATE AND INITIALIZE STORAGE FOR SWI ADAPTIVE TIME STEP SUMMARY
        IF ( NADPTFLG.NE.0 ) THEN
          i = 0
          DO n = 1, NPER
            i = i + NSTP(n)
          END DO
          ALLOCATE(NADPTSUM(i))
          ALLOCATE(RADPTSUM(i))
          DO n = 1, i
            NADPTSUM(n) = 0
            RADPTSUM(n) = 0.0
          END DO
        ELSE
          ALLOCATE(NADPTSUM(1))
          ALLOCATE(RADPTSUM(1))
          NADPTSUM(1) = 0
          RADPTSUM(1) = 0.0
        END IF
        ALLOCATE(IADPTSUM)
        IADPTSUM = 1
C
C         DATASET 4
C---------READ DENSITY DATA BASED ON USER-SPECIFIED DENSITY MODEL
C         CONSTANT ZONE DENSITY MODEL
        CALL SSWI2_RD_COMM(In)
        DENSITYMODEL: IF (ISTRAT.EQ.1) THEN
C
C-----------READ NU FOR EACH ZONE (NZONES)
          CALL U1DREL(NUS,ANAME(1),NZONES,In,IOUT)
C-----------SET EPS EQUAL TO ZERO
          DO iz = 1, NZONES
            EPS(iz) = 0.0
          END DO
C         LINEAR ZONE DENSITY MODEL
        ELSEIF (ISTRAT.EQ.0) THEN
C
C-----------READ NUSRF FOR EACH SURFACE (NZONES+1)
          CALL U1DREL(NUSRF,ANAME(2),NZONES+1,In,IOUT)
C
C-----------CALCULATE NU AND EPS FROM NUSRF
          DO iz = 1, NZONES
            NUS(iz) = 0.5*(NUSRF(iz)+NUSRF(iz+1))
            EPS(iz) = (NUSRF(iz+1)-NUSRF(iz)) / 6
          END DO
        END IF DENSITYMODEL
C
C---------CALCULATE DELNUS FROM NUS
        DELNUS(1)=NUS(1);
        DO iz = 2, NZONES
          DELNUS(iz)=(NUS(iz)-NUS(iz-1))
        END DO
C
C---------DATASET 5
C---------READ ZETA FOR EACH ACTIVE SURFACE (ZONES 2 TO NZONES)
        CALL SSWI2_RD_COMM(In)
        IZ_ZETA: DO iz=2,NZONES
          K_ZETA: DO k=1,NLAY
            kk = k
            WRITE(ZETANAME,111) iz-1
            CALL U2DREL(ZETA(1:NCOL,1:NROW,k,iz),ZETANAME,
     2                  NROW,NCOL,kk,In,IOUT)
C
C-------------LOOP THROUGH EACH ROW AND COLUMN
C             RESET ZETA IF SPECIFIED ZETA IS GREATER THAN THE TOP
C             OF THE CURRENT CELL OR LESS THAN THE BOTTOM OF THE CURRENT
C             CELL - IF ZETA IS RESET TO THE TOP OR BOTTOM BASED ON IF
C             THE SPECIFIED ZETA VALUE IS CLOSER TO THE TOP OR BOTTOM
            d = 0.001
            I_ZETA: DO i = 1, NROW
              J_ZETA: DO j = 1, NCOL
                ttop = BOTM(j,i,LBOTM(k)-1)
                bbot = BOTM(j,i,LBOTM(k))
                z = ZETA(j,i,k,iz)
                IF (z.NE.bbot) THEN
                  IF (z.LT.(bbot+d)) THEN
                    ZETA(j,i,k,iz) = bbot
                  END IF
                END IF
                IF (z.GT.(ttop-d)) ZETA(j,i,k,iz) = ttop
              END DO J_ZETA
            END DO I_ZETA
          END DO K_ZETA
        END DO IZ_ZETA
C
C---------DATASET 6
C---------READ SSZ FOR EACH LAYER
        CALL SSWI2_RD_COMM(In)
        K_SSZ: DO k = 1, NLAY
          kk=k
          CALL U2DREL(SSZ(1:NCOL,1:NROW,k),ANAME(3),
     2                NROW,NCOL,kk,In,IOUT)
        END DO K_SSZ
C
C---------DATASET 7
C---------READ IZONENR FOR EACH LAYER
        ierr = 0
        CALL SSWI2_RD_COMM(In)
        K_IZONENR: DO k = 1, NLAY
          kk=k
          CALL U2DINT(IZONENR(1:NCOL,1:NROW,kk),ANAME(4),
     2                NROW,NCOL,kk,In,IOUT)
C
C-----------CHECK FOR INVALID ZONE NUMBERS
          DO i = 1, NROW
            DO j = 1, NCOL
              IF ( ABS(IZONENR(j,i,k)).GT.NZONES ) THEN
                ierr = ierr + 1
                IF ( ierr.EQ.1 ) WRITE(IOUT,'(//)')
                WRITE(IOUT,2270) IZONENR(j,i,k),k,i,j 
              END IF
            END DO
          END DO
      END DO K_IZONENR
      IF ( ierr.GT.0 ) THEN
        WRITE (IOUT,'(A)') 'INVALID ISOURCE VALUES SPECIFIED'
        WRITE (IOUT,'(A,1X,I4)') 
     2   'MAXIMUM ISOURCE VALUE +/-',NZONES
        CALL USTOP('INVALID ISOURCE VALUES SPECIFIED')
      END IF
C
C---------READ OBSERVATION DATA
        IF ( NOBS.GT.0 ) THEN
          DO n = 1, NOBS
C-------------READ OBSERVATION DATA
            CALL URDCOM(In, IOUT, line)
            lloc = 1
            CALL URWORD(line, lloc, istart, istop, 0, i, r, IOUT, In)
            SWIOBS(n)%OBSNAM = line(istart:istop)
            CALL URWORD(line, lloc, istart, istop, 2, k, r, IOUT, In)
            SWIOBS(n)%KLAY = k
            CALL URWORD(line, lloc, istart, istop, 2, i, r, IOUT, In)
            SWIOBS(n)%IROW = i
            CALL URWORD(line, lloc, istart, istop, 2, j, r, IOUT, In)
            SWIOBS(n)%JCOL = j
            Write(IOUT,*) 'OBSNAM:'
            Write(IOUT,*) SWIOBS(n)%OBSNAM
            Write(IOUT,*) 'KLAY, IROW, JCOL:'
            Write(IOUT,*) SWIOBS(n)%KLAY, SWIOBS(n)%IROW, SWIOBS(n)%JCOL
          END DO
C-------------WRITE OBSERVATION DATA TO IOUT
!          WRITE (IOUT,2110)
!          DO n = 1, NOBS
!            WRITE (IOUT,2120)
!     2        n, SWIOBS(n)%KLAY, SWIOBS(n)%IROW, SWIOBS(n)%JCOL,
!     3        ADJUSTR(TRIM(SWIOBS(n)%OBSNAM))
!          END DO
C-----------CHECK FOR INVALID OBSERVATION LOCATIONS
          ierr = 0
!          WRITE(IOUT,2140)
          DO n = 1, NOBS
            k = SWIOBS(n)%KLAY
            i = SWIOBS(n)%IROW
            j = SWIOBS(n)%JCOL
            IF ( k.LT.1 .OR. k.GT.NLAY ) THEN
              ierr = ierr + 1
              WRITE (IOUT,2150) n, 'LAYER ', k, 1, NLAY
            END IF
            IF ( i.LT.1 .OR. i.GT.NROW ) THEN
              ierr = ierr + 1
              WRITE (IOUT,2150) n, 'ROW   ', i, 1, NROW
            END IF
            IF ( j.LT.1 .OR. j.GT.NCOL ) THEN
              ierr = ierr + 1
              WRITE (IOUT,2150) n, 'COLUMN', j, 1, NCOL
            END IF
          END DO
          IF ( ierr.GT.0 ) THEN
            CALL USTOP('SWI2 ERROR: INVALID OBSERVATION LOCATIONS')
!          ELSE
!            WRITE (IOUT,2160)
          END IF
        END IF
C
C---------ALLOCATE SPACE FOR THE SOLVER ARRAYS
C         DIRECT SOLVER (DE4)
        SOLVERPARAM: IF ( NSOLVER.EQ.1 ) THEN
          SWIDE4%ITMX = 1
C-----------SPECIFY DEFAULT DE4 SOLVER PARAMETERS
          SWIDE4%MXITER = SWIDE4%ITMX
          SWIDE4%NITERDE4 = 1
          SWIDE4%ACCLDE4 = 1.0
          SWIDE4%HCLOSEDE4 = 1.0E-05
          SWIDE4%IFREQ = 3
          SWIDE4%DELTL = 1.
C           INITIALIZE DIMENSION PARAMETERS
          SWIDE4%NODES=NCOL*NROW*NLAYSWI
          SWIDE4%NHALFU=(SWIDE4%NODES-1)/2 + 1
          SWIDE4%NHALFL=SWIDE4%NODES-SWIDE4%NHALFU
          SWIDE4%NBWL = 0
          SWIDE4%NUPL = 0
          SWIDE4%NLOWL = 0
          SWIDE4%ID4DIM = 7
C           CALCULATE SOLVER DIMENSIONS
          IF(NLAYSWI.LE.NCOL .AND. NLAYSWI.LE.NROW) THEN
             IF(NLAYSWI.EQ.1) SWIDE4%ID4DIM=5
             IF(NCOL.GE.NROW) THEN
                SWIDE4%ID4DIR=1
                SWIDE4%NBWGRD=NROW*NLAYSWI+1
             ELSE
                SWIDE4%ID4DIR=2
                SWIDE4%NBWGRD=NCOL*NLAYSWI+1
             END IF
          ELSE IF(NROW.LE.NCOL .AND. NROW.LE.NLAYSWI) THEN
             IF(NROW.EQ.1) SWIDE4%ID4DIM=5
             IF(NCOL.GE.NLAYSWI) THEN
                SWIDE4%ID4DIR=3
                SWIDE4%NBWGRD=NROW*NLAYSWI+1
             ELSE
                SWIDE4%ID4DIR=4
                SWIDE4%NBWGRD=NROW*NCOL+1
             END IF
          ELSE
             IF(NCOL.EQ.1) SWIDE4%ID4DIM=5
             IF(NROW.GE.NLAYSWI) THEN
                SWIDE4%ID4DIR=5
                SWIDE4%NBWGRD=NCOL*NLAYSWI+1
             ELSE
                SWIDE4%ID4DIR=6
                SWIDE4%NBWGRD=NCOL*NROW+1
             END IF
          END IF
          SWIDE4%MXUP=SWIDE4%NHALFU
          SWIDE4%MXLOW=SWIDE4%NHALFL
          SWIDE4%MXBW=SWIDE4%NBWGRD + 4
          SWIDE4%MXEQ=SWIDE4%MXUP+SWIDE4%MXLOW
C         ALLOCATE DE4 ARRAYS
          ALLOCATE (SWIDE4%AU(SWIDE4%ID4DIM,SWIDE4%MXUP))
          ALLOCATE (SWIDE4%IUPPNT(SWIDE4%ID4DIM,SWIDE4%MXUP))
          ALLOCATE (SWIDE4%AL(SWIDE4%MXBW,SWIDE4%MXLOW))
          ALLOCATE (SWIDE4%IEQPNT(NCOL,NROW,NLAYSWI))
          ALLOCATE (SWIDE4%D4B(SWIDE4%MXEQ))
          ALLOCATE (SWIDE4%LRCHDE4(3,SWIDE4%ITMX))
          ALLOCATE (SWIDE4%HDCGDE4(SWIDE4%ITMX))
C           INITIALIZE FLOAT ARRAYS
          DO i = 1, SWIDE4%MXUP
            DO j = 1, SWIDE4%ID4DIM
              SWIDE4%AU(j,i) = 0.0
            END DO
          END DO
          DO i = 1, SWIDE4%MXLOW
            DO j = 1, SWIDE4%ID4DIM
              SWIDE4%AL(j,i) = 0.0
            END DO
          END DO
          DO i = 1, SWIDE4%MXEQ
            SWIDE4%D4B(i) = 0.0
          END DO
C         PRECONDITIONED CONJUGATE GRADIENT SOLVER (PCG4)
        ELSE IF ( NSOLVER.EQ.2 ) THEN
C           INITIALIZE DIMENSION PARAMETERS
          SWIPCG%NODES=NCOL*NROW*NLAYSWI
C---------ALLOCATE SPACE FOR THE PCG ARRAYS
          ALLOCATE (SWIPCG%VPCG(NCOL,NROW,NLAYSWI))
          ALLOCATE (SWIPCG%SS(NCOL,NROW,NLAYSWI))
          ALLOCATE (SWIPCG%P(NCOL,NROW,NLAYSWI))
          ALLOCATE (SWIPCG%HPCG(NCOL,NROW,NLAYSWI))
          ALLOCATE (SWIPCG%CD(NCOL,NROW,NLAYSWI))
          IF(SWIPCG%NPCOND.EQ.2) THEN
             ALLOCATE (SWIPCG%HCSV(NCOL,NROW,NLAYSWI))
          ELSE
             ALLOCATE (SWIPCG%HCSV(1,1,1))
          END IF
          itmem=SWIPCG%MXITER*SWIPCG%ITER1
          ALLOCATE (SWIPCG%HCHG(itmem))
          ALLOCATE (SWIPCG%LHCH(3,itmem))
          ALLOCATE (SWIPCG%RCHG(itmem))
          ALLOCATE (SWIPCG%LRCHPCG(3,itmem))
          ALLOCATE (SWIPCG%IT1(itmem))
C           INITIALIZE FLOAT ARRAYS
          DO k = 1, NLAYSWI
            DO i = 1, NROW
              DO j = 1, NCOL
                SWIPCG%VPCG(j,i,k) = 0.0
                SWIPCG%SS(j,i,k)   = 0.0
                SWIPCG%P(j,i,k)    = 0.0
                SWIPCG%HPCG(j,i,k) = 0.0
                SWIPCG%CD(j,i,k)   = 0.0
                IF ( SWIPCG%NPCOND.EQ.2 ) THEN
                  SWIPCG%HCSV(j,i,k) = 0.0
                END IF
              END DO
            END DO
          END DO
      END IF SOLVERPARAM
C
C-------SET ICONV FOR EACH LAYER
      DO k = 1, NLAY
        ic = 1
        IF ( Ibcf.GT.0 ) THEN
          IF ( LCB(k).EQ.0 .OR. LCB(k).EQ.2 ) ic = 0
        ELSE IF ( Ilpf.GT.0 ) THEN
          IF ( LCL(k).EQ.0 ) ic = 0
        ELSE IF ( Ihuf.GT.0 ) THEN
          IF ( LCH(k).EQ.0 ) THEN
            ic = 0
          ELSE
            WRITE (IOUT,'(A)') 
     2        'CONVERTIBLE HUF LAYER INCONSISTENT WITH SWI2'
            CALL USTOP('CONVERTIBLE HUF LAYER INCONSISTENT WITH SWI2') 
          END IF
        ELSE IF ( Iupw.GT.0 ) THEN
          IF ( LCU(k).EQ.0 ) ic = 0
        ELSE
          WRITE (IOUT,'(A)') 
     2      'FLOW PACKAGE SPECIFIED INCONSISTENT WITH SWI2'
          CALL USTOP('FLOW PACKAGE SPECIFIED INCONSISTENT WITH SWI2') 
        END IF
        ICONV(k) = ic
      END DO
C-------SET SC1 AND SC2 USED BY SWI2 TO CALCULATE MODFLOW STORAGE CHANGES
      IF ( Ibcf.GT.0 ) THEN
        SC1 => SC1B
        SC2 => SC2B
      ELSE IF ( Ilpf.GT.0 ) THEN
        SC1 => SC1L
        SC2 => SC2L
      ELSE IF ( Ihuf.GT.0 ) THEN
        SC1 => SC1H
        SC2 => SC1H
      ELSE IF ( Iupw.GT.0 ) THEN
        SC1 => SC1U
        SC2 => SC2U
      END IF
C
C---------SET POINTERS FOR GRID
        CALL SGWF2SWI2PSV(Igrid)
C
C---------RETURN
        RETURN
      END SUBROUTINE GWF2SWI2AR
C
C      
!      SUBROUTINE GWF2SWI2AD(Kkstp,Kkper,Igrid)
C
C     ******************************************************************
C     SET FIRST AND LAST SURFACE TO TOP AND BOTOM OF LAYER ON FIRST TIME
C     STEP OF THE FIRST STRESS PERIOD AND ADVANCE ZETAOLD AND ZETASWITS0 
C     TO ZETA EVERY TIME STEP.
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C
!      SUBROUTINE GWF2SWI2FM(Kkstp,Kkper,Kkiter,Igrid)
C
C     ******************************************************************
C     ADD SWI2 PACKAGE TERMS TO RHS AND HCOF
C
C     SWI2 FORMULATE (GWF2SWI2FM) NEEDS TO BE THE LAST PACKAGE ENTRY
C     SINCE SWI2 SAVES THE RHS (RHSPRESWI) PRIOR TO ADDING SWI TERMS
C     RHSPRESWI IS USED TO CALCULATE BOUNDARY CONDITION FLUXES 
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C
C
!      SUBROUTINE GWF2SWI2BD(Kkstp,Kkper,Igrid)
C
C     ******************************************************************
C     CALCULATE GLOBAL SWI2 BUDGET TERMS FOR MODFLOW, CALCULATE ZONE
C     BUDGETS, SAVE ZETA, SAVE SWI2 CBC DATA, AND SAVE SWI2 OBSERVATIONS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C
C
!      SUBROUTINE SSWI2_QR(J,I,K,IZ,NCOL,NROW,NLAY,NZONES,
!     &                   Q,HNEW,ZETA,DELNUS,EPS,SWICUMCR,SWICR)
C     ******************************************************************
C     SWI2 PACKAGE - CALCULATE FLUX IN THE ROW DIRECTION
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C
!      SUBROUTINE SSWI2_QC(J,I,K,IZ,NCOL,NROW,NLAY,NZONES,
!     &                   Q,HNEW,ZETA,DELNUS,EPS,SWICUMCC,SWICC)
C
C     ******************************************************************
C     SWI2 PACKAGE - CALCULATE FLUX IN THE COLUMN DIRECTION
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C
!      SUBROUTINE SSWI2_SD(J,I,NCOL,NROW,IPOS,FAC,B,CRLAY,CCLAY,VAR)
C
C     ******************************************************************
C     SWI2 PACKAGE - CALCULATE FLUX IN THE COLUMN AND ROW DIRECTIONS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C
!      SUBROUTINE SSWI2_SR(J,I,NCOL,NROW,IPOS,FAC,B,CRLAY,CCLAY,VAR)
C
C     ******************************************************************
C     SWI2 PACKAGE - CALCULATE FLUX IN THE COLUMN AND ROW DIRECTIONS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C
C
!      SUBROUTINE SSWI2_TIPTOE(Kkstp,Kkper)
C
C     ******************************************************************
C     MOVE TIPS AND TOES FOR SWI2 PACKAGE
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C
C
      SUBROUTINE GWF2SWI2DA(Igrid)
C
C     ******************************************************************
C     DEALLOCATE SWI2 PACKAGE DATA FOR A GRID
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GWFSWIMODULE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Igrid
C       + + + CODE + + +
        DEALLOCATE(GWFSWIDAT(Igrid)%NSRF)
        DEALLOCATE(GWFSWIDAT(Igrid)%ISTRAT)
        DEALLOCATE(GWFSWIDAT(Igrid)%NZONES)

        DEALLOCATE(GWFSWIDAT(Igrid)%NADPTFLG)
        DEALLOCATE(GWFSWIDAT(Igrid)%NADPTMX)
        DEALLOCATE(GWFSWIDAT(Igrid)%NADPTMN)
        DEALLOCATE(GWFSWIDAT(Igrid)%ADPTFCT)
        DEALLOCATE(GWFSWIDAT(Igrid)%IADPT)
        DEALLOCATE(GWFSWIDAT(Igrid)%IADPTMOD)
        DEALLOCATE(GWFSWIDAT(Igrid)%ADPTVAL)
        DEALLOCATE(GWFSWIDAT(Igrid)%SWIDELT)
        DEALLOCATE(GWFSWIDAT(Igrid)%NADPTSUM)
        DEALLOCATE(GWFSWIDAT(Igrid)%RADPTSUM)
        DEALLOCATE(GWFSWIDAT(Igrid)%IADPTSUM)

        DEALLOCATE(GWFSWIDAT(Igrid)%NOBS)
        DEALLOCATE(GWFSWIDAT(Igrid)%IOBSHEADER)

        DEALLOCATE(GWFSWIDAT(Igrid)%ISWIZT)
        DEALLOCATE(GWFSWIDAT(Igrid)%ISWICB)
        DEALLOCATE(GWFSWIDAT(Igrid)%ISWIOBS)

        DEALLOCATE(GWFSWIDAT(Igrid)%NSWIOPT)

        DEALLOCATE(GWFSWIDAT(Igrid)%NLAYSWI)
C         SOLVER
        DEALLOCATE(GWFSWIDAT(Igrid)%NSOLVER)
        DEALLOCATE(GWFSWIDAT(Igrid)%IPRSOL)
        DEALLOCATE(GWFSWIDAT(Igrid)%MUTSOL)
C         SWI PARAMETERS
        DEALLOCATE(GWFSWIDAT(Igrid)%TOESLOPE)
        DEALLOCATE(GWFSWIDAT(Igrid)%TIPSLOPE)
        DEALLOCATE(GWFSWIDAT(Igrid)%ALPHA)
        DEALLOCATE(GWFSWIDAT(Igrid)%BETA)
        DEALLOCATE(GWFSWIDAT(Igrid)%ICONV)
        DEALLOCATE(GWFSWIDAT(Igrid)%IBO)
        DEALLOCATE(GWFSWIDAT(Igrid)%SWIHCOF)
        DEALLOCATE(GWFSWIDAT(Igrid)%SWISOLCR)
        DEALLOCATE(GWFSWIDAT(Igrid)%SWISOLCC)
        DEALLOCATE(GWFSWIDAT(Igrid)%SWISOLCV)
        DEALLOCATE(GWFSWIDAT(Igrid)%ZETA)
        DEALLOCATE(GWFSWIDAT(Igrid)%ZETAOLD)
        DEALLOCATE(GWFSWIDAT(Igrid)%ZETASWITS0)
        DEALLOCATE(GWFSWIDAT(Igrid)%ZONECHG1)
        DEALLOCATE(GWFSWIDAT(Igrid)%ZONECHG2)
        DEALLOCATE(GWFSWIDAT(Igrid)%ZONEIMIX)
        DEALLOCATE(GWFSWIDAT(Igrid)%SSZ)
        DEALLOCATE(GWFSWIDAT(Igrid)%EPS)
        DEALLOCATE(GWFSWIDAT(Igrid)%NUS)
        DEALLOCATE(GWFSWIDAT(Igrid)%DELNUS)
        DEALLOCATE(GWFSWIDAT(Igrid)%NUSRF)
        DEALLOCATE(GWFSWIDAT(Igrid)%SWICR)
        DEALLOCATE(GWFSWIDAT(Igrid)%SWICC)
        DEALLOCATE(GWFSWIDAT(Igrid)%SWICUMCR)
        DEALLOCATE(GWFSWIDAT(Igrid)%SWICUMCC)
        DEALLOCATE(GWFSWIDAT(Igrid)%NUTOP)
        DEALLOCATE(GWFSWIDAT(Igrid)%NUBOT)
        DEALLOCATE(GWFSWIDAT(Igrid)%QLEXTRA)
        DEALLOCATE(GWFSWIDAT(Igrid)%QREXTRA)
        DEALLOCATE(GWFSWIDAT(Igrid)%QFEXTRA)
        DEALLOCATE(GWFSWIDAT(Igrid)%QLEXTRACUM)
        DEALLOCATE(GWFSWIDAT(Igrid)%QREXTRACUM)
        DEALLOCATE(GWFSWIDAT(Igrid)%QFEXTRACUM)
        DEALLOCATE(GWFSWIDAT(Igrid)%BRHS)
        DEALLOCATE(GWFSWIDAT(Igrid)%DUM)
        DEALLOCATE(GWFSWIDAT(Igrid)%RHSPRESWI)
        DEALLOCATE(GWFSWIDAT(Igrid)%IPLPOS)
        DEALLOCATE(GWFSWIDAT(Igrid)%IZONENR)
        DEALLOCATE(GWFSWIDAT(Igrid)%NBDITEMS)
        DEALLOCATE(GWFSWIDAT(Igrid)%CUMBD)
        DEALLOCATE(GWFSWIDAT(Igrid)%INCBD)
        DEALLOCATE(GWFSWIDAT(Igrid)%RRATIN)
        DEALLOCATE(GWFSWIDAT(Igrid)%RRATOUT)
        DEALLOCATE(GWFSWIDAT(Igrid)%SWIOBS)
        DEALLOCATE(GWFSWIDAT(Igrid)%SWIDE4)
        DEALLOCATE(GWFSWIDAT(Igrid)%SWIPCG)
C
C-------RETURN
        RETURN
      END SUBROUTINE GWF2SWI2DA
C
C
      SUBROUTINE SGWF2SWI2PNT(Igrid)
C
C     ******************************************************************
C     SET POINTERS TO SWI2 PACKAGE DATA FOR A GRID
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GWFSWIMODULE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Igrid
C       + + + CODE + + +
        NSRF=>GWFSWIDAT(Igrid)%NSRF
        ISTRAT=>GWFSWIDAT(Igrid)%ISTRAT
        NZONES=>GWFSWIDAT(Igrid)%NZONES

        NADPTFLG=>GWFSWIDAT(Igrid)%NADPTFLG
        NADPTMX=>GWFSWIDAT(Igrid)%NADPTMX
        NADPTMN=>GWFSWIDAT(Igrid)%NADPTMN
        ADPTFCT=>GWFSWIDAT(Igrid)%ADPTFCT
        IADPT=>GWFSWIDAT(Igrid)%IADPT
        IADPTMOD=>GWFSWIDAT(Igrid)%IADPTMOD
        ADPTVAL=>GWFSWIDAT(Igrid)%ADPTVAL
        SWIDELT=>GWFSWIDAT(Igrid)%SWIDELT
        NADPTSUM=>GWFSWIDAT(Igrid)%NADPTSUM
        RADPTSUM=>GWFSWIDAT(Igrid)%RADPTSUM
        IADPTSUM=>GWFSWIDAT(Igrid)%IADPTSUM

        NOBS=>GWFSWIDAT(Igrid)%NOBS
        IOBSHEADER=>GWFSWIDAT(Igrid)%IOBSHEADER

        ISWIZT=>GWFSWIDAT(Igrid)%ISWIZT
        ISWICB=>GWFSWIDAT(Igrid)%ISWICB
        ISWIOBS=>GWFSWIDAT(Igrid)%ISWIOBS

        NSWIOPT=>GWFSWIDAT(Igrid)%NSWIOPT

        NLAYSWI=>GWFSWIDAT(Igrid)%NLAYSWI
C         SOLVER
        NSOLVER=>GWFSWIDAT(Igrid)%NSOLVER
        IPRSOL=>GWFSWIDAT(Igrid)%IPRSOL
        MUTSOL=>GWFSWIDAT(Igrid)%MUTSOL
C         SWI PARAMETERS
        TOESLOPE=>GWFSWIDAT(Igrid)%TOESLOPE
        TIPSLOPE=>GWFSWIDAT(Igrid)%TIPSLOPE
        ALPHA=>GWFSWIDAT(Igrid)%ALPHA
        BETA=>GWFSWIDAT(Igrid)%BETA
        ICONV=>GWFSWIDAT(Igrid)%ICONV
        IBO=>GWFSWIDAT(Igrid)%IBO
        SWIHCOF=>GWFSWIDAT(Igrid)%SWIHCOF
        SWISOLCR=>GWFSWIDAT(Igrid)%SWISOLCR
        SWISOLCC=>GWFSWIDAT(Igrid)%SWISOLCC
        SWISOLCV=>GWFSWIDAT(Igrid)%SWISOLCV
        ZETA=>GWFSWIDAT(Igrid)%ZETA
        ZETAOLD=>GWFSWIDAT(Igrid)%ZETAOLD
        ZETASWITS0=>GWFSWIDAT(Igrid)%ZETASWITS0
        ZONECHG1=>GWFSWIDAT(Igrid)%ZONECHG1
        ZONECHG2=>GWFSWIDAT(Igrid)%ZONECHG2
        ZONEIMIX=>GWFSWIDAT(Igrid)%ZONEIMIX
        SSZ=>GWFSWIDAT(Igrid)%SSZ
        EPS=>GWFSWIDAT(Igrid)%EPS
        NUS=>GWFSWIDAT(Igrid)%NUS
        DELNUS=>GWFSWIDAT(Igrid)%DELNUS
        NUSRF=>GWFSWIDAT(Igrid)%NUSRF
        SWICR=>GWFSWIDAT(Igrid)%SWICR
        SWICC=>GWFSWIDAT(Igrid)%SWICC
        SWICUMCR=>GWFSWIDAT(Igrid)%SWICUMCR
        SWICUMCC=>GWFSWIDAT(Igrid)%SWICUMCC
        NUTOP=>GWFSWIDAT(Igrid)%NUTOP
        NUBOT=>GWFSWIDAT(Igrid)%NUBOT
        QLEXTRA=>GWFSWIDAT(Igrid)%QLEXTRA
        QREXTRA=>GWFSWIDAT(Igrid)%QREXTRA
        QFEXTRA=>GWFSWIDAT(Igrid)%QFEXTRA
        QLEXTRACUM=>GWFSWIDAT(Igrid)%QLEXTRACUM
        QREXTRACUM=>GWFSWIDAT(Igrid)%QREXTRACUM
        QFEXTRACUM=>GWFSWIDAT(Igrid)%QFEXTRACUM
        BRHS=>GWFSWIDAT(Igrid)%BRHS
        DUM=>GWFSWIDAT(Igrid)%DUM
        RHSPRESWI=>GWFSWIDAT(Igrid)%RHSPRESWI
        IPLPOS=>GWFSWIDAT(Igrid)%IPLPOS
        IZONENR=>GWFSWIDAT(Igrid)%IZONENR
        NBDITEMS=>GWFSWIDAT(Igrid)%NBDITEMS
        CUMBD=>GWFSWIDAT(Igrid)%CUMBD
        INCBD=>GWFSWIDAT(Igrid)%INCBD
        RRATIN=>GWFSWIDAT(Igrid)%RRATIN
        RRATOUT=>GWFSWIDAT(Igrid)%RRATOUT
        SC1=>GWFSWIDAT(Igrid)%SC1
        SC2=>GWFSWIDAT(Igrid)%SC2
        SWIOBS=>GWFSWIDAT(Igrid)%SWIOBS
        SWIDE4=>GWFSWIDAT(Igrid)%SWIDE4
        SWIPCG=>GWFSWIDAT(Igrid)%SWIPCG
C
C---------RETURN
        RETURN
      END SUBROUTINE SGWF2SWI2PNT
C
C
      SUBROUTINE SGWF2SWI2PSV(Igrid)
C
C     ******************************************************************
C     SAVE POINTERS TO SWI2 PACKAGE DATA FOR A GRID
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GWFSWIMODULE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Igrid
C       + + + CODE + + +
        GWFSWIDAT(Igrid)%NSRF=>NSRF
        GWFSWIDAT(Igrid)%ISTRAT=>ISTRAT
        GWFSWIDAT(Igrid)%NZONES=>NZONES

        GWFSWIDAT(Igrid)%NADPTFLG=>NADPTFLG
        GWFSWIDAT(Igrid)%NADPTMX=>NADPTMX
        GWFSWIDAT(Igrid)%NADPTMN=>NADPTMN
        GWFSWIDAT(Igrid)%ADPTFCT=>ADPTFCT
        GWFSWIDAT(Igrid)%IADPT=>IADPT
        GWFSWIDAT(Igrid)%IADPTMOD=>IADPTMOD
        GWFSWIDAT(Igrid)%ADPTVAL=>ADPTVAL
        GWFSWIDAT(Igrid)%SWIDELT=>SWIDELT
        GWFSWIDAT(Igrid)%NADPTSUM=>NADPTSUM
        GWFSWIDAT(Igrid)%RADPTSUM=>RADPTSUM
        GWFSWIDAT(Igrid)%IADPTSUM=>IADPTSUM

        GWFSWIDAT(Igrid)%NOBS=>NOBS
        GWFSWIDAT(Igrid)%IOBSHEADER=>IOBSHEADER

        GWFSWIDAT(Igrid)%ISWIZT=>ISWIZT
        GWFSWIDAT(Igrid)%ISWICB=>ISWICB
        GWFSWIDAT(Igrid)%ISWIOBS=>ISWIOBS

        GWFSWIDAT(Igrid)%NSWIOPT=>NSWIOPT

        GWFSWIDAT(Igrid)%NLAYSWI=>NLAYSWI
C         SOLVER
        GWFSWIDAT(Igrid)%NSOLVER=>NSOLVER
        GWFSWIDAT(Igrid)%IPRSOL=>IPRSOL
        GWFSWIDAT(Igrid)%MUTSOL=>MUTSOL
C         SWI PARAMETERS
        GWFSWIDAT(Igrid)%TOESLOPE=>TOESLOPE
        GWFSWIDAT(Igrid)%TIPSLOPE=>TIPSLOPE
        GWFSWIDAT(Igrid)%ALPHA=>ALPHA
        GWFSWIDAT(Igrid)%BETA=>BETA
        GWFSWIDAT(Igrid)%ICONV=>ICONV
        GWFSWIDAT(Igrid)%IBO=>IBO
        GWFSWIDAT(Igrid)%SWIHCOF=>SWIHCOF
        GWFSWIDAT(Igrid)%SWISOLCR=>SWISOLCR
        GWFSWIDAT(Igrid)%SWISOLCC=>SWISOLCC
        GWFSWIDAT(Igrid)%SWISOLCV=>SWISOLCV
        GWFSWIDAT(Igrid)%ZETA=>ZETA
        GWFSWIDAT(Igrid)%ZETAOLD=>ZETAOLD
        GWFSWIDAT(Igrid)%ZETASWITS0=>ZETASWITS0
        GWFSWIDAT(Igrid)%ZONECHG1=>ZONECHG1
        GWFSWIDAT(Igrid)%ZONECHG2=>ZONECHG2
        GWFSWIDAT(Igrid)%ZONEIMIX=>ZONEIMIX
        GWFSWIDAT(Igrid)%SSZ=>SSZ
        GWFSWIDAT(Igrid)%EPS=>EPS
        GWFSWIDAT(Igrid)%NUS=>NUS
        GWFSWIDAT(Igrid)%DELNUS=>DELNUS
        GWFSWIDAT(Igrid)%NUSRF=>NUSRF
        GWFSWIDAT(Igrid)%SWICR=>SWICR
        GWFSWIDAT(Igrid)%SWICC=>SWICC
        GWFSWIDAT(Igrid)%SWICUMCR=>SWICUMCR
        GWFSWIDAT(Igrid)%SWICUMCC=>SWICUMCC
        GWFSWIDAT(Igrid)%NUTOP=>NUTOP
        GWFSWIDAT(Igrid)%NUBOT=>NUBOT
        GWFSWIDAT(Igrid)%QLEXTRA=>QLEXTRA
        GWFSWIDAT(Igrid)%QREXTRA=>QREXTRA
        GWFSWIDAT(Igrid)%QFEXTRA=>QFEXTRA
        GWFSWIDAT(Igrid)%QLEXTRACUM=>QLEXTRACUM
        GWFSWIDAT(Igrid)%QREXTRACUM=>QREXTRACUM
        GWFSWIDAT(Igrid)%QFEXTRACUM=>QFEXTRACUM
        GWFSWIDAT(Igrid)%BRHS=>BRHS
        GWFSWIDAT(Igrid)%DUM=>DUM
        GWFSWIDAT(Igrid)%RHSPRESWI=>RHSPRESWI
        GWFSWIDAT(Igrid)%IPLPOS=>IPLPOS
        GWFSWIDAT(Igrid)%IZONENR=>IZONENR
        GWFSWIDAT(Igrid)%NBDITEMS=>NBDITEMS
        GWFSWIDAT(Igrid)%CUMBD=>CUMBD
        GWFSWIDAT(Igrid)%INCBD=>INCBD
        GWFSWIDAT(Igrid)%RRATIN=>RRATIN
        GWFSWIDAT(Igrid)%RRATOUT=>RRATOUT
        GWFSWIDAT(Igrid)%SC1=>SC1
        GWFSWIDAT(Igrid)%SC2=>SC2
        GWFSWIDAT(Igrid)%SWIOBS=>SWIOBS
        GWFSWIDAT(Igrid)%SWIDE4=>SWIDE4
        GWFSWIDAT(Igrid)%SWIPCG=>SWIPCG
C
C---------RETURN
        RETURN
      END SUBROUTINE SGWF2SWI2PSV

!      SUBROUTINE SSWI2_BDCH(ITOTAL)
C     ******************************************************************
C     COMPUTE SWI2 FLOW CORRECTION FOR CONSTANT-HEAD CELLS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C
C
!      SUBROUTINE SSWI2_UPDZ(Kkstp,Kkper)
C
C     ******************************************************************
C     UPDATE ACTIVE ZETA SURFACES FOR SWI2 PACKAGE
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C
C
!      SUBROUTINE SSWI2_CHKSLOPE()
C
C     ******************************************************************
C     CHECK TIP AND TOE SLOPE FOR SWI2 PACKAGE
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
!      SUBROUTINE SSWI2_ZCHG(A)
C
C     ******************************************************************
C     CALCULATE SWI2 ZONE CHANGE TERM FOR SWI2 PACKAGE ZONE BUDGETS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
!      SUBROUTINE SSWI2_IMIX(A)
C
C     ******************************************************************
C     CALCULATE SWI2 PACKAGE INSTANTANEOUS MIXING TERMS FOR ZONE BUDGETS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C
!      SUBROUTINE SSWI2_MSTO(J,I,K,V,Vc)
C
C     ******************************************************************
C     COMPUTE MODFLOW STORAGE FLOW TERM FROM SC1 AND SC2 TO ADJUST
C     SWI2 PACKAGE BOUNDARY TERM FOR ZONE BUDGETS.
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
!      SUBROUTINE SSWI2_UPZ1(Klay,Init)
C
C     ******************************************************************
C     UPDATE THE UPPER ZETA SURFACE TO THE TOP OF THE WATER TABLE 
C     FOR UNCONFINED CONDITIONS      
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C
!       SUBROUTINE SSWI2_SET_IPLPOS()
C
C     ******************************************************************
C     SET FLAG FOR LOCATION OF THE ZETA SURFACE RELATIVE TO THE TOP AND
C     BOTTOM OF A CELL
C       IPLPOS=1 AT TOP
C       IPLPOS=2 AT BOTTOM
C       IPLPOS=0 IN BETWEEN, AND
C       IPLPOS=3 IN INACTIVE CELLS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C
      CHARACTER (LEN=17) FUNCTION SSWI2_BDCHAR(R) RESULT(value)
C
C     ******************************************************************
C     CREATE SWI2 ZONE BUDGET TERMS CHARACTER STRINGS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        REAL, INTENT(IN) :: R
C     + + + LOCAL DEFINITIONS + + +
        REAL :: t
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        t = ABS(R)
        IF(t.NE.0.0 .AND.
     1    (t.GE.9.99999E11 .OR. t.LT.0.1) ) THEN
          WRITE(value,'(1PE17.4)') R
        ELSE
          WRITE(value,'(F17.4)') R
        END IF
C---------RETURN
        RETURN
      END FUNCTION SSWI2_BDCHAR
C
C
      SUBROUTINE SSWI2_RD_COMM(Iu)
C
C     ******************************************************************
C     READ NON COMMENT AND NON BLANK LINE FROM SWI2 INPUT FILE
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Iu
C       + + + LOCAL DEFINITIONS + + +
        CHARACTER (LEN=2), PARAMETER :: comment = '//'
        CHARACTER (LEN=200) :: line
        LOGICAL :: iscomment
        INTEGER :: ios
        line = comment
        DO
          READ (Iu,'(A)',IOSTAT=ios) line
          IF (ios /= 0) CALL USTOP('COULD NOT READ FROM UNIT Iu')
          IF (LEN_TRIM(line).LT.1) THEN
            line = comment
            CYCLE
          END IF
          line = TRIM(ADJUSTL(line))
          iscomment = .FALSE.
          SELECT CASE (line(1:1))
            CASE ('#')
              iscomment = .TRUE.
            CASE ('!')
              iscomment = .TRUE.
            CASE DEFAULT
              IF (line(1:2).EQ.comment) iscomment = .TRUE.
          END SELECT
          IF (.NOT.iscomment) THEN
            BACKSPACE(Iu)
            RETURN
          END IF
        END DO
        RETURN
      END SUBROUTINE SSWI2_RD_COMM
