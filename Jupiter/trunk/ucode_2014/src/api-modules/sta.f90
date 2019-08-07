! This file is part of the JUPITER API, documented in:
!
! Banta, E.R., Poeter, E.P., Doherty, J.E., and Hill, M.C., 2006, 
! JUPITER: Joint Universal Parameter IdenTification and Evaluation 
! of Reliability--An application programming interface (API) for 
! model analysis: U.S. Geological Survey Techniques and Methods, 
! book 6, chap. E1, 268 p.  
! (Also available at http://pubs.usgs.gov/tm/2006/tm6e1/.)
!
! For the latest updates to source code and documentation, go to:
! http://water.usgs.gov/software/JupiterApi/
!
MODULE STATISTICS
  PRIVATE
  !
  !   PUBLIC DATA
  PUBLIC :: WTDOBS
  !
  !   PUBLIC SUBPROGRAMS
  !
  PUBLIC STA_INI, STA_UEV_FIT, STA_UEV_INIT,  &
         STA_EVA_COMMENTS, STA_EVA_CRITVAL_R2N,  &
         STA_UEV_DX_READ_DM,  &
         STA_UEV_DX_WRITE_DM,  &
         STA_UEV_DX_WRITE_NM, STA_EVA_ORDER, &
         STA_EVA_PROB_NORM_DISTRB, STA_EVA_RUNS, STA_CLN
  !
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: WTDOBS  ! Weighted observations
  !
CONTAINS
  !-----------------------------------------------------------------------------
  SUBROUTINE STA_INI(NOBS,OBSVAL,WTMATSQR)
    !   Allocate space for weighted-observations array, and populate it
    USE DATATYPES
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                           INTENT(IN) :: NOBS
    DOUBLE PRECISION, DIMENSION(NOBS), INTENT(IN) :: OBSVAL   ! Observed values
    TYPE (CDMATRIX),                   INTENT(IN) :: WTMATSQR ! Square-root of weight matrix
    !
    !   Local variables
    !
    !   Allocate and calculate weighted observations
    ALLOCATE(WTDOBS(NOBS))
    !WTDOBS = UTL_MATMULVEC(NOBS,WTMATSQR,OBSVAL)
    CALL UTL_MATMULVEC_SUB(NOBS,WTMATSQR,OBSVAL,WTDOBS)
    RETURN
  END SUBROUTINE STA_INI
  !-----------------------------------------------------------------------------
  SUBROUTINE STA_UEV_DX_READ_DM(IFAIL,OUTNAM, &
                                AIC,BIC,DTLA,HQ, &
                                ICONVERGE,ITERP,KASHYAP,MLOFD,MLOFDP, &
                                MODELLENGTH,MODELMASS,MODELNAME,MODELTIME, &
                                MPR,NDINC,NPE,NPERD,NPS,NOBS,   &
                                STAT1,STAT2,STDERR,VAR)
    !   WRITE _DM DEX FILE Model Fit and Parsimony Data
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                       INTENT(INOUT) :: IFAIL
    CHARACTER(LEN=MAX_STRING_LEN), INTENT(IN)    :: OUTNAM
    DOUBLE PRECISION,              INTENT(OUT)   :: AIC
    DOUBLE PRECISION,              INTENT(OUT)   :: BIC
    DOUBLE PRECISION,              INTENT(OUT)   :: DTLA
    DOUBLE PRECISION,              INTENT(OUT)   :: HQ
    CHARACTER(LEN=3),              INTENT(OUT)   :: ICONVERGE
    INTEGER,                       INTENT(OUT)   :: ITERP
    DOUBLE PRECISION,              INTENT(OUT)   :: KASHYAP
    DOUBLE PRECISION,              INTENT(OUT)   :: MLOFD
    DOUBLE PRECISION,              INTENT(OUT)   :: MLOFDP
    CHARACTER(LEN=12),             INTENT(OUT)   :: MODELLENGTH
    CHARACTER(LEN=12),             INTENT(OUT)   :: MODELMASS
    CHARACTER(LEN=12),             INTENT(OUT)   :: MODELNAME
    CHARACTER(LEN=12),             INTENT(OUT)   :: MODELTIME
    INTEGER,                       INTENT(OUT)   :: MPR
    INTEGER,                       INTENT(OUT)   :: NDINC
    INTEGER,                       INTENT(OUT)   :: NPE
    INTEGER,                       INTENT(OUT)   :: NPERD
    INTEGER,                       INTENT(OUT)   :: NPS
    INTEGER,                       INTENT(OUT)   :: NOBS
    DOUBLE PRECISION,              INTENT(OUT)   :: STAT1
    DOUBLE PRECISION,              INTENT(OUT)   :: STAT2
    DOUBLE PRECISION,              INTENT(OUT)   :: STDERR
    DOUBLE PRECISION,              INTENT(OUT)   :: VAR
    !
    !   Local variables
    CHARACTER(LEN=100) CHECK
    INTEGER IUDM
    !
    !   Format statement
    200 FORMAT(1X,'Error opening file ',A,'._dm')
    !
    IFAIL = 0
    AIC = 0.D0
    BIC = 0.D0
    VAR = 0.D0
    ICONVERGE = 'NUL'
    DTLA = 0.D0
    HQ = 0.D0
    ITERP = 0
    IUDM = 0
    KASHYAP = 0.D0
    MLOFD = 0.D0
    MLOFDP = 0.D0
    MODELLENGTH = 'NA'
    MODELMASS = 'NA'
    MODELNAME = 'NA'
    MODELTIME = 'NA'
    MPR = 0
    NDINC = 0
    NPE = 0
    NPERD = 0
    NPS = 0
    NOBS = 0
    STAT1 = 0.D0
    STAT2 = 0.D0
    STDERR = 0.D0
    !   Open existing _dm file
    IUDM = UTL_DX_OPEN(OUTNAM,'_dm','OLD')
    IF (IUDM < 0) THEN
      WRITE(*,200)TRIM(OUTNAM)
      CALL UTL_STOP()
    ENDIF
    !   Read data
    READ(IUDM,*,END=100)CHECK,MODELNAME
    IF(CHECK .NE. 'MODEL NAME: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,MODELLENGTH
    IF(CHECK .NE. 'MODEL LENGTH UNITS: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,MODELMASS
    IF(CHECK .NE. 'MODEL MASS UNITS: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,MODELTIME
    IF(CHECK .NE. 'MODEL TIME UNITS: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,NPERD
    IF(CHECK .NE. 'NUMBER ESTIMATED PARAMETERS: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,NPE
    IF(CHECK .NE. 'ORIGINAL NUMBER ESTIMATED PARAMETERS: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,NPS
    IF(CHECK .NE. 'TOTAL NUMBER PARAMETERS: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,NDINC
    IF(CHECK .NE. 'NUMBER OBSERVATIONS INCLUDED:') GO TO 100
    READ(IUDM,*,END=100)CHECK,NOBS
    IF(CHECK .NE. 'NUMBER OBSERVATIONS PROVIDED: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,MPR
    IF(CHECK .NE. 'NUMBER PRIOR:') GO TO 100
    READ(IUDM,*,END=100)CHECK,ICONVERGE
    IF(CHECK .NE. 'REGRESSION CONVERGED: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,VAR
    IF(CHECK .NE. 'CALCULATED ERROR VARIANCE: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,STDERR
    IF(CHECK .NE. 'STANDARD ERROR OF THE REGRESSION: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,MLOFD
    IF(CHECK .NE. &
    'MAXIMUM LIKELIHOOD OBJECTIVE FUNCTION - DEPENDENTS (MLOFD): ') GO TO 100
    READ(IUDM,*,END=100)CHECK,MLOFDP
    IF(CHECK .NE. &
    'MAXIMUM LIKELIHOOD OBJECTIVE FUNCTION - DEPENDENTS AND PRIOR (MLOFDP): ') &
    GO TO 100
    READ(IUDM,*,END=100)CHECK,AIC
    IF(CHECK .NE. 'AICc (MLOFD + AICc PENALTY): ') GO TO 100
    READ(IUDM,*,END=100)CHECK,BIC
    IF(CHECK .NE. 'BIC (MLOFD + BIC PENALTY): ') GO TO 100
    READ(IUDM,*,END=100)CHECK,HQ
    IF(CHECK .NE. 'HQ (MLOFD + HQ PENALTY): ') GO TO 100
    READ(IUDM,*,END=100)CHECK,KASHYAP
    IF(CHECK .NE. 'KASHYAP (MLOFD + KASHYAP PENALTY): ') GO TO 100
    READ(IUDM,*,END=100)CHECK,DTLA
    IF(CHECK .NE. 'LN DETERMINANT OF FISHER INFORMATION MATRIX: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,STAT1
    IF(CHECK .NE. 'RN2 DEPENDENTS: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,STAT2
    IF(CHECK .NE. 'RN2 DEPENDENTS AND PRIOR: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,ITERP
    IF(CHECK .NE. 'NUMBER OF ITERATIONS: ') GO TO 100
    IUDM = UTL_DX_CLOSE('_dm')
    RETURN
    100 IUDM = UTL_DX_CLOSE('_dm')
    CALL UTL_STOP('UNEXPECTED CONTENT _dm file, CHECK VERSION or RE-CREATE _dm')
    RETURN
  END SUBROUTINE STA_UEV_DX_READ_DM
  !-----------------------------------------------------------------------------
  SUBROUTINE STA_UEV_DX_WRITE_DM(AIC,BIC,DTLA,HQ,IFO,ITERP,KASHYAP,   &
                                 MLOFD,MLOFDP,MODELLENGTH,MODELMASS,MODELNAME,   &
                                 MODELTIME,MPR,NDINC,NPE,NPERD,NPS,NOBS,   &
                                 OUTNAM,STAT1,STAT2,VAR)
    !   WRITE _DM DEX FILE Model Fit and Parsimony Data
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    DOUBLE PRECISION,   INTENT(IN) :: AIC
    DOUBLE PRECISION,   INTENT(IN) :: BIC
    DOUBLE PRECISION,   INTENT(IN) :: DTLA
    DOUBLE PRECISION,   INTENT(IN) :: HQ
    INTEGER,            INTENT(IN) :: IFO
    INTEGER,            INTENT(IN) :: ITERP
    DOUBLE PRECISION,   INTENT(IN) :: KASHYAP
    DOUBLE PRECISION,   INTENT(IN) :: MLOFD
    DOUBLE PRECISION,   INTENT(IN) :: MLOFDP
    CHARACTER(LEN=12),  INTENT(IN) :: MODELLENGTH
    CHARACTER(LEN=12),  INTENT(IN) :: MODELMASS
    CHARACTER(LEN=12),  INTENT(IN) :: MODELNAME
    CHARACTER(LEN=12),  INTENT(IN) :: MODELTIME
    INTEGER,            INTENT(IN) :: MPR
    INTEGER,            INTENT(IN) :: NDINC
    INTEGER,            INTENT(IN) :: NPE
    INTEGER,            INTENT(IN) :: NPERD
    INTEGER,            INTENT(IN) :: NPS
    INTEGER,            INTENT(IN) :: NOBS
    CHARACTER(LEN=MAX_STRING_LEN), INTENT(IN) :: OUTNAM
    DOUBLE PRECISION,   INTENT(IN) :: STAT1
    DOUBLE PRECISION,   INTENT(IN) :: STAT2
    DOUBLE PRECISION,   INTENT(IN) :: VAR
    !
    !   Local variables
    INTEGER IUDM
    CHARACTER(LEN=3) :: CONVERGE = ' NO'
    !
    !   Format statements
    656 FORMAT(1X, '"MODEL NAME: "',' "',A,'"')
    657 FORMAT(1X, '"MODEL LENGTH UNITS: " "',A,'"')
    658 FORMAT(1X, '"MODEL MASS UNITS: " "',A,'"')
    659 FORMAT(1X, '"MODEL TIME UNITS: " "',A,'"')
    660 FORMAT(1X, '"NUMBER ESTIMATED PARAMETERS: "',I5)
    661 FORMAT(1X, '"ORIGINAL NUMBER ESTIMATED PARAMETERS: "',I5)
    662 FORMAT(1X, '"TOTAL NUMBER PARAMETERS: "',I5)
    670 FORMAT(1X, '"NUMBER OBSERVATIONS INCLUDED: "',I7)
    671 FORMAT(1X, '"NUMBER OBSERVATIONS PROVIDED: "',I7)
    672 FORMAT(1X, '"NUMBER PRIOR: "',I5)
    673 FORMAT(1X, '"CALCULATED ERROR VARIANCE: "',G19.12)
    674 FORMAT(1X, '"STANDARD ERROR OF THE REGRESSION: "',G19.12)
    675 FORMAT(1X, '"MAXIMUM LIKELIHOOD OBJECTIVE FUNCTION ', &
                    '- DEPENDENTS (MLOFD): "',G14.7)
    676 FORMAT(1X, '"MAXIMUM LIKELIHOOD OBJECTIVE FUNCTION ', &
                    '- DEPENDENTS AND PRIOR (MLOFDP): "',G14.7)
    677 FORMAT(1X, '"AICc (MLOFD + AICc PENALTY): "',G14.7)
    678 FORMAT(1X, '"BIC (MLOFD + BIC PENALTY): "',G14.7)
    679 FORMAT(1X, '"HQ (MLOFD + HQ PENALTY): "',G14.7)
    680 FORMAT(1X, '"KASHYAP (MLOFD + KASHYAP PENALTY): "',G14.7)
    681 FORMAT(1X, '"LN DETERMINANT OF FISHER INFORMATION MATRIX: " ',G14.7)
    682 FORMAT(1X, '"RN2 DEPENDENTS: "',G14.7)
    683 FORMAT(1X, '"RN2 DEPENDENTS AND PRIOR: "',G14.7)
    687 FORMAT(1X, '"NUMBER OF ITERATIONS: "',I7)
    700 FORMAT(1X, '"REGRESSION CONVERGED: "',1X,'"',A3,'"')
    ! Write  data
    IUDM = UTL_DX_OPEN(OUTNAM,'_dm','REPLACE')
    IF(IFO .LT. 3) CONVERGE = 'YES'
    WRITE(IUDM,656) TRIM(MODELNAME)
    WRITE(IUDM,657) TRIM(MODELLENGTH)
    WRITE(IUDM,658) TRIM(MODELMASS)
    WRITE(IUDM,659) TRIM(MODELTIME)
    WRITE(IUDM,660) NPERD
    WRITE(IUDM,661) NPE
    WRITE(IUDM,662) NPS
    WRITE(IUDM,670) NDINC
    WRITE(IUDM,671) NOBS
    WRITE(IUDM,672) MPR
    WRITE(IUDM,700) CONVERGE
    WRITE(IUDM,673) VAR
    WRITE(IUDM,674) VAR**.5
    WRITE(IUDM,675) MLOFD
    WRITE(IUDM,676) MLOFDP
    WRITE(IUDM,677) AIC
    WRITE(IUDM,678) BIC
    WRITE(IUDM,679) HQ
    WRITE(IUDM,680) KASHYAP
    WRITE(IUDM,681) DTLA
    WRITE(IUDM,682) STAT1
    IF(STAT2 .GT. 0) THEN
      WRITE(IUDM,683) STAT2
    ELSE
      WRITE(IUDM,683) STAT1
    ENDIF
    WRITE(IUDM,687) ITERP
    IUDM = UTL_DX_CLOSE('_dm')
    RETURN
  END SUBROUTINE STA_UEV_DX_WRITE_DM
  !-----------------------------------------------------------------------------
  SUBROUTINE STA_UEV_DX_WRITE_NM(MPR,NOBS,OBSNAM,OUTNAM,PLOTSYMBOLPRI,PRINAM, &
                           WTDRESIDS,WTDRESIDSPRI)
    !   WRITE _NM DATA EXCHANGE FILES: ORDERED Residuals
    USE GLOBAL_DATA, ONLY: LENDNAM, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: MPR
    INTEGER,                                 INTENT(IN) :: NOBS
    CHARACTER(LEN=LENDNAM), DIMENSION(NOBS), INTENT(IN) :: OBSNAM
    CHARACTER(LEN=MAX_STRING_LEN),           INTENT(IN) :: OUTNAM
    INTEGER,                DIMENSION(MPR),  INTENT(IN) :: PLOTSYMBOLPRI
    CHARACTER(LEN=LENDNAM), DIMENSION(MPR),  INTENT(IN) :: PRINAM
    DOUBLE PRECISION,       DIMENSION(NOBS), INTENT(IN) :: WTDRESIDS
    DOUBLE PRECISION,       DIMENSION(MPR),  INTENT(IN) :: WTDRESIDSPRI
    !
    !   Local variables
    CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: ORDDID
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: ORDERD
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: ORDERR
    INTEGER :: IUNM, N
    !
    !   Format statements
      500 FORMAT(6X,G15.6,1X,G15.6,16X,I5,12X,A)
      501 FORMAT(1X,'"WEIGHTED RESIDUAL" "STANDARD NORMAL STATISTIC" ', &
          ' "PLOT SYMBOL" "OBSERVATION or PRIOR NAME"')
    ! ALLOCATE
    ALLOCATE(ORDDID(NOBS+MPR),ORDERD(NOBS+MPR,2),ORDERR(NOBS+MPR))
    !Initialize
    ORDERD = 0.D0
    ORDERR = 0.D0
    ! Open data exchange file
    IUNM = UTL_DX_OPEN(OUTNAM,'_nm','REPLACE')
    ! Write  data
    CALL STA_EVA_ORDER(MPR,NOBS,OBSNAM,PLOTSYMBOLPRI,PRINAM,WTDRESIDS, &
                       WTDRESIDSPRI,ORDERD,ORDERR,ORDDID)
    !-------write data to _nm file
    WRITE (IUNM,501)
    DO N=1,NOBS+MPR
      WRITE(IUNM,500) ORDERD(N,1),ORDERR(N),INT(ORDERD(N,2)),ORDDID(N) ! iprint 4
    ENDDO
    ! Close data exchange file
    IUNM = UTL_DX_CLOSE('_nm')
    ! DEALLOCATE
    DEALLOCATE(ORDDID,ORDERD,ORDERR)
    RETURN
  END SUBROUTINE STA_UEV_DX_WRITE_NM
  !-----------------------------------------------------------------------------
  SUBROUTINE STA_UEV_FIT(IOUT,MPR,NOBS,IPRINT,DONOTEVALRUNS, &
                         MODELVAL,MODELPRIVAL,OBSNAM, &
                         OBSVAL,OMIT,OUTNAM,PLOTSYMBOLPRI,PRILN,PRINAM, &
                         PRIVAL,PRIWTCORR,PRIWTMATSQR,RESIDS,RESIDSPRI, &
                         WTCORRELATED,WTDRESIDS,WTDRESIDSPRI,WTMATSQR,   &
                         AVET,NNEGT,NPOST,NRUNS,RSQ,RSQP,DNPP,NDINC,WTRL)
    !     ******************************************************************
    !     PRINT WEIGHTED RESIDUALS FOR DEPENDENT VARIABLES AND PRIOR
    !     ******************************************************************
    !        SPECIFICATIONS:
    !     ------------------------------------------------------------------
    USE GLOBAL_DATA, ONLY: LENDNAM, MAX_STRING_LEN
    USE DATATYPES
    USE UTILITIES
    USE DEPENDENTS, ONLY: &
                    DEP_GET_GROUP, NONDETVAL, NTOTOBS, NTOTPRED, NUSEOBS
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN)  :: IOUT
    INTEGER,                                 INTENT(IN)  :: MPR        ! Number of prior
    INTEGER,                                 INTENT(IN)  :: NOBS        ! Number of dependent variables
    LOGICAL, DIMENSION(3),                   INTENT(IN)  :: IPRINT      ! Control of output:
    !   IPRINT(1): List of all observations and residuals
    !   IPRINT(2): Sum of squared residuals
    !   IPRINT(3): Summary statistics
    LOGICAL,                                 INTENT(IN)    :: DONOTEVALRUNS
    DOUBLE PRECISION, DIMENSION(NOBS),       INTENT(IN)    :: MODELVAL  ! Simulated-equivalent values
    DOUBLE PRECISION, DIMENSION(MPR),        INTENT(IN)    :: MODELPRIVAL  ! Simulated-equivalent values
    CHARACTER(LEN=LENDNAM), DIMENSION(NOBS), INTENT(IN)    :: OBSNAM
    DOUBLE PRECISION, DIMENSION(NOBS),       INTENT(IN)    :: OBSVAL    ! Observed values
    INTEGER,          DIMENSION(NOBS),       INTENT(IN)    :: OMIT
    CHARACTER(LEN=MAX_STRING_LEN),           INTENT(IN)    :: OUTNAM
    INTEGER,          DIMENSION(MPR),        INTENT(IN)    :: PLOTSYMBOLPRI
    INTEGER,          DIMENSION(MPR),        INTENT(IN)    :: PRILN
    CHARACTER(LEN=LENDNAM), DIMENSION(MPR),  INTENT(IN)    :: PRINAM
    DOUBLE PRECISION, DIMENSION(MPR),        INTENT(IN)    :: PRIVAL
    LOGICAL,          DIMENSION(MPR),        INTENT(IN)    :: PRIWTCORR
    TYPE (CDMATRIX),                         INTENT(IN)    :: PRIWTMATSQR  ! Square-root of weight matrix
    DOUBLE PRECISION, DIMENSION(NOBS),       INTENT(IN)    :: RESIDS    ! Residuals
    DOUBLE PRECISION, DIMENSION(MPR),        INTENT(IN)    :: RESIDSPRI
    LOGICAL,          DIMENSION(NOBS),       INTENT(IN)    :: WTCORRELATED
    DOUBLE PRECISION, DIMENSION(NOBS),       INTENT(IN)    :: WTDRESIDS ! Weighted residuals
    DOUBLE PRECISION, DIMENSION(MPR),        INTENT(IN)    :: WTDRESIDSPRI
    TYPE (CDMATRIX),                         INTENT(IN)    :: WTMATSQR  ! Square-root of weight matrix
    DOUBLE PRECISION,                        INTENT(INOUT) :: AVET
    INTEGER,                                 INTENT(INOUT) :: NNEGT
    INTEGER,                                 INTENT(INOUT) :: NPOST
    INTEGER,                                 INTENT(INOUT) :: NRUNS
    DOUBLE PRECISION,                        INTENT(INOUT) :: RSQ
    DOUBLE PRECISION,                        INTENT(INOUT) :: RSQP
    DOUBLE PRECISION, DIMENSION(NUSEOBS+MPR,2), INTENT(OUT)   :: DNPP
    INTEGER,                                 INTENT(OUT)   :: NDINC
    DOUBLE PRECISION,                        INTENT(OUT)   :: WTRL
    !
    !   Local variables
    INTEGER :: IPLOT, N, NMAX, NMIN, NNEG, NOMIT, NPOS
    INTEGER :: NMNOBS, NOM, NN
    LOGICAL :: NIN
    DOUBLE PRECISION :: AVE, RES, VMAX, VMIN, WT2, WTR
    CHARACTER(LEN=12) :: GPNAM
    CHARACTER(LEN=LENDNAM) :: MXNAME, MNNAME
    !
    !   Format statements
    635 FORMAT(/,1X,67('-'),/,' FIT OF SIMULATED EQUIVALENTS TO OBSERVATIONS', &
       /,'  A * INDICATES THE OBSERVATION IS ASSOCIATED WITH A NONDETECT VALUE')
    636 FORMAT(/,1X,67('-'),/,' FIT OF SIMULATED EQUIVALENTS TO OBSERVATIONS')
    637 FORMAT(/,   &
        1X,'OBSERVATION',12X,   &
        'MEASURED',6X,'SIMULATED',28X,'WEIGHTED',/,   &
        4X,'NAME',17X,'VALUE',10X,'VALUE',   &
        6X,'RESIDUAL',3X,'WEIGHT**.5',3X,'RESIDUAL',/)
    639 FORMAT(1X,A,'*',1X,1PG13.6,1X,1PG13.6,1X,1PG11.4,3X,1PG9.3,1X,1PG11.4)
    640 FORMAT(1X,A,2X,1PG13.6,1X,1PG13.6,1X,1PG11.4,3X,1PG9.3,1X,1PG11.4)
    641 FORMAT(1X,A,2X,1PG13.6,1X,1PG13.6,1X,1PG11.4,3X,'matrix',4X,1PG11.4)
    642 FORMAT(1X,A,'*',1X,1PG13.6,1X,1PG13.6,1X,1PG11.4,3X,'matrix',4X,1PG11.4)
    645 FORMAT(1X,A,2X,1PG13.6,1X,1PG22.14,'     OMITTED')
    665 FORMAT(/,' SUM OF SQUARED WEIGHTED RESIDUALS           :  ',G12.5)
    666 FORMAT(/,' SUM OF SQUARED WEIGHTED RESIDUALS WITH PRIOR:  ',G12.5)
    680 FORMAT(/,' STATISTICS FOR THESE RESIDUALS:',/,   &
        ' MAXIMUM WEIGHTED RESIDUAL: ',E10.3,'  Observation:',1X,A,/,   &
        ' MINIMUM WEIGHTED RESIDUAL: ',E10.3,'  Observation:',1X,A,/,  &
        ' AVERAGE WEIGHTED RESIDUAL: ',E10.3,/,   &
        ' # RESIDUALS >= 0.  : ',I7,/,' # RESIDUALS <  0.  : ',I7,/,   &
        ' NUMBER OF RUNS: ',I8,'  IN ',I8,' OBSERVATIONS')
    681 FORMAT(/,' STATISTICS FOR THESE RESIDUALS:',/,   &
        ' MAXIMUM WEIGHTED RESIDUAL: ',E10.3,'  Observation:',1X,A,/,   &
        ' MINIMUM WEIGHTED RESIDUAL: ',E10.3,'  Observation:',1X,A,/,  &
        ' AVERAGE WEIGHTED RESIDUAL: ',E10.3,//,' POSITIVE AND NEGATIVE ', &
        ' RESIDUALS AND THE RUNS STATISTIC ',/, &
        ' ARE NOT COMPUTED DUE TO ONE OR MORE USER SELECTED OPTION',/)
    !
    DNPP = 0.D0
    NOMIT = 0
    NOM = NTOTOBS + MPR
    NN = 0
    NMIN = 0 ! ADDED ERB 3/23/09
    !
    !---OBSERVATIONS
    IF(NOBS .GT. 0) THEN
      !     ADD PRINT OF RESIDUALS AT STARTING VALUES EVEN IF INTERMEDIATE
      !     PRINT IS TURNED OFF
      IF (IPRINT(1)) THEN
        IF(DONOTEVALRUNS) THEN
          WRITE(IOUT,635) ! iprint 1
        ELSE
          WRITE(IOUT,636) ! iprint 1
        ENDIF
        WRITE(IOUT,637) ! iprint 1
      ENDIF
      NNEG=0
      NPOS=0
      VMAX=-1.0D30
      VMIN=1.0D30
      AVE=0.0D0
      EACHOBS: DO N=1,NTOTOBS+MPR
        IF(N>NOBS .AND. N<=NTOTOBS) CYCLE
        NIN = .TRUE.
        NN = NN + 1
        CALL DEP_GET_GROUP(NN,GPNAM,IPLOT,'USED')
        IF(N <= NOBS) THEN
          !### check for h(n).eq.romit after calculation with new parameters,
          !### before entering regression if(h(n).eq.romit) wt(n)=-wt(n)
          !### before this, initialize all weights to all be positive
          IF(OMIT(N) .LT. 0) THEN
            IF (IPRINT(1)) WRITE(IOUT,645) OBSNAM(N),OBSVAL(N),MODELVAL(N) ! iprint 1
            NOMIT=NOMIT+1
            CYCLE
          ENDIF
          RES=RESIDS(N)
          WTR=WTDRESIDS(N)
        ELSE !N>NOBS so we are dealing with prior
          NMNOBS=N-NTOTOBS
          RES=RESIDSPRI(NMNOBS)
          WTR=WTDRESIDSPRI(NMNOBS)
        ENDIF
        !---print simulated and observed values and related items to the main
        !---output file
        IF (NIN .AND. IPRINT(1)) THEN
          !Print only observations and prior, not predictions if present
          IF(N .LE. NTOTOBS) THEN
            IF (.NOT. WTCORRELATED(N)) THEN
              WT2 = UTL_GETVAL(WTMATSQR,N,N)
              IF(NONDETVAL(N) .EQ. 0.D0) THEN
                WRITE(IOUT,640) OBSNAM(N),OBSVAL(N),MODELVAL(N),RES,WT2,WTR ! iprint 1
              ELSE
                WRITE(IOUT,639) OBSNAM(N),OBSVAL(N),MODELVAL(N),RES,WT2,WTR ! iprint 1
              ENDIF
            ELSE
              IF(NONDETVAL(N) .EQ. 0.D0) THEN
                WRITE(IOUT,641) OBSNAM(N),OBSVAL(N),MODELVAL(N),RES,WTR ! iprint 1
              ELSE
                WRITE(IOUT,642) OBSNAM(N),OBSVAL(N),MODELVAL(N),RES,WTR ! iprint 1
              ENDIF
            ENDIF
          ELSEIF(N > NOBS) THEN
            IF (.NOT. PRIWTCORR(NMNOBS)) THEN
              WT2 = UTL_GETVAL(PRIWTMATSQR,NMNOBS,NMNOBS)
              IF(PRILN(NMNOBS) .EQ. 0) THEN
                WRITE(IOUT,640) PRINAM(NMNOBS),PRIVAL(NMNOBS), &
                                MODELPRIVAL(NMNOBS),RES,WT2,WTR ! iprint 1
              ELSE
                WRITE(IOUT,640) PRINAM(NMNOBS),EXP(PRIVAL(NMNOBS)), &
                                EXP(MODELPRIVAL(NMNOBS)),RES,WT2,WTR ! iprint 1
              ENDIF
            ELSE
              IF(PRILN(NMNOBS) .EQ. 0) THEN
                WRITE(IOUT,641) PRINAM(NMNOBS),PRIVAL(NMNOBS), &
                                MODELPRIVAL(NMNOBS),RES,WTR ! iprint 1
              ELSE
                WRITE(IOUT,641) PRINAM(NMNOBS),EXP(PRIVAL(NMNOBS)), &
                                EXP(MODELPRIVAL(NMNOBS)),RES,WTR ! iprint 1
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        !-------prepare normal probability plot data
        DNPP(NN-NOMIT,1)=WTR
        DNPP(NN-NOMIT,2)=DBLE(IPLOT)
        !-------weighted sum of squares objective function
        IF(N .LE. NTOTOBS)THEN
          RSQ=RSQ+(WTR**2)
          RSQP=RSQ
        ELSEIF(N > NOBS) THEN
          ! skip predictions if they are present and add prior
          RSQP=RSQP+(WTR**2)
        ENDIF
        !-------find maximum and minimum residuals
        IF(NIN .AND. WTR .GT. VMAX) THEN
          VMAX=WTR
          NMAX=N
        ENDIF
        IF(NIN .AND. WTR .LT. VMIN) THEN
          VMIN=WTR
          NMIN=N
        ENDIF
        !-------count negative and positive residuals
        IF(NIN .AND. WTR .GE. 0.) NPOS=NPOS+1
        IF(NIN .AND. WTR .LT. 0.) NNEG=NNEG+1
        IF(NIN .AND. N .GT. 1) THEN
          IF(WTRL*WTR .LT. 0.) NRUNS=NRUNS+1
        ENDIF
        IF(NIN) THEN
          WTRL=WTR
          AVE=AVE+WTR
        ENDIF
      ENDDO EACHOBS
      !----final printing for observations
      NDINC=NN-MPR-NOMIT
      IF(NOMIT<NN) THEN
        AVET=AVET+AVE
        NPOST=NPOST+NPOS
        NNEGT=NNEGT+NNEG
        AVE=AVE/REAL((NDINC),KIND(0.0D0))
        ! iprint 3
        IF (IPRINT(3)) THEN
          IF (NMAX <= NOBS) THEN
            MXNAME = OBSNAM(NMAX)
          ELSE
            MXNAME = PRINAM(NMAX-NOBS)
          ENDIF
          IF (NMIN>0) THEN ! ADDED ERB 3/23/09
            IF (NMIN <= NOBS) THEN
              MNNAME = OBSNAM(NMIN)
            ELSE
              MNNAME = PRINAM(NMIN-NOBS)
            ENDIF
          ENDIF ! ADDED ERB 3/23/09
          IF(DONOTEVALRUNS) THEN
            WRITE(IOUT,681) VMAX,MXNAME,VMIN,MNNAME,AVE
          ELSE
            CALL STA_EVA_RUNS(AVET,NDINC,NPOS,NNEG,NRUNS,IOUT)
            WRITE(IOUT,680) VMAX,MXNAME,VMIN,MNNAME, &
                          AVE,NPOS,NNEG,NRUNS,NOBS-NOMIT+MPR
          ENDIF
        ENDIF
        IF (IPRINT(2)) THEN
          WRITE(IOUT,665) RSQ ! iprint 2
          WRITE(IOUT,666) RSQP ! iprint 2
        ENDIF
      ENDIF
    ENDIF
    !
    RETURN
  END SUBROUTINE STA_UEV_FIT
  !-----------------------------------------------------------------------------
  SUBROUTINE STA_UEV_INIT(AVET,NNEGT,NPOST,NRUNS,RSQ,RSQP)
    !   Initialize variables for UEV routines of the Statistics module
    IMPLICIT NONE
    !
    !   Argument-list variables
    DOUBLE PRECISION, INTENT(OUT) :: AVET
    INTEGER,          INTENT(OUT) :: NNEGT
    INTEGER,          INTENT(OUT) :: NPOST
    INTEGER,          INTENT(OUT) :: NRUNS
    DOUBLE PRECISION, INTENT(OUT) :: RSQ
    DOUBLE PRECISION, INTENT(OUT) :: RSQP
    !
    !   Local variable
    DOUBLE PRECISION :: DZERO
    !
    DZERO = 0.0D0
    RSQ = DZERO
    RSQP = DZERO
    NNEGT = 0
    NPOST =0
    AVET = DZERO
    NRUNS = 1
    !
    RETURN
  END SUBROUTINE STA_UEV_INIT
  !-----------------------------------------------------------------------------
  SUBROUTINE STA_EVA_COMMENTS(IOUT,NOBS,MPR)
    ! VERSION 20031112 EPP
    !    WRITE COMMENTS ON INTERPRETATION, CUSTOMIZED BASED ON NUMBER OF
    !    RESIDUALS AND CORRELATION BETWEEN ORDERED WEIGHTED RESIDUALS AND
    !    NORMAL ORDER STATISTICS
    !
    !   Argument-list variables
    INTEGER, INTENT(IN) :: IOUT
    INTEGER, INTENT(IN) :: MPR
    INTEGER, INTENT(IN) :: NOBS
    !
    ! Local Variables
    INTEGER :: IFLAG, NDR
    DOUBLE PRECISION :: RN205, RN210
    !
    !    ------------------------------------------------------------------
    NDR = NOBS + MPR
    CALL STA_EVA_CRITVAL_R2N (NDR,RN205,RN210)
    IF (NDR .LT. 35) THEN
      IFLAG = 0
    ELSEIF (NDR .EQ. 35) THEN
      IFLAG = 1
    ELSE
      IFLAG = 2
    ENDIF
    WRITE (IOUT,1001)
    1001 FORMAT (/,80('-'))
         WRITE(IOUT,1002)
    1002 FORMAT(' COMMENTS ON THE INTERPRETATION OF THE CORRELATION ',     &
         'BETWEEN',/,' WEIGHTED RESIDUALS AND NORMAL ORDER STATISTICS:')
    IF (IFLAG .EQ. 1) WRITE(IOUT,996)RN205,RN210
    996 FORMAT(                                                            &
         /,' The critical value for correlation at the 5%'                 &
         ,' significance level is ',f5.3                                   &
         ,//,1X,'IF the reported CORRELATION is GREATER than the 5% critic'&
         ,'al value, ACCEPT',/,1x,'the hypothesis that the weighted'       &
         ,' residuals are INDEPENDENT AND NORMALLY',/,1x,'DISTRIBUTED at'  &
         ,' the 5% significance level.  The probability that this   ',/    &
         ,' conclusion is wrong is less than 5%.',                         &
         //,' IF the reported correlation IS LESS THAN the 5% critical'    &
         ,' value REJECT the',/,' hypothesis that the weighted residuals'  &
         ,' are INDEPENDENT AND NORMALLY',/,' DISTRIBUTED at the 5%'       &
         ,' significance level.',//,' The'                                 &
         ,' analysis can also be done using the 10% significance level.',/ &
         ,' The associated critical value is ',f5.3)
    IF (IFLAG .EQ. 0) WRITE(IOUT,997)
    997 FORMAT(/,1X                                                        &
               ,'Generally, IF the reported CORRELATION is LESS than the'  &
               ,' critical value,',/,' at the '                            &
               ,'selected significance level (usually 5 or 10%), the '     &
               ,'hypothesis',/,' that the '                                &
               ,'weighted residuals are INDEPENDENT AND NORMALLY '         &
               ,'DISTRIBUTED',/,' would be REJECTED.  HOWEVER, '           &
               ,'in this case, conditions are outside of',/,' the range of'&
               ,' published critical values as discussed below.')
    IF (IFLAG .EQ. 2) WRITE(IOUT,995)
    995  FORMAT(/,1X                                                       &
             ,'Generally, IF the reported CORRELATION is GREATER than the' &
             ,' critical value,',/,' at the '                              &
             ,'selected significance level (usually 5 or 10%), the '       &
             ,'hypothesis',/,' that the '                                  &
             ,'weighted residuals are INDEPENDENT AND NORMALLY '           &
             ,'DISTRIBUTED',/,' would be ACCEPTED.  HOWEVER, '             &
             ,'in this case, conditions are outside of',/,' the range of'  &
             ,' published critical values as discussed below.')
    IF (IFLAG .EQ. 0) WRITE (IOUT,998) NDR,RN205,RN210
    998  FORMAT(/,1x,'The sum of the number of observations and prior'     &
          ,' information items is ',i5                                     &
          ,/,1x,'which is less than'                                       &
          ,' 35, the minimum value for which critical values are'          &
          ,/,1x,'published.  Therefore, the critical values'               &
          ,' for the 5 and 10% significance'                               &
          ,/,1x,'levels are less than ',f5.3,' and ',f5.3,', respectively.'&
          ,//,' CORRELATIONS GREATER than these critical values'           &
          ,' indicate that, probably, the '                                &
          ,/,1x,'weighted residuals ARE'                                   &
          ,' INDEPENDENT AND NORMALLY DISTRIBUTED.',//,1x                  &
          ,'Correlations LESS than these critical values MAY BE '          &
          ,'ACCEPTABLE, and',/,' rejection of the hypothesis'              &
          ,' is not necessarily warranted.',//                             &
          ,' The Kolmogorov-Smirnov test can be used'                      &
          ,' to further evaluate the residuals.')
    IF (NDR .GT. 200) WRITE (IOUT,999) NDR,RN205,RN210
    999  FORMAT(/,1x,'The sum of the number of observations and prior'     &
          ,' information items is ',i5                                     &
          ,/,1x,'which is greater than'                                    &
          ,' 200, the maximum value for which critical values are'         &
          ,/,1x,'published.  Therefore, the critical values'               &
          ,' for the 5 and 10% significance'                               &
          ,/,1x,'levels are greater than ',f5.3,' and ',f5.3,', respect'   &
          ,'ively.',//,' CORRELATIONS GREATER THAN these critical values'  &
          ,' suggest that, probably,',/                                    &
          ,' the weighted residuals ARE'                                   &
          ,' INDEPENDENT AND NORMALLY DISTRIBUTED.'                        &
          ,//,1x,'Correlations LESS THAN these critical values clearly'    &
          ,' indicate that we CAN',/,1x,'REJECT the hypothesis.'           &
          ,//,1x,'The Kolmogorov-Smirnov test can be used'                 &
          ,' to further evaluate the residuals.')
    WRITE (IOUT,1003)
    1003 FORMAT (' ----------------------------------------',              &
          '----------------------------------',/)
    RETURN
  END SUBROUTINE STA_EVA_COMMENTS
  !-----------------------------------------------------------------------------
  SUBROUTINE STA_EVA_CRITVAL_R2N(NDR,RN205,RN210)
    !VERSION 20031112 EPP
    !     CRITICAL VALUES Of R2N STATISTIC BELOW WHICH THE HYPOTHESIS THAT
    !     WEIGHTED RESIDUALS ARE INDEPENDENT AND NORMALLY DISTRIBUTED IS
    !     REJECTED. FROM SHAPIRO AND FRANCIA, 1972 AND BROCKWELL AND DAVIS,
    !     1987, P. 304. LISTED IN APPENDIX D OF METHODS AND GUIDELINES
    !
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,          INTENT(IN)  :: NDR
    DOUBLE PRECISION, INTENT(OUT) :: RN205
    DOUBLE PRECISION, INTENT(OUT) :: RN210
    !
    !   Local variables
    DOUBLE PRECISION :: TABLE05
    DOUBLE PRECISION :: TABLE10
    INTEGER          :: I
    INTEGER          :: ITABLE
    !     ------------------------------------------------------------------
    DIMENSION ITABLE(29), TABLE05(29), TABLE10(29)
    DATA (ITABLE(I),I=1,29)/35,50,51,53,55,57,59,61,63,65,67,69,71,73, &
          75,77,79,81,83,85,87,89,91,93,95,97,99,131,200/
    DATA (TABLE05(I),I=1,29)/0.943, 0.953, 0.954, 0.957, 0.958, 0.961, &
          0.962, 0.963, 0.964, 0.965, 0.966, 0.966, 0.967, 0.968,      &
          0.969, 0.969, 0.970, 0.970, 0.971, 0.972, 0.972, 0.972,      &
          0.973, 0.973, 0.974, 0.975, 0.976, 0.980, 0.987/
    DATA (TABLE10(I),I=1,29)/0.952, 0.963, 0.964, 0.964, 0.965, 0.966, &
          0.967, 0.968, 0.970, 0.971, 0.971, 0.972, 0.972, 0.973,      &
          0.973, 0.974, 0.975, 0.975, 0.976, 0.977, 0.977, 0.977,      &
          0.978, 0.979, 0.979, 0.979, 0.980, 0.983, 0.989/
    !    ------------------------------------------------------------------
    !
    IF (NDR .LE. 35) THEN
      RN205 = TABLE05(1)
      RN210 = TABLE10(1)
      RETURN
    ENDIF
    !
    DO I=2,29
      IF(NDR .LE. ITABLE(I)) THEN
        RN205 = TABLE05(I-1)+(TABLE05(I)-TABLE05(I-1))*                &
             (NDR-ITABLE(I-1))/(ITABLE(I)-ITABLE(I-1))
        RN210 = TABLE10(I-1)+(TABLE10(I)-TABLE10(I-1))*                &
             (NDR-ITABLE(I-1))/(ITABLE(I)-ITABLE(I-1))
        RETURN
      ENDIF
    ENDDO
    !
    RN205 = TABLE05(29)
    RN210 = TABLE10(29)
    !
    RETURN
  END SUBROUTINE STA_EVA_CRITVAL_R2N
  !-----------------------------------------------------------------------------
  SUBROUTINE STA_EVA_ORDER(MPR,NOBS,OBSNAM,PLOTSYMBOLPRI,PRINAM,  &
                           WTDRESIDS,WTDRESIDSPRI,D,R,ORDDID)
    !   ORDER THE VALUES IN D
    USE GLOBAL_DATA, ONLY: LENDNAM
    USE UTILITIES
    USE DEPENDENTS, ONLY: DEP_GET_GROUP
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                     INTENT(IN)    :: MPR
    INTEGER,                                     INTENT(IN)    :: NOBS
    CHARACTER(LEN=LENDNAM), DIMENSION(NOBS),     INTENT(IN)    :: OBSNAM
    INTEGER,                DIMENSION(MPR),      INTENT(IN)    :: PLOTSYMBOLPRI
    CHARACTER(LEN=LENDNAM), DIMENSION(MPR),      INTENT(IN)    :: PRINAM
    DOUBLE PRECISION, DIMENSION(NOBS),           INTENT(IN)    :: WTDRESIDS
    DOUBLE PRECISION, DIMENSION(MPR),            INTENT(IN)    :: WTDRESIDSPRI
    DOUBLE PRECISION, DIMENSION(NOBS+MPR,2),     INTENT(INOUT) :: D
    DOUBLE PRECISION, DIMENSION(NOBS+MPR),       INTENT(INOUT) :: R
    CHARACTER(LEN=LENDNAM), DIMENSION(NOBS+MPR), INTENT(INOUT) :: ORDDID
    !
    !   Local variables
    CHARACTER(LEN=LENDNAM) :: DIDMIN
    CHARACTER(LEN=12) :: GPNAM
    INTEGER :: IPLOT, N, N1, N2, NMIN, NPNOBS
    DOUBLE PRECISION :: RMIN, RNORM, SMIN
    !
    !  Initialization added 4/5/04
    DO N=1,NOBS
      D(N,1) = WTDRESIDS(N)
      CALL DEP_GET_GROUP(N,GPNAM,IPLOT,'USED')
      D(N,2) = DBLE(IPLOT)
      ORDDID(N) = OBSNAM(N)
    ENDDO
    IF (MPR>0) THEN
      DO N=1,MPR
        NPNOBS = N+NOBS
        D(NPNOBS,1) = WTDRESIDSPRI(N)
!!! NEED PRI GET GROUP    CALL PRI_GET_GROUP(N,GPNAM,IPLOT)
!!! NEED PRI GET GROUP    D(NPNOBS,2) = DBLE(IPLOT)
        D(NPNOBS,2) = PLOTSYMBOLPRI(N)
        ORDDID(NPNOBS) = PRINAM(N)
      ENDDO
    ENDIF
    R = 0.0D0
    !
    DO N1=1,NOBS+MPR-1
      NMIN=N1
      RMIN=D(N1,1)
      DIDMIN=ORDDID(N1)
      DO N2=N1+1,NOBS+MPR
        IF(D(N2,1) .LE. RMIN) THEN
          RMIN=D(N2,1)
          SMIN=D(N2,2)
          DIDMIN=ORDDID(N2)
          NMIN=N2
        ENDIF
      ENDDO
      IF(NMIN .NE. N1) THEN
        D(NMIN,1)=D(N1,1)
        D(NMIN,2)=D(N1,2)
        ORDDID(NMIN)=ORDDID(N1)
        D(N1,1)=RMIN
        D(N1,2)=SMIN
        ORDDID(N1)=DIDMIN
      ENDIF
    ENDDO
    DO N=1,NOBS+MPR
      RNORM=(REAL((N),KIND(0.0D0))-.5)/(REAL((NOBS+MPR),KIND(0.0D0)))
      CALL STA_EVA_PROB_NORM_DISTRB(R(N),RNORM,-1)
    ENDDO
    !
    RETURN
  END SUBROUTINE STA_EVA_ORDER
  !-----------------------------------------------------------------------------
  SUBROUTINE STA_EVA_PROB_NORM_DISTRB(U,RNORM,IP)
    !   FIND THE PROBABILITY RELATED TO A U (IP=1), OR A U RELATED TO A
    !   PROBABILITY (IP=-1) FOR A STANDARD GAUSSIAN DISTRIBUTION
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    DOUBLE PRECISION, INTENT(INOUT) :: U
    DOUBLE PRECISION, INTENT(INOUT) :: RNORM
    INTEGER,          INTENT(IN)    :: IP
    !
    !   Local variables
    INTEGER :: I
    DOUBLE PRECISION, DIMENSION(2,71) :: PNORM
    DOUBLE PRECISION :: ARNORM, AU, FACTOR
    !
    DATA (PNORM(1,I),I=1,71)/0.00D0,0.15D0,0.20D0,0.25D0,0.30D0,0.35D0,  &
        0.40D0,0.45D0,0.50D0,0.55D0,0.60D0,0.65D0,0.70D0,0.75D0,0.80D0,  &
        0.85D0,0.90D0,0.95D0,1.00D0,1.05D0,1.10D0,1.15D0,1.20D0,1.25D0,  &
        1.30D0,1.35D0,1.40D0,1.45D0,1.50D0,1.55D0,1.60D0,1.65D0,1.70D0,  &
        1.75D0,1.80D0,1.85D0,1.90D0,1.95D0,2.00D0,2.05D0,2.10D0,2.20D0,  &
        2.25D0,2.30D0,2.35D0,2.45D0,2.50D0,2.55D0,2.60D0,2.65D0,2.70D0,  &
        2.75D0,2.80D0,2.85D0,2.90D0,2.95D0,3.00D0,3.05D0,3.10D0,3.15D0,  &
        3.20D0,3.25D0,3.30D0,3.35D0,3.40D0,3.45D0,3.50D0,4.00D0,4.50D0,  &
        5.00D0,5.50D0/
    DATA (PNORM(2,I),I=1,71)/0.5D0,0.5596D0,0.5793D0,0.5987D0,0.6179D0,  &
        0.6368D0,0.6554D0,0.6736D0,0.6915D0,0.7088D0,0.7257D0,0.7422D0,  &
        0.7580D0,0.7734D0,0.7881D0,0.8023D0,0.8159D0,0.8289D0,0.8413D0,  &
        0.8531D0,0.8643D0,0.8749D0,0.8849D0,0.8944D0,0.90320D0,0.91149D0, &
        0.91924D0,0.92647D0,0.93319D0,0.93943D0,0.94520D0,0.95053D0,  &
        0.95543D0,0.95994D0,0.96407D0,0.96784D0,0.97128D0,0.97441D0,  &
        0.9772D0,0.9798D0,0.9821D0,0.98610D0,0.9878D0,0.9893D0,0.9906D0,  &
        0.9918D0,0.9929D0,0.9938D0,0.9946D0,0.9953D0,0.9960D0,0.9965D0,  &
        0.9970D0,0.9974D0,0.9978D0,0.9981D0,0.9984D0,0.9987D0,0.9989D0,  &
        0.9990D0,0.9992D0,0.9993D0,0.9994D0,0.9995D0,0.9996D0,0.9997D0,  &
        0.99976737D0,0.9999683D0,0.99999660D0,0.99999971D0,1.0D0/
    !------------------------------------------------------------------
    !-----GIVEN U, GET THE CUMULATIVE PROBABILITY
    IF(IP .EQ. 1) THEN
      !-------FIND THE VALUES ABOVE AND BELOW U
      AU=ABS(U)
      IF(AU .GE. 5.5) THEN
        RNORM=1.0
        IF(U .LT. 0.0) RNORM=0.0
        RETURN
      ENDIF
      DO I=1,70
        IF(AU .GE. PNORM(1,I).AND.AU .LT. PNORM(1,I+1)) GO TO 150
      ENDDO
      CALL UTL_STOP ('ERROR IN STA_EVA_PROB_NORM_DISTRB -- U NOT FOUND')
      !-------INTERPOLATE
      150 FACTOR=(AU-PNORM(1,I))/(PNORM(1,I+1)-PNORM(1,I))
      RNORM=PNORM(2,I)+FACTOR*(PNORM(2,I+1)-PNORM(2,I))
      IF(U .LT. 0) RNORM=1.0-RNORM
      RETURN
    ENDIF
    !-----GIVEN THE CUMULATIVE PROBABILITY, GET U
    IF(IP .EQ. -1) THEN
      !-------FIND THE VALUES ABOVE AND BELOW RNORM
      ARNORM=RNORM
      IF(RNORM .LT. .50) ARNORM=1.-RNORM
      IF(ARNORM .EQ. 1.0) THEN
        U=5.5
        IF(RNORM .LT. .5) U=-5.5
        RETURN
      ENDIF
      DO I=1,70
        IF(ARNORM .GE. PNORM(2,I).AND.ARNORM .LT. PNORM(2,I+1)) GO TO 190
      ENDDO
      CALL UTL_STOP ('ERROR IN STA_EVA_PROB_NORM_DISTRB -- RNORM NOT FOUND')
      !-------INTERPOLATE
      190 FACTOR=(ARNORM-PNORM(2,I))/(PNORM(2,I+1)-PNORM(2,I))
      U=PNORM(1,I)+FACTOR*(PNORM(1,I+1)-PNORM(1,I))
      IF(RNORM .LT. .50) U=-U
      RETURN
    ENDIF
  END SUBROUTINE STA_EVA_PROB_NORM_DISTRB
  !-----------------------------------------------------------------------------
  SUBROUTINE STA_EVA_RUNS(AVET,NRSO,NPOST,NNEGT,NRUNS,IOUT)
    ! VERSION 20031112 EPP
    !    CALCULATE AND PRINT THE RUNS STATISTIC FOR ALL RESIDUALS
    !
    !   Argument-list variables
    DOUBLE PRECISION, INTENT(IN) :: AVET
    INTEGER,          INTENT(IN) :: NRSO
    INTEGER,          INTENT(IN) :: NPOST
    INTEGER,          INTENT(IN) :: NNEGT
    INTEGER,          INTENT(IN) :: NRUNS
    INTEGER,          INTENT(IN) :: IOUT
    !
    !   Local Variables
    DOUBLE PRECISION :: ERUNS, RN, RNP, RNR, RNS, RP, SDRUNS, ST2RNS, STRUNS
    !
    !   Format statements
    510 FORMAT (/,' STATISTICS FOR ALL RESIDUALS :',/,                    &
        ' AVERAGE WEIGHTED RESIDUAL  :',E10.3,/,                          &
        ' # RESIDUALS >= 0. :',I7,/,' # RESIDUALS < 0.  :',I7,/,          &
        ' NUMBER OF RUNS  :',I5,'  IN',I5,' OBSERVATIONS')
    515 FORMAT (/,' INTERPRETING THE CALCULATED RUNS STATISTIC VALUE OF ' &
        ,G13.3,/,' NOTE: THE FOLLOWING APPLIES ONLY IF ',/,               &
        '        # RESIDUALS >= 0. IS GREATER THAN 10 AND ',/,           &
        '        # RESIDUALS < 0.   IS GREATER THAN 10',/,                &
        ' THE NEGATIVE VALUE MAY INDICATE TOO FEW RUNS:',/,               &
        '    IF THE VALUE IS LESS THAN -1.28,',                           &
        ' THERE IS LESS THAN A 10 PERCENT',/,                             &
        7X,'CHANCE THE VALUES ARE RANDOM,',/,                             &
        '    IF THE VALUE IS LESS THAN -1.645,',                          &
        ' THERE IS LESS THAN A 5 PERCENT',/,                              &
        7X,'CHANCE THE VALUES ARE RANDOM,',/,                             &
        '    IF THE VALUE IS LESS THAN -1.96,',                           &
        ' THERE IS LESS THAN A 2.5 PERCENT',/,                            &
        7X,'CHANCE THE VALUES ARE RANDOM.',/)
    520 FORMAT (/,' INTERPRETING THE CALCULATED RUNS STATISTIC VALUE OF ' &
        ,G13.3,/,' NOTE: THE FOLLOWING APPLIES ONLY IF ',/,               &
        '        # RESIDUALS >= 0. IS GREATER THAN 10 AND ',/,           &
        '        # RESIDUALS < 0.   IS GREATER THAN 10',/,                &
        ' THE POSITIVE VALUE MAY INDICATE TOO MANY RUNS:',/,              &
        '    IF THE VALUE IS GREATER THAN 1.28,',                         &
        ' THERE IS LESS THAN A 10 PERCENT',/,                             &
        7X,'CHANCE THE VALUES ARE RANDOM,',/,                             &
        '    IF THE VALUE IS GREATER THAN 1.645,',                        &
        ' THERE IS LESS THAN A 5 PERCENT',/,                              &
        7X,'CHANCE THE VALUES ARE RANDOM,',/,                             &
        '    IF THE VALUE IS GREATER THAN 1.96,',                         &
        ' THERE IS LESS THAN A 2.5 PERCENT',/,                            &
        7X,'CHANCE THE VALUES ARE RANDOM.',/)
    525 FORMAT (/,' STATISTICS FOR ALL RESIDUALS :',/,                    &
        ' AVERAGE WEIGHTED RESIDUAL  :',E10.3,/,                          &
        ' # RESIDUALS >= 0. :',I7,/,' # RESIDUALS < 0.  :',I7,/,          &
        ' NUMBER OF RUNS  :',I5,'  IN',I5,' OBSERVATIONS',/,              &
         /,' COULD NOT CALCULATE THE RUNS STATISTIC')
    530 FORMAT (/,                                                        &
        ' THE NUMBER OF RUNS EQUALS THE EXPECTED NUMBER OF RUNS')
    !
    RP = (NPOST)
    RN = (NNEGT)
    RNP = 2.*RP*RN
    RNS = RP + RN
    RNR = (NRUNS)
    IF (RNP .GT. 0.0 .AND. RNP .GT. RNS) THEN
      ERUNS = (RNP/RNS) + 1.D0
      SDRUNS = ((RNP*(RNP-RNS))/((RNS**2.)*(RNS-1.)))**.5
      STRUNS = (RNR-ERUNS+.5)/SDRUNS
      ST2RNS = (RNR-ERUNS-.5)/SDRUNS
      WRITE (IOUT,510) AVET/(NRSO), NPOST, NNEGT, NRUNS, NRSO
      IF (ERUNS-RNR .LT. 1.E-30) THEN
        WRITE(IOUT,530)
      ELSE
        IF (STRUNS .LT. 0.0) WRITE (IOUT,515) STRUNS
        IF (ST2RNS .GT. 0.0) WRITE (IOUT,520) ST2RNS
      ENDIF
    ELSE
      WRITE (IOUT,525) AVET/(NRSO), NPOST, NNEGT, NRUNS, NRSO
    ENDIF
    !
    RETURN
  END SUBROUTINE STA_EVA_RUNS
  !-----------------------------------------------------------------------------
  SUBROUTINE STA_CLN()
    !   Deallocate all arrays in the Statistics module
    IMPLICIT NONE
    IF (ALLOCATED(WTDOBS)) DEALLOCATE(WTDOBS)
    RETURN
  END SUBROUTINE STA_CLN
  !-----------------------------------------------------------------------------
END MODULE STATISTICS