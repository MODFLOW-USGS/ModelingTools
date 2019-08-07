PROGRAM MODEL_LINEARITY
USE BASIC
USE GLOBAL_DATA, ONLY: AMESSAGE, ERRSUB, LENDNAM, MAX_STRING_LEN, NPPREC, &
                       VERSIONID
USE DATATYPES
USE SENSITIVITY, ONLY: SEN_UEV_DX_READ_SU
USE STATISTICS, ONLY: STA_UEV_DX_READ_DM
USE EQUATION, ONLY: EQN_INI, EQN_INI_INSTALL, EQN_EVALUATE
USE PRIOR_INFORMATION, ONLY: PRI_UEV_DX_READ_PR
USE UTILITIES
USE UTLUCODE
!
!       CALCULATES BEALE'S MEASURE OF NONLINEARITY
!       MODIFIED BEALE'S MEASURE PROGRAM BY R. L. COOLEY, USGS, DENVER,
!       COLO.   SEE COOLEY AND NAFF (1990, P. 187-198)
!       MODIFIED FOR MODFLOWP BY M. C. HILL 01JUN1992
!       MODIFIED FOR MODFLOW-2000 BY E.R. BANTA 8/12/1999
!       MODIFIED FOR JUPITER COMPATIBILITY BY E.P. POETER (AUGUST/OCTOBER 2004)
!-------ASSIGN VERSION NUMBER AND DATE
  IMPLICIT NONE
  CHARACTER(LEN=40) VERSION
  PARAMETER (VERSION='1.007') !  09/2011
  !
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)                 :: BVAL
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)                 :: BVALS
  DOUBLE PRECISION                                            :: BN
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)                 :: BOPT
  CHARACTER(LEN=12)                                           :: CDUM
  CHARACTER(LEN=10)                                           :: CHDATE
  CHARACTER(LEN=10)                                           :: CHTIME
  CHARACTER(LEN=10)                                           :: CHZONE
  CHARACTER(LEN=3)                                            :: CONVERGE
  INTEGER, ALLOCATABLE, DIMENSION(:)                          :: EPTR
  LOGICAL                                                     :: EQNLVAL
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)                 :: FC
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)                 :: FL
  CHARACTER(LEN=MAX_STRING_LEN)                               :: FN
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)                 :: FOBS
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)                 :: FOPT
  DOUBLE PRECISION                                            :: FSTAT
  INTEGER                                                     :: I
  INTEGER                                                     :: IBDT(8)
  LOGICAL                                                     :: IDENTICAL
  INTEGER                                                     :: IDOF
  INTEGER                                                     :: IDUM
  INTEGER                                                     :: IFAIL
  CHARACTER(LEN=MAX_STRING_LEN)                               :: INNAM
  INTEGER                                                     :: IP
  INTEGER, ALLOCATABLE, DIMENSION(:)                          :: IPLOT
  INTEGER, ALLOCATABLE, DIMENSION(:)                          :: IPTR
  INTEGER                                                     :: ISTAT
  INTEGER                                                     :: ITYP
  INTEGER                                                     :: IUB1
  INTEGER                                                     :: IUB2
  INTEGER                                                     :: IUBE
  INTEGER                                                     :: IUOS
  INTEGER                                                     :: J
  INTEGER                                                     :: K
  LOGICAL                                                     :: LEX = .FALSE.
  INTEGER, ALLOCATABLE, DIMENSION(:)                          :: LN
  INTEGER, ALLOCATABLE, DIMENSION(:)                          :: LOGT
  INTEGER                                                     :: M
  INTEGER                                                     :: MPR
  INTEGER                                                     :: MXRECL
  INTEGER                                                     :: NOBS
  INTEGER                                                     :: NPERD
  INTEGER                                                     :: NPE
  INTEGER                                                     :: NPS
  INTEGER                                                     :: NPTS
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:)           :: OBSNAM
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:)           :: OBSNAMTMP
  CHARACTER(LEN=MAX_STRING_LEN)                               :: OUTNAM
  CHARACTER(LEN=MAX_STRING_LEN)                               :: OUTNAMTMP
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)               :: OVALS
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)                :: PARNAM
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)                :: PARNAMALL
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)                :: PARNAMALLLC
  INTEGER, ALLOCATABLE, DIMENSION(:)                          :: PINC
  INTEGER, ALLOCATABLE,  DIMENSION(:)                         :: PLOTSYMBOL
  INTEGER, ALLOCATABLE,  DIMENSION(:)                         :: PLOTSYMBOLPRI
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)                :: PNAM
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)                :: PNAMTMP
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:)    :: PRIEQNTXT
  INTEGER, ALLOCATABLE, DIMENSION(:)                          :: PRILN
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:)           :: PRINAM
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:)           :: PRINAMLC
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)                 :: PRIVALOBS
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)                 :: PRIVALOBSTMP
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)                 :: PRIVALOPT
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)                 :: PRIVALSET
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)                 :: PVAL
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)               :: PVALS
  DOUBLE PRECISION                                            :: RDUM
  CHARACTER (LEN=3)                                           :: SENTYPE
  DOUBLE PRECISION                                            :: SSE
  DOUBLE PRECISION                                            :: SSTAT
  DOUBLE PRECISION                                            :: SUM
  DOUBLE PRECISION                                            :: SUMA
  DOUBLE PRECISION                                            :: SUMAA
  DOUBLE PRECISION                                            :: SUMB
  DOUBLE PRECISION                                            :: SUMC
  DOUBLE PRECISION                                            :: SUMCC
  DOUBLE PRECISION                                            :: SUMD
  DOUBLE PRECISION                                            :: SUMDD
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)                 :: SUMY
  DOUBLE PRECISION                                            :: SUMYY
  DOUBLE PRECISION                                            :: TMP
  DOUBLE PRECISION                                            :: TMP2
  DOUBLE PRECISION                                            :: VAR
  TYPE(CDMATRIX)                                              :: WTMATDEP
  TYPE(CDMATRIX)                                              :: PRIWTMAT
  DOUBLE PRECISION                                            :: WVAL
  CHARACTER(LEN=20)                                           :: VERSIONMIN
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)               :: X
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)               :: XORIG
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)               :: XPRI
  ! FORMAT LIST
  100 FORMAT(//,78('!'))
  101 FORMAT(78('!'))
  150 FORMAT(1X,A62,1X,2I5)
  480 FORMAT (32X,'MODEL_LINEARITY',//, &
      24X,'CONSTRUCTED USING THE JUPITER API',/, &
      21X,'UCODE POST-PROCESSING PROGRAM TO CALCULATE',/, &
      25X,'BEALE''S MEASURE OF NONLINEARITY',/, &
      34X,'Version ',A/)
  485 FORMAT (//, &
   1X,78('!'),/, &
   5X,'EXECUTION OF MODEL_LINEARITY REQUIRES DATA_EXCHANGE FILES GENERATED',/, &
   5X,'BY UCODE WHEN IT IS EXECUTED WITH LINEARITY=yes IN THE',/, &
   5X,'UCODE_CONTROL_DATA INPUT BLOCK FOLLOWING A SUCCESSFUL REGRESSION.',//, &
   5X,'THE FILES ARE NOT AVAILABLE SO MODEL_LINEARITY CANNOT PROCEED.',//, &
   5X,'REFER TO THE UCODE USER''S MANUAL FOR DETAILS.',/, &
   1X,78('!'),/)
  490 FORMAT (//, &
      1X,78('!'),/, &
      5X,'THE NUMBER OF ADJUSTABLE PARAMETERS IN THE REGESSION,',I5,','/, &
      5X,'EXCEEDS THE NUMBER OF FINAL PARAMETERS ESTIMATED IN THE REGESSION,', &
          I5,',',//, &
      5X,'THEREFORE MODEL_LINEARITY CANNOT PROCEED',//, &
      5X,'PLEASE REVIEW INFORMATION AT THE  END OF THE *.#umodlin FILE FOR',/, &
      5X,'SUGGESTIONS REGARDING HOW TO PROCEED.',/, &
      1X,78('!'),/)
  500 FORMAT (//,1X,78('*'),/,11X, &
            'WARNING SOME OF THE STATISTICS IN THE DATA-EXCHANGE FILES',/,11X, &
            'WERE GENERATED USING FORWARD DIFFERENCE SENSITIVITIES.',/,11X, &
            'THEY MAY BE INACCURATE AND CAUSE INACCURARCIES IN THE',/,11X, &
            'LINEARITY CALCULATED BY THIS PROGRAM.',/,11X, &
            'CONSIDER REGENERATING THE FILES WITH CENTRAL DIFFERENCES',/,11X, &
            'OR EXACT DERIVATIVES.',/,1X,78('*'),//)
  510 FORMAT ( &
      ' NUMBER OF ESTIMATED PARAMETERS...................:',I5,/, &   !(NPERD),
      ' NUMBER OF OBSERVATIONS...........................:',I5,//, &   !(NOBS),
      ' NUMBER OF PRIOR-INFORMATION ITEMS................:',I5,/, &   !(MPR),
      ' NUMBER OF DATA SETS USED FOR BEALES MEASURE......:',I5,//, &   !(NPTS),
      ' CALCULATED ERROR VARIANCE........................:',G12.5)      !(VAR)
  515 FORMAT (/,23X,'OPTIMUM PARAMETERS',/, &
      4X,2('NO. NAME',10X,'BOPT',8X))
  520 FORMAT (/,6X, &
              'DEPENDENT VARIABLES COMPUTED WITH OPTIMUM PARAMETERS',/, &
              4X,2(' NO. OBSERVATION',10X,'FOPT',8X))
  525 FORMAT (/,21X,'PARAMETERS FOR SAMPLE NO.',I4,/,4X, &
              2('NO. NAME',12X,'B',9X))
  530 FORMAT (/,9X,'DEPENDENT VARIABLES COMPUTED FOR SAMPLE NO.',I4,/, &
              4X,2(' NO. OBSERVATION',10X,'FC',9X))
  532 FORMAT (/, &
      ' F STATISTIC FOR BEALE''S MEASURE SET TO...........:',G12.5)
  535 FORMAT (/,' SENSITIVITIES FOR OPTIMUM PARAMETERS')
  540 FORMAT (/,2X, &
              'LINEARIZED DEPENDENT VARIABLES COMPUTED FOR SAMPLE NO.', &
              I4,/,4X,2('NO.',1X,'OBSERVATION',6X,'FL',9X))
  545 FORMAT (/,' USING FSTAT =',G12.5,', BEALES MEASURE =',G12.5,/, &
              ' IF BEALES MEASURE IS GREATER THAN',G11.2, &
              ', THE MODEL IS NONLINEAR.',/, &
              ' IF BEALES MEASURE IS LESS THAN ',G11.2, &
              ', THE MODEL IS EFFECTIVELY LINEAR,',/, &
              ' AND LINEAR CONFIDENCE INTERVALS ', &
              'ARE FAIRLY ACCURATE IF THE RESIDUALS ARE',/, &
              ' NORMALLY DISTRIBUTED.',/)
  550 FORMAT (/,' SS((FC-FOPT)*W**.5) = ',G11.5,/, &
              ' SS((FL-FOPT)*W**.5) =',G12.5)
  590 FORMAT (/,11X,'OBSERVED VALUES OF THE DEPENDENT VARIABLES',/,4X, &
              2(' NO. OBSERVATION',10X,'OBS',8X))
  600 FORMAT (/,28X,'PRIOR INFORMATION', &
              /, 7X,'LOG TRANSFORMED PARAMETER VALUES ARE PRINTED ', &
                    'IN NATURAL LOG SPACE)', &
              /,4X,2(' NO. PRIOR NAME ',10X,'PRIOBS',8X))
  605 FORMAT (' THE FOLLOWING TABLE SHOWS VALUES OF THE', &
              ' STATISTIC DESCRIBED BY COOLEY AND',/, &
              ' NAFF (1990,P.174,TOP OF RIGHT COLUMN).', &
              ' THE STATISTIC EQUALS THE NONLINEAR SUM',/, &
              ' OF SQUARED ERRORS EVALUATED FOR EACH DATA', &
              ' SET (NSSE) MINUS THE SUM OF SQUARED',/, &
              ' ERRORS FOR THE OPTIMUM PARAMETER VALUES (',G10.3,').',/, &
            ' IF THE MODEL IS LINEAR, THE STATISTIC SHOULD BE CLOSE TO ' &
            ,G10.3,'.',/, &
            ' IF THE CORRELATIONS BETWEEN PARAMETERS IS SMALL, THE', &
            ' TABLE SHOWS WHICH ',/,' INDIVIDUAL', &
           ' PARAMETERS ARE MOST NONLINEAR. THE FIRST PAIR OF PARAMETER' &
           ,' SETS ARE ',/,' RELATED TO THE', &
           ' FIRST PARAMETER, THE SECOND PAIR ARE RELATED TO THE SECOND' &
           ,/,' PARAMETER, AND SO ON.',//,' PARAMETER',17X, &
           '      STATISTIC       PERCENT',/, &
           '   SET       NSSE    STATISTIC - ',G10.3,'   DIFFERENCE',/)
  610 FORMAT (I6,5X,2G10.3,G12.3,G12.2)
  620 FORMAT (/,9X,'PARAMETER',/,3X,'NO.',5X,'NAME',7X,'LN')
  625 FORMAT (1X,I5,3X,A,3X,I2)
  900 FORMAT(/,1X,70('*'),/, &
             '  Normal termination of MODEL_LINEARITY, Version: ',A, &
             /,1X,70('*'),//)
  901 FORMAT(/,1X,63('!'),/, &
             '  Unsatisfactory termination of MODEL_LINEARITY, Version: ',A, &
             /,1X,63('!'),//)
  1039 FORMAT(/,1X,'WEIGHT MATRIX FOR OBSERVATIONS WILL BE READ FROM ', &
                 'FILE: ',/,20X,A)
  !
  CALL DATE_AND_TIME(CHDATE,CHTIME,CHZONE,IBDT)
  ! WRITE PROGRAM NAME AND VERSION TO SCREEN
  WRITE (*,*)
  WRITE (*,480) VERSION
  ! GET INPUT FILE NAMES FROM COMMAND LINE
  OUTNAM = UTL_GETARG(1)
  IF(OUTNAM .EQ. ' ') THEN
    WRITE(*,*)' !! ERROR MISSING ROOTFILENAME on MODEL_LINEARITY COMMAND LINE!!'
    CALL UTL_STOP &
    (' PLEASE INCLUDE A ROOTFILENAME on the MODEL_LINEARITY COMMAND LINE')
  ENDIF
  ! OPEN FILES
  IUBE = UTL_GETUNIT(101,150)
  FN = TRIM(OUTNAM)//'.#modlin'
  OPEN (IUBE,FILE=FN,STATUS='UNKNOWN')
  WRITE (IUBE,480) VERSION
  !
  VERSIONMIN = '1.7.3'
  ! CHECK VERSION OF API COMPATIBILITY WITH CODE
  CALL UTL_VERSION_CHECK(VERSIONMIN,ISTAT)
  IF(ISTAT < 0) THEN
    AMESSAGE = ' Programming error:  Version '//TRIM(VERSIONMIN)//   &
               ' of JUPITER API is required.  Version in use is '   &
               //TRIM(VERSIONID)//' -- Please download newer version '   &
               //'of JUPITER API from U.S. Geological Survey'//   &
               ' JUPITER web site.'
    CALL UTL_WRITE_MESSAGE(IUBE,'yes','yes','yes')
    CLOSE(IUBE)
    CALL UTL_STOP()
  ENDIF
  ! Initialize
  IDENTICAL = .FALSE.
  MXRECL = 1+NPPREC*26
  !  READ BASE DATA
  CALL UTLUCODE_DX_READ_DM(IFAIL,OUTNAM, &
                           RDUM,RDUM,RDUM,RDUM, &
                           CONVERGE,IDUM,RDUM,RDUM,RDUM, &
                           CDUM,CDUM,CDUM,CDUM, &
                           MPR,IDUM,NPE,NPERD,NPS,NOBS, &
                           RDUM,RDUM,RDUM,VAR,IUBE,RDUM,RDUM,SENTYPE)
  IFAIL = 0
  IF(VAR .EQ. 1.E+30) CALL UTL_STOP &
         ('Failed based on _dm: Linearity=yes requires a converged regression')
  NPTS = 2*NPERD
  WRITE (IUBE,510) NPERD, NOBS, MPR, NPTS, VAR
  IF(SENTYPE .EQ. "YES")WRITE(IUBE,500)
  !
  ! DYNAMICALLY ALLOCATE ARRAYS
  ALLOCATE (FC(NOBS), FL(NOBS), FOBS(NOBS), FOPT(NOBS), IPLOT(NOBS), &
            LN(NPERD), OBSNAM(NOBS), OBSNAMTMP(NOBS), OVALS(NOBS,NPTS), &
            PARNAM(NPERD), PRILN(MPR), PRINAM(MPR), PRINAMLC(MPR), &
            PRIVALOBS(MPR), PRIVALOBSTMP(MPR), PRIVALOPT(MPR), &
            PRIVALSET(MPR), PRIEQNTXT(MPR), SUMY(2*NPERD), X(NPERD,NOBS))
  CALL EQN_INI(IFAIL,MPR)
  !
  ! DETERMINE THE VALUE OF THE F STATISTIC
  IDOF = NOBS + MPR - NPERD
  CALL UTL_FSTT(NPERD,IDOF,FSTAT)
  WRITE (IUBE,532) FSTAT
  ! READ & WRITE  SIMULATED VALUES, OBSERVED VALUES, OBSERVATION NAMES, from _os
  IUOS = UTL_GETUNIT(101,150)
  INNAM = TRIM(OUTNAM)//'._os'
  OPEN(UNIT=IUOS,FILE=INNAM,STATUS='OLD',ACTION='READ')
  READ (IUOS,*)
  DO I=1,NOBS
    READ (IUOS,*) FOPT(I),FOBS(I),IDUM,OBSNAM(I)
  ENDDO
  IF(MPR > 0) THEN
    DO I=1,MPR
      READ (IUOS,*) PRIVALOPT(I),PRIVALOBS(I),IDUM,PRINAM(I)
    ENDDO
  ENDIF
  CLOSE(UNIT=IUOS)
  ! READ DATA FOR EACH SAMPLE AND COMPUTE MODIFIED BEALE'S MEASURE, BN,
  ! AND THE STATISTIC FROM COOLEY AND NAFF(1990,P.174,TOP OF RIGHT COLUMN)
  ALLOCATE (BVAL(NPERD), BVALS(NPS), BOPT(NPERD), &
            PARNAMALL(NPS), PARNAMALLLC(NPS), &
            EPTR(NPERD), IPTR(NPE), LOGT(NPS), &
            PINC(NPS), PNAM(NPS), PNAMTMP(NPE), PVAL(NPS), PVALS(NPS,NPTS),&
            PLOTSYMBOL(NOBS),PLOTSYMBOLPRI(MPR), &
            XPRI(NPE,MPR), XORIG(NPE,NOBS))
  ! Read _b files
  OUTNAMTMP = TRIM(OUTNAM)//'._b2'
  INQUIRE(FILE=OUTNAMTMP,EXIST=LEX)
  IF(.NOT. LEX) THEN
    WRITE(*,485)
    WRITE(IUBE,485)
    GO TO 998
  ENDIF
  IF(NPERD < NPE) THEN
    WRITE(*,490)NPE,NPERD
    WRITE(IUBE,490)NPE,NPERD
    GO TO 998
  ENDIF
  IUB1 = UTL_DX_OPEN(OUTNAM,'_b1','OLD',MXRECL)
  CALL UTLUCODE_DX_READ_B(NPTS,NPS,IUB1,PARNAMALL,PVALS)
  CLOSE(UNIT=IUB1)
  IUB2 = UTL_DX_OPEN(OUTNAM,'_b2','OLD',MXRECL)
  CALL UTLUCODE_DX_READ_BOBS(NPTS,NOBS,IUB2,OBSNAM,OVALS)
  CLOSE(UNIT=IUB2)
  CALL UTLUCODE_DX_READ_PAOPT(NPS,OUTNAM,LOGT,PNAM,PINC,PVAL)
  J = 0
  K = 0
  DO I=1,NPS
    IF(PINC(I) >= 0) K = K + 1
    IF(PINC(I) > 0) THEN
      J = J + 1
      PARNAM(J) = PNAM(I)
      BOPT(J) = PVAL(I)
      LN(J) = LOGT(I)
      IPTR(J) = I
      EPTR(J) = K
    ENDIF
  ENDDO
  ! READ AND WRITE SENSITIVITIES FOR OBSERVATIONS
  ! READ SENSITIVITIES FROM  _su FILE
  CALL SEN_UEV_DX_READ_SU(NOBS,NPE,OBSNAMTMP,OUTNAM,PNAMTMP,PLOTSYMBOL,XORIG)
  IF(MPR > 0) CALL SEN_UEV_DX_READ_SU &
              (MPR,NPE,PRINAM,OUTNAM,PNAMTMP,PLOTSYMBOLPRI,XPRI,'_supri')
  IF(NPERD < NPE) THEN
    DO I=1,NPERD
      DO J=1,NOBS
        X(I,J) = XORIG(EPTR(I),J)
      ENDDO
    ENDDO
  ELSE
    DO I=1,NPE
      DO J=1,NOBS
        X(I,J) = XORIG(I,J)
      ENDDO
    ENDDO
  ENDIF
  WRITE (IUBE,515)
  CALL PRTOTB12(NPERD,IUBE,BOPT,PARNAM)
  WRITE (IUBE,520)
  CALL PRTOTB20(NOBS,IUBE,FOPT,OBSNAM)
  WRITE (IUBE,590)
  CALL PRTOTB20(NOBS,IUBE,FOBS,OBSNAM)
  ! READ THE WEIGHT MATRIX
  WRITE(IUBE,1039) TRIM(OUTNAM)//'._wt'
  CALL TYP_NULL(WTMATDEP)
  CALL UTL_DX_READ_WT(IUBE,OUTNAM,'_wt',WTMATDEP)
  !for debugging CALL UTL_WRITECDMATRIX(WTMATDEP,1,IUBE,.TRUE.,OBSNAM,OBSNAM)
  IF(MPR > 0) THEN
    WRITE(IUBE,1039) ' and '//TRIM(OUTNAM)//'._wtpri'
    CALL TYP_NULL(PRIWTMAT)
    CALL UTL_DX_READ_WT(IUBE,OUTNAM,'_wtpri',PRIWTMAT)
    !for debugging CALL UTL_WRITECDMATRIX(PRIWTMAT,1,IUBE,.TRUE.,PRINAM,PRINAM)
  ENDIF
  WRITE (IUBE,535)
  CALL PRTOTX(NOBS,NPERD,IUBE,OBSNAM,PARNAM,X)
  ! READ AND WRITE PRIOR INFORMATION EQUATIONS
  IF (MPR .GT. 0) THEN
    !dont replace priobs value with this read because it is not log transformed
    CALL PRI_UEV_DX_READ_PR(MPR,OUTNAM,IPLOT,PRIEQNTXT,PRINAM,PRIVALOBSTMP)
    WRITE (IUBE,600)
    CALL PRTOTB20(MPR,IUBE,PRIVALOBS,PRINAM)
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in Main MODEL_LINEARITY INSTALLING PRIOR EQUATIONS'
    IFAIL = 0
    DO I=1,MPR
      CALL UTL_CASE(PRINAM(I),PRINAMLC(I),-1) ! Store names as lowercase
      CALL EQN_INI_INSTALL(IFAIL,I,PRINAMLC(I),PRIEQNTXT(I))
      CALL UTLUCODE_LOG_PRI(PRIEQNTXT(I),PRILN(I))
      IF (IFAIL .NE. 0) THEN
        CALL UTL_WRITE_MESSAGE(IUBE,'yes','yes','yes')
        CALL UTL_STOP('EQN_INI_INSTALL reported failure')
      ENDIF
    ENDDO
  ENDIF
  ! CONVERT LOG TRANSFORMED PARAMETERS
  WRITE (IUBE,620)
  WRITE (IUBE,625) (I,PARNAM(I),LN(I),I=1,NPERD)
  DO IP = 1, NPERD
    IF (LN(IP) .NE. 0) THEN
      BOPT(IP) = DLOG(BOPT(IP))
    ENDIF
  ENDDO
  DO I=1,NPS
    CALL UTL_CASE(PARNAMALL(I),PARNAMALLLC(I),-1) ! Store names as lowercase
  ENDDO
  SUMA = 0.D0
  SUMB = 0.D0
  DO M = 1, NPTS
    DO I=1,NPERD
      BVAL(I) = PVALS(IPTR(I),M)
    ENDDO
    DO I=1,NPS
      BVALS(I) = PVALS(I,M)
    ENDDO
    WRITE (IUBE,525) M
    CALL PRTOTB12(NPERD,IUBE,BVAL,PARNAM)
    !   Calculate prior values for current parameter set
    DO J=1,MPR
      CALL EQN_EVALUATE(IFAIL,J,NPS,PARNAMALLLC,BVALS,ITYP,PRIVALSET(J),EQNLVAL)
      IF (IFAIL .NE. 0) THEN
        CALL UTL_WRITE_MESSAGE(-1,'yes','yes','yes')
        CALL UTL_STOP('EQN_EVALUATE failed evaluating a prior equation')
      ENDIF
      IF(PRILN(J) == 1) PRIVALSET(J) = PRIVALSET(J) * 2.30258509299405D0
    ENDDO
    DO I = 1, NPERD
      IF (LN(I) .NE. 0) THEN
        BVAL(I) = DLOG(BVAL(I))
      ENDIF
    ENDDO
    ! GET VALUES SIMULATED WITH THIS PARAMETER SET
    DO I=1,NOBS
      FC(I) = OVALS(I,M)
    ENDDO
    WRITE (IUBE,530) M
    CALL PRTOTB20(NOBS,IUBE,FC,OBSNAM)
    ! DO CALCULATIONS FOR THIS SET
    SUMC = 0.D0
    SUMD = 0.D0
    SUMY(M) = 0.D0
    ! ALL DEPENDENTS AND PRIOR HAVE FULL WEIGHT MATRIX
    ! CALCULATE LINEARIZED VALUES
    DO J = 1,NOBS
      SUM = FOPT(J)
      DO I = 1, NPERD
        SUM = SUM + X(I,J)*(BVAL(I)-BOPT(I))
      ENDDO
      FL(J) = SUM
    ENDDO
    !
    DO I = 1,NOBS
      SUMAA = 0.D0
      SUMCC = 0.D0
      SUMDD = 0.D0
      SUMYY = 0.D0
      DO J = 1,NOBS
        WVAL = UTL_GETVAL(WTMATDEP,I,J)
        IF(WVAL == 0.D0) CYCLE
        SUMAA = SUMAA + WVAL*(FC(J)-FL(J))
        SUMCC = SUMCC + WVAL*(FC(J)-FOPT(J))
        SUMDD = SUMDD + WVAL*(FL(J)-FOPT(J))
        SUMYY = SUMYY + WVAL*(FOBS(J)-FC(J))
      ENDDO
      SUMA = SUMA + SUMAA*(FC(I)-FL(I))
      SUMC = SUMC + SUMCC*(FC(I)-FOPT(I))
      SUMD = SUMD + SUMDD*(FL(I)-FOPT(I))
      SUMY(M) = SUMY(M) + SUMYY*(FOBS(I)-FC(I))
    ENDDO
    IF(MPR > 0) THEN
      DO J = 1, MPR
        SUMCC = 0.D0
        SUMDD = 0.D0
        SUMYY = 0.D0
        ! residual of prior at current values - evaluated at optimal values
        TMP = PRIVALSET(J)-PRIVALOPT(J)
        ! residual of prior observed - evaluated at current values
        TMP2 = PRIVALOBS(J)-PRIVALSET(J)
        DO  I = 1, MPR
          WVAL = UTL_GETVAL(PRIWTMAT,I,J)
          IF(WVAL == 0.D0) CYCLE
          SUMCC = SUMCC + WVAL*TMP
          SUMDD = SUMDD + WVAL*TMP
          SUMYY = SUMYY + WVAL*TMP2
        ENDDO
        SUMC = SUMC + SUMCC*TMP
        SUMD = SUMD + SUMDD*TMP
        SUMY(M) = SUMY(M) + SUMYY*TMP2 ! Check with Steen
      ENDDO
     ENDIF
    ! WRITE RESULTS FOR THIS DATA SET
    WRITE (IUBE,540) M
    CALL PRTOTB20(NOBS,IUBE,FL,OBSNAM)
    WRITE (IUBE,550) SUMC, SUMD
    SUMB = SUMB + SUMD*SUMD
  ENDDO
  ! END LOOP FOR DATA SETS; CALCULATE AND WRITE RESULTS
  DO M = 2, NPTS
    DO I=1,NOBS
      IF(OVALS(I,M) .NE. OVALS(I,M-1)) EXIT
      IF(I == NOBS) THEN
        IDENTICAL = .TRUE.
        AMESSAGE = &
        'WARNING: Identical simulated equivalents, for parameter sets: '
        WRITE(*,100)
        WRITE(*,150)TRIM(AMESSAGE),M-1,M
        WRITE(*,101)
        WRITE(IUBE,100)
        WRITE(IUBE,150)TRIM(AMESSAGE),M-1,M
        WRITE(IUBE,101)
      ENDIF
    ENDDO
    IF(M == NPTS .AND. IDENTICAL) THEN
        AMESSAGE = &
        ' Identical simulated equivalents, Beales Measure may be incorrect'
        WRITE(IUBE,101)
        WRITE(IUBE,*)TRIM(AMESSAGE)
        WRITE(*,101)
        WRITE(*,*)TRIM(AMESSAGE)
        AMESSAGE = &
        ' Did process model successfully converge for all parameter sets?'
        WRITE(IUBE,*)TRIM(AMESSAGE)
        WRITE(*,*)TRIM(AMESSAGE)
        WRITE(IUBE,101)
        WRITE(*,101)
    ENDIF
  ENDDO
  TMP = REAL(NPERD)
  BN = TMP*VAR*SUMA/SUMB
  WRITE (IUBE,545) FSTAT, BN, 1./FSTAT, .09/FSTAT
  SSTAT = VAR*NPERD*FSTAT
  SSE = VAR*(NOBS+MPR-NPERD)
  WRITE (IUBE,605) SSE, SSTAT, SSTAT
  WRITE (IUBE,610) (I,SUMY(I),SUMY(I)-SSE,SUMY(I)-SSE-SSTAT, &
                    100.*(SUMY(I)-SSE-SSTAT)/SSTAT,I=1,NPTS)
  CALL TYP_DEALLOC(WTMATDEP)
  IF(MPR > 0)CALL TYP_DEALLOC(PRIWTMAT)
  GO TO 999
  998 WRITE(*,901)VERSION
  WRITE(IUBE,901)VERSION
  GO TO 1000
  999 WRITE(*,900)VERSION
  WRITE(IUBE,900)VERSION
  1000 CALL UTL_ENDTIME(IBDT,IUBE)
  CLOSE(UNIT=IUBE)
  STOP
END PROGRAM MODEL_LINEARITY
  !=============================================================================
  !=============================================================================
  SUBROUTINE PRTOTB12(NO,IOUT,VAL,VID)
  !**PRINT VALUES IN TWO GROUPS OF THREE COLUMNS
  IMPLICIT NONE
  ! Argument variables
  INTEGER,                    INTENT(IN) :: NO
  INTEGER,                    INTENT(IN) :: IOUT
  DOUBLE PRECISION,           INTENT(IN) :: VAL(NO)
  CHARACTER(LEN=12),          INTENT(IN) :: VID(NO)
  ! local variables
  INTEGER                                :: K
  INTEGER                                :: L
  INTEGER                                :: NR
  ! formats
  500 FORMAT (3X,2(I3,2X,A12,1X,G11.5,3X))
  !
  NR = NO/2
  IF (2*NR .NE. NO) NR = NR + 1
  DO K = 1, NR
    WRITE (IOUT,500) (L,VID(L),VAL(L),L=K,NO,NR)
  ENDDO
  RETURN
  END SUBROUTINE PRTOTB12
  !=============================================================================
  !=============================================================================
  SUBROUTINE PRTOTB20(NO,IUOUT,VAL,VID)
  !**PRINT VALUES IN TWO GROUPS OF THREE COLUMNS
  USE GLOBAL_DATA, ONLY: LENDNAM
  IMPLICIT NONE
  ! Argument variables
  INTEGER,                  INTENT(IN) :: NO
  INTEGER,                  INTENT(IN) :: IUOUT
  DOUBLE PRECISION,         INTENT(IN) :: VAL(NO)
  CHARACTER(LEN=LENDNAM),   INTENT(IN) :: VID(NO)
  ! local variables
  INTEGER                              :: K
  INTEGER                              :: L
  INTEGER                              :: NR
  ! formats
  500 FORMAT (3X,2(I3,2X,A20,1X,G11.5,3X))
  !
  NR = NO/2
  IF (2*NR .NE. NO) NR = NR + 1
  DO K = 1, NR
    WRITE (IUOUT,500) (L,VID(L),VAL(L),L=K,NO,NR)
  ENDDO
  RETURN
  END SUBROUTINE PRTOTB20
!===============================================================================
!===============================================================================
SUBROUTINE PRTOTX(NC,NR,IOUT,OBSNAM,PARNAM,X)
  USE GLOBAL_DATA, ONLY: LENDNAM
!**PRINT MATRICES DIVIDED VERTICALLY INTO Two-COLUMN BLOCKS
  IMPLICIT NONE
  ! Argument variables
  INTEGER,               INTENT(IN)             :: NC
  INTEGER,               INTENT(IN)             :: NR
  INTEGER,               INTENT(IN)             :: IOUT
  CHARACTER(LEN=LENDNAM),INTENT(IN)             :: OBSNAM(NC)
  CHARACTER(LEN=12),     INTENT(IN)             :: PARNAM(NR)
  DOUBLE PRECISION,      INTENT(IN)             :: X(NR,NC)
  ! local variables
  INTEGER                                       :: I
  INTEGER                                       :: J
  INTEGER                                       :: J1
  INTEGER                                       :: K
  ! formats
  500 FORMAT (/,2X,'PARAMETER',4X,2(A20,1X))
  505 FORMAT (1X,A12,1X,2(G20.8,1X))
  510 FORMAT (1X,12('-'),1X,2(A20,1X))
  DO K = 1, NC, 2
    J1 = K + 1
    IF (J1.GT.NC) J1 = NC
    WRITE (IOUT,500) (OBSNAM(J),J=K,J1)
    WRITE (IOUT,510) ('--------------------',J=K,J1)
    DO I = 1, NR
      WRITE (IOUT,505) PARNAM(I), (X(I,J),J=K,J1)
    ENDDO
  ENDDO
  RETURN
END SUBROUTINE PRTOTX
!===============================================================================
!===============================================================================
