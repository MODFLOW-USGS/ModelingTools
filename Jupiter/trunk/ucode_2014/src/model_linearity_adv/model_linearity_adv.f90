PROGRAM MODEL_LINEARITY_ADV
USE GLOBAL_DATA, ONLY: AMESSAGE, ERRSUB, LENDNAM, MAX_STRING_LEN, NPPREC, &
                       VERSIONID
USE DATATYPES
USE DEPENDENTS
USE PRIOR_INFORMATION, ONLY: PRI_UEV_DX_READ_PR, PRI_UEV_DX_READ_PRP
USE SENSITIVITY, ONLY: SEN_UEV_DX_READ_SU
USE STATISTICS, ONLY: STA_UEV_DX_READ_DM
USE EQUATION, ONLY: EQN_INI, EQN_INI_INSTALL, EQN_EVALUATE
USE UTILITIES
USE UTLUCODE
  !
  !       CALCULATES MODIFIED BEALE'S AND COOLEY'S MEASURES OF
  !       TOTAL, INTRINSIC, AND COMBINED INTRINSIC NONLINEARITY
  !
  ! It is modified from the code below to be compatible with the JUPITER API
  !
  !       PROGRAM BY R. L. COOLEY, USGS, DENVER, COLO.
  !       MODIFIED FOR MODFLOW-2000 BY STEEN CHRISTENSEN, DEPT. OF EARTH
  !         SCIENCES, UNIVERSITY OF AARHUS, DENMARK.
  !       USES PIECES FROM BEALE2K BY E.R. BANTA 8/12/1999
  !       MODIFIED FOR JUPITER AND UCODE
  !       BY EILEEN POETER IGWMC, COLORADO SCHOOL OF MINES JULY 2005
  IMPLICIT NONE
  !-------ASSIGN VERSION NUMBER AND DATE
  CHARACTER(LEN=40) VERSION
  PARAMETER (VERSION='1.007') !  09/2011
  LOGICAL                                                  :: ANALCONF
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)              :: B
  DOUBLE PRECISION                                         :: BNT
  DOUBLE PRECISION                                         :: BNI
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)              :: BOPT
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)            :: C
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)              :: CC
  CHARACTER(LEN=12)                                        :: CDUM
  DOUBLE PRECISION                                         :: CF1
  CHARACTER(LEN=10)                                        :: CHDATE
  CHARACTER(LEN=200)                                       :: CHECK
  CHARACTER(LEN=10)                                        :: CHTIME
  CHARACTER(LEN=10)                                        :: CHZONE
  DOUBLE PRECISION                                         :: CI
  CHARACTER(LEN=3)                                         :: CONVERGE
  DOUBLE PRECISION                                         :: CR
  DOUBLE PRECISION                                         :: CR2
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)              :: D
  DOUBLE PRECISION                                         :: DET
  DOUBLE PRECISION                                         :: DG
  INTEGER, ALLOCATABLE, DIMENSION(:)                       :: EPTR
  LOGICAL                                                  :: EQNLVAL
  CHARACTER(LEN=4)                                         :: EXTB
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)              :: FC
  LOGICAL                                                  :: FIRST
  LOGICAL                                                  :: FIRSTSEN
  CHARACTER(LEN=MAX_STRING_LEN)                            :: FN
  CHARACTER(LEN=MAX_STRING_LEN)                            :: FNDMP
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)              :: FOBS
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)              :: FOPT
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)              :: FOPTPREV
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)              :: G
  DOUBLE PRECISION                                         :: GDUM
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:)        :: GNAM
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)              :: GOPT
  INTEGER, ALLOCATABLE, DIMENSION(:)                       :: GOPTLN
  DOUBLE PRECISION                                         :: GP
  DOUBLE PRECISION                                         :: GPD
  INTEGER                                                  :: I
  INTEGER                                                  :: IBDT(8)
  INTEGER                                                  :: ICNT
  INTEGER, ALLOCATABLE, DIMENSION(:)                       :: ID
  LOGICAL                                                  :: IDENTICAL
  INTEGER                                                  :: IDUM
  INTEGER                                                  :: IFAIL
  INTEGER                                                  :: IIN1
  INTEGER                                                  :: IMP
  INTEGER, ALLOCATABLE, DIMENSION(:)                       :: IPTR
  INTEGER                                                  :: IOUT
  INTEGER                                                  :: IOUTA
  INTEGER                                                  :: IP
  INTEGER                                                  :: ISET
  INTEGER                                                  :: ISTAT
  INTEGER                                                  :: ISTOP
  INTEGER                                                  :: ITYP
  INTEGER                                                  :: ITYPE
  INTEGER                                                  :: IUB1 = 0
  INTEGER                                                  :: IUB2 = 0
  INTEGER                                                  :: IUB3 = 0
  INTEGER                                                  :: IUB4 = 0
  INTEGER                                                  :: IUCORFACS = 0
  INTEGER                                                  :: IUCORSU = 0
  INTEGER                                                  :: IUPR = 0
  INTEGER                                                  :: J
  INTEGER                                                  :: K
  LOGICAL                                                  :: LEX = .FALSE.
  INTEGER, ALLOCATABLE, DIMENSION(:)                       :: LN
  INTEGER, ALLOCATABLE, DIMENSION(:)                       :: LOGT
  INTEGER                                                  :: M
  CHARACTER(LEN=MAX_STRING_LEN)                            :: MESSAGE= ' '
  INTEGER                                                  :: MPR
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)            :: MVALS2
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)            :: MVALS4
  INTEGER                                                  :: MXRECL
  INTEGER                                                  :: NO
  INTEGER                                                  :: NOBS
  INTEGER                                                  :: NOINT
  INTEGER                                                  :: NOINTORIG
  INTEGER, ALLOCATABLE, DIMENSION(:)                       :: NOPTR
  INTEGER                                                  :: NPE
  INTEGER                                                  :: NPEORIG
  INTEGER                                                  :: NPI
  INTEGER                                                  :: NPIORIG
  INTEGER                                                  :: NPREDI
  INTEGER                                                  :: NPS
  INTEGER                                                  :: NPTS
  INTEGER                                                  :: NTOT
  INTEGER                                                  :: NPSWOP = 0
  INTEGER                                                  :: NUM_OLDPRI = 0
  INTEGER                                                  :: NUM_NEWPRI = 0
  INTEGER                                                  :: NUM_OLDOBS = 0
  INTEGER                                                  :: NUM_NEWOBS = 0
  INTEGER                                                  :: NUM_OLDPAR = 0
  INTEGER                                                  :: NUM_NEWPAR = 0
  INTEGER                                                  :: NVARP = 0
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:)        :: OBSNAM
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:)        :: OBSNAM4
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:)        :: OBSNAMTMP
  CHARACTER(LEN=MAX_STRING_LEN)                            :: OUTNAM
  CHARACTER(LEN=MAX_STRING_LEN)                            :: OUTNAMPRED
  CHARACTER(LEN=MAX_STRING_LEN)                            :: OUTNAMTMP
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)             :: PARNAM
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)             :: PARNAMALLLC
  INTEGER, ALLOCATABLE, DIMENSION(:)                       :: PINC
  INTEGER, ALLOCATABLE, DIMENSION(:)                       :: PLOTSYM
  INTEGER, ALLOCATABLE, DIMENSION(:)                       :: PLOTSYMBOL
  INTEGER, ALLOCATABLE, DIMENSION(:)                       :: PLOTSYMBOLPRI
  INTEGER, ALLOCATABLE, DIMENSION(:)                       :: PLOTSYMBOLPRITMP
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)             :: PNAM
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)             :: PNAMTMP
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: PRIEQNTXT
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: PRIEQNTXTTMP
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:)        :: PRINAM
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:)        :: PRINAMTMP
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:)        :: PRINAMLC
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)              :: PRIVALOBS
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)              :: PRIVALOBSTMP
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)              :: PRIVALSET
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)              :: PVAL
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)            :: PVALS1
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)            :: PVALS3
  DOUBLE PRECISION                                         :: QI
  DOUBLE PRECISION                                         :: RDUM
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)              :: SCLE
  LOGICAL                                                  :: SECOND
  CHARACTER (LEN=3)                                        :: SENTYPE
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)              :: SQCI
  DOUBLE PRECISION                                         :: SCL
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)              :: SQF0
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)              :: SQG0
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)              :: SQCIB
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)              :: SQCIMX
  DOUBLE PRECISION                                         :: SQI
  DOUBLE PRECISION                                         :: SQL
  DOUBLE PRECISION                                         :: SQLB
  DOUBLE PRECISION                                         :: SQT
  DOUBLE PRECISION                                         :: SUMCI
  DOUBLE PRECISION                                         :: SUMCIB
  DOUBLE PRECISION                                         :: SUMQI
  DOUBLE PRECISION                                         :: SUMQL
  DOUBLE PRECISION                                         :: SUMQLB
  DOUBLE PRECISION                                         :: SUMQT
  DOUBLE PRECISION                                         :: UP
  DOUBLE PRECISION                                         :: VAR
  DOUBLE PRECISION                                         :: VG
  DOUBLE PRECISION                                         :: VP
  DOUBLE PRECISION                                         :: VYP
  CHARACTER(LEN=20)                                        :: VERSIONMIN
  DOUBLE PRECISION                                         :: W
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)              :: WG
  TYPE (CDMATRIX)                                          :: PRIWTMAT
  TYPE (CDMATRIX)                                          :: WTMATDEP
  TYPE (CDMATRIX)                                          :: WTMATALL
  TYPE (CDMATRIX)                                          :: WTMATALL1
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)            :: XSENS
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)            :: XPRI
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)            :: XPRITMP
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)            :: XORIG
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)            :: Z
  !
  !**FORMAT LIST
  100 FORMAT(/,78('!'),/)
  101 FORMAT(/,78('!'))
  103 FORMAT(/,78('!'),/,A,/,78('!'))
  150 FORMAT(1X,A62,1X,2I5)
  480 FORMAT (30X,'MODEL_LINEARITY_ADV',//, &
      24X,'CONSTRUCTED USING THE JUPITER API',/, &
      17X,'UCODE POST-PROCESSING PROGRAM TO CALCULATE',/, &
      8X,'MEASURES OF TOTAL, INTRINSIC, AND COMBINED INTRINSIC', &
          ' NONLINEARITY',/, &
      34X,'Version ',A/)
  481 FORMAT (' !!!! ERROR MISSING ROOTFILENAME ON MODEL_LINEARITY_ADV ', &
              'COMMAND LINE !!!')
  482 FORMAT &
      (' This file contains messages generated before analysis type is read.', &
     /,' A message here does not necessarily indicate an error. The code is', &
     /,' checking for files it may need and reporting their status.', &
     //,' Look for analysis results in files with a #modlinadv_conf or', & 
     /,' #modlinadv_pred extension.')
  485 FORMAT (//, &
   1X,78('!'),/, &
   5X,'EXECUTION OF MODEL_LINEARITY_ADV REQUIRES DATA_EXCHANGE FILES',/, &
   5X,'GENERATED BY UCODE WHEN IT IS EXECUTED WITH LINEARITYADV=yes',/, &
   5X,'IN THE UCODE_CONTROL_DATA INPUT BLOCK FOLLOWING A SUCCESSFUL',/, &
   5X,'REGRESSION. THE FILES ARE NOT AVAILABLE SO MODEL_LINEARITYADV',/, &
   5X,'CANNOT PROCEED.',//, &
   5X,'REFER TO THE UCODE USER''S MANUAL FOR DETAILS.',/, &
   1X,78('!'),/)
  500 FORMAT (//,1X,78('*'),/,11X, &
            'WARNING SOME OF THE STATISTICS IN THE DATA-EXCHANGE FILES',/,11X, &
            'WERE GENERATED USING FORWARD DIFFERENCE SENSITIVITIES.',/,11X, &
            'THEY MAY BE INACCURATE AND CAUSE INACCURARCIES IN THE',/,11X, &
            'LINEARITY CALCULATED BY THIS PROGRAM.',/,11X, &
            'CONSIDER REGENERATING THE FILES WITH CENTRAL DIFFERENCES',/,11X, &
            'OR EXACT DERIVATIVES.',/,1X,78('*'),//)
  510 FORMAT ( &
      ' NUMBER OF ESTIMATED PARAMETERS...................:',I5,/, &   !(NPE),
      ' NUMBER OF OBSERVATIONS...........................:',I5,/, &   !(NOBS),
      ' NUMBER OF PRIOR-INFORMATION ITEMS................:',I5,//, &   !(MPR),
      ' NUMBER OF DATA SETS USED FOR LINEARITY MEASURE...:',I5,//, &   !(NPTS),
      ' CALCULATED ERROR VARIANCE........................:',G12.5,//)   !(VAR)
  515 FORMAT (/,23X,'OPTIMUM PARAMETERS',/, &
      4X,2('NO. NAME',10X,'BOPT',8X))
  520 FORMAT (/,6X, &
              'DEPENDENT VARIABLES COMPUTED WITH OPTIMUM PARAMETERS',/, &
              2X,2(2X,'NO. OBSERVATION',11X,'FOPT',8X))
  521 FORMAT (/,6X, &
              'INTERVAL VARIABLES COMPUTED WITH OPTIMUM PARAMETERS',/, &
              2X,2(2X,'NO. INTERVAL  ',12X,'GOPT',8X))
  525 FORMAT (/,21X,'PARAMETERS FOR SAMPLE NO.',I4,/,2X, &
              2(2X,'NO. NAME',10X,'B',11X))
  530 FORMAT (/,9X,'DEPENDENT VARIABLES COMPUTED FOR SAMPLE NO.',I4,/, &
              2X,2(2X,'NO. OBSERVATION',11X,'FC',10X))
  535 FORMAT (/,' SENSITIVITIES FOR OPTIMUM PARAMETERS')
  556 FORMAT (/,11X,'RELIABILITY WEIGHTS FOR INTERVAL SAMPLE',/,2X, &
              2(2X,'NO. INTERVAL ',12X,'W',12X))
  590 FORMAT (/,11X,'OBSERVED VALUES OF THE DEPENDENT VARIABLES',/,2X, &
              2(2X,'NO. OBSERVATION',11X,'FOBS',8X))
  600 FORMAT (/,' PRIOR DURING REGRESSION:',/, &
              3X,'NAME                 ESTIMATE(LOG IF LN>0)  EQUATION',/, &
              3X,'----                 ---------------------  --------')
  601 FORMAT (/,' PRIOR FOR PREDICTION:',/, &
              3X,'NAME                 ESTIMATE(LOG IF LN>0)  EQUATION',/, &
              3X,'----                 ---------------------  --------')
  602 FORMAT (3X,A20,3X,G18.3,3X,A)
  603 FORMAT (/)
  620 FORMAT (/,9X,'PARAMETER',/,3X,'NO.',5X,'NAME',7X,'LN')
  625 FORMAT (1X,I5,3X,A,3X,I2)
  710 FORMAT(//,80('#'),/,11('#'), &
              /,11('#'),4X,'TOTAL NONLINEARITY (BNT).......... = ',G11.5, &
              /,11('#'),4X,'INTRINSIC NONLINEARITY (BNI)...... = ',G11.5, &
              /,11('#'), &
              /,11('#'),'    CRITICAL VALUES FOR BOTH MEASURES:', &
              /,11('#'),'    >1.0 highly nonlinear', &
              /,11('#'),'    0.09 to 1.0 non-linear', &
              /,11('#'),'    0.01 to 0.09 moderately nonlinear', &
              /,11('#'),'    <0.01 effectively linear', &
              /,80('#'),//)
  720 FORMAT(/,1X,60('*'),/,' ****** NONLINEARITY MEASURES FOR ',A, &
             ' INTERVALS ****** ',/,1X,60('*'))
  730 FORMAT(/, &
      ' TOTAL NUMBER OF INTERVALS (NOINT)......................:',I5,/, &
      ' NUMBER OF INTERVALS FOR PARAMETERS (NPI)...............:',I5,/, &
      ' INTERVAL TYPE (1=CONFIDENCE,2=PREDICTION)..............:',I5)
  740 FORMAT (/,16X, &
              'COMBINED INTRINSIC NONLINEARITY',/, &
              8X,'Standard linear intervals are good approximations ',/, &
              14X,'for predictions with values <= 0.01',/, &
              4X,2('  INTERVAL NO. ',5X,' BMI',8X))
  750 FORMAT (/,6X, &
              'COMBINED INTRINSIC NONLINEARITY AS IF F WERE LINEAR ',/, &
              4X,2('  INTERVAL NO. ',5X,'BMF0',8X))
  760 FORMAT (/,6X, &
              'COMBINED INTRINSIC NONLINEARITY AS IF G WERE LINEAR ',/, &
              4X,2('  INTERVAL NO. ',5X,'BMG0',8X))
  770 FORMAT (/,11X, &
              'COMBINED INTRINSIC NONLINEARITY - MAX. SUM',/, &
      5X,'Correction factors are not affected by combined intrinsic model',/, &
      7X,'linearity if this value <0.09. This limit is conservative;',/, &
      13X,'larger values may not affect correction.',/, &
      4X,2('  INTERVAL NO. ',4X,' BMIMAX',6X))
  900 FORMAT(/,1X,70('*'),/, &
             '  Normal termination of MODEL_LINEARITY_ADV, Version: ',A, &
             /,1X,70('*'),//)
  901 FORMAT(/,80('_'))
  1037 FORMAT(20X,A)
  1038 FORMAT(/,1X,'SENSITIVITIES WILL BE READ FROM FILE: ',/,20X,A)
  1039 FORMAT(/,1X,'WEIGHT MATRIX FOR OBSERVATIONS WILL BE READ FROM ', &
                 'FILE: ',/,20X,A)
  !
  CALL DATE_AND_TIME(CHDATE,CHTIME,CHZONE,IBDT)
  !
  !     WRITE PROGRAM NAME AND VERSION TO SCREEN
  WRITE (*,*)
  WRITE (*,480) VERSION
  !  MAIN OUTPUT UNIT
  IOUT = UTL_GETUNIT(7,1000)
  ! GET ROOT NAME
!write(*,*)' enter root' ! only for debugging
!read(*,*) OUTNAM ! only for debugging
  OUTNAM = UTL_GETARG(1)
  IF(OUTNAM .EQ. ' ') THEN
    OPEN (IOUT,FILE='MODEL_LINEARITY_ADV.ERR',STATUS='UNKNOWN')
    WRITE(*,481)
    WRITE(IOUT,481)
    CLOSE(IOUT)
    CALL UTL_STOP &
    (' PLEASE INCLUDE A ROOTFILENAME on the MODEL_LINEARITY_ADV COMMAND LINE')
  ENDIF
  OUTNAMPRED = UTL_GETARG(2)
  IF(OUTNAMPRED .EQ. ' ') OUTNAMPRED = OUTNAM
  FN = TRIM(OUTNAMPRED)//'.#modlinadv'
  IOUTA = UTL_GETUNIT(7,1000)
  OPEN (IOUTA,FILE=FN,STATUS='REPLACE')
  WRITE(IOUTA,480)VERSION
  WRITE(IOUTA,482)
  ! Initialize
  IDENTICAL = .FALSE.
  MXRECL = 1+NPPREC*26
  !**READ BASE DATA
  CALL UTLUCODE_DX_READ_DM(IFAIL,OUTNAM, &
                           RDUM,RDUM,RDUM,RDUM, &
                           CONVERGE,IDUM,RDUM,RDUM,RDUM, &
                           CDUM,CDUM,CDUM,CDUM, &
                           MPR,IDUM,NPEORIG,NPE,NPS,NOBS, &
                           RDUM,RDUM,RDUM,VAR,IOUT,RDUM,RDUM,SENTYPE)
  IF(VAR .EQ. 1.E+30) CALL UTL_STOP &
         ('Failed based on _dm: Linearity=yes requires a converged regression')
  IFAIL = 0
  FNDMP = TRIM(OUTNAMPRED)//'._dmp'
  INQUIRE(FILE=FNDMP,EXIST=LEX)
  IF(LEX) THEN
    IIN1 = UTL_GETUNIT(7,1000)
    OPEN (IIN1,FILE=FNDMP,STATUS='UNKNOWN')
    READ(IIN1,*,END=801)CHECK,NPREDGPS
    READ(IIN1,*,END=801)CHECK,NVARP
    801 CLOSE(UNIT=IIN1)
  ENDIF
  IF(NVARP == 0)NVARP = NPE
  NUM_OLDPRI = MPR
  NUM_NEWPRI = NVARP-NPE
  NUM_OLDOBS = NOBS + MPR
  MPR = NUM_OLDPRI + NUM_NEWPRI
  NUM_NEWOBS = NUM_NEWPRI
  NTOT = NOBS + MPR
  NUM_OLDPAR = NPE
  NUM_NEWPAR = NVARP-NPE
  NPE = NUM_OLDPAR + NUM_NEWPAR
  NPTS = 2*NPE
  NPSWOP = NPS
  NPS = NPS + NUM_NEWPAR
  !
  !     ALLOCATE MEMORY FOR ARRAYS
  ALLOCATE(C(NPE,NPE),D(NPE),G(NPE),SCLE(NPE),PARNAM(NPE),BOPT(NPE), &
           B(NPE),LN(NPE))
  ALLOCATE (PARNAMALLLC(NPS), EPTR(NPE), IPTR(NPE), &
            LOGT(NPS), PINC(NPS), PNAM(NPS), PVAL(NPS))
  ALLOCATE(OBSNAM(NTOT),OBSNAM4(NTOT+1),FC(NTOT),FOBS(NTOT),FOPT(NTOT), &
           FOPTPREV(NTOT),PLOTSYM(NTOT),XSENS(NPE,NTOT))
  ! Initialize
  C = 0.D0
  G = 0.D0
  GP = 0.D0
  GPD = 0.D0
  SCLE = 0.D0
  BOPT = 0.D0
  B = 0.D0
  LN = 0
  EPTR = 0
  IPTR = 0
  FC = 0.D0
  FOBS = 0.D0
  FOPT = 0.D0
  FOPTPREV = 0.D0
  LOGT = 0
  OBSNAM = ' '
  PARNAM = ' '
  PARNAMALLLC = ' '
  PINC = 0
  PNAM = ' '
  PVAL = 0.D0
  XSENS = 0.D0
  CALL EQN_INI(IFAIL,MPR)
  !**OPEN FILES
  ANALCONF = .TRUE.
  FIRST = .TRUE.
  FIRSTSEN = .TRUE.
  SECOND = .TRUE.
  DO
    IF(FIRST .AND. ANALCONF) THEN
      ! check if dataexchange files are ready
      OUTNAMTMP = TRIM(OUTNAMPRED)//'._b2advconf'
      INQUIRE(FILE=OUTNAMTMP,EXIST=LEX)
      IF(.NOT. LEX) THEN
        OUTNAMTMP = TRIM(OUTNAMPRED)//'._b2advpred'
        INQUIRE(FILE=OUTNAMTMP,EXIST=LEX)
      ENDIF
      IF(.NOT. LEX) THEN
        WRITE(*,485)
        FN = TRIM(OUTNAMPRED)//'.#modlinadv_conf'
        OPEN (IOUT,FILE=FN,STATUS='UNKNOWN')
        WRITE(IOUT,485)
        GO TO 999
      ENDIF
      FN = TRIM(OUTNAMPRED)//'._cfconf'
      EXTB = 'conf'
      INQUIRE(FILE=FN,EXIST=LEX)
      IF(LEX) THEN
        ITYP = 1
        FIRST = .FALSE.
        LEX = .FALSE.
        IUCORFACS = UTL_DX_OPEN(OUTNAMPRED,'_cfconf','OLD')
        FN = TRIM(OUTNAMPRED)//'.#modlinadv_conf'
        OPEN (IOUT,FILE=FN,STATUS='UNKNOWN')
        WRITE (IOUT,480) VERSION
        AMESSAGE = &
        'CORFAC_PLUS OR EQUIVALENT GENERATED fn._cfconf'
        WRITE(*,*)
        WRITE(*,*)TRIM(AMESSAGE)
        CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','yes')
        AMESSAGE = &
        ' Linearity with respect to confidence intervals will be calculated'
        WRITE(*,*)
        WRITE(*,*)TRIM(AMESSAGE)
        WRITE(*,*)
        CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','yes')
      ELSE
        ANALCONF = .FALSE.
        AMESSAGE = &
        'CORFAC_PLUS OR EQUIVALENT HAS NOT GENERATED fn._cfconf'
        WRITE(*,*)
        WRITE(*,*)TRIM(AMESSAGE)
        CALL UTL_WRITE_MESSAGE(IOUTA,'no','yes','yes')
        AMESSAGE = &
        ' Linearity with respect to confidence intervals will not be calculated'
        WRITE(*,*)
        WRITE(*,*)TRIM(AMESSAGE)
        CALL UTL_WRITE_MESSAGE(IOUTA,'no','yes','yes')
        AMESSAGE = '   Proceed to check for fn._cfpred'
        WRITE(*,*)
        WRITE(*,*)TRIM(AMESSAGE)
        WRITE(*,*)
        CALL UTL_WRITE_MESSAGE(IOUTA,'no','yes','yes')
        CYCLE
      ENDIF
    ELSEIF(SECOND) THEN
      SECOND = .FALSE.
      FN = TRIM(OUTNAMPRED)//'._cfpred'
      EXTB = 'pred'
      INQUIRE(FILE=FN,EXIST=LEX)
      IF(LEX) THEN
        ITYP = 2
        LEX = .FALSE.
        IUCORFACS = UTL_DX_OPEN(OUTNAMPRED,'_cfpred','OLD')
        CLOSE(IOUT)
        FN = TRIM(OUTNAMPRED)//'.#modlinadv_pred'
        OPEN (IOUT,FILE=FN,STATUS='UNKNOWN')
        WRITE (IOUT,480) VERSION
        AMESSAGE = &
        'CORFAC_PLUS OR EQUIVALENT GENERATED fn._cfpred'
        WRITE(*,*)
        WRITE(*,*)TRIM(AMESSAGE)
        CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','yes')
        AMESSAGE = &
        ' Linearity with respect to prediction intervals will be calculated'
        WRITE(*,*)
        WRITE(*,*)TRIM(AMESSAGE)
        WRITE(*,*)
        CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','yes')
      ELSE
        AMESSAGE = &
        'CORFAC_PLUS OR EQUIVALENT HAS NOT GENERATED fn._cfpred'
        WRITE(*,*)
        WRITE(*,*)TRIM(AMESSAGE)
        CALL UTL_WRITE_MESSAGE(IOUTA,'no','yes','yes')
        AMESSAGE = &
        ' Linearity with respect to prediction intervals will not be calculated'
        WRITE(*,*)
        WRITE(*,*)TRIM(AMESSAGE)
        CALL UTL_WRITE_MESSAGE(IOUTA,'no','yes','yes')
        EXIT
      ENDIF
    ELSE
      EXIT
    ENDIF
    IF(ALLOCATED(NOPTR)) THEN
      CONTINUE
    ELSE
      ! READ Header
      READ(IUCORFACS,*,END=999)
      NPREDI = 0
      NPI = 0
      NPIORIG = 0
      DO
        READ(IUCORFACS,*,END=97)CDUM,IDUM,RDUM,RDUM,CF1
        IF(IDUM == 1) THEN
          NPREDI = NPREDI + 1
        ELSEIF(IDUM ==2) THEN
          NPIORIG = NPIORIG + 1
        ENDIF
      ENDDO
      97 NOINTORIG = NPREDI + NPIORIG
      ALLOCATE(NOPTR(NOINTORIG))
      REWIND(IUCORFACS)
      ! READ Header
      READ(IUCORFACS,*,END=999)
      NOPTR = 1
      DO I=1,NOINTORIG
        READ(IUCORFACS,*,END=98)CDUM,IDUM,RDUM,RDUM,CF1
        IF(IDUM ==2) THEN
          IF(CF1 == 0.D0) THEN
            NOPTR(I) = 0
          ELSE
            NPI = NPI + 1
          ENDIF
        ENDIF
      ENDDO
      98 NOINT = NPREDI + NPI
    ENDIF
    IF(ALLOCATED(PVALS1)) THEN
      CONTINUE
    ELSE
      ! READ PARAMETERS AND SIMULATED VALUES FROM _B files
      FN = TRIM(OUTNAMPRED)//'._b1adv'//EXTB
      INQUIRE(FILE=FN,EXIST=LEX)
      IF(LEX) THEN
        IUB1 = UTL_DX_OPEN(OUTNAMPRED,'_b1adv'//EXTB,'OLD',MXRECL)
      ELSE
        AMESSAGE = &
        'UCODE MUST BE EXECUTED TO OBTAIN '
        WRITE(*,*)TRIM(AMESSAGE)
        CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
        AMESSAGE = &
        'fn._b1adv and _b3 BEFORE RUNNING MODEL_LINEARITY_ADV'
        WRITE(*,*)TRIM(AMESSAGE)
        CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','yes')
        CALL UTL_STOP(' ')
      ENDIF
      FN = TRIM(OUTNAMPRED)//'._b2adv'//EXTB
      INQUIRE(FILE=FN,EXIST=LEX)
      IF(LEX) THEN
        IUB2 = UTL_DX_OPEN(OUTNAMPRED,'_b2adv'//EXTB,'OLD',MXRECL)
      ELSE
        AMESSAGE = &
        'UCODE MUST BE EXECUTED with ModelLinearityAdv = conf or pred TO OBTAIN'
        WRITE(*,*)TRIM(AMESSAGE)
        CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
        AMESSAGE = &
        'fn._b2adv and _b4 BEFORE RUNNING MODEL_LINEARITY_ADV'
        WRITE(*,*)TRIM(AMESSAGE)
        CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','yes')
        CALL UTL_STOP(' ')
      ENDIF
      FN = TRIM(OUTNAMPRED)//'._b3'//EXTB
      INQUIRE(FILE=FN,EXIST=LEX)
      IF(LEX) THEN
        IUB3 = UTL_DX_OPEN(OUTNAMPRED,'_b3'//EXTB,'OLD',MXRECL)
      ELSE
        AMESSAGE = &
        'UCODE MUST BE EXECUTED TO OBTAIN '
        WRITE(*,*)TRIM(AMESSAGE)
        CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
        AMESSAGE = &
        'fn._b3 BEFORE RUNNING MODEL_LINEARITY_ADV'
        WRITE(*,*)TRIM(AMESSAGE)
        CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','yes')
        CALL UTL_STOP(' ')
      ENDIF
      FN = TRIM(OUTNAMPRED)//'._b4'//EXTB
      INQUIRE(FILE=FN,EXIST=LEX)
      IF(LEX) THEN
        IUB4 = UTL_DX_OPEN(OUTNAMPRED,'_b4'//EXTB,'OLD',MXRECL)
      ELSE
        AMESSAGE = &
        'UCODE MUST BE EXECUTED with ModelLinearityAdv = conf or pred TO OBTAIN'
        WRITE(*,*)TRIM(AMESSAGE)
        CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
        AMESSAGE = &
        'fn._b4 BEFORE RUNNING MODEL_LINEARITY_ADV'
        WRITE(*,*)TRIM(AMESSAGE)
        CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','yes')
        CALL UTL_STOP(' ')
      ENDIF
      ALLOCATE(PVALS1(NPS,NPTS),PVALS3(NPS,2*NOINT), &
               MVALS2(NOBS,NPTS),MVALS4(NOBS+1,2*NOINT))
      CALL UTLUCODE_DX_READ_B(NPTS,NPS,IUB1,PNAM,PVALS1)
      CALL UTLUCODE_DX_READ_B(2*NOINT,NPS,IUB3,PNAM,PVALS3)
      CALL UTLUCODE_DX_READ_BOBS(NPTS,NOBS,IUB2,OBSNAM,MVALS2)
      CALL UTLUCODE_DX_READ_BOBS(2*NOINT,NOBS+1,IUB4,OBSNAM4,MVALS4)
      IUB1 = UTL_DX_CLOSE('_b1adv'//EXTB)
      IUB2 = UTL_DX_CLOSE('_b2adv'//EXTB)
      IUB3 = UTL_DX_CLOSE('_b3'//EXTB)
      IUB4 = UTL_DX_CLOSE('_b4'//EXTB)
    ENDIF
    !     ALLOCATE MEMORY FOR ARRAYS
    IF(ALLOCATED(GNAM)) THEN
      CONTINUE
    ELSE
      ALLOCATE(GNAM(NOINT),GOPT(NOINT),GOPTLN(NOINT),WG(NOINT),ID(NOINT), &
               Z(NPE,NOINT), &
               CC(NOINT),SQCI(NOINT),SQF0(NOINT),SQG0(NOINT), &
               SQCIB(NOINT),SQCIMX(NOINT))
    ENDIF
    CC = 0D0
    GNAM = ' '
    GOPT = 0.D0
    GOPTLN = 0
    WG = 0.D0
    Z = 0.D0
    REWIND(IUCORFACS)
    READ(IUCORFACS,*,END=999)
    J = 1
    DO I=1,NOINTORIG
      READ(IUCORFACS,*,END=99)CDUM,IDUM,GDUM,W,CF1,CR,CR2
      IF(ITYP == 2) CR = CR2
      IF(CF1 > 0.D0) THEN
        GNAM(J) = CDUM
        GOPT(J) = GDUM
        WG(J) = W
        CC(J) = CF1
        IF(ITYP == 1) WG(J) = 0.D0
        IF (CC(J).LT.1.E-10) CC(J)=0.D0
        J = J + 1
      ENDIF
    ENDDO
    99 CLOSE(IUCORFACS)
    FN = TRIM(OUTNAMPRED)//'._cfsu'
    INQUIRE(FILE=FN,EXIST=LEX)
    IF(LEX) THEN
      LEX = .FALSE.
      IF(IUCORSU > 0) THEN
        CLOSE(IUCORSU)
      ELSE
        CALL UTLUCODE_DX_READ_CFSU(NOINT,NOINTORIG,NPE,NOPTR, &
                                   OUTNAMPRED,GNAM,PARNAM,ID,Z)
      ENDIF
    ELSE
      AMESSAGE = &
      'FILE fn._cfsu WAS NOT FOUND, TERMINATING'
      WRITE(*,*)TRIM(AMESSAGE)
      CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','yes')
      CALL UTL_STOP(' ')
    ENDIF
    ! START OUTPUT ECHO
    WRITE (IOUT,510) NPE, NOBS, MPR, NPTS, VAR
    IF(SENTYPE .EQ. "YES")WRITE(IOUT,500)
    !     READ AND WRITE PARAMETER INFO
    CALL UTLUCODE_DX_READ_PAOPT(NPS,OUTNAM,LOGT,PNAM,PINC,PVAL,NPSWOP)
    IF(NPS>NPSWOP) THEN
      IUPR = UTL_DX_OPEN(OUTNAMPRED,'_paoptp','OLD')
      READ(IUPR,*)
      READ(IUPR,*)(PNAM(NPSWOP+I),PVAL(NPSWOP+I),LOGT(NPSWOP+I),I=1,NPS-NPSWOP)
    ENDIF
    J = 0
    K = 0
    DO I=1,NPS
      CALL UTL_CASE(PNAM(I),PARNAMALLLC(I),-1) ! Store all names as lowercase
      IF(PINC(I) >= 0) K = K + 1
      IF(PINC(I) > 0 .OR. I > NPSWOP) THEN
        J = J + 1
        PARNAM(J) = PNAM(I)
        BOPT(J) = PVAL(I)
        LN(J) = LOGT(I)
        IPTR(J) = I
        EPTR(J) = K
      ENDIF
    ENDDO
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
      CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
      CLOSE(IOUT)
      CALL UTL_STOP()
    ENDIF
    WRITE (IOUT,515)
    CALL PRTOTB12(NPE,IOUT,BOPT,PARNAM)
    ! READ SIMULATED VALUES, OBSERVED VALUES, OBSERVATION NAMES, from ._os
    CALL DEP_UEV_DX_READ_OS(NUM_OLDOBS,OUTNAM,FOPT,FOBS,PLOTSYM,OBSNAM)

    IF(FIRSTSEN) THEN
      ALLOCATE(OBSNAMTMP(NOBS),PLOTSYMBOL(NOBS),PNAMTMP(NPEORIG), &
               XORIG(NPEORIG,NOBS))
      FIRSTSEN = .FALSE.
      ! READ AND WRITE SENSITIVITIES FOR OBSERVATIONS
      OUTNAMTMP = TRIM(OUTNAM)
      WRITE (IOUT,1038) TRIM(OUTNAM)//'._su'
      CALL SEN_UEV_DX_READ_SU(NOBS,NPEORIG,OBSNAMTMP,OUTNAMTMP,PNAMTMP, &
                              PLOTSYMBOL,XORIG,'_su')
      DEALLOCATE(PNAMTMP)
      IF(MPR > 0) THEN
        ALLOCATE(XPRI(NPE,MPR))
        XPRI = 0.D0
      ENDIF
      IF(NUM_OLDPRI > 0) THEN
        ALLOCATE(PLOTSYMBOLPRITMP(NUM_OLDPRI),PRINAMTMP(NUM_OLDPRI), &
               PNAMTMP(NPEORIG),XPRITMP(NPE,NUM_OLDPRI))
        PLOTSYMBOLPRITMP = 0
        PRINAMTMP = ' '
        XPRITMP = 0.D0
        WRITE (IOUT,1037) ' and '//TRIM(OUTNAM)//'._supri'
        CALL SEN_UEV_DX_READ_SU(NUM_OLDPRI,NPEORIG,PRINAMTMP,OUTNAMTMP, &
                                    PNAMTMP,PLOTSYMBOLPRITMP,XPRITMP,'_supri')
        DO I=1,NUM_OLDPRI
          OBSNAM(NOBS+I) = PRINAMTMP(I)
          DO J=1,NPEORIG
            XPRI(J,I) = XPRITMP(J,I)
          ENDDO
        ENDDO
        DEALLOCATE(PLOTSYMBOLPRITMP,PNAMTMP,PRINAMTMP,XPRITMP)
      ENDIF
      IF(NUM_NEWPRI > 0) THEN
        ALLOCATE(PLOTSYMBOLPRITMP(NUM_NEWPRI),PRINAMTMP(NUM_NEWPRI), &
                 PNAMTMP(NPE), &
                 XPRITMP(NPE,NUM_NEWPRI))
        PLOTSYMBOLPRITMP = 0
        PRINAMTMP = ' '
        XPRITMP = 0.D0
        WRITE (IOUT,1037) ' and '//TRIM(OUTNAMPRED)//'._suprip'
        OUTNAMTMP = TRIM(OUTNAMPRED)
        CALL SEN_UEV_DX_READ_SU(NUM_NEWPRI,NPE,PRINAMTMP, &
                         OUTNAMTMP,PNAMTMP,PLOTSYMBOLPRITMP,XPRITMP,'_suprip')
        DO I=1,NUM_NEWPRI
          OBSNAM(NOBS+NUM_OLDPRI+I) = PRINAMTMP(I)
          DO J=1,NPE
            XPRI(J,NUM_OLDPRI+I) = XPRITMP(J,I)
          ENDDO
        ENDDO
        DEALLOCATE(PLOTSYMBOLPRITMP,PNAMTMP,PRINAMTMP,XPRITMP)
      ENDIF
      IF(NUM_OLDPAR < NPEORIG) THEN
        DO I=1,NUM_OLDPAR
          DO J=1,NOBS
            XSENS(I,J) = XORIG(EPTR(I),J)
          ENDDO
          IF(MPR > 0) THEN
            DO J=1,MPR
              XSENS(I,J+NOBS) = XPRI(I,J)
            ENDDO
          ENDIF
        ENDDO
      ELSE
        DO I=1,NUM_OLDPAR
          DO J=1,NOBS
            XSENS(I,J) = XORIG(I,J)
          ENDDO
          IF(MPR > 0) THEN
            DO J=1,MPR
              XSENS(I,J+NOBS) = XPRI(I,J)
            ENDDO
          ENDIF
        ENDDO
      ENDIF
        IF(NUM_NEWPRI > 0) THEN
          DO I=1,NUM_NEWPRI
            DO J=1,NUM_NEWPRI
              XSENS(NUM_OLDPAR+I,NOBS+NUM_OLDPRI+J) = XPRI(NUM_OLDPAR+I,NUM_OLDPRI+J)
            ENDDO
          ENDDO
        ENDIF
      IF(MPR > 0)DEALLOCATE(XPRI)
      DEALLOCATE(OBSNAMTMP,XORIG)
      ! READ AND WRITE PRIOR INFORMATION EQUATIONS from ._pr
      IF (MPR .GT. 0) THEN
        WRITE(IOUT,603)
        ALLOCATE(PLOTSYMBOLPRI(MPR),PRIEQNTXT(MPR),PRINAM(MPR),PRINAMLC(MPR), &
                 PRIVALOBS(MPR),PRIVALSET(MPR))
        PRINAM = ' '
        PRINAMLC = ' '
        PRIVALOBS = 0.D0
        PRIVALSET = 0.D0
        PRIEQNTXT = ' '
        IF(NUM_OLDPRI > 0) THEN
          ALLOCATE(PLOTSYMBOLPRITMP(NUM_OLDPRI),PRIEQNTXTTMP(NUM_OLDPRI), &
                   PRINAMTMP(NUM_OLDPRI),PRIVALOBSTMP(NUM_OLDPRI))
          PRINAMTMP = ' '
          PRIVALOBSTMP = 0.D0
          PRIEQNTXTTMP = ' '
          CALL PRI_UEV_DX_READ_PR(NUM_OLDPRI,OUTNAM,PLOTSYMBOLPRITMP, &
                                  PRIEQNTXTTMP,PRINAMTMP,PRIVALOBSTMP)
          DO I=1,NUM_OLDPRI
            PLOTSYMBOLPRI(I) = PLOTSYMBOLPRITMP(I)
            PRIEQNTXT(I) = PRIEQNTXTTMP(I)
            PRINAM(I) = PRINAMTMP(I)
            PRIVALOBS(I) = PRIVALOBSTMP(I)
          ENDDO
          WRITE(IOUT,600)
          DO IMP = 1, NUM_OLDPRI
            WRITE(IOUT,602)PRINAM(IMP),PRIVALOBS(IMP), &
                           TRIM(PRIEQNTXT(IMP))
          ENDDO
          DEALLOCATE(PLOTSYMBOLPRITMP,PRIEQNTXTTMP,PRINAMTMP,PRIVALOBSTMP)
        ENDIF
        IF(NUM_NEWPRI > 0) THEN
          ALLOCATE(PLOTSYMBOLPRITMP(NUM_NEWPRI),PRIEQNTXTTMP(NUM_NEWPRI), &
                   PRINAMTMP(NUM_NEWPRI),PRIVALOBSTMP(NUM_NEWPRI))
          PRINAMTMP = ' '
          PRIVALOBSTMP = 0.D0
          PRIEQNTXTTMP = ' '
          CALL PRI_UEV_DX_READ_PRP(NUM_NEWPRI,OUTNAMPRED,PLOTSYMBOLPRITMP, &
                                  PRIEQNTXTTMP,PRINAMTMP,PRIVALOBSTMP)
          DO I=1,NUM_NEWPRI
            PLOTSYMBOLPRI(I+NUM_OLDPRI) = PLOTSYMBOLPRITMP(I)
            PRIEQNTXT(I+NUM_OLDPRI) = PRIEQNTXTTMP(I)
            PRINAM(I+NUM_OLDPRI) = PRINAMTMP(I)
            PRIVALOBS(I+NUM_OLDPRI) = PRIVALOBSTMP(I)
          ENDDO
          WRITE(IOUT,601)
          DO IMP = 1,NUM_NEWPRI
            WRITE(IOUT,602)PRINAM(IMP+NUM_OLDPRI),PRIVALOBS(IMP+NUM_OLDPRI), &
                           TRIM(PRIEQNTXT(IMP+NUM_OLDPRI))
          ENDDO
          DEALLOCATE(PLOTSYMBOLPRITMP,PRIEQNTXTTMP,PRINAMTMP,PRIVALOBSTMP)
        ENDIF
!!!        CALL PRTOTB20(MPR,IOUT,PRIVALOBS,PRINAM)
        ! Initialize
        AMESSAGE = ' '
        ERRSUB=' Error in Main MODEL_LINEARITY INSTALLING PRIOR EQUATIONS'
        IFAIL = 0
        DO I=1,MPR
          CALL UTL_CASE(PRINAM(I),PRINAMLC(I),-1) ! Store names as lowercase
          CALL EQN_INI_INSTALL(IFAIL,I,PRINAMLC(I),PRIEQNTXT(I))
          IF (IFAIL .NE. 0) THEN
            CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
            CALL UTL_STOP('EQN_INI_INSTALL reported failure')
          ENDIF
        ENDDO
        ! Prior values for optimal parameter set
        DO J=1,MPR
          FOBS(NOBS+J) = PRIVALOBS(J)
        ENDDO
        WRITE(IOUT,603)
      ENDIF
    ENDIF
    WRITE (IOUT,520)
    CALL PRTOTB20R(NOBS,NTOT,IOUT,FOPT,OBSNAM)
    WRITE (IOUT,590)
    CALL PRTOTB20(NTOT,IOUT,FOBS,OBSNAM)
    WRITE (IOUT,901)
    WRITE(IOUT,1039) TRIM(OUTNAM)//'._wt'
    CALL TYP_NULL(WTMATDEP)
    CALL TYP_NULL(WTMATALL)
    ! READ THE WEIGHT MATRIX of DEPENDENTS ONLY
    CALL UTL_DX_READ_WT(IOUT,OUTNAM,'_wt',WTMATDEP)
    IF(MPR > 0) THEN
      IF(NUM_OLDPRI > 0) THEN
        CALL TYP_NULL(WTMATALL1)
        WRITE(IOUT,1037) ' and '//TRIM(OUTNAM)//'._wtpri'
        CALL TYP_NULL(PRIWTMAT)
        ! READ THE WEIGHT MATRIX of PRIOR
        CALL UTL_DX_READ_WT(IOUT,OUTNAM,'_wtpri',PRIWTMAT)
        CALL UTL_COMBINESQMATRIX(WTMATDEP,PRIWTMAT,WTMATALL1)
        CALL TYP_DEALLOC(PRIWTMAT)
      ENDIF
      IF(NUM_NEWPRI > 0) THEN
        WRITE(IOUT,1037) ' and '//TRIM(OUTNAMPRED)//'._wtprip'
        CALL TYP_NULL(PRIWTMAT)
        ! READ THE WEIGHT MATRIX of PRIOR
        CALL UTL_DX_READ_WT(IOUT,OUTNAMPRED,'_wtprip',PRIWTMAT)
        IF(NUM_OLDPRI > 0) THEN
          CALL UTL_COMBINESQMATRIX(WTMATALL1,PRIWTMAT,WTMATALL)
          CALL TYP_DEALLOC(WTMATALL1)
        ELSE
          CALL UTL_COMBINESQMATRIX(WTMATDEP,PRIWTMAT,WTMATALL)
        ENDIF
        CALL TYP_DEALLOC(PRIWTMAT)
      ELSE
        WTMATALL = WTMATALL1
      ENDIF
    ELSE
      ! NO PRIOR, both matrices are the same
      WTMATALL = WTMATDEP
    ENDIF
    WRITE (IOUT,535)
    CALL PRTOTX(NTOT,NPE,IOUT,OBSNAM,PARNAM,XSENS)
    ! CONVERT LOG TRANSFORMED PARAMETERS
    WRITE (IOUT,620)
    WRITE (IOUT,625) (I,PARNAM(I),LN(I),I=1,NPE)
    DO IP = 1, NPE
      IF (LN(IP) .NE. 0) THEN
        BOPT(IP) = DLOG(BOPT(IP))
      ENDIF
    ENDDO
    !
    !**COMPUTE C=X'*WGT*X AND FACTOR IT (THE COEFFICIENT MATRIX)
    CALL SETC(NPE,NTOT,XSENS,WTMATALL,C,G,SCLE)
    !
    !**READ DATA FOR EACH SAMPLE AND COMPUTE MODIFIED BEALE'S MEASURE FOR
    !**TOTAL NONLINEARITY, BNT, AND INTRINSIC NONLINEARITY, BNI
    SQT=0.
    SQI=0.
    SQL=0.
    SQLB=0.
    DO M = 1, NPTS
      ! GET A PARAMETER SET IN NATIVE SPACE
      DO J=1,NPS
        PVAL(J) = PVALS1(J,M) ! parameters from IUB1
      ENDDO
      ! PUT ESTIMATED PARAMETERS in B
      DO I=1,NPE
        B(I) = PVAL(IPTR(I))
      ENDDO
      WRITE (IOUT,525) M
      CALL PRTOTB12(NPE,IOUT,B,PARNAM)
      !   Calculate prior values for current parameter set
      DO J=1,MPR
        CALL EQN_EVALUATE(IFAIL,J,NPS,PARNAMALLLC,PVAL,ITYPE,PRIVALSET(J), &
                          EQNLVAL)
        IF (IFAIL .NE. 0) THEN
          CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
          CALL UTL_STOP('EQN_EVALUATE failed evaluating a prior equation')
        ENDIF
      ENDDO
      ! Transform as necessary
      DO I = 1, NPE
        IF (LN(I) .NE. 0) THEN
          B(I) = DLOG(B(I))
        ENDIF
      ENDDO
      ! GET VALUES SIMULATED WITH THIS PARAMETER SET
      DO I=1,NOBS
        FC(I) = MVALS2(I,M)   ! sim equivs from IUB2
      ENDDO
      WRITE (IOUT,530) M
      CALL PRTOTB20R(NOBS,NTOT,IOUT,FC,OBSNAM)
    !
    !**      COMPUTE G=X'*WGT*(F-(FOPT+X*(B-BOPT))) AND SOLVE FOR PARAMETER
    !        CORRECTIONS SO AS OUTPUT G=C*G=PSI, F=F-(FOPT+X*(B-BOPT))
      CALL SETG(NPE,NOBS,NTOT,B,BOPT,FOPT,GP,GP,0,SCLE,XSENS,VYP, &
                WTMATDEP,XSENS(1,1),C,DG,G,FC,G,GPD)
    !
      SQT=SQT+SUMQT(NOBS,FC,WTMATDEP)
      SQI=SQI+SUMQI(NPE,NTOT,FC,G,WTMATALL,XSENS)
      IF (CR.LT.1.E-10) THEN
        SQL=SQL+SUMQL(NPE,NTOT,B,BOPT,WTMATALL,XSENS)**2
        SQLB=SQLB+SUMQLB(NPE,NTOT,B,BOPT,G,WTMATALL,XSENS)**2
      ENDIF
    ENDDO
    !     END LOOP FOR DATA SETS; CALCULATE AND WRITE RESULTS
    IF (CR.LT.1.E-10) THEN
      SCL=NPE*VAR
      SCL=SQL/SCL
      BNT=SQT*SCL/SQL
      BNI=SQI*SCL/SQLB
    ELSE
      SCL=2.*NPE*NPE*CR*VAR
      BNT=SQT/SCL
      BNI=SQI/SCL
    END IF
    DO M = 2, NPTS, 2
      DO I=1,NOBS
        IF(MVALS2(I,M) .NE. MVALS2(I,M-1)) THEN
          EXIT
        ELSEIF(I == NOBS) THEN
          IDENTICAL = .TRUE.
          MESSAGE = &
          'WARNING: Identical simulated equivalents, for parameter sets: '
          WRITE(*,100)
          WRITE(*,150)TRIM(MESSAGE),M-1,M
          WRITE(*,101)
          WRITE(IOUT,100)
          WRITE(IOUT,150)TRIM(MESSAGE),M-1,M
          WRITE(IOUT,101)
        ENDIF
      ENDDO
      IF(M == NPTS .AND. IDENTICAL) THEN
          MESSAGE = &
          ' Identical simulated equivalents, Linearity Measure may be incorrect'
          WRITE(IOUT,100)
          WRITE(IOUT,*)TRIM(MESSAGE)
          WRITE(*,100)
          WRITE(*,*)TRIM(MESSAGE)
          MESSAGE = &
          ' Did process model successfully converge for all parameter sets?'
          WRITE(IOUT,*)TRIM(MESSAGE)
          WRITE(*,*)TRIM(MESSAGE)
          WRITE(IOUT,101)
          WRITE(*,101)
      ENDIF
    ENDDO
    WRITE(IOUT,710) BNT,BNI
    IF (NOINT.LT.1) GO TO 999
    !
    !***** NONLINEARITY MEASURES FOR CONFIDENCE OR PREDICTION INTERVALS
    IF (ITYP.EQ.2) THEN
      WRITE(IOUT,720) 'PREDICTION'
    ELSE
      WRITE(IOUT,720) 'CONFIDENCE'
    END IF
    WRITE (IOUT,730) NOINT, NPI, ITYP
    ! WRITE NAMES AND VALUES
    WRITE (IOUT,521)
    CALL PRTOTB20(NOINT,IOUT,GOPT,GNAM)
    DO I=NPREDI+1,NOINT
      DO J=1,NPS
        IF(UTL_SAMENAME(GNAM(I),PARNAMALLLC(J))) THEN
          IF(LOGT(J) > 0) THEN
            GOPT(I) = DLOG(GOPT(I))
            GOPTLN(I) = 1
            EXIT
          ENDIF
        ENDIF
      ENDDO
    ENDDO
    WRITE(IOUT,556)
    CALL PRTOTB20(NOINT,IOUT,WG,GNAM)
    WRITE (IOUT,535)
    CALL PRTOTX(NOINT,NPE,IOUT,GNAM,PARNAM,Z)
    WRITE(IOUT,'(/,A)') ' CORRECTION FACTORS'
    CALL PRTOTB20(NOINT,IOUT,CC,GNAM)
    !
    !**READ DATA FOR EACH INTERVAL AND COMPUTE COOLEY MEASURES FOR
    !**COMBINED INTRINSIC NONLINEARITY
    !
    DO I=1,NOINT
      SQCI(I)=0.D0
      SQF0(I)=0.D0
      SQG0(I)=0.D0
      SQCIB(I)=0.D0
      SQCIMX(I)=0.D0
    ENDDO
    ICNT = 0
    DO NO=1,NOINT
      WRITE(IOUT,'(//,A,I3)') ' *** INTERVAL NO. ',NO
      ! COMPUTE D=CZ=(X'*WGT*X)^-1*Z
      DO IP=1,NPE
        D(IP)=Z(IP,NO)/SCLE(IP)
      ENDDO
      CALL SOLVE(NPE,D,0,C,D,DET,ISTOP)
      ! COMPUTE VG=Q'Q AND VYP=Q'Q+WG^-1
      VG=0.
      DO IP=1,NPE
        D(IP)=D(IP)/SCLE(IP)
        VG=VG+D(IP)*Z(IP,NO)
      ENDDO
      VP=0.
      IF (ITYP.EQ.2.AND.WG(NO).GT.0) VP=1./WG(NO)
      VYP=VG+VP
      !
      DO ISET=1,2
      ! GET A PARAMETER SET
        ICNT = ICNT + 1
        DO J=1,NPE
          B(J) = PVALS3(IPTR(J),ICNT)  ! parameters from IUB3
        ENDDO
        WRITE (IOUT,525) ISET
        CALL PRTOTB12(NPE,IOUT,B,PARNAM)
        DO I = 1, NPE
          IF (LN(I).NE.0) B(I) = DLOG(B(I))
        ENDDO
        ! GET VALUES SIMULATED WITH THIS _b3 PARAMETER SET
        ! GP IS VALUE PREDICTED WITH _b3 PARAMETER VALUES
        DO I=1,NOBS
          FC(I) = MVALS4(I,ICNT)  ! sim equivs from IUB4
        ENDDO
        GP = MVALS4(NOBS+1,ICNT) ! sim equiv from IUB4
        IF(GOPTLN(NO) > 0)GP = DLOG(GP)
        WRITE (IOUT,530) ISET
        CALL PRTOTB20R(NOBS,NTOT,IOUT,FC,OBSNAM)
        WRITE(IOUT,'(//,A,1X,G11.5)') ' PREDICTED VALUE, GP=',GP
        !
        !** COMPUTE G=C*G, F=F-(FOPT+X*(B-BOPT))
        CALL SETG(NPE,NOBS,NTOT,B,BOPT,FOPT,GOPT(NO),GP,1,SCLE,XSENS,VYP,&
                  WTMATDEP,Z(1,NO),C,DG,D,FC,G,GPD)
        UP=VP*DG
        QI=SUMQI(NPE,NTOT,FC,G,WTMATALL,XSENS)
        CI=SUMCI(NPE,NTOT,D,FC,G,WTMATALL,XSENS)
        DG=UP-VP*GPD/VYP
        SQCI(NO)=SQCI(NO)+QI-GPD*(CI+CI-GPD)/VYP+DG*WG(NO)*DG
        SQF0(NO)=SQF0(NO)+GPD*GPD/VYP
        SQG0(NO)=SQG0(NO)+QI+UP*WG(NO)*UP
        IF (CC(NO).LT.1.E-10) THEN
           QI=SUMQLB(NPE,NTOT,B,BOPT,G,WTMATALL,XSENS)
           CI=SUMCIB(NPE,NTOT,B,BOPT,D,G,WTMATALL,XSENS)
           SQCIB(NO)=SQCIB(NO)+ &
                     (QI-GPD*(CI+CI-GPD)/VYP+DG*WG(NO)*DG)**2
        ENDIF
      ENDDO
    ENDDO
    ! END LOOP FOR INTERVALS; CALCULATE AND WRITE RESULTS
    DO NO=1,NOINT
      IF (CC(NO).LT.1.E-10) THEN
         SCL=NPE*VAR
         SCL=SQL/SCL
         BNT=SQT*SCL/SQL
         BNI=SQI*SCL/SQLB
         SQCI(NO)=SQCI(NO)*SCL/SQCIB(NO)
         SQF0(NO)=SQF0(NO)*SCL/SQCIB(NO)
         SQG0(NO)=SQG0(NO)*SCL/SQCIB(NO)
         SQCIMX(NO)=MAX(SQCI(NO)+2.*SQF0(NO),ABS(SQCI(NO)-2.*SQG0(NO)))
      ELSE
         SCL=2.*VAR*CC(NO)
         SQCI(NO)=SQCI(NO)/SCL
         SQF0(NO)=SQF0(NO)/SCL
         SQG0(NO)=SQG0(NO)/SCL
         SQCIMX(NO)=MAX(SQCI(NO)+2.*SQF0(NO),ABS(SQCI(NO)-2.*SQG0(NO)))
      ENDIF
    ENDDO
    MESSAGE = &
    ' Some Identical simulated equivalents, Linearity Measure may be incorrect'
    IF(MESSAGE .NE. ' ')WRITE(IOUT,103)MESSAGE
    WRITE(IOUT,740)
    CALL PRTOTB20(NOINT,IOUT,SQCI,GNAM)
    IF(MESSAGE .NE. ' ')WRITE(IOUT,103)MESSAGE
    WRITE(IOUT,750)
    CALL PRTOTB20(NOINT,IOUT,SQF0,GNAM)
    IF(MESSAGE .NE. ' ')WRITE(IOUT,103)MESSAGE
    WRITE(IOUT,760)
    CALL PRTOTB20(NOINT,IOUT,SQG0,GNAM)
    IF(MESSAGE .NE. ' ')WRITE(IOUT,103)MESSAGE
    WRITE(IOUT,770)
    CALL PRTOTB20(NOINT,IOUT,SQCIMX,GNAM)
  ENDDO
  AMESSAGE = 'ALL AVAILABLE CORRECTION FACTOR FILES HAVE BEEN IDENTIFIED'
  WRITE(*,*)
  WRITE(*,*)TRIM(AMESSAGE)
  CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','yes')
  AMESSAGE = 'ANALYSIS IS COMPLETE'
  WRITE(*,*)
  WRITE(*,*)TRIM(AMESSAGE)
  CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','yes')
  CALL TYP_DEALLOC(WTMATDEP)
  WRITE(*,900)VERSION
  WRITE(IOUT,900)VERSION
  CALL UTL_ENDTIME(IBDT,IOUT)
  999 CLOSE(IOUT)
  CLOSE(IOUTA)
  STOP
  END PROGRAM MODEL_LINEARITY_ADV
!===============================================================================
!===============================================================================
  SUBROUTINE PRTOTB12(NO,IOUT,VAL,VID)
  !**PRINT VALUES IN TWO GROUPS OF THREE COLUMNS
    IMPLICIT NONE
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
!===============================================================================
!===============================================================================
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
  SUBROUTINE PRTOTB20R(NOR,NO,IUOUT,VAL,VID)
    !**PRINT VALUES IN TWO GROUPS OF THREE COLUMNS
    USE GLOBAL_DATA, ONLY: LENDNAM
    IMPLICIT NONE
    ! Argument variables
    INTEGER,                  INTENT(IN) :: NOR
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
    NR = NOR/2
    IF (2*NR .NE. NOR) NR = NR + 1
    DO K = 1, NR
      WRITE (IUOUT,500) (L,VID(L),VAL(L),L=K,NOR,NR)
    ENDDO
    RETURN
  END SUBROUTINE PRTOTB20R
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
    500 FORMAT (/,2X,'PARAMETER',4X,3(A20,1X))
    505 FORMAT (1X,A12,1X,3(G20.8,1X))
    510 FORMAT (1X,12('-'),1X,3(A20,1X))
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
  SUBROUTINE SOLVE(NPE,G,IND,C,D,DET,ISTOP)
    !-----VERSION 1001 25FEB2003 (SC)
    !     ******************************************************************
    !         SOLVE SYSTEM OF LINEAR EQUATIONS BY LU-DECOMPOSITION
    !     ******************************************************************
    !        SPECIFICATIONS:
    !     ------------------------------------------------------------------
    IMPLICIT NONE
    ! Argument variables
    INTEGER,          INTENT(IN)    :: NPE
    DOUBLE PRECISION, INTENT(IN)    :: G(NPE)
    INTEGER,          INTENT(IN)    :: IND
    DOUBLE PRECISION, INTENT(INOUT) :: C(NPE,NPE)
    DOUBLE PRECISION, INTENT(INOUT) :: D(NPE)
    DOUBLE PRECISION, INTENT(INOUT) :: DET
    INTEGER,          INTENT(INOUT) :: ISTOP
    ! local variables
    DOUBLE PRECISION                :: DPIV
    DOUBLE PRECISION                :: DSUM
    DOUBLE PRECISION                :: DTMPA
    INTEGER                         :: I
    INTEGER                         :: IP1
    INTEGER                         :: J
    INTEGER                         :: K
    INTEGER                         :: KP1
    INTEGER                         :: NM1
    ! ------------------------------------------------------------------
    ! COMPUTE TRIAL PARAMETER STEP LENGTHS USING LDU FACTORIZATION:
    ! DECOMPOSE MATRIX
    NM1=NPE-1
    ISTOP=0
    !
    !-----------LDU FACTORIZATION
    IF (IND.GT.0) THEN
      DET=1.
      DO K=1,NM1
        DPIV=C(K,K)
        DET=DET*DPIV
        IF (DPIV.LT.1.D-13) THEN
          ISTOP=1
          RETURN
        ENDIF
        DPIV=1.D00/DPIV
        KP1=K+1
        DO J=KP1,NPE
          DTMPA=C(J,K)*DPIV
          DO I=J,NPE
            C(I,J)=C(I,J)-DTMPA*C(I,K)
          ENDDO
        ENDDO
        C(K,K)=DPIV
      ENDDO
      DET=DET*C(NPE,NPE)
      IF(C(NPE,NPE).LT.1.D-13) THEN
        ISTOP=1
        RETURN
      ENDIF
    ENDIF
    !
    !-----------INITIALIZE D
    D = G
    !-----------FORWARD SUBSTITUTE
    DO K=1,NM1
      DTMPA=D(K)*C(K,K)
      KP1=K+1
      DO J=KP1,NPE
        D(J)=D(J)-C(J,K)*DTMPA
      ENDDO
    ENDDO
    !
    !-----------BACKWARD SUBSTITUTE
    D(NPE)=D(NPE)/C(NPE,NPE)
    DO I=NPE-1,1,-1
      IP1=I+1
      DSUM=0.
      DO J=IP1,NPE
        DSUM=DSUM+C(J,I)*D(J)
      ENDDO
      D(I)=(D(I)-DSUM)*C(I,I)
    ENDDO
    !
    RETURN
  END SUBROUTINE SOLVE
!===============================================================================
!===============================================================================
  SUBROUTINE SETC(NPE,NTOT,X,WTMATALL,C,G,SCLE)
    !
    !    COMPUTE AND FACTOR C (THE COEFFICIENT MATRIX)
    !
    USE DATATYPES
    USE UTILITIES
    IMPLICIT NONE
    ! Argument variables
    INTEGER,                INTENT(IN)    :: NPE
    INTEGER,                INTENT(IN)    :: NTOT
    DOUBLE PRECISION,       INTENT(IN)    :: X(NPE,NTOT)
    TYPE (CDMATRIX),        INTENT(IN)    :: WTMATALL
    DOUBLE PRECISION,       INTENT(INOUT) :: C(NPE,NPE)
    DOUBLE PRECISION,       INTENT(INOUT) :: G(NPE)
    DOUBLE PRECISION,       INTENT(INOUT) :: SCLE(NPE)
    ! local variables
    INTEGER                               :: IP
    INTEGER                               :: I
    INTEGER                               :: ISTOP
    DOUBLE PRECISION                      :: DTMPA
    DOUBLE PRECISION                      :: DET
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: XTW
    !
    ALLOCATE(XTW(NPE,NTOT))
    XTW = 0.D0
    !
    !----------INITIALIZE C
    C = 0.D0
    G = 0.D0
    !
    !----------CALCULATE SENSITIVITY CONTRIBUTIONS TO C
    !
    ! note X is stored in the transposed condition
    CALL UTL_MATMUL_SUB(NPE,NTOT,X,WTMATALL,XTW)
    C = MATMUL(XTW,TRANSPOSE(X))
    !
    !-------SCALE COEFFICIENT MATRIX AND GRADIENT VECTOR
    DO IP=1,NPE
      SCLE(IP)=1.D00
      IF(C(IP,IP).GT.1.D-30) SCLE(IP)=DSQRT(C(IP,IP))
    ENDDO
    IF(NPE.GT.1) THEN
       DO IP=1,NPE-1
          DTMPA=SCLE(IP)
          DO I=IP+1,NPE
             C(I,IP)=C(I,IP)/(SCLE(I)*DTMPA)
             C(IP,I)=C(I,IP)
          ENDDO
          C(IP,IP)=1.D00
        ENDDO
    ENDIF
    C(NPE,NPE)=1.D00
    !
    !-------COMPUTE SCALED DISPLACEMENT COMPONENTS
    CALL SOLVE(NPE,G,1,C,G,DET,ISTOP)
    DEALLOCATE(XTW)
    RETURN
  END SUBROUTINE SETC
!===============================================================================
!===============================================================================
  SUBROUTINE SETG(NPE,NOBS,NTOT,B,BOPT,FOPT,GOPT,GP,IDG,SCLE,X,VYP, &
                  WTMATDEP,Z,C,DG,D,F,G,GPD)
    !
    ! COMPUTE G=X'*WGT*(F-(FOPT+X*(B-BOPT))) AND SOLVE FOR PARAMETER
    ! CORRECTIONS SO AS OUTPUT G=C*G=PSI, F=F-(FOPT+X*(B-BOPT))
    !
    !     COMPUTE G (THE RHS VECTOR) AND SOLVE FOR PARAMETER CORRECTIONS
    !     NOTE THAT ONLY OBSERVATIONS OF MODEL FUNCTIONS (HEADS AND FLOWS)
    !      CONTRIBUTE TO G=X'*WGT*(F-(FOPT+X*(B-BOPT))). EXTRA TERMS ARE
    !      COMPUTED AND ADDED FOR CONFIDENCE OR PREDICTION INTERVAL WHEN
    !      IDG .NE. 0.
    !     SOLVING MODIFIES G SO AS OUTPUT G=C*G, I.E.
    !      G=(X'*WGT*X)^(-1)*X'*WGT*(F-FOPT-X*(B-BOPT)).
    !     ALSO ON OUTPUT, F=F-FOPT-X*(B-BOPT).
    !
    USE DATATYPES
    USE UTILITIES
    IMPLICIT NONE
    ! Argument variables
    INTEGER,                INTENT(IN)    :: NPE
    INTEGER,                INTENT(IN)    :: NOBS
    INTEGER,                INTENT(IN)    :: NTOT
    DOUBLE PRECISION,       INTENT(IN)    :: B(NPE)
    DOUBLE PRECISION,       INTENT(IN)    :: BOPT(NPE)
    DOUBLE PRECISION,       INTENT(IN)    :: FOPT(NTOT)
    DOUBLE PRECISION,       INTENT(IN)    :: GOPT
    DOUBLE PRECISION,       INTENT(IN)    :: GP
    DOUBLE PRECISION,       INTENT(IN)    :: SCLE(NPE)
    DOUBLE PRECISION,       INTENT(IN)    :: X(NPE,NTOT)
    DOUBLE PRECISION,       INTENT(IN)    :: VYP
    TYPE (CDMATRIX),        INTENT(IN)    :: WTMATDEP
    DOUBLE PRECISION,       INTENT(IN)    :: Z(NPE)
    DOUBLE PRECISION,       INTENT(INOUT) :: C(NPE,NPE)
    DOUBLE PRECISION,       INTENT(INOUT) :: DG
    DOUBLE PRECISION,       INTENT(INOUT) :: D(NPE)
    DOUBLE PRECISION,       INTENT(INOUT) :: F(NTOT)
    DOUBLE PRECISION,       INTENT(INOUT) :: G(NPE)
    DOUBLE PRECISION,       INTENT(INOUT) :: GPD
    ! Local variables
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: BDIFF
    DOUBLE PRECISION                            :: DET
    INTEGER                                     :: I
    INTEGER                                     :: IDG
    INTEGER                                     :: IP
    INTEGER                                     :: ISTOP
    INTEGER                                     :: K
    INTEGER                                     :: L
    INTEGER                                     :: N
    DOUBLE PRECISION                            :: TMP
    !
    ALLOCATE(BDIFF(NPE))
    !
    !----------INITIALIZE D, G AND COMPUTE F=F-(FOPT+X*(B-BOPT))
    G = 0.D0
    BDIFF = B - BOPT
    DO N=1,NOBS
      F(N)=F(N)-FOPT(N)
      DO IP=1,NPE
        F(N)=F(N)-X(IP,N)*(BDIFF(IP))
      ENDDO
    ENDDO
    !
    !----------CALCULATE SENSITIVITY CONTRIBUTIONS TO G.
    DO I = 1, NPE
      DO K = 1, NOBS
        TMP = 0.D0
        IF (UTL_GETVAL(WTMATDEP,K,K) > 0.D0) THEN
          DO L = 1, NOBS
            IF (UTL_GETVAL(WTMATDEP,L,L) > 0.0) &
               TMP=TMP+UTL_GETVAL(WTMATDEP,K,L)*F(L)
          ENDDO
        ENDIF
        G(I) = G(I) + X(I,K)*TMP
      ENDDO
    ENDDO
    !
    !     COMPUTE AND ADD EXTRA TERMS FOR CONFIDENCE OR PREDICTION INTERVALS
    IF (IDG.NE.0) THEN
      GPD = GP - GOPT
      DG = 0.D0
      DO IP=1,NPE
        GPD = GPD - Z(IP) * (BDIFF(IP))
        DG = D(IP) * G(IP)
      ENDDO
      DG = DG / VYP
      DO IP=1,NPE
        G(IP)=G(IP)-DG*Z(IP)
      ENDDO
    ENDIF
    !
    !----------SOLVE FOR PARAMETER CORRECTIONS
    G = G / SCLE
    CALL SOLVE(NPE,G,0,C,G,DET,ISTOP)
    G = G / SCLE
    !
    DEALLOCATE(BDIFF)
    RETURN
  END SUBROUTINE SETG
!===============================================================================
!===============================================================================
  DOUBLE PRECISION FUNCTION SUMCI(NPE,NTOT,D,F,G,WTMATALL,X)
    !     COMPUTE CI=D*X'*WGT*(F-X*PSI) WHERE D=Z*(X'*WGT*X)^-1
    USE DATATYPES
    USE UTILITIES
    IMPLICIT NONE
    ! Argument variables
    INTEGER,                INTENT(IN)    :: NPE
    INTEGER,                INTENT(IN)    :: NTOT
    DOUBLE PRECISION,       INTENT(IN)    :: D(NPE)
    DOUBLE PRECISION,       INTENT(IN)    :: F(NTOT)
    DOUBLE PRECISION,       INTENT(IN)    :: G(NPE)
    TYPE (CDMATRIX),        INTENT(IN)    :: WTMATALL
    DOUBLE PRECISION,       INTENT(IN)    :: X(NPE,NTOT)
    ! local variables
    DOUBLE PRECISION                      :: FD
    INTEGER                               :: IP
    INTEGER                               :: K
    INTEGER                               :: L
    DOUBLE PRECISION                      :: W
    DOUBLE PRECISION                      :: XD
    !
    SUMCI=0.
    !
    DO K=1,NTOT
      IF (UTL_GETVAL(WTMATALL,K,K) > 0.D0) THEN
        W=0.D0
        DO L=1,NTOT
          IF (UTL_GETVAL(WTMATALL,L,L) > 0.D0) THEN
            FD=0.D0
            DO IP=1,NPE
              FD=FD+X(IP,L)*G(IP)
            ENDDO
            FD=F(L)-FD
            W = W + UTL_GETVAL(WTMATALL,K,L) * FD
          END IF
        ENDDO
        XD=0.
        DO IP=1,NPE
          XD=XD+X(IP,K)*D(IP)
        ENDDO
        SUMCI=SUMCI+W*XD
      ENDIF
    ENDDO
    !
  END FUNCTION SUMCI
!===============================================================================
!===============================================================================
  DOUBLE PRECISION FUNCTION SUMCIB(NPE,NTOT,B,BOPT,D,G,WTMATALL,X)
    !
    !     COMPUTE CIB=D*X'*WGT*(FOPT-FOPT-X*PSI) WHERE D=Z*(X'*WGT*X)^-1
    !
    USE DATATYPES
    USE UTILITIES
    IMPLICIT NONE
    ! Argument variables
    INTEGER,                INTENT(IN)    :: NPE
    INTEGER,                INTENT(IN)    :: NTOT
    DOUBLE PRECISION,       INTENT(IN)    :: B(NPE)
    DOUBLE PRECISION,       INTENT(IN)    :: BOPT(NPE)
    DOUBLE PRECISION,       INTENT(IN)    :: D(NPE)
    DOUBLE PRECISION,       INTENT(IN)    :: G(NPE)
    TYPE (CDMATRIX),        INTENT(IN)    :: WTMATALL
    DOUBLE PRECISION,       INTENT(IN)    :: X(NPE,NTOT)
    ! local variables
    DOUBLE PRECISION                      :: FD
    INTEGER                               :: IP
    INTEGER                               :: K
    INTEGER                               :: L
    DOUBLE PRECISION                      :: W
    DOUBLE PRECISION                      :: XD
    !
    SUMCIB=0.
    !
    DO K=1,NTOT
      IF (UTL_GETVAL(WTMATALL,K,K) > 0.D0) THEN
        W=0.D0
        DO L=1,NTOT
          IF (UTL_GETVAL(WTMATALL,L,L) > 0.D0) THEN
            FD=0.D0
            DO IP=1,NPE
              FD=FD+X(IP,L)*(G(IP)+B(IP)-BOPT(IP))
            ENDDO
            W = W + UTL_GETVAL(WTMATALL,K,L) * FD
          END IF
        ENDDO
        XD=0.
        DO IP=1,NPE
          XD=XD+X(IP,K)*D(IP)
        ENDDO
        SUMCIB=SUMCIB-W*XD
      ENDIF
    ENDDO
  !
  END FUNCTION SUMCIB
!===============================================================================
!===============================================================================
  DOUBLE PRECISION FUNCTION SUMQI(NPE,NTOT,F,G,WTMATALL,X)
    !
    !     COMPUTE QI=(F-FOPT-X*PSI)'*WGT*(F-X*PSI)+(XD*PSI)'*WGTD*(XD*PSI)
    !       WHERE PSI=(X'*WGT*X)^(-1)*X'*WGT*(F-FOPT)=G+X*(B-BOPT);
    !       (PLUS SOME MORE FOR INTERVALS)
    !     NOTE AS INPUT F HAS BEEN CHANGED SO F=F-FOPT-X*(B-BOPT), AND
    !       G=(X'*WGT*X)^(-1)*X'*WGT*(F-FOPT-X*(B-BOPT)).
    !
    USE DATATYPES
    USE UTILITIES
    IMPLICIT NONE
    ! Argument variables
    INTEGER,                INTENT(IN)    :: NPE
    INTEGER,                INTENT(IN)    :: NTOT
    DOUBLE PRECISION,       INTENT(IN)    :: F(NTOT)
    DOUBLE PRECISION,       INTENT(IN)    :: G(NPE)
    TYPE (CDMATRIX),        INTENT(IN)    :: WTMATALL
    DOUBLE PRECISION,       INTENT(IN)    :: X(NPE,NTOT)
    ! local variables
    DOUBLE PRECISION                      :: FD
    INTEGER                               :: IP
    INTEGER                               :: K
    INTEGER                               :: L
    DOUBLE PRECISION                      :: W
    !
    SUMQI=0.D0
    !
    DO K=1,NTOT
      IF (UTL_GETVAL(WTMATALL,K,K) > 0.D0) THEN
        W=0.D0
        DO L=1,NTOT
          IF (UTL_GETVAL(WTMATALL,L,L) > 0.D0) THEN
            FD=0.D0
            DO IP=1,NPE
              FD=FD+X(IP,L)*G(IP)
            ENDDO
            FD=F(L)-FD
            W = W + UTL_GETVAL(WTMATALL,K,L) * FD
          END IF
        ENDDO
        FD=0.
        DO IP=1,NPE
          FD=FD+X(IP,K)*G(IP)
        ENDDO
        FD=F(K)-FD
        SUMQI=SUMQI+W*FD
      ENDIF
    ENDDO
    !
  END FUNCTION SUMQI
!===============================================================================
!===============================================================================
  DOUBLE PRECISION FUNCTION SUMQL(NPE,NTOT,B,BOPT,WTMATALL,X)
    !
    !     COMPUTE QL=F'*WGT*F WHERE F=FOPT-FOPT-X*(B-BOPT).
    !
    USE DATATYPES
    USE UTILITIES
    IMPLICIT NONE
    ! Argument variables
    INTEGER,                INTENT(IN)    :: NPE
    INTEGER,                INTENT(IN)    :: NTOT
    DOUBLE PRECISION,       INTENT(IN)    :: B(NPE)
    DOUBLE PRECISION,       INTENT(IN)    :: BOPT(NPE)
    TYPE (CDMATRIX),        INTENT(IN)    :: WTMATALL
    DOUBLE PRECISION,       INTENT(IN)    :: X(NPE,NTOT)
    DOUBLE PRECISION                      :: BDIFF(NPE)
    ! local variables
    DOUBLE PRECISION                      :: FL
    INTEGER                               :: IP
    INTEGER                               :: K
    INTEGER                               :: L
    INTEGER                               :: N
    DOUBLE PRECISION                      :: W
    !
    SUMQL=0.
    BDIFF = B - BOPT
    !
    DO K=1,NTOT
      IF (UTL_GETVAL(WTMATALL,K,K) > 0.D0) THEN
        W = 0.D0
        DO L=1,NTOT
          IF (UTL_GETVAL(WTMATALL,L,L) > 0.D0) THEN
            FL=0.D0
            DO IP=1,NPE
              FL=FL+X(IP,L)*(BDIFF(IP))
            ENDDO
            W = W + UTL_GETVAL(WTMATALL,K,L) * FL
          ENDIF
        ENDDO
        N = K
        FL=0.
        DO IP=1,NPE
          FL=FL+X(IP,N)*(BDIFF(IP))
        ENDDO
        SUMQL=SUMQL+W*FL
      ENDIF
    ENDDO
    !
  END FUNCTION SUMQL
!===============================================================================
!===============================================================================
  DOUBLE PRECISION FUNCTION SUMQLB(NPE,NTOT,B,BOPT,G,WTMATALL,X)
    !
    !     COMPUTE QLB=(FOPT-FOPT-X*PSI)'*WGTM*(FOPT-FOPT-X*PSI)+
    !                                           (XD*PSI)'*WGTD*(XD *PSI)
    !       WHERE PSI=(X'*WGT*X)^(-1)*X'*WGT*(F-FOPT)=G+X*(B-BOPT);
    !                                           (PLUS SOME MORE FOR INTERVALS)
    !       NOTE G=(X'*WGT*X)^(-1)*X'*WGT*(F-FOPT-X*(B-BOPT)).
    !
    USE DATATYPES
    USE UTILITIES
    IMPLICIT NONE
    ! Argument variables
    INTEGER,                INTENT(IN)    :: NPE
    INTEGER,                INTENT(IN)    :: NTOT
    DOUBLE PRECISION,       INTENT(IN)    :: B(NPE)
    DOUBLE PRECISION,       INTENT(IN)    :: BOPT(NPE)
    DOUBLE PRECISION,       INTENT(IN)    :: G(NPE)
    TYPE (CDMATRIX),        INTENT(IN)    :: WTMATALL
    DOUBLE PRECISION,       INTENT(IN)    :: X(NPE,NTOT)
    ! Local variables
    DOUBLE PRECISION                            :: FD
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: GMBDIFF
    INTEGER                                     :: IP
    INTEGER                                     :: K
    INTEGER                                     :: L
    INTEGER                                     :: N
    DOUBLE PRECISION                            :: W
    !
    ALLOCATE(GMBDIFF(NPE))
    !
    SUMQLB=0.
    GMBDIFF = G + B - BOPT
    !
    DO K=1,NTOT
      IF (UTL_GETVAL(WTMATALL,K,K) > 0.D0) THEN
        W = 0.D0
        DO L=1,NTOT
          IF (UTL_GETVAL(WTMATALL,L,L) > 0.D0) THEN
            FD=0.D0
            DO IP=1,NPE
              FD=FD+X(IP,L)*(GMBDIFF(IP))
            ENDDO
            W = W + UTL_GETVAL(WTMATALL,K,L) * FD
          ENDIF
        ENDDO
        N = K
        FD=0.
        DO IP=1,NPE
          FD=FD+X(IP,N)*(GMBDIFF(IP))
        ENDDO
        SUMQLB=SUMQLB+W*FD
      ENDIF
    ENDDO
    DEALLOCATE(GMBDIFF)
    !
  END FUNCTION SUMQLB
!===============================================================================
!===============================================================================
  DOUBLE PRECISION FUNCTION SUMQT(NTOT,F,WTMATDEP)
    !
    !     COMPUTE QT=F'*WGT*F WHERE F=F-FOPT-X*(B-BOPT).
    USE DATATYPES
    USE UTILITIES
    IMPLICIT NONE
    ! Argument variables
    INTEGER,                INTENT(IN)    :: NTOT
    DOUBLE PRECISION,       INTENT(IN)    :: F(NTOT)
    TYPE (CDMATRIX),        INTENT(IN)    :: WTMATDEP
    !
    ! local variables
    INTEGER                               :: K
    INTEGER                               :: L
    DOUBLE PRECISION                      :: W
    !
    SUMQT=0.
    !
    ! OMIT PRIOR NOTE PRIOR IS ALWAYS AT THE END OF THE F AND WT ARRAYS
    DO K=1,NTOT
      IF(UTL_GETVAL(WTMATDEP,K,K) > 0.D0) THEN
        W = 0.D0
        DO L=1,NTOT
          IF(UTL_GETVAL(WTMATDEP,L,L) > 0.D0)W=W+UTL_GETVAL(WTMATDEP,K,L)*F(L)
        ENDDO
        SUMQT = SUMQT + W * F(K)
      ENDIF
    ENDDO
  END FUNCTION SUMQT
!===============================================================================
!===============================================================================
