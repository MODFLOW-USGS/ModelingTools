PROGRAM CORFAC_PLUS
! Jupiter API modules
USE BASIC
USE GLOBAL_DATA, ONLY: AMESSAGE, IVERB, LENDNAM, MAX_STRING_LEN, NPPREC, VERSIONID
USE DATATYPES
USE DEPENDENTS, ONLY: DEP_UEV_DX_READ_PNUM
USE SENSITIVITY, ONLY: SEN_UEV_DX_READ_SU
USE STATISTICS, ONLY: STA_UEV_DX_READ_DM
USE UTILITIES
USE UTLUCODE
  !     ******************************************************************
  !       CALCULATES CORRECTION FACTORS AND APPROXIMATE CORRECTION FACTORS
  !        FOR CONFIDENCE REGION, CONFIDENCE AND PREDICTION INTERVALS
  !
  !       PROGRAM BY R. L. COOLEY, USGS, DENVER, COLO.
  !       MODIFIED FOR MODFLOW-2000 BY STEEN CHRISTENSEN, DEPT. OF EARTH
  !         SCIENCES, UNIVERSITY OF AARHUS, DENMARK.
  !       USES PIECES FROM BEALE2K BY E.R. BANTA 8/12/1999
  !     MODIFIED TO ACCOMODATE JUPITER API DATA EXCHANGE FILES & UCODE
  !       BY EILEEN POETER, IGWMC, COLORADO SCHOOL OF MINES, GOLDEN CO USA
  !       JULY 2005
  !     ******************************************************************
  !     SPECIFICATIONS:
  !     ------------------------------------------------------------------
  IMPLICIT NONE
  DOUBLE PRECISION                                  :: A
  DOUBLE PRECISION                                  :: B
  CHARACTER(LEN=3)                                  :: C1 = ' NO'
  CHARACTER(LEN=3)                                  :: C2 = ' NO'
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)       :: CCI
  DOUBLE PRECISION                                  :: CCIB
  DOUBLE PRECISION                                  :: CCR
  DOUBLE PRECISION                                  :: CCB1 = 0.D0
  DOUBLE PRECISION                                  :: CCRIC
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)       :: CCWRITE
  CHARACTER(LEN=200)                                :: CDUM
  INTEGER                                           :: CFCOL !cfrow = ityp
  DOUBLE PRECISION                                  :: CFSM
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)     :: CF
  CHARACTER(LEN=10)                                 :: CHDATE
  CHARACTER(LEN=200)                                :: CHECK
  CHARACTER(LEN=10)                                 :: CHTIME
  CHARACTER(LEN=10)                                 :: CHZONE
  CHARACTER(LEN=13)                                 :: COLNAM(7)
  CHARACTER(LEN=20)                                 :: COL12NAM(2)
  CHARACTER(LEN=3)                                  :: CONVERGE
  DOUBLE PRECISION                                  :: CORRELATION
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)     :: COV
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)     :: COV1
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)     :: COV2
  DOUBLE PRECISION                                  :: COVP
  DOUBLE PRECISION                                  :: CPI
  DOUBLE PRECISION                                  :: CPIB
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)     :: CPTRUOFF2M
  DOUBLE PRECISION                                  :: CRB
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)     :: DARRAY
  DOUBLE PRECISION                                  :: DF
  DOUBLE PRECISION                                  :: DFB
  DOUBLE PRECISION                                  :: EC
  INTEGER, ALLOCATABLE, DIMENSION(:)                :: EPTR
  CHARACTER(LEN=10)                                 :: EXTB1ADV
  CHARACTER(LEN=7)                                  :: EXTB3
  DOUBLE PRECISION                                  :: FAC
  DOUBLE PRECISION                                  :: FACB
  CHARACTER(LEN=MAX_STRING_LEN)                     :: FN
  CHARACTER(LEN=MAX_STRING_LEN)                     :: FNDMP
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: GNAM
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: GNAMLC
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)       :: GOPT
  INTEGER                                           :: I
  INTEGER                                           :: IBDT(8)
  INTEGER                                           :: IDUM
  INTEGER                                           :: IFAIL
  INTEGER                                           :: IIN1
  INTEGER                                           :: IOUT = 0
  INTEGER                                           :: IP
  INTEGER, ALLOCATABLE, DIMENSION(:)                :: IPTR
  INTEGER                                           :: ISTAT
  INTEGER, ALLOCATABLE, DIMENSION(:)                :: ITEMTYPE
  LOGICAL                                           :: ITRAN
  INTEGER                                           :: ITYP
  INTEGER                                           :: IUCORFACS
  INTEGER                                           :: IUPC
  INTEGER                                           :: IUPR
  LOGICAL                                           :: USEDCOV
  INTEGER                                           :: J
  INTEGER                                           :: K
  INTEGER                                           :: KNT
  LOGICAL                                           :: LEX = .FALSE.
  INTEGER, ALLOCATABLE, DIMENSION(:)                :: LN
  INTEGER                                           :: MPR = 0
  INTEGER                                           :: MXRECL = 0
  INTEGER                                           :: NOBS = 0
  INTEGER                                           :: NUM_NEWOBS = 0
  INTEGER                                           :: NUM_NEWPRI = 0
  INTEGER                                           :: NUM_OLDOBS = 0
  INTEGER                                           :: NUM_OLDPRI = 0
  INTEGER                                           :: NPARI = 0
  INTEGER                                           :: NPRED = 0
  INTEGER                                           :: NPREDI = 0
  INTEGER                                           :: NPS = 0
  INTEGER                                           :: NPSWOP = 0
  INTEGER                                           :: NOINT = 0
  LOGICAL, ALLOCATABLE, DIMENSION(:)                :: NOTESTIMATED
  INTEGER                                           :: NPE = 0
  INTEGER                                           :: NUM_OLDPAR = 0
  INTEGER                                           :: NUM_NEWPAR = 0
  INTEGER                                           :: NPEORIG = 0
  INTEGER                                           :: NPREDGPS = 0
  INTEGER                                           :: NTOT = 0
  INTEGER                                           :: NVARP = 0
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: OBSNAM
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: OBSNAM0
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: OBSNAM1
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: OBSNAM2
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: OBSNAMLC
  LOGICAL, ALLOCATABLE, DIMENSION(:)                :: OMITTED
  CHARACTER(LEN=MAX_STRING_LEN)                     :: OUTNAM
  CHARACTER(LEN=MAX_STRING_LEN)                     :: OUTNAMPRED
  CHARACTER(LEN=MAX_STRING_LEN)                     :: OUTNAMTMP
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)      :: PANAM
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)      :: PARNAM
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)      :: PARNAM1
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)      :: PARNAM2
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)      :: PARNAMLC
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)      :: PARNAMALL
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)      :: PARNAMALLLC
  INTEGER, ALLOCATABLE, DIMENSION(:)                :: PINC
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)      :: PREDGRP
  DOUBLE PRECISION , ALLOCATABLE, DIMENSION(:)      :: PREDVAR
  DOUBLE PRECISION , ALLOCATABLE, DIMENSION(:)      :: PREDVAL
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: PREDNAM
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: PREDNAMLC
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: PRINAM
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: PRINAM1
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: PRINAM2
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)       :: PVAL
  INTEGER, ALLOCATABLE, DIMENSION(:)                :: PLOTSYMBOL
  INTEGER, ALLOCATABLE, DIMENSION(:)                :: PLOTSYMBOLPRI
  INTEGER, ALLOCATABLE, DIMENSION(:)                :: PLOTSYMBOLPRI1
  INTEGER, ALLOCATABLE, DIMENSION(:)                :: PLOTSYMBOLPRI2
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)       :: Q
  DOUBLE PRECISION                                  :: OMEC
  DOUBLE PRECISION                                  :: QQUD
  DOUBLE PRECISION                                  :: QSUM
  DOUBLE PRECISION                                  :: QSMP
  DOUBLE PRECISION                                  :: QSMN
  DOUBLE PRECISION                                  :: RDUM
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)     :: ROWVALS
  CHARACTER (LEN=3)                                 :: SENTYPE
  DOUBLE PRECISION                                  :: SSQM
  DOUBLE PRECISION                                  :: SSQP
  DOUBLE PRECISION                                  :: SUM
  DOUBLE PRECISION                                  :: TPCF
  DOUBLE PRECISION                                  :: TOCF
  DOUBLE PRECISION                                  :: VE
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)     :: VCMPLT
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)       :: VP2M
  DOUBLE PRECISION                                  :: VAR
  CHARACTER(LEN=20)                                 :: VERSION
  CHARACTER(LEN=20)                                 :: VERSIONMIN
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)       :: VARGWG
  TYPE (CDMATRIX)                                   :: WTMAT
  TYPE (CDMATRIX)                                   :: WTMATSQR
  TYPE (CDMATRIX)                                   :: WTMATDEP
  TYPE (CDMATRIX)                                   :: WTMATDEPSQR
  TYPE (CDMATRIX)                                   :: PRIWTMAT
  TYPE (CDMATRIX)                                   :: PRIWTMAT1
  TYPE (CDMATRIX)                                   :: PRIWTMAT2
  TYPE (CDMATRIX)                                   :: PRIWTMATSQR
  TYPE (CDMATRIX)                                   :: PRIWTMATSQR1
  TYPE (CDMATRIX)                                   :: PRIWTMATSQR2
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)     :: XTMP
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)     :: XPRI
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)     :: XPRI1
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)     :: XPRI2
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)     :: XSENS
  DOUBLE PRECISION                                  :: XI
  DOUBLE PRECISION                                  :: XIMX
  DOUBLE PRECISION                                  :: XINB
  DOUBLE PRECISION                                  :: XIPB
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)     :: XX
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)     :: Z
  DOUBLE PRECISION , ALLOCATABLE, DIMENSION(:,:)    :: ZTMP
  PARAMETER (VERSION = '1.007') ! 09/2011
  !**FORMAT LIST
  480 FORMAT (34X,'CORFAC_PLUS',//, &
      24X,'CONSTRUCTED USING THE JUPITER API',/, &
      17X,'UCODE POST-PROCESSING PROGRAM TO CALCULATE',/, &
      1X,'CORRECTION FACTORS FOR CONFIDENCE REGION & CONFIDE', &
          'NCE AND PREDICTION INTERVALS',/, &
      34X,'Version ',A/)
  481 FORMAT (' !!!! ERROR MISSING ROOTFILENAME ON CORFAC_PLUS ', &
              'COMMAND LINE !!!')
  485 FORMAT (//, &
   1X,78('!'),/, &
   5X,'NPRED IS > 0 IN *.corfac, SO THIS EXECUTION OF CORFAC_PLUS REQUIRES',/, &
   5X,'DATA_EXCHANGE FILES GENERATED BY UCODE WHEN IT IS EXECUTED',/, &
   5X,'WITH PREDICTION=yes IN THE UCODE_CONTROL_DATA INPUT BLOCK FOLLOWING',/, &
   5X,'A SUCCESSFUL REGRESSION. THE FILES ARE NOT AVAILABLE SO CORFAC_PLUS',/, &
   5X,'CANNOT PROCEED.',//, &
   5X,'REFER TO THE UCODE USER''S MANUAL FOR DETAILS.',/, &
   1X,78('!'),/)
  500 FORMAT (//,1X,78('*'),/,11X, &
            'WARNING SOME OF THE STATISTICS IN THE DATA-EXCHANGE FILES',/,11X, &
            'WERE GENERATED USING FORWARD DIFFERENCE SENSITIVITIES.',/,11X, &
            'THEY MAY BE INACCURATE AND CAUSE INACCURARCIES IN THE',/,11X, &
            'CALCULATIONS OF THIS PROGRAM.',/,11X, &
            'CONSIDER REGENERATING THE FILES WITH CENTRAL DIFFERENCES',/,11X, &
            'OR EXACT DERIVATIVES.',/,1X,78('*'),//)
  510 FORMAT ( &
      ' CALIBRATION INFORMATION:',//, &
      '   NUMBER OF ESTIMATED PARAMETERS...................:',I5,/, &
      '   NUMBER OF PARAMETERS FOR PREDICTION..............:',I5,/, &
      '   NUMBER OF OBSERVATIONS...........................:',I5,/, &
      '   NUMBER OF PRIOR-INFORMATION EQUATIONS............:',I5,/, &
      '   CALCULATED ERROR VARIANCE........................:',G12.5,/)
  512 FORMAT ( &
      ' CORRECTION FACTOR INFORMATION:',//, &
      '   EFFECTIVE CORRELATION (EC).......................:',G12.5,/, &
      11X,A,/,'   TRANSFORM WITH OBS. COV. MATRIX? ................:',3X,A3,/ &
      11X,/,'   WAS TRUE COV. MATRIX USED FOR REGRESSION? .......:',3X,A3,/)
  535 FORMAT (1X,'CALIBRATION SENSITIVITIES FOR OPTIMUM PARAMETER VALUES',/, &
                 ' CAN BE VIEWED in FILE:',//,2X,A)
  536 FORMAT (1X,'PREDICTION SENSITIVITIES FOR ALL PREDICTIONS',/, &
                 ' AT THE OPTIMUM PARAMETER VALUES',/, &
                 ' CAN BE VIEWED in FILE:',//,2X,A)
  537 FORMAT(1X,'PREDICTION SENSITIVITIES AT THE OPTIMUM PARAMETER VALUES ',/, &
                 ' FOR ITEMS SELECTED FOR ESTIMATION OF',/, &
                 ' NONLINEAR CONFIDENCE INTERVALS:',/)
  556 FORMAT (1X,'ITEMS SELECTED FOR NONLINEAR CONFIDENCE INTERVALS',/,1X, &
                 'AND THEIR MEASUREMENT VARIANCE TO BE USED ',/,1X, &
                 'WHEN NONLINEAR PREDICTION INTERVALS ARE REQUESTED:',//,1X, &
                 2('NO. INTERVAL',12X,'MEAS. VARIANCE '))
  557 FORMAT (1X,'ITEMS SELECTED FOR NONLINEAR CONFIDENCE INTERVALS',/,1X, &
                 'AND THEIR MEASUREMENT VARIANCE TO BE USED ',/,1X, &
                 'WHEN NONLINEAR PREDICTION INTERVALS ARE REQUESTED:',//,1X, &
                 ('NO. INTERVAL',12X,'MEAS. VARIANCE '))
  715 FORMAT(/,' CORRECTION FACTOR COMPUTED FOR CONFIDENCE: REGION')
  720 FORMAT(/,' CORRECTION FACTORS COMPUTED FOR ',A,' INTERVALS:')
  730 FORMAT(/, &
      ' INFORMATION FOR CALCULATION OF INTERVALS:',//, &
      ' TOTAL NUMBER OF INTERVALS ..............................:',I5,/, &
      ' NUMBER OF INTERVALS FOR PARAMETERS .....................:',I5,/, &
      ' INTERVAL TYPE ..........................................: CONFIDENCE')
  731 FORMAT(/, &
      ' INFORMATION FOR CALCULATION OF INTERVALS:',//, &
      ' TOTAL NUMBER OF INTERVALS ..............................:',I5,/, &
      ' NUMBER OF INTERVALS FOR PARAMETERS .....................:',I5,/, &
      ' INTERVAL TYPE ..........................................: PREDICTION')
  799 FORMAT &
      (//,' TRUE COVARIANCE WAS USED FOR WEIGHT MATRIX IN THE REGRESSION',/, &
        /,'     WHEN INDIVIDUAL CONFIDENCE INTERVALS ARE REQUESTED:',/, &
        /,' CORRECTION FACTOR (CR) ----------- = ',G11.5)
  800 FORMAT(//,' CORRECTION FACTOR FOR N-P (A) ---- = ',G11.5, &
      /,' SCALING FACTOR (B) --------------- = ',G11.5, &
      /,' EFFECTIVE CORRELATION (C) -------- = ',G11.5, &
      /,' (N-P)/(N-A*P) VALUE (FAC) -------- = ',G11.5, &
      /,' CORRECTION FACTOR (CR) ----------- = ',G11.5)
  810 FORMAT(//,' APPROXIMATE N-A*P VALUE (DFB) ---- = ',G11.5, &
      /,' APPROX. (N-P)/(N-A*P) VALUE (FACB) = ',G11.5, &
      /,' APPROXIMATE BOUND FOR CR (CRB) --- = ',G11.5)
  820 FORMAT(//,'** ',A,' INTERVAL NO. ',I3,', FOR ', A)
  824 FORMAT(/,'** ',1X, &
             ' PARAMETER WAS OMITTED DURING REGRESSION, BOUNDS ARE IRRELEVANT')
  826 FORMAT(/,'** ',1X, &
       ' PARAMETER WAS NOT ESTIMATED DURING REGRESSION, BOUNDS ARE IRRELEVANT')
  830 FORMAT(/,' CORRECTION FACTOR XI FOR C. I. ------ = ',G11.5, &
      /,' CORRECTION FACTOR (CC) -------------- = ',G11.5)
  840 FORMAT(/,' ESTIMATED VARIANCE OF PREDICTION (VE) = ',G11.5, &
      /,' BOUND FOR XI USING POS. VALUES (XIPB) = ',G11.5, &
      /,' BOUND FOR XI USING NEG. VALUES (XINB) = ',G11.5, &
      /,' APPROXIMATE BOUND FOR CC (CCB) ------ = ',G11.5)
  850 FORMAT(/,' TRUE DIAGONAL PREDICTION SECOND MOMENT (VP) = ',G11.5)
  860 FORMAT(/14X,' TRUE OFF-DIAGONAL PREDICTION SECOND MOMENTS', &
      /,3(3X,'NO.',10X,'CP',6X))
  870 FORMAT(/,' CORRECTION FACTOR XI FOR P. I. ------ = ',G11.5, &
      /,' CORRECTION FACTOR (CP) -------------- = ',G11.5)
  880 FORMAT(/,' ESTIMATED VARIANCE OF PREDICTION (VE) = ',G11.5, &
      /,' BOUND FOR XI USING POS. VALUES (XIPB) = ',G11.5, &
      /,' BOUND FOR XI USING NEG. VALUES (XINB) = ',G11.5, &
      /,' APPROXIMATE BOUND FOR CP (CPB) ------ = ',G11.5)
  900 FORMAT(/,1X,60('*'),/, &
             '  Normal termination of CORFAC_PLUS, Version: ',A, &
             /,1X,60('*'),//)
  901 FORMAT(/,80('_'))
  902 FORMAT(80('='))
  1000 FORMAT(1X,'NO INTERVALS WERE REQUESTED',/, &
              1X,'REVIEW #cf FILE FOR MESSAGES',/, &
              1X,'REVIEW input file as defined on command line for errors',/)
  1001 FORMAT(1X,'INTERVALS TYPE IS NOT DEFINED',/, &
       1X,'INDICATE ConfidenceOrPrediction in CORRECTION_FACTOR_DATA BLOCK',/, &
       1X,'in the input file as defined on command line for errors',/)
  1002 FORMAT(/,1X,'CORRECTION FACTOR CALCULATIONS ARE COMPLETE',/)
  1020 FORMAT(1X,'CORFAC_PLUS requires a *.corfac file,',/, &
                 '  refer to the UCODE User''s Manual for details')
  1030 FORMAT(1X,'This execution of CORFAC_PLUS requires an fn._pc file,',/, &
                 '  execute a successful optimization before CORFAC_PLUS')
  1039 FORMAT(/,1X,'WEIGHT MATRIX FOR OBSERVATIONS WILL BE READ FROM ', &
                 'FILE: ',/,20X,A)
  1040 FORMAT(/,1X,'                                                 ', &
                 ' AND: ',/,20X,A)
  1059 FORMAT(/,1X,'PARAMETER VARIANCE-COVARIANCE MATRIX WAS ', &
                   'READ FROM FILE: ',/,20X,A)
!===============================================================================
!===============================================================================
  CALL DATE_AND_TIME(CHDATE,CHTIME,CHZONE,IBDT)
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
  !
  COLNAM(1) = 'INTERVAL NAME'
  COLNAM(2) = 'PRED=1 PAR=2'
  COLNAM(3) = 'PRED VALUE'
  COLNAM(4) = 'MEAS VAR'
  COLNAM(5) = 'CF INDIVID'
  COLNAM(6) = 'CF SIMULT'
  COLNAM(7) = 'CR'
  IFAIL = 0
  EC = 0.D0
  ITRAN = .FALSE.
  USEDCOV = .FALSE.
  CFCOL = 3
  COL12NAM(1) = 'INTERVAL NAME'
  COL12NAM(2) = 'PRED=1 PAR=2'
  !
  !  WRITE PROGRAM NAME AND VERSION TO SCREEN
  WRITE (*,*)
  WRITE (*,480) VERSION
  !
  ! GET ROOT NAME
  OUTNAM = UTL_GETARG(1)
  IF(OUTNAM .EQ. ' ') THEN
    !  MAIN OUTPUT UNIT
    IOUT = UTL_GETUNIT(7,1000)
    OPEN (IOUT,FILE='CORFAC.ERR',STATUS='UNKNOWN')
    WRITE(*,481)
    WRITE(IOUT,481)
    CLOSE(IOUT)
    CALL UTL_STOP &
    (' PLEASE INCLUDE A ROOTFILENAME on the CORFAC_PLUS COMMAND LINE')
  ENDIF
  OUTNAMPRED = UTL_GETARG(2)
  IF(OUTNAMPRED .EQ. ' ') OUTNAMPRED = OUTNAM
  ! READ USER CORFAC INPUT (PART OF UNC FILE FOR MF2K)
  ! USER INPUT TO CORFAC UNIT
  IIN1 = UTL_GETUNIT(7,1000)
  FN = TRIM(OUTNAMPRED)//'.corfac'
  INQUIRE(FILE=FN,EXIST=LEX)
  IF(LEX) THEN
    LEX = .FALSE.
    OPEN (IIN1,FILE=FN,STATUS='UNKNOWN')
    CALL CORFAC_INI1 (IFAIL,IIN1,OUTNAMPRED,VERSION,CFCOL,IOUT,EC,ITRAN, &
                      NPARI,NPREDI,USEDCOV)
    CLOSE(IIN1)
  ELSE
    WRITE(*,1020)
    WRITE(IOUT,1020)
    CLOSE(IOUT)
    CALL UTL_STOP &
    ('CORFAC_PLUS requires file *.corfac, see the UCODE Manual')
  ENDIF
  ! READ ._DM FILE
  CALL UTLUCODE_DX_READ_DM(IFAIL,OUTNAM, &
                           RDUM,RDUM,RDUM,RDUM, &
                           CONVERGE,IDUM,RDUM,RDUM,RDUM, &
                           CDUM,CDUM,CDUM,CDUM, &
                           MPR,NOBS,NPEORIG,NPE,NPS,IDUM, &
                           RDUM,RDUM,RDUM,VAR,IOUT,RDUM,RDUM,SENTYPE)
  IFAIL = 0
  FNDMP = TRIM(OUTNAMPRED)//'._dmp'
  INQUIRE(FILE=FNDMP,EXIST=LEX)
  NUM_OLDPRI = MPR
  NUM_OLDOBS = NOBS + MPR
  NTOT = NOBS + MPR
  NUM_OLDPAR = NPE
  NPSWOP = NPS
  IF(LEX) THEN
    IIN1 = UTL_GETUNIT(7,1000)
    OPEN (IIN1,FILE=FNDMP,STATUS='UNKNOWN')
    READ(IIN1,*,END=801)CHECK,NPREDGPS
    READ(IIN1,*,END=801)CHECK,NVARP
    801 CLOSE(UNIT=IIN1)
    NUM_NEWPRI = NVARP-NPE
    MPR = NUM_OLDPRI + NUM_NEWPRI
    NUM_NEWOBS = NUM_NEWPRI
    NUM_NEWPAR = NVARP-NPE
    NPE = NPE + NUM_NEWPAR
    NTOT = NTOT + NUM_NEWPRI
    NPS = NPS + NUM_NEWPAR
  ENDIF
  !
  ! TOTAL # INTERVALS = # CI for PARAMETERS + # CI for PREDICTIONS
  NOINT = NPARI + NPREDI
  IF (NOINT.GT.0) THEN
    !  ALLOCATE MEMORY FOR ARRAYS
    ALLOCATE(CCI(NOINT), CCWRITE(NOINT), GNAM(NOINT), GNAMLC(NOINT), &
             GOPT(NOINT), NOTESTIMATED(NOINT), OMITTED(NOINT), &
             VARGWG(NOINT), VP2M(NOINT), Z(NPE,NOINT))
    ALLOCATE(LN(NPS), PARNAMALL(NPS), PARNAMALLLC(NPS), PINC(NPS), PVAL(NPS))
    ALLOCATE(COV(NPE,NPE), EPTR(NUM_OLDPAR), IPTR(NPE), &
             PANAM(NPEORIG), PARNAM(NPE), PARNAMLC(NPE))
    ALLOCATE(OBSNAM(NTOT), OBSNAM0(NOBS), OBSNAMLC(NTOT), &
             PLOTSYMBOL(NOBS), PLOTSYMBOLPRI(MPR), PRINAM(MPR), Q(NTOT), &
             XPRI(NPE,MPR), XSENS(NPEORIG,NOBS), XX(NPE,NTOT), &
             CF(NOBS,NOBS),DARRAY(NPE,NTOT))
    LN = 0
    IF(MPR > 0) THEN
      XPRI = 0.D0
      IF(NUM_OLDPRI > 0) THEN
        ALLOCATE(OBSNAM1(NUM_OLDPRI),PRINAM1(NUM_OLDPRI), &
                 PLOTSYMBOLPRI1(NUM_OLDPRI),XPRI1(NPE,NUM_OLDPRI))
        OBSNAM1 = ' '
        PLOTSYMBOLPRI1 = 0
        PRINAM1 = ' '
        XPRI1 = 0.D0
      ENDIF
      IF(NUM_NEWPRI > 0) THEN
        ALLOCATE(COV1(NUM_OLDPAR,NUM_OLDPAR),COV2(NUM_NEWPAR,NUM_NEWPAR), &
                OBSNAM2(NUM_NEWOBS), &
                PARNAM1(NUM_OLDPAR),PARNAM2(NUM_NEWPAR), &
                PRINAM2(NUM_NEWPRI),PLOTSYMBOLPRI2(NUM_NEWPRI), &
                XPRI2(NPE,NUM_NEWPRI))
        COV1 = 0.D0
        COV2 = 0.D0
        OBSNAM2 = ' '
        PARNAM1 = ' '
        PARNAM2 = ' '
        PLOTSYMBOLPRI2 = 0
        PRINAM2 = ' '
        XPRI2 = 0.D0
      ENDIF
    ENDIF
    ALLOCATE(VCMPLT(NTOT,NTOT),CPTRUOFF2M(NOINT,NTOT))
    ALLOCATE(ITEMTYPE(NOINT))
    CF = 0.D0
    COV = 0.D0
    DARRAY = 0.D0
    GNAM = ' '
    GNAMLC = ' '
    NOTESTIMATED = .FALSE.
    OBSNAM = ' '
    OBSNAMLC = ' '
    OMITTED = .FALSE.
    PARNAM = ' '
    Q = 0.D0
    ITEMTYPE = 0
    VARGWG = 0.D0
    XX = 0.D0
    Z = 0.D0
    IF(SENTYPE .EQ. "YES")WRITE(IOUT,500)
    WRITE (IOUT,510) NUM_OLDPAR, NPE, NOBS, MPR, VAR
    ! CORRECT ERRORS IN EC INPUT
    IF(ITRAN) C1 = 'YES'
    IF(USEDCOV)C2 = 'YES'
    IF (EC < 0.D0) THEN
      EC = 0.8D0
      WRITE (IOUT,512) EC,'(WAS RESET BECAUSE IN INPUT FILE EC<0.0)',C1,C2
    ELSEIF (EC > 1.) THEN
      EC = 0.8D0
      WRITE (IOUT,512) EC,'(WAS RESET BECAUSE IN INPUT FILE EC>1.0)',C1,C2
    ELSE
      WRITE (IOUT,512) EC,' ',C1,C2
    ENDIF
    ! Get INFO FROM _su file PARNAM OBS+PRINAM SENSITIVITIES
    WRITE (IOUT,901)
    WRITE (IOUT,535) TRIM(OUTNAM)//'._su'
    CALL SEN_UEV_DX_READ_SU &
           (NOBS,NPEORIG,OBSNAM0,OUTNAM,PANAM,PLOTSYMBOL,XSENS,'_su')
    IF(NUM_OLDPRI > 0) THEN
      WRITE (IOUT,535) ' and '//TRIM(OUTNAM)//'._supri'
      CALL SEN_UEV_DX_READ_SU(NUM_OLDPRI,NPEORIG,PRINAM1,OUTNAM, &
                                          PANAM,PLOTSYMBOLPRI1,XPRI1,'_supri')
      DEALLOCATE(PANAM)
      DO I=1,NUM_OLDPRI
        PRINAM(I) = PRINAM1(I)
        DO J=1,NPEORIG
          XPRI(J,I) = XPRI1(J,I)
        ENDDO
      ENDDO
      DEALLOCATE(PRINAM1,PLOTSYMBOLPRI1,XPRI1)
    ENDIF
    IF(NUM_NEWPRI > 0) THEN
      WRITE (IOUT,535) ' and '//TRIM(OUTNAMPRED)//'._suprip'
      IF(ALLOCATED(PANAM)) DEALLOCATE(PANAM)
      ALLOCATE(PANAM(NPEORIG+NUM_NEWPRI))
      CALL SEN_UEV_DX_READ_SU(NUM_NEWPRI,NPE,PRINAM2,OUTNAMPRED, &
                                          PANAM,PLOTSYMBOLPRI2,XPRI2,'_suprip')
      DO I=1,NUM_NEWPRI
        PRINAM(NUM_OLDPRI+I) = PRINAM2(I)
        DO J=1,NPE
          XPRI(J,NUM_OLDPRI+I) = XPRI2(J,I)
        ENDDO
      ENDDO
      DEALLOCATE(PRINAM2,PLOTSYMBOLPRI2,XPRI2)
    ENDIF

    IF(ALLOCATED(PRINAM))DEALLOCATE(PRINAM)
    IF(ALLOCATED(PLOTSYMBOL))DEALLOCATE(PLOTSYMBOL)
    IF(ALLOCATED(PLOTSYMBOLPRI))DEALLOCATE(PLOTSYMBOLPRI)
    ! Store names as lowercase
    DO I=1,NOBS
      OBSNAM(I) = OBSNAM0(I)
    ENDDO
    DEALLOCATE(OBSNAM0)
    IF (NUM_OLDPRI > 0) THEN
      DO I=NOBS+1,NUM_OLDOBS
        OBSNAM(I) = OBSNAM1(I-NOBS)
      ENDDO
      DEALLOCATE(OBSNAM1)
    ENDIF
    IF (NUM_NEWPRI > 0) THEN
      DO I=1,NUM_NEWOBS
        OBSNAM(I+NUM_OLDOBS) = OBSNAM2(I)
      ENDDO
      DEALLOCATE(OBSNAM2)
    ENDIF
    DO I=1,NTOT
      CALL UTL_CASE(OBSNAM(I),OBSNAMLC(I),-1)
    ENDDO
    ! COUNT NUMBER OF PREDICTIONS IN _P
    IF(NPREDI > 0) THEN
      OUTNAMTMP = TRIM(OUTNAMPRED)//'._p'
      INQUIRE(FILE=OUTNAMTMP,EXIST=LEX)
      IF(.NOT. LEX) THEN
        WRITE(*,485)
        WRITE(IOUT,485)
        GO TO 9000
      ENDIF
      CALL  DEP_UEV_DX_READ_PNUM (IFAIL,OUTNAMPRED,NPRED)
      !  ALLOCATE MEMORY FOR ARRAYS
      ALLOCATE(PREDGRP(NPRED),PREDNAM(NPRED),PREDNAMLC(NPRED),PREDVAL(NPRED), &
               PREDVAR(NPRED),PLOTSYMBOL(NPRED), ZTMP(NPE,NPRED))
      ! READ PREDICTIONS AND VARIANCES IN _P and _PV
      !These variances will be used unless the user overrides with fn.corfac input
      CALL UTLUCODE_DX_READ_PPV(IOUT,OUTNAMPRED,NPRED,.TRUE.,PREDGRP,PREDNAM, &
                                PREDVAL,PREDVAR,PLOTSYMBOL)
      DO I=1,NPRED
        CALL UTL_CASE(PREDNAM(I),PREDNAMLC(I),-1) ! Store names as lowercase
      ENDDO
    ENDIF
    ! GET NAMES OF PARAMETERS AND PREDICTIONS TO EVALUATE
    OPEN (IIN1,FILE=FN,STATUS='UNKNOWN')
    CALL CORFAC_INI2 (IFAIL,NTOT,IIN1,IOUT,ITRAN, &
                      NPARI,NPREDI,GNAM,ITYP,CPTRUOFF2M,VCMPLT,VP2M)
    ! Store names as lowercase
    DO I=1,NOINT
      CALL UTL_CASE(GNAM(I),GNAMLC(I),-1)
    ENDDO
    CLOSE(IIN1)
    ! define prediction=1 or parameter = 2
    DO I=1,NOINT
      IF(I <= NPREDI) THEN
        ITEMTYPE(I) = 1   ! prediction
      ELSE
        ITEMTYPE(I) = 2  ! parameter
      ENDIF
    ENDDO
    !READ AND WRITE FULL SQUARE-ROOT WEIGHT MATRIX ON OBSERVATIONS
    IF(MPR > 0) THEN
      CALL TYP_NULL(WTMAT)
      CALL TYP_NULL(WTMATSQR)
      CALL TYP_NULL(WTMATDEP)
      CALL TYP_NULL(WTMATDEPSQR)
      CALL UTL_DX_READ_WT(IOUT,OUTNAM,'_wt',WTMATDEP,WTMATDEPSQR)
      IF(NUM_OLDPRI > 0 .AND. NUM_NEWPRI > 0) THEN
        WRITE(IOUT,1039) TRIM(OUTNAM)//'._wt, ._wtpri'
        WRITE(IOUT,1040) TRIM(OUTNAMPRED)//'._wtprip'
        CALL TYP_NULL(PRIWTMAT1)
        CALL TYP_NULL(PRIWTMATSQR1)
        CALL UTL_DX_READ_WT(IOUT,OUTNAM,'_wtpri',PRIWTMAT1,PRIWTMATSQR1)
        CALL TYP_NULL(PRIWTMAT2)
        CALL TYP_NULL(PRIWTMATSQR2)
        CALL UTL_DX_READ_WT(IOUT,OUTNAMPRED,'_wtprip',PRIWTMAT2,PRIWTMATSQR2)
        CALL TYP_NULL(PRIWTMAT)
        CALL TYP_NULL(PRIWTMATSQR)
        CALL UTL_COMBINESQMATRIX(PRIWTMAT1,PRIWTMAT2,PRIWTMAT)
        CALL UTL_COMBINESQMATRIX(PRIWTMATSQR1,PRIWTMATSQR2,PRIWTMATSQR)
        CALL UTL_COMBINESQMATRIX(WTMATDEP,PRIWTMAT,WTMAT)
        CALL UTL_COMBINESQMATRIX(WTMATDEPSQR,PRIWTMATSQR,WTMATSQR)
        CALL TYP_DEALLOC(PRIWTMAT1)
        CALL TYP_DEALLOC(PRIWTMATSQR1)
        CALL TYP_DEALLOC(PRIWTMAT2)
        CALL TYP_DEALLOC(PRIWTMATSQR2)
        CALL TYP_DEALLOC(PRIWTMAT)
        CALL TYP_DEALLOC(PRIWTMATSQR)
      ELSEIF(NUM_OLDPRI > 0) THEN
        WRITE(IOUT,1039) TRIM(OUTNAM)//'._wt and ._wtpri'
        CALL TYP_NULL(PRIWTMAT1)
        CALL TYP_NULL(PRIWTMATSQR1)
        CALL UTL_DX_READ_WT(IOUT,OUTNAM,'_wtpri',PRIWTMAT1,PRIWTMATSQR1)
        CALL UTL_COMBINESQMATRIX(WTMATDEP,PRIWTMAT1,WTMAT)
        CALL UTL_COMBINESQMATRIX(WTMATDEPSQR,PRIWTMATSQR1,WTMATSQR)
        CALL TYP_DEALLOC(PRIWTMAT1)
        CALL TYP_DEALLOC(PRIWTMATSQR1)
      ELSE
        WRITE(IOUT,1039) TRIM(OUTNAM)//'._wt'
        WRITE(IOUT,1040) TRIM(OUTNAMPRED)//'._wtprip'
        CALL TYP_NULL(PRIWTMAT2)
        CALL TYP_NULL(PRIWTMATSQR2)
        CALL UTL_DX_READ_WT(IOUT,OUTNAMPRED,'_wtprip',PRIWTMAT2,PRIWTMATSQR2)
        CALL UTL_COMBINESQMATRIX(WTMATDEP,PRIWTMAT2,WTMAT)
        CALL UTL_COMBINESQMATRIX(WTMATDEPSQR,PRIWTMATSQR2,WTMATSQR)
        CALL TYP_DEALLOC(PRIWTMAT2)
        CALL TYP_DEALLOC(PRIWTMATSQR2)
      ENDIF
      CALL TYP_DEALLOC(WTMATDEP)
      CALL TYP_DEALLOC(WTMATDEPSQR)
    ELSE
      WRITE(IOUT,1039) TRIM(OUTNAM)//'._wt'
      CALL TYP_NULL(WTMAT)
      CALL TYP_NULL(WTMATSQR)
      CALL UTL_DX_READ_WT(IOUT,OUTNAM,'_wt',WTMAT,WTMATSQR)
    ENDIF
    ! TRANSFORM V
    IF (USEDCOV) THEN
      VCMPLT = 0.D0
      DO I =1,NTOT
        VCMPLT(I,I) = 1.D0
      ENDDO
    ELSE
      CALL TRANSV(NTOT,WTMATSQR,VCMPLT)
    ENDIF
    ! PUT PREDICTION VALUE AT OPT PARAM, VARIANCE AND SENSITIVITY
    ! FOR THE ITEMS OF INTEREST FROM TMP TO PERMANENT ARRAYS
    IF(NPREDI > 0) THEN
      ! READ PREDICTION  SENSITIVITY _SPU
      CALL UTLUCODE_DX_READ_SPU(OUTNAMPRED,NPE,NPRED,PREDNAM,PARNAM, &
                                PLOTSYMBOL,ZTMP)
      DO I = 1,NPREDI
        DO J = 1,NPRED
          IF(TRIM(GNAMLC(I)) .EQ. TRIM(PREDNAMLC(J))) THEN
            IF(PREDVAR(J) > 0.D0) THEN
              VARGWG(I) = PREDVAR(J)
              IF(VP2M(I) > 0.D0)VARGWG(I) = VP2M(I)
            ENDIF
            GOPT(I) = PREDVAL(J)
            DO K=1,NPE
              Z(K,I) = ZTMP(K,J)
            ENDDO
            EXIT
          ENDIF
          IF(J == NPRED) CALL UTL_STOP &
          ('PREDICTION NOT FOUND, COMPARE SPELLING: CORFAC & PREDICTION INPUT')
        ENDDO
      ENDDO
      WRITE (IOUT,901)
      WRITE (IOUT,536)TRIM(OUTNAMPRED)//'._spu'
      ! DEALLOCATE TEMPORARY ARRAYS
      DEALLOCATE(PREDGRP,PREDNAM,PREDNAMLC,PREDVAL,PREDVAR,PLOTSYMBOL,ZTMP)
    ELSEIF(NPARI > 0) THEN
      IUPC = UTL_GETUNIT(7,1000)
      FN = TRIM(OUTNAM)//'._pc'
      INQUIRE(FILE=FN,EXIST=LEX)
      IF(LEX) THEN
        LEX = .FALSE.
        OPEN (IUPC,FILE=FN,STATUS='UNKNOWN')
        READ(IUPC,*,END=9999)
        DO I=1,NUM_OLDPAR
          READ(IUPC,*,END=9999)PARNAM(I)
        ENDDO
        GO TO 9998
        9999 CALL UTL_STOP('Premature end of file _pc')
        9998 CLOSE(IUPC)
      ELSE
        WRITE(*,1030)
        WRITE(IOUT,1030)
        CLOSE(IOUT)
        CALL UTL_STOP &
        ('This run of CORFAC_PLUS requires _pc from a successful optimization')
      ENDIF
    ENDIF
    !
    CALL UTLUCODE_DX_READ_PAOPT(NPS,OUTNAM,LN,PARNAMALL,PINC,PVAL,NPSWOP)
    IF(NPS > NPSWOP) THEN
      IUPR = UTL_DX_OPEN(OUTNAMPRED,'_paoptp','OLD')
      READ(IUPR,*)
      DO I=1,NPS-NPSWOP
        READ(IUPR,*)PARNAMALL(NPSWOP+I),PVAL(NPSWOP+I),LN(NPSWOP+I)
      ENDDO
    ENDIF
    J = 0
    K = 0
    DO I=1,NPSWOP
      IF(PINC(I) >= 0) K = K + 1
      IF(PINC(I) > 0) THEN
        J = J + 1
        IPTR(J) = I
        EPTR(J) = K
      ENDIF
    ENDDO
    IF(J < NPE) THEN
      DO I=J+1,NPE
        IPTR(I) = IPTR(I-1) + 1
      ENDDO
    ENDIF
    IF(NUM_OLDPAR < NPEORIG) THEN
      DO I=1,NUM_OLDPAR
        DO J=1,NOBS
          XX(I,J) = XSENS(EPTR(I),J)
        ENDDO
        IF(NUM_OLDPRI > 0) THEN
          DO J=1,NUM_OLDPRI
!new replace below            XX(I,J+NOBS) = XPRI(EPTR(I),J)
            XX(I,J+NOBS) = XPRI(I,J)
          ENDDO
        ENDIF
      ENDDO
      IF(NUM_NEWPAR > 0) THEN
        DO I=1,NUM_NEWPAR
          DO J=1,NUM_NEWPRI
! new below XX(I+NUM_OLDPAR,J+NOBS+NUM_OLDPRI) = XPRI(EPTR(I),J)
            XX(I+NUM_OLDPAR,J+NOBS+NUM_OLDPRI) = XPRI(I+NUM_OLDPAR-NUM_OLDPRI,J)
          ENDDO
        ENDDO
      ENDIF
    ELSE
      DO I=1,NUM_OLDPAR
        DO J=1,NOBS
          XX(I,J) = XSENS(I,J)
        ENDDO
        IF(NUM_OLDPRI > 0) THEN
          DO J=1,NUM_OLDPRI
            XX(I,J+NOBS) = XPRI(I,J)
          ENDDO
        ENDIF
      ENDDO
      IF(NUM_NEWPAR > 0) THEN
        DO I=1,NUM_NEWPAR
          DO J=1,NUM_NEWPRI
            XX(I+NUM_OLDPAR,J+NOBS+NUM_OLDPRI) = XPRI(I+NUM_OLDPAR,J+NUM_OLDPRI)
          ENDDO
        ENDDO
      ENDIF
    ENDIF
    DEALLOCATE (XSENS,XPRI)
    IF(NPARI > 0) THEN
      ! PUT OPTIMAL PARAMETER VALUES IN GOPT
      DO I=1,NPS
        CALL UTL_CASE(PARNAMALL(I),PARNAMALLLC(I),-1) ! Store names as lowercase
      ENDDO
      DO I=1,NPE
        CALL UTL_CASE(PARNAM(I),PARNAMLC(I),-1) ! Store names as lowercase
      ENDDO
      DO I = NPREDI+1,NOINT
        DO J = 1,NPS
          IF(TRIM(GNAMLC(I)) .EQ. TRIM(PARNAMALLLC(J))) THEN
            GOPT(I) = PVAL(J)
            IF(PINC(J) == 0) OMITTED(I) = .TRUE.
            IF(PINC(J) == -1) NOTESTIMATED(I) = .TRUE.
            ! VARIANCE FOR PARAMETERS WILL BE ZERO UNLESS USER SPECIFIES A
            !  AND MEASSTATISTIC AND MEASSTATFLAG
            VARGWG(I) = 0.D0
            IF(VP2M(I) > 0.D0)VARGWG(I) = VP2M(I)
            DO IP=1,NPE
              ! SENSITIVITIY IS ONE TO ITSELF AND ZERO TO OTHER PARAMETERS
              IF(TRIM(GNAMLC(I)) .EQ. TRIM(PARNAMLC(IP))) Z(IP,I) = 1.D0
            ENDDO
            EXIT
          ENDIF
          IF(J == NPS) CALL UTL_STOP &
          ('PARAMETER NOT FOUND, COMPARE SPELLING in CORFAC & REGRESSION INPUT')
        ENDDO
      ENDDO
    ENDIF
    ! TRANSFORM SENSITIVITIES BY SQUARE-ROOT WEIGHTS
    ALLOCATE(XTMP(NPE,NTOT))
    CALL UTL_MATMUL_SUB(NPE,NTOT,XX,WTMATSQR,XTMP)
    XX = XTMP
    DEALLOCATE(XTMP)
    ! START COR FAC CALCS
    WRITE (IOUT,*)
    WRITE (IOUT,902)
    WRITE (IOUT,902)
    IF(ITYP .EQ. 1)THEN
      WRITE (IOUT,730) NOINT, NPARI
    ELSEIF(ITYP .EQ. 2) THEN
      WRITE (IOUT,731) NOINT, NPARI
    ELSE
      WRITE(IOUT,901)
      WRITE(IOUT,1001)
      WRITE(IOUT,901)
      WRITE(*,901)
      WRITE(*,1001)
      WRITE(*,901)
      CLOSE(IOUT)
      STOP
    ENDIF
    !
    ALLOCATE(ROWVALS(5,NOINT))
    ROWVALS = 0.D0
    !
    WRITE (IOUT,901)
    IF(NOINT > 1) THEN
      WRITE(IOUT,556)
    ELSE
      WRITE(IOUT,557)
    ENDIF
    CALL PRTOTB_LENDNAM(NOINT,IOUT,VARGWG,GNAM)
    DO KNT=1,NOINT
      ROWVALS(1,KNT) = GOPT(KNT)
      ROWVALS(2,KNT) = VARGWG(KNT)
    ENDDO
    !
    WRITE (IOUT,901)
    WRITE (IOUT,537)
    CALL PRTOTX(NOINT,NPE,IOUT,GNAM,PARNAM,Z)
    ! COMPUTE THE OBSERVATION BLOCK OF THE HAT MATRIX (CF), THE TRACE
    ! AND SUM OF ELEMENTS OF I-CF (TOCF AND CFSM), AND THE TRACE OF
    ! THE PRIOR BLOCK OF THE I-HAT MATRIX (TPCF)
    CALL HATMAT(NOBS,NPE,NTOT,MPR,XX,CF,CFSM,DARRAY,TOCF,TPCF)
    DF = NTOT - NPE
    !****  FOR CONFIDENCE REGION
    ! COMPUTE CORRECTION FOR CONFIDENCE REGION
    WRITE(IOUT,*)
    WRITE(IOUT,*)
    WRITE(IOUT,902)
    WRITE(IOUT,902)
    WRITE(IOUT,715)
    IF (USEDCOV .OR. ITRAN) THEN
      ! COMPUTE B
      B = 0.D0
      DO I = 1, NOBS
        B = B + VCMPLT(I,I)
      ENDDO
      B = (B + MPR) / NTOT
      ! COMPUTE A
      A = MPR - TPCF
      DO I = 1, NOBS
        DO J = 1, NOBS
          A = A + CF(I,J) * VCMPLT(J,I)
        ENDDO
      ENDDO
      A = A / (B * NPE)
      ! COMPUTE EFFECTIVE CORRELATION, CORRELATION
      CORRELATION = (A - 1.) * NPE / (TOCF - CFSM)
      ! COMPUTE VARIANCE AMPLIFICATION RATIO, FAC
      FAC = DF / (NTOT - A * NPE)
      ! COMPUTE CORRECTION FACTOR FOR CONFIDENCE REGION, CCR
      CCR = A * FAC
      CCB1 = CCR !case  1,1 1,2 1,3 2,1 2,2 2,3
      ! PRINT RESULTS
      WRITE(IOUT,800) A, B, CORRELATION, FAC, CCR
      IF(ITYP == 1) THEN
        IF(USEDCOV) THEN
          CCRIC = 1.D0
          WRITE(IOUT,799) CCRIC
          DO KNT = 1,NOINT
            ROWVALS(3,KNT) = CCRIC ! case 1,1 used cov, individ confint
            ROWVALS(4,KNT) = CCR   ! case 1,2 used cov, simult confint
            ROWVALS(5,KNT) = ROWVALS(4,KNT)
            CCWRITE(KNT) = CCRIC   ! case 1,1 case 1,2
          ENDDO
        ELSEIF(ITRAN) THEN
          DO KNT = 1,NOINT
            ROWVALS(4,KNT) = CCR   ! case 2,2 read cov, simult conint
            ROWVALS(5,KNT) = ROWVALS(4,KNT)
          ENDDO
        ENDIF
      ENDIF
      IF(ITYP == 2) THEN
        IF(USEDCOV) THEN
          CCRIC = 1.D0
          DO KNT = 1,NOINT
            ROWVALS(5,KNT) = CCR !CRMODLIN for case 1,2 used cov, simult predint
          ENDDO
        ELSEIF(ITRAN) THEN
          DO KNT = 1,NOINT
             ROWVALS(5,KNT) = CCR !CRMODLIN for case 2,2 read cov, simult predint
          ENDDO
        ENDIF
      ENDIF
    ENDIF
    !
    ! COMPUTE APPROXIMATE CORRECTION FOR CONFIDENCE REGION
    OMEC = 1. - EC
    ! COMPUTE APPROXIMATE NTOT-A*NPE=DFB
    DFB = OMEC * TOCF + EC * CFSM + TPCF
    ! COMPUTE VARIANCE AMPLIFICATION RATIO, FAC
    FACB = DF / DFB
    ! COMPUTE BOUND, CRB, FOR CORRECTION FACTOR, CCR
    CRB = FACB * (NPE + EC *(TOCF - CFSM)) / NPE
    ! PRINT RESULTS
    WRITE(IOUT,810) DFB, FACB, CRB
    IF((.NOT. USEDCOV) .AND. (.NOT. ITRAN) .AND. (ITYP == 1)) THEN
      DO KNT = 1,NOINT
        ROWVALS(4,KNT) = CRB   ! case 3,2 unknown cov, simult conint
        ROWVALS(5,KNT) = ROWVALS(4,KNT)
        CCWRITE(KNT) = CRB
        CCB1 = CRB ! case 3,2 unknown cov, simult conint
      ENDDO
    ENDIF
    IF((.NOT. USEDCOV) .AND. (.NOT. ITRAN) .AND. (ITYP == 2)) THEN
      DO KNT = 1,NOINT   
       ROWVALS(5,KNT) = CRB !CRMODLIN for case 3,2 unknown cov, simult predint
      ENDDO
    ENDIF
    !
    !****  LOOP ON PROBABILITY INTERVALS
    WRITE(IOUT,*)
    WRITE(IOUT,*)
    WRITE(IOUT,902)
    WRITE(IOUT,902)
    IF (ITYP.EQ.2) THEN
      WRITE(IOUT,720) 'PREDICTION'
    ELSE
      WRITE(IOUT,720) 'CONFIDENCE'
    ENDIF
    !
    DO KNT = 1, NOINT
      IF (ITYP.EQ.2) THEN
        WRITE(IOUT,820) 'PREDICTION', KNT, GNAM(KNT)
      ELSE
        WRITE(IOUT,820) 'CONFIDENCE', KNT, GNAM(KNT)
      ENDIF
      IF(KNT > NPREDI) THEN
        IF(OMITTED(KNT)) THEN
          WRITE(IOUT,824)
          ROWVALS(3,KNT)=0.D0
          ROWVALS(4,KNT)=0.D0
          CCWRITE(KNT)=0.D0  ! case omit
          CYCLE
        ELSEIF(NOTESTIMATED(KNT)) THEN
          WRITE(IOUT,826)
          ROWVALS(3,KNT)=0.D0
          ROWVALS(4,KNT)=0.D0
          CCWRITE(KNT)=0.D0  ! case omit
        ENDIF
      ENDIF
      !   COMPUTE Q=W*XX*(XX'*W*XX)^-1*Z' AND VE=Q'*Q
      VE = 0.D0
      DO I = 1, NTOT
        SUM = 0.D0
        DO IP = 1, NPE
          SUM = SUM + Z(IP,KNT)*DARRAY(IP,I)
        ENDDO
        VE = VE + SUM * SUM
        Q(I) = SUM
      ENDDO
      !   COMPUTE SUM OF SQUARED Q-VALUES FOR PRIOR
      SSQP = 0.D0
      IF (MPR.GT.0) THEN
        DO I = NOBS+1, NTOT
          SSQP = SSQP + Q(I) * Q(I)
        ENDDO
      ENDIF
      !   COMPUTE QQUD=Q'*V*Q
      IF (ITRAN) THEN
        QQUD = SSQP
        DO I=1,NOBS
          SUM = 0.D0
          DO J=1,NOBS
            SUM = SUM + VCMPLT(I,J) * Q(J)
          ENDDO
          QQUD = QQUD + SUM * Q(I)
        ENDDO
      ENDIF
      !   FOR OBSERVATION BLOCK COMPUTE SUM OF Q (QSUM), SUM OF
      !   POSITIVES (QSMP), SUM OF NEGATIVES (QSMN), AND SUM OF
      !   SQUARED Q VALUES
      QSUM = 0.D0
      QSMP = 0.D0
      DO I = 1, NOBS
        QSUM = QSUM + Q(I)
        IF (Q(I).GT.0.D0) QSMP = QSMP + Q(I)
      ENDDO
      QSMN = QSUM - QSMP
      SSQM = VE - SSQP
      !
      !   *** FOR CONFIDENCE INTERVALS ***
      IF (ITYP == 1) THEN
        IF (ITRAN) THEN
          !COMPUTE XI AND CORRECTION FACTOR, CCI
          XI = QQUD / (B * VE)
          CCI(KNT) = XI * FAC
          WRITE(IOUT,830) XI, CCI(KNT)
          CCWRITE(KNT)=CCI(KNT)  ! case 2,1
          ROWVALS(3,KNT) = CCI(KNT)  ! case 2,1 read cov, individ conint
        ENDIF
        !APPROXIMATE BOUND FOR CCI, CCIB
        XIPB = QSMP * QSMP
        XINB = QSMN * QSMN
        XIMX = MAX(XIPB, XINB)
        CCIB = FACB * (OMEC * SSQM + EC * XIMX + SSQP) / VE
        XIPB = XIPB / VE
        XINB = XINB / VE
        VE = VAR * VE
        WRITE(IOUT,840) VE, XIPB, XINB, CCIB
        IF(.NOT. USEDCOV .AND. .NOT. ITRAN) THEN
          ROWVALS(3,KNT) = CCIB   ! case 3,1 unknown cov, individ conint
          CCWRITE(KNT)=CCIB       ! case 3,1
          CCB1 = CCIB ! case 3,1
        ENDIF
        !
        !  *** FOR PREDICTION INTERVALS ***
      ELSE
        IF (USEDCOV .OR. ITRAN) THEN
          IF (ITRAN) THEN
            !WRITE TRUE PREDICTION SECOND MOMENT (VP) AND
            !OFF-DIAGONAL SECOND MOMENTS (CP)
            WRITE(IOUT,850) VP2M(KNT)
            WRITE(IOUT,860)
            CALL PRTOTB_LENDNAM(NTOT,IOUT,CPTRUOFF2M(KNT:KNT,:),OBSNAM)
            !TRANSFORM CPTRUOFF2M TO CPTRUOFF2M*W
            CALL TRANSCP(NTOT,WTMATSQR,CPTRUOFF2M(KNT:KNT,:))
            COVP = 0.D0
            DO I = 1, NTOT
              COVP = COVP + CPTRUOFF2M(KNT,I) * Q(I)
            ENDDO
            !COMPUTE XI AND CORRECTION FACTOR, CPI
            XI = (VP2M(KNT) + (QQUD - 2. * COVP) / B) / (VE + VP2M(KNT))
          ELSE
            ! cov was used for regression
            XI = 1.D0
          ENDIF
          CPI = XI * FAC
          WRITE(IOUT,870) XI, CPI
          ROWVALS(3,KNT) = CPI  ! case 2,3 or case 1,3 use or read cov, individ predint
          CCWRITE(KNT)=CPI      ! case 2,3 case 1,3
        ELSE
          VP2M(KNT) = VARGWG(KNT)
        ENDIF
    !     APPROXIMATE BOUND FOR CPI, CPIB
        XIPB = QSMP * QSMP
        XINB = QSMN * QSMN - 2. * QSMN * SQRT(VP2M(KNT)) + VP2M(KNT)
        XIMX = MAX(XIPB, XINB)
        CPIB = FACB * (OMEC * (SSQM + VP2M(KNT)) &
               + EC * XIMX + SSQP) / (VE + VP2M(KNT))
        XIPB = XIPB / (VE + VP2M(KNT))
        XINB = XINB / (VE + VP2M(KNT))
        VE = VAR * VE
        WRITE(IOUT,880) VE, XIPB, XINB, CPIB
        IF(.NOT. USEDCOV .AND. .NOT. ITRAN) THEN
          ROWVALS(3,KNT) = CPIB ! case 3,3 unknown cov, individ pred
          CCWRITE(KNT)=CPIB  ! case 3,3
          CCB1 = CPIB ! case 3,3
        ENDIF
      ENDIF
    ENDDO
    !WRITE CORFAC underscore files
    IF(ITYP == 1) THEN
      IUCORFACS = UTL_DX_OPEN(OUTNAMPRED,'_cfconf','REPLACE')
      EXTB1ADV = '_b1advconf'
      EXTB3 = '_b3conf'
    ELSE
      IUCORFACS = UTL_DX_OPEN(OUTNAMPRED,'_cfpred','REPLACE')
      EXTB1ADV = '_b1advpred'
      EXTB3 = '_b3pred'
    ENDIF
    CALL UTLUCODE_DX_WRITE_COLS &
       (IUCORFACS,NOINT,5,COLNAM,ROWVALS,GNAM,ITEMTYPE) !,COL12NAM)
    CLOSE(IUCORFACS)
    MXRECL = 52+NPPREC*26
    IUCORFACS = UTL_DX_OPEN(OUTNAMPRED,'_cfsu','REPLACE',MXRECL)
    CALL UTLUCODE_DX_WRITE_MATRIX &
           (IUCORFACS,NOINT,NPE,NPS,IPTR,PARNAMALL,Z,GNAM,ITEMTYPE,COL12NAM)
    CLOSE(IUCORFACS)
  ELSE
    WRITE(IOUT,901)
    WRITE(IOUT,1000)
    WRITE(IOUT,901)
    WRITE(*,901)
    WRITE(*,1000)
    WRITE(*,901)
  ENDIF
  ! READ PARAMETER NAMES AND VARIANCE-COVARIANCE MATRIX ON THE PARAMETERS ._mv
  IF(NUM_NEWPAR == 0) THEN
    WRITE(IOUT,1059) TRIM(OUTNAM)//'._mv'
    CALL UTL_DX_READ_MCMV('_mv',NPE,OUTNAM,PARNAM,COV)
  ELSE
    WRITE(IOUT,1059) TRIM(OUTNAM)//'._mv'
    CALL UTL_DX_READ_MCMV('_mv',NUM_OLDPAR,OUTNAM,PARNAM1,COV1)
    DO I=1,NUM_OLDPAR
      PARNAM(I) = PARNAM1(I)
      CALL UTL_CASE(PARNAM(I),PARNAMALLLC(I),-1)
      CALL UTL_CASE(PARNAM(I),PARNAMLC(I),-1)
      DO J=1,NUM_OLDPAR
        COV(I,J) = COV1(I,J)
      ENDDO
    ENDDO
    DEALLOCATE(COV1)
    ! READ PARAMETER NAMES AND VAR-COV MATRIX ON PARAMETERS FOR PREDICTION ._mvp
    WRITE(IOUT,1059) TRIM(OUTNAMPRED)//'._mvp'
    CALL UTL_DX_READ_MCMV('_mvp',NUM_NEWPAR,OUTNAMPRED,PARNAM2,COV2)
    DO I=1,NUM_NEWPAR
      PARNAM(I+NUM_OLDPAR) = PARNAM2(I)
      PARNAMALL(I+NPSWOP) = PARNAM(I+NUM_OLDPAR)
      CALL UTL_CASE(PARNAM(I+NUM_OLDPAR),PARNAMALLLC(NPSWOP+I),-1)
      CALL UTL_CASE(PARNAM(I+NUM_OLDPAR),PARNAMLC(NUM_OLDPAR+I),-1)
      DO J=1,NUM_NEWPAR
        COV(I+NUM_OLDPAR,J+NUM_OLDPAR) = COV2(I,J)
      ENDDO
     ENDDO
    DEALLOCATE(COV2)
  ENDIF
  !
  !     READ AND WRITE PARAMETER INFO
  ! WRITE _ b1adv and _b3
  CALL UTLUCODE_DX_WRITE_B1 &
       (MPR,NOBS,NPE,NPS,CCB1,COV,IOUT,IPTR,LN,OUTNAMPRED,PARNAMALL,PVAL, &
        EXTB1ADV)
  CALL UTLUCODE_DX_WRITE_B3 &
       (NOINT,NPE,NPS,COV,CCWRITE,EXTB3,IOUT,IPTR,ITYP,LN,OUTNAMPRED, &
        PARNAMALL,PVAL,VARGWG,Z)
  WRITE(IOUT,901)
  WRITE(IOUT,1002)
  WRITE(IOUT,902)
  WRITE(IOUT,902)
  WRITE(*,901)
  WRITE(*,1002)
  WRITE(*,902)
  WRITE(*,902)
  9000 CONTINUE
  WRITE(*,900)VERSION
  WRITE(IOUT,900)VERSION
  CALL TYP_DEALLOC(WTMAT)
  CALL TYP_DEALLOC(WTMATSQR)
  CALL BAS_CLN()
  CALL UTL_ENDTIME(IBDT,IOUT)
  CLOSE(IOUT)
  !
END PROGRAM CORFAC_PLUS
!===============================================================================
!===============================================================================
  SUBROUTINE CORFAC_INI1 (IFAIL,INUNIT,OUTNAMPRED,VERSION, &
                          CFCOL,IOUT,EC,ITRAN,NPARI,NPREDI,USEDCOV)
  !******************************************************************
  !     INITIALIZE PARAMETERS FOR CORFAC
  !     Make counts to initialize arrays containing obs preds and params
  !******************************************************************
  !        SPECIFICATIONS:
  !------------------------------------------------------------------
    USE BASIC, ONLY: BAS_INI_GETOPTIONS
    USE GLOBAL_DATA, ONLY: HYPHENS, IVERB, MAX_STRING_LEN
    USE DATATYPES
    USE UTILITIES
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                                     INTENT(INOUT)    :: IFAIL
    INTEGER,                                     INTENT(IN)       :: INUNIT
    CHARACTER(LEN=MAX_STRING_LEN),               INTENT(IN)       :: OUTNAMPRED
    CHARACTER(LEN=20),                           INTENT(IN)       :: VERSION
    INTEGER,                                     INTENT(INOUT)    :: CFCOL
    INTEGER,                                     INTENT(INOUT)    :: IOUT
    DOUBLE PRECISION,                            INTENT(OUT)      :: EC
    LOGICAL,                                     INTENT(OUT)      :: ITRAN
    INTEGER,                                     INTENT(OUT)      :: NPARI
    INTEGER,                                     INTENT(OUT)      :: NPREDI
    LOGICAL,                                     INTENT(OUT)      :: USEDCOV
    !   Local variables
    TYPE (LLIST), POINTER           :: CORFACHEAD
    CHARACTER(LEN=MAX_STRING_LEN)   :: FN
    INTEGER                         :: IERR
    CHARACTER(LEN=79)               :: INTERVALTYPE
    INTEGER                         :: NUCORFAC
    TYPE (LLIST), POINTER           :: PARAMIHEAD
    TYPE (LLIST), POINTER           :: PREDIHEAD
    TYPE (LLIST), POINTER           :: TAIL
    CHARACTER(LEN=3)                :: TMP1
    CHARACTER(LEN=3)                :: TMP2
    CHARACTER(LEN=79)               :: TMPIT
    !
    !   For CORRECTION_FACTOR_DATA input, define default column order
    INTEGER,           PARAMETER                      :: CORFACCOLS = 3
    CHARACTER(LEN=40), DIMENSION(CORFACCOLS)  :: CORFACCOL = &
        (/ 'CONFIDENCEORPREDICTION','EFFECTIVECORRELATION  ', &
           'READ_COV              '/)
    !   For PREDICTION_LIST input, define default column order
    INTEGER,           PARAMETER                      :: PREDICOLS = 3
    CHARACTER(LEN=40), DIMENSION(PREDICOLS)  :: PREDICOL = &
        (/ 'PREDNAME             ','MEASSTATISTIC        ', &
           'MEASSTATFLAG         '/)
    !   For PARAMETER_LIST input, define default column order
    INTEGER,           PARAMETER                      :: PARAMICOLS = 3
    CHARACTER(LEN=40), DIMENSION(PARAMICOLS)  :: PARAMICOL = &
        (/ 'PARAMETERNAME        ','MEASSTATISTIC        ', &
           'MEASSTATFLAG         '/)
    !   For MATRIX_FILES input, define default column order
    !
    ! formats
    100 FORMAT(1X,A)
    480 FORMAT (34X,'CORFAC_PLUS',//, &
        18X,'UCODE POST-PROCESSING PROGRAM TO CALCULATE',/, &
        1X,'CORRECTION FACTORS FOR CONFIDENCE REGION & CONFIDE', &
           'NCE AND PREDICTION INTERVALS',/, &
        29X,'Version ',A/)
    2000 FORMAT(//, &
        '  The true error convariance matrix was used for the regression.',/, &
        '  However it is supplied here. This is not logical. Alter ',/, &
        '  fn.corfac to either omit the matrix or indicate it was not used.', &
        //)
    ! Initialize
    EC = 0.D0
    IFAIL = 0
    IERR = 0
    TMP1 = ' NO'
    TMP2 = ' NO'
    ITRAN = .FALSE.
    USEDCOV = .FALSE.
    NULLIFY(CORFACHEAD,PARAMIHEAD,PREDIHEAD,TAIL)
    NUCORFAC = 0
    !
    IOUT = UTL_GETUNIT(7,1000)
    FN = TRIM(OUTNAMPRED)//'.#corfac.err'
    OPEN (IOUT,FILE=FN,STATUS='REPLACE')
    CALL BAS_INI_GETOPTIONS(INUNIT,IOUT)
    !   Read options
    CALL UTL_READBLOCK(CORFACCOLS,'CORRECTION_FACTOR_DATA',CORFACCOL,INUNIT, &
                       IOUT,'*',.TRUE.,CORFACHEAD,TAIL,NUCORFAC)
    IF(NUCORFAC > 0) THEN
      !
      CALL UTL_FILTER(IERR,CORFACHEAD,IOUT,'CONFIDENCEORPREDICTION', &
                      INTERVALTYPE)
      CALL UTL_CASE(INTERVALTYPE,TMPIT,1)
      IF(TMPIT == 'CONFIDENCE' .OR. TMPIT == 'PREDICTION') THEN
        SELECT CASE (TMPIT)
          CASE ('CONFIDENCE')
            FN = TRIM(OUTNAMPRED)//'.#corfac_conf'
          CASE ('PREDICTION')
            FN = TRIM(OUTNAMPRED)//'.#corfac_pred'
        END SELECT
        CLOSE(IOUT,STATUS='DELETE')
        IOUT = UTL_GETUNIT(7,1000)
        OPEN (IOUT,FILE=FN,STATUS='UNKNOWN')
        WRITE (IOUT,480) VERSION
      ELSE
        WRITE(IOUT,*) &
        (' ConfidenceOrPrediction MUST BE CONFIDENCE OR PREDICTION TERMINATING')
        CLOSE(IOUT)
        CALL UTL_STOP(' TERMINATING CORFAC_PLUS')
      ENDIF
      !
      IF (IVERB>3) THEN
        !   Write block information (correction factor) to output file
        WRITE(IOUT,'(A)')HYPHENS(1:80)
        WRITE(IOUT,100)'Echo CORRECTION_FACTOR_DATA input:'
        CALL UTL_WRITEBLOCK(CORFACHEAD,IOUT)
      ENDIF
      !
      CALL UTL_FILTER(IERR,CORFACHEAD,IOUT,'EFFECTIVECORRELATION',EC)
      CALL UTL_FILTER(IERR,CORFACHEAD,IOUT,'READ_COV',TMP1)
      !
      CALL UTL_CASE(TMP1,TMP2,1)
      IF(TRIM(TMP2) == 'NO' .OR. TRIM(TMP2) == 'YES') THEN
        ! all is well
        SELECT CASE (TMP2)
          CASE (' NO')
            ITRAN = .FALSE.
            CFCOL = 3
            WRITE(IOUT,*)'  USER WILL NOT SUPPLY True Error Cavariance Matrix'
          CASE ('YES')
            ITRAN = .TRUE.
            CFCOL = 2
            WRITE(IOUT,*)'  USER WILL SUPPLY True Error Cavariance Matrix'
        END SELECT
      ELSE
        !CALL UTL_STOP &
        !(' Read_Cov MUST BE YES OR NO, TERMINATING CORFAC_PLUS')
        ! Default to no for Read_Cov
        ITRAN = .FALSE.
        CFCOL = 3
        WRITE(IOUT,*)'  USER WILL NOT SUPPLY True Error Cavariance Matrix'
      ENDIF
      TMP1 = ' NO'
      TMP2 = ' NO'
      CALL UTL_FILTER(IERR,CORFACHEAD,IOUT,'REGRESSIONUSEDTRUECOV',TMP1)
      !
      CALL UTL_CASE(TMP1,TMP2,1)
      IF(TRIM(TMP2) == 'NO' .OR. TRIM(TMP2) == 'YES') THEN
        ! all is well
        SELECT CASE (TMP2)
          CASE (' NO')
            USEDCOV = .FALSE.
            IF(CFCOL == 0) CFCOL = 3
          CASE ('YES')
            USEDCOV = .TRUE.
            IF(CFCOL == 2) THEN
              WRITE(*,2000)
              WRITE(IOUT,2000)
              CALL UTL_STOP('Select logical input for CORFAC_PLUS')
            ELSE
              CFCOL = 1
            ENDIF
        END SELECT
      ELSE
        ! default to yes
        USEDCOV = .TRUE.
        IF(CFCOL == 2) THEN
          WRITE(*,2000)
          WRITE(IOUT,2000)
          CALL UTL_STOP('Select logical input for CORFAC_PLUS')
        ELSE
          CFCOL = 1
        ENDIF
      ENDIF
    ELSE
      IF(IVERB > 1) THEN
        WRITE(IOUT,*)
        WRITE(IOUT,100) &
                     '*********************************************************'
        WRITE(IOUT,100) &
                     '    NOTE CORRECTION_FACTOR_DATA BLOCK NOT FOUND          '
        WRITE(IOUT,100)'                DEFAULTS ARE USED '
        WRITE(IOUT,100)'   THIS MAY BE THE DESIRED OPTION.  IF NOT, THEN'
        WRITE(IOUT,100)'   TO READ THIS BLOCK, CHECK BLOCK NAME SPELLING'
        WRITE(IOUT,100) &
                     '*********************************************************'
      ENDIF
    ENDIF
    !
    ! Find next data block
    !   PREDICTIONS_NONLINEAR_UNCERTAINTY block
    NPREDI = 0
    CALL UTL_READBLOCK(PREDICOLS,'PREDICTION_LIST',PREDICOL, &
         INUNIT,IOUT,'PREDNAME',.FALSE.,PREDIHEAD,TAIL,NPREDI)
    IF (NPREDI > 0) THEN
      IF (IVERB>3) THEN
        !   Write block information (correction factor) to output file
        WRITE(IOUT,'(A)')HYPHENS(1:80)
        WRITE(IOUT,100)'Echo PREDICTION_LIST input:'
        CALL UTL_WRITEBLOCK(PREDIHEAD,IOUT)
      ENDIF
    ELSE
      IF(IVERB > 1) THEN
        WRITE(IOUT,*)
        WRITE(IOUT,100) &
                    '*********************************************************'
        WRITE(IOUT,100)'        NOTE PREDICTION_LIST BLOCK NOT FOUND          '
        WRITE(IOUT,100) &
                    'CORRECTION FACTORS FOR PREDICTIONS WILL NOT BE CALCULATED'
        WRITE(IOUT,100)'   THIS MAY BE THE DESIRED OPTION.  IF NOT, THEN'
        WRITE(IOUT,100)'   TO READ THIS BLOCK, CHECK BLOCK NAME SPELLING'
        WRITE(IOUT,100) &
                    '*********************************************************'
        WRITE(IOUT,*)
      ENDIF
    ENDIF
    !
    ! Find next data block
    !   PARAMETERS_NONLINEAR_UNCERTAINTY block
    NPARI = 0
    CALL UTL_READBLOCK(PARAMICOLS,'PARAMETER_LIST',PARAMICOL, &
         INUNIT,IOUT,'PARAMETERNAME',.FALSE.,PARAMIHEAD,TAIL,NPARI)
    IF (NPARI > 0) THEN
      IF (IVERB>3) THEN
        !   Write block information (correction factor) to output file
        WRITE(IOUT,'(A)')HYPHENS(1:80)
        WRITE(IOUT,100)'Echo PARAMETER_LIST input:'
        CALL UTL_WRITEBLOCK(PARAMIHEAD,IOUT)
      ENDIF
    ELSE
      IF(IVERB > 1) THEN
        WRITE(IOUT,*)
        WRITE(IOUT,100) &
                     '*********************************************************'
        WRITE(IOUT,100)'  NOTE PARAMETER_LIST BLOCK NOT FOUND          '
        WRITE(IOUT,100) &
                     'CORRECTION FACTORS FOR PARAMETERS WILL NOT BE CALCULATED.'
        WRITE(IOUT,100)'   THIS MAY BE THE DESIRED OPTION.  IF NOT, THEN'
        WRITE(IOUT,100)'   TO READ THIS BLOCK, CHECK BLOCK NAME SPELLING'
        WRITE(IOUT,100) &
                     '*********************************************************'
      ENDIF
    ENDIF
    !
    CALL TYP_DEALLOC(CORFACHEAD)
    CALL TYP_DEALLOC(PARAMIHEAD)
    CALL TYP_DEALLOC(PREDIHEAD)
    RETURN
  END SUBROUTINE CORFAC_INI1
!===============================================================================
!===============================================================================
  SUBROUTINE CORFAC_INI2 (IFAIL,NTOT,INUNIT,IOUT,ITRAN, &
                          NPARI,NPREDI,GNAM,ITYP,CP,V,VP)
  !******************************************************************
  !     INITIALIZE PARAMETERS FOR CORFAC
  !     Read, store, initialize
  !******************************************************************
  !        SPECIFICATIONS:
  !------------------------------------------------------------------
    USE GLOBAL_DATA, ONLY: HYPHENS, IVERB, LENDNAM, MAX_STRING_LEN
    USE DATATYPES
    USE UTILITIES
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                       INTENT(INOUT) :: IFAIL
    INTEGER,                       INTENT(IN)    :: NTOT
    INTEGER,                       INTENT(IN)    :: INUNIT
    INTEGER,                       INTENT(IN)    :: IOUT
    LOGICAL,                       INTENT(IN)    :: ITRAN
    INTEGER,                       INTENT(INOUT) :: NPARI
    INTEGER,                       INTENT(INOUT) :: NPREDI
    CHARACTER(LEN=LENDNAM),        INTENT(INOUT) :: GNAM(NPREDI+NPARI)
    INTEGER,                       INTENT(OUT)   :: ITYP
    DOUBLE PRECISION,              INTENT(OUT)   :: CP(NPREDI+NPARI,NTOT)
    DOUBLE PRECISION,              INTENT(OUT)   :: V(NTOT,NTOT)
    DOUBLE PRECISION,              INTENT(OUT)   :: VP(NPREDI+NPARI)
    !   Local variables
    TYPE (LLIST), POINTER                             :: CORFACHEAD
    TYPE(CDMATRIX)                                    :: CPCDM
    CHARACTER(LEN=MAX_STRING_LEN)                     :: FN
    CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: GNAM1
    CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: GNAM2
    INTEGER                                           :: I
    INTEGER                                           :: IERR
    INTEGER                                           :: INMATRIX
    CHARACTER(LEN=79)                                 :: INTERVALTYPE
    INTEGER                                           :: J
    INTEGER                                           :: K
    TYPE (LLIST), POINTER                             :: LLPTRMATFIL
    INTEGER                                           :: MORE
    INTEGER                                           :: NUCORFAC
    TYPE (LLIST), POINTER                             :: PARAMIHEAD
    TYPE (LLIST), POINTER                             :: PREDIHEAD
    LOGICAL                                           :: READ_OBSPREDCOV
    CHARACTER(LEN=6), ALLOCATABLE, DIMENSION(:)       :: SF1
    CHARACTER(LEN=6), ALLOCATABLE, DIMENSION(:)       :: SF2
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)       :: STATISTIC1
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)       :: STATISTIC2
    TYPE (LLIST), POINTER                             :: TAIL
    CHARACTER(LEN=79)                                 :: TMP1
    TYPE(CDMATRIX)                                    :: VCDM
    DOUBLE PRECISION                                  :: WEIGHT
    !
    !   For CORRECTION_FACTOR_DATA input, define default column order
    INTEGER,           PARAMETER                      :: CORFACCOLS = 4
    CHARACTER(LEN=40), DIMENSION(CORFACCOLS)  :: CORFACCOL = &
        (/ 'CONFIDENCEORPREDICTION','EFFECTIVECORRELATION  ', &
           'READ_COV              ','READ_OBSPREDCOV       '/)
    !   For PREDICTION_LIST input, define default column order
    INTEGER,           PARAMETER                      :: PREDICOLS = 3
    CHARACTER(LEN=40), DIMENSION(PREDICOLS)  :: PREDICOL = &
        (/ 'PREDNAME             ','MEASSTATISTIC        ', &
           'MEASSTATFLAG         '/)
    !   For PARAMETER_LIST input, define default column order
    INTEGER,           PARAMETER                      :: PARAMICOLS = 3
    CHARACTER(LEN=40), DIMENSION(PARAMICOLS)  :: PARAMICOL = &
        (/ 'PARAMETERNAME        ','MEASSTATISTIC        ', &
           'MEASSTATFLAG         '/)
    !   For MATRIX_FILES input, define default column order
    INTEGER,           PARAMETER                      :: NOCOLS = 2
    CHARACTER(LEN=40), DIMENSION(NOCOLS)  :: NOCOL = &
        (/ 'MATRIXFILE','NMATRICES '/)
    ! formats
    80 FORMAT(1X,'Error: MeasStatFlag must be VAR or SD for "',A,'"')
    90 FORMAT(1X,'Error: MeasStatFlag is undefined for "',A,'"')
    100 FORMAT(1X,A)
    !
    ALLOCATE(GNAM1(NPREDI),GNAM2(NPARI),SF1(NPREDI),SF2(NPARI), &
             STATISTIC1(NPREDI),STATISTIC2(NPARI))
    ! Initialize
    IFAIL = 0
    IERR = 0
    INTERVALTYPE = 'CONFIDENCE'
    CP = 0.D0
    NUCORFAC = 0
    READ_OBSPREDCOV = .FALSE.
    SF1 = 'VAR'
    SF2 = 'VAR'
    STATISTIC1 = -1.D-32
    STATISTIC2 = -1.D-32
    V = 0.D0
    VP = 0.D0
    NULLIFY(CORFACHEAD,LLPTRMATFIL,PARAMIHEAD,PREDIHEAD,TAIL)

    !
    CALL UTL_READBLOCK(CORFACCOLS,'CORRECTION_FACTOR_DATA',CORFACCOL,INUNIT, &
                       IOUT,'*',.TRUE.,CORFACHEAD,TAIL,NUCORFAC)
    IF(NUCORFAC > 0) THEN
      !
      CALL UTL_FILTER(IERR,CORFACHEAD,IOUT,'CONFIDENCEORPREDICTION', &
                      INTERVALTYPE)
      !
      CALL UTL_CASE(INTERVALTYPE,TMP1,1)
      IF(TMP1 == 'CONFIDENCE' .OR. TMP1 == 'PREDICTION') THEN
        SELECT CASE (TMP1)
          CASE ('CONFIDENCE')
            ITYP = 1
          CASE ('PREDICTION')
            ITYP = 2
        END SELECT
      ELSE
        WRITE(IOUT,*) &
        (' ConfidenceOrPrediction MUST BE CONFIDENCE OR PREDICTION TERMINATING')
        CALL UTL_STOP(' TERMINATING CORFAC_PLUS')
      ENDIF
      CALL UTL_FILTER(IERR,CORFACHEAD,IOUT,'READ_OBSPREDCOV',READ_OBSPREDCOV)
    ENDIF
    !
    ! Find next data block
    !   PREDICTIONS_NONLINEAR_UNCERTAINTY block
    NPREDI = 0
    CALL UTL_READBLOCK(PREDICOLS,'PREDICTION_LIST',PREDICOL, &
         INUNIT,IOUT,'PREDNAME',.FALSE.,PREDIHEAD,TAIL,NPREDI)
    IF (NPREDI>0) THEN
      CALL UTL_FILTERLIST(PREDIHEAD,IOUT,'PREDNAME',NPREDI,IERR, &
                          GNAM1,MORE)
      DO I=1,NPREDI
        GNAM(I) = GNAM1(I)
      ENDDO
      CALL UTL_FILTERLIST(PREDIHEAD,IOUT,'MEASSTATISTIC',NPREDI,IERR, &
                          STATISTIC1,MORE)
      CALL UTL_FILTERLIST(PREDIHEAD,IOUT,'MEASSTATFLAG',NPREDI,IERR, &
                          SF1,MORE)
      IF (IERR .NE. 0) CALL UTL_STOP()
      DO I=1,NPREDI
        CALL UTL_CASETRANS(SF1(I),'hi')
        IF (STATISTIC1(I) == -1.D-32) THEN
          SF1(I) = 'VAR'
        ENDIF
        IF(STATISTIC1(I) > -1.D-32) THEN
          IF(SF1(I) == 'XXXXXX') THEN
            WRITE(IOUT,90)TRIM(GNAM(I))
            CALL UTL_STOP('Correct MeasStatFlag input in fn.corfac')
          ELSEIF(TRIM(SF1(I)) .NE. 'VAR') THEN
            IF(TRIM(SF1(I)) .NE. 'SD') THEN
              WRITE(IOUT,80)TRIM(GNAM(I))
              CALL UTL_STOP('Correct MeasStatFlag input in fn.corfac')
            ENDIF
          ENDIF
        ENDIF
        IF (STATISTIC1(I) > 0.D0) THEN
          CALL UTL_CALCWT(IOUT,GNAM(I),SF1(I),STATISTIC1(I),1.D0,   &
                            1.D0,IERR,WEIGHT,VP(I))
          IF (IERR.NE.0) CALL UTL_STOP()
        ELSE
          WEIGHT = 0.D0
          VP(I) = 0.D0
        ENDIF
      ENDDO
    ENDIF
    !
    ! Find next data block
    !   PARAMETERS_NONLINEAR_UNCERTAINTY block
    NPARI = 0
    CALL UTL_READBLOCK(PARAMICOLS,'PARAMETER_LIST',PARAMICOL, &
         INUNIT,IOUT,'PARAMETERNAME',.FALSE.,PARAMIHEAD,TAIL,NPARI)
    IF (NPARI>0) THEN
      CALL UTL_FILTERLIST(PARAMIHEAD,IOUT,'PARAMETERNAME',NPARI,IERR, &
                          GNAM2,MORE)
      DO I=NPREDI+1,NPREDI+NPARI
        GNAM(I) = GNAM2(I-NPREDI)
      ENDDO
      CALL UTL_FILTERLIST(PARAMIHEAD,IOUT,'MEASSTATISTIC',NPARI,IERR, &
                          STATISTIC2,MORE)
      CALL UTL_FILTERLIST(PARAMIHEAD,IOUT,'MEASSTATFLAG',NPARI,IERR, &
                          SF2,MORE)
      DO I=1,NPARI
        J = I + NPREDI
        CALL UTL_CASETRANS(SF2(I),'hi')
        IF (STATISTIC2(I) == -1.D-32) THEN
          SF2(I) = 'VAR'
        ENDIF
        IF(STATISTIC2(I) > -1.D-32) THEN
          IF(SF2(I) == 'XXXXXX') THEN
            WRITE(IOUT,90)TRIM(GNAM(I))
            CALL UTL_STOP('Correct MeasStatFlag input in fn.corfac')
          ELSEIF(TRIM(SF2(I)) .NE. 'VAR') THEN
            IF(TRIM(SF2(I)) .NE. 'SD') THEN
              WRITE(IOUT,80)TRIM(GNAM(J))
              CALL UTL_STOP('Correct MeasStatFlag input in fn.corfac')
            ENDIF
          ENDIF
        ENDIF
        IF (STATISTIC2(I) > 0.D0) THEN
          CALL UTL_CALCWT(IOUT,GNAM(I),SF2(I),STATISTIC2(I),1.D0,   &
                            1.D0,IERR,WEIGHT,VP(J))
          IF (IERR.NE.0) CALL UTL_STOP()
        ELSE
          WEIGHT = 0.D0
          VP(J) = 0.D0
        ENDIF
      ENDDO
    ELSE
      IF(IVERB > 1) THEN
        WRITE(IOUT,*)
        WRITE(IOUT,100) &
                     '*********************************************************'
        WRITE(IOUT,100)'  NOTE PARAMETER_LIST BLOCK NOT FOUND          '
        WRITE(IOUT,100) &
                     'CORRECTION FACTORS FOR PARAMETERS WILL NOT BE CALCULATED.'
        WRITE(IOUT,100)'   THIS MAY BE THE DESIRED OPTION.  IF NOT, THEN'
        WRITE(IOUT,100)'   TO READ THIS BLOCK, CHECK BLOCK NAME SPELLING'
        WRITE(IOUT,100) &
                     '*********************************************************'
      ENDIF
    ENDIF
    ! Find next data
    !   TRUE_ERROR_COVARIANCE_STRUCTURE, MATRIX block
    !
    IF(ITRAN) THEN
      K = 0
      CALL UTL_READBLOCK(NOCOLS,'MATRIX_FILES',NOCOL,INUNIT,IOUT,'MATRIXFILE', &
                         .TRUE.,LLPTRMATFIL,TAIL,K)
      IF(K == 0)CALL UTL_STOP('CANNOT FIND matrix in file')
      IF (IVERB>3) THEN
        !   Write block information (correction factor) to output file
        WRITE(IOUT,'(A)')HYPHENS(1:80)
        WRITE(IOUT,100)'Echo MATRIX_FILES input:'
        CALL UTL_WRITEBLOCK(LLPTRMATFIL,IOUT)
      ENDIF
      ! open matirx file
      CALL UTL_FILTER(IERR,LLPTRMATFIL,IOUT,'MATRIXFILE',FN,K)
      INMATRIX = UTL_GETUNIT(7,100)
      OPEN (INMATRIX,FILE=FN,STATUS='OLD',ERR=10)
      ! read V
      CALL TYP_NULL(VCDM)
      CALL UTL_READMATRIX(INMATRIX,IOUT,VCDM)
      V = VCDM
      CALL TYP_DEALLOC(VCDM)
      ! If prediction intervals also need CP
      IF(NPREDI > 0) THEN
        IF(READ_OBSPREDCOV) THEN
          CALL TYP_NULL(CPCDM)
          CALL UTL_READMATRIX(INMATRIX,IOUT,CPCDM)
          CP = CPCDM
          CALL TYP_DEALLOC(CPCDM)
        ENDIF
      ENDIF
    ELSE
      WRITE(IOUT,*)' USER WILL NOT SUPPLY True Error Cavariance Matrix'
    ENDIF
    DEALLOCATE(GNAM1,GNAM2,SF1,SF2,STATISTIC1,STATISTIC2)
    CALL TYP_DEALLOC(CORFACHEAD)
    CALL TYP_DEALLOC(LLPTRMATFIL)
    CALL TYP_DEALLOC(PARAMIHEAD)
    CALL TYP_DEALLOC(PREDIHEAD)
    RETURN
    10 CONTINUE
    CALL TYP_DEALLOC(CORFACHEAD)
    CALL TYP_DEALLOC(LLPTRMATFIL)
    CALL TYP_DEALLOC(PARAMIHEAD)
    CALL TYP_DEALLOC(PREDIHEAD)
    CALL UTL_STOP('Error reading file specified in MATRIX_FILES block')
    DEALLOCATE(GNAM1,GNAM2,SF1,SF2,STATISTIC1,STATISTIC2)
    RETURN
  END SUBROUTINE CORFAC_INI2
!===============================================================================
!===============================================================================
!     Last change:  SC   30 Oct 2003    4:05 pm, EPP 9/9/2005 10:52PM
  SUBROUTINE HATMAT(NOBS,NPE,NTOT,MPR,X,CF,CFSM,D,TOCF,TPCF)
  !
  !     MODIFIED VERSION 1001 24OCT2003 (SC)
  !     ******************************************************************
  !     COMPUTES THE OBSERVATION BLOCK OF THE HAT MATRIX, CF, THE TRACES
  !     OF THE OBSERVATION AND PRIOR BLOCKS OF THE I-HAT MATRIX, TOCF AND
  !     TPCF, AND THE SUM OF ELEMEMTS OF THE I-HAT MATRIX, CFSM.
  !     IN RETURN D=(X'*W*X)^-1*X'*W
  !     ******************************************************************
  !     SPECIFICATIONS:
  !     ------------------------------------------------------------------
  IMPLICIT NONE
  INTEGER,               INTENT(IN)  :: NOBS
  INTEGER,               INTENT(IN)  :: NPE
  INTEGER,               INTENT(IN)  :: NTOT
  INTEGER,               INTENT(IN)  :: MPR
  DOUBLE PRECISION,      INTENT(IN)  :: X(NPE,NTOT)
  DOUBLE PRECISION,      INTENT(OUT) :: CF(NOBS,NOBS)
  DOUBLE PRECISION,      INTENT(OUT) :: CFSM
  DOUBLE PRECISION,      INTENT(OUT) :: D(NPE,NTOT)
  DOUBLE PRECISION,      INTENT(OUT) :: TOCF
  DOUBLE PRECISION,      INTENT(OUT) :: TPCF
  ! Local variables
  DOUBLE PRECISION, ALLOCATABLE      :: C(:,:)
  DOUBLE PRECISION                   :: DTMP
  INTEGER                            :: I
  INTEGER                            :: J
  INTEGER                            :: IP
  INTEGER                            :: ISTOP
  INTEGER                            :: JP
  DOUBLE PRECISION, ALLOCATABLE      :: SCLE(:)
  DOUBLE PRECISION                   :: SUM
  !     ------------------------------------------------------------------
  ALLOCATE (SCLE(NPE), C(NPE,NPE))
  !
  !     COMPUTE C=X'*X AND FACTOR IT
  C = 0.D0
  DO I = 1, NTOT
    DO IP = 1, NPE
      DTMP = X(IP,I)
      DO JP = IP, NPE
        C(IP,JP) = C(IP,JP) + DTMP * X(JP,I)
      ENDDO
    ENDDO
  ENDDO
  DO IP = 1, NPE
    SCLE(IP) = DSQRT(C(IP,IP))
  ENDDO
  IF(NPE.GT.1) THEN
    DO IP = 1, NPE-1
      DTMP = SCLE(IP)
      DO JP = IP+1, NPE
        C(IP,JP) = C(IP,JP) / (SCLE(JP) * DTMP)
        C(JP,IP) = C(IP,JP)
      ENDDO
      C(IP,IP)=1.D00
    ENDDO
  ENDIF
  C(NPE,NPE)=1.D00
  ! FACTOR C
  CALL SOLVE(NPE, NPE, 1, C, D, D, ISTOP, DTMP)
  IF (ISTOP.GT.0) STOP ' C MATRIX SINGULAR'
  !
  ! COMPUTE D
  DO I = 1, NTOT
    DO IP = 1, NPE
      D(IP,I) = X(IP,I) / SCLE(IP)
    ENDDO
    CALL SOLVE(NPE, NPE, 0, C, D(1,I), D(1,I), ISTOP, DTMP)
    DO IP = 1, NPE
      D(IP,I) = D(IP,I) / SCLE(IP)
    ENDDO
  ENDDO
  !
  DEALLOCATE (SCLE, C)
  !
  ! COMPUTE CF, TOCF, AND CFSM
  TOCF = 0.D0
  CFSM = 0.D0
  DO I = 1, NOBS
    DO J = 1, NOBS
      SUM = 0.D0
      DO IP = 1, NPE
        SUM = SUM + X(IP,I)*D(IP,J)
      ENDDO
      CF(I,J) = SUM
      CFSM = CFSM + SUM
    ENDDO
    TOCF = TOCF + CF(I,I)
  ENDDO
  TOCF = NOBS - TOCF
  CFSM = NOBS - CFSM
  !
  !     COMPUTE TPCF
  TPCF = 0.D0
  DO I = NOBS+1, NTOT
    SUM = 0.D0
    DO IP = 1, NPE
      SUM = SUM + X(IP,I)*D(IP,I)
    ENDDO
    TPCF = TPCF + SUM
  ENDDO
  TPCF = MPR - TPCF
  !
  RETURN
  END SUBROUTINE HATMAT
!===============================================================================
!===============================================================================
  SUBROUTINE SOLVE(NPE,NPD,IND,C,D,G,ISTOP,DET)
  !-----VERSION 1001 25FEB2003 (SC) - EPP 9/9/2005 10:52PM
  !     ******************************************************************
  !         SOLVE SYSTEM OF LINEAR EQUATIONS BY LU-DECOMPOSITION
  !     ******************************************************************
  !        SPECIFICATIONS:
  !     ------------------------------------------------------------------
  INTEGER,                           INTENT(IN)    :: NPD
  INTEGER,                           INTENT(IN)    :: NPE
  INTEGER,                           INTENT(IN)    :: IND
  DOUBLE PRECISION,                  INTENT(INOUT) :: C(NPD,NPD)
  DOUBLE PRECISION,                  INTENT(INOUT) :: D(NPD)
  DOUBLE PRECISION,                  INTENT(INOUT) :: G(NPD)
  INTEGER,                           INTENT(INOUT) :: ISTOP
  DOUBLE PRECISION,                  INTENT(INOUT) :: DET
  ! Local variables
  DOUBLE PRECISION                                 :: DPIV
  DOUBLE PRECISION                                 :: DSUM
  DOUBLE PRECISION                                 :: DTMPA
  INTEGER                                          :: I
  INTEGER                                          :: IP1
  INTEGER                                          :: J
  INTEGER                                          :: K
  INTEGER                                          :: KP1
  INTEGER                                          :: NM1
  !     ------------------------------------------------------------------
  !         COMPUTE TRIAL PARAMETER STEP LENGTHS USING LDU FACTORIZATION:
  !           DECOMPOSE MATRIX
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
    IF (IND.LT.2) RETURN
  ENDIF
  !
  !-----------INITIALIZE D
  DO K=1,NPE
    D(K)=G(K)
  ENDDO
  !
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
    DSUM=0.D0
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
  SUBROUTINE TRANSCP(NTOT,WTMATSQR,CP)
  !
  USE DATATYPES
  USE UTILITIES
  !     ******************************************************************
  !     TRANSFORMS CP BY MULTIPLYING TWICE WITH SQUARE-ROOT WEIGHTS TO
  !     BECOME W*CP*W
  !     ******************************************************************
  !     SPECIFICATIONS:
  !     ------------------------------------------------------------------
  INTEGER,          INTENT(IN)    :: NTOT
  TYPE(CDMATRIX),   INTENT(IN)    :: WTMATSQR
  DOUBLE PRECISION, INTENT(INOUT) :: CP(NTOT)
  DOUBLE PRECISION                :: SUM
  DOUBLE PRECISION                :: X(NTOT)
  ! Local variables
  INTEGER                         :: I
  INTEGER                         :: J
  !     ------------------------------------------------------------------
  DO I=1,NTOT
    SUM = 0.D0
    DO J=1,NTOT
      SUM = SUM + UTL_GETVAL(WTMATSQR,I,J) * CP(J)
    ENDDO
    X(I) = SUM
  ENDDO
  DO I =1, NTOT
    CP(I) = X(I)
  ENDDO
  !
  RETURN
  END SUBROUTINE TRANSCP
!===============================================================================
!===============================================================================
  SUBROUTINE PRTOTB_LENDNAM(NO,IOUT,VAL,VID)
  USE GLOBAL_DATA, ONLY: LENDNAM
  !**PRINT VALUES IN TWO GROUPS OF THREE COLUMNS
  IMPLICIT NONE
  INTEGER,             INTENT(IN)             :: NO
  INTEGER,             INTENT(IN)             :: IOUT
  DOUBLE PRECISION,    INTENT(IN)             :: VAL(NO)
  CHARACTER(LEN=LENDNAM),   INTENT(IN)        :: VID(NO)
  ! local variables
  INTEGER                                     :: K
  INTEGER                                     :: L
  INTEGER                                     :: NR
  ! formats
  500 FORMAT (1X,2(I3,1X,A,1X,G11.5,2X))
  !
  NR = NO/2
  IF (2*NR .NE. NO) NR = NR + 1
  DO K = 1, NR
    WRITE (IOUT,500) (L,VID(L),VAL(L),L=K,NO,NR)
  ENDDO
  RETURN
  END SUBROUTINE PRTOTB_LENDNAM
!===============================================================================
!===============================================================================
  SUBROUTINE PRTOTX(NC,NR,IOUT,OBSNAM,PARNAM,X)
  USE GLOBAL_DATA, ONLY: LENDNAM
  !**PRINT MATRICES DIVIDED VERTICALLY INTO Two-COLUMN BLOCKS
  IMPLICIT NONE
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
  SUBROUTINE TRANSV(NQ,WTMATSQR,V)
  USE DATATYPES
  USE UTILITIES
  !
  !     VERSION 1001 24OCT2003 (SC) EPP 9/9/2005 10:52PM
  !     ******************************************************************
  !     TRANSFORMS V BY MULTIPLYING TWICE WITH SQUARE-ROOT WEIGHTS TO
  !     BECOME W*V*W
  !     ******************************************************************
  !     SPECIFICATIONS:
  !     ------------------------------------------------------------------
  IMPLICIT NONE
  INTEGER,            INTENT(IN)    :: NQ
  TYPE (CDMATRIX),    INTENT(IN)    :: WTMATSQR
  DOUBLE PRECISION,   INTENT(INOUT) :: V(NQ,NQ)
  ! Local variables
  DOUBLE PRECISION                  :: DTMPA
  INTEGER                           :: I
  INTEGER                           :: J
  INTEGER                           :: K
  INTEGER                           :: L
  DOUBLE PRECISION, ALLOCATABLE     :: X(:,:)
  !     ------------------------------------------------------------------
  !
  ALLOCATE(X(NQ,NQ))
  X = 0.D0
  DO J = 1, NQ
    DO K = 1, NQ
      DTMPA = 0.D0
      DO L = 1, NQ
        DTMPA = DTMPA + V(K,L)*UTL_GETVAL(WTMATSQR,L,J)
      ENDDO
      DO I = J, NQ
        X(I,J) = X(I,J) + UTL_GETVAL(WTMATSQR,I,K)*DTMPA
      ENDDO
    ENDDO
  ENDDO
  DO J = 1, NQ
    K = J
    V(K,K) = X(J,J)
    DO I = J+1, NQ
      L = I
      V(L,K) = X(I,J)
      V(K,L) = V(L,K)
    ENDDO
  ENDDO
  DEALLOCATE (X)
  !
  RETURN
  END SUBROUTINE TRANSV
!===============================================================================
!===============================================================================
