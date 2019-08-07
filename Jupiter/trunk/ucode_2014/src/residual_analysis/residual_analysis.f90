PROGRAM RESIDUAL_ANALYSIS
! Jupiter API modules
USE GLOBAL_DATA, ONLY: AMESSAGE, LENDNAM, MAX_STRING_LEN, VERSIONID
USE DATATYPES
USE SENSITIVITY, ONLY: SEN_UEV_DX_READ_SU
USE STATISTICS, ONLY: STA_EVA_PROB_NORM_DISTRB, STA_UEV_DX_READ_DM
USE UTILITIES
USE UTLUCODE
  !       RESIDUALS ANALYSIS PROGRAM BY R. L. COOLEY, USGS, DENVER, COLO.
  !       MODIFIED FOR MODFLOWP BY MARY C. HILL, USGS, DENVER, COLO.
  !          NOT ADAPTED FOR DRY OBSERVATIONS
  ! ***      modified to compute Cook's D and DFBETAS
  ! ***      by Richard M. Yager, Ithaca, New York
  ! ***      changes begin and end with c ***
  ! ***      f77 resanp.f -o resanp
  !       Modified to work with MODFLOW-2000 by E.R. Banta, 8/12/1999
  !    Modified for Jupiter compatibility by E.P.Poeter, July 2004
  !    Modified to use _presvd files by E.P.Poeter, February 2012
  IMPLICIT NONE
  CHARACTER(LEN=3)                                  :: ANS(12)
  CHARACTER(LEN=4)                                  :: BLANK
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)      :: BUFF2
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)    :: C
  DOUBLE PRECISION                                  :: CC
  DOUBLE PRECISION                                  :: CCD
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)      :: CD
  CHARACTER (LEN=12)                                :: CDUM
  CHARACTER(LEN=10)                                 :: CHDATE
  CHARACTER(LEN=10)                                 :: CHTIME
  CHARACTER(LEN=10)                                 :: CHZONE
  INTEGER                                           :: CNT
  CHARACTER(LEN=25)                                 :: COL12NAM(2)
  CHARACTER (LEN=3)                                 :: CONVERGE
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)    :: COV
  DOUBLE PRECISION                                  :: CUTOFF
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)      :: D
  DOUBLE PRECISION                                  :: DFB1
  DOUBLE PRECISION                                  :: DFB2
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)    :: DFBETA
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: DID
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: DID1
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)      :: E
  CHARACTER(LEN=7)                                  :: EXT
  CHARACTER(LEN=9)                                  :: FNEXTW
  CHARACTER(LEN=10)                                 :: FNEXTMV
  CHARACTER(LEN=10)                                 :: FNEXTWT
  CHARACTER(LEN=13)                                 :: FNEXTWTPRI
  CHARACTER(LEN=10)                                 :: FNEXTSU
  CHARACTER(LEN=13)                                 :: FNEXTSUPRI
  CHARACTER(LEN=11)                                 :: FNEXTRD
  CHARACTER(LEN=11)                                 :: FNEXTRG
  CHARACTER(LEN=11)                                 :: FNEXTRC
  CHARACTER(LEN=11)                                 :: FNEXTRB
  CHARACTER(LEN=10)                                 :: FNEXTRS
  CHARACTER(LEN=14)                                 :: FNEXTPRESAN
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)      :: F
  CHARACTER(LEN=MAX_STRING_LEN)                     :: FN
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)      :: G
  INTEGER                                           :: I
  INTEGER                                           :: IBAD
  INTEGER                                           :: IBDT(8)
  INTEGER                                           :: ICOOK
  INTEGER                                           :: IDFB
  INTEGER                                           :: IDUM
  INTEGER                                           :: IERR
  INTEGER                                           :: IFAIL
  INTEGER                                           :: IIN
  INTEGER                                           :: INWR
  INTEGER                                           :: IOS
  INTEGER                                           :: IOUT
  INTEGER                                           :: IPLOTD
  INTEGER                                           :: IPLOTG
  INTEGER                                           :: IPM
  INTEGER, ALLOCATABLE,  DIMENSION(:)               :: IPTR
  INTEGER, ALLOCATABLE,  DIMENSION(:)               :: IPTRDID
  LOGICAL                                           :: ISKIP
  INTEGER                                           :: ISTAT
  INTEGER, ALLOCATABLE,  DIMENSION(:)               :: ISYM
  INTEGER, ALLOCATABLE,  DIMENSION(:)               :: ISYM1
  INTEGER                                           :: ISYMTMP
  INTEGER                                           :: J
  INTEGER                                           :: K
  INTEGER                                           :: KK
  LOGICAL                                           :: LEX = .FALSE.
  INTEGER, ALLOCATABLE,  DIMENSION(:)               :: LOGT
  INTEGER                                           :: MPR
  INTEGER                                           :: N
  INTEGER                                           :: N1
  INTEGER                                           :: N2
  INTEGER                                           :: NCYC
  INTEGER                                           :: NCYCLE
  INTEGER                                           :: NITER
  INTEGER                                           :: NOBS
  INTEGER                                           :: NORIG
  INTEGER                                           :: NP
  INTEGER                                           :: NPE
  INTEGER                                           :: NPS
  INTEGER                                           :: NRAN
  INTEGER                                           :: NRESCODE
  INTEGER                                           :: NSETS
  INTEGER                                           :: NTOT
  INTEGER                                           :: NUM
  INTEGER                                           :: NUMSTRIP
  INTEGER                                           :: NUMSTRIP1
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: OBSNAM
  CHARACTER(LEN=MAX_STRING_LEN)                     :: OUTNAM
  CHARACTER(LEN=12), ALLOCATABLE,  DIMENSION(:)     :: PARNAM
  CHARACTER(LEN=12), ALLOCATABLE,  DIMENSION(:)     :: PANAM
  CHARACTER(LEN=12), ALLOCATABLE,  DIMENSION(:)     :: PANAMNPE
  INTEGER, ALLOCATABLE,  DIMENSION(:)               :: PINC
  INTEGER, ALLOCATABLE,  DIMENSION(:)               :: PLOTSYMBOL
  INTEGER, ALLOCATABLE,  DIMENSION(:)               :: PLOTSYMBOLPRI
  LOGICAL                                           :: PRCOVPARAM
  LOGICAL                                           :: PRSQRWT
  LOGICAL                                           :: PRSENSOPTPARAM
  LOGICAL                                           :: PR_RC
  LOGICAL                                           :: PR_RC_EACH
  LOGICAL                                           :: PR_RB
  LOGICAL                                           :: PR_CALCDEV
  LOGICAL                                           :: PR_DEVRS
  LOGICAL                                           :: PR_RD
  LOGICAL                                           :: PR_RG
  LOGICAL                                           :: PRCOVRESID
  LOGICAL                                           :: PRCORRESID
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: PRINAM
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)    :: R
  DOUBLE PRECISION                                  :: RANUM
  DOUBLE PRECISION                                  :: RDUM
  TYPE (LLIST), POINTER                             :: RESANHEAD
  CHARACTER(LEN=40), DIMENSION(1)                   :: RESANCOL = (/ '      '/)
  DOUBLE PRECISION                                  :: RR
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)      :: S
  DOUBLE PRECISION                                  :: SIGMA
  CHARACTER(LEN=40)                                 :: STRIP2
  DOUBLE PRECISION                                  :: SUM
  TYPE (LLIST), POINTER                             :: TAIL
  DOUBLE PRECISION                                  :: TEMP
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)      :: TEMPPVAL
  DOUBLE PRECISION                                  :: TMP
  CHARACTER(LEN=LENDNAM)                                 :: TMPDID
  DOUBLE PRECISION                                  :: VAR
  CHARACTER(LEN=40)                                 :: VERSION
  CHARACTER(LEN=20)                                 :: VERSIONMIN
  TYPE(CDMATRIX)                                    :: WTMAT
  TYPE(CDMATRIX)                                    :: WTMATSQR
  TYPE(CDMATRIX)                                    :: WTMATDEP
  TYPE(CDMATRIX)                                    :: WTMATDEPSQR
  TYPE(CDMATRIX)                                    :: PRIWTMAT
  TYPE(CDMATRIX)                                    :: PRIWTMATSQR
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)    :: X
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)    :: X0
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)    :: XORIG
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)    :: XPRI
  !
  PARAMETER (VERSION='1.009') !  02.2012
  ! Formats
  481 FORMAT (' !!!! ERROR MISSING ROOTFILENAME ON RESIDUAL_ANALYSIS ', &
              'COMMAND LINE !!!')
  581 FORMAT (' !!!! ERROR ONLY ACCEPTABLE SECOND ARGUMENT ON ', &
              'RESIDUAL_ANALYSIS COMMAND LINE IS _presvd !!!')
  900 FORMAT(/,1X,80('*'),/,2X, &
             'Normal termination of RESIDUAL_ANALYSIS, Version: ',A, &
             /,1X,80('*'),//)
  1030 FORMAT (//, &
               ' NUMBER OF ESTIMATED PARAMETERS.............. ',I6,/, &
               ' NUMBER OF OBSERVATIONS ..................... ',I6,/, &
               ' NUMBER OF PRIOR EQUATIONS................... ',I6,/, &
               ' NUMBER OF SETS OF RANDOM NUMBERS ........... ',I6,/, &
               ' NUMBER FOR RANDOM NUMBER GENERATOR ......... ',I11,/, &
               ' CALCULATED ERROR VARIANCE .................. ',G13.6)
  1039 FORMAT(/,1X,'WEIGHT MATRIX FOR OBSERVATIONS WILL BE READ FROM ', &
                 'FILE: ',/,20X,A)
  1060 FORMAT (/,' COVARIANCE MATRIX FOR ESTIMATED PARAMETERS')
  1061 FORMAT (/,'  Default, or .rs File indicated NOT to print',/,11X, &
               'COVARIANCE MATRIX FOR ESTIMATED PARAMETERS')
  1065 FORMAT (/,'  Default, or .rs File indicated NOT to print',/,11X, &
                  'SQUARE ROOT OF FULL WEIGHT MATRIX,',/, &
                  11X,'IT CAN BE FOUND IN _WT',/)
  1070 FORMAT (/,1X,'SENSITIVITIES FOR OPTIMUM PARAMETERS')
  1071 FORMAT (/,1X,' Default, or .rs File indicated NOT to print',/,11X, &
                     'SENSITIVITIES FOR OPTIMUM PARAMETERS')
  1100 FORMAT (/,1X,'COVARIANCE MATRIX FOR RESIDUALS')
  1105 FORMAT (/,1X,' Default, or .rs File indicated NOT to print',/,11X, &
                     'COVARIANCE MATRIX FOR RESIDUALS')
  1120 FORMAT (/,1X,'CORRELATION MATRIX FOR RESIDUALS')
  1125 FORMAT (/,1X,' Default, or .rs File indicated NOT to print',/,11X, &
                     'CORRELATION MATRIX FOR RESIDUALS')
  1150 FORMAT(//,' ERROR OPENING FILE "',A,'" -- STOP EXECUTION',//, &
       ' NOTE: DATA-EXCHANGE FILES FROM A SUCCESSFUL REGRESSION WITH THE'/, &
       '    ROOT FILENAME SPECIFIED ON THE COMMAND LINE MUST BE PRESENT',/, &
       '    IN THE DIRECTORY FOR INPUT TO RESIDUAL_ANALYSIS',/, &
       '    If that has been done, check that the root filename on',/ &
       '    the RESIDUAL_ANALYSIS command line is correct.',/)
  1160 FORMAT (31X,'RESIDUAL_ANALYSIS',//, &
       24X,'CONSTRUCTED USING THE JUPITER API',/, &
       25X,'UCODE POST-PROCESSING PROGRAM',/, &
       24X,'TO PERFORM ANALYSIS OF RESIDUALS',/, &
       32X,'Version ',A/)
  1190 FORMAT(1X,'"ORDERED INDEPENDENT RANDOM NUMBER"',2X, &
       '"STANDARD NORMAL STATISTIC"',2X, &
       '"PLOT SYMBOL"',2X,'"OBSERVATION or PRIOR NAME"',2X, &
       '"RANDOM NUMBER SET NO. "',I3)
  1210 FORMAT(5X,G15.3,15X,G15.3,15X,I6,6X,A)
  1220 FORMAT(A4)
  1225 FORMAT(/,1X,' Default, or .rs File indicated NOT to print the _rd file')
  1226 FORMAT(/,1X,' Default, or .rs File indicated NOT to print the _rg file')
  1230 FORMAT(1X,'WEIGHTED RESIDUALS WILL BE READ FROM FILE ',A)
  1235 FORMAT(1X,/,'User selections:',/, &
   1X, 'Print covariance matrix for estimated parameters to fn.#rs? ',A3,/, &
   1X, 'Print square root of full weight matrix to fn.#rs?          ',A3,/, &
   1X, 'Print sensitivities for optimum parameters to fn.#rs?       ',A3,/, &
   1X, 'Compute Cook''s D statistic?                                 ',A3,/, &
   1X, 'Print Cook''s D for each observation to fn.#rs?              ',A3,/, &
   1X, 'Compute DFBeta''s?                                           ',A3,/, &
   1X, 'Calculate random numbers?                                   ',A3,/, &
   1X, 'Print random numbers to fn.#rs?                             ',A3,/, &
   1X, 'Print _rd file (random numbers)?                            ',A3,/, &
   1X, 'Print _rg file (correlated random numbers)?                 ',A3,/, &
   1X, 'Print covariance matrix for residuals to fn.#rs?            ',A3,/, &
   1X, 'Print correlation matrix for residuals to fn.#rs?           ',A3,/)
  1240 FORMAT(1X,'"ORDERED CORRELATED NUMBER"',2X, &
       '"STANDARD NORMAL STATISTIC"',2X, &
       '"PLOT SYMBOL"',2X,'"OBSERVATION or PRIOR NAME"',2X, &
       '"RANDOM NUMBER SET NO. "',I3)
  1260 FORMAT (/,' COOK''S D STATISTIC'//, &
         ' OBSERVATION',2X,'PLOT-SYMBOL',2X, &
         'STUDENT_RESIDUAL  VAR(Y)/VAR(E)',3X, &
         'COOK''S D')
  1265 FORMAT (/,'  Default, or .rs File indicated NOT to print',/,11X, &
                     'COOK''S D FOR EACH OBSERVATION in the #rs file'/)
  1266 FORMAT (/,'  Default, or .rs File indicated NOT to Calculate ', &
                 'COOK''S D STATISTICS',/)
  1270 FORMAT (1X,A,4X,I4,10X,G10.4,1X,2(4X,G10.4))
  1280 FORMAT (G15.8,2X,A,2X,I9)
  1290 FORMAT (' ANALYSIS OF COOKS D',/, &
         ' FOR PLOTTING, COOKS D STATISTICS ARE LISTED IN THE _RC OUTPUT' &
         ,' FILE',//, &
         ' INFLUENTIAL OBSERVATIONS WITH COOKS D > CRITICAL VALUE ', &
         '(4/(NOBS+MPR)) = ',F5.3// &
         ' OBS# OBSERVATION      PLOT-SYMBOL      COOK''S D')
  1291 FORMAT (1X,/,80('*'))
  1295 FORMAT (/,' Default or Input indicated NOT to Calculate Random Numbers')
  1300 FORMAT (3X,'"COOK''S D"',4X,'"OBSERVATION or PRIOR NAME"  "PLOT SYMBOL"')
  1310 FORMAT (I5,1X,A,4X,1I4,4X,E18.8)
  1320 FORMAT (/,' NUMBER OF INFLUENTIAL OBSERVATIONS IDENTIFIED:',I5,/)
  1330 FORMAT (' ANALYSIS USING DFBETAS',/, &
         ' FOR PLOTTING, DFBETA STATISTICS ARE LISTED IN THE _RB OUTPUT ' &
         ,'FILE',/)
  1331 FORMAT (' PARAMETER NUMBERS AND NAMES:',/)
  1332 FORMAT  (5I13)
  1333 FORMAT  (11X,5(1X,A12))
  1334 FORMAT (' INFLUENTIAL OBSERVATIONS WITH DFBETA >',/, &
               '                                      CRITICAL VALUE ', &
         '(2/(NOBS+MPR)**0.5) = ',F5.3//, &
         ' PARAMETERS INFLUENCED IDENTIFIED BY #'//, &
         37X,'PARAMETER NUMBER')
  1335 FORMAT (/,' OBS# ID               PLOT-SYMBOL',2X,10I4)
  1340 FORMAT (I5,1X,A,4X,1I4,2X,A)
  1375 FORMAT(/,1X,' Default, or .rs File indicated NOT to Calculate DFBeta''s')
  1380 FORMAT(/,' The following files have been prepared: ')
  1381 FORMAT(3X,A)
  1390 FORMAT (1X,A,4X,I4,10X,'RR OUT OF RANGE: ',G25.16)
  1400 FORMAT (//,' ***SETS OF RANDOM NUMBERS***',//, &
        ' FOR PLOTTING, SETS OF INDEPENDENT RANDOM NUMBERS ARE LISTED ', &
        'IN THE _RD OUTPUT FILE,',/, &
        ' SETS OF CORRELATED RANDOM NUMBERS ARE LISTED IN THE _RG ', &
        'OUTPUT FILE')
  1405 FORMAT (/,1X,' Default, or .rs File indicated NOT to print',/,11X, &
                     'SETS OF RANDOM NUMBERS to the #rs file')
  1430 FORMAT(1X,'DFBETAS FOR THE FOLLOWING OBSERVATIONS CANNOT BE', &
       ' CALCULATED, PROBABLY',/,' DUE TO A COMBINATION OF MODEL', &
       ' NONLINEARITY AND LARGE LEVERAGE:')
  1440 FORMAT(5X,A)
  1450 FORMAT(/,' *** DUE TO PRECISION LIMITATION, CORRELATION', &
                ' MATRIX CANNOT BE CALCULATED')
  1500 FORMAT(/,17X,' *** RESIDUAL_ANALYSIS CANNOT BE PERFORMED *** ',/, &
                17X,'    THE NUMBER OF OBSERVATIONS IS TOO LARGE   ',/, &
                17X,'                 TERMINATING                  ')
  !
  CALL DATE_AND_TIME(CHDATE,CHTIME,CHZONE,IBDT)
  !
  !     WRITE PROGRAM NAME AND VERSION TO SCREEN
  WRITE (*,*)
  WRITE (*,1160) VERSION
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
  ! ***
  !**DEFINE AND OPEN INPUT FILES AND OUTPUT FILE, AND DIMENSION ARRAYS
  !   FOR SUBROUTINE PRTOT
  ! GET ROOT NAME AND OPEN GENAL OUTPUT FILE
  OUTNAM = UTL_GETARG(1)
  IOUT = UTL_GETUNIT(7,100)
  IF(OUTNAM .EQ. ' ') THEN
    OPEN (IOUT,FILE='RESIDUAL_ANALYSIS.ERR',STATUS='UNKNOWN')
    WRITE(*,481)
    WRITE(IOUT,481)
    CLOSE(IOUT)
    CALL UTL_STOP &
    (' PLEASE INCLUDE A ROOTFILENAME on the RESIDUAL_ANALYSIS COMMAND LINE')
  ENDIF
  EXT = UTL_GETARG(2) ! Get command-line arg for input file
  CALL UTL_CASE(EXT,EXT,-1)
  IF(EXT .EQ. ' ') THEN
    ! analyze standard ucode output
    FNEXTW = '_w'
    FNEXTMV = '_mv'
    FNEXTWT = '_wt'
    FNEXTWTPRI = '_wtpri'
    FNEXTSU = '_su'
    FNEXTSUPRI = '_supri'
    FNEXTRD = '._rd'
    FNEXTRG = '._rg'
    FNEXTRC = '._rc'
    FNEXTRB = '._rb'
    FNEXTRS = '.rs'
    FNEXTPRESAN = '.#resan'
  ELSEIF(EXT .EQ. 'presvd') THEN
    ! analyze presvd ucode output
    EXT = '_'//TRIM(EXT)
    FNEXTW = '_w'//TRIM(EXT)
    FNEXTMV = '_mv'//TRIM(EXT)
    FNEXTWT = '_wt'//TRIM(EXT)
    FNEXTWTPRI = '_wtpri'//TRIM(EXT)
    FNEXTSU = '_su'//TRIM(EXT)
    FNEXTSUPRI = '_supri'//TRIM(EXT)
    FNEXTRD = '._rd'//TRIM(EXT)
    FNEXTRG = '._rg'//TRIM(EXT)
    FNEXTRC = '._rc'//TRIM(EXT)
    FNEXTRB = '._rb'//TRIM(EXT)
    FNEXTRS = '.rs'//TRIM(EXT)
    FNEXTPRESAN = '.#resan'//TRIM(EXT)
  ELSE
    OPEN (IOUT,FILE='RESIDUAL_ANALYSIS.ERR',STATUS='UNKNOWN')
    WRITE(*,581)
    WRITE(IOUT,581)
    CLOSE(IOUT)
    CALL UTL_STOP &
    (' PLEASE EITHER OMIT SECOND COMMAND LINE ARGUMENT OR CHANGE IT TO _presvd')
  ENDIF
  FN = TRIM(OUTNAM)//TRIM(FNEXTPRESAN)
  OPEN (IOUT,FILE=FN,STATUS='UNKNOWN')
  WRITE(IOUT,1160) VERSION
  BLANK='    '
  !**READ AND PRINT INPUT DATA
  FN = TRIM(OUTNAM)//'.'//TRIM(FNEXTW)
  INWR = UTL_GETUNIT(7,100)
  OPEN (INWR,FILE=FN,STATUS='OLD')
  WRITE(IOUT,1230) TRIM(FN)
  !
  !
    NSETS = 4
    NRAN = 104857
    ANS = '   '
    PRCOVPARAM = .FALSE.
    ANS(1) = ' no'
    PRSQRWT = .FALSE.
    ANS(2) = ' no'
    PRSENSOPTPARAM = .FALSE.
    ANS(3) = ' no'
    PR_RC = .TRUE.
    ANS(4) = 'yes'
    PR_RC_EACH = .FALSE.
    ANS(5) = ' no'
    PR_RB = .TRUE.
    ANS(6) = 'yes'
    PR_CALCDEV = .TRUE.
    ANS(7) = 'yes'
    PR_DEVRS = .FALSE.
    ANS(8) = 'yes'
    PR_RD = .TRUE.
    ANS(9) = 'yes'
    PR_RG = .TRUE.
    ANS(10) = 'yes'
    PRCOVRESID = .FALSE.
    ANS(11) = ' no'
    PRCORRESID = .FALSE.
    ANS(12) = ' no'
    NRESCODE = 0
    COL12NAM(1) = 'OBSERVATION or PRIOR NAME'
    COL12NAM(2) = 'PLOT SYMBOL'
    NULLIFY(RESANHEAD,TAIL)
  ! Get desired # of residual sets & Random # seed
  ! from user created RESIDUAL_ANALYSIS input file OUTNAM..rs)
  !**NOTE: NRAN MUST BE ODD AND MUST LIE BETWEEN 1 AND 1048575
  FN = TRIM(OUTNAM)//TRIM(FNEXTRS)
  INQUIRE(FILE=FN,EXIST=LEX)
    IIN = UTL_GETUNIT(7,100)
  IF(LEX) THEN
    OPEN (IIN,FILE=FN,STATUS='OLD',ACCESS='SEQUENTIAL', &
          FORM='FORMATTED',ERR=10,IOSTAT=IOS)
    CALL UTL_READBLOCK(0,'RESIDUAL_ANALYSIS_CONTROL_DATA',RESANCOL,IIN,IOUT, &
                       '*',.FALSE.,RESANHEAD,TAIL,NRESCODE)
    IF (NRESCODE>0) THEN
      CALL UTL_FILTER(IERR,RESANHEAD,IOUT,'NSETS',NSETS)
      CALL UTL_FILTER(IERR,RESANHEAD,IOUT,'SEED',NRAN)
      CALL UTL_FILTER(IERR,RESANHEAD,IOUT,'PRINT_PAR_VAR_COV_MATRIX',PRCOVPARAM)
      CALL UTL_FILTER(IERR,RESANHEAD,IOUT,'PRINT_SQRT_WT',PRSQRWT)
      CALL UTL_FILTER(IERR,RESANHEAD,IOUT,'PRINT_UNSCALED_SENS',PRSENSOPTPARAM)
      CALL UTL_FILTER(IERR,RESANHEAD,IOUT,'CALC_COOKSD',PR_RC)
      CALL UTL_FILTER(IERR,RESANHEAD,IOUT,'PRINT_EACH_COOKSD',PR_RC_EACH)
      CALL UTL_FILTER(IERR,RESANHEAD,IOUT,'CALC_DFBETA',PR_RB)
      CALL UTL_FILTER(IERR,RESANHEAD,IOUT,'CALC_RANDOMNUMBERS',PR_CALCDEV)
      CALL UTL_FILTER(IERR,RESANHEAD,IOUT,'PRINT_RD',PR_RD)
      CALL UTL_FILTER(IERR,RESANHEAD,IOUT,'PRINT_RG',PR_RG)
      CALL UTL_FILTER(IERR,RESANHEAD,IOUT,'PRINT_RES_VAR_COV_MATRIX',PRCOVRESID)
      CALL UTL_FILTER(IERR,RESANHEAD,IOUT,'PRINT_RES_CORRELATION_MATRIX', &
                                                                     PRCORRESID)
    ENDIF
    IF(PR_RD .OR. PR_RG) PR_DEVRS = .TRUE.
    IF(NRAN < 0) NRAN = -NRAN
    IF(NRAN < 1 .OR. NRAN > 1048575) NRAN = 104857
    IF (MOD(2,NRAN) .EQ. 0.) NRAN = NRAN + 1
    CLOSE(IIN)
  ENDIF
  IF(PRCOVPARAM) ANS(1) = 'yes'
  IF(PRSQRWT) ANS(2) = 'yes'
  IF(PRSENSOPTPARAM) ANS(3) = 'yes'
  IF(.NOT. PR_RC) ANS(4) = ' no'
  IF(PR_RC_EACH) ANS(5) = 'yes'
  IF(.NOT. PR_RB) ANS(6) = ' no'
  IF(.NOT. PR_CALCDEV) ANS(7) = ' no'
  IF(PR_DEVRS) ANS(8) = 'yes'
  IF(.NOT. PR_RD) ANS(9) = ' no'
  IF(.NOT. PR_RG) ANS(10) = ' no'
  IF(PRCOVRESID) ANS(11) = 'yes'
  IF(PRCORRESID) ANS(12) = 'yes'
  WRITE(IOUT,1235) (ANS(I),I=1,12)
  CALL UTLUCODE_DX_READ_DM(IFAIL,OUTNAM,RDUM,RDUM,RDUM, &
                           RDUM,CONVERGE,NITER,RDUM,RDUM, &
                           RDUM,CDUM,CDUM,CDUM,CDUM, &
                           MPR,IDUM,NORIG,NP,NPS, &
                           NOBS,RDUM,RDUM,RDUM,VAR, &
                           IOUT,RDUM,RDUM,CDUM,IDUM, &
                           RDUM,RDUM,EXT)
  IFAIL = 0
  IF(PR_RD) THEN
    !  OPEN FILE FOR NORMAL PROBABILITY GRAPH OF RANDOM NUMBERS
    FN = TRIM(OUTNAM)//TRIM(FNEXTRD)
    IPLOTD = UTL_GETUNIT(50,99)
    OPEN(IPLOTD,FILE=FN,STATUS='UNKNOWN')
  ENDIF
  IF(PR_RG) THEN
    !  OPEN FILE FOR NORMAL PROBABILITY GRAPH OF CORRELATED RANDOM NUMBERS
    FN = TRIM(OUTNAM)//TRIM(FNEXTRG)
    IPLOTG = UTL_GETUNIT(50,99)
    OPEN(IPLOTG,FILE=FN,STATUS='UNKNOWN')
  ENDIF
  ! ECHO
  WRITE(IOUT,1030) NP,NOBS,MPR,NSETS,NRAN,VAR
  NTOT = NOBS+MPR
  IF(NTOT > 12000) THEN
    WRITE(*,1500)
    WRITE(IOUT,1500)
    STOP
  ENDIF
  !     DYNAMICALLY ALLOCATE MEMORY FOR ARRAYS
  ALLOCATE (X(NP,NTOT), X0(NTOT,NP), COV(NP,NP), &
           BUFF2(NTOT), D(NTOT), G(NTOT), F(NTOT), R(NTOT,NTOT), &
           IPTR(NP), IPTRDID(NTOT), LOGT(NPS), PINC(NPS),TEMPPVAL(NPS))
  ALLOCATE (PARNAM(NP), PANAM(NPS), DID(NTOT), DID1(NTOT))
  ALLOCATE (OBSNAM(NOBS),PRINAM(MPR))
  ! *** VARIABLES FOR COOK'S D AND DFBETAS
  ALLOCATE (E(NTOT), CD(NTOT), C(NP,NTOT), S(NTOT), DFBETA(NTOT,NP))
  ALLOCATE (PLOTSYMBOL(NOBS), PLOTSYMBOLPRI(MPR))
  ALLOCATE (ISYM(NTOT), ISYM1(NTOT))
  !
  ! *** READ WEIGHTED RESIDUALS FROM THE _W FILE TO DEFINE THE DATA PLOT
  !     SYMBOLS AND OBS-NAME, AND AS NEEDED TO CALCULATE Cook's D
  READ(INWR,*)  ! skip header line
  DO I=1,NTOT
    READ(INWR,*) E(I),ISYM(I),DID(I)
  ENDDO
  CLOSE(INWR)
  DO J=1,NP
    IPTR(J)=J
  ENDDO
  !     READ PARAMETER NAMES AND VARIANCE-COVARIANCE MATRIX ON THE PARAMETERS
  CALL UTL_DX_READ_MCMV(FNEXTMV,NP,OUTNAM,PARNAM,COV)
  IF(PRCOVPARAM) THEN
    WRITE(IOUT,1060)
    CALL UTL_PARSQMATRIX(IOUT,6,NP,NP,COV,IPTR,PARNAM)
  ELSE
    WRITE(IOUT,1061)
  ENDIF
  !     READ THE SQUARE ROOT OF THE WEIGHT MATRIX IN COMPRESSED FORM
  ! READ THE WEIGHT MATRIX
  IF(MPR > 0) THEN
    WRITE(IOUT,1039) &
         TRIM(TRIM(OUTNAM)//'.'//TRIM(FNEXTWT)//' and '//TRIM(FNEXTWTPRI))
    CALL TYP_NULL(WTMAT)
    CALL TYP_NULL(WTMATSQR)
    CALL TYP_NULL(WTMATDEP)
    CALL TYP_NULL(WTMATDEPSQR)
    CALL TYP_NULL(PRIWTMAT)
    CALL TYP_NULL(PRIWTMATSQR)
    CALL UTL_DX_READ_WT(IOUT,OUTNAM,FNEXTWT,WTMATDEP,WTMATDEPSQR)
    CALL UTL_DX_READ_WT(IOUT,OUTNAM,FNEXTWTPRI,PRIWTMAT,PRIWTMATSQR)
    CALL UTL_COMBINESQMATRIX(WTMATDEP,PRIWTMAT,WTMAT)
    CALL UTL_COMBINESQMATRIX(WTMATDEPSQR,PRIWTMATSQR,WTMATSQR)
    CALL TYP_DEALLOC(WTMATDEP)
    CALL TYP_DEALLOC(WTMATDEPSQR)
    CALL TYP_DEALLOC(PRIWTMAT)
    CALL TYP_DEALLOC(PRIWTMATSQR)
  ELSE
    WRITE(IOUT,1039) TRIM(OUTNAM)//'.'//TRIM(FNEXTWT)
    CALL TYP_NULL(WTMAT)
    CALL TYP_NULL(WTMATSQR)
    CALL UTL_DX_READ_WT(IOUT,OUTNAM,FNEXTWT,WTMAT,WTMATSQR)
  ENDIF
  DO J = 1,NTOT
    IPTRDID(J) = J
  ENDDO
  IF(PRSQRWT) THEN
    CALL UTL_WRITECDMATRIX(WTMATSQR,1,IOUT,.TRUE.,DID,DID)
  ELSE
    WRITE (IOUT,1065)
  ENDIF
  CLOSE(IIN)
  IF(VAR .EQ. 1.E+30) CALL UTL_STOP &
          ('Failed based on _dm: Linearity=yes requires a converged regression')
  ! READ STATTUS FLAGS FOR PARAMETERS IN THE LAST ITERATION
  CALL UTLUCODE_DX_READ_PAOPT(NPS,OUTNAM,LOGT,PANAM,PINC,TEMPPVAL, &
                              NPS,EXT)
  NPE = 0
  DO J=1,NPS
    IF(PINC(J) > -1) NPE=NPE+1
  ENDDO
  ALLOCATE (XORIG(NPE,NOBS), XPRI(NPE,MPR), PANAMNPE(NPE))
  !     READ AND WRITE SENSITIVITIES
  CALL SEN_UEV_DX_READ_SU &
       (NOBS,NPE,OBSNAM,OUTNAM,PANAMNPE,PLOTSYMBOL,XORIG,FNEXTSU)
  IF(MPR > 0) &
  CALL SEN_UEV_DX_READ_SU(MPR,NPE,PRINAM,OUTNAM,PANAMNPE, &
                           PLOTSYMBOLPRI,XPRI,FNEXTSUPRI)
  DO I=1,NOBS
    DID(I) = OBSNAM(I)
    ISYM(I) = PLOTSYMBOL(I)
  ENDDO
  DO I=1,MPR
    DID(I+NOBS) = PRINAM(I)
    ISYM(I+NOBS) = PLOTSYMBOLPRI(I)
  ENDDO
  IF(NPS .NE. NP) THEN
    I = 0
    N = 0
    DO J=1,NPS
      IF(PINC(J) > -1) N=N+1
      IF(PINC(J) < 1) THEN
        CYCLE
      ELSE
        I = I+1
        DO K=1,NOBS
          X(I,K) = XORIG(N,K)
        ENDDO
        IF(MPR > 0) THEN
          DO IPM=1,MPR
            X(I,IPM+NOBS) = XPRI(N,IPM)
          ENDDO
        ENDIF
      ENDIF
    ENDDO
  ELSE
    DO J=1,NPS
      DO K=1,NOBS
        X(J,K) = XORIG(J,K)
      ENDDO
      IF(MPR > 0) THEN
        DO IPM=1,MPR
          X(J,IPM+NOBS) = XPRI(J,IPM)
        ENDDO
      ENDIF
    ENDDO
  ENDIF
  IF (ALLOCATED(XORIG)) DEALLOCATE(XORIG)
  IF (ALLOCATED(XPRI)) DEALLOCATE(XPRI)
  IF (ALLOCATED(PLOTSYMBOLPRI)) DEALLOCATE(PLOTSYMBOLPRI)
  IF (ALLOCATED(PLOTSYMBOL)) DEALLOCATE(PLOTSYMBOL)
  IF (ALLOCATED(OBSNAM)) DEALLOCATE(OBSNAM)
  IF (ALLOCATED(PRINAM)) DEALLOCATE(PRINAM)
  IF(PRSENSOPTPARAM) THEN
    WRITE(IOUT,1070)
    CALL PRTOTX(NP,NTOT,NP,X,DID,PARNAM,IOUT)
  ELSE
    WRITE(IOUT,1071)
  ENDIF
  !     CALCULATE W**.5 X, THE SQUARE-ROOT OF THE WEIGHT MATRIX TIMES X
  CALL UTL_MATMUL_SUB(NTOT,NP,WTMATSQR,TRANSPOSE(X),X0)
  X = TRANSPOSE(X0)
  !
  !**COMPUTE (I-R)*VAR MATRIX
  !
  DO K=1,NTOT
    DO J=1,NP
      SUM=0.D0
      DO I=1,NP
        SUM=SUM+X(I,K)*COV(I,J)
      ENDDO
      R(J,K)=SUM
    ENDDO
  ENDDO
  !
  DO K=1,NTOT
    DO J=K,NTOT
      SUM=0.D0
      DO I=1,NP
        SUM=SUM+X(I,K)*R(I,J)
      ENDDO
      R(J,K)=-SUM
    ENDDO
  ENDDO
  !
  DO J=1,NTOT
    DO I=J,NTOT
      R(J,I)=R(I,J)
    ENDDO
    R(J,J)=VAR+R(J,J)
  ENDDO
  ! *** COMPUTE COOK'S D USING EQN 3.12.2 FROM DRAPER & SMITH, 1981
  ! *** NOTE: R MATRIX AT THIS POINT IN PROGRAM IS (I-R)*VAR
  IF(PR_RC .OR. PR_RB) THEN
    !  OPEN FILE FOR COOK'S D STATISTIC
    FN = TRIM(OUTNAM)//TRIM(FNEXTRC)
    ICOOK = UTL_GETUNIT(7,100)
    OPEN(ICOOK,FILE=FN,STATUS='UNKNOWN', &
        ACCESS='SEQUENTIAL',FORM='FORMATTED')
    IF(PR_RC) THEN
      IF(PR_RC_EACH) THEN
        WRITE(IOUT,1260)
      ELSE
        WRITE(IOUT,1265)
      ENDIF
    ENDIF
    DO I=1,NTOT
      RR = 1.D0-(R(I,I)/VAR)
      IF (RR.GE.0.0D0 .AND. RR.LT.1.0D0) THEN
        CCD = E(I)/( SQRT(VAR*(1.D0-RR)) )
        CD(I) = CCD*CCD*RR / ( (1.D0-RR)*NP )
        IF(PR_RC) THEN
          IF(PR_RC_EACH) WRITE(IOUT,1270)DID(I),ISYM(I), &
                    CCD,(RR)/(1.D0-RR),CD(I)
        ENDIF
      ELSE
        CD(I) = 0.0D0  ! To ensure that all CD are defined
        IF(PR_RC) THEN
          IF(PR_RC_EACH) WRITE(IOUT,1390)DID(I),ISYM(I),RR
        ENDIF
      ENDIF
    ENDDO
    !***
    !*** Find Cook's D > critical value 4./(NOBS+MPR)
    !***
    CUTOFF = 4.D0 / REAL(NTOT)
    NUM = 0
    IF(PR_RC) THEN
      WRITE(IOUT,1291)
      WRITE(IOUT,1290) CUTOFF
    ENDIF
    DO I=1,NTOT
      IF (CD(I).GT.CUTOFF) THEN
        IF(PR_RC) WRITE(IOUT,1310) I,DID(I),ISYM(I),CD(I)
        NUM = NUM + 1
      ENDIF
    ENDDO
    IF(PR_RC) THEN
      WRITE(IOUT,1320) NUM
      !
      !     WRITE COOK'S d TO _RC FILE FOR PLOTTING
      !
      WRITE(ICOOK,1300)
      DO I=1,NTOT
        WRITE(ICOOK,1280) CD(I),DID(I),ISYM(I)
      ENDDO
    ENDIF
    !
    ! *** Compute C matrix = COV * transpose(X) * WT**.5
    ! *** NOTE: X MATRIX AT THIS POINT IN PROGRAM IS transpose(X) * WT**.5
    DO J=1,NP
      DO I=1,NTOT
        C(J,I) = 0.D0
        DO K=1,NP
          C(J,I) = C(J,I) + COV(J,K)*X(K,I)
        ENDDO
      ENDDO
    ENDDO
  ELSE
    WRITE(IOUT,1266)
  ENDIF
  ! *** Compute DFBETAS using eqn 2.7 from Belsley & others, 1980, p. 13
  IF(PR_RB) THEN
    !  OPEN FILE FOR DF BETAS
    FN = TRIM(OUTNAM)//FNEXTRB
    IDFB = UTL_GETUNIT(7,100)
    OPEN(IDFB,FILE=FN,STATUS='UNKNOWN', &
        ACCESS='SEQUENTIAL',FORM='FORMATTED')
    IBAD = 0
    DO I=1,NTOT
      RR   = 1.D0-(R(I,I)/VAR)
      S(I)=(NTOT-NP)*VAR - E(I)**2.D0/(1.D0-RR)
      S(I)=( 1.D0/FLOAT(NTOT-NP-1) )*S(I)
    !
    !  TEST FOR NEGATIVE S(I), WHICH CAN'T BE HANDLED -- ERB 3/30/01.  THIS
    !  SITUATION CAN ARISE DUE TO A COMBINATION OF MODEL NONLINEARITY AND
    !  LARGE LEVERAGE OF THE OBSERVATION.  SEE COOK AND WEISBERG, 1982,
    !  "RESIDUALS AND INFLUENCE IN REGRESSION," MONOGR. STAT. APPL.
    !  PROBABLILITY, VOL. 18, CHAPMAN AND HALL, NEW YORK, P. 14.
      IF (S(I).GT.0.0D0) THEN
        S(I) = SQRT(S(I))
        DFB1 = E(I) / ( S(I) * (1.D0-RR) )
      ELSE
        DFB1 = 0.0D0
        IF (IBAD.EQ.0) WRITE(IOUT,1430)
        WRITE(IOUT,1440) DID(I)
        IBAD = 1
      ENDIF
    !
      DO J=1,NP
        CC = 0.D0
        DO K = 1,NTOT
          CC = CC + C(J,K)**2.D0
        ENDDO
        DFB2 = C(J,I) / SQRT(CC)
        DFBETA(I,J) = DFB1 * DFB2
      ENDDO
    ENDDO
    !***
    !*** Find DFBETAS > critical value 2/(NTOT)**0.5
    !***
    NUM = 0
    CUTOFF = NTOT
    CUTOFF = 2.D0/SQRT(CUTOFF)
    WRITE(IOUT,1291)
    WRITE(IOUT,1330)
    WRITE(IOUT,1331)
    DO K=1,NP,5
      IF(NP < K+4) THEN
        KK = NP
      ELSE
        KK = K+4
      ENDIF
      WRITE(IOUT,1332) (J,J=K,KK)
      WRITE(IOUT,1333) (PARNAM(J),J=K,KK)
    ENDDO
    WRITE(IOUT,*)
    WRITE(IOUT,1334) CUTOFF
    NUMSTRIP=10
    NUMSTRIP1=NUMSTRIP-1
    NCYCLE=NP/NUMSTRIP
    IF(MOD(NP,NUMSTRIP) > 0)NCYCLE=NCYCLE+1
    DO NCYC=1,NCYCLE
      N1=(NCYC-1)*NUMSTRIP+1
      N2=N1+NUMSTRIP1
      IF(N2 > NP)N2=NP
      WRITE(IOUT,1335) (J,J=N1,N2)
      DO I=1,NTOT
        ISKIP = .TRUE.
        DO J=1,NP
          IF (ABS(DFBETA(I,J)) < CUTOFF) CYCLE
          ISKIP = .FALSE.
          NUM = NUM + 1
        ENDDO
        IF(.NOT. ISKIP) THEN
          STRIP2 = ' '
          CNT = 0
          DO K=N1,N2
            CNT = CNT +1
            IF (ABS(DFBETA(I,K)) > CUTOFF) THEN
              STRIP2(4*CNT:4*CNT) = '#'
            ELSE
              STRIP2(4*CNT:4*CNT) = '-'
            ENDIF
          ENDDO
          WRITE(IOUT,1340) I,DID(I),ISYM(I),STRIP2
        ENDIF
      ENDDO
    ENDDO
    WRITE(IOUT,1320) NUM
    !
    !     WRITE DFBETA'S TO _RB FILE FOR PLOTTING
    !
    CALL UTLUCODE_DX_WRITE_MATRIX(IDFB,NTOT,NP,NP,IPTR,PARNAM, &
                         TRANSPOSE(DFBETA),DID,ISYM,COL12NAM)
  ELSE
    WRITE(IOUT,1375)
  ENDIF
  ! ***
  !**COMPUTE THEORETICAL FREQUENCIES FOR DATA SETS
  IF(PR_CALCDEV) THEN
    IF(MOD(2,NTOT) .GT. 0) THEN
      TMP = NTOT
    ELSE
      TMP=NTOT+1
    ENDIF
    DO I=1,NTOT
      TEMP=I
      F(I)=(TEMP-.5D0)/TMP
      CALL STA_EVA_PROB_NORM_DISTRB(BUFF2(I),F(I),-1)
    ENDDO
    SIGMA=SQRT(VAR)
    !
    !**LOOP FOR SETS
    !
    IF(PR_DEVRS) THEN
      WRITE (IOUT,1400)
    ELSE
      WRITE(IOUT,1405)
    ENDIF
    DO K=1,NSETS
      !**COMPUTE NORMAL RANDOM NUMBERS, D AND
      !  CORRELATED NORMAL  RANDOM NUMBERS, G
      DO I=1,NTOT
        SUM=-6.D0
        DO J=1,12
          SUM=SUM+RANUM(NRAN)
        ENDDO
        D(I)=SIGMA*SUM
      ENDDO
      DO J=1,NTOT
        SUM=0.D0
        DO I=1,NTOT
          SUM=SUM+R(I,J)*D(I)
        ENDDO
        G(J)=SUM/VAR
      ENDDO
      !
      !ASSIGN DID1 AND ISYM1
      DO I=1,NTOT
        DID1(I) = DID(I)
        ISYM1(I) = ISYM(I)
      ENDDO
      !
      !**ORDER NORMAL RANDOM NUMBERS AND PRINT
      DO I=1,NTOT
        DO J=I,NTOT
          IF(D(J).GE.D(I)) CYCLE
          TMP=D(I)
          D(I)=D(J)
          D(J)=TMP
          TMPDID=DID1(I)
          DID1(I)=DID1(J)
          DID1(J)=TMPDID
          ISYMTMP=ISYM1(I)
          ISYM1(I)=ISYM1(J)
          ISYM1(J)=ISYMTMP
        ENDDO
      ENDDO
      IF(PR_CALCDEV) THEN
        IF(PR_RD) THEN
          WRITE(IPLOTD,1190) K
          WRITE(IPLOTD,1210) &
           (D(I),BUFF2(I),ISYM1(I),DID1(I),I=1,NTOT)
          WRITE(IPLOTD,1220) BLANK
        ENDIF
      ELSE
        WRITE(IOUT,1225)
      ENDIF
      !
      ! RETURN DID1 AND ISYM1 TO ORIGINAL ORDER
      DO I=1,NTOT
        DID1(I)=DID(I)
        ISYM1(I)=ISYM(I)
      ENDDO
      !
      !
      !**ORDER CORRELATED NORMAL RANDOM NUMBERS AND PRINT
      DO I=1,NTOT
        DO J=I,NTOT
          IF(G(J).GE.G(I)) CYCLE
          TMP=G(I)
          G(I)=G(J)
          G(J)=TMP
          TMPDID=DID1(I)
          DID1(I)=DID1(J)
          DID1(J)=TMPDID
          ISYMTMP=ISYM1(I)
          ISYM1(I)=ISYM1(J)
          ISYM1(J)=ISYMTMP
        ENDDO
      ENDDO
      IF(PR_CALCDEV) THEN
        IF(PR_RG) THEN
          WRITE(IPLOTG,1240) K
          WRITE(IPLOTG,1210) &
            (G(I),BUFF2(I),ISYM1(I),DID1(I),I=1,NTOT)
          WRITE(IPLOTG,1220) BLANK
        ENDIF
      ELSE
        WRITE(IOUT,1226)
      ENDIF
      IF(K == NSETS) THEN
        IF(PR_RD) CLOSE(UNIT=IPLOTD)
        IF(PR_RG) CLOSE(UNIT=IPLOTG)
      ENDIF
    ENDDO
  ELSE
    WRITE(IOUT,1295)
  ENDIF
  !
  !   END SET LOOP
  !
  !**PRINT COVARIANCE MATRIX (I-R)*VAR
  IF(PRCOVRESID) THEN
    WRITE(IOUT,1100)
    CALL UTL_PARSQMATRIX(IOUT,8,NTOT,NTOT,R,IPTRDID,DID)
  ELSE
    WRITE(IOUT, 1105)
  ENDIF
  !**COMPUTE AND PRINT CORRELATION MATRIX
  DO I=1,NTOT
    IF (R(I,I).LT.0.0D0) THEN
      WRITE(IOUT,1450)
      GO TO 999
    ENDIF
    D(I)=SQRT(R(I,I))
  ENDDO
  DO J=1,NTOT
    TMP=D(J)
    DO I=J,NTOT
      R(I,J)=R(I,J)/(TMP*D(I))
      R(J,I)=R(I,J)
    ENDDO
  ENDDO
  IF(PRCORRESID) THEN
    WRITE(IOUT,1120)
    CALL UTL_PARSQMATRIX(IOUT,8,NTOT,NTOT,R,IPTRDID,DID)
  ELSE
    WRITE(IOUT,1125)
  ENDIF
  !
  !     WRITE NAMES OF OUTPUT FILES TO SCREEN
  999 WRITE(*,1380)
  WRITE(*,1381) TRIM(TRIM(OUTNAM)//TRIM(FNEXTRS))
  IF(PR_RD)WRITE(*,1381)TRIM(TRIM(OUTNAM)//TRIM(FNEXTRD))
  IF(PR_RG)WRITE(*,1381)TRIM(TRIM(OUTNAM)//TRIM(FNEXTRG))
  IF(PR_RC)WRITE(*,1381)TRIM(TRIM(OUTNAM)//TRIM(FNEXTRC))
  IF(PR_RB)WRITE(*,1381)TRIM(TRIM(OUTNAM)//TRIM(FNEXTRB))
  WRITE(*,900)TRIM(VERSION)
  WRITE(IOUT,900)TRIM(VERSION)
  CALL UTL_ENDTIME(IBDT,IOUT)
  CLOSE(IOUT)
  STOP
  10 IF (IOS.NE.0) THEN
    WRITE (*,1150)  TRIM(FN)
    STOP
  ENDIF
END PROGRAM RESIDUAL_ANALYSIS
!=======================================================================
!=======================================================================
SUBROUTINE PRTOT(C,NR,NC,NRD,IOUT)
  !**PRINT MATRICES AND VECTORS
  IMPLICIT NONE
  DOUBLE PRECISION,               INTENT(IN) :: C(1)
  INTEGER,                        INTENT(IN) :: NR
  INTEGER,                        INTENT(IN) :: NC
  INTEGER,                        INTENT(IN) :: NRD
  INTEGER,                        INTENT(IN) :: IOUT
  ! local variables
  INTEGER                                    :: I
  INTEGER                                    :: J
  INTEGER                                    :: J10
  INTEGER                                    :: K
  INTEGER                                    :: KB
  INTEGER                                    :: KBC
  INTEGER                                    :: KE
  INTEGER                                    :: KEC
  INTEGER                                    :: L
  INTEGER                                    :: N
  ! formats
  35 FORMAT ('0',10(8X,I4))
  40 FORMAT (1X,I4,10(1X,G11.5))
  50 FORMAT (' ')
  80 FORMAT (3X,3(I4,6X,G11.5,3X))
  !
  IF(NC .NE. 1) THEN
    DO L=1,NC,10
      J10=L+9
      IF(J10.GT.NC) J10=NC
      WRITE(IOUT,35) (J,J=L,J10)
      WRITE(IOUT,50)
      KBC=(L-1)*NRD
      KEC=(J10-1)*NRD
      DO I=1,NR
      KB=KBC+I
      KE=KEC+I
      WRITE(IOUT,40) I,(C(K),K=KB,KE,NRD)
    ENDDO
    ENDDO
    RETURN
  ENDIF
  N=NR/3
  IF((3*N).NE.NR) N=N+1
  DO K=1,N
    WRITE(IOUT,80) (L,C(L),L=K,NR,N)
  ENDDO
  RETURN
END SUBROUTINE PRTOT
!=======================================================================
!=======================================================================
SUBROUTINE PRTOTA(NO,DID,VALB,IOUT)
  !**PRINT VALUES IN TWO GROUPS OF THREE COLUMNS
  IMPLICIT NONE
  INTEGER,                 INTENT(IN) :: NO
  CHARACTER(LEN=12),       INTENT(IN) :: DID(NO)
  DOUBLE PRECISION,        INTENT(IN) :: VALB(NO)
  INTEGER,                 INTENT(IN) :: IOUT
  ! local variables
  INTEGER                             :: K
  INTEGER                             :: L
  INTEGER                             :: NR
  ! formats
  20 FORMAT (2X,2(I4,2X,A,1X,G11.5,2X))
  !
  NR=NO/2
  IF(2*NR.NE.NO) NR=NR+1
  DO K=1,NR
    WRITE(IOUT,20) (L,DID(L),VALB(L),L=K,NO,NR)
  ENDDO
  RETURN
END SUBROUTINE PRTOTA
!=======================================================================
!=======================================================================
SUBROUTINE PRTOTD2(NO,DID,VALA,VALB,IOUT)
!**PRINT VALUES IN TWO GROUPS OF THREE COLUMNS
  IMPLICIT NONE
  INTEGER,                 INTENT(IN) :: NO
  CHARACTER(LEN=12),       INTENT(IN) :: DID(NO)
  DOUBLE PRECISION,        INTENT(IN) :: VALA(NO)
  DOUBLE PRECISION,        INTENT(IN) :: VALB(NO)
  INTEGER,                 INTENT(IN) :: IOUT
  ! local variables
  INTEGER                             :: L
  ! formats
  20 FORMAT (3X,I4,3X,A,3X,G11.5,3X,G11.5)
  DO L=1,NO
    WRITE(IOUT,20) L,DID(L),VALA(L),VALB(L)
  ENDDO
  RETURN
END SUBROUTINE PRTOTD2
!=======================================================================
!=======================================================================
SUBROUTINE PRTOTX(NR,NC,NRD,X,DID,PARNAM,IOUT)
!**PRINT MATRICES DIVIDED VERTICALLY INTO FIVE-COLUMN BLOCKS
  IMPLICIT NONE
  INTEGER,                   INTENT(IN) :: NR
  INTEGER,                   INTENT(IN) :: NC
  INTEGER,                   INTENT(IN) :: NRD
  DOUBLE PRECISION,          INTENT(IN) :: X(NRD,NC)
  CHARACTER(LEN=12),         INTENT(IN) :: DID(NC)
  CHARACTER(LEN=12),         INTENT(IN) :: PARNAM(NR)
  INTEGER,                   INTENT(IN) :: IOUT
  ! local variables
  INTEGER                               :: I
  INTEGER                               :: J
  INTEGER                               :: J10
  INTEGER                               :: K
  ! formats
  500 FORMAT (/,1X,'PARAMETER',3X,5(A12,1X))
  505 FORMAT (1X,A10,2X,5(G12.5,1X))
  510 FORMAT (1X,10('-'),2X,5(A12,1X))
  !
  DO K = 1, NC, 5
    J10 = K + 4
    IF (J10.GT.NC) J10 = NC
    WRITE (IOUT,500) (DID(J),J=K,J10)
    WRITE (IOUT,510) ('------------',J=K,J10)
    DO I = 1, NR
      WRITE (IOUT,505) PARNAM(I), (X(I,J),J=K,J10)
    ENDDO
  ENDDO
  RETURN
END SUBROUTINE PRTOTX
!=======================================================================
!==================================================================
FUNCTION RANUM(IRAN)
!  NOTE: DICK COOLEY SAYS HE HAS A BETTER RANDOM-NUMBER GENERATOR THAT
!  DOES NOT REPEAT - ERB 3/15/2001
  IMPLICIT NONE
  INTEGER,                      INTENT(INOUT) :: IRAN
  ! local variables
  INTEGER                                     :: MODU
  INTEGER                                     :: MULT
  INTEGER                                     :: NADD
  DOUBLE PRECISION                            :: RANUM
  !
  DATA MODU,MULT,NADD/1048576,1027,221589/
  !
  IRAN=MULT*IRAN+NADD
  IRAN=IRAN-(IRAN/MODU)*MODU
  RANUM=FLOAT(IRAN)/FLOAT(MODU)
  RETURN
END FUNCTION RANUM
!=======================================================================
!=======================================================================
