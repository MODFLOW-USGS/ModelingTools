PROGRAM RESIDUAL_ANALYSIS_ADV
!       NON-LINEARITY AND RESIDUALS ANALYSIS PROGRAM BY R. L. COOLEY, USGS,
!          DENVER, COLO.
!       MODIFIED FOR MODFLOW-2000 BY STEEN CHRISTENSEN, DEPT. OF AARHUS,DENMARK.
!          NOT ADAPTED FOR DRY OBSERVATIONS
!       THE INPUT AND PREPARATION PART AND SOME OUTPUT SUBROUTINES ARE
!          MODIFICATIONS FROM RESAN2K BY E.R. BANTA, 8/12/1999
!    Modified for Jupiter compatibility by E.P.Poeter, JULY 2005
!     ******************************************************************
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
USE GLOBAL_DATA, ONLY: AMESSAGE, LENDNAM, MAX_STRING_LEN, VERSIONID
USE BASIC, ONLY: BAS_INI_GETOPTIONS
USE DATATYPES
USE DEPENDENTS, ONLY: DEP_UEV_DX_READ_WS
USE SENSITIVITY, ONLY: SEN_UEV_DX_READ_SU
USE STATISTICS, ONLY: STA_EVA_PROB_NORM_DISTRB
USE UTILITIES
USE UTLUCODE
  IMPLICIT NONE
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)      :: AM
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)    :: BUFF1
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)      :: BUFF2
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)      :: BUFF3
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)      :: CE
  DOUBLE PRECISION                                  :: CED
  DOUBLE PRECISION                                  :: CEI
  DOUBLE PRECISION                                  :: CEV
  CHARACTER (LEN=12)                                :: CDUM
  CHARACTER(LEN=10)                                 :: CHDATE
  CHARACTER(LEN=10)                                 :: CHTIME
  CHARACTER(LEN=10)                                 :: CHZONE
  DOUBLE PRECISION                                  :: CL90
  DOUBLE PRECISION                                  :: CL95
  DOUBLE PRECISION                                  :: CL99
  CHARACTER (LEN=3)                                 :: CONVERGE
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)    :: COV
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)    :: COVNPORIG
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)      :: D
  DOUBLE PRECISION                                  :: DDM
  DOUBLE PRECISION                                  :: DI
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: DID
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: DID1
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: DIDLC
  DOUBLE PRECISION                                  :: DIM
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)      :: DM
  DOUBLE PRECISION                                  :: DMM
  DOUBLE PRECISION                                  :: DMVR
  DOUBLE PRECISION                                  :: DTVCDM
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)      :: DV
  DOUBLE PRECISION                                  :: DVR
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)      :: E
  DOUBLE PRECISION                                  :: ED
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)      :: EDUM
  DOUBLE PRECISION                                  :: EF
  DOUBLE PRECISION                                  :: EI
  DOUBLE PRECISION                                  :: EM
  DOUBLE PRECISION                                  :: ES
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)      :: ET
  DOUBLE PRECISION                                  :: EVR
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)      :: F
  INTEGER                                           :: FIRST
  DOUBLE PRECISION                                  :: FM
  CHARACTER(LEN=MAX_STRING_LEN)                     :: FN
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)      :: FRQ
  DOUBLE PRECISION                                  :: FS
  DOUBLE PRECISION                                  :: FVR
  INTEGER                                           :: I
  INTEGER                                           :: IBDT(8)
  INTEGER                                           :: IC
  INTEGER                                           :: IC1
  INTEGER                                           :: IC2
  INTEGER                                           :: IC3
  LOGICAL                                           :: ICOV
  INTEGER                                           :: IDUM
  INTEGER                                           :: IERR
  LOGICAL                                           :: IERT
  INTEGER                                           :: IFAIL
  INTEGER                                           :: IINRS
  INTEGER                                           :: IK
  INTEGER                                           :: IKP
  INTEGER                                           :: INMATRIX
  INTEGER                                           :: IOS
  INTEGER                                           :: IOUT
  LOGICAL                                           :: IPCF
  INTEGER                                           :: IPLOTD
  LOGICAL                                           :: IPRN
  INTEGER, ALLOCATABLE,  DIMENSION(:)               :: IPTR
  INTEGER                                           :: IS
  INTEGER                                           :: ISET
  INTEGER                                           :: ISCR
  INTEGER                                           :: ISTAT
  INTEGER, ALLOCATABLE,  DIMENSION(:)               :: ISYM
  INTEGER, ALLOCATABLE,  DIMENSION(:)               :: ISYM1
  INTEGER                                           :: ISYMTMP
  INTEGER                                           :: J
  INTEGER                                           :: K
  LOGICAL                                           :: LEX = .FALSE.
  TYPE (LLIST), POINTER                             :: LLPTRMATFIL
  INTEGER                                           :: MORE
  INTEGER                                           :: MPR
  TYPE (LLIST), POINTER                             :: MTEHEAD
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: MTENAM
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: MTENAMLC
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)      :: MTEVAL
  INTEGER                                           :: N
  INTEGER                                           :: NITER
  INTEGER                                           :: NMTE
  INTEGER                                           :: NOBS
  CHARACTER(LEN=40), DIMENSION(1)                   :: NOCOL = (/ '      '/)
  INTEGER                                           :: NPF
  INTEGER                                           :: NPORIG
  INTEGER                                           :: NPS
  INTEGER                                           :: NRAN
  INTEGER                                           :: NRESCODE
  INTEGER                                           :: NSETS
  INTEGER                                           :: NSETSPR
  INTEGER                                           :: NTOT
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: OBSNAM
  CHARACTER(LEN=MAX_STRING_LEN)                     :: OUTNAM
  CHARACTER(LEN=MAX_STRING_LEN)                     :: OUTNAMTMP
  CHARACTER(LEN=12), ALLOCATABLE,  DIMENSION(:)     :: PARNAM
  CHARACTER(LEN=12), ALLOCATABLE,  DIMENSION(:)     :: PANAM
  CHARACTER(LEN=12), ALLOCATABLE,  DIMENSION(:)     :: PANAMI
  CHARACTER(LEN=12), ALLOCATABLE,  DIMENSION(:)     :: PANAMF
  INTEGER, ALLOCATABLE,  DIMENSION(:)               :: PALNI
  INTEGER, ALLOCATABLE,  DIMENSION(:)               :: PALNF
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)      :: PAPVALI
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)      :: PAPVALF
  INTEGER, ALLOCATABLE,  DIMENSION(:)               :: PINCRI
  INTEGER, ALLOCATABLE,  DIMENSION(:)               :: PINCRF
  INTEGER, ALLOCATABLE,  DIMENSION(:)               :: PLOTSYMBOL
  INTEGER, ALLOCATABLE,  DIMENSION(:)               :: PLOTSYMBOLPRI
  LOGICAL                                           :: PRCOVPARAM
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: PRINAM
  LOGICAL                                           :: PRSQRWT
  LOGICAL                                           :: PRSENSOPTPARAM
  DOUBLE PRECISION                                  :: PROB
  DOUBLE PRECISION                                  :: QINT
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)    :: R
  DOUBLE PRECISION                                  :: RDUM
  TYPE (LLIST), POINTER                             :: RESANHEAD
  CHARACTER(LEN=40), DIMENSION(1)                   :: RESANCOL = (/ '      '/)
  DOUBLE PRECISION                                  :: RN
  DOUBLE PRECISION                                  :: RNM1
  DOUBLE PRECISION                                  :: RNDV
  DOUBLE PRECISION                                  :: RNSV
  DOUBLE PRECISION                                  :: RNT
  CHARACTER(LEN=3)                                  :: SENTYPE
  DOUBLE PRECISION                                  :: SLP
  DOUBLE PRECISION                                  :: STDV
  DOUBLE PRECISION                                  :: SUM
  TYPE (LLIST), POINTER                             :: TAIL
  DOUBLE PRECISION                                  :: TMP
  DOUBLE PRECISION                                  :: TMPA
  CHARACTER(LEN=LENDNAM)                            :: TMPDID
  DOUBLE PRECISION                                  :: TVAR
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)      :: U
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)    :: V
  DOUBLE PRECISION                                  :: VAR
  TYPE(CDMATRIX)                                    :: VCDM
  TYPE(CDMATRIX)                                    :: VCDMSQR
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
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)    :: XCOV
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)    :: XNPS
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)    :: XPRI
  !   For MTE VALUES input, define default column order
  INTEGER,           PARAMETER                      :: MTECOLS = 2
  CHARACTER(LEN=40), DIMENSION(MTECOLS)     :: MTECOL = &
      (/ 'MTENAME ','MTEVALUE'/)
  !
  PARAMETER (VERSION='1.004') !  09/2011
  ! Formats
  500 FORMAT (//,1X,78('*'),/,11X, &
            'WARNING SOME OF THE STATISTICS IN THE DATA-EXCHANGE FILES',/,11X, &
            'WERE GENERATED USING FORWARD DIFFERENCE SENSITIVITIES.',/,11X, &
            'THEY MAY BE INACCURATE AND CAUSE INACCURARCIES IN THE',/,11X, &
            'STATISTICS CALCULATED BY THIS PROGRAM.',/,11X, &
            'CONSIDER REGENERATING THE FILES WITH CENTRAL DIFFERENCES',/,11X, &
            'OR EXACT DERIVATIVES.',/,1X,78('*'),//)
   900 FORMAT(/,1X,80('*'),/,2X, &
             'Normal termination of RESIDUAL_ANALYSIS_ADV, Version: ',A, &
             /,1X,80('*'),//)
   999 FORMAT(1X,'READING AND PREPARING INPUT DATA',/, &
              1X,'  Given that less than 20 sets have been requested',/, &
              1X,'  A status line will print to the screen each time',/, &
              1X,'    a set is evaluated',/)
  1000 FORMAT(1X,'READING AND PREPARING INPUT DATA',/, &
              1X,'  A status line will print to the screen each time',/, &
              1X,'    approximately 5% of the sets are evaluated',/)
  1001 FORMAT(1X,'COMPUTING SET: ',I10,' OF: ',I10)
  1010 FORMAT(' !!!! ERROR MISSING ROOTFILENAME ON RESIDUAL_ANALYSIS_ADV ', &
              'COMMAND LINE !!!')
  1030 FORMAT(/, &
             ' NUMBER OF ESTIMATED PARAMETERS.............. ',I6,/, &
             ' NUMBER OF OBSERVATIONS ..................... ',I6,/, &
             ' NUMBER OF PRIOR EQUATIONS................... ',I6,/, &
             ' NUMBER OF SETS OF RANDOM NUMBERS ........... ',I6,/, &
             ' NUMBER FOR RANDOM NUMBER GENERATOR ......... ',I11,/, &
             ' CALCULATED ERROR VARIANCE .................. ',G13.6,/, &
             ' THEORETICAL ERROR VARIANCE ................. ',G13.6,//, &
             ' READ MEAN TRUE ERRORS....................... ',L6,/, &
             ' READ OBSERVATION ERROR COVARIANCE MATRIX.... ',L6,//, &
             ' PRINT PARAMETER VAR-COVARIANCE MATRIX....... ',L6,/, &
             ' PRINT SQRT WEIGHT MATRIX FOR OBSERVATIONS... ',L6,/, &
             ' PRINT UNSCALED SENSITIVITIES................ ',L6,/, &
             ' PRINT (I-R) MATRIX.......................... ',L6,/, &
             ' PRINT SIMULATED WEIGHTED RESIDUALS.......... ',L6,/)
  1039 FORMAT(/,1X,'WEIGHT MATRIX FOR OBSERVATIONS WILL BE READ FROM ', &
                 'FILE: ',/,20X,A)
  1040 FORMAT(/,1X,'WEIGHT MATRIX FOR OBSERVATIONS',/)
  1045 FORMAT(/,'  Default, or .rsadv File indicated NOT to print',/,11X, &
                  'SQUARE ROOT OF FULL WEIGHT MATRIX,',/, &
                  11X,'IT CAN BE FOUND IN _WT',/)
  1050 FORMAT(/,1X,'MEAN WEIGHTED RESIDUAL (EM) --------- = ',1PE12.4, &
              ' (SHOULD ~ 0.00)',/,1X, &
              'SLOPE (SLP) ------------------------- = ',1PE12.4, &
              ' (SHOULD ~ 0.00)',/,1X, &
              '(SLOPE OF THE PLOT OF WEIGHTED RESIDUALS VS ', &
              'WEIGHTED SIMULATED EQUIVALENTS)',/,1X, &
              'INTRINSIC NONLINEARITY MEASURE (QINT) = ',1PE12.4, &
              ' (SHOULD BE <<',1PE12.4,')')
  1060 FORMAT(/,' COVARIANCE MATRIX FOR ESTIMATED PARAMETERS')
  1061 FORMAT(/,'  Default, or .rsadv File indicated NOT to print',/,11X, &
               'COVARIANCE MATRIX FOR ESTIMATED PARAMETERS')
  1069 FORMAT(/,1X,'SENSITIVITIES FOR OBSERVATIONS WILL BE READ FROM ', &
                 'FILE: ',/,20X,A)
  1070 FORMAT(/,1X,'SENSITIVITIES FOR OPTIMUM PARAMETERS')
  1071 FORMAT(/,1X,' Default, or .rsadv File indicated NOT to print',/,11X, &
                     'SENSITIVITIES FOR OPTIMUM PARAMETERS')
  1075 FORMAT(/,9X,'ORDERED, WEIGHTED RESIDUALS',/, &
       10X,'OBSERVATION',/, &
       4X,'NO.',6X,'NAME',20X,'E',9X,'FRQ. (%)')
  1110 FORMAT(/,1X,'DATA GENERATED FROM RANDOM NUMBER SET NO. ',I3)
  1138 FORMAT(/,20X,'SIMULATED WEIGHTED RESIDUALS',/, &
       2X,2(6X,'OBSERVATION',15X),/, &
       2X,2(1X,'NO.',5X,'NAME',10X,'G',8X))
  1140 FORMAT(/,9X,'ORDERED, SIMULATED WEIGHTED RESIDUALS',/, &
       10X,'OBSERVATION',/, &
       4X,'NO.',6X,'NAME',20X,'D',9X,'FRQ. (%)')
  1142 FORMAT(/,9X,'MEAN ORDERED, SIMULATED WEIGHTED RESIDUALS',/, &
       10X,'OBSERVATION',/, &
       4X,'NO.',6X,'NAME',20X,'D',9X,'FRQ. (%)')
  1144 FORMAT(/,9X,'STD. DVS. OF ORDERED, SIMULATED WEIGHTED RESIDUALS', &
       /,10X,'OBSERVATION',/, &
       4X,'NO.',6X,'NAME',20X,'D',9X,'FRQ. (%)')
  1146 FORMAT(/'CORRELATION (CED) --------------- = ',G11.5, &
       /'PROBABILITY OF CORRELATION (PROB) = ',G11.5, &
       /'99% CONFIDENCE LIMIT (CL99) ----- = ',G11.5, &
       /'95% CONFIDENCE LIMIT (CL95) ----- = ',G11.5, &
       /'90% CONFIDENCE LIMIT (CL90) ----- = ',G11.5)
  1148 FORMAT(/23X,'IDENTITY MINUS HAT MATRIX'/, &
       3X,'OBSERVATION NAME',15X,'I-R')
  1150 FORMAT(//,' ERROR OPENING FILE "',A,'" -- STOP EXECUTION',//, &
       ' NOTE: DATA-EXCHANGE FILES FROM A SUCCESSFUL REGRESSION WITH THE'/, &
       '    ROOT FILENAME SPECIFIED ON THE COMMAND LINE MUST BE PRESENT',/, &
       '    IN THE DIRECTORY FOR INPUT TO RESIDUAL_ANALYSIS',/, &
       '    If that has been done, check that the root filename on',/, &
       '    the RESIDUAL_ANALYSIS command line is correct.',/)
  1160 FORMAT(25X,'RESIDUAL_ANALYSIS_ADVANCED',//, &
       24X,'CONSTRUCTED USING THE JUPITER API',/, &
       25X,'UCODE POST-PROCESSING PROGRAM',/, &
       15X,'TO PERFORM ANALYSIS OF NON-LINEARITY AND RESIDUALS',/, &
       32X,'Version ',A/)
  1230 FORMAT(/,1X,'WEIGHTED RESIDUALS WILL BE READ FROM FILE: ',/,20X,A)
  1231 FORMAT(/,1X,'WEIGHTED SIMULATED EQUIVALENTS WILL BE READ FROM FILE: ', &
              /,20X,A)
  1470 FORMAT(2X,'"ORDERED WEIGHTED RESIDUALS"', &
              2X,'"ORDERED SIMULATED WEIGHTED RESIDUALS (OSWR)"', &
              2X,'"STD DEV OF OSRW"', &
              2X,'"2*(STD DEV)"', &
              2X,'"CUMULATIVE PROBABILITY"', &
              2X,'"PROBABILITY PLOTTING POSITION"', &
              2X,'"OBSERVATION or PRIOR NAME"', &
              3X,'"PLOT-SYMBOL"')
  1480 FORMAT(6(4X,G15.5),2X,A12,2X,I6)
  1500 FORMAT &
  (1X,' Statistics in files fn._init, fn._init._mv, amd fn._init._su',/, &
   1X,'   need to be generated with values other than optimal values.',///, &
   1X,' SUGGESTED SOLUTION:',/, &
   1X,' Execute UCODE in nonoptimal sensitivity mode: ',/, &
   1X,'  Sensitivities=yes',/, &
   1X,'  CreateInitFiles=yes',/, &
   1X,'  Optimize=no',/, &
   1X,'  AND enter NonOptimal parameter values in a Parameter_Values Block',/, &
   1X,'      for the parameters listed below',/)
  1501 FORMAT(5X,'Optimal and NonOptimal Values are the same for: ',A)
  1502 FORMAT(//,1X,'Terminating RESIDUAL_ANALYSIS_ADV due to',/, &
              1X,   '  Match of Optimal and NonOptimal Statistics',/, &
              1X,   '  in files fn._init, fn._init._mv, and fn._init._su',/, &
              1X,   '  with files fn._paopt, fn._mv, and fn._su',/)
  !
  CALL DATE_AND_TIME(CHDATE,CHTIME,CHZONE,IBDT)
  !
  !     WRITE PROGRAM NAME AND VERSION TO SCREEN
  WRITE (*,*)
  WRITE (*,1160) VERSION
  ! ***
  !**DEFINE AND OPEN INPUT FILES AND OUTPUT FILE, AND DIMENSION ARRAYS
  !   FOR SUBROUTINE PRTOT
  ISCR = UTL_GETUNIT(7,100)
  OPEN(ISCR,STATUS='SCRATCH',ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
  IOUT = UTL_GETUNIT(7,100)
  ! GET ROOT NAME AND OPEN GENAL OUTPUT FILE
  OUTNAM = UTL_GETARG(1)
  IF(OUTNAM .EQ. ' ') THEN
    OPEN (IOUT,FILE='RESIDUAL_ANALYSIS_ADV.ERR',STATUS='UNKNOWN')
    WRITE(*,1010)
    WRITE(IOUT,1010)
    CLOSE(IOUT)
    CALL UTL_STOP &
    (' PLEASE INCLUDE A ROOTFILENAME on the RESIDUAL_ANALYSIS_ADV COMMAND LINE')
  ENDIF
  FN = TRIM(OUTNAM)//'.#resanadv'
  OPEN (IOUT,FILE=FN,STATUS='UNKNOWN')
  WRITE (IOUT,1160) VERSION
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
  !  OPEN FILE FOR OUTPUT OF NORMAL PROBABILITY GRAPH OF RANDOM NUMBERS
  FN = TRIM(OUTNAM)//'._rdadv'
  IPLOTD = UTL_GETUNIT(7,100)
  OPEN(IPLOTD,FILE=FN,STATUS='UNKNOWN')
  ! INITITALIZE
  NSETS = 1000
  NRAN = -104857
  IERT = .FALSE.
  ICOV = .FALSE.
  IK = 0
  IKP = 0
  IPCF = .FALSE.
  IPRN = .FALSE.
  STDV = 0.D0
  TVAR = 0.D0
  PRCOVPARAM = .FALSE.
  PRSQRWT = .FALSE.
  PRSENSOPTPARAM = .FALSE.
  NRESCODE = 0
  NULLIFY(RESANHEAD,TAIL)
  !**READ AND PRINT INPUT DATA AND CONVERT IT AS NEEDED FOR CALCULATIONS
  ! Get desired # of residual sets & Random # seed
  ! from user created RESIDUAL_ANALYSIS input file OUTNAM..rsadv)
  !**NOTE: NRAN MUST BE ODD AND MUST LIE BETWEEN 1 AND 1048575
  FN = TRIM(OUTNAM)//'.rsadv'
  IINRS = UTL_GETUNIT(7,100)
  INQUIRE(FILE=FN,EXIST=LEX)
  IF(LEX) THEN
    OPEN (IINRS,FILE=FN,STATUS='OLD',ERR=10,IOSTAT=IOS)
    !   Read options
    CALL BAS_INI_GETOPTIONS(IINRS,IOUT)
    CALL UTL_READBLOCK(0,'RESIDUAL_ANALYSIS_ADV_CONTROL_DATA',RESANCOL, &
                       IINRS,IOUT,'*',.FALSE.,RESANHEAD,TAIL,NRESCODE)
    IF (NRESCODE>0) THEN
      CALL UTL_FILTER(IERR,RESANHEAD,IOUT,'NSETS',NSETS)
      CALL UTL_FILTER(IERR,RESANHEAD,IOUT,'SEED',NRAN)
      CALL UTL_FILTER(IERR,RESANHEAD,IOUT,'ALTVAR',TVAR)
      CALL UTL_FILTER(IERR,RESANHEAD,IOUT,'READ_ET',IERT)
      CALL UTL_FILTER(IERR,RESANHEAD,IOUT,'READ_COV',ICOV)
      CALL UTL_FILTER(IERR,RESANHEAD,IOUT,'PRINT_IRMATRIX',IPCF)
      CALL UTL_FILTER(IERR,RESANHEAD,IOUT,'PRINT_SIMWGTRESIDUALS',IPRN)
      CALL UTL_FILTER(IERR,RESANHEAD,IOUT,'PRINT_PAR_VAR_COV_MATRIX',PRCOVPARAM)
      CALL UTL_FILTER(IERR,RESANHEAD,IOUT,'PRINT_SQRT_WT',PRSQRWT)
      CALL UTL_FILTER(IERR,RESANHEAD,IOUT,'PRINT_UNSCALED_SENS',PRSENSOPTPARAM)
    ENDIF
    IF(NRAN > 0) NRAN = -NRAN
    IF(NRAN < -1048575 .OR. NRAN > -1) NRAN = -104857
    IF (MOD(2,NRAN) .EQ. 0.) NRAN = NRAN + 1
  ENDIF
  IF(NSETS > 20) THEN
    WRITE(*,1000)
  ELSE
    WRITE(*,999)
  ENDIF
  CALL UTLUCODE_DX_READ_DM(IFAIL,OUTNAM,RDUM,RDUM,RDUM, &
                           RDUM,CONVERGE,NITER,RDUM,RDUM, &
                           RDUM,CDUM,CDUM,CDUM,CDUM, &
                           MPR,IDUM,NPORIG,NPF,NPS, &
                           NOBS,RDUM,RDUM,RDUM,VAR, &
                           IOUT,RDUM,RDUM,SENTYPE)
  IFAIL = 0
  ALLOCATE(PALNI(NPS),PANAMI(NPS),PINCRI(NPS),PAPVALI(NPS))
  ALLOCATE(PALNF(NPS),PANAMF(NPS),PINCRF(NPS),PAPVALF(NPS))
  CALL UTLUCODE_DX_READ_INIT(NPS,0,OUTNAM,CEV,PALNI,PANAMI,PINCRI,PAPVALI)
  VAR=CEV
  CALL UTLUCODE_DX_READ_PAOPT(NPS,OUTNAM,PALNF,PANAMF,PINCRF,PAPVALF)
  FIRST = 0
  DO I=1,NPS
    IF(PINCRF(I) > 0) THEN
      IF(PINCRF(I) == PINCRI(I)) THEN
        IF(PAPVALF(I) == PAPVALI(I)) THEN
          IF(FIRST == 0)THEN
            WRITE(IOUT,1500)
            WRITE(*,1500)
          ENDIF
          FIRST = 1
          WRITE(*,1501)PANAMF(I)
          WRITE(IOUT,1501)PANAMF(I)
        ENDIF
      ENDIF
    ENDIF
  ENDDO
  IF(FIRST == 1) THEN
    WRITE(IOUT,1502)
    WRITE(*,1502)
    CALL UTL_STOP(' SUGGESTION:    Review contents of fn.#resanadv')
  ENDIF
  WRITE(IOUT,1030) NPF,NOBS,MPR,NSETS,NRAN,VAR,TVAR,IERT,ICOV, &
                   PRCOVPARAM,PRSQRWT,PRSENSOPTPARAM,IPCF,IPRN
  IF(SENTYPE .EQ. "YES")WRITE(IOUT,500)
  IF(VAR .EQ. 1.E+30) CALL UTL_STOP &
          ('CEV in _dm indicates the parameter estimation did not converge')
  NTOT = NOBS+MPR
  IF(TVAR .NE. 0.D0) STDV = SQRT(TVAR)
  !
  !     ALLOCATE ARRAY MEMORY
  ALLOCATE(AM(55))
  AM = 0.D0
  ALLOCATE(CE(NSETS))
  ALLOCATE(COV(NPF,NPF), COVNPORIG(NPORIG,NPORIG), IPTR(NPF), PARNAM(NPF))
  ALLOCATE(BUFF1(NTOT,NTOT), BUFF2(NTOT), BUFF3(NTOT), &
           D(NTOT), DID(NTOT), DIDLC(NTOT), DID1(NTOT), &
           DM(NTOT),  DV(NTOT), E(NTOT), EDUM(NTOT), ET(NTOT), &
           F(NTOT), FRQ(NTOT), ISYM(NTOT), ISYM1(NTOT), &
           MTENAM(NTOT), MTENAMLC(NTOT), MTEVAL(NTOT), &
           R(NTOT,NTOT), U(NTOT), V(NTOT,NTOT))
  ALLOCATE(X(NPF,NTOT), XCOV(NTOT,NPF), X0(NTOT,NPF))
  ET = 0.D0
  U = 0.D0
  X = 0.D0
  !
  ! *** READ WEIGHTED RESIDUALS AND SIMULATED VALUES FROM THE ._WS
  !     FILE TO DEFINE THE DATA PLOT SYMBOLS AND OBS-NAME AND AS NEEDED TO
  !     COMPUTE NON-LINEARITY MEASURES
  WRITE(IOUT,1230) TRIM(OUTNAM)//'._ws'
  CALL DEP_UEV_DX_READ_WS(NTOT,OUTNAM,F,E,ISYM,DID)
  WRITE(IOUT,1231) TRIM(OUTNAM)//'._ww'
  CALL UTLUCODE_DX_READ_WW(NTOT,OUTNAM,F,EDUM,ISYM,DID)
  DO I=1,NTOT
    CALL UTL_CASE(DID(I),DIDLC(I),-1)
  ENDDO
  DID1 = DID
  ISYM1 = ISYM
  ! READ PARAMETER NAMES AND VARIANCE-COVARIANCE MATRIX ON THE PARAMETERS ._mv
  OUTNAMTMP = TRIM(OUTNAM)//'._init'
  CALL UTL_DX_READ_MCMV('_mv',NPORIG,OUTNAMTMP,PARNAM,COVNPORIG)
  COV = 0.D0
  ! READ THE WEIGHT MATRIX ._wt
  IF(MPR > 0) THEN
    WRITE(IOUT,1039) TRIM(OUTNAM)//'._wt and ._wtpri'
    CALL TYP_NULL(WTMAT)
    CALL TYP_NULL(WTMATSQR)
    CALL TYP_NULL(WTMATDEP)
    CALL TYP_NULL(WTMATDEPSQR)
    CALL TYP_NULL(PRIWTMAT)
    CALL TYP_NULL(PRIWTMATSQR)
    CALL UTL_DX_READ_WT(IOUT,OUTNAM,'_wt',WTMATDEP,WTMATDEPSQR)
    CALL UTL_DX_READ_WT(IOUT,OUTNAM,'_wtpri',PRIWTMAT,PRIWTMATSQR)
    CALL UTL_COMBINESQMATRIX(WTMATDEP,PRIWTMAT,WTMAT)
    CALL UTL_COMBINESQMATRIX(WTMATDEPSQR,PRIWTMATSQR,WTMATSQR)
    CALL TYP_DEALLOC(WTMATDEP)
    CALL TYP_DEALLOC(WTMATDEPSQR)
    CALL TYP_DEALLOC(PRIWTMAT)
    CALL TYP_DEALLOC(PRIWTMATSQR)
  ELSE
    WRITE(IOUT,1039) TRIM(OUTNAM)//'._wt'
    CALL TYP_NULL(WTMAT)
    CALL TYP_NULL(WTMATSQR)
    CALL UTL_DX_READ_WT(IOUT,OUTNAM,'_wt',WTMAT,WTMATSQR)
  ENDIF
  IF(PRSQRWT) THEN
    WRITE(IOUT,1040)
    CALL UTL_WRITECDMATRIX(WTMATSQR,1,IOUT,.TRUE.,DID,DID)
  ELSE
    WRITE(IOUT,1045)
  ENDIF
  !     READ AND WRITE SENSITIVITIES ._su
  OUTNAMTMP = TRIM(OUTNAM)//'._init'
  WRITE(IOUT,1069) TRIM(OUTNAMTMP)//'._su'
  ALLOCATE(PANAM(NPORIG),OBSNAM(NOBS),PRINAM(MPR), &
             PLOTSYMBOL(NOBS),PLOTSYMBOLPRI(MPR), &
             XPRI(NPORIG,MPR),XNPS(NPORIG,NOBS))
  CALL SEN_UEV_DX_READ_SU(NOBS,NPORIG,OBSNAM,OUTNAMTMP,PANAM,PLOTSYMBOL,XNPS)
  IF(MPR > 0) CALL SEN_UEV_DX_READ_SU &
                (MPR,NPORIG,PRINAM,OUTNAMTMP,PANAM,PLOTSYMBOLPRI,XPRI,'_supri')
  ! FILL FINAL SENSITIVITY ARRAY
  IF(NPS > NPF) THEN
    I = 0
    N = 0
    DO J=1,NPS
      IF(PINCRF(J) > -1) N=N+1
      IF(PINCRF(J) < 1) THEN
        CYCLE
      ELSE
        I = I+1
        IPTR(I) = N
        DO K=1,NOBS
          X(I,K) = XNPS(N,K)
        ENDDO
        IF(MPR > 0) THEN
          DO K=1,MPR
            X(I,K+NOBS) = XPRI(N,K)
          ENDDO
        ENDIF
      ENDIF
    ENDDO
    DO J=1,NPF
      DO I=1,NPF
        COV(J,I) = COVNPORIG(IPTR(J),IPTR(I))
      ENDDO
    ENDDO
  ELSE
    COV = COVNPORIG
    DO K=1,NOBS
      DO I=1,NPS
        X(I,K) = XNPS(I,K)
      ENDDO
    ENDDO
    IF(MPR > 0) THEN
      DO K=1,MPR
        DO I=1,NPS
          X(I,K+NOBS) = XPRI(I,K)
        ENDDO
      ENDDO
    ENDIF
  ENDIF
  IF(PRCOVPARAM) THEN
    WRITE(IOUT,1060)
    DO J=1,NPF
      IPTR(J)=J
    ENDDO
    CALL UTL_PARSQMATRIX(IOUT,6,NPF,NPF,COV,IPTR,PARNAM)
  ELSE
    WRITE(IOUT,1061)
  ENDIF
  DO I=1,NOBS
    DID(I) = OBSNAM(I)
    ISYM(I) = PLOTSYMBOL(I)
  ENDDO
  DO I=1,MPR
    DID(I+NOBS) = PRINAM(I)
    ISYM(I+NOBS) = PLOTSYMBOLPRI(I)
  ENDDO
  IF (ALLOCATED(XNPS)) DEALLOCATE(XNPS)
  IF (ALLOCATED(XPRI)) DEALLOCATE(XPRI)
  IF (ALLOCATED(XPRI)) DEALLOCATE(PLOTSYMBOLPRI)
  IF (ALLOCATED(XPRI)) DEALLOCATE(PLOTSYMBOL)
  IF (ALLOCATED(XPRI)) DEALLOCATE(OBSNAM)
  IF (ALLOCATED(XPRI)) DEALLOCATE(PRINAM)
  IF(PRSENSOPTPARAM) THEN
    WRITE(IOUT,1070)
    CALL PRTOTX(NTOT,NPF,IOUT,DID,PARNAM,X)
  ELSE
    WRITE(IOUT,1071)
  ENDIF
  !     MEAN TRUE ERRORS
  IF (IERT) THEN
    NMTE = 0
    CALL UTL_READBLOCK(MTECOLS,'MEAN_TRUE_ERROR',MTECOL, &
                       IINRS,IOUT,'*',.TRUE.,MTEHEAD,TAIL,NMTE)
    IF (NMTE>0) THEN
      CALL UTL_FILTERLIST(MTEHEAD,IOUT,'MTENAME',NTOT,IERR,MTENAM,MORE,MORE)
      CALL UTL_FILTERLIST(MTEHEAD,IOUT,'MTEVALUE',NTOT,IERR,MTEVAL,MORE)
      DO I=1,NTOT
        CALL UTL_CASE(MTENAM(I),MTENAMLC(I),-1)
        DO J=1,NTOT
          IF(MTENAMLC(I) .EQ. DIDLC(J)) THEN
            ET(J) = MTEVAL(I)
            EXIT
          ENDIF
          IF(J == NTOT) &
          CALL UTL_STOP('MTENAM DOES NOT MATCH OBSERVATION NAME IN _WS FILE')
        ENDDO
      ENDDO
    ENDIF
  ENDIF
  ! READ OBSERVATION COVARIANCE MATRIX, V, AND CONVERT TO V**.5
  IF (ICOV) THEN
    CALL TYP_NULL(VCDM)
    K = 0
    CALL UTL_READBLOCK(0,'MATRIX_FILES',NOCOL,IINRS,IOUT,'MATRIXFILE', &
                       .TRUE.,LLPTRMATFIL,TAIL,K)
    IF(K == 0)CALL UTL_STOP('CANNOT FIND V matrix in file')
    IF(K > 1)CALL UTL_STOP('ONLY ONE MATRIX FILES CAN BE READ')
    CALL UTL_FILTER(IERR,LLPTRMATFIL,IOUT,'MATRIXFILE',FN,K)
    INMATRIX = UTL_GETUNIT(7,100)
    OPEN (INMATRIX,FILE=FN,STATUS='OLD',ERR=10,IOSTAT=IOS)
    CALL UTL_READMATRIX(INMATRIX,IOUT,VCDM)
    ! UTLSVD returns inverse, sqrt of inv, & log of the det of the inv,
    ! so apply twice in order to get sqrt of the matrix
    CALL UTL_SVD(IFAIL,VCDM,VCDMSQR,DTVCDM)
    CALL UTL_SVD(IFAIL,VCDM,VCDMSQR,DTVCDM)
    V = VCDMSQR
  ENDIF
  CLOSE(IINRS)
  !
  !     CALCULATE W**.5 X, W**.5 ET, W**.5 V
  !        OBSERVATIONS WITH FULL WEIGHT MATRIX
  CALL UTL_MATMUL_SUB(NTOT,NPF,WTMATSQR,TRANSPOSE(X),X0)
  X = TRANSPOSE(X0)
  IF (IERT) THEN
    CALL UTL_MATMULVEC_SUB(NTOT,WTMATSQR,ET,ET)
  ENDIF
  IF (ICOV) THEN
    DO I=1,NTOT
      DO J=1,NTOT
        SUM=0.
        DO K=1,NTOT
          SUM=SUM+V(I,K)*UTL_GETVAL(WTMATSQR,K,J)
        ENDDO
        BUFF1(I,J)=SUM
      ENDDO
    ENDDO
    DO I=1,NTOT
      DO J=1,NTOT
        V(I,J)=BUFF1(I,J)
      ENDDO
    ENDDO
  ENDIF
  !
  !**COMPUTE R MATRIX
  !
  XCOV = MATMUL(TRANSPOSE(X),COV)
  R = MATMUL(XCOV,X)
  R = R/VAR
  !
  !**COMPUTE MEAN WEIGHTED RESIDUAL (EM), SLOPE OF THE PLOT OF WEIGHTED
  !     RESIDUALS VERSUS WEIGHTED SIMULATED VALUES (SLP), AND
  !     NON-LINEARITY MEASURE (QINT)
  EM=0.D0
  FS=0.D0
  EF=0.D0
  ES=0.D0
  QINT=0.D0
  DO J=1,NTOT
    EM=EM+E(J)
    ES=ES+E(J)*E(J)
    FS=FS+F(J)
    EF=EF+E(J)*F(J)
    SUM=0.D0
    DO I=1,NTOT
      SUM=SUM+R(I,J)*E(I)
    ENDDO
    QINT=QINT+SUM**2
  ENDDO
  RNT=NTOT
  EM=EM/RNT
  FM=FS/RNT
  FVR=0.D0
  DO I=1,NTOT
    FVR=FVR+(F(I)-FM)*(F(I)-FM)
  ENDDO
  SLP=(EF-EM*FS)/FVR
  WRITE(IOUT,1050) EM,SLP,QINT,ES
  !
  !**COMPUTE I-R MATRIX
  !
  R = -R
  DO J=1,NTOT
    R(J,J)=1.+R(J,J)
  ENDDO
  !
  !**INITIALIZE VARIABLES AND COMPUTE PLOTTING POSITIONS
  TMP=1./NTOT
  DO I=1,NTOT
    DM(I)=0.
    DV(I)=0.
    FRQ(I)=TMP*(I-.5)
    CALL STA_EVA_PROB_NORM_DISTRB(BUFF2(I),FRQ(I),-1)
    FRQ(I)=100.*FRQ(I)
  ENDDO
  !
  !**ORDER WEIGHTED RESIDUALS FROM SMALLEST TO LARGEST AND PRINT
  !     ORDERED RESIDUALS
  DO I=1,NTOT
    EI=E(I)
    DO J=I,NTOT
      IF (E(J).LT.EI) THEN
        TMP=EI
        EI=E(J)
        E(J)=TMP
        TMPDID=DID1(I)
        DID1(I)=DID1(J)
        DID1(J)=TMPDID
        ISYMTMP=ISYM1(I)
        ISYM1(I)=ISYM1(J)
        ISYM1(J)=ISYMTMP
      ENDIF
    ENDDO
    E(I)=EI
  ENDDO
  WRITE(IOUT,1075)
  CALL PRTOTD2(NTOT,DID1,IOUT,E,FRQ)
  !
  !**LOOP ON MONTE CARLO GENERATION OF SIMULATED RESIDUALS
  IF(NSETS >= 1) THEN
    IF (STDV <= 0.D0) STDV=SQRT(ES/(NTOT-NPF))
    ISET=0
    NSETSPR = NSETS/20
    DO IS=1,NSETS
      IF(NSETSPR > 0) THEN
        IF(MOD(IS,NSETSPR) .EQ. 0) WRITE(*,1001)IS,NSETS
      ELSE
        WRITE(*,1001)IS,NSETS
      ENDIF
    ! COMPUTE U AS N(0,VAR) RANDOM VARIABLES
      DO J=1,NTOT
        U(J)=STDV*RNDV(AM,IK,IKP,NRAN,ISET,RNSV)
      ENDDO
    ! MODIFY U WITH OBSERVATION COVARIANCE MATRIX AND MEAN TRUE
    ! ERRORS, IF SPECIFIED
      IF (ICOV) THEN
        DO I=1,NTOT
          SUM=0.
          DO J=1,NTOT
            SUM=SUM+V(I,J)*U(J)
          ENDDO
          BUFF3(I)=SUM
        ENDDO
        DO I=1,NTOT
          U(I)=BUFF3(I)
        ENDDO
      ENDIF
      IF (IERT) THEN
        DO I=1,NTOT
          U(I)=ET(I)+U(I)
        ENDDO
      ENDIF
    ! COMPUTE SIMULATED WEIGHTED RESIDUALS D AS CORRELATED NORMAL
    ! RANDOM VARIABLES
      DO J=1,NTOT
        SUM=0.D0
        DO I=1,NTOT
          SUM=SUM+R(I,J)*U(I)
        ENDDO
        D(J)=SUM
      ENDDO
    ! PRINT D, IF SPECIFIED
      IF (IPRN) THEN
        WRITE(IOUT,1110) IS
        WRITE(IOUT,1138)
        CALL PRTOTA(NTOT,DID,IOUT,D)
      ENDIF
    ! COMPUTE ORDERED, SIMULATED WEIGHTED RESIDUALS AND THEIR FIRST
    ! TWO MOMENTS
      DO I=1,NTOT
        DI=D(I)
        DO J=I,NTOT
          IF (D(J).LT.DI) THEN
            TMP=DI
            DI=D(J)
            D(J)=TMP
          ENDIF
        ENDDO
        D(I)=DI
        DM(I)=DM(I)+DI
        DV(I)=DV(I)+DI*DI
      ENDDO
      WRITE(ISCR) (D(I),I=1,NTOT)
    ! PRINT ORDERED D, IF SPECIFIED
      IF(IPRN) THEN
         WRITE(IOUT,1140)
         CALL PRTOTD2(NTOT,DID1,IOUT,D,FRQ)
      ENDIF
    ENDDO
    ! COMPUTE AND PRINT MEANS AND STANDARD DEVIATIONS OF D
    RN=NSETS
    RNM1=NSETS-1
    DMM=0.
    DO I=1,NTOT
      DM(I)=DM(I)/RN
      DMM=DMM+DM(I)
      DV(I)=SQRT((DV(I)-RN*DM(I)*DM(I))/RNM1)
    ENDDO
    DMM=DMM/RNT
    WRITE(IOUT,1142)
    CALL PRTOTD2(NTOT,DID1,IOUT,DM,FRQ)
    WRITE(IOUT,1144)
    CALL PRTOTD2(NTOT,DID1,IOUT,DV,FRQ)
    WRITE(IPLOTD,1470)
    WRITE(IPLOTD,1480) (E(I),DM(I),DV(I),2*DV(I),FRQ(I),BUFF2(I), &
                        DID1(I),ISYM1(I),I=1,NTOT)
    ! COMPUTE THE CUMULATIVE DISTRIBUTION FOR THE CORRELATIONS
      REWIND ISCR
      DO IS=1,NSETS
        READ(ISCR) (D(I),I=1,NTOT)
        DIM=0.
        DO I=1,NTOT
          DIM=DIM+D(I)
        ENDDO
        DIM=DIM/RNT
        DDM=0.
        DVR=0.
        DMVR=0.
        DO I=1,NTOT
          TMP=D(I)-DIM
          TMPA=DM(I)-DMM
          DDM=DDM+TMP*TMPA
          DVR=DVR+TMP*TMP
          DMVR=DMVR+TMPA*TMPA
        ENDDO
        CE(IS)=DDM*DDM/(DVR*DMVR)
      ENDDO
      DO I=1,NSETS
        CEI=CE(I)
        DO J=I,NSETS
          IF (CE(J).LT.CEI) THEN
            TMP=CEI
            CEI=CE(J)
            CE(J)=TMP
          ENDIF
        ENDDO
        CE(I)=CEI
      ENDDO
    ! COMPUTE THE CONFIDENCE LIMITS
      IC1=.99*RN
      IC2=.95*RN
      IC3=.90*RN
      CL99=SQRT(CE(IC1))
      CL95=SQRT(CE(IC2))
      CL90=SQRT(CE(IC3))
    ! COMPUTE THE CORRELATION OF THE WEIGHTED RESIDUALS AND THE
    ! MEAN SIMULATED RESIDUALS; COMPUTE THE PROBABILITY OF ITS
    ! OCCURRENCE
      ED=0.
      EVR=0.
      DMVR=0.
      DO I=1,NTOT
        TMP=E(I)-EM
        TMPA=DM(I)-DMM
        ED=ED+TMP*TMPA
        EVR=EVR+TMP*TMP
        DMVR=DMVR+TMPA*TMPA
      ENDDO
      CED=ED*ED/(EVR*DMVR)
      DO IS=1,NSETS
        IC=IS
        IF(CED.LT.CE(IS)) EXIT
      ENDDO
      PROB=IC/RN
      CED=SQRT(CED)
    ! PRINT CORRELATION, PROBABILITY, AND CONFIDENCE LIMITS
      WRITE(IOUT,1146) CED,PROB,CL99,CL95,CL90
    ! PRINT THE IDENTITY MINUS HAT MATRIX, IF SPECIFIED
  ENDIF
  IF(IPCF) THEN
    WRITE(IOUT,1148)
    CALL UTL_WRITEMATRIX(NTOT,NTOT,R,DID,DID,4,IOUT)
  ENDIF
  WRITE(*,900)TRIM(VERSION)
  WRITE(IOUT,900)TRIM(VERSION)
  CALL UTL_ENDTIME(IBDT,IOUT)
  CLOSE(IOUT)
  STOP
  10 IF (IOS.NE.0) THEN
    WRITE(*,1150) TRIM(FN)
    STOP
  ENDIF
  !
  END PROGRAM RESIDUAL_ANALYSIS_ADV
!===============================================================================
!===============================================================================
  SUBROUTINE PRTOTA(NO,DID,IOUT,VALB)
    !**PRINT VALUES IN TWO GROUPS OF THREE COLUMNS
    USE GLOBAL_DATA, ONLY: LENDNAM
    IMPLICIT NONE
    INTEGER,                            INTENT(IN) :: NO
    CHARACTER(LEN=LENDNAM),             INTENT(IN) :: DID(NO)
    INTEGER,                            INTENT(IN) :: IOUT
    DOUBLE PRECISION,                   INTENT(IN) :: VALB(NO)
    ! local variables
    INTEGER                                        :: K
    INTEGER                                        :: L
    INTEGER                                        :: NR
    20 FORMAT(2X,2(I4,2X,A,1X,G11.5,2X))
    NR=NO/2
    IF(2*NR.NE.NO) NR=NR+1
    DO K=1,NR
      WRITE(IOUT,20) (L,DID(L),VALB(L),L=K,NO,NR)
    ENDDO
    RETURN
  END SUBROUTINE PRTOTA
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
    500 FORMAT (3X,2(I3,2X,A,1X,G11.5,3X))
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
  SUBROUTINE PRTOTD2(NO,DID,IOUT,VALA,VALB)
    !**PRINT VALUES IN TWO GROUPS OF THREE COLUMNS
    USE GLOBAL_DATA, ONLY: LENDNAM
    IMPLICIT NONE
    INTEGER,                            INTENT(IN) :: NO
    CHARACTER(LEN=LENDNAM),             INTENT(IN) :: DID(NO)
    INTEGER,                            INTENT(IN) :: IOUT
    DOUBLE PRECISION,                   INTENT(IN) :: VALA(NO)
    DOUBLE PRECISION,                   INTENT(IN) :: VALB(NO)
    ! local variables
    INTEGER                                        :: L
    ! formats
    20 FORMAT(3X,I4,3X,A,3X,G11.5,3X,G11.5)
    !
    DO L=1,NO
      WRITE(IOUT,20) L,DID(L),VALA(L),VALB(L)
    ENDDO
    RETURN
  END SUBROUTINE PRTOTD2
!===============================================================================
!===============================================================================
  SUBROUTINE PRTOTX(NC,NR,IOUT,OBSNAM,PARNAM,X)
    USE GLOBAL_DATA, ONLY: LENDNAM
    !**PRINT MATRICES DIVIDED VERTICALLY INTO Three-COLUMN BLOCKS
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
    DO K = 1, NC, 3
      J1 = K + 2
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
  DOUBLE PRECISION FUNCTION RANUM(IK,IKP,IRAN,MA)
  ! RETURNS A UNIFORM (0,1) RANDOM NUMBER
    IMPLICIT NONE
    INTEGER          :: IK
    INTEGER          :: IKP
    INTEGER          :: IRAN
    DOUBLE PRECISION :: MA(55)
    ! local variables
    INTEGER          :: I
    INTEGER          :: II
    INTEGER          :: K
    DOUBLE PRECISION :: MBIG
    DOUBLE PRECISION :: MJ
    DOUBLE PRECISION :: MK
    DOUBLE PRECISION :: MS
    DOUBLE PRECISION :: MZ
    MBIG=4000000.
    MS=1618033.
    MZ=0.
    IF(IRAN.GT.0) GO TO 40
    IRAN=-IRAN
    MJ=MS-IRAN
    MJ=MOD(MJ,MBIG)
    MA(55)=MJ
    MK=1.
    DO I=1,54
      II=MOD(21*I,55)
      MA(II)=MK
      MK=MJ-MK
      IF(MK.LT.MZ) MK=MK+MBIG
      MJ=MA(II)
    ENDDO
    DO K=1,4
      DO I=1,55
        II=1+MOD(I+30,55)
        MA(I)=MA(I)-MA(II)
        IF(MA(I).LT.MZ) MA(I)=MA(I)+MBIG
      ENDDO
    ENDDO
    IK=0
    IKP=31
    IRAN=1
    40 IK=IK+1
    IF(IK.EQ.56) IK=1
    IKP=IKP+1
    IF(IKP.EQ.56) IKP=1
    MJ=MA(IK)-MA(IKP)
    IF(MJ.LT.MZ) MJ=MJ+MBIG
    MA(IK)=MJ
    RANUM=MJ/MBIG
    RETURN
  END FUNCTION RANUM
!===============================================================================
!===============================================================================
  DOUBLE PRECISION FUNCTION RNDV(AM,IK,IKP,IRAN,ISET,RNSV)
  !         RETURNS A NORMAL, N(0,1), RANDOM VARIABLE
      DOUBLE PRECISION :: AM(55)
      INTEGER          :: IK
      INTEGER          :: IKP
      INTEGER          :: IRAN
      INTEGER          :: ISET
      DOUBLE PRECISION :: RNSV
      ! local variables
      DOUBLE PRECISION :: FAC
      DOUBLE PRECISION :: R
      DOUBLE PRECISION :: RANUM
      DOUBLE PRECISION :: V1
      DOUBLE PRECISION :: V2
      !
      IF(ISET.GT.0) GO TO 20
   10 V1=2.*RANUM(IK,IKP,IRAN,AM)-1.
      V2=2.*RANUM(IK,IKP,IRAN,AM)-1.
      R=V1*V1+V2*V2
      IF(R.GE.1.) GO TO 10
      FAC=SQRT(-2.*DLOG(R)/R)
      RNSV=V1*FAC
      RNDV=V2*FAC
      ISET=1
      RETURN
   20 RNDV=RNSV
      ISET=0
      RETURN
    END FUNCTION RNDV
!===============================================================================
!===============================================================================
