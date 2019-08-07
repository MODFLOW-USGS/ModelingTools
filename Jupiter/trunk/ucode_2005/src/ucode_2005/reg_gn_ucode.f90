MODULE REG_GN_UCODE
  !
  USE DATATYPES
  !
  USE GLOBAL_DATA
  !
  USE DEPENDENTS, ONLY: NTOTOBS, NTOTPRED, NUSEOBS, NUSEPRED
  !
  USE REG_GNMOD, ONLY: &
  ! programs
      REG_GNMOD_INI1, REG_GNMOD_INI2, REG_GNMOD_GEN, REG_GNMOD_UNC, &
      REG_GNMOD_CLN, &
  !variables
      PINCBND, PINCSEN, WTADJFINAL
  !
  USE STATISTICS, ONLY: STA_UEV_DX_READ_DM
  !
  USE UTILITIES
  !
  USE UTLUCODE
  !
  IMPLICIT NONE
  SAVE
  PRIVATE
  ! Public subprograms
  PUBLIC REG_GN_INI_ALLOC, REG_GN_INI,  REG_GN_INI2, REG_GN_INIUNC, &
         REG_GN_GEN, REG_GN_UNC, REG_GN_CLN
  !   Public data
  PUBLIC &
    ALTSTART, AVET, CMAT, CONSECMAX, GNAMPRTS, STATS_ON_NONCONVERGE, ITERP, &
    MAXITER, MAXCHANGE, MAXSTEP, MINSENRAT, NTYP, NUMINTERVALS, &
    OMITIT, OMIT_DEFAULT, OMIT_INSENS, PAREST, PINC, PRNTA, PRNTB, PRNTJ, &
    QNITER, QNSOSR, QUASINEWTON, REINCSENRAT, SCALING, STEPTYPE, &
    TOLPAR, TOLSOSC, WTADJFINAL, &
    CFUSE, DSQ, INTTYP, ISGN, ISIGN, &
    NOINTCF, NPARICF, NPREDICF, &
    TOLINTP, TOLINTS, TOLINTY, &
    FL, RESIDSUNC, WTFULLUNC, XSENSTUNC, ZZ
  ! REG_GN_UCODE MODULE VARIABLES
  CHARACTER(LEN=3)                               :: ADSQ
  LOGICAL                                        :: ALTSTART = .FALSE.
  DOUBLE PRECISION                               :: AP
  LOGICAL                                        :: AN_ITER = .FALSE.
  DOUBLE PRECISION                               :: AVET
  DOUBLE PRECISION                               :: CFDSQ
  LOGICAL                                        :: CFUSE = .FALSE.
  CHARACTER(LEN=12)                              :: CHINTTYPE
  CHARACTER(LEN=29)                              :: CHNTYP
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: CMAT
  INTEGER                                        :: CNTINT = 1
  INTEGER                                        :: CONSECMAX
  DOUBLE PRECISION                               :: DETWTP
  DOUBLE PRECISION                               :: DMX
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: DSQ
  DOUBLE PRECISION                               :: DTLWTFULL
  DOUBLE PRECISION                               :: FL
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: GCFCF
  CHARACTER(LEN=LENDNAM),ALLOCATABLE,DIMENSION(:):: GNAMCF
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: GPOPTCF
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: GVARCF
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: GXOPTCF
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: GCF
  CHARACTER(LEN=LENDNAM),ALLOCATABLE,DIMENSION(:):: GNAM
  CHARACTER(LEN=LENDNAM),ALLOCATABLE,DIMENSION(:):: GNAMPRT
  CHARACTER(LEN=LENDNAM+1),ALLOCATABLE,DIMENSION(:):: GNAMPRTS
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: GPOPT
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: GVAR
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: GXOPT
  INTEGER                                        :: IAP
  INTEGER                                        :: IDSQ
  INTEGER                                        :: INTTYP = 1
  INTEGER                                        :: ISGN = 0
  INTEGER                                        :: ISIGN
  INTEGER                                        :: ITERP
  INTEGER                                        :: IUCF
  INTEGER                                        :: IUDMP
  INTEGER                                        :: INTUNIT
  INTEGER                                        :: INTUNITD
  INTEGER                                        :: INTUNITSUM
  INTEGER                                        :: INTUNITPAR
  INTEGER                                        :: INTUNITPARD
  INTEGER                                        :: JDAMP
  INTEGER                                        :: LASTIINT
  CHARACTER(LEN=6)                               :: LIMITS
  DOUBLE PRECISION                               :: MAXCHANGE
  CHARACTER(LEN=12)                              :: MAXCHANGEREALM
  INTEGER                                        :: MAXITER
  DOUBLE PRECISION                               :: MAXSTEP
  DOUBLE PRECISION                               :: MINSENRAT
  DOUBLE PRECISION                               :: MRQTDIRECTION
  DOUBLE PRECISION                               :: MRQTDEGREES
  DOUBLE PRECISION                               :: MRQTFACTOR
  DOUBLE PRECISION                               :: MRQTINCREMENT
  INTEGER                                        :: NOINTCF
  INTEGER                                        :: NPARICF
  INTEGER                                        :: NPI
  INTEGER, ALLOCATABLE, DIMENSION(:)             :: NPIPTR
  INTEGER                                        :: NPREDI
  INTEGER                                        :: NPREDICF
  INTEGER                                        :: NPREDGPS
  INTEGER                                        :: NTYP
  INTEGER                                        :: NUMINTERVALS
  INTEGER                                        :: NVARP
  LOGICAL                                        :: OMIT_INSENS = .FALSE.
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: OMITIT
  INTEGER                                        :: OMIT_DEFAULT
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: PAREST
  INTEGER, ALLOCATABLE, DIMENSION(:,:)           :: PINC
  INTEGER                                        :: PNPE
  DOUBLE PRECISION                               :: PRNTA = 0.D0
  DOUBLE PRECISION                               :: PRNTB = 0.D0
  INTEGER                                        :: PRNTJ = 0
  INTEGER                                        :: QNITER
  DOUBLE PRECISION                               :: QNSOSR
  LOGICAL                                        :: QUASINEWTON
  DOUBLE PRECISION                               :: REINCSENRAT
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: RESIDSUNC
  INTEGER, ALLOCATABLE, DIMENSION(:)             :: ROWNUM
  DOUBLE PRECISION                               :: SS
  LOGICAL                                        :: STATS_ON_NONCONVERGE
  CHARACTER(LEN=10)                              :: SCALING
  CHARACTER(LEN=10)                              :: STEPTYPE
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: STORINTPAR
  DOUBLE PRECISION                               :: TOLINTP = 1.D-2
  DOUBLE PRECISION                               :: TOLINTS = 0.D0
  DOUBLE PRECISION                               :: TOLINTY = 1.D-3
  DOUBLE PRECISION                               :: TOLPAR
  DOUBLE PRECISION                               :: TOLSOSC
  DOUBLE PRECISION                               :: TOLWTTOS
  LOGICAL                                        :: UPDATE_FINAL_SENS = .FALSE.
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: XD
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: XSENSTUNC
  TYPE (CDMATRIX)                                :: WTFULLUNC
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: ZZ
  !
  CONTAINS
!======================================================================
!======================================================================
  SUBROUTINE REG_GN_INI (IFAIL,INUNIT,IOUT,STDERRONE,OUTNAM,OUTNAMPRED, &
                         OPTNLUNC,OPTIMIZE,SENSITIVITIES,TRUSTREGION)
  !     VERSION 20031109 EPP
  !******************************************************************
  !     INITIALIZE PARAMETERS FOR GAUSS-NEWTON UPDATING
  !     Read, store, initialize and echo regression controls
  !******************************************************************
  !        SPECIFICATIONS:
  !------------------------------------------------------------------
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                        INTENT(INOUT)  :: IFAIL
    INTEGER,                        INTENT(IN)     :: INUNIT
    INTEGER,                        INTENT(IN)     :: IOUT
    LOGICAL,                        INTENT(IN)     :: STDERRONE
    CHARACTER(LEN=MAX_STRING_LEN),  INTENT(IN)     :: OUTNAM
    CHARACTER(LEN=MAX_STRING_LEN),  INTENT(IN)     :: OUTNAMPRED
    LOGICAL,                        INTENT(IN)     :: OPTNLUNC
    LOGICAL,                        INTENT(INOUT)  :: OPTIMIZE
    LOGICAL,                        INTENT(INOUT)  :: SENSITIVITIES
    LOGICAL,                        INTENT(INOUT)  :: TRUSTREGION
    !   Local variables
!    CHARACTER(LEN=3)                :: AWRP
    CHARACTER (LEN=12)                           :: CDUM
    DOUBLE PRECISION                             :: CF1
    CHARACTER(LEN=10)                            :: CHDATE
    CHARACTER(LEN=2000)                          :: CHECK
    CHARACTER(LEN=10)                            :: CHTIME
    CHARACTER(LEN=10)                            :: CHZONE
    CHARACTER (LEN=3)                            :: CONVERGE
    CHARACTER(LEN=2000)                          :: FOMIT
    CHARACTER(LEN=2000)                          :: FN
    CHARACTER(LEN=2000)                          :: FND
    DOUBLE PRECISION                             :: GDUM
    TYPE (LLIST), POINTER                        :: GNUCHEAD
    CHARACTER(LEN=40), DIMENSION(1)              :: GNUCCOL = (/ '      '/)
    TYPE (LLIST), POINTER                        :: GNLUNCCHEAD
    CHARACTER(LEN=40), DIMENSION(1)              :: GNLUNCCCOL = (/ '      '/)
    INTEGER                                      :: I
    INTEGER                                      :: IBDT(8)
    INTEGER, ALLOCATABLE, DIMENSION(:)           :: ID
    INTEGER                                      :: IDUM
    INTEGER                                      :: IUOMIT
    INTEGER                                      :: IUSS
    INTEGER                                      :: J
    LOGICAL                                      :: LEX
    INTEGER                                      :: NOINTCFORIG
    INTEGER                                      :: NGNUC
    INTEGER                                      :: NGNUCNLU
    INTEGER                                      :: NPARICFORIG
    INTEGER, ALLOCATABLE, DIMENSION(:)           :: NOPTR
    INTEGER                                      :: NUMIT
    CHARACTER (LEN=12), ALLOCATABLE, DIMENSION(:):: PARNAMTMP
    TYPE (LLIST), POINTER                        :: PTR
    TYPE (LNODE), POINTER                        :: PIPTR
    DOUBLE PRECISION                             :: RDUM
    CHARACTER (LEN=3)                            :: SENTYPE
    CHARACTER (LEN=19)                           :: STMP
    CHARACTER (LEN=4)                            :: STMP1
    CHARACTER (LEN=2)                            :: STMP2
    CHARACTER (LEN=2)                            :: STMP3
    CHARACTER (LEN=2)                            :: STMP5
    CHARACTER (LEN=2)                            :: STMP6
    CHARACTER (LEN=1)                            :: STMPB
    TYPE (LLIST), POINTER                        :: TAIL
    CHARACTER(LEN=79)                            :: TMP
    CHARACTER(LEN=79)                            :: TMP1
    DOUBLE PRECISION                             :: WDUM
    !
    !   Format statements
    100 FORMAT(1X,A)
    105 FORMAT(1X,A,I11)
    106 FORMAT(1X,A,I3,A)
    107 FORMAT(1X,A,F5.1,A)
    109 FORMAT(1X,A,F8.2)
    110 FORMAT(1X,A,1PE11.2)
    111 FORMAT(1X,A,A12)
    113 FORMAT(1X,5(1X,G15.6))
    114 FORMAT(1X,'PARAMETERS WILL BE OMITTED FROM THE REGRESSION IF THE RAT', &
       'IO',/,1X, &
       ' OF THEIR COMPOSITE SCALED SENSITIVITY TO THE MAXIMUM IS         < ', &
       1PE11.2)
    115 FORMAT(1X,'AN OMITTED PARAMETER WILL BE REINCLUDED IN THE  ', &
        'REGRESSION IF',/,1X, &
        ' THE RATIO OF ITS COMPOSITE SCALED SENSITIVITY TO THE MAXIMUM IS > ', &
        1PE11.2)
    120 FORMAT(/,1X,'OMIT_DEFAULT = ',I6)
    130 FORMAT(/,78('!'),/,1X,'MARQUARDT INCREMENT IS >= 1.0, RESETTING TO ', &
               F10.3,/,78('!'),/)
    131 FORMAT(/,78('!'),/,1X,'MARQUARDT FACTOR <= 1-MARQUARDT INCREMENT',/, &
               1X,'RESETTING TO',/,1X, &
               '(1-MARQUARDT INCREMENT)+10.*MARQUARDT INCREMENT, i.e., ', &
               1PE10.2,/,78('!'),/)
    132 FORMAT(/,78('!'),/,1X,'MARQUARDT DIRECTION IS < 1.0 OR >=90',/, &
               1X,'RESETTING TO: ',F10.2,/,78('!'),/)
    150 FORMAT(/,1X,'ECHO MODIFIED GAUSS-NEWTON UCODE', &
                  ' REGRESSION CONTROL INPUT:',/)
    250 FORMAT(/,1X,'ECHO NonLinear Intervals by GAUSS-NEWTON UCODE', &
                  ' INPUT CONTROLS:',/)
    275 FORMAT(//,1X,78('!'),/,1X,78('!'),//,5X,'ERROR:',//, &
        5X,'RUN CORFAC_PLUS TO GET fn._cfconf BEFORE CALCULATING INTERVALS',/, &
        5X,'REFER TO THE UCODE_2005 USER''S MANUAL FOR DETAILS.',//, &
        1X,78('!'),/,1X,78('!'),//)
    830 FORMAT(//, &
     ' CONTROLS FOR NON-LINEAR INTERVAL CALCULATIONS: ',//, &
     ' CONFIDENCE or PREDICTION .................................: ',A,/, &
     ' UPPER or LOWER LIMITS ....................................: ',A,/, &
     ' INDIVIDUAL or SIMULTANEOUS ...............................: ',A,/, &
     ' MAXIMUM NUMBER OF ITERATIONS..............................: ',G15.6,/, &
     ' TOLINTP (CONVERGENCE CRITERIA FOR PARAMETER CHANGE).......: ',G15.6,/, &
     ' TOLINTS (CONV. CRIT. FOR CHANGE OF OBJECTIVE FUNCTION)....: ',G15.6,/, &
     ' TOLINTY (CONV. CRIT. FOR CHANGE OF COMPUTED LIMIT VALUE)..: ',G15.6)
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine REG_GN_INI'
    IFAIL = 0
    !
    NULLIFY(GNUCHEAD)
    NULLIFY(GNLUNCCHEAD)
    NULLIFY(PTR,PIPTR)
    NULLIFY(TAIL)
    NGNUC = 0
    NGNUCNLU = 0
    !
    !   Read input block type OPTIONS
    !   First Set defaults
    AP = 0.D0
    AVET = 0.D0
    STATS_ON_NONCONVERGE = .TRUE.
    CONSECMAX = 5
    DETWTP = 0.D0
    DMX = 0.D0
    DTLWTFULL = 0.D0
    IAP=0
    ITERP = 0
    JDAMP = 0
    MAXCHANGE = 2.0
    MAXCHANGEREALM = 'NATIVE'
    MAXITER = 5
    MRQTDEGREES = DBLE(85.411137668)
    MRQTDIRECTION = DBLE(COS(MRQTDEGREES/360.0*(2.0*3.141592654)))
    MRQTFACTOR    = DBLE(1.5)
    MRQTINCREMENT = DBLE(0.001)
    NTYP = 1
    QNITER = 5
    OMIT_DEFAULT = 0
    OMIT_INSENS = .FALSE.
    MINSENRAT = 0.005D0
    REINCSENRAT = 0.02D0
    QUASINEWTON = .FALSE.
    TOLPAR = 1.D-02
    TOLSOSC = 0.D0
    QNSOSR = 1.D-2
    TOLWTTOS = 0.D0
    TRUSTREGION = .FALSE.
    SCALING = 'DEFAULT'
    STEPTYPE = 'HOOKSTEP'
    MAXSTEP = -1.D0
    !
    CALL UTL_READBLOCK(0,'REG_GN_CONTROLS',GNUCCOL,INUNIT,IOUT,  &
                       '*',.FALSE.,GNUCHEAD,TAIL,NGNUC)
    IF (NGNUC>0) THEN
      !   Traverse the list of options
      PTR => GNUCHEAD
      GNUCINPUT: DO
        IF (.NOT. ASSOCIATED(PTR)) EXIT GNUCINPUT  ! Pointer valid?
        PIPTR => PTR%LHEAD
        !   Traverse the entries
        INFO: DO
          IF (.NOT. ASSOCIATED(PIPTR)) EXIT INFO
          IF(PIPTR%KEYWORD == 'MRQTDIRECTION') THEN
            READ(PIPTR%VALUE,*) MRQTDEGREES
            IF(MRQTDEGREES <=1.D0 .OR. MRQTDEGREES >=90.D0)THEN
              MRQTDEGREES = DBLE(85.411137668)
              WRITE(IOUT,132)MRQTDEGREES
              WRITE(*,132)MRQTDEGREES
            ENDIF
            MRQTDIRECTION = COS(MRQTDEGREES/360.0*(2.0*3.141592654))
          ELSEIF(PIPTR%KEYWORD == 'MRQTFACTOR') THEN
            READ(PIPTR%VALUE,*) MRQTFACTOR
          ELSEIF(PIPTR%KEYWORD == 'MRQTINCREMENT') THEN
            READ(PIPTR%VALUE,*) MRQTINCREMENT
          ELSEIF(PIPTR%KEYWORD == 'STATS_ON_NONCONVERGE') THEN
             READ(PIPTR%VALUE,*) TMP1
            CALL UTL_CASE(TRIM(TMP1),TMP,1)
            IF (TMP == 'YES' .OR. TMP == 'Y' .OR.   &
              TMP == 'TRUE' .OR. TMP == 'T')   &
              STATS_ON_NONCONVERGE = .TRUE.
            IF (TMP == 'NO' .OR. TMP == 'N' .OR.   &
              TMP == 'FALSE' .OR. TMP == 'F')   &
              STATS_ON_NONCONVERGE = .FALSE.
          ELSEIF(PIPTR%KEYWORD == 'TOLPAR') THEN
            READ(PIPTR%VALUE,*) TOLPAR
          ELSEIF(PIPTR%KEYWORD == 'TOLSOSC') THEN
            READ(PIPTR%VALUE,*) TOLSOSC
          ELSEIF(PIPTR%KEYWORD == 'QUASINEWTON') THEN
            READ(PIPTR%VALUE,*) TMP1
            CALL UTL_CASE(TRIM(TMP1),TMP,1)
            IF (TMP == 'YES' .OR. TMP == 'Y' .OR.   &
              TMP == 'TRUE' .OR. TMP == 'T')   &
              QUASINEWTON = .TRUE.
            IF (TMP == 'NO' .OR. TMP == 'N' .OR.   &
              TMP == 'FALSE' .OR. TMP == 'F')   &
              QUASINEWTON = .FALSE.
          ELSEIF (PIPTR%KEYWORD == 'TOLPARWTOS') THEN
            READ(PIPTR%VALUE,*) TOLWTTOS
          ELSEIF(PIPTR%KEYWORD == 'QNITER') THEN
            READ(PIPTR%VALUE,*) QNITER
          ELSEIF(PIPTR%KEYWORD == 'QNSOSR') THEN
            READ(PIPTR%VALUE,*) QNSOSR
          ELSEIF(PIPTR%KEYWORD == 'MAXITER') THEN
            READ(PIPTR%VALUE,*) MAXITER
            IF(MAXITER > 99999) THEN
              WRITE(*,*)'MAXIMUM NUMBER ITERATIONS UNREASONABLE: ',MAXITER
              WRITE(IOUT,*)'MAXIMUM NUMBER ITERATIONS UNREASONABLE: ',MAXITER
              AMESSAGE = 'MAXIMUM NUMBER ITERATIONS MUST BE < 100,000'
              CALL UTL_STOP()
            ENDIF
          ELSEIF(PIPTR%KEYWORD == 'OMITDEFAULT') THEN
            READ(PIPTR%VALUE,*) OMIT_DEFAULT
          ELSEIF(PIPTR%KEYWORD == 'OMITINSENSITIVE') THEN
            READ(PIPTR%VALUE,*) TMP1
            CALL UTL_CASE(TRIM(TMP1),TMP,1)
            IF (TMP == 'YES' .OR. TMP == 'Y' .OR.   &
              TMP == 'TRUE' .OR. TMP == 'T')   &
              OMIT_INSENS = .TRUE.
            IF (TMP == 'NO' .OR. TMP == 'N' .OR.   &
              TMP == 'FALSE' .OR. TMP == 'F')   &
              OMIT_INSENS = .FALSE.
          ELSEIF(PIPTR%KEYWORD == 'MINIMUMSENSRATIO') THEN
            READ(PIPTR%VALUE,*) MINSENRAT
          ELSEIF(PIPTR%KEYWORD == 'REINCLUDESENSRATIO') THEN
            READ(PIPTR%VALUE,*) REINCSENRAT
          ELSEIF(PIPTR%KEYWORD == 'MAXCHANGE') THEN
            READ(PIPTR%VALUE,*) MAXCHANGE
          ELSEIF(PIPTR%KEYWORD == 'MAXCHANGEREALM') THEN
            READ(PIPTR%VALUE,*) TMP1
            CALL UTL_CASE(TRIM(TMP1),MAXCHANGEREALM,1)
            SELECT CASE (MAXCHANGEREALM)
              CASE ('REGRESSION')
                IAP = 1
              CASE ('NATIVE')
                IAP = 0
            END SELECT
          ELSEIF(PIPTR%KEYWORD == 'MAXSTEP') THEN
            READ(PIPTR%VALUE,*) MAXSTEP
          ELSEIF(PIPTR%KEYWORD == 'CONSECMAX') THEN
            READ(PIPTR%VALUE,*) CONSECMAX
          ELSEIF(PIPTR%KEYWORD == 'TRUSTREGION') THEN
            READ(PIPTR%VALUE,*) TMP1
            CALL UTL_CASE(TRIM(TMP1),TMP,1)
            SELECT CASE (TMP)
              CASE ('DOGLEG')
                STEPTYPE  = 'DOGLEG'
                TRUSTREGION = .TRUE.
              CASE ('HOOKSTEP')
                STEPTYPE  = 'HOOKSTEP'
                TRUSTREGION = .TRUE.
              CASE ('NO')
                TRUSTREGION = .FALSE.
            END SELECT
          ELSEIF(PIPTR%KEYWORD == 'SCALING') THEN
            READ(PIPTR%VALUE,*) TMP1
            CALL UTL_CASE(TRIM(TMP1),TMP,1)
            IF (TMP == 'NO' .OR. TMP == 'N' .OR. TMP == 'NONE' .OR. &
              TMP == 'FALSE' .OR. TMP == 'F')   &
              SCALING = 'NONE'
            IF (TMP == 'DEFAULT') &
              SCALING = 'DEFAULT'
          ELSEIF(PIPTR%KEYWORD == 'MAXSTEP') THEN
            READ(PIPTR%VALUE,*) MAXSTEP
          ENDIF
          PIPTR => PIPTR%NEXTNODE               ! Get pointer to next entry
        ENDDO INFO
        PTR => PTR%NEXTLIST                     ! Get pointer to next dependent
      ENDDO GNUCINPUT
      ! CHECK FOR INPUT THAT WILL NOT CAUSE INFINITE LOOPING
      IF(MRQTINCREMENT >= 1.D0) THEN
        MRQTINCREMENT = 0.001D0
        WRITE(IOUT,130)MRQTINCREMENT
        WRITE(*,130)MRQTINCREMENT
      ENDIF
      IF(MRQTFACTOR < (1-MRQTINCREMENT)) THEN
        MRQTFACTOR = (1-MRQTINCREMENT)+(500.D0*MRQTINCREMENT)
        WRITE(IOUT,131)MRQTFACTOR
        WRITE(*,131)MRQTFACTOR
      ENDIF
      IF(OPTNLUNC) THEN
        OMIT_DEFAULT = 0
        OMIT_INSENS = .FALSE.
        QUASINEWTON = .FALSE.
      ENDIF
      IF (STDERRONE) THEN
        SENSITIVITIES = .TRUE.
        OPTIMIZE = .TRUE.
        TOLPAR = HUGE(TOLPAR)
        MAXCHANGE = 1.D-20
      ENDIF
    ELSE
      IF(OPTIMIZE) THEN
        IF(IVERB > 1) THEN
          WRITE(IOUT,*)
          WRITE(IOUT,100) &
                    '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          WRITE(IOUT,100)'       NOTE REG_GN_CONTROLS BLOCK NOT FOUND '
          WRITE(IOUT,100)'                DEFAULTS ARE USED '
          WRITE(IOUT,100) &
                     'IF YOU WANT TO READ THIS BLOCK, CHECK BLOCK NAME SPELLING'
          WRITE(IOUT,100) &
                     '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          WRITE(IOUT,*)
        ENDIF
      ENDIF
    ENDIF
    IF (OMIT_DEFAULT > 0) THEN
      ALLOCATE(OMITIT(OMIT_DEFAULT))
      LEX = .FALSE.
      FOMIT = TRIM(OUTNAM)//'.omit'
      INQUIRE(FILE=FOMIT,EXIST=LEX)
      IF (LEX) THEN
        IUOMIT = UTL_GETUNIT(101,150)
        OPEN (IUOMIT,FILE=FOMIT)
        CHECK = ' '
        DO WHILE(CHECK .NE. 'BEGIN')
          READ(IUOMIT,*,END=200)TMP1
          CALL UTL_CASE(TRIM(TMP1),CHECK,1)
        ENDDO
        DO I=1,OMIT_DEFAULT
          READ(IUOMIT,*,END=201)CHECK
          IF (CHECK .EQ. 'END') THEN
            WRITE(*,120)OMIT_DEFAULT
            CALL UTL_STOP &
            ('"END" READ IN *.omit BEFORE ALL OMIT_DEFAULT VALUES WERE READ')
          ELSE
            BACKSPACE(IUOMIT)
            READ(IUOMIT,*,END=201)OMITIT(I)
          ENDIF
        ENDDO
        LEX = .FALSE.
      ELSE
        WRITE(*,120)OMIT_DEFAULT
        CALL UTL_STOP &
        ('OMIT_DEFAULT >0 in REG_GN_CONTROLS BLOCK, but FILE *.omit NOT FOUND')
      ENDIF
      READ(IUOMIT,*,END=202)CHECK
      IF (CHECK .NE. 'END') THEN
        WRITE(*,120)OMIT_DEFAULT
        CALL UTL_STOP &
        ('"END OMIT" NOT FOUND AFTER READING ALL OMIT_DEFAULT VALUES')
      ENDIF
    ENDIF
    IF (OPTIMIZE) THEN
    !   Write information (Gauss-Newton UCODE controls) to output file
      WRITE(IOUT,150)
      WRITE(IOUT,105) &
      'MAXIMUM NUMBER OF PARAMETER ITERATIONS                  =',MAXITER
      WRITE(IOUT,110) &
      'DEFAULT TOLERANCE ON % PARAMETER CHANGE FOR CLOSURE     =',TOLPAR
      WRITE(IOUT,110) &
      'TOLERANCE ON % CHANGE OF SOSC OVER 3 ITERATIONS         =',TOLSOSC
      WRITE(IOUT,110) &
      'MAXIMUM FRACTIONAL PARAMETER CHANGE                     =',MAXCHANGE
      WRITE(IOUT,111) &
      'MAXIMUM CHANGE APPLIES TO INDICATED PARAMETER SPACE         ', &
      MAXCHANGEREALM
      WRITE(IOUT,109) &
      'MARQUARDT DIRECTION (IN DEGREES)                        =',MRQTDEGREES
      WRITE(IOUT,110) &
      'MARQUARDT FACTOR                                        =',MRQTFACTOR
      WRITE(IOUT,110) &
      'MARQUARDT INCREMENT                                     =',MRQTINCREMENT
      IF (QUASINEWTON) THEN
        WRITE(IOUT,100) 'QUASINEWTON UPDATING WILL BE USED'
        WRITE(IOUT,106) &
        'AFTER ',QNITER,' PARAMETER ESTIMATION ITERATIONS'
        WRITE(IOUT,107) &
       'OR IF SOS DOES NOT CHANGE MORE THAN ',QNSOSR*100.D0,'% IN 2 ITERATIONS'
      ENDIF
      IF(OMIT_INSENS) THEN
        WRITE(IOUT,*)
        WRITE(IOUT,114) MINSENRAT
        WRITE(IOUT,115) REINCSENRAT
      ENDIF
      IF(OMIT_DEFAULT > 0) THEN
        WRITE(IOUT,*)
        WRITE(IOUT,100) &
          'AN OBSERVATION WILL BE OMITTED IF THE SIMULATED EQUIVALENT', &
          '                    IS EQUAL TO ANY OF THE FOLLOWING VALUES:'
        WRITE(IOUT,113)(OMITIT(I),I=1,OMIT_DEFAULT)
      ENDIF
      IF (TRUSTREGION) THEN
        WRITE(IOUT,100) 'TRUST REGION APPROACH WILL BE USED'
        WRITE(IOUT,111) &
        'TRUST REGION STEP TYPE IS                                 ',STEPTYPE
        IF(MAXSTEP > 0.D0)THEN
          WRITE(IOUT,110) &
          'MAXIMUM TRUST REGION STEP                               =',MAXSTEP
        ELSE
          WRITE(IOUT,100) 'MAXIMUM TRUST REGION CALCULATED BY ALGORITHM'
        ENDIF
        WRITE(IOUT,105) &
        'MAXIMUM NUMBER OF CONSECUTIVE MAXSTEPS                  =',CONSECMAX
        IF(SCALING .EQ. 'NONE') &
          WRITE(IOUT,100) 'LEAST SQUARES MATRIX WILL NOT BE SCALED'
      ENDIF
    ENDIF
    IF(OPTNLUNC) THEN
      LIMITS = 'BOTH'
      CHINTTYPE = 'INDIVIDUAL  '
      ADSQ = ' NO'
      IDSQ = 2
      CALL UTL_READBLOCK(0,'REG_GN_NONLININT ',GNLUNCCCOL,INUNIT,IOUT,  &
                       '*',.FALSE.,GNLUNCCHEAD,TAIL,NGNUCNLU)
      IF (NGNUCNLU>0) THEN
        !   Traverse the list of options
        PTR => GNLUNCCHEAD
        GNLNUNCINPUT: DO
          IF (.NOT. ASSOCIATED(PTR)) EXIT GNLNUNCINPUT  ! Pointer valid?
          PIPTR => PTR%LHEAD
          !   Traverse the entries
          NLUNCINFO: DO
            IF (.NOT. ASSOCIATED(PIPTR)) EXIT NLUNCINFO
            IF(PIPTR%KEYWORD == 'INDIVIDUALORSIMULTANEOUS') THEN
              READ(PIPTR%VALUE,*) TMP1
              CALL UTL_CASE(TRIM(TMP1),TMP,1)
              IF(TRIM(TMP) == 'INDIVIDUAL')THEN
                INTTYP = 1
                CHINTTYPE = 'INDIVIDUAL  '
              ELSEIF(TRIM(TMP) == 'SIMULTANEOUS')THEN
                INTTYP = 2
                CHINTTYPE = 'SIMULTANEOUS'
              ELSE
                !WRITE FAILURE MESSAGE
              ENDIF
            ELSEIF(PIPTR%KEYWORD == 'CONFIDENCEORPREDICTION') THEN
              READ(PIPTR%VALUE,*) TMP1
              CALL UTL_CASE(TRIM(TMP1),TMP,1)
              IF(TRIM(TMP) == 'CONFIDENCE')THEN
                NTYP = 1
              ELSEIF(TRIM(TMP) == 'PREDICTION')THEN
                NTYP = 2
              ELSE
                !WRITE FAILURE MESSAGE
              ENDIF
            ELSEIF(PIPTR%KEYWORD == 'WHICHLIMITS') THEN
              READ(PIPTR%VALUE,*) TMP1
              CALL UTL_CASE(TRIM(TMP1),LIMITS,1)
              SELECT CASE (TRIM(LIMITS))
                CASE ('LOWER')
                  ISGN = -1
                  LIMITS = 'LOWER'
                CASE ('UPPER')
                  ISGN = +1
                  LIMITS = 'UPPER'
                CASE ('BOTH')
                  ISGN =  0
                  LIMITS = 'BOTH'
              END SELECT
            ELSEIF(PIPTR%KEYWORD == 'CORRECTIONFACTORS') THEN
              READ(PIPTR%VALUE,*) TMP1
              CALL UTL_CASE(TRIM(TMP1),TMP,1)
              IF (TMP == 'YES' .OR. TMP == 'Y' .OR.   &
              TMP == 'TRUE' .OR. TMP == 'T')   &
              CFUSE = .TRUE.
              IF (TMP == 'NO' .OR. TMP == 'N' .OR.   &
              TMP == 'FALSE' .OR. TMP == 'F')   &
              CFUSE = .FALSE.
            ELSEIF(PIPTR%KEYWORD == 'ALTERNATIVESTARTVALUES') THEN
              READ(PIPTR%VALUE,*) TMP1
              CALL UTL_CASE(TRIM(TMP1),TMP,1)
              IF (TMP == 'YES' .OR. TMP == 'Y' .OR.   &
              TMP == 'TRUE' .OR. TMP == 'T')   &
              ALTSTART = .TRUE.
              IF (TMP == 'NO' .OR. TMP == 'N' .OR.   &
              TMP == 'FALSE' .OR. TMP == 'F')   &
              ALTSTART = .FALSE.
            ELSEIF(PIPTR%KEYWORD == 'TOLINTP') THEN
              READ(PIPTR%VALUE,*) TOLINTP
            ELSEIF(PIPTR%KEYWORD == 'TOLINTS') THEN
              READ(PIPTR%VALUE,*) TOLINTS
            ELSEIF(PIPTR%KEYWORD == 'TOLINTY') THEN
              READ(PIPTR%VALUE,*) TOLINTY
            ENDIF
            PIPTR => PIPTR%NEXTNODE              ! Get pointer to next entry
          ENDDO NLUNCINFO
          PTR => PTR%NEXTLIST                    ! Get pointer to next dependent
        ENDDO GNLNUNCINPUT
        IF (TOLINTP.LT.0.) TOLINTP=0.D0
        IF (TOLINTS.LT.0.) TOLINTS=0.D0
        IF (TOLINTY.LT.0.) TOLINTY=0.D0
        IF (TOLINTP < 1.D-12 .AND. TOLINTS < 1.D-12 .AND. TOLINTY < 1.D-12) &
          CALL UTL_STOP &
          ('ERROR! ALL CRITERIA (TOLINTP TOLINTS TOLINTY) ARE TOO SMALL')
        !   Write information (Gauss-Newton NonLinearIntervals UCODE controls)
        WRITE(IOUT,250)
        ! check for reasonable input combinations
        IF(INTTYP == 2 .AND. NTYP == 2) THEN
          AMESSAGE = 'CANNOT EVALUATE SIMULTANEOUS PREDICTION INTERVALS'
          CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
          AMESSAGE = 'NOW RESETTING IndividualOrSimulataneous to INDIVIDUAL '
          CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','yes')
          INTTYP = 1
        ENDIF
        IF(CFUSE) THEN
          WRITE(IOUT,100)'Use Correction Factors?                   = Yes'
        ELSE
          WRITE(IOUT,100)'Use Correction Factors?                   = No'
        ENDIF
        WRITE(IOUT,110) &
        'TOLERANCE ON % CHANGE OF SOSC OVER 3 ITERATIONS         =',TOLSOSC
        ! Open unit for intervals
        CALL DATE_AND_TIME(CHDATE,CHTIME,CHZONE,IBDT)
        WRITE(STMP1,'(I4)')IBDT(1)
        IF(IBDT(2) < 10) THEN
          WRITE(STMPB,'(I1)')IBDT(2)
          STMP2 = '0'//STMPB
        ELSE
          WRITE(STMP2,'(I2)')IBDT(2)
        ENDIF
        IF(IBDT(3) < 10) THEN
          WRITE(STMPB,'(I1)')IBDT(3)
          STMP3 = '0'//STMPB
        ELSE
          WRITE(STMP3,'(I2)')IBDT(3)
        ENDIF
        IF(IBDT(5) < 10) THEN
          WRITE(STMPB,'(I1)')IBDT(5)
          STMP5 = '0'//STMPB
        ELSE
          WRITE(STMP5,'(I2)')IBDT(5)
        ENDIF
        IF(IBDT(6) < 10) THEN
          WRITE(STMPB,'(I1)')IBDT(6)
          STMP6 = '0'//STMPB
        ELSE
          WRITE(STMP6,'(I2)')IBDT(6)
        ENDIF
        STMP = &
        '--'//STMP1//'_'//STMP2//'_'//STMP3//'--'//STMP5//'.'//STMP6

        INTUNIT = UTL_GETUNIT(101,150)
        IF(NTYP == 1) THEN
          FN = TRIM(OUTNAMPRED)//'._intconf'
          FND = TRIM(OUTNAMPRED)//trim(STMP)//'._intconf'
        ELSE
          FN = TRIM(OUTNAMPRED)//'._intpred'
          FND = TRIM(OUTNAMPRED)//trim(STMP)//'._intpred'
        ENDIF
        OPEN(UNIT=INTUNIT,FILE=FN,STATUS='REPLACE')
        INTUNITD = UTL_GETUNIT(101,150)
        OPEN(UNIT=INTUNITD,FILE=FND,STATUS='REPLACE')

        INTUNITSUM = UTL_GETUNIT(101,150)
        IF(NTYP == 1) THEN
          FN = TRIM(OUTNAMPRED)//'._intconfsum'
        ELSE
          FN = TRIM(OUTNAMPRED)//'._intpredsum'
        ENDIF
        OPEN(UNIT=INTUNITSUM,FILE=FN,STATUS='REPLACE')

        ! Open unit for parameters for intervals
        INTUNITPAR = UTL_GETUNIT(101,150)
        IF(NTYP == 1) THEN
          FN = TRIM(OUTNAMPRED)//'._intconfpar'
          FND = TRIM(OUTNAMPRED)//trim(STMP)//'._intconfpar'
        ELSE
          FN = TRIM(OUTNAMPRED)//'._intpredpar'
          FND = TRIM(OUTNAMPRED)//trim(STMP)//'._intpredpar'
        ENDIF
        OPEN(UNIT=INTUNITPAR,FILE=FN,STATUS='REPLACE')
        INTUNITPARD = UTL_GETUNIT(101,150)
        OPEN(UNIT=INTUNITPARD,FILE=FND,STATUS='REPLACE')

        IF(NTYP == 1) THEN
          CHNTYP = 'CONFIDENCE                   '
        ELSEIF(NTYP == 2) THEN
          CHNTYP = 'PREDICTION                   '
        ELSE
          CHNTYP = 'UNKNOWN review CORFAC results'
        ENDIF

        ! write to output
        WRITE(IOUT,830) &
            TRIM(CHNTYP),TRIM(LIMITS),TRIM(CHINTTYPE), &
            MAXITER,TOLINTP,TOLINTS,TOLINTY
        ! Read data from _dm
        CALL UTLUCODE_DX_READ_DM(IFAIL,OUTNAM, &
                                 RDUM,RDUM,RDUM,RDUM, &
                                 CONVERGE,NUMIT,RDUM,RDUM,RDUM, &
                                 CDUM,CDUM,CDUM,CDUM, &
                                 IDUM,IDUM,IDUM,PNPE,IDUM,IDUM, &
                                 RDUM,RDUM,RDUM,RDUM,IOUT,RDUM,RDUM,SENTYPE)
        FN = TRIM(OUTNAMPRED)//'._dmp'
        INQUIRE(FILE=FN,EXIST=LEX)
        IF(LEX) THEN
          IUDMP = UTL_DX_OPEN(OUTNAMPRED,'_dmp','OLD')
          LEX = .FALSE.
          READ(IUDMP,*,END=801)CDUM,NPREDGPS
          READ(IUDMP,*,END=801)CDUM,NVARP
          801 CLOSE(UNIT=IUDMP)
          PNPE = NVARP
        ENDIF
        IFAIL = 0
        IUSS = UTL_DX_OPEN(OUTNAM,'_ss','OLD')
        READ(IUSS,*)
        DO I=1,NUMIT+1
          READ(IUSS,*)IDUM,RDUM,RDUM,SS,IDUM
        ENDDO
        IUSS = UTL_DX_CLOSE('_ss')
        ! CRITICAL VALUES
        ! READ CORFACS
        IF(NTYP == 1) THEN
          INQUIRE(FILE=TRIM(OUTNAMPRED)//'._cfconf',EXIST=LEX)
          IF(LEX) THEN
            LEX = .FALSE.
          ELSE
            WRITE(*,275)
            WRITE(IOUT,275)
            IFAIL = -999
            RETURN
          ENDIF
          IUCF = UTL_DX_OPEN(OUTNAMPRED,'_cfconf','OLD')
        ELSE
          INQUIRE(FILE=TRIM(OUTNAMPRED)//'._cfpred',EXIST=LEX)
          IF(LEX) THEN
            LEX = .FALSE.
          ELSE
            WRITE(*,275)
            WRITE(IOUT,275)
            IFAIL = -999
            RETURN
          ENDIF
          IUCF = UTL_DX_OPEN(OUTNAMPRED,'_cfpred','OLD')
        ENDIF
        ! READ Header
        READ(IUCF,*,END=300)
        NPREDICF = 0
        NPARICF = 0
        NPARICFORIG = 0
        DO
          READ(IUCF,*,END=97)CDUM,IDUM,RDUM,RDUM,CF1
          IF(IDUM == 1) THEN
            NPREDICF = NPREDICF + 1
          ELSEIF(IDUM ==2) THEN
            NPARICFORIG = NPARICFORIG + 1
          ENDIF
        ENDDO
        97 NOINTCFORIG = NPREDICF + NPARICFORIG
        ALLOCATE(NOPTR(NOINTCFORIG))
        REWIND(IUCF)
        ! READ Header
        READ(IUCF,*,END=300)
        NOPTR = 1
        DO I=1,NOINTCFORIG
          READ(IUCF,*,END=98)CDUM,IDUM,RDUM,RDUM,CF1
          IF(IDUM ==2) THEN
            IF(CF1 == 0.D0) THEN
              NOPTR(I) = 0
            ELSE
              NPARICF = NPARICF + 1
            ENDIF
          ENDIF
        ENDDO
        98 NOINTCF = NPREDICF + NPARICF
        REWIND(IUCF)
        ALLOCATE(GNAMCF(NOINTCF),GPOPTCF(NOINTCF),GVARCF(NOINTCF), &
                GXOPTCF(PNPE,NOINTCF),GCFCF(NOINTCF),ID(NOINTCF), &
                PARNAMTMP(PNPE))
        READ(IUCF,*,END=300)
        J = 1
        DO I=1,NOINTCFORIG
          READ(IUCF,*,END=300)CDUM,IDUM,GDUM,WDUM,CF1,CFDSQ
          IF(CF1 > 0.D0) THEN
            GNAMCF(J) = CDUM
            GPOPTCF(J) = GDUM
            GVARCF(J) = WDUM
            GCFCF(J) = CF1
            J = J + 1
          ENDIF
        ENDDO
        CLOSE(IUCF)
        FN = TRIM(OUTNAMPRED)//'._cfsu'
        INQUIRE(FILE=FN,EXIST=LEX)
        IF(LEX) THEN
          LEX = .FALSE.
          !     READ SENSITIVITIES OF PREDICTIONS WITH RESPECT TO PARAMETERS
          CALL UTLUCODE_DX_READ_CFSU(NOINTCF,NOINTCFORIG,PNPE,NOPTR, &
                                     OUTNAMPRED,GNAMCF,PARNAMTMP,ID,GXOPTCF)
        ELSE
          AMESSAGE = &
          'FILE fn._cfsu WAS NOT FOUND, TERMINATING'
          WRITE(*,*)TRIM(AMESSAGE)
          CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
          CALL UTL_STOP(' ')
        ENDIF
      ENDIF
    ENDIF
    RETURN
    200 CALL UTL_STOP &
      ('FILE *.omit DOES NOT CONTAIN "BEGIN" BLOCK LABEL')
    RETURN
    201 CALL UTL_STOP &
      ('END OF FILE REACHED on *.omit BEFORE ALL DEFAULT_OMIT VALUES WERE READ')
    RETURN
    202 CALL UTL_STOP &
      ('END OF FILE REACHED on *.omit BEFORE "END OMIT" WAS READ')
    RETURN
    300 CALL UTL_STOP &
      ('END OF FILE REACHED on *._CORFAC')
    RETURN
  END SUBROUTINE REG_GN_INI
!======================================================================
!======================================================================
  SUBROUTINE REG_GN_INI2 (IOUT,MPR,NOBS,NPE,NPS, &
                          NONLININT,NONLINPARAM,OBSNAM,PARNAM)
  !**********************************************************************
  ! ASSIGN VALUES TO PREDICTIONS FOR WHICH INTERVALS ARE TO BE EVALUATED
  !**********************************************************************
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                        INTENT(IN)     :: IOUT
    INTEGER,                        INTENT(IN)     :: MPR
    INTEGER,                        INTENT(IN)     :: NOBS
    INTEGER,                        INTENT(IN)     :: NPE
    INTEGER,                        INTENT(IN)     :: NPS
    LOGICAL,                        INTENT(IN)     :: NONLININT(NPS)
    LOGICAL,                        INTENT(IN)     :: NONLINPARAM
    CHARACTER(LEN=LENDNAM),         INTENT(IN)     :: OBSNAM(NOBS)
    CHARACTER(LEN=12),              INTENT(IN)     :: PARNAM(NPS)
    ! Local variables
    CHARACTER(LEN=1) :: CHR
    INTEGER          :: DOF
    INTEGER          :: I
    INTEGER          :: II
    INTEGER          :: IP
    INTEGER          :: J
    INTEGER          :: K
    INTEGER          :: NITEMS
    DOUBLE PRECISION :: STATVAL
    !
    ! Formats
    830 FORMAT(//,80('*'),//, &
     ' INFORMATION ON ITEMS FOR INTERVAL EVALUATION:',//,   &
     ' NUMBER OF NON-LINEAR INTERVALS FOR EVALUATION.........: ',I6,/,  &
     ' INTERVAL LIMITS.......................................: ',A6,/, &
     ' NUMBER OF NON-LINEAR LIMITS...........................: ',I6,/,  &
     ' STATISTIC FOR CALCULATING CRITICAL VALUE..............: ',G13.6,/)
    831 FORMAT(80('*'),//, &
               1X,'CRITICAL VALUES:',//)
    832 FORMAT(5X,'Correction Factor = 1.0 for all intervals',/)
    833 FORMAT(5X, &
             'Correction Factor for Confidence Region used for all intervals',/)
    834 FORMAT(5X,'ITERVAL             ',3X,'CRITICAL VALUE',/, &
               5X,'--------------------',3X,'---------------')
    835 FORMAT(5X,'ALL INTERVALS',12X,1PE15.6)
    836 FORMAT(5X,A20,3X,1PE15.6)
    837 FORMAT(5X,A12,11X,1PE15.6)
    ! HOW MANY INTERVALS ON PARAMETERS?
    IF(NONLINPARAM) THEN
      NPI = 0
      DO I=1,NPS
        IF(NONLININT(I)) NPI = NPI + 1
      ENDDO
    ENDIF
    !REBUILD INPUT FROM CORFACS TO ONLY INCLUDE ITEMS LISTED IN PREDS BLOCK
    NUSEOBS = NUSEOBS
    NPREDI = NUSEPRED
    NITEMS = NPREDI + NPI
    NUMINTERVALS = ABS(ISGN) * NITEMS
    IF (NUMINTERVALS == 0) NUMINTERVALS = 2 * NITEMS
    IF(NITEMS == 0)CALL UTL_STOP('NO INTERVALS TO EVALUATE')
    ALLOCATE(GNAMPRT(NUMINTERVALS),GNAMPRTS(NUMINTERVALS), &
             ROWNUM(NUMINTERVALS),STORINTPAR(NPE,NUMINTERVALS+1))
    ROWNUM = 0
    STORINTPAR = 0.D0
    ALLOCATE(GNAM(NITEMS),GPOPT(NITEMS),GVAR(NITEMS),GCF(NITEMS), &
             GXOPT(PNPE,NITEMS))
    DO I=1,NPREDI
      II = NUSEOBS + I
      DO J=1,NOINTCF
        IF(UTL_SAMENAME(OBSNAM(II),GNAMCF(J))) THEN
          GNAM(I) = GNAMCF(J)
          GPOPT(I)= GPOPTCF(J)
          GVAR(I) = GVARCF(J)
          GCF(I)  = GCFCF(J)
          DO K=1,PNPE
            GXOPT(K,I)=GXOPTCF(K,J)
          ENDDO
          EXIT
        ENDIF
        IF(J == NOINTCF) CALL UTL_STOP &
        ('NO MATCH BETWEEN PREDICTION NAME IN PREDICTION_DATA BLOCK & _corfac')
      ENDDO
    ENDDO
    I = NPREDI
    ALLOCATE(NPIPTR(NPI))
    DO IP=1,NPS
      IF(NONLININT(IP)) THEN
        I = I + 1
        NPIPTR(I-NPREDI) = IP
        DO J=1,NOINTCF
          IF(UTL_SAMENAME(PARNAM(IP),GNAMCF(J))) THEN
            GNAM(I) = GNAMCF(J)
            GPOPT(I)= GPOPTCF(J)
            GVAR(I) = GVARCF(J)
            GCF(I)  = GCFCF(J)
            DO K=1,PNPE
              GXOPT(K,I)=GXOPTCF(K,J)
            ENDDO
            EXIT
          ENDIF
          IF(J == NOINTCF) CALL UTL_STOP &
        ('*NO MATCH BETWEEN PREDICTION NAME IN PREDICTION_DATA BLOCK & _corfac')
        ENDDO
      ENDIF
    ENDDO
    ! DETERMINE CRITICAL VALUES
    DOF = NUSEOBS + MPR - NPE
    IF(INTTYP == 1) THEN
      ! INDIVIDUAL INTERVAL GET Tstat
      CALL UTL_STUD_T(DOF,STATVAL)
    ELSE
      ! PRED INTERVAL GET SCHEFFE, Fstat
      CALL UTL_FSTT(PNPE,DOF,STATVAL)
    ENDIF
    WRITE(IOUT,830)NITEMS,LIMITS,NUMINTERVALS,STATVAL
    ALLOCATE(DSQ(NITEMS))
    WRITE(IOUT,831)
    IF(.NOT. CFUSE) THEN
      ADSQ = 'YES'
      IDSQ = 1
      CFDSQ = 1.D0
    ENDIF
    IF(IDSQ == 1) THEN
      ! Calculate one DSQ
      IF(INTTYP == 1) THEN
        DSQ = SS * (1.D0 + (CFDSQ * STATVAL**2) / DOF)
      ELSE   ! INTTYP == 2 simultaneous
        DSQ = SS * (1.D0 + (NPE * CFDSQ * STATVAL) / DOF)
      ENDIF
    ELSE
      ! Calculate DSQs
      IF(INTTYP == 1) THEN
        DO I=1,NITEMS
          DSQ(I) = SS * (1.D0 + (GCF(I) * STATVAL**2) / DOF)
        ENDDO
      ELSE   ! INTTYP == 2 simultaneous
        DO I=1,NITEMS
          DSQ(I) = SS * (1.D0 + (NPE * GCF(I) * STATVAL) / DOF)
        ENDDO
      ENDIF
    ENDIF
    IF(IDSQ == 1) THEN
      IF(.NOT. CFUSE) THEN
        WRITE(IOUT,832)
      ELSE
        WRITE(IOUT,833)
      ENDIF
      WRITE(IOUT,834)
      WRITE(IOUT,835)DSQ(1)
    ELSE
      WRITE(IOUT,834)
      DO I=1,NPREDI
        WRITE(IOUT,836)GNAM(I),DSQ(I)
      ENDDO
      IF(NITEMS > NPREDI) THEN
        DO I=NPREDI+1,NITEMS
          WRITE(IOUT,837)GNAM(I),DSQ(I)
        ENDDO
      ENDIF
    ENDIF
    IF(NITEMS == NUMINTERVALS) THEN
      GNAMPRT = GNAM
      IF(ISGN > 0) THEN
        CHR = '+'
      ELSE
        CHR = '-'
      ENDIF
      DO I=1,NITEMS
        GNAMPRTS(I) = CHR//TRIM(GNAM(I))
      ENDDO
    ELSE
      J = 0
      DO I=1,NITEMS
        J=J+1
        GNAMPRT(J) = GNAM(I)
        GNAMPRTS(J) = '-'//TRIM(GNAM(I))
        J=J+1
        GNAMPRT(J) = GNAM(I)
        GNAMPRTS(J) = '+'//TRIM(GNAM(I))
      ENDDO
    ENDIF
    DEALLOCATE(GNAMCF,GPOPTCF,GVARCF,GXOPTCF,GCFCF)
    RETURN
  END SUBROUTINE REG_GN_INI2
!======================================================================
!======================================================================
  SUBROUTINE REG_GN_INIUNC (IOUT,INTERVALLOOPS,ITERP, &
                            NOBS,NPE,NPS,MODELVAL, &
                            PVAL,RESIDS,WTFULL,XSENST,IINT)
  !**********************************************************************
  ! INITIALIZE NEXT NONLINEAR INTREVAL CALCULATION
  !**********************************************************************
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                        INTENT(IN)     :: IOUT
    INTEGER,                        INTENT(IN)     :: INTERVALLOOPS
    INTEGER,                        INTENT(IN)     :: ITERP
    INTEGER,                        INTENT(IN)     :: NOBS
    INTEGER,                        INTENT(IN)     :: NPE
    INTEGER,                        INTENT(IN)     :: NPS
    DOUBLE PRECISION,               INTENT(IN)     :: MODELVAL(NOBS)
    DOUBLE PRECISION,               INTENT(IN)     :: PVAL(NPS)
    DOUBLE PRECISION,               INTENT(IN)     :: RESIDS(NOBS)
    TYPE (CDMATRIX),                INTENT(IN)     :: WTFULL
    DOUBLE PRECISION,               INTENT(IN)     :: XSENST(NPE,NOBS)
    INTEGER,                        INTENT(INOUT)  :: IINT
    ! Local variables
    INTEGER              :: I
    INTEGER              :: IERR = 0
    INTEGER              :: J
    ! FORMATS
    400 FORMAT(//,80('='),/,80('='))
    401 FORMAT(//,1X,' BEGIN EVALUATING INTERVAL: ',I5,3X,A,2X,A,//)
    515 FORMAT (/,' "ITERATION"  "SSE"        "GOAL"     "DETERMINANT" ', &
        '"MARQUARDT"   "DAMPING" "MAXIMUM CHANGE" "FOR PARAM" "LAMBDA" ', &
        '"PREDICTION ERR" "PREDICTION INTERVAL LIMIT"')
    ! INITIALIZE NONLINEAR UNCERTAINTY GN ARRAYS
    IF(IINT == 0) THEN
      ALLOCATE (RESIDSUNC(NUSEOBS),XSENSTUNC(NPE,NUSEOBS),ZZ(NPE))
      CALL TYP_NULL(WTFULLUNC)
      LASTIINT = 0
    ENDIF
    ! INITIALIZE FOR NEW INTERVAL
    ! ESTABLISH UPPER OR LOWER INTERVAL FOR THIS CASE
    IF(ITERP == 0) THEN
      WRITE(INTUNITSUM,515)
      IF(ISGN == 0) THEN
        IINT = (MOD(INTERVALLOOPS,2) + INTERVALLOOPS) / 2
        IF(MOD(INTERVALLOOPS,2) == 1) THEN
          ! WHEN BOTH EVEN VALUES ARE UPPER LIMITS
          ISIGN = -1
          LIMITS = 'LOWER LIMIT'
        ELSE
          ! WHEN BOTH ODD VALUES ARE LOWER LIMITS
          ISIGN = 1
          LIMITS = 'UPPER LIMIT'
        ENDIF
      ELSE
        IINT = ABS(INTERVALLOOPS)
        ISIGN = ISGN
      ENDIF
      WRITE(IOUT,400)
      WRITE(IOUT,401)IINT,TRIM(GNAM(IINT)),TRIM(LIMITS)
      WRITE(*,400)
      WRITE(*,401)IINT,TRIM(GNAM(IINT)),TRIM(LIMITS)
      LASTIINT = IINT
    ENDIF
    ! USE ONLY OBSERVATIONS, NOT PREDICTIONS FOR REGRESSION
    DO I = 1,NUSEOBS
      RESIDSUNC(I) = RESIDS(I)
      DO J=1,NPE
        XSENSTUNC(J,I) = XSENST(J,I)
      ENDDO
    ENDDO
    CALL UTL_GET_MATRIX_SECTION(IERR,WTFULL,1,NUSEOBS,1,NUSEOBS,WTFULLUNC)
    IF(IINT <= NPREDI) THEN
      FL = MODELVAL(IINT+NUSEOBS)
      DO J=1,NPE
        ZZ(J) = XSENST(J,IINT+NUSEOBS)
      ENDDO
    ELSE
      FL = PVAL(NPIPTR(IINT-NPREDI))
      ZZ = 0.D0
      ZZ(NPIPTR(IINT-NPREDI)) = 1.D0
    ENDIF
    RETURN
  END SUBROUTINE REG_GN_INIUNC
!======================================================================
!======================================================================
  SUBROUTINE REG_GN_INI_ALLOC (IFAIL,NOBS,NPE,NPS, &
                               STDERRONE,IFO,PMAXCHANGE,PTOLPAR)
  !     VERSION 20031109 EPP
  !******************************************************************
  !     ALLOCATE AND INITIALIZE ARRAYS FOR GAUSS-NEWTON UPDATING
  !******************************************************************
  !        SPECIFICATIONS:
  !------------------------------------------------------------------
    IMPLICIT NONE
    ! Argument list variables
    INTEGER,                                 INTENT(INOUT) :: IFAIL
    INTEGER,                                 INTENT(IN)    :: NOBS
    INTEGER,                                 INTENT(IN)    :: NPE
    INTEGER,                                 INTENT(IN)    :: NPS
    LOGICAL,                                 INTENT(IN)    :: STDERRONE
    INTEGER,                                 INTENT(INOUT) :: IFO
    DOUBLE PRECISION, DIMENSION(NPS),        INTENT(INOUT) :: PMAXCHANGE
    DOUBLE PRECISION, DIMENSION(NPS),        INTENT(INOUT) :: PTOLPAR
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine REG_GN_INI_ALLOC'
    IFAIL = 0
    ! Allocate
    ALLOCATE (PINC(MAXITER+1,NPS),XD(NOBS,NPE),PAREST(MAXITER,NPS))
    ! Initialize
    IFO = 0
    PAREST = 0.D0
    IF(STDERRONE) THEN
      PMAXCHANGE = 1.D-20
      PTOLPAR = HUGE(PTOLPAR)
    ENDIF
    RETURN
  END SUBROUTINE REG_GN_INI_ALLOC
  !======================================================================
  !======================================================================
  SUBROUTINE REG_GN_GEN (IFAIL,MPR,NOBS,NPE,NPS, &
                         CONSTRAIN,CONSTRAINL,CSS,DATAEXCHANGE,IOUT,IOMIT, &
                         IPTR,LN,CREATEINITFILES,OPTIM,OUTNAM, &
                         PADJ,PARNAM,PMAXCHANGE, &
                         PRIWTMAT,PTOLPAR,PVALINIT,PVALMAXC,PVALMINC, &
                         RESIDS,RESIDSPRI,RSQALL, &
                         STDERRONE,UPDATE_FINAL_SENS,WTFULL,WTOSL, &
                         XPRI,XSENST,FINALSTATS,CTRLDONE,GNUDONE, &
                         IEND,IFO,IND,ISENMETHOD,PVAL)
  !     VERSION 20031109 EPP
  !     MODIFIED FROM MF2K PES1GAU1AP VERSION 20020708 ERB
  !******************************************************************
  !     UPDATE PARAMETER VALUES VIA MODIFIED GAUSS-NEWTON REGRESSION
  !******************************************************************
  !        SPECIFICATIONS:
  !------------------------------------------------------------------
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                         INTENT(INOUT) :: IFAIL
    INTEGER,                         INTENT(IN)    :: MPR
    INTEGER,                         INTENT(IN)    :: NOBS
    INTEGER,                         INTENT(IN)    :: NPE
    INTEGER,                         INTENT(IN)    :: NPS
    LOGICAL,                         INTENT(IN)    :: CONSTRAIN(NPS)
    LOGICAL,                         INTENT(IN)    :: CONSTRAINL
    DOUBLE PRECISION,                INTENT(IN)    :: CSS(NPE)
    LOGICAL,                         INTENT(IN)    :: DATAEXCHANGE
    INTEGER,                         INTENT(IN)    :: IOUT
    INTEGER,                         INTENT(IN)    :: IOMIT
    INTEGER,                         INTENT(IN)    :: IPTR(NPE)
    INTEGER,                         INTENT(IN)    :: LN(NPS)
    LOGICAL,                         INTENT(IN)    :: CREATEINITFILES
    LOGICAL,                         INTENT(IN)    :: OPTIM
    CHARACTER(LEN=MAX_STRING_LEN),   INTENT(IN)    :: OUTNAM
    LOGICAL,                         INTENT(IN)    :: PADJ(NPS)
    CHARACTER(LEN=12),               INTENT(IN)    :: PARNAM(NPS)
    LOGICAL,                         INTENT(IN)    :: STDERRONE
    DOUBLE PRECISION,                INTENT(IN)    :: PMAXCHANGE(NPS)
    TYPE (CDMATRIX),                 INTENT(IN)    :: PRIWTMAT
    DOUBLE PRECISION,                INTENT(IN)    :: PTOLPAR(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: PVALINIT(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: PVALMAXC(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: PVALMINC(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: RESIDS(NOBS)
    DOUBLE PRECISION,                INTENT(IN)    :: RESIDSPRI(MPR)
    DOUBLE PRECISION,                INTENT(IN)    :: RSQALL(MAXITER+2)
    LOGICAL,                         INTENT(IN)    :: UPDATE_FINAL_SENS
    TYPE (CDMATRIX),                 INTENT(IN)    :: WTFULL
    LOGICAL,                         INTENT(IN)    :: WTOSL
    DOUBLE PRECISION,                INTENT(IN)    :: XPRI(NPE,MPR)
    DOUBLE PRECISION,                INTENT(INOUT) :: XSENST(NPE,NOBS)
    LOGICAL,                         INTENT(INOUT) :: FINALSTATS
    LOGICAL,                         INTENT(INOUT) :: CTRLDONE
    LOGICAL,                         INTENT(INOUT) :: GNUDONE
    INTEGER,                         INTENT(INOUT) :: IEND
    INTEGER,                         INTENT(INOUT) :: IFO
    INTEGER,                         INTENT(INOUT) :: IND
    INTEGER,                         INTENT(INOUT) :: ISENMETHOD(NPS)
    DOUBLE PRECISION,                INTENT(INOUT) :: PVAL(NPS)
    !
    ! Local Variables
    INTEGER                                       :: I
    INTEGER                                       :: J
    !
    ! Formats
    599 FORMAT(//,80('*'),/,2X, &
    '"Problem Evaluated with Standard Error = 1"'&
    ,/,80('*'),/)
    600 FORMAT(//,80('*'),/,2X,'Parameter Estimation CONVERGED: ', &
    '% change of PARAMETER VALUES less than TolPar', &
    /,80('*'),/)
    601 FORMAT(//,80('-'),/,2X, &
    'Parameter Estimation CONVERGED: % change of', &
    ' SOSWR in 3 Iterations less than TolSOSC',/,80('-'),/)
    602 FORMAT(//,80('!'),/,2X, &
    'Parameter Estimation DID NOT CONVERGE in ', I8,' iterations', &
    /,80('!'),/)
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine REG_GN_GEN'
    IFAIL = 0
    IF (IFO == 2) GO TO 9999
    !
    IF (ITERP .EQ. 0) CALL REG_GNMOD_INI1 (MAXITER,MPR,NPE)
    IF(.NOT. UPDATE_FINAL_SENS) ITERP = ITERP + 1
    !-------INITIALIZE FOR EVERY ITERATION
    IND = 0
    ! DETERMINE IF ANY PARAMETERS ARE INSENSITIVE AND SET UP ARRAYS TO OMIT THEM
      CALL REG_GNMOD_INI2(IOUT,MPR,NOBS,NPE,NPS,CSS,IOMIT,IPTR,MINSENRAT, &
                        OMIT_INSENS,PADJ,PARNAM,REINCSENRAT,XPRI,XSENST)
    ! IF WE ARE HERE JUST TO SET UP ARRAYS TO DO FINAL STATISTICS ON THE
    ! SET OF ESTIMATED VALUES WITH THE LOWEST SOS THEN BY PASS GAUSS NEWTON
    IF(UPDATE_FINAL_SENS) RETURN
    ! UPDATE PARAMETERS
      CALL REG_GNMOD_GEN (IFAIL,IOUT,MPR,NOBS,NPE,NPS, &
                        CONSTRAIN,CONSTRAINL,DATAEXCHANGE,IAP,IPTR, &
                        MAXCHANGEREALM, &
                        MRQTDIRECTION,MRQTINCREMENT,MRQTFACTOR, &
                        MAXITER,LN,OMIT_INSENS,OUTNAM,CREATEINITFILES, &
                        PARNAM,PMAXCHANGE, &
                        PRIWTMAT,PTOLPAR,PVALINIT,PVALMAXC,PVALMINC, &
                        QNITER,QUASINEWTON,RESIDS,RESIDSPRI,RSQALL, &
                        QNSOSR,TOLWTTOS,WTFULL,WTOSL,XSENST, &
                        AP,DMX,FINALSTATS, &
                        IFO,IND, &
                        ITERP,JDAMP,PAREST,PVAL)
    ! IF OPTIMIZATION IS IN PROGRESS, UPDATE FLAGS, OTHERWISE THESE SETTINGS
    ! WILL INTERFERE WITH PREDICTION OR LINEARITY EXECUTIONS SO BYPASS
    IF(OPTIM) THEN
      J=0
      DO I=1,NPS
        IF(.NOT. PADJ(I)) THEN
          PINC(ITERP,I) = -1
        ELSE
          J = J + 1
          IF(PINCSEN(J) .EQ. 0 .AND. PINCBND(J) .EQ. 0) THEN
            PINC(ITERP,I) = 1
          ELSE
            PINC(ITERP,I) = 0
          ENDIF
        ENDIF
      ENDDO
    ENDIF
    9999 IF(.NOT. GNUDONE) THEN
      ! EVALUATE THE SITUATION
      IF (IFO > 0 .AND. IFO < 4) THEN
        ! TIME FOR PRECISE SENSITIVITY CALCULATION
        DO I=1,NPS
          IF(ISENMETHOD(I) .EQ. 1) ISENMETHOD(I) = 2
        ENDDO
      ENDIF
      ! IFO 1 MEANS CONVERGED TOLPAR
      IF (IFO .EQ. 1) THEN
        IEND = 600
      ! IFO 2 MEANS CONVERGED TolSOSC
      ELSEIF (IFO .EQ. 2) THEN
        IEND = 601
      ! IFO 3 MEANS NOTCONVERGED in MAXIT
      ELSEIF (IFO .EQ. 3) THEN
        IEND = 602
      ENDIF
      IF (IFO .GT. 0) THEN
        IF (IEND == 600) THEN
          IF(STDERRONE) THEN
            WRITE(IOUT,599)
            WRITE(*,599)
          ELSE
            WRITE(IOUT,600)
            WRITE(*,600)
          ENDIF
        ELSEIF (IEND == 601) THEN
          WRITE(IOUT,601)
          WRITE(*,601)
        ELSEIF (IEND == 602) THEN
          WRITE(IOUT,602)MAXITER
          WRITE(*,602)MAXITER
        ENDIF
      ENDIF
      IF (IFO .GT. 0) THEN
        GNUDONE = .TRUE.
      ENDIF
      IF (ITERP .GT. MAXITER) THEN
        CTRLDONE = .TRUE.
      ENDIF
    ENDIF
    !
    RETURN
  END SUBROUTINE REG_GN_GEN
  !======================================================================
  !======================================================================
  SUBROUTINE REG_GN_UNC (IFAIL,IOUT,IINT, &
                         MPR,NPE,NPS,AN_ITER, &
                         BSCAL,IPTR,LN,PARNAM, &
                         PRIWTMAT,PVALINIT, &
                         RESIDS,RESIDSPRI,RSQALL, &
                         UPDATE_FINAL_SENS,WTFULL,XPRI, &
                         FL,GNUDONE,IEND,IFO,IND,PVAL,XSENST,ZZ)
  !     VERSION 20050815 EPP
  !******************************************************************
  !     ESTIMATE NONLINEAR INTERVAL VIA MODIFIED GAUSS-NEWTON REGRESSION
  !******************************************************************
  !        SPECIFICATIONS:
  !------------------------------------------------------------------
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                         INTENT(INOUT) :: IFAIL
    INTEGER,                         INTENT(IN)    :: IOUT
    INTEGER,                         INTENT(IN)    :: IINT
    INTEGER,                         INTENT(IN)    :: MPR
    INTEGER,                         INTENT(IN)    :: NPE
    INTEGER,                         INTENT(IN)    :: NPS
    LOGICAL,                         INTENT(IN)    :: AN_ITER
    DOUBLE PRECISION,                INTENT(IN)    :: BSCAL(NPS)
    INTEGER,                         INTENT(IN)    :: IPTR(NPE)
    INTEGER,                         INTENT(IN)    :: LN(NPS)
    CHARACTER(LEN=12),               INTENT(IN)    :: PARNAM(NPS)
    TYPE (CDMATRIX),                 INTENT(IN)    :: PRIWTMAT
    DOUBLE PRECISION,                INTENT(IN)    :: PVALINIT(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: RESIDS(NUSEOBS)
    DOUBLE PRECISION,                INTENT(IN)    :: RESIDSPRI(MPR)
    DOUBLE PRECISION,                INTENT(IN)    :: RSQALL(MAXITER+2)
    LOGICAL,                         INTENT(IN)    :: UPDATE_FINAL_SENS
    TYPE (CDMATRIX),                 INTENT(IN)    :: WTFULL
    DOUBLE PRECISION,                INTENT(IN)    :: XPRI(NPE,MPR)
    DOUBLE PRECISION,                INTENT(INOUT) :: FL
    LOGICAL,                         INTENT(INOUT) :: GNUDONE
    INTEGER,                         INTENT(INOUT) :: IEND
    INTEGER,                         INTENT(INOUT) :: IFO
    INTEGER,                         INTENT(INOUT) :: IND
    DOUBLE PRECISION,                INTENT(INOUT) :: PVAL(NPS)
    DOUBLE PRECISION,                INTENT(INOUT) :: XSENST(NPE,NUSEOBS)
    DOUBLE PRECISION,                INTENT(INOUT) :: ZZ(NPE)
    !
    ! Local Variables
    CHARACTER(LEN=20)                             :: COL12NAM(2)
    DOUBLE PRECISION                              :: FLCNV
    DOUBLE PRECISION                              :: GV
    !
    ! Formats
    600 FORMAT(//,80('*'),/,2X,'Parameter Estimation CONVERGED: ',/,10X, &
    '% change of PARAMETER VALUES less than TolIntP',/,80('*'),/)
    601 FORMAT(//,80('-'),/,2X,'Parameter Estimation CONVERGED: ',/,10X, &
    '% change of SOSWR in 3 Iterations less than TolIntS',/,80('-'),/)
    602 FORMAT(//,80('-'),/,2X,'Parameter Estimation CONVERGED: ',/,10X, &
    '% change of computed confidence or prediction limit less than TolIntY', &
    /,80('-'),/)
    603 FORMAT(//,80('!'),/,2X, &
    'Parameter Estimation DID NOT CONVERGE in ', I8,' iterations', &
    /,80('!'),/)
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine REG_GN_UNC'
    IFAIL = 0
    IF(.NOT. AN_ITER) THEN
    ! CHECK FOR CONFVERGENCE
      FLCNV = 1.D0
      CALL REG_GNMOD_UNC (IFAIL,IOUT,MPR,NUSEOBS,NPE,NPS,AN_ITER,BSCAL, &
                        CHINTTYPE,CNTINT,DSQ(IINT),FL,FLCNV,GNAM(IINT), &
                        GPOPT(IINT),IINT,INTUNIT,INTUNITD,INTUNITSUM,IPTR, &
                        ISIGN,ITERP,NTYP,LN,MAXCHANGE,MAXITER, &
                        MRQTDIRECTION,MRQTINCREMENT,MRQTFACTOR, &
                        NUMINTERVALS,PARNAM,PRIWTMAT,PVALINIT, &
                        RESIDS,RESIDSPRI,RSQALL,TOLINTP,TOLINTS,TOLINTY, &
                        GV,WTFULL,XPRI,AP,DMX,IFO,IND,PVAL,&
                        ROWNUM,STORINTPAR,XSENST,ZZ)
      IF(.NOT. GNUDONE) THEN
        ! EVALUATE THE SITUATION
        ! IFO 1 MEANS CONVERGED TOLINTP
        IF (IFO .EQ. 1) THEN
          IEND = 600
        ! IFO 2 MEANS CONVERGED TOLINTS
        ELSEIF (IFO .EQ. 2) THEN
          IEND = 601
        ! IFO 3 MEANS CONVERGED TOLINTY
        ELSEIF (IFO .EQ. 3) THEN
          IEND = 602
        ELSEIF (ITERP == MAXITER) THEN
          IFO = 4
          IEND = 603
        ENDIF
        IF (IFO .GT. 0) THEN
          IF (IEND == 600) THEN
            WRITE(IOUT,600)
            WRITE(*,600)
          ELSEIF (IEND == 601) THEN
            WRITE(IOUT,601)
            WRITE(*,601)
          ELSEIF (IEND == 602) THEN
            WRITE(IOUT,602)
            WRITE(*,602)
          ELSEIF (IEND == 603) THEN
            WRITE(IOUT,603)MAXITER
            WRITE(*,603)MAXITER
          ENDIF
        ENDIF
        ! CHECK if NOT CONVERGED in MAXIT
        IF(ITERP == MAXITER) THEN
          IFO=4
        ENDIF
        IF (IFO .GT. 0 .OR. ITERP .GT. MAXITER) CNTINT = CNTINT + 1
        IF (ITERP .GT. MAXITER) ITERP = MAXITER
      ENDIF
      IF(CNTINT > NUMINTERVALS) THEN
        COL12NAM(1) = 'INTERVAL NAME'
        COL12NAM(2) = 'LIMIT IDENTIFIER'
        CALL UTLUCODE_DX_WRITE_MATRIX &
             (INTUNITPAR,NUMINTERVALS,NPE,NPS,IPTR,PARNAM,STORINTPAR, &
              GNAMPRT,ROWNUM,COL12NAM)
        CALL UTLUCODE_DX_WRITE_MATRIX &
             (INTUNITPARD,NUMINTERVALS,NPE,NPS,IPTR,PARNAM,STORINTPAR, &
              GNAMPRT,ROWNUM,COL12NAM)
        CLOSE(INTUNITPAR)
        CLOSE(INTUNITPARD)
      ENDIF
      RETURN
    ENDIF
    !
    IF (ITERP .EQ. 0) CALL REG_GNMOD_INI1 (MAXITER,MPR,NPE)
    IF(.NOT. UPDATE_FINAL_SENS) ITERP = ITERP + 1
    !-------INITIALIZE FOR EVERY ITERATION
    IND = 0
    FLCNV = 1.D0
    IF(NTYP == 1) THEN
      GV = 0.D0
    ELSE
      GV = GVAR(IINT)
    ENDIF
    ! UPDATE INTERVAL
    CALL REG_GNMOD_UNC (IFAIL,IOUT,MPR,NUSEOBS,NPE,NPS,AN_ITER,BSCAL,CHINTTYPE, &
                        CNTINT,DSQ(IINT),FL,FLCNV,GNAM(IINT),GPOPT(IINT), &
                        IINT,INTUNIT,INTUNITD,INTUNITSUM,IPTR,ISIGN,ITERP, &
                        NTYP,LN,MAXCHANGE,MAXITER, &
                        MRQTDIRECTION,MRQTINCREMENT,MRQTFACTOR, &
                        NUMINTERVALS,PARNAM,PRIWTMAT,PVALINIT, &
                        RESIDS,RESIDSPRI,RSQALL,TOLINTP,TOLINTS,TOLINTY, &
                        GV,WTFULL,XPRI,AP,DMX,IFO,IND,PVAL,&
                        ROWNUM,STORINTPAR,XSENST,ZZ)
    !
    RETURN
  END SUBROUTINE REG_GN_UNC
!======================================================================
!======================================================================
  SUBROUTINE REG_GN_CLN (IFO,IOUT,OPTNLUNC,STDERRONE,IFAILHOLD)
  ! Deallocate and write status at end of execution
  IMPLICIT NONE
  ! Argument list variables
    INTEGER,                         INTENT(IN) :: IFO
    INTEGER,                         INTENT(IN) :: IOUT
    LOGICAL,                         INTENT(IN) :: OPTNLUNC
    LOGICAL,                         INTENT(IN) :: STDERRONE
    INTEGER, OPTIONAL,               INTENT(IN) :: IFAILHOLD
    ! FORMATS
    499 FORMAT(//,80('*'),/,80('*'),/,2X, &
    '"Problem Evaluated with Standard Error = 1"'&
    ,/,80('*'),/,80('*'))
    500 FORMAT(//,80('*'),/,80('*'),/,2X,'Parameter Estimation CONVERGED: ', &
    '% change of PARAMETER VALUES less than TolPar', &
    /,80('*'),/,80('*'))
    501 FORMAT(//,80('='),/,80('='),/,2X, &
    'Parameter Estimation CONVERGED: % change of', &
    ' SOSWR in 3 Iterations less than TolSOSC',/,80('-'),/,80('-'))
    502 FORMAT(//,80('!'),/,80('!'),/,2X, &
    'Parameter Estimation DID NOT CONVERGE in ', I8,' iterations', &
    /,80('!'),/,80('!'))
    901 FORMAT(//,80('*'),/,80('*'),/,2X, &
    'Calculation of Non-Linear Confidence Intervals is Complete.',/,2X, &
    ' View fn.#unonlinint_conf for Convergence Status of Each Interval', &
    /,80('*'),/,80('*'))
    902 FORMAT(//,80('*'),/,80('*'),/,2X, &
    'Calculation of Non-Linear Prediction Intervals is Complete.',/,2X, &
    'View fn.#unonlinint_pred for Convergence Status of Each Interval', &
    /,80('*'),/,80('*'))
    ! Confirm Deallocate
    CALL REG_GNMOD_CLN()
    !
    IF (ALLOCATED(PAREST)) DEALLOCATE(PAREST)
    IF (ALLOCATED(XD)) DEALLOCATE(XD)
    IF (ALLOCATED(RESIDSUNC)) DEALLOCATE(RESIDSUNC)
    IF (ALLOCATED(XSENSTUNC)) DEALLOCATE(XSENSTUNC)
    IF (ALLOCATED(ZZ)) DEALLOCATE(ZZ)
    !
    !
    IF(OPTNLUNC) THEN
      IF(IFAILHOLD .NE. -999) THEN
        IF(NTYP == 1) THEN
          WRITE(*,901)
          WRITE(IOUT,901)
        ELSE
          WRITE(*,902)
          WRITE(IOUT,902)
        ENDIF
      ENDIF
    ELSE
      IF (IFO .EQ. 1) THEN
        IF(STDERRONE) THEN
          WRITE(IOUT,499)
          WRITE(*,499)
        ELSE
          WRITE(IOUT,500)
          WRITE(*,500)
        ENDIF
      ELSEIF (IFO .EQ. 2) THEN
        WRITE(IOUT,501)
        WRITE(*,501)
      ELSEIF (IFO .EQ. 3) THEN
        WRITE(IOUT,502)MAXITER
        WRITE(*,502)MAXITER
      ENDIF
    ENDIF
    RETURN
  END SUBROUTINE REG_GN_CLN
!=======================================================================
!=======================================================================
END MODULE REG_GN_UCODE
