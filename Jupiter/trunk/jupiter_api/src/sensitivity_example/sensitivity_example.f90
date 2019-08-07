PROGRAM SENSITIVITY_EXAMPLE
!   Example program using JUPITER API modules
!   This program calculates sensitivities of model-calculated
!   dependents to parameters.
!
USE DATATYPES
USE GLOBAL_DATA, ONLY: AMESSAGE, HYPHENS, IVERB, LENDNAM, MAX_STRING_LEN
USE UTILITIES
USE SENSITIVITY_EXAMPLE_MOD, ONLY:   &
      !   DATA
      AVET, BINC, BSCAL, DNPP, IPERTURBMETHOD, IPRINT, IPTR, ISENMETHOD,   &
      LN, MAXRECL, MCVCAT, MCVUSE, MODELVAL, NCATS, NDINC, NNEGT,   &
      NOBS, NOMIT, NOPNT, NPOST, NRUNS, NW, OBSNAM, OUTNAME, PADJ, PARNAM,   &
      PRECISPRO, PVAL, PVALTMP, RESIDS, RSQ, RSQP, WTDRESIDS,   &
      WTMAT, WTMATSQR, WTRL, WTVEC,   &
      !   SUBROUTINES
      MAIN_INI, MAIN_INI_OBS, MAIN_INI_PARS, MAIN_DEF, MAIN_UEV_BSCALMESS,  &
      MAIN_CLN
USE BASIC, ONLY:   &
      COVMATARR, DERIV_INTERFACE, BAS_INI_COVMAT, BAS_INI_GETOPTIONS,   &
      BAS_INI_MODELEXEC, BAS_GEN, BAS_EXE_SELECT, BAS_EXE, BAS_CLN
USE MODEL_IO, ONLY:   &
      MIO_INI_ALLOC, MIO_INI_ARRAYS, MIO_INI_DIMENSION, MIO_INI_INPUTFILES,   &
      MIO_INI_OUTPUTFILES, MIO_INI_INSTRUCT1, MIO_INI_INSTRUCTALLOC,   &
      MIO_INI_INSTRUCT2, MIO_INI_TEMPLATE, MIO_ADA_WRITEFILES, MIO_EXT
USE DEPENDENTS, ONLY:   &
      OBSVAL, WTCORRELATED, DEP_DX_WRITE_GM, DEP_INI_ALLOC, DEP_INI_READ,   &
      DEP_INI_STORE, DEP_UEV_DX_WRITE_OS, DEP_UEV_DX_WRITE_P,   &
      DEP_UEV_DX_WRITE_R, DEP_UEV_RESIDUALS, DEP_UEV_WRITEOBSTABLE, DEP_CLN
USE SENSITIVITY, ONLY:   &
      SEN_INI, SEN_DEF, SEN_GEN, SEN_EXE_SELECT, SEN_UEV_POPX_MODCALC,   &
      SEN_UEV_POPXROW_DIFF, SEN_UEV_LNX, SEN_UEV_DX_WRITE_MATRIX,   &
      SEN_UEV_WRITESENTABLE, SEN_CLN
USE STATISTICS, ONLY: STA_INI, STA_UEV_INIT, STA_UEV_FIT, STA_CLN
USE EQUATION, ONLY: EQN_INI
USE PARALLEL_PROCESSING, ONLY:   &
      PLL_INI_DISPATCHER, PLL_MAKE_RUNS, PLL_CLN, PLL_STOP_RUNNERS
IMPLICIT NONE
!
!   Fortran parameters
CHARACTER(LEN=19) :: PROGNAM
PARAMETER (PROGNAM='SENSITIVITY_EXAMPLE')
INTEGER JOBDIM, JOBLEN
PARAMETER (JOBDIM=10, JOBLEN=20)
!
INTEGER               :: INUNIT = 9  ! Unit number for input file
INTEGER               :: IOUT = 10   ! Unit number for output file
INTEGER               :: ISU         ! Unit number for _su file of unscaled sensitivities
INTEGER, DIMENSION(8) :: IBDT        ! Begin time
CHARACTER(LEN=10)     :: CHDATE, CHTIME, CHZONE
!
!   Declare variables
CHARACTER(LEN=MAX_STRING_LEN) :: FILEIN=' '  ! Input data file name
CHARACTER(LEN=MAX_STRING_LEN) :: FILEOUT     ! Output data file name
INTEGER                       :: ICOMMAND    ! Number of model command line to execute
INTEGER                       :: IL, IFAIL
CHARACTER(LEN=180)            :: INSTRUCTION
INTEGER                       :: ISTAT       ! Status: 0 for success
INTEGER                       :: NPT         ! Total number of parameters
INTEGER                       :: NPE         ! Number of parameters with adjustable=yes
DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: XOBS   ! Sensitivity of simulated equivalents to parameters
INTEGER                       :: ICTRL    ! Control-loop job pointer
LOGICAL                       :: CTRLDONE
LOGICAL                       :: SENSDONE
CHARACTER(LEN=JOBLEN), DIMENSION(JOBDIM) :: CTRLJOB  ! Sequence of jobs required of Control loop
TYPE (CDMATRIX)               :: WTM
INTEGER                       :: NCOVMAT  ! Number of group variance-covariance matrices
LOGICAL :: NEEDSENS = .TRUE.  ! True unless a forward-only run is specifically needed (other than for perturbation)
!
!   Prior information not used, but following variables needed for
!   STA_UEV_FIT anyway
INTEGER                        :: MPR = 0
DOUBLE PRECISION, DIMENSION(0) :: MODELPRIVAL, PRIVAL, RESIDSPRI, WTDRESIDSPRI
TYPE (CDMATRIX)                :: PRIWTMATSQR
LOGICAL, DIMENSION(0)          :: PRIWTCORR
CHARACTER(LEN=LENDNAM), DIMENSION(0) :: PRINAM
INTEGER, DIMENSION(0)          :: PRILN, PLOTSYMBOLPRI
!
!   Parallel processing
INTEGER :: ISNPRT
INTEGER :: KLOOP    ! Counter for iterations potentially parallel outer loop
INTEGER :: KLPTR
INTEGER :: KPI
INTEGER :: KPPL     ! Counter for iterations of potentially parallel inner loops
INTEGER :: KPTR
INTEGER :: LCIS           ! size of CINSTSET array
INTEGER :: MAXRUNSPLL = 1
INTEGER :: NINSTRUCT      ! size of LOCINS array
INTEGER :: NMIFILE        ! Number of model-input files
INTEGER :: NMOFILE        ! Number of model-output files
INTEGER :: NRUNSPLL = 1
INTEGER :: NUMLADV
INTEGER :: NUMLOOP  ! Number of iterations for potentiall parallel outer loop
INTEGER :: NUMPPL   ! Number of iterations for potentially parallel inner loops
LOGICAL :: DO_PARALLEL
LOGICAL :: PARALLEL_ACTIVE
CHARACTER (LEN=MAX_STRING_LEN) :: COMMANDPLL
CHARACTER(LEN=6),   ALLOCATABLE, DIMENSION(:) :: CATGOR    ! Model-calculated value category for each model-output file
CHARACTER(LEN=1),   ALLOCATABLE, DIMENSION(:) :: CINSTSET  ! holds compressed instruction set
CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: INSTRUCTFILE
INTEGER,            ALLOCATABLE, DIMENSION(:) :: KPE
INTEGER,            ALLOCATABLE, DIMENSION(:) :: LOCINS    ! pointer to instructions
CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: MIFILE    ! Names of model-input files
CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: MOFILE    ! Names of model-output files
CHARACTER(LEN=1),   ALLOCATABLE, DIMENSION(:) :: MRKDL
DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: PARVALSETS
DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: PARVALSETSE
DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: DEPVALSETS
CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: TFILE    ! Names of template files

INTEGER, ALLOCATABLE, DIMENSION(:) :: IPLOTOPT
INTEGER :: NPR
PARAMETER (NPR=4)
LOGICAL :: COPYDEPLISTS = .TRUE.
!
!   Variables added to accomodate Eileen's modules of 10/12/04
INTEGER :: NDOBS=0  ! Number of derived observations
INTEGER :: NEOBS, NTOBS
INTEGER, DIMENSION(2) :: IEQNPTR
INTEGER, ALLOCATABLE, DIMENSION(:) :: OMIT
DOUBLE PRECISION :: DTLA
LOGICAL :: DONOTEVALRUNS = .TRUE.
!
DATA CTRLJOB /'FORWARD','FORWARD&SENS','SENSITIVITY','STOP',6*' '/
!
!   Format statements
10  FORMAT(A)
50  FORMAT(20X,'Output from program ',A)
80  FORMAT(/,1X,'Reading input from file: ',A)
200 FORMAT(/,1X,'Error in instruction: "',A,'"')
250 FORMAT(1X,'Control-loop job = "',A,'" and KLPTR = ',I3)
300 FORMAT(/,1X,'For perturbed parameter "',A,'", model output follows:')
900 FORMAT(/,1X,'Normal termination of ',A)
!
! ******************************************************************************
! ****************************** INITIALIZE ************************************
! ******************************************************************************
!
CALL DATE_AND_TIME(CHDATE,CHTIME,CHZONE,IBDT)
IFAIL = 0
CALL TYP_NULL(WTM)
PARALLEL_ACTIVE = .FALSE.
!
!   Open an output file
FILEOUT = TRIM(PROGNAM)//'.out'
OPEN(UNIT=IOUT,FILE=FILEOUT,STATUS='REPLACE')
INQUIRE(IOUT,RECL=MAXRECL)
WRITE(IOUT,50)PROGNAM
WRITE(IOUT,10)HYPHENS(1:80)
!
FILEIN = UTL_GETARG()  ! Get command-line argument (may be commented-out)
IF (FILEIN==' ') THEN
  WRITE(*,10)' Enter name of input file:'
  READ(*,10)FILEIN
  IF (FILEIN==' ') CALL UTL_STOP('No input file entered')
ENDIF
!
!   Open the input data file
OPEN(UNIT=INUNIT,FILE=FILEIN,STATUS='OLD',ACTION='READ',IOSTAT=ISTAT)
IF (ISTAT .NE. 0) THEN
  WRITE(*,'(1X,A,I6)') 'File open failed--status = ',istat
  CALL UTL_STOP(' ')
ENDIF
WRITE(IOUT,80) TRIM(FILEIN)
!
!   Read options
CALL BAS_INI_GETOPTIONS(INUNIT,IOUT) ! (read OPTIONS block)
!
!   Read, store, and echo application-specific data (read CONTROL_DATA block)
CALL MAIN_INI(INUNIT,IOUT,NCOVMAT)
!
!   Initialize model-execution data (read MODEL_COMMAND_LINES block
CALL BAS_INI_MODELEXEC(INUNIT,IOUT)
!
!   Initialize parameter data (read PARAMETER_GROUPS, PARAMETER_DATA,
!   and PARAMETER_VALUES blocks)
CALL MAIN_INI_PARS(INUNIT,IOUT,NPT,NPE)
!
!   Read, store, and echo observation-related data (part 1; read
!   OBSERVATION_GROUPS, OBSERVATION_DATA, and DERIVED_OBSERVATIONS blocks)
CALL DEP_INI_READ(1,INUNIT,IOUT,NCOVMAT,NOBS,NDOBS,NEOBS,NTOBS,COPYDEPLISTS)
!
!   Allocate observation-related arrays
NDOBS=0
NEOBS=NOBS
CALL MAIN_INI_OBS()
!
!   Read variance-covariance matrices for groups of observations
!   (read MATRIX_FILES block)
IF (NCOVMAT>0) CALL BAS_INI_COVMAT(INUNIT,IOUT,NCOVMAT)
!
!   Read, store, and echo observation-related data (part 2), read variance-
!   covariance matrices for groups, and create weight matrix for all
!   observations.
CALL DEP_INI_ALLOC()
CALL DEP_INI_STORE(1,IOUT,NCOVMAT,NOBS,COVMATARR,IEQNPTR(1),OBSNAM,DTLA,   &
                   WTMAT,WTMATSQR)
!   Calculate weighted observations
CALL STA_INI(NOBS,OBSVAL,WTMATSQR)
!
!   Initialize Model_IO module
CALL MIO_INI_ALLOC(IFAIL,NPT)
!
!   Read, store, and echo file names for model input and output
CALL MIO_INI_INPUTFILES(INUNIT,IOUT)  ! (read MODEL_INPUT_FILES block)
CALL MIO_INI_OUTPUTFILES(INUNIT,IOUT) ! (read MODEL_OUTPUT_FILES block)
!
!   Do initialization related to model-input files
CALL MIO_INI_TEMPLATE(IFAIL,NPT,PARNAM,NW)
IF (IFAIL .NE. 0) CALL UTL_STOP(' ')
!
!   Initialize and populate instruction arrays
CALL MIO_INI_INSTRUCT1(IFAIL)
IF (IFAIL .NE. 0) CALL UTL_STOP(' ')
!
!   Allocate memory for storage of instructions
CALL MIO_INI_INSTRUCTALLOC(NOBS,IFAIL)
IF (IFAIL .NE. 0) CALL UTL_STOP(' ')
CALL MIO_INI_INSTRUCT2(IFAIL,NCATS,MCVCAT)
IF (IFAIL .NE. 0) CALL UTL_STOP(' ')
!
!   Allocate memory needed for calculating sensitivities and do other
!   initialization tasks
CALL SEN_INI(DERIV_INTERFACE,IOUT,NOBS,NPE,NPT,IPTR,ISENMETHOD,PARNAM,   &
             OBSNAM,IFAIL)
IF (IFAIL .NE. 0) CALL UTL_STOP(' ')
!
!   Initialize parallel-processing data
CALL MIO_INI_DIMENSION(LCIS,NINSTRUCT,NMIFILE,NMOFILE,NUMLADV)
ALLOCATE(CATGOR(NMOFILE),CINSTSET(LCIS),INSTRUCTFILE(NMOFILE),   &
         LOCINS(NINSTRUCT),MIFILE(NMIFILE),MOFILE(NMOFILE),   &
         MRKDL(NMOFILE),TFILE(NMIFILE))
CALL MIO_INI_ARRAYS(IFAIL,LCIS,NINSTRUCT,NMIFILE,NMOFILE,CATGOR,CINSTSET,   &
                    INSTRUCTFILE,LOCINS,MIFILE,MOFILE,MRKDL,TFILE)
IF (IFAIL.NE.0) CALL UTL_STOP('Programming error: MIO_INI_ARRAYS reports failure')
!
!   (read PARALLEL_CONTROL and PARALLEL_RUNNERS blocks)
CALL PLL_INI_DISPATCHER(INUNIT,IOUT,LCIS,NCATS,NINSTRUCT,NMIFILE,NMOFILE,   &
                        NOPNT,NPT,NUMLADV,NEOBS,PRECISPRO,CATGOR,CINSTSET,   &
                        OBSNAM,INSTRUCTFILE,LOCINS,MCVUSE,MIFILE,MOFILE,   &
                        MRKDL,NW,PARNAM,TFILE,PARALLEL_ACTIVE)
!
!   Allocate arrays declared in main program unit
MAXRUNSPLL = 2*NPE+1
ALLOCATE(PARVALSETS(NPT,MAXRUNSPLL),PARVALSETSE(NPE,MAXRUNSPLL),   &
         DEPVALSETS(NOBS,MAXRUNSPLL),KPE(MAXRUNSPLL))
ALLOCATE(XOBS(NPE,NOBS),OMIT(NOBS))
allocate(IPLOTOPT(NOBS))
IPLOTOPT=77
OMIT = 0
!
CTRLDONE = .FALSE.
SENSDONE = .TRUE.  ! Necessary initial condition
!
! ******************************************************************************
! ************************** TOP OF CONTROL LOOP *******************************
! ******************************************************************************
!
ICTRL = 0
CONTROL: DO WHILE (.NOT. CTRLDONE)
  !
  ! ****************************************************************************
  ! ************** DEFINE JOB OF CURRENT ITERATION OF CONTROL LOOP *************
  ! ****************************************************************************
  !
  CALL MAIN_DEF(JOBDIM,JOBLEN,NPE,CTRLJOB,IOUT,NEEDSENS,CTRLDONE,ICTRL,   &
                SENSDONE,KPPL,NUMPPL)
  IF (CTRLDONE) EXIT CONTROL
  IF (CTRLJOB(ICTRL)=='SENSITIVITY' .OR. CTRLJOB(ICTRL)=='FORWARD&SENS') THEN
    CALL SEN_DEF(CTRLJOB(ICTRL),IOUT,NOBS,NPE,NPT,IPTR,ISENMETHOD,PADJ,   &
                 PARNAM,SENSDONE,XOBS,ISNPRT,NUMPPL)
  ENDIF
  !
  IF (NUMPPL>0) THEN
    !
    ! **************************************************************************
    ! ************* GENERATE PARAMETER VALUES--Populate PARVALSETS *************
    ! **************************************************************************
    !
    DO KPPL=1,NUMPPL
      !   Populate one column of PARVALSETS with one set of values of
      !   adjustable parameters
      IF (CTRLJOB(ICTRL)=='FORWARD') THEN
        CALL BAS_GEN(NOPNT,NPT,NW,PRECISPRO,PVAL)
      ELSEIF (CTRLJOB(ICTRL)=='SENSITIVITY' .OR.   &
              CTRLJOB(ICTRL)=='FORWARD&SENS') THEN
        CALL SEN_GEN(CTRLJOB(ICTRL),KPPL,NPE,NPT,IOUT,BINC,IPTR,ISENMETHOD,   &
                     NOPNT,NW,PARNAM,PRECISPRO,PVAL,KPE(KPPL),   &
                     PARVALSETSE(:,KPPL:KPPL))
        !   Populate a full set of parameter values, including perturbed parameter
        PVALTMP = UTL_SUBSTITUTE(NPE,NPT,IPTR,PARVALSETSE(:,KPPL:KPPL),PVAL)
        DO IL=1,NPT
          PARVALSETS(IL,KPPL) = PVALTMP(IL)
        ENDDO
      ENDIF
    ENDDO
    !
    !   Establish iterative structure within potentially parallel loop
    IF (PARALLEL_ACTIVE .AND. NUMPPL>1) THEN
      DO_PARALLEL = .TRUE.
      !   Execute outer (PPL) loop once and inner loops multiple times
      NUMLOOP = 1
      NRUNSPLL = NUMPPL
    ELSE
      DO_PARALLEL = .FALSE.
      !   Execute outer loop multiple times and inner loops once
      NUMLOOP = NUMPPL
      NRUNSPLL = 1
    ENDIF
    !
    ! **************************************************************************
    ! ********************* TOP OF POTENTIALLY PARALLEL LOOP *******************
    ! **************************************************************************
    !
    PPL: DO KLOOP=1,NUMLOOP
      PPL1: DO KPPL=1,NRUNSPLL
        IF (DO_PARALLEL) THEN
          KLPTR = KPPL
        ELSE
          KLPTR = KLOOP
        ENDIF
        IF (IVERB>3) WRITE(IOUT,250)TRIM(CTRLJOB(ICTRL)),KLPTR
        IF (CTRLJOB(ICTRL)=='FORWARD' .OR. CTRLJOB(ICTRL)=='FORWARD&SENS') THEN
          PVALTMP = PVAL
        ELSEIF (CTRLJOB(ICTRL)=='SENSITIVITY' ) THEN
          !   Populate PVALTMP with current parameter values, with substitution
          !   of adjustable parameters as needed for current iteration.
          IF (.NOT. DO_PARALLEL) THEN
            PVALTMP = RESHAPE(PARVALSETS(:,KLPTR:KLPTR),SHAPE(PVALTMP))
          ENDIF
        ENDIF
      ENDDO PPL1
      !
      IF (.NOT. DO_PARALLEL) THEN
        !
        ! **********************************************************************
        ! ************************ ADAPT PARAMETER VALUES **********************
        ! **********************************************************************
        !
        CALL MIO_ADA_WRITEFILES(IFAIL,NPT,PARNAM,NOPNT,NW,PRECISPRO,PVALTMP)
        IF (IFAIL .NE. 0) CALL UTL_STOP(' ')
        !
        ! **********************************************************************
        ! ********************** EXECUTE APPLICATION MODEL *********************
        ! **********************************************************************
        !
        IF (CTRLJOB(ICTRL) .EQ. 'FORWARD' .OR.   &
            CTRLJOB(ICTRL)=='FORWARD&SENS') THEN
          CALL BAS_EXE_SELECT(IOUT,CTRLJOB(ICTRL),ICOMMAND)
        ELSEIF (CTRLJOB(ICTRL) .EQ. 'SENSITIVITY') THEN
          CALL SEN_EXE_SELECT(IOUT,ICOMMAND)
        ENDIF
        IF (ICOMMAND>0) THEN
          CALL BAS_EXE(ICOMMAND,-1,KLOOP,NUMPPL)
        ELSE
          !  If current job of control loop requires subroutine call instead
          !  of external process, call it here.
        ENDIF
        !
        ! **********************************************************************
        ! ******************* EXTRACT VALUES FROM MODEL OUTPUT *****************
        ! **********************************************************************
        !
        !   Extract simulated equivalents to observations
        CALL MIO_EXT(IFAIL,IOUT,NCATS,NOBS,OBSNAM,MCVUSE,   &
                     DEPVALSETS(:,KLOOP:KLOOP),INSTRUCTION)
        IF (IFAIL .NE. 0) THEN
          WRITE(IOUT,200) TRIM(AMESSAGE)
          IF (INSTRUCTION .NE. ' ') WRITE(IOUT,200) TRIM(INSTRUCTION)
          CALL UTL_STOP(' ')
        ENDIF
      ELSE
        ! **********************************************************************
        ! ********************* MAKE RUNS IN PARALLEL **************************
        ! **********************************************************************
        !
        !   Assign command to be run in parallel
        IF (CTRLJOB(ICTRL) .EQ. 'FORWARD' .OR.   &
            CTRLJOB(ICTRL)=='FORWARD&SENS') THEN
          CALL BAS_EXE_SELECT(IOUT,CTRLJOB(ICTRL),ICOMMAND,COMMANDPLL)
        ELSEIF (CTRLJOB(ICTRL) .EQ. 'SENSITIVITY') THEN
          CALL SEN_EXE_SELECT(IOUT,ICOMMAND,COMMANDPLL)
        ENDIF
        !
        !   Adapt, execute, and extract in parallel
        CALL PLL_MAKE_RUNS(LCIS,NCATS,NINSTRUCT,NMIFILE,NMOFILE,NOPNT,   &
                           NPT,NUMLADV,NRUNSPLL,NEOBS,PRECISPRO,   &
                           CATGOR,CINSTSET,OBSNAM,INSTRUCTFILE,   &
                           LOCINS,MCVUSE,MIFILE,MOFILE,MRKDL,NW,PARNAM,   &
                           PARVALSETS,TFILE,DEPVALSETS,COMMANDPLL)
      ENDIF
      !
      PPL2: DO KPPL=1,NRUNSPLL
        IF (DO_PARALLEL) THEN
          KLPTR = KPPL
        ELSE
          KLPTR = KLOOP
        ENDIF
        !
        IF (CTRLJOB(ICTRL) .EQ. 'FORWARD' .OR.   &
            CTRLJOB(ICTRL)=='FORWARD&SENS') THEN
          MODELVAL = RESHAPE(DEPVALSETS(:,KLPTR:KLPTR),(/NOBS/))
        ELSEIF (CTRLJOB(ICTRL) .EQ. 'SENSITIVITY') THEN
          !   Save simulated equivalents for model run with perturbed parameters
          IF (KPE(KLPTR)>0) THEN
            IF (IVERB .GT. 3 .AND. ISENMETHOD(IPTR(KPE(KLPTR)))>0) THEN
              WRITE(IOUT,300) TRIM(PARNAM(IPTR(KPE(KLPTR))))
              CALL DEP_UEV_RESIDUALS(NOBS,DEPVALSETS(:,KLPTR:KLPTR),WTMATSQR,  &
                                     RESIDS,WTDRESIDS)
              CALL DEP_UEV_WRITEOBSTABLE(IOUT,NOBS,DEPVALSETS(:,KLPTR:KLPTR),  &
                                         OBSNAM,RESIDS,WTDRESIDS)
            ENDIF
          ENDIF
        ENDIF
        !
        ! **********************************************************************
        ! ******************** END OF POTENTIALLY PARALLEL LOOP ****************
        ! **********************************************************************
        !
      ENDDO PPL2
    ENDDO PPL
  ENDIF
  !
  ! ****************************************************************************
  ! ************************* USE EXTRACTED VALUES *****************************
  ! ****************************************************************************
  !
  IF (CTRLJOB(ICTRL) .EQ. 'FORWARD' .OR. CTRLJOB(ICTRL)=='FORWARD&SENS') THEN
    !   Calculate residuals and weighted residuals for observations
    CALL DEP_UEV_RESIDUALS(NOBS,MODELVAL,WTMATSQR,RESIDS,WTDRESIDS)
    !   Write output: Residuals and related statistics
    CALL STA_UEV_INIT(AVET,NNEGT,NPOST,NRUNS,RSQ,RSQP)
    CALL STA_UEV_FIT(IOUT,MPR,NOBS,IPRINT,DONOTEVALRUNS,MODELVAL,   &
                     MODELPRIVAL,OBSNAM,OBSVAL,OMIT,OUTNAME,   &
                     PLOTSYMBOLPRI,PRILN,PRINAM,PRIVAL,PRIWTCORR,   &
                     PRIWTMATSQR,RESIDS,RESIDSPRI,WTCORRELATED,   &
                     WTDRESIDS,WTDRESIDSPRI,WTMATSQR,AVET,NNEGT,NPOST,   &
                     NRUNS,RSQ,RSQP,DNPP,NDINC,WTRL)
  ENDIF
  !
  IF (CTRLJOB(ICTRL) .EQ. 'SENSITIVITY' .OR.   &
      CTRLJOB(ICTRL)=='FORWARD&SENS') THEN
    !   Populate the XOBS sensitivity matrix
    IF (ISNPRT==0) THEN
      !   Sensitivities are model-calculated
      CALL SEN_UEV_POPX_MODCALC(IOUT,NOBS,NPE,NPT,IPTR,ISENMETHOD,SENSDONE,XOBS)
    ELSE
      !   Calculate sensitivities by a finite-difference method
      KPTR = 1
      IF (CTRLJOB(ICTRL)=='FORWARD&SENS') KPTR = 2
      DO KPI=1,NPE
        IF (ISENMETHOD(IPTR(KPI))==1) THEN
          CALL SEN_UEV_POPXROW_DIFF(KPI,NOBS,DEPVALSETS(:,KPTR:KPTR),   &
                                    MODELVAL,XOBS(KPI:KPI,:))
          KPTR = KPTR+1
        ELSEIF (ISENMETHOD(IPTR(KPI))==2) THEN
          CALL SEN_UEV_POPXROW_DIFF(KPI,NOBS,DEPVALSETS(:,KPTR:KPTR),   &
                                    DEPVALSETS(:,KPTR+1:KPTR+1),XOBS(KPI:KPI,:))
          KPTR = KPTR+2
        ENDIF
      ENDDO
      SENSDONE=.TRUE.
    ENDIF
    !   Write output: Sensitivities
    CALL SEN_UEV_WRITESENTABLE(NOBS,NPE,NPT,OBSNAM,'UNSCALED SENSITIVITIES:', &
                               IOUT,IPTR,7,PARNAM,XOBS)
    IF (SENSDONE) THEN
      !   Write unscaled sensitivities to _su file
      ISU = UTL_DX_OPEN(OUTNAME,'_su','REPLACE')
      CALL SEN_UEV_DX_WRITE_MATRIX(ISU,NOBS,NPE,NPT,OBSNAM,IPTR,PARNAM,XOBS)
      ISU = UTL_DX_CLOSE('_su')
      !   Convert native-space sensitivities to log-space
      !   where LN>0 (XOBS array)
      CALL SEN_UEV_LNX(NOBS,NPE,NPT,IPTR,LN,PVAL,XOBS)
    ENDIF
    CALL MAIN_UEV_BSCALMESS(NPE,NPT,PVAL,BSCAL,IOUT)
  ENDIF
!
! ******************************************************************************
! ************************** END OF CONTROL LOOP *******************************
! ******************************************************************************
!
ENDDO CONTROL
!
! ******************************************************************************
! ******************************* CLEAN UP *************************************
! ******************************************************************************
!
IF (PARALLEL_ACTIVE) CALL PLL_STOP_RUNNERS()
CALL BAS_CLN()
CALL DEP_CLN()
CALL SEN_CLN()
CALL STA_CLN()
CALL PLL_CLN()
CALL MAIN_CLN()
!
WRITE(IOUT,900) PROGNAM
WRITE(*,900) PROGNAM
CALL UTL_ENDTIME(IBDT,IOUT)
CALL UTL_STOP(' ')
END PROGRAM SENSITIVITY_EXAMPLE
