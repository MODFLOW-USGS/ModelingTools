!**********************************************************
!*** This module was programmed by Dan Lu on 2013/01/20 ***
!**********************************************************
!
MODULE DREAM
!
  USE DATATYPES
  USE GLOBAL_DATA
  USE UTILITIES
!
  USE BASIC, ONLY: BAS_EXE, MODCOMLINE
  USE MODEL_IO, ONLY: MIO_ADA_WRITEFILES, MIO_EXT
  USE DEPENDENTS, ONLY: DEP_EXT_DER, DEP_UEV_RESIDUALS
!
  USE UCODEMOD, ONLY: MCMC_PREDICTION
!
  USE PDFLIB
!
  USE PARALLEL_PROCESSING, ONLY: PLL_MAKE_RUNS
!
!
  IMPLICIT NONE
!
! -- Private variables
!
  PRIVATE  &
    PAR_DIM, PARGPNAM, PARGP, PARNAM, MATRIXNAM, FILE_PREFIX, OUTUNIT,      &
    RANGEMIN, RANGEMAX, PDF_TYPE, PDF_GPTYPE, PDF_VAR1, PDF_VAR2,           &
    NCHAIN, MAX_GEN, NPAIR, JUMPSTEP, REG_FLAG, RESTART_FLAG,               &
    NCR, CR, CR_IND, PCR, L_CR, DIS_CR,                                     &
    GEL_RU_R, PRINTSTEP, GR_COUNT, CONV_FLAG, GR_THRESHOLD,                 &
    JUMPRATE_TABLE, JUMPRATE, JUMP_NUM, JUMP_DIM,                           &
    Z, FIT, ZP, AR,                                                         &
    MNOR_DIM, UNI_DIM, MNOR_IND, UNI_IND, MNOR_MEAN, COV_MAT, PARM, DET
!
!
! -- Private subroutines
!
  PRIVATE LIKELIHOOD,       &
          GET_PRIOR_SAMPLE, &
          PRIOR_DENSITY,    &
          GEN_CANDIDATE,    &
          OUTOFBOUND,       &
          INIT_CR,          &
          CHOOSE_CR,        &
          UPDATE_CR_DIS,    &
          UPDATE_PCR,       &
          COMP_STD,         &
          CHOOSE_JUMPRATE,  &
          COMP_DIFF,        &
          GEL_RU,           &
          OUTPUT
!
!
!*****************************************************
!           PARAMETER DEFINITION
!*****************************************************
!
!*** Variables read from MCMC_Prior input block
!  Number of parameters
  INTEGER :: PAR_DIM
  INTEGER :: MC_NPS
!
!  Name of parameters and parameter groups
  CHARACTER(12), ALLOCATABLE :: PARGPNAM(:)
  CHARACTER(12), ALLOCATABLE :: PARNAM(:)
  CHARACTER(12), ALLOCATABLE :: MC_PARNAMLC(:)
  CHARACTER(12), ALLOCATABLE :: PARGP(:)
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE :: MATRIXNAM(:)
!
!  Range of each parameter can be read from MCLowerConstraint and MCUpperConstraint in
!  MCMC_PRIOR input block
  DOUBLE PRECISION, ALLOCATABLE :: RANGEMIN(:)
  DOUBLE PRECISION, ALLOCATABLE :: RANGEMAX(:)
!
!  Define the prior density type and its variables
  CHARACTER(100), ALLOCATABLE   :: PDF_TYPE(:)
  CHARACTER(100), ALLOCATABLE   :: PDF_GPTYPE(:)
  DOUBLE PRECISION, ALLOCATABLE :: PDF_VAR1(:)
  DOUBLE PRECISION, ALLOCATABLE :: PDF_VAR2(:)
!
!*** Prefix of output file name, it is the same as prefix of UCODE exchange file
  CHARACTER(LEN=MAX_STRING_LEN) :: file_prefix
!
!*** Unit of main output file name, #umcmc
  INTEGER :: OUTUNIT
!
!*** Variables read from MCMC_Controls input block
!  Number of Markov chains
  INTEGER :: NCHAIN
!
!  Maximum generations for a single chain
  INTEGER :: MAX_GEN
!
!  Number of pairs of chains to generate candidate samples
  INTEGER :: NPAIR
!
!  Number of steps to reset jump rate variable, Gamma
  INTEGER :: JUMPSTEP
!
!  Use multinormal prior with statistics from regression to start MCMC
  LOGICAL :: REG_FLAG
!
!  Restart the chain from last generation of last time simulation
  LOGICAL :: RESTART_FLAG
!
!  Print model output into a file
  LOGICAL :: MODEL_OUTPUT
!
!  If covergence is yes in restart file, then not call outlier, Gel_R, update CR
!  not write grr file
  CHARACTER(1) :: CONV_RESTART
!
!  Make prediciton using parameter samples from iteration ItStartPred
  INTEGER :: ITSTARTPRED
!
!
!*** Variables called from UCODE main program used to calculate SSWR
  INTEGER :: MC_NCATS, MC_NEOBS, MC_NOBS, MC_NDOBS, MC_IEQNPTR(2)
  INTEGER, ALLOCATABLE :: MC_NW(:), MC_OBSDEROBSNUM(:), MC_OBSEXTOBSNUM(:)
  CHARACTER(LEN=LENDNAM), ALLOCATABLE :: MC_OBSEXTNAM(:), MC_OBSNAM(:)
  LOGICAL, ALLOCATABLE :: MC_MCVUSE(:)
  LOGICAL :: MC_PARALLEL_ACTIVE
  TYPE (CDMATRIX) :: MC_WTFULLSQR
  DOUBLE PRECISION, ALLOCATABLE :: MC_MODEXTVAL(:),MC_MODDERVAL(:), &
                 MC_DEPDERVALSETS(:,:), MC_DEPEXTVALSETS(:,:), MC_MODELVAL(:)
!
!*** Variables to print model outputs
  INTEGER :: INDSIM 
  DOUBLE PRECISION, ALLOCATABLE :: PRINT_SIM(:,:,:), FIT_SIM(:,:)
!
!*** Variables called from UCODE main program used to calculate SSWR in Parallel
  INTEGER :: MC_LCIS,MC_NINSTRUCT,MC_NMIFILE,MC_NMOFILE,MC_NUMLADV
  CHARACTER(LEN=6),ALLOCATABLE, DIMENSION(:) :: MC_CATGOR
  CHARACTER(LEN=1),ALLOCATABLE, DIMENSION(:) :: MC_CINSTSET, MC_MRKDL
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: MC_INSTRUCTFILE
  INTEGER,ALLOCATABLE, DIMENSION(:) :: MC_LOCINS
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: MC_MIFILE
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: MC_MOFILE
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: MC_TFILE
!
!
!*** Other variables used in this module
!  Crossover parameters
  INTEGER           :: NCR
  DOUBLE PRECISION  :: CR(100)
  INTEGER           :: CR_IND
  DOUBLE PRECISION  :: PCR(100)
  INTEGER           :: L_CR(100)
  DOUBLE PRECISION  :: DIS_CR(100)
!
!  Gelman Rubin R statistic used to check MC convergence
  DOUBLE PRECISION, ALLOCATABLE :: Gel_Ru_R(:,:)
  INTEGER                       :: PRINTSTEP
  INTEGER                       :: GR_COUNT
  LOGICAL                       :: CONV_FLAG
  DOUBLE PRECISION              :: GR_THRESHOLD
!
!  Jump rate table
  DOUBLE PRECISION, ALLOCATABLE :: JUMPRATE_TABLE(:)
  DOUBLE PRECISION              :: JUMPRATE
  INTEGER                       :: JUMP_NUM
  INTEGER, ALLOCATABLE          :: JUMP_DIM(:)
!
!  Markov chain
  DOUBLE PRECISION, ALLOCATABLE :: Z(:,:,:)
  DOUBLE PRECISION, ALLOCATABLE :: FIT(:,:)
  DOUBLE PRECISION, ALLOCATABLE :: ZP(:)
!
!  Acceptance rate
  DOUBLE PRECISION :: AR
!
!  Variables for multinormal density
  INTEGER                       :: MNOR_DIM, UNI_DIM, MATRIXINX
  INTEGER                       :: MNOR_IND(1000), UNI_IND(1000)
  DOUBLE PRECISION, ALLOCATABLE :: MNOR_MEAN(:)          ! mean vector
  DOUBLE PRECISION, ALLOCATABLE :: COV_MAT(:,:)          ! covariance matrix
  DOUBLE PRECISION, ALLOCATABLE :: PARM(:,:)             ! used for calculate inverse covariance matrix of multi-normal distribution
  DOUBLE PRECISION              :: DET                   ! determinant of covariance matrix
  LOGICAL                       :: MULTINORMAL_FLAG = .FALSE.   ! if true, in ucode_2005.f90 call MCMC_PRIOR_STORE
!
!
!*** BLOCK PARAMETERS ***
!     For MCMC_PRIOR_PDF input, define default column order
  INTEGER,           PARAMETER           :: NPRICOLS = 7
  CHARACTER(40), DIMENSION(NPRICOLS)     :: PRICOL = &
      (/'PARAMNAME        ','GROUPNAME        ','MCLOWERCONSTRAINT', &
        'MCUPPERCONSTRAINT','PDFTYPE          ','PDFVAR1        ', &
        'PDFVAR2          '/)
  CHARACTER(40), DIMENSION(0) :: NOCOL
!
!
CONTAINS
!
!
!*******************************************************************************
!
!                       FUNCTIONS FOR INITIALIZATING CHAINS
!
!*******************************************************************************
!
!-------------------------------------------------------------------------------
!***  initialize parameters for MCMC simulation from MCMC_Controls input block
!
!
  SUBROUTINE MCMC_INI (IFAIL,INUNIT,IOUT,OUTNAM)
!
    IMPLICIT NONE
!
  ! Argument-list variables
    INTEGER,                                  INTENT(INOUT)  :: IFAIL
    INTEGER,                                  INTENT(IN)     :: INUNIT
    INTEGER,                                  INTENT(IN)     :: IOUT
    CHARACTER(LEN=MAX_STRING_LEN),            INTENT(IN)     :: OUTNAM
!
   ! Local variables
    TYPE (LLIST), POINTER :: UCODEHEAD  ! Ptr to head of list (UCODE cntrls)
    TYPE (LLIST), POINTER :: TAIL       ! Ptr to tail of a list
    CHARACTER(LEN=40), DIMENSION(1) :: UCODECOL = (/ '      '/)
    INTEGER           :: NUCODE
!
   ! Declare variables (and default values) to allow use of UTL_FILTER
    INTEGER           :: IERR = 0
!
   ! Format statements
    100 FORMAT(1X,A)
    101 FORMAT(//,79('-'))
    102 FORMAT(20X,'UCODE MODE FOR MARKOV CHAIN MONTE CARLO ANALYSIS:',/)
    150 FORMAT(1X,'ECHO UCODE MCMC_CONTROLS INPUT:',/)
    105 FORMAT(1X,A,I11)
    106 FORMAT(1X,A,F11.2)
    107 FORMAT(1X,A,L11)
!
   ! Write information (UCODE controls) to output file
    WRITE(IOUT,101)
    WRITE(IOUT,102)
!
   ! Initialize
    ERRSUB=' Error in subroutine MCMC_INI'
    IFAIL = 0
    FILE_PREFIX = OUTNAM
    NULLIFY(UCODEHEAD)
    NULLIFY(TAIL)
    NUCODE = 0
!
   ! Read input block, First Set defaults
    IF (.NOT. MCMC_PREDICTION) THEN
      NCHAIN =  5
      MAX_GEN = 10000
    ELSE
      NCHAIN =  -99999
      MAX_GEN = -99999
    ENDIF
!
    NPAIR = 3
    NCR = 3
    JUMPSTEP = 5
    REG_FLAG = .FALSE.
    MODEL_OUTPUT = .FALSE.
    GR_THRESHOLD = 1.2
    PRINTSTEP = 10
    RESTART_FLAG = .FALSE.
!
    CALL UTL_READBLOCK(0,'MCMC_CONTROLS',UCODECOL,INUNIT,IOUT,  &
                       '*',.FALSE.,UCODEHEAD,TAIL,NUCODE)
    IF (NUCODE>0) THEN
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'NCHAIN',nchain)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'MAXSAMPLES',max_gen)
      ITSTARTPRED = 0.5*MAX_GEN
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'NPAIR',npair)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'NCR',nCR)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'JUMPSTEP',jumpstep)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'USEREGRESULT',reg_flag)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'PRINTMODELOUTPUT',model_output)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'GELMANR',GR_threshold)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'PRINTSTEP',printstep)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'RESTART',restart_flag)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'ITSTARTPRED',ItStartPred)
!
      IF (NCHAIN < 3 .AND. (.NOT. MCMC_PREDICTION)) THEN
        AMESSAGE = 'TERMINATING: Nchain must be larger than 3!'  // &
                  ' SUGGESTION: Nchain set as number of parameters!'
        CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
        CLOSE(IOUT)
        CALL UTL_STOP()
      ENDIF
!
      IF (NCHAIN ==  -99999 .OR. MAX_GEN == -99999) THEN
        AMESSAGE =  &
        'TERMINATING: For MCMC_PREDICTION run, keywords Nchain and'  // &
        ' MaxSamples in MCMC_CONTROLS block must be given!'  // &
        ' Nchain and MaxSamples are better consistent with and'  // &
        ' not greater than the values set in MCMC run!'
        CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
        CLOSE(IOUT)
        CALL UTL_STOP()
      ENDIF
!
    ELSE
      IF (.NOT. MCMC_PREDICTION) THEN
        WRITE(IOUT,*)
        WRITE(IOUT,100) &
                     '********************************************************'
        WRITE(IOUT,100)'        NOTE MCMC_CONTROLS BLOCK NOT FOUND          '
        WRITE(IOUT,100)'                DEFAULTS ARE USED '
        WRITE(IOUT,100)'        IF YOU INTENDED TO READ THIS BLOCK'
        WRITE(IOUT,100)'       CHECK THE SPELLING OF THE BLOCK NAME'
        WRITE(IOUT,100)'  JUPITER CODES IGNORE BLOCKS WITH MISSPELLED NAMES'
        WRITE(IOUT,100) &
                     '*********************************************************'
        WRITE(IOUT,*)
      ELSE
        AMESSAGE =  &
        'TERMINATING: For MCMC_PREDICTION run, keywords Nchain and'  // &
        ' MaxSamples in MCMC_CONTROLS block must be given!'  // &
        ' Nchain and MaxSamples are better consistent with and not'  // &
        ' greater than the values set in MCMC run!'
        CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
        CLOSE(IOUT)
        CALL UTL_STOP()
      ENDIF
    ENDIF
!
!
  ! Write information (UCODE controls) to output file
    WRITE(IOUT,150)
    WRITE(IOUT,105) &
    'NUMBER OF MARKOV CHAINS                                         =',NCHAIN
    WRITE(IOUT,105) &
    'MAXIMUM NUMBER OF PARAMETER SAMPLES FOR EACH CHAIN              =',MAX_GEN
    WRITE(IOUT,105) &
    'NUMBER OF ITERATIONS TO PRINT RESULTS                           =',PRINTSTEP
!
    IF (.NOT. MCMC_PREDICTION) THEN
      WRITE(IOUT,105) &
      'NUMBER OF PAIRS OF CHAINS                                       =' &
       ,NPAIR
      WRITE(IOUT,105) &
      'NUMBER OF CR VALUES                                             =' &
       ,NCR
      WRITE(IOUT,105) &
      'NUMBER OF ITERATIONS TO RESET JUMP RATE                         =' &
       ,JUMPSTEP
      WRITE(IOUT,107) &
      'USE REGRESSION RESULT TO START CHAINS                           =' &
       ,REG_FLAG
      WRITE(IOUT,107) &
      'PRINT MODEL OUTPUT IN A FILE                                    =' &
       ,MODEL_OUTPUT
      WRITE(IOUT,106) &
      'GELMAN-RUBIN R CRITERIA FOR CONVERGENCE                         =' &
       ,GR_THRESHOLD      
      WRITE(IOUT,107) &
      'RESTART MCMC                                                    =' &
       ,RESTART_FLAG
      WRITE(IOUT,101)
    ELSE
      WRITE(IOUT,105) &
      'ITERATION # AFTER WHICH PARAMETER SAMPLES ARE USED FOR PREDICTION'  // &
      '         =',ITSTARTPRED
      WRITE(IOUT,101)
    ENDIF
!
  END SUBROUTINE MCMC_INI
!
!
!-------------------------------------------------------------------------------
!***  Read prior distribution related variables from MCMC_Prior input block
!
  SUBROUTINE MCMC_INI_PRIOR (IFAIL,INUNIT,IOUT,NCOVMAT)
!
    IMPLICIT NONE
!
  ! Argument-list variables
    INTEGER, INTENT(INOUT) :: IFAIL
    INTEGER, INTENT(IN)    :: INUNIT
    INTEGER, INTENT(IN)    :: IOUT
    INTEGER, INTENT(INOUT) :: NCOVMAT  ! number of covariance matrix for multinormal prior distribution
!
  ! Local variables
    TYPE (LLIST), POINTER :: PTGPMCPRIOR ! Pointer to head of first list(prior_pdf. groups)
    TYPE (LLIST), POINTER :: PTMCPRIOR   ! Pointer to head of first list(prior_pdf. info)
    TYPE (LLIST), POINTER :: TAIL
    INTEGER               :: I, IERR, MORE, NPRIGPS, MCPR
    LOGICAL               :: TERMINATE = .FALSE.
!
!
  ! Format statements
    100 FORMAT(/,1X,A)
    102 FORMAT(/,1X,A,1X,A)
!
  ! Initialize
    OUTUNIT = IOUT
    ERRSUB=' Error in subroutine MCMC_INI_PRIOR'
    IFAIL = 0
    NULLIFY(PTGPMCPRIOR,PTMCPRIOR,TAIL)
    NPRIGPS = 0
    MCPR = 0
    IERR = 0
    MNOR_DIM = 0    ! number of parameters have multinormal distribution
    UNI_DIM = 0     ! number of parameters have univariate distribution
!
!
  ! Read MCMC_PRIOR_GROUPS input block
    CALL UTL_READBLOCK(0,'MCMC_PRIOR_GROUPS',NOCOL,INUNIT,IOUT,   &
        'GROUPNAME',.FALSE.,PTGPMCPRIOR,TAIL,NPRIGPS)
!
    IF (IVERB>4 .AND.NPRIGPS>0 .AND. (.NOT. MCMC_PREDICTION)) THEN
  ! Write block information (prior groups) to output file
      WRITE(IOUT,'(A)')HYPHENS(1:80)
      WRITE(IOUT,100)'Echo MCMC_Prior_Groups input:'
      CALL UTL_WRITEBLOCK(PTGPMCPRIOR,IOUT)
    ENDIF
!
    IF (NPRIGPS==0) NPRIGPS = 1
!
    ALLOCATE(PARGPNAM(NPRIGPS),PDF_GPTYPE(NPRIGPS),MATRIXNAM(NPRIGPS))
!
  ! Assign defaults for groups
    PARGPNAM = 'DefaultPrior'
    MNOR_DIM = 0
    PDF_GPTYPE = 'uniform'
    MATRIXNAM = ' '
!
    CALL UTL_FILTERLIST(PTGPMCPRIOR,IOUT,'GROUPNAME',NPRIGPS,   &
                        IERR,PARGPNAM,MORE)
    CALL UTL_FILTERLIST(PTGPMCPRIOR,IOUT,'PDFTYPE',NPRIGPS,   &
                        IERR,PDF_GPTYPE,MORE)
    CALL UTL_FILTERLIST(PTGPMCPRIOR,IOUT,'COVMATRIX',NPRIGPS,   &
                        IERR,MATRIXNAM,MORE)
!
!
  ! Count groups for which a variance-covariance matrix is required
    DO I=1,NPRIGPS
      IF (MATRIXNAM(I) .NE. ' ') THEN
        NCOVMAT = NCOVMAT + 1
        MATRIXINX = I
      ENDIF
    ENDDO
    
    IF (NCOVMAT .GT. 1) THEN
      CALL UTL_STOP &
      ('TERMINATING: IN MCMC_PRIOR_GROUPS INPUT BLOCK, THE NUMBER OF &
        COVARIANCE MATRIX FOR MULTINORMAL DISTRIBUTION CAN ONLY BE ONE!')
    ENDIF
!
!
  ! Read MCMC_Prior_PDF input block
    CALL UTL_READBLOCK(NPRICOLS,'MCMC_PRIOR_PDF',PRICOL,   &
                       INUNIT,IOUT,'PARAMNAME',.FALSE.,   &
                       PTMCPRIOR,TAIL,MCPR)
!
    IF (MCPR == 0) THEN
      IF (.NOT. MCMC_PREDICTION) THEN
        WRITE(IOUT,*)
        WRITE(IOUT,100) &
                   '*********************************************************'
        WRITE(IOUT,100)'        NOTE MCMC_PRIOR_PDF BLOCK NOT FOUND          '
        WRITE(IOUT,100)'                IT IS REQUIRED '
        WRITE(IOUT,100)'        IF YOU CONSTRUCTED THIS BLOCK'
        WRITE(IOUT,100)'       CHECK THE SPELLING OF THE BLOCK NAME'
        WRITE(IOUT,100)'  JUPITER CODES IGNORE BLOCKS WITH MISSPELLED NAMES'
        WRITE(IOUT,100) &
                   '*********************************************************'
        WRITE(IOUT,*)
        WRITE(IOUT,100) &
        'TERMINATING: MCMC_PRIOR_PDF BLOCK IS REQUIRED FOR MCMC RUN!'
        CALL UTL_STOP &
        ('TERMINATING: MCMC_PRIOR_PDF BLOCK IS REQUIRED FOR MCMC RUN!')
      ELSE
        WRITE(IOUT,*)
        AMESSAGE = 'TERMINATING: KEYWORD PARAMNAME MUST BE GIVEN IN'  // &
        ' MCMC_PRIOR_PDF BLOCK FOR MCMC_PREDICTION RUN!'
        CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
        CLOSE(IOUT)
        CALL UTL_STOP()
      ENDIF
!
    ELSEIF (IVERB>4 .AND. MCPR>0 .AND. (.NOT. MCMC_PREDICTION)) THEN
  ! Write block information (mcmc_prior_pdf) to output file
      WRITE(IOUT,100)'Echo MCMC_Prior_PDF before inserting group information:'
      CALL UTL_WRITEBLOCK(PTMCPRIOR,IOUT)
    ENDIF
!
!
  ! Insert Prior_Groups information into Prior_PDF lists
    IF (NPRIGPS>0) THEN
      IF (MCPR>0) THEN
        CALL UTL_GROUPLIST('DefaultPrior',PTGPMCPRIOR,IOUT,PTMCPRIOR,   &
                           NPRIGPS,MCPR)
      ENDIF
    ENDIF
!
    IF (MCPR>0 .AND. (.NOT. MCMC_PREDICTION)) THEN
  ! Write block information (mcmc_prior_pdf) to output file
      WRITE(IOUT,*)
      WRITE(IOUT,100) &
      'Echo MCMC_Prior_PDF input after inserting group information:'
      CALL UTL_WRITEBLOCK(PTMCPRIOR,IOUT)
!  ! Write block information (mcmc_prior groups) to output file
!       WRITE(IOUT,'(A)')HYPHENS(1:80)
!       WRITE(IOUT,100)'Echo MCMC_Prior_Groups input after UTLGROUPLIST:'
!       CALL UTL_WRITEBLOCK(PTGPMCPRIOR,IOUT)
    ENDIF
!
  ! Allocate arrays for prior-related variables
    MC_NPS = MCPR     !number of parameters in template file
    PAR_DIM = MCPR

    ALLOCATE(PARNAM(PAR_DIM), MC_PARNAMLC(PAR_DIM), PARGP(PAR_DIM), &
             RANGEMIN(PAR_DIM), RANGEMAX(PAR_DIM), &
             PDF_TYPE(PAR_DIM), PDF_VAR1(PAR_DIM), PDF_VAR2(PAR_DIM))
!
  ! Read input block, First Set defaults
    PDF_TYPE = 'uniform'
    PARGP = 'DefaultPrior'
    RANGEMIN = -BIGREAL
    RANGEMAX = BIGREAL
    PDF_VAR1 = -BIGREAL
    PDF_VAR2 = BIGREAL
!
    CALL UTL_FILTERLIST(PTMCPRIOR,IOUT,'PARAMNAME',MCPR,IERR,PARNAM,MORE)
    CALL UTL_FILTERLIST(PTMCPRIOR,IOUT,'GROUPNAME',MCPR,IERR,PARGP,MORE)
    CALL UTL_FILTERLIST &
             (PTMCPRIOR,IOUT,'MCLOWERCONSTRAINT',MCPR,IERR,RANGEMIN,MORE)
    CALL UTL_FILTERLIST &
             (PTMCPRIOR,IOUT,'MCUPPERCONSTRAINT',MCPR,IERR,RANGEMAX,MORE)
    CALL UTL_FILTERLIST(PTMCPRIOR,IOUT,'PDFTYPE',MCPR,IERR,PDF_TYPE,MORE)
    CALL UTL_FILTERLIST(PTMCPRIOR,IOUT,'PDFVAR1',MCPR,IERR,PDF_VAR1,MORE)
    CALL UTL_FILTERLIST(PTMCPRIOR,IOUT,'PDFVAR2',MCPR,IERR,PDF_VAR2,MORE)
!
    DO I = 1, PAR_DIM
!
      CALL UTL_CASE(PARNAM(I),MC_PARNAMLC(I),-1) ! Store names as lowercase used in template file
!
      IF (.NOT. MCMC_PREDICTION) THEN
!
        IF (RANGEMIN(I)==-BIGREAL .OR. RANGEMAX(I)==BIGREAL)THEN
          AMESSAGE = 'TERMINATING: MCLowerConstraint and MCUpperConstraint' // &
          ' of parameters in MCMC_Prior_PDF block are not given.' // &
          ' Use default values may cause physical meaninglessness and' // &
          ' low MCMC convergence!  Please give reasonable parameter' // &
          ' bounds for all parameters!'
          CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
          CLOSE(IOUT)
          CALL UTL_STOP()
        ENDIF
!
        IF (RANGEMIN(I) >= RANGEMAX(I))THEN
          AMESSAGE=' MC LOWER CONSTRAINT > MC UPPER CONSTRAINT FOR PARAMETER: '
          WRITE(*,102)TRIM(AMESSAGE),parnam(i)
          WRITE(IOUT,102)TRIM(AMESSAGE),parnam(i)
          TERMINATE = .TRUE.
        ENDIF
        IF(I == PAR_DIM .AND. TERMINATE) THEN
          AMESSAGE= 'TERMINATING: PLEASE CORRECT RELATIVE VALUES OF' // & 
          ' MC CONSTRAINTS TO MAKE SURE MC UPPER CONSTRAINT IS GREATER' // &
          ' MC LOWER CONSTRAINT!'
          CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
          CLOSE(IOUT)
          CALL UTL_STOP()
        ENDIF
!
        CALL UTL_CASETRANS(PDF_TYPE(I),'lo')
        IF ( TRIM(PDF_TYPE(I)) == 'uniform')THEN
          PDF_VAR1(I) = RANGEMIN(I)
          PDF_VAR2(I) = RANGEMAX(I)
!
        ELSEIF (TRIM(PDF_TYPE(I))=='normal' .OR. TRIM(PDF_TYPE(I))=='beta' &
                .OR. TRIM(PDF_TYPE(I))=='scaled-inv-chi-square' .OR.  &
                TRIM(PDF_TYPE(I))=='log-normal' .OR. &
                TRIM(PDF_TYPE(I))=='gamma' .OR. &
                TRIM(PDF_TYPE(I))=='inv-gamma') THEN
          IF (PDF_VAR1(I)==-BIGREAL .OR. PDF_VAR2(I)==BIGREAL)THEN
            AMESSAGE = 'TERMINATING: Prior variables PDFVar1 or PDFVar2' // &
            ' in MCMC_Prior_PDF block are not given. Use default values' // &
            ' may cause physical meaninglessness and low MCMC' // &
            ' convergence!  Please give reasonable parameter bounds' // &
            ' for all parameters!'
            CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
            CLOSE(IOUT)
            CALL UTL_STOP()
          ENDIF
!
        ELSEIF (TRIM(PDF_TYPE(I))=='chi-square' .OR. &
                TRIM(PDF_TYPE(I))=='inv-chi-square' .OR. &
                TRIM(PDF_TYPE(I))=='exponential' .OR. &
                TRIM(PDF_TYPE(I))=='multinormal') THEN
          IF (PDF_VAR1(I)==-BIGREAL)THEN
            AMESSAGE = 'TERMINATING: Prior variables PDFVar1 in' // &
            ' MCMC_Prior_PDF block are not given. Use default values' // &
            ' may cause physical meaninglessness and low MCMC convergence!' // &
            ' Please give reasonable parameter bounds for all parameters!'
            CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
            CLOSE(IOUT)
            CALL UTL_STOP()
          ENDIF
!
        ELSE
          AMESSAGE = 'TERMINATING: Please provide prior PDF from available' // &
          ' 11 choices! Or check the spelling of the given prior PDF!'
          CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
          CLOSE(IOUT)
          CALL UTL_STOP()
        ENDIF
!
        IF (TRIM(PDF_TYPE(I)) == 'multinormal')THEN
          MNOR_DIM = MNOR_DIM + 1
          MNOR_IND(MNOR_DIM) = I
          MULTINORMAL_FLAG = .TRUE.
        ELSE
          UNI_DIM = UNI_DIM + 1
          UNI_IND(UNI_DIM) = I
        ENDIF
!
      ENDIF ! NOT MCMC_PREDICTION
!
    ENDDO  ! do i = 1 to par_dim
!
  END SUBROUTINE MCMC_INI_PRIOR
!
!-------------------------------------------------------------------------------
!***  Read prior distribution related variables from MCMC_Prior input block

  SUBROUTINE MCMC_PRIOR_STORE(IOUT, NCOVMAT, COVMATARR)
!
    IMPLICIT NONE
!
  ! Argument-list variables
    INTEGER,                             INTENT(IN) :: IOUT
    INTEGER,                             INTENT(IN) :: NCOVMAT
    TYPE (CDMATRIX), DIMENSION(NCOVMAT), INTENT(IN) :: COVMATARR
!
  ! local variables
    INTEGER :: IC, IR, I, J, K, NC, NNZ, NR, M
    INTEGER(KIND=8) :: IPOS, COV_NR
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: COV_TEMP
    DOUBLE PRECISION :: DZERO = 0.0D0
!
    100 FORMAT(1X,A)
!
    IF( MNOR_DIM == 1 )THEN
      AMESSAGE = &
      'TERMINATING: Only one parameter follows multinormal distribution.' // &
      ' Please use normal distribution instead!'
      CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
      CLOSE(IOUT)
      CALL UTL_STOP()
!
    ELSEIF( MNOR_DIM > 1 )THEN
      ALLOCATE( MNOR_MEAN( MNOR_DIM ) )
      ALLOCATE( COV_MAT(MNOR_DIM, MNOR_DIM) )
      ALLOCATE( PARM( MNOR_DIM, MNOR_DIM ) )

      IF (NCOVMAT > 0) THEN
        WRITE(IOUT,*)
        WRITE(IOUT,100)'*****************************************************'
        WRITE(IOUT,100)'                       NOTE                          '
        WRITE(IOUT,100)'   THE NUMBER OF COVARIANCE MATRIX FOR MULTINORMAL   '
        WRITE(IOUT,100)'           DISTRIBUTION CAN ONLY BE ONE              '
        WRITE(IOUT,100)'*****************************************************'
        WRITE(IOUT,*)
      ENDIF

    ! Read the covariance matrix of the multinormal density from Matrix_Files input block
    ! First decide which matrix corresponds to the prior PDF
      DO M=1, NCOVMAT
        IF (COVMATARR(M)%ARRAYNAME==MATRIXNAM(MATRIXINX)) THEN
!                
          ALLOCATE(COV_TEMP(COVMATARR(M)%NC))
          NR = COVMATARR(M)%NR
          NC = COVMATARR(M)%NC
          NNZ = COVMATARR(M)%NNZ
!
          IF( COVMATARR(M)%NC /= MNOR_DIM )THEN
            AMESSAGE = 'TERMINATING: Covariance matrix dimension in matrix file' // &
            ' must be the same with the number of parameters having multinormal' // &
            ' PDF type in MCMC_Prior_PDF block!'
            CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
            CLOSE(IOUT)
            CALL UTL_STOP()        
          ENDIF          
!
          COV_NR = NR
          DO I=1,NR
            COV_TEMP = DZERO
            DO K=1,NNZ
              IPOS = COVMATARR(M)%IPOS(K)
              IR = MOD((IPOS-1),COV_NR)+1
              IF (IR==I) THEN
                IC = INT((DBLE(IPOS)-0.1)/DBLE(NR))+1
                COV_TEMP(IC) = COVMATARR(M)%DVAL(K)
              ENDIF
            ENDDO
!
            DO J=1,NC
              COV_MAT(I,J)=COV_TEMP(J)
            ENDDO
          ENDDO
      
        ENDIF
      ENDDO
!
!
    ! Collect the mean values of the multinormal desity
      DO I = 1, MNOR_DIM
        MNOR_MEAN(I) = PDF_VAR1(MNOR_IND(I))
      END DO
!
      CALL R8MAT_POFAC( MNOR_DIM, COV_MAT, PARM)
!
    ENDIF
!
    IF (ALLOCATED(COV_TEMP)) DEALLOCATE(COV_TEMP)
!
  END SUBROUTINE MCMC_PRIOR_STORE
!
!
!-------------------------------------------------------------------------------
!***  Read variables from ucode main program to initialize likelihood function calculation
!
  SUBROUTINE MCMC_INIT_MODEL(NW,NCATS,NEOBS,OBSEXTNAM,MCVUSE,NOBS,OBSNAM, &
                        WTFULLSQR,NDOBS,MODEXTVAL,MODDERVAL,OBSEXTOBSNUM, &
                        OBSDEROBSNUM,IEQNPTR,PARALLEL_ACTIVE,LCIS,NINSTRUCT, &
                        NMIFILE,NMOFILE,NUMLADV,CATGOR,CINSTSET, &
                        INSTRUCTFILE,LOCINS,MIFILE,MOFILE,MRKDL,TFILE)
!
    IMPLICIT NONE
!
    INTEGER :: NCATS, NEOBS, NOBS, NDOBS, IEQNPTR(2)
    INTEGER :: LCIS,NINSTRUCT,NMIFILE,NMOFILE,NUMLADV
    INTEGER :: NW(PAR_DIM),OBSEXTOBSNUM(NEOBS), &
               OBSDEROBSNUM(NDOBS),LOCINS(NINSTRUCT)
    CHARACTER (LEN=LENDNAM) :: OBSEXTNAM(NEOBS)
    CHARACTER (LEN=LENDNAM) :: OBSNAM(NOBS)
    LOGICAL :: MCVUSE(NCATS),PARALLEL_ACTIVE
    TYPE (CDMATRIX) :: WTFULLSQR
    DOUBLE PRECISION :: MODEXTVAL(NEOBS), MODDERVAL(NDOBS)
    CHARACTER(LEN=6) :: CATGOR(NMOFILE)
    CHARACTER(LEN=1) :: CINSTSET(LCIS),MRKDL(NMOFILE)
    CHARACTER(LEN=MAX_STRING_LEN) :: INSTRUCTFILE(NMOFILE),MIFILE(NMIFILE)
    CHARACTER(LEN=MAX_STRING_LEN) :: MOFILE(NMOFILE),TFILE(NMIFILE)
!
!
    ALLOCATE(MC_NW(PAR_DIM),MC_OBSEXTNAM(NEOBS),MC_MODEXTVAL(NEOBS), &
             MC_MODDERVAL(NDOBS),MC_OBSNAM(NOBS),MC_MCVUSE(NCATS),&
             MC_OBSEXTOBSNUM(NEOBS),MC_OBSDEROBSNUM(NDOBS))
!
    ALLOCATE(MC_CATGOR(NMOFILE),MC_CINSTSET(LCIS),MC_INSTRUCTFILE(NMOFILE), &
             MC_LOCINS(NINSTRUCT),MC_MIFILE(NMIFILE),MC_MOFILE(NMOFILE), &
             MC_MRKDL(NMOFILE),MC_TFILE(NMIFILE))
!             
    INDSIM=0 
    ALLOCATE(PRINT_SIM(NOBS,NCHAIN,MAX_GEN),FIT_SIM(NCHAIN,MAX_GEN))
!
!
    IF (.NOT. PARALLEL_ACTIVE) THEN
      ALLOCATE(MC_DEPDERVALSETS(NDOBS,1),MC_DEPEXTVALSETS(NEOBS,1))
    ELSE
      ALLOCATE(MC_DEPDERVALSETS(NDOBS,nchain),MC_DEPEXTVALSETS(NEOBS,nchain))
    ENDIF
!
    MC_NCATS=NCATS          !2 categories, observation or prediction
    MC_NEOBS=NEOBS          !number of extracted observations or predictions, used in instruction file
    MC_NOBS=NOBS            !number of used observations(to calculate residuals) or predictions(to print out results)
    MC_NDOBS=NDOBS
    MC_MODEXTVAL=MODEXTVAL
    MC_MODDERVAL=MODDERVAL
    MC_NW=NW                !Minimum word length of a parameter
    MC_OBSEXTNAM=OBSEXTNAM
    MC_OBSNAM=OBSNAM
    MC_MCVUSE=MCVUSE        !if it is observation MCVUSE(1)=true, if prediction, MCVUSE(2)=true
    MC_WTFULLSQR=WTFULLSQR  !sqrt of full weight matrix
    MC_OBSEXTOBSNUM=OBSEXTOBSNUM
    MC_OBSDEROBSNUM=OBSDEROBSNUM
    MC_IEQNPTR=IEQNPTR
    MC_PARALLEL_ACTIVE=PARALLEL_ACTIVE
!
    MC_LCIS=LCIS
    MC_NINSTRUCT=NINSTRUCT
    MC_NMIFILE=NMIFILE
    MC_NMOFILE=NMOFILE
    MC_NUMLADV=NUMLADV
    MC_CATGOR=CATGOR
    MC_CINSTSET=CINSTSET
    MC_INSTRUCTFILE=INSTRUCTFILE
    MC_LOCINS=LOCINS
    MC_MIFILE=MIFILE
    MC_MOFILE=MOFILE
    MC_MRKDL=MRKDL
    MC_TFILE=TFILE
!
  END SUBROUTINE MCMC_INIT_MODEL
!
!
!-------------------------------------------------------------------------------
!***  Start markov chains
!
!  If restart_flag='yes', then start chains from the last sample of previous simulation
!  If reg_flag='yes', then start chains from multinormal prior distribution with mean from '_paopt' and covariance from '_mv'
!  Otherwise, start chains from user specified prior distribution in MCMC_Prior input block
!
!
  SUBROUTINE MCMC_INIT_CHAIN( )
!
    IMPLICIT NONE
!
    INTEGER          :: I, IUNIT, DUMMYI, ISTAT
    DOUBLE PRECISION :: INIT_SAMPLE(PAR_DIM)
    CHARACTER        :: DUMMY
!
    DOUBLE PRECISION :: INIT_MEAN(PAR_DIM)
    DOUBLE PRECISION :: INIT_COV(PAR_DIM, PAR_DIM)
    DOUBLE PRECISION :: INIT_PARM(PAR_DIM, PAR_DIM) ! an upper triangular matrix such that A = R'*R
!
    ALLOCATE( Z(PAR_DIM, NCHAIN, MAX_GEN) )
    ALLOCATE( FIT( NCHAIN, MAX_GEN ) )
    ALLOCATE( ZP( PAR_DIM) )
!
  ! Format statements
    100 FORMAT(1X,A)
    101 FORMAT(/,79('-'),/)
    102 FORMAT(20X,'MARKOV CHAIN MONTE CARLO RUN RECORD',/)
    110 FORMAT(20X,'*** MARKOV CHAIN MONTE CARLO RUN ***',/)
    103 FORMAT(1X,'Undertaking initial MCMC runs --------',/)
    104 FORMAT(1X,'Restart = yes, the initial sample is read &
               from ._mcmc_restart file',/)
    105 FORMAT(1X,'UseRegResult = yes, the initial sample is drawn from &
               multinormal distribution with mean from ._paopt file and &
               covariance from ._mv file',/)
    106 FORMAT(1X,'The initial sample is drawn from the prior distribution of &
               the parameters specified by users in MCMC_PRIOR input block',/)
    107 FORMAT(/,1X,'--- Iteration number ',I8)
    108 FORMAT(/,1X,'*** Total ',I4, ' chains are running',/)
    13  FORMAT(1X,'Converged: ',A1)
!
    WRITE(OUTUNIT,101)
    WRITE(OUTUNIT,102)
    WRITE(OUTUNIT,103)
    WRITE(*,101)
    WRITE(*,110)
    WRITE(*,101)
!
!
  ! if restart the chain, continue the chain from the last sample of last time simulation
!
    IF( RESTART_FLAG )THEN
!
      WRITE(*,104)
      WRITE(OUTUNIT,104)
      WRITE(*,107)1
      WRITE(*,108)NCHAIN
!
      IUNIT = UTL_GETUNIT(101,150)
      OPEN(IUNIT, FILE = TRIM(FILE_PREFIX)//'._mcmc_restart' )
      READ(IUNIT,*)
      READ(IUNIT,*)
      DO I = 1, NCHAIN
        READ(IUNIT, *)DUMMYI, FIT(I,1), Z(:,I,1)
      END DO
      READ(IUNIT,13) CONV_RESTART
      CALL UTL_CASETRANS(CONV_RESTART,'hi')
      CLOSE(IUNIT)

   ! initialize the chain from regression results by assuming all parameters
   ! follow multinormal distribution with mean from '_paopt' and convariance from '_mv'
!
    ELSE
!
      IF( REG_FLAG )THEN
        WRITE(*,105)
        WRITE(OUTUNIT,105)
        WRITE(*,107)1
        WRITE(*,108)NCHAIN
!
      ! read parameter values from regression output '._paopt'
        IUNIT = UTL_GETUNIT(101,150)
        OPEN(IUNIT, FILE = TRIM(FILE_PREFIX)//'._paopt', IOSTAT=ISTAT)
        IF (ISTAT /= 0)THEN
          CALL UTL_STOP('TERMINATING: Check regression output file _paopt!')
        ENDIF
        READ(IUNIT,*)
        DO I = 1, PAR_DIM
          READ(IUNIT,*) DUMMY, INIT_MEAN(I)
        ENDDO
        CLOSE(IUNIT)
!
      ! read parameter covariance matrix from regression output '_mv'
        IUNIT = UTL_GETUNIT(101,150)
        OPEN(IUNIT, FILE = TRIM(FILE_PREFIX)//'._mv', IOSTAT=ISTAT)
        IF (ISTAT /= 0)THEN
          CALL UTL_STOP('TERMINATING: Check regression output file _mv!')
        ENDIF
        READ(IUNIT,*)
        DO I = 1, PAR_DIM
          READ(IUNIT,*) INIT_COV(I,:)
        ENDDO
        CLOSE(IUNIT)
!
      ! get initial sample from the default multinormal distribution
        CALL R8MAT_POFAC( PAR_DIM, INIT_COV, INIT_PARM)
        DO I = 1, NCHAIN
          CALL R8VEC_MULTINORMAL_SAMPLE( PAR_DIM, INIT_MEAN, INIT_PARM, &
                                                          INIT_SAMPLE)
          Z(:,I,1) = INIT_SAMPLE
!
      !**************    
       ! Check whether candidate sample is out of parameter bound
          ZP=Z(:,I,1)
          CALL OUTOFBOUND( ) 
          Z(:,I,1)=ZP
      !**************    
!          
        ENDDO
!
        FIT(:,1) = LIKELIHOOD( Z(:,:,1) )
!
      ELSE
        WRITE(*,106)
        WRITE(OUTUNIT,106)
        WRITE(*,107)1
        WRITE(*,108)NCHAIN
        DO I =1, NCHAIN
          Z(:,I,1) = GET_PRIOR_SAMPLE( )
!          
      !**************    
       ! Check whether candidate sample is out of parameter bound
          ZP=Z(:,I,1)
          CALL OUTOFBOUND( ) 
          Z(:,I,1)=ZP
      !**************         
!          
        ENDDO
        FIT(:,1) = LIKELIHOOD( Z(:,:,1) )
      ENDIF
!
    ENDIF
!
!   Write MCMC simulation information to output file

    WRITE(OUTUNIT,100)'Undertaking MCMC process --------'
    WRITE(OUTUNIT,*)
    WRITE(OUTUNIT,100)'********************************************************' // &
         '*********************************************************************'
    WRITE(OUTUNIT,100) &
         'For more simulation results please see following output files: '
    WRITE(OUTUNIT,100) &
         '*** Parameter samples printed in ._mcmc_par# where # is chain number. '
    WRITE(OUTUNIT,100)'*** Gelman-Rubin R printed in ._mcmc_grr.'
    WRITE(OUTUNIT,100)'*** If the PrintModelOutput flag is yes,' // & 
          ' the model outputs for parameter samples are printed in ._mcmc_sim#.'
    WRITE(OUTUNIT,100)'*** Restart information printed in ._mcmc_restart. '
    WRITE(OUTUNIT,100)'*** Acceptance rate printed at the end of this file' // &
          ' when simulation is done.'
    WRITE(OUTUNIT,100)'*** When calculated Gelman-Rubin R is smaller than' // &
         ' the given value, the iteration number is printed in this file.'
    WRITE(OUTUNIT,100)'    Please note that the R value may fluctuate' // &
         ' at the early iterations.'       
    WRITE(OUTUNIT,100)'*** For other methods to check convergence, users can' // &
          ' plot evoluation of the samples, sample mean and sample variance.'
    WRITE(OUTUNIT,*)
    WRITE(OUTUNIT,100)'*** If chains converged (flag "T" is printed in'  // &
         ' the ._mcmc_restart file) and users restart the MCMC process' // &
         ' to obtain more samples,' 
    WRITE(OUTUNIT,100)'    then in the restart run all the generated samples' // &
        ' can be used and Gelman-Rubin R will not be printed at this time.'
    WRITE(OUTUNIT,100)'********************************************************' // &
         '*********************************************************************'
    WRITE(OUTUNIT,*)
!
  END SUBROUTINE MCMC_INIT_CHAIN
!
!
!*******************************************************************************
!
!                       FUNCTIONS TO CALCULATE LOG LIKELIHOOD
!
!*******************************************************************************
!
  FUNCTION LIKELIHOOD( ZX )
!
    IMPLICIT NONE
!
    DOUBLE PRECISION :: ZX(PAR_DIM,NCHAIN)  ! Zx is sample of parameters, 
    DOUBLE PRECISION :: ZXTEMP(PAR_DIM), ZXTEMPPLL(PAR_DIM,NCHAIN)
    DOUBLE PRECISION :: LIKELIHOOD(NCHAIN)
!
    INTEGER          :: IFAIL, I, IPLL
    DOUBLE PRECISION :: MC_MODELVAL(MC_NOBS)
    CHARACTER (LEN=MAX_STRING_LEN) :: MC_INSTRUCTION
    DOUBLE PRECISION :: MC_RESIDS(MC_NOBS)
    DOUBLE PRECISION :: MC_WTDRESIDS(MC_NOBS)
!
    204 FORMAT(/,1X,'Error in instruction: "',A,'"')
!    
    INDSIM=INDSIM+1    
!
    IF (.NOT. MC_PARALLEL_ACTIVE) THEN
!
      DO IPLL=1,NCHAIN
!
      ! ***Adapt parameter values
!
        ZXTEMP = ZX(:,IPLL)
        CALL MIO_ADA_WRITEFILES(IFAIL,PAR_DIM,MC_PARNAMLC,0,MC_NW,1,ZXTEMP) ! 1 is double precision
        IF (IFAIL .NE. 0) CALL UTL_STOP(' ')
!
      ! ***Execute application model
!
        CALL BAS_EXE(1,-1) ! 1 is one command
!
      ! ***Extract values from model output
!
        CALL MIO_EXT(IFAIL,OUTUNIT,MC_NCATS,MC_NEOBS,MC_OBSEXTNAM,MC_MCVUSE, &
                     MC_DEPEXTVALSETS(:,1),MC_INSTRUCTION)
        IF (IFAIL .NE. 0) THEN
          WRITE(OUTUNIT,204) TRIM(MC_INSTRUCTION)
          CALL UTL_STOP(' ')
        ENDIF
!
      ! ***Get simulated values used for calculating likelihood function
!
        IF(MC_NDOBS > 0) THEN
          CALL DEP_EXT_DER(OUTUNIT,MC_NDOBS,MC_NEOBS,MC_IEQNPTR(1), &
                          MC_DEPEXTVALSETS(:,1),MC_DEPDERVALSETS(:,1))
          IF (IFAIL .NE. 0) THEN
            WRITE(OUTUNIT,204) TRIM(MC_INSTRUCTION)
            CALL UTL_STOP(' ')
          ENDIF
        ENDIF
!
        DO I=1,MC_NEOBS
          MC_MODEXTVAL(I) = MC_DEPEXTVALSETS(I,1)
          IF(MC_OBSEXTOBSNUM(I) > 0) THEN
             MC_MODELVAL(MC_OBSEXTOBSNUM(I)) = MC_MODEXTVAL(I)
          ENDIF
        ENDDO
!
        IF (MC_NDOBS > 0) THEN
          DO I=1,MC_NDOBS
            MC_MODDERVAL(I) = MC_DEPDERVALSETS(I,1)
            IF(MC_OBSDEROBSNUM(I) > 0) THEN
              MC_MODELVAL(MC_OBSDEROBSNUM(I)) = MC_MODDERVAL(I)
            ENDIF
          ENDDO
        ENDIF
!
      ! ***Calculate SSWR
!
        CALL DEP_UEV_RESIDUALS(MC_NOBS,MC_MODELVAL,MC_WTFULLSQR, &
                               MC_RESIDS,MC_WTDRESIDS) ! MC_WTDRESIDS saved weighted residuals
!
      ! ***log likelihood function equals -0.5*SSWR
!
        LIKELIHOOD(IPLL) = -0.5D0 * SUM( (MC_WTDRESIDS) **2 )
!
        FIT_SIM(IPLL,INDSIM)=LIKELIHOOD(IPLL)
        PRINT_SIM(:,IPLL,INDSIM)=MC_MODELVAL
!
      ENDDO ! enddo IPLL
!
    ELSE  ! run in parallel
!
    ! ***Adapt, execute, and extract in parallel
!
      ZXTEMPPLL = ZX
!
      CALL PLL_MAKE_RUNS(MC_LCIS,MC_NCATS,MC_NINSTRUCT,MC_NMIFILE,MC_NMOFILE, &
                         0,PAR_DIM,MC_NUMLADV,NCHAIN,MC_NEOBS,1, &
                         MC_CATGOR,MC_CINSTSET,MC_OBSEXTNAM,MC_INSTRUCTFILE, &
                         MC_LOCINS,MC_MCVUSE,MC_MIFILE,MC_MOFILE,MC_MRKDL, &
                         MC_NW,MC_PARNAMLC,ZXTEMPPLL,MC_TFILE, &
                         MC_DEPEXTVALSETS,MODCOMLINE(1))
!
    ! ***Get simulated values used for calculating likelihood function
!
      DO IPLL=1,NCHAIN
!
        IF(MC_NDOBS > 0) THEN
          CALL DEP_EXT_DER(OUTUNIT,MC_NDOBS,MC_NEOBS,MC_IEQNPTR(1), &
                           MC_DEPEXTVALSETS(:,IPLL),MC_DEPDERVALSETS(:,IPLL))
          IF (IFAIL .NE. 0) THEN
            WRITE(OUTUNIT,204) TRIM(MC_INSTRUCTION)
            CALL UTL_STOP(' ')
          ENDIF
        ENDIF
!
        DO I=1,MC_NEOBS
          MC_MODEXTVAL(I) = MC_DEPEXTVALSETS(I,IPLL)
          IF(MC_OBSEXTOBSNUM(I) > 0) THEN
            MC_MODELVAL(MC_OBSEXTOBSNUM(I)) = MC_MODEXTVAL(I)
          ENDIF
        ENDDO
!
        IF (MC_NDOBS > 0) THEN
          DO I=1,MC_NDOBS
            MC_MODDERVAL(I) = MC_DEPDERVALSETS(I,IPLL)
            IF(MC_OBSDEROBSNUM(I)>0) THEN
              MC_MODELVAL(MC_OBSDEROBSNUM(I)) = MC_MODDERVAL(I)
            ENDIF
          ENDDO
        ENDIF
!
      ! ***Calculate SSWR
!
        CALL DEP_UEV_RESIDUALS(MC_NOBS,MC_MODELVAL,MC_WTFULLSQR, &
                               MC_RESIDS,MC_WTDRESIDS) ! MC_WTDRESIDS saved weighted residuals
!
      ! ***log likelihood function equals -0.5*SSWR
!
        LIKELIHOOD(IPLL) = -0.5D0 * SUM( (MC_WTDRESIDS) **2 )
!        
        FIT_SIM(IPLL,INDSIM)=LIKELIHOOD(IPLL)
        PRINT_SIM(:,IPLL,INDSIM)=MC_MODELVAL
!
      ENDDO  ! enddo IPLL
!                        
    ENDIF ! endif (.not. mc_parallel_active)
!
  END FUNCTION likelihood
!
!
!*******************************************************************************
!
!                       FUNCTIONS TO RUN PREDICTION
!
!*******************************************************************************
!
  SUBROUTINE MCMC_PREDICTION_RUN()
!
    IMPLICIT NONE
!
    DOUBLE PRECISION :: TEMPL
    DOUBLE PRECISION :: PAR_SAMPLE_SEQ(PAR_DIM), PAR_SAMPLE_PLL(PAR_DIM,NCHAIN)
    INTEGER          :: IUNIT, IUNITPTMP, IUNITP(NCHAIN)
    CHARACTER*10     :: IC
    DOUBLE PRECISION, ALLOCATABLE :: PAR_SAMPLE(:,:,:)
!
    INTEGER          :: IFAIL, ISTAT, ICHAIN, I, J, K, IPAR, IPRED, IPLL
    INTEGER,ALLOCATABLE :: TOUTPRINT(:)
    DOUBLE PRECISION :: MC_MODELVAL(MC_NOBS)
    CHARACTER (LEN=MAX_STRING_LEN) :: MC_INSTRUCTION
    
    INTEGER          :: PREPRINT, POSTPRINT
!
    204 FORMAT(/,1X,'Error in instruction: "',A,'"')
    205 FORMAT(1X,'File open failed for ',A,' status = ',I5, &
               /,' Parameter samples must be saved in the file &
               before performing MCMC_PREDICTION run')
    100 FORMAT(1X,A)
    101 FORMAT(/,79('-'),/)
    102 FORMAT(20X,'MARKOV CHAIN MONTE CARLO PREDICTION RUN RECORD',/)
    110 FORMAT(20X,'*** MARKOV CHAIN MONTE CARLO PREDICTION RUN ***',/)
    103 FORMAT(/,1X,'--- Prediction run for parameter sample ',i8,/)
!
    WRITE(*,101)
    WRITE(*,110)
    WRITE(*,101)
!
    WRITE(OUTUNIT,101)
    WRITE(OUTUNIT,102)
    WRITE(OUTUNIT,100) &
          '***************************************************************'
    WRITE(OUTUNIT,100) &
         'PREDICTION RUN FOR EACH PARAMETER SAMPLE IS PERFORMING ......'
    WRITE(OUTUNIT,*)
    WRITE(OUTUNIT,100) &
         'To see prediction results please check ._mcmc_pred# files'
    WRITE(OUTUNIT,100) &
          '***************************************************************'
    WRITE(OUTUNIT,*)
!
    IUNITPTMP = UTL_GETUNIT(200,400)
!
    IF (PRINTSTEP > ITSTARTPRED)THEN
      WRITE(*,*)'TERMINATING: ItStartPred must be larger than PrintStep, ' // &
                'and be the same value used in the MCMC run, ' // &
                'Please check the input of these two keywords!'
      WRITE(OUTUNIT,100)'ERROR: ItStartPred must be larger than PrintStep, ' // &
                'and be the same value used in the MCMC run, ' // &
                'Please check the input of these two keywords!'           
      CALL UTL_STOP()
    ENDIF
!    
    IF (PRINTSTEP > MAX_GEN)THEN
      WRITE(*,*)'TERMINATING: MaxSamples must be larger than PrintStep, ' // &
                'and not larger than the value used in the MCMC run, ' // &
                'Please check the input of these two keywords!'
      WRITE(OUTUNIT,100)'ERROR: MaxSamples must be larger than PrintStep, ' // &
                'and not larger than the value used in the MCMC run, ' // &
                'Please check the input of these two keywords!'           
      CALL UTL_STOP()
    ENDIF
!
    IF (PRINTSTEP == 1) THEN
       PREPRINT = ITSTARTPRED
       POSTPRINT = MAX_GEN - PREPRINT
    ELSEIF(PRINTSTEP > 1) THEN
       PREPRINT=INT(ITSTARTPRED/PRINTSTEP)
       IF (MOD(ITSTARTPRED, PRINTSTEP) .NE. 0)THEN
          PREPRINT=PREPRINT+1
       ENDIF
!    
       POSTPRINT=INT(MAX_GEN/PRINTSTEP)
       IF (MOD(MAX_GEN, PRINTSTEP) .NE. 0)THEN
         POSTPRINT=POSTPRINT+1
       ENDIF
       POSTPRINT=POSTPRINT-PREPRINT+1
    ENDIF
!   
    ALLOCATE(TOUTPRINT(POSTPRINT))
    ALLOCATE(PAR_SAMPLE(PAR_DIM,NCHAIN,POSTPRINT))
!
    DO ICHAIN = 1, NCHAIN
!
    ! Open file _mcmc_par## to read parameter samples
      IF(NCHAIN < 100 ) THEN
        WRITE(IC,'(I2.2)') ICHAIN
      ELSE
        WRITE(IC,'(I3.3)') ICHAIN
      ENDIF
      IUNIT = UTL_GETUNIT(200,400)
      OPEN(IUNIT, FILE = TRIM(FILE_PREFIX)//'._mcmc_par'//TRIM(IC),STATUS='OLD', &
           ACTION='READ',IOSTAT=ISTAT)
      IF(ISTAT .NE. 0)THEN
        WRITE(*,205)TRIM(FILE_PREFIX)//'._mcmc_par'//TRIM(IC), ISTAT
        CALL UTL_STOP(' ')
      ELSE        
        DO K = 1, 2+1+PREPRINT-1
          READ(IUNIT,*)
        ENDDO
      ENDIF
!
      DO J = 1, POSTPRINT
        READ(IUNIT,*)TOUTPRINT(J), TEMPL, (PAR_SAMPLE(IPAR,ICHAIN,J), IPAR=1, PAR_DIM )
      ENDDO
      CLOSE(IUNIT)
!
    ! Open file _mcpred## to write prediction samples
      IUNITP(ICHAIN)=IUNITPTMP+ICHAIN
      OPEN(IUNITP(ICHAIN), FILE = TRIM(FILE_PREFIX)//'._mcmc_pred'//TRIM(IC), &
           STATUS='REPLACE')
      WRITE(IUNITP(ICHAIN),31)TRIM(IC)
      WRITE(IUNITP(ICHAIN),32)(TRIM(MC_OBSNAM(IPRED)), IPRED=1, MC_NOBS)
!
    ENDDO  ! enddo ichain
!
    DO J = 1, POSTPRINT
!
      WRITE(*,103) TOUTPRINT(J)
!
      IF (.NOT. MC_PARALLEL_ACTIVE) THEN
!
        DO IPLL=1,NCHAIN
!
        ! ***Adapt parameter values
!
          PAR_SAMPLE_SEQ = PAR_SAMPLE(:,IPLL,J)
          CALL MIO_ADA_WRITEFILES(IFAIL,PAR_DIM,MC_PARNAMLC,0,MC_NW,1, &
                                  PAR_SAMPLE_SEQ)
          IF (IFAIL .NE. 0) CALL UTL_STOP(' ')
!
        ! ***Execute application model
!
          CALL BAS_EXE(1,-1)
!
        ! ***ExtraCT VALUES FROM MODEL OUTPUT
!
          CALL MIO_EXT(IFAIL,OUTUNIT,MC_NCATS,MC_NEOBS,MC_OBSEXTNAM,MC_MCVUSE, &
                       MC_DEPEXTVALSETS(:,1),MC_INSTRUCTION)
          IF (IFAIL .NE. 0) THEN
            WRITE(OUTUNIT,204) TRIM(MC_INSTRUCTION)
            CALL UTL_STOP(' ')
          ENDIF
!
        ! ***GET PREDICTED VALUES
!
          IF(MC_NDOBS > 0) THEN
            CALL DEP_EXT_DER(OUTUNIT,MC_NDOBS,MC_NEOBS,MC_IEQNPTR(1), &
                             MC_DEPEXTVALSETS(:,1),MC_DEPDERVALSETS(:,1))
            IF (IFAIL .NE. 0) THEN
              WRITE(OUTUNIT,204) TRIM(MC_INSTRUCTION)
              CALL UTL_STOP(' ')
            ENDIF
          ENDIF
!
          DO I=1,MC_NEOBS
            MC_MODEXTVAL(I) = MC_DEPEXTVALSETS(I,1)
            IF(MC_OBSEXTOBSNUM(I) > 0) THEN
              MC_MODELVAL(MC_OBSEXTOBSNUM(I)) = MC_MODEXTVAL(I)
            ENDIF
          ENDDO
!
          IF (MC_NDOBS > 0) THEN
            DO I=1,MC_NDOBS
              MC_MODDERVAL(I) = MC_DEPDERVALSETS(I,1)
              IF(MC_OBSDEROBSNUM(I) > 0) THEN
                MC_MODELVAL(MC_OBSDEROBSNUM(I)) = MC_MODDERVAL(I)
              ENDIF
            ENDDO
          ENDIF
!
        ! ***WRITE PREDICTION VALUES INTO FILES
!
          WRITE(IUNITP(IPLL),30) J, MC_MODELVAL(:)
!
        ENDDO ! enddo IPLL
!
      ELSE  ! run in parallel
!
      ! ***Adapt, execute, and extract in parallel
!
        PAR_SAMPLE_PLL = PAR_SAMPLE(:,:,J)
!
        CALL PLL_MAKE_RUNS &
                         (MC_LCIS,MC_NCATS,MC_NINSTRUCT,MC_NMIFILE,MC_NMOFILE, &
                           0,PAR_DIM,MC_NUMLADV,NCHAIN,MC_NEOBS,1, &
                           MC_CATGOR,MC_CINSTSET,MC_OBSEXTNAM,MC_INSTRUCTFILE, &
                           MC_LOCINS,MC_MCVUSE,MC_MIFILE,MC_MOFILE,MC_MRKDL, &
                           MC_NW,MC_PARNAMLC,PAR_SAMPLE_PLL,MC_TFILE, &
                           MC_DEPEXTVALSETS,MODCOMLINE(1))
!
      ! ***Get predicted values
!
        DO IPLL=1,NCHAIN
          IF(MC_NDOBS > 0) THEN
            CALL DEP_EXT_DER(OUTUNIT,MC_NDOBS,MC_NEOBS,MC_IEQNPTR(1), &
                             MC_DEPEXTVALSETS(:,IPLL),MC_DEPDERVALSETS(:,IPLL))
          ENDIF
!
          DO I=1,MC_NEOBS
            MC_MODEXTVAL(I) = MC_DEPEXTVALSETS(I,IPLL)
            IF(MC_OBSEXTOBSNUM(I) > 0) THEN
              MC_MODELVAL(MC_OBSEXTOBSNUM(I)) = MC_MODEXTVAL(I)
            ENDIF
          ENDDO
!
          IF (MC_NDOBS > 0) THEN
            DO I=1,MC_NDOBS
              MC_MODDERVAL(I) = MC_DEPDERVALSETS(I,IPLL)
              IF(MC_OBSDEROBSNUM(I) > 0) THEN
                MC_MODELVAL(MC_OBSDEROBSNUM(I)) = MC_MODDERVAL(I)
              ENDIF
            ENDDO
          ENDIF
!
        ! ***Write prediction values into files
!
          WRITE(iunitp(IPLL),30) J, MC_MODELVAL(:)
!
        ENDDO  ! enddo IPLL
!
      ENDIF ! endif (.not. mc_parallel_active)
!
    ENDDO ! enddo j=1,max_gen-itstartpred
!
    DO ICHAIN = 1, NCHAIN
      CLOSE(IUNITP(ICHAIN))
    ENDDO
!
    WRITE(OUTUNIT,*)
    WRITE(OUTUNIT,40)NCHAIN,POSTPRINT
    WRITE(OUTUNIT,*)
!
    31 FORMAT(1X,'"PREDICTION VALUES CALCULATED FROM PARAMETER SAMPLES &
              AFTER CONVERGENCE FOR CHAIN # ',A,'"')
    32 FORMAT(1X,'"Number"',4X,1000('"',A12,'"',2X,:))
    30 FORMAT(1X,I7,6X,1000(ES14.7,2X))
    40 FORMAT(1X,'NUMBER OF PROCESS MODEL RUNS FOR THIS ANALYSIS:  ',I3,' &
              CHAINS   *',I10,' SAMPLES')
!
    IF (ALLOCATED(PAR_SAMPLE)) DEALLOCATE(PAR_SAMPLE)
!
  END SUBROUTINE MCMC_PREDICTION_RUN
!
!
!*******************************************************************************
!
!                       FUNCTIONS ABOUT PRIOR INFORMATION
!
!*******************************************************************************

!-------------------------------------------------------------------------------
!***  Get a Sample from the specified prior distribution to start MCMC
!
  FUNCTION GET_PRIOR_SAMPLE( )
!
    IMPLICIT NONE
!
    DOUBLE PRECISION    :: GET_PRIOR_SAMPLE( PAR_DIM )
    INTEGER             :: I
    DOUBLE PRECISION    :: TMP(MNOR_DIM)
!
  ! generate samples follow univariate density
    IF( UNI_DIM > 0 )THEN
      DO I = 1, UNI_DIM
!
        SELECT CASE( TRIM(PDF_TYPE(UNI_IND(I))) )
!
          CASE('uniform')
            GET_PRIOR_SAMPLE(UNI_IND(I)) = R8_UNIFORM_SAMPLE( &
                           PDF_VAR1(UNI_IND(I)), PDF_VAR2(UNI_IND(I)) )
!
          CASE('normal')
            GET_PRIOR_SAMPLE(UNI_IND(I)) = R8_NORMAL_SAMPLE( &
                           PDF_VAR1(UNI_IND(I)), PDF_VAR2(UNI_IND(I)) )
!
          CASE('log-normal')
            GET_PRIOR_SAMPLE(UNI_IND(I)) = R8_NORMAL_SAMPLE( &
                           PDF_VAR1(UNI_IND(I)), PDF_VAR2(UNI_IND(I)) )
            GET_PRIOR_SAMPLE(UNI_IND(I)) = EXP( GET_PRIOR_SAMPLE(UNI_IND(I)) )
!
          CASE('beta')
            GET_PRIOR_SAMPLE(UNI_IND(I)) = R8_BETA_SAMPLE( &
                           PDF_VAR1(UNI_IND(I)), PDF_VAR2(UNI_IND(I)) )
!
          CASE('chi-square')
            GET_PRIOR_SAMPLE(UNI_IND(I)) = R8_CHI_SAMPLE( &
                                               PDF_VAR1(UNI_IND(I)) )
!
          CASE('inv-chi-square')
            GET_PRIOR_SAMPLE(UNI_IND(I)) = R8_INVCHI_SAMPLE( &
                                               PDF_VAR1(UNI_IND(I)) )    
!
          CASE('scaled-inv-chi-square')
            GET_PRIOR_SAMPLE(UNI_IND(I)) = R8_SCINVCHI_SAMPLE( &
                            PDF_VAR1(UNI_IND(I)), PDF_VAR2(UNI_IND(I)) )            
!
          CASE('gamma')
            GET_PRIOR_SAMPLE(UNI_IND(I)) = R8_GAMMA_SAMPLE( &
                           PDF_VAR1(UNI_IND(I)), PDF_VAR2(UNI_IND(I)) )
!
          CASE('inv-gamma')
            GET_PRIOR_SAMPLE(UNI_IND(I)) = R8_INVGAM_SAMPLE( &
                           PDF_VAR1(UNI_IND(I)), PDF_VAR2(UNI_IND(I)) )
!
          CASE('exponential')
            GET_PRIOR_SAMPLE(UNI_IND(I)) = R8_EXPONENTIAL_SAMPLE( &
                                                   PDF_VAR1(UNI_IND(I)) )
!
          CASE DEFAULT
            CALL UTL_STOP('TERMINATING: UNKNOWN PDF TYPE IN MCMC_PRIOR BLOCK!')
!
        END SELECT
!
      ENDDO
    ENDIF
!
!
  ! generate samples follow multinormal density
    IF( MNOR_DIM > 1 )THEN
       CALL R8VEC_MULTINORMAL_SAMPLE( MNOR_DIM, MNOR_MEAN, PARM, TMP)
      DO I = 1, MNOR_DIM
        GET_PRIOR_SAMPLE(MNOR_IND(I)) = TMP(I)
      ENDDO
    ENDIF
!
  END FUNCTION GET_PRIOR_SAMPLE
!
!
!-------------------------------------------------------------------------------
!***  Compute density values of the specified prior density type to calculate Metropolis ratio
!
  FUNCTION PRIOR_DENSITY( RVAL )
!
    IMPLICIT NONE
!
    DOUBLE PRECISION    :: RVAL( PAR_DIM )
    DOUBLE PRECISION    :: PRIOR_DENSITY
    INTEGER             :: I
    DOUBLE PRECISION    :: TMP( MNOR_DIM )
!
    PRIOR_DENSITY = 1.0d0
!
  ! calculate density values for univariate density
    IF( UNI_DIM > 0 )THEN    
      DO I = 1, UNI_DIM
!
        SELECT CASE( TRIM(PDF_TYPE(UNI_IND(I))) )
!
          CASE('uniform')
             PRIOR_DENSITY =1.0d0  !for high-D, the value may be very small
           ! PRIOR_DENSITY = &
           !           PRIOR_DENSITY * R8_UNIFORM_PDF( PDF_VAR1(UNI_IND(I)), &
           !                 PDF_VAR2(UNI_IND(I)), RVAL(UNI_IND(I)) )                                                                   
!
          CASE('normal')
            PRIOR_DENSITY = &
                       PRIOR_DENSITY * R8_NORMAL_PDF( PDF_VAR1(UNI_IND(I)), &
                            PDF_VAR2(UNI_IND(I)), RVAL(UNI_IND(I)) )
!
          CASE('log-normal')
            PRIOR_DENSITY = &
                    PRIOR_DENSITY * R8_LOGNORMAL_PDF( PDF_VAR1(UNI_IND(I)), &
                            PDF_VAR2(UNI_IND(I)), RVAL(UNI_IND(I)) )
!
          CASE('beta')
            PRIOR_DENSITY = &
                         PRIOR_DENSITY * R8_BETA_PDF( PDF_VAR1(UNI_IND(I)), &
                            PDF_VAR2(UNI_IND(I)), RVAL(UNI_IND(I)) )
!
          CASE('chi-square')
            PRIOR_DENSITY = &
                          PRIOR_DENSITY * R8_CHI_PDF( PDF_VAR1(UNI_IND(I)), &
                            RVAL(UNI_IND(I)) )
!
          CASE('inv-chi-square')
            PRIOR_DENSITY = &
                       PRIOR_DENSITY * R8_INVCHI_PDF( PDF_VAR1(UNI_IND(I)), &
                            RVAL(UNI_IND(I)) )
!
          CASE('scaled-inv-chi-square')
            PRIOR_DENSITY = &
                     PRIOR_DENSITY * R8_SCINVCHI_PDF( PDF_VAR1(UNI_IND(I)), &
                            PDF_VAR2(UNI_IND(I)), RVAL(UNI_IND(I)) )
!
          CASE('gamma')
            PRIOR_DENSITY = &
                        PRIOR_DENSITY * R8_GAMMA_PDF( PDF_VAR1(UNI_IND(I)), &
                            PDF_VAR2(UNI_IND(I)), RVAL(UNI_IND(I)) )
!
          CASE('inv-gamma')
            PRIOR_DENSITY = &
                       PRIOR_DENSITY * R8_INVGAM_PDF( PDF_VAR1(UNI_IND(I)), &
                            PDF_VAR2(UNI_IND(I)), RVAL(UNI_IND(I)) )
!
          CASE('exponential')
            PRIOR_DENSITY = &
                  PRIOR_DENSITY * R8_EXPONENTIAL_PDF( PDF_VAR1(UNI_IND(I)), &
                            RVAL(UNI_IND(I)) )
!
        END SELECT
!
      ENDDO
    ENDIF
!
  ! calculate density values for multinormal density
    IF( MNOR_DIM > 1 )THEN
      DO I = 1, MNOR_DIM
        TMP(I) = RVAL(MNOR_IND(I))
      ENDDO
!      
      CALL R8MAT_PODET( MNOR_DIM, PARM, DET)
      PRIOR_DENSITY = R8VEC_MULTINORMAL_PDF( MNOR_DIM, MNOR_MEAN(1:MNOR_DIM), &
                                              PARM, DET, TMP )
    ENDIF
!    
  END FUNCTION PRIOR_DENSITY
!
!
!*******************************************************************************
!
!                       FUNCTIONS ABOUT DREAM ALGORITHM
!
!*******************************************************************************

!-------------------------------------------------------------------------------
!***  Use DREAM algorithm to get a candicate parameter sample
!
  SUBROUTINE MCMC_DREAM_ALGM( )
!
    IMPLICIT NONE
!
    INTEGER             :: I, J, ACCEPT, ZP_COUNT
    DOUBLE PRECISION    :: RATIO
    DOUBLE PRECISION    :: ZPTEMP(PAR_DIM,NCHAIN)
!
    ZP_COUNT = 0
    ACCEPT = 0
!
    102 FORMAT(/,1X,'--- Iteration number ',i8)
    103 FORMAT(/,1X,'*** Total ',i4, ' chains are running',/)
!
  ! Initialize the CR values
    CALL INIT_CR()
!
    DO I = 2, MAX_GEN
!
      WRITE(*,102)I
      WRITE(*,103)NCHAIN
!
      DO J = 1, NCHAIN
      ! Choose a CR value
        CALL CHOOSE_CR( )
!        
      ! Generate a candidate
        CALL GEN_CANDIDATE( I, J )
        ZP_COUNT = ZP_COUNT + 1
        ZPTEMP(:,J) = ZP
      ENDDO
!
    ! Compute log likelihood function in sequential or parallel
      FIT(:,I) = LIKELIHOOD( ZPTEMP )
!
      DO J = 1, NCHAIN
!      
      ! Compute the metroplis ratio
        RATIO = MIN( EXP( ( FIT(J,I) + LOG( PRIOR_DENSITY(ZPTEMP(:,J)) ) ) &
              - ( FIT(J,I-1) + LOG( PRIOR_DENSITY(Z(:,J,I-1)) ) ) ), 1.0D+00 )
!
        IF ( (RATIO == 1.0D+00) .OR. (RATIO >= R8_UNIFORM_01_SAMPLE()) ) THEN
          Z(:,J,I) = ZPTEMP(:,J)
          ACCEPT = ACCEPT + 1
        ELSE
          Z(:,J,I) = Z(:,J,I-1)
          FIT(J,I) = FIT(J,I-1)
          FIT_SIM(J,I)=FIT_SIM(J,I-1)
          PRINT_SIM(:,J,I)=PRINT_SIM(:,J,I-1)
        ENDIF
!
      ! Update CR distance
        IF( CONV_FLAG == .FALSE. .AND. NCR > 1 )THEN
          CALL UPDATE_CR_DIS( I, J )
        ENDIF
      ENDDO
!
    ! Update the multinomial distribution of CR
      IF (RESTART_FLAG == .FALSE. .OR. CONV_RESTART == 'F' ) THEN
        IF ( CONV_FLAG == .FALSE. .AND. NCR > 1 .AND. MOD(I,10)==0 ) THEN
          CALL UPDATE_PCR()
        ENDIF
      ENDIF
!
    ! Compute Gelman Rubin R
      IF ( (MOD(I, PRINTSTEP) == 0 .OR. I == MAX_GEN) .AND. I >= 4 ) THEN
        CALL GEL_RU(I)
      ENDIF
!
    ! Export result
    !*** Print centain steps based on PRINTSTEP
      IF ( I == 2 ) THEN
        CALL OUTPUT( I-1 )
        IF ( MODEL_OUTPUT ) THEN
          CALL OUTPUT_SIM( I-1 )
        ENDIF
      ENDIF
!      
      IF ( MOD(I, PRINTSTEP) == 0 .OR. I == MAX_GEN ) THEN
        CALL OUTPUT( I )  
        IF ( MODEL_OUTPUT ) THEN
          CALL OUTPUT_SIM( I ) 
        ENDIF
      ENDIF
!
    ! Outlier test
      IF (RESTART_FLAG == .FALSE. .OR. CONV_RESTART == 'F' ) THEN
        IF( CONV_FLAG == .FALSE. .AND. MOD(I,10) == 0 )THEN
          CALL OUTLIERTEST( I )
        ENDIF
      ENDIF
!
    ENDDO   !enddo max_gen
!
  ! Compute the acceptance rate
    AR = DFLOAT(ACCEPT) / DFLOAT(ZP_COUNT)
!
  ! Print out the acceptance rate on main output file.
    WRITE(OUTUNIT,*)
    WRITE(OUTUNIT,40)NCHAIN,MAX_GEN
    WRITE(OUTUNIT,*)
    WRITE(OUTUNIT,101)
    WRITE(OUTUNIT,*)'THE ACCEPTANCE RATE IS: ', AR
    WRITE(OUTUNIT,*)
!
    40  FORMAT(1x,'NUMBER OF PROCESS MODEL RUNS FOR THIS ANALYSIS:  ',I3,' &
                                                CHAINS   *',I10,' SAMPLES')
    101 FORMAT(//,79('-'))
!
  END SUBROUTINE MCMC_DREAM_ALGM
!
!
!-------------------------------------------------------------------------------
!***  Generate candidate parameter samples
!
  SUBROUTINE GEN_CANDIDATE( GEN_NUM, CHAIN_NUM )
!
    IMPLICIT NONE
!
    INTEGER             :: GEN_NUM, CHAIN_NUM
    INTEGER             :: R(2, NPAIR)
    DOUBLE PRECISION    :: NOISE_E(PAR_DIM), B, EPS(PAR_DIM)
    INTEGER             :: I
    INTEGER             :: PAIR(2)
    DOUBLE PRECISION    :: R2(2)
!
    B=1.0D-2  ! USED TO CALCULATE E FOLLOW U(-B,B)
!
!       
    DO I = 1, NPAIR
      DO
        R2(1)=R8_UNIFORM_01_SAMPLE()
        R2(2)=R8_UNIFORM_01_SAMPLE()
    
        PAIR(1:2) = INT ( R2(1:2) * DFLOAT ( NCHAIN ) ) + 1
        
        IF ( PAIR(1) /= PAIR(2) .AND. &
	           PAIR(1) /= CHAIN_NUM .AND. &
	           PAIR(2) /= CHAIN_NUM ) THEN
	  EXIT 
        ENDIF
      ENDDO
      
      R(1:2,I)=PAIR(1:2)
    
    ENDDO       
!
  ! Determine a jump rate and the dimensions to update CR
    CALL CHOOSE_JUMPRATE( GEN_NUM )
!
  ! Calculate e in eq.4 of Vrugt et al. 2009
    DO I =1, PAR_DIM
      NOISE_E(I) = B * (2.0D+00 * R8_UNIFORM_01_SAMPLE() - 1.0D+00)
    ENDDO  
!
  ! Get epsilon value from multinormal distribution
    DO I =1, PAR_DIM
      EPS(I) = R8_NORMAL_SAMPLE(0.0D+00, 1.0D-10)
    ENDDO
!
  ! Generate the candidate sample based on eq.4 of Vrugt et al. 2009
    ZP = Z(:,CHAIN_NUM,GEN_NUM-1) + (1.0D+00 + NOISE_E) * JUMPRATE &
                                  * COMP_DIFF(GEN_NUM, CHAIN_NUM, R) + EPS
!
  ! Check whether candidate sample is out of parameter bound
    CALL OUTOFBOUND( )
!
  END SUBROUTINE GEN_CANDIDATE
!
!
!-------------------------------------------------------------------------------
!*** Out of parameter bound test
!
!  Test whether generated candidate sample is out of the bound set by users in MCMC_Prior input block
!  if it is out of bound then fold the sample into the bound
!
  SUBROUTINE OUTOFBOUND( )
!
    IMPLICIT NONE
!
    INTEGER :: I
!
    DO I = 1, PAR_DIM
      IF( ZP(I) < RANGEMIN(I) )THEN
        ZP(I) = RANGEMAX(I) - RANGEMIN(I) + ZP(I)
      ELSEIF( ZP(I) > RANGEMAX(I) )THEN
        ZP(I) = RANGEMIN(I) - RANGEMAX(I) + ZP(I)
      ENDIF
!
    ! just in case the sample is still outside bound
      IF( ZP(I) < RANGEMIN(I) .OR. ZP(I) > RANGEMAX(I) )THEN
        ZP(I) = RANGEMIN(I) + R8_UNIFORM_01_SAMPLE() * ( RANGEMAX(I) - RANGEMIN(I) )
      ENDIF
    ENDDO
!
  END SUBROUTINE OUTOFBOUND
!
!
!********************* Compute crossover probability CR ************************
!-------------------------------------------------------------------------------
!*** initialize the CR values
!
  SUBROUTINE INIT_CR( )
!
    IMPLICIT NONE
!
    INTEGER :: I
!
    DO I = 1, NCR
      CR(I) = DFLOAT(I) / DFLOAT(NCR)
      PCR(I) = 1.0D0 / DFLOAT(NCR)
      L_CR(I) = 1
      DIS_CR(I) = 1.0D0
    ENDDO
!
    PCR(NCR) = 1.0D0 - SUM(PCR(1:NCR-1))
!
  END SUBROUTINE INIT_CR
!
!
!-------------------------------------------------------------------------------
!***  choose a CR value
!
  SUBROUTINE CHOOSE_CR()
!
    IMPLICIT NONE
!
    INTEGER :: TMP_IND(NCR)
    INTEGER :: I
!
    IF( NCR == 1 ) THEN
      CR_IND = 1
    ELSE
      CALL I4VEC_MULTINOMIAL_SAMPLE(1, PCR(1:NCR), NCR, TMP_IND)
      DO I = 1, NCR
        IF( TMP_IND(I) == 1 ) THEN
          CR_IND = I
          EXIT
        ENDIF
      ENDDO
    ENDIF
!
  END SUBROUTINE CHOOSE_CR
!
!
!-------------------------------------------------------------------------------
!***  update CR distance
!
  SUBROUTINE UPDATE_CR_DIS( GEN_NUM, CHAIN_NUM )
!
    IMPLICIT NONE
!
    INTEGER             :: GEN_NUM, CHAIN_NUM
    DOUBLE PRECISION    :: STD( PAR_DIM )
    INTEGER             :: I
!
  ! COMPUTE STANDARD DEVIATION FOR ALL PARAMETERS
    STD = COMP_STD( GEN_NUM )
    L_CR(CR_IND) = L_CR(CR_IND) + 1
!
    DO I = 1, PAR_DIM
      DIS_CR(CR_IND) = DIS_CR(CR_IND) + ( Z(I, CHAIN_NUM, GEN_NUM ) &
                       - Z(I, CHAIN_NUM, GEN_NUM-1) )**2 / STD(I)**2
    ENDDO
!
  END SUBROUTINE UPDATE_CR_DIS
!
!
!-------------------------------------------------------------------------------
!***  update CR probabilities
!
  SUBROUTINE UPDATE_PCR()
!
    IMPLICIT NONE
!
    INTEGER :: I
!
    DO I = 1, NCR-1
      PCR(I) = (DIS_CR(I)/L_CR(I)) / SUM(DIS_CR(1:NCR) / L_CR(1:NCR))
    ENDDO
!
    PCR(NCR) = 1.0D0 - SUM( PCR(1:NCR-1) )
!
  END SUBROUTINE UPDATE_PCR
!
!
!-------------------------------------------------------------------------------
!***  compute standard deviation
!
  FUNCTION COMP_STD( GEN_NUM )
!
    IMPLICIT NONE
!
    INTEGER             :: GEN_NUM
    DOUBLE PRECISION    :: COMP_STD( PAR_DIM )
    DOUBLE PRECISION    :: MEAN( PAR_DIM )
    INTEGER             :: I
!
    DO I = 1, PAR_DIM
      MEAN(I) = SUM( SUM( Z(I,:,1:GEN_NUM),1 ), 1) / NCHAIN / GEN_NUM
      COMP_STD(I) = SQRT(SUM( SUM( (Z(I,:,1:GEN_NUM) - MEAN(I))**2, 1), 1 ) &
                                                     / (NCHAIN*GEN_NUM-1) )
    ENDDO
!
  END FUNCTION COMP_STD


!************************* Compute jump rate Gamma *****************************
!-------------------------------------------------------------------------------
!***  Initiailize the jump rate table
!
  SUBROUTINE MCMC_INIT_JUMPRATE( )
!
    IMPLICIT NONE
!
    INTEGER :: I
!
    ALLOCATE( JUMP_DIM( PAR_DIM ) )
    ALLOCATE( JUMPRATE_TABLE( PAR_DIM ) )
!
    DO I = 1, PAR_DIM
      JUMPRATE_TABLE(I) = 2.38D0 / SQRT( 2.0D0 * NPAIR * I )
    ENDDO
!
  END SUBROUTINE MCMC_INIT_JUMPRATE
!
!
!-------------------------------------------------------------------------------
!***  Choose a jump rate from the jump rate table
!
  SUBROUTINE CHOOSE_JUMPRATE( GEN_NUM )
!
    IMPLICIT NONE
!
    INTEGER :: I
    INTEGER :: GEN_NUM
!
  ! determine the dimensions that will be updated
    JUMP_NUM = 0
    JUMP_DIM = 0
!
    DO I = 1, PAR_DIM
      IF( R8_UNIFORM_01_SAMPLE( ) > 1.0D+00-CR(CR_IND) )THEN
        JUMP_NUM = JUMP_NUM + 1
        JUMP_DIM(JUMP_NUM) = I
      ENDIF
    ENDDO
!
  ! calculate general jump rate
    IF( JUMP_NUM == 0 )THEN
      JUMPRATE = 0.0D+00
    ELSE
      JUMPRATE = JUMPRATE_TABLE(JUMP_NUM)
    ENDIF
!
  ! if parameter dimension is 1, 2, or 3, fix the jump rate to 0.6
    IF( PAR_DIM <= 3 )THEN
      JUMPRATE = 0.6D+00
    ENDIF
!
  ! determine if do a long jump
    IF( MOD(GEN_NUM-1,JUMPSTEP) == 0 )THEN
      JUMPRATE = 0.98D+00
      RETURN
    ENDIF
!
  END SUBROUTINE CHOOSE_JUMPRATE
!
!
!-------------------------------------------------------------------------------
!***  Calculate the differential evoluation
!
  FUNCTION COMP_DIFF( GEN_NUM, CHAIN_NUM, R )
!
    IMPLICIT NONE
!
    INTEGER             :: GEN_NUM, CHAIN_NUM
    DOUBLE PRECISION    :: COMP_DIFF( PAR_DIM )
    INTEGER             :: R(2, NPAIR)
    INTEGER             :: I, J
!
  ! do differential evolution
    ZP = Z(:,CHAIN_NUM,GEN_NUM-1)
!
  ! produce the difference of the pairs used for population evolution
    COMP_DIFF = 0.0D+00
    DO I = 1, NPAIR
      DO J = 1, JUMP_NUM
        COMP_DIFF(JUMP_DIM(J)) = COMP_DIFF(JUMP_DIM(J)) &
                                 + (Z(JUMP_DIM(J),R(1,I),GEN_NUM-1) &
                                 - Z(JUMP_DIM(J),R(2,I),GEN_NUM-1))
      ENDDO
    ENDDO
!
  END FUNCTION COMP_DIFF
!
!
!*******************************************************************************
!
!               FUNCTIONS ABOUT CONVERGENCE CRETERIA GELMAN-RUBIN R
!
!*******************************************************************************

!-------------------------------------------------------------------------------
!***  Initialize the Gelman Rubin statistic
!
  SUBROUTINE MCMC_INIT_GR()
!
    IMPLICIT NONE
!
    ALLOCATE( GEL_RU_R( PAR_DIM, MAX_GEN ) )
!
    GR_COUNT = 0
    CONV_FLAG = .FALSE.
!
  END SUBROUTINE MCMC_INIT_GR
!
!
!-------------------------------------------------------------------------------
!***  Compute Gelman Rubin statistics R used to check convergence
!
  SUBROUTINE GEL_RU( GEN_NUM )
!
    IMPLICIT NONE
!
    INTEGER             :: GEN_NUM
    DOUBLE PRECISION    :: MEAN_CHAIN( NCHAIN )
    DOUBLE PRECISION    :: MEAN_ALL
    DOUBLE PRECISION    :: S( NCHAIN )
    DOUBLE PRECISION    :: W_VAR, B_VAR, VAR
    INTEGER             :: I, J, IND0
!
    GR_COUNT = GR_COUNT + 1
    IND0 = 0.5D0*GEN_NUM
!
    DO I = 1, PAR_DIM
!
      DO J = 1, NCHAIN
        MEAN_CHAIN(J) = SUM( Z(I,J,IND0:GEN_NUM) ) / DFLOAT(IND0)
      ENDDO
!
      MEAN_ALL = SUM( MEAN_CHAIN ) / NCHAIN
!
      B_VAR = DFLOAT(IND0) / DFLOAT( NCHAIN - 1 ) &
              * SUM( (MEAN_CHAIN - MEAN_ALL)**2 )
!
      DO J = 1, NCHAIN
        S(J) = SUM( (Z(I,J,IND0:GEN_NUM) - MEAN_CHAIN(J))**2 )/(IND0 - 1.0D0)
      ENDDO
!
      W_VAR = SUM(S) / DFLOAT(NCHAIN)
!
      VAR = DFLOAT(IND0-1)/DFLOAT(IND0) * W_VAR + B_VAR / DFLOAT(IND0)
!
      GEL_RU_R( I, GR_COUNT ) = SQRT( VAR / W_VAR )
!
    ENDDO
!
    CONV_FLAG = .TRUE.
!
    DO I = 1, PAR_DIM
      IF( GEL_RU_R( I, GR_COUNT ) > GR_THRESHOLD )THEN
        CONV_FLAG = .FALSE.
        EXIT
      ENDIF
    ENDDO
!
    IF (RESTART_FLAG == .FALSE. .OR. CONV_RESTART == 'F' ) THEN
      IF (CONV_FLAG .EQ. .TRUE.) THEN
        WRITE(OUTUNIT,*) &
        'GELMAN-RUBIN R IS SMALLER THAN USER DEFINED VALUE AT ITERATION: ' &
        ,GEN_NUM
      ENDIF
    ENDIF
!
  END SUBROUTINE GEL_RU
!
!
!*******************************************************************************
!
!                       FUNCTIONS FOR EXPORTING RESULTS
!
!*******************************************************************************

!-------------------------------------------------------------------------------
!***  Write parameter samples into file
!
  SUBROUTINE OUTPUT( IND1 )
!
    IMPLICIT NONE
!
    INTEGER      :: IND1
    INTEGER      :: I, J
    INTEGER      :: IUNIT
    CHARACTER*10 :: IC
!
  ! write parameter samples of all chains
    DO I = 1, NCHAIN
    ! open file
      IF( IND1 == 1 )THEN
        IF(NCHAIN < 100 ) THEN
          WRITE(IC,'(I2.2)') I
        ELSE
          WRITE(IC,'(I3.3)') I
        ENDIF
        IUNIT = UTL_GETUNIT(200,400)
        OPEN(IUNIT,FILE = TRIM(FILE_PREFIX)//'._mcmc_par'//TRIM(IC), &
                                              STATUS='REPLACE')
        WRITE(IUNIT,11)TRIM(IC)
        WRITE(IUNIT,12)( TRIM(PARNAM(J)), J=1, PAR_DIM )
      ELSE
        IF(NCHAIN < 100 ) THEN
          WRITE(IC,'(I2.2)') I
        ELSE
          WRITE(IC,'(I3.3)') I
        ENDIF
        IUNIT = UTL_GETUNIT(200,400)
        OPEN(IUNIT,FILE = TRIM(FILE_PREFIX)//'._mcmc_par'//TRIM(IC),STATUS='OLD', &
                                                        POSITION = 'APPEND')
      ENDIF
!
    ! write loglikelihood function and parameter samples
      WRITE(IUNIT,10)IND1, FIT(I,IND1), Z(:,I,IND1)
!
    ! close file
      CLOSE(IUNIT)    
    ENDDO
!
  ! write gelman-rubin r
    IF ( (RESTART_FLAG == .FALSE. .OR. CONV_RESTART == 'F') ) THEN
    ! open file
      IF( ( PRINTSTEP == 1 .AND. IND1 == 4 ) .OR. &
        ( PRINTSTEP == 2 .AND. IND1 == 4 ) .OR. &
        ( PRINTSTEP == 3 .AND. IND1 == 6 ) .OR. &
        ( PRINTSTEP > 3 .AND. IND1 == PRINTSTEP) ) THEN
        IUNIT = UTL_GETUNIT(200,400)
        OPEN(IUNIT, FILE = TRIM(FILE_PREFIX)//'._mcmc_grr',STATUS='REPLACE')
        WRITE(IUNIT,21)
        WRITE(IUNIT,22)( TRIM(PARNAM(J)), J=1, PAR_DIM )
!
      ! write gelman-rubin statistic R
        WRITE(IUNIT,20) IND1, GEL_RU_R(:,GR_COUNT)
!
      ! close file
        CLOSE(IUNIT)
!
      ELSEIF( ( PRINTSTEP == 1 .AND. IND1 > 4 ) .OR. &
            ( PRINTSTEP == 2 .AND. IND1 > 4 ) .OR. &
            ( PRINTSTEP == 3 .AND. IND1 > 6 ) .OR. &
            ( PRINTSTEP > 3 .AND. IND1 > PRINTSTEP) ) THEN
        OPEN( IUNIT, FILE = TRIM(FILE_PREFIX)//'._mcmc_grr', STATUS='OLD', &
                                                     POSITION = 'APPEND')
!
      ! write gelman-rubin statistic R
        WRITE(IUNIT,20) IND1, GEL_RU_R(:,GR_COUNT)
!
      ! close file
        CLOSE(IUNIT)
!
      ENDIF
    ENDIF
!
    11 FORMAT(1X,'"MONITORED PARAMETER SAMPLE VALUES AND ASSOCIATED &
                   LOG LIKELIHOOD FUNCTION VALUES FOR CHAIN # ',A,'"')
    12 FORMAT(1X,'"Iteration"',3X,'"LogLikelihood"',4X,1000('"',A12,'"',2X,:))
    21 FORMAT(1X,'"MONITORED PARAMETER INTER-CHAINS GELMAN RUBIN R STATISTIC"')
    22 FORMAT(1X,'"Iteration"',5X,1000('"',A12,'"',2X,:))
    10 FORMAT(1X,I7,6X,ES14.7,6X,1000(ES14.7,2X))
    20 FORMAT(1X,I9,6X,1000(F14.4,2X))
!
  END SUBROUTINE OUTPUT
!
!
!-------------------------------------------------------------------------------
!***  Write model outputs for parameter samples into file _mcmc_sim
!
  SUBROUTINE OUTPUT_SIM( IND )
!
    IMPLICIT NONE
!
    INTEGER      :: IND
    INTEGER      :: I,J
    INTEGER      :: IUNIT
    CHARACTER*10 :: IC
!
  ! write parameter samples of all chains
    DO I = 1, NCHAIN
    ! open file
      IF( IND == 1 )THEN
        IF(NCHAIN < 100 ) THEN
          WRITE(IC,'(I2.2)') I
        ELSE
          WRITE(IC,'(I3.3)') I
        ENDIF
        IUNIT = UTL_GETUNIT(200,400)
        OPEN(IUNIT,FILE = TRIM(FILE_PREFIX)//'._mcmc_sim'//TRIM(IC), &
                                              STATUS='REPLACE')
        WRITE(IUNIT,11)TRIM(IC)
        WRITE(IUNIT,12)( TRIM(MC_OBSNAM(J)), J=1, MC_NOBS )
      ELSE
        IF(NCHAIN < 100 ) THEN
          WRITE(IC,'(I2.2)') I
        ELSE
          WRITE(IC,'(I3.3)') I
        ENDIF
        IUNIT = UTL_GETUNIT(200,400)
        OPEN(IUNIT,FILE = TRIM(FILE_PREFIX)//'._mcmc_sim'//TRIM(IC),STATUS='OLD', &
                                                        POSITION = 'APPEND')
      ENDIF
!
    ! write loglikelihood function and parameter samples
      WRITE(IUNIT,10)IND,FIT_SIM(I,IND),PRINT_SIM(:,I,IND)
!
    ! close file
      CLOSE(IUNIT)    
    ENDDO
!
    11 FORMAT(1X,'"MODEL OUTPUTS FOR PARAMETER SAMPLE FOR CHAIN # ',A,'"')
    12 FORMAT(1X,'"Iteration"',3X,'"LogLikelihood"',4X,1000('"',A12,'"',2X,:))
    10 FORMAT(1X,I7,6X,ES14.7,6X,1000(ES14.7,2X))
!
  END SUBROUTINE OUTPUT_SIM
!
!
!-------------------------------------------------------------------------------
!***  Write the last parameter samples into restart file used for the next time simulation
!
  SUBROUTINE MCMC_OUTPUTRESTART( )
!
    IMPLICIT NONE
!
    INTEGER :: IUNIT, I, J
!
    IUNIT = UTL_GETUNIT(200,400)
    OPEN( IUNIT, FILE = TRIM(FILE_PREFIX)//'._mcmc_restart',STATUS='REPLACE' )
    WRITE(IUNIT,11)
    WRITE(IUNIT,12)( TRIM(PARNAM(J)), J=1, PAR_DIM )
    DO I = 1, NCHAIN
      WRITE(IUNIT,10)I, FIT(I, MAX_GEN), Z(:,I,MAX_GEN)
    ENDDO
    IF (CONV_FLAG .EQ. .TRUE.)THEN
      WRITE(IUNIT,13) 'T'
    ELSE
      WRITE(IUNIT,13) 'F'
    ENDIF
    CLOSE(IUNIT)

    10 FORMAT(1X,I7,7X,ES14.7,6X,1000(ES14.7,2X))
    12 FORMAT(1X,'"Chain #"',5X,'"LogLikelihood"',5X,1000('"',A12,'"',2X,:))
    11 FORMAT(1X,'"PARAMETER VALUES OF THE LAST GENERATION FOR &
                  ALL CHAINS FOR USE IN RESTARTING THE CHAINS"')
    13 FORMAT(1X,'Converged: ',A1)
!
  END SUBROUTINE MCMC_OUTPUTRESTART
!
!
!*******************************************************************************
!
!                       FUNCTIONS TO DETECT OUTLIER CHAIN
!
!*******************************************************************************


!-------------------------------------------------------------------------------
! ******* Test outlier chain in burn-in period
!
  SUBROUTINE OUTLIERTEST( GEN_NUM )
!
    IMPLICIT NONE
!
    INTEGER :: GEN_NUM
    DOUBLE PRECISION    :: AVG(NCHAIN)
    DOUBLE PRECISION    :: AVG_TMP(NCHAIN)
    INTEGER :: I
    INTEGER :: IND1, IND2
    DOUBLE PRECISION    :: IQR, Q1, Q3
    INTEGER :: BEST(1)
!
    DO I = 1,NCHAIN
      AVG(I) = SUM(FIT(I, GEN_NUM/2:GEN_NUM))/SIZE(FIT(I, GEN_NUM/2:GEN_NUM))
    ENDDO
!
    BEST = MAXLOC( AVG )
    AVG_TMP = AVG
!
    CALL SORT( NCHAIN, AVG_TMP )
!
    IND1 = NCHAIN * 0.25D+00 + 1
    IND2 = NCHAIN * 0.75D+00 + 1
!
    Q1  = AVG_TMP(IND1)
    Q3  = AVG_TMP(IND2)
    IQR = Q3 - Q1
!
    DO I = 1, NCHAIN
      IF( AVG(I) < Q1 - 2.0D+00 * IQR )THEN
        Z(:,I,GEN_NUM) = Z(:,BEST(1),GEN_NUM)
        FIT(I, GEN_NUM/2:GEN_NUM) = FIT(BEST(1), GEN_NUM/2:GEN_NUM)
        WRITE(OUTUNIT,201)I,GEN_NUM,BEST(1)
      ENDIF
    ENDDO
!
    201 FORMAT(2X,'Chain ',I2,' is an outlier chain at iteration ',I10, / &
              '  its samples at this iteration are replaced by those from &
              the chain ',I2,' with the largest log likelihood function.'/)
!
  END SUBROUTINE OUTLIERTEST

!-------------------------------------------------------------------------------
! sort function used by subroutine outliertest
!
  SUBROUTINE SORT(N,A)
!
    IMPLICIT NONE
!
    INTEGER :: I, J
    INTEGER :: N
    DOUBLE PRECISION  :: A(N), TMP
!
    DO I = 1, N-1
      DO J = I+1, N
        IF( A(I) > A(J) )THEN
          TMP = A(I)
          A(I) = A(J)
          A(J) = TMP
        ENDIF
      ENDDO
    ENDDO
!
  END SUBROUTINE SORT
!
!
!*******************************************************************************
!
!                       DEALLOCATE VARIABLES
!
!*******************************************************************************
!
  SUBROUTINE MCMC_CLN()
!
    IMPLICIT NONE
!
    IF (ALLOCATED(PARGPNAM)) DEALLOCATE(PARGPNAM)
    IF (ALLOCATED(PARNAM)) DEALLOCATE(PARNAM)
    IF (ALLOCATED(MC_PARNAMLC)) DEALLOCATE(MC_PARNAMLC)
    IF (ALLOCATED(PARGP)) DEALLOCATE(PARGP)
    IF (ALLOCATED(MATRIXNAM)) DEALLOCATE(MATRIXNAM)
    IF (ALLOCATED(RANGEMIN)) DEALLOCATE(RANGEMIN)
    IF (ALLOCATED(RANGEMAX)) DEALLOCATE(RANGEMAX)
    IF (ALLOCATED(PDF_TYPE)) DEALLOCATE(PDF_TYPE)
    IF (ALLOCATED(PDF_GPTYPE)) DEALLOCATE(PDF_GPTYPE)
    IF (ALLOCATED(PDF_VAR1)) DEALLOCATE(PDF_VAR1)
    IF (ALLOCATED(PDF_VAR2)) DEALLOCATE(PDF_VAR2)
    IF (ALLOCATED(MC_NW)) DEALLOCATE(MC_NW)
    IF (ALLOCATED(MC_OBSDEROBSNUM)) DEALLOCATE(MC_OBSDEROBSNUM)
    IF (ALLOCATED(MC_OBSEXTOBSNUM)) DEALLOCATE(MC_OBSEXTOBSNUM)
    IF (ALLOCATED(MC_OBSEXTNAM)) DEALLOCATE(MC_OBSEXTNAM)
    IF (ALLOCATED(MC_MCVUSE)) DEALLOCATE(MC_MCVUSE)
    IF (ALLOCATED(MC_OBSNAM)) DEALLOCATE(MC_OBSNAM)
    IF (ALLOCATED(MC_MODEXTVAL)) DEALLOCATE(MC_MODEXTVAL)
    IF (ALLOCATED(MC_MODDERVAL)) DEALLOCATE(MC_MODDERVAL)
    IF (ALLOCATED(MC_DEPDERVALSETS)) DEALLOCATE(MC_DEPDERVALSETS)
    IF (ALLOCATED(MC_DEPEXTVALSETS)) DEALLOCATE(MC_DEPEXTVALSETS)
    IF (ALLOCATED(MC_MODELVAL)) DEALLOCATE(MC_MODELVAL)
!
    IF (ALLOCATED(MC_CATGOR)) DEALLOCATE(MC_CATGOR)
    IF (ALLOCATED(MC_CINSTSET)) DEALLOCATE(MC_CINSTSET)
    IF (ALLOCATED(MC_MRKDL)) DEALLOCATE(MC_MRKDL)
    IF (ALLOCATED(MC_INSTRUCTFILE)) DEALLOCATE(MC_INSTRUCTFILE)
    IF (ALLOCATED(MC_LOCINS)) DEALLOCATE(MC_LOCINS)
    IF (ALLOCATED(MC_MIFILE)) DEALLOCATE(MC_MIFILE)
    IF (ALLOCATED(MC_MOFILE)) DEALLOCATE(MC_MOFILE)
    IF (ALLOCATED(MC_TFILE)) DEALLOCATE(MC_TFILE)
!
    IF (ALLOCATED(GEL_RU_R)) DEALLOCATE(GEL_RU_R)
    IF (ALLOCATED(JUMPRATE_TABLE)) DEALLOCATE(JUMPRATE_TABLE)
    IF (ALLOCATED(JUMP_DIM)) DEALLOCATE(JUMP_DIM)
    IF (ALLOCATED(Z)) DEALLOCATE(Z)
    IF (ALLOCATED(FIT)) DEALLOCATE(FIT)
    IF (ALLOCATED(ZP)) DEALLOCATE(ZP)
    IF (ALLOCATED(MNOR_MEAN)) DEALLOCATE(MNOR_MEAN)
    IF (ALLOCATED(COV_MAT)) DEALLOCATE(COV_MAT)
    IF (ALLOCATED(PARM)) DEALLOCATE(PARM)
!
  END SUBROUTINE MCMC_CLN
!
!
END MODULE
