PROGRAM UCODE
  !Utilizes Jupiter API for universal inversion via Modified Gauss Newton Method
  USE DATATYPES
  !
  USE GLOBAL_DATA, ONLY: AMESSAGE, BIGDOUBLE, ERRSUB, IVERB, MAX_STRING_LEN, &
                         VERSIONID
  !
  USE BASIC, ONLY: &
    BAS_INI_COVMAT, BAS_INI_GETOPTIONS, BAS_INI_MODELEXEC, &
    BAS_GEN, BAS_EXE, BAS_EXE_SELECT, BAS_CLN, &
    ! variables
    COVMATARR, DERIV_INTERFACE, FORSENS
  !
  USE DEPENDENTS, ONLY: &
    ! subroutines &
    DEP_INI_READ, DEP_INI_STORE, DEP_INI_ALLOC, DEP_INI_NAMCHK, &
    DEP_DX_WRITE_GM, DEP_EXT_DER, DEP_UEV_RESIDUALS, DEP_CLN, &
    ! variables
    MODEXTVAL, MODDERVAL, NONDETVAL, OBSEXTNAM, OBSDEROBSNUM, OBSEXTOBSNUM, &
    OBSVAL, WTCORRELATED, WTCOSUSE, WTCOS
  !
  USE EQUATION, ONLY: EQN_INI, EQN_CLN
  !
  USE MODEL_IO, ONLY: &
    ! subroutines
    MIO_INI_ALLOC, MIO_INI_ARRAYS, MIO_INI_DIMENSION, MIO_INI_INPUTFILES, &
    MIO_INI_OUTPUTFILES, MIO_INI_INSTRUCT1, MIO_INI_INSTRUCTALLOC, &
    MIO_INI_INSTRUCT2, MIO_INI_TEMPLATE, MIO_ADA_WRITEFILES, MIO_EXT
  !
  USE PRIOR_INFORMATION, ONLY: &
    ! subroutines
    PRI_INI_ALLOC, PRI_INI_INSTALL, PRI_INI_READ_WPRED,  PRI_INI_PROCESS, &
    PRI_INI_POPX, PRI_INI_STORE_WPRED, PRI_UEV_RESIDUALS, &
    PRI_DX_WRITE_GM, PRI_CLN, &
    ! variables
    PRINAM, PRIVAL, PRIWTMAT, PRIWTMATSQR, &
    MODELPRIVAL, RESIDSPRI, WTDRESIDSPRI
  !
  USE SENSITIVITY, ONLY: &
    ! subroutines
    SEN_INI, SEN_DEF, SEN_EXE_SELECT, SEN_GEN, SEN_UEV_LNX,  &
    SEN_UEV_POPX_MODCALC, SEN_UEV_POPXROW_DIFF, SEN_CLN
  !
  USE STATISTICS, ONLY: &
    ! subroutines
    STA_INI, STA_CLN
  !
  USE UTILITIES
  !
  USE UTLUCODE
  ! Application specific modules  !
  USE UCODEMOD, ONLY: &
    ! subroutines
    UCODE_INI, UCODE_INI_ALLOC, UCODE_INI_CHECK_USEFLAG, &
    UCODE_INI_FILEMANAGE, UCODE_INI_PARNAMALL, UCODE_INI_PARS, &
    UCODE_INI_PARS_INSTALL, UCODE_INI_SET, UCODE_INI_PARWRITE, &
    UCODE_INI_SVD, UCODE_FINAL_SVD, &
    UCODE_DEF, &
    UCODE_GEN_INITRESSEN, UCODE_GEN_LINEARITY, UCODE_GEN_LINEARITY_ADV, &
    UCODE_GEN_DERPARWRITE, &
    UCODE_GEN_PREDICT, UCODE_GEN_PVALALL,  UCODE_GEN_SOSSURF,   &
    UCODE_GEN_SVDCALCPAR, UCODE_GEN_SVDPOPX, &
    UCODE_EXE_SELECT,   &
    UCODE_UEV_CALCWT, UCODE_UEV_CALCINTWT, UCODE_UEV_CSS, &
    UCODE_UEV_LINEARITY, UCODE_UEV_LINEARITY_ADV, UCODE_UEV_OMIT, &
    UCODE_UEV_RESIDUALS, UCODE_UEV_PRINT_PREDICT, &
    UCODE_UEV_RESID_PRINT_DECISION, UCODE_UEV_MERGE, &
    UCODE_UEV_SENS_PRINT_DECISION, &
    UCODE_EVA, UCODE_EVA_LOWEST, UCODE_CLN, &
    ! variables
    BSCAL, CONSTRAIN, CONSTRAINL, CONSTRAINLNOT, CSS, DATAEXCHANGE, &
    LINEARITY, LINEARITYADV, ICONVERT_DERIV, IOMIT, IPTR, &
    ISENANY, ISENCOMB, ISENMETHOD, ISENPERTCNT, ISENPERTFIN, &
    ITRANS, IWRITE, LN, LOWEST, &
    LOWEST_DIFFERS, MAG_PERTURB, MCVCAT, MCVUSE, MODELVAL, &
    NCATS, NCOVMAT, NMERGE, NOBS,  &
    NONLININT, NONLINPARAM, CREATEINITFILES, &
    NOPNT, NPD, NPE, NPEFP, NPENOW, NPO, NPS, NPSNOW, NPSWOP, NPSNPD, &
    NPERTURB_CYCLES, NPERTURB_CYCLES_FINAL, NW, &
    OBSNAM, OPT, OPTIMIZE, OPTNLUNC, &
    PADJ, PARAM_OMIT, PARAMS_OMITTED, PARNAM, PARNAMALL, PARNAMLC, &
    PERTURB, PMAXCHANGE, POMIT, PRECIS, &
    PREDICT, PRED_MODADV, PTOLPAR, &
    PVAL, PVALINIT, PVALMAXC, PVALMINC, PVALSVD, &
    REACT, RESIDS, RESIDONLY, RSQALL, &
    SAVEWTFINAL, SENFORINIT, SENSITIVITIES, &
    SOSSURFACE, STDERRONE, SVDT, SVDPHASE1, &
    WRPAREQ, WRPRIEQ, WTDRESIDS, WTFULL, WTFULLSQR, XPRI, XPRIFILLED, XSENST, &
    IPTRTMP, PARNAMTMP, &
    MCMC, MCMC_PREDICTION
  !
  USE REG_GN_UCODE, ONLY: &
    REG_GN_INI_ALLOC, REG_GN_INI, REG_GN_INI2, REG_GN_INIUNC, &
    REG_GN_GEN, REG_GN_UNC, &
    ! variables
    AVET, STATS_ON_NONCONVERGE, ITERP, MAXITER, MAXCHANGE, &
    OMIT_INSENS, TOLPAR, TOLSOSC, WTADJFINAL, &
    FL, RESIDSUNC, WTFULLUNC, XSENSTUNC, ZZ
  !
  USE REG_TR_UCODE, ONLY: REG_TR_GEN
  !
  USE PARALLEL_PROCESSING, ONLY: PLL_INI_DISPATCHER, PLL_MAKE_RUNS, PLL_CLN, &
    PLL_STOP_RUNNERS
  ! MCMC simulation specific modules  !

  USE DREAM, ONLY: MCMC_INIT_MODEL, MCMC_INI, MCMC_INI_PRIOR, MCMC_PRIOR_STORE, &
    MCMC_INIT_CHAIN, MCMC_INIT_JUMPRATE, MCMC_INIT_GR, &
    MCMC_DREAM_ALGM, MCMC_OUTPUTRESTART, MCMC_PREDICTION_RUN, MCMC_CLN, &
   !variables
    MC_NPS,MC_PARNAMLC, MULTINORMAL_FLAG
  !
  IMPLICIT NONE
  !   Fortran parameters
  INTEGER JOBDIM, JOBLEN
  PARAMETER (JOBDIM=10, JOBLEN=20)
  !
  ! Main UCODE & Jupiter API variables
  CHARACTER(LEN=6),   ALLOCATABLE, DIMENSION(:)            :: CATGOR    ! Model-calculated value category for each model-output file
  CHARACTER(LEN=10)                                        :: CHDATE
  DOUBLE PRECISION                                         :: CHECKTOLSOSC
  DOUBLE PRECISION                                         :: CHECKTOLSOSC1
  CHARACTER(LEN=10)                                        :: CHTIME
  CHARACTER(LEN=10)                                        :: CHZONE
  CHARACTER(LEN=1),   ALLOCATABLE, DIMENSION(:)            :: CINSTSET  ! holds compressed instruction set
  CHARACTER (LEN=MAX_STRING_LEN)                           :: COMMANDPLL
  LOGICAL                                                  :: CONVERT_STAT=.TRUE.
  LOGICAL, ALLOCATABLE, DIMENSION(:)                       :: CONVERTED
  LOGICAL                                                  :: CTRLDONE    ! TRUE when control loop is done
  CHARACTER(LEN=JOBLEN), DIMENSION(JOBDIM)                 :: CTRLJOB     ! Sequence of jobs required of Control loop
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)            :: DEPVALSETS
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)            :: DEPDERVALSETS
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)            :: DEPEXTVALSETS
  LOGICAL                                                  :: DO_PARALLEL      ! True when model runs are to be made in parallel
  DOUBLE PRECISION                                         :: DTPRIWT = 0.D0   ! Log Determinant of weight matrix prior
  DOUBLE PRECISION                                         :: DTWT = 0.D0      ! Log Determinant of weight matrix dependents
  DOUBLE PRECISION                                         :: DTWTINIT = 0.D0
  INTEGER, ALLOCATABLE, DIMENSION(:,:)                     :: EQTN
  CHARACTER(LEN=MAX_STRING_LEN)                            :: FILENAME    ! Input data file name
  LOGICAL                                                  :: FINALSTATS=.FALSE. !TRUE regression done & final statis are calculated
  LOGICAL                                                  :: FINALSTATSDONE  ! TRUE when final statistics have been calculated
  CHARACTER(LEN=MAX_STRING_LEN)                            :: FNTEMP      ! Temporarily holds a file name
  LOGICAL                                                  :: GNUDONE     ! TRUE when Gauss-Newton iterating is done
  INTEGER                                                  :: I           ! counter
  INTEGER                                                  :: IBDT(8)
  INTEGER                                                  :: ICNT = 1
  INTEGER                                                  :: ISN
  INTEGER                                                  :: ICTRL       ! Control-loop job pointer
  INTEGER                                                  :: ICOMMAND    ! Number of model command lines to execute
  INTEGER                                                  :: IDEPTYPE
  INTEGER                                                  :: IEND        ! flag indicating type of close and required printing
  INTEGER                                                  :: IEQNPTR(2)
  INTEGER                                                  :: IFO         ! flag indicating convergence
  INTEGER                                                  :: IFAIL       ! Status: 0 for success
  INTEGER                                                  :: IFAILHOLD
  INTEGER                                                  :: IINT = 0
  INTEGER                                                  :: IND = 0     ! flag indicating singular matrix
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: INSTRUCTFILE
  CHARACTER(LEN=MAX_STRING_LEN)                            :: INSTRUCTION ! extraction direction
  LOGICAL                                                  :: INTDONE = .FALSE.
  INTEGER                                                  :: INTERVALLOOPS
  INTEGER                                                  :: INUNIT = 9  ! Unit number for main input file
  INTEGER                                                  :: IOUT        ! Unit number of main output file
  INTEGER                                                  :: IOUTTEMP    ! Unit number of initial temporary main output file
  INTEGER                                                  :: ISENSTAT =0 ! If == NPE and sens=T and Optimze=F then print full stats
  INTEGER                                                  :: ISENPERT = 0
  INTEGER                                                  :: ISNPRT
  INTEGER                                                  :: ISTAT       ! Status: 0 for success
  INTEGER                                                  :: IUGM
  INTEGER                                                  :: J
  INTEGER                                                  :: KLOOP      ! Counter for iterations of potentially parallel outer loop
  INTEGER                                                  :: KLPTR
  INTEGER,            ALLOCATABLE, DIMENSION(:)            :: KPE
  INTEGER                                                  :: KPI
  INTEGER                                                  :: KPPL      ! Counter for iterations of potentially parallel inner loops
  INTEGER                                                  :: KPTR
  INTEGER                                                  :: LCIS           ! size of CINSTSET array
  LOGICAL                                                  :: LINEARITYDONE  ! TRUE when 2*NPE or 2*NOINT forward loops are complete
  INTEGER                                                  :: LINEARITYLOOPS  ! NUMBER OF FORWARD RUNS USING LINEARITY PARAMETERS
  INTEGER,            ALLOCATABLE, DIMENSION(:)            :: LOCINS    ! pointer to instructions
  INTEGER                                                  :: MAXRUNSPLL = 1
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: MIFILE    ! Names of model-input files
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: MOFILE    ! Names of model-output files
  INTEGER                                                  :: MPR
  INTEGER                                                  :: MPRWOP
  CHARACTER(LEN=1),   ALLOCATABLE, DIMENSION(:)            :: MRKDL
  INTEGER                                                  :: NDOBS ! Number of derived observations
  INTEGER                                                  :: NEOBS ! Number of extracted observations
  INTEGER                                                  :: NINSTRUCT      ! size of LOCINS array
  INTEGER                                                  :: NMIFILE        ! Number of model-input files
  INTEGER                                                  :: NMOFILE        ! Number of model-output files
  LOGICAL                                                  :: NONDETECTL=.FALSE.
  LOGICAL                                                  :: NORMAL = .TRUE.
  INTEGER                                                  :: NPETMP
  INTEGER                                                  :: NPSTMP
  INTEGER                                                  :: NRUNSPLL = 1
  INTEGER                                                  :: NTOBS ! Total number of observations
  INTEGER                                                  :: NUMLADV
  INTEGER                                                  :: NUMLOOP     ! Number of iterations of potentially parallel outer loop
  INTEGER                                                  :: NUMMODRUNS = 0 ! Number of executions of the process model
  INTEGER                                                  :: NUMFINSTAT = 0 ! Number of executions of the process model for final stats
  INTEGER                                                  :: NUMFINSTAT2 = 0 ! Number of executions of the process model for final stats with svd
  INTEGER                                                  :: NUMFINSTAT3 = 0 ! Number of executions of the process model for final stats after svd
  INTEGER                                                  :: NUMSVDRUNS = 0 ! Number of executions of the process model for SVD optimization
  INTEGER                                                  :: NUMPPL      ! Number of iterations of potentially parallel loop
  INTEGER, ALLOCATABLE, DIMENSION(:)                       :: OMITPRI
  INTEGER, ALLOCATABLE, DIMENSION(:)                       :: OPINC
  CHARACTER(LEN=MAX_STRING_LEN)                            :: OUTNAM      ! root name for output files
  CHARACTER(LEN=MAX_STRING_LEN)                            :: OUTNAMPRED    ! root name for pred output files and subsequent analyses
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)            :: PARADAVALSETS
  LOGICAL                                                  :: PARALLEL_ACTIVE  ! True parallel=true in PARALLEL_CONTROL input block
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)            :: PARVALSETS
  INTEGER, ALLOCATABLE,  DIMENSION(:)                      :: PINCR
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)             :: PNAM
  DOUBLE PRECISION,   ALLOCATABLE, DIMENSION(:)            :: PVALTMP
  LOGICAL                                                  :: SENSDONE    ! TRUE when Sentivitiy calculation is donedone
  INTEGER                                                  :: SVDCNT = 1
  INTEGER                                                  :: SVDSTART = 1
  INTEGER                                                  :: SVDLOOP
  INTEGER                                                  :: SUMOMITPRI
  LOGICAL                                                  :: TF = .FALSE.
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: TFILE    ! Names of template files
  LOGICAL                                                  :: TRUSTREGION ! trust region method used when not equal to 'FALSE'
  LOGICAL                                                  :: UNUSUAL = .FALSE.
  DOUBLE PRECISION                                         :: VERSION
  CHARACTER(LEN=20)                                        :: VERSIONMIN
  CHARACTER(LEN=5)                                         :: VERSIONTMP
  TYPE (CDMATRIX)                                          :: WTINIT
  TYPE (CDMATRIX)                                          :: WTINITSQR
  LOGICAL                                                  :: WTOSL = .FALSE.

  !
  !
  DATA CTRLJOB /'FORWARD','FORWARD&SENS','SENSITIVITY', &
                'GAUSS-NEWTON','STOP',5*' '/
  !
  !   Format statements
   6 FORMAT &
     (//,' CRITERIA TO STOP UPDATING WEIGHTS WAS MET AT ITERATION# ',I4,//)
  10 FORMAT(//,80('*'),/,80('*'),/,1X,'Executing UCODE_2014,  Version: ',F7.3, &
           A5,/,1X,'Constructed using the JUPITER API, Version: ',A14,/,80('*'))
  15 FORMAT(//,1X,78('*'),/,1X,78('*'), &
            //,15X,'Output from program UCODE_2014, Version: ',F7.3,A5, &
            //,11X,'   Constructed using the JUPITER API, Version:  ',A14, &
            //,1X,78('*'),/,1X,78('*'))
  20 FORMAT(1X,'File open failed for ',A,' status = ',I5, &
     /,' Check command line that runs UCODE, format is: ', &
     /,' PATH_TO_UCODE\ucode.exe input_file_name root_for_output_files ')
  30 FORMAT(80('-'),/)
  80 FORMAT(/,1X,'Reading input from file: ',A)
  90 FORMAT(//,1X,' PERTURBING:',/,1X,'  PARAMETER       ',A12,/,1X, &
            '  VALUE           ',1PE20.11,/,1X,'  PERTURBED VALUE ',1PE20.11,//)
  100 FORMAT(/,1X,'SOME OBSERVATIONS INVOLVE NON-DETECTS',/, &
               1X,'    POSITIVE AND NEGATIVE RESIDUALS AND',/, &
               1X,'    RUNS STATISTIC WILL NOT BE REPORTED')
  200 FORMAT(/,1X,'Error in instruction: "',A,'"')
  300 FORMAT(/,80('*'),/,1X,'END ECHO OF INPUT - REPORT RESULTS OF SIMULATION',&
             /,80('*'),/)
  700 FORMAT(//,80('*'),/,80('*'),//)
  701 FORMAT(4X, &
    ' OPTIMIZATION USING SVD PARAMETERS WILL CONTINUE FROM THE',/, &
    '                   SENSITIVITY ANALYSIS IN THIS UCODE RUN')
  702 FORMAT(4X, &
    ' OPTIMIZATION USING SVD PARAMETERS WILL CONTINUE FROM A',/, &
    '           SENSITIVITY ANALYSIS IN A PREVIOUS UCODE RUN')
  885 FORMAT &
      (/,1X,'NUMBER OF PROCESS MODEL RUNS CONDUCTED FOR THE',/,34X, &
       'PROCESS MODEL PARAMETER REGRESSION:',I8)
  886 FORMAT &
      (1X,'NUMBER OF PROCESS MODEL RUNS CONDUCTED TO CALCULATE',/,13X, &
       'FINAL STATISTICS FOR PROCESS MODEL PARAMETER REGRESSION:',I8)
  887 FORMAT &
      (1X,'NUMBER OF PROCESS MODEL RUNS CONDUCTED TO CALCULATE',/,21X, &
       'FINAL STATISTICS AFTER SVD PARAMETER REGRESSION:',I8)
  888 FORMAT &
      (/,1X,'NUMBER OF PROCESS MODEL RUNS CONDUCTED FOR THE',/,44X, &
       'SVD PARAMETER REGRESSION:',I8)
  889 FORMAT(/,6X, &
      'TOTAL NUMBER OF PROCESS MODEL RUNS CONDUCTED FOR THIS ANALYSIS:',I8)
  890 FORMAT &
           (/,2X,'NUMBER OF PROCESS MODEL RUNS CONDUCTED FOR THIS ANALYSIS:',I8)
  891 FORMAT(/,8X, &
     'Model runs for final statistics are needed to obtain accurate',/,8X, &
     'sensitivities for the final parameter values. If sensitivities',/,8X, &
     'are caluclated by perturbation, then central differences are used',/,8X, &
     'to obtain accurate sensitivities for the statistical evaluation',/,8X, &
     'of the optimized model. Each central difference calculation',/,8X, &
     'requires two process model runs.')
  892 FORMAT(/, &
    8X,'At least one parameter has been assigned SENMETHOD = 2 indicating',/, &
    8X,'use of central differences which are generally not required to',/, &
    8X,'achieve a successful regression. For future optimizations consider',/, &
    8X,'using SENMETHOD = 1 to reduce the number of process model runs',/, &
    8X,'thus reducing the time required to complete the parameter estimation.')
  898 FORMAT(/,80('!'),//, &
             30X,'WARNING: ',//, &
             '  PARAMETERS WERE OMITTED FROM THE RGERESSION,',//,80('!'))
  899 FORMAT(/,80('!'),//, &
             30X,'WARNING: ',//, &
             '  PARAMETERS WERE OMITTED AND REACTIVATE = NO,',/, &
             '  SO THE NUMBER OF PARAMETERS INCLUDED IN CALCULATION OF THE', &
             '  FINAL STATISTICS',/,'  IS LESS THAN THE INITIAL NUMBER OF', &
             '  ADJUSTABLE PARAMETERS',//,80('!'))
  900 FORMAT(/,80('*'),/,15X,'Normal termination of UCODE_2014,   Version: ', &
             F7.3,/,80('*'),//)
  901 FORMAT(/,80('!'),//, &
             30X,'WARNING: ',//, &
             15X,'UNSATISFACTORY TERMINATION OF UCODE_2014,   Version: ',F7.3, &
             //, &
             '  Search #uout file for the word WARNING to see suggestions',//, &
             /,80('!'),//)
  902 FORMAT(/,80('!'),//, &
             2X,'WARNING: TERMINATION OF UCODE_2014, Version: ',F7.3,//, &
             '  Search #uout file for the word WARNING to see suggestions',//, &
             80('!'),//)
  910 FORMAT(/,80('!'),//,13X,'ADJUSTABLE is NO or FALSE for ALL parameters')
  911 FORMAT(13X,'Set ADJUSTABLE to YES or TRUE for AT LEAST ONE PARAMETER', &
             //,80('!'),//)
  !
  VERSION = 1.000  !  2014
  VERSIONTMP = ' '
  VERSIONMIN = '1.7.3'
  ! Get start time
  CALL DATE_AND_TIME(CHDATE,CHTIME,CHZONE,IBDT)
  !
  ! ****************************************************************************
  ! ****************************** INITIALIZE **********************************
  ! ****************************************************************************
  !
  CALL TYP_NULL(WTINIT)
  CALL TYP_NULL(WTINITSQR)
  WRITE(*,10)VERSION,VERSIONTMP,VERSIONID
  IFAIL = 0   ! Error flag, 0 = no error
  IEND = 0
  PARALLEL_ACTIVE = .FALSE.
  !
  ! Command line should include 1) InputFileName 2) RootNameForOutputFiles
!write(*,*)' enter in file' ! only for debugging
!read(*,*) FILENAME ! only for debugging
  FILENAME = UTL_GETARG(1) ! Get command-line arg for input file
  IF(FILENAME .EQ. ' ') THEN
    WRITE(*,*)'!!!! ERROR MISSING INPUT FILENAME ON UCODE COMMAND LINE !!!'
    CALL UTL_STOP &
    (' PLEASE INCLUDE A INPUT FILENAME on the UCODE COMMAND LINE')
  ENDIF
!write(*,*)' enter root' ! only for debugging
!read(*,*) OUTNAM ! only for debugging
  OUTNAM = UTL_GETARG(2)   ! Get command-line arg for rootname of output files
  IF(OUTNAM .EQ. ' ') THEN
    WRITE(*,*)'!!!! ERROR MISSING ROOTFILENAME ON UCODE COMMAND LINE !!!'
    CALL UTL_STOP &
    (' PLEASE INCLUDE A ROOTFILENAME on the UCODE COMMAND LINE')
  ENDIF
!write(*,*)' enter predroot' ! only for debugging
!read(*,*) OUTNAMPRED ! only for debugging
  OUTNAMPRED = UTL_GETARG(3) ! Get command-line arg for input file
  IF(OUTNAMPRED .EQ. ' ') OUTNAMPRED=OUTNAM
  !   Open the input data file
  OPEN(UNIT=INUNIT,FILE=FILENAME,STATUS='OLD',ACTION='READ',IOSTAT=ISTAT)
  IF (ISTAT .NE. 0) THEN
    WRITE(*,20)FILENAME,ISTAT
    CALL UTL_STOP(' ')
  ENDIF
  ! Open Temporary Main Output to hold output until the purpose of  job is known
  IOUTTEMP = UTL_GETUNIT(101,150)
  FNTEMP = TRIM(OUTNAM)//'._outtemp'
  OPEN(UNIT=IOUTTEMP,FILE=FNTEMP,STATUS='REPLACE')
  !   Read options
  CALL BAS_INI_GETOPTIONS(INUNIT,IOUTTEMP)
  WRITE(IOUTTEMP,80) TRIM(FILENAME)
  WRITE(IOUTTEMP,15)VERSION,VERSIONTMP,VERSIONID
  !   Read information in UCODECTRL block
  CALL UCODE_INI(IFAIL,INUNIT,IOUTTEMP,OUTNAM,IDEPTYPE)
  IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
  !   Read GNUCTRL block
  CALL REG_GN_INI(IFAIL,INUNIT,IOUTTEMP,STDERRONE,OUTNAM,OUTNAMPRED, &
                   OPTNLUNC,OPTIMIZE,SENSITIVITIES,TRUSTREGION)
  IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
  IFAILHOLD = IFAIL
  !  Manage naming of main output file depending on purpose of job
  CLOSE(UNIT=IOUTTEMP)
  OPEN(UNIT=IOUTTEMP,FILE=FNTEMP,STATUS='OLD')
  CALL UCODE_INI_FILEMANAGE(IFAIL,IOUTTEMP,OUTNAM,OUTNAMPRED,IOUT)
  IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
  CLOSE(UNIT=IOUTTEMP,STATUS='DELETE')
  IF(IFAILHOLD == -999) THEN
    NORMAL = .FALSE.
    GO TO 999
  ENDIF
  ! CHECK VERSION OF API COMPATIBILITY WITH UCODE
  CALL UTL_VERSION_CHECK(VERSIONMIN,ISTAT)
  IF(ISTAT < 0) THEN
    AMESSAGE = ' Programming error:  Version '//TRIM(VERSIONMIN)//   &
               ' of JUPITER API is required.  Version in use is '   &
               //TRIM(VERSIONID)//' -- Please download newer version '   &
               //'of JUPITER API from U.S. Geological Survey'//   &
               ' JUPITER web site.'
    CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
    CLOSE(IOUT)
    CALL UTL_STOP(AMESSAGE)
   ENDIF
  IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
  !******************************
  !   Read MCMC_CONTROLS block
    IF (MCMC)THEN
      CALL MCMC_INI(IFAIL,INUNIT,IOUT,OUTNAM)
      IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
    ENDIF
  !******************************
  !   Initialize model-execution data
  CALL BAS_INI_MODELEXEC(INUNIT,IOUT)
  !******************************
    ! Read prior distribution information in MCMC_PRIOR block
  IF (MCMC)THEN
    CALL MCMC_INI_PRIOR(IFAIL,INUNIT,IOUT,NCOVMAT)
    IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
  ENDIF
  ! Read parameter data
  !******************************
  IF (.NOT. MCMC)THEN
    CALL UCODE_INI_PARS(IFAIL,INUNIT,IOUT,MAXITER,MAXCHANGE,OUTNAM,OUTNAMPRED, &
                       TOLPAR,FINALSTATS,'DOUBLE')
  IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
  ENDIF
  ! Read and allocate dependent data
  WRITE(IOUT,30)
  CALL DEP_INI_READ(IDEPTYPE,INUNIT,IOUT,NCOVMAT,NOBS,NDOBS,NEOBS,NTOBS,.TRUE.)
  CALL DEP_INI_ALLOC()
  !******************************
  IF (.NOT. MCMC)THEN
    ! Read and allocate prior-information data
    CALL PRI_INI_READ_WPRED(INUNIT,IOUT,.FALSE.,NPE,NCOVMAT,MPR,PRED_MODADV, &
                            MPRWOP,.TRUE.)
    CALL UCODE_INI_CHECK_USEFLAG(IOUT,MPR,MPRWOP)
    CALL PRI_INI_ALLOC(MPR)
    CALL UCODE_INI_CHECK_USEFLAG(IOUT,MPR,MPRWOP)
    ! Allocate GaussNewtonRelated Arrays now that # par dep & pri are known
    CALL REG_GN_INI_ALLOC(IFAIL,NOBS,NPE,NPS,STDERRONE,IFO,PMAXCHANGE,PTOLPAR)
    IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
  ENDIF
  !******************************
  IF (MCMC)THEN
     NPS = MC_NPS        ! number of parameters
     NPD = 0             ! number of derived parameters
     MPR = 0             ! number of linear prior information in regression
     NOPNT = 0           ! decimal point protocol
     PRECIS = 1          ! double precision
  ENDIF
  !******************************
  ! Initialize Equations
  CALL EQN_INI(IFAIL,NPD+NDOBS+MPR)
  IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
  IEQNPTR(1) = NPD
  IEQNPTR(2) = IEQNPTR(1) + NDOBS
  ! Install derived parameter equations
  IF(NPD > 0) THEN
    CALL UCODE_INI_PARS_INSTALL(IFAIL,IOUT)
    IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
  ENDIF
  ! Allocate Parameter Info
  CALL UCODE_INI_ALLOC(IFAIL,IDEPTYPE,MPR)
  IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
  !******************************
  IF (MCMC)THEN
    ALLOCATE(NW(NPSNPD))
    DO I=1,NPSNPD
      PARNAMALL(I)=MC_PARNAMLC(I) ! parameter name in lower case
    ENDDO
  ENDIF
  !******************************
  IF (.NOT. MCMC)THEN
    CALL UCODE_INI_PARNAMALL(IFAIL,IOUT)
    IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
  ENDIF
  !******************************
  ! Join the covariance matrices
  IF (NCOVMAT>0) THEN
    CALL BAS_INI_COVMAT(INUNIT,IOUT,NCOVMAT)
    IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
  ENDIF
  !******************************
  ! Store, and echo convariance matrix of multinormal prior
  IF (MCMC .AND. MULTINORMAL_FLAG .AND. (.NOT. MCMC_PREDICTION))THEN
    CALL MCMC_PRIOR_STORE(IOUT,NCOVMAT,COVMATARR)
  ENDIF
  !******************************
  ! Store, and echo observation-related data
  CALL DEP_INI_STORE(IDEPTYPE,IOUT,NCOVMAT,NOBS,COVMATARR,IEQNPTR(1),OBSNAM, &
                    DTWT,WTFULL,WTFULLSQR)
  DO I=1,NOBS
    IF(NONDETVAL(I) > 0.D0) NONDETECTL = .TRUE.
  ENDDO
  DO I=1,NOBS
    IF( WTCOSUSE(I) == 'CONVERTOS' .OR.  WTCOSUSE(I) == 'SIM' .OR. &
        WTCOSUSE(I) == 'NONE') THEN
      WTOSL = .TRUE.
      EXIT
    ENDIF
    IF(WTCOSUSE(I) == 'OBS' .AND. WTCOS(I) .NE. 0.D0) THEN
      WTOSL = .TRUE.
      EXIT
    ENDIF
  ENDDO
  DO I =1,NOBS
    IF (WTCORRELATED(I)) THEN
      CALL UCODE_INI_SET(.TRUE.)
      EXIT
    ENDIF
  ENDDO
  !******************************
  IF (.NOT. MCMC)THEN
    IF(OPTNLUNC) CALL REG_GN_INI2(IOUT,MPR,NOBS,NPE,NPS, &
                            NONLININT,NONLINPARAM,OBSNAM,PARNAM)
    IF(NONDETECTL) WRITE(IOUT,100)
    ! Check for unique observation names
    CALL DEP_INI_NAMCHK(IOUT)
    IF((RESIDONLY .OR. SENSITIVITIES .OR. PREDICT) .AND. (.NOT. OPTNLUNC) &
          .AND. (.NOT. CREATEINITFILES)) THEN
      IUGM = UTL_GETUNIT(110,150)
      IF(PREDICT) THEN
        FNTEMP = TRIM(OUTNAMPRED)//'._gmp'
      ELSE
        FNTEMP = TRIM(OUTNAM)//'._gm'
      ENDIF
      OPEN(UNIT=IUGM,FILE=FNTEMP,STATUS='REPLACE')
      CALL DEP_DX_WRITE_GM(IUGM,NOBS,OBSNAM,.TRUE.)
    ENDIF
    IF(PARAM_OMIT) THEN
      NORMAL = .FALSE.
      WRITE(IOUT,898)
      IF(PREDICT .AND. .NOT. SENSITIVITIES) THEN
        ALLOCATE(EQTN(NPSWOP,MPRWOP),OMITPRI(MPRWOP),OPINC(NPSWOP),PNAM(NPSWOP))
        CALL UTLUCODE_DX_READ_PAPRI &
            (MPRWOP,NPSWOP,OUTNAM,PNAM,OPINC,EQTN,OMITPRI,SUMOMITPRI)
        IF(SUMOMITPRI > 0)THEN
          CALL UTLUCODE_PRI_OMIT(IOUT,MPRWOP,OMITPRI,PREDICT)
        ENDIF
        DEALLOCATE(EQTN,OMITPRI,OPINC,PNAM)
        IF(SUMOMITPRI == 0)NORMAL = .TRUE.
      ENDIF
      IF(.NOT. NORMAL) GO TO 998
    ENDIF
    ! Store Prior Info
    ALLOCATE(CONVERTED(MPR))
    CALL PRI_INI_STORE_WPRED(IOUT,MPR,NPE,NPS,ITRANS,PARNAMLC,PVAL, &
                             XPRIFILLED,CONVERT_STAT,PRED_MODADV,MPRWOP,CONVERTED)
    ! Install prior equations
    CALL PRI_INI_INSTALL (IOUT,MPR,IEQNPTR(2))
    CALL UTLUCODE_CHECK_PRI(IOUT,MPR,MPRWOP,NPS,NPSWOP,PADJ,PARNAMLC,WRPRIEQ)
    IF(MPR > 0) CALL PRI_DX_WRITE_GM(IUGM,MPR,PRINAM)
    CLOSE(IUGM)
    IF(NPEFP .NE. MPR-MPRWOP)THEN
      WRITE(IOUT,*)
      WRITE(IOUT,*) &
      ' PRESENCE OF ADJUSTABLE PARAMETERS in PARAMETER_DATA_FOR_PREDICTION'
      WRITE(IOUT,*) &
      ' REQUIRES LINEAR_PRIOR_INFORMATION_FOR_PREDICTION'
      WRITE(*,*)
      CALL UTL_STOP('LINEAR_PRIOR_INFORMATION_FOR_PREDICTION REQUIRED')
    ENDIF
    CALL TYP_NULL(WTINIT)
    CALL TYP_NULL(WTINITSQR)
    WTINIT = WTFULL
    WTINITSQR = WTFULLSQR
  ENDIF
  !******************************
  ! Calculate weighted observations
  CALL STA_INI(NOBS,OBSVAL,WTFULLSQR)
  !******************************
  IF (.NOT. MCMC)THEN
    !
    ! Store and echo prior-information-related data, and create
    ! weight matrix for all prior-information equations.
    IF (MPR>0) THEN
      CALL PRI_INI_PROCESS(IOUT,IWRITE,MPR,NCOVMAT,COVMATARR,DTPRIWT)
      CALL PRI_INI_POPX(IOUT,3,MPR,NPE,NPS,IPTR,ITRANS,PARNAM, &
                        1.D0,XPRI,XPRIFILLED,CONVERTED)
      CALL UTL_WRITECDMATRIX(PRIWTMAT,0,iout,.TRUE.)
      CALL UTL_WRITECDMATRIX(PRIWTMATSQR,0,iout,.TRUE.)
    ENDIF
  ENDIF
  !******************************
  !
  !   Initialize Model_IO module
  CALL MIO_INI_ALLOC(IFAIL,NPSNPD)
  !
  !   Read, store, and echo file names for model input and output
  CALL MIO_INI_INPUTFILES(INUNIT,IOUT)
  CALL MIO_INI_OUTPUTFILES(INUNIT,IOUT)
  !
  !   Needs both Parameters and Derived Parameters
  !   Do initialization related to model-input files
  CALL MIO_INI_TEMPLATE(IFAIL,NPSNPD,PARNAMALL,NW)
  IF (IFAIL .NE. 0) CALL UTL_STOP(' ')
  !
  !   Initialize and populate instruction arrays
  CALL MIO_INI_INSTRUCT1(IFAIL)
  IF (IFAIL .NE. 0) CALL UTL_STOP(' ')
  !
  !   Allocate memory for storage of instructions
  CALL MIO_INI_INSTRUCTALLOC(NEOBS,IFAIL)
  IF (IFAIL .NE. 0) CALL UTL_STOP(' ')
  CALL MIO_INI_INSTRUCT2(IFAIL,NCATS,MCVCAT)
  IF (IFAIL .NE. 0) CALL UTL_STOP(' ')
  !
  IF (NPE == 0 .AND. .NOT. MCMC) THEN
    WRITE(IOUT,910)
    WRITE(IOUT,911)
    WRITE(*,910)
    WRITE(*,911)
    GO TO 999
  ENDIF
  !   Initialize parallel-processing data
  CALL MIO_INI_DIMENSION(LCIS,NINSTRUCT,NMIFILE,NMOFILE,NUMLADV)
  ALLOCATE(CATGOR(NMOFILE),CINSTSET(LCIS),INSTRUCTFILE(NMOFILE), &
           LOCINS(NINSTRUCT),MIFILE(NMIFILE),MOFILE(NMOFILE), &
           MRKDL(NMOFILE),TFILE(NMIFILE))
  CALL MIO_INI_ARRAYS(IFAIL,LCIS,NINSTRUCT,NMIFILE,NMOFILE,CATGOR,CINSTSET, &
                      INSTRUCTFILE,LOCINS,MIFILE,MOFILE,MRKDL,TFILE)
  IF (IFAIL.NE.0) CALL UTL_STOP &
      ('Programming error: MIO_INI_ARRAYS reports failure')
! ********************************************************************
! *******************     RUN UCODE IN MCMC MODE    ******************
! ********************************************************************
  IF (MCMC) THEN
    CALL PLL_INI_DISPATCHER(INUNIT,IOUT,LCIS,NCATS,NINSTRUCT,NMIFILE,NMOFILE, &
                             NOPNT,NPSNPD,NUMLADV,NEOBS,PRECIS,CATGOR,CINSTSET, &
                             OBSEXTNAM,INSTRUCTFILE,LOCINS,MCVUSE,MIFILE, &
                             MOFILE,MRKDL,NW,PARNAMALL,TFILE,PARALLEL_ACTIVE)
    WRITE(IOUT,300)
    CALL MCMC_INIT_MODEL(NW,NCATS,NEOBS,OBSEXTNAM,MCVUSE,NOBS,OBSNAM,WTFULLSQR, &
                        NDOBS,MODEXTVAL,MODDERVAL,OBSEXTOBSNUM,OBSDEROBSNUM, &
                        IEQNPTR,PARALLEL_ACTIVE, &
                        LCIS,NINSTRUCT,NMIFILE,NMOFILE,NUMLADV,CATGOR,CINSTSET, &
                        INSTRUCTFILE,LOCINS,MIFILE,MOFILE,MRKDL,TFILE)
    IF (.NOT. MCMC_PREDICTION) THEN
      CALL MCMC_INIT_JUMPRATE()
      CALL MCMC_INIT_GR()
      CALL MCMC_INIT_CHAIN()
      CALL MCMC_DREAM_ALGM()
      CALL MCMC_OUTPUTRESTART()
    ELSE
       CALL MCMC_PREDICTION_RUN()
    ENDIF
    WRITE(IOUT,*)
    WRITE(*,900)VERSION
    WRITE(IOUT,900)VERSION
    CALL MCMC_CLN()
  ENDIF
! ********************************************************************
! ************  RUN OTHER UCODE MODES EXCEPT MCMC   ******************
! ********************************************************************
  IF (.NOT. MCMC)THEN
    IF (SVDT) THEN
      ! DEFAULT SVDSTART and SVDCNT are 1
      IF(SVDPHASE1) THEN
        SVDCNT = 3
      ELSE
        SVDSTART = 2
        SVDCNT = 3
      ENDIF
    ENDIF
    NPETMP = NPE
    NPSTMP = NPS
    ! *******************************************************************
    ! *******************************************************************
    ! SVD LOOP (IF NOT SVDT, THIS LOOP GOES from 1 to 1 and is irrelevant
    ! *******************************************************************
    ! *******************************************************************
    SVDCYCLE: DO SVDLOOP=SVDSTART,SVDCNT
      NPENOW = NPETMP
      NPSNOW = NPSTMP
      !   Write parameter information to output file
      IF(SVDLOOP == 1) CALL UCODE_INI_PARWRITE(IOUT,SVDLOOP)
      IF(SVDLOOP == 2) THEN
        IF(SVDPHASE1) THEN
          WRITE(IOUT,700)
          WRITE(IOUT,701)
          WRITE(IOUT,700)
          OPTIMIZE = .TRUE.
          OPT = .TRUE.
          FINALSTATS = .FALSE.
          FINALSTATSDONE = .FALSE.
          GNUDONE = .FALSE.
          INTDONE = .FALSE.
          ITERP = 0
          ICNT = 1
          IEND = 0
          IFO = 0
          IND = 0
        ELSE
          WRITE(IOUT,700)
          WRITE(IOUT,702)
          WRITE(IOUT,700)
        ENDIF
        CALL UCODE_INI_PARWRITE(IOUT,SVDLOOP)
        CALL UCODE_INI_SVD(IOUT,OUTNAM,MPR,NPETMP,NPSTMP,SVDLOOP,NPENOW,NPSNOW)
       ! Allocate GaussNewtonRelated Arrays now that # par dep & pri are known
        CALL REG_GN_INI_ALLOC &
            (IFAIL,NOBS,NPENOW,NPSNOW,STDERRONE,IFO,PMAXCHANGE,PTOLPAR)
        IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
      ENDIF
      IF(SVDLOOP == 3 .AND. SVDPHASE1) &
        CALL UCODE_FINAL_SVD(IOUT,MPR,NPETMP,NPSTMP,SVDLOOP,NPENOW,NPSNOW)
      ! create sensitivity matrix for all prior-information equations.
      IF (MPR>0) THEN
        IF(SVDLOOP == 2) THEN
          CALL PRI_INI_POPX(IOUT,3,MPR,NPETMP,NPS,IPTRTMP,ITRANS,PARNAMTMP, &
                            1.D0,XPRI,XPRIFILLED)
          CALL UCODE_GEN_SVDPOPX(MPR,NPETMP,SVDLOOP)
        ELSEIF(SVDLOOP == 1 .OR. SVDLOOP == 3) THEN
          CALL PRI_INI_POPX(IOUT,3,MPR,NPE,NPS,IPTR,ITRANS,PARNAM, &
                            1.D0,XPRI,XPRIFILLED)
        ENDIF
      ENDIF
      CALL SEN_INI(DERIV_INTERFACE,IOUT,NOBS,NPENOW,NPSNOW,IPTR, &
                   ISENMETHOD,PARNAM,OBSNAM,IFAIL)
      IF (IFAIL .NE. 0) CALL UTL_STOP(' ')
      CALL PLL_INI_DISPATCHER(INUNIT,IOUT,LCIS,NCATS,NINSTRUCT,NMIFILE,NMOFILE, &
                              NOPNT,NPSNPD,NUMLADV,NEOBS,PRECIS,CATGOR,CINSTSET, &
                              OBSEXTNAM,INSTRUCTFILE,LOCINS,MCVUSE,MIFILE, &
                              MOFILE,MRKDL,NW,PARNAMALL,TFILE,PARALLEL_ACTIVE)
      !   Allocate arrays declared in main program unit
      MAXRUNSPLL = 2*NPENOW+1
      IF(ALLOCATED(DEPVALSETS)) DEALLOCATE(DEPVALSETS)
      IF(ALLOCATED(DEPDERVALSETS)) DEALLOCATE(DEPDERVALSETS)
      IF(ALLOCATED(DEPEXTVALSETS)) DEALLOCATE(DEPEXTVALSETS)
      IF(ALLOCATED(KPE)) DEALLOCATE(KPE)
      IF(ALLOCATED(PARVALSETS)) DEALLOCATE(PARVALSETS)
      IF(ALLOCATED(PARADAVALSETS)) DEALLOCATE(PARADAVALSETS)
      IF(ALLOCATED(PVALTMP)) DEALLOCATE(PVALTMP)
      ALLOCATE(DEPVALSETS(NOBS,MAXRUNSPLL),DEPDERVALSETS(NDOBS,MAXRUNSPLL), &
               DEPEXTVALSETS(NEOBS,MAXRUNSPLL),KPE(MAXRUNSPLL), &
               PARVALSETS(NPENOW,MAXRUNSPLL), &
               PARADAVALSETS(NPSNPD,MAXRUNSPLL),PVALTMP(NPSNPD))
      DEPVALSETS = 0.D0
      DEPDERVALSETS = 0.D0
      DEPEXTVALSETS = 0.D0
      KPE = 0
      PARVALSETS = 0.D0
      PARADAVALSETS = 0.D0
      PVALTMP = 0.D0
      !
      ! **************************************************************************
      ! ************************** TOP OF TASK CONTROL LOOP **********************
      ! **************************************************************************
      !
      IF((NOBS - 2) < NPE) THEN
        UNUSUAL = .TRUE.
        WRITE(IOUT,*)
        WRITE(IOUT,*)' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        WRITE(IOUT,*)
        WRITE(IOUT,*)' #Observations and  #Parameters: ',NOBS,NPE
        WRITE(IOUT,*)
        WRITE(*,*)
        WRITE(*,*)' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        WRITE(*,*)
        WRITE(*,*)' #Observations and  #Parameters: ',NOBS,NPE
        WRITE(*,*)
        IF(OPTIMIZE) THEN
          AMESSAGE = 'ERROR: '
        ELSE
          AMESSAGE = 'WARNING: '
        ENDIF
        CALL UTL_WRITE_MESSAGE(IOUT,'no','no','yes')
        CALL UTL_WRITE_MESSAGE(-1,'no','no','yes')
        IF(.NOT. SVDT) THEN
          AMESSAGE = &
          'UCODE_2014 is designed for problems that have fewer adjustable ' // &
          'parameters than observations. The current problem has more ' // &
          'adjustable parameters than observations. UCODE_2014 will ' // &
          'execute one forward run and evaluate the residuals for the ' // &
          'current observations, it will also calculate sensitivities and ' // &
          'some of the resulting statistics if sensitivities=yes in the ' // &
          'UCODE_Control_Data input block. However, the problem will need ' // &
          'to be re-posed before omitizing parameters with UCODE_2014.'
        ELSE
          AMESSAGE = &
          'UCODE_2014 is designed to use SVD with problems that have ' // &
          'fewer adjustable parameters than observations. The current ' // &
          'problem has more adjustable parameters than observations.'
        ENDIF
        CALL UTL_WRITE_MESSAGE(IOUT,'no','no','yes')
        CALL UTL_WRITE_MESSAGE(-1,'no','no','yes')
        AMESSAGE = &
        ' Options for proceeding include:'
        CALL UTL_WRITE_MESSAGE(IOUT,'no','no','yes')
        CALL UTL_WRITE_MESSAGE(-1,'no','no','yes')
        AMESSAGE = &
        '1. If available, include more observations.'
        CALL UTL_WRITE_MESSAGE(IOUT,'no','no','yes')
        CALL UTL_WRITE_MESSAGE(-1,'no','no','yes')
        AMESSAGE = &
        '2. When there are more parameters than observations, UCODE_2014' // &
        ' sensitivity analysis mode can still produce composite scaled' // &
        ' sensitivities (CSS). The CSS can be used to identify insensitive' // &
        ' parameters and the user can either (a) join the values of' // &
        ' insensitive parameters to other parameters using the' // &
        ' Derived_Parameter input block in ways that are motivated by' // &
        ' (or at least do not contradict) the conceptual understanding' // &
        ' of the system, or (b) set adjustable=no for some of the least' // &
        ' sensitive parameters so that the starting value is maintained.' // &
        ' If this results in a reasonable system representation with fewer' // &
        ' parameters than observations, UCODE capabilities can be used' // &
        ' to estimate parameter values.'
        CALL UTL_WRITE_MESSAGE(IOUT,'no','no','yes')
        CALL UTL_WRITE_MESSAGE(-1,'no','no','yes')
        AMESSAGE = &
        '3. Use software that allows more adjustable parameters than' // &
        '  observations, such as the PEST or OSTRICH model inversion' // &
        '  software. In general, these programs solve the problem by' // &
        '  using parameter regularization and either, for PEST, singular' // &
        '  value decomposition (SVD) or, for OSTRICH, heuristic methods' // &
        '  like genetic algorithms (GA) that do not require derivatives.' // &
        '  Often such approaches lead to limiting adjustment of the least' // &
        '  sensitive parameters, so there is correspondence' // &
        '  between options 2 and 3.'
        CALL UTL_WRITE_MESSAGE(IOUT,'no','no','yes')
        CALL UTL_WRITE_MESSAGE(-1,'no','no','yes')
        IF(OPTIMIZE .OR. SVDT) THEN
          AMESSAGE = 'TERMINATING, # Observations < # Parameters'
          GO TO 999
        ENDIF
      ENDIF
      IF(SVDLOOP == 1 .OR. (SVDLOOP == 2 .AND. (.NOT. SVDPHASE1))) &
        WRITE(IOUT,300)
      ! Initialize Control Loop Variables
      INTERVALLOOPS = 1
      LINEARITYLOOPS = 0
      LINEARITYDONE  = .FALSE.
      CTRLDONE   = .FALSE.
      SENSDONE   = .TRUE.      ! Necessary initial condition
      SENFORINIT   = .FALSE.
      GNUDONE    = .FALSE.
      FINALSTATSDONE = .FALSE.
      ICTRL = 0
      !
      CONTROL: DO WHILE (.NOT. CTRLDONE)
        !
        ! ************************************************************************
        ! ************** DEFINE JOB OF CURRENT ITERATION OF CONTROL LOOP *********
        ! ************************************************************************
        CALL UCODE_DEF(IFAIL,JOBDIM,JOBLEN,CTRLJOB, &
                              FINALSTATSDONE,IFO,IOUT, &
                              LINEARITYDONE,OUTNAM, &
                              SENSDONE,SVDLOOP, &
                              FINALSTATS,INTDONE,INTERVALLOOPS,ITERP, &
                              TRUSTREGION,LINEARITYLOOPS, &
                              CTRLDONE,ICTRL, &
                              KPPL,NUMPPL)
        IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
        IF (CTRLDONE) EXIT CONTROL
        IF(OPTNLUNC .AND. ITERP == 0 .AND. IINT > 0 .AND. SENFORINIT) THEN
          IF(CTRLJOB(ICTRL)=='FORWARD' .OR. CTRLJOB(ICTRL)=='FORWARD&SENS') THEN
            IF(MPR > 0) THEN
              CALL UCODE_GEN_INITRESSEN(MPR,NOBS,NPE,1,RESIDSPRI,WTDRESIDSPRI)
            ELSE
              CALL UCODE_GEN_INITRESSEN(MPR,NOBS,NPE,1)
            ENDIF
            CYCLE CONTROL
          ELSEIF(CTRLJOB(ICTRL)=='SENSITIVITY') THEN
            CYCLE CONTROL
          ENDIF
        ENDIF
        ! IF JOB IS SENSITIVITY DEFINE CHARACTER OF THE SENS JOB & WRITE LABEL
        IF (CTRLJOB(ICTRL)=='SENSITIVITY' .OR. CTRLJOB(ICTRL)=='FORWARD&SENS')THEN
          CALL SEN_DEF(CTRLJOB(ICTRL),IOUT,NOBS,NPENOW,NPSNOW,IPTR, &
                   ISENMETHOD,PADJ,PARNAM,SENSDONE,XSENST,ISNPRT,NUMPPL)
          IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
        ENDIF
        !
        ! ************************************************************************
        ! ************* GENERATE PARAMETER VALUES--Populate PARVALSETS ***********
        ! ************************************************************************
        IF (NUMPPL>0) THEN
          DO KPPL=1,NUMPPL
            ! FORWARD
            IF (CTRLJOB(ICTRL)=='FORWARD' .OR. &
                              CTRLJOB(ICTRL)=='FORWARD&SENS' ) THEN
              ! If task is calculating data sets to run LINEARITY
              IF (LINEARITY) THEN
                IF(LINEARITYADV) THEN
                  CALL UCODE_GEN_LINEARITY_ADV(IFAIL,MPR,IOUT,LINEARITYLOOPS, &
                   OUTNAM,OUTNAMPRED,PRINAM,LINEARITYDONE,PARVALSETS(:,KPPL:KPPL))
                ELSE
                  IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
                  CALL UCODE_GEN_LINEARITY(IFAIL,IOUT,LINEARITYLOOPS,OUTNAM, &
                             OUTNAMPRED,LINEARITYDONE,PARVALSETS(:,KPPL:KPPL))
                  IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
                ENDIF
              ! If task is sossurface mode substitute incremental parameters
              ELSEIF(SOSSURFACE) THEN
                CALL UCODE_GEN_SOSSURF &
                     (IFAIL,IOUT,OUTNAM,PARVALSETS(:,KPPL:KPPL))
                IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
              ! If task is prediction mode substitute optimal parameters
              ELSEIF(PREDICT) THEN
                CALL UCODE_GEN_PREDICT (IFAIL,IOUT,OUTNAM,OUTNAMPRED, &
                                                PARVALSETS(:,KPPL:KPPL))
                IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
              ! If calculating residuals, sensitivities or optimal parameters
              ELSE
                IF(LOWEST .OR. LOWEST_DIFFERS) THEN
                  IF(OPTIMIZE) THEN
                    IF(ICNT == 2) THEN
                      CALL UCODE_EVA_LOWEST(IFAIL,IOUT,ITERP,WTOSL,UNUSUAL)
                      LOWEST = .FALSE.
                      ICNT = 3
                    ENDIF
                    IF(ICNT == 1)ICNT = 2
                  ELSE
                    ! Nonlinear intervals rather than optimize
                    LOWEST = .FALSE.
                  ENDIF
                ENDIF
                CALL BAS_GEN(NOPNT,NPENOW,NW,PRECIS,PVAL)
                IF(KPPL .EQ. 1) THEN
                  J = 1
                  DO I=1,NPSNOW
                    IF(PADJ(I)) THEN
                      PARVALSETS(J,KPPL) = PVAL(I)
                      J = J +1
                    ENDIF
                  ENDDO
                ENDIF
              ENDIF
              IF(IFAIL == -999) THEN
                NORMAL = .FALSE.
                GO TO 999
              ENDIF
              IF(PARAM_OMIT) THEN
                NORMAL = .FALSE.
                GO TO 998
              ENDIF
              IF(SVDLOOP==2)CALL UCODE_GEN_SVDCALCPAR &
                             (IOUT,NPETMP,NPE,PARVALSETS(:,KPPL:KPPL),.FALSE.)
              CALL UCODE_GEN_PVALALL(IFAIL,IOUT,NPETMP, &
                   PARVALSETS(:,KPPL:KPPL),PARADAVALSETS(:,KPPL:KPPL))
              IF(.NOT. SOSSURFACE) THEN
                IF((KPPL == 1 .AND. WRPAREQ) .OR. ITERP == 1) &
                   CALL UCODE_GEN_DERPARWRITE(IOUT,PARADAVALSETS(:,KPPL:KPPL))
              ENDIF
            ELSEIF ((CTRLJOB(ICTRL)=='SENSITIVITY')) THEN
              CALL SEN_GEN(CTRLJOB(ICTRL),KPPL,NPENOW,NPSNOW,IOUT,MAG_PERTURB, &
                           IPTR,ISENMETHOD,NOPNT,NW,PARNAM,PRECIS,PVAL, &
                           KPE(KPPL),PARVALSETS(:,KPPL:KPPL))
              IF(SVDLOOP==2)CALL UCODE_GEN_SVDCALCPAR &
                             (IOUT,NPETMP,NPE,PARVALSETS(:,KPPL:KPPL),.TRUE.)
              ISN = 0
              IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
              !   Store parameters for each
              CALL UCODE_GEN_PVALALL(IFAIL,IOUT,NPETMP, &
                   PARVALSETS(:,KPPL:KPPL),PARADAVALSETS(:,KPPL:KPPL))
              IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
            ELSEIF (CTRLJOB(ICTRL) .EQ. 'GAUSS-NEWTON') THEN
              IF(OMIT_INSENS) CALL UCODE_UEV_CSS(IFAIL,0,1,0,IOUT,0,MPR, &
                                                 PRIWTMATSQR,0)
              IF(TRUSTREGION) THEN
                CALL REG_TR_GEN(IFAIL,MPR,NOBS,NEOBS,NDOBS, &
                            NPENOW,NPETMP,SVDLOOP,NPSNOW, &
                            IOUT,CSS,DATAEXCHANGE,MAXITER,IOMIT, &
                            IPTR,LN,CREATEINITFILES,OPTIMIZE,OUTNAM,PADJ, &
                            PARNAM,PRIWTMAT,PTOLPAR,PVALINIT,  &
                            RESIDS,RESIDSPRI,RSQALL,WTFULLSQR,PRIWTMATSQR, &
                            WTFULL,XPRI,XSENST,FINALSTATS, &
                            GNUDONE,IFO,ITERP,ISENMETHOD,PVAL)
                IF(GNUDONE) FINALSTATS = .TRUE.
              ELSEIF(OPTNLUNC) THEN
                ! GET VALUES FOR THE PREDICTION OF THIS LOOP
                CALL REG_GN_INIUNC (IOUT,INTERVALLOOPS,ITERP, &
                                NOBS,NPE,NPS,MODELVAL,PVAL, &
                                RESIDS,WTFULL,XSENST,IINT)
                CALL REG_GN_UNC (IFAIL,IOUT,IINT, &
                                 MPR,NPE,NPS,.TRUE., &
                                 BSCAL,IPTR,LN,PARNAM, &
                                 PRIWTMAT,PVALINIT, &
                                 RESIDSUNC,RESIDSPRI,RSQALL, &
                                 FINALSTATS,WTFULLUNC,XPRI, &
                                 FL,GNUDONE,IEND,IFO,IND,PVAL,XSENSTUNC,ZZ)
              ELSE
                CALL REG_GN_GEN(IFAIL,MPR,NOBS,NPENOW,NPSNOW, &
                          CONSTRAIN,CONSTRAINL,CONSTRAINLNOT,CSS,DATAEXCHANGE, &
                          IOUT,IOMIT,IPTR,LN,CREATEINITFILES,OPTIMIZE,OUTNAM, &
                          PADJ,PARNAM,PMAXCHANGE, &
                          PRIWTMAT,PTOLPAR,PVALINIT,PVALMAXC,PVALMINC,  &
                          RESIDS,RESIDSPRI,RSQALL, &
                          STDERRONE,SVDLOOP,SVDT,.FALSE.,WTFULL,WTOSL, &
                          XPRI,XSENST,FINALSTATS,CTRLDONE,GNUDONE, &
                          IEND,IFO,IND,ISENMETHOD,PVAL)
                IF(IND > 0) GO TO 999
              ENDIF
              IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
              IF(GNUDONE) LOWEST = .TRUE.
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
          ! ********************* TOP OF POTENTIALLY PARALLEL LOOP *************
          ! ********************* TOP OF POTENTIALLY PARALLEL LOOP *************
          ! ********************* TOP OF POTENTIALLY PARALLEL LOOP *************
          !
          PPL: DO KLOOP=1,NUMLOOP
            PPL1: DO KPPL=1,NRUNSPLL
              IF (DO_PARALLEL) THEN
                KLPTR = KPPL
              ELSE
                KLPTR = KLOOP
              ENDIF
              !
              IF(CTRLJOB(ICTRL)=='FORWARD' .OR. &
                                         CTRLJOB(ICTRL)=='FORWARD&SENS')THEN
                DO I=1,NPSNPD
                  PVALTMP(I) = PARADAVALSETS(I,KLPTR)
                ENDDO
              ELSEIF (CTRLJOB(ICTRL)=='SENSITIVITY') THEN
                !  Populate PVALTMP with current parameter values, with substitution
                !  of adjustable parameters as needed for current iteration.
                IF (.NOT. DO_PARALLEL)  THEN
                  DO I=1,NPSNPD
                    PVALTMP(I) = PARADAVALSETS(I,KLPTR)
                  ENDDO
                ENDIF
                IF(FINALSTATS) NPERTURB_CYCLES = NPERTURB_CYCLES_FINAL
              ENDIF
            ENDDO PPL1
            !
            IF (.NOT. DO_PARALLEL) THEN
              ! ******************************************************************
              ! ********************** ADAPT PARAMETER VALUES ********************
              ! ******************************************************************
              !
              CALL MIO_ADA_WRITEFILES(IFAIL,NPSNPD,PARNAMALL,NOPNT,NW,PRECIS, &
                                      PVALTMP)
              IF (IFAIL .NE. 0) CALL UTL_STOP()
              !
              ! ******************************************************************
              ! ********************** EXECUTE APPLICATION MODEL *****************
              ! ******************************************************************
              !
              IF (CTRLJOB(ICTRL) .EQ. 'FORWARD' .OR. &
                   CTRLJOB(ICTRL)=='FORWARD&SENS') THEN
                CALL BAS_EXE_SELECT(IOUT,CTRLJOB(ICTRL),ICOMMAND)
              ELSEIF (CTRLJOB(ICTRL) .EQ. 'SENSITIVITY') THEN
                CALL SEN_EXE_SELECT(IOUT,ICOMMAND)
              ENDIF
              CALL UCODE_EXE_SELECT(CTRLJOB(ICTRL),FINALSTATS,ITERP,ICOMMAND)
              IF (ICOMMAND>0) THEN
                ISENPERT = 0
                IF(CTRLJOB(ICTRL) .EQ. 'SENSITIVITY') THEN
                  ISN = ISN + 1
                  DO I=1,NPE
                    IF(PARVALSETS(I,ISN) .NE. PVAL(IPTR(I))) THEN
                      WRITE(*,90)PARNAM(IPTR(I)),PVAL(IPTR(I)),PARVALSETS(I,ISN)
                      ISENPERT = 1
                      IF(IVERB > 3) &
                      WRITE(IOUT,90) &
                                 PARNAM(IPTR(I)),PVAL(IPTR(I)),PARVALSETS(I,ISN)
                      EXIT
                    ENDIF
                  ENDDO
                ENDIF
                IF(ISENPERT == 0 .AND. CTRLJOB(ICTRL) .EQ. 'SENSITIVITY' .AND. &
                   FORSENS .AND. &
                   (.NOT. TRUSTREGION .OR. (TRUSTREGION .AND. ITERP == 0))) THEN
                  CONTINUE
                ELSE
                  NUMMODRUNS = NUMMODRUNS + 1
                  IF(FINALSTATS .AND. SVDLOOP == 1)NUMFINSTAT = NUMFINSTAT + 1
                  IF(FINALSTATS .AND. SVDLOOP == 3)NUMFINSTAT3 = NUMFINSTAT3 + 1
                  IF(SVDLOOP == 2) NUMSVDRUNS = NUMSVDRUNS + 1
                  CALL BAS_EXE(ICOMMAND,-1,KLOOP,NUMPPL)
                ENDIF
              ENDIF
              !
              ! ******************************************************************
              ! ******************* EXTRACT VALUES FROM MODEL OUTPUT *************
              ! ******************************************************************
              ! Extract simulated equivalents to observations
              IF (ICOMMAND>0) THEN
                CALL MIO_EXT(IFAIL,IOUT,NCATS,NEOBS,OBSEXTNAM,MCVUSE, &
                             DEPEXTVALSETS(:,KLOOP:KLOOP),INSTRUCTION)
                IF (IFAIL .NE. 0) THEN
                  WRITE(IOUT,200) TRIM(INSTRUCTION)
                  CALL UTL_STOP(' ')
                ENDIF
              ENDIF
            ELSE
              ! ******************************************************************
              ! ********************** MAKE RUNS IN PARALLEL *********************
              ! ******************************************************************
              !
              !   Assign command to be run in parallel
              IF (CTRLJOB(ICTRL) .EQ. 'FORWARD' .OR. &
                  CTRLJOB(ICTRL)=='FORWARD&SENS') THEN
                CALL BAS_EXE_SELECT(IOUT,CTRLJOB(ICTRL),ICOMMAND,COMMANDPLL)
              ELSEIF (CTRLJOB(ICTRL) .EQ. 'SENSITIVITY') THEN
                CALL SEN_EXE_SELECT(IOUT,ICOMMAND,COMMANDPLL)
              ENDIF
              !
              !   Adapt, execute, and extract in parallel
              NUMMODRUNS = NUMMODRUNS + NRUNSPLL
              IF(FINALSTATS .AND. SVDLOOP == 1)NUMFINSTAT = NUMFINSTAT+NRUNSPLL
              IF(FINALSTATS .AND. SVDLOOP == 3)NUMFINSTAT3 = NUMFINSTAT3+NRUNSPLL
              IF(SVDLOOP == 2)NUMSVDRUNS = NUMSVDRUNS  + NRUNSPLL
              CALL PLL_MAKE_RUNS(LCIS,NCATS,NINSTRUCT,NMIFILE,NMOFILE,NOPNT, &
                                 NPSNPD,NUMLADV,NRUNSPLL,NEOBS,PRECIS, &
                                 CATGOR,CINSTSET,OBSEXTNAM,INSTRUCTFILE, &
                                 LOCINS,MCVUSE,MIFILE,MOFILE,MRKDL,NW,PARNAM, &
                                 PARADAVALSETS,TFILE,DEPEXTVALSETS,COMMANDPLL)
            ENDIF
            !
            PPL2: DO KPPL=1,NRUNSPLL
              IF (DO_PARALLEL) THEN
                KLPTR = KPPL
              ELSE
                KLPTR = KLOOP
              ENDIF
              !
              IF(NEOBS > 0) THEN
                DO I=1,NEOBS
                  IF(OBSEXTOBSNUM(I) > 0) THEN
                    DEPVALSETS(OBSEXTOBSNUM(I),KLPTR)=DEPEXTVALSETS(I,KLPTR)
                  ENDIF
                ENDDO
              ENDIF
              IF(NDOBS > 0) THEN
                CALL DEP_EXT_DER(IOUT,NDOBS,NEOBS,IEQNPTR(1), &
                                    DEPEXTVALSETS(:,KLPTR:KLPTR), &
                                    DEPDERVALSETS(:,KLPTR:KLPTR))
                IF (IFAIL .NE. 0) THEN
                  WRITE(IOUT,200) TRIM(INSTRUCTION)
                  CALL UTL_STOP(' ')
                ENDIF
                DO I=1,NDOBS
                  IF(OBSDEROBSNUM(I) > 0) THEN
                    DEPVALSETS(OBSDEROBSNUM(I),KLPTR)=DEPDERVALSETS(I,KLPTR)
                  ENDIF
                ENDDO
              ENDIF
              !
              !   If there's nonlinear prior info, populate tempval elements here?
              !   This would use the equation module.
              !
              IF (CTRLJOB(ICTRL) .EQ. 'FORWARD' .OR. &
                  CTRLJOB(ICTRL)=='FORWARD&SENS') THEN
                DO I=1,NEOBS
                  MODEXTVAL(I) = DEPEXTVALSETS(I,KLPTR)
                  IF(OBSEXTOBSNUM(I) > 0) THEN
                    MODELVAL(OBSEXTOBSNUM(I)) = MODEXTVAL(I)
                  ENDIF
                ENDDO
                IF (NDOBS > 0) THEN
                  DO I=1,NDOBS
                    MODDERVAL(I) = DEPDERVALSETS(I,KLPTR)
                    IF(OBSDEROBSNUM(I)>0) THEN
                      MODELVAL(OBSDEROBSNUM(I)) = MODDERVAL(I)
                  ENDIF
                  ENDDO
                ENDIF
              ENDIF
            ENDDO PPL2
          ENDDO PPL
          !
          ! ********************************************************************
          ! ******************* END OF POTENTIALLY PARALLEL LOOP ***************
          ! ********************************************************************
          !
        ENDIF

        ! ************************* USE EXTRACTED VALUES *************************
        !
        IF(PREDICT .AND. .NOT. SENSITIVITIES) &
           CALL UCODE_UEV_PRINT_PREDICT(IFAIL,IOUT,NEOBS,OBSEXTNAM,DEPEXTVALSETS)
        IF (CTRLJOB(ICTRL) .EQ. 'FORWARD' .OR. CTRLJOB(ICTRL)=='FORWARD&SENS' &
            .OR. (CTRLJOB(ICTRL) .EQ. 'GAUSS-NEWTON' .AND. TRUSTREGION)) THEN
           CALL UCODE_UEV_OMIT(IFAIL,FINALSTATS,IOUT,ITERP,WTINIT,WTINITSQR, &
                               DTWTINIT)
          IF(IFAIL > 0) THEN
            NORMAL = .FALSE.
            GO TO 999
          ENDIF
          ! MERGE SENSITIVITY FILES IF NECESSARY
          IF(NMERGE > 0) CALL UCODE_UEV_MERGE(IFAIL)
          !PRINT RESIDUALS *******************************************************
          !   Determine whether to print residuals, and print if appropriate
          IF (LINEARITY) THEN
            IF(MPR>0) THEN
              CALL PRI_UEV_RESIDUALS &
                     (IOUT,MPR,NPD+NDOBS,NPS,PARNAMLC,PVAL)
            ENDIF
            IF(LINEARITYADV) THEN
              CALL UCODE_UEV_LINEARITY_ADV &
                   (IFAIL,IOUT,LINEARITYDONE,LINEARITYLOOPS,MPR,MODELPRIVAL)
            ELSE
              CALL UCODE_UEV_LINEARITY(IFAIL,IOUT,LINEARITYDONE,LINEARITYLOOPS, &
                                       OUTNAM)
            ENDIF
            IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
          ELSE
            IF (ITERP == 0 .AND. OPTIMIZE .AND. &
             (CTRLJOB(ICTRL) == 'FORWARD' .OR. CTRLJOB(ICTRL) == 'FORWARD&SENS') &
             .AND. WTOSL .AND. .NOT. WTADJFINAL) &
              CALL UCODE_UEV_CALCWT(IFAIL,IOUT,ITERP,MAXITER,TF,DTWT)
            IF(NONDETECTL) THEN
              CALL UCODE_UEV_RESIDUALS(NOBS,MODELVAL,WTFULLSQR,RESIDS,WTDRESIDS)
            ELSE
              CALL DEP_UEV_RESIDUALS(NOBS,MODELVAL,WTFULLSQR,RESIDS,WTDRESIDS)
            ENDIF
            IF(MPR>0) THEN
              CALL PRI_UEV_RESIDUALS &
                     (IOUT,MPR,NPD+NDOBS,NPS,PARNAMLC,PVAL)
            ENDIF
            CALL UCODE_UEV_RESID_PRINT_DECISION &
                     (IFAIL,IOUT,ITERP,FINALSTATS,MAXITER,MPR,MODELPRIVAL, &
                      NONDETECTL,OUTNAM,OUTNAMPRED,PRINAM,PRIVAL,PRIWTMAT, &
                      PRIWTMATSQR,RESIDSPRI,SVDLOOP,TRUSTREGION,WTDRESIDSPRI, &
                      WTFULLSQR,AVET)
            IF (OPTIMIZE .AND. (CTRLJOB(ICTRL) == 'FORWARD' &
                                .OR. CTRLJOB(ICTRL) == 'FORWARD&SENS') &
                .AND. WTOSL .AND. .NOT. WTADJFINAL) &
                CALL UCODE_UEV_CALCWT(IFAIL,IOUT,ITERP,MAXITER,TF,DTWT)
            IF ((CTRLJOB(ICTRL) == 'FORWARD' .OR. &
                                    CTRLJOB(ICTRL) == 'FORWARD&SENS') &
                .AND. WTOSL .AND. WTADJFINAL)WRITE(IOUT,6)SAVEWTFINAL
            IF(IFO>0) THEN
              IF(ICNT == 2 .OR. TRUSTREGION) THEN
                CALL UCODE_EVA_LOWEST(IFAIL,IOUT,ITERP,WTOSL,UNUSUAL)
                TF = .TRUE.
                IF (WTOSL .AND. OPTIMIZE) &
                    CALL UCODE_UEV_CALCWT(IFAIL,IOUT,ITERP,MAXITER,TF,DTWT)
                IF(SVDLOOP==2) THEN
                  CALL UCODE_FINAL_SVD(IOUT,MPR,NPETMP,NPSTMP,SVDLOOP, &
                                        NPENOW,NPSNOW)
                ENDIF
                LOWEST = .FALSE.
                ICNT = 3
                IF(SVDLOOP==2) EXIT CONTROL
              ENDIF
            ENDIF
            IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
            ! IF LIMITS ARE BEING CALCULATED CHECK FOR CONVERGENCE
            IF(OPTNLUNC) THEN
              IF(ITERP > 0) THEN
                CALL REG_GN_INIUNC (IOUT,INTERVALLOOPS,ITERP, &
                                NOBS,NPE,NPS,MODELVAL,PVAL, &
                                RESIDS,WTFULL,XSENST,IINT)
                CALL REG_GN_UNC(IFAIL,IOUT,IINT, &
                                 MPR,NPE,NPS,.FALSE., &
                                 BSCAL,IPTR,LN,PARNAM, &
                                 PRIWTMAT,PVALINIT, &
                                 RESIDSUNC,RESIDSPRI,RSQALL, &
                                 FINALSTATS,WTFULLUNC,XPRI, &
                                 FL,GNUDONE,IEND,IFO,IND,PVAL,XSENSTUNC,ZZ)
                IF(IFO > 0) THEN
                  CALL UCODE_UEV_CALCINTWT(MPR,NPE,IOUT,MPRWOP,OUTNAM, &
                                           OUTNAMPRED,RESIDSPRI)
                  INTDONE = .TRUE.
                  ITERP = 0
                  IFO = 0
                  CYCLE
                ENDIF
              ENDIF
            ! CHECK FOR CONVERGENCE BY SS
            ELSEIF(TOLSOSC > 0.D0) THEN
              IF (IFO == 0 .AND. ITERP > 3) THEN
                ! Note RSQs TRACK a value for initial parameters at iterp = 0 as
                ! the first value in the array, thus the increment below
                CHECKTOLSOSC = &
                            ABS((RSQALL(ITERP-2)-RSQALL(ITERP+1))/RSQALL(ITERP-2))
                CHECKTOLSOSC1 = &
                            ABS((RSQALL(ITERP-1)-RSQALL(ITERP+1))/RSQALL(ITERP-2))
                IF(CHECKTOLSOSC < TOLSOSC .AND. CHECKTOLSOSC1 < TOLSOSC) IFO = 2
              ENDIF
            ENDIF
          ENDIF
        !  Calculate sensitivities and determine whether to print
        ELSEIF (CTRLJOB(ICTRL) .EQ. 'SENSITIVITY' .OR. &
            (CTRLJOB(ICTRL)=='FORWARD&SENS' .AND. SENSITIVITIES)) THEN
          !   Populate the XSENST sensitivity matrix
          IF (ISNPRT==0) THEN
            ! IF FILE MERGEING FOR DERIVATIVES INTERFACE IS REQUESTED, MERGE NOW
            !   Sensitivities are model-calculated
            CALL SEN_UEV_POPX_MODCALC(IOUT,NOBS,NPE,NPS,IPTR,ISENMETHOD, &
                                      SENSDONE,XSENST)
          ELSE
            !   Calculate sensitivities by a difference method
            KPTR = 1
            IF (CTRLJOB(ICTRL)=='FORWARD&SENS') KPTR = 2
            DO KPI=1,NPE
              IF (ISENMETHOD(IPTR(KPI))==1) THEN
                CALL SEN_UEV_POPXROW_DIFF(KPI,NOBS,DEPVALSETS(:,KPTR:KPTR), &
                                          MODELVAL,XSENST(KPI:KPI,:))
                IF(ISENCOMB .AND.  LN(IPTR(KPI)) > 0) &
                   XSENST(KPI:KPI,:) = XSENST(KPI:KPI,:)*PVAL(IPTR(KPI))
                KPTR = KPTR+1
              ELSEIF (ISENMETHOD(IPTR(KPI))==2) THEN
                ISENSTAT = ISENSTAT + 1
                CALL SEN_UEV_POPXROW_DIFF(KPI,NOBS,DEPVALSETS(:,KPTR:KPTR), &
                              DEPVALSETS(:,KPTR+1:KPTR+1),XSENST(KPI:KPI,:))
                IF(ISENCOMB .AND.  LN(IPTR(KPI)) > 0) &
                   XSENST(KPI:KPI,:) = XSENST(KPI:KPI,:)*PVAL(IPTR(KPI))
                KPTR = KPTR+2
              ELSEIF (ISENMETHOD(IPTR(KPI)) < 1) THEN
                ISENSTAT = ISENSTAT + 1
              ENDIF
            ENDDO
            SENSDONE = .TRUE. ! May need to be contingent on IDONE statuses?
          ENDIF
          ! SENSITIVITY DONE
          IF (SENSDONE) THEN
            !Save SENSITIVITY and RESIDS FOR REPEAT RUNS GIVEN OPTNLUNC
            IF(OPTNLUNC .AND. .NOT. SENFORINIT) THEN
              IF(MPR > 0) THEN
                CALL UCODE_GEN_INITRESSEN(MPR,NOBS,NPE,0,RESIDSPRI,WTDRESIDSPRI)
              ELSE
                CALL UCODE_GEN_INITRESSEN(MPR,NOBS,NPE,0)
              ENDIF
            ENDIF
            !CONVERT SENSITIVITY FOR LOG TRANSFORMED PARAMETERS, PRINT AS DIRECTED
            IF(SVDLOOP /= 2) & ! SVD PARAMETERS ARE NEVER TRANSFORMED
              CALL SEN_UEV_LNX(NOBS,NPE,NPS,IPTR,LN,PVAL,XSENST,ICONVERT_DERIV)
            CALL UCODE_UEV_SENS_PRINT_DECISION &
              (IFAIL,FINALSTATS,IOUT,ITERP,MPR,MPRWOP,NPSNOW,OUTNAM, &
               OUTNAMPRED,PRIWTMAT,PRIWTMATSQR,TRUSTREGION)
            IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
          ENDIF
        ELSEIF (CTRLJOB(ICTRL) .EQ. 'PRIORSENS') THEN
        !   Populate the varying PRI sensitivity matrix
        !   Nothing is done here yet only linear prior is used
        ENDIF
        ! ************************** END OF CONTROL LOOP *************************
        !
        IF(LOWEST) FINALSTATS = .TRUE.
      ENDDO CONTROL
      !
      IF(SVDT .AND. SVDLOOP == 1 .AND. SVDPHASE1 .AND. ((NOBS-2) >= NPE)) THEN
        CALL UCODE_EVA(IFAIL,IOUT,ITERP,NDOBS,MAXITER,MPR, &
          NONDETECTL,OUTNAM,PRINAM,PRIVAL,PRIWTMAT,PRIWTMATSQR, &
          STATS_ON_NONCONVERGE,SVDLOOP,TRUSTREGION,AVET,IFO,IND,MODELPRIVAL, &
          RESIDSPRI,UNUSUAL,WTDRESIDSPRI,WTFULLSQR)
      ENDIF
    ENDDO SVDCYCLE
    ! ****************************************************************************
    ! ******************************* EVALUATE ***********************************
    ! ****************************************************************************
    !
    ! IF REGRESSION CONVERGED, &
    ! SENSITIVITIES HAVE BEEN CALCULATED FOR FINAL VALUES,
    ! CALCULATE AND PRINT FINAL STATISTICS
    !
    IF(.NOT. PREDICT) THEN
      IF(.NOT. OPT .AND. (SENSITIVITIES .AND. ISENSTAT .EQ. NPE)) THEN
        SENSDONE = .TRUE.
        ITERP = 1
        IND = 0
      ENDIF
      IF((FINALSTATS .AND. SENSDONE) .OR. (SENSITIVITIES)) THEN
        IF(.NOT. OPTNLUNC .AND. ((NOBS-2) >= NPE)) THEN
          CALL UCODE_EVA(IFAIL,IOUT,ITERP,NDOBS,MAXITER,MPR, &
              NONDETECTL,OUTNAM,PRINAM,PRIVAL,PRIWTMAT,PRIWTMATSQR, &
              STATS_ON_NONCONVERGE,SVDLOOP,TRUSTREGION,AVET,IFO,IND,MODELPRIVAL, &
              RESIDSPRI,UNUSUAL,WTDRESIDSPRI,WTFULLSQR)
          IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from MAIN UCODE ')
        ENDIF
        FINALSTATSDONE = .TRUE.
      ENDIF
    ENDIF
    IF(PREDICT) THEN
      DO I=1,NPS
        PVALTMP(I) = PVAL(I)
      ENDDO
    ENDIF
    IF(.NOT. RESIDONLY) THEN
      IF(PREDICT) THEN
        DO I=1,NPS
          PVALTMP(I) = PVAL(I)
        ENDDO
      ELSEIF(SOSSURFACE) THEN
        DO I=1,NPS
          PVALTMP(I) = PVALINIT(I)
        ENDDO
!!  !!!!!!!!!!!!!!    ELSE
!!  !!!!!!!!!!!!!!      ALLOCATE(PINCR(NPS))
!!  !!!!!!!!!!!!!!      CALL UTLUCODE_DX_READ_PAOPT(NPS,OUTNAM,LN,PARNAM,PINCR,PVALTMP,NPSWOP)
!!  !!!!!!!!!!!!!!      DEALLOCATE(PINCR)
!!  !!!!!!!!!!!!!!      IF(NPS > NPSWOP) THEN
!!  !!!!!!!!!!!!!!        J = UTL_DX_OPEN(OUTNAMPRED,'_paoptp','OLD')
!!  !!!!!!!!!!!!!!        READ(J,*)
!!  !!!!!!!!!!!!!!        READ(J,*)(PARNAM(NPSWOP+I),PVALTMP(NPSWOP+I),I=1,NPS-NPSWOP)
!!  !!!!!!!!!!!!!!        CLOSE(J)
!!  !!!!!!!!!!!!!!      ENDIF
!!  !!!!!!!!!!!!!!    ENDIF
!!  !!!!!!!!!!!!!!    CALL MIO_ADA_WRITEFILES(IFAIL,NPSNPD,PARNAMALL,NOPNT,NW,PRECIS,PVALTMP)
!!  !!!!!!!!!!!!!!  ENDIF
        CALL MIO_ADA_WRITEFILES(IFAIL,NPSNPD,PARNAMALL,NOPNT,NW,PRECIS,PVALTMP)
      ELSE
        IF(DATAEXCHANGE) THEN
          ALLOCATE(PINCR(NPS))
          CALL UTLUCODE_DX_READ_PAOPT(NPS,OUTNAM,LN,PARNAM,PINCR,PVALTMP,NPSWOP)
          DEALLOCATE(PINCR)
          IF(NPS > NPSWOP) THEN
            J = UTL_DX_OPEN(OUTNAMPRED,'_paoptp','OLD')
            READ(J,*)
            READ(J,*)(PARNAM(NPSWOP+I),PVALTMP(NPSWOP+I),I=1,NPS-NPSWOP)
            CLOSE(J)
          ENDIF
          CALL MIO_ADA_WRITEFILES(IFAIL,NPSNPD,PARNAMALL,NOPNT,NW,PRECIS,PVALTMP)
        ENDIF
      ENDIF
    ENDIF
    !
    GO TO 999
    998 CALL UTLUCODE_PAR_OMIT(IOUT,NPO,NPS,PARNAM,POMIT)
    ! READ ._papri FILE to determine if prior eqtns have all parameters omitted
    IF(MPRWOP > 0 .AND. PREDICT)THEN
      ALLOCATE(EQTN(NPSWOP,MPRWOP), OMITPRI(MPRWOP), OPINC(NPSWOP), PNAM(NPSWOP))
      CALL UTLUCODE_DX_READ_PAPRI &
         (MPRWOP,NPSWOP,OUTNAM,PNAM,OPINC,EQTN,OMITPRI,SUMOMITPRI)
      IF(SUMOMITPRI > 0)THEN
        CALL UTLUCODE_PRI_OMIT(IOUT,MPRWOP,OMITPRI,PREDICT)
      ENDIF
      DEALLOCATE(EQTN,OMITPRI,OPINC,PNAM)
    ENDIF
  ENDIF ! endif .not. mcmc run
  !*************************************
  ! ******************************* CLEAN UP ***********************************
  !
  999 IF (PARALLEL_ACTIVE) CALL PLL_STOP_RUNNERS()
  CALL UCODE_CLN(IFO,IOUT,MCMC,IFAILHOLD)
  CALL BAS_CLN()
  CALL DEP_CLN()
  CALL EQN_CLN()
  CALL PRI_CLN()
  CALL SEN_CLN()
  CALL STA_CLN()
  CALL PLL_CLN()
  !******************************
  IF (.NOT. MCMC)THEN
    IF(PARAMS_OMITTED .AND. REACT == 0) THEN
      WRITE(*,899)
      WRITE(IOUT,899)
    ENDIF
    IF(OPTIMIZE) THEN
      IF((.NOT. SVDT) .OR. SVDPHASE1) THEN
        WRITE(IOUT,885)NUMMODRUNS-NUMFINSTAT-NUMFINSTAT2-NUMFINSTAT3-NUMSVDRUNS
        WRITE(IOUT,886)NUMFINSTAT
      ENDIF
      IF(SVDT)WRITE(IOUT,888)NUMSVDRUNS
      IF(SVDT)WRITE(IOUT,887)NUMFINSTAT2+NUMFINSTAT3
      WRITE(IOUT,889)NUMMODRUNS
    ELSE
      WRITE(IOUT,890)NUMMODRUNS
    ENDIF
    IF(OPTIMIZE) THEN
      WRITE(IOUT,891)
      IF(ISENANY)WRITE(IOUT,892)
    ENDIF
    IF(NORMAL .AND. (.NOT. UNUSUAL)) THEN
      WRITE(*,900)VERSION
      WRITE(IOUT,900)VERSION
    ELSEIF(UNUSUAL) THEN
      WRITE(*,902)VERSION
      WRITE(IOUT,902)VERSION
    ELSE ! must be not normal
      WRITE(*,901)VERSION
      WRITE(IOUT,901)VERSION
    ENDIF
  ENDIF ! endif .not. mcmc run
  !*************************************
  CALL UTL_ENDTIME(IBDT,IOUT)
  CLOSE(UNIT=INUNIT)
  CLOSE(UNIT=IOUT)
  CALL UTL_STOP(' ')
  END PROGRAM UCODE
