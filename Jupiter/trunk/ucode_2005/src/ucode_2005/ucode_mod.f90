MODULE UCODEMOD
  !!
  USE DATATYPES
  !
  USE GLOBAL_DATA
  !
  USE DEPENDENTS, ONLY: &
      ! subroutines
      DEP_UEV_DX_WRITE_SS,   &
      ! variables
      NONDETVAL, NTOTOBS, NUSEOBS, NUSEPRED, OBSVAL, GROUP, WTCORRELATED, WTCOS
  !
  USE PRIOR_INFORMATION, ONLY: &
     ! subroutines
      PRI_UEV_DX_WRITE_PR, &
      ! variables
      PLOTSYMBOLPRI, PRILN, PRINAM, PRIWTCORR
  !
  USE SENSITIVITY, ONLY: &
      ! subroutines
      SEN_UEV_DX_WRITE_MATRIX, SEN_UEV_WRITESENTABLE
  !
  USE STATISTICS, ONLY: &
      ! subroutines
      STA_UEV_INIT, STA_UEV_FIT, STA_UEV_DX_READ_DM, STA_UEV_DX_WRITE_DM, &
      STA_UEV_DX_WRITE_NM
  !
  USE UTILITIES
  !
  USE UTLUCODE
  !
  USE REG_GN_UCODE, ONLY: &
      ! subroutines
      REG_GN_CLN, &
      ! variables
      ALTSTART, GNAMPRTS, MINSENRAT, NTYP, NUMINTERVALS, &
      OMITIT, OMIT_DEFAULT, OMIT_INSENS, &
      PAREST, PINC, PRNTA, PRNTB, PRNTJ, STATS_ON_NONCONVERGE
  !
  USE REG_GNMOD, ONLY:  &
      REG_GNMOD_UNCINI, &
      REG_GNMOD_EVA_FINAL_OUT, REG_GNMOD_EVA_FINISH, &
      REG_GNMOD_EVA_MLOFSTATS, &
      REG_GNMOD_EVA_ORD_NORM_RESIDS, &
      REG_GNMOD_EVA_SUMMARY, &
  ! variables
      CMAT, IUWRP, NPERD, NPARSTAR, NPTR, &
      WTADJFINAL, WTADJNEXT, XPTR
  !
  IMPLICIT NONE
  SAVE
  PRIVATE
  ! Public subprograms
  PUBLIC &
    UCODE_INI, UCODE_INI_ALLOC, UCODE_INI_CHECK_USEFLAG, &
    UCODE_INI_FILEMANAGE, UCODE_INI_PARNAMALL, UCODE_INI_PARS, &
    UCODE_INI_PARS_INSTALL, UCODE_INI_SET, UCODE_DEF, &
    UCODE_GEN_INITRESSEN, UCODE_GEN_LINEARITY, UCODE_GEN_LINEARITY_ADV, &
    UCODE_GEN_DERPARWRITE, &
    UCODE_GEN_PREDICT,  &
    UCODE_EXE_SELECT,   &
    UCODE_UEV_RESIDUALS, UCODE_GEN_SOSSURF, &
    UCODE_UEV_CALCWT, UCODE_UEV_CSS, &
    UCODE_UEV_LINEARITY, UCODE_UEV_LINEARITY_ADV, UCODE_UEV_OMIT,  &
    UCODE_UEV_PRINT_PREDICT, UCODE_UEV_RESID_PRINT_DECISION, &
    UCODE_UEV_SENS_PRINT_DECISION, UCODE_UEV_MERGE,  UCODE_UEV_CALCINTWT, &
    UCODE_GEN_PVALALL, &
    UCODE_EVA, UCODE_EVA_LOWEST, UCODE_CLN, &
  !   Public data
    BSCAL, CONSTRAIN, CONSTRAINL, CSS, DATAEXCHANGE, &
    ICONVERT_DERIV, INSFILESE, ILOW, INTWRVALS, IOMIT, IPTR, &
    ISENANY, ISENMETHOD, ITRANS, IUITER, IWRITE,  &
    LINEARITY, LINEARITYADV, LCOR, LN, LOWEST, LOWEST_DIFFERS, LOWEST_MOD, &
    MAG_PERTURB, MCVCAT, MCVUSE, MODELVAL, &
    NCATS, NCOVMAT, NMERGE, NOBS, NOINT, &
    NONLININT, NONLINPARAM, CREATEINITFILES, &
    NOPNT, NPD, NPE, NPEFP, NPO, NPS, NPSWOP, NPSNPD, &
    NPERTURB_CYCLES, NPERTURB_CYCLES_FINAL, NW, &
    OBSNAM, OPT, OPTIMIZE, OPTNLUNC, &
    PADJ, PARAM_OMIT, PARAMS_OMITTED, PARNAM, PARNAMALL, PARNAMLC, &
    PERTURB, PMAXCHANGE, PNPERD, PNPTR, POMIT, PRECIS, &
    PREDICT, PRED_MODADV, PTOLPAR, &
    PVAL, PVALINIT, PVALMAX, PVALMAXC, PVALMIN, PVALMINC, &
    PXSENSTRD, REACT, &
    RESIDS, RESIDONLY, RSQALL, RSQD, SAVEWTFINAL, SENFORINIT, SENSITIVITIES, &
    SOSPARNAM, SOSPTRNPE, &
    SOSSURFACE, SOSSURFDONE, STDERRONE, &
    WRPAREQ, WRPRIEQ, WTDRESIDS, WTFULL, WTFULLSQR, &
    XPRI, XPRIFILLED, XSENST
  !
  INTEGER, ALLOCATABLE, DIMENSION(:)                 :: BPTR
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)       :: BSCAL
  DOUBLE PRECISION                                   :: CEVINIT
  CHARACTER (LEN=12)                                 :: CDUM
  LOGICAL, ALLOCATABLE,  DIMENSION(:)                :: CONSTRAIN
  LOGICAL                                            :: CONSTRAINL = .FALSE.
  INTEGER, ALLOCATABLE,  DIMENSION(:)                :: COUNTER
  DOUBLE PRECISION,  ALLOCATABLE,  DIMENSION(:)      :: CSS
  LOGICAL                                            :: DATAEXCHANGE
  LOGICAL                                            :: DEFAULT = .TRUE.
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: DERPAREQN
  CHARACTER(LEN=12), ALLOCATABLE,  DIMENSION(:)      :: DERPARNAME
  CHARACTER(LEN=12), ALLOCATABLE,  DIMENSION(:)      :: DERPARNAMELC
  INTEGER, ALLOCATABLE,  DIMENSION (:)               :: EPTR
  CHARACTER(LEN=4)                                   :: EXTB
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: FILES
  INTEGER                                            :: FINALRES
  INTEGER                                            :: FINALSENS
  CHARACTER(LEN=210), ALLOCATABLE,  DIMENSION(:)     :: FNITER
  INTEGER, ALLOCATABLE, DIMENSION(:)                 :: GNAMPTR
  INTEGER, ALLOCATABLE, DIMENSION(:)                 :: GNAMPTRTYP
  INTEGER, ALLOCATABLE,  DIMENSION(:)                :: ICONVERT_DERIV
  INTEGER                                            :: IDUM
  INTEGER                                            :: IINTCNT = 0
  INTEGER                                            :: ILOW = 1
  INTEGER, ALLOCATABLE,  DIMENSION (:)               :: INCLU
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)       :: INITMODVAL
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)       :: INITRESID
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)       :: INITWTDRESID
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)       :: INITRESIDPRI
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)       :: INITWTDRESIDPRI
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)     :: INITXSEN
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)     :: INITXPRI
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)     :: INTWRVALS
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)      :: SAVEWT
  INTEGER                                            :: SAVEWTFINAL = 1
  INTEGER                                            :: STARTRES
  INTEGER                                            :: STARTSENS
  CHARACTER(LEN=180), ALLOCATABLE,  DIMENSION(:)     :: INSFILESE               ! Instruction files for simulated equivalents
  INTEGER                                            :: INTERMEDRES
  INTEGER                                            :: INTERMEDSENS
  INTEGER                                            :: IOMIT
  LOGICAL, DIMENSION(4)                              :: IPRINT
  INTEGER, ALLOCATABLE,  DIMENSION(:)                :: IPTR                    ! Position of parameter in full set
  LOGICAL                                            :: ISENANY
  INTEGER, ALLOCATABLE,  DIMENSION(:)                :: ISENMETHOD
  INTEGER, ALLOCATABLE,  DIMENSION(:)                :: ISENMETHODPRT
  INTEGER, ALLOCATABLE,  DIMENSION(:)                :: ITRANS
  INTEGER                                            :: IUB1
  INTEGER                                            :: IUB2
  INTEGER                                            :: IUB3
  INTEGER                                            :: IUB4
  INTEGER                                            :: IUITER = 0
  INTEGER                                            :: IUSOS = 0
  INTEGER                                            :: IUSOSFILE = 0
  LOGICAL                                            :: IWRITE
  INTEGER                                            :: KPE
  INTEGER                                            :: KPERD
  DOUBLE PRECISION                                   :: LAMBDA
  LOGICAL                                            :: LCOR = .FALSE.
  CHARACTER(LEN=6)                                   :: LINADVANS
  LOGICAL                                            :: LINADVCONF
  LOGICAL                                            :: LINADVPRED
  LOGICAL                                            :: LINEARITY
  LOGICAL                                            :: LINEARITYADV
  INTEGER, ALLOCATABLE,  DIMENSION(:)                :: LN                      ! >0 for log-transformed parameters
  INTEGER                                            :: LPRINT
  LOGICAL                                            :: LOWEST = .FALSE.
  LOGICAL                                            :: LOWEST_DIFFERS = .FALSE.
  LOGICAL                                            :: LOWEST_MOD = .FALSE.
  DOUBLE PRECISION,  ALLOCATABLE,  DIMENSION(:)      :: MAG_PERTURB             ! Magnitude of each parameters perturbation
  INTEGER                                            :: NCATS
  PARAMETER (NCATS=2)
  CHARACTER(LEN=6),  DIMENSION(NCATS)                :: MCVCAT
  DATA MCVCAT/'OBS   ','PRED  '/
  LOGICAL,  DIMENSION(NCATS)                         :: MCVUSE
  DATA MCVUSE/.FALSE.,.FALSE. /
  CHARACTER(LEN=MAX_STRING_LEN)                      :: MERGEPATH
  DOUBLE PRECISION                                   :: MINWT
  CHARACTER(LEN=12)                                  :: MODELLENGTH
  CHARACTER(LEN=12)                                  :: MODELMASS
  CHARACTER(LEN=12)                                  :: MODELNAME
  CHARACTER(LEN=12)                                  :: MODELTIME
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)       :: MODELVAL                ! Simulated equivalent
  CHARACTER(LEN=180), ALLOCATABLE,  DIMENSION(:)     :: MODOUTFILESE            ! Model output files for simulated equivalents
  CHARACTER(LEN=1), ALLOCATABLE,  DIMENSION(:)       :: MRKDELSE
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)     :: MVALS
  INTEGER                                            :: NCOVMAT
  INTEGER                                            :: NMERGE
  INTEGER                                            :: NNEGT
  INTEGER                                            :: NOBS                    ! Number of observations
  INTEGER                                            :: NOBSINC
  INTEGER                                            :: NOINT = 0
  LOGICAL,          ALLOCATABLE,  DIMENSION(:)       :: NONLININT
  LOGICAL                                            :: NONLINPARAM = .FALSE.
  INTEGER                                            :: NPARI
  LOGICAL                                            :: CREATEINITFILES
  INTEGER                                            :: NOPNT
  INTEGER                                            :: NPD
  INTEGER                                            :: NPDFP = 0
  INTEGER                                            :: NPDWOP = 0
  INTEGER                                            :: NPE
  INTEGER                                            :: NPEFINAL
  INTEGER                                            :: NPEFP = 0
  INTEGER                                            :: NPERTURB_CYCLES
  INTEGER                                            :: NPERTURB_CYCLES_FINAL
  INTEGER                                            :: NPFP = 0
  INTEGER                                            :: NPO
  INTEGER                                            :: NPOST
  INTEGER                                            :: NPREDI
  INTEGER                                            :: NPS                     ! Number of parameters
  INTEGER                                            :: NPSWOP
  INTEGER                                            :: NPSFP = 0
  INTEGER                                            :: NPSNPD
  INTEGER                                            :: NPSOS
  INTEGER                                            :: NRUNS
  INTEGER                                            :: NUMOUTFILESE            ! Number of output files for simulated equivalents
  INTEGER,          ALLOCATABLE,  DIMENSION(:)       :: NW                 ! Ptr to head of first list (observations)
  CHARACTER(LEN=LENDNAM), ALLOCATABLE,  DIMENSION(:) :: OBSNAM
  CHARACTER(LEN=LENDNAM), ALLOCATABLE,  DIMENSION(:) :: OBSNAMB
  INTEGER, ALLOCATABLE,  DIMENSION(:)                :: OMIT
  INTEGER                                            :: OMITTEDNP
  LOGICAL                                            :: OPT = .FALSE.
  LOGICAL                                            :: OPTIMIZE
  LOGICAL                                            :: OPTNLUNC
  LOGICAL,           ALLOCATABLE,  DIMENSION(:)      :: PADJ                    ! Is parameter adjustable? yes/no
  LOGICAL                                            :: PARAM_OMIT = .FALSE.
  LOGICAL                                            :: PARAMS_OMITTED = .FALSE.
  CHARACTER(LEN=12), ALLOCATABLE,  DIMENSION(:)      :: PARGP                   ! Parameter groups
  CHARACTER(LEN=12), ALLOCATABLE,  DIMENSION(:)      :: PARNAM                  ! Parameter names
  CHARACTER(LEN=12), ALLOCATABLE,  DIMENSION(:)      :: PARNAMALL
  CHARACTER(LEN=12), ALLOCATABLE,  DIMENSION(:)      :: PARNAMLC                ! Parameter names  lowercase
  LOGICAL,           ALLOCATABLE,  DIMENSION(:)      :: PASSIGNED
  CHARACTER(LEN=MAX_STRING_LEN)                      :: PATHTOMERGEDFILE
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)       :: PERTURB
  INTEGER, ALLOCATABLE,  DIMENSION(:)                :: PINCFP
  INTEGER, ALLOCATABLE,  DIMENSION(:)                :: PINCR
  INTEGER, ALLOCATABLE, DIMENSION(:)                 :: PINCSEN
  INTEGER, ALLOCATABLE, DIMENSION(:)                 :: PINCBND
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)       :: PMAXCHANGE
  INTEGER                                            :: PNPERD
  INTEGER, ALLOCATABLE,  DIMENSION(:)                :: PNPTR
  INTEGER, ALLOCATABLE,  DIMENSION(:)                :: POMIT
  INTEGER                                            :: PRECIS
  LOGICAL                                            :: PRED_MODADV = .FALSE.               ! Precision protocol  ase for   _   files
  LOGICAL                                            :: PREDICT = .FALSE.
  LOGICAL                                            :: PREDICTION = .FALSE.
  INTEGER, ALLOCATABLE,  DIMENSION(:)                :: PRODUCTS
  CHARACTER(LEN=1), ALLOCATABLE,  DIMENSION(:)       :: PSTATUS
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)        :: PTMP
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)       :: PTOLPAR
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)       :: PVAL
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)       :: PVALINIT
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)       :: PVALMAX
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)       :: PVALMAXC
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)       :: PVALMIN
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)       :: PVALMINC
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)     :: PVALS
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)     :: PVALS1
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)     :: PVALS3
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)       :: PVALSET
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)       :: PVALTMP
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)     :: PXSENSTRD
  INTEGER                                            :: REACT = 2
  DOUBLE PRECISION                                   :: RDUM
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)       :: RESIDS
  LOGICAL                                            :: RESIDONLY = .FALSE.
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION (:)      :: RSQALL
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION (:)      :: RSQD
  LOGICAL                                            :: SENFORINIT = .FALSE.
  LOGICAL                                            :: SENSITIVITIES
  CHARACTER(LEN=3)                                   :: SENTYPE = " NO"
  INTEGER, ALLOCATABLE, DIMENSION(:)                 :: SKIPLINES
  DOUBLE PRECISION                                   :: SMALLDOUBLE
  CHARACTER(LEN=MAX_STRING_LEN)                      :: SOSFILE
  LOGICAL                                            :: SOSFILEL
  INTEGER, ALLOCATABLE,  DIMENSION (:)               :: SOSINCREMENT
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)       :: SOSPARNAM
  INTEGER, ALLOCATABLE,  DIMENSION (:)               :: SOSPTRNPE
  LOGICAL                                            :: SOSSURFACE
  CHARACTER(LEN=4)                                   :: SOSSURFACEINPUT
  LOGICAL                                            :: SOSSURFDONE = .FALSE.
  INTEGER                                            :: SOSSURFLOOPS = 0
  LOGICAL                                            :: STDERRONE
  INTEGER                                            :: TOTSOSLOOPS
  LOGICAL,          ALLOCATABLE,  DIMENSION(:)       :: TRANSFORM
  LOGICAL                                            :: WRPAREQ = .TRUE.
  LOGICAL                                            :: WRPRIEQ = .TRUE.
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)       :: WTDRESIDS               ! Weighted residuals
  TYPE (CDMATRIX)                                    :: WTFULL                  ! Weight matrix for all observations
  TYPE (CDMATRIX)                                    :: WTFULLSQR               ! Square-root of weight matrix for all observations
  DOUBLE PRECISION                                   :: WTRL                    ! weighted residuals
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)     :: XPRI                    ! Sens of params for which prior info specified
  LOGICAL                                            :: XPRIFILLED
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:)     :: XSENST
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!! BLOCK PARAMETERS !!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !   For PARAMETER GROUPS input, do not define a default order
  INTEGER,           PARAMETER                      :: NPARGPCOLS = 0
  CHARACTER(LEN=40), DIMENSION(1), TARGET           :: PARGPCOL = (/' '/)
  !   For PARAMETER GROUPS FOR PREDICITON input, do not define a default order
  INTEGER,           PARAMETER                      :: NPARGPFPCOLS = 0
  CHARACTER(LEN=40), DIMENSION(1), TARGET           :: PARGPFPCOL = (/' '/)
  !
  !   For PARAMETER DATA input, define default column order
  INTEGER,           PARAMETER                      :: NPARCOLS = 17
  CHARACTER(LEN=40), DIMENSION(NPARCOLS), TARGET    :: PARCOL = &
      (/ 'PARAMNAME        ','GROUPNAME        ','STARTVALUE       ', &
         'LOWERVALUE       ','UPPERVALUE       ','CONSTRAIN        ', &
         'LOWERCONSTRAINT  ','UPPERCONSTRAINT  ','ADJUSTABLE       ', &
         'PERTURBAMT       ','TRANSFORM        ','TOLPAR           ', &
         'MAXCHANGE        ','SENMETHOD        ','SCALEPVAL        ', &
         'SOSINCREMENT     ','NONLINEARINTERVAL'/)
  !
  !   For PARAMETER DATA FOR PREDICTION input, define default column order
  INTEGER,           PARAMETER                      :: NPARFPCOLS = 17
  CHARACTER(LEN=40), DIMENSION(NPARFPCOLS), TARGET    :: PARFPCOL = &
      (/ 'PARAMNAME        ','GROUPNAME        ','STARTVALUE       ', &
         'LOWERVALUE       ','UPPERVALUE       ','CONSTRAIN        ', &
         'LOWERCONSTRAINT  ','UPPERCONSTRAINT  ','ADJUSTABLE       ', &
         'PERTURBAMT       ','TRANSFORM        ','TOLPAR           ', &
         'MAXCHANGE        ','SENMETHOD        ','SCALEPVAL        ', &
         'SOSINCREMENT     ','NONLINEARINTERVAL'/)
  !
  !   For PARAMETER VALUES input, define default column order
  INTEGER,           PARAMETER                      :: NPVCOLS = 2
  CHARACTER(LEN=40), DIMENSION(NPVCOLS), TARGET     :: PVCOL = &
      (/ 'PARAMNAME   ','STARTVALUE  '/)
    !   For DERIVED PARAMETER VALUES input, define default column order
  INTEGER,           PARAMETER                      :: NPDCOLS = 2
  CHARACTER(LEN=40), DIMENSION(NPDCOLS), TARGET     :: PDCOL = &
      (/ 'DERPARNAME  ','DERPAREQN   '/)
    !   For DERIVED PARAMETER VALUES FOR PREDICTION input, define default column order
  INTEGER,           PARAMETER                      :: NPDFPCOLS = 2
  CHARACTER(LEN=40), DIMENSION(NPDFPCOLS), TARGET     :: PDFPCOL = &
      (/ 'DERPARNAME  ','DERPAREQN   '/)
    ! For OUTput Files
  INTEGER, PARAMETER :: NMODOUTCOLS = 2
  CHARACTER(LEN=40), DIMENSION(NMODOUTCOLS), TARGET :: MODOUTCOL = &
      (/ 'MODOUTFILE','INSFILE   ' /)
  !
  INTEGER :: IDEPTYPEU  ! 1-observations; 2-predictions
  !
CONTAINS
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_INI (IFAIL,INUNIT,IOUT,OUTNAM,IDEPTYPE)
  !*****************************************************************************
  !     INITIALIZE PARAMETERS FOR UCODE
  !     Read, store, initialize and echo output controls
  !*****************************************************************************
  !        SPECIFICATIONS:
  !------------------------------------------------------------------
    USE BASIC, ONLY: LLPTROPT
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                                     INTENT(INOUT)  :: IFAIL
    INTEGER,                                     INTENT(IN)  :: INUNIT
    INTEGER,                                     INTENT(IN)  :: IOUT
    CHARACTER(LEN=MAX_STRING_LEN),               INTENT(IN)  :: OUTNAM
    INTEGER,                                     INTENT(OUT) :: IDEPTYPE
    !   Local variables
    TYPE (LLIST), POINTER :: UCODEHEAD  ! Ptr to head of list (UCODE cntrls)
    TYPE (LLIST), POINTER :: TAIL      ! Ptr to tail of a list
    CHARACTER(LEN=40), DIMENSION(2) :: MERGECOL = (/ '      ','      '/)
    TYPE (LLIST), POINTER :: MERGEHEAD  ! Ptr merge
    TYPE (LLIST), POINTER :: MERGETAIL  ! Ptr to tail merge
    CHARACTER(LEN=40), DIMENSION(1) :: UCODECOL = (/ '      '/)
    INTEGER :: I, NUCODE
    CHARACTER(LEN=79), DIMENSION(0:1) :: IMESS
    CHARACTER(LEN=79), DIMENSION(0:1) :: PMESS
    CHARACTER(LEN=79), DIMENSION(0:1) :: FMESS
    CHARACTER(LEN=79), DIMENSION(0:6) :: ISMESS
    CHARACTER(LEN=79), DIMENSION(0:6) :: PSMESS
    CHARACTER(LEN=79), DIMENSION(0:6) :: FSMESS
    CHARACTER(LEN=79) TMP
    !
    !   Declare variables (and default values) to allow use of UTL_FILTER
    INTEGER           :: IERR = 0
    INTEGER           :: MORE = 0
    CHARACTER(LEN=79) :: STARTRESC = 'yes'
    CHARACTER(LEN=79) :: INTERMEDRESC = 'no'
    CHARACTER(LEN=79) :: FINALRESC = 'yes'
    CHARACTER(LEN=79) :: STARTSENSC = 'dss'
    CHARACTER(LEN=79) :: INTERMEDSENSC = 'none '
    CHARACTER(LEN=79) :: FINALSENSC = 'dss '
    LOGICAL           :: EIGENVALUES = .FALSE.
    CHARACTER(LEN=79) :: REACTIVATE = 'final'
    !   For MERGE FILES input
    INTEGER,           PARAMETER                      :: NMRGCOLS = 2
    !
    DATA (IMESS(I),I=0,1)/&
    'STARTRES - Residuals will not be written at initial parameter values', &
    'STARTRES - Residuals written at initial parameter values'/
    DATA (PMESS(I),I=0,1)/&
    'INTERMEDRES - Residuals will not be written between iterations', &
    'INTERMEDRES - Residuals written between iterations'/
    DATA (FMESS(I),I=0,1)/&
    'FINALRES - Residuals will not be written at final parameter values', &
    'FINALRES - Residuals written at final parameter values'/
    DATA (ISMESS(I),I=0,6)/&
    'STARTSENS Print composite scaled sensitivities at starting values', &
    'STARTSENS Print dimensionless scaled sensitivities at starting values', &
    'STARTSENS Print unscaled sensitivities at starting values', &
    'STARTSENS Print one-percent sensitivities at starting values', &
    'STARTSENS Print 1%& dimensionless sensitivities at starting values', &
    'STARTSENS Print all types of sensitivities at starting values', &
    'STARTSENS Do not print any sensitivities at starting values'/
    DATA (PSMESS(I),I=0,6)/&
    'INTERMEDSENS Print composite scaled sensitivities at intermed values', &
    'INTERMEDSENS Print dimensionless scaled sensitivities at intermd values', &
    'INTERMEDSENS Print unscaled sensitivities at intermed values', &
    'INTERMEDSENS Print one-percent sensitivities at intermed values', &
    'INTERMEDSENS Print 1%& dimensionless sensitivities at intermed values', &
    'INTERMEDSENS Print all types of sensitivities at intermed values', &
    'INTERMEDSENS Do not print any sensitivities at intermed values'/
    DATA (FSMESS(I),I=0,6)/&
    'FINALSENS Print composite scaled sensitivities at final values', &
    'FINALSENS Print dimensionless scaled sensitivities at final values', &
    'FINALSENS Print unscaled sensitivities at final values', &
    'FINALSENS Print one-percent sensitivities at final values', &
    'FINALSENS Print 1%& dimensionless sensitivities at final values', &
    'FINALSENS Print all types of sensitivities at final values', &
    'FINALSENS Do not print any sensitivities at final values'/
    !
    !   Format statements
    100 FORMAT(1X,A)
    101 FORMAT(//,79('-'))
    102 FORMAT(1X,A,2X,A)
    103 FORMAT(1X,'!!! ERROR !!!')
    150 FORMAT(1X,'ECHO UCODE CONTROLS INPUT:',/)
    200 FORMAT(' !!! CANNOT PREDICT AND EVALUATE NONLINEAR CONFIDENCE !!!',/, &
               ' !!!         INTERVALS IN THE SAME EXECUTION          !!!')
    210 FORMAT(' !! CANNOT OPTIMIZE AND EVALUATE NONLINEAR CONFIDENCE !!!',/, &
               ' !!!         INTERVALS IN THE SAME EXECUTION          !!!')
    220 FORMAT(' !! CANNOT PREPARE LINEARITY AND EVALUATE NONNLINEAR !!!',/, &
               ' !!!   CONFIDENCE INTERVALS IN THE SAME EXECUTION    !!!')
    230 FORMAT(' !!! CANNOT CALCULATE SOSsurface at the same time as:',/, &
               ' !!! OPTIMIZING, PREDICTING OR PREPARING FOR LINEARITY',/, &
               ' !!! CALCULATION, OR EVALUATING NONLINEAR CONFIDENCE INTERVALS')
    300 FORMAT(/,3X, &
    'REACTIVATION HAS BEEN REQUESTED IN THE UCODE_CONTROL_DATA BLOCK',/,5X, &
    'EITHER BY USER SPECIFICATION OR BY DEFAULT:',/,5X, &
    'IF ANY PARAMETERS ARE OMITTED FROM THE ITERATION WITH THE LOWEST',/,5X, &
    'OBJECTIVE FUNCTION VALUE DUE TO USER REQUESTED LIMITS ON',/,5X, &
    'SENSITIVITY OR CONSTRAINTED PARAMETER VALUES, CONSEQUENTLY',/,5X, &
    'ALL ADJUSTABLE PARAMETERS WILL BE REACTIVATED FOR CALCULATION OF',/,5X, &
    'FINAL SENSITIVITIES AND STATISTICS.')
    301 FORMAT(3X, &
    'REACTIVATE = starting IN THE UCODE_CONTROL_DATA BLOCK:',/,5X, &
    'STARTING VALUES WILL BE SUBSTITUTED FOR PARAMETERS ARE OMITTED',/,5X, &
    'FROM THE ITERATION WITH THE LOWEST OBJECTIVE FUNCTION VALUE',/)
    302 FORMAT(3X, &
    'REACTIVATE = final IN THE UCODE_CONTROL_DATA BLOCK:',/,5X, &
    'PARAMETER VALUES FROM THE ITERATION WITH THE LOWEST OBJECTIVE',/,5X, &
    'FUNCTION VALUE WILL BE SUBSTITUTED FOR PARAMETERS THAT WERE OMITTED.',/)
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_INI'
    IFAIL = 0
    NULLIFY(UCODEHEAD)
    NULLIFY(TAIL)
    NULLIFY(MERGEHEAD)
    NULLIFY(MERGETAIL)
    NUCODE = 0
    NMERGE = 0
    !
    !   Read input block type OPTIONS
    !   First Set defaults
    LINEARITY = .FALSE.          ! default perfrom regression
    LINEARITYADV = .FALSE.          ! default perfrom regression
    LINADVANS = 'NO'
    LINADVCONF = .FALSE.
    LINADVPRED = .FALSE.
    FINALRES  = 1          ! assume full printing at end of regression
    FINALSENS  = 1           ! if sensitivities, default print dimensionless
    DATAEXCHANGE = .TRUE.    ! assume graphing files will be desired
    STARTRES  = 1           ! assume full printing at start of regression
    STARTSENS  = 1            ! if sensitivities, default print dimensionless
    INTERMEDRES = 0        ! assume minimal printing in middle of regression
    INTERMEDSENS  = 0        ! if sensitivities, default print dimensionless
    IOMIT = 0
    IPRINT = .TRUE.          ! default print everything
    IWRITE = .TRUE.          ! default print everything
    LPRINT = 0               ! default print eigenvalues and eigenvectors
    MINWT = 0.D0
    MODELLENGTH = 'NA'
    MODELMASS = 'NA'
    MODELNAME = 'GENERIC'
    MODELTIME = 'NA'
    NCOVMAT = 0
    NNEGT = 0
    NOBSINC = 0
    CREATEINITFILES = .FALSE.
    NPOST = 0
    NRUNS = 0
    OPTIMIZE = .FALSE.          ! default perfrom regression
    OPTNLUNC = .FALSE.
    SOSFILE = ' '
    SOSFILEL = .FALSE.
    SOSSURFACE = .FALSE.
    STDERRONE = .FALSE.          ! default normal field problem
    SENSITIVITIES = .FALSE.     ! default perfrom sensitivity analysis
    PREDICT = .FALSE.           ! default neither prediction mode
    PREDICTION = .FALSE.        ! default not prediction mode
    IDEPTYPE = 1                ! default dependent type: observation
  !
    CALL UTL_READBLOCK(NMRGCOLS,'MERGE_FILES',MERGECOL,INUNIT,IOUT,  &
                       'PATHTOFILE',.FALSE.,MERGEHEAD,MERGETAIL,NMERGE)
    IF (NMERGE>0) THEN
      ALLOCATE(FILES(nmerge),SKIPLINES(nmerge))
      FILES = '_'
      MERGEPATH = TRIM(OUTNAM)//'._sumerge'
      PATHTOMERGEDFILE = TRIM(OUTNAM)//'._sumerge'
      CALL UTL_FILTER(IERR,LLPTROPT,IOUT,'PATHTOMERGEDFILE',PATHTOMERGEDFILE)
      SKIPLINES = 0
      !
      CALL UTL_FILTERLIST(MERGEHEAD,IOUT,'PATHTOFILE',NMERGE,IERR, &
                                                      FILES,MORE,MORE)
      CALL UTL_FILTERLIST(MERGEHEAD,IOUT,'SKIPLINES',NMERGE,IERR, &
                                                      SKIPLINES,MORE)
    ENDIF
  !
    CALL UTL_READBLOCK(0,'UCODE_CONTROL_DATA',UCODECOL,INUNIT,IOUT,  &
                       '*',.FALSE.,UCODEHEAD,TAIL,NUCODE)
    IF (NUCODE>0) THEN
      !
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'OPTIMIZE',OPTIMIZE)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'REACTIVATE',REACTIVATE)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'NONLINEARINTERVALS',OPTNLUNC)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'STDERRONE',STDERRONE)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'MODELNAME',MODELNAME)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'MODELLENGTHUNITS',MODELLENGTH)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'MODELMASSUNITS',MODELMASS)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'MODELTIMEUNITS',MODELTIME)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'SOSFILE',SOSFILE)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'SOSSURFACE',SOSSURFACEINPUT)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'SENSITIVITIES',SENSITIVITIES)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'CREATEINITFILES', &
                                                CREATEINITFILES)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'LINEARITY',LINEARITY)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'LINEARITYADV',LINADVANS)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'PREDICTION',PREDICTION)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'STARTRES',STARTRESC)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'INTERMEDRES',INTERMEDRESC)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'FINALRES',FINALRESC)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'STARTSENS',STARTSENSC)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'INTERMEDSENS',INTERMEDSENSC)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'FINALSENS',FINALSENSC)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'DATAEXCHANGE',DATAEXCHANGE)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'WRITEDERIVEDPARAMS',WRPAREQ)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'WRITEPRIORINFO',WRPRIEQ)
      CALL UTL_FILTER(IERR,UCODEHEAD,IOUT,'EIGENVALUES',EIGENVALUES)
      !
      IF (PREDICTION) THEN
        PREDICT = .TRUE.
        PRED_MODADV = .TRUE.
      ENDIF
      !
      CALL UTL_CASE(LINADVANS,TMP,1)
      IF(TMP == 'CONF') THEN
        LINADVCONF = .TRUE.
        PRED_MODADV = .TRUE.
      ELSEIF(TMP == 'PRED') THEN
        LINADVPRED = .TRUE.
        PRED_MODADV = .TRUE.
      ELSEIF(TMP == 'NO') THEN
        LINADVCONF = .FALSE.
        LINADVPRED = .FALSE.
      ELSE
        AMESSAGE = ' LINEARITYADV must be CONF, PRED, or NO '
        CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','yes')
        CALL UTL_STOP('TERMINATING: LINEARITYADV must be CONF, PRED, or NO ')
      ENDIF
      !
      CALL UTL_CASE(SOSSURFACEINPUT,TMP,1)
      SELECT CASE (TMP)
        CASE ('NO')
          SOSSURFACE = .FALSE.
          SOSFILEL = .FALSE.
        CASE ('YES')
          SOSSURFACE = .TRUE.
          SOSFILEL = .FALSE.
        CASE ('FILE')
          SOSSURFACE = .TRUE.
          SOSFILEL = .TRUE.
          IF (SOSFILE .EQ. ' ') CALL UTL_STOP &
            ('TERMINATING:   SOSSURFACE=FILE, but SOSFILE not defined')
      END SELECT
      !
      CALL UTL_CASE(STARTRESC,TMP,1)
      SELECT CASE (TMP)
        CASE ('NO')
          STARTRES = 0
        CASE ('YES')
          STARTRES = 1
      END SELECT
      !
      CALL UTL_CASE(INTERMEDRESC,TMP,1)
      SELECT CASE (TMP)
        CASE ('NO')
          INTERMEDRES = 0
        CASE ('YES')
          INTERMEDRES = 1
      END SELECT
      !
      CALL UTL_CASE(FINALRESC,TMP,1)
      SELECT CASE (TMP)
        CASE ('NO')
          FINALRES = 0
        CASE ('YES')
          FINALRES = 1
      END SELECT
      CALL UTL_CASE(STARTSENSC,TMP,1)
      SELECT CASE (TMP)
        CASE ('CSS')
          STARTSENS = 0
        CASE ('DSS')
          STARTSENS = 1
        CASE ('UNSCALED')
          STARTSENS = 2
        CASE ('ONEPERCENTSS')
          STARTSENS = 3
        CASE ('ALLSS')
          STARTSENS = 4
        CASE ('ALL')
          STARTSENS = 5
        CASE ('NONE')
          STARTSENS = 6
      END SELECT
      !
      CALL UTL_CASE(INTERMEDSENSC,TMP,1)
      SELECT CASE (TMP)
        CASE ('CSS')
          INTERMEDSENS = 0
        CASE ('DSS')
          INTERMEDSENS = 1
        CASE ('UNSCALED')
          INTERMEDSENS = 2
        CASE ('ONEPERCENTSS')
          INTERMEDSENS = 3
        CASE ('ALLSS')
          INTERMEDSENS = 4
        CASE ('ALL')
          INTERMEDSENS = 5
        CASE ('NONE')
          INTERMEDSENS = 6
      END SELECT
      !
      CALL UTL_CASE(FINALSENSC,TMP,1)
      SELECT CASE (TMP)
        CASE ('CSS')
          FINALSENS = 0
        CASE ('DSS')
          FINALSENS = 1
        CASE ('UNSCALED')
          FINALSENS = 2
        CASE ('ONEPERCENTSS')
          FINALSENS = 3
        CASE ('ALLSS')
          FINALSENS = 4
        CASE ('ALL')
          FINALSENS = 5
        CASE ('NONE')
          FINALSENS = 6
      END SELECT
      CALL UTL_CASE(REACTIVATE,TMP,1)
      SELECT CASE (TMP)
        CASE ('NO')
          REACT = 0
        CASE ('STARTING')
          REACT = 1
        CASE ('FINAL')
          REACT = 2
      END SELECT
      !
      IF (EIGENVALUES) LPRINT = 1

      !
    ELSE
      IF(IVERB > 1) THEN
        WRITE(IOUT,*)
        WRITE(IOUT,100) &
                     '*********************************************************'
        WRITE(IOUT,100) &
                     '        NOTE UCODE_CONTROL_DATA BLOCK NOT FOUND          '
        WRITE(IOUT,100)'                DEFAULTS ARE USED '
        WRITE(IOUT,100)'        IF YOU INTENDED TO READ THIS BLOCK'
        WRITE(IOUT,100)'       CHECK THE SPELLING OF THE BLOCK NAME'
        WRITE(IOUT,100)'  JUPITER CODES IGNORE BLOCKS WITH MISSPELLED NAMES'
        WRITE(IOUT,100) &
                     '*********************************************************'
      ENDIF
    ENDIF
!   Write information (UCODE controls) to output file
    WRITE(IOUT,101)
    WRITE(IOUT,150)
    IF(PREDICT .AND. OPTIMIZE) THEN
      WRITE(*,103)
      WRITE(IOUT,103)
      WRITE(IOUT,*)' !!! CANNOT PREDICT AND OPTIMIZE IN THE SAME EXECUTION !!!'
      CALL UTL_STOP &
      (' CHANGE EITHER Prediction OR Optimize TO "no" in UCODE_CONTROL_DATA')
    ENDIF
    IF (LINADVCONF .OR. LINADVPRED) THEN
      LINEARITYADV = .TRUE.
      IF (LINADVCONF) THEN
        EXTB = 'conf'
        WRITE(IOUT,100)
        WRITE(IOUT,100)' CALCULATE SIMULATED EQUIVALENTS, CONFIDENCE INTERVALS'
        WRITE(IOUT,100)
      ELSE
        EXTB = 'pred'
        WRITE(IOUT,100)
        WRITE(IOUT,100)' CALCULATE SIMULATED EQUIVALENTS, PREDICTION INTERVALS'
        WRITE(IOUT,100)
      ENDIF
    ENDIF
    IF (LINEARITYADV .AND. LINEARITY) THEN
      WRITE(*,103)
      WRITE(IOUT,103)
      WRITE(IOUT,*)' !! BOTH LINEARITY AND LINEARITYADV = yes, SPECIFY ONE !!'
      CALL UTL_STOP &
      (' CHANGE Linearity LinearityAdv OR Optimize TO no: UCODE_CONTROL_DATA')
    ENDIF
    IF(OPTIMIZE .AND. (LINEARITY .OR. LINEARITYADV)) THEN
      WRITE(*,103)
      WRITE(IOUT,103)
      WRITE(IOUT,*)' !! CANNOT OPTIMIZE & PREPARE LINEARITY AT THE SAME TIME !!'
      CALL UTL_STOP &
      (' CHANGE Linearity LinearityAdv OR Optimize TO no: UCODE_CONTROL_DATA')
    ENDIF
    IF(PREDICT .AND. (LINEARITY .OR. LINEARITYADV)) THEN
      WRITE(*,103)
      WRITE(IOUT,103)
      WRITE(IOUT,*)' !! CANNOT PREDICT & PREPARE LINEARITY AT THE SAME TIME !!'
      CALL UTL_STOP &
      (' CHANGE Prediction OR Linearity LinearityAdv TO no: UCODE_CONTROL_DATA')
    ENDIF
    IF(SOSSURFACE .AND. (OPTIMIZE .OR. LINEARITY .OR. LINEARITYADV &
       .OR. PREDICT)) THEN
      WRITE(*,103)
      WRITE(IOUT,103)
      WRITE(IOUT,*)' CANNOT CALCULATE SOSsurface at the same time as:'
      WRITE(IOUT,*)' OPTIMIZING, PREDICTING OR PREPARING LINEARITY CALCULATION'
      CALL UTL_STOP &
      (' ADJUST SPECIFICATIONS IN THE UCODE_CONTROL_DATA BLOCK, see fn.#uout')
    ENDIF
    IF(PREDICT .AND. OPTNLUNC) THEN
      WRITE(*,103)
      WRITE(IOUT,103)
      WRITE(IOUT,200)
      WRITE(*,200)
      CALL UTL_STOP &
      (' CHANGE Prediction OR NonLinearIntervals TO no in UCODE_CONTROL_DATA')
    ENDIF
    IF(OPTIMIZE .AND. OPTNLUNC) THEN
      WRITE(*,103)
      WRITE(IOUT,103)
      WRITE(IOUT,210)
      WRITE(*,210)
      CALL UTL_STOP &
      (' CHANGE Linearity OR NonLinearIntervals TO no in UCODE_CONTROL_DATA')
    ENDIF
    IF((LINEARITY .OR. LINEARITYADV) .AND. OPTNLUNC) THEN
      WRITE(*,103)
      WRITE(IOUT,103)
      WRITE(IOUT,220)
      WRITE(*,220)
      CALL UTL_STOP &
      (' CHANGE Prediction OR NonLinearIntervals TO no in UCODE_CONTROL_DATA')
    ENDIF
    IF(SOSSURFACE .AND. (OPTIMIZE .OR. LINEARITY .OR. LINEARITYADV &
       .OR. PREDICT .OR. OPTNLUNC)) &
      THEN
      WRITE(*,103)
      WRITE(IOUT,103)
      WRITE(IOUT,230)
      WRITE(*,230)
      CALL UTL_STOP &
      (' ADJUST SPECIFICATIONS IN THE UCODE_CONTROL_DATA BLOCK')
    ENDIF
    IF(.NOT. LINEARITY .AND. LINEARITYADV) LINEARITY = .TRUE.
    IF (SOSSURFACE) THEN
      SENSITIVITIES = .FALSE.
      OPTIMIZE = .FALSE.
      OPTNLUNC = .FALSE.
      DATAEXCHANGE = .FALSE.
      STARTRES = 0
      INTERMEDRES = 0
      FINALRES = 0
      WRITE(IOUT,100)'A Sum-of-Squares surface will be calculated'
      WRITE(IOUT,100)'_sos will be written with the following information'
      WRITE(IOUT,100) &
      ' SSWR pval(1) ... pval(#)'
      WRITE(IOUT,100)'OPTIMIZATION WILL NOT BE PERFORMED'
    ENDIF
    IF (PREDICT) THEN
      IDEPTYPE = 2
!!!!!!!!!!!!!!!!!!!      SENSITIVITIES = .FALSE.
      OPTIMIZE = .FALSE.
      OPTNLUNC = .FALSE.
      WRITE(IOUT,100)'PREDICTION WILL BE PERFORMED'
      WRITE(IOUT,100)'THE FOLLOWING FILES WILL BE WRITTEN'
      WRITE(IOUT,100)'_p contains predictions; _pv prediction variance'
      WRITE(IOUT,100)'_gmp contains prediction names by group'
      WRITE(IOUT,100)'_dmp contains the number of prediction groups and the'
      WRITE(IOUT,100)' number of parameters considered for the predictive run'
      WRITE(IOUT,100)'_spu unscaled sensitivities'
      WRITE(IOUT,100)'_spsr _spsp _sppr & _sppp WILL BE WRITTEN, see the UCODE '
      WRITE(IOUT,100)'manual for descriptions of these scaled sensitivities'
      WRITE(IOUT,100)' '
      WRITE(IOUT,100)'OPTIMIZATION IS NOT PERFORMED DURING PREDICTION'
    ENDIF
    IDEPTYPEU = IDEPTYPE
    IF (STDERRONE .AND. (.NOT. PREDICT) .AND. (.NOT. LINEARITY) &
        .AND. OPTIMIZE) THEN
      SENSITIVITIES = .TRUE.
      WRITE(IOUT,*)
      WRITE(IOUT,*)' STDERRONE_PROBLEM OPTION SELECTED,'
      WRITE(IOUT,*)'       RESIDUALS WILL BE ASSUMED ZERO AND,'
      WRITE(IOUT,*)'       CEV WILL BE SET TO 1 FOR CALCULATING COVARIANCE'
      WRITE(IOUT,*)
    ENDIF
    IF ((LINEARITY) .AND. (.NOT. PREDICT)) THEN
      OPTIMIZE = .FALSE.
      OPTNLUNC = .FALSE.
      SENSITIVITIES = .FALSE.
      IWRITE = .FALSE.
      WRITE(IOUT,100)  'THE FORWARD MODEL WILL BE EXECUTED FOR LINEARITY '
      IF (LINEARITYADV) THEN
        WRITE(IOUT,100)'           PARAMETER SETS TO PRODUCE _b2 and _b4'
        IDEPTYPE = 3
      ELSE
        WRITE(IOUT,100)'                    PARAMETER SETS TO PRODUCE _b2'
      ENDIF
    ENDIF
    IF (LINEARITY .AND. PREDICT) THEN !PREEDICTLINEARITY=.TRUE.
      OPTIMIZE = .FALSE.
      OPTNLUNC = .FALSE.
      SENSITIVITIES = .FALSE.
      IWRITE = .FALSE.
      WRITE(IOUT,100)'THE FORWARD MODEL WILL BE EXECUTED FOR ADV PREDICTIONS'
      WRITE(IOUT,100)'                       PARAMETER SETS TO PRODUCE _padv'
    ENDIF
    WRITE(IOUT,102)'MODEL NAME = ',MODELNAME
    WRITE(IOUT,102)'MODEL LENGTH UNITS = ',MODELLENGTH
    WRITE(IOUT,102)'MODEL TIME UNITS = ',MODELTIME
    WRITE(IOUT,102)'MODEL MASS UNITS = ',MODELMASS
    WRITE(IOUT,102)
    IF (OPTIMIZE) THEN
      SENSITIVITIES = .TRUE.
      WRITE(IOUT,100)'OPTIMIZATION WILL BE PERFORMED'
      IF(REACT > 0) WRITE(IOUT,300)
      IF(REACT == 1) WRITE(IOUT,301)
      IF(REACT == 2) WRITE(IOUT,302)
    ELSE
      WRITE(IOUT,100)'OPTIMIZATION WILL NOT BE PERFORMED'
    ENDIF
    IF (OPTNLUNC) THEN
      SENSITIVITIES = .TRUE.
      DATAEXCHANGE = .FALSE.
      STATS_ON_NONCONVERGE = .FALSE.
      PRED_MODADV = .TRUE.
      IDEPTYPE = 3
      WRITE(IOUT,100)'NONLINEAR INTERVALS WILL BE EVALUATED'
    ELSE
      WRITE(IOUT,100)'NONLINEAR INTERVALS WILL NOT BE PERFORMED (BY DEFAULT)'
    ENDIF
    IF (CREATEINITFILES .AND. (.NOT. SENSITIVITIES)) THEN
      WRITE(*,103)
      WRITE(IOUT,103)
      WRITE(IOUT,*)' CREATEINITFILES=yes in the UCODE input file'
      WRITE(IOUT,*)' CREATEINITFILES is a SENSITVITY MODE FUNCTION'
      WRITE(IOUT,*)' Set SENSITIVITIES=yes in the UCODE_CONTROL_DATA BLOCK'
      WRITE(*,*)' CREATEINITFILES=yes in the UCODE input file'
      WRITE(*,*)' CREATEINITFILES is a SENSITVITY MODE FUNCTION'
      WRITE(*,*)' Set SENSITIVITIES=yes in the UCODE_CONTROL_DATA BLOCK'
      CALL UTL_STOP &
      (' ADJUST SPECIFICATIONS IN THE UCODE_CONTROL_DATA BLOCK')
    ENDIF
    IF (SENSITIVITIES) THEN
      WRITE(IOUT,100)'SENSITIVITIES WILL BE CALCULATED'
      IF (CREATEINITFILES) THEN
        WRITE(IOUT,100)'NONOPTIMAL SENSITIVITIES WILL BE CALCULATED'
        WRITE(IOUT,100)'ONLY THE FOLLOWING DATAEXCHANGE FILES WILL BE WRITTEN:'
        WRITE(IOUT,100)' fn._init fn._init._mv fn._init._su'
        DATAEXCHANGE = .FALSE.
        SENSITIVITIES = .TRUE.
      ENDIF
    ELSE
      IF(PREDICT) THEN
        WRITE(IOUT,100)'SENSITIVITIES WILL NOT BE CALCULATED (BY USER CHOICE)'
      ELSE
        WRITE(IOUT,100)'SENSITIVITIES WILL NOT BE CALCULATED (BY DEFAULT)'
      ENDIF
      IF (CREATEINITFILES) THEN
        WRITE(IOUT,100)'IF NONOPTIMAL SENSITIVITIES ARE TO BE CALCULATED,'
        WRITE(IOUT,100)'   SENSITIVITIES MUST EQUAL YES'
      ENDIF
    ENDIF
    IF (.NOT. OPTIMIZE .AND. .NOT. OPTNLUNC .AND. .NOT. SENSITIVITIES .AND. &
        .NOT. SOSSURFACE .AND. .NOT. LINEARITY .AND. &
        .NOT. STDERRONE) THEN
      IF(PREDICT) THEN
        WRITE(IOUT,100) &
        ' THIS IS ONLY A FORWARD SIMULATION OF PREDICTIVE MODELS FOR OPTIMAL'
        WRITE(IOUT,100) &
        ' PARAMETER VALUES, ALLOWING FOR A CHECK OF THE PREDICTIVE SETUP'
      ELSE
        WRITE(IOUT,100)'ONLY RESIDUAL ANALYSIS FOR STARTING PARAMETER VALUES'
      ENDIF
      RESIDONLY = .TRUE.
      LPRINT = 0
    ENDIF
    IF (.NOT. LINEARITY .AND. .NOT. PREDICT &
        .AND. .NOT. SOSSURFACE &
        .AND. .NOT. RESIDONLY) THEN
      WRITE(IOUT,100) IMESS(STARTRES)
      IF(OPTIMIZE) THEN
        WRITE(IOUT,100) PMESS(INTERMEDRES)
        WRITE(IOUT,100) FMESS(FINALRES)
        IF (LPRINT .GE. 1) THEN
          WRITE(IOUT,100)'EIGENVECTORS/EIGENVALUES WILL BE PRINTED'
        ELSE
          WRITE(IOUT,100)'EIGENVECTORS/EIGENVALUES WILL NOT BE PRINTED'
        ENDIF
      ENDIF
      IF(SENSITIVITIES) THEN
        WRITE(IOUT,100) ISMESS(STARTSENS)
        WRITE(IOUT,100) PSMESS(INTERMEDSENS)
        WRITE(IOUT,100) FSMESS(FINALSENS)
      ENDIF
    ENDIF
    IF(.NOT. OPTNLUNC) THEN
      IF (DATAEXCHANGE) THEN
        WRITE(IOUT,100)'DATA EXCHANGE FILES WILL BE PRODUCED'
      ELSE
        WRITE(IOUT,100)'DATA EXCHANGE FILES WILL NOT BE PRODUCED'
      ENDIF
    ENDIF
    RETURN
  END SUBROUTINE UCODE_INI
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_INI_ALLOC (IFAIL,IDEPTYPE,MPR)
    !   Allocate arrays
    USE GLOBAL_DATA, ONLY: BIGDOUBLE
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER, INTENT(INOUT) :: IFAIL
    INTEGER, INTENT(IN)    :: IDEPTYPE
    INTEGER, INTENT(IN)    :: MPR
    ! Local Variables
    INTEGER :: MPRAR = 1
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_INI_ALLOC'
    IFAIL = 0
    !
    IF (NOBS>0) THEN
      ALLOCATE(OBSNAM(NOBS),MODELVAL(NOBS),OMIT(NOBS), &
               RESIDS(NOBS),WTDRESIDS(NOBS),XSENST(NPE,NOBS))
      IF (IDEPTYPE==1) THEN
        MCVUSE(1) = .TRUE.  ! Activates extraction of observations
        MCVUSE(2) = .FALSE.
      ELSEIF (IDEPTYPE==2) THEN
        MCVUSE(1) = .FALSE.
        MCVUSE(2) = .TRUE.  ! Activates extraction of predictions
      ELSEIF (IDEPTYPE==3) THEN
        MCVUSE(1) = .TRUE.  ! Activates extraction of observations
        MCVUSE(2) = .TRUE.  ! Activates extraction of predictions
      ENDIF
      OBSNAM = ' '
      MODELVAL = BIGDOUBLE
      OMIT = 0
      XSENST = BIGDOUBLE
      ! recent altered dep module, remove if no bugs are reported by Jan2007
      !      NN = 0
      !      DO N=1,NOBS+MPR
      !        IF(N > NTOTOBS .AND. N <= NOBS) CYCLE
      !        NN = NN + 1
      !      ENDDO
      !      INCLU = NN - MPR
      INCLU = NUSEOBS
    ENDIF
    !
    IF (MPR>0) MPRAR = MPR
    ALLOCATE (XPRI(NPE,MPRAR))
    XPRI = 0.D0
    NPSNPD = NPS + NPD
    ALLOCATE(PARNAMALL(NPSNPD))
    !
    RETURN
  END SUBROUTINE UCODE_INI_ALLOC
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_INI_FILEMANAGE(IFAIL,IOUTTEMP,OUTNAM,OUTNAMPRED,IOUT)
    USE UTILITIES
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                        INTENT(INOUT) :: IFAIL
    INTEGER,                        INTENT(IN)    :: IOUTTEMP
    CHARACTER(LEN=MAX_STRING_LEN),  INTENT(IN)    :: OUTNAM
    CHARACTER(LEN=MAX_STRING_LEN),  INTENT(IN)    :: OUTNAMPRED
    INTEGER,                        INTENT(OUT)   :: IOUT
    ! Local variables
    CHARACTER(LEN=MAX_STRING_LEN)                 :: FN
    CHARACTER(LEN=MAX_STRING_LEN)                 :: LINE
    INTEGER I
    ! Formats
    100 FORMAT(A)
    101 FORMAT(1X,A)
    109 FORMAT(1X,79('_'))
    110 FORMAT(/,1X,'FILES TO BE MERGED INTO: ',A)
    120 FORMAT(/,1X,' Path to Merged File Specified: ',A,/, &
                 1X,' Default is overridden',/)
    125 FORMAT(/,1X,' Second Path to Merged File Specified: ',A,/)
    130 FORMAT(/,1X,' SkipLines File ',/,1X,' --------- ---- ')
    135 FORMAT(1X,I8,4X,A)
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_INI_FILEMANAGE'
    IFAIL = 0
    IOUT = UTL_GETUNIT(101,150)
    IF (LINEARITY) THEN
      IF(LINADVCONF) THEN
        FN = TRIM(OUTNAMPRED)//'.#umodlinadv_conf'
      ELSEIF(LINADVPRED) THEN
        FN = TRIM(OUTNAMPRED)//'.#umodlinadv_pred'
      ELSE
        FN = TRIM(OUTNAM)//'.#umodlin'
      ENDIF
    ELSEIF (PREDICT) THEN
      FN = TRIM(OUTNAMPRED)//'.#upred'
    ELSEIF (SOSSURFACE) THEN
      FN = TRIM(OUTNAM)//'.#usos'
    ELSEIF (OPTNLUNC) THEN
      IF(NTYP == 1) THEN
        FN = TRIM(OUTNAMPRED)//'.#unonlinint_conf'
      ELSE
        FN = TRIM(OUTNAMPRED)//'.#unonlinint_pred'
      ENDIF
    ELSEIF (CREATEINITFILES) THEN
      FN = TRIM(OUTNAM)//'.#ucreateinitfiles'
    ELSE
      FN = TRIM(OUTNAM)//'.#uout'
    ENDIF
    OPEN(UNIT=IOUT,FILE=FN,STATUS='REPLACE')
    DO
      READ(IOUTTEMP,100,END=900)LINE
      WRITE(IOUT,101)TRIM(LINE)
    ENDDO
    900 IF (NMERGE>0) THEN
      DO I=1,NMERGE
        IF(TRIM(PATHTOMERGEDFILE) .NE. TRIM(MERGEPATH)) THEN
          IF(DEFAULT) THEN
            MERGEPATH = TRIM(PATHTOMERGEDFILE)
            WRITE(IOUT,120)TRIM(MERGEPATH)
            DEFAULT = .FALSE.
            CYCLE
          ELSE
            WRITE(IOUT,125) TRIM(PATHTOMERGEDFILE)
            AMESSAGE = ' ALTHOUGH NUMEROUS PathToFile ITEMS CAN BE SPECIFIED '
            WRITE(*,*)TRIM(AMESSAGE)
            CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
            AMESSAGE = ' ONLY ONE PathToMergedFile IS PERMITTED '
            WRITE(*,*)TRIM(AMESSAGE)
            CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','yes')
            AMESSAGE = ' CORRECT MERGE_FILES BLOCK  '
            WRITE(*,*)TRIM(AMESSAGE)
            CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','yes')
            AMESSAGE = ' CONFIRM THIS FILE IS USED BY THE DERIVATIVES INTERFACE'
            WRITE(*,*)TRIM(AMESSAGE)
            CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','yes')
            CALL UTL_STOP('TERMINATE: MULTIPLE PathToMergedFile SPECIFICATIONS')
          ENDIF
        ENDIF
      ENDDO
      WRITE(IOUT,109)
      WRITE(IOUT,110)TRIM(MERGEPATH)
      WRITE(IOUT,130)
      DO I=1,NMERGE
        WRITE(IOUT,135)SKIPLINES(I),TRIM(FILES(I))
      ENDDO
      WRITE(IOUT,109)
    ENDIF
    RETURN
  END SUBROUTINE UCODE_INI_FILEMANAGE
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_INI_PARS (IFAIL,INUNIT,IOUT,MAXITER,MAXCHANGE,OUTNAM, &
                             OUTNAMPRED,TOLPAR,FINALSTATS,PRECISION,DECPOINT)
    !   Read parameter-related data
    USE DATATYPES
    USE BASIC, ONLY: FORSENS
    USE GLOBAL_DATA, ONLY: AMESSAGE, BIGDOUBLE, ERRSUB, IVERB, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                         INTENT(INOUT) :: IFAIL
    INTEGER,                         INTENT(IN)    :: INUNIT
    INTEGER,                         INTENT(IN)    :: IOUT
    INTEGER,                         INTENT(IN)    :: MAXITER
    DOUBLE PRECISION,                INTENT(IN)    :: MAXCHANGE
    CHARACTER(LEN=MAX_STRING_LEN),   INTENT(IN)    :: OUTNAM
    CHARACTER(LEN=MAX_STRING_LEN),   INTENT(IN)    :: OUTNAMPRED
    DOUBLE PRECISION,                INTENT(IN)    :: TOLPAR
    LOGICAL,                         INTENT(INOUT) :: FINALSTATS
    CHARACTER (LEN=*), OPTIONAL,     INTENT(IN)    :: PRECISION ! Precision protocol
    CHARACTER (LEN=*), OPTIONAL,     INTENT(IN)    :: DECPOINT  ! Decimal-point protocol
    !
    !   Local variables
    TYPE (LLIST), POINTER :: PGHEAD ! Pointer to head of list (par. groups)
    TYPE (LLIST), POINTER :: PGHEADFP ! Pointer to head of list (par. groups for prediciton)
    TYPE (LLIST), POINTER :: PIHEAD ! Pointer to head of first list (par. info)
    TYPE (LLIST), POINTER :: PIHEADFP ! Pointer to head of list (par. info for prediction)
    TYPE (LLIST), POINTER :: PVHEAD ! Pointer to head of list (par. values)
    TYPE (LLIST), POINTER :: PDHEAD ! Pointer to head of list (derived pars)
    TYPE (LLIST), POINTER :: PDHEADFP ! Pointer to head of list (derived pars for prediciton)
    TYPE (LLIST), POINTER :: TAIL
    INTEGER               :: I, IUPR, IERR, J, JJ, KPGP, KPGPFP, KPV, MORE, NP
    CHARACTER (LEN=10)    :: ATEMP
    CHARACTER(LEN=12) :: DEFGROUP
    LOGICAL :: TERMINATE = .FALSE.
    LOGICAL :: TERMINATE2 = .FALSE.
    INTEGER, ALLOCATABLE, DIMENSION(:)           :: PALNTP
    CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:) :: PANAMTP
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)  :: PAPVALTP
    !   Format statements
    99 FORMAT(1X,A)
    100 FORMAT(/,1X,A)
    101 FORMAT(1X,A)
    102 FORMAT(/,1X,A,1X,A)
    104 FORMAT(//,10('**_sos**'),//, &
        1X,'The parameter values and associated sum-of-squared weighted ', &
        'residuals',/,3X, &
        'are listed in the UCODE data-exchange file with file extension _sos', &
        //,10('**_sos**'),//)
    200 FORMAT(1X,I6,' Error(s) encountered -- STOP EXECUTION (MAININIT)')
    203 FORMAT(/,   &
        1X,'No. ',1X,'Param. name ',1X,'SOSincrement',2X,'Lower value',  &
        3X,'Upper value',/, &
        1X,4('-'),1X,12('-'),1X,12('-'),2(2x,12('-')))
    210 FORMAT(1X,'ERROR: SCALEPVAL<0 for parameter: ',A)
    230 FORMAT(1X,I4,1X,A,1X,I10,2X,2(1X,1PG13.6))
    500 FORMAT (//,1X,78('!'),/,11X, &
             'NOTE: SENSITIVITIES OF THE PREDICTIONS WILL BE',/,11X, &
             'CALCULATED USING FORWARD DIFFERENCE PERTURBATION.',/,11X, &
             'THEY ARE LESS ACCURATE THAN SENSITIVITIES ',/,11X, &
             'CALCULATED BY CENTRAL DIFFERENCES.',/,1X,78('!'),//)
    ! Initialize
    NULLIFY(PGHEAD,PGHEADFP,PIHEAD,PIHEADFP,PVHEAD,PDHEAD,PDHEADFP,TAIL)
    I = 0
    IUPR = 0
    IERR = 0
    J = 0
    JJ = 0
    KPGP = 0
    KPGPFP = 0
    KPV = 0
    MORE = 0
    NP = 0
    AMESSAGE = ' '
    ERRSUB='Error in subroutine UCODE_INI_PARS:'
    IFAIL=0
    KPGPFP = 0
    DEFGROUP='DefaultPar'
    !
    IF(PRESENT(PRECISION))THEN
      ATEMP=ADJUSTL(PRECISION)
      CALL UTL_CASETRANS(ATEMP,'lo')
      IF(ATEMP(1:6).EQ.'double')THEN
        PRECIS=1
      ELSE
        PRECIS=0
      END IF
    ELSE
      PRECIS=0
    END IF
    IF(PRESENT(DECPOINT))THEN
      ATEMP=ADJUSTL(DECPOINT)
      CALL UTL_CASETRANS(ATEMP,'lo')
      IF(ATEMP(1:7).EQ.'nopoint')THEN
        NOPNT=1
      ELSE
        NOPNT=0
      END IF
    ELSE
      NOPNT=0
    END IF
    !   Read and store information in PARAMETERGROUPS block
    KPGP = 0
    CALL UTL_READBLOCK(NPARGPCOLS,'PARAMETER_GROUPS',PARGPCOL,INUNIT,IOUT,   &
        'GROUPNAME',.FALSE.,PGHEAD,TAIL,KPGP)
    IF (KPGP==0) THEN
      IF(IVERB > 1) THEN
        WRITE(IOUT,*)
        WRITE(IOUT,100) &
                     '*********************************************************'
        WRITE(IOUT,100)'       NOTE PARAMETER_GROUPS BLOCK NOT FOUND '
        WRITE(IOUT,100)'                DEFAULTS ARE USED '
        WRITE(IOUT,100)'        IF YOU INTENDED TO READ THIS BLOCK'
        WRITE(IOUT,100)'       CHECK THE SPELLING OF THE BLOCK NAME'
        WRITE(IOUT,100)'  JUPITER CODES IGNORE BLOCKS WITH MISSPELLED NAMES'
        WRITE(IOUT,100) &
                     '*********************************************************'
        WRITE(IOUT,*)
      ENDIF
      KPGP=1
    ENDIF
    IF (IVERB>4 .AND. KPGP>0) THEN
      !   Write block information (parameter groups) to output file
      WRITE(IOUT,'(A)')HYPHENS(1:80)
      WRITE(IOUT,100)'Echo parameter-groups input:'
      CALL UTL_WRITEBLOCK(PGHEAD,IOUT)
    ENDIF
    IF(PREDICT .OR. LINEARITYADV .OR. OPTNLUNC) THEN
      !   Read and store information in PARAMETER_GROUPS_FOR_PREDICTION block
      CALL UTL_READBLOCK(NPARGPFPCOLS,'PARAMETER_GROUPS_FOR_PREDICTION', &
               PARGPFPCOL,INUNIT,IOUT,'GROUPNAME',.FALSE.,PGHEADFP,TAIL,KPGPFP)
      IF (KPGPFP==0) THEN
        IF(IVERB > 1) THEN
          WRITE(IOUT,*)
          WRITE(IOUT,100) &
                       '*******************************************************'
          WRITE(IOUT,100)' NOTE PARAMETER_GROUPS_FOR_PREDICTION BLOCK NOT FOUND'
          WRITE(IOUT,100)'  NO NEW PARAMETER GROUPS CONSIDERED FOR PREDICTION'
          WRITE(IOUT,100)'        IF YOU INTENDED TO READ THIS BLOCK'
          WRITE(IOUT,100)'       CHECK THE SPELLING OF THE BLOCK NAME'
          WRITE(IOUT,100)'  JUPITER CODES IGNORE BLOCKS WITH MISSPELLED NAMES'
          WRITE(IOUT,100) &
                       '*******************************************************'
          WRITE(IOUT,*)
        ENDIF
        KPGPFP=1
      ENDIF
      IF (IVERB>4 .AND. KPGPFP>0) THEN
        !Write block information (parameter groups for prediction) to outputfile
        WRITE(IOUT,'(A)')HYPHENS(1:80)
        WRITE(IOUT,100)'Echo parameter-groups-for-prediction input:'
        CALL UTL_WRITEBLOCK(PGHEADFP,IOUT)
      ENDIF
    ENDIF
    !
    !   Read and store information in PARAMETER_DATA block
    NP = 0
    CALL UTL_READBLOCK(NPARCOLS,'PARAMETER_DATA',PARCOL,INUNIT,IOUT,   &
        'PARAMNAME',.TRUE.,PIHEAD,TAIL,NP)
    IF (IVERB>4 .AND. NP>0) THEN
      !   Write block information (parameters) to output file
      WRITE(IOUT,100)'Echo parameter data before inserting group information:'
      CALL UTL_WRITEBLOCK(PIHEAD,IOUT)
    ENDIF
    !
    !   Insert parameter-group information into parameter-info lists
    CALL UTL_GROUPLIST(DEFGROUP,PGHEAD,IOUT,PIHEAD,KPGP,NP)
    !
    IF (IVERB>4 .AND. NP>0) THEN
      !   Write block information (parameters) to output file
      WRITE(IOUT,100)'Echo parameter input after inserting group information:'
      CALL UTL_WRITEBLOCK(PIHEAD,IOUT)
      !   Write block information (parameter groups) to output file
      WRITE(IOUT,'(A)')HYPHENS(1:80)
      WRITE(IOUT,100)'Echo parameter-groups input after UTLGROUPLIST:'
      CALL UTL_WRITEBLOCK(PGHEAD,IOUT)
    ENDIF
    IF(PREDICT .OR. LINEARITYADV .OR. OPTNLUNC) THEN
      !
      !   Read and store information in PARAMETER_DATA_FOR_PREDICTION block
      NPFP = 0
      CALL UTL_READBLOCK(NPARFPCOLS,'PARAMETER_DATA_FOR_PREDICTION', &
           PARFPCOL,INUNIT,IOUT,'PARAMNAME',.FALSE.,PIHEADFP,TAIL,NPFP)
      IF (NPFP==0) THEN
        IF(IVERB > 1) THEN
          WRITE(IOUT,*)
          WRITE(IOUT,100) &
                       '*******************************************************'
          WRITE(IOUT,100)' NOTE PARAMETER_DATA_FOR_PREDICTION BLOCK NOT FOUND '
          WRITE(IOUT,100) &
                       'NO ADDITIONAL PARAMETERS ARE CONSIDERED FOR PREDICTION '
          WRITE(IOUT,100)'        IF YOU INTENDED TO READ THIS BLOCK'
          WRITE(IOUT,100)'       CHECK THE SPELLING OF THE BLOCK NAME'
          WRITE(IOUT,100)'  JUPITER CODES IGNORE BLOCKS WITH MISSPELLED NAMES'
          WRITE(IOUT,100) &
                       '*******************************************************'
          WRITE(IOUT,*)
        ENDIF
      ENDIF
      IF (IVERB>4 .AND. NPFP>0) THEN
        !   Write block information (parameters) to output file
        WRITE(IOUT,'(A)')HYPHENS(1:80)
        WRITE(IOUT,100) &
        'Echo parameter data for prediction before inserting group information:'
        CALL UTL_WRITEBLOCK(PIHEADFP,IOUT)
      ENDIF
      !
      !   Insert parameter-group information into parameter-info lists
      IF(NPFP>0)CALL UTL_GROUPLIST(DEFGROUP,PGHEADFP,IOUT,PIHEADFP,KPGPFP,NPFP)
      !
      IF (IVERB>4 .AND. NPFP>0) THEN
        !   Write block information (parameters for prediction) to output file
        WRITE(IOUT,100) &
        'Echo parameter input for prediction after inserting group information:'
        CALL UTL_WRITEBLOCK(PIHEADFP,IOUT)
        !Write block information (parameter groups for prediction) to output file
        WRITE(IOUT,'(A)')HYPHENS(1:80)
        WRITE(IOUT,100) &
        'Echo parameter-groups-for-prediciton input after UTLGROUPLIST:'
        CALL UTL_WRITEBLOCK(PGHEADFP,IOUT)
      ENDIF
    ENDIF
    !
    !   Read and store information in PARAMETERVALUES block
    KPV = 0
    CALL UTL_READBLOCK(NPVCOLS,'PARAMETER_VALUES',PVCOL,INUNIT,IOUT,   &
        'PARAMNAME',.FALSE.,PVHEAD,TAIL,KPV)
    IF (IVERB>4 .AND. KPV>0) THEN
      !   Write block information (parameters) to output file
      WRITE(IOUT,100)'Echo parameter values block:'
      CALL UTL_WRITEBLOCK(PVHEAD,IOUT)
    ENDIF
    !
    !   Merge PARAMETERVALUES data with PARAMETERINFO data
    CALL UTL_MERGELIST('PARAMETER_DATA','PARAMETER_VALUES',PIHEAD,PVHEAD,IOUT)
    IF (IVERB>4 .AND. NP>0) THEN
      !   Write block information (parameters) to output file
      WRITE(IOUT,100)'Echo parameter input'
      WRITE(IOUT,100)'               after merging with parameter-values block:'
      CALL UTL_WRITEBLOCK(PIHEAD,IOUT)
    ENDIF
    !   Read and store information in DERIVED_PARAMETERS block
    NPD = 0
    CALL UTL_READBLOCK(NPDCOLS,'DERIVED_PARAMETERS',PDCOL,INUNIT,IOUT,   &
        'DERPARNAME',.FALSE.,PDHEAD,TAIL,NPD)
    IF (NPD==0) THEN
      IF(IVERB > 1) THEN
        WRITE(IOUT,*)
        WRITE(IOUT,99)'*******************************************************'
        WRITE(IOUT,99)'       NOTE DERIVED_PARAMETERS BLOCK NOT FOUND'
        WRITE(IOUT,99)'        NO DERIVED PARAMETERS ARE CONSIDERED'
        WRITE(IOUT,99)'        IF YOU INTENDED TO READ THIS BLOCK'
        WRITE(IOUT,99)'       CHECK THE SPELLING OF THE BLOCK NAME'
        WRITE(IOUT,99)'  JUPITER CODES IGNORE BLOCKS WITH MISSPELLED NAMES'
        WRITE(IOUT,99)'*******************************************************'
        WRITE(IOUT,*)
      ENDIF
      WRPAREQ = .FALSE.
    ENDIF
    IF (IVERB>4 .AND. NPD>0) THEN
      !   Write block information (dervied parameters) to output file
      WRITE(IOUT,'(A)')HYPHENS(1:80)
      WRITE(IOUT,100)'Echo derived parameters block:'
      CALL UTL_WRITEBLOCK(PDHEAD,IOUT)
    ENDIF
    IF(PREDICT .OR. LINEARITYADV .OR. OPTNLUNC) THEN
      !   Read and store information in DERIVED_PARAMETERS_FOR_PREDICTION block
      NPDFP = 0
      CALL UTL_READBLOCK(NPDFPCOLS,'DERIVED_PARAMETERS_FOR_PREDICTION', &
           PDFPCOL,INUNIT,IOUT,'DERPARNAME',.FALSE.,PDHEADFP,TAIL,NPDFP)
      IF (NPDFP==0) THEN
        IF(IVERB > 1) THEN
          WRITE(IOUT,*)
          WRITE(IOUT,99) &
                       '*******************************************************'
          WRITE(IOUT,99)'NOTE DERIVED_PARAMETERS_FOR_PREDICTION BLOCK NOT FOUND'
          WRITE(IOUT,99) &
                       'NO NEW DERIVED PARAMETERS ARE CONSIDERED FOR PREDICTION'
          WRITE(IOUT,99)'        IF YOU INTENDED TO READ THIS BLOCK'
          WRITE(IOUT,99)'       CHECK THE SPELLING OF THE BLOCK NAME'
          WRITE(IOUT,99)'  JUPITER CODES IGNORE BLOCKS WITH MISSPELLED NAMES'
          WRITE(IOUT,99) &
                       '*******************************************************'
          WRITE(IOUT,*)
        ENDIF
      ENDIF
      IF (IVERB>4 .AND. NPDFP>0) THEN
        !Write block info (dervied parameters for prediction) to output file
        WRITE(IOUT,'(A)')HYPHENS(1:80)
        WRITE(IOUT,100)'Echo derived parameters for prediction block:'
        CALL UTL_WRITEBLOCK(PDHEADFP,IOUT)
      ENDIF
    ENDIF !! DONE READING PARAMETER BLOCKS _FOR_PREDICTION
    !
    !   Allocate arrays for parameter-related data
    NPS = NP
    NPSWOP = NP
    NPDWOP = NPD
    IF(PREDICT .OR. LINEARITYADV .OR. OPTNLUNC) THEN
      NPS = NPS+NPFP
      NPD = NPD+NPDFP
    ENDIF
    ALLOCATE(DERPAREQN(NPD), DERPARNAME(NPD), DERPARNAMELC(NPD),ITRANS(NPS), &
              BSCAL(NPS), CONSTRAIN(NPS), ISENMETHOD(NPS), ISENMETHODPRT(NPS), &
              ICONVERT_DERIV(NPS), LN(NPS), &
              NW(NPS+NPD),  &
              PADJ(NPS), PARNAM(NPS), PARNAMLC(NPS), PARGP(NPS), &
              PASSIGNED(NPS), PINCR(NPS), &
              PVAL(NPS), PVALINIT(NPS), &
              PVALMAX(NPS), PVALMAXC(NPS), PVALMIN(NPS), PVALMINC(NPS),  &
              PERTURB(NPS), PMAXCHANGE(NPS), PTOLPAR(NPS),  &
              SOSINCREMENT(NPS), TRANSFORM(NPS), NONLININT(NPS))
    ALLOCATE(INCLU(MAXITER+2),FNITER(MAXITER+2), &
                                RSQALL(MAXITER+2),RSQD(MAXITER+2))
    !
    !   Populate arrays with default values
    CONSTRAIN = .FALSE.
    TRANSFORM = .FALSE.
    DERPARNAME = ' '
    DERPARNAMELC = ' '
    DERPAREQN = ' '
    FNITER = ' '
    ICONVERT_DERIV = 1
    IF(SENSITIVITIES .AND. .NOT. OPTIMIZE) THEN
      ISENMETHOD = 2
    ELSE
      ISENMETHOD = 1
    ENDIF
    ISENMETHODPRT = ISENMETHOD
    LN = 0
    NONLININT = .FALSE.
    PARGP = DEFGROUP
    PARNAM = ' '
    PINCR = -1
    SMALLDOUBLE = -1.0D0*BIGDOUBLE
    PVAL = SMALLDOUBLE
    PVALINIT = BIGDOUBLE
    PVALMAX = BIGDOUBLE
    PVALMAXC = BIGDOUBLE
    PVALMIN = SMALLDOUBLE
    PVALMINC = SMALLDOUBLE
    PERTURB = 0.01
    PMAXCHANGE = MAXCHANGE
    PTOLPAR = TOLPAR
    PADJ = .FALSE.
    PASSIGNED = .FALSE.
    RSQALL=0.D0
    RSQD=0.D0
    SOSINCREMENT=5
    !
    !   Filter information in linked list and populate arrays in the PARAMSET
    !   structure
    IERR = 0
    CALL UTL_FILTERLIST(PIHEAD,IOUT,'PARAMNAME',NPS,IERR,PARNAM,MORE)
    CALL UTL_FILTERLIST(PIHEAD,IOUT,'GROUPNAME',NPS,IERR,PARGP,MORE)
    CALL UTL_FILTERLIST(PIHEAD,IOUT,'STARTVALUE',NPS,IERR,PVAL,MORE)
    IF(NPFP>0) THEN
      CALL UTL_FILTERLIST(PIHEADFP,IOUT,'PARAMNAME',NPS,IERR,PARNAM,MORE,NP+1)
      CALL UTL_FILTERLIST(PIHEADFP,IOUT,'GROUPNAME',NPS,IERR,PARGP,MORE,NP+1)
      CALL UTL_FILTERLIST(PIHEADFP,IOUT,'STARTVALUE',NPS,IERR,PVAL,MORE,NP+1)
    ENDIF
    DO I=1,NPS
      BSCAL(I)=ABS(PVAL(I))/100.D0
    ENDDO
    CALL UTL_FILTERLIST(PIHEAD,IOUT,'LOWERVALUE',NPS,IERR,PVALMIN,MORE)
    CALL UTL_FILTERLIST(PIHEAD,IOUT,'UPPERVALUE',NPS,IERR,PVALMAX,MORE)
    CALL UTL_FILTERLIST(PIHEAD,IOUT,'CONSTRAIN',NPS,IERR,CONSTRAIN,MORE)
    CALL UTL_FILTERLIST(PIHEAD,IOUT,'LOWERCONSTRAINT',NPS,IERR,PVALMINC,MORE)
    CALL UTL_FILTERLIST(PIHEAD,IOUT,'UPPERCONSTRAINT',NPS,IERR,PVALMAXC,MORE)
    CALL UTL_FILTERLIST(PIHEAD,IOUT,'ADJUSTABLE',NPS,IERR,PADJ,MORE)
    CALL UTL_FILTERLIST(PIHEAD,IOUT,'PERTURBAMT',NPS,IERR,PERTURB,MORE)
    CALL UTL_FILTERLIST(PIHEAD,IOUT,'TRANSFORM',NPS,IERR,TRANSFORM,MORE)
    CALL UTL_FILTERLIST(PIHEAD,IOUT,'TOLPAR',NPS,IERR,PTOLPAR,MORE)
    CALL UTL_FILTERLIST(PIHEAD,IOUT,'MAXCHANGE',NPS,IERR,PMAXCHANGE,MORE)
    CALL UTL_FILTERLIST(PIHEAD,IOUT,'SENMETHOD',NPS,IERR,ISENMETHOD,MORE)
    ISENANY = .FALSE.
    DO I=1,NPS
      IF(ISENMETHOD(I) > 1) THEN
        ISENANY = .TRUE.
        EXIT
      ENDIF
    ENDDO
    CALL UTL_FILTERLIST(PIHEAD,IOUT,'SCALEPVAL',NPS,IERR,BSCAL,MORE)
    CALL UTL_FILTERLIST(PIHEAD,IOUT,'SOSINCREMENT',NPS,IERR,SOSINCREMENT,MORE)
    CALL UTL_FILTERLIST(PIHEAD,IOUT,'NONLINEARINTERVAL',NPS,IERR,NONLININT,MORE)
    ! Filter derived parameters
    CALL UTL_FILTERLIST(PDHEAD,IOUT,'DERPARNAME',NPD,IERR,DERPARNAME,MORE)
    CALL UTL_FILTERLIST(PDHEAD,IOUT,'DERPAREQN',NPD,IERR,DERPAREQN,MORE)
    IF(PREDICT) THEN
      CALL UTLUCODE_DX_READ_DM(IFAIL,OUTNAM, &
                             RDUM,RDUM,RDUM,RDUM, &
                             CDUM,IDUM,RDUM,RDUM,RDUM, &
                             CDUM,CDUM,CDUM,CDUM, &
                             IDUM,IDUM,NPE,NPERD,IDUM,IDUM, &
                             RDUM,RDUM,RDUM,RDUM)
      ! read parameter status at end of regression
      CALL UTLUCODE_DX_READ_PAOPT(NPS,OUTNAM,LN,PARNAM,PINCR,PVAL,NPSWOP)
      PADJ = .FALSE.
      DO I=1,NPS
        IF(PINCR(I) > 0) PADJ(I) = .TRUE.
        NPE = NPE +1
        IF(PINCR(I) == 0) THEN
          PARAM_OMIT = .TRUE.
          IF(.NOT. ALLOCATED (POMIT))ALLOCATE(POMIT(NPE-NPERD))
          NPO = NPO +1
          POMIT(NPO) = I
        ENDIF
      ENDDO
    ENDIF
!    IF(PREDICT .AND. .NOT. SENSITIVITIES) PARAM_OMIT = .FALSE.
    IF(NPFP>0) THEN
      CALL UTL_FILTERLIST(PIHEADFP,IOUT,'LOWERVALUE',NPS,IERR,PVALMIN,MORE,NP+1)
      CALL UTL_FILTERLIST(PIHEADFP,IOUT,'UPPERVALUE',NPS,IERR,PVALMAX,MORE,NP+1)
      CALL UTL_FILTERLIST &
           (PIHEADFP,IOUT,'CONSTRAIN',NPS,IERR,CONSTRAIN,MORE,NP+1)
      CALL UTL_FILTERLIST &
           (PIHEADFP,IOUT,'LOWERCONSTRAINT',NPS,IERR,PVALMINC,MORE,NP+1)
      CALL UTL_FILTERLIST &
           (PIHEADFP,IOUT,'UPPERCONSTRAINT',NPS,IERR,PVALMAXC,MORE,NP+1)
      CALL UTL_FILTERLIST(PIHEADFP,IOUT,'ADJUSTABLE',NPS,IERR,PADJ,MORE,NP+1)
      CALL UTL_FILTERLIST(PIHEADFP,IOUT,'PERTURBAMT',NPS,IERR,PERTURB,MORE,NP+1)
      CALL UTL_FILTERLIST &
           (PIHEADFP,IOUT,'TRANSFORM',NPS,IERR,TRANSFORM,MORE,NP+1)
      CALL UTL_FILTERLIST(PIHEADFP,IOUT,'TOLPAR',NPS,IERR,PTOLPAR,MORE,NP+1)
      CALL UTL_FILTERLIST &
           (PIHEADFP,IOUT,'MAXCHANGE',NPS,IERR,PMAXCHANGE,MORE,NP+1)
      CALL UTL_FILTERLIST &
           (PIHEADFP,IOUT,'SENMETHOD',NPS,IERR,ISENMETHOD,MORE,NP+1)
      CALL UTL_FILTERLIST(PIHEADFP,IOUT,'SCALEPVAL',NPS,IERR,BSCAL,MORE,NP+1)
      CALL UTL_FILTERLIST &
           (PIHEADFP,IOUT,'SOSINCREMENT',NPS,IERR,SOSINCREMENT,MORE,NP+1)
      CALL UTL_FILTERLIST &
           (PIHEADFP,IOUT,'NONLINEARINTERVAL',NPS,IERR,NONLININT,MORE,NP+1)
      ! Filter derived parameters
      CALL UTL_FILTERLIST &
           (PDHEADFP,IOUT,'DERPARNAME',NPD,IERR,DERPARNAME,MORE,NPDWOP+1)
      CALL UTL_FILTERLIST &
           (PDHEADFP,IOUT,'DERPAREQN',NPD,IERR,DERPAREQN,MORE,NPDWOP+1)
    ENDIF
    ISENMETHODPRT = ISENMETHOD
    IF(STDERRONE) THEN
      PTOLPAR = HUGE(PTOLPAR)
      PMAXCHANGE = 1.D-20
    ENDIF
    !
    !   Perform error checking and count number of parameters for which
    !   ADJUSTABLE is true
    NPE = 0
    NPERTURB_CYCLES=0
    NPERTURB_CYCLES_FINAL=0
    ! Hold initial values
    PVALINIT = PVAL
    !
    DO I=1,NPS
      CALL UTL_CASE(PARNAM(I),PARNAMLC(I),-1) ! Store names as lowercase
      IF (PVAL(I) .NE. SMALLDOUBLE) PASSIGNED(I)=.TRUE.
      IF(CONSTRAIN(I))THEN
        IF (PVALMINC(I) >= PVALMAXC(I)) THEN
          AMESSAGE=' MINIMUM CONSTRAINT >= MAXIMUM CONSTRAINT FOR: '
          WRITE(*,102)TRIM(AMESSAGE),PARNAM(I)
          WRITE(IOUT,102)TRIM(AMESSAGE),PARNAM(I)
          TERMINATE = .TRUE.
        ENDIF
        IF (PVAL(I) <= PVALMINC(I)) THEN
          AMESSAGE= &
          ' CONSTRAIN IS TRUE & STARTING VALUE IS <= CONSTRAINT FOR: '
          WRITE(*,102)TRIM(AMESSAGE),PARNAM(I)
          WRITE(IOUT,102)TRIM(AMESSAGE),PARNAM(I)
          TERMINATE2 = .TRUE.
        ENDIF
        IF (PVAL(I) >= PVALMAXC(I)) THEN
          AMESSAGE= &
          ' CONSTRAIN IS TRUE & STARTING VALUE IS >= CONSTRAINT FOR: '
          WRITE(*,102)TRIM(AMESSAGE),PARNAM(I)
          WRITE(IOUT,102)TRIM(AMESSAGE),PARNAM(I)
          TERMINATE2 = .TRUE.
        ENDIF
      ENDIF
      IF(I == NPS .AND. TERMINATE) THEN
        AMESSAGE='TERMINATING: PLEASE CORRECT RELATIVE VALUES OF CONSTRAINTS'
        WRITE(*,100)TRIM(AMESSAGE)
        WRITE(IOUT,100)TRIM(AMESSAGE)
      ENDIF
      IF(I == NPS .AND. TERMINATE2) THEN
        AMESSAGE='TERMINATING: PLEASE CORRECT STARTING OR CONSTRAINT VALUES'
        WRITE(*,100)TRIM(AMESSAGE)
        WRITE(IOUT,100)TRIM(AMESSAGE)
      ENDIF
      IF(I == NPS .AND. (TERMINATE .OR. TERMINATE2)) THEN
        AMESSAGE=' '
        CALL UTL_STOP(' ')
      ENDIF
      IF (TRANSFORM(I)) LN(I)=1
      IF (ISENMETHOD(I) == 1) SENTYPE = "YES"
      IF (ISENMETHOD(I) == -1) THEN
          ISENMETHOD(I) = 0
          ICONVERT_DERIV(I) = 0
      ENDIF
      IF ((ISENMETHOD(I) .EQ. 0) .AND. (.NOT. FORSENS)) THEN
        IF(RESIDONLY .OR. LINEARITY) THEN
          CONTINUE
        ELSE
          WRITE(*,100)('SENMETHOD INDICATES MODEL SENSITIVITIES: ')
          WRITE(IOUT,100)('SENMETHOD INDICATES MODEL SENSITIVITIES: ')
          WRITE(*,101) &
                 ('DERIVATIVES INTERFACE and FORWARD&DER COMMAND ARE NEEDED')
          WRITE(IOUT,101) &
                 ('DERIVATIVES INTERFACE and FORWARD&DER COMMAND ARE NEEDED')
          CALL UTL_STOP('TERMINATING, NEED MODEL SENSITIVITIES')
        ENDIF
      ENDIF
      IF (PADJ(I)) THEN
        NPE = NPE + 1
        NPERD = NPE
        IF(I > NPSWOP .AND. PADJ(I)) NPEFP = NPEFP + 1
        PINCR(I) = 1
        NPERTURB_CYCLES = NPERTURB_CYCLES + ISENMETHOD(I)
        IF(ISENMETHOD(I) > 0)NPERTURB_CYCLES_FINAL = NPERTURB_CYCLES_FINAL + 2
        IF (BSCAL(I).LT.0.0) THEN
          WRITE(IOUT,210) PARNAM(I)
          IERR = IERR+1
        ENDIF
        IF (LN(I).GT.0) THEN
          LN(I) = 1
          ITRANS(I) = 1
        ELSEIF (LN(I).LE.0) THEN
          LN(I) = 0
          ITRANS(I) = 0
        ENDIF
      ELSE
        ITRANS(I) = -10000
      ENDIF
      IF(RESIDONLY) THEN
        NPERTURB_CYCLES = 0
        NPERTURB_CYCLES_FINAL = 0
      ENDIF
    ENDDO
    IF(NPS > NPSWOP) THEN
      DO J=NPSWOP+1, NPS
        DO JJ=1,NPSWOP
          IF(UTL_SAMENAME(TRIM(PARNAMLC(J)),TRIM(PARNAMLC(JJ)))) THEN
            WRITE(IOUT,*)
            WRITE(IOUT,*)
            WRITE(IOUT,*) &
            ' PARAMETER NAMES in PARAMETER_DATA_FOR_PREDICTION '
            WRITE(IOUT,*) &
            ' SHOULD DIFFER FROM THOSE IN PARAMETER_DATA, PLEASE CHECK:'
            WRITE(IOUT,*)
            WRITE(IOUT,*)TRIM(PARNAMLC(J))
            WRITE(*,*)
            WRITE(*,*)
            WRITE(*,*) &
            ' PARAMETER NAMES in PARAMETER_DATA_FOR_PREDICTION '
            WRITE(*,*) &
            ' SHOULD DIFFER FROM THOSE IN PARAMETER_DATA, PLEASE CHECK:'
            WRITE(*,*)
            WRITE(*,*)TRIM(PARNAMLC(J))
            CALL UTL_STOP &
            ('ILLEGAL PARAMETER in PARAMETER_DATA_FOR_PREDICTION')
          ENDIF
        ENDDO
      ENDDO
    ENDIF
    IF(NPFP > 0) THEN
      ALLOCATE(PINCFP(NPFP))
      PINCFP = 1
      CALL UTLUCODE_DX_WRITE_PAOPT(NPFP,LN(NP+1:NP+NPFP),OUTNAMPRED, &
           PARNAM(NP+1:NP+NPFP),PINCFP,PVAL(NP+1:NP+NPFP),'_paoptp')
      DEALLOCATE(PINCFP)
    ENDIF
    IF(OPTNLUNC) THEN
      ! read parameter status at end of regression
      CALL UTLUCODE_DX_READ_PAOPT(NPS,OUTNAM,LN,PARNAM,PINCR,PVAL,NPSWOP)
      DO I=1,NPS
        IF(PINCR(I) == 0) THEN
          PARAM_OMIT = .TRUE.
          IF(.NOT. ALLOCATED (POMIT))ALLOCATE(POMIT(NPE-NPERD))
          NPO = NPO +1
          POMIT(NPO) = I
        ENDIF
      ENDDO
!      IF(PREDICT .AND. .NOT. SENSITIVITIES) PARAM_OMIT = .FALSE.
      IF(NPS > NPSWOP) THEN
        IUPR = UTL_DX_OPEN(OUTNAMPRED,'_paoptp','OLD')
        READ(IUPR,*)
        READ(IUPR,*)&
          (PARNAM(NPSWOP+I),PVAL(NPSWOP+I),LN(NPSWOP+I),I=1,NPS-NPSWOP)
      ENDIF
      ALLOCATE(IPTR(NPE))
      KPE = 0
      KPERD = 0
      DO I=1,NPS
        IF(PINCR(I) == 1) THEN
          PADJ(I) = .TRUE.
          KPE = KPE +1
          KPERD = KPERD +1
          IPTR(KPE) = I
        ENDIF
        IF(PINCR(I) == 0) THEN
          PADJ(I) = .TRUE.
          KPE = KPE +1
          IPTR(KPE) = I
        ENDIF
        IF(PINCR(I) < 1) PADJ(I) = .FALSE.
        IF(PINCR(I) < 1) ITRANS(I) = -1
      ENDDO
      NPE = 0
      DO I=1,NPS
        IF (PADJ(I)) NPE = NPE + 1
      ENDDO
      IF(ALLOCATED(IPTR)) DEALLOCATE (IPTR)
      ALLOCATE(IPTR(NPE))
      J = 0
      DO I=1,NPS
        IF (PADJ(I)) THEN
          J = J + 1
          IPTR(J) = I
        ENDIF
      ENDDO
      ALLOCATE(EPTR(KPERD))
      KPERD = 0
      DO I=1,NPS
        IF(PINCR(I) == 1) THEN
          KPERD = KPERD +1
          EPTR(KPERD) = I
        ENDIF
      ENDDO
      IF(ALTSTART) THEN
        ! use values from the parameter values array
        IF(KPV > 0) THEN
          PVAL = PVALINIT
        ELSE
          CALL UTL_STOP &
          ('AlternateStartValues = yes BUT PARAMETER VALUES Block is EMPTY')
        ENDIF
      ELSE
        PVALINIT = PVAL
      ENDIF
    ENDIF
    !   Write parameter information to output file
    WRITE(IOUT,*)' '
    WRITE(IOUT,100)'PARAMETER INFORMATION:'
    CALL UCODE_INI_WRITEPARS(IFAIL,IOUT)
    IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from UCODEMOD ')
    !   Allocate and populate arrays for parameters being estimated or analyzed
    IF (NPE .GT. 0) THEN
      IF(.NOT. ALLOCATED(IPTR)) ALLOCATE (IPTR(NPE))
      ALLOCATE (CSS(NPE),MAG_PERTURB(NPE),PVALSET(NPE),PVALTMP(NPE), &
                PINCSEN(NPE),PINCBND(NPE))
      PINCSEN = 0
      PINCBND = 0
      KPE = 0
      ! IF createinitfiles, determine which parameters were active for the final
      ! iteration, or the last sensitivity run with non-estimated parameters
      ! considered
      IF(CREATEINITFILES) THEN
        ALLOCATE(PALNTP(NPS),PANAMTP(NPS),PAPVALTP(NPS))
        CALL UTLUCODE_DX_READ_DM(IFAIL,OUTNAM, &
                             RDUM,RDUM,RDUM,RDUM, &
                             CDUM,IDUM,RDUM,RDUM,RDUM, &
                             CDUM,CDUM,CDUM,CDUM, &
                             IDUM,IDUM,NPE,NPERD,IDUM,IDUM, &
                             RDUM,RDUM,RDUM,RDUM)
        CALL UTLUCODE_DX_READ_PAOPT(NPS,OUTNAM, &
                                       PALNTP,PANAMTP,PINCR,PAPVALTP,NPSWOP)
        DO I=1,NPS
          IF(PINCR(I) == 0) THEN
            PARAM_OMIT = .TRUE.
            IF(.NOT. ALLOCATED (POMIT))ALLOCATE(POMIT(NPE-NPERD))
            NPO = NPO +1
            POMIT(NPO) = I
          ENDIF
        ENDDO
        PADJ = .FALSE.
        DO I=1,NPS
          IF(PINCR(I) >= 0) PADJ(I) = .TRUE.
        ENDDO
        DEALLOCATE(PALNTP,PANAMTP,PAPVALTP)
      ENDIF
!      IF(PREDICT .AND. .NOT. SENSITIVITIES) PARAM_OMIT = .FALSE.
      IF(.NOT. OPTNLUNC) THEN
        DO I=1,NPS
          IF (PADJ(I)) THEN
            KPE = KPE + 1
            IPTR(KPE) = I
          ENDIF
        ENDDO
      ENDIF
    ELSE
      IF (IVERB .GT. 0) WRITE(IOUT,300)
      300 FORMAT(/,1X,'*** Warning: No parameters for which ADJUSTABLE="YES"',/)
    ENDIF
    IF(OPTIMIZE .OR. OPTNLUNC) OPT = .TRUE.
    IF (RESIDONLY .OR. (SENSITIVITIES .AND. .NOT. OPT)) THEN
      FINALSTATS = .TRUE.
    ENDIF
    IF (SENSITIVITIES) THEN
      IF(.NOT. OPT) THEN
        FINALSTATS = .TRUE.
        DO I=1,NPE
          IF(ISENMETHOD(IPTR(I)) .EQ. 1) FINALSTATS = .FALSE.
        ENDDO
      ENDIF
    ENDIF
    !
    !   Write information for adjustable parameters to output file
!    IF(.NOT. LINEARITY) THEN
      WRITE(IOUT,*)' '
      WRITE(IOUT,100)'INFORMATION FOR ADJUSTABLE PARAMETERS:'
      IF (.NOT. SOSSURFACE) THEN
        CALL UCODE_INI_WRITEEPARS(IFAIL,IOUT)
        IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from UCODEMOD ')
        IF (IERR > 0) THEN
          WRITE(IOUT,200) IERR
          CALL UTL_STOP(' ')
        ENDIF
      ELSE
        WRITE(IOUT,100)'OPTION TO CALCULATE SOS SURFACE WAS SELECTED'
        WRITE(IOUT,104)
        WRITE(IOUT,203)
        DO I=1,NPS
          IF(PADJ(I)) THEN
            IF(SOSINCREMENT(I) < 1)SOSINCREMENT(I)=1
            WRITE(IOUT,230)I,PARNAM(I),SOSINCREMENT(I),PVALMINC(I),PVALMAXC(I)
          ENDIF
        ENDDO
      ENDIF
!    ENDIF
    !
    IF(OPTIMIZE .AND. NPE==0) THEN
      WRITE(IOUT,*)
      WRITE(IOUT,*)' OPTIMIZE = TRUE, but NONE of the Parameters are Adjustable'
      WRITE(IOUT,*)' TERMINATING'
      WRITE(IOUT,*)
      WRITE(*,*)
      WRITE(*,*)' TERMINATING'
      WRITE(*,*)
      CALL UTL_STOP &
      (' OPTIMIZE = TRUE, but NONE of the Parameters are Adjustable')
    ENDIF
    IF(SENSITIVITIES .AND. NPE==0) THEN
      WRITE(IOUT,*)
      WRITE(IOUT,*) &
           ' SENSITIVITIES = TRUE, but NONE of the Parameters are Adjustable'
      WRITE(IOUT,*)' TERMINATING'
      WRITE(IOUT,*)
      WRITE(*,*)
      WRITE(*,*)' TERMINATING'
      WRITE(*,*)
      CALL UTL_STOP &
      (' SENSITIVITIES = TRUE, but NONE of the Parameters are Adjustable')
    ENDIF
    !
    RETURN
  END SUBROUTINE UCODE_INI_PARS
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_INI_PARS_INSTALL (IFAIL,IOUT)
    !   install derived_parameter equations
    USE DATATYPES
    USE EQUATION, ONLY: EQN_INI_INSTALL
    USE GLOBAL_DATA, ONLY: AMESSAGE, BIGDOUBLE, ERRSUB
    USE UTILITIES
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER, INTENT(INOUT)  :: IFAIL
    INTEGER, INTENT(IN)  :: IOUT
    !   Local variables
    INTEGER               :: I
    ! formats
    10 FORMAT(/,80('<'),/)
    15 FORMAT(2X,'DERIVED PARAMETERS',/,5X,'# of DERIVED PARAMETERS = ',I12,/)
    20 FORMAT(3X,'NAME',15X,'EQUATION')
    30 FORMAT(3X,A12,7X,A)
    40 FORMAT(/,80('>'),/)
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_INI_PARS_INSTALL'
    IFAIL = 0
    !
    IF(WRPAREQ) THEN
      WRITE(IOUT,10)
      WRITE(IOUT,15)NPD
      WRITE(IOUT,20)
    ENDIF
    DO I=1,NPD
      IF(WRPAREQ) WRITE(IOUT,30)DERPARNAME(I),TRIM(DERPAREQN(I))
      CALL UTL_CASE(DERPARNAME(I),DERPARNAMELC(I),-1) ! Store names as lowercase
      CALL EQN_INI_INSTALL(IFAIL,I,DERPARNAMELC(I),DERPAREQN(I))
      IF (IFAIL.NE.0) THEN
        CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
        CALL UTL_STOP('EQN_INI_INSTALL reported failure')
      ENDIF
    ENDDO
    IF(WRPAREQ) WRITE(IOUT,40)
    RETURN
  END SUBROUTINE UCODE_INI_PARS_INSTALL
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_INI_WRITEPARS(IFAIL,IOUT)
    !   Print parameter-related data
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                         INTENT(INOUT) :: IFAIL
    INTEGER,                         INTENT(IN)    :: IOUT
    !   Local variables
    INTEGER :: I
    !
    201 FORMAT(/,   &
        1X,'No. ',1X,'Param. name ',4X,'Group',8X,'Value',7X,'Lower value',  &
        3X,'Upper value',4X,'Adj?',/,   &
        1X,4('-'),1X,12('-'),1X,12('-'),3(2x,12('-')),2X,5('-'))
    202 FORMAT(/,   &
        1X,'No. ',1X,'Param. name ',2X,'LN',3X,'SCALEPVAL' &
        ,3X,'PERTURB',4X,'MAXCHANGE',4X,'TOLPAR',/,   &
        1X,4('-'),1X,12('-'),2X,  &
        '--',2X,10('-'),2X,10('-'),2X,9('-'),2X,9('-'))
    210 FORMAT(1X,I4,1X,A,1X,A,3(1X,1PG13.6),3X,A1)
    220 FORMAT(1X,I4,1X,A,2X,I2,2X,1PG10.3,2X,1PG10.3,1X,1PG10.3,1X, &
               1PG10.3)
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_INI_WRITEPARS'
    IFAIL = 0
    !   Write data for each parameter in the parameter set
    WRITE(IOUT,201)
    DO I=1,NPS
      IF(PADJ(I)) THEN
        IF(CONSTRAIN(I)) CONSTRAINL = .TRUE.
        WRITE(IOUT,210)I,PARNAM(I),PARGP(I),PVAL(I),PVALMIN(I),    &
          PVALMAX(I),'Y'
      ELSE
        WRITE(IOUT,210)I,PARNAM(I),PARGP(I),PVAL(I),PVALMIN(I),    &
          PVALMAX(I),'N'
      ENDIF
    ENDDO
    WRITE(IOUT,202)
    DO I=1,NPS
      WRITE(IOUT,220)I,PARNAM(I),LN(I),BSCAL(I), &
        PERTURB(I),PMAXCHANGE(I),PTOLPAR(I)
    ENDDO
    !
    RETURN
  END SUBROUTINE UCODE_INI_WRITEPARS
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_INI_WRITEEPARS(IFAIL,IOUT)
    !   Print parameter-related data
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                         INTENT(INOUT) :: IFAIL
    INTEGER,                         INTENT(IN)    :: IOUT
    ! Local variables
    INTEGER :: I
    INTEGER :: J
    CHARACTER(LEN=3) :: CHYN
    !
    201 FORMAT(/,   &
        1X,'Param. name ',4X,'Group',10X,'Value',7X,'Lower value',  &
        4X,'Upper value',2X,'Par. no.',/,   &
        1X,12('-'),1X,12('-'),3(2X,13('-')),2X,7('-'))
    202 FORMAT(/,15X,'Sens',/,  &
        1X,'Param. name ',2X,'Method',2X,'LN',3X,'SCALEPVAL', &
        3X,'PERTURB',4X,'MAXCHANGE',4X,'TOLPAR',/,   &
        1X,12('-'),2X,6('-'),2X,'--',   &
        2X,10('-'),2X,10('-'),2X,9('-'),2X,10('-'))
    203 FORMAT(/,   &
        1X,'Param. name ',2X,'Constrain',2X,'Lower Constraint', &
        2X,'Upper Constraint',/,   &
        1X,12('-'),2X,9('-'),2X,16('-'),2X,16('-'))
    204 FORMAT(/,   &
        1X,'Param. name ',2X,'Calculate NonLinear Interval?',/, &
        1X,12('-'),2X,29('-'))
    211 FORMAT(1X,A,1X,A,3(2X,1PG13.6),2X,I6)
    212 FORMAT(1X,A,5X,I2,3X,I2,2X,1PG10.3,2X,1PG10.3,1X,1PG10.3,2X, &
               1PG10.3)
    230 FORMAT(1X,A,5X,A1,7X,1PG15.4,2X,1PG15.4)
    240 FORMAT(1X,A,5X,A3)
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_INI_WRITEEPARS'
    IFAIL = 0
    !   Write data for the set of parameters for which ADJUSTABLE=yes
    WRITE(IOUT,201)
    DO I=1,NPE
      WRITE(IOUT,211)PARNAM(IPTR(I)),PARGP(IPTR(I)),PVAL(IPTR(I)),    &
          PVALMIN(IPTR(I)),PVALMAX(IPTR(I)),IPTR(I)
    ENDDO
    WRITE(IOUT,202)
    DO I=1,NPE
      WRITE(IOUT,212)PARNAM(IPTR(I)),ISENMETHODPRT(IPTR(I)),LN(IPTR(I)), &
      BSCAL(IPTR(I)),PERTURB(IPTR(I)),PMAXCHANGE(IPTR(I)),PTOLPAR(IPTR(I))
    ENDDO
    IF(CONSTRAINL) THEN
      WRITE(IOUT,203)
      DO I=1,NPS
        IF(CONSTRAIN(I)) THEN
          WRITE(IOUT,230)PARNAM(I),'Y',PVALMINC(I),PVALMAXC(I)
        ELSE
          WRITE(IOUT,230)PARNAM(I),'N'
        ENDIF
      ENDDO
    ENDIF
    IF(OPTNLUNC) THEN
      WRITE(IOUT,204)
      DO I=1,NPE
        IF(PADJ(IPTR(I))) THEN
          IF(NONLININT(IPTR(I))) THEN
            DO J=1,KPERD
              IF(IPTR(I) == EPTR(J)) THEN
                CHYN = 'Yes'
                NONLINPARAM = .TRUE.
                WRITE(IOUT,240)PARNAM(IPTR(I)),CHYN
                EXIT
              ELSE
                CYCLE
              ENDIF
              IF(J == KPERD) THEN
                CHYN = 'No'
                NONLININT(IPTR(I)) = .FALSE.
                WRITE(IOUT,240)PARNAM(IPTR(I)),CHYN
                EXIT
              ENDIF
            ENDDO
          ELSE
            CHYN = 'No'
            WRITE(IOUT,240)PARNAM(IPTR(I)),CHYN
          ENDIF
        ELSE
          NONLININT(IPTR(I)) = .FALSE.
        ENDIF
      ENDDO
    ENDIF
    !
    RETURN
  END SUBROUTINE UCODE_INI_WRITEEPARS
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_INI_OUTPUTFILES(IFAIL,INUNIT,IOUT)
    !   Allocate and initialize arrays required for model output
    USE GLOBAL_DATA, ONLY: AMESSAGE, ERRSUB, IVERB
    USE UTILITIES
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER, INTENT(INOUT) :: IFAIL
    INTEGER, INTENT(IN) :: INUNIT
    INTEGER, INTENT(IN) :: IOUT
    !
    !   Local variables
    INTEGER :: I, IERR, MORE
    TYPE (LLIST), POINTER :: FILOUTHEAD ! Ptr to head of first list (model-output files)
    TYPE (LLIST), POINTER :: TAIL
    ! Initialize
    NULLIFY(FILOUTHEAD,TAIL)
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_INI_OUTPUTFILES'
    IFAIL = 0
    !   Make call to UTLREADBLOCK to read input blocks containing names
    !   of model-output and corresponding instruction files
    NUMOUTFILESE = 0
    CALL UTL_READBLOCK(NMODOUTCOLS,'MODEL_OUTPUT_FILES',MODOUTCOL,INUNIT,IOUT, &
        'MODOUTFILE',.TRUE.,FILOUTHEAD,TAIL,   &
        NUMOUTFILESE)
    !
    IF (NUMOUTFILESE.LE.0) THEN
      WRITE(AMESSAGE,200)
      200 FORMAT('Error: No model-output files were specified.')
      IFAIL=1
      CALL UTL_STOP(' ')
    END IF
    !
    !   Allocate arrays for storing model-output file names
    ALLOCATE (MODOUTFILESE(NUMOUTFILESE),STAT=IERR)
    !
    IF(IERR.NE.0)THEN
      WRITE(AMESSAGE,220) TRIM(ERRSUB)
      220 FORMAT(A,' cannot allocate sufficient memory to store',   &
             ' model-interface filenames.')
      IFAIL=1
      CALL UTL_STOP(' ')
    END IF
    !
    MODOUTFILESE=' '
    !
    !   Populate arrays of file names by traversing the linked lists
    CALL UTL_FILTERLIST(FILOUTHEAD,IOUT,'MODOUTFILE',NUMOUTFILESE,IFAIL,   &
                       MODOUTFILESE,MORE)
    IF(IFAIL > 0) CALL UTL_STOP(' ERROR IN UTL_FILTERLIST called from UCODEMOD')
    ALLOCATE(INSFILESE(NUMOUTFILESE),MRKDELSE(NUMOUTFILESE),STAT=IERR)
    CALL UTL_FILTERLIST(FILOUTHEAD,IOUT,'INSFILE',NUMOUTFILESE,IFAIL, &
                        INSFILESE,MORE)
    IF (IFAIL.NE.0 .OR. IERR.NE.0) CALL UTL_STOP &
                              (' ERROR IN UTL_FILTERLIST called from UCODEMOD')
    !
    !   Echo input
    IF (IVERB.GE.3) THEN
      WRITE(IOUT,250)
      250 FORMAT(//,1X,'MODEL-OUTPUT AND CORRESPONDING INSTRUCTION FILES:',/)
      WRITE(IOUT,260)(TRIM(MODOUTFILESE(I)),TRIM(INSFILESE(I)),I=1,NUMOUTFILESE)
      260 FORMAT(1X,'Model-output file: ',A,3X,'Instruction file: ',A)
    ENDIF
    !
    !   Deallocate linked lists

    RETURN
  END SUBROUTINE UCODE_INI_OUTPUTFILES
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_EXE_SELECT(CTRLJOB,FINALSTATS,ITERP,ICOMMAND)
    !   Modify ICOMMAND to avoid making a model run in selected
    !   circumstances
    !
    !   Argument-list variables
    CHARACTER(LEN=*), INTENT(IN)    :: CTRLJOB
    LOGICAL,          INTENT(IN)    :: FINALSTATS
    INTEGER,          INTENT(IN)    :: ITERP
    INTEGER,          INTENT(INOUT) :: ICOMMAND
    !
    IF (ICOMMAND==0) RETURN
    IF (CTRLJOB .EQ. 'GAUSS-NEWTON') THEN
      ICOMMAND = 0
      RETURN
    ENDIF
    IF (.NOT. (CTRLJOB .EQ. 'SENSITIVITY' .AND. NPERTURB_CYCLES==0)) THEN
      IF(ITERP > 0 .AND. FINALSTATS .AND. (.NOT. LOWEST) .AND. &
          (CTRLJOB .EQ. 'FORWARD&SENS' .AND. NPERTURB_CYCLES==0)) THEN
        ICOMMAND = 0
        IF(CTRLJOB .EQ. 'FORWARD&SENS' .AND. LOWEST_MOD) ICOMMAND = 2
      ELSE
        CONTINUE ! Allow model run to be made
      ENDIF
    ENDIF
    RETURN
  END SUBROUTINE UCODE_EXE_SELECT
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_INI_CHECK_USEFLAG(IOUT,MPR,MPRWOP)
    USE DEPENDENTS, ONLY: LLPTRDEPCOPY, LLPTRPREDCOPY, NTOTOBS, NTOTPRED
    USE PRIOR_INFORMATION, ONLY: LLPTRGPPRIOR, LLPTRGPPRIORFP, &
                         LLPTRPRIORCOPY, LLPTRPRIORFPCOPY, NPRIGPS, NPRIGPSFP
    IMPLICIT NONE
    INTEGER,                           INTENT(IN)    :: MPR     !all prior
    INTEGER,                           INTENT(IN)    :: MPRWOP  !prior without prior for predictions
    !
    !   Argument-list variables
    INTEGER, INTENT(IN) :: IOUT
    ! Local variable
    CHARACTER(20), ALLOCATABLE, DIMENSION(:) :: CHECK_USEFLAG ! (default=blank)
    LOGICAL, ALLOCATABLE, DIMENSION(:) :: PRI_USEFLAG ! (default=true)
    INTEGER :: I = 0
    INTEGER :: IERR = 0
    INTEGER, DIMENSION(6) :: IFLAG
    INTEGER :: MORE = 0
    333 FORMAT( &
    1X,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',//, &
    1X,'WARNING, A common input misunderstanding occurred:',/, &
    3X,'USEFLAG cannot be defined for individual observations',/, &
    3X,'and predictions. It must be defined by group.',//, &
    1X,'PLEASE NOTE it is ESSENTIAL that the USER REVIEW OUTPUT',/, &
    3X,'to ensure that the code is performing as desired.',//, &
    1X,'UNLIKE MANY OTHER CODES the JUPITER API allows great flexibility',/, &
    3X,'to use the same input files with many codes (e.g. UCODE and other',/, &
    3X,'parameter estimation codes) for easy comparison.  HOWEVER if a ',/, &
    3X,'user misspells a keyword or defines a keyword in a block that does',/, &
    3X,'not use that keyword it will be ignored and the default will be used.' &
      ,//, &
    1X,'This USEFLAG error was so frequent, a special check was developed',/, &
    3X,'and used as an opportunity to remind the user to check the output',/, &
    3X,'carefully to confirm the desired inputs are actually being read',/, &
    3X,'and used.',//, &
    1X,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',//)
    334 FORMAT(//, &
    1X,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',//, &
    1X,'A problem occurred in either the',/, &
    1X,' OBSERVATION_DATA and/or DERIVED_OBSERVATIONS input block(s).',/, &
    1X,'Please read explanation below',//,&
    1X,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',//)
    335 FORMAT(//, &
    1X,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',//, &
    1X,'A problem occurred in either the',/, &
    1X,' PREDICTION_DATA and/or DERIVED_PREDICTIONS input block(s).',/, &
    1X,'Please read explanation below',//,&
    1X,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',//)
    336 FORMAT(//, &
    1X,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',//, &
    1X,'A problem occurred in the',/, &
    1X,' LINEAR_PRIOR_INFORMATION input block.',/, &
    1X,'Please read explanation below',//,&
    1X,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',//)
    337 FORMAT(//, &
    1X,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',//, &
    1X,'A problem occurred in the',/, &
    1X,' LINEAR_PRIOR_INFORMATION_FOR_PREDICTION input block.',/, &
    1X,'Please read explanation below',//,&
    1X,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',//)
    338 FORMAT(//, &
    1X,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',//, &
    1X,'A problem occurred in the',/, &
    1X,' PRIOR_INFORMATION_GROUPS input block.',/, &
    1X,'Please read explanation below',//,&
    1X,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',//)
    339 FORMAT(//, &
    1X,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',//, &
    1X,'A problem occurred in the',/, &
    1X,' PRIOR_INFORMATION_GROUPS_FOR_PREDICTION input block.',/, &
    1X,'Please read explanation below',//,&
    1X,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',//)
    340 FORMAT( &
    1X,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',//, &
    1X,'ERROR:',//, &
    1X,'Contrary to original documentation USEFLAG cannot be defined for',/, &
    3X,'prior information.',//, &
    1X,'If the prior information with useflag=no will not be used for',/, &
    3X,'future runs, then omit that information from the ucode input.',//, &
    1X,'If the user is experimenting with varying combinations of',/, &
    3X,'prior information, it is best to use the FILES input method.',//, &
    1X,'When using the FILES method, many different combinations of prior',/, &
    3X,'information input blocks can be stored in files with different',/, &
    3X,'names. Then use of different prior information can be evaluated',/, &
    3X,'by simply changing the name of the file containing the prior',/, &
    3X,'information input in the main ucode input file.',//, &
    1X,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',//)
    IFLAG = 0
    ! check observation_data & derived_observations blocks for misuse of useflag
    IF(NTOTOBS > 0) THEN
      ALLOCATE(CHECK_USEFLAG(NTOTOBS))
      CHECK_USEFLAG = ' '
      CALL UTL_FILTERLIST(LLPTRDEPCOPY,IOUT,'USEFLAG',NTOTOBS,IERR, &
                          CHECK_USEFLAG,MORE)
      DO I=1,NTOTOBS
        IF(CHECK_USEFLAG(I) .NE. ' ') THEN
          IFLAG(1) = 1
        ENDIF
      ENDDO
      IF(ALLOCATED(CHECK_USEFLAG)) DEALLOCATE(CHECK_USEFLAG)
      CALL TYP_DEALLOC(LLPTRDEPCOPY)
    ENDIF
    ! check prediction_data & derived_predictions blocks for misuse of useflag
    IF(NTOTPRED > 0) THEN
      ALLOCATE(CHECK_USEFLAG(NTOTPRED))
      CHECK_USEFLAG = ' '
      IF(PREDICTION) THEN
        CALL UTL_FILTERLIST(LLPTRDEPCOPY,IOUT,'USEFLAG',NTOTPRED,IERR, &
                            CHECK_USEFLAG,MORE)
        DO I=1,NTOTPRED
          IF(CHECK_USEFLAG(I) .NE. ' ') THEN
            IFLAG(2) = 1
          ENDIF
        ENDDO
        IF(ALLOCATED(CHECK_USEFLAG)) DEALLOCATE(CHECK_USEFLAG)
        CALL TYP_DEALLOC(LLPTRDEPCOPY)
      ELSE
        CALL UTL_FILTERLIST(LLPTRPREDCOPY,IOUT,'USEFLAG',NTOTPRED,IERR, &
                            CHECK_USEFLAG,MORE)
        DO I=1,NTOTPRED
          IF(CHECK_USEFLAG(I) .NE. ' ') THEN
            IFLAG(2) = 1
          ENDIF
        ENDDO
        IF(ALLOCATED(CHECK_USEFLAG)) DEALLOCATE(CHECK_USEFLAG)
        CALL TYP_DEALLOC(LLPTRPREDCOPY)
      ENDIF
    ENDIF
    ! check Linear_Prior_Groups block for useflag false
    IF(NPRIGPS-NPRIGPSFP > 0) THEN
      ALLOCATE(PRI_USEFLAG(NPRIGPS-NPRIGPSFP))
      PRI_USEFLAG = .TRUE.
      CALL UTL_FILTERLIST(LLPTRGPPRIOR,IOUT,'USEFLAG',NPRIGPS-NPRIGPSFP, &
                          IERR,PRI_USEFLAG,MORE)
      DO I=1,NPRIGPS-NPRIGPSFP
        IF(.NOT. PRI_USEFLAG(I)) THEN
          IFLAG(5) = 1
        ENDIF
      ENDDO
      IF(ALLOCATED(PRI_USEFLAG)) DEALLOCATE(PRI_USEFLAG)
      CALL TYP_DEALLOC(LLPTRGPPRIOR)
    ENDIF
    ! check Linear_Prior_Groups_FP block for useflag false
    IF(NPRIGPSFP > 0) THEN
      ALLOCATE(PRI_USEFLAG(NPRIGPSFP))
      PRI_USEFLAG = .TRUE.
      CALL UTL_FILTERLIST(LLPTRGPPRIORFP,IOUT,'USEFLAG',NPRIGPSFP, &
                          IERR,PRI_USEFLAG,MORE)
      DO I=1,NPRIGPSFP
        IF(.NOT. PRI_USEFLAG(I)) THEN
          IFLAG(6) = 1
        ENDIF
      ENDDO
      IF(ALLOCATED(PRI_USEFLAG)) DEALLOCATE(PRI_USEFLAG)
      CALL TYP_DEALLOC(LLPTRGPPRIORFP)
    ENDIF

    IF(MPRWOP > 0) THEN
      ALLOCATE(CHECK_USEFLAG(MPRWOP))
      CHECK_USEFLAG = ' '
      CALL UTL_FILTERLIST(LLPTRPRIORCOPY,IOUT,'USEFLAG',MPRWOP,IERR, &
                          CHECK_USEFLAG,MORE)
      DO I=1,MPRWOP
        IF(CHECK_USEFLAG(I) .NE. ' ') THEN
          IFLAG(3) = 1
        ENDIF
      ENDDO
      IF(ALLOCATED(CHECK_USEFLAG)) DEALLOCATE(CHECK_USEFLAG)
      CALL TYP_DEALLOC(LLPTRPRIORCOPY)
    ENDIF
    ! check Linear_Prior_Information_For_Prediction block for misuse of useflag
    IF(MPR-MPRWOP > 0) THEN
      ALLOCATE(CHECK_USEFLAG(MPR-MPRWOP))
      CHECK_USEFLAG = ' '
      CALL UTL_FILTERLIST(LLPTRPRIORFPCOPY,IOUT,'USEFLAG',MPR-MPRWOP,IERR, &
                          CHECK_USEFLAG,MORE)
      DO I=1,MPR-MPRWOP
        IF(CHECK_USEFLAG(I) .NE. ' ') THEN
          IFLAG(4) = 1
        ENDIF
      ENDDO
      IF(ALLOCATED(CHECK_USEFLAG)) DEALLOCATE(CHECK_USEFLAG)
      CALL TYP_DEALLOC(LLPTRPRIORFPCOPY)
    ENDIF
    IF(IFLAG(1) > 0) THEN
      WRITE(IOUT,334)
      WRITE(*,334)
    ENDIF
    IF(IFLAG(2) > 0) THEN
      WRITE(IOUT,335)
      WRITE(*,335)
    ENDIF
    IF(IFLAG(1)+IFLAG(2) > 0) THEN
      WRITE(IOUT,333)
      WRITE(*,333)
    ENDIF
    IF(IFLAG(3) > 0) THEN
      WRITE(IOUT,336)
      WRITE(*,336)
    ENDIF
    IF(IFLAG(4) > 0) THEN
      WRITE(IOUT,337)
      WRITE(*,337)
    ENDIF
    IF(IFLAG(5) > 0) THEN
      WRITE(IOUT,338)
      WRITE(*,338)
    ENDIF
    IF(IFLAG(6) > 0) THEN
      WRITE(IOUT,339)
      WRITE(*,339)
    ENDIF
    IF(IFLAG(3)+IFLAG(4)+IFLAG(5)+IFLAG(6) > 0) THEN
      WRITE(IOUT,340)
      WRITE(*,340)
    ENDIF
    IF(IFLAG(1)+IFLAG(2)+IFLAG(3)+IFLAG(4)+IFLAG(5)+IFLAG(6) > 0) THEN
      CALL UTL_STOP()
    ENDIF
    RETURN
  END SUBROUTINE UCODE_INI_CHECK_USEFLAG
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_INI_SET(TESTCOR)
    IMPLICIT NONE
    !
    !   Argument-list variables
    LOGICAL,                        INTENT(IN) :: TESTCOR
    LCOR = TESTCOR
    RETURN
  END SUBROUTINE UCODE_INI_SET
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_DEF(IFAIL,JOBDIM,JOBLEN,CTRLJOB,  &
                        FINALSTATSDONE,IFO,IOUT, &
                        LINEARITYDONE,MAXITER,OUTNAM, &
                        SENSDONE,FINALSTATS,INTDONE,INTERVALLOOPS,ITERP, &
                        TRUSTREGION, &
                        LINEARITYLOOPS, &
                        CTRLDONE,ICTRL, &
                        KPPL,NUMPPL)
    !   Determine job of current iteration of control loop
    !
    USE BASIC, ONLY: FORSENS
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                             INTENT(INOUT)   :: IFAIL
    INTEGER,                             INTENT(IN)      :: JOBDIM
    INTEGER,                             INTENT(IN)      :: JOBLEN
    CHARACTER(LEN=JOBLEN), DIMENSION(JOBDIM), INTENT(IN) :: CTRLJOB
    LOGICAL,                             INTENT(IN)      :: FINALSTATSDONE
    INTEGER,                             INTENT(IN)      :: IFO
    INTEGER,                             INTENT(IN)      :: IOUT
    LOGICAL,                             INTENT(IN)      :: LINEARITYDONE
    INTEGER,                             INTENT(IN)      :: MAXITER
    CHARACTER(LEN=MAX_STRING_LEN),       INTENT(IN)      :: OUTNAM
    LOGICAL,                             INTENT(IN)      :: SENSDONE
    LOGICAL,                             INTENT(IN)      :: TRUSTREGION
    LOGICAL,                             INTENT(INOUT)   :: CTRLDONE
    INTEGER,                             INTENT(INOUT)   :: ICTRL
    LOGICAL,                             INTENT(INOUT)   :: FINALSTATS
    LOGICAL,                             INTENT(INOUT)   :: INTDONE
    INTEGER,                             INTENT(INOUT)   :: INTERVALLOOPS
    INTEGER,                             INTENT(INOUT)   :: ITERP
    INTEGER,                             INTENT(INOUT)   :: LINEARITYLOOPS
    INTEGER,                             INTENT(OUT)     :: KPPL
    INTEGER,                             INTENT(OUT)     :: NUMPPL
    !
    !   Local variables
    CHARACTER(LEN=JOBLEN) :: JOB
    INTEGER :: I
    INTEGER :: IP
    LOGICAL :: CHECKNEW
    !
    !   Format statements
    100 FORMAT(/,1X,'Job of current iteration of Control Loop is: ',A)
    101 FORMAT(1X,'CALCULATING RESIDUALS FOR CURRENT PARAMETER VALUES')
    102 FORMAT(/)
    103 FORMAT(/,1X,'CALCULATING SIMULATED EQUIVALENTS FOR SET #',I4, &
               ' OF LINEARITY PARAMETER VALUES FROM _b1')
    104 FORMAT(1X,'Purpose: To Calculate statistics for optimal parameters')
    105 FORMAT(/,1X,' DONE SOS-SURFACE LOOP: ',I8,' OF ',I8,/,2(/,1X,79('=')))
    106 FORMAT(/,1X,'CALCULATING SIMULATED EQUIVALENTS FOR SET #',I4,/, &
              ' OF ADVANCED LINEARITY PARAMETER VALUES FROM _b3 (conf or pred)')
    107 FORMAT(/,1X,'CALCULATING SIMULATED EQUIVALENTS FOR SET #',I4,/, &
           ' OF ADVANCED LINEARITY PARAMETER VALUES FROM _b1adv (conf or pred)')
    130 FORMAT(2002(G18.9,:))
    131 FORMAT &
    (/,' Sum-of-Squared-Weighted-Residuals Surface Noted above was computed', &
     /,' for the following Parameters: ',/,6(3X,A12,:))
    132 FORMAT(6(1X,G15.6,:))
    402 FORMAT &
    (/,' CALCULATING PREDICTION SENSITIVITIES FOR OPTIMAL PARAMETERS ')
    403 FORMAT &
    (/,' CALCULATING SENSITIVITIES FOR PARAMETERS ESTIMATED IN ITERATION:',I5)
    404 FORMAT(/,' CALCULATING SENSITIVITIES FOR THE FINAL PARAMETERS')
    405 FORMAT(/,' CALCULATING SENSITIVITIES FOR THE INITIAL PARAMETERS')
    406 FORMAT(/,' RETRIEVING SENSITIVITIES FOR THE INITIAL PARAMETERS')
    500 FORMAT(/, &
        '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',/, &
        '  REGRESSION DID NOT CONVERGE ',/, &
        '  FINAL STATISTICS ARE NOT PRINTED',/, &
        '  BECAUSE STATS_ON_NONCONVERGE OF REG_GN_CONTROLS IS SET TO NO ',/, &
        '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
    600 FORMAT (80('*'),/, &
        1X,'COMPUTING SIMULATED EQUIVALENTS FOR LINEARITY PARAMETER SETS',/, &
        1X,'              WRITE _b2 USING VALUES FROM _b1',/,80('*'),/)
    601 FORMAT (80('*'),/, &
        1X,'COMPUTING SIMULATED EQUIVALENTS FOR LINEARITY PARAMETER SETS',/, &
        1X,'              WRITE _b2adv USING VALUES FROM _b1adv, and',/, &
        1X,'              WRITE _b4    USING VALUES FROM    _b3',/,80('*'),/)
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_DEF'
    IFAIL = 0
    KPPL = 0
    NUMPPL = 0
    !
    !   Advance counter ICTRL if appropriate, based on job last iteration
    IF (ICTRL .EQ. 0) THEN
      ICTRL = 1
    ELSE
      JOB = CTRLJOB(ICTRL)
      IF (JOB .EQ. 'FORWARD') THEN
        IF(.NOT. LINEARITY) ICTRL = ICTRL + 1
        IF(LOWEST_DIFFERS) THEN
          ICTRL = ICTRL - 1
          LOWEST_DIFFERS = .FALSE.
        ENDIF
        IF(LINEARITYDONE) ICTRL = ICTRL + 4
        IF(SOSSURFACE) THEN
          ICTRL = 1
          IF(SOSSURFDONE) THEN
            WRITE(IUSOS,130)RSQD(1),(PVALTMP(I),I=1,NPE)
!            WRITE(IOUT,131)(PARNAM(IPTR(I)),I=1,NPE)
!            WRITE(IOUT,132)(PVALTMP(I),I=1,NPE)
            WRITE(*,105)SOSSURFLOOPS,TOTSOSLOOPS
!            WRITE(IOUT,105)SOSSURFLOOPS,TOTSOSLOOPS
            ICTRL = ICTRL + 4
          ENDIF
        ENDIF
        IF(INTDONE) THEN
          INTDONE = .FALSE.
          ! Nonlinear Intervals, so start a new regression unless all
          ! intervals have been evaluated
          IF(INTERVALLOOPS > NUMINTERVALS) THEN
            ICTRL = 5
          ELSE
            ICTRL = 1
            INTERVALLOOPS = INTERVALLOOPS + 1
            LOWEST = .FALSE.
            FINALSTATS = .FALSE.
            ITERP = 0
            PVAL = PVALINIT
            NUMPPL = 1
            IF(INTERVALLOOPS > NUMINTERVALS) THEN
              CTRLDONE = .TRUE.
            ELSE
              RETURN
            ENDIF
          ENDIF
        ENDIF
      ELSEIF (JOB .EQ. 'FORWARD&SENS') THEN
        ICTRL = ICTRL + 1
        IF(LOWEST_DIFFERS) THEN
          ICTRL = ICTRL - 1
          LOWEST_DIFFERS = .FALSE.
        ENDIF
        IF(INTDONE) THEN
          INTDONE = .FALSE.
          ! Nonlinear Intervals, so start a new regression unless all
          ! intervals have been evaluated
          IF(INTERVALLOOPS > NUMINTERVALS) THEN
            ICTRL = 5
          ELSE
            ICTRL = 1
            INTERVALLOOPS = INTERVALLOOPS + 1
            LOWEST = .FALSE.
            FINALSTATS = .FALSE.
            ITERP = 0
            PVAL = PVALINIT
            NUMPPL = 1
            IF(INTERVALLOOPS > NUMINTERVALS) THEN
              CTRLDONE = .TRUE.
            ELSE
              RETURN
            ENDIF
          ENDIF
        ENDIF
      ELSEIF (JOB .EQ. 'SENSITIVITY') THEN
        IF (SENSDONE .AND. FINALSTATS) THEN
           ICTRL = ICTRL + 2
        ELSEIF (SENSDONE) THEN
          ICTRL = ICTRL + 1
        ENDIF
      ELSEIF (JOB .EQ. 'GAUSS-NEWTON') THEN
        ICTRL = ICTRL + 1
        IF (LOWEST) THEN
          ICTRL = 1
        ENDIF
        IF (.NOT. FINALSTATSDONE) THEN
          IF (FINALSTATS) THEN
            IF(IFO ==3 .AND. (.NOT. STATS_ON_NONCONVERGE)) THEN
              ICTRL = 5
              CALL UTLUCODE_DX_WRITE_PAOPT(NPS,LN,OUTNAM,PARNAM, &
                                             PINCR,PVAL,'_palast')
              WRITE(IOUT,500)
              WRITE(*,500)
            ELSE
              ICTRL = 1
            ENDIF
          ELSE
            ICTRL = 1
          ENDIF
        ELSE
          ICTRL = ICTRL + 1
        ENDIF
      ELSE
        ICTRL = ICTRL + 1
      ENDIF
    ENDIF
    ! forward model already simulated in trust region update
    IF(TRUSTREGION .AND. ITERP .GT. 0 .AND. CTRLJOB(ICTRL)=='FORWARD') &
       ICTRL = ICTRL + 1
    IF(TRUSTREGION .AND. ITERP .GT. 0 .AND. &
       CTRLJOB(ICTRL)=='FORWARD&SENS') ICTRL = ICTRL + 1

    ! Skip over unsupported and unrequired jobs
    CHECKNEW = .TRUE.
    DO WHILE (CHECKNEW)
      CHECKNEW = .FALSE.
      IF (CTRLJOB(ICTRL)=='FORWARD') THEN
        IF (FORSENS .AND. .NOT. RESIDONLY) THEN
          ICTRL = ICTRL + 1
        ENDIF
       ! If LINEARITY, 2*NPE or 2*NOINT FORWARD RUNS
        IF (LINEARITY) THEN
          IF (LINEARITYLOOPS .EQ. 0) THEN
            WRITE(IOUT,102)
            IF(LINEARITYADV) THEN
              WRITE(IOUT,601)
            ELSE
              WRITE(IOUT,600)
            ENDIF
          ENDIF
          LINEARITYLOOPS= LINEARITYLOOPS+1
          ICTRL = 1
          IF (.NOT. LINEARITYDONE) THEN
            IF(LINEARITYADV) THEN
              IF(LINEARITYLOOPS > 1 .AND. LINEARITYLOOPS > NOINT*2) THEN
                WRITE(IOUT,107)LINEARITYLOOPS-(NOINT*2)
                WRITE(*,107)LINEARITYLOOPS-(NOINT*2)
              ELSE
                WRITE(IOUT,106)LINEARITYLOOPS
                WRITE(*,106)LINEARITYLOOPS
              ENDIF
            ELSE
              WRITE(IOUT,103)LINEARITYLOOPS
              WRITE(*,103)LINEARITYLOOPS
            ENDIF
          ELSE
            WRITE(IOUT,101)
            ICTRL = 4
          ENDIF
        ENDIF
      ELSEIF (CTRLJOB(ICTRL)=='FORWARD&SENS') THEN
        IF(NPERTURB_CYCLES>0)THEN
          ICTRL = ICTRL + 1
        ELSE
          ICTRL = ICTRL + 2
          CHECKNEW = .TRUE.
        ENDIF
      ELSEIF (CTRLJOB(ICTRL)=='SENSITIVITY') THEN
        IF (.NOT. SENSITIVITIES) THEN
          ICTRL = ICTRL + 1
          CHECKNEW = .TRUE.
        ENDIF
      ELSEIF (CTRLJOB(ICTRL)=='GAUSS-NEWTON') THEN
        IF (.NOT. OPT) ICTRL = ICTRL + 1
      ENDIF
    ENDDO
    !
    JOB = CTRLJOB(ICTRL)
    IF(.NOT. SOSSURFACE) WRITE(*,100) TRIM(JOB)
    IF (JOB=='FORWARD') THEN
      NUMPPL = 1
    ELSEIF (JOB=='FORWARD&SENS') THEN
      NUMPPL = 1
      DO IP=1,NPE
        IF(PVAL(IPTR(IP)) .EQ. 0.0) THEN
          IF(PVALINIT(IPTR(IP)) .NE. 0.0) THEN
            MAG_PERTURB(IP)=PVALINIT(IPTR(IP))*PERTURB(IPTR(IP))
          ELSE
            MAG_PERTURB(IP)=PERTURB(IPTR(IP))
          ENDIF
        ELSE
          MAG_PERTURB(IP)=PVAL(IPTR(IP))*PERTURB(IPTR(IP))
        ENDIF
      ENDDO
    ELSEIF (JOB=='SENSITIVITY') THEN
      NUMPPL = 1
      DO IP=1,NPE
        IF(PVAL(IPTR(IP)) .EQ. 0.0) THEN
          IF(PVALINIT(IPTR(IP)) .NE. 0.0) THEN
            MAG_PERTURB(IP)=PVALINIT(IPTR(IP))*PERTURB(IPTR(IP))
          ELSE
            MAG_PERTURB(IP)=PERTURB(IPTR(IP))
          ENDIF
        ELSE
          MAG_PERTURB(IP)=PVAL(IPTR(IP))*PERTURB(IPTR(IP))
        ENDIF
      ENDDO
      ! PRINTING TASK
      IF(PREDICT) THEN
        WRITE(IOUT,402)
        WRITE(*,402)
      ELSEIF(ITERP == 0) THEN
        IF(OPTNLUNC) THEN
          IF(IINTCNT == 0)THEN
            WRITE(IOUT,405)
            WRITE(*,405)
          ELSE
            WRITE(IOUT,406)
            WRITE(*,406)
          ENDIF
        ELSE
          WRITE(IOUT,405)
          WRITE(*,405)
        ENDIF
      ELSEIF (FINALSTATS .AND. .NOT. OPTNLUNC .AND. ITERP > 1) THEN
        IF(.NOT. LOWEST_MOD) THEN
          WRITE(IOUT,404)
        ELSE
          WRITE(IOUT,403)ILOW-1
        ENDIF
          SENTYPE = " NO"
      ELSE
        WRITE(IOUT,403)ITERP
        WRITE(*,403)ITERP
      ENDIF
      ! END PRINTING TASK
    ELSEIF (JOB=='GAUSS-NEWTON') THEN
      NUMPPL = 1
      IF (FINALSTATS) ICTRL = ICTRL - 2
      IF (FINALSTATSDONE) WRITE(*,104)
    ELSEIF (JOB=='STOP') THEN
      CTRLDONE = .TRUE.
    ENDIF
    RETURN
    !
  END SUBROUTINE UCODE_DEF
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_GEN_LINEARITY (IFAIL,IOUT,LINEARITYLOOPS, &
                                  OUTNAM,LINEARITYDONE,PSET)
  !*****************************************************************************
  ! GENERATE SIMULATED EQUIVALENTS FOR LINEARITY PARAMETERS _b1 to _b2
  !*****************************************************************************
  !        SPECIFICATIONS:
  !------------------------------------------------------------------
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                           INTENT(INOUT) :: IFAIL
    INTEGER,                           INTENT(IN)    :: IOUT
    INTEGER,                           INTENT(IN)    :: LINEARITYLOOPS
    CHARACTER(LEN=MAX_STRING_LEN),     INTENT(IN)    :: OUTNAM
    LOGICAL,                           INTENT(INOUT) :: LINEARITYDONE
    DOUBLE PRECISION,                  INTENT(OUT)   :: PSET(NPE)
    ! Local Variable
    DOUBLE PRECISION                              ::  CEV
    CHARACTER(LEN=200)                            ::  CHECK
    CHARACTER (LEN=3)                             ::  CONVERGE
    CHARACTER(LEN=MAX_STRING_LEN)                 ::  FN
    INTEGER                                       ::  I
    INTEGER                                       ::  IUDMP
    INTEGER                                       ::  IP
    INTEGER                                       ::  J
    LOGICAL                                       ::  LEX = .FALSE.
    INTEGER                                       ::  MXRECL
    INTEGER                                       ::  NBOT
    INTEGER                                       ::  NITER
    INTEGER                                       ::  NPETMP
    INTEGER                                       ::  NPERDTMP
    INTEGER                                       ::  NPREDGPS
    INTEGER                                       ::  NTOP
    INTEGER                                       ::  NVARP
    CHARACTER (LEN=12), ALLOCATABLE, DIMENSION(:) ::  TEMPPARNAM
    INTEGER, ALLOCATABLE, DIMENSION(:)            ::  TEMPLN
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   ::  TEMPPVAL
    DOUBLE PRECISION                              ::  TMPVAL
    CHARACTER (LEN=25)                            ::  WORD
    !
    101 FORMAT(//,80('-'))
    202 FORMAT (//, &
        1X,' !!!! ERROR COULD NOT OPEN _b1 FILE !!!',//, &
        5X,' The _b1 file should be generated by',/, &
        5X,'running UCODE with OPTIMIZE=yes in this same directory',//)
    500 FORMAT (//,1X,78('*'),/,11X, &
             'NOTE: SOME OF THE STATISTICS IN THE DATA-EXCHANGE FILES',/,11X, &
             'WERE GENERATED USING FORWARD DIFFERENCE SENSITIVITIES.',/,11X, &
             'THEY MAY BE INACCURATE AND CAUSE INACCURARCIES IN THE',/,11X, &
             'CALCULATION OF MODEL LINEARITY.',/,11X, &
             'CONSIDER REGENERATING THE FILES WITH CENTRAL DIFFERENCES',/,11X, &
             'OR EXACT DERIVATIVES.',/,1X,78('*'),//)
    682 FORMAT(5(1X,A12))
    693 FORMAT(5(1X,G12.4))
    !
    ALLOCATE(TEMPPARNAM(NPS),TEMPLN(NPS),TEMPPVAL(NPS))
    !
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_GEN_LINEARITY'
    IFAIL = 0
    IF (LINEARITYLOOPS == 1) THEN
      PINCSEN = 1
      PINCBND = 1
      ! Read # of parameters estimated in last iteration and # of iterations
      CALL UTLUCODE_DX_READ_DM(IFAIL,OUTNAM, &
                               RDUM,RDUM,RDUM,RDUM, &
                               CONVERGE,NITER,RDUM,RDUM,RDUM, &
                               CDUM,CDUM,CDUM,CDUM, &
                               IDUM,IDUM,NPETMP,NPERDTMP,IDUM,IDUM, &
                               RDUM,RDUM,RDUM,CEV,IOUT,RDUM,RDUM,SENTYPE)
!      FN = TRIM(OUTNAMPRED)//'._dmp'
!      INQUIRE(FILE=FN,EXIST=LEX)
!      IF(LEX) THEN
!        IUDMP = UTL_DX_OPEN(OUTNAMPRED,'_dmp','OLD')
!        LEX = .FALSE.
!        READ(IUDMP,*,END=801)CHECK,NPREDGPS
!        READ(IUDMP,*,END=801)CHECK,NVARP
!        801 CLOSE(UNIT=IUDMP)
!        NPERD = NVARP
!      ENDIF
      IF(SENTYPE .EQ. "YES")WRITE(IOUT,500)
      IFAIL = 0
      IF(CEV .EQ. 1.E+30) CALL UTL_STOP &
          ('Failed based on _dm: Linearity=yes requires a converged regression')
      ! READ STATUS FLAGS FOR PARAMETERS IN THE LAST ITERATION
      CALL UTLUCODE_DX_READ_PAOPT(NPS,OUTNAM,TEMPLN,TEMPPARNAM,PINCR, &
                                  TEMPPVAL,NPSWOP)
      DO I=1,NPS
        IF(PINCR(I) == 0) THEN
          PARAM_OMIT = .TRUE.
          IF(.NOT. ALLOCATED (POMIT))ALLOCATE(POMIT(NPETMP-NPERDTMP))
          NPO = NPO +1
          POMIT(NPO) = I
        ENDIF
      ENDDO
      IF(PARAM_OMIT) RETURN
    ! Open the input data file: NOTE THIS _b1 is being READ, NOT WRITTEN
      MXRECL = (1+NPPREC)*26
      IUB1 = UTL_DX_OPEN(OUTNAM,'_b1','OLD',MXRECL)
      IF(IUB1 .EQ. -2) THEN
        WRITE(*,202)
        CALL UTL_STOP (' ')
      ENDIF
      ALLOCATE(PVALS(NPS,2*NPERD),MVALS(NUSEOBS,2*NPERD))
      PVALS = 0.D0
      MVALS = 0.D0
      ! Read LINEARITY Parameter values from _b1
      CALL UTLUCODE_DX_READ_B(2*NPERD,NPS,IUB1,PARNAM,PVALS)
      IUB1 = UTL_DX_CLOSE('_b1')
    ENDIF
    IF (LINEARITYLOOPS .LE. (2*NPERD)) THEN
      DO J=1,NPS
        PVAL(J) = PVALS(J,LINEARITYLOOPS)
      ENDDO
      DO I=1,NPS,5
        NBOT=I
        NTOP=I+4
        IF(NTOP.GT.NPS)NTOP=NPS
        WRITE (IOUT,682)(PARNAM(IP),IP=NBOT,NTOP)
        WRITE (IOUT,693)(PVAL(IP),IP=NBOT,NTOP)
      ENDDO
      !   Write LINEARITY Parameter values to TMPVAL, ensuring that parameter
      !   values equal value that will eventually be written to model-input file
      DO I=1,NPS
        CALL UTL_WRTSIG(IFAIL,PVAL(I),WORD,NW(I),PRECIS,TMPVAL,NOPNT)
        IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from UCODEMOD ')
        PVAL(I) = TMPVAL
      ENDDO
      IF (IFAIL .NE. 0) CALL UTL_STOP(' ')
    ENDIF
    IF (LINEARITYLOOPS .EQ. (2*NPERD)) THEN
      LINEARITYDONE = .TRUE.
      WRITE(IOUT,101)
      IF(ALLOCATED(BPTR)) DEALLOCATE(BPTR)
    ENDIF
    J = 1
    DO I=1,NPS
      IF(PINCR(I) >= 0) THEN
        PSET(J) = PVAL(I)
        PINCSEN(J) = 0
        PINCBND(J) = 0
        J = J + 1
      ENDIF
    ENDDO
    DEALLOCATE(TEMPPARNAM,TEMPLN,TEMPPVAL)
    RETURN
  END SUBROUTINE UCODE_GEN_LINEARITY
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_GEN_LINEARITY_ADV (IFAIL,MPR,IOUT,LINEARITYLOOPS, &
                               OUTNAM,OUTNAMPRED,PRINAM,LINEARITYDONE,PSET)
  !*****************************************************************************
  ! GENERATE&WRITE SIMULATED EQUIVALENTS FOR ADV LINEARITY PARAMETERS _b3 to _b4
  !*****************************************************************************
  !        SPECIFICATIONS:
  !------------------------------------------------------------------
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                           INTENT(INOUT) :: IFAIL
    INTEGER,                           INTENT(IN)    :: MPR
    INTEGER,                           INTENT(IN)    :: IOUT
    INTEGER,                           INTENT(IN)    :: LINEARITYLOOPS
    CHARACTER(LEN=MAX_STRING_LEN),     INTENT(IN)    :: OUTNAM
    CHARACTER(LEN=MAX_STRING_LEN),     INTENT(IN)    :: OUTNAMPRED
    CHARACTER(LEN=LENDNAM),            INTENT(IN)    :: PRINAM(MPR)
    LOGICAL,                           INTENT(INOUT) :: LINEARITYDONE
    DOUBLE PRECISION,                  INTENT(OUT)   :: PSET(NPE)
    CHARACTER (LEN=12)                                ::  CDUMTITLE
    DOUBLE PRECISION                                  ::  CEV
    DOUBLE PRECISION                                  ::  CF1
    CHARACTER(LEN=200)                                ::  CHECK
    INTEGER                                           ::  CNT
    CHARACTER (LEN=3)                                 ::  CONVERGE
    CHARACTER(LEN=MAX_STRING_LEN)                     ::  FN
    CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) ::  GNAM
    INTEGER                                           ::  I
    INTEGER                                           ::  IP
    INTEGER                                           ::  IUCF
    INTEGER                                           ::  IUCFNAM
    INTEGER                                           ::  IUDMP
    INTEGER                                           ::  J
    INTEGER                                           ::  K
    LOGICAL                                           ::  LEX = .FALSE.
    INTEGER                                           ::  MXRECL
    INTEGER                                           ::  NBOT
    INTEGER                                           ::  NITER
    INTEGER                                           ::  NOINT2
    INTEGER                                           ::  NPETMP
    INTEGER                                           ::  NPREDGPS
    INTEGER                                           ::  NTOP
    INTEGER                                           ::  NVARP
    CHARACTER (LEN=12), ALLOCATABLE, DIMENSION(:)     ::  TEMPPARNAM
    INTEGER, ALLOCATABLE, DIMENSION(:)                ::  TEMPLN
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)       ::  TEMPPVAL
    DOUBLE PRECISION                                  ::  TMPVAL
    CHARACTER (LEN=25)                                ::  WORD
    !
    101 FORMAT(//,80('-'))
    201 FORMAT (//, &
        1X,' !!!! ERROR COULD NOT OPEN _b1adv FILE !!!',//, &
        5X,' The _b1adv file should be generated by',/, &
        5X,'running CORFAC_UC in this same directory',//)
    203 FORMAT (//, &
        1X,' !!!! ERROR COULD NOT OPEN _b3 FILE !!!',//, &
        5X,' The _b3 file should be generated by',/, &
        5X,'running CORFAC_UC in this same directory',//)
    275 FORMAT(//,1X,78('!'),/,1X,78('!'),//,5X,'ERROR:',//, &
        5X,' CORFAC_PLUS OR EQUIVALENT MUST BE EXECUTED TO OBTAIN fn._cfsu',/, &
        5X,' BEFORE RUNNING UCODE with linearityadv=yes',//, &
        1X,78('!'),/,1X,78('!'),//)
    500 FORMAT (//,1X,78('*'),/,11X, &
        'NOTE: SOME OF THE STATISTICS IN THE DATA-EXCHANGE FILES',/,11X, &
        'WERE GENERATED USING FORWARD DIFFERENCE SENSITIVITIES.',/,11X, &
        'THEY MAY BE INACCURATE AND CAUSE INACCURARCIES IN THE',/,11X, &
        'CALCULATION OF MODEL LINEARITY.',/,11X, &
        'CONSIDER REGENERATING THE FILES WITH CENTRAL DIFFERENCES',/,11X, &
        'OR EXACT DERIVATIVES.',/,1X,78('*'),//)
    682 FORMAT(5(1X,A12))
    693 FORMAT(5(1X,G12.4))
    !
    ALLOCATE(TEMPPARNAM(NPS),TEMPLN(NPS),TEMPPVAL(NPS))
    !
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_GEN_LINEARITY_ADV'
    IFAIL = 0
    !
    IF (LINEARITYLOOPS == 1) THEN
      PINCSEN = 1
      PINCBND = 1
      ! Read # of intervals
      NPREDI = 0
      NPARI = 0
      FN = TRIM(OUTNAMPRED)//'._cfsu'
      INQUIRE(FILE=FN,EXIST=LEX)
      IF(LEX) THEN
        MXRECL = (NPS*30)+40
        IUCF = UTL_DX_OPEN(OUTNAMPRED,'_cfsu','OLD',MXRECL)
        LEX = .FALSE.
        ! READ Header
        ! read _cfsu to count number of preds and parms
        READ(IUCF,*,END=100)CDUMTITLE
        DO
          ! check to see if wrap fprmat is repeating the items
          READ(IUCF,*,END=991)CDUM
          IF(TRIM(CDUM) == TRIM(CDUMTITLE)) GO TO 991
          BACKSPACE(IUCF)
          READ(IUCF,*,END=991)CDUM,IDUM
          IF(IDUM == 1) THEN
            NPREDI = NPREDI + 1
          ELSEIF(IDUM ==2) THEN
            NPARI = NPARI + 1
          ENDIF
        ENDDO
        991 IUCF = UTL_DX_CLOSE('_cfsu')
        ! check for omitted params
        FN = TRIM(OUTNAMPRED)//'._cfconf'
        INQUIRE(FILE=FN,EXIST=LEX)
        IF(LEX) THEN
          IUCF = UTL_DX_OPEN(OUTNAMPRED,'_cfconf','OLD')
          LEX = .FALSE.
        ELSE
          FN = TRIM(OUTNAMPRED)//'._cfpred'
          INQUIRE(FILE=FN,EXIST=LEX)
          IF(LEX) THEN
            IUCF = UTL_DX_OPEN(OUTNAMPRED,'_cfpred','OLD')
            LEX = .FALSE.
          ELSE
            WRITE(*,275)
            WRITE(IOUT,275)
            IFAIL = -999
            RETURN
          ENDIF
        ENDIF
        READ(IUCF,*,END=100)CDUMTITLE
        IF(NPREDI > 0) THEN
          DO I=1,NPREDI
            ! check to see if wrap fprmat is repeating the items
            READ(IUCF,*,END=992)CDUM
          ENDDO
        ENDIF
        992 IF(NPARI > 0) THEN
          OMITTEDNP = 0
          DO I=1,NPARI
            ! check to see if wrap fprmat is repeating the items
            READ(IUCF,*,END=993)CDUM,IDUM,RDUM,RDUM,CF1,RDUM
            IF(CF1 == 0.D0) THEN
              OMITTEDNP = OMITTEDNP +1
            ENDIF
          ENDDO
        ENDIF
        993 NPARI = NPARI - OMITTEDNP
        IUCF = UTL_DX_CLOSE('_cfsu')
        NOINT = NPREDI + NPARI
        ALLOCATE(OBSNAMB(NUSEOBS+1))
        OBSNAMB = ' '
        DO I=1,NUSEOBS
          CALL UTL_CASE(OBSNAM(I),OBSNAMB(I),1)
        ENDDO
        OBSNAMB(NUSEOBS+1) = 'PREDICTION'
      ELSE
        WRITE(*,275)
        WRITE(IOUT,275)
        IFAIL = -999
        RETURN
      ENDIF
      ALLOCATE(GNAM(NOINT),GNAMPTR(NOINT),GNAMPTRTYP(NOINT))
      ! READ DATA FROM CORFAC OUTPUT
      IUCFNAM = 0
      FN = TRIM(OUTNAMPRED)//'._cfconf'
      INQUIRE(FILE=FN,EXIST=LEX)
      IF(LEX) THEN
        IUCF = UTL_DX_OPEN(OUTNAMPRED,'_cfconf','OLD')
        IUCFNAM = 1
        LEX = .FALSE.
      ELSE
        FN = TRIM(OUTNAMPRED)//'._cfpred'
        INQUIRE(FILE=FN,EXIST=LEX)
        IF(LEX) THEN
          IUCF = UTL_DX_OPEN(OUTNAMPRED,'_cfpred','OLD')
          IUCFNAM = 2
          LEX = .FALSE.
        ELSE
          AMESSAGE = &
          'CORFAC_PLUS OR EQUIVALENT MUST BE EXECUTED TO OBTAIN either'
          WRITE(*,*)TRIM(AMESSAGE)
          CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
          AMESSAGE = &
          'fn._cfconf OR fn._cfpred BEFORE RUNNING UCODE with linearityadv=yes'
          WRITE(*,*)TRIM(AMESSAGE)
          CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','yes')
          CALL UTL_STOP(' ')
        ENDIF
      ENDIF
      ! READ Header
      READ(IUCF,*,END=100)
      DO I=1,NOINT
        READ(IUCF,*,END=100)GNAM(I)
      ENDDO
      CNT = 1
      DO I=1,NOINT-NPARI
        DO J=1,NOBS
          IF(UTL_SAMENAME(GNAM(I),OBSNAM(J))) THEN
            GNAMPTR(CNT) = J
            GNAMPTRTYP(CNT) = 1
            CNT = CNT + 1
            EXIT
          ENDIF
        ENDDO
        IF(CNT == NOINT-NPARI+1) EXIT
        IF(J == NOBS)THEN
          IF(MPR > 0)THEN
            DO K=1,MPR
              IF(UTL_SAMENAME(GNAM(I),PRINAM(K))) THEN
                GNAMPTR(CNT) = K
                GNAMPTRTYP(CNT) = 2
                CNT = CNT + 1
                EXIT
              ENDIF
            ENDDO
            IF(K .NE. MPR) CYCLE
            CALL UTL_STOP &
            ('PREDNAME in fn.cofac DOES NOT MATCH A PREDICTION NAME')
          ELSE
            CALL UTL_STOP &
            ('PREDNAME in fn.cofac DOES NOT MATCH A PREDICTION NAME')
          ENDIF
        ENDIF
        IF(CNT == NOINT-NPARI+1) EXIT
      ENDDO
      GO TO 199
      100 IF(IUCFNAM == 1) THEN
        IUCF = UTL_DX_CLOSE('_cfconf')
      ELSEIF(IUCFNAM == 2) THEN
        IUCF = UTL_DX_CLOSE('_cfpred')
      ENDIF
      CALL UTL_STOP &
           ('UNEXPECTED CONTENT _cfsu file, CHECK VERSION/RE-CREATE _cfsu')
      199 IUCF = UTL_DX_CLOSE('_cfsu')
      ! Read # of parameters estimated in last iteration and # of iterations
      CALL UTLUCODE_DX_READ_DM(IFAIL,OUTNAM, &
                               RDUM,RDUM,RDUM,RDUM, &
                               CONVERGE,NITER,RDUM,RDUM,RDUM, &
                               CDUM,CDUM,CDUM,CDUM, &
                               IDUM,IDUM,NPETMP,NPERD,IDUM,IDUM, &
                               RDUM,RDUM,RDUM,CEV,IOUT,RDUM,RDUM,SENTYPE)
      FN = TRIM(OUTNAMPRED)//'._dmp'
      INQUIRE(FILE=FN,EXIST=LEX)
      IF(LEX) THEN
        IUDMP = UTL_DX_OPEN(OUTNAMPRED,'_dmp','OLD')
        LEX = .FALSE.
        READ(IUDMP,*,END=801)CHECK,NPREDGPS
        READ(IUDMP,*,END=801)CHECK,NVARP
        801 CLOSE(UNIT=IUDMP)
        NPERD = NVARP
      ENDIF
      IF(SENTYPE .EQ. "YES")WRITE(IOUT,500)
      IFAIL = 0
      IF(LINEARITYLOOPS == 1) THEN
        ALLOCATE(MVALS(NUSEOBS+1,2*NOINT),PVALS(NPS,2*NOINT+2*NPERD), &
                 PVALS1(NPS,2*NPERD),PVALS3(NPS,2*NOINT))
        MVALS = 0.D0
      ENDIF
      IF(CEV .EQ. 1.E+30) CALL UTL_STOP &
       ('Failed based on _dm: LinearityAdv=yes requires a converged regression')
      ! READ STATTUS FLAGS FOR PARAMETERS IN THE LAST ITERATION
      CALL UTLUCODE_DX_READ_PAOPT(NPS,OUTNAM,TEMPLN,TEMPPARNAM,PINCR, &
                                  TEMPPVAL,NPSWOP)
      DO I=1,NPS
        IF(PINCR(I) == 0) THEN
          PARAM_OMIT = .TRUE.
          IF(.NOT. ALLOCATED (POMIT))ALLOCATE(POMIT(NPETMP-NPERD))
          NPO = NPO +1
          POMIT(NPO) = I
        ENDIF
      ENDDO
      ! Open the input data file: NOTE THIS _b3 is being READ, NOT WRITTEN
      MXRECL = (1+NPPREC)*26
      IUB1 = UTL_DX_OPEN(OUTNAMPRED,'_b1adv'//EXTB,'OLD',MXRECL)
      IF(IUB1 .EQ. -2) THEN
        WRITE(*,201)
        CALL UTL_STOP (' ')
      ENDIF
      CALL UTLUCODE_DX_READ_B(2*NPERD,NPS,IUB1,PARNAM,PVALS1)
      IUB1 = UTL_DX_CLOSE('_b1adv'//EXTB)
      ! Open the input data file: NOTE THIS _b3 is being READ, NOT WRITTEN
      IUB3 = UTL_DX_OPEN(OUTNAMPRED,'_b3'//EXTB,'OLD',MXRECL)
      IF(IUB3 .EQ. -2) THEN
        WRITE(*,203)
        CALL UTL_STOP (' ')
      ENDIF
      CALL UTLUCODE_DX_READ_B(2*NOINT,NPS,IUB3,PARNAM,PVALS3)
      IUB3 = UTL_DX_CLOSE('_b3'//EXTB)
      DO I=1,2*NOINT
        DO J=1,NPS
          PVALS(J,I) = PVALS3(J,I)
        ENDDO
      ENDDO
      DO I=1,(2*NPERD)
        NOINT2 = 2 * NOINT
        DO J=1,NPS
          PVALS(J,I+NOINT2) = PVALS1(J,I)
        ENDDO
      ENDDO
      IF(NPARI > 0) THEN
        DO I=NOINT-NPARI+1,NOINT
          DO J=1,NPS
            IF(UTL_SAMENAME(GNAM(I),PARNAM(J))) THEN
              GNAMPTR(CNT) = J
              GNAMPTRTYP(CNT) = 3
              CNT = CNT + 1
              EXIT
            ENDIF
            IF(J == NPS) CALL UTL_STOP &
            ('PARAMETER name in fn.cofac DOES NOT MATCH A REGRESSION PARAMETER')
          ENDDO
          IF(CNT > NOINT) EXIT
        ENDDO
      ENDIF
      ! Open the Output Underscore file _b2adv
      MXRECL = (2+NOBS)*26
      IUB2 = UTL_DX_OPEN(OUTNAMPRED,'_b2adv'//EXTB,'REPLACE',MXRECL)
      ! Open the Output Underscore file _b4
      IUB4 = UTL_DX_OPEN(OUTNAMPRED,'_b4'//EXTB,'REPLACE',MXRECL)
    ENDIF
    IF (LINEARITYLOOPS .LE. (2*NOINT + 2*NPERD)) THEN
      ! Get LINEARITY Parameter values from _b3
      DO J=1,NPS
        PVAL(J) = PVALS(J,LINEARITYLOOPS)
      ENDDO
      DO I=1,NPS,5
        NBOT=I
        NTOP=I+4
        IF(NTOP.GT.NPS)NTOP=NPS
        WRITE (IOUT,682)(PARNAM(IP),IP=NBOT,NTOP)
        WRITE (IOUT,693)(PVAL(IP),IP=NBOT,NTOP)
      ENDDO
      !  Write LINEARITY Parameter values to TMPVAL, ensuring that parameter
      !  values equal value that will eventually be written to model-input files
      DO I=1,NPS
        CALL UTL_WRTSIG(IFAIL,PVAL(I),WORD,NW(I),PRECIS,TMPVAL,NOPNT)
        IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from UCODEMOD ')
        PVAL(I) = TMPVAL
      ENDDO
      IF (IFAIL .NE. 0) CALL UTL_STOP(' ')
    ENDIF
    IF (LINEARITYLOOPS .EQ. (2*NOINT + 2*NPERD)) THEN
      LINEARITYDONE = .TRUE.
      WRITE(IOUT,101)
      IUB1 = UTL_DX_CLOSE('_b1adv'//EXTB)
      IF(ALLOCATED(BPTR)) DEALLOCATE(BPTR)
    ENDIF
    J = 1
    DO I=1,NPS
      IF(PINCR(I) >= 0) THEN
        PSET(J) = PVAL(I)
        PINCSEN(J) = 0
        PINCBND(J) = 0
        J = J + 1
      ENDIF
    ENDDO
    DEALLOCATE(TEMPPARNAM,TEMPLN,TEMPPVAL)
    RETURN
  END SUBROUTINE UCODE_GEN_LINEARITY_ADV
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_GEN_PREDICT (IFAIL,IOUT,OUTNAM,OUTNAMPRED,PSET)
  !*****************************************************************************
  !     Substitute optimal values for prediction
  !*****************************************************************************
  !        SPECIFICATIONS:
  !------------------------------------------------------------------
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                       INTENT(INOUT) :: IFAIL
    INTEGER,                       INTENT(IN)    :: IOUT
    CHARACTER(LEN=MAX_STRING_LEN), INTENT(IN)    :: OUTNAM
    CHARACTER(LEN=MAX_STRING_LEN), INTENT(IN)    :: OUTNAMPRED
    DOUBLE PRECISION,              INTENT(OUT)   :: PSET(NPE)
    ! local variables
    CHARACTER (LEN=3)                    ::  CONVERGE
    INTEGER                              ::  IUPR
    INTEGER                              ::  NITER
    INTEGER                              ::  NPETMP
    INTEGER                              ::  NPERDTMP
    INTEGER I, J, K, KK
    ! Formats
    90 FORMAT(' Substituting Optimal parameter values for predictions: ',/, &
              '  - indicates parameter was not adjustable',/, &
              '  ! indicates parameter was omitted due to insensitivity ', &
              'or bounding contraints')
    100 FORMAT(6(A1,A12))
    101 FORMAT(6(G13.4))
    102 FORMAT(1X,A)
    103 FORMAT(1X,80('*'))
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_GEN_PREDICT'
    IFAIL = 0
    CALL UTLUCODE_DX_READ_DM(IFAIL,OUTNAM, &
                             RDUM,RDUM,RDUM,RDUM, &
                             CONVERGE,NITER,RDUM,RDUM,RDUM, &
                             CDUM,CDUM,CDUM,CDUM, &
                             IDUM,IDUM,NPETMP,NPERDTMP,IDUM,IDUM, &
                             RDUM,RDUM,RDUM,RDUM,IOUT,RDUM,RDUM,SENTYPE)
    IFAIL = 0
    ! READ STATTUS FLAGS FOR PARAMETERS IN THE LAST ITERATION
    ALLOCATE(PSTATUS(NPS))
    PSTATUS = '-'
    CALL UTLUCODE_DX_READ_PAOPT(NPS,OUTNAM,LN,PARNAM,PINCR,PVAL,NPSWOP)
    IF(NPS > NPSWOP) THEN
      IUPR = UTL_DX_OPEN(OUTNAMPRED,'_paoptp','OLD')
      READ(IUPR,*)
      READ(IUPR,*)&
          (PARNAM(NPSWOP+I),PVAL(NPSWOP+I),LN(NPSWOP+I),IDUM,I=1,NPS-NPSWOP)
    ENDIF
    DO I=1,NPSWOP
      IF(PINCR(I) > 0) THEN
        PSTATUS(I) = ' '
      ELSEIF(PINCR(I) .EQ. 0) THEN
        PSTATUS(I) = '!'
        PARAM_OMIT = .TRUE.
        IF(.NOT. ALLOCATED (POMIT))ALLOCATE(POMIT(NPETMP-NPERDTMP))
        NPO = NPO +1
        POMIT(NPO) = I
      ENDIF
    ENDDO
    IF(.NOT. SENSITIVITIES) PARAM_OMIT = .FALSE.
    DO I=NPSWOP+1,NPS
      IF(PINCR(I) > 0) PSTATUS(I) = ' '
    ENDDO
    WRITE(IOUT,*)
    WRITE(IOUT,90)
    WRITE(IOUT,*)
    DO K=1,NPS,5
      IF(NPS < K+5) THEN
        KK = NPS
      ELSE
        KK = K+5
      ENDIF
      WRITE(IOUT,100)(PSTATUS(I),PARNAM(I),I = K,KK)
      WRITE(IOUT,101)(PVAL(I), I = K,KK)
    ENDDO
    WRITE(IOUT,*)
    PNPERD = 0
    DO I=1,NPS
      IF(PINCR(I) > 0) PNPERD = PNPERD + 1
    ENDDO
    J = 1
    DO I=1,NPS
      IF(PINCR(I) > 0) THEN
        PSET(J) = PVAL(I)
        J = J +1
      ENDIF
    ENDDO
    IF(.NOT. SENSITIVITIES) THEN
      WRITE(IOUT,103)
      WRITE(IOUT,*)
      WRITE(IOUT,102) &
      'THIS EXECUTION OF UCODE was made for PREDICT=yes SENSITIVITIES=no'
      WRITE(IOUT,*)
      WRITE(IOUT,102) &
      'CONFIRM THAT THE PROCESS MODELS FOR THE PREDICTIVE PHASE RAN CORRECTLY'
      WRITE(IOUT,102) &
      '  and THAT THE CORRECT MODEL VALUES ARE EXTRACTED'
      WRITE(IOUT,102) &
      '  once confirmed, proceed in the predictive mode with sensitivities=yes'
    ENDIF
    RETURN
  END SUBROUTINE UCODE_GEN_PREDICT
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_GEN_SOSSURF (IFAIL,IOUT,OUTNAM,PSET)
  !*****************************************************************************
  !     CALCULATE SUM OF SQUARES SURFACE
  !*****************************************************************************
  !        SPECIFICATIONS:
  !------------------------------------------------------------------
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                           INTENT(INOUT) :: IFAIL
    INTEGER,                           INTENT(IN)    :: IOUT
    CHARACTER(LEN=MAX_STRING_LEN),     INTENT(IN)    :: OUTNAM
    DOUBLE PRECISION,                  INTENT(OUT)   :: PSET(NPE)
    ! Local Variable
    INTEGER                                       :: END = 0
    CHARACTER(LEN=MAX_STRING_LEN)                 :: FN
    INTEGER                                       :: I
    DOUBLE PRECISION                              :: INCREMENT
    INTEGER                                       :: J
    INTEGER                                       :: K
    INTEGER                                       :: MXRECL
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: PVALSOS
    CHARACTER(LEN=12)                             :: TMP
    !
    ALLOCATE(PVALSOS(NPE))
    !
    105 FORMAT(/,1X,' DONE SOS-SURFACE LOOP: ',I8,' OF ',I8,/,2(/,1X,79('=')))
    120 FORMAT(' "Sum-of-Squared-Weighted-Residuals" ',1000(' "',A,'"',:))
    130 FORMAT(2002(G18.9,:))
    131 FORMAT &
    (/,' Sum-of-Squared-Weighted-Residuals Surface Noted above was computed', &
     /,' for the following Parameters: ',/,6(3X,A12,:))
    132 FORMAT(6(1X,G15.6,:))
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_GEN_SOSSURF'
    IFAIL = 0
    IF (SOSSURFLOOPS < 1) THEN
      ! Open the Output Underscore file _sos
      MXRECL = (NPS*20)+40
      IUSOS = UTL_DX_OPEN(OUTNAM,'_sos','REPLACE',MXRECL)
      WRITE(IUSOS,120)(PARNAM(IPTR(I)),I=1,NPE)
      J = 1
      IF(SOSFILEL)THEN
        TOTSOSLOOPS = 0
        ALLOCATE(SOSPTRNPE(NPE),SOSPARNAM(NPE))
        SOSPTRNPE = 0
        SOSPARNAM = ' '
        IUSOSFILE = UTL_GETUNIT(101,150)
        FN = SOSFILE
        OPEN(UNIT=IUSOSFILE,FILE=FN,STATUS='OLD')
        READ(IUSOSFILE,*)NPSOS
        READ(IUSOSFILE,*)(SOSPARNAM(I),I=1,NPSOS)
        DO I=1,NPSOS
          CALL UTL_CASE(SOSPARNAM(I),TMP,-1) ! Store names as lowercase
          SOSPARNAM(I) = TMP
        ENDDO
        END=0
        DO WHILE(END .EQ. 0)
          READ(IUSOSFILE,*,END=50)(PVALSOS(J),J=1,NPSOS)
          TOTSOSLOOPS = TOTSOSLOOPS + 1
        ENDDO
        50 CLOSE(UNIT=IUSOSFILE)
        DO I = 1,NPSOS
          K = 0
          DO J = 1,NPS
            IF(PADJ(J)) THEN
              K = K + 1
              PVALSET(K) = PVALINIT(J)
            ENDIF
            IF(TRIM(SOSPARNAM(I)) .EQ. TRIM(PARNAMLC(J))) THEN
              IF(SOSPTRNPE(I) < 1) THEN
                SOSPTRNPE(I) = K
                EXIT
              ELSE
                WRITE(IOUT,*)TRIM(SOSPARNAM(I))
                CALL UTL_STOP(' PARAMETER LISTED MORE THAN ONCE IN SOSFILE ')
              ENDIF
            ELSEIF (J .EQ. NPS) THEN
              WRITE(IOUT,*)
              WRITE(*,*)
              WRITE(IOUT,*)' SOSFILE indicates ',NPSOS,' parameters, and'
              WRITE(*,*)' SOSFILE indicates ',NPSOS,' parameters and'
              WRITE(IOUT,*)
              WRITE(*,*)
              WRITE(IOUT,*)TRIM(SOSPARNAM(I))
              WRITE(*,*)TRIM(SOSPARNAM(I))
              CALL UTL_STOP(' IS NOT A PARAMETER NAME ')
            ENDIF
          ENDDO
        ENDDO
        OPEN(UNIT=IUSOSFILE,FILE=FN,STATUS='OLD')
        READ(IUSOSFILE,*)
        READ(IUSOSFILE,*)
        READ(IUSOSFILE,*)(PVALSOS(J),J=1,NPSOS)
        PVAL = PVALINIT
        PSET = PVALSET
        DO I = 1,NPSOS
          PSET(SOSPTRNPE(I)) = PVALSOS(I)
        ENDDO
      ELSE ! INCREMENT RATHER THAN FILE
        TOTSOSLOOPS = 1
        ALLOCATE(COUNTER(NPE),PRODUCTS(NPE))
        COUNTER = 1
        PRODUCTS = 0
        PVAL = PVALINIT
        DO I = 1,NPS
          IF(PADJ(I)) THEN
            TOTSOSLOOPS = TOTSOSLOOPS * SOSINCREMENT(I)
            PRODUCTS(J) = SOSINCREMENT(I)
            PSET(J) = PVALMINC(I)
            J = J + 1
          ENDIF
        ENDDO
        IF(NPE > 1) THEN
          IF (NPE .EQ. 2) THEN
            PRODUCTS(1) = PRODUCTS(2)
            PRODUCTS(2) = 1
          ELSE
            DO I = 1,NPE-1
            PRODUCTS(I) = 1
              DO J = I+1,NPE
                PRODUCTS(I) = PRODUCTS(I) * PRODUCTS(J)
              ENDDO
            ENDDO
            PRODUCTS(NPE) = 1
          ENDIF
        ELSE
          PRODUCTS(1) = 1
        ENDIF
      ENDIF
    ENDIF
    SOSSURFLOOPS = SOSSURFLOOPS + 1
    IF (SOSSURFLOOPS > 1) THEN
      WRITE(IUSOS,130)RSQD(1),(PVALTMP(I),I=1,NPE)
!      WRITE(IOUT,131)(PARNAM(IPTR(I)),I=1,NPE)
!      WRITE(IOUT,132)(PVALTMP(I),I=1,NPE)
      WRITE(*,105)SOSSURFLOOPS-1,TOTSOSLOOPS
!      WRITE(IOUT,105)SOSSURFLOOPS-1,TOTSOSLOOPS
      IF(SOSSURFDONE) THEN
        PSET = PVALTMP
        DEALLOCATE(PVALSOS)
        RETURN
      ENDIF
      IF (SOSFILEL) THEN
        IF (SOSSURFLOOPS <= TOTSOSLOOPS) THEN
          READ(IUSOSFILE,*)(PVALSOS(J),J=1,NPSOS)
          DO I = 1,NPSOS
            PSET(SOSPTRNPE(I)) = PVALSOS(I)
          ENDDO
          IF(SOSSURFLOOPS >= TOTSOSLOOPS) THEN
            SOSSURFDONE = .TRUE.
          ENDIF
        ENDIF
      ELSE !NOT SOSFILE
        PSET = PVALTMP
        IF (SOSSURFLOOPS <= TOTSOSLOOPS) THEN
          J = 1
          DO I=1,NPS
            IF(PADJ(I)) THEN
              IF(LN(I) > 0) THEN
                PVALMINC(I) = LOG10(PVALMINC(I))
                PVALMAXC(I) = LOG10(PVALMAXC(I))
              ENDIF
              IF(MOD((SOSSURFLOOPS-1),PRODUCTS(J)) .EQ. 0) THEN
                INCREMENT = (PVALMAXC(I)-PVALMINC(I))/DBLE((SOSINCREMENT(I)-1))
                PSET(J) = PVALMINC(I) + (DBLE(COUNTER(J)) * INCREMENT)
                COUNTER(J) = COUNTER(J) + 1
                IF(PSET(J) > (PVALMAXC(I)+(INCREMENT*1.D-9))) THEN
                  PSET(J)=PVALMINC(I)
                  COUNTER(J) = 1
                ENDIF
                 IF(LN(I) > 0) PSET(J) = 10**(PSET(J))
              ENDIF
              IF(LN(I) > 0) THEN
                PVALMINC(I) = 10**(PVALMINC(I))
                PVALMAXC(I) = 10**(PVALMAXC(I))
              ENDIF
              J = J +1
            ENDIF
          ENDDO
          IF(SOSSURFLOOPS >= TOTSOSLOOPS) THEN
            SOSSURFDONE = .TRUE.
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    PVALTMP = PSET 
    DEALLOCATE(PVALSOS)
    RETURN
  END SUBROUTINE UCODE_GEN_SOSSURF
!===============================================================================
!===============================================================================
SUBROUTINE UCODE_UEV_PRINT_PREDICT(IFAIL,IOUT,NEOBS,OBSEXTNAM,DEPEXTVALSETS)
    !
    ! -- Subroutine UCODE_UEV_PRINT_PREDICT prints the extracted values for
    !    evaluating whether the predictive mode is set up correctly
    !
    IMPLICIT NONE
    !
    !   ARGUMENT-LIST VARIABLES
    INTEGER,                              INTENT(OUT) :: IFAIL         ! ERROR INDICATOR
    INTEGER,                              INTENT(IN)  :: IOUT          ! OUTPUT UNIT NUMBER
    INTEGER,                              INTENT(IN)  :: NEOBS         ! NUMBER EXTRACTED VALUES
    CHARACTER (LEN=*), DIMENSION(NEOBS),  INTENT(IN)  :: OBSEXTNAM     ! EXTRACTED-VALUE NAMES
    DOUBLE PRECISION,  DIMENSION(NEOBS),  INTENT(IN)  :: DEPEXTVALSETS ! EXTRACTED VALUES
    !
    !   LOCAL VARIABLES
    INTEGER :: I
    !   FORMATS
    100 FORMAT(1X,A)
    101 FORMAT(1X,A,1PE15.7)
    102 FORMAT(1X,80('*'))
    !
    IFAIL = 0
    WRITE(IOUT,100)' '
    WRITE(IOUT,100)'EXTRACTED ITEM NAME   EXTRACTED VALUE '
    DO I=1,NEOBS
      WRITE(IOUT,101)OBSEXTNAM(I),DEPEXTVALSETS(I)
    ENDDO
    WRITE(IOUT,100)' '
    WRITE(IOUT,102)
    !
    RETURN
    !
  END SUBROUTINE UCODE_UEV_PRINT_PREDICT
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_UEV_RESID_PRINT_DECISION &
                (IFAIL,IFO,IOUT,ITERP,FINALSTATS,MAXITER,MPR,MODELPRIVAL, &
                 NONDETECTL,OUTNAM,OUTNAMPRED,PRINAM,PRIVAL,PRIWTMAT, &
                 PRIWTMATSQR,RESIDSPRI,TRUSTREGION,WTDRESIDSPRI,WTMATSQR,AVET)
    !   Determine what to print and print
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                         INTENT(INOUT) :: IFAIL
    INTEGER,                         INTENT(IN) :: IFO
    INTEGER,                         INTENT(IN) :: IOUT
    INTEGER,                         INTENT(IN) :: ITERP
    LOGICAL,                         INTENT(IN) :: FINALSTATS
    INTEGER,                         INTENT(IN) :: MAXITER
    INTEGER,                         INTENT(IN) :: MPR
    DOUBLE PRECISION,                INTENT(IN) :: MODELPRIVAL(MPR)
    LOGICAL,                         INTENT(IN) :: NONDETECTL
    CHARACTER(LEN=MAX_STRING_LEN),   INTENT(IN) :: OUTNAM
    CHARACTER(LEN=MAX_STRING_LEN),   INTENT(IN) :: OUTNAMPRED
    CHARACTER(LEN=LENDNAM),          INTENT(IN) :: PRINAM(MPR)
    DOUBLE PRECISION,                INTENT(IN) :: PRIVAL(MPR)
    TYPE (CDMATRIX),                 INTENT(IN) :: PRIWTMAT
    TYPE (CDMATRIX),                 INTENT(IN) :: PRIWTMATSQR
    DOUBLE PRECISION,                INTENT(IN) :: RESIDSPRI(MPR)
    LOGICAL,                         INTENT(IN) :: TRUSTREGION
    DOUBLE PRECISION,                INTENT(IN) :: WTDRESIDSPRI(MPR)
    TYPE (CDMATRIX),                 INTENT(IN) :: WTMATSQR
    DOUBLE PRECISION,                INTENT(INOUT) :: AVET
    ! local variables
    INTEGER                                        :: AD = 0
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: DNPP
! save this code for the time when a GUI will use this info to update screen
!    INTEGER                                   :: I1 = 48
!    INTEGER                                   :: I2 = 48
!    INTEGER                                   :: I3 = 48
!    INTEGER                                   :: I4 = 48
!    INTEGER                                   :: I5 = 48
!    INTEGER                                   :: IP
!    CHARACTER(LEN=210)                        :: FN
!    CHARACTER(LEN=1)                          :: A1
!    CHARACTER(LEN=1)                          :: A2
!    CHARACTER(LEN=1)                          :: A3
!    CHARACTER(LEN=1)                          :: A4
!    CHARACTER(LEN=1)                          :: A5
    INTEGER                                   :: ITER
    !
    391 FORMAT (/,' SUMS OF SQUARED, WEIGHTED RESIDUALS:',/, &
              '   DEPENDENT VARIABLES:           ',G15.7,/, &
              '   DEPENDENT VARIABLES AND PRIOR: ',G15.7,//, &
              ' NUMBER OF INCLUDED DEPENDENTS = ',I7,' OF ',I7,/, &
              ' NUMBER OF PRIOR =               ',I7,/, &
              /,80('*'))
    395 FORMAT (/,' SUM OF SQUARED, WEIGHTED RESIDUALS:',/, &
              '   DEPENDENT VARIABLES: ',G15.7,//, &
              ' NUMBER OF INCLUDED OBSERVATIONS = ',I7,' OF ',I7,/, &
              /,80('*'))
    400 FORMAT(80('*'))
    491 FORMAT (' SUMS OF SQUARED, WEIGHTED RESIDUALS:',/, &
              '   DEPENDENT VARIABLES:           ',G15.7, &
              '   DEPENDENT VARIABLES AND PRIOR: ',G15.7,/, &
              ' NUMBER OF INCLUDED DEPENDENTS = ',I7,' OF ',I7,/, &
              ' NUMBER OF PRIOR =               ',I7)
    495 FORMAT (' SUM OF SQUARED, WEIGHTED RESIDUALS:',/, &
              '   DEPENDENT VARIABLES: ',G15.7,/, &
              ' NUMBER OF INCLUDED OBSERVATIONS = ',I7,' OF ',I7)
    !
    ALLOCATE(DNPP(NUSEOBS+MPR,2))
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_UEV_RESID_PRINT_DECISION'
    IFAIL = 0
    !
    IPRINT = .FALSE.
    IF ( &
       ((ITERP .LE. 0) .AND. (STARTRES .EQ. 1 .OR. STARTRES .EQ. 3)) .OR. &
       (ITERP .GT. 0 .AND. (.NOT. FINALSTATS) &
        .AND. (INTERMEDRES .EQ. 1 .OR. INTERMEDRES .EQ. 3))) THEN
      IPRINT(1) = .TRUE.; IPRINT(3) = .TRUE.
    ENDIF
    IF (FINALSTATS .AND. (FINALRES .EQ. 1 .OR. FINALRES .EQ. 3)) THEN
      IPRINT(1) = .TRUE.; IPRINT(3) = .TRUE.
    ENDIF
    IF(RESIDONLY) THEN
      IPRINT(2) = .TRUE.
      IPRINT(3) = .TRUE.
      IF(DATAEXCHANGE)IPRINT(4) = .TRUE.
    ENDIF
    !
    ITER = ITERP + 1
    IF((.NOT. PREDICT) .AND. (.NOT. LINEARITY)) THEN
      IF (ITER > MAXITER+1) ITER = MAXITER+1
      IF(LOWEST_MOD) AD = AD+1
      CALL STA_UEV_INIT(AVET,NNEGT,NPOST,NRUNS,RSQD(ITER+AD),RSQALL(ITER+AD))
      CALL STA_UEV_FIT(IOUT,MPR,NUSEOBS,IPRINT(1:3),NONDETECTL, &
                  MODELVAL,MODELPRIVAL,OBSNAM,OBSVAL, &
                  OMIT,OUTNAM,PLOTSYMBOLPRI,PRILN,PRINAM,PRIVAL, &
                  PRIWTCORR,PRIWTMATSQR, &
                  RESIDS,RESIDSPRI,WTCORRELATED, &
                  WTDRESIDS,WTDRESIDSPRI,WTFULLSQR, &
                  AVET,NNEGT,NPOST,NRUNS,RSQD(ITER+AD),RSQALL(ITER+AD), &
                  DNPP,NOBSINC,WTRL)
      !-----COMPUTE INITIAL CALCULATED ERROR VARIANCE
      IF(ITER == 1 .AND. OPTIMIZE .OR. CREATEINITFILES) THEN
        IF (REAL(NOBS-IOMIT+MPR-NPE) <= 0.D0) THEN
          IFAIL = 1
          AMESSAGE = ' DEGREES OF FREEDOM = 0, CANNOT DIVIDE by 0 '
          CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
          AMESSAGE = ' TOO FEW OBSERVATIONS GIVEN THE NUMBER OF PARAMETERS'
          CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','yes')
          DEALLOCATE(DNPP)
          RETURN
        ELSE
          CEVINIT = RSQALL(ITER)/REAL(NOBS-IOMIT+MPR-NPE)
          IF(ITER == 1 .AND. OPTIMIZE) THEN
            IF(DATAEXCHANGE) THEN
              CALL UTLUCODE_DX_WRITE_INIT &
                   (NPS,CEVINIT,LN,OUTNAM,PARNAM,PINCR,PVAL)
            ENDIF
          ENDIF
          IF(SENSITIVITIES .AND. CREATEINITFILES) THEN
            CALL UTLUCODE_DX_WRITE_INIT(NPS,CEVINIT,LN,OUTNAM,PARNAM,PINCR,PVAL)
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    !
    IF(DATAEXCHANGE) THEN
      IF(IPRINT(4) .AND. .NOT. CREATEINITFILES .AND. .NOT. PREDICT) &
           CALL UTLUCODE_DX_WRITE_RESID (IOUT,MPR,NOBS,MODELVAL, &
                 MODELPRIVAL,OBSNAM,OBSVAL,OUTNAM,PLOTSYMBOLPRI, &
                 PRINAM,PRIVAL,PRIWTMATSQR, &
                 RESIDS,RESIDSPRI,WTDRESIDS,WTDRESIDSPRI,WTMATSQR)

      IF(IPRINT(4) .AND. PREDICT .AND. .NOT. SENSITIVITIES) &
        CALL UTLUCODE_DX_WRITE_PREDS &
            (IFAIL,IOUT,MPR,MPR,NOBS,PNPERD,NPEFP,NPS,DATAEXCHANGE,PNPTR, &
            LN,MODELVAL,PRINAM,PRIWTMAT,PRIWTMATSQR,PVAL,OBSNAM,OUTNAM, &
            OUTNAMPRED,PARNAM, WTFULL,XPRI,PXSENSTRD,SENSITIVITIES)
      IF(IFAIL > 0)CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')


      IF(OPTIMIZE .AND. ITERP>0) THEN
        CALL REG_GNMOD_EVA_SUMMARY(MAXITER,NOBS,NPE,NPS,FINALSTATS, &
                        IFO,INCLU,ILOW,ITERP, &
                        IPTR,OUTNAM,PAREST,PARNAM,PVAL,PVALINIT, &
                        REACT,RSQALL,RSQD,TRUSTREGION)
      ENDIF
    ENDIF
    IF(.NOT. PREDICT) THEN
      IF(.NOT. LOWEST_MOD) THEN
        IF (MPR .GT. 0) THEN
          WRITE(*,391)RSQD(ITER),RSQALL(ITER),NOBSINC,NUSEOBS,MPR
          IF(SOSSURFACE) THEN
            WRITE(IOUT,491)RSQD(ITER),RSQALL(ITER),NOBSINC,NUSEOBS,MPR
          ELSE
            WRITE(IOUT,391)RSQD(ITER),RSQALL(ITER),NOBSINC,NUSEOBS,MPR
          ENDIF
        ELSE
          WRITE(*,395)RSQD(ITER),NOBSINC,NUSEOBS
          IF(SOSSURFACE) THEN
            WRITE(IOUT,495)RSQD(ITER),NOBSINC,NUSEOBS
          ELSE
            WRITE(IOUT,395)RSQD(ITER),NOBSINC,NUSEOBS
          ENDIF
        ENDIF
        IF(.NOT. SOSSURFACE) WRITE(IOUT,400)
      ENDIF
    ENDIF
    !
    DEALLOCATE(DNPP)
    RETURN
  END SUBROUTINE UCODE_UEV_RESID_PRINT_DECISION
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_UEV_SENS_PRINT_DECISION(IFAIL,FINALSTATS,IOUT,ITERP, &
                         MPR,MPRWOP,OUTNAM,OUTNAMPRED,PRIWTMAT,PRIWTMATSQR)
    !   Determine what to print and print
    !   Argument-list variables
    USE UTILITIES
    IMPLICIT NONE
    ! Argument list variables
    INTEGER,                         INTENT(INOUT) :: IFAIL
    LOGICAL,                         INTENT(IN) :: FINALSTATS
    INTEGER,                         INTENT(IN) :: IOUT
    INTEGER,                         INTENT(IN) :: ITERP
    INTEGER,                         INTENT(IN) :: MPR
    INTEGER,                         INTENT(IN) :: MPRWOP
    CHARACTER(LEN=MAX_STRING_LEN),   INTENT(IN) :: OUTNAM
    CHARACTER(LEN=MAX_STRING_LEN),   INTENT(IN) :: OUTNAMPRED
    TYPE (CDMATRIX),                 INTENT(IN) :: PRIWTMAT
    TYPE (CDMATRIX),                 INTENT(IN) :: PRIWTMATSQR
    ! local variables
    INTEGER                                   :: I
    INTEGER                                   :: ICSS = 0
    INTEGER                                   :: IDSS = 0
    INTEGER                                   :: IONEPCT = 0
    INTEGER                                   :: IUSS = 0
    INTEGER                                   :: IUSU = 0
    INTEGER                                   :: J
    INTEGER                                   :: K
    INTEGER                                   :: MXRECL
    !FORMAT
    100 FORMAT (//,1X,52('*'),/,1X,52('*'),//,2X, &
    'VIEW PREDICTION SCALED SENSITIVITIES IN _sp* files ', &
    /,'  described in Chapter 14 of the UCODE_2005 report', &
    //,1X,52('*'),/,1X,52('*'))
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_UEV_SENS_PRINT_DECISION'
    IFAIL = 0
    !
    IF(.NOT. OPT) THEN
      DO I=1,NPE
        PAREST(ITERP+1,IPTR(I))=PVAL(I)
      ENDDO
    ENDIF
    !DETERMINE WHETHER TO PRINT FULL SENSITIVITY TABLES ************************
    IF (SENSITIVITIES) THEN
      ICSS=0;
      IDSS=0;
      IONEPCT=0;
      IUSS=0;
      IF (ITERP .LE. 0) THEN
        IF(STARTSENS .EQ. 5) THEN
          ICSS=1
          IDSS=1
          IONEPCT=1
          IUSS=1
        ELSEIF(STARTSENS .EQ. 4) THEN
          ICSS=1
          IDSS=1
          IONEPCT=1
        ELSEIF(STARTSENS .EQ. 3) THEN
          ICSS=1
          IONEPCT=1
        ELSEIF(STARTSENS .EQ. 2) THEN
          ICSS=1
          IUSS=1
        ELSEIF(STARTSENS .EQ. 1) THEN
          ICSS=1
          IDSS=1
        ELSEIF(STARTSENS .EQ. 0) THEN
          ICSS=1
        ENDIF
      ELSEIF(ITERP > 0 .AND. .NOT. FINALSTATS) THEN
        IF(INTERMEDSENS .EQ. 5) THEN
          ICSS=1
          IDSS=1
          IONEPCT=1
          IUSS=1
        ELSEIF(INTERMEDSENS .EQ. 4) THEN
          ICSS=1
          IDSS=1
          IONEPCT=1
        ELSEIF(INTERMEDSENS .EQ. 3) THEN
          ICSS=1
          IONEPCT=1
        ELSEIF(INTERMEDSENS .EQ. 2) THEN
          ICSS=1
          IUSS=1
        ELSEIF(INTERMEDSENS .EQ. 1) THEN
          ICSS=1
          IDSS=1
        ELSEIF(INTERMEDSENS .EQ. 0) THEN
          ICSS=1
        ENDIF
      ELSEIF(ITERP > 0 .AND. FINALSTATS) THEN
        IF(FINALSENS .EQ. 5) THEN
          ICSS=1
          IDSS=1
          IONEPCT=1
          IUSS=1
        ELSEIF(FINALSENS .EQ. 4) THEN
          ICSS=1
          IDSS=1
          IONEPCT=1
        ELSEIF(FINALSENS .EQ. 3) THEN
          ICSS=1
          IONEPCT=1
        ELSEIF(FINALSENS .EQ. 2) THEN
          ICSS=1
          IUSS=1
        ELSEIF(FINALSENS .EQ. 1) THEN
          ICSS=1
          IDSS=1
        ELSEIF(FINALSENS .EQ. 0) THEN
          ICSS=1
        ENDIF
      ENDIF
      IF(.NOT. PREDICT) THEN
        CALL UCODE_UEV_CSS(IFAIL,IDSS,ICSS,IONEPCT,IOUT,IUSS)
      ELSE
        WRITE(*,100)
        WRITE(IOUT,100)
      ENDIF
      IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from UCODEMOD ')
      CALL UCODE_UEV_BSCALMESS(IFAIL,PVAL,BSCAL,IOUT)
      IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from UCODEMOD ')
      IF(.NOT. PREDICT .AND. DATAEXCHANGE)THEN
         CALL UTLUCODE_DX_WRITE_SEN &
             (IOUT,MPR,NCOVMAT,NOBS,NPE,NPS,LN,OBSNAM,OMIT,OUTNAM,IPTR,PARNAM, &
              PRINAM,PVAL,WTFULL,WTFULLSQR,PRIWTMAT,PRIWTMATSQR,XPRI,XSENST)
        !   Write matrix of sensitivities for prior information, in blocks   &
        !   of NPPREC parameters
        IF(MPR > 0) THEN
          MXRECL = (NPPREC*26)+52
          IUSU = UTL_DX_OPEN(OUTNAM,'_supri','REPLACE',MXRECL)
          CALL SEN_UEV_DX_WRITE_MATRIX(IUSU,MPR,NPE,NPS,PRINAM,IPTR,PARNAM, &
                                       XPRI,PLOTSYMBOLPRI)
          IUSU = UTL_DX_CLOSE('_supri')
        ENDIF
      ENDIF
      IF((ITERP == 0 .AND. OPTIMIZE .AND. DATAEXCHANGE) .OR. CREATEINITFILES) &
        THEN
        CALL UTLUCODE_DX_WRITE_SU(MPR,NOBS,NPE,NPS,OBSNAM,OUTNAM, &
                                  IPTR,PARNAM,PRINAM,XPRI,XSENST)
      ENDIF
    ENDIF
    IF(PREDICT) THEN
      ALLOCATE(PNPTR(PNPERD),PXSENSTRD(PNPERD,NOBS))
      J = 0
      K = 0
      DO I=1,NPS
        IF(PINCR(I) >= 0) THEN
          ! COUNT ADJUSTABLES
          J = J + 1
          IF(PINCR(I) > 0) THEN
            ! COUNT INCLUDED
            K = K + 1
            PNPTR(K) = I
          ENDIF
        ENDIF
      ENDDO
      PXSENSTRD = XSENST
      CALL UTLUCODE_DX_WRITE_PREDS &
            (IFAIL,IOUT,MPR,MPRWOP,NOBS,PNPERD,NPEFP,NPS,DATAEXCHANGE,PNPTR, &
            LN,MODELVAL,PRINAM,PRIWTMAT,PRIWTMATSQR,PVAL,OBSNAM,OUTNAM, &
            OUTNAMPRED,PARNAM,WTFULL,XPRI,PXSENSTRD,SENSITIVITIES)
      IF(IFAIL > 0)CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
    ENDIF
    RETURN
  END SUBROUTINE UCODE_UEV_SENS_PRINT_DECISION
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_UEV_BSCALMESS(IFAIL,B,BSCAL,IOUT)
    !   Determine if bscal will apply to scaling of any parameters.  If
    !   so, write message and list parameter(s)
    USE GLOBAL_DATA, ONLY: IVERB
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                           INTENT(INOUT) :: IFAIL
    DOUBLE PRECISION, DIMENSION(NPS),  INTENT(IN) :: B !Current parameter values
    DOUBLE PRECISION, DIMENSION(NPS),  INTENT(IN) :: BSCAL
    INTEGER,                           INTENT(IN) :: IOUT
    !   Local variables
    INTEGER :: IIPP, IP, K
    INTEGER, ALLOCATABLE, DIMENSION(:) :: IBSFLG
    DOUBLE PRECISION :: BB
    !   Format statements
    500 FORMAT(/,   &
        ' FOR SCALING OF THE SENSITIVITIES, B IS REPLACED BY',/,   &
        ' SCALEPVAL (THE ALTERNATE SCALING FACTOR) FOR PARAMETER(S):')
    510 FORMAT(3X,6(2X,A))
    520 FORMAT(/,1X,'No parameter will use SCALEPVAL for scaling')
    !
    ALLOCATE(IBSFLG(NPS))
    !
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_UEV_BSCALMESS'
    IFAIL = 0
    !   Determine if BSCAL applies to any parameter(s)
    K = 0
    DO 10 IP = 1,NPE
      IIPP = IPTR(IP)
      IF (LN(IIPP).LE.0) THEN
        !   Parameter is not log-transformed
        BB = ABS(B(IIPP))
        IF (BB.LT.BSCAL(IIPP)) THEN
          K = K + 1
          IBSFLG(K) = IIPP
        ENDIF
      ENDIF
    10 CONTINUE
    !
    !   Write message listing parameters to which BSCAL applies
    IF (K.GT.0) THEN
      WRITE(IOUT,500)
      WRITE(IOUT,510) (PARNAM(IBSFLG(IP)),IP=1,K)
    ELSE
      IF (IVERB>4) WRITE(IOUT,520)
    ENDIF
    !
    DEALLOCATE(IBSFLG)
    RETURN
  END SUBROUTINE UCODE_UEV_BSCALMESS
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_UEV_CALCINTWT &
                             (MPR,NPE,IOUT,MPRWOP,OUTNAM,OUTNAMPRED,RESIDSPRI)
  USE SENSITIVITY, ONLY: SEN_UEV_DX_READ_SU
  !calculate weighted residuals for interval limit
  IMPLICIT NONE
  ! Argument-list variables
  INTEGER,                                 INTENT(IN) :: MPR
  INTEGER,                                 INTENT(IN) :: NPE
  INTEGER,                                 INTENT(IN) :: IOUT
  INTEGER,                                 INTENT(IN) :: MPRWOP
  CHARACTER(LEN=MAX_STRING_LEN),           INTENT(IN) :: OUTNAM
  CHARACTER(LEN=MAX_STRING_LEN),           INTENT(IN) :: OUTNAMPRED
  DOUBLE PRECISION,                        INTENT(IN) :: RESIDSPRI(MPR)
  ! Local variables
  DOUBLE PRECISION                                    :: DTMPA
  CHARACTER(LEN=MAX_STRING_LEN)                       :: FN
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)         :: GG
  INTEGER                                             :: I
  INTEGER                                             :: J
  INTEGER                                             :: K
  INTEGER                                             :: L
  LOGICAL                                             :: LEX = .FALSE.
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:)   :: ONAM
  CHARACTER(LEN=MAX_STRING_LEN)                       :: OUTNAMTMP
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)        :: PANAM
  INTEGER, ALLOCATABLE, DIMENSION(:)                  :: PLOTSYM
  INTEGER, ALLOCATABLE, DIMENSION(:)                  :: PLOTSYMBOL
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)         :: RESINT
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)         :: SCLE
  DOUBLE PRECISION                                    :: TMPA
  DOUBLE PRECISION                                    :: TMPB
  DOUBLE PRECISION                                    :: UG
  DOUBLE PRECISION                                    :: UGUZ
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)         :: UU
  DOUBLE PRECISION                                    :: UZ
  DOUBLE PRECISION                                    :: W
  DOUBLE PRECISION                                    :: WTR
  TYPE (CDMATRIX), SAVE                               :: WTINT
  TYPE (CDMATRIX), SAVE                               :: WTINTSQR
  TYPE (CDMATRIX)                                     :: WTINTDEP
  TYPE (CDMATRIX)                                     :: WTINTDEPSQR
  TYPE (CDMATRIX)                                     :: PRIWTINT
  TYPE (CDMATRIX)                                     :: PRIWTINTSQR
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)       :: XINT
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)       :: XINTDEP
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)       :: XTW
  ! Formats
  1040 FORMAT(/,1X,'WEIGHT MATRIX FOR OBSERVATIONS WILL BE READ FROM', &
                 ' FILE: ',/,20X,A)
  1041 FORMAT(/,1X,'WEIGHT MATRIX FOR PRIOR WILL BE READ FROM', &
                 ' FILE: ',/,20X,A)
  1042 FORMAT(/,1X,'WEIGHT MATRIX FOR PRIOR FOR PREDICTION WILL BE READ FROM', &
                 ' FILE: ',/,20X,A)
  !
  IINTCNT = IINTCNT + 1
  IF(.NOT. ALLOCATED(INTWRVALS)) ALLOCATE(INTWRVALS(NUMINTERVALS,NUSEOBS+MPR))
  ALLOCATE(GG(NPE),PANAM(NPE),RESINT(NUSEOBS+MPR),SCLE(NPE),UU(NPE))
  ! READ THE WEIGHT MATRIX _wt
  IF(IINTCNT == 1) THEN
    IF(MPR > 0) THEN
      CALL TYP_NULL(WTINT)
      CALL TYP_NULL(WTINTSQR)
      CALL TYP_NULL(WTINTDEP)
      CALL TYP_NULL(WTINTDEPSQR)
      WRITE(IOUT,1040) TRIM(OUTNAM)//'._wt'
      CALL UTL_DX_READ_WT(IOUT,OUTNAM,'_wt',WTINTDEP,WTINTDEPSQR)
      FN = TRIM(OUTNAM)//'._wtpri'
      INQUIRE(FILE=FN,EXIST=LEX)
      IF(LEX) THEN
        CALL TYP_NULL(PRIWTINT)
        CALL TYP_NULL(PRIWTINTSQR)
        WRITE(IOUT,1041) TRIM(OUTNAM)//'._wtpri'
        CALL UTL_DX_READ_WT(IOUT,OUTNAM,'_wtpri',PRIWTINT,PRIWTINTSQR)
        CALL UTL_COMBINESQMATRIX(WTINTDEP,PRIWTINT,WTINT)
        CALL UTL_COMBINESQMATRIX(WTINTDEPSQR,PRIWTINTSQR,WTINTSQR)
        CALL TYP_DEALLOC(WTINTDEP)
        CALL TYP_DEALLOC(WTINTDEPSQR)
        CALL TYP_NULL(WTINTDEP)
        CALL TYP_NULL(WTINTDEPSQR)
        WTINTDEP = WTINT
        WTINTDEPSQR = WTINTSQR
        CALL TYP_DEALLOC(WTINT)
        CALL TYP_DEALLOC(WTINTSQR)
      ENDIF
      FN = TRIM(OUTNAMPRED)//'._wtprip'
      INQUIRE(FILE=FN,EXIST=LEX)
      IF(LEX) THEN
        CALL TYP_NULL(PRIWTINT)
        CALL TYP_NULL(PRIWTINTSQR)
        WRITE(IOUT,1042) TRIM(OUTNAMPRED)//'._wtprip'
        CALL UTL_DX_READ_WT(IOUT,OUTNAMPRED,'_wtprip',PRIWTINT,PRIWTINTSQR)
        CALL UTL_COMBINESQMATRIX(WTINTDEP,PRIWTINT,WTINT)
        CALL UTL_COMBINESQMATRIX(WTINTDEPSQR,PRIWTINTSQR,WTINTSQR)
        CALL TYP_DEALLOC(WTINTDEP)
        CALL TYP_DEALLOC(WTINTDEPSQR)
        CALL TYP_NULL(WTINTDEP)
        CALL TYP_NULL(WTINTDEPSQR)
        WTINTDEP = WTINT
        WTINTDEPSQR = WTINTSQR
        CALL TYP_DEALLOC(WTINT)
        CALL TYP_DEALLOC(WTINTSQR)
      ENDIF
      WTINT = WTINTDEP
      WTINTSQR = WTINTDEPSQR
      CALL TYP_DEALLOC(WTINTDEP)
      CALL TYP_DEALLOC(WTINTDEPSQR)
    ELSE
      CALL TYP_NULL(WTINT)
      CALL TYP_NULL(WTINTSQR)
      CALL UTL_DX_READ_WT(IOUT,OUTNAM,'_wt',WTINT,WTINTSQR)
    ENDIF
  ENDIF
  !     READ SENSITIVITIES ._init_su
  OUTNAMTMP = TRIM(OUTNAM)//'._init'
  ALLOCATE(PLOTSYMBOL(NUSEOBS),XINT(NPE,NUSEOBS+MPR),XTW(NPE,NUSEOBS+MPR))
  IF(MPR > 0) THEN
    ALLOCATE(XINTDEP(NPE-MPR+MPRWOP,NUSEOBS))
    ! READ XIN
    CALL SEN_UEV_DX_READ_SU(NUSEOBS,NPE-MPR+MPRWOP,OBSNAM,OUTNAMTMP,PANAM, &
                            PLOTSYMBOL,XINTDEP,'_su')
    DO I=1,NUSEOBS
      DO J=1,NPE-MPR+MPRWOP
        XINT(J,I) = XINTDEP(J,I)
      ENDDO
    ENDDO
    DEALLOCATE(XINTDEP)
    DO I=1,MPR
      DO J=1,NPE
        XINT(J,NUSEOBS+I) = XPRI(J,I)
      ENDDO
    ENDDO
  ELSE
    ! READ XINT
    CALL SEN_UEV_DX_READ_SU(NUSEOBS,NPE,OBSNAM,OUTNAMTMP,PANAM,PLOTSYMBOL,XINT)
  ENDIF
  ! read values saved at first iteration
  READ(IUWRP) XINT,SCLE,UU,UZ
  REWIND(IUWRP)
  ! calculate weighted residuals at nonlinear interval limits
  ! RECALCULATE GG AT INTITAL VALUES FOR GENERATING WEIGHTED RESIDUALS
  ! AT THIS LIMIT
  GG = 0.D0
  DO J=1,NUSEOBS
    RESINT(J) = RESIDS(J)
  ENDDO
  IF (MPR > 0) THEN
    DO J=1,MPR
      RESINT(NUSEOBS+J) = RESIDSPRI(J)
    ENDDO
  ENDIF
  CALL UTL_MATMUL_SUB(NPE,NUSEOBS+MPR,XINT,WTINT,XTW)
  GG =  MATMUL(XTW,RESINT)
  DEALLOCATE(XTW)
  ! SCALE G, THEN CALCULATE UG=Q'W^½(Y-F) AND UGUZ=Q'W^½(Y-F)/Q'Q
  UG=0.D0
  DO J=1,NPE
    DTMPA=SCLE(J)
    GG(J)=GG(J)/DTMPA
    UG=UG+UU(J)*GG(J)
    UU(J)=UU(J)/DTMPA
  ENDDO
  UGUZ=UG/UZ
  ! COMPUTE WEIGHTED RESIDUALS
  DO K=1,NUSEOBS+MPR
    IF (UTL_GETVAL(WTINT,K,K) < 0.D0) THEN
      INTWRVALS(IINTCNT,K) = 0.D0
      CYCLE
    ELSE
      WTR = 0.D0
      TMPB = 0.D0
      DO L=1,NUSEOBS+MPR
        IF (UTL_GETVAL(WTINT,L,L) > 0.D0) THEN
          TMPA = 0.D0
          DO I=1,NPE
            TMPA = TMPA + XINT(I,L) * UU(I)
          ENDDO
          W = UTL_GETVAL(WTINTSQR,K,L)
          WTR = WTR - W * TMPA
          TMPB = TMPB + W * RESINT(L)
        ENDIF
      ENDDO
      WTR = WTR * UGUZ + TMPB
      INTWRVALS(IINTCNT,K) = WTR
    ENDIF
  ENDDO
  ! IF THIS IS THE LAST INTERVAL PRINT THE WEIGHTED RESIDUALS
  IF(IINTCNT == NUMINTERVALS) THEN
    ALLOCATE(ONAM(NUSEOBS+MPR),PLOTSYM(NUSEOBS+MPR))
    DO I=1,NUSEOBS
      ONAM(I) = OBSNAM(I)
      PLOTSYM(I) = PLOTSYMBOL(I)
    ENDDO
    IF(MPR > 0) THEN
      DO I=1,MPR
        ONAM(I+NUSEOBS) = PRINAM(I)
        PLOTSYM(I+NUSEOBS) = PLOTSYMBOLPRI(I)
      ENDDO
    ENDIF
    CALL UTLUCODE_DX_WRITE_INTWR &
     (MPR+NUSEOBS,NUMINTERVALS,GNAMPRTS,NTYP,ONAM,OUTNAMPRED,PLOTSYM,INTWRVALS)
  ENDIF
  RETURN
  END SUBROUTINE UCODE_UEV_CALCINTWT
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_UEV_CSS (IFAIL,IDSS,ICSS,IONEPCT,IOUT,IUSS,PRT)
  !*****************************************************************************
  !
  ! DIMENSIONLESS SCALED SENSITIVITIES (SCALED BY (PARAMETER_VALUE*(wt**.5))
  !
  ! COMPOSITE SCALED SENSITIVITIES =
  !        ( (SUM OF THE SQUARED SENSITIVITIES) / (# of OBSERVATIONS) )**.5
  !
  ! ONE-PERCENT SCALED SENSITIVITIES (SCALED BY (PARAMETER_VALUE/100)
  !
  !*****************************************************************************
  !        SPECIFICATIONS:
  !------------------------------------------------------------------
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                                INTENT(INOUT) :: IFAIL
    INTEGER,                                INTENT(IN)    :: IDSS
    INTEGER,                                INTENT(IN)    :: ICSS
    INTEGER,                                INTENT(IN)    :: IONEPCT
    INTEGER,                                INTENT(IN)    :: IOUT
    INTEGER,                                INTENT(IN)    :: IUSS
    INTEGER,                      OPTIONAL, INTENT(IN)    :: PRT
    ! Local variables
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: DSS, OSS
    DOUBLE PRECISION CSSMAX
    INTEGER IP, JN, KN
    INTEGER IPRT
    CHARACTER(LEN=72) LABEL
    ! Formats
    98 FORMAT (1X,'SENSITIVITIES ARE FIT-INDEPENDENT STATISTICS')
    99 FORMAT (/,80('-'),/,80('-'),/)
    100 FORMAT (/,1X, &
    'COMPOSITE SCALED SENSITIVITIES ((SUM OF THE SQUARED DSS)/ND)**.5', &
    /,'  DSS = DIMENSIONLESS ', &
    'SCALED SENSITIVITIES (SCALED BY (PARAMETER_VALUE*(wt**.5))', &
    /,'  SENSITIVITIES ARE FIT-INDEPENDENT STATISTICS')
    110 FORMAT &
     (/,2X,'PARAMETER    COMPOSITE SCALED SENSITIVITY    RATIO TO MAXIMUM',/, &
        2X,'----------   ----------------------------    ----------------')
    111 FORMAT (2X,A,9X,1PE12.5,12X,1PE12.5)
    ! Allocate
    ALLOCATE (DSS(NPE,NOBS))
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_UEV_CSS'
    IFAIL = 0
    DSS = 0.D0
    CSS = 0.D0
    IF(PRESENT(PRT))THEN
      IPRT = PRT
    ELSE
      IPRT = 1
    ENDIF
    IF(IPRT > 0 .AND. .NOT. OPTNLUNC) THEN
      WRITE(IOUT,99)
    ENDIF
    !
    ! Print unscaled Sensitivity = Unscaled(XSENST) * PVAL * SQRT(WT)
    IF(IUSS .GT. 0) THEN
      LABEL = 'UNSCALED SENSITIVITIES'
      IF(IPRT > 0) THEN
        WRITE(IOUT,98)
        CALL SEN_UEV_WRITESENTABLE(NOBS,NPE,NPS,OBSNAM,TRIM(LABEL), &
                                 IOUT,IPTR,3,PARNAM,XSENST)
      ENDIF
    ENDIF
    ! Dimensionless Scaled Sensitivity = Unscaled(XSENST) * PVAL * SQRT(WT)
    IF(IDSS .GT. 0 .OR. ICSS .GT. 0) THEN
      IF(NCOVMAT>0) THEN
        DO IP=1,NPE
          DO JN=1,NOBS
            DO KN=1,NOBS
              DSS(IP,JN)= DSS(IP,JN) + XSENST(IP,KN)*UTL_GETVAL(WTFULLSQR,KN,JN)
            ENDDO
            IF(LN(IPTR(IP)).EQ.0)THEN
              IF(ABS(PVAL(IPTR(IP))) > BSCAL(IPTR(IP))) THEN
                DSS(IP,JN) = DSS(IP,JN) * PVAL(IPTR(IP))
              ELSE
                DSS(IP,JN) = DSS(IP,JN) * BSCAL(IPTR(IP))
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ELSE
        DO IP=1,NPE
          DO JN=1,NOBS
            IF(LN(IPTR(IP)).EQ.0)THEN
              IF(ABS(PVAL(IPTR(IP))) > BSCAL(IPTR(IP))) THEN
                DSS(IP,JN)=XSENST(IP,JN)*PVAL(IPTR(IP)) &
                                                   *UTL_GETVAL(WTFULLSQR,JN,JN)
              ELSE
                DSS(IP,JN)=XSENST(IP,JN)*BSCAL(IPTR(IP)) &
                                                   *UTL_GETVAL(WTFULLSQR,JN,JN)
              ENDIF
            ELSE
              DSS(IP,JN)=XSENST(IP,JN)*UTL_GETVAL(WTFULLSQR,JN,JN)
            ENDIF
         ENDDO
        ENDDO
      ENDIF
      LABEL = &
      'DIMENSIONLESS SCALED SENSITIVITIES (SCALED BY (PARAMETER_VALUE*(wt**.5))'
      IF(IPRT > 0 .AND. IDSS > 0)THEN
        WRITE(IOUT,98)
        CALL SEN_UEV_WRITESENTABLE(NOBS,NPE,NPS, &
                                 OBSNAM,TRIM(LABEL),IOUT,IPTR,3,PARNAM,DSS)
      ENDIF
    ENDIF
    !
    ! 1 % Scaled Sensitivity = Unscaled(XSENST) * PVAL /100
    IF(IONEPCT .GT. 0) THEN
      ALLOCATE (OSS(NPE,NOBS))
      DO IP=1,NPE
        DO JN=1,NOBS
          OSS(IP,JN)=XSENST(IP,JN)*PVAL(IPTR(IP))/100.D0
        ENDDO
      ENDDO
      LABEL='ONE-PERCENT SCALED SENSITIVITIES (SCALED BY (PARAMETER_VALUE/100))'
      IF(IPRT > 0) THEN
        WRITE(IOUT,98)
        CALL SEN_UEV_WRITESENTABLE(NOBS,NPE,NPS,OBSNAM,TRIM(LABEL), &
                                 IOUT,IPTR,3,PARNAM,OSS)
      ENDIF
      DEALLOCATE(OSS)
    ENDIF
    ! Composite Scaled Sensitivity = SQRT ( SUM ((DSS*DSS)/NOBS))
    IF(ICSS .GT. 0) THEN
      CSSMAX = 0
      DO IP=1,NPE
        DO JN=1,NOBS
          CSS(IP)= CSS(IP) + DSS(IP,JN)*DSS(IP,JN)
        ENDDO
          CSS(IP)= DSQRT(CSS(IP)/NOBS)
          IF(CSS(IP) > CSSMAX) CSSMAX = CSS(IP)
      ENDDO
      IF(IPRT > 0) THEN
        WRITE (IOUT,100)
        WRITE(IOUT,110)
        WRITE (*,100)
        WRITE(*,110)
        DO IP = 1, NPE
          WRITE (IOUT,111) PARNAM(IPTR(IP)),CSS(IP),CSS(IP)/CSSMAX
          WRITE (*,111) PARNAM(IPTR(IP)),CSS(IP),CSS(IP)/CSSMAX
        ENDDO
      ENDIF
    ENDIF
    DEALLOCATE (DSS)
    RETURN
  END SUBROUTINE UCODE_UEV_CSS
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_UEV_MERGE (IFAIL)
  !*****************************************************************************
  !     MERGE SENSITIVITY FILES
  !*****************************************************************************
  !        SPECIFICATIONS:
  !------------------------------------------------------------------
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                         INTENT(INOUT) :: IFAIL
    ! Local variables
    INTEGER                          :: I
    INTEGER                          :: IUFILE
    INTEGER                          :: IUMERGE
    INTEGER                          :: J
    CHARACTER(LEN=MAX_STRING_LEN)    :: LINE
    ! Formats
    400 FORMAT(A)
    404 FORMAT(' MERGING SENSITIVITY FILES')
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_UEV_MERGE'
    IFAIL = 0
    WRITE(*,404)
    !
    IUMERGE = UTL_GETUNIT(101,150)
    OPEN(UNIT=IUMERGE,FILE=TRIM(MERGEPATH),STATUS='REPLACE')
    DO I=1,NMERGE
      IUFILE = UTL_GETUNIT(101,150)
      OPEN(UNIT=IUFILE,FILE=TRIM(FILES(I)),STATUS='OLD')
      DO J=1,SKIPLINES(I)
        READ(IUFILE,400)LINE
      ENDDO
      DO
        READ(IUFILE,400,END=500)LINE
        WRITE(IUMERGE,400)TRIM(LINE)
      ENDDO
      500 CLOSE(IUFILE)
    ENDDO
    CLOSE(IUMERGE)
    RETURN
  END SUBROUTINE UCODE_UEV_MERGE
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_GEN_INITRESSEN(MPR,NOBS,NPE,TASK,RESIDSPRI,WTDRESIDSPRI)
  IMPLICIT NONE
  !   Argument-list variables
  INTEGER,                                 INTENT(IN)    :: MPR
  INTEGER,                                 INTENT(IN)    :: NOBS
  INTEGER,                                 INTENT(IN)    :: NPE
  INTEGER,                                 INTENT(IN)    :: TASK
  DOUBLE PRECISION, OPTIONAL, DIMENSION(MPR),  INTENT(INOUT) :: RESIDSPRI
  DOUBLE PRECISION, OPTIONAL, DIMENSION(MPR),  INTENT(INOUT) :: WTDRESIDSPRI
  IF(TASK == 0) THEN
    SENFORINIT = .TRUE.
    ALLOCATE(INITMODVAL(NOBS),INITRESID(NOBS),INITWTDRESID(NOBS), &
             INITXSEN(NPE,NOBS))
    INITMODVAL      = MODELVAL
    INITRESID       = RESIDS
    INITWTDRESID    = WTDRESIDS
    INITXSEN        = XSENST
    CALL REG_GNMOD_UNCINI(MPR,NOBS,NPE,XSENST)
    IF(MPR > 0) THEN
      ALLOCATE(INITRESIDPRI(MPR),INITWTDRESIDPRI(MPR),INITXPRI(NPE,MPR))
      INITRESIDPRI    = RESIDSPRI
      INITWTDRESIDPRI = WTDRESIDSPRI
      INITXPRI        = XPRI
      CALL REG_GNMOD_UNCINI(MPR,NOBS,NPE,XSENST,XPRI)
    ENDIF
  ELSE
    MODELVAL     =  INITMODVAL
    RESIDS       =  INITRESID
    WTDRESIDS    =  INITWTDRESID
    XSENST       =  INITXSEN
    IF(MPR > 0) THEN
      RESIDSPRI    =  INITRESIDPRI
      WTDRESIDSPRI =  INITWTDRESIDPRI
      XPRI         =  INITXPRI
    ENDIF
  ENDIF
  RETURN
  END SUBROUTINE UCODE_GEN_INITRESSEN
!===============================================================================
  SUBROUTINE UCODE_UEV_OMIT (IFAIL,FINALSTATS,IOUT,ITERP,WTINIT,WTINITSQR,DTWT)
  !*****************************************************************************
  !     put wt matrix back to original, then check for omitted obs and
  !     alter wtfull to have small weights for omittted obs and
  !     count those obs
  !*****************************************************************************
  !        SPECIFICATIONS:
  !------------------------------------------------------------------
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                           INTENT(INOUT) :: IFAIL
    LOGICAL,                           INTENT(IN)    :: FINALSTATS
    INTEGER,                           INTENT(IN)    :: IOUT
    INTEGER,                           INTENT(IN)    :: ITERP
    TYPE (CDMATRIX),                   INTENT(IN)    :: WTINIT
    TYPE (CDMATRIX),                   INTENT(IN)    :: WTINITSQR
    DOUBLE PRECISION,                  INTENT(OUT)   :: DTWT
    !   Local variables
    INTEGER :: I, J, N, NNZ
    INTEGER(KIND=8) :: IIPOS
    TYPE (CDMATRIX)  :: WTFULL_TEMP
    TYPE (CDMATRIX)  :: WTFULLSQR_TEMP
    ! Formats
    100 FORMAT(/,' NUMBER OF OMITTED OBSERVATIONS = ',I6)
    120 FORMAT(/,' NUMBER OF OMITTED PREDICTIONS = ',I6)
    140 FORMAT(/,' WEIGHT MATRIX IS NOT POSITIVE DEFINITE, REDEFINE MATRIX ')
    ! Initialize
    CALL TYP_NULL(WTFULL_TEMP)
    CALL TYP_NULL(WTFULLSQR_TEMP)
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_UEV_OMIT'
    IFAIL = 0
    MINWT = 0.D0
    ! FIND SMALLEST WEIGHT
    ! CHECK WHETHER WORK IS REQUIRED
    IF(OMIT_DEFAULT > 0) THEN
      IF(IOMIT > 0) THEN
        NNZ = WTFULL%NNZ
        MINWT = WTFULL%DVAL(1)
        IF(NNZ > 1) THEN
          DO I=2,NNZ
            IF(WTFULL%DVAL(I) < MINWT) MINWT = WTFULL%DVAL(I)
          ENDDO
        ELSE
          MINWT = WTFULL%DVAL(1)
        ENDIF
      ENDIF
      IF(MINWT .NE. 0) THEN
        MINWT = MINWT * 1.D-30
      ELSE
        MINWT = 1.D-30
      ENDIF
      !
      WTFULL = WTINIT
      WTFULLSQR = WTINITSQR
      IOMIT = 0
      OMIT = 0
      DO I=1,NOBS
        DO J=1,OMIT_DEFAULT
          IF(MODELVAL(I) == OMITIT(J)) THEN
            OMIT(I)=-1
            ! ASSIGN-CDMATRIX-ELEMENT I for N=1,NOBS = MINWT
            DO N=1,NOBS
              IIPOS = UTL_GETPOS(WTFULL,I,N)
              IF(IIPOS>0) THEN
                WTFULL%DVAL(IIPOS) = MINWT
                WTFULLSQR%DVAL(IIPOS) = MINWT
              ENDIF
              IIPOS = UTL_GETPOS(WTFULL,N,I)
              IF(IIPOS>0) THEN
                WTFULL%DVAL(IIPOS) = MINWT
                WTFULLSQR%DVAL(IIPOS) = MINWT
              ENDIF
            ENDDO
            IOMIT = IOMIT + 1
          ENDIF
        ENDDO
      ENDDO
      INCLU(ITERP+1) = NUSEOBS - IOMIT
      IF(.NOT. LINEARITY) THEN
        IF (IDEPTYPEU==1) THEN
          WRITE(IOUT,100)IOMIT
        ELSEIF (IDEPTYPEU==2) THEN
          WRITE(IOUT,120)IOMIT
        ENDIF
      ENDIF
    ELSEIF(ITERP == 1 .OR. FINALSTATS .OR. OMIT_DEFAULT > 0) THEN
      INCLU(ITERP+1) = NUSEOBS - IOMIT
      ! GET LN DETERMINANT WTFULL - UTLSVD returns log of invers, so change sign
      WTFULL_TEMP = WTFULL
      WTFULLSQR_TEMP = WTFULLSQR
      IF(LCOR) THEN
        !   Observation errors are correlated
        CALL UTL_SVD(IFAIL,WTFULL_TEMP,WTFULLSQR_TEMP,DTWT)
        IF(IFAIL > 0) THEN
          WRITE(IOUT,140)
          WRITE(*,140)
        ENDIF
      ELSE
        !   Observation errors are not correlated
        CALL UTL_INVERT_DIAG(IFAIL,WTFULL_TEMP,WTFULLSQR_TEMP,DTWT)
        IF (IFAIL > 0) THEN
          AMESSAGE = 'Failed in UCODE_UEV_OMIT, cannot take log(-x)'
          CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
        ENDIF
      ENDIF
      DTWT = -DTWT
      CALL TYP_DEALLOC(WTFULL_TEMP)
      CALL TYP_DEALLOC(WTFULLSQR_TEMP)
    ENDIF
    RETURN
  END SUBROUTINE UCODE_UEV_OMIT
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_UEV_CALCWT (IFAIL,IOUT,ITERP,MAXITER,MPR, &
                               WTDRESIDSPRI,SUBST,DTWT)
  !*****************************************************************************
  !     recalculate weights given setting WTCONSTANTOS and
  !     WTTHRESHOLDOS
  !*****************************************************************************
  !        SPECIFICATIONS:
  !------------------------------------------------------------------
    USE DEPENDENTS, ONLY: STATISTIK, DEP_GET_GROUP, DEP_GET_WTMULTIPLIER
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                           INTENT(INOUT) :: IFAIL
    INTEGER,                           INTENT(IN)    :: IOUT
    INTEGER,                           INTENT(IN)    :: ITERP
    INTEGER,                           INTENT(IN)    :: MAXITER
    INTEGER,                           INTENT(IN)    :: MPR
    DOUBLE PRECISION,                  INTENT(IN)    :: WTDRESIDSPRI(MPR)
    LOGICAL,                           INTENT(IN)    :: SUBST
    DOUBLE PRECISION,                  INTENT(OUT)   :: DTWT
    !   Local variables
    DOUBLE PRECISION :: CALCWT
    DOUBLE PRECISION :: DENOMINATORSOS
    CHARACTER(LEN=12) :: GPNAM
    INTEGER(KIND=8) :: IIPOS
    DOUBLE PRECISION :: NUMERATORSOS
    INTEGER :: I, ND, NN, J
    DOUBLE PRECISION :: WTMULT
    TYPE (CDMATRIX)  :: WTFULL_TEMP
    TYPE (CDMATRIX)  :: WTFULLSQR_TEMP
    ! Formats
    100 FORMAT(/,' EVALUATING ADJUSTABLE WEIGHTS based on WtOSConstant', &
                 ' and TolParWtOS')
    101 FORMAT('   LAMBDA = ',G15.6)
    102 FORMAT('   OBSNAM= ',A20,' Weight = ',G15.6)
    701 FORMAT('   Use Simulated values for weights next iteration: ',L1)
    702 FORMAT('   Stop adjusting weights in subsequent iterations: ',L1)
    ! Initialize
    CALL TYP_NULL(WTFULL_TEMP)
    CALL TYP_NULL(WTFULLSQR_TEMP)
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_UEV_CALCWT'
    IFAIL = 0
    IF(.NOT. SUBST) THEN
      WRITE(IOUT,100)
      ! CALCULATE LAMBDA
      IF(LAMBDA > 0.D0) THEN
        ! not first iteration
        ! calc ratio SOSforOBSwithWtOSConstant>0 / SOSforOBSwithWtOSConstant=0
        NUMERATORSOS = 0.D0
        NN = 0
        DENOMINATORSOS = 0.D0
        ND = 0
        DO I = 1,NOBS
          IF(WTCOS(I) > 0.D0) THEN
            DENOMINATORSOS = DENOMINATORSOS + (WTDRESIDS(I))**2
            ND = ND + 1
          ELSE
            NUMERATORSOS = NUMERATORSOS + (WTDRESIDS(I))**2
            NN = NN + 1
          ENDIF
        ENDDO
        DO I = 1,MPR
          NUMERATORSOS = NUMERATORSOS + (WTDRESIDSPRI(I))**2
          NN = NN + 1
        ENDDO
        IF (NN .EQ. 0 .OR. ND .EQ. 0) THEN
          LAMBDA = 1.D0
        ELSE
          LAMBDA = (NUMERATORSOS/DBLE(NN)) / (DENOMINATORSOS/DBLE(ND))
        ENDIF
      ELSE
        ! first iteration, &
        ALLOCATE(SAVEWT(MAXITER+2,NOBS))
        SAVEWT = 0.D0
        ! calc cv for OBSwithWtOSConstant>0
        LAMBDA = 1.D0
      ENDIF
      WRITE(IOUT,101)LAMBDA
      WRITE(IOUT,701) WTADJNEXT
      WRITE(IOUT,702) WTADJFINAL
      ! Now calculate weights for observations with WtOSConstant>0
    ENDIF
    IF(.NOT. SUBST) THEN
      SAVEWTFINAL = ITERP+1
    ELSE
      ! in the future this will be used to calculate the weights
      ! with obs values and compare with those calculated with sim values
    ENDIF
    DO I=1,NOBS
      IIPOS = UTL_GETPOS(WTFULL,I,I)
      IF(WTCOS(I) > 0.D0) THEN
        CALL DEP_GET_GROUP(I,GPNAM,J,'USED')
        CALL DEP_GET_WTMULTIPLIER(GPNAM,WTMULT)
        IF(.NOT. SUBST) THEN
          IF(WTADJNEXT) THEN
            IF(MODELVAL(I) .NE. 0.D0) THEN
              CALCWT=(LAMBDA/(ABS(MODELVAL(I))*STATISTIK(I)+WTCOS(I))**2) &
                     * WTMULT
            ELSE
              CALCWT=(LAMBDA/(WTCOS(I)**2))*WTMULT
            ENDIF
          ELSE
            ! use eta for obs as well as sim CALCWT=WTFULL%DVAL(IIPOS)
            CALCWT=(1.D0/(OBSVAL(I)*STATISTIK(I)+WTCOS(I))**2)*WTMULT
          ENDIF
          ! fill remainder of savewt in case updating stops
          DO J = ITERP+1,MAXITER+2
            SAVEWT(J,I) = CALCWT
          ENDDO
        ELSE
          CALCWT = SAVEWT(ILOW,I)
        ENDIF
        WTFULL%DVAL(IIPOS) = CALCWT
        WTFULLSQR%DVAL(IIPOS) = SQRT(CALCWT)
        IF(IVERB > 3)WRITE(IOUT,102)OBSNAM(I),CALCWT
      ENDIF
    ENDDO
    ! GET LN DETERMINANT WTFULL - UTLSVD returns log of invers, so change sign
    WTFULL_TEMP = WTFULL
    WTFULLSQR_TEMP = WTFULLSQR
    IF (LCOR) THEN
      !   Observation errors are correlated
      CALL UTL_SVD(IFAIL,WTFULL_TEMP,WTFULLSQR_TEMP,DTWT)
    ELSE
      !   Observation errors are not correlated
      CALL UTL_INVERT_DIAG(IFAIL,WTFULL_TEMP,WTFULLSQR_TEMP,DTWT)
    ENDIF
    IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from UCODE_UEV_CALCWT ')
    DTWT = -DTWT
    CALL TYP_DEALLOC(WTFULL_TEMP)
    CALL TYP_DEALLOC(WTFULLSQR_TEMP)
    IF (IFAIL > 0) THEN
      AMESSAGE = 'Failed in UCODE_UEV_CALCWT, cannot take log(-x)'
      CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
      CALL UTL_STOP(' ')
    ENDIF
    RETURN
  END SUBROUTINE UCODE_UEV_CALCWT
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_UEV_LINEARITY (IFAIL,IOUT,LINEARITYDONE,LINEARITYLOOPS, &
                                  OUTNAM)
  !*****************************************************************************
  !     WRITE LINEARITY DEPENDENTS
  !*****************************************************************************
  !        SPECIFICATIONS:
  !------------------------------------------------------------------
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                           INTENT(INOUT) :: IFAIL
    INTEGER,                           INTENT(IN)    :: IOUT
    LOGICAL,                           INTENT(IN)    :: LINEARITYDONE
    INTEGER,                           INTENT(IN)    :: LINEARITYLOOPS
    CHARACTER(LEN=MAX_STRING_LEN),     INTENT(IN)    :: OUTNAM
    !   Local variables
    INTEGER                                     :: I
    LOGICAL, SAVE                               :: IDENTICAL
    INTEGER                                     :: MXRECL
    ! Formats
    100 FORMAT(//,78('!'),/)
    150 FORMAT(1X,A62,1X,2I5)
    200 FORMAT(/,78('!'),//)
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_UEV_LINEARITY'
    IFAIL = 0
    IF(LINEARITYLOOPS == 1) THEN
      IDENTICAL = .FALSE.
      ALLOCATE(OBSNAMB(NUSEOBS))
      DO I=1,NUSEOBS
          CALL UTL_CASE(OBSNAM(I),OBSNAMB(I),1)
      ENDDO
    ENDIF
    ! Write LINEARITY dependents for their parameter set to _b2
    DO I=1,NOBS
      MVALS(I,LINEARITYLOOPS) = MODELVAL(I)
    ENDDO
    IF(LINEARITYLOOPS > 1) THEN
      DO I=1,NOBS
        IF(MVALS(I,LINEARITYLOOPS) .NE. MVALS(I,LINEARITYLOOPS-1))  EXIT
        IF(I == NOBS) THEN
          IDENTICAL = .TRUE.
          AMESSAGE = &
          'WARNING: Identical simulated equivalents, for parameter sets: '
          WRITE(*,100)
          WRITE(*,150)TRIM(AMESSAGE),LINEARITYLOOPS-1,LINEARITYLOOPS
          WRITE(*,200)
          WRITE(IOUT,100)
          WRITE(IOUT,150)TRIM(AMESSAGE),LINEARITYLOOPS-1,LINEARITYLOOPS
          WRITE(IOUT,200)
        ENDIF
      ENDDO
    ENDIF
    IF(LINEARITYDONE) THEN
      MXRECL = (1+NOBS)*26
      IUB2 = UTL_DX_OPEN(OUTNAM,'_b2'//EXTB,'REPLACE',MXRECL)
      CALL UTLUCODE_DX_WRITE_MATRIXNLOBS(IUB2,NPERD*2,NOBS,OBSNAMB,MVALS)
      DEALLOCATE(OBSNAMB,MVALS)
      IF(IDENTICAL) THEN
        AMESSAGE = &
        ' Identical simulated equivalents, Beales Measure may be incorrect'
        WRITE(IOUT,100)
        WRITE(IOUT,*)TRIM(AMESSAGE)
        WRITE(*,100)
        WRITE(*,*)TRIM(AMESSAGE)
        AMESSAGE = &
        ' Did process model successfully converge for all parameter sets?'
        WRITE(IOUT,*)TRIM(AMESSAGE)
        WRITE(*,*)TRIM(AMESSAGE)
        WRITE(IOUT,200)
        WRITE(*,200)
      ENDIF
      IUB2 = UTL_DX_CLOSE('_b2'//EXTB)
    ENDIF
    RETURN
  END SUBROUTINE UCODE_UEV_LINEARITY
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_UEV_LINEARITY_ADV &
             (IFAIL,IOUT,LINEARITYDONE,LINEARITYLOOPS,MPR,MODELPRIVAL)
  !*****************************************************************************
  !     WRITE LINEARITY DEPENDENTS for LINEARITYADV = yes
  !*****************************************************************************
  !        SPECIFICATIONS:
  !------------------------------------------------------------------
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                           INTENT(INOUT) :: IFAIL
    INTEGER,                           INTENT(IN)    :: IOUT
    LOGICAL,                           INTENT(IN)    :: LINEARITYDONE
    INTEGER,                           INTENT(IN)    :: LINEARITYLOOPS
    INTEGER,                           INTENT(IN)    :: MPR
    DOUBLE PRECISION,                  INTENT(IN)    :: MODELPRIVAL(MPR)
    !   Local variables
    INTEGER :: I
    INTEGER :: IADJUST
    LOGICAL, SAVE :: IDENTICAL
    INTEGER :: IUB
    INTEGER :: J
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: GVAL
    INTEGER :: NM
    ! formats
    100 FORMAT(//,78('!'),/)
    150 FORMAT(1X,A62,1X,2I5)
    200 FORMAT(/,78('!'),//)
    300 FORMAT &
        (//,1X,'Pointer assignment is not Prediction Prior or Parameter',/, &
            1X,' Contact epoeter@mines.edu',//)
    !
    ALLOCATE(GVAL(NOINT))
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_UEV_LINEARITY_ADV'
    IFAIL = 0
    ! Determine INTERVAL VALUE
    IF (LINEARITYLOOPS == 1)IDENTICAL = .FALSE.
    IF (LINEARITYLOOPS .LE. (2*NOINT)) THEN
      IUB = IUB4
      IADJUST = 0
      J = LINEARITYLOOPS/2+MOD(LINEARITYLOOPS,2)
      IF(GNAMPTRTYP(J) == 1) THEN
        GVAL(J) = MODELVAL(GNAMPTR(J))
      ELSEIF(GNAMPTRTYP(J) == 2) THEN
        GVAL(J) = MODELPRIVAL(GNAMPTR(J))
      ELSEIF(GNAMPTRTYP(J) == 3) THEN
        GVAL(J) = PVAL(GNAMPTR(J))
      ELSE
        WRITE(*,300)
        CALL UTL_STOP('!!! ERROR UCODE_UEV_LINEARITY_ADV !!!')
      ENDIF
    ELSE
      IF (LINEARITYLOOPS == (2*NOINT+1)) THEN
        CALL UTLUCODE_DX_WRITE_MATRIXNLOBS(IUB4,NOINT*2,NUSEOBS+1,OBSNAMB,MVALS)
        IUB4 = UTL_DX_CLOSE('_b4'//EXTB)
        DEALLOCATE(OBSNAMB,MVALS)
        ALLOCATE(OBSNAMB(NUSEOBS),MVALS(NUSEOBS,NPERD*2))
        OBSNAMB = ' '
        MVALS = 0.D0
        DO I=1,NUSEOBS
          CALL UTL_CASE(OBSNAM(I),OBSNAMB(I),1)
        ENDDO
      ENDIF
      IUB = IUB2
      IADJUST = 2*NOINT
    ENDIF
    ! Write LINEARITY dependents for their parameter set to _b4
    DO I=1,NUSEOBS
      MVALS(I,LINEARITYLOOPS-IADJUST) = MODELVAL(I)
    ENDDO
    IF(LINEARITYLOOPS-IADJUST > 1) THEN
      DO I=1,NUSEOBS
        IF(MVALS(I,LINEARITYLOOPS-IADJUST) .NE. &
                 MVALS(I,LINEARITYLOOPS-IADJUST-1)) EXIT
        IF(I == NOBS-NPREDI) THEN
          IDENTICAL = .TRUE.
          AMESSAGE = &
          'WARNING: Identical simulated equivalents, for parameter sets: '
          WRITE(*,100)
          WRITE(*,150)TRIM(AMESSAGE), &
                    LINEARITYLOOPS-IADJUST-1,LINEARITYLOOPS-IADJUST
          WRITE(*,200)
          WRITE(IOUT,100)
          WRITE(IOUT,150)TRIM(AMESSAGE), &
                    LINEARITYLOOPS-IADJUST-1,LINEARITYLOOPS-IADJUST
          WRITE(IOUT,200)
        ENDIF
      ENDDO
    ENDIF
    ! yes I intend to use the finally incremented value of I
    IF(IUB == IUB4) MVALS(NUSEOBS+1,LINEARITYLOOPS-IADJUST) = GVAL(J)
    !
    IF(LINEARITYDONE) THEN
      NM = NUSEOBS
      IF(IUB == IUB4) NM = NM + 1
      CALL UTLUCODE_DX_WRITE_MATRIXNLOBS(IUB2,NPERD*2,NM,OBSNAMB,MVALS)
      DEALLOCATE(OBSNAMB,MVALS)
      IF(IDENTICAL) THEN
        AMESSAGE = &
        ' Identical simulated equivalents, Linearity Measure may be incorrect'
        WRITE(IOUT,100)
        WRITE(IOUT,*)TRIM(AMESSAGE)
        WRITE(*,100)
        WRITE(*,*)TRIM(AMESSAGE)
        AMESSAGE = &
        ' Did process model successfully converge for all parameter sets?'
        WRITE(IOUT,*)TRIM(AMESSAGE)
        WRITE(*,*)TRIM(AMESSAGE)
        WRITE(IOUT,200)
        WRITE(*,200)
      ENDIF
      IUB2 = UTL_DX_CLOSE('_b2adv'//EXTB)
    ENDIF
    DEALLOCATE(GVAL)
    RETURN
  END SUBROUTINE UCODE_UEV_LINEARITY_ADV
  !-----------------------------------------------------------------------------
  SUBROUTINE UCODE_UEV_RESIDUALS(NOBS,MODELVAL,WTMATSQR,RESIDS,WTDRESIDS)
    !   Calculate residuals and weighted residuals, using a full weight matrix.
    !   Specifically for non-detects
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                           INTENT(IN)  :: NOBS
    DOUBLE PRECISION, DIMENSION(NOBS), INTENT(IN)  :: MODELVAL
    TYPE (CDMATRIX),                   INTENT(IN)  :: WTMATSQR
    DOUBLE PRECISION, DIMENSION(NOBS), INTENT(OUT) :: RESIDS
    DOUBLE PRECISION, DIMENSION(NOBS), INTENT(OUT) :: WTDRESIDS
    !
    !   Local variables
    INTEGER :: I
    !
    !   Calculate residuals
    DO I=1,NOBS
      IF (NONDETVAL(I) > 0.D0) THEN
        IF(MODELVAL(I) < NONDETVAL(I)) THEN
          RESIDS(I) = 0.D0
        ELSE
          RESIDS(I) = NONDETVAL(I)-MODELVAL(I)
        ENDIF
      ELSE
        RESIDS(I) = OBSVAL(I)-MODELVAL(I)
      ENDIF
    ENDDO
    !   Calculate weighted residuals
    CALL UTL_MATMULVEC_SUB(NOBS,WTMATSQR,RESIDS,WTDRESIDS)
    !
    RETURN
  END SUBROUTINE UCODE_UEV_RESIDUALS
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_EVA(IFAIL,IOUT,ITERP,NDEROBS,MAXITER,MPR,DTPRIWT,   &
                   NONDETECTL,OUTNAM,PRINAM,PRIVAL,PRIWTMAT, &
                   PRIWTMATSQR,STATS_ON_NONCONVERGE,TRUSTREGION,AVET, &
                   IFO,IND,MODELPRIVAL,RESIDSPRI,UNUSUAL,WTDRESIDSPRI,WTMATSQR)
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                         INTENT(INOUT) :: IFAIL
    INTEGER,                         INTENT(IN) :: IOUT
    INTEGER,                         INTENT(IN) :: ITERP
    INTEGER,                         INTENT(IN) :: NDEROBS
    INTEGER,                         INTENT(IN) :: MAXITER
    INTEGER,                         INTENT(IN) :: MPR
    DOUBLE PRECISION,                INTENT(IN) :: DTPRIWT
    LOGICAL,                         INTENT(IN) :: NONDETECTL
    CHARACTER(LEN=MAX_STRING_LEN),   INTENT(IN) :: OUTNAM
    CHARACTER(LEN=LENDNAM),          INTENT(IN) :: PRINAM(MPR)
    DOUBLE PRECISION,                INTENT(IN) :: PRIVAL(MPR)
    TYPE (CDMATRIX),                 INTENT(IN) :: PRIWTMAT
    TYPE (CDMATRIX),                 INTENT(IN) :: PRIWTMATSQR
    LOGICAL,                         INTENT(IN) :: STATS_ON_NONCONVERGE
    LOGICAL,                         INTENT(IN) :: TRUSTREGION
    DOUBLE PRECISION,                INTENT(INOUT) :: AVET
    INTEGER,                         INTENT(INOUT) :: IFO
    INTEGER,                         INTENT(INOUT) :: IND
    DOUBLE PRECISION,                INTENT(INOUT) :: MODELPRIVAL(MPR)
    DOUBLE PRECISION,                INTENT(INOUT) :: RESIDSPRI(MPR)
    LOGICAL,                         INTENT(INOUT) :: UNUSUAL
    DOUBLE PRECISION,                INTENT(INOUT) :: WTDRESIDSPRI(MPR)
    TYPE (CDMATRIX),                 INTENT(IN)    :: WTMATSQR
    ! local variables
    INTEGER                                        :: AD = 1
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: DNPP
    INTEGER                                        :: I
    INTEGER                                        :: IPLUS = 1
    INTEGER                                        :: ITER
    INTEGER                                        :: ITP
    INTEGER                                        :: IUB
    INTEGER                                        :: IUPASUB
    INTEGER                                        :: J
    DOUBLE PRECISION                               :: PVTMP
    LOGICAL                                        :: SENS_NO_OPT = .FALSE.
    CHARACTER(LEN=30)                              :: WORD
    DOUBLE PRECISION                               :: AIC
    DOUBLE PRECISION                               :: BIC
    DOUBLE PRECISION                               :: DTLA
    DOUBLE PRECISION                               :: DTLANP
    DOUBLE PRECISION                               :: HQ
    DOUBLE PRECISION                               :: LNDETFN
    DOUBLE PRECISION                               :: LNDETFP
    DOUBLE PRECISION                               :: KASHYAP
    DOUBLE PRECISION                               :: KASHYAPNP
    DOUBLE PRECISION                               :: MLOFD
    DOUBLE PRECISION                               :: MLOFDP
    DOUBLE PRECISION                               :: STAT1
    DOUBLE PRECISION                               :: STAT2
    DOUBLE PRECISION                               :: VAR
    ! formats
    200 FORMAT(1X,'ITERATION: ',I5,/,1X,'"PARAMETER"  "ESTIMATE"')
    201 FORMAT(1X,A,6X,1PE25.16)
    !
    ALLOCATE(DNPP(NUSEOBS+MPR,2))
    !
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_EVA'
    IFAIL = 0
    ITER = ITERP
    IF(IFO<3 .OR. STATS_ON_NONCONVERGE) THEN
      ! clean up these calls and writes
      IPRINT(1) = .FALSE.
      IPRINT(3) = .FALSE.
      IF(DATAEXCHANGE .AND. .NOT. CREATEINITFILES) THEN
        IPRINT(4) = .TRUE.
        IF(MPR > 0) THEN
          CALL UTL_DX_WRITE_WT(OUTNAM,'_wtpri',PRIWTMAT,PRIWTMATSQR)
          CALL PRI_UEV_DX_WRITE_PR(MPR,OUTNAM)
        ENDIF
        CALL UTL_DX_WRITE_WT(OUTNAM,'_wt',WTFULL,WTFULLSQR)
      ENDIF
      IF(LOWEST_MOD) AD = AD+1
      CALL STA_UEV_INIT(AVET,NNEGT,NPOST,NRUNS,RSQD(ITERP+AD),RSQALL(ITERP+AD))
      CALL STA_UEV_FIT(IOUT,MPR,NOBS,IPRINT(1:3),NONDETECTL,MODELVAL, &
                    MODELPRIVAL,OBSNAM,OBSVAL,OMIT,OUTNAM,PLOTSYMBOLPRI,PRILN, &
                    PRINAM,PRIVAL,PRIWTCORR,PRIWTMATSQR, &
                    RESIDS,RESIDSPRI,WTCORRELATED, &
                    WTDRESIDS,WTDRESIDSPRI,WTFULLSQR, &
                    AVET,NNEGT,NPOST,NRUNS,RSQD(ITERP+AD),RSQALL(ITERP+AD), &
                    DNPP,NOBSINC,WTRL)
       IF(IPRINT(4) .AND. .NOT. CREATEINITFILES) &
             CALL UTLUCODE_DX_WRITE_RESID (IOUT,MPR,NOBS,MODELVAL, &
                   MODELPRIVAL,OBSNAM,OBSVAL,OUTNAM,PLOTSYMBOLPRI, &
                   PRINAM,PRIVAL,PRIWTMATSQR, &
                   RESIDS,RESIDSPRI,WTDRESIDS,WTDRESIDSPRI,WTMATSQR)
      IF(DATAEXCHANGE .AND. .NOT. CREATEINITFILES) &
             CALL STA_UEV_DX_WRITE_NM (MPR,NOBS,OBSNAM,OUTNAM, &
                            PLOTSYMBOLPRI,PRINAM,WTDRESIDS,WTDRESIDSPRI)
      IF(RESIDONLY .OR. (SENSITIVITIES .AND. .NOT. OPT)) THEN
        ITP = 1
      ELSE
        ITP = ITERP+2
      ENDIF
      CALL REG_GNMOD_EVA_ORD_NORM_RESIDS  (IFAIL,IOUT,MPR,NOBS, &
              OBSNAM,OMIT,PRINAM,RSQALL(ITP), &
              WTDRESIDS,WTDRESIDSPRI,STAT1,STAT2)
      IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from UCODE_EVA ')
      IF(RESIDONLY) THEN
        DEALLOCATE(DNPP)
        RETURN
      ENDIF
      IF(SENSITIVITIES .AND. .NOT. OPT) THEN
        NPERD = NPE
        SENS_NO_OPT = .TRUE.
        ITER = ITERP + 1
      ENDIF
      IF(OMIT_INSENS) CALL UCODE_UEV_CSS(IFAIL,0,1,0,IOUT,0,0)
      IF(OPTIMIZE) IPLUS=2
      CALL REG_GNMOD_EVA_FINAL_OUT (IFAIL,IOUT,IFO,IND,IOMIT,3,ITERP, &
              MAXITER,MPR,NDEROBS,NPD,NOBS,NPE,NPS,CSS, &
              DATAEXCHANGE,IPTR,LN,LPRINT,MODELVAL,CREATEINITFILES, &
              OBSVAL,OUTNAM,PARNAM,PARNAMLC,PVALINIT, &
              PVALMAX,PVALMIN,PRIVAL,PRIWTMAT,PRIWTMATSQR,SENS_NO_OPT, &
              RSQALL(ITERP+IPLUS),RSQD(ITERP+IPLUS), &
              STDERRONE,WTFULL,WTFULLSQR,XPRI,XSENST,DTLA,DTLANP,PVAL, &
              RESIDSPRI,LNDETFP,LNDETFN,VAR)
      IF(IFAIL > 0) THEN
        IF(SENS_NO_OPT) THEN
          CONTINUE
        ELSE
          CALL UTL_STOP(ERRSUB//' called from UCODE_EVA ')
          !-------CALC AND PRINT STATISTICS BASED ON MAX LIKELIHOOD OBJ FUNCTION
          !
        ENDIF
      ENDIF
      IF(.NOT. OPTIMIZE) IPLUS = IPLUS - 1
      IF(IFAIL < 1) CALL REG_GNMOD_EVA_MLOFSTATS &
              (IFAIL,IOMIT,IOUT,MPR,NOBS,DTLANP,DTLA, &
               DTPRIWT,LNDETFN,LNDETFP,RSQD(ITER+IPLUS), &
               RSQALL(ITER+IPLUS),AIC,BIC, &
               HQ,KASHYAPNP,KASHYAP,MLOFD,MLOFDP)
      IF(IFAIL < 1 .AND. (OPTIMIZE .OR. SENSITIVITIES) .AND. DATAEXCHANGE) &
             CALL UTLUCODE_DX_WRITE_B1 &
             (MPR,NOBS,NPERD,NPS,-1.D0,CMAT,IOUT,NPTR,LN,OUTNAM,PARNAM,PVAL)
    ENDIF
    IF(SENSITIVITIES .AND. OPTIMIZE) THEN
      DO I=1,NPERD
        IF(LN(NPTR(I)) .NE. 0)PVAL(NPTR(I)) = EXP(PVAL(NPTR(I)))
      ENDDO
      CALL REG_GNMOD_EVA_FINISH (DATAEXCHANGE,MAXITER,MPR,NOBS,NPE,NPS,IFO, &
                        ILOW,INCLU,INTERMEDRES,IOUT,ITERP,IPTR,OUTNAM, &
                        PAREST,PARNAM,PVAL,PVALINIT,REACT,RSQALL,RSQD, &
                        STATS_ON_NONCONVERGE,TRUSTREGION,UNUSUAL)
      IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from UCODE_EVA ')
    ENDIF
    ! WRITE PARAMETER ESTIMATES FOR EACH ITERATION TO _pa1 FILE
    ! FIRST, ASSIGN PAREST AS VALUES ACTUALLY WRITTEN TO MODEL-INPUT FILES
    DO I=1,ITERP
      DO J=1,NPE
        CALL UTL_WRTSIG(IFAIL,PAREST(I,IPTR(J)),WORD,NW(J),PRECIS,PVTMP,NOPNT)
        IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from UCODE_EVA ')
        PAREST(I,IPTR(J))=PVTMP
      ENDDO
    ENDDO
    IF((.NOT. OPTIMIZE .AND. SENSITIVITIES) .AND. DATAEXCHANGE) THEN
      CALL UTLUCODE_DX_WRITE_PA (ITERP,MAXITER,NPS,OUTNAM,PARNAM,PVALINIT, &
                                 PAREST,PVALINIT)
      !   Write parameter estimates to _pasub file
      IUPASUB = UTL_DX_OPEN(OUTNAM,'_pasub','REPLACE')
      WRITE(IUPASUB,200)I
      DO I=1,NPS
        WRITE(IUPASUB,201)PARNAM(I),PVALINIT(I)
      ENDDO
      IUPASUB = UTL_DX_CLOSE('_pasub')
      CALL UTLUCODE_DX_WRITE_PAOPT(NPS,LN,OUTNAM,PARNAM,PINCR,PVALINIT)
      CALL DEP_UEV_DX_WRITE_SS (MAXITER,INCLU,ITERP+1,OUTNAM,RSQD,RSQALL)
    ENDIF
    IF(OPTIMIZE .AND. DATAEXCHANGE) THEN
      CALL UTLUCODE_DX_WRITE_PA (ITERP,MAXITER,NPS,OUTNAM,PARNAM,PVALINIT, &
                                 PAREST,PVAL)
      CALL UTLUCODE_DX_WRITE_PAOPT(NPS,LN,OUTNAM,PARNAM,PINC(ITERP+1,:),PVAL)
      IF(MPR>0)CALL UTLUCODE_DX_WRITE_PAPRI(MPR,NPE,NPS,OUTNAM,PARNAM, &
                                                       PINC(ITERP+1,:),XPRI)
      CALL DEP_UEV_DX_WRITE_SS (MAXITER,INCLU,ITERP+1,OUTNAM,RSQD,RSQALL)
    ENDIF
    IF(IFO<3 .OR. STATS_ON_NONCONVERGE)THEN
      IF(DATAEXCHANGE) THEN
        CALL STA_UEV_DX_WRITE_DM(AIC,BIC,LNDETFN,HQ,IFO,ITERP,KASHYAPNP, &
                 MLOFD,MLOFDP,MODELLENGTH,MODELMASS,MODELNAME, &
                 MODELTIME,MPR,INCLU(ITER),NPE,NPERD,NPS,NOBS, &
                 OUTNAM,STAT1,STAT2,VAR)
        CALL UTLUCODE_DX_WRITE_DM(OUTNAM,LNDETFP,KASHYAP,SENTYPE,NPEFINAL, &
                                  DTLANP,DTLA)
      ENDIF
    ENDIF
    ! CLOSE FILES
    IF(LINEARITY) THEN
      IF(LINEARITYADV) THEN
        IUB = UTL_DX_CLOSE('_b4'//EXTB)
      ELSE
        IUB = UTL_DX_CLOSE('_b2'//EXTB)
      ENDIF
    ENDIF
    IF(ALLOCATED(DNPP))DEALLOCATE(DNPP)
    RETURN
  END SUBROUTINE UCODE_EVA
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_EVA_LOWEST(IFAIL,IOUT,ITERP,WTOSL,UNUSUAL) !
  USE REG_GNMOD, ONLY: PINCBND, PINCSEN, PSTATUS
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                   INTENT(INOUT) :: IFAIL
    INTEGER,                   INTENT(IN)    :: IOUT
    INTEGER,                   INTENT(IN)    :: ITERP
    LOGICAL,                   INTENT(IN)    :: WTOSL
    LOGICAL,                   INTENT(INOUT) :: UNUSUAL
    ! local variables
    INTEGER I, IP, J, K
    DOUBLE PRECISION RS
    !
    ! FORMATS
    90 FORMAT(/,1X,'INITIAL PARAMETER VALUES ',/, &
               ' YIELDED THE LOWEST SUM OF SQUARED RESIDUALS ',//)
    100 FORMAT(/,1X,'SEARCHING FOR ITERATION WITH LOWEST SUM OF SQUARED ', &
               'RESIDUALS',/)
    101 FORMAT(/,1X,'PARAMETER VALUES FROM THE FINAL ITERATION ',/, &
               ' YIELDED THE LOWEST SUM OF SQUARED RESIDUALS',//)
    102 FORMAT(/,1X,'PARAMETER VALUES FROM ITERATION ',I5,/, &
               ' YIELDED THE LOWEST SUM OF SQUARED RESIDUALS',//)
    103 FORMAT(/,80('*'),/,80('*'),/)
    104 FORMAT(/,80('*'),/)
    110 FORMAT (6(3X,A10))
    111 FORMAT (6(2X,1PG11.4))
    200 FORMAT (1X, &
    'REACTIVATE = no in the UCODE_CONTROL_DATA block so only the',/,3X, &
    'parameters included in that iteration will be activated for',/,3X, &
    'computation of final sensitivities and statistics. All parameters',/,3X, &
    'will be assigned the values they had in that iteration.',//)
    210 FORMAT (1X, &
    'REACTIVATE = starting in the UCODE_CONTROL_DATA block so all',/,3X, &
    'parameters will be activated for computation of final',/,3X, &
    'sensitivities and statistics. The parameters included in the',/,3X, &
    'iteration with the lowest sum of squared residuals will be',/,3X, &
    'assigned the value they had in that iteration, while parameters',/,3X, &
    'that were omitted in that iteration will be assigned their',/,3X, &
    'starting values as indicated in the PARAMETER_DATA or, if present,',/,3X, &
    'the PARAMETER_VALUES block.',//,1X, &
    'This could result in a poor fit in which case it is appropriate to',/,3X, &
    'repeat the regression with the inactive parameters not adjustable',/,3X, &
    'so appropriate values can be estimated for the active parameters.',//,1X, &
    'Parameters omitted in that iteration are:',/)
    211 FORMAT (5X,A)
    220 FORMAT (1X, &
    'REACTIVATE = final in the UCODE_CONTROL_DATA block so all',/,3X, &
    'parameters will be activated for computation of final',/,3X, &
    'sensitivities and statistics. All parameters will be assigned',/,3X, &
    'the values they had in the iteration with the lowest sum of',/,3X, &
    'squared residuals.',//,1X, &
    'Parameters omitted in that iteration are:',/)
    230 FORMAT(/,1X,'PARAMETER VALUES FROM THAT ITERATION ARE:',/)
    300 FORMAT(/,1X,'ONLY ITERATIONS STARTING WITH ITERATION ',I4, &
        ' WILL BE CONSIDERED BECAUSE',/, &
        '   AT LEAST SOME WEIGHTS WERE BASED ON SIMULATED VALUES AND WEIGHTS', &
        ' WERE',/,'   NOT UPDATED AFTER THAT ITERATION',/)
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_EVA_LOWEST'
    IFAIL = 0
    LOWEST_DIFFERS = .FALSE.
    LOWEST_MOD = .FALSE.
    NPEFINAL = NPE
    !
    WRITE(IOUT,100)
    WRITE(*,100)
    ! Note RSQALL(1) is the SSWR for initial parameters or iteration 0
    ! even though the counter is 1, so when ilow is found it is actually
    ! for values in iter ilow-1
    ! saveweight final will use only iteration after weights stopped changing
    IF(WTOSL) WRITE(IOUT,300)SAVEWTFINAL
    ILOW = SAVEWTFINAL
    RS = RSQALL(SAVEWTFINAL)
    DO I = SAVEWTFINAL+1,ITERP+1
      IF( RSQALL(I) .EQ. 0.D0) EXIT
      IF( RSQALL(I) <= RS ) THEN
        ILOW = I
        RS = RSQALL(I)
      ENDIF
    ENDDO
    RSQALL(ITERP+2) = RSQALL(ILOW)
    RSQD(ITERP+2) = RSQD(ILOW)
    INCLU(ITERP+2) = INCLU(ILOW)
    IF (ILOW == 1) THEN
      LOWEST_DIFFERS = .TRUE.
      LOWEST_MOD = .TRUE.
      ! Initial parameters gave lowest sos, activate all at initial values
      WRITE(IOUT,90)
      WRITE(*,90)
      DO I = 1,NPS
        PVAL(I) = PVALINIT(I)
        IF(PADJ(I)) THEN
          PINC(ITERP+1,I) = 1
        ELSE
          PINC(ITERP+1,I) = -1
        ENDIF
      ENDDO
      NPARSTAR(ITERP+1) = ' '
      WRITE (IOUT,110) (PARNAM(IPTR(IP)),IP=1,NPE)
      WRITE (IOUT,'(1X)')
      WRITE (IOUT,111) (PVAL(IPTR(IP)),IP=1,NPE)
    ELSEIF (ILOW <= ITERP+1) THEN
      IF(ILOW < ITERP+1) THEN
        LOWEST_DIFFERS = .TRUE.
        LOWEST_MOD = .TRUE.
        WRITE(IOUT,102)ILOW-1
        WRITE(*,102)ILOW-1
      ELSE
        WRITE(IOUT,101)
        WRITE(*,101)
      ENDIF
      ! determine if any parameters were omitted in the lowest sos iteration
      NPEFINAL = 0
      DO I=1,NPS
        IF(PINC(ILOW-1,I) == 1) NPEFINAL = NPEFINAL + 1
        IF(PINC(ILOW-1,I) == 0) PARAMS_OMITTED = .TRUE.
      ENDDO
      ! react=0 uses those active parameters & values from that iteration
      ! react=1 uses those active params & values, but makes inactive active with initial values
      ! react=2 uses those active params & values, but makes inactive active with final values
      !
      ! first replace all initially adjustable parameters with values from iteration with lowest sowsr
      DO I = 1,NPE
        IP = IPTR(I)
        PVAL(IP) = PAREST(ILOW-1,IP)
      ENDDO
      IF(.NOT. PARAMS_OMITTED .OR. REACT == 0) THEN
        ! REACT = 0 use active values from that iteration
        DO I = 1,NPS
          PINC(ITERP+1,I) = PINC(ILOW-1,I)
        ENDDO
        IF(PARAMS_OMITTED) THEN
          WRITE(IOUT,200)
          WRITE(*,200)
        ENDIF
      ELSEIF(REACT == 1) THEN
        IF(PARAMS_OMITTED) THEN
          WRITE(IOUT,210)
          WRITE(*,210)
        ENDIF
        DO I = 1,NPE
          IF(PINC(ILOW-1,IPTR(I)) == 0) THEN
            PVAL(IPTR(I)) = PVALINIT(IPTR(I))
            WRITE(IOUT,211)PARNAM(IPTR(I))
            WRITE(*,211)PARNAM(IPTR(I))
          ENDIF
        ENDDO
        IF(PARAMS_OMITTED) THEN
          WRITE(IOUT,230)
          WRITE(*,230)
        ENDIF
      ELSEIF(REACT == 2) THEN
        IF(PARAMS_OMITTED) THEN
          WRITE(IOUT,220)
          WRITE(*,220)
        ENDIF
        DO I = 1,NPE
          IF(PINC(ILOW-1,IPTR(I)) == 0) THEN
            WRITE(IOUT,211)PARNAM(IPTR(I))
            WRITE(*,211)PARNAM(IPTR(I))
          ENDIF
        ENDDO
        IF(PARAMS_OMITTED) THEN
          WRITE(IOUT,230)
          WRITE(*,230)
        ENDIF
      ENDIF
      IF(REACT > 0) THEN
        DO I = 1,NPS
          IF(PINC(ILOW-1,I) == -1) THEN
             PINC(ITERP+1,I) = -1
          ELSE
            PINC(ITERP+1,I) = 1
          ENDIF
        ENDDO
      ENDIF
      NPARSTAR(ITERP+2) = NPARSTAR(ILOW-1)
      WRITE (IOUT,110) (PARNAM(IPTR(IP)),IP=1,NPE)
      WRITE (IOUT,'(1X)')
      WRITE (IOUT,111) (PVAL(IPTR(IP)),IP=1,NPE)
      WRITE (*,110) (PARNAM(IPTR(IP)),IP=1,NPE)
      WRITE (*,'(1X)')
      WRITE (*,111) (PVAL(IPTR(IP)),IP=1,NPE)
    ENDIF
    WRITE(IOUT,103)
    WRITE(*,104)
    PSTATUS = ' '
    IF(ALLOCATED(NPTR)) DEALLOCATE (NPTR)
    IF(ALLOCATED(XPTR)) DEALLOCATE (XPTR)
    IF (ILOW == 1 .OR. REACT > 0) THEN
      ALLOCATE(NPTR(NPE),XPTR(NPE))
      NPTR = -1
      XPTR = -1
      J = 0
      DO I=1,NPS
        IF(PADJ(I)) THEN
          J = J + 1
          NPTR(J) = I
        ENDIF
      ENDDO
      DO I=1,NPE
        XPTR(I) = I
      ENDDO
      NPERD = NPE
    ELSE
      NPERD = 0
      DO I=1,NPS
        IF(PINC(ITERP+1,I) > 0) NPERD = NPERD + 1
      ENDDO
      J = 0
      ALLOCATE(NPTR(NPERD),XPTR(NPERD))
      NPTR = -1
      XPTR = -1
      K = 0
      DO I=1,NPS
        IF(PADJ(I)) K = K + 1
        IF(PINC(ITERP+1,I) > 0) THEN
          J = J + 1
          NPTR(J) = I
          XPTR(J) = K
        ENDIF
      ENDDO
    ENDIF
    IF(PARAMS_OMITTED .AND. REACT == 0) UNUSUAL = .TRUE.
    RETURN
  END SUBROUTINE UCODE_EVA_LOWEST
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_INI_PARNAMALL(IFAIL)
    !***************************************************************************
    !     JOIN PARAMETER NAMES WITH DERIVED PARAMETERS
    !***************************************************************************
    !        SPECIFICATIONS:
    !------------------------------------------------------------------
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                       INTENT(INOUT)  :: IFAIL
    !   Local variables
    INTEGER I
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_INI_PARNAMALL'
    IFAIL = 0
    DO I=1,NPSNPD
      IF(I <= NPS) THEN
        PARNAMALL(I)=PARNAMLC(I)
      ELSE
        PARNAMALL(I)=DERPARNAMELC(I-NPS)
      ENDIF
    ENDDO
    RETURN
  END SUBROUTINE UCODE_INI_PARNAMALL
  !=============================================================================
  !=============================================================================
  SUBROUTINE UCODE_GEN_PVALALL(IFAIL,IOUT,PSET,PADASET)
    !***************************************************************************
    !     EVALUATE DERIVED PARAMETERS
    !***************************************************************************
    !        SPECIFICATIONS:
    !------------------------------------------------------------------
    USE EQUATION, ONLY: EQN_EVALUATE
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                  INTENT(OUT) :: IFAIL
    INTEGER,                  INTENT(IN)  :: IOUT
    DOUBLE PRECISION,         INTENT(IN)  :: PSET(NPE)
    DOUBLE PRECISION,         INTENT(OUT) :: PADASET(NPSNPD)
    !   Local variables
    LOGICAL :: EQNLVAL
    INTEGER :: I
    INTEGER :: ITYP
    INTEGER :: J
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: PTMP
    !Formats
    10 FORMAT(/,80('<'))
    15 FORMAT(2X,'DERIVED PARAMETERS')
    20 FORMAT(3X,'NAME',15X,'VALUE')
    30 FORMAT(3X,A12,2X,G15.7)
    40 FORMAT(80('>'),/)
    100 FORMAT(/,1X,'Derived Parameters:',/)
    !
    ALLOCATE(PTMP(NPSNPD))
    !
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UCODE_GEN_PVALALL'
    IFAIL = 0
    PTMP = 0
    J = 0
    !
    IF(IVERB > 4) THEN
      WRITE(IOUT,10)
      WRITE(IOUT,15)
      WRITE(IOUT,20)
    ENDIF
    !
    IF(IVERB > 4 .AND. NPSNPD > NPS) WRITE(IOUT,100)
    DO I=1,NPSNPD
      IF(I <= NPS) THEN
        IF(PADJ(I)) THEN
          J = J + 1
          IF(PINCSEN(J) .EQ. 0 .AND. PINCBND(J) .EQ. 0) THEN
            PTMP(I) = PSET(J)
            PADASET(I)=PSET(J)
          ELSE
            PTMP(I) = PVAL(I)
            PADASET(I)=PVAL(I)
          ENDIF
        ELSE
          PTMP(I) = PVAL(I)
          PADASET(I)=PVAL(I)
        ENDIF
      ELSE
        CALL EQN_EVALUATE(IFAIL,I-NPS,NPSNPD,PARNAMALL,PTMP,ITYP, &
                                             PADASET(I),EQNLVAL)
        PTMP(I) = PADASET(I)
        IF(IVERB > 4) WRITE(IOUT,30)PARNAMALL(I),PTMP(I)
        IF (IFAIL.NE.0) THEN
          CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
          CALL UTL_STOP('EQN_EVALUATE failed evalualting a derived parameter')
        ENDIF
      ENDIF
    ENDDO
    IF(IVERB > 4) WRITE(IOUT,40)
    DEALLOCATE(PTMP)
    RETURN
  END SUBROUTINE UCODE_GEN_PVALALL
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_GEN_DERPARWRITE(IOUT,PSET)
    USE UTILITIES
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                         INTENT(IN) :: IOUT
    DOUBLE PRECISION,                INTENT(IN) :: PSET(NPSNPD)
    ! local variables
    INTEGER :: I
    !Formats
    10 FORMAT(/,80('<'),/)
    40 FORMAT(/,80('>'),/)
    100 FORMAT(/,1X,'VALUES OF DERIVED PARAMETERS:')
    110 FORMAT(/,1X,'THERE ARE NO DERIVED PARAMETERS FOR THIS EVALUATION',/)
    251 FORMAT(1X,'Param. name ',2X,'Value',12X,'Equation',/, &
               ' ------------  ---------------  --------')
    255 FORMAT(1X,A,2X,1PG13.4,4X,A)
    WRITE(IOUT,10)
    IF(NPSNPD > NPS) THEN
      WRITE(IOUT,100)
      WRITE(IOUT,251)
      DO I = NPS+1,NPSNPD
        WRITE(IOUT,255)PARNAMALL(I),PSET(I),TRIM(DERPAREQN(I-NPS))
      ENDDO
    ELSE
      WRITE(IOUT,110)
    ENDIF
    WRITE(IOUT,40)
  END SUBROUTINE UCODE_GEN_DERPARWRITE
!===============================================================================
!===============================================================================
  SUBROUTINE UCODE_CLN(IFO,IOUT,VERSION,IFAILHOLD)
    !   Close data exchange files for residuals and statistics
    USE UTILITIES
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                         INTENT(IN) :: IFO
    INTEGER,                         INTENT(IN) :: IOUT
    DOUBLE PRECISION,                INTENT(IN) :: VERSION
    INTEGER, OPTIONAL,               INTENT(IN) :: IFAILHOLD
    !
    CALL REG_GN_CLN (IFO,IOUT,OPTNLUNC,STDERRONE,IFAILHOLD)
    !
    IF (ALLOCATED(NW)) DEALLOCATE(NW)
    IF (ALLOCATED(CONSTRAIN)) DEALLOCATE(CONSTRAIN)
    IF (ALLOCATED(DERPARNAMELC)) DEALLOCATE(DERPARNAMELC)
    IF (ALLOCATED(DERPAREQN)) DEALLOCATE(DERPAREQN)
    IF (ALLOCATED(PARNAM)) DEALLOCATE(PARNAM)
    IF (ALLOCATED(PARGP)) DEALLOCATE(PARGP)
    IF (ALLOCATED(PINCR)) DEALLOCATE(PINCR)
    IF (ALLOCATED(PVAL)) DEALLOCATE(PVAL)
    IF (ALLOCATED(PVALTMP)) DEALLOCATE(PVALTMP)
    IF (ALLOCATED(PVALMAX)) DEALLOCATE(PVALMAX)
    IF (ALLOCATED(PVALMAX)) DEALLOCATE(PVALMAXC)
    IF (ALLOCATED(PVALMIN)) DEALLOCATE(PVALMIN)
    IF (ALLOCATED(PVALMIN)) DEALLOCATE(PVALMINC)
    IF (ALLOCATED(PVALS)) DEALLOCATE(PVALS)
    IF (ALLOCATED(PADJ)) DEALLOCATE(PADJ)
    IF (ALLOCATED(PASSIGNED)) DEALLOCATE(PASSIGNED)
    IF (ALLOCATED(POMIT)) DEALLOCATE(POMIT)
    IF (ALLOCATED(LN)) DEALLOCATE(LN)
    IF (ALLOCATED(BSCAL)) DEALLOCATE(BSCAL)
    IF (ALLOCATED(PARNAMLC)) DEALLOCATE(PARNAMLC)
    IF (ALLOCATED(ITRANS)) DEALLOCATE(ITRANS)
    IF (ALLOCATED(IPTR)) DEALLOCATE(IPTR)
    IF (ALLOCATED(MODELVAL)) DEALLOCATE(MODELVAL)
    IF (ALLOCATED(RESIDS)) DEALLOCATE(RESIDS)
    IF (ALLOCATED(WTDRESIDS)) DEALLOCATE(WTDRESIDS)
    IF (ALLOCATED(OBSNAM)) DEALLOCATE(OBSNAM)
    IF (ALLOCATED(INSFILESE)) DEALLOCATE(INSFILESE)
    IF (ALLOCATED(MRKDELSE)) DEALLOCATE(MRKDELSE)
    IF (ALLOCATED(MODOUTFILESE)) DEALLOCATE(MODOUTFILESE)
    IF (ALLOCATED(INCLU)) DEALLOCATE(INCLU)
    IF (ALLOCATED(RSQALL)) DEALLOCATE(RSQALL)
    IF (ALLOCATED(RSQD)) DEALLOCATE(RSQD)
    IF (ALLOCATED(XPRI)) DEALLOCATE(XPRI)
    IF (ALLOCATED(PARNAMALL)) DEALLOCATE(PARNAMALL)
    IF (ALLOCATED(IPTR)) DEALLOCATE(IPTR)
    IF (ALLOCATED(MAG_PERTURB)) DEALLOCATE(MAG_PERTURB)
    IF (ALLOCATED(DERPARNAME)) DEALLOCATE(DERPARNAME)
    IF (ALLOCATED(PARNAMLC)) DEALLOCATE(PARNAMLC)
    IF (ALLOCATED(PVALINIT)) DEALLOCATE(PVALINIT)
    IF (ALLOCATED(PERTURB)) DEALLOCATE(PERTURB)
    IF (ALLOCATED(PMAXCHANGE)) DEALLOCATE(PMAXCHANGE)
    IF (ALLOCATED(PTOLPAR)) DEALLOCATE(PTOLPAR)
    IF (ALLOCATED(TRANSFORM)) DEALLOCATE(TRANSFORM)
    IF (ALLOCATED(ISENMETHOD)) DEALLOCATE(ISENMETHOD)
    IF (ALLOCATED(ISENMETHODPRT)) DEALLOCATE(ISENMETHODPRT)
    IF (ALLOCATED(ICONVERT_DERIV)) DEALLOCATE(ICONVERT_DERIV)
    IF (ALLOCATED(XSENST)) DEALLOCATE(XSENST)
    !
! save this code for the time when a GUI will use this info to update screen
!    IF(IUITER > 0)THEN
!      CLOSE(UNIT=IUITER)
!      DO I = 1,ITERP+1
!        INQUIRE(FILE=FNITER(I),EXIST=LEX)
!        IF (LEX) THEN
!          IUITER = UTL_GETUNIT(101,150)
!          OPEN (IUITER,FILE=FNITER(I))
!          CLOSE(IUITER,STATUS='DELETE')
!          LEX = .FALSE.
!        ENDIF
!      ENDDO
!    ENDIF
    !
    RETURN
  END SUBROUTINE UCODE_CLN
!===============================================================================
!===============================================================================
END MODULE UCODEMOD



