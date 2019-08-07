MODULE UCODE_DATA
  USE GLOBAL_DATA
  USE DATATYPES
  IMPLICIT NONE
  SAVE
  PRIVATE
  PUBLIC &
  !   Public data
    CREATEINITFILES, DATAEXCHANGE,  &
    INUNIT, IOUT,  &
    LINADVANS, LINEARITY, LINEARITYADV,  &
    MODELLENGTH, MODELMASS, MODELNAME, MODELTIME, MODELVAL,  &
    NOBS, NPD, NPE, NPS,   &
    OBSNAM, OPTIMIZE, OPTNLUNC,  &
    PADJ, PARNAM, PATHTOMERGEDFILE, PREDICT, PREDICTION, PVAL,  PVALMAX,  &
    SENSITIVITIES, SOSFILE, SOSSURFACEINPUT, STDERRONE,  &
    TAIL, UCODEHEAD, GNUCHEAD, HEADPARDATA, HEADPARGPS, TAILPARGPS, &
    PARGP, &
    GPLOWERVALUE, GPUPPERVALUE, &
    GPLOWERCONSTRAINT, GPUPPERCONSTRAINT, GPPERTURBAMT, GPTOLPAR, &
    GPMAXCHANGE, GPSCALEPVAL, LOWERVALUE, UPPERVALUE, &
    LOWERCONSTRAINT, UPPERCONSTRAINT, PERTURBAMT, TOLPAR, &
    MAXCHANGE, SCALEPVAL, PARDATAGP, STARTVALUE, &
    GPCONSTRAIN, GPADJUSTABLE, GPTRANSFORM, GPNONLINEARINT, &
    GPSENMETHOD, GPSOSINCREMENT, GROUPNAME, CONSTRAIN, ADJUSTABLE, TRANSFORM, &
    NONLINEARINT, SENMETHOD, SOSINCREMENT, NPARCOLS, PARCOL, HEADPARVALDATA, &
    NPVCOLS, PVCOL, NPDCOLS, PDCOL, PDHEAD, DERPARNAME, DERPAREQN, &
    GPSTATISTIC, GPSTATFLAG, GPNONDETECT, GPWTOSCONSTANT, STATISTIC, STATFLAG, &
    NONDETECT, WTOSCONSTANT, DEROBSEQN, LLPTRPRED, NPREDGPS, PREDPLOTSYMBOL, &
    PREDUSEFLAG, PREDNAME, REFVALUE, GROUPNPRED, MEASSTATISTIC, MEASSTATFLAG, &
    DERPREDEQN, NPREDCOLS, PREDCOL, PREDGROUPNAME, LLPTRPREDGP, GPMEASSTATISTIC, &
    GPMEASSTATFLAG, GPREFVALUE, &
    LLPTRPRI, NPRIGPS, PRIPLOTSYMBOL, PRIUSEFLAG, NPRICOLS, PRICOL, PRIORNAME, &
    PRIORINFOVALUE, GROUPNPRI, PRISTATISTIC, PRISTATFLAG, PRIEQN, PRIGROUPNAME, &
    LLPTRPRIGP, GPPRISTATISTIC, GPPRISTATFLAG, GPPRIORINFOVALUE, PRICOVMATNAM, &
    PRIWTMULTIPLIER, GPPRIEQN
  !   
  !   Input-block LLIST pointers
  TYPE (LLIST), POINTER :: GNUCHEAD     ! Ptr to head of linked list for Reg_GN_Controls input block
  TYPE (LLIST), POINTER :: TAIL         ! Ptr to tail of a linked list
  TYPE (LLIST), POINTER :: UCODEHEAD    ! Ptr to head of linked list for UCODE_Control_Data input block
  TYPE (LLIST), POINTER :: HEADPARGPS   ! Pointer to head of list for Parameter_Groups input block
  TYPE (LLIST), POINTER :: TAILPARGPS   ! Pointer to tail of list for Parameter_Groups input block 
  TYPE (LLIST), POINTER :: HEADPARDATA    ! Pointer to head of list for Parameter_Data input block
  TYPE (LLIST), POINTER :: HEADPARVALDATA ! Pointer to head of list for Parameter_Values input block
  TYPE (LLIST), POINTER :: PDHEAD         ! Pointer to head of list for Derived_Parameters input block
  TYPE (LLIST), POINTER :: LLPTRPRED      ! Pointer to head of list for Prediction_Data input block
  TYPE (LLIST), POINTER :: LLPTRPREDGP    ! Pointer to head of list for Prediction_Groups input block
  TYPE (LLIST), POINTER :: LLPTRPRI       ! Pointer to head of list for Linear_Prior_Information input block
  TYPE (LLIST), POINTER :: LLPTRPRIGP     ! Pointer to head of list for Prior_Information_Groups input block
  !
  !   From UCODE main
  CHARACTER(LEN=1),   ALLOCATABLE, DIMENSION(:)            :: CINSTSET  ! holds compressed instruction set
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)            :: DEPVALSETS
  CHARACTER(LEN=200)                                       :: FILENAME    ! Input data file name
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: INSTRUCTFILE
  CHARACTER(LEN=MAX_STRING_LEN)                            :: INSTRUCTION ! extraction direction
  INTEGER                                                  :: INUNIT
  INTEGER                                                  :: IOUT        ! Unit number of main output file
  INTEGER                                                  :: LCIS           ! size of CINSTSET array
  INTEGER,            ALLOCATABLE, DIMENSION(:)            :: LOCINS    ! pointer to instructions
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: MIFILE    ! Names of model-input files
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: MOFILE    ! Names of model-output files
  CHARACTER(LEN=1),   ALLOCATABLE, DIMENSION(:)            :: MRKDL
  INTEGER                                                  :: NINSTRUCT      ! size of LOCINS array
  INTEGER                                                  :: NMIFILE        ! Number of model-input files
  INTEGER                                                  :: NMOFILE        ! Number of model-output files
  CHARACTER(LEN=MAX_STRING_LEN)                            :: OUTNAM      ! root name for output files
  !   From UCODEMOD
  LOGICAL                                            :: CREATEINITFILES
  LOGICAL                                            :: DATAEXCHANGE
  INTEGER, ALLOCATABLE,  DIMENSION(:)                :: IPTR                    ! Position of parameter in full set
  INTEGER, ALLOCATABLE,  DIMENSION(:)                :: ISENMETHOD
  CHARACTER(LEN=6)                                   :: LINADVANS
  LOGICAL                                            :: LINEARITY
  LOGICAL                                            :: LINEARITYADV
  CHARACTER(LEN=12)                                  :: MODELLENGTH
  CHARACTER(LEN=12)                                  :: MODELMASS
  CHARACTER(LEN=12)                                  :: MODELNAME
  CHARACTER(LEN=12)                                  :: MODELTIME
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)       :: MODELVAL                ! Simulated equivalent
  CHARACTER(LEN=180), ALLOCATABLE,  DIMENSION(:)     :: MODOUTFILESE            ! Model output files for simulated equivalents
  CHARACTER(LEN=1), ALLOCATABLE,  DIMENSION(:)       :: MRKDELSE
  INTEGER                                            :: NOBS                    ! Number of observations
  INTEGER                                            :: NPD
  INTEGER                                            :: NPE
  INTEGER                                            :: NPS                     ! Number of parameters
  INTEGER                                            :: NUMOUTFILESE            ! Number of output files for simulated equivalents
  CHARACTER(LEN=LENDNAM), ALLOCATABLE,  DIMENSION(:) :: OBSNAM
  LOGICAL                                            :: OPTIMIZE
  LOGICAL                                            :: OPTNLUNC
  LOGICAL,           ALLOCATABLE,  DIMENSION(:)      :: PADJ                    ! Is parameter adjustable? yes/no
  CHARACTER(LEN=12), ALLOCATABLE,  DIMENSION(:)      :: PARGP                   ! Parameter groups (from Parameter_Groups block)
  CHARACTER(LEN=12), ALLOCATABLE,  DIMENSION(:)      :: PARDATAGP               ! Parameter groups (from Parameter_Data block)
  CHARACTER(LEN=12), ALLOCATABLE,  DIMENSION(:)      :: PARNAM                  ! Parameter names
  CHARACTER(LEN=MAX_STRING_LEN)                      :: PATHTOMERGEDFILE
  INTEGER                                            :: PRECIS
  LOGICAL                                            :: PREDICT
  LOGICAL                                            :: PREDICTION
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)       :: PVAL
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:)       :: PVALMAX
  LOGICAL                                            :: SENSITIVITIES
  INTEGER, ALLOCATABLE, DIMENSION(:)                 :: SKIPLINES
  CHARACTER(LEN=MAX_STRING_LEN)                      :: SOSFILE
  CHARACTER(LEN=4)                                   :: SOSSURFACEINPUT
  LOGICAL                                            :: STDERRONE
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!! BLOCK PARAMETERS !!!!!!!!!!!!!!!!!!!!!!
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
      (/ 'PARAMNAME      ','GROUPNAME      ','STARTVALUE     ', &
         'LOWERVALUE     ','UPPERVALUE     ','CONSTRAIN      ', &
         'LOWERCONSTRAINT','UPPERCONSTRAINT','ADJUSTABLE     ', &
         'PERTURBAMT     ','TRANSFORM      ','TOLPAR         ', &
         'MAXCHANGE      ','SENMETHOD      ','SCALEPVAL      ', &
         'SOSINCREMENT   ','NONLININTERVAL '/)
  !
  !   For PARAMETER DATA FOR PREDICTION input, define default column order
  INTEGER,           PARAMETER                      :: NPARFPCOLS = 17
  CHARACTER(LEN=40), DIMENSION(NPARFPCOLS), TARGET    :: PARFPCOL = &
      (/ 'PARAMNAME      ','GROUPNAME      ','STARTVALUE     ', &
         'LOWERVALUE     ','UPPERVALUE     ','CONSTRAIN      ', &
         'LOWERCONSTRAINT','UPPERCONSTRAINT','ADJUSTABLE     ', &
         'PERTURBAMT     ','TRANSFORM      ','TOLPAR         ', &
         'MAXCHANGE      ','SENMETHOD      ','SCALEPVAL      ', &
         'SOSINCREMENT   ','NONLININTERVAL '/)
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
  ! For Prediction_Data
  INTEGER,           PARAMETER                      :: NPREDCOLS = 6
  CHARACTER(LEN=40), DIMENSION(NPREDCOLS), TARGET    :: PREDCOL = &
      (/ 'PREDNAME       ','REFVALUE       ','MEASSTATISTIC  ', &
         'MEASSTATFLAG   ','GROUPNAME      ','EQUATION       '/)
  ! For Linear_Prior_Information
  INTEGER,           PARAMETER                      :: NPRICOLS = 6
  CHARACTER(LEN=40), DIMENSION(NPREDCOLS), TARGET    :: PRICOL = &
      (/ 'PRIORNAME      ','EQUATION       ','PRIORINFOVALUE ', &
         'STATISTIC      ','STATFLAG       ','GROUPNAME      '/)
  !
  INTEGER :: IDEPTYPEU  ! 1-observations; 2-predictions
  ! ----------------------------------------------------------------------------
  !   Arrays to hold data from Parameter_Groups input block
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: GPLOWERVALUE, GPUPPERVALUE, &
      GPLOWERCONSTRAINT, GPUPPERCONSTRAINT, GPPERTURBAMT, GPTOLPAR, &
      GPMAXCHANGE, GPSCALEPVAL
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: GPCONSTRAIN, GPADJUSTABLE, GPTRANSFORM, &
      GPNONLINEARINT
  INTEGER, ALLOCATABLE, DIMENSION(:) :: GPSENMETHOD, GPSOSINCREMENT
  ! ----------------------------------------------------------------------------  
  !   Arrays to hold data from Parameter_Data input block
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: LOWERVALUE, UPPERVALUE, &
      LOWERCONSTRAINT, UPPERCONSTRAINT, PERTURBAMT, TOLPAR, MAXCHANGE, &
      SCALEPVAL, STARTVALUE
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:) :: GROUPNAME
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CONSTRAIN, ADJUSTABLE, TRANSFORM, &
      NONLINEARINT
  INTEGER, ALLOCATABLE, DIMENSION(:) :: SENMETHOD, SOSINCREMENT
  ! ----------------------------------------------------------------------------  
  !   Arrays to hold data from Derived_Parameters input block
  CHARACTER(LEN=12),             ALLOCATABLE, DIMENSION(:) :: DERPARNAME
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: DERPAREQN
  ! ----------------------------------------------------------------------------  
  !   Arrays to hold data from Observation_Groups input block
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: GPSTATISTIC, GPWTOSCONSTANT
  CHARACTER(LEN=9), ALLOCATABLE, DIMENSION(:) :: GPSTATFLAG
  LOGICAL,          ALLOCATABLE, DIMENSION(:) :: GPNONDETECT
  ! ----------------------------------------------------------------------------  
  !   Arrays to hold data from Observation_Data input block
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: DEROBSEQN
  DOUBLE PRECISION,              ALLOCATABLE, DIMENSION(:) :: STATISTIC, WTOSCONSTANT
  CHARACTER(LEN=9),              ALLOCATABLE, DIMENSION(:) :: STATFLAG
  LOGICAL,                       ALLOCATABLE, DIMENSION(:) :: NONDETECT
  ! ----------------------------------------------------------------------------
  !   Arrays to hold data from Prediction_Groups input block
  INTEGER :: NPREDGPS
  CHARACTER(LEN=12),             ALLOCATABLE, DIMENSION(:) :: PREDGROUPNAME
  LOGICAL,                       ALLOCATABLE, DIMENSION(:) :: PREDUSEFLAG
  INTEGER,                       ALLOCATABLE, DIMENSION(:) :: PREDPLOTSYMBOL
  DOUBLE PRECISION,              ALLOCATABLE, DIMENSION(:) :: GPREFVALUE
  DOUBLE PRECISION,              ALLOCATABLE, DIMENSION(:) :: GPMEASSTATISTIC
  CHARACTER(LEN=9),              ALLOCATABLE, DIMENSION(:) :: GPMEASSTATFLAG
  ! ----------------------------------------------------------------------------  
  !   Arrays to hold data from Prediction_Data input block
  CHARACTER(LEN=12),             ALLOCATABLE, DIMENSION(:) :: PREDNAME
  DOUBLE PRECISION,              ALLOCATABLE, DIMENSION(:) :: REFVALUE
  DOUBLE PRECISION,              ALLOCATABLE, DIMENSION(:) :: MEASSTATISTIC
  CHARACTER(LEN=9),              ALLOCATABLE, DIMENSION(:) :: MEASSTATFLAG
  CHARACTER(LEN=12),             ALLOCATABLE, DIMENSION(:) :: GROUPNPRED
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: DERPREDEQN
  ! ----------------------------------------------------------------------------
  !   Arrays to hold data from Prior_Information_Groups input block
  INTEGER :: NPRIGPS
  CHARACTER(LEN=12),             ALLOCATABLE, DIMENSION(:) :: PRIGROUPNAME
  LOGICAL,                       ALLOCATABLE, DIMENSION(:) :: PRIUSEFLAG
  INTEGER,                       ALLOCATABLE, DIMENSION(:) :: PRIPLOTSYMBOL
  DOUBLE PRECISION,              ALLOCATABLE, DIMENSION(:) :: PRIWTMULTIPLIER
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: GPPRIEQN
  DOUBLE PRECISION,              ALLOCATABLE, DIMENSION(:) :: GPPRIORINFOVALUE
  DOUBLE PRECISION,              ALLOCATABLE, DIMENSION(:) :: GPPRISTATISTIC
  CHARACTER(LEN=9),              ALLOCATABLE, DIMENSION(:) :: GPPRISTATFLAG
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: PRICOVMATNAM
  ! ----------------------------------------------------------------------------  
  !   Arrays to hold data from Linear_Prior_Information input block
  CHARACTER(LEN=12),             ALLOCATABLE, DIMENSION(:) :: PRIORNAME
  DOUBLE PRECISION,              ALLOCATABLE, DIMENSION(:) :: PRIORINFOVALUE
  DOUBLE PRECISION,              ALLOCATABLE, DIMENSION(:) :: PRISTATISTIC
  CHARACTER(LEN=9),              ALLOCATABLE, DIMENSION(:) :: PRISTATFLAG
  CHARACTER(LEN=12),             ALLOCATABLE, DIMENSION(:) :: GROUPNPRI
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: PRIEQN
  ! ----------------------------------------------------------------------------    
END MODULE UCODE_DATA
