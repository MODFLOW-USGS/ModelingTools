MODULE SENSITIVITY_EXAMPLE_MOD
  USE DATATYPES
  USE GLOBAL_DATA, ONLY: LENDNAM, MAX_STRING_LEN
  IMPLICIT NONE
  SAVE
  PRIVATE
  !
  !   Public data
  PUBLIC AVET, BINC, BSCAL, DNPP, IPERTURBMETHOD, IPRINT, IPTR, ISENMETHOD,   &
         LN, MAXRECL, MCVCAT, MCVUSE, MODELVAL, NCATS, NDINC, NNEGT,   &
         NOBS, NOMIT, NOPNT, NPOST, NRUNS, NW, OBSNAM, OUTNAME, PADJ,   &
         PARNAM, PRECISPRO, PVAL, PVALTMP, RESIDS, RSQ, RSQP,   &
         TEMPVAL, WTDRESIDS, WTMAT, WTMATSQR, WTRL, WTVEC
  !
  TYPE (LLIST), POINTER :: LLPTRPARDATA  ! Pointer to head of first list (par. data)
  !
  !   Model-calculated values categories
  INTEGER                            :: NCATS
  PARAMETER (NCATS=1)
  CHARACTER(LEN=6), DIMENSION(NCATS) :: MCVCAT
  LOGICAL, DIMENSION(NCATS)          :: MCVUSE
  DATA MCVCAT/'OBS   '/
  DATA MCVUSE/.FALSE./
  !
  !   Public subprograms
  PUBLIC MAIN_INI, MAIN_INI_OBS, MAIN_INI_PARS, MAIN_WRITEPARS, MAIN_DEF,   &
         MAIN_UEV_BSCALMESS, MAIN_CLN
  !
  INTEGER                            :: MAXRECL ! Max. record length for IOUT
  INTEGER                            :: NOPNT   ! Decimal point protocol
  INTEGER, ALLOCATABLE, DIMENSION(:) :: NW      ! Minimum word length of a parameter
  INTEGER                            :: PRECISPRO  ! Precision protocol
  !
  !   Variables related to a full parameter set (parameters to be estimated or
  !   analyzed, plus parameters that are fixed or not to be analyzed)
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:) :: PARNAM   ! Parameter names
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:) :: PARGP    ! Parameter groups
  DOUBLE PRECISION,  ALLOCATABLE, DIMENSION(:) :: PVAL     ! Current parameter values
  DOUBLE PRECISION,  ALLOCATABLE, DIMENSION(:) :: PVALTMP  ! Parameter temporary values
  DOUBLE PRECISION,  ALLOCATABLE, DIMENSION(:) :: PVALMIN  ! Minimum parameter value
  DOUBLE PRECISION,  ALLOCATABLE, DIMENSION(:) :: PVALMAX  ! Maximum parameter value
  LOGICAL, ALLOCATABLE, DIMENSION(:)           :: PADJ       ! Is parameter adjustable?
  LOGICAL, ALLOCATABLE, DIMENSION(:)           :: PASSIGNED  ! Has PVAL been assigned?
  INTEGER, ALLOCATABLE, DIMENSION(:)           :: LN         ! >0 for log-transformed parameters
  DOUBLE PRECISION,  ALLOCATABLE, DIMENSION(:) :: BSCAL
  INTEGER                                      :: NPARGPCOLSAR, NPARGPS
  CHARACTER (LEN=8)                            :: PRECISION='single' ! Precision protocol
  CHARACTER (LEN=8)                            :: DECPOINT='point'   ! Decimal-point protocol
  !
  !   Variables related to parameter subset (only parameters to be estimated or
  !   analyzed)
  INTEGER, ALLOCATABLE, DIMENSION(:)      :: IPTR ! Position of parameter in full set
  !
  !   For PARAMETER_GROUPS input, do not define a default order
  INTEGER, PARAMETER                      :: NPARGPCOLS = 0
  CHARACTER(LEN=40), DIMENSION(1), TARGET :: PARGPCOL = (/' '/)
  !
  !   For PARAMETER_DATA input, define default column order
  INTEGER, PARAMETER                             :: NPARCOLS = 4
  CHARACTER(LEN=40), DIMENSION(NPARCOLS), TARGET :: PARCOL =    &
      (/ 'PARAMNAME   ','GROUPNAME   ','STARTVALUE  ','ADJUSTABLE  '/)
  !
  !   For PARAMETERVALUES input, define default column order
  INTEGER, PARAMETER                            :: NPVCOLS = 2
  CHARACTER(LEN=40), DIMENSION(NPVCOLS), TARGET :: PVCOL =    &
      (/ 'PARAMNAME   ','STARTVALUE  '/)
  !
  !   Variables related to dependents
  INTEGER               :: NOBS    ! Number of observations
  DOUBLE PRECISION,  ALLOCATABLE, DIMENSION(:) :: MODELVAL      ! Simulated equivalents
  DOUBLE PRECISION,  ALLOCATABLE, DIMENSION(:) :: TEMPVAL       ! Temporary storage for simulated equivalent
  DOUBLE PRECISION,  ALLOCATABLE, DIMENSION(:) :: RESIDS        ! Residuals
  DOUBLE PRECISION,  ALLOCATABLE, DIMENSION(:) :: WTDRESIDS     ! Weighted residuals
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: OBSNAM
  DOUBLE PRECISION,  ALLOCATABLE, DIMENSION(:) :: WTVEC         ! (1/variance)
  TYPE (CDMATRIX) :: WTMAT    !   Weight matrix for all observations
  TYPE (CDMATRIX) :: WTMATSQR !   Square-root of weight matrix for all observations
  !
  !   Variables related to calculation of sensitivities
  INTEGER, ALLOCATABLE, DIMENSION(:)          :: ISENMETHOD
  INTEGER, ALLOCATABLE, DIMENSION(:)          :: IPERTURBMETHOD
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: PERTURBRAT ! Perturbation ratio for each parameter
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: PERTURBMIN ! Minimum (absolute) perturbation for each parameter
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: BINC       ! Perturbation increment for each parameter
  !
  !   Other application variables
  CHARACTER(LEN=MAX_STRING_LEN)               :: OUTNAME
  LOGICAL, DIMENSION(3) :: IPRINT   ! Control of output:
                                    !   IPRINT(1): List of all observations and residuals
                                    !   IPRINT(2): Sum of squared residuals
                                    !   IPRINT(3): Summary statistics
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: DNPP
  INTEGER :: NDINC, NNEGT, NOMIT, NPOST, NRUNS
  DOUBLE PRECISION :: AVET, RSQ, RSQP, WTRL
  !
CONTAINS
  !-----------------------------------------------------------------------------
  SUBROUTINE MAIN_INI(INUNIT,IOUT,NCOVMAT)
    !   Read application-specific data
    USE DATATYPES
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(IN)  :: INUNIT
    INTEGER, INTENT(IN)  :: IOUT
    INTEGER, INTENT(OUT) :: NCOVMAT
    !
    !   Local variables
    TYPE (LLIST), POINTER :: CDHEAD ! Application Control Data
    TYPE (LLIST), POINTER :: TAIL
    CHARACTER(LEN=40), DIMENSION(1) :: COLNAMES
    INTEGER :: IERR, KLCD
    !
    !   Initialize variables
    CALL TYP_NULL(WTMAT)
    CALL TYP_NULL(WTMATSQR)
    NULLIFY(CDHEAD,TAIL)
    IPRINT = .TRUE.
    NCOVMAT = 0
    !
    KLCD = 0
    CALL UTL_READBLOCK(0,'CONTROL_DATA',COLNAMES,INUNIT,IOUT,'*',.TRUE.,   &
                       CDHEAD,TAIL,KLCD)
    IERR = 0
    OUTNAME = ' '
    CALL UTL_FILTER(IERR,CDHEAD,IOUT,'OUTNAME',OUTNAME)
    IF (IERR .NE. 0) CALL UTL_STOP(' ')
    RETURN
  END SUBROUTINE MAIN_INI
  !-----------------------------------------------------------------------------
  SUBROUTINE MAIN_INI_PARS(INUNIT,IOUT,NPT,NPE)
    !   Read parameter-related data
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: AMESSAGE, BIGDOUBLE, IVERB, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(IN)  :: INUNIT
    INTEGER, INTENT(IN)  :: IOUT
    INTEGER, INTENT(OUT) :: NPT       ! Number of parameters
    INTEGER, INTENT(OUT) :: NPE       ! Number of parameters with adjustable=yes
    !
    !   Local variables
    TYPE (LLIST), POINTER :: PGHEAD ! Pointer to head of first list (par. groups)
    TYPE (LLIST), POINTER :: PVHEAD ! Pointer to head of first list (par. values)
    TYPE (LLIST), POINTER :: TAIL
    INTEGER               :: I, IERR, IFAIL, KPE, KPGP, KPV, MORE
    CHARACTER (LEN=10)    :: ATEMP
    CHARACTER(LEN=12)     :: DEFGROUP
    !
    !   Format statements
    !
    100 FORMAT(/,1X,A)
    200 FORMAT(1X,I6,' Error(s) encountered -- STOP EXECUTION (MAININIT)')
    210 FORMAT(1X,'ERROR: BSCAL<0 for parameter: ',A)
    !
    NULLIFY(LLPTRPARDATA)
    NULLIFY(PGHEAD,PVHEAD,TAIL)
    IFAIL=0
    DEFGROUP = 'DefaultPar'
    !
    ATEMP=ADJUSTL(PRECISION)
    CALL UTL_CASETRANS(ATEMP,'lo')
    IF(ATEMP(1:6).EQ.'double')THEN
      PRECISPRO=1
    ELSE
      PRECISPRO=0
    END IF
    ATEMP=ADJUSTL(DECPOINT)
    CALL UTL_CASETRANS(ATEMP,'lo')
    IF(ATEMP(1:7).EQ.'nopoint')THEN
      NOPNT=1
    ELSE
      NOPNT=0
    END IF
    !
    !   Read and store information in PARAMETER_GROUPS block
    KPGP = 0
    CALL UTL_READBLOCK(NPARGPCOLS,'PARAMETER_GROUPS',PARGPCOL,INUNIT,IOUT,   &
        'GROUPNAME',.FALSE.,PGHEAD,TAIL,KPGP)
    IF (IVERB>4) THEN
      !   Write block information (parameter groups) to output file
      WRITE(IOUT,100)'Echo Parameter_Groups input:'
      CALL UTL_WRITEBLOCK(PGHEAD,IOUT)
    ENDIF
    !
    !   Read and store information in PARAMETER_DATA block
    NPT = 0
    CALL UTL_READBLOCK(NPARCOLS,'PARAMETER_DATA',PARCOL,INUNIT,IOUT,   &
        'PARAMNAME',.TRUE.,LLPTRPARDATA,TAIL,NPT)
    IF (IVERB>4) THEN
      !   Write block information (parameters) to output file
      WRITE(IOUT,100)'Echo parameter input before inserting group information:'
      CALL UTL_WRITEBLOCK(LLPTRPARDATA,IOUT)
    ENDIF
    !
    !   Insert Parameter_Group information into Parameter_Data lists
    CALL UTL_GROUPLIST(DEFGROUP,PGHEAD,IOUT,LLPTRPARDATA,KPGP,NPT)
    !
    IF (IVERB>4) THEN
      !   Write block information (parameters) to output file
      WRITE(IOUT,100)'Echo parameter input after inserting group information:'
      CALL UTL_WRITEBLOCK(LLPTRPARDATA,IOUT)

      !   Write block information (Parameter_Groups) to output file
      WRITE(IOUT,100)'Echo Parameter_Groups input after UTL_GROUPLIST:'
      CALL UTL_WRITEBLOCK(PGHEAD,IOUT)
    ENDIF
    !
    !   Read and store information in PARAMETER_VALUES block
    KPV = 0
    CALL UTL_READBLOCK(NPVCOLS,'PARAMETER_VALUES',PVCOL,INUNIT,IOUT,   &
        'PARAMNAME',.FALSE.,PVHEAD,TAIL,KPV)
    IF (IVERB>4) THEN
      !   Write block information (parameters) to output file
      WRITE(IOUT,100)'Echo Parameter_Values block:'
      CALL UTL_WRITEBLOCK(PVHEAD,IOUT)
    ENDIF
    !
    !   Merge PARAMETER_VALUES data with PARAMETER_DATA data
    CALL UTL_MERGELIST('PARAMETER_DATA','PARAMETER_VALUES',LLPTRPARDATA,   &
                       PVHEAD,IOUT)
    IF (IVERB>4) THEN
      !   Write block information (parameters) to output file
      WRITE(IOUT,100)'Echo parameter input after merging with &
                     &Parameter_Values block:'
      CALL UTL_WRITEBLOCK(LLPTRPARDATA,IOUT)
    ENDIF
    !
    !   Allocate arrays for parameter-related data
    ALLOCATE (BSCAL(NPT), ISENMETHOD(NPT), LN(NPT), NW(NPT), PADJ(NPT),   &
              PARGP(NPT), PARNAM(NPT),    &
              PASSIGNED(NPT), PERTURBMIN(NPT), PERTURBRAT(NPT),   &
              PVAL(NPT), PVALMAX(NPT), PVALMIN(NPT), PVALTMP(NPT))
    !
    !   Populate arrays with default values
    BSCAL = TINY(BSCAL) ! Value that will ensure BSCAL is not used in scaling
    ISENMETHOD = 1
    LN = 0
    PADJ = .FALSE.
    PARGP = DEFGROUP
    PARNAM = ' '
    PASSIGNED = .FALSE.
    PVAL = BIGDOUBLE
    PVALMAX = BIGDOUBLE
    PVALMIN = BIGDOUBLE
   !
    !   Filter information in linked list and populate arrays in the PARAMSET
    !   structure
    IERR = 0
    CALL UTL_FILTERLIST(LLPTRPARDATA,IOUT,'PARAMNAME',NPT,IERR,PARNAM,MORE)
    CALL UTL_FILTERLIST(LLPTRPARDATA,IOUT,'GROUPNAME',NPT,IERR,PARGP,MORE)
    CALL UTL_FILTERLIST(LLPTRPARDATA,IOUT,'STARTVALUE',NPT,IERR,PVAL,MORE)
    CALL UTL_FILTERLIST(LLPTRPARDATA,IOUT,'LOWERBOUND',NPT,IERR,PVALMIN,MORE)
    CALL UTL_FILTERLIST(LLPTRPARDATA,IOUT,'UPPERBOUND',NPT,IERR,PVALMAX,MORE)
    CALL UTL_FILTERLIST(LLPTRPARDATA,IOUT,'ADJUSTABLE',NPT,IERR,PADJ,MORE)
    CALL UTL_FILTERLIST(LLPTRPARDATA,IOUT,'LN',NPT,IERR,LN,MORE)
    CALL UTL_FILTERLIST(LLPTRPARDATA,IOUT,'BSCAL',NPT,IERR,BSCAL,MORE)
    CALL UTL_FILTERLIST(LLPTRPARDATA,IOUT,'ISENMETHOD',NPT,IERR,ISENMETHOD,MORE)
    PERTURBRAT = 0.01D0  ! Default value = 1 percent
    CALL UTL_FILTERLIST(LLPTRPARDATA,IOUT,'PERTURBRAT',NPT,IFAIL,PERTURBRAT,MORE)
    PERTURBMIN = 0.0D0   ! Default value
    CALL UTL_FILTERLIST(LLPTRPARDATA,IOUT,'PERTURBMIN',NPT,IFAIL,PERTURBMIN,MORE)
    PERTURBMIN = ABS(PERTURBMIN)
    !
    !   Perform error checking and count number of parameters for which
    !   ADJUSTABLE is true.
    !   Also, assign arrays needed by equation module.
    NPE = 0
    DO I=1,NPT
      IF (PVAL(I) .NE. BIGDOUBLE) PASSIGNED(I)=.TRUE.
      IF (PADJ(I)) THEN
        NPE = NPE + 1
        IF (BSCAL(I).LT.0.0) THEN
          WRITE(IOUT,210) PARNAM(I)
          IERR = IERR+1
        ENDIF
        IF (LN(I).GE.1) THEN
          LN(I) = 1
        ELSEIF (LN(I).LE.0) THEN
          LN(I) = 0
        ENDIF
      ELSE
      ENDIF
    ENDDO
    !
    !   Write parameter information to output file
    WRITE(IOUT,*)' '
    WRITE(IOUT,100)'PARAMETER INFORMATION:'
    CALL MAIN_WRITEPARS(IOUT,NPT)
    CALL UTL_CHECK_NAMES(IOUT,NPT,PARNAM)
    !
    !   Allocate and populate arrays for parameters being estimated or analyzed
    IF (NPE .GT. 0) THEN
      ALLOCATE (IPTR(NPE),BINC(NPE))
      KPE = 0
      DO I=1,NPT
        IF (PADJ(I)) THEN
          KPE = KPE + 1
          IPTR(KPE) = I
        ENDIF
      ENDDO
    ELSE
      IF (IVERB .GT. 0) WRITE(IOUT,300)
      300 FORMAT(/,1X,'*** Warning: No parameters for which ADJUSTABLE="YES"',/)
    ENDIF
    !
    !   Write information for adjustable parameters to output file
    WRITE(IOUT,*)' '
    IF (NPE>0) THEN
      WRITE(IOUT,100)'INFORMATION FOR ADJUSTABLE PARAMETERS:'
      CALL MAIN_WRITEEPARS(IOUT,NPE)
    ENDIF
    IF (IERR > 0) THEN
      WRITE(IOUT,200) IERR
      CALL UTL_STOP(' ')
    ENDIF
    !
    RETURN
  END SUBROUTINE MAIN_INI_PARS
  !-----------------------------------------------------------------------------
  SUBROUTINE MAIN_INI_OBS()
    !   Allocate arrays related to dependents, and populate with defaults
    USE GLOBAL_DATA, ONLY: BIGDOUBLE
    IMPLICIT NONE
    !
    ALLOCATE(DNPP(NOBS,2))
    IF (NOBS>0) THEN
      ALLOCATE(OBSNAM(NOBS),MODELVAL(NOBS),TEMPVAL(NOBS),   &
               WTVEC(NOBS),RESIDS(NOBS),WTDRESIDS(NOBS))
      MCVUSE(1) = .TRUE.  ! Activates extraction of simulated equivalents to observations
      OBSNAM = ' '
      MODELVAL = BIGDOUBLE
      WTVEC = BIGDOUBLE
      TEMPVAL = BIGDOUBLE
    ENDIF
  END SUBROUTINE MAIN_INI_OBS
  !-----------------------------------------------------------------------------
  SUBROUTINE MAIN_WRITEPARS(IOUT,NPT)
    !   Print parameter-related data
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(IN) :: IOUT
    INTEGER, INTENT(IN) :: NPT
    !
    !   Local variables
    INTEGER :: I
    !
    !   Format statements
    200 FORMAT(/,   &
        1X,'Param. name ',4X,'Group',10X,'Value',7X,'Lower bound',  &
        4X,'Upper bound',3X,'Adj.?',2X,'Assigned?',2X,'LN',4X,'BSCAL',/,   &
        1X,12('-'),1X,12('-'),3(2x,13('-')),2X,5('-'),2X,9('-'),2X,'--',   &
        2X,10('-'))
    210 FORMAT(1X,A,1X,A,3(2x,G13.6),4X,L1,8X,L1,6X,I2,2X,G10.3)
    !
    !   Write data for each parameter in the parameter set
    WRITE(IOUT,200)
    DO I=1,NPT
      WRITE(IOUT,210)PARNAM(I),PARGP(I),PVAL(I),PVALMIN(I),    &
          PVALMAX(I),PADJ(I),PASSIGNED(I),LN(I),BSCAL(I)
    ENDDO
    !
    RETURN
  END SUBROUTINE MAIN_WRITEPARS
  !-----------------------------------------------------------------------------
  SUBROUTINE MAIN_DEF(JOBDIM,JOBLEN,NPE,CTRLJOB,IOUT,NEEDSENS,   &
                      CTRLDONE,ICTRL,SENSDONE,KPPL,NUMPPL)
    !   Determine job of current iteration of control loop
    USE GLOBAL_DATA, ONLY: IVERB
    USE BASIC, ONLY: COMPURPOSE, FORSENS, NCOMLINES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                             INTENT(IN)      :: JOBDIM
    INTEGER,                             INTENT(IN)      :: JOBLEN
    INTEGER,                             INTENT(IN)      :: NPE
    CHARACTER(LEN=JOBLEN), DIMENSION(JOBDIM), INTENT(IN) :: CTRLJOB  ! Sequence of jobs required of Control loop
    INTEGER,                             INTENT(IN)      :: IOUT
    LOGICAL,                             INTENT(IN)      :: NEEDSENS
    LOGICAL,                             INTENT(INOUT)   :: CTRLDONE
    INTEGER,                             INTENT(INOUT)   :: ICTRL
    LOGICAL,                             INTENT(INOUT)   :: SENSDONE
    INTEGER,                             INTENT(OUT)     :: KPPL
    INTEGER,                             INTENT(OUT)     :: NUMPPL
    !
    !   Local variables
    CHARACTER(LEN=JOBLEN) :: JOB
    INTEGER :: IPTRKPE, KPE
    LOGICAL :: CHECKNEW
    !
    !   Format statements
    100 FORMAT(/,1X,'Job of current iteration of Control Loop is: ',A)
    140 FORMAT(1X,'NOTE: For parameter "',A,   &
        '", Perturbation increment set to PERTURBMIN value: ',G9.3)
    !
    KPPL = 0
    !
    !   Advance counter ICTRL if appropriate, based on job last iteration
    IF (ICTRL .EQ. 0) THEN
      ICTRL = 1
    ELSE
      JOB = CTRLJOB(ICTRL)
      IF (JOB .EQ. 'FORWARD') THEN
        ICTRL = ICTRL + 1
      ELSEIF (JOB .EQ. 'FORWARD&SENS') THEN
        ICTRL = ICTRL + 1
      ELSEIF (JOB .EQ. 'SENSITIVITY') THEN
        IF (SENSDONE) ICTRL = ICTRL + 1
      ELSEIF (JOB .EQ. 'REPEAT') THEN
        ICTRL = 1
      ELSE
        ICTRL = ICTRL + 1
      ENDIF
    ENDIF
    !
    ! Skip over unsupported and unrequired jobs
    CHECKNEW = .TRUE.
    DO WHILE (CHECKNEW)
      CHECKNEW = .FALSE.
      IF (CTRLJOB(ICTRL)=='FORWARD') THEN
        IF (FORSENS .AND. NEEDSENS) THEN
          ICTRL = ICTRL + 1
          CHECKNEW = .TRUE.
        ENDIF
      ELSEIF (CTRLJOB(ICTRL)=='FORWARD&SENS') THEN
        IF (.NOT. FORSENS .OR. .NOT. NEEDSENS) THEN
          ICTRL = ICTRL + 1
          CHECKNEW = .TRUE.
        ENDIF
      ENDIF
    ENDDO
    !
    JOB = CTRLJOB(ICTRL)
    WRITE(IOUT,100) TRIM(JOB)
    WRITE(*,100) TRIM(JOB)
    IF (JOB=='FORWARD') THEN
      NUMPPL = 1
    ELSEIF (JOB=='FORWARD&SENS') THEN
      NUMPPL = 1
    ELSEIF (JOB=='SENSITIVITY') THEN
      !   Assign perturbation amount for each parameter
      DO KPE=1,NPE
        IPTRKPE = IPTR(KPE)
        BINC(KPE) = PVAL(IPTRKPE)*PERTURBRAT(IPTRKPE)
        IF (ABS(BINC(KPE)) < PERTURBMIN(IPTRKPE)) THEN
          BINC(KPE) = PERTURBMIN(IPTRKPE)
          IF (IVERB>1) WRITE(IOUT,140) TRIM(PARNAM(IPTRKPE)),PERTURBMIN(IPTRKPE)
        ENDIF
      ENDDO
    ELSEIF (JOB=='STOP') THEN
      CTRLDONE = .TRUE.
    ENDIF
    !
    RETURN
    !
  END SUBROUTINE MAIN_DEF
  !-----------------------------------------------------------------------------
  SUBROUTINE MAIN_UEV_BSCALMESS(NPE,NPS,B,BSCAL,IOUT)
    !   Determine if bscal will apply to scaling of any parameters.  If
    !   so, write message and list parameter(s)
    USE GLOBAL_DATA, ONLY: IVERB
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                           INTENT(IN) :: NPE
    INTEGER,                           INTENT(IN) :: NPS
    DOUBLE PRECISION, DIMENSION(NPS),  INTENT(IN) :: B      ! Current parameter values
    DOUBLE PRECISION, DIMENSION(NPS),  INTENT(IN) :: BSCAL
    INTEGER,                           INTENT(IN) :: IOUT
    !
    !   Local variables
    INTEGER :: IIPP, IP, K
    INTEGER, DIMENSION(NPS) :: IBSFLG
    DOUBLE PRECISION :: BB
    !
    !   Format statements
    500 FORMAT(/,   &
        ' FOR SCALING OF THE SENSITIVITIES, B IS REPLACED BY',/,   &
        ' BSCAL (THE ALTERNATE SCALING FACTOR) FOR PARAMETER(S):')
    510 FORMAT(3X,6(2X,A))
    520 FORMAT(1X,'No parameter will use BSCAL for scaling')
    !
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
      IF (IVERB>3) WRITE(IOUT,520)
    ENDIF
    !
    RETURN
  END SUBROUTINE MAIN_UEV_BSCALMESS
  !-----------------------------------------------------------------------------
  SUBROUTINE MAIN_WRITEEPARS(IOUT,NPE)
    !   Print parameter-related data
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(IN) :: IOUT
    INTEGER, INTENT(IN) :: NPE
    !
    !   Local variables
    INTEGER :: I
    !
    200 FORMAT(/,   &
        1X,'Param. name ',4X,'Group',10X,'Value',7X,'Lower bound',  &
        4X,'Upper bound',3X,'Par. no.',2X,'LN',4X,'BSCAL',/     &
        1X,12('-'),1X,12('-'),3(2X,13('-')),2X,8('-'),2X,'--',   &
        2X,10('-'))
    210 FORMAT(1X,A,1X,A,3(2X,G13.6),2X,I6,4X,I2,2X,G10.3)
    !
    !   Write data for the set of parameters for which adjustable=yes
    WRITE(IOUT,200)
    DO I=1,NPE
      WRITE(IOUT,210)PARNAM(IPTR(I)),PARGP(IPTR(I)),PVAL(IPTR(I)),    &
          PVALMIN(IPTR(I)),PVALMAX(IPTR(I)),IPTR(I),LN(I),BSCAL(I)
    ENDDO
    !
    RETURN
  END SUBROUTINE MAIN_WRITEEPARS
  !-----------------------------------------------------------------------------
  SUBROUTINE MAIN_CLN()
    !   Deallocate all arrays, linked lists, and CDMATRIX structures in the
    !   Main module
    USE UTILITIES
    IMPLICIT NONE
    IF (ALLOCATED(NW)) DEALLOCATE(NW)
    IF (ALLOCATED(PARNAM)) DEALLOCATE(PARNAM)
    IF (ALLOCATED(PARGP)) DEALLOCATE(PARGP)
    IF (ALLOCATED(PVAL)) DEALLOCATE(PVAL)
    IF (ALLOCATED(PVALTMP)) DEALLOCATE(PVALTMP)
    IF (ALLOCATED(PVALMIN)) DEALLOCATE(PVALMIN)
    IF (ALLOCATED(PVALMAX)) DEALLOCATE(PVALMAX)
    IF (ALLOCATED(PADJ)) DEALLOCATE(PADJ)
    IF (ALLOCATED(PASSIGNED)) DEALLOCATE(PASSIGNED)
    IF (ALLOCATED(LN)) DEALLOCATE(LN)
    IF (ALLOCATED(BSCAL)) DEALLOCATE(BSCAL)
    IF (ALLOCATED(IPTR)) DEALLOCATE(IPTR)
    IF (ALLOCATED(MODELVAL)) DEALLOCATE(MODELVAL)
    IF (ALLOCATED(TEMPVAL)) DEALLOCATE(TEMPVAL)
    IF (ALLOCATED(RESIDS)) DEALLOCATE(RESIDS)
    IF (ALLOCATED(WTDRESIDS)) DEALLOCATE(WTDRESIDS)
    IF (ALLOCATED(OBSNAM)) DEALLOCATE(OBSNAM)
    IF (ALLOCATED(WTVEC)) DEALLOCATE(WTVEC)
    IF (ALLOCATED(ISENMETHOD)) DEALLOCATE(ISENMETHOD)
    IF (ALLOCATED(IPERTURBMETHOD)) DEALLOCATE(IPERTURBMETHOD)
    IF (ALLOCATED(PERTURBRAT)) DEALLOCATE(PERTURBRAT)
    IF (ALLOCATED(PERTURBMIN)) DEALLOCATE(PERTURBMIN)
    IF (ALLOCATED(BINC)) DEALLOCATE(BINC)
    IF (ALLOCATED(DNPP)) DEALLOCATE(DNPP)
    CALL TYP_DEALLOC(LLPTRPARDATA)
    CALL TYP_DEALLOC(WTMAT)
    CALL TYP_DEALLOC(WTMATSQR)
    RETURN
  END SUBROUTINE MAIN_CLN
  !-----------------------------------------------------------------------------
END MODULE SENSITIVITY_EXAMPLE_MOD
