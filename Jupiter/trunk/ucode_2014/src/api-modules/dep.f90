! This file is part of the JUPITER API, documented in:
!
! Banta, E.R., Poeter, E.P., Doherty, J.E., and Hill, M.C., 2006, 
! JUPITER: Joint Universal Parameter IdenTification and Evaluation 
! of Reliability--An application programming interface (API) for 
! model analysis: U.S. Geological Survey Techniques and Methods, 
! book 6, chap. E1, 268 p.  
! (Also available at http://pubs.usgs.gov/tm/2006/tm6e1/.)
!
! For the latest updates to source code and documentation, go to:
! http://water.usgs.gov/software/JupiterApi/
!
MODULE DEPENDENTS
  USE DATATYPES
  USE GLOBAL_DATA, ONLY: AMESSAGE, BIGDOUBLE, LENDNAM, MAX_STRING_LEN
  IMPLICIT NONE
  SAVE
  PRIVATE
  !
  !   PUBLIC DATA
  !
  PUBLIC :: GROUP, GROUPNAM, GROUPNOBS, IGPTYPE, LLPTRDEP, LLPTRDEPGP,   &
            MODEXTVAL, MODDERVAL, NONDETVAL, NPREDGPS, NTOTOBS, NTOTPRED, &
            NUSEDEP, NUSEOBS, NUSEPRED, OBSDEROBSNUM, OBSEXTNAM, OBSEXTOBSNUM, &
            OBSVAL, STATISTIK, USE_FLAGDEP, USEFLAG, WTCORRELATED, &
            NDEPGPS, WTCOS, WTCOSUSE
  PUBLIC :: LLPTRDEPCOPY, LLPTRPREDCOPY
  !
  CHARACTER(LEN=12),      ALLOCATABLE, DIMENSION(:) :: GROUP         ! Name of observation group
  CHARACTER(LEN=12),      ALLOCATABLE, DIMENSION(:) :: GROUPNAM
  CHARACTER(LEN=12),      ALLOCATABLE, DIMENSION(:) :: GROUPNOBS      ! Group names for used dependents
  INTEGER,                ALLOCATABLE, DIMENSION(:) :: IGPTYPE ! For each group: 0=unused; 1=observations; 2=predictions
  TYPE (LLIST),           POINTER                   :: LLPTRDEP      ! Ptr to list of dependents
  TYPE (LLIST),           POINTER                   :: LLPTRDEPCOPY  ! Ptr to list of dependents (copy)
  TYPE (LLIST),           POINTER                   :: LLPTRPREDCOPY ! Ptr to list of predictions (copy)
  TYPE (LLIST),           POINTER                   :: LLPTRDEPGP    ! Ptr to list dependent groups
  DOUBLE PRECISION,       ALLOCATABLE, DIMENSION(:) :: MODEXTVAL     ! Simulated equivalent
  DOUBLE PRECISION,       ALLOCATABLE, DIMENSION(:) :: MODDERVAL     ! Derived Simulated equivalent
  DOUBLE PRECISION,       ALLOCATABLE, DIMENSION(:) :: NONDETVAL
  INTEGER                                           :: NPREDGPS
  INTEGER                                           :: NTOTOBS = 0   ! Number of observations
  INTEGER                                           :: NTOTPRED      ! Number of predictions
  INTEGER                                           :: NUSEDEP       ! Number of used dependents
  INTEGER                                           :: NUSEOBS       ! Number of used observations
  INTEGER                                           :: NUSEPRED      ! Number of used predictions
  INTEGER,                ALLOCATABLE, DIMENSION(:) :: OBSDEROBSNUM  ! Order number of derived observation
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: OBSEXTNAM     ! Names of extracted observations
  INTEGER,                ALLOCATABLE, DIMENSION(:) :: OBSEXTOBSNUM  ! Order number of extracted observation
  DOUBLE PRECISION,       ALLOCATABLE, DIMENSION(:) :: OBSVAL        ! Observed value
  DOUBLE PRECISION,       ALLOCATABLE, DIMENSION(:) :: STATISTIK     ! Statistics for weight calculation
  LOGICAL,                ALLOCATABLE, DIMENSION(:) :: USE_FLAGDEP   ! (default=true)
  LOGICAL,                ALLOCATABLE, DIMENSION(:) :: USEFLAG       ! (default=TRUE)
  LOGICAL,                ALLOCATABLE, DIMENSION(:) :: WTCORRELATED  ! (default=false)
  DOUBLE PRECISION,       ALLOCATABLE, DIMENSION(:) :: WTCOS         ! (default=0.0)
  CHARACTER(LEN=9),       ALLOCATABLE, DIMENSION(:) :: WTCOSUSE      ! (CONVERTOS)
  !
  !   PUBLIC SUBPROGRAMS
  !
  PUBLIC :: DEP_INI_ALLOC, DEP_INI_NAMCHK, DEP_INI_READ, DEP_INI_STORE,   &
            DEP_EXT_DER, DEP_UEV_DX_READ_OS, DEP_UEV_DX_READ_WS,   &
            DEP_UEV_DX_READ_P, DEP_UEV_DX_READ_PNUM, DEP_UEV_DX_WRITE_OS,  &
            DEP_UEV_DX_WRITE_P, DEP_UEV_DX_WRITE_R, DEP_UEV_DX_WRITE_SS,   &
            DEP_UEV_DX_WRITE_W, DEP_UEV_DX_WRITE_WS, DEP_UEV_DX_WRITE_WW,   &
            DEP_UEV_RESIDUALS, DEP_UEV_WRITEOBSTABLE, DEP_CLN,   &
            DEP_DX_WRITE_GM, DEP_GET_GROUP, DEP_GET_PLOTSYMBOL, &
            DEP_GET_WTMULTIPLIER
  !
  !   PRIVATE DATA
  !
  CHARACTER(LEN=12),             ALLOCATABLE, DIMENSION(:) :: COVMATNAM     ! (Blank if group is not correlated)
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: DERDEPEQN     ! Derived Dependent Equations
  LOGICAL,                       ALLOCATABLE, DIMENSION(:) :: EQUATION_FLAG ! (default=false)
  TYPE (LLIST),           POINTER                          :: LLPTRPRED     ! Ptr to list of predictions
  TYPE (LLIST),           POINTER                          :: LLPTRPREDGP   ! Ptr to list prediction groups
  INTEGER                                                  :: NDEPGPS
  INTEGER                                                  :: NDERDEP       ! Number of derived dependents
  INTEGER                                                  :: NEXTDEP       ! Number of extracted dependents
  INTEGER                                                  :: NOBSGPS
  INTEGER                                                  :: NTOTDEP       ! Number of dependents
  INTEGER,                       ALLOCATABLE, DIMENSION(:) :: NUMINGROUP    ! (always determined from observation list)
  CHARACTER(LEN=LENDNAM),        ALLOCATABLE, DIMENSION(:) :: OBSALLNAM     ! Names of all observations
  CHARACTER(LEN=LENDNAM),        ALLOCATABLE, DIMENSION(:) :: OBSALLNAMLC   ! Lower-case names of all observations
  CHARACTER(LEN=LENDNAM),        ALLOCATABLE, DIMENSION(:) :: OBSDERNAM     ! Names of dervied observations
  INTEGER,                       ALLOCATABLE, DIMENSION(:) :: PLOTSYMBOL    ! (default: program-determined)
  DOUBLE PRECISION,              ALLOCATABLE, DIMENSION(:) :: VARIANCE      ! Variance (uncorrelated)
  DOUBLE PRECISION,              ALLOCATABLE, DIMENSION(:) :: WTMULTIPLIER  ! (multiplies weight; default=1.0)
  !
  INTEGER, PARAMETER                                       :: NOBSCOLS = 6
  CHARACTER(LEN=40),             DIMENSION(NOBSCOLS,2)     :: OBSCOL =    &
      RESHAPE((/'OBSNAME      ','OBSVALUE     ','STATISTIC    ',   &
                'STATFLAG     ','GROUPNAME    ','EQUATION     ',   &
                'PREDNAME     ','REFVALUE     ','MEASSTATISTIC',   &
                'MEASSTATFLAG ','GROUPNAME    ','EQUATION     '/),   &
                (/NOBSCOLS,2/))
  CHARACTER(LEN=40), DIMENSION(0) :: NOCOL
  !
  INTERFACE DEP_INI_NAMCHK
    !   A generic interface for checking that names in a list have no
    !   duplicate names and that "DUM" is not used as a name.
    MODULE PROCEDURE DEP_INI_NAMCHK_DEF
    MODULE PROCEDURE DEP_INI_NAMCHK_SPEC
  END INTERFACE

CONTAINS
  !
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_INI_ALLOC()
    USE GLOBAL_DATA, ONLY: BIGDOUBLE, IVERB, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    ALLOCATE(MODEXTVAL(NEXTDEP),OBSEXTNAM(NEXTDEP),OBSEXTOBSNUM(NEXTDEP))
    MODEXTVAL = 0.D0
    OBSALLNAM = ' '
    OBSALLNAMLC = ' '
    OBSEXTNAM = ' '
    OBSEXTOBSNUM = -1
    ALLOCATE(DERDEPEQN(NDERDEP),MODDERVAL(NDERDEP),   &
             OBSDERNAM(NDERDEP),OBSDEROBSNUM(NDERDEP))
    MODDERVAL = 0.D0
    OBSDERNAM = ' '
    OBSDEROBSNUM = -1
    !
    RETURN
  END SUBROUTINE DEP_INI_ALLOC
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_INI_COUNT(DEFGROUP,IDEPTYPE,IDEP,IOUT,KDEP,NDDEP,NEDEP,NTDEP)
    !   Determine the number of used, derived, extracted, and total dependents
    USE GLOBAL_DATA, ONLY: BIGDOUBLE, IVERB
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=12), INTENT(IN)  :: DEFGROUP
    INTEGER,           INTENT(IN)  :: IDEPTYPE
    INTEGER,           INTENT(IN)  :: IDEP
    INTEGER,           INTENT(IN)  :: IOUT
    INTEGER,           INTENT(OUT) :: KDEP
    INTEGER,           INTENT(OUT) :: NDDEP
    INTEGER,           INTENT(OUT) :: NEDEP
    INTEGER,           INTENT(OUT) :: NTDEP
    !
    !   Local variables
    CHARACTER(LEN=12)  :: DEFGROUPINT
    TYPE (LLIST), POINTER :: PTR
    TYPE (LNODE), POINTER :: PIPTR
    INTEGER :: I, IERR, J
    CHARACTER(LEN=40) :: UPGROUP, UPWORD1
    CHARACTER(LEN=MAX_STRING_LEN) :: EQ
    LOGICAL :: GROUPDEFINE
    !
    !   Format statements
    200 FORMAT(1X,I6,' ERRORS -- STOP EXECUTION (DEP_INI_COUNT)')
    250 FORMAT(1X,'*** Warning: Group not found: "',A,'"')
    300 FORMAT(/,1X,'OBSERVATIONS', &
               /,1X,' Total number of observations read---------',I6, &
               /,1X,' Number of directly extracted observations-',I6, &
               /,1X,' Number of observations to be derived------',I6, &
               /,1X,' Number of observations to be used---------',I6,/)
    310 FORMAT(/,1X,'PREDICTIONS', &
               /,1X,' Total number of predictions read----------',I6, &
               /,1X,' Number of directly extracted predictions--',I6, &
               /,1X,' Number of predictions to be derived-------',I6, &
               /,1X,' Number of predictions to be used----------',I6,/)
    320 FORMAT(/,1X,'OBSERVATIONS AND PREDICTIONS', &
               /,1X,' Total observations and predictions read---',I6, &
               /,1X,' Number of directly extracted items--------',I6, &
               /,1X,' Number of items to be derived-------------',I6, &
               /,1X,' Number of items to be used----------------',I6,/)
    !
    NULLIFY(PTR,PIPTR)
    !   Allocate and initialize arrays related to observations
    ALLOCATE(GROUP(NTOTDEP),DERDEPEQN(NTOTDEP),EQUATION_FLAG(NTOTDEP), &
             OBSALLNAM(NTOTDEP),OBSALLNAMLC(NTOTDEP),USE_FLAGDEP(NTOTDEP))
    GROUP = ' '
    DERDEPEQN = '_'
    EQUATION_FLAG = .FALSE.
    !
    !   Traverse the list of dependents
    DEFGROUPINT = DEFGROUP
    PTR => LLPTRDEP
    I = 0
    IERR = 0
    DEPENDENT: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT DEPENDENT  ! Pointer valid?
      I = I+1
      IF(I > IDEP) DEFGROUPINT = 'DefaultPred'
      PIPTR => PTR%LHEAD
      GROUPDEFINE = .FALSE.
      !   Traverse the entries for this dependent
      INFO: DO
        IF (.NOT. ASSOCIATED(PIPTR)) THEN
          IF (.NOT. GROUPDEFINE) THEN
            GROUP(I) = DEFGROUPINT
            CALL UTL_CASE(DEFGROUPINT,UPGROUP,1)
            FINDGPDEF: DO J=1,NDEPGPS
              CALL UTL_CASE(GROUPNAM(J),UPWORD1,1)
              IF (UPGROUP == UPWORD1) THEN
                NUMINGROUP(J) = NUMINGROUP(J)+1
                USE_FLAGDEP(I) = USEFLAG(J)
                GOTO 40
              ENDIF
            ENDDO FINDGPDEF
          ENDIF
          40 CONTINUE
          EXIT INFO
        ENDIF
        !
        IF (PIPTR%KEYWORD == 'GROUPNAME') THEN
          IF (.NOT. GROUPDEFINE) THEN
            GROUP(I) = PIPTR%VALUE
            GROUPDEFINE = .TRUE.
            !   Increment count for this group, and assign weight multiplier
            !   and flag indicating if weights are correlated
            CALL UTL_CASE(PIPTR%VALUE,UPGROUP,1)
            FINDGP: DO J=1,NDEPGPS
              CALL UTL_CASE(GROUPNAM(J),UPWORD1,1)
              IF (UPGROUP == UPWORD1) THEN
                NUMINGROUP(J) = NUMINGROUP(J)+1
                USE_FLAGDEP(I) = USEFLAG(J)
                GOTO 50
              ENDIF
            ENDDO FINDGP
            IF (IVERB>0) WRITE(IOUT,250) TRIM(PIPTR%VALUE)
          ENDIF
          50 CONTINUE
        ELSEIF (PIPTR%KEYWORD == 'EQUATION') THEN
          CALL UTL_ARR2STRING(PIPTR%NCHAR,PIPTR%STRING,EQ)
          CALL UTL_CASE(EQ,DERDEPEQN(I),-1)
        ENDIF
        PIPTR => PIPTR%NEXTNODE               ! Get pointer to next entry
      ENDDO INFO
      !
      PTR => PTR%NEXTLIST                     ! Get pointer to next dependent
    ENDDO DEPENDENT
    !
    KDEP = 0
    NUSEOBS = 0
    NUSEPRED = 0
    DO I=1,NDEPGPS
      IF(USEFLAG(I)) KDEP = KDEP + NUMINGROUP(I)
      IF(IGPTYPE(I)==1) NUSEOBS = NUSEOBS + NUMINGROUP(I)
      IF(IGPTYPE(I)==2) NUSEPRED = NUSEPRED + NUMINGROUP(I)
    ENDDO
    NDERDEP = 0
    NEXTDEP = 0
    DO I=1,NTOTDEP
      IF(DERDEPEQN(I).EQ. '_') THEN
        NEXTDEP = NEXTDEP + 1
        EQUATION_FLAG(I) = .FALSE.
      ELSE
        NDERDEP = NDERDEP + 1
        EQUATION_FLAG(I) = .TRUE.
      ENDIF
    ENDDO
    !
    !    DEALLOCATE
    IF (ALLOCATED(DERDEPEQN)) DEALLOCATE(DERDEPEQN)
    !
    IF (IERR > 0) THEN
      WRITE (IOUT,200) IERR
      CALL UTL_STOP(' Error in DEP_INI_COUNT ')
    ENDIF
    ! Write counts of dependent types
    IF (IDEPTYPE==1) THEN
      WRITE(IOUT,300) NTOTDEP, NEXTDEP, NDERDEP, KDEP
    ELSEIF (IDEPTYPE==2) THEN
      WRITE(IOUT,310) NTOTDEP, NEXTDEP, NDERDEP, KDEP
    ELSEIF (IDEPTYPE==3) THEN
      WRITE(IOUT,320) NTOTDEP, NEXTDEP, NDERDEP, KDEP
    ENDIF
    NDDEP = NDERDEP
    NEDEP = NEXTDEP
    NTDEP = NTOTDEP
    NUSEDEP = KDEP
    ALLOCATE(GROUPNOBS(NUSEDEP))
    !
    RETURN
  END SUBROUTINE DEP_INI_COUNT
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_INI_FILTERLIST(DEFGROUP,IOUT,NOBS,NEQNPTR,OBSNAM)
    !   Traverse  a linked list and store applicable information in arrays of
    !   dependents data
    USE GLOBAL_DATA, ONLY: BIGDOUBLE, IVERB, LENDNAM
    USE EQUATION, ONLY: EQN_INI_INSTALL
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=12),                       INTENT(IN)    :: DEFGROUP
    INTEGER,                                 INTENT(IN)    :: IOUT
    INTEGER,                                 INTENT(IN)    :: NOBS
    INTEGER,                                 INTENT(IN)    :: NEQNPTR
    CHARACTER(LEN=LENDNAM), DIMENSION(NOBS), INTENT(INOUT) :: OBSNAM
    !
    !   Local variables
    TYPE (LLIST), POINTER :: PTR
    TYPE (LNODE), POINTER :: PIPTR
    INTEGER :: I, II, IERR, IFAIL, J, KERR, NOBSCNT, NDEROBSCNT, NEXTOBSCNT
    CHARACTER(LEN=40) :: UPGROUP, UPWORD1
    CHARACTER(LEN=12) :: GROUPTMP
    CHARACTER(LEN=LENDNAM) :: OBSNAMTMP
    CHARACTER(LEN=6) :: STATFLAG, SF
    CHARACTER(LEN=MAX_STRING_LEN) :: EQ
    DOUBLE PRECISION :: OBSVALTMP, STATISTIC, WTMULT
    CHARACTER(LEN=40) :: NONDETECTTMP
    CHARACTER(LEN=40) :: TMP
    DOUBLE PRECISION ::  WTCOSTMP = 0.D0
    CHARACTER(LEN=9) ::  WTCOSUSETMP = 'DEFAULT'
    LOGICAL :: GROUPDEFINE, WTCORRELATEDTMP
    LOGICAL :: NONDETECTL=.FALSE.
    !DOUBLE PRECISION, DIMENSION(NOBS) :: WTVEC ! (1/variance)
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: WTVEC ! (1/variance)
    !
    !   Format statements
    80 FORMAT(1X,'Error: STATISTIC is undefined for dependent "',A,'"')
    100 FORMAT(1X,'Error: STATFLAG is undefined for dependent "',A,'"')
    200 FORMAT(1X,I6,' ERRORS -- STOP EXECUTION (DEP_INI_FILTERLIST)')
    250 FORMAT(1X,'*** Warning: Group not found: "',A,'"')
    260 FORMAT(1X,'Error: STATISTIC is non-positive for dependent "',A,'"')
    !
    NULLIFY(PTR,PIPTR)
    !   Allocate and initialize arrays related to observations
    ALLOCATE(NONDETVAL(NOBS),OBSVAL(NOBS),STATISTIK(NOBS),VARIANCE(NOBS), &
             WTCOS(NOBS),WTCOSUSE(NOBS),WTCORRELATED(NOBS),WTVEC(NOBS))
    NONDETVAL = 0.D0
    OBSNAM = ' '
    OBSVAL = BIGDOUBLE
    OBSVALTMP = 0.D0
    STATISTIK = BIGDOUBLE
    WTCOS = 0.D0
    WTCOSUSE = 'OBS'
    WTCORRELATED = .FALSE.
    VARIANCE = BIGDOUBLE
    !
    !   Traverse the list of dependents
    PTR => LLPTRDEP
    I = 0
    IERR = 0
    KERR = 0
    NOBSCNT = 0
    NEXTOBSCNT = 0
    NDEROBSCNT = 0
    DEPENDENT: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT DEPENDENT  ! Pointer valid?
      I = I+1
      PIPTR => PTR%LHEAD
      STATFLAG = 'xxxxxx'
      STATISTIC = BIGDOUBLE
      WTMULT = 1.0
      GROUPDEFINE = .FALSE.
      !   Traverse the entries for this dependent
      INFO: DO
        IF (.NOT. ASSOCIATED(PIPTR)) THEN
          IF (.NOT. GROUPDEFINE) THEN
            GROUPTMP = DEFGROUP
            WTCORRELATEDTMP = .FALSE.
            CALL UTL_CASE(DEFGROUP,UPGROUP,1)
            FINDGPDEF: DO J=1,NDEPGPS
              CALL UTL_CASE(GROUPNAM(J),UPWORD1,1)
              IF (UPGROUP == UPWORD1) THEN
                WTMULT = WTMULTIPLIER(J)
                USE_FLAGDEP(I) = USEFLAG(J)
                GOTO 40
              ENDIF
            ENDDO FINDGPDEF
          ENDIF
          40 CONTINUE
          EXIT INFO
        ENDIF
        IF (PIPTR%KEYWORD == 'OBSNAME' .OR. PIPTR%KEYWORD == 'PREDNAME') THEN
          OBSNAMTMP = PIPTR%VALUE
        ELSEIF (PIPTR%KEYWORD == 'GROUPNAME') THEN
          IF (.NOT. GROUPDEFINE) THEN
            GROUPTMP = PIPTR%VALUE
            GROUPDEFINE = .TRUE.
            !
            !   Increment count for this group, and assign weight multiplier
            !   and flag indicating if weights are correlated
            CALL UTL_CASE(PIPTR%VALUE,UPGROUP,1)
            FINDGP: DO J=1,NDEPGPS
              CALL UTL_CASE(GROUPNAM(J),UPWORD1,1)
              IF (UPGROUP == UPWORD1) THEN
                WTMULT = WTMULTIPLIER(J)
                IF (COVMATNAM(J).NE.' ') THEN
                  WTCORRELATEDTMP = .TRUE.
                ELSE
                  WTCORRELATEDTMP = .FALSE.
                ENDIF
                GOTO 50
              ENDIF
            ENDDO FINDGP
            IF (IVERB>0) WRITE(IOUT,250) TRIM(PIPTR%VALUE)
          ENDIF
          50 CONTINUE
        ELSEIF (PIPTR%KEYWORD == 'VALUE' .OR.      &
                PIPTR%KEYWORD == 'OBSVALUE' .OR.   &
                PIPTR%KEYWORD == 'REFVALUE') THEN
          READ(PIPTR%VALUE,*) OBSVALTMP
        ELSEIF (PIPTR%KEYWORD == 'EQUATION') THEN
          CALL UTL_ARR2STRING(PIPTR%NCHAR,PIPTR%STRING,EQ)
        ELSEIF (PIPTR%KEYWORD == 'STATISTIC' .OR.   &
                PIPTR%KEYWORD == 'MEASSTATISTIC') THEN
          READ(PIPTR%VALUE,*) STATISTIC
        ELSEIF (PIPTR%KEYWORD == 'STATFLAG' .OR.   &
                PIPTR%KEYWORD == 'MEASSTATFLAG') THEN
          SF = PIPTR%VALUE
          CALL UTL_CASE(SF,STATFLAG,1)
        ELSEIF (PIPTR%KEYWORD == 'NONDETECT') THEN
          READ(PIPTR%VALUE,*) TMP
          CALL UTL_CASE(TMP,NONDETECTTMP,1)
          IF (NONDETECTTMP == 'YES' .OR. NONDETECTTMP == 'Y' .OR.   &
              NONDETECTTMP == 'TRUE' .OR. NONDETECTTMP == 'T')   &
                  NONDETECTL = .TRUE.
        ELSEIF (PIPTR%KEYWORD == 'WTOSCONSTANT') THEN
          READ(PIPTR%VALUE,*) WTCOSTMP
        ELSEIF (PIPTR%KEYWORD == 'WTOSUSE') THEN
          READ(PIPTR%VALUE,*) WTCOSUSETMP
          CALL UTL_CASETRANS(WTCOSUSETMP,'hi')
          IF( WTCOSUSETMP == 'OBS' .OR.  WTCOSUSETMP == 'CONVERTOS' &
            .OR.  WTCOSUSETMP == 'SIM'.OR.  WTCOSUSETMP == 'NONE') THEN
            WTCOSUSE = WTCOSUSETMP
          ELSE
            CALL UTL_STOP &
            ('IF ASSIGNED, WTOSUSE must be: CONVERTOS, OBS, SIM, or NONE')
          ENDIF
        ENDIF
        PIPTR => PIPTR%NEXTNODE               ! Get pointer to next entry
      ENDDO INFO
      IF(USE_FLAGDEP(I)) THEN
        NOBSCNT = NOBSCNT + 1
        II = NOBSCNT
        OBSNAM(II) = OBSNAMTMP
        OBSVAL(II) = OBSVALTMP
        STATISTIK(II) = STATISTIC
        GROUPNOBS(II) = GROUPTMP
        IF (NONDETECTL) THEN
          NONDETVAL(II) = OBSVALTMP
          NONDETECTL = .FALSE.
        ENDIF 
        IF(STATFLAG .NE. 'CV') THEN
          IF(WTCOSTMP .NE. 0.D0) CALL UTL_STOP &
            ('WEIGHTING WITH NONZERO WTCOSONSTANT REQUIRES StatFlag = CV ')
        ELSE !(STATFLAG == 'CV')
           WTCOS(II) =  WTCOSTMP
          IF(WTCOSUSETMP == 'NONE') THEN
             WTCOSUSE(II) = 'OBS'
             WTCOS(II) = 0.D0
          ELSEIF(WTCOSUSETMP == 'DEFAULT') THEN
             IF(WTCOS(II) > 0.D0) THEN
               WTCOSUSE(II) = 'CONVERTOS'
             ELSE
               WTCOSUSE(II) = 'OBS'
             ENDIF
          ELSE ! must be either OBS CONVERTOS or SIM
             WTCOSUSE(II) =  WTCOSUSETMP
          ENDIF   
           WTCOSUSETMP = 'DEFAULT'
           WTCOSTMP = 0.D0
        ENDIF
        WTCORRELATED(II) = WTCORRELATEDTMP
        !   Calculate Weight and Variance from STATISTIC for this dependent
        IF (.NOT. WTCORRELATED(II)) THEN
          IF (STATISTIC==BIGDOUBLE) THEN
            WRITE(IOUT,80)TRIM(OBSNAM(II))
            IERR = 1
          ENDIF
          IF (STATISTIC .LE. 0.0D0) THEN
            WRITE(IOUT,260)TRIM(OBSNAM(II))
            IERR = 1
          ENDIF
          IF (STATFLAG=='xxxxxx') THEN
            WRITE(IOUT,100)TRIM(OBSNAM(II))
            IERR = 1
          ENDIF
          IF (IERR .NE. 0) CALL UTL_STOP()
          CALL UTL_CASETRANS(STATFLAG,'hi')
          CALL UTL_CALCWT(IOUT,OBSNAM(II),STATFLAG,STATISTIC,    &
                          OBSVAL(II),WTMULT,KERR,WTVEC(II),VARIANCE(II))
        ENDIF
      ENDIF
      IF(EQUATION_FLAG(I)) THEN
        NDEROBSCNT = NDEROBSCNT + 1
        II = NDEROBSCNT
        CALL UTL_CASE(EQ,DERDEPEQN(II),-1)
        CALL UTL_CASE(OBSNAMTMP,OBSDERNAM(II),-1) ! Store names as lowercase
        CALL EQN_INI_INSTALL(IFAIL,II+NEQNPTR,OBSDERNAM(II),DERDEPEQN(II))
        IF (IFAIL.NE.0) THEN
          CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
          CALL UTL_STOP('EQN_INI_INSTALL reported failure')
        ENDIF
        IF(USE_FLAGDEP(I)) OBSDEROBSNUM(II) = NOBSCNT
      ELSE
        NEXTOBSCNT = NEXTOBSCNT + 1
        II = NEXTOBSCNT
        OBSEXTNAM(NEXTOBSCNT) = OBSNAMTMP
        IF(USE_FLAGDEP(I)) OBSEXTOBSNUM(II) = NOBSCNT
      ENDIF
      !
      PTR => PTR%NEXTLIST                     ! Get pointer to next dependent
    ENDDO DEPENDENT
    !
    DO I=1,NEXTDEP
      OBSALLNAM(I) = OBSEXTNAM(I)
    ENDDO
    DO I=1,NDERDEP
      OBSALLNAM(NEXTDEP+I) = OBSDERNAM(I)
    ENDDO
    KERR = KERR + IERR
    IF (KERR > 0) THEN
      WRITE (IOUT,200) KERR
      CALL UTL_STOP(' ')
    ENDIF
    DO I=1,NTOTDEP
      CALL UTL_CASE(OBSALLNAM(I),OBSALLNAMLC(I),-1)
    ENDDO
    !
    DEALLOCATE(WTVEC)
    RETURN
  END SUBROUTINE DEP_INI_FILTERLIST
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_INI_NAMCHK_DEF(IOUT)
    !
    USE GLOBAL_DATA, ONLY: AMESSAGE
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variable
    INTEGER,                INTENT(IN)    :: IOUT
    !
    !   Local variables
    INTEGER :: I
    INTEGER :: IFLAG = 0
    CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: NAMES
    CHARACTER(LEN=LENDNAM)                            :: TEMPNAM
    !
    !   Format statements
    100 FORMAT(//,' OBSERVATIONS OF ALL TYPES MUST HAVE UNIQUE NAMES ',/, &
                  '  (EACH NON-UNIQUE NAME IS LISTED BELOW EACH TIME ',/, &
                  '  IT IS DUPLICATED)')
    200 FORMAT(1X,A)
    !
    ALLOCATE(NAMES(NTOTDEP))
    NAMES = ' '
    ! Fill the names array with the lower case observation names of all types
    DO I=1,NTOTDEP
      TEMPNAM = OBSALLNAM(I)
      CALL UTL_CASE(TEMPNAM,NAMES(I),-1)
    ENDDO
    ! Sort the names array in alpha order
    CALL UTL_SHELLSORT (NTOTDEP,NAMES)
    ! Compare the names and write any that are nonunique
    DO I=1,NTOTDEP-1
      IF(NAMES(I) .EQ. NAMES(I+1)) THEN
        ! Write message first time a nonunique name is found
        IF(IFLAG .EQ. 0) THEN
          IFLAG = 1
          WRITE(*,100)
          WRITE(IOUT,100)
        ENDIF
        ! Write nonunique names
        WRITE(*,200)NAMES(I)
        WRITE(IOUT,200)NAMES(I)
      ENDIF
    ENDDO
    ! Terminate if there are nonunique names
    IF (IFLAG.NE.0) THEN
      WRITE(*,*)
      WRITE(IOUT,*)
      AMESSAGE = ' STOPPING DUE TO NON-UNIQUE OBSERVATION NAMES'
      CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
      CALL UTL_STOP(' ')
    ENDIF
    DEALLOCATE(NAMES)
    RETURN
  END SUBROUTINE DEP_INI_NAMCHK_DEF
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_INI_NAMCHK_SPEC(IOUT,ND1,NAMES1)
    !   Check that list of names in OBSALLNAM array does not contain
    !   duplicate names, and that "DUM" is not used as a name.
    !
    USE GLOBAL_DATA, ONLY: AMESSAGE, LENDNAM
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variable
    INTEGER,                                INTENT(IN) :: IOUT
    INTEGER,                                INTENT(IN) :: ND1
    CHARACTER(LEN=LENDNAM), DIMENSION(ND1), INTENT(IN) :: NAMES1
    !
    !   Local variables
    INTEGER :: I
    INTEGER :: IFLAG = 0, IFLAGDUM = 0
    CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: NAMES
    CHARACTER(LEN=LENDNAM)                            :: TEMPNAM
    !
    !   Format statements
    100 FORMAT(//,' OBSERVATIONS OF ALL TYPES MUST HAVE UNIQUE NAMES ',/, &
                  '  (EACH NON-UNIQUE NAME IS LISTED BELOW EACH TIME ',/, &
                  '  IT IS DUPLICATED)')
    200 FORMAT(1X,A)
    !
    !   Fill the names array with the lower case observation names of all types
    ALLOCATE(NAMES(NTOTDEP))
    NAMES = ' '
    DO I=1,ND1
      TEMPNAM = NAMES1(I)
      CALL UTL_CASE(TEMPNAM,NAMES(I),-1)
      IF (NAMES(I)=='dum') IFLAGDUM = 1
    ENDDO
    !   Sort the names array in alpha order
    CALL UTL_SHELLSORT (NTOTDEP,NAMES)
    !   Compare the names and write any that are nonunique
    DO I=1,NTOTDEP-1
      IF(NAMES(I) .EQ. NAMES(I+1)) THEN
        !   Write message first time a nonunique name is found
        IF(IFLAG .EQ. 0) THEN
          IFLAG = 1
          WRITE(*,100)
          WRITE(IOUT,100)
        ENDIF
        !   Write nonunique names
        WRITE(*,200)NAMES(I)
        WRITE(IOUT,200)NAMES(I)
      ENDIF
    ENDDO
    !   Terminate if there are nonunique names
    IF (IFLAG.NE.0) THEN
      WRITE(*,*)
      WRITE(IOUT,*)
      AMESSAGE = ' STOPPING DUE TO NON-UNIQUE OBSERVATION NAMES'
      CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
      CALL UTL_STOP(' ')
    ENDIF
    !   Terminate if "dum" is used as a name
    IF (IFLAGDUM.NE.0) THEN
      WRITE(*,*)
      WRITE(IOUT,*)
      AMESSAGE = '"DUM" cannot be used as a name'
      CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
      CALL UTL_STOP(' ')
    ENDIF
    DEALLOCATE(NAMES)
    RETURN
  END SUBROUTINE DEP_INI_NAMCHK_SPEC
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_INI_READ(IDEPTYPE,INUNIT,IOUT,NCOVMAT,NOBS,NDDEP,NEDEP,NTDEP, &
                          COPYDEPLISTS)
    !   Read, store, and echo information related to observations and predictions
    USE GLOBAL_DATA, ONLY: AMESSAGE, IVERB
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(IN)    :: IDEPTYPE
    INTEGER, INTENT(IN)    :: INUNIT
    INTEGER, INTENT(IN)    :: IOUT
    INTEGER, INTENT(INOUT) :: NCOVMAT
    INTEGER, INTENT(OUT)   :: NOBS
    INTEGER, INTENT(OUT)   :: NDDEP
    INTEGER, INTENT(OUT)   :: NEDEP
    INTEGER, INTENT(OUT)   :: NTDEP
    LOGICAL, INTENT(IN), OPTIONAL :: COPYDEPLISTS
    !
    !   Local variables
    TYPE (LLIST), POINTER :: TAIL      ! Ptr to tail of a list
    INTEGER :: I, IDEP, IERR, MORE, NGPS
    INTEGER :: IDEPTYPEIN
    CHARACTER(LEN=12) :: DEFGROUP
    CHARACTER(LEN=20), DIMENSION(2) :: GPSBLOCK, DATBLOCK, DERBLOCK
    CHARACTER(LEN=8), DIMENSION(2) :: DEPNAME
    CHARACTER(LEN=80) :: LINEOUT
    LOGICAL :: COPYDEPLISTSLOCAL
    !
    DATA GPSBLOCK/'OBSERVATION_GROUPS  ','PREDICTION_GROUPS   '/
    DATA DATBLOCK/'OBSERVATION_DATA    ','PREDICTION_DATA     '/
    DATA DERBLOCK/'DERIVED_OBSERVATIONS','DERIVED_PREDICTIONS '/
    DATA DEPNAME/'OBSNAME ','PREDNAME'/
    !   Format statements
    100 FORMAT(/,1X,A)
    !
    !   Initialize some variables
    NDEPGPS = 0
    NGPS = 0
    NOBSGPS = 0
    NPREDGPS = 0
    NTOTDEP = 0
    NTOTPRED = 0
    IDEP = 0
    IERR = 0
    COPYDEPLISTSLOCAL = .FALSE.
    IF (PRESENT(COPYDEPLISTS)) COPYDEPLISTSLOCAL = COPYDEPLISTS
    NULLIFY(LLPTRDEP)
    NULLIFY(LLPTRDEPCOPY,LLPTRPREDCOPY)
    NULLIFY(LLPTRDEPGP)
    NULLIFY(LLPTRPRED,LLPTRPREDGP)
    NULLIFY(TAIL)
    IF (IDEPTYPE==1) THEN
      DEFGROUP = 'DefaultObs'
    ELSEIF (IDEPTYPE==2) THEN
      DEFGROUP = 'DefaultPred'
    ELSEIF (IDEPTYPE==3) THEN
      DEFGROUP = 'DefaultObs'
    ELSE
      AMESSAGE = 'Probable programmer error: IDEPTYPE neither 1, 2, nor 3'  &
                 //' in DEP_INI_READ'
      CALL UTL_STOP()
    ENDIF
    !
    !   Read and echo input block OBSERVATION_GROUPS
    !   (or PREDICTION_GROUPS if IDEPTYPE=2)
    IDEPTYPEIN = IDEPTYPE
    IF(IDEPTYPE == 3) IDEPTYPEIN = 1
    CALL UTL_READBLOCK(0,TRIM(GPSBLOCK(IDEPTYPEIN)),NOCOL,INUNIT, &
                       IOUT,'GROUPNAME',.FALSE.,LLPTRDEPGP,TAIL,NGPS)
    IF (NGPS==0) NGPS = 1
    NDEPGPS = NDEPGPS+NGPS
    IF (IDEPTYPEIN==1) THEN
      NOBSGPS = NGPS
    ELSEIF (IDEPTYPEIN==2) THEN
      NPREDGPS = NGPS
    ENDIF
    IF (IVERB>4) THEN
      WRITE(IOUT,100)'Echo '//TRIM(GPSBLOCK(IDEPTYPEIN))//' input:'
      CALL UTL_WRITEBLOCK(LLPTRDEPGP,IOUT)
    ENDIF
    !
    IF(IDEPTYPE < 3) THEN
      !   Allocate and initialize arrays to hold groups data
      ALLOCATE(GROUPNAM(NDEPGPS), NUMINGROUP(NDEPGPS), &
          PLOTSYMBOL(NDEPGPS), WTMULTIPLIER(NDEPGPS),   &
          COVMATNAM(NDEPGPS), USEFLAG(NDEPGPS), IGPTYPE(NDEPGPS))
      !   Assign defaults for groups
      GROUPNAM = DEFGROUP
      NUMINGROUP = 0
      USEFLAG = .TRUE.
      PLOTSYMBOL = 1
      WTMULTIPLIER = 1.0D0
      COVMATNAM = ' '
      IGPTYPE = 0
      !
      !   Populate arrays of groups data using calls to UTL_FILTERLIST
      CALL UTL_FILTERLIST(LLPTRDEPGP,IOUT,'GROUPNAME',NDEPGPS,IERR,GROUPNAM,   &
                          MORE)
      CALL UTL_FILTERLIST(LLPTRDEPGP,IOUT,'USEFLAG',NDEPGPS,IERR,USEFLAG,MORE)
      CALL UTL_FILTERLIST(LLPTRDEPGP,IOUT,'PLOTSYMBOL',NDEPGPS,IERR,   &
                          PLOTSYMBOL,MORE)
      CALL UTL_FILTERLIST(LLPTRDEPGP,IOUT,'WTMULTIPLIER',NDEPGPS,IERR,   &
                          WTMULTIPLIER,MORE)
      CALL UTL_FILTERLIST(LLPTRDEPGP,IOUT,'COVMATRIX',NDEPGPS,IERR,   &
                          COVMATNAM,MORE)
      !
      DO I=1,NDEPGPS
        IF (USEFLAG(I)) IGPTYPE(I) = IDEPTYPEIN
      ENDDO
      !
      !   Write groups information to output file
      IF (IVERB>4) THEN
        IF (IDEPTYPEIN==1) THEN
          LINEOUT='OBSERVATION-GROUP INFORMATION (before inserting group'   &
                  //' information):'
        ELSEIF (IDEPTYPEIN==2) THEN
          LINEOUT='PREDICTION-GROUP INFORMATION (before inserting group'   &
                  //' information):'
        ENDIF
        WRITE(IOUT,100) TRIM(LINEOUT)
        !
        !   Write observation-group info to output file
        CALL DEP_INI_WRITEGROUPS(IOUT)
      ENDIF
    ENDIF
    !
    !   Read and echo input block OBSERVATION_DATA
    !   (or PREDICTION_DATA if IDEPTYPE=2)
    CALL UTL_READBLOCK(NOBSCOLS,TRIM(DATBLOCK(IDEPTYPEIN)),OBSCOL(:,IDEPTYPEIN), &
                       INUNIT,IOUT,TRIM(DEPNAME(IDEPTYPEIN)),.TRUE.,LLPTRDEP, &
                       TAIL,NTOTDEP)
    IDEP = NTOTDEP
    IF (IVERB>4) THEN
      IF (IDEPTYPEIN==1) THEN
        LINEOUT='Echo observation input:'
      ELSEIF (IDEPTYPEIN==2) THEN
        LINEOUT='Echo prediction input:'
      ENDIF
      WRITE(IOUT,100)LINEOUT
      CALL UTL_WRITEBLOCK(LLPTRDEP,IOUT)
    ENDIF
    !
    !   Read and echo input block DERIVED_OBSERVATIONS
    !   (or DERIVED_PREDICTIONS if IDEPTYPE=2)
    CALL UTL_READBLOCK(NOBSCOLS,TRIM(DERBLOCK(IDEPTYPEIN)),OBSCOL(:,IDEPTYPEIN),   &
                       INUNIT,IOUT,TRIM(DEPNAME(IDEPTYPEIN)),.FALSE.,LLPTRDEP,   &
                       TAIL,NTOTDEP)
    IF (COPYDEPLISTSLOCAL) THEN
      ! COPY dependents data to LLPTRDEPCOPY here
      CALL UTL_COPYLIST(LLPTRDEP, LLPTRDEPCOPY)
    ENDIF
    IF (IDEPTYPEIN==1) THEN
      NTOTOBS = NTOTDEP
    ELSEIF (IDEPTYPEIN==2) THEN
      NTOTPRED = NTOTDEP
    ENDIF
    IF (IVERB>4) THEN
      IF (IDEPTYPEIN==1) THEN
        LINEOUT='Echo observation input:'
      ELSEIF (IDEPTYPEIN==2) THEN
        LINEOUT='Echo prediction input:'
      ENDIF
      WRITE(IOUT,100)LINEOUT
      CALL UTL_WRITEBLOCK(LLPTRDEP,IOUT)
    ENDIF
    !
    !   INSERT "GROUPS" INFORMATION INTO "DATA" LISTS AND PRINT
    CALL UTL_GROUPLIST(DEFGROUP,LLPTRDEPGP,IOUT,LLPTRDEP,NDEPGPS,NTOTDEP)
    !
    !   Filter information in linked list to get data needed to populate
    !   dependents arrays, and populate them.
    !   Write block information (observations or predictions) to output file
    IF (IVERB>4) THEN
      IF (IDEPTYPEIN==1) THEN
        LINEOUT='Observations after inserting group information:'
      ELSEIF (IDEPTYPEIN==2) THEN
        LINEOUT='Predictions after inserting group information:'
      ENDIF
      WRITE(IOUT,100)LINEOUT
      CALL UTL_WRITEBLOCK(LLPTRDEP,IOUT)
    ENDIF
    !
    IF(IDEPTYPE < 3) THEN
      !   Count groups for which a variance-covariance matrix is required, and
      !   allocate an array of CDMATRIX structures to hold the matrices.
      DO I=1,NOBSGPS
        IF (COVMATNAM(I).NE.' ') NCOVMAT = NCOVMAT+1
      ENDDO
    ENDIF
    !
    IF(IDEPTYPE == 3) THEN
      ! Read and echo input block PREDICTION_GROUPS
      IDEPTYPEIN = 2
      CALL UTL_READBLOCK(0,TRIM(GPSBLOCK(IDEPTYPEIN)),NOCOL,INUNIT, &
                         IOUT,'GROUPNAME',.FALSE.,LLPTRPREDGP,TAIL,NPREDGPS)
      NDEPGPS = NDEPGPS+NPREDGPS
      IF (IVERB>4) THEN
        WRITE(IOUT,100)'Echo '//TRIM(GPSBLOCK(IDEPTYPEIN))//' input:'
        CALL UTL_WRITEBLOCK(LLPTRPREDGP,IOUT)
      ENDIF
      !
      !   Allocate arrays to hold obs and preds group data
      ALLOCATE(GROUPNAM(NDEPGPS), NUMINGROUP(NDEPGPS), &
          PLOTSYMBOL(NDEPGPS), WTMULTIPLIER(NDEPGPS),   &
          COVMATNAM(NDEPGPS), USEFLAG(NDEPGPS), IGPTYPE(NDEPGPS))
      !   Assign defaults for groups
      GROUPNAM = DEFGROUP
      NUMINGROUP = 0
      USEFLAG = .TRUE.
      PLOTSYMBOL = 1
      WTMULTIPLIER = 1.0D0
      COVMATNAM = ' '
      IGPTYPE = 0
      !
      !   Append list of prediction groups to list of observation groups
      CALL UTL_APPENDLIST(LLPTRDEPGP,LLPTRPREDGP)
      !
      !   Populate arrays of groups data using calls to UTL_FILTERLIST
      CALL UTL_FILTERLIST(LLPTRDEPGP,IOUT,'GROUPNAME',NDEPGPS,IERR,   &
                          GROUPNAM,MORE)
      CALL UTL_FILTERLIST(LLPTRDEPGP,IOUT,'USEFLAG',NDEPGPS,IERR,USEFLAG,MORE)
      CALL UTL_FILTERLIST(LLPTRDEPGP,IOUT,'PLOTSYMBOL',NDEPGPS,IERR,  &
                          PLOTSYMBOL,MORE)
      CALL UTL_FILTERLIST(LLPTRDEPGP,IOUT,'WTMULTIPLIER',NDEPGPS,IERR,   &
                          WTMULTIPLIER,MORE)
      CALL UTL_FILTERLIST(LLPTRDEPGP,IOUT,'COVMATRIX',NDEPGPS,IERR,   &
                          COVMATNAM,MORE)
      !
      IF (NOBSGPS>0) THEN
        DO I=1,NOBSGPS
          IF (USEFLAG(I)) IGPTYPE(I) = 1
        ENDDO
      ENDIF
      IF (NPREDGPS>0) THEN
        DO I=NOBSGPS+1,NOBSGPS+NPREDGPS
          IF (USEFLAG(I)) IGPTYPE(I) = 2
        ENDDO
      ENDIF
      !
      !   Read and echo input block PREDICTION_DATA
      CALL UTL_READBLOCK(NOBSCOLS,TRIM(DATBLOCK(IDEPTYPEIN)), &
                         OBSCOL(:,IDEPTYPEIN),INUNIT,IOUT,   &
                         TRIM(DEPNAME(IDEPTYPEIN)),.FALSE.,LLPTRPRED, &
                         TAIL,NTOTPRED)
      NTOTDEP = NTOTDEP+NTOTPRED
      IF (IVERB>4) THEN
        LINEOUT='Echo prediction input:'
        WRITE(IOUT,100)LINEOUT
        CALL UTL_WRITEBLOCK(LLPTRPRED,IOUT)
      ENDIF
      !
      !   Read and echo input block DERIVED_PREDICTIONS
      CALL UTL_READBLOCK(NOBSCOLS,TRIM(DERBLOCK(IDEPTYPEIN)),   &
                         OBSCOL(:,IDEPTYPEIN),INUNIT,IOUT,   &
                         TRIM(DEPNAME(IDEPTYPEIN)),.FALSE.,LLPTRPRED,   &
                         TAIL,NTOTPRED)
      IF (COPYDEPLISTSLOCAL) THEN
        CALL UTL_COPYLIST(LLPTRPRED,LLPTRPREDCOPY)
      ENDIF
      IF (IVERB>4) THEN
        LINEOUT='Echo prediction input:'
        WRITE(IOUT,100)LINEOUT
        CALL UTL_WRITEBLOCK(LLPTRPRED,IOUT)
      ENDIF
      !
      !   INSERT "GROUPS" INFORMATION INTO "DATA" LISTS AND PRINT
      IF(NPREDGPS > 0) THEN
!        DEFGROUP = 'DefaultDep'
        CALL UTL_GROUPLIST(DEFGROUP,LLPTRPREDGP,IOUT,LLPTRPRED,NPREDGPS,   &
                           NTOTPRED)
        !   Write block information (observations or predictions) to output file
        IF (IVERB>4) THEN
          LINEOUT='Predictions after inserting group information:'
          WRITE(IOUT,100)LINEOUT
          CALL UTL_WRITEBLOCK(LLPTRPRED,IOUT)
        ENDIF
      ENDIF
      !
      !   Merge lists of data for obs and preds
      CALL UTL_APPENDLIST(LLPTRDEP,LLPTRPRED)
      !
      !   Count groups for which a variance-covariance matrix is required, and
      !   allocate an array of CDMATRIX structures to hold the matrices.
      DO I=1,NDEPGPS
        IF (COVMATNAM(I).NE.' ') NCOVMAT = NCOVMAT+1
      ENDDO
    ENDIF
    !   Determine which dependents are used, which extracted, which derived
    !   Do this is such a way that the user can choose whether to put derived
    !   observations in the same block as observation_data
    CALL DEP_INI_COUNT(DEFGROUP,IDEPTYPE,IDEP,IOUT,NOBS,NDDEP,NEDEP,NTDEP)
    !
    !   Write groups information to output file
    IF (NDEPGPS>0 .AND. IVERB>4) THEN
      LINEOUT='DEPENDENT-GROUP INFORMATION:'
      WRITE(IOUT,100) TRIM(LINEOUT)
      !
      !   Write DEPENDENT-group info to output file
      CALL DEP_INI_WRITEGROUPS(IOUT)
      WRITE(IOUT,'(1X)')
    ENDIF
    !
    RETURN
  END SUBROUTINE DEP_INI_READ
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_INI_STORE(IDEPTYPE,IOUT,NCOVMAT,NOBS,COVMATARR,NEQNPTR, &
                           OBSNAM,DTLA,WTMAT,WTMATSQR)
    !   Read, store, and echo observation-related information.  This subroutine
    !   reads and stores group-covariance matrices.
    USE GLOBAL_DATA, ONLY: AMESSAGE, IVERB
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN)    :: IDEPTYPE ! 1:Obs; 2:Preds
    INTEGER,                                 INTENT(IN)    :: IOUT
    INTEGER,                                 INTENT(IN)    :: NCOVMAT
    INTEGER,                                 INTENT(IN)    :: NOBS
    TYPE (CDMATRIX),     DIMENSION(NCOVMAT), INTENT(IN)    :: COVMATARR
    INTEGER,                                 INTENT(IN)    :: NEQNPTR
    CHARACTER(LEN=LENDNAM), DIMENSION(NOBS), INTENT(INOUT) :: OBSNAM
    DOUBLE PRECISION,                        INTENT(OUT)   :: DTLA
    TYPE (CDMATRIX),                         INTENT(OUT)   :: WTMAT    ! Weight matrix
    TYPE (CDMATRIX),                         INTENT(OUT)   :: WTMATSQR ! Square-root of weight matrix
    !
    !   Local variables
    INTEGER :: I, IFAIL
    CHARACTER(LEN=12) :: DEFGROUP
    CHARACTER(LEN=24), DIMENSION(3) :: INFOHEADER
    CHARACTER(LEN=80) :: LINEOUT
    LOGICAL           :: LCOR
    !
    DATA INFOHEADER/'OBSERVATION INFORMATION:','PREDICTION INFORMATION: ',   &
                    'DEPENDENT INFORMATION:  '/
    !
    !   Format statements
    100 FORMAT(/,1X,A)
    !
    IFAIL = 0
    IF (IDEPTYPE==1) THEN
      DEFGROUP = 'DefaultObs'
    ELSEIF (IDEPTYPE==2) THEN
      DEFGROUP = 'DefaultPred'
    ELSEIF (IDEPTYPE==3) THEN
      DEFGROUP = 'DefaultObs'
    ELSE
      AMESSAGE = 'Probable programmer error: IDEPTYPE neither 1, 2, nor 3'  &
                 //' in DEP_INI_STORE'
      CALL UTL_STOP()
    ENDIF
    !   Allocate and populate arrays related to observations
    CALL DEP_INI_FILTERLIST(DEFGROUP,IOUT,NOBS,NEQNPTR,OBSNAM)
    !
    !   Build variance-covariance matrix for all observations
    CALL UTL_COVMAT(IOUT,NCOVMAT,NOBS,NOBSGPS,COVMATARR,COVMATNAM,GROUPNOBS,   &
                    GROUPNAM,OBSNAM,VARIANCE,WTCORRELATED,WTMAT)
    !   Invert variance-covariance matrix to create weight matrix
    CALL TYP_NULL(WTMATSQR)
    WTMATSQR%ARRAYNAME = ' '
    LCOR = .FALSE.
    CHECKCORR: DO I =1,NOBS
      IF (WTCORRELATED(I)) THEN
        LCOR = .TRUE.
        EXIT CHECKCORR
      ENDIF
    ENDDO CHECKCORR
    IF (LCOR) THEN
      !   Observation errors are correlated
      CALL UTL_SVD(IFAIL,WTMAT,WTMATSQR,DTLA)
    ELSE
      !   Observation errors are not correlated
      CALL UTL_INVERT_DIAG(IFAIL,WTMAT,WTMATSQR,DTLA)
    ENDIF
    IF (IFAIL > 0) THEN
      AMESSAGE = &
      'ERROR: Failed in calculation of log determinant of'// &
      ' variance-covariance matrix for dependents; matrix may not be invertible'
      CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
      CALL UTL_STOP(' ')
    ENDIF
    !
    !   Write observation info to output file
    IF (IVERB>2) THEN
      WRITE(IOUT,*)' '
      WRITE(IOUT,100) INFOHEADER(IDEPTYPE)
      CALL DEP_INI_WRITEDEPS(IOUT,IDEPTYPE,NOBS,OBSNAM,WTMAT)
    ENDIF
    CALL UTL_CHECK_NAMES(IOUT,NOBS,OBSNAM)
    !
    !   Write block information (observation groups) to output file
    IF (IVERB>2) THEN
      WRITE(IOUT,*)' '
      IF (IDEPTYPE==1) THEN
        LINEOUT='OBSERVATION-GROUP INFORMATION:'
      ELSEIF (IDEPTYPE==2) THEN
        LINEOUT='PREDICTION-GROUP INFORMATION:'
      ELSEIF (IDEPTYPE==3) THEN
        LINEOUT='DEPENDENT-GROUP INFORMATION:'
      ENDIF
      WRITE(IOUT,100)TRIM(LINEOUT)
      !
      !   Write observation-group info to output file
      CALL DEP_INI_WRITEGROUPS(IOUT)
    ENDIF
    !
    !   Deallocate linked lists here (unless needed by another module)
    IF (ALLOCATED(USE_FLAGDEP)) DEALLOCATE(USE_FLAGDEP)
    IF (ALLOCATED(EQUATION_FLAG)) DEALLOCATE(EQUATION_FLAG)
    RETURN
  END SUBROUTINE DEP_INI_STORE
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_INI_WRITEDEPS(IOUT,ITYPE,NDEP,OBSNAM,WTMAT)
    !   Write a dependents set as a table
    !   Use ITYPE=1 for observations
    !   Use ITYPE=2 for predictions
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: BLANKS, HYPHENS
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: IOUT
    INTEGER,                                 INTENT(IN) :: ITYPE
    INTEGER,                                 INTENT(IN) :: NDEP
    CHARACTER(LEN=LENDNAM), DIMENSION(NDEP), INTENT(IN) :: OBSNAM
    TYPE (CDMATRIX),                         INTENT(IN) :: WTMAT ! Weight matrix
    ! ITYPE=1 for observations, 2 for predictions, other values as needed
    !
    !   Local variables
    INTEGER, PARAMETER :: NTYPES=3
    INTEGER :: I
    CHARACTER(LEN=16), DIMENSION(NTYPES) :: DEPTYPE
    CHARACTER(LEN=10), DIMENSION(NTYPES) :: REFTYPE
    DATA DEPTYPE/'OBSERVATION NAME','PREDICTION NAME ','DEPENDENT NAME  '/
    DATA REFTYPE/'OBS. VALUE','REF. VALUE','REF. VALUE'/
    !
    !   Format statements
    150 FORMAT(/,1X,'*** Programmer ERROR: ITYPE > NTYPES in call',   &
        ' to DEP_INI_WRITEDEPS')
    200 FORMAT(/,1X,A,A,5X,'GROUP',7X,A,6X,'WEIGHT',/,    &
        1X,A,2X,12('-'),2X,12('-'),2X,12('-'))
    201 FORMAT(/,1X,A,A,5X,'GROUP',7X,A,5X,'VARIANCE',/,    &
        1X,A,2X,12('-'),2X,12('-'),2X,12('-'))
    210 FORMAT(1X,A,2X,A,2X,G12.5,2X,G12.5)
    220 FORMAT(1X,A,2X,A,2X,G12.5,5X,'matrix')
    !
    !   Write data for each dependent in the set
    IF (ITYPE > NTYPES) THEN
      WRITE(IOUT,150)
      CALL UTL_STOP(' ')
    ENDIF
    IF(ITYPE == 1) THEN
      WRITE(IOUT,200) DEPTYPE(ITYPE),BLANKS(1:LENDNAM-16),REFTYPE(ITYPE),   &
                    HYPHENS(1:LENDNAM)
      DO I=1,NDEP
        IF (.NOT. WTCORRELATED(I)) THEN
          WRITE(IOUT,210)OBSNAM(I),GROUPNOBS(I),OBSVAL(I),UTL_GETVAL(WTMAT,I,I)
        ELSE
          WRITE(IOUT,220)OBSNAM(I),GROUPNOBS(I),OBSVAL(I)
        ENDIF
      ENDDO
    ELSE
      WRITE(IOUT,201) DEPTYPE(ITYPE),BLANKS(1:LENDNAM-16),REFTYPE(ITYPE),   &
                    HYPHENS(1:LENDNAM)
      DO I=1,NDEP
        WRITE(IOUT,210)OBSNAM(I),GROUPNOBS(I),OBSVAL(I),1.D0/UTL_GETVAL(WTMAT,I,I)
      ENDDO
    ENDIF
    !
    RETURN
  END SUBROUTINE DEP_INI_WRITEDEPS
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_INI_WRITEGROUPS(IOUT)
    !   Write group data as a table
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(IN) :: IOUT
    !
    !   Local variables
    INTEGER :: I
    CHARACTER(LEN=6),DIMENSION(0:2) :: DTYP
    DATA DTYP/'UNUSED','OBS   ','PRED  '/
    !
    !   Format statements
    200 FORMAT(/,   &
        15X,'NUM. OF',9X,'PLOT',5X,'WEIGHT',4X,'COV. MATRIX',5X,'DEP.',/,    &
        1X,'GROUP NAME',4X,'MEMBERS',2X,'USE?',2X,'SYMBOL',2X,   &
        'MULTIPLIER',2X,'IF CORRELATED',3X,'TYPE',/,1X,12('-'),    &
        2X,7('-'),2X,4('-'),2X,6('-'),2X,10('-'),2X,13('-'),2X,6('-'))
    210 FORMAT(1X,A,2X,I7,3X,L1,4X,I6,2X,G10.3,2X,A,3X,A)
    !
    !   Write data for each group
    WRITE(IOUT,200)
    DO I=1,NDEPGPS
      WRITE(IOUT,210) GROUPNAM(I),NUMINGROUP(I),USEFLAG(I),PLOTSYMBOL(I),   &
                      WTMULTIPLIER(I),COVMATNAM(I),DTYP(IGPTYPE(I))
    ENDDO
    !
    RETURN
  END SUBROUTINE DEP_INI_WRITEGROUPS
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_EXT_DER(IOUT,NDDEP,NEDEP,NEQNPTR,DEREXTVAL,DEPDERVAL)
    !   Calculate derived observations
    USE EQUATION, ONLY: EQN_EVALUATE
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                             INTENT(IN)  :: IOUT
    INTEGER,                             INTENT(IN)  :: NDDEP
    INTEGER,                             INTENT(IN)  :: NEDEP
    INTEGER,                             INTENT(IN)  :: NEQNPTR
    DOUBLE PRECISION,  DIMENSION(NEDEP), INTENT(IN)  :: DEREXTVAL
    DOUBLE PRECISION,  DIMENSION(NDDEP), INTENT(OUT) :: DEPDERVAL
    !
    !   Local variables
    !
    LOGICAL :: EQNLVAL
    INTEGER :: I
    INTEGER :: II
    INTEGER :: IFAIL
    INTEGER :: ITYP
    !DOUBLE PRECISION, DIMENSION(NTOTDEP) :: OBSALLVAL
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: OBSALLVAL
    !
    ALLOCATE(OBSALLVAL(NTOTDEP))
    DO I=1,NEDEP
      OBSALLVAL(I) = DEREXTVAL(I)
    ENDDO
    DO I=1,NDDEP
      II = NEQNPTR + I
      CALL EQN_EVALUATE(IFAIL,II,NTOTDEP,OBSALLNAMLC,OBSALLVAL,ITYP, &
                        DEPDERVAL(I),EQNLVAL)
      IF (IFAIL.NE.0) THEN
        CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
        CALL UTL_STOP('EQN_EVALUATE failed while evaluating derived dependent')
      ENDIF
      OBSALLVAL(NEXTDEP+I)=DEPDERVAL(I)
    ENDDO
    DEALLOCATE(OBSALLVAL)
    RETURN
  END SUBROUTINE DEP_EXT_DER
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_UEV_DX_READ_OS(NTOT,OUTNAM,SIMEQV,OBSVAL,PLOTSYM,OBSNAME)
    !     READ_OS DX FILE simulated equivalents and observed values
    USE GLOBAL_DATA, ONLY: LENDNAM, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                    INTENT(IN)  :: NTOT
    CHARACTER(LEN=MAX_STRING_LEN),              INTENT(IN)  :: OUTNAM
    DOUBLE PRECISION, DIMENSION(NTOT),          INTENT(OUT) :: SIMEQV
    DOUBLE PRECISION, DIMENSION(NTOT),          INTENT(OUT) :: OBSVAL
    INTEGER, DIMENSION(NTOT),                   INTENT(OUT) :: PLOTSYM
    CHARACTER(LEN=LENDNAM), DIMENSION(NTOT),    INTENT(OUT) :: OBSNAME
    !
    !   Local variables
    INTEGER                                                :: I
    INTEGER                                                :: IUOS
    !
    IUOS = UTL_DX_OPEN(OUTNAM,'_os','OLD')
    ! READ Header
    READ (IUOS,*,END=100)
    DO I=1,NTOT
      READ (IUOS,*,END=100)SIMEQV(I),OBSVAL(I),PLOTSYM(I),OBSNAME(I)
    ENDDO
    IUOS = UTL_DX_CLOSE('_os')
    !
    RETURN
    100 IUOS = UTL_DX_CLOSE('_os')
    CALL UTL_STOP('UNEXPECTED CONTENT _os file, CHECK VERSION/RE-CREATE _os')
    RETURN
  END SUBROUTINE DEP_UEV_DX_READ_OS
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_UEV_DX_READ_P(OUTNAM,NPRED,PREDGRP,PREDNAM,PREDVAL,   &
                               PREDVAR,PLOTSYMBOL)
    !   READ _p Data-Exchange Files: Prediction
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN, LENDNAM
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=MAX_STRING_LEN),            INTENT(IN)  :: OUTNAM
    INTEGER,                                  INTENT(IN)  :: NPRED
    CHARACTER(LEN=12), DIMENSION(NPRED),      INTENT(OUT) :: PREDGRP
    CHARACTER(LEN=LENDNAM), DIMENSION(NPRED), INTENT(OUT) :: PREDNAM
    DOUBLE PRECISION, DIMENSION(NPRED),       INTENT(OUT) :: PREDVAL
    DOUBLE PRECISION, DIMENSION(NPRED),       INTENT(OUT) :: PREDVAR
    INTEGER, DIMENSION(NPRED),                INTENT(OUT) :: PLOTSYMBOL
    !
    !   Local variables
    INTEGER                 :: I
    INTEGER                 :: IUP
    !
    !   Formats
    220 FORMAT(/,1X,'ERROR: Cannot open file "',A,'._p"')
    !
    ! read  predictions
    IUP = UTL_DX_OPEN(OUTNAM,'_p','OLD')
    IF(IUP < 1) THEN
      WRITE(*,220)TRIM(OUTNAM)
      CALL UTL_STOP()
    ENDIF
    READ(IUP,*)
    DO I = 1,NPRED
      READ(IUP,*)PREDVAL(I),PLOTSYMBOL(I),PREDNAM(I),PREDVAR(I),PREDGRP(I)
    ENDDO
    ! Close data exchange file
    IUP = UTL_DX_CLOSE('_p')
    RETURN
  END SUBROUTINE DEP_UEV_DX_READ_P
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_UEV_DX_READ_PNUM(IFAIL,OUTNAM,NPRED)
    !   Read _p Data-Exchange file to get number of predictions
    USE GLOBAL_DATA, ONLY: AMESSAGE
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                       INTENT(OUT) :: IFAIL
    CHARACTER(LEN=MAX_STRING_LEN), INTENT(IN)  :: OUTNAM
    INTEGER,                       INTENT(OUT) :: NPRED
    !
    !   Local variables
    CHARACTER(LEN=10)       :: CHECK
    INTEGER                 :: IUP
    ! Formats
    220 FORMAT(/,1X,'Warning: Cannot open file "',A,'._p"')
    !
    ! Open data exchange file
    IFAIL = 0
    IUP = UTL_DX_OPEN(OUTNAM,'_p','OLD')
    IF(IUP < 1) THEN
      WRITE(*,220) TRIM(OUTNAM)
      IFAIL = 1
    ENDIF
    READ(IUP,*)
    NPRED = 0
    DO
      READ(IUP,*,END=100)CHECK
      NPRED = NPRED + 1
    ENDDO
    100 CONTINUE
    IUP = UTL_DX_CLOSE('_p')
    RETURN
  END SUBROUTINE DEP_UEV_DX_READ_PNUM
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_UEV_DX_READ_WS(NTOT,OUTNAM,SIMEQV,WTRES,PLOTSYM,OBSNAME)
    !    READ_WS DX FILE simulated equivalents and weighted residuals
    USE GLOBAL_DATA, ONLY: LENDNAM, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                    INTENT(IN)  :: NTOT
    CHARACTER(LEN=MAX_STRING_LEN),              INTENT(IN)  :: OUTNAM
    DOUBLE PRECISION, DIMENSION(NTOT),          INTENT(OUT) :: SIMEQV
    DOUBLE PRECISION, DIMENSION(NTOT),          INTENT(OUT) :: WTRES
    INTEGER, DIMENSION(NTOT),                   INTENT(OUT) :: PLOTSYM
    CHARACTER(LEN=LENDNAM), DIMENSION(NTOT),    INTENT(OUT) :: OBSNAME
    !
    !   Local variables
    INTEGER                                                :: I
    INTEGER                                                :: IUWS
    !
    IUWS = UTL_DX_OPEN(OUTNAM,'_ws','OLD')
    ! READ Header
    READ (IUWS,*,END=100)
    DO I=1,NTOT
      READ (IUWS,*,END=100)SIMEQV(I),WTRES(I),PLOTSYM(I),OBSNAME(I)
    ENDDO
    IUWS = UTL_DX_CLOSE('_ws')
    !
    RETURN
    100 IUWS = UTL_DX_CLOSE('_ws')
    CALL UTL_STOP('UNEXPECTED CONTENT _ws file, CHECK VERSION/RE-CREATE _ws')
    RETURN
  END SUBROUTINE DEP_UEV_DX_READ_WS
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_UEV_DX_WRITE_OS(IUOS,NOBS,MODELVAL,OBSNAM,OBSVAL,   &
                                 WRITEHEADER,IPLOTOPT)
    !   Write _os Data-Exchange file: Simulated and observed values
    USE GLOBAL_DATA, ONLY: AMESSAGE, BIGREAL, LENDNAM, IVERB, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: IUOS
    INTEGER,                                 INTENT(IN) :: NOBS
    DOUBLE PRECISION,       DIMENSION(NOBS), INTENT(IN) :: MODELVAL
    CHARACTER(LEN=LENDNAM), DIMENSION(NOBS), INTENT(IN) :: OBSNAM
    DOUBLE PRECISION,       DIMENSION(NOBS), INTENT(IN) :: OBSVAL
    LOGICAL,                                 INTENT(IN) :: WRITEHEADER
    INTEGER, OPTIONAL,      DIMENSION(NOBS), INTENT(IN) :: IPLOTOPT
    !
    !   Local variables
    INTEGER :: IPLOT, N !, NMNOBS
    CHARACTER(LEN=12) :: GPNAM
    !
    !   Format statements
    520 FORMAT(G22.15,3X,G22.15,11X,I5,9X,A)
    600 FORMAT('"SIMULATED EQUIVALENT"',3X,'"OBSERVED or PRIOR VALUE"',1X, &
        '"PLOT SYMBOL"',5X,'"OBSERVATION or PRIOR NAME"')
    !
    !   Open data exchange file
    ! write header
    IF (IUOS .GT. 0) THEN
      IF (WRITEHEADER) WRITE(IUOS,600)
    ELSE
      AMESSAGE = 'Warning: IUOS<0 in DEP_UEV_DX_WRITE_OS;'//   &
                 ' _os file not written"'
      CALL UTL_WRITE_MESSAGE()
      RETURN
    ENDIF
    !   Write simulated and observed values
    EACHOBS: DO N=1,NOBS
      IF (PRESENT(IPLOTOPT)) THEN
        WRITE(IUOS,520) MODELVAL(N),OBSVAL(N),IPLOTOPT(N),TRIM(OBSNAM(N))
      ELSE
        CALL DEP_GET_GROUP(N,GPNAM,IPLOT,'USED')
        WRITE(IUOS,520) MODELVAL(N),OBSVAL(N),IPLOT,TRIM(OBSNAM(N))
      ENDIF
    ENDDO EACHOBS
    !
    RETURN
  END SUBROUTINE DEP_UEV_DX_WRITE_OS
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_UEV_DX_WRITE_P(IUP,NPRED,MODELVAL,PREDNAM,WRITEHEADER,IPLOTOPT)
    !   Write _p Data-Exchange file: Predictions
    USE GLOBAL_DATA, ONLY: AMESSAGE, LENDNAM
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                  INTENT(IN) :: IUP
    INTEGER,                                  INTENT(IN) :: NPRED
    DOUBLE PRECISION,       DIMENSION(NPRED), INTENT(IN) :: MODELVAL
    CHARACTER(LEN=LENDNAM), DIMENSION(NPRED), INTENT(IN) :: PREDNAM
    LOGICAL,                                  INTENT(IN) :: WRITEHEADER
    INTEGER, OPTIONAL,      DIMENSION(NPRED), INTENT(IN) :: IPLOTOPT
    !
    !   Local variables
    INTEGER :: I, IPLOT
    INTEGER :: N, NOBS
    CHARACTER(LEN=12) :: GPNAM
    !
    !   Format statements
    300 FORMAT(1X,'"PREDICTED VALUE"',4X,'"PLOT SYMBOL"',2X,'"PREDICTION NAME"')
    301 FORMAT(1X,G22.15,1X,I10,4X,A20,1X,G22.15,1X,A12)
    !
    !   Open data exchange file and, if required, write header
    NOBS = NTOTDEP-NTOTPRED
    IF (IUP .GT. 0) THEN
      IF (WRITEHEADER) WRITE(IUP,300)
    ELSE
      AMESSAGE = 'Warning: IUP<0 in DEP_UEV_DX_WRITE_P;'//   &
                 ' _p file not written"'
      CALL UTL_WRITE_MESSAGE()
      RETURN
    ENDIF
    !   Write predictions
    DO I=1,NPRED
      N=NOBS+I
      IF (PRESENT(IPLOTOPT)) THEN
        IPLOT = IPLOTOPT(I)
      ELSE
        CALL DEP_GET_GROUP(N,GPNAM,IPLOT,'USED')
      ENDIF
      WRITE(IUP,301)MODELVAL(I),IPLOT,PREDNAM(I)
    ENDDO
    !
    RETURN
  END SUBROUTINE DEP_UEV_DX_WRITE_P
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_UEV_DX_WRITE_R(IUR,NOBS,MODELVAL,OBSNAM,OBSVAL,   &
                                WRITEHEADER,IPLOTOPT)
    !   Write _r Data-Exchange file: Residuals
    USE GLOBAL_DATA, ONLY: AMESSAGE, BIGREAL, LENDNAM, IVERB, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: IUR
    INTEGER,                                 INTENT(IN) :: NOBS
    DOUBLE PRECISION,       DIMENSION(NOBS), INTENT(IN) :: MODELVAL
    CHARACTER(LEN=LENDNAM), DIMENSION(NOBS), INTENT(IN) :: OBSNAM
    DOUBLE PRECISION,       DIMENSION(NOBS), INTENT(IN) :: OBSVAL
    LOGICAL,                                 INTENT(IN) :: WRITEHEADER
    INTEGER, OPTIONAL,      DIMENSION(NOBS), INTENT(IN) :: IPLOTOPT
    !
    !   Local variables
    INTEGER :: IPLOT, N !, NMNOBS
    CHARACTER(LEN=12) :: GPNAM
    !
    !   Format statements
    520 FORMAT(G22.15,11X,I5,9X,A)
    !
    603 FORMAT('"RESIDUAL"',16X,'"PLOT SYMBOL"',5X, &
        '"OBSERVATION or PRIOR NAME"')
    !   Open data exchange file
    ! write header
    IF (IUR .GT. 0) THEN
      IF (WRITEHEADER) WRITE(IUR,603)
    ELSE
      AMESSAGE = 'Warning: IUR<0 in DEP_UEV_DX_WRITE_R;'//   &
                 ' _r file not written"'
      CALL UTL_WRITE_MESSAGE()
      RETURN
    ENDIF
    !   Write residuals
    EACHOBS: DO N=1,NOBS
      IF (PRESENT(IPLOTOPT)) THEN
        WRITE(IUR,520) OBSVAL(N)-MODELVAL(N),IPLOTOPT(N),TRIM(OBSNAM(N))
      ELSE
        CALL DEP_GET_GROUP(N,GPNAM,IPLOT,'USED')
        WRITE(IUR,520) OBSVAL(N)-MODELVAL(N),IPLOT,TRIM(OBSNAM(N))
      ENDIF
    ENDDO EACHOBS
    !
    RETURN
  END SUBROUTINE DEP_UEV_DX_WRITE_R
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_UEV_DX_WRITE_SS(MAXITER,INCLU,ITERP,OUTNAM,RSQA,RSPA)
    !     VERSION 20040715 EPP
    !     WRITE _SS DX FILE Sum of Squared Residuals
    USE UTILITIES
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                INTENT(IN) :: MAXITER
    INTEGER,          DIMENSION(MAXITER+1), INTENT(IN) :: INCLU
    INTEGER,                                INTENT(IN) :: ITERP
    CHARACTER(LEN=MAX_STRING_LEN),          INTENT(IN) :: OUTNAM
    DOUBLE PRECISION, DIMENSION(MAXITER+1), INTENT(IN) :: RSQA
    DOUBLE PRECISION, DIMENSION(MAXITER+1), INTENT(IN) :: RSPA
    !
    ! Local variables
    INTEGER I, IUSS
    !
    ! Format statments
    392 FORMAT(1X,'"ITERATION" "SSWR-(OBSERVATIONS ONLY)"', &
             ' "SSWR-(PRIOR INFORMATION ONLY)"', &
             ' "SSWR-(TOTAL)"', &
             ' "# OBSERVATIONS INCLUDED"')
    393 FORMAT(1X,I10,3(20X,1PE15.7),1X,I8)
    !
    ! Open File
    IUSS = UTL_DX_OPEN(OUTNAM,'_ss','REPLACE')
    ! Write Header
    WRITE(IUSS,392)
    ! Write  data
    DO I=1,ITERP
      WRITE(IUSS,393)I-1,RSQA(I),RSPA(I)-RSQA(I),RSPA(I),INCLU(I)
    ENDDO
    !
    ! Close File
    IUSS = UTL_DX_CLOSE('_ss')
    RETURN
  END SUBROUTINE DEP_UEV_DX_WRITE_SS
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_UEV_DX_WRITE_W(IUW,NOBS,WTDRESIDS,OBSNAM,WRITEHEADER,IPLOTOPT)
    !   Write _w Data-Exchange file: Weighted residuals
    USE GLOBAL_DATA, ONLY: AMESSAGE, BIGREAL, LENDNAM, IVERB, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: IUW
    INTEGER,                                 INTENT(IN) :: NOBS
    DOUBLE PRECISION,       DIMENSION(NOBS), INTENT(IN) :: WTDRESIDS
    CHARACTER(LEN=LENDNAM), DIMENSION(NOBS), INTENT(IN) :: OBSNAM
    LOGICAL,                                 INTENT(IN) :: WRITEHEADER
    INTEGER, OPTIONAL,      DIMENSION(NOBS), INTENT(IN) :: IPLOTOPT
    !
    !   Local variables
    INTEGER :: IPLOT, N
    CHARACTER(LEN=12) :: GPNAM
    !
    !   Format statements
    540 FORMAT(G22.15,3X,I10,3X,A)
    604 FORMAT(1X,'"WEIGHTED RESIDUAL"',3X,'"PLOT SYMBOL"', &
        ' "OBSERVATION or PRIOR NAME"')
    !
    !   Open data exchange file
    ! write header
    IF (IUW .GT. 0) THEN
      IF (WRITEHEADER) WRITE(IUW,604)
    ELSE
      AMESSAGE = 'Warning: IUW<0 in DEP_UEV_DX_WRITE_W;'//   &
                 ' _os file not written"'
      CALL UTL_WRITE_MESSAGE()
      RETURN
    ENDIF
    !   Write weighted residuals
    EACHOBS: DO N=1,NOBS
      IF (PRESENT(IPLOTOPT)) THEN
        WRITE(IUW,540) WTDRESIDS(N),IPLOTOPT(N),OBSNAM(N)
      ELSE
        CALL DEP_GET_GROUP(N,GPNAM,IPLOT,'USED')
        WRITE(IUW,540) WTDRESIDS(N),IPLOT,OBSNAM(N)
      ENDIF
    ENDDO EACHOBS
    !
    RETURN
  END SUBROUTINE DEP_UEV_DX_WRITE_W
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_UEV_DX_WRITE_WS(IUWS,NOBS,MODELVAL,OBSNAM,WTDRESIDS,   &
                                 WRITEHEADER,IPLOTOPT)
    !   Write _ws Data-Exchange file: Simulated values and weighted residuals
    USE GLOBAL_DATA, ONLY: AMESSAGE, BIGREAL, LENDNAM, IVERB, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: IUWS
    INTEGER,                                 INTENT(IN) :: NOBS
    DOUBLE PRECISION,       DIMENSION(NOBS), INTENT(IN) :: MODELVAL
    CHARACTER(LEN=LENDNAM), DIMENSION(NOBS), INTENT(IN) :: OBSNAM
    DOUBLE PRECISION,       DIMENSION(NOBS), INTENT(IN) :: WTDRESIDS
    LOGICAL,                                 INTENT(IN) :: WRITEHEADER
    INTEGER, OPTIONAL,      DIMENSION(NOBS), INTENT(IN) :: IPLOTOPT
    !
    !   Local variables
    INTEGER :: IPLOT, N
    CHARACTER(LEN=12) :: GPNAM
    !
    !   Format statements
    523 FORMAT(2X,G22.15,2X,G22.15,3X,I10,4X,A)
    602 FORMAT(1X,'"SIMULATED EQUIVALENT"',4X,   &
        '"WEIGHTED RESIDUAL"',3X,'"PLOT SYMBOL"',2X,   &
        '"OBSERVATION or PRIOR NAME"')
    !
    !   Open data exchange file
    ! write header
    IF (IUWS .GT. 0) THEN
      IF (WRITEHEADER) WRITE(IUWS,602)
    ELSE
      AMESSAGE = 'Warning: IUWS<0 in DEP_UEV_DX_WRITE_WS;'//   &
                 ' _ws file not written"'
      CALL UTL_WRITE_MESSAGE()
      RETURN
    ENDIF
    !   Write simulated values and weighted residuals
    EACHOBS: DO N=1,NOBS
      IF (PRESENT(IPLOTOPT)) THEN
        WRITE(IUWS,523) MODELVAL(N),WTDRESIDS(N),IPLOTOPT(N),OBSNAM(N)
      ELSE
        CALL DEP_GET_GROUP(N,GPNAM,IPLOT,'USED')
        WRITE(IUWS,523) MODELVAL(N),WTDRESIDS(N),IPLOT,OBSNAM(N)
      ENDIF
    ENDDO EACHOBS
    !
    RETURN
  END SUBROUTINE DEP_UEV_DX_WRITE_WS
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_UEV_DX_WRITE_WW(IUWW,NOBS,OBSNAM,WTDOBS,WTDSIM,WRITEHEADER,   &
                                 IPLOTOPT)
    !   Write _ww Data-Exchange file: Weighted simulated values and weighted
    !   observed or prior values
    USE GLOBAL_DATA, ONLY: AMESSAGE, BIGREAL, LENDNAM, IVERB, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: IUWW
    INTEGER,                                 INTENT(IN) :: NOBS
    CHARACTER(LEN=LENDNAM), DIMENSION(NOBS), INTENT(IN) :: OBSNAM
    DOUBLE PRECISION,       DIMENSION(NOBS), INTENT(IN) :: WTDOBS
    DOUBLE PRECISION,       DIMENSION(NOBS), INTENT(IN) :: WTDSIM
    LOGICAL,                                 INTENT(IN) :: WRITEHEADER
    INTEGER, OPTIONAL,      DIMENSION(NOBS), INTENT(IN) :: IPLOTOPT
    !
    !   Local variables
    INTEGER :: IPLOT, N
    CHARACTER(LEN=12) :: GPNAM
    !
    !   Format statements
    522 FORMAT(2X,G22.15,9X,G22.15,14X,I10,4X,A)
    601 FORMAT(1X,'"WEIGHTED SIMULATED EQUIVALENT"', &
        ' "WEIGHTED OBSERVED or PRIOR VALUE"', &
        ' "PLOT SYMBOL" "OBSERVATION or PRIOR NAME"')
    !
    !   Open data exchange file
    ! write header
    IF (IUWW .GT. 0) THEN
      IF (WRITEHEADER) WRITE(IUWW,601)
    ELSE
      AMESSAGE = 'Warning: IUWW<0 in DEP_UEV_DX_WRITE_WW;'//   &
                 ' _ww file not written"'
      CALL UTL_WRITE_MESSAGE()
      RETURN
    ENDIF
    !   Write weighted simulated and weighted observed values
    EACHOBS: DO N=1,NOBS
      IF (PRESENT(IPLOTOPT)) THEN
        WRITE(IUWW,522) WTDSIM(N),WTDOBS(N),IPLOTOPT(N),OBSNAM(N)
      ELSE
        CALL DEP_GET_GROUP(N,GPNAM,IPLOT,'USED')
        WRITE(IUWW,522) WTDSIM(N),WTDOBS(N),IPLOT,OBSNAM(N)
      ENDIF
    ENDDO EACHOBS
    !
    RETURN
  END SUBROUTINE DEP_UEV_DX_WRITE_WW
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_UEV_RESIDUALS(NOBS,MODELVAL,WTMATSQR,RESIDS,WTDRESIDS)
    !   Calculate residuals and weighted residuals, using a full weight matrix.
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                           INTENT(IN)  :: NOBS
    DOUBLE PRECISION, DIMENSION(NOBS), INTENT(IN)  :: MODELVAL  ! Simulated equivalent
    TYPE (CDMATRIX),                   INTENT(IN)  :: WTMATSQR  ! Square-root of weight matrix
    DOUBLE PRECISION, DIMENSION(NOBS), INTENT(OUT) :: RESIDS    ! Residuals (Obs-Sim)
    DOUBLE PRECISION, DIMENSION(NOBS), INTENT(OUT) :: WTDRESIDS ! Weighted residuals
    !
    !   Local variables
    INTEGER :: I
    !
    !   Calculate residuals
    DO I=1,NOBS
      RESIDS(I) = OBSVAL(I)-MODELVAL(I)
    ENDDO
    !   Calculate weighted residuals
    !WTDRESIDS = UTL_MATMULVEC(NOBS,WTMATSQR,RESIDS)
    CALL UTL_MATMULVEC_SUB(NOBS,WTMATSQR,RESIDS,WTDRESIDS)
    !
    RETURN
  END SUBROUTINE DEP_UEV_RESIDUALS
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_UEV_SCALESENS(NDEP,NPE,NPT,B,BSCAL,IFLAG,IPTR,LN,WTM, &
                               WTMSQR,X,CSS,SCALEX)
    !   Calculate either: (IFLAG=1) -- Dimensionless scaled sensitivity matrix
    !   and composite scaled sensitivity vector; or (IFLAG=2) -- One percent
    !   scaled sensitivity matrix.
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                               INTENT(IN)    :: NDEP
    INTEGER,                               INTENT(IN)    :: NPE
    INTEGER,                               INTENT(IN)    :: NPT
    DOUBLE PRECISION, DIMENSION(NPT),      INTENT(IN)    :: B     ! Current parameter values
    DOUBLE PRECISION, DIMENSION(NPT),      INTENT(IN)    :: BSCAL
    INTEGER,                               INTENT(IN)    :: IFLAG ! 1 for DSS, 2 for 1%SS
    INTEGER, DIMENSION(NPE),               INTENT(IN)    :: IPTR
    INTEGER, DIMENSION(NPT),               INTENT(IN)    :: LN
    TYPE (CDMATRIX),                       INTENT(IN)    :: WTM
    TYPE (CDMATRIX),                       INTENT(IN)    :: WTMSQR
    DOUBLE PRECISION, DIMENSION(NPE,NDEP), INTENT(IN)    :: X
    DOUBLE PRECISION, DIMENSION(NPE),      INTENT(INOUT) :: CSS
    DOUBLE PRECISION, DIMENSION(NPE,NDEP), INTENT(OUT)   :: SCALEX
    !
    !   Local variables
    INTEGER :: I, J, JP, K, L, NNZWTS
    DOUBLE PRECISION :: BB, DONE, DZERO
    !
    !   Format statements
    100 FORMAT(1X,'Programmer error: IFLAG must be 1 or 2 in DEP_UEV_SCALESENS')
    !
    DZERO = 0.0D0
    DONE = 1.0D0
    !
    SCALEX = DZERO
    NNZWTS = WTMSQR%NNZ
    !
    IF (IFLAG==1) THEN
      !   Calculate Dimensionless Scaled Sensitivity matrix
      DO J=1,NPE
        JP = IPTR(J)
        BB=ABS(B(JP))
        IF (LN(JP).LE.0) THEN
          !   Parameter is not log-transformed
          IF (BB<BSCAL(JP)) BB = DBLE(BSCAL(JP))
        ELSE
          !   Parameter is log transformed
          BB = DONE
        ENDIF
        DO L=1,NNZWTS   ! L is location in WTMSQR%DVAL and WTMSQR%IPOS arrays
          I = UTL_GETIROW(WTMSQR,L)
          K = UTL_GETICOL(WTMSQR,L)
          IF (UTL_GETVAL(WTM,K,K)>DZERO) THEN
            SCALEX(J,K) = SCALEX(J,K)+BB*UTL_GETVAL(WTMSQR,I,K)*X(J,K)
          ENDIF
        ENDDO
      ENDDO
      !   Calculate Composite Scaled Sensitivity vector
      CSS = DZERO
      DO J=1,NPE
        DO I=1,NDEP
          CSS(J) = CSS(J)+SCALEX(J,I)*SCALEX(J,I)
        ENDDO
        CSS(J) = SQRT(CSS(J)/DBLE(NDEP))
      ENDDO
    ELSEIF (IFLAG==2) THEN
      !   Calculate One-Percent Scaled Sensitivity matrix
    ELSE
      WRITE(*,100)
      CALL UTL_STOP(' ')
    ENDIF
    !
    RETURN
  END SUBROUTINE DEP_UEV_SCALESENS
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_UEV_WRITEOBSTABLE(IOUT,NOBS,MODELVAL,OBSNAM,RESIDS,WTDRESIDS)
    !   Write observed and model-calculated values as a table, showing
    !   residuals and weighted residuals
    !
    USE GLOBAL_DATA, ONLY: BLANKS, HYPHENS
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: IOUT
    INTEGER,                                 INTENT(IN) :: NOBS      ! Number of observations
    DOUBLE PRECISION, DIMENSION(NOBS),       INTENT(IN) :: MODELVAL  ! Simulated equivalents
    CHARACTER(LEN=LENDNAM), DIMENSION(NOBS), INTENT(IN) :: OBSNAM    ! Observation names
    DOUBLE PRECISION, DIMENSION(NOBS),       INTENT(IN) :: RESIDS    ! Residuals
    DOUBLE PRECISION, DIMENSION(NOBS),       INTENT(IN) :: WTDRESIDS ! Weighted residuals
    !
    !   Local variables
    INTEGER :: I
    DOUBLE PRECISION :: RSS
    !
    !   Format statements
    200 FORMAT(//,1X,'OBSERVED AND MODEL-CALCULATED VALUES,',   &
        ' WITH RESIDUALS AND WEIGHTED RESIDUALS:',//,   &
        1X,'OBSERVATION',A,2X,'OBSERVED',5X,'MODEL-CALC.',   &
        18X       ,'WEIGHTED',/,  &
        1X,'   NAME',A,3X,'VALUE  ',7X,'VALUE',   &
        8X,'RESIDUAL',6X,'RESIDUAL',/,  &
        1X,A,4(2X,12('-')))
    250 FORMAT(1X,A,4(2X,G12.5))
    300 FORMAT(/,1X,'SUM OF SQUARED, WEIGHTED RESIDUALS = ',G12.5)

    RSS = 0.0
    WRITE(IOUT,200)BLANKS(1:LENDNAM-9),BLANKS(1:LENDNAM-5),HYPHENS(1:LENDNAM)
    DO I=1,NOBS
      WRITE(IOUT,250) OBSNAM(I),OBSVAL(I),MODELVAL(I),RESIDS(I),WTDRESIDS(I)
      RSS = RSS + WTDRESIDS(I)**2
    ENDDO
    WRITE(IOUT,300) RSS
    RETURN
  END SUBROUTINE DEP_UEV_WRITEOBSTABLE
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_CLN()
    !   Deallocate all arrays and linked lists in the Dependents module
    IMPLICIT NONE
    !
    !   Deallocate arrays
    IF (ALLOCATED(COVMATNAM)) DEALLOCATE(COVMATNAM)
    IF (ALLOCATED(GROUP)) DEALLOCATE(GROUP)
    IF (ALLOCATED(GROUPNAM)) DEALLOCATE(GROUPNAM)
    IF (ALLOCATED(DERDEPEQN)) DEALLOCATE(DERDEPEQN)
    IF (ALLOCATED(EQUATION_FLAG)) DEALLOCATE(EQUATION_FLAG)
    IF (ALLOCATED(NUMINGROUP)) DEALLOCATE(NUMINGROUP)
    IF (ALLOCATED(OBSVAL)) DEALLOCATE(OBSVAL)
    IF (ALLOCATED(PLOTSYMBOL)) DEALLOCATE(PLOTSYMBOL)
    IF (ALLOCATED(USEFLAG)) DEALLOCATE(USEFLAG)
    IF (ALLOCATED(USE_FLAGDEP)) DEALLOCATE(USE_FLAGDEP)
    IF (ALLOCATED(VARIANCE)) DEALLOCATE(VARIANCE)
    IF (ALLOCATED(WTCORRELATED)) DEALLOCATE(WTCORRELATED)
    IF (ALLOCATED(WTMULTIPLIER)) DEALLOCATE(WTMULTIPLIER)
    !
    !   Deallocate pointers to linked lists
    CALL TYP_DEALLOC(LLPTRDEPGP)
    CALL TYP_DEALLOC(LLPTRDEP)
    !
    RETURN
  END SUBROUTINE DEP_CLN
  !-----------------------------------------------------------------------------
  !   Dependents-module utilities
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_DX_WRITE_GM(IUGM,NOBS,OBSNAM,WRITEHEADER)
    !   Write _gm Data-Exchange file: Group names and member names
    USE GLOBAL_DATA, ONLY: AMESSAGE, BIGREAL, LENDNAM, IVERB, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: IUGM
    INTEGER,                                 INTENT(IN) :: NOBS
    CHARACTER(LEN=LENDNAM), DIMENSION(NOBS), INTENT(IN) :: OBSNAM
    LOGICAL,                                 INTENT(IN) :: WRITEHEADER
    !
    !   Local variables
    INTEGER :: IPLOT, N
    CHARACTER(LEN=12) :: GPNAM
    !
    !   Format statements
    520 FORMAT(A,3X,A,2X,I10)
    600 FORMAT('"GROUP NAME"',3X,'"MEMBER NAME"',8X,'"PLOT SYMBOL"')
    !
    !   Open data exchange file
    ! write header
    IF (IUGM .GT. 0) THEN
      IF (WRITEHEADER) WRITE(IUGM,600)
    ELSE
      AMESSAGE = 'Warning: IUGM<0 in DEP_DX_WRITE_GM;'//   &
                 ' _gm file not written"'
      CALL UTL_WRITE_MESSAGE()
      RETURN
    ENDIF
    !   Write simulated and observed values
    EACHOBS: DO N=1,NOBS
      CALL DEP_GET_GROUP(N,GPNAM,IPLOT,'USED')
      WRITE(IUGM,520) GPNAM,OBSNAM(N),IPLOT
    ENDDO EACHOBS
    !
    RETURN
  END SUBROUTINE DEP_DX_WRITE_GM
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_GET_GROUP(N,GPNAM,IPLOT,DEPSET)
    !   Return group name and plot symbol, given an observation number
    USE GLOBAL_DATA, ONLY: AMESSAGE
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                    INTENT(IN)  :: N
    CHARACTER(LEN=12),          INTENT(OUT) :: GPNAM
    INTEGER,                    INTENT(OUT) :: IPLOT
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: DEPSET
    !
    !   Local variables
    CHARACTER(LEN=5) :: DEPSETLOCAL
    !
    DEPSETLOCAL = 'ALL  '
    IF (PRESENT(DEPSET)) CALL UTL_CASE(DEPSET,DEPSETLOCAL,1)
    GPNAM = ' '
    IPLOT = -1
    IF (N>0) THEN
      IF (DEPSETLOCAL=='ALL  ') THEN
        IF (N .LE. NTOTDEP) THEN
          GPNAM = GROUP(N)
          IPLOT = DEP_GET_PLOTSYMBOL(GPNAM,1)
        ENDIF
      ELSEIF (DEPSETLOCAL=='USED') THEN
        IF (N .LE. NUSEDEP) THEN
          GPNAM = GROUPNOBS(N)
          IPLOT = DEP_GET_PLOTSYMBOL(GPNAM,1)
        ENDIF
      ELSE
        AMESSAGE = 'Programmer error in call to DEP_GET_GROUP:'//   &
            ' Argument DEPSET must be either ALL or USED.'
        CALL UTL_STOP()
      ENDIF
    ENDIF
    !
    RETURN
  END SUBROUTINE DEP_GET_GROUP
  !-----------------------------------------------------------------------------
  SUBROUTINE DEP_GET_WTMULTIPLIER(GPNAM,WTMULT)
    !   Return wtmultiplier, given an observation group name
    USE GLOBAL_DATA, ONLY: AMESSAGE
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=12), INTENT(IN)  :: GPNAM
    DOUBLE PRECISION,  INTENT(OUT) :: WTMULT
    !
    !   Local variables
    INTEGER :: I
    !
    WTMULT = 1.0D0
    DO I=1,NDEPGPS
      IF (GPNAM.EQ.GROUPNAM(I)) THEN
        WTMULT = WTMULTIPLIER(I)
        EXIT
      ENDIF
    ENDDO
    !
    RETURN
  END SUBROUTINE DEP_GET_WTMULTIPLIER
  !-----------------------------------------------------------------------------
  FUNCTION DEP_GET_PLOTSYMBOL(GNAM,IDEFAULT) RESULT(IPLOT)
    !   Find and return plotsymbol value for group GNAM
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    CHARACTER(LEN=12), INTENT(IN)  :: GNAM     ! Group name
    INTEGER,           INTENT(IN)  :: IDEFAULT ! Default value to be assigned to IPLOT
    INTEGER                        :: IPLOT    ! Plot symbol for group GNAM
    !
    !   Local variables
    INTEGER :: I
    !
    IPLOT = IDEFAULT
    EACHGROUP: DO I=1,NDEPGPS
      IF (GNAM.EQ.GROUPNAM(I)) THEN
        IPLOT = PLOTSYMBOL(I)
        EXIT EACHGROUP
      ENDIF
    ENDDO EACHGROUP
    RETURN
  END FUNCTION DEP_GET_PLOTSYMBOL
  !-----------------------------------------------------------------------------
END MODULE DEPENDENTS
