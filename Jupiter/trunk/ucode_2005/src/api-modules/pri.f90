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
MODULE PRIOR_INFORMATION
  USE DATATYPES
  USE GLOBAL_DATA, ONLY: LENDNAM, MAX_STRING_LEN
  USE UTILITIES
  IMPLICIT NONE
  SAVE
  PRIVATE
  !
  !   PUBLIC DATA
  !
  PUBLIC LLPTRPRIOR, LLPTRGPPRIOR, LLPTRPRIORFP, LLPTRGPPRIORFP, &
         MODELPRIVAL, NPRIGPS, NPRIGPSFP, PLOTSYMBOLPRI, PRIEQN, &
         PRILN, PRINAM, PRINAMLC, PRIVAL, PRIWTCORR, PRIWTMAT, PRIWTMATSQR, &
         RESIDSPRI, WTDRESIDSPRI
  PUBLIC :: LLPTRPRIORCOPY, LLPTRPRIORFPCOPY
  !
  TYPE (LLIST), POINTER                             :: LLPTRGPPRIOR
  TYPE (LLIST), POINTER                             :: LLPTRGPPRIORFP
  TYPE (LLIST), POINTER                             :: LLPTRPRIOR, LLPTRPRIORCOPY
  TYPE (LLIST), POINTER                             :: LLPTRPRIORFP, LLPTRPRIORFPCOPY
  DOUBLE PRECISION,       ALLOCATABLE, DIMENSION(:) :: MODELPRIVAL
  INTEGER                                           :: NPRIGPS = 0
  INTEGER                                           :: NPRIGPSFP = 0
  INTEGER,                ALLOCATABLE, DIMENSION(:) :: PLOTSYMBOLPRI
  DOUBLE PRECISION,     ALLOCATABLE, DIMENSION(:,:) :: PRIEQN
  INTEGER,                ALLOCATABLE, DIMENSION(:) :: PRILN
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: PRINAM
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: PRINAMLC
  DOUBLE PRECISION,       ALLOCATABLE, DIMENSION(:) :: PRIVAL    ! Prior-information value
  LOGICAL,                ALLOCATABLE, DIMENSION(:) :: PRIWTCORR ! (default=false)
  TYPE (CDMATRIX)                                   :: PRIWTMAT  ! Weight matrix for all prior-information equations
  TYPE (CDMATRIX)                                   :: PRIWTMATSQR
  DOUBLE PRECISION,       ALLOCATABLE, DIMENSION(:) :: RESIDSPRI
  DOUBLE PRECISION,       ALLOCATABLE, DIMENSION(:) :: WTDRESIDSPRI
  !
  !   PUBLIC SUBPROGRAMS
  !
  PUBLIC PRI_INI_ALLOC, PRI_INI_INSTALL, PRI_INI_POPX, PRI_INI_PROCESS, &
         PRI_INI_READ, PRI_INI_READ_WPRED,   &
         PRI_INI_STORE, PRI_INI_STORE_WPRED, &
         PRI_DEF, PRI_UEV_RESIDUALS, PRI_UEV_DX_READ_PR, PRI_UEV_DX_READ_PRP, &
         PRI_UEV_DX_WRITE_PR, PRI_UEV_DX_WRITE_PRP, PRI_CLN , &
         PRI_DX_WRITE_GM, PRI_GET_GROUP, PRI_GET_PLOTSYMBOL
  !
  !   PRIVATE DATA
  !
  INTEGER                                                  :: MPRWOP = 0
  INTEGER                                                  :: NPRIGPSWOP = 0
! NUMINPRIGROUP is always determined from list of prior-information equations
  INTEGER,                       ALLOCATABLE, DIMENSION(:) :: NUMINPRIGROUP
  INTEGER,                       ALLOCATABLE, DIMENSION(:) :: PLOTSYMBOLPRIGRP
  CHARACTER(LEN=12),             ALLOCATABLE, DIMENSION(:) :: PRICOVMATNAM
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: PRIEQTEXT
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:) :: PRIGROUP
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:) :: PRIGROUPNAM
  TYPE (CHAR_1D_30), ALLOCATABLE, DIMENSION(:) :: PRIMEMBER   ! (default: component ARR not allocated)
  LOGICAL,           ALLOCATABLE, DIMENSION(:) :: PRIUSEFLAG
  DOUBLE PRECISION,  ALLOCATABLE, DIMENSION(:) :: PRIVARIANCE ! Variance (uncorrelated)
  DOUBLE PRECISION,  ALLOCATABLE, DIMENSION(:) :: PRIWTMULTIPLIER
  DOUBLE PRECISION,  ALLOCATABLE, DIMENSION(:) :: WTDPRI      ! Weighted prior-information values
  !
  !   For LINEAR_PRIOR_INFORMATION input, define default column order
  INTEGER, PARAMETER                     :: NPRICOLS = 6
  CHARACTER(LEN=40), DIMENSION(NPRICOLS) :: PRICOL =    &
      (/'PRIORNAME     ','EQUATION      ','PRIORINFOVALUE','STATISTIC     ', &
        'STATFLAG      ','GROUPNAME     '/)
  !
  !   For LINEAR_PRIOR_INFORMATION_FOR_PREDICTION input, define default column order
  INTEGER, PARAMETER                     :: NPRICOLSFP = 6
  CHARACTER(LEN=40), DIMENSION(NPRICOLSFP) :: PRICOLFP =    &
      (/'PRIORNAME     ','EQUATION      ','PRIORINFOVALUE','STATISTIC     ', &
        'STATFLAG      ','GROUPNAME     '/)
  CHARACTER(LEN=40), DIMENSION(0) :: NOCOL
  !
CONTAINS
  !
  !-----------------------------------------------------------------------------
  SUBROUTINE PRI_INI_ALLOC(MPR)
    IMPLICIT NONE
    !
    INTEGER, INTENT(IN) :: MPR
    !
    ALLOCATE (MODELPRIVAL(MPR),RESIDSPRI(MPR),WTDRESIDSPRI(MPR))
    MODELPRIVAL = 0.D0
    RESIDSPRI = 0.D0
    WTDRESIDSPRI = 0.D0
    CALL TYP_NULL(PRIWTMAT)
    CALL TYP_NULL(PRIWTMATSQR)
    RETURN
  END SUBROUTINE PRI_INI_ALLOC
  !-----------------------------------------------------------------------------
  SUBROUTINE PRI_INI_INSTALL(IOUT,MPR,NEQNPTR)
    !   install prior equations
    USE UTILITIES
    USE EQUATION, ONLY: EQN_INI_INSTALL
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(IN) :: IOUT
    INTEGER, INTENT(IN) :: MPR     ! Number of prior
    INTEGER, INTENT(IN) :: NEQNPTR ! Number of equations previously installed
    !
    !   Local variables
    INTEGER               :: I, II, IFAIL
    !
    DO I=1,MPR
      II = NEQNPTR + I
      CALL UTL_CASE(PRINAM(I),PRINAMLC(I),-1) ! Store names as lowercase
      CALL UTL_CASETRANS(PRIEQTEXT(I),'lo') ! Store names as lowercase
      CALL EQN_INI_INSTALL(IFAIL,II,PRINAMLC(I),PRIEQTEXT(I))
      IF (IFAIL.NE.0) THEN
        CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
        CALL UTL_STOP('EQN_INI_INSTALL reported failure')
      ENDIF
    ENDDO
    RETURN
  END SUBROUTINE PRI_INI_INSTALL
  !-----------------------------------------------------------------------------
  SUBROUTINE PRI_INI_POPX(IOUT,IPRC,MPR,NPE,NPT,IPTR,LN,PARNAM,XLOG,XPRI, &
                          XPRIFILLED,CONVERTED)
    !   Populate XPRI array
    USE GLOBAL_DATA, ONLY: IVERB, LENDNAM
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                              INTENT(IN)    :: IOUT
    INTEGER,                              INTENT(IN)    :: IPRC
    INTEGER,                              INTENT(IN)    :: MPR
    INTEGER,                              INTENT(IN)    :: NPE
    INTEGER,                              INTENT(IN)    :: NPT
    INTEGER, DIMENSION(NPE),              INTENT(IN)    :: IPTR   ! Position of parameter in full set
    INTEGER, DIMENSION(NPT),              INTENT(IN)    :: LN
    CHARACTER(LEN=12), DIMENSION(NPT),    INTENT(IN)    :: PARNAM ! Parameter names
    DOUBLE PRECISION,                     INTENT(IN)    :: XLOG
    DOUBLE PRECISION, DIMENSION(NPE,MPR), INTENT(INOUT) :: XPRI
    LOGICAL,                              INTENT(INOUT) :: XPRIFILLED
    LOGICAL, OPTIONAL,                    INTENT(IN)    :: CONVERTED(MPR)
    !
    !   Local variables
    DOUBLE PRECISION                  :: CONVERT
    INTEGER                           :: I
    INTEGER                           :: II
    INTEGER                           :: IP
    INTEGER                           :: J
    CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:) :: NPENAM
    DOUBLE PRECISION                  :: SQCONVERT
    DOUBLE PRECISION                  :: NVTMP
    DOUBLE PRECISION                  :: NSDTMP
    DOUBLE PRECISION                  :: NATSD
    DOUBLE PRECISION                  :: NATPAR
    DOUBLE PRECISION                  :: LOG10VHI
    DOUBLE PRECISION                  :: LOG10VLO
    DOUBLE PRECISION                  :: LOG10SD
    DOUBLE PRECISION                  :: LOG10VAR
    DOUBLE PRECISION                  :: PRIVALLOG10
    DOUBLE PRECISION                  :: TESTLO
    DOUBLE PRECISION                  :: TESTSD
    DOUBLE PRECISION                  :: FRAC
    DOUBLE PRECISION                  :: RSDP
    !
    !   Format statements
    100 FORMAT(/,1X,'Prior-information sensitivity matrix:')
    120 FORMAT(/,1X,'Prior-information weight matrix:')
    140 FORMAT('PRIOR INFORMATION WEIGHTS (for a log-transformed parameter,', &
               ' the values relate to',/,'the NATURAL LOG of the parameter)')
    160 FORMAT(/,'Weight matrix:')
    180 FORMAT(/,'Square-root of the weight matrix:')
    !
    ! ALLOCATE
    ALLOCATE(NPENAM(NPE))
    !
    SQCONVERT = 2.30258509299405D0 * 2.30258509299405D0
    CONVERT = 2.30258509299405D0
    !
    IF (MPR>0 .AND. .NOT. XPRIFILLED) THEN
      !
      !   Write prior-information-group info to output file
      CALL PRI_INI_WRITEPRIGRPS(IOUT)
      XPRI = PRIEQN
      DO I=1,MPR
        CHECKLOG: DO J=1,NPE
          IF(XPRI(J,I) .NE. 0 .AND. LN(IPTR(J)).EQ.1) THEN
            XPRI(J,I) = XLOG
            PRILN(I) = 1
            ! TRANSFORMED PRIVALs ARE ENTERED IN Native Space,
            ! TAKE LN
            NATPAR = PRIVAL(I)
            PRIVALLOG10 = LOG10(PRIVAL(I))
            PRIVAL(I) = LOG(PRIVAL(I))
            IF(PRESENT(CONVERTED)) THEN
              IF(CONVERTED(I)) THEN
              ! EQUATION DID NOT INCLUDE LOG10 FOR THIS TRANSFORMED PARAMETER SO
              ! CONVERT STATISTIC TO LOG SPACE AND THEN TO LN SPACE BELOW
                IP = UTL_GETPOS(PRIWTMAT,I,I)
                IF(IP > 0) THEN
                  NATSD = SQRT(1.D0/PRIWTMAT%DVAL(IP))
                  TESTLO = NATPAR - (2.D0 * NATSD)
                  IF(TESTLO > 0.D0) THEN
                    LOG10SD  &
                    = (LOG10(NATPAR + (2.D0 * NATSD)) - LOG10(TESTLO)) / (4.D0)
                    LOG10VAR = LOG10SD*LOG10SD
                    NVTMP = 10.D0**(CONVERT*LOG10VAR+2.D0*LOG10(NATPAR))* &
                            (10.D0**(CONVERT*LOG10VAR) - 1.D0)
                  ELSE
                    LOG10SD  &
                    = (LOG10(NATPAR + (2.D0 * NATSD)) - PRIVALLOG10) / 2.D0
                    RSDP = LOG10SD/NATPAR
                    IF(RSDP > 0.49999D0) RSDP = 0.49999D0
                    FRAC = -843.676540*RSDP**6 + 1124.006352*RSDP**5 -  &
                           562.117956*RSDP**4 + 129.108628*RSDP**3 -  &
                           13.498901*RSDP**2 + 0.018265*RSDP + 0.496888
                    LOG10SD = LOG10SD/FRAC
                    LOG10VAR = LOG10SD*LOG10SD
                    NVTMP = 10.D0**(CONVERT*LOG10VAR+2.D0*LOG10(NATPAR))* &
                               (10.D0**(CONVERT*LOG10VAR) - 1.D0)
                  ENDIF
                  LOG10VHI = 0.D0
                  LOG10VLO = 0.D0
                  DO II=1,1000
                    NSDTMP = SQRT(NVTMP)
                    TESTSD = (NATSD-NSDTMP)/NATSD
                    IF(ABS(TESTSD) < 0.05) EXIT
                    ! 1st time through set hi or lo and adjust log10v
                    IF(II == 1) THEN
                      IF(TESTSD <= 0.D0) THEN
                        LOG10VHI = LOG10VAR
                        LOG10SD = LOG10SD * 0.01D0
                      ELSE
                        LOG10VLO = LOG10VAR
                        LOG10SD = LOG10SD * 1.99D0
                      ENDIF
                      LOG10VAR = LOG10SD * LOG10SD
                    ! >1st time if high hasnt been set do this
                    ELSEIF(LOG10VHI == 0.D0) THEN
                      IF(TESTSD <= 0.D0) THEN
                        IF(LOG10VHI == 0.D0 .OR. LOG10VHI > LOG10VAR) &
                                                            LOG10VHI = LOG10VAR
                        LOG10VAR = (LOG10VHI + LOG10VLO)/2.D0
                        LOG10SD = SQRT(LOG10VAR)
                      ELSE
                        IF (LOG10VLO < LOG10VAR) LOG10VLO = LOG10VAR
                        LOG10SD = LOG10SD * 1.99D0
                        LOG10VAR = LOG10SD * LOG10SD
                      ENDIF
                    ! >1st time if low hasnt been set do this
                    ELSEIF(LOG10VLO == 0.D0) THEN
                      IF(TESTSD >= 0.D0) THEN
                        IF(LOG10VLO == 0.D0 .OR. LOG10VLO < LOG10VAR) &
                                                            LOG10VLO = LOG10VAR
                        LOG10VAR = (LOG10VHI + LOG10VLO)/2.D0
                        LOG10SD = SQRT(LOG10VAR)
                      ELSE
                        IF(LOG10VHI > LOG10VAR) LOG10VHI = LOG10VAR
                        LOG10SD = LOG10SD * 0.01D0
                        LOG10VAR = LOG10SD * LOG10SD
                      ENDIF
                    ! >1st time LOW AND HI HAVE BEEN SET
                    ELSE
                      IF(TESTSD <= 0.D0) THEN
                        IF(LOG10VHI > LOG10VAR) LOG10VHI = LOG10VAR
                      ELSE
                        IF(LOG10VLO < LOG10VAR) LOG10VLO = LOG10VAR
                      ENDIF
                      LOG10VAR = (LOG10VHI + LOG10VLO)/2.D0
                    ENDIF
                    LOG10SD = SQRT(LOG10VAR)
                    NVTMP = 10.D0**(CONVERT*LOG10VAR+2.D0*LOG10(NATPAR))* &
                               (10.D0**(CONVERT*LOG10VAR) - 1.D0)
                  ENDDO
                  IF(IVERB > 2) THEN
                    WRITE(IOUT,*)
                    WRITE(IOUT,*)' PRIOR INFORMATION NOT IN LOG10 SPACE '
                    WRITE(IOUT,*)'   Prior#          #Iter to estimate LOG10Var'
                    WRITE(IOUT,*)'   ', I, II
                    WRITE(IOUT,*) &
                    '   Fractional Diff         InputSD & Estimate from LOG10SD'
                    WRITE(IOUT,*)'   ', TESTSD, NATSD, NSDTMP  
                  ENDIF                
                  PRIWTMAT%DVAL(IP) = 1.D0/ LOG10VAR
                  PRIWTMATSQR%DVAL(IP) = SQRT(PRIWTMAT%DVAL(IP))
                ENDIF
              ENDIF
            ENDIF
            ! IF THE PRIOR EQUATION INCLUDES LOG10 THEN TRANSFORMED WEIGHTS ARE
            ! IN LOG10 SPACE, SO TRANSFORM TO LN SPACE
            DO II=1,MPR
              IP = UTL_GETPOS(PRIWTMAT,I,II)
              IF(IP > 0) THEN
                PRIWTMAT%DVAL(IP)=PRIWTMAT%DVAL(IP)/(SQCONVERT)
                PRIWTMATSQR%DVAL(IP)=PRIWTMATSQR%DVAL(IP)/(CONVERT)
              ENDIF
! FULL MATRICES MUST BE INPUT IN A MANNER CONSISTENT WITH THE PARAMETER 
! TRANSFORM SPECIFICATIONS
!              IF(I .NE. II) THEN
!                IP = UTL_GETPOS(PRIWTMAT,II,I)
!                IF(IP > 0) THEN
!                  PRIWTMAT%DVAL(IP)=PRIWTMAT%DVAL(IP)*SQCONVERT
!                ENDIF
!              ENDIF
            ENDDO
            EXIT CHECKLOG
          ENDIF
        ENDDO CHECKLOG
      ENDDO
      !   Write prior information to output file
      WRITE(IOUT,*)' '
      WRITE(IOUT,*)' '
      WRITE(IOUT,*)'PRIOR INFORMATION'
      WRITE(IOUT,*) &
      ' (TRANSFORMED PARAMETERS AND WEIGHTS ARE REPORTED IN NATURAL LOG SPACE)'
      CALL PRI_INI_WRITEPRIS(IOUT,MPR)
      CALL UTL_CHECK_NAMES(IOUT,MPR,PRINAM)
      IF (IVERB>2) THEN
        WRITE(IOUT,*)' '
        WRITE(IOUT,*)' '
        WRITE(IOUT,140)
        WRITE(IOUT,160)
        CALL UTL_WRITECDMATRIX(PRIWTMAT,1,IOUT,.FALSE.,PRINAM,PRINAM)
        WRITE(IOUT,180)
        CALL UTL_WRITECDMATRIX(PRIWTMATSQR,1,IOUT,.FALSE.,PRINAM,PRINAM)
      ENDIF
    ENDIF
    IF (IVERB>3) THEN
      DO I=1,NPE
        NPENAM(I) = PARNAM(IPTR(I))
      ENDDO
      WRITE(IOUT,100)
      CALL UTL_WRITEMATRIX(NPE,MPR,XPRI,NPENAM,PRINAM,IPRC,IOUT)
      WRITE(IOUT,120)
      CALL UTL_WRITECDMATRIX(PRIWTMAT,1,IOUT)
    ENDIF
    XPRIFILLED = .TRUE.
    !
    ! DEALLOCATE
    DEALLOCATE(NPENAM)
    !
    RETURN
  END SUBROUTINE PRI_INI_POPX
  !-----------------------------------------------------------------------------
  SUBROUTINE PRI_INI_PROCESS(IOUT,IWRITE,MPR,NCOVMAT,COVMATARR,DTLA)
    !   Store and echo prior-information-related information.
    USE GLOBAL_DATA, ONLY: AMESSAGE, IVERB
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                             INTENT(IN)  :: IOUT
    LOGICAL,                             INTENT(IN)  :: IWRITE
    INTEGER,                             INTENT(IN)  :: MPR
    INTEGER,                             INTENT(IN)  :: NCOVMAT
    TYPE (CDMATRIX), DIMENSION(NCOVMAT), INTENT(IN)  :: COVMATARR   ! Array of all group variance-covariance matrices
    DOUBLE PRECISION,                    INTENT(OUT) :: DTLA
    !
    !   Local variables
    INTEGER :: I, J, K
    INTEGER :: IFAIL
    LOGICAL :: LCOR
    !
    !   Format statements
    250 FORMAT(1X,'Programmer error: In PRI_INI_PROCESS, K (',I7,   &
        ') does not equal NUMINPRIGROUP (',I7,') for group: ',A)
    !
    IFAIL = 0
    !   Check group members
    MEMBERS: DO I=1,NPRIGPS
      !
      !   Allocate PRIMEMBER array and populate it with names of prior-
      !   information equations that belong to this group
      ALLOCATE(PRIMEMBER(I)%CVAL(NUMINPRIGROUP(I)))
      PRIMEMBER(I)%NDIM = NUMINPRIGROUP(I)
      PRIMEMBER(I)%CVAL = ' '
      K = 0
      DO J=1,MPR
        IF (UTL_SAMENAME(PRIGROUP(J),PRIGROUPNAM(I))) THEN
          K = K+1
          PRIMEMBER(I)%CVAL(K) = PRINAM(J)
        ENDIF
      ENDDO
      !
      !   Check that number of prior-information equations in group is correct
      IF (K .NE. NUMINPRIGROUP(I)) THEN
        WRITE(IOUT,250) K,NUMINPRIGROUP(I),PRIGROUPNAM(I)
        CALL UTL_STOP(' ')
      ENDIF
      !
    ENDDO MEMBERS
    !
    !   Build variance-covariance matrix for all prior information
    CALL UTL_COVMAT(IOUT,NCOVMAT,MPR,NPRIGPS,COVMATARR,PRICOVMATNAM,   &
                    PRIGROUP,PRIGROUPNAM,PRINAM,PRIVARIANCE,   &
                    PRIWTCORR,PRIWTMAT)
    PRIWTMAT%arrayname = 'Prior Weight'
    !   Invert variance-covariance matrix to create weight matrix
    PRIWTMATSQR%ARRAYNAME = 'SQRT Prior Wt'
    LCOR = .FALSE.
    CHECKCORR: DO I =1,MPR
      IF (PRIWTCORR(I)) THEN
        LCOR = .TRUE.
        EXIT CHECKCORR
      ENDIF
    ENDDO CHECKCORR
    IF (LCOR) THEN
      !   Prior-information errors are correlated
      CALL UTL_SVD(IFAIL,PRIWTMAT,PRIWTMATSQR,DTLA)
    ELSE
      !   Prior-information errors are not correlated
      CALL UTL_INVERT_DIAG(IFAIL,PRIWTMAT,PRIWTMATSQR,DTLA)
    ENDIF
    !
    IF (IFAIL > 0) THEN
      AMESSAGE = 'ERROR: Failed in calculation of log determinant of variance-'// &
      'covariance matrix for prior information; matrix may not be invertible'
      CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
      CALL UTL_STOP(' ')
    ENDIF
    CALL UTL_CHECK_NAMES(IOUT,MPR,PRINAM)
    !
    !   Calculate weighted prior estimates
    CALL UTL_MATMULVEC_SUB(MPR,PRIWTMATSQR,PRIVAL,WTDPRI)
    !
    !   Deallocate linked lists here (unless needed by another module)
    RETURN
  END SUBROUTINE PRI_INI_PROCESS
  !-----------------------------------------------------------------------------
  SUBROUTINE PRI_INI_READ(INUNIT,IOUT,IWRITE,NPE,NCOVMAT,MPR,COPYPRILIST)
    !   Read prior-information input blocks and store data
    USE GLOBAL_DATA, ONLY: BIGDOUBLE, IVERB, MAX_STRING_LEN
    USE DATATYPES
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(IN)    :: INUNIT
    INTEGER, INTENT(IN)    :: IOUT
    LOGICAL, INTENT(IN)    :: IWRITE
    INTEGER, INTENT(IN)    :: NPE
    INTEGER, INTENT(INOUT) :: NCOVMAT
    INTEGER, INTENT(OUT)   :: MPR
    LOGICAL, INTENT(IN), OPTIONAL :: COPYPRILIST
    !
    !   Local variables
    TYPE (LLIST), POINTER            :: TAIL
    INTEGER                          :: I, IERR, MORE
    CHARACTER(LEN=12) :: CVNAM
    LOGICAL :: GPSDEF
    LOGICAL :: COPYPRILISTLOCAL
    !
    !   Format statements
    100 FORMAT(/,1X,'Number of linear prior-information equations = ',I4)
    !
    NULLIFY(LLPTRGPPRIOR,LLPTRPRIOR,TAIL)
    MPR = 0
    NPRIGPS = 0
    IERR = 0
    COPYPRILISTLOCAL = .FALSE.
    IF (PRESENT(COPYPRILIST)) THEN
      COPYPRILISTLOCAL = COPYPRILIST
    ENDIF
    !
    !   Read Prior_Information_Groups input block
    CALL UTL_READBLOCK(0,'PRIOR_INFORMATION_GROUPS',NOCOL,INUNIT,   &
                       IOUT,'GROUPNAME',.FALSE.,LLPTRGPPRIOR,TAIL,   &
                       NPRIGPS)
    IF (NPRIGPS==0) THEN
      GPSDEF = .FALSE.
      NPRIGPS = 1
    ELSE
      GPSDEF = .TRUE.
    ENDIF
    ALLOCATE(PLOTSYMBOLPRIGRP(NPRIGPS),PRIGROUPNAM(NPRIGPS), &
             PRIMEMBER(NPRIGPS),NUMINPRIGROUP(NPRIGPS), &
             PRIWTMULTIPLIER(NPRIGPS), &
             PRIUSEFLAG(NPRIGPS))
    !   Assign defaults for groups
    PLOTSYMBOLPRIGRP = 1
    PRIGROUPNAM = 'DefaultPrior'
    NUMINPRIGROUP = 0
    PRIWTMULTIPLIER = 1.0D0
    PRIUSEFLAG = .TRUE.
    CALL UTL_FILTERLIST(LLPTRGPPRIOR,IOUT,'GROUPNAME',NPRIGPS,   &
                        IERR,PRIGROUPNAM,MORE)
    CALL UTL_FILTERLIST(LLPTRGPPRIOR,IOUT,'USEFLAG',NPRIGPS,   &
                        IERR,PRIUSEFLAG,MORE)
    CALL UTL_FILTERLIST(LLPTRGPPRIOR,IOUT,'PLOTSYMBOL',NPRIGPS,   &
                        IERR,PLOTSYMBOLPRIGRP,MORE)
    CALL UTL_FILTERLIST(LLPTRGPPRIOR,IOUT,'WTMULTIPLIER',NPRIGPS,   &
                        IERR,PRIWTMULTIPLIER,MORE)
    NUMINPRIGROUP = 0
    IF (GPSDEF) THEN
      DO I=1,NPRIGPS
        CVNAM = ' '
        CALL UTL_FILTER(IERR,LLPTRGPPRIOR,IOUT,'COVMATRIX',CVNAM,I)
        IF (CVNAM .NE. ' ') NCOVMAT = NCOVMAT+1
      ENDDO
    ENDIF
    !
    !   Read Linear_Prior_Information input block
    CALL UTL_READBLOCK(NPRICOLS,'LINEAR_PRIOR_INFORMATION',PRICOL,   &
                       INUNIT,IOUT,'PRIORNAME',.FALSE.,   &
                       LLPTRPRIOR,TAIL,MPR)
    IF (COPYPRILISTLOCAL) THEN
      CALL UTL_COPYLIST(LLPTRPRIOR,LLPTRPRIORCOPY)
    ENDIF
    !
    !   Insert Prior_Information_Groups information into
    !   Linear_Prior_Information lists
    IF (NPRIGPS>0) THEN
      IF (MPR>0) THEN
        CALL UTL_GROUPLIST('DefaultPrior',LLPTRGPPRIOR,IOUT,LLPTRPRIOR,   &
                           NPRIGPS,MPR)
      ENDIF
    ENDIF
    !
    IF(IWRITE) WRITE(IOUT,100) MPR
    ALLOCATE (PLOTSYMBOLPRI(MPR),PRIEQN(NPE,MPR), &
              PRIEQTEXT(MPR),PRINAM(MPR),PRINAMLC(MPR), &
              PRICOVMATNAM(MPR),PRIGROUP(MPR), PRILN(MPR), &
              PRIVAL(MPR), PRIVARIANCE(MPR), PRIWTCORR(MPR), &
              WTDPRI(MPR))
    !   Assign defaults
    PLOTSYMBOLPRI = 1
    PRINAM = ' '
    PRICOVMATNAM = ' '
    PRIGROUP = 'DefaultPrior'
    PRILN = 0
    PRIWTCORR = .FALSE.
    PRIVAL = BIGDOUBLE
    PRIVARIANCE = BIGDOUBLE
    WTDPRI = BIGDOUBLE
    !
    RETURN
  END SUBROUTINE PRI_INI_READ
  !-----------------------------------------------------------------------------
  SUBROUTINE PRI_INI_READ_WPRED(INUNIT,IOUT,IWRITE,NPE,NCOVMAT,MPR,PRED,   &
                                MPRWOPPAS,COPYPRILISTS)
    !   Read prior-information input blocks and store data
    USE GLOBAL_DATA, ONLY: BIGDOUBLE, IVERB, MAX_STRING_LEN
    USE DATATYPES
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(IN)    :: INUNIT
    INTEGER, INTENT(IN)    :: IOUT
    LOGICAL, INTENT(IN)    :: IWRITE
    INTEGER, INTENT(IN)    :: NPE
    INTEGER, INTENT(INOUT) :: NCOVMAT
    INTEGER, INTENT(OUT)   :: MPR
    LOGICAL, INTENT(IN)    :: PRED
    INTEGER, INTENT(OUT)   :: MPRWOPPAS
    LOGICAL, INTENT(IN), OPTIONAL :: COPYPRILISTS
    !
    !   Local variables
    TYPE (LLIST), POINTER            :: TAIL
    INTEGER                          :: I, IERR, MORE, MPRFP
    CHARACTER(LEN=12) :: CVNAM
    LOGICAL :: GPSDEF
    LOGICAL :: COPYPRILISTSLOCAL
    !
    !   Format statements
    100 FORMAT(/,1X,'Number of linear prior-information equations = ',I4)
    !
    MPRWOPPAS = 0
    !
    NULLIFY(LLPTRGPPRIOR,LLPTRPRIOR,LLPTRGPPRIORFP,LLPTRPRIORFP,TAIL)
    NULLIFY(LLPTRPRIORCOPY,LLPTRPRIORFPCOPY)
    MPR = 0
    MPRFP = 0
    NPRIGPS = 0
    MPRWOP = 0
    IERR = 0
    COPYPRILISTSLOCAL = .FALSE.
    IF (PRESENT(COPYPRILISTS)) THEN
      COPYPRILISTSLOCAL = COPYPRILISTS
    ENDIF
    !
    !   Read Prior_Information_Groups input block
    CALL UTL_READBLOCK(0,'PRIOR_INFORMATION_GROUPS',NOCOL,INUNIT,   &
                       IOUT,'GROUPNAME',.FALSE.,LLPTRGPPRIOR,TAIL,   &
                       NPRIGPS)
    IF (NPRIGPS==0) THEN
      GPSDEF = .FALSE.
      NPRIGPSWOP = 1
      NPRIGPS = 1
    ELSE
      GPSDEF = .TRUE.
      NPRIGPSWOP = NPRIGPS
    ENDIF
    IF(PRED) THEN
      !   Read Prior_Information_Groups_For_Prediction input block
      CALL UTL_READBLOCK(0,'PRIOR_INFORMATION_GROUPS_FOR_PREDICTION', &
                         NOCOL,INUNIT,IOUT,'GROUPNAME',.FALSE., &
                         LLPTRGPPRIORFP,TAIL,NPRIGPSFP)
      IF (NPRIGPSFP==0) THEN
        IF(NPRIGPSWOP == 0)GPSDEF = .FALSE.
        NPRIGPSFP = 0
      ELSE
        GPSDEF = .TRUE.
        IF(NPRIGPSWOP > 0) NPRIGPS = NPRIGPS + NPRIGPSFP
      ENDIF
    ENDIF
    !
    !   Read Linear_Prior_Information input block
    CALL UTL_READBLOCK(NPRICOLS,'LINEAR_PRIOR_INFORMATION',PRICOL,   &
                       INUNIT,IOUT,'PRIORNAME',.FALSE.,   &
                       LLPTRPRIOR,TAIL,MPR)
    IF (COPYPRILISTSLOCAL) THEN
      CALL UTL_COPYLIST(LLPTRPRIOR,LLPTRPRIORCOPY)
    ENDIF
    MPRWOP = MPR
    IF(MPR == 0) THEN
      NPRIGPSWOP = 0
      NPRIGPS = NPRIGPSFP
    ENDIF
    MPRWOPPAS = MPR
    IF(PRED) THEN
      !   Read Linear_Prior_Information input block
      CALL UTL_READBLOCK(NPRICOLSFP,'LINEAR_PRIOR_INFORMATION_FOR_PREDICTION', &
                       PRICOLFP,INUNIT,IOUT,'PRIORNAME',.FALSE.,   &
                       LLPTRPRIORFP,TAIL,MPRFP)
      IF (COPYPRILISTSLOCAL) THEN
        CALL UTL_COPYLIST(LLPTRPRIORFP,LLPTRPRIORFPCOPY)
      ENDIF
      MPR = MPR + MPRFP
    ENDIF
    !
    !   Insert Prior_Information_Groups information into
    !   Linear_Prior_Information lists
    IF (NPRIGPS>0) THEN
      IF (MPRWOP>0) THEN
        CALL UTL_GROUPLIST('DefaultPrior',LLPTRGPPRIOR,IOUT,LLPTRPRIOR, &
                           NPRIGPS,MPRWOP)
      ENDIF
      IF (PRED .AND. MPR>MPRWOPPAS) THEN
        CALL UTL_GROUPLIST('DefaultPrior',LLPTRGPPRIORFP,IOUT,LLPTRPRIORFP,   &
                           NPRIGPSFP,MPRFP)
      ENDIF
    ENDIF
    ALLOCATE(PLOTSYMBOLPRIGRP(NPRIGPS),PRIGROUPNAM(NPRIGPS), &
             PRIMEMBER(NPRIGPS),NUMINPRIGROUP(NPRIGPS), &
             PRIWTMULTIPLIER(NPRIGPS), &
             PRIUSEFLAG(NPRIGPS))
    !   Assign defaults for groups
    PLOTSYMBOLPRIGRP = 1
    PRIGROUPNAM = 'DefaultPrior'
    NUMINPRIGROUP = 0
    PRIWTMULTIPLIER = 1.0D0
    PRIUSEFLAG = .TRUE.
    IF(NPRIGPSWOP > 0) THEN
      CALL UTL_FILTERLIST(LLPTRGPPRIOR,IOUT,'GROUPNAME',NPRIGPS,   &
                        IERR,PRIGROUPNAM,MORE)
      CALL UTL_FILTERLIST(LLPTRGPPRIOR,IOUT,'USEFLAG',NPRIGPS,   &
                        IERR,PRIUSEFLAG,MORE)
      CALL UTL_FILTERLIST(LLPTRGPPRIOR,IOUT,'PLOTSYMBOL',NPRIGPS,   &
                        IERR,PLOTSYMBOLPRIGRP,MORE)
      CALL UTL_FILTERLIST(LLPTRGPPRIOR,IOUT,'WTMULTIPLIER',NPRIGPS,   &
                        IERR,PRIWTMULTIPLIER,MORE)
    ENDIF
    IF(PRED .AND. NPRIGPSFP > 0)THEN
      CALL UTL_FILTERLIST(LLPTRGPPRIORFP,IOUT,'GROUPNAME',NPRIGPS,   &
                        IERR,PRIGROUPNAM,MORE,NPRIGPSWOP+1)
      CALL UTL_FILTERLIST(LLPTRGPPRIORFP,IOUT,'USEFLAG',NPRIGPS,   &
                        IERR,PRIUSEFLAG,MORE,NPRIGPSWOP+1)
      CALL UTL_FILTERLIST(LLPTRGPPRIORFP,IOUT,'PLOTSYMBOL',NPRIGPS,   &
                        IERR,PLOTSYMBOLPRIGRP,MORE,NPRIGPSWOP+1)
      CALL UTL_FILTERLIST(LLPTRGPPRIORFP,IOUT,'WTMULTIPLIER',NPRIGPS,   &
                        IERR,PRIWTMULTIPLIER,MORE,NPRIGPSWOP+1)
    ENDIF
    IF (GPSDEF) THEN
      IF(NPRIGPSWOP>0) THEN
        DO I=1,NPRIGPSWOP
          CVNAM = ' '
          CALL UTL_FILTER(IERR,LLPTRGPPRIOR,IOUT,'COVMATRIX',CVNAM,I)
          IF (CVNAM .NE. ' ') NCOVMAT = NCOVMAT+1
        ENDDO
      ENDIF
      IF(NPRIGPS>NPRIGPSWOP) THEN
        DO I=1,NPRIGPS-NPRIGPSWOP
          CVNAM = ' '
          CALL UTL_FILTER(IERR,LLPTRGPPRIORFP,IOUT,'COVMATRIX',CVNAM,I)
          IF (CVNAM .NE. ' ') NCOVMAT = NCOVMAT+1
        ENDDO
      ENDIF
    ENDIF
    !
    IF(IWRITE) WRITE(IOUT,100) MPR
    ALLOCATE (PLOTSYMBOLPRI(MPR),PRIEQN(NPE,MPR), &
              PRIEQTEXT(MPR),PRINAM(MPR),PRINAMLC(MPR), &
              PRICOVMATNAM(MPR),PRIGROUP(MPR), PRILN(MPR), &
              PRIVAL(MPR), PRIVARIANCE(MPR), PRIWTCORR(MPR), &
              WTDPRI(MPR))
    !   Assign defaults
    PLOTSYMBOLPRI = 1
    PRINAM = ' '
    PRICOVMATNAM = ' '
    PRIGROUP = 'DefaultPrior'
    PRILN = 0
    PRIWTCORR = .FALSE.
    PRIVAL = BIGDOUBLE
    PRIVARIANCE = BIGDOUBLE
    WTDPRI = BIGDOUBLE
    !
    RETURN
  END SUBROUTINE PRI_INI_READ_WPRED
  !-----------------------------------------------------------------------------
  SUBROUTINE PRI_INI_STORE(IOUT,MPR,NPE,NPT,ITRANS,PARNAMLC,PVAL,XPRIFILLED)
    !   Store data read from prior-information input blocks
    USE GLOBAL_DATA, ONLY: BIGDOUBLE, IVERB, MAX_STRING_LEN
    USE UTILITIES
    USE EQUATION, ONLY: EQN_LINEAR_COEFFS
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                           INTENT(IN)    :: IOUT
    INTEGER,                           INTENT(IN)    :: MPR
    INTEGER,                           INTENT(IN)    :: NPE
    INTEGER,                           INTENT(IN)    :: NPT
    INTEGER, DIMENSION(NPT),           INTENT(IN)    :: ITRANS
    CHARACTER(LEN=12), DIMENSION(NPT), INTENT(IN)    :: PARNAMLC ! Parameter names, lowercase
    DOUBLE PRECISION,  DIMENSION(NPT), INTENT(IN)    :: PVAL     ! Current parameter values
    LOGICAL,                           INTENT(INOUT) :: XPRIFILLED
    !
    !   Local variables
    INTEGER                          :: IEQ, IERR, IFAIL, J, MORE
    DOUBLE PRECISION                 :: CONST_TERM
    !!!!DOUBLE PRECISION, DIMENSION(NPT) :: WORK
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: WORK
    LOGICAL                          :: UFLAG
    CHARACTER(LEN=6)  :: STATFLAG
    DOUBLE PRECISION  :: STATISTIC, WEIGHT, WTMULTIPLIER
    !   Format statements
    80 FORMAT(1X,'Error: STATISTIC is undefined for prior-information',   &
              ' equation "',A,'"')
    100 FORMAT(1X,'Error: STATFLAG is undefined for prior-information',   &
               ' equation "',A,'"')
    !
    ! ALLOCATE
    ALLOCATE(WORK(NPT))
    !
    IERR = 0
    !
    IF (MPR > 0) THEN
      IF (IVERB>4) CALL UTL_WRITEBLOCK(LLPTRPRIOR,IOUT)
      !
      !
      !   Populate arrays from linked list
      CALL UTL_FILTERLIST(LLPTRPRIOR,IOUT,'PRIORNAME',MPR,IERR,PRINAM,MORE)
      CALL UTL_FILTERLIST(LLPTRPRIOR,IOUT,'PRIORINFOVALUE',MPR,IERR,PRIVAL,MORE)
      CALL UTL_FILTERLIST(LLPTRPRIOR,IOUT,'STATISTIC',MPR,IERR,PRIVARIANCE,MORE)
      CALL UTL_FILTERLIST(LLPTRPRIOR,IOUT,'GROUPNAME',MPR,IERR,PRIGROUP,MORE)
      CALL UTL_FILTERLIST(LLPTRPRIOR,IOUT,'PLOTSYMBOL',MPR,IERR, &
                          PLOTSYMBOLPRI,MORE)
      CALL UTL_FILTERLIST(LLPTRPRIOR,IOUT,'COVMATRIX',MPR,IERR,PRICOVMATNAM,   &
                          MORE)
      !   Process linked list and store equations
      IFAIL = 0
      DO IEQ=1,MPR
        !   Get current equation information
        CALL UTL_FILTER(IFAIL,LLPTRPRIOR,IOUT,'EQUATION',PRIEQTEXT(IEQ),IEQ)
        UFLAG = .TRUE.
        CALL UTL_FILTER(IFAIL,LLPTRPRIOR,IOUT,'USEFLAG',UFLAG,IEQ)
        !   Store coefficients for current equation
        IF (UFLAG) THEN
          CALL EQN_LINEAR_COEFFS(IFAIL,PRINAM(IEQ),PRIEQTEXT(IEQ),NPT,NPE,   &
                                 PARNAMLC,ITRANS,PVAL,CONST_TERM,   &
                                 WORK,PRIEQN(1:NPE,IEQ:IEQ))
          IF (IFAIL.NE.0) CALL UTL_STOP('EQN_LINEAR_COEFFS reports failure')
        ENDIF
        !   Process and store other data
        IF (PRICOVMATNAM(IEQ).NE.' ') THEN
          PRIWTCORR(IEQ) = .TRUE.
        ENDIF
        !   Populate PRIVARIANCE array
        STATISTIC = PRIVARIANCE(IEQ)
        STATFLAG = 'xxxxxx'
        WTMULTIPLIER = 1.0D0
        CALL UTL_FILTER(IFAIL,LLPTRPRIOR,IOUT,'WTMULTIPLIER',WTMULTIPLIER,IEQ)
        CALL UTL_FILTER(IFAIL,LLPTRPRIOR,IOUT,'STATFLAG',STATFLAG,IEQ)
        IF (STATISTIC==BIGDOUBLE) THEN
          WRITE(IOUT,80)TRIM(PRINAM(IEQ))
          IERR = 1
        ENDIF
        IF (STATFLAG=='xxxxxx') THEN
          WRITE(IOUT,100)TRIM(PRINAM(IEQ))
          IERR = 1
        ENDIF
        IF (IERR .NE. 0) CALL UTL_STOP()
        CALL UTL_CASETRANS(STATFLAG,'hi')
        CALL UTL_CALCWT(IOUT,PRINAM(IEQ),STATFLAG,STATISTIC,PRIVAL(IEQ),   &
                            WTMULTIPLIER,IERR,WEIGHT,PRIVARIANCE(IEQ))
        IF (IERR.NE.0) CALL UTL_STOP()
        !   Count number of members in each group
        DO J=1,NPRIGPS
          IF (UTL_SAMENAME(PRIGROUPNAM(J),PRIGROUP(IEQ))) THEN
            NUMINPRIGROUP(J) = NUMINPRIGROUP(J)+1
          ENDIF
        ENDDO
      ENDDO
      XPRIFILLED = .FALSE.
    ENDIF
    !
    ! DEALLOCATE
    DEALLOCATE(WORK)
    !
    RETURN
  END SUBROUTINE PRI_INI_STORE
  !-----------------------------------------------------------------------------
  SUBROUTINE PRI_INI_STORE_WPRED(IOUT,MPR,NPE,NPT,ITRANS,PARNAMLC,PVAL, &
                           XPRIFILLED,CONVERT_STAT,PRED,MPRWOPPAS,CONVERTED)
    !   Store data read from prior-information input blocks
    USE GLOBAL_DATA, ONLY: BIGDOUBLE, IVERB, MAX_STRING_LEN
    USE UTILITIES
    USE EQUATION, ONLY: EQN_LINEAR_COEFFS
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                           INTENT(IN)    :: IOUT
    INTEGER,                           INTENT(IN)    :: MPR
    INTEGER,                           INTENT(IN)    :: NPE
    INTEGER,                           INTENT(IN)    :: NPT
    INTEGER, DIMENSION(NPT),           INTENT(IN)    :: ITRANS
    CHARACTER(LEN=12), DIMENSION(NPT), INTENT(IN)    :: PARNAMLC ! Parameter names, lowercase
    DOUBLE PRECISION,  DIMENSION(NPT), INTENT(IN)    :: PVAL     ! Current parameter values
    LOGICAL,                           INTENT(INOUT) :: XPRIFILLED
    LOGICAL,                           INTENT(INOUT) :: CONVERT_STAT
    LOGICAL,                           INTENT(IN)    :: PRED
    INTEGER,                           INTENT(IN)    :: MPRWOPPAS
    LOGICAL, OPTIONAL, DIMENSION(MPR), INTENT(INOUT) :: CONVERTED
    !
    !   Local variables
    INTEGER                          :: I, IEQ, IERR, IFAIL, J, MORE
    DOUBLE PRECISION                 :: CONST_TERM
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: WORK
    LOGICAL                          :: UFLAG
    CHARACTER(LEN=6)  :: STATFLAG
    DOUBLE PRECISION  :: STATISTIC, WEIGHT, WTMULTIPLIER
    CHARACTER(LEN=MAX_STRING_LEN) :: PRITEXTTMP
    !   Format statements
    80 FORMAT(1X,'Error: STATISTIC is undefined for prior-information',   &
              ' equation "',A,'"')
    100 FORMAT(1X,'Error: STATFLAG is undefined for prior-information',   &
               ' equation "',A,'"')
    !
    ! ALLOCATE
    ALLOCATE(WORK(NPT))
    !
    IERR = 0
    !
    IF (MPR > 0) THEN
      IF (IVERB>4) CALL UTL_WRITEBLOCK(LLPTRPRIOR,IOUT)
      !
      !   Populate arrays from linked list
      CALL UTL_FILTERLIST(LLPTRPRIOR,IOUT,'PRIORNAME',MPR,IERR,PRINAM,MORE)
      CALL UTL_FILTERLIST(LLPTRPRIOR,IOUT,'PRIORINFOVALUE',MPR,IERR,PRIVAL,MORE)
      CALL UTL_FILTERLIST(LLPTRPRIOR,IOUT,'STATISTIC',MPR,IERR,PRIVARIANCE,MORE)
      CALL UTL_FILTERLIST(LLPTRPRIOR,IOUT,'GROUPNAME',MPR,IERR,PRIGROUP,MORE)
      CALL UTL_FILTERLIST(LLPTRPRIOR,IOUT,'PLOTSYMBOL',MPR,IERR, &
                          PLOTSYMBOLPRI,MORE)
      CALL UTL_FILTERLIST(LLPTRPRIOR,IOUT,'COVMATRIX',MPR,IERR,PRICOVMATNAM,   &
                          MORE)
      IF(PRED .AND. MPR>MPRWOPPAS) THEN
        IF (IVERB>4) CALL UTL_WRITEBLOCK(LLPTRPRIORFP,IOUT)
        !   Populate arrays from linked list
        CALL UTL_FILTERLIST(LLPTRPRIORFP,IOUT,'PRIORNAME',MPR,IERR,PRINAM, &
                            MORE,MPRWOPPAS+1)
        CALL UTL_FILTERLIST(LLPTRPRIORFP,IOUT,'PRIORINFOVALUE',MPR,IERR, &
                            PRIVAL,MORE,MPRWOPPAS+1)
        CALL UTL_FILTERLIST(LLPTRPRIORFP,IOUT,'STATISTIC',MPR,IERR, &
                            PRIVARIANCE,MORE,MPRWOPPAS+1)
        CALL UTL_FILTERLIST(LLPTRPRIORFP,IOUT,'GROUPNAME',MPR,IERR, &
                            PRIGROUP,MORE,MPRWOPPAS+1)
        CALL UTL_FILTERLIST(LLPTRPRIORFP,IOUT,'PLOTSYMBOL',MPR,IERR, &
                            PLOTSYMBOLPRI,MORE,MPRWOPPAS+1)
        CALL UTL_FILTERLIST(LLPTRPRIORFP,IOUT,'COVMATRIX',MPR,IERR, &
                            PRICOVMATNAM,MORE,MPRWOPPAS+1)
      ENDIF
      !   Process linked list and store equations
      IFAIL = 0
      I = 1
      IF(MPRWOP>0)THEN
        DO IEQ=1,MPRWOP
          !   Get current equation information
          CALL UTL_FILTER(IFAIL,LLPTRPRIOR,IOUT,'EQUATION',PRIEQTEXT(IEQ),IEQ)
          CALL UTL_CASETRANS(PRIEQTEXT(IEQ),'lo') ! Store names as lowercase
          UFLAG = .TRUE.
          CALL UTL_FILTER(IFAIL,LLPTRPRIOR,IOUT,'USEFLAG',UFLAG,IEQ)
          !   Store coefficients for current equation
          IF (UFLAG) THEN
            IF(CONVERT_STAT) THEN
              PRITEXTTMP = ' '
              CALL EQN_LINEAR_COEFFS(IFAIL,PRINAM(IEQ),PRIEQTEXT(IEQ),NPT,NPE, &
                             PARNAMLC,ITRANS,PVAL,CONST_TERM,WORK, &
                             PRIEQN(1:NPE,IEQ:IEQ),PRITEXTTMP,CONVERT_STAT)
              IF(PRESENT(CONVERTED)) CONVERTED(IEQ) = CONVERT_STAT
              CONVERT_STAT = .TRUE.
              PRIEQTEXT(IEQ) = PRITEXTTMP
            ELSE
              CALL EQN_LINEAR_COEFFS(IFAIL,PRINAM(IEQ),PRIEQTEXT(IEQ),NPT,NPE,   &
                                  PARNAMLC,ITRANS,PVAL,CONST_TERM,WORK, &
                                  PRIEQN(1:NPE,IEQ:IEQ))
            ENDIF
            IF (IFAIL.NE.0) CALL UTL_STOP('EQN_LINEAR_COEFFS reports failure')
          ENDIF
          !   Process and store other data
          IF (PRICOVMATNAM(IEQ).NE.' ') THEN
            PRIWTCORR(IEQ) = .TRUE.
          ENDIF
          !   Populate PRIVARIANCE array
          STATISTIC = PRIVARIANCE(IEQ)
          STATFLAG = 'xxxxxx'
          WTMULTIPLIER = 1.0D0
          CALL UTL_FILTER(IFAIL,LLPTRPRIOR,IOUT,'WTMULTIPLIER',WTMULTIPLIER,IEQ)
          CALL UTL_FILTER(IFAIL,LLPTRPRIOR,IOUT,'STATFLAG',STATFLAG,IEQ)
          IF (STATISTIC==BIGDOUBLE) THEN
            WRITE(IOUT,80)TRIM(PRINAM(IEQ))
            IERR = 1
          ENDIF
          IF (STATFLAG=='xxxxxx') THEN
            WRITE(IOUT,100)TRIM(PRINAM(IEQ))
            IERR = 1
          ENDIF
          IF (IERR .NE. 0) CALL UTL_STOP()
          CALL UTL_CALCWT(IOUT,PRINAM(IEQ),STATFLAG,STATISTIC,PRIVAL(IEQ),   &
                              WTMULTIPLIER,IERR,WEIGHT,PRIVARIANCE(IEQ))
          IF (IERR.NE.0) CALL UTL_STOP()
          !   Count number of members in each group
          DO J=1,NPRIGPSWOP
            IF (UTL_SAMENAME(PRIGROUPNAM(J),PRIGROUP(IEQ))) THEN
              NUMINPRIGROUP(J) = NUMINPRIGROUP(J)+1
            ENDIF
          ENDDO
        ENDDO
        !START AT IEQ+1
        I = IEQ
      ENDIF
      IF(MPR>MPRWOP)THEN
        DO IEQ=I,MPR
          !   Get current equation information
          CALL UTL_FILTER(IFAIL,LLPTRPRIORFP,IOUT,'EQUATION',PRIEQTEXT(IEQ), &
                          IEQ-MPRWOP)
          UFLAG = .TRUE.
          CALL UTL_FILTER(IFAIL,LLPTRPRIORFP,IOUT,'USEFLAG',UFLAG,IEQ-MPRWOP)
          !   Store coefficients for current equation
          IF (UFLAG) THEN
            IF(CONVERT_STAT) THEN
              PRITEXTTMP = ' '
              CALL EQN_LINEAR_COEFFS(IFAIL,PRINAM(IEQ),PRIEQTEXT(IEQ),NPT,NPE, &
                             PARNAMLC,ITRANS,PVAL,CONST_TERM,WORK, &
                             PRIEQN(1:NPE,IEQ:IEQ),PRITEXTTMP,CONVERT_STAT)
              IF(PRESENT(CONVERTED)) CONVERTED(IEQ) = CONVERT_STAT
              CONVERT_STAT = .TRUE.
              PRIEQTEXT(IEQ) = PRITEXTTMP
            ELSE
              CALL EQN_LINEAR_COEFFS(IFAIL,PRINAM(IEQ),PRIEQTEXT(IEQ),NPT,NPE,   &
                                  PARNAMLC,ITRANS,PVAL,CONST_TERM,WORK, &
                                  PRIEQN(1:NPE,IEQ:IEQ))
            ENDIF
            IF (IFAIL.NE.0) CALL UTL_STOP('EQN_LINEAR_COEFFS reports failure')
          ENDIF
          !   Process and store other data
          IF (PRICOVMATNAM(IEQ).NE.' ') THEN
            PRIWTCORR(IEQ) = .TRUE.
          ENDIF
          !   Populate PRIVARIANCE array
          STATISTIC = PRIVARIANCE(IEQ)
          STATFLAG = 'xxxxxx'
          WTMULTIPLIER = 1.0D0
          CALL UTL_FILTER(IFAIL,LLPTRPRIORFP,IOUT,'WTMULTIPLIER',WTMULTIPLIER, &
                          IEQ-MPRWOP)
          CALL UTL_FILTER(IFAIL,LLPTRPRIORFP,IOUT,'STATFLAG',STATFLAG, &
                          IEQ-MPRWOP)
          IF (STATISTIC==BIGDOUBLE) THEN
            WRITE(IOUT,80)TRIM(PRINAM(IEQ))
            IERR = 1
          ENDIF
          IF (STATFLAG=='xxxxxx') THEN
            WRITE(IOUT,100)TRIM(PRINAM(IEQ))
            IERR = 1
          ENDIF
          IF (IERR .NE. 0) CALL UTL_STOP()
          CALL UTL_CASETRANS(STATFLAG,'hi')
          CALL UTL_CALCWT(IOUT,PRINAM(IEQ),STATFLAG,STATISTIC,PRIVAL(IEQ),   &
                              WTMULTIPLIER,IERR,WEIGHT,PRIVARIANCE(IEQ))
          IF (IERR.NE.0) CALL UTL_STOP()
          !   Count number of members in each group
          DO J=1,NPRIGPS
            IF (UTL_SAMENAME(PRIGROUPNAM(J),PRIGROUP(IEQ))) THEN
              NUMINPRIGROUP(J) = NUMINPRIGROUP(J)+1
            ENDIF
          ENDDO
        ENDDO
      ENDIF
      XPRIFILLED = .FALSE.
    ENDIF
    !
    ! DEALLOCATE
    DEALLOCATE(WORK)
    !
    RETURN
  END SUBROUTINE PRI_INI_STORE_WPRED
  !-----------------------------------------------------------------------------
  SUBROUTINE PRI_INI_WRITEPRIS(IOUT,MPR)
    !   Write a prior set as a table
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: BLANKS, HYPHENS
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(IN) :: IOUT
    INTEGER, INTENT(IN) :: MPR
    !
    !   Local variables
    INTEGER :: I
    !
    !   Format statements
    200 FORMAT(/,1X,'NAME',A,2X,'GROUP',9X,'VALUE',9X,'WEIGHT',/,    &
        1X,A,2X,12('-'),2X,12('-'),2X,12('-'))
    210 FORMAT(1X,A,2X,A,2X,G12.5,2X,G12.5)
    220 FORMAT(1X,A,2X,A,2X,G12.5,5X,'matrix')
    !
    WRITE(IOUT,200) BLANKS(1:LENDNAM-4),HYPHENS(1:LENDNAM)
    DO I=1,MPR
      ! AS written now Prior must have full weight matrix
      IF (.NOT. PRIWTCORR(I)) THEN
        WRITE(IOUT,210)PRINAM(I),PRIGROUP(I),PRIVAL(I),UTL_GETVAL(PRIWTMAT,I,I)
      ELSE
        WRITE(IOUT,220)PRINAM(I),PRIGROUP(I),PRIVAL(I)
      ENDIF
    ENDDO
    !
    RETURN
  END SUBROUTINE PRI_INI_WRITEPRIS
  !-----------------------------------------------------------------------------
  SUBROUTINE PRI_INI_WRITEPRIGRPS(IOUT)
    !   Write group data as a table
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(IN) :: IOUT
    !
    !   Local variables
    INTEGER :: I
    CHARACTER(LEN=3) :: USETAG = 'NO '
    !
    !   Format statements
    200 FORMAT(/,   &
        15X,'NUM. OF',9X,'PLOT',5X,'WEIGHT',4X,'COV. MATRIX,',/,    &
        1X,'GROUP NAME',4X,'MEMBERS',2X,'USE?',2X,'SYMBOL',2X,   &
        'MULTIPLIER',2X,'IF CORRELATED',/,1X,12('-'),    &
        2X,7('-'),2X,4('-'),2X,6('-'),2X,10('-'),2X,13('-'))
    210 FORMAT(1X,A,2X,I7,2X,A3,3X,I6,1X,G11.3,2X,A)
    !
    !   Write data for each group
    WRITE(IOUT,200)
    DO I=1,NPRIGPS
      IF(PRIUSEFLAG(I))USETAG='YES'
      WRITE(IOUT,210) PRIGROUPNAM(I),NUMINPRIGROUP(I),USETAG,   &
          PLOTSYMBOLPRIGRP(I),PRIWTMULTIPLIER(I),PRICOVMATNAM(I)
      USETAG='NO '
    ENDDO
    !
    RETURN
  END SUBROUTINE PRI_INI_WRITEPRIGRPS
  !-----------------------------------------------------------------------------
  SUBROUTINE PRI_DEF(NUMPPL)
    IMPLICIT NONE
    !
    !   Argument-list variable
    INTEGER, INTENT(OUT) :: NUMPPL
    !
    NUMPPL = 0
    RETURN
  END SUBROUTINE PRI_DEF
  !-----------------------------------------------------------------------------
  SUBROUTINE PRI_UEV_DX_READ_PR(MPR,OUTNAM,PLOTSYM,PRIEQNTXT,PRINAME,PRIVAL)
    !     READ _PR DX FILE Prior information equations
    USE GLOBAL_DATA, ONLY: LENDNAM, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                       INTENT(IN)  :: MPR
    CHARACTER(LEN=MAX_STRING_LEN),                 INTENT(IN)  :: OUTNAM
    INTEGER, DIMENSION(MPR),                       INTENT(OUT) :: PLOTSYM
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(MPR), INTENT(OUT) :: PRIEQNTXT
    CHARACTER(LEN=LENDNAM), DIMENSION(MPR),        INTENT(OUT) :: PRINAME
    DOUBLE PRECISION, DIMENSION(MPR),              INTENT(OUT) :: PRIVAL
    !
    !   Local variables
    INTEGER                                                :: I
    INTEGER                                                :: IUPR
    !
    IUPR = UTL_DX_OPEN(OUTNAM,'_pr','OLD')
    ! READ Header
    READ (IUPR,*,END=100)
    DO I=1,MPR
      READ (IUPR,*,END=100)PRINAME(I),PLOTSYM(I),PRIEQNTXT(I),PRIVAL(I)
    ENDDO
    IUPR = UTL_DX_CLOSE('_pr')
    !
    RETURN
    100 IUPR = UTL_DX_CLOSE('_pr')
    CALL UTL_STOP('UNEXPECTED CONTENT _pr file, CHECK VERSION/RE-CREATE _pr')
    RETURN
  END SUBROUTINE PRI_UEV_DX_READ_PR
  !-----------------------------------------------------------------------------
  SUBROUTINE PRI_UEV_DX_WRITE_PR(MPR,OUTNAM)
    !     VERSION 20040723 EPP
    !     ******************************************************************
    !     WRITE _PR DATA-EXCHANGE FILE: Prior-information equations
    !     ******************************************************************
    !        SPECIFICATIONS:
    !     ------------------------------------------------------------------
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                               INTENT(IN) :: MPR
    CHARACTER(LEN=*),                      INTENT(IN) :: OUTNAM
    !
    ! Local variables
    INTEGER :: I
    INTEGER :: IUPR
    !
    !   Format statements
    100 FORMAT (1X,'"PRIOR NAME" "PLOT SYMBOL" "EQUATION" ',   &
                '"NATIVE SPACE PRIOR VALUE"')
    200 FORMAT(1X,A,1X,I6,5X,'"',A,'"',1X,G14.7)
    !
    IF(MPR > 0) THEN
      ! Open Data exchange Files
      IUPR = UTL_DX_OPEN(OUTNAM,'_pr','REPLACE')
      WRITE(IUPR,100)
      ! Write  data
      DO 300 I = 1, MPR
        CALL UTL_CASETRANS(PRIEQTEXT(I),'lo')
        IF(PRILN(I) .EQ. 1) THEN
          WRITE (IUPR,200) PRINAM(I),PLOTSYMBOLPRI(I),TRIM(PRIEQTEXT(I)), &
                           EXP(PRIVAL(I))
        ELSE
          WRITE (IUPR,200) PRINAM(I),PLOTSYMBOLPRI(I),TRIM(PRIEQTEXT(I)), &
                           PRIVAL(I)
        ENDIF
      300 CONTINUE
      ! Close Data exchange Files
      IUPR = UTL_DX_CLOSE('_pr')
    ENDIF
    RETURN
  END SUBROUTINE PRI_UEV_DX_WRITE_PR
  !-----------------------------------------------------------------------------
  SUBROUTINE PRI_UEV_DX_READ_PRP(MPRFP,OUTNAM,PLOTSYM,PRIEQNTXT,PRINAME,PRIVAL)
    !     READ _PRP DX FILE Prior information equations
    USE GLOBAL_DATA, ONLY: LENDNAM, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                         INTENT(IN)  :: MPRFP
    CHARACTER(LEN=MAX_STRING_LEN),                   INTENT(IN)  :: OUTNAM
    INTEGER, DIMENSION(MPRFP),                       INTENT(OUT) :: PLOTSYM
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(MPRFP), INTENT(OUT) :: PRIEQNTXT
    CHARACTER(LEN=LENDNAM), DIMENSION(MPRFP),        INTENT(OUT) :: PRINAME
    DOUBLE PRECISION, DIMENSION(MPRFP),              INTENT(OUT) :: PRIVAL
    !
    !   Local variables
    INTEGER                                                :: I
    INTEGER                                                :: IUPR
    !
    IUPR = UTL_DX_OPEN(OUTNAM,'_prp','OLD')
    ! READ Header
    READ (IUPR,*,END=100)
    DO I=1,MPRFP
      READ (IUPR,*,END=100)PRINAME(I),PLOTSYM(I),PRIEQNTXT(I),PRIVAL(I)
    ENDDO
    IUPR = UTL_DX_CLOSE('_prp')
    !
    RETURN
    100 IUPR = UTL_DX_CLOSE('_prp')
    CALL UTL_STOP('UNEXPECTED CONTENT _pr file, CHECK VERSION/RE-CREATE _prp')
    RETURN
  END SUBROUTINE PRI_UEV_DX_READ_PRP
  !-----------------------------------------------------------------------------
  SUBROUTINE PRI_UEV_DX_WRITE_PRP(MPR,MPRWOPPAS,OUTNAM)
    !     VERSION 20040723 EPP
    !     ******************************************************************
    !     WRITE _PRP DATA-EXCHANGE FILE: Prior-information equations
    !                     related to prior added only for prediction
    !     ******************************************************************
    !        SPECIFICATIONS:
    !     ------------------------------------------------------------------
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                               INTENT(IN) :: MPR
    INTEGER,                               INTENT(IN) :: MPRWOPPAS
    CHARACTER(LEN=*),                      INTENT(IN) :: OUTNAM
    !
    ! Local variables
    INTEGER :: I
    INTEGER :: IUPR
    !
    !   Format statements
    100 FORMAT (1X,'"PRIOR NAME" "PLOT SYMBOL" "EQUATION" ',   &
                '"NATIVE SPACE PRIOR VALUE"')
    200 FORMAT(1X,A,1X,I6,5X,'"',A,'"',1X,G14.7)
    !
    IF(MPR > 0) THEN
      ! Open Data exchange Files
      IUPR = UTL_DX_OPEN(OUTNAM,'_prp','REPLACE')
      WRITE(IUPR,100)
      ! Write  data
      DO 300 I = MPRWOPPAS+1, MPR
        IF(PRILN(I) .EQ. 1) THEN
          WRITE (IUPR,200) PRINAM(I),PLOTSYMBOLPRI(I),TRIM(PRIEQTEXT(I)), &
                           EXP(PRIVAL(I))
        ELSE
          WRITE (IUPR,200) PRINAM(I),PLOTSYMBOLPRI(I),TRIM(PRIEQTEXT(I)), &
                           PRIVAL(I)
        ENDIF
      300 CONTINUE
      ! Close Data exchange Files
      IUPR = UTL_DX_CLOSE('_prp')
    ENDIF
    RETURN
  END SUBROUTINE PRI_UEV_DX_WRITE_PRP
  !-----------------------------------------------------------------------------
  SUBROUTINE PRI_UEV_RESIDUALS(IOUT,MPR,NEQNPTR,NPT,PARNAMLC,PVAL)
    !   Calculate residuals and weighted residuals, using a full weight matrix.
    USE EQUATION, ONLY: EQN_EVALUATE
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                           INTENT(IN) :: IOUT
    INTEGER,                           INTENT(IN) :: MPR
    INTEGER,                           INTENT(IN) :: NEQNPTR
    INTEGER,                           INTENT(IN) :: NPT
    CHARACTER(LEN=12), DIMENSION(NPT), INTENT(IN) :: PARNAMLC ! Parameter names, lowercase
    DOUBLE PRECISION,  DIMENSION(NPT), INTENT(IN) :: PVAL  ! Simulated equivalent
    !
    !   Local variables
    LOGICAL :: EQNLVAL
    INTEGER :: I
    INTEGER :: IFAIL
    INTEGER :: ITYP
    INTEGER :: J
    !
    !   Calculate residuals for prior
    DO I=1,MPR
      J = NEQNPTR + I
      CALL EQN_EVALUATE(IFAIL,J,NPT,PARNAMLC,PVAL,ITYP,MODELPRIVAL(I),EQNLVAL)
      IF (IFAIL.NE.0) THEN
        CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
        CALL UTL_STOP('EQN_EVALUATE failed evaluating a prior equation')
      ENDIF
      IF(PRILN(I) .EQ. 1)MODELPRIVAL(I) = MODELPRIVAL(I)*2.30258509299405D0
      RESIDSPRI(I) = PRIVAL(I)- MODELPRIVAL(I)
    ENDDO
    !   Calculate weighted residuals for prior
    !WTDRESIDSPRI = UTL_MATMULVEC(MPR,PRIWTMATSQR,RESIDSPRI)
    CALL UTL_MATMULVEC_SUB(MPR,PRIWTMATSQR,RESIDSPRI,WTDRESIDSPRI)
    RETURN
  END SUBROUTINE PRI_UEV_RESIDUALS
  !-----------------------------------------------------------------------------
  SUBROUTINE PRI_CLN()
    !   Deallocate all arrays, linked lists, and CDMATRIX structures in the
    !   PriorInformation module
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Deallocate arrays
    IF (ALLOCATED(PRIEQTEXT)) DEALLOCATE(PRIEQTEXT)
    IF (ALLOCATED(NUMINPRIGROUP)) DEALLOCATE(NUMINPRIGROUP)
    IF (ALLOCATED(PRICOVMATNAM)) DEALLOCATE(PRICOVMATNAM)
    IF (ALLOCATED(PRIGROUP)) DEALLOCATE(PRIGROUP)
    IF (ALLOCATED(PRIGROUPNAM)) DEALLOCATE(PRIGROUPNAM)
    IF (ALLOCATED(PRIMEMBER)) DEALLOCATE(PRIMEMBER)
    IF (ALLOCATED(PRINAM)) DEALLOCATE(PRINAM)
    IF (ALLOCATED(PRIVAL)) DEALLOCATE(PRIVAL)
    IF (ALLOCATED(PRIVARIANCE)) DEALLOCATE(PRIVARIANCE)
    IF (ALLOCATED(PRIWTCORR)) DEALLOCATE(PRIWTCORR)
    IF (ALLOCATED(WTDPRI)) DEALLOCATE(WTDPRI)
    IF (ALLOCATED(PRIEQN)) DEALLOCATE(PRIEQN)
    !
    !   Deallocate linked lists
    CALL TYP_DEALLOC(LLPTRGPPRIOR)
    CALL TYP_DEALLOC(LLPTRPRIOR)
    !
    !   Deallocate CDMATRIX structures
    CALL TYP_DEALLOC(PRIWTMAT)
    CALL TYP_DEALLOC(PRIWTMATSQR)
    !
    RETURN
  END SUBROUTINE PRI_CLN
  !-----------------------------------------------------------------------------
  !   PRIOR_INFORMATION-module utilities
  !-----------------------------------------------------------------------------
  SUBROUTINE PRI_DX_WRITE_GM(IUGM,MPR,PRINAM)
    !   Add to _gm Data-Exchange file: Group names and member names
    !   Assumes iugm _gm file is open and ready to write after deps written
    USE GLOBAL_DATA, ONLY: AMESSAGE, BIGREAL, LENDNAM, IVERB, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: IUGM
    INTEGER,                                 INTENT(IN) :: MPR
    CHARACTER(LEN=LENDNAM), DIMENSION(MPR),  INTENT(IN) :: PRINAM
    !
    !   Local variables
    INTEGER :: IPLOT, N
    CHARACTER(LEN=12) :: GPNAM
    !
    !   Format statements
    520 FORMAT(A,3X,A,2X,I10)
    IF (IUGM .LE. 0) THEN
      AMESSAGE = 'Warning: IUGM<=0 in PRI_DX_WRITE_GM;'//   &
                 ' prior-inforamtion not written to _gm file"'
      CALL UTL_WRITE_MESSAGE()
      RETURN
    ENDIF
    !   Write group name, member name, plot symbol
    EACHMPR: DO N=1,MPR
      CALL PRI_GET_GROUP(N,GPNAM,IPLOT)
      WRITE(IUGM,520) GPNAM,PRINAM(N),IPLOT
    ENDDO EACHMPR
    !
    RETURN
  END SUBROUTINE PRI_DX_WRITE_GM
  !-----------------------------------------------------------------------------
  SUBROUTINE PRI_GET_GROUP(N,GPNAM,IPLOT)
    !   Return group name and plot symbol, given a prior information number
    USE GLOBAL_DATA, ONLY: AMESSAGE
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                    INTENT(IN)  :: N
    CHARACTER(LEN=12),          INTENT(OUT) :: GPNAM
    INTEGER,                    INTENT(OUT) :: IPLOT
    !
    !   Local variables
    !
    GPNAM = ' '
    IPLOT = -1
    GPNAM = PRIGROUP(N)
    IPLOT = PRI_GET_PLOTSYMBOL(GPNAM,IPLOT)
    !
    RETURN
  END SUBROUTINE PRI_GET_GROUP
  !-----------------------------------------------------------------------------
  FUNCTION PRI_GET_PLOTSYMBOL(GNAM,IDEFAULT) RESULT(IPLOT)
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
    EACHGROUP: DO I=1,NPRIGPS
      IF (GNAM.EQ.PRIGROUPNAM(I)) THEN
        IPLOT = PLOTSYMBOLPRI(I)
        EXIT EACHGROUP
      ENDIF
    ENDDO EACHGROUP
    RETURN
  END FUNCTION PRI_GET_PLOTSYMBOL
  !-----------------------------------------------------------------------------
  !
END MODULE PRIOR_INFORMATION
