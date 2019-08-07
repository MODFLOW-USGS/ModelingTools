MODULE SENSITIVITY
  USE GLOBAL_DATA, ONLY: LENDNAM, MAX_STRING_LEN
  USE DATATYPES
  PRIVATE
  !
  !   PUBLIC DATA
  !
  !   PUBLIC SUBPROGRAMS
  PUBLIC SEN_INI, SEN_DEF, SEN_GEN, SEN_EXE_SELECT, SEN_UEV_DX_WRITE_SEN, &
         SEN_UEV_LNX, SEN_UEV_POPX_MODCALC, SEN_UEV_POPXROW_DIFF, &
         SEN_UEV_DX_READ_MATRIX,   &
         SEN_UEV_DX_READ_SU, SEN_UEV_DX_WRITE_MATRIX, &
         SEN_UEV_WRITESENTABLE, SEN_CLN
  !
  !   PRIVATE DATA
  !
  !   Variables for calculation of sensitivities
  INTEGER, ALLOCATABLE, DIMENSION(:)             :: IDONE ! Flag indicating if row in X populated for a parameter
  INTEGER                                        :: ISENPERT   ! 0 for model-calc'd sens.; 1 for perturb. sens.
  DOUBLE PRECISION,  ALLOCATABLE, DIMENSION(:)   :: DELB       ! Delta-B amount, as written to model input file
  DOUBLE PRECISION,  ALLOCATABLE, DIMENSION(:)   :: PVALFWD    ! Forward-perturbed parameter value
  INTEGER                                        :: NPARDONE   ! Number of parameters for which sensitivities have been calculated
  !
  !   Derivatives Interface (DI) information
  CHARACTER(LEN=MAX_STRING_LEN) :: DI_DERFILE   ! Derivatives file name
  INTEGER :: DI_NSKIP   ! Number of lines to be skipped before reading derivatives
  INTEGER :: DI_NDEP    ! Number of dependents in derivatives file
  INTEGER :: DI_NPAR    ! Number of parameters in derivatives file
  CHARACTER(LEN=10)                                 :: DI_ORIENTATION ! Orientation
  CHARACTER(LEN=MAX_STRING_LEN)                     :: DI_DERFORMAT   ! Format for reading derivatives
  CHARACTER(LEN=12),      ALLOCATABLE, DIMENSION(:) :: DI_PARNAM  ! Parameter names, as ordered in derivatives file
  CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: DI_DEPNAM  ! Dependent names, as ordered in derivatives file
  INTEGER,                ALLOCATABLE, DIMENSION(:) :: DI_XROW ! Pointer to row in X array
  INTEGER,                ALLOCATABLE, DIMENSION(:) :: DI_XCOL ! Pointer to column in X array
  !
CONTAINS
  !-----------------------------------------------------------------------------
  SUBROUTINE SEN_INI(DERIV_INTERFACE,IOUT,NDEP,NPE,NPT,IPTR,ISENMETHOD,   &
                     PARNAM,DEPNAM,IFAIL)
    !   Allocate memory for forward- and backward-difference model-calculated
    !   values, and for perturbation amount for each parameter
    USE GLOBAL_DATA, ONLY: AMESSAGE, LENDNAM, MAX_STRING_LEN
    USE DATATYPES
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=MAX_STRING_LEN),           INTENT(IN)    :: DERIV_INTERFACE
    INTEGER,                                 INTENT(IN)    :: IOUT
    INTEGER,                                 INTENT(IN)    :: NDEP   ! Number of dependents
    INTEGER,                                 INTENT(IN)    :: NPE    ! Number of parameters with estimate=yes
    INTEGER,                                 INTENT(IN)    :: NPT    ! Number of parameters, total
    INTEGER, DIMENSION(NPE),                 INTENT(IN)    :: IPTR   ! Pointers to adjustable parameters
    INTEGER, DIMENSION(NPT),                 INTENT(IN)    :: ISENMETHOD
    CHARACTER(LEN=12), DIMENSION(NPT),       INTENT(IN)    :: PARNAM ! Parameter names
    CHARACTER(LEN=LENDNAM), DIMENSION(NDEP), INTENT(IN)    :: DEPNAM ! Dependent names
    INTEGER,                                 INTENT(OUT)   :: IFAIL
    !
    !   Local variables
    INTEGER :: IERR, IFAIL1, IP, IPTRI, N
    CHARACTER(LEN=12) :: UPNAME1, UPNAME2
    !
    !   Format statements
    130 FORMAT(/,1X,'ERROR: ISENMETHOD specified as 0 for parameter "',   &
        A,'" but this',/,' parameter is not listed in the',   &
        ' DERIVATIVES INTERFACE file:',/,' "',A,'" (SEN_INI)')
    140 FORMAT(/,1X,'ERROR: ISENMETHOD specified as 0 for parameter "',   &
        A,'" but this',/,' parameter is not listed in a',   &
        ' DERIVATIVES INTERFACE file (SEN_INI)')
    !
    IFAIL = 0
    IFAIL1 = 0
    NPARDONE = 0
    !
    IF (NPE .GT. 0) THEN
      ALLOCATE (DELB(NPE),IDONE(NPE),PVALFWD(NPE),STAT=IFAIL1)
    ENDIF
    !
    IF (IFAIL1.NE.0) THEN
      AMESSAGE = 'Subroutine SENINIALLOC error: Unable to allocate memory'
      IFAIL1 = 1
      RETURN
    ENDIF
    !
    !   Read Derivatives Interface file
    IF (DERIV_INTERFACE .NE. ' ') THEN
      CALL SEN_INI_DI(IOUT,NDEP,NPE,NPT,IPTR,PARNAM,DEPNAM)
    ENDIF
    !
    IERR = 0
    DOIP: DO IP=1,NPE
      IPTRI = IPTR(IP)
      IF (ISENMETHOD(IPTRI)==0) THEN
        !   Check that parameter is on the DI_PARNAM list
        CALL UTL_CASE(PARNAM(IPTRI),UPNAME1,1)
        IF (DERIV_INTERFACE .NE. ' ') THEN
          DO N=1,DI_NPAR
            CALL UTL_CASE(DI_PARNAM(N),UPNAME2,1)
            IF (UPNAME1==UPNAME2) CYCLE DOIP
          ENDDO
          WRITE(IOUT,130)TRIM(PARNAM(IPTRI)),TRIM(DERIV_INTERFACE)
        ELSE
          WRITE(IOUT,140)TRIM(PARNAM(IPTRI))
        ENDIF
        IERR = 1
      ENDIF
    ENDDO DOIP
    IF (IERR>0) CALL UTL_STOP(' ')
    !
    IDONE = -1
    !
    RETURN
  END SUBROUTINE SEN_INI
  !-----------------------------------------------------------------------------
  SUBROUTINE SEN_INI_DI(IOUT,NDEP,NPE,NPT,IPTR,PARNAM,DEPNAM)
    !   Read Derivatives Interface file and set up protocol for reading
    !   model-calculated derivatives.
    USE GLOBAL_DATA, ONLY: LENDNAM, MAX_STRING_LEN
    USE UTILITIES
    USE BASIC, ONLY: DERIV_INTERFACE
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: IOUT
    INTEGER,                                 INTENT(IN) :: NDEP   ! Number of dependents
    INTEGER,                                 INTENT(IN) :: NPE    ! Number of parameters with estimate=yes
    INTEGER,                                 INTENT(IN) :: NPT    ! Number of parameters, total
    INTEGER, DIMENSION(NPE),                 INTENT(IN) :: IPTR   ! Pointers to adjustable parameters
    CHARACTER(LEN=12), DIMENSION(NPT),       INTENT(IN) :: PARNAM ! Parameter names
    CHARACTER(LEN=LENDNAM), DIMENSION(NDEP), INTENT(IN) :: DEPNAM ! Dependent names
    !
    !   Local variables
    INTEGER :: I, IDIUNIT, IERR, ILOC, INUMBER, ISTART, ISTAT, ISTOP, KCHK,   &
               KERR, NPOS
    LOGICAL :: LEX, LOP
    CHARACTER(LEN=MAX_STRING_LEN) :: LINE
    CHARACTER(LEN=12) :: DUMMY, PN1, PN1UP, PN2, PN2UP
    CHARACTER(LEN=LENDNAM) :: DN1, DN1UP, DN2, DN2UP
    LOGICAL :: FOUND
    !
    !   Format statements
    100 FORMAT(/,1X,'ERROR: Derivatives Interface file name is blank',   &
        ' (SEN_INI_DI)')
    110 FORMAT(/,1X,'ERROR: Derivatives Interface file "',A,   &
        '" does not exist (SEN_INI_DI)')
    112 FORMAT(1X,'ERROR: NSKIP read from derivatives interface file "',   &
        A,'"',/,' is less than zero (SEN_INI_DI)')
    116 FORMAT(1X,'ERROR: Orientatation from derivatives interface file "',   &
        A,'"',/,1X,'should be either ROW/DEP or ROW/PAR, but it is "',   &
        A,'" (SEN_INI_DI)')
    118 FORMAT(/,1X,'ERROR: NDEP read from derivatives interface file "',   &
        A,'"',/,1X,'(',I7,') does not equal expected number',   &
        ' of dependents (',I7,')',/,' (SEN_INI_DI)')
    120 FORMAT(/,1X,'ERROR: Can''t allocate memory (SEN_INI_DI)')
    130 FORMAT(/,1X,'ERROR in SEN_INI_DI:  Expected: ',A,' in file "',A,   &
        '" but found:',/,1X,A)
    140 FORMAT(/,1X,'ERROR reading ',A,' from file "',A,'" (SEN_INI_DI)')
    150 FORMAT(1X,'(Encountered End-Of-File)')
    160 FORMAT(/,1X,'ERROR: One or more parameter names in derivatives',   &
        ' interface',/,' file can''t be matched:')
    165 FORMAT(/,1X,'ERROR: One or more dependent names in derivatives',   &
        ' interface',/,' file can''t be matched:')
    170 FORMAT(1X,A)
    200 FORMAT(A)
    !
    !   Error checking
    KERR = 0
    IF (DERIV_INTERFACE==' ') THEN
      WRITE(IOUT,100)
      CALL UTL_STOP(' ')
    ENDIF
    INQUIRE(FILE=DERIV_INTERFACE,EXIST=LEX,OPENED=LOP,NUMBER=INUMBER)
    IF (.NOT. LEX) THEN
      WRITE(IOUT,110)TRIM(DERIV_INTERFACE)
      CALL UTL_STOP(' ')
    ENDIF
    IF (LOP) CLOSE(INUMBER)
    !
    IDIUNIT = UTL_GETUNIT(7,10000)
    OPEN(IDIUNIT,FILE=DERIV_INTERFACE,ACTION='READ')
    LINE = UTL_GETLINE(IDIUNIT,IOUT)
    ILOC = 1
    CALL UTL_GETTOKEN(KERR,IDIUNIT,IOUT,0,ILOC,LINE,ISTART,ISTOP,DI_DERFILE)
    READ(IDIUNIT,*) DI_NSKIP
    READ(IDIUNIT,*) DI_NDEP,DI_NPAR
    LINE = UTL_GETLINE(IDIUNIT,IOUT)
    ILOC = 1
    CALL UTL_GETTOKEN(KERR,IDIUNIT,IOUT,0,ILOC,LINE,ISTART,ISTOP,DI_ORIENTATION)
    !
    IF (DI_NSKIP<0) THEN
      WRITE(IOUT,112) TRIM(DERIV_INTERFACE)
      KERR = KERR+1
    ENDIF
    CALL UTL_CASETRANS(DI_ORIENTATION,'hi')
    IF (DI_ORIENTATION.NE.'ROW/DEP' .AND. DI_ORIENTATION.NE.'ROW/PAR') THEN
      WRITE(IOUT,116) TRIM(DERIV_INTERFACE),TRIM(DI_ORIENTATION)
      KERR = KERR+1
    ENDIF
    !
    LINE = UTL_GETLINE(IDIUNIT,IOUT)
    ILOC = 1
    CALL UTL_GETTOKEN(KERR,IDIUNIT,IOUT,0,ILOC,LINE,ISTART,ISTOP,DI_DERFORMAT)
    CALL UTL_CASETRANS(DI_DERFORMAT,'hi')
    IF (DI_NDEP>0) THEN
      IF (DI_NDEP.NE.NDEP) THEN
        WRITE(IOUT,118)TRIM(DERIV_INTERFACE),DI_NDEP,NDEP
        CALL UTL_STOP(' ')
      ENDIF
      ALLOCATE(DI_DEPNAM(DI_NDEP),DI_XCOL(DI_NDEP))
    ELSE
      WRITE(IOUT,120)
      KERR = KERR+1
      CALL UTL_STOP(' ')
    ENDIF
    IF (DI_NPAR>0) THEN
      ALLOCATE(DI_PARNAM(DI_NPAR),DI_XROW(DI_NPAR))
    ELSE
      WRITE(IOUT,120)
      KERR = KERR+1
      CALL UTL_STOP(' ')
    ENDIF
    !   Read "PARAMETERS"
    READ(IDIUNIT,200) LINE
    ILOC = 1
    CALL UTL_GETTOKEN(KERR,IDIUNIT,IOUT,1,ILOC,LINE,ISTART,ISTOP,DUMMY)
    IF (DUMMY.NE.'PARAMETERS') THEN
      WRITE(IOUT,130)'"PARAMETERS"',TRIM(DERIV_INTERFACE),TRIM(LINE)
      CALL UTL_STOP(' ')
    ENDIF
    READ(IDIUNIT,*,IOSTAT=ISTAT)(DI_PARNAM(I),I=1,DI_NPAR)
    IF (ISTAT.NE.0) THEN
      WRITE(IOUT,140)'parameter names',TRIM(DERIV_INTERFACE)
      IF (ISTAT<0) WRITE(IOUT,150)
      CALL UTL_STOP(' ')
    ENDIF
    !   Read "DEPENDENTS" and dependent names
    READ(IDIUNIT,200) LINE
    ILOC = 1
    CALL UTL_GETTOKEN(KERR,IDIUNIT,IOUT,1,ILOC,LINE,ISTART,ISTOP,DUMMY)
    IF (DUMMY.NE.'DEPENDENTS') THEN
      WRITE(IOUT,130)'"DEPENDENTS"',TRIM(DERIV_INTERFACE),TRIM(LINE)
      CALL UTL_STOP(' ')
    ENDIF
    READ(IDIUNIT,*,IOSTAT=ISTAT)(DI_DEPNAM(I),I=1,DI_NDEP)
    IF (ISTAT.NE.0) THEN
      WRITE(IOUT,140)'dependent names',TRIM(DERIV_INTERFACE)
      IF (ISTAT<0) WRITE(IOUT,150)
      CALL UTL_STOP(' ')
    ENDIF
    IF (KERR.NE.0) CALL UTL_STOP(' ')
    !
    !   Set up pointer arrays for order of parameters and dependents in
    !   derivatives file (need to reference lists of parameters and dependents
    !   read elsewhere -- may need to make the PARNAM and IPTR arrays public
    !   and store them somewhere other than application module.)
    !
    !   Define parameter-name order
    NPOS = 0
    DI_XROW = 0
    IERR = 0
    PARORDER: DO I=1,DI_NPAR
      KCHK = 0
      FOUND = .FALSE.
      PN1 = DI_PARNAM(I)
      CALL UTL_CASE(PN1,PN1UP,1)
      PARSEARCH: DO WHILE (.NOT. FOUND)
        IF (KCHK>NPE) EXIT PARSEARCH
        NPOS = NPOS+1
        IF (NPOS>NPE) NPOS = 1
        PN2 = PARNAM(IPTR(NPOS))
        CALL UTL_CASE(PN2,PN2UP,1)
        IF (PN1UP==PN2UP) THEN
          FOUND = .TRUE.
          DI_XROW(I) = NPOS
        ENDIF
        KCHK = KCHK+1
      ENDDO PARSEARCH
    ENDDO PARORDER
    !   Check for unmatched parameter names
    PARCHECK: DO I=1,DI_NPAR
      IF (DI_XROW(I)==0) THEN
        IERR = IERR+1
        IF (IERR==1) WRITE(IOUT,160)
        WRITE(IOUT,170)DI_PARNAM(I)
      ENDIF
    ENDDO PARCHECK
    IF (IERR>0) CALL UTL_STOP(' ')
    !
    !   Define dependent-name order
    NPOS = 0
    DI_XCOL = 0
    DEPORDER: DO I=1,DI_NDEP
      KCHK = 0
      FOUND = .FALSE.
      DN1 = DI_DEPNAM(I)
      CALL UTL_CASE(DN1,DN1UP,1)
      DEPSEARCH: DO WHILE (.NOT. FOUND)
        IF (KCHK>NDEP) EXIT DEPSEARCH ! bug fixed 1/7/08 ERB
        NPOS = NPOS+1
        IF (NPOS>NDEP) NPOS = 1
        DN2 = DEPNAM(NPOS)
        CALL UTL_CASE(DN2,DN2UP,1)
        IF (DN1UP==DN2UP) THEN
          FOUND = .TRUE.
          DI_XCOL(I) = NPOS
        ENDIF
        KCHK = KCHK+1
      ENDDO DEPSEARCH
    ENDDO DEPORDER
    !   Check for unmatched dependent names
    IERR = 0
    DEPCHECK: DO I=1,DI_NDEP
      IF (DI_XCOL(I)==0) THEN
        IERR = IERR+1
        IF (IERR==1) WRITE(IOUT,165)
        WRITE(IOUT,170)DI_DEPNAM(I)
      ENDIF
    ENDDO DEPCHECK
    IF (IERR>0) CALL UTL_STOP(' ')
    !
    RETURN
  END SUBROUTINE SEN_INI_DI
  !-----------------------------------------------------------------------------
  SUBROUTINE SEN_DEF(CTRLJOB,IOUT,NDEP,NPE,NPT,IPTR,ISENMETHOD,PADJ,   &
                     PARNAM,SENSDONE,X,ISNPRT,NUMPPL)
    !   Define job of current iteration of Control loop when X array is to be
    !   populated.
    USE DATATYPES
    USE UTILITIES
    USE BASIC, ONLY: DERIV_INTERFACE
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=*),                      INTENT(IN)    :: CTRLJOB
    INTEGER,                               INTENT(IN)    :: IOUT
    INTEGER,                               INTENT(IN)    :: NDEP       ! Number of dependent variables
    INTEGER,                               INTENT(IN)    :: NPE        ! Number of parameters with estimate=yes
    INTEGER,                               INTENT(IN)    :: NPT        ! Number of parameters
    INTEGER, DIMENSION(NPE),               INTENT(IN)    :: IPTR
    INTEGER, DIMENSION(NPT),               INTENT(IN)    :: ISENMETHOD ! Flag to identify sensitivity method for each parameter
    LOGICAL, DIMENSION(NPT),               INTENT(IN)    :: PADJ
    CHARACTER(LEN=12), DIMENSION(NPT),     INTENT(IN)    :: PARNAM
    LOGICAL,                               INTENT(INOUT) :: SENSDONE
    DOUBLE PRECISION, DIMENSION(NPE,NDEP), INTENT(INOUT) :: X          ! Dependent-variable sensitivities, dy/db
    INTEGER,                               INTENT(OUT)   :: ISNPRT
    INTEGER,                               INTENT(OUT)   :: NUMPPL     ! Number of iterations of potentially parallel loop
    !
    !   Local variables
    INTEGER           :: IERR, IP, IPTRI
    !
    !   Format statements
    100 FORMAT(/,1X,'Programmer error: Value of ISENMETHOD (',I4,   &
        ') for parameter "',A,'" is invalid (SEN_DEF)')
    120 FORMAT(/,1X,'Method of obtaining sensitivities is: ',A)
    !
    !   Assignments that are independent of ISENPERT and ISENMETHOD
    IF (NPARDONE==0) THEN
      X = 0.0D0
      IDONE = -1
    ENDIF
    IF (SENSDONE) THEN
      NPARDONE = 0
    ENDIF
    SENSDONE = .FALSE.
    IERR = 0
    !
    !   Assign ISENPERT for this iteration of the Control loop,
    !   either as 0 (model-calculated sensitivities),
    !   or as 1 (perturbation sensitivities).
    IF (DERIV_INTERFACE==' ' .OR. NPARDONE>0) THEN
      !   In this iteration of Control loop, calculate sensitivities by perturbation
      WRITE(IOUT,120)'PERTURBATION SENSITIVITY'
      ISENPERT = 1
      NUMPPL = 0
      IF (CTRLJOB=='FORWARD&SENS') NUMPPL = 1
      DOIP: DO IP=1,NPE
        IPTRI = IPTR(IP)
        IF (ISENMETHOD(IPTRI)==0) THEN
          CONTINUE
        ELSEIF (ISENMETHOD(IPTRI)==1) THEN
          IF (PADJ(IPTRI)) THEN
            NUMPPL = NUMPPL+1
          ENDIF
        ELSEIF (ISENMETHOD(IPTRI)==2) THEN
          IF (PADJ(IPTRI)) THEN
            NUMPPL = NUMPPL+2
          ENDIF
        ELSE
          WRITE(*,100)ISENMETHOD(IPTRI),TRIM(PARNAM(IPTRI))
          CALL UTL_STOP(' ')
        ENDIF
      ENDDO DOIP
    ELSE
      !   In this iteration of Control loop, obtain model-calculated sensitivities
      WRITE(IOUT,120)'MODEL-CALCULATED SENSITIVITY'
      ISENPERT = 0
      NUMPPL = 1
    ENDIF
    ISNPRT = ISENPERT
    !
    IF (IERR>0) CALL UTL_STOP(' ')
    RETURN
  END SUBROUTINE SEN_DEF
  !-----------------------------------------------------------------------------
  SUBROUTINE SEN_GEN(CTRLJOB,KPPL,NPE,NPT,IOUT,BINC,IPTR,ISENMETHOD,NOPNT,NW,  &
                     PARNAM,PRECPR,PVAL,KPE,PSETTMP)
    !   Generate one set of parameter values for one iteration of potentially
    !   parallel loop.  For the parameter being perturbed, ensure that perturbed
    !   value equals value that will be written to model-input files, and that
    !   perturbation amount (DELB) equals the difference of the (potentially
    !   precision-limited) perturbed and unperturbed parameter values.
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: IVERB
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=*),                  INTENT(IN)    :: CTRLJOB
    INTEGER,                           INTENT(IN)    :: KPPL
    INTEGER,                           INTENT(IN)    :: NPE
    INTEGER,                           INTENT(IN)    :: NPT     ! Number of parameters
    INTEGER,                           INTENT(IN)    :: IOUT
    DOUBLE PRECISION,  DIMENSION(NPE), INTENT(IN)    :: BINC    ! Perturbation increment for each parameter
    INTEGER,           DIMENSION(NPE), INTENT(IN)    :: IPTR
    INTEGER,           DIMENSION(NPT), INTENT(IN)    :: ISENMETHOD
    INTEGER,                           INTENT(IN)    :: NOPNT
    INTEGER,           DIMENSION(NPT), INTENT(IN)    :: NW      ! Minimum word length of a parameter
    CHARACTER(LEN=*),  DIMENSION(NPT), INTENT(IN)    :: PARNAM  ! Parameter names
    INTEGER,                           INTENT(IN)    :: PRECPR
    DOUBLE PRECISION,  DIMENSION(NPT), INTENT(IN)    :: PVAL    ! Current parameter values
    INTEGER,                           INTENT(OUT)   :: KPE
    DOUBLE PRECISION,  DIMENSION(NPE), INTENT(OUT)   :: PSETTMP ! Parameter temporary values
    !
    !   Local variables
    INTEGER            :: I, IFAIL, IP, IPTRI, K, KINC
    DOUBLE PRECISION   :: PERTVAL ! Perturbed parameter value
    DOUBLE PRECISION   :: PSIGN   ! Multiplier that implements perturbation direction
    CHARACTER (LEN=25) :: WORD  ! Character representation of perturbed parameter value
    CHARACTER(LEN=8)   :: PERTURBDIRECTION
    !
    !   Format statements
    100 FORMAT(/,   &
        1X,'PARAMETER',9X,'VALUE',6X,'PERTURBED VALUE',3X,'DIFFERENCE',   &
        3X,'PERTURB DIRECTION',/,   &
        1X,A,2X,G12.5,4X,G12.5,4X,G12.5,2X,G12.5,2X,A)
    !
    IFAIL = 0
    !
    !   First, populate PSETTMP with current parameter values
    DO I=1,NPE
      PSETTMP(I) = PVAL(IPTR(I))
    ENDDO
    KPE = 0
    !
    IF (CTRLJOB=='FORWARD&SENS' .AND. KPPL==1) THEN
      RETURN
    ENDIF
    !
    IF (ISENPERT>0) THEN
      !   Assign KPE and PERTURBDIRECTION in a parallelizable way.
      !   KPE is the parameter number of the current parameter being perturbed,
      !   among the parameters for which sensitivities are needed
      !   (between 1 and NPE).
      K = 0
      IF (CTRLJOB=='FORWARD&SENS') K = 1
      PDIR: DO IP=1,NPE
        IPTRI = IPTR(IP)
        IF (ISENMETHOD(IPTRI)==0) THEN
          KINC = 0
        ELSEIF (ISENMETHOD(IPTRI)==1) THEN
          KINC = 1
        ELSEIF (ISENMETHOD(IPTRI)==2) THEN
          KINC = 2
        ENDIF
        K = K+KINC
        IF (K.GE.KPPL) THEN
          KPE = IP
          IF (KINC==1 .OR. KPPL<K) THEN
            PERTURBDIRECTION = 'FORWARD'
          ELSE
            PERTURBDIRECTION = 'BACKWARD'
          ENDIF
          EXIT PDIR
        ENDIF
      ENDDO PDIR
      !
      IF (PERTURBDIRECTION=='FORWARD') THEN
        PSIGN = 1.0D0
      ELSE
        PSIGN = -1.0D0
      ENDIF
      PERTVAL = PVAL(IPTR(KPE)) + PSIGN*BINC(KPE)
      CALL UTL_WRTSIG(IFAIL,PERTVAL,WORD,NW(IPTR(KPE)),PRECPR,   &
                      PSETTMP(KPE),NOPNT)
      IF (IFAIL .NE. 0) CALL UTL_STOP(' ')
    !
      IF (PERTURBDIRECTION=='FORWARD') PVALFWD(KPE) = PSETTMP(KPE)
      DELB(KPE) = PSETTMP(KPE) - PVAL(IPTR(KPE))
      !
      IF (IVERB .GE. 3) WRITE(IOUT,100)PARNAM(IPTR(KPE)),   &
          PVAL(IPTR(KPE)),PSETTMP(KPE),DELB(KPE),PERTURBDIRECTION
      IF (PERTURBDIRECTION=='BACKWARD')   &
          DELB(KPE) = PVALFWD(KPE)-PSETTMP(KPE)
    ENDIF
    !
    RETURN
  END SUBROUTINE SEN_GEN
  !-----------------------------------------------------------------------------
  SUBROUTINE SEN_EXE_SELECT(IOUT,ICOMMAND,COMMAND)
    !   Assign number of command line to be executed for calculation of
    !   sensitivities, depending on the current value of ISENPERT and the ###?.
    USE UTILITIES
    USE BASIC, ONLY: NCOMLINES, COMPURPOSE, COMMANDID, MODCOMLINE
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN)  :: IOUT
    INTEGER,                                 INTENT(OUT) :: ICOMMAND
    CHARACTER(LEN=MAX_STRING_LEN), OPTIONAL, INTENT(OUT) :: COMMAND
    !
    !   Local variables
    INTEGER :: I
    CHARACTER(LEN=12) :: UPSTRING
    !
    !   Format statements
    110 FORMAT(/,1X,'ERROR: No command lines defined for BASIC Module',  &
        ' (SEN_EXE_SELECT)')
    120 FORMAT(/,1X,'ERROR: List of command lines does not include a',   &
        ' command for which',/,' PURPOSE equals FORWARD (SEN_EXE_SELECT)')
    !
    ICOMMAND = 0
    IF (PRESENT(COMMAND)) COMMAND = ' '
    IF (NCOMLINES.LE.0) THEN
      WRITE(IOUT,110)
      CALL UTL_STOP(' ')
    ENDIF
    !
    !   If model-calculated derivatives are needed, make that run first
    IF (ISENPERT==0) THEN
      !   Find command to make a run to generate model-calculated sensitivities
      COMLINES: DO I=1,NCOMLINES
        CALL UTL_CASE(COMPURPOSE(I),UPSTRING,1)
        IF (UPSTRING=='DERIVATIVES' .OR. UPSTRING=='FORWARD&DER') THEN
          ICOMMAND = I
          EXIT COMLINES
        ENDIF
      ENDDO COMLINES
    ELSE
      !   Find command to make a forward run, for perturbation sensitivities
      COMLINES2: DO I=1,NCOMLINES
        CALL UTL_CASE(COMPURPOSE(I),UPSTRING,1)
        IF (UPSTRING=='FORWARD') THEN
          ICOMMAND = I
          EXIT COMLINES2
        ENDIF
      ENDDO COMLINES2
    ENDIF
    !
    IF (ICOMMAND==0) THEN
      WRITE(IOUT,120)
      CALL UTL_STOP(' ')
    ENDIF
    !
    IF (PRESENT(COMMAND)) COMMAND = MODCOMLINE(ICOMMAND)
    RETURN
    !
  END SUBROUTINE SEN_EXE_SELECT
  !-----------------------------------------------------------------------------
  SUBROUTINE SEN_UEV_DX_READ_MATRIX(IUSM,NDEP,NPE,X,COLNAMES,ROWNAMES)
    !   Read matrix of sensitivities from a data-exchange file.  This subroutine
    !   can be used to read the following data exchange files:
    !      _s1 -- One-percent scaled sensitivities
    !      _sd -- Dimensionless scaled sensitivies
    !      _su -- Unscaled sensitivities
    !
    USE GLOBAL_DATA, ONLY: AMESSAGE, BIGINTEGER
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                               INTENT(IN)  :: IUSM ! Unit opened for DX file
    INTEGER,                               INTENT(IN)  :: NDEP ! Number of dependent variables
    INTEGER,                               INTENT(IN)  :: NPE  ! Number of adjustable parameters
    DOUBLE PRECISION, DIMENSION(NPE,NDEP), INTENT(OUT) :: X    ! Array of sensitivities
    CHARACTER(LEN=*), DIMENSION(NPE),  OPTIONAL, INTENT(INOUT) :: COLNAMES
    CHARACTER(LEN=*), DIMENSION(NDEP), OPTIONAL, INTENT(INOUT) :: ROWNAMES
    !
    !   Local variables
    INTEGER :: IERR, IDUM, IP, IP1, IP2, KB, KC, KP, KQ, MRECL, MREM,   &
               N, NPREC1
    CHARACTER(LEN=1) :: CH
    CHARACTER(LEN=40) :: CDUM
    !
    !   Format statements
    200 FORMAT(1X,'Programmer error in call to SEN_UEV_DX_READ_MATRIX--',   &
               'Arguments COLNAMES and',/,1X,   &
               'ROWNAMES must either both be',   &
               ' present or both be absent.')
    !
    MRECL = BIGINTEGER
    !
    !   Check for valid combination of optional arguments
    IERR = 0
    IF (PRESENT(COLNAMES)) THEN
      IF (.NOT. PRESENT(ROWNAMES)) IERR = 1
    ELSE
      IF (PRESENT(ROWNAMES)) IERR=1
    ENDIF
    IF (IERR==1) THEN
      WRITE(*,200)
      CALL UTL_STOP()
    ENDIF
    !
    !   Read header one character at a time to determine number of parameters
    !   per record
    KQ = 0
    DO KC=1,MRECL
      READ(IUSM,'(A1)',ADVANCE='NO',EOR=50) CH
      IF (CH=='"') KQ = KQ+1
    ENDDO
    50 CONTINUE
    MREM = MOD(KQ,2)
    IF (MREM .NE. 0) THEN
      AMESSAGE = 'Programming error: SEN_UEV_DX_READ_MATRIX'//   &
                 ' finds odd number of quotes'
      CALL UTL_STOP()
    ENDIF
    NPREC1 = (KQ-4)/2
    REWIND(IUSM)
    !
    !   Read matrix of sensitivities, in blocks of NPPREC parameters
    KB = 0
    KP = 0
    BLOK: DO WHILE (KP<NPE)
      IP1 = KB*NPREC1+1
      KB = KB+1
      IP2 = KB*NPREC1
      IF (IP2>NPE) IP2 = NPE
      IF (PRESENT(COLNAMES)) THEN
        !   Read parameter names into COLNAMES
        READ(IUSM,*)CDUM,CDUM,(COLNAMES(IP),IP=IP1,IP2)
        DO N = 1, NDEP
          READ(IUSM,*) ROWNAMES(N),IDUM,(X(IP,N),IP=IP1,IP2)
        ENDDO
      ELSE
        !   Read and ignore header
        READ (IUSM,*) CH
        DO N = 1, NDEP
          READ(IUSM,*) CH,IDUM,(X(IP,N),IP=IP1,IP2)
        ENDDO
      ENDIF
      KP = IP2
    ENDDO BLOK
    RETURN
  END SUBROUTINE SEN_UEV_DX_READ_MATRIX
  !-----------------------------------------------------------------------------
  SUBROUTINE SEN_UEV_DX_READ_SU(NDEP,NPE,OBSNAM,OUTNAM,PARNAM,   &
                                PLOTSYMBOL,XSENS,EXT)
    !   READ _SU Data-Exchange File: Unscaled Sensitivities
    USE GLOBAL_DATA, ONLY: AMESSAGE, MAX_STRING_LEN, BIGINTEGER
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN)  :: NDEP
    INTEGER,                                 INTENT(IN)  :: NPE
    CHARACTER(LEN=MAX_STRING_LEN),           INTENT(IN)  :: OUTNAM
    CHARACTER(LEN=LENDNAM), DIMENSION(NDEP), INTENT(OUT) :: OBSNAM
    CHARACTER(LEN=12),      DIMENSION(NPE),  INTENT(OUT) :: PARNAM
    INTEGER,                DIMENSION(NDEP), INTENT(OUT) :: PLOTSYMBOL
    DOUBLE PRECISION,   DIMENSION(NPE,NDEP), INTENT(OUT) :: XSENS
    CHARACTER(LEN=*), OPTIONAL,              INTENT(IN)  :: EXT
    !
    !   Local variables
    CHARACTER(LEN=20) :: CDUM
    CHARACTER(LEN=1) :: CH
    CHARACTER(LEN=7) :: EXTLOCAL
    INTEGER :: I, IP1, IP2, IUSU, J, KB, KC, KP, KQ, MRECL, MREM, NPREC1
    !
    IF (NDEP <= 0) THEN
      AMESSAGE = 'Error encountered in SEN_UEV_DX_READ_SU: NUMBER OF'//   &
                 ' OBSERVATIONS AND PRIOR = 0, NOTHING TO DO'
      CALL UTL_STOP()
    ENDIF
    IF(PRESENT(EXT)) THEN
      IF (UTL_SAMENAME(TRIM(EXT),'_su')) THEN
        EXTLOCAL = '_su'
      ELSEIF (UTL_SAMENAME(TRIM(EXT),'_supri')) THEN
        EXTLOCAL = '_supri'
      ELSEIF (UTL_SAMENAME(TRIM(EXT),'_suprip')) THEN
        EXTLOCAL = '_suprip'
      ELSE
        AMESSAGE='ERROR in SEN_UEV_DX_READ_SU: EXT argument not supported: "'// &
            TRIM(EXT)//'"'
        CALL UTL_STOP()
      ENDIF
    ELSE
      EXTLOCAL = '_su'
    ENDIF
    ! Open data exchange file
    IUSU = UTL_DX_OPEN(OUTNAM,TRIM(EXTLOCAL),'OLD')
    !
    !   Read header one character at a time to determine number of parameters
    !   per record
    MRECL = BIGINTEGER
    KQ = 0
    DO KC=1,MRECL
      READ(IUSU,'(A1)',ADVANCE='NO',EOR=50) CH
      IF (CH=='"') KQ = KQ+1
    ENDDO
    50 CONTINUE
    NPREC1 = KQ/2 - 2
    MREM = MOD(KQ,2)
    IF (MREM .NE. 0) THEN
      AMESSAGE = &
      'Programming error: SEN_UEV_DX_READ_SU '//   &
                 ' finds odd number of quotes in '//TRIM(EXTLOCAL)//' file'
      CALL UTL_STOP()
    ENDIF
    REWIND(IUSU)
    !
    !  Read  parameter names
    KB = 0
    KP = 0
    BLOK: DO WHILE (KP<NPE)
      IP1 = KB*NPREC1+1
      KB = KB+1
      IP2 = KB*NPREC1
      IF (IP2>NPE) IP2 = NPE
      READ(IUSU,*,END=100)CDUM,CDUM,(PARNAM(I),I=IP1,IP2)
      DO I = 1, NDEP
        READ(IUSU,*,END=100)OBSNAM(I),PLOTSYMBOL(I),(XSENS(J,I),J=IP1,IP2)
      ENDDO
      KP = IP2
    ENDDO BLOK
    !
    ! Close data exchange file
    IUSU = UTL_DX_CLOSE(TRIM(EXTLOCAL))
    RETURN
    100 CONTINUE
    AMESSAGE = &
    'Error in SEN_UEV_DX_READ_SU: Premature end of '//TRIM(EXTLOCAL)//' file'
    CALL UTL_STOP()
    RETURN
  END SUBROUTINE SEN_UEV_DX_READ_SU
  !-----------------------------------------------------------------------------
  SUBROUTINE SEN_UEV_DX_WRITE_MATRIX(IUSM,NDEP,NPE,NPT,OBSNAM,IPTR,PARNAM,   &
                                     X,IPLOTOPT)
    !   Write matrix of sensitivities to a data-exchange file.  The following
    !   data exchange files use this subroutine:
    !      _s1 -- One-percent scaled sensitivities
    !      _sd -- Dimensionless scaled sensitivies
    !      _su -- Unscaled sensitivities
    !
    USE GLOBAL_DATA, ONLY: AMESSAGE, BLANKS, NPPREC
    USE UTILITIES
    USE DEPENDENTS, ONLY: DEP_GET_PLOTSYMBOL, GROUP
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: IUSM   ! Unit number opened for DX file
    INTEGER,                                 INTENT(IN) :: NDEP   ! Number of dependent variables
    INTEGER,                                 INTENT(IN) :: NPE    ! Number of active parameters
    INTEGER,                                 INTENT(IN) :: NPT    ! Number of parameters, total
    CHARACTER(LEN=LENDNAM), DIMENSION(NDEP), INTENT(IN) :: OBSNAM
    INTEGER,                DIMENSION(NPE),  INTENT(IN) :: IPTR   ! Pointers to adjustable parameters
    CHARACTER(LEN=12),      DIMENSION(NPT),  INTENT(IN) :: PARNAM ! Parameter names
    DOUBLE PRECISION,   DIMENSION(NPE,NDEP), INTENT(IN) :: X      ! Array of sensitivities
    !   IPLOTOPT is plot symbol.  If present, use instead invoking DEP_GET_PLOTSYMBOL
    INTEGER, OPTIONAL, DIMENSION(NDEP),      INTENT(IN) :: IPLOTOPT
    !
    !   Local variables
    INTEGER :: IP, IP1, IP2, KB, KP, N
    CHARACTER(LEN=100) :: FMT_STRING1, FMT_STRING2
    CHARACTER(LEN=10)  :: STR_NPPREC
    !
    !   Construct format strings
    WRITE(STR_NPPREC,'(I10)') NPPREC
    STR_NPPREC = ADJUSTL(STR_NPPREC)
    FMT_STRING1 = '(1X,''"OBSERVATION NAME" "PLOT SYMBOL"'','//   &
                   TRIM(STR_NPPREC)//'(1X,''"'',A12,''"'',:))'
    FMT_STRING2 = '(1X,A,1X,I6,5X,'//TRIM(STR_NPPREC)//'(1X,G25.16))'
    !
    !   Write matrix of sensitivities, in blocks of NPPREC parameters
    KB = 0
    KP = 0
    BLOK: DO WHILE (KP<NPE)
      IP1 = KB*NPPREC+1
      KB = KB+1
      IP2 = KB*NPPREC
      IF (IP2>NPE) IP2 = NPE
      WRITE (IUSM,FMT_STRING1) (PARNAM(IPTR(IP)),IP=IP1,IP2)
      IF (PRESENT(IPLOTOPT)) THEN
        DO N = 1, NDEP
          WRITE (IUSM,FMT_STRING2) OBSNAM(N),IPLOTOPT(N),(X(IP,N),IP=IP1,IP2)
        ENDDO
      ELSE
        DO N = 1, NDEP
          WRITE (IUSM,FMT_STRING2) OBSNAM(N),DEP_GET_PLOTSYMBOL(GROUP(N),1),   &
                                   (X(IP,N),IP=IP1,IP2)
        ENDDO
      ENDIF
      KP = IP2
    ENDDO BLOK
    RETURN
  END SUBROUTINE SEN_UEV_DX_WRITE_MATRIX
  !-----------------------------------------------------------------------------
  SUBROUTINE SEN_UEV_DX_WRITE_SEN(NCOVMAT,NOBS,NPE,NPS,LN,OBSNAM,OUTNAM, &
                                  IPTR,PARNAM,PVAL,WTFULLSQR,XSENS)
    !   VERSION 20051018 ERB
    !   Write _S1_SC_SD_SO_SU Data-Exchange Files: Sensitivities
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN, NPPREC
    USE UTILITIES
    USE DEPENDENTS, ONLY: DEP_GET_PLOTSYMBOL, GROUP
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: NCOVMAT
    INTEGER,                                 INTENT(IN) :: NOBS
    INTEGER,                                 INTENT(IN) :: NPE
    INTEGER,                                 INTENT(IN) :: NPS
    INTEGER,                DIMENSION(NPS),  INTENT(IN) :: LN
    CHARACTER(LEN=LENDNAM), DIMENSION(NOBS), INTENT(IN) :: OBSNAM
    CHARACTER(LEN=MAX_STRING_LEN),           INTENT(IN) :: OUTNAM
    INTEGER,                DIMENSION(NPE),  INTENT(IN) :: IPTR
    CHARACTER(LEN=12),      DIMENSION(NPS),  INTENT(IN) :: PARNAM
    DOUBLE PRECISION,       DIMENSION(NPS),  INTENT(IN) :: PVAL
    TYPE (CDMATRIX),                         INTENT(IN) :: WTFULLSQR
    DOUBLE PRECISION,   DIMENSION(NPE,NOBS), INTENT(IN) :: XSENS
    !
    !   Local variables
    INTEGER  ID, IP, IPP, IUSU, IUSC, IUSD, IUS1, IUSO,   &
             JN, KN
    CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: MAX1OBS, MAX2OBS
    CHARACTER(LEN=100) :: FMT_STRING2
    CHARACTER(LEN=10)  :: STR_NPPREC
    DOUBLE PRECISION  ABSDSS, ABSMSS, MSS, SSS
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: DSS
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)  :: CSS, MAX1DSS, MAX2DSS
    !
    !   Format statements
    600 FORMAT (1X,'"PARAMETER NAME" "COMPOSITE SCALED SENSITIVITY"', &
        ' "LARGEST DSS FOR THE PARAMETER" "OBSERVATION WITH LARGEST DSS"', &
        ' "SECOND LARGEST DSS FOR THE PARAMETER"', &
        ' "OBSERVATION WITH SECOND LARGEST DSS"')
    601 FORMAT(1X,A,5X,G22.15,16X,G22.15,1X,A,16X,G22.15,1X,A)    
    700 FORMAT (1X,'"OBSERVATION NAME"',1X,'"PLOT SYMBOL"',1X, &
                '"LEVERAGE"',1X, &
                '"LARGEST DSS"',1X, &
                '"PARAMETER WITH LARGEST DSS"')
    701 FORMAT(1X,A,1X,I6,5X,2(1X,G22.15),1X,A)
    !
    !ALLOCATE
    ALLOCATE(MAX1OBS(NPE),MAX2OBS(NPE),DSS(NPE,NOBS),CSS(NPE), &
             MAX1DSS(NPE),MAX2DSS(NPE))
    !
    !   Construct format strings
    WRITE(STR_NPPREC,'(I10)') NPPREC
    STR_NPPREC = ADJUSTL(STR_NPPREC)
    FMT_STRING2 = '(1X,A,1X,I6,5X,'//TRIM(STR_NPPREC)//'(1X,G22.15))'
    !
    ! Initialize
    MAX1DSS = 0.D0
    MAX2DSS = 0.D0
    MAX1OBS = ' '
    MAX2OBS = ' '
    ! Open data exchange file
    IUSU = UTL_DX_OPEN(OUTNAM,'_su','REPLACE')
    ! Write  data
    CALL SEN_UEV_DX_WRITE_MATRIX(IUSU,NOBS,NPE,NPS,OBSNAM,IPTR,PARNAM,XSENS)
    ! Close data exchange file
    IUSU = UTL_DX_CLOSE('_su')
    ! Initialize
    DSS = 0.D0
    CSS = 0.D0
    ! Calculate Dimensionless Scaled Sensitivity = Unscaled(XSENS) * PVAL * SQRT(WT)
    ! Open data exchange file
    IUSD = UTL_DX_OPEN(OUTNAM,'_sd','REPLACE')
    IF(NCOVMAT>0) THEN
      DO IP=1,NPE
        DO JN=1,NOBS
          DO KN=1,NOBS
            DSS(IP,JN) = DSS(IP,JN) + XSENS(IP,KN)*UTL_GETVAL(WTFULLSQR,KN,JN)
          ENDDO
          IF(LN(IPTR(IP)).EQ.0) DSS(IP,JN) = DSS(IP,JN) * PVAL(IPTR(IP))
        ENDDO
      ENDDO
    ELSE
      DO IP=1,NPE
        DO JN=1,NOBS
          IF(LN(IPTR(IP)).EQ.0)THEN
            DSS(IP,JN)=XSENS(IP,JN)*PVAL(IPTR(IP))*UTL_GETVAL(WTFULLSQR,JN,JN)
          ELSE
            DSS(IP,JN)=XSENS(IP,JN)*UTL_GETVAL(WTFULLSQR,JN,JN)
          ENDIF
        ENDDO
      ENDDO
    ENDIF
    ! Write  data
    CALL SEN_UEV_DX_WRITE_MATRIX(IUSD,NOBS,NPE,NPS,OBSNAM,IPTR,PARNAM,DSS)
    ! Close data exchange file
    IUSD = UTL_DX_CLOSE('_sd')
    !
    ! Open data exchange file
    IUSO = UTL_DX_OPEN(OUTNAM,'_so','REPLACE')
    ! Sensitivity Info by observation, depends on dss's being in DSS
    ! further down in this routine the same DSS is used for 1% ss
    WRITE(IUSO,700)
    DO ID = 1, NOBS
      ABSDSS = 0.D0
      SSS = 0.D0
      ABSMSS = 0.D0
      MSS = 0.D0
      IPP = 1
      DO IP = 1,NPE
        ABSDSS = ABS(DSS(IP,ID))
        SSS = SSS + ABSDSS
        IF(ABSDSS .GT. ABSMSS) THEN
          ABSMSS = ABSDSS
          MSS = DSS(IP,ID)
          IPP = IP
        ENDIF
      ENDDO
      ! ASS = SSS / NPE average dss for a parameter not printed at this time
      WRITE (IUSO,701) OBSNAM(ID),DEP_GET_PLOTSYMBOL(GROUP(ID),1),SSS,MSS, &
                       PARNAM(IPTR(IPP))
    ENDDO
    ! Close data exchange file
    IUSO = UTL_DX_CLOSE('_so')
    ! Composite Scaled Sensitivity = SQRT ( SUM ((DSS*DSS)/NOBS))
    ! Note again this depends on dss being in dss
    ! further down in this routine we use dss for 1% ss
    DO IP=1,NPE
      DO JN=1,NOBS
        CSS(IP)= CSS(IP) + DSS(IP,JN)*DSS(IP,JN)
        IF(ABS(DSS(IP,JN)) > ABS(MAX2DSS(IP))) THEN
          MAX2DSS(IP) = DSS(IP,JN)
          MAX2OBS(IP) = OBSNAM(JN)
        ENDIF
        IF(ABS(MAX2DSS(IP)) > ABS(MAX1DSS(IP))) THEN
          MAX2DSS(IP) = MAX1DSS(IP)
          MAX2OBS(IP) = MAX1OBS(IP)
          MAX1DSS(IP) = DSS(IP,JN)
          MAX1OBS(IP) = OBSNAM(JN)
        ENDIF
      ENDDO
      CSS(IP)= DSQRT(CSS(IP)/NOBS)
    ENDDO
    ! Open data exchange file
    IUSC = UTL_DX_OPEN(OUTNAM,'_sc','REPLACE')
    WRITE(IUSC,600)
    DO IP = 1, NPE
      WRITE (IUSC,601) PARNAM(IPTR(IP)),CSS(IP),MAX1DSS(IP),MAX1OBS(IP), &
                       MAX2DSS(IP),MAX2OBS(IP)
    ENDDO
    ! Close data exchange file
    IUSC = UTL_DX_CLOSE('_sc')
    !
    ! REPLACE DSS WITH ONE PERCENT SCALED SENS
    ! 1 % Scaled Sensitivity = Unscaled(XSENS) * PVAL /100
    DO IP=1,NPE
      DO JN=1,NOBS
        DSS(IP,JN)=XSENS(IP,JN)*PVAL(IPTR(IP))/100.D0
      ENDDO
    ENDDO
    ! Open data exchange file
    IUS1 = UTL_DX_OPEN(OUTNAM,'_s1','REPLACE')
    ! Write  data
    CALL SEN_UEV_DX_WRITE_MATRIX(IUS1,NOBS,NPE,NPS,OBSNAM,IPTR,PARNAM,DSS)
    ! Close data exchange file
    IUS1 = UTL_DX_CLOSE('_s1')
    !
    ! DEALLOCATE
    DEALLOCATE(MAX1OBS,MAX2OBS,DSS,CSS,MAX1DSS,MAX2DSS)
    RETURN
  END SUBROUTINE SEN_UEV_DX_WRITE_SEN
  !-----------------------------------------------------------------------------
  SUBROUTINE SEN_UEV_LNX(NDEP,NPE,NPT,IPTR,LN,PVAL,X,ICONVERT_DERIV)
    !   Convert native-space derivatives to log-space derivatives (by
    !   multiplying by the parameter value) for parameters where LN > 0.
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                               INTENT(IN)    :: NDEP
    INTEGER,                               INTENT(IN)    :: NPE
    INTEGER,                               INTENT(IN)    :: NPT
    INTEGER,           DIMENSION(NPE),     INTENT(IN)    :: IPTR
    INTEGER,           DIMENSION(NPT),     INTENT(IN)    :: LN
    DOUBLE PRECISION,  DIMENSION(NPT),     INTENT(IN)    :: PVAL ! Current parameter values
    DOUBLE PRECISION, DIMENSION(NPE,NDEP), INTENT(INOUT) :: X
    INTEGER,     DIMENSION(NPT), OPTIONAL, INTENT(IN)    :: ICONVERT_DERIV
    !
    !   Local variables
    INTEGER :: ID, IP, IPT
    !
    DO IP = 1,NPE
      IPT = IPTR(IP)
      IF(PRESENT(ICONVERT_DERIV)) THEN
        IF(ICONVERT_DERIV(IP) == 0) CYCLE
      ENDIF
      IF (LN(IPT)>0) THEN
        DO ID = 1,NDEP
          X(IP,ID) = PVAL(IPT)*X(IP,ID)
        ENDDO
      ENDIF
    ENDDO
    !
    RETURN
    !
  END SUBROUTINE SEN_UEV_LNX
  !-----------------------------------------------------------------------------
  SUBROUTINE SEN_UEV_POPX_METHOD0(IOUT,NDEP,NPE,X)
    !   Populate rows of the X array with model-calculated derivatives.
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                               INTENT(IN)    :: IOUT
    INTEGER,                               INTENT(IN)    :: NDEP ! Number of dependent variables
    INTEGER,                               INTENT(IN)    :: NPE  ! Number of parameters with estimate=yes
    DOUBLE PRECISION, DIMENSION(NPE,NDEP), INTENT(INOUT) :: X
    !
    !   Local variables
    INTEGER :: I, IPE, ISTAT, IXUNIT, J
    LOGICAL :: LEX, LOP
    !
    !   Format statements
    100 FORMAT(/,1X,'ERROR: File of model-calculated derivatives "',A,'"',/,   &
        1X,'does not exist (SEN_UEV_POPX_METHOD0)')
    110 FORMAT(/,1X,'ERROR in opening file of model-calculated',   &
        ' derivatives:',/,' "',A,   &
        1X,'" (SEN_UEV_POPX_METHOD0)')
    !
    !   Open file of model-calculated derivatives
    INQUIRE(FILE=DI_DERFILE,EXIST=LEX,OPENED=LOP,NUMBER=IXUNIT)
    IF (LEX) THEN
      IF (LOP) THEN
        CLOSE(IXUNIT)
      ELSE
        IXUNIT = UTL_NEXTUNIT()
      ENDIF
    ELSE
      WRITE(IOUT,100)TRIM(DI_DERFILE)
      CALL UTL_STOP(' ')
    ENDIF
    OPEN(IXUNIT,FILE=DI_DERFILE,STATUS='OLD',IOSTAT=ISTAT)
    IF (ISTAT.NE.0) THEN
      WRITE(IOUT,110)DI_DERFILE
      CALL UTL_STOP(' ')
    ENDIF
    !
    !   If specified, skip first line(s) of derivatives file
    IF (DI_NSKIP>0) THEN
      DO I=1,DI_NSKIP
        READ(IXUNIT,'(A)')
      ENDDO
    ENDIF
    !
    !   Populate rows of X matrix by reading model-calculated derivatives
    !   using instructions obtained from the DERIV_INTERFACE file
    IF (DI_ORIENTATION=='ROW/DEP') THEN
      IF (DI_DERFORMAT=='(FREE)') THEN
        DO J=1,DI_NDEP
          READ(IXUNIT,*)(X(DI_XROW(I),DI_XCOL(J)),I=1,DI_NPAR)
        ENDDO
      ELSE
        DO J=1,DI_NDEP
          READ(IXUNIT,DI_DERFORMAT)(X(DI_XROW(I),DI_XCOL(J)),I=1,DI_NPAR)
        ENDDO
      ENDIF
    ELSE
      IF (DI_DERFORMAT=='(FREE)') THEN
        DO I=1,DI_NPAR
          IPE = DI_XROW(I)
          READ(IXUNIT,*)(X(IPE,DI_XCOL(J)),J=1,DI_NDEP)
        ENDDO
      ELSE
        DO I=1,DI_NPAR
          IPE = DI_XROW(I)
          READ(IXUNIT,*)(X(IPE,DI_XCOL(J)),J=1,DI_NDEP)
        ENDDO
      ENDIF
    ENDIF
    !
    DO I=1,DI_NPAR
      IDONE(DI_XROW(I)) = 1
      NPARDONE = NPARDONE + 1
    ENDDO
    CLOSE(IXUNIT)
    !
    RETURN
  END SUBROUTINE SEN_UEV_POPX_METHOD0
  !-----------------------------------------------------------------------------
  SUBROUTINE SEN_UEV_POPX_MODCALC(IOUT,NDEP,NPE,NPT,IPTR,ISENMETHOD,SENSDONE,X)
    !   Populate rows of the sensitivity array with model-calculated derivatives
    USE GLOBAL_DATA, ONLY: IVERB
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                               INTENT(IN)    :: IOUT
    INTEGER,                               INTENT(IN)    :: NDEP
    INTEGER,                               INTENT(IN)    :: NPE      ! Number of parameter with estimate=yes
    INTEGER,                               INTENT(IN)    :: NPT
    INTEGER, DIMENSION(NPE),               INTENT(IN)    :: IPTR
    INTEGER, DIMENSION(NPT),               INTENT(IN)    :: ISENMETHOD
    LOGICAL,                               INTENT(INOUT) :: SENSDONE
    DOUBLE PRECISION, DIMENSION(NPE,NDEP), INTENT(INOUT) :: X
    !
    !   Local variables
    INTEGER :: I
    !
    CALL SEN_UEV_POPX_METHOD0(IOUT,NDEP,NPE,X)
    !
    !   Determine if all required perturbations have been completed
    DO I=1,NPE
      IF (IDONE(I)<0) GOTO 20
      IF (ISENMETHOD(IPTR(I)).NE.0) GOTO 20
    ENDDO
    SENSDONE = .TRUE.
    20 CONTINUE
    !
    RETURN
  END SUBROUTINE SEN_UEV_POPX_MODCALC
  !-----------------------------------------------------------------------------
  SUBROUTINE SEN_UEV_POPXROW_DIFF(KPE,NDEP,SIMEQ1,SIMEQ2,XROW)
    !   Populate a row of the X array with sensitivities calculated by finite
    !   differences.
    !
    !   Argument-list variables
    INTEGER,                           INTENT(IN)    :: KPE
    INTEGER,                           INTENT(IN)    :: NDEP
    DOUBLE PRECISION, DIMENSION(NDEP), INTENT(IN)    :: SIMEQ1 ! Simulated equivalent 1
    DOUBLE PRECISION, DIMENSION(NDEP), INTENT(IN)    :: SIMEQ2 ! Simulated equivalent 2
    DOUBLE PRECISION, DIMENSION(NDEP), INTENT(INOUT) :: XROW
    !
    !   Local variables
    INTEGER :: J
    !
    DO J=1,NDEP        ! Cycle through observations
      XROW(J) = (SIMEQ1(J)-SIMEQ2(J)) / DELB(KPE)
    ENDDO
    IDONE(KPE)=1
    NPARDONE = NPARDONE+1
    !
    RETURN
  END SUBROUTINE SEN_UEV_POPXROW_DIFF
  !-----------------------------------------------------------------------------
  SUBROUTINE SEN_UEV_WRITESENTABLE(NDEP,NPE,NPT,DEPNAM,HEADING,IOUT,IPTR,   &
                                   NCOLS,PARNAM,X)
    !  Write a formatted table of dependent-variable sensitivities to unit
    !  IOUT
    USE GLOBAL_DATA, ONLY: BLANKS, HYPHENS, LENDNAM
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: NDEP    ! Number of dependent variables
    INTEGER,                                 INTENT(IN) :: NPE     ! Number of active parameters
    INTEGER,                                 INTENT(IN) :: NPT     ! Number of parameters, total
    CHARACTER(LEN=LENDNAM), DIMENSION(NDEP), INTENT(IN) :: DEPNAM
    CHARACTER(LEN=*),                        INTENT(IN) :: HEADING ! Table heading
    INTEGER,                                 INTENT(IN) :: IOUT    ! Output unit number
    INTEGER,                 DIMENSION(NPE), INTENT(IN) :: IPTR    ! Pointers to active parameters
    INTEGER,                                 INTENT(IN) :: NCOLS   ! Number of parameters per table section
    CHARACTER(LEN=12),       DIMENSION(NPT), INTENT(IN) :: PARNAM  ! Parameter names
    DOUBLE PRECISION,   DIMENSION(NPE,NDEP), INTENT(IN) :: X       ! Array of sensitivities
    !
    !   Local variables
    INTEGER :: IG1, IG2, IP, IS, N, NSECTS
    !
    !   Format statements
    500 FORMAT(/,1X,'*** Programmer error: NCOLS<1 in call to',   &
        ' SEN_UEV_WRITESENTABLE - Stopping')
    520 FORMAT(/,1X,A,//,8X,A,' PARAMETER:',500(2X,A12))
    540 FORMAT(' OBS #  OBSERVATION ',A,500(2X,A12))
    550 FORMAT(1X,I5,2X,A,500(2X,G12.5))
    !
    !   Find number of table sections required for writing sensitivities for
    !   all parameters
    IF (NCOLS.LT.1) THEN
      WRITE(IOUT,500)
      CALL UTL_STOP(' ')
    ENDIF
    NSECTS = UTL_TABLESECTIONS(NPE,NCOLS)
    !
    !   Write sensitivity table, a section at a time
    EACHSECTION: DO IS = 1,NSECTS
      IG1 = IS*NCOLS-NCOLS+1
      IG2 = IS*NCOLS
      IF (IG2.GT.NPE) IG2 = NPE
      WRITE (IOUT,520) HEADING,BLANKS(1:LENDNAM-10),(PARNAM(IPTR(IP)),   &
          IP=IG1,IG2)
      WRITE (IOUT,540) HYPHENS(1:LENDNAM-11),(HYPHENS(1:12),IP=IG1,IG2)
      DO N = 1, NDEP
        WRITE (IOUT,550) N, DEPNAM(N), (X(IP,N),IP=IG1,IG2)
      ENDDO
    ENDDO EACHSECTION
    !
    RETURN
  END SUBROUTINE SEN_UEV_WRITESENTABLE
  !-----------------------------------------------------------------------------
  SUBROUTINE SEN_CLN()
    !   Deallocate all arrays in the Sensitivity module
    IMPLICIT NONE
    IF (ALLOCATED(DELB)) DEALLOCATE(DELB)
    IF (ALLOCATED(DI_DEPNAM)) DEALLOCATE(DI_DEPNAM)
    IF (ALLOCATED(DI_PARNAM)) DEALLOCATE(DI_PARNAM)
    IF (ALLOCATED(DI_XCOL)) DEALLOCATE(DI_XCOL)
    IF (ALLOCATED(DI_XROW)) DEALLOCATE(DI_XROW)
    IF (ALLOCATED(IDONE)) DEALLOCATE(IDONE)
    IF (ALLOCATED(PVALFWD)) DEALLOCATE(PVALFWD)
    RETURN
  END SUBROUTINE SEN_CLN
  !
END MODULE SENSITIVITY