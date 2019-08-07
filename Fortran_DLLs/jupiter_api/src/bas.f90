MODULE BASIC
  USE DATATYPES
  USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
  PRIVATE
  !
  !   PUBLIC DATA
  !
  PUBLIC COMMANDID, COMPURPOSE, COVMATARR, DERIV_INTERFACE, FORSENS,   &
         LLPTRMATFIL, LLPTRMODCOM, LLPTROPT, MODCOMLINE, NCOMLINES
  !
  CHARACTER (LEN=12), ALLOCATABLE, DIMENSION(:) :: COMMANDID  ! Model command identifier
  CHARACTER (LEN=12), ALLOCATABLE, DIMENSION(:) :: COMPURPOSE ! Model command purpose
  TYPE (CDMATRIX),    ALLOCATABLE, DIMENSION(:) :: COVMATARR   ! Array of all group variance-covariance matrices
  CHARACTER(LEN=MAX_STRING_LEN)                 :: DERIV_INTERFACE
  LOGICAL :: FORSENS    ! True if input has a command with COMPURPOSE='FORWARD&DER'
  TYPE (LLIST), POINTER :: LLPTRMATFIL  ! Pointer to list of matrix files
  TYPE (LLIST), POINTER :: LLPTRMODCOM  ! Pointer to list of model command lines
  TYPE (LLIST), POINTER :: LLPTROPT     ! Pointer to head of first list (options)
  INTEGER :: NCOMLINES  ! Number of model command lines
  !
  !   PUBLIC SUBPROGRAMS
  !
  PUBLIC BAS_INI_COVMAT, BAS_INI_GETOPTIONS,    &
         BAS_INI_MODELEXEC, BAS_GEN, BAS_EXE, BAS_EXE_SELECT, BAS_CLN
  !
  !   PRIVATE DATA
  !
  !   Data related to model command lines
  CHARACTER (LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: MODCOMLINE ! Model command lines
  !
  !   Default column for table of model command lines
  INTEGER, PARAMETER                                :: NMODCOMCOLS = 3
  CHARACTER(LEN=40), DIMENSION(NMODCOMCOLS), TARGET :: MODCOMCOL =    &
      (/ 'COMMAND  ','PURPOSE  ','COMMANDID' /)
  CHARACTER(LEN=40), DIMENSION(0) :: NOCOL
  !
CONTAINS
  !
  !-----------------------------------------------------------------------------
  SUBROUTINE BAS_INI_COVMAT(INUNIT,IOUT,NCOVMAT)
    !   Read and store all group variance-covariance matrices
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: AMESSAGE, IVERB, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(IN) :: INUNIT
    INTEGER, INTENT(IN) :: IOUT
    INTEGER, INTENT(IN) :: NCOVMAT
    !
    !   Local variables
    INTEGER :: I, IERR, ISTAT, IU, K, KFILES, N, NMAT
    CHARACTER(LEN=MAX_STRING_LEN) :: FNAME
    TYPE (LLIST), POINTER :: TAIL
    !
    !   Format statements
    120 FORMAT(/,1X,'MATRICES')
    140 FORMAT(/,1X,'WARNING: The following variance-covariance matrix',   &
        ' read from unit ',I5,' is unnamed')
    161 FORMAT('Error: Expected to read ',I4,' variance-covariance',   &
               ' matrices but only ',I1,' matrix is specified in the',   &
               ' MATRIX_FILES input block')
    162 FORMAT('Error: Expected to read ',I4,' variance-covariance',   &
               ' matrices but only ',I4,' matrices are specified in the',   &
               ' MATRIX_FILES input block')
    !
    NULLIFY(LLPTRMATFIL)
    IF (NCOVMAT<0) RETURN
    IERR = 0
    KFILES = 0
    AMESSAGE = ' '
    ALLOCATE(COVMATARR(NCOVMAT))
    !
    IF (NCOVMAT>0) THEN
      WRITE(IOUT,120)
      !
      DO I=1,NCOVMAT
        CALL TYP_NULL(COVMATARR(I))
      ENDDO
      IU = UTL_GETUNIT(7,1000)
      !
      !   Read names of files from which matrices are to be read
      CALL UTL_READBLOCK(0,'MATRIX_FILES',NOCOL,INUNIT,IOUT,'MATRIXFILE',   &
                         .TRUE.,LLPTRMATFIL,TAIL,KFILES)
      IF (KFILES==0) THEN
        AMESSAGE = 'No matrix files found in MATRIX_FILES input block'
        CALL UTL_WRITE_MESSAGE(IOUT)
        CALL UTL_STOP()
      ENDIF
      !
      I = 0
      FILES: DO K=1,KFILES
        CALL UTL_FILTER(IERR,LLPTRMATFIL,IOUT,'MATRIXFILE',FNAME,K)
        NMAT = 1
        CALL UTL_FILTER(IERR,LLPTRMATFIL,IOUT,'NMATRICES',NMAT,K)
        OPEN(IU,FILE=FNAME,STATUS='OLD',IOSTAT=ISTAT,ERR=50)
        50 CONTINUE
        IF (ISTAT.NE.0) THEN
          AMESSAGE = 'Error opening file: "'//TRIM(FNAME)//'"'
          CALL UTL_WRITE_MESSAGE(IOUT)
          CALL UTL_STOP()
        ENDIF
        IF (NMAT>0) THEN
          DO N=1,NMAT
            IF (I<NCOVMAT) THEN
              I = I+1
              !
              !   Read one variance-covariance matrix
              CALL UTL_READMATRIX(IU,IOUT,COVMATARR(I))
              IF (IVERB .LE. 3) THEN
                IF (COVMATARR(I)%ARRAYNAME==' ') THEN
                  WRITE(IOUT,140) INUNIT
                  CALL UTL_WRITECDMATRIX(COVMATARR(I),1,IOUT)
                ENDIF
              ELSE
                !   Echo variance-covariance matrix to output
                CALL UTL_WRITECDMATRIX(COVMATARR(I),1,IOUT)
              ENDIF
            ELSE
              CLOSE(IU,STATUS='KEEP')
              EXIT FILES
            ENDIF
          ENDDO
        ENDIF
        CLOSE(IU,STATUS='KEEP')
      ENDDO FILES
      IF (I<NCOVMAT) THEN
        IF (I==1) THEN
          WRITE(AMESSAGE,161)NCOVMAT,I
        ELSE
          WRITE(AMESSAGE,162)NCOVMAT,I
        ENDIF
        CALL UTL_WRITE_MESSAGE(IOUT)
        CALL UTL_STOP()
      ENDIF
    ENDIF
    !
    RETURN
  END SUBROUTINE BAS_INI_COVMAT
  !-----------------------------------------------------------------------------
  SUBROUTINE BAS_INI_GETOPTIONS(INUNIT,IOUT)
    !   Read, store, and echo global options
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: IVERB, MINVERB, MAXVERB
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(IN) :: INUNIT  ! Unit number for input
    INTEGER, INTENT(IN) :: IOUT    ! Unit number for output
    !
    !   Local variables
    TYPE (LLIST), POINTER :: TAIL      ! Ptr to tail of a list
    INTEGER :: I, IERR, NOPT
    CHARACTER(LEN=79), DIMENSION(0:5) :: VMESS
    !
    DATA (VMESS(I),I=0,5)/   &
    'VERBOSE=0 -- No extraneous output',   &
    'VERBOSE=1 -- Warnings will be written',   &
    'VERBOSE=2 -- Warnings and notes will be written',   &
    'VERBOSE=3 -- Warnings, notes, and selected input will be written',   &
    'VERBOSE=4 -- Warnings, notes, and all input will be written',   &
    'VERBOSE=5 -- All available output will be written'/
    !   Format statements
    100 FORMAT(/,1X,A)
    150 FORMAT(/,1X,'Echo OPTIONS input:')
    200 FORMAT(/,1X,'No options were read')
    250 FORMAT(/,1X,'Derivatives interface file is: ',A)
    !
    NULLIFY(LLPTROPT)
    NULLIFY(TAIL)
    NOPT = 0
    IERR = 0
    DERIV_INTERFACE = ' '
    !
    !   Read input block type OPTIONS
    CALL UTL_READBLOCK(0,'OPTIONS',NOCOL,INUNIT,IOUT,  &
        '*',.FALSE.,LLPTROPT,TAIL,NOPT)
    IF (NOPT>0) THEN
      !   Traverse the list of options to populate recognized options
      CALL UTL_FILTER(IERR,LLPTROPT,IOUT,'VERBOSE',IVERB)
      IF (IVERB<MINVERB) THEN
        IVERB = MINVERB
      ELSEIF (IVERB>MAXVERB) THEN
        IVERB = MAXVERB
      ENDIF
      IF (IVERB>4) THEN
        WRITE(IOUT,150)
        CALL UTL_WRITEBLOCK(LLPTROPT,IOUT)
      ENDIF
      CALL UTL_FILTER(IERR,LLPTROPT,IOUT,'DERIVATIVES_INTERFACE',DERIV_INTERFACE)
    ELSE
      WRITE(IOUT,200)
    ENDIF
    IF (IVERB .GT. 0)WRITE(IOUT,100) VMESS(IVERB)
    IF (DERIV_INTERFACE.NE.' ') WRITE(IOUT,250)TRIM(DERIV_INTERFACE)
    RETURN
  END SUBROUTINE BAS_INI_GETOPTIONS
  !-----------------------------------------------------------------------------
  SUBROUTINE BAS_INI_MODELEXEC(INUNIT,IOUT)
    !   Read, store, and echo model command lines
    USE DATATYPES
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(IN) :: INUNIT  ! Unit number for input
    INTEGER, INTENT(IN) :: IOUT    ! Unit number for output
    !
    !   Local variables
    INTEGER               :: I, IERR, MORE
    TYPE (LLIST), POINTER :: TAIL
    CHARACTER(LEN=12)     :: UPSTRING
    !
    !   Format statements
    100 FORMAT(//,1X,'ECHO MODEL COMMAND LINES:',//,1X,'Command ID',4X,'Purpose',   &
               7X,'Command Line',/,   &
        1X,12('-'),2X,12('-'),2X,50('-'))
    120 FORMAT(1X,A,2x,A,2X,A)
    140 FORMAT(/,1X,'ERROR: Command purpose "',A,'" for Command ID "',   &
        A,'"',/,' is invalid.  Purpose must be FORWARD, DERIVATIVES,',   &
        ' or FORWARD&DER (BAS_INI_MODELEXEC)')
    150 FORMAT(/,1X,'ERROR: Command purpose FORWARD&DER is specified,',   &
        ' but no Derivatives Interface',/,' file has been defined.')
    !
    IERR = 0
    NCOMLINES = 0
    FORSENS = .FALSE.
    NULLIFY(LLPTRMODCOM)
    !
    !   Read COMMANDLINES input block
    CALL UTL_READBLOCK(NMODCOMCOLS,'MODEL_COMMAND_LINES',MODCOMCOL,INUNIT,   &
                       IOUT,'COMMAND',.TRUE.,LLPTRMODCOM,TAIL,NCOMLINES)
    !
    !   Allocate memory
    ALLOCATE (MODCOMLINE(NCOMLINES),COMPURPOSE(NCOMLINES),COMMANDID(NCOMLINES))
    !   Assign defaults
    MODCOMLINE = ' '
    COMPURPOSE = 'FORWARD'
    COMMANDID = ' '
    !   Store command lines
    CALL UTL_FILTERLIST(LLPTRMODCOM,IOUT,'COMMAND',NCOMLINES,IERR,MODCOMLINE,  &
                        MORE)
    CALL UTL_FILTERLIST(LLPTRMODCOM,IOUT,'PURPOSE',NCOMLINES,IERR,COMPURPOSE,  &
                        MORE)
    CALL UTL_FILTERLIST(LLPTRMODCOM,IOUT,'COMMANDID',NCOMLINES,IERR,COMMANDID, &
                        MORE)
    !
    !   Write model command lines to output
    WRITE(IOUT,100)
    DO I=1,NCOMLINES
      WRITE(IOUT,120)COMMANDID(I),COMPURPOSE(I),TRIM(MODCOMLINE(I))
    ENDDO
    DO I=1,NCOMLINES
      CALL UTL_CASE(COMPURPOSE(I),UPSTRING,1)
      IF (UPSTRING .NE. 'FORWARD' .AND. UPSTRING .NE. 'DERIVATIVES' .AND.   &
          UPSTRING .NE. 'FORWARD&DER') THEN
        WRITE(IOUT,140)TRIM(COMPURPOSE(I)),TRIM(COMMANDID(I))
        IERR = IERR+1
      ENDIF
      IF (UPSTRING=='FORWARD&DER') THEN
        IF (DERIV_INTERFACE==' ') THEN
          WRITE(IOUT,150)
          CALL UTL_STOP()
        ELSE
          FORSENS = .TRUE.
        ENDIF
      ENDIF
    ENDDO
    !
    IF (IERR .NE. 0) CALL UTL_STOP(' ')
    !
    RETURN
  END SUBROUTINE BAS_INI_MODELEXEC
  !-----------------------------------------------------------------------------
  SUBROUTINE BAS_GEN(NOPNT,NPT,NW,PRECPR,PVAL)
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                           INTENT(IN)    :: NOPNT
    INTEGER,                           INTENT(IN)    :: NPT     ! Number of parameters
    INTEGER,           DIMENSION(NPT), INTENT(IN)    :: NW      ! Minimum word length of a parameter
    INTEGER,                           INTENT(IN)    :: PRECPR
    DOUBLE PRECISION,  DIMENSION(NPT), INTENT(INOUT) :: PVAL    ! Current parameter values
    !
    !   Local variables
    INTEGER :: I, IFAIL
    DOUBLE PRECISION :: TMPVAL
    CHARACTER (LEN=25) :: WORD
    !
    IFAIL = 0
    !   Copy PVAL to TMPVAL, ensuring that parameter values equal value that
    !   will eventually be written to model-input files
    DO I=1,NPT
      CALL UTL_WRTSIG(IFAIL,PVAL(I),WORD,NW(I),PRECPR,TMPVAL,NOPNT)
      PVAL(I) = TMPVAL
    ENDDO
    IF (IFAIL .NE. 0) CALL UTL_STOP(' ')
    RETURN
  END SUBROUTINE BAS_GEN
  !-----------------------------------------------------------------------------
  SUBROUTINE BAS_EXE(ICOMMAND,IOUT,KRUN,MRUNS)
    USE GLOBAL_DATA, ONLY: IVERB
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,           INTENT(IN) :: ICOMMAND ! Command number (position in list of commands)
    INTEGER,           INTENT(IN) :: IOUT     ! Unit number of output file
    INTEGER, OPTIONAL, INTENT(IN) :: KRUN
    INTEGER, OPTIONAL, INTENT(IN) :: MRUNS
    !
    !   Local variables
    !
    !   Format statements
    100 FORMAT(/,1X,'Running system command: ',1X,A,/)
    105 FORMAT(/,1X,'Running system command: ',1X,A,' (run ',I5,')',/)
    110 FORMAT(/,1X,'Running system command: ',1X,A,' (run ',I5,' of ',I5,')',/)
    120 FORMAT(1X,'Finished system command:',1X,A)
    !
    IF (IVERB>3) THEN
      IF (IOUT>0) THEN
        IF (PRESENT(KRUN)) THEN
          IF (PRESENT(MRUNS)) THEN
            WRITE(IOUT,110) TRIM(MODCOMLINE(ICOMMAND)),KRUN,MRUNS
          ELSE
            WRITE(IOUT,105) TRIM(MODCOMLINE(ICOMMAND)),KRUN
          ENDIF
        ELSE
          WRITE(IOUT,100) TRIM(MODCOMLINE(ICOMMAND))
        ENDIF
      ELSEIF (IOUT<0) THEN
        IF (PRESENT(KRUN)) THEN
          IF (PRESENT(MRUNS)) THEN
            WRITE(*,110) TRIM(MODCOMLINE(ICOMMAND)),KRUN,MRUNS
          ELSE
            WRITE(*,105) TRIM(MODCOMLINE(ICOMMAND)),KRUN
          ENDIF
        ELSE
          WRITE(*,100) TRIM(MODCOMLINE(ICOMMAND))
        ENDIF
      ENDIF
    ENDIF
    !
    CALL UTL_SYSTEM(MODCOMLINE(ICOMMAND))
    !
    IF (IVERB>3) THEN
      IF (IOUT>0) THEN
        WRITE(IOUT,120) TRIM(MODCOMLINE(ICOMMAND))
      ELSEIF (IOUT<0) THEN
        WRITE(*,120) TRIM(MODCOMLINE(ICOMMAND))
      ENDIF
    ENDIF
    !
    RETURN
  END SUBROUTINE BAS_EXE
  !-----------------------------------------------------------------------------
  SUBROUTINE BAS_EXE_SELECT(IOUT,JOB,ICOMMAND,COMMAND)
    !   Assign number of command line to be executed to make a forward run.
    !   The first command line for which PURPOSE is FORWARD is used.
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN)  :: IOUT
    CHARACTER(LEN=*),                        INTENT(IN)  :: JOB
    INTEGER,                                 INTENT(OUT) :: ICOMMAND
    CHARACTER(LEN=MAX_STRING_LEN), OPTIONAL, INTENT(OUT) :: COMMAND
    !
    !   Local variables
    INTEGER :: I
    CHARACTER(LEN=12) :: UPSTRING
    !
    !   Format statements
    110 FORMAT(/,1X,'ERROR: No command lines defined for BASIC Module',  &
        ' (BAS_EXE_SELECT)')
    120 FORMAT(/,1X,'ERROR: List of command lines does not include a',   &
        ' command for which',/,' PURPOSE equals FORWARD (BAS_EXE_SELECT)')
    !
    IF (PRESENT(COMMAND)) COMMAND = ' '
    IF (NCOMLINES>0) THEN
      ICOMMAND = 0
      COMLINES: DO I=1,NCOMLINES
        CALL UTL_CASE(COMPURPOSE(I),UPSTRING,1)
        IF (JOB=='FORWARD') THEN
          IF (UPSTRING=='FORWARD') THEN
            ICOMMAND = I
            EXIT COMLINES
          ENDIF
        ELSEIF (JOB=='FORWARD&SENS') THEN
          IF (UPSTRING=='FORWARD&DER') THEN
            ICOMMAND = I
            EXIT COMLINES
          ENDIF
        ENDIF
      ENDDO COMLINES
      !
      IF (ICOMMAND==0) THEN
        WRITE(IOUT,120)
        CALL UTL_STOP(' ')
      ENDIF
    ELSE
      WRITE(IOUT,110)
      CALL UTL_STOP(' ')
    ENDIF
    IF (PRESENT(COMMAND)) COMMAND = MODCOMLINE(ICOMMAND)
    !
    RETURN
  END SUBROUTINE BAS_EXE_SELECT
  !-----------------------------------------------------------------------------
  SUBROUTINE BAS_CLN()
    !   Deallocate all arrays and linked lists in the Basic module
    IMPLICIT NONE
    !
    !   Deallocate arrays
    IF (ALLOCATED(COMMANDID)) DEALLOCATE(COMMANDID)
    IF (ALLOCATED(COMPURPOSE)) DEALLOCATE(COMPURPOSE)
    IF (ALLOCATED(MODCOMLINE)) DEALLOCATE(MODCOMLINE)
    !
    !   Deallocate pointers to linked lists
    CALL TYP_DEALLOC(LLPTRMATFIL)
    CALL TYP_DEALLOC(LLPTRMODCOM)
    CALL TYP_DEALLOC(LLPTROPT)
    RETURN
  END SUBROUTINE BAS_CLN
  !
END MODULE BASIC
