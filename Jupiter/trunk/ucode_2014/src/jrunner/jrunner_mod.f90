MODULE JRUNNERMOD
  USE GLOBAL_DATA, ONLY: LENDNAM, MAX_STRING_LEN
  IMPLICIT NONE
  PUBLIC
  !
  INTEGER :: IFAIL, IOUNIT, IRUNRUNNER, ISTAT, KWAIT, LCIS, NCATS, NINSTRUCT,   &
             NMIFILE, NMOFILE, NOPNT, NPT, NRUNRUNNER, NUMLADV, NVEXT,   &
             PRECISPRO
  LOGICAL :: FINISH, LEX, RESET
  CHARACTER(LEN=5) :: FINMESS = 'STOP'
  CHARACTER(LEN=6),              ALLOCATABLE, DIMENSION(:) :: CATGOR   ! Model-calculated value category for each model-output file
  CHARACTER(LEN=1),              ALLOCATABLE, DIMENSION(:) :: CINSTSET ! holds compressed instruction set
  CHARACTER(LEN=LENDNAM),        ALLOCATABLE, DIMENSION(:) :: EXTNAM
  DOUBLE PRECISION,              ALLOCATABLE, DIMENSION(:) :: EXTVAL
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: INSTRUCTFILE
  INTEGER,                       ALLOCATABLE, DIMENSION(:) :: LOCINS   ! pointer to instructions
  LOGICAL,                       ALLOCATABLE, DIMENSION(:) :: MCVUSE
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: MIFILE
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: MOFILE   ! Names of model-output files
  CHARACTER(LEN=1),              ALLOCATABLE, DIMENSION(:) :: MRKDL
  INTEGER,                       ALLOCATABLE, DIMENSION(:) :: NW
  CHARACTER(LEN=12),             ALLOCATABLE, DIMENSION(:) :: PARNAM
  DOUBLE PRECISION,              ALLOCATABLE, DIMENSION(:) :: PVAL
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: TFILE
  !
CONTAINS
  !-----------------------------------------------------------------------------
  SUBROUTINE JRN_INI()
    !   Delete jdispatch.fin, jrunner.fin, jrundep.rdy, jrunfail.fin,
    !   and jdispar.rdy files, if they exist
    USE UTILITIES
    USE PARALLEL_PROCESSING, ONLY: FNDISFIN, FNDISPAR, FNDISRDY, FNRUNDEP, &
                                   FNRUNFAIL, FNRUNFIN, PLL_WAIT, &
                                   PLL_CLOSE_AND_DELETE
    IMPLICIT NONE
    !
    !   Local variables
    INTEGER :: IDELUNIT, IOUNIT, ISTAT, K, KTRY
    LOGICAL :: LEX
    !
    !   Format statements
    1000 FORMAT(1X,'WARNING: Problem encountered deleting file: ',A,' (JRN_INI)')
    !
    ! Start by deleting jdispatch.rdy file
    IDELUNIT = UTL_GETUNIT(7,1000)
    ISTAT = PLL_CLOSE_AND_DELETE(IDELUNIT,FNDISRDY,1000)
    IF (ISTAT/=0) THEN
      WRITE(*,1000)TRIM(FNDISRDY)
    ENDIF
    !
    IOUNIT = UTL_GETUNIT(7,1000)
    K = 0
    INQUIRE(FILE=FNDISFIN,EXIST=LEX)
    IF (LEX) THEN
      KTRY = 0
      12 CONTINUE
      OPEN(IOUNIT,FILE=FNDISFIN,IOSTAT=ISTAT)
      IF (ISTAT>0 .AND. KTRY<21) THEN
        KTRY = KTRY+1
        CALL PLL_WAIT()
        GOTO 12
      ENDIF
      IF (ISTAT==0) THEN
        KTRY = 0
        14 CONTINUE
        CLOSE(IOUNIT,STATUS='DELETE',IOSTAT=ISTAT)
        IF (ISTAT>0) THEN
          IF (KTRY<21) THEN
            KTRY = KTRY+1
            CALL PLL_WAIT()
            GOTO 14          
          ENDIF
          WRITE(*,1000)FNDISFIN
          GOTO 20
        ENDIF
      ENDIF
    ENDIF
    20 CONTINUE
    !
    INQUIRE(FILE=FNRUNFIN,EXIST=LEX)
    IF (LEX) THEN
      KTRY = 0
      25 CONTINUE
      OPEN(IOUNIT,FILE=FNRUNFIN,IOSTAT=ISTAT)
      IF (ISTAT>0 .AND. KTRY<11) THEN
        KTRY = KTRY+1
        CALL PLL_WAIT()
        GOTO 25
      ENDIF
      KTRY = 0
      30 CONTINUE
      CLOSE(IOUNIT,STATUS='DELETE',IOSTAT=ISTAT)
      IF (ISTAT>0 .AND. KTRY<11) THEN
        KTRY = KTRY+1
        CALL PLL_WAIT()
        GOTO 30
      ENDIF
    ENDIF
    !
    INQUIRE(FILE=FNRUNDEP,EXIST=LEX)
    IF (LEX) THEN
      KTRY = 0
      35 CONTINUE
      OPEN(IOUNIT,FILE=FNRUNDEP,IOSTAT=ISTAT)
      IF (ISTAT>0 .AND. KTRY<11) THEN
        KTRY = KTRY+1
        CALL PLL_WAIT()
        GOTO 35
      ENDIF
      KTRY = 0
      40 CONTINUE
      CLOSE(IOUNIT,STATUS='DELETE',IOSTAT=ISTAT)
      IF (ISTAT>0 .AND. KTRY<11) THEN
        KTRY = KTRY+1
        CALL PLL_WAIT()
        GOTO 40
      ENDIF
    ENDIF
    !
    INQUIRE(FILE=FNRUNFAIL,EXIST=LEX)
    IF (LEX) THEN
      KTRY = 0
      45 CONTINUE
      OPEN(IOUNIT,FILE=FNRUNFAIL,IOSTAT=ISTAT)
      IF (ISTAT>0 .AND. KTRY<11) THEN
        KTRY = KTRY+1
        CALL PLL_WAIT()
        GOTO 45
      ENDIF
      KTRY = 0
      50 CONTINUE
      CLOSE(IOUNIT,STATUS='DELETE',IOSTAT=ISTAT)
      IF (ISTAT>0 .AND. KTRY<11) THEN
        KTRY = KTRY+1
        CALL PLL_WAIT()
        GOTO 50
      ENDIF
    ENDIF
    !
    K=0
    80 CONTINUE
    INQUIRE(FILE=FNDISPAR,EXIST=LEX)
    IF (LEX) THEN
      OPEN(IOUNIT,FILE=FNDISPAR,IOSTAT=ISTAT)
      IF (ISTAT==0) THEN
        CLOSE(IOUNIT,STATUS='DELETE',IOSTAT=ISTAT)
        IF (ISTAT.NE.0) THEN
          CALL PLL_WAIT()
          K = K+1
          IF (K>10) THEN
            WRITE(*,1000)FNDISPAR
            GOTO 80
          ENDIF
          GOTO 100
        ENDIF
      ENDIF
    ENDIF
    100 CONTINUE
  END SUBROUTINE JRN_INI
  !-----------------------------------------------------------------------------
  SUBROUTINE JRN_INI_ALLOC()
    IMPLICIT NONE
    ALLOCATE(CATGOR(NMOFILE),CINSTSET(LCIS),EXTNAM(NVEXT),EXTVAL(NVEXT),   &
             INSTRUCTFILE(NMOFILE),LOCINS(NINSTRUCT),MCVUSE(NCATS),  &
             MIFILE(NMIFILE),MOFILE(NMOFILE),MRKDL(NMOFILE),NW(NPT),   &
             PARNAM(NPT),PVAL(NPT),TFILE(NMIFILE))
    RETURN
  END SUBROUTINE JRN_INI_ALLOC
  !-----------------------------------------------------------------------------
  SUBROUTINE JRN_UEV_WRITE(IEXTOUT,IFAIL)
    !   Write dependent names and extracted values to an _ext data-exchange file
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(IN)  :: IEXTOUT
    INTEGER, INTENT(OUT) :: IFAIL
    !
    !   Local variables
    INTEGER :: I
    LOGICAL :: LOP
    !
    !   Format statements
    100 FORMAT('"DEPENDENT NAME", "VALUE"')
    120 FORMAT(A,',',1X,G25.16)
    !
    IFAIL = 0
    INQUIRE(IEXTOUT,OPENED=LOP)
    IF (.NOT. LOP) THEN
      IFAIL = 1
      RETURN
    ENDIF
    !
    !   Write header line
    WRITE(IEXTOUT,100)
    !
    !   Write dependent names and extracted values
    DO I=1,NVEXT
      WRITE(IEXTOUT,120)EXTNAM(I),EXTVAL(I)
    ENDDO
    !
    RETURN
  END SUBROUTINE JRN_UEV_WRITE
  !-----------------------------------------------------------------------------
  SUBROUTINE JRN_CLN()
    IMPLICIT NONE
    IF (ALLOCATED(CATGOR)) DEALLOCATE(CATGOR)
    IF (ALLOCATED(CINSTSET)) DEALLOCATE(CINSTSET)
    IF (ALLOCATED(EXTNAM)) DEALLOCATE(EXTNAM)
    IF (ALLOCATED(EXTVAL)) DEALLOCATE(EXTVAL)
    IF (ALLOCATED(INSTRUCTFILE)) DEALLOCATE(INSTRUCTFILE)
    IF (ALLOCATED(LOCINS)) DEALLOCATE(LOCINS)
    IF (ALLOCATED(MCVUSE)) DEALLOCATE(MCVUSE)
    IF (ALLOCATED(MIFILE)) DEALLOCATE(MIFILE)
    IF (ALLOCATED(MOFILE)) DEALLOCATE(MOFILE)
    IF (ALLOCATED(MRKDL)) DEALLOCATE(MRKDL)
    IF (ALLOCATED(NW)) DEALLOCATE(NW)
    IF (ALLOCATED(PARNAM)) DEALLOCATE(PARNAM)
    IF (ALLOCATED(PVAL)) DEALLOCATE(PVAL)
    IF (ALLOCATED(TFILE)) DEALLOCATE(TFILE)
    RETURN
  END SUBROUTINE JRN_CLN
  !-----------------------------------------------------------------------------
END MODULE JRUNNERMOD