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
MODULE PARALLEL_PROCESSING
  USE DATATYPES
  USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
  PRIVATE
  !
  !   Selected data are PUBLIC:
  PUBLIC :: AUTOSTOPRUNNERS, COMMAND, FNDISFIN, FNDISPAR, FNDISRDY, FNRUNDEP,  &
            FNRUNFAIL, FNRUNFIN, FNRUNRDY, LLPTRPLLCTRL, LLPTRPLLRUN,   &
            SNAME, WAIT, WAITRUNNERS ! Added WAITRUNNERS 6/21/2010 ERB
  !
  !   Selected subprograms are PUBLIC:
  PUBLIC :: PLL_INI_DISPATCHER, PLL_INI_RUNNER_DIM, PLL_INI_RUNNER_POP,  &
            PLL_ADA, PLL_EXE, PLL_EXE_FUNC, PLL_EXT, PLL_CLN, PLL_MAKE_RUNS,   &
            PLL_READ_DISPAR, PLL_RUNNER_STOP, PLL_STOP_RUNNERS, PLL_WAIT,   &
            PLL_WRITE_RUNDEP, PLL_CLOSE_AND_DELETE
  !
  !   PUBLIC data
  LOGICAL                       :: AUTOSTOPRUNNERS=.TRUE.
  CHARACTER(LEN=MAX_STRING_LEN) :: COMMAND = ' '
  !   Names of signal files
  CHARACTER(LEN=13), PARAMETER  :: FNDISFIN = 'jdispatch.fin'
  CHARACTER(LEN=11), PARAMETER  :: FNDISPAR = 'jdispar.rdy'
  CHARACTER(LEN=13), PARAMETER  :: FNDISRDY = 'jdispatch.rdy'
  CHARACTER(LEN=11), PARAMETER  :: FNRUNDEP = 'jrundep.rdy'
  CHARACTER(LEN=12), PARAMETER  :: FNRUNFAIL = 'jrunfail.fin'
  CHARACTER(LEN=11), PARAMETER  :: FNRUNFIN = 'jrunner.fin'
  CHARACTER(LEN=11), PARAMETER  :: FNRUNRDY = 'jrunner.rdy'
  !
  TYPE (LLIST),    POINTER      :: LLPTRPLLCTRL !  Parallel control data
  TYPE (LLIST),    POINTER      :: LLPTRPLLRUN  !  Parallel runners data
  CHARACTER(LEN=20)             :: SNAME = ' '
  DOUBLE PRECISION              :: WAIT = 0.001D0, WAITRUNNERS = 0.001D0 ! 6/21/2010 ERB
  !
  !   PRIVATE data
  INTEGER,                     ALLOCATABLE, DIMENSION(:,:) :: BTIME       ! Run begin times
  LOGICAL                                                  :: DOPLL
  CHARACTER(LEN=MAX_STRING_LEN)                            :: ERRRUNNER = ' '
  INTEGER,                       ALLOCATABLE, DIMENSION(:) :: IRUNNERSTAT, KOD
  INTEGER                                                  :: IVERBRUNNER = 3
  INTEGER, PARAMETER                                       :: NRUNNERCOLS = 3
  INTEGER                                                  :: NUMRUNNERS
  CHARACTER(LEN=20)                                        :: OS_DIS    ! Operating system on which dispatcher runs
  CHARACTER(LEN=20)                                        :: OS_PLL    ! Operating system of both dispatcher and runner
  CHARACTER(LEN=20)                                        :: OS_TEMP
  CHARACTER(LEN=6)                                         :: RENAME_DIS  ! "rename" command on dispatcher
  CHARACTER(LEN=40),                DIMENSION(NRUNNERCOLS) :: RUNNERCOLS
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: RUNNERDIR
  CHARACTER(LEN=20),             ALLOCATABLE, DIMENSION(:) :: RUNNERNAME
  CHARACTER(LEN=MAX_STRING_LEN)                            :: RUNRECFIL
  DOUBLE PRECISION,              ALLOCATABLE, DIMENSION(:) :: RUNTIME
  CHARACTER(LEN=6)                                         :: SRENAME = 'ren' ! "rename" command on runner
  DOUBLE PRECISION                                         :: TIMEOUTFAC = 3.0D0
  !
  DATA RUNNERCOLS /'RUNNERNAME','RUNNERDIR ','RUNTIME  '/
  !
  !   IRUNNERSTAT flags:
  !     <0 -- Runner nonresponsive; absolute value indicates last
  !              status before nonresponse
  !      0 -- Initial status
  !      1 or greater -- Runner recognized as activated
  !      2 -- jdispatch.rdy file (FNDISRDY) copied to runner directory
  !      3 -- jdispar.rdy file (FNDISPAR) copied to runner directory;
  !              presumably, model is running
  !      4 -- jrundep.rdy file (FNRUNDEP) present in runner directory;
  !              model run done
  !      8 -- jdispatch.fin file (FNDISFIN) copied to runner directory;
  !              signal to stop j_runner executable
  !      9 -- jrunner.fin file (FNRUNFIN) found, indicating j_runner
  !              executable has stopped execution
  !     10 -- jrunfail.fin file (FNRUNFAIL) found, indicating j_runner
  !              executable has stopped execution due to error
  !     11 -- runner is overdue
  !
CONTAINS
  !-----------------------------------------------------------------------------
  SUBROUTINE PLL_INI_DISPATCHER(INUNIT,IOUT,LCIS,NCATS,NINSTRUCT,NMIFILE,   &
                            NMOFILE,NOPNT,NPT,NUMLADV,NVEXT,PRECISPRO,   &
                            CATGOR,CINSTSET,EXTNAM,INSTRUCTFILE,LOCINS,   &
                            MCVUSE,MIFILE,MOFILE,MRKDL,NW,PARNAM,TFILE,   &
                            PARACTIVE)
    !   Initialize PARALLEL module: Read parallel control and runner data and
    !   allocate and populate arrays.
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: AMESSAGE, ERRSUB, IVERB, LENDNAM, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                             INTENT(IN)  :: INUNIT
    INTEGER,                                             INTENT(IN)  :: IOUT
    INTEGER,                                             INTENT(IN)  :: LCIS
    INTEGER,                                             INTENT(IN)  :: NCATS
    INTEGER,                                             INTENT(IN)  :: NINSTRUCT
    INTEGER,                                             INTENT(IN)  :: NMIFILE
    INTEGER,                                             INTENT(IN)  :: NMOFILE
    INTEGER,                                             INTENT(IN)  :: NOPNT
    INTEGER,                                             INTENT(IN)  :: NPT
    INTEGER,                                             INTENT(IN)  :: NUMLADV
    INTEGER,                                             INTENT(IN)  :: NVEXT
    INTEGER,                                             INTENT(IN)  :: PRECISPRO
    CHARACTER(LEN=6),              DIMENSION(NMOFILE),   INTENT(IN)  :: CATGOR ! Model-calc'd val. cat. for each model-output file
    CHARACTER(LEN=1),              DIMENSION(LCIS),      INTENT(IN)  :: CINSTSET  ! holds compressed instruction set
    CHARACTER(LEN=LENDNAM),        DIMENSION(NVEXT),     INTENT(IN)  :: EXTNAM
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NMOFILE),   INTENT(IN)  :: INSTRUCTFILE ! Instruction file names
    INTEGER,                       DIMENSION(NINSTRUCT), INTENT(IN)  :: LOCINS    ! pointer to instructions
    LOGICAL,                       DIMENSION(NCATS),     INTENT(IN)  :: MCVUSE
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NMIFILE),   INTENT(IN)  :: MIFILE
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NMOFILE),   INTENT(IN)  :: MOFILE ! Model output file names
    CHARACTER(LEN=1),              DIMENSION(NMOFILE),   INTENT(IN)  :: MRKDL  ! Marker delimiters
    INTEGER,                       DIMENSION(NPT),       INTENT(IN)  :: NW
    CHARACTER(LEN=12),             DIMENSION(NPT),       INTENT(IN)  :: PARNAM
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NMIFILE),   INTENT(IN)  :: TFILE
    LOGICAL,                                             INTENT(OUT) :: PARACTIVE
    !
    !   Local variables
    INTEGER :: I, IERR, ISTAT, IU, KACTIVE, KCTRL, KLOOPS, LD, MORE
    CHARACTER(LEN=40), DIMENSION(0) :: COLNAMES
    CHARACTER(LEN=MAX_STRING_LEN)   :: FNAME, FRUNNERNAME
    TYPE (LLIST),      POINTER      :: TAIL
    LOGICAL :: LEX
    !
    !   Format statements
    30 FORMAT(1X,'Runner "',A,'" is active')
    40 FORMAT(1X,'File ',A,' exists on runner ',A,   &
              ' but access to the file is prevented. ',/,   &
              ' Runner ',A,' is assumed to be inactive.')
    50 FORMAT(/,1X,I4,' runner(s) are active')
    60 FORMAT(/,1X,'*** ERROR: No runners are active')
    100 FORMAT(//,1X,'PARALLEL PROCESSING option is enabled.')
    150 FORMAT(1X,'Operating system under which dispatcher program runs: ',A)
    200 FORMAT(1X,'Number of runners = ',I4)
    250 FORMAT(1X,'WARNING: Unrecognized operating system: "',A,'"',/,   &
               1X,'OPERATINGSYSTEM defaults to: "',A,'"')
    300 FORMAT(/,1X,'Runner number ',I4,':  Name = ',A,/,   &
               1X,'Directory = ',A,/,   &
               1X,'Estimated runtime = ',G11.4,' seconds')
    350 FORMAT(/,1X,'ERROR: The following directory name does not end',   &
                  ' in a back slash or forward slash:',/,1X,A)
    400 FORMAT(A)
    !
    NULLIFY(LLPTRPLLCTRL,LLPTRPLLRUN,TAIL)
    !   Assign defaults
    OS_PLL = ' '
    OS_TEMP = 'WINDOWS'
    PARACTIVE = .FALSE.
    DOPLL = .FALSE.
    KCTRL = 0
    NUMRUNNERS = 0
    RUNRECFIL = ' '
    WAIT = 0.001D0
    WAITRUNNERS = 0.001D0  ! 6/21/2010 ERB
    !
    CALL UTL_READBLOCK(0,'PARALLEL_CONTROL',COLNAMES,INUNIT,IOUT,'*',   &
                       .FALSE.,LLPTRPLLCTRL,TAIL,KCTRL)
    CALL UTL_READBLOCK(NRUNNERCOLS,'PARALLEL_RUNNERS',RUNNERCOLS,INUNIT,   &
                       IOUT,'RUNNERNAME',.FALSE.,LLPTRPLLRUN,TAIL,NUMRUNNERS)
    IF (KCTRL>0) THEN
      CALL UTL_FILTER(IERR,LLPTRPLLCTRL,IOUT,'PARALLEL',DOPLL)
      PARACTIVE = DOPLL
      IF (DOPLL) THEN
        IF (NUMRUNNERS==0) THEN
          AMESSAGE = 'PARALLEL is TRUE, but zero runners have been defined'
          CALL UTL_SUBERROR(ERRSUB)
        ENDIF
        ALLOCATE(BTIME(8,NUMRUNNERS),IRUNNERSTAT(NUMRUNNERS),   &
                 KOD(NUMRUNNERS), RUNTIME(NUMRUNNERS),   &
                 RUNNERDIR(NUMRUNNERS),RUNNERNAME(NUMRUNNERS))
        !
        !   Populate arrays with defaults as appropriate
        IRUNNERSTAT = 0
        KOD = 0
        RUNTIME = 10.0D0
        !
        !   Populate arrays and assign variables
        CALL UTL_FILTERLIST(LLPTRPLLRUN,IOUT,'RUNNERNAME',NUMRUNNERS,IERR,   &
                            RUNNERNAME,MORE)
        CALL UTL_FILTERLIST(LLPTRPLLRUN,IOUT,'RUNNERDIR',NUMRUNNERS,IERR,   &
                            RUNNERDIR,MORE)
        CALL UTL_FILTERLIST(LLPTRPLLRUN,IOUT,'RUNTIME',NUMRUNNERS,IERR,   &
                            RUNTIME,MORE)
        CALL UTL_FILTER(IERR,LLPTRPLLCTRL,IOUT,'WAIT',WAIT)
        CALL UTL_FILTER(IERR,LLPTRPLLCTRL,IOUT,'WAITRUNNERS',WAITRUNNERS) ! 6/21/2010 ERB
        CALL UTL_FILTER(IERR,LLPTRPLLCTRL,IOUT,'RUNRECORD',RUNRECFIL)
        CALL UTL_FILTER(IERR,LLPTRPLLCTRL,IOUT,'VERBOSERUNNER',IVERBRUNNER)
        CALL UTL_FILTER(IERR,LLPTRPLLCTRL,IOUT,'AUTOSTOPRUNNERS',   &
                        AUTOSTOPRUNNERS)
        CALL UTL_FILTER(IERR,LLPTRPLLCTRL,IOUT,'OSDISPATCHER',OS_TEMP)
        CALL UTL_FILTER(IERR,LLPTRPLLCTRL,IOUT,'OPERATINGSYSTEM',OS_PLL)
        CALL UTL_FILTER(IERR,LLPTRPLLCTRL,IOUT,'TIMEOUTFACTOR',TIMEOUTFAC)
        !
        IF (OS_PLL .NE. ' ') OS_TEMP = OS_PLL
        CALL UTL_CASETRANS(OS_TEMP,'hi')
        OS_DIS = OS_TEMP
        !   Echo input
        IF (IVERB>2) THEN
          WRITE(IOUT,100)
          WRITE(IOUT,150)TRIM(OS_DIS)
          WRITE(IOUT,200)NUMRUNNERS
        ENDIF
        !
        !   Assign variables dependent on operating system of dispatcher program
        IF (OS_TEMP == 'WINDOWS') THEN
          RENAME_DIS = 'ren'
        ELSEIF (OS_TEMP == 'DOS') THEN
          OS_DIS = 'WINDOWS'
          RENAME_DIS = 'ren'
        ELSEIF (OS_TEMP == 'UNIX') THEN
          RENAME_DIS = 'mv'
        ELSEIF (OS_TEMP == 'LINUX') THEN
          OS_DIS = 'UNIX'
          RENAME_DIS = 'mv'
        ELSE
          OS_DIS = 'WINDOWS'
          IF (IVERB>0) WRITE(IOUT,250)TRIM(OS_TEMP),TRIM(OS_DIS)
          RENAME_DIS = 'ren'
        ENDIF

        DO I=1,NUMRUNNERS
          LD = LEN_TRIM(RUNNERDIR(I))
          IF (IVERB>2) WRITE(IOUT,300)I,TRIM(RUNNERNAME(I)),   &
              TRIM(RUNNERDIR(I)),RUNTIME(I)
          IF (RUNNERDIR(I)(LD:LD) .NE. '/' .AND.   &
              RUNNERDIR(I)(LD:LD) .NE. '\') THEN
            WRITE(IOUT,350)TRIM(RUNNERDIR(I))
            CALL PLL_STOP_RUNNERS()
          ENDIF
          !
          !   Delete jrunner.rdy and jdispatch.rdy files
          FRUNNERNAME = TRIM(RUNNERDIR(I))//FNRUNRDY
          INQUIRE(FILE=FRUNNERNAME,EXIST=LEX)
          IF (LEX) THEN
            IU = UTL_GETUNIT(7,1000)
            OPEN(IU,FILE=FRUNNERNAME,ERR=310,IOSTAT=ISTAT)
            310 CONTINUE
            ISTAT = PLL_CLOSE_AND_DELETE(IU,FRUNNERNAME)
          ENDIF
          FRUNNERNAME = TRIM(RUNNERDIR(I))//FNDISRDY
          INQUIRE(FILE=FRUNNERNAME,EXIST=LEX)
          IF (LEX) THEN
            IU = UTL_GETUNIT(7,1000)
            OPEN(IU,FILE=FRUNNERNAME,ERR=320,IOSTAT=ISTAT)
            320 CONTINUE
            ISTAT = PLL_CLOSE_AND_DELETE(IU,FRUNNERNAME)
          ENDIF
          !
        ENDDO
        IF (IVERB>2) WRITE(IOUT,400)' '
        !
        KACTIVE = 0
        KLOOPS = 0
        340 CONTINUE
        CALL PLL_WAIT(1.0D0)   ! Allow time for runners to recreate FNRUNRDY file
        !   Determine status of runners
        DO I=1,NUMRUNNERS
          IF (IRUNNERSTAT(I)==0) THEN
            FRUNNERNAME = TRIM(RUNNERDIR(I))//FNRUNRDY
            INQUIRE(FILE=FRUNNERNAME,EXIST=LEX)
            IF (LEX) THEN
              IU = UTL_GETUNIT(7,1000)
              OPEN(IU,FILE=FRUNNERNAME,ERR=410,IOSTAT=ISTAT)
              410 CONTINUE
              ISTAT = PLL_CLOSE_AND_DELETE(IU,FRUNNERNAME)
              IF (ISTAT==0) THEN
                IRUNNERSTAT(I) = 1
                KOD(I) = 0
                IF (IVERB>2) WRITE(*,30)TRIM(RUNNERNAME(I))
                KACTIVE = KACTIVE+1
              ELSE
                WRITE(*,40)TRIM(FNRUNRDY),TRIM(RUNNERNAME(I)),TRIM(RUNNERNAME(I))
                IRUNNERSTAT(I) = -1
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        IF (KACTIVE==0 .AND. KLOOPS<20) THEN
          KLOOPS = KLOOPS+1
          GOTO 340
        ENDIF
        !
        IF (IVERB>2) WRITE(*,50)KACTIVE
        IF (KACTIVE==0) THEN
          WRITE(*,60)
          IF (IVERB>3) WRITE(*,*)' PLL_INI_DISPATCHER calling PLL_STOP_RUNNERS'
          CALL PLL_STOP_RUNNERS()
          CALL UTL_STOP()
        ENDIF
        !
        !   Create jdispatch.rdy files to pass model-interaction data to runners
        DO I=1,NUMRUNNERS
          IF (IRUNNERSTAT(I)==1) THEN
            !   Create "jdispatch.rdy" file for this runner
            CALL PLL_WRITE_DISRDY(I,LCIS,NCATS,NINSTRUCT,NMIFILE,NMOFILE,   &
                                  NOPNT,NPT,NUMLADV,NVEXT,PRECISPRO,   &
                                  CATGOR,CINSTSET,EXTNAM,INSTRUCTFILE,   &
                                  LOCINS,MCVUSE,MIFILE,MOFILE,MRKDL,NW,   &
                                  PARNAM,TFILE)
          ENDIF
        ENDDO
        !
        FNAME = 'jparallel.interrupt'
        INQUIRE(FILE=FNAME,EXIST=LEX)
        IF (LEX) THEN
          IU = UTL_GETUNIT(7,1000)
          OPEN(IU,FILE=FNAME)
          CLOSE(IU,STATUS='DELETE',IOSTAT=ISTAT)
        ENDIF
      ENDIF
    ENDIF
    !
    RETURN
  END SUBROUTINE PLL_INI_DISPATCHER
  !-----------------------------------------------------------------------------
  SUBROUTINE PLL_INI_RUNNER_DIM(IOUNIT,RESET,LCIS,NCATS,NINSTRUCT,NMIFILE,   &
                               NMOFILE,NOPNT,NPT,NUMLADV,NVEXT,   &
                               PRECISPRO,INIMESS)
    !   Look for jdispatch.rdy file.  When found, read scalar values from it.
    USE GLOBAL_DATA, ONLY: AMESSAGE, IVERB
    USE UTILITIES
    USE MODEL_IO, ONLY: MIO_INI_ALLOC
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                    INTENT(IN)    :: IOUNIT
    LOGICAL,                    INTENT(INOUT) :: RESET
    INTEGER,                    INTENT(OUT)   :: LCIS
    INTEGER,                    INTENT(OUT)   :: NCATS
    INTEGER,                    INTENT(OUT)   :: NINSTRUCT
    INTEGER,                    INTENT(OUT)   :: NMIFILE ! Number of model-input files
    INTEGER,                    INTENT(OUT)   :: NMOFILE ! Number of model-output files
    INTEGER,                    INTENT(OUT)   :: NOPNT
    INTEGER,                    INTENT(OUT)   :: NPT
    INTEGER,                    INTENT(OUT)   :: NUMLADV
    INTEGER,                    INTENT(OUT)   :: NVEXT   ! Number of dependent values to extract
    INTEGER,                    INTENT(OUT)   :: PRECISPRO
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN)    :: INIMESS
    !
    !   Local variables
    INTEGER :: IFAIL, ISTAT, ISTAT1, ISTAT2, ISTAT3, K, KFND, KTRY, KTRY1, IU
    LOGICAL :: LEX
    CHARACTER(LEN=5) :: FINMESS
    CHARACTER(LEN=MAX_STRING_LEN) :: FILERR
    CHARACTER(LEN=20) :: VARERR = ' '
    !
    !   Format statements
    100 FORMAT(1X,A,/)
    150 FORMAT(A)
    170 FORMAT(1X,'File "',A,'" has been created.')
    200 FORMAT(F20.0)
    250 FORMAT(I10)
    300 FORMAT(1X,'File "',A,'" found...')
    320 FORMAT(1X,'VERBOSERUNNER = ',I2)
    340 FORMAT(1X,'This is runner "',A,'".  The rename command is "',A,'"')
    700 FORMAT(1X,'Error reading from file "',A,'"')
    720 FORMAT(1X,'Error reading variable "',A,'" from file "',A,'"')
    740 FORMAT(1X,'Closing file and making attempt: ',I2)
    800 FORMAT(/,1X,'Runner "',A,'" received signal to stop execution')
    810 FORMAT(/,1X,'Runner received signal to stop execution')
    820 FORMAT(/,1X,'Runner "',A,   &
        '" resetting to continue execution (PLL_INI_RUNNER_DIM)',/)
    830 FORMAT(/,1X,   &
        'Runner resetting to continue execution (PLL_INI_RUNNER_DIM)',/)
    840 FORMAT(/,1X,'Runner cannot open file ',A)
    850 FORMAT(/,1X,'Runner cannot close file ',A)
    !
    IF (IVERB>2) THEN
      IF (PRESENT(INIMESS)) WRITE(*,100)TRIM(INIMESS)
    ENDIF
    !
    !   Write FNRUNRDY file to signal that runner is active
    KTRY = 0
    3 CONTINUE
    OPEN(IOUNIT,FILE=FNRUNRDY,STATUS='REPLACE',IOSTAT=ISTAT)
    IF (ISTAT .NE. 0) THEN
      KTRY = KTRY+1
      CALL PLL_WAIT(0.05D0)
      IF (KTRY<10) GOTO 3
      WRITE(*,840)TRIM(FNRUNRDY)
      RESET = .TRUE.
      RETURN
    ENDIF
    IF (IVERB>4) WRITE(*,170)TRIM(FNRUNRDY)
    !
    KTRY = 0
    6 CONTINUE
    CLOSE(IOUNIT,STATUS='KEEP',IOSTAT=ISTAT)
    IF (ISTAT .NE. 0) THEN
      KTRY = KTRY+1
      CALL PLL_WAIT(0.05D0)
      IF (KTRY<10) GOTO 6
      WRITE(*,850)TRIM(FNRUNRDY)
      RESET = .TRUE.
      RETURN
    ENDIF
    !
    !   Look for FNDISRDY here, and initialize arrays
    !
    K = 0
    KFND = 0
    10 CONTINUE
    INILOOP: DO WHILE (.TRUE.)
      !   Ensure that FNRUNRDY file exists, to show runner is alive
      INQUIRE(FILE=FNRUNRDY,EXIST=LEX)
      IF (.NOT. LEX) THEN
        IU = UTL_GETUNIT(7,1000)
        KTRY1 = 0
        20 CONTINUE
        OPEN(IU,FILE=FNRUNRDY,IOSTAT=ISTAT)
        IF (ISTAT>0 .AND. KTRY1<11) THEN
          KTRY1 = KTRY1+1
          CALL PLL_WAIT()
          GOTO 20
        ENDIF
        KTRY1 = 0
        30 CONTINUE
        CLOSE(IU,STATUS='KEEP',IOSTAT=ISTAT)
        IF (ISTAT>0 .AND. KTRY1<11) THEN
          KTRY1 = KTRY1+1
          CALL PLL_WAIT()
          GOTO 30
        ENDIF
        IF (IVERB>4) WRITE(*,170)TRIM(FNRUNRDY)
      ENDIF
      !   Look for FNDISRDY file, indicating dispatcher is in communication
      !   with runner
      INQUIRE(FILE=FNDISRDY,EXIST=LEX)
      IF (LEX) THEN
        IF (IVERB>2 .AND. KFND==0) WRITE(*,300)FNDISRDY
        !   Read initialization data provided by dispatcher in jdispatch.rdy
        OPEN(IOUNIT,FILE=FNDISRDY,STATUS='OLD',IOSTAT=ISTAT)
        IF (ISTAT .NE. 0) THEN
          KFND = KFND+1
          GOTO 50
        ENDIF
        FILERR=FNDISRDY
        VARERR='SNAME'
        READ(IOUNIT,150,ERR=990,END=990)SNAME
        VARERR='SRENAME'
        READ(IOUNIT,150,ERR=990,END=990)SRENAME
        VARERR='WAIT'
        READ(IOUNIT,200,ERR=990,END=990)WAIT
        VARERR='IVERB'
        READ(IOUNIT,250,ERR=990,END=990)IVERB
        VARERR='LCIS'
        READ(IOUNIT,250,ERR=990,END=990)LCIS
        VARERR='NINSTRUCT'
        READ(IOUNIT,250,ERR=990,END=990)NINSTRUCT
        VARERR='NMIFILE'
        READ(IOUNIT,250,ERR=990,END=990)NMIFILE
        VARERR='NMOFILE'
        READ(IOUNIT,250,ERR=990,END=990)NMOFILE
        VARERR='NPT'
        READ(IOUNIT,250,ERR=990,END=990)NPT
        VARERR='NOPNT'
        READ(IOUNIT,250,ERR=990,END=990)NOPNT
        VARERR='PRECISPRO'
        READ(IOUNIT,250,ERR=990,END=990)PRECISPRO
        VARERR='NCATS'
        READ(IOUNIT,250,ERR=990,END=990)NCATS
        VARERR='NVEXT'
        READ(IOUNIT,250,ERR=990,END=990)NVEXT
        VARERR='NUMLADV'
        READ(IOUNIT,250,ERR=990,END=990)NUMLADV
        !
        IF (IVERB>2) WRITE(*,320) IVERB
        IF (IVERB>3) WRITE(*,340) TRIM(SNAME),TRIM(SRENAME)
        !
        !   Initialize the MIO module for model-input files
        CALL MIO_INI_ALLOC(IFAIL,NPT)
        IF (IFAIL .NE. 0) THEN
          ERRRUNNER = 'Error: runner cannot initialize the Model_IO module.'
          WRITE(*,*) TRIM(ERRRUNNER)
          CALL PLL_RUNNER_STOP(1)
        ENDIF
        !
        RETURN
      ENDIF
      50 CONTINUE
      !
      !   Look for FNDISFIN file, indicating runner
      !   should stop execution or reset
      INQUIRE(FILE=FNDISFIN,EXIST=LEX)
      IF (LEX) THEN
        IF (IVERB>2)WRITE(*,300)TRIM(FNDISFIN)
        KTRY = 0
        560 CONTINUE
        OPEN(IOUNIT,FILE=FNDISFIN,IOSTAT=ISTAT1)
        IF (ISTAT1 .NE. 0) THEN
          KTRY = KTRY+1
          IF (KTRY<10) THEN
            CALL PLL_WAIT()
            GOTO 560
          ELSE
            FINMESS=' '
            WRITE(*,'(A)')' Warning: WAIT time may be too small'
          ENDIF
        ELSE
          READ(IOUNIT,150,END=590,ERR=590,IOSTAT=ISTAT2)FINMESS
          590 CONTINUE
          KTRY = 0
          595 CONTINUE
          CLOSE(IOUNIT,STATUS='KEEP',IOSTAT=ISTAT3)
          IF (ISTAT3 .NE. 0) THEN
            KTRY = KTRY+1
            IF (KTRY<10) THEN
              CALL PLL_WAIT()
              GOTO 595
            ELSE
              WRITE(*,'(A)')' Warning: WAIT time may be too small'
            ENDIF
          ENDIF
          !
          IF (FINMESS=='RESET') THEN
            RESET=.TRUE.
            IF (SNAME==' ') THEN
              WRITE(*,830)
            ELSE
              WRITE(*,820)TRIM(SNAME)
            ENDIF
            !
            RETURN
            !
          ELSE
            IF (SNAME==' ') THEN
              WRITE(*,810)
            ELSE
              WRITE(*,800)TRIM(SNAME)
            ENDIF
          ENDIF
          CALL PLL_RUNNER_STOP()
        ENDIF
      ENDIF
    ENDDO INILOOP
    !
    990 CONTINUE
    WRITE(*,700)TRIM(FILERR)
    IF (VARERR .NE. ' ') THEN
      WRITE(*,720)TRIM(VARERR),TRIM(FNDISRDY)
      ERRRUNNER = ' '
      WRITE(ERRRUNNER,720)TRIM(VARERR),TRIM(FNDISRDY)
      K = K+1
      IF (K<11) THEN
        CLOSE(IOUNIT,IOSTAT=ISTAT)
        CALL PLL_WAIT(WAIT)
        WRITE(*,740)K
        GOTO 10
      ELSE
        WRITE(*,'(A)')' Warning: WAIT time may be too small'
      ENDIF
    ENDIF
    AMESSAGE = 'Error in subroutine PLL_INI_RUNNER_DIM'
    CALL PLL_RUNNER_STOP(1)
    !
    RETURN
  END SUBROUTINE PLL_INI_RUNNER_DIM
  !-----------------------------------------------------------------------------
  SUBROUTINE PLL_INI_RUNNER_POP(IOUNIT,LCIS,NCATS,NINSTRUCT,NMIFILE,NMOFILE,   &
                               NPT,NUMLADV,NVEXT,CATGOR,CINSTSET,EXTNAM,   &
                               INSTRUCTFILE,LOCINS,MCVUSE,MIFILE,   &
                               MOFILE,MRKDL,NW,PARNAM,TFILE)
    !   Read array values from jdispatch.rdy file and populate arrays,
    !   then close and delete the file
    USE GLOBAL_DATA, ONLY: AMESSAGE, IVERB, LENDNAM, MAX_STRING_LEN
    USE MODEL_IO, ONLY: MIO_INI_INPUTFILES_RUNNER, MIO_INI_INSTRUCT_RUNNER,   &
                        MIO_INI_OUTPUTFILES_RUNNER, MIO_INI_TEMPLATE
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                            INTENT(IN)  :: IOUNIT
    INTEGER,                                            INTENT(IN)  :: LCIS
    INTEGER,                                            INTENT(IN)  :: NCATS
    INTEGER,                                            INTENT(IN)  :: NINSTRUCT
    INTEGER,                                            INTENT(IN)  :: NMIFILE
    INTEGER,                                            INTENT(IN)  :: NMOFILE
    INTEGER,                                            INTENT(IN)  :: NPT
    INTEGER,                                            INTENT(IN)  :: NUMLADV
    INTEGER,                                            INTENT(IN)  :: NVEXT
    CHARACTER(LEN=6),              DIMENSION(NMOFILE),  INTENT(OUT) :: CATGOR ! Model-calc'd vaL. cat. for each model-output file
    CHARACTER(LEN=1),              DIMENSION(LCIS),     INTENT(OUT) :: CINSTSET ! holds compressed instruction set
    CHARACTER(LEN=LENDNAM),        DIMENSION(NVEXT),    INTENT(OUT) :: EXTNAM
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NMOFILE),  INTENT(OUT) :: INSTRUCTFILE ! Instruction file names
    INTEGER,                       DIMENSION(NINSTRUCT), INTENT(OUT) :: LOCINS ! pointer to instructions
    LOGICAL,                       DIMENSION(NCATS),    INTENT(OUT) :: MCVUSE
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NMIFILE),  INTENT(OUT) :: MIFILE
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NMOFILE),  INTENT(OUT) :: MOFILE ! Model output file names
    CHARACTER(LEN=1),              DIMENSION(NMOFILE),  INTENT(OUT) :: MRKDL  ! Marker delimiters
    INTEGER,                       DIMENSION(NPT),      INTENT(OUT) :: NW
    CHARACTER(LEN=12),             DIMENSION(NPT),      INTENT(OUT) :: PARNAM
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NMIFILE),  INTENT(OUT) :: TFILE
    !
    !   Local variables
    INTEGER :: I, IFAIL, ISTAT, K
    !
    !   Format statements
    100 FORMAT(A)
    200 FORMAT(6(1X,A12))
    300 FORMAT(6(1X,I4,8X))
    360 FORMAT(3(1X,A))
    380 FORMAT(10(1X,L1))
    420 FORMAT(10(1X,A6))
    440 FORMAT(40(1X,A1))
    450 FORMAT(1X,'Deleting file "',A,'"')
    460 FORMAT(80A1)
    480 FORMAT(8I10)
    500 FORMAT(1X,'Error reading from file "',A,'"')
    !
    READ(IOUNIT,100,ERR=990,END=990)(MIFILE(I),I=1,NMIFILE)
    READ(IOUNIT,100,ERR=990,END=990)(TFILE(I),I=1,NMIFILE)
    READ(IOUNIT,200,ERR=990,END=990)(PARNAM(I),I=1,NPT)
    READ(IOUNIT,300,ERR=990,END=990)(NW(I),I=1,NPT)
    READ(IOUNIT,420,ERR=990,END=990)(CATGOR(I),I=1,NMOFILE)
    READ(IOUNIT,360,ERR=990,END=990)(EXTNAM(I),I=1,NVEXT)
    READ(IOUNIT,100,ERR=990,END=990)(INSTRUCTFILE(I),I=1,NMOFILE)
    READ(IOUNIT,100,ERR=990,END=990)(MOFILE(I),I=1,NMOFILE)
    READ(IOUNIT,440,ERR=990,END=990)(MRKDL(I),I=1,NMOFILE)
    READ(IOUNIT,380,ERR=990,END=990)(MCVUSE(I),I=1,NCATS)
    READ(IOUNIT,460,ERR=990,END=990)(CINSTSET(I),I=1,LCIS)
    READ(IOUNIT,480,ERR=990,END=990)(LOCINS(I),I=1,NINSTRUCT)
    IF(IVERB>3)WRITE(*,450)FNDISRDY
    !
    !   Initialize model-input and template file arrays
    CALL MIO_INI_INPUTFILES_RUNNER(IFAIL,NMIFILE,MIFILE,TFILE)
    IF (IFAIL .NE. 0) THEN
      ERRRUNNER = 'Error: Runner cannot initialize model-input file data.'
      CALL PLL_RUNNER_STOP(1)
    ENDIF
    CALL MIO_INI_TEMPLATE(IFAIL,NPT,PARNAM,NW)
    IF (IFAIL .NE. 0) THEN
      ERRRUNNER = 'Error: Runner cannot initialize template file data.'
      CALL PLL_RUNNER_STOP(1)
    ENDIF
    !
    !   Initialize model-output and instruction file arrays
    CALL MIO_INI_OUTPUTFILES_RUNNER(IFAIL,NMOFILE,CATGOR,INSTRUCTFILE,MOFILE,MRKDL)
    IF (IFAIL .NE. 0) THEN
      ERRRUNNER = 'Error: Runner cannot initialize model-output file data.'
      CALL PLL_RUNNER_STOP(1)
    ENDIF
    CALL MIO_INI_INSTRUCT_RUNNER(IFAIL,LCIS,NINSTRUCT,NUMLADV,NVEXT,CINSTSET,   &
                                LOCINS)
    IF (IFAIL .NE. 0) THEN
      ERRRUNNER = 'Error: Runner cannot initialize instruction data.'
      CALL PLL_RUNNER_STOP(1)
    ENDIF
    !
    RETURN
    !
    990 CONTINUE
    WRITE(*,500)FNDISRDY
    ERRRUNNER = ' '
    WRITE(ERRRUNNER,500)FNDISRDY
    AMESSAGE = ' '
    WRITE(AMESSAGE,500)FNDISRDY
    CALL PLL_RUNNER_STOP(1)
  END SUBROUTINE PLL_INI_RUNNER_POP
  ! ----------------------------------------------------------------------------
  SUBROUTINE PLL_ADA(NPT,NOPNT,PARNAM,PRECISPRO,NW,PVAL)
    !   Write model-input file(s).  This subroutine is intended to be
    !   called by a runner.
    USE GLOBAL_DATA, ONLY: AMESSAGE, IVERB
    USE MODEL_IO, ONLY: MIO_ADA_WRITEFILES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN)    :: NPT     ! Number of parameters
    INTEGER,                                 INTENT(IN)    :: NOPNT   ! Decimal point protocol
    CHARACTER(LEN=12),   DIMENSION(NPT),     INTENT(IN)    :: PARNAM    ! Parameter names
    INTEGER,                                 INTENT(IN)    :: PRECISPRO ! Precision protocol
    INTEGER,             DIMENSION(NPT),     INTENT(INOUT) :: NW    ! Minimum word length of a parameter
    DOUBLE PRECISION,    DIMENSION(NPT),     INTENT(INOUT) :: PVAL  ! parameter values
    !
    !   Local variables
    INTEGER :: IFAIL
    !
    !   Write the model-input files
    CALL MIO_ADA_WRITEFILES(IFAIL,NPT,PARNAM,NOPNT,NW,PRECISPRO,PVAL)
    IF (IFAIL .NE. 0) THEN
      ERRRUNNER = 'Error: Runner cannot write model-input file(s).'
      CALL PLL_RUNNER_STOP(1)
    ENDIF
    RETURN
  END SUBROUTINE PLL_ADA
  ! ----------------------------------------------------------------------------
  SUBROUTINE PLL_EXE(IRUNRUNNER,NRUNRUNNER)
    USE GLOBAL_DATA, ONLY: IVERB
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(IN) :: IRUNRUNNER
    INTEGER, INTENT(IN) :: NRUNRUNNER
    !
    !   Local variables
    !
    !   Format statements
    100 FORMAT(/,1X,'Running system command: ',1X,A,/)
    110 FORMAT(1X,'(Starting run ',I5,' of ',I5,')')
    120 FORMAT(1X,'Finished system command:',1X,A)
    150 FORMAT(1X,'(Completed system command for run ',I5,' of ',I5,')',/)
    !
    IF (IVERB>3) WRITE(*,100) TRIM(COMMAND)
    IF (IVERB>2) WRITE(*,110) IRUNRUNNER,NRUNRUNNER
    !
    CALL UTL_SYSTEM(TRIM(COMMAND))
    !
    IF (IVERB>3) WRITE(*,120) TRIM(COMMAND)
    IF (IVERB>2) WRITE(*,150) IRUNRUNNER,NRUNRUNNER
    !
    RETURN
  END SUBROUTINE PLL_EXE
  ! ----------------------------------------------------------------------------
  LOGICAL FUNCTION PLL_EXE_FUNC(IRUNRUNNER,NRUNRUNNER,ERRORMSG)
    USE GLOBAL_DATA, ONLY: IVERB
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(IN) :: IRUNRUNNER
    INTEGER, INTENT(IN) :: NRUNRUNNER
    CHARACTER(LEN=*), INTENT(OUT) :: ERRORMSG
    !
    !   Local variables
    INTEGER :: KTRY
    LOGICAL :: OK
    !
    !   Format statements
    100 FORMAT(/,1X,'Running system command: ',1X,A,/)
    110 FORMAT(1X,'(Run ',I5,' of ',I5,')')
    120 FORMAT(1X,'Finished system command:',1X,A)
    140 FORMAT(1X,'Error running system command:',1X,A,/,  &
        'Error returned by operating system: ',A)
    150 FORMAT(1X,'(Completed system command for run ',I5,' of ',I5,')',/)
    !
    IF (IVERB>3) WRITE(*,100) TRIM(COMMAND)
    IF (IVERB>2) WRITE(*,110) IRUNRUNNER,NRUNRUNNER
    !
    ERRORMSG = ' '
    OK = .FALSE.
    KTRY = 0
    10 CONTINUE
    IF (UTL_SYSTEM_FUNC(TRIM(COMMAND),ERRORMSG)) THEN
      IF (IVERB>3) WRITE(*,120) TRIM(COMMAND)
      IF (IVERB>2) WRITE(*,150) IRUNRUNNER,NRUNRUNNER
      OK = .TRUE.
    ELSE
      IF (KTRY < 21) THEN
        KTRY = KTRY + 1
        GOTO 10
      ENDIF
      WRITE(*,140) TRIM(COMMAND),TRIM(ERRORMSG)
      ERRRUNNER = ERRORMSG
    ENDIF
    PLL_EXE_FUNC = OK
    !
    RETURN
  END FUNCTION PLL_EXE_FUNC
  ! ----------------------------------------------------------------------------
  SUBROUTINE PLL_EXT(NCATS,NVEXT,EXTNAM,MCVUSE,EXTVAL)
    !   Extract model-calculated dependents
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    USE MODEL_IO, ONLY: MIO_EXT
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                             INTENT(IN)  :: NCATS  ! Number of categories of model-calculated values
    INTEGER,                             INTENT(IN)  :: NVEXT  ! number of values to extract
    CHARACTER(LEN=*), DIMENSION(NVEXT),  INTENT(IN)  :: EXTNAM ! extracted-value names
    LOGICAL,          DIMENSION(NCATS),  INTENT(IN)  :: MCVUSE ! Do extractions for this category?
    DOUBLE PRECISION, DIMENSION(NVEXT),  INTENT(OUT) :: EXTVAL ! extracted values
    !
    !   Local variables
    INTEGER :: IFAIL
    CHARACTER(LEN=MAX_STRING_LEN) :: INSTRUCTION ! instruction of error
    !
    !   Format statements
    100 FORMAT(/,1X,'Error encountered in extraction instruction:',/,1X,A)
    !
    CALL MIO_EXT(IFAIL,-1,NCATS,NVEXT,EXTNAM,MCVUSE,EXTVAL,INSTRUCTION)
    IF (IFAIL .NE. 0) THEN
      WRITE(*,100) TRIM(INSTRUCTION)
      ERRRUNNER = 'Error encountered in extraction instruction: '//  &
          TRIM(INSTRUCTION)
      CALL PLL_RUNNER_STOP(1)
    ENDIF
    !
    RETURN
  END SUBROUTINE PLL_EXT
  !-----------------------------------------------------------------------------
  SUBROUTINE PLL_CLN()
    !   Deallocate all arrays in the Parallel module
    USE GLOBAL_DATA, ONLY: IVERB
    IMPLICIT NONE
    !
    IF (ALLOCATED(BTIME)) DEALLOCATE(BTIME)
    IF (ALLOCATED(IRUNNERSTAT)) DEALLOCATE(IRUNNERSTAT)
    IF (ALLOCATED(KOD)) DEALLOCATE(KOD)
    IF (ALLOCATED(RUNTIME)) DEALLOCATE(RUNTIME)
    IF (ALLOCATED(RUNNERDIR)) DEALLOCATE(RUNNERDIR)
    IF (ALLOCATED(RUNNERNAME)) DEALLOCATE(RUNNERNAME)
    CALL TYP_DEALLOC(LLPTRPLLCTRL)
    CALL TYP_DEALLOC(LLPTRPLLRUN)
    RETURN
  END SUBROUTINE PLL_CLN
  !-----------------------------------------------------------------------------
  !----------------------- PARALLEL-PROCESSING UTILITIES -----------------------
  !-----------------------------------------------------------------------------
  INTEGER FUNCTION PLL_CLOSE_AND_DELETE(NUNIT,FNAME,MAXTRIES,WAITSECS)
    ! Ensure that unit NUNIT is closed and that file FNAME is deleted.  
    ! If closure of unit NUNIT and deletion of file FNAME are confirmed,
    ! return value is 0.  If not confirmed, return value is 1.
    !
    ! If unit NUNIT is open, the file connected to unit NUNIT is closed,
    ! regardless whether the connected file is FNAME.  If unit NUNIT is
    ! not open, it is used to connect to FNAME, and then NUNIT is closed
    ! and FNAME is deleted.
    !
    USE GLOBAL_DATA, ONLY: IVERB, MAX_STRING_LEN
    USE UTILITIES, ONLY: UTL_CASE
    USE IFPORT
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                    INTENT(IN) :: NUNIT
    CHARACTER(LEN=*),           INTENT(IN) :: FNAME
    INTEGER, OPTIONAL,          INTENT(IN) :: MAXTRIES
    DOUBLE PRECISION, OPTIONAL, INTENT(IN) :: WAITSECS
    !
    !   Local variables
    INTEGER :: ISTAT, KTRY, MAXTRIESLOCAL
    INTEGER(2) :: K 
    DOUBLE PRECISION :: WAITSECSLOCAL
    LOGICAL :: DELETED, LEX, LOP
    CHARACTER(LEN=MAX_STRING_LEN) :: LOCNAME
    !
    ! Format statements
    120 FORMAT(1X,'Warning: Unable to delete file "',A,'"')
    !
    ! Initialize variables
    PLL_CLOSE_AND_DELETE = 1
    DELETED = .FALSE.
    IF (PRESENT(MAXTRIES)) THEN
      MAXTRIESLOCAL = MAXTRIES
    ELSE
      MAXTRIESLOCAL = 10
    ENDIF
    IF (PRESENT(WAITSECS)) THEN
      WAITSECSLOCAL = WAITSECS
    ELSE
      WAITSECSLOCAL = WAIT
    ENDIF
    !
    ! First, ensure that file is open on unit NUNIT
    INQUIRE(UNIT=NUNIT,NAME=LOCNAME,OPENED=LOP)
    IF (.NOT. LOP) THEN
      LOCNAME = FNAME
      OPEN(NUNIT,FILE=LOCNAME,IOSTAT=ISTAT)
    ENDIF
    !
    ! Close and delete file
    KTRY = 0
    20 CONTINUE
    INQUIRE(FILE=LOCNAME,IOSTAT=ISTAT,EXIST=LEX)
    IF (LEX) THEN
      INQUIRE(NUNIT,OPENED=LOP)
      IF (LOP) THEN
        CLOSE(NUNIT,STATUS='DELETE',IOSTAT=ISTAT)
        IF (ISTAT==0) DELETED = .TRUE.
      ELSE
        OPEN(NUNIT,FILE=LOCNAME,IOSTAT=ISTAT)
        IF (ISTAT==0) CLOSE(NUNIT,STATUS='DELETE',IOSTAT=ISTAT)
        IF (ISTAT==0) DELETED = .TRUE.
      ENDIF
      IF (ISTAT>0 .AND. KTRY<MAXTRIESLOCAL) THEN
        KTRY = KTRY+1
        CALL PLL_WAIT(WAITSECSLOCAL)
        GOTO 20
      ENDIF
    ELSE
      INQUIRE(NUNIT,OPENED=LOP)
      IF (LOP) CLOSE(NUNIT)
      DELETED = .TRUE.
    ENDIF
    !
    IF (.NOT. DELETED) THEN
      INQUIRE(NUNIT,OPENED=LOP)
      IF (LOP) THEN
        CLOSE(NUNIT,IOSTAT=ISTAT)
      ENDIF
      K = 0
    ! For compilers that do not support DELFILESQQ or equivalent, 
    ! the following line can be commented out.
      K = DELFILESQQ(LOCNAME)
      IF (K>0) DELETED = .TRUE.
    ENDIF
    !
    IF (DELETED) THEN
      PLL_CLOSE_AND_DELETE = 0
    ELSE
      IF (IVERB>0) WRITE(*,120)TRIM(LOCNAME)
    ENDIF
    !
    RETURN
  END FUNCTION PLL_CLOSE_AND_DELETE
  !-----------------------------------------------------------------------------
  SUBROUTINE PLL_DONE(NRUNSPLL,IRUNSTAT,DONE,NUMDONE)
    !   Return TRUE only if all elements of IRUNSTAT >= 3.
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                      INTENT(IN)  :: NRUNSPLL
    INTEGER, DIMENSION(NRUNSPLL), INTENT(IN)  :: IRUNSTAT
    LOGICAL,                      INTENT(OUT) :: DONE
    INTEGER,                      INTENT(OUT) :: NUMDONE
    !
    !   Local variables
    INTEGER :: I
    !
    NUMDONE = 0
    DONE = .FALSE.
    DO I=1,NRUNSPLL
      IF (IRUNSTAT(I)==3) NUMDONE = NUMDONE+1
    ENDDO
    IF (NUMDONE==NRUNSPLL) DONE = .TRUE.
    RETURN
  END SUBROUTINE PLL_DONE
  !-----------------------------------------------------------------------------
  INTEGER FUNCTION PLL_FIND_RUNNER() RESULT (IDRUNNER)
    !   Return the number of a runner that is available to make a model run.
    !   Return -1 if no runners are available.
    IMPLICIT NONE
    !
    !   Local variables
    INTEGER :: I
    CHARACTER(LEN=MAX_STRING_LEN) :: FNAME
    LOGICAL :: LEX
    !
    IDRUNNER = -1
    DO I=1,NUMRUNNERS
      FNAME=TRIM(RUNNERDIR(I))//FNRUNRDY
      INQUIRE(FILE=FNAME,EXIST=LEX)
      IF (LEX) THEN
        !   Double check that file still exists
        CALL PLL_WAIT()
        INQUIRE(FILE=FNAME,EXIST=LEX)
        IF (LEX) THEN
          IDRUNNER = I
          RETURN
        ENDIF
      ENDIF
    ENDDO
    RETURN
  END FUNCTION PLL_FIND_RUNNER
  !-----------------------------------------------------------------------------
  SUBROUTINE PLL_MAKE_RUNS(LCIS,NCATS,NINSTRUCT,NMIFILE,NMOFILE,NOPNT,   &
                           NPT,NUMLADV,NRUNSPLL,NVEXT,PRECISPRO,   &
                           CATGOR,CINSTSET,EXTNAM,INSTRUCTFILE,   &
                           LOCINS,MCVUSE,MIFILE,MOFILE,MRKDL,NW,PARNAM,   &
                           PARVALSETS,TFILE,DEPVALSETS,COMMANDPLL)
    !   Use runners to perform ADApt, EXEcute, and EXTract tasks in parallel
    USE GLOBAL_DATA, ONLY: AMESSAGE, IVERB, LENDNAM, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                     INTENT(IN)  :: LCIS
    INTEGER,                                     INTENT(IN)  :: NCATS
    INTEGER,                                     INTENT(IN)  :: NINSTRUCT
    INTEGER,                                     INTENT(IN)  :: NMIFILE
    INTEGER,                                     INTENT(IN)  :: NMOFILE
    INTEGER,                                     INTENT(IN)  :: NOPNT
    INTEGER,                                     INTENT(IN)  :: NPT
    INTEGER,                                     INTENT(IN)  :: NUMLADV
    INTEGER,                                     INTENT(IN)  :: NRUNSPLL
    INTEGER,                                     INTENT(IN)  :: NVEXT
    INTEGER,                                     INTENT(IN)  :: PRECISPRO
    CHARACTER(LEN=6),              DIMENSION(NMOFILE),  INTENT(IN)  :: CATGOR ! Model-calc'd val. cat. for each model-output file
    CHARACTER(LEN=1),              DIMENSION(LCIS),     INTENT(IN)  :: CINSTSET ! holds compressed instruction set
    CHARACTER(LEN=LENDNAM),        DIMENSION(NVEXT),    INTENT(IN)  :: EXTNAM
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NMOFILE),  INTENT(IN)  :: INSTRUCTFILE ! Instruction file names
    INTEGER,                       DIMENSION(NINSTRUCT),  INTENT(IN)  :: LOCINS ! pointer to instructions
    LOGICAL,                       DIMENSION(NCATS),    INTENT(IN)  :: MCVUSE
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NMIFILE),  INTENT(IN)  :: MIFILE
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NMOFILE),  INTENT(IN)  :: MOFILE ! Model output file names
    CHARACTER(LEN=1),              DIMENSION(NMOFILE),  INTENT(IN)  :: MRKDL  ! Marker delimiters
    INTEGER,                       DIMENSION(NPT),      INTENT(IN)  :: NW
    CHARACTER(LEN=12),             DIMENSION(NPT),      INTENT(IN)  :: PARNAM
    DOUBLE PRECISION,              DIMENSION(NPT,NRUNSPLL),   INTENT(IN)  :: PARVALSETS
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NMIFILE),  INTENT(IN)  :: TFILE
    DOUBLE PRECISION,              DIMENSION(NVEXT,NRUNSPLL), INTENT(OUT) :: DEPVALSETS
    CHARACTER(LEN=MAX_STRING_LEN),   OPTIONAL,   INTENT(IN)  :: COMMANDPLL
    !
    !   Local variables
    LOGICAL :: DONE, LEX
    INTEGER :: IERR, IRUN, IRUNNER, ISTAT, IU, KDONE, KND, KNS, KSL1,   &
               KSTART, KTRY, KWAIT, NUMDONE, NUMDONELAST
    INTEGER                       :: NR2ST    ! Number of run to be started
    INTEGER, DIMENSION(NRUNSPLL)  :: IRUNSTAT ! Status of each run that's needed
    INTEGER, DIMENSION(NUMRUNNERS) :: IRUNNUM ! Number of run in progress on runner
    INTEGER, DIMENSION(8)         :: VALUES
    CHARACTER(LEN=MAX_STRING_LEN) :: FNAME, FRUNNERNAME
    CHARACTER(LEN=10)             :: CHDATE, CHTIME, CHZONE
    DOUBLE PRECISION              :: ELMIN, ELSECS, RTIME, TLIM
    CHARACTER(LEN=1) :: OK2INTERRUPT
    INTEGER :: IFIL
    !
    !   Format statements
    13 FORMAT(' in PLL_MAKE_RUNS, PLL_WRITE_DISPAR reports',   &
              ' failure on runner ',A)
    20 FORMAT(1X,'Making ',I5,' runs of command "',A,'" on runners...')
    30 FORMAT(1X,'Runner "',A,'" is active')
    40 FORMAT(1X,'File inquiry on runner ',A,   &
              ' failed--runner is assumed inactive.')
    50 FORMAT(1X,'Runner "',A,'" reports failure and needs to be restarted.')
    60 FORMAT(A)
    70 FORMAT(1X,'WARNING: Cannot read error message from file "',A,'"')
    80 FORMAT(/,1X,'Runner "',A,'" reports the following error:')
    100 FORMAT(1X,I5,' of ',I5,' model runs completed.')
    120 FORMAT(1X,'No runners active -- Stopping')
    130 FORMAT(1X,'Runner ',A,' has successfully completed run number ',I5,   &
        ' in ',F8.2,' min.')
    135 FORMAT(1X,'Runtime for runner "',A,'" changed to ',G12.5,/,   &
        ' because elapsed time < expected runtime.')
    136 FORMAT(1X,'Runtime for runner "',A,'" changed to ',G12.5,/,   &
        ' because elapsed time > expected runtime.')
    150 FORMAT(/,1X,'Runner "',A,'" is overdue for run number ',I5,/   &
        1X,'Expected run time = ',G9.3,' seconds; elapsed time = ',G9.3,   &
        ' seconds.',/)
    160 FORMAT(/,1X,'Runner "',A,'" is overdue for run number ',I5,/)
    170 FORMAT(/,1X,'Runner "',A,'" is overdue for run number ',I5,/   &
        1X,'and needs to be restarted.',/)
    180 FORMAT(1X,'Increasing expected runtime for runner "',A,'" to ',G10.3,   &
        1X,'seconds.')
    200 FORMAT(1X,'Failed to read dependent values from "jrundep.rdy" file',   &
               /,' for run number ',I5,' and runner number ',I5)
    260 FORMAT(/,1X,'Runner "',A,'" has been terminated at run number ',I5,/   &
        1X,'and needs to be restarted.',/)
    300 FORMAT(' File ',A,' found -- OK to interrupt main program (Y or N)?')
    425 FORMAT(' Warning: WAIT time may be too small')
    600 FORMAT(' Dependent values were read from ',A, &
        ' but file jrundep.rdy could not be deleted.')
    !
    !   IRUNSTAT flags for model runs:
    !      0 -- Run not started
    !      1 -- Run in progress
    !      2 -- Run done
    !      3 -- Extraction done--job complete
    !     -1 -- Run failed or was terminated before completion
    !
    IF (.NOT. DOPLL) RETURN
    IF (PRESENT(COMMANDPLL)) COMMAND = COMMANDPLL
    IF (COMMAND == ' ') THEN
      AMESSAGE = 'Error in PLL_MAKE_RUNS -- No command to execute'
      CALL PLL_STOP_RUNNERS()
      CALL UTL_STOP()
    ENDIF
    !
    IF (IVERB>0) WRITE(*,20) NRUNSPLL,TRIM(COMMAND)
    !
    DONE = .FALSE.
    NUMDONE = 0
    NUMDONELAST = 0
    IRUNSTAT = 0
    IRUNNUM = 0
    LOOP1: DO WHILE (.NOT. DONE)
      !
      !   Determine number of runs not yet started
      KNS = 0
      LOOP2: DO IRUN=1,NRUNSPLL
        IF (IRUNSTAT(IRUN)==0 .OR. IRUNSTAT(IRUN)==-1) THEN
          KNS = KNS+1
        ENDIF
      ENDDO LOOP2
      !
      !   If any model runs need to be started, start as many runs as possible
      IF (KNS>0) THEN
        KSTART = 0
        KSL1 = 0
        KWAIT = 0
        320 CONTINUE
        LOOP3: DO IRUNNER=1,NUMRUNNERS
          !   Look for jrunfail.fin file
          FRUNNERNAME = TRIM(RUNNERDIR(IRUNNER))//FNRUNFAIL
          INQUIRE(FILE=FRUNNERNAME,EXIST=LEX,IOSTAT=ISTAT)
          IF (ISTAT>0) THEN
            !   File inquiry has failed.  Assume runner is inactive.
            LEX = .FALSE.
            IF (IRUNNERSTAT(IRUNNER)>0) THEN
              IRUNNERSTAT(IRUNNER) = -IRUNNERSTAT(IRUNNER)
              IF (IVERB>2) WRITE(*,40)TRIM(RUNNERNAME(IRUNNER))
            ELSE
              IRUNNERSTAT(IRUNNER) = -1
            ENDIF
            IF (IRUNNUM(IRUNNER)>0) THEN
              IRUNSTAT(IRUNNUM(IRUNNER)) = -1
            ENDIF
            IRUNNUM(IRUNNER) = 0
          ENDIF
          IF (LEX) THEN
            !   jrunfail.fin file has been found.
            IRUNNERSTAT(IRUNNER) = 10
            IF (IRUNNUM(IRUNNER)>0) THEN
              IRUNSTAT(IRUNNUM(IRUNNER)) = -1
            ENDIF
            IRUNNUM(IRUNNER) = 0
            IU = UTL_GETUNIT(7,1000)
            OPEN(IU,FILE=FRUNNERNAME)
            IF (IVERB>0) THEN
              WRITE(*,50)TRIM(RUNNERNAME(IRUNNER))
              ERRRUNNER = ' '
              READ(IU,60,IOSTAT=ISTAT)ERRRUNNER
              IF (ISTAT==0) THEN
                IF (ERRRUNNER .NE. ' ') THEN
                  WRITE(*,80)TRIM(RUNNERNAME(IRUNNER))
                  AMESSAGE = ERRRUNNER
                  CALL UTL_WRITE_MESSAGE()
                  WRITE(*,60)' '
                  AMESSAGE = ' '
                ENDIF
              ELSE
                WRITE(*,70)TRIM(FRUNNERNAME)
              ENDIF
            ENDIF
            KTRY = 0
            500 CONTINUE
            ISTAT = PLL_CLOSE_AND_DELETE(IU,FRUNNERNAME)
            IF (ISTAT>0 .AND. KTRY<11) THEN
              KTRY = KTRY+1
              CALL PLL_WAIT()
              GOTO 500
            ENDIF
            !
            FRUNNERNAME = TRIM(RUNNERDIR(IRUNNER))//FNRUNRDY
            INQUIRE(FILE=FRUNNERNAME,EXIST=LEX)
            IF (LEX) THEN
              KTRY = 0
              510 CONTINUE
              OPEN(IU,FILE=FRUNNERNAME,IOSTAT=ISTAT)
              IF (ISTAT>0 .AND. KTRY<11) THEN
                KTRY = KTRY+1
                CALL PLL_WAIT()
                GOTO 510
              ENDIF
              IF (ISTAT==0) THEN
                KTRY = 0
                520 CONTINUE
                ISTAT = PLL_CLOSE_AND_DELETE(IU,FRUNNERNAME)
                IF (ISTAT>0 .AND. KTRY<11) THEN
                  KTRY = KTRY+1
                  CALL PLL_WAIT()
                  GOTO 520
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          !
          !   Check for jrunner.fin, indicating runner has been terminated
          FNAME = TRIM(RUNNERDIR(IRUNNER))//FNRUNFIN
          INQUIRE(FILE=FNAME,EXIST=LEX,IOSTAT=ISTAT)
          IF (ISTAT>0) THEN
            !   File inquiry has failed.  Assume runner is inactive.
            LEX = .FALSE.
            IF (IRUNNERSTAT(IRUNNER)>0) THEN
              IRUNNERSTAT(IRUNNER) = -IRUNNERSTAT(IRUNNER)
              IF (IVERB>2) WRITE(*,40)TRIM(RUNNERNAME(IRUNNER))
            ELSE
              IRUNNERSTAT(IRUNNER) = -1
            ENDIF
            IF (IRUNNUM(IRUNNER)>0) THEN
              IRUNSTAT(IRUNNUM(IRUNNER)) = -1
            ENDIF
            IRUNNUM(IRUNNER) = 0
          ENDIF
          IF (LEX) THEN
            !   jrunner.fin has been found
            IF (IRUNNUM(IRUNNER)>0) THEN
              IF (IVERB>1) WRITE(*,260)TRIM(RUNNERNAME(IRUNNER)),   &
                         IRUNNUM(IRUNNER)
              IRUNSTAT(IRUNNUM(IRUNNER)) = -1
            ENDIF
            IRUNNERSTAT(IRUNNER) = 9  ! changed from -1 to 9 ERB 2/15/06
            IRUNNUM(IRUNNER) = 0
            IU = UTL_GETUNIT(7,1000)
            KTRY = 0
            530 CONTINUE
            OPEN(IU,FILE=FNAME,IOSTAT=ISTAT)
            IF (ISTAT>0 .AND. KTRY<11) THEN
              KTRY = KTRY+1
              CALL PLL_WAIT()
              GOTO 530
            ENDIF
            IF (ISTAT==0) THEN
              KTRY = 0
              540 CONTINUE
              ISTAT = PLL_CLOSE_AND_DELETE(IU,FNAME)
              IF (ISTAT>0 .AND. KTRY<11) THEN
                KTRY = KTRY+1
                CALL PLL_WAIT()
                GOTO 540
              ENDIF
            ENDIF
          ENDIF
          !
          IF (IRUNNERSTAT(IRUNNER)==2) THEN
            KSL1 = KSL1 + 1
            !   Select a run that needs to be started
            LOOP4: DO IRUN=1,NRUNSPLL
              IF (IRUNSTAT(IRUN)==0 .OR. IRUNSTAT(IRUN)==-1) THEN
                NR2ST = IRUN
                EXIT LOOP4
              ENDIF
            ENDDO LOOP4
            !   Before signalling runner to start model run, ensure that 
            !   runner directory does not contain a jrundep.rdy file
            IF (PLL_JRUNDEP_DELETED(IRUNNER)) THEN
              !   Create "jdispar.rdy" file, which signals to runner to ADApt
              !   parameters, EXEcute model, and EXTract dependent values
              CALL PLL_WRITE_DISPAR(IERR,NR2ST,IRUNNER,NPT,NRUNSPLL,   &
                                    PARVALSETS(:,NR2ST))
              IF (IERR==0) THEN
                CALL DATE_AND_TIME(CHDATE,CHTIME,CHZONE,VALUES)
                BTIME(1:8,IRUNNER) = VALUES(1:8)
                IRUNSTAT(NR2ST) = 1
                IRUNNERSTAT(IRUNNER) = 3
                IRUNNUM(IRUNNER) = NR2ST
                KSTART = KSTART+1
                ! Model runs started nearly simultaneously can
                ! interfere with each other.  Insert a delay to
                ! avoid this situation.
                CALL PLL_WAIT()
              ELSE
                WRITE(*,13)TRIM(RUNNERNAME(IRUNNER))
                IRUNNERSTAT(IRUNNER) = -2
              ENDIF
              IF (KSTART==KNS) EXIT LOOP3
            ENDIF
          ELSEIF (IRUNNERSTAT(IRUNNER)==3 .OR. IRUNNERSTAT(IRUNNER)==4) THEN
            KSL1 = KSL1+1
          ELSEIF (IRUNNERSTAT(IRUNNER)<1 .OR. IRUNNERSTAT(IRUNNER)==9   &
                  .OR. IRUNNERSTAT(IRUNNER)==10   &
                  .OR. IRUNNERSTAT(IRUNNER)==11) THEN
            !   See if non-active runner is now available
            FRUNNERNAME = TRIM(RUNNERDIR(IRUNNER))//FNRUNRDY
            INQUIRE(FILE=FRUNNERNAME,EXIST=LEX)
            IF (LEX) THEN
                IRUNNERSTAT(IRUNNER) = 1
                KOD(IRUNNER) = 0
                KSL1 = KSL1+1
                IF (IVERB>2) WRITE(*,30)TRIM(RUNNERNAME(IRUNNER))
                !   Write "jdispatch.rdy" file and set IRUNNERSTAT=2
                CALL PLL_WRITE_DISRDY(IRUNNER,LCIS,NCATS,NINSTRUCT,NMIFILE,   &
                                      NMOFILE,NOPNT,NPT,NUMLADV,NVEXT,   &
                                      PRECISPRO,CATGOR,CINSTSET,EXTNAM,   &
                                      INSTRUCTFILE,LOCINS,MCVUSE,MIFILE,   &
                                      MOFILE,MRKDL,NW,PARNAM,TFILE)
            ENDIF
          ENDIF
        ENDDO LOOP3
        IF (KSL1==0) THEN
          IF (KWAIT<21) THEN
            KWAIT = KWAIT+1
            CALL PLL_WAIT()
            GOTO 320
          ELSE
            WRITE(*,425)
          ENDIF
          WRITE(*,120)
          CALL PLL_STOP_RUNNERS()
          CALL UTL_STOP()
        ENDIF
      ENDIF
      !
      !   Determine number of model runs for which dependents are needed
      KND = 0
      LOOP5: DO IRUN=1,NRUNSPLL
        IF (IRUNSTAT(IRUN)==1) THEN
          KND = KND+1
        ENDIF
      ENDDO LOOP5
      !
      !   If any model runs are in progress, look to see if jrundep.rdy exists
      !   or if any model runs are long overdue
      IF (KND>0) THEN
        KDONE = 0
        LOOP6: DO IRUNNER=1,NUMRUNNERS
          IF (IRUNNERSTAT(IRUNNER)==3) THEN
            !   When available, copy dependent values from runners and store them
            FNAME = TRIM(RUNNERDIR(IRUNNER))//FNRUNDEP
            INQUIRE(FILE=FNAME,EXIST=LEX,IOSTAT=ISTAT)
            IF (ISTAT>0) THEN
              !   File inquiry has failed.  Assume runner is inactive.
              LEX = .FALSE.
              IF (IRUNNERSTAT(IRUNNER)>0) THEN
                IRUNNERSTAT(IRUNNER) = -IRUNNERSTAT(IRUNNER)
                IF (IVERB>2) WRITE(*,40)TRIM(RUNNERNAME(IRUNNER))
              ELSE
                IRUNNERSTAT(IRUNNER) = -1
              ENDIF
              IF (IRUNNUM(IRUNNER)>0) THEN
                IRUNSTAT(IRUNNUM(IRUNNER)) = -1
              ENDIF
              IRUNNUM(IRUNNER) = 0
            ENDIF
            IF (LEX) THEN
              IRUNNERSTAT(IRUNNER) = 4
              IRUN = IRUNNUM(IRUNNER)
              IRUNSTAT(IRUN) = 2
              !   jrundep.rdy is available
              CALL PLL_READ_RUNDEP(IERR,IRUN,IRUNNER,NVEXT,   &
                                   DEPVALSETS(:,IRUN))
              IF (IERR==0) THEN
                IF (IVERB>2) THEN
                  ELMIN = UTL_ELAPSED_TIME(BTIME(:,IRUNNER))/60.0D0
                  WRITE(*,130)TRIM(RUNNERNAME(IRUNNER)),IRUN,ELMIN
                ENDIF
                IRUNSTAT(IRUN) = 3
                IRUNNERSTAT(IRUNNER) = 2
                IRUNNUM(IRUNNER) = 0
                KDONE = KDONE+1
                ELSECS = UTL_ELAPSED_TIME(BTIME(:,IRUNNER))
                RTIME = RUNTIME(IRUNNER)
                IF (ELSECS<RTIME) THEN
                  RUNTIME(IRUNNER) = (2.0D0*RTIME + ELSECS)/3.0D0
                  IF (IVERB>4) WRITE(*,135)TRIM(RUNNERNAME(IRUNNER)),RUNTIME(IRUNNER)
                ELSEIF (ELSECS>RTIME) THEN
                  RUNTIME(IRUNNER) = ELSECS
                  IF (IVERB>4) WRITE(*,136)TRIM(RUNNERNAME(IRUNNER)),RUNTIME(IRUNNER)
                ENDIF
              ELSEIF (IERR==1) THEN ! Unable either to open or read jrundep.rdy
                !   Unable either to open or read jrundep.rdy
                IRUNNERSTAT(IRUNNER) = -ABS(IRUNNERSTAT(IRUNNER))
                IRUNNUM(IRUNNER) = 0
                IRUNSTAT(IRUN) = 0
              ELSEIF (IERR==2) THEN
                !   Run number read from jrundep.rdy file does not match
                !   expected run number.  Assume runner is ready to accept
                !   another job and the run needs to be rerun.
                IRUNNERSTAT(IRUNNER) = 2
                IRUNNUM(IRUNNER) = 0
                IRUNSTAT(IRUN) = 0
              ELSEIF (IERR==3) THEN
                ! File jrundep.rdy could not be deleted, but extracted values were successfully read
                WRITE(*,600)TRIM(RUNNERNAME(IRUNNER))
                IRUNNERSTAT(IRUNNER) = -4  ! Runner is nonresponsive
                IRUNNUM(IRUNNER) = 0
                IRUNSTAT(IRUN) = 3
                ELSECS = UTL_ELAPSED_TIME(BTIME(:,IRUNNER))
                RTIME = RUNTIME(IRUNNER)
                IF (ELSECS<RTIME) THEN
                  RUNTIME(IRUNNER) = (2.0D0*RTIME + ELSECS)/3.0D0
                  IF (IVERB>4) WRITE(*,135)TRIM(RUNNERNAME(IRUNNER)),RUNTIME(IRUNNER)
                ELSEIF (ELSECS>RTIME) THEN
                  RUNTIME(IRUNNER) = ELSECS
                  IF (IVERB>4) WRITE(*,136)TRIM(RUNNERNAME(IRUNNER)),RUNTIME(IRUNNER)
                ENDIF
              ELSE
                write(*,*)'In PLL_MAKE_RUNS, unexpected value of IERR: ',ierr
                WRITE(*,200)IRUNNUM(IRUNNER),IRUNNER
                CALL PLL_STOP_RUNNERS()
                call utl_stop()
              ENDIF
            ELSE
              !   Check for long overdue model run
              ELSECS = UTL_ELAPSED_TIME(BTIME(:,IRUNNER))
              TLIM = RUNTIME(IRUNNER)*TIMEOUTFAC
              IF (ELSECS>TLIM) THEN
                !   Model run is overdue
                IF (IVERB>2) THEN
                  WRITE(*,150)TRIM(RUNNERNAME(IRUNNER)),IRUNNUM(IRUNNER),   &
                      RUNTIME(IRUNNER),ELSECS
                ELSE
                  WRITE(*,160)TRIM(RUNNERNAME(IRUNNER)),IRUNNUM(IRUNNER)
                ENDIF
                IF (IRUNNUM(IRUNNER)>0) THEN
                  IRUNSTAT(IRUNNUM(IRUNNER)) = -1
                ENDIF
                IF (KOD(IRUNNER)>2) THEN
                  !   Too many overdue runs; assume runner is no longer running
                  WRITE(*,170)TRIM(RUNNERNAME(IRUNNER))
                  IRUNNERSTAT(IRUNNER) = -1
                  IRUNNUM(IRUNNER) = 0
                ELSE
                  !   Increase expected run time for next run
                  RUNTIME(IRUNNER) = RUNTIME(IRUNNER)*1.5D0
                  IF (IVERB>3) WRITE(*,180)TRIM(RUNNERNAME(IRUNNER)),RUNTIME(IRUNNER)
                  KOD(IRUNNER) = KOD(IRUNNER) + 1
                  IRUNNERSTAT(IRUNNER) = 11
                ENDIF
              ENDIF
              !   Check for jrunner.fin, indicating runner has been terminated
              FNAME = TRIM(RUNNERDIR(IRUNNER))//FNRUNFIN
              INQUIRE(FILE=FNAME,EXIST=LEX,IOSTAT=ISTAT)
              IF (ISTAT>0) THEN
                !   File inquiry has failed.  Assume runner is inactive.
                LEX = .FALSE.
                IF (IRUNNERSTAT(IRUNNER)>0) THEN
                  IRUNNERSTAT(IRUNNER) = -IRUNNERSTAT(IRUNNER)
                  IF (IVERB>2) WRITE(*,40)TRIM(RUNNERNAME(IRUNNER))
                ELSE
                  IRUNNERSTAT(IRUNNER) = -1
                ENDIF
                IF (IRUNNUM(IRUNNER)>0) THEN
                  IRUNSTAT(IRUNNUM(IRUNNER)) = -1
                ENDIF
                IRUNNUM(IRUNNER) = 0
              ENDIF
              IF (LEX) THEN
                !   jrunner.fin has been found
                IF (IRUNNUM(IRUNNER)>0) THEN
                  IF (IVERB>1) WRITE(*,260)TRIM(RUNNERNAME(IRUNNER)),   &
                             IRUNNUM(IRUNNER)
                  IRUNSTAT(IRUNNUM(IRUNNER)) = -1
                ENDIF
                IRUNNERSTAT(IRUNNER) = -1
                IRUNNUM(IRUNNER) = 0
              ENDIF
              !   Check for jrunfail.fin, indicating runner reports error
              FNAME = TRIM(RUNNERDIR(IRUNNER))//FNRUNFAIL
              INQUIRE(FILE=FNAME,EXIST=LEX,IOSTAT=ISTAT)
              IF (ISTAT>0) THEN
                !   File inquiry has failed.  Assume runner is inactive.
                LEX = .FALSE.
                IF (IRUNNERSTAT(IRUNNER)>0) THEN
                  IRUNNERSTAT(IRUNNER) = -IRUNNERSTAT(IRUNNER)
                  IF (IVERB>2) WRITE(*,40)TRIM(RUNNERNAME(IRUNNER))
                ELSE
                  IRUNNERSTAT(IRUNNER) = -1
                ENDIF
                IF (IRUNNUM(IRUNNER)>0) THEN
                  IRUNSTAT(IRUNNUM(IRUNNER)) = -1
                ENDIF
                IRUNNUM(IRUNNER) = 0
              ENDIF
              IF (LEX) THEN
                !   jrunfail.fin has been found
                IRUNNERSTAT(IRUNNER) = 10
                IF (IRUNNUM(IRUNNER)>0) THEN
                  IRUNSTAT(IRUNNUM(IRUNNER)) = -1
                ENDIF
                IRUNNUM(IRUNNER) = 0
                IU = UTL_GETUNIT(7,1000)
                KTRY = 0
                545 CONTINUE
                OPEN(IU,FILE=FNAME,IOSTAT=ISTAT)
                IF (ISTAT>0 .AND. KTRY<11) THEN
                  KTRY = KTRY+1
                  CALL PLL_WAIT()
                  GOTO 545
                ENDIF
                IF (IVERB>0) THEN
                  WRITE(*,50)TRIM(RUNNERNAME(IRUNNER))
                  ERRRUNNER = ' '
                  READ(IU,60)ERRRUNNER
                  IF (ERRRUNNER .NE. ' ') THEN
                    WRITE(*,80)TRIM(RUNNERNAME(IRUNNER))
                    AMESSAGE = ERRRUNNER
                    CALL UTL_WRITE_MESSAGE()
                    WRITE(*,60)' '
                    AMESSAGE = ' '
                  ENDIF
                ENDIF
                IF (ISTAT==0) THEN
                  KTRY = 0
                  560 CONTINUE
                  ISTAT = PLL_CLOSE_AND_DELETE(IU,FNAME)
                  IF (ISTAT>0 .AND. KTRY<11) THEN
                    KTRY = KTRY+1
                    CALL PLL_WAIT()
                    GOTO 560
                  ENDIF
                ENDIF
                !
                !   Delete jrunner.rdy file for failed runner
                FRUNNERNAME = TRIM(RUNNERDIR(IRUNNER))//FNRUNRDY
                INQUIRE(FILE=FRUNNERNAME,EXIST=LEX)
                IF (LEX) THEN
                  KTRY = 0
                  580 CONTINUE
                  OPEN(IU,FILE=FRUNNERNAME,IOSTAT=ISTAT)
                  IF (ISTAT>0 .AND. KTRY<11) THEN
                    KTRY = KTRY+1
                    CALL PLL_WAIT()
                    GOTO 580
                  ENDIF
                  IF (ISTAT==0) THEN
                    KTRY = 0
                    620 CONTINUE
                    ISTAT = PLL_CLOSE_AND_DELETE(IU,FRUNNERNAME)
                    IF (ISTAT>0 .AND. KTRY<11) THEN
                      KTRY = KTRY+1
                      CALL PLL_WAIT()
                      GOTO 620
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO LOOP6
        !
        !   Provide capability to interrupt main program while parallel runs
        !   are being made.
        FNAME = 'jparallel.interrupt'
        INQUIRE(FILE=FNAME,EXIST=LEX)
        IF (LEX) THEN
          WRITE(*,300)TRIM(FNAME)
          READ(*,*)OK2INTERRUPT
          IFIL = UTL_GETUNIT(7,1000)
          KTRY = 0
          640 CONTINUE
          OPEN(IFIL,FILE=FNAME,IOSTAT=ISTAT)
          IF (ISTAT>0 .AND. KTRY<11) THEN
            KTRY = KTRY+1
            CALL PLL_WAIT()
            GOTO 640
          ENDIF
          IF (ISTAT==0) THEN
            KTRY = 0
            660 CONTINUE
            CLOSE(IFIL,STATUS='DELETE',IOSTAT=ISTAT)
            IF (ISTAT>0 .AND. KTRY<11) THEN
              KTRY = KTRY+1
              CALL PLL_WAIT()
              GOTO 660
            ENDIF
          ENDIF
          IF (UTL_SAMENAME(OK2INTERRUPT,'Y')) THEN
            CALL PLL_STOP_RUNNERS()
            CALL UTL_STOP()
          ENDIF
        ENDIF
        !
      ENDIF
      !
      CALL PLL_DONE(NRUNSPLL,IRUNSTAT,DONE,NUMDONE)
      IF (.NOT. DONE) THEN
        IF (NUMDONE .NE. NUMDONELAST) THEN
          NUMDONELAST = NUMDONE
          IF (IVERB>2) WRITE(*,100)NUMDONE,NRUNSPLL
        ENDIF
        ! Build in a short (1 millisecond) delay so that 
        ! program does not consume all available CPU cycles.
        CALL PLL_WAIT(0.001D0)
      ELSE
        IF (IVERB>2) WRITE(*,100)NRUNSPLL,NRUNSPLL
      ENDIF
      !
    ENDDO LOOP1
    !
    RETURN
  END SUBROUTINE PLL_MAKE_RUNS
  ! ----------------------------------------------------------------------------
  SUBROUTINE PLL_READ_DISPAR(IFAIL,NPT,IRUNRUNNER,NRUNRUNNER,PVAL)
    !   Read the contents of a "jdispar.rdy" file and delete it
    USE GLOBAL_DATA, ONLY: IVERB
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                          INTENT(OUT) :: IFAIL
    INTEGER,                          INTENT(IN)  :: NPT
    INTEGER,                          INTENT(OUT) :: IRUNRUNNER
    INTEGER,                          INTENT(OUT) :: NRUNRUNNER
    DOUBLE PRECISION, DIMENSION(NPT), INTENT(OUT) :: PVAL
    !
    !   Local variables
    INTEGER :: I, IOUNIT, ISTAT, K, KD, KTRY
    CHARACTER(LEN=20) :: VARERR = ' '
    LOGICAL :: LEX, LOP, FNDISPARDELETED
    !
    !   Format statements
    100 FORMAT(A)
    120 FORMAT(I10)
    200 FORMAT(3(1X,G24.16))
    270 FORMAT(1X,'Deleting file "',A,'"')
    300 FORMAT(1X,'Error opening file "',A,'"')
    320 FORMAT(1X,'Warning: Unable to delete file "',A,'"')
    720 FORMAT(1X,'Error reading variable "',A,'" from file "',A,'"')
    740 FORMAT(1X,'Closing file and making READ attempt: ',I2)
    !
    IFAIL = 0
    IOUNIT = UTL_GETUNIT(7,1000)
    ISTAT = 0
    K = 0
    KD = 0
    KTRY = 0
    10 CONTINUE
    INQUIRE(UNIT=IOUNIT,OPENED=LOP)
    IF (LOP) CLOSE(IOUNIT,IOSTAT=ISTAT)
    IF (ISTAT>0 .AND. KTRY<21) THEN
      KTRY = KTRY+1
      CALL PLL_WAIT()
      GOTO 10
    ENDIF
    OPEN(IOUNIT,FILE=FNDISPAR,STATUS='OLD',ERR=500)
    VARERR = 'COMMAND'
    READ(IOUNIT,100,ERR=400,END=400) COMMAND
    VARERR = 'IRUNRUNNER'
    READ(IOUNIT,120,ERR=400,END=400) IRUNRUNNER
    VARERR = 'NRUNRUNNER'
    READ(IOUNIT,120,ERR=400,END=400) NRUNRUNNER
    VARERR = 'PVE'
    READ(IOUNIT,200,ERR=400,END=400)(PVAL(I),I=1,NPT)
    IF (IVERB>3) WRITE(*,270) TRIM(FNDISPAR)
    !
    KTRY = 0
    FNDISPARDELETED = (PLL_CLOSE_AND_DELETE(IOUNIT,FNDISPAR,1000) == 0)
    IF (.NOT. FNDISPARDELETED) THEN
      IF (IVERB>0) WRITE(*,320)TRIM(FNDISPAR)
    ENDIF
    RETURN
    !
    ! Error handling
    400 CONTINUE  ! READ error
    IF (VARERR .NE. ' ') THEN
      WRITE(*,720)TRIM(VARERR),TRIM(FNDISPAR)
      ERRRUNNER = ' '
      WRITE(ERRRUNNER,720)TRIM(VARERR),TRIM(FNDISPAR)
      K = K+1
      CLOSE(IOUNIT)
      IF (K<11) THEN
        CALL PLL_WAIT(WAIT)
        INQUIRE(FILE=FNDISPAR,EXIST=LEX)
        IF (LEX) THEN
          WRITE(*,740)K
          GOTO 10
        ELSE
          IFAIL = 1
          RETURN
        ENDIF
      ELSE
        WRITE(*,'(A)')' Warning: WAIT time may be too small'
        CALL PLL_RUNNER_STOP(1)
      ENDIF
    ENDIF
    !
    500 CONTINUE  ! OPEN error
    KD = KD+1
    IF (KD<11) THEN
      CALL PLL_WAIT(WAIT)
      GOTO 10
    ENDIF
    WRITE(*,300) TRIM(FNDISPAR)
    ERRRUNNER = ' '
    WRITE(ERRRUNNER,300) TRIM(FNDISPAR)
    CALL PLL_RUNNER_STOP(1)
  END SUBROUTINE PLL_READ_DISPAR
  ! ----------------------------------------------------------------------------
  SUBROUTINE PLL_READ_RUNDEP(IERR,IRUN,IRUNNER,NVEXT,EXTVAL)
    !   Read the contents of a "jrundep.rdy" file, then delete it
    USE GLOBAL_DATA, ONLY: IVERB, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(OUT) :: IERR
    INTEGER, INTENT(IN)  :: IRUN     ! Run number
    INTEGER, INTENT(IN)  :: IRUNNER  ! Runner number
    INTEGER, INTENT(IN)  :: NVEXT
    DOUBLE PRECISION, DIMENSION(NVEXT), INTENT(OUT) :: EXTVAL
    !
    !   Local variables
    INTEGER :: I, ISTAT, IRUNRUNNER, IUNIT, KTRY
    CHARACTER(LEN=MAX_STRING_LEN) :: FNAME
    LOGICAL :: LEX
    !
    !   Format statements
    100 FORMAT(3(1X,G24.16))
    150 FORMAT(I10)
    180 FORMAT(1X,'Run number (',I5,') read from ',A,' does not match run number ',   &
               I5,/,' -- dependent values not read')
    200 FORMAT(1X,'ERROR opening file "',A,'"',/,1X,'for run number ',I5,   &
               ' on runner number ',I4)
    250 FORMAT(1X,'ERROR reading from file "',A,'"',/,1X,'for run number ',I5)
    300 FORMAT(1X,'ERROR reading from file "',A,'"',/,1X,'for run number ',I5, &
               ' on runner number ',I4)
    350 FORMAT(1X,'ERROR: Can''t delete file "',A,'"',/,1X,'for run number ',I5, &
               ' on runner number ',I4)
    !
    IERR = 0
    FNAME = TRIM(RUNNERDIR(IRUNNER))//FNRUNDEP
    IUNIT = UTL_GETUNIT(7,1000)
    KTRY = 0
    10 CONTINUE
    OPEN(IUNIT,FILE=FNAME,STATUS='OLD',ERR=20,IOSTAT=ISTAT)
    20 CONTINUE
    IF (ISTAT .NE. 0) THEN
      IF (KTRY<11) THEN
        KTRY = KTRY+1
        CALL PLL_WAIT()
        GOTO 10
      ELSE
        IF (IVERB>2) WRITE(*,200)TRIM(FNAME),IRUN,IRUNNER
        IERR = 1
        RETURN
      ENDIF
    ENDIF
    !
    !   Read run number and dependent values from jrundep.rdy file
    KTRY = 0
    30 CONTINUE
    READ(IUNIT,150,IOSTAT=ISTAT)IRUNRUNNER
    IF (ISTAT .NE. 0) THEN
      IF (KTRY>=11) THEN
        IF (IVERB>2) WRITE(*,250)TRIM(FNAME),IRUN
        IERR = 1
        WRITE(*,'(A)')' Warning: WAIT time may be too small'
        35 CONTINUE
        CLOSE(IUNIT,STATUS='KEEP',IOSTAT=ISTAT)
        IF (ISTAT>0 .AND. KTRY<21) THEN
          KTRY = KTRY+1
          CALL PLL_WAIT()
          GOTO 35
        ENDIF
        RETURN
      ELSE
        KTRY = KTRY+1
        CALL PLL_WAIT()
        GOTO 30
      ENDIF
    ENDIF
    IF (IRUNRUNNER==IRUN) THEN
      READ(IUNIT,100,ERR=40,END=40,IOSTAT=ISTAT)(EXTVAL(I),I=1,NVEXT)
      40 CONTINUE
      IF (ISTAT .NE. 0) THEN
        CLOSE(IUNIT,STATUS='KEEP')
        IF (KTRY==10) THEN
          IF (IVERB>2) WRITE(*,300)TRIM(FNAME),IRUN,IRUNNER
          IERR = 1
          WRITE(*,'(A)')' Warning: WAIT time may be too small'
          RETURN
        ELSE
          KTRY = KTRY+1
          CALL PLL_WAIT()
          GOTO 10
        ENDIF
      ENDIF
    ELSE
      IERR = 2 ! Signifies that IRUNRUNNER does not match IRUN
      IF (IVERB>3) WRITE(*,180)IRUNRUNNER,FNRUNDEP,IRUN
    ENDIF
    !
    KTRY = 0
    60 CONTINUE
    ISTAT = PLL_CLOSE_AND_DELETE(IUNIT,FNAME)
    IF (ISTAT .NE. 0) THEN
      IF (KTRY < 10) THEN
        KTRY = KTRY+1
        CALL PLL_WAIT()
        GOTO 60
      ELSE
        IF (IVERB>2) WRITE(*,350)TRIM(FNAME),IRUN,IRUNNER
        ! IERR=3 Signifies that run completed and model-simulated values 
        ! were extracted, but jrundep.rdy could not be deleted
        IF (IERR == 0) IERR = 3 
      ENDIF
    ENDIF
    RETURN
  END SUBROUTINE PLL_READ_RUNDEP
  ! ----------------------------------------------------------------------------
  LOGICAL FUNCTION PLL_RUNNER_READY(IRUNNER) RESULT (READY)
    IMPLICIT NONE
    !
    !   Argument-list variable
    INTEGER, INTENT(IN) :: IRUNNER
    !
    !   Local variables
    CHARACTER(LEN=MAX_STRING_LEN) :: FNAME
    LOGICAL :: LEX
    !
    READY = .FALSE.
    FNAME = TRIM(RUNNERDIR(IRUNNER))//FNRUNRDY
    INQUIRE(FILE=FNAME,EXIST=LEX)
    READY = LEX
    RETURN
  END FUNCTION PLL_RUNNER_READY
  !-----------------------------------------------------------------------------
  SUBROUTINE PLL_RUNNER_STOP(IFLAG)
    !   To be executed by runner when it needs to stop.  If IFLAG=1, create a
    !   jrunfail.fin file, indicating that the runner is stopping due to an
    !   error.
    USE GLOBAL_DATA, ONLY: AMESSAGE, IVERB
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, OPTIONAL, INTENT(IN) :: IFLAG
    !
    !   Local variables
    INTEGER :: IFL, ISTAT, IU, K5, K6, K10, K11, K20, K21, K40, K41, K50, K51
    LOGICAL :: LEX
    !
    !   Format statements
    100 FORMAT(1X,'Runner "',A,'" stopping')
    120 FORMAT(1X,A)
    150 FORMAT(1X,'Runner stopping')
    200 FORMAT(1X,'Runner "',A,'" stopping due to error')
    250 FORMAT(1X,'Runner stopping due to error')
    270 FORMAT(1X,'Deleting file "',A,'"')
    !
    IU = UTL_GETUNIT(7,1000)
    IF (PRESENT(IFLAG)) THEN
      IFL = IFLAG
    ELSE
      IFL = 0
    ENDIF
    !
    K5 = 0
    5 CONTINUE
    INQUIRE(FILE=FNDISRDY,EXIST=LEX)
    IF (LEX) THEN
      OPEN(IU,FILE=FNDISRDY,IOSTAT=ISTAT)
      IF (ISTAT==0) THEN
        IF (IVERB>3) WRITE(*,270)TRIM(FNDISRDY)
        K6 = 0
        6 CONTINUE
        ISTAT = PLL_CLOSE_AND_DELETE(IU,FNDISRDY)
        IF (ISTAT.NE.0) THEN
          K6 = K6+1
          IF (K6<10) THEN
            CALL PLL_WAIT()
            GOTO 6
          ENDIF
        ENDIF
      ELSE
        K5 = K5+1
        IF (K5<10) THEN
          CALL PLL_WAIT()
          GOTO 5
        ENDIF
      ENDIF
    ENDIF
    !
    K10 = 0
    10 CONTINUE
    INQUIRE(FILE=FNRUNRDY,EXIST=LEX)
    IF (LEX) THEN
      OPEN(IU,FILE=FNRUNRDY,IOSTAT=ISTAT)
      IF (ISTAT==0) THEN
        IF (IVERB>3) WRITE(*,270)TRIM(FNRUNRDY)
        K11 = 0
        11 CONTINUE
        ISTAT = PLL_CLOSE_AND_DELETE(IU,FNRUNRDY)
        IF (ISTAT.NE.0) THEN
          K11 = K11+1
          IF (K11<10) THEN
            CALL PLL_WAIT()
            GOTO 11
          ENDIF
        ENDIF
      ELSE
        K10 = K10+1
        IF (K10<10) THEN
          CALL PLL_WAIT()
          GOTO 10
        ENDIF
      ENDIF
    ENDIF
    !
    K20 = 0
    20 CONTINUE
    INQUIRE(FILE=FNRUNDEP,EXIST=LEX)
    IF (LEX) THEN
      OPEN(IU,FILE=FNRUNDEP,IOSTAT=ISTAT)
      IF (ISTAT==0) THEN
        IF (IVERB>3) WRITE(*,270)TRIM(FNRUNDEP)
        K21 = 0
        21 CONTINUE
        ISTAT = PLL_CLOSE_AND_DELETE(IU,FNRUNDEP)
        IF (ISTAT.NE.0) THEN
          CALL PLL_WAIT()
          K21 = K21+1
          IF (K21<10) GOTO 21
        ENDIF        
      ELSE
        K20 = K20+1
        IF (K20<10) THEN
          CALL PLL_WAIT()
          GOTO 20
        ENDIF
      ENDIF
    ENDIF
    !
    30 CONTINUE ! Probably not needed for FNRUNFAIL
    IF (IFL==1) THEN
      OPEN(IU,FILE=FNRUNFAIL,STATUS='REPLACE')
      WRITE(IU,120)TRIM(AMESSAGE)//'  '//TRIM(ERRRUNNER)
      CLOSE(IU,STATUS='KEEP',IOSTAT=ISTAT)
      IF (SNAME .NE. ' ') THEN
        WRITE(*,200)TRIM(SNAME)
      ELSE
        WRITE(*,250)
      ENDIF
    ENDIF
    !
    K40 = 0
    40 CONTINUE
    INQUIRE(FILE=FNDISFIN,EXIST=LEX)
    IF (LEX) THEN
      OPEN(IU,FILE=FNDISFIN,IOSTAT=ISTAT)
      IF (ISTAT==0) THEN
        IF (IVERB>3) WRITE(*,270)TRIM(FNDISFIN)
        K41 = 0
        41 CONTINUE
        ISTAT = PLL_CLOSE_AND_DELETE(IU,FNDISFIN)
        IF (ISTAT.NE.0) THEN
          K41 = K41+1
          IF (K41<10) THEN
            CALL PLL_WAIT()
            GOTO 41
          ENDIF
        ENDIF        
      ELSE
        K40 = K40+1
        IF (K40<10) THEN
          CALL PLL_WAIT()
          GOTO 40
        ENDIF
      ENDIF
    ENDIF
    !
    K50 = 0
    50 CONTINUE
    OPEN(IU,FILE=FNRUNFIN,STATUS='REPLACE',IOSTAT=ISTAT)
    IF (ISTAT==0) THEN
      K51 = 0
      51 CONTINUE
      CLOSE(IU,STATUS='KEEP',IOSTAT=ISTAT)
      IF (ISTAT.NE.0) THEN
        K51 = K51+1
        IF (K51<10) THEN
          CALL PLL_WAIT()
          GOTO 51
        ENDIF
      ENDIF
    ELSE
      K50 = K50+1
      IF (K50<10) THEN
        CALL PLL_WAIT()
        GOTO 50
      ENDIF
    ENDIF
    !
    IF (SNAME .NE. ' ') THEN
      WRITE(*,100)TRIM(SNAME)
    ELSE
      WRITE(*,150)
    ENDIF
    CALL UTL_STOP(' ')
    !
    RETURN
  END SUBROUTINE PLL_RUNNER_STOP
  !-----------------------------------------------------------------------------
  SUBROUTINE PLL_STOP_RUNNERS()
    !   Delete "jdispatch.rdy" file in directories of all runners
    !   Write "jdispatch.fin" file in directories of all runners
    USE GLOBAL_DATA, ONLY: AMESSAGE, IVERB, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    !
    !   Local variables
    LOGICAL :: LEX
    INTEGER :: I, IRSTAT, ISTAT, IU, K, KACTIVE, KTRY
    CHARACTER(LEN=MAX_STRING_LEN) :: FNAME, SYSCOM
    CHARACTER(LEN=16), DIMENSION(-1:11) :: RUNNERSTAT
    DATA RUNNERSTAT/'Nonresponsive   ','Not active      ','Active          ',   &
        'Active          ','Running model   ','Active          ',   &
        'unused          ','unused          ','unused          ',   &
        'Notified to stop','Terminated      ','Stopped -- Error',   &
        'Overdue         '/
    !
    !   Format statements
    120 FORMAT(/,1X,'Status of runners:')
    140 FORMAT(1X,I5,2X,A20,2X,A)
    270 FORMAT(1X,'Deleting file "',A,'"')
    300 FORMAT(1X,'Executing PLL_STOP_RUNNERS')
    320 FORMAT(1X,'Executing system command: ',A)
    650 FORMAT(1X,'ERROR opening file: ',A)
    !
    IF (IVERB>3) WRITE(*,300)
    IU = UTL_GETUNIT(7,1000)
    DO I=1,NUMRUNNERS
      !   First, delete no-longer relevant signal file(s)
      !
      FNAME = TRIM(RUNNERDIR(I))//FNDISPAR
      INQUIRE(FILE=FNAME,EXIST=LEX)
      IF (LEX) THEN
        OPEN(IU,FILE=FNAME,IOSTAT=ISTAT)
        IF (IVERB>3) WRITE(*,270)TRIM(FNAME)
        ISTAT = PLL_CLOSE_AND_DELETE(IU,FNAME)
      ENDIF
      !
      FNAME = TRIM(RUNNERDIR(I))//FNDISRDY
      5 CONTINUE
      INQUIRE(FILE=FNAME,EXIST=LEX)
      IF (LEX) THEN
        OPEN(IU,FILE=FNAME,IOSTAT=ISTAT)
        IF (ISTAT .NE. 0) THEN
          CALL PLL_WAIT()
          GOTO 5
        ENDIF
        IF (IVERB>3) WRITE(*,270)TRIM(FNAME)
        KTRY = 0
        7 CONTINUE
        ISTAT = PLL_CLOSE_AND_DELETE(IU,FNAME)
        IF (ISTAT>0 .AND. KTRY<11) THEN
          KTRY = KTRY+1
          CALL PLL_WAIT()
          GOTO 7
        ENDIF
      ENDIF
      !
      !   Write "jdispatch.fin" file
      !
      FNAME = TRIM(RUNNERDIR(I))//FNDISFIN
      INQUIRE(FILE=FNAME,IOSTAT=ISTAT,EXIST=LEX)
      IF (LEX .AND. ISTAT==0) THEN
        OPEN(IU,FILE=FNAME,IOSTAT=ISTAT)
        ISTAT = PLL_CLOSE_AND_DELETE(IU,FNAME)
      ENDIF
      !
      FNAME = TRIM(RUNNERDIR(I))//'j_temp.fin'
      OPEN(IU,FILE=FNAME,STATUS='REPLACE',ERR=550,IOSTAT=ISTAT)
      550 CONTINUE
      IF (ISTAT .NE. 0) THEN
        WRITE(*,650)TRIM(FNAME)
      ELSE
        IF (AUTOSTOPRUNNERS) THEN
          WRITE(IU,'(A)')'STOP'
        ELSE
          WRITE(IU,'(A)')'RESET'
        ENDIF
        KTRY = 0
        20 CONTINUE
        CLOSE(IU,STATUS='KEEP',IOSTAT=ISTAT)
        IF (ISTAT>0 .AND. KTRY<11) THEN
          KTRY = KTRY+1
          CALL PLL_WAIT()
          GOTO 20
        ENDIF
        IF (OS_DIS == 'WINDOWS') THEN
          SYSCOM = TRIM(RENAME_DIS)//' '//TRIM(FNAME)//' '//FNDISFIN
        ELSE
          SYSCOM = TRIM(RENAME_DIS)//' '//TRIM(FNAME)//' '//   &
                   TRIM(RUNNERDIR(I))//FNDISFIN
        ENDIF
        IF (IVERB>3) WRITE(*,320)TRIM(SYSCOM)
        CALL UTL_SYSTEM(SYSCOM)
      ENDIF
      !
    ENDDO
    !
    K = 0
    LOOP1: DO WHILE (K<100)
      K = K+1
      DO I=1,NUMRUNNERS
        FNAME = TRIM(RUNNERDIR(I))//FNRUNFIN
        INQUIRE(FILE=FNAME,EXIST=LEX)
        IF (LEX) THEN
          OPEN(IU,FILE=FNAME)
          IF (IVERB>3) WRITE(*,270)TRIM(FNAME)
          KTRY = 0
          10 CONTINUE
          ISTAT = PLL_CLOSE_AND_DELETE(IU,FNAME)
          IF (ISTAT .NE. 0) THEN
            IF (KTRY==100) THEN
              AMESSAGE = 'File access conflict for file "'//TRIM(FNAME)//'"'
              CALL UTL_STOP()
            ELSE
              KTRY = KTRY+1
              CALL PLL_WAIT()
              GOTO 10
            ENDIF
          ENDIF
          IF (IRUNNERSTAT(I) .NE. 10) IRUNNERSTAT(I) = 9
          FNAME = TRIM(RUNNERDIR(I))//FNDISFIN
          INQUIRE(FILE=FNAME,EXIST=LEX)
          IF (LEX) THEN
            OPEN(IU,FILE=FNAME,IOSTAT=ISTAT)
            IF (IVERB>3) WRITE(*,270)TRIM(FNAME)
            ISTAT = PLL_CLOSE_AND_DELETE(IU,FNAME)
          ENDIF
        ENDIF
      ENDDO
      !
      KACTIVE = 0
      DO I=1,NUMRUNNERS
        IF (IRUNNERSTAT(I)>-1 .AND. IRUNNERSTAT(I)<9) KACTIVE = KACTIVE+1
      ENDDO
      !
      IF (KACTIVE==0) GOTO 800
      CALL PLL_WAIT()
    ENDDO LOOP1
    !
    800 CONTINUE
    !   Write status of runners to screen
      WRITE(*,120)
      DO I=1,NUMRUNNERS
        IF (IRUNNERSTAT(I)>0) THEN
          IRSTAT = IRUNNERSTAT(I)
        ELSE
          IRSTAT = -1
        ENDIF
        WRITE(*,140)I,RUNNERNAME(I),RUNNERSTAT(IRSTAT)
      ENDDO
    !
    RETURN
  END SUBROUTINE PLL_STOP_RUNNERS
  !-----------------------------------------------------------------------------
  SUBROUTINE PLL_WAIT(TSECS)
    !   Wait a while
    USE UTILITIES, ONLY: UTL_SLEEP
    IMPLICIT NONE
    !
    !   Argument-list variable
    DOUBLE PRECISION, OPTIONAL, INTENT(IN) :: TSECS
    !
    !   Local variables
    DOUBLE PRECISION :: SECS
    !
    IF (PRESENT(TSECS)) THEN
      SECS = TSECS
    ELSE
      SECS = WAIT
    ENDIF
    !
    CALL UTL_SLEEP(SECS)
    !
    RETURN
  END SUBROUTINE PLL_WAIT
  ! ----------------------------------------------------------------------------
  SUBROUTINE PLL_WRITE_DISPAR(IFAIL,IRUN,IRUNNER,NPT,NRUNSPLL,PVAL)
    !   Write a "jdispar.rdy" file
    USE GLOBAL_DATA, ONLY: IVERB, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument list variables
    INTEGER, INTENT(OUT) :: IFAIL
    INTEGER, INTENT(IN)  :: IRUN        ! Run number
    INTEGER, INTENT(IN)  :: IRUNNER     ! Runner number
    INTEGER, INTENT(IN)  :: NPT         ! Number of  parameters
    INTEGER, INTENT(IN)  :: NRUNSPLL
    DOUBLE PRECISION, DIMENSION(NPT), INTENT(IN) :: PVAL ! Parameter values
    !
    !   Local variables
    CHARACTER(LEN=MAX_STRING_LEN) :: RENAM, TEMPNAME
    INTEGER :: I, IRDY, ISTAT
    CHARACTER(LEN=10) :: CHDATE, CHTIME, CHZONE
    INTEGER, DIMENSION(8) :: IEDT
    !
    !   Format statements
    100 FORMAT(A)
    120 FORMAT(I10)
    200 FORMAT(3(1X,G24.16))
    270 FORMAT(1X,'Deleting file "',A,'"')
    300 FORMAT(1X,'Executing command: "',A,'" for run ',I5)
    320 FORMAT(1X,'Signal sent to start run ',I3,' on runner ',A,   &
               ' at ',I4,'/',I2.2,'/',I2.2,1X,I2,':',I2.2,':',I2.2)
    !
    TEMPNAME = TRIM(RUNNERDIR(IRUNNER))//'j_temp.rdy'
    IFAIL = 0
    IRDY = UTL_GETUNIT(7,1000)
    OPEN(IRDY,FILE=TEMPNAME,STATUS='REPLACE',IOSTAT=ISTAT)
    IF (ISTAT .NE. 0) THEN
      IFAIL = 1
      RETURN
    ENDIF
    !   Write data to local, temporary file
    WRITE(IRDY,100)COMMAND
    WRITE(IRDY,120)IRUN
    WRITE(IRDY,120)NRUNSPLL
    WRITE(IRDY,200)(PVAL(I),I=1,NPT)
    CLOSE(IRDY,STATUS='KEEP')
    !
    IF (OS_DIS == 'WINDOWS') THEN
      RENAM=TRIM(RENAME_DIS)//' '//TRIM(TEMPNAME)//' '//FNDISPAR
    ELSE
      RENAM=TRIM(RENAME_DIS)//' '//TRIM(TEMPNAME)//' '//   &
            TRIM(RUNNERDIR(IRUNNER))//FNDISPAR
    ENDIF
    IF (IVERB>3) WRITE(*,300)TRIM(RENAM),IRUN
    CALL UTL_SYSTEM(RENAM)
    OPEN(IRDY,FILE=TEMPNAME)
    IF (IVERB>3) WRITE(*,270)TRIM(TEMPNAME)
    ISTAT = PLL_CLOSE_AND_DELETE(IRDY,TEMPNAME)
    CALL DATE_AND_TIME(CHDATE,CHTIME,CHZONE,IEDT)
    IF (IVERB>2) WRITE(*,320)IRUN,TRIM(RUNNERNAME(IRUNNER)),   &
                             (IEDT(I),I=1,3),(IEDT(I),I=5,7)
    !
    RETURN
  END SUBROUTINE PLL_WRITE_DISPAR
  !-----------------------------------------------------------------------------
  SUBROUTINE PLL_WRITE_DISRDY(IRUNNER,LCIS,NCATS,NINSTRUCT,NMIFILE,NMOFILE,   &
                              NOPNT,NPT,NUMLADV,NVEXT,PRECISPRO,CATGOR,   &
                              CINSTSET,EXTNAM,INSTRUCTFILE,LOCINS,   &
                              MCVUSE,MIFILE,MOFILE,MRKDL,NW,PARNAM,   &
                              TFILE)
    !   Indicate dispatcher is ready, by creating file "jdispatch.rdy" in a runner
    !   directory.  jdispatch.rdy contains command to start model, and other
    !   data needed by the runner.
    !
    !   Contents of jdispatch.rdy:
    !      RUNNERNAME
    !      COMMAND to start model
    !      WAIT (seconds)
    !      IVERBRUNNER
    !      Number of parameters(?)
    !      Number of model-input files
    !      Number of dependents to be extracted
    !      Number of model-output files
    !      Length of instructions array
    !      Extraction instructions
    !      ... (more data from MODEL_IO module?)
    !
    USE UTILITIES
    USE GLOBAL_DATA, ONLY: IVERB, LENDNAM, MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                    INTENT(IN) :: IRUNNER
    INTEGER,                                    INTENT(IN) :: LCIS
    INTEGER,                                    INTENT(IN) :: NCATS
    INTEGER,                                    INTENT(IN) :: NINSTRUCT
    INTEGER,                                    INTENT(IN) :: NMIFILE
    INTEGER,                                    INTENT(IN) :: NMOFILE
    INTEGER,                                    INTENT(IN) :: NOPNT
    INTEGER,                                    INTENT(IN) :: NPT
    INTEGER,                                    INTENT(IN) :: NUMLADV
    INTEGER,                                    INTENT(IN) :: NVEXT
    INTEGER,                                    INTENT(IN) :: PRECISPRO
    CHARACTER(LEN=6),              DIMENSION(NMOFILE), INTENT(IN) :: CATGOR ! Model-calc'd val. cat. for each model-output file
    CHARACTER(LEN=1),              DIMENSION(LCIS),    INTENT(IN) :: CINSTSET ! holds compressed instruction set
    CHARACTER(LEN=LENDNAM),        DIMENSION(NVEXT),   INTENT(IN) :: EXTNAM
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NMOFILE), INTENT(IN) :: INSTRUCTFILE ! Instruction file names
    INTEGER,                       DIMENSION(NINSTRUCT), INTENT(IN) :: LOCINS ! pointer to instructions
    LOGICAL,                       DIMENSION(NCATS),   INTENT(IN) :: MCVUSE
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NMIFILE), INTENT(IN) :: MIFILE
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NMOFILE), INTENT(IN) :: MOFILE ! Model output file names
    CHARACTER(LEN=1),              DIMENSION(NMOFILE), INTENT(IN) :: MRKDL  ! Marker delimiters
    INTEGER,                       DIMENSION(NPT),     INTENT(IN) :: NW
    CHARACTER(LEN=12),             DIMENSION(NPT),     INTENT(IN) :: PARNAM
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NMIFILE), INTENT(IN) :: TFILE
    !
    !   Local variables
    INTEGER :: IRDY, ISTAT, ISTAT2, ITEMP, J, K, KK, KTRY
    LOGICAL :: LEX, LOP
    CHARACTER(LEN=MAX_STRING_LEN) :: ERRORMSG, FNAME, TEMPNAME, RENAM
    !
    !   Format statements
    6 FORMAT(/,'Attempting to write ',a,' in runner directory ',a)
    100 FORMAT(A)
    150 FORMAT(I10)
    200 FORMAT(G12.5)
    300 FORMAT(1X,'Executing command: "',A,'"')
    320 FORMAT(6(1X,A12))
    330 FORMAT(6(1X,I4,8X))
    360 FORMAT(3(1X,A))
    380 FORMAT(10(1X,L1))
    420 FORMAT(10(1X,A6))
    440 FORMAT(40(1X,A1))
    460 FORMAT(80A1)
    480 FORMAT(8I10)
    !
    IF (.NOT. DOPLL) RETURN
    !
    IF (IVERB>3) WRITE(*,6)TRIM(FNDISRDY),TRIM(RUNNERDIR(IRUNNER))
    TEMPNAME = TRIM(RUNNERDIR(IRUNNER))//'j_temp.rdy'
    IRDY = UTL_GETUNIT(7,1000)
    ! Add code to make writing of j_temp.rdy more robust - 6/21/2010 ERB
    KTRY = 0
    10 CONTINUE
    OPEN(IRDY,FILE=TEMPNAME,STATUS='REPLACE',IOSTAT=ISTAT)
    INQUIRE(UNIT=IRDY,OPENED=LOP,IOSTAT=ISTAT2)
    IF (.NOT. LOP .AND. (ISTAT.NE.0 .OR. ISTAT2.NE.0)) THEN
      KTRY = KTRY + 1
      CALL PLL_WAIT()
      IF (KTRY<10) GOTO 10
      IRUNNERSTAT(IRUNNER) = -1
      RETURN
    ENDIF
    !
    WRITE(IRDY,100)TRIM(RUNNERNAME(IRUNNER))
    WRITE(IRDY,100)RENAME_DIS
    WRITE(IRDY,200)WAITRUNNERS  ! Changed 6/21/2010 ERB
    WRITE(IRDY,150)IVERBRUNNER
    WRITE(IRDY,150)LCIS
    WRITE(IRDY,150)NINSTRUCT
    WRITE(IRDY,150)NMIFILE
    WRITE(IRDY,150)NMOFILE
    WRITE(IRDY,150)NPT
    WRITE(IRDY,150)NOPNT
    WRITE(IRDY,150)PRECISPRO
    WRITE(IRDY,150)NCATS
    WRITE(IRDY,150)NVEXT
    WRITE(IRDY,150)NUMLADV
    WRITE(IRDY,100)(MIFILE(J),J=1,NMIFILE)
    WRITE(IRDY,100)(TFILE(J),J=1,NMIFILE)
    WRITE(IRDY,320)(PARNAM(J),J=1,NPT)
    WRITE(IRDY,330)(NW(J),J=1,NPT)
    WRITE(IRDY,420)(CATGOR(J),J=1,NMOFILE)
    WRITE(IRDY,360)(EXTNAM(J),J=1,NVEXT)
    WRITE(IRDY,100)(INSTRUCTFILE(J),J=1,NMOFILE)
    WRITE(IRDY,100)(MOFILE(J),J=1,NMOFILE)
    WRITE(IRDY,440)(MRKDL(J),J=1,NMOFILE)
    WRITE(IRDY,380)(MCVUSE(J),J=1,NCATS)
    WRITE(IRDY,460)(CINSTSET(J),J=1,LCIS)
    WRITE(IRDY,480)(LOCINS(J),J=1,NINSTRUCT)
    !
    ! Close j_temp.rdy and ensure that it is closed
    CLOSE(IRDY)
    K = 0
    20 CONTINUE
    INQUIRE(UNIT=IRDY,OPENED=LOP)
    IF (LOP) THEN
      CALL PLL_WAIT()
    CLOSE(IRDY)
      K = K + 1
      IF (K<30) THEN
        GOTO 20
      ELSE
        RETURN
      ENDIF
    ENDIF
    IF (IVERB>3) WRITE(*,500)TRIM(RUNNERDIR(IRUNNER))
    500 FORMAT('j_temp.rdy in directory ',a,' has been closed')
    !
    ! Delete existing jdispatch.rdy file
    FNAME = TRIM(RUNNERDIR(IRUNNER))//FNDISRDY
    INQUIRE(FILE=FNAME,EXIST=LEX)
    IF (LEX) THEN
      OPEN(IRDY,FILE=FNAME)
      ISTAT = PLL_CLOSE_AND_DELETE(IRDY,FNAME)
      IF (ISTAT /= 0) THEN
        RETURN
      ENDIF
    ENDIF
    IF (IVERB>3) WRITE(*,510)TRIM(FNAME)
    510 FORMAT(' File ',a,' has been deleted or did not exist.')
    !
    ! Rename j_temp.rdy to jdispatch.rdy
    IF (OS_DIS == 'WINDOWS') THEN
      RENAM=TRIM(RENAME_DIS)//' '//TRIM(TEMPNAME)//' '//TRIM(FNDISRDY)
    ELSE
      RENAM=TRIM(RENAME_DIS)//' '//TRIM(TEMPNAME)//' '//   &
            TRIM(RUNNERDIR(IRUNNER))//FNDISRDY
    ENDIF
    IF (IVERB>3) WRITE(*,300)TRIM(RENAM)
    
    ISTAT = 1
    CALL UTL_SYSTEM(RENAM)
    CALL PLL_WAIT()
    25 CONTINUE
    INQUIRE(FILE=FNAME,EXIST=LEX)
    IF (LEX) THEN
      INQUIRE(FILE=FNAME,OPENED=LOP)
      IF (LOP) THEN
        INQUIRE(FILE=FNAME,NUMBER=ITEMP)
        IF (ITEMP>0) THEN
          K = 0
          30 CONTINUE
          INQUIRE(UNIT=ITEMP,OPENED=LOP)
          IF (LOP) THEN
            CALL PLL_WAIT()
            CLOSE(ITEMP)
            K = K + 1
            IF (K<30) THEN
              GOTO 30
            ELSE
              IF (IVERB>3) WRITE(*,514)TRIM(FNAME)
              514 FORMAT('File ',a,' cannot be closed')            
              RETURN
            ENDIF
          ENDIF
          IF (IVERB>3) WRITE(*,515)TRIM(FNAME)
          515 FORMAT('File ',a,' has been closed')
          ISTAT = 0
        ENDIF
      ELSE
        ISTAT = 0
      ENDIF
    ELSE
      KK = KK + 1
      IF (KK < 30) THEN
        CALL PLL_WAIT()
        GOTO 25
      ELSE
        IF (IVERB>3) WRITE(*,518)TRIM(FNAME)
        518 FORMAT('File ',a,' has not been created')
        RETURN
      ENDIF
    ENDIF
    !
    IF (ISTAT==0) THEN
    IRUNNERSTAT(IRUNNER) = 2
      IF (IVERB>3) WRITE(*,520)TRIM(RENAM)
      520 FORMAT('Command ',a,' has succeeded')
    ELSE
      IF (IVERB>3) WRITE(*,530)TRIM(RENAM)
      530 FORMAT('Command ',a,' has failed')
    ENDIF
    !
    RETURN
  END SUBROUTINE PLL_WRITE_DISRDY
  ! ----------------------------------------------------------------------------
  SUBROUTINE PLL_WRITE_RUNDEP(IRUN,NVEXT,EXTVAL)
    !   Write dependent values to "jrundep.rdy" file
    USE GLOBAL_DATA, ONLY: IVERB, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                            INTENT(IN) :: IRUN
    INTEGER,                            INTENT(IN) :: NVEXT
    DOUBLE PRECISION, DIMENSION(NVEXT), INTENT(IN) :: EXTVAL
    !
    !   Local variables
    INTEGER :: I, IUNIT
    CHARACTER(LEN=MAX_STRING_LEN) :: SYSCOM
    CHARACTER(LEN=11) :: FNAME
    !
    !   Format statements
    100 FORMAT(3(1X,G24.16))
    150 FORMAT(I10)
    200 FORMAT(1X,'File "',A,'" has been written')
    !
    IUNIT = UTL_GETUNIT(7,1000)
    FNAME = 'temp_rundep'
    OPEN(IUNIT,FILE=FNAME,STATUS='REPLACE')
    WRITE(IUNIT,150)IRUN
    WRITE(IUNIT,100)(EXTVAL(I),I=1,NVEXT)
    CLOSE(IUNIT,STATUS='KEEP')
    SYSCOM = TRIM(SRENAME)//' '//FNAME//' '//FNRUNDEP
    CALL UTL_SYSTEM(SYSCOM)
    IF (IVERB>3) WRITE(*,200)FNRUNDEP
    RETURN
  END SUBROUTINE PLL_WRITE_RUNDEP
  !-----------------------------------------------------------------------------
  LOGICAL FUNCTION PLL_JRUNDEP_DELETED(IRUNNER) RESULT (OK)
    ! Ensure that runner directory does not contain a jrundep.rdy file
    USE UTILITIES, ONLY: UTL_GETUNIT
    IMPLICIT NONE
    ! Argument-list variables
    INTEGER, INTENT(IN) :: IRUNNER
    ! Local variables
    INTEGER :: ISTAT, IU, KTRY
    LOGICAL :: LEX
    CHARACTER(LEN=MAX_STRING_LEN) :: FNAME
    !
    OK = .FALSE.
    FNAME = TRIM(RUNNERDIR(IRUNNER))//FNRUNDEP
    KTRY = 0
    20 CONTINUE
    INQUIRE(FILE=FNAME,IOSTAT=ISTAT,EXIST=LEX)
    IF (ISTAT==0) THEN
      IF (LEX) THEN
        IU = UTL_GETUNIT(7,1000)
        OPEN(IU,FILE=FNAME,IOSTAT=ISTAT)
        IF (ISTAT==0) CLOSE(IU,STATUS='DELETE',IOSTAT=ISTAT)
        CALL PLL_WAIT()
        KTRY = KTRY + 1
        IF (KTRY<20) GOTO 20
      ELSE
        OK = .TRUE.
      ENDIF
    ENDIF
    RETURN
  END FUNCTION PLL_JRUNDEP_DELETED
  !-----------------------------------------------------------------------------
END MODULE PARALLEL_PROCESSING
