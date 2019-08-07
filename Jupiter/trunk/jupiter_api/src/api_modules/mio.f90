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
MODULE MODEL_IO
  USE DATATYPES
  USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
  PRIVATE
  !
  !   PUBLIC DATA
  !
  PUBLIC LLPTRMODIN, LLPTRMODOUT
  !
  TYPE (LLIST), POINTER :: LLPTRMODIN  ! Ptr to head of first list (model-input files)
  TYPE (LLIST), POINTER :: LLPTRMODOUT ! Ptr to head of first list (model-output files)
  !
  !   PUBLIC SUBPROGRAMS
  PUBLIC MIO_INI_ALLOC,              &
         MIO_INI_ARRAYS,             &
         MIO_INI_DIMENSION,          &
         MIO_INI_INPUTFILES,         &
         MIO_INI_INPUTFILES_RUNNER,  &
         MIO_INI_INSTRUCT1,          &
         MIO_INI_INSTRUCT2,          &
         MIO_INI_INSTRUCTALLOC,      &
         MIO_INI_INSTRUCT_RUNNER,    &
         MIO_INI_OUTPUTFILES,        &
         MIO_INI_OUTPUTFILES_RUNNER, &
         MIO_INI_TEMPLATE,           &
         MIO_ADA_MODINFILE_CHANGE,   &
         MIO_ADA_WRITEFILES,         &
         MIO_EXT,                    &
         MIO_EXT_MODOUTFILE_CHANGE,  &
         MIO_CLN_DEALLOC,            &
         MIO_CLN_OUTFILES
  !
  !   PRIVATE DATA
  !
  ! -- Variables

  INTEGER                        :: NUMINFILE  ! Number of model-input files
  INTEGER                        :: NUMOUTFILE ! Number of model-output files
  CHARACTER (LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: MODINFILE  ! Model-input file names
  CHARACTER (LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:) :: TEMPFILE   ! Template file names
  CHARACTER (LEN=1),   ALLOCATABLE, DIMENSION(:) :: PARDEL     ! Parameter delimiters
  CHARACTER (LEN=MAX_STRING_LEN)            :: AFILE   ! Temporary character storage
  CHARACTER (LEN=50000)          :: DLINE   ! Character string for text storage
  CHARACTER (LEN=23), ALLOCATABLE, DIMENSION(:)  :: PWORD ! Word for each parameter value
  !
  ! -- Variables for model output
  INTEGER                            :: LENCIS   ! size of CISET array
  INTEGER                            :: NUML     ! size of LADV array
  INTEGER                            :: NINSTR   ! size of LCINS array
  INTEGER                            :: MCALL    ! number of model calls
  INTEGER, ALLOCATABLE, DIMENSION(:) :: LADV     ! holds line advance data
  INTEGER, ALLOCATABLE, DIMENSION(:) :: LCINS    ! pointer to instructions
  INTEGER, ALLOCATABLE, DIMENSION(:) :: INS1     ! stores part of instruction
  INTEGER, ALLOCATABLE, DIMENSION(:) :: INS2     ! stores part of instruction
  INTEGER, ALLOCATABLE, DIMENSION(:) :: INS3     ! stores part of instruction
  CHARACTER (LEN=1), ALLOCATABLE, DIMENSION(:) :: CISET  ! holds compressed instruction set
  !
  !   Variables related to extraction instructions
  CHARACTER (LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:)  :: MODOUTFILE ! Model output file names
  CHARACTER (LEN=1),              ALLOCATABLE, DIMENSION(:)  :: MRKDEL     ! Marker delimiters
  CHARACTER (LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:)  :: INSFILE    ! Instruction file names
  CHARACTER (LEN=6),              ALLOCATABLE, DIMENSION(:)  :: CATEGORY   ! Model-calculated value category for each model-output file
  !
  !   Default column order for table of model-input files
  INTEGER, PARAMETER                               :: NMODINCOLS = 2
  CHARACTER(LEN=40), DIMENSION(NMODINCOLS), TARGET :: MODINCOL =    &
      (/ 'MODINFILE   ','TEMPLATEFILE' /)
  !
  !   Default column order for table of model-output files
  INTEGER, PARAMETER                                :: NMODOUTCOLS = 3
  CHARACTER(LEN=40), DIMENSION(NMODOUTCOLS), TARGET :: MODOUTCOL =    &
      (/ 'MODOUTFILE     ','INSTRUCTIONFILE','CATEGORY       ' /)
  !
CONTAINS
  !
  !-----------------------------------------------------------------------------
  SUBROUTINE MIO_INI_ALLOC(IFAIL,NPT)
    !
    ! Initialize the MODEL_IO module.
    !
    USE GLOBAL_DATA, ONLY: AMESSAGE, ERRSUB
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(OUT) :: IFAIL
    INTEGER, INTENT(IN)  :: NPT      ! number of parameters
    !
    !   Local variables
    INTEGER :: IERR
    !
    ERRSUB='Error in subroutine MIO_INI_ALLOC:'
    IFAIL=0
    !
    !   Allocate PWORD array
    IF((NPT.LE.0))THEN
      WRITE(AMESSAGE,30) TRIM(ERRSUB)
30    FORMAT(A,' Variable NPT must be supplied as positive.')
      IFAIL=1
      RETURN
    END IF
    !
    IERR = 0
    IF (.NOT. ALLOCATED(PWORD)) ALLOCATE(PWORD(NPT),STAT=IERR)
    IF (IERR.NE.0) GO TO 9200
    !
    RETURN

9200  WRITE(AMESSAGE,9210) TRIM(ERRSUB)
9210  FORMAT(A,   &
' CANNOT ALLOCATE SUFFICIENT MEMORY TO STORE MODEL INTERFACE WORK ARRAYS.')
    IFAIL=1
    RETURN
    !
  END SUBROUTINE MIO_INI_ALLOC
  !-----------------------------------------------------------------------------
  SUBROUTINE MIO_INI_ARRAYS(IFAIL,LCIS,NINSTRUCT,NMIFILE,NMOFILE,CATGOR,   &
                            CINSTSET,INSTRUCTFILE,LOCINS,MIFILE,MOFILE,   &
                            MRKDL,TFILE)
    !   Populate argument-list arrays with values from module arrays
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                             INTENT(OUT) :: IFAIL
    INTEGER,                                             INTENT(IN)  :: LCIS
    INTEGER,                                             INTENT(IN)  :: NINSTRUCT
    INTEGER,                                             INTENT(IN)  :: NMIFILE
    INTEGER,                                             INTENT(IN)  :: NMOFILE
    CHARACTER(LEN=6),              DIMENSION(NMOFILE),   INTENT(OUT) :: CATGOR ! Model-calc'd val. cat. for each model-output file
    CHARACTER(LEN=1),              DIMENSION(LCIS),      INTENT(OUT) :: CINSTSET  ! holds compressed instruction set
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NMOFILE),   INTENT(OUT) :: INSTRUCTFILE
    INTEGER,                       DIMENSION(NINSTRUCT), INTENT(OUT) :: LOCINS    ! pointer to instructions
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NMIFILE),   INTENT(OUT) :: MIFILE
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NMOFILE),   INTENT(OUT) :: MOFILE
    CHARACTER(LEN=1),              DIMENSION(NMOFILE),   INTENT(OUT) :: MRKDL
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NMIFILE),   INTENT(OUT) :: TFILE
    !
    IFAIL = 0
    IF (NMIFILE==NUMINFILE) THEN
      MIFILE = MODINFILE
      TFILE = TEMPFILE
    ELSE
      IFAIL = 1
    ENDIF
    !
    IF (NMOFILE==NUMOUTFILE) THEN
      CATGOR = CATEGORY
      INSTRUCTFILE = INSFILE
      MOFILE = MODOUTFILE
      MRKDL = MRKDEL
    ELSE
      IFAIL = 1
    ENDIF
    !
    IF (LCIS==LENCIS) THEN
      CINSTSET = CISET
    ELSE
      IFAIL = 1
    ENDIF
    !
    IF (NINSTRUCT==NINSTR) THEN
      LOCINS = LCINS
    ELSE
      IFAIL = 1
    ENDIF
    !
    RETURN
  END SUBROUTINE MIO_INI_ARRAYS
  !-----------------------------------------------------------------------------
  SUBROUTINE MIO_INI_DIMENSION(LCIS,NINSTRUCT,NMIFILE,NMOFILE,NUMLADV)
    !   Provide dimensioning integers of the Model_IO module
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(OUT) :: LCIS
    INTEGER, INTENT(OUT) :: NINSTRUCT
    INTEGER, INTENT(OUT) :: NMIFILE ! Number of model-input files
    INTEGER, INTENT(OUT) :: NMOFILE ! Number of model-output files
    INTEGER, INTENT(OUT) :: NUMLADV
    !
    LCIS = LENCIS
    NINSTRUCT = NINSTR
    NMIFILE = NUMINFILE
    NMOFILE = NUMOUTFILE
    NUMLADV = NUML
    RETURN
  END SUBROUTINE MIO_INI_DIMENSION
  !-----------------------------------------------------------------------------
  SUBROUTINE MIO_INI_INPUTFILES(INUNIT,IOUT)
    !   Read file names and allocate and populate arrays needed for model input
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: AMESSAGE, ERRSUB, IVERB
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(IN)  :: INUNIT ! Unit number of file from which file names are to be read
    INTEGER, INTENT(IN)  :: IOUT
    !
    !   Local variables
    INTEGER :: I, IERR, IFAIL, MORE
    TYPE (LLIST), POINTER :: TAIL
    !
    NULLIFY(LLPTRMODIN)
    NULLIFY(TAIL)
    ERRSUB = 'Error in subroutine MIO_INI_INPUTFILES:'
    IFAIL = 0
    !
    !   Make call to UTLREADBLOCK to read input blocks containing names
    !   of model-input and corresponding template files
    NUMINFILE = 0
    CALL UTL_READBLOCK(NMODINCOLS,'MODEL_INPUT_FILES',MODINCOL,INUNIT,IOUT,  &
        'MODINFILE',.TRUE.,LLPTRMODIN,TAIL,NUMINFILE)
    !
    IF (NUMINFILE.LE.0) THEN
      WRITE(AMESSAGE,12)
12    FORMAT('Error: No model-input files were specified.')
      IFAIL=1
      CALL UTL_STOP(' ')
    END IF
    !
    !   Allocate arrays for storing file names
    ALLOCATE (MODINFILE(NUMINFILE),   &
              TEMPFILE(NUMINFILE),PARDEL(NUMINFILE),STAT=IERR)
    !
    IF(IERR.NE.0)THEN
      WRITE(AMESSAGE,20) TRIM(ERRSUB)
20    FORMAT(A,' cannot allocate sufficient memory to store',   &
             ' model-interface filenames.')
      IFAIL=1
      CALL UTL_STOP(' ')
    END IF
    !
    TEMPFILE=' '
    MODINFILE=' '
    !
    !   Populate arrays of file names by traversing the linked lists
    CALL UTL_FILTERLIST(LLPTRMODIN,IOUT,'MODINFILE',NUMINFILE,IFAIL,   &
                        MODINFILE,MORE)
    IF (IFAIL.NE.0) CALL UTL_STOP(' ')
    CALL UTL_FILTERLIST(LLPTRMODIN,IOUT,'TEMPLATEFILE',NUMINFILE,IFAIL,   &
                        TEMPFILE,MORE)
    IF (IFAIL.NE.0) CALL UTL_STOP(' ')
    !
    !   Echo input
    IF (IVERB.GE.3) THEN
      WRITE(IOUT,150)
      150 FORMAT(//,1X,'MODEL-INPUT AND CORRESPONDING TEMPLATE FILES:',/)
      WRITE(IOUT,160) (TRIM(MODINFILE(I)),TRIM(TEMPFILE(I)),I=1,NUMINFILE)
      160 FORMAT(1X,'Model-input file: ',A,3X,'Template file: ',A)
    ENDIF

    !   Deallocate linked lists

    RETURN
  END SUBROUTINE MIO_INI_INPUTFILES
  !-----------------------------------------------------------------------------
  SUBROUTINE MIO_INI_INPUTFILES_RUNNER(IFAIL,NMIFILE,MIFILE,TFILE)
    !   Allocate and populate arrays needed for model input
    USE GLOBAL_DATA, ONLY: AMESSAGE, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                           INTENT(OUT) :: IFAIL
    INTEGER,                                           INTENT(IN)  :: NMIFILE ! Number of model-input files
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NMIFILE), INTENT(IN)  :: MIFILE
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NMIFILE), INTENT(IN)  :: TFILE
    !
    !   Local variables
    INTEGER :: IERR
    !
    !
    IFAIL = 0
    NUMINFILE = NMIFILE
    IF (NUMINFILE.LE.0) THEN
      WRITE(AMESSAGE,10)
10    FORMAT('Error: No model-input files were specified.')
      IFAIL = 1
      RETURN
    END IF
    !
    !   Allocate arrays for storing file names
    ALLOCATE (MODINFILE(NUMINFILE),TEMPFILE(NUMINFILE),   &
              PARDEL(NUMINFILE),STAT=IERR)
    !
    IF(IERR.NE.0)THEN
      WRITE(AMESSAGE,20)
20    FORMAT('Error in subroutine MIO_INI_INPUTFILES_RUNNER: cannot',   &
             ' allocate sufficient memory to store',   &
             ' model-interface filenames.')
      IFAIL = 1
      RETURN
    END IF
    !
    !   Populate arrays of file names
    MODINFILE = MIFILE
    TEMPFILE = TFILE
    !
    RETURN
  END SUBROUTINE MIO_INI_INPUTFILES_RUNNER
  !-----------------------------------------------------------------------------
  SUBROUTINE MIO_INI_INSTRUCT1(IFAIL)
    !
    ! -- Subroutine MIO_INI_INSTRUCT1 reads all instruction files to determine
    !    dimensioning variables
    !
    USE GLOBAL_DATA, ONLY: AMESSAGE, ERRSUB, LENDNAM
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                INTENT(OUT) :: IFAIL
    !
    !   Local variables
    INTEGER :: I, NBLBMX, IUNIT, IERR, NBLC, J
    INTEGER :: ICOL, IREADCOL, ISTART, ISTOP, KLINE, KREAD,   &
               LENDLINE, NCOLSKIP, NFIELD, NREAD, NSKIP
    DOUBLE PRECISION :: RDUM
    LOGICAL :: STDFILE
    CHARACTER(LEN=LENDNAM) :: DEPNAM
    500 FORMAT('l',I10.10)
    510 FORMAT('l1 ',10000(A))
    520 FORMAT(1X,'ERROR: Blank line encountered in instruction file "',A,'"')

    ERRSUB='Error in user-supplied instruction set for reading model output files:'
    IFAIL=0
    LENCIS=0
    NINSTR=0

    NBLBMX=0
    NUML=0

    DO 200 I=1,NUMOUTFILE
      IF(INSFILE(I).EQ.' ')THEN
        WRITE(AMESSAGE,10)
        10 FORMAT('Error in subroutine MIO_INI_INSTRUCT1: names have not ',  &
            'been installed for all instruction files.')
        IFAIL=1
        RETURN
      END IF
      IF(MODOUTFILE(I).EQ.' ')THEN
        WRITE(AMESSAGE,11)
        11 FORMAT('Error in subroutine MIO_INI_INSTRUCT1: names have not ', &
            'been installed for all model output files.')
        IFAIL=1
        RETURN
      END IF
      CALL UTL_ADDQUOTE(INSFILE(I),AFILE)
      IUNIT=UTL_NEXTUNIT()
      OPEN(UNIT=IUNIT,FILE=INSFILE(I),STATUS='OLD',IOSTAT=IERR)
      IF(IERR.NE.0)THEN
        WRITE(AMESSAGE,20) TRIM(AFILE)
        20 FORMAT('Cannot open instruction file ',a,'.')
        IFAIL=1
        RETURN
      END IF
      STDFILE = .FALSE.
      READ(IUNIT,'(A)',END=9400,ERR=9000) DLINE
      KLINE = 1
      CALL UTL_REMCHAR(DLINE,UTL_ACHAR(9))
      CALL UTL_CASETRANS(DLINE,'lo')
      IF(DLINE(1:3).ne.'jif')GO TO 9400
      MRK: DO J=4,LEN_TRIM(DLINE)
        IF (DLINE(J:J) .NE. ' ') THEN
          MRKDEL(I) = DLINE(J:J)
          EXIT MRK
        ENDIF
      ENDDO MRK
      IF(MRKDEL(I).EQ.' ') GO TO 9400
50    READ(IUNIT,'(A)',END=180,ERR=9000) DLINE
      IF (DLINE==' ') THEN
        WRITE(*,520) TRIM(INSFILE(I))
        IFAIL = 1
        RETURN
      ENDIF
      KLINE = KLINE+1
      LENDLINE = LEN_TRIM(DLINE)
      IF (KLINE==2) THEN
        CALL UTL_GETWORD(0,DLINE,LENDLINE,IERR,ISTART,ISTOP)
        IF (UTL_SAMENAME(DLINE(ISTART:ISTOP),'STANDARDFILE')) THEN
          STDFILE = .TRUE.
          KREAD = 0
          ICOL = ISTOP+1
          !   READ NSKIP, READCOLUMN, NREAD, "NAMES"
          CALL UTL_RWORD(0,2,.FALSE.,ICOL,DLINE,ISTART,ISTOP,NSKIP,RDUM)
          CALL UTL_RWORD(0,2,.FALSE.,ICOL,DLINE,ISTART,ISTOP,IREADCOL,RDUM)
          CALL UTL_RWORD(0,2,.FALSE.,ICOL,DLINE,ISTART,ISTOP,NREAD,RDUM)
          IF (NSKIP>0) THEN
            DLINE = ' '
            WRITE(DLINE,500)NSKIP ! WRITE A LINE-ADVANCE INSTRUCTION
          ELSE
            GOTO 50
          ENDIF
        ENDIF
      ELSEIF (STDFILE) THEN
        !   Read dependent name and compose an instruction line
        CALL UTL_GETWORD(0,DLINE,LENDLINE,IERR,ISTART,ISTOP)
        DEPNAM = DLINE(ISTART:ISTOP)
        KREAD = KREAD+1
        IF (IREADCOL==1) THEN
          DLINE = ' '
          WRITE(DLINE,510)'!',TRIM(DEPNAM),'!'
        ELSEIF (IREADCOL>1) THEN
          NCOLSKIP = IREADCOL-1
          DLINE = ' '
          WRITE(DLINE,510)('!dum! ',NFIELD=1,NCOLSKIP),'!',TRIM(DEPNAM),'!'
        ELSE
          AMESSAGE = 'Error: READCOLUMN for STANDARDFILE < 1'
          CALL UTL_STOP()
        ENDIF
      ENDIF
      !
      CALL UTL_REMCHAR(DLINE,UTL_ACHAR(9))
      IF(INDEX(DLINE,MRKDEL(I)).EQ.0) CALL UTL_COMPRESSLINE(DLINE)
      NBLC=LEN_TRIM(DLINE)
      IF(NBLC.EQ.0) GO TO 50
      IF(NBLC.GT.NBLBMX)NBLBMX=NBLC
      NINSTR=NINSTR+1
      DO 60 J=1,NBLC
        IF(DLINE(J:J).NE.' ') THEN
          IF((DLINE(J:J).EQ.'l').OR.(DLINE(J:J).EQ.'L')) NUML=NUML+1
          GO TO 100
        END IF
60    CONTINUE
100   LENCIS=LENCIS+NBLC
      GO TO 50
180   CLOSE(UNIT=IUNIT)

      IF (STDFILE) THEN
        IF (KREAD .NE. NREAD) THEN
          AMESSAGE =   &
          'Error: Number of names read does not equal NREAD for file "'   &
          //TRIM(INSFILE(I))//'"'
          CALL UTL_STOP()
        ENDIF
      ENDIF

200 CONTINUE
    NBLBMX=NBLBMX+1
    DO 300 I=1,NUMOUTFILE
!      LENCIS=LENCIS+3+LEN_TRIM(MODOUTFILE(I))  !erb: changed 2 to 3 to accomodate category flag
      LENCIS=LENCIS+3  !erb: changed 8/24/05 because modoutfile is not stored or needed
300 CONTINUE
    NINSTR=NINSTR+NUMOUTFILE
    RETURN
    !
    9000 WRITE(AMESSAGE,9010) TRIM(AFILE)
    9010 FORMAT('Unable to read instruction file ',A,'.')
    IFAIL=1
    RETURN
    !
    9400 WRITE(AMESSAGE,9410) TRIM(AFILE)
    9410 FORMAT('Header of "jif" expected on first line of instruction ',  &
        'file ',A,'.')
    IFAIL=1
    RETURN
    !
  END SUBROUTINE MIO_INI_INSTRUCT1
  !-----------------------------------------------------------------------------
  SUBROUTINE MIO_INI_INSTRUCT2(IFAIL,NCATS,MCVCAT)
    !
    !   Reread and store instruction sets
    USE GLOBAL_DATA, ONLY: AMESSAGE, LENDNAM
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                             INTENT(OUT) :: IFAIL
    INTEGER,                             INTENT(IN)  :: NCATS
    CHARACTER (LEN=6), DIMENSION(NCATS), INTENT(IN)  :: MCVCAT
    !
    !   Local variables
    INTEGER :: I, IERR, INS, ISUM, IUNIT, J, K, NBLC
    CHARACTER(LEN=1) :: CCAT
    INTEGER :: ICOL, IREADCOL, ISTART, ISTOP, KLINE, KREAD, LENDLINE,   &
               NCOLSKIP, NFIELD, NREAD, NSKIP
    LOGICAL :: STDFILE
    DOUBLE PRECISION :: RDUM
    CHARACTER(LEN=LENDNAM) :: DEPNAM
    !
    !   Format statements
    500 FORMAT('l',I10.10)
    510 FORMAT('l1 ',10000(A))
    520 FORMAT(1X,'ERROR: Blank line encountered in instruction file "',A,'"')
    !
    CISET=' '              ! CISET is an array
    IFAIL=0
    !
    ! -- The instruction set is now re-read and stored.

    INS=0
    ISUM=0
    DO 400 I=1,NUMOUTFILE
      call UTL_ADDQUOTE(insfile(i),afile)
      iunit=UTL_NEXTUNIT()
      open(unit=iunit,file=insfile(i),status='OLD',iostat=ierr)
      if(ierr.ne.0)then
        WRITE(amessage,20) trim(afile)
        20 format('Cannot open instruction file ',a,'.')
        ifail=1
        return
      end if
      !
      !   Find model-calculated value category for current
      !   model-output/instruction file pair.  Default is category 0.
      CCAT='0'
      FINDCAT: DO K=1,NCATS
        IF (MCVCAT(K)==CATEGORY(I)) THEN
          CALL UTL_NUM2CHAR(K,CCAT)
          GOTO 100
        ENDIF
      ENDDO FINDCAT
      IF (NCATS > 1) THEN
        WRITE(AMESSAGE,30)TRIM(MODOUTFILE(I)),CATEGORY(I)
 30     FORMAT('Error: Model-output file "',A,'" has invalid CATEGORY: ',A)
        IFAIL=1
        RETURN
      ELSE
        !   When NCATS=1, default CATEGORY to MCVAT(1) unless specified category
        !   is not supported
        IF (CATEGORY(I) .NE. '0' .AND. CATEGORY(I) .NE. MCVCAT(1)) THEN
          WRITE(AMESSAGE,30)TRIM(MODOUTFILE(I)),CATEGORY(I)
          IFAIL=1
          RETURN
        ENDIF
        CALL UTL_NUM2CHAR(1,CCAT)
      ENDIF
100   CONTINUE
      !
      READ(iunit,*,err=9000)
      STDFILE = .FALSE.
      KLINE = 1
      INS=INS+1
      dline(1:1)=UTL_ACHAR(2)
      dline(2:2)=ccat
      dline(3:3)=' '
      LCINS(INS)=ISUM+1
      NBLC=3
      DO 320 J=1,NBLC
        CISET(J+ISUM)=dline(J:J)
      320 continue
      ISUM=ISUM+NBLC
      350 READ(iunit,322,END=181,err=9000) dline
      IF (DLINE==' ') THEN
        WRITE(*,520) TRIM(INSFILE(I))
        IFAIL = 1
        RETURN
      ENDIF
      322 FORMAT(A)
      !
      !   Check for STANDARDFILE option
      KLINE = KLINE+1
      LENDLINE = LEN_TRIM(DLINE)
      IF (KLINE==2) THEN
        CALL UTL_GETWORD(0,DLINE,LENDLINE,IERR,ISTART,ISTOP)
        IF (UTL_SAMENAME(DLINE(ISTART:ISTOP),'STANDARDFILE')) THEN
          STDFILE = .TRUE.
          KREAD = 0
          ICOL = ISTOP+1
          !   READ NSKIP, READCOLUMN, NREAD, "NAMES"
          CALL UTL_RWORD(0,2,.FALSE.,ICOL,DLINE,ISTART,ISTOP,NSKIP,RDUM)
          CALL UTL_RWORD(0,2,.FALSE.,ICOL,DLINE,ISTART,ISTOP,IREADCOL,RDUM)
          CALL UTL_RWORD(0,2,.FALSE.,ICOL,DLINE,ISTART,ISTOP,NREAD,RDUM)
          IF (NSKIP>0) THEN
            DLINE = ' '
            WRITE(DLINE,500)NSKIP ! WRITE A LINE-ADVANCE INSTRUCTION
          ELSE
            GOTO 350
          ENDIF
        ENDIF
      ELSEIF (STDFILE) THEN
        !   Read dependent name and compose an instruction line
        CALL UTL_GETWORD(0,DLINE,LENDLINE,IERR,ISTART,ISTOP)
        DEPNAM = DLINE(ISTART:ISTOP)
        KREAD = KREAD+1
        IF (IREADCOL==1) THEN
          DLINE = ' '
          WRITE(DLINE,510)'!',TRIM(DEPNAM),'!'
        ELSEIF (IREADCOL>1) THEN
          NCOLSKIP = IREADCOL-1
          DLINE = ' '
          WRITE(DLINE,510)('!dum! ',NFIELD=1,NCOLSKIP),'!',TRIM(DEPNAM),'!'
        ELSE
          AMESSAGE = 'Error: READCOLUMN for STANDARDFILE < 1'
          CALL UTL_STOP()
        ENDIF
      ENDIF
      !
      call UTL_REMCHAR(dline,UTL_ACHAR(9))
      IF(INDEX(dline,MRKDEL(I)).EQ.0) CALL UTL_COMPRESSLINE(DLINE)
      NBLC=len_trim(dline)
      IF(NBLC.EQ.0) GO TO 350
      INS=INS+1
      LCINS(INS)=ISUM+1
      DO 370 J=1,NBLC
        CISET(J+ISUM)=dline(J:J)
      370 continue
      ISUM=ISUM+NBLC
      GO TO 350
      181 CLOSE(UNIT=iunit)
    400 CONTINUE
    return
    !
    9000 WRITE(amessage,9010) trim(afile)
    9010 format('Unable to read instruction file ',a,'.')
    ifail=1
    return
    !
  end subroutine MIO_INI_INSTRUCT2
  !-----------------------------------------------------------------------------
  SUBROUTINE MIO_INI_INSTRUCTALLOC(NDEP,IFAIL)
    !   Allocate arrays for storing extraction instructions
    USE GLOBAL_DATA, ONLY: AMESSAGE
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(IN)  :: NDEP
    INTEGER, INTENT(OUT) :: IFAIL
    !
    !   Local variables
    INTEGER :: IERR
    !
    IFAIL = 0
    MCALL = 0
    ALLOCATE(CISET(LENCIS),LADV(NUML),LCINS(NINSTR),INS1(NDEP),  &
             INS2(NDEP),INS3(NDEP),STAT=IERR)
    IF(IERR.NE.0)THEN
      WRITE(AMESSAGE,310)
310   FORMAT('Cannot allocate sufficient memory to store instruction set.')
      IFAIL=1
      RETURN
    END IF
    RETURN
  END SUBROUTINE MIO_INI_INSTRUCTALLOC
  !-----------------------------------------------------------------------------
  SUBROUTINE MIO_INI_INSTRUCT_RUNNER(IFAIL,LCIS,NINSTRUCT,NUMLADV,NVEXT,   &
                                    CINSTSET,LOCINS)
    !   Call MIO_INI_INSTRUCTALLOC to allocate module arrays for storing
    !   extraction instructions, and populate module arrays CISET and LCINS
    !   from arrays in argument list.
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(OUT) :: IFAIL
    INTEGER, INTENT(IN)  :: LCIS
    INTEGER, INTENT(IN)  :: NINSTRUCT
    INTEGER, INTENT(IN)  :: NUMLADV
    INTEGER, INTENT(IN)  :: NVEXT
    CHARACTER(LEN=1),  DIMENSION(LCIS),    INTENT(IN) :: CINSTSET ! holds compressed instruction set
    INTEGER,         DIMENSION(NINSTRUCT), INTENT(IN) :: LOCINS   ! pointer to instructions
    !
    IFAIL = 0
    LENCIS = LCIS
    NINSTR = NINSTRUCT
    NUML = NUMLADV
    !
    CALL MIO_INI_INSTRUCTALLOC(NVEXT,IFAIL)
    CISET = CINSTSET
    LCINS = LOCINS
    !
    RETURN
  END SUBROUTINE MIO_INI_INSTRUCT_RUNNER
  !-----------------------------------------------------------------------------
  SUBROUTINE MIO_INI_OUTPUTFILES(INUNIT,IOUT)
    !   Allocate and initialize arrays required for model output
    USE GLOBAL_DATA, ONLY: AMESSAGE, ERRSUB, IVERB
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(IN) :: INUNIT
    INTEGER, INTENT(IN) :: IOUT
    !
    !   Local variables
    INTEGER :: I, IERR, IFAIL, MORE
    TYPE (LLIST), POINTER :: TAIL
    !
    NULLIFY(LLPTRMODOUT)
    NULLIFY(TAIL)
    IFAIL = 0
    !   Make call to UTLREADBLOCK to read input blocks containing names
    !   of model-output and corresponding instruction files
    NUMOUTFILE = 0
    CALL UTL_READBLOCK(NMODOUTCOLS,'MODEL_OUTPUT_FILES',MODOUTCOL,INUNIT,  &
        IOUT,'MODOUTFILE',.TRUE.,LLPTRMODOUT,TAIL,NUMOUTFILE)
    !
    IF (NUMOUTFILE.LE.0) THEN
      WRITE(AMESSAGE,200)
200   FORMAT('Error: No model-output files were specified.')
      IFAIL=1
      CALL UTL_STOP(' ')
    END IF
    !
    !   Allocate arrays for storing model-output file names
    ALLOCATE (MODOUTFILE(NUMOUTFILE),INSFILE(NUMOUTFILE),   &
              MRKDEL(NUMOUTFILE),CATEGORY(NUMOUTFILE),STAT=IERR)
    !
    IF(IERR.NE.0)THEN
      WRITE(AMESSAGE,220) TRIM(ERRSUB)
220   FORMAT(A,' cannot allocate sufficient memory to store',   &
             ' model-interface filenames.')
      IFAIL=1
      CALL UTL_STOP(' ')
    END IF
    !
    MODOUTFILE = ' '
    INSFILE = ' '
    MRKDEL = ' '
    CATEGORY = '0'
    !
    !   Populate arrays of file names by traversing the linked lists
    CALL UTL_FILTERLIST(LLPTRMODOUT,IOUT,'MODOUTFILE',NUMOUTFILE,IFAIL,   &
                        MODOUTFILE,MORE)
    IF (IFAIL.NE.0) CALL UTL_STOP(' ')
    !
    CALL UTL_FILTERLIST(LLPTRMODOUT,IOUT,'INSTRUCTIONFILE',NUMOUTFILE,IFAIL,   &
                        INSFILE,MORE)
    IF (IFAIL.NE.0) CALL UTL_STOP(' ')
    !
    CALL UTL_FILTERLIST(LLPTRMODOUT,IOUT,'CATEGORY',NUMOUTFILE,IFAIL,   &
                        CATEGORY,MORE)
    IF (IFAIL.NE.0) CALL UTL_STOP(' ')
    DO I=1,NUMOUTFILE
      CALL UTL_CASETRANS(CATEGORY(I),'hi')
    ENDDO
    !
    !   Echo input
    IF (IVERB.GE.3) THEN
      WRITE(IOUT,250)
      250 FORMAT(//,1X,'MODEL-OUTPUT AND CORRESPONDING INSTRUCTION FILES:',/)
      WRITE(IOUT,260) (TRIM(MODOUTFILE(I)),TRIM(INSFILE(I)),CATEGORY(I),I=1,NUMOUTFILE)
      260 FORMAT(1X,'Model-output file: ',A,3X,'Instruction file: ',A,3X,   &
                 'Category: ',A)
    ENDIF
    !
    !   Deallocate linked lists

    RETURN
  END SUBROUTINE MIO_INI_OUTPUTFILES
  !-----------------------------------------------------------------------------
  SUBROUTINE MIO_INI_OUTPUTFILES_RUNNER(IFAIL,NMOFILE,CATGOR,INSTRUCTFILE,   &
                                       MOFILE,MRKDL)
    !   Allocate and initialize arrays required for model output
    USE GLOBAL_DATA, ONLY: AMESSAGE, ERRSUB, IVERB, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                           INTENT(OUT) :: IFAIL
    INTEGER,                                           INTENT(IN)  :: NMOFILE ! Number of model-output files
    CHARACTER(LEN=6),              DIMENSION(NMOFILE), INTENT(IN)  :: CATGOR  ! Model-calc'd val. cat. for each model-output file
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NMOFILE), INTENT(IN)  :: INSTRUCTFILE ! Instruction file names
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NMOFILE), INTENT(IN)  :: MOFILE       ! Model output file names
    CHARACTER(LEN=1),              DIMENSION(NMOFILE), INTENT(IN)  :: MRKDL        ! Marker delimiters
    !
    !   Local variables
    INTEGER :: IERR
    !
    IFAIL = 0
    !   Make call to UTLREADBLOCK to read input blocks containing names
    !   of model-output and corresponding instruction files
    NUMOUTFILE = NMOFILE
    !
    IF (NUMOUTFILE.LE.0) THEN
      WRITE(AMESSAGE,200)
200   FORMAT('Error: No model-output files were specified.')
      IFAIL=1
      RETURN
    END IF
    !
    !   Allocate arrays for storing model-output file names
    ALLOCATE (MODOUTFILE(NUMOUTFILE),INSFILE(NUMOUTFILE),   &
              MRKDEL(NUMOUTFILE),CATEGORY(NUMOUTFILE),STAT=IERR)
    !
    IF(IERR.NE.0)THEN
      WRITE(AMESSAGE,220) TRIM(ERRSUB)
220   FORMAT(A,' cannot allocate sufficient memory to store',   &
             ' model-interface filenames.')
      IFAIL=1
      RETURN
    END IF
    !
    CATEGORY = CATGOR
    INSFILE = INSTRUCTFILE
    MODOUTFILE = MOFILE
    MRKDEL = MRKDL
    !
    RETURN
  END SUBROUTINE MIO_INI_OUTPUTFILES_RUNNER
  !-----------------------------------------------------------------------------
  SUBROUTINE MIO_INI_TEMPLATE(IFAIL,NPT,APAR,NW)

    ! -- Subroutine MIO_INI_TEMPLATE does rudimentary checking of template files.
    !    However its main role is to find the smallest character width to which
    !    each parameter will be written.

    USE GLOBAL_DATA, ONLY: AMESSAGE, ERRSUB
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                           INTENT(OUT) :: IFAIL ! indicates error condition
    INTEGER,                           INTENT(IN)  :: NPT   ! number of parameters
    CHARACTER (LEN=*), DIMENSION(NPT), INTENT(IN)  :: APAR  ! parameter names
    INTEGER, DIMENSION(NPT),           INTENT(OUT) :: NW    ! Minimum word length of a parameter
    !
    !   Local variables
    INTEGER            :: I,IERR,ILINE,IPAR,IUNIT,J1,J2,JFAIL,NBLC,NNW
    CHARACTER (LEN=10) :: ALINE
    CHARACTER (LEN=12) :: TPAR

    errsub='Error in subroutine MIO_INI_TEMPLATE:'

    ifail=0

    if(npt.le.0)then
      WRITE(amessage,10) trim(errsub)
      10 format(a,' value supplied for NPT is zero or negative.')
      ifail=1
      return
    end if

    IPAR=1
    DO 400 I=1,NPT
      NW(I)=1000
400 continue
    DO 500 I=1,NUMINFILE
      if(tempfile(i).eq.' ')then
        WRITE(amessage,401)
401     format('Error in subroutine MIO_INI_TEMPLATE: names have not ',  &
            'been installed for all template files.')
        ifail=1
        return
      end if
      if(modinfile(i).eq.' ')then
        WRITE(amessage,402)
402     format('Error in subroutine MIO_INI_TEMPLATE: names have not ',  &
            'been installed for all model input files.')
        ifail=1
        return
      end if
      call UTL_ADDQUOTE(tempfile(i),afile)
      iunit=UTL_NEXTUNIT()
      open(unit=iunit,file=tempfile(i),status='OLD',iostat=ierr)
      if(ierr.ne.0)then
        WRITE(amessage,410) trim(afile)
410     format('Cannot open template file ',a,'.')
        ifail=1
        return
      end if
      READ(iunit,'(A)',err=9000,END=9200) dline
      call UTL_CASETRANS(dline(1:3),'lo')
      IF(dline(1:3).NE.'jtf')GO TO 9200
      PARDEL(I)=dline(5:5)
      IF(PARDEL(I).EQ.' ') GO TO 9200
      ILINE=1
520   ILINE=ILINE+1
      READ(iunit,'(A)',err=9000,END=680) dline
      NBLC=len_trim(dline)
      J2=0
550   IF(J2.GE.NBLC) GO TO 520
      J1=INDEX(dline(J2+1:NBLC),PARDEL(I))
      IF(J1.EQ.0) GO TO 520
      J1=J1+J2
      J2=INDEX(dline(J1+1:NBLC),PARDEL(I))
      IF(J2.EQ.0)then
        call UTL_NUM2CHAR(iline,aline)
        WRITE(amessage,555) trim(aline),trim(afile)
555     format('Unbalanced parameter delimiters at line ',a,' of template file ',a,'.')
        ifail=1
        return
      end if
      J2=J2+J1
      CALL UTL_GETWORDLEFT(DLINE,jfail,J1,J2,TPAR)
      if(jfail.eq.1)then
        call UTL_NUM2CHAR(iline,aline)
        WRITE(amessage,556) trim(aline),trim(afile)
556     format('Parameter space less than three characters wide at line ',a,  &
            ' of file ',a,'.')
        ifail=1
        return
      else if (jfail.eq.2)then
        call UTL_NUM2CHAR(iline,aline)
        WRITE(amessage,557) trim(aline),trim(afile)
557     format('Blank parameter space at line ',a,' of file ',a,'.')
        ifail=1
        return
      end if
      CALL UTL_WHICH1(jfail,NPT,IPAR,APAR,TPAR)
      if(jfail.ne.0)then
        call UTL_NUM2CHAR(iline,aline)
        WRITE(amessage,558) trim(tpar),trim(aline),trim(afile)
558     format('Parameter "',a,'" cited on line ',a,' of template file ',a,   &
            ' has not been supplied with a value.')
        ifail=1
        return
      end if
      NNW=J2-J1+1
      IF(NNW.LT.NW(IPAR)) NW(IPAR)=NNW
      GO TO 550
680   CLOSE(UNIT=iunit)
500 CONTINUE
    DO 800 I=1,NPT
!write
!note this loop needs to check whether the parameter might appear in a
!derived parameter equation and allow for that
!until that is ready I will comment this out
!      IF(NW(I).EQ.1000)then
!        WRITE(amessage,690) trim(apar(i))
!690     format('Parameter "',a,'" is not cited on any template file.')
!        ifail=1
!        return
 !     end if
800 CONTINUE

    RETURN

9000 WRITE(amessage,9010) trim(afile)
9010 format('Unable to read template file ',a,'.')
    ifail=1
    return
9200 WRITE(amessage,9210) trim(afile)
9210 format('"jtf" header expected on first line of template file ',a,'.')
    ifail=1

    RETURN
  END subroutine MIO_INI_TEMPLATE



! Note that there seems to be a bug in lf95 in that if there is an error deleting
! the file there is a compiler error rather than ierr being given a nonzero value.

  !-----------------------------------------------------------------------------
  SUBROUTINE MIO_ADA_MODINFILE_CHANGE(IFAIL,IFILE,INFILENAME)
    !   Store a filename in a selected element of the MODINFILE array
    USE GLOBAL_DATA, ONLY: AMESSAGE
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,          INTENT(OUT) :: IFAIL
    INTEGER,          INTENT(IN)  :: IFILE
    CHARACTER(LEN=*), INTENT(IN)  :: INFILENAME
    !
    !   Local variables
    INTEGER :: LENARG, LENARRAY
    !
    !   Format statements
    900 FORMAT('ERROR: File name too long to store in MODINFILE',   &
          ' array (MIO_ADA_MODINFILE_CHANGE)')
    905 FORMAT('ERROR: Array MODINFILE has not been allocated',   &
          ' (MIO_ADA_MODINFILE_CHANGE)')
    910 FORMAT('ERROR: Array index outside MODINFILE array',   &
          ' bounds (MIO_ADA_MODINFILE_CHANGE)')
    !
    IFAIL = 0
    LENARG = LEN_TRIM(INFILENAME)
    LENARRAY = LEN(MODINFILE)
    IF (LENARG>LENARRAY) THEN
      IFAIL = 1
      WRITE(AMESSAGE,900)
      RETURN
    ENDIF
    IF (.NOT. ALLOCATED(MODINFILE)) THEN
      IFAIL = 1
      WRITE(AMESSAGE,905)
      RETURN
    ENDIF
    IF (IFILE<1 .OR. IFILE>NUMINFILE) THEN
      IFAIL = 1
      WRITE(AMESSAGE,910)
      RETURN
    ENDIF
    MODINFILE(IFILE) = INFILENAME
    RETURN
  END SUBROUTINE MIO_ADA_MODINFILE_CHANGE
  !-----------------------------------------------------------------------------
  SUBROUTINE MIO_ADA_WRITEFILES(IFAIL,NPT,APAR,NOPNT,NW,PRECPR,PVAL)
    ! -- Subroutine MIO_ADA_WRITEFILES writes model input files based on
    !    specified parameter values and a set of model input template files.
    !
    USE GLOBAL_DATA, ONLY: AMESSAGE, ERRSUB
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                           INTENT(OUT)   :: IFAIL  ! error condition indicator
    INTEGER,                           INTENT(IN)    :: NPT    ! number of parameters
    CHARACTER (LEN=*), DIMENSION(NPT), INTENT(IN)    :: APAR   ! parameter names
    INTEGER,                           INTENT(IN)    :: NOPNT  ! Decimal point protocol
    INTEGER, DIMENSION(NPT),           INTENT(IN)    :: NW     ! Minimum word length of a parameter
    INTEGER,                           INTENT(IN)    :: PRECPR ! Precision protocol
    DOUBLE PRECISION, DIMENSION(NPT),  INTENT(INOUT) :: PVAL   ! parameter values
    !
    !   Local variables
    INTEGER            :: IERR,IPAR,IPP,IFILE,ILINE,IUNIT,IUNIT1,J,J1,J2,   &
                          JFAIL,LC
    DOUBLE PRECISION   :: TVAL
    CHARACTER (LEN=12) :: TPAR

    errsub='Error writing parameters to model input file(s):'
    ifail=0

    ! -- EACH OF THE PARAMETER WORDS IS FILLED

    IPAR=1
    DO 100 IPP=1,NPT
      CALL UTL_WRTSIG(jfail,PVAL(IPP),PWORD(IPP),NW(IPP),PRECPR,TVAL,NOPNT)
      if(jfail.lt.0)then
        WRITE(amessage,10) trim(apar(ipp))
10      format('Internal error condition has arisen while attempting to write ', &
            'current value of parameter "',a,'" to model output file.')
        go to 9900
      else if (jfail.eq.1)then
        WRITE(amessage,11) trim(errsub),trim (apar(ipp))
11      format(a,' exponent of parameter "',a,'" is too large or too small for ', &
            'single precision protocol.')
        go to 9900
      else if (jfail.eq.2)then
        WRITE(amessage,12) trim(errsub),trim (apar(ipp))
12      format(a,' exponent of parameter "',a,'" is too large or too small for ', &
            'double precision protocol.')
        go to 9900
      else if (jfail.eq.3)then
        WRITE(amessage,13) trim(errsub),trim (apar(ipp))
13      format(a,' field width of parameter "',a,'" on at least one template file ', &
            'is too small to represent current parameter value. The number is too large ', &
            'to fit, or too small to be represented with any precision.')
        go to 9900
      end if
      PVAL(IPP)=TVAL
100 CONTINUE

    ! -- NEXT THE SUBSTITUTIONS IN THE TEMPLATE FILES ARE MADE

    DO 500 IFILE=1,NUMINFILE
      call UTL_ADDQUOTE(tempfile(ifile),afile)
      iunit=UTL_NEXTUNIT()
      open(unit=iunit,file=tempfile(ifile),status='OLD',iostat=ierr)
      if(ierr.ne.0)then
        WRITE(amessage,110) trim(errsub),trim(afile)
110     format(a,' cannot open template file ',a,'.')
        go to 9900
      end if
      iunit1=UTL_NEXTUNIT()
      open(unit=iunit1,file=modinfile(ifile),status='REPLACE',iostat=ierr)
      if(ierr.ne.0)then
        call UTL_ADDQUOTE(modinfile(ifile),afile)
        WRITE(amessage,115) trim(errsub),trim(afile)
115     format(a,' cannot open model input file ',a,' to write updated parameter ',  &
            'values prior to running model.')
        go to 9900
      end if
      READ(iunit,*,err=9000)
      ILINE=1
120   ILINE=ILINE+1
      READ(iunit,22,END=400,err=9000) dline
22    FORMAT(A)
      LC=len_trim(dline)
      J2=0
150   IF(J2.GE.LC) GO TO 300
      J1=INDEX(dline(J2+1:LC),pardel(IFILE))
      IF(J1.EQ.0) GO TO 300
      J1=J1+J2
      J2=INDEX(dline(J1+1:LC),pardel(IFILE))
      J2=J2+J1
      CALL UTL_GETWORDLEFT(DLINE,jfail,J1,J2,TPAR)
      CALL UTL_WHICH1(jfail,NPT,IPAR,APAR,TPAR)
      !       The following works when space bigger than pword(:nblnk(pword))
      !       dline(j1:j2)=pword(ipar)(:nblnk(pword(ipar)))
      DO 160 J=J1,J2
        dline(J:J)=' '
160   continue
      J=len_trim(PWORD(IPAR))
      dline(J2-J+1:J2)=PWORD(IPAR)(1:J)
      GO TO 150
300   WRITE(iunit1,22,ERR=320) trim(dline)
      GO TO 120
320   call UTL_ADDQUOTE(modinfile(ifile),afile)
      WRITE(amessage,321) trim(errsub),trim(afile)
321   format(a,' cannot write to model input file ',a,'.')
      go to 9900
400   CLOSE(UNIT=iunit)
      CLOSE(UNIT=iunit1,iostat=ierr)
      if(ierr.ne.0)then
        call UTL_ADDQUOTE(modinfile(ifile),afile)
        WRITE(amessage,321) trim(errsub),trim(afile)
        go to 9900
      end if
500 CONTINUE
    RETURN

9000 WRITE(amessage,9010) trim(afile)
9010 format('Unable to read template file ',a,'.')
    go to 9900
9900 ifail=1
    return

  END subroutine MIO_ADA_WRITEFILES
  !-----------------------------------------------------------------------------
  SUBROUTINE MIO_EXT(IFAIL,IOUT,NCATS,NVEXT,EXTNAM,MCVUSE,EXTVAL,   &
                     INSTRUCTION)
    !
    ! -- Subroutine MIO_EXT reads model output files using an instruction
    !    set.
    !
    !! Important note: if an error condition occurs and the INSTRUCTION variable
    !! is not blank, then this variable contains the offending instruction. This
    !! should be reproduced with the error message.
    !
    USE GLOBAL_DATA, ONLY: AMESSAGE, ERRSUB, IVERB, LENDNAM, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                              INTENT(OUT) :: IFAIL  ! error indicator
    INTEGER,                              INTENT(IN)  :: IOUT   ! output unit number
    INTEGER,                              INTENT(IN)  :: NCATS  ! Number of categories of model-calculated values
    INTEGER,                              INTENT(IN)  :: NVEXT  ! number of values to extract
    CHARACTER (LEN=*), DIMENSION(NVEXT),  INTENT(IN)  :: EXTNAM ! extracted-value names
    LOGICAL,           DIMENSION(NCATS),  INTENT(IN)  :: MCVUSE ! Do extractions for this category?
    DOUBLE PRECISION,  DIMENSION(NVEXT),  INTENT(OUT) :: EXTVAL ! extracted values
    CHARACTER (LEN=*),                    INTENT(OUT) :: INSTRUCTION ! instruction of error
    !
    !   Local variables
    INTEGER :: IFILE,IL,JVEXT,CIL,IVEXT,BEGINS,INS,NBLB,I,INSNUM,    &
               NBLC,DUMFLG,ALMARK,IUNIT,NOL,JFAIL,INSFLE, &
               MRKTYP,IERR,J2,J1,MCVFLAG,N1,N2,N3,NUM1,NUM2,J
    DOUBLE PRECISION  :: RTEMP
    CHARACTER (LEN=1) :: AA,MKRDEL
    CHARACTER (LEN=LENDNAM)  :: EXTNAM1
    CHARACTER (LEN=15)  :: FMT
    CHARACTER (LEN=10)  :: ANUM
    CHARACTER (LEN=MAX_STRING_LEN) :: FLENME
    INTEGER :: IFAIL1, LENINST, CISETLOC, KTRY
    LOGICAL :: PROCFILE
    !
    IF (IVERB>3 .AND. IOUT>0) THEN
      WRITE(IOUT,*)' '
      WRITE(IOUT,*)' Extractions are printed because Verbose is > 3'
      WRITE(IOUT,*)' "Extracted Item Name"     "Extracted Value"'
    ENDIF
    !
    ERRSUB='Error reading model output file(s):'
    IFAIL=0
    IFAIL1=0
    PROCFILE=.FALSE.
    !
    MCALL=MCALL+1
    IFILE=0
    IL=0
    JVEXT=0
    MKRDEL=MRKDEL(1)
    CIL=0
    IVEXT=1
    BEGINS=0
    LENINST=LEN(INSTRUCTION)
    !
    INS=1
10  IF(INS.LT.NINSTR)THEN
      NBLB=LCINS(INS+1)-LCINS(INS)
    ELSE
      NBLB=LENCIS-LCINS(INS)+1
    END IF
    IF (NBLB>LENINST) THEN
      WRITE(*,2000)(CISET(LCINS(INS)+I-1),I=I,LENINST)
      2000 FORMAT(/,1X,'*** Probable programming error.',/,   &
           3X,'In MIO_EXT, NBLB exceeds available instruction length.',/,   &
           3X,'First part of instruction is:',/,200A1)
      CALL UTL_STOP()
    ENDIF
    instruction=' '
    CISETLOC = LCINS(INS)
    DO 20 I=1,NBLB
      instruction(I:I)=CISET(CISETLOC+I-1)
20  continue
25  N2=0
    INSNUM=0
50  CALL MIO_EXT_GETINSTRUCT(MKRDEL,NBLB,instruction,N2,jfail,N1)
    if(jfail.ne.0)then
      WRITE(amessage,49) trim(errsub)
49    format(a,' missing marker delimiter in user-supplied instruction.')
      go to 9995
    end if
51  IF(N1.EQ.0) GO TO 1000
    INSNUM=INSNUM+1
    IF(INSNUM.EQ.1)THEN
      IF(instruction(N1:N1).NE.'&') THEN
        MRKTYP=0
        ALMARK=1
        BEGINS=0
      ELSE
        IF(INS.EQ.INSFLE+1) then
          WRITE(amessage,52) trim(errsub)
52        format(a,' first instruction line in instruction file cannot start ', &
              'with continuation character.')
          go to 9995
        end if
        IF(BEGINS.EQ.1)THEN
          INS=INS-1
          GO TO 10
        END IF
      END IF
    END IF
    IF(ICHAR(instruction(N1:N1)).EQ.2)THEN
      IF(IFILE.NE.0 .AND. PROCFILE) CLOSE(UNIT=iunit)
      CALL UTL_CHAR2NUM(IFAIL1,INSTRUCTION(N1+1:N1+1),MCVFLAG)
      IF (IFAIL1>0) THEN
        IF (IOUT>0) THEN
          WRITE(IOUT,56)
        ELSE
          AMESSAGE = ' Programmer error: In MIO_EXT, cannot interpret'//   &
                     ' MCVFLAG in CIS array'
        ENDIF
        CALL UTL_STOP(' ')
      ENDIF
56    FORMAT(' Programmer error: In MIO_EXT, cannot interpret',   &
             ' MCVFLAG in CIS array')
      IF (MCVFLAG<1 .OR. MCVFLAG>NCATS) THEN
        IF (IOUT>0) THEN
          WRITE(IOUT,58)
        ELSE
          AMESSAGE = ' Programmer error: In MIO_EXT, MCVFLAG invalid'
        ENDIF
        CALL UTL_STOP(' ')
      ENDIF
58    FORMAT(' Programmer error: In MIO_EXT, MCVFLAG invalid')
      IFILE=IFILE+1
      PROCFILE=MCVUSE(MCVFLAG)
      IF (.NOT. PROCFILE) GOTO 1000
      FLENME=MODOUTFILE(IFILE)
      iunit=UTL_NEXTUNIT()
      KTRY = 0
60    CONTINUE      
      open(unit=iunit,file=flenme,status='OLD',iostat=ierr)
      call UTL_ADDQUOTE(flenme,afile)
      if(ierr.ne.0)then
        IF (KTRY<200) THEN
          KTRY = KTRY + 1
          CALL UTL_SLEEP(0.001D0)
          GOTO 60
        ENDIF
        WRITE(amessage,71) trim(errsub),trim(afile)
71      format(a,' cannot open model output file ',a,'.')
        instruction=' '
        go to 9995
      end if
      CIL=0
      MKRDEL=MRKDEL(IFILE)
      INSFLE=INS
      GO TO 1000
    ELSE IF((instruction(N1:N1).EQ.'l').OR.(instruction(N1:N1).EQ.'L'))THEN
      IF (.NOT. PROCFILE) GOTO 1000
      ALMARK=0
      IL=IL+1
      IF(MCALL.EQ.1)THEN
        if(n2.le.n1) go to 9050     ! put in pest
        WRITE(FMT,150) N2-N1
150     FORMAT('(I',I4,')')
        READ(instruction(N1+1:N2),FMT,ERR=9050) NOL
        LADV(IL)=NOL
      ELSE
        NOL=LADV(IL)
      END IF
      IF(NOL.GT.1) THEN
        DO 160 I=1,NOL-1
          READ(iunit,*,END=9100,err=9850)
          CIL=CIL+1
160     continue
      END IF
      READ(iunit,22,END=9100,err=9850) dline
22    FORMAT(A)
      IF(INDEX(dline,CHAR(9)).NE.0) CALL UTL_TABREP(DLINE)
      CIL=CIL+1
      NBLC=len_trim(dline)
      MRKTYP=1
      J1=0
    ELSE IF(instruction(N1:N1).EQ.MKRDEL)THEN
      IF (.NOT. PROCFILE) GOTO 1000
      IF(MRKTYP.EQ.0)THEN
200     READ(iunit,22,END=9100,err=9850) dline
        IF(INDEX(dline,CHAR(9)).NE.0) CALL UTL_TABREP(DLINE)
        CIL=CIL+1
        J1=INDEX(dline,instruction(N1+1:N2-1))
        IF(J1.EQ.0) GO TO 200
        NBLC=len_trim(dline)
        J1=J1+N2-N1-2
        MRKTYP=1
      ELSE
        IF(J1.GE.NBLC) THEN
          IF(ALMARK.EQ.1) THEN
            BEGINS=1
            GO TO 25
          END IF
          GO TO 9200
        END IF
        J2=INDEX(dline(J1+1:NBLC),instruction(N1+1:N2-1))
        IF(J2.EQ.0) THEN
          IF(ALMARK.EQ.1) THEN
            BEGINS=1
            GO TO 25
          END IF
          GO TO 9200
        END IF
        J1=J1+J2
        J1=J1+N2-N1-2
      END IF
    ELSE IF(instruction(N1:N1).EQ.'&')THEN
      IF (.NOT. PROCFILE) GOTO 1000
      IF(INSNUM.NE.1) then
        WRITE(amessage,201) trim(errsub)
201     format(a,' if present, continuation character must be first instruction on ', &
            'an instruction line.')
        go to 9995
      end if
    ELSE IF((instruction(N1:N1).EQ.'w').OR.(instruction(N1:N1).EQ.'W'))THEN
      IF (.NOT. PROCFILE) GOTO 1000
      ALMARK=0
      IF(J1.GE.NBLC) GO TO 9400
      J2=INDEX(dline(J1+1:NBLC),' ')
      IF(J2.EQ.0) GO TO 9400
      J1=J1+J2
      DO 210 I=J1,NBLC
        IF(dline(I:I).NE.' ') GO TO 220
210   CONTINUE
      I=NBLC+1
220   J1=I-1
    ELSE IF((instruction(N1:N1).EQ.'t').OR.(instruction(N1:N1).EQ.'T'))THEN
      IF (.NOT. PROCFILE) GOTO 1000
      ALMARK=0
      if(n2.le.n1) go to 9000       ! put in PEST
      WRITE(FMT,150) N2-N1
      READ(instruction(N1+1:N2),FMT,ERR=9000) J2
      IF(J2.LT.J1) then
        call UTL_NUM2CHAR(cil,anum)
        WRITE(amessage,221) trim(errsub),trim(anum),trim(afile)
221     format(a,' backwards move to tab position not allowed on line ',a,  &
            ' of model output file ',a,'.')
        go to 9995
      end if
      J1=J2
      IF(J1.GT.NBLC) then
        call UTL_NUM2CHAR(cil,anum)
        WRITE(amessage,222) trim(errsub),trim(anum),trim(afile)
222     format(a,' tab position beyond end of line at line ',a,' of ', &
            'model output file ',a,'.')
        go to 9995
      end if
    ELSE IF((instruction(N1:N1).EQ.'[').OR.(instruction(N1:N1).EQ.'('))THEN
      IF (.NOT. PROCFILE) GOTO 1000
      ALMARK=0
      AA=instruction(N1:N1)
      JVEXT=JVEXT+1
      IF(MCALL.EQ.1)THEN
        IF(AA.EQ.'[')THEN
          N3=INDEX(instruction(N1:N2),']')
        ELSE
          N3=INDEX(instruction(N1:N2),')')
        END IF
        if(n3.eq.0)then
          call UTL_NUM2CHAR(cil,anum)
          WRITE(amessage,226) trim(errsub)
226       format(a,' missing "]" or ")" character in instruction.')
          go to 9995
        end if
        N3=N3+N1-1
        EXTNAM1=instruction(N1+1:N3-1)
        CALL UTL_WHICH1(jfail,NVEXT,IVEXT,EXTNAM,EXTNAM1)
        IF(jfail.NE.0) GO TO 9700
        CALL MIO_EXT_GETCOLS(instruction,N2,N3,FMT,jfail,NUM1,NUM2)
        IF(jfail.NE.0) then
          WRITE(amessage,223) trim(errsub)
223       format(a,'cannot interpret user-supplied instruction for reading model ', &
              'output file.')
          go to 9995
        end if
        INS1(JVEXT)=NUM1
        INS2(JVEXT)=NUM2
        INS3(JVEXT)=IVEXT
      ELSE
        NUM1=INS1(JVEXT)
        NUM2=INS2(JVEXT)
        IVEXT=INS3(JVEXT)
      END IF
      IF(AA.EQ.'(') THEN
        CALL MIO_EXT_SEMI(NBLC,NUM1,NUM2,jfail)
        if(jfail.ne.0)then
          call UTL_NUM2CHAR(cil,anum)
          WRITE(amessage,224) trim (errsub),trim(EXTNAM(IVEXT)),trim(anum),   &
          trim(afile)
224       format(a,' cannot find extract-value "',a,'" on line ',a,     &
              ' of model output file ',a,'.')
          go to 9995
        end if
      ELSE
        if(num1.gt.nblc)then
          call UTL_NUM2CHAR(cil,anum)
          WRITE(amessage,224) trim(errsub),trim(EXTNAM(IVEXT)),trim(anum),trim(afile)
          go to 9995
        end if
        IF(NUM2.GT.NBLC) NUM2=NBLC
        IF(dline(NUM1:NUM2).EQ.' ')then
          call UTL_NUM2CHAR(cil,anum)
          WRITE(amessage,224) trim(errsub),trim(EXTNAM(IVEXT)),trim(anum),trim(afile)
          go to 9995
        end if
      end if
      WRITE(FMT,250) NUM2-NUM1+1
250   FORMAT('(F',I4,'.0)')
      READ(dline(NUM1:NUM2),FMT,ERR=260) EXTVAL(IVEXT)
      IF (IVERB>3 .AND. IOUT>0)   &
          WRITE(IOUT,255)TRIM(EXTNAM(IVEXT)), EXTVAL(IVEXT)
255 FORMAT(2X,A,T30,G12.5)
      J1=NUM2
      GO TO 50
260   continue
      call UTL_NUM2CHAR(cil,anum)
      WRITE(amessage,261) trim(errsub),trim(EXTNAM(IVEXT)),trim(anum),trim(afile)
261   format(a,' cannot read extract-value "',a,'" from line ',a,     &
          ' of model output file ',a,'.')
      go to 9995
    ELSE IF(instruction(N1:N1).EQ.'!') THEN
      IF (.NOT. PROCFILE) GOTO 1000
      ALMARK=0
      CALL UTL_CASETRANS(instruction(N1+1:N2-1),'lo')
      IF((N2-N1.NE.4).OR.(instruction(N1+1:N2-1).NE.'dum'))THEN
        JVEXT=JVEXT+1
        IF(MCALL.EQ.1) THEN
          EXTNAM1=instruction(N1+1:N2-1)
          CALL UTL_WHICH1(jfail,NVEXT,IVEXT,EXTNAM,EXTNAM1)
          IF(jfail.NE.0) GO TO 9700
          INS3(JVEXT)=IVEXT
        ELSE
          IVEXT=INS3(JVEXT)
        END IF
        DUMFLG=0
      ELSE
        DUMFLG=1
      END IF
      CALL UTL_GETWORD(J1,DLINE,NBLC,jfail,NUM1,NUM2)
      IF(jfail.NE.0) THEN
        IF(DUMFLG.EQ.0) THEN
          call UTL_NUM2CHAR(cil,anum)
          WRITE(amessage,224) trim(errsub),trim(EXTNAM(IVEXT)),trim(anum),trim(afile)
          go to 9995
        ELSE
          call UTL_NUM2CHAR(cil,anum)
          WRITE(amessage,224) trim(errsub),'dum',trim(anum),trim(afile)
          go to 9995
        END IF
      END IF
      IF(DUMFLG.EQ.0) THEN
        WRITE(FMT,250) NUM2-NUM1+1
        READ(dline(NUM1:NUM2),FMT,ERR=270) RTEMP
        EXTVAL(IVEXT)=RTEMP
      ENDIF
      IF (IVERB>3 .AND. IOUT>0 .AND. DUMFLG==0)   &  ! DUMFLG condition added 11/18/2008 ERB
          WRITE(IOUT,255)TRIM(EXTNAM(IVEXT)),EXTVAL(IVEXT)
      J1=NUM2
      GO TO 50
270   CALL MIO_EXT_GETINSTRUCT(MKRDEL,NBLB,instruction,N2,jfail,N1)
      IF(jfail.NE.0) then
        WRITE(amessage,271) trim(errsub)
271     format(a,' missing marker delimiter in user-supplied instruction set.')
        go to 9995
      end if
      IF(N1.EQ.0)THEN
        IF(DUMFLG.EQ.1) GO TO 9900
        GO TO 9800
      END IF
      IF(instruction(N1:N1).NE.MKRDEL) THEN
        IF(DUMFLG.EQ.1) GO TO 9900
        GO TO 9800
      END IF
      J2=INDEX(dline(J1+1:NBLC),instruction(N1+1:N2-1))
      IF(J2.EQ.0) THEN
        IF(DUMFLG.EQ.1) GO TO 9900
        GO TO 9800
      END IF
      NUM2=J1+J2-1
      IF(NUM2.LT.NUM1)THEN
        IF(DUMFLG.EQ.1) GO TO 9900
        GO TO 9800
      END IF
      WRITE(FMT,250) NUM2-NUM1+1
      IF(DUMFLG.EQ.1)THEN
        READ(dline(NUM1:NUM2),FMT,ERR=9900) RTEMP
      ELSE
        READ(dline(NUM1:NUM2),FMT,ERR=9800) EXTVAL(IVEXT)
        IF (IVERB>3 .AND. IOUT>0)   &
            WRITE(IOUT,255)TRIM(EXTNAM(IVEXT)),EXTVAL(IVEXT)
      END IF
      J1=NUM2
      GO TO 51
    ELSE
      WRITE(amessage,272) trim(errsub)
272   format(a,' cannot interpret user-supplied instruction for reading model ',  &
          'output file.')
      go to 9995
    END IF
    GO TO 50
1000 INS=INS+1
    IF(INS.LE.NINSTR) GO TO 10

    IF(MCALL.EQ.1)THEN
      DO 1100 I=1,NVEXT
        DO 1050 J=1,JVEXT
          IF(INS3(J).EQ.I) GO TO 1100
1050    CONTINUE
        WRITE(amessage,1051) trim(errsub),trim(EXTNAM(i))
1051    format(a,' extract-value "',a,'" not referenced in the user-supplied instruction set.')
        instruction=' '
        go to 9995
1100  CONTINUE
    END IF
    IF (IVERB>3 .AND. IOUT>0) WRITE(IOUT,*)' '
    CLOSE(UNIT=iunit)

    RETURN

9000 WRITE(amessage,9010) trim(errsub)
9010 format(a,' cannot read tab position from user-supplied instruction.')
    go to 9995
9050 WRITE(amessage,9060) trim(errsub)
9060 format(a,' cannot read line advance item from user-supplied instruction.')
    go to 9995
9100 WRITE(amessage,9110) trim(errsub),trim(afile)
9110 format(a,' unexpected end to model output file ',a,'.')
    go to 9995
9200 call UTL_NUM2CHAR(cil,anum)
    WRITE(amessage,9210) trim(errsub),trim(anum),trim(afile)
9210 format(a,' unable to find secondary marker on line ',a,   &
        ' of model output file ',a,'.')
    go to 9995
9400 call UTL_NUM2CHAR(cil,anum)
    WRITE(amessage,9410) trim(errsub),trim(anum),trim(afile)
9410 format(a,' unable to find requested whitespace, or whitespace ',  &
        'precedes end of line at line ',a,' of model output file ',a,'.')
    go to 9995
9700 WRITE(amessage,9710) trim(errsub),trim(EXTNAM1)
9710 format(a,' extract-value name "',a,'" from user-supplied instruction set ',&
        'is not cited in main program input file.')
    go to 9995
9800 call UTL_NUM2CHAR(cil,anum)
    WRITE(amessage,9810) trim(errsub),trim(EXTNAM(IVEXT)),trim(anum),trim(afile)
9810 format(a,' cannot read extract-value "',a,'" from line ',a,   &
        ' of model output file ',a,'.')
    go to 9995
9850 WRITE(amessage,9860) trim(afile)
9860 format('Unable to read model output file ',a,'.')
    instruction=' '
    go to 9995
9900 call UTL_NUM2CHAR(cil,anum)
    WRITE(amessage,9810) trim(errsub),'dum',trim(anum),trim(afile)
    go to 9995

9995 ifail=1
    return

  END subroutine MIO_EXT
  !-----------------------------------------------------------------------------
  SUBROUTINE MIO_EXT_GETCOLS(BUF,N2,N3,FMT,IFAIL,NUM1,NUM2)
    !
    ! -- SUBROUTINE MIO_EXT_GETCOLS RETRIEVES CHARACTER POSITIONS FROM FIXED AND
    ! -- SEMI-FIXED EXTRACT INSTRUCTIONS
    !
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER (LEN=*), INTENT(IN)    :: BUF
    INTEGER,           INTENT(IN)    :: N2
    INTEGER,           INTENT(INOUT) :: N3
    CHARACTER (LEN=*), INTENT(OUT)   :: FMT
    INTEGER,           INTENT(OUT)   :: IFAIL
    INTEGER,           INTENT(OUT)   :: NUM1
    INTEGER,           INTENT(OUT)   :: NUM2
    !
    !   Local variables
    INTEGER   :: I

    IFAIL=0
    I=INDEX(BUF(N3+1:N2),':')
    IF(I.EQ.0) GO TO 100
    WRITE(FMT,20) I-1
20  FORMAT('(I',I3,')')
    READ(BUF(N3+1:N3+I-1),FMT,ERR=100) NUM1
    N3=N3+I
    I=N2-N3
    IF(I.LT.1) GO TO 100
    WRITE(FMT,20) I
    READ(BUF(N3+1:N2),FMT,ERR=100) NUM2
    RETURN
100 IFAIL=1
    RETURN

  END SUBROUTINE MIO_EXT_GETCOLS

  !-----------------------------------------------------------------------------
  SUBROUTINE MIO_EXT_GETINSTRUCT(MRKDEL,NBLB,BUF,N2,IFAIL,N1)
    !
    ! -- SUBROUTINE MIO_EXT_GETINSTRUCT GETS THE NEXT STORED INSTRUCTION FOR PROCESSING
    !
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER (LEN=1), INTENT(IN)    :: MRKDEL
    INTEGER,           INTENT(IN)    :: NBLB
    CHARACTER (LEN=*), INTENT(INOUT) :: BUF
    INTEGER,           INTENT(INOUT) :: N2
    INTEGER,           INTENT(OUT)   :: IFAIL
    INTEGER,           INTENT(OUT)   :: N1
    !
    !   Local variables
    INTEGER   :: I,II

    IFAIL=0
    IF(N2.GE.NBLB) THEN
      N1=0
      RETURN
    END IF
    DO 10 I=N2+1,NBLB
      IF((BUF(I:I).NE.' ').AND.(ICHAR(BUF(I:I)).NE.9)) GO TO 50
10  CONTINUE
    N1=0
    RETURN
50  N1=I
    IF(BUF(N1:N1).NE.MRKDEL)THEN
      I=INDEX(BUF(N1:NBLB),' ')
      II=INDEX(BUF(N1:NBLB),CHAR(9))
      IF((I.EQ.0).AND.(II.EQ.0))THEN
        I=0
      ELSE IF(I.EQ.0)THEN
        I=II
      ELSE IF(II.EQ.0) THEN
        I=I
      ELSE
        I=MIN(I,II)
      END IF
      IF(I.NE.0) THEN
        N2=N1+I-2
      ELSE
        N2=NBLB
      END IF
    ELSE
      IF(N1.EQ.NBLB)THEN
        IFAIL=1
        RETURN
      END IF
      I=INDEX(BUF(N1+1:NBLB),MRKDEL)
      IF(I.EQ.0) THEN
        IFAIL=1
        RETURN
      END IF
      N2=N1+I
    END IF

    RETURN
  END subroutine MIO_EXT_GETINSTRUCT
  !-----------------------------------------------------------------------------
  SUBROUTINE MIO_EXT_MODOUTFILE_CHANGE(IFAIL,IFILE,OUTFILENAME)
    !   Store a filename in a selected element of the MODOUTFILE array
    USE GLOBAL_DATA, ONLY: AMESSAGE
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,          INTENT(OUT) :: IFAIL
    INTEGER,          INTENT(IN)  :: IFILE
    CHARACTER(LEN=*), INTENT(IN)  :: OUTFILENAME
    !
    !   Local variables
    INTEGER :: LENARG, LENARRAY
    !
    !   Format statements
    900 FORMAT('ERROR: File name too long to store in MODOUTFILE',   &
          ' array (MIO_EXT_MODOUTFILE_CHANGE)')
    905 FORMAT('ERROR: Array MODOUTFILE has not been allocated',   &
          ' (MIO_EXT_MODOUTFILE_CHANGE)')
    910 FORMAT('ERROR: Array index outside MODOUTFILE array',   &
          ' bounds (MIO_EXT_MODOUTFILE_CHANGE)')
    !
    IFAIL = 0
    LENARG = LEN_TRIM(OUTFILENAME)
    LENARRAY = LEN(MODOUTFILE)
    IF (LENARG>LENARRAY) THEN
      IFAIL = 1
      WRITE(AMESSAGE,900)
      RETURN
    ENDIF
    IF (.NOT. ALLOCATED(MODOUTFILE)) THEN
      IFAIL = 1
      WRITE(AMESSAGE,905)
      RETURN
    ENDIF
    IF (IFILE<1 .OR. IFILE>NUMOUTFILE) THEN
      IFAIL = 1
      WRITE(AMESSAGE,910)
      RETURN
    ENDIF
    MODOUTFILE(IFILE) = OUTFILENAME
    RETURN
  END SUBROUTINE MIO_EXT_MODOUTFILE_CHANGE
  !-----------------------------------------------------------------------------
  SUBROUTINE MIO_EXT_SEMI(NBLC,J1,J2,IFAIL)
    !
    ! -- SUBROUTINE MIO_EXT_SEMI DETERMINES THE EXACT POSITION OCCUPIED BY A NUMBER
    !
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(IN)    :: NBLC
    INTEGER, INTENT(INOUT) :: J1
    INTEGER, INTENT(INOUT) :: J2
    INTEGER, INTENT(OUT)   :: IFAIL
    !
    !   Local variables
    INTEGER :: I

    IFAIL=0
    IF(J1.GT.NBLC)THEN
      IFAIL=1
      RETURN
    END IF
    IF(J2.GT.NBLC)J2=NBLC
    IF(dline(J2:J2).EQ.' ') THEN
      DO 10 I=J2,J1,-1
      IF(dline(I:I).NE.' ')THEN
        J2=I
        GO TO 100
      END IF
10    CONTINUE
      IFAIL=1
      RETURN
    ELSE
      IF(J2.EQ.NBLC) GO TO 100
      DO 20 I=J2,NBLC
      IF(dline(I:I).EQ.' ') THEN
        J2=I-1
        GO TO 100
      END IF
20    CONTINUE
      J2=NBLC
    END IF
100 IF(J1.EQ.1) GO TO 200
    DO 120 I=J1,1,-1
      IF(dline(I:I).EQ.' ') THEN
        J1=I+1
        GO TO 200
      END IF
120 CONTINUE
    J1=1
200 RETURN

  END subroutine MIO_EXT_SEMI
  !-----------------------------------------------------------------------------
  SUBROUTINE MIO_CLN_DEALLOC()
    ! -- Subroutine MIO_CLN_DEALLOC deallocates memory usage by the MODEL_IO
    !    module.
    IMPLICIT NONE
    !   Deallocate arrays
    IF (ALLOCATED(CATEGORY)) DEALLOCATE(CATEGORY)
    IF (ALLOCATED(CISET)) DEALLOCATE(CISET)
    IF (ALLOCATED(INS1)) DEALLOCATE(INS1)
    IF (ALLOCATED(INS2)) DEALLOCATE(INS2)
    IF (ALLOCATED(INS3)) DEALLOCATE(INS3)
    IF (ALLOCATED(INSFILE)) DEALLOCATE(INSFILE)
    IF (ALLOCATED(LADV)) DEALLOCATE(LADV)
    IF (ALLOCATED(LCINS)) DEALLOCATE(LCINS)
    IF (ALLOCATED(MODINFILE)) DEALLOCATE(MODINFILE)
    IF (ALLOCATED(MODOUTFILE)) DEALLOCATE(MODOUTFILE)
    IF (ALLOCATED(MRKDEL)) DEALLOCATE(MRKDEL)
    IF (ALLOCATED(PARDEL)) DEALLOCATE(PARDEL)
    IF (ALLOCATED(PWORD)) DEALLOCATE(PWORD)
    IF (ALLOCATED(TEMPFILE)) DEALLOCATE(TEMPFILE)
    !
    !   Deallocate pointers to linked lists
    CALL TYP_DEALLOC(LLPTRMODIN)
    !
    RETURN
  END SUBROUTINE MIO_CLN_DEALLOC
  !-----------------------------------------------------------------------------
  SUBROUTINE MIO_CLN_OUTFILES(IFAIL)
    !
    ! -- Subroutine MIO_CLN_OUTFILES deletes model output files.
    !
    USE GLOBAL_DATA, ONLY: AMESSAGE
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                INTENT(OUT) :: IFAIL      ! indicates error condition
    !
    !   Local variables
    INTEGER                       :: IERR,JERR,IUNIT,I

    ifail=0
    do i=1,NUMOUTFILE
      iunit=UTL_NEXTUNIT()
      open(unit=iunit,file=modoutfile(i),status='OLD',iostat=ierr)
      if(ierr.eq.0)then
        close(unit=iunit,status='DELETE',iostat=jerr)
        if(jerr.ne.0)then
          call UTL_ADDQUOTE(modoutfile(i),afile)
          WRITE(amessage,10) trim(afile)
10        format('Cannot delete model output file ',a,' prior to running model.')
          ifail=1
          return
        end if
      end if
    end do

    return

  end subroutine MIO_CLN_OUTFILES



END MODULE MODEL_IO
