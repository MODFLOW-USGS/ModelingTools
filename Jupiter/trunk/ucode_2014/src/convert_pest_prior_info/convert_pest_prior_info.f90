PROGRAM ConvertPestPriorInfo
!
USE GLOBAL_DATA, ONLY: AMESSAGE, MAX_STRING_LEN, VERSIONID
USE UTILITIES
!
  IMPLICIT NONE
  CHARACTER(LEN=MAX_STRING_LEN) CHAR1
  CHARACTER(LEN=MAX_STRING_LEN) CHAR2
  CHARACTER(LEN=MAX_STRING_LEN) CHAR3
  LOGICAL FIRSTBLANKDONE
  CHARACTER(LEN=MAX_STRING_LEN) FN
  CHARACTER(LEN=MAX_STRING_LEN) FNOUT
  CHARACTER(LEN=MAX_STRING_LEN) GROUPNAME
  INTEGER I
  INTEGER ICNTREG
  INTEGER IFAIL
  INTEGER INERR
  INTEGER INFILE
  INTEGER IREGNAME
  INTEGER J
  INTEGER K
  CHARACTER(LEN=MAX_STRING_LEN) LINE
  CHARACTER(LEN=MAX_STRING_LEN) LINE2
  INTEGER M
  INTEGER N
  INTEGER OUTFILE
  INTEGER OUTRESULT
  CHARACTER(LEN=MAX_STRING_LEN) REGNAME
  CHARACTER(LEN=MAX_STRING_LEN) REGNAMEWHOLE
  CHARACTER(LEN=40) VERSION
  CHARACTER(LEN=20) VERSIONMIN
  PARAMETER (VERSION='1.000') !  03/2013
  !
! FORMAT LIST
  100 FORMAT(A74)
  200 FORMAT('nrow=',I7,' ncol=6  columnlabels')
  210 FORMAT('PriorName Equation PriorInfoValue Statistic StatFlag GroupName')
  220 FORMAT(A)
  230 FORMAT('*',A,'*')
  470 FORMAT(//,'       Regularization Base Name is: ',A)
  471 FORMAT('       Number of characters in base name is: ',I5)
  480 FORMAT (30X,'CONVERT_PEST_PRIOR_INFO',//, &
      24X,'CONSTRUCTED USING THE JUPITER API',/, &
      10X,'THIS IS A UCODE PRE-PROCESSING PROGRAM TO REFORMAT PRIOR',/, &
      14X,'INFORMATION FROM A PEST PREPROCESSED FILE',//, &
      34X,'Version ',A/)
  490 FORMAT (10X,'Result is listed in file: ',A)
  500 FORMAT(/,1X,70('*'),/, &
      '       Normal termination of CONVERT_PEST_PRIOR_INFO, Version: ',A, &
      /,1X,70('*'),//)
  !
  ! get srguments and open files
  FN = UTL_GETARG(1)
  IF(TRIM(FN) .EQ. ' ') THEN
    AMESSAGE = &
    ' !! MISSING PEST FILE NAME on COMMAND LINE!!'
    INERR = UTL_GETUNIT(101,150)
    FN = 'err.convertpestpriorinfo'
    OPEN (INERR,FILE=FN,STATUS='UNKNOWN')
    CALL UTL_WRITE_MESSAGE(INERR,'yes','yes','yes')
    CALL UTL_STOP &
    (' INCLUDE A ROOTFILENAME as the 1st argument on the COMMAND LINE')
    CLOSE(INERR)
  ENDIF
  !
  REGNAME = UTL_GETARG(2)
  IF(TRIM(REGNAME) .EQ. ' ') THEN
    AMESSAGE = &
    ' !! MISSING REGULARIZATION BASE NAME on COMMAND LINE!!'
    INERR = UTL_GETUNIT(101,150)
    FN = 'err.convertpestpriorinfo'
    OPEN (INERR,FILE=FN,STATUS='UNKNOWN')
    CALL UTL_WRITE_MESSAGE(INERR,'yes','yes','yes')
    CALL UTL_STOP &
    (' INCLUDE A REGULARIZATION BASE NAME as the 2nd argument on the' // &
     ' COMMAND LINE')
    CLOSE(INERR)
  ENDIF
  !
  INFILE = UTL_GETUNIT(101,150)
  OPEN(INFILE,STATUS='OLD',FILE=FN)
  FNOUT = TRIM(FN)//'.out'
  !
  OUTFILE = UTL_GETUNIT(101,150)
  OPEN(OUTFILE,STATUS='REPLACE',FILE='ConvertPestPriorInfo.#out')
  !
  OUTRESULT = UTL_GETUNIT(101,150)
  OPEN(OUTRESULT,STATUS='REPLACE',FILE=FNOUT)
  !
  ! WRITE PROGRAM NAME AND VERSION
  WRITE (*,*)
  WRITE (*,480)TRIM(VERSION)
  WRITE (OUTFILE,*)
  WRITE (OUTFILE,480)TRIM(VERSION)
  WRITE(OUTFILE,*)
  WRITE(OUTFILE,490)TRIM(FNOUT)
  WRITE(OUTRESULT,220)'BEGIN Linear_Prior_Information TABLE'
  !
  VERSIONMIN = '1.7.3'
  ! CHECK VERSION OF API COMPATIBILITY WITH CODE
  CALL UTL_VERSION_CHECK(VERSIONMIN,IFAIL)
  IF(IFAIL < 0) THEN
    AMESSAGE = ' Programming error:  Version '//TRIM(VERSIONMIN)//   &
               ' of JUPITER API is required.  Version in use is '   &
               //TRIM(VERSIONID)//' -- Please download newer version '   &
               //'of JUPITER API from U.S. Geological Survey'//   &
               ' JUPITER web site.'
    CALL UTL_WRITE_MESSAGE(OUTFILE,'yes','yes','yes')
    CALL UTL_WRITE_MESSAGE(OUTFILE,'yes','yes','yes')
    CLOSE(INFILE)
    CLOSE(OUTFILE)
    CLOSE(OUTRESULT)
    CALL UTL_STOP()
  ENDIF
  !
  ! count characters in regularization base name
  DO I=1,50000
    IF(REGNAME(I:I) /= ' ') CYCLE
    EXIT
  ENDDO
  IREGNAME = I-1
  WRITE(OUTFILE,470)TRIM(REGNAME)
  WRITE(OUTFILE,471)IREGNAME
  !
  DO
    READ(INFILE,*,END=999)CHAR1
    IF(TRIM(CHAR1) == '*') THEN
      BACKSPACE(INFILE)
      READ(INFILE,*,END=999)CHAR1,CHAR2
      CALL UTL_CASE(TRIM(CHAR2),CHAR2,-1)
      IF(TRIM(CHAR2) == 'prior') THEN
        BACKSPACE(INFILE)
        READ(INFILE,*,END=999)CHAR1,CHAR2,CHAR3
        CALL UTL_CASE(TRIM(CHAR3),CHAR3,-1)
        IF(TRIM(CHAR3) == 'information') EXIT
      ENDIF
    ENDIF
  ENDDO
  !
  ! count regularization items
  ICNTREG = 0
  DO
    READ(INFILE,*,END=999)LINE
    CALL UTL_CASE(LINE,LINE,-1)
    IF(LINE(1:1) == '*') EXIT
    DO I=1,IREGNAME+1
      IF(LINE(I:I) /= REGNAME(I:I)) THEN
        IF(I-1 == IREGNAME) ICNTREG = ICNTREG +1
        EXIT
      ENDIF
    ENDDO
  ENDDO
  !
  WRITE(OUTRESULT,200)ICNTREG
  WRITE(OUTRESULT,210)
  !
  DO I=1,ICNTREG+1
    BACKSPACE(INFILE)
  ENDDO
  !
  ! read regularization lines
  DO N=1,ICNTREG
    LINE(1:MAX_STRING_LEN) = ' '
    LINE2(1:MAX_STRING_LEN) = ' '
    READ(INFILE,*,END=999)REGNAMEWHOLE
    BACKSPACE(INFILE)
    READ(INFILE,100,END=999)LINE
    CALL UTL_CASE(LINE,LINE,-1)
    ! parse and reformat line
    K = 1
    ! read and add whole reg name including part after base name
    DO I=1,MAX_STRING_LEN
      IF(LINE(I:I) == REGNAMEWHOLE(I:I)) THEN
        LINE2(K:K) = LINE(I:I)
        K=K+1
        CYCLE
      ELSE
        EXIT
      ENDIF
    ENDDO
    ! add banks to output line
    DO I=1,3
      LINE2(K:K) = ' '
      K=K+1
    ENDDO
    ! add equation
    DO J=I,MAX_STRING_LEN
      IF(LINE(J:J) /= ' ')THEN
        IF(LINE(J:J) /= '=')THEN
          LINE2(K:K) = LINE(J:J)
          IF(LINE2(K:K) == 'g') THEN
            IF(LINE(J+1:J+1) == '(') THEN
              IF(LINE2(K-1:K-1) == 'o') THEN
                IF(LINE2(K-2:K-2) == 'l') THEN
                  K=K+1
                  LINE2(K:K) = '1'
                  K=K+1
                  LINE2(K:K) = '0'
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          K=K+1
        ELSE
          EXIT
        ENDIF
      ENDIF
    ENDDO
    ! add banks to output line
    DO I=1,3
      LINE2(K:K) = ' '
      K=K+1
    ENDDO
    J=J+1
    !
    ! add value
    FIRSTBLANKDONE = .FALSE.
    DO M=J,MAX_STRING_LEN
      IF(LINE(M:M) /= ' ')THEN
        FIRSTBLANKDONE = .TRUE.
        LINE2(K:K) = LINE(M:M)
        K=K+1
        CYCLE
      ELSEIF(FIRSTBLANKDONE)THEN
        EXIT
      ENDIF
    ENDDO
    ! add banks to output line
    DO I=1,3
      LINE2(K:K) = ' '
      K=K+1
    ENDDO
    M=M+1
    !
    ! add statistic
    FIRSTBLANKDONE = .FALSE.
    DO J=M,MAX_STRING_LEN
      IF(LINE(J:J) /= ' ')THEN
        FIRSTBLANKDONE = .TRUE.
        LINE2(K:K) = LINE(J:J)
        K=K+1
        CYCLE
      ELSEIF(FIRSTBLANKDONE)THEN
        EXIT
      ENDIF
    ENDDO
    ! add banks to output line
    DO I=1,3
      LINE2(K:K) = ' '
      K=K+1
    ENDDO
    J=J+1
    !
    ! add statflag
    LINE2(K:K) = 's'
    K=K+1
    LINE2(K:K) = 'q'
    K=K+1
    LINE2(K:K) = 'r'
    K=K+1
    LINE2(K:K) = 'w'
    K=K+1
    LINE2(K:K) = 't'
    K=K+1
    ! add banks to output line
    DO I=1,3
      LINE2(K:K) = ' '
      K=K+1
    ENDDO
    J=J+1
    !
    ! add group name
    FIRSTBLANKDONE = .FALSE.
    DO M=J,MAX_STRING_LEN
      IF(LINE(M:M) /= ' ')THEN
        FIRSTBLANKDONE = .TRUE.
        LINE2(K:K) = LINE(M:M)
        K=K+1
        CYCLE
      ELSEIF(FIRSTBLANKDONE)THEN
        EXIT
      ENDIF
    ENDDO
    !
    WRITE(OUTRESULT,220)TRIM(LINE2)
    !
  ENDDO
  !
  WRITE(OUTRESULT,220)'END Linear_Prior_Information'
  WRITE(OUTFILE,500)TRIM(VERSION)
  WRITE(*,500)TRIM(VERSION)
  !
  GO TO 998
  999 AMESSAGE = ' TERMINATING '
  CALL UTL_WRITE_MESSAGE(OUTFILE,'yes','yes','yes')
  998 CONTINUE
  !
  ! Close files
  CLOSE(INFILE)
  CLOSE(OUTFILE)
  CLOSE(OUTRESULT)
  STOP
END PROGRAM ConvertPestPriorInfo
!===============================================================================
!===============================================================================
