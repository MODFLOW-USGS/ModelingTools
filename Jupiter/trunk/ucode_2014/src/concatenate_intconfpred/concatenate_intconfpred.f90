PROGRAM CONCATENATE_INTCONF
!
USE GLOBAL_DATA, ONLY: AMESSAGE, MAX_STRING_LEN, VERSIONID
USE DATATYPES
USE UTILITIES
USE UTLUCODE
!
  IMPLICIT NONE
  CHARACTER(LEN=10)                                           :: CHDATE
  CHARACTER(LEN=10)                                           :: CHTIME
  CHARACTER(LEN=10)                                           :: CHZONE
  CHARACTER(LEN=20)                                           :: CHARREAD
  INTEGER                                                     :: I
  INTEGER                                                     :: J
  INTEGER                                                     :: K
  INTEGER                                                     :: KCOMP
  INTEGER                                                     :: L
  INTEGER                                                     :: M
  INTEGER                                                     :: N
  INTEGER                                                     :: NN
  INTEGER, ALLOCATABLE, DIMENSION(:)                          :: I1
  INTEGER, ALLOCATABLE, DIMENSION(:)                          :: I1F
  INTEGER, ALLOCATABLE, DIMENSION(:)                          :: I2
  INTEGER, ALLOCATABLE, DIMENSION(:)                          :: I2F
  INTEGER                                                     :: IBDT(8)
  INTEGER, ALLOCATABLE, DIMENSION(:)                          :: ICNT
  INTEGER                                                     :: IFAIL
  INTEGER                                                     :: IFN
  INTEGER                                                     :: IREPDIFF
  INTEGER                                                     :: IFNNONEX
  INTEGER                                                     :: ILOGTUP
  INTEGER                                                     :: INERR
  INTEGER                                                     :: INFILE
  INTEGER                                                     :: INPUT
  CHARACTER(LEN=MAX_STRING_LEN)                               :: FN
  CHARACTER(LEN=MAX_STRING_LEN)                               :: FNINPUT
  CHARACTER(LEN=MAX_STRING_LEN)                               :: FNBAD(10000)
  CHARACTER(LEN=MAX_STRING_LEN)                               :: FNNONEX(10000)
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:)    :: FNID
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:)    :: FNID2
  CHARACTER(LEN=MAX_STRING_LEN), ALLOCATABLE, DIMENSION(:)    :: FNIN
  LOGICAL                                                     :: LEX = .FALSE.
  LOGICAL                                                     :: LOGTUP = &
                                                                        .FALSE.
  LOGICAL                                                     :: MIXEDLIM = &
                                                                        .FALSE.
  CHARACTER(LEN=20), ALLOCATABLE, DIMENSION(:)                :: NAMID
  CHARACTER(LEN=20), ALLOCATABLE, DIMENSION(:)                :: NAM
  CHARACTER(LEN=20), ALLOCATABLE, DIMENSION(:)                :: NAMF
  INTEGER                                                     :: OUTFILE
  CHARACTER(LEN=MAX_STRING_LEN)                               :: OUTNAM
  INTEGER                                                     :: OUTPUT
  INTEGER                                                     :: OUTPUTLB
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)                 :: R1
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)                 :: R1F
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)                 :: R2
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)                 :: R2F
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)                 :: R3
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)                 :: R3F
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)                 :: R4
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)                 :: R4F
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)                 :: R5
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)                 :: R5F
  LOGICAL                                                     :: REPDIFF = &
                                                                        .FALSE.
  CHARACTER(LEN=MAX_STRING_LEN)                               :: ROOTNAM
  LOGICAL                                                     :: TERMINATE = &
                                                                       .FALSE.
  CHARACTER(LEN=13), ALLOCATABLE, DIMENSION(:)                :: TYPEINDSIM
  CHARACTER(LEN=4)                                            :: TYPEINT
  CHARACTER(LEN=13), ALLOCATABLE, DIMENSION(:)                :: TYPEINDSIMF
  CHARACTER(LEN=8), ALLOCATABLE, DIMENSION(:)                 :: YN
  CHARACTER(LEN=8), ALLOCATABLE, DIMENSION(:)                 :: YNF
  CHARACTER(LEN=40)                                           :: VERSION
  CHARACTER(LEN=20)                                           :: VERSIONMIN
  PARAMETER (VERSION='1.000') !  03/2013
  ! FORMAT LIST
  134 FORMAT(1X,'ITEM # ',I4,1X,A20,1X,I12,3(3X,1PE16.7),13X,1PE16.7,10X, &
             G16.7,12X,A13,1X,I12)
  135 FORMAT(1X,A20,1X,I12,' "YES"     ',3(3X,1PE16.7),13X,1PE16.7,10X, &
             G16.7,12X,A13,1X,I12)
  136 FORMAT(1X,A20,1X,I12,' "!!!NO!!!"',3(3X,1PE16.7),13X,1PE16.7,10X, &
             G16.7,12X,A13,1X,I12)
  250 FORMAT(1X,A20,2X,A,2X,A)
  370 FORMAT (1X,'"ITEM #   " "INTERVAL NAME" "LIMIT IDENTIFIER"' &
      ' "PREDICTED VALUE" "CONFIDENCE LIMIT" "SUM OF SQUARED RESIDUALS"' &
      ' "OBJECTIVE-FUNCTION GOAL" "PERCENT DEVIATION FROM GOAL"' &
      ' "INDIVIDUALorSIMULTANEOUS" "ITERATIONS"')
  371 FORMAT (1X,'"ITEM #   " "INTERVAL NAME" "LIMIT IDENTIFIER"' &
      ' "PREDICTED VALUE" "PREDICTION LIMIT" "SUM OF SQUARED RESIDUALS"' &
      ' "OBJECTIVE-FUNCTION GOAL" "PERCENT DEVIATION FROM GOAL"' &
      ' "INDIVIDUALorSIMULTANEOUS" "ITERATIONS"')
  380 FORMAT (1X,'"INTERVAL NAME" "LIMIT IDENTIFIER" "CONVERGED"' &
      ' "PREDICTED VALUE" "CONFIDENCE LIMIT" "SUM OF SQUARED RESIDUALS"' &
      ' "OBJECTIVE-FUNCTION GOAL" "PERCENT DEVIATION FROM GOAL"' &
      ' "INDIVIDUALorSIMULTANEOUS" "ITERATIONS"')
  381 FORMAT (1X,'"INTERVAL NAME" "LIMIT IDENTIFIER" "CONVERGED"' &
      ' "PREDICTED VALUE" "PREDICTION LIMIT" "SUM OF SQUARED RESIDUALS"' &
      ' "OBJECTIVE-FUNCTION GOAL" "PERCENT DEVIATION FROM GOAL"' &
      ' "INDIVIDUALorSIMULTANEOUS" "ITERATIONS"')
  480 FORMAT (30X,'CONCATENATE_INTCONFPRED',//, &
      24X,'CONSTRUCTED USING THE JUPITER API',/, &
      10X,'THIS IS A UCODE POST-PROCESSING PROGRAM TO CONCATENATE MULTIPLE',/, &
      14X,'*._intconf OR *._intpred FILES FOR POST-PROCESSING',//, &
      34X,'Version ',A/)
  500 FORMAT(/,1X,70('*'),/, &
             '  Normal termination of CONCATENATE_INTCONFPRED, Version: ',A, &
             /,1X,70('*'),//)
  ! WRITE PROGRAM NAME AND VERSION TO SCREEN
  WRITE (*,*)
  WRITE (*,480) VERSION
  CALL DATE_AND_TIME(CHDATE,CHTIME,CHZONE,IBDT)
  ! GET INPUT FILE NAMES FROM COMMAND LINE
  ROOTNAM = UTL_GETARG(1)
  IF(TRIM(ROOTNAM) .EQ. ' ') THEN
    AMESSAGE = &
    ' !! MISSING ROOTFILENAME on CONCATENATE_INTCONFPRED COMMAND LINE!!'
    INERR = UTL_GETUNIT(101,150)
    FN = 'err.concatenate'
    OPEN (INERR,FILE=FN,STATUS='UNKNOWN')
    CALL UTL_WRITE_MESSAGE(INERR,'yes','yes','yes')
    CALL UTL_STOP &
    (' INCLUDE A ROOTFILENAME on the CONCATENATE_INTCONFPRED COMMAND LINE')
    CLOSE(INERR)
  ENDIF
  ! OPEN FILES
  INPUT = UTL_GETUNIT(101,150)
  FN = TRIM(ROOTNAM)//'.concatenate'
  OPEN (INPUT,FILE=FN,STATUS='OLD')
  READ(INPUT,*)TYPEINT
  CALL UTL_CASE(TRIM(TYPEINT),TYPEINT,-1)
  OUTPUTLB = UTL_GETUNIT(101,150)
  FN = TRIM(ROOTNAM)//'.#concatenated._int'//TRIM(TYPEINT)
  FNINPUT = FN
  OPEN (OUTPUTLB,FILE=FN,STATUS='UNKNOWN')
  WRITE (OUTPUTLB,*)
  WRITE (OUTPUTLB,480) VERSION
  WRITE (OUTPUTLB,*)
  OUTPUT = UTL_GETUNIT(101,150)
  FN = TRIM(ROOTNAM)//'._concatenated._int'//TRIM(TYPEINT)
  OPEN (OUTPUT,FILE=FN,STATUS='UNKNOWN')
  WRITE (OUTPUTLB,*)
  IF(TYPEINT == 'conf')WRITE (OUTPUTLB,370)
  IF(TYPEINT == 'pred')WRITE (OUTPUTLB,371)
  WRITE (OUTPUTLB,*)
  WRITE (OUTPUT,*)
  IF(TYPEINT == 'conf')WRITE (OUTPUT,380)
  IF(TYPEINT == 'pred')WRITE (OUTPUT,381)
  WRITE (OUTPUT,*)
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
    CALL UTL_WRITE_MESSAGE(OUTPUTLB,'yes','yes','yes')
    CALL UTL_WRITE_MESSAGE(OUTPUT,'yes','yes','yes')
    CLOSE(INPUT)
    CLOSE(OUTPUT)
    CLOSE(OUTPUTLB)
    CALL UTL_STOP()
  ENDIF
  !
  FNBAD = ' '
  FNNONEX = ' '
  ALLOCATE(ICNT(30000))
  ICNT = 0
  IFN = 0
  IREPDIFF = 0
  IFNNONEX = 0
  ILOGTUP = 0
  I = 0
  J = 0
  DO
    READ(INPUT,*,END=600)FN
    IF(TRIM(FN) == ' ') EXIT
    FN = TRIM(FN)//'._int'//TRIM(TYPEINT)
    INQUIRE(FILE=FN,EXIST=LEX)
    IF(LEX) THEN
      INFILE = UTL_GETUNIT(101,150)
      OPEN (INFILE,FILE=FN,STATUS='OLD')
      READ(INFILE,*)
      READ(INFILE,*) CHARREAD,CHARREAD,CHARREAD,CHARREAD,CHARREAD
      IF((TYPEINT == 'conf' .AND. TRIM(CHARREAD) /= "CONFIDENCE LIMIT") .OR. &
         (TYPEINT == 'pred' .AND. TRIM(CHARREAD) /= "PREDICTION LIMIT")) THEN
        IFN = IFN + 1
        FNBAD(IFN) = FN
        TERMINATE = .TRUE.
        CYCLE
      ENDIF
      I = I+1
      READ(INFILE,*)
      DO
        READ(INFILE,*,END=700)NAM
        ICNT(I) = ICNT(I) + 1
        J = J+1
      ENDDO
      700 CLOSE(INFILE)
    ELSE
      IFNNONEX = IFNNONEX + 1
      FNNONEX(IFNNONEX) = TRIM(FN)
    ENDIF
  ENDDO
  !
  600 CLOSE(INPUT)
  !
  IF(IFN > 0) THEN
    AMESSAGE = &
           ' LIMITS MUST ALL BE THE SAME TYPE, EITHER CONFIDENCE OR PRECDICTION'
    CALL UTL_WRITE_MESSAGE(OUTPUTLB,'yes','yes','yes')
    CALL UTL_WRITE_MESSAGE(OUTPUT,'yes','yes','yes')
    AMESSAGE = &
         ' UNACCEPTABLE FILES INCLUDE:'
    CALL UTL_WRITE_MESSAGE(OUTPUTLB,'yes','yes','yes')
    CALL UTL_WRITE_MESSAGE(OUTPUT,'yes','yes','yes')
    DO I = 1,IFN
      WRITE(OUTPUTLB,*)TRIM(FNBAD(I))
      WRITE(OUTPUT,*)TRIM(FNBAD(I))
    ENDDO
    TERMINATE = .TRUE.
  ENDIF
  !
  IF(IFNNONEX > 0) THEN
    AMESSAGE = &
       ' SOME OF THE TIME-STAMPED FILES LISTED IN THE INPUT DO NOT EXIST, ' // &
       ' INCLUDING:'
    CALL UTL_WRITE_MESSAGE(OUTPUTLB,'yes','yes','yes')
    CALL UTL_WRITE_MESSAGE(OUTPUT,'yes','yes','yes')
    DO I = 1,IFNNONEX
      WRITE(OUTPUTLB,*)TRIM(FNNONEX(I))
      WRITE(OUTPUT,*)TRIM(FNNONEX(I))
    ENDDO
    TERMINATE = .TRUE.
  ENDIF
  !
  IF(.NOT. TERMINATE) THEN
    !
    INPUT = UTL_GETUNIT(101,150)
    FN = TRIM(ROOTNAM)//'.concatenate'
    OPEN (INPUT,FILE=FN,STATUS='OLD')
    READ(INPUT,*)
    !
    ALLOCATE(NAM(J),I1(J),YN(J),R1(J),R2(J),R3(J),R4(J),R5(J),TYPEINDSIM(J),I2(J))
    ALLOCATE(NAMF(J),I1F(J),YNF(J),R1F(J),R2F(J),R3F(J),R4F(J),R5F(J), &
             TYPEINDSIMF(J),I2F(J),FNIN(J))
    ALLOCATE(FNID(J),FNID2(J),NAMID(J))
    NAM = ' '
    I1  = 0
    YN  = ' '
    R1  = 0.D0
    R2  = 0.D0
    R3  = 0.D0
    R4  = 0.D0
    R5  = 0.D0
    TYPEINDSIM = ' '
    I2  = 0
    NAMF = ' '
    I1F  = 0
    YNF  = ' '
    R1F  = 0.D0
    R2F  = 0.D0
    R3F  = 0.D0
    R4F  = 0.D0
    R5F  = 0.D0
    TYPEINDSIMF = ' '
    I2F  = 0
    FNID = ' '
    FNID2 = ' '
    NAMID = ' '
    !
    K = 1
    DO M=1,I
      READ(INPUT,*,END=900)FN
      FN = TRIM(FN)//'._int'//TRIM(TYPEINT)
      INFILE = UTL_GETUNIT(101,150)
      OPEN (INFILE,FILE=FN,STATUS='OLD')
      WRITE(OUTPUTLB,*)
      WRITE(OUTPUTLB,*)' READING ',TRIM(FN)
      WRITE(OUTPUTLB,*)
      READ(INFILE,*)
      READ(INFILE,*)
      READ(INFILE,*)
      DO L=1,ICNT(M)
        FNIN(K) = TRIM(FN)
        READ(INFILE,*,END=800) &
             NAM(K),I1(K),YN(K),R1(K),R2(K),R3(K),R4(K),R5(K),TYPEINDSIM(K),I2(K)
        IF(K > 1) THEN
          IF(TYPEINDSIM(K) /= TYPEINDSIM(K-1)) THEN
            MIXEDLIM = .TRUE.
            WRITE(OUTPUTLB,*)' !!!! MIXED INDIVIDUAL and SIMULTANEOUS LIMITS !!!!'
            TERMINATE = .TRUE.
          ENDIF
        ENDIF
        WRITE(OUTPUTLB,134) &
             K,NAM(K),I1(K),R1(K),R2(K),R3(K),R4(K),R5(K),TYPEINDSIM(K),I2(K)
        K = K+1
      ENDDO
      800 CLOSE(INFILE)
    ENDDO
    DEALLOCATE(ICNT)
    !
    900 CLOSE(OUTFILE)
    !
    IF(MIXEDLIM) THEN
      AMESSAGE = ' ALL LIMITS MUST BE OF THE SAME TYPE, EITHER INDIVIDUAL' // &
      ' OR SIMULTANEOUS. THE USER IS ADVISED TO EDIT THE FILES INCLUDED IN' // &
      ' THE MAIN INPUT FILE.'
      CALL UTL_WRITE_MESSAGE(OUTPUTLB,'yes','yes','yes')
      CALL UTL_WRITE_MESSAGE(OUTPUT,'yes','yes','yes')
      TERMINATE = .TRUE.
    ENDIF
    !
    CALL UTLUCODE_SHELLSORTMANY(K,NAM,R1,R2,R3,R4,R5,YN,TYPEINDSIM,I1,I2,FNIN)
    !
    IF(I1(1) > 0) THEN
      I1F(1) = 1
    ELSE
      I1F(1) = -1
    ENDIF
    NAMF(1) = NAM(1)
    YNF(1)  = YN(1)
    R1F(1)  = R1(1)
    R2F(1)  = R2(1)
    R3F(1)  = R3(1)
    R4F(1)  = R4(1)
    R5F(1)  = R5(1)
    TYPEINDSIMF(1) = TYPEINDSIM(1)
    I2F(1)  = I2(1)
    !
    I = 1
    DO K=2,J
      ! is this the second occurrence of this name?
      IF(TRIM(NAM(K)) == TRIM(NAM(K-1))) THEN
        ! if so, is it the opposite limit?
        KCOMP = 1
        IF(I1(K) > 0 .AND. I1(K-1) > 0) GO TO 333 !repeat of same limit if all else identical skip this and move on
        IF(I1(K) < 0 .AND. I1(K-1) < 0) GO TO 333 !repeat of same limit if all else identical skip this and move on
        IF(K > 2) THEN
          IF(TRIM(NAM(K)) == TRIM(NAM(K-2))) THEN !repeat
            KCOMP = 2
            IF(I1(K) > 0 .AND. I1(K-2) > 0) GO TO 333 !repeat of same limit 2 back if all else identical skip this and move on
            IF(I1(K) < 0 .AND. I1(K-2) < 0) GO TO 333 !repeat of same limit 2 back if all else identical skip this and move on
          ENDIF
        ENDIF
      ENDIF
      GO TO 334 ! a new name and/or a new limit
      333 CONTINUE ! same limit for same name - test to see if it can be ignored
      IF(R1(K) /= R1(K-KCOMP)) REPDIFF = .TRUE.
      IF(R2(K) /= R2(K-KCOMP)) REPDIFF = .TRUE.
      IF(R3(K) /= R3(K-KCOMP)) REPDIFF = .TRUE.
      IF(R4(K) /= R4(K-KCOMP)) REPDIFF = .TRUE.
      IF(R5(K) /= R5(K-KCOMP)) REPDIFF = .TRUE.
      IF(YN(K) /= YN(K-KCOMP)) REPDIFF = .TRUE.
      IF(TYPEINDSIM(K) /= TYPEINDSIM(K-KCOMP)) REPDIFF = .TRUE.
      IF(.NOT. REPDIFF) THEN
        WRITE(OUTPUTLB,*)
        IF(I1(K) > 0)WRITE(OUTPUTLB,*)' IDENTICAL UPPER LIMIT REPEATED FOR:'
        IF(I1(K) < 0)WRITE(OUTPUTLB,*)' IDENTICAL LOWER LIMIT REPEATED FOR:'
        WRITE(OUTPUTLB,*)'  ',TRIM(NAM(K))
        WRITE(OUTPUTLB,*)'                      IGNORE AND CONTINUE'
        ! must be exactly same limit repeated so ignore and go to next item
      ELSE ! non-identical repeated item
        IREPDIFF = IREPDIFF + 1
        NAMID(IREPDIFF) = TRIM(NAM(K))
        FNID(IREPDIFF) = TRIM(FNIN(K-KCOMP))
        FNID2(IREPDIFF) = TRIM(FNIN(K))
        REPDIFF = .FALSE.
      ENDIF
      CYCLE
      334 CONTINUE ! a valid new limit
      I = I + 1
      IF(TRIM(NAM(K)) == TRIM(NAM(K-1))) THEN
        IF(I1(K) > I1(K-1)) THEN
          IF(R2(K) < R2(K-1)) THEN ! error upper limit < lower limit
            LOGTUP = .TRUE.
          ENDIF
        ENDIF
        IF(I1(K) < I1(K-1)) THEN
          IF(R2(K) > R2(K-1)) THEN ! error lower limit > upper limit
            LOGTUP = .TRUE.
          ENDIF
        ENDIF
        IF(LOGTUP) THEN
            ILOGTUP = ILOGTUP + 1
            TERMINATE = .TRUE.
            WRITE(OUTPUTLB,*)' !!!! LOWER LIMT GREATER THAN UPPER LIMIT !!!!'
            WRITE(OUTPUTLB,*)'  ITEM NAME '
            WRITE(OUTPUTLB,*)'   ',TRIM(NAM(K))
            WRITE(OUTPUTLB,*)'  FILE NAMES '
            WRITE(OUTPUTLB,*)'   ',TRIM(FNIN(K)),'   ',TRIM(FNIN(K-1))
            LOGTUP = .FALSE.
        ENDIF
        I1F(I) = -I1F(I-1)
      ELSE
        IF(I1(K) > 0) THEN
          I1F(I) =  (ABS(I1F(K-1)) + 1)
        ELSE
          I1F(I) = -(ABS(I1F(K-1)) + 1)
        ENDIF
      ENDIF
      NAMF(I) = NAM(K)
      YNF(I)  = YN(K)
      R1F(I)  = R1(K)
      R2F(I)  = R2(K)
      R3F(I)  = R3(K)
      R4F(I)  = R4(K)
      R5F(I)  = R5(K)
      TYPEINDSIMF(I) = TYPEINDSIM(K)
      I2F(I)  = I2(K)
    ENDDO
    !
    IF(IREPDIFF > 0) THEN
      AMESSAGE = ' ENCOUNTERED SAME NAME AND LIMIT MORE THAN ONCE IN' // &
      ' THE LIST OF TIME-STAMPED FILES WITH DIFFERENT ASSOCIATED VALUES.' // &
      ' THIS IS NOT VALID. REVIEW THE FILES BEING CONCATENATED AND REVISE' // &
      ' THE LIST OF FILES AND/OR REMOVE UNWANTED ITEMS FROM THE FILES.' // &
      ' THE FOLLOWING PROBLEMS WERE IDENTIFIED:'
      CALL UTL_WRITE_MESSAGE(OUTPUTLB,'yes','yes','yes')
      CALL UTL_WRITE_MESSAGE(OUTPUT,'yes','yes','yes')
      AMESSAGE = '                ITEM    FILE1    FILE 2'
      CALL UTL_WRITE_MESSAGE(OUTPUTLB,'no','yes','yes')
      CALL UTL_WRITE_MESSAGE(OUTPUT,'no','yes','yes')
      DO I = 1,IREPDIFF
        WRITE(OUTPUTLB,250)TRIM(NAMID(I)),TRIM(FNID(I)),TRIM(FNID2(I))
        WRITE(OUTPUT,250)TRIM(NAMID(I)),TRIM(FNID(I)),TRIM(FNID2(I))
      ENDDO
      TERMINATE = .TRUE.
    ENDIF

    IF(ILOGTUP > 0) THEN
      AMESSAGE = ' ENCOUNTERED SOME ITEMS WITH UPPER LIMIT < LOWER LIMIT.'
      CALL UTL_WRITE_MESSAGE(OUTPUTLB,'no','yes','no')
      CALL UTL_WRITE_MESSAGE(OUTPUT,'no','yes','no')
      AMESSAGE = ' REVIEW the information in the file:' 
      CALL UTL_WRITE_MESSAGE(OUTPUT,'no','yes','yes')
      WRITE(OUTPUT,*)'     ',TRIM(FNINPUT)
      AMESSAGE = ' REVIEW the information above in this file' 
      CALL UTL_WRITE_MESSAGE(OUTPUTLB,'no','yes','yes')
      AMESSAGE = ' Search for: ' // &
      ' !!!! LOWER LIMT GREATER THAN UPPER LIMIT !!!!.'
      CALL UTL_WRITE_MESSAGE(OUTPUTLB,'no','no','no')
      CALL UTL_WRITE_MESSAGE(OUTPUT,'no','yes','no')
      AMESSAGE = ' THEN RERUN THOSE' // &
      ' INTERVALS OR REMOVE THEM FROM THE TIME-STAMPED FILES'
      CALL UTL_WRITE_MESSAGE(OUTPUTLB,'no','yes','yes')
      CALL UTL_WRITE_MESSAGE(OUTPUT,'no','yes','yes')
      TERMINATE = .TRUE.
    ENDIF

    IF(.NOT. TERMINATE) THEN
      DO K=1,I
        IF(TRIM(YN(K)) == 'YES') THEN
          WRITE(OUTPUT,135) &
          NAMF(K),I1F(K),R1F(K),R2F(K),R3F(K),R4F(K),R5F(K),TYPEINDSIM(K),I2F(K)
        ELSE
          WRITE(OUTPUT,136) &
          NAMF(K),I1F(K),R1F(K),R2F(K),R3F(K),R4F(K),R5F(K),TYPEINDSIMF(K),I2F(K)
        ENDIF
      ENDDO
    ENDIF
    !
    !Deallocate
    DEALLOCATE(NAM,I1,YN,R1,R2,R3,R4,R5,TYPEINDSIM,I2)
    DEALLOCATE(NAMF,I1F,YNF,R1F,R2F,R3F,R4F,R5F,TYPEINDSIMF,I2F,FNIN)
    DEALLOCATE(FNID,FNID2,NAMID)
  ENDIF
  
  IF(TERMINATE) THEN
    AMESSAGE = ' TERMINATING '
    CALL UTL_WRITE_MESSAGE(OUTPUTLB,'yes','yes','yes')
    CALL UTL_WRITE_MESSAGE(OUTPUT,'yes','yes','yes')
  ENDIF
  !
  ! Close files
  WRITE(*,500)VERSION
  WRITE(OUTPUTLB,500)VERSION
  CALL UTL_ENDTIME(IBDT,OUTPUTLB)
  CLOSE(INPUT)
  CLOSE(OUTPUT)
  CLOSE(OUTPUTLB)
  STOP
END PROGRAM CONCATENATE_INTCONF
!===============================================================================
!===============================================================================
