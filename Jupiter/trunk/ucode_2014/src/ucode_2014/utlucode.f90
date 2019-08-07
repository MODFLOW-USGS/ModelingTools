MODULE UTLUCODE
  !
  USE GLOBAL_DATA, ONLY: AMESSAGE, BIGDOUBLE, ERRSUB, LENDNAM, MAX_STRING_LEN
  USE DATATYPES
  USE DEPENDENTS, ONLY: OBSVAL
  USE SENSITIVITY, ONLY: SEN_UEV_DX_WRITE_MATRIX
  USE UTILITIES
  PRIVATE
  PUBLIC :: UTLUCODE_CHECK_PRI,             &
            UTLUCODE_COPY_FILE,             &
            UTLUCODE_LOG_PRI,               &
            UTLUCODE_DX_WRITE_B1,           &
            UTLUCODE_DX_WRITE_B3,           &
            UTLUCODE_DX_READ_B,             &
            UTLUCODE_DX_READ_BOBS,          &
            UTLUCODE_DX_READ_CFSU,          &
            UTLUCODE_CHISQ,                 &
            UTLUCODE_DX_READ_DM,            &
            UTLUCODE_DX_WRITE_DM,           &
            UTLUCODE_CSSSVD,                &
            UTLUCODE_DX_WRITE_CSSSVD,       &
            UTLUCODE_DX_WRITE_SVD_ID,       &
            UTLUCODE_EIGEN,                 &
            UTLUCODE_DX_WRITE_EIG,          &
            UTLUCODE_EIGEN1,                &
            UTLUCODE_EIGEN2,                &
            UTLUCODE_EIGEN4,                &
            UTLUCODE_DX_READ_INIT,          &
            UTLUCODE_DX_WRITE_INIT,         &
            UTLUCODE_DX_WRITE_PA,           &
            UTLUCODE_DX_READ_PAOPT,         &
            UTLUCODE_DX_WRITE_PAOPT,        &
            UTLUCODE_DX_READ_PAPRI,         &
            UTLUCODE_DX_WRITE_PAPRI,        &
            UTLUCODE_WRITEPARS,             &
            UTLUCODE_DX_WRITE_PC,           &
            UTLUCODE_DX_READ_PPV,           &
            UTLUCODE_DX_WRITE_PREDS,        &
            UTLUCODE_PAR_OMIT,              &
            UTLUCODE_PAR_OMIT_REACT1,       &
            UTLUCODE_PRI_OMIT,              &
            UTLUCODE_DX_WRITE_RESID,        &
            UTLUCODE_DX_WRITE_SEN,          &
            UTLUCODE_DX_READ_SPU,           &
            UTLUCODE_DX_WRITE_SU,           &
            UTLUCODE_DX_READ_SVD,           &
            UTLUCODE_DX_WRITE_SVD,          &
            UTLUCODE_DX_WRITE_INTWR,        &
            UTLUCODE_DX_READ_WW,            &
            UTLUCODE_DX_WRITE_MATRIX,       &
            UTLUCODE_DX_WRITE_MATRIXNL,     &
            UTLUCODE_DX_WRITE_MATRIXNLOBS,  &
            UTLUCODE_DX_WRITE_COLS,         &
            UTLUCODE_SHELLSORT_VALUE,       &
            UTLUCODE_SHELLSORT_INTEGERS,     &
            UTLUCODE_SVD,                   &
            UTLUCODE_SVDINV,                &
            UTLUCODE_SVD_PARCHK,            &
            UTLUCODE_INVERT,                &
            UTLUCODE_SHELLSORTMANY,         &
            UTLUCODE_SHELLSORTTHREE
  !
  CONTAINS
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_CHECK_PRI(IOUT,MPR,MPRWOP,NPS,NPSWOP,PADJ,PARNAMLC, &
                                WRPRIEQ)
    ! CHECK THAT PARAMETERS WITH LN HAVE EQTN WITH LOG10 NOT LOG
    USE GLOBAL_DATA, ONLY: LENDNAM
    USE PRIOR_INFORMATION, ONLY: LLPTRPRIOR, LLPTRPRIORFP
    USE UTILITIES
    ! argument variables
    IMPLICIT NONE
    INTEGER,                                INTENT(IN) :: IOUT
    INTEGER,                                INTENT(IN) :: MPR
    INTEGER,                                INTENT(IN) :: MPRWOP
    INTEGER,                                INTENT(IN) :: NPS
    INTEGER,                                INTENT(IN) :: NPSWOP
    LOGICAL, DIMENSION(NPS),                INTENT(IN) :: PADJ
    CHARACTER(LEN=12), DIMENSION(NPS),      INTENT(IN) :: PARNAMLC
    LOGICAL,                                INTENT(IN) :: WRPRIEQ
    ! local variables
    INTEGER                        :: I
    INTEGER                        :: IEQ
    INTEGER                        :: IFAIL
    INTEGER, DIMENSION(NPS-NPSWOP) :: IPRIOR
    CHARACTER(LEN=MAX_STRING_LEN)  :: PRIEQNTXT
    CHARACTER(LEN=LENDNAM)         :: PRINAM
    CHARACTER(LEN=12)              :: TXTNAME
    ! formats
    10 FORMAT(/,80('@'),/)
    12 FORMAT(2X,'THERE ARE NO PRIOR EQUATIONS FOR THIS EVALUATION')
    15 FORMAT(2X,'PRIOR EQUATIONS',//,2X,'# of PRIOR EQUATIONS = ',I,/)
    20 FORMAT(3X,'NAME',23X,'EQUATION',/)
    30 FORMAT(3X,A20,7X,A)
    IPRIOR = 0
    PRIEQNTXT = ' '
    PRINAM = ' '
    IF(NPS-NPSWOP /= MPR-MPRWOP) THEN
      WRITE(IOUT,*)
      WRITE(IOUT,*) &
      'PRIOR INFORMATION MUST BE SPECIFIED ON PARAMETERS FOR PREDICTION'
      WRITE(IOUT,*)
    ENDIF
    IF(MPR > MPRWOP) THEN
      DO IEQ=1,MPR-MPRWOP
        CALL UTL_FILTER(IFAIL,LLPTRPRIORFP,IOUT,'EQUATION',PRIEQNTXT,IEQ)
        TXTNAME=TRIM(PRIEQNTXT(1:12))
        IF(PRIEQNTXT(1:6) == 'log10(') THEN
          TXTNAME=' '
          DO I=7,18
            IF(PRIEQNTXT(I:I) /= '(') THEN
              IF(PRIEQNTXT(I:I) /= ')')TXTNAME(I-6:I-6)=PRIEQNTXT(I:I)
            ELSE
              EXIT
            ENDIF
          ENDDO
        ENDIF
        CALL UTL_CASE(TXTNAME,TXTNAME,-1)
        IF(NPS <= NPSWOP) THEN
          WRITE(IOUT,*)
          WRITE(IOUT,*)' THERE ARE NO NEW PARAMETERS DEFINED FOR PREDICTION'
          WRITE(IOUT,*)' DO NOT USE LINEAR_PRIOR_INFORMATION_FOR_PREDICTION'
          WRITE(*,*)
          WRITE(*,*)' THERE ARE NO NEW PARAMETERS DEFINED FOR PREDICTION'
          WRITE(*,*)' DO NOT USE LINEAR_PRIOR_INFORMATION_FOR_PREDICTION'
          CALL UTL_STOP('TERMINATING : Evaluate Input')
        ENDIF
        DO I=NPSWOP+1,NPS
          IF(UTL_SAMENAME(TRIM(TXTNAME),TRIM(PARNAMLC(I)))) THEN
            IPRIOR(I-NPSWOP) = 1
            EXIT
          ENDIF
          IF(I == NPS) THEN
            WRITE(IOUT,*)
            WRITE(IOUT,*) &
            ' PARAMETERS in LINEAR_PRIOR_INFORMATION_FOR_PREDICTION EQUATIONS'
            WRITE(IOUT,*) &
            ' MUST APPEAR in PARAMETER_DATA_FOR_PREDICTION, PLEASE CHECK:'
            WRITE(IOUT,*)TRIM(TXTNAME)
            WRITE(*,*)
            WRITE(*,*) &
            ' PARAMETERS in LINEAR_PRIOR_INFORMATION_FOR_PREDICTION EQUATIONS'
            WRITE(*,*) &
            ' MUST APPEAR in PARAMETER_DATA_FOR_PREDICTION, PLEASE CHECK:'
            WRITE(*,*)TRIM(TXTNAME)
            CALL UTL_STOP &
            ('ILLEGAL PARAMETER in LINEAR_PRIOR_INFORMATION_FOR_PREDICTION')
          ENDIF
        ENDDO
      ENDDO
      DO I=1,NPS-NPSWOP
        IF(IPRIOR(I) == 1) CYCLE
        IF(PADJ(I+NPSWOP)) THEN
          WRITE(IOUT,*)
          WRITE(IOUT,*) &
          ' ADJUSTABLE PARAMETERS in PARAMETER_DATA_FOR_PREDICTION REQUIRE'
          WRITE(IOUT,*) &
          ' LINEAR_PRIOR_INFORMATION_FOR_PREDICTION, PLEASE CHECK:'
          WRITE(IOUT,*)
          WRITE(IOUT,*)TRIM(PARNAMLC(I+NPSWOP))
          WRITE(IOUT,*)
          WRITE(*,*)
          WRITE(*,*) &
          ' ADJUSTABLE PARAMETERS in PARAMETER_DATA_FOR_PREDICTION REQUIRE'
          WRITE(*,*) &
          ' LINEAR_PRIOR_INFORMATION_FOR_PREDICTION, PLEASE CHECK:'
          WRITE(*,*)
          WRITE(*,*)TRIM(PARNAMLC(I+NPSWOP))
          WRITE(*,*)
          CALL UTL_STOP &
          ('MISSING information in LINEAR_PRIOR_INFORMATION_FOR_PREDICTION')
        ENDIF
      ENDDO
    ENDIF
    IF(MPR > 0) THEN
      IF(WRPRIEQ) THEN
        WRITE(IOUT,10)
        WRITE(IOUT,15)MPR
        WRITE(IOUT,20)
      ENDIF
    ELSE
      WRITE(IOUT,10)
      WRITE(IOUT,12)
    ENDIF
    DO IEQ=1,MPR
      IF(IEQ <= MPRWOP) THEN
        CALL UTL_FILTER(IFAIL,LLPTRPRIOR,IOUT,'EQUATION',PRIEQNTXT,IEQ)
        CALL UTL_FILTER(IFAIL,LLPTRPRIOR,IOUT,'PRIORNAME',PRINAM,IEQ)
      ELSE
        CALL UTL_FILTER(IFAIL,LLPTRPRIORFP,IOUT,'EQUATION',PRIEQNTXT,IEQ-MPRWOP)
        CALL UTL_FILTER(IFAIL,LLPTRPRIORFP,IOUT,'PRIORNAME',PRINAM,IEQ-MPRWOP)
      ENDIF
      IF(WRPRIEQ) WRITE(IOUT,30)PRINAM,TRIM(PRIEQNTXT)
      ![ check EQTEXT for presence of 'log('
      !-- Use UTL_SAMENAME to make the comparison case-insensitive?
      !Don't know if EQN accepts LOG as opposed to log -John likes lower case. ]
      CALL UTL_CASE(PRIEQNTXT(1:6),PRIEQNTXT(1:6),-1)
!@      IF(PRIEQNTXT(1:1) /= 'l') CYCLE
!@      IF(PRIEQNTXT(2:2) /= 'o') CYCLE
!@      IF(PRIEQNTXT(3:3) /= 'g') CYCLE
!@      IF(PRIEQNTXT(4:4) /= '(') THEN
!@        IF(PRIEQNTXT(4:4) /= '1') CYCLE
!@      ENDIF
!@      IF(PRIEQNTXT(5:5) /= '0') CYCLE
!@      IF(PRIEQNTXT(6:6) /= '(') CYCLE
!@      AMESSAGE = &
!@      ' UCODE EXPECTS NATIVE PARAMETER VALUES FOR TRANSFORMED PARAMETERS'
!@      CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
!@      AMESSAGE = ' UCODE will make the necessary conversions '
!@      CALL UTL_WRITE_MESSAGE(IOUT,'yes','no','yes')
!@      AMESSAGE = ' problem found in equation: '//TRIM(PRIEQNTXT)
!@      CALL UTL_WRITE_MESSAGE(IOUT,'yes','no','yes')
!@      CALL UTL_STOP('Express Prior Information in native space')
    ENDDO
    IF(WRPRIEQ) WRITE(IOUT,10)
    RETURN
  END SUBROUTINE UTLUCODE_CHECK_PRI
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_COPY_FILE(IOUT,FNIN,FNCOPY)
    !   make a file copy
    USE GLOBAL_DATA, ONLY: AMESSAGE, MAX_STRING_LEN, NPPREC
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                       INTENT(IN) :: IOUT
    CHARACTER(LEN=MAX_STRING_LEN), INTENT(IN) :: FNIN
    CHARACTER(LEN=MAX_STRING_LEN), INTENT(IN) :: FNCOPY
    ! local
    INTEGER :: IUNIT1
    INTEGER :: IUNIT2
    INTEGER :: MAXRECL
    LOGICAL :: LEX = .FALSE.
    CHARACTER(LEN=MAX_STRING_LEN) :: FMT_STRING
    CHARACTER(LEN=MAX_STRING_LEN) :: FMT_STRING_COPY
    CHARACTER(LEN=MAX_STRING_LEN) :: LINE
    CHARACTER(LEN=10)  :: STR_MAX
! create format
    WRITE(STR_MAX,'(I10)') MAX_STRING_LEN
    STR_MAX = ADJUSTL(STR_MAX)
    FMT_STRING = '(A'//TRIM(STR_MAX)//')'
    FMT_STRING_COPY = '(A)'
    MAXRECL = 500 + NPPREC*26
! open file to read
    INQUIRE(FILE=FNIN,EXIST=LEX)
    IF(LEX) THEN
      IUNIT1 = UTL_GETUNIT(101,150)
      OPEN(UNIT=IUNIT1,FILE=FNIN,RECL=MAXRECL,STATUS='OLD')
      ! open file to write
      IUNIT2 = UTL_GETUNIT(101,150)
      OPEN(UNIT=IUNIT2,FILE=FNCOPY,RECL=MAXRECL,STATUS='REPLACE')
      ! read and write until done
      DO
        READ(IUNIT1,FMT_STRING,END=30) LINE
        LINE = ADJUSTL(LINE)
        WRITE(IUNIT2,FMT_STRING_COPY)TRIM(LINE)
      ENDDO
      30 CONTINUE
      CLOSE(IUNIT1)
      CLOSE(IUNIT2)
    ELSE
      WRITE(IOUT,*)' THE FILE LISTED BELOW DOES NOT EXIST'
      WRITE(IOUT,*)TRIM(FNIN)
      WRITE(IOUT,*) &
      ' THE SELECTED SVDphase/SVDstartpars COMBINATION MAY NOT BE APPROPRIATE'
      WRITE(*,*)' THE FILE LISTED BELOW DOES NOT EXIST'
      WRITE(*,*)TRIM(FNIN)
      WRITE(*,*) &
      ' THE SELECTED SVDphase/SVDstartpars COMBINATION MAY NOT BE APPROPRIATE'
    ENDIF
    RETURN
  END SUBROUTINE UTLUCODE_COPY_FILE
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_LOG_PRI(PRIEQNTXT,ILOG)
    ! DETERMINE IF PRIOR IS LOG TRANSFORMED
    ! argument variables
    IMPLICIT NONE
    CHARACTER(LEN=MAX_STRING_LEN),          INTENT(IN) :: PRIEQNTXT
    INTEGER,                                INTENT(OUT):: ILOG
    ILOG = 0
    DO
      IF(PRIEQNTXT(1:1) /= 'l') CYCLE
      IF(PRIEQNTXT(2:2) /= 'o') CYCLE
      IF(PRIEQNTXT(3:3) /= 'g') CYCLE
      IF(PRIEQNTXT(4:4) /= '1') CYCLE
      IF(PRIEQNTXT(5:5) /= '0') CYCLE
      IF(PRIEQNTXT(6:6) /= '(') RETURN
      ILOG = 1
      EXIT
    ENDDO
    RETURN
  END SUBROUTINE UTLUCODE_LOG_PRI
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_WRITE_B1(MPR,NOBS,NPE,NPS,CFR,C,IOUT,IPTR, &
                                 LN,OUTNAM,PARNAM,PVAL,EXT)
    !   Write parameter values to either _b1 or _b1adv (depending on value of
    !   CFR) to calculate Beale's measure
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN, NPPREC
    USE UTILITIES
    !
    !   Argument-list variables
    IMPLICIT NONE
    INTEGER,                                INTENT(IN) :: MPR
    INTEGER,                                INTENT(IN) :: NOBS
    INTEGER,                                INTENT(IN) :: NPE
    INTEGER,                                INTENT(IN) :: NPS
    DOUBLE PRECISION,                       INTENT(IN) :: CFR
    DOUBLE PRECISION,   DIMENSION(NPE,NPE), INTENT(IN) :: C
    INTEGER,                                INTENT(IN) :: IOUT
    INTEGER,            DIMENSION(NPE),     INTENT(IN) :: IPTR
    INTEGER,            DIMENSION(NPS),     INTENT(IN) :: LN
    CHARACTER(LEN=MAX_STRING_LEN),          INTENT(IN) :: OUTNAM
    CHARACTER(LEN=12),  DIMENSION(NPS),     INTENT(IN) :: PARNAM
    DOUBLE PRECISION,   DIMENSION(NPS),     INTENT(INOUT) :: PVAL
    CHARACTER(LEN=10), OPTIONAL,            INTENT(IN) :: EXT
    !
    !   Local variables
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: BUFF
    DOUBLE PRECISION                              :: FSTAT
    INTEGER                                       :: I
    INTEGER                                       :: ICNT
    INTEGER                                       :: IDOF
    INTEGER                                       :: IIPP
    INTEGER                                       :: IP
    INTEGER                                       :: IS
    INTEGER                                       :: IUB1
    INTEGER                                       :: J
    INTEGER                                       :: MXRECL
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: ROWVALS
    DOUBLE PRECISION                              :: SB
    DOUBLE PRECISION                              :: TEMP
    !
    !   Format statements
    800 FORMAT(1X,'WARNING: In UTLUCODE_DX_WRITE_B1, unable to',   &
               ' open an output file')
    900 FORMAT(/,   &
    1X,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',/,  &
    1X,' Warning: Cannot write the _b1 file, likely due to very large  ',/,   &
    1X,'  parameter confidence intervals. When composite scaled        ',/,   &
    1X,'  sensitivities are very small and/or parameters are           ',/,   &
    1X,'  extremely correlated (parameter correlation coefficients     ',/,   &
    1X,'  close to 1 or -1) confidence interval limits can become so   ',/,   &
    1X,'  large that the values calculated using them exceed the       ',/,   &
    1X,'  capacity of some computers. The rest of the ucode            ',/,   &
    1X,'  calculations are correct but the extreme intervals may be due',/,   &
    1X,'  to a poorly posed regression problem. Alternatively this may ',/,   &
    1X,'  be a result of evaluating sensitivities for initial parameter',/,   &
    1X,'  values and the situation will improve after optimization.',/,   &
    1X,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',/)
    !
    ! ALLOCATE
    ALLOCATE(BUFF(NPS),ROWVALS(NPS,NPE*2))
    !
    ! Open Exchange File
    MXRECL = 1+NPPREC*26
    IF(CFR >= 0.D0) THEN
      IUB1 = UTL_DX_OPEN(OUTNAM,EXT,'REPLACE',MXRECL)
      IF (IUB1 < 1) THEN
        WRITE(IOUT,800)
        CALL UTL_STOP('Cannot open _b1adv')
      ENDIF
    ELSE
      IUB1 = UTL_DX_OPEN(OUTNAM,'_b1','REPLACE',MXRECL)
      IF (IUB1 < 1) THEN
        WRITE(IOUT,800)
        CALL UTL_STOP('Cannot open _b1')
      ENDIF
    ENDIF
    BUFF = PVAL
    TEMP = 0.D0
    SB = 0.D0
    !
    ! CALCULATE DEGREES OF FREEDOM
    IDOF = NOBS + MPR - NPE
    IF(CFR >= 0.D0) THEN
      !   CALCULATE PARAMETER SETS
      TEMP = (REAL(NPE)*CFR)**.5D0
    ELSE
      ! GET F statistic
      CALL UTL_FSTT (NPE,IDOF,FSTAT)
      !   CALCULATE PARAMETER SETS
      TEMP = (REAL(NPE)*FSTAT)**.5D0
    ENDIF
    ICNT = 0
    DO IP = 1, NPE
      SB = C(IP,IP)**.5D0
      DO IS = -1, 1, 2
        DO I = 1, NPE
          IIPP = IPTR(I)
          IF(CFR >= 0.D0 .AND. LN(IIPP) > 0) PVAL(IIPP) = DLOG(PVAL(IIPP))
          BUFF(IIPP) = PVAL(IIPP) + REAL(IS)*(TEMP/SB)*C(IP,I)
          IF(CFR >= 0.D0 .AND. LN(IIPP) > 0) THEN
            IF (BUFF(IPTR(I)) > 88.D0) THEN
              WRITE(IOUT,900)
              IUB1 = UTL_DX_CLOSE(EXT)
              DEALLOCATE(BUFF,ROWVALS)
              RETURN
            ELSE
              PVAL(IIPP) = EXP(PVAL(IIPP))
            ENDIF
          ENDIF
          IF (LN(IPTR(I)) .NE. 0) THEN
            IF(BUFF(IPTR(I)) < 88.D0) THEN
              BUFF(IPTR(I)) = EXP(BUFF(IPTR(I)))
            ELSE
              WRITE (IOUT,900)
              IF(CFR >= 0.D0) THEN
                IUB1 = UTL_DX_CLOSE(EXT)
              ELSE
                IUB1 = UTL_DX_CLOSE('_b1')
              ENDIF
              DEALLOCATE(BUFF,ROWVALS)
              RETURN
            ENDIF
          ENDIF
        ENDDO
        ICNT = ICNT + 1
        DO J = 1,NPS
          ROWVALS(J,ICNT) = BUFF(J)
        ENDDO
      ENDDO
    ENDDO
    CALL UTLUCODE_DX_WRITE_MATRIXNL(IUB1,ICNT,NPS,PARNAM,ROWVALS)
    !
    ! Close data exchange file
    IF(CFR >= 0.D0) THEN
      IUB1 = UTL_DX_CLOSE(EXT)
    ELSE
      IUB1 = UTL_DX_CLOSE('_b1')
    ENDIF
    DEALLOCATE(BUFF,ROWVALS)
    RETURN
  END SUBROUTINE UTLUCODE_DX_WRITE_B1
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_WRITE_B3(NOINT,NPE,NPS,C,CFI,EXT,IOUT,IPTR,ITYP,LN, &
                                 OUTNAM,PARNAM,PVAL,WB,Z)
    !   Write parameter values to _b3 to calculate Advanced Beale's measure
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN, NPPREC
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                INTENT(IN) :: NOINT
    INTEGER,                                INTENT(IN) :: NPE
    INTEGER,                                INTENT(IN) :: NPS
    DOUBLE PRECISION,  DIMENSION(NPE,NPE),  INTENT(IN) :: C
    DOUBLE PRECISION,  DIMENSION(NOINT),    INTENT(IN) :: CFI
    CHARACTER(LEN=7),                       INTENT(IN) :: EXT
    INTEGER,                                INTENT(IN) :: IOUT
    INTEGER,           DIMENSION(NPE),      INTENT(IN) :: IPTR
    INTEGER,                                INTENT(IN) :: ITYP
    INTEGER,           DIMENSION(NPS),      INTENT(IN) :: LN
    CHARACTER(LEN=MAX_STRING_LEN),          INTENT(IN) :: OUTNAM
    CHARACTER(LEN=12), DIMENSION(NPS),      INTENT(IN) :: PARNAM
    DOUBLE PRECISION,  DIMENSION(NPS),      INTENT(IN) :: PVAL
    DOUBLE PRECISION,  DIMENSION(NOINT),    INTENT(IN) :: WB
    DOUBLE PRECISION,  DIMENSION(NPE,NOINT),INTENT(IN) :: Z(NPE,NOINT)
    !
    !   Local variables
    DOUBLE PRECISION                              :: B
    DOUBLE PRECISION                              :: TEMP
    DOUBLE PRECISION                              :: VG
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: BUFB
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: BUFF
    INTEGER                                       :: I
    INTEGER                                       :: ICNT
    INTEGER                                       :: IINT
    INTEGER                                       :: IIPP
    INTEGER                                       :: IP
    INTEGER                                       :: IS
    INTEGER                                       :: IUB3
    INTEGER                                       :: J
    INTEGER                                       :: MXRECL
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: ROWVALS
    CHARACTER(LEN=20)                             :: STRING = ' '
    !
    !   Format statements
    800 FORMAT(1X,'WARNING: In UTLUCODE_DX_WRITE_B3, unable to',   &
               ' open an output file')
    900 FORMAT(/   &
        1X,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',   &
        1X,'Error: Failed in UTLUCODE_DX_WRITE_B3; cannot take exp(x>88)',   &
        1X,'         Consequently, _b3 file is not written',   &
        1X,'         Evaluate the regression output',   &
        1X,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',/)
    !
    !ALLOCATE
    ALLOCATE(BUFB(NPS),BUFF(NPS),ROWVALS(NPS,NOINT*2))
    !
    ! INITIALIZE
    BUFF = PVAL
    ROWVALS = 0.D0
    MXRECL = 1+NPPREC*26
    !   CALCULATE PARAMETER SETS
    ICNT = 0
    DO IINT=1,NOINT
      DO IP=1,NPE
        IIPP=IPTR(IP)
        BUFB(IIPP)=0.D0
        DO J=1,NPE
          BUFB(IIPP)=BUFB(IIPP)+C(IP,J)*Z(J,IINT)
        ENDDO
      ENDDO
      VG = 0.D0
      DO IP=1,NPE
        VG=VG+BUFB(IPTR(IP))*Z(IP,IINT)
      ENDDO
      ! add std dev of meas err, WB was passed in as variance
      IF (ITYP.EQ.2) VG=VG+SQRT(WB(IINT))
      TEMP=(CFI(IINT)/VG)**.5D0
      DO IS=-1,1,2
        DO IP=1, NPE
          IIPP=IPTR(IP)
          B=PVAL(IIPP)
          IF (LN(IIPP).GT.0) B=DLOG(B)
          BUFF(IIPP)=B+REAL(IS)*TEMP*BUFB(IIPP)
          IF (LN(IIPP).GT.0) THEN
            IF (BUFF(IIPP) > 88) THEN
              WRITE(IOUT,900)
              DEALLOCATE(BUFB,BUFF,ROWVALS)
              RETURN
            ENDIF
            BUFF(IIPP)=EXP(BUFF(IIPP))
          ENDIF
        ENDDO
        ICNT = ICNT +1
        DO I=1,NPS
          ROWVALS(I,ICNT) = BUFF(I)
        ENDDO
      ENDDO
    ENDDO
    ! Open Exchange File
    IUB3 = UTL_DX_OPEN(OUTNAM,EXT,'REPLACE',MXRECL)
    IF (IUB3 < 1) THEN
      WRITE(IOUT,800)
      STRING = 'Cannot open '//EXT
      CALL UTL_STOP(STRING)
    ENDIF
    CALL UTLUCODE_DX_WRITE_MATRIXNL(IUB3,ICNT,NPS,PARNAM,ROWVALS)
    ! Close data exchange file
    IUB3 = UTL_DX_CLOSE(EXT)
    DEALLOCATE(BUFB,BUFF,ROWVALS)
    RETURN
  END SUBROUTINE UTLUCODE_DX_WRITE_B3
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_READ_B(NROW,NPT,IU,PNAM,PVALS)
    !   READ an _b parameter Data-Exchange File
    USE GLOBAL_DATA, ONLY: AMESSAGE, MAX_STRING_LEN, BIGINTEGER
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN)  :: NROW
    INTEGER,                                 INTENT(IN)  :: NPT
    INTEGER,                                 INTENT(IN)  :: IU
    CHARACTER(LEN=12),      DIMENSION(NPT),  INTENT(OUT) :: PNAM
    DOUBLE PRECISION,   DIMENSION(NPT,NROW), INTENT(OUT) :: PVALS
    !
    !   Local variables
    CHARACTER(LEN=1) :: CH
    INTEGER :: I, IP1, IP2, J, KB, KC, KP, KQ, MRECL, MREM, NPREC1
    !
    IF (NROW <= 0) THEN
      AMESSAGE = 'Error encountered in UTLUCODE_DX_READ_B: NUMBER OF'//   &
                 ' INTERVALS = 0, NOTHING TO DO'
      CALL UTL_STOP()
    ENDIF
    !   Read header one character at a time to determine number of parameters
    !   per record
    MRECL = BIGINTEGER
    KQ = 0
    DO KC=1,MRECL
      READ(IU,'(A1)',ADVANCE='NO',EOR=50) CH
      IF (CH=='"') KQ = KQ+1
    ENDDO
    50 CONTINUE
    NPREC1 = KQ/2
    MREM = MOD(KQ,2)
    IF (MREM .NE. 0) THEN
      AMESSAGE = 'Programming error: UTLUCODE_DX_READ_B'//   &
                 ' finds odd number of quotes in _b1 _b1adv or _b3 file'
      CALL UTL_STOP()
    ENDIF
    REWIND(IU)
    !
    !  Read  parameter names
    KB = 0
    KP = 0
    BLOK: DO WHILE (KP<NPT)
      IP1 = KB*NPREC1+1
      KB = KB+1
      IP2 = KB*NPREC1
      IF (IP2>NPT) IP2 = NPT
      READ(IU,*,END=100)(PNAM(I),I=IP1,IP2)
      DO I = 1, NROW
        READ(IU,*,END=100)(PVALS(J,I),J=IP1,IP2)
      ENDDO
      KP = IP2
    ENDDO BLOK
    !
    RETURN
    100 CONTINUE
    AMESSAGE = 'Error encountered in UTLUCODE_DX_READ_B: Premature end'//   &
               ' of _b1 _b1adv or _b3 file'
    CALL UTL_STOP()
    RETURN
  END SUBROUTINE UTLUCODE_DX_READ_B
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_READ_BOBS(NROW,NCOL,IU,ONAM,OVALS)
    !   READ  _b2 _b2adv or _b4 File: Simulated equivalents for linearity
    USE GLOBAL_DATA, ONLY: AMESSAGE, LENDNAM, MAX_STRING_LEN, BIGINTEGER
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN)  :: NROW
    INTEGER,                                 INTENT(IN)  :: NCOL
    INTEGER,                                 INTENT(IN)  :: IU
    CHARACTER(LEN=LENDNAM), DIMENSION(NCOL), INTENT(OUT) :: ONAM
    DOUBLE PRECISION,  DIMENSION(NCOL,NROW), INTENT(OUT) :: OVALS
    !
    !   Local variables
    CHARACTER(LEN=1) :: CH
    INTEGER :: I, IP1, IP2, J, KB, KC, KP, KQ, MRECL, MREM, NPREC1
    !
    IF (NROW <= 0) THEN
      AMESSAGE = 'Error encountered in UTLUCODE_DX_READ_BOBS: NUMBER OF'//   &
                 ' INTERVALS = 0, NOTHING TO DO'
      CALL UTL_STOP()
    ENDIF
    !   Read header one character at a time to determine number of parameters
    !   per record
    MRECL = BIGINTEGER
    KQ = 0
    DO KC=1,MRECL
      READ(IU,'(A1)',ADVANCE='NO',EOR=50) CH
      IF (CH=='"') KQ = KQ+1
    ENDDO
    50 CONTINUE
    NPREC1 = KQ/2
    MREM = MOD(KQ,2)
    IF (MREM .NE. 0) THEN
      AMESSAGE = 'Programming error: UTLUCODE_DX_READ_BOBS'//   &
                 ' finds odd number of quotes in _b2 _b2adv or _b4 file'
      CALL UTL_STOP()
    ENDIF
    REWIND(IU)
    !
    !  Read  parameter names
    KB = 0
    KP = 0
    BLOK: DO WHILE (KP<NCOL)
      IP1 = KB*NPREC1+1
      KB = KB+1
      IP2 = KB*NPREC1
      IF (IP2>NCOL) IP2 = NCOL
      READ(IU,*,END=100)(ONAM(I),I=IP1,IP2)
      DO I = 1, NROW
        READ(IU,*,END=100)(OVALS(J,I),J=IP1,IP2)
      ENDDO
      KP = IP2
    ENDDO BLOK
    !
    RETURN
    100 CONTINUE
    AMESSAGE = 'Error encountered in UTLUCODE_DX_READ_BOBS: Premature end'//  &
               ' of _b2 _b2adv or _b4 file'
    CALL UTL_STOP()
    RETURN
  END SUBROUTINE UTLUCODE_DX_READ_BOBS
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_READ_CFSU(NDEP,NDEPORIG,NPE,NOPTR,OUTNAMPRED,OBSNAM, &
                                   PARNAM,ID,XSENS)
    !   READ _CFSU file: Unscaled Sensitivities for Correction factors
    USE GLOBAL_DATA, ONLY: AMESSAGE, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN)  :: NDEP
    INTEGER,                                 INTENT(IN)  :: NDEPORIG
    INTEGER,                                 INTENT(IN)  :: NPE
    INTEGER,            DIMENSION(NDEPORIG), INTENT(IN)  :: NOPTR
    CHARACTER(LEN=MAX_STRING_LEN),           INTENT(IN)  :: OUTNAMPRED
    CHARACTER(LEN=LENDNAM), DIMENSION(NDEP), INTENT(OUT) :: OBSNAM
    CHARACTER(LEN=12),      DIMENSION(NPE),  INTENT(OUT) :: PARNAM
    INTEGER,                DIMENSION(NDEP), INTENT(OUT) :: ID
    DOUBLE PRECISION,   DIMENSION(NPE,NDEP), INTENT(OUT) :: XSENS
    !
    !   Local variables
    CHARACTER(LEN=20) :: CDUM
    CHARACTER(LEN=1) :: CH
    INTEGER :: I, IP1, IP2, IUSU, J, K, KB, KC, KP, KQ, MRECL, MREM, NPREC1
    !
    IF (NDEP <= 0) THEN
      AMESSAGE = 'Error encountered in UTLUCODE_DX_READ_CFSU: NUMBER OF'//   &
                 ' INTERVALS = 0, NOTHING TO DO'
      CALL UTL_STOP()
    ENDIF
    !
    !   Read header one character at a time to determine number of parameters
    !   per record
    MRECL = NPE*30+40
    ! Open data exchange file
    IUSU = UTL_DX_OPEN(OUTNAMPRED,'_cfsu','OLD',MRECL)
    KQ = 0
    DO KC=1,MRECL
      READ(IUSU,'(A1)',ADVANCE='NO',EOR=50) CH
      IF (CH=='"') KQ = KQ+1
    ENDDO
    50 CONTINUE
    NPREC1 = KQ/2 - 2
    MREM = MOD(KQ,2)
    IF (MREM .NE. 0) THEN
      AMESSAGE = 'Programming error: UTLUCODE_DX_READ_CFSU'//   &
                 ' finds odd number of quotes in _cfsu file'
      CALL UTL_STOP()
    ENDIF
    REWIND(IUSU)
    !
    KB = 0
    KP = 0
    BLOK: DO WHILE (KP<NPE)
      IP1 = KB*NPREC1+1
      KB = KB+1
      IP2 = KB*NPREC1
      IF (IP2>NPE) IP2 = NPE
      READ(IUSU,*,END=100)CDUM,CDUM,(PARNAM(I),I=IP1,IP2)
      I = 0
      DO K = 1, NDEPORIG
        IF(NOPTR(K) > 0) THEN
          I = I + 1
          READ(IUSU,*,END=100)OBSNAM(I),ID(I),(XSENS(J,I),J=IP1,IP2)
        ELSE
          READ(IUSU,*,END=100)
        ENDIF
      ENDDO
      KP = IP2
    ENDDO BLOK
    !
    ! Close data exchange file
    IUSU = UTL_DX_CLOSE('_cfsu')
    RETURN
    100 CONTINUE
    AMESSAGE = 'Error encountered in UTLUCODE_DX_READ_CFSU: Premature end'//   &
               ' of _cfsu file'
    CALL UTL_STOP()
    RETURN
  END SUBROUTINE UTLUCODE_DX_READ_CFSU
  !-----------------------------------------------------------------------------
  SUBROUTINE UTLUCODE_CHISQ(IDOF,CHISQL,CHISQU)
    ! DETERMINE CHI SQUARED FOR N DEGREES OF FREEDOM
    ! FOR A TWO-SIDED SIGNIFICANCE LEVEL OF 0.05
    !
    !   Argument-list variables
    IMPLICIT NONE
    INTEGER,          INTENT(IN)  :: IDOF
    DOUBLE PRECISION, INTENT(OUT) :: CHISQL
    DOUBLE PRECISION, INTENT(OUT) :: CHISQU
    !
    !   Local variables
    DOUBLE PRECISION, DIMENSION(52) :: TABLE975
    DOUBLE PRECISION, DIMENSION(52) :: TABLE025
    INTEGER, DIMENSION(52) :: ITABLE
    INTEGER :: I
    !
    !     ------------------------------------------------------------------
    DATA (ITABLE(I),I=1,52)/1, 2, 3, 4, 5, &
           6,  7,  8,  9, 10, 11, 12,    &
          13, 14, 15, 16, 17, 18, 19,    &
          20, 21, 22, 23, 24, 25, 26,    &
          27, 28, 29, 30, 40, 50, 60,    &
          70, 80, 90, 100, 120, 240,     &
          500,1000,2500,5000,10000,     &
          25000,50000,100000,250000,    &
          500000,1000000,2500000,5000000/
    DATA (TABLE975(I),I=1,52)/ &
                    0.000982D0, 0.05064D0, 0.2158D0, 0.4844D0, 0.8312D0,   &
          1.237D0, 1.690D0, 2.180D0, 2.700D0, 3.247D0, 3.816D0, 4.404D0,   &
          5.009D0, 5.629D0, 6.262D0, 6.908D0, 7.564D0, 8.231D0, 8.907D0,   &
          9.591D0, 10.28D0, 10.98D0, 11.69D0, 12.40D0, 13.12D0, 13.84D0,   &
          14.57D0, 15.31D0, 16.06D0, 16.79D0, 24.43D0, 32.36D0, 40.48D0,   &
          48.76D0, 57.15D0, 65.65D0, 74.22D0, 91.57D0, 198.98D0,           &
          439.90D0,914.30D0,2363.00D0,4806.00D0,9725.00D0,                 &
          24564.00D0,49382.00D0,99125.00D0,248616.00D0,                    &
          498042.00D0,997230.00D0,2495619.00D0,4993804.00D0/
    DATA (TABLE025(I),I=1,52)/5.024D0, 7.378D0, 9.348D0, 11.14D0, 12.83D0,   &
          14.45D0, 16.01D0, 17.53D0, 19.02D0, 20.48D0, 21.92D0, 23.34D0,   &
          24.74D0, 26.12D0, 27.49D0, 28.85D0, 30.19D0, 31.53D0, 32.85D0,   &
          34.17D0, 35.48D0, 36.78D0, 38.08D0, 39.36D0, 40.65D0, 41.92D0,   &
          43.19D0, 44.46D0, 45.72D0, 46.98D0, 59.34D0, 71.42D0, 83.30D0,   &
          95.02D0, 106.63D0, 118.14D0, 129.56D0, 152.21D0, 284.80D0,       &
          563.90D0,1090.00D0,2640.00D0,5198.00D0,10279.00D0,               &
          25440.00D0,50622.00D0,100878.00D0,251388.00D0,                   &
          501962.00D0,1002774.00D0,2504385.00D0,5006200.00D0/
    !     ------------------------------------------------------------------
    !
    IF (IDOF .LE. 30) THEN
      CHISQU=TABLE975(IDOF)
      CHISQL=TABLE025(IDOF)
      RETURN
    ENDIF
    DO I=31,52
      IF(IDOF .LE. ITABLE(I)) THEN
        CHISQU = TABLE975(I-1)+(TABLE975(I)-TABLE975(I-1))*                &
             DBLE(IDOF-ITABLE(I-1))/DBLE(ITABLE(I)-ITABLE(I-1))
        CHISQL = TABLE025(I-1)+(TABLE025(I)-TABLE025(I-1))*                &
             DBLE(IDOF-ITABLE(I-1))/DBLE(ITABLE(I)-ITABLE(I-1))
        RETURN
      ENDIF
    ENDDO
    CHISQU=TABLE975(52)
    CHISQL=TABLE025(52)
    RETURN
  END SUBROUTINE UTLUCODE_CHISQ
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_READ_DM(IFAIL,OUTNAM,AIC,BIC,DTLANP, &
                           HQ,ICONVERGE,ITERP,KASHYAPNP,MLOFD, &
                           MLOFDP,MODELLENGTH,MODELMASS,MODELNAME,MODELTIME, &
                           MPR,NDINC,NPE,NPERD,NPS, &
                           NOBS,STAT1,STAT2,STDERR,VAR, &
                           IOUT,DTLA,KASHYAP,SENTYPE,NPLOWEST, &
                           XTWXNP,XTWXP,EXT)
    !   WRITE _DM DEX FILE Model Fit and Parsimony Data
    USE GLOBAL_DATA, ONLY: IVERB, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                       INTENT(INOUT) :: IFAIL
    CHARACTER(LEN=MAX_STRING_LEN), INTENT(IN)    :: OUTNAM
    DOUBLE PRECISION,              INTENT(OUT)   :: AIC
    DOUBLE PRECISION,              INTENT(OUT)   :: BIC
    DOUBLE PRECISION,              INTENT(OUT)   :: DTLANP ! ln|FwObsOnly|
    DOUBLE PRECISION,              INTENT(OUT)   :: HQ
    CHARACTER(LEN=3),              INTENT(OUT)   :: ICONVERGE
    INTEGER,                       INTENT(OUT)   :: ITERP
    DOUBLE PRECISION,              INTENT(OUT)   :: KASHYAPNP
    DOUBLE PRECISION,              INTENT(OUT)   :: MLOFD
    DOUBLE PRECISION,              INTENT(OUT)   :: MLOFDP
    CHARACTER(LEN=12),             INTENT(OUT)   :: MODELLENGTH
    CHARACTER(LEN=12),             INTENT(OUT)   :: MODELMASS
    CHARACTER(LEN=12),             INTENT(OUT)   :: MODELNAME
    CHARACTER(LEN=12),             INTENT(OUT)   :: MODELTIME
    INTEGER,                       INTENT(OUT)   :: MPR
    INTEGER,                       INTENT(OUT)   :: NDINC
    INTEGER,                       INTENT(OUT)   :: NPE
    INTEGER,                       INTENT(OUT)   :: NPERD
    INTEGER,                       INTENT(OUT)   :: NPS
    INTEGER,                       INTENT(OUT)   :: NOBS
    DOUBLE PRECISION,              INTENT(OUT)   :: STAT1
    DOUBLE PRECISION,              INTENT(OUT)   :: STAT2
    DOUBLE PRECISION,              INTENT(OUT)   :: STDERR
    DOUBLE PRECISION,              INTENT(OUT)   :: VAR
    INTEGER,            OPTIONAL,  INTENT(IN)    :: IOUT
    DOUBLE PRECISION,   OPTIONAL,  INTENT(OUT)   :: DTLA
    DOUBLE PRECISION,   OPTIONAL,  INTENT(OUT)   :: KASHYAP
    CHARACTER(LEN=3),   OPTIONAL,  INTENT(OUT)   :: SENTYPE
    INTEGER,            OPTIONAL,  INTENT(OUT)   :: NPLOWEST
    DOUBLE PRECISION,   OPTIONAL,  INTENT(OUT)   :: XTWXNP ! ln|XTwX| obs only
    DOUBLE PRECISION,   OPTIONAL,  INTENT(OUT)   :: XTWXP  ! ln|XTwX| wpri
    CHARACTER(LEN=*),   OPTIONAL,  INTENT(IN)    :: EXT
    ! IDUM,RDUM,RDUM,CDUM,IDUM,RDUM,RDUM,'_presvd')
    !
    !   Local variables
    CHARACTER(LEN=100) CHECK
    CHARACTER(LEN=MAX_STRING_LEN) :: FN
    INTEGER IUDM
    LOGICAL :: LEX = .FALSE.
    LOGICAL :: LPRINT = .FALSE.
    !
    !   Format statement
    10 FORMAT(A20,2X,A15,4X,A)
    20 FORMAT(A20,2X,I15,4X,A)
    30 FORMAT(A20,2X,1PE15.7,4X,A)
    101 FORMAT(//, &
    ' If the _dm file is present and contains information about the ',/, &
    ' model, and if the content of the _dm file is unexpected, it is ',/, &
    ' likely, because the Jupiter application used to create the _dm file',/, &
    ' is not compatible with the application code that is reading it. The',/, &
    ' likely solution is to rerun one, or both, code(s). The one that',/, &
    ' created the _dm file and the code that uses it. A common problem is',/, &
    ' that a user does not update the paths of batch files such that they',/, &
    ' all point to executables from the same download.',//)
    200 FORMAT(1X,'Error opening file ',A,'._dm')
    !
    IFAIL = 0
    AIC = 0.D0
    BIC = 0.D0
    VAR = 0.D0
    ICONVERGE = 'NUL'
    DTLANP = 0.D0
    IF(PRESENT(XTWXNP))XTWXNP = 0.D0
    IF(PRESENT(XTWXP))XTWXP = 0.D0
    IF(PRESENT(DTLA))DTLA = 0.D0
    HQ = 0.D0
    ITERP = 0
    IUDM = 0
    KASHYAPNP = 0.D0
    IF(PRESENT(KASHYAP))KASHYAP = 0.D0
    MLOFD = 0.D0
    MLOFDP = 0.D0
    MODELLENGTH = 'NA'
    MODELMASS = 'NA'
    MODELNAME = 'NA'
    MODELTIME = 'NA'
    MPR = 0
    NDINC = 0
    NPE = 0
    NPERD = 0
    NPS = 0
    NOBS = 0
    STAT1 = 0.D0
    STAT2 = 0.D0
    STDERR = 0.D0
    !   Open existing _dm file
    IF(PRESENT(EXT)) THEN
      FN = TRIM(OUTNAM)//'._dm'//TRIM(EXT)
    ELSE
      FN = TRIM(OUTNAM)//'._dm'
    ENDIF
    INQUIRE(FILE=TRIM(FN),EXIST=LEX)
    IF(LEX) THEN
      IUDM = UTL_GETUNIT(101,150)
      OPEN(UNIT=IUDM,FILE=FN,STATUS='OLD')
    ELSE
      WRITE(*,200)TRIM(FN)
      CALL UTL_STOP()
    ENDIF
    IF(PRESENT(IOUT)) THEN
      IF(IVERB>3 .AND. IOUT>0)LPRINT = .TRUE.
    ENDIF
    IF(LPRINT)WRITE(IOUT,*)
    IF(LPRINT)WRITE(IOUT,*)
    IF(LPRINT)WRITE(IOUT,*) &
    ' VARIABLES POPULATED FROM _dm FILE, ECHOED BECAUSE VERBOSE > 3 '
    IF(LPRINT)WRITE(IOUT,*)
    IF(LPRINT)WRITE(IOUT,*) &
    '       ITEM                VALUE        _DM LABEL '
    IF(LPRINT)WRITE(IOUT,*) &
    ' ___________________  _______________   __________________________________'
    !   Read data
    READ(IUDM,*,END=100)CHECK,MODELNAME
    IF(LPRINT)WRITE(IOUT,10) &
    ' MODELNAME  ',TRIM(MODELNAME),TRIM(CHECK)
    IF(CHECK .NE. 'MODEL NAME: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,MODELLENGTH
    IF(LPRINT)WRITE(IOUT,10) &
    ' MODELLENGTH  ',TRIM(MODELLENGTH),TRIM(CHECK)
    IF(CHECK .NE. 'MODEL LENGTH UNITS: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,MODELMASS
    IF(LPRINT)WRITE(IOUT,10) &
    ' MODELMASS  ',TRIM(MODELMASS),TRIM(CHECK)
    IF(CHECK .NE. 'MODEL MASS UNITS: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,MODELTIME
    IF(LPRINT)WRITE(IOUT,10) &
    ' MODELTIME  ',TRIM(MODELTIME),TRIM(CHECK)
    IF(CHECK .NE. 'MODEL TIME UNITS: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,NPERD
    IF(LPRINT)WRITE(IOUT,20)' NPERD  ',NPERD,TRIM(CHECK)
    IF(CHECK .NE. 'NUMBER ESTIMATED PARAMETERS: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,NPE
    IF(LPRINT)WRITE(IOUT,20)' NPE  ',NPE,TRIM(CHECK)
    IF(CHECK .NE. 'ORIGINAL NUMBER ESTIMATED PARAMETERS: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,NPS
    IF(LPRINT)WRITE(IOUT,20)' NPS  ',NPS,TRIM(CHECK)
    IF(CHECK .NE. 'TOTAL NUMBER PARAMETERS: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,NDINC
    IF(LPRINT)WRITE(IOUT,20)' NDINC  ',NDINC,TRIM(CHECK)
    IF(CHECK .NE. 'NUMBER OBSERVATIONS INCLUDED:') GO TO 100
    READ(IUDM,*,END=100)CHECK,NOBS
    IF(LPRINT)WRITE(IOUT,20)' NOBS  ',NOBS,TRIM(CHECK)
    IF(CHECK .NE. 'NUMBER OBSERVATIONS PROVIDED: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,MPR
    IF(LPRINT)WRITE(IOUT,20)' MPR  ',MPR,TRIM(CHECK)
    IF(CHECK .NE. 'NUMBER PRIOR:') GO TO 100
    READ(IUDM,*,END=100)CHECK,ICONVERGE
    IF(LPRINT)WRITE(IOUT,10)' CONVERGE  ',ICONVERGE,TRIM(CHECK)
    IF(CHECK .NE. 'REGRESSION CONVERGED: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,VAR
    IF(LPRINT)WRITE(IOUT,30)' CEV  ',VAR,TRIM(CHECK)
    IF(CHECK .NE. 'CALCULATED ERROR VARIANCE: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,STDERR
    IF(CHECK .NE. 'STANDARD ERROR OF THE REGRESSION: ') GO TO 100
    IF(LPRINT)WRITE(IOUT,30)' STDERR  ',STDERR,TRIM(CHECK)
    READ(IUDM,*,END=100)CHECK,MLOFD
    IF(LPRINT)WRITE(IOUT,30) &
    ' MLOFD  ',MLOFD,TRIM(CHECK)
    IF(CHECK .NE. &
    'MAXIMUM LIKELIHOOD OBJECTIVE FUNCTION - DEPENDENTS (MLOFD): ') GO TO 100
    READ(IUDM,*,END=100)CHECK,MLOFDP
    IF(LPRINT)WRITE(IOUT,30) &
    ' MLOFDP  ',MLOFDP,TRIM(CHECK)
    IF(CHECK .NE. &
    'MAXIMUM LIKELIHOOD OBJECTIVE FUNCTION - DEPENDENTS AND PRIOR (MLOFDP): ') &
    GO TO 100
    READ(IUDM,*,END=100)CHECK,AIC
    IF(LPRINT)WRITE(IOUT,30)' AICc ObsOnly  ',AIC,TRIM(CHECK)
    IF(CHECK .NE. 'AICc (MLOFD + AICc PENALTY): ') GO TO 100
    READ(IUDM,*,END=100)CHECK,BIC
    IF(LPRINT)WRITE(IOUT,30)' BIC ObsOnly  ',BIC,TRIM(CHECK)
    IF(CHECK .NE. 'BIC (MLOFD + BIC PENALTY): ') GO TO 100
    READ(IUDM,*,END=100)CHECK,HQ
    IF(LPRINT)WRITE(IOUT,30)' HQ ObsOnly  ',HQ,TRIM(CHECK)
    IF(CHECK .NE. 'HQ (MLOFD + HQ PENALTY): ') GO TO 100
    READ(IUDM,*,END=100)CHECK,KASHYAPNP
    IF(LPRINT)WRITE(IOUT,30)' KASHYAP ObsOnly  ',KASHYAP,TRIM(CHECK)
    IF(CHECK .NE. 'KASHYAP (MLOFD + KASHYAP PENALTY): ') GO TO 100
    READ(IUDM,*,END=100)CHECK,DTLANP
    IF(LPRINT)WRITE(IOUT,30)' ln|FwObsOnly|  ',DTLA,TRIM(CHECK)
    IF(CHECK .NE. 'LN DETERMINANT OF FISHER INFORMATION MATRIX: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,STAT1
    IF(LPRINT)WRITE(IOUT,30)' RN2DEP  ',STAT1,TRIM(CHECK)
    IF(CHECK .NE. 'RN2 DEPENDENTS: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,STAT2
    IF(LPRINT)WRITE(IOUT,30)' RN2DEP  ',STAT2,TRIM(CHECK)
    IF(CHECK .NE. 'RN2 DEPENDENTS AND PRIOR: ') GO TO 100
    READ(IUDM,*,END=100)CHECK,ITERP
    IF(LPRINT)WRITE(IOUT,20)' ITERP  ',ITERP,TRIM(CHECK)
    IF(CHECK .NE. 'NUMBER OF ITERATIONS: ') GO TO 100
    IF(PRESENT(KASHYAP)) THEN
      READ(IUDM,*,END=100)CHECK,KASHYAP
      IF(LPRINT)WRITE(IOUT,30)' KASHYAP wPri  ',KASHYAP,TRIM(CHECK)
      IF(CHECK .NE. 'KASHYAP (MLOFDP + KASHYAP PENALTY wPri): ') GO TO 100
    ENDIF
    IF(PRESENT(DTLA)) THEN
      READ(IUDM,*,END=100)CHECK,DTLA
      IF(LPRINT)WRITE(IOUT,30)' ln|FwPri|  ',DTLA,TRIM(CHECK)
      IF(CHECK .NE. &
      'LN DETERMINANT OF FISHER INFORMATION MATRIX wPri: ') &
      GO TO 100
    ENDIF
    IF(PRESENT(SENTYPE)) THEN
      READ(IUDM,*,END=100)CHECK,SENTYPE
      IF(LPRINT)WRITE(IOUT,10)' FORWARD DIFFERENCE SENSITIVITIES?  ', &
      SENTYPE,TRIM(CHECK)
      IF(CHECK .NE. &
      'SOME SENSITIVITIES BY FORWARD DIFFERENCE PERTURBATION: ') &
      GO TO 100
    ENDIF
    IF(PRESENT(NPLOWEST)) THEN
      READ(IUDM,*,END=100)CHECK,NPLOWEST
      IF(LPRINT)WRITE(IOUT,20)' # ESTIMATED PARAMETERS IN LOWEST SOS ITER:  ', &
      NPLOWEST,TRIM(CHECK)
      IF(CHECK .NE. &
      'NUMBER ESTIMATED PARAMETERS IN LOWEST SUM OF SQUARES ITERATION: ') &
      GO TO 100
    ENDIF
    IF(PRESENT(XTWXNP)) THEN
      READ(IUDM,*,END=100)CHECK,XTWXNP
      IF(LPRINT)WRITE(IOUT,30)' LN DETERMINANT OF X^TwX MATRIX ObsOnly:   ', &
      XTWXNP,TRIM(CHECK)
      IF(CHECK .NE. 'LN DETERMINANT OF X^TwX MATRIX ObsOnly: ') GO TO 100
    ENDIF
    IF(PRESENT(XTWXP)) THEN
      READ(IUDM,*,END=100)CHECK,XTWXP
      IF(LPRINT)WRITE(IOUT,30)'LN DETERMINANT OF X^TwX MATRIX wPri: ', &
      XTWXP,TRIM(CHECK)
      IF(CHECK .NE. 'LN DETERMINANT OF X^TwX MATRIX wPri: ') GO TO 100
    ENDIF
    IF(LPRINT)WRITE(IOUT,*)
    IF(LPRINT)WRITE(IOUT,*)
    CLOSE(IUDM)
    LPRINT = .FALSE.
    RETURN
    100 IF(LPRINT)WRITE(IOUT,*)
    IF(LPRINT)WRITE(IOUT,*)
    CLOSE(IUDM)
    IF(LPRINT) WRITE(IOUT,101)
    LPRINT = .FALSE.
    CALL UTL_STOP('UNEXPECTED CONTENT _dm file, CHECK VERSION or RE-CREATE _dm')
    RETURN
  END SUBROUTINE UTLUCODE_DX_READ_DM
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_WRITE_DM(AIC,BIC,LNDETFN,HQ,IFO,ITERP,KASHYAPNP,   &
                         MLOFD,MLOFDP,MODELLENGTH,MODELMASS,MODELNAME,   &
                         MODELTIME,MPR,NDINC,NPE,NPERD,NPS,NOBS,   &
                         OUTNAM,STAT1,STAT2,VAR,DTLA,KASHYAP,SENTYPE,NPFINAL, &
                         DTLANP,DTLAP)
    !   WRITE _DM DEX FILE Model Fit and Parsimony Data
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    DOUBLE PRECISION,   INTENT(IN) :: AIC
    DOUBLE PRECISION,   INTENT(IN) :: BIC
    DOUBLE PRECISION,   INTENT(IN) :: LNDETFN
    DOUBLE PRECISION,   INTENT(IN) :: HQ
    INTEGER,            INTENT(IN) :: IFO
    INTEGER,            INTENT(IN) :: ITERP
    DOUBLE PRECISION,   INTENT(IN) :: KASHYAPNP
    DOUBLE PRECISION,   INTENT(IN) :: MLOFD
    DOUBLE PRECISION,   INTENT(IN) :: MLOFDP
    CHARACTER(LEN=12),  INTENT(IN) :: MODELLENGTH
    CHARACTER(LEN=12),  INTENT(IN) :: MODELMASS
    CHARACTER(LEN=12),  INTENT(IN) :: MODELNAME
    CHARACTER(LEN=12),  INTENT(IN) :: MODELTIME
    INTEGER,            INTENT(IN) :: MPR
    INTEGER,            INTENT(IN) :: NDINC
    INTEGER,            INTENT(IN) :: NPE
    INTEGER,            INTENT(IN) :: NPERD
    INTEGER,            INTENT(IN) :: NPS
    INTEGER,            INTENT(IN) :: NOBS
    CHARACTER(LEN=MAX_STRING_LEN), INTENT(IN) :: OUTNAM
    DOUBLE PRECISION,   INTENT(IN) :: STAT1
    DOUBLE PRECISION,   INTENT(IN) :: STAT2
    DOUBLE PRECISION,   INTENT(IN) :: VAR
    DOUBLE PRECISION,              INTENT(IN) :: DTLA
    DOUBLE PRECISION,              INTENT(IN) :: KASHYAP
    CHARACTER(LEN=3),              INTENT(IN) :: SENTYPE
    INTEGER,                       INTENT(IN) :: NPFINAL
    DOUBLE PRECISION,              INTENT(IN) :: DTLANP
    DOUBLE PRECISION,              INTENT(IN) :: DTLAP
    !
    !   Local variables
    INTEGER IUDM
    CHARACTER(LEN=3) :: CONVERGE = ' NO'
    CHARACTER(LEN=MAX_STRING_LEN) :: FN
    !
    !   Format statements
    656 FORMAT(1X, '"MODEL NAME: "',' "',A,'"')
    657 FORMAT(1X, '"MODEL LENGTH UNITS: " "',A,'"')
    658 FORMAT(1X, '"MODEL MASS UNITS: " "',A,'"')
    659 FORMAT(1X, '"MODEL TIME UNITS: " "',A,'"')
    660 FORMAT(1X, '"NUMBER ESTIMATED PARAMETERS: "',I5)
    661 FORMAT(1X, '"ORIGINAL NUMBER ESTIMATED PARAMETERS: "',I5)
    662 FORMAT(1X, '"TOTAL NUMBER PARAMETERS: "',I5)
    670 FORMAT(1X, '"NUMBER OBSERVATIONS INCLUDED: "',I7)
    671 FORMAT(1X, '"NUMBER OBSERVATIONS PROVIDED: "',I7)
    672 FORMAT(1X, '"NUMBER PRIOR: "',I5)
    673 FORMAT(1X, '"CALCULATED ERROR VARIANCE: "',G19.12)
    674 FORMAT(1X, '"STANDARD ERROR OF THE REGRESSION: "',G19.12)
    675 FORMAT(1X, '"MAXIMUM LIKELIHOOD OBJECTIVE FUNCTION ', &
                    '- DEPENDENTS (MLOFD): "',G14.7)
    676 FORMAT(1X, '"MAXIMUM LIKELIHOOD OBJECTIVE FUNCTION ', &
                    '- DEPENDENTS AND PRIOR (MLOFDP): "',G14.7)
    677 FORMAT(1X, '"AICc (MLOFD + AICc PENALTY): "',G14.7)
    678 FORMAT(1X, '"BIC (MLOFD + BIC PENALTY): "',G14.7)
    679 FORMAT(1X, '"HQ (MLOFD + HQ PENALTY): "',G14.7)
    680 FORMAT(1X, '"KASHYAP (MLOFD + KASHYAP PENALTY): "',G14.7)
    681 FORMAT(1X, '"LN DETERMINANT OF FISHER INFORMATION MATRIX: " ',G14.7)
    682 FORMAT(1X, '"RN2 DEPENDENTS: "',G14.7)
    683 FORMAT(1X, '"RN2 DEPENDENTS AND PRIOR: "',G14.7)
    687 FORMAT(1X, '"NUMBER OF ITERATIONS: "',I7)
    700 FORMAT(1X, '"REGRESSION CONVERGED: "',1X,'"',A3,'"')
    701 FORMAT(1X,'"KASHYAP (MLOFDP + KASHYAP PENALTY wPri): " ',G14.7)
    702 FORMAT(1X,'"LN DETERMINANT OF FISHER INFORMATION MATRIX wPri: " ',G14.7)
    703 FORMAT(1X, &
        '"SOME SENSITIVITIES BY FORWARD DIFFERENCE PERTURBATION: "  "',A3,'"')
    704 FORMAT(1X, &
    '"NUMBER ESTIMATED PARAMETERS IN LOWEST SUM OF SQUARES ITERATION: " ',I10)
    705 FORMAT(1X,'"LN DETERMINANT OF X^TwX MATRIX ObsOnly: " ',G14.7)
    706 FORMAT(1X,'"LN DETERMINANT OF X^TwX MATRIX wPri: " ',G14.7)
    ! Write  data
    FN = TRIM(OUTNAM)//'._dm'
    IUDM = UTL_GETUNIT(101,150)
    OPEN(UNIT=IUDM,FILE=FN,STATUS='REPLACE')
    IF(IFO .LT. 3) CONVERGE = 'YES'
    WRITE(IUDM,656) TRIM(MODELNAME)
    WRITE(IUDM,657) TRIM(MODELLENGTH)
    WRITE(IUDM,658) TRIM(MODELMASS)
    WRITE(IUDM,659) TRIM(MODELTIME)
    WRITE(IUDM,660) NPERD
    WRITE(IUDM,661) NPE
    WRITE(IUDM,662) NPS
    WRITE(IUDM,670) NDINC
    WRITE(IUDM,671) NOBS
    WRITE(IUDM,672) MPR
    WRITE(IUDM,700) CONVERGE
    WRITE(IUDM,673) VAR
    WRITE(IUDM,674) VAR**.5
    WRITE(IUDM,675) MLOFD
    WRITE(IUDM,676) MLOFDP
    WRITE(IUDM,677) AIC
    WRITE(IUDM,678) BIC
    WRITE(IUDM,679) HQ
    WRITE(IUDM,680) KASHYAPNP
    WRITE(IUDM,681) LNDETFN
    WRITE(IUDM,682) STAT1
    IF(STAT2 .GT. 0) THEN
      WRITE(IUDM,683) STAT2
    ELSE
      WRITE(IUDM,683) STAT1
    ENDIF
    WRITE(IUDM,687) ITERP
    WRITE(IUDM,701) KASHYAP
    WRITE(IUDM,702) DTLA
    WRITE(IUDM,703) TRIM(SENTYPE)
    WRITE(IUDM,704) NPFINAL
    WRITE(IUDM,705) DTLANP
    WRITE(IUDM,706) DTLAP
    CLOSE (IUDM)
    RETURN
  END SUBROUTINE UTLUCODE_DX_WRITE_DM
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_CSSSVD(IOUT,NPE,NPS,IPTR,CSSWP,OUTNAM,PARNAM, &
                             SINGVAL,SINGVEC,CSSNP,SVALNP,SVECNP)
    !   USE SINGULAR VALUE DECOMPOSITION VECTORS OF SQRTWX
    !   TO WRITE THE _SC_SVD FILE
    USE GLOBAL_DATA, ONLY: IVERB
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                               INTENT(IN)    :: IOUT
    INTEGER,                               INTENT(IN)    :: NPE
    INTEGER,                               INTENT(IN)    :: NPS
    INTEGER,           DIMENSION(NPE),     INTENT(IN)    :: IPTR
    DOUBLE PRECISION,  DIMENSION(NPE),     INTENT(IN)    :: CSSWP
    CHARACTER(LEN=*),                      INTENT(IN)    :: OUTNAM
    CHARACTER(LEN=12), DIMENSION(NPS),     INTENT(IN)    :: PARNAM
    DOUBLE PRECISION,  DIMENSION(NPE),     INTENT(IN)    :: SINGVAL
    DOUBLE PRECISION,  DIMENSION(NPE,NPE), INTENT(IN)    :: SINGVEC
    DOUBLE PRECISION, OPTIONAL, DIMENSION(NPE),     INTENT(IN) :: CSSNP
    DOUBLE PRECISION, OPTIONAL, DIMENSION(NPE),     INTENT(IN) :: SVALNP
    DOUBLE PRECISION, OPTIONAL, DIMENSION(NPE,NPE), INTENT(IN) :: SVECNP
    !
    !   Local variables
    DOUBLE PRECISION CSSSVD(NPE,NPE)
    CHARACTER(LEN=MAX_STRING_LEN) :: OUTNAMTMP
    DOUBLE PRECISION TEMPSUM
    INTEGER I, IUCSSSVD, K, M
    !
    !   Format statements
    8 FORMAT (1X,'"NO PRIOR INFORMATION WAS USED IN THE REGRESSION"')
    ! _sc_svd
    IUCSSSVD = UTL_GETUNIT(101,150)
    OUTNAMTMP = TRIM(OUTNAM)//'._sc_svd'
    OPEN(UNIT=IUCSSSVD,FILE=OUTNAMTMP,STATUS='REPLACE')
    M = 1
    CSSSVD = 0.D0
    DO K=1,NPE
      TEMPSUM=0.
      DO I=1,NPE
        CSSSVD(K,I)= ((SINGVAL(I)**2)*(SINGVEC(K,I)**2))
        TEMPSUM=TEMPSUM+CSSSVD(K,I)
      ENDDO
      DO I=1,NPE
        CSSSVD(K,I)=CSSSVD(K,I)/TEMPSUM
      ENDDO
    ENDDO
    CALL UTLUCODE_DX_WRITE_CSSSVD(IUCSSSVD,NPE,NPS,IPTR,PARNAM,CSSWP,CSSSVD,M)
    IF(PRESENT(CSSNP) .AND. PRESENT(SVALNP) .AND. PRESENT(SVECNP)) THEN
      M = 2
      CSSSVD = 0.D0
      DO K=1,NPE
        TEMPSUM=0.
        DO I=1,NPE
          CSSSVD(K,I)= ((SVALNP(I)**2)*(SVECNP(K,I)**2))
          TEMPSUM=TEMPSUM+CSSSVD(K,I)
        ENDDO
        DO I=1,NPE
          CSSSVD(K,I)=CSSSVD(K,I)/TEMPSUM
        ENDDO
      ENDDO
      CALL UTLUCODE_DX_WRITE_CSSSVD(IUCSSSVD,NPE,NPS,IPTR,PARNAM,CSSNP,CSSSVD,M)
    ELSE
      WRITE(IUCSSSVD,8)
    ENDIF
    CLOSE(IUCSSSVD)
    !
    RETURN
  END SUBROUTINE UTLUCODE_CSSSVD
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_WRITE_CSSSVD(IUCSSSVD,NPE,NPT,IPTR,PARNAM,CS,SCSVDVC,I) !CSSWP
    !   WRITE _SC_SVD DX FILE
    USE GLOBAL_DATA, ONLY: AMESSAGE, BLANKS, NPPREC
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: IUCSSSVD  ! Unit number opened for DX file
    INTEGER,                                 INTENT(IN) :: NPE       ! Number of active parameters
    INTEGER,                                 INTENT(IN) :: NPT       ! Number of parameters, total
    INTEGER,                DIMENSION(NPE),  INTENT(IN) :: IPTR      ! Pointers to adjustable parameters
    CHARACTER(LEN=12),      DIMENSION(NPT),  INTENT(IN) :: PARNAM    ! Parameter names
    DOUBLE PRECISION,   DIMENSION(NPE),      INTENT(IN) :: CS        ! Array of composite scaled sensitivties !CSSWP
    DOUBLE PRECISION,   DIMENSION(NPE,NPE),  INTENT(IN) :: SCSVDVC
    ! SCSVDVC is Array of sqrt((singval^2*singvec^2)/ND for each parameter)/(sum of that for all vectors in that row)
    INTEGER,                                 INTENT(IN) :: I         ! 1=forCSSWP if prior used 2=forCSS no prior
    !
    !   Local variables
    INTEGER :: IP, IP1, IP2, J, KB, KP, N
    CHARACTER(LEN=150) :: FMT_STRING1, FMT_STRING2, FMT_STRING3
    CHARACTER(LEN=10)  :: STR_NPPREC
    DOUBLE PRECISION :: MAXCSS
    !
    ! Formats
    2 FORMAT &
    (1X,'"Decomposition of the CSS (with prior if used) using SVD of ', &
    'the [wt^0.5][X] matrix scaled by parameter values and the #obs+#prior"')
    9 FORMAT (1X,'"WITHOUT PRIOR:"')
   12 FORMAT &
    (1X,'"Decomposition of the CSS (without prior) ', &
    'using SVD of the [wt^0.5][X] matrix scaled by parameter values and #obs"')
    !   Construct format strings
    WRITE(STR_NPPREC,'(I10)') NPPREC
    STR_NPPREC = ADJUSTL(STR_NPPREC)
    FMT_STRING1 = &
    '(1X,''"PARAMETER NAME" "COMPOSITE SCALED SENSITIVITY WITH PRIOR"'', &
    '' "RATIO TO LARGEST CSS"'',' &
    //TRIM(STR_NPPREC)//'(1X,''"FROM SINGULAR VECTOR'',1X,I6,''"'',:))'
    FMT_STRING2 = &
     '(1X,A,2X,G25.16,18X,G15.7,2X,'//TRIM(STR_NPPREC)//'(1X,G29.16))'
    FMT_STRING3 = &
    '(1X,''"PARAMETER NAME" "COMPOSITE SCALED SENSITIVITY           "'', &
    '' "RATIO TO LARGEST CSS"'',' &
    //TRIM(STR_NPPREC)//'(1X,''"FROM SINGULAR VECTOR'',1X,I6,''"'',:))'
    !
    !   Find largest CSS
    MAXCSS = 0.D0
    DO N = 1,NPE
      IF(CS(N) > MAXCSS) MAXCSS = CS(N) !CSSWP
    ENDDO
    !   Write singular values, in blocks of NPPREC parameters
    KB = 0
    KP = 0
    IF(I == 1) THEN
      WRITE (IUCSSSVD,2)
    ELSE
      WRITE (IUCSSSVD,9)
      WRITE (IUCSSSVD,12)
    ENDIF
    DO WHILE (KP<NPE)
      IP1 = KB*NPPREC+1
      KB = KB+1
      IP2 = KB*NPPREC
      IF (IP2>NPE) IP2 = NPE
      WRITE (IUCSSSVD,FMT_STRING1) (J,J=IP1,IP2)
      DO N = 1, NPE
        WRITE (IUCSSSVD,FMT_STRING2) PARNAM(IPTR(N)),CS(N),CS(N)/MAXCSS, & !CSSWP
                                        (SCSVDVC(N,IP),IP=IP1,IP2)
      ENDDO
      KP = IP2
    ENDDO
    RETURN
  END SUBROUTINE UTLUCODE_DX_WRITE_CSSSVD !CSSWP
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_WRITE_SVD_ID &
                          (IUSVDPL,NSVD,NPE,NPT,IPTR,PARNAM,SINGVEC)
    !  CALCULATE PARAMETER LOADING and WRITE _SC_SVD DX FILE
    USE GLOBAL_DATA, ONLY: AMESSAGE, BLANKS
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                    INTENT(IN) :: IUSVDPL  ! Unit number opened for DX file
    INTEGER,                                    INTENT(IN) :: NSVD    ! Number of active svd parameters
    INTEGER,                                    INTENT(IN) :: NPE ! Number of possible svd parameters
    INTEGER,                                    INTENT(IN) :: NPT    ! Number of parameters, total
    INTEGER,                    DIMENSION(NPE), INTENT(IN) :: IPTR   ! Pointers to adjustable parameters
    CHARACTER(LEN=12),          DIMENSION(NPT), INTENT(IN) :: PARNAM ! Parameter names
    DOUBLE PRECISION,       DIMENSION(NPE,NPE), INTENT(IN) :: SINGVEC ! Array of values from singular vectors
    !
    !   Local variables
    INTEGER :: I, J, K
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: SUMSQSVDCOEF
    !
    ! formats
    201 FORMAT(1X,' "PARAMETER NAME" ', &
    ' "SVD-ID statistic (see _svd for vector contributions to SVD-ID stat)" ', &
    ' "Number of SVD parameters = " ',I8)
    202 FORMAT(1X,A,2X,G25.16)
    210 FORMAT(1X, &
    ' "If a different SVDNumber is selected, SVD-ID statistic would be:"')
    211 FORMAT(1X,' "PARAMETER NAME" ', &
    ' "SVD-ID statistic " ', &
    ' "Number of SVD parameters = " ',I8)
    !
    ALLOCATE(SUMSQSVDCOEF(NPE)) ! Array of sum of squared svd coefficients
    SUMSQSVDCOEF = 0.D0
    !
    WRITE(IUSVDPL,201)NSVD
    DO I=1,NPE
      DO J=1,NSVD
        SUMSQSVDCOEF(I) = SUMSQSVDCOEF(I) + (SINGVEC(I,J)*SINGVEC(I,J))
      ENDDO
      WRITE (IUSVDPL,202) PARNAM(IPTR(I)),SUMSQSVDCOEF(I)
    ENDDO
! help user consider options
    WRITE(IUSVDPL,*)
    WRITE(IUSVDPL,210)
    DO K=1,NPE
      IF(K .NE. NSVD) THEN
        SUMSQSVDCOEF = 0.D0
        WRITE(IUSVDPL,211)K
        DO I=1,NPE
          DO J=1,K
            SUMSQSVDCOEF(I) = SUMSQSVDCOEF(I) + (SINGVEC(I,J)*SINGVEC(I,J))
          ENDDO
          WRITE (IUSVDPL,202) PARNAM(IPTR(I)),SUMSQSVDCOEF(I)
        ENDDO
      ENDIF
    ENDDO
    RETURN
  END SUBROUTINE UTLUCODE_DX_WRITE_SVD_ID
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_EIGEN(IOUT,IPRC,NPE,NPS,IPTR,ITERP,PARNAM,PVALINIT,PVAL, &
                       OUTNAM,C,CNOPRI,IPRINTIOUT,LARGE2SMALL,BOTH)
    ! similar to UTL_EIGEN
    ! CALCULATE EIGENVALUES AND EIGENVECTORS OF COVARIANCE MATRIX
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                               INTENT(IN)    :: IOUT
    INTEGER,                               INTENT(IN)    :: IPRC
    INTEGER,                               INTENT(IN)    :: NPE
    INTEGER,                               INTENT(IN)    :: NPS
    INTEGER,           DIMENSION(NPE),     INTENT(IN)    :: IPTR
    INTEGER,                               INTENT(IN)    :: ITERP
    CHARACTER(LEN=12), DIMENSION(NPS),     INTENT(IN)    :: PARNAM
    DOUBLE PRECISION,  DIMENSION(NPS),     INTENT(IN)    :: PVALINIT
    DOUBLE PRECISION,  DIMENSION(NPS),     INTENT(IN)    :: PVAL
    CHARACTER(LEN=*),                      INTENT(IN)    :: OUTNAM
    DOUBLE PRECISION,  DIMENSION(NPE,NPE), INTENT(INOUT) :: C
    DOUBLE PRECISION,  DIMENSION(NPE,NPE), INTENT(INOUT) :: CNOPRI
    LOGICAL, OPTIONAL, INTENT(IN) :: IPRINTIOUT
    LOGICAL, OPTIONAL, INTENT(IN) :: LARGE2SMALL
    LOGICAL, OPTIONAL, INTENT(IN) :: BOTH
    !
    !   Local variables
    DOUBLE PRECISION EIGL(NPE)
    DOUBLE PRECISION EIGW(NPE)
    DOUBLE PRECISION EIGV(NPE,NPE)
    DOUBLE PRECISION TEMP
    INTEGER I, IIP, IIPP, IP, IUEIG, J, NLP
    INTEGER NLOOP
    LOGICAL IBOTH, L2S, PRINTIOUT
    CHARACTER(LEN=47) :: ANAME
    DATA ANAME /'VARIANCE-COVARIANCE MATRIX SCALED W/ PARAMETERS'/
    !
    !   Format statements
    500 FORMAT (A12,8D13.4,/,10(12X,8D13.4))
    505 FORMAT (/,12X,8D13.4,/,10(12X,8D13.4))
    510 FORMAT (/,' EIGENVALUES')
    515 FORMAT (/,' EIGENVECTORS',/)
    520 FORMAT (/,10X,A47,/,10X,47('-'))
    610 FORMAT (' "WITHOUT PRIOR IN THE COVARIANCE MATRIX"')
    !
    IF(PRESENT(LARGE2SMALL)) THEN
      L2S = LARGE2SMALL
    ELSE
      L2S = .FALSE.
    ENDIF
    IF(PRESENT(IPRINTIOUT)) THEN
      PRINTIOUT = IPRINTIOUT
    ELSE
      PRINTIOUT = .FALSE.
    ENDIF
    IF(PRESENT(BOTH)) THEN
      IBOTH = BOTH
    ELSE
      IBOTH = .FALSE.
    ENDIF
    !---------SCALE VARIANCE-COVARIANCE MATRIX WITH PARAMETER VALUES
    EIGL = 0.D0
    EIGV = 0.D0
    EIGW = 0.D0
    IF (ITERP.GT.1) THEN
      DO IP = 1, NPE
        IIPP = IPTR(IP)
        DO IIP = 1, NPE
          C(IP,IIP) = C(IP,IIP)/(PVAL(IIPP)*PVAL(IPTR(IIP)))
        ENDDO
      ENDDO
    ELSE
      DO IP = 1, NPE
        IIPP = IPTR(IP)
        DO IIP = 1, NPE
          C(IP,IIP) = C(IP,IIP)/(PVALINIT(IIPP)*PVALINIT(IPTR(IIP)))
        ENDDO
      ENDDO
    ENDIF
    IF(IBOTH) THEN
      IF (ITERP.GT.1) THEN
        DO IP = 1, NPE
          IIPP = IPTR(IP)
          DO IIP = 1, NPE
            CNOPRI(IP,IIP) = CNOPRI(IP,IIP)/(PVAL(IIPP)*PVAL(IPTR(IIP)))
          ENDDO
        ENDDO
      ELSE
        DO IP = 1, NPE
          IIPP = IPTR(IP)
          DO IIP = 1, NPE
            CNOPRI(IP,IIP) = CNOPRI(IP,IIP)/(PVALINIT(IIPP)*PVALINIT(IPTR(IIP)))
          ENDDO
        ENDDO
      ENDIF
    ENDIF
    IF(PRINTIOUT) THEN
      WRITE (IOUT,520) ANAME
      CALL UTL_PARSQMATRIX(IOUT,IPRC,NPE,NPS,C,IPTR,PARNAM)
    ENDIF
    IUEIG = UTL_DX_OPEN(OUTNAM,'_eig','REPLACE')
    NLOOP = 1
    IF(IBOTH) NLOOP = 2
    DO NLP=1,NLOOP
      !-------CALCULATE EIGENVALUES AND EIGENVECTORS
      IF(NLP == 1) THEN
        DO IP = 1, NPE
          DO I = 1, NPE
            EIGV(I,IP) = C(I,IP)
          ENDDO
        ENDDO
      ELSE
        EIGL = 0.D0
        EIGV = 0.D0
        EIGW = 0.D0
        DO IP = 1, NPE
          DO I = 1, NPE
            EIGV(I,IP) = CNOPRI(I,IP)
          ENDDO
        ENDDO
      ENDIF
      CALL UTLUCODE_EIGEN1(NPE,NPE,EIGL,EIGV,EIGW)
      CALL UTLUCODE_EIGEN2(NPE,NPE,IOUT,EIGL,EIGV,EIGW)
      CALL UTLUCODE_EIGEN4(NPE,NPE,EIGL,EIGV,L2S)
      !-------SCALE EIGENVECTORS TO FORM UNIT VECTORS
      DO I = 1, NPE
        TEMP = 0.D0
        DO J = 1, NPE
          TEMP = TEMP + EIGV(J,I)**2
        ENDDO
        TEMP = TEMP**.5
        DO J = 1, NPE
          EIGV(J,I) = EIGV(J,I)/TEMP
        ENDDO
      ENDDO
      IF(NLP == 1) THEN
        !-------PRINT EIGENVALUES
        WRITE (IOUT,510)
        WRITE (IOUT,'(/,6X,8I13,/,10(6X,8I13))') (J,J=1,NPE)
        WRITE (IOUT,505) (EIGL(J),J=1,NPE)
        !-------PRINT EIGENVECTORS
        WRITE (IOUT,515)
        DO J = 1, NPE
          WRITE (IOUT,500) PARNAM(IPTR(J)), (EIGV(J,I),I=1,NPE)
        ENDDO
        CALL UTLUCODE_DX_WRITE_EIG(IUEIG,NPE,NPS,IPTR,PARNAM,EIGL,EIGV)
      ENDIF
      IF(NLP == 2) THEN
        WRITE(IUEIG,610)
        CALL UTLUCODE_DX_WRITE_EIG(IUEIG,NPE,NPS,IPTR,PARNAM,EIGL,EIGV)
      ENDIF
    ENDDO
    !---------UNSCALE VARIANCE-COVARIANCE MATRIX
    IF (ITERP.GT.1) THEN
      DO IP = 1, NPE
        IIPP = IPTR(IP)
        DO IIP = 1, NPE
          C(IP,IIP) = C(IP,IIP)*(PVAL(IIPP)*PVAL(IPTR(IIP)))
        ENDDO
      ENDDO
    ELSE
      DO IP = 1, NPE
        IIPP = IPTR(IP)
        DO IIP = 1, NPE
          C(IP,IIP) = C(IP,IIP)*(PVALINIT(IIPP)*PVALINIT(IPTR(IIP)))
        ENDDO
      ENDDO
    ENDIF
    RETURN
  END SUBROUTINE UTLUCODE_EIGEN
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_WRITE_EIG(IUEIG,NPE,NPT,IPTR,PARNAM,EIGVAL,EIGVEC)
    !   WRITE _EIG DX FILE  EigenValues and EigenVectors
    USE GLOBAL_DATA, ONLY: AMESSAGE, BLANKS, NPPREC
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: IUEIG  ! Unit number opened for DX file
    INTEGER,                                 INTENT(IN) :: NPE    ! Number of active parameters
    INTEGER,                                 INTENT(IN) :: NPT    ! Number of parameters, total
    INTEGER,                DIMENSION(NPE),  INTENT(IN) :: IPTR   ! Pointers to adjustable parameters
    CHARACTER(LEN=12),      DIMENSION(NPT),  INTENT(IN) :: PARNAM ! Parameter names
    DOUBLE PRECISION,   DIMENSION(NPE),      INTENT(IN) :: EIGVAL ! Array of eigenvalues
    DOUBLE PRECISION,   DIMENSION(NPE,NPE),  INTENT(IN) :: EIGVEC ! Array of eigenvectors
    !
    !   Local variables
    INTEGER :: IP, IP1, IP2, J, KB, KP, N
    CHARACTER(LEN=300) :: FMT_STRING0, FMT_STRING1, FMT_STRING2, FMT_STRING3
    CHARACTER(LEN=10)  :: STR_NPPREC
    !
    ! Formats
    2 FORMAT(1X,'"Variance-Covariance Matrix of the Parameters scaled by ', &
                'parameter values: Eigenvalues and Eigenvectors"')
    !   Construct format strings
    WRITE(STR_NPPREC,'(I10)') NPPREC
    STR_NPPREC = ADJUSTL(STR_NPPREC)
    FMT_STRING0 = '(42X,'// &
                  TRIM(STR_NPPREC)//'(1X,''"EIGENVALUE    '',1X,I8,''"'',:))'
    FMT_STRING3 = '(1X,''"EIGENVALUE FOR EACH VECTOR  "'',8X,'// &
                  TRIM(STR_NPPREC)//'(1X,G25.16))'
    FMT_STRING1 = '(1X,''"PARAMETER FOR EACH VECTOR ELEMENT "'',5X,'// &
                  TRIM(STR_NPPREC)//'(1X,''"EIGENVECTOR   '',1X,I8,''"'',:))'
    FMT_STRING2 = '(1X,A,27X,'//TRIM(STR_NPPREC)//'(1X,G25.16))'
    !
    !   Write eigenvalues & vectors, in blocks of NPPREC parameters
    KB = 0
    KP = 0
    WRITE (IUEIG,2)
    DO WHILE (KP<NPE)
      IP1 = KB*NPPREC+1
      KB = KB+1
      IP2 = KB*NPPREC
      IF (IP2>NPE) IP2 = NPE
      WRITE (IUEIG,FMT_STRING0) (J,J=IP1,IP2)
      WRITE (IUEIG,FMT_STRING3) (EIGVAL(J),J=IP1,IP2)
      WRITE (IUEIG,FMT_STRING1) (J,J=IP1,IP2)
      DO N = 1, NPE
        WRITE (IUEIG,FMT_STRING2) PARNAM(IPTR(N)),(EIGVEC(N,IP),IP=IP1,IP2)
      ENDDO
      KP = IP2
    ENDDO
    RETURN
  END SUBROUTINE UTLUCODE_DX_WRITE_EIG
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_EIGEN1(N,NP,EIGL,EIGV,EIGW)
    ! identical to UTL_EIGEN1 but that is not public & cannot be called directly
    !     VERSION 20031110 EPP
    !     ******************************************************************
    !     COPYRIGHT (C) 1986 NUMERICAL RECIPES SOFTWARE, SUBROUTINE TRED2,
    !     REPRODUCED BY PERMISSION FROM THE BOOK NUMERICAL RECIPES: THE ART
    !     OF SCIENTIFIC COMPUTING, PUBLISHED BY CAMBRIDGE UNIVERSITY PRESS
    !     MODIFIED FOR DOUBLE PRECISION
    !     ******************************************************************
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                            INTENT(IN)    :: N
    INTEGER,                            INTENT(IN)    :: NP
    DOUBLE PRECISION, DIMENSION(NP),    INTENT(INOUT) :: EIGL
    DOUBLE PRECISION, DIMENSION(NP,NP), INTENT(INOUT) :: EIGV
    DOUBLE PRECISION, DIMENSION(NP),    INTENT(INOUT) :: EIGW
    !
    !   Local variables
    DOUBLE PRECISION :: F, G, H, HH, SCALE
    INTEGER :: I, J, K, L
    !
    !     ------------------------------------------------------------------
    IF (N.GT.1) THEN
      DO I = N, 2, -1
        L = I - 1
        H = 0.D0
        SCALE = 0.D0
        IF (L.GT.1) THEN
          DO K = 1, L
            SCALE = SCALE + DABS(EIGV(I,K))
          ENDDO
          IF (SCALE.EQ.0.0) THEN
            EIGW(I) = EIGV(I,L)
          ELSE
            DO K = 1, L
              EIGV(I,K) = EIGV(I,K)/SCALE
              H = H + EIGV(I,K)**2
            ENDDO
            F = EIGV(I,L)
            G = -SIGN(DSQRT(H),F)
            EIGW(I) = SCALE*G
            H = H - F*G
            EIGV(I,L) = F - G
            F = 0.D0
            DO J = 1, L
              EIGV(J,I) = EIGV(I,J)/H
              G = 0.D0
              DO K = 1, J
                G = G + EIGV(J,K)*EIGV(I,K)
              ENDDO
              IF (L.GT.J) THEN
                DO K = J+1, L
                  G = G + EIGV(K,J)*EIGV(I,K)
                ENDDO
              ENDIF
              EIGW(J) = G/H
              F = F + EIGW(J)*EIGV(I,J)
          ENDDO
          HH = F/(H+H)
          DO J = 1, L
            F = EIGV(I,J)
            G = EIGW(J) - HH*F
            EIGW(J) = G
            DO K = 1, J
              EIGV(J,K) = EIGV(J,K) - F*EIGW(K) - G*EIGV(I,K)
            ENDDO
          ENDDO
        ENDIF
        ELSE
          EIGW(I) = EIGV(I,L)
        ENDIF
        EIGL(I) = H
      ENDDO
    ENDIF
    EIGL(1) = 0.D0
    EIGW(1) = 0.D0
    DO I = 1, N
      L = I - 1
      IF (EIGL(I).NE.0.0) THEN
        DO J = 1, L
          G = 0.D0
          DO K = 1, L
            G = G + EIGV(I,K)*EIGV(K,J)
          ENDDO
          DO K = 1, L
            EIGV(K,J) = EIGV(K,J) - G*EIGV(K,I)
          ENDDO
        ENDDO
      ENDIF
      EIGL(I) = EIGV(I,I)
      EIGV(I,I) = 1.D0
      IF (L.GE.1) THEN
        DO J = 1, L
          EIGV(I,J) = 0.D0
          EIGV(J,I) = 0.D0
        ENDDO
      ENDIF
    ENDDO
  RETURN
  END SUBROUTINE UTLUCODE_EIGEN1
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_EIGEN2(N,NP,IOUT,EIGL,EIGV,EIGW)
    ! identical to UTL_EIGEN2 but that is not public & cannot be called directly
    !     ******************************************************************
    !     COPYRIGHT (C) 1986 NUMERICAL RECIPES SOFTWARE, TQLI,
    !     REPRODUCED BY PERMISSION FROM THE BOOK NUMERICAL RECIPES: THE ART
    !     OF SCIENTIFIC COMPUTING, PUBLISHED BY CAMBRIDGE UNIVERSITY PRESS
    !     MODIFIED FOR DOUBLE PRECISION
    !     ******************************************************************
    !        SPECIFICATIONS:
    !     ------------------------------------------------------------------
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                            INTENT(IN)    :: N
    INTEGER,                            INTENT(IN)    :: NP
    INTEGER,                            INTENT(IN)    :: IOUT
    DOUBLE PRECISION, DIMENSION(NP),    INTENT(INOUT) :: EIGL
    DOUBLE PRECISION, DIMENSION(NP,NP), INTENT(INOUT) :: EIGV
    DOUBLE PRECISION, DIMENSION(NP),    INTENT(INOUT) :: EIGW
    !
    !   Local variables
    DOUBLE PRECISION :: B, CC, DDD, F, G, P, R, S
    INTEGER :: I, ITER, K, L, M
    !
    !   Format statement
    45  FORMAT(/,' WARNING:  TOO MANY ITERATIONS IN SPES1BAS6TQ',/)
    !
    IF (N.GT.1) THEN
      DO I = 2, N
        EIGW(I-1) = EIGW(I)
      ENDDO
      EIGW(N) = 0.D0
      DO L = 1, N
        ITER = 0
        20 DO M = L, N-1
          DDD = DABS(EIGL(M)) + DABS(EIGL(M+1))
          IF (DABS(EIGW(M))+DDD.EQ.DDD) GOTO 40
        ENDDO
        M = N
        40 IF (M.NE.L) THEN
          IF (ITER.EQ.30) THEN
            WRITE (IOUT,45)
          ENDIF
          ITER = ITER + 1
          G = (EIGL(L+1)-EIGL(L))/(2.D0*EIGW(L))
          R = DSQRT(G**2+1.D0)
          G = EIGL(M) - EIGL(L) + EIGW(L)/(G+SIGN(R,G))
          S = 1.D0
          CC = 1.D0
          P = 0.D0
          DO I = M-1, L, -1
            F = S*EIGW(I)
            B = CC*EIGW(I)
            IF (DABS(F).GE.DABS(G)) THEN
              CC = G/F
              R = DSQRT(CC**2+1.D0)
              EIGW(I+1) = F*R
              S = 1.D0/R
              CC = CC*S
            ELSE
              S = F/G
              R = DSQRT(S**2+1.D0)
              EIGW(I+1) = G*R
              CC = 1.D0/R
              S = S*CC
            ENDIF
            G = EIGL(I+1) - P
            R = (EIGL(I)-G)*S + 2.D0*CC*B
            P = S*R
            EIGL(I+1) = G + P
            G = CC*R - B
            DO K = 1, N
              F = EIGV(K,I+1)
              EIGV(K,I+1) = S*EIGV(K,I) + CC*F
              EIGV(K,I) = CC*EIGV(K,I) - S*F
            ENDDO
          ENDDO
          EIGL(L) = EIGL(L) - P
          EIGW(L) = G
          EIGW(M) = 0.D0
          GOTO 20
        ENDIF
      ENDDO
    ENDIF
    RETURN
  END SUBROUTINE UTLUCODE_EIGEN2
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_EIGEN4(N,NP,EIGL,EIGV,LARGE2SMALL)
    ! similar to UTL_EIGEN3 but this orders eigenvalues from smallest to largest
    !     ******************************************************************
    !     COPYRIGHT (C) 1986 NUMERICAL RECIPES SOFTWARE, EIGSRT,
    !     REPRODUCED BY PERMISSION FROM THE BOOK NUMERICAL RECIPES: THE ART
    !     OF SCIENTIFIC COMPUTING, PUBLISHED BY CAMBRIDGE UNIVERSITY PRESS
    !     MODIFIED FOR DOUBLE PRECISION AND TO ORDER THE EIGENVALUES FROM
    !     LARGEST TO SMALLEST
    !     ******************************************************************
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                            INTENT(IN)    :: N
    INTEGER,                            INTENT(IN)    :: NP
    DOUBLE PRECISION, DIMENSION(NP),    INTENT(INOUT) :: EIGL
    DOUBLE PRECISION, DIMENSION(NP,NP), INTENT(INOUT) :: EIGV
    LOGICAL, OPTIONAL, INTENT(IN) :: LARGE2SMALL
    !
    !   Local variables
    DOUBLE PRECISION P
    INTEGER I, J, K
    LOGICAL L2S
    !
    IF(PRESENT(LARGE2SMALL)) THEN
      L2S = LARGE2SMALL
    ELSE
      L2S = .FALSE.
    ENDIF
    !     ------------------------------------------------------------------
    DO I = 1, N-1
      K = I
      P = EIGL(I)
      DO J = I+1, N
        !!!!! utl has LE not GE
        IF(LARGE2SMALL) THEN
          IF (EIGL(J).GE.P) THEN
            K = J
            P = EIGL(J)
          ENDIF
        ELSE ! order from small to large
          IF (EIGL(J).LE.P) THEN
            K = J
            P = EIGL(J)
          ENDIF
        ENDIF
      ENDDO
      IF (K.NE.I) THEN
        EIGL(K) = EIGL(I)
        EIGL(I) = P
        DO J = 1, N
          P = EIGV(J,I)
          EIGV(J,I) = EIGV(J,K)
          EIGV(J,K) = P
        ENDDO
      ENDIF
    ENDDO
    RETURN
  END SUBROUTINE UTLUCODE_EIGEN4
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_READ_INIT &
             (NPS,ISTAT,OUTNAM,CEVINIT,PALN,PANAM,PAPINCR,PAPVAL)
    !     READ _INIT DX FILE
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                                    INTENT(IN)  :: NPS
    INTEGER,                                    INTENT(IN)  :: ISTAT
    CHARACTER(LEN=MAX_STRING_LEN),              INTENT(IN)  :: OUTNAM
    DOUBLE PRECISION,                           INTENT(OUT) :: CEVINIT
    INTEGER,                                    INTENT(OUT) :: PALN(NPS)
    CHARACTER(LEN=12),                          INTENT(OUT) :: PANAM(NPS)
    INTEGER,                                    INTENT(OUT) :: PAPINCR(NPS)
    DOUBLE PRECISION,                           INTENT(OUT) :: PAPVAL(NPS)
    ! Local variables
    CHARACTER(LEN=MAX_STRING_LEN) :: CHECK
    INTEGER                       :: I
    INTEGER                       :: IUINIT
    CHARACTER(LEN=MAX_STRING_LEN) :: OUTNAMTMP
    !
    IUINIT = UTL_GETUNIT(101,150)
    OUTNAMTMP = TRIM(OUTNAM)//'._init._dm'
    OPEN(UNIT=IUINIT,FILE=OUTNAMTMP,STATUS='OLD')
    ! Read cevinit
    READ (IUINIT,*,END=100)CHECK,CEVINIT
    CLOSE(IUINIT)
    !
    OUTNAMTMP = TRIM(OUTNAM)//'._init'
    OPEN(UNIT=IUINIT,FILE=OUTNAMTMP,STATUS='OLD')
    READ(IUINIT,*,END=100)CHECK
    IF(ISTAT == 1) RETURN
    ! Read requested values
    DO I=1,NPS
      READ (IUINIT,*,END=100)PANAM(I),PAPVAL(I),PALN(I),PAPINCR(I)
    ENDDO
    CLOSE(IUINIT)
    RETURN
    !
    100 CLOSE(IUINIT)
    CALL UTL_STOP &
    ('ERROR READING FN._INIT in this directory')
    RETURN
  END SUBROUTINE UTLUCODE_DX_READ_INIT
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_WRITE_PA(ITERP,MAXITER,NPS,OUTNAM,PARNAM, &
                             PVALINIT,PAREST,PVAL)
    !   Write _pa and _pasub DX files:
    !     _pa -- Parameter estimates listed by parameter, then by iteration
    !     _pasub -- Parameter estimates listed by iteration, then by parameter,
    !             formatted for subsitution into a PARAMETER_VALUES input block
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                    INTENT(IN) :: ITERP
    INTEGER,                                    INTENT(IN) :: MAXITER
    INTEGER,                                    INTENT(IN) :: NPS
    CHARACTER(LEN=*),                           INTENT(IN) :: OUTNAM
    CHARACTER(LEN=12), DIMENSION(NPS),          INTENT(IN) :: PARNAM
    DOUBLE PRECISION, DIMENSION(NPS),           INTENT(IN) :: PVALINIT
    DOUBLE PRECISION, DIMENSION(MAXITER,NPS),   INTENT(IN) :: PAREST
    DOUBLE PRECISION, DIMENSION(NPS), OPTIONAL, INTENT(IN) :: PVAL
    !
    !   Local variables
    INTEGER :: I, IUPA, IUPASUB, J
    !
    !   Format statements
    100 FORMAT(1X,'PARAMETER: ',A,/,1X,'"ITERATION"  "ESTIMATE"')
    101 FORMAT(1X,I5,6X,1PE15.7)
    200 FORMAT(1X,'ITERATION: ',I5,/,1X,'"PARAMETER"  "ESTIMATE"')
    201 FORMAT(1X,A,6X,1PE25.16)
    !
    !   Write parameter estimates to _pa file
    IUPA = UTL_DX_OPEN(OUTNAM,'_pa','REPLACE')
    DO I = 1,NPS
      WRITE(IUPA,100)PARNAM(I)
      WRITE (IUPA,101) 0,PVALINIT(I)
      DO J=1,ITERP
        WRITE(IUPA,101)J,PAREST(J,I)
      ENDDO
      IF (PRESENT(PVAL)) WRITE (IUPA,101) ITERP+1,PVAL(I)
    ENDDO
    IUPA = UTL_DX_CLOSE('_pa')
    !
    !   Write parameter estimates to _pasub file
    IUPASUB = UTL_DX_OPEN(OUTNAM,'_pasub','REPLACE')
    DO I = 1,ITERP
      WRITE(IUPASUB,200)I
      DO J=1,NPS
        WRITE(IUPASUB,201)PARNAM(J),PAREST(I,J)
      ENDDO
    ENDDO
    IUPASUB = UTL_DX_CLOSE('_pasub')
    !
    RETURN
  END SUBROUTINE UTLUCODE_DX_WRITE_PA
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_WRITE_INIT(NPS,CEVINIT,LN,OUTNAM, &
                                     PARNAM,PINC,PVAL)
    !     WRITE _INIT DX FILE Initial CEV
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                               INTENT(IN)  :: NPS
    DOUBLE PRECISION,                      INTENT(IN)  :: CEVINIT
    INTEGER,                               INTENT(IN)  :: LN(NPS)
    CHARACTER(LEN=MAX_STRING_LEN),         INTENT(IN)  :: OUTNAM
    CHARACTER(LEN=12),                     INTENT(IN)  :: PARNAM(NPS)
    INTEGER,                               INTENT(IN)  :: PINC(NPS)
    DOUBLE PRECISION,                      INTENT(IN)  :: PVAL(NPS)
    ! Local variables
    INTEGER                       :: I
    INTEGER                       :: IUINIT
    CHARACTER(LEN=MAX_STRING_LEN) :: OUTNAMTMP
    !   Format statements
    100 FORMAT(1X, '"CALCULATED ERROR VARIANCE (INITIAL):" ',G19.12)
    300 FORMAT(1X,'"PARAMETER" "OPTIMAL VALUE" "LOG TRANSFORM Native=0 Log=1"',&
    ' "PINC"')
    301 FORMAT(1X,A,6X,1PE25.16,2I5)
    !
    IUINIT = UTL_GETUNIT(101,150)
    OUTNAMTMP = TRIM(OUTNAM)//'._init'
    OPEN(UNIT=IUINIT,FILE=OUTNAMTMP,STATUS='REPLACE')
    WRITE(IUINIT,300)
    DO I = 1,NPS
      WRITE(IUINIT,301)PARNAM(I),PVAL(I),LN(I),PINC(I)
    ENDDO
    CLOSE(IUINIT)
    OUTNAMTMP = TRIM(OUTNAM)//'._init._dm'
    OPEN(UNIT=IUINIT,FILE=OUTNAMTMP,STATUS='REPLACE')
    WRITE(IUINIT,100)CEVINIT
    CLOSE(IUINIT)
    !CALL UTL_DX_WRITE_WT(OUTNAMTMP,'_wt',WTFULL,WTFULLSQR)
    !
    RETURN
  END SUBROUTINE UTLUCODE_DX_WRITE_INIT
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_WRITE_RESID(IOUT,MPR,NOBS,MODELVAL,MODELPRIVAL, &
                OBSNAM,OBSVAL,OUTNAM,PLOTSYMBOLPRI,PRINAM,PRIVAL,PRIWTMATSQR, &
                RESIDS,RESIDSPRI,WTDRESIDS,WTDRESIDSPRI,WTMATSQR)
    !   Write _OS _WS _WW _R _W,  Data-Exchange files: Residuals
    USE GLOBAL_DATA, ONLY: BIGREAL, LENDNAM, IVERB, MAX_STRING_LEN
    USE DATATYPES
    USE UTILITIES
    USE DEPENDENTS, ONLY: DEP_GET_GROUP, NTOTOBS, NUSEOBS
    USE STATISTICS, ONLY: WTDOBS
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: IOUT
    INTEGER,                                 INTENT(IN) :: MPR
    INTEGER,                                 INTENT(IN) :: NOBS
    DOUBLE PRECISION,       DIMENSION(NOBS), INTENT(IN) :: MODELVAL
    DOUBLE PRECISION,       DIMENSION(MPR),  INTENT(IN) :: MODELPRIVAL
    CHARACTER(LEN=LENDNAM), DIMENSION(NOBS), INTENT(IN) :: OBSNAM
    DOUBLE PRECISION,       DIMENSION(NOBS), INTENT(IN) :: OBSVAL
    CHARACTER(LEN=MAX_STRING_LEN),           INTENT(IN) :: OUTNAM
    INTEGER,                DIMENSION(MPR),  INTENT(IN) :: PLOTSYMBOLPRI
    CHARACTER(LEN=LENDNAM), DIMENSION(MPR),  INTENT(IN) :: PRINAM
    DOUBLE PRECISION,       DIMENSION(MPR),  INTENT(IN) :: PRIVAL
    TYPE (CDMATRIX),                         INTENT(IN) :: PRIWTMATSQR
    DOUBLE PRECISION,       DIMENSION(NOBS), INTENT(IN) :: RESIDS
    DOUBLE PRECISION,       DIMENSION(MPR),  INTENT(IN) :: RESIDSPRI
    DOUBLE PRECISION,       DIMENSION(NOBS), INTENT(IN) :: WTDRESIDS
    DOUBLE PRECISION,       DIMENSION(MPR),  INTENT(IN) :: WTDRESIDSPRI
    TYPE (CDMATRIX),                         INTENT(IN) :: WTMATSQR
    !
    !   Local variables
    INTEGER :: IUOS, IUR, IUXYZWR, IUWS, IUW, IUWW, IUXYZ
    INTEGER :: I, IPLOT, ICNT, J, N, NMNOBS, NXYZTS
    CHARACTER(LEN=12)                           :: GPNAM
    CHARACTER(LEN=MAX_STRING_LEN)               :: FN
    CHARACTER(LEN=2000)                         :: HEADER
    LOGICAL                                     :: FXYZ=.FALSE.
    LOGICAL                                     :: OK = .FALSE., OK2
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: WTDPRIVAL
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: WTDSIMPRI
    CHARACTER(LEN=LENDNAM), ALLOCATABLE, &
                                   DIMENSION(:) :: XYZNAM,TMPOBSNAM,TMPXYZNAM
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: XCOORD,YCOORD,ZCOORD,TCOORD,&
                                          LXCOORD,LYCOORD,LZCOORD,LTCOORD
    CHARACTER(LEN=LENDNAM)                      :: TEMPXYZNAM
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: WTDSIM
    !
    !   Format statements
    200 FORMAT(1X,'HEADER OF .xyzt file is: ',A)
    201 FORMAT(1X,'DEBUGGING .xyzt file',/, &
               1X,'SEARCH for values equal to: ',G15.7,' to identify ', &
               'missing names or values of x, y, z, or t')
    210 FORMAT(/,80('!'),/,1X,'NOT ENOUGH NAMES IN .xyzt file, ', &
               '_xyztwr file will not be written',/,1X, &
               'PERHAPS the HEADER IS MISSING in the .xyzt file?',/,80('!'),/)
    220 FORMAT(/,80('!'),/,1X,A,'NOT FOUND IN .xyzt file, ', &
               '_xyztwr file will not be written',/,80('!'),/)
    230 FORMAT(/,80('!'),/,1X,'IS the HEADER MISSING in the .xyzt file?',/,1X, &
               'PERHAPS SOME NAMES in .xyzt are NOT FOLLOWED by 4 NUMBERS?',/, &
               1X,'TRY VERBOSE=3 in the OPTIONS BLOCK to check .xyzt', &
               /,1X,'_xyztwr file will not be written',1X,/,80('!'),/)
    240 FORMAT(1X,' VERBOSE>=3 ',A,4(3X,G15.7))
    520 FORMAT(3X,G15.7,3X,G15.7,11X,I5,9X,A)
    522 FORMAT(2X,G15.7,12X,G15.7,23X,I5,7X,A)
    523 FORMAT(2X,G15.7,4X,G15.7,7X,I5,7X,A)
    530 FORMAT(G15.7,6X,I5,7X,A)
    540 FORMAT(G15.7,10X,I5,7X,A)
    600 FORMAT(1X,'"SIMULATED EQUIVALENT" "OBSERVED or PRIOR VALUE"', &
        ' "PLOT SYMBOL" "OBSERVATION or PRIOR NAME"')
    601 FORMAT(1X,'"WEIGHTED SIMULATED EQUIVALENT"', &
        ' "WEIGHTED OBSERVED or PRIOR VALUE"', &
        ' "PLOT SYMBOL" "OBSERVATION or PRIOR NAME"')
    602 FORMAT(1X,'"SIMULATED EQUIVALENT"', &
        ' "WEIGHTED RESIDUAL" "PLOT SYMBOL" "OBSERVATION or PRIOR NAME"')
    603 FORMAT(1X,'"RESIDUAL"      "PLOT SYMBOL"', &
        ' "OBSERVATION or PRIOR NAME"')
    604 FORMAT(1X,'"WEIGHTED RESIDUAL" "PLOT SYMBOL"', &
        ' "OBSERVATION or PRIOR NAME"')
    605 FORMAT(1X,'"X" "Y" "Z" "T" "WEIGHTED RESIDUAL"', &
             ' "RESIDUAL" "PLOT SYMBOL" "OBSERVATION or PRIOR NAME"')
    !
    ALLOCATE(WTDPRIVAL(MPR),WTDSIMPRI(MPR),WTDSIM(NOBS))
    !
    NXYZTS = 0
    !   Open data exchange files
    !   Set up control of output
    IUOS = 0
    IUR = 0
    IUW = 0
    IUWS = 0
    IUWW = 0
    !
    !   Calculate weighted simulated equivalents
    CALL UTL_MATMULVEC_SUB(NOBS,WTMATSQR,MODELVAL,WTDSIM)
    !
    IUOS = UTL_DX_OPEN(OUTNAM,'_os','REPLACE')
    IUR = UTL_DX_OPEN(OUTNAM,'_r','REPLACE')
    IUW = UTL_DX_OPEN(OUTNAM,'_w','REPLACE')
    IUWS = UTL_DX_OPEN(OUTNAM,'_ws','REPLACE')
    IUWW = UTL_DX_OPEN(OUTNAM,'_ww','REPLACE')
    ! write headers
    IF (IUOS .GT. 0) WRITE(IUOS,600)
    IF (IUWW .GT. 0) WRITE(IUWW,601)
    IF (IUWS .GT. 0) WRITE(IUWS,602)
    IF (IUR .GT. 0) WRITE(IUR,603)
    IF (IUW .GT. 0) WRITE(IUW,604)
    ! write files
    EACHOBS: DO N=1,NOBS+MPR
      IF(N <= NOBS) THEN
        CALL DEP_GET_GROUP(N,GPNAM,IPLOT,'USED')
        !### check for h(n).eq.romit after calculation with new parameters,
        !### before entering regression if(h(n).eq.romit) wt(n)=-wt(n)
        !### before this, initialize all weights to all be positive
          WRITE(IUOS,520) MODELVAL(N),OBSVAL(N),IPLOT,OBSNAM(N)
          WRITE(IUWW,522) WTDSIM(N),WTDOBS(N),IPLOT,OBSNAM(N)
          WRITE(IUWS,523) MODELVAL(N),WTDRESIDS(N),IPLOT,OBSNAM(N)
          !-----------plot files that include unweighted and weighted residuals
          WRITE(IUR,530) RESIDS(N),IPLOT,OBSNAM(N)
          WRITE(IUW,540) WTDRESIDS(N),IPLOT,OBSNAM(N)
      ELSEIF(N > NOBS) THEN !N>NOBS so we are dealing with prior
        NMNOBS=N-NOBS
        !   Calculate weighted simulated equivalents
        WRITE(IUOS,520) MODELPRIVAL(NMNOBS),PRIVAL(NMNOBS), &
                                         PLOTSYMBOLPRI(NMNOBS),PRINAM(NMNOBS)
        CALL UTL_MATMULVEC_SUB(MPR,PRIWTMATSQR,MODELPRIVAL,WTDSIMPRI)
        CALL UTL_MATMULVEC_SUB(MPR,PRIWTMATSQR,PRIVAL,WTDPRIVAL)
        WRITE(IUWW,522) WTDSIMPRI(NMNOBS),WTDPRIVAL(NMNOBS), &
                                         PLOTSYMBOLPRI(NMNOBS),PRINAM(NMNOBS)
        WRITE(IUWS,523) MODELPRIVAL(NMNOBS),WTDRESIDSPRI(NMNOBS), &
                                         PLOTSYMBOLPRI(NMNOBS),PRINAM(NMNOBS)
        !-----------plot files that include unweighted and weighted residuals
        WRITE(IUR,530) RESIDSPRI(NMNOBS),PLOTSYMBOLPRI(NMNOBS),PRINAM(NMNOBS)
        WRITE(IUW,540) WTDRESIDSPRI(NMNOBS),PLOTSYMBOLPRI(NMNOBS),PRINAM(NMNOBS)
      ENDIF
    ENDDO EACHOBS
    IUOS = UTL_DX_CLOSE('_os')
    IUR = UTL_DX_CLOSE('_r')
    IUW = UTL_DX_CLOSE('_w')
    IUWS = UTL_DX_CLOSE('_ws')
    IUWW = UTL_DX_CLOSE('_ww')
    !
    !   XYZ data
    FN = TRIM(OUTNAM)//'.xyzt'
    INQUIRE (FILE=FN,EXIST=FXYZ)
    IF(FXYZ) THEN
      OK2 = .TRUE.
      IUXYZ = UTL_GETUNIT(101,150)
      OPEN(UNIT=IUXYZ,FILE=FN,STATUS='OLD')
      IUXYZWR = UTL_DX_OPEN(OUTNAM,'_xyztwr','REPLACE')
      ALLOCATE(TMPOBSNAM(NUSEOBS),XCOORD(NUSEOBS),YCOORD(NUSEOBS), &
               ZCOORD(NUSEOBS),TCOORD(NUSEOBS))
      READ(IUXYZ,*,END=997)HEADER ! SKIP HEADER
      WRITE(IUXYZWR,200)TRIM(HEADER) ! SKIP HEADER
      DO
        READ(IUXYZ,*,END=997)TEMPXYZNAM
        NXYZTS = NXYZTS + 1
      ENDDO
      997 CLOSE(UNIT = IUXYZ)
      IF (NUSEOBS .GT. NXYZTS+1) THEN
        WRITE(*,210)
        WRITE(IOUT,210)
        WRITE(IUXYZWR,210)
      ENDIF
      ALLOCATE(XYZNAM(NXYZTS),TMPXYZNAM(NXYZTS),LXCOORD(NXYZTS), &
                              LYCOORD(NXYZTS),LZCOORD(NXYZTS),LTCOORD(NXYZTS))
      TMPXYZNAM = " "
      LXCOORD = -BIGREAL
      LYCOORD = -BIGREAL
      LZCOORD = -BIGREAL
      LTCOORD = -BIGREAL
      OPEN(UNIT=IUXYZ,FILE=FN,STATUS='OLD')
      READ(IUXYZ,*) ! SKIP HEADER
      IF(IVERB < 3) THEN
        DO N=1,NXYZTS
          READ(IUXYZ,*,END=998)XYZNAM(N),LXCOORD(N), &
                               LYCOORD(N),LZCOORD(N),LTCOORD(N)
        ENDDO
      ELSE
        WRITE(IUXYZWR,201)-BIGREAL
        DO N=1,NXYZTS
          READ(IUXYZ,*,END=998)XYZNAM(N),LXCOORD(N), &
                               LYCOORD(N),LZCOORD(N),LTCOORD(N)
          WRITE(IUXYZWR,240)XYZNAM(N),LXCOORD(N), &
                               LYCOORD(N),LZCOORD(N),LTCOORD(N)
        ENDDO
      ENDIF
      998 IF(N .NE. NXYZTS+1) THEN
        OK2 = .FALSE.
        WRITE(*,230)
        WRITE(IOUT,230)
        WRITE(IUXYZWR,230)
      ENDIF
      IF (OK2) THEN
        TMPOBSNAM = ' '
        TMPXYZNAM = ' '
        OK = .TRUE.
        DO I=1,NUSEOBS
          CALL UTL_CASE(OBSNAM(I),TMPOBSNAM(I),-1)
        ENDDO
        DO I=1,NXYZTS
          CALL UTL_CASE(XYZNAM(I),TMPXYZNAM(I),-1)
        ENDDO
        ICNT = 0
        FINDNAME: DO I=1,NUSEOBS
          LOOPXYZ: DO J = 1,NXYZTS
            ICNT = ICNT+1
            IF (ICNT > NXYZTS) ICNT = 1
            IF(TMPOBSNAM(I) .NE. TMPXYZNAM(ICNT)) THEN
              IF(J .EQ. NXYZTS) THEN
                WRITE(*,220)TMPOBSNAM(I)
                WRITE(IOUT,220)TMPOBSNAM(I)
                WRITE(IUXYZWR,220)TMPOBSNAM(I)
                OK = .FALSE.
                EXIT FINDNAME
              ENDIF
              CYCLE LOOPXYZ
            ELSE
              XCOORD(I)=LXCOORD(ICNT)
              YCOORD(I)=LYCOORD(ICNT)
              ZCOORD(I)=LZCOORD(ICNT)
              TCOORD(I)=LTCOORD(ICNT)
              EXIT LOOPXYZ
            ENDIF
          ENDDO LOOPXYZ
        ENDDO FINDNAME
        IF (OK) THEN
          IF (IUXYZWR .GT. 0) THEN
            IUXYZWR = UTL_DX_CLOSE('_xyztwr')
            IUXYZWR = UTL_DX_OPEN(OUTNAM,'_xyztwr','REPLACE')
            WRITE(IUXYZWR,605)
          ENDIF
          DO N=1,NUSEOBS
            CALL DEP_GET_GROUP(N,GPNAM,IPLOT,'USED')
            WRITE(IUXYZWR,*)XCOORD(N),YCOORD(N),ZCOORD(N),TCOORD(N), &
                            WTDRESIDS(N),RESIDS(N),IPLOT,OBSNAM(N)
          ENDDO
        ENDIF
      ENDIF
      ! Close data exchange file
      CLOSE(UNIT=IUXYZ)
      IUXYZWR = UTL_DX_CLOSE('_xyztwr')
      DEALLOCATE (XYZNAM,XCOORD,YCOORD,ZCOORD,TCOORD,TMPOBSNAM,TMPXYZNAM, &
                  LXCOORD,LYCOORD,LZCOORD,LTCOORD)
    ENDIF
    DEALLOCATE(WTDPRIVAL,WTDSIMPRI,WTDSIM)
    RETURN
  END SUBROUTINE UTLUCODE_DX_WRITE_RESID
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_READ_PAOPT(NPS,OUTNAM,PALN,PANAM,PAPINCR,PAPVAL, &
                                    NPSWOP,EXT)
    !     READ _PAOPT DX FILE Parameter INFORMATION
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                    INTENT(IN)  :: NPS
    CHARACTER(LEN=MAX_STRING_LEN),              INTENT(IN)  :: OUTNAM
    INTEGER,                                    INTENT(OUT) :: PALN(NPS)
    CHARACTER(LEN=12),                          INTENT(OUT) :: PANAM(NPS)
    INTEGER,                                    INTENT(OUT) :: PAPINCR(NPS)
    DOUBLE PRECISION,                           INTENT(OUT) :: PAPVAL(NPS)
    INTEGER,                         OPTIONAL,  INTENT(IN)  :: NPSWOP
    CHARACTER(LEN=*),                OPTIONAL,  INTENT(IN)  :: EXT
    !
    !   Local variables
    CHARACTER(LEN=MAX_STRING_LEN)                          :: FN
    INTEGER                                                :: I
    INTEGER                                                :: IUPA1
    LOGICAL                                                :: LEX = .FALSE.
    INTEGER                                                :: N
    !
    200 FORMAT(1X,'Error opening file: ',A)
    !   Open existing _paopt file
    IF(PRESENT(EXT)) THEN
      FN = TRIM(OUTNAM)//'._paopt'//TRIM(EXT)
    ELSE
      FN = TRIM(OUTNAM)//'._paopt'
    ENDIF
    INQUIRE(FILE=TRIM(FN),EXIST=LEX)
    IF(LEX) THEN
      IUPA1 = UTL_GETUNIT(101,150)
      OPEN(UNIT=IUPA1,FILE=FN,STATUS='OLD')
    ELSE
      WRITE(*,200)TRIM(FN)
      CALL UTL_STOP()
    ENDIF
    ! READ Header
    READ (IUPA1,*,END=100)
    ! Read requested values
    N = NPS
    IF(PRESENT(NPSWOP))  THEN
      IF(NPSWOP > NPS)CALL UTL_STOP &
      ('Programmer error? UTLUCODE_DX_READ_PAOPT: # to be read > size of array')
      N = NPSWOP
    ENDIF
    PAPINCR = 1
    DO I=1,N
      READ (IUPA1,*,END=100)PANAM(I),PAPVAL(I),PALN(I),PAPINCR(I)
    ENDDO
    CLOSE(IUPA1)
    !
    RETURN
    100 CLOSE(IUPA1)
    CALL UTL_STOP &
         ('UNEXPECTED CONTENT _paopt file, CHECK VERSION/RE-CREATE _paopt')
    RETURN
  END SUBROUTINE UTLUCODE_DX_READ_PAOPT
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_WRITE_PAOPT(NPS,LN,OUTNAM,PARNAM,PINC,PVAL,EXT)
    !     WRITE _PA PAOPT DX FILE Parameter INFORMATION
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                               INTENT(IN)  :: NPS
    INTEGER,                               INTENT(IN)  :: LN(NPS)
    CHARACTER(LEN=MAX_STRING_LEN),         INTENT(IN)  :: OUTNAM
    CHARACTER(LEN=12),                     INTENT(IN)  :: PARNAM(NPS)
    INTEGER,                               INTENT(IN)  :: PINC(NPS)
    DOUBLE PRECISION,                      INTENT(IN)  :: PVAL(NPS)
    CHARACTER(LEN=*), OPTIONAL,            INTENT(IN)  :: EXT
    !   Local variables
    INTEGER                                            :: I
    INTEGER                                            :: IUPAOPT
    CHARACTER(LEN=MAX_STRING_LEN)                      :: OUTNAMTMP
    !
    !   Format statements
    300 FORMAT &
      (1X,' "PARAMETER"  "OPTIMAL VALUE" "LOG TRANSFORM Native=0 Log=1" "PINC"')
    301 FORMAT(1X,A,6X,1PE25.16,2I5)
    !
    IUPAOPT = UTL_GETUNIT(101,150)
    IF(PRESENT(EXT)) THEN
      OUTNAMTMP = TRIM(OUTNAM)//'.'//TRIM(EXT)
    ELSE
      OUTNAMTMP = TRIM(OUTNAM)//'._paopt'
    ENDIF
    OPEN(UNIT=IUPAOPT,FILE=OUTNAMTMP,STATUS='REPLACE')
    WRITE(IUPAOPT,300)
    DO I = 1,NPS
      WRITE(IUPAOPT,301)PARNAM(I),PVAL(I),LN(I),PINC(I)
    ENDDO
    CLOSE(IUPAOPT)
    !
    RETURN
  END SUBROUTINE UTLUCODE_DX_WRITE_PAOPT
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_READ_PAPRI(MPR,NPS,OUTNAM,PARNAM,PINC,EQTN, &
                                    OMITPRI,SUMOMITPRI)
    !     READ _PAPRI DX FILE and determine which prior equations contain
    !     only parameters that have been omitted from the regression
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                               INTENT(IN)  :: MPR
    INTEGER,                               INTENT(IN)  :: NPS
    CHARACTER(LEN=MAX_STRING_LEN),         INTENT(IN)  :: OUTNAM
    CHARACTER(LEN=12),                     INTENT(OUT) :: PARNAM(NPS)
    INTEGER,                               INTENT(OUT) :: PINC(NPS)
    INTEGER,                               INTENT(OUT) :: EQTN(NPS,MPR)
    INTEGER,                               INTENT(OUT) :: OMITPRI(MPR)
    INTEGER,                               INTENT(OUT) :: SUMOMITPRI
    !
    !   Local variables
    INTEGER                                                :: I
    INTEGER                                                :: IUPAPRI
    INTEGER                                                :: J
    CHARACTER(LEN=MAX_STRING_LEN)                          :: OUTNAMTMP
    !
    IUPAPRI = UTL_GETUNIT(101,150)
    OUTNAMTMP = TRIM(OUTNAM)//'._papri'
    OPEN(UNIT=IUPAPRI,FILE=OUTNAMTMP,STATUS='OLD')
    ! READ Header
    READ (IUPAPRI,*,END=100)
    ! Read requested values
    DO I=1,NPS
      READ (IUPAPRI,*,END=100)PARNAM(I),PINC(I),(EQTN(I,J),J=1,MPR)
    ENDDO
    CLOSE(IUPAPRI)
    ! Determine which prior equations contain only omitted parameters
    OMITPRI = 0
    DO J=1,MPR
      DO I=1,NPS
        IF(EQTN(I,J) == 1)THEN !at least one parameter is still active
          IF(PINC(I) > 0)THEN
            OMITPRI(J) = 0 !zero in case a previous parameter was not & move on
            EXIT
          ELSE             !willnot get here if a previous parameter was active
            OMITPRI(J) = 1 !maybe reset to 0 if a later parameter is active
          ENDIF
        ENDIF
      ENDDO
    ENDDO
    SUMOMITPRI = 0
    DO J=1,MPR
      IF(OMITPRI(J) == 1)SUMOMITPRI = SUMOMITPRI + 1
    ENDDO
    !
    RETURN
    100 CLOSE(IUPAPRI)
    CALL UTL_STOP &
         ('UNEXPECTED CONTENT _papri file, CHECK VERSION/RE-CREATE _papri')
    RETURN
  END SUBROUTINE UTLUCODE_DX_READ_PAPRI
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_WRITE_PAPRI(MPR,NPE,NPS,OUTNAM,PARNAM,PINC,XPRI)
    !     WRITE _PA PAPRI DX FILE Parameter INFORMATION
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                               INTENT(IN)  :: MPR
    INTEGER,                               INTENT(IN)  :: NPE
    INTEGER,                               INTENT(IN)  :: NPS
    CHARACTER(LEN=MAX_STRING_LEN),         INTENT(IN)  :: OUTNAM
    CHARACTER(LEN=12),                     INTENT(IN)  :: PARNAM(NPS)
    INTEGER,                               INTENT(IN)  :: PINC(NPS)
    DOUBLE PRECISION,                      INTENT(IN)  :: XPRI(NPE,MPR)
    !   Local variables
    INTEGER                                            :: EQTN(MPR)
    INTEGER                                            :: I
    INTEGER                                            :: IUPAPRI
    CHARACTER(LEN=200)                                 :: FMT_STRING
    INTEGER                                            :: J
    INTEGER                                            :: K
    CHARACTER(LEN=MAX_STRING_LEN)                      :: OUTNAMTMP
    !
    !   Format statements
    300 FORMAT &
      (1X,' "PARAMETER"  "PINC"  "1 IF PRESENT IN EACH OF MPR EQUATIONS"')
    WRITE(FMT_STRING, '(I2)') MPR
    FMT_STRING = '(1X,A12,4X,I2,4X,'//TRIM(ADJUSTL(FMT_STRING))//'(I2))'
    !
    IUPAPRI = UTL_GETUNIT(101,150)
    OUTNAMTMP = TRIM(OUTNAM)//'._papri'
    OPEN(UNIT=IUPAPRI,FILE=OUTNAMTMP,STATUS='REPLACE')
    WRITE(IUPAPRI,300)
    J = 0
    DO I = 1,NPS
      EQTN = 0
      J = J + 1
      IF(PINC(I) < 0) THEN
        J = J - 1
        WRITE(IUPAPRI,FMT_STRING)PARNAM(I),PINC(I),(EQTN(K),K=1,MPR)
      ELSE
        DO K=1,MPR
          IF(XPRI(J,K) .NE. 0.D0) EQTN(K) = 1
        ENDDO
        WRITE(IUPAPRI,FMT_STRING)PARNAM(I),PINC(I),(EQTN(K),K=1,MPR)
      ENDIF
    ENDDO
    CLOSE(IUPAPRI)
    !
    RETURN
  END SUBROUTINE UTLUCODE_DX_WRITE_PAPRI
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_WRITEPARS(IFAIL,IOUT,NPS,BSCAL,LN,PADJ,PARGP,PARNAM, &
                        PERTURB,PMAXCHANGE,PTOLPAR,PVAL,PVALMAX,PVALMIN,SVDSET)
    !   Print parameter-related data
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                         INTENT(INOUT) :: IFAIL
    INTEGER,                         INTENT(IN)    :: IOUT
    INTEGER,                         INTENT(IN)    :: NPS
    DOUBLE PRECISION,                INTENT(IN)    :: BSCAL(NPS)
    INTEGER,                         INTENT(IN)    :: LN(NPS)
    LOGICAL,                         INTENT(IN)    :: PADJ(NPS)
    CHARACTER(LEN=12),               INTENT(IN)    :: PARGP(NPS)
    CHARACTER(LEN=12),               INTENT(IN)    :: PARNAM(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: PERTURB(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: PMAXCHANGE(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: PTOLPAR(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: PVAL(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: PVALMAX(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: PVALMIN(NPS)
    INTEGER,                         INTENT(IN)    :: SVDSET(NPS)
    !   Local variables
    INTEGER :: I
    !
    201 FORMAT(/,   &
        1X,'No. ',1X,'Param. name ',4X,'Group',8X,'Value',7X,'Lower value',  &
        3X,'Upper value',4X,'Adj?',/,   &
        1X,4('-'),1X,12('-'),1X,12('-'),3(2x,12('-')),2X,5('-'))
    202 FORMAT(/,   &
        1X,'No. ',1X,'Param. name ',2X,'LN',3X,'SCALEPVAL' &
        ,3X,'PERTURB',4X,'MAXCHANGE',4X,'TOLPAR',4X,'SVDSET',/,   &
        1X,4('-'),1X,12('-'),2X,  &
        '--',2X,10('-'),2X,10('-'),2X,9('-'),2X,9('-'),4X,6('-'))
    210 FORMAT(1X,I4,1X,A,1X,A,3(1X,1PG13.6),3X,A1)
    220 FORMAT(1X,I4,1X,A,2X,I2,2X,1PG10.3,2X,1PG10.3,1X,1PG10.3,1X, &
               1PG10.3,4X,I7)
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine UTLUCODE_WRITEPARS'
    IFAIL = 0
    !   Write data for each parameter in the parameter set
    WRITE(IOUT,201)
    DO I=1,NPS
      IF(PADJ(I)) THEN
        WRITE(IOUT,210)I,PARNAM(I),PARGP(I),PVAL(I),PVALMIN(I),PVALMAX(I),'Y'
      ELSE
        WRITE(IOUT,210)I,PARNAM(I),PARGP(I),PVAL(I),PVALMIN(I),PVALMAX(I),'N'
      ENDIF
    ENDDO
    WRITE(IOUT,202)
    DO I=1,NPS
      WRITE(IOUT,220)I,PARNAM(I),LN(I),BSCAL(I),PERTURB(I),PMAXCHANGE(I), &
                     PTOLPAR(I),SVDSET(I)
    ENDDO
    !
    RETURN
  END SUBROUTINE UTLUCODE_WRITEPARS
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_WRITE_PC(NPE,NPS,IPTR,LN,OUTNAM,PARNAM,PRNT, &
                           PVALMAX,PVALMIN)
    !     WRITE _PC DATA EXCHANGE FILE - Final PARAMETERS
    !                          THEIR CONFIDENCE INTERVALS & EXPECTED MIN MAX
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                              INTENT(IN) :: NPE
    INTEGER,                              INTENT(IN) :: NPS
    INTEGER,              DIMENSION(NPE), INTENT(IN) :: IPTR
    INTEGER,              DIMENSION(NPS), INTENT(IN) :: LN
    CHARACTER(LEN=MAX_STRING_LEN),        INTENT(IN) :: OUTNAM
    CHARACTER(LEN=12),  DIMENSION(NPS),   INTENT(IN) :: PARNAM
    DOUBLE PRECISION,  DIMENSION(NPE,10), INTENT(IN) :: PRNT
    DOUBLE PRECISION,   DIMENSION(NPS),   INTENT(IN) :: PVALMAX
    DOUBLE PRECISION,   DIMENSION(NPS),   INTENT(IN) :: PVALMIN
    !
    !   Local variables
    INTEGER IP, IIPP, IUPC
    INTEGER PREASONABLE, INTREASONABLE
    !
    !   Format statements
    670 FORMAT(' "PARAMETER NAME" "OPTIMAL VALUE (NATIVE)" ', &
        '"LOWER LIMIT (NATIVE)" "UPPER LIMIT (NATIVE)" ', &
        '"LOG TRANSFORM Native=0 Log=1" ', &
        '"OPTIMAL VALUE (REGRESSION)" "STANDARD DEVIATION (REGRESSION)" ', &
        '"COEFFICIENT OF VARIATION (REGRESSION)"  "STANDARD DEVIATION ', &
        '(NATIVE)"  "COEFFICIENT OF VARIATION (NATIVE)" ', &
        '"REASONABLE RANGE MINIMUM" "REASONABLE RANGE MAXIMUM" ', &
        '"IN REASONABLE RANGE? Yes=1 No=0" ', &
        '"CONFIDENCE INTERVAL INCLUDES REASONABLE VALUES? Yes=1 No=0"')
    671 FORMAT(1X,A,5X,G15.7,6X,G15.7,6X,G15.7,I15,10X,5(8X,G26.13), &
               2(8X,ES26.13E3),2I30)
    ! Open Exchange File
    IUPC = UTL_DX_OPEN(OUTNAM,'_pc','REPLACE')
    ! Write Header
    WRITE(IUPC,670)
    ! print native values
    DO IP = 1, NPE
      PREASONABLE = 0
      INTREASONABLE = 0
      IIPP = IPTR(IP)
      IF(PVALMIN(IIPP) <= PRNT(IP,2) .AND. PRNT(IP,2) <= PVALMAX(IIPP)) &
        PREASONABLE = 1
      IF((PRNT(IP,7) < PVALMIN(IIPP) .AND. PVALMIN(IIPP) < PRNT(IP,5)) .OR. &
         (PRNT(IP,7) < PVALMAX(IIPP) .AND. PVALMAX(IIPP) < PRNT(IP,5)) .OR. &
         (PVALMIN(IIPP) < PRNT(IP,7) .AND. PRNT(IP,7) < PVALMAX(IIPP)) .OR. &
         (PVALMIN(IIPP) < PRNT(IP,5) .AND. PRNT(IP,5) < PVALMAX(IIPP))) &
        INTREASONABLE = 1
      WRITE(IUPC,671)PARNAM(IIPP),PRNT(IP,2),PRNT(IP,7),PRNT(IP,5),LN(IIPP), &
                     PRNT(IP,1),PRNT(IP,3),ABS(PRNT(IP,3)/PRNT(IP,1)), &
                     PRNT(IP,10),ABS(PRNT(IP,10)/PRNT(IP,2)), &
                     PVALMIN(IIPP),PVALMAX(IIPP),PREASONABLE,INTREASONABLE
    ENDDO
    IUPC = UTL_DX_CLOSE('_pc')
    RETURN
  END SUBROUTINE UTLUCODE_DX_WRITE_PC
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_READ_PPV(IOUT,OUTNAM,NPRED,N,PREDGRP,PREDNAM,PREDVAL, &
                               PREDVAR,PLOTSYMBOL)
    !   READ _ppv Data-Exchange Files: Prediction Info
    USE GLOBAL_DATA, ONLY: AMESSAGE, MAX_STRING_LEN, LENDNAM
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                  INTENT(IN)  :: IOUT
    CHARACTER(LEN=MAX_STRING_LEN),            INTENT(IN)  :: OUTNAM
    INTEGER,                                  INTENT(IN)  :: NPRED
    LOGICAL,                                  INTENT(IN)  :: N
    CHARACTER(LEN=12), DIMENSION(NPRED),      INTENT(OUT) :: PREDGRP
    CHARACTER(LEN=LENDNAM), DIMENSION(NPRED), INTENT(OUT) :: PREDNAM
    DOUBLE PRECISION, DIMENSION(NPRED),       INTENT(OUT) :: PREDVAL
    DOUBLE PRECISION, DIMENSION(NPRED),       INTENT(OUT) :: PREDVAR
    INTEGER, DIMENSION(NPRED),                INTENT(OUT) :: PLOTSYMBOL
    !
    !   Local variables
    INTEGER                 :: I
    INTEGER                 :: IUP
    INTEGER                 :: IUPV
     CHARACTER(LEN=LENDNAM) :: NAM
    !
    !   Formats
    220 FORMAT(/,1X,'ERROR: Cannot open file "',A,'._p"')
    221 FORMAT(/,1X,'ERROR: Cannot open file "',A,'._pv"')
    !
    ! read  predictions
    IUP = UTL_DX_OPEN(OUTNAM,'_p','OLD')
    IF(IUP < 1) THEN
      WRITE(*,220)TRIM(OUTNAM)
      WRITE(IOUT,220)TRIM(OUTNAM)
      CALL UTL_STOP()
    ENDIF
    READ(IUP,*)
    ! read  prediction variance
    IF(N) THEN
      IUPV = UTL_DX_OPEN(OUTNAM,'_pv','OLD')
      IF(IUPV < 1) THEN
        WRITE(*,221)TRIM(OUTNAM)
        WRITE(IOUT,221)TRIM(OUTNAM)
        CALL UTL_STOP()
      ENDIF
      READ(IUPV,*)
    ENDIF
    DO I = 1,NPRED
      READ(IUP,*)PREDVAL(I),PLOTSYMBOL(I),PREDNAM(I)
      IF(N) THEN
        READ(IUPV,*)PREDVAR(I),NAM,PREDGRP(I)
        IF(TRIM(NAM) /= TRIM(PREDNAM(I))) THEN
          AMESSAGE = 'Names or Plotsymbols do not match in _p and _pv files'
          CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
          CALL UTL_WRITE_MESSAGE(-1,'yes','yes','yes')
          AMESSAGE = 'Rerun codes that generated prediction information'
          CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','yes')
          CALL UTL_WRITE_MESSAGE(-1,'no','yes','yes')
          CALL UTL_STOP(' ')
        ENDIF
      ENDIF
    ENDDO
    ! Close data exchange file
    IUP = UTL_DX_CLOSE('_p')
    IF(N) IUPV = UTL_DX_CLOSE('_pv')
    RETURN
  END SUBROUTINE UTLUCODE_DX_READ_PPV
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_WRITE_PREDS(IFAIL,IOUT,MPR,MPRWOP,NOBS,NPE,NPEFP, &
             NPT,DATAEXCHANGE,IPTR,LN,MODELVAL,PRINAM,PRIWTMAT, &
             PRIWTMATSQR,PVAL,OBSNAM,OUTNAM,OUTNAMPRED,PARNAM,WTFULL, &
             XPRI,XSENS,SENSITIVITIES)
  !  VERSION 2004_0507 EPP
  !  ***************************************************************************
  !  if in PREDICTION MODE
  !  WRITE _p _spu _spsr _spsp _sppr _sppp
  !
!   Eventually, writing of the _sppp, _sppr, _spsp, _spsr, and _spu files will
!   need to support the format dependent on NPPREC, but now they don't.
!   When this subroutine is revised to support that format, UTL_DX_RECL will
!   need to be edited to set RECL correctly, and this subroutine can be moved to
!   DEP.
!
  !  ***************************************************************************
  !     SPECIFICATIONS:
  !  ---------------------------------------------------------------------------
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN, NPPREC
    USE UTILITIES
    USE DEPENDENTS, ONLY: GROUPNAM, GROUPNOBS, NPREDGPS, USEFLAG, DEP_GET_GROUP
    USE PRIOR_INFORMATION, ONLY: PRI_UEV_DX_WRITE_PRP, &
    ! variables
    PLOTSYMBOLPRI
    USE SENSITIVITY, ONLY: SEN_UEV_DX_WRITE_MATRIX
    !
    !   Argument-list variables
    IMPLICIT NONE
    INTEGER,                                 INTENT(OUT):: IFAIL
    INTEGER,                                 INTENT(IN) :: IOUT
    INTEGER,                                 INTENT(IN) :: MPR
    INTEGER,                                 INTENT(IN) :: MPRWOP
    INTEGER,                                 INTENT(IN) :: NOBS
    INTEGER,                                 INTENT(IN) :: NPE
    INTEGER,                                 INTENT(IN) :: NPEFP
    INTEGER,                                 INTENT(IN) :: NPT
    LOGICAL,                                 INTENT(IN) :: DATAEXCHANGE
    INTEGER,                DIMENSION(NPE),  INTENT(IN) :: IPTR
    INTEGER,                DIMENSION(NPT),  INTENT(IN) :: LN
    DOUBLE PRECISION,       DIMENSION(NOBS), INTENT(IN) :: MODELVAL
    CHARACTER(LEN=LENDNAM),                  INTENT(IN) :: PRINAM(MPR)
    TYPE (CDMATRIX),                         INTENT(IN) :: PRIWTMAT
    TYPE (CDMATRIX),                         INTENT(IN) :: PRIWTMATSQR
    DOUBLE PRECISION,                        INTENT(IN) :: PVAL(NPT)
    CHARACTER(LEN=LENDNAM), DIMENSION(NOBS), INTENT(IN) :: OBSNAM
    CHARACTER(LEN=MAX_STRING_LEN),           INTENT(IN) :: OUTNAM
    CHARACTER(LEN=MAX_STRING_LEN),           INTENT(IN) :: OUTNAMPRED
    CHARACTER(LEN=12),      DIMENSION(NPT),  INTENT(IN) :: PARNAM
    TYPE(CDMATRIX),                          INTENT(IN) :: WTFULL
    DOUBLE PRECISION,   DIMENSION(NPE,MPR),  INTENT(IN) :: XPRI
    DOUBLE PRECISION,   DIMENSION(NPE,NOBS), INTENT(IN) :: XSENS
    LOGICAL,                                 INTENT(IN) :: SENSITIVITIES
    !
    ! Local Variables
    CHARACTER(LEN=MAX_STRING_LEN)                 :: CDUM
    DOUBLE PRECISION                              :: CEV
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: CMATFP
    CHARACTER(LEN=20)                             :: COL12NAM(2)
    DOUBLE PRECISION                              :: DIFF
    DOUBLE PRECISION                              :: DIVIDEBYZERO
    DOUBLE PRECISION                              :: DUM
    INTEGER                                       :: I
    INTEGER                                       :: IDUM
    INTEGER, ALLOCATABLE, DIMENSION(:)            :: IPTRSHT
    INTEGER                                       :: IUSU = 0
    INTEGER                                       :: IUY
    INTEGER                                       :: IUYDM
    INTEGER                                       :: IUYS
    INTEGER                                       :: IUYV
    INTEGER, ALLOCATABLE, DIMENSION(:)            :: ILOG
    INTEGER, ALLOCATABLE, DIMENSION(:)            :: IPLOT
    CHARACTER(LEN=60)                             :: FMT_STRING1
    CHARACTER(LEN=60)                             :: FMT_STRING2
    INTEGER, ALLOCATABLE, DIMENSION(:)            :: GGT,GNUMA,GNUMD,GNUMPD,GLT
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: GMAX,GMIN,GAVG,GDIFMAX, &
                                                     GDIFMIN,GDIFAVG, &
                                                     GPERDIFMAX,GPERDIFMIN, &
                                                     GPERDIFAVG
    CHARACTER(LEN=12)                             :: GPNAM
    INTEGER                                       :: J
    INTEGER                                       :: K
    INTEGER                                       :: MXRECL
    INTEGER                                       :: NMPRFP
    INTEGER                                       :: NPEWOP
    INTEGER                                       :: MYGP
    CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)  :: PARNAMSHT
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: PCOEF
    DOUBLE PRECISION                              :: PERDIFF
    CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)  :: PNAMTMP
    TYPE (CDMATRIX)                               :: PRIWTMATFP
    TYPE (CDMATRIX)                               :: PRIWTMATSQRFP
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: PSD
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: PSDREG
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: PVALREG
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: PVALNAT
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: PVAR
    DOUBLE PRECISION                              :: RDUM
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: XSP
    DOUBLE PRECISION                              :: WT
    !
    ! Formats
    201 FORMAT(///,1X,'PREDICTION SUMMARY STATISTICS:',/)
    202 FORMAT(1X,'  == SUMMARY STATISTICS FOR PREDICTION GROUPS ==')
    203 FORMAT(//,4(1X,18('-'),1X))
    204 FORMAT(5X,'GROUP NAME',5X,6X,'LARGEST',7X,6X,'SMALLEST',6X,6X, &
               'AVERAGE')
    205 FORMAT(5X,'GROUP NAME',5X,6X,'DIFF-MAX',6X,6X,'DIFF-MIN',6X,6X, &
               'DIFF-AVG')
    206 FORMAT(5X,'GROUP NAME',5X,5X,'%DIFF-MAX',6X,5X,'%DIFF-MIN',5X,6X, &
               '%DIFF-AVG')
    207 FORMAT(5X,'GROUP NAME',5X,6X,'# < REF',6X,6X,'# > REF')
    208 FORMAT(1X,//,2X, &
      '   *INTERPRETING STATISTICS FOR PREDICTION GROUPS*           ',//,2X, &
      'Statistics are presented for each PREDICTION GROUP             ',/,2X, &
      '  Statistics are of three types:                               ',/,2X, &
      '                                                               ',/,2X, &
      '   ABSOLUTE    - LARGEST, SMALLEST, AVERAGE                    ',/,2X, &
      '   DEVIATIONAL - deviation from user specified reference value ',/,2X, &
      '                 DIFF-MAX, DIFF-MIN, DIFF-AVG                  ',/,2X, &
      '   RELATIVE    - % deviation relative to reference value       ',/,2X, &
      '                 %DIFF-MAX, %DIFF-MIN, %-DIFF-AVG            ',/,2X, &
      '                                                               ',/,2X, &
      'This summary table can be used to indicate the deviation of    ',/,2X, &
      'simulated predictions from reference values. HOWEVER - if      ',/,2X, &
      'there is no supportable basis for specifying a reference       ',/,2X, &
      'value, DEVIATIONAL and RELATIVE statistics are meaningless.    ',/,2X, &
      'THUS use only ABSOLUTE statistics for PREDICTIONS.             ',/,2X, &
      '(Refer to MOD-PREDICT documentation for discussion, (Tonkin et ',/,2X, &
      'al., 2003, USGS OFR03-385)).                                   ',/)
    209 FORMAT(3X,A12,6X, &
       'Group statistics cannot be calculated, too many Obsvalues=0')
    210 FORMAT(3X,A12,5X,3(2X,1PG16.7,2X))
    211 FORMAT(3X,A12,5X,2(2X,I12,6X))
    213 FORMAT(4(1X,18('-'),1X))
    298 FORMAT('"NUMBER OF PREDICTION GROUPS = "',I10)
    299 FORMAT('"NUMBER OF PARAMETERS FOR PREDICTIVE EVALUATION = "',I10)
    300 FORMAT(1X,' "PREDICTED VALUE" "PLOT SYMBOL" "PREDICTION NAME" ')
    301 FORMAT(1X,' "PREDICTION VARIANCE" "PREDICTION NAME" "PREDICTION GROUP"')
    302 FORMAT(1X,G25.16,1X,I10,1X,A20)
    303 FORMAT(1X,G25.16,1X,A20,1X,A12)
    !
    !   Create FORMATs according to number of parameters
    WRITE(FMT_STRING1, '(I10)') NPE                     !swm
    WRITE(FMT_STRING2, '(I10)') NPE                     !swm
    FMT_STRING1 = '(1X,''"PREDICTION NAME" "PLOT SYMBOL"'',' &
                   //TRIM(ADJUSTL(FMT_STRING1))//'(1X,''"'',A,''"''))'    !swm
    FMT_STRING2 = '(1X, A,1X,I7,4X' &
                  //TRIM(ADJUSTL(FMT_STRING2))//'(G14.7,1X))'    !swm
    MXRECL = 52+NPPREC*26
    COL12NAM(1) = 'PREDICTION NAME'
    COL12NAM(2) = 'PLOT SYMBOL'
    !
    IFAIL = 0
    ! If additional prior is included for prediction write _prp, _suprip, _wtprip
    IF(MPR>MPRWOP) THEN
      CALL PRI_UEV_DX_WRITE_PRP(MPR,MPRWOP,OUTNAMPRED)
      IUSU = UTL_DX_OPEN(OUTNAMPRED,'_suprip','REPLACE',MXRECL)
      CALL SEN_UEV_DX_WRITE_MATRIX(IUSU,MPR-MPRWOP,NPE,NPT, &
           PRINAM(MPRWOP+1:MPR),IPTR,PARNAM,XPRI(:,MPRWOP+1:MPR),PLOTSYMBOLPRI)
      IUSU = UTL_DX_CLOSE('_suprip')
      CALL TYP_NULL(PRIWTMATFP)
      CALL TYP_NULL(PRIWTMATSQRFP)
      CALL UTL_GET_MATRIX_SECTION &
           (IFAIL,PRIWTMAT,MPRWOP+1,MPR,MPRWOP+1,MPR,PRIWTMATFP)
      CALL UTL_GET_MATRIX_SECTION &
           (IFAIL,PRIWTMATSQR,MPRWOP+1,MPR,MPRWOP+1,MPR,PRIWTMATSQRFP)
      CALL UTL_DX_WRITE_WT(OUTNAMPRED,'_wtprip',PRIWTMATFP,PRIWTMATSQRFP)
      CALL TYP_DEALLOC(PRIWTMATFP)
      CALL TYP_DEALLOC(PRIWTMATSQRFP)
      ALLOCATE(CMATFP(MPR-MPRWOP,MPR-MPRWOP),IPTRSHT(MPR-MPRWOP), &
               PARNAMSHT(MPR-MPRWOP))
      CMATFP = 0.D0
      CALL UTLUCODE_DX_READ_DM(IFAIL,OUTNAM,RDUM,RDUM,RDUM, &
                               RDUM,CDUM,IDUM,RDUM,RDUM, &
                               RDUM,CDUM,CDUM,CDUM,CDUM, &
                               IDUM,IDUM,IDUM,IDUM,IDUM, &
                               IDUM,RDUM,RDUM,RDUM,CEV)
      DO I=1,(MPR-MPRWOP)
        PARNAMSHT(I) = PARNAM(NPT-(MPR-MPRWOP)+I)
        IPTRSHT(I) = I
        CMATFP(I,I) = 1.D0/(UTL_GETVAL(PRIWTMAT,(MPRWOP+I),(MPRWOP+I)))
      ENDDO
      CALL UTL_DX_WRITE_MCMV('_mvp',(MPR-MPRWOP),(MPR-MPRWOP),IPTRSHT, &
                             OUTNAMPRED,PARNAMSHT,CMATFP)
      DEALLOCATE(PARNAMSHT)
    ENDIF
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ALLOCATE(ILOG(NPE),IPLOT(NOBS),GGT(NPREDGPS),GNUMA(NPREDGPS), &
             GNUMD(NPREDGPS),GNUMPD(NPREDGPS),GLT(NPREDGPS),GMAX(NPREDGPS), &
             GMIN(NPREDGPS),GAVG(NPREDGPS),GDIFMAX(NPREDGPS), &
             GDIFMIN(NPREDGPS),GDIFAVG(NPREDGPS),GPERDIFMAX(NPREDGPS), &
             GPERDIFMIN(NPREDGPS),GPERDIFAVG(NPREDGPS))
    ! Initialize
    WT = 0.D0
    ! Initialize
    DIFF = 0.D0
    DIVIDEBYZERO = 0.D0
    GGT = 0
    GNUMA = 0
    GNUMD = 0
    GNUMPD = 0
    GLT = 0
    GMAX = -BIGDOUBLE
    GMIN = BIGDOUBLE
    GAVG = 0.D0
    GDIFMAX = -BIGDOUBLE
    GDIFMIN = BIGDOUBLE
    GDIFAVG = 0.D0
    GPERDIFMAX = -BIGDOUBLE
    GPERDIFMIN = BIGDOUBLE
    GPERDIFAVG = 0.D0
    GPNAM = ' '
    MYGP = 0
    PERDIFF = 0.D0
    ! Open p, pv & spu if running predictive case
    IUYDM = UTL_DX_OPEN(OUTNAMPRED,'_dmp','REPLACE')
    WRITE(IUYDM,298)NPREDGPS
    WRITE(IUYDM,299)NPE
    CLOSE(IUYDM)
    IUY = UTL_DX_OPEN(OUTNAMPRED,'_p','REPLACE')
    WRITE(IUY,300)
    IUYV = UTL_DX_OPEN(OUTNAMPRED,'_pv','REPLACE')
    WRITE(IUYV,301)
    ! Loop through predictions
    DO I=1,NOBS
      CALL DEP_GET_GROUP(I,GPNAM,IPLOT(I),'USED')
      DO J=1,NPREDGPS
        IF(GPNAM == GROUPNAM(J)) THEN
          MYGP = J
          EXIT
        ENDIF
      ENDDO
      WT = UTL_GETVAL(WTFULL,I,I)
      WRITE(IUY,302)MODELVAL(I),IPLOT(I),OBSNAM(I)
      WRITE(IUYV,303)1.D0/WT,OBSNAM(I),GPNAM
      GNUMA(MYGP) = GNUMA(MYGP) + 1
      GNUMD(MYGP) = GNUMD(MYGP) + 1
      GNUMPD(MYGP) = GNUMPD(MYGP) + 1
      IF (MODELVAL(I) < OBSVAL(I)) GLT(MYGP) = GLT(MYGP) + 1
      IF (MODELVAL(I) > OBSVAL(I)) GGT(MYGP) = GGT(MYGP) + 1
      IF(MODELVAL(I) > GMAX(MYGP)) GMAX(MYGP)=MODELVAL(I)
      IF(MODELVAL(I) < GMIN(MYGP)) GMIN(MYGP)=MODELVAL(I)
      GAVG(MYGP) = GAVG(MYGP)+MODELVAL(I)
      DIFF = ABS(OBSVAL(I)-MODELVAL(I))
      IF(DIFF > GDIFMAX(MYGP)) GDIFMAX(MYGP)=DIFF
      IF(DIFF < GDIFMIN(MYGP)) GDIFMIN(MYGP)=DIFF
      GDIFAVG(MYGP) = GDIFAVG(MYGP)+DIFF
      IF(OBSVAL(I) /= 0.D0) THEN
        PERDIFF = (ABS((OBSVAL(I)-MODELVAL(I)))*100.D0)/OBSVAL(I)
        IF(PERDIFF > GPERDIFMAX(MYGP)) GPERDIFMAX(MYGP)=PERDIFF
        IF(PERDIFF < GPERDIFMIN(MYGP)) GPERDIFMIN(MYGP)=PERDIFF
        GPERDIFAVG(MYGP) = GPERDIFAVG(MYGP)+PERDIFF
      ELSE
        GNUMPD(MYGP) = GNUMPD(MYGP) - 1
      ENDIF
    ENDDO
    IUY = UTL_DX_CLOSE('_p')
    IUYV = UTL_DX_CLOSE('_pv')
    DO J=1,NPREDGPS
      IF(USEFLAG(J)) THEN
        IF(GNUMA(J) > 0) THEN
          GAVG(J) = GAVG(J)/ GNUMA(J)
        ENDIF
        IF(GNUMD(J) > 0) THEN
          GDIFAVG(J) = GDIFAVG(J)/ GNUMD(J)
        ENDIF
        IF(GNUMPD(J) > 0) THEN
          GPERDIFAVG(J) = GPERDIFAVG(J)/ GNUMPD(J)
        ENDIF
      ENDIF
    ENDDO
    IF(IOUT > 0) THEN
      WRITE(IOUT,201)
      WRITE(IOUT,202)
      WRITE(IOUT,203)
      WRITE(IOUT,204)
      WRITE(IOUT,213)
      DO J = 1,NPREDGPS
        IF(USEFLAG(J)) &
            WRITE(IOUT,210)GROUPNAM(J),GMAX(J),GMIN(J),GAVG(J)
      ENDDO
      WRITE(IOUT,203)
      WRITE(IOUT,205)
      WRITE(IOUT,213)
      DO J = 1,NPREDGPS
        IF(USEFLAG(J))THEN
          IF(GNUMD(J) > 0) THEN
            WRITE(IOUT,210)GROUPNAM(J),GDIFMAX(J),GDIFMIN(J),GDIFAVG(J)
          ELSE
            WRITE(IOUT,209)GROUPNAM(J)
          ENDIF
        ENDIF
      ENDDO
      WRITE(IOUT,203)
      WRITE(IOUT,206)
      WRITE(IOUT,213)
      DO J = 1,NPREDGPS
        IF(USEFLAG(J))THEN
          IF(GNUMD(J) > 0) THEN
            WRITE(IOUT,210)GROUPNAM(J),GPERDIFMAX(J),GPERDIFMIN(J),GPERDIFAVG(J)
          ELSE
            WRITE(IOUT,209)GROUPNAM(J)
          ENDIF
        ENDIF
      ENDDO
      WRITE(IOUT,203)
      WRITE(IOUT,207)
      WRITE(IOUT,213)
      DO J = 1,NPREDGPS
        IF(USEFLAG(J)) &
            WRITE(IOUT,211)GROUPNAM(J),GLT(J),GGT(J)
      ENDDO
      WRITE(IOUT,208)
    ENDIF
    ! IF requested create data exchange files
    NMPRFP = MPR-MPRWOP
    NPEWOP = NPE-NPEFP
    IF (DATAEXCHANGE) THEN
      ALLOCATE(PCOEF(NPE),PNAMTMP(NPE),PSD(NPE),PSDREG(NPE),PVALREG(NPE), &
               PVALNAT(NPE),PVAR(NPEWOP,NPEWOP),XSP(NPE,NOBS))
      PCOEF = 0.D0
      PNAMTMP = ' '
      PSD = 0.D0
      PSDREG = 0.D0
      PVALREG = 0.D0
      PVALNAT = 0.D0
      PVAR = 0.D0
      XSP = 0.D0
      ! Get parameter values for scaling
      IUY = UTL_DX_OPEN(OUTNAM,'_pc','OLD')
      READ(IUY,*,END=900)
      IFAIL = 0
      DO I=1,NPEWOP
        READ(IUY,*,END=900)PNAMTMP(I),PVALNAT(I),DUM,DUM,ILOG(I)
        PVALREG(I) = 1.D0
        IF(ILOG(I) > 0) PVALREG(I) = LOG10(PVALNAT(I))
      ENDDO
      IUY = UTL_DX_CLOSE('_pc')
      IF(MPR>MPRWOP) THEN
        DO I=1,NMPRFP
          PVALNAT(NPEWOP+I) = PVAL(IPTR(NPEWOP+I))
          ILOG(NPEWOP+I) = LN(IPTR(NPEWOP+I))
          PVALREG(NPEWOP+I) = 1.D0
          IF(ILOG(NPEWOP+I) > 0) PVALREG(NPEWOP+I) = LOG10(PVALNAT(NPEWOP+I))
        ENDDO
      ENDIF
      ! get std dev for scaling
      CALL UTL_DX_READ_MCMV('_mv',(NPEWOP),OUTNAM,PNAMTMP,PVAR)
      DEALLOCATE(PNAMTMP)
      DO I=1,NPEWOP
        PSDREG(I) = SQRT(PVAR(I,I))
        IF(ILOG(I) > 0) THEN
          PCOEF = PVALNAT(I)
        ELSE
          PCOEF = 1.D0
        ENDIF
        PSD(I)= PSDREG(I)
      ENDDO
      DEALLOCATE(PVAR)
      IF(SENSITIVITIES) THEN
        IF(MPR>MPRWOP) THEN
          DO I=1,NMPRFP
            PSDREG(NPEWOP+I) = SQRT(CMATFP(I,I))
            IF(ILOG(NPEWOP+I) > 0) THEN
              PCOEF = PVALNAT(NPEWOP+I)
            ELSE
              PCOEF = 1.D0
            ENDIF
            PSD(NPEWOP+I)= PSDREG(NPEWOP+I)
          ENDDO
        ENDIF
        MXRECL = 406+NPPREC*32
        IUYS = UTL_DX_OPEN(OUTNAMPRED,'_spu','REPLACE',MXRECL)
        CALL UTLUCODE_DX_WRITE_MATRIX &
             (IUYS,NOBS,NPE,NPT,IPTR,PARNAM,XSENS,OBSNAM,IPLOT,COL12NAM)
        IUYS = UTL_DX_CLOSE('_spu')
        DO K=1,4
          IF(K == 1) THEN
            IUYS = UTL_DX_OPEN(OUTNAMPRED,'_spsr','REPLACE',MXRECL)
          ELSEIF(K == 2) THEN
            IUYS = UTL_DX_OPEN(OUTNAMPRED,'_sppr','REPLACE',MXRECL)
          ELSEIF(K == 3) THEN
            IUYS = UTL_DX_OPEN(OUTNAMPRED,'_spsp','REPLACE',MXRECL)
          ELSE
            IUYS = UTL_DX_OPEN(OUTNAMPRED,'_sppp','REPLACE',MXRECL)
          ENDIF
          DO I=1,NOBS
            ! PCOEF = PVALNAT for logged transformed to get sens in reg space
            ! PCOEF is set to one for non log transformed parameters
            ! so is irrelevant in the equations below
            ! PSD is in regression space so it is transformd for log transformed
            ! parameters and not for others
            ! PVALREG is set to one for non log transformed parameters
            ! so is irrelevant in the equations below
            IF(K == 1) THEN
              IF(OBSVAL(I) /= 0.D0) THEN
                DO J=1,NPE
                  XSP(J,I) = XSENS(J,I)*ABS(PCOEF(J)*PSD(J)/OBSVAL(I))
                ENDDO
              ELSE
                DO J=1,NPE
                  XSP(J,I) = DIVIDEBYZERO
                ENDDO
              ENDIF
            ELSEIF(K ==2) THEN
              IF(OBSVAL(I) /= 0.D0) THEN
                DO J=1,NPE
                  IF(LN(IPTR(J)) == 1) THEN
                    XSP(J,I) = XSENS(J,I)/ABS(OBSVAL(I))
                  ELSE
                    XSP(J,I) = XSENS(J,I)*ABS(PVALNAT(J)*PVALREG(J)/OBSVAL(I))
                  ENDIF
                ENDDO
              ENDIF
            ELSEIF(K == 3) THEN
              IF(MODELVAL(I) /= 0.D0) THEN
                DO J=1,NPE
                  XSP(J,I) = XSENS(J,I)*ABS(PCOEF(J)*PSD(J)/MODELVAL(I))
                ENDDO
              ELSE
                DO J=1,NPE
                  XSP(J,I) = DIVIDEBYZERO
                ENDDO
              ENDIF
            ELSE
              IF(MODELVAL(I) /= 0.D0) THEN
                DO J=1,NPE
                  XSP(J,I) = XSENS(J,I)*ABS(PVALNAT(J)*PVALREG(J)/MODELVAL(I))
                ENDDO
              ENDIF
            ENDIF
          ENDDO
          CALL UTLUCODE_DX_WRITE_MATRIX &
               (IUYS,NOBS,NPE,NPT,IPTR,PARNAM,XSP,OBSNAM,IPLOT,COL12NAM)
          IF(K == 1) THEN
            IUYS = UTL_DX_CLOSE('_spsr')
          ELSEIF(K == 2) THEN
            IUYS = UTL_DX_CLOSE('_sppr')
          ELSEIF(K == 3) THEN
            IUYS = UTL_DX_CLOSE('_spsp')
          ELSE
            IUYS = UTL_DX_CLOSE('_sppp')
          ENDIF
        ENDDO
      ENDIF
      DEALLOCATE(PCOEF,PSD,PSDREG,PVALREG,PVALNAT,XSP)
    ENDIF
    DEALLOCATE(ILOG,IPLOT,GGT,GNUMA,GNUMD,GNUMPD,GLT,GMAX,GMIN,GAVG,GDIFMAX, &
             GDIFMIN,GDIFAVG,GPERDIFMAX,GPERDIFMIN,GPERDIFAVG)
    IF(ALLOCATED(CMATFP)) DEALLOCATE(CMATFP)
    RETURN
    900 IFAIL = 1
    AMESSAGE = ' Error in UTLUCODE_DX_WRITE_PREDS reading'//   &
               '  _pc; could not write _sp files'
    DEALLOCATE(ILOG,IPLOT,GGT,GNUMA,GNUMD,GNUMPD,GLT,GMAX,GMIN,GAVG,GDIFMAX, &
             GDIFMIN,GDIFAVG,GPERDIFMAX,GPERDIFMIN,GPERDIFAVG)
    IF(ALLOCATED(CMATFP)) DEALLOCATE(CMATFP)
    RETURN
  END SUBROUTINE UTLUCODE_DX_WRITE_PREDS
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_PAR_OMIT(IOUT,NPO,NPS,PARNAM,POMIT)
    !   This writes a message for later phases of analysis when
    !   parameter have been omitted during the regression
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                           INTENT(IN)    :: IOUT
    INTEGER,                           INTENT(IN)    :: NPO
    INTEGER,                           INTENT(IN)    :: NPS
    CHARACTER(LEN=12),                 INTENT(IN)    :: PARNAM(NPS)
    INTEGER,                           INTENT(IN)    :: POMIT(NPO)
    !
    INTEGER                             :: I
    ! formats
    250 FORMAT(/,1X,'USER INTERVENTION IS NEEDED: ',//, &
    3X,'Parameters were omitted during the regression either because',/, &
    3X,'  they were below the user defined relative sensitivity level',/, &
    3X,'  (defined by OmitInsensitive in the Reg_GN_Controls input block)',/, &
    3X,'  or outside of user specified constraints (defined by',/, &
    3X,'  constrain=yes in the Parameter_Data input block). Those',/, &
    3X,'  parameter names are listed in a table following this message.',/, &
    3X,'  When this occurs Residual_Analysis and Residual_Analysis_Adv',/, &
    3X,'  can be executed, but other auxiliary programs and advanced',/, &
    3X,'  analyses of UCODE require user intervention and an ',/, &
    3X,'  additional run of UCODE to generate data-exchange files',/, &
    3X,'  needed for advanced analyses. Until that intervention is made,',/, &
    3X,'  the only advanced mode of UCODE that is permitted is',/, &
    3X,'  prediction without sensitivity (using Prediction=yes and',/, &
    3X,'  Sensitivities=no in the UCODE_Control_Data block).',//, &
    3X,'Please review this information and decide how to proceed.',//, &
    3X,'ONE OF THE FOLLOWING TWO OPTIONS NEED TO BE ACCOMPLISHED BEFORE ', &
       'PROCEEDING:',//, &
    4X,'1) Repeat the regression mode (optimize=yes and sensitivity=yes in',/, &
    4X,'   the UCODE_Control_Data input block) with one or more of the',/, &
    4X,'   following changes for each of the omitted parameters so that',/, &
    4X,'   they will not be omitted during the regression:',/, &
    7X,'a) different specifications for acceptable relative sensitivity ',/, &
    7X,'   and/or parameter constraints',/, &
    7X,'b) new prior information',/, &
    7X,'c) prior information with smaller statistics',/, &
    4X,'OR',/, &
    4X,'2) Run sensitivity analysis (optimize=no and sensitivity=yes in',/, &
    7X,'the UCODE_Control_Data input block) using a Parameter_Values input',/, &
    7X,'block to specify the final regression parameter values or altered',/, &
    7X,'values. For example, constrained values may be replaced by values',/, &
    7X,'supported by prior information. Depending on the situation in the',/, &
    7X,'sensitivity analysis mode run, consider the following options:',/, &
    9X,'a) include the omitted parameters as adjustable so their',/, &
    9X,'   uncertainty is included in subsequent analyses',/,&
    9X,'b) define the omitted parameters as not adjustable so they are',/,&
    9X,'   excluded from consideration in subsequent analyses',/,&
    9X,'c) move the omitted parameters out of the Parameter_Data input',/,&
    9X,'   block, but include them in a Parameters_For_Prediction input',/, &
    9X,'   block and define prior information for these parameters in the',/, &
    9X,'   Linear_Prior_Information_For_Prediction input block')
    251 FORMAT(/,3X,'THE FOLLOWING PARAMETERS WERE OMITTED DURING THE ',&
               'REGRESSION',/,3X,'DUE TO LOW RELATIVE SENSITIVITY OR',&
                    ' USER SPECIFIED CONTRAINTS',/)
    887 FORMAT(5X,A)
    !
    WRITE(IOUT,250)
    WRITE(IOUT,251)
    WRITE(*,250)
    WRITE(*,251)
    !
    DO I=1,NPO
      WRITE(IOUT,887) PARNAM(POMIT(I))
      WRITE(*,887) PARNAM(POMIT(I))
    ENDDO
    !
    RETURN
  END SUBROUTINE UTLUCODE_PAR_OMIT
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_PAR_OMIT_REACT1(ILOW,IUNIT,RSQALLHI,RSQALLLOW)
    USE UTILITIES
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                           INTENT(IN)    :: ILOW
    INTEGER,                           INTENT(IN)    :: IUNIT
    DOUBLE PRECISION,                  INTENT(IN)    :: RSQALLHI
    DOUBLE PRECISION,                  INTENT(IN)    :: RSQALLLOW
    ! formats
    695 FORMAT (//,1X,78('!'),//, &
    4X,' WARNING: GIVEN THAT',//, &
    3X,'    REACTIVATE = starting IN THE UCODE_CONTROL_DATA BLOCK,',//, &
    3X,'    THE PARAMETER VALUES USED TO PRODUCE THE FINAL STATISTICS AND',/, &
    3X,'    DATA-EXCHANGE FILES YIELDED A LARGER SUM OF SQUARED RESIDUALS',/, &
    3X,'    THAN THE LOWEST SUM OF SQUARED RESIDUALS OBTAINED FOR AT LEAST',/ &
    3X,'    ONE ITERATION OF THE REGRESSION:',// &
    3X,'    FINAL  = ',F35.8,/, &
    3X,'    LOWEST = ',F35.8,'   IN ITERATION = ',I4,//, &
    5X,'IF THE FINAL IS MUCH HIGHER THAN THE LOWEST,',/, &
    5X,'THEN USER INTERVENTION IS RECOMMENDED, POSSIBLE ACTIONS INCLUDE: ',//, &
    5X,'1) Repeat the regression mode (optimize=yes and sensitivity=yes in',/, &
    5X,'   the UCODE_Control_Data input block) with one or more of the',/, &
    5X,'   following changes for each of the omitted parameters so that',/, &
    5X,'   they will not be omitted during the regression:',/, &
    8X,'a) different specifications for acceptable relative sensitivity ',/, &
    8X,'   and/or parameter constraints',/, &
    8X,'b) new prior information',/, &
    8X,'c) prior information with smaller statistics',/, &
    5X,'2) Repeat the regression mode (optimize=yes and sensitivity=yes in',/, &
    5X,'   the UCODE_Control_Data input block) with the omitted parameters',/, &
    5X,'   defined as not adjustable.',//,1X,78('!'))
    WRITE(IUNIT,695)RSQALLHI,RSQALLLOW,ILOW
    RETURN
  END SUBROUTINE UTLUCODE_PAR_OMIT_REACT1
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_PRI_OMIT(IOUT,MPR,OMITPRI,PREDICT)
    !   This determines when a parameter with prior may have been omitted
    !   and writes a message
    USE UTILITIES
    USE PRIOR_INFORMATION, &
        ONLY: LLPTRPRIOR, PRINAM, PRIVAL, PRIEQN, PLOTSYMBOLPRI
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                           INTENT(IN)    :: IOUT
    INTEGER,                           INTENT(IN)    :: MPR
    INTEGER,                           INTENT(IN)    :: OMITPRI(MPR)
    LOGICAL,                           INTENT(IN)    :: PREDICT
    !
    INTEGER                             :: IEQ, IERR
    INTEGER                             :: MORE
    CHARACTER(LEN=MAX_STRING_LEN)       :: PRIEQTEXT(MPR)
    CHARACTER(LEN=12)                   :: PRIGROUP(MPR)
    DOUBLE PRECISION                    :: PRIVARIANCE(MPR)
    CHARACTER(LEN=6)                    :: STATFLAG(MPR)
    ! formats
    248 FORMAT(/,1X,78('!'),//, &
    1X,'SOME PARAMETERS THAT WERE OMITTED DURING THE REGRESSION WERE',/, &
    3X,'INCLUDED IN PRIOR INFORMATION. NORMALLY A UCODE EXECUTION',/, &
    3X,'WITH PREDICTION=yes AND SENSITIVITY=no COULD PROCEED EVEN IF SOME',/, &
    3X,'PARAMETERS WERE OMITTED, BUT THIS IS NOT POSSIBLE IF THIS CAUSES',/, &
    3X,'ANY OF THE PRIOR EQUATIONS TO BE DEVOID OF ADJUSTABLE PARAMETERS.',/)
    249 FORMAT(/,1X,78('!'),//, &
    1X,'GUIDANCE ON HOW TO PROCEED IS GIVEN BELOW.',//)
    250 FORMAT(/,3X,'IN ADDITION SOME OF THOSE PARAMETERS WERE INCLUDED ', &
    'IN PRIOR INFORMATION')
    251 FORMAT(/,3X,'PRIOR INFORMATION ITEMS WITH NO ADJUSTABLE PARAMETERS:',/)
    886 FORMAT(5X,A,' = ',A)
!    887 FORMAT(5X,A,2(1X,G18.10),3(1X,A))
    !
    IF(PREDICT) THEN
      WRITE(IOUT,248)
      WRITE(*,248)
    ELSE
      WRITE(IOUT,250)
      WRITE(*,250)
    ENDIF
    WRITE(IOUT,251)
    WRITE(*,251)
    !
    IERR = 0
    !   Populate arrays from linked list
    CALL UTL_FILTERLIST(LLPTRPRIOR,IOUT,'PRIORNAME',MPR,IERR,PRINAM,MORE)
    CALL UTL_FILTERLIST(LLPTRPRIOR,IOUT,'PRIORINFOVALUE',MPR,IERR,PRIVAL,MORE)
    CALL UTL_FILTERLIST(LLPTRPRIOR,IOUT,'STATISTIC',MPR,IERR,PRIVARIANCE,MORE)
    CALL UTL_FILTERLIST(LLPTRPRIOR,IOUT,'GROUPNAME',MPR,IERR,PRIGROUP,MORE)
    CALL UTL_FILTERLIST(LLPTRPRIOR,IOUT,'PLOTSYMBOL',MPR,IERR, &
                        PLOTSYMBOLPRI,MORE)
    CALL UTL_FILTERLIST(LLPTRPRIOR,IOUT,'EQUATION',MPR,IERR,PRIEQTEXT,MORE)
    CALL UTL_FILTERLIST(LLPTRPRIOR,IOUT,'STATFLAG',MPR,IERR,STATFLAG,MORE)
    !   Process linked list and rewrite block
    DO IEQ=1,MPR
      !   write equations that still have active parameters
      IF(OMITPRI(IEQ) == 1)THEN
        WRITE(IOUT,886) &
        TRIM(PRINAM(IEQ)),TRIM(PRIEQTEXT(IEQ))
        WRITE(*,886) &
        TRIM(PRINAM(IEQ)),TRIM(PRIEQTEXT(IEQ))
!        WRITE(IOUT,887) &
!        TRIM(PRINAM(IEQ)),PRIVAL(IEQ),PRIVARIANCE(IEQ), &
!        TRIM(STATFLAG(IEQ)),PRIGROUP(IEQ),TRIM(PRIEQTEXT(IEQ))
!        WRITE(*,887) &
!        TRIM(PRINAM(IEQ)),PRIVAL(IEQ),PRIVARIANCE(IEQ), &
!        TRIM(STATFLAG(IEQ)),PRIGROUP(IEQ),TRIM(PRIEQTEXT(IEQ))
      ENDIF
    ENDDO
    IF(PREDICT) THEN
      WRITE(IOUT,249)
      WRITE(*,249)
    ENDIF
    !
    RETURN
  END SUBROUTINE UTLUCODE_PRI_OMIT
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_READ_SPU &
                             (OUTNAM,NPE,NDEP,OBSNAM,PARNAM,PLOTSYMBOL,XSENS)
    !   READ _spu Data-Exchange Files: Unscaled Prediction Sensitivities
    USE GLOBAL_DATA, ONLY: AMESSAGE, MAX_STRING_LEN, BIGINTEGER
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=MAX_STRING_LEN),           INTENT(IN)  :: OUTNAM
    INTEGER,                                 INTENT(IN)  :: NPE
    INTEGER,                                 INTENT(IN)  :: NDEP
    CHARACTER(LEN=LENDNAM), DIMENSION(NDEP), INTENT(OUT) :: OBSNAM
    CHARACTER(LEN=12),      DIMENSION(NPE),  INTENT(OUT) :: PARNAM
    INTEGER,                DIMENSION(NDEP), INTENT(OUT) :: PLOTSYMBOL
    DOUBLE PRECISION,                        INTENT(INOUT) :: XSENS(NPE,NDEP)
    !
    !   Local variables
    CHARACTER(LEN=LENDNAM)  :: CDUM
    CHARACTER(LEN=1)        :: CH
    INTEGER                 :: I
    INTEGER                 :: IP1
    INTEGER                 :: IP2
    INTEGER                 :: IUSPU
    INTEGER                 :: J
    INTEGER                 :: KB
    INTEGER                 :: KC
    INTEGER                 :: KP
    INTEGER                 :: KQ
    INTEGER                 :: MRECL
    INTEGER                 :: MREM
    INTEGER                 :: NPREC1
    !
    IF (NDEP <= 0) THEN
      AMESSAGE = 'Error encountered in UTLUCODE_DX_READ_SPU: NUMBER OF'//   &
                 ' OBSERVATIONS AND PRIOR = 0, NOTHING TO DO'
      CALL UTL_STOP()
    ENDIF
    ! read  predictions
    IUSPU = UTL_DX_OPEN(OUTNAM,'_spu','OLD')
    !
    !   Read header one character at a time to determine number of parameters
    !   per record
    MRECL = BIGINTEGER
    KQ = 0
    DO KC=1,MRECL
      READ(IUSPU,'(A1)',ADVANCE='NO',EOR=50) CH
      IF (CH=='"') KQ = KQ+1
    ENDDO
    50 CONTINUE
    NPREC1 = KQ/2 - 2
    MREM = MOD(KQ,2)
    IF (MREM .NE. 0) THEN
      AMESSAGE = 'Programming error: UTLUCODE_DX_READ_SPU'//   &
                 ' finds odd number of quotes in _sPu file'
      CALL UTL_STOP()
    ENDIF
    REWIND(IUSPU)
    !
    !  Read  parameter names
    KB = 0
    KP = 0
    BLOK: DO WHILE (KP<NPE)
      IP1 = KB*NPREC1+1
      KB = KB+1
      IP2 = KB*NPREC1
      IF (IP2>NPE) IP2 = NPE
      READ(IUSPU,*,END=100)CDUM,CDUM,(PARNAM(I),I=IP1,IP2)
      DO I = 1, NDEP
        READ(IUSPU,*,END=100)OBSNAM(I),PLOTSYMBOL(I),(XSENS(J,I),J=IP1,IP2)
      ENDDO
      KP = IP2
    ENDDO BLOK
    !
    ! Close data exchange file
    IUSPU = UTL_DX_CLOSE('_spu')
    RETURN
    100 CONTINUE
    AMESSAGE = 'Error encountered in UTLUCODE_DX_READ_SPU: Premature end'//   &
               ' of _spu file'
    CALL UTL_STOP()
  END SUBROUTINE UTLUCODE_DX_READ_SPU
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_WRITE_SEN(IOUT,MPR,NOBS,NPE,NPS,OBSNAM, &
      OMIT,OUTNAM,IPTR,PARNAM,PRINAM,PVAL, &
      WTFULL,WTFULLSQR,WTFULLPRI,WTFULLSQRPRI,XPRI,XSENS,DSS,CSS,DSSWP,CSSWP)
    !   Write _S1_SC_SD_SO Data-Exchange Files: Sensitivities
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN, NPPREC
    USE UTILITIES
    USE DEPENDENTS, ONLY: DEP_GET_GROUP, GROUP, GROUPNOBS
    USE PRIOR_INFORMATION, ONLY: PLOTSYMBOLPRI
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: IOUT
    INTEGER,                                 INTENT(IN) :: MPR
    INTEGER,                                 INTENT(IN) :: NOBS
    INTEGER,                                 INTENT(IN) :: NPE
    INTEGER,                                 INTENT(IN) :: NPS
    CHARACTER(LEN=LENDNAM), DIMENSION(NOBS), INTENT(IN) :: OBSNAM
    INTEGER,                DIMENSION(NOBS), INTENT(IN) :: OMIT
    CHARACTER(LEN=MAX_STRING_LEN),           INTENT(IN) :: OUTNAM
    INTEGER,                DIMENSION(NPE),  INTENT(IN) :: IPTR
    CHARACTER(LEN=12),      DIMENSION(NPS),  INTENT(IN) :: PARNAM
    CHARACTER(LEN=LENDNAM), DIMENSION(MPR),  INTENT(IN) :: PRINAM
    DOUBLE PRECISION,       DIMENSION(NPS),  INTENT(IN) :: PVAL
    TYPE (CDMATRIX),                         INTENT(IN) :: WTFULL
    TYPE (CDMATRIX),                         INTENT(IN) :: WTFULLSQR
    TYPE (CDMATRIX),                         INTENT(IN) :: WTFULLPRI
    TYPE (CDMATRIX),                         INTENT(IN) :: WTFULLSQRPRI
    DOUBLE PRECISION,    DIMENSION(NPE,MPR), INTENT(IN) :: XPRI
    DOUBLE PRECISION,   DIMENSION(NPE,NOBS), INTENT(IN) :: XSENS
    DOUBLE PRECISION,    DIMENSION(NPE,NOBS), INTENT(IN) :: DSS
    DOUBLE PRECISION,   DIMENSION(NPE), INTENT(INOUT) :: CSS
    DOUBLE PRECISION,    DIMENSION(NPE,NOBS), INTENT(INOUT) :: DSSWP
    DOUBLE PRECISION,   DIMENSION(NPE), INTENT(INOUT) :: CSSWP
    !
    !   Local variables
    INTEGER  I, ID, ID2, IP, IPP, IUSU, IUSC, IUSD, IUS1, IUSO, J, JN
    INTEGER  :: IFAIL, MXRECL, NGRPSCSS
    CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: MAX1OBS, MAX2OBS
    CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: MAX1OBSWP, MAX2OBSWP
    CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:,:) :: MAX1OBSGP, MAX2OBSGP
    CHARACTER(LEN=10)  :: STR_NPPREC
    DOUBLE PRECISION  ABSDSS, ABSMSS, MSS
    DOUBLE PRECISION  :: DTLA
    DOUBLE PRECISION  :: LEV
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: MAX1DSS, MAX2DSS
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: MAX1DSSWP, MAX2DSSWP
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: CSSGP, MAX1DSSGP, MAX2DSSGP
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: OSS
    INTEGER                                       :: GN
    CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)  :: GRPS
    CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:)  :: GRPSCSS
    INTEGER, ALLOCATABLE, DIMENSION(:)            :: GRPSNUM
    INTEGER, ALLOCATABLE, DIMENSION(:)            :: NOBSGP
    INTEGER                                       :: NOBSINCLD
    CHARACTER(LEN=12)                             :: GRPSCHECK
    CHARACTER(LEN=12)                             :: GPNAM
    INTEGER                                       :: IPLOT
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: WSQRX
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: WXROW_XTWXCD
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: WXROW
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: XP
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: XTRANS
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: XTW
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: XTWX
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: XTWXI
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: XWROW
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: XWSQR
    TYPE (CDMATRIX) :: XTWXICD
    TYPE (CDMATRIX) :: WT
    TYPE (CDMATRIX) :: WTSQR
    !
    !   Format statements
    600 FORMAT (1X,'"PARAMETER NAME" "COMPOSITE SCALED SENSITIVITY"', &
        ' "LARGEST DSS FOR THE PARAMETER" "OBSERVATION WITH LARGEST DSS"', &
        ' "SECOND LARGEST DSS FOR THE PARAMETER"', &
        ' "OBSERVATION WITH SECOND LARGEST DSS"')
    601 FORMAT(1X,A,5X,G22.15,16X,G22.15,1X,A,16X,G22.15,1X,A)
    609 FORMAT (1X,'"WITH PRIOR:"')
    610 FORMAT (1X,'"PARAMETER NAME"', &
        ' "COMPOSITE SCALED SENSITIVITY WITH PRIOR"', &
        ' "LARGEST DSS FOR THE PARAMETER" "OBSERVATION WITH LARGEST DSS"', &
        ' "SECOND LARGEST DSS FOR THE PARAMETER"', &
        ' "OBSERVATION WITH SECOND LARGEST DSS"')
    700 FORMAT (1X,'"OBSERVATION or PRIOR NAME"',1X,'"PLOT SYMBOL"',1X, &
                '"LEVERAGE"',1X, &
                '"LARGEST DSS"',1X, &
                '"PARAMETER WITH LARGEST DSS"')
    701 FORMAT(1X,A,1X,I6,6X,G22.15,5X,'0.0',16X,'N/A')
    702 FORMAT(1X,A,1X,I6,5X,2(1X,G22.15),1X,A)
    799 FORMAT(1X,'"GROUP NAME" ',A12,'  "NUMBER IN GROUP" ',I10)
    800 FORMAT (1X,'"PARAMETER NAME" "COMPOSITE SCALED SENSITIVITY"', &
        ' "LARGEST DSS FOR THE PARAMETER" "OBSERVATION WITH LARGEST DSS"', &
        ' "SECOND LARGEST DSS FOR THE PARAMETER"', &
        ' "OBSERVATION WITH SECOND LARGEST DSS"')
    !   Construct format strings
    WRITE(STR_NPPREC,'(I10)') NPPREC
    STR_NPPREC = ADJUSTL(STR_NPPREC)
    !
    ! Allocate memory
    ALLOCATE (OSS(NPE,NOBS),MAX1OBS(NPE),MAX2OBS(NPE),MAX1DSS(NPE),MAX2DSS(NPE))
    ALLOCATE (MAX1OBSWP(NPE),MAX2OBSWP(NPE),MAX1DSSWP(NPE),MAX2DSSWP(NPE))
    !
    ! Initialize
    CSS = 0.D0
    MAX1DSS = 0.D0
    MAX2DSS = 0.D0
    MAX1OBS = 'not-applicable'
    MAX2OBS = 'not-applicable'
    MAX1DSSWP = 0.D0
    MAX2DSSWP = 0.D0
    MAX1OBSWP = 'not-applicable'
    MAX2OBSWP = 'not-applicable'
    MXRECL = 52+NPPREC*26
    OSS = 0.D0
    !_su
    IUSU = UTL_DX_OPEN(OUTNAM,'_su','REPLACE',MXRECL)
    CALL SEN_UEV_DX_WRITE_MATRIX(IUSU,NOBS,NPE,NPS,OBSNAM,IPTR,PARNAM,XSENS)
    IUSU = UTL_DX_CLOSE('_su')
    !_sd
    IUSD = UTL_DX_OPEN(OUTNAM,'_sd','REPLACE',MXRECL)
    CALL SEN_UEV_DX_WRITE_MATRIX(IUSD,NOBS,NPE,NPS,OBSNAM,IPTR,PARNAM,DSS)
    IUSD = UTL_DX_CLOSE('_sd')
    !_so
    IUSO = UTL_DX_OPEN(OUTNAM,'_so','REPLACE',MXRECL)
    ! Sensitivity Info by observation, depends on dss's being in DSS
    !
    CALL TYP_NULL(XTWXICD)
    ALLOCATE(WXROW_XTWXCD(NPE),WXROW(NPE),XWROW(NPE),WSQRX(NOBS+MPR,NPE), &
             XWSQR(NPE,NOBS+MPR),XTW(NPE,NOBS+MPR),XTWX(NPE,NPE),XTWXI(NPE,NPE))
    WXROW_XTWXCD = 0.D0
    WXROW        = 0.D0
    XWROW        = 0.D0
    WSQRX        = 0.D0
    XWSQR        = 0.D0
    XTW          = 0.D0
    XTWX         = 0.D0
    XTWXI        = 0.D0
    IF(MPR > 0) THEN
      ALLOCATE(XP(NPE,NOBS+MPR),XTRANS(NOBS+MPR,NPE))
      CALL TYP_NULL(WT)
      CALL TYP_NULL(WTSQR)
      CALL UTL_COMBINESQMATRIX(WTFULL,WTFULLPRI,WT)
      CALL UTL_COMBINESQMATRIX(WTFULLSQR,WTFULLSQRPRI,WTSQR)
      DO I=1,NOBS
        DO J=1,NPE
          XP(J,I) = XSENS(J,I)
        ENDDO
      ENDDO
      DO I=1,MPR
        DO J=1,NPE
          XP(J,I+NOBS) = XPRI(J,I)
        ENDDO
      ENDDO
      ! Calculate w1/2X
      XTRANS = TRANSPOSE(XP)
      CALL UTL_MATMUL_SUB(NOBS+MPR,NPE,WTSQR,XTRANS,WSQRX)
      ! Calculate Xw1/2
      CALL UTL_MATMUL_SUB(NPE,NOBS+MPR,XP,WTSQR,XWSQR)
      ! Calculate XTw
      CALL UTL_MATMUL_SUB(NPE,NOBS+MPR,XP,WT,XTW)
      ! XTwX
      XTWX = MATMUL(XTW,XTRANS)
      DEALLOCATE(XP)
      CALL TYP_DEALLOC(WT)
      CALL TYP_DEALLOC(WTSQR)
    ELSE
      ! Calculate w1/2X
      ALLOCATE (XTRANS(NOBS,NPE))
      XTRANS = TRANSPOSE(XSENS)
      CALL UTL_MATMUL_SUB(NOBS,NPE,WTFULLSQR,XTRANS,WSQRX)
      ! Calculate Xw1/2
      CALL UTL_MATMUL_SUB(NPE,NOBS,XSENS,WTFULLSQR,XWSQR)
      ! Calculate XTw
      CALL UTL_MATMUL_SUB(NPE,NOBS,XSENS,WTFULL,XTW)
      ! XTwX
!      XTWX = MATMUL(XTW,TRANSPOSE(XSENS))
      XTWX = MATMUL(XTW,XTRANS)
      DEALLOCATE(XTRANS)
    ENDIF
    CALL UTLUCODE_INVERT(IFAIL,XTWX,NPE,XTWXI,IOUT,DTLA)
    !   Store matrix XTWXI in CDMATRIX structure XTWXICD
    CALL UTL_ARR2CDMATRIX(NPE,NPE,XTWXI,XTWXICD)
    !
    WRITE(IUSO,700)
    ALLOCATE(GRPS(NOBS),GRPSNUM(NOBS))
    GRPS=' '
    NGRPSCSS = 1
    DO ID = 1, NOBS
      GRPSCHECK=GROUPNOBS(ID)
      IF(ID == 1)THEN
        GRPS(1) = TRIM(GRPSCHECK)
        GRPSNUM(1) = 1
      ELSE
        DO ID2 = 1, ID
          IF(TRIM(GRPSCHECK) == TRIM(GRPS(ID2))) THEN
            GRPSNUM(ID) = ID2
            EXIT
          ENDIF
          IF(GRPS(ID2) == ' ')THEN
            GRPS(ID2) = TRIM(GRPSCHECK)
            GRPSNUM(ID) = ID2
            NGRPSCSS = NGRPSCSS + 1
            EXIT
          ENDIF
        ENDDO
      ENDIF
    ENDDO
    ALLOCATE(GRPSCSS(NGRPSCSS))
    DO ID = 1 , NGRPSCSS
      GRPSCSS(ID) = GRPS(ID)
    ENDDO
    DEALLOCATE(GRPS)
    DO ID = 1, NOBS+MPR
      ABSDSS = 0.D0
      ABSMSS = 0.D0
      MSS = 0.D0
      IPP = 1
      IF(ID .LE. NOBS) THEN
        DO IP = 1,NPE
          ABSDSS = ABS(DSS(IP,ID))
          IF(ABSDSS .GT. ABSMSS) THEN
            ABSMSS = ABSDSS
            MSS = DSS(IP,ID)
            IPP = IP
          ENDIF
        ENDDO
      ENDIF
      DO IP = 1, NPE
        WXROW(IP) = WSQRX(ID,IP)
        XWROW(IP) = XWSQR(IP,ID)
      ENDDO
      CALL UTL_MATMULVEC_SUB(NPE,XTWXICD,WXROW,WXROW_XTWXCD)
      LEV = 0.D0
      DO IP = 1, NPE
        LEV = LEV + WXROW_XTWXCD(IP)*XWROW(IP)
      ENDDO
      IF(ID > NOBS) THEN
        WRITE (IUSO,701)PRINAM(ID-NOBS),PLOTSYMBOLPRI(ID-NOBS),LEV
      ELSE
        CALL DEP_GET_GROUP(ID,GPNAM,IPLOT,'USED')
        WRITE (IUSO,702) OBSNAM(ID),IPLOT,LEV,MSS, &
                     PARNAM(IPTR(IPP))
      ENDIF
    ENDDO
    IUSO = UTL_DX_CLOSE('_so')
    CALL TYP_DEALLOC(XTWXICD)
    DEALLOCATE(WXROW_XTWXCD,WXROW,XWROW,WSQRX,XWSQR,XTW,XTWX,XTWXI)
    ! evaluate by group
    ALLOCATE(CSSGP(NPE,NGRPSCSS),NOBSGP(NGRPSCSS), &
             MAX1DSSGP(NPE,NGRPSCSS),MAX2DSSGP(NPE,NGRPSCSS), &
             MAX1OBSGP(NPE,NGRPSCSS),MAX2OBSGP(NPE,NGRPSCSS))
    CSSGP = 0.D0
    NOBSGP = 0
    MAX1DSSGP = 0.D0
    MAX2DSSGP = 0.D0
    MAX1OBSGP = 'not-applicable'
    MAX2OBSGP = 'not-applicable'
    DO IP=1,NPE
      NOBSINCLD = 0
      DO JN=1,NOBS
        IF(OMIT(JN) > -1) THEN
          NOBSINCLD = NOBSINCLD + 1
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
          IF(NGRPSCSS > 1) THEN
            DO GN=1,NGRPSCSS
              IF(GROUPNOBS(JN) == GRPSCSS(GN)) THEN
                IF(IP == 1) NOBSGP(GN) = NOBSGP(GN) + 1
                CSSGP(IP,GN)= CSSGP(IP,GN) + DSS(IP,JN)*DSS(IP,JN)
                IF(ABS(DSS(IP,JN)) > ABS(MAX2DSSGP(IP,GN))) THEN
                  MAX2DSSGP(IP,GN) = DSS(IP,JN)
                  MAX2OBSGP(IP,GN) = OBSNAM(JN)
                ENDIF
                IF(ABS(MAX2DSSGP(IP,GN)) > ABS(MAX1DSSGP(IP,GN))) THEN
                  MAX2DSSGP(IP,GN) = MAX1DSSGP(IP,GN)
                  MAX2OBSGP(IP,GN) = MAX1OBSGP(IP,GN)
                  MAX1DSSGP(IP,GN) = DSS(IP,JN)
                  MAX1OBSGP(IP,GN) = OBSNAM(JN)
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDIF
      ENDDO
      CSS(IP)= DSQRT(CSS(IP)/NOBSINCLD)
      IF(NGRPSCSS > 1) THEN
        DO GN=1,NGRPSCSS
          IF(NOBSGP(GN) > 0)CSSGP(IP,GN)= DSQRT(CSSGP(IP,GN)/NOBSGP(GN))
        ENDDO
      ENDIF
      IF(MPR>0) THEN
        MAX1DSSWP(IP) = MAX1DSS(IP)
        MAX1OBSWP(IP) = MAX1OBS(IP)
        MAX2DSSWP(IP) = MAX2DSS(IP)
        MAX2OBSWP(IP) = MAX2OBS(IP)
        DO JN=1,MPR
          IF(ABS(DSSWP(IP,JN)) > ABS(MAX2DSSWP(IP))) THEN
            MAX2DSSWP(IP) = DSSWP(IP,JN)
            MAX2OBSWP(IP) = PRINAM(JN)
          ENDIF
          IF(ABS(MAX2DSSWP(IP)) > ABS(MAX1DSSWP(IP))) THEN
            MAX2DSSWP(IP) = MAX1DSSWP(IP)
            MAX2OBSWP(IP) = MAX1OBSWP(IP)
            MAX1DSSWP(IP) = DSSWP(IP,JN)
            MAX1OBSWP(IP) = PRINAM(JN)
          ENDIF
        ENDDO
      ENDIF
    ENDDO
    !_sc
    IUSC = UTL_DX_OPEN(OUTNAM,'_sc','REPLACE')
    WRITE(IUSC,600)
    DO IP = 1, NPE
      WRITE (IUSC,601) PARNAM(IPTR(IP)),CSS(IP),MAX1DSS(IP),MAX1OBS(IP), &
                       MAX2DSS(IP),MAX2OBS(IP)
    ENDDO
    IF(MPR>0) THEN
      WRITE(IUSC,609)
      WRITE(IUSC,610)
      DO IP = 1, NPE
        WRITE (IUSC,601) PARNAM(IPTR(IP)),CSSWP(IP),MAX1DSSWP(IP), &
                         MAX1OBSWP(IP),MAX2DSSWP(IP),MAX2OBSWP(IP)
      ENDDO
    ENDIF
    IUSC = UTL_DX_CLOSE('_sc')
    !_scgrp
    IF(NGRPSCSS > 1) THEN
      IUSC = UTL_DX_OPEN(OUTNAM,'_scgrp','REPLACE')
      DO GN = 1,NGRPSCSS
        WRITE(IUSC,799)GRPSCSS(GN), NOBSGP(GN)
        WRITE(IUSC,800)
        DO IP = 1, NPE
          WRITE (IUSC,601) PARNAM(IPTR(IP)),CSSGP(IP,GN),MAX1DSSGP(IP,GN), &
                           MAX1OBSGP(IP,GN),MAX2DSSGP(IP,GN),MAX2OBSGP(IP,GN)
        ENDDO
      ENDDO
      IUSC = UTL_DX_CLOSE('_scgrp')
    ENDIF
    !_s1
    DO IP=1,NPE
      DO JN=1,NOBS
        OSS(IP,JN)=XSENS(IP,JN)*PVAL(IPTR(IP))/100.D0
      ENDDO
    ENDDO
    IUS1 = UTL_DX_OPEN(OUTNAM,'_s1','REPLACE',MXRECL)
    CALL SEN_UEV_DX_WRITE_MATRIX(IUS1,NOBS,NPE,NPS,OBSNAM,IPTR,PARNAM,OSS)
    IUS1 = UTL_DX_CLOSE('_s1')
    DEALLOCATE (OSS,MAX1OBS,MAX2OBS,MAX1DSS,MAX2DSS)
    RETURN
  END SUBROUTINE UTLUCODE_DX_WRITE_SEN
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_WRITE_SU(MPR,NOBS,NPE,NPS,OBSNAM,OUTNAM, &
                                  IPTR,PARNAM,PRINAM, &
                                  XPRI,XSENS)
    !   Write _INIT_SU Data-Exchange Files: Sensitivities for non-optimal params
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN, NPPREC
    USE UTILITIES
    USE SENSITIVITY, ONLY: SEN_UEV_DX_WRITE_MATRIX
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: MPR
    INTEGER,                                 INTENT(IN) :: NOBS
    INTEGER,                                 INTENT(IN) :: NPE
    INTEGER,                                 INTENT(IN) :: NPS
    CHARACTER(LEN=LENDNAM), DIMENSION(NOBS), INTENT(IN) :: OBSNAM
    CHARACTER(LEN=MAX_STRING_LEN),           INTENT(IN) :: OUTNAM
    INTEGER,                DIMENSION(NPE),  INTENT(IN) :: IPTR
    CHARACTER(LEN=12),      DIMENSION(NPS),  INTENT(IN) :: PARNAM
    CHARACTER(LEN=LENDNAM), DIMENSION(MPR),  INTENT(IN) :: PRINAM
    DOUBLE PRECISION,    DIMENSION(NPE,MPR), INTENT(IN) :: XPRI
    DOUBLE PRECISION,   DIMENSION(NPE,NOBS), INTENT(IN) :: XSENS
    !
    !   Local variables
    INTEGER   IUSU, MXRECL
    CHARACTER(LEN=MAX_STRING_LEN)          :: OUTNAMTMP
    CHARACTER(LEN=10)  :: STR_NPPREC
    !
    !   Construct format strings
    WRITE(STR_NPPREC,'(I10)') NPPREC
    STR_NPPREC = ADJUSTL(STR_NPPREC)
    MXRECL = 52+NPPREC*26
    !
    ! Open data exchange file
    OUTNAMTMP = TRIM(OUTNAM)//'._init'
    IUSU = UTL_DX_OPEN(OUTNAMTMP,'_su','REPLACE',MXRECL)
    ! Write  data
    CALL SEN_UEV_DX_WRITE_MATRIX(IUSU,NOBS,NPE,NPS,OBSNAM,IPTR,PARNAM,XSENS)
    IUSU = UTL_DX_CLOSE('_su')
    !   Write matrix of sensitivities for prior information, in blocks
    !   of NPPREC parameters
    IF(MPR > 0) THEN
      OUTNAMTMP = TRIM(OUTNAM)//'._init'
      IUSU = UTL_DX_OPEN(OUTNAMTMP,'_supri','REPLACE',MXRECL)
      CALL SEN_UEV_DX_WRITE_MATRIX(IUSU,MPR,NPE,NPS,PRINAM,IPTR,PARNAM,XPRI)
      IUSU = UTL_DX_CLOSE('_supri')
    ENDIF
    RETURN
  END SUBROUTINE UTLUCODE_DX_WRITE_SU
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_READ_SVD &
                    (NPE,NPT,IOUT,OUTNAM,IPTR,PARNAM,SINGVAL,SINGVEC)
    !   READ _SVD DX FILE  SingularValues and Vectors
    USE GLOBAL_DATA, ONLY: AMESSAGE, NPPREC, BIGINTEGER, MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: NPE    ! Number of active parameters
    INTEGER,                                 INTENT(IN) :: NPT    ! Number of parameters, total
    INTEGER,                                 INTENT(IN) :: IOUT   ! Output file for writine msg
    CHARACTER(LEN=MAX_STRING_LEN),           INTENT(IN) :: OUTNAM ! Root name for output file
    INTEGER,                DIMENSION(NPE),  INTENT(IN) :: IPTR   ! Pointers to adjustable parameters
    CHARACTER(LEN=12),      DIMENSION(NPT),  INTENT(OUT) :: PARNAM ! Parameter names
    DOUBLE PRECISION,   DIMENSION(NPE),      INTENT(OUT) :: SINGVAL ! Array of singularvalues
    DOUBLE PRECISION,   DIMENSION(NPE,NPE),  INTENT(OUT) :: SINGVEC ! Array of singularvectors
    !
    !   Local variables
    INTEGER :: IP, IP1, IP2, IUSVD, KB, KC, KP, KQ, MRECL, MREM, N, NPREC1
    CHARACTER(LEN=1) :: CH
    CHARACTER(LEN=MAX_STRING_LEN) :: FN
    LOGICAL :: LEX
    CHARACTER(LEN=300) :: FMT_STRING
    CHARACTER(LEN=10)  :: STR_NPPREC
    !Formats
    903 FORMAT(//,80('!'),/,80('!'),/)
    904 FORMAT(' FILE IS NOT PRESENT IN THE WORKING DIRECTORY',/,A)
    WRITE(STR_NPPREC,'(I10)') NPPREC
    STR_NPPREC = ADJUSTL(STR_NPPREC)
    FMT_STRING = '(39X,'//TRIM(STR_NPPREC)//'(1X,G25.16))'
    !
    LEX = .FALSE.
    !
    FN = TRIM(OUTNAM)
    INQUIRE(FILE=FN,EXIST=LEX)
    IF(LEX) THEN
      LEX = .FALSE.
      IUSVD = UTL_GETUNIT(101,150)
      OPEN(UNIT=IUSVD,FILE=FN,RECL=NPPREC*26+52,STATUS='OLD')
    ELSE
      WRITE(*,903)
      WRITE(IOUT,903)
      WRITE(IOUT,904)TRIM(FN)
      WRITE(*,904)TRIM(FN)
      WRITE(*,903)
      WRITE(IOUT,903)
      CALL UTL_STOP
    ENDIF
    !   Read header
    READ(IUSVD,*) ! skip first line
    MRECL = BIGINTEGER
    KQ = 0
    !   Read header one character at a time to determine number of parameters
    !   per record
    DO KC=1,MRECL
      READ(IUSVD,'(A1)',ADVANCE='NO',EOR=50) CH
      IF (CH=='"') KQ = KQ+1
    ENDDO
    50 CONTINUE
    NPREC1 = KQ/2-3
    MREM = MOD(KQ,2)
    IF (MREM .NE. 0) THEN
      AMESSAGE = &
      'Programming error: UTLUCODE_DX_READ_SVD finds odd number of quotes'
      CALL UTL_STOP()
    ENDIF
    REWIND(IUSVD)
    !
    !   Read singularvalues & vectors, in blocks of NPPREC parameters
    KB = 0
    KP = 0
    !skip 1 header lines
    READ (IUSVD,*)
    DO WHILE (KP<NPE)
      IP1 = KB*NPPREC+1
      KB = KB+1
      IP2 = KB*NPPREC
      IF (IP2>NPE) IP2 = NPE
      READ (IUSVD,*)
      READ(IUSVD,FMT_STRING)(SINGVAL(IP),IP=IP1,IP2)
      READ (IUSVD,*)
      READ (IUSVD,*)
      DO N = 1, NPE
        READ(IUSVD,*)PARNAM(IPTR(N)),(SINGVEC(N,IP),IP=IP1,IP2)
      ENDDO
      KP = IP2
    ENDDO
    CLOSE(IUSVD)
    RETURN
  END SUBROUTINE UTLUCODE_DX_READ_SVD
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_WRITE_SVD(IUSVD,NPE,NPT,IPTR,PARNAM,SINGVAL,SINGVEC)
    !   WRITE _SVD DX FILE  SINGULARValues and Vectors
    USE GLOBAL_DATA, ONLY: AMESSAGE, BLANKS, NPPREC
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: IUSVD  ! Unit number opened for DX file
    INTEGER,                                 INTENT(IN) :: NPE    ! Number of active parameters
    INTEGER,                                 INTENT(IN) :: NPT    ! Number of parameters, total
    INTEGER,                DIMENSION(NPE),  INTENT(IN) :: IPTR   ! Pointers to adjustable parameters
    CHARACTER(LEN=12),      DIMENSION(NPT),  INTENT(IN) :: PARNAM ! Parameter names
    DOUBLE PRECISION,   DIMENSION(NPE),      INTENT(IN) :: SINGVAL ! Array of eigenvalues
    DOUBLE PRECISION,   DIMENSION(NPE,NPE),  INTENT(IN) :: SINGVEC ! Array of eigenvectors
    !
    !   Local variables
    INTEGER :: IP, IP1, IP2, J, KB, KP, N
    CHARACTER(LEN=300) :: FMT_STRING0, FMT_STRING1, FMT_STRING2, FMT_STRING3, &
                          FMT_STRING4
    CHARACTER(LEN=10)  :: STR_NPPREC
    !
    ! Formats
    2 FORMAT(1X,'" Singularvalues and vectors of [wt^0.5][X]{b} matrix')
    !   Construct format strings
    WRITE(STR_NPPREC,'(I10)') NPPREC
    STR_NPPREC = ADJUSTL(STR_NPPREC)
    FMT_STRING0 = '(42X,'// &
                  TRIM(STR_NPPREC)//'(1X,''"SINGULARVALUE '',1X,I8,''"'',:))'
    FMT_STRING3 = '(1X,''"SINGULARVALUE FOR EACH VECTOR  "'',6X,'// &
                  TRIM(STR_NPPREC)//'(1X,G25.16))'
    FMT_STRING4 = '(1X,''"RATIO TO LARGEST SINGULARVALUE "'',6X,'// &
                  TRIM(STR_NPPREC)//'(1X,G25.16))'
    FMT_STRING1 = '(1X,''"PARAMETER FOR EACH VECTOR ELEMENT "'',5X,'// &
                  TRIM(STR_NPPREC)//'(1X,''"SINGULARVECTOR'',1X,I8,''"'',:))'
    FMT_STRING2 = '(1X,A,27X,'//TRIM(STR_NPPREC)//'(1X,G25.16))'
    !   Singular values are in order laregst to smallest
    !   Write singularvalues & vectors, in blocks of NPPREC parameters
    KB = 0
    KP = 0
    WRITE (IUSVD,2)
    DO WHILE (KP<NPE)
      IP1 = KB*NPPREC+1
      KB = KB+1
      IP2 = KB*NPPREC
      IF (IP2>NPE) IP2 = NPE
      WRITE (IUSVD,FMT_STRING0) (J,J=IP1,IP2)
      WRITE (IUSVD,FMT_STRING3) (SINGVAL(J),J=IP1,IP2)
      WRITE (IUSVD,FMT_STRING4) (SINGVAL(J)/SINGVAL(1),J=IP1,IP2)
      WRITE (IUSVD,FMT_STRING1) (J,J=IP1,IP2)
      DO N = 1, NPE
        WRITE (IUSVD,FMT_STRING2) PARNAM(IPTR(N)),(SINGVEC(N,IP),IP=IP1,IP2)
      ENDDO
      KP = IP2
    ENDDO
    RETURN
  END SUBROUTINE UTLUCODE_DX_WRITE_SVD
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_WRITE_INTWR(NTOT,NUMINTERVALS,INTNAM,NTYP, &
                                     ONAM,OUTNAM,PLOTSYM,IWTS)
    !   Write _intwr Data-Exchange Files: weighted residuals at limits
    !   Not this has the same format as a sensitivity file
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN, NPPREC
    USE UTILITIES
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                                          INTENT(IN) :: NTOT
    INTEGER,                                          INTENT(IN) :: NUMINTERVALS
    CHARACTER(LEN=LENDNAM+1),DIMENSION(NUMINTERVALS), INTENT(IN) :: INTNAM
    INTEGER,                                          INTENT(IN) :: NTYP
    CHARACTER(LEN=LENDNAM),        DIMENSION(NTOT),   INTENT(IN) :: ONAM
    CHARACTER(LEN=MAX_STRING_LEN),                    INTENT(IN) :: OUTNAM
    INTEGER,                       DIMENSION(NTOT),   INTENT(IN) :: PLOTSYM
    DOUBLE PRECISION, DIMENSION(NUMINTERVALS,NTOT),   INTENT(IN) :: IWTS
    !   Local variables
    CHARACTER(LEN=25)                  :: COL12NAM(2)
    INTEGER                            :: I
    INTEGER, ALLOCATABLE, DIMENSION(:) :: IPTR
    INTEGER                            :: IUIWT
    INTEGER                            :: MXRECL
    !
    ALLOCATE(IPTR(NUMINTERVALS))
    !
    COL12NAM(1) = 'OBSERVATION or PRIOR NAME'
    COL12NAM(2) = 'PLOT SYMBOL'
    MXRECL = 406+NPPREC*32
    !
    DO I=1,NUMINTERVALS
      IPTR(I) = I
    ENDDO
    ! Open data exchange file
    IF(NTYP == 1) THEN
      IUIWT = UTL_DX_OPEN(OUTNAM,'_intconfwr','REPLACE',MXRECL)
    ELSE
      IUIWT = UTL_DX_OPEN(OUTNAM,'_intpredwr','REPLACE',MXRECL)
    ENDIF
    ! Write  interval weighted residuals for dependents
    CALL UTLUCODE_DX_WRITE_MATRIX(IUIWT,NTOT,NUMINTERVALS,NUMINTERVALS, &
                                  IPTR,INTNAM,IWTS,ONAM,PLOTSYM,COL12NAM)
    ! Close data exchange file
    IF(NTYP == 1) THEN
      IUIWT = UTL_DX_CLOSE('_intconfwr')
    ELSE
      IUIWT = UTL_DX_CLOSE('_intpredwr')
    ENDIF
    DEALLOCATE(IPTR)
    RETURN
  END SUBROUTINE UTLUCODE_DX_WRITE_INTWR
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_READ_WW(NTOT,OUTNAM,WTSIMEQV,WTOBS,PLOTSYM,OBSNAME)
    !    READ_WW DX FILE weighted simulated equivalents and weighted residuals
    USE GLOBAL_DATA, ONLY: LENDNAM, MAX_STRING_LEN
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                    INTENT(IN)  :: NTOT
    CHARACTER(LEN=MAX_STRING_LEN),              INTENT(IN)  :: OUTNAM
    DOUBLE PRECISION, DIMENSION(NTOT),          INTENT(OUT) :: WTSIMEQV
    DOUBLE PRECISION, DIMENSION(NTOT),          INTENT(OUT) :: WTOBS
    INTEGER, DIMENSION(NTOT),                   INTENT(OUT) :: PLOTSYM
    CHARACTER(LEN=LENDNAM), DIMENSION(NTOT),    INTENT(OUT) :: OBSNAME
    !
    !   Local variables
    INTEGER                                                :: I
    INTEGER                                                :: IUWW
    !
    IUWW = UTL_DX_OPEN(OUTNAM,'_ww','OLD')
    ! READ Header
    READ (IUWW,*,END=100)
    DO I=1,NTOT
      READ (IUWW,*,END=100)WTSIMEQV(I),WTOBS(I),PLOTSYM(I),OBSNAME(I)
    ENDDO
    IUWW = UTL_DX_CLOSE('_ww')
    !
    RETURN
    100 IUWW = UTL_DX_CLOSE('_ww')
    CALL UTL_STOP('UNEXPECTED CONTENT _ww file, CHECK VERSION/RE-CREATE _ww')
    RETURN
  END SUBROUTINE UTLUCODE_DX_READ_WW
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_WRITE_COLS(IUSM,NROW,NPE,NAM,VALS,ROWLAB,ROWNUM)
!                                                                   ,COL12LAB)
    !   Write matrix with known # COLs no wrapping (NROWxNPE MATRIX)
    !
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: IUSM
    INTEGER,                                 INTENT(IN) :: NROW
    INTEGER,                                 INTENT(IN) :: NPE
    CHARACTER(LEN=13),    DIMENSION(NPE+2),  INTENT(IN) :: NAM
    DOUBLE PRECISION,   DIMENSION(NPE,NROW), INTENT(IN) :: VALS
    CHARACTER(LEN=*),       DIMENSION(NROW), INTENT(IN) :: ROWLAB
    INTEGER,                DIMENSION(NROW), INTENT(IN) :: ROWNUM
!    CHARACTER(LEN=*), OPTIONAL, DIMENSION(2),INTENT(IN) :: COL12LAB
    !
    !   Local variables
    INTEGER :: IP, N
    ! formats
    100 FORMAT(500(5X,'"',A13,'"',:))
    !   Write matrix
    WRITE (IUSM,100) (NAM(IP),IP=1,NPE+2)
    DO N=1,NROW
      WRITE (IUSM,*) ROWLAB(N),ROWNUM(N),(VALS(IP,N),IP=1,NPE)
    ENDDO
    RETURN
  END SUBROUTINE UTLUCODE_DX_WRITE_COLS
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_WRITE_MATRIX(IUSM,NROW,NPE,NPT,IPTR,PARNAM,VALS, &
                                      ROWLAB,ROWNUM,COL12LAB)
    !   Write matrix with unknown # parameters such that a limited number occur
    !   on a line and the rest wraps (WRAPPED NROWxNPE MATRIX)
    !
    USE GLOBAL_DATA, ONLY: BLANKS, NPPREC
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: IUSM
    INTEGER,                                 INTENT(IN) :: NROW
    INTEGER,                                 INTENT(IN) :: NPE
    INTEGER,                                 INTENT(IN) :: NPT
    INTEGER,                DIMENSION(NPE),  INTENT(IN) :: IPTR
    CHARACTER(LEN=*),       DIMENSION(NPT), INTENT(IN) :: PARNAM
    DOUBLE PRECISION,   DIMENSION(NPE,NROW), INTENT(IN) :: VALS
    CHARACTER(LEN=*),       DIMENSION(NROW), INTENT(IN) :: ROWLAB
    INTEGER,                DIMENSION(NROW), INTENT(IN) :: ROWNUM
    CHARACTER(LEN=*), OPTIONAL, DIMENSION(2),INTENT(IN) :: COL12LAB
    !
    !   Local variables
    INTEGER :: IP, IP1, IP2, KB, KP, N
    CHARACTER(LEN=200) :: FMT_STRING1, FMT_STRING2
    CHARACTER(LEN=10)  :: STR_NPPREC
    !
    !   Construct format strings
    WRITE(STR_NPPREC,'(I10)') NPPREC
    STR_NPPREC = ADJUSTL(STR_NPPREC)
    IF(PRESENT(COL12LAB))THEN
      FMT_STRING1 = '(1X,''"'//TRIM(COL12LAB(1))//'" "'//TRIM(COL12LAB(2))// &
                    '"'','//TRIM(STR_NPPREC)//'(10X,''"'',A,''"'',:))'
    ELSE
      FMT_STRING1 = '(1X,''"LABEL"          "PLOT SYMBOL" '',' &
                    //TRIM(STR_NPPREC)//'(10X,''"'',A,''"'',:))'
    ENDIF
      FMT_STRING2 = '(1X,A,1X,I10,1X,'//TRIM(STR_NPPREC)//'(1X,G25.16))'
    !
    !   Write matrix, in blocks of NPPREC parameters
    KB = 0
    KP = 0
    BLOK: DO WHILE (KP<NPE)
      IP1 = KB*NPPREC+1
      KB = KB+1
      IP2 = KB*NPPREC
      IF (IP2 > NPE) IP2 = NPE
      WRITE (IUSM,FMT_STRING1) (PARNAM(IPTR(IP)),IP=IP1,IP2)
      DO N = 1, NROW
        WRITE (IUSM,FMT_STRING2) ROWLAB(N),ROWNUM(N),(VALS(IP,N),IP=IP1,IP2)
      ENDDO
      KP = IP2
    ENDDO BLOK
    RETURN
  END SUBROUTINE UTLUCODE_DX_WRITE_MATRIX
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_WRITE_MATRIXNL(IUSM,NROW,NPT,PARNAM,VALS)
    !   Write matrix with unknown # parameters such that a limited number occur
    !   on a line and the rest wraps (WRAPPED NROWxNPT MATRIX)
    !
    USE GLOBAL_DATA, ONLY: BLANKS, NPPREC
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: IUSM
    INTEGER,                                 INTENT(IN) :: NROW
    INTEGER,                                 INTENT(IN) :: NPT
    CHARACTER(LEN=12),       DIMENSION(NPT), INTENT(IN) :: PARNAM
    DOUBLE PRECISION,   DIMENSION(NPT,NROW), INTENT(IN) :: VALS
    !
    !   Local variables
    INTEGER :: IP, IP1, IP2, KB, KP, N
    CHARACTER(LEN=200) :: FMT_STRING1, FMT_STRING2
    CHARACTER(LEN=10)  :: STR_NPPREC
    !
    !   Construct format strings
    WRITE(STR_NPPREC,'(I10)') NPPREC
    STR_NPPREC = ADJUSTL(STR_NPPREC)
    FMT_STRING1 = '(1X,'//TRIM(STR_NPPREC)//'(12X,''"'',A12,''"'',:))'
    FMT_STRING2 = '(1X,'//TRIM(STR_NPPREC)//'(1X,G25.16))'
    !
    !   Write matrix, in blocks of NPPREC parameters
    KB = 0
    KP = 0
    BLOK: DO WHILE (KP<NPT)
      IP1 = KB*NPPREC+1
      KB = KB+1
      IP2 = KB*NPPREC
      IF (IP2 > NPT) IP2 = NPT
      WRITE (IUSM,FMT_STRING1) (PARNAM(IP),IP=IP1,IP2)
      DO N = 1, NROW
        WRITE (IUSM,FMT_STRING2)(VALS(IP,N),IP=IP1,IP2)
      ENDDO
      KP = IP2
    ENDDO BLOK
    RETURN
  END SUBROUTINE UTLUCODE_DX_WRITE_MATRIXNL
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_DX_WRITE_MATRIXNLOBS(IU,NROW,NCOL,ONAM,VALS)
    !   Write matrix with unknown # observations such that a limited number
    !   occur on a line and the rest wrap (WRAPPED NROWxNPE MATRIX)
    !
    USE GLOBAL_DATA, ONLY: BLANKS, LENDNAM, NPPREC
    USE UTILITIES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: IU
    INTEGER,                                 INTENT(IN) :: NROW
    INTEGER,                                 INTENT(IN) :: NCOL
    CHARACTER(LEN=LENDNAM), DIMENSION(NCOL), INTENT(IN) :: ONAM
    DOUBLE PRECISION,  DIMENSION(NCOL,NROW), INTENT(IN) :: VALS
    !
    !   Local variables
    INTEGER :: IP, IP1, IP2, KB, KP, N
    CHARACTER(LEN=200) :: FMT_STRING1, FMT_STRING2
    CHARACTER(LEN=10)  :: STR_NPPREC
    !
    !   Construct format strings
    WRITE(STR_NPPREC,'(I10)') NPPREC
    STR_NPPREC = ADJUSTL(STR_NPPREC)
    FMT_STRING1 = '(1X,'//TRIM(STR_NPPREC)//'(4X,''"'',A20,''"'',:))'
    FMT_STRING2 = '(1X,'//TRIM(STR_NPPREC)//'(1X,G25.16))'
    !
    !   Write matrix, in blocks of NPPREC parameters
    KB = 0
    KP = 0
    BLOK: DO WHILE (KP<NCOL)
      IP1 = KB*NPPREC+1
      KB = KB+1
      IP2 = KB*NPPREC
      IF (IP2 > NCOL) IP2 = NCOL
      WRITE (IU,FMT_STRING1) (ONAM(IP),IP=IP1,IP2)
      DO N = 1, NROW
        WRITE (IU,FMT_STRING2)(VALS(IP,N),IP=IP1,IP2)
      ENDDO
      KP = IP2
    ENDDO BLOK
    RETURN
  END SUBROUTINE UTLUCODE_DX_WRITE_MATRIXNLOBS
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_SHELLSORT_VALUE(N,X)
    !    Algorithm obtained from http://www.nist.gov/dads/HTML/shellsort.html
    !    Thank you NIST!
    !  ALGORITHM AS 304.8 APPL.STATIST. (1996), VOL.45, NO.3
    !MODIFIED HERE TO SORT IN DESCENDING ORDER
    !  Sorts the N values stored in array X in ascending order
    !  Based on an "adaptation by Marlene Metzner" this algorithm is referred to
    !  as the Shell-Metzner sort by John P. Grillo, A Comparison of Sorts,
    !  Creative Computing, 2:76-80, Nov/Dec 1976. Grillo cites Fredric Stuart,
    !  FORTRAN Programming, John Wiley and Sons, New York, 1969, page 294. In
    !  crediting "one of the fastest" programs for sorting, Stuart says in a
    !  footnote, "Published by Marlene Metzner, Pratt & Whitney Aircraft
    !  Company. From a method described by D. L. Shell."
    !
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                        INTENT(IN)    ::  N
    DOUBLE PRECISION, DIMENSION(N), INTENT(INOUT) ::  X
    !
    ! Local variables
    DOUBLE PRECISION                ::  TEMP
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: Y
    INTEGER                         ::  I
    INTEGER                         ::  J
    INTEGER                         ::  INCR = 1
    !
    ALLOCATE(Y(N))
    !    Loop : calculate the increment
    !
    10 INCR = 3 * INCR + 1
    IF (INCR .LE. N) GOTO 10
    !
    !    Loop : Shell-Metzner sort
    !
    20 INCR = INCR / 3
    I = INCR + 1
    30 IF (I .GT. N) GOTO 60
    TEMP = X(I)
    J = I
    40 IF (X(J - INCR) .LT. TEMP) GOTO 50
    X(J) = X(J - INCR)
    J = J - INCR
    IF (J .GT. INCR) GOTO 40
    50 X(J) = TEMP
    I = I + 1
    GOTO 30
    60 IF (INCR .GT. 1) GOTO 20
    ! reorder descending
    DO I = 1,N
      Y(N+1-I) = X(I)
    ENDDO
    X = Y
    DEALLOCATE(Y)
    !
    RETURN
  END SUBROUTINE UTLUCODE_SHELLSORT_VALUE
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_SHELLSORT_INTEGERS(N,X,XX)
    !    Algorithm obtained from http://www.nist.gov/dads/HTML/shellsort.html
    !    Thank you NIST!
    !  ALGORITHM AS 304.8 APPL.STATIST. (1996), VOL.45, NO.3
    !  Sorts the N values stored in array X in ascending order
    !  Based on an "adaptation by Marlene Metzner" this algorithm is referred to
    !  as the Shell-Metzner sort by John P. Grillo, A Comparison of Sorts,
    !  Creative Computing, 2:76-80, Nov/Dec 1976. Grillo cites Fredric Stuart,
    !  FORTRAN Programming, John Wiley and Sons, New York, 1969, page 294. In
    !  crediting "one of the fastest" programs for sorting, Stuart says in a
    !  footnote, "Published by Marlene Metzner, Pratt & Whitney Aircraft
    !  Company. From a method described by D. L. Shell."
    !
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                        INTENT(IN)    ::  N
    INTEGER, DIMENSION(N), INTENT(INOUT) ::  X
    INTEGER, DIMENSION(N), INTENT(INOUT) ::  XX
    !
    ! Local variables
    INTEGER                         ::  TEMP
    INTEGER                         ::  TEMP2
    INTEGER                         ::  I
    INTEGER                         ::  J
    INTEGER                         ::  INCR = 1
    !
    !    Loop : calculate the increment
    !
    10 INCR = 3 * INCR + 1
    IF (INCR .LE. N) GOTO 10
    !
    !    Loop : Shell-Metzner sort
    !
    20 INCR = INCR / 3
    I = INCR + 1
    30 IF (I .GT. N) GOTO 60
    TEMP = X(I)
    TEMP2 = XX(I)
    J = I
    40 IF (X(J - INCR) .LT. TEMP) GOTO 50
    X(J) = X(J - INCR)
    XX(J) = XX(J - INCR)
    J = J - INCR
    IF (J .GT. INCR) GOTO 40
    50 X(J) = TEMP
    XX(J) = TEMP2
    I = I + 1
    GOTO 30
    60 IF (INCR .GT. 1) GOTO 20
    !
    RETURN
  END SUBROUTINE UTLUCODE_SHELLSORT_INTEGERS
!===========================================================
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_SVD(IERR,IOUT,OUTNAM,NPE,NPT,IPTR,PARNAM, &
                          NMIN,MMIN,N,A,MATU,MATV,SVDPRINT,SVDLOOP,SVDPHASE1, &
                          SVAL,SVEC,MPR,ANP,SVALNP,SVECNP)
    !     CALCULATE SVDs from wt1/2X
    ! SOURCE CODE MODIFIED FROM http://www.netlib.org/fmm/svd.f
    !
    USE GLOBAL_DATA, ONLY: AMESSAGE, BLANKS, IVERB, MAX_STRING_LEN, NPPREC
    IMPLICIT NONE
    !
    ! Argument list variables
    INTEGER,                            INTENT(INOUT)  :: IERR
    INTEGER,                            INTENT(IN)     :: IOUT
    CHARACTER(LEN=MAX_STRING_LEN),      INTENT(IN)     :: OUTNAM
    INTEGER,                            INTENT(IN)     :: NPE    ! Number of active parameters
    INTEGER,                            INTENT(IN)     :: NPT    ! Number of parameters, total
    INTEGER,           DIMENSION(NPE),  INTENT(IN)     :: IPTR   ! Pointers to adjustable parameters
    CHARACTER(LEN=12), DIMENSION(NPT),  INTENT(IN)     :: PARNAM ! Parameter names
    INTEGER,                            INTENT(IN)     :: NMIN
    INTEGER,                            INTENT(IN)     :: MMIN
    INTEGER,                            INTENT(IN)     :: N
    DOUBLE PRECISION,                   INTENT(IN)     :: A(NMIN,N)
    LOGICAL,                            INTENT(IN)     :: MATU
    LOGICAL,                            INTENT(IN)     :: MATV
    LOGICAL,                            INTENT(IN)     :: SVDPRINT
    INTEGER,                            INTENT(IN)     :: SVDLOOP
    LOGICAL,                            INTENT(IN)     :: SVDPHASE1
    DOUBLE PRECISION,                   INTENT(OUT)    :: SVAL(NPE)
    DOUBLE PRECISION,                   INTENT(OUT)    :: SVEC(NPE,NPE)
    INTEGER,                            INTENT(IN)     :: MPR
    DOUBLE PRECISION, OPTIONAL,         INTENT(IN)     :: ANP(NMIN-MPR,N)
    DOUBLE PRECISION, OPTIONAL,         INTENT(OUT)    :: SVALNP(NPE)
    DOUBLE PRECISION, OPTIONAL,         INTENT(OUT)    :: SVECNP(NPE,NPE)
    !   Local variables
    INTEGER I,J,K,L,II,I1,KK,K1,LL,L1,MN,ITS
    INTEGER NM, M
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: W
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: U
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: V
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: RV1
    DOUBLE PRECISION C,F,G,H,S,X,Y,Z,SCALE,ANORM
    DOUBLE PRECISION DSQRT,DMAX1,DABS,DSIGN
    INTEGER IUSVD, IUSVDPL, NLP, NLOOP, NPTOP
    LOGICAL REORDER
    INTEGER, ALLOCATABLE, DIMENSION(:)   :: IPTRSVAL
    INTEGER, ALLOCATABLE, DIMENSION(:)   :: USEDIPTR
    CHARACTER(LEN=150) :: FMT_STRING
    CHARACTER(LEN=100) :: FMT_STRING2
    CHARACTER(LEN=10)  :: STR_NPPREC
    CHARACTER(LEN=MAX_STRING_LEN) :: OUTNAMTMP
!
!     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE SVD,
!     NUM. MATH. 14, 403-420(1970) BY GOLUB AND REINSCH.
!     HANDBOOK FOR AUTO. COMP., VOL II-LINEAR ALGEBRA, 134-151(1971).
!
!     THIS SUBROUTINE DETERMINES THE SINGULAR VALUE DECOMPOSITION
!          T
!     A=USV  OF A REAL M BY N RECTANGULAR MATRIX.  HOUSEHOLDER
!     BIDIAGONALIZATION AND A VARIANT OF THE QR ALGORITHM ARE USED.
!
!     ON INPUT.
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.  NOTE THAT NM MUST BE AT LEAST
!          AS LARGE AS THE MAXIMUM OF M AND N.
!
!        M IS THE NUMBER OF ROWS OF A (AND U).
!
!        N IS THE NUMBER OF COLUMNS OF A (AND U) AND THE ORDER OF V.
!
!        A CONTAINS THE RECTANGULAR INPUT MATRIX TO BE DECOMPOSED.
!
!        MATU SHOULD BE SET TO .TRUE. IF THE U MATRIX IN THE
!          DECOMPOSITION IS DESIRED, AND TO .FALSE. OTHERWISE.
!
!        MATV SHOULD BE SET TO .TRUE. IF THE V MATRIX IN THE
!          DECOMPOSITION IS DESIRED, AND TO .FALSE. OTHERWISE.
!
!     ON OUTPUT.
!
!        A IS UNALTERED (UNLESS OVERWRITTEN BY U OR V).
!
!        W CONTAINS THE N (NON-NEGATIVE) SINGULAR VALUES OF A (THE
!          DIAGONAL ELEMENTS OF S).  THEY ARE UNORDERED.  IF AN
!          ERROR EXIT IS MADE, THE SINGULAR VALUES SHOULD BE CORRECT
!          FOR INDICES IERR+1,IERR+2,...,N.
!
!        U CONTAINS THE MATRIX U (ORTHOGONAL COLUMN VECTORS) OF THE
!          DECOMPOSITION IF MATU HAS BEEN SET TO .TRUE.  OTHERWISE
!          U IS USED AS A TEMPORARY ARRAY.  U MAY COINCIDE WITH A.
!          IF AN ERROR EXIT IS MADE, THE COLUMNS OF U CORRESPONDING
!          TO INDICES OF CORRECT SINGULAR VALUES SHOULD BE CORRECT.
!
!        V CONTAINS THE MATRIX V (ORTHOGONAL) OF THE DECOMPOSITION IF
!          MATV HAS BEEN SET TO .TRUE.  OTHERWISE V IS NOT REFERENCED.
!          V MAY ALSO COINCIDE WITH A IF U IS NOT NEEDED.  IF AN ERROR
!          EXIT IS MADE, THE COLUMNS OF V CORRESPONDING TO INDICES OF
!          CORRECT SINGULAR VALUES SHOULD BE CORRECT.
!
!        IERR IS SET TO
!          ZERO       FOR NORMAL RETURN,
!          K          IF THE K-TH SINGULAR VALUE HAS NOT BEEN
!                     DETERMINED AFTER 30 ITERATIONS.
!
!        RV1 IS A TEMPORARY STORAGE ARRAY.
!
!     THIS IS A MODIFIED VERSION OF A ROUTINE FROM THE EISPACK
!     COLLECTION BY THE NATS PROJECT
!
!     MODIFIED TO ELIMINATE MACHEP
!
    ! Formats
    503 FORMAT (A12,6E13.4)
    505 FORMAT (/,12X,6E13.4)
    511 FORMAT (/,' SINGULAR VALUES OF [wt^0.5][X]{b}')
    516 FORMAT (/, &
               ' VECTORS ASSOCIATED WITH SINGULAR VALUES OF [wt^0.5][X]{b}',/)
    602 FORMAT (/,1X,'SINGULAR VALUES and VECTORS ARE FIT-INDEPENDENT',//)
   9997 FORMAT (/,1X,'SINGULAR VALUES WERE REORDERED AS FOLLOWS:')
   9998 FORMAT (/,' "WITHOUT PRIOR IN THE [wt^0.5][X]{b} MATRIX"')
   9999 FORMAT (/,1X,80('*'))
!
    WRITE(STR_NPPREC,'(I10)') NPPREC
    STR_NPPREC = ADJUSTL(STR_NPPREC)
    FMT_STRING =  '(1X,''"SINGULAR VALUES FOR EACH VECTOR"' // &
    ' "PARAMETER FOR EACH VECTOR ELEMENT "'','// &
    ',5X,'//TRIM(STR_NPPREC)//'(1X,''"VECTOR     '',1X,I11,''"'',:))'
    FMT_STRING2 = '(1X,G25.16,9X,A,27X,'//TRIM(STR_NPPREC)//'(1X,G25.16))'
!
    ! WRITE to _svd
    OUTNAMTMP = TRIM(OUTNAM)//'._svd'
    IUSVD = UTL_GETUNIT(101,150)
    OPEN(UNIT=IUSVD,FILE=OUTNAMTMP,RECL=NPPREC*26+52,STATUS='REPLACE')
!
    NLOOP = 1
    NM = NMIN
    M = MMIN

    IF(PRESENT(ANP) .AND. PRESENT(SVALNP) .AND. PRESENT(SVECNP))  NLOOP = 2

    IERR = 0
    ALLOCATE(U(NM,N))
    ALLOCATE(V(NM,N))
    ALLOCATE(W(N))
    ALLOCATE(RV1(N))
    ALLOCATE(IPTRSVAL(NPE))
    ALLOCATE(USEDIPTR(NPE))
    U = 0.D0
    V = 0.D0
    W = 0.D0
    SVEC = 0.D0
    SVAL = 0.D0
    RV1 = 0.D0
!
    DO 999 NLP = 1,NLOOP

      IF(NLP == 1) THEN
        DO 100 I = 1, M
           DO 100 J = 1, N
              U(I,J) = A(I,J)
  100   CONTINUE
      ELSE
        NM = NMIN-MPR
        M = MMIN-MPR
        IERR = 0
        IF(ALLOCATED(U)) DEALLOCATE(U)
        IF(ALLOCATED(V)) DEALLOCATE(V)
        ALLOCATE(U(NM,N))
        ALLOCATE(V(NM,N))
        IPTRSVAL = 0
        USEDIPTR = 0
        U = 0.D0
        V = 0.D0
        W = 0.D0
        SVECNP = 0.D0
        SVALNP = 0.D0
        RV1 = 0.D0
        DO 1100 I = 1, M
           DO 1100 J = 1, N
              U(I,J) = ANP(I,J)
 1100   CONTINUE
      ENDIF

!     .......... HOUSEHOLDER REDUCTION TO BIDIAGONAL FORM ..........
      G = 0.0D0
      SCALE = 0.0D0
      ANORM = 0.0D0
!
      DO 300 I = 1, N
         L = I + 1
         RV1(I) = SCALE * G
         G = 0.0D0
         S = 0.0D0
         SCALE = 0.0D0
         IF (I .GT. M) GO TO 210
!
         DO 120 K = I, M
  120    SCALE = SCALE + DABS(U(K,I))
!
         IF (SCALE .EQ. 0.0D0) GO TO 210
!
         DO 130 K = I, M
            U(K,I) = U(K,I) / SCALE
            S = S + U(K,I)**2
  130    CONTINUE
!
         F = U(I,I)
         G = -DSIGN(DSQRT(S),F)
         H = F * G - S
         U(I,I) = F - G
         IF (I .EQ. N) GO TO 190
!
         DO 150 J = L, N
            S = 0.0D0
!
            DO 140 K = I, M
  140       S = S + U(K,I) * U(K,J)
!
            F = S / H
!
            DO 150 K = I, M
               U(K,J) = U(K,J) + F * U(K,I)
  150    CONTINUE
!
  190    DO 200 K = I, M
  200    U(K,I) = SCALE * U(K,I)
!
  210    W(I) = SCALE * G
         G = 0.0D0
         S = 0.0D0
         SCALE = 0.0D0
         IF (I .GT. M .OR. I .EQ. N) GO TO 290
!
         DO 220 K = L, N
  220    SCALE = SCALE + DABS(U(I,K))
!
         IF (SCALE .EQ. 0.0D0) GO TO 290
!
         DO 230 K = L, N
            U(I,K) = U(I,K) / SCALE
            S = S + U(I,K)**2
  230    CONTINUE
!
         F = U(I,L)
         G = -DSIGN(DSQRT(S),F)
         H = F * G - S
         U(I,L) = F - G
!
         DO 240 K = L, N
  240    RV1(K) = U(I,K) / H
!
         IF (I .EQ. M) GO TO 270
!
         DO 260 J = L, M
            S = 0.0D0
!
            DO 250 K = L, N
  250       S = S + U(J,K) * U(I,K)
!
            DO 260 K = L, N
               U(J,K) = U(J,K) + S * RV1(K)
  260    CONTINUE
!
  270    DO 280 K = L, N
  280    U(I,K) = SCALE * U(I,K)
!
  290    ANORM = DMAX1(ANORM,DABS(W(I))+DABS(RV1(I)))
  300 CONTINUE
!     .......... ACCUMULATION OF RIGHT-HAND TRANSFORMATIONS ..........
      IF (.NOT. MATV) GO TO 410
!     .......... FOR I=N STEP -1 UNTIL 1 DO -- ..........
      DO 400 II = 1, N
         I = N + 1 - II
         IF (I .EQ. N) GO TO 390
         IF (G .EQ. 0.0D0) GO TO 360
!
         DO 320 J = L, N
!     .......... DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW ..........
  320    V(J,I) = (U(I,J) / U(I,L)) / G
!
         DO 350 J = L, N
            S = 0.0D0
!
            DO 340 K = L, N
  340       S = S + U(I,K) * V(K,J)
!
            DO 350 K = L, N
               V(K,J) = V(K,J) + S * V(K,I)
  350    CONTINUE
!
  360    DO 380 J = L, N
            V(I,J) = 0.0D0
            V(J,I) = 0.0D0
  380    CONTINUE
!
  390    V(I,I) = 1.0D0
         G = RV1(I)
         L = I
  400 CONTINUE
!     .......... ACCUMULATION OF LEFT-HAND TRANSFORMATIONS ..........
  410 IF (.NOT. MATU) GO TO 510
!     ..........FOR I=MIN(M,N) STEP -1 UNTIL 1 DO -- ..........
      MN = N
      IF (M .LT. N) MN = M
!
      DO 500 II = 1, MN
         I = MN + 1 - II
         L = I + 1
         G = W(I)
         IF (I .EQ. N) GO TO 430
!
         DO 420 J = L, N
  420    U(I,J) = 0.0D0
!
  430    IF (G .EQ. 0.0D0) GO TO 475
         IF (I .EQ. MN) GO TO 460
!
         DO 450 J = L, N
            S = 0.0D0
!
            DO 440 K = L, M
  440       S = S + U(K,I) * U(K,J)
!     .......... DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW ..........
            F = (S / U(I,I)) / G
!
            DO 450 K = I, M
               U(K,J) = U(K,J) + F * U(K,I)
  450    CONTINUE
!
  460    DO 470 J = I, M
  470    U(J,I) = U(J,I) / G
!
         GO TO 490
!
  475    DO 480 J = I, M
  480    U(J,I) = 0.0D0
!
  490    U(I,I) = U(I,I) + 1.0D0
  500 CONTINUE
!     .......... DIAGONALIZATION OF THE BIDIAGONAL FORM ..........
!     .......... FOR K=N STEP -1 UNTIL 1 DO -- ..........
  510 DO 700 KK = 1, N
         K1 = N - KK
         K = K1 + 1
         ITS = 0
!     .......... TEST FOR SPLITTING.
!                FOR L=K STEP -1 UNTIL 1 DO -- ..........
  520    DO 530 LL = 1, K
            L1 = K - LL
            L = L1 + 1
            IF (DABS(RV1(L)) + ANORM .EQ. ANORM) GO TO 565
!     .......... RV1(1) IS ALWAYS ZERO, SO THERE IS NO EXIT
!                THROUGH THE BOTTOM OF THE LOOP ..........
           IF (DABS(W(L1)) + ANORM .EQ. ANORM) GO TO 540
  530    CONTINUE
!     .......... CANCELLATION OF RV1(L) IF L GREATER THAN 1 ..........
  540    C = 0.0D0
         S = 1.0D0
!
         DO 560 I = L, K
            F = S * RV1(I)
            RV1(I) = C * RV1(I)
            IF (DABS(F) + ANORM .EQ. ANORM) GO TO 565
            G = W(I)
            H = DSQRT(F*F+G*G)
            W(I) = H
            C = G / H
            S = -F / H
            IF (.NOT. MATU) GO TO 560
!
            DO 550 J = 1, M
               Y = U(J,L1)
               Z = U(J,I)
               U(J,L1) = Y * C + Z * S
               U(J,I) = -Y * S + Z * C
  550       CONTINUE
!
  560    CONTINUE
!     .......... TEST FOR CONVERGENCE ..........
  565    Z = W(K)
         IF (L .EQ. K) GO TO 650
!     .......... SHIFT FROM BOTTOM 2 BY 2 MINOR ..........
         IF (ITS .EQ. 30) GO TO 1000
         ITS = ITS + 1
         X = W(L)
         Y = W(K1)
         G = RV1(K1)
         H = RV1(K)
         F = ((Y - Z) * (Y + Z) + (G - H) * (G + H)) / (2.0D0 * H * Y)
         G = DSQRT(F*F+1.0D0)
         F = ((X - Z) * (X + Z) + H * (Y / (F + DSIGN(G,F)) - H)) / X
!     .......... NEXT QR TRANSFORMATION ..........
         C = 1.0D0
         S = 1.0D0
!
         DO 600 I1 = L, K1
            I = I1 + 1
            G = RV1(I)
            Y = W(I)
            H = S * G
            G = C * G
            Z = DSQRT(F*F+H*H)
            RV1(I1) = Z
            C = F / Z
            S = H / Z
            F = X * C + G * S
            G = -X * S + G * C
            H = Y * S
            Y = Y * C
            IF (.NOT. MATV) GO TO 575
!
            DO 570 J = 1, N
               X = V(J,I1)
               Z = V(J,I)
               V(J,I1) = X * C + Z * S
               V(J,I) = -X * S + Z * C
  570       CONTINUE
!
  575       Z = DSQRT(F*F+H*H)
            W(I1) = Z
!     .......... ROTATION CAN BE ARBITRARY IF Z IS ZERO ..........
            IF (Z .EQ. 0.0D0) GO TO 580
            C = F / Z
            S = H / Z
  580       F = C * G + S * Y
            X = -S * G + C * Y
            IF (.NOT. MATU) GO TO 600
!
            DO 590 J = 1, M
               Y = U(J,I1)
               Z = U(J,I)
               U(J,I1) = Y * C + Z * S
               U(J,I) = -Y * S + Z * C
  590       CONTINUE
!
  600    CONTINUE
!
         RV1(L) = 0.0D0
         RV1(K) = F
         W(K) = X
         GO TO 520
!     .......... CONVERGENCE ..........
  650    IF (Z .GE. 0.0D0) GO TO 700
!     .......... W(K) IS MADE NON-NEGATIVE ..........
         W(K) = -Z
         IF (.NOT. MATV) GO TO 700
!
         DO 690 J = 1, N
  690    V(J,K) = -V(J,K)
!
  700 CONTINUE
! Sort W from Highest to lowest and put in SVAL
! Put V in SVEC with columns switched to match SVAL order
      IF(NLP == 1) THEN
        SVAL = W
        SVEC = V
        CALL UTLUCODE_SHELLSORT_VALUE(NPE,SVAL)
        IPTRSVAL = 0
        USEDIPTR = 0
        DO I = 1,NPE
          DO J = 1,NPE
            IF(USEDIPTR(J) == 1) CYCLE
            IF(SVAL(I) == W(J)) THEN
              IPTRSVAL(I) = J
              USEDIPTR(J) = 1
              EXIT
            ENDIF
          ENDDO
        ENDDO
        DO I = 1,NPE
          DO J = 1,NPE
            SVEC(I,J) = V(I,IPTRSVAL(J))
          ENDDO
        ENDDO
        ! Determine if reordered

        REORDER = .FALSE.
        IF(IPTRSVAL(1) .NE. 1) REORDER = .TRUE.
        DO I = 2,NPE
          IF(IPTRSVAL(I) < IPTRSVAL(I-1)) REORDER = .TRUE.
        ENDDO
        IF(REORDER .AND. IVERB > 4) THEN
          WRITE(IOUT,9999)
          WRITE(IOUT,9997)
          WRITE(IOUT,*)IPTRSVAL
          WRITE(IOUT,9999)
        ENDIF
        DO I = 1,NPE
          DO J = 1,NPE
            SVEC(I,J) = V(I,IPTRSVAL(J))
          ENDDO
        ENDDO
        ! WRITE to #uout
        IF(SVDPRINT) THEN
          WRITE (IOUT,511)
          DO J=1,NPE,6
            NPTOP = J+5
            IF(J+5 > NPE) NPTOP=NPE
            WRITE (IOUT,'(/,6X,6I13)') (I,I=J,NPTOP)
            WRITE (IOUT,505) (SVAL(I),I=J,NPTOP)
            WRITE (IOUT,516)
            DO K = 1, NPE
              WRITE (IOUT,503) PARNAM(IPTR(K)),(SVEC(K,I),I=J,NPTOP)
            ENDDO
            WRITE(IOUT,602)
          ENDDO
        ENDIF
        CALL UTLUCODE_DX_WRITE_SVD(IUSVD,NPE,NPT,IPTR,PARNAM,SVAL,SVEC)
      ELSE
        SVALNP = W
        SVECNP = V
        CALL UTLUCODE_SHELLSORT_VALUE(NPE,SVALNP)
        IPTRSVAL = 0
        USEDIPTR = 0
        DO I = 1,NPE
          DO J = 1,NPE
            IF(USEDIPTR(J) == 1) CYCLE
            IF(SVALNP(I) == W(J)) THEN
              IPTRSVAL(I) = J
              USEDIPTR(J) = 1
              EXIT
            ENDIF
          ENDDO
        ENDDO
        ! Determine if reordered
        REORDER = .FALSE.
        IF(IPTRSVAL(1) .NE. 1) REORDER = .TRUE.
        DO I = 2,NPE
          IF(IPTRSVAL(I) < IPTRSVAL(I-1)) REORDER = .TRUE.
        ENDDO
        DO I = 1,NPE
          DO J = 1,NPE
            SVECNP(I,J) = V(I,IPTRSVAL(J))
          ENDDO
        ENDDO
        IF(REORDER) THEN
          WRITE(IOUT,9999)
          WRITE(IOUT,9997)
          WRITE(IOUT,*)IPTRSVAL
          WRITE(IOUT,9999)
        ENDIF
        WRITE(IUSVD,9998)
        CALL UTLUCODE_DX_WRITE_SVD(IUSVD,NPE,NPT,IPTR,PARNAM,SVALNP,SVECNP)
      ENDIF
999 CONTINUE
    CLOSE(IUSVD)
    IF(SVDLOOP-1 == 1 .OR. (SVDLOOP == 1 .AND. SVDPHASE1)) THEN
      !write the svd-id file for assessment purposes to faciliate selection of SVDnumber
      OUTNAMTMP = TRIM(OUTNAM)//'._svd-id'
      IUSVDPL = UTL_GETUNIT(101,150)
      OPEN(UNIT=IUSVDPL,FILE=OUTNAMTMP,STATUS='REPLACE')
      CALL UTLUCODE_DX_WRITE_SVD_ID(IUSVDPL,1,NPE,NPT,IPTR,PARNAM,SVEC)
      CLOSE(UNIT=IUSVDPL)
    ENDIF
!
    IF(ALLOCATED(U)) DEALLOCATE(U)
    IF(ALLOCATED(V)) DEALLOCATE(V)
    IF(ALLOCATED(W)) DEALLOCATE(W)
    IF(ALLOCATED(RV1)) DEALLOCATE(RV1)
    IF(ALLOCATED(IPTRSVAL)) DEALLOCATE(IPTRSVAL)
    IF(ALLOCATED(USEDIPTR)) DEALLOCATE(USEDIPTR)
    GO TO 1001
!     .......... SET ERROR -- NO CONVERGENCE TO A
!                SINGULAR VALUE AFTER 30 ITERATIONS ..........
 1000 IERR = K
 1001 RETURN
  END SUBROUTINE UTLUCODE_SVD
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_SVDINV(IERR,IOUT,NM,M,N,A,MATU,MATV,AINV)
    !     CALCULATE INVERSE MATRIX OF FULL REAL RECTANGULAR MATRIX
    ! SOURCE CODE MODIFIED FROM http://www.netlib.org/fmm/svd.f
    !
    IMPLICIT NONE
    !
    ! Argument list variables
    INTEGER,                            INTENT(INOUT)  :: IERR
    INTEGER,                            INTENT(IN)     :: IOUT
    INTEGER,                            INTENT(IN)     :: NM
    INTEGER,                            INTENT(IN)     :: M
    INTEGER,                            INTENT(IN)     :: N
    DOUBLE PRECISION,                   INTENT(IN)     :: A(NM,N)
    LOGICAL,                            INTENT(IN)     :: MATU
    LOGICAL,                            INTENT(IN)     :: MATV
    DOUBLE PRECISION,                   INTENT(OUT)    :: AINV(NM,N)
!
    !
    !   Local variables
    INTEGER I,J,K,L,II,I1,KK,K1,LL,L1,MN,ITS
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: W
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: U
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: V
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: RV1
    DOUBLE PRECISION C,F,G,H,S,X,Y,Z,SCALE,ANORM
    DOUBLE PRECISION DSQRT,DMAX1,DABS,DSIGN
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)   :: ATMP

!
!     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE SVD,
!     NUM. MATH. 14, 403-420(1970) BY GOLUB AND REINSCH.
!     HANDBOOK FOR AUTO. COMP., VOL II-LINEAR ALGEBRA, 134-151(1971).
!
!     THIS SUBROUTINE DETERMINES THE SINGULAR VALUE DECOMPOSITION
!          T
!     A=USV  OF A REAL M BY N RECTANGULAR MATRIX.  HOUSEHOLDER
!     BIDIAGONALIZATION AND A VARIANT OF THE QR ALGORITHM ARE USED.
!
!     ON INPUT.
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.  NOTE THAT NM MUST BE AT LEAST
!          AS LARGE AS THE MAXIMUM OF M AND N.
!
!        M IS THE NUMBER OF ROWS OF A (AND U).
!
!        N IS THE NUMBER OF COLUMNS OF A (AND U) AND THE ORDER OF V.
!
!        A CONTAINS THE RECTANGULAR INPUT MATRIX TO BE DECOMPOSED.
!
!        MATU SHOULD BE SET TO .TRUE. IF THE U MATRIX IN THE
!          DECOMPOSITION IS DESIRED, AND TO .FALSE. OTHERWISE.
!
!        MATV SHOULD BE SET TO .TRUE. IF THE V MATRIX IN THE
!          DECOMPOSITION IS DESIRED, AND TO .FALSE. OTHERWISE.
!
!     ON OUTPUT.
!
!        A IS UNALTERED (UNLESS OVERWRITTEN BY U OR V).
!
!        W CONTAINS THE N (NON-NEGATIVE) SINGULAR VALUES OF A (THE
!          DIAGONAL ELEMENTS OF S).  THEY ARE UNORDERED.  IF AN
!          ERROR EXIT IS MADE, THE SINGULAR VALUES SHOULD BE CORRECT
!          FOR INDICES IERR+1,IERR+2,...,N.
!
!        U CONTAINS THE MATRIX U (ORTHOGONAL COLUMN VECTORS) OF THE
!          DECOMPOSITION IF MATU HAS BEEN SET TO .TRUE.  OTHERWISE
!          U IS USED AS A TEMPORARY ARRAY.  U MAY COINCIDE WITH A.
!          IF AN ERROR EXIT IS MADE, THE COLUMNS OF U CORRESPONDING
!          TO INDICES OF CORRECT SINGULAR VALUES SHOULD BE CORRECT.
!
!        V CONTAINS THE MATRIX V (ORTHOGONAL) OF THE DECOMPOSITION IF
!          MATV HAS BEEN SET TO .TRUE.  OTHERWISE V IS NOT REFERENCED.
!          V MAY ALSO COINCIDE WITH A IF U IS NOT NEEDED.  IF AN ERROR
!          EXIT IS MADE, THE COLUMNS OF V CORRESPONDING TO INDICES OF
!          CORRECT SINGULAR VALUES SHOULD BE CORRECT.
!
!        IERR IS SET TO
!          ZERO       FOR NORMAL RETURN,
!          K          IF THE K-TH SINGULAR VALUE HAS NOT BEEN
!                     DETERMINED AFTER 30 ITERATIONS.
!
!        RV1 IS A TEMPORARY STORAGE ARRAY.
!
!     THIS IS A MODIFIED VERSION OF A ROUTINE FROM THE EISPACK
!     COLLECTION BY THE NATS PROJECT
!
!     MODIFIED TO ELIMINATE MACHEP
!
 61 FORMAT(/,80('!'),' WARNING: INVERSION OF [AT]^-1 DID NOT CONVERGE',/, &
                     ' CALCULATION OF PROCESS MODEL PARAMETERS USING SVD',/, &
                     ' PARAMETERS MAY HAVE BEEN COMPROMISED',/,80('!'),/)
!
    ALLOCATE(U(NM,N))
    ALLOCATE(V(NM,N))
    ALLOCATE(ATMP(NM,N))
    ALLOCATE(W(N))
    ALLOCATE(RV1(N))
    IERR = 0
    U = 0.D0
    V = 0.D0
    W = 0.D0
    RV1 = 0.D0
    AINV = 0.D0
!
      DO 100 I = 1, M
!
         DO 100 J = 1, N
            U(I,J) = A(I,J)
  100 CONTINUE
!     .......... HOUSEHOLDER REDUCTION TO BIDIAGONAL FORM ..........
      G = 0.0D0
      SCALE = 0.0D0
      ANORM = 0.0D0
!
      DO 300 I = 1, N
         L = I + 1
         RV1(I) = SCALE * G
         G = 0.0D0
         S = 0.0D0
         SCALE = 0.0D0
         IF (I .GT. M) GO TO 210
!
         DO 120 K = I, M
  120    SCALE = SCALE + DABS(U(K,I))
!
         IF (SCALE .EQ. 0.0D0) GO TO 210
!
         DO 130 K = I, M
            U(K,I) = U(K,I) / SCALE
            S = S + U(K,I)**2
  130    CONTINUE
!
         F = U(I,I)
         G = -DSIGN(DSQRT(S),F)
         H = F * G - S
         U(I,I) = F - G
         IF (I .EQ. N) GO TO 190
!
         DO 150 J = L, N
            S = 0.0D0
!
            DO 140 K = I, M
  140       S = S + U(K,I) * U(K,J)
!
            F = S / H
!
            DO 150 K = I, M
               U(K,J) = U(K,J) + F * U(K,I)
  150    CONTINUE
!
  190    DO 200 K = I, M
  200    U(K,I) = SCALE * U(K,I)
!
  210    W(I) = SCALE * G
         G = 0.0D0
         S = 0.0D0
         SCALE = 0.0D0
         IF (I .GT. M .OR. I .EQ. N) GO TO 290
!
         DO 220 K = L, N
  220    SCALE = SCALE + DABS(U(I,K))
!
         IF (SCALE .EQ. 0.0D0) GO TO 290
!
         DO 230 K = L, N
            U(I,K) = U(I,K) / SCALE
            S = S + U(I,K)**2
  230    CONTINUE
!
         F = U(I,L)
         G = -DSIGN(DSQRT(S),F)
         H = F * G - S
         U(I,L) = F - G
!
         DO 240 K = L, N
  240    RV1(K) = U(I,K) / H
!
         IF (I .EQ. M) GO TO 270
!
         DO 260 J = L, M
            S = 0.0D0
!
            DO 250 K = L, N
  250       S = S + U(J,K) * U(I,K)
!
            DO 260 K = L, N
               U(J,K) = U(J,K) + S * RV1(K)
  260    CONTINUE
!
  270    DO 280 K = L, N
  280    U(I,K) = SCALE * U(I,K)
!
  290    ANORM = DMAX1(ANORM,DABS(W(I))+DABS(RV1(I)))
  300 CONTINUE
!     .......... ACCUMULATION OF RIGHT-HAND TRANSFORMATIONS ..........
      IF (.NOT. MATV) GO TO 410
!     .......... FOR I=N STEP -1 UNTIL 1 DO -- ..........
      DO 400 II = 1, N
         I = N + 1 - II
         IF (I .EQ. N) GO TO 390
         IF (G .EQ. 0.0D0) GO TO 360
!
         DO 320 J = L, N
!     .......... DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW ..........
  320    V(J,I) = (U(I,J) / U(I,L)) / G
!
         DO 350 J = L, N
            S = 0.0D0
!
            DO 340 K = L, N
  340       S = S + U(I,K) * V(K,J)
!
            DO 350 K = L, N
               V(K,J) = V(K,J) + S * V(K,I)
  350    CONTINUE
!
  360    DO 380 J = L, N
            V(I,J) = 0.0D0
            V(J,I) = 0.0D0
  380    CONTINUE
!
  390    V(I,I) = 1.0D0
         G = RV1(I)
         L = I
  400 CONTINUE
!     .......... ACCUMULATION OF LEFT-HAND TRANSFORMATIONS ..........
  410 IF (.NOT. MATU) GO TO 510
!     ..........FOR I=MIN(M,N) STEP -1 UNTIL 1 DO -- ..........
      MN = N
      IF (M .LT. N) MN = M
!
      DO 500 II = 1, MN
         I = MN + 1 - II
         L = I + 1
         G = W(I)
         IF (I .EQ. N) GO TO 430
!
         DO 420 J = L, N
  420    U(I,J) = 0.0D0
!
  430    IF (G .EQ. 0.0D0) GO TO 475
         IF (I .EQ. MN) GO TO 460
!
         DO 450 J = L, N
            S = 0.0D0
!
            DO 440 K = L, M
  440       S = S + U(K,I) * U(K,J)
!     .......... DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW ..........
            F = (S / U(I,I)) / G
!
            DO 450 K = I, M
               U(K,J) = U(K,J) + F * U(K,I)
  450    CONTINUE
!
  460    DO 470 J = I, M
  470    U(J,I) = U(J,I) / G
!
         GO TO 490
!
  475    DO 480 J = I, M
  480    U(J,I) = 0.0D0
!
  490    U(I,I) = U(I,I) + 1.0D0
  500 CONTINUE
!     .......... DIAGONALIZATION OF THE BIDIAGONAL FORM ..........
!     .......... FOR K=N STEP -1 UNTIL 1 DO -- ..........
  510 DO 700 KK = 1, N
         K1 = N - KK
         K = K1 + 1
         ITS = 0
!     .......... TEST FOR SPLITTING.
!                FOR L=K STEP -1 UNTIL 1 DO -- ..........
  520    DO 530 LL = 1, K
            L1 = K - LL
            L = L1 + 1
            IF (DABS(RV1(L)) + ANORM .EQ. ANORM) GO TO 565
!     .......... RV1(1) IS ALWAYS ZERO, SO THERE IS NO EXIT
!                THROUGH THE BOTTOM OF THE LOOP ..........
            IF (DABS(W(L1)) + ANORM .EQ. ANORM) GO TO 540
  530    CONTINUE
!     .......... CANCELLATION OF RV1(L) IF L GREATER THAN 1 ..........
  540    C = 0.0D0
         S = 1.0D0
!
         DO 560 I = L, K
            F = S * RV1(I)
            RV1(I) = C * RV1(I)
            IF (DABS(F) + ANORM .EQ. ANORM) GO TO 565
            G = W(I)
            H = DSQRT(F*F+G*G)
            W(I) = H
            C = G / H
            S = -F / H
            IF (.NOT. MATU) GO TO 560
!
            DO 550 J = 1, M
               Y = U(J,L1)
               Z = U(J,I)
               U(J,L1) = Y * C + Z * S
               U(J,I) = -Y * S + Z * C
  550       CONTINUE
!
  560    CONTINUE
!     .......... TEST FOR CONVERGENCE ..........
  565    Z = W(K)
         IF (L .EQ. K) GO TO 650
!     .......... SHIFT FROM BOTTOM 2 BY 2 MINOR ..........
         IF (ITS .EQ. 30) GO TO 1000
         ITS = ITS + 1
         X = W(L)
         Y = W(K1)
         G = RV1(K1)
         H = RV1(K)
         F = ((Y - Z) * (Y + Z) + (G - H) * (G + H)) / (2.0D0 * H * Y)
         G = DSQRT(F*F+1.0D0)
         F = ((X - Z) * (X + Z) + H * (Y / (F + DSIGN(G,F)) - H)) / X
!     .......... NEXT QR TRANSFORMATION ..........
         C = 1.0D0
         S = 1.0D0
!
         DO 600 I1 = L, K1
            I = I1 + 1
            G = RV1(I)
            Y = W(I)
            H = S * G
            G = C * G
            Z = DSQRT(F*F+H*H)
            RV1(I1) = Z
            C = F / Z
            S = H / Z
            F = X * C + G * S
            G = -X * S + G * C
            H = Y * S
            Y = Y * C
            IF (.NOT. MATV) GO TO 575
!
            DO 570 J = 1, N
               X = V(J,I1)
               Z = V(J,I)
               V(J,I1) = X * C + Z * S
               V(J,I) = -X * S + Z * C
  570       CONTINUE
!
  575       Z = DSQRT(F*F+H*H)
            W(I1) = Z
!     .......... ROTATION CAN BE ARBITRARY IF Z IS ZERO ..........
            IF (Z .EQ. 0.0D0) GO TO 580
            C = F / Z
            S = H / Z
  580       F = C * G + S * Y
            X = -S * G + C * Y
            IF (.NOT. MATU) GO TO 600
!
            DO 590 J = 1, M
               Y = U(J,I1)
               Z = U(J,I)
               U(J,I1) = Y * C + Z * S
               U(J,I) = -Y * S + Z * C
  590       CONTINUE
!
  600    CONTINUE
!
         RV1(L) = 0.0D0
         RV1(K) = F
         W(K) = X
         GO TO 520
!     .......... CONVERGENCE ..........
  650    IF (Z .GE. 0.0D0) GO TO 700
!     .......... W(K) IS MADE NON-NEGATIVE ..........
         W(K) = -Z
         IF (.NOT. MATV) GO TO 700
!
         DO 690 J = 1, N
  690    V(J,K) = -V(J,K)
!
  700 CONTINUE

!  [A]^-1 = V W^-1 UT
!  invert W and multiply V W^-1
    DO I=1,NM
      W(I) = 1.D0/W(I)
      DO J=1,NM
        ATMP(J,I) = V(J,I) * W(I)
      ENDDO
    ENDDO
    AINV = MATMUL(ATMP,TRANSPOSE(U))
      IF(ALLOCATED(U)) DEALLOCATE(U)
      IF(ALLOCATED(V)) DEALLOCATE(V)
      IF(ALLOCATED(W)) DEALLOCATE(W)
      IF(ALLOCATED(RV1)) DEALLOCATE(RV1)
!
      GO TO 1001
!     .......... SET ERROR -- NO CONVERGENCE TO A
!                SINGULAR VALUE AFTER 30 ITERATIONS ..........
 1000 IERR = K
      WRITE(IOUT,61)
 1001 RETURN
  END SUBROUTINE UTLUCODE_SVDINV
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_SVD_PARCHK(NPS,IOUT,SVDPMPFN,BSCAL,PALN,PANAM,PAPINCR, &
           PARGP,PERTURB,PMAXCHANGE,PTOLPAR,PVALMAX,PVALMIN,PAPVAL,SVDSETORIG)
    !     COMPARE _PAOPT DX FILE Parameter INFORMATION TO PARAMETER_DATA BLOCK
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                           INTENT(IN)    :: NPS
    INTEGER,                           INTENT(IN)    :: IOUT
    CHARACTER(LEN=MAX_STRING_LEN),     INTENT(IN)    :: SVDPMPFN
    DOUBLE PRECISION,                  INTENT(IN)    :: BSCAL(NPS)
    INTEGER,                           INTENT(IN)    :: PALN(NPS)
    CHARACTER(LEN=12),                 INTENT(IN)    :: PANAM(NPS)
    LOGICAL,                           INTENT(IN)    :: PAPINCR(NPS)
    CHARACTER(LEN=12),                 INTENT(IN)    :: PARGP(NPS)
    DOUBLE PRECISION,                  INTENT(IN)    :: PERTURB(NPS)
    DOUBLE PRECISION,                  INTENT(IN)    :: PMAXCHANGE(NPS)
    DOUBLE PRECISION,                  INTENT(IN)    :: PTOLPAR(NPS)
    DOUBLE PRECISION,                  INTENT(IN)    :: PVALMAX(NPS)
    DOUBLE PRECISION,                  INTENT(IN)    :: PVALMIN(NPS)
    DOUBLE PRECISION,                  INTENT(INOUT) :: PAPVAL(NPS)
    INTEGER,                           INTENT(IN)    :: SVDSETORIG(NPS)
    !
    !   Local variables
    INTEGER                                      :: I
    INTEGER                                      :: IFAIL = 0
    INTEGER                                      :: IUPA
    LOGICAL                                      :: LEX
    INTEGER                                      :: N
    LOGICAL                                      :: NOMATCH = .FALSE.
    LOGICAL, ALLOCATABLE, DIMENSION(:)           :: PADJTMP
    INTEGER, ALLOCATABLE, DIMENSION(:)           :: PALNTMP
    CHARACTER(LEN=12)                            :: PANAMEXTRA = ' '
    CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:) :: PANAMTMP
    INTEGER, ALLOCATABLE, DIMENSION(:)           :: PAPINCRTMP
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)  :: PAPVALTMP
    ! Formats
    30 FORMAT(/,80('-'))
    31 FORMAT(1X,A)
    32 FORMAT(1X,2A)
    900 FORMAT( &
    ' OPTIMIZATION OF SVD PARAMETERS BASED ON A PREVIOUS PARAMETER',/, &
    ' COVARAIANCE MATRIX WAS REQUESTED.  HOWEVER, THE PARAMETER_DATA BLOCK',/, &
    ' DIFFERS FROM THE RUN THAT PRODUCED THAT ._paopt FILE.')
    901 FORMAT( &
    ' # PARAMETERS in PARAMETER_DATA BLOCK, CHECK VERSION/RE-CREATE _paopt')
    902 FORMAT(/, &
    ' EITHER CREATE A NEW PARAMETER COVARAIANCE MATRIX FOR THE NEW',/, &
    ' PARAMETER_DATA BLOCK BY RUNNING UCODE WITH SVDPHASE=new;',/, &
    ' OR USE THE ORIGINAL PARAMETER_DATA BLOCK CONSISTENT WITH THE',/, &
    ' CURRENT DX FILES.')
    903 FORMAT(//,80('!'),/,80('!'),/)
    904 FORMAT( &
    ' OPTIMIZATION OF SVD PARAMETERS BASED ON A PREVIOUS PARAMETER',/, &
    ' COVARAIANCE MATRIX WAS REQUESTED.',/, &
    ' HOWEVER, THE FILE NEEDED FOR THE SPECIFIED SVDphase/SVDstartpars' &
    ' COMBINATION', &
    '   ',A,/, &
    ' IS NOT PRESENT IN THE WORKING DIRECTORY. ASSUMING A REGRESSION USING',/, &
    ' SVD IS DESIRED EITHER RUN A PROCESS-MODEL SENSITIVITY ANALYSIS FIRST',/, &
    ' OR RESTART THIS RUN WITH SVDPHASE=new/SVDstartpars=Parameter_Data.',/)
    !!READ PAOPT
    INQUIRE(FILE=TRIM(SVDPMPFN),EXIST=LEX)
    IF(LEX) THEN
      LEX = .FALSE.
      IUPA = UTL_GETUNIT(101,150)
      OPEN(UNIT=IUPA,FILE=SVDPMPFN,STATUS='OLD')
    ELSE
      WRITE(*,903)
      WRITE(IOUT,903)
      WRITE(IOUT,904)TRIM(SVDPMPFN)
      WRITE(*,904)TRIM(SVDPMPFN)
      WRITE(*,903)
      WRITE(IOUT,903)
      CALL UTL_STOP
    ENDIF
    ALLOCATE (PAPINCRTMP(NPS),PALNTMP(NPS),PANAMTMP(NPS),PAPVALTMP(NPS), &
              PADJTMP(NPS))
    PADJTMP = .FALSE.
    ! READ Header
    READ ( IUPA,*,END=100)
    WRITE(IOUT,30)
    WRITE(IOUT,31)'PARAMETER INFORMATION FROM PREVIOUS EVALUATION:'
    ! Read requested values
    N = NPS
    DO I=1,N
      READ (IUPA,*,END=100)PANAMTMP(I),PAPVALTMP(I),PALNTMP(I),PAPINCRTMP(I)
      IF(PAPINCRTMP(I) > 0)PADJTMP(I) = .TRUE.
    ENDDO
    READ (IUPA,*,END=200)PANAMEXTRA
    IF(PANAMEXTRA .NE. ' ') GO TO 300
    200 CLOSE(IUPA)
    DO I=1,NPS
      IF(PALN(I) .NE. PALNTMP(I)) NOMATCH = .TRUE.
      IF(PANAM(I) .NE. PANAMTMP(I)) NOMATCH = .TRUE.
      IF((PAPINCR(I) .AND. PAPINCRTMP(I)==0) .OR. &
         (.NOT. PAPINCR(I) .AND. PAPINCRTMP(I)==1)) NOMATCH = .TRUE.
      IF(NOMATCH)EXIT
    ENDDO
    IF(NOMATCH)THEN
      ! mismatched info in paopt and parameter data block
      WRITE(*,903)
      WRITE(IOUT,903)
      WRITE(IOUT,900)
      WRITE(*,900)
      WRITE(*,902)
      WRITE(IOUT,902)
      WRITE(*,903)
      WRITE(IOUT,903)
      CALL UTL_STOP
    ELSE
      ! use optimal parameter values
      PAPVAL = PAPVALTMP
    ENDIF
    WRITE(IOUT,30)
    WRITE(IOUT,32) &
      'PARAMETER INFORMATION READ FROM the MAIN UCODE INPUT and:',TRIM(SVDPMPFN)
    CALL UTLUCODE_WRITEPARS(IFAIL,IOUT,NPS,BSCAL,PALNTMP,PADJTMP,PARGP, &
       PANAMTMP,PERTURB,PMAXCHANGE,PTOLPAR,PAPVALTMP,PVALMAX,PVALMIN,SVDSETORIG)
    DEALLOCATE(PALNTMP,PANAMTMP,PAPINCRTMP,PAPVALTMP)
    !
    RETURN
    ! less info in paopt than parameter data block
    100 CLOSE(IUPA)
    AMESSAGE= &
    'UNEXPECTED CONTENT _paopt file, # PARAMETERS in _paopt is LESS THAN '
    WRITE(*,903)
    WRITE(IOUT,903)
    WRITE(*,*)TRIM(AMESSAGE)
    WRITE(IOUT,*)TRIM(AMESSAGE)
    WRITE(*,901)
    WRITE(IOUT,901)
    WRITE(*,902)
    WRITE(IOUT,902)
    WRITE(*,903)
    WRITE(IOUT,903)
    CALL UTL_STOP
    RETURN
    ! more info in paopt than parameter data block
    300 CLOSE(IUPA)
    AMESSAGE= &
    'UNEXPECTED CONTENT _paopt file, # PARAMETERS in _paopt is GREATER THAN '
    WRITE(*,903)
    WRITE(IOUT,903)
    WRITE(*,*)TRIM(AMESSAGE)
    WRITE(IOUT,*)TRIM(AMESSAGE)
    WRITE(*,901)
    WRITE(IOUT,901)
    WRITE(*,902)
    WRITE(IOUT,902)
    WRITE(*,903)
    WRITE(IOUT,903)
    CALL UTL_STOP
    RETURN
  END SUBROUTINE UTLUCODE_SVD_PARCHK
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_INVERT(IFAIL,A,NCA,AI,IOUT,DTLA,RPTINVERR)
    !   This routine inverts A to AI using compressed matrix format
    !   and calculates the DETERMINANT of the INVERTED MATRIX
    USE DATATYPES
    ! Argument list variables
    IMPLICIT NONE
    INTEGER, INTENT(INOUT) :: IFAIL
    INTEGER, INTENT(IN) :: NCA
    INTEGER, INTENT(IN) :: IOUT
    DOUBLE PRECISION, INTENT(INOUT) :: A(NCA,NCA)
    DOUBLE PRECISION, INTENT(OUT) :: AI(NCA,NCA)
    DOUBLE PRECISION, INTENT(OUT) :: DTLA
    LOGICAL, OPTIONAL,   INTENT(IN) :: RPTINVERR
    ! Local variables
    INTEGER         :: I, IP, IP1
    DOUBLE PRECISION  :: DTMPA
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: SCLE
    LOGICAL :: REPORT
    TYPE (CDMATRIX) :: CA, CSQRAI
    ! Initialize
    CALL TYP_NULL(CA)
    CALL TYP_NULL(CSQRAI)
    AMESSAGE = ' '
    IFAIL = 0
    REPORT = .TRUE.
    IF(PRESENT(RPTINVERR)) REPORT = RPTINVERR

    ALLOCATE(SCLE(NCA))
    ! BUILD SCALING VECTOR BASED ON DIAGONALS OF MATRIX
    DO IP = 1, NCA
      SCLE(IP) = 1.D0
      IF (A(IP,IP) > 1.D-30) SCLE(IP) = DSQRT(A(IP,IP))
      A(IP,IP) = 1.D0
    ENDDO
    ! SCALE MATRIX USING DIAGONAL VALUES
    DO IP = 1, NCA-1
      DTMPA = SCLE(IP)
      IP1 = IP + 1
      DO I = IP1, NCA
        A(I,IP) = A(I,IP)/(SCLE(I)*DTMPA)
        A(IP,I) = A(I,IP)
      ENDDO
    ENDDO

    !   Store matrix A in CDMATRIX structure CA
    CALL UTL_ARR2CDMATRIX(NCA,NCA,A,CA)
    !
    !   Call UTL_SVD to invert CA.  Resulting inverted matrix is returned in
    !   CA, and the square-root of the inverse is returned in CSQRAI.
    CSQRAI%ARRAYNAME = 'SQRT_AI'
    CALL UTL_SVD(IFAIL,CA,CSQRAI,DTLA)
    IF (REPORT .AND. IFAIL > 0) THEN
      AMESSAGE = 'Warning from UTLUCODE_INVERT, cannot take ln(-x)'
      CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','no')
      AMESSAGE = 'Determinant is set to 1.E+30'
      CALL UTL_WRITE_MESSAGE(IOUT,'no','no','no')
      AMESSAGE = 'This PREVENTS CALCULATION of the KASHYAP STATISTIC'
      CALL UTL_WRITE_MESSAGE(IOUT,'no','no','yes')
      DTLA = 1.0D+30
      IFAIL = 0
    ENDIF
    !   The following statement takes advantage of overloading of assignment
    !   (=) operator by invoking INTERFACE ASSIGNMENT in UTILITIES module.
    AI = CA
    ! UNSCALE ORIGINAL MATRIX AND INVERSE TO OBTAIN CORRECT VALUES
    DO IP = 1, NCA
      DTMPA = SCLE(IP)
      DO I = IP, NCA
        A(I,IP) = A(I,IP)*(SCLE(I)*DTMPA)
        AI(I,IP) = AI(I,IP)/(SCLE(I)*DTMPA)
        A(IP,I) = A(I,IP)
        AI(IP,I) = AI(I,IP)
      ENDDO
    ENDDO
    !
    ! CA holds the inverted matrix, so calculate its log determinant
    ! UNSCALE DETERMINANT
    DTMPA=1.D0
    DO IP = 1, NCA
      DTMPA = DTMPA*SCLE(IP)**2
    ENDDO
    DTLA = -(DLOG(DTMPA)+DTLA)
    !
    !   Deallocate memory used by array components of CDMATRIX structures
    CALL TYP_DEALLOC(CA)
    CALL TYP_DEALLOC(CSQRAI)
    DEALLOCATE(SCLE)
    !
    RETURN
  END SUBROUTINE UTLUCODE_INVERT
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_SHELLSORTMANY(N,X,A,B,C,D,E,F,G,H,P,Q)
    !    Algorithm based on one obtained from
    !    http://www.nist.gov/dads/HTML/shellsort.html
    !    Thank you NIST!
    !  ALGORITHM AS 304.8 APPL.STATIST. (1996), VOL.45, NO.3
    !  Sorts the N values stored in array X in ascending order
    !  Based on an "adaptation by Marlene Metzner" this algorithm is referred to
    !  as the Shell-Metzner sort by John P. Grillo, A Comparison of Sorts,
    !  Creative Computing, 2:76-80, Nov/Dec 1976. Grillo cites Fredric Stuart,
    !  FORTRAN Programming, John Wiley and Sons, New York, 1969, page 294. In
    !  crediting "one of the fastest" programs for sorting, Stuart says in a
    !  footnote, "Published by Marlene Metzner, Pratt & Whitney Aircraft
    !  Company. From a method described by D. L. Shell."
    !
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                        INTENT(IN)    ::  N
    CHARACTER(LEN=*), DIMENSION(N), INTENT(INOUT) ::  X
    DOUBLE PRECISION, DIMENSION(N), INTENT(INOUT) ::  A
    DOUBLE PRECISION, DIMENSION(N), INTENT(INOUT) ::  B
    DOUBLE PRECISION, DIMENSION(N), INTENT(INOUT) ::  C
    DOUBLE PRECISION, DIMENSION(N), INTENT(INOUT) ::  D
    DOUBLE PRECISION, DIMENSION(N), INTENT(INOUT) ::  E
    CHARACTER(LEN=*), DIMENSION(N), INTENT(INOUT) ::  F
    CHARACTER(LEN=*), DIMENSION(N), INTENT(INOUT) ::  G
    INTEGER, DIMENSION(N), INTENT(INOUT)          ::  H
    INTEGER, DIMENSION(N), INTENT(INOUT)          ::  P
    CHARACTER(LEN=*), DIMENSION(N), INTENT(INOUT) ::  Q
    !
    ! Local variables
    CHARACTER(LEN=MAX_STRING_LEN)   ::  TEMP
    DOUBLE PRECISION                ::  TEMPA
    DOUBLE PRECISION                ::  TEMPB
    DOUBLE PRECISION                ::  TEMPC
    DOUBLE PRECISION                ::  TEMPD
    DOUBLE PRECISION                ::  TEMPE
    CHARACTER(LEN=MAX_STRING_LEN)   ::  TEMPF
    CHARACTER(LEN=MAX_STRING_LEN)   ::  TEMPG
    INTEGER                         ::  TEMPH
    INTEGER                         ::  TEMPP
    CHARACTER(LEN=MAX_STRING_LEN)   ::  TEMPQ
    INTEGER                         ::  I
    INTEGER                         ::  J
    INTEGER                         ::  INCR = 1
    !
    !    Loop : calculate the increment
    !
    10 INCR = 3 * INCR + 1
    IF (INCR .LE. N) GOTO 10
    !
    !    Loop : Shell-Metzner sort
    !
    20 INCR = INCR / 3
    I = INCR + 1
    30 IF (I .GT. N) GOTO 60
    TEMP = X(I)
    TEMPA = A(I)
    TEMPB = B(I)
    TEMPC = C(I)
    TEMPD = D(I)
    TEMPE = E(I)
    TEMPF = F(I)
    TEMPG = G(I)
    TEMPH = H(I)
    TEMPP = P(I)
    TEMPQ = Q(I)
    J = I
    40 IF (X(J - INCR) .LT. TEMP) GOTO 50
    X(J) = X(J - INCR)
    A(J) = A(J - INCR)
    B(J) = B(J - INCR)
    C(J) = C(J - INCR)
    D(J) = D(J - INCR)
    E(J) = E(J - INCR)
    F(J) = F(J - INCR)
    G(J) = G(J - INCR)
    H(J) = H(J - INCR)
    P(J) = P(J - INCR)
    Q(J) = Q(J - INCR)
    J = J - INCR
    IF (J .GT. INCR) GOTO 40
    50 X(J) = TEMP
    A(J) = TEMPA
    B(J) = TEMPB
    C(J) = TEMPC
    D(J) = TEMPD
    E(J) = TEMPE
    F(J) = TEMPF
    G(J) = TEMPG
    H(J) = TEMPH
    P(J) = TEMPP
    Q(J) = TEMPQ
    I = I + 1
    GOTO 30
    60 IF (INCR .GT. 1) GOTO 20
    !
    RETURN
  END SUBROUTINE UTLUCODE_SHELLSORTMANY
!===============================================================================
!===============================================================================
  SUBROUTINE UTLUCODE_SHELLSORTTHREE(N,X,A,B)
    !    Algorithm based on one obtained from
    !    http://www.nist.gov/dads/HTML/shellsort.html
    !    Thank you NIST!
    !  ALGORITHM AS 304.8 APPL.STATIST. (1996), VOL.45, NO.3
    !  Sorts the N values stored in array X in ascending order
    !  Based on an "adaptation by Marlene Metzner" this algorithm is referred to
    !  as the Shell-Metzner sort by John P. Grillo, A Comparison of Sorts,
    !  Creative Computing, 2:76-80, Nov/Dec 1976. Grillo cites Fredric Stuart,
    !  FORTRAN Programming, John Wiley and Sons, New York, 1969, page 294. In
    !  crediting "one of the fastest" programs for sorting, Stuart says in a
    !  footnote, "Published by Marlene Metzner, Pratt & Whitney Aircraft
    !  Company. From a method described by D. L. Shell."
    !
!    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                        INTENT(IN)    ::  N
    DOUBLE PRECISION, DIMENSION(N), INTENT(INOUT) ::  X
    CHARACTER(12), DIMENSION(N), INTENT(INOUT) ::  A
    INTEGER, DIMENSION(N), INTENT(INOUT) ::  B
    !
    ! Local variables
!    CHARACTER(LEN=MAX_STRING_LEN)   ::  TEMP
    DOUBLE PRECISION                ::  TEMP
    CHARACTER(12)                ::  TEMPA
    INTEGER                         ::  TEMPB
    INTEGER                         ::  I
    INTEGER                         ::  J
    INTEGER                         ::  INCR = 1

    !
    !    Loop : calculate the increment
    !
    10 INCR = 3 * INCR + 1
    IF (INCR .LE. N) GOTO 10
    !
    !    Loop : Shell-Metzner sort
    !
    20 INCR = INCR / 3
    I = INCR + 1
    30 IF (I .GT. N) GOTO 60
    TEMP = X(I)
    TEMPA = A(I)
    TEMPB = B(I)
    J = I
    40 IF (X(J - INCR) .LT. TEMP) GOTO 50
    X(J) = X(J - INCR)
    A(J) = A(J - INCR)
    B(J) = B(J - INCR)
    J = J - INCR
    IF (J .GT. INCR) GOTO 40
    50 X(J) = TEMP
    A(J) = TEMPA
    B(J) = TEMPB
    I = I + 1
    GOTO 30
    60 IF (INCR .GT. 1) GOTO 20
    !
    RETURN
  END SUBROUTINE UTLUCODE_SHELLSORTTHREE
!===============================================================================
!===============================================================================
END MODULE UTLUCODE
!===============================================================================
!===============================================================================
