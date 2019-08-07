MODULE REG_GNMOD
  !
  USE DATATYPES
  !
  USE GLOBAL_DATA
  !
  USE PRIOR_INFORMATION, ONLY: PRI_UEV_RESIDUALS, PRIEQN
  !
  USE STATISTICS, ONLY: STA_EVA_COMMENTS, STA_EVA_PROB_NORM_DISTRB
  !
  USE UTLUCODE
  !
  USE UTILITIES
  !
  IMPLICIT NONE
  ! Public subprograms
  PUBLIC REG_GNMOD_INI1, REG_GNMOD_INI2, REG_GNMOD_GEN, &
      REG_GNMOD_GEN_QUASINEWTON_C, &
      REG_GNMOD_UNCINI, REG_GNMOD_UNC, &
      REG_GNMOD_EVA_FINAL_OUT, REG_GNMOD_EVA_FINISH, &
      REG_GNMOD_EVA_MLOFSTATS, REG_GNMOD_EVA_SUMMARY, &
      REG_GNMOD_EVA_ORD_NORM_RESIDS, REG_GNMOD_CLN, &
  ! variables
  AAP, AMCA, AMPA, BNDCYCLE, CMAT, DD, DDV, DDVC, DMXA, GG, GGQN, IUWRP, &
  NPAREST, NPARSTAR, NPDAMP, NPERD, NPERDSEN, NPMAXCHG, NPTR, &
  PINCBND, PINCSEN, PSTATUS, RADCHG, SCLE, STEPUSED, TRAD, &
  WTADJ, WTADJF, WTADJFINAL, &
  WTADJNEXT, XPRIRD, XPTR
  !
  ! REG_GNMOD MODULE VARIABLES
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: AAP
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: AMCA
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: AMPA
  INTEGER                                        :: BNDCYCLE = 1
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: CMAT
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: DD
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: DDV
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: DDVC
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: DMXA
  INTEGER, ALLOCATABLE, DIMENSION(:)             :: ELIMINATED
  DOUBLE PRECISION                               :: ERO = 0.D0
  LOGICAL                                        :: FIRST = .TRUE.
  DOUBLE PRECISION                               :: FL0 = 0.D0
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: GD
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: GG
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: GGQN
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:) :: INITXSEN
  DOUBLE PRECISION, ALLOCATABLE,  DIMENSION(:,:) :: INITXPRI
  INTEGER                                        :: IUWRP
  INTEGER                                        :: NIM
  INTEGER, ALLOCATABLE, DIMENSION(:)             :: NPAREST
  CHARACTER(LEN=1), ALLOCATABLE, DIMENSION(:)    :: NPARSTAR
  INTEGER, ALLOCATABLE, DIMENSION(:)             :: NPDAMP
  INTEGER                                        :: NPERD
  INTEGER                                        :: NPERDSEN
  INTEGER                                        :: NPERDSTART
  INTEGER, ALLOCATABLE, DIMENSION(:)             :: NPMAXCHG
  INTEGER, ALLOCATABLE, DIMENSION(:)             :: NPTR
  INTEGER, ALLOCATABLE, DIMENSION(:)             :: PINCBND
  INTEGER, ALLOCATABLE, DIMENSION(:)             :: PINCSEN
  CHARACTER(LEN=1), ALLOCATABLE, DIMENSION(:)    :: PSTATUS
  CHARACTER(LEN=9), ALLOCATABLE, DIMENSION(:)    :: RADCHG
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: RD
  LOGICAL                                        :: RECALCDDV = .TRUE.
  LOGICAL                                        :: REDAMP = .FALSE.
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: SCLE
  INTEGER                                        :: SSEITERP
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: SSELIST
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: SSELISTPREDLIM
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: SSEPVALLIST
  DOUBLE PRECISION                               :: SSECLOSEST
  DOUBLE PRECISION                               :: SSECLOSESTPREDLIM
  DOUBLE PRECISION                               :: SSEDIFF
  CHARACTER(LEN=3), ALLOCATABLE, DIMENSION(:)    :: STEPUSED
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: TRAD
  LOGICAL                                        :: UPDATEQN = .FALSE.
  LOGICAL                                        :: UPDATE_FINAL_SENS = .FALSE.
  LOGICAL, ALLOCATABLE, DIMENSION(:)             :: WTADJ
  LOGICAL, ALLOCATABLE, DIMENSION(:)             :: WTADJF
  LOGICAL                                        :: WTADJFINAL = .FALSE.
  LOGICAL                                        :: WTADJNEXT = .FALSE.
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: XD
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: XPRIRD
  INTEGER, ALLOCATABLE, DIMENSION(:)             :: XPTR
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: XW
  SAVE
  PRIVATE
  CONTAINS
!===============================================================================
!===============================================================================
  SUBROUTINE REG_GNMOD_INI1 (MAXITER,MPR,NPE)
    IMPLICIT NONE
    INTEGER,                         INTENT(IN)    :: MAXITER
    INTEGER,                         INTENT(IN)    :: MPR
    INTEGER,                         INTENT(IN)    :: NPE
    IF (ALLOCATED(AAP)) DEALLOCATE(AAP)
    IF (ALLOCATED(AMCA)) DEALLOCATE(AMCA)
    IF (ALLOCATED(AMPA)) DEALLOCATE(AMPA)
    IF (ALLOCATED(DMXA)) DEALLOCATE(DMXA)
    IF (ALLOCATED(NPAREST)) DEALLOCATE(NPAREST)
    IF (ALLOCATED(NPARSTAR)) DEALLOCATE(NPARSTAR)
    IF (ALLOCATED(NPDAMP)) DEALLOCATE(NPDAMP)
    IF (ALLOCATED(NPMAXCHG)) DEALLOCATE(NPMAXCHG)
    IF (ALLOCATED(RADCHG)) DEALLOCATE(RADCHG)
    IF (ALLOCATED(STEPUSED)) DEALLOCATE(STEPUSED)
    IF (ALLOCATED(TRAD)) DEALLOCATE(TRAD)
    ALLOCATE(AAP(MAXITER),AMCA(MAXITER),AMPA(MAXITER),DMXA(MAXITER+1), &
             NPAREST(MAXITER+1), NPARSTAR(MAXITER+2), &
             NPDAMP(MAXITER+1), NPMAXCHG(MAXITER+1), &
             STEPUSED(MAXITER),RADCHG(MAXITER), TRAD(MAXITER))
    AAP = 0.D0
    AMCA = 0.D0
    AMPA = 0.D0
    DMXA = 0.D0
    NPAREST = 0
    NPARSTAR = ' '
    NPDAMP = 0
    NPMAXCHG = 0
    RADCHG = ' '
    STEPUSED = ' '
    TRAD = 0.D0
    IF (ALLOCATED(ELIMINATED)) DEALLOCATE(ELIMINATED)
    IF (ALLOCATED(PINCBND)) DEALLOCATE(PINCBND)
    IF (ALLOCATED(PINCSEN)) DEALLOCATE(PINCSEN)
    IF (ALLOCATED(PSTATUS)) DEALLOCATE(PSTATUS)
    ALLOCATE(ELIMINATED(NPE),PINCBND(NPE),PINCSEN(NPE),PSTATUS(NPE))
    ELIMINATED = 0
    PINCBND = 0
    PINCSEN = 0
    PSTATUS = ' '
    IF (ALLOCATED(CMAT)) DEALLOCATE(CMAT)
    IF (ALLOCATED(DD)) DEALLOCATE(DD)
    IF (ALLOCATED(DDV)) DEALLOCATE(DDV)
    IF (ALLOCATED(DDVC)) DEALLOCATE(DDVC)
    IF (ALLOCATED(GD)) DEALLOCATE(GD)
    IF (ALLOCATED(GG)) DEALLOCATE(GG)
    IF (ALLOCATED(GGQN)) DEALLOCATE(GGQN)
    IF (ALLOCATED(RD)) DEALLOCATE(RD)
    IF (ALLOCATED(SCLE)) DEALLOCATE(SCLE)
    IF (ALLOCATED(WTADJ)) DEALLOCATE(WTADJ)
    IF (ALLOCATED(WTADJF)) DEALLOCATE(WTADJF)
    IF(ALLOCATED(XPRIRD)) DEALLOCATE(XPRIRD)
    ALLOCATE(CMAT(NPE,NPE),DD(NPE),DDV(NPE),DDVC(NPE), &
             GD(NPE),GG(NPE),GGQN(NPE),RD(NPE,NPE), &
             SCLE(NPE),WTADJ(NPE),WTADJF(NPE),XPRIRD(NPE,MPR))
    CMAT = 0.D0
    DD = 0.D0
    DDV = 0.D0
    DDVC = 0.D0
    GD = 0.D0
    GG = 0.D0
    GGQN = 0.D0
    RD = 0.D0
    SCLE = 0.D0
    WTADJ = .FALSE.
    WTADJF = .FALSE.
    XPRIRD = 0.D0
    RETURN
  END SUBROUTINE REG_GNMOD_INI1
!===============================================================================
!===============================================================================
  SUBROUTINE REG_GNMOD_INI2 (IOUT,MPR,NOBS,NPE,NPS,CSS,IOMIT,IPTR,MINSENRAT, &
                           OMIT_INSENS,PADJ,PARNAM,REINCSENRAT,XPRI,XSENST)
    IMPLICIT NONE
    INTEGER,                         INTENT(IN)    :: IOUT
    INTEGER,                         INTENT(IN)    :: MPR
    INTEGER,                         INTENT(IN)    :: NOBS
    INTEGER,                         INTENT(IN)    :: NPE
    INTEGER,                         INTENT(IN)    :: NPS
    DOUBLE PRECISION,                INTENT(IN)    :: CSS(NPE)
    INTEGER,                         INTENT(IN)    :: IOMIT
    INTEGER,                         INTENT(IN)    :: IPTR(NPE)
    DOUBLE PRECISION,                INTENT(IN)    :: MINSENRAT
    LOGICAL,                         INTENT(IN)    :: OMIT_INSENS
    LOGICAL,                         INTENT(IN)    :: PADJ(NPS)
    CHARACTER(LEN=12),               INTENT(IN)    :: PARNAM(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: REINCSENRAT
    DOUBLE PRECISION,                INTENT(IN)    :: XPRI(NPE,MPR)
    DOUBLE PRECISION,                INTENT(INOUT) :: XSENST(NPE,NOBS)
    ! local variables
    DOUBLE PRECISION                               :: CSSMAX
    INTEGER                                        :: I
    INTEGER                                        :: J
    INTEGER                                        :: K
    INTEGER                                        :: N
    DOUBLE PRECISION                               :: RATIO
    ! formats
    100 FORMAT(/,1X, &
        'OMITTED/REINCLUDED PARAMETERS DUE TO SENSITIVITY RATIOS:',/,1X, &
        '--------------------------------------------------------',/,2X, &
        'Parameter     Sensitivity Ratio      Status',/,1X, &
        '--------------------------------------------------------')
    101 FORMAT(2X,A,5X,1PE12.4,4X,'    OMITTED')
    102 FORMAT(2X,A,5X,1PE12.4,4X,'    REINCLUDED')
    103 FORMAT(23X,'NONE')
    104 FORMAT(/,1X,'NUMBER OF OMITTED PARAMETERS BASED ON INSENSITIVITY = ',I4)
    105 FORMAT(/, &
        1X,'THE COMBINATION OF ',/, &
        1X,' # of OBSERVATIONS included in the regression: ',I8,/, &
        1X,' # of ESTIMATED PARAMETERS included in the regression: ',I8,/, &
        1X,' resulting from inputs as well as eliminations based on',/, &
        1X,'   settings for OmitDefault OmitInsensitive & Constrain',/, &
        1X,'   renders the regression impossible. Please review inputs,',/, &
        1X,'   and reports of elimination in the output file.',//, &
        1X,'DEGREES of FREEDOM = ',I8,/)
    ! INITIALIZE
    PSTATUS = ' '
    ! DETERMINE NPERDSEN
    NPERDSEN = NPE
    BNDCYCLE = 1
    ELIMINATED = 0
    ! IF LOW SENS PARAMS ARE OMITTED
    IF (OMIT_INSENS) THEN
      WRITE(IOUT,100)
      ! FIND MAX CSS
      CSSMAX = 0
      DO I = 1,NPE
        IF(ABS(CSS(I)) > CSSMAX) CSSMAX = CSS(I)
      ENDDO
      ! CHECK FOR PARAMS WITH SENS < CRITERION
      DO I = 1,NPE
        RATIO = ABS(CSS(I)/CSSMAX)
        IF(RATIO < MINSENRAT) THEN
          PINCSEN(I) = PINCSEN(I) + 1
          NPERDSEN = NPERDSEN - 1
          WRITE(IOUT,101)PARNAM(IPTR(I)),RATIO
          PSTATUS(I) = '!'
        ELSE
          ! NOT LESS THAN CRITERION BUT IF IT WAS BEFORE. IS IT NOW GREATER
          ! THAN THE REINCLUDE CRITERION?
          IF(PINCSEN(I) > 0) THEN
            ! IF SO INCLUDE
            IF(RATIO > REINCSENRAT) THEN
              PINCSEN(I) = 0
              PSTATUS(I) = ' '
              WRITE(IOUT,102)PARNAM(IPTR(I)),RATIO
            ! IF NOT INCREMENT # OF TIMES OMITTED
            ELSE
              PINCSEN(I) = PINCSEN(I) + 1
              PSTATUS(I) = '!'
              NPERDSEN = NPERDSEN - 1
              WRITE(IOUT,101)PARNAM(IPTR(I)),RATIO
            ENDIF
          ELSE
            ! OTHERWISE IT IS INCLUDED
            PINCSEN(I) = 0
            PSTATUS(I) = ' '
          ENDIF
        ENDIF
      ENDDO
      IF(NPERDSEN .EQ. NPE) WRITE(IOUT,103)
      WRITE(IOUT,104) NPE-NPERDSEN
      WRITE(*,104) NPE-NPERDSEN
    ENDIF
    NPERD = NPERDSEN
    NPERDSTART = NPERD
    ! TEST TO SEE IF WE STILL HAVE SUFFICIENT
    ! #S OF OBS AND PARAMS TO DO A REGRESSION
    NIM = NOBS-IOMIT+MPR
    IF (NPERDSEN < 1 .OR. NIM < 1 .OR. NIM < NPERDSEN) THEN
      WRITE(IOUT,105)NIM,NPERDSEN,NIM-NPERDSEN-1
      WRITE(*,105)NIM,NPERDSEN,NIM-NPERDSEN-1
      CALL UTL_STOP('TERMINATION due to insuffcicent degrees of freedom')
    ENDIF
    ! SET UP PRIOR X ARRAY FOR INACTIVE PARAMETERS
    ! These linear sensitivities are only calculated at the start of the
    ! regression so this copy is used to temporarily set sensitivity to zero
    ! fir any one iteration
    XPRIRD = XPRI
    ! SET UP POINTER FOR ACTIVE PARAMETERS AND PREPARE REDUCDED X ARRAYS
    IF(ALLOCATED(NPTR)) DEALLOCATE(NPTR)
    ALLOCATE(NPTR(NPERDSEN))
    NPTR = 0
    IF(ALLOCATED(XPTR)) DEALLOCATE(XPTR)
    ALLOCATE(XPTR(NPERDSEN))
    NPTR = 0
    J = 0
    K = 0
    DO I = 1,NPE
      ! THIS PARAMETER IS ADJUSTABLE SO INCREMENT FOR XPTR
      ! NPTR has dimensions equal to number of included parameters and points
      ! to the full parameter list similar to iprt for all adjustable
      ! XPTR has dimensions equal to number of included parameters and points
      ! to the position in the original adjustable parameter list
      IF(PADJ(IPTR(I))) K = K + 1
      ! THIS PARAMETER IS OMITTED
      IF(PINCSEN(I) > 0) THEN
        ! THIS PARAMETER IS NOT INCLUDED
        ! ASSIGN ITS SENSITIVITIES 0
        DO N = 1,NOBS
          XSENST(I,N) = 0.D0
        ENDDO
        ! ASSIGN ITS PRIOR X 0
        DO N = 1,MPR
          XPRIRD(I,N) = 0.D0
        ENDDO
      ELSE
        ! THIS PARAMETER IS INCLUDED SO ASSIGN ITS IPTR POINTER TO NPS TO NPTR
        J = J + 1
        NPTR(J) = IPTR(I)
        XPTR(J) = K
      ENDIF
    ENDDO
    RETURN
  END SUBROUTINE REG_GNMOD_INI2
!===============================================================================
!===============================================================================
  SUBROUTINE REG_GNMOD_GEN (IFAIL,IOUT,MPR,NOBS,NPE,NPS, &
                         CONSTRAIN,CONSTRAINL,DATAEXCHANGE,IAP,IPTR, &
                         MAXCHANGEREALM, &
                         MRQTDIRECTION,MRQTINCREMENT,MRQTFACTOR, &
                         MAXITER,LN,OMIT_INSENS,OUTNAM,CREATEINITFILES, &
                         PARNAM,PMAXCHANGE, &
                         PRIWTMAT,PTOLPAR,PVALINIT,PVALMAXC,PVALMINC, &
                         QNITER,QUASINEWTON,RESIDS,RESIDSPRI,RSQALL, &
                         QNSOSR,TOLWTTOS,WTFULL,WTOSL,XSENST, &
                         AP,DMX,FINALSTATS, &
                         IFO,IND, &
                         ITERP,JDAMP,PAREST,PVAL)
  !     VERSION 20031109 EPP
  !     MODIFIED FROM MF2K PES1GAU1AP VERSION 20020708 ERB
  !******************************************************************
  !
  !-------ASSEMBLE LEAST-SQUARES MATRIX (C) AND GRADIENT VECTOR (G)
  !----------INITIALIZE C AND CONVERT LN PARAMETERS
  !     UPDATE PARAMETER VALUES VIA MODIFIED GAUSS-NEWTON REGRESSION
  !******************************************************************
  !        SPECIFICATIONS:
  !------------------------------------------------------------------
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                         INTENT(INOUT) :: IFAIL
    INTEGER,                         INTENT(IN)    :: IOUT
    INTEGER,                         INTENT(IN)    :: MPR
    INTEGER,                         INTENT(IN)    :: NOBS
    INTEGER,                         INTENT(IN)    :: NPE
    INTEGER,                         INTENT(IN)    :: NPS
    LOGICAL,                         INTENT(IN)    :: CONSTRAIN(NPS)
    LOGICAL,                         INTENT(IN)    :: CONSTRAINL
    LOGICAL,                         INTENT(IN)    :: DATAEXCHANGE
    INTEGER,                         INTENT(IN)    :: IAP
    INTEGER,                         INTENT(IN)    :: IPTR(NPE)
    CHARACTER(LEN=10),               INTENT(IN)    :: MAXCHANGEREALM
    DOUBLE PRECISION,                INTENT(IN)    :: MRQTDIRECTION
    DOUBLE PRECISION,                INTENT(IN)    :: MRQTINCREMENT
    DOUBLE PRECISION,                INTENT(IN)    :: MRQTFACTOR
    INTEGER,                         INTENT(IN)    :: MAXITER
    INTEGER,                         INTENT(IN)    :: LN(NPS)
    LOGICAL,                         INTENT(IN)    :: CREATEINITFILES
    LOGICAL,                         INTENT(IN)    :: OMIT_INSENS
    CHARACTER(LEN=MAX_STRING_LEN),   INTENT(IN)    :: OUTNAM
    CHARACTER(LEN=12),               INTENT(IN)    :: PARNAM(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: PMAXCHANGE(NPS)
    TYPE (CDMATRIX),                 INTENT(IN)    :: PRIWTMAT
    DOUBLE PRECISION,                INTENT(IN)    :: PTOLPAR(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: PVALINIT(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: PVALMAXC(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: PVALMINC(NPS)
    INTEGER,                         INTENT(IN)    :: QNITER
    LOGICAL,                         INTENT(IN)    :: QUASINEWTON
    DOUBLE PRECISION,                INTENT(IN)    :: RESIDS(NOBS)
    DOUBLE PRECISION,                INTENT(IN)    :: RESIDSPRI(MPR)
    DOUBLE PRECISION,                INTENT(IN)    :: RSQALL(MAXITER+2)
    DOUBLE PRECISION,                INTENT(IN)    :: QNSOSR
    DOUBLE PRECISION,                INTENT(IN)    :: TOLWTTOS
    TYPE (CDMATRIX),                 INTENT(IN)    :: WTFULL
    LOGICAL,                         INTENT(IN)    :: WTOSL
    DOUBLE PRECISION,                INTENT(INOUT) :: XSENST(NPE,NOBS)
    DOUBLE PRECISION,                INTENT(INOUT) :: AP
    DOUBLE PRECISION,                INTENT(INOUT) :: DMX
    LOGICAL,                         INTENT(INOUT) :: FINALSTATS
    INTEGER,                         INTENT(INOUT) :: IFO
    INTEGER,                         INTENT(INOUT) :: IND
    INTEGER,                         INTENT(INOUT) :: ITERP
    INTEGER,                         INTENT(INOUT) :: JDAMP
    DOUBLE PRECISION,                INTENT(INOUT) :: PAREST(MAXITER,NPS)
    DOUBLE PRECISION,                INTENT(INOUT) :: PVAL(NPS)
    !
    ! Local Variables
    DOUBLE PRECISION                              :: ABDMX
    DOUBLE PRECISION                              :: ABDMXU
    DOUBLE PRECISION                              :: ABDMXN
    DOUBLE PRECISION                              :: ABDMXP
    DOUBLE PRECISION                              :: ADMXU
    DOUBLE PRECISION                              :: ADMXN
    DOUBLE PRECISION                              :: ADMXP
    DOUBLE PRECISION                              :: AMP
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: AMXCHG
    DOUBLE PRECISION                              :: AAMXCHG
    INTEGER                                       :: AMXCHGJ
    DOUBLE PRECISION                              :: AP1
    DOUBLE PRECISION                              :: APO
    DOUBLE PRECISION                              :: APN
    DOUBLE PRECISION                              :: APP
    DOUBLE PRECISION                              :: APU
    DOUBLE PRECISION                              :: ATMP = 0.D0
    INTEGER                                       :: ATMPJ = 0
    DOUBLE PRECISION                              :: BDMX
    DOUBLE PRECISION                              :: BDMXU
    DOUBLE PRECISION                              :: BDMXN
    DOUBLE PRECISION                              :: BDMXP
    DOUBLE PRECISION                              :: BU
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: BUFFTMP
    LOGICAL                                       :: CONSTRAINTRAP = .FALSE.
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: CTMP
    DOUBLE PRECISION                              :: DDVMAXN
    DOUBLE PRECISION                              :: DDVMAXP
    DOUBLE PRECISION                              :: DET
    DOUBLE PRECISION                              :: DMAX1
    DOUBLE PRECISION                              :: DMAXIU
    DOUBLE PRECISION                              :: DMAXIP
    DOUBLE PRECISION                              :: DMAXTMP
    DOUBLE PRECISION                              :: DMAXU
    DOUBLE PRECISION                              :: DMAXP
    DOUBLE PRECISION, SAVE                        :: DMX1
    DOUBLE PRECISION, SAVE                        :: DMXO
    DOUBLE PRECISION                              :: DMXN
    DOUBLE PRECISION                              :: DMXP
    DOUBLE PRECISION                              :: DMXU
    DOUBLE PRECISION                              :: DTMPA
    INTEGER                                       :: I
    INTEGER                                       :: ICNT
    INTEGER                                       :: IIPP
    INTEGER                                       :: IOSTAR = 0 !1 no screen prt
    INTEGER                                       :: IP
    INTEGER                                       :: IP1
    INTEGER                                       :: J
    INTEGER                                       :: JJ
    INTEGER                                       :: JMINCHG
    INTEGER                                       :: JJN
    INTEGER                                       :: JJP
    INTEGER                                       :: JJU
    INTEGER                                       :: JDP
    INTEGER                                       :: JDN
    INTEGER                                       :: JDU
    INTEGER                                       :: JMAXCHG
    INTEGER                                       :: LNN
    DOUBLE PRECISION                              :: MINFCHG
    INTEGER                                       :: NP1
    DOUBLE PRECISION                              :: OCF
    DOUBLE PRECISION                              :: PTEST
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: PVALTP
    CHARACTER(LEN=MAX_STRING_LEN)                 :: OUTNAMTMP
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: RESIDSWPRI
    DOUBLE PRECISION                              :: SPR
    DOUBLE PRECISION                              :: TEMP
    DOUBLE PRECISION                              :: TMPB
    TYPE (CDMATRIX)                               :: WTFULLPRI
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: XSENSTWPRI
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: XTW
    !
    ! Formats
    400 FORMAT(//,80('='),/,80('='))
    401 FORMAT(/,1X,'UCODE Modified Gauss-Newton: ', &
                    'Parameter-Estimation Iteration#:',I5)
    500 FORMAT (/,' SCALED LEAST-SQUARES MATRIX :')
    505 FORMAT (/,' SCALED GRADIENT VECTOR :')
    515 FORMAT (/,' VALUES FROM SOLVING THE NORMAL EQUATION :',/, &
     2X,' MRQT PARAMETER ------------------- = ',G11.5,/,  &
     2X,' FRACTIONAL PARAMETER CHANGE IS EVALUATED IN                  ', &
          A10,' SPACE',/, &
     7X,' MAXIMUM FRACTIONAL CHANGE OCCURRED FOR PARAMETER:  "',A,'"',/, &
     7X,'               MAXIMUM FRACTIONAL PARAMETER CHANGE     = ',1PG10.3,/, &
     7X,' CONVERGENCE TOLERANCE FOR THIS PARAMETER (TolPar)     = ',1PG10.3,/, &
     7X,' MAXIMUM CHANGE ALLOWED FOR THIS PARAMETER (MaxChange) = ',1PG10.3,/)
    516 FORMAT (/,' VALUES FROM SOLVING THE NORMAL EQUATION :',/, &
     2X,' MRQT PARAMETER ------------------- = ',G11.5,/,  &
     2X,' FRACTIONAL PARAMETER CHANGE FOR LIMITING CHANGE IS IN:       ', &
          A10,' SPACE',/, &
     7X,' MAXIMUM FRACTIONAL CHANGE OCCURRED FOR PARAMETER:  "',A,'"',/, &
     7X,'               MAXIMUM FRACTIONAL PARAMETER CHANGE     = ',1PG10.3,/, &
     7X,' MAXIMUM FRACTIONAL CHANGE IN NATIVE SPACE CONTROLS CONVERGENCE',/, &
     7X,' MAXIMUM FRACTIONAL NATIVE SPACE CHANGE PARAMETER:  "',A,'"',/, &
     7X,'               MAXIMUM FRACTIONAL CHANGE, NATIVE SPACE = ',1PG10.3,/, &
     7X,' CONVERGENCE TOLERANCE FOR THIS PARAMETER (TolPar)     = ',1PG10.3,/, &
     7X,' MAXIMUM CHANGE ALLOWED FOR THIS PARAMETER (MaxChange) = ',1PG10.3,/)
    520 FORMAT (' CALCULATION OF DAMPING PARAMETER',/, &
     38X,'CONTROLLING PARAMETER : "',A,'"',/, &
     2X,' CHANGE CALCULATED FOR CONTROLLING PARAM: ',G9.2, &
        '   USED: ',G9.2,/, &
     2X,' OSCILL. CONTROL FACTOR (1 is NO EFFECT) = ',G11.5,/, &
     2X,' DAMPING PARAMETER (RANGE 0 TO 1) ------ = ',G11.5)
    521 FORMAT (' PARAMETER CONSTRAINTS RESULTED IN ADDITIONAL DAMPING ',/, &
     39X,' CONTROLLING PARAMETER : "',A,'"',/, &
     39X,' ADDITIONAL DAMPING    = ',G11.5)
    522 FORMAT (' ADJUSTMENTS TO PARAMETER CHANGE VECTOR WERE NOT REQUIRED')
    523 FORMAT (' HOWEVER PARAMETER CONSTRAINTS RESULTED IN DAMPING ',/, &
     39X,' CONTROLLING PARAMETER : "',A,'"',/, &
     39X,' ADDITIONAL DAMPING    = ',G11.5)
    525 FORMAT (6(A1,A12))
    530 FORMAT (6(2X,1PG11.4))
    545 FORMAT (/,' STARTING VALUES OF REGRESSION PARAMETERS :',/)
    546 FORMAT (/,' STARTING VALUES OF REGRESSION PARAMETERS :',/, &
                  '  "!" PRECEDING PARAMETER NAME INDICATES OMISSION FROM ', &
                  'THE REGRESSSION',/)
    547 FORMAT (/,' UPDATED ESTIMATES OF REGRESSION PARAMETERS :',/)
    548 FORMAT (/,' UPDATED ESTIMATES OF REGRESSION PARAMETERS :',/, &
                1X,' "!" PRECEEDING THE PARAMETER NAME INDICATES ', &
                'OMISSION FROM THE REGRESSION',/)
    550 FORMAT (/,' ERROR: CALCULATED PARAMETER CHANGE OF ',G12.5,/, &
        ' FOR LOG-TRANSFORMED PARAMETER "',A,'" IS TOO LARGE FOR ',/, &
        ' CALCULATION OF EXPONENTIAL -- STOP EXECUTION (REG_GNMOD_GEN)')
    !
    ALLOCATE(AMXCHG(NPS),PVALTP(NPS))
    !
    ! Initialize
    CALL TYP_NULL(WTFULLPRI)
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine REG_GNMOD_GEN'
    IFAIL = 0
    RECALCDDV = .TRUE.
    REDAMP = .FALSE.
    !
    ! START FIRST ITERATION
    IF (ITERP.EQ.1) THEN
      DMXU = 0
      IF(OMIT_INSENS) THEN
        WRITE (IOUT,546)
      ELSE
        WRITE (IOUT,545)
      ENDIF
      WRITE (IOUT,525) (PSTATUS(IP),PARNAM(IPTR(IP)),IP=1,NPE)
      WRITE (IOUT,'(1X)')
      WRITE (IOUT,530) (PVALINIT(IPTR(IP)),IP=1,NPE)
    ENDIF
    !     PRINT ITERATION HEADING
    WRITE(IOUT,400)
    IF (.NOT. FINALSTATS) WRITE(IOUT,401) ITERP
    DO WHILE(RECALCDDV)
      AMP = 0.D0
      IND = 0
      AMPA=0.D0
      IFO = 0
      JMINCHG = 0
      MINFCHG = 0.D0
      SCLE=0.0D0
      NP1 = NPE - 1
      IF(.NOT. WTADJFINAL) THEN
        WTADJF = .FALSE.
        IF(.NOT. WTADJNEXT) WTADJ = .FALSE.
      ENDIF
      !
      !-------ASSEMBLE LEAST-SQUARES MATRIX (C) AND GRADIENT VECTOR (G)
      !----------INITIALIZE C AND CONVERT LN PARAMETERS
      CMAT = 0.D0
      GG = 0.D0
      DO IP = 1, NPE
        IIPP = IPTR(IP)
        IF (LN(IIPP) > 0) PVAL(IIPP) = DLOG(PVAL(IIPP))
      ENDDO
      !----------CALCULATE SENSITIVITY CONTRIBUTIONS TO C AND G.
      IF (MPR > 0) THEN
        IF(ALLOCATED(RESIDSWPRI)) DEALLOCATE(RESIDSWPRI)
        IF(ALLOCATED(XSENSTWPRI)) DEALLOCATE(XSENSTWPRI)
        IF(ALLOCATED(XTW)) DEALLOCATE(XTW)
        ALLOCATE(RESIDSWPRI(NOBS+MPR),XSENSTWPRI(NPE,NOBS+MPR), &
                 XTW(NPE,NOBS+MPR))
        DO I=1,NPE
          DO J=1,NOBS
            XSENSTWPRI(I,J) = XSENST(I,J)
            RESIDSWPRI(J) = RESIDS(J)
          ENDDO
          DO J=1,MPR
            XSENSTWPRI(I,NOBS+J) = XPRIRD(I,J)
            RESIDSWPRI(NOBS+J) = RESIDSPRI(J)
          ENDDO
        ENDDO
        CALL UTL_COMBINESQMATRIX(WTFULL,PRIWTMAT,WTFULLPRI)
        ! Calculate XTw
        CALL UTL_MATMUL_SUB(NPE,NOBS+MPR,XSENSTWPRI,WTFULLPRI,XTW)
        ! XTwX
        CMAT = MATMUL(XTW,TRANSPOSE(XSENSTWPRI))
        ! GG = XTw(residuals)
        GG =  MATMUL(XTW,RESIDSWPRI)
        DEALLOCATE(RESIDSWPRI,XSENSTWPRI)
        CALL TYP_DEALLOC (WTFULLPRI)
      ELSE
        IF(ALLOCATED(XTW)) DEALLOCATE(XTW)
        ALLOCATE(XTW(NPE,NOBS))
        ! Calculate XTw
        CALL UTL_MATMUL_SUB(NPE,NOBS,XSENST,WTFULL,XTW)
        ! XTwX
        CMAT = MATMUL(XTW,TRANSPOSE(XSENST))
        ! GG = XTw(residuals)
        GG =  MATMUL(XTW,RESIDS)
      ENDIF
      IF((ITERP == 1 .AND. DATAEXCHANGE) .OR. CREATEINITFILES) THEN
      ! WRITE COVARIANCE MATRIX _mv
        ALLOCATE(BUFFTMP(NPE,NPE),CTMP(NPE,NPE))
        BUFFTMP = 0.D0
        CTMP = CMAT
        CALL UTLUCODE_INVERT(IFAIL,CTMP,NPE,BUFFTMP,IOUT,BU)
        BUFFTMP = BUFFTMP*RSQALL(ITERP)/REAL(NIM-NPE)
        OUTNAMTMP = TRIM(OUTNAM)//'._init'
        CALL UTL_DX_WRITE_MCMV('_mv',NPE,NPS,IPTR,OUTNAMTMP,PARNAM,BUFFTMP)
      ENDIF
      !-----QUASI-NEWTON ADDITION TO COEFFICIENT MATRIX
      IF (QUASINEWTON .AND. IFO .EQ. 0) CALL REG_GNMOD_GEN_QUASINEWTON_C &
                             (IFAIL,ITERP,MAXITER,QNITER,NOBS,NPE, &
                                RESIDS,RSQALL,QNSOSR,WTFULL,XSENST,IOUT,CMAT)
      !-----FOR ONE PARAMETER CASE
      IF (NPE.LT.2) THEN
      !-----CALCULATE STEP LENGTH FOR SINGLE-PARAMETER CASE
        DET = CMAT(1,1)
        SCLE(1) = 1.D0
        !-----IF MATRIX EQUATION IS SINGULAR, OR CONVERGENCE HAS BEEN REACHED:
        IF (IFO > 0) THEN
          IF (LN(IPTR(1)) > 0) PVAL(IPTR(1)) = EXP(PVAL(IPTR(1)))
          DEALLOCATE(AMXCHG,PVALTP)
        RETURN
        ENDIF
        DDV(1) = GG(1)/DET
      ELSE
      !-----SCALE COEFFICIENT MATRIX AND GRADIENT VECTOR
        DO IP = 1, NPE
          SCLE(IP) = 1.D0
          IF (CMAT(IP,IP) > 1.D-30) SCLE(IP) = DSQRT(CMAT(IP,IP))
        ENDDO
        DO IP = 1, NP1
          DTMPA = SCLE(IP)
          IP1 = IP + 1
          DO I = IP1, NPE
            CMAT(I,IP) = CMAT(I,IP)/(SCLE(I)*DTMPA)
            CMAT(IP,I) = CMAT(I,IP)
          ENDDO
          GG(IP) = GG(IP)/DTMPA
          CMAT(IP,IP) = 1.D0 + AMP
        ENDDO
        GG(NPE) = GG(NPE)/SCLE(NPE)
        CMAT(NPE,NPE) = 1.D0 + AMP
        !-----PRINT AS INDICATED BY IVERB
        IF (IVERB.GE.5) THEN
          WRITE (IOUT,500)
          DO J = 1, NPE
            WRITE (IOUT,530) (CMAT(I,J),I=1,NPE)
          ENDDO
          WRITE (IOUT,505)
          WRITE (IOUT,530) (GG(I),I=1,NPE)
        ENDIF
        !-----COMPUTE PARAMETER STEP LENGTHS
        CALL REG_GNMOD_GEN_DDV(IFAIL,IFO,NPE,GG, &
                             MRQTDIRECTION,MRQTINCREMENT,MRQTFACTOR, &
                             AMP,CMAT,DDV,DET,IND)
        IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from REG_GNMOD ')
        !-----IF MATRIX EQUATION IS SINGULAR, OR CONVERGENCE HAS BEEN REACHED:
        IF (IND  >  0 .OR. IFO  >  0) THEN
        !-------CONVERT NATURAL LOGS OF PARAMETER VALUES AND RETURN
          DO IP = 1, NPE
            IIPP = IPTR(IP)
            IF (LN(IIPP) > 0) PVAL(IIPP) = EXP(PVAL(IIPP))
          ENDDO
          !-----SINGULAR COEFFICIENT MATRIX
          IF (IND > 0) THEN
             IFAIL = 1
             AMESSAGE = ' LEAST SQUARES COEFFICIENT MATRIX SINGULAR '
             WRITE(*,*)AMESSAGE
             CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
          ENDIF
          DEALLOCATE(AMXCHG,PVALTP)
          RETURN
        ENDIF
        !-----UNSCALE PARAMETER CHANGE VECTOR
        DO IP = 1, NPE
          DDV(IP) = DDV(IP)/SCLE(IP)
        ENDDO
      ENDIF
      !-----COMPUTE DAMPING PARAMETER AND NEW ESTIMATES OF REGRESSION
      !-----PARAMETERS
      !----------DEFINITION OF VARIABLES
      !----------x IS U for untransformed parameters
      !---------------N for transformed parameters with negative DDV
      !---------------P for transformed parameters with positive DDV
      !----------ABDMXx Absolute value of BDMXx
      !----------ADMXx Values used to calculate the damping parameter (Col B,
      !----------------------Table B1)
      !----------AP   Value of the damping parameter used. Results in all
      !----------------------parameters respecting MAX-CHANGE or MAX-CHANGE*
      !----------APx  Value of the damping parameter if only the U, N, or P
      !----------------------parameters were considered
      !----------BDMXx  Maximum fractional parameter change calculated
      !----------------------by the normal equations (Col A, Table B1)
      !----------DDVMAXx Maximum ddv value; used only for x= N and P
      !----------DMAXIx Indicator for adjusting MAXCHANGE for extreme
      !----------------------parameter values (B/B0 of eq. B4)
      !----------DMXx  Values used for oscillation control (Col C, Table B1)
      !----------JDx  Parameter that governs damping; JDx may not equal JJx
      !----------JJx  Parameter with maximum change for convergence testing
      !----------DEFINITION OF VARIABLE NOT CHANGED IN THE CALCULATIONS
      !----------IAP = 0, Apply MAXCHANGE in native space
      !----------IAP = 1, Apply MAXCHANGE in parameter optimization space
      !---------------------------------------------------(possibly transformed)
      DMXO = DMX
      ADMXU = 0.D0
      ADMXN = 0.D0
      ADMXP = 0.D0
      BDMXU = 0.D0
      BDMXN = 0.D0
      BDMXP = 0.D0
      ABDMXU = 0.D0
      ABDMXN = 0.D0
      ABDMXP = 0.D0
      AMXCHG = 0.D0
      AAMXCHG = 0.D0
      AMXCHGJ = 1
      DMAXIU = 0.D0
      DMAXIP = 0.D0
      DDVMAXN = 0.D0
      DDVMAXP = 0.D0
      DMAXU = 1.D0
      DMAXP = 1.D0
      APO = AP
      APU = 1.D0
      APN = 1.D0
      APP = 1.D0
      JJU = 1
      JJN = 1
      JJP = 1
      JDU = 1
      JDP = 1
      JDN = 1
      DO IP = 1, NPE
        IIPP = IPTR(IP)
        DTMPA = 0.D0
        TMPB = 0.D0
        LNN = LN(IIPP)
        AMXCHG(IIPP) = 0.D0
        IF(LNN < 1) THEN
          AMXCHG(IIPP) = PVAL(IIPP)
          IF (DABS(AMXCHG(IIPP)) .EQ. 0.D0) AMXCHG(IIPP) = 1.0
          AMXCHG(IIPP) = DDV(IP)/AMXCHG(IIPP)
        ELSE
          AMXCHG(IIPP) = EXP(DDV(IP))-1.0
        ENDIF
        !-------PARAMETERS ADJUSTED AS IN REGRESSION SPACE
        IF (LNN < 1 .OR. IAP .EQ. 1) THEN
          DTMPA = PVAL(IIPP)
          IF (DABS(DTMPA).EQ.0.0) DTMPA = 1.0
          DTMPA = DDV(IP)/ABS(DTMPA)
          TMPB = DABS(DTMPA)
         !----------IF THIS IS THE LARGEST CHANGE, SAVE THE INFORMATION
         !----------FOR CONVERGENCE AND OSCILLATION CONTROL
          IF(TMPB > ADMXU) THEN
            ADMXU = TMPB
            JJU = IIPP
            DMXU = DTMPA
            BDMXU = DMXU
            ABDMXU = ABS(DMXU)
          ENDIF
          !----------ADJUST MAXCHANGE IF THE PARAMETER VALUE IS VERY SMALL
          DMAXTMP = PMAXCHANGE(IIPP)
          IF (PVALINIT(IIPP) .NE. 0.0) THEN
            ! PVALINIT IS STORED IN NATIVE SPACE
            IF(LNN < 1) THEN
              DMAXIU = PVAL(IIPP)/PVALINIT(IIPP)
            ELSE
              IF(PVALINIT(IIPP) .NE. 1.D0)DMAXIU =PVAL(IIPP)/DLOG(PVALINIT(IIPP))
            ENDIF
            CALL REG_GNMOD_GEN_ADJ_DMAX (IFAIL,IOUT,PARNAM(IIPP),DMAXTMP,DMAXIU)
            IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from REG_GNMOD ')
          ENDIF
          IF (DMAXTMP .LT. PMAXCHANGE(IIPP)) DMAXTMP = PMAXCHANGE(IIPP)
          !----------CALCULATE THE DAMPING THAT WOULD BE NEEDED FOR THIS
          !----------PARAMETER (COL B, TABLE B1)
          TEMP = 1.D0
          IF (TMPB.NE.0.0) TEMP = DMAXTMP/TMPB
          !----------IF THIS IS THE SMALLEST DAMPING PARAMETER, SAVE INFORMATION
          IF (NPE.EQ.1 .OR. APU > TEMP) THEN
            IF (TEMP.LT.1.0) APU = TEMP
            JDU = IIPP
            DMAXU = DMAXTMP
          ENDIF
        ENDIF
        !-----LOG-TRANSFORMED PARAMETERS ADJUSTED AS NATIVE
        IF (LNN > 0 .AND. IAP.EQ.0) THEN
        !--------DECREASING PARAMETER VALUES
          IF (DDV(IP) .LT. 0.0) THEN
          !-------------DAMPING PARAMETER
            IF (PMAXCHANGE(IIPP) .LT. 1.0) THEN
              ADMXN = DLOG(-PMAXCHANGE(IIPP)+1.D0)/DDV(IP)
              IF (ADMXN .LT. APN) THEN
                APN = ADMXN
                JDN = IIPP
              ENDIF
            ENDIF
            !--------------LARGEST CHANGE
            IF(NPE.EQ.1 .OR. DDV(IP).LT.DDVMAXN) THEN
              JJN = IIPP
              DDVMAXN = DDV(IP)
              DMXN = DDV(IP)
              IF (PVAL(IIPP).NE.0.0) DMXN = DDV(IP)/ABS(PVAL(IIPP))
              BDMXN = EXP(DDV(IP))-1.0
              ABDMXN = ABS(BDMXN)
            ENDIF
          ELSE
            !--------INCREASING PARAMETER VALUE
            !-------------DAMPING PARAMETER
            DMAXTMP = PMAXCHANGE(IIPP)
            ! PVALINIT IS STORED IN NATIVE SPACE
            DMAXIP = EXP(PVAL(IIPP))/PVALINIT(IIPP)
            CALL REG_GNMOD_GEN_ADJ_DMAX (IFAIL,IOUT,PARNAM(IIPP),DMAXTMP,DMAXIP)
            IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from REG_GNMOD ')
            ADMXP = DLOG(DMAXTMP+1.)/DDV(IP)
            IF (ADMXP .LT. APP) THEN
              APP = ADMXP
              JDP = IIPP
              DMAXP = DMAXTMP
            ENDIF
            !--------------LARGEST CHANGE
            IF(NPE.EQ.1 .OR. DDV(IP) > DDVMAXP) THEN
              JJP = IIPP
              DDVMAXP = DDV(IP)
              DMXP = DDV(IP)
              IF (PVAL(IIPP).NE.0.0) DMXP = DDV(IP)/ABS(PVAL(IIPP))
              IF (DDV(IP).LE.709.) THEN
                BDMXP = EXP(DDV(IP))-1.0
              ELSE
                WRITE(IOUT,550) DDV(IP), PARNAM(IIPP)
                CALL UTL_STOP(' ')
              ENDIF
              ABDMXP = ABS(BDMXP)
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      !-------FIND LARGEST BDMXx (NATIVE PARAMETER CHANGE
      !-------FROM THE NORMAL EQUATIONS) AND ASSOCIATED PARAMETER NUMBER.
      IF (ABDMXU .EQ. 0.D0 .AND. ABDMXN .EQ. 0.D0 .AND. ABDMXP .EQ. 0.D0) THEN
        AMESSAGE = ' NONE OF THE PARAMETERS ARE CHANGING '
        CALL UTL_WRITE_MESSAGE(-1,'yes','yes','yes')
        CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
        AMESSAGE = ' CHECK THAT PROCESS MODEL IS EXECUTING FOR THIS ITERATION'
        CALL UTL_WRITE_MESSAGE(-1,'no','yes','yes')
        CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','yes')
        AMESSAGE = ' CONFIRM THAT EXTRACTIONS ARE TAKEN FROM THE UPDATED OUTPUT'
        CALL UTL_WRITE_MESSAGE(-1,'no','yes','yes')
        CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','yes')
        AMESSAGE = ' EVALUATE SENSITIVITIES '
        CALL UTL_WRITE_MESSAGE(-1,'no','yes','yes')
        CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','yes')
        AMESSAGE = ' TERMINATING '
        CALL UTL_WRITE_MESSAGE(-1,'no','yes','yes')
        CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','yes')
        CALL UTL_STOP(TRIM(ERRSUB//' called from REG_GNMOD '))
      ENDIF
      IF (ABDMXU.GE.ABDMXP .AND. ABDMXU.GE.ABDMXN) THEN
        JMAXCHG = JJU
        BDMX = BDMXU
        ABDMX = ABDMXU
        DMX = DMXU
      ELSEIF (ABDMXP.GE.ABDMXU .AND. ABDMXP.GE.ABDMXN) THEN
        JMAXCHG = JJP
        BDMX = BDMXP
        ABDMX = ABDMXP
        DMX = DMXP
      ELSEIF (ABDMXN.GE.ABDMXU .AND. ABDMXN.GE.ABDMXP) THEN
        JMAXCHG = JJN
        BDMX = BDMXN
        ABDMX = ABDMXN
        DMX = DMXN
      ENDIF
      IF(WTOSL .AND. ITERP > 1) THEN
        IF (.NOT. WTADJFINAL) THEN
          IF (WTADJNEXT) THEN
            PTEST = 0.1*(PTOLPAR(JMAXCHG)*TOLWTTOS-PTOLPAR(JMAXCHG))+PTOLPAR(JMAXCHG)
            IF (ABDMX < PTEST) THEN
              WTADJFINAL = .TRUE.
              WRITE(IOUT,*)' Met criteria to stop weight adjustment '
            ENDIF
          ELSE
            PTEST = PTOLPAR(JMAXCHG)*TOLWTTOS
            IF (ABDMX < PTEST) THEN
              WTADJNEXT = .TRUE.
              WRITE(IOUT,*)' Met criteria for simulated value weight adjustment'
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      !
      !-----WITH A MIXTURE OF NATIVE AND TRANSFORMED PARAMETERS, THE
      !-----DAMPING PARAMETER MAY NOT BE ASSOCIATED WITH THE PARAMETER
      !-----WITH THE LARGEST NATIVE PARAMETER CHANGE AS CALCULATED BY
      !-----THE NORMAL EQUATIONS. USE DAMPING PARAMETERS FOR ALL
      !-----TYPES CALCULATED ABOVE AND PICK THE SMALLEST ONE.
      !
      !
      !-----DETERMINE THE DAMPING PARAMETER
      !
      !---------IF ALL APx = 1.0, SET DAMPING PARAMETER TO 1.0
      IF (APU.EQ.1.0 .AND. APN.EQ.1.0 .AND. APP.EQ.1.0) THEN
        AP=1.D0
        JJ = JMAXCHG
        DMAX1 = PMAXCHANGE(IIPP)
      !       DMX FROM ABOVE
      !--------...OTHERWISE, FIND THE SMALLEST DAMPING PARAMETER
      ELSE
        IF (APU.LE.APP .AND. APU.LE.APN) THEN
          AP = APU
          JJ = JDU
          DMX = DMXU
          DMAX1 = DMAXU
        ELSEIF (APP.LE.APU .AND. APP.LE.APN) THEN
          AP = APP
          JJ = JDP
          DMX = DMXP
          DMAX1 = DMAXP
        ELSEIF (APN.LE.APU .AND. APN.LE.APP) THEN
          AP = APN
          JJ = JDN
          DMX = DMXN
          DMAX1 = PMAXCHANGE(IIPP)
        ENDIF
      ENDIF
      !
      !---------IF THE SAME PARAMETER CONTROLLED THE DAMPING PARAMETER
      !---------LAST ITERATION, CONSIDER OSCILLATION CONTROL.
      !-----------THESE CALCULATIONS ARE DONE IN TRANSFORMED PARAMETER
      !-----------SPACE USING DMXx
      OCF = 1.D0
      IF (ITERP > 1) THEN
        IF (JJ.EQ.JDAMP) THEN
          DMX1 = DMX
          SPR = DMX1/(APO*DMXO)
          IF (SPR.LT.-1.) THEN
            AP1 = .5/ABS(SPR)
          ELSE
            AP1 = (3.+SPR)/(3.+ABS(SPR))
          ENDIF
        ELSE
          AP1 = 1.D0
        ENDIF
        IF(AP1.LT.AP) then
          AP = AP1
          OCF = AP
        ENDIF
      ENDIF
      JDAMP = JJ
      !
      !---TEMPRORAY UPDATE OF PARAMETERS AND CONVERT TO PHYSICAL VALUES
      DO IP = 1, NPE
        IIPP = IPTR(IP)
        DDVC(IP) = AP*DDV(IP)
        PVALTP(IIPP) = PVAL(IIPP) + DDVC(IP)
        IF(LN(IIPP) > 0) PVALTP(IIPP) = EXP(PVALTP(IIPP))
      ENDDO
      ! FIRST CHECK IF ALL PARAMETERS ARE IN BOUNNDS
      IF(CONSTRAINL) THEN
        ! CHECK TO SEE IF CONSTRAINTED PARAMETERS ARE REPEATING
        IF(ITERP > 2) THEN
          ! ARE THE CONSTRAINTS CAUSING AN ENDLESS LOOP?
          ATMP = 0.D0
          ATMPJ = 0
          DO IP = 1, NPE
            IIPP = IPTR(IP)
            !****************!??IF(ABS(AMXCHG(IIPP)) > AAMXCHG) THEN
            ! perhaps instead check if all are the same not just maxs and
            ! let the remaining adjust?
            IF(ABS(AMXCHG(IIPP)) > ATMP) THEN
              ATMP = ABS(AMXCHG(IIPP))
              ATMPJ = IIPP
            ENDIF
          ENDDO
          CONSTRAINTRAP = .FALSE.
          IF(ATMPJ > 0 .AND. ATMPJ == NPMAXCHG(ITERP-1))THEN
            IF(ATMP == DMXA(ITERP-1))THEN
              CONSTRAINTRAP = .TRUE.
              AMESSAGE = ' CONSTRAINED PARAMETERS ARE REPEATING '
              CALL UTL_WRITE_MESSAGE(-1,'no','yes','no')
              CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','no')
              AMESSAGE = '   CONSIDER WHETHER BOUNDS ARE REASONABLE '
              CALL UTL_WRITE_MESSAGE(-1,'no','no','no')
              CALL UTL_WRITE_MESSAGE(IOUT,'no','no','no')
            ENDIF
          ENDIF
        ENDIF
        CALL REG_GNMOD_CHECKBND &
                 (IOUT,ATMPJ,MPR,NOBS,NPE,NPS,IPTR,CONSTRAIN,CONSTRAINTRAP,LN, &
                  OMIT_INSENS,PARNAM,PVAL,PVALTP,PVALMAXC,PVALMINC, &
                  XSENST,JMINCHG,MINFCHG)
        IF(RECALCDDV) THEN
          DO IP = 1,NPE
            IIPP = IPTR(IP)
            IF(LN(IIPP) > 0) PVAL(IIPP) = EXP(PVAL(IIPP))
          ENDDO
        ELSEIF(REDAMP) THEN
          DO IP = 1, NPE
            IIPP = IPTR(IP)
            PVAL(IIPP) = PVAL(IIPP) + DDV(IP)
            IF(LN(IIPP) > 0) PVAL(IIPP) = EXP(PVAL(IIPP))
          ENDDO
        ELSE
          DO IP = 1, NPE
            IIPP = IPTR(IP)
            PVAL(IIPP) = PVALTP(IIPP)
          ENDDO
          DDV = DDVC
          DD = DDV
        ENDIF
      ELSE
        RECALCDDV = .FALSE.
        DO IP = 1, NPE
            IIPP = IPTR(IP)
            PVAL(IIPP) = PVALTP(IIPP)
        ENDDO
        DDV = DDVC
        DD = DDV
      ENDIF
    ENDDO
    ! FINAL VALUES ESTABLISHED
    DO IP=1,NPS
      PAREST(ITERP,IP) = PVAL(IP)
    ENDDO
    !
    ! CHECK FOR CONVERGENCE BY FRACTIONAL PARAMETER CHANGE
    AAMXCHG = 0.D0
    DO IP = 1, NPE
      IIPP = IPTR(IP)
      IF(ABS(AMXCHG(IIPP)) > AAMXCHG) THEN
        AAMXCHG = ABS(AMXCHG(IIPP))
        AMXCHGJ = IIPP
      ENDIF
    ENDDO
    IFO = 1
    DO IP = 1, NPE
      IIPP = IPTR(IP)
      ! corrected 9/10/2010 IF (ABS(AMXCHG(IIPP)) .GT. PTOLPAR(IIPP)) THEN
      IF (ABS(BDMX) .GT. PTOLPAR(IIPP)) THEN
        IFO = 0
        EXIT
      ENDIF
    ENDDO
    IF(IFO == 0 .AND. ITERP == MAXITER) IFO = 3
    !-------PRINT TO THE LISTING FILE
    ! furthest from convergence is JMAXCHG
    ! smallest damping is JDAMP
    IF(IAP == 0) THEN
      WRITE (IOUT,515) AMP, MAXCHANGEREALM, PARNAM(JMAXCHG), BDMX, &
                     PTOLPAR(JMAXCHG),PMAXCHANGE(JMAXCHG)
    ELSE
      WRITE (IOUT,516) AMP, MAXCHANGEREALM, PARNAM(JMAXCHG), BDMX, &
          PARNAM(AMXCHGJ), AMXCHG(AMXCHGJ), PTOLPAR(AMXCHGJ),PMAXCHANGE(AMXCHGJ)
    ENDIF

    WRITE(*,*)
    IF(OCF  /= 1.0 .OR. AP /= 1.0) THEN
      WRITE (IOUT,520) PARNAM(JDAMP), PMAXCHANGE(JDAMP), DMAX1, OCF, AP
      IF(JMINCHG > 0) WRITE (IOUT,521) PARNAM(JMINCHG), MINFCHG
    ELSE
      WRITE (IOUT,522)
      IF(JMINCHG > 0) WRITE (IOUT,523) PARNAM(JMINCHG), MINFCHG
    ENDIF
    IF(OMIT_INSENS .OR. CONSTRAINL) THEN
      WRITE (IOUT,548)
    ELSE
      WRITE (IOUT,547)
    ENDIF
    WRITE (IOUT,525) (PSTATUS(IP),PARNAM(IPTR(IP)),IP=1,NPE)
    WRITE (IOUT,'(1X)')
    WRITE (IOUT,530) (PVAL(IPTR(IP)),IP=1,NPE)
    !-------PRINT TO THE SCREEN
    IF (IOSTAR.NE.1) THEN
      IF(IAP == 0) THEN
        WRITE (*,515) AMP, MAXCHANGEREALM, PARNAM(JMAXCHG), BDMX, &
                       PTOLPAR(JMAXCHG),PMAXCHANGE(JMAXCHG)
      ELSE
        WRITE (*,516) AMP, MAXCHANGEREALM, PARNAM(JMAXCHG), BDMX, &
          PARNAM(AMXCHGJ), AMXCHG(AMXCHGJ), PTOLPAR(AMXCHGJ),PMAXCHANGE(AMXCHGJ)
      ENDIF
      IF(OCF  /= 1.0 .OR. AP /= 1.0) THEN
        WRITE (*,520) PARNAM(JDAMP), PMAXCHANGE(JDAMP), DMAX1, OCF, AP
        IF(JMINCHG > 0) WRITE (*,521) PARNAM(JMINCHG), MINFCHG
      ELSE
        WRITE (*,522)
        IF(JMINCHG > 0) WRITE (*,523) PARNAM(JMINCHG), MINFCHG
      ENDIF
      IF(OMIT_INSENS .OR. CONSTRAINL) THEN
        WRITE (*,548)
      ELSE
        WRITE (*,547)
      ENDIF
      WRITE (*,525)(PSTATUS(IP),PARNAM(IPTR(IP)),IP=1,NPE)
      WRITE (*,'(1X)')
      WRITE (*,530) (PVAL(IPTR(IP)),IP=1,NPE)
    ENDIF
    !-------STORE STATISTICS FOR THIS ITERATION
    ! corrected 9/10/2010 DMXA(ITERP) = AMXCHG(AMXCHGJ)
    DMXA(ITERP) = BDMX
    NPDAMP(ITERP) = JDAMP
    AMPA(ITERP) = AMP
    AMCA(ITERP) = PMAXCHANGE(JMAXCHG)
    AAP(ITERP) = AP
    NPAREST(ITERP) = NPERD
    IF(NPERD < NPE)NPARSTAR(ITERP) = '*'
    IF(REDAMP)NPARSTAR(ITERP) = '-'
    NPMAXCHG(ITERP) = 0
    IF (AMXCHGJ /= 0) NPMAXCHG(ITERP) = AMXCHGJ
    DEALLOCATE(AMXCHG,PVALTP)
    RETURN
  END SUBROUTINE REG_GNMOD_GEN
!===============================================================================
!===============================================================================
  SUBROUTINE REG_GNMOD_CHECKBND &
                 (IOUT,ATMPJ,MPR,NOBS,NPE,NPS,IPTR,CONSTRAIN,CONSTRAINTRAP,LN, &
                  OMIT_INSENS,PARNAM,PVAL,PVALTP,PVALMAXC,PVALMINC, &
                  XSENST,JMINCHG,MINFCHG)
    IMPLICIT NONE
    INTEGER,                         INTENT(IN)    :: IOUT
    INTEGER,                         INTENT(IN)    :: ATMPJ
    INTEGER,                         INTENT(IN)    :: MPR
    INTEGER,                         INTENT(IN)    :: NOBS
    INTEGER,                         INTENT(IN)    :: NPE
    INTEGER,                         INTENT(IN)    :: NPS
    INTEGER,                         INTENT(IN)    :: IPTR(NPE)
    LOGICAL,                         INTENT(IN)    :: CONSTRAIN(NPS)
    LOGICAL,                         INTENT(IN)    :: CONSTRAINTRAP
    INTEGER,                         INTENT(IN)    :: LN(NPS)
    LOGICAL,                         INTENT(IN)    :: OMIT_INSENS
    CHARACTER(LEN=12),               INTENT(IN)    :: PARNAM(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: PVAL(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: PVALTP(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: PVALMAXC(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: PVALMINC(NPS)
    DOUBLE PRECISION,                INTENT(INOUT) :: XSENST(NPE,NOBS)
    INTEGER,                         INTENT(OUT)   :: JMINCHG
    DOUBLE PRECISION,                INTENT(OUT)   :: MINFCHG
    ! local variables
    INTEGER                                        :: ELIM
    DOUBLE PRECISION                               :: FCHG
    INTEGER                                        :: I
    INTEGER                                        :: IMOUT
    INTEGER                                        :: IP
    INTEGER                                        :: J
    INTEGER                                        :: JDDVELIM
    INTEGER                                        :: L
    INTEGER                                        :: M
    DOUBLE PRECISION                               :: MAXFCHG
    DOUBLE PRECISION                               :: MCHG
    INTEGER                                        :: N
    INTEGER, ALLOCATABLE, DIMENSION(:)             :: NEWPTR
    INTEGER                                        :: NUMOUT1ST
    INTEGER                                        :: NUMOUTAGAIN
    INTEGER                                        :: NUMOUTOPP
    INTEGER                                        :: SUMELIM
    ! FORMATS
      1 FORMAT(1X,79('-'))
     10 FORMAT(/,2X,'NO PARAMETERS ARE OUT OF BOUNDS',/)
     20 FORMAT(/,2X,'NO ADDITIONAL PARAMETERS ARE OUT OF BOUNDS')
     30 FORMAT(/,2X,'ALL OUT OF BOUND PARAMETERS ARE OUT FOR THE 1st TIME, ', &
               'UPDATING WILL BE DAMPED',/)
     31 FORMAT(/,1X,'REMAINING OUT OF BOUND PARAMETERS ARE OUT FOR 1st TIME,', &
               ' UPDATING WILL BE DAMPED',/)
     40 FORMAT(/,1X,' GAUSS-NEWTON UPDATING WILL BE RECALCULATED OMITTING: ',A)
     50 FORMAT(/,2X,'CYCLE = ',I3,2X,'NUMBER OF ADJUSTABLE PARAMETERS = ',I5)
     51 FORMAT(15X,'NUMBER OF OMITTED INSENSITIVE PARAMETERS = ',I5)
     52 FORMAT(15X,'NUMBER OF OMITTED BOUNDED PARAMETERS     = ',I5)
     53 FORMAT(15X,'TOATL NUMBER OF OMITTED PARAMETERS       = ',I5)
     98 FORMAT(/,1X, &
       'OMITTED/REINCLUDED PARAMETERS DUE TO CONSTRAINT TO BOUNDS:',/, &
       1X,79('-'),/,2X,'Parameter      Value        MIN          MAX',/, &
       1X,79('-'))
     99 FORMAT( &
       /,2X,'CYCLE ',I4,2X, &
       ' OF GAUSS-NEWTON UPDATING WITH ELIMINATED BOUNDED PARAMETERS:',/, &
       /,2X,'Parameter      Value        MIN          MAX')
    100 FORMAT(2X,A,3(1X,1PE12.4),1X,'  BELOW BOUND 1st time')
    101 FORMAT(2X,A,3(1X,1PE12.4),1X,'  ABOVE BOUND 1st time')
    102 FORMAT(2X,A,3(1X,1PE12.4),1X,'  REINCLUDED')
    105 FORMAT(/, &
        1X,79("!"),/, &
        1X,'NO MORE PARAMETERS LEFT TO ESTIMATE ',/, &
        1X,'   due to eliminations based on settings for',/, &
        1X,'   OmitDefault OmitInsensitive & Constrain',/, &
        1X,'   this renders the regression impossible. Please review ',/, &
        1X,'   inputs, and reports of elimination in the output file.',/, &
        1X,79("!")//)
    200 FORMAT(2X,A,3(1X,1PE12.4),1X,'  BELOW BOUND again')
    201 FORMAT(2X,A,3(1X,1PE12.4),1X,'  ABOVE BOUND again')
    300 FORMAT(2X,A,3(1X,1PE12.4),1X,'  BELOW BOUND was above')
    301 FORMAT(2X,A,3(1X,1PE12.4),1X,'  ABOVE BOUND was below')
    400 FORMAT(2X,A,3(1X,1PE12.4),1X,'  CIRCULAR CONSTRAINT')
    500 FORMAT(2X,A,' HAS ALREADY BEEN ELIMIMNATED FOR THIS ITERATION')
    !
    ELIM = 0
    IMOUT = 0
    JDDVELIM = 0
    JMINCHG = 0
    RECALCDDV = .FALSE.
    REDAMP = .FALSE.
    !
    IF(BNDCYCLE .EQ. 1) THEN
      WRITE(IOUT,98)
    ELSE
      WRITE(IOUT,99)BNDCYCLE
    ENDIF
    ! CHECK FOR BOUNDED PARAMETERS
    MINFCHG = 1.D30
    MAXFCHG = 0.D0
    MCHG = 0.D0
    FCHG = 1.D30
    NUMOUT1ST = 0
    NUMOUTAGAIN = 0
    NUMOUTOPP = 0
    DO I = 1,NPE
      IP = IPTR(I)
      ! IF THIS PARAMETER IS TO BE CONSTRAINED BY BOUNDS, CHECK VALUES
      IF(CONSTRAIN(IP)) THEN
        ! EVALUATE IF NOT OMITTED FOR INSENSITIVITY
        IF(PINCSEN(I) .EQ. 0 .AND. ELIMINATED(I) .EQ. 0) THEN
          ! CHECK FOR BELOW LIMIT
          IF(PVALTP(IP) < PVALMINC(IP)) THEN
            IF(LN(IP) > 0) THEN
              FCHG = ABS((PVAL(IP) - DLOG(PVALMINC(IP))) / DDV(I))
              MCHG = ABS((DLOG(PVALMINC(IP)) - DLOG(PVALTP(IP))) / DDV(I))
            ELSE
              FCHG = ABS((PVAL(IP) - PVALMINC(IP)) / DDV(I))
              MCHG = ABS((PVALMINC(IP) - PVALTP(IP)) / DDV(I))
            ENDIF
            ! WAS IT IN BOUNDS LAST ITERATION?
            IF(PINCBND(I) .EQ. 0) THEN
              NUMOUT1ST = NUMOUT1ST + 1
              PINCBND(I) = - 1
              IF(FCHG < MINFCHG) THEN
                MINFCHG = FCHG
                JMINCHG = IP
              ENDIF
              WRITE(IOUT,100)PARNAM(IP),PVALTP(IP),PVALMINC(IP),PVALMAXC(IP)
              ! CHECK FOR TRAP IN ENDLESS LOOP
              IF(CONSTRAINTRAP .AND. IP == ATMPJ) THEN
                NUMOUTAGAIN = NUMOUTAGAIN + 1
                PINCBND(I) = PINCBND(I) - 1
                IF(MCHG > MAXFCHG) THEN
                  MAXFCHG = FCHG
                  JDDVELIM = IP
                  ELIM = I
                ENDIF
                WRITE(IOUT,400)PARNAM(IP),PVALTP(IP),PVALMINC(IP),PVALMAXC(IP)
              ENDIF
            ELSEIF (PINCBND(I) < 0) THEN
            ! WAS IT BELOW LIMIT LAST ITERATION?
              NUMOUTAGAIN = NUMOUTAGAIN + 1
              PINCBND(I) = PINCBND(I) - 1
              IF(MCHG > MAXFCHG) THEN
                MAXFCHG = FCHG
                JDDVELIM = IP
                ELIM = I
              ENDIF
              WRITE(IOUT,200)PARNAM(IP),PVALTP(IP),PVALMINC(IP),PVALMAXC(IP)
            ELSEIF (PINCBND(I) > 0) THEN
            ! WAS IT ABOVE LIMIT LAST ITERATION?
              ! IF IT HAD ALREADY BEEN OMITTED DUE TO ABOVE
              ! ONLY DAMP THIS TIME
              NUMOUTOPP = NUMOUTOPP + 1
              PINCBND(I) = -1
              WRITE(IOUT,300)PARNAM(IP),PVALTP(IP),PVALMINC(IP),PVALMAXC(IP)
            ENDIF
          ELSEIF(PVALTP(IP) > PVALMAXC(IP)) THEN
            ! CHECK FOR ABOVE LIMIT
            IF(LN(IP) > 0) THEN
              FCHG = ABS((DLOG(PVALMAXC(IP)) - PVAL(IP)) / DDV(I))
              MCHG = ABS((DLOG(PVALTP(IP)) - DLOG(PVALMAXC(IP))) / DDV(I))
            ELSE
              FCHG = ABS((PVALMAXC(IP) - PVAL(IP)) / DDV(I))
              MCHG = ABS((PVALTP(IP) - PVALMAXC(IP)) / DDV(I))
            ENDIF
            IF(PINCBND(I) .EQ. 0) THEN
            ! WAS IT WITHIN BOUNDS LAST ITERATION?
              NUMOUT1ST = NUMOUT1ST + 1
              PINCBND(I) = +1
              IF(FCHG < MINFCHG) THEN
                MINFCHG = FCHG
                JMINCHG = IP
              ENDIF
              WRITE(IOUT,101)PARNAM(IP),PVALTP(IP),PVALMINC(IP),PVALMAXC(IP)
              ! CHECK FOR TRAP IN ENDLESS LOOP
              IF(CONSTRAINTRAP .AND. IP == ATMPJ) THEN
                NUMOUTAGAIN = NUMOUTAGAIN + 1
                PINCBND(I) = PINCBND(I) + 1
                IF(MCHG > MAXFCHG) THEN
                  MAXFCHG = FCHG
                  JDDVELIM = IP
                  ELIM = I
                ENDIF
                WRITE(IOUT,400)PARNAM(IP),PVALTP(IP),PVALMINC(IP),PVALMAXC(IP)
              ENDIF
            ELSEIF (PINCBND(I) > 0) THEN
            ! WAS IT ABOVE LIMIT LAST ITERATION?
              NUMOUTAGAIN = NUMOUTAGAIN + 1
              PINCBND(I) = PINCBND(I) + 1
              IF(MCHG > MAXFCHG) THEN
                MAXFCHG = FCHG
                JDDVELIM = IP
                ELIM = I
              ENDIF
              WRITE(IOUT,201)PARNAM(IP),PVALTP(IP),PVALMINC(IP),PVALMAXC(IP)
            ELSEIF (PINCBND(I) < 0) THEN
            ! WAS IT BELOW LIMIT LAST ITERATION?
              ! IF IT HAD ALREADY BEEN OMITTED DUE TO BELOW
              ! ONLY DAMP THIS TIME
              NUMOUTOPP = NUMOUTOPP + 1
              PINCBND(I) = +1
              WRITE(IOUT,301)PARNAM(IP),PVALTP(IP),PVALMINC(IP),PVALMAXC(IP)
            ENDIF
          ELSE
            ! PARAMETER IS WITHIN BOUNDS BUT WAS IT OUT BEFORE?
            ! THEN WRITE REINCLUDE MESSAGE
            IF(PINCBND(I) .NE. 0) THEN
              PINCBND(I) = 0
              WRITE(IOUT,102)PARNAM(IP),PVALTP(IP),PVALMINC(IP),PVALMAXC(IP)
            ENDIF
          ENDIF
        ELSE
          ! PARAMETER IS INSENSITIVE or WAS PREVIOUSLY ELIMINATED THIS ITERATION
          IF(ELIMINATED(I) > 0) THEN
            WRITE(IOUT,500)PARNAM(IP)
          ENDIF
        ENDIF
      ENDIF
    ENDDO
    SUMELIM = 0
    DO I = 1,NPE
      SUMELIM = SUMELIM + ELIMINATED(I)
    ENDDO
    IMOUT = NUMOUT1ST + NUMOUTAGAIN + NUMOUTOPP
    ! IF ANY PARAMETERS ARE OUT OF BOUNDS, ADJUST DDV
    IF(IMOUT .EQ. 0) THEN
      ! NO PARAMETERS ARE OUT OF BOUNDS THAT HAVE NOT ALREADY BEEN ELIMINATED
      ! SO CONTINUE
      IF(SUMELIM .EQ. 0) THEN
        WRITE(IOUT,10)
        WRITE(*,10)
      ELSE
        WRITE(IOUT,20)
        WRITE(*,20)
        RECALCDDV = .FALSE.
        REDAMP = .FALSE.
      ENDIF
    ELSEIF (NUMOUTAGAIN .EQ. 0) THEN
      ! OUT OF BOUND PARAMETERS ARE OUT FOR 1st TIME OR IN OPPOSITE DIRECTION
      ! SO LIMIT DDV
      IF(SUMELIM == 0) THEN
        WRITE(IOUT,30)
        WRITE(*,30)
      ELSE
        WRITE(IOUT,31)
        WRITE(*,31)
      ENDIF
      DDV = DDV * MINFCHG
      RECALCDDV = .FALSE.
      REDAMP = .TRUE.
    ELSEIF (NUMOUTAGAIN > 0) THEN
      ! IF SOME PARAMETERS ARE OUT FOR AN ADDITIONAL TIME THEN
      ! RECALCULATE DDV OMITTING THE ONE WITH THE LARGEST CHANGE
      WRITE(IOUT,40) PARNAM(JDDVELIM)
      WRITE(*,40) PARNAM(JDDVELIM)
      ELIMINATED(ELIM) = 1
      RECALCDDV = .TRUE.
      REDAMP = .FALSE.
      ! CHECK NUMBER OF ACTIVE PARAMETERS
        NPERD = NPERDSEN - SUMELIM - 1
        ! TEST TO SEE IF WE STILL HAVE SUFFICIENT
        ! #S OF PARAMS TO DO A REGRESSION
        IF (NPERD < 1) THEN
          WRITE(IOUT,105)
          WRITE(*,105)
          CALL UTL_STOP('TERMINATION due to elimination of all parameters')
        ENDIF
        !REBUILD THE POINTER FROM INCLUDED PARAMETERS TO ALL PARAMETERS
        IF(ALLOCATED(NEWPTR)) DEALLOCATE(NEWPTR)
        ALLOCATE(NEWPTR(NPERD))
        NEWPTR = 0
        L = 1
        DO I = 1,NPE
          ! LOOK FOR OMITTED PARAMETER AND REMOVE FROM XPTR
          IF(IPTR(I) .EQ. JDDVELIM) THEN
            IF(L <= NPERD) THEN
              NEWPTR(L) = NPTR(L+1)
              DO J=L+1,NPERDSTART-1
                NEWPTR(J) = NPTR(J+1)
              ENDDO
            ENDIF
            ! ASSIGN ITS SENSITIVITIES 0
            DO N = 1,NOBS
              XSENST(I,N) = 0.D0
            ENDDO
            ! ASSIGN ITS PRIOR SENSITIVITIES 0
            DO N = 1,MPR
              XPRIRD(I,N) = 0.D0
            ENDDO
            EXIT
          ELSEIF(IPTR(I) .EQ. NPTR(L)) THEN
            NEWPTR(L) = NPTR(L)
            L = L + 1
          ENDIF
        ENDDO
        ! REBUILD THE POINTER FROM INCLUDED PARAMETERS TO
        ! ALL ADJUSTABLE PARAMETERS
        IF(ALLOCATED(NPTR)) DEALLOCATE(NPTR)
        ALLOCATE(NPTR(NPERD))
        NPTR = NEWPTR
        IF(ALLOCATED(XPTR)) DEALLOCATE(XPTR)
        ALLOCATE(XPTR(NPERD))
        XPTR = 0
        L = 1
        M = 1
        DO I=1,NPE
          IF(NPTR(L) .EQ. IPTR(I)) THEN
            XPTR(M) = I
            IF(L .EQ. NPERD) EXIT
            L = L + 1
            M = M + 1
          ENDIF
        ENDDO
      ELSE
    ENDIF
    IF(OMIT_INSENS .OR. NUMOUTAGAIN > 0) THEN
      WRITE(IOUT,50)BNDCYCLE,NPE
      WRITE(IOUT,51)NPE-NPERDSEN
      WRITE(IOUT,52)SUMELIM
      WRITE(IOUT,53)NPE-NPERDSEN+SUMELIM
      WRITE(*,50)BNDCYCLE,NPE
      WRITE(*,51)NPE-NPERDSEN
      WRITE(*,52)NPE-NPERDSEN+SUMELIM
    ENDIF
    WRITE(IOUT,1)
    NPERDSTART = NPERD
    BNDCYCLE = BNDCYCLE + 1
    ! CHECK OUT OF BOUNDS MARKER
    PSTATUS = ' '
    ! CHECK OUT OF BOUNDS MARKER
    DO I=1,NPE
      IF(CONSTRAIN(IPTR(I)))THEN
        IF(PVALTP(IPTR(I)) <= PVALMINC(IPTR(I)) .OR. &
          PVALTP(IPTR(I)) >= PVALMAXC(IPTR(I))) &
          PSTATUS(I) = '!'
      ENDIF
      IF(PINCSEN(I) > 0)PSTATUS(I) = '!'
    ENDDO
    RETURN
  END SUBROUTINE REG_GNMOD_CHECKBND
!===============================================================================
!===============================================================================
  SUBROUTINE REG_GNMOD_GEN_ADJ_DMAX (IFAIL,IOUT,PNAM,DMAX1,DMAXI)
    !     VERSION 20031109 EPP
    !      MODIFIED FROM MF2K SPES1GAU1DM VERSION 20000424 ERB
    !******************************************************************
    !     ADJUST DMAX FOR PARAMETER VALUES FAR FROM THE STARTING VALUE SO
    !     SO THAT EXTREME VALUES CAN RECOVER.
    !******************************************************************
    !        SPECIFICATIONS:
    !------------------------------------------------------------------
    IMPLICIT NONE
    ! Argument list variables
    INTEGER,                             INTENT(INOUT)    :: IFAIL
    INTEGER,                             INTENT(IN)       :: IOUT
    CHARACTER(LEN=12),                   INTENT(IN)       :: PNAM
    DOUBLE PRECISION,                    INTENT(INOUT)    :: DMAX1 !MAXCHG
    DOUBLE PRECISION,                    INTENT(INOUT)    :: DMAXI !CUR B:INIT B
    ! local variables
    DOUBLE PRECISION TEMP
    !------------------------------------------------------------------
    !
    100 FORMAT(/,1X,' MAXCHANGE TEMPORARILY INCREASED FOR ',A,' TO ',1PG10.3,/)
    ! Initialize
    AMESSAGE = ' '
!    ERRSUB=' Error in subroutine REG_GNMOD_GEN_ADJ_DMAX'
    IFAIL = 0
    ! IF THE CURRENT PARAMETER VALUE IS 0, OR THE USER SPECIFIED MAXCHG <0.4
    !    THEN DO NOT ADJUST MAXCHANGE
    IF (DMAXI.EQ.0.0 .OR. DMAX1.LT.0.4) RETURN
    ! HOLD SPECIFIED MAXCHG IN TEMP
    TEMP = DMAX1
    ! CALCULATE A NEW MAXCHG,
    ! INVERT: ADDV ONE TO ORIG, RAISE TO THE 4th, MULT by CURRENT RATIO
    DMAX1 = 1./(DMAXI*(DMAX1+1.)**4)
    ! IF NEW VALUE IS > THEN SPECIFIED VALUE, TEMPORARILY INCREASE MAXCHG
    IF (DMAX1.LT.TEMP) THEN
      DMAX1=TEMP
    ELSE
      WRITE(IOUT,100)PNAM,DMAX1
    ENDIF
    !
    RETURN
  END SUBROUTINE REG_GNMOD_GEN_ADJ_DMAX
!===============================================================================
!===============================================================================
  SUBROUTINE REG_GNMOD_GEN_DDV (IFAIL,IFO,NPE,GG, &
                              MRQTDIRECTION,MRQTINCREMENT,MRQTFACTOR, &
                              AMP,CMAT,DDV,DET,IND)
    !     VERSION 20031109 EPP
    !-----MODIFIED FROM MF2K SPES1GAU1SL VERSION 1000 01FEB1992
    !******************************************************************
    !         COMPUTE PARAMETER STEP LENGTHS USING THE MRQT PROCEDURE
    !******************************************************************
    !        SPECIFICATIONS:
    !------------------------------------------------------------------
    IMPLICIT NONE
    ! Argument list variables
    INTEGER,                         INTENT(INOUT)   :: IFAIL
    INTEGER,                         INTENT(IN)      :: IFO
    INTEGER,                         INTENT(IN)      :: NPE
    DOUBLE PRECISION,                INTENT(IN)      :: GG(NPE)
    DOUBLE PRECISION,                INTENT(IN)      :: MRQTDIRECTION
    DOUBLE PRECISION,                INTENT(IN)      :: MRQTINCREMENT
    DOUBLE PRECISION,                INTENT(IN)      :: MRQTFACTOR
    DOUBLE PRECISION,                INTENT(INOUT)   :: AMP
    DOUBLE PRECISION,                INTENT(INOUT)   :: CMAT(NPE,NPE)
    DOUBLE PRECISION,                INTENT(INOUT)   :: DDV(NPE)
    DOUBLE PRECISION,                INTENT(INOUT)   :: DET
    INTEGER,                         INTENT(INOUT)   :: IND
    ! LOCAL VARIABLES
    DOUBLE PRECISION                                 :: DPIV
    DOUBLE PRECISION                                 :: DSUM
    DOUBLE PRECISION                                 :: DTMPA
    INTEGER                                          :: I
    INTEGER                                          :: IP1
    INTEGER                                          :: J
    INTEGER                                          :: K
    INTEGER                                          :: KP1
    INTEGER                                          :: NM1
    DOUBLE PRECISION                                 :: SSUM
    DOUBLE PRECISION                                 :: SUMA
    DOUBLE PRECISION                                 :: SUMB
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine REG_GNMOD_GEN_DDV'
    IFAIL = 0
    !------------------------------------------------------------------
    !         COMPUTE TRIAL PARAMETER STEP LENGTHS USING LDU FACTORIZATION:
    !           DECOMPOSE MATRIX
    NM1 = NPE - 1
    10 IND = 0
    DET = 1.D0
    DO K = 1, NM1
      DPIV = CMAT(K,K)
      DET = DET*DPIV
      IF (DPIV  >  1.E-13) THEN
        DPIV = 1.D0/DPIV
        KP1 = K + 1
        DO J = KP1, NPE
          DTMPA = CMAT(J,K)*DPIV
          DO I = J, NPE
            CMAT(I,J) = CMAT(I,J) - DTMPA*CMAT(I,K)
          ENDDO
        ENDDO
        CMAT(K,K) = DPIV
      ELSE
        IND = 1
        GOTO 110
      ENDIF
    ENDDO
    DET = DET*CMAT(NPE,NPE)
    IF (CMAT(NPE,NPE) .LE. 1.E-13) THEN
      IND = 1
      IF (IFO .EQ. 0) GO TO 110
    ENDIF
    IF (IFO  >  0) RETURN
    DO K = 1, NPE
      DDV(K) = GG(K)
    ENDDO
    !           FORWARD SUBSTITUTE
    DO K = 1, NM1
      DTMPA = DDV(K)*CMAT(K,K)
      KP1 = K + 1
      DO J = KP1, NPE
        DDV(J) = DDV(J) - CMAT(J,K)*DTMPA
      ENDDO
    ENDDO
    !           BACK SUBSTITUTE
    DDV(NPE) = DDV(NPE)/CMAT(NPE,NPE)
    I = NPE
    80 I = I - 1
    IF (I  >  0) THEN
      IP1 = I + 1
      DSUM = 0.D0
      DO J = IP1, NPE
        DSUM = DSUM + CMAT(J,I)*DDV(J)
      ENDDO
      DDV(I) = (DDV(I)-DSUM)*CMAT(I,I)
      GOTO 80
    ENDIF
    !         CHECK SOLUTION AND ADDV MRQT PARAMETER IF NEEDED
    SSUM = 0.D0
    SUMA = 0.D0
    SUMB = 0.D0
    DO I = 1, NPE
      SSUM = SSUM + DDV(I)*GG(I)
      SUMA = SUMA + DDV(I)*DDV(I)
      SUMB = SUMB + GG(I)*GG(I)
    ENDDO
    IF (SSUM  >  MRQTDIRECTION*SQRT(SUMA*SUMB)) RETURN
    110 AMP = MRQTFACTOR*AMP + MRQTINCREMENT
    IF (AMP  >  1.D0) RETURN
    DO I = 1, NPE
      CMAT(I,I) = 1.D0 + (AMP)
      DO J = I, NPE
        CMAT(J,I) = CMAT(I,J)
      ENDDO
    ENDDO
    GOTO 10
  END SUBROUTINE REG_GNMOD_GEN_DDV
!===============================================================================
!===============================================================================
  SUBROUTINE REG_GNMOD_GEN_QUASINEWTON_C (IFAIL,ITERP,MAXITER,QNITER,NOBS,NPE, &
                                RESIDS,RSQALL,QNSOSR,WTFULL,XSENST,IOUT,CMAT)
    ! VERSION 20040627 EPP
    !-DOES SAME CHORE AS MF2K SPES1GAU1QN
    !******************************************************************
    !     COMPUTE QUASI-NEWTON COMPONENT OF C MATRIX AND ADD TO C
    !      (NOTE: PRIOR NOT INCLUDED BECAUSE LINEAR TERMS DO NOT CONTRIBUTE)
    !******************************************************************
    !        SPECIFICATIONS:
    !------------------------------------------------------------------
    IMPLICIT NONE
    ! Argument list variables
    INTEGER,                INTENT(INOUT) :: IFAIL
    INTEGER,                INTENT(IN)    :: ITERP
    INTEGER,                INTENT(IN)    :: MAXITER
    INTEGER,                INTENT(IN)    :: QNITER
    INTEGER,                INTENT(IN)    :: NOBS
    INTEGER,                INTENT(IN)    :: NPE
    DOUBLE PRECISION,       INTENT(IN)    :: RESIDS(NOBS)
    DOUBLE PRECISION,       INTENT(IN)    :: RSQALL(MAXITER+2)
    DOUBLE PRECISION,       INTENT(IN)    :: QNSOSR
    TYPE (CDMATRIX),        INTENT(IN)    :: WTFULL
    DOUBLE PRECISION,       INTENT(IN)    :: XSENST(NPE,NOBS)
    INTEGER,                INTENT(IN)    :: IOUT
    DOUBLE PRECISION,       INTENT(INOUT) :: CMAT(NPE,NPE)
    ! Local variables
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: DDTRDDD
    DOUBLE PRECISION                              :: DENOM
    DOUBLE PRECISION                              :: DTU
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: DTUDG
    INTEGER                                       :: I
    INTEGER                                       :: J
    DOUBLE PRECISION                              :: NUMER
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: SCDTERM
    DOUBLE PRECISION                              :: SSWR2ITERS
    DOUBLE PRECISION                              :: T
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: TERM1
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: TERM2
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: U
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: WTR
    !------------------------------------------------------------------
    !
    510 FORMAT (/,' QUASI-NEWTON UPDATING:',/,2X, &
        ' # ITERATIONS BEFORE UPDATING ',I3,' CURRENT ITERATION ',I3,/,2X, &
        ' MIN % CHANGE SOSR OVER 2 ITERATIONS BEFORE UPDATING = ',G8.3,/)
    512 FORMAT (2X,' CHANGE in SOSR is not checked until later iterations ')
    513 FORMAT (2X,' % CHANGE SOSR OVER LAST 2 ITERATIONS WAS: ',G12.3)
    521 FORMAT (/,' QUASI-NEWTON UPDATING IS APPLIED ')
    522 FORMAT (/,' C Matrix before updating',/)
    523 FORMAT (/,' QUASI-NEWTON UPDATE MATRIX',/)
    524 FORMAT (/,' C Matrix after updating',/)
    525 FORMAT (/,' QUASI-NEWTON UPDATING IS NOT APPLIED FOR THIS ITERATION ',/)
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine REG_GNMOD_GEN_QUASINEWTON_C'
    IFAIL = 0
    IF (ITERP.EQ.1) THEN
      ALLOCATE(XD(NPE,NOBS),XW(NPE,NOBS))
    ENDIF
      CALL UTL_MATMUL_SUB(NPE,NOBS,XSENST,WTFULL,XW)
      GGQN = MATMUL(XW,RESIDS)
      GGQN = -GGQN
    !- FIRST PARAM-EST ITER, INITIALIZE UPDATING MATRIX R AND SAVE G AND X
    IF (ITERP.EQ.1) THEN
      GD = GGQN
      RD = 0.0D0
      DD = 0.0D0
      UPDATEQN = .FALSE.
      XD = XSENST
      RETURN
    ELSE
      ALLOCATE(DDTRDDD(NPE),DTUDG(NPE),SCDTERM(NPE), &
               TERM1(NPE,NPE),TERM2(NPE,NPE),U(NPE),WTR(NOBS))
      ! Note when entering this routine DD is the displacement from r-1
      !------FOR SUBSEQUENT ITERATIONS, CALCULATE MATRIX R
      IF(ITERP > 2) THEN
        SSWR2ITERS = ABS((RSQALL(ITERP)-RSQALL(ITERP-2))/RSQALL(ITERP-2))
      ELSE
        SSWR2ITERS = 2.D0 * QNSOSR
      ENDIF
      ! FIND t and u starting with second iteration
      GD = GGQN - GD
      XD = XSENST - XD
      CALL UTL_MATMULVEC_SUB(NOBS,WTFULL,RESIDS,WTR)
      U = MATMUL(XD,WTR)
      ! Numerator of t
      NUMER = 0.D0
      DO I = 1,NPE
        NUMER = NUMER + (DD(I)*U(I))
      ENDDO
      DDTRDDD = MATMUL(DD,RD)
      DENOM = 0.D0
      DO I = 1,NPE
        DENOM = DENOM + (DDTRDDD(I)*DD(I))
      ENDDO
      ! CALCULATE ALTERNATIVE FOR t AFTER ITER=2, BEFORE THAT LAST R=0, & T=1
      IF(ITERP > 2) THEN
        ! note DD is global and is dd of previous iiteration at this point
        T = ABS(NUMER/DENOM)
        IF(T > 1.D0) T = 1.D0
      ELSE
        T = 1.D0
      ENDIF
      SCDTERM = MATMUL(RD,DD)
      SCDTERM = T * SCDTERM
      U = U - SCDTERM
      ! CALCULATE R
      DENOM = 0.D0
      DO I = 1,NPE
        DENOM = DENOM + (DD(I)*GD(I))
      ENDDO
      TERM1 = 0.D0
      DO I = 1,NPE
        DO J = 1,NPE
          TERM1(I,J) = U(I)*GD(J) + GD(I)*U(J)
        ENDDO
      ENDDO
      TERM1 =TERM1/DENOM
      TERM2 = 0.D0
      DTU = 0.D0
      DO I = 1,NPE
        DTU = DTU + DD(I)*U(I)
      ENDDO
      DTUDG = DTU*GD
      DO I = 1,NPE
        DO J = 1,NPE
          TERM2(I,J) = DTUDG(I)*GD(J)
        ENDDO
      ENDDO
      TERM2 =TERM2/(DENOM*DENOM)
      RD = T*RD + TERM1 - TERM2
      !------SAVE GRADIENT
      GD = GGQN
      !-----SAVE SENSITIVITIES
      XD = XSENST
      WRITE(IOUT,510)QNITER,ITERP,QNSOSR*100.D0
      IF(ITERP > 2) THEN
        WRITE(IOUT,513)SSWR2ITERS*100.D0
      ELSE
        WRITE(IOUT,512)
      ENDIF
      !------ADD QN APPROXIMATION TO COEFFICIENT MATRIX
      IF (UPDATEQN .OR. (ITERP > QNITER) .OR. (SSWR2ITERS < QNSOSR)) THEN
        UPDATEQN = .TRUE.
        WRITE(IOUT,521)
        IF (IVERB > 4) THEN
          WRITE(IOUT,522)
          DO I=1,NPE
            WRITE(IOUT,*)(CMAT(I,J),J=1,NPE)
          ENDDO
          WRITE(IOUT,523)
          DO I=1,NPE
            WRITE(IOUT,*)(RD(I,J),J=1,NPE)
          ENDDO
        ENDIF
        CMAT = CMAT + RD
       IF (IVERB > 4) THEN
         WRITE(IOUT,524)
         DO I=1,NPE
           WRITE(IOUT,*)(CMAT(I,J),J=1,NPE)
         ENDDO
       ENDIF
      ELSE
        WRITE(IOUT,525)
      ENDIF
    ENDIF
    DEALLOCATE(DDTRDDD,DTUDG,SCDTERM,TERM1,TERM2,U,WTR)
    RETURN
  END SUBROUTINE REG_GNMOD_GEN_QUASINEWTON_C
!===============================================================================
!===============================================================================
  SUBROUTINE REG_GNMOD_UNCINI(MPR,NOBS,NPE,INITXS,INITXP)
    IMPLICIT NONE
    ! argument variables
    INTEGER,                    INTENT(IN) :: MPR
    INTEGER,                    INTENT(IN) :: NOBS
    INTEGER,                    INTENT(IN) :: NPE
    DOUBLE PRECISION,           INTENT(IN) :: INITXS(NPE,NOBS)
    DOUBLE PRECISION, OPTIONAL, INTENT(IN) :: INITXP(NPE,MPR)
    !
    IF(ALLOCATED(INITXSEN)) THEN
      INITXSEN = INITXS
    ELSE
      ALLOCATE(INITXSEN(NPE,NOBS))
      INITXSEN = INITXS
    ENDIF
    IF(PRESENT(INITXP)) THEN
      IF(ALLOCATED(INITXPRI)) THEN
        INITXPRI = INITXP
      ELSE
        ALLOCATE(INITXPRI(NPE,MPR))
        INITXPRI = INITXP
      ENDIF
    ENDIF
  RETURN
  END SUBROUTINE REG_GNMOD_UNCINI
!===============================================================================
!===============================================================================
  SUBROUTINE REG_GNMOD_UNC (IFAIL,IOUT,MPR,NOBS,NPE,NPS,AN_ITER, &
                         BSCAL,CHINTTYPE,CNTINT,DSQ,FL,FLCNV,GNAM,GPOPT, &
                         IINT,INTUNIT,INTUNITD,INTUNITSUM,IPTR,ISGN,ITERP, &
                         ITYP,LN,MAXCHANGE,MAXITER, &
                         MRQTDIRECTION,MRQTINCREMENT,MRQTFACTOR, &
                         NUMINTERVALS,PARNAM,PRIWTMAT,PVALINIT, &
                         RESIDS,RESIDSPRI,RSQALL, &
                         TOLP,TOLS,TOLY, &
                         WCI,WTFULL,XPRI,AP,DMX,IFO,IND,PVAL, &
                         ROWNUM,STORINTPAR,XSENST,ZZ)
  !******************************************************************
  !REGRESSION BY THE MODIFIED GAUSS-NEWTON METHOD TO ESTIMATE
  !PARAMETERS FOR CONFIDENCE INTERVALS BY THE VECCHIA & COOLEY (1987)
  !METHOD.
  !
  !TO CALCULATE LAMBDA (ALAM): SENSITIVITIES FOR THE CONFIDENCE
  !INTERVAL POINT IS STORED IN Z; VARIANCE OF MEASUREMENT ERROR
  !IS IN WCI; CRITICAL VALUE IS IN DSQ; LAMBDA SIGN IS IN SGN
  !******************************************************************
  !------------------------------------------------------------------
  !     VERSION 20031109 EPP
  !     MODIFIED FROM MF2K UNC1NLI1IT VERSION 1001 19JUN2003 (SC)
  !     MODIFIED BY EPP AUG 2005
  !******************************************************************
  !        SPECIFICATIONS:
  !------------------------------------------------------------------
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER,                        INTENT(INOUT) :: IFAIL
    INTEGER,                        INTENT(IN)    :: IOUT
    INTEGER,                        INTENT(IN)    :: MPR
    INTEGER,                        INTENT(IN)    :: NOBS
    INTEGER,                        INTENT(IN)    :: NPE
    INTEGER,                        INTENT(IN)    :: NPS
    LOGICAL,                        INTENT(IN)    :: AN_ITER
    DOUBLE PRECISION,               INTENT(IN)    :: BSCAL(NPS)
    CHARACTER(LEN=12),              INTENT(IN)    :: CHINTTYPE
    INTEGER,                        INTENT(IN)    :: CNTINT
    DOUBLE PRECISION,               INTENT(IN)    :: DSQ
    DOUBLE PRECISION,               INTENT(IN)    :: FL
    DOUBLE PRECISION,               INTENT(IN)    :: FLCNV
    CHARACTER(LEN=LENDNAM),         INTENT(IN)    :: GNAM
    DOUBLE PRECISION,               INTENT(IN)    :: GPOPT
    INTEGER,                        INTENT(IN)    :: IINT
    INTEGER,                        INTENT(IN)    :: INTUNIT
    INTEGER,                        INTENT(IN)    :: INTUNITD
    INTEGER,                        INTENT(IN)    :: INTUNITSUM
    INTEGER,                        INTENT(IN)    :: IPTR(NPE)
    INTEGER,                        INTENT(IN)    :: ISGN
    INTEGER,                        INTENT(IN)    :: ITERP
    INTEGER,                        INTENT(IN)    :: ITYP
    INTEGER,                        INTENT(IN)    :: LN(NPS)
    DOUBLE PRECISION,               INTENT(IN)    :: MAXCHANGE
    INTEGER,                        INTENT(IN)    :: MAXITER
    DOUBLE PRECISION,               INTENT(IN)    :: MRQTDIRECTION
    DOUBLE PRECISION,               INTENT(IN)    :: MRQTINCREMENT
    DOUBLE PRECISION,               INTENT(IN)    :: MRQTFACTOR
    INTEGER,                        INTENT(IN)    :: NUMINTERVALS
    CHARACTER(LEN=12),              INTENT(IN)    :: PARNAM(NPS)
    TYPE (CDMATRIX),                INTENT(IN)    :: PRIWTMAT
    DOUBLE PRECISION,               INTENT(IN)    :: PVALINIT(NPS)
    DOUBLE PRECISION,               INTENT(IN)    :: RESIDS(NOBS)
    DOUBLE PRECISION,               INTENT(IN)    :: RESIDSPRI(MPR)
    DOUBLE PRECISION,               INTENT(IN)    :: RSQALL(MAXITER+2)
    DOUBLE PRECISION,               INTENT(IN)    :: TOLP
    DOUBLE PRECISION,               INTENT(IN)    :: TOLS
    DOUBLE PRECISION,               INTENT(IN)    :: TOLY
    DOUBLE PRECISION,               INTENT(IN)    :: WCI
    TYPE (CDMATRIX),                INTENT(IN)    :: WTFULL
    DOUBLE PRECISION,               INTENT(IN)    :: XPRI(NPE,MPR)
    DOUBLE PRECISION,               INTENT(INOUT) :: AP
    DOUBLE PRECISION,               INTENT(INOUT) :: DMX
    INTEGER,                        INTENT(INOUT) :: IFO
    INTEGER,                        INTENT(INOUT) :: IND
    DOUBLE PRECISION,               INTENT(INOUT) :: PVAL(NPS)
    INTEGER,                        INTENT(INOUT) :: ROWNUM(NUMINTERVALS)
    DOUBLE PRECISION,               INTENT(INOUT):: STORINTPAR(NPE,NUMINTERVALS)
    DOUBLE PRECISION,               INTENT(INOUT) :: XSENST(NPE,NOBS)
    DOUBLE PRECISION,               INTENT(INOUT) :: ZZ(NPE)
    ! Local variables
    DOUBLE PRECISION, SAVE                        :: ADMX
    DOUBLE PRECISION                              :: ALAM
    DOUBLE PRECISION                              :: AMP
    DOUBLE PRECISION                              :: BBSCL
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: CSV
    DOUBLE PRECISION                              :: DD
    DOUBLE PRECISION                              :: DDC
    DOUBLE PRECISION                              :: DET
    DOUBLE PRECISION                              :: DG
    DOUBLE PRECISION, SAVE                        :: DMXO
    DOUBLE PRECISION                              :: DTMPA
    DOUBLE PRECISION                              :: DTMPB
    DOUBLE PRECISION, SAVE                        :: ER
    DOUBLE PRECISION                              :: FLIM
    DOUBLE PRECISION                              :: GGG
    INTEGER                                       :: I
    INTEGER                                       :: IIPP
    INTEGER                                       :: IP
    INTEGER                                       :: IP1
    INTEGER                                       :: J
    INTEGER                                       :: JJ
    INTEGER                                       :: N
    INTEGER                                       :: NP1
    DOUBLE PRECISION                              :: PCT
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: RESIDSWPRI
    DOUBLE PRECISION, SAVE                        :: SGN
    DOUBLE PRECISION                              :: SPR
    DOUBLE PRECISION                              :: SSE
    DOUBLE PRECISION                              :: TEST
    DOUBLE PRECISION                              :: TMPA
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: UU
    DOUBLE PRECISION                              :: UZ
    TYPE (CDMATRIX)                               :: WTFULLPRI
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: XSENSTWPRI
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: XTW
    !
    ! Formats
    100 FORMAT(/,' VALUES REPORTED ARE FOR ITERATION WITH SSE',/, &
    '       CLOSEST TO GOAL, WHICH WAS ITERATION:  ', &
    I8,'   of ',I8,' ITERATIONS',/, &
    '       PERCENT DEVIATION FROM GOAL:                 ',G13.5,/, &
    '       GOAL is SUM OF SQUARED, WEIGHTED RESIDUALS = ',G13.5,/,7X, &
    'If the percent deviation from goal for that iteration is large,',/,7X, &
    'consider starting the nonlinear interval calculation with a',/,7X, &
    'different value of MaxChange or with different parameter values.')
    101 FORMAT(//,200('*'),//)
    102 FORMAT(//,200('!'),//)
    110 FORMAT(/,' LOWER C.I. LIMIT FOR',3X,A,1X,I5,': FL=',G13.5)
    111 FORMAT(/,' UPPER C.I. LIMIT FOR',3X,A,1X,I5,': FL=',G13.5)
    120 FORMAT(/,' LOWER P.I. LIMIT FOR',3X,A,1X,I5,': YP=',G13.5)
    121 FORMAT(/,' UPPER P.I. LIMIT FOR',3X,A,1X,I5,': YP=',G13.5)
    130 FORMAT(/,' "INTERVAL NAME" "LIMIT IDENTIFIER" "CONVERGED" ', &
        '"PREDICTED VALUE" "CONFIDENCE LIMIT" "SUM OF SQUARED RESIDUALS" ', &
        '"OBJECTIVE-FUNCTION GOAL" "PERCENT DEVIATION FROM GOAL" ', &
        '"INDIVIDUALorSIMULTANEOUS" "ITERATIONS"',/)
    135 FORMAT(1X,A20,1X,I12,' "YES"     ',3(3X,1PE16.7),13X,1PE16.7,10X, &
               G16.7,12X,A13,1X,I12)
    136 FORMAT(1X,A20,1X,I12,' "!!!NO!!!"',3(3X,1PE16.7),13X,1PE16.7,10X, &
               G16.7,12X,A13,1X,I12)
    140 FORMAT(/,' "INTERVAL NAME" "LIMIT IDENTIFIER" "CONVERGED" ', &
        '"PREDICTED VALUE" "PREDICTION LIMIT" ', &
        '"SUM OF SQUARED RESIDUALS" "OBJECTIVE-FUNCTION GOAL" ', &
        '"PERCENT DIFFERENCE" "INDIVIDUALorSIMULTANEOUS" "ITERATIONS"',/)
!    145 FORMAT(1X,A20,1X,I12,' "Non-convergence"',52X,1PE16.7,38X,A13,1X,I12)
    145 FORMAT(1X,'!!! "INTERVAL DID NOT CONVERGE" ',47('!'))
    146 FORMAT(/' LOWER C.I. LIMIT FOR',3X,A,1X,I12,/, &
               ' !! Non-convergence !! FL=',G13.5)
    147 FORMAT(/' UPPER C.I. LIMIT FOR',3X,A,1X,I12,/, &
               ' !! Non-convergence !! FL=',G13.5)
    400 FORMAT(//,80('='),/,80('='))
    401 FORMAT(/,1X,'UCODE Modified Gauss-Newton: ', &
                    'Interval-Estimation #:',I5,' Iteration #:',I5)
    402 FORMAT(/,1X,'Predictive Sensitivity: ',//, &
                 1X,'  PARAMETER   ',3X,'Sensitivity',/ &
                 1X,'  ---------   ',3X,'-----------')
    403 FORMAT(3X,A12,1PE15.5)
    500 FORMAT (/,' SCALED LEAST-SQUARES MATRIX :')
    505 FORMAT (/,' SCALED GRADIENT VECTOR :')
    510 FORMAT (/,' ITERATION NO. = ',I5)
    515 FORMAT (/,' VALUES FROM SOLVING THE NORMAL EQUATION :',/, &
     2X,' OBJECTIVE FUNCTION GOAL----------- = ',G11.5,/,  &
     2X,' DETERMINANT OF COEFFICIENT MATRIX- = ',G11.5,/,  &
     2X,' MRQT PARAMETER ------------------- = ',G11.5,/,  &
     7X,' DAMPING PARAMETER                  = ',1PG10.3,/, &
     7X,' MAXIMUM FRACTIONAL CHANGE OCCURRED FOR PARAMETER:  "',A,'"',/, &
     7X,'               MAXIMUM FRACTIONAL PARAMETER CHANGE     = ',1PG10.3,/, &
     7X,' LAMBDA Equation 8-10 Chirstensen & Cooley             = ',1PG10.3,/, &
     7X,' COMPUTED PREDICTION ERROR                             = ',1PG10.3,   &
              //,' UPDATED ESTIMATES OF REGRESSION PARAMETERS :',/, &
              13(3X,A10,3X,A10,3X,A10,3X,A10,3X,A10,3X,A10,/))
    516 FORMAT (36X,4G14.5,2X,A10,2G14.5)
    517 FORMAT (2X,I4,2X,2G14.5,96X,G14.5)
    518 FORMAT (//,' PARMETER VALUES FOR THAT ITERATION:',/, &
              13(3X,A10,3X,A10,3X,A10,3X,A10,3X,A10,3X,A10,/))
    519 FORMAT (2X,I4,2X,2G14.5,96X,G14.5)
    520 FORMAT (6(2X,G11.4))
    525 FORMAT (6(A1,A12))
    530 FORMAT (6(2X,1PG11.4))
    545 FORMAT (/,' STARTING VALUES OF REGRESSION PARAMETERS :',/)
    600 FORMAT(/,' PARAMETER ESTIMATION CONVERGED BY SATISFYING TOL',A1, &
                 ' CRITERIA',/)
    !
    ALLOCATE(CSV(NPE,NPE),UU(NPE))
    ! Initialize
    CALL TYP_NULL(WTFULLPRI)
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine REG_GNMOD_UNC'
    IFAIL = 0
    ! DECIDE WHETHER TO CHECK CONVERGENCE OR MAKE AN INTERVAL ITERATION
    IF(AN_ITER) THEN
      !
      ! START FIRST ITERATION
      IF (ITERP == 1) THEN
        IF(ALLOCATED(SSELIST)) THEN
          ! move on
        ELSE
          ALLOCATE(SSELIST(MAXITER+1),SSELISTPREDLIM(MAXITER+1), &
                   SSEPVALLIST(MAXITER+1,NPS))
        ENDIF
        SSELIST = 0.D0
        SSELISTPREDLIM = 0.D0
        SSEPVALLIST = 0.D0
        AP = 1.D0
        WRITE (IOUT,545)
        WRITE (IOUT,525) (' ',PARNAM(IPTR(IP)),IP=1,NPE)
        WRITE (IOUT,'(1X)')
        WRITE (IOUT,530) (PVALINIT(IPTR(IP)),IP=1,NPE)
        WRITE (*,545)
        WRITE (*,525) (' ',PARNAM(IPTR(IP)),IP=1,NPE)
        WRITE (*,'(1X)')
        WRITE (*,530) (PVALINIT(IPTR(IP)),IP=1,NPE)
      ENDIF
      !     PRINT ITERATION HEADING
      WRITE(*,400)
      WRITE(*,401) INT(IINT*SGN),ITERP
      WRITE(IOUT,400)
      WRITE(IOUT,401) IINT,ITERP
      WRITE(IOUT,402)
      DO IP=1,NPE
        WRITE(IOUT,403)PARNAM(IPTR(IP)),ZZ(IP)
      ENDDO
      AMP = 0.D0
      AMPA=0.D0
      CSV=0.D0
      IND = 0
      IFO = 0
      SCLE=0.D0
      SGN = DBLE(ISGN)
      NP1 = NPE - 1
      WTADJF = .FALSE.
      !
      !-------ASSEMBLE LEAST-SQUARES MATRIX (C) AND GRADIENT VECTOR (G)
      !----------INITIALIZE C AND CONVERT LN PARAMETERS
      CMAT = 0.D0
      GG = 0.D0
      DO IP = 1, NPE
        IIPP = IPTR(IP)
        IF (LN(IIPP) > 0) PVAL(IIPP) = DLOG(PVAL(IIPP))
      ENDDO
      !----------CALCULATE SENSITIVITY CONTRIBUTIONS TO C AND G.
      IF (MPR > 0) THEN
        IF(ALLOCATED(RESIDSWPRI)) DEALLOCATE(RESIDSWPRI)
        IF(ALLOCATED(XSENSTWPRI)) DEALLOCATE(XSENSTWPRI)
        IF(ALLOCATED(XTW)) DEALLOCATE(XTW)
        ALLOCATE(RESIDSWPRI(NOBS+MPR),XSENSTWPRI(NPE,NOBS+MPR), &
                 XTW(NPE,NOBS+MPR))
        DO I=1,NPE
          DO J=1,NOBS
            XSENSTWPRI(I,J) = XSENST(I,J)
            RESIDSWPRI(J) = RESIDS(J)
          ENDDO
          DO J=1,MPR
            XSENSTWPRI(I,NOBS+J) = XPRI(I,J)
            RESIDSWPRI(NOBS+J) = RESIDSPRI(J)
          ENDDO
        ENDDO
        CALL UTL_COMBINESQMATRIX(WTFULL,PRIWTMAT,WTFULLPRI)
        ! Calculate XTw
        CALL UTL_MATMUL_SUB(NPE,NOBS+MPR,XSENSTWPRI,WTFULLPRI,XTW)
        ! XTwX
        CMAT = MATMUL(XTW,TRANSPOSE(XSENSTWPRI))
        ! GG = XTw(residuals)
        GG =  MATMUL(XTW,RESIDSWPRI)
        DEALLOCATE(RESIDSWPRI)
        CALL TYP_DEALLOC (WTFULLPRI)
      ELSE
        IF(ALLOCATED(XTW)) DEALLOCATE(XTW)
        ALLOCATE(XTW(NPE,NOBS))
        ! Calculate XTw
        CALL UTL_MATMUL_SUB(NPE,NOBS,XSENST,WTFULL,XTW)
        ! XTwX
        CMAT = MATMUL(XTW,TRANSPOSE(XSENST))
        ! GG = XTw(residuals)
        GG =  MATMUL(XTW,RESIDS)
      ENDIF
      !-----FOR ONE PARAMETER CASE
      IF (NPE.LT.2) THEN
      !-----CALCULATE STEP LENGTH FOR SINGLE-PARAMETER CASE
        DET = CMAT(1,1)
        SCLE(1) = 1.D0
        !-----IF MATRIX EQUATION IS SINGULAR, OR CONVERGENCE HAS BEEN REACHED:
        IF (IFO > 0) THEN
          IF (LN(IPTR(1)) > 0) PVAL(IPTR(1)) = EXP(PVAL(IPTR(1)))
          DEALLOCATE(CSV,UU)
        RETURN
        ENDIF
        DDV(1) = GG(1)/DET
      ELSE
      !-----SCALE COEFFICIENT MATRIX AND GRADIENT VECTOR
        DO IP = 1, NPE
          SCLE(IP) = 1.D0
          IF (CMAT(IP,IP) > 1.D-30) SCLE(IP) = DSQRT(CMAT(IP,IP))
        ENDDO
        DO IP = 1, NP1
          DTMPA = SCLE(IP)
          IP1 = IP + 1
          DO I = IP1, NPE
            CMAT(I,IP) = CMAT(I,IP)/(SCLE(I)*DTMPA)
            CMAT(IP,I) = CMAT(I,IP)
            CSV(IP,I)=CMAT(I,IP)
            CSV(I,IP)=CMAT(I,IP)
          ENDDO
          GG(IP) = GG(IP)/DTMPA
          CMAT(IP,IP) = 1.D0 + AMP
          ZZ(IP)=ZZ(IP)/DTMPA
        ENDDO
        GG(NPE) = GG(NPE)/SCLE(NPE)
        ZZ(NPE) = ZZ(NPE)/SCLE(NPE)
        CMAT(NPE,NPE) = 1.D0 + AMP
        !-----PRINT AS INDICATED BY IVERB
        IF (IVERB.GE.5) THEN
          WRITE (IOUT,500)
          DO J = 1, NPE
            WRITE (IOUT,530) (CMAT(I,J),I=1,NPE)
          ENDDO
          WRITE (IOUT,505)
          WRITE (IOUT,530) (GG(I),I=1,NPE)
        ENDIF
        !-----COMPUTE SCALED DISPLACEMENT COMPONENTS
        CALL REG_GNMOD_UNCSL(NPE,1,GG,CMAT,DDV,IFAIL,DET)
        IF(IFAIL > 0) THEN
        !-----IF MATRIX EQUATION IS SINGULAR
        !-------CONVERT NATURAL LOGS OF PARAMETER VALUES AND RETURN
          DO IP = 1, NPE
            IIPP = IPTR(IP)
            IF (LN(IIPP) > 0) PVAL(IIPP) = EXP(PVAL(IIPP))
          ENDDO
          !-----SINGULAR COEFFICIENT MATRIX
          IFAIL = 1
          AMESSAGE = ' LEAST SQUARES COEFFICIENT MATRIX SINGULAR '
          WRITE(*,*)AMESSAGE
          CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
          DEALLOCATE(CSV,UU)
          RETURN
        ENDIF
        ALAM=0.D0
        CALL REG_GNMOD_UNCSL(NPE,0,ZZ,CMAT,UU,IFAIL,DET)
        !COMPUTE QUADRATIC FORMS IN ALAM INVOLVING G AND Z VECTORS
        DG = 0.D0
        UZ = 0.D0
        DO J=1,NPE
          DG = DG + DDV(J)*GG(J)
          UZ = UZ + UU(J)*ZZ(J)
        ENDDO
        !COMPUTE LAMBDA
        DTMPA = DSQ - RSQALL(ITERP) + DG
        IF (DTMPA > 0.) ALAM=SGN*DSQRT(DTMPA/(UZ+WCI))
        !COMPUTE PARAMETER DISPLACEMENTS
        DO J=1,NPE
          DDV(J) = DDV(J) + ALAM*UU(J)
        ENDDO
        !-MARQUARDT LOOP
        155 DG=0.D0
        DD=0.D0
        GGG=0.D0
        DO J=1,NPE
          DTMPA=ALAM*ZZ(J)+GG(J)
          DG=DG+DDV(J)*DTMPA
          DD=DD+DDV(J)*DDV(J)
          GGG=GGG+DTMPA*DTMPA
        ENDDO
        TEST = MRQTDIRECTION*DSQRT(DD*GGG)
        IF (DG > TEST) GO TO 210
        165 AMP = MRQTFACTOR*AMP+MRQTINCREMENT
        IF (AMP > 1.D0) GO TO 210
        ! RECREATE CMAT MATRIX
        IF (NPE > 1) THEN
          DO IP=1,NP1
            IP1=IP+1
            DO I=IP1,NPE
              CMAT(I,IP) = CSV(I,IP)
              CMAT(IP,I) = CSV(IP,I)
            ENDDO
            CMAT(IP,IP)=1.D0+AMP
         ENDDO
        ENDIF
        CMAT(NPE,NPE)=1.D0+AMP
        ! COMPUTE PARAMETER STEP LENGTHS
        CALL REG_GNMOD_UNCSL(NPE,1,GG,CMAT,DDV,IFAIL,DET)
        ALAM=0.D0
        IF (IFAIL > 0) THEN
          GO TO 165
        ELSE
          CALL REG_GNMOD_UNCSL(NPE,0,ZZ,CMAT,UU,IFAIL,DET)
          ! COMPUTE QUADRATIC FORMS IN ALAM INVOLVING G AND Z VECTORS
          DG = 0.D0
          UZ = 0.D0
          DO J=1,NPE
            DG = DG + DDV(J)*GG(J)
            UZ = UZ + UU(J)*ZZ(J)
          ENDDO
          ! COMPUTE LAMBDA
          DTMPA = DSQ - RSQALL(ITERP) + DG
          IF (DTMPA > 0.D0) ALAM=SGN*DSQRT(DTMPA/(UZ+WCI))
          ! COMPUTE PARAMETER DISPLACEMENTS
          DO J=1,NPE
            DDV(J) = DDV(J) + ALAM*UU(J)
          ENDDO
        ENDIF
        GO TO 155
        !
        !IF MATRIX EQUATION IS SINGULAR,
        !--CONVERT NATURAL LOGS OF PARAMETER VALUES AND RETURN
        210 IF (IFAIL > 0) THEN
          DO I=1,NPE
            IP=IPTR(I)
            IF (LN(IP) > 0) THEN
              PVAL(IP)=EXP(PVAL(IP))
            ENDIF
          ENDDO
          DEALLOCATE(CSV,UU)
          RETURN
        ENDIF
      ENDIF
      !TO PRINT WEIGHTED RESIDUALS at limits SAVE X C SCLE U UZ FROM 1st ITER
      ! X is already saved in _init_su
      IF (ITERP == 1) THEN
        IUWRP = UTL_GETUNIT(101,150)
        OPEN(UNIT=IUWRP,FORM='UNFORMATTED',STATUS='SCRATCH')
        IF(MPR > 0) THEN
          WRITE(IUWRP) XSENSTWPRI,SCLE,UU,UZ
        ELSE
          WRITE(IUWRP) XSENST,SCLE,UU,UZ
        ENDIF
        REWIND(IUWRP)
      ENDIF
      !---UNSCALE PARAMETER CHANGE VECTOR
      DO IP = 1, NPE
        DDV(IP) = DDV(IP)/SCLE(IP)
      ENDDO
      ER=ALAM*WCI
      !-----COMPUTE DAMPING PARAMETER AND NEW ESTIMATES OF REGRESSION
      !-----PARAMETERS
      ADMX = 0.D0
      DMXO = DMX
      JJ = 0
      DO IP = 1, NPE
        IIPP = IPTR(IP)
        BBSCL=BSCAL(IIPP)
        IF (LN(IIPP)>0) THEN
          BBSCL=DLOG(BBSCL)
          IF(BBSCL == 0.D0) BBSCL = 1.D0
        ENDIF
        DTMPA = DDV(IP)/BBSCL
        DTMPB = DABS(DTMPA)
        IF (DTMPB.LT.ADMX) CYCLE
        JJ=IIPP
        DMX=DTMPA
        ADMX=DTMPB
      ENDDO
      ! COMPUTE AP USING OSCILLATION CONTROL
      IF(ITERP > 1) THEN
        SPR = DMX / (AP*DMXO)
        IF(SPR < -1.D0) THEN
          AP = 5.D-01/ABS(SPR)
        ELSE
          AP = (3.D0+SPR)/(3.D0+ABS(SPR))
        ENDIF
      ENDIF
      IF(AP*ADMX > MAXCHANGE) AP=MAXCHANGE/ADMX
      ! UPDATE PARAMETERS
      DO I=1,NPE
        IP=IPTR(I)
        DDC=AP*DDV(I)
        PVAL(IP)=PVAL(IP)+DDC
        DDV(I)=DDC
      ENDDO
      !RETRANSFORM PARAMETERS
      DO I=1,NPE
        IP = IPTR(I)
        IF (LN(IP) > 0) THEN
          PVAL(IP)=EXP(PVAL(IP))
          ! Christensen suggests possibly limiting lower bound
          !IF (PVAL(IP) < (1.D-14*PVALINIT(IP))) PVAL(IP)= 1.D-14*PVALINIT(IP)
         ENDIF
      ENDDO
      !PRINT DATA FROM CURRENT ITERATION
      SSE=RSQALL(ITERP)
      IF (ITYP == 2) SSE=SSE+ER*ER/WCI
      WRITE(IOUT,510) ITERP
      WRITE(*,510) ITERP
      IF(ITERP == 1) THEN
        SSELIST(1) = RSQALL(1)
        SSELISTPREDLIM(1) = (FL+ER)*FLCNV
        DO I=1,NPS
          SSEPVALLIST(1,I) = PVALINIT(I)
        ENDDO
      ELSE
      ENDIF
      DO I=1,NPS
        SSEPVALLIST(ITERP+1,I) = PVAL(I)
      ENDDO
      IF(ITERP == 1)WRITE(INTUNITSUM,517) 0,SSELIST(1),DSQ,(SSELISTPREDLIM(1))
      WRITE(*,515) DSQ,DET,AMP,AP,PARNAM(JJ),DMX,ALAM, &
                   ER*FLCNV,(PARNAM(IPTR(IP)),IP=1,NPE)
      WRITE(INTUNITSUM,516) DET,AMP,AP,DMX,PARNAM(JJ),ALAM,ER*FLCNV
      WRITE(IOUT,515) DSQ,DET,AMP,AP,PARNAM(JJ),DMX,ALAM, &
                    ER*FLCNV,(PARNAM(IPTR(IP)),IP=1,NPE)
      WRITE(IOUT,520) (PVAL(IPTR(IP)),IP=1,NPE)
      WRITE(*,520) (PVAL(IPTR(IP)),IP=1,NPE)
    ELSE
      SSELISTPREDLIM(ITERP+1) = (FL+ER)*FLCNV
      SSELIST(ITERP+1) = RSQALL(ITERP+1)
      WRITE(INTUNITSUM,519) ITERP,SSELIST(ITERP+1),DSQ,SSELISTPREDLIM(ITERP+1)
      !CHECK FOR CONVERGENCE
      IF (TOLY  > 1.D-12) THEN
        IF (ITERP > 1) THEN
          DTMPA=MAX(ABS(FL+ER+FL0+ERO),1.D-12)
          DTMPA=2.D0*(FL+ER-FL0-ERO)/DTMPA
          IF (DABS(DTMPA) < TOLY) THEN
            IFO=3
            WRITE(IOUT,600) 'Y'
            WRITE(*,600) 'Y'
          ENDIF
        ENDIF
        FL0=FL
        ERO=ER
      ENDIF
      IF (TOLS > 0.D0) THEN
        IF (ITERP > 2) THEN
          TMPA = (ABS(RSQALL(ITERP)-RSQALL(ITERP-1))/RSQALL(ITERP)) &
                 + (ABS(RSQALL(ITERP-1)-RSQALL(ITERP-2))/RSQALL(ITERP-1))
          IF (TMPA < TOLS) THEN
            IFO = 2
            WRITE(IOUT,600) 'S'
            WRITE(*,600) 'S'
          ENDIF
        ENDIF
      ENDIF
      IF(ADMX <= TOLP) THEN
        IFO=1
        WRITE(IOUT,600) 'P'
        WRITE(*,600) 'P'
      ENDIF
      I=INT(IINT*SGN)
      IF(IFO > 0 .OR. ITERP == MAXITER) THEN
        SSEITERP = 0
        SSECLOSEST = SSELIST(1)
        SSEDIFF = ABS(SSECLOSEST-DSQ)
        SSECLOSESTPREDLIM =  SSELISTPREDLIM(1)
        DO J=1,ITERP
          IF(SSEDIFF > ABS(SSELIST(J+1)-DSQ)) THEN
            SSEDIFF = ABS(SSELIST(J+1)-DSQ)
            SSEITERP = J
            SSECLOSEST = SSELIST(J+1)
            SSECLOSESTPREDLIM =  SSELISTPREDLIM(J+1)
          ENDIF
        ENDDO
        PCT = (((SSECLOSEST-DSQ)/DSQ)*100.D0)
        IF (ITYP .NE. 2) THEN
          IF(FIRST) THEN
            WRITE(INTUNIT,130)
            WRITE(INTUNITD,130)
            FIRST = .FALSE.
          ENDIF
          FLIM=FL
          WRITE(IOUT,100)SSEITERP,ITERP,PCT,DSQ
          WRITE(*,100)SSEITERP,ITERP,PCT,DSQ
          IF(I < 0) THEN
            IF(ITERP == MAXITER) THEN
              WRITE(IOUT,146) GNAM,I,SSECLOSESTPREDLIM
              WRITE(*,146) GNAM,I,SSECLOSESTPREDLIM
            ELSE
              WRITE(IOUT,110) GNAM,I,SSECLOSESTPREDLIM
              WRITE(*,110) GNAM,I,SSECLOSESTPREDLIM
            ENDIF
          ELSE
            IF(ITERP == MAXITER) THEN
              WRITE(IOUT,147) GNAM,I,SSECLOSESTPREDLIM
              WRITE(*,147) GNAM,I,SSECLOSESTPREDLIM
            ELSE
              WRITE(IOUT,111) GNAM,I,SSECLOSESTPREDLIM
              WRITE(*,111) GNAM,I,SSECLOSESTPREDLIM
            ENDIF
          ENDIF
          WRITE(INTUNITSUM,130)
          IF(ITERP == MAXITER) THEN
            WRITE(IOUT,145)
            WRITE(*,145)
            WRITE(INTUNIT,136) GNAM,I,GPOPT,SSECLOSESTPREDLIM,SSECLOSEST,DSQ, &
             PCT,TRIM(CHINTTYPE),ITERP
            WRITE(INTUNITD,136) GNAM,I,GPOPT,SSECLOSESTPREDLIM,SSECLOSEST,DSQ, &
             PCT,TRIM(CHINTTYPE),ITERP
            WRITE(INTUNITSUM,136) GNAM,I,GPOPT,SSECLOSESTPREDLIM,SSECLOSEST, &
               DSQ,PCT,TRIM(CHINTTYPE),ITERP
          ELSE
            WRITE(INTUNIT,135) GNAM,I,GPOPT,SSECLOSESTPREDLIM,SSECLOSEST,DSQ, &
             PCT,TRIM(CHINTTYPE),ITERP
            WRITE(INTUNITD,135) GNAM,I,GPOPT,SSECLOSESTPREDLIM,SSECLOSEST,DSQ, &
             PCT,TRIM(CHINTTYPE),ITERP
            WRITE(INTUNITSUM,135) GNAM,I,GPOPT,SSECLOSESTPREDLIM,SSECLOSEST, &
               DSQ,PCT,TRIM(CHINTTYPE),ITERP
          ENDIF
          WRITE(INTUNITSUM,100)SSEITERP,ITERP,PCT,DSQ
          WRITE(IOUT,518) (PARNAM(IPTR(IP)),IP=1,NPE)
          WRITE(IOUT,520) (SSEPVALLIST(SSEITERP+1,IPTR(IP)),IP=1,NPE)
          WRITE(*,518) (PARNAM(IPTR(IP)),IP=1,NPE)
          WRITE(*,520) (SSEPVALLIST(SSEITERP+1,IPTR(IP)),IP=1,NPE)
          IF(ITERP == MAXITER) THEN
            WRITE(INTUNITSUM,102)
          ELSE
            WRITE(INTUNITSUM,101)
          ENDIF
        ELSE
          IF(FIRST) THEN
            WRITE(INTUNIT,140)
            WRITE(INTUNITD,140)
            FIRST = .FALSE.
          ENDIF
          FLIM=FL+ER
          WRITE(IOUT,100)SSEITERP,ITERP,PCT,DSQ
          WRITE(*,100)SSEITERP,ITERP,PCT,DSQ
          IF(I < 0) THEN
            IF(ITERP == MAXITER) THEN
              WRITE(IOUT,146) GNAM,I,SSECLOSESTPREDLIM
              WRITE(*,146) GNAM,I,SSECLOSESTPREDLIM
            ELSE
              WRITE(IOUT,120) GNAM,I,SSECLOSESTPREDLIM
              WRITE(*,120) GNAM,I,SSECLOSESTPREDLIM
            ENDIF
          ELSE
            IF(ITERP == MAXITER) THEN
              WRITE(IOUT,147) GNAM,I,SSECLOSESTPREDLIM
              WRITE(*,147) GNAM,I,SSECLOSESTPREDLIM
            ELSE
              WRITE(IOUT,121) GNAM,I,SSECLOSESTPREDLIM
              WRITE(*,121) GNAM,I,SSECLOSESTPREDLIM
            ENDIF
          ENDIF
          WRITE(INTUNITSUM,140)
          IF(ITERP == MAXITER) THEN
            WRITE(IOUT,145)
            WRITE(*,145)
            WRITE(INTUNITSUM,145)
            WRITE(INTUNIT,136) GNAM,I,GPOPT,SSECLOSESTPREDLIM,SSECLOSEST,DSQ, &
             PCT,TRIM(CHINTTYPE),ITERP
            WRITE(INTUNITD,136) GNAM,I,GPOPT,SSECLOSESTPREDLIM,SSECLOSEST,DSQ, &
             PCT,TRIM(CHINTTYPE),ITERP
            WRITE(INTUNITSUM,136) GNAM,I,GPOPT,SSECLOSESTPREDLIM,SSECLOSEST,DSQ,&
               PCT,TRIM(CHINTTYPE),ITERP
          ELSE
            WRITE(INTUNIT,135) GNAM,I,GPOPT,SSECLOSESTPREDLIM,SSECLOSEST,DSQ, &
             PCT,TRIM(CHINTTYPE),ITERP
            WRITE(INTUNITD,135) GNAM,I,GPOPT,SSECLOSESTPREDLIM,SSECLOSEST,DSQ, &
             PCT,TRIM(CHINTTYPE),ITERP
            WRITE(INTUNITSUM,135) GNAM,I,GPOPT,SSECLOSESTPREDLIM,SSECLOSEST,DSQ,&
               PCT,TRIM(CHINTTYPE),ITERP
          ENDIF
          WRITE(INTUNITSUM,100)SSEITERP,ITERP,PCT,DSQ
          WRITE(IOUT,518) (PARNAM(IPTR(IP)),IP=1,NPE)
          IF(ITERP == MAXITER) THEN
            WRITE(INTUNITSUM,102)
          ELSE
            WRITE(INTUNITSUM,101)
          ENDIF
        ENDIF
        ROWNUM(CNTINT) = I
        DO N=1,NPE
          STORINTPAR(N,CNTINT) = SSEPVALLIST(SSEITERP+1,IPTR(N))
        ENDDO
      ENDIF
    ENDIF
    DEALLOCATE(CSV,UU)
    RETURN
  END SUBROUTINE REG_GNMOD_UNC
!===============================================================================
  SUBROUTINE REG_GNMOD_UNCSL(NPE,IND,G,C,D,IFAIL,DET)
  !-MODIFIED SUNC1NLI1SL VERSION 1001 25FEB2003 (SC) by EPP 15AUG2005
  ! ******************************************************************
  !     SOLVE SYSTEM OF LINEAR EQUATIONS BY LU-DECOMPOSITION
  ! ******************************************************************
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------
  IMPLICIT NONE
  INTEGER,                     INTENT(IN)    :: NPE
  INTEGER,                     INTENT(IN)    :: IND
  DOUBLE PRECISION,            INTENT(IN)    :: G(NPE)
  DOUBLE PRECISION,            INTENT(INOUT) :: C(NPE,NPE)
  DOUBLE PRECISION,            INTENT(INOUT) :: D(NPE)
  DOUBLE PRECISION,            INTENT(INOUT) :: DET
  INTEGER,                     INTENT(INOUT) :: IFAIL
  ! Local Variables
  DOUBLE PRECISION :: DPIV
  DOUBLE PRECISION :: DSUM
  DOUBLE PRECISION :: DTMPA
  INTEGER          :: NM1
  INTEGER          :: K
  INTEGER          :: KP1
  INTEGER          :: J
  INTEGER          :: I
  INTEGER          :: IP1
  ! ------------------------------------------------------------------
  !     COMPUTE TRIAL PARAMETER STEP LENGTHS USING LDU FACTORIZATION:
  !       DECOMPOSE MATRIX
  NM1=NPE-1
  IFAIL=0
  !
  !-------LDU FACTORIZATION
  IF (IND > 0) THEN
    DET=1.
    DO K=1,NM1
      DPIV=C(K,K)
      DET=DET*DPIV
      IF (DPIV.LT.1.D-13) THEN
        IFAIL=1
        RETURN
      ENDIF
      DPIV=1.D00/DPIV
      KP1=K+1
      DO J=KP1,NPE
        DTMPA=C(J,K)*DPIV
        DO I=J,NPE
          C(I,J)=C(I,J)-DTMPA*C(I,K)
        ENDDO
      ENDDO
      C(K,K)=DPIV
    ENDDO
    DET=DET*C(NPE,NPE)
    IF(C(NPE,NPE).LT.1.D-13) THEN
      IFAIL=1
      RETURN
    ENDIF
  ENDIF
  !
  !------INITIALIZE D
  DO K=1,NPE
    D(K)=G(K)
  ENDDO
  !
  !------FORWARD SUBSTITUTE
  DO K=1,NM1
    DTMPA=D(K)*C(K,K)
    KP1=K+1
    DO J=KP1,NPE
      D(J)=D(J)-C(J,K)*DTMPA
    ENDDO
  ENDDO
  !
  !------BACKWARD SUBSTITUTE
  D(NPE)=D(NPE)/C(NPE,NPE)
  DO I=NPE-1,1,-1
    IP1=I+1
    DSUM=0.
    DO J=IP1,NPE
      DSUM=DSUM+C(J,I)*D(J)
    ENDDO
    D(I)=(D(I)-DSUM)*C(I,I)
  ENDDO
  !
  RETURN
  END SUBROUTINE REG_GNMOD_UNCSL
!===============================================================================
!===============================================================================
  SUBROUTINE REG_GNMOD_EVA_MLOFSTATS (IFAIL,IOMIT,IOUT,MPR,NOBS,DTLAN,DTLAP, &
             DTPRIWT,LNDETFN,LNDETFP,RSQ,RSQALL1,AICCN,BICN, &
             HQN,KASHYAPN,KASHYAPP,MLOFD,MLOFDP)
    !     VERSION 20031109 EPP
    !-----MODIFIED FROM MF2K SOBS1BAS6ML VERSION 1000 01FEB1992
    !******************************************************************
    !     CALCULATE STATISTICS BASED ON THE MAXIMUM LIKELIHOOD OBJECTIVE
    !     FUNCTION
    !******************************************************************
    !        SPECIFICATIONS:
    !------------------------------------------------------------------
    IMPLICIT NONE
    ! Argument list variables
    INTEGER,                    INTENT(INOUT) :: IFAIL
    INTEGER,                    INTENT(IN)    :: IOMIT
    INTEGER,                    INTENT(IN)    :: IOUT
    INTEGER,                    INTENT(IN)    :: MPR
    INTEGER,                    INTENT(IN)    :: NOBS
    DOUBLE PRECISION,           INTENT(IN)    :: DTLAN ! ln|XTwX obs|
    DOUBLE PRECISION,           INTENT(IN)    :: DTLAP ! ln|XTwX wpri|
    DOUBLE PRECISION,           INTENT(IN)    :: DTPRIWT
    DOUBLE PRECISION,           INTENT(IN)    :: LNDETFN ! ln|F obs| |
    DOUBLE PRECISION,           INTENT(IN)    :: LNDETFP !ln|F wpri|
    DOUBLE PRECISION,           INTENT(IN)    :: RSQ
    DOUBLE PRECISION,           INTENT(IN)    :: RSQALL1
    DOUBLE PRECISION,           INTENT(OUT)   :: AICCN
    DOUBLE PRECISION,           INTENT(OUT)   :: BICN
    DOUBLE PRECISION,           INTENT(OUT)   :: HQN
    DOUBLE PRECISION,           INTENT(OUT)   :: KASHYAPN
    DOUBLE PRECISION,           INTENT(OUT)   :: KASHYAPP
    DOUBLE PRECISION,           INTENT(OUT)   :: MLOFD
    DOUBLE PRECISION,           INTENT(OUT)   :: MLOFDP
    !LOCAL
    DOUBLE PRECISION RNDD, RND, RNPE, RNPE1
    DOUBLE PRECISION AICN, AICNP, AICCNP, BICNP
    !------------------------------------------------------------------
    500 FORMAT &
     (//,2X,'STATISTICS FOR EVALUATING ALTERNATIVE MODELS:',//, &
      4X,'MAX LIKE OBJ FUNC OBSERVATIONS ONLY (MLOF)--------------- = ',G11.5,/, &
      4X,'MAX LIKE OBJ FUNC OBSERVATIONS and PRIOR (MLOFOP)-------- = ',G11.5,/, &
      4X,'LN DETERMINANT of Fisher Information Matrix (OBS ONLY)--- = ',G11.5,/, &
      4X,'LN DETERMINANT of Fisher Information Matrix (OBS & Prior) = ',G11.5,//, &
      4X,'MODEL EVALUATION MEASURES: ',//, &
      6X,'OBSERVATIONS ONLY: ',/, &
      6X,'AIC CRITERION-------------  = ',G11.5,/, &
      6X,'AICc CRITERION------------  = ',G11.5,/, &
      6X,'BIC CRITERION-------------  = ',G11.5,/, &
      6X,'KASHYAP CRITERION---------  = ',G11.5,//, &
      6X,'OBSERVATIONS and PRIOR: ',/, &
      6X,'AIC CRITERION-------------  = ',G11.5,/, &
      6X,'AICc CRITERION------------  = ',G11.5,/, &
      6X,'BIC CRITERION-------------  = ',G11.5,/, &
      6X,'KASHYAP CRITERION---------  = ',G11.5)
    501 FORMAT &
     (//,2X,'STATISTICS FOR EVALUATING ALTERNATIVE MODELS:',//, &
      4X,'No prior used in this parameter estimation',/, &
      4X,'MAX LIKE OBJ FUNC OBSERVATIONS ONLY (MLOFO)----------- = ',G11.5,/, &
      4X,'LN DETERMINANT of Fisher Information Matrix (OBS ONLY) = ',G11.5,//, &
      4X,'MODEL EVALUATION MEASURES: ',//, &
      6X,'OBSERVATIONS ONLY (no prior used in this parameter estimation)',/, &
      6X,'AIC CRITERION-------------  = ',G11.5,/, &
      6X,'AICc CRITERION------------  = ',G11.5,/, &
      6X,'BIC CRITERION-------------  = ',G11.5,/, &
      6X,'KASHYAP CRITERION---------  = ',G11.5)
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine REG_GNMOD_EVA_MLOFSTATS'
    IFAIL = 0
    !
    RND = REAL(NOBS-IOMIT) + REAL(MPR)
    RNDD = REAL(NOBS-IOMIT)
    RNPE = REAL(NPERD)
    RNPE1 = REAL(NPERD) + 1.D0
    !-----LOG-LIKELIHOOD FUNCTION (DEP only Then Dep+Prior)
    MLOFD = RNDD * DLOG(RSQ/RNDD)
    MLOFDP = RND * DLOG(RSQALL1/RND)
    !-----AIC BIC HQ KASHYAP NSIG DEP ONLY
    AICN = MLOFD + (2.0*RNPE1)
    AICCN = MLOFD + (2.0*RNPE1+((2.0*RNPE1*(RNPE1+1))/((RNDD)-RNPE1-1.0)))
    BICN = MLOFD + (RNPE1*DLOG(RNDD))
    HQN = MLOFD + (2.D0 * RNPE1 * DLOG(DLOG(RNDD)))
    KASHYAPN = ((RNDD-RNPE)*DLOG(RSQ/RNDD)) &
               - (RNPE * DLOG(2.D0*3.14159265)) + DTLAN
    !-----AIC BIC HQ KASHYAP NSIG DEP and PRIOR
    AICNP = MLOFDP + (2.0*RNPE1)
    AICCNP = MLOFDP + (2.0*RNPE1+((2.0*RNPE1*(RNPE1+1))/((RND)-RNPE1-1.0)))
    BICNP = MLOFDP + (RNPE1*DLOG(RND))
    KASHYAPP = ((RND-RNPE)*DLOG(RSQALL1/RND)) &
                - (RNPE * DLOG(2.D0*3.14159265)) + DTLAP
    IF (IVERB.GE.5) THEN
      WRITE(IOUT,*)
      WRITE(IOUT,*)' KIC TERMS printed because VERBOSE >= 5 ' ! ***** KIC terms
      WRITE(IOUT,*)' N obsonly        ',RNDD ! N is RNDD
      WRITE(IOUT,*)' K-1 obsonly      ',RNPE ! K is RNPE
      WRITE(IOUT,*)' SWSR obsonly     ',RSQ ! SWSR is RSQ
      WRITE(IOUT,*)' ln(2pi)          ',DLOG(2.D0*3.14159265) ! ln(2pi) is DLOG(2.D0*3.14159265)
      WRITE(IOUT,*)' ln|XTWX| obsonly ',DTLAN ! ln|XTWX| is DTLAN
      WRITE(IOUT,*)' KIC obsonly      ',KASHYAPN ! KIC = ((N-K)*ln(SWSR/N))-(K*ln(2pi))+ln|XTWX|  is  KASHYAPN
      WRITE(IOUT,*)' That value is printed in _dm as --- KASHYAP (MLOFD + KASHYAP PENALTY):'
      WRITE(IOUT,*)' mma reads that value and prints it as KIC in _mma'
      WRITE(IOUT,*)
    ENDIF
    IF(MPR > 0) THEN
      WRITE (IOUT,500) MLOFD, MLOFDP, &
                       LNDETFN, LNDETFP, &
                       AICN, AICCN, BICN, KASHYAPN, &
                       AICNP, AICCNP, BICNP, KASHYAPP
    ELSE
      WRITE (IOUT,501) MLOFD, &
                       LNDETFN, &
                       AICN, AICCN, BICN, KASHYAPN
    ENDIF
    RETURN
  END SUBROUTINE REG_GNMOD_EVA_MLOFSTATS
!===============================================================================
!===============================================================================
  SUBROUTINE REG_GNMOD_UEV_WRITE_PARAM_STATS &
             (IFAIL,DATAEXCHANGE,IOUT,ITERP,MPR,NOBS,NPS, &
              NPTR,LN,OUTNAM,STDEV,PARNAM,PVALMAX,PVALMIN,PVALINIT,PVAL)
    ! VERSION 20031109 EPP
    ! MODIFIED FROM MF2K SPES1BAS6WS VERSION 19980925 ERB
    !******************************************************************
    !     WRITE PARAMETER STATISTICS
    !******************************************************************
    !        SPECIFICATIONS:
    !------------------------------------------------------------------
    USE UTILITIES
    IMPLICIT NONE
    ! Argument list variables
    INTEGER,                         INTENT(INOUT) :: IFAIL
    LOGICAL,                         INTENT(IN) :: DATAEXCHANGE
    INTEGER,                         INTENT(IN) :: IOUT
    INTEGER,                         INTENT(IN) :: ITERP
    INTEGER,                         INTENT(IN) :: MPR
    INTEGER,                         INTENT(IN) :: NOBS
    INTEGER,                         INTENT(IN) :: NPS
    INTEGER,                         INTENT(IN) :: NPTR(NPERD)
    INTEGER,                         INTENT(IN) :: LN(NPS)
    CHARACTER(LEN=MAX_STRING_LEN),   INTENT(IN) :: OUTNAM
    DOUBLE PRECISION,                INTENT(IN) :: STDEV(NPERD)
    CHARACTER(LEN=12),               INTENT(IN) :: PARNAM(NPS)
    DOUBLE PRECISION,                INTENT(IN) :: PVALMAX(NPS)
    DOUBLE PRECISION,                INTENT(IN) :: PVALMIN(NPS)
    DOUBLE PRECISION,                INTENT(IN) :: PVALINIT(NPS)
    DOUBLE PRECISION,                INTENT(INOUT) :: PVAL(NPS)
    !LOCAL
    DOUBLE PRECISION                               :: BB
    DOUBLE PRECISION                               :: BBU
    DOUBLE PRECISION                               :: BBL
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: BUF3
    INTEGER                                        :: ICHECK
    INTEGER                                        :: IEXPFLG
    INTEGER                                        :: IFLAG
    INTEGER                                        :: IL
    INTEGER                                        :: IIPP
    INTEGER                                        :: IP
    INTEGER                                        :: JJ
    INTEGER                                        :: JPT1
    INTEGER                                        :: JPT2
    INTEGER                                        :: JTOP
    CHARACTER(LEN=1)                               :: LID(5)
    CHARACTER(LEN=80)                              :: LINE
    INTEGER                                        :: LNFLAG
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: PRNT
    DOUBLE PRECISION                               :: TEMP
    DOUBLE PRECISION                               :: TST
    !------------------------------------------------------------------
    533 FORMAT (/,2(' ----------------------------------------', &
        '--------------------------------',/))
    534 FORMAT (' ________________________________________',     &
        '________________________________',/)
    535 FORMAT (//,' _________________',//,' PARAMETER SUMMARY'/,&
        ' _________________',/)
    536 FORMAT (' PARAMETER VALUES IN "REGRESSION" SPACE --- ',  &
        'LOG TRANSFORMED AS APPLICABLE')
    537 FORMAT (' PHYSICAL PARAMETER VALUES --- ',               &
        'NONE OF THE PARAMETERS IS LOG TRANSFORMED')
    538 FORMAT (' PHYSICAL PARAMETER VALUES --- ',               &
        'EXP10 OF LOG TRANSFORMED PARAMETERS')
    540 FORMAT (' PARAMETER:',7X,5(1x,A10))
    541 FORMAT (' * = LOG TRNS:',8X,A1,4(10X,A1))
    560 FORMAT (' COEF. OF VAR. (STD. DEV. / FINAL VALUE); "--"',&
        ' IF FINAL VALUE = 0.0')
    562 FORMAT (A)
    564 FORMAT (' UPPER 95% C.I.  ',1x,1PE10.2,1x,1PE10.2,       &
        1x,1PE10.2,1x,1PE10.2,1x,1PE10.2)
    566 FORMAT (' LOWER 95% C.I.  ',1x,1PE10.2,1x,1PE10.2,       &
        1x,1PE10.2,1x,1PE10.2,1x,1PE10.2)
    567 FORMAT ('    UPPER LIMIT  ',1x,1PE10.2,1x,1PE10.2,       &
        1x,1PE10.2,1x,1PE10.2,1x,1PE10.2)
    568 FORMAT (' FINAL VALUES    ',1x,1PE10.2,1x,1PE10.2,       &
        1x,1PE10.2,1x,1PE10.2,1x,1PE10.2)
    569 FORMAT ('    LOWER LIMIT  ',1x,1PE10.2,1x,1PE10.2,      &
        1x,1PE10.2,1x,1PE10.2,1x,1PE10.2)
    571 FORMAT (/,5X,'!!!!!STATISTICS WERE CALCULATED FOR STARTING VALUES!!!!!')
    585 FORMAT (1X)
    598 FORMAT (' STD. DEV.       ',1x,1PE10.2,1x,1PE10.2,      &
        1x,1PE10.2,1x,1PE10.2,1x,1PE10.2)
    620 FORMAT ('     REASONABLE')
    625 FORMAT (' ESTIMATE ABOVE (1)')
    630 FORMAT (' ENTIRE CONF. INT.')
    635 FORMAT (                                                &
        ' SOME PARAMETER VALUES ARE OUTSIDE THEIR USER-SPECIFIED',           &
        ' REASONABLE',/,                                                     &
        ' RANGES TO A STATISTICALLY SIGNIFICANT EXTENT, BASED ON',           &
        ' LINEAR THEORY.',/,                                                 &
        ' THIS IMPLIES THAT THERE ARE PROBLEMS WITH THE OBSERVATIONS,',      &
        ' THE MODEL',/,                                                      &
        ' DOES NOT ADEQUATELY REPRESENT THE PHYSICAL SYSTEM, THE DATA',      &
        ' ARE NOT',/,                                                        &
        ' CONSISTENT WITH THEIR SIMULATED EQUIVALENTS, OR THE SPECIFIED',    &
        ' MINIMUM',/,                                                        &
        ' AND/OR MAXIMUM ARE NOT REASONABLE.  CHECK YOUR DATA,',             &
        ' CONCEPTUAL MODEL,',/,' AND MODEL DESIGN.')
    645 FORMAT (' BELOW(-1)LIMITS',I9,5I11)
    646 FORMAT (' ABOVE(1)BELOW(-1)',I7,5I11)
    660 FORMAT (/,                                                           &
        ' *** WARNING: ONE OR MORE CONFIDENCE LIMIT(S) FOR LOG-',            &
        'TRANSFORMED PARAMETER(S)',/,                                        &
        '              ARTIFICIALLY LIMITED TO 0.99E29 TO AVOID NUMERIC',    &
        ' OVERFL0W ***')
    !
    ALLOCATE(BUF3(NPERD,3))
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine REG_GNMOD_UEV_WRITE_PARAM_STATS'
    IFAIL = 0
    ALLOCATE(PRNT(NPERD,10))
    PRNT = 0.D0
    !
    !-------PRINT FINAL PARAMETER VALUES, STD. DEV., COEFFICIENTS OF
    !-------VARIATION, AND PARAMETER 95-PERCENT LINEAR CONFIDENCE INTERVALS
    !---------BUF3(IP,3) IS LOG10(B) AND THEN THE PHYSICAL PARAMETER VALUES
    !---------BUF3(IP,1) IS THE PHYSICAL PARAMETER VALUES, THEN  THE
    !---------           STANDARD DEVIATION OF LOG10(B), AND THEN
    !---------           THE LOWER LIMIT OF THE CONFIDENCE INTERVAL OF LOG10(B)
    !---------BUF3(10,2) IS THE UPPER CONFIDENCE INTERVAL OF LOG10(B)
    !---------LOG TRANSFORM
    LNFLAG = 0
    IEXPFLG = 0
    !-----PARAMETER SUMMARY CALCULATIONS
    DO IP = 1, NPERD
      IIPP = NPTR(IP)
      BUF3(IP,3)=PVAL(IIPP)
      PRNT(IP,1) = BUF3(IP,3)
      IF (LN(IIPP) > 0) THEN
        LNFLAG = 1
        BUF3(IP,3)=DLOG10(PVAL(IIPP))
        PRNT(IP,1) = BUF3(IP,3)
        PVAL(IIPP) = DLOG(PVAL(IIPP))
      ENDIF
    ENDDO
    !---------PHYSICAL PARAMETER VALUES FOR LOG-TRANSFORMED PARAMETERS
    DO IP = 1, NPERD
      IIPP = NPTR(IP)
      BUF3(IP,1) = PVAL(IIPP)
      IF (LN(IIPP) > 0) BUF3(IP,1) = EXP(PVAL(IIPP))
      PRNT(IP,2) = BUF3(IP,1)
    ENDDO
    !---------PUT STANDARD DEVIATIONS IN BUF3 ARRAY
    DO IP = 1, NPERD
      BUF3(IP,1) = STDEV(IP)
      ! if transformed convert to log10 from ln
      IF(LN(NPTR(IP)) > 0) BUF3(IP,1)=BUF3(IP,1)/2.30258509299405D0
      PRNT(IP,3) = BUF3(IP,1)
      PRNT(IP,10) = PRNT(IP,3)
    ENDDO
    !---------COMPUTE  CONFIDENCE LIMITS AND PRINT
    ! change April 2012 1.026 add following for native SD
    IF (LNFLAG > 0) THEN
      DO IP = 1, NPERD
        IIPP = NPTR(IP)
        IF (LN(IIPP) > 0) THEN
          PRNT(IP,10) = SQRT( &
          (10.**((2.30258509299405D0*PRNT(IP,3)*PRNT(IP,3))+(2.*PRNT(IP,1))))* &
          ((10.**(2.30258509299405D0*PRNT(IP,3)*PRNT(IP,3)))-1.))
        ENDIF
      ENDDO
    ENDIF
    !---------COMPUTE UPPER CONFIDENCE LIMITS AND PRINT
    CALL UTL_STUD_T (NOBS+MPR-NPERD, TST)
    DO IP = 1, NPERD
      BUF3(IP,2) = BUF3(IP,3) + TST*BUF3(IP,1)
      PRNT(IP,4) = BUF3(IP,2)
      PRNT(IP,5) = PRNT(IP,4)
!      IF (LN(NPTR(IP)) > 0) PRNT(IP,5) = PRNT(IP,2) + TST*PRNT(IP,10)
      IF (LN(NPTR(IP)) > 0) PRNT(IP,5) = 10.D0**(PRNT(IP,4))
    ENDDO
    !---------COMPUTE LOWER CONFIDENCE LIMITS AND PRINT
    DO IP = 1, NPERD
      BUF3(IP,1) = BUF3(IP,3) - TST*BUF3(IP,1)
      PRNT(IP,6) = BUF3(IP,1)
      PRNT(IP,7) = PRNT(IP,6)
!      IF (LN(NPTR(IP)) > 0) PRNT(IP,7) = PRNT(IP,2) - TST*PRNT(IP,10)
      IF (LN(NPTR(IP)) > 0) PRNT(IP,7) = 10.D0**(PRNT(IP,6))
    ENDDO
    !
    !---------SAVE PHYSICAL PARAMETER VALUES
    DO IP = 1, NPERD
      IIPP = NPTR(IP)
      BUF3(IP,3) = PVAL(IIPP)
      IF (LN(IIPP) > 0) BUF3(IP,3) = EXP(PVAL(IIPP))
    ENDDO
    !--------COMPARE ESTIMATES AND CONFIDENCE INTERVALS TO UPPER
    !--------AND LOWER REASONABLE PARAMETER VALUES
    IFLAG = 0
    DO IP = 1, NPERD
      IIPP = NPTR(IP)
      BB = PRNT(IP,1)
      IF (LN(IIPP) > 0) BB = PRNT(IP,2)
      BBU = PRNT(IP,5)
      BBL = PRNT(IP,7)
      PRNT(IP,8) = 0.
      PRNT(IP,9) = 0.
      IF (BB > PVALMAX(IIPP)) PRNT(IP,8) = 1.
      IF (BB.LT.PVALMIN(IIPP)) PRNT(IP,8) = -1.
      IF (BBU.LE.PVALMIN(IIPP)) PRNT(IP,9) = -1.
      IF (BBL.GE.PVALMAX(IIPP)) PRNT(IP,9) = 1.
      IF (BBU.LE.PVALMIN(IIPP) .OR. BBL.GE.PVALMAX(IIPP)) IFLAG = 1
    ENDDO
    !-----PRINT PARAMETER SUMMARY
    WRITE (IOUT,535)
    IF(ITERP <= 1) THEN
      DO ICHECK=1,NPS
        IF(PVAL(ICHECK) .NE. PVALINIT(ICHECK)) EXIT
        IF(ICHECK .EQ. NPS)WRITE(IOUT,571)
      ENDDO
    ENDIF
    DO JJ = 1,NPERD,5
      JTOP = JJ + 4
      IF (JTOP > NPERD) JTOP = NPERD
      DO IP = JJ,JTOP
        IL = IP-JJ+1
        IIPP = NPTR(IP)
        IF (LN(IIPP) > 0) LID(IL) = '*'
        IF (LN(IIPP).LE.0) LID(IL) = ' '
      ENDDO
    !-------PRINT HEADER
      WRITE(IOUT,534)
      IF (LNFLAG.EQ.1) WRITE (IOUT,536)
      IF (LNFLAG.EQ.0) WRITE (IOUT,537)
      WRITE (IOUT,534)
      WRITE (IOUT,540) (PARNAM(NPTR(IP)),IP=JJ,JTOP)
      WRITE (IOUT,541) (LID(IP-JJ+1),IP=JJ,JTOP)
      WRITE (IOUT,585)
    !-------PRINT UPPER CONFIDENCE LIMITS
      WRITE (IOUT,585)
      WRITE (IOUT,564) (PRNT(IP,4),IP=JJ,JTOP)
    !-------PRINT ESTIMATED PARAMETERS
      WRITE (IOUT,568) (PRNT(IP,1),IP=JJ,JTOP)
    !-------PRINT LOWER CONFIDENCE LIMITS
      WRITE (IOUT,566) (PRNT(IP,6),IP=JJ,JTOP)
    !-------PRINT STANDARD DEVIATIONS
      WRITE (IOUT,585)
      WRITE (IOUT,598) (PRNT(IP,3),IP=JJ,JTOP)
    !-------PRINT COEFFICIENTS OF VARIATION
      WRITE (IOUT,585)
      WRITE (IOUT,560)
      LINE = ' '
      JPT1 = 19
      DO IP = JJ,JTOP
        JPT2 = JPT1 + 9
        IF (PRNT(IP,1).NE.0.0) THEN
          TEMP = PRNT(IP,3)/ABS(PRNT(IP,1))
          WRITE (LINE(JPT1:JPT2),'(1PE10.2)') TEMP
        ELSE
          WRITE (LINE(JPT1:JPT2),'(4X,A2,4X)') '--'
        ENDIF
        JPT1 = JPT1 + 11
      ENDDO
      WRITE (IOUT,562) LINE
      IF (LNFLAG.EQ.0) THEN
        WRITE (IOUT,585)
        WRITE (IOUT,620)
        WRITE (IOUT,567) (PVALMAX(NPTR(IP)),IP=JJ,JTOP)
        WRITE (IOUT,620)
        WRITE (IOUT,569) (PVALMIN(NPTR(IP)),IP=JJ,JTOP)
        WRITE (IOUT,585)
        WRITE (IOUT,625)
        WRITE (IOUT,645) (INT(PRNT(IP,8)),IP=JJ,JTOP)
        WRITE (IOUT,630)
        WRITE (IOUT,646) (INT(PRNT(IP,9)),IP=JJ,JTOP)
        WRITE (IOUT,585)
      ENDIF
    ENDDO
    !
    !-----IF THERE ARE LOG-TRANSFORMED PARAMETERS, PRINT SUMMARY STATISTICS
    !     FOR THE PHYSICAL PARAMETER VALUES
    IF (LNFLAG.NE.0) THEN
      WRITE (IOUT,533)
      DO JJ = 1,NPERD,5
        JTOP = JJ+4
        IF (JTOP > NPERD) JTOP=NPERD
        DO IP=JJ,JTOP
          IL = IP-JJ+1
          IIPP = NPTR(IP)
          IF (LN(IIPP) > 0) LID(IL) = '*'
          IF (LN(IIPP).LE.0) LID(IL) = ' '
        ENDDO
        WRITE (IOUT,534)
        WRITE (IOUT,538)
        WRITE (IOUT,534)
        WRITE (IOUT,540) (PARNAM(NPTR(IP)),IP=JJ,JTOP)
        WRITE (IOUT,541) (LID(IP-JJ+1),IP=JJ,JTOP)
        WRITE (IOUT,585)
        WRITE (IOUT,564) (PRNT(IP,5),IP=JJ,JTOP)
        WRITE (IOUT,568) (PRNT(IP,2),IP=JJ,JTOP)
        WRITE (IOUT,566) (PRNT(IP,7),IP=JJ,JTOP)
        WRITE (IOUT,585)
        WRITE (IOUT,620)
        WRITE (IOUT,567) (PVALMAX(NPTR(IP)),IP=JJ,JTOP)
        WRITE (IOUT,620)
        WRITE (IOUT,569) (PVALMIN(NPTR(IP)),IP=JJ,JTOP)
        WRITE (IOUT,585)
        WRITE (IOUT,625)
        WRITE (IOUT,645) (INT(PRNT(IP,8)),IP=JJ,JTOP)
        WRITE (IOUT,630)
        WRITE (IOUT,646) (INT(PRNT(IP,9)),IP=JJ,JTOP)
      ENDDO
      IF (IEXPFLG.EQ.1) WRITE (IOUT,660)
      WRITE (IOUT,585)
    ENDIF
    IF (DATAEXCHANGE) THEN
      ! WRITE OPTIMAL PARAMETER STATISTICS _pc
      CALL UTLUCODE_DX_WRITE_PC(NPERD,NPS,NPTR,LN,OUTNAM,PARNAM,PRNT, &
                               PVALMAX,PVALMIN)
    ENDIF
    IF (IFLAG.EQ.1) THEN
      WRITE (IOUT,635)
      WRITE (IOUT,585)
    ENDIF
    !
    IF (ALLOCATED(PRNT)) DEALLOCATE(PRNT)
    DEALLOCATE(BUF3)
    RETURN
  END SUBROUTINE REG_GNMOD_UEV_WRITE_PARAM_STATS
!===============================================================================
!===============================================================================
  SUBROUTINE REG_GNMOD_UEV_CORRELCOEF (IFAIL,IOUT,MPR,NDEROBS,NDP,NPS,NOBS,LN, &
                                    MODELVAL,OBSVAL,PARNAMLC,PRIVAL, &
                                    PRIWTMATSQR,PVAL,WTFULLSQR,RESIDSPRI,R1,R2)
    !     VERSION 20031109 EPP
    !     MODIFIED FROM MF2K SOBS1BAS6CC VERSION 20000201 ERB
    !******************************************************************
    !     CALCULATE CORRELATION COEFFICIENT
    !******************************************************************
    !        SPECIFICATIONS:
    !------------------------------------------------------------------
    IMPLICIT NONE
    ! Argument list variables
    INTEGER,                         INTENT(INOUT)    :: IFAIL
    INTEGER,                         INTENT(IN)    :: IOUT
    INTEGER,                         INTENT(IN)    :: MPR
    INTEGER,                         INTENT(IN)    :: NDEROBS
    INTEGER,                         INTENT(IN)    :: NDP
    INTEGER,                         INTENT(IN)    :: NPS
    INTEGER,                         INTENT(IN)    :: NOBS
    INTEGER,                         INTENT(IN)    :: LN(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: MODELVAL(NOBS)
    DOUBLE PRECISION,                INTENT(IN)    :: OBSVAL(NOBS)
    CHARACTER(LEN=12),               INTENT(IN)    :: PARNAMLC(NPS) !Parlower
    DOUBLE PRECISION,                INTENT(IN)    :: PRIVAL(MPR)
    TYPE (CDMATRIX),                 INTENT(IN)    :: PRIWTMATSQR
    DOUBLE PRECISION,                INTENT(INOUT) :: PVAL(NPS)
    TYPE (CDMATRIX),                 INTENT(IN)    :: WTFULLSQR
    DOUBLE PRECISION,                INTENT(INOUT) :: RESIDSPRI(MPR)
    DOUBLE PRECISION,                INTENT(OUT)   :: R1
    DOUBLE PRECISION,                INTENT(OUT)   :: R2
    !LOCAL
    DOUBLE PRECISION                            :: DENOM
    DOUBLE PRECISION                            :: DENOM1
    DOUBLE PRECISION                            :: DENOM2
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: EXTENDEDMODELVAL
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: EXTENDEDOBSVAL
    INTEGER                                     :: I
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: MYO
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: MYS
    INTEGER                                     :: N
    INTEGER                                     :: NLOOP
    INTEGER                                     :: NN
    DOUBLE PRECISION                            :: NUMER
    DOUBLE PRECISION                            :: R(2)
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: WOSQR
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: WSSQR
    DOUBLE PRECISION                            :: WSUMO
    DOUBLE PRECISION                            :: WSUMS
    TYPE (CDMATRIX)                             :: WTMATSQR
    !
    ALLOCATE(EXTENDEDMODELVAL(NOBS+MPR),EXTENDEDOBSVAL(NOBS+MPR))
    !
    ! Initialize
    CALL TYP_NULL(WTMATSQR)
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine REG_GNMOD_UEV_CORRELCOEF'
    IFAIL = 0
    !
    IF(MPR>0) THEN
      NLOOP = 2
    ELSE
      NLOOP = 1
    ENDIF
    DO N=1,NLOOP
      IF(N.EQ.1) THEN
        NN = NOBS
        IF (NN <= 0.D0) THEN
           IFAIL = 1
           AMESSAGE = ' NUMBER OF OBSERVATIONS <= 0, TERMINATE '
           WRITE(*,*)AMESSAGE
           CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
           DEALLOCATE(EXTENDEDMODELVAL,EXTENDEDOBSVAL)
           RETURN
        ENDIF
        WTMATSQR = WTFULLSQR
        ALLOCATE(MYO(NN),MYS(NN),WOSQR(NN),WSSQR(NN))
        CALL UTL_MATMULVEC_SUB(NN,WTMATSQR,OBSVAL,WOSQR)
        CALL UTL_MATMULVEC_SUB(NN,WTMATSQR,MODELVAL,WSSQR)
      ENDIF
      IF(N.EQ.2) THEN
        NN = NOBS+MPR
        IF (NN <= 0.D0) THEN
           IFAIL = 1
           AMESSAGE = ' NUMBER OF OBSERVATIONS+PRIOR <= 0, TERMINATE '
           WRITE(*,*)AMESSAGE
           CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
           DEALLOCATE(EXTENDEDMODELVAL,EXTENDEDOBSVAL)
           RETURN
        ENDIF
        ALLOCATE(MYO(NN),MYS(NN),WOSQR(NN),WSSQR(NN))
        DO I=1,NOBS
          EXTENDEDOBSVAL(I) = OBSVAL(I)
          EXTENDEDMODELVAL(I) = MODELVAL(I)
        ENDDO
        DO I=1,NPS
          IF(LN(I) .EQ. 1)PVAL(I)=EXP(PVAL(I))
        ENDDO
        CALL PRI_UEV_RESIDUALS &
                 (IOUT,MPR,NDP+NDEROBS,NPS,PARNAMLC,PVAL)
        DO I=1,NPS
          IF(LN(I) .EQ. 1)PVAL(I)=DLOG(PVAL(I))
        ENDDO
        DO I=1,MPR
          EXTENDEDOBSVAL(NOBS+I) = PRIVAL(I)
          EXTENDEDMODELVAL(NOBS+I) = RESIDSPRI(I)
        ENDDO
        CALL UTL_COMBINESQMATRIX(WTFULLSQR,PRIWTMATSQR,WTMATSQR)
        CALL UTL_MATMULVEC_SUB(NN,WTMATSQR,EXTENDEDOBSVAL,WOSQR)
        CALL UTL_MATMULVEC_SUB(NN,WTMATSQR,EXTENDEDMODELVAL,WSSQR)
      ENDIF
      WSUMO = 0.D0
      WSUMS = 0.D0
      DO I=1,NN
        WSUMO = WSUMO + WOSQR(I)
        WSUMS = WSUMS + WSSQR(I)
      ENDDO
      WSUMO = WSUMO / REAL(NN)
      WSUMS = WSUMS / REAL(NN)
      DO I=1,NN
        MYO(I) = WSUMO
        MYS(I) = WSUMS
      ENDDO
      WOSQR = WOSQR - MYO
      WSSQR = WSSQR - MYS
      NUMER = 0.D0
      DENOM1 = 0.D0
      DENOM2 = 0.D0
      DO I=1,NN
        NUMER = NUMER + (WOSQR(I)*WSSQR(I))
        DENOM1 = DENOM1 + (WOSQR(I)*WOSQR(I))
        DENOM2 = DENOM2 + (WSSQR(I)*WSSQR(I))
      ENDDO
      DENOM = DENOM1 * DENOM2
      IF (DENOM <= 0.D0) THEN
         IFAIL = 1
         AMESSAGE = ' DENOMINATOR <= 0 for CORRELATION COEFFICIENT CALCULATION'
         WRITE(*,*)AMESSAGE
         CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
         DEALLOCATE(EXTENDEDMODELVAL,EXTENDEDOBSVAL)
         RETURN
      ENDIF
      R(N) = (NUMER)/SQRT(DENOM)
      DEALLOCATE(MYO,MYS,WOSQR,WSSQR)
      CALL TYP_DEALLOC(WTMATSQR)
    ENDDO
    IF(NLOOP.EQ.1) R(2)=R(1)
    R1 = R(1)
    R2 = R(2)
    DEALLOCATE(EXTENDEDMODELVAL,EXTENDEDOBSVAL)
    RETURN
  END SUBROUTINE REG_GNMOD_UEV_CORRELCOEF
!===============================================================================
!===============================================================================
  SUBROUTINE REG_GNMOD_EVA (IFAIL,NPS,IFO,IOUT,MAXITER,STDERRONE, &
                      IEND,ISENMETHOD)
    IMPLICIT NONE
    ! Argument list variables
    INTEGER,                             INTENT(INOUT)  :: IFAIL
    INTEGER,                             INTENT(IN)    :: NPS
    INTEGER,                             INTENT(IN)    :: IFO
    INTEGER,                             INTENT(IN)    :: IOUT
    INTEGER,                             INTENT(IN)    :: MAXITER
    LOGICAL,                             INTENT(IN)    :: STDERRONE
    INTEGER,                             INTENT(INOUT) :: IEND
    INTEGER,                             INTENT(INOUT) :: ISENMETHOD(NPS)
    ! Local variables
    INTEGER                                            :: I
    ! Formats
    599 FORMAT(//,80('*'),/,2X, &
    '"Problem Evaluated with Standard Error = 1"'&
    ,/,80('*'),/)
    600 FORMAT(//,80('*'),/,2X,'Parameter Estimation CONVERGED: ', &
    '% change of PARAMETER VALUES less than TolPar', &
    /,80('*'),/)
    601 FORMAT(//,80('-'),/,2X, &
    'Parameter Estimation CONVERGED: % change of', &
    ' SOSWR in 3 Iterations less than TolSOSC',/,80('-'),/)
    602 FORMAT(//,80('!'),/,2X, &
    'Parameter Estimation DID NOT CONVERGE in ', I8,' iterations', &
    /,80('!'),/)
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine REG_GNMOD_EVA'
    IFAIL = 0
      IF (IFO > 0 .AND. IFO < 4) THEN
        DO I=1,NPS
          IF(ISENMETHOD(I) .EQ. 1) ISENMETHOD(I) = 2
        ENDDO
      ENDIF
      IF (IFO .EQ. 1) THEN
        IEND = 600
      ELSEIF (IFO .EQ. 2) THEN
        IEND = 601
      ELSEIF (IFO .EQ. 3) THEN
        IEND = 602
      ENDIF
      IF (IFO  >  0) THEN
        IF (IEND == 600) THEN
          IF(STDERRONE) THEN
            WRITE(IOUT,599)
            WRITE(*,599)
          ELSE
            WRITE(IOUT,600)
            WRITE(*,600)
          ENDIF
        ELSEIF (IEND == 601) THEN
          WRITE(IOUT,601)
          WRITE(*,601)
        ELSEIF (IEND == 602) THEN
          WRITE(IOUT,602)MAXITER
          WRITE(*,602)MAXITER
        ENDIF
      ENDIF
    RETURN
  END SUBROUTINE REG_GNMOD_EVA
!===============================================================================
!===============================================================================
  SUBROUTINE REG_GNMOD_EVA_FINAL_OUT (IFAIL,IOUT,IFO,IND,IOMIT,IPRC,ITERP, &
                            MAXITER,MPR,NDEROBS,NDP,NOBS,NPE,NPS,CSS, &
                            DATAEXCHANGE,IPTR,LN,LPRINT, &
                            MODELVAL,CREATEINITFILES, &
                            OBSVAL,OUTNAM, &
                            PARNAM,PARNAMLC, &
                            PVALINIT, &
                            PVALMAX,PVALMIN,PRIVAL,PRIWTMAT,PRIWTMATSQR, &
                            SENS_NO_OPT,RSQALL1,RSQD1,STDERRONE, &
                            WTFULL,WTFULLSQR,XPRI,XSENST,DTLA,DTLANP, &
                            PVAL,RESIDSPRI,DTLAF,DTLANPF,VAR)
!     VERSION 20031109 EPP
!     MODFIED FROM MF2K PES1BAS6OT VERSION 20010613 ERB
!******************************************************************
!     FINAL GAUSS-NEWTON OUTPUT FOR PARAMETER ESTIMATION
!******************************************************************
!        SPECIFICATIONS:
!------------------------------------------------------------------
    USE DATATYPES
    USE UTILITIES
    IMPLICIT NONE
    ! Argument list variables
    INTEGER,                         INTENT(INOUT) :: IFAIL
    INTEGER,                         INTENT(IN) :: IFO
    INTEGER,                         INTENT(IN) :: IND
    INTEGER,                         INTENT(IN) :: IOMIT
    INTEGER,                         INTENT(IN) :: IOUT
    INTEGER,                         INTENT(IN) :: IPRC
    INTEGER,                         INTENT(IN) :: ITERP
    INTEGER,                         INTENT(IN) :: MAXITER
    INTEGER,                         INTENT(IN) :: MPR
    INTEGER,                         INTENT(IN) :: NDEROBS
    INTEGER,                         INTENT(IN) :: NDP
    INTEGER,                         INTENT(IN) :: NOBS
    INTEGER,                         INTENT(IN) :: NPE
    INTEGER,                         INTENT(IN) :: NPS
    DOUBLE PRECISION,                INTENT(IN) :: CSS(NPE)
    LOGICAL,                         INTENT(IN) :: DATAEXCHANGE
    INTEGER,                         INTENT(IN) :: IPTR(NPE)
    INTEGER,                         INTENT(IN) :: LN(NPS)
    INTEGER,                         INTENT(IN) :: LPRINT
    DOUBLE PRECISION,                INTENT(IN) :: MODELVAL(NOBS)
    LOGICAL,                         INTENT(IN) :: CREATEINITFILES
    DOUBLE PRECISION,                INTENT(IN) :: OBSVAL(NOBS)
    CHARACTER(LEN=MAX_STRING_LEN),   INTENT(IN) :: OUTNAM
    CHARACTER(LEN=12),               INTENT(IN) :: PARNAM(NPS)
    CHARACTER(LEN=12),               INTENT(IN) :: PARNAMLC(NPS) !Paramlowercase
    DOUBLE PRECISION,                INTENT(IN) :: PVALINIT(NPS)
    DOUBLE PRECISION,                INTENT(IN) :: PVALMAX(NPS)
    DOUBLE PRECISION,                INTENT(IN) :: PVALMIN(NPS)
    DOUBLE PRECISION,                INTENT(IN) :: PRIVAL(MPR)
    TYPE (CDMATRIX),                 INTENT(IN) :: PRIWTMAT
    TYPE (CDMATRIX),                 INTENT(IN) :: PRIWTMATSQR
    LOGICAL,                         INTENT(IN) :: SENS_NO_OPT
    DOUBLE PRECISION,                INTENT(IN) :: RSQALL1
    DOUBLE PRECISION,                INTENT(IN) :: RSQD1
    LOGICAL,                         INTENT(IN) :: STDERRONE
    TYPE (CDMATRIX),                 INTENT(IN) :: WTFULL
    TYPE (CDMATRIX),                 INTENT(IN) :: WTFULLSQR
    DOUBLE PRECISION,                INTENT(IN) :: XPRI(NPE,MPR)
    DOUBLE PRECISION,                INTENT(IN) :: XSENST(NPE,NOBS)
    DOUBLE PRECISION,                INTENT(INOUT) :: DTLA
    DOUBLE PRECISION,                INTENT(INOUT) :: DTLANP
    DOUBLE PRECISION,                INTENT(INOUT) :: PVAL(NPS)
    DOUBLE PRECISION,                INTENT(INOUT) :: RESIDSPRI(MPR)
    DOUBLE PRECISION,                INTENT(OUT) :: DTLAF
    DOUBLE PRECISION,                INTENT(OUT) :: DTLANPF
    DOUBLE PRECISION,                INTENT(OUT) :: VAR
    ! Local Variables
    CHARACTER(LEN=45)                             :: ANAME1
    CHARACTER(LEN=37)                             :: ANAME2
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: BUFF
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: BUFF2
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: BUFFCSSSVD
    DOUBLE PRECISION                              :: CHISQL
    DOUBLE PRECISION                              :: CHISQU
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: CSSWP
    DOUBLE PRECISION                              :: DENOM
    DOUBLE PRECISION                              :: DOF
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: DSSWP
    DOUBLE PRECISION                              :: DTMPA
    INTEGER                                       :: I
    INTEGER                                       :: ICHECK
    INTEGER                                       :: IDOF
    INTEGER                                       :: IP
    INTEGER                                       :: J
    INTEGER                                       :: K
    CHARACTER(LEN=MAX_STRING_LEN)                 :: OUTNAMTMP
    DOUBLE PRECISION                              :: R
    DOUBLE PRECISION                              :: R1
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: STDEV
    DOUBLE PRECISION                              :: VARLOW
    DOUBLE PRECISION                              :: VARHIGH
    TYPE (CDMATRIX)                               :: WTFULLPRI
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: XSENSTWPRIRD
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: XSENSTRD
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: XTW
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: XTWNOPRIOR
!!!!!!!!! START FOR FISHER EXPERIMENTAL EXPLORATION !!!!!!!!!!!!!!!!!
!    CHARACTER(LEN=200) :: FMT_STRING
!    CHARACTER(LEN=10)  :: STR_NPERD
!    INTEGER :: IUFISH
!!!!!!!!! END FOR FISHER EXPERIMENTAL EXPLORATION !!!!!!!!!!!!!!!!!
    DATA ANAME1 /'VARIANCE-COVARIANCE MATRIX FOR THE PARAMETERS'/
    DATA ANAME2 /'CORRELATION MATRIX FOR THE PARAMETERS'/
    !------------------------------------------------------------------
    !
    500 FORMAT(' NUMBER OF OMITTED OBSERVATIONS = ',I7)
    501 FORMAT(' PARAMETER FOR WHICH PROBLEM OCCURS = ',I5)
    502 FORMAT(/,80('!'),/,80('!'),/,2X, &
    'Parameter Estimation DID NOT CONVERGE in ', I8,' iterations', &
    /,80('!'),/,80('!'))
    505 FORMAT (/,                                                       &
        '  LEAST-SQUARES OBJ FUNC (OBS. ONLY)----- = ',1PG11.5,/,   &
        '  LEAST-SQUARES OBJ FUNC (W/PRIOR)------- = ',1PG11.5,/,   &
        '  NUMBER OF INCLUDED OBSERVATIONS-------- = ',I7,' OF ',I7,/,   &
        '  NUMBER OF PRIOR ESTIMATES-------------- = ',I7,/,   &
        '  NUMBER OF ESTIMATED PARAMETERS--------- = ',I7,//,   &
        '  CALCULATED ERROR VARIANCE (CEV)-------- = ',1PG11.5,/,   &
        '  95% CONFIDENCE INTERVAL ON CEV--------- = ',1PG11.5,1X,1PG11.5,/,   &
        '  STANDARD ERROR ------------------------ = ',1PG11.5,/,   &
        '  95% CONFIDENCE INTERVAL ON STD ERR----- = ',1PG11.5,1X,1PG11.5,//, &
        '  CORRELATION COEFFICIENT---------------- = ',1PG11.5,/,   &
        '       W/PRIOR--------------------------- = ',1PG11.5,/,    &
        '  NUMBER OF GAUSS-NEWTON ITERATIONS------ = ',I7)
    506 FORMAT (/,                                                       &
        '  LEAST-SQUARES OBJ FUNC (OBS. ONLY)----- = ',1PG11.5,/,   &
        '  NUMBER OF INCLUDED OBSERVATIONS-------- = ',I7,' OF ',I7,/,   &
        '  NUMBER OF PRIOR ESTIMATES-------------- = ',I7,/,   &
        '  NUMBER OF ESTIMATED PARAMETERS--------- = ',I7,//,   &
        '  CALCULATED ERROR VARIANCE (CEV)-------- = ',1PG11.5,/,   &
        '  95% CONFIDENCE INTERVAL ON CEV--------- = ',1PG11.5,1X,1PG11.5,/,   &
        '  STANDARD ERROR ------------------------ = ',1PG11.5,/,   &
        '  95% CONFIDENCE INTERVAL ON STD ERR----- = ',1PG11.5,1X,1PG11.5,//, &
        '  CORRELATION COEFFICIENT---------------- = ',1PG11.5,/,   &
        '  NUMBER OF GAUSS-NEWTON ITERATIONS------ = ',I7)
    570 FORMAT (/,10X,A,/,10X,45('-'))
    571 FORMAT (/,5X,'!!!!!STATISTICS WERE CALCULATED FOR STARTING VALUES!!!!!')
    600 FORMAT (/,92('='),/,1X,'START EIGENVALUES and EIGENVECTORS OF ', &
        'PARAMETER VARIANCE-COVARIANCE MATRIX',/,92('='),/)
    601 FORMAT (/,92('='),/,1X,'END EIGENVALUES and EIGENVECTORS OF ', &
        'PARAMETER VARIANCE-COVARIANCE MATRIX. SEE FILE _mv_eig',/,92('='),/)
    602 FORMAT (1X,'EIGENVALUES and EIGENVECTORS ARE FIT-INDEPENDENT')
    869 FORMAT (/,8X,'THE CORRELATION MATRIX IS FIT-INDEPENDENT')
    870 FORMAT (/,10X,37('-'),/,10X,A,/,10X,37('-'))
!!!! START FOR FISHER EXPERIMENTAL EXPLORATION !!!!!!!!!!!!!!!!!!!!!!
!    !   Write Fisher Information Matrix
!    !   Construct format string
!    WRITE(STR_NPERD,'(I10)') NPERD
!    STR_NPERD = ADJUSTL(STR_NPERD)
!    FMT_STRING = '(1X,'//TRIM(STR_NPERD)//'(1X,G25.16))'
!    IUFISH = UTL_GETUNIT(101,150)
!    OUTNAMTMP = TRIM(OUTNAM)//'._fisher'
!    OPEN(UNIT=IUFISH,FILE=OUTNAMTMP,STATUS='REPLACE')
!!!! END FOR FISHER EXPERIMENTAL EXPLORATION !!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    CALL TYP_NULL(WTFULLPRI)
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine REG_GNMOD_EVA_FINAL_OUT'
    IFAIL = 0
    !-----SINGULAR COEFFICIENT MATRIX
    IF (IND > 0) THEN
       IFAIL = 1
       AMESSAGE = ' LEAST SQUARES COEFFICIENT MATRIX SINGULAR '
       WRITE(*,*)AMESSAGE
       CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
       RETURN
    ENDIF
    !-----COMPUTE VARIANCE-COVARIANCE MATRIX FOR PARAMETERS BY UNSCALING
    !-----C-INVERSE AND MULTIPLYING BY THE FINAL ERROR VARIANCE.
    !      IF (MYID.EQ.MPROC) THEN
    IF(STDERRONE)THEN
      WRITE(IOUT,*)
      WRITE(IOUT,*)'------------------------------------------------------'
      WRITE(IOUT,*)' STDERRONE_PROBLEM OPTION SELECTED,'
      WRITE(IOUT,*)'       RESIDUALS WILL BE ASSUMED ZERO AND,'
      WRITE(IOUT,*)'       CEV WILL BE SET TO 1 FOR CALCULATING COVARIANCE'
      WRITE(IOUT,*)'------------------------------------------------------'
      VAR = 1.D0
    ELSE
      !-----COMPUTE FINAL CALCULATED ERROR VARIANCE
      DENOM = REAL(NOBS-IOMIT+MPR-NPERD)
      IF (DENOM <= 0.D0) THEN
         IFAIL = 1
         IF(SENS_NO_OPT) THEN
           RETURN
         ELSE
           AMESSAGE = ' DEGREES OF FREEDOM = 0, CANNOT DIVIDE by 0 '
           WRITE(*,*)AMESSAGE
           CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
           AMESSAGE = ' TOO FEW OBSERVATIONS GIVEN THE NUMBER OF PARAMETERS'
           WRITE(*,*)AMESSAGE
           CALL UTL_WRITE_MESSAGE(IOUT,'no','yes','yes')
           WRITE(*,500)IOMIT
           WRITE(IOUT,500)IOMIT
         ENDIF
         RETURN
      ENDIF
      VAR = RSQALL1/DENOM
    ENDIF
    IF(ALLOCATED(BUFF)) DEALLOCATE(BUFF)
    IF(ALLOCATED(BUFF2)) DEALLOCATE(BUFF2)
    IF(ALLOCATED(CMAT)) DEALLOCATE(CMAT)
    IF(ALLOCATED(STDEV)) DEALLOCATE(STDEV)
    IF(ALLOCATED(XTW)) DEALLOCATE(XTW)
    IF(ALLOCATED(XTWNOPRIOR)) DEALLOCATE(XTWNOPRIOR)
    ALLOCATE(BUFF(NPERD,NPERD),BUFF2(NPERD,NPERD),CMAT(NPERD,NPERD), &
             BUFFCSSSVD(NPERD,NPERD),CSSWP(NPERD),STDEV(NPERD))
    BUFF = 0.D0
    BUFFCSSSVD = 0.D0
    BUFF2 = 0.D0
    CMAT = 0.D0
    STDEV = 0.D0
    IF (MPR > 0) THEN
      IF(ALLOCATED(XSENSTRD)) DEALLOCATE(XSENSTRD)
      IF(ALLOCATED(XSENSTWPRIRD)) DEALLOCATE(XSENSTWPRIRD)
      ALLOCATE(XTW(NPERD,NOBS+MPR),XTWNOPRIOR(NPERD,NOBS), &
                   XSENSTWPRIRD(NPERD,NOBS+MPR),XSENSTRD(NPERD,NOBS), &
                   DSSWP(NPERD,MPR))
      IF(ALLOCATED(XPTR)) THEN
        ! do nothing because appropriate eliminations have been specified
      ELSE
        ! include all parameters
        ALLOCATE(XPTR(NPE))
        DO I=1,NPE
          XPTR(I) = I
        ENDDO
      ENDIF
      DO I = 1,NPERD
        CSSWP(I) = CSS(XPTR(I))
      ENDDO
      XSENSTRD = 0.D0
      XSENSTWPRIRD = 0.D0
      XTW = 0.D0
      XTWNOPRIOR = 0.D0
      DO I=1,NPERD
        DO J=1,NOBS
          XSENSTWPRIRD(I,J) = XSENST(XPTR(I),J)
          XSENSTRD(I,J) = XSENST(XPTR(I),J)
        ENDDO
        DO J=1,MPR
          XSENSTWPRIRD(I,NOBS+J) = XPRI(XPTR(I),J)
        ENDDO
      ENDDO
      CALL UTL_COMBINESQMATRIX(WTFULL,PRIWTMAT,WTFULLPRI)
      ! Calculate XTw no prior
      CALL UTL_MATMUL_SUB(NPERD,NOBS,XSENSTRD,WTFULL,XTWNOPRIOR)
      BUFF = MATMUL(XTWNOPRIOR,TRANSPOSE(XSENSTRD))
      CALL UTLUCODE_INVERT(IFAIL,BUFF,NPERD,CMAT,IOUT,DTLANP)
      BUFF = BUFF/(RSQD1/NOBS)
      CALL UTLUCODE_INVERT(IFAIL,BUFF,NPERD,BUFF2,IOUT,DTLANPF)
          !!!! START FOR FISHER EXPERIMENTAL EXPLORATION !!!!!!!!!!!!!!!!!!!!!!!
          !      WRITE(IUFISH,*)' Fisher Info Matrix No Prior '
          !      DO I = 1,NPERD
          !        WRITE(IUFISH,FMT_STRING) (BUFF(I,J),J=1,NPERD)
          !      ENDDO
          !      WRITE(IUFISH,*)
          !      WRITE(IUFISH,*)' LN |XTwX| no prior '
          !      WRITE(IUFISH,*)-DTLANP
          !      WRITE(IUFISH,*)' LN |F| no prior '
          !      WRITE(IUFISH,*)-DTLANPF
          !!!! END FOR FISHER EXPERIMENTAL EXPLORATION !!!!!!!!!!!!!!!!!!!!!!!
      ! Calculate XTw with prior
      CALL UTL_MATMUL_SUB(NPERD,NOBS+MPR,XSENSTWPRIRD,WTFULLPRI,XTW)
      BUFF = MATMUL(XTW,TRANSPOSE(XSENSTWPRIRD))
      BUFFCSSSVD = BUFF
      CALL UTLUCODE_INVERT(IFAIL,BUFF,NPERD,CMAT,IOUT,DTLA)
      BUFF = BUFF/(RSQALL1/(NOBS+MPR))
      CALL UTLUCODE_INVERT(IFAIL,BUFF,NPERD,BUFF2,IOUT,DTLAF)
      DSSWP = 0.D0
      DO I = 1,NPERD
        CSSWP(I) = (CSSWP(I) * CSSWP(I)) * (NOBS)
      ENDDO
      ! Calculate Dimensionless Scaled Sensitivity for prior
      ! Unscaled(XPRI) * PVAL * SQRT(WT)
      DO I=1,NPERD
        DO J=1,MPR
          DO K=1,MPR
            DSSWP(I,J) = DSSWP(I,J) &
                            + XPRI(XPTR(I),K)*UTL_GETVAL(PRIWTMATSQR,K,J)
          ENDDO
          IF(LN(IPTR(XPTR(I))) == 0) DSSWP(I,J)=DSSWP(I,J)*PVAL(IPTR(XPTR(I)))
        ENDDO
      ENDDO
      DO I=1,NPERD
        DO J=1,MPR
          CSSWP(I)= CSSWP(I) + (DSSWP(I,J) * DSSWP(I,J))
        ENDDO
        CSSWP(I) = SQRT(CSSWP(I) / (NOBS+MPR))
      ENDDO
      !!!! START FOR FISHER EXPERIMENTAL EXPLORATION !!!!!!!!!!!!!!!!!!!!!!!
      !      WRITE(IUFISH,*)
      !      WRITE(IUFISH,*)' Fisher Info Matrix With Prior '
      !      DO I = 1,NPERD
      !        WRITE(IUFISH,FMT_STRING) (BUFF(I,J),J=1,NPERD)
      !      ENDDO
      !      WRITE(IUFISH,*)
      !      WRITE(IUFISH,*)' LN |XTwX| with prior '
      !      WRITE(IUFISH,*)-DTLA
      !      WRITE(IUFISH,*)' LN |F| with prior '
      !      WRITE(IUFISH,*)-DTLAF
      !      CLOSE(IUFISH)
      !!!! END FOR FISHER EXPERIMENTAL EXPLORATION !!!!!!!!!!!!!!!!!!!!!!!
      ! CLEAN UP
      DEALLOCATE(DSSWP)
      DEALLOCATE(XSENSTRD)
      DEALLOCATE(XSENSTWPRIRD)
      CALL TYP_DEALLOC (WTFULLPRI)
      DEALLOCATE(XTWNOPRIOR)
    ELSE
      IF(ALLOCATED(XSENSTRD)) DEALLOCATE(XSENSTRD)
      ALLOCATE(XTW(NPERD,NOBS),XSENSTRD(NPERD,NOBS))
      XSENSTRD = 0.D0
      XTW = 0.D0
      IF(SENS_NO_OPT) THEN
        XSENSTRD = XSENST
        IF(ALLOCATED(NPTR)) DEALLOCATE(NPTR)
        ALLOCATE(NPTR(NPERD))
        NPTR = IPTR
        CSSWP = CSS
      ELSE
        DO I=1,NPERD
          DO J=1,NOBS
            XSENSTRD(I,J) = XSENST(XPTR(I),J)
          ENDDO
        ENDDO
        DO I = 1,NPERD
          CSSWP(I) = CSS(XPTR(I))
        ENDDO
      ENDIF
      ! Calculate XTw
      CALL UTL_MATMUL_SUB(NPERD,NOBS,XSENSTRD,WTFULL,XTW)
      ! XTwX
      BUFF = MATMUL(XTW,TRANSPOSE(XSENSTRD))
      BUFFCSSSVD = BUFF
      DEALLOCATE(XSENSTRD)
      ! INVERT
      CALL UTLUCODE_INVERT(IFAIL,BUFF,NPERD,CMAT,IOUT,DTLANP)
      BUFF = BUFF/(RSQD1/NOBS)
      CALL UTLUCODE_INVERT(IFAIL,BUFF,NPERD,BUFF2,IOUT,DTLANPF)
          !!!! START FOR FISHER EXPERIMENTAL EXPLORATION !!!!!!!!!!!!!!!!!!!!!!!
          !      WRITE (IUFISH,*)' Fisher Info Matrix No Prior '
          !      DO I = 1,NPERD
          !        WRITE (IUFISH,FMT_STRING) (BUFF(I,J),J=1,NPERD)
          !      ENDDO
          !      WRITE(IUFISH,*)
          !      WRITE(IUFISH,*)' LN |XTwX| no prior '
          !      WRITE(IUFISH,*)-DTLANP
          !      WRITE(IUFISH,*)' LN |F| no prior '
          !      WRITE(IUFISH,*)-DTLANPF
          !      CLOSE(IUFISH)
          !!!! END FOR FISHER EXPERIMENTAL EXPLORATION !!!!!!!!!!!!!!!!!!!!!!!
      ! WHEN NO PRIOR ARE USED DTLA AND DTLANP ARE THE SAME
      DTLAF = DTLANPF
      DTLA = DTLANP
    ENDIF
    DEALLOCATE(BUFF2)
    DEALLOCATE(XTW)
    IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//' called from REG_GNMOD_EVA_FINAL_OUT ')
    ! want ln|det| XTwX, but DTLA is the ln|det| of (XTwX)^-1
    ! therefore has the opposite sign, so the sign is reversed here
    DTLA = -DTLA
    DTLANP = -DTLANP
    DTLAF = -DTLAF
    DTLANPF = -DTLANPF
    IF (IVERB.GE.5) THEN
      WRITE(IOUT,*)
      WRITE(IOUT,*)' KIC TERMS printed because VERBOSE >= 5 ' ! ***** KIC terms
      WRITE(IOUT,*)' LN DETERMINANT OF FISHER INFORMATION MATRIX obsonly ',DTLANPF ! ***** LN DETERMINANT OF FISHER INFORMATION MATRIX: is DTLANPF
      WRITE(IOUT,*)
    ENDIF
    CMAT = CMAT*VAR
    BUFF = CMAT
    ! GET STDEVs from diagonal of cov matrix for later use
    DO IP=1,NPERD
      STDEV(IP) = DSQRT(CMAT(IP,IP))
    ENDDO
    !------PRINT VARIANCE-COVARIANCE MATRIX.
    WRITE (IOUT,570) ANAME1
    IF(ITERP <= 1) THEN
      DO ICHECK=1,NPS
        IF(PVAL(ICHECK) .NE. PVALINIT(ICHECK)) EXIT
        IF(ICHECK .EQ. NPS)WRITE(IOUT,571)
      ENDDO
    ENDIF
    IF(ALLOCATED(NPTR)) THEN
      ! do nothing
    ELSE
      ALLOCATE(NPTR(NPE))
      NPTR = IPTR
    ENDIF
    CALL UTL_PARSQMATRIX(IOUT,IPRC,NPERD,NPS,BUFF,NPTR,PARNAM)
    IF(DATAEXCHANGE) THEN
      ! WRITE COVARIANCE MATRIX _mv
      CALL UTL_DX_WRITE_MCMV('_mv',NPERD,NPS,NPTR,OUTNAM,PARNAM,CMAT)
    ENDIF
    IF(CREATEINITFILES) THEN
    ! WRITE COVARIANCE MATRIX _mv
      OUTNAMTMP = TRIM(OUTNAM)//'._init'
      CALL UTL_DX_WRITE_MCMV('_mv',NPE,NPS,IPTR,OUTNAMTMP,PARNAM,CMAT)
    ENDIF
    !
    !-------EIGENVALUES AND EIGENVECTORS OF COV MATRIX SCALED BY PARAMETERS
    IF (.NOT. STDERRONE .AND. LPRINT == 1) THEN
      WRITE(IOUT,600)
      WRITE(IOUT,602)
      CALL UTL_EIGEN(IOUT,IPRC,NPERD,NPS,NPTR,ITERP,PARNAM,PVALINIT,PVAL,CMAT, &
                     DATAEXCHANGE,OUTNAM)
      WRITE(IOUT,601)
    ENDIF
    IF(.NOT. CREATEINITFILES) &
      CALL UTLUCODE_CSSSVD(IOUT,IPRC,MPR,NOBS,NPERD,NPS,NPTR,ITERP,CSSWP,LN, &
                   PARNAM,PVALINIT,PVAL,BUFFCSSSVD,DATAEXCHANGE,OUTNAM)
    DEALLOCATE(CSSWP)
    DEALLOCATE(BUFFCSSSVD)
    !
    !--------COMPUTE AND PRINT CORRELATION MATRIX FOR PARAMETERS
    !     BUFF REPLACES C FOR NEXT 30 LINES
    IF (NPERD > 1) THEN
      DO IP = 1, NPERD
        DTMPA = STDEV(IP)
        DO I = IP, NPERD
          DENOM = STDEV(I)*DTMPA
          IF (DENOM .EQ. 0.D0) THEN
             IFAIL = 1
             AMESSAGE = ' DENOMINATOR FOR CORRELATION MATRIX = 0, CANNOT DIVIDE'
             WRITE(*,*)AMESSAGE
             CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
             WRITE(*,501)IP
             WRITE(IOUT,501)IP
             RETURN
          ENDIF
          BUFF(I,IP) = CMAT(I,IP)/DENOM
          BUFF(IP,I) = BUFF(I,IP)
        ENDDO
      ENDDO
      WRITE (IOUT,869)
      WRITE (IOUT,870) ANAME2
      IF(ITERP <= 1) THEN
        DO ICHECK=1,NPS
          IF(PVAL(ICHECK) .NE. PVALINIT(ICHECK)) EXIT
          IF(ICHECK .EQ. NPS)WRITE(IOUT,571)
        ENDDO
      ENDIF
      CALL UTL_PARSQMATRIX(IOUT,IPRC,NPERD,NPS,BUFF,NPTR,PARNAM)
    !
      IF (DATAEXCHANGE) THEN
        ! WRITE CORRELATION MATRIX _mc
        CALL UTL_DX_WRITE_MCMV('_mc',NPERD,NPS,NPTR,OUTNAM,PARNAM,BUFF)
        CALL UTL_DX_WRITE_PCC (NPERD,NPS,BUFF,DATAEXCHANGE,IOUT,NPTR,OUTNAM, &
                               PARNAM)
      ENDIF
    !
    ENDIF
    !-------PRINT PARAMETER STATISTICS
    IF (.NOT. STDERRONE) THEN
      CALL REG_GNMOD_UEV_WRITE_PARAM_STATS  &
            (IFAIL,DATAEXCHANGE,IOUT,ITERP,MPR, &
            NOBS,NPS,NPTR,LN,OUTNAM,STDEV,PARNAM, &
            PVALMAX,PVALMIN,PVALINIT,PVAL)
      IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//'called from REG_GNMOD_EVA_FINAL_OUT')
      !-------CALCULATE CORRELATION COEFFICIENT
      CALL REG_GNMOD_UEV_CORRELCOEF &
             (IFAIL,IOUT,MPR,NDEROBS,NDP,NPS,NOBS,LN,MODELVAL, &
              OBSVAL,PARNAMLC,PRIVAL, &
              PRIWTMATSQR,PVAL,WTFULLSQR, &
              RESIDSPRI,R,R1)
      IF(IFAIL > 0) CALL UTL_STOP(ERRSUB//'called from REG_GNMOD_EVA_FINAL_OUT')
      !-------PRINT FINAL RSQ'S, ERROR VARIANCE, CORRELATION, ITERATIONS
      IF(IFO.EQ.3)WRITE(IOUT,502)MAXITER
      DOF = REAL(NOBS-IOMIT+MPR-NPERD)
      IDOF = NOBS-IOMIT+MPR-NPERD
      CALL UTLUCODE_CHISQ(IDOF,CHISQL,CHISQU)
      VARLOW = DOF * VAR / CHISQL
      VARHIGH = DOF * VAR / CHISQU
      IF(MPR > 0) THEN
        WRITE (IOUT,505) RSQD1,RSQALL1,NOBS-IOMIT,NOBS,MPR,NPERD,VAR,VARLOW, &
                     VARHIGH,VAR**.5,VARLOW**.5,VARHIGH**.5,R,R1,ITERP
      ELSE
        WRITE (IOUT,506) RSQD1,NOBS-IOMIT,NOBS,MPR,NPERD,VAR,VARLOW, &
                     VARHIGH,VAR**.5,VARLOW**.5,VARHIGH**.5,R,ITERP
      ENDIF
    ENDIF
    !
    RETURN
  END SUBROUTINE REG_GNMOD_EVA_FINAL_OUT
!===============================================================================
!===============================================================================
  SUBROUTINE REG_GNMOD_EVA_SUMMARY (MAXITER,NOBS,NPE,NPS,FINALSTATS, &
                        IFO,INCLU,ILOW,ITERP, &
                        IPTR,OUTNAM,PAREST,PARNAM,PVAL,PVALINIT, &
                        REACT,RSQALL,RSQD,TRUSTREGION)
    IMPLICIT NONE
    ! Argument list variables
    INTEGER,                              INTENT(IN) :: MAXITER
    INTEGER,                              INTENT(IN) :: NOBS
    INTEGER,                              INTENT(IN) :: NPE
    INTEGER,                              INTENT(IN) :: NPS
    LOGICAL,                              INTENT(IN) :: FINALSTATS
    INTEGER,                              INTENT(IN) :: IFO
    INTEGER,                              INTENT(IN) :: INCLU(MAXITER+2)
    INTEGER,                              INTENT(IN) :: ILOW
    INTEGER,                              INTENT(IN) :: ITERP
    INTEGER,                              INTENT(IN) :: IPTR(NPE)
    CHARACTER(LEN=MAX_STRING_LEN),        INTENT(IN) :: OUTNAM
    DOUBLE PRECISION,                     INTENT(IN) :: PAREST(MAXITER,NPS)
    CHARACTER(LEN=12),                    INTENT(IN) :: PARNAM(NPS)
    DOUBLE PRECISION,                     INTENT(IN) :: PVAL(NPS)
    DOUBLE PRECISION,                     INTENT(IN) :: PVALINIT(NPS)
    INTEGER,                              INTENT(IN) :: REACT
    DOUBLE PRECISION,                     INTENT(IN) :: RSQALL(MAXITER+2)
    DOUBLE PRECISION,                     INTENT(IN) :: RSQD(MAXITER+2)
    LOGICAL,                              INTENT(IN) :: TRUSTREGION
    ! local variables
    INTEGER                    :: I
    INTEGER                    :: IP
    INTEGER                    :: IM1
    INTEGER, SAVE              :: IUSUMMARY
    INTEGER                    :: ITER
    INTEGER                    :: NBOT
    INTEGER                    :: NTOP
    !   ------------------------------------------------------------------
    ! formats
    680 FORMAT(/,1X,'SUMS OF SQUARED WEIGHTED RESIDUALS FOR EACH', &
        ' ITERATION',//, &
        8X,' SUMS OF SQUARED WEIGHTED RESIDUALS',/ &
        9X,'ITER.',2X,'OBSERVATIONS',2X,'PRIOR INFO. ',5X,'TOTAL',7X, &
        '# INCLUDED OBS')
    681 FORMAT(//,1X,'PARAMETER VALUES FOR EACH ITERATION',/, &
        ' - INDICATES DAMPING RESULTED FROM PARAMETER CONSTRAINTS FOR THAT ', &
        'ITERATION.',/, &
        ' * INDICATES SOME PARAMETERS WERE OMITTED DURING THAT ITERATION.', &
        '  THE VALUES',/, &
        '   OF THOSE PARAMETERS ARE REPORTED EVEN THOUGH THEY WERE', &
        ' NOT UPDATED')
    682 FORMAT(/,1X,'  ITER  ',5(1X,A12))
    690 FORMAT(1X,I14,3(2X,G12.5),I7,' OF ',I7)
    691 FORMAT(1X,I6,1X,5(1X,G12.4))
    693 FORMAT(1X,I6,A1,5(1X,G12.4))
    700 FORMAT(/, &
        ' SELECTED STATISTICS FROM MODIFIED GAUSS-NEWTON ITERATIONS',/,/, &
        8X,'# PARAMs',18X,'MAX.',13X,'MAX. CHANGE   DAMPING',/, &
        1X,'ITER.',2X,'ESTIMATED',2X,'PARNAM',9X,'CALC. CHANGE',5X,'ALLOWED', &
        7X,'PARAMETER',/, &
        ' -----  --------- ------------  ',16('-'),1X,13('-'),2X,12('-'))
    710 FORMAT(1X,I4,3X,I6,5X,A,1X,G13.6,3X,G13.6,2X,G12.5)
    711 FORMAT(/, &
        ' SELECTED STATISTICS FROM TRUST REGION GAUSS-NEWTON ITERATIONS',/, &
        '   FN = FULL NEWTON STEP',/, &
        '   RN = RESTRICTED NEWTON STEP',/, &
        '   HS = HOOKSTEP',/, &
        '   DD = DOUBLE-DOGLEG STEP',/, &
        '   RSD = RESTRICTED STEEPEST DESCENT STEP',/,/, &
        8X,'# PARAMs',2X,'MAX. CHANGE',4X, 'MAX.',10X,'TRUSTREGION',5X,'RADIUS' /, &
        1X,'ITER.',2X,'ESTIMATED',2X,'PARNAM',8X,'CALC. CHANGE',2X,'RADIUS', &
        10X,'CHANGE',3X,'STEP',/, &
        ' -----  --------- ------------ ',14('-'),1X,14('-'),1X,9('-'),1X,5('-'))
    712 FORMAT(1X,I4,3X,I6,5X,A,1X,G13.6,2X,G13.6,2X,A,2X,A)
    !     WRITE SELECTED STATISTICS FOR EACH ITERATION
    IUSUMMARY = UTL_DX_OPEN(OUTNAM,'_summary','REPLACE')
    IF(.NOT. TRUSTREGION)THEN   ! WRITE GN STATS IF NOT USING TRUSTREGION
      WRITE (IUSUMMARY,700)
      DO I=1,ITERP
        WRITE(IUSUMMARY,710) &
                         I,NPAREST(I),PARNAM(NPMAXCHG(I)),DMXA(I),AMCA(I),AAP(I)
      ENDDO
    ELSE                        ! WRITE TRUSTREGION STATS
      WRITE (IUSUMMARY,711)
      DO I=1,ITERP
        WRITE(IUSUMMARY,712) I,NPAREST(I),PARNAM(NPMAXCHG(I)),DMXA(I), &
                             TRAD(I),RADCHG(I),STEPUSED(I)
      ENDDO
    ENDIF
    !     WRITE PARAMETER ESTIMATES FOR EACH ITERATION
    WRITE (IUSUMMARY,681)
    DO I=1,NPE,5
      NBOT=I
      NTOP=I+4
      IF(NTOP > NPE)NTOP=NPE
      WRITE (IUSUMMARY,682)(PARNAM(IPTR(IP)),IP=NBOT,NTOP)
      WRITE (IUSUMMARY,691) 0,(PVALINIT(IPTR(IP)),IP=NBOT,NTOP)
      DO ITER=1,ITERP
        WRITE (IUSUMMARY,693) &
                     ITER,NPARSTAR(ITER),(PAREST(ITER,IPTR(IP)),IP=NBOT,NTOP)
      ENDDO
    ENDDO
    !     WRITE SSWR FOR EACH ITERATION
    WRITE (IUSUMMARY,680)
    DO I=1,ITERP
      IM1 = I - 1
      WRITE (IUSUMMARY,690)IM1,RSQD(I),RSQALL(I)-RSQD(I),RSQALL(I),INCLU(I),NOBS
    ENDDO
    IF(.NOT. TRUSTREGION .AND. FINALSTATS .AND. &
             REACT == 1 .AND. RSQALL(ITERP+2) > RSQALL(ILOW)) &
             CALL UTLUCODE_PAR_OMIT_REACT1 &
                  (ILOW-1,IUSUMMARY,RSQALL(ITERP+2),RSQALL(ILOW))
    IUSUMMARY = UTL_DX_CLOSE('_summary')
    RETURN
  END SUBROUTINE REG_GNMOD_EVA_SUMMARY
!===============================================================================
!===============================================================================
  SUBROUTINE REG_GNMOD_EVA_FINISH (DATAEXCHANGE,MAXITER,MPR,NOBS,NPE,NPS,IFO, &
                        ILOW,INCLU,INTERMEDPRINT,IOUT,ITERP,IPTR,OUTNAM, &
                        PAREST,PARNAM,PVAL,PVALINIT,REACT,RSQALL,RSQD, &
                        STATS_ON_NONCONVERGE,TRUSTREGION,UNUSUAL)
    !     VERSION 20031109 EPP
    !     MODIFIED FROM MF2K, SPES1GAU1PR VERSION 20010613 ERB
    !-----VERSION 1001 01JUN1992
    !******************************************************************
    !     PRINT PARAMETER VALUES AND STATISTICS FOR ALL ITERATIONS
    !******************************************************************
    !        SPECIFICATIONS:
    !------------------------------------------------------------------
    IMPLICIT NONE
    ! Argument list variables
    LOGICAL,                              INTENT(IN) :: DATAEXCHANGE
    INTEGER,                              INTENT(IN) :: MAXITER
    INTEGER,                              INTENT(IN) :: MPR
    INTEGER,                              INTENT(IN) :: NOBS
    INTEGER,                              INTENT(IN) :: NPE
    INTEGER,                              INTENT(IN) :: NPS
    INTEGER,                              INTENT(IN) :: IFO
    INTEGER,                              INTENT(IN) :: ILOW
    INTEGER,                              INTENT(IN) :: INCLU(MAXITER+2)
    INTEGER,                              INTENT(IN) :: INTERMEDPRINT
    INTEGER,                              INTENT(IN) :: IOUT
    INTEGER,                              INTENT(IN) :: ITERP
    INTEGER,                              INTENT(IN) :: IPTR(NPE)
    CHARACTER(LEN=MAX_STRING_LEN),        INTENT(IN) :: OUTNAM
    DOUBLE PRECISION,                     INTENT(IN) :: PAREST(MAXITER,NPS)
    CHARACTER(LEN=12),                    INTENT(IN) :: PARNAM(NPS)
    DOUBLE PRECISION,                     INTENT(IN) :: PVAL(NPS)
    DOUBLE PRECISION,                     INTENT(IN) :: PVALINIT(NPS)
    INTEGER,                              INTENT(IN) :: REACT
    DOUBLE PRECISION,                     INTENT(IN) :: RSQALL(MAXITER+2)
    DOUBLE PRECISION,                     INTENT(IN) :: RSQD(MAXITER+2)
    LOGICAL,                              INTENT(IN) :: STATS_ON_NONCONVERGE
    LOGICAL,                              INTENT(IN) :: TRUSTREGION
    LOGICAL,                           INTENT(INOUT) :: UNUSUAL
    ! local variables
    CHARACTER(2000)            :: FN
    INTEGER                    :: I
    INTEGER                    :: II
    INTEGER                    :: IP
    INTEGER                    :: IM1
    INTEGER                    :: ITER
    INTEGER                    :: IUSUMMARY
    INTEGER                    :: NBOT
    INTEGER                    :: NTOP
    DOUBLE PRECISION           :: RSQPI
    !   ------------------------------------------------------------------
    400 FORMAT (/,80('-'))
    515 FORMAT (//,' SUMMARY OF PARAMETER VALUES AND STATISTICS FOR',/, &
        '      ALL PARAMETER-ESTIMATION ITERATIONS')
    516 FORMAT (//,' Extended intermediate printing was selected so an ', &
                   ' extended summary is provided',/,' to facilitate ', &
                   ' tracking the history of the regression')
    530 FORMAT (/,1X,'PARAMETER-ESTIMATION ITERATION: ',I4)
    535 FORMAT (/,1X,'PARAMETER VALUES AND STATISTICS:')
    540 FORMAT (/,1X,'PARAMETER NAME(S) AND VALUE(S):',/)
    545 FORMAT (6(3X,A10))
    550 FORMAT (6(2X,1PG11.4))
    560 FORMAT (1X,3(2X,G10.3,2X),I7,' OF ',I7)
    565 FORMAT (/,1X,'SUMS OF SQUARED WEIGHTED', &
        ' RESIDUALS:',/,3X,'OBSERVATIONS',3X,'PRIOR INFO.',4X, &
        'TOTAL',7X,'# INCLUDED OBS')
    570 FORMAT (/,1X,'PARAMETER UPDATE:',/, &
        1X,'MAX CHANGE PARAMETER  MAX CALC. CHANGE   DAMPING',/, &
        5X,A10,7X,G10.3,9X,G10.3)
    680 FORMAT(/,1X,'SUMS OF SQUARED WEIGHTED RESIDUALS FOR EACH', &
        ' ITERATION',//, &
        8X,' SUMS OF SQUARED WEIGHTED RESIDUALS',/ &
        9X,'ITER.',2X,'OBSERVATIONS',2X,'PRIOR INFO. ',5X,'TOTAL',7X, &
        '# INCLUDED OBS')
    681 FORMAT(//,1X,'PARAMETER VALUES FOR EACH ITERATION',/, &
        ' - INDICATES DAMPING RESULTED FROM PARAMETER CONSTRAINTS FOR THAT ', &
        'ITERATION.',/, &
        ' * INDICATES SOME PARAMETERS WERE OMITTED DURING THAT ITERATION.', &
        '  THE VALUES',/, &
        '   OF THOSE PARAMETERS ARE REPORTED EVEN THOUGH THEY WERE', &
        ' NOT UPDATED')
    682 FORMAT(/,1X,'   ITER   ',5(1X,A12))
    689 FORMAT(1X,I14,2X, &
        'Solution did not converge and Statistics were not calculated',/,17X, &
        'Sum of squared weighted residuals is not available for printing')
    690 FORMAT(1X,I14,3(2X,G12.5),I7,' OF ',I7)
    691 FORMAT(1X,I7,2X,5(1X,G12.4))
   6921 FORMAT(/,1X,'FINAL PARAMETER VALUES ARE TAKEN FROM THE ITERATION ', &
        'WITH THE LOWEST SUM OF',/,3X,'SQUARED WEIGHTED RESIDUALS WHICH IS ', &
        'NORMALLY THE LAST ITERATION, BUT',/,3X,'OCCASIONALLY IT IS NOT.')
   6922 FORMAT(/,3X,'IN THIS CASE IT WAS THE LAST ITERATION.')
   6923 FORMAT(/,3X,'IN THIS CASE IT WAS ITERATION:',I7)
   6924 FORMAT(/,1X,'FINAL REGRESSION STATISTICS ARE EVALUATED WITH THE ', &
               'FOLLOWING PARAMETER VALUES:')
   6925 FORMAT(/,11X,5(1X,A12))
   6926 FORMAT(8X,A1,1X,5(1X,G12.4))
    693 FORMAT(1X,I7,A1,1X,5(1X,G12.4))
   6951 FORMAT(1X,'        LOWEST',3(2X,G12.5),I7,' OF ',I7,/)
   6952 FORMAT(9X,'ITERATION WITH LOWEST SUM OF SQUARED WEIGHTED RESIDUALS:',I7)
    696 FORMAT (//,1X,78('!'),//,4X, &
      ' WARNING: INITIAL VALUES PRODUCED THE LOWEST SUM OF SQUARED RESIDUALS', &
      //,1X,78('!'))
    700 FORMAT(/, &
        ' SELECTED STATISTICS FROM MODIFIED GAUSS-NEWTON ITERATIONS',/,/, &
        8X,'# PARAMs',18X,'MAX.',13X,'MAX. CHANGE   DAMPING',/, &
        1X,'ITER.',2X,'ESTIMATED',2X,'PARNAM',9X,'CALC. CHANGE',5X,'ALLOWED', &
        7X,'PARAMETER',/, &
        ' -----  --------- ------------  ',16('-'),1X,13('-'),2X,12('-'))
    701 FORMAT(/,' STARTING VALUES ARE USED AS OPTIMAL VALUES',/)
    710 FORMAT(1X,I4,3X,I6,5X,A,1X,G13.6,3X,G13.6,2X,G12.5)
    711 FORMAT(/, &
        ' SELECTED STATISTICS FROM TRUST REGION GAUSS-NEWTON ITERATIONS',/, &
        '   FN = FULL NEWTON STEP',/, &
        '   RN = RESTRICTED NEWTON STEP',/, &
        '   HS = HOOKSTEP',/, &
        '   DD = DOUBLE-DOGLEG STEP',/, &
        '   RSD = RESTRICTED STEEPEST DESCENT STEP',/,/, &
        8X,'# PARAMs',2X,'MAX. CHANGE',4X, 'MAX.',10X,'TRUSTREGION',5X,'RADIUS' /, &
        1X,'ITER.',2X,'ESTIMATED',2X,'PARNAM',8X,'CALC. CHANGE',2X,'RADIUS', &
        10X,'CHANGE',3X,'STEP',/, &
        ' -----  --------- ------------ ',14('-'),1X,14('-'),1X,9('-'),1X,5('-'))
    712 FORMAT(1X,I4,3X,I6,5X,A,1X,G13.6,2X,G13.6,2X,A,2X,A)
    !     WRITE SUMMARY OF PARAMETER VALUES AND STATISTICS FOR ALL
    !     PARAMETER ITERATIONS
    WRITE (IOUT,515)
    !
    IF (INTERMEDPRINT .EQ. 1 .OR. INTERMEDPRINT .EQ. 3) THEN
      WRITE (IOUT,516)
      DO ITER = 1, ITERP
        WRITE (IOUT,400)
        IF(ITER .LE. ITERP) WRITE (IOUT,530) ITER
        WRITE (IOUT,535)
        WRITE (IOUT,540)
        WRITE (IOUT,545) (PARNAM(NPTR(IP)),IP=1,NPERD)
        WRITE (IOUT,'(1X)')
        WRITE (IOUT,550) (PAREST(ITER,NPTR(IP)),IP=1,NPERD)
        RSQPI = 0.D0
        IF (MPR > 0) RSQPI = RSQALL(ITER+1) - RSQD(ITER+1)
        WRITE (IOUT,565)
        WRITE (IOUT,560) RSQD(ITER+1), RSQPI, RSQALL(ITER+1),INCLU(ITER+1),NOBS
        WRITE (IOUT,570) PARNAM(NPMAXCHG(ITER)), DMXA(ITER),AAP(ITER)
      ENDDO
    ENDIF
    !
    WRITE (IOUT,400)
    !
    !     WRITE SELECTED STATISTICS FOR EACH ITERATION
    WRITE (IOUT,400)
    IF(.NOT. TRUSTREGION)THEN   ! WRITE GN STATS IF NOT USING TRUSTREGION
      WRITE (IOUT,700)
      IF (ILOW .EQ. 1) THEN
        WRITE (IOUT,701)
      ELSE
        DO I=1,ITERP
          WRITE(IOUT,710)I,NPAREST(I),PARNAM(NPMAXCHG(I)),DMXA(I),AMCA(I),AAP(I)
        ENDDO
      ENDIF
    ELSE                        ! WRITE TRUSTREGION STATS
      WRITE (IOUT,711)
      IF (ILOW .EQ. 1) THEN
        WRITE (IOUT,701)
      ELSE
        DO I=1,ITERP
          WRITE(IOUT,712)I,NPAREST(I),PARNAM(NPMAXCHG(I)),DMXA(I), &
                         TRAD(I),RADCHG(I),STEPUSED(I)
        ENDDO
      ENDIF
    ENDIF
    !
    !     WRITE PARAMETER ESTIMATES FOR EACH ITERATION
    WRITE (IOUT,400)
    WRITE (IOUT,681)
    DO I=1,NPE,5
      NBOT=I
      NTOP=I+4
      IF(NTOP > NPE)NTOP=NPE
      WRITE (IOUT,682)(PARNAM(IPTR(IP)),IP=NBOT,NTOP)
      IF (ITERP .EQ. 1)WRITE (IOUT,701)
      WRITE (IOUT,691) 0,(PVALINIT(IPTR(IP)),IP=NBOT,NTOP)
      DO ITER=1,ITERP
        WRITE (IOUT,693)ITER,NPARSTAR(ITER),(PAREST(ITER,IPTR(IP)),IP=NBOT,NTOP)
      ENDDO
    ENDDO
    IF(IFO .NE. 3 .OR. STATS_ON_NONCONVERGE) THEN
      WRITE (IOUT,6921)
      IF(ILOW-1 == ITERP) THEN
        WRITE (IOUT,6922)
      ELSE
        WRITE (IOUT,6923)ILOW-1
      ENDIF
      WRITE (IOUT,6924)
      DO I=1,NPE,5
        NBOT=I
        NTOP=I+4
        IF(NTOP > NPE)NTOP=NPE
          WRITE (IOUT,6925)(PARNAM(IPTR(IP)),IP=NBOT,NTOP)
          WRITE (IOUT,6926)NPARSTAR(ITERP+1),(PVAL(IPTR(IP)),IP=NBOT,NTOP)
      ENDDO
    ENDIF
    !
    !     WRITE SSWR FOR EACH ITERATION
    WRITE (IOUT,400)
    WRITE (IOUT,680)
    DO I=1,ITERP+2
      IM1 = I - 1
      IF(I < ITERP+2) THEN
        IF(IFO .NE. 3 .OR. (I<ITERP+1 .OR. &
                            (I==ITERP+1 .AND. STATS_ON_NONCONVERGE))) THEN
          WRITE (IOUT,690)IM1,RSQD(I),RSQALL(I)-RSQD(I),RSQALL(I),INCLU(I),NOBS
        ELSE
          WRITE (IOUT,689)IM1
        ENDIF
        IF(I == ITERP+1 .AND. DATAEXCHANGE) THEN
          FN = TRIM(OUTNAM)//'._summary'
          IUSUMMARY = UTL_GETUNIT(101,150)
          OPEN(UNIT=IUSUMMARY,FILE=FN,STATUS='OLD',ACCESS="SEQUENTIAL", &
               POSITION="APPEND")
          WRITE(IUSUMMARY,690) &
               I-1,RSQD(I),RSQALL(I)-RSQD(I),RSQALL(I),INCLU(I),NOBS
          CLOSE(IUSUMMARY)
        ENDIF
      ELSE
        !     IF USING TRUSTREGION, SET FINAL VALUES TO LAST ITERATION
        II=I
        IF(TRUSTREGION) II=IM1
        IF(STATS_ON_NONCONVERGE) THEN
          WRITE(IOUT,6951)RSQD(II),RSQALL(II)-RSQD(II),RSQALL(II),INCLU(II),NOBS
          WRITE(IOUT,6952)ILOW-1
        ENDIF
      ENDIF
    ENDDO
    IF(REACT == 1 .AND. RSQALL(ITERP+2) > RSQALL(ILOW)) THEN
      CALL UTLUCODE_PAR_OMIT_REACT1(ILOW-1,IOUT,RSQALL(ITERP+2),RSQALL(ILOW))
      UNUSUAL = .TRUE.
    ENDIF
    IF(ILOW == 1) THEN
      WRITE (IOUT,696)
      UNUSUAL = .TRUE.
    ENDIF
    RETURN
  END SUBROUTINE REG_GNMOD_EVA_FINISH
!===============================================================================
!===============================================================================
  SUBROUTINE REG_GNMOD_EVA_ORD_NORM_RESIDS (IFAIL,IOUT,MPR,NOBS, &
                          OBSNAM,OMIT,PRINAM,RSQALL1, &
                          WTDRESIDS,WTDRESIDSPRI,STAT1,STAT2)
    ! VERSION 20040604 EPP
    !---- MODIFIED FROM MF2K  OBS1BAS6REVERSION 1001 01JUN1993
    !     VERSION 20000509 ERB
    !******************************************************************
    !     MEASURE OF NORMALITY AND INDEPENDENCE
    !******************************************************************
    !        SPECIFICATIONS:
    !------------------------------------------------------------------
    INTEGER,                     INTENT(INOUT) :: IFAIL
    INTEGER,                     INTENT(IN) :: IOUT
    INTEGER,                     INTENT(IN) :: MPR
    INTEGER,                     INTENT(IN) :: NOBS
    CHARACTER(LEN=LENDNAM),      INTENT(IN) :: OBSNAM(NOBS)
    INTEGER,                     INTENT(IN) :: OMIT(NOBS)
    CHARACTER(LEN=LENDNAM),      INTENT(IN) :: PRINAM(MPR)
    DOUBLE PRECISION,            INTENT(IN) :: RSQALL1
    DOUBLE PRECISION,            INTENT(IN) :: WTDRESIDS(NOBS)
    DOUBLE PRECISION,            INTENT(IN) :: WTDRESIDSPRI(MPR)
    DOUBLE PRECISION,            INTENT(INOUT) :: STAT1
    DOUBLE PRECISION,            INTENT(INOUT) :: STAT2
    ! Local Variables
    DOUBLE PRECISION                                  :: AVE
    DOUBLE PRECISION                                  :: AVET
    DOUBLE PRECISION                                  :: DEN1
    DOUBLE PRECISION                                  :: DEN2
    DOUBLE PRECISION                                  :: DIF
    DOUBLE PRECISION                                  :: HL
    CHARACTER(LEN=LENDNAM), ALLOCATABLE, DIMENSION(:) :: HOLDOBSNAM
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)       :: HOLDWTDRESIDS
    CHARACTER(LEN=1), ALLOCATABLE, DIMENSION(:)       :: HOLDTYPE
    DOUBLE PRECISION                                  :: HS
    CHARACTER(LEN=1)                                  :: HTYPE
    CHARACTER(LEN=1)                                  :: HTYPEL
    CHARACTER(LEN=1)                                  :: HTYPES
    INTEGER                                           :: I
    INTEGER, ALLOCATABLE, DIMENSION(:)                :: IOBSEQ
    INTEGER                                           :: ISEQMIN
    INTEGER                                           :: J
    INTEGER                                           :: N
    CHARACTER(LEN=LENDNAM)                            :: NAML
    CHARACTER(LEN=LENDNAM)                            :: NAMLU
    CHARACTER(LEN=LENDNAM)                            :: NAMS
    CHARACTER(LEN=LENDNAM)                            :: NAMSU
    INTEGER                                           :: NMIN
    INTEGER                                           :: NN
    INTEGER                                           :: NND
    INTEGER                                           :: NREST
    CHARACTER(LEN=LENDNAM)                            :: OBSMIN
    DOUBLE PRECISION                                  :: RMIN
    DOUBLE PRECISION                                  :: RNORM
    DOUBLE PRECISION                                  :: RNUM
    DOUBLE PRECISION                                  :: U
      !
    460 FORMAT (/,1X,'SMALLEST AND LARGEST WEIGHTED RESIDUALS')
    461 FORMAT (/,13X,'SMALLEST WEIGHTED RESIDUALS',/,22X, &
        'WEIGHTED   PERCENT OF   ',/,1X,'NAME',17X,'RESIDUAL    OBJ FUNC')
    462 FORMAT (/,13X,'LARGEST  WEIGHTED RESIDUALS',/,22X, &
        'WEIGHTED   PERCENT OF   ',/,1X,'NAME',17X,'RESIDUAL    OBJ FUNC')
    470 FORMAT (1X,A,1X,A,1X,G10.3,3X,F6.2,5X)
    502 FORMAT(/,' COULD NOT CALCULATE THE CORRELATION BETWEEN ORDERED', &
        ' WEIGHTED RESIDUALS AND',/, &
        ' NORMAL ORDER STATISTICS (EQ.38 OF TEXT)')
    515 FORMAT (/,' CORRELATION BETWEEN ORDERED WEIGHTED RESIDUALS AND', &
        ' NORMAL ORDER STATISTICS',/,' FOR OBSERVATIONS =',G13.3)
    516 FORMAT (/,' CORRELATION BETWEEN ORDERED WEIGHTED RESIDUALS AND', &
        ' NORMAL ORDER STATISTICS',/,' FOR OBSERVATIONS AND', &
        ' PRIOR INFORMATION =',G13.3)
    !
    ALLOCATE(HOLDOBSNAM(NOBS+MPR),HOLDWTDRESIDS(NOBS+MPR),HOLDTYPE(NOBS+MPR), &
             IOBSEQ(NOBS+MPR))
    !
    ! Initialize
    AMESSAGE = ' '
    ERRSUB=' Error in subroutine REG_GNMOD_EVA_ORD_NORM_RESIDS'
    IFAIL = 0
    HOLDOBSNAM = ' '
    HOLDTYPE = ' '
    HOLDWTDRESIDS = 0.D0
    IOBSEQ = 0
    AVET = 0.
    U = 0.
    NND = 0
    !     POPULATE IOBSEQ WITH ORIGINAL OBSERVATION SEQUENCE NUMBERS.  IF
    !     H AND OBSNAM ARRAYS NEED TO BE KEPT IN ORDER, THIS CODE COULD BE
    !     MODIFIED TO USE IOBSEQ AS A POINTER ARRAY, AND H AND OBSNAM COULD
    !     BE LEFT AS IS
    DO I = 1, NOBS
      IOBSEQ(I) = I
      HOLDOBSNAM(I) = OBSNAM(I)
      HOLDWTDRESIDS(I) = WTDRESIDS(I)
    ENDDO
    !---- FIND INCLUDED DEPENDENTS
    DO N = 1, NOBS
      IF (OMIT(N) .GE. 0) THEN
        NND = NND + 1
        HOLDWTDRESIDS(NND) = HOLDWTDRESIDS(N)
        HOLDOBSNAM(NND) = HOLDOBSNAM(N)
        AVET = AVET + HOLDWTDRESIDS(N)
      ENDIF
    ENDDO
    IF(NND <= 0) THEN
     IFAIL = 1
     AMESSAGE = ' NO INCLUDED OBSERVATIONS, REVIEW APPLICATION CODE OUTPUT '
     WRITE(*,*)AMESSAGE
     IF(IFAIL > 0) CALL UTL_WRITE_MESSAGE(IOUT,'yes','yes','yes')
     DEALLOCATE(HOLDOBSNAM,HOLDWTDRESIDS,HOLDTYPE,IOBSEQ)
     RETURN
    ENDIF
    !---- -TEST FOR NORMALITY OF THE DEPENDENT-VARIABLE WEIGHTED RESIDUALS
    !---- --CALCULATE THE MEAN
    AVE = AVET/(NND)
    !---- --ORDER THE RESIDUALS
    DO NN = 1, NND-1
      NMIN = NN
      RMIN = HOLDWTDRESIDS(NN)
      OBSMIN = HOLDOBSNAM(NN)
      ISEQMIN = IOBSEQ(NN)
      DO N = NN+1, NND
        IF (HOLDWTDRESIDS(N).LE.RMIN) THEN
          RMIN = HOLDWTDRESIDS(N)
          OBSMIN = HOLDOBSNAM(N)
          ISEQMIN = IOBSEQ(N)
          NMIN = N
        ENDIF
      ENDDO
      IF (NMIN.NE.NN) THEN
        HOLDWTDRESIDS(NMIN) = HOLDWTDRESIDS(NN)
        HOLDWTDRESIDS(NN) = RMIN
        HOLDOBSNAM(NMIN) = HOLDOBSNAM(NN)
        HOLDOBSNAM(NN) = OBSMIN
        IOBSEQ(NMIN) = IOBSEQ(NN)
        IOBSEQ(NN) = ISEQMIN
      ENDIF
    ENDDO
    !---- -CALCULATE THE STATISTIC
    RNUM = 0.D0
    DEN1 = 0.D0
    DEN2 = 0.D0
    DO N = 1, NND
      RNORM = ((N)-.5)/(NND)
      CALL STA_EVA_PROB_NORM_DISTRB(U,RNORM,-1)
      DIF = HOLDWTDRESIDS(N) - AVE
      RNUM = RNUM + DIF*U
      DEN1 = DEN1 + DIF**2
      DEN2 = DEN2 + U**2
    ENDDO
    STAT1 = -1.D0
    IF(DEN1*DEN2 > 0.) THEN
      STAT1 = RNUM**2/(DEN1*DEN2)
    ENDIF
    STAT2 = -1.D0
    !---- DEPENDENTS WITH PRIOR
    IF (MPR>0) THEN
      DO I = NND+1, NOBS
        IOBSEQ(I) = 0
        HOLDOBSNAM(I) = ' '
        HOLDWTDRESIDS(I) = 0.D0
      ENDDO
      J = 0
      DO I = 1, MPR
        J = J + 1
        IOBSEQ(NND+I) = NND+I
        HOLDOBSNAM(NND+J) = PRINAM(I)
        HOLDTYPE(NND+J) = '*'
        HOLDWTDRESIDS(NND+J) = WTDRESIDSPRI(I)
        AVET = AVET + WTDRESIDSPRI(I)
      ENDDO
      !---- -TEST FOR NORMALITY OF THE DEPENDENT-VARIABLE WEIGHTED RESIDUALS
      !---- --CALCULATE THE MEAN
      AVE = AVET/(NND+MPR)
      !---- --ORDER THE RESIDUALS
      DO NN = 1, NND+MPR-1
        NMIN = NN
        RMIN = HOLDWTDRESIDS(NN)
        OBSMIN = HOLDOBSNAM(NN)
        ISEQMIN = IOBSEQ(NN)
        DO N = NN+1, NND+MPR
          IF (HOLDWTDRESIDS(N).LE.RMIN) THEN
            RMIN = HOLDWTDRESIDS(N)
            HTYPE = HOLDTYPE(N)
            OBSMIN = HOLDOBSNAM(N)
            ISEQMIN = IOBSEQ(N)
            NMIN = N
          ENDIF
        ENDDO
        IF (NMIN.NE.NN) THEN
          HOLDWTDRESIDS(NMIN) = HOLDWTDRESIDS(NN)
          HOLDWTDRESIDS(NN) = RMIN
          HOLDTYPE(NMIN) = HOLDTYPE(NN)
          HOLDTYPE(NN) = HTYPE
          HOLDOBSNAM(NMIN) = HOLDOBSNAM(NN)
          HOLDOBSNAM(NN) = OBSMIN
          IOBSEQ(NMIN) = IOBSEQ(NN)
          IOBSEQ(NN) = ISEQMIN
        ENDIF
      ENDDO
      !---- -CALCULATE THE STATISTIC
      RNUM = 0.D0
      DEN1 = 0.D0
      DEN2 = 0.D0
      DO N = 1, NND+MPR
        RNORM = ((N)-.5)/(NND+MPR)
        CALL STA_EVA_PROB_NORM_DISTRB(U,RNORM,-1)
        DIF = HOLDWTDRESIDS(N) - AVE
        RNUM = RNUM + DIF*U
        DEN1 = DEN1 + DIF**2
        DEN2 = DEN2 + U**2
      ENDDO
      STAT2 = -1.D0
      IF(DEN1*DEN2 > 0.) THEN
        STAT2 = RNUM**2/(DEN1*DEN2)
      ENDIF
    ENDIF
    !
    !---- PRINT TABLE OF SMALLEST AND LARGEST WEIGHTED RESIDUALS
    WRITE (IOUT,460)
    IF (NND+MPR >= 10) THEN
      NREST = 5
    ELSE
      NREST = (NND+MPR)/2
    ENDIF
    WRITE (IOUT,461)
    DO I = 1,NREST
      HS = HOLDWTDRESIDS(I)
      HTYPES = HOLDTYPE(I)
      NAMS = HOLDOBSNAM(I)
      CALL UTL_CASE(NAMS,NAMSU,1)
      WRITE (IOUT,470) NAMS, HTYPES, HS, 100.0*HS**2/RSQALL1
    ENDDO
    WRITE (IOUT,462)
    DO I = 1,NREST
      HL = HOLDWTDRESIDS(NND+MPR+1-I)
      HTYPEL = HOLDTYPE(NND+MPR+1-I)
      NAML = HOLDOBSNAM(NND+MPR+1-I)
      CALL UTL_CASE(NAML,NAMLU,1)
      WRITE (IOUT,470) NAML,  HTYPEL, HL, 100.0*HL**2/RSQALL1
    ENDDO
    !
    !     WRITE STATISTICS (R2N STATISTIC) FOR CORRELATION BETWEEN ORDERED
    !     WEIGHTED RESIDUALS AND NORMAL ORDER STATISTICS, AND COMMENTS ON
    !     INTERPRETING THEM -- WRITE OUTPUT TO BOTH GLOBAL AND LIST FILES
    !     UNLESS GLOBAL AND LIST OUTPUT GO TO THE SAME FILE
    IF (STAT1 > 0.) THEN
      WRITE (IOUT,515) STAT1
    ELSE
      WRITE(IOUT,502)
    ENDIF
    IF (STAT2 > 0.0) THEN
      WRITE (IOUT,516) STAT2
    ENDIF
    CALL STA_EVA_COMMENTS(IOUT,NND,MPR)
    !
    DEALLOCATE(HOLDOBSNAM,HOLDWTDRESIDS,HOLDTYPE,IOBSEQ)
    RETURN
  END SUBROUTINE REG_GNMOD_EVA_ORD_NORM_RESIDS
!===============================================================================
!===============================================================================
  SUBROUTINE REG_GNMOD_CLN ()
  ! Deallocate and write status at end of execution
  IMPLICIT NONE
    ! Confirm Deallocate
    !
    IF (ALLOCATED(AAP)) DEALLOCATE(AAP)
    IF (ALLOCATED(AMPA)) DEALLOCATE(AMPA)
    IF (ALLOCATED(AMCA)) DEALLOCATE(AMCA)
    IF (ALLOCATED(CMAT)) DEALLOCATE(CMAT)
    IF (ALLOCATED(DDV)) DEALLOCATE(DDV)
    IF (ALLOCATED(DDVC)) DEALLOCATE(DDVC)
    IF (ALLOCATED(DMXA)) DEALLOCATE(DMXA)
    IF (ALLOCATED(GG)) DEALLOCATE(GG)
    IF (ALLOCATED(GGQN)) DEALLOCATE(GGQN)
    IF (ALLOCATED(GD)) DEALLOCATE(GD)
    IF (ALLOCATED(NPDAMP)) DEALLOCATE(NPDAMP)
    IF (ALLOCATED(NPMAXCHG)) DEALLOCATE(NPMAXCHG)
    IF (ALLOCATED(RD)) DEALLOCATE(RD)
    IF (ALLOCATED(SCLE)) DEALLOCATE(SCLE)
    IF (ALLOCATED(WTADJ)) DEALLOCATE(WTADJ)
    IF (ALLOCATED(WTADJF)) DEALLOCATE(WTADJF)
    IF (ALLOCATED(XD)) DEALLOCATE(XD)
    IF (ALLOCATED(XW)) DEALLOCATE(XW)
    RETURN
  END SUBROUTINE REG_GNMOD_CLN
!===============================================================================
!===============================================================================
END MODULE REG_GNMOD
