MODULE REG_TRUSTMOD
!
!
  IMPLICIT NONE
  PRIVATE
  ! Public subprograms
  PUBLIC  REG_TR_MODELHESS, REG_TR_CHOLSOLVE, REG_TR_DOGDRIVE, &
          REG_TR_HOOKDRIVE, REG_TR_STOP, REG_TR_L2NORM
  CONTAINS
!=======================================================================
!=======================================================================
  SUBROUTINE REG_TR_MODELHESS(MAXADD,MACHEPS,N,SCLE,CMAT,L,TEMPC)
    !  THIS SUBROUTINE FORMULATES THE DECOMPOSED MODEL HESSIAN USING A
    !  MODIFIED CHOLESKY DECOMPOSITION
    !*******************************************************************
    !        SPECIFICATIONS:
    !------------------------------------------------------------------
    IMPLICIT NONE
    ! ARGUMENT-LIST VARIABLES
    DOUBLE PRECISION,                INTENT(IN)    :: MACHEPS
    INTEGER,                         INTENT(IN)    :: N
    DOUBLE PRECISION,                INTENT(IN)    :: SCLE(N)
    DOUBLE PRECISION,                INTENT(INOUT) :: CMAT(N,N)
    DOUBLE PRECISION,                INTENT(OUT)   :: L(N,N)
    DOUBLE PRECISION,                INTENT(OUT)   :: MAXADD
    DOUBLE PRECISION,                INTENT(OUT)   :: TEMPC(N,N)
    ! LOCAL VARIABLES
    INTEGER                                        :: I, J
    DOUBLE PRECISION                               :: MAXADD2
    !------------------------------------------------------------------
    !
    ! PUT IN SCALING ALGORITHM
    DO I = 1,N
      DO J = I,N
        CMAT(I,J)=CMAT(I,J)/SCLE(I)/SCLE(J)
      ENDDO
    ENDDO
    CALL CHOLDECOMP(MACHEPS,N,CMAT,L,MAXADD,TEMPC)
    ! IF MAXADD IS > 0, THEN HESSIAN WAS NOT POS. DEF.  ADD TERM
    ! "MAXADD" ONTO DIAGONALS, AND THEN RE-DECOMPOSE THE MATRIX.
    ! MAXADD CAN BE THOUGHT OF AS A MARQUARDT PARAMETER
    ! STORE MAXADD IN MAXADD2 AND RESTORE FOR PRINTING
    MAXADD2 = 0.D0
    IF(MAXADD .GT. 0.D0)THEN
      MAXADD2=MAXADD
      DO I = 1,N
        CMAT(I,I)=CMAT(I,I)+MAXADD
      END DO
      CALL CHOLDECOMP(MACHEPS,N,CMAT,L,MAXADD,TEMPC)
    ENDIF
    MAXADD=MAXADD2
    !
    ! UNSCALE CMAT AND L
    DO I =1,N
      DO J = I,N
        CMAT(I,J)=CMAT(I,J)*SCLE(I)*SCLE(J)
      ENDDO
      DO J = 1,I
        L(I,J)=L(I,J)*SCLE(I)
      ENDDO
    ENDDO
  END SUBROUTINE REG_TR_MODELHESS
!
!=======================================================================
!=======================================================================
  SUBROUTINE CHOLDECOMP(MACHEPS,N,TEMPC,L,MAXADD,CMAT)
    !  THIS SUBROUTINE PERFORMS A MODIFIED CHOLESKY DECOMPOSITION FOR
    !  ADDITONS TO THE DIAGONAL TO MAKE THE MATRIX POSITIVE DEFINITE (IF
    !  IT IS POSITIVE DEFINITE ALREADY, THEN IT PROCEDES NORMALLY.
    !  THE ALGORITHM IS TAKEN FROM DENNIS AND SCHABEL A5.5.2 REVISED
    !*******************************************************************
    !        SPECIFICATIONS:
    !------------------------------------------------------------------
    IMPLICIT NONE
    ! ARGUMENT-LIST VARIABLES
    DOUBLE PRECISION,                INTENT(IN)    :: MACHEPS
    INTEGER,                         INTENT(IN)    :: N
    DOUBLE PRECISION,                INTENT(IN)    :: TEMPC(N,N)
    DOUBLE PRECISION,                INTENT(INOUT) :: CMAT(N,N)
    DOUBLE PRECISION,                INTENT(INOUT) :: L(N,N)
    DOUBLE PRECISION,                INTENT(INOUT) :: MAXADD
    ! LOCAL VARIABLES
    INTEGER                                        :: I, J, K, PHASE1
    DOUBLE PRECISION                               :: DSUM
    DOUBLE PRECISION                               :: MAXDIAG
    DOUBLE PRECISION                               :: SMALL
    DOUBLE PRECISION                               :: TEMPJJ
    DOUBLE PRECISION                               :: TERM
    DOUBLE PRECISION                               :: TOL
    !------------------------------------------------------------------

    ! STORE THE HESSIAN
    DO I = 1,N
      DO J = I,N
        CMAT(I,J)=TEMPC(I,J)
      ENDDO
    ENDDO
    ! SET PHASE1=1 IS TRUE, PHASE1=0 IS FALSE
    PHASE1=1
    ! FIND MAXIMUM ABSOLUTE DIAGIONAL TERM
    MAXDIAG=ABS(CMAT(1,1))
    DO I=2,N
      IF(ABS(CMAT(I,I)).GT. MAXDIAG)MAXDIAG=ABS(CMAT(I,I))
    END DO
!SWM      TOL=MACHEPS**(1.D0/3.D0)*MAXDIAG
    ! SET TOL ACCORDING TO SCHNABEL AND ESKOW
    TOL=MACHEPS**(2.D0/3.D0)*MAXDIAG
    IF(CMAT(1,1) .LE. TOL)PHASE1=0
    MAXADD=0.D0
    DO J = 1,N
      IF(PHASE1 .EQ. 1)THEN
        ! FIND THE MINIMUM TERM
        IF(J+1 .LE. N) SMALL=CMAT(J+1,J+1)-CMAT(J,J+1)*CMAT(J,J+1)/CMAT(J,J)
        DO I = J+2,N
          TERM=CMAT(I,I)-CMAT(J,I)*CMAT(J,I)/CMAT(J,J)
          IF(TERM .LT. SMALL)SMALL=TERM
        END DO
        ! FOUND THE MINIMUM TERM--> STORED IN SMALL
        IF(SMALL .LT. TOL)PHASE1=0
      END IF
      ! START 5.2
      IF(PHASE1 .EQ. 1)THEN
        L(J,J)=SQRT(CMAT(J,J))
      ELSE
        DSUM = 0.D0
        DO I=J+1,N
          DSUM=DSUM+ABS(CMAT(J,I))
        END DO
        TEMPJJ=MAX(CMAT(J,J),DSUM,TOL)
        L(J,J)=SQRT(TEMPJJ)
        MAXADD=MAX(MAXADD,TEMPJJ-CMAT(J,J))
      END IF
      DO I = J+1,N
        L(I,J)=CMAT(J,I)/L(J,J)
      END DO
      DO I = J+1, N
        DO K = I, N
          CMAT(I,K)=CMAT(I,K)-L(I,J)*L(K,J)
        END DO
      END DO
    END DO
    RETURN
  END SUBROUTINE CHOLDECOMP
!=======================================================================
!=======================================================================
  SUBROUTINE REG_TR_CHOLSOLVE(N,G,L,S)
    !  THIS SUBROUTINE USES CHOLESKY DECOMPOSITION TO SOLVE (LL^T)S=-G
    !  FOR S. THE ALGORITHM IS TAKEN FROM DENNIS AND SCHABEL A3.2.3
    !*********VARIABLE DICTIONARY***************************************
    !  N = THE NUMBER OF PARAMETERS
    !  L = DECOMPOSED LOWER TRIANGULAR MATRIX
    !  L^T = TRANSPOSE OF L, UPPER TRIANGULAR
    !  G = RHS --> GRADIENT VECTOR OF FUNCTION
    !  S = SOLUTION VECTOR (THE NEW STEP -->X+=Xc+S
    !*******************************************************************
    !        SPECIFICATIONS:
    !------------------------------------------------------------------
    IMPLICIT NONE
    ! ARGUMENT-LIST VARIABLES
    INTEGER,                         INTENT(IN)    :: N
    DOUBLE PRECISION,                INTENT(IN)    :: G(N)
    DOUBLE PRECISION,                INTENT(IN)    :: L(N,N)
    DOUBLE PRECISION,                INTENT(OUT)   :: S(N)
    ! LOCAL VARIABLES
    INTEGER                                        :: I
    !------------------------------------------------------------------

    CALL LSOLVE(N,G,L,S)
    CALL LTSOLVE(N,L,S,S)
    !
    ! SWITCH THE SIGNS BECAUSE ALL THIS TIME I'VE BEEN USING THE
    ! POSITIVE GRADIENT, BUT I WANT A NEGATIVE GRADIENT FOR MINIMIZATION
    DO I = 1,N
      S(I)=-S(I)
    END DO
    RETURN
  END SUBROUTINE REG_TR_CHOLSOLVE
!=======================================================================
!=======================================================================
  SUBROUTINE LSOLVE(N,B,L,Y)
    !  THIS SUBROUTINE DOES FORWARD SUBSTITUTION TO SOLVE FOR THE LOWER
    !  TRIANGLAR PORTION OF THE CHOLESKY FACTORED MATRIX.
    !*******************************************************************
    !        SPECIFICATIONS:
    !------------------------------------------------------------------
    IMPLICIT NONE
    ! ARGUMENT-LIST VARIABLES
    INTEGER,                         INTENT(IN)    :: N
    DOUBLE PRECISION,                INTENT(IN)    :: B(N)
    DOUBLE PRECISION,                INTENT(IN)    :: L(N,N)
    DOUBLE PRECISION,                INTENT(OUT)   :: Y(N)
    ! LOCAL VARIABLES
    DOUBLE PRECISION                               :: DSUM
    INTEGER                                        :: I, J
    !------------------------------------------------------------------
    !
    Y(1)=B(1)/L(1,1)
    DO I =2,N
      DSUM=0.D0
      DO J=1,I-1
        DSUM=DSUM+L(I,J)*Y(J)
      END DO
      Y(I)=(B(I)-DSUM)/L(I,I)
    END DO
    RETURN
  END SUBROUTINE LSOLVE
!=======================================================================
!=======================================================================
  SUBROUTINE LTSOLVE(N,L,Y,X)
    !  THIS SUBROUTINE DOES BACKWARD SUBSTITUTION TO SOLVE FOR THE
    !  UPPER TRIANGLAR PORTION OF THE CHOLESKY FACTORED MATRIX.
    !*****************************************************************
    !        SPECIFICATIONS:
    !------------------------------------------------------------------
    IMPLICIT NONE
    ! ARGUMENT-LIST VARIABLES
    INTEGER,                         INTENT(IN)    :: N
    DOUBLE PRECISION,                INTENT(IN)    :: L(N,N)
    DOUBLE PRECISION,                INTENT(IN)    :: Y(N)
    DOUBLE PRECISION,                INTENT(OUT)   :: X(N)
    ! LOCAL VARIABLES
    DOUBLE PRECISION                               :: DSUM
    INTEGER                                        :: I, J
    !------------------------------------------------------------------
    !
    X(N)=Y(N)/L(N,N)
    DO I=N-1,1, -1
      DSUM=0.D0
      DO J=I+1,N
        DSUM=DSUM+L(J,I)*X(J)
      END DO
      X(I)=(Y(I)-DSUM)/L(I,I)
    END DO
    RETURN
  END SUBROUTINE LTSOLVE
!
!=======================================================================
!=======================================================================
!eppsvd changes to accomodate svd added npetmp svdloop
  SUBROUTINE REG_TR_DOGDRIVE(MPR,NDOBS,NEOBS,NOBS,NPE,NPETMP,SVDLOOP,N,NPS, &
                             FC,G,IOUT,IPTR,L,LN,MAXSTEP,NPTR,PARNAM,PTOLPAR, &
                             SCLE,SNEWT,STEPTYPE,XPTR,R,FPLUS, &
                             MAXTAKEN,MODVALPREV,XNPE,XC,XPLUS, &
                             RETCODE,S,STEPTAKEN)
    !  THIS SUBROUTINE IMPLEMENTS THE DOUBLE-DOGLEG GLOBAL OPTIMIZATION
    !  APPROACH.  IT IS USED TO FIND A STEP BETWEEN THE NEWTON STEP AND
    !  THE STEEPEST DESCENT WHEN THE NEWTON STEP IS NOT SATISFACTORY.
    !  IT WILL ALSO CALL THE ROUTINE TRUSTUP TO UPDATE THE TRUST REGION.
    !  ALGORITHM 6.4.3 DENNIS AND SCHNABEL
    !  *************VARIABLE DICTIONARY*********************************
    !  ETA = THE LENGTH OF THE STEP ALONG THE NEWTON DIRECTION TO NHAT.
    !  FC = THE VALUE OF THE FUNCTION AT THE CURRENT POINT, XC(I)
    !  FIRSTDOG = FIRST TIME CALCULATING DOGLEG CURVE FOR THIS ITERATION
    !  FPLUS = THE VALUE OF THE FUNCTION AT THE NEW POINT, XPLUS(I)
    !  FPREV = THE VALUE OF THE FUNCTION AT THE PREVIOUS POINT, XPREV(I)
    !  G(I) = THE GRADIENT AT XC(I)
    !  L(I,J) = LOWER TRIANGULAR CHOLESKY FACTORIZATION OF THE HESSIAN
    !  LN = A FLAG INDICATING IF PARAMETER IS LOG TRANSFORMED
    !  MAXSTEP = THE MAXIMUM STEP ALLOWED AT ANY ITERATION
    !  MAXTAKEN = A FLAG INDICATING THE MAXIMUM STEP SIZE WAS TAKEN
    !  MODVALPREV = PREVIOUS RESULTS OF MODEL OBSERVATIONS
    !  N = NUMBER OF INCLUDED PARAMETERS (ACTIVE THIS ITERATION)
    !  NEWTLEN = THE LENGTH OF THE NEWTON STEP
    !  NEWTTAKEN = A FLAG INDICATING THE FULL NEWTON STEP WAS TAKEN
    !  NNEWT = THE NUMBER OF NEWTON STEPS TAKEN
    !  NPE = # OF POTENTIALLY ADJUSTABLE PARAMETERS (ACTIVE + OMITTED)
    !  NPS = # OF TOTAL DEFINED PARAMETERS (ADJUSTABLE + NON ADJUSTABLE)
    !  PTOLPAR = IF SUCCESSIVE STEPS ARE LESS THAN PTOLPAR ->STOP
    !  RETCODE = TERMINATION CODE OF A TRUST REGION/LINESEARCH UPDATE
    !  S(I) = THE ACTUAL STEP USED
    !  SCLE(X) = A SCALING VECTOR
    !  SDLEN = THE LENGTH OF THE STEEPEST DESCENT STEP
    !  SNEWT(I) = THE NEWTON STEP
    !  SSD(I)  = THE STEEPEST DESCENT STEP
    !  STEPTYPE = THE TYPE OF TRUST REGION STEP USED
    !  R = RADIUS OF THE TRUST REGION
    !  XC(I) = THE CURRENT SOLUTION
    !  XNPE(I) = SOLUTION VECTOR FOR ALL ACTIVE AND OMITTED PARAMETERS
    !  XPLUS(I) = THE NEW SOLUTION
    !  XPREV(I) = THE PREVIOUS SOLUTION
    !  V(I) = THE DIFFERENCE BETWEEN STEEPEST DESCENT & NEWT (NHAT) STEP
    !  IOUT = UNIT NUMBER OF THE MAIN OUTPUT FILE
    !  IPTR = A POINTER MAPPING FROM VECTOR(NPE) TO VECTOR(NPS)
    !  NPTR = A POINTER MAPPING FROM VECTOR(N) TO VECTOR(NPS)
    !  XPTR = A POINTER MAPPING FROM VECTOR(N) TO VECTOR(NPE)
    !*******************************************************************
    !        SPECIFICATIONS:
    !------------------------------------------------------------------
    IMPLICIT NONE
    ! ARGUMENT-LIST VARIABLES
    INTEGER,                         INTENT(IN)    :: MPR
    INTEGER,                         INTENT(IN)    :: N
    INTEGER,                         INTENT(IN)    :: NDOBS
    INTEGER,                         INTENT(IN)    :: NEOBS
    INTEGER,                         INTENT(IN)    :: NOBS
    INTEGER,                         INTENT(IN)    :: NPE
!eppsvd changes to accomodate svd added npetmp svdloop
    INTEGER,                         INTENT(IN)    :: NPETMP
    INTEGER,                         INTENT(IN)    :: SVDLOOP
    INTEGER,                         INTENT(IN)    :: NPS
    DOUBLE PRECISION,                INTENT(IN)    :: FC
    DOUBLE PRECISION,                INTENT(IN)    :: G(N)
    INTEGER,                         INTENT(IN)    :: IOUT
    INTEGER,                         INTENT(IN)    :: IPTR(NPE)
    DOUBLE PRECISION,                INTENT(IN)    :: L(N,N)
    INTEGER,                         INTENT(IN)    :: LN(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: MAXSTEP
    INTEGER,                         INTENT(IN)    :: NPTR(N)
    CHARACTER(LEN=12),               INTENT(IN)    :: PARNAM(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: PTOLPAR(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: SCLE(N)
    DOUBLE PRECISION,                INTENT(IN)    :: SNEWT(N)
    CHARACTER(LEN=10),               INTENT(IN)    :: STEPTYPE
    INTEGER,                         INTENT(IN)    :: XPTR(N)
    !
    DOUBLE PRECISION,                INTENT(INOUT) :: MODVALPREV(NOBS)
    DOUBLE PRECISION,                INTENT(INOUT) :: R
    DOUBLE PRECISION,                INTENT(INOUT) :: XC(N)
    DOUBLE PRECISION,                INTENT(INOUT) :: XNPE(NPE)
    !
    DOUBLE PRECISION,                INTENT(OUT)   :: FPLUS
    INTEGER,                         INTENT(OUT)   :: MAXTAKEN
    INTEGER,                         INTENT(OUT)   :: RETCODE
    DOUBLE PRECISION,                INTENT(OUT)   :: S(N)
    CHARACTER(LEN=27),               INTENT(OUT)   :: STEPTAKEN
    DOUBLE PRECISION,                INTENT(OUT)   :: XPLUS(N)
    ! LOCAL VARIABLES
    DOUBLE PRECISION                               :: ETA
    LOGICAL                                        :: FIRSTDOG
    DOUBLE PRECISION                               :: FPREV
    INTEGER                                        :: IFCNT
    DOUBLE PRECISION                               :: NEWTLEN
    INTEGER                                        :: NEWTTAKEN
    INTEGER                                        :: NNEWT
    DOUBLE PRECISION                               :: SDLEN
    !!!!DOUBLE PRECISION                           :: SSD(N)
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: SSD
    !!!!DOUBLE PRECISION                           :: XPREV(N)
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: XPREV
    !!!!DOUBLE PRECISION                           :: V(N)
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: V
    !------------------------------------------------------------------
    !
    ALLOCATE(SSD(N),XPREV(N),V(N))
    !
    ! SET RETCODE = 4 AND FIRSTDOG FOR INITIAL CALL
    RETCODE = 4
    FIRSTDOG = .TRUE.
    ! CALCULATE THE LENGTH OF THE NEWTON STEP
    NEWTLEN=REG_TR_L2NORM(N,1.D0,SCLE,SNEWT)
    !
    ! FIND THE STEP WITHIN THE TRUST REGION
    DO WHILE (RETCODE .GE. 2)
      CALL DOGSTEP(N,G,L,MAXSTEP,NEWTLEN,SCLE,SNEWT,ETA,FIRSTDOG, &
                     NNEWT,R,SDLEN,SSD,V,NEWTTAKEN,S,STEPTAKEN)
      !
      !  WE NOW HAVE THE NEW STEP, S(I), CALCULATED FROM THE DOUBLE
      !  DOGLEG.  UPDATE THE TRUST REGION AS ACCORDINGLY.
      !  SECOND L IS A DUMMY VARIABLE IN CALL BELOW
!eppsvd changes to accomodate svd added npetmp svdloop
      CALL TRUSTUP(MPR,N,NDOBS,NEOBS,NEWTTAKEN,NOBS,NPE,NPETMP,SVDLOOP,NPS, &
                   FC,G,L,IOUT,IPTR,L,LN,MAXSTEP,NPTR,PARNAM,PTOLPAR, &
                   S,SCLE,STEPTYPE,XPTR,FPREV,MODVALPREV,R,XC,XNPE, &
                   XPREV,FPLUS,MAXTAKEN,RETCODE,XPLUS)
      IFCNT=IFCNT+1
    ENDDO
    DEALLOCATE(SSD,XPREV,V)
    RETURN
  END SUBROUTINE REG_TR_DOGDRIVE
!
!=======================================================================
!=======================================================================
  SUBROUTINE DOGSTEP(N,G,L,MAXSTEP,NEWTLEN,SCLE,SNEWT,ETA,FIRSTDOG, &
                     NNEWT,R,SDLEN,SSD,V,NEWTTAKEN,S,STEPTAKEN)
    !  THIS SUBROUTINE CALCULATES THE STEP ALONG THE DOUBLE-DOGLEG
    !  CURVE.
    !  ALGORITHM A6.4.4 DENNIS AND SCHNABEL
    !  ******************VARIABLE DICTIONARY****************************
    !  A = SCALED NORM OF GRADIENT;NUMERATOR OF STEEPEST DESCENT CALC.
    !  B = SCALED NORM OF G^T*H*G; RECALL THAT L^T*L=H; DENOMINATOR "  "
    !  ETA = LENGTH ALONG THE NEWTON STEP TO NHAT; GAMMA<ETA<1
    !  FIRSTDOG = FIRST TIME CALCULATING DOGLEG CURVE FOR THIS ITERATION
    !  G(I) = THE GRADIENT AT XC(I)
    !  L(I,J) = LOWER TRIANGULAR CHOLESKY FACTORIZATION OF THE HESSIAN
    !  LAMBDA = DISTANCE ALONG SECOND "DOGLEG" PORTION BETWEEN SD, AND
    !           NHAT.  LAMBDA IS CALCULATED VIA THE QUAD. FORMULA SUCH
    !           THAT THE LENGTH IS EXACTLY EQUAL TO THE TRUST RADIUS, R
    !  MAXSTEP = THE MAXIMUM STEP ALLOWED AT ANY ITERATION
    !  N = NUMBER OF INCLUDED PARAMETERS (ACTIVE THIS ITERATION)
    !  NEWTLEN = THE LENGTH OF THE NEWTON STEP
    !  NEWTTAKEN = A FLAG INDICATING THE FULL NEWTON STEP WAS TAKEN
    !  NNEWT = THE NUMBER OF NEWTON STEPS TAKEN
    !  R = RADIUS OF THE TRUST REGION
    !  S(I) = THE ACTUAL STEP USED
    !  SCLE(X) = A SCALING VECTOR
    !  SDLEN = THE LENGTH OF THE STEEPEST DESCENT STEP
    !  SNEWT(I) = THE NEWTON STEP
    !  SSD = STEEPEST DESCENT STEP; --> A/B*G(I)
    !  STEPTAKEN = INDICATES THE TYPE OF STEP TAKEN (NEWT, DOGLEG, ETc.)
    !  V(I) = DIFFERENCE BETWEEN THE STEEPEST DESCENT & NEWT (NHAT) STEP
    !*******************************************************************
    !        SPECIFICATIONS:
    !------------------------------------------------------------------
    IMPLICIT NONE
    ! ARGUMENT-LIST VARIABLES
    INTEGER,                         INTENT(IN)    :: N
    DOUBLE PRECISION,                INTENT(IN)    :: G(N)
    DOUBLE PRECISION,                INTENT(IN)    :: L(N,N)
    DOUBLE PRECISION,                INTENT(IN)    :: MAXSTEP
    DOUBLE PRECISION,                INTENT(IN)    :: NEWTLEN
    DOUBLE PRECISION,                INTENT(IN)    :: SCLE(N)
    DOUBLE PRECISION,                INTENT(IN)    :: SNEWT(N)
    !
    DOUBLE PRECISION,                INTENT(INOUT) :: ETA
    LOGICAL,                         INTENT(INOUT) :: FIRSTDOG
    INTEGER,                         INTENT(INOUT) :: NNEWT
    DOUBLE PRECISION,                INTENT(INOUT) :: R
    DOUBLE PRECISION,                INTENT(INOUT) :: SDLEN
    DOUBLE PRECISION,                INTENT(INOUT) :: SSD(N)
    DOUBLE PRECISION,                INTENT(INOUT) :: V(N)
    !
    INTEGER,                         INTENT(OUT)   :: NEWTTAKEN
    DOUBLE PRECISION,                INTENT(OUT)   :: S(N)
    CHARACTER(LEN=27),               INTENT(OUT)   :: STEPTAKEN
    ! LOCAL VARIABLES
    DOUBLE PRECISION                               :: A
    DOUBLE PRECISION                               :: B
    DOUBLE PRECISION                               :: LAMBDA
    INTEGER                                        :: I, J
    DOUBLE PRECISION                               :: TEMP
    DOUBLE PRECISION                               :: TEMPV
    !------------------------------------------------------------------
    !
    ! IF THE NEWTON STEP FALLS W/IN THE TRUST REGION, USE IT,
    ! AND SET TRUST RADIUS = TO THE LENGTH OF THE NEWTON STEP
    IF(NEWTLEN .LT. R )THEN
      DO I = 1,N
        S(I)=SNEWT(I)
      ENDDO
      NEWTTAKEN = 1
      NNEWT=NNEWT+1
      R = NEWTLEN
      STEPTAKEN = 'FULL NEWTON'
    ELSE
      ! NEWTON STEP WAS NO GOOD.  LETS TRY THE DOGLEG.
      NEWTTAKEN = 0
      IF(FIRSTDOG)THEN
        FIRSTDOG = .FALSE.
        ! CALCULATE THE NUMERATOR (G^T*G) AND DENOMINATOR (G^T*H*G) FOR
        ! THE LENGTH OF THE STEEPEST DESCENT STEP.
        A=REG_TR_L2NORM(N,-1.D0,SCLE,G)
        A = A*A
        B = 0.D0
        DO I = 1,N
          TEMP=0.D0
          DO J = I,N
            TEMP = TEMP + L(J,I)*G(J)/SCLE(J)/SCLE(J)
          ENDDO
          B=B+TEMP*TEMP
        ENDDO
        ! CALCULATE THE STEEPEST DESCENT STEP.
        ! CALCULATE G^T*SNEWT-->ETA
        ETA = 0.D0
        DO I = 1,N
          SSD(I) = -A/B/SCLE(I)*G(I)
          ETA = ETA + G(I)*SNEWT(I)
        ENDDO
        SDLEN = A*SQRT(A)/B
        ETA = 0.2D0+0.8D0*A*A/B/ABS(ETA)
        DO I = 1,N
          V(I)=ETA*SCLE(I)*SNEWT(I)-SSD(I)
        ENDDO
        ! CHECK TRUST REGION FOR FIRST ITERATION
        IF(R .EQ. -1)R=MIN(SDLEN,MAXSTEP)
      ENDIF
      ! IF NHAT IS INSIDE TRUST REGION, TAKE NEWT STEP TO EDGE OF
      ! TRUST REGION RADIUS.
      IF(ETA*NEWTLEN .LE. R)THEN
        DO I = 1,N
          S(I)=R/NEWTLEN*SNEWT(I)
        ENDDO
        STEPTAKEN = 'RESTRICTED NEWTON'
      ! IF STEEPEST DESCENT STEP IS TOO LONG, TAKE STEEPEST DESCENT
      ! STEP OF LENGTH R
      ELSEIF(SDLEN .GE. R)THEN
        DO I = 1,N
          S(I)=R/SDLEN/SCLE(I)*SSD(I)
        ENDDO
        STEPTAKEN = 'RESTRICTED STEEPEST DESCENT'
      ! TAKE A STEP BETWEEN STEEPEST DESCENT AND NHAT --> DOGLEG
      ELSE
        TEMP = 0.D0
        TEMPV = 0.D0
        DO I = 1,N
          TEMP=TEMP+V(I)*SSD(I)
          TEMPV = TEMPV + V(I)*V(I)
        ENDDO
        LAMBDA =(-TEMP+SQRT(TEMP*TEMP-TEMPV*(SDLEN*SDLEN-R*R))) &
                /TEMPV
        DO I = 1,N
          S(I)=(SSD(I)+LAMBDA*V(I))/SCLE(I)
        ENDDO
        STEPTAKEN = 'DOUBLE-DOGLEG'
      ENDIF
    ENDIF
    !
    RETURN
  END SUBROUTINE DOGSTEP
!
!=======================================================================
!=======================================================================
!eppsvd changes to accomodate svd added npetmp svdloop
  SUBROUTINE REG_TR_HOOKDRIVE(MPR,NDOBS,NEOBS,NOBS,NPE,NPETMP,SVDLOOP,N,NPS, &
                             FC,G,IOUT,IPTR,ITERP,LN,MACHEPS,MAXSTEP, &
                             NPTR,PARNAM,PTOLPAR,SCLE,SNEWT,STEPTYPE, &
                             XPTR,H,L,MU,MUC,PHI,PHID,R,RPREV,FPLUS, &
                             MAXTAKEN,MODVALPREV,XNPE,XC,XPLUS, &
                             RETCODE,S)
    !  THIS SUBROUTINE IMPLEMENTS THE HOOKSTEOP GLOBAL OPTIMIZATION
    !  APPROACH.  IT IS USED TO FIND A STEP BETWEEN THE NEWTON STEP AND
    !  THE STEEPEST DESCENT WHEN THE NEWTON STEP IS NOT SATISFACTORY.
    !  IT WILL ALSO CALL THE ROUTINE TRUSTUP TO UPDATE THE TRUST REGION.
    !  ALGORITHM 6.4.1 DENNIS AND SCHNABEL
    !  *************VARIABLE DICTIONARY*********************************
    !  A = SCALED NORM OF GRADIENT;NUMERATOR OF STEEPEST DESCENT CALC.
    !  B = SCALED NORM OF G^T*H*G; RECALL THAT L^T*L=H; DENOMINATOR "  "
    !  FC = THE VALUE OF THE FUNCTION AT THE CURRENT POINT, XC(I)
    !  FIRSTHOOk = FIRST TIME CALCULATING HOOKSTEP CURVE FOR THIS ITERATION
    !  FPLUS = THE VALUE OF THE FUNCTION AT THE NEW POINT, XPLUS(I)
    !  FPREV = THE VALUE OF THE FUNCTION AT THE PREVIOUS POINT, XPREV(I)
    !  G(I) = THE GRADIENT AT XC(I)
    !  H(I,J) = HESSIAN (X^T*w*X) -- CMATRD IN CALL
    !  ITERP = PAREMETER ESTIMATION ITERATION NUMBER
    !  L(I,J) = LOWER TRIANGULAR CHOLESKY FACTORIZATION OF THE HESSIAN
    !  LN = A FLAG INDICATING IF PARAMETER IS LOG TRANSFORMED
    !  MAXSTEP = THE MAXIMUM STEP ALLOWED AT ANY ITERATION
    !  MAXTAKEN = A FLAG INDICATING THE MAXIMUM STEP SIZE WAS TAKEN
    !  MODVALPREV = PREVIOUS RESULTS OF MODEL OBSERVATIONS
    !  MU = HOOKSTEP BASED MARQUARDT PARAMETER
    !  MUC = CURRENTLY APPLIED VALUE OF MU
    !  N = NUMBER OF INCLUDED PARAMETERS (ACTIVE THIS ITERATION)
    !  NEWTLEN = THE LENGTH OF THE NEWTON STEP
    !  NEWTTAKEN = A FLAG INDICATING THE FULL NEWTON STEP WAS TAKEN
    !  NNEWT = THE NUMBER OF NEWTON STEPS TAKEN
    !  NPE = # OF POTENTIALLY ADJUSTABLE PARAMETERS (ACTIVE + OMITTED)
    !  NPS = # OF TOTAL DEFINED PARAMETERS (ADJUSTABLE + NON ADJUSTABLE)
    !  PTOLPAR = IF SUCCESSIVE STEPS ARE LESS THAN PTOLPAR ->STOP
    !  RETCODE = TERMINATION CODE OF A TRUST REGION/LINESEARCH UPDATE
    !  S(I) = THE ACTUAL STEP USED
    !  SCLE(X) = A SCALING VECTOR
    !  SNEWT(I) = THE NEWTON STEP
    !  STEPTYPE = THE TYPE OF TRUST REGION STEP USED
    !  MU = HOOKSTEP BASED MARQUARDT PARAMETER
    !  PHI = DIFFERENCE BETWEEN R AND STEP LENGTH BASED ON MU
    !  PHID = DERIVATIVE OF PHI W.R.T MU
    !  R = RADIUS OF THE TRUST REGION
    !  RPREV = RADIUS OF THE TRUST REGION ON PREVIOUS ITERATION
    !  XC(I) = THE CURRENT SOLUTION
    !  XNPE(I) = SOLUTION VECTOR FOR ALL ACTIVE AND OMITTED PARAMETERS
    !  XPLUS(I) = THE NEW SOLUTION
    !  XPREV(I) = THE PREVIOUS SOLUTION
    !  IOUT = UNIT NUMBER OF THE MAIN OUTPUT FILE
    !  IPTR = A POINTER MAPPING FROM VECTOR(NPE) TO VECTOR(NPS)
    !  NPTR = A POINTER MAPPING FROM VECTOR(N) TO VECTOR(NPS)
    !  XPTR = A POINTER MAPPING FROM VECTOR(N) TO VECTOR(NPE)
    !*******************************************************************
    !        SPECIFICATIONS:
    !------------------------------------------------------------------
    IMPLICIT NONE
    ! ARGUMENT-LIST VARIABLES
    INTEGER,                         INTENT(IN)    :: MPR
    INTEGER,                         INTENT(IN)    :: N
    INTEGER,                         INTENT(IN)    :: NDOBS
    INTEGER,                         INTENT(IN)    :: NEOBS
    INTEGER,                         INTENT(IN)    :: NOBS
    INTEGER,                         INTENT(IN)    :: NPE
!eppsvd changes to accomodate svd added npetmp svdloop
    INTEGER,                         INTENT(IN)    :: NPETMP
    INTEGER,                         INTENT(IN)    :: SVDLOOP
    INTEGER,                         INTENT(IN)    :: NPS
    DOUBLE PRECISION,                INTENT(IN)    :: FC
    DOUBLE PRECISION,                INTENT(IN)    :: G(N)
    INTEGER,                         INTENT(IN)    :: IOUT
    INTEGER,                         INTENT(IN)    :: IPTR(NPE)
    INTEGER,                         INTENT(IN)    :: ITERP
    INTEGER,                         INTENT(IN)    :: LN(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: MACHEPS
    DOUBLE PRECISION,                INTENT(IN)    :: MAXSTEP
    INTEGER,                         INTENT(IN)    :: NPTR(N)
    CHARACTER(LEN=12),               INTENT(IN)    :: PARNAM(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: PTOLPAR(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: SCLE(N)
    DOUBLE PRECISION,                INTENT(IN)    :: SNEWT(N)
    CHARACTER(LEN=10),               INTENT(IN)    :: STEPTYPE
    INTEGER,                         INTENT(IN)    :: XPTR(N)
    !
    DOUBLE PRECISION,                INTENT(INOUT) :: H(N,N)
    DOUBLE PRECISION,                INTENT(INOUT) :: L(N,N)
    DOUBLE PRECISION,                INTENT(INOUT) :: MODVALPREV(NOBS)
    DOUBLE PRECISION,                INTENT(INOUT) :: MU
    DOUBLE PRECISION,                INTENT(INOUT) :: MUC
    DOUBLE PRECISION,                INTENT(INOUT) :: PHI
    DOUBLE PRECISION,                INTENT(INOUT) :: PHID
    DOUBLE PRECISION,                INTENT(INOUT) :: R
    DOUBLE PRECISION,                INTENT(INOUT) :: RPREV
    DOUBLE PRECISION,                INTENT(INOUT) :: XC(N)
    DOUBLE PRECISION,                INTENT(INOUT) :: XNPE(NPE)
    !
    DOUBLE PRECISION,                INTENT(OUT)   :: FPLUS
    INTEGER,                         INTENT(OUT)   :: MAXTAKEN
    INTEGER,                         INTENT(OUT)   :: RETCODE
    DOUBLE PRECISION,                INTENT(OUT)   :: S(N)
    DOUBLE PRECISION,                INTENT(OUT)   :: XPLUS(N)
    ! LOCAL VARIABLES
    DOUBLE PRECISION                               :: A
    DOUBLE PRECISION                               :: B
    LOGICAL                                        :: FIRSTHOOK
    DOUBLE PRECISION                               :: FPREV
    INTEGER                                        :: I, J
    INTEGER                                        :: IFCNT
    DOUBLE PRECISION                               :: NEWTLEN
    INTEGER                                        :: NEWTTAKEN
    INTEGER                                        :: NNEWT
    DOUBLE PRECISION                               :: PHID1
    DOUBLE PRECISION                               :: SDLEN
    !!!!DOUBLE PRECISION                           :: XPREV(N)
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: XPREV
    DOUBLE PRECISION                               :: TEMP
    !------------------------------------------------------------------
    !
    ALLOCATE(XPREV(N))
    !
    ! SET RETCODE = 4 AND FIRSTHOOK FOR INITIAL CALL
    RETCODE = 4
    FIRSTHOOK = .TRUE.
    ! CALCULATE THE LENGTH OF THE NEWTON STEP
    NEWTLEN=REG_TR_L2NORM(N,1.D0,SCLE,SNEWT)
    !
    IF (ITERP .EQ. 1 .OR. R .LT. -0.D0) THEN
      MU = 0.D0
      ! INITIALIZE TRUST REGION. CALCULATE THE NUMERATOR (G^T*G) AND
      ! DENOMINATOR (G^T*H*G) FOR THE LENGTH OF THE STEEPEST DESCENT STEP.
      A=REG_TR_L2NORM(N,-1.D0,SCLE,G)
      A = A*A
      B = 0.D0
      DO I = 1,N
        TEMP=0.D0
        DO J = I,N
          TEMP = TEMP + L(J,I)*G(J)/SCLE(J)/SCLE(J)
        ENDDO
        B=B+TEMP*TEMP
      ENDDO
      SDLEN = A*SQRT(A)/B
      ! SET TRUST RADIUS TO THE STEEPEST DESCENT LENGTH (BOUND BY MAXSTEP)
      R = MIN(SDLEN,MAXSTEP)
    ENDIF
    ! FIND THE STEP WITHIN THE TRUST REGION
    DO WHILE (RETCODE .GE. 2)
      MUC=MU
      CALL HOOKSTEP(N,G,IOUT,MACHEPS,NEWTLEN,SCLE,SNEWT,RPREV,&
                    FIRSTHOOK,H,L,MU,NNEWT,PHI,PHID,PHID1,R, &
                    NEWTTAKEN,S)
      RPREV = R
      !
      !  WE NOW HAVE THE NEW STEP, S(I), CALCULATED FROM THE HOOKSTEP
      !  UPDATE THE TRUST REGION AS ACCORDINGLY.
!eppsvd changes to accomodate svd added npetmp svdloop
      CALL TRUSTUP(MPR,N,NDOBS,NEOBS,NEWTTAKEN,NOBS,NPE,NPETMP,SVDLOOP,NPS, &
                   FC,G,H,IOUT,IPTR,L,LN,MAXSTEP,NPTR,PARNAM, &
                   PTOLPAR,S,SCLE,STEPTYPE,XPTR,FPREV,MODVALPREV,R, &
                   XC,XNPE,XPREV,FPLUS,MAXTAKEN,RETCODE,XPLUS)

      IF(RETCODE .EQ. 0 .AND. FPLUS .NE. FPREV) MUC=MU
      IFCNT=IFCNT+1
    ENDDO
    DEALLOCATE(XPREV)
    RETURN
  END SUBROUTINE REG_TR_HOOKDRIVE
!
!=======================================================================
!=======================================================================
  SUBROUTINE HOOKSTEP(N,G,IOUT,MACHEPS,NEWTLEN,SCLE,SNEWT,RPREV, &
                      FIRSTHOOK,H,L,MU,NNEWT,PHI,PHID,PHID1,R, &
                      NEWTTAKEN,S)
    !  THIS SUBROUTINE CALCULATES THE HOOKSTEP ALONG THE HOOKSTEP CURVE
    !  BASED ON MU.
    !  ALGORITHM A6.4.2 DENNIS AND SCHNABEL
    !  ******************VARIABLE DICTIONARY****************************
    !  FIRSTHOOK = FIRST TIME CALCULATING HOOKSTEP FOR THIS ITERATION
    !  G(I) = THE GRADIENT AT XC(I)
    !  H(I,J) = HESSIAN (X^T*w*X) -- CMATRD IN CALL (NOT MODIFIED!)
    !  HI = UPPER BOUND FOR TRUST REGION TOLERANCE FOR MU ADJUSTMENT
    !  LO = UPPER BOUND FOR TRUST REGION TOLERANCE FOR MU ADJUSTMENT
    !  L(I,J) = LOWER TRIANGULAR CHOLESKY FACTORIZATION OF THE HESSIAN
    !  MU = HOOKSTEP BASED MARQUARDT PARAMETER
    !  MULOW = LOWER BOUND ON MU
    !  MUUP = UPPER BOUND ON MU
    !  N = NUMBER OF INCLUDED PARAMETERS (ACTIVE THIS ITERATION)
    !  NEWTLEN = THE LENGTH OF THE NEWTON STEP
    !  NEWTTAKEN = A FLAG INDICATING THE FULL NEWTON STEP WAS TAKEN
    !  NNEWT = THE NUMBER OF NEWTON STEPS TAKEN
    !  PHI = DIFFERENCE BETWEEN R AND STEP LENGTH BASED ON MU
    !  PHID = DERIVATIVE OF PHI W.R.T MU
    !  PHID1 = INITIAL VALUE OF PHID
    !  R = RADIUS OF THE TRUST REGION
    !  RPREV = PREVIOUS RADIUS OF THE TRUST REGION
    !  S(I) = THE ACTUAL STEP USED
    !  SCLE(X) = A SCALING VECTOR
    !  SNEWT(I) = THE NEWTON STEP
    !  STEPLEN = THE SCALED LENGTH OF THE CURRENT STEP
    !*******************************************************************
    !        SPECIFICATIONS:
    !------------------------------------------------------------------
    USE UTILITIES, ONLY: UTL_STOP
    !
    IMPLICIT NONE
    ! ARGUMENT-LIST VARIABLES
    INTEGER,                         INTENT(IN)    :: N
    DOUBLE PRECISION,                INTENT(IN)    :: G(N)
    INTEGER,                         INTENT(IN)    :: IOUT
    DOUBLE PRECISION,                INTENT(IN)    :: MACHEPS
    DOUBLE PRECISION,                INTENT(IN)    :: NEWTLEN
    DOUBLE PRECISION,                INTENT(IN)    :: RPREV
    DOUBLE PRECISION,                INTENT(IN)    :: SCLE(N)
    DOUBLE PRECISION,                INTENT(IN)    :: SNEWT(N)
    !
    LOGICAL,                         INTENT(INOUT) :: FIRSTHOOK
    DOUBLE PRECISION,                INTENT(INOUT) :: H(N,N)
    DOUBLE PRECISION,                INTENT(INOUT) :: L(N,N)
    DOUBLE PRECISION,                INTENT(INOUT) :: MU
    INTEGER,                         INTENT(INOUT) :: NNEWT
    DOUBLE PRECISION,                INTENT(INOUT) :: PHI
    DOUBLE PRECISION,                INTENT(INOUT) :: PHID
    DOUBLE PRECISION,                INTENT(INOUT) :: PHID1
    DOUBLE PRECISION,                INTENT(INOUT) :: R
    !
    INTEGER,                         INTENT(OUT)   :: NEWTTAKEN
    DOUBLE PRECISION,                INTENT(OUT)   :: S(N)
    ! LOCAL VARIABLES
    LOGICAL                                        :: DONE
    DOUBLE PRECISION                               :: HI
    DOUBLE PRECISION                               :: LO
    INTEGER                                        :: I, J, K, KMAX
    DOUBLE PRECISION                               :: MAXADD
    DOUBLE PRECISION                               :: MULOW
    DOUBLE PRECISION                               :: MUUP
    DOUBLE PRECISION                               :: STEPLEN
    !!!!DOUBLE PRECISION                           :: TEMP(N)
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: TEMP
    !!!!DOUBLE PRECISION                           :: TEMPH(N,N)
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)  :: TEMPH
    !------------------------------------------------------------------
    100 FORMAT (/,' ERROR: HOOKSTEP UNABLE TO FIND ADEQUATE VALUE OF', &
                  ' MU IN ',I4, ' ATTEMPTS.',/, &
                  ' THIS IS OFTEN AN INDICATOR OF ILL-CONDITIONING.', &
                  ' CHECK CORRELATIONS.'/,'-- STOP EXECUTION (REG_TR_)')
    !
    ALLOCATE(TEMP(N),TEMPH(N,N))
    !
    HI = 1.5D0
    LO = 0.75D0
    KMAX = 100
    K = 0
    !
    ! IF THE NEWTON STEP FALLS W/IN HI*THE TRUST REGION, USE IT,
    ! AND UPDATE TRUST RADIUS BASED ON THE LENGTH OF THE NEWTON STEP
    IF (NEWTLEN .LE. HI*R) THEN
      NEWTTAKEN = 1
      DO I = 1,N
        S(I)=SNEWT(I)
      ENDDO
      NNEWT = NNEWT + 1
      MU = 0.D0
      R = MIN(R,NEWTLEN)
    ELSE
      ! NEWTON STEP WAS TOO LARGE.  TRY HOOKSTEP.  FIND MU SUCH THAT
      ! LENGTH OF HOOKSTEP FALLS W/IN HI-LO OF TRUST RADIUS.
      NEWTTAKEN = 0
      ! INITIALIZE MU IF PREVIOUS STEP WASN'T A NEWTON STEP
      IF (MU .GT. 0) MU = MU - ((PHI + RPREV)/R) * &
                               (((RPREV - R) + PHI)/PHID)
      !
      ! FIND UPPER AND LOWER BOUNDS ON MU. LOWER BOUND BASED ON
      ! A NEWTON ITERATION, WHICH UNDERSHOOTS.  UPPER BOUND BASED ON
      ! LENGTH OF GRADIENT VECTOR (STEEPEST DESCENT DIRECTION).
      ! IF FIRST TIME THROUGH, INITIALIZE PHID1
      ! PHID1 IS A SCALED VERSION OF Sn^T*(H)^-1*Sn /NEWTLEN
      PHI = NEWTLEN - R
      IF (FIRSTHOOK) THEN
        FIRSTHOOK = .FALSE.
        DO I = 1,N
          TEMP(I)=SCLE(I)*SCLE(I)*SNEWT(I)
        ENDDO
        CALL LSOLVE(N,TEMP,L,TEMP)
        ! TEMP NOW HAS L^-1*Sn AND SQUARING IT GIVES Sn^T*(H)^-1*Sn
        PHID1 = REG_TR_L2NORM(N,1.D0,TEMP,TEMP,1)
        PHID1 = -PHID1*PHID1/NEWTLEN
      ENDIF
      MULOW = -PHI/PHID1 ! LIKE A NEWTON STEP W/ MU = 0
      MUUP = REG_TR_L2NORM(N,-1.D0,SCLE,G)/R
      !
      ! BEGIN LOOP FOR CALCULATING MU ACCORDING TO HOOKSTEP
      ! FACTOR HESSIAN W/ MU ADDITION AND CALCULATE STEP LENGTH
      DONE = .FALSE.
      DO WHILE (.NOT. DONE)
        ! IF MU IS OUT OF BOUNDS, SET MU TO GEOMETRIC AVG OF MLOW AND MUUP
        IF (MU .LT. MULOW .OR. MU .GT. MUUP) &
            MU = MAX(SQRT(MULOW*MUUP),1.D-3*MUUP)
        ! ADD CONTRIBUTION OF MU TO HESSIAN DIAGONAL
        ! NOTE: ADDED SCALING TO HESSIAN RATHER THAN MU.  THIS IMPROVES
        ! STABILITY IN THE CHOLESKY FACTORIZATION (NOW L IS SCALED TOO)
        DO I =1,N
          DO J=I,N
            H(I,J) = H(I,J)/SCLE(I)/SCLE(J)
          ENDDO
          H(I,I) = H(I,I) + MU
        ENDDO
        CALL CHOLDECOMP(MACHEPS,N,H,L,MAXADD,TEMPH)  ! MAXADD IS A DUMMY
        ! RESTORE AND UNSCALE H.   UNSCALE L
        DO I =1,N
          H(I,I) = H(I,I) - MU
          DO J=I,N
            H(I,J) = H(I,J)*SCLE(I)*SCLE(J)
          ENDDO
          DO J=1,I
            L(I,J) = L(I,J)*SCLE(I)
          ENDDO
        ENDDO
        CALL REG_TR_CHOLSOLVE(N,G,L,S)
        ! CALCULATE STEP LENGTH AND UPDATE PHI AND PHID
        ! PHID IS A SCALED VERSION OF S^T*(H+MU*I)^-1*S /STEPLEN
        STEPLEN = REG_TR_L2NORM(N,1.D0,SCLE,S)
        PHI = STEPLEN - R
        DO I = 1,N
          TEMP(I)=SCLE(I)*SCLE(I)*S(I)
        ENDDO
        CALL LSOLVE(N,TEMP,L,TEMP)
        ! TEMP NOW HAS L^-1*S AND SQUARING IT GIVES S^T*(H+MU*I)^-1*S
        PHID = REG_TR_L2NORM(N,1.D0,TEMP,TEMP,1)
        PHID = -PHID*PHID/STEPLEN
        !
        ! CHECK IF STEP LENGTH FALLS W/IN UPPER,LOWER BOUNDS OR IF
        ! BOUNDS ESTIMATES OF MU ARE CONVERGED.
        IF ((STEPLEN .GE. LO*R .AND. STEPLEN .LE. HI*R) .OR. &
            (MUUP - MULOW) .LE. 0.D0) THEN
          ! STEP LENGTH IS ACCEPTABLE
          DONE = .TRUE.
        ELSE
          ! STEP LENGTH IS NOT ACCEPTABLE.  UPDATE MULOW, MUUP, AND MU
          ! USE A NEWTON APPX. TO UPDATE MULOW
          ! IF STEP LENGTH IS TOO SHORT, SET MUUP TO CURRENT MU
          MULOW = MAX(MULOW, MU-(PHI/PHID))
          IF (PHI .LT. 0.D0) MUUP = MU
          MU = MU - (STEPLEN/R) * (PHI/PHID)
        ENDIF
        ! STOP INFINITE LOOP IF HOOKSTEP IS FAILING FOR SOME REASON
        K=K+1
        IF(K .GT. KMAX) THEN
          WRITE(IOUT,100) KMAX
          CALL UTL_STOP(' ')
        ENDIF
        !
      ENDDO
    ENDIF
    !
    DEALLOCATE(TEMP,TEMPH)
    RETURN
  END SUBROUTINE HOOKSTEP
!
!=======================================================================
!=======================================================================
!eppsvd changes to accomodate svd added npetmp svdloop
  SUBROUTINE TRUSTUP(MPR,N,NDOBS,NEOBS,NEWTTAKEN,NOBS,NPE,NPETMP,SVDLOOP,NPS, &
                     FC,G,H,IOUT,IPTR,L,LN,MAXSTEP,NPTR,PARNAM, &
                     PTOLPAR,S,SCLE,STEPTYPE,XPTR,FPREV,MODVALPREV,R, &
                     XC,XNPE,XPREV,FPLUS,MAXTAKEN,RETCODE,XPLUS)
    !  THIS ALGRORITHM UPDATES THE TRUST REGION.  IF THE NEW PREDICTION
    !  OF F AGREES WITHIN 10% OF F, THEN DOUBLE TRUST REGION (R) AND
    !  REDO STEP.  IF THEY ONLY AGREE TO WITHIN 90% OF EACH OTHER,
    !  HALVE R AND START A NEW ITERATION. IF THEY AGREE TO WITHIN 25%,
    !  DOUBLE R AND START A NEW ITERATION. IF ALPHA CONDITION IS NOT
    !  SATISFIED, HALVE R AND REDO STEP.
    !  ALGORITHM A6.4.5 DENNIS AND SCHNABEL
    !******************VARIABLE DICTIONARY******************************
    !  ALPHA = THE ALPHA CONDITION (% OF ORIGINAL SLOPE)
    !  DELF = DIFFERENCE BETWEEN THE NEW FUNCTION AND THE OLD FUNCTION
    !  DELFPRED = THE PREDICTED (QUADRATIC MODEL) CHANGE IN THE FUNCTION
    !  FC = THE VALUE OF THE FUNCTION AT THE CURRENT POINT, XC(I)
    !  FPLUS = THE VALUE OF THE FUNCTION AT THE NEW POINT, XPLUS(I)
    !  FPREV = THE VALUE OF THE FUNCTION AT THE PREVIOUS POINT, XPREV(I)
    !  FUNC = 1/2* SUM OF SQUARES OBJECTIVE FUNCTION  EXTERNAL
    !  G(I) = THE GRADIENT AT XC(I)
    !  H(I,J) = THE HESSIAN (CMAT)
    !  L(I,J) = LOWER TRIANGULAR CHOLESKY FACTORIZATION OF THE HESSIAN
    !  LN = A FLAG INDICATING IF PARAMETER IS LOG TRANSFORMED
    !  MAXSTEP = THE MAXIMUM STEP ALLOWED AT ANY ITERATION
    !  MAXTAKEN = A FLAG INDICATING THE MAXIMUM STEP SIZE WAS TAKEN
    !  MODVALPREV = PREVIOUS RESULTS OF MODEL OBSERVATIONS
    !  N = NUMBER OF INCLUDED PARAMETERS (ACTIVE THIS ITERATION)
    !  NEWTTAKEN = A FLAG INDICATING THE FULL NEWTON STEP WAS TAKEN
    !  NPE = # OF POTENTIALLY ADJUSTABLE PARAMETERS (ACTIVE + OMITTED)
    !  NPS = # OF TOTAL DEFINED PARAMETERS (ADJUSTABLE + NON ADJUSTABLE)
    !  PARNAM = VECTOR CONTAINING NAMES OF ALL DEFINED THE PARAMETERS
    !  PTOLPAR = IF SUCCESSIVE STEPS ARE LESS THAN PTOLPAR ->STOP
    !  R = RADIUS OF THE TRUST REGION
    !  RELLEN = THE RELATIVE LENGTH OF THE STEP VS X (% CHANGE IN X)
    !  RETCODE = TERMINATION CODE OF A TRUST REGION/LINESEARCH UPDATE
    !  S(I) = THE ACTUAL STEP USED
    !  SCLE(X) = A SCALING VECTOR
    !  STEPLEN = THE SCALED LENGTH OF THE CURRENT STEP
    !  STEPTYPE = DOGLEG OR HOOKSTEP
    !  TPFLAG = FLAG INDICATING PARAMETER CHANGE IS LESS THAN PTOLPAR
    !  XC(I) = THE CURRENT SOLUTION
    !  XPLUS(I) = THE NEW SOLUTION
    !  XPREV(I) = THE PREVIOUS SOLUTION
    !  IOUT = UNIT NUMBER OF THE MAIN OUTPUT FILE
    !  IPTR = A POINTER MAPPING FROM VECTOR(NPE) TO VECTOR(NPS)
    !  NPTR = A POINTER MAPPING FROM VECTOR(N) TO VECTOR(NPS)
    !  XPTR = A POINTER MAPPING FROM VECTOR(N) TO VECTOR(NPE)
    !***********************************************************************
    !        SPECIFICATIONS:
    !------------------------------------------------------------------
    USE UTILITIES, ONLY: UTL_STOP
    !
    USE UCODEMOD, ONLY: &
    ! VARIABLES
    MODELVAL
    !
    IMPLICIT NONE
    ! ARGUMENT-LIST VARIABLES
    INTEGER,                         INTENT(IN)    :: MPR
    INTEGER,                         INTENT(IN)    :: N
    INTEGER,                         INTENT(IN)    :: NDOBS
    INTEGER,                         INTENT(IN)    :: NEOBS
    INTEGER,                         INTENT(IN)    :: NEWTTAKEN
    INTEGER,                         INTENT(IN)    :: NOBS
    INTEGER,                         INTENT(IN)    :: NPE
!eppsvd changes to accomodate svd added npetmp svdloop
    INTEGER,                         INTENT(IN)    :: NPETMP
    INTEGER,                         INTENT(IN)    :: SVDLOOP
    INTEGER,                         INTENT(IN)    :: NPS
    DOUBLE PRECISION,                INTENT(IN)    :: FC
    DOUBLE PRECISION,                INTENT(IN)    :: G(N)
    DOUBLE PRECISION,                INTENT(IN)    :: H(N,N)
    INTEGER,                         INTENT(IN)    :: IOUT
    INTEGER,                         INTENT(IN)    :: IPTR(NPE)
    DOUBLE PRECISION,                INTENT(IN)    :: L(N,N)
    INTEGER,                         INTENT(IN)    :: LN(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: MAXSTEP
    INTEGER,                         INTENT(IN)    :: NPTR(N)
    CHARACTER(LEN=12),               INTENT(IN)    :: PARNAM(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: PTOLPAR(NPS)
    DOUBLE PRECISION,                INTENT(IN)    :: S(N)
    DOUBLE PRECISION,                INTENT(IN)    :: SCLE(N)
    CHARACTER(LEN=10),               INTENT(IN)    :: STEPTYPE
    INTEGER,                         INTENT(IN)    :: XPTR(N)
    !
    DOUBLE PRECISION,                INTENT(INOUT) :: FPREV
    DOUBLE PRECISION,                INTENT(INOUT) :: MODVALPREV(NOBS)
    DOUBLE PRECISION,                INTENT(INOUT) :: R
    DOUBLE PRECISION,                INTENT(INOUT) :: XC(N)
    DOUBLE PRECISION,                INTENT(INOUT) :: XNPE(NPE)
    DOUBLE PRECISION,                INTENT(INOUT) :: XPREV(N)
    !
    DOUBLE PRECISION,                INTENT(OUT)   :: FPLUS
    INTEGER,                         INTENT(OUT)   :: MAXTAKEN
    INTEGER,                         INTENT(OUT)   :: RETCODE
    DOUBLE PRECISION,                INTENT(OUT)   :: XPLUS(N)
    ! LOCAL VARIABLES
    DOUBLE PRECISION                               :: ALPHA
    DOUBLE PRECISION                               :: DELF
    DOUBLE PRECISION                               :: DELFPRED
    DOUBLE PRECISION                               :: INITSLOPE
    INTEGER                                        :: I
    INTEGER                                        :: IIPP
    INTEGER                                        :: J
    DOUBLE PRECISION                               :: RTEMP
    DOUBLE PRECISION                               :: STEPLEN
    DOUBLE PRECISION                               :: STEPSIZE
    DOUBLE PRECISION                               :: TEMP
    INTEGER                                        :: TPFLAG
    !------------------------------------------------------------------
    550 FORMAT (/,' ERROR: CALCULATED PARAMETER CHANGE', &
        ' FOR LOG-TRANSFORMED PARAMETER "',A,'" IS TOO LARGE FOR ',/, &
        ' CALCULATION OF EXPONENTIAL -- STOP EXECUTION (REG_TR_)')
    !
    !NOTE: SKIP THIS IF WE ARE RETURNING FROM A FCN EVAL
    MAXTAKEN = 0
    ALPHA = 1.D-4
    STEPLEN=REG_TR_L2NORM(N,1.D0,SCLE,S)

    ! UPDATE THE SOLUTION VECTOR BEFORE FINDING THE NEW VALUE OF THE
    ! FUNCTION. THE FUNCTION CALL NEEDS THE VECTOR OF ALL ADJUSTALBE
    ! PARAMETERS --> XNPE
    DO I =1,N
      XPLUS(I)=XC(I) + S(I)
      XNPE(XPTR(I))=XPLUS(I)
    ENDDO
    !-------CONVERT XNPE TO NATIVE VALUES FOR FUNCTION EVALUATION
    DO I =1,NPE
      IIPP = IPTR(I)
      IF (LN(IIPP).GT.0)THEN
        IF(XNPE(I) .LE. 709.)THEN
          XNPE(I) = EXP(XNPE(I))
        ELSE
          WRITE(IOUT,550) PARNAM(IIPP)
          CALL UTL_STOP(' ')
        ENDIF
      ENDIF
    ENDDO
    !
!eppsvd changes to accomodate svd added npetmp svdloop
    CALL GETOBJFCN(MPR,NDOBS,NEOBS,NOBS,NPE,NPETMP,SVDLOOP,NPS,XNPE,FPLUS)
    !END NOTE:
    !-------CONVERT XNPE BACK TO TRANSFORMED VALUES. THIS IS NEEDED
    !-------WHEN RETCODE = 2 OR 3 TO PREVENT OMITTED PARAMETERS FROM
    !-------GETTING EXPONENTIATED MULTIPLE TIMES.
    DO I =1,NPE
      IIPP = IPTR(I)
      IF (LN(IIPP).GT.0) XNPE(I) = LOG(XNPE(I))
    ENDDO
    !---NOTE: XPLUS IS IN TRANSFORMED SPACE FOR TRUST REGION UPDATING
    !
    ! CALCULATE REDUCTION IN OBJECTIVE FUNCTION AND THE REDUCTION
    ! BASED ON THE INITIAL SLOPE AT THE OLD SOLUTION.
    DELF = FPLUS - FC
    INITSLOPE = 0.D0
    DO I = 1,N
      INITSLOPE = INITSLOPE + G(I)*S(I)
    ENDDO
    !
    IF(RETCODE .NE. 3)FPREV = 0.D0
    IF(RETCODE .EQ. 3 .AND. (FPLUS .GE. FPREV &
                             .OR. DELF .GT. ALPHA*INITSLOPE))THEN
      ! RESET XPLUS TO XPREV AND TERMINATE GLOBAL STEP.
      ! HALVE TRUST REGION
      RETCODE = 0
      DO I = 1,N
        XPLUS(I) = XPREV(I)
      ENDDO
      ! SWM: NOTE: MIGHT NEED TO RESET THE RESIDUAL VECTOR HERE
      MODELVAL = MODVALPREV
      FPLUS = FPREV
      R = 0.5D0*R
    ELSEIF(DELF .GE. ALPHA*INITSLOPE)THEN
      ! F(XPLUS) IS TOO LARGE.  DID NOT MEET ALPHA CONDITION
      ! CHECK IF STEPSIZE IS SMALLER THAN PTOLPAR (TRANSFORM IF NEEDED)
      TPFLAG = 1
      DO I=1,N
        IIPP = NPTR(I)
        IF (LN(IIPP).GT.0)THEN
          STEPSIZE = ABS(EXP(XPLUS(I)-XC(I)) - 1.D0)
        ELSE
          STEPSIZE = ABS((XPLUS(I)-XC(I))/XC(I))
        ENDIF
        IF(STEPSIZE .GT. PTOLPAR(NPTR(I)))THEN
          TPFLAG = 0
          EXIT
        ENDIF
      ENDDO
      IF(TPFLAG .EQ. 1)THEN
        ! THE % CHANGE IN X IS TOO SMALL, TERMINATE GLOBAL STEP
        RETCODE = 1
        DO I =1,N
          XPLUS(I)=XC(I)
        ENDDO
        ! SWM:  MAY NEED TO STORE THE RESIDUALS HERE
        MODELVAL = MODVALPREV
        FPLUS = FC
      ELSE
        ! REDUCE THE TRUST REGION AND TRY AGAIN.  FIND THE STEP SUCH
        ! THAT IT IS THE MINIMIZER OF THE QUADRATIC INTERPOLATION.
        ! TRY THIS STEP AS THE NEW TRUST REGION.  HOWEVER, BOUND THE
        ! TRUST REGION TO BE WITHIN 0.1 AND 0.5 OF THE ORIGINAL TRUST
        ! REGION.
        RETCODE = 2
        RTEMP = -0.5D0*INITSLOPE*STEPLEN/(DELF-INITSLOPE)
        IF(RTEMP .LT. 0.1D0*R)THEN
          R = 0.1D0*R
        ELSEIF(RTEMP .GT. 0.5D0*R)THEN
          R = 0.5D0*R
        ELSE
          R = RTEMP
        ENDIF
      ENDIF
    !
    ELSE
    ! F(XPLUS) IS SUFFICIENTLY SMALL!!  CHECK HOW CLOSE OUR QUADRATIC
    ! MODEL FITS THE ACTUAL FUNCTION AND ADJUST OUR TRUST REGION.  THE
    ! HESSIAN APPRX WILL BE DIFFERENT DEPENDING ON IF WE USED:
    ! HOOKSTEP OR A DOGLEG
      DELFPRED=INITSLOPE
      IF(STEPTYPE .EQ. 'HOOKSTEP')THEN
        !  FOR HOOKSTEP CALCULATE 1/2*S^T*H*S
        DO I = 1,N
          TEMP = 0.5D0*H(I,I)*S(I)*S(I)
          DO J = I+1,N
            TEMP=TEMP + H(I,J)*S(I)*S(J)
          ENDDO
          DELFPRED=DELFPRED + TEMP
        ENDDO
      ELSEIF(STEPTYPE .EQ. 'DOGLEG')THEN
        !  FOR DOGLEG, CALCULATE 1/2*S^T*L*L^T*S
        DO I = 1,N
          TEMP = 0.D0
          DO J = I,N
            TEMP=TEMP + L(J,I)*S(J)
          ENDDO
          DELFPRED=DELFPRED+0.5D0*TEMP*TEMP
        ENDDO
      ENDIF
      IF(RETCODE .NE. 2 .AND. (ABS(DELFPRED-DELF) .LE. 0.1D0*ABS(DELF) &
           .OR. DELF .LE. INITSLOPE) .AND. NEWTTAKEN .EQ. 0 &
           .AND. R .LE. 0.99D0*MAXSTEP)THEN
        ! DOUBLE THE TRUST REGION AND CONTINUE GLOBAL STEP
        RETCODE = 3
        DO I = 1,N
          XPREV(I)=XPLUS(I)
        ENDDO
        FPREV=FPLUS
        ! SWM:  MAY NEED TO STORE THE RESIDUALS HERE
        MODVALPREV = MODELVAL
        !
        ! NOTE: CAN FIT A QUADRATIC MODEL HERE AND TRY AND INCREASE THE
        !       THE TRUST REGION ACCORDING TO THAT INSTEAD OF JUST DOUBLING
        !       BUT KEEPING IT LESS THE QUADRUPLING (IE, BETWEEN 2-4)
        ! SWM          RTEMP = -0.5D0*INITSLOPE*STEPLEN/(DELF-INITSLOPE)
        !          IF(RTEMP .GT. 4.0D0*R)THEN
        !            R=R*4.D0
        !          ELSE
        !            R=MIN(MAX(RTEMP,2.0D0*R),MAXSTEP)
        ! SWM          ENDIF
        R=MIN(2.D0*R,MAXSTEP)
      ELSE
        !  ACCEPT XPLUS AS NEW SOLUTION AND UPDATE TRUST REGION
        RETCODE = 0
        MODVALPREV = MODELVAL
        IF(STEPLEN .GT. 0.99D0*MAXSTEP) MAXTAKEN = 1
        IF(DELF .GE. 0.1D0*DELFPRED)THEN
          ! DECREASE TRUST REGION FOR NEXT ITERATION
          R = 0.5D0*R
        ELSEIF(DELF .LE. 0.75D0*DELFPRED)THEN
          ! INCREASE TRUST REGION FOR NEXT ITERATION
          R = MIN(2.D0*R,MAXSTEP)
        ENDIF
      ENDIF
    ENDIF
    !
    !---CONVERT BACK TO NATIVE VALUES FOR RETURN FROM GLOBAL STEP
    IF(RETCODE .LT. 2)THEN
      DO I =1,N
        IIPP = NPTR(I)
        IF (LN(IIPP).GT.0)THEN
          XC(I) = EXP(XC(I))
          XPLUS(I) = EXP(XPLUS(I))
        ENDIF
      ENDDO
    ENDIF
    RETURN
  END SUBROUTINE TRUSTUP
!
!=======================================================================
!=======================================================================
  SUBROUTINE REG_TR_STOP(CONSECMAX,F,ITER,MAXITER, &
                         MAXTAKEN,N,NPS,NPTR,PTOLPAR,XC,XPLUS, &
                         RETCODE,COUNTCONSEC,DMXA,IFO,NPMAXCHG)
    !  THIS SUBROUTINE CHECKS FOR CONVERGENCE AT EACH ITERATION.
    !  MODIFIED FROM ALGORITHM A7.2.1 FROM DENNIS AND SCHNABEL
    !  ******************VARIABLE DICTIONARY****************************
    !  CHECKTOLSOSC = THE RELATIVE DIFFERENCE BETWEEN F AND FOLD3
    !  CONSECMAX = CHECKS FOR CONSECUTIVE LARGE STEPS (UNBOUNDEDNESS)
    !  COUNTCONSEC = COUNTER FOR CONSECUTIVE MAX STEPS
    !  DMXA = THE MAXIMUM FRACTIONAL PARAMETER CHANGE
    !  F = THE NEW VALUE OF THE OBJECTIVE FUNCTION
    !  FOLD2 = THE VALUE OF THE OBJECTIVE FUNCTION 2 ITERATIONS AGO
    !  FOLD3 = THE VALUE OF THE OBJECTIVE FUNCTION 3 ITERATIONS AGO
    !  IFO = THE TERMINATION FLAG
    !  ITER = ITERATION COUNTER
    !  MAXITER = MAXIMUM # OF ITERATIONS ALLOWED
    !  MAXTAKEN = A FLAG INDICATING THE MAXIMUM STEP SIZE WAS TAKEN
    !  N = NUMBER OF INCLUDED PARAMETERS (ACTIVE THIS ITERATION)
    !  NPS = # OF TOTAL DEFINED PARAMETERS (ADJUSTABLE + NON ADJUSTABLE)
    !  NPMAXCHG = PARAMETER INDEX W/ MAXIMUM FRACTIONAL PARAMETER CHANGE
    !  NPTR = A POINTER MAPPING FROM VECTOR(N) TO VECTOR(NPS)
    !  PTOLPAR = IF SUCCESSIVE STEPS ARE LESS THAN PTOLPAR ->STOP
    !  RETCODE = TERMINATION CODE OF A TRUST REGION/LINESEARCH UPDATE
    !  STEPSIZE = THE RELATIVE SIZE OF THE PARAMETER STEP
    !  TPFLAG = FLAG INDICATING PARAMETER CHANGE IS LESS THAN PTOLPAR
    !  XC(I) = THE PREVIOUS SOLUTION VECTOR
    !  XPLUS(I) = THE NEW SOLUTION VECTOR
    !*******************************************************************
    !        SPECIFICATIONS:
    !------------------------------------------------------------------
    USE REG_GN_UCODE, ONLY: &
    ! VARIABLES
    TOLSOSC
    !
    IMPLICIT NONE
    ! ARGUMENT-LIST VARIABLES
    INTEGER,                         INTENT(IN)    :: CONSECMAX
    DOUBLE PRECISION,                INTENT(IN)    :: F
    INTEGER,                         INTENT(IN)    :: ITER
    INTEGER,                         INTENT(IN)    :: MAXITER
    INTEGER,                         INTENT(IN)    :: MAXTAKEN
    INTEGER,                         INTENT(IN)    :: N
    INTEGER,                         INTENT(IN)    :: NPS
    INTEGER,                         INTENT(IN)    :: NPTR(N)
    DOUBLE PRECISION,                INTENT(IN)    :: PTOLPAR(NPS)
    INTEGER,                         INTENT(IN)    :: RETCODE
    DOUBLE PRECISION,                INTENT(IN)    :: XC(N)
    DOUBLE PRECISION,                INTENT(IN)    :: XPLUS(N)
!
    INTEGER,                         INTENT(INOUT) :: COUNTCONSEC
!
    DOUBLE PRECISION,                INTENT(OUT)   :: DMXA(MAXITER+1)
    INTEGER,                         INTENT(OUT)   :: IFO
    INTEGER,                         INTENT(OUT)   :: NPMAXCHG(MAXITER+1)
    ! LOCAL VARIABLES
    DOUBLE PRECISION                               :: CHECKTOLSOSC
    DOUBLE PRECISION, SAVE                         :: FOLD2
    DOUBLE PRECISION, SAVE                         :: FOLD3
    INTEGER                                        :: I
    DOUBLE PRECISION                               :: STEPSIZE
    INTEGER                                        :: TPFLAG
    !------------------------------------------------------------------
    !
    !INITIALIZE
    IFO = 0
    NPMAXCHG(ITER) = NPTR(1)
    !
    IF(RETCODE .NE. 1)THEN
      TPFLAG = 1
      DMXA(ITER) = 0.D0
      ! FIND IF ANY STEPSIZE IS GREATER THAN TOLERANCE
      DO I=1,N
        STEPSIZE = ABS((XPLUS(I)-XC(I))/XC(I))
        IF(STEPSIZE .GT. PTOLPAR(NPTR(I)))THEN
          TPFLAG = 0
        ENDIF
        !  FIND LARGEST STEPSIZE AND STORE ITS LOCATION
        IF(STEPSIZE .GT. DMXA(ITER))THEN
          DMXA(ITER) = STEPSIZE
          NPMAXCHG(ITER) = NPTR(I)
        ENDIF
      ENDDO
      !
      ! CHECK STORED OBJECTIVE FUNCTION VALUES AGAINST TOLSOSC
      CHECKTOLSOSC =  HUGE(TOLSOSC) !SET TO PREVENT FALSE CONVERGENCE
      IF(TPFLAG .EQ. 0)THEN   !HAVEN'T CONVERGED YET
        IF(ITER .GE. 3) THEN
          CHECKTOLSOSC = ABS((FOLD3 - F)/FOLD3)
          FOLD3 = FOLD2
          FOLD2 = F
        ELSEIF(ITER .EQ. 2)THEN   ! INITIALIZE
          FOLD2 = F
        ELSE                      ! ITER = 1
          FOLD3 = F
        ENDIF
      ENDIF
    ENDIF
    !
    ! SET IFO DEPENDING ON CONVERGENCE CRITERIA
    IF(RETCODE .EQ. 1 .OR. TPFLAG .EQ. 1)THEN
      IFO=1
    ELSEIF(CHECKTOLSOSC .LT. TOLSOSC)THEN
      IFO=2
    ELSEIF(ITER .GE. MAXITER)THEN
      IFO=3
    ELSEIF(MAXTAKEN .EQ. 1)THEN
      COUNTCONSEC = COUNTCONSEC+1
      IF(COUNTCONSEC .EQ. CONSECMAX)IFO =4
    ELSE
      COUNTCONSEC = 0
    ENDIF
    !
    ! WRITE OUTPUT DEPENDING ON TERMINATION CODE
    IF(IFO .EQ. 1)THEN
      WRITE(*,*) 'FRACTIONAL DISTANCE BETWEEN LAST TWO STEPS LESS THAN'
      WRITE(*,*) 'TOLPAR; SOLUTION MAY BE A MINIMIZER, BUT'
      WRITE(*,*) 'IT IS ALSO POSSIBLE THAT THE ALGORITHM IS MAKING'
      WRITE(*,*) 'VERY SLOW PROGRESS OR TOLPAR IS TOO LARGE.'
      WRITE(*,*) 'FINAL FUNCTION VALUE',2.D0*F
    ELSEIF(IFO .EQ. 2)THEN
      WRITE(*,*) 'FRACTIONAL SOSR CHANGE BETWEEN LAST 3 STEPS '
      WRITE(*,*)  'IS LESS THAN TOLSOSC'
      WRITE(*,*) 'FINAL FUNCTION VALUE',2.D0*F
    ELSEIF(IFO .EQ. 3)THEN
      WRITE(*,*) 'ITERATION LIMIT EXCEEDED'
      WRITE(*,*) 'FINAL FUNCTION VALUE',2.D0*F
    ELSEIF(IFO .EQ. 4)THEN
      WRITE(*,*) CONSECMAX,' CONSECUTIVE STEPS OF LENGTH MAXSTEP WERE TAKEN;'
      WRITE(*,*) 'EITHER F(X) IS UNBOUNDED BELOW, FINITE ASYMPTOTE,'
      WRITE(*,*) 'OR MAXSTEP IS TOO SMALL.'
      WRITE(*,*) 'FINAL FUNCTION VALUE',2.D0*F
    ENDIF
    !
    RETURN
  END SUBROUTINE REG_TR_STOP
!=======================================================================
!=======================================================================
  DOUBLE PRECISION FUNCTION REG_TR_L2NORM(N,P,V,W,IFLAG)
    !  THIS FUNCTION DOES JUST WHAT IT SAYS...CALCULATES THE L2 NORM OF
    !  THE PRODUCT OF TWO VECTORS.  THE FIRST VECTOR WILL BE RAISED TO
    !  THE POWER GIVEN BY THE VARIABLE "P".  IF IFLAG IS PRESENT, ONLY
    !  CALCULATE L2NORM OF THE FIRST VECTOR.
    !*******************************************************************
    !        SPECIFICATIONS:
    !------------------------------------------------------------------
    IMPLICIT NONE
    ! ARGUMENT LIST VARIABLES
    INTEGER,                         INTENT(IN)    :: N
    DOUBLE PRECISION,                INTENT(IN)    :: P
    DOUBLE PRECISION,                INTENT(IN)    :: V(N)
    DOUBLE PRECISION,                INTENT(IN)    :: W(N)
    INTEGER, OPTIONAL,               INTENT(IN)    :: IFLAG
    ! LOCAL VARIABLES
    INTEGER                                        :: I
    DOUBLE PRECISION                               :: X
    !------------------------------------------------------------------
    !
    X=0.D0
    IF(PRESENT(IFLAG))THEN
      DO I = 1,N
        X = X + (V(I)**P)**2
      ENDDO
    ELSE
      DO I = 1,N
        X = X + (W(I)*V(I)**P)**2
      ENDDO
    ENDIF
    REG_TR_L2NORM = SQRT(X)
    RETURN
  END FUNCTION REG_TR_L2NORM
!
!=======================================================================
!=======================================================================
!eppsvd changes to accomodate svd added npetmp svdloop
  SUBROUTINE GETOBJFCN(MPR,NDOBS,NEOBS,NOBS,NPE,NPETMP,SVDLOOP,NPS,PVAL,FPLUS)
    !
    ! VERSION 20050615 SWM
    !******************************************************************
    !     EVALUATE THE OBJECTIVE FUNCTION
    !******************************************************************
    !        SPECIFICATIONS:
    !------------------------------------------------------------------
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    USE UTILITIES
    !
    USE BASIC, ONLY: &
    ! SUBROUTINES
    BAS_EXE_SELECT, BAS_EXE
    !
    USE MODEL_IO, ONLY: &
    ! SUBROUTINES
    MIO_ADA_WRITEFILES, MIO_EXT
    !
    USE UCODEMOD, ONLY: &
    ! SUBROUTINES
    UCODE_GEN_PVALALL, UCODE_GEN_SVDCALCPAR4TRUST, &
    ! VARIABLES
    MCVUSE, MODELVAL, NOPNT, NPD, NW, NPSNPD, NCATS, PARNAMALL, &
    PARNAMLC, PRECIS, RESIDS, WTDRESIDS, WTFULLSQR
    !
    USE DEPENDENTS, ONLY: &
    ! SUBROUTINES
    DEP_EXT_DER, DEP_UEV_RESIDUALS, &
    ! VARIABLES
    OBSEXTNAM, OBSDEROBSNUM, OBSEXTOBSNUM
    !
    USE PRIOR_INFORMATION, ONLY: &
    ! SUBROUTINES
    PRI_UEV_RESIDUALS, &
    ! VARIABLES
    WTDRESIDSPRI
    !
    IMPLICIT NONE
    ! ARGUMENT LIST VARIABLES
    INTEGER,                         INTENT(IN)    :: MPR
    INTEGER,                         INTENT(IN)    :: NDOBS ! NUMBER OF DERIVED OBSERVATIONS
    INTEGER,                         INTENT(IN)    :: NEOBS
    INTEGER,                         INTENT(IN)    :: NOBS
    INTEGER,                         INTENT(IN)    :: NPE
!eppsvd changes to accomodate svd added npetmp svdloop
    INTEGER,                         INTENT(IN)    :: NPETMP
    INTEGER,                         INTENT(IN)    :: SVDLOOP
    INTEGER,                         INTENT(IN)    :: NPS
    DOUBLE PRECISION,                INTENT(IN)    :: PVAL(NPE) !NEEDS ALL ESTIMATED PARAMETER VALUES
    DOUBLE PRECISION,                INTENT(OUT)   :: FPLUS  ! OBJ FCN
    ! LOCAL VARIABLES
    INTEGER :: I, IFAIL, ICOMMAND
    !!!!DOUBLE PRECISION                          :: PVALTMP(NPSNPD) !NEEDS ALL PARAMETER VALUES
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: PVALTMP !NEEDS ALL PARAMETER VALUES
    !!!!DOUBLE PRECISION                          :: EXTVAL(NEOBS)
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: EXTVAL
    !!!!DOUBLE PRECISION                          :: DEPDERVAL(NDOBS)
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: DEPDERVAL
!    DOUBLE PRECISION                             :: MODELVAL(NDOBS)
    CHARACTER(LEN=MAX_STRING_LEN)                 :: INSTRUCTION ! EXTRACTION DIRECTION
!eppsvd changes to accomodate svd
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: PVALTMPSVD
    !------------------------------------------------------------------
    !
    ALLOCATE(PVALTMP(NPSNPD),EXTVAL(NEOBS),DEPDERVAL(NDOBS))
    !
    FPLUS = 0.D0
    ! PUT PVAL INTO PVALTMP
    ! GENERATE PARAMETER VALUES
!eppsvd changes to accomodate svd
    IF(SVDLOOP==2) THEN
      ALLOCATE(PVALTMPSVD(NPETMP))
      CALL UCODE_GEN_SVDCALCPAR4TRUST (NPETMP,NPE,PVAL,PVALTMPSVD)
      CALL UCODE_GEN_PVALALL(IFAIL,0,NPETMP,PVALTMPSVD,PVALTMP)
      DEALLOCATE(PVALTMPSVD)
    ELSE
!eppsvd following line was present here before svd changes
      CALL UCODE_GEN_PVALALL(IFAIL,0,NPETMP,PVAL,PVALTMP)
    ENDIF
!eppsvd end changes to accomodate svd
    !
    ! SUBSTITUTE IN THE CURRENT PARAMETER VALUES
    CALL MIO_ADA_WRITEFILES(IFAIL,NPSNPD,PARNAMALL,NOPNT,NW,PRECIS, &
                            PVALTMP)
    IF (IFAIL .NE. 0) CALL UTL_STOP(' ')
    !
    ! RUN THE FORWARD MODEL
    CALL BAS_EXE_SELECT(0,'FORWARD',ICOMMAND)
    CALL BAS_EXE(ICOMMAND,-1,1,1)

    ! ********************************************************************
    ! ******************* EXTRACT VALUES FROM MODEL OUTPUT ***************
    ! ********************************************************************
    ! EXTRACT SIMULATED EQUIVALENTS TO OBSERVATIONS
    CALL MIO_EXT(IFAIL,0,NCATS,NEOBS,OBSEXTNAM,MCVUSE, &
                 EXTVAL,INSTRUCTION)
    IF (IFAIL .NE. 0) THEN
      ! WRITE(IOUT,200) TRIM(INSTRUCTION)
      CALL UTL_STOP(' ')
    ENDIF

    ! NEED TO GO FROM EXTVAL TO MODELVAL (HOW ARE NONDETECTS HANDELED)?
    CALL DEP_EXT_DER(0,NDOBS,NEOBS,NPD,EXTVAL,DEPDERVAL)
    DO I=1,NEOBS
      IF(OBSEXTOBSNUM(I) > 0) MODELVAL(OBSEXTOBSNUM(I)) = EXTVAL(I)
    ENDDO
    IF (NDOBS > 0) THEN
      DO I=1,NDOBS
        IF(OBSDEROBSNUM(I)>0) MODELVAL(OBSDEROBSNUM(I)) = DEPDERVAL(I)
      ENDDO
    ENDIF
    !
    ! CALCULATE WEIGHTED RESIDUALS FROM OBSERVATIONS AND PRIOR
    CALL DEP_UEV_RESIDUALS(NOBS,MODELVAL,WTFULLSQR,RESIDS,WTDRESIDS)
    IF(MPR>0) THEN
      CALL PRI_UEV_RESIDUALS(0,MPR,NPD+NDOBS,NPS,PARNAMLC,PVAL)
    ENDIF
    !
    ! SUM UP SQUARED WEIGHTED RESIDUALS TO FORMULATE OBJECTIVE FUNCTION
    DO I = 1,NOBS
      FPLUS =  FPLUS + WTDRESIDS(I)**2
    ENDDO
    DO I = 1,MPR
      FPLUS =  FPLUS + WTDRESIDSPRI(I)**2
    ENDDO
    FPLUS = 0.5D0 * FPLUS
    !
    DEALLOCATE(PVALTMP,EXTVAL,DEPDERVAL)
    RETURN
  END SUBROUTINE GETOBJFCN
!===============================================================================
END MODULE REG_TRUSTMOD
