MODULE LINEAR_UNCERTAINTY_MOD
  USE GLOBAL_DATA, ONLY: LENDNAM, MAX_STRING_LEN
  USE UTILITIES
  IMPLICIT NONE
  SAVE
  PRIVATE
  ! Public subprograms
  PUBLIC YCINT_TSTAT, YCINT_BSTAT, YCINT_FSTT, YCINT_PRTOTA, YCINT_PRTOTX, &
         YCINT_UPARPM, YCINT_UCOLLBL, STATS_OPEN
  !
  CONTAINS
!-----------------------------------------------------------------------------
SUBROUTINE STATS_OPEN(IFAIL,NAM,NOW,TASK,IU,IOUT)
  ! this routine gets an available unit number and opens a file
  IMPLICIT NONE
  !   Argument-list variables
  INTEGER,            INTENT(INOUT)          :: IFAIL
  CHARACTER(LEN=MAX_STRING_LEN), INTENT(IN)  :: NAM
  CHARACTER(LEN=7),   INTENT(IN)             :: NOW
  CHARACTER(LEN=5),   INTENT(IN)             :: TASK
  INTEGER,            INTENT(OUT)            :: IU
  INTEGER,OPTIONAL,   INTENT(IN)             :: IOUT
  ! local variables
  CHARACTER(LEN=MAX_STRING_LEN)              :: FFILE
  INTEGER                                    :: ISTAT
  ! Formats
  10 FORMAT (//,1X,78('!'),//,5X,A,//, &
     5X,' NOTE: DATA-EXCHANGE FILES FROM A SUCCESSFUL REGRESSION AND',/, &
     5X,'    A SUBSEQUENT EXECUTION WITH PREDICTION=yes, MUST BE PRESENT',/, &
     5X,'    IN THE FOLDER WHERE LINEAR_UNCERTAINTY IS LAUNCHED.',//, &
     5X,'    If that has been done, check that the root filename on',/ &
     5X,'    the LINEAR_UNCERTAINTY command line is correct.',//,1X,78('!'),//)
  ! Initialize
  IFAIL = 0
  IU = UTL_GETUNIT(10,99)
  OPEN(UNIT=IU,FILE=TRIM(NAM),STATUS=TRIM(NOW),ACTION=TRIM(TASK),IOSTAT=ISTAT)
  IF (ISTAT  .NE.  0) THEN
    FFILE = ' Cannot open "'//TRIM(NAM)//'"'
    WRITE(*,10)TRIM(FFILE)
    IF(PRESENT(IOUT))WRITE(IOUT,10)TRIM(FFILE)
    CALL UTL_STOP()
  ENDIF
  RETURN
END SUBROUTINE STATS_OPEN
!=======================================================================
SUBROUTINE YCINT_TSTAT(IDOF,TST)
  !--VERSION 1000 01DEC1997
  !  ******************************************************************
  !  DETERMINE THE VALUE OF THE T STATISTIC NEEDED TO CALCULATE LINEAR
  !  INDIVIDUAL CONFIDENCE INTERVALS FOR A TWO-SIDED SIGNIFICANCE LEVEL
  !  OF 0.05
  !  ******************************************************************
  !     SPECIFICATIONS:
  IMPLICIT NONE
  INTEGER,                      INTENT(IN)  :: IDOF
  DOUBLE PRECISION,             INTENT(OUT) :: TST
  INTEGER                                   :: I
  INTEGER                                   :: ITABLE(35)
  DOUBLE PRECISION                          :: TABLE(35)
  !  ------------------------------------------------------------------
  DATA (ITABLE(I),I=1,35)/1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, &
        13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, &
        28, 29, 30, 40, 60, 120, 240, 500/
  DATA (TABLE(I),I=1,35)/12.706, 4.303, 3.182, 2.776, 2.571, 2.447, &
        2.365, 2.306, 2.262, 2.228, 2.201, 2.179, 2.160, 2.145, &
        2.131, 2.120, 2.110, 2.101, 2.093, 2.086, 2.080, 2.074, &
        2.069, 2.064, 2.060, 2.056, 2.052, 2.048, 2.045, 2.042, &
        2.021, 2.000, 1.980, 1.970, 1.960/
  !  ------------------------------------------------------------------
  !
  IF (IDOF .LE. 30) THEN
    TST=TABLE(IDOF)
    RETURN
  ENDIF
  DO I=31,35
    IF(IDOF .LE. ITABLE(I)) THEN
      TST = TABLE(I-1)+(TABLE(I)-TABLE(I-1))* &
           (IDOF-ITABLE(I-1))/(ITABLE(I)-ITABLE(I-1))
      RETURN
    ENDIF
  ENDDO
  TST=TABLE(35)
  RETURN
END SUBROUTINE YCINT_TSTAT
!=======================================================================
SUBROUTINE YCINT_BSTAT(K,IDOF,TST,IOUT)
  !-----VERSION 1000 01DEC1997
  !     ******************************************************************
  !     DETERMINE THE VALUE OF THE F STATISTIC NEEDED TO CALCULATE
  !     BONFERRONI CONFIDENCE INTERVALS
  !      -- SUPERFICIALLY MODIFIED FROM UCODE VERSION - ERB, 8/24/1999
  !     ******************************************************************
  !        SPECIFICATIONS:
  IMPLICIT NONE
  ! argument variables
  INTEGER,                      INTENT(IN)  :: K
  INTEGER,                      INTENT(IN)  :: IDOF
  DOUBLE PRECISION,             INTENT(OUT) :: TST
  INTEGER,                      INTENT(IN)  :: IOUT
  ! local variables
  INTEGER                                   :: ITABLE1(12)
  INTEGER                                   :: ITABLE2(17)
  DOUBLE PRECISION                          :: T(17,12)
  DOUBLE PRECISION                          :: TST1
  DOUBLE PRECISION                          :: TST2
  INTEGER                                   :: I
  INTEGER                                   :: J
  !     ------------------------------------------------------------------
  !     IDOF IS INDICATOR FOR TABLE1
   DATA (ITABLE1(I),I=1,12)/5, 7, 10, 12, &
         15, 20, 24, 30, 40, 60, 120, 32000/
  !   K IS INDICATOR FOR TABLE2
   DATA (ITABLE2(I),I=1,17)/ 2, 3, 4, 5, 6, 7, 8, 9, 10, &
         15, 20, 25, 30, 35, 40, 45, 50/
  !   TABLE IS SET UP AS (K,IDOF)
  !
   DATA (T(1,I),I=1,12) &
                       /3.17, 2.84, 2.64, 2.56, 2.49, 2.42, &
                        2.39, 2.36, 2.33, 2.30, 2.27, 2.24/
  !
   DATA (T(2,I),I=1,12) &
                       /3.54, 3.13, 2.87, 2.78, 2.69, 2.61, &
                        2.58, 2.54, 2.50, 2.47, 2.43, 2.39/
  !
   DATA (T(3,I),I=1,12) &
                       /3.81, 3.34, 3.04, 2.94, 2.84, 2.75, &
                        2.70, 2.66, 2.62, 2.58, 2.54, 2.50/
  !
   DATA (T(4,I),I=1,12) &
                       /4.04, 3.50, 3.17, 3.06, 2.95, 2.85, &
                        2.80, 2.75, 2.71, 2.66, 2.62, 2.58/
  !
   DATA (T(5,I),I=1,12) &
                       /4.22, 3.64, 3.28, 3.15, 3.04, 2.93, &
                        2.88, 2.83, 2.78, 2.73, 2.68, 2.64/
  !
   DATA (T(6,I),I=1,12) &
                       /4.38, 3.76, 3.37, 3.24, 3.11, 3.00, &
                        2.94, 2.89, 2.84, 2.79, 2.74, 2.69/
  !
   DATA (T(7,I),I=1,12) &
                       /4.53, 3.86, 3.45, 3.31, 3.18, 3.06, &
                        3.00, 2.94, 2.89, 2.84, 2.79, 2.74/
  !
   DATA (T(8,I),I=1,12) &
                       /4.66, 3.95, 3.52, 3.37, 3.24, 3.11, &
                        3.05, 2.99, 2.93, 2.88, 2.83, 2.77/
  !
   DATA (T(9,I),I=1,12) &
                       /4.78, 4.03, 3.58, 3.43, 3.29, 3.16, &
                        3.09, 3.03, 2.97, 2.92, 2.86, 2.81/
  !
   DATA (T(10,I),I=1,12) &
                       /5.25, 4.36, 3.83, 3.65, 3.48, 3.33, &
                        3.26, 3.19, 3.12, 3.06, 2.99, 2.94/
  !
   DATA (T(11,I),I=1,12) &
                       /5.60, 4.59, 4.01, 3.80, 3.62, 3.46, &
                        3.38, 3.30, 3.23, 3.16, 3.09, 3.02/
  !
   DATA (T(12,I),I=1,12) &
                       /5.89, 4.78, 4.15, 3.93, 3.74, 3.55, &
                        3.47, 3.39, 3.31, 3.24, 3.16, 3.09/
  !
   DATA (T(13,I),I=1,12) &
                       /6.15, 4.95, 4.27, 4.04, 3.82, 3.63, &
                        3.54, 3.46, 3.38, 3.30, 3.22, 3.15/
  !
   DATA (T(14,I),I=1,12) &
                       /6.36, 5.09, 4.37, 4.13, 3.90, 3.70, &
                        3.61, 3.52, 3.43, 3.34, 3.27, 3.19/
  !
   DATA (T(15,I),I=1,12) &
                       /6.56, 5.21, 4.45, 4.20, 3.97, 3.76, &
                        3.66, 3.57, 3.48, 3.39, 3.31, 3.23/
  !
   DATA (T(16,I),I=1,12) &
                       /6.70, 5.31, 4.53, 4.26, 4.02, 3.80, &
                        3.70, 3.61, 3.51, 3.42, 3.34, 3.26/
  !
   DATA (T(17,I),I=1,12) &
                       /6.86, 5.40, 4.59, 4.32, 4.07, 3.85, &
                        3.74, 3.65, 3.55, 3.46, 3.37, 3.29/
  !
  !     ------------------------------------------------------------------
  3 FORMAT(/,'* The number of intervals and degrees of freedom',/, &
            '* are below the range of this table, & 2,5 is used',/)
  4 FORMAT(/,' ** The number of degrees of freedom (',I1,') is below  **',/, &
             ' ** the range considered in the table of this code (5)  **',/, &
             ' ** the critical value for 5 degrees of freedom is used **',/)
  999 FORMAT (/,'***** K is larger than statistics table maximum ***', &
             /,'      consequently the largest statistic is used', &
             /,'***** check the validity of this situation      ***',/)
  !
  IF (K <= 2 .AND. IDOF <= 5) THEN
    TST=T(1,1)
    IF (K < 2 .OR. IDOF < 5) THEN
      WRITE (IOUT,3)
    ENDIF
    RETURN
  ENDIF
  !
  IF (K <= 2 .AND. IDOF > 5) THEN
    DO I=2,12
      IF(IDOF .LE. ITABLE1(I)) THEN
        TST = ((T(1,I)-T(1,I-1))*(IDOF-ITABLE1(I-1)) &
              /(ITABLE1(I)-ITABLE1(I-1)))+T(1,I-1)
        RETURN
      ENDIF
    ENDDO
  ENDIF
  !
  IF (IDOF .LT. 5) THEN
    DO I=2,17
      IF(K .LE. ITABLE2(I)) THEN
        TST = ((T(I,1)-T(I-1,1))*(K-ITABLE2(I-1)) &
              /(ITABLE2(I)-ITABLE2(I-1)))+T(I-1,1)
        WRITE (IOUT,4) IDOF
        RETURN
      ENDIF
    ENDDO
  ENDIF
  !
  IF (K .LE. 50) THEN
    DO I=2,17
      IF(K .LE. ITABLE2(I)) THEN
        DO J=2,12
          IF(IDOF .LE. ITABLE1(J)) THEN
            TST1 = ((T(I,J)-T(I-1,J))*(K-ITABLE2(I-1)) &
                   /(ITABLE2(I)-ITABLE2(I-1)))+T(I-1,J)
            TST2 = ((T(I,J-1)-T(I-1,J-1))*(K-ITABLE2(I-1)) &
                   /(ITABLE2(I)-ITABLE2(I-1)))+T(I-1,J-1)
            TST = ((TST1-TST2)*(IDOF-ITABLE1(J-1)) &
                  /(ITABLE1(J)-ITABLE1(J-1)))+TST2
            RETURN
          ENDIF
        ENDDO
      ENDIF
    ENDDO
  ENDIF
  !
  !
  WRITE(IOUT,999)
  TST=T(17,1)
  !
  RETURN
END SUBROUTINE YCINT_BSTAT
!=======================================================================
SUBROUTINE YCINT_FSTT(NP,IDOF,TST)
  ! ******************************************************************
  ! DETERMINE THE VALUE OF THE F STATISTIC NEEDED TO CALCULATE
  ! SCHEFFE CONFIDENCE INTERVALS
  ! -- MODIFIED FROM UCODE VERSION -- ERB 9/23/99
  ! ******************************************************************
  !    SPECIFICATIONS:
  IMPLICIT NONE
  ! argument variables
  INTEGER,                      INTENT(IN)  :: NP
  INTEGER,                      INTENT(IN)  :: IDOF
  DOUBLE PRECISION,             INTENT(OUT) :: TST
  ! local variables
  INTEGER                                   :: ITABLE1(19)
  INTEGER                                   :: ITABLE2(34)
  DOUBLE PRECISION                          :: T(19,34)
  DOUBLE PRECISION                          :: TST1
  DOUBLE PRECISION                          :: TST2
  INTEGER                                   :: I
  INTEGER                                   :: J
  ! ------------------------------------------------------------------
  ! NP is indicator for table1
  DATA (ITABLE1(I),I=1,19)/1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, &
        15, 20, 24, 30, 40, 60, 120, 32000/
  ! IDOF is indicator for table2
  DATA (ITABLE2(I),I=1,34)/1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, &
        13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, &
        28, 29, 30, 40, 60, 120, 32000/
  ! TABLE IS SET UP AS (NP,IDOF)
  !
  DATA (T(1,I),I=1,34) &
       /161.4, 18.51, 10.13, 7.71, 6.61, 5.99, 5.59, &
        5.32, 5.12, 4.96, 4.84, 4.75, 4.67, 4.60, 4.54, 4.49, &
        4.45, 4.41, 4.38, 4.35, 4.32, 4.30, 4.28, 4.26, 4.24, &
        4.23, 4.21, 4.20, 4.18, 4.17, 4.08, 4.00, 3.92, 3.84/
  !
  DATA (T(2,I),I=1,34) &
       /199.5,19.00,9.55,6.94,5.79,5.14,4.74, &
        4.46, 4.26, 4.10, 3.98, 3.89, 3.81, 3.74, 3.68, 3.63, &
        3.59, 3.55, 3.52, 3.49, 3.47, 3.44, 3.42, 3.40, 3.39, &
        3.37, 3.35, 3.34, 3.33, 3.32, 3.23, 3.15, 3.07, 3.00/
  !
  DATA (T(3,I),I=1,34)/215.7,19.16,9.28,6.59,5.41,4.76,4.35, &
        4.07, 3.86, 3.71, 3.59, 3.49, 3.41, 3.34, 3.29, 3.24, &
        3.20, 3.16, 3.13, 3.10, 3.07, 3.05, 3.03, 3.01, 2.99, &
        2.98, 2.96, 2.95, 2.93, 2.92, 2.84, 2.76, 2.68, 2.60/
  !
  DATA (T(4,I),I=1,34)/224.6,19.25,9.12,6.39,5.19,4.53,4.12, &
        3.84, 3.63, 3.48, 3.36, 3.26, 3.18, 3.11, 3.06, 3.01, &
        2.96, 2.93, 2.90, 2.87, 2.84, 2.82, 2.80, 2.78, 2.76, &
        2.74, 2.73, 2.71, 2.70, 2.69, 2.61, 2.53, 2.45, 2.37/
  !
  DATA (T(5,I),I=1,34)/230.2,19.30,9.01,6.26,5.05,4.39,3.97, &
        3.69, 3.48, 3.33, 3.20, 3.11, 3.03, 2.96, 2.90, 2.85, &
        2.81, 2.77, 2.74, 2.71, 2.68, 2.66, 2.64, 2.62, 2.60, &
        2.59, 2.57, 2.56, 2.55, 2.53, 2.45, 2.37, 2.29, 2.21/
  !
  DATA (T(6,I),I=1,34)/234.0,19.33,8.94,6.16,4.95,4.28,3.87, &
        3.58, 3.37, 3.22, 3.09, 3.00, 2.92, 2.85, 2.79, 2.74, &
        2.70, 2.66, 2.63, 2.60, 2.57, 2.55, 2.53, 2.51, 2.49, &
        2.47, 2.46, 2.45, 2.43, 2.42, 2.34, 2.25, 2.17, 2.10/
  !
  DATA (T(7,I),I=1,34)/236.8,19.35,8.89,6.09,4.88,4.21,3.79, &
        3.50, 3.29, 3.14, 3.01, 2.91, 2.83, 2.76, 2.71, 2.66, &
        2.61, 2.58, 2.54, 2.51, 2.49, 2.46, 2.44, 2.42, 2.40, &
        2.39, 2.37, 2.36, 2.35, 2.33, 2.25, 2.17, 2.09, 2.01/
  !
  DATA (T(8,I),I=1,34)/238.9,19.37,8.85,6.04,4.82,4.15,3.73, &
        3.44, 3.23, 3.07, 2.95, 2.85, 2.77, 2.70, 2.64, 2.59, &
        2.55, 2.51, 2.48, 2.45, 2.42, 2.40, 2.37, 2.36, 2.34, &
        2.32, 2.31, 2.29, 2.28, 2.27, 2.18, 2.10, 2.02, 1.94/
  !
  DATA (T(9,I),I=1,34)/240.5,19.38,8.81,6.00,4.77,4.10,3.68, &
        3.39, 3.18, 3.02, 2.90, 2.80, 2.71, 2.65, 2.59, 2.54, &
        2.49, 2.46, 2.42, 2.39, 2.37, 2.34, 2.32, 2.30, 2.28, &
        2.27, 2.25, 2.24, 2.22, 2.21, 2.12, 2.04, 1.96, 1.88/
  !
  DATA (T(10,I),I=1,34)/241.9,19.40,8.79,5.96,4.74,4.06,3.64, &
        3.35, 3.14, 2.98, 2.85, 2.75, 2.67, 2.60, 2.54, 2.49, &
        2.45, 2.41, 2.38, 2.35, 2.32, 2.30, 2.27, 2.25, 2.24, &
        2.22, 2.20, 2.19, 2.18, 2.16, 2.08, 1.99, 1.91, 1.83/
  !
  DATA (T(11,I),I=1,34)/243.9,19.41,8.74,5.91,4.68,4.00,3.57, &
        3.28, 3.07, 2.91, 2.79, 2.69, 2.60, 2.53, 2.48, 2.42, &
        2.38, 2.34, 2.31, 2.28, 2.25, 2.23, 2.20, 2.18, 2.16, &
        2.15, 2.13, 2.12, 2.10, 2.09, 2.00, 1.92, 1.83, 1.75/
  !
  DATA (T(12,I),I=1,34)/245.9,19.43,8.70,5.86,4.62,3.94,3.51, &
        3.22, 3.01, 2.85, 2.72, 2.62, 2.53, 2.46, 2.40, 2.35, &
        2.31, 2.27, 2.23, 2.20, 2.18, 2.15, 2.13, 2.11, 2.09, &
        2.07, 2.06, 2.04, 2.03, 2.01, 1.92, 1.84, 1.75, 1.67/
  !
  DATA (T(13,I),I=1,34)/248.0,19.45,8.66,5.80,4.56,3.87,3.44, &
        3.15, 2.94, 2.77, 2.65, 2.54, 2.46, 2.39, 2.33, 2.28, &
        2.23, 2.19, 2.16, 2.12, 2.10, 2.07, 2.05, 2.03, 2.01, &
        1.99, 1.97, 1.96, 1.94, 1.93, 1.84, 1.75, 1.66, 1.57/
  !
  DATA (T(14,I),I=1,34)/249.1,19.45,8.64,5.77,4.53,3.84,3.41, &
        3.12, 2.90, 2.74, 2.61, 2.51, 2.42, 2.35, 2.29, 2.24, &
        2.19, 2.15, 2.11, 2.08, 2.05, 2.03, 2.01, 1.98, 1.96, &
        1.95, 1.93, 1.91, 1.90, 1.89, 1.79, 1.70, 1.61, 1.52/
  !
  DATA (T(15,I),I=1,34)/250.1,19.46,8.62,5.75,4.50,3.81,3.38, &
        3.08, 2.86, 2.70, 2.57, 2.47, 2.38, 2.31, 2.25, 2.19, &
        2.15, 2.11, 2.07, 2.04, 2.01, 1.98, 1.96, 1.94, 1.92, &
        1.90, 1.88, 1.87, 1.85, 1.84, 1.74, 1.65, 1.55, 1.46/
  !
  DATA (T(16,I),I=1,34)/251.1,19.47,8.59,5.72,4.46,3.77,3.34, &
        3.04, 2.83, 2.66, 2.53, 2.43, 2.34, 2.27, 2.20, 2.15, &
        2.10, 2.06, 2.03, 1.99, 1.96, 1.94, 1.91, 1.89, 1.87, &
        1.85, 1.84, 1.82, 1.81, 1.79, 1.69, 1.59, 1.50, 1.39 /
  !
  DATA (T(17,I),I=1,34)/252.2,19.48,8.57,5.69,4.43,3.74,3.30, &
        3.01, 2.79, 2.62, 2.49, 2.38, 2.30, 2.22, 2.16, 2.11, &
        2.06, 2.02, 1.98, 1.95, 1.92, 1.89, 1.86, 1.84, 1.82, &
        1.80, 1.79, 1.77, 1.75, 1.74, 1.64, 1.53, 1.43, 1.32/
  !
  DATA (T(18,I),I=1,34)/253.3,19.49,8.55,5.66,4.40,3.70,3.27, &
        2.97, 2.75, 2.58, 2.45, 2.34, 2.25, 2.18, 2.11, 2.06, &
        2.01, 1.97, 1.93, 1.90, 1.87, 1.84, 1.81, 1.79, 1.77, &
        1.75, 1.73, 1.71, 1.70, 1.68, 1.58, 1.47, 1.35, 1.22/
  !
  DATA (T(19,I),I=1,34)/254.3,19.50,8.53,5.63,4.36,3.67,3.23, &
        2.93, 2.71, 2.54, 2.40, 2.30, 2.21, 2.13, 2.07, 2.01, &
        1.96, 1.92, 1.88, 1.84, 1.81, 1.78, 1.76, 1.73, 1.71, &
        1.69, 1.67, 1.65, 1.64, 1.62, 1.51, 1.39, 1.25, 1.00/
  !
  ! ------------------------------------------------------------------
  !
  IF (NP .LE. 10.AND.IDOF .LE. 30) THEN
  !   ENTRIES ARE EXACT FOR FIRST AND SECOND SUBSCRIPTS
    TST=T(NP,IDOF)
    RETURN
  ENDIF
  !
  IF (NP .GT. 10.AND.IDOF .LE. 30) THEN
  !   INTERPOLATE BETWEEN ENTRIES FOR FIRST SUBSCRIPT;
  !   ENTRIES FOR SECOND SUBSCRIPT ARE EXACT
    DO I=11,19
      IF(NP .LE. ITABLE1(I)) THEN
        TST = ((T(I,IDOF)-T(I-1,IDOF))*(NP-ITABLE1(I-1)) &
              /(ITABLE1(I)-ITABLE1(I-1)))+T(I-1,IDOF)
        RETURN
      ENDIF
    ENDDO
       TST = T(19,IDOF)
       RETURN
  ENDIF
  !
  IF (NP .LE. 10.AND.IDOF .GT. 30) THEN
  !  INTERPOLATE BETWEEN ENTRIES FOR SECOND SUBSCRIPT;
  !  ENTRIES FOR FIRST SUBSCRIPT ARE EXACT
    DO J=31,34
      IF(IDOF .LE. ITABLE2(J)) THEN
        TST = ((T(NP,J)-T(NP,J-1))*(IDOF-ITABLE2(J-1)) &
              /(ITABLE2(J)-ITABLE2(J-1)))+T(NP,J-1)
        RETURN
      ENDIF
    ENDDO
    TST = T(NP,34)
    RETURN
  ENDIF
  !
  IF (NP .LE. ITABLE1(19)) THEN
    IF (IDOF .LE. ITABLE2(34)) THEN
  ! INTERPOLATE BETWEEN ENTRIES FOR FIRST AND SECOND SUBSCRIPTS
      DO I=11,19
        IF(NP .LE. ITABLE1(I)) THEN
          DO J=31,34
            IF(IDOF .LE. ITABLE2(J)) THEN
              TST1 = ((T(I,J)-T(I-1,J))*(NP-ITABLE1(I-1)) &
                     /(ITABLE1(I)-ITABLE1(I-1)))+T(I-1,J)
              TST2 = ((T(I,J-1)-T(I-1,J-1))*(NP-ITABLE1(I-1)) &
                     /(ITABLE1(I)-ITABLE1(I-1)))+T(I-1,J-1)
              TST = ((TST1-TST2)*(IDOF-ITABLE2(J-1)) &
                     /(ITABLE2(J)-ITABLE2(J-1)))+TST2
              RETURN
            ENDIF
          ENDDO
        ENDIF
      ENDDO
    ELSE
    ! NP IS WITHIN RANGE OF VALUES IN ITABLE1, BUT IDOF EXCEEDS
    ! LARGEST VALUE IN ITABLE2
      DO I=11,19
        IF (NP .LE. ITABLE1(I)) THEN
          TST = ((T(I,34)-T(I-1,34))*(NP-ITABLE1(I-1)) &
                /(ITABLE1(I)-ITABLE1(I-1)))+T(I-1,34)
          RETURN
        ENDIF
      ENDDO
    ENDIF
  ENDIF
  !
  TST=T(19,34)
  !
  RETURN
END SUBROUTINE YCINT_FSTT
!=======================================================================
SUBROUTINE YCINT_PRTOTA(NO,DID,VALB,IOUT)
  !**PRINT VALUES IN TWO GROUPS OF THREE COLUMNS
  IMPLICIT NONE
  ! argument variables
  INTEGER,                           INTENT(IN) :: NO
  CHARACTER(LEN=LENDNAM),            INTENT(IN) :: DID(NO)
  DOUBLE PRECISION,                  INTENT(IN) :: VALB(NO)
  INTEGER,                           INTENT(IN) :: IOUT
  ! local variables
  INTEGER                                       :: K
  INTEGER                                       :: L
  INTEGER                                       :: NR
  ! formats
  20 FORMAT (2X,2(I4,2X,A,1X,G11.5,2X))
  NR=NO/2
  IF(2*NR .NE. NO) NR=NR+1
  DO K=1,NR
    WRITE(IOUT,20) (L,DID(L),VALB(L),L=K,NO,NR)
  ENDDO
  RETURN
END SUBROUTINE YCINT_PRTOTA
!=======================================================================
SUBROUTINE YCINT_PRTOTX(NR,NC,NRD,OBSNAM,PARNAM,X,IOUT)
  !**PRINT MATRICES DIVIDED VERTICALLY INTO FIVE-COLUMN BLOCKS
  IMPLICIT NONE
  ! argument variables
  INTEGER,                           INTENT(IN) :: NR
  INTEGER,                           INTENT(IN) :: NC
  INTEGER,                           INTENT(IN) :: NRD
  CHARACTER(LEN=LENDNAM),            INTENT(IN) :: OBSNAM(NC)
  CHARACTER(LEN=12),                 INTENT(IN) :: PARNAM(NR)
  DOUBLE PRECISION,                  INTENT(IN) :: X(NRD,NC)
  INTEGER,                           INTENT(IN) :: IOUT
  ! local variables
  INTEGER                                       :: I
  INTEGER                                       :: J
  INTEGER                                       :: J10
  INTEGER                                       :: K
  ! formats
  500 FORMAT (/,1X,'PARAMETER',4X,3(A20,1X))
  505 FORMAT (1X,A12,1X,3(G20.5,1X))
  510 FORMAT (1X,10('-'),3X,3(A20,1X))
  DO K = 1, NC, 3
    J10 = K + 2
    IF (J10 .GT. NC) J10 = NC
    WRITE (IOUT,500) (OBSNAM(J),J=K,J10)
    WRITE (IOUT,510) ('--------------------',J=K,J10)
    DO I = 1, NR
      WRITE (IOUT,505) PARNAM(I), (X(I,J),J=K,J10)
    ENDDO
  ENDDO
  RETURN
END SUBROUTINE YCINT_PRTOTX
!=======================================================================
SUBROUTINE YCINT_UPARPM(NPE,NPD,IPRC,IOUT,BUF,PARNAM)
  !
  !-----VERSION 19980825 ERB
  !     ******************************************************************
  !     PRINT ONE NPE*NPE CORRELATION OR VARIANCE-COVARIANCE MATRIX
  !     ******************************************************************
  !
  !        SPECIFICATIONS:
  !     ------------------------------------------------------------------
  IMPLICIT NONE
  ! argument variables
  INTEGER,                           INTENT(IN) :: NPE
  INTEGER,                           INTENT(IN) :: NPD
  INTEGER,                           INTENT(IN) :: IPRC
  INTEGER,                           INTENT(IN) :: IOUT
  DOUBLE PRECISION,                  INTENT(IN) :: BUF(NPD,NPD)
  CHARACTER(LEN=12),                 INTENT(IN) :: PARNAM(NPE)
  ! local variables
  INTEGER                                       :: I
  INTEGER                                       :: IP
  INTEGER                                       :: J
  !  ------------------------------------------------------------------
  !
  !2---MAKE SURE THE FORMAT CODE (IPRC) IS VALID
  IP=IPRC
  IF(IP .LT. 1 .OR. IP .GT. 10) IP=1
  !
  !3---LABEL COLUMNS WITH PARAMETER NAMES.
  IF(IP .EQ. 1) CALL YCINT_UCOLLBL(NPE,1,NPE,0,11,11,IOUT,PARNAM)
  IF(IP .EQ. 2) CALL YCINT_UCOLLBL(NPE,1,NPE,0,10,12,IOUT,PARNAM)
  IF(IP .EQ. 3) CALL YCINT_UCOLLBL(NPE,1,NPE,0,9,13,IOUT,PARNAM)
  IF(IP .EQ. 4) CALL YCINT_UCOLLBL(NPE,1,NPE,0,8,14,IOUT,PARNAM)
  IF(IP .EQ. 5) CALL YCINT_UCOLLBL(NPE,1,NPE,0,8,15,IOUT,PARNAM)
  IF(IP .EQ. 6) CALL YCINT_UCOLLBL(NPE,1,NPE,0,6,11,IOUT,PARNAM)
  IF(IP .EQ. 7) CALL YCINT_UCOLLBL(NPE,1,NPE,0,5,12,IOUT,PARNAM)
  IF(IP .EQ. 8) CALL YCINT_UCOLLBL(NPE,1,NPE,0,5,13,IOUT,PARNAM)
  IF(IP .EQ. 9) CALL YCINT_UCOLLBL(NPE,1,NPE,0,4,14,IOUT,PARNAM)
  IF(IP .EQ. 10 ) CALL YCINT_UCOLLBL(NPE,1,NPE,0,4,15,IOUT,PARNAM)
  !
  !4------LOOP THROUGH THE ROWS PRINTING EACH ONE IN ITS ENTIRETY.
  DO I=1,NPE
  !
  !--------- FORMAT 11G10.3
    IF (IP .EQ. 1) THEN
      WRITE(IOUT,11) PARNAM(I),(BUF(J,I),J=1,NPE)
      11 FORMAT(1X,A,1X,1PG10.3,10(1X,G10.3):/(11X,11(1X,G10.3)))
  !
  !------------ FORMAT 10G11.4
    ELSEIF (IP .EQ. 2) THEN
      WRITE(IOUT,21) PARNAM(I),(BUF(J,I),J=1,NPE)
      21 FORMAT(1X,A,1X,1PG11.4,9(1X,G11.4):/(11X,10(1X,G11.4)))
  !
  !------------ FORMAT 9G12.5
    ELSEIF (IP .EQ. 3) THEN
      WRITE(IOUT,31) PARNAM(I),(BUF(J,I),J=1,NPE)
      31 FORMAT(1X,A,1X,1PG12.5,8(1X,G12.5):/(11X,9(1X,G12.5)))
  !
  !------------ FORMAT 8G13.6
    ELSEIF (IP .EQ. 4) THEN
      WRITE(IOUT,41) PARNAM(I),(BUF(J,I),J=1,NPE)
      41 FORMAT(1X,A,1X,1PG13.6,7(1X,G13.6):/(11X,8(1X,G13.6)))
  !
  !------------ FORMAT 8G14.7
    ELSEIF (IP .EQ. 5) THEN
      WRITE(IOUT,51) PARNAM(I),(BUF(J,I),J=1,NPE)
      51 FORMAT(1X,A,1X,1PG14.7,7(1X,G14.7):/(11X,8(1X,G14.7)))
  !
  !------------ FORMAT 6G10.3
    ELSEIF (IP .EQ. 6) THEN
      WRITE(IOUT,61) PARNAM(I),(BUF(J,I),J=1,NPE)
      61 FORMAT(1X,A,1X,1PG10.3,5(1X,G10.3):/(11X,6(1X,G10.3)))
  !
  !------------ FORMAT 5G11.4
    ELSEIF (IP .EQ. 7) THEN
      WRITE(IOUT,71) PARNAM(I),(BUF(J,I),J=1,NPE)
      71 FORMAT(1X,A,1X,1PG11.4,4(1X,G11.4):/(11X,5(1X,G11.4)))
  !
  !------------ FORMAT 5G12.5
    ELSEIF (IP .EQ. 8) THEN
      WRITE(IOUT,81) PARNAM(I),(BUF(J,I),J=1,NPE)
      81 FORMAT(1X,A,1X,1PG12.5,4(1X,G12.5):/(11X,5(1X,G12.5)))
  !
  !------------ FORMAT 4G13.6
    ELSEIF (IP .EQ. 9) THEN
      WRITE(IOUT,91) PARNAM(I),(BUF(J,I),J=1,NPE)
      91 FORMAT(1X,A,1X,1PG13.6,3(1X,G13.6):/(11X,4(1X,G13.6)))
  !
  !------------ FORMAT 4G14.7
    ELSEIF (IP .EQ. 10) THEN
      WRITE(IOUT,101) PARNAM(I),(BUF(J,I),J=1,NPE)
      101 FORMAT(1X,A,1X,1PG14.7,3(1X,G14.7):/(11X,4(1X,G14.7)))
  !
    ENDIF
  ENDDO
  !
  RETURN
END SUBROUTINE YCINT_UPARPM
! **********************************************************************
SUBROUTINE YCINT_UCOLLBL(NPE,NLBL1,NLBL2,NSPACE,NCPL,NDIG,IOUT,PARNAM)
  !
  !-----VERSION 19980825 ERB
  !     ******************************************************************
  !     LABEL THE COLUMNS OF MATRIX PRINTOUT WITH PARAMETER NAMES
  !        NLBL1 IS THE START COLUMN (NUMBER)
  !        NLBL2 IS THE STOP COLUMN (NUMBER)
  !        NSPACE IS NUMBER OF BLANK SPACES TO LEAVE AT START OF LINE
  !        NCPL IS NUMBER OF COLUMNS PER LINE
  !        NDIG IS NUMBER OF CHARACTERS IN EACH COLUMN FIELD
  !        IOUT IS OUTPUT UNIT
  !     ******************************************************************
  !
  !        SPECIFICATIONS:
  !     ------------------------------------------------------------------
  IMPLICIT NONE
  ! argument variables
  INTEGER,                           INTENT(IN) :: NPE
  INTEGER,                           INTENT(IN) :: NLBL1
  INTEGER,                           INTENT(IN) :: NLBL2
  INTEGER,                           INTENT(IN) :: NSPACE
  INTEGER,                           INTENT(IN) :: NCPL
  INTEGER,                           INTENT(IN) :: NDIG
  INTEGER,                           INTENT(IN) :: IOUT
  CHARACTER(LEN=12),                 INTENT(IN) :: PARNAM(NPE)
  ! local variables
  CHARACTER(LEN=1)                              :: BF*200
  CHARACTER(LEN=1)                              :: DOT
  INTEGER                                       :: IND
  INTEGER                                       :: I
  INTEGER                                       :: J
  INTEGER                                       :: J1
  INTEGER                                       :: J2
  INTEGER                                       :: LENPN
  INTEGER                                       :: MINCOLSP
  INTEGER                                       :: N
  INTEGER                                       :: NBF
  INTEGER                                       :: NBF2
  INTEGER                                       :: NLBL
  INTEGER                                       :: NTOT
  INTEGER                                       :: NWRAP
  CHARACTER(LEN=1)                              :: SPACE
  !
  DATA DOT,SPACE/'.',' '/
  !     ------------------------------------------------------------------
  !
  !------CALCULATE # OF COLUMNS TO BE PRINTED (NLBL), WIDTH
  !------OF A LINE (NTOT), NUMBER OF LINES (NWRAP).
  WRITE(IOUT,1)
  1 FORMAT(1X)
  NLBL = NLBL2-NLBL1+1
  N = NLBL
  IF(NLBL.GT.NCPL) N = NCPL
  LENPN = LEN(PARNAM(1))
  MINCOLSP = LENPN+1
  IF (NDIG.LT.MINCOLSP) THEN
    WRITE(IOUT,200) NDIG,MINCOLSP
    200 FORMAT(' SPECIFIED FIELD WIDTH TOO SMALL FOR PARNAM', &
        '--STOP EXECUTION (UCOLLBL)',/ &
        ' NDIG = ',I2,'   MINCOLSP = ',I2)
    STOP
  ENDIF
  NTOT = NSPACE+LENPN+N*NDIG
  IND = (NDIG-LENPN-1)/2
  NWRAP = (NLBL-1)/NCPL+1
  J1 = NLBL1-NCPL
  J2 = NLBL1-1
  !
  !------BUILD AND PRINT EACH LINE
  DO N=1,NWRAP
    !
    !-----CLEAR THE BUFFER (BF).
    DO I=1,130
      BF(I:I)=SPACE
    ENDDO
    NBF = MINCOLSP+1+IND-NDIG
    !
    !-----DETERMINE FIRST (J1) AND LAST (J2) COLUMN # FOR THIS LINE.
    J1=J1+NCPL
    J2=J2+NCPL
    IF(J2.GT.NLBL2) J2=NLBL2
    !-----LOAD THE COLUMN LABELS INTO THE BUFFER.
    DO J=J1,J2
      NBF=NBF+NDIG
      NBF2 = NBF+LENPN-1
      BF(NBF:NBF2) = PARNAM(J)
    ENDDO
    !
    !-----PRINT THE CONTENTS OF THE BUFFER (I.E. PRINT THE LINE).
    WRITE(IOUT,31) BF(1:NBF2)
    31 FORMAT(1X,A)
  !
  ENDDO
  !
  !7------PRINT A LINE OF DOTS (FOR ESTHETIC PURPOSES ONLY).
  WRITE(IOUT,51) (DOT,I=1,NTOT)
  51 FORMAT(1X,200A1)
  !
  RETURN
  END SUBROUTINE YCINT_UCOLLBL
END MODULE LINEAR_UNCERTAINTY_MOD