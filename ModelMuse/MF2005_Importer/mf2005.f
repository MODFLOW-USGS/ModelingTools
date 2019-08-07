C     ******************************************************************
C     MAIN CODE FOR U.S. GEOLOGICAL SURVEY MODULAR MODEL -- MODFLOW-2005
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C1------USE package modules.
      USE GLOBAL
      USE GWFBASMODULE
      USE GWFHUFMODULE, ONLY:IOHUFHDS,IOHUFFLWS
      USE GWFEVTMODULE, ONLY:NEVTOP
      USE GWFRCHMODULE, ONLY:NRCHOP
      USE GWFLAKMODULE, ONLY:NLAKESAR,THETA,STGOLD,STGNEW,VOL
      USE GWFUZFMODULE, ONLY: IUZFBND, FINF, VKS
      USE GWFSFRMODULE, ONLY:NUMTAB
      USE GWFNWTMODULE, ONLY:LINMETH,ICNVGFLG,ITREAL
      USE PCGMODULE
c      USE LMGMODULE
      USE SIPMODULE
      USE DE4MODULE
      USE GMGMODULE
      USE PCGN
      INCLUDE 'openspec.inc'
C
C-------ASSIGN VERSION NUMBER AND DATE
      CHARACTER*40 VERSION
      CHARACTER*10 MFVNAM
      PARAMETER (VERSION='1.11.00 8/8/2013')
      PARAMETER (MFVNAM='-2005')
C
      CHARACTER*80 HEADNG(2)
      CHARACTER*200 FNAME
      INTEGER IBDT(8)
C
      CHARACTER*4 CUNIT(NIUNIT)
      DATA CUNIT/'BCF6', 'WEL ', 'DRN ', 'RIV ', 'EVT ', 'gfd ', 'GHB ',  !  7
     &           'RCH ', 'SIP ', 'DE4 ', '    ', 'OC  ', 'PCG ', 'lmg ',  ! 14
     &           'gwt ', 'FHB ', 'RES ', 'STR ', 'IBS ', 'CHD ', 'HFB6',  ! 21
     &           'LAK ', 'LPF ', 'DIS ', '    ', 'PVAL', '    ', 'HOB ',  ! 28
     &           '    ', '    ', 'ZONE', 'MULT', 'DROB', 'RVOB', 'GBOB',  ! 35
     &           'STOB', 'HUF2', 'CHOB', 'ETS ', 'DRT ', '    ', 'GMG ',  ! 42
     &           'HYD ', 'SFR ', '    ', 'GAGE', 'LVDA', '    ', 'LMT6',  ! 49
     &           'MNW2', 'MNWI', 'MNW1', 'KDEP', 'SUB ', 'UZF ', 'gwm ',  ! 56
     &           'SWT ', 'cfp ', 'PCGN', '    ', '    ', 'UPW ', 'NWT ',  ! 63
     &           'SWR ', 'SWI2', '    ', '    ', '    ', '    ', '    ',  ! 70
     &           30*'    '/                                               ! 71-100 - SWR - JDH
C     ------------------------------------------------------------------
C
C2------WRITE BANNER TO SCREEN AND DEFINE CONSTANTS.
      WRITE (*,*) 'ModflowImporter based on:'
      WRITE (*,1) MFVNAM,VERSION
    1 FORMAT (/,34X,'MODFLOW',A,/,
     &4X,'U.S. GEOLOGICAL SURVEY MODULAR FINITE-DIFFERENCE',
     &' GROUND-WATER FLOW MODEL',/,29X,'Version ',A/)
      INUNIT = 99
      NCVGERR=0
C
C3------GET THE NAME OF THE NAME FILE
      CALL GETNAMFIL(FNAME)
      MAXUNIT= INUNIT
C
C4------OPEN NAME FILE.
      OPEN (UNIT=INUNIT,FILE=FNAME,STATUS='OLD',ACTION=ACTION(1))
      NC=INDEX(FNAME,' ')
      WRITE(*,490)' Using NAME file: ',FNAME(1:NC)
  490 FORMAT(A,A)
C
C5------Get current date and time, assign to IBDT, and write to screen
      CALL DATE_AND_TIME(VALUES=IBDT)
      WRITE(*,2) (IBDT(I),I=1,3),(IBDT(I),I=5,7)
    2 FORMAT(1X,'Run start date and time (yyyy/mm/dd hh:mm:ss): ',
     &I4,'/',I2.2,'/',I2.2,1X,I2,':',I2.2,':',I2.2,/)
C
C6------ALLOCATE AND READ (AR) PROCEDURE
      IGRID=1
      NSOL=1
      CALL GWF2BAS7AR(INUNIT,CUNIT,VERSION,24,31,32,MAXUNIT,IGRID,12,
     1                HEADNG,26,MFVNAM)
      IF(IUNIT(50).GT.0 .AND. IUNIT(52).GT.0) THEN
        WRITE(IOUT,'(1X,/,1X,A)')
     1  'MNW1 and MNW2 cannot both be active in the same simulation'
        CALL USTOP(' ')
      END IF
      IF(IUNIT(1).GT.0) CALL GWF2BCF7AR(IUNIT(1),IGRID)
      IF(IUNIT(23).GT.0) CALL GWF2LPF7AR(IUNIT(23),IGRID)
      IF(IUNIT(37).GT.0) CALL GWF2HUF7AR(IUNIT(37),IUNIT(47),
     1                                     IUNIT(53),IGRID)
! Allocate arrays for Newton Solver
      IF(IUNIT(63).GT.0) CALL GWF2NWT1AR(IUNIT(63),MXITER, 
     1                                   IUNIT(22),IGRID)
      IF(IUNIT(62).GT.0) CALL GWF2UPW1AR(IUNIT(62), Igrid)
      IF(IUNIT(2).GT.0) CALL GWF2WEL7AR(IUNIT(2),IUNIT(63),IGRID)
      IF(IUNIT(3).GT.0) CALL GWF2DRN7AR(IUNIT(3),IGRID)
      IF(IUNIT(4).GT.0) CALL GWF2RIV7AR(IUNIT(4),IGRID)
      IF(IUNIT(5).GT.0) CALL GWF2EVT7AR(IUNIT(5),IGRID)
      IF(IUNIT(7).GT.0) CALL GWF2GHB7AR(IUNIT(7),IGRID)
      IF(IUNIT(8).GT.0) CALL GWF2RCH7AR(IUNIT(8),IGRID)
      IF(IUNIT(16).GT.0) CALL GWF2FHB7AR(IUNIT(16),IGRID)
      IF(IUNIT(17).GT.0) CALL GWF2RES7AR(IUNIT(17),IGRID)
      IF(IUNIT(18).GT.0) CALL GWF2STR7AR(IUNIT(18),IGRID)
      IF(IUNIT(19).GT.0) CALL GWF2IBS7AR(IUNIT(19),IUNIT(54),IGRID)
      IF(IUNIT(20).GT.0) CALL GWF2CHD7AR(IUNIT(20),IGRID)
      IF(IUNIT(21).GT.0) CALL GWF2HFB7AR(IUNIT(21),IGRID)
      IF(IUNIT(44).GT.0) CALL GWF2SFR7AR(IUNIT(44),IUNIT(1),IUNIT(23),
     1                           IUNIT(37),IUNIT(15),NSOL,IOUTS,
     2                           IUNIT(62),IUNIT(55),IGRID)
      IF(IUNIT(55).GT.0) CALL GWF2UZF1AR(IUNIT(55),IUNIT(1),
     1                                   IUNIT(23),IUNIT(37),
     2                                   IUNIT(63),IGRID)
      IF(IUNIT(22).GT.0 .OR. IUNIT(44).GT.0) CALL GWF2LAK7AR(
     1             IUNIT(22),IUNIT(44),IUNIT(15),IUNIT(55),NSOL,IGRID)
      IF(IUNIT(46).GT.0) CALL GWF2GAG7AR(IUNIT(46),IUNIT(44),
     1                                     IUNIT(22),IGRID)
      IF(IUNIT(39).GT.0) CALL GWF2ETS7AR(IUNIT(39),IGRID)
      IF(IUNIT(40).GT.0) CALL GWF2DRT7AR(IUNIT(40),IGRID)
      IF(IUNIT(54).GT.0) CALL GWF2SUB7AR(IUNIT(54),IGRID)
      IF(IUNIT(9).GT.0) CALL SIP7AR(IUNIT(9),MXITER,IGRID)
      IF(IUNIT(10).GT.0) CALL DE47AR(IUNIT(10),MXITER,IGRID)
      IF(IUNIT(13).GT.0) CALL PCG7AR(IUNIT(13),MXITER,IGRID)
c      IF(IUNIT(14).GT.0) CALL LMG7AR(IUNIT(14),MXITER,IGRID)
      IF(IUNIT(42).GT.0) CALL GMG7AR(IUNIT(42),MXITER,IGRID)
      IF(IUNIT(59).GT.0) CALL PCGN2AR(IUNIT(59),IFREFM,MXITER,IGRID)
      IF(IUNIT(50).GT.0) CALL GWF2MNW27AR(IUNIT(50),IGRID)
      IF(IUNIT(51).GT.0) CALL GWF2MNW2I7AR(IUNIT(51),IUNIT(50),IGRID)
      IF(IUNIT(52).GT.0) CALL GWF2MNW17AR(IUNIT(52),IUNIT(9),
     1                     IUNIT(10),IUNIT(63),0,IUNIT(13),
     2                     IUNIT(42),IUNIT(59),FNAME,IGRID)
      IF(IUNIT(57).GT.0) CALL GWF2SWT7AR(IUNIT(57),IGRID)
      IF(IUNIT(64).GT.0) CALL GWF2SWR7AR(IUNIT(64),
     2                        IUNIT(1),IUNIT(23),IUNIT(37),
     3                        IUNIT(62),IUNIT(44),IUNIT(63),IGRID)  !SWR  - JDH
      IF(IUNIT(65).GT.0) CALL GWF2SWI2AR(IUNIT(65),IUNIT(1),
     2                     IUNIT(23),IUNIT(37),IUNIT(62),IGRID)
      IF(IUNIT(43).GT.0) CALL GWF2HYD7BAS7AR(IUNIT(43),IGRID)
      IF(IUNIT(43).GT.0 .AND. IUNIT(19).GT.0)
     1                   CALL GWF2HYD7IBS7AR(IUNIT(43),IGRID)
      IF(IUNIT(43).GT.0 .AND. IUNIT(54).GT.0)
     1                   CALL GWF2HYD7SUB7AR(IUNIT(43),IGRID)
      IF(IUNIT(43).GT.0 .AND. IUNIT(18).GT.0)
     1                   CALL GWF2HYD7STR7AR(IUNIT(43),IGRID)
      IF(IUNIT(43).GT.0 .AND. IUNIT(44).GT.0)
     1                   CALL GWF2HYD7SFR7AR(IUNIT(43),IGRID)
!      IF(IUNIT(49).GT.0) CALL LMT7BAS7AR(INUNIT,CUNIT,IGRID)
C
C  Observation allocate and read
      CALL OBS2BAS7AR(IUNIT(28),IGRID)
      IF(IUNIT(33).GT.0) CALL OBS2DRN7AR(IUNIT(33),IUNIT(3),IGRID)
      IF(IUNIT(34).GT.0) CALL OBS2RIV7AR(IUNIT(34),IUNIT(4),IGRID)
      IF(IUNIT(35).GT.0) CALL OBS2GHB7AR(IUNIT(35),IUNIT(7),IGRID)
      IF(IUNIT(36).GT.0) CALL OBS2STR7AR(IUNIT(36),IUNIT(18),IGRID)
      IF(IUNIT(38).GT.0) CALL OBS2CHD7AR(IUNIT(38),IGRID)
C
C7------SIMULATE EACH STRESS PERIOD.
      DO 100 KPER = 1, NPER
        WRITE(IOUT,*)'KPER:'
        WRITE(IOUT,*)KPER
        KKPER = KPER
        CALL GWF2BAS7ST(KKPER,IGRID)
!        IF(IUNIT(19).GT.0) CALL GWF2IBS7ST(KKPER,IGRID)
!        IF(IUNIT(54).GT.0) CALL GWF2SUB7ST(KKPER,IGRID)
!        IF(IUNIT(57).GT.0) CALL GWF2SWT7ST(KKPER,IGRID)
C
C7B-----READ AND PREPARE INFORMATION FOR STRESS PERIOD.
C----------READ USING PACKAGE READ AND PREPARE MODULES.
        IF(IUNIT(2).GT.0) CALL GWF2WEL7RP(IUNIT(2),KKPER,IGRID)
        IF(IUNIT(3).GT.0) CALL GWF2DRN7RP(IUNIT(3),IGRID)
        IF(IUNIT(4).GT.0) CALL GWF2RIV7RP(IUNIT(4),IGRID)
        IF(IUNIT(5).GT.0) CALL GWF2EVT7RP(IUNIT(5),IGRID)
        IF(IUNIT(7).GT.0) CALL GWF2GHB7RP(IUNIT(7),IGRID)
        IF(IUNIT(8).GT.0) CALL GWF2RCH7RP(IUNIT(8),IGRID)
        IF(IUNIT(17).GT.0) CALL GWF2RES7RP(IUNIT(17),IGRID)
        IF(IUNIT(18).GT.0) CALL GWF2STR7RP(IUNIT(18),IGRID)
        IF(IUNIT(43).GT.0 .AND. IUNIT(18).GT.0)
     1                     CALL GWF2HYD7STR7RP(IUNIT(43),KKPER,IGRID)
        IF(IUNIT(20).GT.0) CALL GWF2CHD7RP(IUNIT(20),IGRID)
        IF(IUNIT(44).GT.0) CALL GWF2SFR7RP(IUNIT(44),IUNIT(15),
     1                                     IUNIT(22),KKPER,KKSTP,
     2                                     NSOL,IOUTS,IUNIT(1),
     3                                     IUNIT(23),IUNIT(37),
     4                                     IUNIT(62), IUNIT(55), IGRID)
      IF(IUNIT(43).GT.0 .AND. IUNIT(44).GT.0)
     1                     CALL GWF2HYD7SFR7RP(IUNIT(43),KKPER,IGRID)
        IF(IUNIT(55).GT.0) CALL GWF2UZF1RP(IUNIT(55),KKPER,IUNIT(44),
     1                                     IGRID)
        IF(IUNIT(22).GT.0) CALL GWF2LAK7RP(IUNIT(22),IUNIT(1),
     1               IUNIT(15),IUNIT(23),IUNIT(37),IUNIT(44),IUNIT(55),
     2               KKPER,NSOL,IOUTS,IGRID)
        IF(IUNIT(46).GT.0.AND.KKPER.EQ.1) CALL GWF2GAG7RP(IUNIT(15),
     1             IUNIT(22),IUNIT(55),NSOL,IGRID)
        IF(IUNIT(39).GT.0) CALL GWF2ETS7RP(IUNIT(39),IGRID)
        IF(IUNIT(40).GT.0) CALL GWF2DRT7RP(IUNIT(40),IGRID)
        IF(IUNIT(50).GT.0) CALL GWF2MNW27RP(IUNIT(50),KKPER,IUNIT(9),
     1                     IUNIT(10),0,IUNIT(13),IUNIT(42),
     2                     IUNIT(59),0,IGRID)
        IF(IUNIT(51).GT.0.AND.KKPER.EQ.1) CALL GWF2MNW2I7RP(IUNIT(51),
     1                     0,IGRID)
        IF(IUNIT(52).GT.0) CALL GWF2MNW17RP(IUNIT(52),IUNIT(1),
     1                            IUNIT(23),IUNIT(37),IUNIT(62),KKPER,
     2                            IGRID)
        IF(IUNIT(64).GT.0) CALL GWF2SWR7RP(IUNIT(64),KKPER,IGRID)  !SWR - JDH
C
C7C-----SIMULATE EACH TIME STEP.
!        DO 90 KSTP = 1, NSTP(KPER)
        DO 90 KSTP = 1, 1
          KKSTP = KSTP
C
C7C1----CALCULATE TIME STEP LENGTH. SET HOLD=HNEW.
!          IF(IUNIT(62).GT.0 ) CALL GWF2UPWUPDATE(1,Igrid)
          CALL GWF2BAS7AD(KKPER,KKSTP,IGRID)
!          IF(IUNIT(62).GT.0) CALL GWF2UPW1AD(IGRID)
!          IF(IUNIT(20).GT.0) CALL GWF2CHD7AD(KKPER,IGRID)
!          IF(IUNIT(1).GT.0) CALL GWF2BCF7AD(KKPER,IGRID)
!          IF(IUNIT(17).GT.0) CALL GWF2RES7AD(KKSTP,KKPER,IGRID)
!          IF(IUNIT(23).GT.0) CALL GWF2LPF7AD(KKPER,IGRID)
!          IF(IUNIT(37).GT.0) CALL GWF2HUF7AD(KKPER,IGRID)
!          IF(IUNIT(16).GT.0) CALL GWF2FHB7AD(IGRID)
!          IF(IUNIT(22).GT.0) CALL GWF2LAK7AD(KKPER,KKSTP,IUNIT(15),
!     1                                           IGRID)
!          IF( IUNIT(44).GT.0 .AND. NUMTAB.GT.0 ) 
!     2                             CALL GWF2SFR7AD(IUNIT(22))
!          IF(IUNIT(65).GT.0) CALL GWF2SWI2AD(KKSTP,KKPER,IGRID)
          IF(IUNIT(50).GT.0) THEN
!            IF (IUNIT(1).GT.0) THEN
!              CALL GWF2MNW27BCF(KPER,IGRID)
!            ELSE IF (IUNIT(23).GT.0) THEN
!              CALL GWF2MNW27LPF(KPER,IGRID)
!            ELSE IF(IUNIT(37).GT.0) THEN
!              CALL GWF2MNW27HUF(KPER,IGRID)
!            ELSE IF(IUNIT(62).GT.0) THEN
!              CALL GWF2MNW27UPW(KPER,IGRID)
!            ELSE
!              WRITE(IOUT,1000)
! 1000         FORMAT(/1X,
!     &      '***ERROR: MNW2 PACKAGE DOES NOT SUPPORT',/,
!     &      ' SELECTED FLOW PACKAGE',/,
!     &      ' (MNW2 DOES FULLY SUPPORT BCF, LPF, AND HUF PACKAGES)',/,
!     &      ' -- STOP EXECUTION')
!              CALL USTOP('MNW2 error-flow package')
!            END IF
!            CALL GWF2MNW27AD(KKSTP,KKPER,IGRID)
          END IF
!          IF(IUNIT(52).GT.0) CALL GWF2MNW17AD(IUNIT(1),IUNIT(23),
!     1                                       IUNIT(37),IGRID)
C
C---------INDICATE IN PRINTOUT THAT SOLUTION IS FOR HEADS
!          CALL UMESPR('SOLVING FOR HEAD',' ',IOUT)
          WRITE(*,25)KPER,KSTP
   25     FORMAT(' Solving:  Stress period: ',i5,4x,
     &       'Time step: ',i5,4x,'Ground-Water Flow Eqn.')
C
C7C2----ITERATIVELY FORMULATE AND SOLVE THE FLOW EQUATIONS.
!          DO 30 KITER = 1, MXITER
            KKITER = 1
C
C7C2A---FORMULATE THE FINITE DIFFERENCE EQUATIONS.
C7C2C---IF CONVERGENCE CRITERION HAS BEEN MET STOP ITERATING.
! rbw begin new
!            GOTO 33
!            IF (ICNVG.EQ.1) GOTO 33
! rbw end new
 ! 30      CONTINUE
 !         KITER = MXITER
C
   33     CONTINUE
C
C7C3----DETERMINE WHICH OUTPUT IS NEEDED.
!          CALL GWF2BAS7OC(KKSTP,KKPER,ICNVG,IUNIT(12),IGRID)
C
C7C4----CALCULATE BUDGET TERMS. SAVE CELL-BY-CELL FLOW TERMS.
          MSUM = 1
C
C  Observation and hydrograph simulated equivalents
!          CALL OBS2BAS7SE(IUNIT(28),IGRID)
!          IF(IUNIT(33).GT.0) CALL OBS2DRN7SE(IGRID)
!          IF(IUNIT(34).GT.0) CALL OBS2RIV7SE(IGRID)
!          IF(IUNIT(35).GT.0) CALL OBS2GHB7SE(IGRID)
!          IF(IUNIT(36).GT.0) CALL OBS2STR7SE(IGRID)
!          IF(IUNIT(38).GT.0) CALL OBS2CHD7SE(KKPER,IGRID)
!          IF(IUNIT(43).GT.0) CALL GWF2HYD7BAS7SE(1,IGRID)
!          IF(IUNIT(43).GT.0 .AND. IUNIT(19).GT.0)
!     1                              CALL GWF2HYD7IBS7SE(1,IGRID)
!          IF(IUNIT(43).GT.0 .AND. IUNIT(54).GT.0)
!     1                              CALL GWF2HYD7SUB7SE(1,IGRID)
!          IF(IUNIT(43).GT.0 .AND. IUNIT(18).GT.0)
!     1                              CALL GWF2HYD7STR7SE(1,IGRID)
!          IF(IUNIT(43).GT.0 .AND. IUNIT(44).GT.0)
!     1                              CALL GWF2HYD7SFR7SE(1,IGRID)
C
C7C5---PRINT AND/OR SAVE DATA.
!          CALL GWF2BAS7OT(KKSTP,KKPER,ICNVG,1,IGRID)
!          IF(IUNIT(19).GT.0) CALL GWF2IBS7OT(KKSTP,KKPER,IUNIT(19),
!     1                                       IGRID)
!          IF(IUNIT(37).GT.0)THEN
!            IF(IOHUFHDS .NE.0 .OR.IOHUFFLWS .NE.0)
!     1         CALL GWF2HUF7OT(KKSTP,KKPER,ICNVG,1,IGRID)
!          ENDIF
!          IF(IUNIT(51).NE.0) CALL GWF2MNW2I7OT(NSTP(KKPER),KKSTP,
!     1                       KKPER,IGRID)
!          IF(IUNIT(54).GT.0) CALL GWF2SUB7OT(KKSTP,KKPER,IUNIT(54),
!     1                                       IGRID)
!          IF(IUNIT(57).GT.0) CALL GWF2SWT7OT(KKSTP,KKPER,IGRID)
!          IF(IUNIT(43).GT.0) CALL GWF2HYD7BAS7OT(KKSTP,KKPER,IGRID)
C
C7C6---JUMP TO END OF PROGRAM IF CONVERGENCE WAS NOT ACHIEVED.
!          IF(ICNVG.EQ.0) THEN
!            NCVGERR=NCVGERR+1
!            WRITE(IOUT,87) BUDPERC
!   87       FORMAT(1X,'FAILURE TO MEET SOLVER CONVERGENCE CRITERIA',/
!     1       1X,'BUDGET PERCENT DISCREPANCY IS',F10.4)
!            IF(ABS(BUDPERC).GT.STOPER) THEN
!              WRITE(IOUT,*) 'STOPPING SIMULATION'
!              GO TO 110
!            ELSE
!              WRITE(IOUT,*) 'CONTINUING EXECUTION'
!            END IF
!          END IF
C
C-----END OF TIME STEP (KSTP) AND STRESS PERIOD (KPER) LOOPS
   90   CONTINUE
  100 CONTINUE
C
C
!      IF(IUNIT(52).NE.0) CALL GWF2MNW17OT(IGRID)
C
C8------END OF SIMULATION
C-------SAVE RESTART RECORDS FOR SUB PACKAGE
!  110 IF(IUNIT(54).GT.0) CALL GWF2SUB7SV(IGRID)
C
C  Observation output
!      IF(IUNIT(28).GT.0) CALL OBS2BAS7OT(IUNIT(28),IGRID)
!      IF(IUNIT(33).GT.0) CALL OBS2DRN7OT(IGRID)
!      IF(IUNIT(34).GT.0) CALL OBS2RIV7OT(IGRID)
!      IF(IUNIT(35).GT.0) CALL OBS2GHB7OT(IGRID)
!      IF(IUNIT(36).GT.0) CALL OBS2STR7OT(IGRID)
!      IF(IUNIT(38).GT.0) CALL OBS2CHD7OT(IGRID)
      CALL GLO1BAS6ET(IOUT,IBDT,1)
C
C9------CLOSE FILES AND DEALLOCATE MEMORY.  GWF2BAS7DA MUST BE CALLED
C9------LAST BECAUSE IT DEALLOCATES IUNIT.
      CALL SGWF2BAS7PNT(IGRID)
      IF(IUNIT(1).GT.0) CALL GWF2BCF7DA(IGRID)
      IF(IUNIT(2).GT.0) CALL GWF2WEL7DA(IGRID)
      IF(IUNIT(3).GT.0) CALL GWF2DRN7DA(IGRID)
      IF(IUNIT(4).GT.0) CALL GWF2RIV7DA(IGRID)
      IF(IUNIT(5).GT.0) CALL GWF2EVT7DA(IGRID)
      IF(IUNIT(7).GT.0) CALL GWF2GHB7DA(IGRID)
      IF(IUNIT(8).GT.0) CALL GWF2RCH7DA(IGRID)
      IF(IUNIT(9).GT.0) CALL SIP7DA(IGRID)
      IF(IUNIT(10).GT.0) CALL DE47DA(IGRID)
      IF(IUNIT(13).GT.0) CALL PCG7DA(IGRID)
      IF(IUNIT(63).GT.0) THEN    
!        IF(LINMETH.EQ.1) THEN
!          CALL GMRES7DA(IGRID)
!        ELSEIF(LINMETH.EQ.2) THEN
!          CALL XMD7DA(IGRID)
!        END IF
        CALL GWF2NWT1DA(IGRID)
      END IF
      IF(IUNIT(62).GT.0) CALL GWF2UPW1DA(IGRID)      
c      IF(IUNIT(14).GT.0) CALL LMG7DA(IGRID)
      IF(IUNIT(16).GT.0) CALL GWF2FHB7DA(IGRID)
      IF(IUNIT(17).GT.0) CALL GWF2RES7DA(IGRID)
      IF(IUNIT(18).GT.0) CALL GWF2STR7DA(IGRID)
      IF(IUNIT(19).GT.0) CALL GWF2IBS7DA(IGRID)
      IF(IUNIT(20).GT.0) CALL GWF2CHD7DA(IGRID)
      IF(IUNIT(43).GT.0) CALL GWF2HYD7DA(IGRID)
      IF(IUNIT(21).GT.0) CALL GWF2HFB7DA(IGRID)
      IF(IUNIT(22).GT.0 .OR. IUNIT(44).GT.0)CALL GWF2LAK7DA(IUNIT(22),
     1                                              IGRID)
      IF(IUNIT(23).GT.0) CALL GWF2LPF7DA(IGRID)
      IF(IUNIT(37).GT.0) CALL GWF2HUF7DA(IGRID)
      IF(IUNIT(39).GT.0) CALL GWF2ETS7DA(IGRID)
      IF(IUNIT(40).GT.0) CALL GWF2DRT7DA(IGRID)
      IF(IUNIT(42).GT.0) CALL GMG7DA(IGRID)
      IF(IUNIT(59).GT.0) CALL PCGN2DA(IGRID)
      IF(IUNIT(44).GT.0) CALL GWF2SFR7DA(IGRID)
      IF(IUNIT(46).GT.0) CALL GWF2GAG7DA(IGRID)
      IF(IUNIT(50).GT.0) CALL GWF2MNW27DA(IGRID)
      IF(IUNIT(51).GT.0) CALL GWF2MNW2I7DA(IGRID)
      IF(IUNIT(52).GT.0) CALL GWF2MNW17DA(IGRID)
      IF(IUNIT(54).GT.0) CALL GWF2SUB7DA(IGRID)
      IF(IUNIT(55).GT.0) CALL GWF2UZF1DA(IGRID)
      IF(IUNIT(57).GT.0) CALL GWF2SWT7DA(IGRID)
      IF(IUNIT(64).GT.0) CALL GWF2SWR7DA(IGRID)  !SWR - JDH      
      IF(IUNIT(65).GT.0) CALL GWF2SWI2DA(IGRID)
      CALL OBS2BAS7DA(IUNIT(28),IGRID)
      IF(IUNIT(33).GT.0) CALL OBS2DRN7DA(IGRID)
      IF(IUNIT(34).GT.0) CALL OBS2RIV7DA(IGRID)
      IF(IUNIT(35).GT.0) CALL OBS2GHB7DA(IGRID)
      IF(IUNIT(36).GT.0) CALL OBS2STR7DA(IGRID)
      IF(IUNIT(38).GT.0) CALL OBS2CHD7DA(IGRID)
      IF(IUNIT(43).GT.0) CALL GWF2HYD7DA(IGRID)
!      IF(IUNIT(49).GT.0) CALL LMT7DA(IGRID)
      CALL GWF2BAS7DA(IGRID)
C
C10-----END OF PROGRAM.
!      IF(NCVGERR.GT.0) THEN
!        WRITE(*,*) 'FAILED TO MEET SOLVER CONVERGENCE CRITERIA ',
!     1          NCVGERR,' TIME(S)'
!      ELSE
        WRITE(*,*) ' Normal termination of simulation'
!      END IF
      CALL USTOP(' ')
C
      END
      SUBROUTINE GETNAMFIL(FNAME)
C     ******************************************************************
C     GET THE NAME OF THE NAME FILE
C     ******************************************************************
C        SPECIFICATIONS:
C
C     ------------------------------------------------------------------
      CHARACTER*(*) FNAME
      CHARACTER*200 COMLIN
      LOGICAL EXISTS
C     ------------------------------------------------------------------
C
C Get name file from command line or user interaction.
        FNAME=' '
        COMLIN=' '
C *** Subroutines GETARG and GETCL are extensions to Fortran 90/95 that
C *** allow a program to retrieve command-line arguments.  To enable
C *** Modflow-2000 to read the name of a Name file from the command
C *** line, either GETARG or GETCL must be called, but not both.  As
C *** distributed, the call to GETARG is uncommented.  For compilers
C *** that support GETCL but not GETARG, comment out the call to GETARG
C *** and uncomment the call to GETCL.  The calls to both GETARG and
C *** GETCL may be commented out for compilers that do not support
C *** either extension.
        CALL GETARG(1,COMLIN)
C        CALL GETCL(COMLIN)
        ICOL = 1
        IF(COMLIN.NE.' ') THEN
          FNAME=COMLIN
        ELSE
   15     WRITE (*,*) ' Enter the name of the NAME FILE: '
          READ (*,'(A)') FNAME
          CALL URWORD(FNAME,ICOL,ISTART,ISTOP,0,N,R,0,0)
          FNAME=FNAME(ISTART:ISTOP)
          IF (FNAME.EQ.' ') GOTO 15
        ENDIF
        INQUIRE (FILE=FNAME,EXIST=EXISTS)
        IF(.NOT.EXISTS) THEN
          NC=INDEX(FNAME,' ')
          FNAME(NC:NC+3)='.nam'
          INQUIRE (FILE=FNAME,EXIST=EXISTS)
          IF(.NOT.EXISTS) THEN
            WRITE (*,480) FNAME(1:NC-1),FNAME(1:NC+3)
  480       FORMAT(1X,'Can''t find name file ',A,' or ',A)
            CALL USTOP(' ')
          ENDIF
        ENDIF
C
      RETURN
      END
      SUBROUTINE GLO1BAS6ET(IOUT,IBDT,IPRTIM)
C     ******************************************************************
C     Get end time and calculate elapsed time
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER IBDT(8), IEDT(8), IDPM(12)
      DATA IDPM/31,28,31,30,31,30,31,31,30,31,30,31/ ! Days per month
      DATA NSPD/86400/  ! Seconds per day
C     ------------------------------------------------------------------
C
C     Get current date and time, assign to IEDT, and write.
      CALL DATE_AND_TIME(VALUES=IEDT)
      WRITE(*,1000) (IEDT(I),I=1,3),(IEDT(I),I=5,7)
 1000 FORMAT(1X,'Run end date and time (yyyy/mm/dd hh:mm:ss): ',
     &I4,'/',I2.2,'/',I2.2,1X,I2,':',I2.2,':',I2.2)
      IF(IPRTIM.GT.0) THEN
        WRITE(IOUT,'(1X)')
        WRITE(IOUT,1000) (IEDT(I),I=1,3),(IEDT(I),I=5,7)
      END IF
C
C     Calculate elapsed time in days and seconds
      NDAYS=0
      LEAP=0
      IF (MOD(IEDT(1),4).EQ.0) LEAP = 1
      IBD = IBDT(3)            ! BEGIN DAY
      IED = IEDT(3)            ! END DAY
C     FIND DAYS
      IF (IBDT(2).NE.IEDT(2)) THEN
C       MONTHS DIFFER
        MB = IBDT(2)             ! BEGIN MONTH
        ME = IEDT(2)             ! END MONTH
        NM = ME-MB+1             ! NUMBER OF MONTHS TO LOOK AT
        IF (MB.GT.ME) NM = NM+12
        MC=MB-1
        DO 10 M=1,NM
          MC=MC+1                ! MC IS CURRENT MONTH
          IF (MC.EQ.13) MC = 1
          IF (MC.EQ.MB) THEN
            NDAYS = NDAYS+IDPM(MC)-IBD
            IF (MC.EQ.2) NDAYS = NDAYS + LEAP
          ELSEIF (MC.EQ.ME) THEN
            NDAYS = NDAYS+IED
          ELSE
            NDAYS = NDAYS+IDPM(MC)
            IF (MC.EQ.2) NDAYS = NDAYS + LEAP
          ENDIF
   10   CONTINUE
      ELSEIF (IBD.LT.IED) THEN
C       START AND END IN SAME MONTH, ONLY ACCOUNT FOR DAYS
        NDAYS = IED-IBD
      ENDIF
      ELSEC=NDAYS*NSPD
C
C     ADD OR SUBTRACT SECONDS
      ELSEC = ELSEC+(IEDT(5)-IBDT(5))*3600.0
      ELSEC = ELSEC+(IEDT(6)-IBDT(6))*60.0
      ELSEC = ELSEC+(IEDT(7)-IBDT(7))
      ELSEC = ELSEC+(IEDT(8)-IBDT(8))*0.001
C
C     CONVERT SECONDS TO DAYS, HOURS, MINUTES, AND SECONDS
      NDAYS = ELSEC/NSPD
      RSECS = MOD(ELSEC,86400.0)
      NHOURS = RSECS/3600.0
      RSECS = MOD(RSECS,3600.0)
      NMINS = RSECS/60.0
      RSECS = MOD(RSECS,60.0)
      NSECS = RSECS
      RSECS = MOD(RSECS,1.0)
      MSECS = NINT(RSECS*1000.0)
      NRSECS = NSECS
      IF (RSECS.GE.0.5) NRSECS=NRSECS+1
C
C     Write elapsed time to screen
        IF (NDAYS.GT.0) THEN
          WRITE(*,1010) NDAYS,NHOURS,NMINS,NRSECS
 1010     FORMAT(1X,'Elapsed run time: ',I3,' Days, ',I2,' Hours, ',I2,
     &      ' Minutes, ',I2,' Seconds',/)
        ELSEIF (NHOURS.GT.0) THEN
          WRITE(*,1020) NHOURS,NMINS,NRSECS
 1020     FORMAT(1X,'Elapsed run time: ',I2,' Hours, ',I2,
     &      ' Minutes, ',I2,' Seconds',/)
        ELSEIF (NMINS.GT.0) THEN
          WRITE(*,1030) NMINS,NSECS,MSECS
 1030     FORMAT(1X,'Elapsed run time: ',I2,' Minutes, ',
     &      I2,'.',I3.3,' Seconds',/)
        ELSE
          WRITE(*,1040) NSECS,MSECS
 1040     FORMAT(1X,'Elapsed run time: ',I2,'.',I3.3,' Seconds',/)
        ENDIF
C
C     Write times to file if requested
      IF(IPRTIM.GT.0) THEN
        IF (NDAYS.GT.0) THEN
          WRITE(IOUT,1010) NDAYS,NHOURS,NMINS,NRSECS
        ELSEIF (NHOURS.GT.0) THEN
          WRITE(IOUT,1020) NHOURS,NMINS,NRSECS
        ELSEIF (NMINS.GT.0) THEN
          WRITE(IOUT,1030) NMINS,NSECS,MSECS
        ELSE
          WRITE(IOUT,1040) NSECS,MSECS
        ENDIF
      ENDIF
C
      RETURN
      END
