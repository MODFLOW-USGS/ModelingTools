      MODULE OBSSTRMODULE
         INTEGER, SAVE, POINTER  ::NQST,NQCST,NQTST,IUSTOBSV
         INTEGER, SAVE, DIMENSION(:),   POINTER ::NQOBST
         INTEGER, SAVE, DIMENSION(:),   POINTER ::NQCLST
         INTEGER, SAVE, DIMENSION(:),   POINTER ::IOBTS
         REAL,    SAVE, DIMENSION(:),   POINTER ::FLWSIM
         REAL,    SAVE, DIMENSION(:),   POINTER ::FLWOBS
         REAL,    SAVE, DIMENSION(:),   POINTER ::TOFF
         REAL,    SAVE, DIMENSION(:),   POINTER ::OTIME
         REAL,    SAVE, DIMENSION(:,:), POINTER ::QCELL
         CHARACTER*12,SAVE,DIMENSION(:),POINTER ::OBSNAM
      TYPE OBSSTRTYPE
         INTEGER, POINTER  ::NQST,NQCST,NQTST,IUSTOBSV
         INTEGER,     DIMENSION(:),   POINTER ::NQOBST
         INTEGER,     DIMENSION(:),   POINTER ::NQCLST
         INTEGER,     DIMENSION(:),   POINTER ::IOBTS
         REAL,        DIMENSION(:),   POINTER ::FLWSIM
         REAL,        DIMENSION(:),   POINTER ::FLWOBS
         REAL,        DIMENSION(:),   POINTER ::TOFF
         REAL,        DIMENSION(:),   POINTER ::OTIME
         REAL,        DIMENSION(:,:), POINTER ::QCELL
         CHARACTER*12,DIMENSION(:),POINTER ::OBSNAM
      END TYPE
      TYPE(OBSSTRTYPE),  SAVE   ::OBSSTRDAT(10)
      END MODULE
C  NQST -- number of cell groups
C  NQCST -- total number of cells in all groups
C  NQTST -- total number of observations -- sum of the number of times for each group
C  NQOBST(NQST) -- The number of observations in each observation group
C  NQCLST(NQST) -- The number of cells in each observation group
C  IOBTS(NQTST) -- observation time step
C  FLWSIM(NQTST) -- Simulated value
C  FLWOBS(NQTST) -- observed value
C  TOFF(NQTST) -- Fractional offset between time steps
C  OTIME(NQTST) -- observation time in model time units
C  QCELL(4,NQCST) -- Data for each observation cell -- Segment, Reach, Unused,
C                    Proportion factor


      SUBROUTINE OBS2STR7AR(IUSTOB,IUST,IGRID)
C     ******************************************************************
C     ALLOCATE MEMORY AND READ FLOW OBSERVATIONS AT STREAM CELLS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,  ONLY:IOUT,NPER,NSTP,PERLEN,TSMULT,ISSFLG,ITRSS
      USE OBSSTRMODULE
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
      ALLOCATE(NQST,NQCST,NQTST,IUSTOBSV)
C
      ZERO=0.0
      IERR=0
C  NT is the observation counter.
      NT=0
C  NC is the cell counter.
      NC=0
C
C     IDENTIFY PROCESS AND PACKAGE
      WRITE(IOUT,*) 'STOB:'
!      WRITE(IOUT,7) IUSTOB
!    7 FORMAT(/,' OBS2STR7 -- OBSERVATION PROCESS (STREAMFLOW ',
!     &    'OBSERVATIONS)',/,' VERSION 2, 08/05/2009',/,
!     &    ' INPUT READ FROM UNIT ',I4)
C
Cx------Stop if GWFSTR is not active
      IF (IUST.EQ.0) THEN
        WRITE (IOUT,29 )
   29   FORMAT (/,' STREAMFLOW PACKAGE OF GWF IS NOT OPEN')
        CALL USTOP(' ')
        RETURN
      ENDIF
C
Cx------Read items 0 and 1.
      CALL URDCOM(IUSTOB,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQST,DUM,IOUT,IUSTOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQCST,DUM,IOUT,IUSTOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQTST,DUM,IOUT,IUSTOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUSTOBSV,DUM,IOUT,IUSTOB)
      WRITE (IOUT,*) 'NQST, NQCST, NQTST, IUSTOBSV:'
      WRITE (IOUT,*) NQST, NQCST, NQTST, IUSTOBSV
!      WRITE (IOUT,9) NQST, NQCST, NQTST
!    9 FORMAT (/,
!     &     ' NUMBER OF FLOW-OBSERVATION STREAM-CELL GROUPS.....: ',I6,/,
!     &     '   NUMBER OF CELLS IN STREAM-CELL GROUPS...........: ',I6,/,
!     &     '   NUMBER OF STREAM-CELL FLOWS.....................: ',I6)
      IF(NQTST.LE.0) THEN
         WRITE(IOUT,*) ' NUMBER OF OBSERVATIONS LESS THAN OR EQUAL TO 0'
         CALL USTOP(' ')
      END IF
!      IF(IUSTOBSV.GT.0) THEN
!         WRITE(IOUT,21) IUSTOBSV
!   21    FORMAT(1X,
!     1      'STREAM OBSERVATIONS WILL BE SAVED ON UNIT.........:',I7)
!      ELSE
!         WRITE(IOUT,22)
!   22    FORMAT(1X,'STREAM OBSERVATIONS WILL NOT BE SAVED IN A FILE')
!      END IF
C
Cx------Allocate memory
      ALLOCATE(NQOBST(NQST))
      ALLOCATE(NQCLST(NQST))
      ALLOCATE(IOBTS(NQTST))
      ALLOCATE(FLWSIM(NQTST))
      ALLOCATE(FLWOBS(NQTST))
      ALLOCATE(TOFF(NQTST))
      ALLOCATE(OTIME(NQTST))
      ALLOCATE(QCELL(4,NQCST))
      ALLOCATE(OBSNAM(NQTST))
C
Cx------Initialize simulated equivalents
      DO 15 N=1,NQTST
      FLWSIM(N)=ZERO
   15 CONTINUE
C
Cx------READ AND WRITE TIME-OFFSET MULTIPLIER FOR FLOW-OBSERVATION TIMES.
      READ(IUSTOB,*) TOMULTST
      WRITE (IOUT,*) 'TOMULTST:'
      WRITE (IOUT,*) TOMULTST
!      WRITE (IOUT,20) TOMULTST
!   20 FORMAT (/,' OBSERVED STREAM-CELL FLOW DATA',/,' -- TIME OFFSETS',
!     &        ' ARE MULTIPLIED BY: ',G12.5)
C
Cx------LOOP THROUGH CELL GROUPS.
      DO 200 IQ = 1,NQST
C
Cx------READ NUMBER OF OBSERVATINS AND NUMBER OF CELLS FOR ONE GROUP
Cx------(ITEM 3).
        READ (IUSTOB,*) NQOBST(IQ), NQCLST(IQ)
        WRITE (IOUT,*) 'NQOBST(IQ), NQCLST(IQ):'
        WRITE (IOUT,*) NQOBST(IQ), NQCLST(IQ)
!        WRITE (IOUT,25) IQ, 'STR', NQCLST(IQ), NQOBST(IQ)
!   25   FORMAT (/,'   GROUP NUMBER: ',I6,'   BOUNDARY TYPE: ',A,
!     &  '   NUMBER OF CELLS IN GROUP: ',I6,/,
!     &  '   NUMBER OF FLOW OBSERVATIONS: ',I6,//,
!     &  40X,'OBSERVED',/,
!     &  20X,'REFER.',13X,'STREAMFLOW',/,
!     &  7X,'OBSERVATION',2X,'STRESS',4X,'TIME',5X,'GAIN (-) OR',14X,/,
!     &  2X,'OBS#    NAME',6X,'PERIOD   OFFSET',5X,'LOSS (+)')
C
Cx------SET FLAG FOR SETTING ALL PORTION FACTORS TO 1
        IFCTFLG = 0
        IF (NQCLST(IQ).LT.0) THEN
          IFCTFLG = 1
          NQCLST(IQ) = -NQCLST(IQ)
        ENDIF
C
C
Cx------READ THE OBSERVATION NAMES, TIMES, AND MEASURED VALUES FOR
Cx------ONE CELL GROUP (ITEM 4)
        NT1 = NT + 1
        NT2 = NT + NQOBST(IQ)
        DO 30 J = NT1, NT2
          READ (IUSTOB,*) OBSNAM(J), IREFSP, TOFFSET, FLWOBS(J)
          WRITE(IOUT,* ) 'OBSNAM(J),IREFSP,TOFFSET,FLWOBS(J):'
          WRITE(IOUT,* ) OBSNAM(J)
          WRITE(IOUT,* ) IREFSP,TOFFSET,FLWOBS(J)
!          WRITE(IOUT,27 ) J,OBSNAM(J),IREFSP,TOFFSET,FLWOBS(J)
!   27     FORMAT (I6,1X,A12,2X,I4,2X,G11.4,1X,G11.4)
!          CALL UOBSTI(OBSNAM(J),IOUT,ISSFLG,ITRSS,NPER,NSTP,IREFSP,
!     &                IOBTS(J),PERLEN,TOFF(J),TOFFSET,TOMULTST,TSMULT,1,
!     &                OTIME(J))
   30   CONTINUE
C
Cx------READ SEGMENT, REACH, AND FACTOR (ITEM 5) FOR EACH CELL IN
Cx------THE CELL GROUP.
        NC1 = NC + 1
        NC2 = NC + NQCLST(IQ)
!        WRITE (IOUT,54)
!   54   FORMAT (/,'     SEGMENT  REACH    FACTOR')
        DO 100 L = NC1, NC2
          READ (IUSTOB,*) (QCELL(I,L),I=1,2),QCELL(4,L)
          WRITE (IOUT,*) '(QCELL(I,L),I=1,2),QCELL(4,L):'
          WRITE (IOUT,*) (QCELL(I,L),I=1,2),QCELL(4,L)
          IF(IFCTFLG.EQ.1) QCELL(4,L) = 1.
!          WRITE (IOUT,55) (QCELL(I,L),I=1,2),QCELL(4,L)
!   55     FORMAT (4X,F8.0,F6.0,F9.2)
          QCELL(3,L)=0.0
  100   CONTINUE
C
Cx------END OF INPUT FOR ONE CELL GROUP -- UPDATE COUNTERS.
        NC = NC2
        NT = NT2
  200 CONTINUE
C
C
      IF (IERR.GT.0) THEN
        WRITE(IOUT,620)
  620 FORMAT (/,1X,'ERROR: SEARCH ABOVE FOR ERROR MESSAGE(S)',/,
     &' -- STOP EXECUTION (OBS2STR7)')
        CALL USTOP(' ')
      ENDIF
C
Cx------RETURN.
      CALL SOBS2STR7PSV(IGRID)
      RETURN
      END
!      SUBROUTINE OBS2STR7SE(IGRID)
C     ******************************************************************
C     CALCULATE SIMULATED EQUIVALENTS TO OBSERVED FLOWS FOR THE STREAM
C     PACKAGE
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
!      SUBROUTINE OBS2STR7OT(IGRID)
C     ******************************************************************
C     WRITE ALL OBSERVATIONS TO LISTING FILE.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      SUBROUTINE OBS2STR7DA(IGRID)
C  Deallocate OBSSTR memory
      USE OBSSTRMODULE
C
      CALL SOBS2STR7PNT(IGRID)
      DEALLOCATE(NQST)
      DEALLOCATE(NQCST)
      DEALLOCATE(NQTST)
      DEALLOCATE(IUSTOBSV)
      DEALLOCATE(NQOBST)
      DEALLOCATE(NQCLST)
      DEALLOCATE(IOBTS)
      DEALLOCATE(FLWSIM)
      DEALLOCATE(FLWOBS)
      DEALLOCATE(TOFF)
      DEALLOCATE(OTIME)
      DEALLOCATE(QCELL)
      DEALLOCATE(OBSNAM)
C
      RETURN
      END
      SUBROUTINE SOBS2STR7PNT(IGRID)
C  Change OBSSTR data to a different grid.
      USE OBSSTRMODULE
C
      NQST=>OBSSTRDAT(IGRID)%NQST
      NQCST=>OBSSTRDAT(IGRID)%NQCST
      NQTST=>OBSSTRDAT(IGRID)%NQTST
      IUSTOBSV=>OBSSTRDAT(IGRID)%IUSTOBSV
      NQOBST=>OBSSTRDAT(IGRID)%NQOBST
      NQCLST=>OBSSTRDAT(IGRID)%NQCLST
      IOBTS=>OBSSTRDAT(IGRID)%IOBTS
      FLWSIM=>OBSSTRDAT(IGRID)%FLWSIM
      FLWOBS=>OBSSTRDAT(IGRID)%FLWOBS
      TOFF=>OBSSTRDAT(IGRID)%TOFF
      OTIME=>OBSSTRDAT(IGRID)%OTIME
      QCELL=>OBSSTRDAT(IGRID)%QCELL
      OBSNAM=>OBSSTRDAT(IGRID)%OBSNAM
C
      RETURN
      END
      SUBROUTINE SOBS2STR7PSV(IGRID)
C  Save OBSSTR data for a grid.
      USE OBSSTRMODULE
C
      OBSSTRDAT(IGRID)%NQST=>NQST
      OBSSTRDAT(IGRID)%NQCST=>NQCST
      OBSSTRDAT(IGRID)%NQTST=>NQTST
      OBSSTRDAT(IGRID)%IUSTOBSV=>IUSTOBSV
      OBSSTRDAT(IGRID)%NQOBST=>NQOBST
      OBSSTRDAT(IGRID)%NQCLST=>NQCLST
      OBSSTRDAT(IGRID)%IOBTS=>IOBTS
      OBSSTRDAT(IGRID)%FLWSIM=>FLWSIM
      OBSSTRDAT(IGRID)%FLWOBS=>FLWOBS
      OBSSTRDAT(IGRID)%TOFF=>TOFF
      OBSSTRDAT(IGRID)%OTIME=>OTIME
      OBSSTRDAT(IGRID)%QCELL=>QCELL
      OBSSTRDAT(IGRID)%OBSNAM=>OBSNAM
C
      RETURN
      END
