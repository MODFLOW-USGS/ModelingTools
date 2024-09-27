      MODULE GLOBAL
        PARAMETER(NIUNIT=100)
        INTEGER, SAVE, POINTER    ::NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD
        INTEGER, SAVE, POINTER    ::ITMUNI,LENUNI,IXSEC,ITRSS,INBAS
        INTEGER, SAVE, POINTER    ::IFREFM,NODES,IOUT,MXITER
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::IUNIT(:)
        DOUBLE PRECISION, SAVE, DIMENSION(:,:,:), POINTER ::HNEW
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::LBOTM
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::LAYCBD
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::LAYHDT
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::LAYHDS
        REAL,    SAVE,    DIMENSION(:),     POINTER ::PERLEN
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::NSTP
        REAL,    SAVE,    DIMENSION(:),     POINTER ::TSMULT
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::ISSFLG
        REAL,    SAVE,    DIMENSION(:),     POINTER ::DELR
        REAL,    SAVE,    DIMENSION(:),     POINTER ::DELC
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::BOTM
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::HOLD
        INTEGER, SAVE,    DIMENSION(:,:,:), POINTER ::IBOUND
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::CR
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::CC
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::CV
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::HCOF
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::RHS
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::BUFF
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::STRT
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::DDREF
      TYPE GLOBALTYPE
        INTEGER,POINTER    :: NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD
        INTEGER,POINTER    :: ITMUNI,LENUNI,IXSEC,ITRSS,INBAS
        INTEGER,POINTER    :: IFREFM,NODES,IOUT,MXITER
        INTEGER,    DIMENSION(:),     POINTER ::IUNIT
        DOUBLE PRECISION, DIMENSION(:,:,:), POINTER ::HNEW
        INTEGER,    DIMENSION(:),     POINTER ::LBOTM
        INTEGER,    DIMENSION(:),     POINTER ::LAYCBD
        INTEGER,    DIMENSION(:),     POINTER ::LAYHDT
        INTEGER,    DIMENSION(:),     POINTER ::LAYHDS
        REAL,       DIMENSION(:),     POINTER ::PERLEN
        INTEGER,    DIMENSION(:),     POINTER ::NSTP
        REAL,       DIMENSION(:),     POINTER ::TSMULT
        INTEGER,    DIMENSION(:),     POINTER ::ISSFLG
        REAL,       DIMENSION(:),     POINTER ::DELR
        REAL,       DIMENSION(:),     POINTER ::DELC
        REAL,       DIMENSION(:,:,:), POINTER ::BOTM
        REAL,       DIMENSION(:,:,:), POINTER ::HOLD
        INTEGER,    DIMENSION(:,:,:), POINTER ::IBOUND
        REAL,       DIMENSION(:,:,:), POINTER ::CR
        REAL,       DIMENSION(:,:,:), POINTER ::CC
        REAL,       DIMENSION(:,:,:), POINTER ::CV
        REAL,       DIMENSION(:,:,:), POINTER ::HCOF
        REAL,       DIMENSION(:,:,:), POINTER ::RHS
        REAL,       DIMENSION(:,:,:), POINTER ::BUFF
        REAL,       DIMENSION(:,:,:), POINTER ::STRT
        REAL,       DIMENSION(:,:,:), POINTER ::DDREF
      END TYPE GLOBALTYPE
      TYPE(GLOBALTYPE),SAVE ::GLOBALDAT(10)
      END MODULE GLOBAL
      MODULE PARAMMODULE
C  Data definitions for Named Parameters
C  Explicitly declare all variables to enable subroutines that include
C  this file to use the IMPLICIT NONE statement.
        PARAMETER (MXPAR=2000,MXCLST=20000,MXINST=50000)
        INTEGER,SAVE,POINTER ::ICLSUM,IPSUM,INAMLOC,NMLTAR,NZONAR,NPVAL
        REAL,          SAVE,    DIMENSION(:),    POINTER ::B
        INTEGER,       SAVE,    DIMENSION(:),    POINTER ::IACTIVE
        INTEGER,       SAVE,    DIMENSION(:,:),  POINTER ::IPLOC
        INTEGER,       SAVE,    DIMENSION(:,:),  POINTER ::IPCLST
        INTEGER,       SAVE,    DIMENSION(:,:,:),POINTER ::IZON
        REAL,          SAVE,    DIMENSION(:,:,:),POINTER ::RMLT
        CHARACTER(LEN=10),SAVE, DIMENSION(:),    POINTER ::PARNAM
        CHARACTER(LEN=4), SAVE, DIMENSION(:),    POINTER ::PARTYP
        CHARACTER(LEN=10),SAVE, DIMENSION(:),    POINTER ::ZONNAM
        CHARACTER(LEN=10),SAVE, DIMENSION(:),    POINTER ::MLTNAM
        CHARACTER(LEN=10),SAVE, DIMENSION(:),    POINTER ::INAME
      TYPE PARAMTYPE
        INTEGER,POINTER  ::ICLSUM,IPSUM,INAMLOC,NMLTAR,NZONAR,NPVAL
        REAL,              DIMENSION(:),    POINTER ::B
        INTEGER,           DIMENSION(:),    POINTER ::IACTIVE
        INTEGER,           DIMENSION(:,:),  POINTER ::IPLOC
        INTEGER,           DIMENSION(:,:),  POINTER ::IPCLST
        INTEGER,           DIMENSION(:,:,:),POINTER ::IZON
        REAL,              DIMENSION(:,:,:),POINTER ::RMLT
        CHARACTER(LEN=10), DIMENSION(:),    POINTER ::PARNAM
        CHARACTER(LEN=4),  DIMENSION(:),    POINTER ::PARTYP
        CHARACTER(LEN=10), DIMENSION(:),    POINTER ::ZONNAM
        CHARACTER(LEN=10), DIMENSION(:),    POINTER ::MLTNAM
        CHARACTER(LEN=10), DIMENSION(:),    POINTER ::INAME
      END TYPE
      TYPE(PARAMTYPE), SAVE  ::PARAMDAT(10)
      END MODULE PARAMMODULE
      MODULE GWFBASMODULE
        INTEGER, SAVE, POINTER  ::MSUM
        INTEGER, SAVE, POINTER  ::IHEDFM,IHEDUN,IDDNFM,IDDNUN,IBOUUN
        INTEGER, SAVE, POINTER  ::LBHDSV,LBDDSV,LBBOSV
        INTEGER, SAVE, POINTER  ::IBUDFL,ICBCFL,IHDDFL,IAUXSV,IBDOPT
        INTEGER, SAVE, POINTER  ::IPRTIM,IPEROC,ITSOC,ICHFLG
        INTEGER, SAVE, POINTER  ::IDDREF,IDDREFNEW
        REAL,    SAVE, POINTER  ::DELT,PERTIM,TOTIM,HNOFLO,HDRY,STOPER
        CHARACTER(LEN=20), SAVE, POINTER   ::CHEDFM,CDDNFM,CBOUFM
!        INTEGER,           SAVE, DIMENSION(:,:), POINTER ::IOFLG
!        REAL,              SAVE, DIMENSION(:,:), POINTER ::VBVL
!        CHARACTER(LEN=16), SAVE, DIMENSION(:),   POINTER ::VBNM
      TYPE GWFBASTYPE
        INTEGER, POINTER  ::MSUM
        INTEGER, POINTER  ::IHEDFM,IHEDUN,IDDNFM,IDDNUN,IBOUUN
        INTEGER, POINTER  ::LBHDSV,LBDDSV,LBBOSV
        INTEGER, POINTER  ::IBUDFL,ICBCFL,IHDDFL,IAUXSV,IBDOPT
        INTEGER, POINTER  ::IPRTIM,IPEROC,ITSOC,ICHFLG
        INTEGER, POINTER  ::IDDREF,IDDREFNEW
        REAL,    POINTER  ::DELT,PERTIM,TOTIM,HNOFLO,HDRY,STOPER
        CHARACTER(LEN=20), POINTER   ::CHEDFM,CDDNFM,CBOUFM
!        INTEGER,           DIMENSION(:,:), POINTER ::IOFLG
!        REAL,              DIMENSION(:,:), POINTER ::VBVL
!        CHARACTER(LEN=16), DIMENSION(:),   POINTER ::VBNM
      END TYPE
      TYPE(GWFBASTYPE), SAVE  ::GWFBASDAT(10)
      END MODULE GWFBASMODULE


      SUBROUTINE GWF2BAS7AR(INUNIT,CUNIT,VERSION,IUDIS,IUZON,IUMLT,
     2              MAXUNIT,IGRID,IUOC,HEADNG,IUPVAL,MFVNAM)
C     ******************************************************************
C     Allocate and Read for GWF Basic Package
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,ITMUNI,
     1                     LENUNI,IXSEC,ITRSS,INBAS,IFREFM,NODES,IOUT,
     2                     MXITER,IUNIT,NIUNIT,HNEW,LBOTM,LAYCBD,LAYHDT,
     3                     LAYHDS,PERLEN,NSTP,TSMULT,ISSFLG,DELR,DELC,
     4                     BOTM,HOLD,IBOUND,CR,CC,CV,HCOF,RHS,BUFF,STRT,
     5                     DDREF
      USE PARAMMODULE,ONLY:MXPAR,MXCLST,MXINST,ICLSUM,IPSUM,
     1                     INAMLOC,NMLTAR,NZONAR,NPVAL,
     2                     B,IACTIVE,IPLOC,IPCLST,PARNAM,PARTYP,
     3                     ZONNAM,MLTNAM,INAME
      USE GWFBASMODULE,ONLY:MSUM,IHEDFM,IHEDUN,IDDNFM,IDDNUN,IBOUUN,
     1                      LBHDSV,LBDDSV,LBBOSV,IBUDFL,ICBCFL,IHDDFL,
     2                      IAUXSV,IBDOPT,IPRTIM,IPEROC,ITSOC,ICHFLG,
     3                      IDDREF,IDDREFNEW,DELT,PERTIM,TOTIM,HNOFLO,
     4                      HDRY,STOPER,CHEDFM,CDDNFM,CBOUFM
!     4                      HDRY,STOPER,CHEDFM,CDDNFM,CBOUFM,VBVL,VBNM
C
      CHARACTER*4 CUNIT(NIUNIT)
      CHARACTER*(*) VERSION
      CHARACTER*80 HEADNG(2)
      CHARACTER*(*) MFVNAM
      CHARACTER*200 LINE
      logical Owhm2
C
      DOUBLE PRECISION HNF
      CHARACTER*24 ANAME(2)
      DATA ANAME(1) /'          BOUNDARY ARRAY'/
      DATA ANAME(2) /'            INITIAL HEAD'/
C     ------------------------------------------------------------------
C1------Allocate scalar variables, which makes it possible for multiple
C1------grids to be defined.
      ALLOCATE(NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,ITMUNI,LENUNI,ITRSS)
      ALLOCATE(IXSEC,INBAS,IFREFM,NODES,IOUT,MXITER)
      MXITER=1
      ALLOCATE(IUNIT(NIUNIT))
C
      ALLOCATE(ICLSUM,IPSUM,INAMLOC,NMLTAR,NZONAR,NPVAL)
      ALLOCATE (B(MXPAR))
      ALLOCATE (IACTIVE(MXPAR))
      ALLOCATE (IPLOC(4,MXPAR))
      ALLOCATE (IPCLST(14,MXCLST))
      ALLOCATE (PARNAM(MXPAR))
      ALLOCATE (PARTYP(MXPAR))
      ALLOCATE (INAME(MXINST))
C
      ALLOCATE(MSUM,IHEDFM,IHEDUN,IDDNFM,IDDNUN,IBOUUN,LBHDSV,LBDDSV,
     1         LBBOSV)
      ALLOCATE(IBUDFL,ICBCFL,IHDDFL,IAUXSV,IBDOPT,IPRTIM,IPEROC,ITSOC,
     1         ICHFLG,IDDREF,IDDREFNEW)
      ALLOCATE(DELT,PERTIM,TOTIM,HNOFLO,HDRY,STOPER)
      ALLOCATE(CHEDFM,CDDNFM,CBOUFM)
      HDRY=1.E30
	STOPER = 0.0;
C
C2------Open all files in name file.
      CALL SGWF2BAS7OPEN(INUNIT,IOUT,IUNIT,CUNIT,NIUNIT,
     &                 VERSION,INBAS,MAXUNIT,MFVNAM)
C
C3------PRINT A MESSAGE IDENTIFYING THE BASIC PACKAGE.
      WRITE(IOUT,1)INBAS
    1 FORMAT(1X,/1X,'BAS -- BASIC PACKAGE, VERSION 7, 5/2/2005',
     2' INPUT READ FROM UNIT ',I4)
C
C4------Initialize parameter definition variables.
      IPSUM=0
      ICLSUM=0
      INAMLOC=1
      DO 10 N=1,MXPAR
        PARNAM(N)=' '
        PARTYP(N)=' '
        IPLOC(1,N)=0
        IPLOC(2,N)=0
        IACTIVE(N)=0
   10 CONTINUE
C
C5------Allocate and read discretization data.
      CALL SGWF2BAS7ARDIS(IUDIS,IOUT)
      NODES=NCOL*NROW*NLAY
C
C6------Allocate space for global arrays except discretization data.
      ALLOCATE (HNEW(NCOL,NROW,NLAY))
      ALLOCATE (HOLD(NCOL,NROW,NLAY))
      ALLOCATE (IBOUND(NCOL,NROW,NLAY))
      ALLOCATE (CR(NCOL,NROW,NLAY))
      ALLOCATE (CC(NCOL,NROW,NLAY))
      ALLOCATE (CV(NCOL,NROW,NLAY))
      ALLOCATE (HCOF(NCOL,NROW,NLAY))
      ALLOCATE (RHS(NCOL,NROW,NLAY))
      ALLOCATE (BUFF(NCOL,NROW,NLAY))
      ALLOCATE (STRT(NCOL,NROW,NLAY))
      DDREF=>STRT
      ALLOCATE (LAYHDT(NLAY))
      ALLOCATE (LAYHDS(NLAY))
C
C7------Initialize head-dependent thickness indicator to code that
C7------indicates layer is undefined.
      DO 100 I=1,NLAY
        LAYHDT(I)=-1
        LAYHDS(I)=-1
  100 CONTINUE
!      WRITE(IOUT,'(//)')
C
C8------Read BAS Package file.
      WRITE(IOUT, *) 'BAS:'
C8A-----READ AND PRINT COMMENTS.  SAVE THE FIRST TWO COMMENTS IN HEADNG.
      HEADNG(1)=' '
      HEADNG(2)=' '
      WRITE(IOUT,*)
      READ(INBAS,'(A)') LINE
      IF(LINE(1:1).NE.'#') GO TO 20
      HEADNG(1)=LINE(1:80)
!      WRITE(IOUT,'(1X,A)') HEADNG(1)
      READ(INBAS,'(A)') LINE
      IF(LINE(1:1).NE.'#') GO TO 20
      HEADNG(2)=LINE(1:80)
!      WRITE(IOUT,'(1X,A)') HEADNG(2)
      CALL URDCOM(INBAS,IOUT,LINE)
C
C8B-----LOOK FOR OPTIONS IN THE FIRST ITEM AFTER THE HEADING.
   20 CONTINUE
      WRITE(IOUT, *) 'HEADNG(1):'
      WRITE(IOUT, *) HEADNG(1)
      WRITE(IOUT, *) 'HEADNG(2):'
      WRITE(IOUT, *) HEADNG(2)
      IXSEC=0
      ICHFLG=0
      IFREFM=0
      IPRTIM=0
	STOPER=0.0
      LLOC=1
      Owhm2 = .False.
   25 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INBAS)
      IF (LINE(ISTART:ISTOP).EQ.'BEGIN') THEN
         CALL URDCOM(INBAS,IOUT,LINE)
         LLOC=1
         Owhm2 = .True.
         IFREFM=1
         GOTO 25
      ELSE IF(LINE(ISTART:ISTOP).EQ.'END') THEN
        CONTINUE
      ELSE IF(LINE(ISTART:ISTOP).EQ.'XSECTION') THEN
         IXSEC=1
         If (Owhm2) then
           CALL URDCOM(INBAS,IOUT,LINE)
           LLOC=1
           GOTO 25
         ENDIF
      ELSE IF(LINE(ISTART:ISTOP).EQ.'CHTOCH') THEN
         ICHFLG=1
         If (Owhm2) then
           CALL URDCOM(INBAS,IOUT,LINE)
           LLOC=1
           GOTO 25
         ENDIF
      ELSE IF(LINE(ISTART:ISTOP).EQ.'FREE') THEN
         IFREFM=1
         If (Owhm2) then
           CALL URDCOM(INBAS,IOUT,LINE)
           LLOC=1
           GOTO 25
         ENDIF
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOFREE') THEN
         IFREFM=0
         If (Owhm2) then
           CALL URDCOM(INBAS,IOUT,LINE)
           LLOC=1
           GOTO 25
         ENDIF
!         WRITE(IOUT,26)
!   26    FORMAT (1X,'THE FREE FORMAT OPTION HAS BEEN SELECTED')
      ELSEIF(LINE(ISTART:ISTOP).EQ.'PRINTTIME') THEN
         IPRTIM=1
         If (Owhm2) then
           CALL URDCOM(INBAS,IOUT,LINE)
           LLOC=1
           GOTO 25
         ENDIF
!         WRITE(IOUT,7)
!    7    FORMAT(1X,'THE PRINTTIME OPTION HAS BEEN SELECTED')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'STOPERROR') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,STOPER,IOUT,INBAS)
         If (Owhm2) then
           CALL URDCOM(INBAS,IOUT,LINE)
           LLOC=1
           GOTO 25
         ENDIF
      ELSE
         CALL URDCOM(INBAS,IOUT,LINE)
         LLOC=1
         GOTO 25
!         WRITE(IOUT,*) STOPER
!    8    FORMAT(1X,'When solver convergence criteria are not met,',/
!     1    1X,'execution will continue unless the budget percent',/
!     2    1X,'discrepancy is greater than:',F10.4)
      END IF
      IF(LLOC.LT.200) GO TO 25
      WRITE(IOUT, *) 'IXSEC, ICHFLG, IFREFM, IPRTIM, STOPER:'
      WRITE(IOUT, *) IXSEC, ICHFLG, IFREFM, IPRTIM, STOPER
C
C8C-----PRINT A MESSAGE SHOWING OPTIONS.
!      IF(IXSEC.NE.0) WRITE(IOUT,61)
!   61 FORMAT(1X,'CROSS SECTION OPTION IS SPECIFIED')
!      IF(ICHFLG.NE.0) WRITE(IOUT,62)
!   62 FORMAT(1X,'CALCULATE FLOW BETWEEN ADJACENT CONSTANT-HEAD CELLS')
C
C8D-----INITIALIZE TOTAL ELAPSED TIME COUNTER STORAGE ARRAY COUNTER
C8D-----AND CALCULATE NUMBER OF CELLS.
      TOTIM=0.
C
C8E-----READ BOUNDARY ARRAY(IBOUND).
      IF(IXSEC.EQ.0) THEN
         DO 280 K=1,NLAY
         KK=K
         CALL U2DINT(IBOUND(:,:,KK),ANAME(1),NROW,NCOL,KK,INBAS,IOUT)
  280    CONTINUE
      ELSE
         CALL U2DINT(IBOUND(:,:,1),ANAME(1),NLAY,NCOL,-1,INBAS,IOUT)
      END IF
C
C8F-----READ AND PRINT HEAD VALUE TO BE PRINTED FOR NO-FLOW CELLS.
      IF(IFREFM.EQ.0) THEN
         READ(INBAS,'(F10.0)') HNOFLO
      ELSE
         READ(INBAS,*) HNOFLO
      END IF
      HNF=HNOFLO
      WRITE(IOUT,*) 'HNOFLO:'
      WRITE(IOUT,*) HNOFLO
!      WRITE(IOUT,3) HNOFLO
!    3 FORMAT(1X,/1X,'AQUIFER HEAD WILL BE SET TO ',1PG11.5,
!     1       ' AT ALL NO-FLOW NODES (IBOUND=0).')
C
C8G-----READ INITIAL HEADS.
      IF(IXSEC.EQ.0) THEN
         DO 300 K=1,NLAY
         KK=K
         CALL U2DREL(STRT(:,:,KK),ANAME(2),NROW,NCOL,KK,INBAS,IOUT)
  300    CONTINUE
      ELSE
         CALL U2DREL(STRT(:,:,1),ANAME(2),NLAY,NCOL,-1,INBAS,IOUT)
      END IF
C
C9------COPY INITIAL HEADS FROM STRT TO HNEW.
      DO 400 K=1,NLAY
      DO 400 I=1,NROW
      DO 400 J=1,NCOL
      HNEW(J,I,K)=STRT(J,I,K)
      IF(IBOUND(J,I,K).EQ.0) HNEW(J,I,K)=HNF
  400 CONTINUE
C
C10-----SET UP OUTPUT CONTROL.
!      CALL SGWF2BAS7I(NLAY,IUNIT(IUOC),IOUT,IFREFM,NIUNIT)
C
C11-----INITIALIZE VOLUMETRIC BUDGET ACCUMULATORS TO ZERO.
  590 ZERO=0.
!      DO 600 I=1,NIUNIT
!      DO 600 J=1,4
!      VBVL(J,I)=ZERO
!  600 CONTINUE
C
C12-----Allocate and read Zone and Multiplier arrays
      CALL SGWF2BAS7ARMZ(IUNIT(IUZON),IUNIT(IUMLT))
C
C13-----READ PARAMETER VALUES FILE.
      CALL SGWF2BAS7ARPVAL(IUPVAL)
C
C14-----SAVE POINTERS TO DATA AND RETURN.
      CALL SGWF2BAS7PSV(IGRID)
      RETURN
      END
      SUBROUTINE GWF2BAS7ST(KPER,IGRID)
C     ******************************************************************
C     SETUP TIME VARIABLES FOR NEW TIME PERIOD
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,PERLEN,NSTP,TSMULT
      USE GWFBASMODULE,ONLY:DELT,PERTIM
C     ------------------------------------------------------------------
      CALL SGWF2BAS7PNT(IGRID)
C
C1------WRITE STRESS PERIOD INFORMATION
!      WRITE (IOUT,1) KPER,PERLEN(KPER),NSTP(KPER),TSMULT(KPER)
!    1 FORMAT('1',/28X,'STRESS PERIOD NO. ',I4,', LENGTH =',G15.7,/
!     1            28X,47('-'),//
!     2            30X,'NUMBER OF TIME STEPS =',I6,//
!     3            31X,'MULTIPLIER FOR DELT =',F10.3)
C
C2------CALCULATE THE LENGTH OF THE FIRST TIME STEP.
C
C2A-----ASSUME TIME STEP MULTIPLIER IS EQUAL TO ONE.
      DELT=PERLEN(KPER)/FLOAT(NSTP(KPER))
C
C2B-----IF TIME STEP MULTIPLIER IS NOT ONE THEN CALCULATE FIRST
C2B-----TERM OF GEOMETRIC PROGRESSION.
      ONE=1.
      IF(TSMULT(KPER).NE.ONE)
     1    DELT=PERLEN(KPER)*(ONE-TSMULT(KPER))/
     2        (ONE-TSMULT(KPER)**NSTP(KPER))
C
C3------PRINT THE LENGTH OF THE FIRST TIME STEP.
!      WRITE (IOUT,9) DELT
!    9 FORMAT(1X,/28X,'INITIAL TIME STEP SIZE =',G15.7)
C
C4------INITIALIZE PERTIM (ELAPSED TIME WITHIN STRESS PERIOD).
      PERTIM=0.
C
C5------CHECK THAT ALL PARAMETERS IN PARAMETER VALUE FILE HAVE BEEN DEFINED.
      IF(KPER.GT.1) CALL SGWF2BAS7STPVAL()
C
C6------RETURN
      RETURN
      END
      SUBROUTINE GWF2BAS7AD(KPER,KSTP,IGRID)
C     ******************************************************************
C     ADVANCE TO NEXT TIME STEP
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,TSMULT,HNEW,HOLD
      USE GWFBASMODULE,ONLY:DELT,TOTIM,PERTIM
C     ------------------------------------------------------------------
      CALL SGWF2BAS7PNT(IGRID)
C
C1------IF NOT FIRST TIME STEP THEN CALCULATE TIME STEP LENGTH.
      IF(KSTP.NE.1) DELT=TSMULT(KPER)*DELT
C
C2------ACCUMULATE ELAPSED TIME IN SIMULATION(TOTIM) AND IN THIS
C2------STRESS PERIOD(PERTIM).
      TOTIM=TOTIM+DELT
      PERTIM=PERTIM+DELT
C
C3------COPY HNEW TO HOLD.
      DO 10 K=1,NLAY
      DO 10 I=1,NROW
      DO 10 J=1,NCOL
   10 HOLD(J,I,K)=HNEW(J,I,K)
C
C4------RETURN
      RETURN
      END
!      SUBROUTINE GWF2BAS7FM(IGRID)
C     ******************************************************************
C     SET HCOF=RHS=0.
C     ******************************************************************
!      SUBROUTINE GWF2BAS7OC(KSTP,KPER,ICNVG,INOC,IGRID)
C     ******************************************************************
C     OUTPUT CONTROLLER FOR HEAD, DRAWDOWN, AND BUDGET
C     ******************************************************************
!      SUBROUTINE GWF2BAS7OT(KSTP,KPER,ICNVG,ISA,IGRID)
C     ******************************************************************
C     OUTPUT TIME, VOLUMETRIC BUDGET, HEAD, AND DRAWDOWN
      SUBROUTINE SGWF2BAS7ARDIS(IUDIS,IOUT)
C     *****************************************************************
C     ALLOCATE AND READ DIS DATA
C     *****************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,ITMUNI,
     1                     LENUNI,IUNIT,LBOTM,LAYCBD,ITRSS,
     3                     PERLEN,NSTP,TSMULT,ISSFLG,DELR,DELC,BOTM
C
      CHARACTER*200 LINE
      CHARACTER*24 ANAME(5)
      integer temp_start
      integer temp_stop
      DATA ANAME(1) /'                    DELR'/
      DATA ANAME(2) /'                    DELC'/
      DATA ANAME(3) /'TOP ELEVATION OF LAYER 1'/
      DATA ANAME(4) /'  MODEL LAYER BOTTOM EL.'/
      DATA ANAME(5) /'BOT. EL. OF QUASI-3D BED'/
C     ------------------------------------------------------------------
C
C1------Check for existence of discretization file
      INDIS=IUNIT(IUDIS)
      IF(INDIS.LE.0) THEN
         WRITE(IOUT,*) ' DIS file must be specified for MODFLOW to run'
         CALL USTOP(' ')
      END IF
      WRITE(IOUT,11) INDIS
   11 FORMAT(1X,/1X,'DISCRETIZATION INPUT DATA READ FROM UNIT ',I4)
C
C
C2------Read comments and the first line following the comments.
      CALL URDCOM(INDIS,IOUT,LINE)
C
C3------Get the number of layers, rows, columns, stress periods,
C3------ITMUNI, and LENUNI from the line.
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NLAY,R,IOUT,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NROW,R,IOUT,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NCOL,R,IOUT,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPER,R,IOUT,INDIS)
      temp_start = ISTART
      temp_stop = ISTOP
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,ITMUNI,R,IOUT,INDIS)
      SELECT CASE(LINE(ISTART:ISTOP))
      CASE('SECOND','SECONDS'); ITMUNI = ONE
      CASE('MINUTE','MINUTES'); ITMUNI = TWO
      CASE('HOUR'  ,'HOURS'  ); ITMUNI = THREE
      CASE('DAY'   ,'DAYS'   ); ITMUNI = FOUR
      CASE('YEAR'  ,'YEARS'  ); ITMUNI = FIVE
      CASE DEFAULT
        ISTART = temp_start
        ISTOP = temp_stop
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMUNI,R,IOUT,INDIS)
      END SELECT
      temp_start = ISTART
      temp_stop = ISTOP
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,LENUNI,R,IOUT,INDIS)
      SELECT CASE(LINE(ISTART:ISTOP))
      CASE('FOOT'      ,'FEET'       ); LENUNI = ONE
      CASE('METER'     ,'METERS'     ); LENUNI = TWO
      CASE('CENTIMETER','CENTIMETERS'); LENUNI = THREE
      CASE DEFAULT
        ISTART = temp_start
        ISTOP = temp_stop
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,LENUNI,R,IOUT,INDIS)
      END SELECT
C
C4------PRINT # OF LAYERS, ROWS, COLUMNS AND STRESS PERIODS.
      WRITE(IOUT, *) 'DIS:'
      WRITE(IOUT, *) 'NLAY, NROW, NCOL, NPER, ITMUNI, LENUNI:'
      WRITE(IOUT, *) NLAY, NROW, NCOL, NPER, ITMUNI, LENUNI
!      WRITE(IOUT,15) NLAY,NROW,NCOL
!   15 FORMAT(1X,I4,' LAYERS',I10,' ROWS',I10,' COLUMNS')
!      WRITE(IOUT,20) NPER
!   20 FORMAT(1X,I4,' STRESS PERIOD(S) IN SIMULATION')
C
C5------SELECT AND PRINT A MESSAGE SHOWING TIME UNIT.
C
C6------SELECT AND PRINT A MESSAGE SHOWING LENGTH UNIT.
C
C7------ALLOCATE LAYER FLAGS.
      ALLOCATE(LBOTM(NLAY))
      ALLOCATE(LAYCBD(NLAY))
C
C8------Read confining bed information
      ! adapt to NOLAYCBD option in MODFLOW OWHM V2
      CALL URDCOM(INDIS,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INDIS)
      IF(LINE(ISTART:ISTOP)=='NOLAYCBD'  .OR.
     +   LINE(ISTART:ISTOP)=='NO_LAYCBD' .OR.
     +   LINE(ISTART:ISTOP)=='NO' ) THEN
        LAYCBD = 0
      else
        BACKSPACE(INDIS)
        READ(INDIS,*) (LAYCBD(K),K=1,NLAY)
        LAYCBD(NLAY)=0
        WRITE(IOUT, FMT=*) 'LAYCBD(K):'
        WRITE(IOUT, *) (LAYCBD(K),K=1,NLAY)
      endif

C
C9------Count confining beds, setup the pointer to each layer's
C9------bottom array (LBOTM), and setup LAYCBD to be the confining
C9------bed number for each layer.
      NCNFBD=0
      DO 100 K=1,NLAY
      LBOTM(K)=K+NCNFBD
      IF(LAYCBD(K).NE.0) THEN
         NCNFBD=NCNFBD+1
         LAYCBD(K)=NCNFBD
      END IF
  100 CONTINUE
      NBOTM=NLAY+NCNFBD
C
C10-----Allocate space for discretization arrays
C10-----Note that NBOTM+1 arrays are allocated for BOTM
C10-----because BOTM(J,I,0) contains the top elevation of layer 1.
      ALLOCATE (DELR(NCOL))
      ALLOCATE (DELC(NROW))
      ALLOCATE (BOTM(NCOL,NROW,0:NBOTM))
      ALLOCATE (PERLEN(NPER),NSTP(NPER),TSMULT(NPER),ISSFLG(NPER))
C
C11-----Read the DELR and DELC arrays.
      CALL U1DREL(DELR,ANAME(1),NCOL,INDIS,IOUT)
      CALL U1DREL(DELC,ANAME(2),NROW,INDIS,IOUT)

      ! skip reading surface in OWHM V2.
      ! this won't work if the array is internal.
      CALL URDCOM(INDIS,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INDIS)
      IF(LINE(ISTART:ISTOP) == 'SURFACE' .OR.
     +   LINE(ISTART:ISTOP) == 'SURFACE_ELEVATION') THEN
         continue
      else
         BACKSPACE(INDIS)
      endif


C
C12-----Read the top elevation of layer 1.
      CALL U2DREL(BOTM(:,:,0),ANAME(3),NROW,NCOL,0,INDIS,IOUT)
C
C13-----Read the bottom elevations.
      DO 120 K=1,NLAY
      KK=K
      CALL U2DREL(BOTM(:,:,LBOTM(K)),ANAME(4),NROW,NCOL,KK,INDIS,IOUT)
      IF(LAYCBD(K).NE.0) CALL U2DREL(BOTM(:,:,LBOTM(K)+1),ANAME(5),
     1          NROW,NCOL,KK,INDIS,IOUT)
  120 CONTINUE
C
C14-----READ AND WRITE LENGTH OF STRESS PERIOD, NUMBER OF TIME STEPS,
C14-----TIME STEP MULTIPLIER, AND STEADY-STATE FLAG..
!      WRITE(IOUT,161)
  !161 FORMAT(1X,//1X,'STRESS PERIOD     LENGTH       TIME STEPS',
!     1            '     MULTIPLIER FOR DELT    SS FLAG',/1X,76('-'))
      ISS=0
      ITR=0
      DO 200 N=1,NPER
      READ(INDIS,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,PERLEN(N),IOUT,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NSTP(N),R,IOUT,INDIS)
      ! OWHM V2 allows negative NSTP
	  NSTP(N) = ABS(NSTP(N))
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TSMULT(N),IOUT,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,INDIS)
      IF (LINE(ISTART:ISTOP).EQ.'TR') THEN
         ISSFLG(N)=0
         ITR=1
      ELSE IF (LINE(ISTART:ISTOP).EQ.'SS') THEN
         ISSFLG(N)=1
         ISS=1
      ELSE
         WRITE(IOUT,162)
  162    FORMAT(' SSFLAG MUST BE EITHER "SS" OR "TR"',
     1      ' -- STOP EXECUTION (SGWF2BAS7ARDIS)')
         CALL USTOP(' ')
      END IF
      WRITE (IOUT,*) 'N,PERLEN(N),NSTP(N),TSMULT(N),ISSFLG(N):'
      WRITE (IOUT,*) N,PERLEN(N),NSTP(N),TSMULT(N),ISSFLG(N)
      !WRITE (IOUT,163) N,PERLEN(N),NSTP(N),TSMULT(N),LINE(ISTART:ISTOP)
  !163 FORMAT(1X,I8,1PG21.7,I7,0PF25.3,A11)
C
C15-----STOP IF NSTP LE 0, PERLEN EQ 0 FOR TRANSIENT STRESS PERIODS,
C15-----TSMULT LE 0, OR PERLEN LT 0..
      IF(NSTP(N).LE.0) THEN
         WRITE(IOUT,164)
  164    FORMAT(1X,/1X,
     1  'THERE MUST BE AT LEAST ONE TIME STEP IN EVERY STRESS PERIOD')
         CALL USTOP(' ')
      END IF
      ZERO=0.
      IF(PERLEN(N).EQ.ZERO .AND. ISSFLG(N).EQ.0) THEN
         WRITE(IOUT,165)
  165    FORMAT(1X,/1X,
     1  'PERLEN MUST NOT BE 0.0 FOR TRANSIENT STRESS PERIODS')
         CALL USTOP(' ')
      END IF
      IF(TSMULT(N).LE.ZERO) THEN
         WRITE(IOUT,170)
  170    FORMAT(1X,/1X,'TSMULT MUST BE GREATER THAN 0.0')
         CALL USTOP(' ')
      END IF
      IF(PERLEN(N).LT.ZERO) THEN
         WRITE(IOUT,175)
  175    FORMAT(1X,/1X,
     1  'PERLEN CANNOT BE LESS THAN 0.0 FOR ANY STRESS PERIOD')
         CALL USTOP(' ')
      END IF
  200 CONTINUE
C
C16-----Assign ITRSS.
      IF(ISS.EQ.0 .AND. ITR.NE.0) THEN
         ITRSS=1
!         WRITE(IOUT,270)
!  270    FORMAT(/,1X,'TRANSIENT SIMULATION')
      ELSE IF(ISS.NE.0 .AND. ITR.EQ.0) THEN
         ITRSS=0
!         WRITE(IOUT,275)
!  275    FORMAT(/,1X,'STEADY-STATE SIMULATION')
      ELSE
         ITRSS=-1
!         WRITE(IOUT,280)
!  280    FORMAT(/,1X,'COMBINED STEADY-STATE AND TRANSIENT SIMULATION')
      END IF
C
C17-----RETURN.
      RETURN
      END
!      SUBROUTINE SGWF2BAS7D(KSTP,KPER,IPFLG,ISA)
C     ******************************************************************
C     CALCULATE, PRINT, AND SAVE DRAWDOWNS
C     ******************************************************************
!      SUBROUTINE SGWF2BAS7H(KSTP,KPER,IPFLG,ISA)
C     ******************************************************************
C     PRINT AND RECORD HEADS
C     ******************************************************************
!      SUBROUTINE SGWF2BAS7IB(KSTP,KPER)
C     ******************************************************************
C     RECORD IBOUND
C     ******************************************************************
!      SUBROUTINE SGWF2BAS7I(NLAY,INOC,IOUT,IFREFM,NIUNIT)
C     ******************************************************************
C     SET UP OUTPUT CONTROL.
C     ******************************************************************
      SUBROUTINE SGWF2BAS7J(INOC,IOUT,LINE,LLOC,ISTART,ISTOP)
C     ******************************************************************
C     READ INITIAL ALPHABETIC OUTPUT CONTROL RECORDS.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFBASMODULE, ONLY: IHEDFM,IDDNFM,IHEDUN,IDDNUN,IPEROC,ITSOC,
     1                        CHEDFM,CDDNFM,IBDOPT,LBHDSV,LBDDSV,
     2                        IBOUUN,LBBOSV,CBOUFM,IAUXSV,IDDREFNEW
C
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
C
C1------ALPHABETIC OUTPUT CONTROL.  WRITE MESSAGE AND SET INITIAL VALUES
C1------FOR IPEROC AND ITSOC.
!      WRITE(IOUT,91)
!   91 FORMAT(1X,/1X,'OUTPUT CONTROL IS SPECIFIED ONLY AT TIME STEPS',
!     1    ' FOR WHICH OUTPUT IS DESIRED')
      IPEROC=9999
      ITSOC=9999
C
C2------LOOK FOR ALPHABETIC WORDS:
C2A-----LOOK FOR "PERIOD", WHICH INDICATES THE END OF INITIAL OUTPUT
C2A-----CONTROL DATA.  IF FOUND, DECODE THE PERIOD NUMBER AND TIME
C2A-----STEP NUMBER FOR LATER USE.
  100 IF(LINE(ISTART:ISTOP).EQ.'PERIOD') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPEROC,R,IOUT,INOC)
	   WRITE(IOUT,*) 'PERIOD:'
	   WRITE(IOUT,*) IPEROC
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP).NE.'STEP') GO TO 2000
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITSOC,R,IOUT,INOC)
	   WRITE(IOUT,*) 'STEP:'
	   WRITE(IOUT,*) ITSOC
         WRITE(IOUT,101) IHEDFM,IDDNFM
  101    FORMAT(1X,'HEAD PRINT FORMAT CODE IS',I4,
     1        '    DRAWDOWN PRINT FORMAT CODE IS',I4)
         WRITE(IOUT,102) IHEDUN,IDDNUN
  102    FORMAT(1X,'HEADS WILL BE SAVED ON UNIT ',I4,
     1        '    DRAWDOWNS WILL BE SAVED ON UNIT ',I4)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP).EQ.'DDREFERENCE') THEN
           IDDREFNEW=1
         ELSE
           IDDREFNEW=0
         END IF
         GO TO 1000
C
C2B-----LOOK FOR "HEAD PRINT ..." AND "HEAD SAVE ...".  IF
C2B-----FOUND, SET APPROPRIATE FLAGS.
      ELSE IF(LINE(ISTART:ISTOP).EQ.'HEAD') THEN
	   WRITE(IOUT,*) 'HEAD:'
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP).EQ.'PRINT') THEN
	      WRITE(IOUT,*) 'PRINT:'
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).NE.'FORMAT') GO TO 2000
	      WRITE(IOUT,*) 'FORMAT:'
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDFM,R,IOUT,INOC)
	      WRITE(IOUT,*) IHEDFM
         ELSE IF(LINE(ISTART:ISTOP).EQ.'SAVE') THEN
	      WRITE(IOUT,*) 'SAVE:'
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).EQ.'UNIT') THEN
	         WRITE(IOUT,*) 'UNIT:'
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDUN,R,IOUT,
     1            INOC)
	         WRITE(IOUT,*) IHEDUN
            ELSE IF(LINE(ISTART:ISTOP).EQ.'FORMAT') THEN
	         WRITE(IOUT,*) 'FORMAT:'
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INOC)
               CHEDFM=LINE(ISTART:ISTOP)
               WRITE(IOUT,*) CHEDFM
!               WRITE(IOUT,103) CHEDFM
!  103          FORMAT(1X,'HEADS WILL BE SAVED WITH FORMAT: ',A)
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
               IF(LINE(ISTART:ISTOP).EQ.'LABEL') THEN
	            WRITE(IOUT,*) 'LABEL:'
                  LBHDSV=1
!                  WRITE(IOUT,104)
!  104             FORMAT(1X,'SAVED HEADS WILL BE LABELED')
               END IF
            ELSE
               GO TO 2000
            END IF
         ELSE
            GO TO 2000
         END IF
C
C2C-----LOOK FOR "DRAWDOWN PRINT ..." AND "DRAWDOWN SAVE ...".
C2C-----IF FOUND, SET APPROPRIATE FLAGS
      ELSE IF(LINE(ISTART:ISTOP).EQ.'DRAWDOWN') THEN
	   WRITE(IOUT,*) 'HEAD:'
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP).EQ.'PRINT') THEN
	      WRITE(IOUT,*) 'PRINT:'
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).NE.'FORMAT') GO TO 2000
	      WRITE(IOUT,*) 'FORMAT:'
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNFM,R,IOUT,INOC)
	      WRITE(IOUT,*) IDDNFM
         ELSE IF(LINE(ISTART:ISTOP).EQ.'SAVE') THEN
	      WRITE(IOUT,*) 'SAVE:'
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).EQ.'UNIT') THEN
	         WRITE(IOUT,*) 'UNIT:'
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNUN,R,IOUT,
     1                   INOC)
	         WRITE(IOUT,*) IDDNUN
            ELSE IF(LINE(ISTART:ISTOP).EQ.'FORMAT') THEN
	         WRITE(IOUT,*) 'FORMAT:'
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INOC)
               CDDNFM=LINE(ISTART:ISTOP)
               WRITE(IOUT,*) CDDNFM
!               WRITE(IOUT,105) CDDNFM
!  105          FORMAT(1X,'DRAWDOWN WILL BE SAVED WITH FORMAT: ',A)
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
               IF(LINE(ISTART:ISTOP).EQ.'LABEL') THEN
	            WRITE(IOUT,*) 'LABEL:'
                  LBDDSV=1
                  WRITE(IOUT,106)
  106             FORMAT(1X,'SAVED DRAWDOWN WILL BE LABELED')
               END IF
            ELSE
               GO TO 2000
            END IF
         ELSE
            GO TO 2000
         END IF
C
C2D-----LOOK FOR "COMPACT BUDGET FILES" -- "COMPACT" IS SUFFICIENT.
C2D-----IF FOUND, SET APPROPRIATE FLAG.
      ELSE IF(LINE(ISTART:ISTOP).EQ.'COMPACT') THEN
	   WRITE(IOUT,*) 'COMPACT:'
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP).EQ.'BUDGET') THEN
 	      WRITE(IOUT,*) 'BUDGET:'
            IBDOPT=2
!            WRITE(IOUT,107)
!  107       FORMAT(1X,
!     1      'COMPACT CELL-BY-CELL BUDGET FILES WILL BE WRITTEN')
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.
     1         LINE(ISTART:ISTOP).EQ.'AUX') THEN
 	         WRITE(IOUT,*) 'AUXILIARY:'
               IAUXSV=1
!               WRITE(IOUT,108)
!  108          FORMAT(1X,
!     1     'AUXILIARY DATA WILL BE SAVED IN CELL-BY-CELL BUDGET FILES')
            END IF
         ELSE
            GO TO 2000
         END IF
C
C2E-----LOOK FOR  "IBOUND SAVE ...".  IF FOUND, SET APPROPRIATE FLAGS.
      ELSE IF(LINE(ISTART:ISTOP).EQ.'IBOUND') THEN
	   WRITE(IOUT,*) 'IBOUND:'
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP).EQ.'SAVE') THEN
	      WRITE(IOUT,*) 'SAVE:'
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).EQ.'UNIT') THEN
	         WRITE(IOUT,*) 'UNIT:'
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IBOUUN,R,IOUT,
     1            INOC)
	         WRITE(IOUT,*) IBOUUN
!               WRITE(IOUT,111) IBOUUN
!  111          FORMAT(1X,'IBOUND WILL BE SAVED ON UNIT ',I4)
            ELSE IF(LINE(ISTART:ISTOP).EQ.'FORMAT') THEN
	         WRITE(IOUT,*) 'FORMAT:'
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INOC)
               CBOUFM=LINE(ISTART:ISTOP)
               WRITE(IOUT,*) CBOUFM
!               WRITE(IOUT,112) CBOUFM
!  112          FORMAT(1X,'IBOUND WILL BE SAVED WITH FORMAT: ',A)
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
               IF(LINE(ISTART:ISTOP).EQ.'LABEL') THEN
	            WRITE(IOUT,*) 'LABEL:'
                  LBBOSV=1
                  WRITE(IOUT,109)
  109             FORMAT(1X,'SAVED IBOUND WILL BE LABELED')
               END IF
            ELSE
               GO TO 2000
            END IF
         ELSE
            GO TO 2000
         END IF
C
C2F-----ERROR IF UNRECOGNIZED WORD.
      ELSE
         GO TO 2000
      END IF
C
C3------FINISHED READING A RECORD.  READ NEXT RECORD, IGNORING BLANK
C3------LINES.  GO BACK AND DECODE IT.
  110 READ(INOC,'(A)',END=1000) LINE
      IF(LINE.EQ.' ') GO TO 110
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
      GO TO 100
C
C4------RETURN.
 1000 RETURN
C
C5------ERROR DECODING INPUT DATA.
 2000 WRITE(IOUT,2001) LINE
 2001 FORMAT(1X,/1X,'ERROR READING OUTPUT CONTROL INPUT DATA:'/1X,A80)
      CALL USTOP(' ')
      END
!      SUBROUTINE SGWF2BAS7T(KSTP,KPER,DELT,PERTIM,TOTIM,ITMUNI,IOUT)
C     ******************************************************************
C     PRINT SIMULATION TIME
C     ******************************************************************
!      SUBROUTINE SGWF2BAS7V(MSUM,VBNM,VBVL,KSTP,KPER,IOUT)
C     ******************************************************************
C     PRINT VOLUMETRIC BUDGET
C     ******************************************************************
!      SUBROUTINE SGWF2BAS7N(KPER,KSTP,INOC,IOUT,NLAY)
C     ******************************************************************
C     SET OUTPUT FLAGS USING ALPHABETIC OUTPUT CONTROL INPUT STRUCTURE
C     ******************************************************************
!      SUBROUTINE SGWF2BAS7L(IPOS,LINE,LLOC,IOFLG,NLAY,IOUT,LABEL,INOC)
C     ******************************************************************
C     WHEN USING ALPHABETIC OUTPUT CONTROL, DECODE LAYER
C     NUMBERS FOR PRINTING OR SAVING HEAD OR DRAWDOWN
C     ******************************************************************
      SUBROUTINE SGWF2BAS7OPEN(INUNIT,IOUT,IUNIT,CUNIT,
     1              NIUNIT,VERSION,INBAS,MAXUNIT,MFVNAM)
C     ******************************************************************
C     OPEN FILES.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'openspec.inc'
      DIMENSION IUNIT(NIUNIT)
      CHARACTER*4 CUNIT(NIUNIT)
      CHARACTER*7 FILSTAT
      CHARACTER*20 FILACT, FMTARG, ACCARG
      CHARACTER*(*) VERSION,MFVNAM
      CHARACTER*40 SPACES
      CHARACTER*300 LINE, FNAME
      CHARACTER*20 FILTYP
      LOGICAL LOP
C     ---------------------------------------------------------------
C
C1------INITIALIZE CONSTANTS.
!      WRITE(IOUT, *) 'NAM:'
      INBAS=0
      NFILE=0
      IOUT=0
      DO 5 I=1,NIUNIT
      IUNIT(I)=0
5     CONTINUE
      SPACES=' '
      LENVER=LEN_TRIM(VERSION)
      INDENT=40-(LENVER+8)/2
C
C2------READ A LINE; IGNORE BLANK LINES AND PRINT COMMENT LINES.
10    READ(INUNIT,'(A)',END=1000) LINE
      IF(LINE.EQ.' ') GO TO 10
      IF(LINE(1:1).EQ.'#') THEN
        IF(NFILE.NE.0 .AND. IOUT.NE.0) WRITE(IOUT,'(A)') LINE
        GO TO 10
      END IF
C
C3------DECODE THE FILE TYPE, UNIT NUMBER, AND NAME.
      LLOC=1
      CALL URWORD(LINE,LLOC,ITYP1,ITYP2,1,N,R,IOUT,INUNIT)
      FILTYP=LINE(ITYP1:ITYP2)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,IOUT,INUNIT)
      CALL URWORD(LINE,LLOC,INAM1,INAM2,0,N,R,IOUT,INUNIT)
      IFLEN=INAM2-INAM1+1
      FNAME(1:IFLEN)=LINE(INAM1:INAM2)
      INQUIRE(UNIT=IU,OPENED=LOP)
      IF(LOP) THEN
         IF(IOUT.EQ.0) THEN
            WRITE(*,11) FNAME(1:IFLEN),IU
   11       FORMAT(1X,/1X,'CANNOT OPEN ',A,' ON UNIT',I4,
     1              ' BECAUSE UNIT IS ALREADY BEING USED')
         ELSE
            WRITE(IOUT,11) FNAME(1:IFLEN),IU
         END IF
         CALL USTOP(' ')
      END IF
C
C4------KEEP TRACK OF LARGEST UNIT NUMBER
      IF (IU.GT.MAXUNIT) MAXUNIT = IU
C
C5------SET DEFAULT FILE ATTRIBUTES.
      FMTARG='FORMATTED'
      ACCARG='SEQUENTIAL'
      FILSTAT='UNKNOWN'
      FILACT=' '
C
C6------SPECIAL CHECK FOR 1ST FILE.
      IF(NFILE.EQ.0) THEN
        IF(FILTYP.EQ.'LIST') THEN
          IOUT=IU
          OPEN(UNIT=IU,FILE=FNAME(1:IFLEN),STATUS='REPLACE',
     1          FORM='FORMATTED',ACCESS='SEQUENTIAL')
          WRITE(IOUT,60) MFVNAM,SPACES(1:INDENT),VERSION(1:LENVER)
60        FORMAT(34X,'MODFLOW',A,/,
     &             6X,'U.S. GEOLOGICAL SURVEY MODULAR',
     &             ' FINITE-DIFFERENCE GROUND-WATER FLOW MODEL',/,
     &             A,'VERSION ',A,/)
          WRITE(IOUT,65)
65        FORMAT('If you are attempting to run MODFLOW, you '
     &     'are making a big ***ERROR***. This is not MODFLOW. This '
     &     'program should only be used for importing existing models '
     &     'into ModelMuse.')
          WRITE(IOUT,78) FNAME(1:IFLEN),IOUT
78        FORMAT(1X,'LIST FILE: ',A,/25X,'UNIT ',I4)
        ELSE
          WRITE(*,*)
     1       ' FIRST ENTRY IN NAME FILE MUST BE "LIST".'
          CALL USTOP(' ')
        END IF
C  Get next file name
        NFILE=1
        GO TO 10
      END IF
C
C8------CHECK FOR "BAS" FILE TYPE.
      IF(FILTYP.EQ.'BAS6') THEN
         INBAS=IU
         FILSTAT='OLD    '
         FILACT=ACTION(1)
C
C9------CHECK FOR "UNFORMATTED" FILE TYPE.
      ELSE IF(FILTYP.EQ.'DATA(BINARY)' .OR.
     1        FILTYP.EQ.'DATAGLO(BINARY)') THEN
         FMTARG=FORM
         ACCARG=ACCESS
C
C10-----CHECK FOR "FORMATTED" FILE TYPE.
      ELSE IF(LINE(ITYP1:ITYP2).EQ.'DATA' .OR.
     1        LINE(ITYP1:ITYP2).EQ.'DATAGLO') THEN
         FMTARG='FORMATTED'
         ACCARG='SEQUENTIAL'
C
C11-----CHECK FOR MAJOR OPTIONS.
      ELSE
        DO 20 I=1,NIUNIT
           IF(LINE(ITYP1:ITYP2).EQ.CUNIT(I)) THEN
              IUNIT(I)=IU
              FILSTAT='OLD    '
              FILACT=ACTION(1)
              GO TO 30
           END IF
20      CONTINUE
        WRITE(IOUT,21) LINE(ITYP1:ITYP2)
21      FORMAT(1X,'ILLEGAL FILE TYPE IN NAME FILE: ',A)
        CALL USTOP(' ')
30      CONTINUE
      END IF
C
C12-----FOR DATA FILES, CHECK FOR "REPLACE" OR "OLD" OPTION
      IF (FILSTAT.EQ.'UNKNOWN') THEN
        CALL URWORD(LINE,LLOC,IOPT1,IOPT2,1,N,R,IOUT,INUNIT)
        IF (LINE(IOPT1:IOPT2).EQ.'REPLACE' .OR.
     &      LINE(IOPT1:IOPT2).EQ.'OLD')
     &      FILSTAT = LINE(IOPT1:IOPT2)
      ENDIF
C12A----Open file as read-only when 'OLD' is present to allow parallel
C12A----model runs to read data from file simultaneously.
      IF (FILACT.EQ.' ') THEN
        IF (FILSTAT.EQ.'OLD') THEN
          FILACT=ACTION(1)
        ELSE
          FILACT=ACTION(2)
        ENDIF
      ENDIF
C
C13-----WRITE THE FILE NAME AND OPEN IT.
      WRITE(IOUT,*) 'OPENING:'
      WRITE(IOUT,*) IU, FNAME(1:IFLEN)
            WRITE(IOUT,50) FNAME(1:IFLEN),
     1     LINE(ITYP1:ITYP2),IU,FILSTAT,FMTARG,ACCARG
50    FORMAT(1X,/1X,'OPENING ',A,/
     &  1X,'FILE TYPE:',A,'   UNIT ',I4,3X,'STATUS:',A,/
     &  1X,'FORMAT:',A,3X,'ACCESS:',A)
      OPEN(UNIT=IU,FILE=FNAME(1:IFLEN),FORM=FMTARG,
     1      ACCESS=ACCARG,STATUS=FILSTAT,ACTION=FILACT,ERR=2000)
      NFILE=NFILE+1
      GO TO 10
C
C14-----END OF NAME FILE.  RETURN PROVIDED THAT LISTING FILE AND BAS
C14-----FILES HAVE BEEN OPENED.
1000  IF(NFILE.EQ.0) THEN
         WRITE(*,*) ' NAME FILE IS EMPTY.'
         CALL USTOP(' ')
      ELSE IF(INBAS.EQ.0) THEN
         WRITE(IOUT,*) ' BAS PACKAGE FILE HAS NOT BEEN OPENED.'
         CALL USTOP(' ')
      END IF
      CLOSE (UNIT=INUNIT)
C
      RETURN
C
C15-----ERROR OPENING FILE.
 2000 CONTINUE
      WRITE(*,2010)FNAME(1:IFLEN),IU,FILSTAT,FMTARG,ACCARG,FILACT
      WRITE(IOUT,2010)FNAME(1:IFLEN),IU,FILSTAT,FMTARG,ACCARG,FILACT
 2010 FORMAT(/,1X,'*** ERROR OPENING FILE "',A,'" ON UNIT ',I5,/,
     &7X,'SPECIFIED FILE STATUS: ',A,/
     &7X,'SPECIFIED FILE FORMAT: ',A,/
     &7X,'SPECIFIED FILE ACCESS: ',A,/
     &7X,'SPECIFIED FILE ACTION: ',A,/
     &2X,'-- STOP EXECUTION (SGWF2BAS7OPEN)')
      CALL USTOP(' ')
C
      END
      SUBROUTINE SGWF2BAS7ARMZ(INZONE,INMULT)
C     ******************************************************************
C     ALLOCATE AND READ MULTIPLIER AND ZONE ARRAYS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:NCOL,NROW,IOUT
      USE PARAMMODULE,ONLY:NZONAR,NMLTAR,ZONNAM,MLTNAM,IZON,RMLT
C
      CHARACTER*20 RW
      CHARACTER*1 COP
      CHARACTER*24 ANAME
      CHARACTER*10 CTMP1,CTMP2
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
C
C1------Read Number of Zone Arrays if Zone Option is active.
      WRITE(IOUT, *) 'ZONE_MULT:'
      NZONAR=0
      IF(INZONE.GT.0) THEN
!         WRITE(IOUT,1) INZONE
    !1    FORMAT(1X,/1X,'ZONE OPTION, INPUT READ FROM UNIT ',I4)
         CALL URDCOM(INZONE,IOUT,LINE)
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NZONAR,R,IOUT,INZONE)
!         WRITE(IOUT,2) NZONAR
!    2    FORMAT(1X,I5,' ZONE ARRAYS')
         IF(NZONAR.LT.0) NZONAR=0
      END IF
      WRITE(IOUT, *) 'NZONAR:'
      WRITE(IOUT, *) NZONAR
C
C2------Allocate memory for zone arrays.  Allocate one array element if
C2------there are no zone arrays.
      IF(NZONAR.GT.0) THEN
        ALLOCATE (ZONNAM(NZONAR))
        ALLOCATE (IZON(NCOL,NROW,NZONAR))
      ELSE
        ALLOCATE (ZONNAM(1))
        ALLOCATE (IZON(1,1,1))
      ENDIF
C
C3------Read Number of Multiplier Arrays if Multiplier Option is active.
      NMLTAR=0
      IF(INMULT.GT.0) THEN
!         WRITE(IOUT,11) INMULT
!   11    FORMAT(1X,/1X,'MULTIPLIER OPTION, INPUT READ FROM UNIT ',I4)
         CALL URDCOM(INMULT,IOUT,LINE)
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NMLTAR,R,IOUT,INMULT)
!         WRITE(IOUT,12) NMLTAR
!   12    FORMAT(1X,I3,' MULTIPLIER ARRAYS')
         IF(NMLTAR.LT.0) NMLTAR=0
      END IF
      WRITE(IOUT, *) 'NMLTAR:'
      WRITE(IOUT, *) NMLTAR
C
C4------Allocate memory for multiplier arrays.  Allocate one array element if
C4------there are no multiplier arrays.
      IF(NMLTAR.GT.0) THEN
        ALLOCATE (MLTNAM(NMLTAR))
        ALLOCATE (RMLT(NCOL,NROW,NMLTAR))
      ELSE
        ALLOCATE (MLTNAM(1))
        ALLOCATE (RMLT(1,1,1))
      ENDIF
C
C5------Initialize names of zones, multipliers, and parameters.
      IF(NZONAR.GT.0) THEN
        DO 10 I=1,NZONAR
        ZONNAM(I)=' '
10      CONTINUE
      END IF
      IF(NMLTAR.GT.0) THEN
        DO 20 I=1,NMLTAR
        MLTNAM(I)=' '
20      CONTINUE
      END IF
C
C6------Define the multiplier arrays.
      IF(NMLTAR.GT.0) THEN
        DO 2000 M=1,NMLTAR
C
C6A-----Read a line describing a multiplier array.
          READ (INMULT,'(A)') LINE
C
C6B-----Get the name of the new array
          LLOC=1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INMULT)
C
C6C-----Add new multiplier name into list.
          MLTNAM(M)=LINE(ISTART:ISTOP)
	    WRITE(IOUT, *) 'M, MLTNAM(M):'
	    WRITE(IOUT, *) M
	    WRITE(IOUT, *) MLTNAM(M)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INMULT)
          IF(LINE(ISTART:ISTOP).NE.'FUNCTION') THEN
C
C6D-----Define array using array reader.
             ANAME=' MULT. ARRAY: '//MLTNAM(M)
             CALL U2DREL(RMLT(:,:,M),ANAME,NROW,NCOL,0,INMULT,IOUT)
          ELSE
  	    WRITE(IOUT, *) 'DEFINED BY FUNCTION:'
C
C6E-----Define array as aritmetic combination of other multiplier arrays.
C6E-----Start by initializing the array to 0.
!             WRITE(IOUT,30) MLTNAM(M)
!   30        FORMAT(1X,/1X,'Calculated multiplier array: ',A)
             DO 40 I=1,NROW
             DO 40 J=1,NCOL
             RMLT(J,I,M)=0.
   40        CONTINUE
C
C6E1----Get the names of the multipliers and the operands.
             READ (INMULT,'(A)') LINE
             LLOC=1
             NOP=0
C
C6E2----Get the operator.
   45        IF(NOP.EQ.0) THEN
C
C6E2A---No operator is specified before the first operand -- define it to be " "
                COP=' '
             ELSE
C
C6E2B---Get the operator that precedes each operand after the first operand.
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INMULT)
                IF(LINE(ISTART:ISTOP).EQ.'+' .OR.
     1             LINE(ISTART:ISTOP).EQ.'-' .OR.
     2             LINE(ISTART:ISTOP).EQ.'*' .OR.
     3             LINE(ISTART:ISTOP).EQ.'/') THEN
                   COP=LINE(ISTART:ISTOP)
                ELSE
                   GO TO 1000
                END IF
             END IF
             NOP=NOP+1
C
C6E3----Get the operand.
             CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INMULT)
!             WRITE(IOUT,47 ) COP,LINE(ISTART:ISTOP)
!   47        FORMAT(1X,'                        ',A,' ARRAY ',A)
C
C6E4----Lookup the operand in the list of existing multipliers
             CTMP2=LINE(ISTART:ISTOP)
             CALL UPCASE(CTMP2)
             DO 50 MM=1,M
               CTMP1=MLTNAM(MM)
               CALL UPCASE(CTMP1)
               IF(CTMP1.EQ.CTMP2) GO TO 60
   50        CONTINUE
             WRITE(IOUT,51) LINE(ISTART:ISTOP)
   51        FORMAT(1X,
     1        'ARRAY OPERAND HAS NOT BEEN PREVIOUSLY DEFINED:',A)
             CALL USTOP(' ')
C
C6E5----Apply the + operator.
   60        IF(COP.EQ.'+' .OR. COP.EQ.' ') THEN
                DO 100 I = 1, NROW
                DO 100 J = 1, NCOL
                  RMLT(J,I,M) = RMLT(J,I,M)+ RMLT(J,I,MM)
  100           CONTINUE
             ELSE IF(COP.EQ.'-') THEN
                DO 200 I = 1, NROW
                DO 200 J = 1, NCOL
                  RMLT(J,I,M) = RMLT(J,I,M)- RMLT(J,I,MM)
  200           CONTINUE
             ELSE IF(COP.EQ.'*') THEN
                DO 300 I = 1, NROW
                DO 300 J = 1, NCOL
                  RMLT(J,I,M) = RMLT(J,I,M)* RMLT(J,I,MM)
  300           CONTINUE
             ELSE
                DO 400 I = 1, NROW
                DO 400 J = 1, NCOL
                  RMLT(J,I,M) = RMLT(J,I,M)/ RMLT(J,I,MM)
  400           CONTINUE
             END IF
C
C6E6----Get the next operator.
             GO TO 45
C
C6E7-----Done defining the array.  Get the print code and print the array.
1000          IPRN=0
              L=20-ISTOP+ISTART
              IF(L.GT.1)  THEN
                 RW=' '
                 RW(L:20)=LINE(ISTART:ISTOP)
                 READ(RW,'(I20)',ERR=1200) IPRN
              END IF
 1200         IF(IPRN.GE.0) THEN
                 ANAME=' MULT. ARRAY: '//MLTNAM(M)
!                 CALL ULAPRWC(RMLT(:,:,M),NCOL,NROW,0,IOUT,IPRN,
!     1                 ANAME)
              END IF
              DO 1400 I = 1, NROW
                  WRITE(IOUT, *) (RMLT(J,I,M),J=1,NCOL)
 1400         CONTINUE
          END IF
 2000   CONTINUE
      ENDIF
C
C7------Read the zone array names and arrays
      IF(NZONAR.GT.0) THEN
         DO 3000 NZ=1,NZONAR
         READ(INZONE,'(A)') ZONNAM(NZ)
	   WRITE(IOUT, *) 'NZ, ZONNAM(NZ):'
	   WRITE(IOUT, *) NZ
	   WRITE(IOUT, *) ZONNAM(NZ)
         CALL U2DINT(IZON(:,:,NZ),'  ZONE ARRAY: '//ZONNAM(NZ),
     1            NROW,NCOL,0,INZONE,IOUT)
 3000    CONTINUE
      END IF
C
C8------Return.
      RETURN
      END
      SUBROUTINE SGWF2BAS7ARPVAL(IUPVAL)
C     ******************************************************************
C     READ PARAMETER INPUT FILE
C     ******************************************************************
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,    ONLY: IOUT,IUNIT
      USE PARAMMODULE, ONLY:MXPAR,IPSUM,PARNAM,B,NPVAL
C
      CHARACTER*10 PNI, PNJ
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
C
C1------CHECK TO SEE IF THE PARAMETER FILE WAS DECLARED IN THE NAME FILE.
      IU=IUNIT(IUPVAL)
      IF(IU.LE.0) THEN
         NPVAL=0
         RETURN
      END IF
C
C2------INITIALIZE VARIABLES
      IERR = 0
      NPE = 0
C
C3------IDENTIFY PARAMETER VALUE OPTION.
      WRITE (IOUT,*) 'PVAL:'
!      WRITE (IOUT,12) IU
!   12 FORMAT (1X,/,1X,
!     1  'PARAMETER VALUE INPUT FILE,  INPUT READ FROM UNIT ',I4)
C
C4------READ & PRINT NUMBER OF PARAMETER VALUES.
      CALL URDCOM(IU,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPVAL,DUM,IOUT,IU)
      WRITE (IOUT,*) 'NPVAL:'
      WRITE (IOUT,*) NPVAL
!      WRITE (IOUT,14) NPVAL
!   14 FORMAT (1X,/,1X,'NUMBER OF PARAMETER VALUES TO BE READ FROM',
!     1               ' PARAMETER VALUE FILE:',I5)
      IF (NPVAL.LE.0) THEN
        WRITE (IOUT,16)
   16   FORMAT(1X,'NPVAL IN PARAMETER INPUT FILE MUST BE',
     1         ' > 0 -- STOP EXECUTION')
        CALL USTOP(' ')
      ENDIF
      IPSUM=NPVAL
C
C5-----DEACTIVATE OPTION IF THERE ARE NO PARAMETERS IN FILE.
      IF(NPVAL.LE.0) THEN
         WRITE(IOUT,*) ' NPVAL in parameter file is 0,',
     1            ' so ignoring the parameter file'
        CLOSE(UNIT=IU)
        IU=0
        RETURN
      END IF
C
C6------STOP IF THERE ARE MORE THAN THE MAXIMUM NUMBER OF PARAMETERS.
      IF(NPVAL.GT.MXPAR) THEN
         WRITE(IOUT,*) ' PARAMETER FILE CONTAINS',NPVAL,
     1     ' VALUES, BUT THE MAXIMUM NUMBER OF PARAMETERS IS',MXPAR
         CALL USTOP(' ')
      END IF
C
C7------WRITE A HEADING FOR THE LIST OF PARAMETERS.
!      WRITE (IOUT,520)
!  520 FORMAT (/,' INFORMATION ON PARAMETERS LISTED IN PARAMETER FILE',/,
!     &             13X,'  VALUE IN',/,
!     &   '    NAME     PARAMETER FILE',/,
!     &   ' ----------  --------------')
C
C8-----READ AND WRITE PARAMETER NAMES AND VALUES.
      DO 70 I=1,NPVAL
        READ(IU,*,ERR=80) PARNAM(I),B(I)
        WRITE(IOUT,*) 'PARNAM(I),B(I):'
        WRITE(IOUT,*) PARNAM(I)
        WRITE(IOUT,*) B(I)
!        WRITE(IOUT,570) PARNAM(I),B(I)
!  570   FORMAT(1X,A10,2X,G12.5)
C
C8A-----CHECK FOR DUPLICATE PARAMETER NAME FOR ALL BUT THE FIRST PARAMETER.
        IF (I.GT.1) THEN
          PNI=PARNAM(I)
          CALL UPCASE(PNI)
          IM1 = I-1
          DO 60 J=1,IM1
            PNJ=PARNAM(J)
            CALL UPCASE(PNJ)
            IF (PNI.EQ.PNJ) THEN
              WRITE(IOUT,500) PARNAM(I)
  500         FORMAT (' PARAMETER "',A10,
     &        '" IS LISTED MORE THAN ONCE IN PARAMETER FILE',/,
     &        ' -- STOP EXECUTION')
                IERR = 1
            ENDIF
   60     CONTINUE
        ENDIF
   70 CONTINUE
C
C9------WRITE A MESSAGE EXPLAINING THAT THE PARAMETER VALUES REPLACE THE
C9------VALUES FROM PACKAGE INPUT FILES..
      WRITE (IOUT,620)
  620 FORMAT(1X,77('-'))
      WRITE (IOUT,630)
  630 FORMAT(' FOR THE PARAMETERS LISTED IN THE TABLE ABOVE,',
     &       ' PARAMETER VALUES IN INDIVIDUAL',/,
     &       ' PACKAGE INPUT FILES ARE REPLACED BY THE VALUES FROM',
     &       ' THE PARAMETER INPUT FILE.')
C
C10-----STOP IF THERE WERE DUPLICATE NAMES.
      IF (IERR.GT.0) THEN
        WRITE(IOUT,680)
  680 FORMAT(/,
     &' ERROR FOUND IN PARAMETER INPUT FILE.  SEARCH ABOVE',/,
     &' FOR "STOP EXECUTION"')
         CALL USTOP(' ')
      ENDIF
C
C11-----CLOSE FILE AND RETURN.
      CLOSE(UNIT=IU)
      RETURN
C
C
   80 WRITE(IOUT,590)
  590 FORMAT(1X,/,1X,
     1  'ERROR ENCOUNTERED IN READING PARAMETER INPUT FILE',/,
     2       ' -- STOP EXECUTION')
      CALL USTOP(' ')
C
      END
      SUBROUTINE SGWF2BAS7STPVAL()
C     ******************************************************************
C     CHECK THAT PARAMETER DEFINITIONS ARE COMPLETE.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY: IOUT
      USE PARAMMODULE, ONLY:NPVAL,PARTYP,PARNAM
C     ------------------------------------------------------------------
      IF(NPVAL.LE.0) RETURN
      IERR=0
C
Cx------CHECK THAT ALL PARAMETERS IN PARAMETER INPUT FILE HAVE BEEN DEFINED.
      DO 90 IP=1,NPVAL
        IF (PARTYP(IP).EQ.' ') THEN
          IERR = 1
          WRITE(IOUT,110) PARNAM(IP)
  110     FORMAT(1X,/,1X,'PARAMETER "',A10,
     1      '" IN PARAMETER INPUT FILE HAS NOT BEEN DEFINED',/,
     2           ' -- STOP EXECUTION')
        ENDIF
   90 CONTINUE
C
      IF(IERR.NE.0) CALL USTOP(' ')
C
Cx------RETURN.
      RETURN
      END
      SUBROUTINE GWF2BAS7DA(IGRID)
C  DEALLOCATE GLOBAL DATA
      USE GLOBAL
      USE PARAMMODULE
      USE GWFBASMODULE
C
        DEALLOCATE(GLOBALDAT(IGRID)%NCOL)
        DEALLOCATE(GLOBALDAT(IGRID)%NROW)
        DEALLOCATE(GLOBALDAT(IGRID)%NLAY)
        DEALLOCATE(GLOBALDAT(IGRID)%NPER)
        DEALLOCATE(GLOBALDAT(IGRID)%NBOTM)
        DEALLOCATE(GLOBALDAT(IGRID)%NCNFBD)
        DEALLOCATE(GLOBALDAT(IGRID)%ITMUNI)
        DEALLOCATE(GLOBALDAT(IGRID)%LENUNI)
        DEALLOCATE(GLOBALDAT(IGRID)%IXSEC)
        DEALLOCATE(GLOBALDAT(IGRID)%ITRSS)
        DEALLOCATE(GLOBALDAT(IGRID)%INBAS)
        DEALLOCATE(GLOBALDAT(IGRID)%IFREFM)
        DEALLOCATE(GLOBALDAT(IGRID)%NODES)
        DEALLOCATE(GLOBALDAT(IGRID)%IOUT)
        DEALLOCATE(GLOBALDAT(IGRID)%MXITER)
C
        DEALLOCATE(GLOBALDAT(IGRID)%IUNIT)
        DEALLOCATE(GLOBALDAT(IGRID)%LAYCBD)
        DEALLOCATE(GLOBALDAT(IGRID)%LAYHDT)
        DEALLOCATE(GLOBALDAT(IGRID)%LAYHDS)
        DEALLOCATE(GLOBALDAT(IGRID)%PERLEN)
        DEALLOCATE(GLOBALDAT(IGRID)%NSTP)
        DEALLOCATE(GLOBALDAT(IGRID)%TSMULT)
        DEALLOCATE(GLOBALDAT(IGRID)%ISSFLG)
        DEALLOCATE(GLOBALDAT(IGRID)%DELR)
        DEALLOCATE(GLOBALDAT(IGRID)%DELC)
        DEALLOCATE(GLOBALDAT(IGRID)%BOTM)
        DEALLOCATE(GLOBALDAT(IGRID)%LBOTM)
        DEALLOCATE(GLOBALDAT(IGRID)%HNEW)
        DEALLOCATE(GLOBALDAT(IGRID)%HOLD)
        DEALLOCATE(GLOBALDAT(IGRID)%IBOUND)
        DEALLOCATE(GLOBALDAT(IGRID)%CR)
        DEALLOCATE(GLOBALDAT(IGRID)%CC)
        DEALLOCATE(GLOBALDAT(IGRID)%CV)
        DEALLOCATE(GLOBALDAT(IGRID)%HCOF)
        DEALLOCATE(GLOBALDAT(IGRID)%RHS)
        DEALLOCATE(GLOBALDAT(IGRID)%BUFF)
        DEALLOCATE(GLOBALDAT(IGRID)%STRT)
        IF(.NOT.ASSOCIATED(DDREF,STRT))
     1           DEALLOCATE(GLOBALDAT(IGRID)%DDREF)
C
        DEALLOCATE(ICLSUM,IPSUM,INAMLOC,NMLTAR,NZONAR,NPVAL)
        DEALLOCATE (PARAMDAT(IGRID)%B)
        DEALLOCATE (PARAMDAT(IGRID)%IACTIVE)
        DEALLOCATE (PARAMDAT(IGRID)%IPLOC)
        DEALLOCATE (PARAMDAT(IGRID)%IPCLST)
        DEALLOCATE (PARAMDAT(IGRID)%PARNAM)
        DEALLOCATE (PARAMDAT(IGRID)%PARTYP)
        DEALLOCATE (PARAMDAT(IGRID)%ZONNAM)
        DEALLOCATE (PARAMDAT(IGRID)%MLTNAM)
        DEALLOCATE (PARAMDAT(IGRID)%INAME)
        DEALLOCATE (PARAMDAT(IGRID)%RMLT)
        DEALLOCATE (PARAMDAT(IGRID)%IZON)
C
        DEALLOCATE(GWFBASDAT(IGRID)%MSUM)
        DEALLOCATE(GWFBASDAT(IGRID)%IHEDFM)
        DEALLOCATE(GWFBASDAT(IGRID)%IHEDUN)
        DEALLOCATE(GWFBASDAT(IGRID)%IDDNFM)
        DEALLOCATE(GWFBASDAT(IGRID)%IDDNUN)
        DEALLOCATE(GWFBASDAT(IGRID)%IBOUUN)
        DEALLOCATE(GWFBASDAT(IGRID)%LBHDSV)
        DEALLOCATE(GWFBASDAT(IGRID)%LBDDSV)
        DEALLOCATE(GWFBASDAT(IGRID)%LBBOSV)
        DEALLOCATE(GWFBASDAT(IGRID)%IBUDFL)
        DEALLOCATE(GWFBASDAT(IGRID)%ICBCFL)
        DEALLOCATE(GWFBASDAT(IGRID)%IHDDFL)
        DEALLOCATE(GWFBASDAT(IGRID)%IAUXSV)
        DEALLOCATE(GWFBASDAT(IGRID)%IBDOPT)
        DEALLOCATE(GWFBASDAT(IGRID)%IPRTIM)
        DEALLOCATE(GWFBASDAT(IGRID)%IPEROC)
        DEALLOCATE(GWFBASDAT(IGRID)%ITSOC)
        DEALLOCATE(GWFBASDAT(IGRID)%ICHFLG)
        DEALLOCATE(GWFBASDAT(IGRID)%IDDREF)
        DEALLOCATE(GWFBASDAT(IGRID)%IDDREFNEW)
        DEALLOCATE(GWFBASDAT(IGRID)%DELT)
        DEALLOCATE(GWFBASDAT(IGRID)%PERTIM)
        DEALLOCATE(GWFBASDAT(IGRID)%TOTIM)
        DEALLOCATE(GWFBASDAT(IGRID)%HNOFLO)
        DEALLOCATE(GWFBASDAT(IGRID)%HDRY)
        DEALLOCATE(GWFBASDAT(IGRID)%STOPER)
        DEALLOCATE(GWFBASDAT(IGRID)%CHEDFM)
        DEALLOCATE(GWFBASDAT(IGRID)%CDDNFM)
        DEALLOCATE(GWFBASDAT(IGRID)%CBOUFM)
C
!        DEALLOCATE(GWFBASDAT(IGRID)%IOFLG)
!        DEALLOCATE(GWFBASDAT(IGRID)%VBVL)
!        DEALLOCATE(GWFBASDAT(IGRID)%VBNM)
C
      RETURN
      END
      SUBROUTINE SGWF2BAS7PNT(IGRID)
C  Change global data to a different grid.
      USE GLOBAL
      USE PARAMMODULE
      USE GWFBASMODULE
C
        NCOL=>GLOBALDAT(IGRID)%NCOL
        NROW=>GLOBALDAT(IGRID)%NROW
        NLAY=>GLOBALDAT(IGRID)%NLAY
        NPER=>GLOBALDAT(IGRID)%NPER
        NBOTM=>GLOBALDAT(IGRID)%NBOTM
        NCNFBD=>GLOBALDAT(IGRID)%NCNFBD
        ITMUNI=>GLOBALDAT(IGRID)%ITMUNI
        LENUNI=>GLOBALDAT(IGRID)%LENUNI
        IXSEC=>GLOBALDAT(IGRID)%IXSEC
        ITRSS=>GLOBALDAT(IGRID)%ITRSS
        INBAS=>GLOBALDAT(IGRID)%INBAS
        IFREFM=>GLOBALDAT(IGRID)%IFREFM
        NODES=>GLOBALDAT(IGRID)%NODES
        IOUT=>GLOBALDAT(IGRID)%IOUT
        MXITER=>GLOBALDAT(IGRID)%MXITER
C
        IUNIT=>GLOBALDAT(IGRID)%IUNIT
        LAYCBD=>GLOBALDAT(IGRID)%LAYCBD
        LAYHDT=>GLOBALDAT(IGRID)%LAYHDT
        LAYHDS=>GLOBALDAT(IGRID)%LAYHDS
        PERLEN=>GLOBALDAT(IGRID)%PERLEN
        NSTP=>GLOBALDAT(IGRID)%NSTP
        TSMULT=>GLOBALDAT(IGRID)%TSMULT
        ISSFLG=>GLOBALDAT(IGRID)%ISSFLG
        DELR=>GLOBALDAT(IGRID)%DELR
        DELC=>GLOBALDAT(IGRID)%DELC
        BOTM=>GLOBALDAT(IGRID)%BOTM
        LBOTM=>GLOBALDAT(IGRID)%LBOTM
        HNEW=>GLOBALDAT(IGRID)%HNEW
        HOLD=>GLOBALDAT(IGRID)%HOLD
        IBOUND=>GLOBALDAT(IGRID)%IBOUND
        CR=>GLOBALDAT(IGRID)%CR
        CC=>GLOBALDAT(IGRID)%CC
        CV=>GLOBALDAT(IGRID)%CV
        HCOF=>GLOBALDAT(IGRID)%HCOF
        RHS=>GLOBALDAT(IGRID)%RHS
        BUFF=>GLOBALDAT(IGRID)%BUFF
        STRT=>GLOBALDAT(IGRID)%STRT
        DDREF=>GLOBALDAT(IGRID)%DDREF
C
        ICLSUM=>PARAMDAT(IGRID)%ICLSUM
        IPSUM=>PARAMDAT(IGRID)%IPSUM
        INAMLOC=>PARAMDAT(IGRID)%INAMLOC
        NMLTAR=>PARAMDAT(IGRID)%NMLTAR
        NZONAR=>PARAMDAT(IGRID)%NZONAR
        NPVAL=>PARAMDAT(IGRID)%NPVAL
C
        B=>PARAMDAT(IGRID)%B
        IACTIVE=>PARAMDAT(IGRID)%IACTIVE
        IPLOC=>PARAMDAT(IGRID)%IPLOC
        IPCLST=>PARAMDAT(IGRID)%IPCLST
        IZON=>PARAMDAT(IGRID)%IZON
        RMLT=>PARAMDAT(IGRID)%RMLT
        PARNAM=>PARAMDAT(IGRID)%PARNAM
        PARTYP=>PARAMDAT(IGRID)%PARTYP
        ZONNAM=>PARAMDAT(IGRID)%ZONNAM
        MLTNAM=>PARAMDAT(IGRID)%MLTNAM
        INAME=>PARAMDAT(IGRID)%INAME
C
        MSUM=>GWFBASDAT(IGRID)%MSUM
        IHEDFM=>GWFBASDAT(IGRID)%IHEDFM
        IHEDUN=>GWFBASDAT(IGRID)%IHEDUN
        IDDNFM=>GWFBASDAT(IGRID)%IDDNFM
        IDDNUN=>GWFBASDAT(IGRID)%IDDNUN
        IBOUUN=>GWFBASDAT(IGRID)%IBOUUN
        LBHDSV=>GWFBASDAT(IGRID)%LBHDSV
        LBDDSV=>GWFBASDAT(IGRID)%LBDDSV
        LBBOSV=>GWFBASDAT(IGRID)%LBBOSV
        IBUDFL=>GWFBASDAT(IGRID)%IBUDFL
        ICBCFL=>GWFBASDAT(IGRID)%ICBCFL
        IHDDFL=>GWFBASDAT(IGRID)%IHDDFL
        IAUXSV=>GWFBASDAT(IGRID)%IAUXSV
        IBDOPT=>GWFBASDAT(IGRID)%IBDOPT
        IPRTIM=>GWFBASDAT(IGRID)%IPRTIM
        IPEROC=>GWFBASDAT(IGRID)%IPEROC
        ITSOC=>GWFBASDAT(IGRID)%ITSOC
        ICHFLG=>GWFBASDAT(IGRID)%ICHFLG
        IDDREF=>GWFBASDAT(IGRID)%IDDREF
        IDDREFNEW=>GWFBASDAT(IGRID)%IDDREFNEW
        DELT=>GWFBASDAT(IGRID)%DELT
        PERTIM=>GWFBASDAT(IGRID)%PERTIM
        TOTIM=>GWFBASDAT(IGRID)%TOTIM
        HNOFLO=>GWFBASDAT(IGRID)%HNOFLO
        HDRY=>GWFBASDAT(IGRID)%HDRY
        STOPER=>GWFBASDAT(IGRID)%STOPER
        CHEDFM=>GWFBASDAT(IGRID)%CHEDFM
        CDDNFM=>GWFBASDAT(IGRID)%CDDNFM
        CBOUFM=>GWFBASDAT(IGRID)%CBOUFM
C
!        IOFLG=>GWFBASDAT(IGRID)%IOFLG
!        VBVL=>GWFBASDAT(IGRID)%VBVL
!        VBNM=>GWFBASDAT(IGRID)%VBNM
C
      RETURN
      END
      SUBROUTINE SGWF2BAS7PSV(IGRID)
C  Save global data for a grid.
      USE GLOBAL
      USE PARAMMODULE
      USE GWFBASMODULE
C
        GLOBALDAT(IGRID)%NCOL=>NCOL
        GLOBALDAT(IGRID)%NROW=>NROW
        GLOBALDAT(IGRID)%NLAY=>NLAY
        GLOBALDAT(IGRID)%NPER=>NPER
        GLOBALDAT(IGRID)%NBOTM=>NBOTM
        GLOBALDAT(IGRID)%NCNFBD=>NCNFBD
        GLOBALDAT(IGRID)%ITMUNI=>ITMUNI
        GLOBALDAT(IGRID)%LENUNI=>LENUNI
        GLOBALDAT(IGRID)%IXSEC=>IXSEC
        GLOBALDAT(IGRID)%ITRSS=>ITRSS
        GLOBALDAT(IGRID)%INBAS=>INBAS
        GLOBALDAT(IGRID)%IFREFM=>IFREFM
        GLOBALDAT(IGRID)%NODES=>NODES
        GLOBALDAT(IGRID)%IOUT=>IOUT
        GLOBALDAT(IGRID)%MXITER=>MXITER
C
        GLOBALDAT(IGRID)%IUNIT=>IUNIT
        GLOBALDAT(IGRID)%LAYCBD=>LAYCBD
        GLOBALDAT(IGRID)%LAYHDT=>LAYHDT
        GLOBALDAT(IGRID)%LAYHDS=>LAYHDS
        GLOBALDAT(IGRID)%PERLEN=>PERLEN
        GLOBALDAT(IGRID)%NSTP=>NSTP
        GLOBALDAT(IGRID)%TSMULT=>TSMULT
        GLOBALDAT(IGRID)%ISSFLG=>ISSFLG
        GLOBALDAT(IGRID)%DELR=>DELR
        GLOBALDAT(IGRID)%DELC=>DELC
        GLOBALDAT(IGRID)%BOTM=>BOTM
        GLOBALDAT(IGRID)%LBOTM=>LBOTM
        GLOBALDAT(IGRID)%HNEW=>HNEW
        GLOBALDAT(IGRID)%HOLD=>HOLD
        GLOBALDAT(IGRID)%IBOUND=>IBOUND
        GLOBALDAT(IGRID)%CR=>CR
        GLOBALDAT(IGRID)%CC=>CC
        GLOBALDAT(IGRID)%CV=>CV
        GLOBALDAT(IGRID)%HCOF=>HCOF
        GLOBALDAT(IGRID)%RHS=>RHS
        GLOBALDAT(IGRID)%BUFF=>BUFF
        GLOBALDAT(IGRID)%STRT=>STRT
        GLOBALDAT(IGRID)%DDREF=>DDREF
C
        PARAMDAT(IGRID)%ICLSUM=>ICLSUM
        PARAMDAT(IGRID)%IPSUM=>IPSUM
        PARAMDAT(IGRID)%INAMLOC=>INAMLOC
        PARAMDAT(IGRID)%NMLTAR=>NMLTAR
        PARAMDAT(IGRID)%NZONAR=>NZONAR
        PARAMDAT(IGRID)%NPVAL=>NPVAL
C
        PARAMDAT(IGRID)%B=>B
        PARAMDAT(IGRID)%IACTIVE=>IACTIVE
        PARAMDAT(IGRID)%IPLOC=>IPLOC
        PARAMDAT(IGRID)%IPCLST=>IPCLST
        PARAMDAT(IGRID)%IZON=>IZON
        PARAMDAT(IGRID)%RMLT=>RMLT
        PARAMDAT(IGRID)%PARNAM=>PARNAM
        PARAMDAT(IGRID)%PARTYP=>PARTYP
        PARAMDAT(IGRID)%ZONNAM=>ZONNAM
        PARAMDAT(IGRID)%MLTNAM=>MLTNAM
        PARAMDAT(IGRID)%INAME=>INAME
C
        GWFBASDAT(IGRID)%MSUM=>MSUM
        GWFBASDAT(IGRID)%IHEDFM=>IHEDFM
        GWFBASDAT(IGRID)%IHEDUN=>IHEDUN
        GWFBASDAT(IGRID)%IDDNFM=>IDDNFM
        GWFBASDAT(IGRID)%IDDNUN=>IDDNUN
        GWFBASDAT(IGRID)%IBOUUN=>IBOUUN
        GWFBASDAT(IGRID)%LBHDSV=>LBHDSV
        GWFBASDAT(IGRID)%LBDDSV=>LBDDSV
        GWFBASDAT(IGRID)%LBBOSV=>LBBOSV
        GWFBASDAT(IGRID)%IBUDFL=>IBUDFL
        GWFBASDAT(IGRID)%ICBCFL=>ICBCFL
        GWFBASDAT(IGRID)%IHDDFL=>IHDDFL
        GWFBASDAT(IGRID)%IAUXSV=>IAUXSV
        GWFBASDAT(IGRID)%IBDOPT=>IBDOPT
        GWFBASDAT(IGRID)%IPRTIM=>IPRTIM
        GWFBASDAT(IGRID)%IPEROC=>IPEROC
        GWFBASDAT(IGRID)%ITSOC=>ITSOC
        GWFBASDAT(IGRID)%ICHFLG=>ICHFLG
        GWFBASDAT(IGRID)%IDDREF=>IDDREF
        GWFBASDAT(IGRID)%IDDREFNEW=>IDDREFNEW
        GWFBASDAT(IGRID)%DELT=>DELT
        GWFBASDAT(IGRID)%PERTIM=>PERTIM
        GWFBASDAT(IGRID)%TOTIM=>TOTIM
        GWFBASDAT(IGRID)%HNOFLO=>HNOFLO
        GWFBASDAT(IGRID)%HDRY=>HDRY
        GWFBASDAT(IGRID)%STOPER=>STOPER
        GWFBASDAT(IGRID)%CHEDFM=>CHEDFM
        GWFBASDAT(IGRID)%CDDNFM=>CDDNFM
        GWFBASDAT(IGRID)%CBOUFM=>CBOUFM
C
!        GWFBASDAT(IGRID)%IOFLG=>IOFLG
!        GWFBASDAT(IGRID)%VBVL=>VBVL
!        GWFBASDAT(IGRID)%VBNM=>VBNM
C
      RETURN
      END
