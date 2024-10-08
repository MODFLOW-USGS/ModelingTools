      MODULE GWFSUBMODULE
        INTEGER,SAVE,POINTER ::IIBSCB,ITMIN,NNDB,NDB,NMZ,NN,ND2,IDSAVE
        REAL,   SAVE,POINTER ::AC1,AC2
        LOGICAL,SAVE,POINTER ::NDF,NNDF
        INTEGER,SAVE, DIMENSION(:),   POINTER ::ISBOCF,ISBOCU
        LOGICAL,SAVE, DIMENSION(:,:), POINTER ::OCFLGS
        LOGICAL,SAVE, DIMENSION(:),   POINTER ::OCLAY
        INTEGER,SAVE, DIMENSION(:),   POINTER ::ILSYS
        INTEGER,SAVE, DIMENSION(:),   POINTER ::NTSSUM
        INTEGER,SAVE, DIMENSION(:),   POINTER ::LN
        INTEGER,SAVE, DIMENSION(:),   POINTER ::LDN
        INTEGER,SAVE, DIMENSION(:),   POINTER ::NZ
        REAL,   SAVE, DIMENSION(:),   POINTER ::RNB
        REAL,   SAVE, DIMENSION(:),   POINTER ::DH
        REAL,   SAVE, DIMENSION(:),   POINTER ::DHP
        REAL,   SAVE, DIMENSION(:),   POINTER ::DHC
        REAL,   SAVE, DIMENSION(:),   POINTER ::DZ
        REAL,   SAVE, DIMENSION(:),   POINTER ::HC
        REAL,   SAVE, DIMENSION(:),   POINTER ::SCE
        REAL,   SAVE, DIMENSION(:),   POINTER ::SCV
        REAL,   SAVE, DIMENSION(:),   POINTER ::DCOM
        REAL,   SAVE, DIMENSION(:),   POINTER ::A1
        REAL,   SAVE, DIMENSION(:),   POINTER ::A2
        REAL,   SAVE, DIMENSION(:),   POINTER ::BB
        REAL,   SAVE, DIMENSION(:),   POINTER ::SUB
        REAL,   SAVE, DIMENSION(:,:), POINTER ::DP
        REAL,   SAVE, DIMENSION(:,:), POINTER ::DVB
      TYPE GWFSUBTYPE
        INTEGER, POINTER  ::IIBSCB,ITMIN,NNDB,NDB,NMZ,NN,ND2,IDSAVE
        REAL,    POINTER  ::AC1,AC2
        LOGICAL, POINTER  ::NDF,NNDF
        INTEGER, DIMENSION(:),   POINTER ::ISBOCF,ISBOCU
        LOGICAL, DIMENSION(:,:), POINTER ::OCFLGS
        LOGICAL, DIMENSION(:),   POINTER ::OCLAY
        INTEGER, DIMENSION(:),   POINTER ::ILSYS
        INTEGER, DIMENSION(:),   POINTER ::NTSSUM
        INTEGER, DIMENSION(:),   POINTER ::LN
        INTEGER, DIMENSION(:),   POINTER ::LDN
        INTEGER, DIMENSION(:),   POINTER ::NZ
        REAL,    DIMENSION(:),   POINTER ::RNB
        REAL,    DIMENSION(:),   POINTER ::DH
        REAL,    DIMENSION(:),   POINTER ::DHP
        REAL,    DIMENSION(:),   POINTER ::DHC
        REAL,    DIMENSION(:),   POINTER ::DZ
        REAL,    DIMENSION(:),   POINTER ::HC
        REAL,    DIMENSION(:),   POINTER ::SCE
        REAL,    DIMENSION(:),   POINTER ::SCV
        REAL,    DIMENSION(:),   POINTER ::DCOM
        REAL,    DIMENSION(:),   POINTER ::A1
        REAL,    DIMENSION(:),   POINTER ::A2
        REAL,    DIMENSION(:),   POINTER ::BB
        REAL,    DIMENSION(:),   POINTER ::SUB
        REAL,    DIMENSION(:,:), POINTER ::DP
        REAL,    DIMENSION(:,:), POINTER ::DVB
      END TYPE
      TYPE(GWFSUBTYPE), SAVE  ::GWFSUBDAT(10)

      END MODULE GWFSUBMODULE



      SUBROUTINE GWF2SUB7AR(IN,IGRID)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR SUBSIDENCE PACKAGE.
C     READ SUBSIDENCE PACKAGE DATA.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,ISSFLG,NPER,NSTP,HNEW,
     1                      DELR,DELC,BUFF
      USE GWFSUBMODULE,ONLY:IIBSCB,ITMIN,NNDB,NDB,NMZ,NN,ND2,IDSAVE,
     1                      AC1,AC2,NDF,NNDF,ISBOCF,ISBOCU,
     2                      OCFLGS,OCLAY,ILSYS,NTSSUM,LN,LDN,NZ,RNB,
     3                      DH,DHP,DHC,DZ,HC,SCE,SCV,DCOM,A1,A2,BB,
     4                      SUB,DP,DVB
C
      DIMENSION IFL(13)
      CHARACTER*24 ANAME(10)
      CHARACTER*200 LINE
      DATA ANAME(1) /'   PRECONSOLIDATION HEAD'/
      DATA ANAME(2) /'ELASTIC INTERBED STORAGE'/
      DATA ANAME(3) /' VIRGIN INTERBED STORAGE'/
      DATA ANAME(4) /'     STARTING COMPACTION'/
      DATA ANAME(5) /'     DELAY STARTING HEAD'/
      DATA ANAME(6) /'   DELAY PRECOLSOL. HEAD'/
      DATA ANAME(7) /'DELAY INITIAL COMPACTION'/
      DATA ANAME(8) /'DELAY INTERBED THICKNESS'/
      DATA ANAME(9) /'   MATERIAL ZONE INDICES'/
      DATA ANAME(10)/'NUMBER OF BEDS IN SYSTEM'/
      DIMENSION IBUFF(NCOL,NROW)
C     ------------------------------------------------------------------
      ALLOCATE (IIBSCB,ITMIN,NNDB,NDB,NMZ,NN,ND2,IDSAVE)
      ALLOCATE (AC1,AC2)
      ALLOCATE (NDF,NNDF)
      ALLOCATE (ISBOCF(6),ISBOCU(6))
      ZERO=0.0
C
C1------IDENTIFY PACKAGE.
      WRITE(IOUT,*)'SUB:'
!      WRITE(IOUT,1)IN
!    1 FORMAT(/,'SUB7 -- SUBSIDENCE PACKAGE, VERSION 7,',
!     1     ' 03/31/2006',' INPUT READ FROM UNIT',I3)
C
C2------CHECK TO SEE THAT SUBSIDENCE OPTION IS APPROPRIATE
C2------IF INAPPROPRIATE PRINT A MESSAGE & STOP THE SIMULATION.
C2------ALSO, SUM TO GET THE TOTAL NUMBER OF TIME STEPS IN THE
C2------SIMULATION.
C
      NSTPT=0
      DO 12 NS=1,NPER
      NSTPT=NSTPT+NSTP(NS)
      IF(ISSFLG(NS).NE.0.AND.NS.GT.1) THEN
       WRITE(IOUT,10)
   10  FORMAT(1X,'SUBSIDENCE CANNOT BE USED IN SIMULATIONS',
     1  ' IN WHICH STRESS PERIODS OTHER THAN THE ',/,1X,
     2  ' FIRST ARE STEADY-STATE. SIMULATION ABORTED.')
       CALL USTOP(' ')
      ENDIF
 12   CONTINUE
C
C3------ALLOCATE SPACE FOR ARRAY NTSSUM, WHICH WILL CONTAIN THE TOTAL
C3------NUMBER OF TIME STEPS PRIOR TO THE CURRENT TIME STEP.
      ALLOCATE(NTSSUM(NPER))
C
C4------READ FLAG FOR STORING CELL-BY-CELL STORAGE CHANGES AND
C4------FLAG FOR PRINTING AND STORING COMPACTION, SUBSIDENCE, AND
C4------CRITICAL HEAD ARRAYS.
      CALL URDCOM(IN,IOUT,LINE)
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IIBSCB,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISUBOC,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NNDB,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NDB,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NMZ,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NN,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,AC1,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,AC2,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMIN,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDSAVE,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDREST,R,IOUT,IN)
	WRITE(IOUT, *) 'ISUBCB ISUBOC NNDB NDB NMZ NN '
     +  // 'AC1 AC2 ITMIN IDSAVE IDREST:'
	WRITE(IOUT, *) IIBSCB, ISUBOC, NNDB, NDB, NMZ, NN,
     +    AC1, AC2, ITMIN, IDSAVE, IDREST
      IF(AC2.EQ.ZERO) AC2=1.0
      NDF=.TRUE.
      NNDF=.TRUE.
      IF(NNDB.LT.1) THEN
       NNDF=.FALSE.
       NNDB=0
      ENDIF
      IF(NDB.LT.1) THEN
       NDF=.FALSE.
       NDB=0
       NMZ=0
       NN=0
      ENDIF
!      WRITE(IOUT,50) NNDB,NDB,NMZ,NN
!   50 FORMAT(/,'         NUMBER OF SYSTEMS OF NO-DELAYED INTERBEDS:',
!     1 I3,/,'              NUMBER OF SYSTEMS OF DELAY INTERBEDS:',
!     2 I3,/,'                          NUMBER OF MATERIAL ZONES:',
!     3 I3,/,'                    NUMBER OF NODES IN EACH STRING:',I3)
!      IF(IDSAVE.GT.0) THEN
!       WRITE(IOUT,52) IDSAVE
!   52 FORMAT(' RESTART INFORMATION WILL BE SAVED ON UNIT ', I5,
!     1 ' FOR DELAY INTERBEDS')
!      ELSE
!       WRITE(IOUT,53)
!   53 FORMAT(' RESTART INFORMATION WILL NOT BE SAVED FOR DELAY',
!     1 ' INTERBEDS')
!      ENDIF
!      IF(IDREST.GT.0) THEN
!       WRITE(IOUT,54) IDREST
!   54 FORMAT(' RESTART INFORMATION WILL BE READ FROM UNIT ', I5,
!     1 ' FOR DELAY INTERBEDS')
!      ELSE
!       WRITE(IOUT,55)
!   55 FORMAT(' RESTART INFORMATION WILL NOT BE READ FOR DELAY',
!     1 ' INTERBEDS')
!      ENDIF
C
C4A-----ABORT IF NO LAYERS ARE SPECIFIED FOR INTERBED STORAGE
      IF(.NOT.NNDF.AND..NOT.NDF) THEN
       WRITE(IOUT,60)
   60  FORMAT(1X,'NO LAYERS WITH INTERBED STORAGE OF EITHER TYPE ',
     1  'WERE SPECIFIED IN INPUT.',/,1X,'SIMULATION ABORTED.')
       CALL USTOP(' ')
      ENDIF
C4B-----ABORT IF NO PROPERTY ZONES ARE SPECIFIED
      IF(NDF.AND.NMZ.LT.1) THEN
         WRITE(IOUT,*) ' STOPPING-- At least one property zone must ',
     &                 'be specified for delay beds.'
         CALL USTOP(' ')
      ENDIF
C4C-----ABORT IF NOT ENOUGH NODES ARE SPECIFIED
      IF(NDF.AND.NN.LT.2) THEN
         WRITE(IOUT,*) ' STOPPING-- Number of nodes in strings for ',
     &                 'delay beds (NN) should be at least 2.'
         CALL USTOP(' ')
      ENDIF
C
C5------IF CELL-BY-CELL TERMS TO BE SAVED THEN PRINT UNIT NUMBER.
!   70 IF(IIBSCB.GT.0) WRITE(IOUT,80) IIBSCB
!   80 FORMAT(1X,'CELL-BY-CELL FLOW TERMS WILL BE SAVED ON UNIT',I3)
C
C5A-----IF OUTPUT CONTROL FOR PRINTING ARRAYS IS SELECTED PRINT MESSAGE.
!      IF(ISUBOC.GT.0) WRITE(IOUT,90)
!   90 FORMAT(1X,'OUTPUT CONTROL RECORDS FOR SUB PACKAGE WILL BE ',
!     1 'READ EACH TIME STEP.')
C
C6------READ IN MODEL LAYER NUMBERS FOR EACH SYSTEM OF INTERBEDS,
C6------FOR LAYERS WITHOUT DELAY.
      IF(NNDF) THEN
       ALLOCATE(LN(NNDB))
!       WRITE(IOUT,100) NNDB
!  100  FORMAT(/,' MODEL LAYER ASSIGNMENTS FOR EACH OF',I3,' NO-DELAY',
!     1  ' SYSTEMS OF INTERBEDS:')
       CALL URDCOM(IN,IOUT,LINE)
       READ(LINE,*) (LN(N),N=1,NNDB)
       WRITE(IOUT,*) '(LN(N),N=1,NNDB):'
       WRITE(IOUT,*) (LN(N),N=1,NNDB)
!       WRITE(IOUT,115) (LN(N),N=1,NNDB)
!  115  FORMAT(1X,25I4)
       DO 120 N=1,NNDB
       IF(LN(N).GE.1.AND.LN(N).LE.NLAY) GO TO 120
       WRITE(IOUT,118)
  118  FORMAT(/,' IMPROPER LAYER ASSIGNMENT FOR NO-DELAY SYSTEM OF ',
     1  'INTERBEDS.',/,' ABORTING...')
       CALL USTOP(' ')
  120  CONTINUE
      ELSE
       ALLOCATE(LN(1))
      ENDIF
C
C7------READ IN MODEL LAYER NUMBERS FOR EACH SYSTEM OF INTERBEDS,
C7------FOR LAYERS WITH DELAY.
      IF(NDF) THEN
       ALLOCATE(LDN(NDB))
!       WRITE(IOUT,135) NDB
!  135  FORMAT(/,' MODEL LAYER ASSIGNMENTS FOR EACH OF',I3,' DELAY',
!     1  ' SYSTEMS OF INTERBEDS:')
       CALL URDCOM(IN,IOUT,LINE)
       READ(LINE,*) (LDN(N),N=1,NDB)
       WRITE(IOUT,*) '(LDN(N),N=1,NDB):'
       WRITE(IOUT,*) (LDN(N),N=1,NDB)
!       WRITE(IOUT,115) (LDN(N),N=1,NDB)
       DO 140 N=1,NDB
       IF(LDN(N).GE.1.AND.LDN(N).LE.NLAY) GO TO 140
       WRITE(IOUT,138)
  138  FORMAT(/,' IMPROPER LAYER ASSIGNMENT FOR DELAY SYSTEM OF ',
     1  'INTERBEDS.',/,' ABORTING...')
       CALL USTOP(' ')
  140  CONTINUE
      ELSE
       ALLOCATE(LDN(1))
      ENDIF
C
C8------ALLOCATE SPACE FOR THE ARRAYS HC, SCE, SCV, AND SUB.
      NCR=NROW*NCOL
      NND1=NCR*NNDB
      ND1=NCR*NDB
      ND2=0
C
C9-----READ IN ARRAY RNB TO SEE HOW MANY STRINGS OF NN CELLS ARE NEEDED.
      IF(NDF) THEN
       ALLOCATE(RNB(ND1))
       NNSUM=0
       DO 190 KQ=1,NDB
       LOC1 = 1+(KQ-1)*NCR
       LAYNUM=LDN(KQ)
!       WRITE(IOUT,144) KQ
! 144   FORMAT(/,1X,' SYSTEM',I4,' OF DELAY BEDS:')
C       CALL U2DREL(RNB(LOC1),ANAME(10),NROW,NCOL,LAYNUM,IN,IOUT)
       CALL U2DREL(BUFF(:,:,1),ANAME(10),NROW,NCOL,LAYNUM,IN,IOUT)
       CALL GWF2SUB72D1D(BUFF(:,:,1),NCOL,NROW,RNB,ND1,LOC1)
       DO 180 N=1,NCR
       IF(RNB(LOC1+N-1).GE.1.0) NNSUM=NNSUM+1
  180  CONTINUE
  190  CONTINUE
       ND2=NN*NNSUM
      ELSE
       ALLOCATE(RNB(1))
      ENDIF
      IF(ND2.LT.1.AND.NDF) THEN
         WRITE(IOUT,*) ' STOPPING-- Delay beds were not found in ',
     &                 ' array specifying numbers of delay beds (RNB).'
         CALL USTOP(' ')
      ENDIF
C
C10-----ALLOCATE MEMORY.
      ALLOCATE(OCFLGS(13,NSTPT))
      ALLOCATE(OCLAY(NLAY))
      IF(NNDF) THEN
         ALLOCATE(HC(NND1))
         ALLOCATE(SCE(NND1))
         ALLOCATE(SCV(NND1))
         ALLOCATE(SUB(NND1))
         ALLOCATE(ILSYS(NNDB))
      ELSE
         ALLOCATE(HC(1))
         ALLOCATE(SCE(1))
         ALLOCATE(SCV(1))
         ALLOCATE(SUB(1))
         ALLOCATE(ILSYS(1))
      ENDIF
      IF(NDF) THEN
         ALLOCATE(NZ(ND1))
         ALLOCATE(DZ(ND1))
         ALLOCATE(DCOM(ND1))
         ALLOCATE(DHP(ND2))
         ALLOCATE(DH(ND2))
         ALLOCATE(DHC(ND2))
         ALLOCATE(DP(NMZ,3))
         ALLOCATE(DVB(NDB,4))
         ALLOCATE(A1(NN))
         ALLOCATE(A2(NN))
         ALLOCATE(BB(NN))
      ELSE
         ALLOCATE(NZ(1))
         ALLOCATE(DZ(1))
         ALLOCATE(DCOM(1))
         ALLOCATE(DHP(1))
         ALLOCATE(DH(1))
         ALLOCATE(DHC(1))
         ALLOCATE(DP(1,1))
         ALLOCATE(DVB(1,1))
         ALLOCATE(A1(1))
         ALLOCATE(A2(1))
         ALLOCATE(BB(1))
      ENDIF
C
C11-----READ ARRAYS.
      NCR=NROW*NCOL
      ANNI=0.5/(FLOAT(NN)-.5)
C
C12-----READ RESTART RECORDS IF THIS SIMULATION CONTINUES FROM A
C12-----PREVIOUS SIMULATION
      IF(NDF) THEN
       IF(IDREST.GT.0) THEN
        READ(IDREST) NND2
        IF(NND2.EQ.ND2) THEN
         WRITE(IOUT,242)
  242    FORMAT(' HEAD AND PRECONSOLIDATION HEAD FOR DELAY BEDS ARE',
     1   ' BEING READ FROM RESTART RECORDS')
         READ(IDREST) (DH(N),N=1,ND2)
         READ(IDREST) (DHC(N),N=1,ND2)
         DO 250 N2=1,ND2
         DHP(N2)=DH(N2)
  250    CONTINUE
        ELSE
         WRITE(IOUT,252)
  252    FORMAT(' HEAD AND PRECONSOLIDATION HEAD FOR DELAY BEDS ',
     1   'CANNOT BE READ FROM RESTART RECORDS',/,
     2   ' SIMULATION ABORTING')
         CALL USTOP(' ')
        ENDIF
       ENDIF
      ENDIF
C
C13-----READ IN ARRAYS FOR SYSTEMS OF NO-DELAY INTERBEDS.
      IF(NNDF) THEN
       DO 260 KQ=1,NNDB
       K=LN(KQ)
       LOC1=1+(KQ-1)*NCR
!       WRITE(IOUT,256) KQ
!  256  FORMAT(/,1X,' SYSTEM',I4,' OF NO-DELAY BEDS:')
C       CALL U2DREL(HC(LOC1),ANAME(1),NROW,NCOL,K,IN,IOUT)
       CALL U2DREL(BUFF(:,:,1),ANAME(1),NROW,NCOL,K,IN,IOUT)
       CALL GWF2SUB72D1D(BUFF(:,:,1),NCOL,NROW,HC,NND1,LOC1)
!       WRITE(IOUT,256) KQ
C       CALL U2DREL(SCE(LOC1),ANAME(2),NROW,NCOL,K,IN,IOUT)
       CALL U2DREL(BUFF(:,:,1),ANAME(2),NROW,NCOL,K,IN,IOUT)
       CALL GWF2SUB72D1D(BUFF(:,:,1),NCOL,NROW,SCE,NND1,LOC1)
!       WRITE(IOUT,256) KQ
C       CALL U2DREL(SCV(LOC1),ANAME(3),NROW,NCOL,K,IN,IOUT)
       CALL U2DREL(BUFF(:,:,1),ANAME(3),NROW,NCOL,K,IN,IOUT)
       CALL GWF2SUB72D1D(BUFF(:,:,1),NCOL,NROW,SCV,NND1,LOC1)
!       WRITE(IOUT,256) KQ
C       CALL U2DREL(SUB(LOC1),ANAME(4),NROW,NCOL,K,IN,IOUT)
       CALL U2DREL(BUFF(:,:,1),ANAME(4),NROW,NCOL,K,IN,IOUT)
       CALL GWF2SUB72D1D(BUFF(:,:,1),NCOL,NROW,SUB,NND1,LOC1)
  260  CONTINUE
C
C14-----INITIALIZE ARRAYS FOR SYSTEMS OF NO-DELAY INTERBEDS.
       DO 280 KQ=1,NNDB
       K=LN(KQ)
       NQ=(KQ-1)*NCR
C       NK=(K-1)*NCR
       DO 270 IR=1,NROW
       NQR=NQ+(IR-1)*NCOL
C       NKR=NK+(IR-1)*NCOL
       DO 270 IC=1,NCOL
       LOC2=NQR+IC
C       LOC2H=NKR+IC
C
C15------MULTIPLY STORAGE BY AREA TO GET STORAGE CAPACITY.
       AREA=DELR(IC)*DELC(IR)
       SCE(LOC2)=SCE(LOC2)*AREA
       SCV(LOC2)=SCV(LOC2)*AREA
C
C16-----MAKE SURE THAT PRECONSOLIDATION HEAD VALUES
C16-----ARE CONSISTANT WITH STARTING HEADS.
       IF(HC(LOC2).GT.HNEW(IC,IR,K)) HC(LOC2)=HNEW(IC,IR,K)
  270  CONTINUE
  280  CONTINUE
      ENDIF
      IF(NDF) THEN
C
C17-----READ IN TABLE OF MATERIAL PROPERTIES: K, Sse, Ssv FOR EACH
C17-----OF NMZ ZONES.
!       WRITE(IOUT,295)
!  295 FORMAT(/,' MATERIAL PROPERTIES OF INTERBEDS WITH DELAY PROPERTIES'
!     1 ,//,'   ZONE        HYDRAULIC           ELASTIC            INEL',
!     2 'ASTIC       ',/,'  NUMBER      CONDUCTIVITY     SPECIFIC STORA',
!     3 'GE    SPECIFIC STORAGE   ',/,' ',69('-'))
       WRITE(IOUT,*) '(DP(N,NP),NP=1,3):'
       DO 300 N=1,NMZ
       READ(IN,*) (DP(N,NP),NP=1,3)
       WRITE(IOUT,*) (DP(N,NP),NP=1,3)
  300  CONTINUE
!       WRITE(IOUT,305) (N,(DP(N,NP),NP=1,3),N=1,NMZ)
!  305  FORMAT(I5,4X,G15.5,5X,G15.5,5X,G15.5)
       LOC3=0
       LOC4=0
       DO 380 KQ=1,NDB
       K=LDN(KQ)
       LOC1=1+(KQ-1)*NCR
C
C18-----READ IN ARRAYS FOR SYSTEMS OF DELAY INTERBEDS.
       IF(IDREST.LE.0) THEN
!        WRITE(IOUT,308) KQ
! 308    FORMAT(/,1X,' SYSTEM',I4,' OF DELAY BEDS:')
        CALL U2DREL(BUFF(:,:,1),ANAME(5),NROW,NCOL,K,IN,IOUT)
        N1=0
        DO 320 IR=1,NROW
        DO 320 IC=1,NCOL
        N1=N1+1
        LOC2=LOC1+N1-1
        IF(RNB(LOC2).LT.1.0) GO TO 320
        DO 315 N2=1,NN
        LOC3=LOC3+1
        DHP(LOC3)=BUFF(IC,IR,1)
        DH(LOC3)=BUFF(IC,IR,1)
  315   CONTINUE
  320   CONTINUE
!        WRITE(IOUT,308) KQ
        CALL U2DREL(BUFF(:,:,1),ANAME(6),NROW,NCOL,K,IN,IOUT)
        N1=0
        DO 330 IR=1,NROW
        DO 330 IC=1,NCOL
        N1=N1+1
        LOC2=LOC1+N1-1
        IF(RNB(LOC2).LT.1.0) GO TO 330
        DO 325 N2=1,NN
        LOC4=LOC4+1
        DHC(LOC4)=BUFF(IC,IR,1)
        IF(DHC(LOC4).GT.DH(LOC4)) DHC(LOC4)=DH(LOC4)
  325   CONTINUE
  330   CONTINUE
       ENDIF
!       WRITE(IOUT,308) KQ
C       CALL U2DREL(DCOM(LOC1),ANAME(7),NROW,NCOL,K,IN,IOUT)
       CALL U2DREL(BUFF(:,:,1),ANAME(7),NROW,NCOL,K,IN,IOUT)
       CALL GWF2SUB72D1D(BUFF(:,:,1),NCOL,NROW,DCOM,ND1,LOC1)
!       WRITE(IOUT,308) KQ
C       CALL U2DREL(DZ(LOC1),ANAME(8),NROW,NCOL,K,IN,IOUT)
       CALL U2DREL(BUFF(:,:,1),ANAME(8),NROW,NCOL,K,IN,IOUT)
       CALL GWF2SUB72D1D(BUFF(:,:,1),NCOL,NROW,DZ,ND1,LOC1)
!       WRITE(IOUT,308) KQ
C       CALL U2DINT(NZ(LOC1),ANAME(9),NROW,NCOL,K,IN,IOUT)
       CALL U2DINT(IBUFF,ANAME(9),NROW,NCOL,K,IN,IOUT)
       L=LOC1-1
       DO 335 I=1,NROW
       DO 335 J=1,NCOL
       L=L+1
       NZ(L)=IBUFF(J,I)
 335   CONTINUE
C
C19-----INITIALIZE ARRAYS FOR SYSTEMS OF DELAY INTERBEDS.
       DO 360 NL=1,NCR
       LOC2=LOC1+NL-1
       IF(RNB(LOC2).GE.1.0.AND.DZ(LOC2).LE.ZERO) THEN
          WRITE(IOUT,355)
 355      FORMAT(' A VALUE OF ZERO WAS FOUND IN THE DZ ARRAY WHERE ',
     1    'DELAY INTERBEDS OCCUR.',/,' MAKE SURE THAT',
     2    ' DZ IS GREATER THAN 0.0 AT ALL CELLS WHERE RNB ',/,
     3    ' IS 1.0 OR MORE. SIMULATION ABORTING')
          CALL USTOP(' ')
       ENDIF
       DZ(LOC2)=DZ(LOC2)*ANNI
  360  CONTINUE
       DO 370 N=1,4
       DVB(KQ,N)=ZERO
  370  CONTINUE
  380  CONTINUE
      ENDIF
C
C20-----SET ALL FLAGS FOR OUTPUT CONTROL TO "FALSE".
      DO 390 I=1,NSTPT
      DO 385 N=1,13
      OCFLGS(N,I)=.FALSE.
  385 CONTINUE
  390 CONTINUE
C the following initialization of the NTSSUM array was removed from the
C block IF construct immediatly below and placed here so that it would
C be executed even if ISUBOC is not greater than zero.
C Stan Leake, July 14, 2010
       NTSSUM(1)=0
       IF(NPER.GT.1) THEN
        DO 415 N=2,NPER
        NTSSUM(N)=NTSSUM(N-1)+NSTP(N-1)
  415   CONTINUE
C
C21-----READ FORMATS AND UNIT NUMBERS OUTPUT FLAGS.
      IF(ISUBOC.GT.0) THEN
       CALL URDCOM(IN,IOUT,LINE)
       LLOC=1
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISBOCF(1),R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISBOCU(1),R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISBOCF(2),R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISBOCU(2),R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISBOCF(3),R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISBOCU(3),R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISBOCF(4),R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISBOCU(4),R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISBOCF(5),R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISBOCU(5),R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISBOCF(6),R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISBOCU(6),R,IOUT,IN)
	 WRITE(IOUT,*)
     +   'Ifm1 Iun1 Ifm2 Iun2 Ifm3 Iun3 Ifm4 Iun4 Ifm5 Iun5 Ifm6 Iun6:'
       WRITE(IOUT,*) (ISBOCF(N),ISBOCU(N),N=1,6)
!       WRITE(IOUT,410) (ISBOCF(N),ISBOCU(N),N=1,6)
!  410  FORMAT(/,'             SUBSIDENCE PRINT FORMAT IS NUMBER',I4/
!     &            '                 UNIT FOR SAVING SUBSIDENCE IS',I4/
!     &            '    COMPACTION BY LAYER PRINT FORMAT IS NUMBER',I4/
!     &            '        UNIT FOR SAVING COMPACTION BY LAYER IS',I4/
!     &            '   COMPACTION BY SYSTEM PRINT FORMAT IS NUMBER',I4/
!     &            '       UNIT FOR SAVING COMPACTION BY SYSTEM IS',I4/
!     &            '  VERTICAL DISPLACEMENT PRINT FORMAT IS NUMBER',I4/
!     &            '      UNIT FOR SAVING VERTICAL DISPLACEMENT IS',I4/
!     &            ' NO-DELAY CRITICAL HEAD PRINT FORMAT IS NUMBER',I4/
!     &            '     UNIT FOR SAVING NO-DELAY CRITICAL HEAD IS',I4/
!     &            '    DELAY CRITICAL HEAD PRINT FORMAT IS NUMBER',I4/
!     &            '        UNIT FOR SAVING DELAY CRITICAL HEAD IS',I4)
       ENDIF
	 WRITE(IOUT,*) 'ISP1, ISP2, JTS1, JTS2, (IFL(II), II=1,13):'
       DO 450 NOCLIN=1,ISUBOC
       CALL URDCOM(IN,IOUT,LINE)
       LLOC=1
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISP1,R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISP2,R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,JTS1,R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,JTS2,R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFL(1),R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFL(2),R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFL(3),R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFL(4),R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFL(5),R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFL(6),R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFL(7),R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFL(8),R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFL(9),R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFL(10),R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFL(11),R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFL(12),R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFL(13),R,IOUT,IN)
	 WRITE(IOUT,*) ISP1, ISP2, JTS1, JTS2, (IFL(II), II=1,13)
       IF(ISP1.LT.1) ISP1=1
       IF(ISP1.GT.NPER) ISP1=NPER
       IF(ISP2.LT.1) ISP2=1
       IF(ISP2.GT.NPER) ISP2=NPER
       IF(ISP1.GT.ISP2) ISP1=ISP2
       DO 440 I=ISP1,ISP2
       J1=JTS1
       J2=JTS2
       IF(J1.LT.1) J1=1
       IF(J1.GT.NSTP(I)) J1=NSTP(I)
       IF(J2.LT.1) J2=1
       IF(J2.GT.NSTP(I)) J2=NSTP(I)
       IF(J1.GT.J2) J1=J2
       DO 430 J=J1,J2
       ILOC=NTSSUM(I)+J
       DO 420 N=1,13
       IF(IFL(N).GT.0) OCFLGS(N,ILOC)=.TRUE.
       IF(IFL(N).EQ.0) OCFLGS(N,ILOC)=.FALSE.
  420  CONTINUE
  430  CONTINUE
  440  CONTINUE
  450  CONTINUE
      ENDIF
C
C22-----RETURN
  500 CALL SGWF2SUB7PSV(IGRID)
      RETURN
      END
!      SUBROUTINE GWF2SUB7ST(KPER,IGRID)
C     ******************************************************************
C        SET PRECONSOLIDATION HEAD (HC AND DHC) EQUAL TO THE STEADY-
C        STATE HEAD IF HEAD IS LOWER THAN PRECONSOLIDATION HEAD.
C     ******************************************************************
!      SUBROUTINE GWF2SUB7FM(KPER,KITER,ISIP,IGRID)
C     ******************************************************************
C        ADD INTERBED STORAGE TERMS TO RHS AND HCOF
C     ******************************************************************
!      SUBROUTINE GWF2SUB7BD(KSTP,KPER,IGRID)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR INTERBED STORAGE
C     ******************************************************************
!      SUBROUTINE GWF2SUB7OT(KSTP,KPER,IN,IGRID)
C     ******************************************************************
C     PRINT AND STORE SUBSIDENCE, COMPACTION AND CRITICAL HEAD.
C     ******************************************************************
!      SUBROUTINE SGWF2SUB7A(HAQ,TLED,CI,SSE,SSV,DZ,DH,DHP,DHC,NN)
C     ******************************************************************
C        ASSEMBLE COEFFICIENTS FOR SOLVING FOR HEAD DISTRIBUTION
C        IN ONE STRING OF CELLS REPRESENTING ONE-HALF OF A DOUBLY
C        DRAINING INTERBED
C     ******************************************************************
!      SUBROUTINE SGWF2SUB7S(NN)
C     ******************************************************************
C        SOLVE SYSTEM OF EQUATIONS WITH A SYMMETRICAL TRI-DIAGONAL
C        COEFFICIENT MATRIX
      SUBROUTINE GWF2SUB72D1D(BUFF,NCOL,NROW,D,ND,LOC)
C     ******************************************************************
C     Move 2-D array into 1-D array
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION BUFF(NCOL,NROW),D(ND)
C     ------------------------------------------------------------------
      L=LOC-1
      DO 10 I=1,NROW
      DO 10 J=1,NCOL
      L=L+1
      D(L)=BUFF(J,I)
   10 CONTINUE
      RETURN
      END
C     ******************************************************************
!      SUBROUTINE GWF2SUB7SV(IGRID)
C     ******************************************************************
C     SAVE INTERBED STORAGE DATA FOR FUTURE RESTART
C     ******************************************************************

      SUBROUTINE GWF2SUB7DA(IGRID)
C
C     ******************************************************************
C     DEALLOCATE DYNAMIC STORAGE FOR SUB PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFSUBMODULE
C     ------------------------------------------------------------------
C
      DEALLOCATE (GWFSUBDAT(IGRID)%IIBSCB)
      DEALLOCATE (GWFSUBDAT(IGRID)%ITMIN)
      DEALLOCATE (GWFSUBDAT(IGRID)%NNDB)
      DEALLOCATE (GWFSUBDAT(IGRID)%NDB)
      DEALLOCATE (GWFSUBDAT(IGRID)%NMZ)
      DEALLOCATE (GWFSUBDAT(IGRID)%NN)
      DEALLOCATE (GWFSUBDAT(IGRID)%ND2)
      DEALLOCATE (GWFSUBDAT(IGRID)%IDSAVE)
      DEALLOCATE (GWFSUBDAT(IGRID)%AC1)
      DEALLOCATE (GWFSUBDAT(IGRID)%AC2)
      DEALLOCATE (GWFSUBDAT(IGRID)%NDF)
      DEALLOCATE (GWFSUBDAT(IGRID)%NNDF)
      DEALLOCATE (GWFSUBDAT(IGRID)%ISBOCF)
      DEALLOCATE (GWFSUBDAT(IGRID)%ISBOCU)
      DEALLOCATE (GWFSUBDAT(IGRID)%OCFLGS)
      DEALLOCATE (GWFSUBDAT(IGRID)%OCLAY)
      DEALLOCATE (GWFSUBDAT(IGRID)%ILSYS)
      DEALLOCATE (GWFSUBDAT(IGRID)%NTSSUM)
      DEALLOCATE (GWFSUBDAT(IGRID)%LN)
      DEALLOCATE (GWFSUBDAT(IGRID)%LDN)
      DEALLOCATE (GWFSUBDAT(IGRID)%NZ)
      DEALLOCATE (GWFSUBDAT(IGRID)%RNB)
      DEALLOCATE (GWFSUBDAT(IGRID)%DH)
      DEALLOCATE (GWFSUBDAT(IGRID)%DHP)
      DEALLOCATE (GWFSUBDAT(IGRID)%DHC)
      DEALLOCATE (GWFSUBDAT(IGRID)%DZ)
      DEALLOCATE (GWFSUBDAT(IGRID)%HC)
      DEALLOCATE (GWFSUBDAT(IGRID)%SCE)
      DEALLOCATE (GWFSUBDAT(IGRID)%SCV)
      DEALLOCATE (GWFSUBDAT(IGRID)%DCOM)
      DEALLOCATE (GWFSUBDAT(IGRID)%A1)
      DEALLOCATE (GWFSUBDAT(IGRID)%A2)
      DEALLOCATE (GWFSUBDAT(IGRID)%BB)
      DEALLOCATE (GWFSUBDAT(IGRID)%SUB)
      DEALLOCATE (GWFSUBDAT(IGRID)%DP)
      DEALLOCATE (GWFSUBDAT(IGRID)%DVB)

C2-----RETURN
      RETURN
      END
      SUBROUTINE SGWF2SUB7PNT(IGRID)
C  Change SUB data to a different grid.
      USE GWFSUBMODULE
C
      IIBSCB=>GWFSUBDAT(IGRID)%IIBSCB
      ITMIN=>GWFSUBDAT(IGRID)%ITMIN
      NNDB=>GWFSUBDAT(IGRID)%NNDB
      NDB=>GWFSUBDAT(IGRID)%NDB
      NMZ=>GWFSUBDAT(IGRID)%NMZ
      NN=>GWFSUBDAT(IGRID)%NN
      ND2=>GWFSUBDAT(IGRID)%ND2
      IDSAVE=>GWFSUBDAT(IGRID)%IDSAVE
      AC1=>GWFSUBDAT(IGRID)%AC1
      AC2=>GWFSUBDAT(IGRID)%AC2
      NDF=>GWFSUBDAT(IGRID)%NDF
      NNDF=>GWFSUBDAT(IGRID)%NNDF
      ISBOCF=>GWFSUBDAT(IGRID)%ISBOCF
      ISBOCU=>GWFSUBDAT(IGRID)%ISBOCU
      OCFLGS=>GWFSUBDAT(IGRID)%OCFLGS
      OCLAY=>GWFSUBDAT(IGRID)%OCLAY
      ILSYS=>GWFSUBDAT(IGRID)%ILSYS
      NTSSUM=>GWFSUBDAT(IGRID)%NTSSUM
      LN=>GWFSUBDAT(IGRID)%LN
      LDN=>GWFSUBDAT(IGRID)%LDN
      NZ=>GWFSUBDAT(IGRID)%NZ
      RNB=>GWFSUBDAT(IGRID)%RNB
      DH=>GWFSUBDAT(IGRID)%DH
      DHP=>GWFSUBDAT(IGRID)%DHP
      DHC=>GWFSUBDAT(IGRID)%DHC
      DZ=>GWFSUBDAT(IGRID)%DZ
      HC=>GWFSUBDAT(IGRID)%HC
      SCE=>GWFSUBDAT(IGRID)%SCE
      SCV=>GWFSUBDAT(IGRID)%SCV
      DCOM=>GWFSUBDAT(IGRID)%DCOM
      A1=>GWFSUBDAT(IGRID)%A1
      A2=>GWFSUBDAT(IGRID)%A2
      BB=>GWFSUBDAT(IGRID)%BB
      SUB=>GWFSUBDAT(IGRID)%SUB
      DP=>GWFSUBDAT(IGRID)%DP
      DVB=>GWFSUBDAT(IGRID)%DVB
C
      RETURN
      END
      SUBROUTINE SGWF2SUB7PSV(IGRID)
C  Save SUB data for a grid.
      USE GWFSUBMODULE
C
      GWFSUBDAT(IGRID)%IIBSCB=>IIBSCB
      GWFSUBDAT(IGRID)%ITMIN=>ITMIN
      GWFSUBDAT(IGRID)%NNDB=>NNDB
      GWFSUBDAT(IGRID)%NDB=>NDB
      GWFSUBDAT(IGRID)%NMZ=>NMZ
      GWFSUBDAT(IGRID)%NN=>NN
      GWFSUBDAT(IGRID)%ND2=>ND2
      GWFSUBDAT(IGRID)%IDSAVE=>IDSAVE
      GWFSUBDAT(IGRID)%AC1=>AC1
      GWFSUBDAT(IGRID)%AC2=>AC2
      GWFSUBDAT(IGRID)%NDF=>NDF
      GWFSUBDAT(IGRID)%NNDF=>NNDF
      GWFSUBDAT(IGRID)%ISBOCF=>ISBOCF
      GWFSUBDAT(IGRID)%ISBOCU=>ISBOCU
      GWFSUBDAT(IGRID)%OCFLGS=>OCFLGS
      GWFSUBDAT(IGRID)%OCLAY=>OCLAY
      GWFSUBDAT(IGRID)%ILSYS=>ILSYS
      GWFSUBDAT(IGRID)%NTSSUM=>NTSSUM
      GWFSUBDAT(IGRID)%LN=>LN
      GWFSUBDAT(IGRID)%LDN=>LDN
      GWFSUBDAT(IGRID)%NZ=>NZ
      GWFSUBDAT(IGRID)%RNB=>RNB
      GWFSUBDAT(IGRID)%DH=>DH
      GWFSUBDAT(IGRID)%DHP=>DHP
      GWFSUBDAT(IGRID)%DHC=>DHC
      GWFSUBDAT(IGRID)%DZ=>DZ
      GWFSUBDAT(IGRID)%HC=>HC
      GWFSUBDAT(IGRID)%SCE=>SCE
      GWFSUBDAT(IGRID)%SCV=>SCV
      GWFSUBDAT(IGRID)%DCOM=>DCOM
      GWFSUBDAT(IGRID)%A1=>A1
      GWFSUBDAT(IGRID)%A2=>A2
      GWFSUBDAT(IGRID)%BB=>BB
      GWFSUBDAT(IGRID)%SUB=>SUB
      GWFSUBDAT(IGRID)%DP=>DP
      GWFSUBDAT(IGRID)%DVB=>DVB
C
      RETURN
      END
