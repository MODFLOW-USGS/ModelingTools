      MODULE GWFHUFMODULE
        INTEGER, SAVE,   POINTER ::IHUFCB,NHUF,NPHUF,IWETIT,IHDWET,
     1                             IOHUFHDS,IOHUFFLWS
        REAL,    SAVE,   POINTER ::WETFCT
        CHARACTER(LEN=10),SAVE, POINTER, DIMENSION(:)     ::HGUNAM    
        INTEGER, SAVE,   POINTER, DIMENSION(:)     ::LTHUF
        INTEGER, SAVE,   POINTER, DIMENSION(:)     ::LAYWT
        INTEGER, SAVE,   POINTER, DIMENSION(:,:)   ::IHGUFLG
        REAL,    SAVE,   POINTER, DIMENSION(:)     ::HGUHANI
        REAL,    SAVE,   POINTER, DIMENSION(:)     ::HGUVANI
        REAL,    SAVE,   POINTER, DIMENSION(:)     ::HUFHK
        REAL,    SAVE,   POINTER, DIMENSION(:)     ::HUFVK
        REAL,    SAVE,   POINTER, DIMENSION(:)     ::HUFSS
        REAL,    SAVE,   POINTER, DIMENSION(:)     ::HUFSY
        REAL,    SAVE,   POINTER, DIMENSION(:)     ::HUFHANI
        REAL,    SAVE,   POINTER, DIMENSION(:)     ::HUFKDEP
        REAL,    SAVE,   POINTER, DIMENSION(:,:)   ::GS,SC2HUF
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::VKAH
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::SC1
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::WETDRY
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::HK
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::HKCC
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::HUFTMP
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::VDHD
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:,:) ::HUFTHK
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:,:) ::VDHT
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:,:) ::A9
      TYPE GWFHUFTYPE
        INTEGER, POINTER ::IHUFCB,NHUF,NPHUF,IWETIT,IHDWET,
     1                     IOHUFHDS,IOHUFFLWS 
        REAL, POINTER    ::WETFCT
        CHARACTER(LEN=10), POINTER, DIMENSION(:)     ::HGUNAM    
        INTEGER,   POINTER, DIMENSION(:)     ::LTHUF
        INTEGER,   POINTER, DIMENSION(:)     ::LAYWT
        INTEGER,   POINTER, DIMENSION(:,:)   ::IHGUFLG
        REAL,      POINTER, DIMENSION(:)     ::HGUHANI
        REAL,      POINTER, DIMENSION(:)     ::HGUVANI
        REAL,      POINTER, DIMENSION(:)     ::HUFHK
        REAL,      POINTER, DIMENSION(:)     ::HUFVK
        REAL,      POINTER, DIMENSION(:)     ::HUFSS
        REAL,      POINTER, DIMENSION(:)     ::HUFSY
        REAL,      POINTER, DIMENSION(:)     ::HUFHANI
        REAL,      POINTER, DIMENSION(:)     ::HUFKDEP
        REAL,      POINTER, DIMENSION(:,:)   ::GS,SC2HUF
        REAL,      POINTER, DIMENSION(:,:,:) ::VKAH
        REAL,      POINTER, DIMENSION(:,:,:) ::SC1
        REAL,      POINTER, DIMENSION(:,:,:) ::WETDRY
        REAL,      POINTER, DIMENSION(:,:,:) ::HK
        REAL,      POINTER, DIMENSION(:,:,:) ::HKCC
        REAL,      POINTER, DIMENSION(:,:,:) ::HUFTMP
        REAL,      POINTER, DIMENSION(:,:,:) ::VDHD
        REAL,      POINTER, DIMENSION(:,:,:,:) ::HUFTHK
        REAL,      POINTER, DIMENSION(:,:,:,:) ::VDHT
        REAL,      POINTER, DIMENSION(:,:,:,:) ::A9
      END TYPE
      TYPE(GWFHUFTYPE) GWFHUFDAT(10)
      END MODULE GWFHUFMODULE

! Time of File Save by ERB: 7/16/2004 5:20PM
      SUBROUTINE GWF2HUF7AR(IN,ILVDA,IKDEP,IGRID)
C
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR HYDROGEOLOGIC UNIT PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,BOTM,ITRSS,LAYHDT,LAYHDS,
     1                      IOUT,ISSFLG,NPER,IBOUND,LBOTM
      USE GWFBASMODULE,ONLY:HDRY
      USE GWFHUFMODULE,ONLY:IHUFCB,NHUF,NPHUF,IWETIT,IHDWET,IOHUFHDS,
     1                      IOHUFFLWS,WETFCT,HGUNAM,LTHUF,LAYWT,
     2                      IHGUFLG,HGUHANI,HGUVANI,HUFHK,HUFVK,HUFSS,
     3                      HUFSY,HUFHANI,HUFKDEP,GS,VKAH,SC1,WETDRY,HK,
     4                      HKCC,HUFTMP,VDHD,HUFTHK,VDHT,A9,SC2HUF 

      CHARACTER*10 TMPNAM,CTMP1
      CHARACTER*14 LAYPRN(5),TYPNAM(2),VKANAM(2),WETNAM(2),HANNAM,
     &             LAYPRN2 
      DATA TYPNAM/'      CONFINED','   CONVERTIBLE'/
      DATA VKANAM/'    VERTICAL K','    ANISOTROPY'/
      DATA WETNAM/'  NON-WETTABLE','      WETTABLE'/
      DATA HANNAM/'      VARIABLE'/
C
      INTEGER          ::IFLG(5)
C
      CHARACTER*200 LINE
      CHARACTER*24 ANAME(8)
      CHARACTER*4 PTYP
      DOUBLE PRECISION HN
C
      DATA ANAME(1) /'   HYD. COND. ALONG ROWS'/
      DATA ANAME(2) /'  HORIZ. ANI. (COL./ROW)'/
      DATA ANAME(3) /'     VERTICAL HYD. COND.'/
      DATA ANAME(4) /' HORIZ. TO VERTICAL ANI.'/
      DATA ANAME(5) /'QUASI3D VERT. HYD. COND.'/
      DATA ANAME(6) /'        SPECIFIC STORAGE'/
      DATA ANAME(7) /'          SPECIFIC YIELD'/
      DATA ANAME(8) /'        WETDRY PARAMETER'/

C     ------------------------------------------------------------------
      ALLOCATE(IHUFCB,NHUF,NPHUF,IWETIT,IHDWET,IOHUFHDS,
     &         IOHUFFLWS)
      ALLOCATE(WETFCT)
C
C1------IDENTIFY PACKAGE
      WRITE(IOUT, *) 'HUF2:'
!      WRITE(IOUT,1) IN
!    1 FORMAT(1X,/1X,'HUF2 -- HYDROGEOLOGIC-UNIT FLOW PACKAGE,
!     & VERSION 7.0, 05/05/2005',/,' INPUT READ FROM UNIT',I3,/)
C
C2------READ FIRST RECORD AND WRITE
      CALL URDCOM(IN,IOUT,LINE)
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHUFCB,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,HDRY,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NHUF,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPHUF,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IOHUFHDS,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IOHUFFLWS,R,-1,IN)
!      IF(IHUFCB.LT.0) WRITE(IOUT,8)
!    8 FORMAT(1X,'CONSTANT-HEAD CELL-BY-CELL FLOWS WILL BE PRINTED',
!     &  ' WHEN ICHUFL IS NOT 0')
!      IF(IHUFCB.GT.0) WRITE(IOUT,9) IHUFCB
!    9 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT',I3)
!      WRITE(IOUT,11) HDRY
!   11 FORMAT(1X,'HEAD AT CELLS THAT CONVERT TO DRY=',1PG13.5)
!      if(NHUF.GT.0) WRITE(IOUT,12) NHUF,NPHUF
!   12 format(1x,'Hydrogeologic-Unit Flow Package Active with ',i3,
!     &  ' units and ',i3,' parameters')
!      IF(IOHUFHDS.GT.0) WRITE(IOUT,15) IOHUFHDS
!   15 FORMAT(1X,'HEADS IN HYDROGEOLOGIC UNITS WILL BE SAVED',
!     &  ' ON UNIT',I3)
      LINELEN=LEN(LINE)
      IF(LINE(LINELEN:LINELEN).EQ.'E'.OR.ISTART.EQ.LINELEN) THEN
        IOHUFFLWS = 0
!        WRITE(IOUT,17)
!   17   FORMAT(/1X,'IOHUFFLWS NOT FOUND AND WILL BE SET TO BE ZERO',/)
      ENDIF
 !     IF(IOHUFFLWS.GT.0) WRITE(IOUT,18) IOHUFFLWS
 !  18 FORMAT(1X,'FLOWS IN HYDROGEOLOGIC UNITS WILL BE SAVED',
 !    &  ' ON UNIT',I3)
	WRITE(IOUT, *) 'IHUFCB, HDRY, NHUF, NPHUF, IOHUFHDS, IOHUFFLWS:'
	WRITE(IOUT, *) IHUFCB, HDRY, NHUF, NPHUF, IOHUFHDS, IOHUFFLWS
C
C  Allocate space in parameter-information arrays
      CALL UPARARRAL(0,IOUT,LINE,NPHUF)
   
C-------CHECK FOR LVDA 
      NPLVDA=0
      IF(ILVDA .NE. 0)THEN
	  WRITE(IOUT, *) 'LVDA:'
C-------IDENTIFY PACKAGE
!        WRITE(IOUT,21) ILVDA 
!   21   FORMAT(1X,/1X,'LVDA1 -- MODEL-LAYER VARIABLE-DIRECTION
!     &  HORIZONTAL ANISOTROPY CAPABILITY,
!     &  VERSION 7.0, 05/05/2005',/,' INPUT READ FROM UNIT',I3,/)
C
C2------READ FIRST RECORD AND WRITE
        CALL URDCOM(ILVDA,IOUT,LINE)
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPLVDA,R,IOUT,ILVDA)
C
C  Allocate space in parameter-information arrays
	  WRITE(IOUT, *) 'NPLVDA:'
	  WRITE(IOUT, *) NPLVDA
        CALL UPARARRAL(0,IOUT,LINE,NPLVDA)
      ENDIF
C
C-------CHECK FOR KDEP 
      IFKDEP=0
      NPKDEP=0
      IF(IKDEP .NE. 0)THEN
C-------IDENTIFY PACKAGE
        WRITE(IOUT,*) 'KDEP:'
!        WRITE(IOUT,31) IKDEP
!   31 FORMAT(1X,/1X,'KDEP1 -- HYDRAULIC CONDUCTIVITY DEPTH-DECAY
!     &   VERSION 7.0, 05/05/2005',/,' INPUT READ FROM UNIT',I3,/)
C
C-------READ FIRST RECORD AND WRITE
        CALL URDCOM(IKDEP,IOUT,LINE)
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPKDEP,R,IOUT,IKDEP)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFKDEP,R,IOUT,IKDEP)
	  WRITE(IOUT, *) 'NPKDEP, IFKDEP:'
	  WRITE(IOUT, *) NPKDEP, IFKDEP
C
C  Allocate space in parameter-information arrays
        CALL UPARARRAL(0,IOUT,LINE,NPKDEP)
      ENDIF
      WRITE(IOUT, *) 'HUF2:'
C
C4------READ LTHUF, LAYWT.
      ALLOCATE(LTHUF(NLAY))
      ALLOCATE(LAYWT(NLAY))
      ALLOCATE(HGUNAM(NHUF))
      ALLOCATE(HGUHANI(NHUF))
      ALLOCATE(HGUVANI(NHUF))
      ALLOCATE(HUFHK(NHUF))
      ALLOCATE(HUFVK(NHUF))
      ALLOCATE(HUFSS(NHUF))
      ALLOCATE(HUFSY(NHUF))
      ALLOCATE(HUFHANI(NHUF))
      ALLOCATE(HUFKDEP(NHUF))
      READ(IN,*) (LTHUF(K),K=1,NLAY)
      READ(IN,*) (LAYWT(K),K=1,NLAY)
      WRITE(IOUT,*) '(LTHUF(K),K=1,NLAY):'
      WRITE(IOUT,*) (LTHUF(K),K=1,NLAY)
      WRITE(IOUT,*) '(LAYWT(K),K=1,NLAY):'
      WRITE(IOUT,*) (LAYWT(K),K=1,NLAY)
C
C4A-----PRINT TABLES OF VALUES FOR LTHUF, HGUHANI, HGUVANI
C4B-----BASED ON LTHUF, HUFLAYAVG, HGUHANI, LAYWT, COUNT THE NUMBER OF EACH
C4B-----TYPE OF 2-D ARRAY; CHECK VALUES FOR CONSISTENCY; AND SETUP
C4B-----POINTERS IN LTHUF, HGUHANI, AND LAYWT FOR CONVENIENT ACCESS
C4B-----TO SC2, HANI, and WETDRY.  PRINT INTERPRETED VALUES OF FLAGS.
!      WRITE(IOUT,47)
!   47 FORMAT(1X,/3X,'INTERPRETATION OF LAYER FLAGS:',/1X,
!     & 'LAYER     LTHUF    LAYER TYPE     LAYWT WETTABILITY',
!     & /1X,75('-'))
      NCNVRT=0
      NWETD=0
      DO 50 K=1,NLAY
        IF(LTHUF(K).NE.0) THEN
          NCNVRT=NCNVRT+1
          LTHUF(K)=NCNVRT
        END IF
        IF(LAYWT(K).NE.0) THEN
          IF(LTHUF(K).EQ.0) THEN
            WRITE(IOUT,*)
     &          ' LAYWT is not 0 and LTHUF is 0 for layer:',K
            WRITE(IOUT,*) ' LAYWT must be 0 if LTHUF is 0'
            CALL USTOP(' ')
          ELSE
            NWETD=NWETD+1
            LAYWT(K)=NWETD
          END IF
        END IF
        LAYPRN(1)=TYPNAM(1)
        IF(LTHUF(K).NE.0) LAYPRN(1)=TYPNAM(2)
        LAYPRN(5)=WETNAM(1)
        IF(LAYWT(K).NE.0) LAYPRN(5)=WETNAM(2)
!        WRITE(IOUT,78) K,LTHUF(K),LAYPRN(1),LAYWT(K),LAYPRN(5)
!   78   FORMAT(1X,I4,2(I10,A14))
C     SET GLOBAL HEAD-DEPENDENT THICKNESS INDICATOR
      IF (LTHUF(K).NE.0) THEN
        LAYHDT(K)=1
        LAYHDS(K)=1
      ELSE
        LAYHDT(K)=0
        LAYHDS(K)=0
      ENDIF
   50 CONTINUE
C
C
C5------COMPUTE THE NUMBER OF CELLS IN THE ENTIRE GRID AND IN ONE LAYER.
C     NRC=NROW*NCOL
C
C6------ALLOCATE SPACE FOR ARRAYS.
      ALLOCATE(HK(NCOL,NROW,NLAY))
      ALLOCATE(HKCC(NCOL,NROW,NLAY))
      ALLOCATE(VKAH(NCOL,NROW,NLAY))
      ALLOCATE(SC2HUF(NCOL,NROW))
      SC2HUF = 0.0
      IF(ITRSS.NE.0)THEN 
        ALLOCATE(SC1(NCOL,NROW,NLAY))
      ELSE
        ALLOCATE(SC1(1,1,1))
      ENDIF
      IF(NWETD .GT. 0)THEN
        ALLOCATE(WETDRY(NCOL,NROW,NWETD))
      ELSE
        ALLOCATE(WETDRY(1,1,1))
      ENDIF
      IF(NHUF .GT. 0)THEN
        ALLOCATE(IHGUFLG(5,NHUF))
        ALLOCATE(HUFTMP(NCOL,NROW,NHUF))
        ALLOCATE(HUFTHK(NCOL,NROW,NHUF,2))
      ELSE
        ALLOCATE(IHGUFLG(1,1))
        ALLOCATE(HUFTMP(1,1,1))
        ALLOCATE(HUFTHK(1,1,1,1))
      ENDIF

      IF(ILVDA .NE. 0)THEN
        ALLOCATE(VDHD(NCOL,NROW,NLAY))
        ALLOCATE(VDHT(NCOL,NROW,NLAY,3))
        ALLOCATE(A9(NCOL,NROW,NLAY,5))
      ELSE
        ALLOCATE(VDHD(1,1,1))
        ALLOCATE(VDHT(1,1,1,1))
        ALLOCATE(A9(1,1,1,1))
      ENDIF
      ALLOCATE(GS(NCOL,NROW))
C7------READ DATA FOR HYDROGEOLOGIC-UNIT FLOW PACKAGE 

C---Read rewetting information
      IWDFLG=0
      KLAYFLG=0
      DO 10 K=1,NLAY
        IF(LAYWT(K).NE.0) IWDFLG=IWDFLG+1
        IF(LTHUF(K).NE.0) KLAYFLG=1
   10 CONTINUE
      IF(IWDFLG.EQ.0) THEN
 !        WRITE(IOUT,111)
 ! 111    FORMAT(1X,/,1X,'WETTING CAPABILITY IS NOT ACTIVE IN ANY LAYER')
      ELSE
!         WRITE(IOUT,112) IWDFLG
!  112    FORMAT(1X,/,1X,'WETTING CAPABILITY IS ACTIVE IN',I4,' LAYERS')
         IWDFLG=1
         READ(IN,*) WETFCT,IWETIT,IHDWET
         IF(IWETIT.LE.0) IWETIT=1
         WRITE(IOUT,*) 'WETFCT,IWETIT,IHDWET:'
         WRITE(IOUT,*) WETFCT,IWETIT,IHDWET
!         WRITE(IOUT,*) ' WETTING FACTOR=',WETFCT
!         WRITE(IOUT,*) ' WETTING ITERATION INTERVAL=',IWETIT
!         WRITE(IOUT,*) ' IHDWET=',IHDWET
      END IF
C
C2H-----READ WETDRY CODES IF WETTING CAPABILITY HAS BEEN INVOKED
C2H-----(LAYWT NOT 0).
      DO 300 K=1,NLAY
      IF(LAYWT(K).NE.0) THEN
         CALL U2DREL(WETDRY(:,:,LAYWT(K)),ANAME(8),NROW,NCOL,K,IN,
     &            IOUT)
      END IF
  300 CONTINUE

!      WRITE(IOUT,147)
!  147 FORMAT(
!     & //1X,'HUF7 -- HYDROGEOLOGIC-UNIT FLOW PACKAGE',
!     & /1X,75('-'))
C
C-------READ HYDROGEOLOGIC-UNIT GEOMETRY
      Call SGWF2HUF7GEOMRP(IN)
C
C---Read HANI and VANI values for each named unit
      WRITE(IOUT, *) 'HUF ITEM 9:'
      DO 100 NU=1,NHUF
        READ(IN,'(A)') LINE
C  Get the name of the new unit
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
C  Find the unit name in the list
        TMPNAM=LINE(ISTART:ISTOP)
	  WRITE(IOUT, *) 'HGUNAM:'
	  WRITE(IOUT, *) TMPNAM
        IF(TMPNAM.EQ.'ALL') THEN
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,HANITMP,IOUT,IN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,VANITMP,IOUT,IN)
	    WRITE(IOUT, *) 'HGUHANI, HGUVANI:'
	    WRITE(IOUT, *) HANITMP, VANITMP
          DO 150 NNU=1,NHUF
            HGUHANI(NNU)=HANITMP
            HGUVANI(NNU)=VANITMP
  150     CONTINUE
          GOTO 101
        ENDIF
        IU=0
        DO 200 NNU=1,NHUF
          CTMP1=HGUNAM(NNU)
          CALL UPCASE(CTMP1)
          IF(TMPNAM.EQ.CTMP1) THEN
            IU=NNU
!            WRITE(IOUT,38) TMPNAM,IU
!   38       FORMAT('UNIT ',A10,'CORRESPONDS TO UNIT NO. ',I5)
            GO TO 201
          END IF
  200   CONTINUE
  201   CONTINUE
        IF(IU.EQ.0) THEN
          WRITE(IOUT,41) TMPNAM
   41     FORMAT('UNIT ',A10,'NOT FOUND (STOP EXECUTION(GWF2HUF7RPGD))')
          CALL USTOP(' ')
        ENDIF
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,HGUHANI(IU),IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,HGUVANI(IU),IOUT,IN)
	  WRITE(IOUT, *) 'HGUHANI, HGUVANI:'
	  WRITE(IOUT, *) HGUHANI(IU), HGUVANI(IU)
  100 CONTINUE
  101 CONTINUE


!      WRITE(IOUT,48)
!   48 FORMAT(
!     & //3X,'INTERPRETATION OF UNIT FLAGS:',/1X,
!     & '    UNIT      HANI           VK/VANI',
!     & /1X,75('-'))
      DO 210 K=1,NHUF
        LAYPRN2=VKANAM(1)
        IF(HGUVANI(K).NE.0.0) LAYPRN2=VKANAM(2)
!        IF(HGUHANI(K).LE.0.0) THEN
!          WRITE(IOUT,79) HGUNAM(K),HANNAM,LAYPRN2
!        ELSE
!          WRITE(IOUT,80) HGUNAM(K),HGUHANI(K),LAYPRN2
!        END IF
!   79   FORMAT(1X,A10,2A14)
!   80   FORMAT(1X,A10,G14.7,A14)
  210 CONTINUE

C
C-------READ NAMED PARAMETERS
      NPSS=0
      NPSY=0
      NPSYTP=0
      IF(NPHUF.GT.0) THEN
        DO 20 K=1,NPHUF
c          CALL UHUF7PARRP(IN,IOUT,NP,PTYP,ITERP,NHUF)
          CALL UHUF7PARRP(IN,IOUT,N,PTYP,1,NHUF)
          IF(PTYP.EQ.'HK') THEN
            CONTINUE
          ELSE IF(PTYP.EQ.'HANI') THEN
            CONTINUE
          ELSE IF(PTYP.EQ.'VK') THEN
            CONTINUE
          ELSE IF(PTYP.EQ.'VANI') THEN
            CONTINUE
          ELSE IF(PTYP.EQ.'SS') THEN
            NPSS=1
          ELSE IF(PTYP.EQ.'SY') THEN
            NPSY=1
          ELSE IF(PTYP.EQ.'SYTP') THEN
            NPSYTP=1
          ELSE
            WRITE(IOUT,*) ' Invalid parameter type for HUF Package'
            CALL USTOP(' ')
          END IF
   20   CONTINUE
      END IF
C
C  Make some basic parameter checks
      IF(ITRSS.NE.0.AND.NPSS.EQ.0.AND.NPSY.EQ.0) THEN
        WRITE(IOUT,500)
  500   FORMAT(//3X,
     &    'Simulation is transient and no storage parameters are ',
     &    'defined in the HUF Package (STOP GWF2HUF7RPGD)')
        CALL USTOP(' ')
      ENDIF
C
      IF(ITRSS.EQ.0.AND.(NPSS.NE.0.OR.NPSY.NE.0)) THEN
        WRITE(IOUT,510)
  510   FORMAT(//3X,
     &    'Simulation is steady state and storage parameters are',
     &    ' defined in the HUF Package (STOP GWF2HUF7RPGD)')
        CALL USTOP(' ')
      ENDIF
C
      IF(ITRSS.NE.0.AND.KLAYFLG.NE.0.AND.
     &       (NPSS.EQ.0.OR.NPSY.EQ.0)) THEN
        WRITE(IOUT,520)
  520   FORMAT(//3X,
     &    'Simulation is transient and has convertible ',
     &    'layers and only one storage parameter is defined in the HUF',
     &    ' Package (STOP GWF2HUF7RPGD)')
        CALL USTOP(' ')
      ENDIF
      IF(KLAYFLG.NE.0.AND.NPSYTP.GT.0) THEN
        WRITE(IOUT,530)
  530   FORMAT(//3X,
     &    'Simulation has SYTP parameter(s) defined but has a ',
     &    'convertible layer.  SYTP will be disregarded (GWF2HUF7RPGD)')
      ENDIF
      IF(ITRSS.EQ.0.AND.NPSYTP.NE.0) THEN
        WRITE(IOUT,540)
  540   FORMAT(//3X,
     &    'Simulation is steady state and SYTP parameter(s) are',
     &    ' defined in the HUF Package (STOP GWF2HUF7RPGD)')
        CALL USTOP(' ')
      ENDIF
C
C
C---Read PRINTCODE
      DO 390 NU=1,NHUF
        DO 390 I=1,5
          IHGUFLG(I,NU)=0
  390 CONTINUE
  399 CONTINUE
      READ(IN,'(A)',END=400) LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
      TMPNAM=LINE(ISTART:ISTOP)
      CALL UPCASE(TMPNAM)
      IF(TMPNAM.NE.'PRINT') GOTO 400
!      WRITE(IOUT,'(/,A)') 'Reading PRINTCODE information'
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
C  Find the unit name in the list
      TMPNAM=LINE(ISTART:ISTOP)
!	WRITE(IOUT, *) 'HGUNAM:'
!      WRITE(IOUT, *) TMPNAM
      CALL UPCASE(TMPNAM)
      IF(TMPNAM.EQ.'ALL') THEN
        IU=-1
      ELSE
        IU=0
        DO 410 NU=1,NHUF
          CTMP1=HGUNAM(NU)
          CALL UPCASE(CTMP1)
          IF(TMPNAM.EQ.CTMP1) THEN
            IU=NU
!            WRITE(IOUT,438) TMPNAM,IU
!  438       FORMAT('UNIT ',A10,'CORRESPONDS TO UNIT NO. ',I5)
            GO TO 411
          END IF
  410   CONTINUE
      ENDIF
  411 CONTINUE
      IF(IU.EQ.0) THEN
        WRITE(IOUT,440) TMPNAM
  440   FORMAT('UNIT ',A10,'NOT FOUND (STOP EXECUTION)')
        CALL USTOP(' ')
      ENDIF
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICODE,R,IOUT,IN)
!	WRITE(IOUT, *) 'PRINTCODE:'
!	WRITE(IOUT, *) ICODE
C---Reset flags
      DO 445 I=1,5
        IFLG(I)=0
  445 CONTINUE
  450 CONTINUE
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
      TMPNAM=LINE(ISTART:ISTOP)
      IF(TMPNAM.EQ.' ') GOTO 455
!	WRITE(IOUT, *) 'PRINTFLAGS:'
!	WRITE(IOUT, *) TMPNAM
      IF(TMPNAM.EQ.'ALL') THEN
        IFLG(1)=ICODE
        IFLG(2)=ICODE
        IFLG(3)=ICODE
        IF(ITRSS.NE.0) THEN
          IFLG(4)=ICODE
          IFLG(5)=ICODE
        ENDIF
        GOTO 455
      ELSEIF(TMPNAM.EQ.'HK') THEN
        IFLG(1)=ICODE
      ELSEIF(TMPNAM.EQ.'HANI') THEN
        IFLG(2)=ICODE
      ELSEIF(TMPNAM.EQ.'VK') THEN
        IFLG(3)=ICODE
      ELSEIF(TMPNAM.EQ.'SS'.AND.ITRSS.NE.0)THEN
        IFLG(4)=ICODE
      ELSEIF(TMPNAM.EQ.'SY'.AND.ITRSS.NE.0) THEN
        IFLG(5)=ICODE
      ENDIF
      GOTO 450
  455 CONTINUE
      IF(IU.EQ.-1) THEN
        DO 460 NU=1,NHUF
          DO 465 I=1,5
            IHGUFLG(I,NU)=IFLG(I)
  465     CONTINUE
  460   CONTINUE
        GOTO 400
      ELSE
        DO 470 I=1,5
          IHGUFLG(I,IU)=IFLG(I)
  470   CONTINUE
        GOTO 399
      ENDIF
  400 CONTINUE
	WRITE(IOUT, *) 'HUF ITEM 12:'
!      WRITE(IOUT,442)
!  442 FORMAT(//,'PRINTCODE FLAGS ARE SET AS FOLLOWS',/,
!     &  '   UNIT       HK   HANI   VK    SS    SY',/,
!     &  '------------------------------------------')
      DO 480 NU=1,NHUF
        WRITE(IOUT,*) 'HGUNAM(NU):'
        WRITE(IOUT,*) HGUNAM(NU)
        WRITE(IOUT,*) '(IHGUFLG(I,NU),I=1,5):'
        WRITE(IOUT,*) (IHGUFLG(I,NU),I=1,5)
!  444 FORMAT(A10,5I6)
  480 CONTINUE

C8------READ DATA FOR LVDA
      IF(ILVDA .NE. 0) CALL SGWF2HUF7LVDA1RPGD(ILVDA,IOUT,1,NHUF,
     &                       NPLVDA,NLAY,0)
C
C9------READ DATA FOR KDEP
      IF(IKDEP .NE. 0) CALL SGWF2HUF7KDEP1RPGD(IKDEP,IOUT,1,NPKDEP,
     &                       IFKDEP,NROW,NCOL,GS,BOTM(:,:,0),NHUF) 
C
C-------SUBSTITUTE AND PREPARE DATA FOR HYDROGEOLOGIC-UNIT FLOW PACKAGE

!      CALL SGWF2HUF7SP(0,0,0,ILVDA)    
C
C-------RETURN
      CALL GWF2HUF7PSV(IGRID)
!  RGN DEFINE SECONDARY STORAGE COEFFICIENT FOR SFR2 AND UZF1
!  DETERMINE IF THERE IS A TRANSIENT STRESS PERIOD
      ISS = 1
      DO KPER = 1 , NPER
        IF ( ISSFLG(KPER).EQ.0 ) ISS = 0
      END DO
      IF(ISS.EQ.0) THEN
C   IF CONVERTIBLE LAYER GET PRIMARY STORAGE
        DO  I=1,NROW
          DO  J=1,NCOL
            K = 0
            KK = 1
            UPLAY: DO WHILE ( KK.LE.NLAY )
              IF( IBOUND(J,I,KK).GT.0 ) THEN
                K = KK
                EXIT UPLAY
              ELSE
                KK = KK + 1
              END IF
            END DO UPLAY
            IF ( K.GT.0 ) THEN
              IF(LTHUF(K).NE.0) THEN
! TRICK SUBROUTINE TO THINK UPERMOST LAYER IS UNCONFINED
                TOP=BOTM(J,I,LBOTM(K)-1)
                BOT=BOTM(J,I,LBOTM(K))
                HO=TOP-1.0E-1
                HN=TOP-1.0D-1
c RBW begin change
	          CHCOF = 0
c RBW end change
!                CALL SGWF2HUF7SC2(0,J,I,K,TOP,BOT,HN,HO,1.0,CHCOF,
!     &                      CRHS,HUFTHK,NCOL,NROW,NHUF,1.0,IOUT)
                SC2HUF(J,I) = CHCOF
              END IF
            END IF
          END DO
        END DO
      END IF  
      RETURN
      END

c======================================================================
      SUBROUTINE SGWF2HUF7GEOMRP(IN)
C
C     ******************************************************************
C     Read and prepare HYDROGEOLOGIC-UNIT GEOMETRY.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,IOUT
      USE GWFHUFMODULE,ONLY:NHUF,HUFTHK,HGUNAM
      CHARACTER*200 LINE
      CHARACTER*24 ANAME
C     ------------------------------------------------------------------
C
C-----Read the hydrogeologic-unit names and arrays
      DO 100 M=1,NHUF
C  Read a line describing a hydrogeologic unit
        READ (IN,'(A)') LINE
C  Get the name of the new unit
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
C  Add new unit name into list
        HGUNAM(M)=LINE(ISTART:ISTOP)
	  WRITE(IOUT, *) 'HGUNAM(M):'
	  WRITE(IOUT, *) HGUNAM(M)
C  Read top elevation of unit
        ANAME=' TOP ELEVATN: '//HGUNAM(M)
        CALL U2DREL(HUFTHK(:,:,M,1),ANAME,NROW,NCOL,0,IN,IOUT)
C  Read thickness of unit
        ANAME='   THICKNESS: '//HGUNAM(M)
        CALL U2DREL(HUFTHK(:,:,M,2),ANAME,NROW,NCOL,0,IN,IOUT)
  100 CONTINUE
C
      RETURN
      END
c======================================================================
!      SUBROUTINE SGWF2HUF7SP(KITER,KSTP,KPER,ILVDA)
C
C     ******************************************************************
C     SUBSTITUTE AND PREPARE DATA FOR HYDROGEOLOGIC-UNIT FLOW PACKAGE
C     ******************************************************************

c======================================================================
!      SUBROUTINE SGWF2HUF7HK(
!     &  NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,KT,KB,HK,
!     &  HKCC,HUFHK,HUFHANI,LAMBDA,NHUF,NU,HNEW,GS)
C
C     ******************************************************************
C     Substitute for HK parameters.
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VKA(
!     & NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,VKA,HNEW,IBOUND,HUFHK,
!     & HUFVK,NHUF,NU,LAMBDA,IOUT,GS)
C
C     ******************************************************************
C     Substitute for VKA parameters.
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VKL(
!     & VK,KL,NCOL,NROW,NLAY,BOTM,NBOTM,HNEW,IBOUND,IOUT,GS,HUFTHK,NHUF)
C
C     ******************************************************************
C     Calculate VK for a given layer
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VSRCH(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,
!     &  HNEW,IBOUND,KT,KB,IFLG)
C
C     ******************************************************************
C     Search for top and bottom layer the unit applies to.
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7HSRCH(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,
!     &  HNEW,IBOUND,KT,KB,IFLG)
C
C     ******************************************************************
C     Search for top and bottom layer the unit applies to.
C     Values for IFLG:
C       IFLG = 0, Unit successfully found
C       IFLG = 1, Unit not found
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7SC1(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,
!     &                        SC1,HUFSS,KT,KB,NHUF,NU)
C
C     ******************************************************************
C     Substitute for SS parameters.
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7N(HNEW,IBOUND,HK,VKA,NCOL,
!     &  NROW,NLAY,IOUT,WETDRY,BOTM,NBOTM)
C
C     ******************************************************************
C     INITIALIZE AND CHECK HUF DATA
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7HCND(HNEW,IBOUND,CR,CC,HK,DELR,DELC,
!     &  NCOL,NROW,NLAY,IOUT,BOTM,NBOTM,HKCC,HDRY,
!     &  KITER,KSTP,KPER)
C
C     ******************************************************************
C     CALCULATE HORIZONTAL CONDUCTANCES
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VCND(IBOUND,CV,VKA,DELR,DELC,
!     &  NCOL,NROW,NLAY)
C
C     ******************************************************************
C     CALCULATE VERTICAL CONDUCTANCES
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7SYTP(SC1,IBOUND,NCOL,NROW,NLAY)
C
C     ******************************************************************
C     REPLACE SC1 WITH SY FOR TOPMOST ACTIVE CELL FOR ANY I,J
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7SC(IBOUND,SC1,DELR,DELC,NCOL,NROW,NLAY)
C
C     ******************************************************************
C     CALCULATE SC1
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7HCOND(HNEW,IBOUND,CR,CC,HK,DELR,DELC,BOTM,
!     & NBOTM,K,KITER,KSTP,KPER,NCOL,NROW,NLAY,IOUT,
!     & HDRY,HKCC)
C     ******************************************************************
C     COMPUTE HORIZONTAL BRANCH CONDUCTANCE FOR ONE LAYER.
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7WETCHK(HNEW,IBOUND,CC,BOTM,
!     & NBOTM,K,KITER,KSTP,KPER,NCOL,NROW,NLAY,IOUT,WETDRY,
!     & WETFCT,IWETIT,IHDWET,HDRY)
C     ******************************************************************
C     CHECK FOR CELLS THAT GO DRY/REWET
C     ******************************************************************
c======================================================================
!      SUBROUTINE GWF2HUF7AD(KPER,IGRID)
C
C%%%%%
C  PLEASE NOTE: THIS SUBROUTINE WAS COPIED DIRECTLY FROM THE LPF
C PACKAGE.  THERE IS NOTHING SPECIFIC TO THE HUF PACKAGE.
C%%%%%
C     ******************************************************************
C     SET HOLD TO BOTM WHENEVER A WETTABLE CELL IS DRY
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7WET(HNEW,IBOUND,BOTM,NBOTM,K,KITER,KSTP,KPER,
!     &      NCOL,NROW,NLAY,IOUT,WETDRY,WETFCT,IHDWET,IHDCNV,
!     &      NCNVRT,ICNVRT,JCNVRT,ACNVRT)
C
C%%%%%
C  PLEASE NOTE: THIS SUBROUTINE WAS COPIED DIRECTLY FROM THE LPF
C PACKAGE.  THERE IS NOTHING SPECIFIC TO THE HUF PACKAGE.
C%%%%%
C     ******************************************************************
C     CONVERT DRY CELLS TO WET.
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7WDMSG(ICODE,NCNVRT,ICNVRT,JCNVRT,ACNVRT,
!     &             IHDCNV,IOUT,KITER,J,I,K,KSTP,KPER)
C
C%%%%%
C  PLEASE NOTE: THIS SUBROUTINE WAS COPIED DIRECTLY FROM THE LPF
C PACKAGE.  THERE IS NOTHING SPECIFIC TO THE HUF PACKAGE.
C%%%%%
C     ******************************************************************
C     PRINT MESSAGE WHEN CELLS CONVERT BETWEEN WET AND DRY.
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7HHARM(CR,CC,HK,IBOUND,DELR,DELC,K,NCOL,NROW,
!     &         NLAY,HKCC)
C
C     ******************************************************************
C     COMPUTE HORIZONTAL BRANCH CONDUCTANCE USING HARMONIC MEAN OF BLOCK
C     CONDUCTANCES (DISTANCE WEIGHTED HARMONIC MEAN OF TRANSMISSIVITY).
C     CELL THICKNESS IS IN CC UPON ENTRY.
C     ******************************************************************
c======================================================================
!      SUBROUTINE GWF2HUF7FM(KITER,KSTP,KPER,ILVDA,IGRID)
C     ******************************************************************
C     ADD LEAKAGE CORRECTION AND STORAGE TO HCOF AND RHS, AND CALCULATE
C     CONDUCTANCE AS REQUIRED.
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7SC2(IFLG,J,I,K,TOP,BOT,HN,HO,TLED,CHCOF,CRHS,
!     &                    HUFTHK,NCOL,NROW,NHUF,AREA,IOUT)
C
C     ******************************************************************
C     Compute contributions to HCOF and RHS for convertible cell
C     Enter subroutine when HO and/or HN are below TOP
C     Values for IFLG:
C       IFLG = 0, Calculate contributions to HCOF and RHS
C       IFLG = 1, Calculate contributions to flow within cell
C       IFLG = 2, Calculate contributions to sensitivity calculations
C     Subroutine will halt execution if Sy is not defined.
C     ******************************************************************
c======================================================================
!      SUBROUTINE GWF2HUF7BDS(KSTP,KPER,IGRID)
C     ******************************************************************
C     COMPUTE STORAGE BUDGET FLOW TERM FOR HUF.
C     ******************************************************************
c======================================================================
!      SUBROUTINE GWF2HUF7BDADJ(KSTP,KPER,IDIR,IBDRET,IC1,IC2,IR1,IR2,
!     1                          IL1,IL2,ILVDA,IGRID)
C
C     ******************************************************************
C     COMPUTE FLOW BETWEEN ADJACENT CELLS IN A SUBREGION OF THE GRID
C     ******************************************************************
c======================================================================
!      SUBROUTINE GWF2HUF7BDCH(KSTP,KPER,ILVDA,IGRID)
C     ******************************************************************
C     COMPUTE FLOW FROM CONSTANT-HEAD CELLS
C     ******************************************************************
c======================================================================
!      SUBROUTINE GWF2HUF7OT(KSTP,KPER,ICNVG,ISA,IGRID)
C
C     ******************************************************************
C     PRINT AND RECORD HEADS AND FLOWS INTERPOLATED TO HGU'S
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7FLOT(HNWHGU,HNEW,IBOUND,NHUF,NCOL,NROW,NLAY,
!     &                   HUFTHK,BOTM,NBOTM,CV,DELR,DELC,IOUT,GS,IHUFCB,
!     &                   IBD,KSTP,KPER,DELT,PERTIM,TOTIM,ICHFLG)
C
C     ******************************************************************
C     CALCULATE FLOWS WITHIN A GIVEN HYDROGEOLOGIC UNIT
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7HDOT(HNWHGU,HNEW,IBOUND,NHUF,NCOL,NROW,NLAY,
!     &                   HUFTHK,BOTM,NBOTM,CV,DELR,DELC,HNOFLO,IOUT,GS)
C
C     ******************************************************************
C     CALCULATE HEADS WITHIN A GIVEN HYDROGEOLOGIC UNIT
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7CHFLOT(HNWHGU,HNEW,IBOUND,NHUF,NCOL,NROW,NLAY,
!     &                   HUFTHK,BOTM,NBOTM,CV,DELR,DELC,IOUT,GS,IHUFCB,
!     &                   IBD,KSTP,KPER,ICHFLG)
C     ******************************************************************
C     COMPUTE FLOW FROM CONSTANT-HEAD CELLS IN HGU's
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7C(
!     1 IPT,JPT,KPT,GPT,CRL,CRR,CCT,CCB,THK0,THKL,
!     2 THKR,THKT,THKB,HNEW,DELR,DELC,BOTM,NCOL,NROW,NLAY,
!     3 IOUT,NHUF,NBOTM,HUFTHK,GS)
C
C     ******************************************************************
C     CALCULATE HORIZONTAL BRANCH CONDUCTANCES FOR SINGLE HGU OR FOR
C     LAYER WITHIN AN HGU.  FOR UNITS THAT PINCH OUT, THE EQUIVALENT
C     THICKNESS IN THE ADJACENT UNIT IS USED
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7IND(NCOL,NROW,NHUF,HUFTHK,JJ,II,INDX)
C
C     ******************************************************************
C     INDEX HYDROGEOLOGIC UNITS FOR A GIVEN ROW/COLUMN LOCATION
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
c======================================================================
      SUBROUTINE SGWF2HUF7KDEP1RPGD(
     & IN,IOUT,ITERP,NPKDEP,IFKDEP,NROW,NCOL,GS,TOP,NHUF)
C
C     ******************************************************************
C     READ PARAMETERS FOR KDEP CAPABILITY
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE PARAMMODULE
      CHARACTER*4 PTYP
      DIMENSION GS(NCOL,NROW),TOP(NCOL,NROW)
C     ------------------------------------------------------------------
C
      WRITE(IOUT,*) 'KDEP:'
!      WRITE(IOUT,47)
!   47 FORMAT(
!     & //1X,'KDEP1 -- KDEP CAPABILITY',
!     & /1X,75('-'))
C
C-------READ GROUND SURFACE OR TRANSFER FROM BOTTOM ARRAY
      IF(IFKDEP.GT.0) THEN
!        WRITE(IOUT,*) 'Reading ground surface'
        CALL U2DREL(GS,'GROUND SURFACE          ',NROW,NCOL,0,IN,IOUT)
      ELSE
 !       WRITE(IOUT,*) 'Transferring ground surface from TOP'
        DO 100 I=1,NROW
          DO 200 J=1,NCOL
            GS(J,I) = TOP(J,I)
  200     CONTINUE
  100   CONTINUE
      ENDIF
C
C-------READ NAMED PARAMETERS
      IF(NPKDEP.GT.0) THEN
        DO 20 K=1,NPKDEP
          CALL UHUF7PARRP(IN,IOUT,NP,PTYP,ITERP,NHUF)
          IF(PTYP.EQ.'KDEP') THEN
            CONTINUE
          ELSE
            WRITE(IOUT,*) ' Invalid parameter type for KDEP capability'
            CALL USTOP(' ')
          END IF
C  Make the parameter global
          IACTIVE(NP)=-1
   20   CONTINUE
      END IF
C
C4------RETURN
      RETURN
      END
c======================================================================
!      SUBROUTINE SGWF2HUF7KDEP(
!     &  LAMBDA,TOP,BOT,GS,MULTKDEP)
C
C     ******************************************************************
C     Calculate KDEP multiplier for interval
C     ******************************************************************
C=======================================================================
      SUBROUTINE SGWF2HUF7LVDA1RPGD(
     & IN,IOUT,ITERP,NHUF,NPLVDA,NLAY,ISEN)
C
C     ******************************************************************
C     READ PARAMETERS FOR LVDA CAPABILITY
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE PARAMMODULE
      USE GWFHUFMODULE,ONLY:LTHUF
C      
      CHARACTER*4 PTYP
C     ------------------------------------------------------------------
C
      WRITE(IOUT,*) 'LVDA:'
!      WRITE(IOUT,47)
!   47 FORMAT(
!     & //1X,'LVDA1 -- LVDA CAPABILITY',
!     & /1X,75('-'))
C
C-------READ NAMED PARAMETERS
      IF(NPLVDA.GT.0) THEN
        DO 20 K=1,NPLVDA
          CALL UHUF7PARRP(IN,IOUT,NP,PTYP,ITERP,NHUF)
          IF(PTYP.EQ.'LVDA') THEN
            CONTINUE
          ELSE
            WRITE(IOUT,*) ' Invalid parameter type for LVDA capability'
            CALL USTOP(' ')
          END IF
C  Make the parameter global
          IACTIVE(NP)=-1
   20   CONTINUE
      END IF
C
C-----CHECK TO SEE IF THE SEN PROCESS IS ACTIVE AND THERE ARE CONVERTIBLE
C     LAYERS
      IF(ISEN.GT.0) THEN
        KLAYFLG=0
        DO 30 K=1,NLAY
          IF(LTHUF(K).NE.0) KLAYFLG=1
   30   CONTINUE
        IF(KLAYFLG.NE.0) THEN
          WRITE(IOUT,*) ' LVDA cannot calculate sensitivities for',
     & ' convertible layers!'
          WRITE(IOUT,*) ' STOP EXECUTION - GWF2HUF7LVDA1RPGD'
          CALL USTOP(' ')
        ENDIF
      ENDIF
C
C4------RETURN
      RETURN
      END

c======================================================================
!      SUBROUTINE GWF2HUF7VDFM(
!     &  HNEW,IBOUND,CR,CC,VDHT,RHS,NCOL,NROW,NLAY,A9)
C
C     ******************************************************************
C     Calculate stiffness matrix and RHS for LVDA
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VDHHV(IBOUND,HK,HKCC,VDHD,NCOL,NROW,NLAY,
!     &  HUFTHK,NHUF,BOTM,NBOTM,HNEW,GS)
C
C     ******************************************************************
C     Populate the Hydraulic Conductivity Arrays for LVDA.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
c======================================================================
!      SUBROUTINE SGWF2HUF7VDHT(
!     &  IBOUND,HK,HKCC,VDHD,VDHT,NCOL,NROW,NLAY,DELR,DELC)
C
C     ******************************************************************
C     Populate the Hydraulic Conductivity Tensor for LVDA.
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VDF9(I,J,K,VDHT,HNEW,IBOUND,NLAY,NROW,NCOL,
!     &                         FL,FR,FT,FB)
C     ******************************************************************
C     COMPUTE FLOW THROUGH CELL FACES USING THE 9-POINT STENCIL
C     ******************************************************************
!      SUBROUTINE SGWF2HUF7VDA9F9(I,J,K,A9,HNEW,IBOUND,NLAY,NROW,NCOL,F)
C     ******************************************************************
C     COMPUTE FLOW THROUGH CELL FACES USING THE STORED 9-POINT STENCIL
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
c======================================================================
!      SUBROUTINE SGWF2HUF7VDA9(VDHT,A9,IBOUND,NLAY,NROW,NCOL)
C     ******************************************************************
C     COMPUTE AND STORE STIFFNESS-COEFFICIENT MATRIX FOR 9-POINT STENCIL
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VDHGET(
!     &  IBOUND,HNEW,J,I,K,NROW,NCOL,NLAY,
!     &  H1,H2,H3,H4,H5,H6,H7,H8,H9,
!     &  IBD2,IBD3,IBD4,IBD5,IBD6,IBD7,IBD8,IBD9)
C
C     ******************************************************************
C     Grab heads and ibound for all local cells
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VDA1(
!     &  IBOUND,VDHT,NCOL,NROW,NLAY,I,J,K,TMPCOF,
!     2  AS11234,AS21234,AS31234,AS41234,
!     3  AW11234,AW21234,AW31234,AW41234,
!     4  TAA1,TAB1,TBB1)
C
C     ******************************************************************
C     Calculate the A coefficients for Quadrant I
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VDA2(
!     1  IBOUND,VDHT,NCOL,NROW,NLAY,I,J,K,TMPCOF,
!     2  AS16145,AS26145,AS36145,AS46145,
!     3  AE16145,AE26145,AE36145,AE46145)
C
C     ******************************************************************
C     Calculate the A coefficients for Quadrant II
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VDA3(
!     1  IBOUND,VDHT,NCOL,NROW,NLAY,I,J,K,TMPCOF,
!     2  AN17816,AN27816,AN37816,AN47816,
!     3  AE17816,AE27816,AE37816,AE47816)
C
C     ******************************************************************
C     Calculate the A coefficients for Quadrant III
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VDA4(
!     1  IBOUND,VDHT,NCOL,NROW,NLAY,I,J,K,TMPCOF,
!     2  AN18921,AN28921,AN38921,AN48921,
!     3  AW18921,AW28921,AW38921,AW48921)
C
C     ******************************************************************
C     Calculate the A coefficients for Quadrant IV
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VDCOFD(D,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
!     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4)
C
C     ******************************************************************
C     Calculate the D coefficent
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VDCOFAS1(AS1,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
!     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AS1 coefficent
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VDCOFAS2(AS2,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
!     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AS2 coefficent
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VDCOFAS4(AS4,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
!     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
c======================================================================
!      SUBROUTINE SGWF2HUF7VDCOFAW1(AW1,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
!     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AW1 coefficent
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VDCOFAW2(AW2,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
!     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AW2 coefficent
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VDCOFAW3(AW3,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
!     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AW3 coefficent
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VDCOFAW4(AW4,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
!     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AW4 coefficent
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VDCOFAE1(AE1,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
!     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AE1 coefficent
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VDCOFAE2(AE2,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
!     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AE2 coefficent
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VDCOFAE3(AE3,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
!     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AE3 coefficent
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VDCOFAE4(AE4,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
!     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AE4 coefficent
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VDCOFAN1(AN1,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
!     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AN1 coefficent
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VDCOFAN2(AN2,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
!     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AN2 coefficent
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VDCOFAN3(AN3,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
!     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AN3 coefficent
C     ******************************************************************
c======================================================================
!      SUBROUTINE SGWF2HUF7VDCOFAN4(AN4,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
!     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AN4 coefficent
C     ******************************************************************
c======================================================================
      SUBROUTINE GWF2HUF7DA(IGRID)
C
C     ******************************************************************
C     DEALLOCATE HUF DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFHUFMODULE
C     ------------------------------------------------------------------
      DEALLOCATE(GWFHUFDAT(IGRID)%IHUFCB)
      DEALLOCATE(GWFHUFDAT(IGRID)%NHUF)
      DEALLOCATE(GWFHUFDAT(IGRID)%NPHUF)
      DEALLOCATE(GWFHUFDAT(IGRID)%IWETIT)
      DEALLOCATE(GWFHUFDAT(IGRID)%IHDWET)
      DEALLOCATE(GWFHUFDAT(IGRID)%IOHUFHDS)
      DEALLOCATE(GWFHUFDAT(IGRID)%IOHUFFLWS)
      DEALLOCATE(GWFHUFDAT(IGRID)%WETFCT)
      DEALLOCATE(GWFHUFDAT(IGRID)%HGUNAM)
      DEALLOCATE(GWFHUFDAT(IGRID)%LTHUF)
      DEALLOCATE(GWFHUFDAT(IGRID)%LAYWT)
      DEALLOCATE(GWFHUFDAT(IGRID)%IHGUFLG)
      DEALLOCATE(GWFHUFDAT(IGRID)%HGUHANI)
      DEALLOCATE(GWFHUFDAT(IGRID)%HGUVANI)
      DEALLOCATE(GWFHUFDAT(IGRID)%HUFHK)
      DEALLOCATE(GWFHUFDAT(IGRID)%HUFVK)
      DEALLOCATE(GWFHUFDAT(IGRID)%HUFSS)
      DEALLOCATE(GWFHUFDAT(IGRID)%HUFSY)
      DEALLOCATE(GWFHUFDAT(IGRID)%HUFKDEP)
      DEALLOCATE(GWFHUFDAT(IGRID)%GS)
      DEALLOCATE(GWFHUFDAT(IGRID)%VKAH)
      DEALLOCATE(GWFHUFDAT(IGRID)%SC1)
      DEALLOCATE(GWFHUFDAT(IGRID)%WETDRY)
      DEALLOCATE(GWFHUFDAT(IGRID)%HK)
      DEALLOCATE(GWFHUFDAT(IGRID)%HKCC)
      DEALLOCATE(GWFHUFDAT(IGRID)%HUFTMP)
      DEALLOCATE(GWFHUFDAT(IGRID)%VDHD)
      DEALLOCATE(GWFHUFDAT(IGRID)%HUFTHK)
      DEALLOCATE(GWFHUFDAT(IGRID)%VDHT)
      DEALLOCATE(GWFHUFDAT(IGRID)%A9)
      DEALLOCATE(GWFHUFDAT(IGRID)%SC2HUF)
C
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7PNT(IGRID)
C
C     ******************************************************************
C     POINT TO HUF DATA FOR A SPECIFIC GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFHUFMODULE
C     ------------------------------------------------------------------
      IHUFCB=>GWFHUFDAT(IGRID)%IHUFCB
      NHUF=>GWFHUFDAT(IGRID)%NHUF
      NPHUF=>GWFHUFDAT(IGRID)%NPHUF
      IWETIT=>GWFHUFDAT(IGRID)%IWETIT
      IHDWET=>GWFHUFDAT(IGRID)%IHDWET
      IOHUFHDS=>GWFHUFDAT(IGRID)%IOHUFHDS
      IOHUFFLWS=>GWFHUFDAT(IGRID)%IOHUFFLWS
      WETFCT=>GWFHUFDAT(IGRID)%WETFCT
      HGUNAM=>GWFHUFDAT(IGRID)%HGUNAM
      LTHUF=>GWFHUFDAT(IGRID)%LTHUF
      LAYWT=>GWFHUFDAT(IGRID)%LAYWT
      IHGUFLG=>GWFHUFDAT(IGRID)%IHGUFLG
      HGUHANI=>GWFHUFDAT(IGRID)%HGUHANI
      HGUVANI=>GWFHUFDAT(IGRID)%HGUVANI
      HUFHK=>GWFHUFDAT(IGRID)%HUFHK
      HUFVK=>GWFHUFDAT(IGRID)%HUFVK
      HUFSS=>GWFHUFDAT(IGRID)%HUFSS
      HUFSY=>GWFHUFDAT(IGRID)%HUFSY
      HUFKDEP=>GWFHUFDAT(IGRID)%HUFKDEP
      GS=>GWFHUFDAT(IGRID)%GS
      VKAH=>GWFHUFDAT(IGRID)%VKAH
      SC1=>GWFHUFDAT(IGRID)%SC1
      WETDRY=>GWFHUFDAT(IGRID)%WETDRY
      HK=>GWFHUFDAT(IGRID)%HK
      HKCC=>GWFHUFDAT(IGRID)%HKCC
      HUFTMP=>GWFHUFDAT(IGRID)%HUFTMP
      VDHD=>GWFHUFDAT(IGRID)%VDHD
      HUFTHK=>GWFHUFDAT(IGRID)%HUFTHK
      VDHT=>GWFHUFDAT(IGRID)%VDHT
      A9=>GWFHUFDAT(IGRID)%A9      
      SC2HUF=>GWFHUFDAT(IGRID)%SC2HUF     
C
      RETURN
      END
c======================================================================
      SUBROUTINE GWF2HUF7PSV(IGRID)
C
C     ******************************************************************
C     SAVE HUF DATA FOR A GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFHUFMODULE
C     ------------------------------------------------------------------
      GWFHUFDAT(IGRID)%IHUFCB=>IHUFCB
      GWFHUFDAT(IGRID)%NHUF=>NHUF
      GWFHUFDAT(IGRID)%NPHUF=>NPHUF
      GWFHUFDAT(IGRID)%IWETIT=>IWETIT
      GWFHUFDAT(IGRID)%IHDWET=>IHDWET
      GWFHUFDAT(IGRID)%IOHUFHDS=>IOHUFHDS
      GWFHUFDAT(IGRID)%IOHUFFLWS=>IOHUFFLWS
      GWFHUFDAT(IGRID)%WETFCT=>WETFCT
      GWFHUFDAT(IGRID)%HGUNAM=>HGUNAM
      GWFHUFDAT(IGRID)%LTHUF=>LTHUF
      GWFHUFDAT(IGRID)%LAYWT=>LAYWT
      GWFHUFDAT(IGRID)%IHGUFLG=>IHGUFLG
      GWFHUFDAT(IGRID)%HGUHANI=>HGUHANI
      GWFHUFDAT(IGRID)%HGUVANI=>HGUVANI
      GWFHUFDAT(IGRID)%HUFHK=>HUFHK
      GWFHUFDAT(IGRID)%HUFVK=>HUFVK
      GWFHUFDAT(IGRID)%HUFSS=>HUFSS
      GWFHUFDAT(IGRID)%HUFSY=>HUFSY
      GWFHUFDAT(IGRID)%HUFKDEP=>HUFKDEP
      GWFHUFDAT(IGRID)%GS=>GS
      GWFHUFDAT(IGRID)%VKAH=>VKAH
      GWFHUFDAT(IGRID)%SC1=>SC1
      GWFHUFDAT(IGRID)%WETDRY=>WETDRY
      GWFHUFDAT(IGRID)%HK=>HK
      GWFHUFDAT(IGRID)%HKCC=>HKCC
      GWFHUFDAT(IGRID)%HUFTMP=>HUFTMP
      GWFHUFDAT(IGRID)%VDHD=>VDHD
      GWFHUFDAT(IGRID)%HUFTHK=>HUFTHK
      GWFHUFDAT(IGRID)%VDHT=>VDHT
      GWFHUFDAT(IGRID)%A9=>A9
      GWFHUFDAT(IGRID)%SC2HUF=>SC2HUF
C
      RETURN
      END
