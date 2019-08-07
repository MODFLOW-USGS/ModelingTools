      MODULE GWFUPWMODULE
      IMPLICIT NONE                                                     
      DOUBLE PRECISION, PARAMETER :: HEPS = 1.0E-7                      
      DOUBLE PRECISION, PARAMETER :: CLOSEZERO = 1.0E-15
      DOUBLE PRECISION,PARAMETER :: BIG = 1.0D20 
      DOUBLE PRECISION,PARAMETER :: SMALL = 1.0D-5     
      DOUBLE PRECISION, SAVE, DIMENSION(:), POINTER :: Sn, So 
      INTEGER, SAVE,   POINTER :: Iuupw        
! Cell property data
        INTEGER, SAVE,   POINTER ::IUPWCB,IWDFLG,IWETIT,IHDWET,IPHDRY
        INTEGER, SAVE,   POINTER ::ISFAC,ICONCV,ITHFLG,NOCVCO,NOVFC
        REAL,    SAVE,   POINTER ::WETFCT
        INTEGER, SAVE,   POINTER, DIMENSION(:)     ::LAYTYPUPW
        INTEGER, SAVE,   POINTER, DIMENSION(:)     ::LAYAVG
        REAL,    SAVE,   POINTER, DIMENSION(:)     ::CHANI
        INTEGER, SAVE,   POINTER, DIMENSION(:)     ::LAYVKAUPW
        INTEGER, SAVE,   POINTER, DIMENSION(:)     ::LAYWET
        INTEGER, SAVE,   POINTER, DIMENSION(:)     ::LAYSTRT
        INTEGER, SAVE,   POINTER, DIMENSION(:,:)   ::LAYFLG
        INTEGER, SAVE,    DIMENSION(:,:,:), POINTER ::IBOUND2
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::VKAUPW
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::VKCB
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::SC1
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::SC2UPW
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::HANI
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::WETDRY
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::HKUPW     
      TYPE GWFUPWTYPE   
        INTEGER, POINTER :: Iuupw
! Cell property data
        INTEGER, POINTER ::IUPWCB,IWDFLG,IWETIT,IHDWET,IPHDRY
        INTEGER, POINTER ::ISFAC,ICONCV,ITHFLG,NOCVCO,NOVFC
        REAL, POINTER    ::WETFCT
        DOUBLE PRECISION, DIMENSION(:), POINTER :: Sn, So
        INTEGER,   POINTER, DIMENSION(:)     ::LAYTYPUPW
        INTEGER,   POINTER, DIMENSION(:)     ::LAYAVG
        REAL,      POINTER, DIMENSION(:)     ::CHANI
        INTEGER,   POINTER, DIMENSION(:)     ::LAYVKAUPW
        INTEGER,   POINTER, DIMENSION(:)     ::LAYWET
        INTEGER,   POINTER, DIMENSION(:)     ::LAYSTRT
        INTEGER,   POINTER, DIMENSION(:,:)   ::LAYFLG
        INTEGER,   POINTER, DIMENSION(:,:,:) ::IBOUND2
        REAL,      POINTER, DIMENSION(:,:,:) ::VKAUPW
        REAL,      POINTER, DIMENSION(:,:,:) ::VKCB
        REAL,      POINTER, DIMENSION(:,:,:) ::SC1
        REAL,      POINTER, DIMENSION(:,:,:) ::SC2UPW
        REAL,      POINTER, DIMENSION(:,:,:) ::HANI
        REAL,      POINTER, DIMENSION(:,:,:) ::WETDRY
        REAL,      POINTER, DIMENSION(:,:,:) ::HKUPW
      END TYPE GWFUPWTYPE                                               
      TYPE (GWFUPWTYPE) , SAVE::Gwfupwdat(10)                           
      END MODULE GWFUPWMODULE
!
 
!
!-------SUBROUTINE GWF2UPW1AR
!
      SUBROUTINE GWF2UPW1AR(In, Igrid)
 
      USE GLOBAL,     ONLY:NCOL,NROW,NLAY,ITRSS,LAYHDT,LAYHDS,LAYCBD,
     1                     NCNFBD,IBOUND,BUFF,BOTM,NBOTM,DELR,DELC,IOUT,
     2                     LBOTM,HNEW
      USE GWFBASMODULE,ONLY:HDRY
      USE GWFNWTMODULE, ONLY: Numcell
      USE GWFUPWMODULE
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
      INTRINSIC INT
      EXTERNAL URDCOM, URWORD
      EXTERNAL SGWF2UPW1PSV
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
      INTEGER In, Igrid
!     ------------------------------------------------------------------
!     LOCAL VARIABLES
!     ------------------------------------------------------------------
      INTEGER lloc, istart, istop, i, ic, ir, il, jj
      CHARACTER(LEN=200) line
      INTEGER NPHK,NPVKCB,NPVK,NPVANI,NPSS,NPSY,NPHANI
      INTEGER IANAME,KHANI,N,KK,j,k,NCNVRT,NHANI,NWETD
!     LOCAL VARIABLES FOR DEFINING CELL PROPERTES (FROM LPF)
      INTEGER NPUPW, NOPCHK
      REAL ZERO, R
!
      CHARACTER*14 LAYPRN(5),AVGNAM(3),TYPNAM(2),VKANAM(2),WETNAM(2),
     1            HANNAM
      DATA AVGNAM/'      HARMONIC','   LOGARITHMIC','     LOG-ARITH'/
      DATA TYPNAM/'      CONFINED','   CONVERTIBLE'/
      DATA VKANAM/'    VERTICAL K','    ANISOTROPY'/
      DATA WETNAM/'  NON-WETTABLE','      WETTABLE'/
      DATA HANNAM/'      VARIABLE'/
      CHARACTER*24 ANAME(9),STOTXT
      CHARACTER*4 PTYP
C
      DATA ANAME(1) /'   HYD. COND. ALONG ROWS'/
      DATA ANAME(2) /'  HORIZ. ANI. (COL./ROW)'/
      DATA ANAME(3) /'     VERTICAL HYD. COND.'/
      DATA ANAME(4) /' HORIZ. TO VERTICAL ANI.'/
      DATA ANAME(5) /'QUASI3D VERT. HYD. COND.'/
      DATA ANAME(6) /'        SPECIFIC STORAGE'/
      DATA ANAME(7) /'          SPECIFIC YIELD'/
      DATA ANAME(9) /'     STORAGE COEFFICIENT'/
!     ------------------------------------------------------------------
!1------IDENTIFY PACKAGE AND INITIALIZE.
      WRITE(IOUT, *) 'UPW:'
!      WRITE (Iout, 9001) In
! 9001 FORMAT (1X, /' UPW1 -- UPSTREAM WEIGHTING FLOW PACKAGE, ',
!     +       'VERSION 1.0.6, 12/05/2012', /, 9X, 'INPUT READ FROM UNIT',
!     +        I3,/)
!  ALLOCATE, READ AND SET DATA FOR CELL PROPERTIES (FROM LPF)
C1------Allocate scalar data.
      ALLOCATE(IUPWCB,NOVFC,Iuupw)
!  STORE UPW UNIT NUMBER IN MODULE VARIABLE
      Iuupw = In
      ALLOCATE(ISFAC,ICONCV,ITHFLG,NOCVCO,IPHDRY)
      ZERO=0.
C
C3------READ COMMENTS AND ITEM 1.
      CALL URDCOM(IN,IOUT,LINE)
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUPWCB,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,HDRY,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPUPW,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPHDRY,R,IOUT,IN)

	WRITE(IOUT, *) 'IUPWCB, HDRY, NPUPW, IPHDRY:'
	WRITE(IOUT, *) IUPWCB, HDRY, NPUPW, IPHDRY
!
C
C3A-----WRITE ITEM 1
!      IF(IUPWCB.LT.0) WRITE(IOUT,8)
!    8 FORMAT(1X,'CONSTANT-HEAD CELL-BY-CELL FLOWS WILL BE PRINTED',
!     1  ' WHEN ICBCFL IS NOT 0')
!      IF(IUPWCB.GT.0) WRITE(IOUT,9) IUPWCB
!    9 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
      IF(NPUPW.GT.0) THEN
!         WRITE(IOUT,15) NPUPW
!   15    FORMAT(1X,I5,' Named Parameters     ')
      ELSE
         NPUPW=0
!         WRITE(IOUT,'(A)') ' No named parameters'
      END IF
C
C3B-----GET OPTIONS.
      ISFAC=0
      ICONCV=0
      ITHFLG=0
      NOCVCO=0
      NOVFC=0
      NOPCHK=0
      STOTXT=ANAME(6)
   20 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'STORAGECOEFFICIENT') THEN
         ISFAC=1
         STOTXT=ANAME(9)
!         WRITE(IOUT,21)
!   21    FORMAT(1X,'STORAGECOEFFICIENT OPTION:',/,
!     1     1X,'Read storage coefficient rather than specific storage'
!     2     1X,'Option not supported in UPW Package for MODFLOW-NWT')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'CONSTANTCV') THEN
         ICONCV=1
!         WRITE(IOUT,23)
!   23    FORMAT(1X,'CONSTANTCV OPTION:',/,1X,'Constant vertical',
!     1         ' conductance for convertible layers'
!     2     1X,'Option not supported in UPW Package for MODFLOW-NWT')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'THICKSTRT') THEN
         ITHFLG=1
!         WRITE(IOUT,25)
!   25    FORMAT(1X,'THICKSTRT OPTION:',/,1X,'Negative LAYTYP indicates',
!     1 ' confined layer with thickness computed from STRT-BOT'
!     2     1X,'Option not supported in UPW Package for MODFLOW-NWT')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOCVCORRECTION') THEN
         NOCVCO=1
!         WRITE(IOUT,27)
!   27    FORMAT(1X,'NOCVCORRECTION OPTION:',/,1X,
!     1    'Do not adjust vertical conductance when applying',
!     2              ' the vertical flow correction'
!     2     1X,'Option not supported, no vertical flow correction '
!     4        ' in UPW Package')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOVFC') THEN
         NOVFC=1
         NOCVCO=1
!         WRITE(IOUT,29)
!   29    FORMAT(1X,'NOVFC OPTION:',/,1X,
!     1    'vertical flow correction does not apply in UPW Package')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOPARCHECK') THEN
         NOPCHK=1
!         WRITE(IOUT,30)
!   30    FORMAT(1X,'NOPARCHECK  OPTION:',/,1X,
!     1    'For data defined by parameters, do not check to see if ',
!     2        'parameters define data at all cells')
      END IF
      
	WRITE(IOUT, *) 'ISFAC, ICONCV, ITHFLG, NOCVCO, NOVFC, NOPCHK:'
	WRITE(IOUT, *) ISFAC, ICONCV, ITHFLG, NOCVCO, NOVFC, NOPCHK
      
      IF(LLOC.LT.200) GO TO 20
C
C4------ALLOCATE AND READ LAYTYP, LAYAVG, CHANI, LAYVKA, LAYWET, LAYSTRT.
      ALLOCATE (Sn(Numcell), So(Numcell))
      Sn = 0.0D0
      So = 0.0D0
      ALLOCATE(LAYTYPUPW(NLAY))
      ALLOCATE(LAYAVG(NLAY))
      ALLOCATE(CHANI(NLAY))
      ALLOCATE(LAYVKAUPW(NLAY))
      ALLOCATE(LAYWET(NLAY))
      ALLOCATE(LAYSTRT(NLAY))
      ALLOCATE(IBOUND2(NCOL,NROW,NLAY))
      IBOUND2 = 0
      READ(IN,*) (LAYTYPUPW(K),K=1,NLAY)
      READ(IN,*) (LAYAVG(K),K=1,NLAY)
      READ(IN,*) (CHANI(K),K=1,NLAY)
      READ(IN,*) (LAYVKAUPW(K),K=1,NLAY)
      READ(IN,*) (LAYWET(K),K=1,NLAY)
C
C4A-----PRINT A TABLE OF VALUES FOR LAYTYP, LAYAVG, CHANI, LAYVKA, LAYWET.
!      WRITE(IOUT,47)
!   47 FORMAT(1X,/3X,'LAYER FLAGS:',/1X,
!     1 'LAYER       LAYTYP          LAYAVG    CHANI    ',
!     2 '       LAYVKA           LAYWET',/1X,75('-'))
      DO 50 K=1,NLAY

      WRITE(IOUT,*) 
	1  'K,LAYTYPUPW(K),LAYAVG(K),CHANI(K),LAYVKAUPW(K),LAYWET(K):'
      WRITE(IOUT,*) K,LAYTYPUPW(K),LAYAVG(K),CHANI(K),LAYVKAUPW(K),
     1     LAYWET(K)

!      WRITE(IOUT,48) K,LAYTYPUPW(K),LAYAVG(K),CHANI(K),LAYVKAUPW(K),
!     1               LAYWET(K)
!   48 FORMAT(1X,I4,2I14,1PE14.3,2I14)
C
C4A1----SET GLOBAL HEAD-DEPENDENT TRANSMISSIVITY AND STORAGE FLAGS.
      IF (LAYTYPUPW(K).GT.0) THEN
        LAYHDT(K)=1
        LAYHDS(K)=1
      ELSE
        LAYHDT(K)=0
        LAYHDS(K)=0
      ENDIF
   50 CONTINUE
C
C4A2----SET LAYSTRT AND RESET LAYTYP IF THICKSTRT OPTION IS ACTIVE.
      DO 60 K=1,NLAY
      LAYSTRT(K)=0
      IF(LAYTYPUPW(K).LT.0 .AND. ITHFLG.NE.0) THEN
         LAYSTRT(K)=1
         LAYTYPUPW(K)=0
         LAYHDT(K)=0
         LAYHDS(K)=0
!         WRITE(IOUT,57) K
!   57    FORMAT(1X,'Layer',I5,
!     1' is confined because LAYTYP<0 and THICKSTRT option is active')
      END IF
   60 CONTINUE
C
C4B-----BASED ON LAYTYP, LAYAVG, CHANI, LAYWET, COUNT THE NUMBER OF EACH
C4B-----TYPE OF 2-D ARRAY; CHECK VALUES FOR CONSISTENCY; AND SETUP
C4B-----POINTERS IN LAYTYP, CHANI, AND LAYWET FOR CONVENIENT ACCESS
C4B-----TO SC2, HANI, and WETDRY.  PRINT INTERPRETED VALUES OF FLAGS.
      NCNVRT=0
      NHANI=0
      NWETD=0
!      WRITE(IOUT,67)
!   67 FORMAT(1X,/3X,'INTERPRETATION OF LAYER FLAGS:',/1X,
!     1  '                       INTERBLOCK     HORIZONTAL',
!     2  '    DATA IN',/1X,
!     3  '        LAYER TYPE   TRANSMISSIVITY   ANISOTROPY',
!     4  '   ARRAY VKA   WETTABILITY',/1X,
!     5  'LAYER   (LAYTYP)        (LAYAVG)      (CHANI)   ',
!     6  '  (LAYVKA)       (LAYWET)',/1X,75('-'))
      DO 100 K=1,NLAY
      IF(LAYTYPUPW(K).GT.0) THEN
         NCNVRT=NCNVRT+1
         LAYTYPUPW(K)=NCNVRT
      END IF
      IF(CHANI(K).LE.ZERO) THEN
         NHANI=NHANI+1
         CHANI(K)=-NHANI
      END IF
      IF(LAYWET(K).NE.0) THEN
!         IF(LAYTYPUPW(K).EQ.0) THEN
            WRITE(IOUT,*)
            WRITE(IOUT,*)
     1          ' LAYWET is not 0 and wetting does not apply in UPW '
            WRITE(IOUT,*) ' LAYWET must be 0 when using the UPW Package'
            CALL USTOP(' ')
!         ELSE
!            NWETD=NWETD+1
!            LAYWET(K)=NWETD
!         END IF
      END IF
      IF(LAYAVG(K).LT.0 .OR. LAYAVG(K).GT.2) THEN
         WRITE(IOUT,74) LAYAVG(K)
   74    FORMAT(1X,I8,
     1    ' IS AN INVALID LAYAVG VALUE -- MUST BE 0, 1, or 2')
         CALL USTOP(' ')
      END IF
      LAYPRN(1)=TYPNAM(1)
      IF(LAYTYPUPW(K).GT.0) LAYPRN(1)=TYPNAM(2)
      LAYPRN(2)=AVGNAM(LAYAVG(K)+1)
      IF(CHANI(K).LE.0) THEN
         LAYPRN(3)=HANNAM
      ELSE
!         WRITE(LAYPRN(3),'(1PE14.3)') CHANI(K)
      END IF
      LAYPRN(4)=VKANAM(1)
      IF(LAYVKAUPW(K).NE.0) LAYPRN(4)=VKANAM(2)
      LAYPRN(5)=WETNAM(1)
      IF(LAYWET(K).NE.0) LAYPRN(5)=WETNAM(2)
!      WRITE(IOUT,78) K,(LAYPRN(I),I=1,5)
!   78 FORMAT(1X,I4,5A)
  100 CONTINUE
C
C4C-----PRINT WETTING INFORMATION.  RGN commented out because this does not apply
!      IF(NWETD.EQ.0) THEN
!         WRITE(IOUT,13)
!   13    FORMAT(1X,/,1X,'WETTING CAPABILITY IS NOT ACTIVE IN ANY LAYER')
!         IWDFLG=0
!      ELSE
!         WRITE(IOUT,12) NWETD
!   12    FORMAT(1X,/,1X,'WETTING CAPABILITY IS ACTIVE IN',I4,' LAYERS')
!         IWDFLG=1
!         READ(IN,*) WETFCT,IWETIT,IHDWET
!         IF(IWETIT.LE.0) IWETIT=1
!         WRITE(IOUT,*) ' WETTING FACTOR=',WETFCT
!         WRITE(IOUT,*) ' WETTING ITERATION INTERVAL=',IWETIT
!         WRITE(IOUT,*) ' IHDWET=',IHDWET
!      END IF
C
C5------ALLOCATE MEMORY FOR ARRAYS.
      ALLOCATE(LAYFLG(6,NLAY))
      ALLOCATE(HKUPW(NCOL,NROW,NLAY))
      ALLOCATE(VKAUPW(NCOL,NROW,NLAY))
      IF(NCNFBD.GT.0) THEN
         ALLOCATE(VKCB(NCOL,NROW,NCNFBD))
      ELSE
         ALLOCATE(VKCB(1,1,1))
      END IF
      IF(ITRSS.NE.0) THEN
         ALLOCATE(SC1(NCOL,NROW,NLAY))
      ELSE
         ALLOCATE(SC1(1,1,1))
      END IF
      SC1 = 0.0
! RGN 6/25/09      IF(ITRSS.NE.0 .AND. NCNVRT.GT.0) THEN
      IF(ITRSS.NE.0) THEN
! RGN 6/25/09        ALLOCATE(SC2UPW(NCOL,NROW,NCNVRT))
         ALLOCATE(SC2UPW(NCOL,NROW,NLAY))
      ELSE
         ALLOCATE(SC2UPW(1,1,1))
      END IF
      SC2UPW = 0.0
      IF(NHANI.GT.0) THEN
         ALLOCATE(HANI(NCOL,NROW,NHANI))
      ELSE
         ALLOCATE(HANI(1,1,1))
      END IF
      IF(NWETD.GT.0) THEN
         ALLOCATE(WETDRY(NCOL,NROW,NWETD))
      ELSE
         ALLOCATE(WETDRY(1,1,1))
      END IF
C
C6------READ PARAMETER DEFINITIONS
      NPHK=0
      NPVKCB=0
      NPVK=0
      NPVANI=0
      NPSS=0
      NPSY=0
      NPHANI=0
      IF(NPUPW.GT.0) THEN
!         WRITE(IOUT,115)
!  115    FORMAT(/,' PARAMETERS DEFINED IN THE NWT PACKAGE')
         DO 120 K=1,NPUPW
         CALL UPARARRRP(IN,IOUT,N,1,PTYP,1,0,-1)
C   Note that NPHK and the other NP variables in
C   this group are used only as flags, not counts
         IF(PTYP.EQ.'HK') THEN
            NPHK=1
         ELSE IF(PTYP.EQ.'HANI') THEN
C6A-----WHEN A HANI PARAMETER IS USED, THEN ALL HORIZONTAL ANISOTROPY
C6A-----MUST BE DEFINED USING PARAMETERS.  ENSURE THAT ALL CHANI <= 0
            DO 118 I = 1, NLAY
              IF (CHANI(I).GT.0.0) THEN
                WRITE(IOUT,117)
  117           FORMAT(/,
     &'ERROR: WHEN A HANI PARAMETER IS USED, CHANI FOR ALL LAYERS',/,
     &'MUST BE LESS THAN OR EQUAL TO 0.0 -- STOP EXECUTION',
     &' (GWF2UPW1AR)')
                CALL USTOP(' ')
              ENDIF
  118       CONTINUE
            NPHANI=1
         ELSE IF(PTYP.EQ.'VKCB') THEN
            NPVKCB=1
         ELSE IF(PTYP.EQ.'VK') THEN
            NPVK=1
!            CALL SGWF2UPWCK(IOUT,N,'VK  ')
         ELSE IF(PTYP.EQ.'VANI') THEN
            NPVANI=1
!            CALL SGWF2UPWCK(IOUT,N,'VANI')
         ELSE IF(PTYP.EQ.'SS') THEN
            NPSS=1
         ELSE IF(PTYP.EQ.'SY') THEN
            NPSY=1
         ELSE
            WRITE(IOUT,*) ' Invalid parameter type for UPW Package'
            CALL USTOP(' ')
         END IF
  120    CONTINUE
      END IF
C
C7------DEFINE DATA FOR EACH LAYER -- VIA READING OR NAMED PARAMETERS.
      DO 200 K=1,NLAY
      KK=K
C
C7A-----DEFINE HORIZONTAL HYDRAULIC CONDUCTIVITY (HK)
      IF(NPHK.EQ.0) THEN
         CALL U2DREL(HKUPW(:,:,KK),ANAME(1),NROW,NCOL,KK,IN,IOUT)
      ELSE
         READ(IN,*) LAYFLG(1,K)
!         WRITE(IOUT,121) ANAME(1),K,LAYFLG(1,K)
!  121    FORMAT(1X,/1X,A,' FOR LAYER',I4,
!     1   ' WILL BE DEFINED BY PARAMETERS',/1X,'(PRINT FLAG=',I4,')')
!         CALL UPARARRSUB1(HKUPW(:,:,KK),NCOL,NROW,KK,'HK',
!     1      IOUT,ANAME(1),LAYFLG(1,KK))
         IF(NOPCHK.EQ.0) CALL UPARARRCK(BUFF,IBOUND,IOUT,K,NCOL,NLAY,
     1      NROW,'HK  ')
      END IF
C
C7B-----READ HORIZONTAL ANISOTROPY IF CHANI IS NON-ZERO
      IF(CHANI(K).LE.ZERO) THEN
        KHANI=-CHANI(K)
        IF(NPHANI.EQ.0) THEN
           CALL U2DREL(HANI(:,:,KHANI),ANAME(2),NROW,NCOL,KK,IN,IOUT)
        ELSE
           READ(IN,*) LAYFLG(6,K)
!           WRITE(IOUT,121) ANAME(2),K,LAYFLG(6,K)
!           CALL UPARARRSUB1(HANI(:,:,KHANI),NCOL,NROW,KK,'HANI',
!     1      IOUT,ANAME(2),LAYFLG(6,KK))
           IF(NOPCHK.EQ.0) CALL UPARARRCK(BUFF,IBOUND,IOUT,K,NCOL,
     1      NLAY,NROW,'HANI')
        END IF
      END IF
C
C7C-----DEFINE VERTICAL HYDRAULIC CONDUCTIVITY OR HORIZONTAL TO VERTICAL
C7C-----ANISOTROPY (VKA).
      IANAME=3
      PTYP='VK'
      IF(LAYVKAUPW(K).NE.0) THEN
         IANAME=4
         PTYP='VANI'
      END IF
      IF(NPVK.EQ.0 .AND. NPVANI.EQ.0) THEN
         CALL U2DREL(VKAUPW(:,:,KK),ANAME(IANAME),NROW,NCOL,KK,IN,IOUT)
      ELSE
         READ(IN,*) LAYFLG(2,K)
!         WRITE(IOUT,121) ANAME(IANAME),K,LAYFLG(2,K)
!         CALL UPARARRSUB1(VKAUPW(:,:,KK),NCOL,NROW,KK,PTYP,IOUT,
!     1                       ANAME(IANAME),LAYFLG(2,KK))
         IF(NOPCHK.EQ.0) CALL UPARARRCK(BUFF,IBOUND,IOUT,K,NCOL,NLAY,
     1                       NROW,PTYP)
      END IF
C
C7D-----DEFINE SPECIFIC STORAGE OR STORAGE COEFFICIENT IN ARRAY SC1 IF TRANSIENT.
      IF(ITRSS.NE.0) THEN
         IF(NPSS.EQ.0) THEN
            CALL U2DREL(SC1(:,:,KK),STOTXT,NROW,NCOL,KK,IN,IOUT)
         ELSE
            READ(IN,*) LAYFLG(3,K)
!            WRITE(IOUT,121) STOTXT,K,LAYFLG(3,K)
!            CALL UPARARRSUB1(SC1(:,:,KK),NCOL,NROW,KK,'SS',
!     1           IOUT,STOTXT,LAYFLG(3,KK))
            IF(NOPCHK.EQ.0) CALL UPARARRCK(BUFF,IBOUND,IOUT,K,NCOL,
     1           NLAY,NROW,'SS  ')
         END IF
!         IF(ISFAC.EQ.0) THEN
!            CALL SGWF2UPWSC(SC1(:,:,KK),KK,1)
!         ELSE
!            CALL SGWF2UPWSC(SC1(:,:,KK),KK,0)
!         END IF
      END IF
C
C7E-----DEFINE SPECIFIC YIELD IN ARRAY SC2 IF TRANSIENT AND LAYER IS
C7E-----IS CONVERTIBLE.
      IF(LAYTYPUPW(K).GT.0) THEN
         IF(ITRSS.NE.0) THEN
            IF(NPSY.EQ.0) THEN
             CALL U2DREL(SC2UPW(:,:,LAYTYPUPW(K)),ANAME(7),NROW,NCOL,KK,
     1                 IN,IOUT)
            ELSE
               READ(IN,*) LAYFLG(4,K)
!               WRITE(IOUT,121) ANAME(7),K,LAYFLG(4,K)
!               CALL UPARARRSUB1(SC2UPW(:,:,LAYTYPUPW(K)),NCOL,
!     1         NROW,KK,'SY',IOUT,ANAME(7),LAYFLG(4,KK))
               IF(NOPCHK.EQ.0) CALL UPARARRCK(BUFF,IBOUND,IOUT,K,
     1           NCOL,NLAY,NROW,'SY  ')
            END IF
!            CALL SGWF2UPWSC(SC2UPW(:,:,LAYTYPUPW(K)),KK,0)
         END IF
      ELSE
        IF(ITRSS.NE.0) THEN
          DO J=1,NROW
            DO I=1,NCOL
              SC2UPW(I,J,KK) = 0.0D0    !SC1(I,J,KK)
            END DO
          END DO
        END IF
      END IF
C
C7F-----READ CONFINING BED VERTICAL HYDRAULIC CONDUCTIVITY (VKCB)
      IF(LAYCBD(K).NE.0) THEN
         IF(NPVKCB.EQ.0) THEN
            CALL U2DREL(VKCB(:,:,LAYCBD(K)),ANAME(5),NROW,NCOL,KK,IN,
     1             IOUT)
         ELSE
            READ(IN,*) LAYFLG(5,K)
!            WRITE(IOUT,121) ANAME(5),K,LAYFLG(5,K)
!            CALL UPARARRSUB1(VKCB(:,:,LAYCBD(K)),NCOL,NROW,KK,
!     1         'VKCB',IOUT,ANAME(5),LAYFLG(5,KK))
            IF(NOPCHK.EQ.0) CALL UPARARRCK(BUFF,IBOUND,IOUT,K,NCOL,
     1         NLAY,NROW,'VKCB')
         END IF
      END IF
C
C7G-----READ WETDRY CODES IF WETTING CAPABILITY HAS BEEN INVOKED
C7G-----(LAYWET NOT 0).
      IF(LAYWET(K).NE.0) THEN
         CALL U2DREL(WETDRY(:,:,LAYWET(K)),ANAME(8),NROW,NCOL,KK,IN,
     1            IOUT)
      END IF
  200 CONTINUE
C
C8------PREPARE AND CHECK LPF DATA.
!      CALL SGWF2UPWN()
C9------Calculate constant part of conductance. Conductance includes
C       cell thickness for confined conditions.
!      DO K=1,NLAY     
!        IF(LAYAVG(K).EQ.0) THEN
!          IF ( LAYTYPUPW(K).GT.0 ) THEN
!            CALL SGWF2UPW1HHARM(K)
!          ELSE
!            CALL SGWF2UPW1HHARMCON(K)
!          END IF
!        ELSE IF(LAYAVG(K).EQ.1) THEN
!          IF ( LAYTYPUPW(K).GT.0 ) THEN
!            CALL SGWF2UPW1HLOG(K)
!          ELSE
!            CALL SGWF2UPW1HLOGCON(K)
!          END IF
!        ELSE IF(LAYAVG(K).EQ.2) THEN
!          IF ( LAYTYPUPW(K).GT.0 ) THEN
!            CALL SGWF2UPW1HUNCNF(K)
!          ELSE
!            CALL SGWF2UPW1HUNCNFCON(K)
!          END IF
!        END IF
!        CALL SGWF2UPW1VCOND(K)
!      END DO
!10-----SAVE POINTERS FOR GRID AND RETURN
      CALL SGWF2UPW1PSV(Igrid)
!
!11-----RETURN
      END SUBROUTINE GWF2UPW1AR
!     -----------------------------------------------------------------
!      SUBROUTINE SGWF2UPWN()
C     ******************************************************************
C     INITIALIZE AND CHECK UPW DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
!
!      SUBROUTINE GWF2UPWFMS(KITER,KSTP,KPER,IGRID)
C     ******************************************************************
C     ADD LEAKAGE CORRECTION AND STORAGE TO HCOF AND RHS, AND CALCULATE
C     CONDUCTANCE AS REQUIRED.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C
!      SUBROUTINE GWF2UPWBDADJ(KSTP,KPER,IDIR,IBDRET,
!     1                      IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
C     ******************************************************************
C     COMPUTE FLOW BETWEEN ADJACENT CELLS IN A SUBREGION OF THE GRID
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
!      SUBROUTINE GWF2UPWBDS(KSTP,KPER,IGRID)
C     ******************************************************************
C     COMPUTE STORAGE BUDGET FLOW TERM.
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
!      SUBROUTINE GWF2UPWBDCH(KSTP,KPER,IGRID)
C     ******************************************************************
C     COMPUTE FLOW FROM CONSTANT-HEAD CELLS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
!      SUBROUTINE SGWF2UPWSC(SC,K,ISPST)
C     ******************************************************************
C     COMPUTE STORAGE CAPACITY
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
!
      !SUBROUTINE SGWF2UPWCK(IOUT,NP,PTYP)
C     ******************************************************************
C     CHECK THAT JUST-DEFINED PARAMETER OF TYPE 'VK' OR 'VANI' IS USED
C     CONSISTENTLY WITH LAYVKA ENTRIES FOR LAYERS LISTED IN CLUSTERS FOR
C     THE PARAMETER
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
!
!      SUBROUTINE SGWF2UPW1HHARM(K)
C     ******************************************************************
C      COMPUTE THE CONSTANT PART OF HORIZONTAL CONDUCTANCE BASED ON THE
C      HARMONIC AVEARGE K FOR ADJACENT CELLS.
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
!      SUBROUTINE SGWF2UPW1HLOG(K)
C     ******************************************************************
C-----COMPUTE CONSTANT PART OF HORIZONTAL CONDUCTANCE USING LOGARITHMIC 
C-----MEAN HYDRAULIC CONDUCTIVITY -- ACTIVATED BY LAYAVG=1
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
!
!      SUBROUTINE SGWF2UPW1HUNCNF(K)
C     ******************************************************************
C-----COMPUTE CONSTANT PART OF HORIZONTAL CONDUCTANCE USING ARITHMETIC 
C-----MEAN CELL THICKNESS AND LOGARITHMIC MEAN HYDRAULIC CONDUCTIVITY. 
C-----THIS IS DIFFERENT FROM SGWF2UPW1HLOG FOR CONFINED LAYERS. 
C-----ACTIVATED BY LAYAVG=2
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
!
!      SUBROUTINE SGWF2UPW1HHARMCON(K)
C     ******************************************************************
C      COMPUTE THE HORIZONTAL CONDUCTANCE FOR CONFINED CELLS BASED ON THE
C      HARMONIC AVEARGE K FOR ADJACENT CELLS.
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
C
!      SUBROUTINE SGWF2UPW1HLOGCON(K)
C     ******************************************************************
C-----COMPUTE THE HORIZONTAL CONDUCTANCE FOR CONFINED CELLS BASED ON THE 
C-----LOGARITHMIC  MEAN HYDRAULIC CONDUCTIVITY -- ACTIVATED BY LAYAVG=1
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
!
!      SUBROUTINE SGWF2UPW1HUNCNFCON(K)
C     ******************************************************************
C-----COMPUTE HORIZONTAL CONDUCTANCE FOR CONFINED CELLS USING ARITHMETIC 
C-----MEAN CELL THICKNESS AND LOGARITHMIC MEAN HYDRAULIC CONDUCTIVITY.  
C-----ACTIVATED BY LAYAVG=2
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------

!      DOUBLE PRECISION FUNCTION SAT_THICK(Hup,Ttop,Bbot,il)
! RETURNS SATURATED THICKNESS OF CELL BASED ON SMOOTH FUNCTION
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!
!      SUBROUTINE SGWF2UPW1VCOND(K)
C     ******************************************************************
C     COMPUTE VERTICAL BRANCH CONDUCTANCE BETWEEN A LAYER AND THE NEXT
C     LOWER LAYER FROM VERTICAL HYDRAULIC CONDUCTIVITY.
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
!
!     -----------------------------------------------------------------
!     Updates saturation for previous time step.
!      SUBROUTINE GWF2UPW1AD(IGRID)
!
!
!     SUBROUTINE GWF2UPWUPDATE. UPDATE VALUES AFTER OUTER ITERATION.
!      SUBROUTINE GWF2UPWUPDATE(Itest, Igrid)
!     -----------------------------------------------------------------
!     Updates saturation for latest iteration.

!      SUBROUTINE Sn_update()
!     -----------------------------------------------------------------
!
!      FUNCTION DHORIZUPW(Hup, Ttop, Bbot, il)
! RETURNS DERIVATIVE OF HORIZONTAL CONDUCTANCE BASED ON SMOOTH FUNCTION
! FUNCTION IS CALCULATED IN UPW PACKAGE IN SUBROUTINE SAT_THICK
!
      SUBROUTINE GWF2UPW1DA(Igrid)
      USE GWFUPWMODULE
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
      INTEGER Igrid    
!     ------------------------------------------------------------------
! Deallocate UPW data.
        DEALLOCATE(Gwfupwdat(IGRID)%Sn)
        DEALLOCATE(Gwfupwdat(IGRID)%So)
        DEALLOCATE(Gwfupwdat(IGRID)%IUPWCB)
        DEALLOCATE(Gwfupwdat(IGRID)%IWDFLG)
        DEALLOCATE(Gwfupwdat(IGRID)%IWETIT)
        DEALLOCATE(Gwfupwdat(IGRID)%IHDWET)
        DEALLOCATE(Gwfupwdat(IGRID)%IPHDRY)
        DEALLOCATE(Gwfupwdat(IGRID)%ISFAC)
        DEALLOCATE(Gwfupwdat(IGRID)%ICONCV)
        DEALLOCATE(Gwfupwdat(IGRID)%ITHFLG)
        DEALLOCATE(Gwfupwdat(IGRID)%NOCVCO)
        DEALLOCATE(Gwfupwdat(IGRID)%NOVFC)
        DEALLOCATE(Gwfupwdat(IGRID)%WETFCT)
        DEALLOCATE(Gwfupwdat(IGRID)%LAYTYPUPW)
        DEALLOCATE(Gwfupwdat(IGRID)%LAYAVG)
        DEALLOCATE(Gwfupwdat(IGRID)%CHANI)
        DEALLOCATE(Gwfupwdat(IGRID)%LAYVKAUPW)
        DEALLOCATE(Gwfupwdat(IGRID)%LAYWET)
        DEALLOCATE(Gwfupwdat(IGRID)%LAYSTRT)
        DEALLOCATE(Gwfupwdat(IGRID)%LAYFLG)
        DEALLOCATE(Gwfupwdat(IGRID)%VKAUPW)
        DEALLOCATE(Gwfupwdat(IGRID)%VKCB)
        DEALLOCATE(Gwfupwdat(IGRID)%SC1)
        DEALLOCATE(Gwfupwdat(IGRID)%SC2UPW)
        DEALLOCATE(Gwfupwdat(IGRID)%HANI)
        DEALLOCATE(Gwfupwdat(IGRID)%WETDRY)
        DEALLOCATE(Gwfupwdat(IGRID)%HKUPW)
        DEALLOCATE(Gwfupwdat(IGRID)%IBOUND2)
      END SUBROUTINE GWF2UPW1DA
 
 
 
      SUBROUTINE SGWF2UPW1PNT(Igrid)
      USE GWFUPWMODULE
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
      INTEGER Igrid   
!     ------------------------------------------------------------------
! Cell property data
        Sn=>Gwfupwdat(IGRID)%Sn
        So=>Gwfupwdat(IGRID)%So
        IUPWCB=>Gwfupwdat(IGRID)%IUPWCB
        IWDFLG=>Gwfupwdat(IGRID)%IWDFLG
        IWETIT=>Gwfupwdat(IGRID)%IWETIT
        IHDWET=>Gwfupwdat(IGRID)%IHDWET
        IPHDRY=>Gwfupwdat(IGRID)%IPHDRY
        ISFAC=>Gwfupwdat(IGRID)%ISFAC
        ICONCV=>Gwfupwdat(IGRID)%ICONCV
        ITHFLG=>Gwfupwdat(IGRID)%ITHFLG
        NOCVCO=>Gwfupwdat(IGRID)%NOCVCO
        NOVFC=>Gwfupwdat(IGRID)%NOVFC
        WETFCT=>Gwfupwdat(IGRID)%WETFCT
        LAYTYPUPW=>Gwfupwdat(IGRID)%LAYTYPUPW
        LAYAVG=>Gwfupwdat(IGRID)%LAYAVG
        CHANI=>Gwfupwdat(IGRID)%CHANI
        LAYVKAUPW=>Gwfupwdat(IGRID)%LAYVKAUPW
        LAYWET=>Gwfupwdat(IGRID)%LAYWET
        LAYSTRT=>Gwfupwdat(IGRID)%LAYSTRT
        LAYFLG=>Gwfupwdat(IGRID)%LAYFLG
        VKAUPW=>Gwfupwdat(IGRID)%VKAUPW
        VKCB=>Gwfupwdat(IGRID)%VKCB
        SC1=>Gwfupwdat(IGRID)%SC1
        SC2UPW=>Gwfupwdat(IGRID)%SC2UPW
        HANI=>Gwfupwdat(IGRID)%HANI
        WETDRY=>Gwfupwdat(IGRID)%WETDRY
        HKUPW=>Gwfupwdat(IGRID)%HKUPW
        IBOUND2=>Gwfupwdat(IGRID)%IBOUND2
      END SUBROUTINE SGWF2UPW1PNT
!
      SUBROUTINE SGWF2UPW1PSV(Igrid)
      USE GWFUPWMODULE
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
      INTEGER Igrid
!     ------------------------------------------------------------------
! Cell property data
        Gwfupwdat(IGRID)%Sn=>Sn
        Gwfupwdat(IGRID)%So=>So
        Gwfupwdat(IGRID)%IUPWCB=>IUPWCB
        Gwfupwdat(IGRID)%IWDFLG=>IWDFLG
        Gwfupwdat(IGRID)%IWETIT=>IWETIT
        Gwfupwdat(IGRID)%IHDWET=>IHDWET
        Gwfupwdat(IGRID)%IPHDRY=>IPHDRY
        Gwfupwdat(IGRID)%ISFAC=>ISFAC
        Gwfupwdat(IGRID)%ICONCV=>ICONCV
        Gwfupwdat(IGRID)%ITHFLG=>ITHFLG
        Gwfupwdat(IGRID)%NOCVCO=>NOCVCO
        Gwfupwdat(IGRID)%NOVFC=>NOVFC
        Gwfupwdat(IGRID)%WETFCT=>WETFCT
        Gwfupwdat(IGRID)%LAYTYPUPW=>LAYTYPUPW
        Gwfupwdat(IGRID)%LAYAVG=>LAYAVG
        Gwfupwdat(IGRID)%CHANI=>CHANI
        Gwfupwdat(IGRID)%LAYVKAUPW=>LAYVKAUPW
        Gwfupwdat(IGRID)%LAYWET=>LAYWET
        Gwfupwdat(IGRID)%LAYSTRT=>LAYSTRT
        Gwfupwdat(IGRID)%LAYFLG=>LAYFLG
        Gwfupwdat(IGRID)%VKAUPW=>VKAUPW
        Gwfupwdat(IGRID)%VKCB=>VKCB
        Gwfupwdat(IGRID)%SC1=>SC1
        Gwfupwdat(IGRID)%SC2UPW=>SC2UPW
        Gwfupwdat(IGRID)%HANI=>HANI
        Gwfupwdat(IGRID)%WETDRY=>WETDRY
        Gwfupwdat(IGRID)%HKUPW=>HKUPW
        Gwfupwdat(IGRID)%IBOUND2=>IBOUND2
!
      END SUBROUTINE SGWF2UPW1PSV