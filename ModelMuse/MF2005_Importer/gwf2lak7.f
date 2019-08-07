! Time of File Save by ERB: 3/23/2004 1:57PM
Crgn&dep Revised method of computing lake depth March-August 2009. 
Crgn&dep Previously depth computed using surface area of lake at 
Crgn&dep beginning of time step but the method neglected the change 
Crgn&dep in lake area caused by a change over the time step. 
Crgn&dep This could cause substantial lake mass errors. Added explicit 
Crgn&dep lake stage calculations to determine lake seepage.
Crgn  Made EVAP, PRECIP, SEEP, and SEEP3 double precision Nov. 6, 2006
Cdep  Converted from MODFLOW-2000 to MODFLOW-2005 May 2006, RGN and DEP
Cdep  Lake Package modified by DEP and RGN May 20 through June 29, 2006
Cdep  to compute lake outflow as a function of lake stage inside the
Cdep  FORMULATE MODULE. Lake outflow had been previously computed in the
Cdep  Streamflow-Routing Package. The Streamflow-Routing (sfr7) Package
Cdep  was also modified to remain compatible with the modifications in
Cdep  the Lake Package.
C     Modifications made February and March 21, 2004; DEP
C     Last change:  MLM & LFK  10 Oct 2003;  LFK 21 Jan 2004
C     Previous change:  ERB  13 Sep 2002    9:22 am
C
      MODULE GWFLAKMODULE
C------OLD USGS VERSION 7.1; JUNE 2006 GWFLAKMODULE; 
C------UPDATED FOR MF-2005, 1.9 RELEASE, FEBRUARY 6, 2012  
        CHARACTER(LEN=64),PARAMETER ::Version_lak =
     +'$Id: gwf2lak7.f 1457 2011-10-01 12:00:00Z rniswon $'
        INTEGER,SAVE,POINTER   ::NLAKES,NLAKESAR,ILKCB,NSSITR,LAKUNIT
        INTEGER,SAVE,POINTER   ::MXLKND,LKNODE,ICMX,NCLS,LWRT,NDV,NTRB,
     +                           IRDTAB
        REAL,   SAVE,POINTER   ::THETA,SSCNCR,SURFDEPTH
Cdep    Added SURFDEPTH  3/3/2009
Crgn    Added budget variables for GSFLOW CSV file
        REAL,   SAVE,POINTER   ::TOTGWIN_LAK,TOTGWOT_LAK,TOTDELSTOR_LAK
        REAL,   SAVE,POINTER   ::TOTSTOR_LAK,TOTEVAP_LAK,TOTPPT_LAK
        REAL,   SAVE,POINTER   ::TOTRUNF_LAK,TOTWTHDRW_LAK,TOTSURFIN_LAK
        REAL,   SAVE,POINTER   ::TOTSURFOT_LAK
        INTEGER,SAVE, DIMENSION(:),  POINTER ::ICS, NCNCVR, LIMERR, 
     +                                         LAKTAB
        INTEGER,SAVE, DIMENSION(:,:),POINTER ::ILAKE,ITRB,IDIV,ISUB,IRK
        INTEGER,SAVE, DIMENSION(:,:,:),POINTER ::LKARR1
        REAL,   SAVE, DIMENSION(:),  POINTER ::STAGES
        DOUBLE PRECISION,SAVE,DIMENSION(:), POINTER ::STGNEW,STGOLD,
     +                                        STGITER,VOLOLDD,STGOLD2
        REAL,   SAVE, DIMENSION(:),  POINTER ::VOL,FLOB,DSRFOT
        DOUBLE PRECISION,   SAVE, DIMENSION(:),  POINTER ::PRCPLK,EVAPLK
        REAL,   SAVE, DIMENSION(:),  POINTER ::BEDLAK
        REAL,   SAVE, DIMENSION(:),  POINTER ::WTHDRW,RNF,CUMRNF
        REAL,   SAVE, DIMENSION(:),  POINTER ::CUMPPT,CUMEVP,CUMGWI
        REAL,   SAVE, DIMENSION(:),  POINTER ::CUMUZF
        REAL,   SAVE, DIMENSION(:),  POINTER ::CUMGWO,CUMSWI,CUMSWO
        REAL,   SAVE, DIMENSION(:),  POINTER ::CUMWDR,CUMFLX,CNDFCT
        REAL,   SAVE, DIMENSION(:),  POINTER ::VOLINIT
        REAL,   SAVE, DIMENSION(:),  POINTER ::BOTTMS,BGAREA,SSMN,SSMX
Cdep    Added cumulative and time step error budget arrays
        REAL,   SAVE, DIMENSION(:),  POINTER ::CUMVOL,CMLAKERR,CUMLKOUT
        REAL,   SAVE, DIMENSION(:),  POINTER ::CUMLKIN,TSLAKERR,DELVOL
crgn        REAL,   SAVE, DIMENSION(:),  POINTER ::EVAP,PRECIP,SEEP,SEEP3
        DOUBLE PRECISION,   SAVE, DIMENSION(:),  POINTER ::EVAP,PRECIP
        DOUBLE PRECISION,   SAVE, DIMENSION(:),  POINTER ::EVAP3,PRECIP3
        DOUBLE PRECISION,   SAVE, DIMENSION(:),  POINTER ::FLWITER
        DOUBLE PRECISION,   SAVE, DIMENSION(:),  POINTER ::FLWITER3
        DOUBLE PRECISION,   SAVE, DIMENSION(:),  POINTER ::SEEP,SEEP3
        DOUBLE PRECISION,   SAVE, DIMENSION(:),  POINTER ::SEEPUZ
        DOUBLE PRECISION,   SAVE, DIMENSION(:),  POINTER ::WITHDRW
        DOUBLE PRECISION,   SAVE, DIMENSION(:),  POINTER ::SURFA
        REAL,   SAVE, DIMENSION(:),  POINTER ::SURFOT,SURFIN
        REAL,   SAVE, DIMENSION(:),  POINTER ::SUMCNN,SUMCHN
        REAL,   SAVE, DIMENSION(:,:),POINTER ::CLAKE,CRNF,SILLVT
        REAL,   SAVE, DIMENSION(:,:),POINTER ::CAUG,CPPT,CLAKINIT
        REAL,   SAVE, DIMENSION(:,:,:),POINTER ::BDLKN1
Cdep  Added arrays for tracking lake budgets for dry lakes
        REAL,   SAVE, DIMENSION(:),  POINTER ::EVAPO,FLWIN
        REAL,   SAVE, DIMENSION(:),  POINTER ::GWRATELIM
Cdep    Allocate arrays to add runoff from UZF Package
        REAL,   SAVE, DIMENSION(:),  POINTER ::OVRLNDRNF,CUMLNDRNF
Cdep    Allocate arrays for lake depth, area,and volume relations
        DOUBLE PRECISION,   SAVE, DIMENSION(:,:),  POINTER ::DEPTHTABLE
        DOUBLE PRECISION,   SAVE, DIMENSION(:,:),  POINTER ::AREATABLE
        DOUBLE PRECISION,   SAVE, DIMENSION(:,:),  POINTER ::VOLUMETABLE
Cdep    Allocate space for three dummy arrays used in GAGE Package
C         when Solute Transport is active
        REAL,   SAVE, DIMENSION(:,:),POINTER ::XLAKES,XLAKINIT,XLKOLD
Crsr    Allocate arrays in BD subroutine
        INTEGER,SAVE, DIMENSION(:),  POINTER ::LDRY,NCNT,NCNST,KSUB
        INTEGER,SAVE, DIMENSION(:),  POINTER ::MSUB1
        INTEGER,SAVE, DIMENSION(:,:),POINTER ::MSUB
        REAL,   SAVE, DIMENSION(:),  POINTER ::FLXINL,VOLOLD,GWIN,GWOUT
        REAL,   SAVE, DIMENSION(:),  POINTER ::DELH,TDELH,SVT,STGADJ
      TYPE GWFLAKTYPE
        INTEGER,      POINTER   ::NLAKES,NLAKESAR,ILKCB,NSSITR,LAKUNIT
        INTEGER,      POINTER   ::MXLKND,LKNODE,ICMX,NCLS,LWRT,NDV,NTRB,
     +                            IRDTAB
Cdep    Added SURFDEPTH 3/3/2009
        REAL,         POINTER   ::THETA,SSCNCR,SURFDEPTH
Crgn    Added budget variables for GSFLOW CSV file
        REAL,         POINTER   ::TOTGWIN_LAK,TOTGWOT_LAK,TOTDELSTOR_LAK
        REAL,         POINTER   ::TOTSTOR_LAK,TOTEVAP_LAK,TOTPPT_LAK
        REAL,         POINTER   ::TOTRUNF_LAK,TOTWTHDRW_LAK
        REAL,         POINTER   ::TOTSURFOT_LAK,TOTSURFIN_LAK
        INTEGER,      DIMENSION(:),  POINTER ::ICS, NCNCVR, LIMERR, 
     +                                         LAKTAB
        INTEGER,      DIMENSION(:,:),POINTER ::ILAKE,ITRB,IDIV,ISUB,IRK
        INTEGER,      DIMENSION(:,:,:),POINTER ::LKARR1
        REAL,         DIMENSION(:),  POINTER ::STAGES
        DOUBLE PRECISION,DIMENSION(:),POINTER ::STGNEW,STGOLD,STGITER,
     +                                          STGOLD2 
        DOUBLE PRECISION,DIMENSION(:),POINTER :: VOLOLDD
        REAL,         DIMENSION(:),  POINTER ::VOL,FLOB, DSRFOT
        DOUBLE PRECISION,DIMENSION(:),  POINTER ::PRCPLK,EVAPLK
        REAL,         DIMENSION(:),  POINTER ::BEDLAK
        REAL,         DIMENSION(:),  POINTER ::WTHDRW,RNF,CUMRNF
        REAL,         DIMENSION(:),  POINTER ::CUMPPT,CUMEVP,CUMGWI
        REAL,         DIMENSION(:),  POINTER ::CUMUZF
        REAL,         DIMENSION(:),  POINTER ::CUMGWO,CUMSWI,CUMSWO
        REAL,         DIMENSION(:),  POINTER ::CUMWDR,CUMFLX,CNDFCT
        REAL,         DIMENSION(:),  POINTER ::VOLINIT
        REAL,         DIMENSION(:),  POINTER ::BOTTMS,BGAREA,SSMN,SSMX
Cdep    Added cumulative and time step error budget arrays
        REAL,         DIMENSION(:),  POINTER ::CUMVOL,CMLAKERR,CUMLKOUT
        REAL,         DIMENSION(:),  POINTER ::TSLAKERR,DELVOL,CUMLKIN 
Crgn        REAL,         DIMENSION(:),  POINTER ::EVAP,PRECIP,SEEP,SEEP3
        DOUBLE PRECISION,DIMENSION(:),  POINTER :: EVAP,PRECIP
        DOUBLE PRECISION,DIMENSION(:),  POINTER :: EVAP3,PRECIP3
        DOUBLE PRECISION,DIMENSION(:),  POINTER :: FLWITER
        DOUBLE PRECISION,DIMENSION(:),  POINTER :: FLWITER3
        DOUBLE PRECISION,DIMENSION(:),  POINTER :: SEEP,SEEP3
        DOUBLE PRECISION,DIMENSION(:),  POINTER :: SEEPUZ
        DOUBLE PRECISION,DIMENSION(:),  POINTER :: WITHDRW  
        DOUBLE PRECISION,DIMENSION(:),  POINTER :: SURFA
        REAL,         DIMENSION(:),  POINTER ::SURFIN,SURFOT
        REAL,         DIMENSION(:),  POINTER ::SUMCNN,SUMCHN
        REAL,         DIMENSION(:,:),POINTER ::CLAKE,CRNF,SILLVT
        REAL,         DIMENSION(:,:),POINTER ::CAUG,CPPT,CLAKINIT
        REAL,         DIMENSION(:,:,:),POINTER ::BDLKN1
Cdep  Added arrays for tracking lake budgets for dry lakes
        REAL,         DIMENSION(:),  POINTER ::EVAPO,FLWIN
        REAL,         DIMENSION(:),  POINTER ::GWRATELIM
Cdep    Allocate arrays to add runoff from UZF Package
        REAL,         DIMENSION(:),  POINTER ::OVRLNDRNF,CUMLNDRNF
Cdep    Allocate arrays for lake depth, area, and volume relations
        DOUBLE PRECISION,         DIMENSION(:,:),POINTER ::DEPTHTABLE
        DOUBLE PRECISION,         DIMENSION(:,:),POINTER ::AREATABLE
        DOUBLE PRECISION,         DIMENSION(:,:),POINTER ::VOLUMETABLE
Cdep    Allocate space for three dummy arrays used in GAGE Package
C         when Solute Transport is active
        REAL,         DIMENSION(:,:),POINTER ::XLAKES,XLAKINIT,XLKOLD
Crsr    Allocate arrays in BD subroutine
        INTEGER,      DIMENSION(:),  POINTER ::LDRY,NCNT,NCNST,KSUB
        INTEGER,      DIMENSION(:),  POINTER ::MSUB1
        INTEGER,      DIMENSION(:,:),POINTER ::MSUB
        REAL,         DIMENSION(:),  POINTER ::FLXINL,VOLOLD,GWIN,GWOUT
        REAL,         DIMENSION(:),  POINTER ::DELH,TDELH,SVT,STGADJ
      END TYPE
      TYPE(GWFLAKTYPE), SAVE:: GWFLAKDAT(10)
      END MODULE GWFLAKMODULE
C
      SUBROUTINE GWF2LAK7AR(IN,IUNITSFR,IUNITGWT,IUNITUZF,NSOL,IGRID)
C
C------USGS VERSION 7.1; JUNE 2006 GWF2LAK7AR; 
C------UPDATED FOR MF-2005, FEBRUARY 6, 2012  
C     ******************************************************************
C     INITIALIZE POINTER VARIABLES USED BY SFR1 TO SUPPORT LAKE3 AND
C     GAGE PACKAGES AND THE GWT PROCESS
C     ******************************************************************
C
      USE GWFLAKMODULE
      USE GLOBAL,       ONLY: IOUT, NCOL, NROW, NLAY, IFREFM, ITRSS,
     +                        NODES
      USE GWFSFRMODULE, ONLY: NSS
C
C      ******************************************************************
C      ALLOCATE ARRAY STORAGE FOR LAKES
C      ******************************************************************
C
C      ------------------------------------------------------------------
C      SPECIFICATIONS:
       CHARACTER (LEN=40):: CARD 
       CHARACTER*200 line
C      ------------------------------------------------------------------
Crsr  Allocate lake variables used by SFR even if lakes not active so that
C       argument lists are defined     
      ALLOCATE (NLAKES, NLAKESAR,THETA,LAKUNIT)
      NLAKES = 0
      LAKUNIT = IN
      NLAKESAR = 1
      THETA = 0.0
      IF (IN.GT.0) THEN
Cdep added SURFDEPTH 3/3/2009
        ALLOCATE (ILKCB, NSSITR, SSCNCR, SURFDEPTH)
        ALLOCATE (MXLKND, LKNODE, ICMX, NCLS, LWRT, NDV, NTRB)
        ALLOCATE (IRDTAB)
C
C1------IDENTIFY PACKAGE AND INITIALIZE LKNODE.
      WRITE(IOUT,*) 'LAK:'
!      WRITE(IOUT,1) IN
      LKNODE=0
Cdep  initialize number of iterations and closure criteria to zero.
      DUM = 0.0
      NSSITR = 0
      SSCNCR = 0.0
      SURFDEPTH = 0.0
!
      lloc = 1
      IRDTAB = 0
      CALL URDCOM(In, IOUT, line)
! Check for alternate option to specifiy stage/vol/area tables.
      CALL UPARLSTAL(IN,IOUT,LINE,NPP,MXVL)
      lloc = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'TABLEINPUT') THEN
         IRDTAB = 1
         WRITE(IOUT,*) 'TABLEINPUT:'
!         WRITE(IOUT,32)
!   32  FORMAT(1X,I10,' Stage, volume and area relationship specified ',
!     +                'based on an external tabular input file')
      ELSE
        BACKSPACE IN
!        WRITE(IOUT,'(A)') ' Model grid will be used to develop ',
!     +                     ' volume and area relationship. '
      END IF
C
C2------READ NLAKES, ILKCB.
C
Cdep  Revised input statement to read THETA,NSSITR,SSCNCR for
Cdep  transient simulations when THETA is negative.
        IF(IFREFM.EQ.0) THEN
           READ(IN,'(2I10)')NLAKES,ILKCB
           IF (ITRSS.LE.0) THEN
              READ(IN,'(F10.2,I10,F10.2)') THETA,NSSITR,SSCNCR
              IF (THETA.LT.0.0) BACKSPACE IN
           ELSE
              READ(IN,'(F10.2)') THETA
              IF (THETA.LT.0.0) BACKSPACE IN
           END IF
        ELSE
           READ(IN,*) NLAKES,ILKCB
           IF (ITRSS.LE.0) THEN
              READ(IN,*) THETA,NSSITR,SSCNCR
              IF(THETA.LT.0.0) BACKSPACE IN
           ELSE
              READ(IN,*) THETA
              IF(THETA.LT.0.0) BACKSPACE IN
           END IF
        END IF
        WRITE(IOUT,*)'NLAKES,ILKCB:'
        WRITE(IOUT,*)NLAKES,ILKCB
        WRITE(IOUT,*) 'THETA:'
        WRITE(IOUT,*) THETA
      
Cdep    Set default values for number of iterations and closure criteria
Cdep     for transient simulations when using original version of 
Cdep     LAKE Package.
        IF(THETA.GE.0.0.AND.NSSITR.EQ.0) THEN
          NSSITR=100
          SSCNCR=1.0E-05
        ELSE IF(THETA.LT.0.0)THEN 
          THETA=ABS(THETA) 
          IF(IFREFM.EQ.0) THEN 
Cdep fixed format can't read in exponent notation 
!rsr, old data sets may not have SURFDEPTH, may need to trap this for some compilers 
            READ (IN, '(A)') CARD 
            NUMCHAR = LEN(TRIM(CARD)) 
            IF ( NUMCHAR>30 ) THEN 
              READ(CARD,'(F10.2,I10,2F10.5)') DUM,NSSITR,SSCNCR,
     +                                         SURFDEPTH 
            ELSE 
              READ(CARD,'(F10.2,I10,F10.5)') DUM,NSSITR,SSCNCR 
            ENDIF 
          ELSE 
            READ(IN,*,IOSTAT=IOS) DUM,NSSITR,SSCNCR,SURFDEPTH 
            IF ( IOS.NE.0 ) SURFDEPTH = 0.0 
          END IF
        END IF
          WRITE(IOUT,*) 'NSSITR,SSCNCR,SURFDEPTH:'
          WRITE(IOUT,*) NSSITR,SSCNCR,SURFDEPTH
Cdep   Add check to reset THETA when > 1 or < 0.5.
        IF(THETA.GT.1.0) THEN
          THETA = 1.0
        ELSE IF(THETA.LT.0.5)THEN
          THETA = 0.0
        END IF
      END IF
C
C
C  SET NLAKES ARRAY VARIABLE TO NLAKES IF NLAKES GREATER THAN 0.
      IF (NLAKES.GT.0) NLAKESAR = NLAKES
      ALLOCATE (VOL(NLAKESAR), STGOLD(NLAKESAR), STGNEW(NLAKESAR))
      ALLOCATE(STGOLD2(NLAKESAR))
      ALLOCATE (VOLOLDD(NLAKESAR))
!     ALLOCATE (VOLOLDD(NLAKESAR), VOLOLD(NLAKES), VOLINIT(NLAKES))
      ALLOCATE (STGITER(NLAKESAR))
      STGNEW = 0.0D0
      STGOLD = 0.0D0
      STGOLD2 = 0.0D0
      STGITER = 0.0D0
      VOLOLDD = 0.0D0
Cdep initialized VOLOLD and VOLINIT  6/4/2009 (VOLOLD is single precision)
!     VOLOLD = 0.0
!     VOLINIT = 0.0
      VOL = 0.0
      CALL SGWF2LAK7PSV1(IGRID)
      IF (IN.LT.1) RETURN
C
C Lakes are active
      ALLOCATE (STAGES(NLAKESAR), CLAKE(NLAKESAR,NSOL))
      STAGES = 0.0
      CLAKE = 0.0
C Budget variables for GSFLOW   
      ALLOCATE (TOTGWIN_LAK,TOTGWOT_LAK,TOTDELSTOR_LAK,TOTSTOR_LAK)  
      ALLOCATE (TOTEVAP_LAK,TOTPPT_LAK,TOTRUNF_LAK,TOTWTHDRW_LAK)
      ALLOCATE (TOTSURFIN_LAK,TOTSURFOT_LAK)
      TOTGWIN_LAK = 0.0
      TOTGWOT_LAK = 0.0
      TOTDELSTOR_LAK = 0.0
      TOTSTOR_LAK = 0.0
      TOTEVAP_LAK = 0.0
      TOTPPT_LAK = 0.0
      TOTRUNF_LAK = 0.0
      TOTWTHDRW_LAK = 0.0
      TOTSURFIN_LAK = 0.0
      TOTSURFOT_LAK = 0.0
C
C  VALUE OF MXLKND (NUMBER OF LAKE-AQUIFER INTERFACES) IS AN ESTIMATE.
C    TO SAVE MEMORY, REDUCE ITS SIZE IF APPROPRIATE.
C    IF MXLKND TOO SMALL, ERROR MESSAGE WILL BE PRINTED.
      MXLKND=NCOL*NROW*NLAY/2
      IF (NLAKES.LT.1) THEN
!        WRITE(IOUT,2)
        IN=0
        NLAKES = 0
      ELSE
!      WRITE(IOUT,5) MXLKND,NLAKES
!      IF (ILKCB.GT.0) WRITE(IOUT,7) ILKCB
!      IF (ILKCB.LE.0) WRITE(IOUT,9)
Cdep   Write THETA, NSSITR, SSCNCR
!      IF (ITRSS.GT.0) THEN
!        WRITE(IOUT,22) THETA
!        WRITE(IOUT,10) NSSITR, SSCNCR
!      ELSE
!        WRITE(IOUT,11) THETA, NSSITR, SSCNCR
!      END IF
Cdep   Changed default values for NSSITR and SSCNCR and revised
Cdep     print statements using format statement 10.   
Cdep      IF(ITRSS.LE.0.AND.NSSITR.EQ.0) NSSITR = 50
Cdep      IF(ITRSS.LE.0.AND.SSCNCR.EQ.0.0) SSCNCR = 0.01
Cdep      IF(ITRSS.EQ.0) WRITE(IOUT,23) NSSITR, SSCNCR
Cdep      IF(ITRSS.LT.0) WRITE(IOUT,24) NSSITR, SSCNCR
1     FORMAT(/1X,'LAK7 -- LAKE PACKAGE, VERSION 7, 2/06/2012',
     1' INPUT READ FROM UNIT',I3)
2       FORMAT(1X,' NUMBER OF LAKES=0, ',
     1              ' SO LAKE PACKAGE IS BEING TURNED OFF')
5     FORMAT(1X,'SPACE ALLOCATION FOR',I7,' GRID CELL FACES ADJACENT TO
     1LAKES'/1X,'MAXIMUM NUMBER OF LAKES IS',I3, ' FOR THIS SIMULATION')
7     FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE RECORDED ON UNIT',I5)
9     FORMAT(1X,'CELL-BY-CELL SEEPAGES WILL NOT BE PRINTED OR SAVED')
Cdep added format statement when starting with transient simulation
  10  FORMAT(//1X,'LAKE PACKAGE HAS BEEN MODIFIED TO ITERATIVELY ',
     1 'SOLVE FOR LAKE STAGE DURING TRANSIENT STRESS PERIODS:',/1X,
     2 'MAXIMUM NUMBER OF ITERATIONS (NSSITR) = ',I5,/1X,
     3 'CLOSURE CRITERIA FOR LAKE STAGE (SSCNCR) = ',1PE12.6,/1X,
     4 'DEFAULT VALUES FOR TRANSIENT ONLY SIMULATIONS ARE: ',
     5 'NSSITR = 100 AND SSCNCR = 0.0001',/1X,'VALUES OTHER THAN ',
     6 'DEFAULT CAN BE READ BY SPECIFYING A THETA LESS THAN ZERO ',
     7 'THEN ADDING NSSITR AND SSCNCR PER ORIGINAL INSTRUCTIONS.',/1X,
     8 'NEGATIVE THETA MUST BE LESS THAN ZERO BUT NOT MORE THAN ',
     9 'ONE. THETA IS CONVERTED TO A POSITIVE VALUE.',/1X,
     * 'MINIMUM AND MAXIMUM LAKE STAGES FOR TRANSIENT ',
     * 'SIMULATIONS ARE SET TO BOTTOM AND TOP ELEVATIONS USED TO ',
     * 'COMPUTE LAKE VOLUME, RESPECTIVELY.',//)
Cdep added format statement for steady state only simulations.
  11  FORMAT(//1X,'NEWTON ITERATION METHOD FOR COMPUTING LAKE STAGE ',
     1 'DURING STEADY-STATE STRESS PERIODS HAS BEEN MODIFIED:',/1X,
     2 'SPECIFIED THETA OF ',F6.3,' WILL BE AUTOMATICALLY CHANGED TO ',
     3 '1.0 FOR ALL STEADY STATE STRESS PERIODS.',/1X,
     4 'MAXIMUM NUMBER OF STEADY-STATE ITERATIONS (NSSITR) = ',I5,/1X,
     5 'CLOSURE CRITERIA FOR STEADY-STATE LAKE STAGE (SSCNCR) = ',
     6  1PE12.6,//)
Cdep revised print statement to note that time weighting of theta can
Cdep  vary only between 0.5 and 1 for transient simulations
Cdep   22 FORMAT(/1X,'THETA = ',F10.2,'  METHOD FOR UPDATING LAKE STAGES IN
Cdep     1ITERATIONS OF THE SOLUTION FOR AQUIFER HEADS.'/20X,'0.0 IS EXPLICI
Cdep     2T, 0.5 IS CENTERED, AND 1.0 IS FULLY IMPLICIT.')
   22 FORMAT(/1X,'THETA = ',F6.3,/1X,'THETA IS THE TIME WEIGHTING ',
     *'FACTOR FOR COMPUTING LAKE STAGE DURING TRANSIENT MODFLOW ', 
     *'TIME STEPS AND ITS DEFINITION HAS BEEN MODIFIED.',/1X,'A THETA ',
     *'OF LESS THEN 0.5 IS AUTOMATICALLY SET TO 0 AND LAKE STAGE IS ',
     *'EQUAL TO THE STAGE AT THE END OF THE PREVIOUS TIME STEP. ',/1X,
     *'TRANSIENT SIMULATIONS OF LAKE STAGE WITH THE CURRENT TIME STEP ',
     *'REQUIRES A THETA BETWEEN 0.5 AND 1.0. ',/1X,'VALUES GREATER ',
     *'THAN 1.0 ARE AUTOMATICALLY RESET TO  1.0 AND VALUES LESS ',
     *'THAN 0.5 ARE RESET TO 0.0.',/1X,'A THETA OF 0.5 REPRESENTS THE ',
     *'AVERAGE LAKE STAGE DURING A TIME STEP.',/1X,'A THETA OF 1.0 ',
     *'REPRESENTS THE LAKE STAGE AT THE END OF THE TIME STEP.',//)
Cdep   23 FORMAT(/1X,'STEADY-STATE SOLUTION FOR LAKES.'
Cdep     2/1X,'MAXIMUM NUMBER OF ITERATIONS = ',I4,3X,
Cdep     1'CONVERGENCE CRITERION = ',1PE9.2)
Cdep   24 FORMAT(/1X,'COMBINED STEADY-STATE/TRANSIENT SOLUTION FOR LAKES.'
Cdep     2/1X,'MAXIMUM NUMBER OF ITERATIONS = ',I4,3X,
Cdep     1'CONVERGENCE CRITERION = ',1PE9.2)

        ALLOCATE (ILAKE(5,MXLKND), BEDLAK(MXLKND), CNDFCT(MXLKND))
        ALLOCATE (PRCPLK(NLAKES), EVAPLK(NLAKES), WTHDRW(NLAKES))
        ALLOCATE (RNF(NLAKES), CRNF(NLAKES,NSOL), CUMRNF(NLAKES))
        ALLOCATE (CUMUZF(NLAKES))
        ALLOCATE (ISUB(NLAKES,NLAKES), SILLVT(NLAKES,NLAKES))
        ALLOCATE (IRK(2,NLAKES))
        ALLOCATE (CUMPPT(NLAKES), CUMEVP(NLAKES), CUMGWI(NLAKES))
        ALLOCATE (CUMGWO(NLAKES), CUMSWI(NLAKES), CUMSWO(NLAKES))
        ALLOCATE (CUMWDR(NLAKES), CUMFLX(NLAKES))
        ALLOCATE (CAUG(NLAKES,NSOL), CPPT(NLAKES,NSOL))
        ALLOCATE (CLAKINIT(NLAKESAR,NSOL))
        ALLOCATE (ICS(NLAKES),BOTTMS(NLAKES), BGAREA(NLAKES))
        ALLOCATE (SSMN(NLAKES), SSMX(NLAKES))
        ALLOCATE (LKARR1(NCOL,NROW,NLAY), BDLKN1(NCOL,NROW,NLAY))
        ALLOCATE (EVAP(NLAKES), PRECIP(NLAKES), SEEP(NLAKES),
     +            SEEP3(NLAKES),EVAP3(NLAKES), PRECIP3(NLAKES))
        ALLOCATE (SEEPUZ(NLAKES))
        ALLOCATE (FLWITER(NLAKES),FLWITER3(NLAKES))
        ALLOCATE (SURFA(NLAKES), SURFIN(NLAKES), SURFOT(NLAKES))
        ALLOCATE (SUMCNN(NLAKES), SUMCHN(NLAKES))
        ALLOCATE (NCNCVR(NLAKES), LIMERR(NLAKES), DSRFOT(NLAKES))
Cdep  Allocate arrays that track lake budgets for dry lakes
        ALLOCATE (EVAPO(NLAKES),WITHDRW(NLAKES),FLWIN(NLAKES))
        ALLOCATE (GWRATELIM(NLAKES))
        EVAPO = 0.0
        WITHDRW = 0.0D0
        FLWIN = 0.0
        FLWITER = 0.0D0
        FLWITER3 = 0.0D0
        EVAP = 0.0D0
        PRECIP = 0.0D0
        EVAP3 = 0.0D0
        PRECIP3 = 0.0D0
        IF ( IRDTAB.GT.0 ) THEN
          ALLOCATE(LAKTAB(NLAKES))
        ELSE
          ALLOCATE(LAKTAB(1))
        END IF
        LAKTAB = 0
!rsr    GWRATLIM= 0.0
Cdep  Allocate space for three arrays used in GAGE Package 
C       when Solute Transport is active
        ALLOCATE (XLAKES(NLAKES,1), XLAKINIT(NLAKES,1))
        ALLOCATE (XLKOLD(NLAKES,1))
crsr  Allocate arrays for BD subroutine
        ALLOCATE (LDRY(NODES), FLXINL(NLAKES))
        ALLOCATE (NCNT(NLAKES), NCNST(NLAKES))
        ALLOCATE (SVT(NLAKES), KSUB(NLAKES), STGADJ(NLAKES))
        ALLOCATE (MSUB(NLAKES,NLAKES), MSUB1(NLAKES))
        ALLOCATE (GWIN(NLAKES), GWOUT(NLAKES))
        ALLOCATE (DELH(NLAKES), TDELH(NLAKES))
Cdep   Allocate lake budget error arrays for BD subroutine 6/9/2009
        ALLOCATE (CUMVOL(NLAKES), CMLAKERR(NLAKES))
        ALLOCATE (CUMLKIN(NLAKES), CUMLKOUT(NLAKES))
        ALLOCATE (DELVOL(NLAKES), TSLAKERR(NLAKES))  
Cdep initialized VOLOLD and VOLINIT  6/4/2009 (VOLOLD is single precision)
        ALLOCATE (VOLOLD(NLAKES), VOLINIT(NLAKES))  
        VOLOLD = 0.0
        VOLINIT = 0.0
      END IF
Cdep   ALLOCATE SPACE FOR CONNECTION WITH STREAMS
      IF (IUNITSFR.LE.0) THEN
        NSSAR = 1
      ELSE
        NSSAR = NSS
      END IF
Cdep   ALLOCATE SPACE FOR FLOB ARRAY WHEN TRANSPORT ACTIVE.   
      IF (IUNITGWT.LE.0) THEN
        MXLKAR = 1
      ELSE
        MXLKAR = MXLKND
      END IF
Cdep    ALLOCATE SPACE FOR OVERLAND FLOW WHEN UNSATURATED FLOW ACTIVE.
! RGN Allocate NUZFAR to nlakes for all cases because of the GAG package 5/28/09
!      IF (IUNITUZF.LE.0) THEN
!       NUZFAR = 1
!      ELSE
        NUZFAR = NLAKESAR
!      END IF

      !rsr, what if NLAKES < 1, sanity check
      IF (NLAKES<1 ) THEN
        print *, 'nlakes dimension problem in lak7', nlakes
        stop
      ENDIF

      ALLOCATE (ITRB(NLAKES,NSSAR), IDIV(NLAKES,NSSAR))
      ALLOCATE (FLOB(MXLKAR))
      ALLOCATE (OVRLNDRNF(NUZFAR), CUMLNDRNF(NUZFAR))
Cdep    ALLOCATE SPACE FOR DEPTHTABLE, AREATABLE, AND VOLUMETABLE
      ALLOCATE (DEPTHTABLE(151,NLAKES), AREATABLE(151,NLAKES))
      ALLOCATE (VOLUMETABLE(151,NLAKES))
      ITRB = 0
      IDIV = 0
      FLOB = 0.0
      OVRLNDRNF = 0.0
      CUMLNDRNF = 0.0
      CUMUZF = 0.0
      DEPTHTABLE = 0.0D0
      AREATABLE = 0.0D0
      VOLUMETABLE = 0.0D0
Cdep initialized lake budget error arrays  6/9/2009
      CUMVOL = 0.0
      DELVOL = 0.0
      CMLAKERR = 0.0
      TSLAKERR = 0.0
      CUMLKOUT = 0.0
      CUMLKIN = 0.0
C-----SAVE POINTERS FOR GRID AND RETURN
      CALL SGWF2LAK7PSV(IGRID)
C
C11-----RETURN.
      RETURN
      END
C
      SUBROUTINE GWF2LAK7RP(IN,IUNITBCF,IUNITGWT,IUNITLPF,IUNITHUF,
     +                      IUNITSFR,IUNITUZF,KKPER,NSOL,IOUTS,IGRID)
C
C------USGS VERSION 7.1;  JUNE 2006 GWF2LAK7RP
C        REVISED FEBRUARY 6, 2012
C     ******************************************************************
C       READ INPUT DATA FOR THE LAKE PACKAGE.
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFLAKMODULE
      USE GLOBAL,       ONLY: IOUT, NCOL, NROW, NLAY, IFREFM, IBOUND,
     +                        LBOTM, BOTM, DELR, DELC, ISSFLG
C     USE GWFSFRMODULE, ONLY: NSS
C     ------------------------------------------------------------------
C     FUNCTIONS
C     ------------------------------------------------------------------
      DOUBLE PRECISION VOLTERP
      EXTERNAL VOLTERP
C     ------------------------------------------------------------------
      CHARACTER*24 ANAME(2)
!     CHARACTER*30 LFRMAT
!dep  added STGINIT as double precision
      DOUBLE PRECISION STGINIT      
      DATA ANAME(1)/'           LAKE ID ARRAY'/
      DATA ANAME(2)/'  LAKEBED LEAKANCE ARRAY'/
C
C     ------------------------------------------------------------------
C------SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2LAK7PNT(IGRID)
	WRITE(IOUT,*)'LAK:'
C
C1A-----IF MXLKND IS LESS THAN 1, THEN LAKE IS INACTIVE. RETURN.
      IF(MXLKND.LT.1) RETURN
C
C1A1----READ INITIAL CONDITIONS FOR ALL LAKES (ONLY READ ONCE)
      ISS = ISSFLG(KKPER)
      IF (KKPER.EQ.1) THEN
!         WRITE (IOUT,19)
!         IF(ISS.NE.0) WRITE (IOUT,20)
!         IF(ISS.EQ.0) WRITE (IOUT,820)
         IF (IUNITGWT.EQ.0) THEN
            DO 30 LM=1,NLAKES
               IF (IFREFM.EQ.0) THEN
                 IF ( IRDTAB.GT.0 ) THEN
                   IF(ISS.NE.0) READ (IN,'(3F10.4,I5)') STAGES(LM),
     1                                SSMN(LM),SSMX(LM),LAKTAB(LM)
                   IF(ISS.EQ.0) READ (IN,'(F10.4,I5)') STAGES(LM),
     2                                               LAKTAB(LM)
                 ELSE
                   IF(ISS.NE.0) READ (IN,'(3F10.4)') STAGES(LM),
     1              SSMN(LM),SSMX(LM)
                   IF(ISS.EQ.0) READ (IN,'(F10.4)') STAGES(LM)
                 END IF
               ELSE
                 IF ( IRDTAB.GT.0 ) THEN
                   IF(ISS.NE.0) READ (IN,*)STAGES(LM),SSMN(LM),SSMX(LM),
     1                                     LAKTAB(LM)
                   IF(ISS.EQ.0) READ (IN,*) STAGES(LM),LAKTAB(LM)
                 ELSE
                   IF(ISS.NE.0) READ (IN,*) STAGES(LM),SSMN(LM),SSMX(LM)
                   IF(ISS.EQ.0) READ (IN,*) STAGES(LM)
                 END IF
               END IF
            IF(ISS.NE.0) 
	1	    WRITE (IOUT,*) 'LM,STAGES(LM),SSMN(LM),SSMX(LM):'
            IF(ISS.NE.0) WRITE (IOUT,*) LM,STAGES(LM),SSMN(LM),SSMX(LM)
            IF(ISS.EQ.0) WRITE (IOUT,*) 'LM,STAGES(LM):'
            IF(ISS.EQ.0) WRITE (IOUT,*) LM,STAGES(LM)
!            IF(ISS.NE.0) WRITE (IOUT,22) LM,STAGES(LM),SSMN(LM),SSMX(LM)
!            IF(ISS.EQ.0) WRITE (IOUT,22) LM,STAGES(LM)
 30         CONTINUE
         ELSE
            WRITE (IOUTS,*) 'NSOL:'
            WRITE (IOUTS,*) NSOL
!            WRITE (LFRMAT,23) NSOL  !LFRMAT is not set
            DO 35 LM=1,NLAKES
               IF (IFREFM.EQ.0) THEN
                 IF ( IRDTAB.GT.0 ) THEN
                   IF(ISS.NE.0) READ(IN,'(100F10.4)') STAGES(LM),
     1                SSMN(LM),SSMX(LM),(CLAKE(LM,ISOL),ISOL=1,NSOL),
     2                LAKTAB(LM)
                   IF(ISS.EQ.0) READ (IN,'(100F10.4)') STAGES(LM),
     1                          (CLAKE(LM,ISOL),ISOL=1,NSOL),LAKTAB(LM)
                 ELSE
                   IF(ISS.NE.0) READ(IN,'(100F10.4)') STAGES(LM),
     1                SSMN(LM),SSMX(LM),(CLAKE(LM,ISOL),ISOL=1,NSOL)
                   IF(ISS.EQ.0) READ (IN,'(100F10.4)') STAGES(LM),
     1                          (CLAKE(LM,ISOL),ISOL=1,NSOL)
                 END IF
               ELSE
                 IF ( IRDTAB.GT.0 ) THEN
                   IF(ISS.NE.0) READ (IN,*) STAGES(LM),SSMN(LM),
     1                          SSMX(LM),(CLAKE(LM,ISOL),ISOL=1,NSOL),
     2                          LAKTAB(LM)
                   IF(ISS.EQ.0) READ (IN,*) STAGES(LM),
     1                          (CLAKE(LM,ISOL),ISOL=1,NSOL),LAKTAB(LM)
                 ELSE
                   IF(ISS.NE.0) READ (IN,*) STAGES(LM),SSMN(LM),
     1                          SSMX(LM),(CLAKE(LM,ISOL),ISOL=1,NSOL)
                   IF(ISS.EQ.0) READ (IN,*) STAGES(LM),
     1                          (CLAKE(LM,ISOL),ISOL=1,NSOL)
                 END IF
               END IF
            IF(ISS.NE.0) WRITE (IOUT,*) 
	1 	    'LM,STAGES(LM),SSMN(LM),SSMX(LM):'
            IF(ISS.NE.0) WRITE (IOUT,*) LM,STAGES(LM),SSMN(LM),SSMX(LM)
            IF(ISS.EQ.0) WRITE (IOUT,*) 'LM,STAGES(LM):'
            IF(ISS.EQ.0) WRITE (IOUT,*) LM,STAGES(LM)
              WRITE (IOUTS,*) 'LM,(CLAKE(LM,ISOL),ISOL=1,NSOL):'
 35           WRITE (IOUTS,*) LM,(CLAKE(LM,ISOL),ISOL=1,NSOL)
cgage
C            CLAKINIT=CLAKE
         END IF
      END IF
C
!      WRITE (IOUT,'(/)')
!      WRITE(IOUT,822)
 19   FORMAT(//1X,'LAKE PACKAGE ACTIVE:  CALCULATED LAKE STAGE FOR EACH 
     1TIME STEP WILL BE STORED IN HNEW ARRAY.')
 20   FORMAT(///1X,'INITIAL LAKE STAGE:  LAKE    STAGE    SS MIN    SS M
     1AX'/)
 21   FORMAT (//1X,'INITIAL LAKE CONCENTRATIONS:  LAKE   CONCENTRATION (
     1NSOL =',I3,')'/)                                                  
 22   FORMAT (22X,I3,3F10.3)
 23   FORMAT ('(31X,I3,3X,1P',I3,'(E12.3))')                            
 820  FORMAT (/1X,'INITIAL LAKE STAGE:  LAKE    STAGE'/)
 822  FORMAT(//1X,'If any subsequent steady-state stress periods, min. a
     1nd max. stages for each lake will be read in Record 9a.'//)
C

! RGN 9/25/12 moved this to read lake bathymetry before stress period information.
      IF ( KKPER==1 .AND. IRDTAB.GT.0 ) THEN
! Read tables for stage,volume, and area relations.
        DO L1=1,NLAKES
!          WRITE(IOUT,1399) L1
          iunit = LAKTAB(L1)
Cdep  revised print statement to include area
! 1399 FORMAT(//1X,'STAGE/VOLUME RELATION FOR LAKE',I3//6X,'STAGE',
!     1        8X,'VOLUME',8X,'AREA'/)
	    WRITE(IOUT,*) 'L1, DEPTHTABLE, VOLUMETABLE, AREATABLE:'
          WRITE(IOUT,*) L1
          DO  INC=1,151
          READ(iunit,*) DEPTHTABLE(INC,L1), VOLUMETABLE(INC,L1), 
     +                    AREATABLE(INC,L1) 
          WRITE(IOUT,*) DEPTHTABLE(INC,L1), VOLUMETABLE(INC,L1), 
     +                    AREATABLE(INC,L1) 

          END DO
        END DO      
      END IF
C1B-----READ ITMP (FLAG TO REUSE LAKE-GEOMETRY DATA).
      IF(IFREFM.EQ.0) THEN
         READ(IN,'(3I10)') ITMP, ITMP1, LWRT
      ELSE
         READ(IN,*) ITMP, ITMP1, LWRT
      END IF
      WRITE(IOUT,*) 'ITMP, ITMP1, LWRT:'
      WRITE(IOUT,*) ITMP, ITMP1, LWRT
C
C2A-----IF ITMP < 0 THEN REUSE LAKE CONFIGURATION DATA FROM LAST STRESS
C       PERIOD.
      IF(ITMP.GE.0) GO TO 50
!      WRITE (IOUT,'(/)')
!      WRITE(IOUT,2)
!    2 FORMAT(1H ,'REUSING LAKE CONFIGURATION DATA FROM LAST STRESS PERIO
!     1D'/)
      GO TO 800
C
C4------IF THERE ARE NO LAKE NODES THEN RETURN.
   50 LKNODE = 0
      IF(ITMP.EQ.0) GOTO 900
C
C   INITIALIZE BGAREA
      DO 60 LK=1,NLAKES
      BGAREA(LK)=0.0
   60 CONTINUE
C
C5------READ INTEGER ARRAYS THAT DEFINE THE POSITIONS OF ALL LAKES IN
C5A     EACH MODEL GRID LAYER.  THEN READ ARRAYS OF LAKEBED CONDUCTANCES
C5B     IN EACH LAYER.
C
C   READ ARRAY OF LAKE ID'S, LAYER BY LAYER
C   REVISED 11/30/2005 DEP
      DO 125 K=1,NLAY
      KK = K
      CALL U2DINT(LKARR1(:,:,KK),ANAME(1),NROW,NCOL,KK,IN,IOUT)
  125 CONTINUE
C
C   CHECK THAT ALL ENTRIES ARE VALID LAKE ID NUMBERS OR ZERO
C
      DO 130 K=1,NLAY
      DO 130 I=1,NCOL
      DO 130 J=1,NROW
      IF(LKARR1(I,J,K).GT.0.AND.LKARR1(I,J,K).LE.NLAKES) GO TO 130
      LKARR1(I,J,K)=0
  130 CONTINUE
C
C   CHECK IF LAKE CELLS HAVE VALUES OF IBOUND=0; WARN IF INCONSISTENT
C
!      WRITE (IOUT,'(/)')
!      DO 132 K=1,NLAY
!      DO 132 I=1,NCOL
!      DO 132 J=1,NROW
!      IF(LKARR1(I,J,K).GT.0.AND.IBOUND(I,J,K).NE.0) THEN
!         WRITE (IOUT,232) IBOUND(I,J,K),LKARR1(I,J,K),I,J,K
!  232    FORMAT (7X,'*** WARNING: IBOUND = ',I2,
!     1  ' & LKARR = ',I2,' at CELL I=',I3,
!     2  ', J=',I3,', K=',I3,' ***')
!      END IF
!  132 CONTINUE
C
C   READ ARRAY OF BED LEAKANCES, LAYER BY LAYER
Cdep    REVISED 11/30/2005
!      WRITE (IOUT,'(/)')
      DO 135 K=1,NLAY
      KK = K
      CALL U2DREL(BDLKN1(:,:,KK),ANAME(2),NROW,NCOL,KK,IN,IOUT)
  135 CONTINUE
C
!        WRITE(IOUT,36)
!        WRITE(IOUT,4)
!36    FORMAT(/7X,'LOCATIONS, LAKE #, INTERFACE TYPE FOR GRID CELLS',
!     1 ' ADJACENT TO LAKES:',5X,/
!     3 5X,71('-'))
!4     FORMAT(5X,'LAYER #',4X,'ROW #',4X,'COLUMN #',3X,'LAKE #',
!     1       2X,'INTERFACE TYPE',2X,'LAKEBED LEAKANCE')
C
C   IDENTIFY LAKE BORDER CELLS, ASSIGN CELL TYPE ID'S, COMPUTE AND
C     ASSIGN LAKE-AQUIFER INTERFACE CONDUCTANCES.
C
      M = 0
      DO 180 I=1,NCOL
      DO 180 J=1,NROW
      K = 1
      IF(LKARR1(I,J,K).EQ.0) GO TO 150
      IF(NLAY.EQ.1) GO TO 145
C   Keep searching in vertical direction until non-lake cell is found, 
C     and define interface there ("K" for interface is layer below 
C     bottom of lake)
      DO 140 K=2,NLAY
      IF(LKARR1(I,J,K).EQ.0) GO TO 145
  140 CONTINUE
C   Make sure that K=NLAY if lake extends to bottom cell of grid:
      K=NLAY
C      GO TO 145
C
C   VERTICAL LAKEBED INTERFACE (TYPE 0) DETECTED
C
  145 M = M + 1
      IF(M.LE.MXLKND) GO TO 147
      WRITE(IOUT,149) I,J,K
  149 FORMAT(/1X,'MAXIMUM NUMBER OF GRID CELLS ADJACENT TO LAKES HAS BEE
     1N EXCEEDED WITH CELL ',3I5,'  REDEFINE VARIABLE MXLKND TO A LARGER
     2 VALUE IN MODULE GWF2LAK7AR')
      CALL USTOP(' ')
  147 ILAKE(1,M) = K
      ILAKE(2,M) = J
      ILAKE(3,M) = I
Cdep  changed if statement August 24, 2009
Cdep      IF(K.GT.1.AND.LKARR1(I,J,K).EQ.0) LID = LKARR1(I,J,K-1)
Cdep      IF(LKARR1(I,J,K).NE.0) LID = LKARR1(I,J,K)
      IF(K.GT.1) THEN
        IF(LKARR1(I,J,K).EQ.0) THEN
          LID = LKARR1(I,J,K-1)
        ELSE
          LID = LKARR1(I,J,K)
        END IF
      ELSE IF (K.EQ.1) THEN
        IF(LKARR1(I,J,K).EQ.0) THEN
          LID = 0
        ELSE
          LID = LKARR1(I,J,K)
        END IF
      END IF
      ILAKE(4,M) = LID
      ILAKE(5,M) = 6
      IF ( K.GT.1 ) THEN             !RGN 5/21/12 added IF test
        BEDLAK(M) = BDLKN1(I,J,K-1)
      ELSE                           !RGN
        BEDLAK(M) = BDLKN1(I,J,K)    !RGN
      END IF                         !RGN
      IF(K.EQ.NLAY.AND.LKARR1(I,J,K).NE.0) BEDLAK(M) = 0.0
      BGAREA(LID) = BGAREA(LID) + DELC(J)*DELR(I)
!      WRITE(IOUT,5) (ILAKE(I1,M),I1=1,5), BEDLAK(M)
5     FORMAT(5I10,10X,F10.5)
      IF(LKARR1(I,J,K).NE.0) GO TO 180
C
C   SEARCH FOR CELL(S) ADJACENT TO LAKE
C
  150 K2 = K
      DO 175 K1=K2,NLAY
cgzh fix for 2D-problems
      IF(NCOL.EQ.1) GO TO 165
      IF(I.NE.1) GO TO 1151
      IF(LKARR1(I+1,J,K1).EQ.0) GO TO 165
      GO TO 1153
 1151 IF(I.NE.NCOL) GO TO 1152
      IF(LKARR1(I-1,J,K1).EQ.0) GO TO 165
      GO TO 1153
 1152 IF(LKARR1(I+1,J,K1).EQ.0.AND.LKARR1(I-1,J,K1).EQ.0) GO TO 165
C
C   CELL(S) LATERALLY ADJACENT TO LAKE IN X-DIRECTION (TYPE 1) DETECTED
C
 1153 DO 160 N=1,2
      IF(N.EQ.2) GO TO 155
      IF(I.EQ.1) GO TO 160
      IF(LKARR1(I-1,J,K1).EQ.0) GO TO 160
      I2 = I-1
      IFACE=1
      GO TO 157
  155 IF(I.EQ.NCOL) GO TO 160
      IF(LKARR1(I+1,J,K1).EQ.0) GO TO 160
      I2 = I + 1
      IFACE=2
  157 M = M + 1
      IF(M.LE.MXLKND) GO TO 158
      WRITE(IOUT,149) I,J,K1
      CALL USTOP(' ')
  158 ILAKE(1,M) = K1
      ILAKE(2,M) = J
      ILAKE(3,M) = I
      ILAKE(4,M) = LKARR1(I2,J,K1)
      ILAKE(5,M) = IFACE
      BEDLAK(M) = BDLKN1(I,J,K1)
      K4 = K1 - 1
      DO 3158 K3=1,K4
      IF(LKARR1(I,J,K3).EQ.0) GO TO 3158
      GO TO 3162
 3158 CONTINUE
      BEDLAK(M) = BDLKN1(I,J,1)
 3162 CONTINUE
!      WRITE(IOUT,5) (ILAKE(I1,M),I1=1,5), BEDLAK(M)
  160 CONTINUE
cgzh fix for 2D-problems
  165 IF(NROW.EQ.1) GO TO 175
      IF(J.NE.1) GO TO 1161
      IF(LKARR1(I,J+1,K1).EQ.0) GO TO 175
      GO TO 1163
 1161 IF(J.NE.NROW) GO TO 1162
      IF(LKARR1(I,J-1,K1).EQ.0) GO TO 175
      GO TO 1163
 1162 IF(LKARR1(I,J+1,K1).EQ.0.AND.LKARR1(I,J-1,K1).EQ.0) GO TO 175
C
C   CELL(S) LATERALLY ADJACENT TO LAKE IN Y-DIRECTION (TYPE 2) DETECTED
C
 1163 DO 170 N=1,2
      IF(N.EQ.2) GO TO 172
      IF(J.EQ.1) GO TO 170
      IF(LKARR1(I,J-1,K1).EQ.0) GO TO 170
      J2 = J - 1
      IFACE=4
      GO TO 174
  172 IF(J.EQ.NROW) GO TO 170
      IF(LKARR1(I,J+1,K1).EQ.0) GO TO 170
      J2 = J + 1
      IFACE=3
  174 M = M + 1
      IF(M.LE.MXLKND) GO TO 176
      WRITE(IOUT,149) I,J,K1
      CALL USTOP(' ')
  176 ILAKE(1,M) = K1
      ILAKE(2,M) = J
      ILAKE(3,M) = I
      ILAKE(4,M) = LKARR1(I,J2,K1)
      ILAKE(5,M) = IFACE
      BEDLAK(M) = BDLKN1(I,J,K1)
      K4 = K1 - 1
      DO 4158 K3=1,K4
      IF(LKARR1(I,J,K3).EQ.0) GO TO 4158
      GO TO 4162
 4158 CONTINUE
      BEDLAK(M) = BDLKN1(I,J,1)
 4162 CONTINUE
!      WRITE(IOUT,5) (ILAKE(I1,M),I1=1,5), BEDLAK(M)
  170 CONTINUE
  175 CONTINUE
  180 CONTINUE
!      WRITE(IOUT,195) M
  195 FORMAT(/5X,'NUMBER OF LAKE-AQUIFER CELL INTERFACES = ',I5)
      LKNODE = M
C
C   SET LAKE BOTTOM ELEVATIONS
      DO 295 LK=1,NLAKES
  295 BOTTMS(LK) = 999999
C
      DO 350 II=1,LKNODE
      K = ILAKE(1,II)
      J = ILAKE(2,II)
      I = ILAKE(3,II)
C  Convert ILAKE(5,II):  1 and 2 are type 1,  3 and 4 are type 2, 
C    6 is type 0
      NTYP = (ILAKE(5,II)+1)/2
      IF(NTYP.EQ.3) NTYP=0
      IF(NTYP.EQ.0) THEN
        LAKE = ILAKE(4,II)
Cdep  changed if statement August 24, 2009
Cdep        IF(K.GT.1) BOTLK = BOTM(I,J,LBOTM(K-1))
Cdep        IF(K.EQ.NLAY.AND.LKARR1(I,J,K).GT.0) BOTLK = BOTM(I,J,LBOTM(K))
         IF(K.EQ.1.OR.K.EQ.NLAY.AND.LKARR1(I,J,K).GT.0) THEN
            BOTLK = BOTM(I,J,LBOTM(K))
         ELSE IF (K.EQ.0) THEN
            BOTLK = BOTM(I,J,LBOTM(1))
         ELSE
            BOTLK = BOTM(I,J,LBOTM(K-1))
         END IF     
        IF(BOTLK.LT.BOTTMS(LAKE)) BOTTMS(LAKE) = BOTLK
      END IF
  350 CONTINUE
C
C-- COMPUTE AND PRINT STAGE/VOLUME TABLES WHEN MORE THAN ONE LAYER
Cdep  revised print statement to include stage/area tables
C
      IF ( IRDTAB.EQ.0 ) THEN
!      IF(NLAY.EQ.1) GO TO 1331       !RGN 5/21/12
      DO 1330 L1=1,NLAKES
!      WRITE(IOUT,1306) L1
Cdep  revised print statement to include area
! 1306 FORMAT(//1X,'STAGE/VOLUME RELATION FOR LAKE',I3//6X,'STAGE',
!     1        8X,'VOLUME',8X,'AREA'/)
      DO  INC=1,151
        AREATABLE(INC,L1) = 0.D0
      END DO
      EVOL = 0.0
      GTSDPH = 40.0
      TOPMST = BOTTMS(L1)+GTSDPH
      TBELV = BOTTMS(L1)
      DO 1340 I=1,NCOL
      DO 1340 J=1,NROW
      IF(LKARR1(I,J,1).NE.L1) GO TO 1340
Cdep Revised estimate of DTHK to be thickness of top most 
C     layer 6/09/2009
      IF(BOTM(I,J,0).GT.TOPMST) TOPMST = BOTM(I,J,0) 
!      DTHK = BOTM(I,J,0) - BOTM(I,J,1)   RGN this was causing problems 7/8/11
!      IF (DTHK.LE.GTSDPH) THEN
!        TOPMST = BOTM(I,J,1)+DTHK
!      ELSE 
!        TOPMST = BOTM(I,J,1)+GTSDPH
!      END IF
 1340 CONTINUE
      TBNC = (TOPMST-BOTTMS(L1))/150.0
Cdep Revised looping for computing lake stage, volume, 
Cdep   and area Apr 2009.
Cdep   WRITE(IOUT,1315) TBELV, EVOL
      DO  INC=1,151
        IF (INC.GT.1) THEN
          VOLUMETABLE(INC,L1)=VOLUMETABLE(INC-1,L1)
        END IF
        DO I=1,NCOL
          DO J=1,NROW
            LAKEFLG = 0
            K = 1
            MOSTBOT: DO WHILE (LAKEFLG.EQ.0)
              IF(LKARR1(I,J,K).EQ.L1) THEN
                LAKEFLG = K
              END IF
              IF(K.EQ.NLAY)EXIT MOSTBOT
              K = K + 1
            END DO MOSTBOT
            IF(LAKEFLG.GT.0) THEN
              K=LAKEFLG
              FINDBOT: DO WHILE(LKARR1(I,J,K).GT.0)
                K=K+1
                IF(K.EQ.NLAY+1) EXIT
              END DO FINDBOT
              BOTIJ = BOTM(I,J,LBOTM(K-1))
              IF(INC.EQ.1) THEN     
                IF(TBELV+1.0E-03.GT.BOTIJ) THEN
                  AREATABLE(INC,L1)=AREATABLE(INC,L1)+DELC(J)*DELR(I)
                  DEPTHTABLE(INC,L1)=TBELV
                END IF
              ELSE
                IF (TBELV-BOTIJ.GT.0.0) THEN
                  AREATABLE(INC,L1)=AREATABLE(INC,L1)+DELC(J)*DELR(I)
                  DEPTHTABLE(INC,L1)=TBELV
                  IF(ABS(TBELV-BOTIJ).GT.1.0E-04) THEN
                    VOLUMETABLE(INC,L1)=VOLUMETABLE(INC,L1)+
     +                                (DELC(J)*DELR(I))*TBNC
                  END IF
                END IF               
              END IF
            END IF  
          END DO
        END DO       
Cdep PRINT TABLE OF ELEVATION, VOLUME, AND AREA
        WRITE(IOUT,1315) DEPTHTABLE(INC,L1), VOLUMETABLE(INC,L1), 
     +                    AREATABLE(INC,L1) 
        TBELV = TBELV + TBNC 
      END DO
 1315 FORMAT(3(1X,1PE13.5))
!      WRITE(IOUT,1326)
 1326 FORMAT(120X)
Cdep  set minimum and maximum lake stages for transient simulations
      IF(ISS.EQ.0) THEN
        SSMN(L1)=BOTTMS(L1)
        SSMX(L1)=TBELV
      END IF
 1330 CONTINUE
 1331 CONTINUE
      END IF
      IF(IUNITSFR.LE.0) THEN
         NDV=0
         NTRB=0
      END IF
C
C
C--  READ LINKAGE PARAMETERS FOR COALESCING LAKES
C
C    FOR EACH CONNECTED LAKE SYSTEM, READ LAKE NUMBERS OF CENTER LAKES
C    AND ADJOINING LAKES AND SILL ELEVATIONS.  ENTER CARD IMAGES
C    FOR SUBLAKE SYSTEMS EVEN IF LINKED TO MAIN LAKE SYSTEM.  SYSTEMS
C    MUST BE ORDERED HIERARCHICALLY.
C
      ICMX = 0
      NCLS=0
      IF(IFREFM.EQ.0) THEN
        READ(IN,'(I5)') NSLMS
      ELSE
        READ(IN,*) NSLMS
      END IF
      WRITE(IOUT,*) 'NSLMS:'
      WRITE(IOUT,*) NSLMS
!      WRITE(IOUT,680) NSLMS
!  680 FORMAT(/1X,'NUMBER OF CONNECTED LAKE SYSTEMS IN SIMULATION IS ',I3
!     1)
      IF(NSLMS.LE.0) GO TO 760
      DO 700 IS=1,NSLMS
      IF(IFREFM.EQ.0) THEN
        READ(IN,'(16I5)',END=750) IC,(ISUB(IS,I),I=1,IC)
      ELSE
        READ(IN,*,END=750) IC,(ISUB(IS,I),I=1,IC)
      END IF
      WRITE(IOUT,*) 'IC,(ISUB(IS,I),I=1,IC):'
      WRITE(IOUT,*) IC,(ISUB(IS,I),I=1,IC)
      IF(IC.LE.0) GO TO 750
      IF(IC.GT.ICMX) ICMX=IC
      ICS(IS)=IC
      IC1 = IC - 1
      IF(IFREFM.EQ.0) THEN
        READ(IN,'(100F10.2)') (SILLVT(IS,I),I=1,IC1)
      ELSE
        READ(IN,*) (SILLVT(IS,I),I=1,IC1)
      END IF
      WRITE(IOUT,*) '(SILLVT(IS,I),I=1,IC1):'
      WRITE(IOUT,*) (SILLVT(IS,I),I=1,IC1)
!      WRITE(IOUT,18) IS, ICS(IS), ISUB(IS,1)
!   18 FORMAT(/10X,'SYSTEM',I3//2X,'NUMBER OF LAKES IN SYSTEM',I5,
!     1  '  CENTER LAKE NUMBER',I5//1X,'SUBLAKE NUMBER',3X,
!     2  'SILL ELEVATION'/)
      DO 715 JK=2,IC
  715 CONTINUE
!  715 WRITE(IOUT,717) ISUB(IS,JK), SILLVT(IS,JK-1)
!  717 FORMAT(8X,I2,8X,F10.2)
  700 CONTINUE
  750 CONTINUE
      NCLS=IS-1
!      WRITE(IOUT,751) NCLS
!  751 FORMAT(/1X,'READ DATA FOR',I5,' LAKE SYSTEMS'/)
  760 CONTINUE
C
C----- READ LAKE PRECIPITATION, EVAPORATION, RUNOFF, AND WITHDRAWAL RATES.
C      IF ITMP1 LT 0, SPECIFICATIONS FROM LAST STRESS PERIOD ARE USED.
C
  800 IF(ITMP1.GE.0) GO TO 801
!      WRITE(IOUT,802)
!  802 FORMAT(1H0,'REUSING RECH,ET,WITHDRAWAL RATES FROM LAST STRESS PERI
!     1OD'/)
      GOTO 900
  801 CONTINUE	  
!  801 IF(ISS.NE.0.AND.KKPER.GT.1) WRITE(IOUT,7)
!7     FORMAT(/1X,'LAKE',7X,'PRECIP',5X,'EVAP',5X,'RUNOFF',
!     2     3X,'WITHDRAW',3X,'BOTTOM',5X,'AREA',5X,'SS MIN',3X,'SS MAX'
!     1/90('-'))
!      IF(ISS.EQ.0.OR.KKPER.EQ.1) WRITE(IOUT,77)
!   77 FORMAT(/1X,'LAKE',7X,'PRECIP',5X,'EVAP',5X,'RUNOFF',
!     2     3X,'WITHDRAW',3X,'BOTTOM',5X,'AREA',5X,/70('-'))
!      IF (IUNITGWT.GT.0) WRITE (IOUTS,8)
! 8    FORMAT (//1X,'LAKE',4X,'SOLUTE',6X,'CPPT',6X,'CRNF',6X,'CAUG'/)
      DO 300 LM=1,NLAKES
      IF(IFREFM.EQ.0) THEN
        IF(ISS.NE.0.AND.KKPER.GT.1) READ(IN,'(6F10.4)') PRCPLK(LM),
     1   EVAPLK(LM),RNF(LM),WTHDRW(LM),SSMN(LM),SSMX(LM)
        IF(ISS.NE.0.AND.KKPER.GT.1) WRITE(IOUT,*) 'PRCPLK(LM),
     1EVAPLK(LM),RNF(LM),WTHDRW(LM),SSMN(LM),SSMX(LM):'
        IF(ISS.NE.0.AND.KKPER.GT.1) WRITE(IOUT,*) PRCPLK(LM),
     1    EVAPLK(LM),RNF(LM),WTHDRW(LM),SSMN(LM),SSMX(LM)


        IF(ISS.EQ.0.OR.KKPER.EQ.1) READ(IN,'(6F10.4)') PRCPLK(LM),
     1   EVAPLK(LM),RNF(LM),WTHDRW(LM)
        
	  IF(ISS.EQ.0.OR.KKPER.EQ.1) WRITE(IOUT,*) 'PRCPLK(LM),
     1EVAPLK(LM),RNF(LM),WTHDRW(LM):'
	  IF(ISS.EQ.0.OR.KKPER.EQ.1) WRITE(IOUT,*) PRCPLK(LM),
     1   EVAPLK(LM),RNF(LM),WTHDRW(LM)
      ELSE
        IF(ISS.NE.0.AND.KKPER.GT.1) READ(IN,*) PRCPLK(LM),EVAPLK(LM),
     1   RNF(LM),WTHDRW(LM),SSMN(LM),SSMX(LM)
        IF(ISS.NE.0.AND.KKPER.GT.1) 
	1    WRITE(IOUT,*) 'PRCPLK(LM),EVAPLK(LM),
     2RNF(LM),WTHDRW(LM),SSMN(LM),SSMX(LM):'
        IF(ISS.NE.0.AND.KKPER.GT.1) WRITE(IOUT,*) PRCPLK(LM),EVAPLK(LM),
     1   RNF(LM),WTHDRW(LM),SSMN(LM),SSMX(LM)
        IF(ISS.EQ.0.OR.KKPER.EQ.1) READ(IN,*) PRCPLK(LM),EVAPLK(LM),
     1   RNF(LM),WTHDRW(LM)
        IF(ISS.EQ.0.OR.KKPER.EQ.1) WRITE(IOUT,*) 'PRCPLK(LM),EVAPLK(LM),
     1RNF(LM),WTHDRW(LM):'
        IF(ISS.EQ.0.OR.KKPER.EQ.1) WRITE(IOUT,*) PRCPLK(LM),EVAPLK(LM),
     1   RNF(LM),WTHDRW(LM)
      END IF
!      IF(ISS.NE.0.AND.KKPER.GT.1) WRITE(IOUT,9) LM,PRCPLK(LM),EVAPLK(LM)
!     1 ,RNF(LM),WTHDRW(LM),BOTTMS(LM),BGAREA(LM),SSMN(LM),SSMX(LM)
!9     FORMAT(1X,I3,4X,1P,3E10.3,1X,5E10.3)
!      IF(ISS.EQ.0.OR.KKPER.EQ.1) WRITE(IOUT,9) LM,PRCPLK(LM),EVAPLK(LM),
!     1 RNF(LM),WTHDRW(LM),BOTTMS(LM),BGAREA(LM)
      IF(IUNITGWT.LE.0) GO TO 300
      DO 850 ISOL=1,NSOL
        IF(IFREFM.EQ.0) THEN
          IF(WTHDRW(LM).LT.0.0) THEN
            READ(IN,'(3F10.4)')CPPT(LM,ISOL),CRNF(LM,ISOL),CAUG(LM,ISOL)
          ELSE
            READ(IN,'(2F10.4)')CPPT(LM,ISOL),CRNF(LM,ISOL)
          END IF
        ELSE
          IF(WTHDRW(LM).LT.0.0) THEN
            READ(IN,*) CPPT(LM,ISOL),CRNF(LM,ISOL),CAUG(LM,ISOL)
          ELSE
            READ(IN,*) CPPT(LM,ISOL),CRNF(LM,ISOL)
          END IF
        END IF
        IF(WTHDRW(LM).LT.0.0)WRITE(IOUTS,840) LM,ISOL,
     +       CPPT(LM,ISOL),CRNF(LM,ISOL),CAUG(LM,ISOL)       
        IF(WTHDRW(LM).GE.0.0)
     1  WRITE(IOUTS,841) LM,ISOL,CPPT(LM,ISOL),CRNF(LM,ISOL)
  840   FORMAT(1X,I3,6X,I3,4X,1P,3E10.2)
  841 FORMAT(1X,I3,6X,I3,4X,1P,2E10.2)
  850 CONTINUE
C      WRITE (IOUTS,'(/)')
  300 CONTINUE
!      WRITE (IOUT,'(/)')
C
C------Define Initial Lake Volume & Initialize Cumulative Budget Terms
      IF(KKPER.EQ.1) THEN
!dep revised calculation of initial lake volume July 2009
        STGINIT=0.0D0
        DO 8400 LK=1,NLAKES
!dep 8400    VOL(LK)=0.0
             STGINIT=STAGES(LK)
             VOL(LK)=VOLTERP(STGINIT,LK)
             VOLINIT(LK)=VOL(LK)
 8400   CONTINUE
        DO 8450 LK=1,NLAKES
             CUMPPT(LK)=0.0
             CUMEVP(LK)=0.0
             CUMRNF(LK)=0.0
             CUMGWI(LK)=0.0
             CUMGWO(LK)=0.0
             CUMSWI(LK)=0.0
             CUMSWO(LK)=0.0
             CUMWDR(LK)=0.0
             CUMFLX(LK)=0.0
 8450   CONTINUE
            DO 8900 L=1,LKNODE
               IL=ILAKE(1,L)
               IR=ILAKE(2,L)
               IC=ILAKE(3,L)
               LAKE=ILAKE(4,L)
C------Convert ILAKE(5,L):  1 and 2 are type 1,  3 and 4 are type 2, 
C        6 is type 0
               ITYPE = (ILAKE(5,L)+1)/2
               IF(ITYPE.EQ.3) ITYPE=0
               IF(ITYPE.NE.0) GO TO 8900
               IF(IL.GT.1) BOTLK = BOTM(IC,IR,LBOTM(IL-1))
               IF(IL.EQ.NLAY.AND.LKARR1(IC,IR,IL).GT.0)
     1            BOTLK = BOTM(IC,IR,LBOTM(IL))
 8900       CONTINUE
      ENDIF

! rbw begin new code
 900  Continue
! rbw end new code
! 900  IF (IUNITBCF.GT.0) THEN  ! rsr, moved if block from main
!        CALL SGWF2LAK7BCF7RPS()
!      ELSE IF (IUNITLPF.GT.0) THEN
!        CALL SGWF2LAK7LPF7RPS()
!      ELSE IF (IUNITHUF.GT.0) THEN
!        STOP 'ERROR, HUF Package not implemented in GSFLOW'   !gsf
 !       CALL SGWF2LAK7HUF7RPS()
      !ELSE
!gsf    WRITE (IOUT, *) 'LAK Package requires BCF, LPF, or HUF'
!        WRITE (IOUT, *) 'LAK Package requires BCF or LPF'     !gsf
!        CALL USTOP(' ')
!      END IF
!      IF (IUNITSFR.GT.0) CALL SGWF2LAK7SFR7RPS()
      
C
C7------RETURN
      RETURN
      END
C
!      SUBROUTINE GWF2LAK7AD(KKPER,KKSTP,IUNITGWT,IGRID)
C
C------VERSION 7.1 JUNE 2006 GWF2LAK7AD; REVISIONS AUGUST 2009 DEP&RGN
C
C     ******************************************************************
C     ADVANCE TO NEXT TIME STEP FOR TRANSIENT LAKE SIMULATION, AND COPY
C             INITIAL LAKE STAGES TO STGOLD FOR STEADY STATE.
C     ******************************************************************
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
!      SUBROUTINE GWF2LAK7ST(NFLG,IGRID)
C   ********************************************************************
C   SET IBOUND VALUES SO THAT RECHARGE AND EVAPOTRANSPIRATION (ET) WILL
C   BE ASSIGNED CORRECTLY UNDERNEATH DRYING LAKES (NFLG = 0), OR RESET
C   IBOUND AFTER RECHARGE AND ET ARE COMPUTED (NFLG = 1).
C   ********************************************************************
!      SUBROUTINE GWF2LAK7FM(KITER,KKPER,IUNITSFR,IUNITUZF,IUZFBND,
!     +                      FINF,VKS,IGRID)
C
C----- USGS VERSION 7; JUNE 2006 GWF2LAK7FM
Cdep  MODIFIED SUBROUTINE TO ITERATIVELY SOLVE FOR LAKE STAGE EVEN
C       DURING TRANSIENT STRESS PERIODS.
C     ******************************************************************
C     ADD LAKE TERMS TO RHS AND HCOF IF SEEPAGE OCCURS IN MODEL CELLS
C     ******************************************************************
C
!      SUBROUTINE GWF2LAK7BD(KSTP,KPER,IUNITGWT,IUNITGAGE,IUNITSFR,
!     1                      IUNITUZF,NSOL,IUZFBND,FINF,VKS,IGRID)
C
C----- USGS VERSION 7; JUNE 2006 GWF2LAK7BD
C
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR LAKES
C     ******************************************************************
!      SUBROUTINE SGWF2LAK7SFR7RPS()
C
C    *******************************************************************
C--  IF STREAMS EXIST, DEFINE CONNECTIONS BETWEEN LAKES AND STREAMS
C    *******************************************************************
!      SUBROUTINE SGWF2LAK7BCF7RPS()
C
C     ******************************************************************
C     COMPUTE VERTICAL CONDUCTANCES AND HORIZONTAL CONDUCTANCES PER UNIT
C     THICKNESS FOR LAKES WHEN BCF PACKAGE IS USED
C     ******************************************************************
!      SUBROUTINE SGWF2LAK7LPF7RPS()
C
C     ******************************************************************
C     COMPUTE VERTICAL CONDUCTANCES AND HORIZONTAL CONDUCTANCES PER UNIT
C     THICKNESS FOR LAKES WHEN LPF PACKAGE IS USED
C     ******************************************************************
!      SUBROUTINE SGWF2LAK7HUF7RPS()
C
C     ******************************************************************
C     COMPUTE VERTICAL CONDUCTANCES AND HORIZONTAL CONDUCTANCES PER UNIT
C     THICKNESS FOR LAKES WHEN HUF PACKAGE IS USED
C     ******************************************************************
Cdep  Added function statements to compute derivatives for Newton method
Cdep     used in solving lake stage in the FORMULATE SUBROUTINE (LAK7FM).      
!      DOUBLE PRECISION FUNCTION FINTERP (STAGE,LN)
Cdep&rgn  FUNCTION LINEARLY INTERPOLATES BETWEEN TWO VALUES
C          OF LAKE STAGE TO CACULATE LAKE AREA.
C         ADDED 5/15/2006
C------FUNCTION DERIVTERP FOR INTERPOLATING DERIVATIVE OF LAKE OUTFLOW. 
!      DOUBLE PRECISION FUNCTION DERIVTERP (STAGE,LSEG)
Cdep&rgn  FUNCTION LINEARLY INTERPOLATES BETWEEN TWO VALUES
C          OF LAKE STAGE TO CACULATE LAKE OUTFLOW DERIVATIVE.
C         ADDED 5/16/2006
C------FUNCTION OUTFLWTERP FOR INTERPOLATING DERIVATIVE OF LAKE OUTFLOW. 
!      DOUBLE PRECISION FUNCTION OUTFLWTERP (STAGE,LSEG)
Cdep&rgn  FUNCTION LINEARLY INTERPOLATES BETWEEN TWO VALUES
C          OF LAKE OUTFLOW STORED IN SLKOTFLW ARRAY.
C         ADDED 5/16/2006

!     Interpolate lake volume as a function of lake stage
C     used in solving lake stage in the FORMULATE SUBROUTINE (LAK7FM).      
      DOUBLE PRECISION FUNCTION VOLTERP (STAGE,LN)
C     FUNCTION LINEARLY INTERPOLATES BETWEEN TWO VALUES
C          OF LAKE STAGE TO CACULATE LAKE VOLUME.
      USE GWFLAKMODULE, ONLY: VOLUMETABLE, DEPTHTABLE, AREATABLE
      IMPLICIT NONE
      INTEGER LN, IFLG, I
      DOUBLE PRECISION STAGE, VOLUME, TOLF2, FOLD
      TOLF2=1.0E-7
      IF (STAGE.GT.DEPTHTABLE(151,LN))THEN
 ! bug 5/4/09 changed FINTERP TO VOLUME
        VOLTERP =  VOLUMETABLE(151,LN)+(STAGE-DEPTHTABLE(151,LN))*
     +             AREATABLE(151,LN) 
        RETURN
      END IF
      IFLG = 0
      I = 1
      DO WHILE ( IFLG.EQ.0 )
        FOLD=ABS(STAGE-DEPTHTABLE(I,LN))     
        IF (FOLD .LE. TOLF2) THEN  
          VOLUME=VOLUMETABLE(I,LN)
          IFLG = 1
        ELSEIF (STAGE.GT.DEPTHTABLE(I,LN) .AND. STAGE.LT.
     1          DEPTHTABLE(I+1,LN))THEN
          VOLUME=((VOLUMETABLE(I+1,LN)-VOLUMETABLE(I,LN))/
     1         (DEPTHTABLE(I+1,LN)- DEPTHTABLE(I,LN)))*
     2         STAGE+VOLUMETABLE(I+1,LN)-((VOLUMETABLE(I+1,LN)-
     3         VOLUMETABLE(I,LN))/(DEPTHTABLE(I+1,LN)-
     4         DEPTHTABLE(I,LN)))*DEPTHTABLE(I+1,LN)                 
          IFLG = 1
        END IF
        I = I + 1
        IF( I.GT.150 ) THEN
          IFLG = 1 
          VOLUME = VOLUMETABLE(151,LN)
        END IF
      END DO
      VOLTERP = VOLUME
      IF ( VOLTERP.LT.TOLF2 ) VOLTERP = TOLF2
      RETURN
      END FUNCTION VOLTERP
!     Interpolate lake STAGE as a function of lake VOLUME


      SUBROUTINE GWF2LAK7DA(IUNITLAK, IGRID)
Cdep  End of FUNCTIONS used for Newton method in 
Cdep     FORMULATE SUBROUTINE (LAK7FM).
C  Deallocate LAK data  
      USE GWFLAKMODULE
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER, INTENT(IN) :: IUNITLAK, IGRID
C
      DEALLOCATE (GWFLAKDAT(IGRID)%NLAKES)
      DEALLOCATE (GWFLAKDAT(IGRID)%NLAKESAR)
      DEALLOCATE (GWFLAKDAT(IGRID)%THETA)
      DEALLOCATE (GWFLAKDAT(IGRID)%STGNEW)
      DEALLOCATE (GWFLAKDAT(IGRID)%STGOLD)
      DEALLOCATE (GWFLAKDAT(IGRID)%STGOLD2)
      DEALLOCATE (GWFLAKDAT(IGRID)%STGITER)
      DEALLOCATE (GWFLAKDAT(IGRID)%VOL)
      DEALLOCATE (GWFLAKDAT(IGRID)%LAKUNIT)
      IF ( IUNITLAK.LT.1 ) RETURN

      DEALLOCATE (GWFLAKDAT(IGRID)%ILKCB)
      DEALLOCATE (GWFLAKDAT(IGRID)%LAKTAB)
      DEALLOCATE (GWFLAKDAT(IGRID)%IRDTAB)
      DEALLOCATE (GWFLAKDAT(IGRID)%NSSITR)
Cdep  deallocate SURFDEPTH 3/3/2009
      DEALLOCATE (GWFLAKDAT(IGRID)%SURFDEPTH)
      DEALLOCATE (GWFLAKDAT(IGRID)%MXLKND)
      DEALLOCATE (GWFLAKDAT(IGRID)%LKNODE)
      DEALLOCATE (GWFLAKDAT(IGRID)%ICMX)
      DEALLOCATE (GWFLAKDAT(IGRID)%NCLS)
      DEALLOCATE (GWFLAKDAT(IGRID)%LWRT)
      DEALLOCATE (GWFLAKDAT(IGRID)%NDV)
      DEALLOCATE (GWFLAKDAT(IGRID)%NTRB)
      DEALLOCATE (GWFLAKDAT(IGRID)%SSCNCR)
      DEALLOCATE (GWFLAKDAT(IGRID)%ICS)
      DEALLOCATE (GWFLAKDAT(IGRID)%NCNCVR)
      DEALLOCATE (GWFLAKDAT(IGRID)%LIMERR)
      DEALLOCATE (GWFLAKDAT(IGRID)%ILAKE)
      DEALLOCATE (GWFLAKDAT(IGRID)%ITRB)
      DEALLOCATE (GWFLAKDAT(IGRID)%IDIV)
      DEALLOCATE (GWFLAKDAT(IGRID)%ISUB)
      DEALLOCATE (GWFLAKDAT(IGRID)%IRK)
      DEALLOCATE (GWFLAKDAT(IGRID)%LKARR1)
      DEALLOCATE (GWFLAKDAT(IGRID)%STAGES)
      DEALLOCATE (GWFLAKDAT(IGRID)%FLOB)
      DEALLOCATE (GWFLAKDAT(IGRID)%DSRFOT)
      DEALLOCATE (GWFLAKDAT(IGRID)%PRCPLK)
      DEALLOCATE (GWFLAKDAT(IGRID)%EVAPLK)
      DEALLOCATE (GWFLAKDAT(IGRID)%BEDLAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%WTHDRW)
      DEALLOCATE (GWFLAKDAT(IGRID)%RNF)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMRNF)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMPPT)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMEVP)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMGWI)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMGWO)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMSWI)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMSWO)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMWDR)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMFLX)
      DEALLOCATE (GWFLAKDAT(IGRID)%CNDFCT)
      DEALLOCATE (GWFLAKDAT(IGRID)%VOLINIT)
      DEALLOCATE (GWFLAKDAT(IGRID)%BOTTMS)
      DEALLOCATE (GWFLAKDAT(IGRID)%BGAREA)
      DEALLOCATE (GWFLAKDAT(IGRID)%SSMN)
      DEALLOCATE (GWFLAKDAT(IGRID)%SSMX)
      DEALLOCATE (GWFLAKDAT(IGRID)%EVAP)
      DEALLOCATE (GWFLAKDAT(IGRID)%PRECIP)
      DEALLOCATE (GWFLAKDAT(IGRID)%EVAP3)
      DEALLOCATE (GWFLAKDAT(IGRID)%PRECIP3)
      DEALLOCATE (GWFLAKDAT(IGRID)%SEEP)
      DEALLOCATE (GWFLAKDAT(IGRID)%SEEP3)
      DEALLOCATE (GWFLAKDAT(IGRID)%SURFA)
      DEALLOCATE (GWFLAKDAT(IGRID)%SURFIN)
      DEALLOCATE (GWFLAKDAT(IGRID)%SURFOT)
      DEALLOCATE (GWFLAKDAT(IGRID)%SUMCNN)
      DEALLOCATE (GWFLAKDAT(IGRID)%SUMCHN)
      DEALLOCATE (GWFLAKDAT(IGRID)%CLAKE)
      DEALLOCATE (GWFLAKDAT(IGRID)%CRNF)
      DEALLOCATE (GWFLAKDAT(IGRID)%SILLVT)
      DEALLOCATE (GWFLAKDAT(IGRID)%CAUG)
      DEALLOCATE (GWFLAKDAT(IGRID)%CPPT)
      DEALLOCATE (GWFLAKDAT(IGRID)%CLAKINIT)
      DEALLOCATE (GWFLAKDAT(IGRID)%BDLKN1)
Cdep  Added arrays that track lake budgets for dry lakes
      DEALLOCATE (GWFLAKDAT(Igrid)%EVAPO)
      DEALLOCATE (GWFLAKDAT(Igrid)%WITHDRW)
      DEALLOCATE (GWFLAKDAT(Igrid)%FLWIN)
      DEALLOCATE (GWFLAKDAT(Igrid)%FLWITER)
      DEALLOCATE (GWFLAKDAT(Igrid)%FLWITER3)
      DEALLOCATE (GWFLAKDAT(Igrid)%GWRATELIM)
Cdep  Deallocate arrays used in conjunction with UZF Package
      DEALLOCATE (GWFLAKDAT(Igrid)%OVRLNDRNF)
      DEALLOCATE (GWFLAKDAT(Igrid)%CUMLNDRNF)
      DEALLOCATE (GWFLAKDAT(Igrid)%CUMUZF)
Cdep  Deallocate arrays for storing depth, and area arrays
      DEALLOCATE (GWFLAKDAT(Igrid)%DEPTHTABLE)
      DEALLOCATE (GWFLAKDAT(Igrid)%AREATABLE)
      DEALLOCATE (GWFLAKDAT(Igrid)%VOLUMETABLE)
      DEALLOCATE (GWFLAKDAT(Igrid)%XLAKES)
      DEALLOCATE (GWFLAKDAT(Igrid)%XLAKINIT)
      DEALLOCATE (GWFLAKDAT(Igrid)%XLKOLD)
Crsr allocate BD arrays
      DEALLOCATE (GWFLAKDAT(IGRID)%LDRY)
      DEALLOCATE (GWFLAKDAT(IGRID)%NCNT)
      DEALLOCATE (GWFLAKDAT(IGRID)%NCNST)
      DEALLOCATE (GWFLAKDAT(IGRID)%KSUB)
      DEALLOCATE (GWFLAKDAT(IGRID)%MSUB1)
      DEALLOCATE (GWFLAKDAT(IGRID)%MSUB)
      DEALLOCATE (GWFLAKDAT(IGRID)%FLXINL)
      DEALLOCATE (GWFLAKDAT(IGRID)%VOLOLD)
      DEALLOCATE (GWFLAKDAT(IGRID)%GWIN)
      DEALLOCATE (GWFLAKDAT(IGRID)%GWOUT)
      DEALLOCATE (GWFLAKDAT(IGRID)%DELH)
      DEALLOCATE (GWFLAKDAT(IGRID)%TDELH)
      DEALLOCATE (GWFLAKDAT(IGRID)%SVT)
      DEALLOCATE (GWFLAKDAT(IGRID)%STGADJ)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTGWIN_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTGWOT_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTDELSTOR_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTSTOR_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTEVAP_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTPPT_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTRUNF_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTWTHDRW_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTSURFIN_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTSURFOT_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%VOLOLDD)
Cdep  Added arrays that calculate lake budgets 6/9/2009
      DEALLOCATE (GWFLAKDAT(IGRID)%DELVOL)
      DEALLOCATE (GWFLAKDAT(IGRID)%TSLAKERR)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMVOL)
      DEALLOCATE (GWFLAKDAT(IGRID)%CMLAKERR)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMLKIN)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMLKOUT)
      END SUBROUTINE GWF2LAK7DA

      SUBROUTINE SGWF2LAK7PNT(IGRID)
C  Set pointers to LAK data for grid      
      USE GWFLAKMODULE
C
      NLAKES=>GWFLAKDAT(IGRID)%NLAKES
      NLAKESAR=>GWFLAKDAT(IGRID)%NLAKESAR
      ILKCB=>GWFLAKDAT(IGRID)%ILKCB
      LAKTAB=>GWFLAKDAT(IGRID)%LAKTAB
      IRDTAB=>GWFLAKDAT(IGRID)%IRDTAB
      NSSITR=>GWFLAKDAT(IGRID)%NSSITR
      MXLKND=>GWFLAKDAT(IGRID)%MXLKND
      LKNODE=>GWFLAKDAT(IGRID)%LKNODE
      ICMX=>GWFLAKDAT(IGRID)%ICMX
      NCLS=>GWFLAKDAT(IGRID)%NCLS
      LWRT=>GWFLAKDAT(IGRID)%LWRT
      NDV=>GWFLAKDAT(IGRID)%NDV
      NTRB=>GWFLAKDAT(IGRID)%NTRB
      THETA=>GWFLAKDAT(IGRID)%THETA
      SSCNCR=>GWFLAKDAT(IGRID)%SSCNCR
Cdep  added SURFDEPTH 3/3/2009
      SURFDEPTH=>GWFLAKDAT(IGRID)%SURFDEPTH
      ICS=>GWFLAKDAT(IGRID)%ICS
      NCNCVR=>GWFLAKDAT(IGRID)%NCNCVR
      LIMERR=>GWFLAKDAT(IGRID)%LIMERR
      ILAKE=>GWFLAKDAT(IGRID)%ILAKE
      ITRB=>GWFLAKDAT(IGRID)%ITRB
      IDIV=>GWFLAKDAT(IGRID)%IDIV
      ISUB=>GWFLAKDAT(IGRID)%ISUB
      IRK=>GWFLAKDAT(IGRID)%IRK
      LKARR1=>GWFLAKDAT(IGRID)%LKARR1
      STAGES=>GWFLAKDAT(IGRID)%STAGES
      STGNEW=>GWFLAKDAT(IGRID)%STGNEW
      STGOLD=>GWFLAKDAT(IGRID)%STGOLD
      STGOLD2=>GWFLAKDAT(IGRID)%STGOLD2
      STGITER=>GWFLAKDAT(IGRID)%STGITER
      VOL=>GWFLAKDAT(IGRID)%VOL
      FLOB=>GWFLAKDAT(IGRID)%FLOB
      DSRFOT=>GWFLAKDAT(IGRID)%DSRFOT
      PRCPLK=>GWFLAKDAT(IGRID)%PRCPLK
      EVAPLK=>GWFLAKDAT(IGRID)%EVAPLK
      BEDLAK=>GWFLAKDAT(IGRID)%BEDLAK
      WTHDRW=>GWFLAKDAT(IGRID)%WTHDRW
      RNF=>GWFLAKDAT(IGRID)%RNF
      CUMRNF=>GWFLAKDAT(IGRID)%CUMRNF
      CUMPPT=>GWFLAKDAT(IGRID)%CUMPPT
      CUMEVP=>GWFLAKDAT(IGRID)%CUMEVP
      CUMGWI=>GWFLAKDAT(IGRID)%CUMGWI
      CUMGWO=>GWFLAKDAT(IGRID)%CUMGWO
      CUMSWI=>GWFLAKDAT(IGRID)%CUMSWI
      CUMSWO=>GWFLAKDAT(IGRID)%CUMSWO
      CUMWDR=>GWFLAKDAT(IGRID)%CUMWDR
      CUMFLX=>GWFLAKDAT(IGRID)%CUMFLX
      CNDFCT=>GWFLAKDAT(IGRID)%CNDFCT
      VOLINIT=>GWFLAKDAT(IGRID)%VOLINIT
      BOTTMS=>GWFLAKDAT(IGRID)%BOTTMS
      BGAREA=>GWFLAKDAT(IGRID)%BGAREA
      SSMN=>GWFLAKDAT(IGRID)%SSMN
      SSMX=>GWFLAKDAT(IGRID)%SSMX
      EVAP=>GWFLAKDAT(IGRID)%EVAP
      PRECIP=>GWFLAKDAT(IGRID)%PRECIP
      EVAP3=>GWFLAKDAT(IGRID)%EVAP3
      PRECIP3=>GWFLAKDAT(IGRID)%PRECIP3
      SEEP=>GWFLAKDAT(IGRID)%SEEP
      SEEP3=>GWFLAKDAT(IGRID)%SEEP3
      SURFA=>GWFLAKDAT(IGRID)%SURFA
      SURFIN=>GWFLAKDAT(IGRID)%SURFIN
      SURFOT=>GWFLAKDAT(IGRID)%SURFOT
      SUMCNN=>GWFLAKDAT(IGRID)%SUMCNN
      SUMCHN=>GWFLAKDAT(IGRID)%SUMCHN
      CLAKE=>GWFLAKDAT(IGRID)%CLAKE
      CRNF=>GWFLAKDAT(IGRID)%CRNF
      SILLVT=>GWFLAKDAT(IGRID)%SILLVT
      CAUG=>GWFLAKDAT(IGRID)%CAUG
      CPPT=>GWFLAKDAT(IGRID)%CPPT
      CLAKINIT=>GWFLAKDAT(IGRID)%CLAKINIT
      BDLKN1=>GWFLAKDAT(IGRID)%BDLKN1
Cdep  Added arrays that track lake budgets for dry lakes
      EVAPO=>GWFLAKDAT(Igrid)%EVAPO
      WITHDRW=>GWFLAKDAT(Igrid)%WITHDRW
      FLWIN=>GWFLAKDAT(Igrid)%FLWIN
      FLWITER=>GWFLAKDAT(Igrid)%FLWITER
      FLWITER3=>GWFLAKDAT(Igrid)%FLWITER3
      GWRATELIM=>GWFLAKDAT(Igrid)%GWRATELIM
Cdep  added two variable arrays
      OVRLNDRNF=>GWFLAKDAT(Igrid)%OVRLNDRNF
      CUMLNDRNF=>GWFLAKDAT(Igrid)%CUMLNDRNF
      CUMUZF=>GWFLAKDAT(Igrid)%CUMUZF
Cdep  added three variable arrays for depth,area, and volume
      DEPTHTABLE=>GWFLAKDAT(Igrid)%DEPTHTABLE
      AREATABLE=>GWFLAKDAT(Igrid)%AREATABLE    
      VOLUMETABLE=>GWFLAKDAT(Igrid)%VOLUMETABLE   
      XLAKES=>GWFLAKDAT(Igrid)%XLAKES
      XLAKINIT=>GWFLAKDAT(Igrid)%XLAKINIT
      XLKOLD=>GWFLAKDAT(Igrid)%XLKOLD
Crsr allocate BD arrays
      LDRY=>GWFLAKDAT(IGRID)%LDRY
      NCNT=>GWFLAKDAT(IGRID)%NCNT
      NCNST=>GWFLAKDAT(IGRID)%NCNST
      KSUB=>GWFLAKDAT(IGRID)%KSUB
      MSUB1=>GWFLAKDAT(IGRID)%MSUB1
      MSUB=>GWFLAKDAT(IGRID)%MSUB
      FLXINL=>GWFLAKDAT(IGRID)%FLXINL
      VOLOLD=>GWFLAKDAT(IGRID)%VOLOLD
      GWIN=>GWFLAKDAT(IGRID)%GWIN
      GWOUT=>GWFLAKDAT(IGRID)%GWOUT
      DELH=>GWFLAKDAT(IGRID)%DELH
      TDELH=>GWFLAKDAT(IGRID)%TDELH
      SVT=>GWFLAKDAT(IGRID)%SVT
      STGADJ=>GWFLAKDAT(IGRID)%STGADJ
      TOTGWIN_LAK=>GWFLAKDAT(IGRID)%TOTGWIN_LAK
      TOTGWOT_LAK=>GWFLAKDAT(IGRID)%TOTGWOT_LAK
      TOTDELSTOR_LAK=>GWFLAKDAT(IGRID)%TOTDELSTOR_LAK
      TOTSTOR_LAK=>GWFLAKDAT(IGRID)%TOTSTOR_LAK
      TOTEVAP_LAK=>GWFLAKDAT(IGRID)%TOTEVAP_LAK
      TOTPPT_LAK=>GWFLAKDAT(IGRID)%TOTPPT_LAK
      TOTRUNF_LAK=>GWFLAKDAT(IGRID)%TOTRUNF_LAK
      TOTWTHDRW_LAK=>GWFLAKDAT(IGRID)%TOTWTHDRW_LAK
      TOTSURFIN_LAK=>GWFLAKDAT(IGRID)%TOTSURFIN_LAK
      TOTSURFOT_LAK=>GWFLAKDAT(IGRID)%TOTSURFOT_LAK
      LAKUNIT=>GWFLAKDAT(IGRID)%LAKUNIT
      VOLOLDD=>GWFLAKDAT(IGRID)%VOLOLDD
Cdep  Allocate lake budget error arrays 6/9/2009
      DELVOL=>GWFLAKDAT(IGRID)%DELVOL
      TSLAKERR=>GWFLAKDAT(IGRID)%TSLAKERR
      CUMVOL=>GWFLAKDAT(IGRID)%CUMVOL
      CMLAKERR=>GWFLAKDAT(IGRID)%CMLAKERR
      CUMLKOUT=>GWFLAKDAT(IGRID)%CUMLKOUT
      CUMLKIN=>GWFLAKDAT(IGRID)%CUMLKIN
      END SUBROUTINE SGWF2LAK7PNT

      SUBROUTINE SGWF2LAK7PSV1(IGRID)
C  Save LAK data for a grid for data shared with SFR
      USE GWFLAKMODULE
C
      GWFLAKDAT(IGRID)%NLAKES=>NLAKES
      GWFLAKDAT(IGRID)%NLAKESAR=>NLAKESAR
      GWFLAKDAT(IGRID)%THETA=>THETA
      GWFLAKDAT(IGRID)%STGOLD=>STGOLD
      GWFLAKDAT(IGRID)%STGOLD2=>STGOLD2
      GWFLAKDAT(IGRID)%STGNEW=>STGNEW
      GWFLAKDAT(IGRID)%STGITER=>STGITER
      GWFLAKDAT(IGRID)%VOL=>VOL
      GWFLAKDAT(IGRID)%LAKUNIT=>LAKUNIT
      END SUBROUTINE SGWF2LAK7PSV1

      SUBROUTINE SGWF2LAK7PSV(IGRID)
C  Save LAK data for a grid
      USE GWFLAKMODULE
C
      GWFLAKDAT(IGRID)%ILKCB=>ILKCB
      GWFLAKDAT(IGRID)%NSSITR=>NSSITR
      GWFLAKDAT(IGRID)%MXLKND=>MXLKND
      GWFLAKDAT(IGRID)%LKNODE=>LKNODE
      GWFLAKDAT(IGRID)%ICMX=>ICMX
      GWFLAKDAT(IGRID)%LAKTAB=>LAKTAB
      GWFLAKDAT(IGRID)%IRDTAB=>IRDTAB
      GWFLAKDAT(IGRID)%NCLS=>NCLS
      GWFLAKDAT(IGRID)%LWRT=>LWRT
      GWFLAKDAT(IGRID)%NDV=>NDV
      GWFLAKDAT(IGRID)%NTRB=>NTRB
      GWFLAKDAT(IGRID)%SSCNCR=>SSCNCR
Cdep  Added SURDEPTH 3/3/2009
      GWFLAKDAT(IGRID)%SURFDEPTH=>SURFDEPTH
      GWFLAKDAT(IGRID)%ICS=>ICS
      GWFLAKDAT(IGRID)%NCNCVR=>NCNCVR
      GWFLAKDAT(IGRID)%LIMERR=>LIMERR
      GWFLAKDAT(IGRID)%ILAKE=>ILAKE
      GWFLAKDAT(IGRID)%ITRB=>ITRB
      GWFLAKDAT(IGRID)%IDIV=>IDIV
      GWFLAKDAT(IGRID)%ISUB=>ISUB
      GWFLAKDAT(IGRID)%IRK=>IRK
      GWFLAKDAT(IGRID)%LKARR1=>LKARR1
      GWFLAKDAT(IGRID)%STAGES=>STAGES
      GWFLAKDAT(IGRID)%FLOB=>FLOB
      GWFLAKDAT(IGRID)%DSRFOT=>DSRFOT
      GWFLAKDAT(IGRID)%PRCPLK=>PRCPLK
      GWFLAKDAT(IGRID)%EVAPLK=>EVAPLK
      GWFLAKDAT(IGRID)%BEDLAK=>BEDLAK
      GWFLAKDAT(IGRID)%WTHDRW=>WTHDRW
      GWFLAKDAT(IGRID)%RNF=>RNF
      GWFLAKDAT(IGRID)%CUMRNF=>CUMRNF
      GWFLAKDAT(IGRID)%CUMPPT=>CUMPPT
      GWFLAKDAT(IGRID)%CUMEVP=>CUMEVP
      GWFLAKDAT(IGRID)%CUMGWI=>CUMGWI
      GWFLAKDAT(IGRID)%CUMGWO=>CUMGWO
      GWFLAKDAT(IGRID)%CUMSWI=>CUMSWI
      GWFLAKDAT(IGRID)%CUMSWO=>CUMSWO
      GWFLAKDAT(IGRID)%CUMWDR=>CUMWDR
      GWFLAKDAT(IGRID)%CUMFLX=>CUMFLX
      GWFLAKDAT(IGRID)%CNDFCT=>CNDFCT
      GWFLAKDAT(IGRID)%VOLINIT=>VOLINIT
      GWFLAKDAT(IGRID)%BOTTMS=>BOTTMS
      GWFLAKDAT(IGRID)%BGAREA=>BGAREA
      GWFLAKDAT(IGRID)%SSMN=>SSMN
      GWFLAKDAT(IGRID)%SSMX=>SSMX
      GWFLAKDAT(IGRID)%EVAP=>EVAP
      GWFLAKDAT(IGRID)%PRECIP=>PRECIP
      GWFLAKDAT(IGRID)%EVAP3=>EVAP3
      GWFLAKDAT(IGRID)%PRECIP3=>PRECIP3
      GWFLAKDAT(IGRID)%SEEP=>SEEP
      GWFLAKDAT(IGRID)%SEEP3=>SEEP3
      GWFLAKDAT(IGRID)%SURFA=>SURFA
      GWFLAKDAT(IGRID)%SURFIN=>SURFIN
      GWFLAKDAT(IGRID)%SURFOT=>SURFOT
      GWFLAKDAT(IGRID)%SUMCNN=>SUMCNN
      GWFLAKDAT(IGRID)%SUMCHN=>SUMCHN
      GWFLAKDAT(IGRID)%CLAKE=>CLAKE
      GWFLAKDAT(IGRID)%CRNF=>CRNF
      GWFLAKDAT(IGRID)%SILLVT=>SILLVT
      GWFLAKDAT(IGRID)%CAUG=>CAUG
      GWFLAKDAT(IGRID)%CPPT=>CPPT
      GWFLAKDAT(IGRID)%CLAKINIT=>CLAKINIT
      GWFLAKDAT(IGRID)%BDLKN1=>BDLKN1
Cdep  Added arrays that track lake budgets for dry lakes
      GWFLAKDAT(Igrid)%EVAPO=>EVAPO
      GWFLAKDAT(Igrid)%WITHDRW=>WITHDRW
      GWFLAKDAT(Igrid)%FLWIN=>FLWIN
      GWFLAKDAT(Igrid)%FLWITER=>FLWITER
      GWFLAKDAT(Igrid)%FLWITER3=>FLWITER3
      GWFLAKDAT(Igrid)%GWRATELIM=>GWRATELIM
Cdep  added two variable arrays
      GWFLAKDAT(Igrid)%OVRLNDRNF=>OVRLNDRNF
      GWFLAKDAT(Igrid)%CUMLNDRNF=>CUMLNDRNF
      GWFLAKDAT(Igrid)%CUMUZF=>CUMUZF
Cdep  added three variable arrays for depth, area, and volume
      GWFLAKDAT(Igrid)%DEPTHTABLE=>DEPTHTABLE
      GWFLAKDAT(Igrid)%AREATABLE=>AREATABLE
      GWFLAKDAT(Igrid)%VOLUMETABLE=>VOLUMETABLE
      GWFLAKDAT(Igrid)%XLAKES=>XLAKES
      GWFLAKDAT(Igrid)%XLAKINIT=>XLAKINIT
      GWFLAKDAT(Igrid)%XLKOLD=>XLKOLD
Crsr allocate BD arrays
      GWFLAKDAT(IGRID)%LDRY=>LDRY
      GWFLAKDAT(IGRID)%NCNT=>NCNT
      GWFLAKDAT(IGRID)%NCNST=>NCNST
      GWFLAKDAT(IGRID)%KSUB=>KSUB
      GWFLAKDAT(IGRID)%MSUB1=>MSUB1
      GWFLAKDAT(IGRID)%MSUB=>MSUB
      GWFLAKDAT(IGRID)%FLXINL=>FLXINL
      GWFLAKDAT(IGRID)%VOLOLD=>VOLOLD
      GWFLAKDAT(IGRID)%GWIN=>GWIN
      GWFLAKDAT(IGRID)%GWOUT=>GWOUT
      GWFLAKDAT(IGRID)%DELH=>DELH
      GWFLAKDAT(IGRID)%TDELH=>TDELH
      GWFLAKDAT(IGRID)%SVT=>SVT
      GWFLAKDAT(IGRID)%STGADJ=>STGADJ
Cdep  Allocate lake budget error arrays 6/9/2009
      GWFLAKDAT(IGRID)%DELVOL=>DELVOL
      GWFLAKDAT(IGRID)%TSLAKERR=>TSLAKERR
      GWFLAKDAT(IGRID)%CUMVOL=>CUMVOL
      GWFLAKDAT(IGRID)%CMLAKERR=>CMLAKERR
      GWFLAKDAT(IGRID)%CUMLKOUT=>CUMLKOUT
      GWFLAKDAT(IGRID)%CUMLKIN=>CUMLKIN
crgn Allocate budget arrays for GSFLOW CSV file
      GWFLAKDAT(IGRID)%TOTGWIN_LAK=>TOTGWIN_LAK
      GWFLAKDAT(IGRID)%TOTGWOT_LAK=>TOTGWOT_LAK
      GWFLAKDAT(IGRID)%TOTDELSTOR_LAK=>TOTDELSTOR_LAK
      GWFLAKDAT(IGRID)%TOTSTOR_LAK=>TOTSTOR_LAK
      GWFLAKDAT(IGRID)%TOTEVAP_LAK=>TOTEVAP_LAK
      GWFLAKDAT(IGRID)%TOTPPT_LAK=>TOTPPT_LAK
      GWFLAKDAT(IGRID)%TOTRUNF_LAK=>TOTRUNF_LAK
      GWFLAKDAT(IGRID)%TOTWTHDRW_LAK=>TOTWTHDRW_LAK
      GWFLAKDAT(IGRID)%TOTSURFIN_LAK=>TOTSURFIN_LAK
      GWFLAKDAT(IGRID)%TOTSURFOT_LAK=>TOTSURFOT_LAK        
      GWFLAKDAT(IGRID)%VOLOLDD=>VOLOLDD       
      END SUBROUTINE SGWF2LAK7PSV
