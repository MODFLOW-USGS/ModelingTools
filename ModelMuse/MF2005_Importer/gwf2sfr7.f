C
C
C     ******************************************************************
C     CHECK FOR STEAMBED BELOW CELL BOTTOM. RECORD REACHES FOR PRINTING
C     ******************************************************************
      MODULE ICHKSTRBOT_MODULE
      USE GWFSFRMODULE,ONLY:ISTRM,STRM,NSTRM
      USE GLOBAL,ONLY:BOTM,IBOUND,LBOTM
      implicit none
      type check_bot
        integer ltype,irchnum,iflag,iunit
      end type check_bot
      public check_bot
      CONTAINS
      FUNCTION ICHKSTRBOT(self)
      type (check_bot), intent(in) :: self
      INTEGER JRCH,IRCH,KRCH,JSEG,ISEG,ICHKSTRBOT
      ICHKSTRBOT = 0
      KRCH = ISTRM(1,self%IRCHNUM)
      IRCH = ISTRM(2,self%IRCHNUM)
      JRCH = ISTRM(3,self%IRCHNUM)
      JSEG = ISTRM(4,self%IRCHNUM)
      ISEG = ISTRM(5,self%IRCHNUM)
      IF ( self%LTYPE.GT.0  .AND. IBOUND(JRCH,IRCH,KRCH).GT.0 ) THEN
        IF ( STRM(4, self%IRCHNUM)-BOTM(JRCH,IRCH,LBOTM(KRCH))
     +                                      .LT.-1.0E-12 ) THEN
          IF ( self%IFLAG.EQ.0 ) THEN
          WRITE(self%IUNIT,*)
          WRITE(self%IUNIT,*)' REACHES WITH ALTITUDE ERRORS:'
          WRITE(self%IUNIT,*)'   LAY    ROW    COL    SEG  REACH      ',
     +                'STR.ELEV.      CELL-BOT.'
          END IF
          WRITE(self%IUNIT,100)KRCH,IRCH,JRCH,JSEG,ISEG,
     +                STRM(4, self%IRCHNUM),BOTM(JRCH,IRCH,LBOTM(KRCH))
          ICHKSTRBOT = 1
        END IF
      END IF
!      IF ( self%IFLAG.GT.0 .AND. self%IRCHNUM.EQ.NSTRM ) THEN
!        WRITE(self%IUNIT,*)' MODEL STOPPING DUE TO REACH ALTITUDE ERROR'
!        CALL USTOP(' ')
!      END IF
  100 FORMAT(5I7,2F15.7)
      END FUNCTION ICHKSTRBOT
      END MODULE ICHKSTRBOT_MODULE
C
C-------SUBROUTINE GWF2SFR7AR
      SUBROUTINE GWF2SFR7AR(In, Iunitbcf, Iunitlpf, Iunithuf, Iunitgwt,
     +                      Nsol, Iouts, Iunitupw, Iunituzf, Igrid)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR STREAMS
C     INITIALIZE VARIABLES FOR SFR PACKAGES
C     READ STREAM DATA THAT IS CONSTANT FOR ENTIRE SIMULATION:
C     REACH DATA AND PARAMETER DEFINITIONS
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     ******************************************************************
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFSFRMODULE
      USE GLOBAL,       ONLY: IOUT, IBOUND, BOTM, STRT, DELR, DELC,
     +                        ITRSS,NCOL,NROW,LAYHDT  !CJM added ncol and nrow
      USE GWFLPFMODULE, ONLY: SC2LPF=>SC2, LAYTYP
      USE GWFBCFMODULE, ONLY: SC1, SC2, LAYCON
      USE GWFHUFMODULE, ONLY: SC2HUF, LTHUF
      USE GWFUPWMODULE, ONLY: SC2UPW
      USE ICHKSTRBOT_MODULE
      IMPLICIT NONE
      INTRINSIC ABS, DBLE
      type (check_bot) :: uzfar_check
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER In, Iunitbcf, Iunitlpf, Iunithuf, Iunitgwt, Nsol, Iouts,
     +        Iunitupw, Iunituzf, Igrid
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      CHARACTER*200 line
!IFACE
      CHARACTER*8 face
      INTEGER iface,ndash
      INTEGER lloc, istart, istop, nparseg, i, ii, nlst, lb, ichk, icalc
      INTEGER nseg, nreach, krch, irch, jrch, jseg, ireach, ksfropt
      INTEGER krck, irck, jrck, jsegck, ireachck, kkptflg, ib
      INTEGER lstsum, lstbeg, numinst, idum(1), ip, iterp, mstrmar
      INTEGER nssar, nstrmar, Ltyp, NPP, MXVL, IRFG, ITRFLG
      INTEGER k, kkrch, IERR, IFLG
      REAL r, seglen, sumlen, thsslpe, thislpe, uhcslpe, rchlen, dist
      REAL epsslpe
      character(len=40) :: keyvalue
      logical found
      real factor
C     ------------------------------------------------------------------
      Version_sfr =
     +'$Id: gwf2sfr7.f 2359 2011-12-29 00:41:23Z rniswon $'
      iterp = 1
      idum(1) = 0
      ALLOCATE (NSS, NSTRM,TOTSPFLOW)
      ALLOCATE (NSFRPAR, ISTCB1, ISTCB2, IUZT, MAXPTS)
      ALLOCATE (ISFROPT, NSTRAIL, ISUZN, NSFRSETS)
      ALLOCATE (NUZST, NSTOTRL, NUMAVE)
      ALLOCATE (ITMP, IRDFLG, IPTFLG, NP)
      ALLOCATE (CONST, DLEAK, IRTFLG, NUMTIM, WEIGHT, FLWTOL)
      ALLOCATE (NSEGDIM)
      ALLOCATE (SFRRATIN, SFRRATOUT)
      ALLOCATE (STRMDELSTOR_CUM, STRMDELSTOR_RATE)
C1------IDENTIFY PACKAGE AND INITIALIZE NSTRM.
      WRITE (IOUT, *) 'SFR:'
!      WRITE (IOUT, 9001) In
! 9001 FORMAT (1X, /, ' SFR7 -- STREAMFLOW ROUTING PACKAGE, '
!     +        ,'VERSION 1.0.4, 2012-02-06', /, 9X,
!     +         'INPUT READ FROM UNIT', I4)
C
C2------READ COMMENT RECORDS, NSTRM, NSS, NSFRPAR, NPARSEG, CONST,
C         DLEAK, ISTCB1, ISTCB2.
      ISFROPT = 0
      IUZT = 0
      IRTFLG = 0
      NUMTIM = 1
      NSEGDIM = 1
      FLWTOL = 1.0E-4
      STRMDELSTOR_CUM = 0.0E0
      STRMDELSTOR_RATE = 0.0E0
      SFRRATIN = 0.0
      SFRRATOUT = 0.0
      TOTSPFLOW = 0.0D0
      NUMTAB = 0
      MAXVAL = 1
      IRFG = 0
      MXVL = 0
      NPP = 0
      ITRFLG = 0
      lloc = 1
      IERR = 0
      IFLG = 0
      CALL URDCOM(In, IOUT, line)
! Check for alternate input (replacement for setting NSTRM<0).
      CALL UPARLSTAL(IN,IOUT,LINE,NPP,MXVL)
      lloc = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      keyvalue = LINE(ISTART:ISTOP)
      call upcase(keyvalue)
      IF(keyvalue.EQ.'OPTIONS') THEN
          do
          CALL URDCOM(In, IOUT, line)
          lloc = 1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
          keyvalue = LINE(ISTART:ISTOP)
          call upcase(keyvalue)
          select case (keyvalue)
            case('REACHINPUT')
              IRFG = 1
!              WRITE(IOUT,32)
!   32   FORMAT(1X,I10,' Some stream information will be read by reach. ',
!     +                  'This option replaces NSTRM<0')
              found = .true.
            case('TRANSROUTE')
              ITRFLG = 1
!              WRITE(iout,*)
!              WRITE(IOUT,'(A)')' TRANSIENT ROUTING IN STREAMS IS ACTIVE'
!              WRITE(iout,*)
              found = .true.
            case('TABFILES')
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMTAB,R,IOUT,IN)
              IF(NUMTAB.LT.0) NUMTAB=0
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MAXVAL,R,IOUT,IN)
              IF(MAXVAL.LT.0) MAXVAL=0
              WRITE(IOUT,*) 'NUMTAB,MAXVAL:'
              WRITE(IOUT,*) NUMTAB,MAXVAL
!              WRITE(IOUT,31) NUMTAB,MAXVAL
!   31      FORMAT(1X,I10,' Specifed inflow files will be read ',
!     +                   'with a maximum of ',I10,' row entries per file')
              found = .true.
            case('LOSSFACTOR')
!              WRITE(IOUT,*)
              CALL URWORD(line, lloc, istart, istop, 3, i,
     *          FACTOR,IOUT,In)
              WRITE(IOUT,*) 'FACTOR:'
              WRITE(IOUT,*) FACTOR
!              WRITE(IOUT,322) FACTOR
!  322      FORMAT('Stream loss will be calculated as a factor ',
!     +                   'of the streambed hydraulic conductivity. ',
!     +                   'Multiplication factor is equal to ',E20.10)
             found = .true.
          case ('END')
            CALL URDCOM(In, IOUT, line)
            exit
          case default
    !   -- No options found
          found = .false.
          CALL URDCOM(In, IOUT, line)
          exit
          end select
        end do
      ELSE
        IF(LINE(ISTART:ISTOP).EQ.'REACHINPUT') THEN
           IRFG = 1
!         WRITE(IOUT,32)
!   32  FORMAT(1X,I10,' Some stream information will be read by reach. ',
!     +                'This option replaces NSTRM<0')
!      ELSE
!         WRITE(IOUT,'(A)') ' Segment information will not be ',
!     +                     ' read by reach'
        END IF
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'TRANSROUTE') THEN
          ITRFLG = 1
        END IF
        IF ( ITRFLG.EQ.1 .OR. IRFG.EQ.1 ) READ(IN,'(A)') LINE
! Check keyword for tabular inflow rates.
        CALL UPARLSTAL(IN,IOUT,LINE,NPP,MXVL)
        lloc = 1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'TABFILES') THEN
           CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMTAB,R,IOUT,IN)
           IF(NUMTAB.LT.0) NUMTAB=0
           CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MAXVAL,R,IOUT,IN)
           IF(MAXVAL.LT.0) MAXVAL=0
           WRITE(IOUT,*) 'NUMTAB,MAXVAL:'
           WRITE(IOUT,*) NUMTAB,MAXVAL
!   31    FORMAT(1X,I10,' Specifed inflow files will be read ',
!     +                 'with a maximum of ',I10,' row entries per file')
           READ(IN,'(A)') LINE
!      ELSE
!         WRITE(IOUT,'(A)') ' No specifed inflow files'
        END IF
      ENDIF
      lloc = 1
      CALL URWORD(line, lloc, istart, istop, 2, NSTRM, r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, NSS, r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, NSFRPAR, r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, nparseg, r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 3, i, CONST, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 3, i, DLEAK, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, ISTCB1, r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, ISTCB2, r, IOUT, In)
	WRITE(IOUT, *)
	1  'NSTRM, NSS, NSFRPAR, NPARSEG, CONST, DLEAK, ISTCB1, ISTCB2:'
	WRITE(IOUT, *) NSTRM, NSS, NSFRPAR, NPARSEG, CONST, DLEAK,
	1   ISTCB1, ISTCB2
      IF ( NSTRM.LT.0 ) THEN
!        WRITE(IOUT, 9036)
! 9036   FORMAT (//, 'NSTRM IS NEGATIVE AND THIS METHOD FOR ',
!     +          'SPECIFYING INFORMATION BY REACH HAS BEEN REPLACED BY ',
!     +          'THE KEYWORD OPTION "REACHINPUT"--PROGRAM STOPPING ',/)
!        CALL USTOP(' ')
        IRFG = 1
        NSTRM = ABS(NSTRM)
      END IF
C
C3------READ ISFROPT FLAGS WHEN NSTRM IS LESS THAN ZERO.
      IF ( IRFG.GT.0 ) THEN
        IF ( NSFRPAR.GT.0 ) THEN
          WRITE(IOUT, 9002)
 9002  FORMAT (//, 'KEYWORD "REACHINPUT" IS SPECIFIED AND NSFRPAR IS ',
     +  'GREATER THAN  ZERO ', /1X , ' ALTERNATE SFR7 OPTIONS DO NOT ',
     +  'SUPPORT PARAMETERS--PROGRAM STOPPING ',/)
          CALL USTOP(' ')
        END IF
!        NSTRM = ABS(NSTRM)
        CALL URWORD(line, lloc, istart, istop, 2, ISFROPT, r, IOUT, In)
	  WRITE(IOUT, *) 'ISFROPT:'
	  WRITE(IOUT, *) ISFROPT
C
C4------READ UNSATURATED FLOW VARIABLES WHEN ISFROPT GREATER THAN 1.
        IF ( ISFROPT.GE.2 ) THEN
          IUZT = 1
          CALL URWORD(line, lloc, istart, istop, 2, NSTRAIL, r, IOUT,
     +                In)
          CALL URWORD(line, lloc, istart, istop, 2, ISUZN, r, IOUT, In)
          CALL URWORD(line, lloc, istart, istop, 2, NSFRSETS, r, IOUT,
     +                In)
	    WRITE(IOUT, *) 'NSTRAIL, ISUZN, NSFRSETS:'
	    WRITE(IOUT, *) NSTRAIL, ISUZN, NSFRSETS
        END IF
!4b-----Data read for transient routing.
        IF ( ITRFLG.EQ.1 ) THEN
          CALL URWORD(line, lloc, istart, istop, 2, IRTFLG, r, IOUT, In)
          IF ( IRTFLG .GT. 0 ) THEN
            NUMTIM = 1
            WEIGHT = 1.0
            FLWTOL = 1.0D-6
            CALL URWORD(line, lloc, istart, istop,2,NUMTIM, r, IOUT, In)
            CALL URWORD(line, lloc, istart, istop,3,i, WEIGHT, IOUT, In)
            CALL URWORD(line, lloc, istart, istop,3,i, FLWTOL, IOUT, In)
            IF ( NUMTIM.LT.1 ) NUMTIM = 1
            IF ( WEIGHT.LT.0.0 .OR. WEIGHT.GT.1.0 ) WEIGHT=1.0
            IF ( FLWTOL.LT.1.0e-6 ) FLWTOL=1.0e-6
          ELSE
            NUMTIM = 1
            WEIGHT = 1.0
            FLWTOL = 1.0e-6
          END IF
        END IF
	  WRITE(IOUT, *) 'IRTFLG, NUMTIM, WEIGHT, FLWTOL:'
	  WRITE(IOUT, *) IRTFLG, NUMTIM, WEIGHT, FLWTOL
      END IF
      IF ( NSS.LT.0 ) NSS = 0
      IF ( NSFRPAR.LE.0 ) THEN
        NSFRPAR = 0
        nparseg = 0
      END IF
      IF ( nparseg.LT.0 ) nparseg = 0
! RGN if ISFROPT=2 or 4 then you cannot use BCF Package.
      IF ( Iunitbcf.GT.0 ) THEN
        IF ( ISFROPT.EQ.2 .OR. ISFROPT.EQ.4 ) THEN
          WRITE(IOUT, 9045)
 9045  FORMAT (//, 'If the BCF Package is used and unsaturated ',
     +  'flow is active then ISFROPT must equal 3 or 5. ',
     +  '--PROGRAM STOPPING ',/)
          CALL USTOP(' ')
        END IF
      END IF
      nssar = 1
      IF (NSS.GT.0) nssar = NSS
      nstrmar = 1
      IF (NSTRM.GT.0) nstrmar = NSTRM
! RGN put NSEGDIM into module for FMP 10/15/11
      NSEGDIM = NSS + nparseg
      IF (nsegdim.LT.1) nsegdim = 1
!IFACE
!4c-----Look for IFACE Flag
      iface=0
      CALL URWORD(line,lloc,istart,istop,1,i,r,IOUT,In)
      IF(line(istart:istop).EQ.'IFACE') THEN
        iface=1
!        WRITE ( IOUT, '(1X,A)')
!     +    'IFACE values will be read from reach records'
      END IF
C
C5------CALCULATE SPACE NEEDED FOR TABULATED DISCHARGE VERSUS FLOW
C         AND WIDTH RELATIONS.
      MAXPTS = 3*50
C
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR STREAMS
C     ******************************************************************
C
Cdep  changed DSTROT to FXLKOT
      ALLOCATE (STRIN(nssar), STROUT(nssar), FXLKOT(nssar))
      STRIN = 0.0
      STROUT = 0.0
      FXLKOT = 0.0
!IFACE -- 6th element of ISTRM is for IFACE
      ALLOCATE (STRM(30,nstrmar), ISTRM(6,nstrmar))
      ALLOCATE (HSTRM(nstrmar,NUMTIM), HWDTH(nstrmar,NUMTIM))
      ALLOCATE (QSTRM(nstrmar,NUMTIM))
      ALLOCATE (HWTPRM(nstrmar,NUMTIM))
      ALLOCATE (DVRCH(nstrmar),DVEFF(nstrmar))        !cjm
      ALLOCATE (DVRCELL(500,2,nss))  !cjm
      ALLOCATE (RECHSAVE(NCOL,NROW),DVRPERC(NCOL,NROW))   !cjm
      ALLOCATE (FNETSEEP(NCOL,NROW)) !rgn printing net recharge in UZF
      STRM = 0.0
      HSTRM = 0.0
      QSTRM = 0.0
      HWDTH = 0.0
      HWTPRM = 0.0
      ISTRM = 0
      DVRCH = 0       !cjm
      DVEFF = 0.0     !cjm
      DVRCELL = 0     !cjm
      RECHSAVE = 0.0  !cjm
      DVRPERC = 0.0   !cjm
      FNETSEEP = 0.0  !rgn
      ALLOCATE (SEG(26,nsegdim), ISEG(4,nsegdim), IDIVAR(2,nsegdim))
Cdep  allocate space for stream outflow derivatives for lake package
      ALLOCATE (DLKOTFLW(200,nssar), SLKOTFLW(200,nssar))
      ALLOCATE (DLKSTAGE(200,nssar))
      IF ( NUMTAB.GT.0 ) THEN
        ALLOCATE (TABFLOW(MAXVAL,NSS), TABTIME(MAXVAL,NSS))
      ELSE
        ALLOCATE (TABFLOW(1,1), TABTIME(1,1))
      END IF
      ALLOCATE (ISFRLIST(3,nssar))
      TABFLOW = 0.0
      TABTIME = 0.0
      ISFRLIST = 0
      SEG = 0.0
      ISEG = 0
      IDIVAR = 0
      DLKOTFLW = 0.0D0
      DLKSTAGE = 0.0D0
      SLKOTFLW = 0.0D0
      ALLOCATE (IOTSG(nsegdim))
      IOTSG = 0
      ALLOCATE (SFRQ(5,nstrmar))
      SFRQ = 0.0
      IF ( Iunitgwt.GT.0 ) THEN
        ALLOCATE (CONCQ(nsegdim,Nsol), CONCRUN(nsegdim,Nsol))
        ALLOCATE (CONCPPT(nsegdim,Nsol))
      ELSE
        ALLOCATE (CONCQ(1,Nsol), CONCRUN(1,Nsol), CONCPPT(1,Nsol))
      END IF
C
C6------PRINT INFORMATION THAT WAS READ.
!      WRITE (IOUT, 9003) NSTRM, NSS, NSFRPAR, nparseg, DLEAK, CONST
!      IF ( ISFROPT.EQ.1 ) WRITE (IOUT, 9004)
!      IF ( ISFROPT.GE.2 ) WRITE (IOUT, 9005)
!      IF ( ISTCB1.GT.0 ) WRITE (IOUT, 9006) ISTCB1
!      IF ( ISTCB2.GT.0 ) WRITE (IOUT, 9007) ISTCB2
!      IF ( IRTFLG.GT.0 ) WRITE (IOUT, 9035)
! 9003 FORMAT (//, ' NUMBER OF STREAM REACHES IS', I5, //,
!     +        ' NUMBER OF STREAM SEGMENTS IS', I5, //,
!     +        ' NUMBER OF STREAM PARAMETERS IS', I5, //,
!     +        ' NUMBER OF STREAM SEGMENTS DEFINED USING PARAMETERS IS',
!     +        I5, //, ' MAXIMUM ERROR FOR STREAM LEAKAGE RATES IS',
!     +        1PE10.2, //, ' CONSTANT FOR MANNINGS EQUATION IS', E12.4,
!     +        ///)
! 9004 FORMAT (//, ' USING DATA INPUT MODIFIED FROM ORIGINAL SFR ',
!     +        'PROGRAM FOR FARM PACKAGE', /)
! 9005 FORMAT (//, ' OPTION FOR UNSATURATED FLOW BENEATH STREAMBEDS IS ',
!     +        'ACTIVE ', //)
! 9006 FORMAT (' FLOW TO AND FROM GROUND WATER FOR EACH STREAM REACH ',
!     +        'WILL BE SAVED ON UNIT', I3)
! 9007 FORMAT (' STREAM OUTPUT WILL BE WRITTEN TO FILE ON UNIT', I4)
! 9035 FORMAT (' TRANSIENT STREAMFLOW ROUTING IS ACTIVE ')
C
C7------CHECK FOR ERRORS.
      IF ( NSTRM.LE.0 .OR. NSS.LE.0 ) THEN
        WRITE (IOUT, 9008)
        In = 0
        NSS = 0
        NSTRM = 0
        RETURN
      END IF
      IF ( NSFRPAR.GT.0 .AND. nparseg.LE.0 ) THEN
        WRITE (IOUT, 9009)
        In = 0
        NSS = 0
        NSTRM = 0
        RETURN
      END IF
      IF ( IUZT.EQ.1 ) THEN
        IF ( NSTRAIL.LT.0 ) THEN
          WRITE (IOUT, 9010)
          NSTRAIL = ABS(NSTRAIL)
        END IF
        IF ( NSTRAIL.EQ.0 ) THEN
          WRITE (IOUT, 9011)
          IUZT = 0
        END IF
      END IF
      IF ( DLEAK.LE.0.0 ) THEN
        DLEAK = 0.00001
        WRITE (IOUT, 9012)
      END IF
 9008 FORMAT (//, ' NO STREAM REACHES (NSTRM) AND/OR SEGMENTS (NSS)--',
     +        //, ' SFR PACKAGE BEING TURNED OFF'///)
 9009 FORMAT (//, ' NO STREAM SEGMENTS DEFINED BY PARAMETERS--',
     +        'NSFRPAR GT ZERO AND NPARSEG LE ZERO', //,
     +        ' SFR PACKAGE BEING TURNED OFF'///)
 9010 FORMAT (//, ' NUMBER OF TRAILING WAVES IS LESS THAN ZERO',
     +        '--SETTING VALUE TO A POSITIVE VALUE'///)
 9011 FORMAT (//, ' VERTICAL FLOW THROUGH UNSATURATED ZONE IS ',
     +        'ACTIVE AND NUMBER OF TRAILING WAVES IS ZERO-- ',
     +        ' RESETTING UNSATURATED FLOW TO BE INACTIVE '///)
 9012 FORMAT (//, ' *** WARNING ***   DLEAK IS LESS THAN OR EQUAL',
     +        ' TO ZERO --- DLEAK ASSIGNED A VALUE OF 0.0001', ///)
C
      IF ( IUZT.EQ.1 ) THEN
C
C8------ALLOCATE SPACE FOR UNSATURATED FLOW.
        NUZST = NSTRM
        NSTOTRL = ISUZN*NSTRAIL*NSFRSETS
        NUMAVE = 21
        mstrmar = nstrmar
      ELSE
C
C9------ALLOCATE ONLY ONE ARRAY ELEMENT IF UNSATURATED FLOW IS INACTIVE.
        NUZST = 1
        NSTOTRL = 1
        NUMAVE = 1
        ISUZN = 1
        NSTRAIL = 1
        NSFRSETS = 1
        mstrmar = 1
      END IF
C
C ALLOCATE AND INITIALIZE ARRAYS
C
      ALLOCATE (THTS(NUZST), THTR(NUZST), THTI(NUZST), EPS(NUZST))
      THTS = 0.0D0
      THTR = 0.0D0
      THTI = 0.0D0
      EPS = 0.0D0
      ALLOCATE (UHC(NUZST))
      UHC = 0.0
      ALLOCATE (XSEC(16, nsegdim), QSTAGE(MAXPTS,nsegdim))
      XSEC = 0.0
      QSTAGE = 0.0
      ALLOCATE (NSEGCK(nssar), SGOTFLW(nssar), DVRSFLW(nssar))
      NSEGCK = 0
      SGOTFLW = 0.0
      DVRSFLW = 0.0
      ALLOCATE (SFRUZBD(10))
      SFRUZBD = 0.0
C
C10-----READ AND PRINT DATA FOR EACH STREAM REACH.
!IFACE
      ndash=50
      face=' '
      if(iface.eq.1) then
        face='   IFACE'
        ndash=ndash+8
      end if
C
C11-----READ AND WRITE DATA FOR EACH REACH ON BASIS OF ISFROPT.
      nseg = 0
      nreach = 0
!      IF ( Iunithuf.GT.0 ) THEN
!        IF ( ISFROPT.NE.3 .AND. ISFROPT.NE.5 ) THEN
!          WRITE (IOUT, 9034)
! 9034     FORMAT (//, ' ***ERROR***  HUF PACKAGE IS ACTIVE ',
!     +          'AND ISFROPT NOT 3 or 5 ',/,
!     +          ' PROGRAM IS STOPPING')
!          CALL USTOP(' ')
!        END IF
!      END IF

      DO ii = 1, NSTRM
        IF ( ISFROPT.EQ.0 ) THEN
!IFACE
          IF(iface.eq.1) THEN
            READ (In, *) krch, irch, jrch, jseg, ireach, STRM(1, ii),
     +                 ISTRM(6, ii)
          ELSE
            READ (In, *) krch, irch, jrch, jseg, ireach, STRM(1, ii)
          END IF
        ELSE IF ( ISFROPT.EQ.1 ) THEN
!IFACE
          IF (iface.eq.1) THEN
            READ (In, *) krch, irch, jrch, jseg, ireach, STRM(1, ii),
     +                 STRM(3, ii), STRM(2, ii), STRM(8, ii),
     +                 STRM(6, ii), ISTRM(6, ii)
          ELSE
            READ (In, *) krch, irch, jrch, jseg, ireach, STRM(1, ii),
     +                 STRM(3, ii), STRM(2, ii), STRM(8, ii),
     +                 STRM(6, ii)
          END IF
          STRM(4, ii) = STRM(3, ii) - STRM(8, ii)
          IF ( Iunitlpf.GT.0 ) THEN
            Ltyp = LAYTYP(krch)
          ELSEIF ( Iunitbcf.GT.0 ) THEN
            Ltyp = LAYCON(krch)
          ELSEIF ( Iunithuf.GT.0 ) THEN
            Ltyp = LTHUF(krch)
          END IF
!          IF ( Ltyp.GT.0  .AND. IBOUND(jrch,irch,krch).GT.0 ) THEN
!            IF ( STRM(4, ii).LT.BOTM(jrch,irch,krch) ) THEN
!              write(iout,*)
!              write(iout,*)'Streambed has lower altitude than ',
!     +                      'GW cell bottom. Model stopping '
!              write(iout,*)'Segment: ',jseg,' Reach: ',ireach
!              write(iout,*)'Layer: ',krch,' Row: ',irch,' Col: ',jrch
!              write(iout,*)'Streambed bottom altitude: ',STRM(4, ii)
!              write(iout,*)'Cell bottom altitude: ',BOTM(jrch,irch,krch)
!              CALL USTOP('')
!            END IF
!          END IF
          IF ( STRM(2, ii).LE.0.0 ) THEN
            WRITE (IOUT, 9017) jseg, ireach
            CALL USTOP(' ')
          END IF
        ELSE IF ( ISFROPT.EQ.2 ) THEN
!IFACE
          IF (iface.eq.1) THEN
            READ (In, *) krch, irch, jrch, jseg, ireach, STRM(1, ii),
     +                 STRM(3, ii), STRM(2, ii), STRM(8, ii),
     +                 STRM(6, ii), THTS(ii), THTI(ii), EPS(ii),
     +                 ISTRM(6, ii)
          ELSE
            READ (In, *) krch, irch, jrch, jseg, ireach, STRM(1, ii),
     +                 STRM(3, ii), STRM(2, ii), STRM(8, ii),
     +                 STRM(6, ii), THTS(ii), THTI(ii), EPS(ii)
          END IF
          STRM(4, ii) = STRM(3, ii) - STRM(8, ii)
          IF ( Iunitlpf.GT.0 ) THEN
            Ltyp = LAYTYP(krch)
          ELSEIF ( Iunitbcf.GT.0 ) THEN
            Ltyp = LAYCON(krch)
          ELSEIF ( Iunithuf.GT.0 ) THEN
            Ltyp = LTHUF(krch)
          END IF
!          IF ( Ltyp.GT.0  .AND. IBOUND(jrch,irch,krch).GT.0 ) THEN
!            IF ( STRM(4, ii).LT.BOTM(jrch,irch,krch) ) THEN
!              write(iout,*)
!              write(iout,*)'Streambed has lower altitude than ',
!     +                    'GW cell bottom. Model stopping '
!              write(iout,*)'Segment: ',jseg,' Reach: ',ireach
!              write(iout,*)'Layer: ',krch,' Row: ',irch,' Col: ',jrch
!              write(iout,*)'Streambed bottom altitude: ',STRM(4, ii)
!              write(iout,*)'Cell bottom altitude: ',BOTM(jrch,irch,krch)
!              CALL USTOP('')
!            END IF
!          END IF
          IF ( STRM(2, ii).LE.0.0 ) THEN
            WRITE (IOUT, 9017) jseg, ireach
            CALL USTOP(' ')
          END IF
        ELSE IF ( ISFROPT.EQ.3 ) THEN
!IFACE
          IF (iface.eq.1) THEN
            READ (In, *) krch, irch, jrch, jseg, ireach, STRM(1, ii),
     +                 STRM(3, ii), STRM(2, ii), STRM(8, ii),
     +                 STRM(6, ii), THTS(ii), THTI(ii), EPS(ii),
     +                 UHC(ii), ISTRM(6, ii)
          ELSE
            READ (In, *) krch, irch, jrch, jseg, ireach, STRM(1, ii),
     +                 STRM(3, ii), STRM(2, ii), STRM(8, ii),
     +                 STRM(6, ii), THTS(ii), THTI(ii), EPS(ii), UHC(ii)
          END IF
          STRM(4, ii) = STRM(3, ii) - STRM(8, ii)
          IF ( Iunitlpf.GT.0 ) THEN
            Ltyp = LAYTYP(krch)
          ELSEIF ( Iunitbcf.GT.0 ) THEN
            Ltyp = LAYCON(krch)
          ELSEIF ( Iunithuf.GT.0 ) THEN
            Ltyp = LTHUF(krch)
          END IF
!          IF ( Ltyp.GT.0 .AND. IBOUND(jrch,irch,krch).GT.0 ) THEN
!            IF ( STRM(4, ii).LT.BOTM(jrch,irch,krch) ) THEN
!              write(iout,*)
!              write(iout,*)'Streambed has lower altitude than ',
!     +                      'GW cell bottom. Model stopping '
!              write(iout,*)'Segment: ',jseg,' Reach: ',ireach
!              write(iout,*)'Layer: ',krch,' Row: ',irch,' Col: ',jrch
!              write(iout,*)'Streambed bottom altitude: ',STRM(4, ii)
!              write(iout,*)'Cell bottom altitude: ',BOTM(jrch,irch,krch)
!              CALL USTOP('')
!            END IF
!          END IF
          IF ( STRM(2, ii).LE.0.0 ) THEN
            WRITE (IOUT, 9017) jseg, ireach
            CALL USTOP(' ')
          END IF
        ELSE IF ( ISFROPT.EQ.4 .OR. ISFROPT.EQ.5 ) THEN
!IFACE
          IF (iface.eq.1) THEN
            READ (In, *) krch, irch, jrch, jseg, ireach, STRM(1, ii),
     +                 ISTRM(6, ii)
          ELSE
            READ (In, *) krch, irch, jrch, jseg, ireach, STRM(1, ii)
          END IF
        END IF
        IF ( IUZT.EQ.1 ) THEN
          IF ( IBOUND(jrch, irch, krch).LE.0 )
     +      WRITE (IOUT, 9018) ireach, jseg
        END IF
 9017   FORMAT (//, ' ***ERROR***  SLOPE IS SPECIFIED LESS THAN OR ',
     +          'EQUAL TO ZERO FOR SEGMENT', I8, ' REACH', I8, /,
     +          ' PROGRAM IS STOPPING')
 9018   FORMAT (5X, '**WARNING** CELL BENEATH STREAM REACH IS INACTIVE',
     +          /, 5X, 'CELL BELOW MUST HAVE THE SAME SPECIFIC YIELD',
     +          /, 5X, 'AND THE TOP ELEVATION OF ACTIVE CELL MUST ',
     +          'EQUAL BOTTOM OF INACTIVE CELL', /, 5X,
     +          'INACTIVE CELL IS BELOW STREAM REACH AND SEGMENT:', 2I5)
C
C12-----CALCULATE RESIDUAL WATER CONTENT FROM SATURATED WATER CONTENT
C        AND SPECIFIC YIELD WHEN UNSATURATED FLOW IS ACTIVE.
        IF ( ABS(ITRSS).EQ.1 ) THEN
          IF ( ISFROPT.EQ.2 .OR. ISFROPT.EQ.3 ) THEN
            IF ( Iunitlpf.GT.0 ) THEN
              THTR(ii) = THTS(ii) - SC2LPF(jrch, irch, krch)
     +                   /(DELR(jrch)*DELC(irch))
            ELSE IF ( Iunitbcf.GT.0 ) THEN
              IF ( LAYCON(krch).LT.2 ) THEN
                kkrch = 0
                DO k = 1, krch
                IF(LAYCON(K).EQ.3 .OR. LAYCON(K).EQ.2)kkrch = kkrch + 1
                END DO
                THTR(ii) = THTS(ii) - SC1(jrch, irch, krch)
     +                   /(DELR(jrch)*DELC(irch))
              ELSE
                THTR(ii) = THTS(ii) - SC2(jrch, irch, krch)
     +                   /(DELR(jrch)*DELC(irch))
              END IF
            ELSE IF ( Iunithuf.GT.0 ) THEN
              THTR(ii) = THTS(ii) - SC2HUF(jrch, irch)
            END IF
          END IF
        ELSEIF ( ISFROPT.EQ.2 .OR. ISFROPT.EQ.3 ) THEN
          THTR(ii) = 0.0
        END IF
!        IF ( ISFROPT.EQ.0 ) THEN
!        ELSE IF ( ISFROPT.EQ.1 ) THEN
!        ELSE IF ( ISFROPT.EQ.2 ) THEN
!        ELSE IF ( ISFROPT.EQ.3 ) THEN
!        ELSE IF ( ISFROPT.EQ.4 .OR. ISFROPT.EQ.5 ) THEN
!        END IF
! 9019   FORMAT (2X, I5, 2I7, I8, I9, 3X, 1PE11.4)
! 9020   FORMAT (2X, I6, 2I7, I8, I9, 3X, 1PE11.4, 2X, 1PE11.4, 2X,
!     +          1PE11.4, 2X, 1PE11.4, 2X, 1PE11.4)
! 9021   FORMAT (3(1X, I5), 1X, I5, 3X, I5, 1X, 5(1X, 1PE11.4),
!     +          3(1X, 0PE11.4), 1(1X, 1PE11.4))
! 9022   FORMAT (3(1X, I5), 1X, I5, 3X, I5, 1X, 5(1X, 1PE11.4),
!     +          3(1X, 0PE11.4), 2(1X, 1PE11.4))
C
C13-----CHECK RANGE AND ORDER FOR SEGMENTS AND REACHES.
        IF ( jseg.LE.0 .OR. jseg.GT.NSS ) THEN
          WRITE (IOUT, 9023)
          CALL USTOP(' ')
        END IF
        IF ( jseg.NE.nseg ) THEN
          nseg = nseg + 1
          nreach = 0
          IF ( jseg.NE.nseg ) THEN
            WRITE (IOUT, 9024)
            CALL USTOP(' ')
          END IF
        END IF
        nreach = nreach + 1
        IF ( ireach.NE.nreach ) THEN
          WRITE (IOUT, 9025)
          CALL USTOP(' ')
        END IF
 9023   FORMAT (' SEGMENT MUST BE GREATER THAN 0 AND LESS THAN NSS')
 9024   FORMAT (' SEGMENTS MUST BE IN ORDER FROM 1 THROUGH NSS')
 9025   FORMAT (' EACH SEGMENT MUST START WITH REACH 1, AND', /,
     +          ' REACHES MUST BE NUMBERED CONSECUTIVELY')
        ISTRM(1, ii) = krch
        ISTRM(2, ii) = irch
        ISTRM(3, ii) = jrch
        ISTRM(4, ii) = jseg
        ISTRM(5, ii) = ireach
	  WRITE(IOUT, *) 'KRCH IRCH JRCH ISEG IREACH:'
	  WRITE(IOUT, *) (ISTRM(I,ii), I=1,5)
	  WRITE(IOUT, *) 'RCHLEN, STRTOP, SLOPE, STRTHICK, STRHC1:'
	  WRITE(IOUT, *) STRM(1, ii), STRM(3, ii), STRM(2, ii),
	1    STRM(8, ii), STRM(6, ii)
	  IF ((ISFROPT.EQ.2).OR.(ISFROPT.EQ.3)) THEN
	    WRITE(IOUT, *) 'THTS,THTI,EPS:'
	    WRITE(IOUT, *) THTS(ii), THTI(ii), EPS(ii)
	  ENDIF
	  IF (ISFROPT.EQ.3) THEN
	    WRITE(IOUT, *) 'UHC:'
	    WRITE(IOUT, *) UHC(ii)
	  ENDIF


        SEG(1, ISTRM(4, ii)) = SEG(1, ISTRM(4, ii)) + STRM(1, ii)
C       Number of reaches in segment added to ISEG
        ISEG(4, jseg) = ireach
      END DO
C
C14-----READ SEGMENT INFORMATION FOR FIRST STRESS PERIOD.
      IF ( NSFRPAR.EQ.0 ) THEN
        READ (In, *) ITMP, IRDFLG, IPTFLG
        WRITE (IOUT, *) 'ITMP, IRDFLG, IPTFLG:'
        WRITE (IOUT, *) ITMP, IRDFLG, IPTFLG
        NP = 0
        nlst = NSS
        lb = 1
        ichk = 1
        CALL SGWF2SFR7RDSEG(nlst, lb, In, Iunitgwt, Iunituzf, NSEGCK,
     +                      NSS, ichk, 1, Nsol)
      END IF
C
C15-----COMPUTE UNSATURATED VARIABLE WHEN SPECIFIED BY SEGMENT.
      IF ( IUZT.EQ.1 ) THEN
        irch = 1
        ksfropt = 0
        DO nseg = 1, NSS
          icalc = ISEG(1, nseg)
          seglen = SEG(1, nseg)
          sumlen = 0.0
          IF ( icalc.EQ.1 .OR. icalc.EQ.2 ) THEN
            IF ( ISFROPT.EQ.4 .OR. ISFROPT.EQ.5 ) THEN
              ksfropt = 1
              thsslpe = (SEG(18, nseg)-SEG(22, nseg))/seglen
              thislpe = (SEG(19, nseg)-SEG(23, nseg))/seglen
              epsslpe = (SEG(20, nseg)-SEG(24, nseg))/seglen
              IF ( ISFROPT.EQ.5 )
     +             uhcslpe = (SEG(21, nseg)-SEG(25, nseg))/seglen
            END IF
          END IF
          DO ii = 1, ISEG(4, nseg)
            IF ( icalc.EQ.1 .OR. icalc.EQ.2 ) THEN
              krck = ISTRM(1, irch)
              irck = ISTRM(2, irch)
              jrck = ISTRM(3, irch)
              rchlen = STRM(1, irch)
              dist = sumlen + (0.5*rchlen)
              IF ( ksfropt.EQ.1 ) THEN
                THTS(irch) = SEG(18, nseg) - (thsslpe*dist)
                THTI(irch) = SEG(19, nseg) - (thislpe*dist)
                EPS(irch) = SEG(20, nseg) - (epsslpe*dist)
                IF ( ISFROPT.EQ.5 ) UHC(irch) = SEG(21, nseg)
     +               - (uhcslpe*dist)
              END IF
C
C16-----CALCULATE RESIDUAL WATER CONTENT FROM SATURATED WATER CONTENT
C         AND SPECIFIC YIELD WHEN UNSATURATED FLOW IS ACTIVE.
! RGN 5/8/09 Fixed calculation of THTR to include HUF
              IF ( ITRSS.EQ.1 ) THEN
                IF ( ISFROPT.EQ.4 .OR. ISFROPT.EQ.5 ) THEN
                  IF ( Iunitlpf.GT.0 ) THEN
                    THTR(irch) = THTS(irch) - SC2LPF(jrck, irck, krck)
     +                         /(DELR(jrck)*DELC(irck))
                  ELSE IF ( Iunitbcf.GT.0 ) THEN
                    IF( LAYCON(krck).EQ.0 ) THEN
                      THTR(irch) = THTS(irch) - SC1(jrck, irck, krck)
     +                         /(DELR(jrck)*DELC(irck))
                    ELSE
                      kkrch = 0
                      DO k = 1, krch
                        IF(LAYCON(K).EQ.3 .OR. LAYCON(K).EQ.2)
     +                   kkrch = kkrch + 1
                      END DO
                      THTR(irch) = THTS(irch) - SC2(jrck, irck, kkrch)
     +                         /(DELR(jrck)*DELC(irck))
                    END IF
                  ELSE IF( Iunithuf.GT.0 ) THEN
                    THTR(irch) = THTS(irch) - SC2HUF(jrck, irck)
                  END IF
                END IF
              ELSEIF ( ISFROPT.EQ.4 .OR. ISFROPT.EQ.5 ) THEN
                THTR(irch) = 0.0
              END IF
C
C17-----CHECK THAT RESIDUAL WATER CONTENT IS LESS THAN
C         SATURATED WATER CONTENT.
              IF ( IUZT.EQ.1 ) THEN
                IF ( THTR(irch).GE.THTS(irch) ) THEN
                  WRITE (IOUT, 9026)
                  CALL USTOP(' ')
                END IF
                IF ( THTI(irch).GT.THTS(irch) ) THEN
                  WRITE (IOUT, 9027)
                  CALL USTOP(' ')
                END IF
Cdep  Added check that THTI is greater than THTR.
                IF ( THTI(irch).LT.THTR(irch) ) THEN
!                  WRITE (IOUT, 9028)ISTRM(4,irch), ISTRM(5,irch),
!     +                              THTR(irch)
                  THTI(irch) = THTR(irch)
                END IF
              END IF
              sumlen = sumlen + rchlen
            END IF
            irch = irch + 1
          END DO
        END DO
      END IF
 9026 FORMAT (' RESIDUAL WATER CONTENT IS EQUAL OR GREATER THAN ',
     +        'SATURATED WATER CONTENT. CHECK INPUT DATA FOR SPECIFIC',
     +        ' YIELD AND SATURATED WATER CONTENT')
 9027 FORMAT (' INITIAL WATER CONTENT IS GREATER THAN SATURATED ',
     +        'WATER CONTENT. CHECK INPUT DATA')
 9028 FORMAT (' INITIAL WATER CONTENT IS LESS THAN RESIDUAL ',
     +        'WATER CONTENT FOR STREAM SEGMENT: ',I5,' REACH: ',I5,
     +        ' INITIAL WATER CONTENT RESET TO RESIDUAL OF ',E12.5)
C
C18-----CHECK IF STREAM REACH IS IN ACTIVE CELL.
      kkptflg = 0
      DO ichk = 1, NSTRM
        krck = ISTRM(1, ichk)
        irck = ISTRM(2, ichk)
        jrck = ISTRM(3, ichk)
        jsegck = ISTRM(4, ichk)
        ireachck = ISTRM(5, ichk)
        IF ( IBOUND(jrck, irck, krck).EQ.0 ) THEN
          kkptflg = kkptflg + 1
!          IF ( kkptflg.EQ.1 ) WRITE (IOUT, 9029) jsegck, ireachck,
!     +                              IBOUND(jrck, irck, krck), krck,
!     +                              irck, jrck
!        ELSE IF ( IBOUND(jrck, irck, krck).LT.0 ) THEN
!          WRITE (IOUT, 9030) jsegck, ireachck, IBOUND(jrck, irck, krck),
!     +                       krck, irck, jrck
        END IF
      END DO
!      IF ( kkptflg.EQ.1 ) THEN
!        WRITE (IOUT, 9031)
!      ELSE IF ( kkptflg.GT.1 ) THEN
!        WRITE (IOUT, 9032) kkptflg
!      END IF
C
 9029 FORMAT (/, ' *** WARNING *** FIRST OCCURRENCE WHERE A ',
     +        'STREAM REACH IS ASSIGNED TO AN INACTIVE CELL IS SEGMENT',
     +        I5, ' REACH NO.', I5, /, '  IBOUND ARRAY VALUE IS', I5,
     +        ' AT LAYER', I5, '; ROW', I5, '; COLUMN', I5, '.')
! 9030 FORMAT (/, ' *** WARNING *** STREAM SEGMENT', I5, ' REACH NO.',
!     +        I5, ' IS CONNECTED TO A CONSTANT HEAD CELL.'/,
!     +        ' IBOUND ARRAY VALUE IS', I5, ' AT ', 'LAYER', I5,
!     +        '; ROW', I5, '; COLUMN', I5, '.', /,
!     +        ' NO STREAM LEAKAGE WILL BE ALLOWED-- SUGGEST ',
!     +        'REMOVING STREAM REACH FROM CELL OR CHANGE CELL ',
!     +        'TO VARIABLE HEAD.', /)
 9031 FORMAT (/, ' *** WARNING *** ONLY 1 STREAM REACH WAS ',
     +        'ASSIGNED TO A CELL WHERE THE IBOUND ARRAY WAS ZERO.', /,
     +        ' PROGRAM SEARCHES FOR UPPERMOST ACTIVE CELL IN VERTICAL',
     +        ' COLUMN,IF ALL CELLS ARE INACTIVE, STREAM LEAKAGE WILL',
     +        ' NOT BE ALLOWED. ', /)
 9032 FORMAT (/, ' *** WARNING *** A TOTAL OF', I6, 'STREAM REACHES ',
     +        'WERE ASSIGNED TO CELLS WHERE THE IBOUND ARRAY WAS ZERO.',
     +        /, ' PROGRAM SEARCHES FOR UPPERMOST ACTIVE CELL IN',
     +        ' VERTICAL COLUMN FOR ALL OCCURRENCES.', /,
     +        ' IF ALL CELLS IN A VERTICAL COLUMN ARE INACTIVE,',
     +        ' STREAM LEAKAGE WILL NOT BE ALLOWED FOR ASSOCIATED',
     +        ' STREAM REACH. ', /)
C
C19-----READ PARAMETER DEFINITIONS.
      IF ( NSFRPAR.GT.0 ) THEN
        lstsum = NSS + 1
        DO ii = 1, NSFRPAR
          lstbeg = lstsum
          CALL UPARLSTRP(lstsum, nsegdim, In, IOUT, ip, 'SFR', 'SFR',
     +                   iterp, numinst)
          nlst = lstsum - lstbeg
          IF ( numinst.GT.1 ) nlst = nlst/numinst
C
C20-----ASSIGN STARTING INDEX FOR READING INSTANCES.
          IF ( numinst.EQ.0 ) THEN
            ib = 0
          ELSE
            ib = 1
          END IF
C
C21-----READ LIST(S) OF CELLS, PRECEDED BY INSTANCE NAME IF NUMINST>0.
Cdep  Revised to change ib loop counter
          lb = lstbeg
          DO i = ib, numinst
            IF ( i.GT.0 ) CALL UINSRP(i, In, IOUT, ip, iterp)
            ichk = 0
            CALL SGWF2SFR7RDSEG(nlst, lb, In, Iunitgwt, Iunituzf, idum,
     +                          1, ichk, 1, Nsol)
!            CALL SGWF2SFR7PRSEG(nlst, lb, Iunitgwt, 1, Nsol, Iouts)
            lb = lb + nlst
          END DO
        END DO
      END IF
C
!      WRITE (IOUT, 9033)
! 9033 FORMAT (//)
C
C22-----INITIALIZE VARIABLES AND LISTS FOR UNSATURATED FLOW BENEATH STREAM.
C         NWAVS INITIALLY SET TO 1.
      ALLOCATE (FOLDFLBT(mstrmar))
      Nfoldflbt = mstrmar
      FOLDFLBT = 0.0D0
      ALLOCATE (UZFLWT(ISUZN,NUZST), UZSTOR(ISUZN,NUZST))
      UZFLWT = 0.0D0
      UZSTOR = 0.0D0
      ALLOCATE (UZWDTH(ISUZN,NUZST), UZSEEP(ISUZN,NUZST))
      UZWDTH = 0.0D0
      UZSEEP = 0.0D0
      ALLOCATE (DELSTOR(ISUZN,NUZST), UZOLSFLX(ISUZN,NUZST))
      DELSTOR = 0.0D0
      UZOLSFLX = 0.0D0
      ALLOCATE (NWAVST(ISUZN,NUZST))
      NWAVST = 1
      ALLOCATE (UZDPIT(NSTOTRL,NUZST), UZDPST(NSTOTRL,NUZST))
      UZDPIT = 0.0D0
      UZDPST = 0.0D0
      ALLOCATE (UZTHIT(NSTOTRL,NUZST), UZTHST(NSTOTRL,NUZST))
      UZTHIT = 0.0D0
      UZTHST = 0.0D0
      ALLOCATE (UZSPIT(NSTOTRL,NUZST), UZSPST(NSTOTRL,NUZST))
      UZSPIT = 0.0D0
      UZSPST = 0.0D0
      ALLOCATE (UZFLIT(NSTOTRL,NUZST), UZFLST(NSTOTRL,NUZST))
      UZFLIT = 0.0D0
      UZFLST = 0.0D0
      ALLOCATE (LTRLIT(NSTOTRL,NUZST), LTRLST(NSTOTRL,NUZST))
      LTRLIT = 0
      LTRLST = 0
      ALLOCATE (ITRLIT(NSTOTRL,NUZST), ITRLST(NSTOTRL,NUZST))
      ITRLIT = 0
      ITRLST = 0
      ALLOCATE (ITRLSTH(NSTOTRL))
      ITRLSTH = 0
      ALLOCATE (WETPER(ISUZN,NUZST))
      WETPER = 0.0D0
      ALLOCATE (AVDPT(NUMAVE,NUZST), AVWAT(NUMAVE,NUZST))
      AVDPT = 0.0
      AVWAT = 0.0
      ALLOCATE (WAT1(NUMAVE,NUZST))
      WAT1 = 0.0
C
C22B-----INITIALIZE VARIABLES FOR STREAM DEPTH, LEAKAGE, AND
C         PREVIOUS HEAD BENEATH STREAM.
      ALLOCATE (SUMLEAK(nstrmar))
      SUMLEAK = 0.0D0
      ALLOCATE (SUMRCH(nstrmar))
      SUMRCH = 0.0D0
      ALLOCATE (HLDSFR(nstrmar))
      HLDSFR = 0.0D0
C     ------------------------------------------------------------------
C
!      IF ( Iunitlpf.GT.0 .OR. Iunithuf.GT.0 ) THEN
!      IF ( ISFROPT.EQ.2.OR.ISFROPT.EQ.4 )
!     +     CALL SGWF2SFR7UHC(Iunitlpf)
!      END IF
C
C23-----SAVE POINTERS FOR GRID AND RETURN.
      CALL SGWF2SFR7PSV(Igrid)
      RETURN
      END SUBROUTINE GWF2SFR7AR
C
C-------SUBROUTINE SGWF2SFR7UHC
!      SUBROUTINE SGWF2SFR7UHC(Iunitlpf, Iunithuf, Iunitupw)
C     ******************************************************************
C     SETS UNSATURATED VERTICAL HYDRAULIC CONDUCTIVITY TO VERTICAL
C     HYDRAULIC CONDUCTIVITY IN THE LAYER-PROPERTY FLOW PACKAGE.
C     VERSION  7.1: JUNE 29, 2006
C     ******************************************************************
C
C-------SUBROUTINE GWF2SFR7RP
      SUBROUTINE GWF2SFR7RP(In, Iunitgwt, Iunitlak, Kkper, Kkstp, Nsol,
     +                      Iouts, Iunitbcf, Iunitlpf, Iunithuf,
     +                      Iunitupw, Iunituzf, Igrid)
C     ******************************************************************
C     READ STREAM DATA FOR STRESS PERIOD
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     Compute three new tables for lake outflow
C     ******************************************************************
      USE GWFSFRMODULE
      USE GLOBAL,       ONLY: IOUT, ISSFLG, IBOUND, BOTM, HNEW, NLAY,
     +                        LAYHDT
      USE PARAMMODULE,  ONLY: MXPAR, PARTYP, IACTIVE, IPLOC
      USE ICHKSTRBOT_MODULE
      USE GWFLPFMODULE, ONLY: LAYTYP
      USE GWFBCFMODULE, ONLY: LAYCON
      USE GWFHUFMODULE, ONLY: LTHUF
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
      type (check_bot) :: uzfrp_check
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER Kkper, Kkstp, In, Iunitgwt, Iunitlak, Nsol, Iouts, Igrid
      Integer Iunitbcf, Iunitlpf, Iunithuf, Iunitupw, Iunituzf
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      DOUBLE PRECISION h, sbot
      REAL avdpth, avhc, avthk, bottom, dist, dpslpe, dpth1, dpth2,
     +     dpthlw, dndiff, eldn, elslpe, etsw, flw1, flw2, flwlw,
     +     hcslpe, pptsw,rchlen, rough, roughbnk, roughch, runoff,
     +     seglen, strlen, sumlen, thkslpe, top, wdslpe, wdth1, wdth2,
     +     wdthlw, width, updiff, zero
      INTEGER i, ic, icalc, ichk, icp, iflginit, ii, ik, il, ilay, ip,
     +        ipt, ir, irch, irp, isoptflg, iss, istep, istsg, iwvcnt,
     +        jj, jk, k5, k6, k7, kk, ksfropt, kss, ktot, l, lstbeg,
     +        nseg, nstrpts,krck,irck,jrck,ireachck, j, numval, iunit,
     +        Ltyp,ierr,IFLG
C     ------------------------------------------------------------------
C
C-------SET POINTERS FOR CURRENT GRID.
      CALL SGWF2SFR7PNT(Igrid)
      IERR = 0
      IFLG = 0
	WRITE(IOUT,*)'SFR:'
C
C1------READ ITMP FLAG TO REUSE NON-PARAMETER DATA, 2 PRINTING FLAGS,
C         AND NUMBER OF PARAMETERS BEING USED IN CURRENT STRESS PERIOD.
      iss = ISSFLG(Kkper)
      zero = 1.0E-7
Cdep added NSFRPAR to IF statement
      IF ( Kkper.GT.1 ) THEN
        IF ( NSFRPAR.EQ.0 ) THEN
          READ (In, *) ITMP, IRDFLG, IPTFLG
          WRITE (IOUT, *) 'ITMP, IRDFLG, IPTFLG:'
          WRITE (IOUT, *) ITMP, IRDFLG, IPTFLG
          NP = 0
        ELSE
          READ (In, *) ITMP, IRDFLG, IPTFLG, NP
          WRITE (IOUT, *) 'ITMP, IRDFLG, IPTFLG, NP:'
          WRITE (IOUT, *) ITMP, IRDFLG, IPTFLG, NP
        END IF
      ELSE IF ( NSFRPAR.GT.0 ) THEN
        READ (In, *) ITMP, IRDFLG, IPTFLG, NP
          WRITE (IOUT, *) 'ITMP, IRDFLG, IPTFLG, NP:'
          WRITE (IOUT, *) ITMP, IRDFLG, IPTFLG, NP
      END IF
C
C2------CHECK FOR TOO MANY SEGMENTS.
      IF ( ITMP.GT.NSS ) THEN
        WRITE (IOUT, 9001)
        CALL USTOP(' ')
      END IF
C
C3------REUSE NON-PARAMETER DATA FROM LAST STRESS PERIOD IF ITMP<0.
      IF ( ITMP.GE.0 ) THEN
C
C4------NOT REUSING DATA -- INITIALIZE NSEGCK LIST TO ZERO FOR ALL
C         SEGMENTS. Moved NSEGCK below ELSE IF 6/9/2005 dep
        IF ( Kkper.GT.1 ) THEN
          DO kss = 1, NSS
            NSEGCK(kss) = 0
          END DO
        END IF
      ELSE IF ( Kkper.EQ.1 ) THEN
        WRITE (IOUT, 9002)
        CALL USTOP(' ')
      ELSE IF ( NSFRPAR.EQ.0 .AND. IUZT.EQ.0 ) THEN
        WRITE (IOUT, 9003)
        RETURN
      ELSE IF ( NSFRPAR.NE.0 ) THEN
C
C5------INITIALIZE NSEGCK TO 0 FOR SEGMENTS THAT ARE DEFINED BY
C         CURRENTLY USED PARAMETERS.
        WRITE (IOUT, 9003)
        DO ip = 1, MXPAR
          IF ( PARTYP(ip).EQ.'SFR' .AND. IACTIVE(ip).GT.0 ) THEN
            DO ic = IPLOC(1, ip), IPLOC(2, ip)
              NSEGCK(ISEG(3, ic)) = 0
            END DO
          END IF
        END DO
      END IF
 9001 FORMAT (/, ' CANNOT SPECIFY MORE THAN NSS STREAM SEGMENTS')
 9002 FORMAT (//, '  ***  STREAM SEGMENTS MUST BE DEFINED FOR ',
     +        'FIRST STRESS PERIOD; CODE STOPPING ***')
 9003 FORMAT (/, ' REUSING STREAM SEGMENT DATA FROM LAST STRESS PERIOD')
C
C6------READ NON-PARAMETER STREAM SEGMENT DATA.
      IF ( ITMP.GT.0 ) THEN
        lstbeg = 1
        ichk = 1
        IF ( ISFROPT.GT.0 ) THEN
          IF ( Kkper.GT.1 ) CALL SGWF2SFR7RDSEG(ITMP, lstbeg, In,
     +                                        Iunitgwt, Iunituzf,
     +                                        NSEGCK, NSS, ichk, Kkper,
     +                                        Nsol)
Crgn 10/16/06 fixed logic for calls to RDSEG
        ELSEIF( NSFRPAR.EQ.0 ) THEN
          IF ( Kkper.GT.1 )CALL SGWF2SFR7RDSEG(ITMP, lstbeg, In,
     +                        Iunitgwt, Iunituzf, NSEGCK, NSS,
     +                        ichk, Kkper, Nsol)
        ELSEIF( NSFRPAR.GT.0 ) THEN
          CALL SGWF2SFR7RDSEG(ITMP, lstbeg, In,
     +                        Iunitgwt, Iunituzf, NSEGCK, NSS,
     +                        ichk, Kkper, Nsol)
        END IF
      END IF
C
C7------DEACTIVATE ANY PREVIOUSLY USED STREAM PARAMETERS, AND
C         ACTIVATE PARAMETERS BEING USED IN CURRENT STRESS PERIOD.
      IF ( NSFRPAR.NE.0 ) THEN
        CALL PRESET('SFR')
        DO jj = 1, NP
          CALL SGWF2SFR7PARMOV(In, Iunitgwt, Nsol)
        END DO
      END IF
C
C8------CHECK FOR ERRORS IN SEGMENT DATA.
      IF ( ITMP.GT.0 .OR. NSFRPAR.NE.0 ) THEN
        DO nseg = 1, NSS
          IF ( ISFROPT.EQ.0 ) THEN
            IF ( NSEGCK(nseg).LE.0 .AND. Kkper.EQ.1 ) THEN
!              WRITE (IOUT, 9004) nseg
            ELSE IF ( NSEGCK(nseg).GT.1 ) THEN
              WRITE (IOUT, 9005) nseg, NSEGCK(nseg)
              CALL USTOP(' ')
            END IF
          END IF
C
C9------READ DATA ACCORDING TO VARIABLE ISFROPT.
          isoptflg = 0
          IF ( ISFROPT.EQ.1 .OR. ISFROPT.EQ.2 .OR. ISFROPT.EQ.3 )
     +         isoptflg = 1
          IF ( isoptflg.EQ.0 .AND. SEG(8,nseg).LE.SEG(13,nseg) ) THEN
 !           WRITE (IOUT, 9006) nseg
            IF ( ISEG(1, nseg).EQ.1 .OR. ISEG(1, nseg).EQ.2 ) THEN
              WRITE (IOUT, 9007) nseg, ISEG(1, nseg)
              CALL USTOP(' ')
            END IF
          END IF
          IF ( IDIVAR(2, nseg).GT.0 ) THEN
 !           WRITE (IOUT, 9008) nseg
            IDIVAR(2, nseg) = 0
          ELSE IF ( IDIVAR(2, nseg).LT.-3 ) THEN
 !           WRITE (IOUT, 9009) nseg
            IDIVAR(2, nseg) = 0
          ELSE IF ( IDIVAR(2, nseg).EQ.-2 ) THEN
            IF ( SEG(2, nseg).LT.0.0 .OR. SEG(2, nseg).GT.1.0 ) THEN
!              WRITE (IOUT, 9010) nseg
              SEG(2, nseg) = 0.0
            END IF
          END IF
        END DO
 9004   FORMAT (/, 5X, '*** WARNING ***  INPUT DATA FOR SEGMENT', I7,
     +          ' WERE NOT DEFINED')
 9005   FORMAT (/, 5X, '*** ERROR ***  DATA FOR SEGMENT', I6,
     +          ' WERE DEFINED', I3, ' TIMES (INSTEAD OF ONCE)')
 9006   FORMAT (/, 5X, '*** WARNING *** UPSTREAM ELEVATION IS ',
     +          'EQUAL TO OR LOWER THAN DOWNSTREAM ELEVATION FOR ',
     +          'SEGMENT No. ', I6)
 9007   FORMAT (/, 5X, '*** ERROR ***  ',
     +          'SLOPE IS ZERO OR NEGATIVE FOR SEGMENT No.', I5,
     +          '   SLOPE MUST BE POSITIVE WHEN ICALC IS', I3)
 9008   FORMAT (/, 5X, '*** WARNING *** IPRIOR > 0 FOR NSEG =', I7, /,
     +          10X, 'THIS OPTION NOT YET AVAILABLE; CODE WILL ',
     +          'ASSUME IPRIOR = 0', /)
 9009   FORMAT (/, 5X, '*** WARNING *** IPRIOR < -3 FOR NSEG =', I7, /,
     +          10X, 'THIS VALUE IS OUT OF RANGE; CODE WILL ',
     +          'ASSUME IPRIOR = 0', /)
 9010   FORMAT (/, 5X, '*** WARNING *** IPRIOR = -2 FOR NSEG =', I7,
     +          ' & FLOW VALUE IS OUT OF RANGE (.0 - 1.);', /, 10X,
     +          'ASSUME NO DIVERSION OF FLOW', /)
C
C10-----PLACE STREAM SEGMENT IDENTITY NUMBERS IN ISEG ARRAY.
C         5 ASSIGNED TO SEGMENTS NOT RECEIVING TRIBUTARY FLOW.
C         6 ASSINGED TO SEGMENTS THAT DIVERT FLOW.
C         7 ASSIGNED TO SEGMENTS RECEIVING TRIBUTARY FLOW.
        k5 = 0
        k6 = 0
        k7 = 0
        DO nseg = 1, NSS
C
C11-----IDENTIFY SEGMENTS THAT DIVERT FLOW.
          IF ( IDIVAR(1, nseg).NE.0 ) THEN
            ISEG(3, nseg) = 6
            k6 = k6 + 1
C
C12-----IDENTIFY SEGMENTS THAT DO NOT DIVERT FLOW.
          ELSE
            jj = 0
C
C13-----IDENTIFY SEGMENTS THAT RECEIVE TRIBUTARY FLOW.
            DO ii = 1, NSS
              IF ( IOTSG(ii).EQ.nseg ) jj = 1
            END DO
C
C14-----IDENTIFY SEGMENTS THAT DO NOT RECEIVE TRIBUTARY FLOW.
            IF ( jj.EQ.0 ) THEN
              ISEG(3, nseg) = 5
              k5 = k5 + 1
            ELSE
              ISEG(3, nseg) = 7
              k7 = k7 + 1
              IF ( jj.NE.1 ) WRITE (IOUT, 9011) nseg, jj
            END IF
          END IF
        END DO
C
C15-----TALLY DIFFERENT STREAM SEGMENT TYPES.
        ktot = k5 + k6 + k7
!        WRITE (IOUT, 9012) k5, k6, k7
C
C16-----PRINT WARNING IF TALLIED SEGMENTS LESS THAN NSS.
        IF ( ktot.NE.NSS ) THEN
          WRITE (IOUT, 9013) ktot, NSS
          CALL USTOP(' ')
        END IF
 9011   FORMAT (//, 5X, '*** WARNING *** ERROR WHILE ',
     +          'CLASSIFYING SEGMENTS:   NSEG =', I6, 4X, 'JJ =', I6,//)
 9012   FORMAT (///1X, 'CLASSIFICATION & COUNT OF STREAM SEGMENTS ',
     +          'BASED ON SOURCE OF INFLOW:', //, 16X,
     +          'HEADWATER     DIVERSION     RECEIVES TRIBUTARY FLOW', /
     +          16X, '---------     ---------    ',
     +          ' -----------------------', /, 16X, I6, I15, I16, /)
 9013   FORMAT (/, 5X, '*** WARNING ***  INTERNAL ERROR SUMMING ',
     +          'TYPES OF STREAM SEGMENTS:  NSEG =', I6, 5X, 'JJ =',
     +          I6//)
C
C17-----PRINT INPUT DATA IF IRDFLG IS ZERO.
C         SKIP IF INPUT READ BY REACHES (ISFROPT = 1, 3, OR 5)
!        IF ( IRDFLG.LE.0 ) CALL SGWF2SFR7PRSEG(NSS, 1, Iunitgwt, Kkper,
!     +                                         Nsol, Iouts)
C
C18-----COMPUTE STREAM REACH VARIABLES.
        irch = 1
        ksfropt = 0
        DO nseg = 1, NSS
          ireachck = ISTRM(5, irch)
          icalc = ISEG(1, nseg)
          seglen = SEG(1, nseg)
          runoff = SEG(3, nseg)
          etsw = SEG(4, nseg)
          pptsw = SEG(5, nseg)
          sumlen = 0.0
C
C19-----COMPUTE VARIABLES NEEDED FOR STREAM LEAKAGE.
          IF ( icalc.EQ.0 .OR. icalc.EQ.1 ) THEN
            wdslpe = (SEG(9, nseg)-SEG(14, nseg))/seglen
            IF ( icalc.EQ.0 ) dpslpe = (SEG(10, nseg)-SEG(15, nseg))
     +                                 /seglen
          END IF
          IF ( ISFROPT.EQ.0 .OR. ISFROPT.EQ.4 .OR. ISFROPT.EQ.5 ) THEN
            ksfropt = 1
            elslpe = (SEG(8, nseg)-SEG(13, nseg))/seglen
            hcslpe = (SEG(6, nseg)-SEG(11, nseg))/seglen
            thkslpe = (SEG(7, nseg)-SEG(12, nseg))/seglen
          END IF
          DO ii = 1, ISEG(4, nseg)
            krck = ISTRM(1, irch)
            irck = ISTRM(2, irch)
            jrck = ISTRM(3, irch)
            rchlen = STRM(1, irch)
            dist = sumlen + (0.5*rchlen)
            STRM(12, irch) = runoff*(rchlen/seglen)
            IF ( ksfropt.EQ.1 ) THEN
              avhc = SEG(6, nseg) - (hcslpe*dist)
              avthk = SEG(7, nseg) - (thkslpe*dist)
              STRM(2, irch) = elslpe
              STRM(3, irch) = SEG(8, nseg) - (elslpe*dist)
              STRM(4, irch) = STRM(3, irch) - avthk
              IF ( Iunitlpf.GT.0 ) THEN
                Ltyp = LAYTYP(krck)
              ELSEIF ( Iunitbcf.GT.0 ) THEN
                Ltyp = LAYCON(krck)
              ELSEIF ( Iunithuf.GT.0 ) THEN
                Ltyp = LTHUF(krck)
              END IF
!              IF ( Ltyp.GT.0 .AND. IBOUND(jrck,irck,krck).GT.0 ) THEN
!                IF ( STRM(4, irch).LT.BOTM(jrck,irck,krck) ) THEN
!                  write(iout,*)'Streambed has lower altitude than ',
!     +                      'GW cell bottom. Model stopping '
!                  write(iout,*)'Segment: ',nseg,' Reach: ',ireachck
!                  write(iout,*)'Layer: ',krck,' Row: ',irck,' Col: ',
!     +                          jrck
!                  write(iout,*)'Streambed bottom altitude: ',
!     +                          STRM(4, irch)
!                  write(iout,*)'Cell bottom altitude: ',
!     +                        BOTM(jrck,irck,krck)
!                  CALL USTOP('')
!                END IF
!              END IF
              STRM(6, irch) = avhc
              STRM(8, irch) = avthk
C20-----COMPUTE STREAMBED ELEVATION AND STREAM WIDTH FOR BEGINNING
C         OF EACH STREAM SEGMENT FOR COMPUTATION OF LAKE OUTFLOW.
cdep 4/26/2006
            ELSE
              IF ( ii.EQ.1) THEN
                SEG(8,nseg) = STRM(3,irch) + ( 0.5 * STRM(1,irch)
     +                       * STRM(2,irch) )
              END IF
            END IF
!dep 4/28/2008 Added check and warning for streambed thickness
            IF (STRM(8, irch).LT.CLOSEZERO)THEN
!              WRITE (IOUT, 9030) nseg, irch, STRM(8, irch)
              STRM(8, irch) = 1.0
            END IF
!dep 4/28/2008 end of change
            IF ( icalc.EQ.0 ) THEN
              avdpth = SEG(10, nseg) - (dpslpe*dist)
              STRM(5, irch) = SEG(9, nseg) - (wdslpe*dist)
              STRM(7, irch) = avdpth
              STRM(13, irch) = etsw*rchlen*STRM(5, irch)
              STRM(14, irch) = pptsw*rchlen*STRM(5, irch)
              STRM(15, irch) = avdpth + STRM(3, irch)
              IF ( ksfropt.EQ.1 ) STRM(16, irch)
     +             = (avhc*STRM(5, irch)*rchlen)/avthk
            ELSE IF ( icalc.EQ.1 ) THEN
              STRM(5, irch) = SEG(9, nseg) - (wdslpe*dist)
              STRM(7, irch) = 1.0
              STRM(13, irch) = etsw*rchlen*STRM(5, irch)
              STRM(14, irch) = pptsw*rchlen*STRM(5, irch)
              STRM(15, irch) = STRM(3, irch)
              IF ( ksfropt.EQ.1 ) STRM(16, irch)
     +             = (avhc*STRM(5, irch)*rchlen)/avthk
            ELSE IF ( icalc.GE.2 .AND. icalc.LE.4 ) THEN
              STRM(5, irch) = 1.0
              STRM(7, irch) = 1.0
              STRM(13, irch) = etsw*rchlen
              STRM(14, irch) = pptsw*rchlen
              STRM(15, irch) = STRM(3, irch)
              IF ( ksfropt.EQ.1 )
     +             STRM(16, irch) = STRM(5, irch)*STRM(1, irch)
     +                              *STRM(6, irch)/STRM(8, irch)
C
C21-----STOP IF ICALC LESS THAN 0 AND GREATER THAN 4.
            ELSE
              STOP 'icalc problem, < 0 or > 4'
            END IF
            sumlen = sumlen + rchlen
            irch = irch + 1
          END DO
        END DO
C
C22-----CHECK VALUES IN STREAM CROSS SECTION LIST (XSEC).
        DO nseg = 1, NSS
          icalc = ISEG(1, nseg)
          IF ( icalc.EQ.2 ) THEN
            IF ( ABS(XSEC(1,nseg)).GT.zero ) THEN
              WRITE (IOUT, 9014) nseg
              CALL USTOP(' ')
            END IF
            DO jj = 1, 8
              IF ( XSEC(jj, nseg).LT.0.0 ) THEN
                WRITE (IOUT, 9015) nseg, jj, XSEC(jj, nseg)
                CALL USTOP(' ')
              END IF
              kk = jj + 8
              IF ( XSEC(kk, nseg).LT.0.0 ) THEN
                WRITE (IOUT, 9016) nseg, kk, XSEC(kk, nseg)
                CALL USTOP(' ')
              END IF
            END DO
          END IF
        END DO
 9030   FORMAT (/, ' *** WARNING *** STREAMBED THICKNESS',
     +          'FOR SEGMENT ',I10,' REACH ',I10,
     +          ' IS ', E10.4,' WHICH IS ZERO OR LESS. '/,
     +          ' VALUE MUST BE GREATER THAN ZERO-- IT HAS BEEN ',
     +          'RESET TO 1.0')
 9014   FORMAT (1X, /, ' *** ERROR *** EIGHT POINT CROSS ',
     +          'SECTION FOR STREAM SEGMENT', I7,
     +          ' DOES NOT BEGIN WITH ZERO FOR FIRST VALUE --',
     +          'PROGRAM STOPPING')
 9015   FORMAT (1X, /, ' *** ERROR *** STREAM SEGMENT', I7,
     +          ' HAS A NEGATIVE X DISTANCE FOR POINT', I6,
     +          ' INPUT VALUE IS', E11.3, /,
     +          ' ALL VALUES MUST BE POSITIVE WITH ',
     +          'FIRST X VALUE STARTING AT EXTREME LEFT ',
     +          'EDGE OF SECTION LOOKING DOWNSTREAM PROGRAM STOPPING')
 9016   FORMAT (1X, /, ' *** ERROR *** STREAM SEGMENT', I7,
     +          ' HAS A NEGATIVE Z DISTANCE FOR POINT', I6,
     +          ' INPUT VALUE IS', E11.3, /,
     +          ' ALL VALUES MUST BE POSITIVE RELATIVE ',
     +          'TO STREAMBED ELEVATION ')
C
C23-----CHECK ROUGHNESS COEFFICIENTS WHEN ICALC = 1 OR 2.
        DO nseg = 1, NSS
          icalc = ISEG(1, nseg)
          IF ( icalc.EQ.1 ) THEN
            rough = SEG(16, nseg)
            IF ( rough.LE.0.0 ) THEN
              WRITE (IOUT, 9017) rough
              CALL USTOP(' ')
            END IF
          ELSE IF ( icalc.EQ.2 ) THEN
            roughch = SEG(16, nseg)
            roughbnk = SEG(17, nseg)
            IF ( roughch.LE.0.0 ) THEN
              WRITE (IOUT, 9018) roughch
              CALL USTOP(' ')
            ELSE IF ( roughbnk.LE.0.0 ) THEN
              WRITE (IOUT, 9019) roughbnk
              CALL USTOP(' ')
            END IF
          END IF
        END DO
 9017   FORMAT ('*** ERROR *** ROUGHNESS COEFFICIENT WHEN ',
     +          'ICALC = 1 IS LESS THAN OR EQUAL TO ZERO', //,
     +          ' VALUE IS', 1PE11.3, //, ' PROGRAM STOPPING')
 9018   FORMAT ('*** ERROR *** ROUGHNESS COEFFICIENT FOR ',
     +          'CHANNEL WHEN ICALC =2 IS LESS THAN OR EQUAL TO ZERO',//
     +          ' VALUE IS', 1PE11.3, //, ' PROGRAM STOPPING')
 9019   FORMAT ('*** ERROR *** ROUGHNESS COEFFICIENT FOR BANK ',
     +          'WHEN ICALC =2 IS LESS THAN OR EQUAL TO ZERO', //,
     +          ' VALUE IS', 1PE11.3, //, ' PROGRAM STOPPING')
C
C24-----CHECK VALUES IN TABLE OF FLOW VERSUS DEPTH AND WIDTH
C         WHEN ICALC = 4.
        DO nseg = 1, NSS
          icalc = ISEG(1, nseg)
          IF ( icalc.EQ.4 ) nstrpts = ISEG(2, nseg)
          IF ( icalc.EQ.4 ) THEN
            flwlw = QSTAGE(1, nseg)
            IF ( flwlw.LE.0.0 ) THEN
!              WRITE (IOUT, 9020) nseg
              QSTAGE(1, nseg) = 0.1
            END IF
            dpthlw = QSTAGE(1+nstrpts, nseg)
            IF ( dpthlw.LE.0.0 ) THEN
!              WRITE (IOUT, 9021) nseg
              QSTAGE(1+nstrpts, nseg) = 0.01
            END IF
            wdthlw = QSTAGE(1+2*nstrpts, nseg)
            IF ( wdthlw.LE.0.0 ) THEN
!              WRITE (IOUT, 9022) nseg
              QSTAGE(1+2*nstrpts, nseg) = 1.0
            END IF
            DO ipt = 2, nstrpts
              flw1 = QSTAGE(ipt-1, nseg)
              flw2 = QSTAGE(ipt, nseg)
              dpth1 = QSTAGE((ipt-1)+nstrpts, nseg)
              dpth2 = QSTAGE(ipt+nstrpts, nseg)
              wdth1 = QSTAGE((ipt-1)+(2*nstrpts), nseg)
              wdth2 = QSTAGE(ipt+(2*nstrpts), nseg)
              IF ( flw2.LE.flw1 ) THEN
                WRITE (IOUT, 9023) nseg, flw2, ipt
                CALL USTOP(' ')
              END IF
              IF ( dpth2.LE.dpth1 ) THEN
                WRITE (IOUT, 9024) nseg, dpth2, ipt
                CALL USTOP(' ')
              END IF
!              IF ( wdth2.LT.wdth1 ) WRITE (IOUT, 9025) nseg, wdth2, ipt
            END DO
          END IF
        END DO
C
!        WRITE (IOUT, 9026)
      END IF
 9020 FORMAT (/, ' *** WARNING *** FIRST FLOW VALUE IN ',
     +        'TABLE OF FLOW VERSUS DEPTH AND WIDTH IS ',
     +        'LESS THAN OR EQUAL TO ZERO FOR SEGMENT NUMBER', I7, /,
     +        ' VALUE SHOULD BE GREATER THAN ZERO-- IT HAS BEEN RESET ',
     +        'TO 0.1 BUT MAY CAUSE INSTABILITY')
 9021 FORMAT (/, ' *** WARNING *** FIRST DEPTH VALUE IN TABLE ',
     +        'OF FLOW VERSUS DEPTH AND WIDTH IS LESS THAN ',
     +        'OR EQUAL TO ZERO FOR SEGMENT NUMBER', I7, /,
     +        ' VALUE SHOULD BE GREATER THAN ZERO-- ',
     +        'IT HAS BEEN RESET TO 0.01 BUT MAY CAUSE INSTABILITY')
 9022 FORMAT (/, ' *** WARNING *** FIRST WIDTH VALUE IN TABLE OF ',
     +        'FLOW VERSUS DEPTH AND WIDTH IS LESS THAN OR EQUAL',
     +        ' TO ZERO FOR SEGMENT NUMBER', I7, /,
     +        ' VALUE SHOULD BE GREATER THAN ZERO-- IT HAS BEEN ',
     +        'RESET TO 1.0 BUT MAY CAUSE INSTABILITY')
 9023 FORMAT (/, ' *** ERROR *** SEGMENT NUMBER', I7,
     +        'HAS SPECIFIED FLOW VALUE OF', 1PE11.2, ' IN LOCATION',
     +        I6, ' THAT IS LESS THAN OR EQUAL TO PRECEDING VALUE', /,
     +        ' FLOW VALUES MUST BE GREATER THAN PRECEDING VALUE',
     +        ' IN TABLE-- PROGRAM STOPPING')
 9024 FORMAT (/, ' *** ERROR *** SEGMENT NUMBER', I7,
     +        'HAS SPECIFIED DEPTH VALUE OF', 1PE11.2, ' IN LOCATION',
     +        I6, ' THAT IS LESS THAN OR EQUAL TO PRECEDING VALUE', /,
     +        ' DEPTH VALUES MUST BE GREATER THAN PRECEDING VALUE',
     +        ' IN TABLE-- PROGRAM STOPPING')
 9025 FORMAT (/, ' *** WARNING *** SEGMENT NUMBER', I7,
     +        ' HAS SPECIFIED WIDTH VALUE OF', 1PE11.2, ' IN LOCATION',
     +        I6, ' THAT IS LESS THAN PRECEDING VALUE', /,
     +        ' FOR MOST CHANNELS, WIDTH NORMALLY INCREASES WITH FLOW')
 9026 FORMAT (//)
C
C25-----COMPUTE STREAMBED ELEVATIONS FOR TOP AND BOTTOM, AND STREAMBED
C        SLOPE FROM LAND SURFACE ELEVATION WHEN SPECIFIED.
C        MODIFIED BY WOLFGANG SCHMID FOR FARM PROCESS.
Crgn---3/19/07 separated if statement to avoid referencing zero elements in array.
      IF( ABS(IRDFLG).EQ.2 ) THEN
        DO irch = 2, NSTRM
          IF( ISTRM(4, irch).GT.1 ) THEN
            IF( IDIVAR(1,ISTRM(4, irch)-1).GT.0 ) THEN
              icp = ISTRM(3, irch-1)
              irp = ISTRM(2, irch-1)
              IF( ISTRM(5, irch).EQ.1 )  SEG(13, ISTRM(4, irch)-1) =
     +           BOTM(icp, irp, 0) - SEG(13, ISTRM(4, irch)-1 )
            END IF
          END IF
        END DO
        DO nseg = 1, NSS
          IF( IDIVAR(1, nseg).GT.0 ) THEN
C
C26-----COMPUTE STREAMBED TOP ELEVATION FOR CANAL REACHES
C        IN FARM PROCESS.
            seglen = SEG(1, nseg)
            sumlen = 0.0
            DO irch = 1, NSTRM
              IF( IDIVAR(1, ISTRM(4, irch)).EQ.IDIVAR(1, nseg) ) THEN
                icalc = ISEG(1, nseg)
                rchlen = STRM(1, irch)
                dist = sumlen + (0.5 * rchlen)
                sumlen = sumlen + rchlen
                ic = ISTRM(3, irch)
                ir = ISTRM(2, irch)
                updiff = 0.0
                IF( ISTRM(5, irch).EQ.1 ) updiff = BOTM(ic, ir, 0) -
     +                                    SEG(8, ISTRM(4, irch))
                dndiff = SEG(13, ISTRM(4, irch))
                STRM(3, irch) = BOTM(ic, ir, 0) - (updiff -
     +                          (((updiff - dndiff) / seglen) * dist))
                avthk = SEG(7, nseg) - (((SEG(7, nseg) -
     +                  SEG(12, nseg)) / seglen) * dist)
                STRM(4, irch) = STRM(3, irch) - avthk
                IF ( icalc.EQ.0 ) THEN
                  STRM(15, irch) = avdpth + STRM(3, irch)
                ELSE IF ( icalc.EQ.1 ) THEN
                  STRM(15, irch) = STRM(3, irch)
                ELSE IF ( icalc.GE.2 .AND. icalc.LE.4 ) THEN
                  STRM(15, irch) = STRM(3, irch)
                END IF
              END IF
            END DO
C
C27-----COMPUTE STREAMBED SLOPE FOR CANAL REACHES IN FARM PROCESS.
C       NOTE THAT FIRST AND LAST REACH CAN NOT BE CANAL REACHES.
            DO irch = 2, NSTRM-1
              IF( IDIVAR(1, ISTRM(4, irch)).EQ.IDIVAR(1, nseg) ) THEN
                STRM(2, irch) = (STRM(3, irch-1) - STRM(3,irch+1) )
     +                           / (0.5 * STRM(1, irch-1) +
     +                          STRM(1, irch) + 0.5 * STRM(1, irch+1))
                IF( ISTRM(5, irch).EQ.1 ) THEN
                  STRM(2, irch) = (SEG(8, ISTRM(4, irch)) -
     +                             STRM(3, irch+1)) / (STRM(1, irch) +
     +                             0.5 * STRM(1, irch+1))
                END IF
                IF( ISTRM(5, irch+1).LT.ISTRM(5, irch) ) THEN
                  ic = ISTRM(3, irch)
                  ir = ISTRM(2, irch)
                  dndiff = SEG(13,ISTRM(4, irch))
                  eldn = BOTM(ic,ir,0) - dndiff
                  STRM(2, irch) = (STRM(3, irch-1) - eldn) / (0.5 *
     +                             STRM(1, irch-1) + STRM(1, irch))
                END IF
                IF( STRM(2, irch).LT.zero ) THEN
                  IF( STRM(2, irch ).LT.zero ) STRM(2, irch) = 1.0E-06
                  WRITE(IOUT,9027)  ISTRM(4,irch), ISTRM(5,irch),
     +                              STRM(2, irch)
 9027             FORMAT(1X,'SLOPE FOR SEGMENT AND REACH ',2(1x,I5),
     +                   'IS LESS THAN 1.0E-07: SETTING SLOPE TO '
     +                   '1.0E-06 ')
                END IF
              END IF
            END DO
          END IF
        END DO
C
!        WRITE (IOUT, 9028)
 9028 FORMAT (//)
      END IF
C
C29-----SET FLAGS FOR STEADY STATE OR TRANSIENT SIMULATIONS.
      iflginit = 0
      IF ( Kkper.EQ.1 ) THEN
        iflginit = 1
      ELSE IF ( iss.EQ.0 .AND. ISSFLG(Kkper-1).NE.0 ) THEN
        iflginit = 2
      END IF
C
C30-----DETERMINE VARIABLES WHEN UNSATURATED FLOW IS ACTIVE.
      DO l = 1, NSTRM
        il = ISTRM(1, l)
        ir = ISTRM(2, l)
        ic = ISTRM(3, l)
        h = HNEW(ic, ir, il)
        HLDSFR(l) = h
        IF ( IUZT.EQ.1 .AND. iflginit.GE.1 ) THEN
          istsg = ISTRM(4, l)
          icalc = ISEG(1, istsg)
          sbot = STRM(4, l)
          strlen = STRM(1, l)
          width = STRM(5, l)
C
C31-----SKIP IF CELL IS OUTSIDE ACTIVE BOUNDARY OR IS NOT WATER TABLE.
Cdep
C31B-----SEARCH FOR UPPER MOST ACTIVE CELL IN STREAM REACH.
          ilay = il
          IF ( IBOUND(ic, ir, il).GT.0 ) THEN
            TOPCELL: DO WHILE ( ilay.LE.NLAY )
              IF ( HNEW(ic, ir, ilay).LE.BOTM(ic,ir,ilay) ) THEN
                ilay = ilay + 1
              ELSE
                EXIT TOPCELL
              END IF
            END DO TOPCELL
          END IF
          IF ( ilay.LE.NLAY ) THEN
            il = ilay
            h = HNEW(ic, ir, il)
          ELSE
            h = DBLE(BOTM(ic,ir,NLAY))
          END IF
          IF ( IBOUND(ic, ir, il).LE.0 ) THEN
            UZDPST(1, l) = 0.0D0
            UZFLST(1, l) = 0.0D0
            UZSPST(1, l) = 0.0D0
            UZTHST(1, l) = THTR(l)
            UZSTOR(1, l) = 0.0D0
            UZOLSFLX(1, l) = 0.0D0
C
C32-----BREAK CHANNEL INTO ISUZN WIDTHS FOR UNSATURATED FLOW
C         WHEN ICALC IS 2 AND UNSATURATED FLOW IS ACTIVE.
          ELSE IF ( icalc.EQ.2 ) THEN
!            CALL CHANNELAREA(istsg, l)
            istep = NSTOTRL/ISUZN
            DO jk = 1, NSTOTRL
              UZTHST(jk, l) = THTR(l)
            END DO
C
C33-----INITIALIZE UNSATURATED ZONE ARRAYS WHEN GROUND WATER HEAD
C         IS LESS THAN BOTTOM OF STREAMBED.
            IF ( sbot.GT.h ) THEN
              iwvcnt = 1
              DO i = 1, ISUZN
                UZDPST(iwvcnt, l) = sbot - h
                UZSPST(iwvcnt, l) = 0.0D0
                NWAVST(i, l) = 1
C
C34-----INITIALIZE UNSATURATED ZONES ARRAYS FOR SECOND STRESS PERIOD
C         WHEN FIRST STRESS PERIOD IS STEADY STATE.
                IF ( iflginit.EQ.2 ) THEN
                  IF ( UZSEEP(i, l).GT.0.0 ) THEN
                    UZFLST(iwvcnt, l) = UZSEEP(i, l)
                    UZTHST(iwvcnt, l) = (((UZFLST(iwvcnt,l)/UHC(l))**(
     +                                  1.0D0/EPS(l)))*(THTS(l)-THTR(l))
     +                                  ) + THTR(l)
                    top = UZTHST(iwvcnt, l) - THTR(l)
                    UZSTOR(i, l) = UZDPST(iwvcnt, l)*top
     +                             *WETPER(i, l)*strlen
                    UZOLSFLX(i, l) = UZSEEP(i, l)
                  ELSE
                    UZFLST(iwvcnt, l) = 0.0D0
                    UZTHST(iwvcnt, l) = THTR(l)
                    UZSTOR(i, l) = 0.0D0
                    UZOLSFLX(i, l) = 0.0D0
                  END IF
C
C35-----INITIALIZE UNSATURATED ZONE ARRAYS WHEN FIRST STRESS PERIOD IS
C         TRANSIENT.
                ELSE IF ( iss.EQ.0 ) THEN
                  top = THTI(l) - THTR(l)
                  IF ( top.LT.CLOSEZERO ) top = 0.0
                  UZTHST(1, l) = THTI(l)
                  UZSTOR(1, l) = UZDPST(1, l)*top*WETPER(1, l)*strlen
                  bottom = THTS(l) - THTR(l)
                  IF ( bottom.LT.CLOSEZERO .OR. top.LT.CLOSEZERO ) THEN
                    UZFLST(1, l) = 0.0D0
                  ELSE
                    UZFLST(1, l) = UHC(l)*(top/bottom)**EPS(l)
                  END IF
C
C36-----INITIALIZE UNSATURATED ZONE ARRAYS WHEN FIRST STRESS PERIOD IS
C         STEADY STATE.
                ELSE
                  UZTHST(1, l) = THTR(l)
                  UZSTOR(1, l) = 0.0D0
                  UZFLST(1, l) = 0.0D0
                  UZOLSFLX(1, l) = 0.0D0
                END IF
                iwvcnt = iwvcnt + istep
              END DO
C
C37-----INITIALIZE UNSATURATED ZONE ARRAYS TO ZERO WHEN NO UNSATURATED
C         ZONE.
            ELSE
              iwvcnt = 1
              istep = NSTOTRL/ISUZN
              DO i = 1, ISUZN
                UZDPST(iwvcnt, l) = 0.0D0
                UZFLST(iwvcnt, l) = 0.0D0
                UZSPST(iwvcnt, l) = 0.0D0
                UZTHST(1, l) = THTR(l)
                iwvcnt = iwvcnt + istep
              END DO
            END IF
            UZOLSFLX(1, l) = UZFLST(1, l)
C
C38-----ONLY ONE UNSATURATED ZONE WIDTH WHEN ICALC IS 1.
          ELSE IF ( icalc.EQ.1 ) THEN
            WETPER(1, l) = width
C
C39-----INITIALIZE UNSATURATED ZONE ARRAYS WHEN GROUND WATER HEAD
C         IS LESS THAN BOTTOM OF STREAMBED.
            IF ( sbot.GT.h ) THEN
              UZDPST(1, l) = sbot - h
              UZSPST(1, l) = 0.0D0
              NWAVST(1, l) = 1
C
C40-----INITIALIZE UNSATURATED ZONE ARRAYS FOR SECOND STRESS PERIOD
C         WHEN FIRST STRESS PERIOD IS STEADY STATE.
              IF ( iflginit.EQ.2 ) THEN
                IF ( UZSEEP(1, l).GT.0.0 ) THEN
                  UZFLST(1, l) = UZSEEP(1, l)
                  UZTHST(1, l) = (((UZFLST(1,l)/UHC(l))**(1.0D0/EPS(l)))
     +                           *(THTS(l)-THTR(l))) + THTR(l)
                  top = UZTHST(1, l) - THTR(l)
                  IF ( top.LT.CLOSEZERO ) top = 0.0
                  UZSTOR(1, l) = UZDPST(1, l)*top*WETPER(1, l)*strlen
                  UZOLSFLX(1, l) = UZSEEP(1, l)
                ELSE
                  UZFLST(1, l) = 0.0D0
                  UZTHST(1, l) = THTR(l)
                  UZSTOR(1, l) = 0.0D0
                  UZOLSFLX(1, l) = 0.0D0
                END IF
C
C41-----INITIALIZE UNSATURATED ZONE ARRAYS WHEN FIRST STRESS PERIOD IS
C         TRANSIENT.
              ELSE IF ( iss.EQ.0 ) THEN
                UZTHST(1, l) = THTI(l)
                top = THTI(l) - THTR(l)
                IF ( top.LT.CLOSEZERO ) top = 0.0
                UZSTOR(1, l) = UZDPST(1, l)*top*width*strlen
                bottom = THTS(l) - THTR(l)
                IF ( bottom.LT.CLOSEZERO .OR. top.LT.CLOSEZERO ) THEN
                  UZFLST(1, l) = 0.0D0
                ELSE
                  UZFLST(1, l) = UHC(l)*(top/bottom)**EPS(l)
                END IF
C
C42-----INITIALIZE UNSATURATED ZONE ARRAYS WHEN FIRST STRESS PERIOD IS
C         STEADY STATE.
              ELSE
                UZTHST(1, l) = THTR(l)
                UZSTOR(1, l) = 0.0D0
                UZFLST(1, l) = 0.0D0
              END IF
            ELSE
C
C43-----INITIALIZE UNSATURATED ZONE ARRAYS TO ZERO WHEN NO UNSATURATED
C         ZONE.
              UZTHST(1, l) = THTR(l)
              UZDPST(1, l) = 0.0D0
              UZFLST(1, l) = 0.0D0
              UZSPST(1, l) = 0.0D0
              UZSTOR(1, l) = 0.0D0
            END IF
            DO ik = 2, NSTOTRL
              UZTHST(ik, l) = THTR(l)
            END DO
            UZOLSFLX(1, l) = UZFLST(1, l)
          END IF
        END IF
      END DO
Cdep    Added new subroutine to compute tables for lake outflow
C44-----COMPUTE VALUES FOR ARRAYS DKLOTFLW AND DLKSTAGE WHEN OUTFLOW FROM
C        LAKES ARE COMPUTED IN THE LAKE PACKAGE.
!      IF ( Iunitlak.GT.0 ) THEN
!        CALL GWF2SFR7LAKOUTFLW(1)
!      END IF
CC45-----READ TABLES FOR SPECIFIED INFLOWS
      IF ( Kkper.EQ.1 ) THEN
        IF ( NUMTAB.GT.0 ) THEN
          DO i=1,NUMTAB
! segment number, number of rows, unit number
            READ(In,*)ISFRLIST(1,i),ISFRLIST(2,i),ISFRLIST(3,i)
	      WRITE(iout, *) 'SEGNUM NUMVAL IUNIT:'
	      WRITE(iout, *) ISFRLIST(1,i),ISFRLIST(2,i),ISFRLIST(3,i)
!            WRITE(iout,9033)ISFRLIST(1,i),ISFRLIST(3,i)
!            WRITE(iout,9031)
            numval = ISFRLIST(2,i)
            iunit = ISFRLIST(3,i)
            DO j = 1, numval
              READ(iunit,*)TABTIME(j,ISFRLIST(1,i)),
     +                     TABFLOW(j,ISFRLIST(1,i))
              IF ( TABFLOW(j,ISFRLIST(1,i)).LT.0.0 ) THEN
                TABFLOW(j,ISFRLIST(1,i)) = 0.0
!                WRITE(IOUT,9029)
              END IF
              WRITE(IOUT,*)TABTIME(j,ISFRLIST(1,i)),
     +                        TABFLOW(j,ISFRLIST(1,i))
!              WRITE(IOUT,9032)TABTIME(j,ISFRLIST(1,i)),
!     +                        TABFLOW(j,ISFRLIST(1,i))
            END DO
          END DO
        END IF
      END IF
 9029 FORMAT('A NEGATIVE VALUE FOR FLOW WAS SPECIFIED IN A ',
     +        'SFR TABULAR INFLOW FILE. VALUE WILL BE RESET TO ZERO')
 9033 FORMAT('TABULAR INFLOWS WERE READ FOR SEGMENT ',I6,/
     +       'FROM FILE UNIT NUMBER ',I6,/)
 9031 FORMAT(10X,'TIMES',20X,'INFLOWS')
 9032 FORMAT(5X,F20.10,1X,F20.10)
C
C45-----RETURN.
      RETURN
      END SUBROUTINE GWF2SFR7RP
C
C-------SUBROUTINE GWF2SFR7FM
!      SUBROUTINE GWF2SFR7FM(Kkiter, Kkper, Kkstp, Iunitlak, Nlakesar,
!     +                      Theta, Stgold, Stgnew, Vol, Igrid)
C     *****************************************************************
C     ADD STREAM TERMS TO RHS AND HCOF IF FLOW OCCURS IN MODEL CELL
C     VERSION  7.1: JUNE 29, 2006
C     *****************************************************************
C-------SUBROUTINE GWF2SFR7BD
!      SUBROUTINE GWF2SFR7BD(Kkstp, Kkper, Iunitgwt, Iunitlak, Iunitgage,
!     +                      Iunituzf, Nlakesar, Vol, Nsol, Igrid)
C     *****************************************************************
C     CALCULATE VOLUMETRIC GROUND-WATER BUDGET FOR STREAMS AND SUM
C     STREAMFLOWS IN MODELED AREA
C     VERSION  7.1: JUNE 29, 2006
C     *****************************************************************
C-------SUBROUTINE GWF2SFR7LAKOUTFLW
!      SUBROUTINE GWF2SFR7LAKOUTFLW(kkiter)
C     *****************************************************************
C     CALCULATE ARRAYS OF LAKE STAGE, FLOW, AND THE DERIVATIVE OF
C     FLOWS FOR STREAM SEGMENTS THAT HAVE INFLOWS DETERMINED BY
C     LAKE STAGE
C     VERSION  7.1: JUNE 29, 2006
C     *****************************************************************
C-------SUBROUTINE GWF2SFR7DIVERS
!      SUBROUTINE GWF2SFR7DIVERS(Iprior, Idivseg, Upflw, Dvrsn)
C     ******************************************************************
C     COMPUTES DIVERSIONS FROM AN UPSTREAM SEGMENT
C     VERSION  7.1: SEPTEMBER 20, 2006   DEP AND RGN
C     ******************************************************************
C-------SUBROUTINE GWF2SFR7UZOT
!      SUBROUTINE GWF2SFR7UZOT(Kkstp, Kkper)
C     ******************************************************************
C     PRINTS MASS BALANCE FOR ENTIRE UNSATURATED ZONE
C     VERSION  7.1: JUNE 29, 2006
C     ******************************************************************
C-------SUBROUTINE GWF2SFR7DPTH
!      SUBROUTINE GWF2SFR7DPTH(Flow, Slope, Istsg, Nreach, Roughch,
!     +                        Roughbnk, Wetperm, Depth, Itstr, Totwdth,
!     +                        Iprndpth)
C     ******************************************************************
C     COMPUTE STREAM DEPTH GIVEN FLOW USING 8-POINT CROSS SECTION
C     VERSION  7.1: JUNE 29, 2006
C     ******************************************************************
C-------SUBROUTINE GWF2SFR7FLW
!      SUBROUTINE GWF2SFR7FLW(Depth, Istsg, Roughch, Roughbnk, Slope,
!     +                       Wetperm, Flow, Totwdth)
C     *******************************************************************
C     COMPUTE FLOW IN STREAM GIVEN DEPTH USING 8-POINT CROSS SECTION
C     VERSION  7.1: JUNE 29, 2006
C     *******************************************************************
C-------SUBROUTINE GWF2SFR7TBD
!      SUBROUTINE GWF2SFR7TBD(Flow, Depth, Width, Nstrpts, Istsg)
C     *******************************************************************
C     COMPUTE DEPTH AND WIDTH IN STREAM GIVEN FLOW USING RATING TABLES.
C     VERSION  7.1: JUNE 29, 2006
C     *******************************************************************
C-------SUBROUTINE GWF2SFR7TBF
!      SUBROUTINE GWF2SFR7TBF(Flow, Depth, Width, Nstrpts, Nreach, Istsg,
!     +                       Kkiter, Itb)
C     *******************************************************************
C     COMPUTE FLOW AND WIDTH IN STREAM GIVEN DEPTH USING RATING TABLES.
C     VERSION  7.1: JUNE 29, 2006
C     *******************************************************************
C-------SUBROUTINE SGWF2SFR7RDSEG
      SUBROUTINE SGWF2SFR7RDSEG(Nlst, Lstbeg, In, Iunitgwt, Iunituzf,
     +                          Ischk, Nischk, Ichk, Kkper, Nsol)
C     ******************************************************************
C     READ STREAM SEGMENT DATA -- parameters or non parameters
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     ******************************************************************
      USE GWFSFRMODULE, ONLY: NSS, MAXPTS, ISFROPT, IDIVAR, IOTSG, ISEG,
     +                        SEG, XSEC, QSTAGE, CONCQ, CONCRUN,CONCPPT,
     +                        DVRCH, DVRCELL, RECHSAVE, DVEFF, DVRPERC  !cjm (added DVRCH, DVRCELL and RECHSAVE)
      USE GLOBAL,       ONLY: IOUT
      USE GWFRCHMODULE,ONLY: RECH  !cjm
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER Iunitgwt, Ichk, In, Ischk, Lstbeg, Nischk, Nlst, Kkper
      INTEGER Nsol, Iunituzf
      DIMENSION Ischk(Nischk)
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      INTEGER icalc, idum, ii, iqseg, isol, iupseg, jj, jk, lstend, n,
     +        noutseg, nseg, nstrpts, numcell, i  !cjm (added numcell and i)
      REAL dum, totdum
C     ------------------------------------------------------------------
C
C1------READ STREAM SEGMENT DATA.
      lstend = Lstbeg + Nlst - 1
      DO iqseg = Lstbeg, lstend
C
C2------ONLY READ FIRST 4 VARIABLES TO DETERMINE VALUE OF IUPSEG.
        READ (In, *) n, icalc, noutseg, iupseg
        write (IOUT, *) 'n, icalc, noutseg, iupseg:'
        write (IOUT, *) n, icalc, noutseg, iupseg
!       IF ( n.GT.NSS .OR. n.LT.1 ) THEN  !cjm (commented this line out)
        IF ( n.GT.NSS .OR. n.EQ.0 ) THEN              !cjm
          WRITE (IOUT, 9001) n
 9001     FORMAT (1X, /1X, 'SEGMENT NUMBER (NSEG) OUT OF RANGE: ', I6)
          IF ( Ichk.NE.0 ) THEN
            WRITE (IOUT, 9002) iqseg - Lstbeg + 1
 9002       FORMAT (1X, 'READING ENTRY ', I6, ' OF ITEM 6A')
          ELSE
            WRITE (IOUT, 9003) iqseg - Lstbeg + 1
 9003       FORMAT (1X, 'READING ENTRY ', I6, ' OF ITEM 4A')
          END IF
          CALL USTOP(' ')
        END IF
C
C2a-----DETERMINE IF SEGMENT OUTFLOW WILL BE DIVERTED TO RECHARGE MF CELLS  !cjm
        IF ( N.LT.0 ) THEN
          N = ABS(N)
          DVRCH(N) = 1
	  ELSE              !cjm 20090708
	    DVRCH(N) = 0       !cjm 20090708
        END IF
C
C3------DETERMINE WHERE DATA ARE STORED.
        IF ( Ichk.NE.0 ) THEN
C  Store data in active segment area
          nseg = n
          Ischk(n) = Ischk(n) + 1
        ELSE
C  Store data in parameter area
          nseg = iqseg
          ISEG(3, iqseg) = n
          SEG(1, nseg) = SEG(1, n)
        END IF
        BACKSPACE In
C
C4------READ DATA SET 4B FOR SEGMENTS THAT ARE NOT DIVERSIONS.
        IF ( iupseg.LE.0 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) idum, ISEG(1, nseg), IOTSG(nseg),
     +                   IDIVAR(1, nseg), (SEG(jj, nseg), jj=2, 5)
	      WRITE (IOUT, '(A100)')
     +	    'NSEG, ICALC, OUTSEG, IUPSEG, FLOW, RUNOFF, ETSW, PPTSW:'
            WRITE (IOUT, *) idum, ISEG(1, nseg), IOTSG(nseg),
     +                   IDIVAR(1, nseg), (SEG(jj, nseg), jj=2, 5)
          ELSE IF ( icalc.EQ.1 ) THEN
            READ (In, *) idum, ISEG(1, nseg), IOTSG(nseg),
     +                   IDIVAR(1, nseg), (SEG(jj, nseg), jj=2, 5),
     +                   SEG(16, nseg)
	      WRITE (IOUT, '(A100)')
     +	    'NSEG, ICALC, OUTSEG, IUPSEG, FLOW, RUNOFF, ETSW, PPTSW,
     +ROUGHCH:'
            WRITE (IOUT, *) idum, ISEG(1, nseg), IOTSG(nseg),
     +                   IDIVAR(1, nseg), (SEG(jj, nseg), jj=2, 5),
     +                   SEG(16, nseg)
          ELSE IF ( icalc.EQ.2 ) THEN
            READ (In, *) idum, ISEG(1, nseg), IOTSG(nseg),
     +                   IDIVAR(1, nseg), (SEG(jj, nseg), jj=2, 5),
     +                   (SEG(jk, nseg), jk=16, 17)
	      WRITE (IOUT, '(A100)')
     +	    'NSEG, ICALC, OUTSEG, IUPSEG, FLOW, RUNOFF,
     +ETSW, PPTSW, ROUGHCH, ROUGHBK:'
            WRITE (IOUT, *) idum, ISEG(1, nseg), IOTSG(nseg),
     +                   IDIVAR(1, nseg), (SEG(jj, nseg), jj=2, 5),
     +                   (SEG(jk, nseg), jk=16, 17)
          ELSE IF ( icalc.EQ.3 ) THEN
            READ (In, *) idum, ISEG(1, nseg), IOTSG(nseg),
     +                   IDIVAR(1, nseg), (SEG(jj, nseg), jj=2, 5),
     +                   SEG(9, nseg), SEG(10, nseg), SEG(14, nseg),
     +                   SEG(15, nseg)
	      WRITE (IOUT, '(A100)')
     +	    'NSEG, ICALC, OUTSEG, IUPSEG, FLOW, RUNOFF,
     +ETSW, PPTSW, CDPTH, FDPTH, AWDTH, BWDTH:'
            WRITE (IOUT, *) idum, ISEG(1, nseg), IOTSG(nseg),
     +                   IDIVAR(1, nseg), (SEG(jj, nseg), jj=2, 5),
     +                   SEG(9, nseg), SEG(10, nseg), SEG(14, nseg),
     +                   SEG(15, nseg)
          ELSE IF ( icalc.EQ.4 ) THEN
            READ (In, *) idum, ISEG(1, nseg), IOTSG(nseg),
     +                   IDIVAR(1, nseg), ISEG(2, nseg),
     +                   (SEG(jj, nseg), jj=2, 5)
	      WRITE (IOUT, '(A100)')
     +	    'NSEG, ICALC, OUTSEG, IUPSEG, NSTRPTS, FLOW,
     +RUNOFF, ETSW, PPTSW:'
            WRITE (IOUT, *) idum, ISEG(1, nseg), IOTSG(nseg),
     +                   IDIVAR(1, nseg), ISEG(2, nseg),
     +                   (SEG(jj, nseg), jj=2, 5)
          END IF
C
C5------READ DATA 4B FOR SEGMENTS THAT ARE DIVERSIONS FROM STREAMS.
        ELSE IF ( icalc.LE.0 ) THEN
          READ (In, *) idum, ISEG(1, nseg), IOTSG(nseg),
     +                 (IDIVAR(ii, nseg), ii=1, 2),
     +                 (SEG(jj, nseg), jj=2, 5)
	      WRITE (IOUT, '(A100)')
     +	    'NSEG, ICALC, OUTSEG, IUPSEG, IPRIOR, FLOW,
     +RUNOFF, ETSW, PPTSW:'
          WRITE (IOUT, *) idum, ISEG(1, nseg), IOTSG(nseg),
     +                 (IDIVAR(ii, nseg), ii=1, 2),
     +                 (SEG(jj, nseg), jj=2, 5)
        ELSE IF ( icalc.EQ.1 ) THEN
          READ (In, *) idum, ISEG(1, nseg), IOTSG(nseg),
     +                 (IDIVAR(ii, nseg), ii=1, 2),
     +                 (SEG(jj, nseg), jj=2, 5), SEG(16, nseg)
	      WRITE (IOUT, '(A100)')
     +	    'NSEG, ICALC, OUTSEG, IUPSEG, IPRIOR, FLOW,
     +RUNOFF, ETSW, PPTSW, ROUGHCH:'
          WRITE (IOUT, *) idum, ISEG(1, nseg), IOTSG(nseg),
     +                 (IDIVAR(ii, nseg), ii=1, 2),
     +                 (SEG(jj, nseg), jj=2, 5), SEG(16, nseg)
        ELSE IF ( icalc.EQ.2 ) THEN
          READ (In, *) idum, ISEG(1, nseg), IOTSG(nseg),
     +                 (IDIVAR(ii, nseg), ii=1, 2),
     +                 (SEG(jj, nseg), jj=2, 5),
     +                 (SEG(jk, nseg), jk=16, 17)
	      WRITE (IOUT, '(A100)')
     +	    'NSEG, ICALC, OUTSEG, IUPSEG, IPRIOR, FLOW,
     +RUNOFF, ETSW, PPTSW, ROUGHCH, ROUGHBK:'
          WRITE (IOUT, *) idum, ISEG(1, nseg), IOTSG(nseg),
     +                 (IDIVAR(ii, nseg), ii=1, 2),
     +                 (SEG(jj, nseg), jj=2, 5),
     +                 (SEG(jk, nseg), jk=16, 17)
        ELSE IF ( icalc.EQ.3 ) THEN
          READ (In, *) idum, ISEG(1, nseg), IOTSG(nseg),
     +                 (IDIVAR(ii, nseg), ii=1, 2),
     +                 (SEG(jj, nseg), jj=2, 5), SEG(9, nseg),
     +                 SEG(10, nseg), SEG(14, nseg), SEG(15, nseg)
	      WRITE (IOUT, '(A100)')
     +	    'NSEG, ICALC, OUTSEG, IUPSEG, IPRIOR, FLOW, RUNOFF,
     +ETSW, PPTSW, CDPTH, FDPTH, AWDTH, BWDTH:'
          WRITE (IOUT, *) idum, ISEG(1, nseg), IOTSG(nseg),
     +                 (IDIVAR(ii, nseg), ii=1, 2),
     +                 (SEG(jj, nseg), jj=2, 5), SEG(9, nseg),
     +                 SEG(10, nseg), SEG(14, nseg), SEG(15, nseg)
        ELSE IF ( icalc.EQ.4 ) THEN
          READ (In, *) idum, ISEG(1, nseg), IOTSG(nseg),
     +                 (IDIVAR(ii, nseg), ii=1, 2), ISEG(2, nseg),
     +                 (SEG(jj, nseg), jj=2, 5)
	      WRITE (IOUT, '(A100)')
     +	    'NSEG, ICALC, OUTSEG, IUPSEG, IPRIOR, NSTRPTS, FLOW,
     +RUNOFF, ETSW, PPTSW:'
          WRITE (IOUT, *) idum, ISEG(1, nseg), IOTSG(nseg),
     +                 (IDIVAR(ii, nseg), ii=1, 2), ISEG(2, nseg),
     +                 (SEG(jj, nseg), jj=2, 5)
        END IF
C
C6------READ DATA SET 4C.
        IF ( ISFROPT.EQ.0 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=6, 10)
	      WRITE(IOUT, *) 'HCOND1, THICKM1, ELEVUP, WIDTH1,DEPTH1:'
            WRITE(IOUT, *) (SEG(jj, nseg), jj=6, 10)
          ELSE IF ( icalc.EQ.1 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=6, 9)
	      WRITE(IOUT, *) 'HCOND1, THICKM1, ELEVUP, WIDTH1:'
            WRITE(IOUT, *) (SEG(jj, nseg), jj=6, 9)
          ELSE IF ( icalc.GE.2 .AND. icalc.LE.4 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=6, 8)
	      WRITE(IOUT, *) 'HCOND1, THICKM1, ELEVUP:'
            WRITE(IOUT, *) (SEG(jj, nseg), jj=6, 8)
          END IF
        ELSE IF ( ISFROPT.EQ.1 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) SEG(9, nseg), SEG(10, nseg)
	      WRITE(IOUT, *) 'WIDTH1,DEPTH1:'
            WRITE(IOUT, *) SEG(9, nseg), SEG(10, nseg)
          ELSE IF ( icalc.EQ.1 ) THEN
            READ (In, *) SEG(9, nseg)
	      WRITE(IOUT, *) 'WIDTH1:'
            WRITE(IOUT, *) SEG(9, nseg)
          END IF
        ELSE IF ( ISFROPT.EQ.2 .OR. ISFROPT.EQ.3 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) SEG(9, nseg), SEG(10, nseg)
	      WRITE(IOUT, *) 'WIDTH1,DEPTH1:'
            WRITE(IOUT, *) SEG(9, nseg), SEG(10, nseg)
          ELSE IF ( icalc.EQ.1 .AND. Kkper.EQ.1 ) THEN
            READ (In, *) SEG(9, nseg)
	      WRITE(IOUT, *) 'WIDTH1:'
            WRITE(IOUT, *) SEG(9, nseg)
          END IF
        ELSE IF ( ISFROPT.EQ.4 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=6, 10)
	      WRITE(IOUT, *) 'HCOND1, THICKM1, ELEVUP, WIDTH1,DEPTH1:'
            WRITE(IOUT, *) (SEG(jj, nseg), jj=6, 10)
          ELSE IF ( icalc.EQ.1 ) THEN
            IF ( Kkper.EQ.1 ) THEN
              READ (In, *) (SEG(jj, nseg), jj=6, 9),
     +                     (SEG(jj, nseg), jj=18, 20)
	        WRITE(IOUT, *) 'HCOND1, THICKM1, ELEVUP, WIDTH1,
     +THTS1, THTI1, EPS1:'
              WRITE(IOUT, *) (SEG(jj, nseg), jj=6, 9),
     +                     (SEG(jj, nseg), jj=18, 20)
            ELSE
              READ (In, *) SEG(6, nseg)
	        WRITE(IOUT, *) 'HCOND1:'
              WRITE(IOUT, *) SEG(6, nseg)
            END IF
          ELSE IF ( icalc.EQ.2 ) THEN
            IF ( Kkper.EQ.1 ) THEN
              READ (In, *) (SEG(jj, nseg), jj=6, 8),
     +                     (SEG(jj, nseg), jj=18, 20)
	        WRITE(IOUT, *) 'HCOND1, THICKM1, ELEVUP,'
     +  // 'THTS1, THTI1, EPS1:'
              WRITE(IOUT, *) (SEG(jj, nseg), jj=6, 8),
     +                     (SEG(jj, nseg), jj=18, 20)
            ELSE
              READ (In, *) SEG(6, nseg)
	        WRITE(IOUT, *) 'HCOND1:'
              WRITE(IOUT, *) SEG(6, nseg)
            END IF
          ELSE IF ( icalc.GE.3 .AND. icalc.LE.4 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=6, 8)
	      WRITE(IOUT, *) 'HCOND1, THICKM1, ELEVUP:'
            WRITE(IOUT, *) (SEG(jj, nseg), jj=6, 8)
          END IF
        ELSE IF ( ISFROPT.EQ.5 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=6, 10)
	      WRITE(IOUT, *) 'HCOND1, THICKM1, ELEVUP, WIDTH1,DEPTH1:'
            WRITE(IOUT, *) (SEG(jj, nseg), jj=6, 10)
          ELSE IF ( icalc.EQ.1 ) THEN
            IF ( Kkper.EQ.1 ) THEN
              READ (In, *) (SEG(jj, nseg), jj=6, 9),
     +                     (SEG(jj, nseg), jj=18, 21)
	        WRITE(IOUT, *) 'HCOND1, THICKM1, ELEVUP, WIDTH1,'
     +  // 'THTS1, THTI1, EPS1, UHC1:'
              WRITE (IOUT, *) (SEG(jj, nseg), jj=6, 9),
     +                     (SEG(jj, nseg), jj=18, 21)
            ELSE
              READ (In, *) SEG(6, nseg)
	        WRITE(IOUT, *) 'HCOND1:'
              WRITE(IOUT, *) SEG(6, nseg)
            END IF
          ELSE IF ( icalc.EQ.2 ) THEN
            IF ( Kkper.EQ.1 ) THEN
              READ (In, *) (SEG(jj, nseg), jj=6, 8),
     +                     (SEG(jj, nseg), jj=18, 21)
	        WRITE(IOUT, *) 'HCOND1, THICKM1, ELEVUP,'
     +  // 'THTS1, THTI1, EPS1, UHC1:'
              WRITE(IOUT, *) (SEG(jj, nseg), jj=6, 8),
     +                     (SEG(jj, nseg), jj=18, 21)
            ELSE
              READ (In, *) SEG(6, nseg)
	        WRITE(IOUT, *) 'HCOND1:'
              WRITE(IOUT, *) SEG(6, nseg)
            END IF
          ELSE IF ( icalc.GE.3 .AND. icalc.LE.4 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=6, 8)
	      WRITE(IOUT, *) 'HCOND1, THICKM1, ELEVUP:'
            WRITE(IOUT, *) (SEG(jj, nseg), jj=6, 8)
          END IF
        END IF
C
C7------READ DATA SET 4D.
        IF ( ISFROPT.EQ.0 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=11, 15)
	      WRITE(IOUT, *) 'HCOND2, THICKM2, ELEVDN, WIDTH2,DEPTH2:'
            WRITE(IOUT, *) (SEG(jj, nseg), jj=11, 15)
          ELSE IF ( icalc.EQ.1 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=11, 14)
	      WRITE(IOUT, *) 'HCOND2, THICKM2, ELEVDN, WIDTH2:'
            WRITE(IOUT, *) (SEG(jj, nseg), jj=11, 14)
          ELSE IF ( icalc.GE.2 .AND. icalc.LE.4 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=11, 13)
	      WRITE(IOUT, *) 'HCOND2, THICKM2, ELEVDN:'
            WRITE(IOUT, *) (SEG(jj, nseg), jj=11, 13)
          END IF
        ELSE IF ( ISFROPT.EQ.1 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) SEG(14, nseg), SEG(15, nseg)
	      WRITE(IOUT, *) 'WIDTH2,DEPTH2:'
            WRITE(IOUT, *) SEG(14, nseg), SEG(15, nseg)
          ELSE IF ( icalc.EQ.1 ) THEN
            READ (In, *) SEG(14, nseg)
	      WRITE(IOUT, *) 'WIDTH2:'
            WRITE(IOUT, *) SEG(14, nseg)
          END IF
        ELSE IF ( ISFROPT.EQ.2 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) SEG(14, nseg), SEG(15, nseg)
	      WRITE(IOUT, *) 'WIDTH2,DEPTH2:'
            WRITE(IOUT, *) SEG(14, nseg), SEG(15, nseg)
          ELSE IF ( icalc.EQ.1 .AND. Kkper.EQ.1 ) THEN
            READ (In, *) SEG(14, nseg)
	      WRITE(IOUT, *) 'WIDTH2:'
            WRITE(IOUT, *) SEG(14, nseg)
          END IF
        ELSE IF ( ISFROPT.EQ.3 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) SEG(14, nseg), SEG(15, nseg)
	      WRITE(IOUT, *) 'WIDTH2,DEPTH2:'
            WRITE(IOUT, *) SEG(14, nseg), SEG(15, nseg)
          ELSE IF ( icalc.EQ.1 .AND. Kkper.EQ.1 ) THEN
            READ (In, *) SEG(14, nseg)
	      WRITE(IOUT, *) 'WIDTH2:'
            WRITE(IOUT, *) SEG(14, nseg)
          END IF
        ELSE IF ( ISFROPT.EQ.4 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=11, 15)
	      WRITE(IOUT, *) 'HCOND2, THICKM2, ELEVDN, WIDTH2,DEPTH2:'
            WRITE(IOUT, *) (SEG(jj, nseg), jj=11, 15)
          ELSE IF ( icalc.EQ.1 ) THEN
            IF ( Kkper.EQ.1 ) THEN
              READ (In, *) (SEG(jj, nseg), jj=11, 14),
     +                     (SEG(jj, nseg), jj=22, 24)
	        WRITE(IOUT, *) 'HCOND2, THICKM2, ELEVDN, WIDTH2,'
     +  // 'THTS2, THTI2, EPS2:'
              WRITE(IOUT, *) (SEG(jj, nseg), jj=11, 14),
     +                     (SEG(jj, nseg), jj=22, 24)
            ELSE
              READ (In, *) SEG(11, nseg)
	        WRITE(IOUT, *) 'HCOND2:'
              WRITE(IOUT, *) SEG(11, nseg)
            END IF
          ELSE IF ( icalc.EQ.2 ) THEN
            IF ( Kkper.EQ.1 ) THEN
              READ (In, *) (SEG(jj, nseg), jj=11, 13),
     +                     (SEG(jj, nseg), jj=22, 24)
	        WRITE(IOUT, *) 'HCOND2, THICKM2, ELEVDN, WIDTH2,'
     +  // 'THTS2, THTI2, EPS2:'
              WRITE(IOUT, *) (SEG(jj, nseg), jj=11, 13),
     +                     (SEG(jj, nseg), jj=22, 24)
            ELSE
              READ (In, *) SEG(11, nseg)
	        WRITE(IOUT, *) 'HCOND2:'
              WRITE(IOUT, *) SEG(11, nseg)
            END IF
          ELSE IF ( icalc.GE.3 .AND. icalc.LE.4 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=11, 13)
	      WRITE(IOUT, *) 'HCOND2, THICKM2, ELEVDN, WIDTH2:'
            WRITE(IOUT, *) (SEG(jj, nseg), jj=11, 13)
          END IF
        ELSE IF ( ISFROPT.EQ.5 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=11, 15)
	      WRITE(IOUT, *) 'HCOND2, THICKM2, ELEVDN, WIDTH2,DEPTH2:'
            WRITE(IOUT, *) (SEG(jj, nseg), jj=11, 15)
          ELSE IF ( icalc.EQ.1 ) THEN
            IF ( Kkper.EQ.1 ) THEN
              READ (In, *) (SEG(jj, nseg), jj=11, 14),
     +                     (SEG(jj, nseg), jj=22, 25)
	        WRITE(IOUT, *) 'HCOND2, THICKM2, ELEVDN, WIDTH2, '
     +  // 'THTS2, THTI2, EPS2, UHC2:'
              WRITE(IOUT, *) (SEG(jj, nseg), jj=11, 14),
     +                     (SEG(jj, nseg), jj=22, 25)
            ELSE
              READ (In, *) SEG(11, nseg)
	        WRITE(IOUT, *) 'HCOND2:'
              WRITE(IOUT, *) SEG(11, nseg)
            END IF
          ELSE IF ( icalc.EQ.2 ) THEN
            IF ( Kkper.EQ.1 ) THEN
              READ (In, *) (SEG(jj, nseg), jj=11, 13),
     +                     (SEG(jj, nseg), jj=22, 25)
	        WRITE(IOUT, *) 'HCOND2, THICKM2, ELEVDN,'
     +  // 'THTS2, THTI2, EPS2, UHC2:'
              WRITE(IOUT, *) (SEG(jj, nseg), jj=11, 13),
     +                     (SEG(jj, nseg), jj=22, 25)
            ELSE
              READ (In, *) SEG(11, nseg)
	        WRITE(IOUT, *) 'HCOND2:'
              WRITE(IOUT, *) SEG(11, nseg)
            END IF
          ELSE IF ( icalc.GE.3 .AND. icalc.LE.4 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=11, 13)
	      WRITE(IOUT, *) 'HCOND2, THICKM2, ELEVDN,:'
            WRITE(IOUT, *) (SEG(jj, nseg), jj=11, 13)
          END IF
        END IF
C
C8------READ DATA SET 4E FOR SEGMENT WHEN ICALC IS 2.
        IF ( icalc.EQ.2 ) THEN
C       ADDED CONDITIONAL IF WHEN UNSATURATED FLOW INACTIVE DEP
          IF ( Kkper.EQ.1 .OR. ISFROPT.LE.1 ) THEN
            READ (In, *) (XSEC(jj, nseg), jj=1, 8)
	      WRITE(IOUT,*) 'XCPT1 XCPT2 ... XCPT8:'
            WRITE(IOUT, *) (XSEC(jj, nseg), jj=1, 8)
            READ (In, *) (XSEC(jj, nseg), jj=9, 16)
	      WRITE(IOUT,*) 'ZCPT1 ZCPT2 ... ZCPT8:'
            WRITE(IOUT, *) (XSEC(jj, nseg), jj=9, 16)
          END IF
        END IF
C
C9------READ DATA SET 4F FOR SEGMENT WHEN ICALC IS 4.
        IF ( icalc.EQ.4 ) THEN
          nstrpts = ISEG(2, nseg)
          IF ( nstrpts.LT.2 ) THEN
            WRITE (IOUT, 9004) n
 9004       FORMAT (/1X, 'NUMBER OF POINTS USED TO RELATE ',
     +              'STREAMFLOW WITH STREAM DEPTH AND WIDTH FOR ',
     +              'SEGMENT ', I6, ' IS LESS THAN TWO'//1X,
     +              'PROGRAM STOPPING')
            CALL USTOP(' ')
          ELSE IF ( nstrpts.GT.MAXPTS/3 ) THEN
            WRITE (IOUT, 9005) n, nstrpts
 9005       FORMAT (/1X, 'FOR SEGMENT ', I6, ' NUMBER OF POINTS',
     +              'USED TO RELATE STREAMFLOW WITH DEPTH AND ',
     +              'WIDTH IS ', I5//1X, 'WHICH IS MORE THAN ',
     +              'MAXIMUM NUMBER OF 50 POINTS', //1X,
     +              'PROGRAM STOPPING'//)
            CALL USTOP(' ')
          ELSE
            READ (In, *) (QSTAGE(jj, nseg), jj=1, nstrpts)
	      WRITE(IOUT,*) 'FLOWTAB(1) FLOWTAB(2) ... FLOWTAB(NSTRPTS):'
            WRITE(IOUT, *) (QSTAGE(jj, nseg), jj=1, nstrpts)

            READ (In, *) (QSTAGE(jj, nseg), jj=nstrpts+1, 2*nstrpts)
	      WRITE(IOUT,*) 'DPTHTAB(1) DPTHTAB(2) ... DPTHTAB(NSTRPTS):'
            WRITE(IOUT, *) (QSTAGE(jj, nseg), jj=nstrpts+1, 2*nstrpts)

            READ (In, *) (QSTAGE(jj, nseg), jj=2*nstrpts+1, 3*nstrpts)
	      WRITE(IOUT,*) 'WDTHTAB(1) WDTHTAB(2) ... WDTHTAB(NSTRPTS):'
            WRITE(IOUT, *) (QSTAGE(jj, nseg), jj=2*nstrpts+1, 3*nstrpts)
          END IF
        END IF
C
C10-----READ DATA SET 4G FOR SEGMENT IF SOLUTES SPECIFIED.
        IF ( Iunitgwt.GT.0 ) THEN
          DO isol = 1, Nsol
            IF ( IDIVAR(1, nseg).EQ.0 ) THEN
              READ (In, *) CONCQ(nseg, isol), CONCRUN(nseg, isol),
     +                     CONCPPT(nseg, isol)
	        WRITE(IOUT,*) 'CONCQ(NSOL), CONCRUN(NSOL), CONCPPT(NSOL):'
              WRITE(IOUT, *) CONCQ(nseg, isol), CONCRUN(nseg, isol),
     +                     CONCPPT(nseg, isol)
            ELSE
              READ (In, *) CONCRUN(nseg, isol), CONCPPT(nseg, isol)
	        WRITE(IOUT,*) 'CONCRUN(NSOL), CONCPPT(NSOL):'
              WRITE(IOUT, *) CONCRUN(nseg, isol), CONCPPT(nseg, isol)
            END IF
          END DO
        END IF
C
C10b----READ CELL INDECES THAT RECEIVE RECHARGE: i,1 = ROW, i,2 = COL  !cjm
! RBW. I have skipped this because it is undocumented and appears to be buggy.
      END DO
C
C11-----RETURN.
      RETURN
      END SUBROUTINE SGWF2SFR7RDSEG
C
C-------SUBROUTINE SGWF2SFR7PARMOV
      SUBROUTINE SGWF2SFR7PARMOV(In, Iunitgwt, Nsol)
C     ******************************************************************
C     MOVE STREAM PARAMETER DATA INTO ACTIVE SEGMENTS
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     ******************************************************************
      USE GWFSFRMODULE, ONLY: IDIVAR, IOTSG, ISEG, SEG, XSEC, QSTAGE,
     +                        CONCQ, CONCRUN, CONCPPT, NSEGCK
      USE GLOBAL,       ONLY: IOUT
      USE PARAMMODULE,  ONLY: IACTIVE, IPLOC, PARNAM, INAME, B
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER In, Iunitgwt, Nsol
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      REAL rdum
      INTEGER icalc, idum, iloc, ip, iqseg, isol, istart, istop, iupseg,
     +        jend, jj, ki, lloc, lstend, ni, nlst, nseg, nstrpts,
     +        numinst, lstbeg
      CHARACTER*4 package
      CHARACTER*200 line
      CHARACTER*10 pname, ctmp3, ctmp4
C     ------------------------------------------------------------------
C
      package = 'SFR '
C
C1------READ PARAMETER NAME AND FIND IT IN THE PARAMETER LIST.
      READ (In, '(A)') line
      lloc = 1
      CALL URWORD(line, lloc, istart, istop, 0, idum, rdum, IOUT, In)
      pname = line(istart:istop)
      WRITE (IOUT, *) 'Pname:'
      WRITE (IOUT, *) pname
!      WRITE (IOUT, 9001) pname
! 9001 FORMAT (/, ' Parameter:  ', A)
      CALL UPARFIND(pname, 'SFR', 'SFR', ip, IOUT)
C
C2------DESIGNATE CELLS CORRESPONDING TO CORRECT PARAMETER INSTANCE.
      nlst = IPLOC(2, ip) - IPLOC(1, ip) + 1
      numinst = IPLOC(3, ip)
      iloc = IPLOC(4, ip)
      ni = 1
      IF ( numinst.GT.0 ) THEN
        nlst = nlst/numinst
        CALL URWORD(line, lloc, istart, istop, 0, idum, rdum, IOUT, In)
        ctmp3 = line(istart:istop)
        IF ( ctmp3.EQ.' ' ) THEN
          WRITE (IOUT, 9002) package, PARNAM(ip)
 9002     FORMAT (/, 1X, 'Blank instance name in the ', A,
     +            ' file for parameter ', A)
          CALL USTOP(' ')
        END IF
	  WRITE(IOUT,*) 'Iname:'
	  WRITE(IOUT,*) ctmp3
        WRITE (IOUT, 9003) ctmp3
 9003   FORMAT (3X, 'Instance:  ', A)
        CALL UPCASE(ctmp3)
        DO ki = 1, numinst
          ctmp4 = INAME(iloc+ki-1)
          CALL UPCASE(ctmp4)
          IF ( ctmp3.EQ.ctmp4 ) THEN
            ni = ki
            GOTO 100
          END IF
        END DO
        WRITE (IOUT, 9004) package, ctmp3, PARNAM(ip)
 9004   FORMAT (/, 1X, 'The ', A,
     +          ' file specifies undefined instance "', A,
     +          '" for parameter ', A)
        CALL USTOP(' ')
      END IF
C
 100  IF ( IACTIVE(ip).GT.0 ) THEN
        WRITE (IOUT, 9005) PARNAM(ip)
 9005   FORMAT (/, 1X, '*** ERROR: PARAMETER "', A,
     +          '" HAS ALREADY BEEN ACTIVATED THIS STRESS PERIOD', /,
     +          ' -- STOP EXECUTION (SGWF2SFR7PARMOV)')
        CALL USTOP(' ')
      END IF
C
      IACTIVE(ip) = ni
C
C3------MOVE EACH ENTRY FOR THE PARAMETER.
      lstbeg = IPLOC(1, ip) + (ni-1)*nlst
C changed iqseg to lstbeg in the following line. 4/25/06
      lstend = lstbeg + nlst - 1
      DO iqseg = lstbeg, lstend
C
C4------DETERMINE VALUES OF ICALC, NSEG, AND IUPSEG.
        icalc = ISEG(1, iqseg)
        nseg = ISEG(3, iqseg)
        iupseg = IDIVAR(1, iqseg)
C
C5------COUNT THE NUMBER OF TIMES A SEGMENT IS DEFINED.
        NSEGCK(nseg) = NSEGCK(nseg) + 1
C
C6------MOVE DATA SET 4A.
        ISEG(1, nseg) = ISEG(1, iqseg)
        IOTSG(nseg) = IOTSG(iqseg)
        IDIVAR(1, nseg) = IDIVAR(1, iqseg)
        IF ( iupseg.GT.0 ) IDIVAR(2, nseg) = IDIVAR(2, iqseg)
        SEG(2, nseg) = SEG(2, iqseg)
        SEG(3, nseg) = SEG(3, iqseg)
        SEG(4, nseg) = SEG(4, iqseg)
        SEG(5, nseg) = SEG(5, iqseg)
        IF ( icalc.EQ.1 ) THEN
          SEG(16, nseg) = SEG(16, iqseg)
        ELSE IF ( icalc.EQ.2 ) THEN
          SEG(16, nseg) = SEG(16, iqseg)
          SEG(17, nseg) = SEG(17, iqseg)
        ELSE IF ( icalc.EQ.3 ) THEN
          SEG(9, nseg) = SEG(9, iqseg)
          SEG(10, nseg) = SEG(10, iqseg)
          SEG(14, nseg) = SEG(14, iqseg)
          SEG(15, nseg) = SEG(15, iqseg)
        ELSE IF ( icalc.EQ.4 ) THEN
          ISEG(2, nseg) = ISEG(2, iqseg)
        END IF
C
C7------MOVE DATA SET 4B.
        IF ( icalc.LE.0 ) THEN
          jend = 10
        ELSE IF ( icalc.EQ.1 ) THEN
          jend = 9
        ELSE IF ( icalc.GE.2 .AND. icalc.LE.4 ) THEN
          jend = 8
        END IF
        DO jj = 6, jend
          SEG(jj, nseg) = SEG(jj, iqseg)
        END DO
        SEG(6, nseg) = SEG(6, nseg)*B(ip)
C
C8------MOVE DATA SET 4C.
        IF ( icalc.LE.0 ) THEN
          jend = 15
        ELSE IF ( icalc.EQ.1 ) THEN
          jend = 14
        ELSE IF ( icalc.GE.2 .AND. icalc.LE.4 ) THEN
          jend = 13
        END IF
        DO jj = 11, jend
          SEG(jj, nseg) = SEG(jj, iqseg)
        END DO
        SEG(11, nseg) = SEG(11, nseg)*B(ip)
C
C9------MOVE DATA SET 4D FOR SEGMENT WHEN ICALC IS 2.
        IF ( icalc.EQ.2 ) THEN
          DO jj = 1, 16
            XSEC(jj, nseg) = XSEC(jj, iqseg)
          END DO
        END IF
C
C10-----MOVE DATA SET 4E FOR SEGMENT WHEN ICALC IS 4.
        IF ( icalc.EQ.4 ) THEN
          nstrpts = ISEG(2, nseg)
          DO jj = 1, nstrpts*3
            QSTAGE(jj, nseg) = QSTAGE(jj, iqseg)
          END DO
        END IF
C
C11-----MOVE DATA SET 4F FOR SEGMENT IF SOLUTES SPECIFIED.
        IF ( Iunitgwt.GT.0 ) THEN
          DO isol = 1, Nsol
            IF ( IDIVAR(1, nseg).EQ.0 ) THEN
              CONCQ(nseg, isol) = CONCQ(iqseg, isol)
              CONCRUN(nseg, isol) = CONCRUN(iqseg, isol)
              CONCPPT(nseg, isol) = CONCPPT(iqseg, isol)
            ELSE
              CONCRUN(nseg, isol) = CONCRUN(iqseg, isol)
              CONCPPT(nseg, isol) = CONCPPT(iqseg, isol)
            END IF
          END DO
        END IF
C
      END DO
C12-----RETURN.
      RETURN
      END SUBROUTINE SGWF2SFR7PARMOV
C
C-------SUBROUTINE SGWF2SFR7PRSEG
!      SUBROUTINE SGWF2SFR7PRSEG(Nlst, Lstbeg, Iunitgwt, Kkper, Nsol,
!     +                          Iouts)
C     ******************************************************************
C     PRINT STREAM SEGMENT DATA -- parameters or non parameters
C     VERSION  7.1: JUNE 29, 2006
C     ******************************************************************
C
C-------FUNCTION CALCUNSATFLOBOT written by RGN, MAY 24, 2004
!      REAL FUNCTION CALCUNSATFLOBOT(Depth, Avhc, Fks, Wetperm, Sbdthk,
!     +                              Areamax, Strlen, Fbcheck, Nwavst,
!     +                              Maxwav, Foldflbt)
C-------SUBROUTINE CALC_UNSAT_INFIL written by RGN, MAY 24, 2004
!      SUBROUTINE CALC_UNSAT_INFIL(Flobot, Uzseep, Uzthst, Thr, Ha,
!     +                            Thetas, Epsilon, Fks, Avhc, Depth,
!     +                            Sbdthk, Wetper, Uzwdth, Flow, Nwavst,
!     +                            Strlen, Iwidthcheck, Icalc)
C     ******************************************************************
C     DEFINE UNSATURATED CELLS TO ACCOMMODATE STREAM LOSS.
C     VERSION  7.1: JUNE 29, 2006
C     ******************************************************************
C
C-------SUBROUTINE UZMASSBAL written MAY 24, 2004
!      SUBROUTINE UZMASSBAL(L, H, Hld, Thr, Thetas, Epsilon, Fks, Uzdpst,
!     +                     Uzthst, Uzspst, Uzflst, Ltrlst, Itrlst,
!     +                     Uzflwt, Uzstor, Delstor, Nwavst, Uzolsflx,
!     +                     Uzwdth, Wetper, Uzseep, Ratin, Ratout,
!     +                     Il, Ir, Ic, Flobot, Sbot, Strlen, Totflwt,
!     +                     Totuzstor, Totdelstor, Iwidthcheck, Avdpt,
!     +                     Avwat, Wat1, Ibd, Icalc, Deltinc, Imassroute,
!     +                     Iunitgage, Gwflow)
!rsr unused arguments Kkper, Kkstp, Irt
C     ******************************************************************
C     COMPUTE INFLOW, OUTFLOW, AND CHANGE IN STORAGE IN UNSATURATED
C     ZONE BENEATH STREAMBED.
C     VERSION  7.1: JUNE 29, 2006
C     ******************************************************************
C
C-------SUBROUTINE ROUTWAVESIT
!      SUBROUTINE ROUTWAVESIT(L, Seep, H, Hld, Thr, Thetas, Fks, Epsilon,
!     +                       Icalc, Nwavst, Uzwdth, Uzflwt, Uzolsflx,
!     +                       Uzseep, Itrlst, Ltrlst, Uzspst, Uzflst,
!     +                       Uzdpst, Uzthst, Itrlit, Ltrlit, Uzspit,
!     +                       Uzflit, Uzdpit, Uzthit, Deltinc, Sbot)
C     ******************************************************************
C     ROUTE UNSATURATED ZONE WAVES DURING MODEL ITERATIONS
C     CALLED FROM SUBROUTINE GWF2SFR7FM
C     VERSION  7.1: JUNE 29, 2006
C     ******************************************************************
C
C-------SUBROUTINE ROUTWAVESST
!      SUBROUTINE ROUTWAVESST(L, Seep, H, Hld, Thr, Thetas, Fks, Epsilon,
!     +                       Iwidthcheck, Sbot, Icalc, Nwavst, Uzwdth,
!     +                       Uzflwt, Uzolsflx, Uzseep, Itrlst, Ltrlst,
!     +                       Uzspst, Uzflst, Uzdpst, Uzthst, Deltinc)
C     ******************************************************************
C     ROUTE UNSATURATED-ZONE WAVES AFTER FINAL ITERATION
C     CALLED FROM SUBROUTINE GWF2SFR7BD
C     VERSION  7.1: JUNE 29, 2006
C     ******************************************************************
C
C-------SUBROUTINE UZFLOW
!      SUBROUTINE UZFLOW(I, Surflux, Dlength, Zoldist, Depth, Theta,
!     +                  Flux, Speed, Itrwave, Ltrail, Totalflux,
!     +                  Numwaves, Thetar, Thetas, Fksat, Eps, Oldsflx,
!     +                  Jpnt, Deltinc)
C     ******************************************************************
C     WAVE INTERACTION WITHIN AN UNSATURATED FLOW COMPARTMENT
C     VERSION  7.1: JUNE 29, 2006
C     ******************************************************************
C
C
C-------SUBROUTINE LEADWAVE
!      SUBROUTINE LEADWAVE(Numwaves, Time, Totalflux, Itester, Flux,
!     +                    Theta, Speed, Depth, Itrwave, Ltrail, Fksat,
!     +                    Eps, Thetas, Thetar, Surflux, Oldsflx, Jpnt,
!     +                    Feps2, Itrailflg, Deltinc)
C     ******************************************************************
C     CREATE LEAD WAVE WHEN THE SURFACE FLUX INCREASES AND ROUTE WAVES.
C     VERSION  7.1: JUNE 29, 2006
C     ******************************************************************
C
C
C-------SUBROUTINE TRAILWAVE
!      SUBROUTINE TRAILWAVE(Numwaves, I, Flux, Theta, Speed, Depth,
!     +                     Itrwave, Ltrail, Fksat, Eps, Thetas, Thetar,
!     +                     Surflux, Jpnt)
C     ******************************************************************
C     INITIALIZE A NEW SET OF TRAIL WAVES WHEN SURFACE FLUX DECREASES.
C     VERSION  7.1: JUNE 29, 2006
C     ******************************************************************
C
C-------SUBROUTINE CHANNELAREA
!      SUBROUTINE CHANNELAREA(Istsg, L)
C     ******************************************************************
C     COMPARTMENTALIZE UNSATURATED ZONE BENEATH STREAMBED ON BASIS OF
C     EIGHT POINT CROSS SECTION WHEN ICALC IS 2
C     VERSION  7.1: JUNE 29, 2006
C     ******************************************************************
C
C-------SUBROUTINE ROUTE_CHAN
!      SUBROUTINE ROUTE_CHAN(Qa, Qb, Qc, Qd, Qcnst, Cdpth, Awdth, Fdpth,
!     +                      Bwdth, Deltinc, Icalc, Strlen, Slope, Istsg,
!     +                      Nreach, Itstr, Qlat, Flobot, Width, L)
C***********************************************************************
C     IMPLICIT FINITE-DIFFERENCE SCHEME TO ROUTE FLOW DOWN CHANNELS
C     VERSION  7.1: JUNE 29, 2006
C***********************************************************************
C-------SUBROUTINE GWF2SFR7DA
      SUBROUTINE GWF2SFR7DA(IGRID)
C  Save SFR data for a grid.
      USE GWFSFRMODULE
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER IGRID
C     ------------------------------------------------------------------
      DEALLOCATE (GWFSFRDAT(IGRID)%NSS)
      DEALLOCATE (GWFSFRDAT(IGRID)%NSTRM)
      DEALLOCATE (GWFSFRDAT(IGRID)%NSFRPAR)
      DEALLOCATE (GWFSFRDAT(IGRID)%ISTCB1)
      DEALLOCATE (GWFSFRDAT(IGRID)%ISTCB2)
      DEALLOCATE (GWFSFRDAT(IGRID)%IUZT)
      DEALLOCATE (GWFSFRDAT(IGRID)%MAXPTS)
      DEALLOCATE (GWFSFRDAT(IGRID)%ISFROPT)
      DEALLOCATE (GWFSFRDAT(IGRID)%NSTRAIL)
      DEALLOCATE (GWFSFRDAT(IGRID)%ISUZN)
      DEALLOCATE (GWFSFRDAT(IGRID)%NSFRSETS)
      DEALLOCATE (GWFSFRDAT(IGRID)%NUZST)
      DEALLOCATE (GWFSFRDAT(IGRID)%NSTOTRL)
      DEALLOCATE (GWFSFRDAT(IGRID)%NUMAVE)
      DEALLOCATE (GWFSFRDAT(IGRID)%ITMP)
      DEALLOCATE (GWFSFRDAT(IGRID)%IRDFLG)
      DEALLOCATE (GWFSFRDAT(IGRID)%IPTFLG)
      DEALLOCATE (GWFSFRDAT(IGRID)%NUMTIM)
      DEALLOCATE (GWFSFRDAT(IGRID)%WEIGHT)
      DEALLOCATE (GWFSFRDAT(IGRID)%SFRRATIN)
      DEALLOCATE (GWFSFRDAT(IGRID)%SFRRATOUT)
      DEALLOCATE (GWFSFRDAT(IGRID)%FLWTOL)
      DEALLOCATE (GWFSFRDAT(IGRID)%IRTFLG)
      DEALLOCATE (GWFSFRDAT(IGRID)%NP)
      DEALLOCATE (GWFSFRDAT(IGRID)%CONST)
      DEALLOCATE (GWFSFRDAT(IGRID)%DLEAK)
      DEALLOCATE (GWFSFRDAT(IGRID)%IOTSG)
      DEALLOCATE (GWFSFRDAT(IGRID)%DVRCH)     !cjm
      DEALLOCATE (GWFSFRDAT(IGRID)%DVEFF)     !cjm
      DEALLOCATE (GWFSFRDAT(IGRID)%DVRCELL)   !cjm
      DEALLOCATE (GWFSFRDAT(IGRID)%RECHSAVE)  !cjm
      DEALLOCATE (GWFSFRDAT(IGRID)%DVRPERC)  !cjm
      DEALLOCATE (GWFSFRDAT(IGRID)%NSEGCK)
      DEALLOCATE (GWFSFRDAT(IGRID)%ITRLSTH)
      DEALLOCATE (GWFSFRDAT(IGRID)%ISEG)
      DEALLOCATE (GWFSFRDAT(IGRID)%IDIVAR)
      DEALLOCATE (GWFSFRDAT(IGRID)%ISTRM)
      DEALLOCATE (GWFSFRDAT(IGRID)%LTRLIT)
      DEALLOCATE (GWFSFRDAT(IGRID)%LTRLST)
      DEALLOCATE (GWFSFRDAT(IGRID)%ITRLIT)
      DEALLOCATE (GWFSFRDAT(IGRID)%ITRLST)
      DEALLOCATE (GWFSFRDAT(IGRID)%NWAVST)
      DEALLOCATE (GWFSFRDAT(IGRID)%STRIN)
      DEALLOCATE (GWFSFRDAT(IGRID)%STROUT)
      DEALLOCATE (GWFSFRDAT(IGRID)%FXLKOT)
      DEALLOCATE (GWFSFRDAT(IGRID)%UHC)
      DEALLOCATE (GWFSFRDAT(IGRID)%SGOTFLW)
      DEALLOCATE (GWFSFRDAT(IGRID)%DVRSFLW)
      DEALLOCATE (GWFSFRDAT(IGRID)%SFRUZBD)
      DEALLOCATE (GWFSFRDAT(IGRID)%SEG)
      DEALLOCATE (GWFSFRDAT(IGRID)%STRM)
      DEALLOCATE (GWFSFRDAT(IGRID)%HSTRM)
      DEALLOCATE (GWFSFRDAT(IGRID)%QSTRM)
      DEALLOCATE (GWFSFRDAT(IGRID)%SLKOTFLW)
      DEALLOCATE (GWFSFRDAT(IGRID)%DLKOTFLW)
      DEALLOCATE (GWFSFRDAT(IGRID)%DLKSTAGE)
      DEALLOCATE (GWFSFRDAT(IGRID)%HWDTH)
      DEALLOCATE (GWFSFRDAT(IGRID)%HWTPRM)
      DEALLOCATE (GWFSFRDAT(IGRID)%SFRQ)
      DEALLOCATE (GWFSFRDAT(IGRID)%QSTAGE)
      DEALLOCATE (GWFSFRDAT(IGRID)%XSEC)
      DEALLOCATE (GWFSFRDAT(IGRID)%AVDPT)
      DEALLOCATE (GWFSFRDAT(IGRID)%AVWAT)
      DEALLOCATE (GWFSFRDAT(IGRID)%WAT1)
      DEALLOCATE (GWFSFRDAT(IGRID)%CONCQ)
      DEALLOCATE (GWFSFRDAT(IGRID)%CONCRUN)
      DEALLOCATE (GWFSFRDAT(IGRID)%CONCPPT)
      DEALLOCATE (GWFSFRDAT(IGRID)%THTS)
      DEALLOCATE (GWFSFRDAT(IGRID)%THTR)
      DEALLOCATE (GWFSFRDAT(IGRID)%EPS)
      DEALLOCATE (GWFSFRDAT(IGRID)%FOLDFLBT)
      DEALLOCATE (GWFSFRDAT(IGRID)%THTI)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZFLWT)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZSTOR)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZWDTH)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZSEEP)
      DEALLOCATE (GWFSFRDAT(IGRID)%DELSTOR)
      DEALLOCATE (GWFSFRDAT(IGRID)%WETPER)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZDPIT)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZDPST)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZTHIT)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZTHST)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZSPIT)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZSPST)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZFLIT)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZFLST)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZOLSFLX)
      DEALLOCATE (GWFSFRDAT(IGRID)%SUMRCH)
      DEALLOCATE (GWFSFRDAT(IGRID)%SUMLEAK)
      DEALLOCATE (GWFSFRDAT(IGRID)%HLDSFR)
      DEALLOCATE (GWFSFRDAT(IGRID)%STRMDELSTOR_CUM)
      DEALLOCATE (GWFSFRDAT(IGRID)%STRMDELSTOR_RATE)
      DEALLOCATE (GWFSFRDAT(IGRID)%TOTSPFLOW)
      DEALLOCATE (GWFSFRDAT(IGRID)%TABFLOW)
      DEALLOCATE (GWFSFRDAT(IGRID)%TABTIME)
      DEALLOCATE (GWFSFRDAT(IGRID)%ISFRLIST)
      DEALLOCATE (GWFSFRDAT(IGRID)%FNETSEEP)
      DEALLOCATE (GWFSFRDAT(IGRID)%NSEGDIM)
C
      END SUBROUTINE GWF2SFR7DA
C
C-------SUBROUTINE GWF2SFR7PNT
      SUBROUTINE SGWF2SFR7PNT(IGRID)
C  Change SFR data to a different grid.
      USE GWFSFRMODULE
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER IGRID
C     ------------------------------------------------------------------
      NSS=>GWFSFRDAT(IGRID)%NSS
      NSTRM=>GWFSFRDAT(IGRID)%NSTRM
      NSFRPAR=>GWFSFRDAT(IGRID)%NSFRPAR
      ISTCB1=>GWFSFRDAT(IGRID)%ISTCB1
      ISTCB2=>GWFSFRDAT(IGRID)%ISTCB2
      IUZT=>GWFSFRDAT(IGRID)%IUZT
      MAXPTS=>GWFSFRDAT(IGRID)%MAXPTS
      ISFROPT=>GWFSFRDAT(IGRID)%ISFROPT
      NSTRAIL=>GWFSFRDAT(IGRID)%NSTRAIL
      ISUZN=>GWFSFRDAT(IGRID)%ISUZN
      NSFRSETS=>GWFSFRDAT(IGRID)%NSFRSETS
      NUZST=>GWFSFRDAT(IGRID)%NUZST
      NSTOTRL=>GWFSFRDAT(IGRID)%NSTOTRL
      NUMAVE=>GWFSFRDAT(IGRID)%NUMAVE
      ITMP=>GWFSFRDAT(IGRID)%ITMP
      IRDFLG=>GWFSFRDAT(IGRID)%IRDFLG
      IPTFLG=>GWFSFRDAT(IGRID)%IPTFLG
      NP=>GWFSFRDAT(IGRID)%NP
      CONST=>GWFSFRDAT(IGRID)%CONST
      DLEAK=>GWFSFRDAT(IGRID)%DLEAK
      NUMTIM=>GWFSFRDAT(IGRID)%NUMTIM
      WEIGHT=>GWFSFRDAT(IGRID)%WEIGHT
      SFRRATIN=>GWFSFRDAT(IGRID)%SFRRATIN
      SFRRATOUT=>GWFSFRDAT(IGRID)%SFRRATOUT
      FLWTOL=>GWFSFRDAT(IGRID)%FLWTOL
      IRTFLG=>GWFSFRDAT(IGRID)%IRTFLG
      IOTSG=>GWFSFRDAT(IGRID)%IOTSG
      DVRCH=>GWFSFRDAT(IGRID)%DVRCH        !cjm
      DVEFF=>GWFSFRDAT(IGRID)%DVEFF        !cjm
      DVRCELL=>GWFSFRDAT(IGRID)%DVRCELL    !cjm
      RECHSAVE=>GWFSFRDAT(IGRID)%RECHSAVE  !cjm
      DVRPERC=>GWFSFRDAT(IGRID)%DVRPERC  !cjm
      NSEGCK=>GWFSFRDAT(IGRID)%NSEGCK
      ITRLSTH=>GWFSFRDAT(IGRID)%ITRLSTH
      ISEG=>GWFSFRDAT(IGRID)%ISEG
      IDIVAR=>GWFSFRDAT(IGRID)%IDIVAR
      ISTRM=>GWFSFRDAT(IGRID)%ISTRM
      LTRLIT=>GWFSFRDAT(IGRID)%LTRLIT
      LTRLST=>GWFSFRDAT(IGRID)%LTRLST
      ITRLIT=>GWFSFRDAT(IGRID)%ITRLIT
      ITRLST=>GWFSFRDAT(IGRID)%ITRLST
      NWAVST=>GWFSFRDAT(IGRID)%NWAVST
      STRIN=>GWFSFRDAT(IGRID)%STRIN
      STROUT=>GWFSFRDAT(IGRID)%STROUT
      FXLKOT=>GWFSFRDAT(IGRID)%FXLKOT
      UHC=>GWFSFRDAT(IGRID)%UHC
      SGOTFLW=>GWFSFRDAT(IGRID)%SGOTFLW
      DVRSFLW=>GWFSFRDAT(IGRID)%DVRSFLW
      SFRUZBD=>GWFSFRDAT(IGRID)%SFRUZBD
      SEG=>GWFSFRDAT(IGRID)%SEG
      STRM=>GWFSFRDAT(IGRID)%STRM
      HSTRM=>GWFSFRDAT(IGRID)%HSTRM
      QSTRM=>GWFSFRDAT(IGRID)%QSTRM
      HWDTH=>GWFSFRDAT(IGRID)%HWDTH
      HWTPRM=>GWFSFRDAT(IGRID)%HWTPRM
      SFRQ=>GWFSFRDAT(IGRID)%SFRQ
      QSTAGE=>GWFSFRDAT(IGRID)%QSTAGE
      SLKOTFLW=>GWFSFRDAT(IGRID)%SLKOTFLW
      DLKOTFLW=>GWFSFRDAT(IGRID)%DLKOTFLW
      DLKSTAGE=>GWFSFRDAT(IGRID)%DLKSTAGE
      XSEC=>GWFSFRDAT(IGRID)%XSEC
      AVDPT=>GWFSFRDAT(IGRID)%AVDPT
      AVWAT=>GWFSFRDAT(IGRID)%AVWAT
      WAT1=>GWFSFRDAT(IGRID)%WAT1
      CONCQ=>GWFSFRDAT(IGRID)%CONCQ
      CONCRUN=>GWFSFRDAT(IGRID)%CONCRUN
      CONCPPT=>GWFSFRDAT(IGRID)%CONCPPT
      THTS=>GWFSFRDAT(IGRID)%THTS
      THTR=>GWFSFRDAT(IGRID)%THTR
      EPS=>GWFSFRDAT(IGRID)%EPS
      FOLDFLBT=>GWFSFRDAT(IGRID)%FOLDFLBT
      THTI=>GWFSFRDAT(IGRID)%THTI
      UZFLWT=>GWFSFRDAT(IGRID)%UZFLWT
      UZSTOR=>GWFSFRDAT(IGRID)%UZSTOR
      UZWDTH=>GWFSFRDAT(IGRID)%UZWDTH
      UZSEEP=>GWFSFRDAT(IGRID)%UZSEEP
      DELSTOR=>GWFSFRDAT(IGRID)%DELSTOR
      WETPER=>GWFSFRDAT(IGRID)%WETPER
      UZDPIT=>GWFSFRDAT(IGRID)%UZDPIT
      UZDPST=>GWFSFRDAT(IGRID)%UZDPST
      UZTHIT=>GWFSFRDAT(IGRID)%UZTHIT
      UZTHST=>GWFSFRDAT(IGRID)%UZTHST
      UZSPIT=>GWFSFRDAT(IGRID)%UZSPIT
      UZSPST=>GWFSFRDAT(IGRID)%UZSPST
      UZFLIT=>GWFSFRDAT(IGRID)%UZFLIT
      UZFLST=>GWFSFRDAT(IGRID)%UZFLST
      UZOLSFLX=>GWFSFRDAT(IGRID)%UZOLSFLX
      SUMRCH=>GWFSFRDAT(IGRID)%SUMRCH
      SUMLEAK=>GWFSFRDAT(IGRID)%SUMLEAK
      HLDSFR=>GWFSFRDAT(IGRID)%HLDSFR
      STRMDELSTOR_CUM=>GWFSFRDAT(IGRID)%STRMDELSTOR_CUM
      STRMDELSTOR_RATE=>GWFSFRDAT(IGRID)%STRMDELSTOR_RATE
      TOTSPFLOW=>GWFSFRDAT(IGRID)%TOTSPFLOW
      TABFLOW=>GWFSFRDAT(IGRID)%TABFLOW
      TABTIME=>GWFSFRDAT(IGRID)%TABTIME
      ISFRLIST=>GWFSFRDAT(IGRID)%ISFRLIST
      FNETSEEP=>GWFSFRDAT(IGRID)%FNETSEEP
      NSEGDIM=>GWFSFRDAT(IGRID)%NSEGDIM
C
      END SUBROUTINE SGWF2SFR7PNT
C
C-------SUBROUTINE SGWF2SFR7PSV
      SUBROUTINE SGWF2SFR7PSV(IGRID)
C  Save SFR data for a grid.
      USE GWFSFRMODULE
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER IGRID
C     ------------------------------------------------------------------
      GWFSFRDAT(IGRID)%NSS=>NSS
      GWFSFRDAT(IGRID)%NSTRM=>NSTRM
      GWFSFRDAT(IGRID)%NSFRPAR=>NSFRPAR
      GWFSFRDAT(IGRID)%ISTCB1=>ISTCB1
      GWFSFRDAT(IGRID)%ISTCB2=>ISTCB2
      GWFSFRDAT(IGRID)%IUZT=>IUZT
      GWFSFRDAT(IGRID)%MAXPTS=>MAXPTS
      GWFSFRDAT(IGRID)%ISFROPT=>ISFROPT
      GWFSFRDAT(IGRID)%NSTRAIL=>NSTRAIL
      GWFSFRDAT(IGRID)%ISUZN=>ISUZN
      GWFSFRDAT(IGRID)%NSFRSETS=>NSFRSETS
      GWFSFRDAT(IGRID)%NUZST=>NUZST
      GWFSFRDAT(IGRID)%NSTOTRL=>NSTOTRL
      GWFSFRDAT(IGRID)%NUMAVE=>NUMAVE
      GWFSFRDAT(IGRID)%ITMP=>ITMP
      GWFSFRDAT(IGRID)%IRDFLG=>IRDFLG
      GWFSFRDAT(IGRID)%IPTFLG=>IPTFLG
      GWFSFRDAT(IGRID)%NP=>NP
      GWFSFRDAT(IGRID)%CONST=>CONST
      GWFSFRDAT(IGRID)%DLEAK=>DLEAK
      GWFSFRDAT(IGRID)%NUMTIM=>NUMTIM
      GWFSFRDAT(IGRID)%WEIGHT=>WEIGHT
      GWFSFRDAT(IGRID)%SFRRATIN=>SFRRATIN
      GWFSFRDAT(IGRID)%SFRRATOUT=>SFRRATOUT
      GWFSFRDAT(IGRID)%FLWTOL=>FLWTOL
      GWFSFRDAT(IGRID)%IRTFLG=>IRTFLG
      GWFSFRDAT(IGRID)%IOTSG=>IOTSG
      GWFSFRDAT(IGRID)%DVRCH=>DVRCH        !cjm
      GWFSFRDAT(IGRID)%DVEFF=>DVEFF        !cjm
      GWFSFRDAT(IGRID)%DVRCELL=>DVRCELL    !cjm
      GWFSFRDAT(IGRID)%DVRPERC=>DVRPERC  !cjm
      GWFSFRDAT(IGRID)%RECHSAVE=>RECHSAVE  !cjm
      GWFSFRDAT(IGRID)%NSEGCK=>NSEGCK
      GWFSFRDAT(IGRID)%ITRLSTH=>ITRLSTH
      GWFSFRDAT(IGRID)%ISEG=>ISEG
      GWFSFRDAT(IGRID)%IDIVAR=>IDIVAR
      GWFSFRDAT(IGRID)%ISTRM=>ISTRM
      GWFSFRDAT(IGRID)%LTRLIT=>LTRLIT
      GWFSFRDAT(IGRID)%LTRLST=>LTRLST
      GWFSFRDAT(IGRID)%ITRLIT=>ITRLIT
      GWFSFRDAT(IGRID)%ITRLST=>ITRLST
      GWFSFRDAT(IGRID)%NWAVST=>NWAVST
      GWFSFRDAT(IGRID)%STRIN=>STRIN
      GWFSFRDAT(IGRID)%STROUT=>STROUT
      GWFSFRDAT(IGRID)%FXLKOT=>FXLKOT
      GWFSFRDAT(IGRID)%UHC=>UHC
      GWFSFRDAT(IGRID)%SGOTFLW=>SGOTFLW
      GWFSFRDAT(IGRID)%DVRSFLW=>DVRSFLW
      GWFSFRDAT(IGRID)%SFRUZBD=>SFRUZBD
      GWFSFRDAT(IGRID)%SEG=>SEG
      GWFSFRDAT(IGRID)%STRM=>STRM
      GWFSFRDAT(IGRID)%HSTRM=>HSTRM
      GWFSFRDAT(IGRID)%QSTRM=>QSTRM
      GWFSFRDAT(IGRID)%SLKOTFLW=>SLKOTFLW
      GWFSFRDAT(IGRID)%DLKOTFLW=>DLKOTFLW
      GWFSFRDAT(IGRID)%DLKSTAGE=>DLKSTAGE
      GWFSFRDAT(IGRID)%HWDTH=>HWDTH
      GWFSFRDAT(IGRID)%HWTPRM=>HWTPRM
      GWFSFRDAT(IGRID)%SFRQ=>SFRQ
      GWFSFRDAT(IGRID)%QSTAGE=>QSTAGE
      GWFSFRDAT(IGRID)%XSEC=>XSEC
      GWFSFRDAT(IGRID)%AVDPT=>AVDPT
      GWFSFRDAT(IGRID)%AVWAT=>AVWAT
      GWFSFRDAT(IGRID)%WAT1=>WAT1
      GWFSFRDAT(IGRID)%CONCQ=>CONCQ
      GWFSFRDAT(IGRID)%CONCRUN=>CONCRUN
      GWFSFRDAT(IGRID)%CONCPPT=>CONCPPT
      GWFSFRDAT(IGRID)%THTS=>THTS
      GWFSFRDAT(IGRID)%THTR=>THTR
      GWFSFRDAT(IGRID)%EPS=>EPS
      GWFSFRDAT(IGRID)%FOLDFLBT=>FOLDFLBT
      GWFSFRDAT(IGRID)%THTI=>THTI
      GWFSFRDAT(IGRID)%UZFLWT=>UZFLWT
      GWFSFRDAT(IGRID)%UZSTOR=>UZSTOR
      GWFSFRDAT(IGRID)%UZWDTH=>UZWDTH
      GWFSFRDAT(IGRID)%UZSEEP=>UZSEEP
      GWFSFRDAT(IGRID)%DELSTOR=>DELSTOR
      GWFSFRDAT(IGRID)%WETPER=>WETPER
      GWFSFRDAT(IGRID)%UZDPIT=>UZDPIT
      GWFSFRDAT(IGRID)%UZDPST=>UZDPST
      GWFSFRDAT(IGRID)%UZTHIT=>UZTHIT
      GWFSFRDAT(IGRID)%UZTHST=>UZTHST
      GWFSFRDAT(IGRID)%UZSPIT=>UZSPIT
      GWFSFRDAT(IGRID)%UZSPST=>UZSPST
      GWFSFRDAT(IGRID)%UZFLIT=>UZFLIT
      GWFSFRDAT(IGRID)%UZFLST=>UZFLST
      GWFSFRDAT(IGRID)%UZOLSFLX=>UZOLSFLX
      GWFSFRDAT(IGRID)%SUMRCH=>SUMRCH
      GWFSFRDAT(IGRID)%SUMLEAK=>SUMLEAK
      GWFSFRDAT(IGRID)%HLDSFR=>HLDSFR
      GWFSFRDAT(IGRID)%STRMDELSTOR_CUM=>STRMDELSTOR_CUM
      GWFSFRDAT(IGRID)%STRMDELSTOR_RATE=>STRMDELSTOR_RATE
      GWFSFRDAT(IGRID)%TOTSPFLOW=>TOTSPFLOW
      GWFSFRDAT(IGRID)%TABFLOW=>TABFLOW
      GWFSFRDAT(IGRID)%TABTIME=>TABTIME
      GWFSFRDAT(IGRID)%ISFRLIST=>ISFRLIST
      GWFSFRDAT(IGRID)%FNETSEEP=>FNETSEEP
      GWFSFRDAT(IGRID)%NSEGDIM=>NSEGDIM
C
      END SUBROUTINE SGWF2SFR7PSV
