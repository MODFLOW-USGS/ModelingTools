C                  KJH  20030327      -- Patched Hyd.K term in LPF option -- cel2wel function
C                  KJH  20030717      -- Patched budget output switch -- subroutine GWF1MNW1bd
c                                        Cleaned output so outrageous pointers are not printed
c                  GZH  20050405      -- Converted calculations to use double precision
c                  KJH  20050419      -- Array WELL2 dimensioned to 18 to store well id
C                  AWH  20080411      -- Retrieve HDRY from GWFBASMODULE rather than from
C                                        LPF, BCF, or HUF
C                  RGN  20111001      -- Modified to support UPW; removed BC switching is UPW is used.
c
      MODULE GWFMNW1MODULE
        DOUBLE PRECISION, PARAMETER :: TWOPI=2.0D0*3.1415926535897932D0
        DOUBLE PRECISION, PARAMETER :: ZERO25=1.0D-25, ZERO20=1.0D-20
        DOUBLE PRECISION, PARAMETER :: ZERO8=1.0D-8, BIG=1.0D30
        CHARACTER(LEN=200),SAVE,POINTER:: MNWNAME
        INTEGER,          SAVE,POINTER :: NWELL2, MXWEL2, IWL2CB, KSPREF
        INTEGER,          SAVE,POINTER :: IWELPT, NOMOITER
        DOUBLE PRECISION, SAVE,POINTER :: PLOSS
        DOUBLE PRECISION, SAVE,POINTER :: SMALL, HMAX
        CHARACTER(LEN=32),SAVE,DIMENSION(:),    POINTER :: MNWSITE
        INTEGER,          SAVE,DIMENSION(:),    POINTER :: IOWELL2
        DOUBLE PRECISION, SAVE,DIMENSION(:,:),  POINTER :: WELL2
        DOUBLE PRECISION, SAVE,DIMENSION(:,:,:),POINTER :: HREF
      TYPE GWFMNWTYPE
        CHARACTER(LEN=200),    POINTER :: MNWNAME
        INTEGER,               POINTER :: NWELL2, MXWEL2, IWL2CB, KSPREF
        INTEGER,               POINTER :: IWELPT, NOMOITER
        DOUBLE PRECISION,      POINTER :: PLOSS
        DOUBLE PRECISION,      POINTER :: SMALL, HMAX
        CHARACTER(LEN=32),     DIMENSION(:),    POINTER :: MNWSITE
        INTEGER,               DIMENSION(:),    POINTER :: IOWELL2
        DOUBLE PRECISION,      DIMENSION(:,:),  POINTER :: WELL2
        DOUBLE PRECISION,      DIMENSION(:,:,:),POINTER :: HREF
      END TYPE
      TYPE(GWFMNWTYPE), SAVE:: GWFMNWDAT(10)
      END MODULE GWFMNW1MODULE
C
c-------------------------------------------------------------------------
c
      SUBROUTINE GWF2MNW17AR(In, Iusip, Iude4, Iunwt, Iusor, Iupcg, 
     +                      Iulmg, Iugmg, Fname, Igrid)
!rgn------REVISION NUMBER CHANGED TO BE CONSISTENT WITH NWT RELEASE
!rgn------NEW VERSION NUMBER 1.0.5:  April 5, 2012
C     VERSION 20020819 KJH
c
c----- MNW by K.J. Halford        1/31/98
c     ******************************************************************
c     allocate array storage for well package
c     ******************************************************************
c
c        specifications:
c     ------------------------------------------------------------------
      USE GLOBAL,      ONLY: IOUT,NCOL,NROW,NLAY
      USE GWFMNW1MODULE
      USE SIPMODULE,ONLY:HCLOSE
      USE DE4MODULE,ONLY:HCLOSEDE4
      USE PCGMODULE,ONLY:HCLOSEPCG
      USE GMGMODULE,ONLY:HCLOSEGMG
      USE GWFNWTMODULE,ONLY:Tol
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INTRINSIC ABS
      INTEGER, EXTERNAL :: IFRL
      EXTERNAL NCREAD, UPCASE, QREAD, USTOP
c     ------------------------------------------------------------------
c     Arguments
c     ------------------------------------------------------------------
      INTEGER :: In, Iusip, Iude4, Iusor, Iupcg, Iulmg, Iugmg, 
     +           Iunwt, Igrid
      CHARACTER(LEN=200) :: Fname                 !!08/19/02KJH-MODIFIED
c     ------------------------------------------------------------------
c     Local Variables
c     ------------------------------------------------------------------
      REAL :: bs
      INTEGER :: ierr, io, iok, jf, ke, kf, ki, kio
      DOUBLE PRECISION :: rn(25)
      CHARACTER(LEN=256) :: txt, tx2
c     ------------------------------------------------------------------
c     Static Variables
c     ------------------------------------------------------------------
      CHARACTER(LEN=6) :: ftag(3)
      INTEGER :: icf(3)
      DATA ftag/'WEL1  ', 'BYNODE', 'QSUM  '/
      DATA icf/4, 6, 4/
c     ------------------------------------------------------------------
      ALLOCATE (MNWNAME, NWELL2, MXWEL2, IWL2CB, NOMOITER, KSPREF, 
     +          IWELPT)
      ALLOCATE (PLOSS, SMALL, HMAX, IOWELL2(3), 
     +          HREF(NCOL,NROW,NLAY))
c
      IOWELL2(1) = 0
      IOWELL2(2) = 0
      IOWELL2(3) = 0
c
c1------identify package and initialize nwell2
      WRITE (IOUT, *) 'MNW1:'
!      WRITE (IOUT, 9001) In
! 9001 FORMAT (/, ' MNW1 -- MULTI-NODE WELL 1 PACKAGE, VERSION 7,', 
!     +        ' 11/07/2005.', /, '    INPUT READ FROM UNIT', i4)
      NWELL2 = 0
c
c2------read max number of wells and
c2------unit or flag for cell-by-cell flow terms.
      CALL NCREAD(In, txt, ierr)
      CALL UPCASE(txt)
c
      ki = INDEX(txt, 'REF')
      IF ( ki.GT.0 ) THEN
        tx2 = txt(ki:256)
        CALL QREAD(rn, 1, tx2, ierr)
        IF ( ierr.EQ.0 ) KSPREF = IFRL(rn(1))
        txt(ki:256) = '                                '
      ELSE
        KSPREF = 1
      ENDIF
c
      CALL QREAD(rn, 4, txt, ierr)
      MXWEL2 = IFRL(rn(1))
      IWL2CB = 0
      IF ( ierr.LE.2 ) IWL2CB = IFRL(rn(2))
      IWELPT = 0
      IF ( ierr.EQ.1 ) IWELPT = IFRL(rn(3))
      NOMOITER = 9999
      IF ( ierr.EQ.0 ) NOMOITER = IFRL(rn(4))
	WRITE(IOUT,*) 'MXMNW IWL2CB IWELPT NOMOITER KSPREF:'
	WRITE(IOUT,*) MXWEL2, IWL2CB, IWELPT, NOMOITER, KSPREF 
c
!      WRITE (IOUT, 9002) MXWEL2
!      IF ( IWL2CB.GT.0 ) WRITE (IOUT, 9003) IWL2CB
!      IF ( IWL2CB.LT.0 ) WRITE (IOUT, 9004)
!      WRITE (IOUT, 9005) KSPREF
!      WRITE (IOUT, 9006) NOMOITER
! 9002 FORMAT (' MAXIMUM OF', i5, ' WELLS')
! 9003 FORMAT (' CELL-BY-CELL FLOWS WILL BE RECORDED ON UNIT', i3)
! 9004 FORMAT (' CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')
! 9005 FORMAT ('  The heads at the beginning of SP:', i4, 
!     +        ' will be the default reference elevations.', /)
! 9006 FORMAT (' Flow rates will not be estimated after the', i4, 
!     +        'th iteration')
c
c   Define well model to be used
c
      CALL NCREAD(In, txt, ierr)
      CALL UPCASE(txt)
      PLOSS = 0.0D0   !!  Default use of Skin so linear loss varies with T
      IF ( INDEX(txt, 'LINEAR').GT.0 ) THEN
        PLOSS = 1.0D0 !!  ADD THIS LINE to make sure that the power term is 1 for the linear model
        ki = INDEX(txt, ':') + 1
        tx2 = txt(ki:256)
        CALL QREAD(rn, 1, tx2, ierr)
        IF ( ierr.EQ.0 ) PLOSS = rn(1)
c   Add error checking to shut down MODFLOW
        bs = 3.6           !!   Maximum limit on power term
        IF ( PLOSS.GT.bs ) THEN
          WRITE (*, *) 'Power term of', PLOSS, ' exceeds maximum of', bs
          WRITE (IOUT, *) 'Power term of', PLOSS, ' exceeds maximum of',
     +                    bs
C
C         When compiling MNW with Modflow-96, comment out the call to
C         USTOP and uncomment the STOP statement
          CALL USTOP(' ')
C         STOP
C
        ENDIF
c
      ENDIF
	IF ( INDEX(txt, 'NONLINEAR').GT.0 ) THEN
	  WRITE(IOUT,*) 'LOSSTYPE PLossMNW:'
	  WRITE(IOUT,*) 'NONLINEAR'
	  WRITE(IOUT,*) PLOSS
	ELSEIF ( INDEX(txt, 'LINEAR').GT.0 ) THEN
	  WRITE(IOUT,*) 'LOSSTYPE PLossMNW:'
	  WRITE(IOUT,*) 'LINEAR'
	  WRITE(IOUT,*) PLOSS
	ELSE
	  WRITE(IOUT,*) 'LOSSTYPE:'
	  WRITE(IOUT,*) 'SKIN'
	ENDIF
c
c   Test for a specified PREFIX NAME  for time series output from MNW7OT
c
      CALL NCREAD(In, txt, ierr)
      tx2 = txt
      CALL UPCASE(tx2)
      kf = INDEX(tx2, 'PREFIX:')
      IF ( kf.GT.0 ) THEN
        MNWNAME = txt(kf+7:256)
        ke = INDEX(MNWNAME, ' ')
        MNWNAME(ke:200) = '               '
        tx2 = MNWNAME
        CALL UPCASE(tx2)
        IF ( INDEX(tx2, 'FILEPREFIX').GT.0 ) THEN
          MNWNAME = Fname
          ke = INDEX(MNWNAME, '.')
          MNWNAME(ke:200) = '               '
        ENDIF
      ELSE
        MNWNAME = 'OUTput_MNW'
        BACKSPACE (In)
      ENDIF
	WRITE(IOUT, *) 'MNWNAME:'
	WRITE(IOUT, *) MNWNAME
c
c     Test for creation of a WEL1 package and auxillary output files
c
      iok = 1
      DO WHILE ( iok.EQ.1 )
        CALL NCREAD(In, txt, ierr)
        tx2 = txt
        CALL UPCASE(tx2)
        kf = INDEX(tx2, 'FILE:')
        IF ( kf.GT.0 ) THEN
          kio = 0
          jf = 0
          DO WHILE ( kio.EQ.0 .AND. jf.LT.3 )
            jf = jf + 1
            kio = INDEX(tx2, ftag(jf)(1:icf(jf)))
            IF ( kio.GT.0 ) THEN
              tx2 = txt(kio+1+icf(jf):256)
              CALL QREAD(rn, 1, tx2, ierr)
              IF ( ierr.EQ.0 ) THEN
                IOWELL2(jf) = IFRL(rn(1))
c            OC over ride is ALLTIME
                IF ( INDEX(tx2, 'ALLTIME').GT.0 ) IOWELL2(jf)
     +               = -IOWELL2(jf)
c            Find and use file name
                tx2 = txt(kf+5:256)
                kf = INDEX(tx2, ' ') - 1
                CLOSE (ABS(IOWELL2(jf)))
                OPEN (ABS(IOWELL2(jf)), FILE=tx2(1:kf))
                WRITE (tx2(253:256), '(i4)') ABS(IOWELL2(jf))
                txt = ' A '//ftag(jf)
     +                //' data input file will be written'//' to '//
     +                tx2(1:kf)//' on unit '//tx2(253:256)
!                WRITE (IOUT, '(/1x,a79)') txt
                IF ( jf.EQ.1 )
     +          WRITE (ABS(IOWELL2(jf)),'(3i10)') MXWEL2, IWL2CB, IWELPT
                WRITE(IOUT, *) 'MNW DATA SET 3:'
	          IF (IOWELL2(jf).LT.0) THEN
	            WRITE(IOUT, *) tx2(1:kf) 
      			  WRITE(IOUT, *) ftag(jf)(1:icf(jf))
      			  WRITE(IOUT, *) ABS(IOWELL2(jf))
      			  WRITE(IOUT, *) , 'ALLTIME:'
	          ELSE
	            WRITE(IOUT, *) tx2(1:kf)
     		  	      WRITE(IOUT, *) ftag(jf)(1:icf(jf))
			      WRITE(IOUT, *) ABS(IOWELL2(jf))
	          ENDIF
              ENDIF
            ENDIF
          ENDDO
        ELSE
          BACKSPACE (In)
          iok = 0
        ENDIF
      ENDDO
c
c  Write header in Auxillary BYNODE file if KPER=1 & IO>0
c
      IF ( IOWELL2(2).NE.0 ) THEN
        io = ABS(IOWELL2(2))
        WRITE (io, 9008)
      ENDIF
c
c  Write header in Auxillary QSUM file if KPER=1 & IO>0
c
      IF ( IOWELL2(3).NE.0 ) THEN
        io = ABS(IOWELL2(3))
        WRITE (io, 9009)
      ENDIF
c
 9008 FORMAT ('SiteID', 27x, 'Entry  NODE', 5x, 'Total_Time', 8x, 'Q', 
     +        5x, 'H-Well', 5x, 'H-Cell', 5x, 'QW-Avg')
 9009 FORMAT ('SiteID', 31x, 'Entry', 5x, 'Total_Time', 10x, 'Qin', 
     +        10x, 'Qout', 10x, 'Qsum', 5x, 'H-Well', 5x, 'QW-Avg')
c
C  4/18/2005 - KJH:  Explicit well tracking addition changed 1st WELL2
C                    dimension from 17 to 18
      ALLOCATE (WELL2(18, MXWEL2+1), MNWSITE(MXWEL2))
c
C-------SET SMALL DEPENDING ON CLOSURE CRITERIA OF THE SOLVER
      SMALL = 0.0D0
      IF ( Iusip.NE.0 ) SMALL = HCLOSE
      IF ( Iude4.NE.0 ) SMALL = HCLOSEDE4
!     IF ( Iusor.NE.0 ) SMALL = HCLOSESOR
      IF ( Iupcg.NE.0 ) SMALL = HCLOSEPCG
      IF ( Iulmg.NE.0 ) SMALL = 0.0D0  !LMG SETS HCLOSE TO ZERO
      IF ( Iugmg.NE.0 ) SMALL = HCLOSEGMG
      IF ( Iunwt.NE.0 ) SMALL = TOL
c
c-----SAVE POINTERS FOR GRID AND RETURN
      CALL SGWF2MNW1PSV(Igrid)
c
c7------return
      END SUBROUTINE GWF2MNW17AR
c
c_________________________________________________________________________________
c
      SUBROUTINE GWF2MNW17RP(In, Iubcf, Iulpf, Iuhuf, Iuupw, Kper, 
     +                       Igrid)
c     VERSION 20020819 KJH
C
c----- MNW by K.J. Halford        1/31/98
c     ******************************************************************
c     read new well locations, stress rates, conc, well char., and limits
c     ******************************************************************
c
c        specifications:
c     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NODES,NCOL,NROW,NLAY,IBOUND,HOLD,HNEW,IOUT
      USE GWFBASMODULE,ONLY:TOTIM,HDRY
      USE GWFMNW1MODULE, ONLY:NWELL2,MXWEL2,IWELPT,PLOSS,HMAX,
     1                       MNWSITE,IOWELL2,WELL2,HREF,KSPREF,
     2                       BIG,ZERO25
      IMPLICIT NONE
      INTRINSIC ABS, MAX, MOD, INT
      INTEGER, EXTERNAL :: IFRL, IDIRECT
      DOUBLE PRECISION, EXTERNAL :: CEL2WELBCF, CEL2WELLPF, CEL2WELHUF, 
     +                              CEL2WELUPW
      EXTERNAL NCREAD, UPCASE, QREAD, USTOP
c     ------------------------------------------------------------------
c     Arguments
c     ------------------------------------------------------------------
      INTEGER, INTENT(IN) :: Iubcf, Iulpf, Iuhuf, Kper, Igrid, Iuupw
      INTEGER, INTENT(INOUT) :: In
c     ------------------------------------------------------------------
c     Local Variables
c     ------------------------------------------------------------------
      DOUBLE PRECISION :: qfrcmn, qfrcmx, qreject, drytest, ipole, hlim
      DOUBLE PRECISION :: hrfw, qsum, rw, cond, qact, sk, cf, q, rn(25)
      INTEGER :: i, icmn, ierr, igrp, ii, iin, io, iok, ip, ipt, irmx
      INTEGER :: itmp, j, k, kblk, kcp, kfini, ki, kpc, kqc, ksiteid
      INTEGER :: ktab, m, mstep, n, n1, nb, ne, ngrp, nl, nn, node
      INTEGER :: nqreject, nstart
      INTEGER :: idwell,mm
      CHARACTER(LEN=1) :: tab
      CHARACTER(LEN=32) :: tempsite
      CHARACTER(LEN=256) :: txt, tx2, txtraw
c     ------------------------------------------------------------------
c-----SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2MNW1PNT(Igrid)
	WRITE(IOUT,*) 'MNW1:'
cswm: SET POINTERS FOR FLOW PACKAGE TO GET K's FOR CEL2WEL
      IF ( Iubcf.NE.0 ) CALL SGWF2BCF7PNT(Igrid)
      IF ( Iulpf.NE.0 ) CALL SGWF2LPF7PNT(Igrid)
      IF ( Iuhuf.NE.0 ) CALL SGWF2HUF7PNT(Igrid)
      IF ( Iuupw.NE.0 ) CALL SGWF2UPW1PNT(Igrid)
C
      icmn = 1
      kfini = 1
      tab = CHAR(9)
      qfrcmn = ZERO25
      qfrcmx = ZERO25
      qreject = 0.0D0
      nqreject = 0
      nl = 0
      IF ( PLOSS.GT.1.001D0 ) nl = 1 !!  Read NL loss Coefficient after Skin
c
c  Check for setting the HREFerence array
CERB     IN FIRST STRESS PERIOD, HOLD IS UNDEFINED, SO USE HNEW INSTEAD
      IF ( Kper.EQ.1 ) THEN
        HMAX = ABS(HNEW(1,1,1))
        DO k = 1, NLAY
          DO i = 1, NROW
            DO j = 1, NCOL
              HREF(j,i,k) = HNEW(j,i,k)
              HMAX = MAX(ABS(HREF(j,i,k)),HMAX)
            ENDDO
          ENDDO
        ENDDO
      ELSE IF ( Kper.LE.KSPREF ) THEN
        HMAX = ABS(HOLD(1,1,1))
        DO k = 1, NLAY
          DO i = 1, NROW
            DO j = 1, NCOL
              HREF(j,i,k) = HOLD(j,i,k)
              HMAX = MAX(ABS(HREF(j,i,k)),HMAX)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
c
c------------------------------------------------------------------
c     The 18 rows of the well array store:
c      Row #  = Description
c------------------------------------------------------------------
c         1   = Well node locator
c         2   = Desired flow rate
c         3   = Actual flow rate used
c         4   = Water Quality attribute to be averaged by flow
c         5   = Radius of wellbore
c         6   = Skin associated with well completion
c         7   = Minimum/Maximum head or drawdown
c         8   = Elevation of reference head for computing lift costs
c         9   = Water Quality Group identifier
c        10   = Water level in wellbore
c        11   = HCOF value / QWaverage
c        12   = RHS  value
c        13   = Minimum flow rate -  to turn off
c        14   = Minimum flow rate -- to turn on
c        15   = Reserve Desired flow rate
c        16   = Non-linear loss term
c        17   = Actual flow rate to individual nodes of a multi-node well
c               kept for transport or other purposes !!7/13/2003 - CZ
c        18   = Explicit well identifier -- Same value for all nodes in a well 
c------------------------------------------------------------------
c
c1------read itmp(number of wells or flag saying reuse well data)
      CALL NCREAD(In, txtraw, ierr)
      txt = txtraw
      CALL UPCASE(txt)
      CALL QREAD(rn, 1, txt, ierr)
      itmp = rn(1)
	WRITE(IOUT,*) 'itmp:'
	WRITE(IOUT,*) itmp
c
      IF ( itmp.LT.0 ) THEN
c        if itmp less than zero reuse data. print message and return.
        WRITE (IOUT, 9001)
 9001   FORMAT (1X,/1X,'REUSING MNW7  FROM LAST STRESS PERIOD')
        RETURN
      ELSE
c  If itmp > 0,  Test if wells are to replace old ones or be added.
c
        IF ( INDEX(txt, 'ADD').EQ.0 ) NWELL2 = 0
        IF ( INDEX(txt, 'ADD').NE.0 ) WRITE(IOUT,*)'ADD:'
c
c   return if there are no wells to read ........
        IF ( itmp.EQ.0 ) RETURN
c
c  Redundant well information is allowed in MNW
c
c   Read additional well info
        nstart = NWELL2
        DO m = 1, itmp
          CALL NCREAD(In, txtraw, ierr)
          txt = txtraw
          CALL UPCASE(txt)
c   Attempt read with QREAD first
          CALL QREAD(rn, 4, txt, ierr)
          IF ( ierr.EQ.0 .AND. rn(5).LT.0.5D0 ) THEN
            k = IFRL(rn(1))
            j = IFRL(rn(2))
            i = IFRL(rn(3))
            q = rn(4)
            irmx = IFRL(rn(6)) + 1
          ELSE
c  Use fixed form reader if errors were detected
            READ (txt(1:40), '(3i10,f10.0)') k, j, i, q
            irmx = 41
          ENDIF
          WRITE (IOUT, *) 'Layer Row Column Qdes:'
          WRITE (IOUT, *) k, j, i, q
          node = (k-1)*NCOL*NROW + (j-1)*NCOL + i
c    Test for if well is in active grid ......
          iok = 1
          IF ( i.GT.NCOL .OR. j.GT.NROW .OR. node.GT.NODES ) iok = 0
          drytest = HNEW(i,j,k) - HDRY
          IF (iok.GT.0 .AND. ABS(drytest).GT.ZERO25) iok = IBOUND(i,j,k)
c
c  Should MNW wells be allowed in specified-head cells?
          IF ( iok.NE.0 ) THEN      !! Allow SH now, "gt" for no SH
c    Test for redundant info ......
            ipt = 0
c    The commented statements prevent having multiple MNW sites in the same cells
c            nt  = 0
c            do while (nt.lt.nwell2 .and. ipt.eq.0 )
c              nt = nt + 1
c              if( well2(1,nt).eq.node ) ipt = nt
c            enddo
            IF ( ipt.EQ.0 ) THEN
              NWELL2 = NWELL2 + 1
              ipt = NWELL2
            ENDIF
c
c    Assign data now that the pointer is set
            WELL2(1, ipt) = node
            WELL2(2, ipt) = q
            ipole = 0.0D0
            IF ( ABS(q).GT.ZERO25 ) ipole = q/ABS(q)
            WELL2(3, ipt) = WELL2(2, ipt)
            WELL2(13, ipt) = qfrcmn       ! default lower limit
            WELL2(14, ipt) = qfrcmx
c
c    Look for limit modifications
            kqc = INDEX(txt, 'QCUT')
            kpc = INDEX(txt, '%CUT')
            IF ( kqc+kpc.GT.0 .AND. ABS(q).GT.ZERO25 ) THEN
              if (kqc.GT.0) then
                WRITE(IOUT, *)'QCUT:'
              endif
              if (kpc.GT.0) then
                WRITE(IOUT, *)'%CUT:'
              endif
              tx2 = txt(kqc+kpc+5:256)
              CALL QREAD(rn, 2, tx2, ierr)
!              IF ( kqc.GT.0 ) THEN         !!  Absolute value was provided
!                rn(1) = 100.0D0*rn(1)/q        !!  Convert to percentage
!                rn(2) = 100.0D0*rn(2)/q
!              ENDIF
              IF ( ierr.GE.1 ) rn(2) = rn(1)
              WELL2(13, ipt) = rn(1)!*0.01D0    !! convert percentages to fractions
              WELL2(14, ipt) = rn(2)!*0.01D0


              IF ( INDEX(tx2, 'DEFAULT').GT.0 ) THEN
                qfrcmn = rn(1)!*0.01D0          !!  New default lower limit
                qfrcmx = rn(2)!*0.01D0          !!  New default upper limit
              ENDIF
            ENDIF
	      WRITE(IOUT, *) 'Qfrcmn, Qfrcmx:' 
	      WRITE(IOUT, *) WELL2(13, ipt), WELL2(14, ipt) 
c
c    Look for NonLinear coefficient
            WELL2(16, ipt) = 0.0D0              !!  NonLinear Loss Coefficient
            kcp = INDEX(txt, 'CP:')
            IF ( kcp.GT.0 .AND. nl.GT.0 ) THEN
              tx2 = txt(kcp+3:256)
              CALL QREAD(rn, 1, tx2, ierr)
              IF ( ierr.EQ.0 ) THEN
                WELL2(16, ipt) = rn(1)
c         Could reset default C-term here to a non-zero value
              ENDIF
  	        WRITE(IOUT,*) 'Cp:C:'
	        WRITE(IOUT,*) WELL2(16, ipt)
            ENDIF
c
c   Look for Site Identifier   -- Set to NO-PRINT  if not present.
            ksiteid = INDEX(txt, 'SITE')
            IF ( ksiteid.GT.0 ) THEN
              MNWSITE(ipt) = txtraw(ksiteid+5:256)
              kblk = INDEX(MNWSITE(ipt), ' ')
              ktab = INDEX(MNWSITE(ipt), tab)
              IF ( kblk.GT.0 ) kfini = kblk
              IF ( ktab.GT.0 .AND. ktab.LT.kblk ) kfini = ktab
              IF ( kfini.LE.32 ) THEN
                MNWSITE(ipt)(kfini:32) = '                 '
              ELSE
                kfini = 32
              ENDIF
              txt(ksiteid:ksiteid+kfini+4) = '                        '
            ELSE
              MNWSITE(ipt) = 'NO-PRINT                     '
            ENDIF
	      WRITE(IOUT,*) 'MNWsite:'
	      WRITE(IOUT,*) MNWSITE(ipt)
c
c    Read remaining info from card to set MNW specific parameters
            tx2 = txt(irmx:256)
            ki = INDEX(tx2, 'ZONE')
            IF ( ki.GT.0 ) tx2(ki:256) = '                         '
            CALL QREAD(rn, 6, tx2, ierr)
c
c         1   = Well node locator
c         2   = Desired flow rate
c         3   = Actual flow rate used
c         4   = Water Quality attribute to be averaged by flow
c         5   = Radius of wellbore
c         6   = Skin associated with well completion
c         7   = Minimum/Maximum head or drawdown
c         8   = Elevation of reference head for computing lift costs
c         9   = Water Quality Group identifier
c        10   = Water level in wellbore
c        11   = HCOF value / QWaverage
c        12   = RHS  value
c        13   = Minimum flow rate -  to turn off
c        14   = Minimum flow rate -- to turn on
c        15   = Reserve Desired flow rate
c        16   = Non-linear loss term
c        17   = Actual flow rate to individual nodes of a multi-node well
c               kept for transport or other purposes !!7/13/2003 - CZ
c        18   = Explicit well identifier -- Same value for all nodes in a well 
c   Move from well data from temp to permanent locations
            DO ip = 1, 6 - ierr
              WELL2(ip+3, ipt) = rn(ip)
            ENDDO
            IF ( ierr.GE.1 ) WELL2(9, ipt) = ipt
            IF ( ierr.GE.2 .OR. ABS(WELL2(8,ipt)).GT.HMAX )
     +           WELL2(8, ipt) = HREF(i,j,k)
c  Compute HLIM relative to reference elevation if HLIM read was a DrawDown (DD)
            IF ( INDEX(txt, 'DD').GT.0 ) then
                WELL2(7, ipt) = ipole*WELL2(7, ipt) + WELL2(8, ipt)
                WRITE(IOUT, *) 'DD:'
            endif
            IF ( ierr.GE.3 ) WELL2(7, ipt) = ipole*1.0D+26
            IF ( ierr.GE.4 ) WELL2(6, ipt) = 0.0D0
            IF ( ierr.GE.5 ) WELL2(5, ipt) = 0.0D0
            IF ( ierr.GE.6 ) WELL2(4, ipt) = -1.0D0
		  WRITE(IOUT,*) 'QWval, Rw, Skin, Hlim, Href, Iwgrp:'
	      WRITE(IOUT,*) (WELL2(IP, ipt), IP=4,9)
c  Flag as 2-point definition of a multi-node well if MULTI is detected.
            IF ( INDEX(tx2, 'MULTI').GT.0 .AND.
     +           ABS(WELL2(5,ipt)).GT.ZERO25 ) THEN
	        WRITE(IOUT,*)'MULTI:'
c  Define direction and # of points in well
              WELL2(2, ipt-1) = WELL2(2, ipt) + WELL2(2, ipt-1)
              n1 = IFRL(WELL2(1, ipt-1))
              mstep = IDIRECT(n1, node, NCOL, NROW)
              DO nn = n1 + mstep, node, mstep
                ipt = ipt + 1
                NWELL2 = NWELL2 + 1
                WELL2(1, ipt) = nn
                WELL2(2, ipt) = 0.0D0
                WELL2(3, ipt) = WELL2(2, ipt)
                WELL2(4, ipt) = WELL2(4, ipt-1)
                WELL2(5, ipt) = WELL2(5, ipt-1)
                WELL2(6, ipt) = WELL2(6, ipt-1)
                WELL2(16, ipt) = WELL2(16, ipt-1)  !!  NonLinear Loss Coefficient
                WELL2(9, ipt) = WELL2(9, ipt-1)
                WELL2(8, ipt) = -1.0D31
                WELL2(13, ipt) = 0.0D0
                WELL2(14, ipt) = 0.0D0
                icmn = icmn + 1
                WELL2(7, ipt) = icmn
              ENDDO
c  Flag as part of a multi-node well if MN is detected.
            ELSE IF ( INDEX(tx2, 'MN').GT.0 .AND.
     +                 ABS(WELL2(5,ipt)).GT.ZERO25 ) THEN
	        WRITE(IOUT,*)'MN:'
c  Set to very large -value to flag MN status
              WELL2(8, ipt) = -1.0D31
              icmn = icmn + 1
              WELL2(7, ipt) = icmn
            ELSE
              icmn = 1
            ENDIF
          ELSE
c   Sum details on rejected wells
            qreject = qreject + q
            nqreject = nqreject + 1
          ENDIF   !   IBOUND test statement
        ENDDO     !   end of well entry loop
c
c   Process wells that are screened across multiple nodes
c
c Check for extreme contrast in conductance
c
        WELL2(8, NWELL2+1) = 0.0D0
        IF ( nstart.LT.1 ) nstart = 1
        IF ( nstart.GT.NWELL2 ) nstart = NWELL2 - itmp + 1
        DO i = nstart, NWELL2
          IF ( WELL2(8, i).LT.-1.E30 .AND. WELL2(8, i+1).GT.-1.E30 .OR. 
     +         WELL2(8, i).LT.-1.E30 .AND. i.EQ.NWELL2 ) THEN
            ngrp = IFRL(WELL2(7, i))
            ne = i
            nb = ne - ngrp + 1
            hlim = WELL2(7, nb)
            hrfw = WELL2(8, nb)
            qsum = 0.0D0
            tempsite = 'NO-PRINT                     '
            DO iin = nb, ne
              qsum = qsum + WELL2(2, iin)
              IF ( MNWSITE(iin)(1:8).NE.'NO-PRINT' )
     +             tempsite = MNWSITE(iin)
              WELL2(2, iin) = 0.0D0
              WELL2(7, iin) = 1.0D31
              WELL2(8, iin) = 1.0D31
c   Set to very large +value to flag MN status
            ENDDO
c   Set All SiteIDs in a multinode well to a common tag
            DO iin = nb, ne
              MNWSITE(iin) = tempsite
            ENDDO
            WELL2(7, nb) = ne
            WELL2(2, ne) = qsum
            WELL2(7, ne) = hlim
            WELL2(8, ne) = hrfw
          ENDIF
        ENDDO  !   end of multi-well pointer setting
c
      ENDIF
c
c  nwell2>mxwel2.  print message. stop.
      IF ( NWELL2.GT.MXWEL2 ) THEN
        WRITE (IOUT, 9002) NWELL2, MXWEL2
 9002   FORMAT (1X,/
     1     1X,'nwell2(', i4, ') IS GREATER THAN mxwel2(', i4, ')')
C
C       When compiling MNW with Modflow-96, comment out the call to
C       USTOP and uncomment the STOP statement
        CALL USTOP(' ')
C        STOP
C
      ENDIF
c
c   Place desired flow rates in a reserved location
c
      DO m = 1, NWELL2
        WELL2(15, m) = WELL2(2, m)
      ENDDO
c
c--assign unique well id for use with MT3DMS link package (cdl: 4/19/05)
        m=0
        IDwell=1
        do while (m.lt.nwell2)
           m=m+1
           if(well2(8,m).gt.1.D30) then
              do mm=m,ifrl(well2(7,m))
                 well2(18,mm)=IDwell
              enddo
              m=ifrl(well2(7,m))
           else
              well2(18,m)=IDwell
           endif
           IDwell=IDwell+1
        enddo
c
c   Echo input to iout file
c
      IF ( IWELPT.EQ.0 ) THEN
!        IF ( nqreject.GT.0 ) THEN
!          txt = ' wells were outside of the model domain.'
!          WRITE (IOUT, '(1X,/,5x,i5,a50)') nqreject, txt
!          txt = 'The rejected pumpage totaled: '
!          WRITE (IOUT, '(1X,/1X,a34,g14.5)') txt, qreject
!        ENDIF
c
!        WRITE (IOUT, '(1X,/,10x,i5," MNW WELLS")') NWELL2
!        WRITE (IOUT, 9003)
! 9003   FORMAT ('    No.   Lay   Row   Col    Stress   QW param', 6x, 
!     +          'Rw       Skin    WL Limit    WL Refer   NonLinear Cp', 
!     +          '  QW Group  Cell-To-Well  Min-Qoff  Min-Qon', 
!     +          '  Site Identifier')
c
        DO m = 1, NWELL2
          n = INT(WELL2(1, m))
          k = (n-1)/(NCOL*NROW) + 1
          j = MOD((n-1), NCOL*NROW)/NCOL + 1  !swm: note i,j, are switched
          i = MOD((n-1), NCOL) + 1            !swm: from usual MF2K style
          igrp = INT(WELL2(9, m))
c
          rw = WELL2(5, m)
          IF ( rw.LT.-ZERO25 ) THEN
            cond = -rw
          ELSE
            qact = WELL2(3, m)
            sk = WELL2(6, m)
            cf = WELL2(16, m)
!            IF ( Iubcf.NE.0 ) cond = CEL2WELBCF(i,j,k,rw,sk,qact,cf)
!            IF ( Iulpf.NE.0 ) cond = CEL2WELLPF(i,j,k,rw,sk,qact,cf)
!            IF ( Iuhuf.NE.0 ) cond = CEL2WELHUF(i,j,k,rw,sk,qact,cf)
!            IF ( Iuupw.NE.0 ) cond = CEL2WELUPW(i,j,k,rw,sk,qact,cf)
            IF ( rw.LT.ZERO25 ) cond = cond*1.0D3
          ENDIF
          WELL2(11, m) = cond
c
c ---------Modified OUTPUT to hide internal pointers that "Look Funny" --KJH-- July 10, 2003
          IF ( WELL2(8, m).LT.BIG ) THEN
            hlim = WELL2(7, m)
            hrfw = WELL2(8, m)
          ELSE IF ( WELL2(7, m).LT.BIG ) THEN
            ne = IFRL(WELL2(7, m))
            hlim = WELL2(7, ne)
            hrfw = WELL2(8, ne)
          ENDIF
!          WRITE (IOUT, 9004) m, k, j, i, (WELL2(ii, m), ii=3, 6), hlim, 
!     +                       hrfw, WELL2(16, m), igrp, WELL2(11, m), 
!     +                       (WELL2(ii, m)*100.0D0, ii=13, 14), 
!     +                       MNWSITE(m)
 9004     FORMAT (1x, 4I6, 6g11.4, g13.6, i10, g13.6, 2F10.3, 2x, a32)
c
        ENDDO
      ELSE
!        WRITE (IOUT, *) 'WELLS WILL NOT BE PRINTED'
      ENDIF
c
c  Write blank fields in Auxillary BYNODE file if KPER=1 & IO>0
c
      IF ( TOTIM.LT.1E-26 .AND. IOWELL2(2).NE.0 ) THEN
        io = ABS(IOWELL2(2))
        DO m = 1, NWELL2
          n = IFRL(WELL2(1, m))
          WRITE (io, '(a32,1x,2i8)') MNWSITE(m), m, n
        ENDDO
      ENDIF
c
c  Write blank fields in Auxillary QSUM file if KPER=1 & IO>0
c
      IF ( TOTIM.LT.1E-26 .AND. IOWELL2(3).NE.0 ) THEN
        io = ABS(IOWELL2(3))
        m = 0
        DO WHILE ( m.LT.NWELL2 )
          m = m + 1
          IF ( WELL2(8, m).GT.BIG ) THEN
            ne = IFRL(WELL2(7, m))
!            WRITE (ABS(IOWELL2(3)), '(a32,1x,i5.5,"-",i5.5)') MNWSITE(m)
!     +             , m, ne
            m = ne
          ENDIF
        ENDDO
      ENDIF
c
      END SUBROUTINE GWF2MNW17RP
c
c_________________________________________________________________________________
c
!      SUBROUTINE GWF2MNW17AD(Iubcf, Iulpf, Iuhuf, Igrid)
C     VERSION 20020819 KJH
c
c----- MNW by K.J. Halford
c
c     ******************************************************************
c     Update Qact for wells that were constrained
c     ******************************************************************
c
c_________________________________________________________________________________
c
!      SUBROUTINE GWF2MNW17FM(Kiter, Iubcf, Iulpf, Iuhuf, Igrid)
c     VERSION 20020819 KJH
c
c----- MNW by K.J. Halford
c
c     ******************************************************************
c     add well flow to source term
c     ******************************************************************
!      SUBROUTINE GWF2MNW17BD(Nstp, Kstp, Kper, Igrid)
c     VERSION 20030710 KJH
c
c----- MNW by K.J. Halford        1/31/98
c     ******************************************************************
c     calculate volumetric budget for wells
c     ******************************************************************
!      SUBROUTINE GWF2MNW17OT(Igrid)
C     VERSION 20020819 KJH
c
c     ******************************************************************
c     Sort well output into useful tables
c     ******************************************************************
!      DOUBLE PRECISION FUNCTION CEL2WELBCF(Ix, Iy, Iz, Rw, Skin, Q, Cf)
C     VERSION 20030327 KJH        -- Patched Hyd.K term in LPF solution
c
c----- MNW by K.J. Halford
c
c     ******************************************************************
c     Compute conductance term to define head loss from cell to wellbore
c      Methodology is described in full by Peaceman (1983)
cswm: NOTE: MODIFIED FOR USE WITH THE BCF PACKAGE
c     ******************************************************************
!      DOUBLE PRECISION FUNCTION CEL2WELLPF(Ix, Iy, Iz, Rw, Skin, Q, Cf)
C     VERSION 20030327 KJH        -- Patched Hyd.K term in LPF solution
c
c----- MNW by K.J. Halford
c
c     ******************************************************************
c     Compute conductance term to define head loss from cell to wellbore
c      Methodology is described in full by Peaceman (1983)
cswm: NOTE: MODIFIED FOR USE WITH THE LPF PACKAGE
c     ******************************************************************
!      DOUBLE PRECISION FUNCTION CEL2WELHUF(Ix, Iy, Iz, Rw, Skin, Q, Cf)
C     VERSION 20030327 KJH        -- Patched Hyd.K term in LPF solution
c
c----- MNW by K.J. Halford
c
c     ******************************************************************
c     Compute conductance term to define head loss from cell to wellbore
c      Methodology is described in full by Peaceman (1983)
cswm: NOTE: MODIFIED FOR USE WITH THE HUF PACKAGE
c     ******************************************************************
c
      INTEGER FUNCTION IDIRECT(N1, N2, Ncol, Nrow)
c     ******************************************************************
c     Define direction of pointer along a row, column, or layer
c     ******************************************************************
      IMPLICIT NONE
      INTRINSIC ABS
c Arguments
      INTEGER, INTENT(IN) :: N1, N2, Ncol, Nrow
c     ------------------------------------------------------------------
      IDIRECT = Ncol
      IF ( ABS(N2-N1).GT.Ncol*Nrow ) IDIRECT = Ncol*Nrow
      IF ( ABS(N2-N1).LT.Ncol ) IDIRECT = 1
      IF ( N2.LT.N1 ) IDIRECT = -IDIRECT
c
      END FUNCTION IDIRECT
c     ******************************************************************
      INTEGER FUNCTION IFRL(R)
      IMPLICIT NONE
      INTRINSIC ABS
c Arguments
      DOUBLE PRECISION, INTENT(IN) :: R
c Local Variables
      INTEGER :: ip
c     ------------------------------------------------------------------
      ip = ABS(R) + 0.5D0
      IF ( R.LT.0.0D0 ) ip = -ip
      IFRL = ip
      END FUNCTION IFRL
c     ******************************************************************
c
      SUBROUTINE NCREAD(Io, Txt, Ierr)
c     ******************************************************************
c     NCREAD: reads lines of input and ignores lines that begin with a "#" sign.
c          All information after a ! is wiped from the input card.
c     ******************************************************************
      IMPLICIT NONE
      EXTERNAL UPCASE, USTOP
c Arguments
      INTEGER, INTENT(INOUT) :: Io
      INTEGER, INTENT(OUT) :: Ierr
      CHARACTER(LEN=256), INTENT(OUT) :: Txt
c Local Variables
      INTEGER :: ioalt, ioflip, iohold, ki
      CHARACTER(LEN=128) :: afile
      CHARACTER(LEN=256) :: tx2
      DATA ioflip, ioalt/69, 69/
c     ------------------------------------------------------------------
      Ierr = 0
    5 READ (Io, '(a)', END=10) Txt
      IF ( Txt(1:1).EQ.'#' ) GOTO 5
c
      ki = INDEX(Txt, '!')
      IF ( ki.GT.0 )
     +  Txt(ki:256) = '                                                '
c
      tx2 = Txt
      CALL UPCASE(tx2)
c
c    Test for switching control to an auxillary input file
c
      ki = INDEX(Txt, ':')
      IF ( INDEX(tx2, 'REDIRECT').GT.0 .AND. ki.GT.0 ) THEN
        afile = Txt(ki+1:256)
        ki = INDEX(afile, '  ') - 1
        iohold = Io
        Io = ioflip
        ioflip = iohold
        OPEN (Io, FILE=afile(1:ki), STATUS='OLD', ERR=20)
        GOTO 5
      ENDIF
c
c    Test for returning io control from auxillary input to master input file
c
      IF ( INDEX(tx2, 'RETURN').GT.0 .AND.
     +     INDEX(tx2, 'CONTROL').GT.0 ) GOTO 10
c
      ki = INDEX(tx2, '<END>')
      IF ( ki.GT.0 ) THEN
        Ierr = 1
        Txt(ki+5:256) = '                                           '
      ENDIF
c
      IF ( INDEX(tx2, '<STOP>').GT.0 ) Ierr = 2
      RETURN
c
c    Report error in opening auxillary input file and stop
c
   20 WRITE (*, 25) afile
   25 FORMAT (/, '  ERROR opening auxillary input file', //,
     + '   The file:  ', a40, ' does not exist', /)
c
c     When compiling MNW with Modflow-96, comment out the call to
c     USTOP and uncomment the STOP statement
      CALL USTOP(' ')
c      STOP
c
   10 Txt(1:3) = 'EOF'
      IF ( Io.EQ.ioalt ) THEN
        CLOSE (Io)
        iohold = Io
        Io = ioflip
        ioflip = iohold
        GOTO 5
      ELSE
        Ierr = -1
      ENDIF
c
      END SUBROUTINE NCREAD
c
c     ******************************************************************
c     ******************************************************************
      SUBROUTINE QREAD(R, Ni, Ain, Ierr)
      IMPLICIT NONE
      INTRINSIC CHAR, INDEX
      INTEGER, PARAMETER :: MRNV=25
c Arguments
      DOUBLE PRECISION, INTENT(OUT), DIMENSION(MRNV) :: R
      INTEGER, INTENT(IN) :: Ni
      INTEGER, INTENT(OUT) :: Ierr
      CHARACTER(LEN=256), INTENT(IN) :: Ain
c Local Variables
      INTEGER :: i, istat, ki, n, nd
      CHARACTER(LEN=1) :: tab
      CHARACTER(LEN=8) :: rdfmt
      CHARACTER(LEN=256) :: a256
c     ------------------------------------------------------------------
      Ierr = 0
      tab = CHAR(9)           ! sets tab delimiter
c
c   r(ni+1) records the number of non-numeric entries that were attempted to be read as a number
c   r(ni+2) records the last column that was read from the card
c
      R(Ni+1) = -1.0D0
      a256 = Ain
      DO i = 1, 256
        IF ( a256(i:i).EQ.tab ) a256(i:i) = ' '
        IF ( a256(i:i).EQ.',' ) a256(i:i) = ' '
        IF ( a256(i:i).EQ.':' ) a256(i:i) = ' '
        IF ( a256(i:i).EQ.'=' ) a256(i:i) = ' '
      ENDDO
      n = 1
      i = 0
   11 R(Ni+1) = R(Ni+1) + 1.0D0
   10 i = i + 1
      IF ( i.GE.256 ) GOTO 15
      IF ( a256(i:i).EQ.' ' ) THEN
        a256(i:i) = '?'
        GOTO 10
      ENDIF
c
      ki = INDEX(a256, ' ') - 1
      nd = ki - i + 1
      rdfmt = '(F??.0) '
      WRITE (rdfmt(3:4), '(i2.2)') nd
CERB  Fix for bug that caused i to be incremented by only 1 position
CERB  each time the read statement returns an error.  This bug also
CERB  incremented r(ni+1) unnecessarily.  With Lahey-compiled code, the
CERB  buggy version would read a final E in a word (without returning an
CERB  error) as a zero.
CERB      read (a256(i:ki),rdfmt,err=11,end=10) r(n)
      READ (a256(i:ki), rdfmt, ERR=13, IOSTAT=istat) R(n)
   13 CONTINUE
      i = ki
      IF ( istat.GT.0 ) GOTO 11 ! PART OF BUG FIX -- ERB
      n = n + 1
      IF ( n.LE.Ni .AND. i.LT.256 ) GOTO 10
c
   15 n = n - 1
      Ierr = Ni - n
      R(Ni+2) = i
c
      END SUBROUTINE QREAD
C***********************************************************************
      SUBROUTINE GWF2MNW17DA(Igrid)
C     ******************************************************************
C     DEALLOCATE MNW DATA      
C     ******************************************************************
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFMNW1MODULE
C     ------------------------------------------------------------------
C Arguments
      INTEGER :: Igrid, IDUM
C     ------------------------------------------------------------------
      IDUM=IGRID
      DEALLOCATE (GWFMNWDAT(Igrid)%NWELL2)
      DEALLOCATE (GWFMNWDAT(Igrid)%MXWEL2)
      DEALLOCATE (GWFMNWDAT(Igrid)%IWL2CB)
      DEALLOCATE (GWFMNWDAT(Igrid)%NOMOITER)
      DEALLOCATE (GWFMNWDAT(Igrid)%KSPREF)
      DEALLOCATE (GWFMNWDAT(Igrid)%IWELPT)
      DEALLOCATE (GWFMNWDAT(Igrid)%PLOSS)
      DEALLOCATE (GWFMNWDAT(Igrid)%SMALL)
      DEALLOCATE (GWFMNWDAT(Igrid)%HMAX)
      DEALLOCATE (GWFMNWDAT(Igrid)%MNWNAME)
      DEALLOCATE (GWFMNWDAT(Igrid)%IOWELL2)
      DEALLOCATE (GWFMNWDAT(Igrid)%HREF)
      DEALLOCATE (GWFMNWDAT(Igrid)%WELL2)
      DEALLOCATE (GWFMNWDAT(Igrid)%MNWSITE)
C
      END SUBROUTINE GWF2MNW17DA
C***********************************************************************
      SUBROUTINE SGWF2MNW1PNT(Igrid)
C     ******************************************************************
C     SET MNW POINTER DATA TO CURRENT GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFMNW1MODULE
C     ------------------------------------------------------------------
C Arguments
      INTEGER :: Igrid
C     ------------------------------------------------------------------
      NWELL2=>GWFMNWDAT(Igrid)%NWELL2
      MXWEL2=>GWFMNWDAT(Igrid)%MXWEL2
      IWL2CB=>GWFMNWDAT(Igrid)%IWL2CB
      NOMOITER=>GWFMNWDAT(Igrid)%NOMOITER
      KSPREF=>GWFMNWDAT(Igrid)%KSPREF
      IWELPT=>GWFMNWDAT(Igrid)%IWELPT
      PLOSS=>GWFMNWDAT(Igrid)%PLOSS
      SMALL=>GWFMNWDAT(Igrid)%SMALL
      HMAX=>GWFMNWDAT(Igrid)%HMAX
      MNWNAME=>GWFMNWDAT(Igrid)%MNWNAME
      IOWELL2=>GWFMNWDAT(Igrid)%IOWELL2
      HREF=>GWFMNWDAT(Igrid)%HREF
      WELL2=>GWFMNWDAT(Igrid)%WELL2
      MNWSITE=>GWFMNWDAT(Igrid)%MNWSITE
C
      END SUBROUTINE SGWF2MNW1PNT
C***********************************************************************
      SUBROUTINE SGWF2MNW1PSV(Igrid)
C     ******************************************************************
C     SAVE MNW POINTER DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFMNW1MODULE
C     ------------------------------------------------------------------
C Arguments
      INTEGER :: Igrid
C     ------------------------------------------------------------------
      GWFMNWDAT(Igrid)%NWELL2=>NWELL2
      GWFMNWDAT(Igrid)%MXWEL2=>MXWEL2
      GWFMNWDAT(Igrid)%IWL2CB=>IWL2CB
      GWFMNWDAT(Igrid)%NOMOITER=>NOMOITER
      GWFMNWDAT(Igrid)%KSPREF=>KSPREF
      GWFMNWDAT(Igrid)%IWELPT=>IWELPT
      GWFMNWDAT(Igrid)%PLOSS=>PLOSS
      GWFMNWDAT(Igrid)%SMALL=>SMALL
      GWFMNWDAT(Igrid)%HMAX=>HMAX
      GWFMNWDAT(Igrid)%MNWNAME=>MNWNAME
      GWFMNWDAT(Igrid)%IOWELL2=>IOWELL2
      GWFMNWDAT(Igrid)%HREF=>HREF
      GWFMNWDAT(Igrid)%WELL2=>WELL2
      GWFMNWDAT(Igrid)%MNWSITE=>MNWSITE
C
      END SUBROUTINE SGWF2MNW1PSV
