 
!
!-------SUBROUTINE GWF2NWT1AR
!
      SUBROUTINE GWF2NWT1AR(In, Mxiter, Iunitlak, Igrid)
!
!------NEWTON SOLVER VERSION NUMBER 1.0.6:  DECEMBER 5, 2012
!      RICHARD G. NISWONGER
      USE GLOBAL,     ONLY:NCOL,NROW,NLAY,ITRSS,LAYHDT,LAYHDS,LAYCBD,
     1                     NCNFBD,IBOUND,BUFF,BOTM,NBOTM,DELR,DELC,IOUT,
     2                     LBOTM,HNEW
      USE GWFBASMODULE,ONLY:HDRY
      USE GWFNWTMODULE
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
      INTRINSIC INT
      EXTERNAL URDCOM, URWORD
      EXTERNAL SGWF2NWT1PSV
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
      INTEGER In, Igrid, Mxiter, L, NRC, Iunitlak
!     ------------------------------------------------------------------
!     LOCAL VARIABLES
!     ------------------------------------------------------------------
      INTEGER lloc, LLOCSAVE, istart, istop, i, ic, ir, il, jj
      CHARACTER(LEN=300) line
      REAL r, toldum, ftoldum, relaxdum, thetadum, amomentdum
      REAL akappadum, gammadum, Breducdum, Btoldum, Thickdum,ZERO
      INTEGER IANAME,KHANI,N,KK,nc,nr,nl,j,k,NCNVRT,NHANI,NWETD
! Memory use variables
      INTEGER lrwrk,liwrk,NODES,MBLACK,NJAF
      REAL Memuse1,Memuse2
!     ------------------------------------------------------------------
!

      CALL URDCOM(In, Iout, line)
      lloc = 1
      ALLOCATE (Tol, Ftol, RMS2, RMS1, Iierr,IFDPARAM,ICNVGFLG)
      ALLOCATE (ITER1,THETA,THICKFACT,BTOL,Numtrack)
      ALLOCATE (RMSAVE)
      ALLOCATE (Numnonzero, Numactive, Numcell, II)
      ALLOCATE (Akappa,Gamma,Amomentum,Btrack,Breduc,Numtrack)
      ALLOCATE (Nonmeth, Linmeth, IPRNWT, Itreal, Ibt)
      ALLOCATE (IBOTAV)
!1------IDENTIFY PACKAGE AND INITIALIZE.
      WRITE (Iout, *) 'NWT:'
!      WRITE (Iout, 9001) In
! 9001 FORMAT (1X, /' NWT1 -- Newton Solver, ',
!     +       'VERSION 1.0.6, 12/05/2012', /, 9X, 'INPUT READ FROM UNIT',
!     +        I3,/)
      i = 1
      Itreal = 0
      Ibt = 0
      RMS2 = 0.0D0
      RMS1 = 0.0D0
      RMSAVE = 0.0D0
      Numactive = 0
      Numcell = 0
      akappadum = 0.0   
      toldum = 1.0e-4
      ftoldum = 100.0
      Mxiter = 100
      Thickdum = 1.0e-4
      Linmeth = 2     ! Linmeth=1 GMRES; Linmeth=2 XMD; Linmeth=3 SAMG 
      IPRNWT = 1     ! Iteration stats (>0 prints)
      IBOTAV = 1     ! Doesn't reset to bottom
      LLOC=1
      Numtrack = 0
      Btoldum = 1.0
      Breducdum = 1.0
      ICNVGFLG = 0
      CALL URWORD(line, lloc, istart, istop, 3, i, toldum, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 3, i, ftoldum, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, Mxiter, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 3, i, Thickdum, Iout, In)
!      CALL URWORD(line, lloc, istart, istop, 2, Nonmeth, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, Linmeth, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, IPRNWT, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, IBOTAV, r, Iout, In)
      Write(IOUT, *) 
     1  'toldum,ftoldum,Mxiter,Thickdum,Linmeth,IPRNWT,IBOTAV:' 
      Write(IOUT, *) 
     1  toldum,ftoldum,Mxiter,Thickdum,Linmeth,IPRNWT,IBOTAV 
C
C3B-----GET OPTIONS.
      IFDPARAM=0
   20 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'SIMPLE') THEN
         IFDPARAM=1
         WRITE(IOUT,*) 'SIMPLE:'
!         WRITE(IOUT,21)
!   21    FORMAT(1X,'SIMPLE OPTION:',/,
!     1     1X,'DEFAULT SOLVER INPUT VALUES REFLECT NEARLY LINEAR MODEL')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'MODERATE') THEN
         WRITE(IOUT,*) 'MODERATE:'
         IFDPARAM=2
!         WRITE(IOUT,23)
!   23    FORMAT(1X,'MODERATE OPTION:',/,1X,'DEFAULT SOLVER',
!     1         ' INPUT VALUES REFLECT MODERETELY NONLINEAR MODEL')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'COMPLEX') THEN
         WRITE(IOUT,*) 'COMPLEX:'
         IFDPARAM=3
!         WRITE(IOUT,25)
!   25    FORMAT(1X,'COMPLEX OPTION:',/,1X,'DEFAULT SOLVER',
!     1 ' INPUT VALUES REFLECT STRONGLY NONLINEAR MODEL')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'SPECIFIED') THEN
         WRITE(IOUT,*) 'SPECIFIED:'
         IFDPARAM=4
!         WRITE(IOUT,26)
!   26    FORMAT(1X,'SPECIFIED OPTION:',/,1X,'SOLVER INPUT',
!     1 ' VALUES ARE SPECIFIED BY USER')
      END IF
      LLOCSAVE = LLOC
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'CONTINUE') THEN
         WRITE(IOUT,*) 'CONTINUE:'
        ICNVGFLG = 1
      ELSE
        LLOC = LLOCSAVE
      END IF
        
      
!      IF(LLOC.LT.200) GO TO 20    
!
! Don't need to read these when using default Options.
      IF ( IFDPARAM.EQ.4 ) THEN
      CALL URWORD(line, lloc, istart, istop, 3, i, thetadum, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 3, i, akappadum, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 3, i, gammadum, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 3, i, amomentdum, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, Btrack, r, Iout, In)
      Write(IOUT,*) 'thetadum,akappadum,gammadum,amomentdum,Btrack:'
      Write(IOUT,*) thetadum,akappadum,gammadum,amomentdum,Btrack
      IF ( BTRACK.GT.0 ) THEN
      CALL URWORD(line, lloc, istart, istop, 2, Numtrack, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 3, i, Btoldum, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 3, i, Breducdum, Iout, In)
      Write(IOUT,*) 'Numtrack,Btoldum,Breducdum:'
      Write(IOUT,*) Numtrack,Btoldum,Breducdum
      END IF
      ELSEIF ( IFDPARAM.EQ.1 ) THEN
! Set values based on default and Option keyword.
        thetadum = 0.97
        akappadum = 0.0001
        gammadum = 0.0
        amomentdum = 0.0
        Btrack = 0
        Numtrack = 20
        Btoldum = 1.5
        Breducdum = 0.97
      ELSEIF ( IFDPARAM.EQ.2 ) THEN
        thetadum = 0.90
        akappadum = 0.0001
        gammadum = 0.00
        amomentdum = 0.1
        Btrack = 0
        Numtrack = 20
        Btoldum = 1.1
        Breducdum = 0.9
      ELSEIF ( IFDPARAM.EQ.3 ) THEN
        thetadum = 0.85
        akappadum = 0.00001
        gammadum = 0.0
        amomentdum = 0.1
        Btrack = 1
        Numtrack = 50
        Btoldum = 1.1
        Breducdum = 0.7
      ELSE
        Write(iout,*)
        Write(iout,*)'***Erroneous value for Input value "Options."***'
        Write(iout,*)'Check input. Model Stopping.'
        Write(iout,*) 
        CALL USTOP(' ')
      END IF
 !     
 !     IF ( Nonmeth==1 )Then
 !       Write(iout,*) '***Newton Linearization will be used***'
 !       Write(iout,*)
 !     ELSEIF ( Nonmeth==0 )Then
 !       Write(iout,*) '***Picard Linearization will be used***'
 !       Write(iout,*)
 !     ELSE
 !       Write(iout,*) '***Incorrect value for variable Nonmeth was ',
 !    +                'specified. Check input.***'
 !       Write(iout,*)
 !       Call USTOP('  ')
 !     END IF
      Nonmeth = 1
!
      IF ( Linmeth==1 )Then
!        Write(iout,*) '***GMRES linear solver will be used***'
!        Write(iout,*)
      ELSEIF ( Linmeth==2 )Then
!        Write(iout,*) '***XMD linear solver will be used***'
!        Write(iout,*)
       ELSEIF ( Linmeth==3 )Then
!        Write(iout,*) '***SAMG linear solver will be used***'
!        Write(iout,*)
      ELSE
        Write(iout,*) '***Incorrect value for Linear solution method ',
     +                'specified. Check input.***'
        Write(iout,*)
        Call USTOP('  ')
      END IF
!
      Thickfact = Thickdum
      Btol = Btoldum
      Breduc = Breducdum
      Theta = Thetadum
      Akappa = akappadum
      Gamma = gammadum
      Amomentum = amomentdum
      IF ( Theta.LT.CLOSEZERO ) Theta = 0.9D0
      Tol = toldum
      Ftol = ftoldum
!2--ECHO NWT INPUT
!      WRITE(IOUT,9010) Tol,Ftol,MXITER
!      WRITE(IOUT,9011) THETA,AKAPPA,GAMMADUM,AMOMENTUM 
! 9010 FORMAT(1X,'  CONVERGENCE CRITERION OF',E15.6,' FOR HEAD SOLUTION',
!     +      /1X,'  AND A TOLERANCE OF',E15.6,' FOR FLOW SOLUTION AND ',
!     +      /1X,'  A MAXIMUM OF ',I5,' OUTER ITERATIONS. ',//)
! 9011 FORMAT(1X,'  D-B-D REDUCTION FACTOR OF ',E15.6,' AND ',
!     +      /1X,'  A D-B-D INCREASE FACTOR OF ',E15.6,' AND ',
!     +      /1X,'  A D-B-D RELAXATION OF ',E15.6,' AND ', 
!     +      /1X,'  A MOMENTUM FACTOR OF ',E15.6,' .',//)
!      IF ( BTRACK.GT.0 ) THEN 
!        WRITE(IOUT,9012) Numtrack,BTOL,BREDUC
!      ELSE
!        WRITE(IOUT,*)'***BACKTRACKING IS INACTIVE***'
!      END IF
! 9012 FORMAT(1X,'  BACKTRACKING IS ACTIVE ',
!     +      /1X,'  THE MAXIMUM NUMBER OF BACKTRACKS IS ',I5,' AND ',
!     +      /1X,'  THE BACKTRACKING TOLERANCE IS ',E15.6, ' AND',
!     +      /1X,'  THE BACKTRACKING REDUCTION FACTOR IS ',E15.6,/)
!
!3-----ALLOCATE SPACE
      
      IF ( Linmeth.EQ.1 ) THEN
        CALL GMRES7AR(IN)
      ELSEIF ( Linmeth==2 ) THEN
        CALL XMD7AR(IN)
 !     ELSEIF ( Linmeth==3 ) THEN
!        CALL SAMG7AR(IN)
      END IF
!-------SAVE POINTERS FOR GRID AND RETURN
      CALL SGWF2NWT1PSV(Igrid)
!
      END SUBROUTINE GWF2NWT1AR
!     -----------------------------------------------------------------
!
!1     SUBROUTINE TEMPFILLUN. SET SCALERS FOR UNCONFINED FLOW
!      SUBROUTINE TEMPFILLUN(Ic, Ir, Il)
!     SUBROUTINE TEMPFILLCON. SET SCALERS FOR CONFINED FLOW
!      SUBROUTINE TEMPFILLCON(Ic, Ir, Il)
!
!     -----------------------------------------------------------------
!
!      FUNCTION DHORIZ(Hup, Ttop, Bbot, il)
! RETURNS DERIVATIVE OF HORIZONTAL CONDUCTANCE BASED ON SMOOTH FUNCTION
! FUNCTION IS CALCULATED IN UPW PACKAGE IN SUBROUTINE SAT_THICK
!
!     -----------------------------------------------------------------
!      DOUBLE PRECISION FUNCTION DVERT(Hh,Ttop,Bbot)
!
!     -----------------------------------------------------------------
!
!      SUBROUTINE ORDERCELL()
! Order system for MODFLOW storage scheme by relating Row, Col, and Lay to
! Jacobian order
!     -----------------------------------------------------------------
!      SUBROUTINE FILLINDEX(jj)
! Fill CRS pointers for MODFLOW storage scheme by relating Row, Col, and Lay to
! Jacobian order
!
!     -----------------------------------------------------------------
!      SUBROUTINE COUNTACTIVE(jj)
!
!     -----------------------------------------------------------------
!      SUBROUTINE GWF2NWT1FM(Kkiter, ICNVG, KSTP, KPER, Maxiter, 
!     +                      Iunitchd, Igrid)
! Builds and Solves Jacobian
! Calls various unstructured linear solvers to solve Jacobian
!
!     -----------------------------------------------------------------
!     Set the head in dewatered cells to Hdry.
!      SUBROUTINE GWF2NWT1BD()
!
!
!     -----------------------------------------------------------------
!
!      SUBROUTINE Back_track(ichld,irhld,ilhld,ISS)
!
!
!     -----------------------------------------------------------------
!      SUBROUTINE GWF2NWT1UPH2(ichld,irhld,ilhld,ISS,Kkiter)
!  Update heads and apply relaxation (delta-bar-delta)
!
!
!     -----------------------------------------------------------------
!     Save previous iteration heads.
!      SUBROUTINE Head_save()
!
!     -----------------------------------------------------------------
!     Return value of groundwater flow equation
!      DOUBLE PRECISION FUNCTION GW_func(Ic, Ir, Il)
!
!
!
!     -----------------------------------------------------------------
!     Return value of L2-Norm of GW equation, max flux
!     and head residuals
!      DOUBLE PRECISION FUNCTION RMS_func(icfld,irfld,ilfld)
!
!     -----------------------------------------------------------------
!     Calculates derivatives of conductance.
!      SUBROUTINE Dcon(kkiter)
!     -----------------------------------------------------------------
!     Returns sum of saturated thicknesses for cells connected
!     to ic,ir,il.
!     and head residuals
!      DOUBLE PRECISION FUNCTION Sum_sat(sum,ic,ir,il)
!
!      SUBROUTINE Jacobian(kkiter,kper,kstp)
!
      

