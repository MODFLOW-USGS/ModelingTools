C  The hydrograph lables are established by the SE subroutines the
C  first time they are called.  SE is called the first time by the
C  AR subroutine for the BAS, IBS, and SUB Packages.  SE is called
C  the first time by the RP subroutine for the STR and SFR Packages.
C  Subsequent calls to SE subroutines in the Main must be made in
C  the same order as the initial calls.  A HYDMOD package that
C  has an RP subroutine will always have the initial SE call after
C  the initial SE calls for all of the packages that have the
C  inital SE call in the AR subroutine.
      MODULE HYDBASMODULE
        INTEGER,    SAVE,       POINTER                ::NHYDTOT
        REAL,       SAVE,       POINTER,DIMENSION(:,:) ::HYDVAL
        CHARACTER(LEN=20),SAVE, POINTER,DIMENSION(:)   ::HYDLBL
        INTEGER,    SAVE,       POINTER            ::IHYDMUN,NHYDBAS
        REAL,       SAVE,       POINTER                ::HYDNOH
        LOGICAL,    SAVE,       POINTER,DIMENSION(:)   ::IBHYDBAS
        LOGICAL,    SAVE,       POINTER,DIMENSION(:)   ::INTRPHYDBAS
        INTEGER,    SAVE,       POINTER,DIMENSION(:,:) ::JIKHYDBAS
        REAL,       SAVE,       POINTER,DIMENSION(:,:) ::HYDBASWT
        REAL,       SAVE,       POINTER,DIMENSION(:)   ::HYDBASSTRT
        CHARACTER(LEN=4),SAVE,  POINTER,DIMENSION(:)   ::HYDBASARR
        TYPE HYDBASTYPE
          INTEGER, POINTER                         ::NHYDTOT
          REAL,             POINTER,DIMENSION(:,:) ::HYDVAL
          CHARACTER(LEN=20),POINTER,DIMENSION(:)   ::HYDLBL
          INTEGER,          POINTER                ::IHYDMUN,NHYDBAS
          REAL,             POINTER                ::HYDNOH
          LOGICAL,          POINTER,DIMENSION(:)   ::IBHYDBAS
          LOGICAL,          POINTER,DIMENSION(:)   ::INTRPHYDBAS
          INTEGER,          POINTER,DIMENSION(:,:) ::JIKHYDBAS
          REAL,             POINTER,DIMENSION(:,:) ::HYDBASWT
          REAL,             POINTER,DIMENSION(:)   ::HYDBASSTRT
          CHARACTER(LEN=4), POINTER,DIMENSION(:)   ::HYDBASARR
        END TYPE
        TYPE(HYDBASTYPE),   SAVE  :: HYDBASDAT(10)
      END MODULE

      MODULE HYDIBSMODULE
        INTEGER, SAVE,         POINTER                ::NHYDIBS
        LOGICAL, SAVE,         POINTER,DIMENSION(:)   ::IBHYDIBS
        LOGICAL, SAVE,         POINTER,DIMENSION(:)   ::INTRPHYDIBS
        INTEGER, SAVE,         POINTER,DIMENSION(:,:) ::JIKHYDIBS
        REAL,    SAVE,         POINTER,DIMENSION(:,:) ::HYDIBSWT
        CHARACTER(LEN=4),SAVE, POINTER,DIMENSION(:)   ::HYDIBSARR
        TYPE HYDIBSTYPE
          INTEGER,          POINTER                ::NHYDIBS
          LOGICAL,          POINTER,DIMENSION(:)   ::IBHYDIBS
          LOGICAL,          POINTER,DIMENSION(:)   ::INTRPHYDIBS
          INTEGER,          POINTER,DIMENSION(:,:) ::JIKHYDIBS
          REAL,             POINTER,DIMENSION(:,:) ::HYDIBSWT
          CHARACTER(LEN=4), POINTER,DIMENSION(:)   ::HYDIBSARR
        END TYPE 
        TYPE(HYDIBSTYPE),  SAVE  ::HYDIBSDAT(10)
      END MODULE

      MODULE HYDSUBMODULE
        INTEGER, SAVE,         POINTER                ::NHYDSUB
        LOGICAL, SAVE,         POINTER,DIMENSION(:)   ::IBHYDSUB
        LOGICAL, SAVE,         POINTER,DIMENSION(:)   ::INTRPHYDSUB
        INTEGER, SAVE,         POINTER,DIMENSION(:,:) ::JIKHYDSUB
        REAL,    SAVE,         POINTER,DIMENSION(:,:) ::HYDSUBWT
        CHARACTER(LEN=4),SAVE, POINTER,DIMENSION(:)   ::HYDSUBARR
        TYPE HYDSUBTYPE
          INTEGER,          POINTER                ::NHYDSUB
          LOGICAL,          POINTER,DIMENSION(:)   ::IBHYDSUB
          LOGICAL,          POINTER,DIMENSION(:)   ::INTRPHYDSUB
          INTEGER,          POINTER,DIMENSION(:,:) ::JIKHYDSUB
          REAL,             POINTER,DIMENSION(:,:) ::HYDSUBWT
          CHARACTER(LEN=4), POINTER,DIMENSION(:)   ::HYDSUBARR
        END TYPE 
        TYPE(HYDSUBTYPE),  SAVE  ::HYDSUBDAT(10)
      END MODULE

      MODULE HYDSTRMODULE
        INTEGER, SAVE,         POINTER                ::NHYDSTR
        INTEGER, SAVE,         POINTER,DIMENSION(:)   ::ISTRHYD
        CHARACTER(LEN=4),SAVE, POINTER,DIMENSION(:)   ::HYDSTRARR
        TYPE HYDSTRTYPE
          INTEGER,          POINTER                ::NHYDSTR
          INTEGER,          POINTER,DIMENSION(:)   ::ISTRHYD
          CHARACTER(LEN=4), POINTER,DIMENSION(:)   ::HYDSTRARR
        END TYPE
        TYPE(HYDSTRTYPE),   SAVE   ::HYDSTRDAT(10)
      END MODULE

      MODULE HYDSFRMODULE
        INTEGER, SAVE,         POINTER                ::NHYDSFR
        INTEGER, SAVE,         POINTER,DIMENSION(:)   ::ISFRHYD
        CHARACTER(LEN=4),SAVE, POINTER,DIMENSION(:)   ::HYDSFRARR
        TYPE HYDSFRTYPE
          INTEGER,          POINTER                ::NHYDSFR
          INTEGER,          POINTER,DIMENSION(:)   ::ISFRHYD
          CHARACTER(LEN=4), POINTER,DIMENSION(:)   ::HYDSFRARR
        END TYPE
        TYPE(HYDSFRTYPE),   SAVE   ::HYDSFRDAT(10)
      END MODULE

      SUBROUTINE GWF2HYD7BAS7AR(IN,IGRID)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR HYDROGRAPH DATA FOR BAS PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL , ONLY: IOUT,NCOL,NROW,STRT
      USE HYDBASMODULE
      CHARACTER*80 LINE
      CHARACTER*1 INTYP
      CHARACTER*20 HYDBASLBL
C     ------------------------------------------------------------------
      ALLOCATE(NHYDTOT)
      ALLOCATE(IHYDMUN,NHYDBAS,HYDNOH)
      ONE=1.0
      ZERO=0.0
      NHYDTOT=0
C
C1------IDENTIFY PROGRAM.
      WRITE(IOUT,*) 'HYD:'
!      WRITE(IOUT,1) IN
!    1 FORMAT(1X,/1X,'HYD -- HYDROGRAPH DATA FOR BAS PACKAGE,',
!     1  ' VERSION 7, 07/14/2006',/
!     2  1X,'        INPUT READ FROM UNIT',I3)
C
C4------READ NUMBER OF HYDROGRAPHS AND UNIT FOR SAVING UNFORMATTED
C4------HYDROGRAPH FILE AND NUMERIC FLAG FOR DRY/INACTIVE CELLS
      READ(IN,'(A)') LINE
      LLOC=1
C  Number of hydrographs (NHYDM) specified by the user is ignored --
C    the program initially counts the number of hydrographs (NTOT).
C    Note that there may be less than NHTOT hydrograps because some
C    may be eliminated due to invalid values.
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NHYDM,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHYDMUN,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,HYDNOH,IOUT,IN)
      WRITE(IOUT,*) 'NHYDM,IHYDMUN,HYDNOH:'
      WRITE(IOUT,*) NHYDM,IHYDMUN,HYDNOH
!      WRITE(IOUT,5) IHYDMUN,HYDNOH
!    5 FORMAT(1X,'HYDROGRAPH VALUES WILL BE SAVED ON UNIT:',I4,
!     2     /,1X,'HYDROGRAPH VALUES AT DRY CELLS WILL BE:',1PG14.5)
C
C4------COUNT NUMBER OF BAS PACKAGE AND OVERALL HYDROGRAPHS.
      NTOT=0
      NHYDBAS=0
      REWIND(IN)
      READ(IN,'(A)',END=19) LINE
 10   READ(IN,'(A)',END=19) LINE
      IF(LINE.EQ.' ') GO TO 10
      NTOT=NTOT+1
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'BAS') NHYDBAS=NHYDBAS+1
      GO TO 10
C
C  ALLOCATE MEMORY FOR ALL TYPES OF HYDROGRAPHS
 19   IF(NTOT.GT.0) THEN
        ALLOCATE(HYDVAL(NTOT,2))
        ALLOCATE(HYDLBL(NTOT))
!        WRITE(IOUT,17)
! 17     FORMAT(1X,/1X,
!     1   'ARR INTYP  KLAY     XL            YL         HYDLBL',
!     2   /1X,64('-'))
      ELSE
        ALLOCATE(HYDVAL(1,2))
        ALLOCATE(HYDLBL(1))
      END IF
C
C  ALLOCATE MEMORY FOR BAS HYDROGRAPH DATA
      IF(NHYDBAS.GT.0) THEN
        ALLOCATE(IBHYDBAS(NHYDBAS))
        ALLOCATE(INTRPHYDBAS(NHYDBAS))
        ALLOCATE(JIKHYDBAS(3,NHYDBAS))
        ALLOCATE(HYDBASWT(4,NHYDBAS))
        ALLOCATE(HYDBASSTRT(NHYDBAS))
        ALLOCATE(HYDBASARR(NHYDBAS))
      ELSE
        ALLOCATE(IBHYDBAS(1))
        ALLOCATE(INTRPHYDBAS(1))
        ALLOCATE(JIKHYDBAS(3,1))
        ALLOCATE(HYDBASWT(4,1))
        ALLOCATE(HYDBASSTRT(1))
        ALLOCATE(HYDBASARR(1))
!        WRITE(IOUT,18)
!  18    FORMAT(1X,'NO HYDROGRAPHS FOR BAS PACKAGE')
        GO TO 999
      END IF
      IF(NTOT.LE.0) THEN
!        WRITE(IOUT,16)
!  16    FORMAT(1X,'NO HYDROGRAPHS FOR ANY PACKAGE')
        GO TO 999
      END IF
C
C  READ BAS HYDROGRAPH DATA
      NHYDBAS=0
      REWIND(IN)
      READ(IN,'(A)',END=99) LINE
 20   READ(IN,'(A)',END=99) LINE
      IF(LINE.EQ.' ') GO TO 20
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).NE.'BAS') GO TO 20
	Write(IOUT,*) 'PCKG:'
	Write(IOUT,*) LINE(ISTART:ISTOP)
C
C Record applies to BAS Package
      NHYDBAS=NHYDBAS+1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      HYDBASARR(NHYDBAS)=LINE(ISTART:ISTOP)
	Write(IOUT,*) 'ARR:'
	Write(IOUT,*) LINE(ISTART:ISTOP)
      WRITE(HYDBASLBL(1:2),FMT='(A2)') HYDBASARR(NHYDBAS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
	write(IOUT,*) 'INTYP:'
	Write(IOUT,*) LINE(ISTART:ISTOP)
      INTYP=LINE(ISTART:ISTOP)
      WRITE(HYDBASLBL(3:3),FMT='(A1)') LINE(ISTART:ISTOP)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KLAY,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,XL,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,YL,IOUT,IN)
	write(IOUT,*) 'KLAY XL YL:' 
	write(IOUT,*) KLAY, XL, YL 
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
	write(IOUT,*)  'HYDLBL:'
	Write(IOUT,*) LINE(ISTART:ISTOP)
      WRITE(HYDBASLBL(4:6),FMT='(I3.3)')KLAY
      HYDBASLBL(7:20)=LINE(ISTART:ISTOP)
!      WRITE(IOUT,23) HYDBASARR(NHYDBAS),INTYP,KLAY,XL,YL,HYDBASLBL
! 23   FORMAT(1X,A,1X,A,I7,1PE14.5,E14.5,2X,A)
C
C Determine if head
      IF(HYDBASARR(NHYDBAS).EQ.'HD') THEN
         IBHYDBAS(NHYDBAS)=.TRUE.
C
C Determine if drawdown
      ELSE IF(HYDBASARR(NHYDBAS).EQ.'DD') THEN
         IBHYDBAS(NHYDBAS)=.TRUE.
C
C  Not head or drawdown, so error.
      ELSE
!         WRITE(IOUT,25) LINE
! 25      FORMAT(' Invalid array type was found on the following',
!     &   ' record:',/,A80)
!         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         NHYDBAS=NHYDBAS-1
         GO TO 20
      ENDIF
C
C  Find the grid coodrdinates for the cell.
!      CALL SGWF2HYD7GRDLOC(XL,YL,NR1,NC1,NR2,NC2,X1,X2,Y1,Y2)
C
C  Check if interpolating between nodes.
!      IF(INTYP.EQ.'C') THEN
C
C  Do not interpolate
!         INTRPHYDBAS(NHYDBAS)=.FALSE.
!         IF(NR1.LT.1.OR.NR1.GT.NROW.OR.NC1.LT.1.OR.NC1.GT.NCOL) THEN
 !           WRITE(IOUT,26) LINE
 !26         FORMAT(' Coordinates of the following record are ',
 !    &           'outside of the model grid:',/,A80)
 !           WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
!            NHYDBAS=NHYDBAS-1
!            GO TO 20
!         ENDIF
!         JIKHYDBAS(1,NHYDBAS)=NC1
!         JIKHYDBAS(2,NHYDBAS)=NR1
!         JIKHYDBAS(3,NHYDBAS)=KLAY
!         HYDBASWT(1,NHYDBAS)=ONE
!         HYDBASWT(2,NHYDBAS)=ZERO
!         HYDBASWT(3,NHYDBAS)=ZERO
!         HYDBASWT(4,NHYDBAS)=ZERO
!      ELSE IF(INTYP.EQ.'I') THEN
C
C  Interpolate between cells
!         INTRPHYDBAS(NHYDBAS)=.TRUE.
!         CALL SGWF2HYD7MW(XL,YL,X1,X2,Y1,Y2,W1,W2,W3,W4)
!         IF(NR2.LT.2.OR.NR2.GT.NROW.OR.NC2.LT.1.OR.NC2.GT.(NCOL-1)) THEN
  !          WRITE(IOUT,26) LINE
!            NHYDBAS=NHYDBAS-1
!            GO TO 20
!         ENDIF
!         JIKHYDBAS(1,NHYDBAS)=NC2
!         JIKHYDBAS(2,NHYDBAS)=NR2
!         JIKHYDBAS(3,NHYDBAS)=KLAY
!         HYDBASWT(1,NHYDBAS)=W1
!         HYDBASWT(2,NHYDBAS)=W2
!         HYDBASWT(3,NHYDBAS)=W3
!         HYDBASWT(4,NHYDBAS)=W4
!      ELSE
C
C  Interpolation coding error.
!         WRITE(IOUT,27) LINE
! 27      FORMAT(' Invalid interpolation type was found on the ',
!     &   'following record:',/,A80)
!         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
!         NHYDBAS=NHYDBAS-1
!         GO TO 20
!      ENDIF
C
C  If computing drawdown, save the starting head
 !     IF(HYDBASARR(NHYDBAS).EQ.'DD') THEN
 !        IF(INTYP.EQ.'I')THEN
 !           H1=STRT(NC2,NR2,KLAY)
 !           H2=STRT(NC2+1,NR2,KLAY)
!            H3=STRT(NC2+1,NR2-1,KLAY)
!            H4=STRT(NC2,NR2-1,KLAY)
!            HYDBASSTRT(NHYDBAS)=H1*W1+H2*W2+H3*W3+H4*W4
!         ELSEIF(INTYP.EQ.'C')THEN
!            HYDBASSTRT(NHYDBAS)=STRT(NC1,NR1,KLAY)
!         ENDIF
!      ENDIF
C
C  Save the hydrograph label and continue with the next record.
!      HYDLBL(NHYDBAS)=HYDBASLBL
      GO TO 20
C
C  End of file after all BAS HYDROGRAPH data have been processed
C  Note that NHYDTOT is accumulated by each package from this point on.
C  NHYDTOT is the total number of valid hydrographs after data are
C  checked for errors.
 99   NHYDTOT=NHYDBAS
C
999   continue
!999   CALL SGWF2HYD7BAS7PSV(IGRID)
!      IF(NHYDBAS.GT.0) CALL GWF2HYD7BAS7SE(2,IGRID)
      RETURN
      END
      SUBROUTINE GWF2HYD7IBS7AR(IN,IGRID)
C     ******************************************************************
C     READ IBS PACKAGE DATA FOR HYDROGRAPHS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY: IOUT,NCOL,NROW,NLAY
      USE HYDBASMODULE
      USE HYDIBSMODULE
      USE GWFIBSMODULE,   ONLY: IBQ
C
      CHARACTER LINE*80
      CHARACTER HYDIBSLBL*20,PCKG*3,ARR*2,INTYP*1
C     ------------------------------------------------------------------
      ALLOCATE (NHYDIBS)
      ONE=1.0
      ZERO=0.0
      NHYDIBS=0
      REWIND(IN)
C
      READ(IN,'(A)',END=19) LINE
 10   READ(IN,'(A)',END=19) LINE
      IF(LINE.EQ.' ') GO TO 10
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'IBS') NHYDIBS=NHYDIBS+1
      GO TO 10
C
C  ALLOCATE MEMORY FOR IBS HYDROGRAPH DATA
 19   IF(NHYDIBS.GT.0) THEN
        ALLOCATE(IBHYDIBS(NHYDIBS))
        ALLOCATE(INTRPHYDIBS(NHYDIBS))
        ALLOCATE(JIKHYDIBS(3,NHYDIBS))
        ALLOCATE(HYDIBSWT(4,NHYDIBS))
        ALLOCATE(HYDIBSARR(NHYDIBS))
      ELSE
        ALLOCATE(IBHYDIBS(1))
        ALLOCATE(INTRPHYDIBS(1))
        ALLOCATE(JIKHYDIBS(3,1))
        ALLOCATE(HYDIBSWT(4,1))
        ALLOCATE(HYDIBSARR(1))
!        WRITE(IOUT,18)
!  18    FORMAT(1X,'NO HYDROGRAPHS FOR IBS PACKAGE')
        GO TO 999
      END IF
C
C  Read IBS hydrograph data.
      WRITE(IOUT,*) 'HYD:'
      NHYDIBS=0
      REWIND(IN)
      READ(IN,'(A)',END=99) LINE
 15   READ(IN,'(A)',END=99) LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).NE.'IBS') GO TO 15
	Write(IOUT,*) 'PCKG:'
	Write(IOUT,*) LINE(ISTART:ISTOP)
      NHYDIBS=NHYDIBS+1
      PCKG=LINE(ISTART:ISTOP)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
	Write(IOUT,*) 'ARR:'
	Write(IOUT,*) LINE(ISTART:ISTOP)
      ARR=LINE(ISTART:ISTOP)
      WRITE(HYDIBSLBL(1:2),FMT='(A2)')ARR
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
	write(IOUT,*) 'INTYP:'
	Write(IOUT,*) LINE(ISTART:ISTOP)
      INTYP=LINE(ISTART:ISTOP)
      WRITE(HYDIBSLBL(3:3),FMT='(A1)')INTYP
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KLAY,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,XL,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,YL,IOUT,IN)
	write(IOUT,*) 'KLAY XL YL:' 
	write(IOUT,*) KLAY, XL, YL 
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
	write(IOUT,*)  'HYDLBL:'
	Write(IOUT,*) LINE(ISTART:ISTOP)
      WRITE(HYDIBSLBL(4:6),FMT='(I3.3)')KLAY
      HYDIBSLBL(7:20)=LINE(ISTART:ISTOP)
cc    TIME SERIES FROM THE CRITICAL HEAD ARRAY
      IF (ARR.EQ.'HC') THEN
         HYDIBSARR(NHYDIBS)='HC'
         IBHYDIBS(NHYDIBS)=.TRUE.
cc    TIME SERIES FROM THE COMPACTION ARRAY
      ELSE IF (ARR.EQ.'CP') THEN
         HYDIBSARR(NHYDIBS)='CP'
         IBHYDIBS(NHYDIBS)=.TRUE.
cc    TIME SERIES ACCUMULATED OVER ALL LAYERS OF THE COMPACTION ARRAY
      ELSE IF (ARR.EQ.'SB') THEN
         HYDIBSARR(NHYDIBS)='SB'
         IBHYDIBS(NHYDIBS)=.FALSE.
C  Change the layer number to the number of subsidence layers
         NQ=0
         DO 20 K=1,KLAY
         IF(IBQ(K).GT.0) NQ=NQ+1
 20      CONTINUE
         KLAY=NQ
      ELSE
!         WRITE(IOUT,25) LINE
! 25      FORMAT(' Invalid array type was found on the following',
!     &   ' record:',/,A80)
!         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         NHYDIBS=NHYDIBS-1
         GO TO 15
      ENDIF
 !     WRITE(IOUT,23) HYDIBSARR(NHYDIBS),INTYP,KLAY,XL,YL,HYDIBSLBL
 !23   FORMAT(1X,A,1X,A,I7,1PE14.5,E14.5,2X,A)
C
C  Get the cell location
!      CALL SGWF2HYD7GRDLOC(XL,YL,NR1,NC1,NR2,NC2,X1,X2,Y1,Y2)
!      IF(INTYP.EQ.'C') THEN
C  Use cell value without interpolation
!         INTRPHYDIBS(NHYDIBS)=.FALSE.
!         IF(NR1.LT.1.OR.NR1.GT.NROW.OR.NC1.LT.1.OR.NC1.GT.NCOL) THEN
 !           WRITE(IOUT,26) LINE
 !26         FORMAT(' Coordinates of the following record are ',
 !    &           'outside of the model grid:',/,A80)
 !           WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
!            NHYDIBS=NHYDIBS-1
!            GO TO 15
!         ENDIF
!         JIKHYDIBS(1,NHYDIBS)=NC1
!         JIKHYDIBS(2,NHYDIBS)=NR1
!         JIKHYDIBS(3,NHYDIBS)=KLAY
!         HYDIBSWT(1,NHYDIBS)=ONE
!         HYDIBSWT(2,NHYDIBS)=ZERO
!         HYDIBSWT(3,NHYDIBS)=ZERO
!         HYDIBSWT(4,NHYDIBS)=ZERO
!      ELSE IF(INTYP.EQ.'I') THEN
C  Interpolate value between nodes
!         INTRPHYDIBS(NHYDIBS)=.TRUE.
!         CALL SGWF2HYD7MW(XL,YL,X1,X2,Y1,Y2,W1,W2,W3,W4)
!         IF(NR2.LT.2.OR.NR2.GT.NROW.OR.NC2.LT.1.OR.NC2.GT.NCOL-1) THEN
!            WRITE(IOUT,26) LINE
!            NHYDIBS=NHYDIBS-1
!            GO TO 15
!         ENDIF
!         JIKHYDIBS(1,NHYDIBS)=NC2
!         JIKHYDIBS(2,NHYDIBS)=NR2
!         JIKHYDIBS(3,NHYDIBS)=KLAY
!         HYDIBSWT(1,NHYDIBS)=W1
!         HYDIBSWT(2,NHYDIBS)=W2
!         HYDIBSWT(3,NHYDIBS)=W3
!         HYDIBSWT(4,NHYDIBS)=W4
!      ELSE
!         WRITE(IOUT,27) LINE
! 27      FORMAT(' Invalid interpolation type was found on the ',
!     &   'following hydrograph record:',/,A80)
!         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
!         NHYDIBS=NHYDIBS-1
!         GO TO 15
!      ENDIF
C
C  Save the hydrograph label and process the next line in input file
!      HYDLBL(NHYDTOT+NHYDIBS)=HYDIBSLBL
      GO TO 15
C
C  End of file after all IBS hydrograph data have been processed
 99   IF(NHYDIBS.GT.0) THEN
!        WRITE(IOUT,108) NHYDIBS
!108     FORMAT(' A total of ',I3,' points have been added ',
!     & 'for the hydrographs of IBS arrays.')
      END IF
C
 999  continue
! 999  CALL SGWF2HYD7IBS7PSV(IGRID)
!      IF(NHYDIBS.GT.0) CALL GWF2HYD7IBS7SE(2,IGRID)
      RETURN
      END
      SUBROUTINE GWF2HYD7SUB7AR(IN,IGRID)
C     ******************************************************************
C     READ SUB PACKAGE DATA FOR HYDROGRAPHS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY: IOUT,NCOL,NROW,NLAY
      USE HYDBASMODULE
      USE HYDSUBMODULE
      USE GWFSUBMODULE,   ONLY: LN,HC,SUB,NNDB 
C
      CHARACTER LINE*80
      CHARACTER HYDSUBLBL*20,PCKG*3,ARR*2,INTYP*1
C     ------------------------------------------------------------------
      ALLOCATE (NHYDSUB)
      ONE=1.0
      ZERO=0.0
      NHYDSUB=0
      REWIND(IN)
C
      READ(IN,'(A)',END=19) LINE
 10   READ(IN,'(A)',END=19) LINE
      IF(LINE.EQ.' ') GO TO 10
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'SUB') NHYDSUB=NHYDSUB+1
      GO TO 10
C
C  ALLOCATE MEMORY FOR SUB HYDROGRAPH DATA
 19   IF(NHYDSUB.GT.0) THEN
        ALLOCATE(IBHYDSUB(NHYDSUB))
        ALLOCATE(INTRPHYDSUB(NHYDSUB))
        ALLOCATE(JIKHYDSUB(3,NHYDSUB))
        ALLOCATE(HYDSUBWT(4,NHYDSUB))
        ALLOCATE(HYDSUBARR(NHYDSUB))
      ELSE
        ALLOCATE(IBHYDSUB(1))
        ALLOCATE(INTRPHYDSUB(1))
        ALLOCATE(JIKHYDSUB(3,1))
        ALLOCATE(HYDSUBWT(4,1))
        ALLOCATE(HYDSUBARR(1))
!        WRITE(IOUT,18)
!  18    FORMAT(1X,'NO HYDROGRAPHS FOR SUB PACKAGE')
        GO TO 999
      END IF
C
C  Read SUB hydrograph data.
      WRITE(IOUT,*) 'HYD:'
      NHYDSUB=0
      REWIND(IN)
      READ(IN,'(A)',END=99) LINE
 15   READ(IN,'(A)',END=99) LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).NE.'SUB') GO TO 15
	Write(IOUT,*) 'PCKG:'
	Write(IOUT,*) LINE(ISTART:ISTOP)
      NHYDSUB=NHYDSUB+1
      PCKG=LINE(ISTART:ISTOP)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
	Write(IOUT,*) 'ARR:'
	Write(IOUT,*) LINE(ISTART:ISTOP)
      ARR=LINE(ISTART:ISTOP)
      WRITE(HYDSUBLBL(1:2),FMT='(A2)')ARR
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
	write(IOUT,*) 'INTYP:'
	Write(IOUT,*) LINE(ISTART:ISTOP)
      INTYP=LINE(ISTART:ISTOP)
      WRITE(HYDSUBLBL(3:3),FMT='(A1)')INTYP
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KLAY,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,XL,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,YL,IOUT,IN)
	write(IOUT,*) 'KLAY XL YL:' 
	write(IOUT,*) KLAY, XL, YL 
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
	write(IOUT,*)  'HYDLBL:'
	Write(IOUT,*) LINE(ISTART:ISTOP)
      WRITE(HYDSUBLBL(4:6),FMT='(I3.3)')KLAY
      HYDSUBLBL(7:20)=LINE(ISTART:ISTOP)
cc    TIME SERIES FROM THE CRITICAL HEAD ARRAY
      IF (ARR.EQ.'HC') THEN
         HYDSUBARR(NHYDSUB)='HC'
         IBHYDSUB(NHYDSUB)=.TRUE.
cc    TIME SERIES FROM THE COMPACTION ARRAY
      ELSE IF (ARR.EQ.'CP') THEN
         HYDSUBARR(NHYDSUB)='CP'
         IBHYDSUB(NHYDSUB)=.TRUE.
cc    TIME SERIES ACCUMULATED OVER ALL LAYERS OF THE COMPACTION ARRAY
      ELSE IF (ARR.EQ.'SB') THEN
         HYDSUBARR(NHYDSUB)='SB'
         IBHYDSUB(NHYDSUB)=.FALSE.
C  Change the layer number to the number of subsidence layers
         NQSUB=0
         DO 20 K=1,KLAY
         IF(LN(K).GT.0) NQSUB=NQSUB+1
 20      CONTINUE
         KLAY=NQSUB
      ELSE
!         WRITE(IOUT,25) LINE
! 25      FORMAT(' Invalid array type was found on the following',
!     &   ' record:',/,A80)
!         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         NHYDSUB=NHYDSUB-1
         GO TO 15
      ENDIF
!      WRITE(IOUT,23) HYDSUBARR(NHYDSUB),INTYP,KLAY,XL,YL,HYDSUBLBL
! 23   FORMAT(1X,A,1X,A,I7,1PE14.5,E14.5,2X,A)
C
C  Get the cell location
!      CALL SGWF2HYD7GRDLOC(XL,YL,NR1,NC1,NR2,NC2,X1,X2,Y1,Y2)
!      IF(INTYP.EQ.'C') THEN
C  Use cell value without interpolation
!         INTRPHYDSUB(NHYDSUB)=.FALSE.
!         IF(NR1.LT.1.OR.NR1.GT.NROW.OR.NC1.LT.1.OR.NC1.GT.NCOL) THEN
!            WRITE(IOUT,26) LINE
! 26         FORMAT(' Coordinates of the following record are ',
!     &           'outside of the model grid:',/,A80)
!            WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
!            NHYDSUB=NHYDSUB-1
!            GO TO 15
!         ENDIF
!         JIKHYDSUB(1,NHYDSUB)=NC1
!         JIKHYDSUB(2,NHYDSUB)=NR1
!         JIKHYDSUB(3,NHYDSUB)=KLAY
!         HYDSUBWT(1,NHYDSUB)=ONE
!         HYDSUBWT(2,NHYDSUB)=ZERO
!         HYDSUBWT(3,NHYDSUB)=ZERO
!         HYDSUBWT(4,NHYDSUB)=ZERO
!      ELSE IF(INTYP.EQ.'I') THEN
C  Interpolate value between nodes
!         INTRPHYDSUB(NHYDSUB)=.TRUE.
!         CALL SGWF2HYD7MW(XL,YL,X1,X2,Y1,Y2,W1,W2,W3,W4)
!         IF(NR2.LT.2.OR.NR2.GT.NROW.OR.NC2.LT.1.OR.NC2.GT.NCOL-1) THEN
!            WRITE(IOUT,26) LINE
!            NHYDSUB=NHYDSUB-1
!            GO TO 15
!         ENDIF
!         JIKHYDSUB(1,NHYDSUB)=NC2
!         JIKHYDSUB(2,NHYDSUB)=NR2
!         JIKHYDSUB(3,NHYDSUB)=KLAY
!         HYDSUBWT(1,NHYDSUB)=W1
!         HYDSUBWT(2,NHYDSUB)=W2
!         HYDSUBWT(3,NHYDSUB)=W3
!         HYDSUBWT(4,NHYDSUB)=W4
!      ELSE
!         WRITE(IOUT,27) LINE
! 27      FORMAT(' Invalid interpolation type was found on the ',
!     &   'following hydrograph record:',/,A80)
!         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
!         NHYDSUB=NHYDSUB-1
!         GO TO 15
!      ENDIF
C
C  Save the hydrograph label and process the next line in input file
      HYDLBL(NHYDTOT+NHYDSUB)=HYDSUBLBL
      GO TO 15
C
C  End of file after all SUB hydrograph data have been processed
 99   IF(NHYDSUB.GT.0) THEN
!        WRITE(IOUT,108) NHYDSUB
!108     FORMAT(' A total of ',I3,' points have been added ',
!     & 'for the hydrographs of SUB arrays.')
      END IF
C
 999  continue
! 999  CALL SGWF2HYD7SUB7PSV(IGRID)
 !     IF(NHYDSUB.GT.0) CALL GWF2HYD7SUB7SE(2,IGRID)
      RETURN
      END
      SUBROUTINE GWF2HYD7STR7AR(IN,IGRID)
C     ******************************************************************
C     READ STREAM PACKAGE DATA FOR HYDROGRAPHS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,    ONLY: IOUT
      USE HYDBASMODULE
      USE HYDSTRMODULE
      CHARACTER LINE*80
C     ------------------------------------------------------------------
      ALLOCATE (NHYDSTR)
C  Count number of Stream hydrographs
      NHYDSTR=0
      REWIND(IN)
      READ(IN,'(A)',END=15) LINE
 10   READ(IN,'(A)',END=15) LINE
      IF(LINE.EQ.' ') GO TO 10
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'STR') NHYDSTR=NHYDSTR+1
      GO TO 10
C
C  Allocate memory
 15   IF(NHYDSTR.GT.0) THEN
        ALLOCATE (ISTRHYD(NHYDSTR))
        ALLOCATE (HYDSTRARR(NHYDSTR))
      ELSE
        ALLOCATE (ISTRHYD(1))
        ALLOCATE (HYDSTRARR(1))
      END IF
C
C ------RETURN.
      CALL SGWF2HYD7STR7PSV(IGRID)
      RETURN
      END
      SUBROUTINE GWF2HYD7STR7RP(IN,KPER,IGRID)
C     ******************************************************************
C     READ HYDROGRAPH RECORDS FOR STREAMS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY: IBOUND,IOUT
      USE HYDBASMODULE
      USE HYDSTRMODULE
      USE GWFSTRMODULE, ONLY: ISTRM,STRM,NSTREM
      CHARACTER HYDSTRLBL*20,LINE*80,INTYP*1
C     ------------------------------------------------------------------
      CALL SGWF2HYD7STR7PNT(IGRID)
C
      IF(NSTREM.LT.1)THEN
!         WRITE(IOUT,8)
!  8      FORMAT(' No Active Streams in this Model')
!         WRITE(IOUT,'(1X,A)')
!     1        'Stream hydrograph records will be ignored.'
         NHYDSTR=0
         RETURN
      END IF
C
C
C ------Read STR hydrograph data
C ------Reading is done here (rather than in AR) because stream data are
C ------not available until the first time step is initiated.
C ---- Reading could be done in a special RP routine right after STRRP
      IF(KPER.EQ.1) THEN
      WRITE(IOUT,*) 'HYD:'
        NUMSTR=0
        REWIND(IN)
        READ(IN,'(A)',END=99) LINE
 20     READ(IN,'(A)',END=99) LINE
        IF(LINE.EQ.' ') GO TO 20
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).NE.'STR') GO TO 20
	Write(IOUT,*) 'PCKG:'
	Write(IOUT,*) LINE(ISTART:ISTOP)
C
C ------PROCESS A STR HYDROGRAPH RECORD
        NUMSTR=NUMSTR+1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
	Write(IOUT,*) 'ARR:'
	Write(IOUT,*) LINE(ISTART:ISTOP)
        HYDSTRARR(NUMSTR)=LINE(ISTART:ISTOP)
        WRITE(HYDSTRLBL(1:2),FMT='(A2)') HYDSTRARR(NUMSTR)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
	write(IOUT,*) 'INTYP:'
	Write(IOUT,*) LINE(ISTART:ISTOP)
        INTYP=LINE(ISTART:ISTOP)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KLAY,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,XL,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,YL,IOUT,IN)
	write(IOUT,*) 'KLAY XL YL:' 
	write(IOUT,*) KLAY, XL, YL 
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
	write(IOUT,*)  'HYDLBL:'
	Write(IOUT,*) LINE(ISTART:ISTOP)
        WRITE(HYDSTRLBL(3:5),FMT='(I3.3)') INT(XL)
        WRITE(HYDSTRLBL(6:8),FMT='(I3.3)') INT(YL)
        HYDSTRLBL(9:20)=LINE(ISTART:ISTOP)
C
        IF(HYDSTRARR(NUMSTR).NE.'ST' .AND.
     1      HYDSTRARR(NUMSTR).NE.'SO' .AND.
     2      HYDSTRARR(NUMSTR).NE.'SI' .AND.
     3      HYDSTRARR(NUMSTR).NE.'SA') THEN
!           WRITE(IOUT,25) LINE
! 25        FORMAT(' Invalid streamflow array was found on the following'
!     &     ,' record:',/,A80)
!           WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
           NUMSTR=NUMSTR-1
           GO TO 20
        ENDIF
C
C ------Look through all reaches to find the hydrograph reach.
        DO 30 N=1,NSTREM
 26       ISEG=ISTRM(4,N)
          IRCH=ISTRM(5,N)
C    XL contains the SEGMENT number and YL contains the REACH number. 
          IF(ISEG.EQ.INT(XL).AND.IRCH.EQ.INT(YL))THEN
            ISTRHYD(NUMSTR)=N
            GO TO 35
          END IF
 30     CONTINUE
!        WRITE(IOUT,*)
!     1      ' Hydrograph specified for non-existent stream reach'
!        WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
        NUMSTR=NUMSTR-1
        GO TO 20
C
C ------The interpolation type must be 'C'
 35     IF(INTYP.NE.'C') THEN
!           WRITE(IOUT,39) LINE
! 39        FORMAT(' Invalid interpolation type was found on the ',
!     &     'following record:',/,A80)
!           WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
           NUMSTR=NUMSTR-1
           GO TO 20
        ENDIF
C
C ------SAVE THE LABEL AND PROCESS NEXT RECORD.
        HYDLBL(NHYDTOT+NUMSTR)=HYDSTRLBL
        GO TO 20
C
C ------End of input file.
 99    continue 
! 99     WRITE(IOUT,100) NUMSTR
!100     FORMAT(' A total of ',I3,' hydrographs have been added ',
!     &   'for STR arrays.')
        NHYDSTR=NUMSTR
C
C  Create initial values for stream hydrographs
!        IF(NHYDSTR.GT.0) CALL GWF2HYD7STR7SE(2,IGRID)
      END IF
C
C ------RETURN
      RETURN
      END
      SUBROUTINE GWF2HYD7SFR7AR(IN,IGRID)
C     ******************************************************************
C     READ SFR2 PACKAGE DATA FOR HYDROGRAPHS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,    ONLY: IOUT
      USE HYDBASMODULE
      USE HYDSFRMODULE
      CHARACTER LINE*80
C     ------------------------------------------------------------------
      ALLOCATE (NHYDSFR)
C  Count number of Stream hydrographs
      NHYDSFR=0
      REWIND(IN)
      READ(IN,'(A)',END=15) LINE
 10   READ(IN,'(A)',END=15) LINE
      IF(LINE.EQ.' ') GO TO 10
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'SFR') NHYDSFR=NHYDSFR+1
      GO TO 10
C
C  Allocate memory
 15   IF(NHYDSFR.GT.0) THEN
        ALLOCATE (ISFRHYD(NHYDSFR))
        ALLOCATE (HYDSFRARR(NHYDSFR))
      ELSE
        ALLOCATE (ISFRHYD(1))
        ALLOCATE (HYDSFRARR(1))
      END IF
C
C ------RETURN.
      CALL SGWF2HYD7SFR7PSV(IGRID)
      RETURN
      END
      SUBROUTINE GWF2HYD7SFR7RP(IN,KPER,IGRID)
C     ******************************************************************
C     READ HYDROGRAPH RECORDS FOR STREAMS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY: IBOUND,IOUT
      USE HYDBASMODULE
      USE HYDSFRMODULE
      USE GWFSFRMODULE, ONLY: ISTRM,STRM,NSTRM
      CHARACTER HYDSFRLBL*20,LINE*80,INTYP*1
C     ------------------------------------------------------------------
      CALL SGWF2HYD7SFR7PNT(IGRID)
C
      IF(NSTRM.LT.1)THEN
!         WRITE(IOUT,8)
!  8      FORMAT(' No Active Streams in this Model')
!         WRITE(IOUT,'(1X,A)')
!     1        'Stream hydrograph records will be ignored.'
         NHYDSFR=0
         RETURN
      END IF
C
C
C ------Read SFR hydrograph data
C ------Reading is done here (rather than in AR) because stream data are
C ------not available until the first time step is initiated.
C ---- Reading could be done in a special RP routine right after SFRRP
      IF(KPER.EQ.1) THEN
      WRITE(IOUT,*) 'HYD:'
        NUMSFR=0
        REWIND(IN)
        READ(IN,'(A)',END=99) LINE
 20     READ(IN,'(A)',END=99) LINE
        IF(LINE.EQ.' ') GO TO 20
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).NE.'SFR') GO TO 20
	Write(IOUT,*) 'PCKG:'
	Write(IOUT,*) LINE(ISTART:ISTOP)
C
C ------PROCESS A SFR HYDROGRAPH RECORD
        NUMSFR=NUMSFR+1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
	Write(IOUT,*) 'ARR:'
	Write(IOUT,*) LINE(ISTART:ISTOP)
        HYDSFRARR(NUMSFR)=LINE(ISTART:ISTOP)
        WRITE(HYDSFRLBL(1:2),FMT='(A2)') HYDSFRARR(NUMSFR)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
	write(IOUT,*) 'INTYP:'
	Write(IOUT,*) LINE(ISTART:ISTOP)
        INTYP=LINE(ISTART:ISTOP)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KLAY,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,XL,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,YL,IOUT,IN)
	write(IOUT,*) 'KLAY XL YL:' 
	write(IOUT,*) KLAY, XL, YL 
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
	write(IOUT,*)  'HYDLBL:'
	Write(IOUT,*) LINE(ISTART:ISTOP)
        WRITE(HYDSFRLBL(3:5),FMT='(I3.3)') INT(XL)
        WRITE(HYDSFRLBL(6:8),FMT='(I3.3)') INT(YL)
        HYDSFRLBL(9:20)=LINE(ISTART:ISTOP)
C
        IF(HYDSFRARR(NUMSFR).NE.'ST' .AND.
     1      HYDSFRARR(NUMSFR).NE.'SO' .AND.
     2      HYDSFRARR(NUMSFR).NE.'SI' .AND.
     3      HYDSFRARR(NUMSFR).NE.'SA') THEN
!           WRITE(IOUT,25) LINE
! 25        FORMAT(' Invalid SFR array was found on the following'
!     &     ,' record:',/,A80)
!           WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
           NUMSFR=NUMSFR-1
           GO TO 20
        ENDIF
C
C ------Look through all reaches to find the hydrograph reach.
        DO 30 N=1,NSTRM
 26       ISEG=ISTRM(4,N)
          IRCH=ISTRM(5,N)
C    XL contains the SEGMENT number and YL contains the REACH number. 
          IF(ISEG.EQ.INT(XL).AND.IRCH.EQ.INT(YL))THEN
            ISFRHYD(NUMSFR)=N
            GO TO 35
          END IF
 30     CONTINUE
!        WRITE(IOUT,*)
!     1      ' Hydrograph specified for non-existent stream reach'
!        WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
        NUMSFR=NUMSFR-1
        GO TO 20
C
C ------The interpolation type must be 'C'
 35     IF(INTYP.NE.'C') THEN
!           WRITE(IOUT,39) LINE
! 39        FORMAT(' Invalid interpolation type was found on the ',
!     &     'following record:',/,A80)
!           WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
           NUMSFR=NUMSFR-1
           GO TO 20
        ENDIF
C
C ------SAVE THE LABEL AND PROCESS NEXT RECORD.
        HYDLBL(NHYDTOT+NUMSFR)=HYDSFRLBL
        GO TO 20
C
C ------End of input file.
 99   continue
! 99     WRITE(IOUT,100) NUMSFR
!100     FORMAT(' A total of ',I3,' hydrographs have been added ',
!     &   'for SFR arrays.')
        NHYDSFR=NUMSFR
C
C  Create initial values for stream hydrographs
!        IF(NHYDSFR.GT.0) CALL GWF2HYD7SFR7SE(2,IGRID)
      END IF
C
C ------RETURN
      RETURN
      END
!      SUBROUTINE GWF2HYD7BAS7SE(IHYDLOC,IGRID)
C     ******************************************************************
C     COMPUTE HYDROGRAPH RECORDS FOR BAS
C     ******************************************************************
!      SUBROUTINE GWF2HYD7IBS7SE(IHYDLOC,IGRID)
C     ******************************************************************
C     COMPUTE HYDROGRAPH RECORDS FOR IBS
C     ******************************************************************
!      SUBROUTINE GWF2HYD7SUB7SE(IHYDLOC,IGRID)
C     ******************************************************************
C     COMPUTE HYDROGRAPH RECORDS FOR SUB
C     ******************************************************************
!      SUBROUTINE GWF2HYD7STR7SE(IHYDLOC,IGRID)
C     ******************************************************************
C     COMPUTE HYDROGRAPH RECORDS FOR STREAMS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
!      SUBROUTINE GWF2HYD7SFR7SE(IHYDLOC,IGRID)
C     ******************************************************************
C     COMPUTE HYDROGRAPH RECORDS FOR STREAMS
C     ******************************************************************
!      SUBROUTINE GWF2HYD7BAS7OT(KSTP,KPER,IGRID)
C     ******************************************************************
C     WRITE HYDROGRAPH DATA FOR ONE TIME STEP
C     ******************************************************************
!      SUBROUTINE SGWF2HYD7GRDLOC(XL,YL,NR1,NC1,NR2,NC2,XX1,XX2,YY1,YY2)
C     ******************************************************************
C     LOCATE CELLS FOR HYDROGRAPH POINTS
C     ******************************************************************
!      FUNCTION SHYD7WTAVG(N)
C     ******************************************************************
C     COMPUTE WEIGHTED AVERAGE OF HEAD
C     ******************************************************************
!      SUBROUTINE SGWF2HYD7MW(X0,Y0,X1,X2,Y1,Y2,W1,W2,W3,W4)
C     ******************************************************************
C     COMPUTE WEIGHTS FOR BILINEAR INTERPOLATION
C     ******************************************************************
      SUBROUTINE GWF2HYD7DA(IGRID)
C     ******************************************************************
C     SUBROUTINE TO DEALLOCATE ALL HYDMOD VARIABLES
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY: IUNIT
C     ------------------------------------------------------------------
C1------DEALLOCATE HYDBAS VARIABLES      
      CALL SGWF2HYD7BAS7DA(IGRID)
C      
C2------CHECK IF HYDMOD IS USED WITH OTHER PACKAGES.  IF SO, DEALLOCATE
      IF(IUNIT(43).GT.0 .AND. IUNIT(19).GT.0) 
     1                   CALL SGWF2HYD7IBS7DA(IGRID)
      IF(IUNIT(43).GT.0 .AND. IUNIT(54).GT.0) 
     1                   CALL SGWF2HYD7SUB7DA(IGRID)
      IF(IUNIT(43).GT.0 .AND. IUNIT(18).GT.0) 
     1                   CALL SGWF2HYD7STR7DA(IGRID)
      IF(IUNIT(43).GT.0 .AND. IUNIT(44).GT.0) 
     1                   CALL SGWF2HYD7SFR7DA(IGRID)
C3
      RETURN
      END
      SUBROUTINE SGWF2HYD7BAS7DA(IGRID)
C  Deallocate HYD BAS memory
      USE HYDBASMODULE
C
      IF (ASSOCIATED(HYDBASDAT(IGRID)%NHYDTOT))  
	1   DEALLOCATE(HYDBASDAT(IGRID)%NHYDTOT)
      IF (ASSOCIATED(HYDBASDAT(IGRID)%HYDVAL))
     1   DEALLOCATE(HYDBASDAT(IGRID)%HYDVAL)
      IF (ASSOCIATED(HYDBASDAT(IGRID)%HYDLBL))
     1   DEALLOCATE(HYDBASDAT(IGRID)%HYDLBL)
      IF (ASSOCIATED(HYDBASDAT(IGRID)%IHYDMUN))
     1   DEALLOCATE(HYDBASDAT(IGRID)%IHYDMUN)
      IF (ASSOCIATED(HYDBASDAT(IGRID)%NHYDBAS))
     1   DEALLOCATE(HYDBASDAT(IGRID)%NHYDBAS)
      IF (ASSOCIATED(HYDBASDAT(IGRID)%HYDNOH))
     1   DEALLOCATE(HYDBASDAT(IGRID)%HYDNOH)
      IF (ASSOCIATED(HYDBASDAT(IGRID)%IBHYDBAS))
     1   DEALLOCATE(HYDBASDAT(IGRID)%IBHYDBAS)
      IF (ASSOCIATED(HYDBASDAT(IGRID)%INTRPHYDBAS))
     1   DEALLOCATE(HYDBASDAT(IGRID)%INTRPHYDBAS)
      IF (ASSOCIATED(HYDBASDAT(IGRID)%JIKHYDBAS))
     1   DEALLOCATE(HYDBASDAT(IGRID)%JIKHYDBAS)
      IF (ASSOCIATED(HYDBASDAT(IGRID)%HYDBASWT))
     1   DEALLOCATE(HYDBASDAT(IGRID)%HYDBASWT)
      IF (ASSOCIATED(HYDBASDAT(IGRID)%HYDBASSTRT))
     1   DEALLOCATE(HYDBASDAT(IGRID)%HYDBASSTRT)
      IF (ASSOCIATED(HYDBASDAT(IGRID)%HYDBASARR))
     1   DEALLOCATE(HYDBASDAT(IGRID)%HYDBASARR)
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7BAS7PNT(IGRID)
C  Change HYD BAS data to a different grid.
      USE HYDBASMODULE
C
      NHYDTOT=>HYDBASDAT(IGRID)%NHYDTOT
      HYDVAL=>HYDBASDAT(IGRID)%HYDVAL
      HYDLBL=>HYDBASDAT(IGRID)%HYDLBL
      IHYDMUN=>HYDBASDAT(IGRID)%IHYDMUN
      NHYDBAS=>HYDBASDAT(IGRID)%NHYDBAS
      HYDNOH=>HYDBASDAT(IGRID)%HYDNOH
      IBHYDBAS=>HYDBASDAT(IGRID)%IBHYDBAS
      INTRPHYDBAS=>HYDBASDAT(IGRID)%INTRPHYDBAS
      JIKHYDBAS=>HYDBASDAT(IGRID)%JIKHYDBAS
      HYDBASWT=>HYDBASDAT(IGRID)%HYDBASWT
      HYDBASSTRT=>HYDBASDAT(IGRID)%HYDBASSTRT
      HYDBASARR=>HYDBASDAT(IGRID)%HYDBASARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7BAS7PSV(IGRID)
C  Save HYD BAS data for a grid.
      USE HYDBASMODULE
C
      HYDBASDAT(IGRID)%NHYDTOT=>NHYDTOT
      HYDBASDAT(IGRID)%HYDVAL=>HYDVAL
      HYDBASDAT(IGRID)%HYDLBL=>HYDLBL
      HYDBASDAT(IGRID)%IHYDMUN=>IHYDMUN
      HYDBASDAT(IGRID)%NHYDBAS=>NHYDBAS
      HYDBASDAT(IGRID)%HYDNOH=>HYDNOH
      HYDBASDAT(IGRID)%IBHYDBAS=>IBHYDBAS
      HYDBASDAT(IGRID)%INTRPHYDBAS=>INTRPHYDBAS
      HYDBASDAT(IGRID)%JIKHYDBAS=>JIKHYDBAS
      HYDBASDAT(IGRID)%HYDBASWT=>HYDBASWT
      HYDBASDAT(IGRID)%HYDBASSTRT=>HYDBASSTRT
      HYDBASDAT(IGRID)%HYDBASARR=>HYDBASARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7IBS7DA(IGRID)
C  Deallocate HYD IBS memory
      USE HYDIBSMODULE
C
      IF (ASSOCIATED(HYDIBSDAT(IGRID)%NHYDIBS))
     1   DEALLOCATE(HYDIBSDAT(IGRID)%NHYDIBS)
      IF (ASSOCIATED(HYDIBSDAT(IGRID)%IBHYDIBS))
     1   DEALLOCATE(HYDIBSDAT(IGRID)%IBHYDIBS)
      IF (ASSOCIATED(HYDIBSDAT(IGRID)%INTRPHYDIBS))
     1   DEALLOCATE(HYDIBSDAT(IGRID)%INTRPHYDIBS)
      IF (ASSOCIATED(HYDIBSDAT(IGRID)%JIKHYDIBS))
     1   DEALLOCATE(HYDIBSDAT(IGRID)%JIKHYDIBS)
      IF (ASSOCIATED(HYDIBSDAT(IGRID)%HYDIBSWT))
     1   DEALLOCATE(HYDIBSDAT(IGRID)%HYDIBSWT)
      IF (ASSOCIATED(HYDIBSDAT(IGRID)%HYDIBSARR))
     1   DEALLOCATE(HYDIBSDAT(IGRID)%HYDIBSARR)
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7IBS7PNT(IGRID)
C  Change HYD IBS data to a different grid.
      USE HYDIBSMODULE
C
      NHYDIBS=>HYDIBSDAT(IGRID)%NHYDIBS
      IBHYDIBS=>HYDIBSDAT(IGRID)%IBHYDIBS
      INTRPHYDIBS=>HYDIBSDAT(IGRID)%INTRPHYDIBS
      JIKHYDIBS=>HYDIBSDAT(IGRID)%JIKHYDIBS
      HYDIBSWT=>HYDIBSDAT(IGRID)%HYDIBSWT
      HYDIBSARR=>HYDIBSDAT(IGRID)%HYDIBSARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7IBS7PSV(IGRID)
C  Save HYD IBS data for a grid.
      USE HYDIBSMODULE
C
      HYDIBSDAT(IGRID)%NHYDIBS=>NHYDIBS
      HYDIBSDAT(IGRID)%IBHYDIBS=>IBHYDIBS
      HYDIBSDAT(IGRID)%INTRPHYDIBS=>INTRPHYDIBS
      HYDIBSDAT(IGRID)%JIKHYDIBS=>JIKHYDIBS
      HYDIBSDAT(IGRID)%HYDIBSWT=>HYDIBSWT
      HYDIBSDAT(IGRID)%HYDIBSARR=>HYDIBSARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7SUB7DA(IGRID)
C  Deallocate HYD SUB memory
      USE HYDSUBMODULE
C
      IF (ASSOCIATED(HYDSUBDAT(IGRID)%NHYDSUB))
     1   DEALLOCATE(HYDSUBDAT(IGRID)%NHYDSUB)
      IF (ASSOCIATED(HYDSUBDAT(IGRID)%IBHYDSUB))
     1   DEALLOCATE(HYDSUBDAT(IGRID)%IBHYDSUB)
      IF (ASSOCIATED(HYDSUBDAT(IGRID)%INTRPHYDSUB))
     1   DEALLOCATE(HYDSUBDAT(IGRID)%INTRPHYDSUB)
      IF (ASSOCIATED(HYDSUBDAT(IGRID)%JIKHYDSUB))
     1   DEALLOCATE(HYDSUBDAT(IGRID)%JIKHYDSUB)
      IF (ASSOCIATED(HYDSUBDAT(IGRID)%HYDSUBWT))
     1   DEALLOCATE(HYDSUBDAT(IGRID)%HYDSUBWT)
      IF (ASSOCIATED(HYDSUBDAT(IGRID)%HYDSUBARR))
     1   DEALLOCATE(HYDSUBDAT(IGRID)%HYDSUBARR)
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7SUB7PNT(IGRID)
C  Change HYD SUB data to a different grid.
      USE HYDSUBMODULE
C
      NHYDSUB=>HYDSUBDAT(IGRID)%NHYDSUB
      IBHYDSUB=>HYDSUBDAT(IGRID)%IBHYDSUB
      INTRPHYDSUB=>HYDSUBDAT(IGRID)%INTRPHYDSUB
      JIKHYDSUB=>HYDSUBDAT(IGRID)%JIKHYDSUB
      HYDSUBWT=>HYDSUBDAT(IGRID)%HYDSUBWT
      HYDSUBARR=>HYDSUBDAT(IGRID)%HYDSUBARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7SUB7PSV(IGRID)
C  Save HYD SUB data for a grid.
      USE HYDSUBMODULE
C
      HYDSUBDAT(IGRID)%NHYDSUB=>NHYDSUB
      HYDSUBDAT(IGRID)%IBHYDSUB=>IBHYDSUB
      HYDSUBDAT(IGRID)%INTRPHYDSUB=>INTRPHYDSUB
      HYDSUBDAT(IGRID)%JIKHYDSUB=>JIKHYDSUB
      HYDSUBDAT(IGRID)%HYDSUBWT=>HYDSUBWT
      HYDSUBDAT(IGRID)%HYDSUBARR=>HYDSUBARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7STR7DA(IGRID)
C  Deallocate HYD STR memory
      USE HYDSTRMODULE
C
      IF (ASSOCIATED(HYDSTRDAT(IGRID)%NHYDSTR))
     1   DEALLOCATE(HYDSTRDAT(IGRID)%NHYDSTR)
      IF (ASSOCIATED(HYDSTRDAT(IGRID)%ISTRHYD))
     1   DEALLOCATE(HYDSTRDAT(IGRID)%ISTRHYD)
      IF (ASSOCIATED(HYDSTRDAT(IGRID)%HYDSTRARR))
     1   DEALLOCATE(HYDSTRDAT(IGRID)%HYDSTRARR)
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7STR7PNT(IGRID)
C  Change HYD STR data to a different grid.
      USE HYDSTRMODULE
C
      NHYDSTR=>HYDSTRDAT(IGRID)%NHYDSTR
      ISTRHYD=>HYDSTRDAT(IGRID)%ISTRHYD
      HYDSTRARR=>HYDSTRDAT(IGRID)%HYDSTRARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7STR7PSV(IGRID)
C  Save HYD STR data for a grid.
      USE HYDSTRMODULE
C
      HYDSTRDAT(IGRID)%NHYDSTR=>NHYDSTR
      HYDSTRDAT(IGRID)%ISTRHYD=>ISTRHYD
      HYDSTRDAT(IGRID)%HYDSTRARR=>HYDSTRARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7SFR7DA(IGRID)
C  Deallocate HYD SFR memory
      USE HYDSFRMODULE
C
      IF (ASSOCIATED(HYDSFRDAT(IGRID)%NHYDSFR))     
     1   DEALLOCATE(HYDSFRDAT(IGRID)%NHYDSFR)
      IF (ASSOCIATED(HYDSFRDAT(IGRID)%ISFRHYD))     
     1   DEALLOCATE(HYDSFRDAT(IGRID)%ISFRHYD)
      IF (ASSOCIATED(HYDSFRDAT(IGRID)%HYDSFRARR))     
     1   DEALLOCATE(HYDSFRDAT(IGRID)%HYDSFRARR)
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7SFR7PNT(IGRID)
C  Change HYD SFR data to a different grid.
      USE HYDSFRMODULE
C
      NHYDSFR=>HYDSFRDAT(IGRID)%NHYDSFR
      ISFRHYD=>HYDSFRDAT(IGRID)%ISFRHYD
      HYDSFRARR=>HYDSFRDAT(IGRID)%HYDSFRARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7SFR7PSV(IGRID)
C  Save HYD SFR data for a grid.
      USE HYDSFRMODULE
C
      HYDSFRDAT(IGRID)%NHYDSFR=>NHYDSFR
      HYDSFRDAT(IGRID)%ISFRHYD=>ISFRHYD
      HYDSFRDAT(IGRID)%HYDSFRARR=>HYDSFRARR
C
      RETURN
      END
