      MODULE GWFWELMODULE
        INTEGER,SAVE,POINTER  ::NWELLS,MXWELL,NWELVL,IWELCB,IPRWEL
        INTEGER,SAVE,POINTER  ::NPWEL,IWELPB,NNPWEL,IRDPSI
        CHARACTER(LEN=16),SAVE, DIMENSION(:),   POINTER     ::WELAUX
        REAL,             SAVE, DIMENSION(:,:), POINTER     ::WELL
        REAL,             SAVE, DIMENSION(:,:),   POINTER     ::TABTIME
        REAL,             SAVE, DIMENSION(:,:),   POINTER     ::TABRATE
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::TABLAY
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::TABROW
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::TABCOL
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::TABVAL
        REAL,             SAVE,                 POINTER     ::PSIRAMP
        INTEGER,          SAVE,                 POINTER     ::IUNITRAMP
        INTEGER,          SAVE,                 POINTER     ::NUMTAB
        INTEGER,          SAVE,                 POINTER     ::MAXVAL
      TYPE GWFWELTYPE
        INTEGER,POINTER  ::NWELLS,MXWELL,NWELVL,IWELCB,IPRWEL
        INTEGER,POINTER  ::NPWEL,IWELPB,NNPWEL,IRDPSI
        CHARACTER(LEN=16), DIMENSION(:),   POINTER     ::WELAUX
        REAL,              DIMENSION(:,:), POINTER     ::WELL
        REAL,              DIMENSION(:,:),   POINTER     ::TABTIME
        REAL,              DIMENSION(:,:),   POINTER     ::TABRATE
        INTEGER,           DIMENSION(:),   POINTER     ::TABLAY
        INTEGER,           DIMENSION(:),   POINTER     ::TABROW
        INTEGER,           DIMENSION(:),   POINTER     ::TABCOL
        INTEGER,           DIMENSION(:),   POINTER     ::TABVAL
        REAL,                              POINTER     ::PSIRAMP
        INTEGER,                           POINTER     ::IUNITRAMP
        INTEGER,                           POINTER     ::NUMTAB
        INTEGER,                           POINTER     ::MAXVAL
      END TYPE
      TYPE(GWFWELTYPE), SAVE:: GWFWELDAT(10)
      END MODULE GWFWELMODULE


      SUBROUTINE GWF2WEL7AR(IN,IUNITNWT,IGRID)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR WELL PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,IFREFM
      USE GWFWELMODULE
C
      CHARACTER*200 LINE
      LOGICAL :: found
      Logical :: Nwt1_1
      character(len=40) :: keyvalue
      INTEGER NUMTABHOLD
C     ------------------------------------------------------------------
      ALLOCATE(NWELLS,MXWELL,NWELVL,IWELCB,IPRWEL)
      ALLOCATE(NPWEL,IWELPB,NNPWEL,PSIRAMP,IUNITRAMP)
      ALLOCATE(NUMTAB,MAXVAL)
C
C1------IDENTIFY PACKAGE AND INITIALIZE NWELLS.
      WRITE(IOUT,*)'WEL:'
!      WRITE(IOUT,1)IN
!    1 FORMAT(1X,/1X,'WEL -- WELL PACKAGE, VERSION 7, 5/2/2005',
!     1' INPUT READ FROM UNIT ',I4)
      NWELLS=0
      NNPWEL=0
      PSIRAMP = 0.1
      IUNITRAMP=IOUT
      NUMTAB = 0
      MAXVAL = 1
C
C2------READ MAXIMUM NUMBER OF WELLS AND UNIT OR FLAG FOR
C2------CELL-BY-CELL FLOW TERMS.
      LLOC=1
      CALL URDCOM(IN,IOUT,LINE)
      CALL UPARLSTAL(IN,IOUT,LINE,NPWEL,MXPW)
      
      lloc = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      keyvalue = LINE(ISTART:ISTOP)
      call upcase(keyvalue)
      Nwt1_1 = .false.
      IF(keyvalue.EQ.'OPTIONS') THEN
        Nwt1_1 = .true.
!              write(iout,'(/1x,a)') 'PROCESSING '//
!     +              trim(adjustl(text)) //' OPTIONS'
        do
        CALL URDCOM(In, IOUT, line)
        lloc = 1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
        keyvalue = LINE(ISTART:ISTOP)
        call upcase(keyvalue)
        select case (keyvalue)
! REDUCING PUMPING FOR DRY CELLS
        case('SPECIFY')
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,PSIRAMP,IOUT,IN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUNITRAMP,R,IOUT,IN)
          IF(PSIRAMP.LT.1.0E-5) PSIRAMP=1.0E-5
          IF ( IUNITRAMP.EQ.0 ) IUNITRAMP = IOUT
!          WRITE(IOUT,*)
          !WRITE(IOUT,9) PSIRAMP,IUNITRAMP
           WRITE(IOUT,*) "PSIRAMP,IUNITRAMP:"
           WRITE(IOUT,*) PSIRAMP,IUNITRAMP
!          IF ( Iunitnwt.EQ.0 ) write(IOUT,32)
! SPEICYING PUMPING RATES AS TIMES SERIES INPUT FILE FOR EACH WELL
        case('TABFILES')
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMTAB,R,IOUT,IN)
            IF(NUMTAB.LT.0) NUMTAB=0
!            WRITE(IOUT,30) NUMTAB
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MAXVAL,R,IOUT,IN)
            IF(MAXVAL.LT.0) THEN
                MAXVAL=1
                NUMTAB=0
            END IF
!            WRITE(IOUT,31) MAXVAL
            write(IOUT, *) "NUMTAB,MAXVAL:" 
            write(IOUT, *) NUMTAB, MAXVAL 
            found = .true.
        case ('END')
          CALL URDCOM(In, IOUT, line)
          exit
        case default
    ! -- No options found
        found = .false.
        CALL URDCOM(In, IOUT, line)
        exit
        end select
      end do
      end if
! ALLOCATE VARS FOR TIME SERIES WELL RATES
      NUMTABHOLD = NUMTAB
      IF ( NUMTABHOLD.EQ.0 ) NUMTABHOLD = 1
      ALLOCATE(TABTIME(MAXVAL,NUMTABHOLD),TABRATE(MAXVAL,NUMTABHOLD))
      ALLOCATE(TABLAY(NUMTABHOLD),TABROW(NUMTABHOLD),TABCOL(NUMTABHOLD))
      ALLOCATE(TABVAL(NUMTABHOLD))
      TABTIME = 0.0
      TABRATE = 0.0
      TABLAY = 0
      TABROW = 0
      TABCOL = 0
      TABVAL = 0
      
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(2I10)') MXACTW,IWELCB
         LLOC=21
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXACTW,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IWELCB,R,IOUT,IN)
      END IF
      i = 0
      WRITE(IOUT,*) 'MXACTW, IWELCB:'
      WRITE(IOUT,*) MXACTW, IWELCB
!      WRITE(IOUT,3) MXACTW
!    3 FORMAT(1X,'MAXIMUM OF ',I6,' ACTIVE WELLS AT ONE TIME')
!      IF(IWELCB.LT.0) WRITE(IOUT,7)
!    7 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')
!      IF(IWELCB.GT.0) WRITE(IOUT,8) IWELCB
!    8 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
!    9 FORMAT(1X,'NEGATIVE PUMPING RATES WILL BE REDUCED IF HEAD '/
!     +       ' FALLS WITHIN THE INTERVAL PSIRAMP TIMES THE CELL '/
!     +       ' THICKNESS. THE VALUE SPECIFIED FOR PHISRAMP IS ',E12.5,/
!     +       ' WELLS WITH REDUCED PUMPING WILL BE '
!     +       'REPORTED TO FILE UNIT NUMBER',I5)
C
C3------READ AUXILIARY VARIABLES AND PRINT FLAG.
      ALLOCATE(WELAUX(20))
      NAUX=0
      IPRWEL=1
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.
     1        LINE(ISTART:ISTOP).EQ.'AUX') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
         IF(NAUX.LT.20) THEN
            NAUX=NAUX+1
            WELAUX(NAUX)=LINE(ISTART:ISTOP)
            WRITE(IOUT,*) 'WELAUX(NAUX):'
            WRITE(IOUT,*) WELAUX(NAUX)
!   12       FORMAT(1X,'AUXILIARY WELL VARIABLE: ',A)
         END IF
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
         WRITE(IOUT,*) 'NOPRINT:'
!         WRITE(IOUT,13)
!   13    FORMAT(1X,'LISTS OF WELL CELLS WILL NOT BE PRINTED')
         IPRWEL = 0
         GO TO 10
      END IF
      if (.not. Nwt1_1) then

! Check keyword for specifying PSI (NWT).
      CALL URDCOM(IN,IOUT,LINE)
      CALL UPARLSTAL(IN,IOUT,LINE,NPP,MXVL)
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'SPECIFY') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,PSIRAMP,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUNITRAMP,R,IOUT,IN)
         IF(PSIRAMP.LT.1.0E-5) PSIRAMP=1.0E-5
         IF ( IUNITRAMP.EQ.0 ) IUNITRAMP = IOUT
!         WRITE(IOUT,*) 
!         WRITE(IOUT,9) PSIRAMP,IUNITRAMP
      ELSE
         BACKSPACE IN
         IF ( IUNITNWT.GT.0 )THEN
           IUNITRAMP = IOUT
!      WRITE(IOUT,*)' PHIRAMP WILL BE SET TO A DEFAULT VALUE OF 0.05'
!      WRITE(IOUT,*) ' WELLS WITH REDUCED PUMPING WILL BE '
!     +                      ,'REPORTED TO THE MAIN LISTING FILE'
         END IF
      END IF
         WRITE(IOUT,*) "PSIRAMP,IUNITRAMP:"
         WRITE(IOUT,*) PSIRAMP,IUNITRAMP
      endif
!
C3A-----THERE ARE FOUR INPUT VALUES PLUS ONE LOCATION FOR
C3A-----CELL-BY-CELL FLOW.
      NWELVL=5+NAUX
C
C4------ALLOCATE SPACE FOR THE WELL DATA.
      IWELPB=MXACTW+1
      MXWELL=MXACTW+MXPW
      IF(MXACTW.LT.1) THEN
         WRITE(IOUT,17)
   17    FORMAT(1X,
     1'Deactivating the Well Package because MXACTW=0')
         IN=0
      END IF
      ALLOCATE (WELL(NWELVL,MXWELL))
C
C5------READ NAMED PARAMETERS.
!      WRITE(IOUT,18) NPWEL
!   18 FORMAT(1X,//1X,I5,' Well parameters')
      IF(NPWEL.GT.0) THEN
        LSTSUM=IWELPB
        DO 120 K=1,NPWEL
          LSTBEG=LSTSUM
          CALL UPARLSTRP(LSTSUM,MXWELL,IN,IOUT,IP,'WEL','Q',1,
     &                   NUMINST)
          NLST=LSTSUM-LSTBEG
          IF(NUMINST.EQ.0) THEN
C5A-----READ PARAMETER WITHOUT INSTANCES.
            WRITE(IOUT,*) 'Layer Row Column Qfact [xyz]:' 
            CALL ULSTRD(NLST,WELL,LSTBEG,NWELVL,MXWELL,1,IN,
     &        IOUT,'WELL NO.  LAYER   ROW   COL   STRESS FACTOR',
     &        WELAUX,20,NAUX,IFREFM,NCOL,NROW,NLAY,4,4,IPRWEL)
          ELSE
C5B-----READ INSTANCES.
            NINLST=NLST/NUMINST
            DO 110 I=1,NUMINST
            CALL UINSRP(I,IN,IOUT,IP,IPRWEL)
            WRITE(IOUT,*) 'Layer Row Column Qfact [xyz]:' 
            CALL ULSTRD(NINLST,WELL,LSTBEG,NWELVL,MXWELL,1,IN,
     &        IOUT,'WELL NO.  LAYER   ROW   COL   STRESS FACTOR',
     &        WELAUX,20,NAUX,IFREFM,NCOL,NROW,NLAY,4,4,IPRWEL)
            LSTBEG=LSTBEG+NINLST
  110       CONTINUE
          END IF
  120   CONTINUE
      END IF
C
C6------RETURN
      CALL SGWF2WEL7PSV(IGRID)
      RETURN
      END
      SUBROUTINE GWF2WEL7RP(IN,KPER,IGRID)
C     ******************************************************************
C     READ WELL DATA FOR A STRESS PERIOD
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,IFREFM
      USE GWFWELMODULE
C
      CHARACTER*6 CWELL
      CHARACTER(LEN=200)::LINE
      INTEGER TABUNIT,I
      REAL TTIME,TRATE
C     ------------------------------------------------------------------
      WRITE(IOUT,*) 'WEL:' 
      CALL SGWF2WEL7PNT(IGRID)
      TABUNIT = 0
      TTIME = 0.0
      TRATE = 0.0
C
C1----READ NUMBER OF WELLS (OR FLAG SAYING REUSE WELL DATA).
C1----AND NUMBER OF PARAMETERS
      IF ( KPER.EQ.1 .OR. NUMTAB.EQ.0 ) THEN
        IF(NPWEL.GT.0) THEN
          IF(IFREFM.EQ.0) THEN
             READ(IN,'(2I10)') ITMP,NP
          ELSE
             READ(IN,*) ITMP,NP
          END IF
          WRITE(IOUT,*) 'ITMP,NP:'
          WRITE(IOUT,*) ITMP,NP
        ELSE
           NP=0
           IF(IFREFM.EQ.0) THEN
              READ(IN,'(I10)') ITMP
           ELSE
              READ(IN,*) ITMP
           END IF
          WRITE(IOUT,*) 'ITMP:'
          WRITE(IOUT,*) ITMP
        END IF 
      ELSE
        ITMP = -1
        NP = 0
      END IF
C
C------Calculate some constants.
      NAUX=NWELVL-5
      IOUTU = IOUT
      IF (IPRWEL.EQ.0) IOUTU=-IOUTU
C
C1A-----IF ITMP LESS THAN ZERO REUSE NON-PARAMETER DATA. PRINT MESSAGE.
C1A-----IF ITMP=>0, SET NUMBER OF NON-PARAMETER WELLS EQUAL TO ITMP.
      IF(ITMP.LT.0) THEN
!         WRITE(IOUT,6)
!    6    FORMAT(1X,/
!     1    1X,'REUSING NON-PARAMETER WELLS FROM LAST STRESS PERIOD')
      ELSE
         NNPWEL=ITMP
      END IF
C
C1B-----IF THERE ARE NEW NON-PARAMETER WELLS, READ THEM.
      MXACTW=IWELPB-1
      IF(ITMP.GT.0) THEN
         IF(NNPWEL.GT.MXACTW) THEN
            WRITE(IOUT,99) NNPWEL,MXACTW
   99       FORMAT(1X,/1X,'THE NUMBER OF ACTIVE WELLS (',I6,
     1                     ') IS GREATER THAN MXACTW(',I6,')')
            CALL USTOP(' ')
         END IF
         IF ( NUMTAB.EQ.0 ) THEN
  	     WRITE(IOUT,*) 'Layer Row Column Q [xyz]:'
           CALL ULSTRD(NNPWEL,WELL,1,NWELVL,MXWELL,1,IN,IOUT,
     1            'WELL NO.  LAYER   ROW   COL   STRESS RATE',
     2             WELAUX,20,NAUX,IFREFM,NCOL,NROW,NLAY,4,4,IPRWEL)
         ELSEIF ( KPER.EQ.1 ) THEN
           DO J = 1, NUMTAB
             READ(IN,*)TABUNIT,TABVAL(J),TABLAY(J),TABROW(J),TABCOL(J)
             write(IOUT,*) 
     1          "TABUNIT,TABVAL(J),TABLAY(J),TABROW(J),TABCOL(J):"
             write(IOUT,*) 
     1          TABUNIT,TABVAL(J),TABLAY(J),TABROW(J),TABCOL(J)
             IF ( TABUNIT.LE.0 ) THEN
                 WRITE(IOUT,100)
                 CALL USTOP('')
             END IF
             write(IOUT,*) "TTIME,TRATE:"
             DO I = 1, TABVAL(J)
              LLOC = 1
              CALL URDCOM_NoPrint(TABUNIT,IOUT,LINE)
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TTIME,IOUT,TABUNIT)
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TRATE,IOUT,TABUNIT)
              TABTIME(I,J) = TTIME
              TABRATE(I,J) = TRATE
               write(IOUT,*) TTIME,TRATE
             END DO
           END DO
         END IF
      END IF
      NWELLS=NNPWEL
C
C1C-----IF THERE ARE ACTIVE WELL PARAMETERS, READ THEM AND SUBSTITUTE
      CALL PRESET('Q')
      NREAD=NWELVL-1
      IF(NP.GT.0) THEN
         DO 30 N=1,NP
         CALL UPARLSTSUB(IN,'WEL',IOUTU,'Q',WELL,NWELVL,MXWELL,NREAD,
     1                MXACTW,NWELLS,4,4,
     2            'WELL NO.  LAYER   ROW   COL   STRESS RATE',
     3            WELAUX,20,NAUX)
   30    CONTINUE
      END IF
C
C3------PRINT NUMBER OF WELLS IN CURRENT STRESS PERIOD.
      CWELL=' WELLS'
      IF(NWELLS.EQ.1) CWELL=' WELL '
!      WRITE(IOUT,101) NWELLS,CWELL
!  101 FORMAT(1X,/1X,I6,A)
  100 FORMAT(1X,/1X,'****MODEL STOPPING**** ',
     +       'UNIT NUMBER FOR TABULAR INPUT FILE SPECIFIED AS ZERO.')
C
C6------RETURN
      RETURN
      END
!      SUBROUTINE GWF2WEL7FM(IGRID)
C     ******************************************************************
C     SUBTRACT Q FROM RHS
C     ******************************************************************
!      SUBROUTINE GWF2WEL7BD(KSTP,KPER,IGRID)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR WELLS
C     ******************************************************************
      SUBROUTINE GWF2WEL7DA(IGRID)
C  Deallocate WEL MEMORY
      USE GWFWELMODULE
C
        CALL SGWF2WEL7PNT(IGRID)
        DEALLOCATE(PSIRAMP) 
        DEALLOCATE(IUNITRAMP) 
        DEALLOCATE(NWELLS)
        DEALLOCATE(MXWELL)
        DEALLOCATE(NWELVL)
        DEALLOCATE(IWELCB)
        DEALLOCATE(IPRWEL)
        DEALLOCATE(NPWEL)
        DEALLOCATE(IWELPB)
        DEALLOCATE(NNPWEL)
        DEALLOCATE(WELAUX)
        DEALLOCATE(WELL)
        DEALLOCATE(NUMTAB)
        DEALLOCATE(MAXVAL)
        DEALLOCATE(TABTIME)
        DEALLOCATE(TABRATE)
        DEALLOCATE(TABVAL)
C
      RETURN
      END
      SUBROUTINE SGWF2WEL7PNT(IGRID)
C  Change WEL data to a different grid.
      USE GWFWELMODULE
C
        PSIRAMP=>GWFWELDAT(IGRID)%PSIRAMP
        IUNITRAMP=>GWFWELDAT(IGRID)%IUNITRAMP 
        NWELLS=>GWFWELDAT(IGRID)%NWELLS
        MXWELL=>GWFWELDAT(IGRID)%MXWELL
        NWELVL=>GWFWELDAT(IGRID)%NWELVL
        IWELCB=>GWFWELDAT(IGRID)%IWELCB
        IPRWEL=>GWFWELDAT(IGRID)%IPRWEL
        NPWEL=>GWFWELDAT(IGRID)%NPWEL
        IWELPB=>GWFWELDAT(IGRID)%IWELPB
        NNPWEL=>GWFWELDAT(IGRID)%NNPWEL
        WELAUX=>GWFWELDAT(IGRID)%WELAUX
        WELL=>GWFWELDAT(IGRID)%WELL
        NUMTAB=>GWFWELDAT(IGRID)%NUMTAB
        MAXVAL=>GWFWELDAT(IGRID)%MAXVAL
        TABTIME=>GWFWELDAT(IGRID)%TABTIME
        TABRATE=>GWFWELDAT(IGRID)%TABRATE
        TABVAL=>GWFWELDAT(IGRID)%TABVAL
C
      RETURN
      END
      SUBROUTINE SGWF2WEL7PSV(IGRID)
C  Save WEL data for a grid.
      USE GWFWELMODULE
C 
        GWFWELDAT(IGRID)%PSIRAMP=>PSIRAMP
        GWFWELDAT(IGRID)%IUNITRAMP=>IUNITRAMP
        GWFWELDAT(IGRID)%NWELLS=>NWELLS
        GWFWELDAT(IGRID)%MXWELL=>MXWELL
        GWFWELDAT(IGRID)%NWELVL=>NWELVL
        GWFWELDAT(IGRID)%IWELCB=>IWELCB
        GWFWELDAT(IGRID)%IPRWEL=>IPRWEL
        GWFWELDAT(IGRID)%NPWEL=>NPWEL
        GWFWELDAT(IGRID)%IWELPB=>IWELPB
        GWFWELDAT(IGRID)%NNPWEL=>NNPWEL
        GWFWELDAT(IGRID)%WELAUX=>WELAUX
        GWFWELDAT(IGRID)%WELL=>WELL
        GWFWELDAT(IGRID)%NUMTAB=>NUMTAB
        GWFWELDAT(IGRID)%MAXVAL=>MAXVAL
        GWFWELDAT(IGRID)%TABTIME=>TABTIME
        GWFWELDAT(IGRID)%TABRATE=>TABRATE
        GWFWELDAT(IGRID)%TABVAL=>TABVAL
C
      RETURN
      END
