C     VERSION 2.3.1, 05/13/2003
!      SUBROUTINE UHUF7RMLT(RMLT0,J,I,NZ,NM,ICL)
C
C     ******************************************************************
C     Calculate RMLT for specified cell.
C     ******************************************************************
c======================================================================
      SUBROUTINE UHUF7PARRP(IN,IOUT,NP,PTYP,ITERP,NHUF)
C
C     ******************************************************************
C     Read and store array parameter definition information for HUF package
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE PARAMMODULE
      USE GWFHUFMODULE, ONLY:HGUNAM
      CHARACTER*(*) PTYP
      CHARACTER*200 LINE
      CHARACTER*10 PN,CTMP1,CTMP2
C     ------------------------------------------------------------------
C
      ILFLG=1
C  Read a parameter definition line and decode the parameter name, type,
C  and value
      READ(IN,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      PN=LINE(ISTART:ISTOP)
      CTMP1=PN
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      PTYP=LINE(ISTART:ISTOP)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,PV,IOUT,IN)
C
C  Look for the parameter name in the parameter list
      DO 10 NP=1,MXPAR
          CTMP2=PARNAM(NP)
          CALL UPCASE(CTMP2)
          IF(CTMP1.EQ.CTMP2) THEN
C
C  If found, determine if it is an illegal duplicate or if it was
C  predefined.
cswm              IF(PARTYP(NP).NE.' ' .AND. IDEFPAR.EQ.0) THEN
              IF(PARTYP(NP).NE.' ' .AND. ITERP.EQ.1) THEN
C  Illegal duplicate
                  WRITE(IOUT,*) ' Duplicate parameter name'
                  CALL USTOP(' ')
              END IF
C  Parameter was predefined -- leave its value alone (i.e. ignore PV).
	        WRITE(IOUT, *), 'PARNAM:'
	        WRITE(IOUT, *), PARNAM(NP)
	        WRITE(IOUT, *), 'PTYP:'
	        WRITE(IOUT, *), PTYP
	        WRITE(IOUT, *), 'Parval:'
	        WRITE(IOUT, *), PV
              GO TO 100
          ELSE IF(PARNAM(NP).EQ.' ') THEN
C  Parameter was not found in the list, so it is a new definition.
C  Put values in the list.
              PARNAM(NP)=PN
              B(NP)=PV
	        WRITE(IOUT, *), 'PARNAM:'
	        WRITE(IOUT, *), PARNAM(NP)
	        WRITE(IOUT, *), 'PTYP:'
	        WRITE(IOUT, *), PTYP
	        WRITE(IOUT, *), 'Parval:'
	        WRITE(IOUT, *), PV
              IPSUM=IPSUM+1
              GO TO 100
          END IF
10    CONTINUE
C  Too many parameters
      WRITE(IOUT,11)
   11 FORMAT(1X,'The number of parameters has exceeded the maximum')
      CALL USTOP(' ')
C
C  Parse the rest of the parameter definition.
  100 PARTYP(NP)=PTYP
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NDHUF,R,IOUT,IN)
	WRITE(IOUT, *) 'NCLU:'
	WRITE(IOUT, *) NDHUF

      IF(IPLOC(1,NP).EQ.0) THEN
         ICLSUM=ICLSUM+1
         IPLOC(1,NP)=ICLSUM
         ICLSUM=ICLSUM+NDHUF-1
         IPLOC(2,NP)=ICLSUM
      END IF
      IACTIVE(NP)=0
C
      IF(IPLOC(2,NP).GT.MXCLST) THEN
          WRITE(IOUT,117) IPLOC(2,NP),MXCLST
  117     FORMAT(1X,I5,
     & ' CLUSTERS WERE SPECIFIED, BUT THERE IS SPACE FOR ONLY',I5)
           WRITE(IOUT,*) NP,NDHUF
           WRITE(IOUT,'(A)') PARNAM(NP)
           WRITE(IOUT,'(2I10)') IPLOC
          CALL USTOP(' ')
      END IF
!      WRITE(IOUT,121) PARNAM(NP),PARTYP(NP),NDHUF
!  121 FORMAT(1X/,1X,'PARAMETER NAME:',A,'   TYPE:',A,' UNITS:',I4)
!      WRITE(IOUT,122) PV
  122 FORMAT(1X,'The parameter value from the package file is:',1PG13.5)
!      IF(B(NP).NE.PV) WRITE(IOUT,123) B(NP)
  123  FORMAT(1X,'This parameter value has been replaced by the',
     &  ' value from the',/1X,'Parameter Value file:',1PG13.5)
C
C  Read clusters
      DO 200 I=IPLOC(1,NP),IPLOC(2,NP)
          READ(IN,'(A)') LINE
          LLOC=1
C
C  Store layer number for LVDA
          IF(PARTYP(NP).EQ.'LVDA') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPCLST(1,I),R,-1,IN)
	      WRITE(IOUT, *) 'LAYER:'
	      WRITE(IOUT, *) IPCLST(1,I)
          ELSEIF(PARTYP(NP).EQ.'SYTP')THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPCLST(1,I),R,-1,IN)
            IPCLST(1,I)=1
          ELSE
C  Find hydrogeologic-unit number
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
            PN=LINE(ISTART:ISTOP)
            CTMP1=PN
	      WRITE(IOUT, *) 'HGUNAM:'
!	      WRITE(IOUT, *) CTMP1
C
C  Look for the unit name in the list of unit names
            DO 220 NU=1,NHUF
              CTMP2=HGUNAM(NU)
              CALL UPCASE(CTMP2)
              IF(CTMP1.EQ.CTMP2) THEN
                IPCLST(1,I)=NU
                WRITE(IOUT,*) NU
!                WRITE(IOUT,38) CTMP1,NU
!   38           FORMAT('UNIT ',A10,'CORRESPONDS TO UNIT NO. ',I5)
                GO TO 221
              END IF
  220       CONTINUE
  221       CONTINUE
          ENDIF
c
c Parse multiplication and zone array information
      CALL URWORD(LINE,LLOC,IM1,IM2,1,N,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,IZ1,IZ2,1,N,R,IOUT,IN)
	IZONECOUNT = 0
      DO 30 J=5,14
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPCLST(J,I),R,-1,IN)
      IF(IPCLST(J,I).EQ.0) THEN
         IPCLST(4,I)=J-1
         GO TO 32
      END IF
	IZONECOUNT = IZONECOUNT + 1
   30 CONTINUE
      IPCLST(4,I)=14
   32 IF(ILFLG.NE.0) THEN
!         WRITE(IOUT, *)'Mltarr:'  
         WRITE(IOUT, *)LINE(IM1:IM2)  
         !WRITE(IOUT, *)'Zonarr:'  
         WRITE(IOUT, *)LINE(IZ1:IZ2)  
!         WRITE(IOUT,36) IPCLST(1,I),LINE(IM1:IM2),LINE(IZ1:IZ2)
!   36    FORMAT(1X,'               UNIT:',I3,'   MULTIPLIER:',A,
!     &      '   ZONE:',A)
      ELSE
         WRITE(IOUT, *)'Mltarr:'  
         WRITE(IOUT, *)LINE(IM1:IM2)  
         WRITE(IOUT, *)'Zonarr:'  
         WRITE(IOUT, *)LINE(IZ1:IZ2)  
!         WRITE(IOUT,37) LINE(IM1:IM2),LINE(IZ1:IZ2)
!   37    FORMAT(1X,'               MULTIPLIER:',A,'   ZONE:',A)
      END IF
	IF (IZONECOUNT.GT.0) THEN
        WRITE(IOUT, *) 'NUMBER OF ZONES:'  
        WRITE(IOUT, *) IZONECOUNT 
	ENDIF 
C
C  Find the multiplier array number
      CTMP1=LINE(IM1:IM2)
      IF(CTMP1.EQ.'NONE') THEN
         IPCLST(2,I)=0
      ELSE
         DO 40 J=1,NMLTAR
         CTMP2=MLTNAM(J)
         CALL UPCASE(CTMP2)
         IF(CTMP1.EQ.CTMP2) GO TO 45
   40    CONTINUE
         WRITE(IOUT,'(A)') ' Multiplier array has not been defined'
         CALL USTOP(' ')
   45    IPCLST(2,I)=J
      END IF
C
C  Find the zone array number
      CTMP1=LINE(IZ1:IZ2)
      IF(CTMP1.EQ.'ALL') THEN
         IPCLST(3,I)=0
      ELSE
         IF(IPCLST(4,I).EQ.4) THEN
            WRITE(IOUT,47)
   47       FORMAT(
     &      1X,'There were no zone values specified in the cluster',/
     &      1X,'At least one zone must be specified')
            CALL USTOP(' ')
         END IF
         WRITE(IOUT, *) 'ZONE VALUES:'
         WRITE(IOUT,*) (IPCLST(J,I),J=5,IPCLST(4,I))
!         WRITE(IOUT,48) (IPCLST(J,I),J=5,IPCLST(4,I))
!   48    FORMAT(1X,'               ZONE VALUES:',10I5)
         DO 50 J=1,NZONAR
         CTMP2=ZONNAM(J)
         CALL UPCASE(CTMP2)
         IF(CTMP1.EQ.CTMP2) GO TO 55
   50    CONTINUE
         WRITE(IOUT,'(A)') ' Zone array has not been defined'
         CALL USTOP(' ')
   55    IPCLST(3,I)=J
      END IF
  200 CONTINUE
C
      RETURN
      END
c======================================================================
!      SUBROUTINE UHUF7POPL(VDHD,NCOL,NROW,NLAY,I,J)
C
C     ******************************************************************
C     Populate VDHD array.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
c======================================================================
!      SUBROUTINE UHUF7POP(HUFARRAY,PTYPE,I,J,NNU,IOUT)
C
C     ******************************************************************
C     Populate HUF arrays.
C     ******************************************************************

c======================================================================
!      SUBROUTINE UHUF7THK(TOP,BOT,TOPU,THKU,THCK,ATOP,ABOT)
C
C     ******************************************************************
C     Determine contributing thicknesses of hydrogeologic units.
C     Return adjusted top and bottom of unit in ATOP & ABOT
C     ******************************************************************
c======================================================================
!      SUBROUTINE UHUF7MMTH(
!     &  ARRAY1,M1,ARRAY2,M2,ARRAY3,CHAR,NCOL,NROW)
C
C     ******************************************************************
C     Perform matrix math on ARRAY1 & ARRAY2, put results in ARRAY3
C       ARRAY3 = ARRAY1 'CHAR' ARRAY2, where CHAR = +,-,*, or /
C       M1 & M2 are constants that can replace the arrays
C     ******************************************************************
c======================================================================
!      SUBROUTINE UHUFPRWC(A,NCOL,NROW,HGUNAM,IOUT,IPRN,ANAME)
C
C     ******************************************************************
C     CHECK TO SEE IF AN ARRAY IS CONSTANT, AND PRINT IT APPROPRIATELY
C     ******************************************************************

