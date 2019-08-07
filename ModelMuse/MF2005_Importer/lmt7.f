C
C ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C LINK-MT3DMS (LMT) PACKAGE V7 FOR MODFLOW-2005
C Modified from LMT V6 for MODFLOW-2000 as documented in:
C     Zheng, C., M.C. Hill, and P.A. Hsieh, 2001,
C         MODFLOW-2000, the U.S. Geological Survey modular ground-water
C         model--User guide to the LMT6 Package, the linkage with
C         MT3DMS for multispecies mass transport modeling:
C         U.S. Geological Survey Open-File Report 01-82
C
C Revision History: 
C     Version 7.0: 08-08-2008 cz
C ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
C
      SUBROUTINE LMT7BAS7(INUNIT,CUNIT,ISSMT3D,IUMT3D,ILMTFMT,IGRID)
C **********************************************************************
C OPEN AND READ THE INPUT FILE FOR THE LINK-MT3DMS PACKAGE VERSION 7.
C CHECK KEY FLOW MODEL INFORMATION AND SAVE IT IN THE HEADER OF
C THE MODFLOW-MT3DMS LINK FILE FOR USE IN MT3DMS TRANSPORT SIMULATION.
C NOTE THE 'STANDARD' HEADER OPTION IS NO LONGER SUPPORTED. INSTEAD,
C THE 'EXTENDED' HEADER OPTION IS THE DEFAULT. THE RESULTING LINK FILE 
C IS ONLY COMPATIBLE WITH MT3DMS VERSION [4.00] OR LATER.
C **********************************************************************
C last modified: 08-08-2008
C      
      USE GLOBAL,   ONLY:NCOL,NROW,NLAY,NPER,NODES,NIUNIT,IUNIT,
     &                   ISSFLG,IBOUND,IOUT
      LOGICAL       LOP
      CHARACTER*4   CUNIT(NIUNIT)
      CHARACTER*200 LINE,FNAME,NME
      CHARACTER*8   OUTPUT_FILE_HEADER
      CHARACTER*11  OUTPUT_FILE_FORMAT      
      DATA          INLMT,MTBCF,MTLPF,MTHUF,MTWEL,MTDRN,MTRCH,MTEVT,
     &              MTRIV,MTSTR,MTGHB,MTRES,MTFHB,MTDRT,MTETS,MTSUB,
     &              MTIBS,MTLAK,MTMNW,MTSWT,MTSFR,MTUZF
     &             /22*0/
C     -----------------------------------------------------------------    
C
C--USE FILE SPECIFICATION of MODFLOW-2005
      INCLUDE 'openspec.inc'
	write(IOUT,*) 'LMT6:';
C
C--SET POINTERS FOR THE CURRENT GRID 
      CALL SGWF2BAS7PNT(IGRID)     
C
C--CHECK for OPTIONS/PACKAGES USED IN CURRENT SIMULATION
      IUMT3D=0
      DO IU=1,NIUNIT
        IF(CUNIT(IU).EQ.'LMT6') THEN
          INLMT=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'BCF6') THEN
          MTBCF=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'LPF ') THEN
          MTLPF=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'HUF2') THEN
          MTHUF=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'WEL ') THEN
          MTWEL=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'DRN ') THEN
          MTDRN=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'RCH ') THEN
          MTRCH=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'EVT ') THEN
          MTEVT=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'RIV ') THEN
          MTRIV=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'STR ') THEN
          MTSTR=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'GHB ') THEN
          MTGHB=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'RES ') THEN
          MTRES=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'FHB ') THEN
          MTFHB=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'DRT ') THEN
          MTDRT=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'ETS ') THEN
          MTETS=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'SUB ') THEN
          MTSUB=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'IBS ') THEN
          MTIBS=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'LAK ') THEN
          MTLAK=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'MNW1') THEN
          MTMNW=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'SWT ') THEN
          MTSWT=IUNIT(IU)        
        ELSEIF(CUNIT(IU).EQ.'SFR ') THEN
          MTSFR=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'UZF ') THEN
          MTUZF=IUNIT(IU)
        ENDIF
      ENDDO            
C
C--IF LMT7 PACKAGE IS NOT ACTIVATED, SKIP TO END AND RETURN
      IF(INLMT.EQ.0) GOTO 9999
C
C--ASSIGN DEFAULTS TO LMT INPUT VARIABLES AND OUTPUT FILE NAME
      IUMT3D=333
      OUTPUT_FILE_HEADER='EXTENDED'
      ILMTHEAD=1
      OUTPUT_FILE_FORMAT='UNFORMATTED'
      ILMTFMT=0     
      INQUIRE(UNIT=INLMT,NAME=NME,OPENED=LOP)
      IFLEN=INDEX(NME,' ')-1
      DO NC=IFLEN,2,-1
        IF(NME(NC:NC).EQ.'.') THEN      
          FNAME=NME(1:NC-1)//'.FTL'
          GO TO 10
        ENDIF
      ENDDO    
      FNAME=NME(1:IFLEN)//'.FTL'     
C
C--READ ONE LINE OF LMT PACKAGE INPUT FILE
   10 READ(INLMT,'(A)',END=1000) LINE
      IF(LINE.EQ.' ') GOTO 10
      IF(LINE(1:1).EQ.'#') GOTO 10
C
C--DECODE THE INPUT RECORD
      LLOC=1
      CALL URWORD(LINE,LLOC,ITYP1,ITYP2,1,N,R,IOUT,INLMT)
C
C--CHECK FOR "OUTPUT_FILE_NAME" KEYWORD AND GET FILE NAME
      IF(LINE(ITYP1:ITYP2).EQ.'OUTPUT_FILE_NAME') THEN
        CALL URWORD(LINE,LLOC,INAM1,INAM2,0,N,R,IOUT,INLMT)
        IFLEN=INAM2-INAM1+1
        IF(LINE(INAM1:INAM2).EQ.' ') THEN
        ELSE
          FNAME=LINE(INAM1:INAM2)
        ENDIF
C
C--CHECK FOR "OUTPUT_FILE_UNIT" KEYWORD AND GET UNIT NUMBER
      ELSEIF(LINE(ITYP1:ITYP2).EQ.'OUTPUT_FILE_UNIT') THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,IOUT,INLMT)
        IF(IU.GT.0) THEN
          IUMT3D=IU
        ELSEIF(IU.LT.0) THEN
          WRITE(IOUT,11) IU
          WRITE(*,11) IU
          CALL USTOP(' ')
        ENDIF
C
C--CHECK FOR "OUTPUT_FILE_HEADER" KEYWORD AND GET INPUT VALUE
      ELSEIF(LINE(ITYP1:ITYP2).EQ.'OUTPUT_FILE_HEADER') THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INLMT)
        IF(LINE(ISTART:ISTOP).EQ.' '.OR.
     &     LINE(ISTART:ISTOP).EQ.'EXTENDED') THEN
          OUTPUT_FILE_HEADER='EXTENDED'
          ILMTHEAD=1
        ELSEIF(LINE(ISTART:ISTOP).EQ.'STANDARD') THEN
!          WRITE(IOUT,120)
!          WRITE(*,120)                   
        ELSE
          WRITE(IOUT,12) LINE(ISTART:ISTOP)
          WRITE(*,12) LINE(ISTART:ISTOP)
          CALL USTOP(' ')
        ENDIF
C
C--CHECK FOR "OUTPUT_FILE_FORMAT" KEYWORD AND GET INPUT VALUE
      ELSEIF(LINE(ITYP1:ITYP2).EQ.'OUTPUT_FILE_FORMAT') THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INLMT)
        IF(LINE(ISTART:ISTOP).EQ.' '.OR.
     &     LINE(ISTART:ISTOP).EQ.'UNFORMATTED') THEN
          OUTPUT_FILE_FORMAT='UNFORMATTED'
          ILMTFMT=0
        ELSEIF(LINE(ISTART:ISTOP).EQ.'FORMATTED') THEN
          OUTPUT_FILE_FORMAT='FORMATTED'
          ILMTFMT=1
        ELSE
          WRITE(IOUT,14) LINE(ISTART:ISTOP)
          WRITE(*,14) LINE(ISTART:ISTOP)
          CALL USTOP(' ')
        ENDIF
C
C--ERROR DECODING LMT INPUT KEYWORDS
      ELSE
        WRITE(IOUT,28) LINE
        WRITE(*,28) LINE
        CALL USTOP(' ')
      ENDIF
C
C--CONTINUE TO THE NEXT INPUT RECORD IN LMT FILE
      GOTO 10
C
   11 FORMAT(/1X,'ERROR READING LMT PACKAGE INPUT DATA:',
     & /1X,'INVALID OUTPUT FILE UNIT: ',I5)
   12 FORMAT(/1X,'ERROR READING LMT PACKAGE INPUT DATA:',
     & /1X,'INVALID OUTPUT_FILE_HEADER CODE: ',A)
   14 FORMAT(/1X,'ERROR READING LMT PACKAGE INPUT DATA:',
     & /1X,'INVALID OUTPUT_FILE_FORMAT SPECIFIER: ',A)
   28 FORMAT(/1X,'ERROR READING LMT PACKAGE INPUT DATA:',
     & /1X,'UNRECOGNIZED KEYWORD: ',A)
  120 FORMAT(/1X,'WARNING READING LMT PACKAGE INPUT DATA:',
     &       /1X,'[STANDARD] HEADER NO LONGER SUPPORTED; ',
     &           '[EXTENDED] HEADER USED INSTEAD.')     
C     
 1000 CONTINUE     
C
C--ENSURE A UNIQUE UNIT NUMBER FOR LINK-MT3DMS OUTPUT FILE
      IF(IUMT3D.EQ.IOUT .OR. IUMT3D.EQ.INUNIT) THEN
        WRITE(IOUT,1010) IUMT3D
        WRITE(*,1010) IUMT3D
        CALL USTOP(' ')
      ELSE
        DO IU=1,NIUNIT       
          IF(IUMT3D.EQ.IUNIT(IU)) THEN
            WRITE(IOUT,1010) IUMT3D
            WRITE(*,1010) IUMT3D
            CALL USTOP(' ')
          ENDIF
        ENDDO
      ENDIF  
 1010 FORMAT(/1X,'ERROR IN LMT PACKAGE INPT DATA:'
     &       /1X,'UNIT NUMBER GIVEN FOR FLOW-TRANSPORT LINK FILE:', 
     &        I4,' ALREADY IN USE;' 
     &       /1X,'SPECIFY A UNIQUE UNIT NUMBER.')   
C
C--OPEN THE LINK-MT3DMS OUTPUT FILE NEEDED BY MT3DMS
C--AND PRINT AN IDENTIFYING MESSAGE IN MODFLOW OUTPUT FILE  
      INQUIRE(UNIT=IUMT3D,OPENED=LOP)
      IF(LOP) THEN
        REWIND (IUMT3D)
 !     ELSE
!        IF(ILMTFMT.EQ.0) THEN
!          OPEN(IUMT3D,FILE=FNAME,FORM=FORM,ACCESS=ACCESS,
!     &      ACTION=ACTION(2),STATUS='REPLACE')
!        ELSEIF(ILMTFMT.EQ.1) THEN
!          OPEN(IUMT3D,FILE=FNAME,FORM='FORMATTED',ACTION=ACTION(2),
!     &      STATUS='REPLACE',DELIM='APOSTROPHE')
!        ENDIF
      ENDIF
C
!      WRITE(IOUT,30) FNAME,IUMT3D,
!     &               OUTPUT_FILE_FORMAT,OUTPUT_FILE_HEADER
   30 FORMAT(//1X,'***Link-MT3DMS Package v7***',
     &        /1x,'OPENING LINK-MT3DMS OUTPUT FILE: ',A,
     &        /1X,'ON UNIT NUMBER: ',I5,
     &        /1X,'FILE TYPE: ',A,
     &        /1X,'HEADER OPTION: ',A,
     &        /1X,'***Link-MT3DMS Package v7***',/1X)
C
C--GATHER AND CHECK KEY FLOW MODEL INFORMATION
      ISSMT3D=1    !loop through all stress periods        
      DO N=1,NPER    !to check if any transient sp is used
        IF(ISSFLG(N).EQ.0) THEN
          ISSMT3D=0
          EXIT
        ENDIF
      ENDDO                  
      MTISS=ISSMT3D
      MTNPER=NPER 
C
      MTCHD=0    !loop through the entire grid to get
      DO K=1,NLAY    !total number of constant-head cells
        DO I=1,NROW
          DO J=1,NCOL
            IF(IBOUND(J,I,K).LT.0) MTCHD=MTCHD+1
          ENDDO
        ENDDO
      ENDDO
C
C--ERROR CHECKING BEFORT OUTPUT
      IF(MTEVT.GT.0.AND.MTETS.GT.0) THEN
        WRITE(IOUT,1300)
        WRITE(*,1300)
        CALL USTOP(' ')
      ENDIF    
 1300 FORMAT(/1X,'ERROR IN LMT PACKAGE INPT DATA:'
     &  /1X,'Both EVT and ETS Packages are used in flow simulation;'
     &  /1X,'Only one is allowed in the same transport simulation.')
C
C--WRITE A HEADER TO MODFLOW-MT3DMS LINK FILE
!      IF(OUTPUT_FILE_HEADER.EQ.'EXTENDED') THEN        
!        IF(ILMTFMT.EQ.0) THEN
!          WRITE(IUMT3D) 'MT3D4.00.00',
!     &     MTWEL,MTDRN,MTRCH,MTEVT,MTRIV,MTGHB,MTCHD,MTISS,MTNPER,
!     &     MTSTR,MTRES,MTFHB,MTDRT,MTETS,MTSUB,MTIBS,MTLAK,MTMNW,
!     &     MTSWT,MTSFR,MTUZF
!        ELSEIF(ILMTFMT.EQ.1) THEN
!          WRITE(IUMT3D,*) 'MT3D4.00.00',
!     &     MTWEL,MTDRN,MTRCH,MTEVT,MTRIV,MTGHB,MTCHD,MTISS,MTNPER,
!     &     MTSTR,MTRES,MTFHB,MTDRT,MTETS,MTSUB,MTIBS,MTLAK,MTMNW,
!     &     MTSWT,MTSFR,MTUZF
!        ENDIF
!      ENDIF
C
C--NORMAL RETURN
 9999 RETURN
      END
C
C
!      SUBROUTINE LMT7BCF7(ILMTFMT,ISSMT3D,IUMT3D,KSTP,KPER,IGRID)
C *********************************************************************
C SAVE SATURATED CELL THICKNESS; FLOW ACROSS THREE CELL INTERFACES;
C TRANSIENT FLUID-STORAGE; AND LOCATIONS AND FLOW RATES OF
C CONSTANT-HEAD CELLS FOR USE BY MT3D.  THIS SUBROUTINE IS CALLED
C ONLY IF THE 'BCF' PACKAGE IS USED IN MODFLOW.
C *********************************************************************
C
C
!      SUBROUTINE LMT7LPF7(ILMTFMT,ISSMT3D,IUMT3D,KSTP,KPER,IGRID)
C *********************************************************************
C SAVE FLOW ACROSS THREE CELL INTERFACES (QXX, QYY, QZZ), FLOW RATE TO
C OR FROM TRANSIENT FLUID-STORAGE (QSTO), AND LOCATIONS AND FLOW RATES
C OF CONSTANT-HEAD CELLS FOR USE BY MT3D.  THIS SUBROUTINE IS CALLED
C ONLY IF THE 'LPF' PACKAGE IS USED IN MODFLOW.
C *********************************************************************
C
C
c      SUBROUTINE LMT7HUF7(ILMTFMT,ISSMT3D,IUMT3D,KSTP,KPER,ILVDA,IGRID)
C **********************************************************************
C SAVE FLOW ACROSS THREE CELL INTERFACES (QXX, QYY, QZZ), FLOW RATE TO
C OR FROM TRANSIENT FLUID-STORAGE (QSTO), AND LOCATIONS AND FLOW RATES
C OF CONSTANT-HEAD CELLS FOR USE BY MT3D.  THIS SUBROUTINE IS CALLED
C ONLY IF THE 'HUF' PACKAGE IS USED IN MODFLOW.
C **********************************************************************
C Modified from Anderman and Hill (2000), Harbaugh (2005)
C last modified: 08-08-2008
C
C
C
!      SUBROUTINE LMT7WEL7(ILMTFMT,IUMT3D,KSTP,KPER,IGRID)
C *********************************************************************
C SAVE WELL CELL LOCATIONS AND VOLUMETRIC FLOW RATES FOR USE BY MT3D.
C *********************************************************************
!      SUBROUTINE LMT7DRN7(ILMTFMT,IUMT3D,KSTP,KPER,IGRID)
C ********************************************************************
C SAVE DRAIN CELL LOCATIONS AND VOLUMETRIC FLOW RATES FOR USE BY MT3D.
C ********************************************************************
C
!      SUBROUTINE LMT7RIV7(ILMTFMT,IUMT3D,KSTP,KPER,IGRID)
C *********************************************************************
C SAVE RIVER CELL LOCATIONS AND VOLUMETRIC FLOW RATES FOR USE BY MT3D.
C *********************************************************************
C
C
!      SUBROUTINE LMT7RCH7(ILMTFMT,IUMT3D,KSTP,KPER,IGRID)
C *******************************************************************
C SAVE RECHARGE LAYER LOCATION AND VOLUMETRIC FLOW RATES
C FOR USE BY MT3D.
C *******************************************************************
C
!      SUBROUTINE LMT7EVT7(ILMTFMT,IUMT3D,KSTP,KPER,IGRID)
C ******************************************************************
C SAVE EVAPOTRANSPIRATION LAYER LOCATION AND VOLUMETRIC FLOW RATES
C FOR USE BY MT3D.
C ******************************************************************
C
!      SUBROUTINE LMT7GHB7(ILMTFMT,IUMT3D,KSTP,KPER,IGRID)
C *****************************************************************
C SAVE HEAD-DEPENDENT BOUNDARY CELL LOCATIONS AND VOLUMETRIC FLOW
C RATES FOR USE BY MT3D.
C *****************************************************************
C
!      SUBROUTINE LMT7FHB7(ILMTFMT,IUMT3D,KSTP,KPER,IGRID)
C **********************************************************************
C SAVE SPECIFIED-FLOW CELL LOCATIONS AND VOLUMETRIC FLOW RATES
C FOR USE BY MT3D.
C **********************************************************************
C
!      SUBROUTINE LMT7RES7(ILMTFMT,IUMT3D,KSTP,KPER,IGRID)
C **********************************************************************
C SAVE RESERVOIR CELL LOCATIONS AND VOLUMETRIC FLOW RATES
C FOR USE BY MT3D.
C **********************************************************************
C
!      SUBROUTINE LMT7STR7(ILMTFMT,IUMT3D,KSTP,KPER,IGRID)
C **********************************************************************
C SAVE STREAM CELL LOCATIONS AND VOLUMETRIC FLOW RATES FOR USE BY MT3D.
C **********************************************************************
C
!      SUBROUTINE LMT7MNW7(ILMTFMT,IUMT3D,KSTP,KPER,IGRID)
C *********************************************************************
C SAVE MNW LOCATIONS AND VOLUMETRIC FLOW RATES FOR USE BY MT3D.
C *********************************************************************
C
!      SUBROUTINE LMT7ETS7(ILMTFMT,IUMT3D,KSTP,KPER,IGRID)
C ********************************************************************
C SAVE SEGMENTED EVAPOTRANSPIRATION LAYER INDICES (IF NLAY>1) AND
C VOLUMETRIC FLOW RATES FOR USE BY MT3D.
C ********************************************************************
C
!      SUBROUTINE LMT7DRT7(ILMTFMT,IUMT3D,KSTP,KPER,IGRID)
C ******************************************************************
C SAVE DRT (Drain with Return Flow) CELL LOCATIONS AND 
C VOLUMETRIC FLOW RATES FOR USE BY MT3D.
C ******************************************************************
