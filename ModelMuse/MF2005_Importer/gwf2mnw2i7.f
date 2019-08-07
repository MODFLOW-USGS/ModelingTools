      MODULE GWFMNW2IMODULE
        INTEGER,SAVE,POINTER  ::Wel1flag,QSUMflag,BYNDflag,MNWOBS
        CHARACTER(LEN=20),SAVE, DIMENSION(:),   POINTER     ::MNWIID
        DOUBLE PRECISION, SAVE, DIMENSION(:,:), POINTER     ::MNWILST
      TYPE GWFMNWITYPE
        INTEGER,POINTER  ::Wel1flag,QSUMflag,BYNDflag,MNWOBS
        CHARACTER(LEN=20),DIMENSION(:),   POINTER     ::MNWIID
        DOUBLE PRECISION, DIMENSION(:,:), POINTER     ::MNWILST
      END TYPE
      TYPE(GWFMNWITYPE), SAVE:: GWFMNWIDAT(10)
      END MODULE GWFMNW2IMODULE
c   GZH  20080208 
C
C
C GWF2MNW2I7AR READ INIT DATA AND ALLOCATE SPACE FOR MNW WELLS DESIGNATED FOR OBSERVATION
C
C     ******************************************************************
C
      SUBROUTINE GWF2MNW2I7AR(INMNWI,INMNW2,IGRID)    
C
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,        ONLY:IOUT
      USE GWFMNW2IMODULE, ONLY:Wel1flag,QSUMflag,BYNDflag,MNWOBS,
     1                            MNWIID,MNWILST
C
      IF(INMNWI.GT.0.AND.INMNW2.LE.0) THEN
        WRITE(IOUT,*) '***ERROR*** : MNWI PACKAGE CAN ONLY BE 
     *USED IF MNW2 PACKAGE IS ACTIVE'
        STOP 'MNWI ERROR'
      END IF  
C1------Allocate scalar variables, which makes it possible for multiple
C1------grids to be defined.
      write(IOUT, *) 'MNWI:'
      ALLOCATE(Wel1flag,QSUMflag,BYNDflag,MNWOBS)
C
      IF(INMNWI.EQ.0) THEN
        LCMNIO=1
      ELSE
c     if transport on, read concflag
        READ(INMNWI,*) Wel1flag,QSUMflag,BYNDflag
        WRITE(IOUT,*) 'Wel1flag,QSUMflag,BYNDflag:'
        WRITE(IOUT,*) Wel1flag,QSUMflag,BYNDflag
!        WRITE(IOUT,*) 'MNWI Package input:'
!        WRITE(IOUT,*) 'Wel1flag = ',Wel1flag
!        WRITE(IOUT,*) 'QSUMflag = ',QSUMflag
!        WRITE(IOUT,*) 'BYNDflag = ',BYNDflag
!        WRITE(IOUT,*) 
C
        READ(INMNWI,*) MNWOBS
        WRITE(IOUT,*) 'MNWOBS:'
        WRITE(IOUT,*) MNWOBS
        IF(MNWOBS.LT.0) THEN
          WRITE(IOUT,*) 'MNWOBS MUST BE > 0'
          STOP
        END IF
C
C5------ALLOCATE SPACE FOR MNWILST ARRAY.
C5------FOR EACH OBS WELL, THERE ARE 6 DATA VALUES
      NMNWIVL=6
      ALLOCATE (MNWILST(NMNWIVL,MNWOBS))
C5------ALLOCATE SPACE FOR MNWIID ARRAY.
      ALLOCATE (MNWIID(MNWOBS+1))
	END IF
C7------SAVE POINTERS TO DATA AND RETURN.
      CALL SGWF2MNW2IPSV(IGRID)
C
      RETURN
      END
c
c_________________________________________________________________________________
c
C
C  GWF2MNW2I7RP READ INPUT FILE FOR MNW2 WELLS DESIGNATED FOR OBSERVATION 
C
C     ******************************************************************
C
      SUBROUTINE GWF2MNW2I7RP(INMNWI,GWTUNIT,IGRID)
C
C     ******************************************************************
C
C     READ LOCATIONS OF MNW2 WELLS DESIGNATED FOR OBSERVATION 
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT
      USE GWFMNW2MODULE, ONLY:MNWMAX,MNW2,WELLID
      USE GWFMNW2IMODULE, ONLY:Wel1flag,QSUMflag,BYNDflag,MNWOBS,
     1                       MNWILST,MNWIID
c     ------------------------------------------------------------------
      INTEGER GWTUNIT
      CHARACTER*20 SITE,MSITE
C
C     ******************************************************************
C
      CALL SGWF2MNW2IPNT(IGRID)
C
      IF(MNWOBS.EQ.0) THEN
       RETURN
	ENDIF
!      IF(MNWOBS.EQ.1) THEN
!       WRITE (IOUT,120) MNWOBS
!	ELSEIF(MNWOBS.GT.1) THEN
!       WRITE (IOUT,140) MNWOBS
!	ELSEIF(MNWOBS.LT.1) THEN
!       RETURN
!	END IF
!      WRITE (IOUT,150)
      IF(MNWOBS.GT.MNWMAX) then
        write(iout,*) '***ERROR*** MNWOBS > MNWMAX'
        STOP 'MNWI ERROR'
      end if
      write(IOUT, *) 'MNWI:'
C
C  Initialize data array
      MNWILST=0.0
C READ THE FIRST RECORD
      IOB=1
      IS_SITE=0
c
c MNWILST(1,IOB) is Well # in MNW list
c MNWILST(2,IOB) is net volume in/out well
c MNWILST(3,IOB) is unit number for output
c MNWILST(4,IOB4) is QNDflag
c MNWILST(5,IOB) is QBHflag
c MNWILST(6,IOB) is CONCflag
c
      if(GWTUNIT.GT.0) then
       READ(INMNWI,*) MNWIID(IOB),MNWILST(3,IOB),MNWILST(4,IOB),
     & MNWILST(5,IOB),MNWILST(6,IOB)
       write(iout,*) 'WELLID UNIT QNDflag QBHflag CONCflag:'
       write(iout,*) MNWIID(IOB)
       write(iout,*) MNWILST(3,IOB),MNWILST(4,IOB),
     &   MNWILST(5,IOB),MNWILST(6,IOB)
      else
       READ(INMNWI,*) MNWIID(IOB),MNWILST(3,IOB),MNWILST(4,IOB),
     & MNWILST(5,IOB)
       write(iout,*) 'WELLID UNIT QNDflag QBHflag:'
       write(iout,*) MNWIID(IOB)
       write(iout,*) MNWILST(3,IOB),MNWILST(4,IOB),
     & MNWILST(5,IOB)
       MNWILST(6,IOB)=0
      end if
      SITE=MNWIID(IOB)
      call UPCASE(SITE)
c check site vs list of site names in MNWSITE
c Loop over all MNW locations
c   Loop over all wells
      do iw=1,MNWMAX
        MSITE=WELLID(iw)
        call UPCASE(MSITE)
        IF(SITE.EQ.MSITE) THEN
          IS_SITE=1
          MNWILST(1,IOB)=iw
        END IF
      end do      
C
!      WRITE(IOUT,'(I8,3X,A12,3I8)') IOB,SITE,INT(MNWILST(3,IOB)),
!     &  INT(MNWILST(4,IOB)),INT(MNWILST(5,IOB))       
      IF(IS_SITE.EQ.0) THEN
         WRITE(IOUT,*) '***ERROR***   SITE FOR MNWI ',
     *'WELL DESIGNATED FOR OBSERVATION NOT FOUND'
         STOP 'MNWI ERROR'
      ENDIF
C CYCLE THROUGH THE REMAINING RECORDS
      DO IOB=2,MNWOBS
        IS_SITE=0
      if(GWTUNIT.GT.0) then
       READ(INMNWI,*) MNWIID(IOB),MNWILST(3,IOB),MNWILST(4,IOB),
     & MNWILST(5,IOB),MNWILST(6,IOB)
       write(iout,*) 'WELLID UNIT QNDflag QBHflag CONCflag:'
       write(iout,*) MNWIID(IOB),MNWILST(3,IOB),MNWILST(4,IOB),
     & MNWILST(5,IOB),MNWILST(6,IOB)
      else
       READ(INMNWI,*) MNWIID(IOB),MNWILST(3,IOB),MNWILST(4,IOB),
     & MNWILST(5,IOB)
       write(iout,*) 'WELLID UNIT QNDflag QBHflag:'
       write(iout,*) MNWIID(IOB)
       write(iout,*) MNWILST(3,IOB),MNWILST(4,IOB),
     & MNWILST(5,IOB)
       MNWILST(6,IOB)=0
      end if
c check site vs list of site names in WELLID
        SITE=MNWIID(IOB)
        call UPCASE(SITE)
c   Loop over all wells
        do iw=1,MNWMAX
          MSITE=WELLID(iw)
          call UPCASE(MSITE)
          IF(SITE.EQ.MSITE) THEN
            IS_SITE=1
            MNWILST(1,IOB)=iw
          END IF
        end do      
C
!        WRITE(IOUT,'(I8,3X,A12,3I8)') IOB,SITE,INT(MNWILST(3,IOB)),
!     &  INT(MNWILST(4,IOB)),INT(MNWILST(5,IOB))       
        IF(IS_SITE.EQ.0) THEN
         WRITE(IOUT,*) '***ERROR***   SITE FOR MNWI ',
     *'WELL DESIGNATED FOR OBSERVATION NOT FOUND'
          STOP 'MNWI ERROR'
        ENDIF
C
      END DO
!            WRITE(IOUT,'(140A)') 'DATA FOR MNW WELLS DESIGNATED FOR
!     * OBSERVATION WILL BE WRITTEN ON UNIT NUMBERS LISTED ABOVE' 
!            WRITE(IOUT,'(/)')
!  120 FORMAT(///'SITE ID FOR',I4,
!     * ' MNW2 WELL DESIGNATED FOR OBSERVATION:')
!  140 FORMAT(///'SITE IDS FOR',I4,
!     * ' MNW2 WELLS DESIGNATED FOR OBSERVATION:')
!  150 FORMAT(/'  WELL #   SITE ID         UNIT  QNDflag QBHflag')
      RETURN
      END       
C
c_________________________________________________________________________________
c
!      SUBROUTINE GWF2MNW2I7OT(nstp,kkstp,kkper,IGRID)
C     VERSION 20070923 GZH
c
c     ******************************************************************
c    Sort well output into useful tables
c     ******************************************************************
      SUBROUTINE GWF2MNW2I7DA(IGRID)
C  Deallocate MNW MEMORY
      USE GWFMNW2IMODULE
C
        CALL SGWF2MNW2IPNT(IGRID)
        DEALLOCATE(Wel1flag)
        DEALLOCATE(QSUMflag)
        DEALLOCATE(BYNDflag)
        DEALLOCATE(MNWOBS)
        DEALLOCATE(MNWIID)
        DEALLOCATE(MNWILST)
C
      RETURN
      END
      SUBROUTINE SGWF2MNW2IPNT(IGRID)
C  Change MNW data to a different grid.
      USE GWFMNW2IMODULE
C
        Wel1flag=>GWFMNWIDAT(IGRID)%Wel1flag
        QSUMflag=>GWFMNWIDAT(IGRID)%QSUMflag
        BYNDflag=>GWFMNWIDAT(IGRID)%BYNDflag
        MNWOBS=>GWFMNWIDAT(IGRID)%MNWOBS
        MNWIID=>GWFMNWIDAT(IGRID)%MNWIID
        MNWILST=>GWFMNWIDAT(IGRID)%MNWILST
C
      RETURN
      END
      SUBROUTINE SGWF2MNW2IPSV(IGRID)
C  Save MNW2 data for a grid.
      USE GWFMNW2IMODULE
C
        GWFMNWIDAT(IGRID)%Wel1flag=>Wel1flag
        GWFMNWIDAT(IGRID)%QSUMflag=>QSUMflag
        GWFMNWIDAT(IGRID)%BYNDflag=>BYNDflag
        GWFMNWIDAT(IGRID)%MNWOBS=>MNWOBS
        GWFMNWIDAT(IGRID)%MNWIID=>MNWIID
        GWFMNWIDAT(IGRID)%MNWILST=>MNWILST
C
      RETURN
      END
