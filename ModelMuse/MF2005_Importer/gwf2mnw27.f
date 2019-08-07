      MODULE GWFMNW2MODULE
        INTEGER,SAVE,POINTER  ::NMNW2,MNWMAX,NMNWVL,IWL2CB,MNWPRNT
        INTEGER,SAVE,POINTER  ::NODTOT,INTTOT,NTOTNOD
        DOUBLE PRECISION, SAVE,POINTER :: SMALL
        CHARACTER(LEN=20),SAVE, DIMENSION(:),   POINTER     ::WELLID
        CHARACTER(LEN=16),SAVE, DIMENSION(:),   POINTER     ::MNWAUX
        DOUBLE PRECISION, SAVE, DIMENSION(:,:), POINTER     ::MNW2
        DOUBLE PRECISION, SAVE, DIMENSION(:,:), POINTER     ::MNWNOD
        DOUBLE PRECISION, SAVE, DIMENSION(:,:), POINTER     ::MNWINT
        DOUBLE PRECISION, SAVE, DIMENSION(:,:,:), POINTER     ::CapTable
      TYPE GWFMNWTYPE
        INTEGER,POINTER  ::NMNW2,MNWMAX,NMNWVL,IWL2CB,MNWPRNT
        INTEGER,POINTER  ::NODTOT,INTTOT,NTOTNOD
        DOUBLE PRECISION, POINTER :: SMALL
        CHARACTER(LEN=20), DIMENSION(:),   POINTER     ::WELLID
        CHARACTER(LEN=16), DIMENSION(:),   POINTER     ::MNWAUX
        DOUBLE PRECISION,  DIMENSION(:,:), POINTER     ::MNW2
        DOUBLE PRECISION,  DIMENSION(:,:), POINTER     ::MNWNOD
        DOUBLE PRECISION,  DIMENSION(:,:), POINTER     ::MNWINT
        DOUBLE PRECISION,  DIMENSION(:,:,:), POINTER     ::CapTable
      END TYPE
      TYPE(GWFMNWTYPE), SAVE:: GWFMNWDAT(10)
      END MODULE GWFMNW2MODULE


      SUBROUTINE GWF2MNW27AR(IN,IGRID)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR MNW2 PACKAGE.
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT,NLAY
      USE GWFMNW2MODULE, ONLY:NMNW2,MNWMAX,NMNWVL,IWL2CB,MNWPRNT,
     1                       NODTOT,INTTOT,MNWAUX,MNW2,MNWNOD,MNWINT,
     2                       CapTable,SMALL,NTOTNOD,WELLID
C
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
C
C1------Allocate scalar variables, which makes it possible for multiple
C1------grids to be defined.
      ALLOCATE(NMNW2,MNWMAX,NTOTNOD,IWL2CB,MNWPRNT,NODTOT,INTTOT,SMALL,
     1 NMNWVL)
C
C2------IDENTIFY PACKAGE AND INITIALIZE NMNW2.
      WRITE (IOUT, *) 'MNW2:'
!      WRITE(IOUT,1)IN
!    1 format(/,1x,'MNW2 -- MULTI-NODE WELL 2 PACKAGE, VERSION 7,',
!     +' 11/09/2009.',/,4X,'INPUT READ FROM UNIT ',i3)
      NMNW2=0
      ntotnod=0
c-lfk-Dec 2012
      nodtot=0
C
C3------READ MAXIMUM NUMBER OF MNW2 WELLS, UNIT OR FLAG FOR
C3------CELL-BY-CELL FLOW TERMS, AND PRINT FLAG
      CALL URDCOM(IN,IOUT,LINE)
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MNWMAX,R,IOUT,IN)
c--LFK
        IF (MNWMAX.LT.0) THEN
           CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NODTOT,R,IOUT,IN)
           MNWMAX=-MNWMAX
        END IF
c--LFK
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IWL2CB,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MNWPRNT,R,IOUT,IN)
c
      write(iout,*) 'MNWMAX, IWL2CB, MNWPRNT:'
      write(iout,*) MNWMAX, IWL2CB, MNWPRNT 
!      write(iout,3) MNWMAX
!    3 format(1h ,'MAXIMUM OF',i5,' ACTIVE MULTI-NODE WELLS AT ONE TIME')
!      write(iout,*) 
!      if(IWL2CB.gt.0) write(iout,9) IWL2CB
!    9 format(1x, 'CELL-BY-CELL FLOWS WILL BE RECORDED ON UNIT', i3)
!      if(IWL2CB.lt.0) write(iout,*) 'IWL2CB = ',IWL2CB
!      if(IWL2CB.lt.0) write(iout,8)
!    8 format(1x,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')
!      write(iout,*) 'MNWPRNT = ',MNWPRNT
cdebug NoMoIter can be set here and used to force the solution to stop after
cdebug a certain amount of flow solution iterations (used for debugging)
c      NoMoIter=9999
c      write(iout,7) NoMoIter
c    7 format(1x,'Flow rates will not be estimated after the',i4,'th',
c     +          ' iteration')
c
C4------READ AUXILIARY VARIABLES 
      ALLOCATE (MNWAUX(20))
      NAUX=0
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
c      IF(LINE(ISTART:ISTOP).EQ.'CBCALLOCATE' .OR.
c     1   LINE(ISTART:ISTOP).EQ.'CBC') THEN
c         IMNWAL=1
c         WRITE(IOUT,11)
c   11    FORMAT(1X,'MEMORY IS ALLOCATED FOR CELL-BY-CELL BUDGET TERMS')
c         GO TO 10
      IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.
     1        LINE(ISTART:ISTOP).EQ.'AUX') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
         IF(NAUX.LT.5) THEN
            NAUX=NAUX+1
            MNWAUX(NAUX)=LINE(ISTART:ISTOP)
	      WRITE(IOUT,*) 'MNWAUX(NAUX):'
            WRITE(IOUT,*) MNWAUX(NAUX)
!            WRITE(IOUT,12) MNWAUX(NAUX)
!   12       FORMAT(1X,'AUXILIARY MNW2 VARIABLE: ',A)
         END IF
         GO TO 10
      END IF
C
C5------ALLOCATE SPACE FOR MNW2 ARRAYS.
C5------FOR EACH WELL, THERE ARE 30 DATA VALUES PLUS THE AUXILIARY VARIABLES
      NMNWVL=30+NAUX
      ALLOCATE (MNW2(NMNWVL,MNWMAX))
C5------FOR EACH NODE, THERE ARE 31 DATA VALUES
c approximate number of nodes= max mnw wells * number of layers, this works well
c if all are mostly vertical wells.  add 10*nlay+25 for extra room.  ispmnwn is 
c passed out to RP routine to check allocation while reading actual # nodes used
C--LFK  Dec. 2012
      IF (NODTOT.EQ.0) THEN
         NODTOT=(MNWMAX*NLAY)+(10*NLAY)+25
      END IF   
C-LFK
      ALLOCATE (MNWNOD(34,NODTOT))
C5------FOR EACH INTERVAL, THERE ARE 11 DATA VALUES
      ALLOCATE (MNWINT(11,NODTOT))
C5------FOR Capacity Table, 
c  27 is the hard-wired number of entries allowed in the Capacity Table (CapTable)
c  2 is the number of fields in the Capacity Table (CapTable): Lift (1) and Q (2)
      ALLOCATE (CapTable(mnwmax,27,2))
C5------FOR WELLID array, add an extra spot 
      ALLOCATE (WELLID(mnwmax+1))
C
C7------SAVE POINTERS TO DATA AND RETURN.
      CALL SGWF2MNW2PSV(IGRID)
      RETURN
      END
      SUBROUTINE GWF2MNW27RP(IN,kper,Iusip, Iude4,Iusor,Iupcg,
     +                      Iulmg,Iugmg,igwtunit,IGRID)
C     ******************************************************************
c     read mnw2 locations, stress rates, conc, well char., and limits
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,NBOTM,LBOTM,BOTM,
     1                       IBOUND,LAYCBD
      USE GWFMNW2MODULE, ONLY:NMNW2,MNWMAX,NMNWVL,IWL2CB,MNWPRNT,
     1                       NODTOT,INTTOT,MNWAUX,MNW2,MNWNOD,MNWINT,
     2                       CapTable,SMALL,WELLID,NTOTNOD
      USE SIPMODULE,ONLY:HCLOSE
      USE DE4MODULE,ONLY:HCLOSEDE4
      USE PCGMODULE,ONLY:HCLOSEPCG
c      USE GMGMODULE,ONLY:HCLOSEGMG
C     ------------------------------------------------------------------
      INTEGER Qlimit,QCUT,firstnode,lastnode,
     & PUMPLAY,PUMPROW,PUMPCOL,PUMPLOC,PPFLAG,PUMPCAP
      DOUBLE PRECISION CapMult
      DOUBLE PRECISION Rw,Rskin,Kskin,B,C,P,CWC,RwNode,RskinNode,
     & KskinNode,BNode,CNode,PNode,CWCNode,Ztop,Zbotm,Zbotmlast,
     & Zpump,Hlim,Qfrcmn,Qfrcmx,Qdes,Cprime,PP,
     & Qtemp,Hlift,LIFTq0,LIFTqdes,LIFTn,Qn,HWtol
      CHARACTER*20 WELLNAME,LOSSTYPE
C
      CALL SGWF2MNW2PNT(IGRID)
C
c
c------------------------------------------------------------------
c     The 11 rows of the MNW2 array store:
c      Row #  = Description
c------------------------------------------------------------------
c         1   = IACTIV (0: inactive; 1: active)
c         2   = NNODES (number of nodes in this well)
c         3   = LOSSTYPE (0: none; 1:THIEM; 2: SKIN; 3: GENERAL; 4:SPEC. COND)
c         4   = NODNUM (number, in node list (MNWNOD), of first node of well)
c         5   = QDES (desired flow rate for this stress period)
c         6   = QLIMIT (pumpage constraint flag, QLIMIT>0 turns on constraint)
c         7   = HLIM (limiting water level for pumpage constraint)
c         8   = QCUT (pump cutoff flag: QCUT>0 limit by rate, QCUT<0 limit by 
c                     fraction of QDES)
c         9   = Qfrcmn (minimum rate for well to remain active)
c        10   = Qfrcmx (maximum rate to reactivate)
c        11   = Cprime (concentration of inflow)
c        12   = 
c------------------------------------------------------------------
c
	write(iout,*) 'MNW2:'
c  If past first stress period, skip reading data sets1+2
      IF(KPER.GT.1) GOTO 888
c     set defaults
      ntotnod=0
      INTTOT=0
C-------SET SMALL DEPENDING ON CLOSURE CRITERIA OF THE SOLVER
      IF ( Iusip.NE.0 ) SMALL = HCLOSE
      IF ( Iude4.NE.0 ) SMALL = HCLOSEDE4
!     IF ( Iusor.NE.0 ) SMALL = HCLOSESOR
      IF ( Iupcg.NE.0 ) SMALL = HCLOSEPCG
      IF ( Iulmg.NE.0 ) SMALL = 0.0D0  !LMG SETS HCLOSE TO ZERO
c      IF ( Iugmg.NE.0 ) SMALL = HCLOSEGMG
c     initialize
      WELLID=' '
      MNW2=0.0D0
      MNWNOD=0.0D0
      MNWINT=0.0D0
c--lfk
      NDALT=0
!      write(iout,*) 
!      write(iout,*) 'MNW2 Input:'
c0----read MNW info: wellid, nnodes, losstype, pumploc, Qlimit
      DO 1 MNWID=1,MNWMAX
c  initialize CapFlag2 at beginning of simulation
      mnw2(27,MNWID)=0
c
!      write(iout,*) 
!      write(iout,*) 
!      write(iout,*) 'WELLID             NNODES    LOSSTYPE',
!     &' PUMPLOC  Qlimit  PPFLAG PUMPCAP'
c     read Data Set 2a
        read(in,*) WELLID(MNWID),NNODES
        write(iout,*) 'WELLID(MNWID):'
        write(iout,*) WELLID(MNWID)
        write(iout,*) 'NNODES:'
        write(iout,*) NNODES
c     read Data Set 2b
        read(in,*) LOSSTYPE,PUMPLOC,Qlimit,PPFLAG,PUMPCAP
        write(iout,*) 'LOSSTYPE:'
        write(iout,*) LOSSTYPE
        write(iout,*) 'PUMPLOC,Qlimit,PPFLAG,PUMPCAP:'
        write(iout,*) PUMPLOC,Qlimit,PPFLAG,PUMPCAP
c     convert wellid and losstype to uppercase
!      call UPCASE(WELLID(MNWID))
      call UPCASE(LOSSTYPE)
c        write(iout,'(1x,A20,1x,I4,5x,A10,2x,I3,2(5x,I3))')
c     & WELLID(MNWID),INT(NNODES),LOSSTYPE,INT(PUMPLOC),Qlimit,PPFLAG
c     write output with Format depending on LOSSTYPE
        if(LOSSTYPE.EQ.'NONE') then
!	   write(iout,*) 
!     &     'WELLID,NNODES,LOSSTYPE,PUMPLOC,Qlimit,PPFLAG,PUMPCAP:'
!         write(iout,'(1x,A20,1x,I4,6x,A10,1x,I3,3(5x,I3))')
!     & WELLID(MNWID),INT(NNODES),LOSSTYPE,INT(PUMPLOC),Qlimit,PPFLAG,
!     & PUMPCAP
          Continue
        elseif(LOSSTYPE.EQ.'THIEM') then
!	   write(iout,*) 
!     &     'WELLID,NNODES,LOSSTYPE,PUMPLOC,Qlimit,PPFLAG,PUMPCAP:'
!         write(iout,'(1x,A20,1x,I4,6x,A10,1x,I3,3(5x,I3))')
!     & WELLID(MNWID),INT(NNODES),LOSSTYPE,INT(PUMPLOC),Qlimit,PPFLAG,
!     & PUMPCAP
          Continue
        elseif(LOSSTYPE.EQ.'SKIN') then
!	   write(iout,*) 
!     &     'WELLID,NNODES,LOSSTYPE,PUMPLOC,Qlimit,PPFLAG,PUMPCAP:'
!         write(iout,'(1x,A20,1x,I4,6x,A10,1x,I3,3(5x,I3))')
!     & WELLID(MNWID),INT(NNODES),LOSSTYPE,INT(PUMPLOC),Qlimit,PPFLAG,
!     & PUMPCAP
          Continue
        elseif(LOSSTYPE.EQ.'GENERAL') then
!	   write(iout,*) 
!     &     'WELLID,NNODES,LOSSTYPE,PUMPLOC,Qlimit,PPFLAG,PUMPCAP:'
!         write(iout,'(1x,A20,1x,I4,5x,A10,2x,I3,3(5x,I3))')
!     & WELLID(MNWID),INT(NNODES),LOSSTYPE,INT(PUMPLOC),Qlimit,PPFLAG,
!     & PUMPCAP
          Continue
        elseif(LOSSTYPE.EQ.'SPECIFYCWC') then
!	   write(iout,*) 
!     &     'WELLID,NNODES,LOSSTYPE,PUMPLOC,Qlimit,PPFLAG,PUMPCAP:'
!         write(iout,'(1x,A20,1x,I4,2x,A10,5x,I3,3(5x,I3))')
!     & WELLID(MNWID),INT(NNODES),LOSSTYPE,INT(PUMPLOC),Qlimit,PPFLAG,
!     & PUMPCAP
          Continue
        else
	   write(iout,*) '***ERROR*** LOSSTYPE Not Recognized; stopping.'
	   STOP 'MNW2 ERROR - LOSSTYPE'
	  end if
        call UPCASE(WELLID(MNWID))
c     check WELLID vs existing names
        do 2 ID=1,MNWID-1
	    if(WELLID(MNWID).EQ.WELLID(ID)) then
            write(iout,*) '***ERROR*** (MNW2) non-unique MNWID:,',
     *        WELLID(MNWID)
            STOP 'MNW2 ERROR - WELLID'
          end if                
    2   continue
c     set PUMPLOC and PUMPCAP
        MNW2(11,MNWID)=PUMPLOC
        MNW2(22,MNWID)=PUMPCAP
c     CapTable has max 27 entires, so PUMPCAP must not be > 25
        if(PUMPCAP.GT.25) then
          write(iout,*) '***ERROR*** PUMPCAP cannot be greater than 25'
          STOP 'MNW2 ERROR - PUMPCAP'
        end if
c     set IACTIV=0, defaulting well to inactive
        MNW2(1,MNWID)=0
c     set number of nodes
        MNW2(2,MNWID)=NNODES
c     define LOSSTYPE using integers in MNW2 array
        if(LOSSTYPE.EQ.'NONE') then
c     for none, NNODES must be 1
          if(NNODES.NE.1) then
            STOP 'MNW2 ERROR - OPTION: NONE  REQUIRES NNODES=1'
	    end if
          MNW2(3,MNWID)=0
        elseif(LOSSTYPE.EQ.'THIEM') then
          MNW2(3,MNWID)=1 
        elseif(LOSSTYPE.EQ.'SKIN') then
          MNW2(3,MNWID)=2
        elseif(LOSSTYPE.EQ.'GENERAL') then
          MNW2(3,MNWID)=3
        elseif(LOSSTYPE.EQ.'SPECIFYCWC') then
          MNW2(3,MNWID)=4	
	  end if
c     initialize QDES=0
        MNW2(5,MNWID)=0.0
c     set Qlimit flag.  Qlimit.ne.0 means limit or constraint is on.  Qlimit<0 means
c          read it every stress period
        MNW2(6,MNWID)=Qlimit
c     set PPFLAG flag.  PPFLAG>0 means calculate partial penetration effect.
        MNW2(19,MNWID)=PPFLAG
c     end read Data Set 2a and 2b
c
c     warning if LOSSTYPE=SPECIFYCWC and PPFLAG>0 (no PP correction done for this LOSSTYPE)
!        if(LOSSTYPE.EQ.'SPECIFYCWC'.and.PPFLAG.GT.0) then
!          write(iout,*)
!          write(iout,*) '***WARNING*** Partial penetration not',
!     & ' calculated for LOSSTYPE = SPECIFYCWC'
!          write(iout,*)
!        end if

c
c     read Data Set 2c, depending on LOSSTYPE (MNW2(3,MNWID)
        SELECT CASE (INT(MNW2(3,MNWID)))
          CASE (1)
            READ(in,*) Rw
	      write(IOUT,*) 'Rw:'
	      write(IOUT,*) Rw
c     don't allow Rw = 0
            if(Rw.eq.0.0) then
              write(iout,*) '***ERROR*** Rw=0.0; Rw read=',Rw
              STOP 'MNW2 ERROR - Rw'
            endif
          CASE (2)
            READ(in,*) Rw,Rskin,Kskin
	      write(IOUT,*) 'Rw,Rskin,Kskin:'
	      write(IOUT,*) Rw,Rskin,Kskin
            if(Rw.eq.0.0) then
              write(iout,*) '***ERROR*** Rw=0.0; Rw read=',Rw
              STOP 'MNW2 ERROR - Rw'
            endif
            if(Rskin.eq.0.0) then
              write(iout,*) '***ERROR*** Rskin=0.0; Rskin read=',Rskin
              STOP 'MNW2 ERROR'
            endif
            if(Kskin.eq.0.0) then
              write(iout,*) '***ERROR*** Kskin=0.0; Kskin read=',Kskin
              STOP 'MNW2 ERROR - Kskin'
            endif
          CASE (3)
            READ(in,*) Rw,B,C,P
            write(iout,*) 'Rw,B,C,P:'
            write(iout,*) Rw,B,C,P
            if(Rw.eq.0.0) then
              write(iout,*) '***ERROR*** Rw=0.0; Rw read=',Rw
              STOP 'MNW2 ERROR - Rw'
            endif
            if(P.gt.0.0.and.(P.lt.1.0.or.P.gt.3.5)) then
              write(iout,*) '***ERROR*** P=',P,' exceeds 1 <= P <=3.5'
              STOP 'MNW2 ERROR - P'
            endif
          CASE (4)
            READ(in,*) CWC            
            write(iout,*) 'CWC:'
            write(iout,*) CWC            
        END SELECT
c     end read Data Set 2c
c
c     set HWflag (horizontal well flag) to 0 as default
c     can be set to 1 (true) if NNODES>0 and any R,C is different than first
            MNW2(21,MNWID)=0
c     read Data Set 2d, the list of nodes
c
c     if nnodes>0, read IL,IR,IC and LOSSTYPE variables set < 0
        IF(NNODES.GT.0) THEN
c     calculate first node number in nodal list= (total nodes so far) + 1
c     this will be used to access the information in the nodal array (MNWNOD)
          NODNUM=ntotnod+1
          MNW2(4,MNWID)=NODNUM
c     count nodes (will uses this to check vs. allocation)
          ntotnod=ntotnod+NNODES
          if(ntotnod.gt.nodtot) then
            write(iout,*)
            write(iout,*) 'MNW2 NODE ARRAY ALLOCATION INSUFFICIENT'
            STOP 'MNW2 ALLOCATION ERROR'
          end if
c     loop over nodes 
          DO 3 INODE=1,NNODES
c     If PPFLAG=0, don't read PP variable
           PPFLAG=INT(MNW2(19,mnwid))
           IF(PPFLAG.eq.0) then           
c     access the LOSSTYPE
            SELECT CASE (INT(MNW2(3,MNWID)))
c     LOSSTYPE=NONE, read IL,IR,IC only
              CASE (0)
                READ(in,*) IL,IR,IC
                write(iout,*) 'IL,IR,IC:'
                write(iout,*) IL,IR,IC
c
c     LOSSTYPE=THIEM, read IL,IR,IC,{Rw}
              CASE (1)
                IF(Rw.GT.0.0) THEN            
                  READ(in,*) IL,IR,IC
                  write(iout,*) 'IL,IR,IC:'
                  write(iout,*) IL,IR,IC
c     Rw at each node is the same if Rw>0.0  
                  RwNode=Rw         
                ELSE
c     If Rw<0, read in separate Rw for each node
                  READ(in,*) IL,IR,IC,RwNode
                  write(iout,*) 'IL,IR,IC,RwNode:'
                  write(iout,*) IL,IR,IC,RwNode
                END IF
c     Set Rw in node list, spot is 1st node (NODNUM) + current step (INODE) - 1
                MNWNOD(5,NODNUM+INODE-1)=RwNode             
c
c     LOSSTYPE=SKIN, read IL,IR,IC,{Rw Rskin Kskin}
              CASE (2)
                IF(Rw.GT.0.0) THEN            
                  IF(Rskin.GT.0.0) THEN
                    IF(Kskin.GT.0.0) THEN
                      READ(in,*) IL,IR,IC
                      write(iout,*) 'IL,IR,IC:'
                      write(iout,*) IL,IR,IC
                      RwNode=Rw             
                      RskinNode=Rskin            
                      KskinNode=Kskin
                    ELSE             
                      READ(in,*) IL,IR,IC,KskinNode
                      write(iout,*) 'IL,IR,IC,KskinNode:'
                      write(iout,*) IL,IR,IC,KskinNode
                      RwNode=Rw             
                      RskinNode=Rskin            
                    ENDIF
c                 else Rskin<0
                  ELSE
                    IF(Kskin.GT.0.0) THEN
                      READ(in,*) IL,IR,IC,RskinNode
                      write(iout,*) 'IL,IR,IC,RskinNode:'
                      write(iout,*) IL,IR,IC,RskinNode
                      RwNode=Rw             
                      KskinNode=Kskin
                    ELSE             
                      READ(in,*) IL,IR,IC,RskinNode,KskinNode
                      write(iout,*) 'IL,IR,IC,RskinNode,KskinNode:'
                      write(iout,*) IL,IR,IC,RskinNode,KskinNode
                      RwNode=Rw             
                    ENDIF
                  ENDIF
c               else Rw<0
                ELSE
                  IF(Rskin.GT.0.0) THEN
                    IF(Kskin.GT.0.0) THEN
                      READ(in,*) IL,IR,IC,RwNode
                      write(iout,*) 'IL,IR,IC,RwNode:'
                      write(iout,*) IL,IR,IC,RwNode
                      RskinNode=Rskin            
                      KskinNode=Kskin
                    ELSE             
                      READ(in,*) IL,IR,IC,RwNode,KskinNode
                      write(iout,*) 'IL,IR,IC,RwNode,KskinNode:'
                      write(iout,*) IL,IR,IC,RwNode,KskinNode
                      RskinNode=Rskin            
                    ENDIF
c                 else Rskin<0
                  ELSE
                    IF(Kskin.GT.0.0) THEN
                      READ(in,*) IL,IR,IC,RwNode,RskinNode
                      write(iout,*) 'IL,IR,IC,RwNode,RskinNode:'
                      write(iout,*) IL,IR,IC,RwNode,RskinNode
                      KskinNode=Kskin
                    ELSE             
                      READ(in,*) IL,IR,IC,RwNode,RskinNode,KskinNode
                      write(iout,*) 
     &	  			  'IL,IR,IC,RwNode,RskinNode,KskinNode:'
                      write(iout,*) IL,IR,IC,RwNode,RskinNode,KskinNode
                    ENDIF
                  ENDIF
                END IF
c     Set vars in node list, spot is 1st node (NODNUM) + current step (INODE) - 1
                MNWNOD(5,NODNUM+INODE-1)=RwNode             
                MNWNOD(6,NODNUM+INODE-1)=RskinNode             
                MNWNOD(7,NODNUM+INODE-1)=KskinNode             
c
c     LOSSTYPE=GENERAL, read IL,IR,IC,{Rw B C P}
              CASE (3)
                IF(Rw.GT.0.0) THEN            
                  IF(B.GE.0.0) THEN
                    IF(C.GE.0.0) THEN
                      IF(P.GE.0.0) THEN
                        READ(in,*) IL,IR,IC
                        write(iout,*) 'IL,IR,IC:'
                        write(iout,*) IL,IR,IC
                        RwNode=Rw             
                        BNode=B            
                        CNode=C
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,PNode
                        write(iout,*) 'IL,IR,IC,PNode:'
                        write(iout,*) IL,IR,IC,PNode
                        RwNode=Rw             
                        BNode=B            
                        CNode=C
                      END IF
c                   else C<0
                    ELSE             
                      IF(P.GE.0.0) THEN
                        READ(in,*) IL,IR,IC,CNode
                        write(iout,*) 'IL,IR,IC,CNode:'
                        write(iout,*) IL,IR,IC,CNode
                        RwNode=Rw             
                        BNode=B            
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,CNode,PNode
                        write(iout,*) 'IL,IR,IC,CNode,PNode:'
                        write(iout,*) IL,IR,IC,CNode,PNode
                        RwNode=Rw             
                        BNode=B            
                      END IF
                    ENDIF
c                 else B<0
                  ELSE
                    IF(C.GE.0.0) THEN
                      IF(P.GE.0.0) THEN
                        READ(in,*) IL,IR,IC,BNode
                        write(iout,*) 'IL,IR,IC,BNode:'
                        write(iout,*) IL,IR,IC,BNode
                        RwNode=Rw             
                        CNode=C
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,BNode,PNode
                        write(iout,*) 'IL,IR,IC,BNode,PNode:'
                        write(iout,*) IL,IR,IC,BNode,PNode
                        RwNode=Rw             
                        CNode=C
                      END IF
c                   else C<0
                    ELSE             
                      IF(P.GE.0.0) THEN
                        READ(in,*) IL,IR,IC,BNode,CNode
                        write(iout,*) 'IL,IR,IC,BNode,CNode:'
                        write(iout,*) IL,IR,IC,BNode,CNode
                        RwNode=Rw             
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,BNode,CNode,PNode
                        write(iout,*) 'IL,IR,IC,BNode,CNode,PNode:'
                        write(iout,*) IL,IR,IC,BNode,CNode,PNode
                        RwNode=Rw             
                      END IF
                    ENDIF
                  ENDIF
c               else Rw<0
                ELSE
                  IF(B.GE.0.0) THEN
                    IF(C.GE.0.0) THEN
                      IF(P.GE.0.0) THEN
                        READ(in,*) IL,IR,IC,RwNode
                        write(iout,*) 'IL,IR,IC,RwNode:'
                        write(iout,*) IL,IR,IC,RwNode
                        BNode=B            
                        CNode=C
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,Rwnode,PNode
                        write(iout,*) 'IL,IR,IC,Rwnode,PNode:'
                        write(iout,*) IL,IR,IC,Rwnode,PNode
                        BNode=B            
                        CNode=C
                      END IF
c                   else C<0
                    ELSE             
                      IF(P.GE.0.0) THEN
                        READ(in,*) IL,IR,IC,Rwnode,CNode
                        write(iout,*) 'IL,IR,IC,Rwnode,CNode:'
                        write(iout,*) IL,IR,IC,Rwnode,CNode
                        BNode=B            
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,Rwnode,CNode,PNode
                        write(iout,*) 'IL,IR,IC,Rwnode,CNode,PNode:'
                        write(iout,*) IL,IR,IC,Rwnode,CNode,PNode
                        BNode=B            
                      END IF
                    ENDIF
c                 else B<0
                  ELSE
                    IF(C.GE.0.0) THEN
                      IF(P.GE.0.0) THEN
                        READ(in,*) IL,IR,IC,Rwnode,BNode
                        write(iout,*) 'IL,IR,IC,Rwnode,BNode:'
                        write(iout,*) IL,IR,IC,Rwnode,BNode
                        CNode=C
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,Rwnode,BNode,PNode
                        write(iout,*) 'IL,IR,IC,Rwnode,BNode,PNode:'
                        write(iout,*) IL,IR,IC,Rwnode,BNode,PNode
                        CNode=C
                      END IF
c                   else C<0
                    ELSE             
                      IF(P.GE.0.0) THEN
                        READ(in,*) IL,IR,IC,Rwnode,BNode,CNode
                        write(iout,*) 'IL,IR,IC,Rwnode,BNode,CNode:'
                        write(iout,*) IL,IR,IC,Rwnode,BNode,CNode
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,Rwnode,BNode,CNode,PNode
                        write(iout,*) 
     &                   'IL,IR,IC,Rwnode,BNode,CNode,PNode:'
                        write(iout,*) IL,IR,IC,Rwnode,BNode,CNode,PNode
                      END IF
                    ENDIF
                  ENDIF
                END IF
c     Set vars in node list, spot is 1st node (NODNUM) + current step (INODE) - 1
                MNWNOD(5,NODNUM+INODE-1)=RwNode             
                MNWNOD(8,NODNUM+INODE-1)=BNode             
                MNWNOD(9,NODNUM+INODE-1)=CNode            
                MNWNOD(10,NODNUM+INODE-1)=PNode             
c
c     LOSSTYPE=SPECIFYcwc, read IL,IR,IC,{CWC}
              CASE (4)
                IF(CWC.GT.0.0) THEN            
                  READ(in,*) IL,IR,IC
                  write(iout,*) 'IL,IR,IC:'
                  write(iout,*) IL,IR,IC
                  CWCNode=CWC         
                ELSE
                  READ(in,*) IL,IR,IC,CWCNode
                  write(iout,*) 'IL,IR,IC,CWCNode:'
                  write(iout,*) IL,IR,IC,CWCNode
                END IF
                MNWNOD(11,NODNUM+INODE-1)=CWCNode             
            END SELECT
c    ELSE if PPFLAG NE 0, read PP flag
           ELSE
c     access the LOSSTYPE
            SELECT CASE (INT(MNW2(3,MNWID)))
c     LOSSTYPE=NONE, read IL,IR,IC only
              CASE (0)
                READ(in,*) IL,IR,IC,PP
                write(iout,*) 'IL,IR,IC,PP:'
                write(iout,*) IL,IR,IC,PP
c
c     LOSSTYPE=THIEM, read IL,IR,IC,{Rw}
              CASE (1)
                IF(Rw.GT.0.0) THEN           
                  READ(in,*) IL,IR,IC,PP
                  write(iout,*) 'IL,IR,IC,PP:'
                  write(iout,*) IL,IR,IC,PP
c     Rw at each node is the same if Rw>0.0  
                  RwNode=Rw         
                ELSE
c     If Rw<0, read in separate Rw for each node
                  READ(in,*) IL,IR,IC,RwNode,PP
                  write(iout,*) 'IL,IR,IC,RwNode,PP:'
                  write(iout,*) IL,IR,IC,RwNode,PP
                END IF
c     Set Rw in node list, spot is 1st node (NODNUM) + current step (INODE) - 1
                MNWNOD(5,NODNUM+INODE-1)=RwNode             
c
c     LOSSTYPE=SKIN, read IL,IR,IC,{Rw Rskin Kskin}
              CASE (2)
                IF(Rw.GT.0.0) THEN            
                  IF(Rskin.GT.0.0) THEN
                    IF(Kskin.GT.0.0) THEN
                      READ(in,*) IL,IR,IC,PP
                      write(iout,*) 'IL,IR,IC,PP:'
                      write(iout,*) IL,IR,IC,PP
                      RwNode=Rw             
                      RskinNode=Rskin            
                      KskinNode=Kskin
                    ELSE             
                      READ(in,*) IL,IR,IC,KskinNode,PP
                      write(iout,*) 'IL,IR,IC,KskinNode,PP:'
                      write(iout,*) IL,IR,IC,KskinNode,PP
                      RwNode=Rw             
                      RskinNode=Rskin            
                    ENDIF
c                 else Rskin<0
                  ELSE
                    IF(Kskin.GT.0.0) THEN
                      READ(in,*) IL,IR,IC,RskinNode,PP
                      write(iout,*) 'IL,IR,IC,RskinNode,PP:'
                      write(iout,*) IL,IR,IC,RskinNode,PP
                      RwNode=Rw             
                      KskinNode=Kskin
                    ELSE             
                      READ(in,*) IL,IR,IC,RskinNode,KskinNode,PP
                      write(iout,*) 'IL,IR,IC,RskinNode,KskinNode,PP:'
                      write(iout,*) IL,IR,IC,RskinNode,KskinNode,PP
                      RwNode=Rw             
                    ENDIF
                  ENDIF
c               else Rw<0
                ELSE
                  IF(Rskin.GT.0.0) THEN
                    IF(Kskin.GT.0.0) THEN
                      READ(in,*) IL,IR,IC,RwNode,PP
                      write(iout,*) 'IL,IR,IC,RwNode,PP:'
                      write(iout,*) IL,IR,IC,RwNode,PP
                      RskinNode=Rskin            
                      KskinNode=Kskin
                    ELSE             
                      READ(in,*) IL,IR,IC,RwNode,KskinNode,PP
                      write(iout,*) 'IL,IR,IC,RwNode,KskinNode,PP:'
                      write(iout,*) IL,IR,IC,RwNode,KskinNode,PP
                      RskinNode=Rskin            
                    ENDIF
c                 else Rskin<0
                  ELSE
                    IF(Kskin.GT.0.0) THEN
                      READ(in,*) IL,IR,IC,RwNode,RskinNode,PP
                      write(iout,*) 'IL,IR,IC,RwNode,RskinNode,PP:'
                      write(iout,*) IL,IR,IC,RwNode,RskinNode,PP
                      KskinNode=Kskin
                    ELSE             
                      READ(in,*) IL,IR,IC,RwNode,RskinNode,KskinNode,PP
                      write(iout,*) 
     &				  'IL,IR,IC,RwNode,RskinNode,KskinNode,PP:'
                      write(iout,*) 
     &                  IL,IR,IC,RwNode,RskinNode,KskinNode,PP
                    ENDIF
                  ENDIF
                END IF
c     Set vars in node list, spot is 1st node (NODNUM) + current step (INODE) - 1
                MNWNOD(5,NODNUM+INODE-1)=RwNode             
                MNWNOD(6,NODNUM+INODE-1)=RskinNode             
                MNWNOD(7,NODNUM+INODE-1)=KskinNode             
c
c     LOSSTYPE=GENERAL, read IL,IR,IC,{Rw B C P}
              CASE (3)
                IF(Rw.GT.0.0) THEN            
                  IF(B.GE.0.0) THEN
                    IF(C.GE.0.0) THEN
                      IF(P.GE.0.0) THEN
                        READ(in,*) IL,IR,IC,PP
                        write(iout,*) 'IL,IR,IC,PP:'
                        write(iout,*) IL,IR,IC,PP
                        RwNode=Rw             
                        BNode=B            
                        CNode=C
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,PNode,PP
                        write(iout,*) 'IL,IR,IC,PNode,PP:'
                        write(iout,*) IL,IR,IC,PNode,PP
                        RwNode=Rw             
                        BNode=B            
                        CNode=C
                      END IF
c                   else C<0
                    ELSE             
                      IF(P.GE.0.0) THEN
                        READ(in,*) IL,IR,IC,CNode,PP
                        write(iout,*) 'IL,IR,IC,CNode,PP:'
                        write(iout,*) IL,IR,IC,CNode,PP
                        RwNode=Rw             
                        BNode=B            
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,CNode,PNode,PP
                        write(iout,*) 'IL,IR,IC,CNode,PNode,PP:'
                        write(iout,*) IL,IR,IC,CNode,PNode,PP
                        RwNode=Rw             
                        BNode=B            
                      END IF
                    ENDIF
c                 else B<0
                  ELSE
                    IF(C.GE.0.0) THEN
                      IF(P.GE.0.0) THEN
                        READ(in,*) IL,IR,IC,BNode,PP
                        write(iout,*) 'IL,IR,IC,BNode,PP:'
                        write(iout,*) IL,IR,IC,BNode,PP
                        RwNode=Rw             
                        CNode=C
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,BNode,PNode,PP
                        write(iout,*) 'IL,IR,IC,BNode,PNode,PP:'
                        write(iout,*) IL,IR,IC,BNode,PNode,PP
                        RwNode=Rw             
                        CNode=C
                      END IF
c                   else C<0
                    ELSE             
                      IF(P.GE.0.0) THEN
                        READ(in,*) IL,IR,IC,BNode,CNode,PP
                        write(iout,*) 'IL,IR,IC,BNode,CNode,PP:'
                        write(iout,*) IL,IR,IC,BNode,CNode,PP
                        RwNode=Rw             
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,BNode,CNode,PNode,PP
                        write(iout,*) 'IL,IR,IC,BNode,CNode,PNode,PP:'
                        write(iout,*) IL,IR,IC,BNode,CNode,PNode,PP
                        RwNode=Rw             
                      END IF
                    ENDIF
                  ENDIF
c               else Rw<0
                ELSE
                  IF(B.GE.0.0) THEN
                    IF(C.GE.0.0) THEN
                      IF(P.GE.0.0) THEN
                        READ(in,*) IL,IR,IC,RwNode,PP
                        write(iout,*) 'IL,IR,IC,RwNode,PP:'
                        write(iout,*) IL,IR,IC,RwNode,PP
                        BNode=B            
                        CNode=C
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,Rwnode,PNode,PP
                        write(iout,*) 'IL,IR,IC,Rwnode,PNode,PP:'
                        write(iout,*) IL,IR,IC,Rwnode,PNode,PP
                        BNode=B            
                        CNode=C
                      END IF
c                   else C<0
                    ELSE             
                      IF(P.GE.0.0) THEN
                        READ(in,*) IL,IR,IC,Rwnode,CNode,PP
                        write(iout,*) 'IL,IR,IC,Rwnode,CNode,PP:'
                        write(iout,*) IL,IR,IC,Rwnode,CNode,PP
                        BNode=B            
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,Rwnode,CNode,PNode,PP
                        write(iout,*) 'IL,IR,IC,Rwnode,CNode,PNode,PP:'
                        write(iout,*) IL,IR,IC,Rwnode,CNode,PNode,PP
                        BNode=B            
                      END IF
                    ENDIF
c                 else B<0
                  ELSE
                    IF(C.GE.0.0) THEN
                      IF(P.GE.0.0) THEN
                        READ(in,*) IL,IR,IC,Rwnode,BNode,PP
                        write(iout,*) 'IL,IR,IC,Rwnode,BNode,PP:'
                        write(iout,*) IL,IR,IC,Rwnode,BNode,PP
                        CNode=C
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,Rwnode,BNode,PNode,PP
                        write(iout,*) 'IL,IR,IC,Rwnode,BNode,PNode,PP:'
                        write(iout,*) IL,IR,IC,Rwnode,BNode,PNode,PP
                        CNode=C
                      END IF
c                   else C<0
                    ELSE             
                      IF(P.GE.0.0) THEN
                        READ(in,*) IL,IR,IC,Rwnode,BNode,CNode,PP
                        write(iout,*) 'IL,IR,IC,Rwnode,BNode,CNode,PP:'
                        write(iout,*) IL,IR,IC,Rwnode,BNode,CNode,PP
                        PNode=P
                      ELSE
                        READ(in,*) IL,IR,IC,Rwnode,BNode,CNode,PNode,PP
                        write(iout,*) 
     &                    'IL,IR,IC,Rwnode,BNode,CNode,PNode,PP:'
                        write(iout,*) 
     &                    IL,IR,IC,Rwnode,BNode,CNode,PNode,PP
                      END IF
                    ENDIF
                  ENDIF
                END IF
c     Set vars in node list, spot is 1st node (NODNUM) + current step (INODE) - 1
                MNWNOD(5,NODNUM+INODE-1)=RwNode             
                MNWNOD(8,NODNUM+INODE-1)=BNode             
                MNWNOD(9,NODNUM+INODE-1)=CNode            
                MNWNOD(10,NODNUM+INODE-1)=PNode             
c
c     LOSSTYPE=SPECIFYcwc, read IL,IR,IC,{CWC}
              CASE (4)
                IF(CWC.GT.0.0) THEN            
                  READ(in,*) IL,IR,IC,PP
                  write(iout,*) 'IL,IR,IC,PP:'
                  write(iout,*) IL,IR,IC,PP
                  CWCNode=CWC         
                ELSE
                  READ(in,*) IL,IR,IC,CWCNode,PP
                  write(iout,*) 'IL,IR,IC,CWCNode,PP:'
                  write(iout,*) IL,IR,IC,CWCNode,PP
                END IF
                MNWNOD(11,NODNUM+INODE-1)=CWCNode             
            END SELECT            
           END IF
c     save node location, set Qdes=0.0, set PP, flag ZPD
            MNWNOD(1,NODNUM+INODE-1)=IL             
            MNWNOD(2,NODNUM+INODE-1)=IR            
            MNWNOD(3,NODNUM+INODE-1)=IC           
            MNWNOD(4,NODNUM+INODE-1)=0.0           
            if(PPFLAG.LE.0) then
              PP=0.d0
            end if
            MNWNOD(19,NODNUM+INODE-1)=PP  
c     save IR and IC to check vs the subsequent nodes for vert/horiz
            IF(INODE.EQ.1) THEN
              IRlast=IR
              IClast=IC
            ELSE
c     if any node is a different R,C, this is a nonvertical well
              IF((IR.NE.IRlast).OR.(IC.NE.IClast)) THEN
c       set HWflag to true
                MNW2(21,MNWID)=1
              END IF
            END IF

c     if partial penetration ne 0, set ZPD to 1d30.  this will act as a flag
c     until ZDP (and ZPL) are set for the well)
            if(pp.ne.0.d0) MNWNOD(20,NODNUM+INODE-1)=1d30         
    3     CONTINUE
c
c       end nnodes>0 read statements
c
c     if nnodes<0, read in Ztop and Zbot which define intervals
        ELSE
c
c     calculate first interval number in interval list= (total ints so far) + 1
c     this will be used to access the information in the interval array (MNWINT)
          INTNUM=INTTOT+1
          MNW2(13,MNWID)=INTNUM
c     the abs value of NNODES represents the number of intervals to de defined
          NINTVL=ABS(NNODES)
c     count intervals to check vs. allocation
          INTTOT=INTTOT+NINTVL
c     initialize interval node counter
          intnodes=0
c     loop over the intervals in this well
          DO 4 IINT=1,NINTVL
c
c     access the LOSSTYPE
            SELECT CASE (INT(MNW2(3,MNWID)))
c     LOSSTYPE=NONE, read Ztop,Zbotm,IR,IC only
              CASE (0)
                READ(in,*) Ztop,Zbotm,IR,IC
                write(iout,*) 'Ztop,Zbotm,IR,IC:'
                write(iout,*) Ztop,Zbotm,IR,IC
c
c     LOSSTYPE=THIEM, read Ztop,Zbotm,IR,IC,{Rw}
              CASE (1)
                IF(Rw.GT.0.0) THEN            
                  READ(in,*) Ztop,Zbotm,IR,IC
                  write(iout,*) 'Ztop,Zbotm,IR,IC:'
                  write(iout,*) Ztop,Zbotm,IR,IC
c     Rw at each node is the same if Rw>0.0  
                  RwNode=Rw         
                ELSE
c     If Rw<0, read in separate Rw for each node
                  READ(in,*) Ztop,Zbotm,IR,IC,RwNode
                  write(iout,*) 'Ztop,Zbotm,IR,IC,RwNode:'
                  write(iout,*) Ztop,Zbotm,IR,IC,RwNode
                END IF
c     Set Rw in interval list, spot is 1st int (INTNUM) + current step (IINT) - 1
                MNWINT(5,INTNUM+IINT-1)=RwNode             
c
c     LOSSTYPE=SKIN, read Ztop,Zbotm,IR,IC,{Rw Rskin Kskin}
              CASE (2)
                IF(Rw.GT.0.0) THEN            
                  IF(Rskin.GT.0.0) THEN
                    IF(Kskin.GT.0.0) THEN
                      READ(in,*) Ztop,Zbotm,IR,IC
                      write(iout,*) 'Ztop,Zbotm,IR,IC:'
                      write(iout,*) Ztop,Zbotm,IR,IC
                      RwNode=Rw             
                      RskinNode=Rskin            
                      KskinNode=Kskin
                    ELSE             
                      READ(in,*) Ztop,Zbotm,IR,IC,KskinNode
                      write(iout,*) 'Ztop,Zbotm,IR,IC,KskinNode:'
                      write(iout,*) Ztop,Zbotm,IR,IC,KskinNode
                      RwNode=Rw             
                      RskinNode=Rskin            
                    ENDIF
c                 else Rskin<0
                  ELSE
                    IF(Kskin.GT.0.0) THEN
                      READ(in,*) Ztop,Zbotm,IR,IC,RskinNode
                      write(iout,*) 'Ztop,Zbotm,IR,IC,RskinNode:'
                      write(iout,*) Ztop,Zbotm,IR,IC,RskinNode
                      RwNode=Rw             
                      KskinNode=Kskin
                    ELSE             
                      READ(in,*) Ztop,Zbotm,IR,IC,RskinNode,KskinNode
                      write(iout,*) 
     &                  'Ztop,Zbotm,IR,IC,RskinNode,KskinNode:'
                      write(iout,*) Ztop,Zbotm,IR,IC,RskinNode,KskinNode
                      RwNode=Rw             
                    ENDIF
                  ENDIF
c               else Rw<0
                ELSE
                  IF(Rskin.GT.0.0) THEN
                    IF(Kskin.GT.0.0) THEN
                      READ(in,*) Ztop,Zbotm,IR,IC,RwNode
                      write(iout,*) 'Ztop,Zbotm,IR,IC,RwNode:'
                      write(iout,*) Ztop,Zbotm,IR,IC,RwNode
                      RskinNode=Rskin            
                      KskinNode=Kskin
                    ELSE             
                      READ(in,*) Ztop,Zbotm,IR,IC,RwNode,KskinNode
                      write(iout,*) 'Ztop,Zbotm,IR,IC,RwNode,KskinNode:'
                      write(iout,*) Ztop,Zbotm,IR,IC,RwNode,KskinNode
                      RskinNode=Rskin            
                    ENDIF
c                 else Rskin<0
                  ELSE
                    IF(Kskin.GT.0.0) THEN
                      READ(in,*) Ztop,Zbotm,IR,IC,RwNode,RskinNode
                      write(iout,*) 'Ztop,Zbotm,IR,IC,RwNode,RskinNode:'
                      write(iout,*) Ztop,Zbotm,IR,IC,RwNode,RskinNode
                        KskinNode=Kskin
                    ELSE             
                      READ(in,*) Ztop,Zbotm,IR,IC,RwNode,RskinNode,
     &                  KskinNode
                      write(iout,*) 
     &                  'Ztop,Zbotm,IR,IC,RwNode,RskinNode,KskinNode:'
                      write(iout,*) Ztop,Zbotm,IR,IC,RwNode,RskinNode,
     &                  KskinNode
                    ENDIF
                  ENDIF
                END IF
c     Set vars for interval
                MNWINT(5,INTNUM+IINT-1)=RwNode             
                MNWINT(6,INTNUM+IINT-1)=RskinNode             
                MNWINT(7,INTNUM+IINT-1)=KskinNode             
c
c     LOSSTYPE=GENERAL, read Ztop,Zbotm,IR,IC,{Rw B C P}
              CASE (3)
                IF(Rw.GT.0.0) THEN            
                  IF(B.GE.0.0) THEN
                    IF(C.GE.0.0) THEN
                      IF(P.GE.0.0) THEN
                        READ(in,*) Ztop,Zbotm,IR,IC
                        write(iout,*) 'Ztop,Zbotm,IR,IC:'
                        write(iout,*) Ztop,Zbotm,IR,IC
                        RwNode=Rw             
                        BNode=B            
                        CNode=C
                        PNode=P
                      ELSE
                        READ(in,*) Ztop,Zbotm,IR,IC,PNode
                        write(iout,*) Ztop,Zbotm,IR,IC,PNode
                        write(iout,*) Ztop,Zbotm,IR,IC,PNode
                        RwNode=Rw             
                        BNode=B            
                        CNode=C
                      END IF
c                   else C<0
                    ELSE             
                      IF(P.GE.0.0) THEN
                        READ(in,*) Ztop,Zbotm,IR,IC,CNode
                        write(iout,*) 'Ztop,Zbotm,IR,IC,CNode:'
                        write(iout,*) Ztop,Zbotm,IR,IC,CNode
                        RwNode=Rw             
                        BNode=B            
                        PNode=P
                      ELSE
                        READ(in,*) Ztop,Zbotm,IR,IC,CNode,PNode
                        write(iout,*) 'Ztop,Zbotm,IR,IC,CNode,PNode:'
                        write(iout,*) Ztop,Zbotm,IR,IC,CNode,PNode
                        RwNode=Rw             
                        BNode=B            
                      END IF
                    ENDIF
c                 else B<0
                  ELSE
                    IF(C.GE.0.0) THEN
                      IF(P.GE.0.0) THEN
                        READ(in,*) Ztop,Zbotm,IR,IC,BNode
                        write(iout,*) 'Ztop,Zbotm,IR,IC,BNode:'
                        write(iout,*) Ztop,Zbotm,IR,IC,BNode
                        RwNode=Rw             
                        CNode=C
                        PNode=P
                      ELSE
                        READ(in,*) Ztop,Zbotm,IR,IC,BNode,PNode
                        write(iout,*) 'Ztop,Zbotm,IR,IC,BNode,PNode:'
                        write(iout,*) Ztop,Zbotm,IR,IC,BNode,PNode
                        RwNode=Rw             
                        CNode=C
                      END IF
c                   else C<0
                    ELSE             
                      IF(P.GE.0.0) THEN
                        READ(in,*) Ztop,Zbotm,IR,IC,BNode,CNode
                        write(iout,*) 'Ztop,Zbotm,IR,IC,BNode,CNode:'
                        write(iout,*) Ztop,Zbotm,IR,IC,BNode,CNode
                        RwNode=Rw             
                        PNode=P
                      ELSE
                        READ(in,*) Ztop,Zbotm,IR,IC,BNode,CNode,PNode
                        write(iout,*) 
     &                    'Ztop,Zbotm,IR,IC,BNode,CNode,PNode:'
                        write(iout,*) Ztop,Zbotm,IR,IC,BNode,CNode,PNode
                        RwNode=Rw             
                      END IF
                    ENDIF
                  ENDIF
c               else Rw<0
                ELSE
                  IF(B.GE.0.0) THEN
                    IF(C.GE.0.0) THEN
                      IF(P.GE.0.0) THEN
                        READ(in,*) Ztop,Zbotm,IR,IC,RwNode
                        write(iout,*) 'Ztop,Zbotm,IR,IC,RwNode:'
                        write(iout,*) Ztop,Zbotm,IR,IC,RwNode
                        BNode=B            
                        CNode=C
                        PNode=P
                      ELSE
                        READ(in,*) Ztop,Zbotm,IR,IC,Rwnode,PNode
                        write(iout,*) 'Ztop,Zbotm,IR,IC,Rwnode,PNode:'
                        write(iout,*) Ztop,Zbotm,IR,IC,Rwnode,PNode
                        BNode=B            
                        CNode=C
                      END IF
c                   else C<0
                    ELSE             
                      IF(P.GE.0.0) THEN
                        READ(in,*) Ztop,Zbotm,IR,IC,Rwnode,CNode
                        write(iout,*) 'Ztop,Zbotm,IR,IC,Rwnode,CNode:'
                        write(iout,*) Ztop,Zbotm,IR,IC,Rwnode,CNode
                        BNode=B            
                        PNode=P
                      ELSE
                        READ(in,*) Ztop,Zbotm,IR,IC,Rwnode,CNode,PNode
                        write(iout,*) 
     &                    'Ztop,Zbotm,IR,IC,Rwnode,CNode,PNode:'
                        write(iout,*) 
     &                    Ztop,Zbotm,IR,IC,Rwnode,CNode,PNode
                        BNode=B            
                      END IF
                    ENDIF
c                 else B<0
                  ELSE
                    IF(C.GE.0.0) THEN
                      IF(P.GE.0.0) THEN
                        READ(in,*) Ztop,Zbotm,IR,IC,Rwnode,BNode
                        write(iout,*) 'Ztop,Zbotm,IR,IC,Rwnode,BNode:'
                        write(iout,*) Ztop,Zbotm,IR,IC,Rwnode,BNode
                        CNode=C
                        PNode=P
                      ELSE
                        READ(in,*) Ztop,Zbotm,IR,IC,Rwnode,BNode,PNode
                        write(iout,*) 
     &                    'Ztop,Zbotm,IR,IC,Rwnode,BNode,PNode:'
                        write(iout,*) 
     &                    Ztop,Zbotm,IR,IC,Rwnode,BNode,PNode
                        CNode=C
                      END IF
c                   else C<0
                    ELSE             
                      IF(P.GE.0.0) THEN
                        READ(in,*) Ztop,Zbotm,IR,IC,Rwnode,BNode,CNode
                        write(iout,*) 
     &                    'Ztop,Zbotm,IR,IC,Rwnode,BNode,CNode:'
                        write(iout,*) 
     &                    Ztop,Zbotm,IR,IC,Rwnode,BNode,CNode
                        PNode=P
                      ELSE
                        READ(in,*) Ztop,Zbotm,IR,IC,Rwnode,BNode,CNode,
     &                    PNode
                        write(iout,*) 
     &                    'Ztop,Zbotm,IR,IC,Rwnode,BNode,CNode,PNode:'
                        write(iout,*) 
     &                    Ztop,Zbotm,IR,IC,Rwnode,BNode,CNode,PNode
                      END IF
                    ENDIF
                  ENDIF
                END IF
c     Set vars for interval
                MNWINT(5,INTNUM+IINT-1)=RwNode             
                MNWINT(8,INTNUM+IINT-1)=BNode             
                MNWINT(9,INTNUM+IINT-1)=CNode            
                MNWINT(10,INTNUM+IINT-1)=PNode             
c
c     LOSSTYPE=SPECIFYcwc, read Ztop,Zbotm,IR,IC,{CWC}
              CASE (4)
                IF(CWC.GT.0.0) THEN            
                  READ(in,*) Ztop,Zbotm,IR,IC
                  write(iout,*) 'Ztop,Zbotm,IR,IC:'
                  write(iout,*) Ztop,Zbotm,IR,IC
                  CWCNode=CWC         
                ELSE
                  READ(in,*) Ztop,Zbotm,IR,IC,CWCNode
                  write(iout,*) 'Ztop,Zbotm,IR,IC,CWCNode:'
                  write(iout,*) Ztop,Zbotm,IR,IC,CWCNode
                END IF
c     Set var for interval
                MNWINT(11,INTNUM+IINT-1)=CWCNode             
            END SELECT
c     Set vars for interval
            MNWINT(1,INTNUM+IINT-1)=Ztop            
            MNWINT(2,INTNUM+IINT-1)=Zbotm            
            MNWINT(3,INTNUM+IINT-1)=IR            
            MNWINT(4,INTNUM+IINT-1)=IC            
c     Do error-checking on interval elevations
c     If beyond the first interval, do check on elevation
            IF(IINT.GT.1) THEN
              IF(Ztop.GT.Zbotmlast) THEN
                WRITE(iout,*) '***ERROR*** Top of interval is ',
     &           'above bottom of last interval'                
                WRITE(iout,*) 'Well: ',WELLID(MNWID)
                STOP 'MNW2 ERROR - Intervals'
	        END IF
	      END IF
c     Save bottom of last interval for above check

            Zbotmlast=Zbotm
c
c     create nodes from interval information
c     MNWNOD will access MNWINT by pointing to the first and last interval
c       that intersects the node
c
c     set node counter for this interval
            nodecount=0
c     save IR and IC to check vs the subsequent nodes
            IF(IINT.EQ.1) THEN
              IRlast=IR
              IClast=IC
            ELSE
              IF((IR.NE.IRlast).OR.(IC.NE.IClast)) THEN
                write(iout,*) '***ERROR*** Row,Col must be constant in',
     &            'a vertical well (NNODES < 0)'
                STOP 'MNW2 ERROR - Vertical'
              END IF
            END IF
c
c     find first layer that the top of this interval penetrates, set = IL
            K=1
c     botm(...k) points to the bottom of layer K
            DO WHILE (Ztop.le.BOTM(IC,IR,LBOTM(K)))
              K=K+1
            END DO
            IF(K.LE.NLAY) then
              IL=K
            ELSE
              write(iout,*) '***ERROR*** MNW: ',
     &                      'Ztop below bottom of model'
              STOP 'MNW2 ERROR - Ztop'
            END IF
c
c     now that we have coordinates, start creating cells
c
c     if we haven't create any cells, create the first
            IF(intnodes.eq.0) THEN
              IF(IBOUND(IC,IR,IL).NE.0) THEN
c     calculate first node number in nodal list= (total nodes so far) +1
c     this will be used to access the information in the nodal array (MNWNOD)
                NODNUM=ntotnod+1
c     increase total node count 
                ntotnod=ntotnod+1          
          if(ntotnod.gt.nodtot) then
            write(iout,*)
            write(iout,*) 'MNW2 NODE ARRAY ALLOCATION INSUFFICIENT'
            STOP 'MNW2 ALLOCATION ERROR'
          end if
c     increase count of nodes in this interval
                nodecount=nodecount+1   
c     increase count of nodes in this well
                intnodes=intnodes+1   
c     mark this as the first node in the node list for this well
                if(intnodes.eq.1) MNW2(4,MNWID)=NODNUM
c     create node in node list from interval coordinates
                MNWNOD(1,NODNUM)=IL             
                MNWNOD(2,NODNUM)=IR            
                MNWNOD(3,NODNUM)=IC           
                MNWNOD(4,NODNUM)=0.0           
c     set first and last intervals in this node= interval #1
c     INTNUM is location in int list of 1st interval for this well
c     IINT is loop counter for intervals
                MNWNOD(12,NODNUM)=INTNUM+IINT-1
                MNWNOD(13,NODNUM)=INTNUM+IINT-1
c     if partial penetration ne 0, set ZPD to 1d30.  this will act as a flag
c     until ZDP (and ZPL) are set for the well)
                MNWNOD(20,NODNUM)=1d30         
              ELSE
                WRITE(iout,*) '***ERROR*** MNW2 screen in no-flow node'
                STOP 'MNW2 - screen in no-flow node' 
              END IF
c     if a node has been created (nodecount>0), then check to see if this interval
c     is still in that node (still NODNUM)
            ELSE
              IF(MNWNOD(1,NODNUM).EQ.IL) THEN
c     if interval is still in previous node, re-set "last int" for that node
c     do not increase nodecount, as it is in same node
                MNWNOD(13,NODNUM)=INTNUM+IINT-1
C--LFK
                NDALT=1
c     if top of this interval is in a new node, create a node in that layer
              ELSE
                IF(IBOUND(IC,IR,IL).NE.0) THEN
                  NODNUM=NODNUM+1   
c     increase total node count 
                  ntotnod=ntotnod+1          
          if(ntotnod.gt.nodtot) then
            write(iout,*)
            write(iout,*) 'MNW2 NODE ARRAY ALLOCATION INSUFFICIENT'
            STOP 'MNW2 ALLOCATION ERROR'
          end if
c     increase count of nodes in this interval
                  nodecount=nodecount+1   
c     increase count of nodes in this well
                  intnodes=intnodes+1   
                  MNWNOD(1,NODNUM)=IL             
                  MNWNOD(2,NODNUM)=IR            
                  MNWNOD(3,NODNUM)=IC           
                  MNWNOD(4,NODNUM)=0.0           
c     set first and last intervals in this node= interval #1
c     INTNUM is location in int list of 1st interval for this well
c     IINT is loop counter for intervals
                  MNWNOD(12,NODNUM)=INTNUM+IINT-1
                  MNWNOD(13,NODNUM)=INTNUM+IINT-1
c     if partial penetration ne 0, set ZPD to 1d30.  this will act as a flag
c     until ZDP (and ZPL) are set for the well)
                  MNWNOD(20,NODNUM)=1d30         
                ELSE
                 WRITE(iout,*) '***ERROR*** MNW2 screen in no-flow node'
                 STOP 'MNW2 - screen in no-flow node' 
                END IF
	        END IF
            END IF
c
c     check nodecount here before looping over lower layers, possibility that the
c     interval defines nothing is then caught
C--LFK
C            IF(nodecount.gt.0) THEN
            IF(nodecount.gt.0.OR.NDALT.GT.0) THEN
c     add more nodes if the bottom of the interval penetrates below this layer,
c     as long as it isn't the last layer
C--LFK
              NDALT=0
              K=IL
              DO WHILE(Zbotm.lt.BOTM(IC,IR,LBOTM(K)).and.
     &               ((K+1).LE.NLAY))                  
                K=K+1
                IL=K
			    NODNUM=NODNUM+1
c     increase total node count 
                ntotnod=ntotnod+1          
          if(ntotnod.gt.nodtot) then
            write(iout,*)
            write(iout,*) 'MNW2 NODE ARRAY ALLOCATION INSUFFICIENT'
            STOP 'MNW2 ALLOCATION ERROR'
          end if
c     increase count of nodes in this interval
                nodecount=nodecount+1   
c     increase count of nodes in this well
                intnodes=intnodes+1   
                MNWNOD(1,NODNUM)=IL             
                MNWNOD(2,NODNUM)=IR            
                MNWNOD(3,NODNUM)=IC           
                MNWNOD(4,NODNUM)=0.0           
c     set first and last intervals in this node= interval number
                MNWNOD(12,NODNUM)=INTNUM+IINT-1
                MNWNOD(13,NODNUM)=INTNUM+IINT-1
c     if partial penetration ne 0, set ZPD (MNWNOD(20) to 1d30.  this will act as a flag
c     until ZDP (and ZPL) are set for the well
                MNWNOD(20,NODNUM)=1d30         
              END DO
	      END IF
c

    4     CONTINUE
c         end loop over intervals
c     reset NUMNODES to -(number of nodes) instead of -(# of intervals)
            MNW2(2,MNWID)=-1*intnodes
c
c     Print interval information
          if(MNWPRNT.gt.0) then
!            write(iout,*)
c     write info line
!            write(iout,'(100A)') ' NNODES < 0: well defined using open',
!     &' intervals as described below'
c     write header depending on LOSSTYPE
!            SELECT CASE (INT(MNW2(3,MNWID)))
c     LOSSTYPE=NONE, read Ztop,Zbotm,IR,IC only
!              CASE (0)
!            write(iout,'(100A)') ' Interval      Ztop       Zbotm    ',
!     &'  Row  Col'
c     LOSSTYPE=THIEM, read Ztop,Zbotm,IR,IC,{Rw}
!              CASE (1)
!            write(iout,'(100A)') ' Interval      Ztop       Zbotm    ',
!     &'  Row  Col      Rw     '

c     LOSSTYPE=SKIN, read Ztop,Zbotm,IR,IC,{Rw Rskin Kskin}
!              CASE (2)
!            write(iout,'(100A)') ' Interval      Ztop       Zbotm    ',
!     &'  Row  Col      Rw     Rskin    ',
!     &' Kskin '

c     LOSSTYPE=GENERAL, read Ztop,Zbotm,IR,IC,{Rw B C P}
!              CASE (3)
!            write(iout,'(100A)') ' Interval      Ztop       Zbotm    ',
!     &'  Row  Col      Rw     B         C         P  '

c     LOSSTYPE=SPECIFYcwc, read Ztop,Zbotm,IR,IC,{CWC}
!              CASE (4)
!            write(iout,'(100A)') ' Interval      Ztop       Zbotm    ',
!     &'  Row  Col      spec.CWC'
!            END SELECT
c
c     get first interval in this well
            INTNUM=MNW2(13,MNWID)
c     write data depending on LOSSTYPE
!            SELECT CASE (INT(MNW2(3,MNWID)))
c     LOSSTYPE=NONE, write Ztop,Zbotm,IR,IC only
!              CASE (0)
!            do IINT=INTNUM,(INTNUM+NINTVL-1)
!            write(iout,'(1x,I4,6x,1P2G12.5,1x,I4,1x,I4)')
!     &IINT-intnum+1,(MNWINT(j,iint),j=1,2),(INT(MNWINT(j,iint)),j=3,4)
!            end do
c     LOSSTYPE=THIEM, write Ztop,Zbotm,IR,IC,{Rw}
!              CASE (1)
!            do IINT=INTNUM,(INTNUM+NINTVL-1)
!            write(iout,'(1x,I4,6x,1P2G12.5,1x,I4,1x,I4,2x,1P1G10.4)')
!     &IINT-intnum+1,(MNWINT(j,iint),j=1,2),(INT(MNWINT(j,iint)),j=3,4),
!     &(MNWINT(5,iint))
!            end do
c     LOSSTYPE=SKIN, write Ztop,Zbotm,IR,IC,{Rw Rskin Kskin}
!              CASE (2)
!            do IINT=INTNUM,(INTNUM+NINTVL-1)
!            write(iout,'(1x,I4,6x,1P2G12.5,1x,I4,1x,I4,2x,1P3G10.4)')
!     &IINT-intnum+1,(MNWINT(j,iint),j=1,2),(INT(MNWINT(j,iint)),j=3,4),
!     &(MNWINT(j,iint),j=5,7)
!            end do
c     LOSSTYPE=GENERAL, write Ztop,Zbotm,IR,IC,{Rw B C P}
!              CASE (3)
!            do IINT=INTNUM,(INTNUM+NINTVL-1)
!            write(iout,'(1x,I4,6x,1P2G12.5,1x,I4,1x,I4,2x,1P4G10.4)')
!     &IINT-intnum+1,(MNWINT(j,iint),j=1,2),(INT(MNWINT(j,iint)),j=3,4),
!     &(MNWINT(5,iint)),(MNWINT(j,iint),j=8,10)
!            end do
c     LOSSTYPE=SPECIFYcwc, write Ztop,Zbotm,IR,IC,{CWC}
!              CASE (4)
!            do IINT=INTNUM,(INTNUM+NINTVL-1)
!            write(iout,'(1x,I4,6x,1P2G12.5,1x,I4,1x,I4,2x,1P1G10.4)')
!     &IINT-intnum+1,(MNWINT(j,iint),j=1,2),(INT(MNWINT(j,iint)),j=3,4),
!     &(MNWINT(11,iint))
!            end do
!            END SELECT
          end if
c
        END IF
c       end read Data Set 2d
c
c     loop over wells to print out Node Info
c
      write(iout,*)
c
      if(MNWPRNT.gt.0) then
        firstnode=MNW2(4,MNWID)
        lastnode=MNW2(4,MNWID)+ABS(MNW2(2,MNWID))-1
c     write info line
        if(nnodes.lt.0) then
c     for MNWs defined by intervals
!         write(iout,'(100A)') ' The following',
!     &' nodes were assigned to this well based on above open interval',
!     &' information'
!         write(iout,'(100A)') ' Node  Lay  Row  Col '
         do INODE=firstnode,lastnode
          nod=INODE-firstnode+1
c  if more than one interval made up this node, write composite 
          if(MNWNOD(12,INODE).ne.MNWNOD(13,INODE)) then

!           write(iout,'(1x,I4,1x,I4,1x,I4,1x,I4,2x,9A)')
!     &nod,(INT(MNWNOD(i,INODE)),i=1,3),
!     & 'COMPOSITE'

          else
!           write(iout,'(1x,I4,1x,I4,1x,I4,1x,I4)')
!     &nod,(INT(MNWNOD(i,INODE)),i=1,3)       
          end if
         end do
c
        else
c     for MNWs defined by nodes
c
      if(PPFLAG.EQ.0) then
c     write header depending on LOSSTYPE
!            SELECT CASE (INT(MNW2(3,MNWID)))
c     LOSSTYPE=NONE, read IL,IR,IC only
!              CASE (0)
!      write(iout,'(100A)') ' Node  Lay  Row  Col'
c     LOSSTYPE=THIEM, read IL,IR,IC,{Rw}
!              CASE (1)
!      write(iout,'(100A)') ' Node  Lay  Row  Col      Rw     '
c     LOSSTYPE=SKIN, read IL,IR,IC,{Rw Rskin Kskin}
!              CASE (2)
!      write(iout,'(100A)') ' Node  Lay  Row  Col      Rw     Rskin    ',
!     &' Kskin'
c     LOSSTYPE=GENERAL, read IL,IR,IC,{Rw B C P}
!              CASE (3)
!      write(iout,'(100A)') ' Node  Lay  Row  Col      Rw     B
!     &         C          P  '
c     LOSSTYPE=SPECIFYcwc, read IL,IR,IC,{CWC}
!              CASE (4)
!      write(iout,'(100A)') ' Node  Lay  Row  Col    spec.CWC'
!            END SELECT
c
c     write data depending on LOSSTYPE
!            SELECT CASE (INT(MNW2(3,MNWID)))
c     LOSSTYPE=NONE, write IL,IR,IC only
!              CASE (0)
!         do INODE=firstnode,lastnode
!          nod=INODE-firstnode+1
!          write(iout,'(1x,I4,1x,I4,1x,I4,1x,I4)')
!     &nod,(INT(MNWNOD(i,INODE)),i=1,3)       
!         end do
c     LOSSTYPE=THIEM, write IL,IR,IC,{Rw}
!              CASE (1)
!         do INODE=firstnode,lastnode
!          nod=INODE-firstnode+1
!          write(iout,'(1x,I4,1x,I4,1x,I4,1x,I4,2x,1P1G10.4)')
!     &nod,(INT(MNWNOD(i,INODE)),i=1,3),(MNWNOD(5,INODE))       
!         end do
c     LOSSTYPE=SKIN, write IL,IR,IC,{Rw Rskin Kskin}
!              CASE (2)
 !        do INODE=firstnode,lastnode
 !         nod=INODE-firstnode+1
 !         write(iout,'(1x,I4,1x,I4,1x,I4,1x,I4,2x,1P3G10.4)')
 !    &nod,(INT(MNWNOD(i,INODE)),i=1,3),(MNWNOD(j,INODE),j=5,7)       
 !        end do
c     LOSSTYPE=GENERAL, write IL,IR,IC,{Rw B C P}
!              CASE (3)
!         do INODE=firstnode,lastnode
!          nod=INODE-firstnode+1
!          write(iout,'(1x,I4,1x,I4,1x,I4,1x,I4,2x,1P4G10.4)')
!     &nod,(INT(MNWNOD(i,INODE)),i=1,3),(MNWNOD(5,INODE)),
!     &(MNWNOD(j,INODE),j=8,10)       
!         end do
c     LOSSTYPE=SPECIFYcwc, write IL,IR,IC,{CWC}
!              CASE (4)
!         do INODE=firstnode,lastnode
!          nod=INODE-firstnode+1
!          write(iout,'(1x,I4,1x,I4,1x,I4,1x,I4,2x,1P1G10.4)')
!     &nod,(INT(MNWNOD(i,INODE)),i=1,3),(MNWNOD(11,INODE))       
!         end do
!            END SELECT
c     If PPFLAG>0 print PP input
      else
c     write header depending on LOSSTYPE
!            SELECT CASE (INT(MNW2(3,MNWID)))
c     LOSSTYPE=NONE, read IL,IR,IC only
!              CASE (0)
!      write(iout,'(100A)') ' Node  Lay  Row  Col        PP'
c     LOSSTYPE=THIEM, read IL,IR,IC,{Rw}
!              CASE (1)
!      write(iout,'(100A)') ' Node  Lay  Row  Col      Rw        PP'
c     LOSSTYPE=SKIN, read IL,IR,IC,{Rw Rskin Kskin}
!              CASE (2)
!      write(iout,'(100A)') ' Node  Lay  Row  Col      Rw     Rskin    ',
!     &' Kskin        PP'
c     LOSSTYPE=GENERAL, read IL,IR,IC,{Rw B C P}
!              CASE (3)
!      write(iout,'(100A)') ' Node  Lay  Row  Col      Rw     B
!     &         C          P        PP'
c     LOSSTYPE=SPECIFYcwc, read IL,IR,IC,{CWC}
!              CASE (4)
!      write(iout,'(100A)') ' Node  Lay  Row  Col    spec.CWC        PP'
!            END SELECT
c
c     write data depending on LOSSTYPE
!            SELECT CASE (INT(MNW2(3,MNWID)))
c     LOSSTYPE=NONE, write IL,IR,IC only
!              CASE (0)
!         do INODE=firstnode,lastnode
!          nod=INODE-firstnode+1
!          write(iout,'(1x,I4,1x,I4,1x,I4,1x,I4,G10.3)')
!     &nod,(INT(MNWNOD(i,INODE)),i=1,3),MNWNOD(19,INODE)       
!         end do
c     LOSSTYPE=THIEM, write IL,IR,IC,{Rw}
!              CASE (1)
!         do INODE=firstnode,lastnode
!          nod=INODE-firstnode+1
!          write(iout,'(1x,I4,1x,I4,1x,I4,1x,I4,2x,1P2G10.3)')
!     &nod,(INT(MNWNOD(i,INODE)),i=1,3),MNWNOD(5,INODE),
!     & MNWNOD(19,INODE)       
!         end do
c     LOSSTYPE=SKIN, write IL,IR,IC,{Rw Rskin Kskin}
!              CASE (2)
!         do INODE=firstnode,lastnode
!          nod=INODE-firstnode+1
!          write(iout,'(1x,I4,1x,I4,1x,I4,1x,I4,2x,1P4G10.4)')
!     &nod,(INT(MNWNOD(i,INODE)),i=1,3),(MNWNOD(j,INODE),j=5,7),
!     & MNWNOD(19,INODE)      
!         end do
c     LOSSTYPE=GENERAL, write IL,IR,IC,{Rw B C P}
!              CASE (3)
!         do INODE=firstnode,lastnode
!          nod=INODE-firstnode+1
!          write(iout,'(1x,I4,1x,I4,1x,I4,1x,I4,2x,1P5G10.4)')
!     &nod,(INT(MNWNOD(i,INODE)),i=1,3),(MNWNOD(5,INODE)),
!     &(MNWNOD(j,INODE),j=8,10),MNWNOD(19,INODE)       
!         end do
c     LOSSTYPE=SPECIFYcwc, write IL,IR,IC,{CWC}
!              CASE (4)
!         do INODE=firstnode,lastnode
!          nod=INODE-firstnode+1
!          write(iout,'(1x,I4,1x,I4,1x,I4,1x,I4,2x,1P2G10.4)')
!     &nod,(INT(MNWNOD(i,INODE)),i=1,3),MNWNOD(11,INODE),
!     &MNWNOD(19,INODE)      
!         end do
!            END SELECT
      end if      
        end if
      end if
c
c   check well nodes in grid
c   Loop over nodes in well
      firstnode=MNW2(4,MNWID)
      lastnode=MNW2(4,MNWID)+ABS(MNW2(2,MNWID))-1
      do INODE=firstnode,lastnode
        il=MNWNOD(1,INODE)              
        ir=MNWNOD(2,INODE)              
        ic=MNWNOD(3,INODE) 

        if(il.lt.1.or.il.gt.nlay.or.
     &     ir.lt.1.or.ir.gt.nrow.or.
     &     ic.lt.1.or.ic.gt.ncol) then
          write(iout,*) 
          write(iout,*) 'MNW2 Node not in grid; Layer, Row, Col='
          write(iout,*) il,ir,ic
          STOP 'MNW2 ERROR - Grid'
        end if
      end do 
c
c     read Data Set 2e, PUMPLOC
c
        IF(PUMPLOC.GT.0.0) THEN
c     if PUMPLOC>0, read PUMPLAY,PUMPROW,PUMPCOL 
          READ(in,*) PUMPLAY,PUMPROW,PUMPCOL    
          write(iout,*) 'PUMPLAY,PUMPROW,PUMPCOL:'
          write(iout,*) PUMPLAY,PUMPROW,PUMPCOL    
          MNW2(14,MNWID)=PUMPLAY
          MNW2(15,MNWID)=PUMPROW
          MNW2(16,MNWID)=PUMPCOL

        ELSEIF(PUMPLOC.LT.0) THEN
c     if PUMPLOC<0, read Zpump and calulate PUMPLAY,PUMPROW,PUMPCOL
          READ(in,*) Zpump
          write(iout,*) 'Zpump:'
          write(iout,*) Zpump
c         loop over nodes in this well
          firstnode=MNW2(4,MNWID)
          lastnode=MNW2(4,MNWID)+ABS(MNW2(2,MNWID))-1
          ifound=0
          DO INODE=firstnode,lastnode
            IL=MNWNOD(1,INODE)
            IF(Zpump.LT.BOTM(IC,IR,LBOTM(IL)-1).AND.
     &         Zpump.GT.BOTM(IC,IR,LBOTM(IL))) THEN
              IR=MNWNOD(2,INODE)
              IC=MNWNOD(3,INODE)
              MNW2(14,MNWID)=IL
              MNW2(15,MNWID)=IR
              MNW2(16,MNWID)=IC
              ifound=1
            END IF
          END DO
c     if PUMPLOC not in a node, assume it is at top and print warning
          if(ifound.eq.0) then
            write(iout,*) '***WARNING*** Pump location specified but
     & not found within a MNW2 node'
            write(iout,*) ' Pump assumed to be at top node'
            MNW2(11,MNWID)=0
          end if
	  END IF
c
c     end read Data Set 2e
c
c     read data set 2f (if Qlimit > 0)
c
        IF(Qlimit.GT.0.0) THEN
          READ(in,*) Hlim,QCUT
          BACKSPACE in
          IF(QCUT.NE.0) THEN
            READ(in,*) Hlim,QCUT,Qfrcmn,Qfrcmx
            write(iout,*) 'Hlim,QCUT,Qfrcmn,Qfrcmx:'
            write(iout,*) Hlim,QCUT,Qfrcmn,Qfrcmx
          ELSE
            READ(in,*) Hlim,QCUT
            write(iout,*) 'Hlim,QCUT:'
            write(iout,*) Hlim,QCUT
          END IF
c     write info line
c            write(iout,*) 
!            write(iout,'(100A)') ' Qlimit > 0 : this well will
!     & be constrained'
!            write(iout,'(A,1PG13.5)') '     Hlim = ',Hlim 
!            write(iout,1111) QCUT
! 1111 FORMAT('     QCUT = ',I4)
!           if(QCUT.lt.0) then 
!           write(iout,'(A,1PG13.5,A)') '   Qfrcmn = ',Qfrcmn
!           write(iout,'(A,1PG13.5,A)') '   Qfrcmx = ',Qfrcmx
!           elseif(QCUT.gt.0) then
!            write(iout,'(A,1PG13.5,A)') '   Qfrcmn = ',Qfrcmn, ' L**3/T'
!            write(iout,'(A,1PG13.5,A)') '   Qfrcmx = ',Qfrcmx, ' L**3/T'
!           end if
!            write(iout,*) 
c     process min and max Q's based on QCUT
          MNW2(7,MNWID)=Hlim
          MNW2(8,MNWID)=QCUT
          MNW2(9,MNWID)=Qfrcmn
          MNW2(10,MNWID)=Qfrcmx
c     error check Qfrcmn
C-LFK          if(QCUT.GT.0) then
          if(QCUT.LT.0) then
           if(Qfrcmn.gt.1.d0) then
            write(iout,*) '***ERROR*** Qfrcmn > 1 is out of range'
            STOP 'MNW2 ERROR - Qfrcmn'
           end if
c     error check Qfrcmx
           if(Qfrcmx.gt.1.d0) then
            write(iout,*) '***ERROR*** Qfrcmx > 1 is out of range'
            STOP 'MNW2 ERROR - Qfrcmx'
           end if
	    end if
	  END IF
c
c     end read Data Set 2f
c
c     read Data Sets 2g & 2h, PUMPCAP data
c
        IF(PUMPCAP.GT.0.0) THEN
c     if PUMPCAP>0, read Hlift, LIFTq0, LIFTqdes
          READ(in,*) Hlift,LIFTq0,LIFTqdes,HWtol   
          write(iout,*) 'Hlift,LIFTq0,LIFTqdes,HWtol:'
          write(iout,*) Hlift,LIFTq0,LIFTqdes,HWtol   
          mnw2(23,MNWID)=Hlift
          mnw2(28,MNWID)=HWtol
c    CapTable(WELLID,index,type) where
c      index counts the number of values (PUMPCAP+2)
c      type 1 = Lift values
c      type 2 = Q values
          CapTable(MNWID,1,1)=LIFTq0
          CapTable(MNWID,1,2)=0.d0
          CapTable(MNWID,PUMPCAP+2,1)=abs(LIFTqdes)
c  when we know qdes, set this
c          CapTable(MNWID,PUMPCAP+1,2)=qdes
          DO 382 index=2,PUMPCAP+1
            READ(in,*) Liftn,Qn              
            write(iout,*) 'Liftn,Qn:'
            write(iout,*) Liftn,Qn              
            CapTable(MNWID,index,1)=LIFTn
            CapTable(MNWID,index,2)=abs(Qn)
 382      CONTINUE
	  END IF
c     end read Data Sets 2g & 2h
c
c     check consistency of CapTable
        DO 383 index=2,PUMPCAP
          if(CapTable(MNWID,index,1).GT.CapTable(MNWID,index-1,1)) then
            write(iout,*) '***ERROR*** Lift values in capacity table 
     &must be in descending order'
            STOP 'MNW2 ERROR - CapTable'
          end if
          if(CapTable(MNWID,index,2).LT.CapTable(MNWID,index-1,2)) then
            write(iout,*) '***ERROR*** Q values in capacity table 
     &must be in ascending order'
            STOP 'MNW2 ERROR - CapTable'
          end if
 383    CONTINUE

c
c     end loop over MNWMAX
    1 CONTINUE
c
c     read Data Set 3, ITMP (number of wells or flag saying reuse well data)
c
c  Skip to here unless in first SP
  888 CONTINUE
      READ(in,*) ITMP
      write(iout,*) 'ITMP:'
      write(iout,*) ITMP
      write(iout,*) 
c     return if there are no wells to read ........
      if( itmp .ge. 0 ) then
c     reset # of wells and active well flag
         nmnw2=0
         do iw=1,MNWMAX
           MNW2(1,iw)=0
	   end do
         if(itmp.eq.0) return
      end if    
      if( itmp.lt.0 ) then
c        if itmp less than zero, reuse data. print message and return.
!        write(iout,6)
!    6   format(1h0,'REUSING MNW2 INFORMATION FROM LAST STRESS PERIOD')
        return
      else
c  If itmp > 0, read ITMP wells
c
!       if(itmp.gt.1) then
!        write(iout,*) 'MNW2: ',ITMP, ' active wells in stress period ',
!     & kper
!       else
!        write(iout,*) 'MNW2: ',ITMP, ' active well in stress period ',
!     & kper
!       end if
!        write(iout,*) 
        do iread=1,ITMP
c  read data set 4a
c  read WELLNAME & Qdes and then backspace to check for PUMPCAP
         read(in,*) WELLNAME,qdes
         write(iout,*) 'WELLNAME:'
         write(iout,*) WELLNAME
         backspace(in)
         call UPCASE(WELLNAME)
c  check for existence of well
         ifound=0
         MNWID=0
         do iw=1,MNWMAX
 	      if(WELLID(iw).EQ.WELLNAME) then
              ifound=1
c             set IACTIV=1 to turn well on for this SP
              MNW2(1,iw)=1
              nmnw2=nmnw2+1
              MNWID=iw
C-LFK
!       WRITE(IOUT,*)
!       WRITE(IOUT,*)'WELLNAME = ',WELLNAME
	      end if
	   end do
         if (ifound.eq.0) then
            write(iout,*) '***ERROR*** Well name not found in list'
            STOP 'MNW2 ERROR - WELLID'
         end if
c   continue reading data set 4a        
         PUMPCAP=MNW2(22,MNWID)
         NAUX=NMNWVL-30
         if(PUMPCAP.EQ.0) then
          if(igwtunit.le.0) then
            read(in,*) WELLNAME,Qdes,(MNW2(30+IAUX,MNWID),IAUX=1,NAUX)
            write(iout,*) 
     &        'Qdes,AUX,IAUX=1,NAUX:'
            write(iout,*) 
     &         Qdes,(MNW2(30+IAUX,MNWID),IAUX=1,NAUX)
          else
c-lfk:  Only read Cprime for recharge/injection well (Qdes.gt.0.0)
            if (Qdes.gt.0.0) then
              read(in,*) WELLNAME,Qdes,Cprime,
     &                 (MNW2(30+IAUX,MNWID),IAUX=1,NAUX)
	        Cprime = 0
            else
              read(in,*) WELLNAME,Qdes,
     &                 (MNW2(30+IAUX,MNWID),IAUX=1,NAUX)
            end if
            write(iout,*) 'Qdes,Cprime,AUX,IAUX=1,NAUX:'
            write(iout,*) Qdes,Cprime,
     &                 (MNW2(30+IAUX,MNWID),IAUX=1,NAUX)
	      endif
	     else
          if(igwtunit.le.0) then
            read(in,*) WELLNAME,Qdes,CapMult,
     &                 (MNW2(30+IAUX,MNWID),IAUX=1,NAUX)
            write(iout,*) 'Qdes,CapMult,AUX,IAUX=1,NAUX:'
            write(iout,*) Qdes,CapMult,
     &                 (MNW2(30+IAUX,MNWID),IAUX=1,NAUX)
          else
c-lfk:  Only read Cprime for recharge/injection well (Qdes.gt.0.0)
            if (Qdes.gt.0.0) then
              read(in,*) WELLNAME,Qdes,CapMult,Cprime,
     &                 (MNW2(30+IAUX,MNWID),IAUX=1,NAUX)
            else
              read(in,*) WELLNAME,Qdes,CapMult,
     &                 (MNW2(30+IAUX,MNWID),IAUX=1,NAUX)
	        Cprime = 0
            end if
            write(iout,*) 
     &        'Qdes,CapMult,Cprime,AUX,IAUX=1,NAUX:'
            write(iout,*) Qdes,CapMult,Cprime,
     &                 (MNW2(30+IAUX,MNWID),IAUX=1,NAUX)
	      endif
         end if
C-LFK   write auxiliary variable info.
!         if (naux.gt.0.and.mnwprnt.gt.0) then
!            write(iout,1900) (mnwaux(iaux),iaux=1,naux)
!            write(iout,1910) (MNW2(30+IAUX,MNWID),IAUX=1,NAUX)
! 1900 format(2x,'Auxiliary Variable Information:  ',5(:(A16,2x)))
! 1910 format(35x,5(:(f4.0,14x)))
!         end if
c     read Data Set 4b, if Qlimit < 0
c
        Qlimit=MNW2(6,MNWID)
        IF(Qlimit.LT.0.0) THEN
          READ(in,*) Hlim,QCUT
          BACKSPACE in
          IF(QCUT.NE.0) THEN
            READ(in,*) Hlim,QCUT,Qfrcmn,Qfrcmx
            write(iout,*) 'Hlim,QCUT,Qfrcmn,Qfrcmx:'
            write(iout,*) Hlim,QCUT,Qfrcmn,Qfrcmx
          ELSE
            READ(in,*) Hlim,QCUT
            write(iout,*) 'Hlim,QCUT:'
            write(iout,*) Hlim,QCUT
            Qfrcmn=0.0
            Qfrcmx=0.0
          END IF
c     write info line
c            write(iout,*) 
!            write(iout,'(100A)') ' Qlimit < 0 : this well will
!     & be constrained'
!            write(iout,'(A,1PG13.5)') '     Hlim = ',Hlim 
!            write(iout,2111) QCUT
! 2111 FORMAT('     QCUT = ',I4)
!           if(QCUT.lt.0) then 
!           write(iout,'(A,1PG13.5,A)') '   Qfrcmn = ',Qfrcmn
!           write(iout,'(A,1PG13.5,A)') '   Qfrcmx = ',Qfrcmx
!           elseif(QCUT.gt.0) then
!            write(iout,'(A,1PG13.5,A)') '   Qfrcmn = ',Qfrcmn, ' L**3/T'
!            write(iout,'(A,1PG13.5,A)') '   Qfrcmx = ',Qfrcmx, ' L**3/T'
!           end if
!            write(iout,*) 
c     process min and max Q's based on QCUT
          MNW2(7,MNWID)=Hlim
          MNW2(8,MNWID)=QCUT
          if(QCUT.EQ.0) then
            Qfrcmn=0.0
            Qfrcmx=0.0
          end if 
          MNW2(9,MNWID)=Qfrcmn
          MNW2(10,MNWID)=Qfrcmx
c     error check Qfrcmn
          if(QCUT.LT.0) then
           if(Qfrcmn.gt.1.d0) then
            write(iout,*) '***ERROR*** Qfrcmn > 1 is out of range'
            STOP 'MNW2 ERROR - Qfrcmn > 1'
           end if
           if(Qfrcmn.gt.Qfrcmx) then
            write(iout,*) '***ERROR*** Qfrcmn > Qfrcmx '
            STOP 'MNW2 ERROR - Qfrcmn > Qfrcmx'
           end if
c     error check Qfrcmx
           if(Qfrcmx.gt.1.d0) then
            write(iout,*) '***ERROR*** Qfrcmx > 1 is out of range'
            STOP 'MNW2 ERROR - Qfrcmx > 1'
           end if
	    end if
	  END IF
c
c     end read Data Set 4b
c
c        set desired flow rate for well
         MNW2(5,MNWID)=Qdes
c        if transport, set cprime for well
         if(igwtunit.gt.0) MNW2(12,MNWID)=Cprime
c        set all Qact (actual flow rate at node) = Qdes/NNODES as launching point
c        Loop over nodes in well
         firstnode=MNW2(4,MNWID)
c          put total Qdes in first node, allow it to go from there
         MNWNOD(4,firstnode)=Qdes
!         write(iout,2112) WELLNAME,Qdes,kper
! 2112 FORMAT('MNW2 Well ', A20,' active, desired Q =',
!     & 1pe12.4,' for stress period ',I4)
         if(PUMPCAP.GT.0) then
c  if Qdes is not negative for this stress period, do not apply pump capacity restraints
           if(Qdes.GE.0.d0) then
              MNW2(25,MNWID)=0
c   Initialize CapFlag2
              mnw2(27,MNWID)=0
           else
              MNW2(25,MNWID)=1
c  only set Qdes at upper end of CapTable if not set already
             if (CapTable(MNWID,PUMPCAP+2,2).LE.0.d0) then
               CapTable(MNWID,PUMPCAP+2,2)=abs(qdes)
             end if
             MNW2(24,MNWID)=CapMult
!             write(iout,*) 
!             write(iout,1114) mnw2(23,MNWID)
! 1114 FORMAT('Reference head for calculating lift = ', 1pE12.4)
!             write(iout,1113) CapMult
! 1113 FORMAT('Pump Capacity Multiplier = ', 1pE12.4)
!             if(mnwprnt.gt.0) write(iout,1112) mnw2(28,MNWID)
! 1112 FORMAT('HWtol = ', 1pE12.4)
!             if(mnwprnt.gt.1) write(iout,1115) 
! 1115 FORMAT(5x,'(Note: Solution may be sensitive to value of HWtol;',
!     *' adjust value if solution fails to converge.)')
c   zero capacity use flag if CapMult=0
             if( CapMult.eq.0.d0) then
              MNW2(25,MNWID)=0
c   Initialize CapFlag2
              mnw2(27,MNWID)=0
             end if
c  now that we have Qdes, write Capacity table
!             write(iout,*) 
!             write(iout,*) 'Well Capacity Table'
!             write(iout,*) ' Lift     Discharge'
!             do index=1,PUMPCAP+2
!               write(iout,'(1x,1pG10.5,G10.4)') CapTable(MNWID,index,1),
!     &                      CapTable(MNWID,index,2)
!             end do
	     end if
	   end if
   	  end do
      end if
      return
c
      end
C
!      SUBROUTINE GWF2MNW27AD(kstp,kper,IGRID)
C     ******************************************************************
c     Update Qact for wells that were constrained
C     ******************************************************************
!      SUBROUTINE GWF2MNW27FM(KITER,kstp,kper,IGRID)
C     ******************************************************************
C     ADD MNW2 TERMS TO RHS AND HCOF
C     ******************************************************************
c      SUBROUTINE GWF2MNW27BD(kstp,kper,IGRID)
C     ******************************************************************
c     calculate volumetric budget for multi-node wells
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
c
!      SUBROUTINE GWF2MNW27BCF(KPER,IGRID)
C     ******************************************************************
c     ******************************************************************
c     Compute transmissivities used to calculate cell-to-well conductance
c     ******************************************************************
C     Note: BCF, when LAYCON=0 or 2, does not save cell-by-cell
C     Transmissivity (T) values.  Instead, it converts the cell-by-cell
C     T values to branch conductances CR and CC, using harmonic
C     averaging.  When BCF is used, the method used in this routine to
C     generate cell-specific values of Tx and Ty is an approximation
C     based on CR and CC.  When LPF or HUF is used, cell-by-cell
C     hydraulic-conductivity values are stored, this approximation is
C     not needed, and the values generated for Tx and Ty are exact --
C     ERB 1/29/01.
C     ******************************************************************
!      SUBROUTINE GWF2MNW27LPF(KPER,IGRID)
c     ******************************************************************
c     Compute transmissivities used to calculate cell-to-well conductance
c     ******************************************************************
!      SUBROUTINE GWF2MNW27HUF(KPER,IGRID)
c     ******************************************************************
c     Compute transmissivities used to calculate cell-to-well conductance
c     ******************************************************************
!      SUBROUTINE SMNW2COND(IGRID,kstp,kper,kiter,ITFLAG)
C     VERSION 20060704 KJH
c
c----- MNW1 by K.J. Halford
c----- MNW2 by G.Z. Hornberger
c
c     ******************************************************************
c     Calculate all Cell-to-well conductance terms
c     ******************************************************************
c
c
c_________________________________________________________________________________
c
      DOUBLE PRECISION function cel2wel2(LOSSTYPE,Txx,Tyy,dx,dy,
     &                 rw,Rskin,Kskin,B,Cf,PLoss,thck,Q,WELLNAME,Skin)
C
C     VERSION 20030327 KJH        -- Patched Hyd.K term in LPF solution
C     VERSION 20090405 GZH        -- MNW2
c
c----- MNW1 by K.J. Halford
c
c     ******************************************************************
c     Compute conductance term to define head loss from cell to wellbore
c      Methodology is described in full by Peaceman (1983)
c     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT
      IMPLICIT NONE
      CHARACTER*20 WELLNAME
      INTEGER LOSSTYPE
C--LFK
      INTEGER ISEGFLG
C
      DOUBLE PRECISION pi,verysmall,rw,Txx,Tyy,yx4,xy4,ro,dx,dy,Tpi2,A,
     & Ploss,B,Rskin,Kskin,C,Cf,Q,thck,T,Tskin,Skin
C     ------------------------------------------------------------------
C
      pi = 3.1415926535897932D0
      verysmall = 1.D-25
C-LFK
      ISEGFLG=0
C      
      if( rw.lt.verysmall .or. Txx.lt.verysmall .or. Tyy.lt.verysmall ) 
     &  then
        cel2wel2 = ( Txx * Tyy )** 0.5D0
      else
        yx4 = (Tyy/Txx)**0.25D0
        xy4 = (Txx/Tyy)**0.25D0
        ro = 0.28D0 *((yx4*dx)**2 +(xy4*dy)**2)**0.5D0 / (yx4+xy4)
c
        Tpi2 = 2.D0*pi * (Txx*Tyy)**0.5D0
c       if ro/rw is <1, 'A' term will be negative.  Warn user and cut off flow from this node
        if (ro/rw.lt.1.D0) then
!          write(iout,*) 
!     &      '     Ro/Rw =  ',Ro/Rw, 
!     &      '***WARNING*** Ro/Rw < 1, CWC set = 0.0 for well ',WELLNAME
          cel2wel2 = 0.D0
          GOTO 888
        end if
        A = log(ro/rw) / Tpi2
c--LFK--Dec 2012   IF Vert.Segment, Length & Cond. = 1/2 of calc. value.
          IF (SKIN.EQ.-999.0) THEN
            A=A*2.0
            ISEGFLG=1
            SKIN=0.0
          END IF
c
c       For the "NONE" option, multiply the Kh by 1000 to equivalate Hnew and hwell
        if(LOSSTYPE.EQ.0) then
          cel2wel2=1.0D3*((Txx*Tyy)**0.5D0)/thck    
c
c       THEIM option (LOSSTYPE.EQ.1) only needs A, so no need to calculate  B or C
c
c       SKIN (LINEAR) option, calculate B, C=0
        elseif(LOSSTYPE.EQ.2) then
c         average T in aquifer assumed to be sqrt of Txx*Tyy
          T  = (Txx*Tyy)**0.5D0
          Tskin = Kskin*thck
c--LFK--Dec 2012    Check for segment calculation
          IF (ISEGFLG.EQ.1) THEN
            Tskin=Tskin*0.5
          END IF
C
          if(Tskin.gt.0.D0.and.rw.gt.0.D0) then
c         this is from eqs 3 and 5 in orig MNW report
            Skin = ((T/Tskin)-1)*(DLOG(Rskin/rw))
            B = Skin / Tpi2
          else
            B = 0.D0
          end if
          C = 0.D0
c       GENERAL option, calculate B and C
       else if (LOSSTYPE.EQ.3) then
          if(Cf.NE.0.0) then
            C = Cf * abs(Q)**(PLoss-1)
          else
            C = 0.D0
          end if
c--LFK--Dec 2012    Check for segment calculation
          IF (ISEGFLG.EQ.1) THEN
            C = C*2.0
          END IF
C
       else
          B = 0.D0
          C = 0.D0
       end if
        cel2wel2 = A + B + C 
        cel2wel2 = 1.000000D0 / cel2wel2
      endif
c
 888  return
      end
c
c
c
c_________________________________________________________________________________
c
c
c_________________________________________________________________________________
c
      subroutine SMNW2SEEP(IGRID,KPER,KSTP,iw,kiter)
c
c     ******************************************************************
c     determine q
c     ******************************************************************
c
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
     
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,NBOTM,LBOTM,BOTM,
     1                       IBOUND,LAYCBD,HNEW
      USE GWFBASMODULE, ONLY:HDRY
      USE GWFMNW2MODULE, ONLY:NMNW2,MNWMAX,MNWPRNT,
     1                       NODTOT,MNW2,MNWNOD,SMALL,WELLID

      INTEGER firstnode,lastnode,nd
      DOUBLE PRECISION qdes,qact,csum,chsum,Qseep,
     & hwell,hlim,verysmall,hmax,hsim,bottom,qcut,qoff,qon,qsmall,
     & qpot,cond,ratio
C
      verysmall = 1.D-25
C-LFK   initialize MNWNOD(15,IW) TO HIGH VALUE
c       INODE = MNW2(4,IW)
      iseepflg=0
c       MNWNOD(15,INODE) = 1.0D31
      qdes = mnw2(5,iw)
      Qsmall = small*abs(qdes)
         qoff=0.0
         qon=0.0
c
c QFM  rechecking Q cutoff within FM routine
c skip this whole thing if 1st or 2nd iter
      if(kiter.gt.2) then
c   Retrieve Qfrcmn, Qfrcmx, Qdes
         if(mnw2(6,iw).ne.0) then
          QCUT = MNW2(8,iw)
          if(QCUT.ne.0) then
           qoff = mnw2(9,iw)
           qon  = mnw2(10,iw)
           if(QCUT.GT.0) then
c     convert rate into fraction of Qdes (ratio is used to compare)
            if(Qdes.NE.0) then
              qoff=abs(qoff/Qdes)
              qon=abs(qon/Qdes)
c            else
c              qoff=0.0
c              qon=0.0
            end if
           end if
          end if
c
c   Compute hwell / Qpot for multi-node well (not single-cell wells)
c   
          NNODES=abs(MNW2(2,iw))
            csum = 0.000D0
            chsum = 0.000D0
            qact = 0.0000D0
            Qsmall = small*abs(qdes)
c   Loop over nodes in well
            firstnode=MNW2(4,iw)
            lastnode=MNW2(4,iw)+NNODES-1
            hwell = mnw2(17,iw)
            do INODE=firstnode,lastnode
              il=MNWNOD(1,INODE)              
              ir=MNWNOD(2,INODE)              
              ic=MNWNOD(3,INODE)              
              if(IBOUND(ic,ir,il).ne.0) then
                csum  = csum  + MNWNOD(14,INODE)
                chsum = chsum + MNWNOD(14,INODE)*hnew(ic,ir,il)
                qact  = qact  + MNWNOD(4,INODE)
              else
                qact  = 0.0000D0
              end if            
            end do
c---div0 ---  CSUM could go to zero if the entire well is dry
            if( csum .gt. verysmall ) then
              hwell = ( qdes + chsum ) / csum
            else
              hwell = hnew(ic,ir,il)
            endif
c      Test Hlim constraint if QLIMIT flag is set
            if(MNW2(6,iw).NE.0) then
              hlim = mnw2(7,iw)
              ipole = 0
              if( abs(qdes).gt.verysmall ) ipole = qdes / abs(qdes)
              hmax = ipole*( hlim )
              hsim = ipole*( hwell )
c      Potential Q is...
              if( hsim .gt. hmax ) then
                hwell = hlim
              endif
              qpot = hlim*csum - chsum
            end if
            cond = csum
c    
c  Compute ratio of potential/desired flow rates
            if(QCUT.ne.0) then
             ratio = 1.00D0
             if( abs(qdes) .gt. small ) ratio =  qpot / qdes
             if( ratio .gt. 0.9999D0 ) then
              ratio =  1.000D0
              Qpot = Qdes
             endif
c  Check if potential flow rate is below cutoff
c  If so, set Qact = 0.0
c  If q-limit condition is met, do not perform this check on subsequent iterations to
c    avoid oscillations
c  Flag to perform this check is mnw2(20,iw).  If > 0, limit has been met
c  Do not enforce q-limit oscillation-stopper until the 3rd iteration; hardwire=0
c    for first two iterations
             if(kiter.le.2) mnw2(20,iw) = 0
c
c-lfk             Qsmall = small*abs(qdes)
             if( ratio .lt. Qoff . and. mnw2(20,iw) .ge. 0) then
c              mnw2(18,iw) = 0.D0
              mnw2(30,iw) = 0.D0
c  Set flag to avoid rechecking q-limit
              mnw2(20,iw) = -1
c  Check if potential flow rate is above restart threshold
c  If so, set Qact = Qpot
c  If q-limit condition is met, do not perform this check on subsequent iterations to
c    avoid oscillations
c  Flag to perform this check is mnw2(20,iw).  If > 0, limit has been met
             elseif( ratio.gt.Qon .and. abs(qact).lt.Qsmall .and. 
     &              mnw2(20,iw) .le. 0 ) then
c              mnw2(18,iw) = Qpot
              mnw2(30,iw) = Qpot
c  Set flag to avoid rechecking q-limit
              mnw2(20,iw) = 1
             elseif( ratio.lt.Qon.and.ratio.gt.Qoff ) then
              mnw2(20,iw) = 2
             else
c  Otherwise leave the flow rate alone
             endif
            endif
c  End if, QLimit>0
         endif
c  End if, kiter < 3
      end if
c
c
      qdes = mnw2(5,iw)
      qact = qdes
c   If Q in last TS was restricted, update qact
c      if(kstp.gt.1) then
        if((abs(mnw2(18,iw)-qdes).gt.Qsmall))  then
          qact = mnw2(18,iw)
        end if
c      end if
c   If restrictions were set this time step, update qact
       if((mnw2(20,iw).ne.0.or.mnw2(27,iw).ne.0)) then
C-lfk         if((abs(mnw2(29,iw)).lt.abs(mnw2(18,iw)))) then   [subst.30 below]?
         if((abs(mnw2(29,iw)).lt.abs(mnw2(30,iw)))) then
           if(mnw2(27,iw).ne.0) then
             mnw2(18,iw)=mnw2(29,iw)
           else
             mnw2(18,iw)=mnw2(30,iw)
           end if
        else
           if(mnw2(27,iw).eq.0) then
             mnw2(18,iw)=mnw2(30,iw)
           else
             mnw2(18,iw)=mnw2(29,iw)
           end if
        end if
C-LFK
          if(mnw2(27,iw).ne.0.and.mnw2(20,iw).ne.0) then
               if(abs(mnw2(29,iw)).lt.abs(mnw2(30,iw))) then
                 mnw2(18,iw)=mnw2(29,iw)
C--LFK           PUMP-CAPACITY constraint over-rides Hlimit constraint; 
C                   set flag to stop checking Hlim constraint this Time Step 
C                   and use spec. flux bdy. cond.
c-lfk temp hold                 mnw2(20,iw)=-2
                 mnw2(27,iw)=2
               else
                 mnw2(18,iw)=mnw2(30,iw)
               end if
          end if
        qact = mnw2(18,iw)
      end if
c
C-LFK   Skip seepage face calculations for single-node well
          NNODES=abs(MNW2(2,iw))
c--lfk
c      IF (NNODES.EQ.1) GO TO 100
c  Make 2 passes to find seepage faces
      do kSeep = 1, 2          
        csum = 0.000D0
        chsum = 0.000D0
        Qseep = 0.000D0
        firstnode=MNW2(4,iw)
        lastnode=MNW2(4,iw)+ABS(MNW2(2,iw))-1
c  Loop over nodes in well
        do INODE=firstnode,lastnode
            il=MNWNOD(1,INODE)              
            ir=MNWNOD(2,INODE)              
            ic=MNWNOD(3,INODE)              
            nd=INODE-firstnode+1
c  First time through, set the hwell_by_node to big; this will be used as a flag for 
c  whether or not there is a seepage face in the cell
            if( kSeep.eq.1 ) then
              MNWNOD(15,INODE) = 1.0D31
            end if
            if(IBOUND(ic,ir,il).ne.0) then
              Bottom = BOTM(ic,ir,LBOTM(il))
c
c  only allow seepage or flow into a cell if the head in the cell is greater than the bottom of the cell
c
             if(hnew(ic,ir,il).gt.bottom) then
c  Second time through loop, now that we have a guess for hwell, check to see if there is a seepage face
c  If so, use the bottom (instead of the low hwell) as the gradient driver in the qact calc
c  Set the hwell_by_node to the bottom to flag this as a seepage face cell
c  Sum the seepage (there may be more than one node's worth
              if( kSeep.gt.1 .and. hwell.lt.Bottom )then
                MNWNOD(4,INODE) = (bottom - hnew(ic,ir,il))
     & * MNWNOD(14,INODE)
                MNWNOD(15,INODE) = bottom
                Qseep = Qseep + MNWNOD(4,INODE)
              else 
                csum  = csum  + MNWNOD(14,INODE)
                chsum = chsum + MNWNOD(14,INODE)*hnew(ic,ir,il)
              endif
             else
              MNWNOD(4,INODE) = 0.0D0
              MNWNOD(14,INODE) = 0.0D0
              MNWNOD(15,INODE) = hdry
             end if
            else
             MNWNOD(4,INODE) = 0.0D0
             MNWNOD(15,INODE) = hdry
c  write message that inode was deactivated this time step
                if(kseep.gt.1.and.mnwprnt.gt.0) then
      write(iout,210) ND,WELLID(iw)
  210 FORMAT ('Node no. ',I3,'  of Multi-Node Well ', A20)
      write(iout,*) '  deactivated this time step because IBOUND=0'
                end if
            end if
        end do           
c  End loop over nodes in well
c---div0 ---  CSUM could go to verysmall if the entire well is dry
        if( csum .gt. verysmall ) then
          hwell = ( qact - Qseep + chsum ) / csum
        else
          hwell = hnew(ic,ir,il)
        endif
c   Because the q/hwell may now be different due to seepage flow, need to re-check constraints
c   Test Hlim constraints, Hlim is assumed to be a Max/Min for Injection/Production wells
        if(MNW2(6,iw).NE.0) then
          hlim = mnw2(7,iw)
          ipole = 0
          if( abs(qdes).gt.verysmall ) ipole = qdes / abs(qdes)
          hmax = ipole*( hlim )
          hsim = ipole*( hwell )
c
          if( hsim .gt. hmax ) then
            hwell = hlim
            qact = hwell*csum - chsum + Qseep
c      Hlim constraints that stop production are not tested until after the 2nd iteration
            if( kiter.gt.2 ) then
              ratio = 1.00D0
              if( abs(qdes) .gt. small ) ratio =  qact / qdes
              if( ratio .lt. 0.00001D0 ) then
                qact  = 0.000D0
                if (csum .gt. 0.0D0) then
                  hwell = ( qact - Qseep + chsum ) / csum
                else
                  hwell = hnew(ic,ir,il)
                endif
              endif  
            endif   !! potentially stop all production after Kiter = 2 
          endif     !! End Hlim exceedence loop
        endif  !! Qlimit>0
      enddo  !!  kSeep -- End of seepage face corrector here
c
c  Loop over nodes in well, assign flow rates and water levels
c  use qdes to sum q's at end
      qdes=0
      csum=0.0
c-lfk  10/10/2012
      iseepchk=0
      seepchk=0.0
c-lfk
      do INODE=firstnode,lastnode
        il=MNWNOD(1,INODE)              
        ir=MNWNOD(2,INODE)              
        ic=MNWNOD(3,INODE)              
c  Qseep flag (MNWNOD(15): if still 1E31 here, there is no seepage face so use actual hwell to calc Q
c (if not set here, hwell < bottom and MNWNOD(15) stores value of bottom to calculate seepage, see above)
c                           
        csum=csum+mnwnod(14,inode)
        if(MNWNOD(15,INODE) .gt. 1.0E30 )then
          qact = ( hwell - hnew(ic,ir,il) ) * MNWNOD(14,INODE)
          MNWNOD(4,INODE) = qact
          MNWNOD(15,INODE) = hwell
          qdes=qdes+qact
c-lfk  10/10/2012
          iseepchk=1
c--lfk
        endif
c-lfk  10/10/2012
        seepchk=seepchk+mnwnod(4,inode)
c--lfk
      end do           
C-LFK
c-lfk  10/10/2012
!        if (abs(seepchk).lt.abs(MNW2(18,IW)).and.qseep.ne.0.0) 
!     &         LIMQ(3,IW)=1
        if (abs(seepchk).lt.abs(MNW2(18,IW))) iseepflg=1
C   DEACTIVATE WELL IF ALL NODES ARE INACTIVE (Check based on csum=0.0)
c-lfk 2/15/13
c      IF (CSUM.EQ.0.0) THEN
       IF (abs(CSUM).EQ.0.0) THEN
          MNW2(1,IW)=0 
          WRITE (iout,220) WELLID(IW)
  220 FORMAT (/,'Multi-Node Well ',A20,'DEACTIVATED THIS STRESS PERIOD
     * BECAUSE ALL NODES WERE DEACTIVATED'/)
       END IF
C-LFK
C-LFK--comment out single-node processing section below
c      GO TO 105
c  100 CONTINUE
C-LFK   for single-node wells, update values & check constraints
c            INODE=MNW2(4,IW)
c            il=MNWNOD(1,INODE)              
c            ir=MNWNOD(2,INODE)              
c            ic=MNWNOD(3,INODE)              
c            if(IBOUND(ic,ir,il).ne.0) then
c
c              csum  =  MNWNOD(14,INODE)
c              chsum =  MNWNOD(14,INODE)*hnew(ic,ir,il)
c            else
c              MNWNOD(4,INODE) = 0.0D0
c              MNWNOD(14,INODE) = 0.0D0
c              MNWNOD(15,INODE) = hdry
c              csum=0.0
c              chsum=0.0
c            end if
c---div0 ---  CSUM could go to verysmall if the entire well is dry
c        if( csum .gt. verysmall ) then
c          hwell = ( qact - Qseep + chsum ) / csum
c          hwell = ( qact + chsum ) / csum
c        else
c          hwell = hnew(ic,ir,il)
c        endif
c   Test Hlim constraints, Hlim is assumed to be a Max/Min for Injection/Production wells
C-LFK        if(MNW2(6,iw).NE.0) then   !also skip if PUMPCAP overrode Hlim constraint
c        if(MNW2(6,iw).NE.0.and.MNW2(20,IW).GT.-2) then
c          hlim = mnw2(7,iw)
c          ipole = 0
c          if( abs(qdes).gt.verysmall ) ipole = qdes / abs(qdes)
c          hmax = ipole*( hlim )
c          hsim = ipole*( hwell )
c
c          if( hsim .gt. hmax ) then
c            hwell = hlim
c            qact = hwell*csum - chsum 
c      Hlim constraints that stop production are not tested until after the 2nd iteration
c            if( kiter.gt.2 ) then
c              ratio = 1.00D0
c              if( abs(qdes) .gt. small ) ratio =  qact / qdes
c              if( ratio .lt. 0.00001D0 ) then
c                qact  = 0.000D0
c                if (csum .gt. 0.0D0) then
C                  hwell = ( qact - Qseep + chsum ) / csum
c                  hwell = ( qact + chsum ) / csum
c                else
c                  hwell = hnew(ic,ir,il)
c                endif
c              endif  
c            endif   !! potentially stop all production after Kiter = 2 
c          endif     !! End Hlim exceedence loop
c        endif  !! Qlimit>0
c          MNWNOD(4,INODE) = qact
c          MNWNOD(15,INODE) = hwell
c  105 continue
c  Set hwell 
      MNW2(17,iw) = hwell
c                           
c          
      return      
      end
c
c_________________________________________________________________________________
c
c
c_________________________________________________________________________________
c
      SUBROUTINE GWF2MNW27BH(iw,IGRID)
C
c     ******************************************************************
c     compute borehole flow in mnw well 
c     ******************************************************************
c
      USE GLOBAL,       ONLY:IOUT
      USE GWFMNW2MODULE, ONLY:MNWMAX,MNW2,MNWNOD
      integer firstnode,lastnode,PUMPLOC
      double precision Qnet,diff,q
C
      CALL SGWF2MNW2PNT(IGRID)
c   QBH (MNWNOD(27,m) is flow between nodes (saved at upper face) in borehole
c   Only operate on active wells (MNW2(1,iw)=1)
       if (MNW2(1,iw).EQ.1) then
        firstnode=MNW2(4,iw)
        lastnode=MNW2(4,iw)+ABS(MNW2(2,iw))-1
        PUMPLOC=MNW2(11,iw)
c
        if(PUMPLOC.eq.0) then
         nodepump=firstnode
        else
          ifound=0
C-RBW
          findpump: do inode=firstnode,lastnode
C Initialize
            MNWNOD(27,INODE)=0.D0
c get node coordinates
            il=MNWNOD(1,INODE)              
            ir=MNWNOD(2,INODE)              
            ic=MNWNOD(3,INODE)              
c get pump coordinates
            ilp=MNW2(14,iw)              
            irp=MNW2(15,iw)              
            icp=MNW2(16,iw)              
            if(il.eq.ilp.and.ir.eq.irp.and.ic.eq.icp) then
              ifound=1
              nodepump=inode 
              exit findpump
            end if   
          enddo findpump
          if(ifound.eq.0) then
            write(iout,*) '***ERROR*** Pump location specified but 
     & not found, MNW2'
            STOP 'MNW2 ERROR - PUMPLOC2'
          end if
        end if
c get qnet
        Qnet=0.D0
        do INODE=firstnode,lastnode
          q=MNWNOD(4,INODE)
          Qnet=Qnet+q
        end do
          
c   Set flux at top of first node equal to Qnet (if pump at top)
c     or set to zero if pump is somewhere else
c
c   Set flux inbetween node 1 and node 2 (saved at top of node 2) equal to flux at 
c     that node and Qnet (if pump at top)
c     or just to flow if pump is somewhere else
        if(nodepump.eq.firstnode) then
          MNWNOD(27,firstnode)=-Qnet
        else
          MNWNOD(27,firstnode)=0.D0
        end if
c
c   Loop over nodes in well
        do INODE=firstnode+1,lastnode
c   Loop over other nodes in this MNW to set QBH
c   QBH between successive nodes is Q at previous node - Q at node
          if(nodepump.eq.inode) then
            MNWNOD(27,INODE)=MNWNOD(27,INODE-1)+MNWNOD(4,INODE-1)-Qnet
          else          
            MNWNOD(27,INODE)=MNWNOD(27,INODE-1)+MNWNOD(4,INODE-1)
          end if
        enddo
       end if
C
      RETURN
C
      END
C
C
C MNW2HORIZ
C
C Process CWC for nonvertical MNWs
C
C     ******************************************************************
C
      SUBROUTINE MNW2HORIZ(IGRID,LOSSTYPE,NNODES,firstnode,lastnode,
     & IW,kstp,kper,ipr,alpha)
C
C     ******************************************************************
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,NBOTM,LBOTM,BOTM,
     1                       IBOUND,LAYCBD,DELR,DELC,LAYHDT,
     2                       HNEW
      USE GWFMNW2MODULE, ONLY:NMNW2,MNWMAX,NMNWVL,IWL2CB,MNWPRNT,
     1                       NODTOT,INTTOT,MNWAUX,MNW2,MNWNOD,MNWINT,
     2                       CapTable,SMALL,WELLID
C     ------------------------------------------------------------------
C
      ALLOCATABLE ivert1(:),ivert2(:),zseg1(:),zseg2(:)
      INTEGER Wel1flag,QSUMflag,BYNDflag
      INTEGER L1,R1,C1,L2,R2,C2,L,R,C,
     & firstnode,lastnode    
      REAL 
     & x1face1,x1face2,y1face1,y1face2,z1face1,z1face2,
     & x1face,y1face,z1face,
     & x2face1,x2face2,y2face1,y2face2,z2face1,z2face2,
     & x2face,y2face,z2face,
     & m,lxf,lyf,lzf,lbf,
     & zwt,ywt,xwt,
     & zi,yi,xi,zi2,yi2,xi2,t1,b1
c-LFK
     & ,za,ya,xa,zb,yb,xb
      DOUBLE PRECISION z1,y1,x1,z2,y2,x2,top1,bot1,top2,bot2,
     & betweennodes,omega_opp,omega,theta_opp,theta_hyp,
     & theta,thck1,thck2,lw,cel2wel2SEG,dx1,dx2,dy1,dy2,
     & cel2wel2,alpha,T,Kh,Kz,Txx1,Tyy1
      DOUBLE PRECISION 
     & Txx,Tyy,rw,Rskin,Kskin,B,Cf,PLoss,Qact,cond1,cond2,cond,Skin
      DOUBLE PRECISION dgr_to_rad,pi
      ALLOCATE(ivert1(NODTOT),ivert2(NODTOT),zseg1(NODTOT),
     & zseg2(NODTOT))
c convert degree trig func modified from http://techpubs.sgi.com
      pi = 3.1415926535897932D0
      dgr_to_rad = (pi/180.D0)
c     compute borehole length and screen orientation
c
c     compute length associated with each section 
c
c     compute CWC for each node
c
c   Initialize flags
      ivert1=0
      ivert2=0
C-LFK
      ZSEG1=0.0
      ZSEG2=0.0
      IMFLG=0
      lxf=0.0
      lyf=0.0
      lzf=0.0
      lbf=0.0
c   Loop over "segments"
        do INODE=firstnode,lastnode-1
         nod=INODE-firstnode+1
c   Initialize flags
         is_intersection=0
c   Define node and next node
         L1=MNWNOD(1,INODE)              
         R1=MNWNOD(2,INODE)              
         C1=MNWNOD(3,INODE)              
         L2=MNWNOD(1,INODE+1)              
         R2=MNWNOD(2,INODE+1)              
         C2=MNWNOD(3,INODE+1)
         dx1=DELR(C1)             
         dx2=DELR(C2)             
         dy1=DELC(R1)             
         dy2=DELC(R2)             
C     convert to real coodinates
         x1=0 
         do C=1,C1-1
           x1=x1+DELR(C)
         end do
         x1=x1+0.5D0*DELR(C1)
         x2=0 
         do C=1,C2-1
           x2=x2+DELR(C)
         end do
         x2=x2+0.5D0*DELR(C2)
         y1=0 
         do R=1,R1-1
           y1=y1+DELC(R)
         end do
         y1=y1+0.5D0*DELC(R1)
         y2=0 
         do R=1,R2-1
           y2=y2+DELC(R)
         end do
         y2=y2+0.5D0*DELC(R2)
          if(LAYHDT(L1).EQ.0) then
c if confined (THICKNESS IS NOT HEAD-DEPENDENT), don't use hnew=top
           top1=BOTM(C1,R1,LBOTM(L1)-1)
          else
           top1 = hnew(C1,R1,L1)
           if(top1.gt.(BOTM(C1,R1,LBOTM(L1)-1)))
     &       top1=BOTM(C1,R1,LBOTM(L1)-1)
          end if
          bot1 = BOTM(C1,R1,LBOTM(L1))
          thck1 = (top1-bot1)/2.d0
          z1 = 0.5D0*(top1+bot1)
c
          if(LAYHDT(L2).EQ.0) then
c if confined (THICKNESS IS NOT HEAD-DEPENDENT), don't use hnew=top
           top2=BOTM(C2,R2,LBOTM(L2)-1)
          else
           top2 = hnew(C2,R2,L2)
           if(top2.gt.(BOTM(C2,R2,LBOTM(L2)-1)))
     &      top2=BOTM(C2,R2,LBOTM(L2)-1)
          end if
          bot2 = BOTM(C2,R2,LBOTM(L2))
          thck2 = (top2-bot2)/2.d0
          z2 = 0.5D0*(top2+bot2)
c   save z coords as we don't want z screen elevations to change for WT cases
c
c--LFK Dec 2012
         if(MNWNOD(26,INODE+1).eq.0.0)THEN
           IMFLG=1
         END IF
         if(kstp.eq.1.and.kper.eq.1.OR.(IMFLG.EQ.1.AND.KSTP.EQ.1)) then
C         if(kstp.eq.1.and.kper.eq.1) then
           IMFLG=0
C
           MNWNOD(26,INODE)=z1
           MNWNOD(26,INODE+1)=z2
         else
           z1=MNWNOD(26,INODE)
           z2=MNWNOD(26,INODE+1)
         end if
c     caculate distance between nodes
      betweennodes=SQRT(((x1-x2)**2)+((y1-y2)**2)+((z1-z2)**2))
c
c
c   estimate length of borehole segments
c
c   in first node, use vertical section up to top or WT for segment 1 
c--LFK
c      if(INODE.eq.1) then
      if(INODE.eq.firstnode) then
c
        MNWNOD(23,INODE)=0.D0
        if(z1.lt.top1) MNWNOD(23,INODE)=top1-z1   
        ivert1(INODE)=1
      end if
c   if this is a vertical segment, define lengths with elevations, skip other calc
      if(x1.eq.x2.and.y1.eq.y2) then   
        if(top1.le.bot1) MNWNOD(24,INODE)=0.D0         
        if(z1.gt.top1) then
          MNWNOD(24,INODE)=top1-bot1  
        else
          MNWNOD(24,INODE)=z1-bot1  
        endif       
        MNWNOD(23,INODE+1)=top2-z2         
c  if blank spaces inbetween, save that length
        if(bot1.ne.top2) MNWNOD(25,INODE)=bot1-top2
        ivert2(INODE)=1
        ivert1(INODE+1)=1
c
c     if not vertical, calculate theta and omega for segment
c
      else        
c
      if(z1.eq.z2) then
C-LFK        omega=0.d0
        omega=90.d0
      else if(z1.gt.z2) then
c-lfk        omega_opp=SQRT(((x1-x2)**2)+((y1-y2)**2))
c-lfk        omega=DASIN((dgr_to_rad * omega_opp)/betweennodes)
        omega=acos((z1-z2)/betweennodes)/dgr_to_rad
      else
        omega=asin((z2-z1)/betweennodes)/dgr_to_rad+90.0
c-lfk        omega=dasind(dabs(z2-z1)/betweennodes)+90.0     
c-lfk
        write(iout,*) 'Note: z2>z1 & distal part of well is shallower.'
      end if
      MNWNOD(28,INODE)=omega
c
      theta_opp=dabs(y2-y1)
      theta_hyp=SQRT(((x1-x2)**2)+((y1-y2)**2))
c-lfk      theta=DASIN((dgr_to_rad * theta_opp)/(dgr_to_rad * theta_hyp))
      theta=ASIN((theta_opp)/(theta_hyp))/dgr_to_rad
c     correct for right quadrant
      if(y2.ge.y1) then
        if(x2.ge.x1) then
          theta=360.D0-theta
        else if (x2.le.x1) then
          theta=270.D0-theta
        end if
      else if (y2.le.y1) then
        if (x2.le.x1) then
          theta=180.D0-theta
        end if        
      end if      
      MNWNOD(29,INODE)=theta
c   define first cell's limits to test for first intersection
c   only for nonvertical sections
          x1face1=x1-0.5D0*DELR(C1)
          x1face2=x1+0.5D0*DELR(C1)
          y1face1=y1-0.5D0*DELC(R1)
          y1face2=y1+0.5D0*DELC(R1)
        z1face1=z1-0.5D0*(BOTM(C1,R1,LBOTM(L1)-1)-BOTM(C1,R1,LBOTM(L1)))
        z1face2=z1+0.5D0*(BOTM(C1,R1,LBOTM(L1)-1)-BOTM(C1,R1,LBOTM(L1)))
c   define possible face of intersection in x direction, first cell
          if(x2.gt.x1) then
            x1face=x1face2
          else if(x2.lt.x1) then
            x1face=x1face1
          else
            x1face=0
          end if           
c   define possible face of intersection in y direction, first cell
          if(y2.gt.y1) then
            y1face=y1face2
          else if(y2.lt.y1) then
            y1face=y1face1
          else
            y1face=0
          end if           
c   define possible face of intersection in z direction, first cell
          if(z2.gt.z1) then
            z1face=z1face2
          else if(z2.lt.z1) then
            z1face=z1face1
          else
            z1face=0
          end if           
c   define second cell's limits to test for last intersection
          x2face1=x2-0.5D0*DELR(C2)
          x2face2=x2+0.5D0*DELR(C2)
          y2face1=y2-0.5D0*DELC(R2)
          y2face2=y2+0.5D0*DELC(R2)
        z2face1=z2-0.5D0*(BOTM(C2,R2,LBOTM(L2)-1)-BOTM(C2,R2,LBOTM(L2)))
        z2face2=z2+0.5D0*(BOTM(C2,R2,LBOTM(L2)-1)-BOTM(C2,R2,LBOTM(L2)))
c   define possible face of intersection in x direction, second cell
          if(x2.gt.x1) then
            x2face=x2face1
          else if(x2.lt.x1) then
            x2face=x2face2
          else
            x2face=0
          end if           
c   define possible face of intersection in y direction, second cell
          if(y2.gt.y1) then
            y2face=y2face1
          else if(y2.lt.y1) then
            y2face=y2face2
          else
            y2face=0
          end if           
c   define possible face of intersection in z direction, second cell
          if(z2.gt.z1) then
            z2face=z2face1
          else if(z2.lt.z1) then
            z2face=z2face2
          else        
            z2face=0
          end if           
c
c   if 1st z-coord is greater than the WT, start from intersection with WT
C-LFK          if(z1.gt.HNEW(C1,R1,L1)) then
          if(z1.gt.HNEW(C1,R1,L1).and.layhdt(L1).NE.0) then
            zwt=HNEW(C1,R1,L1)                
c   at wt face, determine intersection with line segment
c-lfk
            if ((z2-z1).eq.0.0) then
              m=0.0
            else
              m=(zwt-z1)/(z2-z1)
            end if
            xwt=x1+m*(x2-x1)             
            ywt=y1+m*(y2-y1)
c   redefine 1st point
            x1=xwt
            y1=ywt
            z1=zwt
          end if
c   at x face, determine intersection with line segment
c   xi=intersection point for x face
c   m is "slope" in parameterization of 3d line segment,
c     define m for known x (at the face) and then use that m to solve for
c     other coordinates to give point of intersection
c   xi=x1 + (x2-x1)*m
c   xi-x1/(x2-x1)=m  
c
c-lfk Dec. 2012
          is_intersection=0
          idone=0
c
          if(x1face.ne.0) then
c          is_intersection=0
c          idone=0
          m=(x1face-x1)/(x2-x1)
          yi=y1+m*(y2-y1)             
          zi=z1+m*(z2-z1)
          if(yi.ge.y1face1.and.yi.le.y1face2.and.
     &       zi.ge.z1face1.and.zi.le.z1face2) then
c       if x1face intersection point lies within cell, this is exit point
             xi=x1face
             lxf=SQRT(((x1-xi)**2)+((y1-yi)**2)+((z1-zi)**2))     
             MNWNOD(24,INODE)=lxf
             is_intersection=1
          end if
c       if exit point is on boundary with second cell, done with both segments
          if(is_intersection.eq.1) then
            if(x2face.eq.xi) then
              lxf=SQRT(((x2-xi)**2)+((y2-yi)**2)+((z2-zi)**2))         
              MNWNOD(23,INODE+1)=lxf
              idone=1
            end if
          end if
          else
            lxf=0.d0
          end if
c
c   at y face, determine intersection with line segment
          if(y1face.ne.0) then
          m=(y1face-y1)/(y2-y1)
          xi=x1+m*(x2-x1)             
          zi=z1+m*(z2-z1)
          if(xi.ge.x1face1.and.xi.le.x1face2.and.
     &       zi.ge.z1face1.and.zi.le.z1face2) then
c       if yface intersection point lies within cell, this is exit point
             yi=y1face
             lyf=SQRT(((x1-xi)**2)+((y1-yi)**2)+((z1-zi)**2))     
             MNWNOD(24,INODE)=lyf
             is_intersection=1
          end if
c       if exit point is on boundary with second cell, done with both segments
          if(is_intersection.eq.1) then
            if(y2face.eq.yi) then
              lyf=SQRT(((x2-xi)**2)+((y2-yi)**2)+((z2-zi)**2))         
              MNWNOD(23,INODE+1)=lyf
              idone=1
            end if
          end if
          else
            lyf=0.d0
          end if
c
c   at z face, determine intersection with line segment
          if(z1face.ne.0) then
          m=(z1face-z1)/(z2-z1)
          xi=x1+m*(x2-x1)             
          yi=y1+m*(y2-y1)
          if(xi.ge.x1face1.and.xi.le.x1face2.and.
     &       yi.ge.y1face1.and.yi.le.y1face2) then
c       if zface intersection point lies within cell, this is exit point
             zi=z1face
             lzf=SQRT(((x1-xi)**2)+((y1-yi)**2)+((z1-zi)**2))     
             MNWNOD(24,INODE)=lzf             
             is_intersection=1
          end if
c       if exit point is on boundary with second cell, done with both segments
          if(is_intersection.eq.1) then
            if(z2face.eq.zi) then
              lzf=SQRT(((x2-xi)**2)+((y2-yi)**2)+((z2-zi)**2))         
              MNWNOD(23,INODE+1)=lzf
              idone=1
            end if
          end if
          else
            lzf=0.d0
          end if
c   if idone still=0, then there are blank spaces inbetween nodes.  Calculate
c   length of that segment by getting intersection out of last node
          if(idone.eq.0) then
            is_intersection=0
c   at x face, determine intersection with line segment
            if(x2face.ne.0) then
            m=(x2face-x2)/(x2-x1)
            yi2=y2+m*(y2-y1)             
            zi2=z2+m*(z2-z1)
            if(yi2.ge.y2face1.and.yi2.le.y2face2.and.
     &       zi2.ge.z2face1.and.zi2.le.z2face2) then
c       if x2face intersection point lies within cell, this is exit point
             xi2=x2face 
             lxf=SQRT(((x2-xi2)**2)+((y2-yi2)**2)+((z2-zi2)**2))     
             MNWNOD(23,INODE+1)=lxf
             is_intersection=1
            end if
            else
             lxf=0.d0
            end if
c   at y face, determine intersection with line segment
            if(y2face.ne.0) then
            m=(y2face-y2)/(y2-y1)
            xi2=x2+m*(x2-x1)             
            zi2=z2+m*(z2-z1)
            if(xi2.ge.x2face1.and.xi2.le.x2face2.and.
     &       zi2.ge.z2face1.and.zi2.le.z2face2) then
c       if y2face intersection point lies within cell, this is exit point
             yi2=y2face 
             lyf=SQRT(((x2-xi2)**2)+((y2-yi2)**2)+((z2-zi2)**2))     
             MNWNOD(23,INODE+1)=lyf
             is_intersection=1
            end if
            else
             lyf=0.d0
            end if
c   at z face, determine intersection with line segment
            if(z2face.ne.0) then
             m=(z2face-z2)/(z2-z1)
             xi2=x2+m*(x2-x1)             
             yi2=y2+m*(y2-y1)
             if(xi2.ge.x2face1.and.xi2.le.x2face2.and.
     &        yi2.ge.y2face1.and.yi2.le.y2face2) then
c       if z2face intersection point lies within cell, this is exit point
              zi2=z2face 
              lzf=SQRT(((x2-xi2)**2)+((y2-yi2)**2)+((z2-zi2)**2)) 
              MNWNOD(23,INODE+1)=lzf
              is_intersection=1
             end if
            else
             lzf=0.d0
            end if
c  now that we have both node exit intersection points, blank distance is betweem
c  them.  Save in MNWNOD(25) of the first node between them            
c-LFK Dec 2012: correct calc. of length of blank casing
c           lbf=SQRT(((xi-xi2)**2)+((yi-yi2)**2)+((zi-zi2)**2))     
          if (x1.eq.x2) THEN
            xa=x1
            xb=x2
          ELSE 
            IF (xi.eq.xi2) then
              xa=x1face2
              xb=x2face1
            else
              xa=xi
              xb=xi2
            end if
          END IF  
          if (y1.eq.y2) THEN
            ya=y1
            yb=y2
          ELSE 
            IF (yi.eq.yi2) then
              ya=y1face2
              yb=y2face1
            else
              ya=yi
              yb=yi2
            end if
          END IF  
          if (z1.eq.z2) THEN
            za=z1
            zb=z2
          ELSE 
            IF (zi.eq.zi2) then
              za=z1face2
              zb=z2face1
            else
              za=zi
              zb=zi2
            end if
          END IF  
            lbf=SQRT(((xa-xb)**2)+((ya-yb)**2)+((za-zb)**2))     
c-LFK
            MNWNOD(25,INODE)=lbf 
          end if
C-LFK   Set vertical elev. limits for nonvertical segments
       if (ivert1(inode).eq.0) then
         zseg1(inode)=zseg2(inode-1)
       end if
       if (ivert2(inode).eq.0) then
         zseg2(inode)=zi
         zseg1(inode+1)=zi
       end if
c
c   For last segment, continue the line to the exit intersection of the last cell
c   Define possible face of intersection in x direction, second cell
         if(INODE.eq.(lastnode-1)) then
          if(x2.gt.x1) then
            x2face=x2face2
          else if(x2.lt.x1) then
            x2face=x2face1
          else
            x2face=0
          end if           
c   define possible face of intersection in y direction, second cell
          if(y2.gt.y1) then
            y2face=y2face2
          else if(y2.lt.y1) then
            y2face=y2face1
          else
            y2face=0
          end if           
c   define possible face of intersection in z direction, second cell
          if(z2.gt.z1) then
            z2face=z2face2
          else if(z2.lt.z1) then
            z2face=z2face1
          else        
            z2face=0
          end if  
          if(x2face.ne.0) then
          m=(x2face-x2)/(x2-x1)
          yi=y2+m*(y2-y1)             
          zi=z2+m*(z2-z1)
          if(yi.ge.y2face1.and.yi.le.y2face2.and.
     &       zi.ge.z2face1.and.zi.le.z2face2) then
c       if x2face intersection point lies within cell, this is exit point
             xi=x2face
             lxf=SQRT(((x2-xi)**2)+((y2-yi)**2)+((z2-zi)**2))     
             MNWNOD(24,INODE+1)=lxf
          end if
          else
            lxf=0.d0
          end if
c
c   at y face, determine intersection with line segment
          if(y2face.ne.0) then
          m=(y2face-y2)/(y2-y1)
          xi=x2+m*(x2-x1)             
          zi=z2+m*(z2-z1)
          if(xi.ge.x2face1.and.xi.le.x2face2.and.
     &       zi.ge.z2face1.and.zi.le.z2face2) then
c       if yface intersection point lies within cell, this is exit point
             yi=y2face
             lyf=SQRT(((x2-xi)**2)+((y2-yi)**2)+((z2-zi)**2))     
             MNWNOD(24,INODE+1)=lyf
          end if
          else
            lyf=0.d0
          end if
c
c   at z face, determine intersection with line segment
          if(z2face.ne.0) then
          m=(z2face-z2)/(z2-z1)
          xi=x2+m*(x2-x1)             
          yi=y2+m*(y2-y1)
          if(xi.ge.x2face1.and.xi.le.x2face2.and.
     &       yi.ge.y2face1.and.yi.le.y2face2) then
c       if zface intersection point lies within cell, this is exit point
             zi=z2face
             lzf=SQRT(((x2-xi)**2)+((y2-yi)**2)+((z2-zi)**2))     
C-LFK             MNWNOD(24,INODE+1)=lyf             
             MNWNOD(24,INODE+1)=lzf             
          end if
          else
            lzf=0.d0
          end if
C-LFK   Set vertical elev. limit for final nonvertical segment
           if(ivert2(inode+1).eq.0) then
C              write(iout,*) 'ivert(inode+1).eq.0'
              zseg2(inode+1)=zi
           end if
c
        end if
      end if
c     
      lw=MNWNOD(23,INODE)
      if (lw.gt.0.D0) then
          Txx = MNWNOD(16,INODE)
          Tyy = MNWNOD(17,INODE)
          Txx1 = Txx*0.5d0
          Tyy1 = Tyy*0.5d0
          rw  = MNWNOD(5,INODE)
          Rskin = MNWNOD(6,INODE)  
          Kskin = MNWNOD(7,INODE)
          B = MNWNOD(8,INODE)
          Cf = MNWNOD(9,INODE)
          PLoss = MNWNOD(10,INODE)
          Qact = MNWNOD(4,INODE)
c   compute conductance term for segment
          if(ivert1(INODE).eq.0) then
             Kz=MNWNOD(33,INODE)
             cond1 = cel2wel2SEG(lw,theta,omega,LOSSTYPE,
c--LFK
     &         Txx,Tyy,dx1,dy1,rw,Rskin,Kskin,B,Cf,PLoss,thck1*2,Qact,
c    &         Txx,Tyy,dx1,dy1,rw,Rskin,Kskin,B,Cf,PLoss,thck1,Qact,
     &       WELLID(iw),Kz)
c   if a vertical segment, use original function
          else
c-LFK
c-LFK--use 'Skin' as a flag to denote calc. is for a segment
             Skin=-999.0
c
c              cond1 = cel2wel2(LOSSTYPE,Txx1,Tyy1,dx1,dy1,
c     &                 rw,Rskin,Kskin,B,Cf,PLoss,thck1,Qact,
              cond1 = cel2wel2(LOSSTYPE,Txx,Tyy,dx1,dy1,
     &                 rw,Rskin,Kskin,B,Cf,PLoss,thck1*2,Qact,
     &                 WELLID(iw),Skin)
         end if
      else
         cond1=0.D0 
      end if
      MNWNOD(30,INODE)=cond1
c calculate CWC of second segment of node
      lw=MNWNOD(24,INODE)
      if(lw.gt.0D0) then                      
          Txx = MNWNOD(16,INODE)
          Tyy = MNWNOD(17,INODE)
          Txx1 = Txx*0.5d0
          Tyy1 = Tyy*0.5d0
          rw  = MNWNOD(5,INODE)
          Rskin = MNWNOD(6,INODE)  
          Kskin = MNWNOD(7,INODE)
          B = MNWNOD(8,INODE)
          Cf = MNWNOD(9,INODE)
          PLoss = MNWNOD(10,INODE)
          Qact = MNWNOD(4,INODE)
c   compute conductance term for segment
          if(ivert2(INODE).eq.0) then
             Kz=MNWNOD(33,INODE)
c--LFK
             cond2 = cel2wel2SEG(lw,theta,omega,LOSSTYPE,
     &       Txx,Tyy,dx1,dy1,rw,Rskin,Kskin,B,Cf,PLoss,thck1*2,Qact,
C    &         Txx,Tyy,dx1,dy1,rw,Rskin,Kskin,B,Cf,PLoss,thck1,Qact,
     &       WELLID(iw),Kz)
c   if a vertical segment, use original function
          else
c-LFK
c-LFK--use 'Skin' as a flag to denote calc. is for a segment
             Skin=-999.0
c
c            cond2 = cel2wel2(LOSSTYPE,Txx1,Tyy1,dx1,dy1,
c    &                 rw,Rskin,Kskin,B,Cf,PLoss,thck1,Qact,
             cond2 = cel2wel2(LOSSTYPE,Txx,Tyy,dx1,dy1,
     &                 rw,Rskin,Kskin,B,Cf,PLoss,thck1*2,Qact,
     &                 WELLID(iw),Skin)
          end if
      else
         cond2=0.D0 
      end if
      MNWNOD(31,INODE)=cond2
c sum cond for cell to get resultant CWC for node
      cond=cond1+cond2
c     Save conductance of each node
      MNWNOD(14,INODE) = cond
      if(ipr.eq.1) then 
        t1=top1
        b1=bot1
        if (ivert1(inode).eq.0) t1=zseg1(inode)
        if (ivert2(inode).eq.0) b1=zseg2(inode)
c-lfk       write(iout,'(A15,I3,1P6G12.5,9A)') WELLID(iw),nod,cond,
       write(iout,'(A15,I3,1PG12.4,1x,5G12.4,9A)') WELLID(iw),nod,cond,
     & top1,bot1,t1,b1,alpha,'         '
      end if
c
c process last node separately
c
      if(INODE.EQ.lastnode-1) then
c calculate CWC of first segment in node
      lw=MNWNOD(23,INODE+1)
      if (lw.gt.0.D0) then
          Txx = MNWNOD(16,INODE+1)
          Tyy = MNWNOD(17,INODE+1)
          Txx1 = Txx*0.5d0
          Tyy1 = Tyy*0.5d0
          rw  = MNWNOD(5,INODE+1)
          Rskin = MNWNOD(6,INODE+1)  
          Kskin = MNWNOD(7,INODE+1)
          B = MNWNOD(8,INODE+1)
          Cf = MNWNOD(9,INODE+1)
          PLoss = MNWNOD(10,INODE+1)
          Qact = MNWNOD(4,INODE+1)
c   compute conductance term for segment
          if(ivert1(INODE+1).eq.0) then
c--LFK
             Kz=MNWNOD(33,INODE+1)
c             Kz=MNWNOD(33,INODE)
             cond1 = cel2wel2SEG(lw,theta,omega,LOSSTYPE,
c--LFK
     &         Txx,Tyy,dx2,dy2,rw,Rskin,Kskin,B,Cf,PLoss,thck2*2,Qact,
C    &         Txx,Tyy,dx2,dy2,rw,Rskin,Kskin,B,Cf,PLoss,thck2,Qact,
     &         WELLID(iw),Kz)
c   if a vertical segment, use original function
          else
c-LFK
c-LFK--use 'Skin' as a flag to denote calc. is for a segment
             Skin=-999.0
c
c             cond1 = cel2wel2(LOSSTYPE,Txx1,Tyy1,dx2,dy2,
              cond1 = cel2wel2(LOSSTYPE,Txx,Tyy,dx2,dy2,
     &                 rw,Rskin,Kskin,B,Cf,PLoss,thck2,Qact,
     &                 WELLID(iw),Skin)
         end if
      else
         cond1=0.D0 
      end if
      MNWNOD(30,INODE+1)=cond1
c calculate CWC of second segment of node
c it is the same as the other segment in this node
      MNWNOD(24,INODE+1)=MNWNOD(23,INODE+1)
      cond2=cond1
      MNWNOD(31,INODE+1)=cond2
c sum cond for cell to get resultant CWC for node
      cond=cond1+cond2
c     Save conductance of each node
      MNWNOD(14,INODE+1) = cond      
      if(ipr.eq.1) then 
        t1=top2
        b1=bot2
        if (ivert1(inode+1).eq.0) t1=zseg1(inode+1)
        if (ivert2(inode+1).eq.0) b1=zseg2(inode+1)
c-lfk       write(iout,'(A15,I3,1P6G12.5,9A)') WELLID(iw),nod+1,cond,
       write(iout,'(A15,I3,1PG12.4,1x,5G12.4,9A)') WELLID(iw),nod+1,cond
     &, top2,bot2,t1,b1,alpha,'         '
      end if
      end if
c     end loop over "segments"
      end do
c     print segment info
      if(ipr.eq.1) then
       write(iout,*)
       write(iout,*) 'MNW2 Nonvertical Well:   Segment Information for W
     &ell ',WELLID(IW)
       write(iout,'(A)')  'Node   L   R   C   Segment   Length   
     &   DEG.TILT   MAP-ANGLE    CWC-segment'            
       do INODE=firstnode,lastnode
         L=MNWNOD(1,INODE)              
         R=MNWNOD(2,INODE)              
         C=MNWNOD(3,INODE)              
c segment 1
         lw=MNWNOD(23,INODE)
         if(inode.gt.1) then
           omega=MNWNOD(28,INODE-1)
           theta=MNWNOD(29,INODE-1)
         else
           omega=0.d0
           theta=0.d0
         end if
         cond1=MNWNOD(30,INODE)
         write(iout,'(4I4,I8,1pG16.6,1p3G12.5)')
     & INODE,L,R,C,1,lw,omega,theta,cond1
c segment 2
         lw=MNWNOD(24,INODE)
         if(inode.lt.lastnode) then
           omega=MNWNOD(28,INODE)
           theta=MNWNOD(29,INODE)
         else
           omega=MNWNOD(28,INODE-1)
           theta=MNWNOD(29,INODE-1)        
         end if
         cond2=MNWNOD(31,INODE)
         write(iout,'(4I4,I8,1pG16.6,1p3G12.5)') 
     & INODE,L,R,C,2,lw,omega,theta,cond2
c closed casings
         if(MNWNOD(25,INODE).GT.0) then
           write(iout,'(A,1pG16.6)') '   Closed casing length = ',
     & MNWNOD(25,INODE)
         end if
C
       end do
       write(iout,*)
C-LFK   rewrite header for MNW well conductances if segment info was printed
C        (if there are any more MNW wells)
       if (iw.lt.nmnw2) then
        write(iout,'(120A)') '                              M O D E L
     &  L A Y E R     W E L L  S C R E E N   Penetration    SKIN     
     &  CALCULATED'
        write(iout,'(120A)') 'WELLID        Node    CWC*    top elev   
     &bott.elev     top elev   bott.elev    fraction     COEFF.
     &          B'
       end if
c
      end if
c     
      DEALLOCATE(ivert1,ivert2,zseg1,zseg2)
C
      RETURN
C
      END
C
c
c_________________________________________________________________________________
c
      DOUBLE PRECISION function cel2wel2SEG(lw,theta,omega,LOSSTYPE,Txx,
     &  Tyy,dx,dy,rw,Rskin,Kskin,B,Cf,PLoss,thck,Q,WELLNAME,Kz)
c
C
C     VERSION 20030327 KJH        -- Patched Hyd.K term in LPF solution
C     VERSION 20090405 GZH        -- MNW2
c
c----- MNW1 by K.J. Halford
c
c     ******************************************************************
c     Compute conductance term to define head loss from cell to wellbore
c      Methodology is described in full by Peaceman (1983)
c     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT
      IMPLICIT NONE
      INTEGER LOSSTYPE,i
      CHARACTER*20 WELLNAME
      DOUBLE PRECISION pi,verysmall,rw,Txx,Tyy,yx4,xy4,ro,dx,dy,Tpi2,A,
c-lfk     & Ploss,B,Rskin,Kskin,C,Cf,Q,thck,T,Tskin,x1,x2,x3,x4,
     & Ploss,B,Rskin,Kskin,C,Cf,Q,thck,T,Tskin,
     & roz,roy,rox,zx4,xz4,zy4,yz4,Az,Ay,Ax,Tpi2z,Tpi2y,Tpi2x,
     & theta,omega,kz,ky,kx,lw,bz,by,bx,clz,cly,clx,CLi,
     & numerator,denom1,denom2,lwz,lwy,lwx,
     & ct,st,cw,sw,omega0
c convert degree trig func modified from http://techpubs.sgi.com
      DOUBLE PRECISION dgr_to_rad
c      dsind = sin(dgr_to_rad * dgr_argument)
c      dcosd = cos(dgr_to_rad * dgr_argument)
C     ------------------------------------------------------------------
c define parameters
c
C
c-lfk
      if (omega.gt.90.0) then
         omega0=omega
         omega=180.0-omega
      end if
      pi = 3.1415926535897932D0
      dgr_to_rad = (pi/180.D0)
      verysmall = 1.D-25
c
      Kx=Txx/thck
      Ky=Tyy/thck
c    this makes conductance very small
      if( rw.lt.verysmall .or. Txx.lt.verysmall .or. Tyy.lt.verysmall ) 
     &  then
        cel2wel2SEG = ( Txx * Tyy )** 0.5D0
c       For the "NONE" option, multiply the Kh by 1000 to equivalate Hnew and hwell
      else if(LOSSTYPE.EQ.0) then
        cel2wel2SEG=1.0D3*((Kx*Ky)**0.5D0)   
      else 
c
c    define ro (effective radius) for each direction 
        yx4 = (Ky/Kx)**0.25D0
        xy4 = (Kx/Ky)**0.25D0
        roz = 0.28D0 *((yx4*dx)**2 +(xy4*dy)**2)**0.5D0 / (yx4+xy4)
        Tpi2z = 2.D0*pi * thck *(Kx*Ky)**0.5D0
c
        zx4 = (Kz/Kx)**0.25D0
        xz4 = (Kx/Kz)**0.25D0
        roy = 0.28D0 *((zx4*dx)**2 +(xz4*thck)**2)**0.5D0 / (zx4+xz4)
        Tpi2y = 2.D0*pi * dy * (Kx*Kz)**0.5D0
c
        yz4 = (Kz/Ky)**0.25D0
        zy4 = (Ky/Kz)**0.25D0
        rox = 0.28D0 *((yz4*dy)**2 +(zy4*thck)**2)**0.5D0 / (yz4+zy4)
        Tpi2x = 2.D0*pi * dx * (Kz*Ky)**0.5D0
c
c       if ro/rw is <1, 'A' term will be negative.  Warn user and cut off flow from this node
        if (rox/rw.lt.1.D0.or.roy/rw.lt.1.or.roz/rw.lt.1) then
          write(iout,*) 
     &      '     Ro_x/Rw =  ',Rox/Rw, 
     &      '     Ro_y/Rw =  ',Roy/Rw, 
     &      '     Ro_z/Rw =  ',Roz/Rw, 
     &      '***WARNING*** At least one value of Ro/Rw < 1,
     & CWC set = 0.0 for well '
          cel2wel2SEG = 0.D0
          GOTO 888
        end if
        Az = log(roz/rw) / Tpi2z
        Ay = log(roy/rw) / Tpi2y
        Ax = log(rox/rw) / Tpi2x
c
c       THEIM option (LOSSTYPE.EQ.1) only needs A, so no need to calculate  B or C
c
c       SKIN (LINEAR) option, calculate B, C=0
        if(LOSSTYPE.EQ.2) then
c         average T in aquifer assumed to be sqrt of Txx*Tyy
          if(Kskin.gt.0.D0.and.rw.gt.0.D0) then
c         this is from eqs 3 and 5 in orig MNW report
            lwz=thck
            Bz=(thck*(Kx*Ky)**0.5D0/(Kskin*lw)-1)*(DLOG(Rskin/rw))/Tpi2z
            lwy=dy
            By = (dy*(Kx*Kz)**0.5D0/(Kskin*lw)-1)*(DLOG(Rskin/rw))/Tpi2y
            lwx=dx
            Bx = (dx*(Ky*Kz)**0.5D0/(Kskin*lw)-1)*(DLOG(Rskin/rw))/Tpi2x
          else
            Bx = 0.D0
            By = 0.D0
            Bz = 0.D0
          end if
          C = 0.D0
c       NONLINEAR option, calculate B and C
       else if (LOSSTYPE.EQ.3) then
          B = B / Tpi2z 
          if(Cf.NE.0.0) then
            C = Cf * abs(Q)**(PLoss-1)
          else
            C = 0.D0
          end if
       else
          Bx = 0.D0
          By = 0.D0
          Bz = 0.D0
          C = 0.D0
       end if
c       these are per length
       CLz = Az + Bz + C 
       CLz = 1.000000D0 / CLz / thck
       CLy = Ay + By + C 
       CLy = 1.000000D0 / CLy / dy
       CLx = Ax + Bx + C 
       CLx = 1.000000D0 / CLx / dx
c calculate CWC for slanted well (from 2.45b in SUTRA doc)
       numerator=(CLz*CLy*CLx)
c      dsind = sin(dgr_to_rad * dgr_argument)
c      dcosd = cos(dgr_to_rad * dgr_argument)
c-lfk       x1=dcos(dgr_to_rad * theta)
c-lfk       x2=dsin(dgr_to_rad * theta)
c-lfk       x3=dcos(dgr_to_rad * omega)
c-lfk       x4=dsin(dgr_to_rad * omega)
       denom1=CLz*((CLy*(cos(dgr_to_rad * theta)**2))
     &              +CLx*(sin(dgr_to_rad * theta)**2))
     &              *sin(dgr_to_rad * omega)**2
       denom2=CLx*Cly*(cos(dgr_to_rad * omega)**2)
c 
       if((denom1+denom2).eq.0) then
         write(iout,*) '***ERROR*** MNW2 slanted well error'
         STOP 'MNW2 -- slanted well'
       end if
	 CLi=numerator/(denom1+denom2)
       cel2wel2SEG=lw*(numerator/(denom1+denom2))
      endif
c
 888  continue
c-lfk
      if (omega0.gt.90.0) omega=180.0-omega
      end
c
C
C
C MNW2CAPACITY
C
C Compute Qact restrained by pumping capacity
C
C     ******************************************************************
C
!      SUBROUTINE MNW2CAPACITY(IGRID,qactCap,iw)
C
C     ******************************************************************
!      SUBROUTINE PPC(DDPP,ISOLNFLAG,BB,HKR,HKZ,SS,QQ,RW,ZPD,ZPL)
C
C      ********************************************************
C      *                                                      *
C      *                    **** PPC ****                     *
C      *      COMPUTER PROGRAM FOR CALCULATING DRAWDOWN       *
C      *      IN A CONFINED AQUIFER WITH AXIAL-SYMMETRIC      *
C      *           FLOW TO A PARTIALLY PENETRATING,           *
C      *         INFINITESIMAL-DIAMETER PUMPED WELL           *
C      *          VERSION 3.0 CURRENT AS OF 01/25/08          *
C      *From Barlow, P.M., and Moench, A.F., 1999, WTAQ--A    *
C      *     computer program for calculating drawdowns and   *
C      *     estimating hydraulic properties for confined and *
C      *     water-table aquifers: U.S. Geological Survey     *
C      *     Water-Resources Investigations Report 99-4225    *
C      *                                                      *
C      ********************************************************
C
C       ******************************************************
C      *                                                      *
C      *                  SUBROUTINE LINVST                   *
C      *                                                      *
C      *          VERSION 1.0 CURRENT AS OF 10/01/99          *
C      *                                                      *
C       ******************************************************
C
C15-SUBROUTINE LINVST CALCULATES COEFFICIENTS USED FOR THE
C   STEHFEST ALGORITHM
C
!       SUBROUTINE LINVST(V,NS)

C
C
C       ******************************************************
C      *                                                      *
C      *                  SUBROUTINE LTST1                    *
C      *                                                      *
C      *          VERSION 1.0 CURRENT AS OF 10/01/99          *
C      *                                                      *
C       ******************************************************
C
C16-SUBROUTINE LTST1 CALCULATES THE LAPLACE TRANSFORM SOLUTION FOR
C   DRAWDOWN FOR FLOW TO A FULLY PENETRATING WELL OF INFINITESIMAL
C   DIAMETER IN A CONFINED AQUIFER (THEIS SOLUTION).
C
!       SUBROUTINE LTST1(TD,HDT)
C       ******************************************************
C      *                                                      *
C      *                  SUBROUTINE LTST2                    *
C      *                                                      *
C      *          VERSION 1.0 CURRENT AS OF 10/01/99          *
C      *                                                      *
C       ******************************************************
C
C17-SUBROUTINE LTST2 CALCULATES THE LAPLACE TRANSFORM SOLUTION FOR
C   DRAWDOWN FOR FLOW TO A FINITE DIAMETER, PARTIALLY PENETRATING
C   WELL IN A CONFINED AQUIFER (MODIFIED SOLUTION OF DOUGHERTY AND
C   BABU, 1984). DELAYED DRAWDOWN RESPONSE AT OBSERVATION WELLS
C   IS INCLUDED.
C
!       SUBROUTINE LTST2(TD,HD)
C---SPECIFICATIONS
C
C       ******************************************************
C      *                                                      *
C      *                    FUNCTION BESSK0                   *
C      *                                                      *
C      *          VERSION 1.0 CURRENT AS OF 10/01/99          *
C      *                                                      *
C       ******************************************************
C
C19-FUNCTION BESSK0 CALCULATES THE ZERO-ORDER MODIFIED BESSEL FUNCTION
C    OF THE SECOND KIND. SOURCE: PRESS AND OTHERS (1992).
!      DOUBLE PRECISION FUNCTION BESSK0(X)

C       ******************************************************
C      *                                                      *
C      *                    FUNCTION BESSI0                   *
C      *                                                      *
C      *          VERSION 1.0 CURRENT AS OF 10/01/99          *
C      *                                                      *
C       ******************************************************
C
C20-FUNCTION BESSI0 CALCULATES THE ZERO-ORDER MODIFIED BESSEL FUNCTION 
C    OF THE FIRST KIND. SOURCE: PRESS AND OTHERS (1992).
!      DOUBLE PRECISION FUNCTION BESSI0(X)
C       ******************************************************
C      *                                                      *
C      *                    FUNCTION BESSK1                   *
C      *                                                      *
C      *          VERSION 1.0 CURRENT AS OF 10/01/99          *
C      *                                                      *
C       ******************************************************
C
C21-FUNCTION BESSK1 CALCULATES THE FIRST-ORDER MODIFIED BESSEL FUNCTION 
C    OF THE SECOND KIND. SOURCE: PRESS AND OTHERS (1992).
!      DOUBLE PRECISION FUNCTION BESSK1(X)

C
C       ******************************************************
C      *                                                      *
C      *                    FUNCTION BESSI1                   *
C      *                                                      *
C      *          VERSION 1.0 CURRENT AS OF 10/01/99          *
C      *                                                      *
C       ******************************************************
C
C22-FUNCTION BESSI1 CALCULATES THE FIRST-ORDER MODIFIED BESSEL FUNCTION 
C    OF THE FIRST KIND. SOURCE: PRESS AND OTHERS (1992).
!      DOUBLE PRECISION FUNCTION BESSI1(X)
C---SPECIFICATIONS
C
      SUBROUTINE GWF2MNW27DA(IGRID)
C  Deallocate MNW MEMORY
      USE GWFMNW2MODULE
C
        CALL SGWF2MNW2PNT(IGRID)
        DEALLOCATE(NMNW2)
        DEALLOCATE(MNWMAX)
        DEALLOCATE(NMNWVL)
        DEALLOCATE(IWL2CB)
        DEALLOCATE(MNWPRNT)
        DEALLOCATE(NODTOT)
        DEALLOCATE(INTTOT)
        DEALLOCATE(NTOTNOD)
        DEALLOCATE(SMALL)
        DEALLOCATE(WELLID)
        DEALLOCATE(MNWAUX)
        DEALLOCATE(MNW2)
        DEALLOCATE(MNWNOD)
        DEALLOCATE(MNWINT)
        DEALLOCATE(CapTable)
C
      RETURN
      END
      SUBROUTINE SGWF2MNW2PNT(IGRID)
C  Change MNW data to a different grid.
      USE GWFMNW2MODULE
C
        NMNW2=>GWFMNWDAT(IGRID)%NMNW2
        MNWMAX=>GWFMNWDAT(IGRID)%MNWMAX
        NMNWVL=>GWFMNWDAT(IGRID)%NMNWVL
        IWL2CB=>GWFMNWDAT(IGRID)%IWL2CB
        MNWPRNT=>GWFMNWDAT(IGRID)%MNWPRNT
        NODTOT=>GWFMNWDAT(IGRID)%NODTOT
        INTTOT=>GWFMNWDAT(IGRID)%INTTOT
        NTOTNOD=>GWFMNWDAT(IGRID)%NTOTNOD
        SMALL=>GWFMNWDAT(IGRID)%SMALL
        WELLID=>GWFMNWDAT(IGRID)%WELLID
        MNWAUX=>GWFMNWDAT(IGRID)%MNWAUX
        MNW2=>GWFMNWDAT(IGRID)%MNW2
        MNWNOD=>GWFMNWDAT(IGRID)%MNWNOD
        MNWINT=>GWFMNWDAT(IGRID)%MNWINT
        CapTable=>GWFMNWDAT(IGRID)%CapTable
C
      RETURN
      END
      SUBROUTINE SGWF2MNW2PSV(IGRID)
C  Save MNW2 data for a grid.
      USE GWFMNW2MODULE
C
        GWFMNWDAT(IGRID)%NMNW2=>NMNW2
        GWFMNWDAT(IGRID)%MNWMAX=>MNWMAX
        GWFMNWDAT(IGRID)%NMNWVL=>NMNWVL
        GWFMNWDAT(IGRID)%IWL2CB=>IWL2CB
        GWFMNWDAT(IGRID)%MNWPRNT=>MNWPRNT
        GWFMNWDAT(IGRID)%NODTOT=>NODTOT
        GWFMNWDAT(IGRID)%INTTOT=>INTTOT
        GWFMNWDAT(IGRID)%NTOTNOD=>NTOTNOD
        GWFMNWDAT(IGRID)%SMALL=>SMALL
        GWFMNWDAT(IGRID)%WELLID=>WELLID
        GWFMNWDAT(IGRID)%MNWAUX=>MNWAUX
        GWFMNWDAT(IGRID)%MNW2=>MNW2
        GWFMNWDAT(IGRID)%MNWNOD=>MNWNOD
        GWFMNWDAT(IGRID)%MNWINT=>MNWINT
        GWFMNWDAT(IGRID)%CapTable=>CapTable
C
      RETURN
      END
