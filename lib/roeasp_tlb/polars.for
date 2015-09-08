      SUBROUTINE POLARS
*+
*    POLARS
*
*    Given Q and U frames, calculates total polarization
*    and polarization angle.
*
*    Subroutines called :
*    INPICR,OUTPICR,YESNO,READR,CLEARIM : ASPFACE
*    POLAR1,PRPLTS                      : E2DASP
*    WRUSER                             : STARLINK
*
*    B.D.Kelly/ROE/29.1.1982
*    D.W.T.BAINES/ROE/March 1983
*-
      INTEGER NAXQ(2),NAXU(2),NAXT(2),NAXA(2),NPLT(2),NAXE(2)
      INTEGER NPQ,NPU,NPT,NPA,NPE,IST,ISTAT,NTOT,NANG
      CHARACTER*10 REPLY
      REAL ANGLE,THETA,VECSCL
      REAL DUMMY(1,1)
*
*    set all status variables to 0
*
      IST = 0
      ISTAT = 0
*
*    request the Q and U frames
*
      CALL INPICR('INPIC2','Give the Q frame',2,NAXQ,NPQ,IST)
      CALL INPICR('INPIC3','Give the U frame',2,NAXU,NPU,IST)
      NAXT(1)=MIN(NAXQ(1),NAXU(1))
      NAXT(2)=MIN(NAXQ(2),NAXU(2))
      NAXA(1)=NAXT(1)
      NAXA(2)=NAXT(2)
*
*    request the output image frames
*
      CALL OUTPICR('OUTPIC2','Give Total Polarization Output frame',
     :             2,NAXT,NPT,IST)
      CALL OUTPICR('OUTPIC3','Give Polarization Angle Output frame',
     :             2,NAXA,NPA,IST)
      CALL READR('ANGLE','Give Instrumental Angle to be added',
     :           0.0,-180.0,180.0,ANGLE,IST)
      THETA=-ANGLE*3.14159/180.0
      NPLT(1)=128
      NPLT(2)=128
      CALL OUTPICR('WORK1','FIRST WORK FRAME',2,NPLT,NTOT,IST)
      CALL OUTPICR('WORK2','SECOND WORK FRAME',2,NPLT,NANG,IST)
      CALL YESNO('Do you want to supply an Error frame, Y or N',
     :           'Y',REPLY,IST)
      NAXE(1)=1
      NAXE(2)=1
      IF(REPLY.EQ.'Y') THEN
         CALL INPICR('INPIC4','Give Error frame',2,NAXE,NPE,IST)
      ELSE
         NPE=%LOC(DUMMY(1,1))
      ENDIF
*
*    if all I/O frames set up O.K. then continue , otherwise end
*
      IF(IST.EQ.0) THEN
         CALL POLAR1(THETA,
     :               NAXQ(1),NAXQ(2),%VAL(NPQ),
     :               NAXU(1),NAXU(2),%VAL(NPU),
     :               NAXT(1),NAXT(2),NAXA(1),NAXA(2),
     :               NAXE(1),NAXE(2),%VAL(NPE),
     :               %VAL(NPT),%VAL(NPA))
*
*       set up limits for the plot
*
         NXST = 1
         NXFN = NAXA(1)
         NYST = 1
         NYFN = NAXA(2)
*
*       perform the plotting
*
         CALL PRPLTS (NXST,NXFN,NYST,NYFN,NAXT,%VAL(NPT),
     :   NAXA,%VAL(NPA),NPLT,%VAL(NTOT),%VAL(NANG))
      ELSE
*
*       here if an error accessing the I/O frames
*
         CALL WRUSER('Error accessing I/O images',ISTAT)
      ENDIF
*
*    clear all the frames used
*
      CALL CLEARIM('INPIC2')
      CALL CLEARIM('INPIC3')
      CALL CLEARIM('INPIC4')
      CALL CLEARIM('OUTPIC2')
      CALL CLEARIM('OUTPIC3')
      CALL CLEARIM('WORK1')
      CALL CLEARIM('WORK2')
      END
