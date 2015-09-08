      SUBROUTINE STKS
*+
*    STKS
*
*    Calculates Stokes parameters given total
*    polarization and polarization angle.
*
*    Subroutines called :
*
*    INPICR,OUTPICR,CLEARIM     : ASPFACE
*    STOKES1                    : E2DASP
*    WRUSER                     : STARLINK
*
*    B.D.Kelly/ROE/1.3.1982
*    D.W.T.Baines/ROE/JAN 1983
*-
      INTEGER NAXT(2),NAXA(2),NAXQ(2),NAXU(2)
      INTEGER NPT,NPA,NPQ,NPU,IST,ISTAT
*
*    set all status return variables to 0
*
      IST = 0
      ISTAT = 0
*
*    request the total polarization and polarization angle frames
*
      CALL INPICR('INPIC2','Give Total Polarization frame',
     :            2,NAXT,NPT,IST)
      CALL INPICR('INPIC3','Give Angle frame',2,NAXA,NPA,IST)
      NAXQ(1)=MIN(NAXT(1),NAXA(1))
      NAXQ(2)=MIN(NAXT(2),NAXA(2))
      NAXU(1)=NAXQ(1)
      NAXU(2)=NAXQ(2)
*
*    request frames to store the Q and U data
*
      CALL OUTPICR('OUTPIC2','Give Q Output frame',2,NAXQ,NPQ,IST)
      CALL OUTPICR('OUTPIC3','Give U Output frame',2,NAXU,NPU,IST)
*
*    check if all I/O frames accessed o.k.
*
      IF(IST.EQ.0) THEN
         CALL STOKES1(NAXT(1),NAXT(2),%VAL(NPT),
     :                NAXA(1),NAXA(2),%VAL(NPA),
     :                NAXQ(1),NAXQ(2),%VAL(NPQ),
     :                NAXU(1),NAXU(2),%VAL(NPU))
      ELSE
*
*       here on an error accessing either the input or output frames
*
         CALL WRUSER ('Error accessing I/O data frames',ISTAT)
      ENDIF
*
*    clear all the frames used
*
      CALL CLEARIM('INPIC2')
      CALL CLEARIM('INPIC3')
      CALL CLEARIM('OUTPIC2')
      CALL CLEARIM('OUTPIC3')
      END
