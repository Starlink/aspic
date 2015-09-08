      SUBROUTINE LINFIXSUB (Y,NEWVAL,IXEXT,IYEXT,OUTPICT)
C+
C  LINFIXSUB
C
C	called from program LINFIX
C	changes line y=cy to NEWVAL
C
C  Given (arguments)
C    Y    (R)   line to be changed
C    NEWVAL (R)   new value for line
C    IXEXT,IYEXT (I)   dims of OUTPICT
C    OUTPICT (RA)   image to be modified
C
C  Returned (arguments)
C    OUTPICT  (RA)  modified image
C
C  D. Brownrigg, D. Tudhope  ROE  1981
C-

      INTEGER IXEXT,IYEXT
      REAL OUTPICT(IXEXT,IYEXT)
      REAL Y,NEWVAL

C  arrays start at 1, not 0
      IY=NINT(Y+1)
      DO I=1,IXEXT
        OUTPICT(I,IY)=NEWVAL
      ENDDO
      END
