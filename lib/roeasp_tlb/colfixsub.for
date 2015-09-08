      SUBROUTINE COLFIXSUB (X,NEWVAL,IXEXT,IYEXT,OUTPICT)
C+
C  COLFIXSUB
C
C	called from program COLFIX
C	changes line x=cx to NEWVAL
C
C  Given (arguments)
C    X    (R)   line to be changed
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
      REAL X,NEWVAL

C  arrays start at 1, not 0
      IX=NINT(X+1)
      DO I=1,IYEXT
        OUTPICT(IX,I)=NEWVAL
      ENDDO
      END
