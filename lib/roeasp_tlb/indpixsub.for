      SUBROUTINE INDPIXSUB (X,Y,PIXVAL,IXEXT,IYEXT,OUTPICT)
C+
C  INDPIXSUB
C
C	called from program INDPIX
C	changes pixel at (X,Y) to PIXVAL
C
C  Given (arguments)
C    X,Y  (R)   coords of pixel to be changed
C    PIXVAL (R)   new value for (X,Y)
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
      REAL X,Y,PIXVAL

C  arrays start at 1, not 0
      IX=NINT(X+1)
      IY=NINT(Y+1)
      OUTPICT(IX,IY)=PIXVAL
      END
