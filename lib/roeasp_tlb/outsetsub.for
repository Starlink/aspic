      SUBROUTINE OUTSETSUB (CX,CY,DIAM,NEWVAL,IXEXT,IYEXT,OUTPICT)
C+
C  OUTSETSUB
C
C	called from program OUTSET
C	changes pixels outside circle centred at CX,CY diameter DIAM to NEWVAL
C
C  Given (arguments)
C    CX,CY  (R)   centre of circle
C    DIAM   (R)   diameter of circle
C    NEWVAL (R)   new values for outside circle
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
      REAL CX,CY,DIAM,NEWVAL
      REAL RAD,KR
      INTEGER ICX,ICY,I,J

C  arrays start at 1, not 0
      ICX=NINT(CX+1)
      ICY=NINT(CY+1)
      RAD=DIAM/2.0
      DO J=1,IYEXT
        DO I=1,IXEXT
          KR=SQRT(REAL((I-ICX)**2+(J-ICY)**2))
          IF (KR.GT.RAD) OUTPICT(I,J)=NEWVAL
        ENDDO
      ENDDO
      END
