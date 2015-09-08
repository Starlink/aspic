      SUBROUTINE SINFILSUB (CX,CY,SPREAD,IXEXT,IYEXT,OUTPICT)
C+
C  SINFILSUB
C
C	called from program SINFIL
C       multiplies fft image by a SINC function
C
C  Given (arguments)
C    CX,CY  (R)   centre of image
C    SPREAD (R)   width for sinc spread
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
      REAL CX,CY,SPREAD
      REAL ARG
      INTEGER ICX,ICY,I,J
      PARAMETER (PI=3.1415962)

C  arrays start at 1, not 0
      ICX=NINT(CX+1)
      ICY=NINT(CY+1)
      DO J=1,IYEXT
        DO I=1,IXEXT
          IF ((I.NE.ICX) .OR. (J.NE.ICY)) THEN
            ARG=(SQRT(REAL((I-ICX)**2+(J-ICY)**2))*PI)/SPREAD
            OUTPICT(I,J)=OUTPICT(I,J)*SIN(ARG)/ARG
          ENDIF
        ENDDO
      ENDDO
      END
