      SUBROUTINE LMEAN(XPOS,YPOS,CSIZ,MEAN,A,NAXIS1,NAXIS2)                     
C+
C   LMEAN
C
C   Calculate mean over area of rectangular cursor
C
C   Given      (arguments)
C   XPOS        X-centre of cursor
C   YPOS        Y-centre of cursor
C   CSIZ        dimensions of rectangular cursor
C   NAXIS1      X-dimension of image
C   NAXIS2      Y-dimension of image
C   A           image array
C
C   Returned   (arguments)
C   XPOS        adopted X-centre of cursor
C   YPOS        adopted Y-centre of cursor
C   MEAN        mean value within cursor
C
C   Subroutine calls :
C   CHKLIM           : E2DlIB
C
C	J.A COOKE/UOE/1981
C-
C
C   If (XPOS,YPOS) is too close to the edge of the image for the
C   given cursor size, then XPOS and YPOS are modified suitably.
C
      REAL MEAN
      INTEGER XPOS,YPOS,CSIZ(2),NAXIS1,NAXIS2,IX,IY,IXA,IYA,BX,BY
      REAL A(NAXIS1,NAXIS2)
C
C     CHECK AND CORRECT LIMITS
C
      CALL CHKLIM(XPOS,YPOS,CSIZ)
      BX=XPOS
      BY=YPOS
C
C     CALCULATE MEAN
C
      MEAN=0.
      DO IY=1,CSIZ(2)
        IYA=BY+IY
        IF(IYA.GT.NAXIS2)IYA=NAXIS2
          DO IX=1,CSIZ(1)
            IXA=BX+IX
            IF(IXA.GT.NAXIS1)IXA=NAXIS1
            MEAN=MEAN+A(IXA,IYA)
          ENDDO
      ENDDO
      MEAN=MEAN/(CSIZ(1)*CSIZ(2))
      RETURN
      END
C
C
C
C
C
C****************************************************************
C****************************************************************
