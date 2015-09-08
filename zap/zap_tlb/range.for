      SUBROUTINE RANGE(KDATA,KX,KY,LX,LY,MIN,MAX,INVAL,IERR)
      INTEGER*2 KDATA(KX,KY)
      INTEGER LX(2),LY(2)
C
      MIN = 32767
      MAX = -32768
C
      DO J = LX(1),LX(2)
         DO K = LY(1),LY(2)
            L = KDATA(J,K)
            IF(L.NE.INVAL) THEN
               IF(L.LT.MIN) MIN = L
               IF(L.GT.MAX) MAX = L
            ENDIF
         ENDDO
      ENDDO
C
      IERR = 0
      IF (MIN.EQ.32767) IERR = 1
C
      END
 
 
 
 
 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      *************
C      *           *
C      * S/R GETXY *
C      *           *
C      *************
C
C  This subroutine gets the corners of a rectangle
C  from the ARGS Cursor or from the keyboard
C  If from ARGS cursor, it deals with an ICDISP displayed image
C  where the image may be compressed by a factor COMFAC and may
C  only be a window on the whole image (lhb corner pixel is
C  realy pixel DX,DY in main image.
C
C
C ---------------------------------------------------
C
C
