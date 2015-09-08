      SUBROUTINE DOLINE(X,Y,NUM,KLOOP)
C
C
C
      REAL X(NUM),Y(NUM)
C
C
C
      CALL BREAK
      KK = -2*(KLOOP-1)
      IF (KLOOP.GT.4) KK = 2*(KLOOP-1)
      IF (KLOOP.GT.7) KK = 0
      CALL BRKN CV(X,Y,NUM,KK)
C
C
C
      END
 
 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R GETDAT *
C      *            *
C      **************
C
C
C  This s/r extracts data from two arrays and puts into two others
C
C  AJPENNY               RGO                         83-1-11
C --------------------------------------------------------------
C
C
C
