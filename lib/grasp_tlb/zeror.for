C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C      S/R ZEROR
C
C   Zeroes a REAL*4 array
C
C
C     A J PENNY               RGO                     84-04-03
C ---------------------------------------------------------------
C
C
C
      SUBROUTINE ZEROR(DATA,NX,NY)
C
C
C
      REAL DATA(NX,NY)
C
C
C
      DO K = 1,NY
         DO J = 1,NX
            DATA(J,K) = 0.0
         ENDDO
      ENDDO
C
C
C
      END



