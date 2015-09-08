C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C      S/R ZEROI
C
C   Zeroes a INTEGER*2 array
C
C
C     A J PENNY               RGO                     84-04-03
C ---------------------------------------------------------------
C
C
C
      SUBROUTINE ZEROI(IN,NX,NY)
C
C
C
      INTEGER*2 IN(NX,NY)
C
C
C
      DO K = 1,NY
         DO J = 1,NX
            IN(J,K) = 0
         ENDDO
      ENDDO
C
C
C
      END



