C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R COPYIR *
C      *            *
C      **************
C
C
C   PURPOSE
C
C   ARGUMENTS
C  IN
C  IN/OUT
C  OUT
C
C   STARLINK PARAMETERS
C
C
C
C   CALLS
C     Starlink
C     Aspic
C     Edrs
C     Grasp
C     This file
C
C   USES
C     I*2 arrays
C     %VAL facility
C     Byte arrays
C     Starlink ERRPAR,FMTPAR
C
C
C   A.J.PENNY                   RGO                    83-Z-Z
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE COPYIR(KIN,NXA,NYA,KX1,KX2,KY1,KY2,
     +                  AOUT,NXB,NYB,LX1,LY1)
C
C
C
      INTEGER*2 KIN(NXA,NYA)
      REAL AOUT(NXB,NYB)
C
C
C
      DO K = KY1,KY2
         DO J = KX1,KX2
            VAL = KIN(J,K)
            IF (VAL.GT.32767.0) VAL = 32767.0
            IF (VAL.LT.-32767.0) VAL = -32767.0
            AOUT(LX1+J-KX1,LY1+K-KY1) = VAL
         ENDDO
      ENDDO
C
C
C
      END



