      SUBROUTINE TRANS(KD,KX,KY,LX,LY,DATA,KXA,KYA,BS,BZ)
C
C
C
      REAL DATA(KXA,KYA)
      INTEGER*2 KD(KX,KY)
C
C
C
      DO K = 1,KYA
         DO J = 1,KXA
            JA = J + LX - 1
            KA = K + LY - 1
            DATA(J,K) = REAL(KD(JA,KA))*BS + BZ
         ENDDO
      ENDDO
C
C
C
      END
 
 
 
