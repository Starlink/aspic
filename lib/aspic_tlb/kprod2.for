C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  This s/r multiplies a row and a column together
C
      SUBROUTINE KPROD2(KP,JK,KK,NK,MK,A,JA,KA,NA,MA,NS,NE,KR,S)
C
      INTEGER KP(JK,KK)
      REAL A(JA,KA)
C
      IF(KR.EQ.0) THEN
         S = 0.0
         DO 1 I = NS,NE
            S = S + REAL(KP(NK,I))*A(I,MA)
    1    CONTINUE
      ELSE
         S = 0.0
         DO 2 I = NS,NE
            S = S + REAL(KP(I,MK))*A(I,MA)
    2    CONTINUE
      ENDIF
C
      END
