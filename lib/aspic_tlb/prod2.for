C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  This s/r multiplies a row and a column together
C
      SUBROUTINE PROD2(P,JP,KP,NP,MP,A,JA,KA,NA,MA,NS,NE,KR,S)
C
      REAL P(JP,KP),A(JA,KA)
C
      IF(KR.EQ.0) THEN
         S = 0.0
         DO 1 I = NS,NE
            S = S + P(NP,I)*A(I,MA)
    1    CONTINUE
      ELSE
         S = 0.0
         DO 2 I = NS,NE
            S = S + P(I,MP)*A(I,MA)
    2    CONTINUE
      ENDIF
C
      END
