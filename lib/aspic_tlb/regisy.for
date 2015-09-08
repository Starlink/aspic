      SUBROUTINE REGISY(M,N,XC,RC,IFL)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/WAIT/ WEIGHT(100),IWEIGHT
      COMMON/CAL/ Y1(100),X1(100),X2(100),Y2(100),IPASS,IMAP
      DIMENSION XC(N),RC(M)
      LOGICAL IFL
C IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
C I                                                     I
C I   ****** PLATE REGISTRATION Y-DIRECTION *****       I
C I                                                     I
C IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
      DO 1 I = 1,M
      Y2MY1 = Y2(I)-Y1(I)
      XX2 = X1(I) * X1(I)
      YY2 = Y1(I) * Y1(I)
      RCSIMP = Y2MY1-(XC(1)+XC(2)*X1(I)+XC(3)*Y1(I))
      RC(I) = RCSIMP
      IF(IPASS.EQ.1)GO TO 11
      RC(I) = RC(I)-(XC(4)*XX2+XC(5)*Y1(I)*X1(I)+XC(6)*YY2)
      IF(IPASS.EQ.2) GO TO 11
      RC(I) = RC(I)-(XC(7)*Y1(I)*(XX2+YY2)+XC(8)*Y1(I)
     1*((XX2+YY2)**2))
   11 IF(IWEIGHT.NE.1)GO TO 1
      RC(I) = RC(I) * WEIGHT(I)
    1 CONTINUE
      RETURN
      END
