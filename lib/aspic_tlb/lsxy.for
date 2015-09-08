      SUBROUTINE LSXY(M,N,XC,RC,AJTJC,GC)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/CAL/ Y1(100),X1(100),X2(100),Y2(100),IPASS,IMAP
      COMMON /WAIT/ WEIGHT(100),IWEIGHT
      DIMENSION XC(N),RC(M),AJTJC(N,N),GC(N),AJC(100,8)
      DO 1 J= 1,M
      XX2 = X1(J) * X1(J)
      YY2 = Y1(J) * Y1(J)
      AJC(J,2) = -X1(J)
      AJC(J,1) = -1.0D0
      AJC(J,3) = -Y1(J)
      IF(IWEIGHT.NE.1) GO TO 11
      DO 12 I = 1,3
      AJC(J,I) = AJC(J,I)*WEIGHT(J)
   12 CONTINUE
   11 IF(IPASS.EQ.1)GO TO 1
      AJC(J,4) = -XX2
      AJC(J,5) = -X1(J)*Y1(J)
      AJC(J,6) = -YY2
      IF(IWEIGHT.NE.1) GO TO 13
      DO 14 I = 4,6
      AJC(J,I) = AJC(J,I) * WEIGHT(J)
   14 CONTINUE
   13 IF (IPASS.EQ.2) GO TO 1
      IF(IMAP.EQ.2) GO TO 33
      AJC(J,7) = -X1(J)*(XX2+YY2)
      AJC(J,8) = -X1(J)*((XX2+YY2)**2)
      GO TO 15
   33 AJC(J,7) = - Y1(J)*(XX2+YY2)
      AJC(J,8) = -Y1(J) * ((XX2+YY2)**2)
   15 IF (IWEIGHT.NE.1) GO TO 1
      DO 16 I = 7,8
      AJC(J,I) = AJC(J,I)* WEIGHT(J)
   16 CONTINUE
    1 CONTINUE
      DO 80 I = 1,N
      SUM = 0.0D0
      DO 20 K = 1,M
      SUM = SUM + AJC(K,I)*RC(K)
   20 CONTINUE
      GC(I) = SUM
      II = I
      DO 60 J = 1,II
      SUM = 0.0D0
      DO 40 K = 1,M
      SUM = SUM +AJC(K,I)*AJC(K,J)
   40 CONTINUE
      AJTJC(J,I) = SUM
   60 CONTINUE
   80 CONTINUE
      RETURN
      END
