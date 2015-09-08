      FUNCTION POLY(Z1,Z2,D)
C
C **********************************************************************
C
C     FUNCTION POLY(X,Y,D)
C
C     EVALUATES THE POLYNOMIAL DEFINED BY THE COEFFICIENTS D, AT
C     THE NORMALIZED COORDINATE (X,Y).
C
C     X = NORMALIZED X COORDINATE  (R*4)
C     Y = NORMALIZED Y COORDINATE  (R*4)
C     D(30) = ARRAY CONTAINING THE COEFFICIENTS OF THE POLYNOMIAL (R*8)
C
C     WRITTEN BY W D PENCE, NOV. 1980
C
C **********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H),(O-Y)
      DIMENSION D(30)
      DATA YLAST/-2./
      SAVE YLAST
C
C     FIRST, CALC 1-D POLYNOMIAL IN X FOR GIVEN Y VALUE, IF
C     NOT ALREADY DONE
C
      X=Z1
      Y1=Z2
C
      IF (Y1 .NE. YLAST)THEN
      Y2=Y1*Y1
      Y3=Y2*Y1
      Y4=Y3*Y1
      Y5=Y4*Y1
      Y6=Y5*Y1
C
      C0=D(1)+D(3)*Y1+D(6)*Y2+D(10)*Y3+D(15)*Y4+D(21)*Y5+D(28)*Y6
      C1=     D(2)   +D(5)*Y1+D(9) *Y2+D(14)*Y3+D(20)*Y4+D(27)*Y5
      C2=             D(4)   +D(8) *Y1+D(13)*Y2+D(19)*Y3+D(26)*Y4
      C3=                     D(7)    +D(12)*Y1+D(18)*Y2+D(25)*Y3
      C4=                              D(11)   +D(17)*Y1+D(24)*Y2
      C5=                                       D(16)   +D(23)*Y1
      C6=D(22)+D(30)*Y1
      C7=D(29)
      END IF
C
C     EVALUATE POLYNOMIAL IN X
C
      POLY=((((((C7*X+C6)*X+C5)*X+C4)*X+C3)*X+C2)*X+C1)*X+C0
C
      YLAST=Y1
      RETURN
      END
