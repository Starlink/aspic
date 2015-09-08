      SUBROUTINE POLVAL(NPOLY,COEFF,X,Y)                                        
C+
C   POLVAL
C
C     EVALUATES POLYNOMIAL OF ORDER NPOLY
C
C     J.A.COOKE/UOE/1981
C-
      REAL*8 COEFF(21),X,Y
      N=NPOLY+1
      Y=COEFF(1)
      DO 1 I=2,N
    1 Y=Y+COEFF(I)*X**(I-1)
      RETURN
      END
C
C
C*********************************************************************
