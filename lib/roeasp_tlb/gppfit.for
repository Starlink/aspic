      SUBROUTINE GPPFIT(X1,Y1,X2,Y2,GRAD2,A,B,C)
*+
*   GPPFIT
*
*   Fits parabola from two points and gradient at one point.
*   Parabola is of form  A + B*X + C*X*X
*
*   J A Cooke 31Aug82
*-
      REAL X1,Y1,X2,Y2,GRAD2,A,B,C,DENOM

      IF (X1.EQ.X2) THEN
         WRITE(6,*)' *** ERROR in GPPFIT: same X for points'
      ELSE
         C=((Y1-Y2)-GRAD2*(X1-X2))/(X1*X1-2.*X1*X2+X2*X2)
         B=GRAD2-2.*X2*C
         A=Y2-C*X2*X2-B*X2
      ENDIF

      END
