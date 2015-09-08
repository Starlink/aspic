      SUBROUTINE COMCAL(X0,Y0,A,B,ALPHA,F1,F2,F3,F4,XEL,YEL,
     :                  XER,YER,XEB,YEB,XET,YET)
C+
C
C   SUBROUTINE COMCAL(X0,Y0,A,B,ALPHA,F1,F2,F3,F4,XEL,YEL,
C                     XER,YER,XEB,YEB,XET,YET)
C
C   Calculates values of variables that remain fixed
C   during one call of ADDEL3
C
C   Given       (arguments)
C   X0      D   X coordinate of centre of ellipse
C   Y0      D   Y coordinate of centre of ellipse
C   A       D   Length of first semi major axis
C   B       D   Length of second semi major axis
C   ALPHA   D   Tilt of A axis w.r.t. X direction
C
C   Returned    (arguments)
C   F1      D   Parameter of equation of ellipse
C   F2      D   Parameter of equation of ellipse
C   F3      D   Parameter of equation of ellipse
C   F4      D   Parameter of equation of ellipse
C   XEL     D   X coordinate of leftmost point on ellipse
C   YEL     D   Y coordinate of leftmost point on ellipse
C   XER     D   X coordinate of rightmost point on ellipse
C   YER     D   Y coordinate of rightmost point on ellipse
C   XEB     D   X coordinate of lowest point on ellipse
C   YEB     D   Y coordinate of lowest point on ellipse
C   XET     D   X coordinate of highest point on ellipse
C   YET     D   Y coordinate of highest point on ellipse
C  
C   D. R. K. Brownrigg/ROE/10.12.1981
C-
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      A2=A*A
      B2=B*B
      SAL=DSIN(ALPHA)
      CAL=DCOS(ALPHA)
      SAL2=SAL*SAL
      CAL2=CAL*CAL
      F1=B2*SAL2+A2*CAL2
      F2=2.0*CAL*SAL*(B2-A2)
      F3=B2*CAL2+A2*SAL2
      F4=A2*B2
      XLIM=DSQRT(4.0*F1*F4/(4.0*F1*F3-F2*F2))
      XEL=X0-XLIM
      YEL=Y0+(-F2*(-XLIM)/(2.0*F1))
      XER=X0+XLIM
      YER=Y0+(-F2*XLIM/(2.0*F1))
      YLIM=DSQRT(4.0*F3*F4/(4.0*F1*F3-F2*F2))
      YEB=Y0-YLIM
      XEB=X0+(-F2*(-YLIM)/(2.0*F3))
      YET=Y0+YLIM
      XET=X0+(-F2*YLIM/(2.0*F3))
      RETURN
      END
