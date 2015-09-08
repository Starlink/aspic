      SUBROUTINE SINT(X0,F1,F2,XLEFT,XRIGHT,VALUE)
C+
C
C   SUBROUTINE SINT(X0,F1,F2,XLEFT,XRIGHT,VALUE)
C
C   Integrates under line joining leftmost and rightmost points of
C   ellipse, from XLEFT to XRIGHT, returning the result in VALUE
C
C   Given       (arguments)
C   X0      D   X coordinate of centre of ellipse
C   F1      D   Parameter of equation of ellipse
C   F2      D   Parameter of equation of ellipse
C   XLEFT   D   Lower bound for integration
C   XRIGHT  D   Upper bound for integration
C  
C   Returned    (arguments)
C   VALUE   D   Value of integral
C
C   D. R. K. Brownrigg/ROE/10.12.1981
C-
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      VALUE=-F2*((XRIGHT-X0)**2-(XLEFT-X0)**2)/(4.0*F1)
      RETURN
      END
