      SUBROUTINE LINT(X0,Y0,F1,F2,F3,F4,XLEFT,XRIGHT,VALUE)
C+
C
C   SUBROUTINE LINT(X0,Y0,F1,F2,F3,F4,XLEFT,XRIGHT,VALUE)
C
C   Integrates lower curve of ellipse from XLEFT to XRIGHT, returning
C   the result in VALUE
C
C   Given       (arguments)
C   X0      D   X coordinate of centre of ellipse
C   Y0      D   Y coordinate of centre of ellipse
C   F1      D   Parameter of equation of ellipse
C   F2      D   Parameter of equation of ellipse
C   F3      D   Parameter of equation of ellipse
C   F4      D   Parameter of equation of ellipse
C   XLEFT   D   Lower bound for integration
C   XRIGHT  D   Upper bound for integration
C
C   Returned    (arguments)
C   VALUE   D   Value of integral
C
C   Subroutines called:
C   SINT,TINT         :E2DLIB
C  
C   D. R. K. Brownrigg/ROE/10.12.1981
C-
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CALL SINT(X0,F1,F2,XLEFT,XRIGHT,VALUE1)
      CALL TINT(X0,F1,F2,F3,F4,XLEFT,XRIGHT,VALUE2)
      VALUE=(XRIGHT-XLEFT)*Y0+VALUE1-VALUE2
      RETURN
      END
