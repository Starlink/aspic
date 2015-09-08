      SUBROUTINE TINT(X0,F1,F2,F3,F4,XLEFT,XRIGHT,VALUE)
C+
C
C   SUBROUTINE TINT(X0,F1,F2,F3,F4,XLEFT,XRIGHT,VALUE)
C
C   Integrates the half distance (vertical) between upper and lower
C   curves of the ellipse, between XLEFT and XRIGHT, returning the
C   result in VALUE
C
C   Given       (arguments)
C   X0      D   X coordinate of centre of ellipse
C   F1      D   Parameter of equation of ellipse
C   F2      D   Parameter of equation of ellipse
C   F3      D   Parameter of equation of ellipse
C   F4      D   Parameter of equation of ellipse
C   XLEFT   D   Lower bound for integral
C   XRIGHT  D   Upper bound for integral
C  
C   Returned    (arguments)
C   VALUE   D   Value of the integral
C
C   D. R. K. Brownrigg/ROE/10.12.1981
C-
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      XT=XRIGHT-X0
      XB=XLEFT-X0
      XT2=XT*XT
      XB2=XB*XB
      FF1=4.0*F1*F4
      FF2=F2*F2-4.0*F1*F3
      RFF2=DSQRT(ABS(FF2))
      SQ1=FF1+FF2*XT2
      ARG1=DMAX1(0.0D0,SQ1)
      SQ2=FF1+FF2*XB2
      ARG2=DMAX1(0.0D0,SQ2)
      VALUE1=(XT*DSQRT(ARG1)-XB*DSQRT(ARG2))/2.0
      IF(FF2.GT.0.0) THEN
          VALUE=(1.0/(2.0*F1))*(VALUE1+(FF1/(2.0*RFF2))*
     +    (DLOG(XT*RFF2+DSQRT(ARG1))
     +    -DLOG(XB*RFF2+DSQRT(ARG2))))
      ELSE IF(FF1.GT.0.0.AND.FF2.LT.0.0) THEN
          ARG3=RFF2/DSQRT(FF1)
          ARG4=ARG3*XT
          IF(ARG4.LT.-1.0) THEN
              ARG4=-1.0
          ELSE IF(ARG4.GT.1.0) THEN
              ARG4=1.0
          ENDIF
          ARG5=ARG3*XB
          IF(ARG5.LT.-1.0) THEN
              ARG5=-1.0
          ELSE IF(ARG5.GT.1.0) THEN
              ARG5=1.0
          ENDIF
          VALUE=(1.0/(2.0*F1))*(VALUE1+(FF1/(2.0*RFF2))*
     +    (DASIN(ARG4)-DASIN(ARG5)))
      ELSE
C         [IF(FF2.EQ.0.0)]
          VALUE=(1.0/(2.0*F1))*(XT-XB)*DSQRT(ABS(FF1))
      ENDIF
      RETURN
      END
