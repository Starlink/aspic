      SUBROUTINE ELLMIN (MAXPTS,NPTS,XPTS,YPTS,MAXPAR,PARAM,
     :                   SUMSQ)
C+
C     ELLMIN.
C
C     Subroutine to compute the sum of the squares of the residuals,
C     in the polar radial coordinate, between a set of data
C     points representing an approximate ellipse
C     and a set of parameters defining an ellipse.
C
C  Given;
C   MAXPTS (I)  Max. permitted no. of data points.
C   NPTS   (I)  Actual no. of data points.
C   XPTS   (RA) Cartesian X coords. of data points.
C   YPTS   (RA)     "     Y   "   . "   "     "   .
C   MAXPAR (I)  Size of parameter array; must be .ge. 5.
C   PARAM  (RA) Array holding parameters defining the ellipse.
C
C  Returned;
C   SUMSQ  (R)  Sum of the squares of the residuals of the radial
C               polar coord. between the observed and computed 
C               ellipses.
C
C  Subroutines called;
C   E2D:-  CARPLR, ELLPLR, ELPLR1.
C
C  A C Davenhall./ROE/                                     17/9/82.
C  A C Davenhall./ROE/     {Modified}                      25/9/82.
C-
      INTEGER MAXPTS,NPTS,MAXPAR
      REAL XPTS(MAXPTS),YPTS(MAXPTS),PARAM(MAXPAR)
      REAL SUMSQ
C
      REAL A,B,H,K,PHI
      REAL RADIUS,RADEXP,SINTHA,COSTHA,SINEXP,COSEXP
      REAL SUMRAD,SUMSIN,SUMCOS
C
C
C   Decode the parameter array defining the ellipse.
C   The variables decoded into, and hence the corresponding 
C   array elements have the following meanings;
C
C    A     ... semi major axis.
C    B     ...  "   minor axis.
C    H     ... X coord. of centre of ellipse.
C    K     ... Y   "  . "    "    "     "   .
C    PHI   ... Orientation of the ellipse.
C
      A=PARAM(1)
      B=PARAM(2)
      H=PARAM(3)
      K=PARAM(4)
      PHI=PARAM(5)
C
C    Compute the sum of the squares of the residuals.
C
      SUMRAD=0.0E0
      SUMSIN=0.0E0
      SUMCOS=0.0E0
C
      DO I=1,NPTS
C
C    Convert the cartesian coords. of the input data into
C    (rotated and translated) polar coords.
C
        CALL CARPLR (XPTS(I),YPTS(I),H,K,PHI,RADIUS,SINTHA,COSTHA)
C
C    Compute the expected radius at given angular polar coord.
C    for the specified ellipse.
C
        CALL ELLPLR (A,B,SINTHA,COSTHA,RADEXP)
C
C    Accummulate the sum of the square of the residuals between
C    the observed and expected radii.
C
        SUMRAD=SUMRAD+((RADIUS-RADEXP)**2)
C
C    Compute the expected angular coords at a given radius.
C    
        CALL ELPLR1 (A,B,RADIUS,SINEXP,COSEXP)
C
C    Accummulate the sum of squares of the residuals between the
C    observed and expected angular coords.
C
        SUMSIN=SUMSIN+((ABS(SINTHA)-SINEXP)**2)
        SUMCOS=SUMCOS+((ABS(COSTHA)-COSEXP)**2)
      END DO
C
C    Compute the overall sum of the squares of the residuals.
C
      SUMSQ=SUMRAD+((SUMSIN+SUMCOS)/2.0E0)
      END
