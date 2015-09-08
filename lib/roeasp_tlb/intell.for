      SUBROUTINE INTELL (MAXPTS,NPTS,XPTS,YPTS,XCEN,YCEN,A,B,THETA)
C+
C     INTELL.
C
C     Subroutine to take a set of points representing a 
C     contour approximately defining ellipse and to4
C     produce approximate values for the parameters
C     defining the ellipse. Normally these values will
C     be further refined by least squares fitting.
C
C  Given;
C   MAXPTS (I)  Max. permitted no. of points.
C   NPTS   (I)  Actual no. of points.
C   XPTS   (RA) X Coords. of points.
C   YPTS   (RA) Y   "   . "    "   .
C   
C  Returned;
C   XCEN   (R)  X Coord. of centre of the ellipse.
C   YCEN   (R)  Y   "  . "    "    "   "     "   .
C   A      (R)  Semi-major axis.
C   B      (R)   "  -minor  "  .
C   THETA  (R)  Orientation.
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                             22/9/82.
C-
      INTEGER MAXPTS,NPTS
      REAL XPTS(MAXPTS),YPTS(MAXPTS)
      REAL XCEN,YCEN,A,B,THETA
C
      DOUBLE PRECISION SUM1,SUM2
      REAL RADIUS,RADMAX,RADMIN,XMAX,YMAX
C
C
C    Compute the X and Y centroids.
C
      SUM1=0.0D0
      SUM2=0.0D0
      DO I=1,NPTS
        SUM1=SUM1+XPTS(I)
        SUM2=SUM2+YPTS(I)
      END DO
      XCEN=SUM1/FLOAT(NPTS)
      YCEN=SUM2/FLOAT(NPTS)
C
C    Find the largest and smallest radii, and also keep
C    track of which points are giving the largest radius.
C
      RADMAX=SQRT(((XPTS(1)-XCEN)**2)+((YPTS(1)-YCEN)**2))
      RADMIN=RADMAX
      XMAX=XPTS(1)
      YMAX=YPTS(1)
C
      DO I=2,NPTS
        RADIUS=SQRT(((XPTS(I)-XCEN)**2)+((YPTS(I)-YCEN)**2))
        IF (RADIUS.GT.RADMAX) THEN
          RADMAX=RADIUS
          XMAX=XPTS(I)
          YMAX=YPTS(I)
        END IF
        IF (RADIUS.LT.RADMIN) RADMIN=RADIUS
      END DO
C
C    Set the major and minor axes to the largest and smallest 
C    radii found.
C
      A=RADMAX
      B=RADMIN
C
C    compute theta from the angle formed by the largest radius.
C
      THETA=ATAN2(YMAX-YCEN,XMAX-XCEN)
      END
