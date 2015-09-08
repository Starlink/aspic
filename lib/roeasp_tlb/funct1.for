      SUBROUTINE FUNCT1 (N,XC,FC)
C+
C     FUNCT1.
C
C     Subroutine to return the sum of the squares of the
C     residuals between an ellipse evaluated at a given point
C     and a particular data set representing a contour.
C     FUNCT1 is called by NAG routine E04CGF, hence its
C     (unalterable) name. Similarly because of this the
C     data representing the contour must be passed in
C     using COMMON.
C
C  Given;
C   N   (I)  No. of variables to be fitted.
C   XC  (DA) Parameters of the fitted ellipse for which the sum
C            of the squares of the residuals are to be evaluated.
C
C  Returned;
C   FC  (D)  Sum of the squares of the residuals evaluated at 
C            the specified point.
C
C  Subroutines called;
C   E2D:-   ELLMIN.
C
C  A C Davenhall./ROE/                                  22/9/82.
C-
      INTEGER N
      DOUBLE PRECISION XC(N)
      DOUBLE PRECISION FC
C
C
      COMMON /CNTR/ MAXPTS,NPTS,CNTRX,CNTRY
      INTEGER MAXPTS,NPTS
      REAL CNTRX(5000),CNTRY(5000)
C
C
      INTEGER MAXPAR
      PARAMETER (MAXPAR=5)
      REAL PARAM(5)
      REAL SUMSQ
C
C
C    Copy the double precision array into the single precision
C    array required by ELLMIN.
C
      DO I=1,MAXPAR
        PARAM(I)=XC(I)
      END DO
C
C    Evaluate the sum of the squares of the residuals at
C    the specified point in parameter space.
C
      CALL ELLMIN (MAXPTS,NPTS,CNTRX,CNTRY,MAXPAR,PARAM,
     :             SUMSQ)
C
C    Copy the sum of the squares of the residuals into the
C    double precision return variable.
C
      FC=SUMSQ
      END
