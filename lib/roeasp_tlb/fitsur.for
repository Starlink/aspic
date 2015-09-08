      SUBROUTINE FITSUR (X,Y,F,MAXPTS,NPTS,A)
C+
C	 Subroutine to fit a 2 dimensional polynomial to a set
C       of values that have been extracted from around a star
C       image or similar blemish.
C
C  Given;
C  X  (R)  Array of X coords. of fitted points.
C  Y  (R)    "   "  Y   "   . "    "      "   .
C  F  (R)    "   "  values to be fitted.
C  MAXPTS (I) Size of arrays X,Y,F.
C  NPTS   (I) No. of pts. to be fitted.
C
C  Returned;
C  A   (R)  Array of computed coeffs.
C
C    Subroutines called;  MATINV, MATMUL.
C    Function called;     FUNCT.
C
C    A C Davenhall./ROE/                                   20/1/82.
C-
      REAL X(MAXPTS),Y(MAXPTS),F(MAXPTS)
      REAL A(9)
      INTEGER NPTS,MAXPTS
      REAL ALPHA(9,9),BETA(9)
      INTEGER MAXORD
      REAL FMEAN,XX,YY
      DATA MAXORD/9/
C
C      Find the mean value of F and subtract it from all the data
C      points in order to minimise the absolute size of the
C      summations.
C
      FMEAN=0.0E0
      DO I=1,NPTS
        FMEAN=FMEAN+F(I)
      END DO
      FMEAN=FMEAN/FLOAT(NPTS)
      DO I=1,NPTS
        F(I)=F(I)-FMEAN
      END DO
C
C      Set all array elements to zero prior to accumulating
C      summations.
C
      DO J=1,MAXORD
        BETA(J)=0.0E0
        DO K=1,MAXORD
          ALPHA(J,K)=0.0E0
        END DO
      END DO
C
C      Accumulate summations.
C
      DO I=1,NPTS
        XX=X(I)
        YY=Y(I)
        DO J=1,MAXORD
          DO K=1,MAXORD
            ALPHA(J,K)=ALPHA(J,K)+(FUNCT(J,XX,YY)*FUNCT(K,XX,YY))
          END DO
          BETA(J)=BETA(J)+(F(I)*FUNCT(J,XX,YY))
        END DO
      END DO
C
C      Invert the matrix Alpha.
C
      CALL MATINV (ALPHA,MAXORD,MAXORD)
C
C      Multiply alpha**-1 by beta to give the polynomial coeffs.
C
      CALL MATMUL (ALPHA,BETA,A,MAXORD,MAXORD)
C
C      Add the mean F value back onto the zeroth coeff.
C
      A(1)=A(1)+FMEAN
      END
