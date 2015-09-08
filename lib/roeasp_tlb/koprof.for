      SUBROUTINE KOPROF (MAXEQP,MAXDIM,EQPRO,NPTS,OUTARR)
C+
C     KOPROF.
C
C     Subroutine to copy the equivalent profile work array
C     into a Starlink image "output" array.
C
C  Given;
C   MAXEQP    (I)  Max. permitted no. of points in input array.
C   MAXDIM    (I)  Max. permitted no. of parameters for each profile
C                  (=2)
C   EQPRO     (RA) Input profile.
C   NPTS      (I)  Actual no. of points in the profile.
C   
C  Returned;
C   OUTARR    (RA) Output profile.
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                                     22/7/82.
C-
      INTEGER MAXEQP,MAXDIM,NPTS
      REAL EQPRO(MAXDIM,MAXEQP)
      REAL OUTARR(NPTS,2)
C
      INTEGER PTS
C
C
      PTS=MIN(NPTS,MAXEQP)
C
      DO I=1,PTS
        OUTARR(I,1)=EQPRO(1,I)
        OUTARR(I,2)=EQPRO(2,I)
      END DO
      END
