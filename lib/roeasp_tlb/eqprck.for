      SUBROUTINE EQPRCK (MAXEQP,MAXPAR,NPTS,EQPRO,NEWPTS)
C+
C     EQPRCK.
C
C     Subroutine to detect and remove turnovers in an
C     (equivalent) profile, ie. a set of radii which are not
C     monotonically increasing. Points below the last 
C     monotonic one are discarded.
C
C  Given;
C   MAXEQP  (I)  Max. permitted no. of points in the profile.
C   MAXPAR  (I)  No. of parameters for each point in profile (=2).
C   NPTS    (I)  Actual no. of points in the profile.
C   EQPRO   (RA) Array holding equivalent profile.
C
C  Returned;
C   NEWPTS  (I)  No. of monotonic points in the equivalent profile
C                (may have been modified from the original no. of
C                points).
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                                    6/12/82.
C  A C Davenhall./ROE/  {Modified}                        13/2/83.
C-
      INTEGER MAXEQP,MAXPAR,NPTS,NEWPTS
      REAL EQPRO(MAXPAR,MAXEQP)
C
      INTEGER COUNT,KOUNT,LAST
      REAL MAXVAL
C
C
C    Check whether the profile ever doubles back on itself.
C
      LAST=NPTS
      DO COUNT=NPTS,2,-1
        KOUNT=COUNT-1
        IF (EQPRO(1,COUNT).LE.EQPRO(1,KOUNT)) LAST=KOUNT
      END DO
C
C    If the profile ever does double back throw away all the
C    points below the turn over.
C
      IF (LAST.NE.NPTS) THEN
        NEWPTS=LAST
      ELSE
        NEWPTS=NPTS
      END IF
C
      END
