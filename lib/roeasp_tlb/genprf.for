      SUBROUTINE GENPRF (IOBULG,ROBULG,IODISK,ALPHA,
     :                   MAXPTS,NPTS,RADIUS,LOGINT)
C+
C     GENPRF.
C
C     Subroutine to generate a profile for a disk galaxy
C     with a spheroid following the r**1/4 law and a disk
C     following the exponential law. The parameters
C     characterising the profile are given as arguments.
C
C  Given;
C   IOBULG  (R)  Io for the r**1/4 bulge.
C   ROBULG  (R)  Ro  "   "    "      "  .
C   IODISK  (R)  Io for the exponential disk.
C   ALPHA   (R)  Alpha for the exponential disk.
C   MAXPTS  (I)  Size of the arrays holding the profile (below).
C   NPTS    (I)  No. of points in the profile.
C   RADIUS  (RA) Set of radii at which the profile is to be
C                evaluated.
C   
C  Returned;
C   LOGINT  (RA) Log I evaluated along the profile at each of the
C                radii in RADIUS.
C
C  Function called;
C   PROFANAL:-  BULDISK.
C
C  A C Davenhall./ROE/                               2/8/82.
C-
      INTEGER MAXPTS,NPTS
      REAL RADIUS(MAXPTS),LOGINT(MAXPTS)
      REAL IOBULG,ROBULG,IODISK,ALPHA
C
      REAL WORK(4)
      INTEGER PTS
C
C
C    Load the parameters defining the profile into the array
C    that is used as an argument for the function to evaluate
C    the profile.
C
      WORK(1)=IOBULG
      WORK(2)=ROBULG
      WORK(3)=IODISK
      WORK(4)=ALPHA
C
C    Force the number of points to be sensible.
C
      PTS=MIN(NPTS,MAXPTS)
C
C    Evaluate the profile.
C
      DO I=1,PTS
        LOGINT(I)=BULDISK(RADIUS(I),WORK,4)
      END DO
      END
