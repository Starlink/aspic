      SUBROUTINE KOPY (PROFIL,NPTS,MAXPTS,RADIUS,INTEN)
C+
C     KOPY.
C
C     Subroutine to copy a profile from a 2D (Starlink) array
C     into 2 arrays, one for the position and the other for the
C     intensity, prior to output.
C
C  Given;
C   PROFIL   (RA)  Input 2D array containing profile.
C   NPTS     (I)   No. of points in input profile.
C   MAXPTS   (I)   Size of output arrays.
C
C  Returned;
C   RADIUS   (RA)  Output array containg positions.
C   INTEN    (RA)  Output array containing intensities.
C
C  A C Davenhall./ROE/                                        14/7/82.
C-
      INTEGER NPTS,MAXPTS
      REAL PROFIL(NPTS,2)
      REAL RADIUS(MAXPTS),INTEN(MAXPTS)
C
      INTEGER PTS
C
C
      PTS=MIN(NPTS,MAXPTS)
C
      DO I=1,PTS
        RADIUS(I)=PROFIL(I,1)
        INTEN(I)=PROFIL(I,2)
      END DO
      END
