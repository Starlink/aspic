      SUBROUTINE KOPY3 (PTS,RADIUS,INTEN,PROFIL)
C+
C     KOPY3.
C
C     Subroutine to take a profile held as separate arrays of
C     radial position and intensity and from these form a single
C     2D array suitable for outputting as a Starlink image.
C
C  Given;
C   PTS    (I)  No. of points in the profile.
C   RADIUS (RA) Radii for points in the profile.
C   INTEN  (RA) Intensities for points in the profile.
C
C  Returned;
C   PROFIL (RA) Combined profile.
C
C  Subroutines called;
C   None.
C 
C  A C Davenhall./ROE/                                          2/4/83.
C-
      INTEGER PTS
      REAL RADIUS(PTS),INTEN(PTS),PROFIL(PTS,2)
C
      DO I=1,PTS
        PROFIL(I,1)=RADIUS(I)
        PROFIL(I,2)=INTEN(I)
      END DO
C
      END
