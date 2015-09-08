      SUBROUTINE KOPY2 (PROFIL,NPTS,SKY,MAXPTS,RADIUS,INTEN)
C+
C     KOPY2.
C
C     Copy a profile from a 2D (Starlink) array into 2 arrays,
C     one for the position and the other the intensity. The
C     intensity is assumed input relative to a sky level of
C     1.0 and on this basis is converted to magnitudes/sq.
C     arcsec.
C
C     Note; Points below the sky, ie. intensities less than 1.0,
C           for which the brightness is undefined will be
C           arbitarily set to 30.0 mag./sq.arcsec.
C
C  Given;
C   PROFIL  (RA)  Input 2D array containing the profile.
C   NPTS    (I)   No. of points in the input profile.
C   SKY     (R)   Sky brightness (mag./sq.arcsec).
C   MAXPTS  (I)   Size of the output arrays.
C
C  Returned;
C   RADIUS  (RA)  Output array containing positions.
C   INTEN   (RA)  Output array containg intensities expressed
C                 in mag./sq.arcsec.
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                                    1/8/82.
C-
      INTEGER NPTS,MAXPTS
      REAL PROFIL(NPTS,2)
      REAL RADIUS(MAXPTS),INTEN(MAXPTS)
      REAL SKY
C
      INTEGER PTS
C
      REAL POGSON
      PARAMETER (POGSON=2.5E0)
C
C
      PTS=MIN(NPTS,MAXPTS)
C
      DO I=1,PTS
        RADIUS(I)=PROFIL(I,1)
        IF (PROFIL(I,2).GT.1.00001E0) THEN
          INTEN(I)=SKY-(POGSON*ALOG10(PROFIL(I,2)-1.0E0))
        ELSE
          INTEN(I)=3.0E1
        END IF
      END DO
      END
