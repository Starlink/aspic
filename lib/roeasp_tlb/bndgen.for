      SUBROUTINE BNDGEN (MAXLEV,MAXPAR,MINLGI,DELOGI,HWIDTH,
     :                   NLEVEL,LOGI,INTEN)
C+
C     BNDGEN.
C
C     Generate a set of isophotal bands, equally spaced in Log I
C     above the sky, and also the corresponding values of
C     intensity relative to a sky of 1.0.
C
C  Given;
C   MAXLEV (I)  Maximum permitted no. of levels
C               (=size of arrays LOGI, INTEN).
C   MAXPAR (I)  = 2.
C   MINLGI (R)  Minimum Log I in the set of levels.
C   DELOGI (R)  Icrement in Log I.
C   HWIDTH (R)  Half width of the bands generated, in Log I.
C   NLEVEL (I)  No. of levels required.
C
C  Returned;
C   LOGI   (RA) Array of Log I values above the sky.
C   INTEN  (RA) Array of intensities relative to a sky of 1.0.
C
C  A C Davenhall./ROE/                                  25/9/82.
C-
      INTEGER MAXLEV,NLEVEL
      REAL MINLGI,DELOGI
      REAL LOGI(MAXLEV),INTEN(MAXPAR,MAXLEV)
C
      INTEGER PTS
C
      REAL TEN
      PARAMETER (TEN=1.0E1)
C
C
      PTS=MIN(NLEVEL,MAXLEV)
C
      DO I=1,PTS
        LOGI(I)=MINLGI+(DELOGI*FLOAT(I-1))
        INTEN(1,I)=1.0E0+(TEN**(LOGI(I)-HWIDTH))
        INTEN(2,I)=1.0E0+(TEN**(LOGI(I)+HWIDTH))
      END DO
      END
