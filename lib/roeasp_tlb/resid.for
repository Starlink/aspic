      SUBROUTINE RESID (MAXPTS,NPTS,Y1,Y2,RES)
C+
C     RESID.
C
C     Subroutine to evaluate a set of residuals between
C     2 datasets.
C
C  Given;
C   MAXPTS  (I)  Size of arrays to hold datasets (below).
C   NPTS    (I)  No. of points in datasets.
C   Y1      (RA) First dataset.
C   Y2      (RA) Second dataset.
C
C  Returned;
C   RES     (RA) Residuals evaluated by subracting Y2 from Y1.
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                                  3/8/82.
C-
      INTEGER MAXPTS,NPTS
      REAL Y1(MAXPTS),Y2(MAXPTS),RES(MAXPTS)
C
      INTEGER PTS
C
C
      PTS=MIN(NPTS,MAXPTS)
C
      DO I=1,PTS
        RES(I)=Y1(I)-Y2(I)
      END DO
      END
