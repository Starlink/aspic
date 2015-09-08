      SUBROUTINE BOSMO (BOXSIZ,XPIX,YPIX,IXEXT,IYEXT,IMAGE,PIXVAL)
C+
C     BOSMO.
C
C     Subroutine to compute the boxsmoothed value for
C     a given pixel in a 2 dimensional array.
C
C  Given;
C   BOXSIZ  (I)  Smoothing half boxsize (excluding central pixel).
C   XPIX    (I)  X coord of pixel to be smoothed.
C   YPIX    (I)  Y   "   "   "    "  "     "    .
C   IXEXT   (I)  X size of array.
C   IYEXT   (I)  Y  "   "    "  .
C   IMAGE   (RA) Image array.
C
C  Returned;
C   PIXVAL  (R)  Smoothed value for selected pixel.
C
C  A C Davenhall./ROE/                                27/5/82.
C-
      INTEGER BOXSIZ,IXEXT,IYEXT,XPIX,YPIX
      REAL IMAGE(IXEXT,IYEXT)
      REAL PIXVAL
C
C
      INTEGER ISTRT,ISTP,JSTRT,JSTP,PTS
C
C
C      Check that the required pixel falls inside the array.
C
      IF (XPIX.LT.1.OR.XPIX.GT.IXEXT.OR.
     :    YPIX.LT.1.OR.YPIX.GT.IYEXT) THEN
        PIXVAL=0.0E0
      ELSE
C
C      Required pixel falls inside image.
C
C      Set up limits for smoothing boxsize.
C
        ISTRT=XPIX-BOXSIZ
        ISTRT=MAX(1,ISTRT)
        ISTP=XPIX+BOXSIZ
        ISTP=MIN(IXEXT,ISTP)
        JSTRT=YPIX-BOXSIZ
        JSTRT=MAX(1,JSTRT)
        JSTP=YPIX+BOXSIZ
        JSTP=MIN(IYEXT,JSTP)
C
        PIXVAL=0.0E0
        PTS=0
        DO JJ=JSTRT,JSTP
          DO II=ISTRT,ISTP
            PTS=PTS+1
            PIXVAL=PIXVAL+IMAGE(II,JJ)
          END DO
        END DO
        PIXVAL=PIXVAL/FLOAT(PTS)
      END IF
      END
