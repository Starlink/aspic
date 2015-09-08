      SUBROUTINE HISTPLOT (JHIST,ARMAX,ARMIN,XMIN,XMAX,YMIN,YMAX)
C+
C     HISTPLOT
C
C     Plots histogram on graphics terminal.
C
C  Given;
C   JHIST  (IA) Histogram array.
C   ARMAX  (R)  Maximum value in the histogram.
C   ARMIN  (R)  Minimum   "   "   "     "     .
C
C  Returned;
C   XMIN   (R)  Minimum value in the plotting space.
C   XMAX   (R)  Maximum   "   "   "     "       "  .
C   YMIN   (R)  Minimum   "   "   "     "       "  .
C   YMAX   (R)  Maximum   "   "   "     "       "  .
C
C  Subroutines called;
C   E2D:-  PICCLE, WINDO, DRAXIS, QLOT.
C
C      The plotting is friged so that the first and last bins
C      in the histogram are only plotted when they are not the
C      largest bins in the histogram. This is to avoid damaging
C      the autoscaling when an expanded histogram consisting of only
C      part of the complete histogram is being plotted (in this
C      case the unplotted parts of the histogram "pile up" in
C      the first and last bins.
C
C	B.D KELLY/ROE/3.9.1981
C       A C Davenhall./ROE/   {Modified}                  9/12/82.
C-
      INTEGER JHIST(2048)
      REAL ARMAX,ARMIN,XMIN,XMAX,YMIN,YMAX
C
      REAL STEP BASE
      INTEGER NBIN
      PARAMETER (NBIN=2048)
      REAL HISTX(NBIN),HISTY(NBIN)
C
C
C    Reconstruct the X-values of the histogram.
C
      STEP=(ARMAX-ARMIN)/FLOAT(NBIN-1)
      BASE=ARMIN-(STEP/2.0E0)
      DO I=1,NBIN
        HISTX(I)=BASE+(FLOAT(I)*STEP)
      END DO
C
C    Reconstruct the Y-values (counts in a given bin)
C    of the histogram.
C
      PHIST=JHIST(2)
C
      DO I=2,NBIN-1
        HISTY(I)=FLOAT(JHIST(I))
        PHIST=MAX(PHIST,HISTY(I))
      ENDDO
C
      IF (FLOAT(JHIST(1)).LE.PHIST) THEN
        HISTY(1)=FLOAT(JHIST(1))
      ELSE
        HISTY(1)=0.0E0
      END IF
      IF (FLOAT(JHIST(NBIN)).LE.PHIST) THEN
        HISTY(NBIN)=FLOAT(JHIST(NBIN))
      ELSE
        HISTY(NBIN)=0.0E0
      END IF
C
      CALL PICCLE 
      CALL WINDO (HISTX,HISTY,NBIN,NBIN,XMIN,XMAX,
     :            YMIN,YMAX,.FALSE.)
      CALL DRAXIS (XMIN,XMAX,YMIN,YMAX,'PIXEL VALUE','COUNT')
      CALL QLOT (HISTX,HISTY,NBIN,NBIN)
C
      END
