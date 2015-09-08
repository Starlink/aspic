      SUBROUTINE ZENTRE (A,IXEXT,IYEXT,XCEN,YCEN)
C+
C     ZENTRE.
C
C       Subroutine to semi-automatically locate the centre of
C       an image, using an intensity weighted mean.
C
C       Given;
C       A     R  Array holding image.
C       IXEXT I  X Size of image.
C       IYEXT I  Y  "   "    "  .
C
C       Returned;
C       XCEN  R  X Coord. of centre of image.
C       YCEN  R  Y   "    "    "    "    "  .
C
C       Subroutines called:-
C                  GENHIS.
C        Args;     BOXCUR.
C
C       Structure:-
C
C       Pick off box using the square cursor.
C       Compute a threshold using GENHIS.
C       Compute weighted mean centre.
C       Plot centre.
C
C       A C Davenhall./ROE/                        18/12/81.
C-
      REAL A(IXEXT,IYEXT)
      REAL XCEN,YCEN
      INTEGER IXEXT,IYEXT
      INTEGER ISTAT
      INTEGER ISTART,IFIN,JSTART,JFIN,NBIN,MAXM,MAXBIN,NPTS
      REAL RMAX,RMIN,THRESH,SUMLUM
      INTEGER IXCUR,IYCUR,ISIZE,NSIZE
      INTEGER HIST(100)
C
C       Pick off a box using the square cursor.
C       The image is assumed to be plotted on the ARGS.
C
      IXCUR=IXEXT/2
      IYCUR=IYEXT/2
      ISIZE=50
      CALL BOXCUR (IXCUR,IYCUR,ISIZE)
      NSIZE=ISIZE/2
C
C      Compute the coords. of the edge of the box that has been
C      chosen.
C
      ISTART=IXCUR-NSIZE
      IFIN=ISTART+ISIZE
      JSTART=IYCUR-NSIZE
      JFIN=JSTART+ISIZE
C
C      Force these coords. to be inside the array bounds.
C
      ISTART=MAX(ISTART,1)
      IFIN=MIN(IFIN,IXEXT)
      JSTART=MAX(JSTART,1)
      JFIN=MIN(JFIN,IYEXT)
C
C       Hunt through the points inside the box to find the overall
C       max. & min., in order to generate a range for the
C       histogram routine.
C
      RMAX=A(ISTART,JSTART)
      RMIN=A(ISTART,JSTART)
      DO J=JSTART,JFIN
        DO I=ISTART,IFIN
          IF (A(I,J).LT.RMIN) RMIN=A(I,J)
          IF (A(I,J).GT.RMAX) RMAX=A(I,J)
        END DO
      END DO
C
C       Setup the no. of bins in the histogram.
C
      NBIN=100
C
C       Generate the histogram.
C
      CALL GENHIS (A,IXEXT,IYEXT,ISTART,IFIN,JSTART,JFIN,RMAX,
     :                 RMIN,HIST,NBIN)
C
C       Find the most frequently occuring value in the histogram.
C
      MAXM=HIST(1)
      MAXBIN=1
      DO I=2,NBIN
        IF (HIST(I).GT.MAXM) THEN
          MAXBIN=I
          MAXM=HIST(I)
        END IF
      END DO
C
C       Finally calculate the inensity level that this corresponds
C       to.
C
      THRESH=RMIN+((RMAX-RMIN)*FLOAT(MAXBIN))/FLOAT(NBIN)
C
C       Now compute the weighted centre.
C
      NPTS=0
      XCEN=0.0E0
      YCEN=0.0E0
      SUMLUM=0.0E0
      DO J=JSTART,JFIN
        DO I=ISTART,IFIN
          XCEN=XCEN+(FLOAT(I)*(A(I,J)-THRESH))
          YCEN=YCEN+(FLOAT(J)*(A(I,J)-THRESH))
          SUMLUM=SUMLUM+(A(I,J)-THRESH)
          NPTS=NPTS+1
        END DO
      END DO
      XCEN=XCEN/SUMLUM
      YCEN=YCEN/SUMLUM
      END
