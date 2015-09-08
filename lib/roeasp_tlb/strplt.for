      SUBROUTINE STRPLT (MAXSTR,NSTAR,XCORD,YCORD,APMAG,THRESH,
     :                   XMIN,XMAX,YMIN,YMAX)
C+
C      STRPLT.
C
C     Subroutine to take a series of star images, represented by
C     their X & Y positions and apparent magnitude and produce
C     a map of the positions for all the stars brighter than
C     a threshold magnitude. Each star plotted as a circle and
C     the brightness of the star is indicated by the size of the
C     circle.
C
C  Given;
C  MAXSTR  (I)   Max. permitted no. of stars (= size of arrays
C                XCORD, YCORD & APMAG, below).
C  NSTAR   (I)   No. of stars.
C  XCORD   (RA)  X positions of stars.
C  YCORD   (RA)  Y    "      "    "  .
C  APMAG   (RA)  Apparent magnitude of stars.
C  THRESH  (R)   Threshold brightness for plotting (magnitudes).
C  XMIN    (R)   Minimum X coord. to be plotted.
C  XMAX    (R)   Maximum X   "  . "  "     "   .
C  YMIN    (R)   Minimum Y   "  . "  "     "   .
C  YMAX    (R)   Maximum Y   "  . "  "     "   .
C
C  Subroutines called;
C  Graphics;   CIRCLE, AXIS.
C  Fings;      WINDOL, MOVTO2, CHAFIX, CHAINT, CHAESC, CHAHOL.
C
C  Structure:-
C   Force the number of stars to be less than the array size.
C   Find the largest of the X & Y ranges and set up a square
C     WINDOL from this.
C   Find brightest star in the field.
C   magnitude of brightest star corresponds 0.015 of largest range.
C       "     "  threshold          "       0.001 "    "      "  .
C   Do for all stars in field
C     If brighter than threshold
C       If position falls inside selected window.
C         Compute radius of circle to be drawn from magnitude.
C         plot circle.
C         print identification no.
C       end if
C     end if
C   end do
C   draw axes
C   print title.
C   plot a magnitude scale.
C
C   A C Davenhall./ROE/                                   2/3/82.
C-
      INTEGER MAXSTR,NSTAR
      REAL XCORD(MAXSTR),YCORD(MAXSTR),APMAG(MAXSTR)
      REAL THRESH,XMIN,XMAX,YMIN,YMAX
      INTEGER ITHR,IBRI,INCR
C
C 
      INTEGER ISTAR,ICOUNT
      REAL XRANGE,YRANGE,RANGE
      REAL XMIN1,XMAX1,YMIN1,YMAX1
      REAL BRIGHT,RADBRI,RADTHR,RADIUS,XPOS,YPOS,XPOS1,MAG
C
C
C      Force no. of pts. to be less than array size.
C
      ISTAR=MIN(MAXSTR,NSTAR)
C
C      Find the largest of the X & Y ranges & set up a
C      square window based on this.
C
      XRANGE=XMAX-XMIN
      YRANGE=YMAX-YMIN
      IF (XRANGE.GE.YRANGE) THEN
        RANGE=XRANGE
      ELSE
        RANGE=YRANGE
      END IF
      XMIN1=XMIN-(2.0E-1*RANGE)
      XMAX1=XMIN+(1.2E0*RANGE)
      YMIN1=YMIN-(2.0E-1*RANGE)
      YMAX1=YMIN+(1.2E0*RANGE)
      CALL WINDOL (XMIN1,XMAX1,YMIN1,YMAX1)
C
C      Find the brightest star in the catalogue.
C
      BRIGHT=APMAG(1)
      DO I=2,ISTAR
        IF (APMAG(I).LT.BRIGHT) BRIGHT=APMAG(I)
      END DO
      RADBRI=1.5E-2*RANGE
      RADTHR=1.0E-3*RANGE
C
C      Plot stars satisfying selection criteria.
C
      DO I=1,ISTAR
        IF (APMAG(I).LE.THRESH) THEN
          XPOS=XCORD(I)
          YPOS=YCORD(I)
          MAG=APMAG(I)
          IF (XPOS.GE.XMIN.AND.XPOS.LE.XMAX.AND.
     :        YPOS.GE.YMIN.AND.YPOS.LE.YMAX) THEN
            RADIUS=RADTHR+((MAG-THRESH)*(RADBRI-RADTHR)/
     :                     (BRIGHT-THRESH))
            CALL CIRCLE (XPOS,YPOS,RADIUS)
            CALL CHAINT (I,3)
          END IF
        END IF
      END DO
C
C      Draw axes.
C
      CALL AXIS (XMIN,XMAX,YMIN,YMAX,'X COORD.(PIXELS).',
     :          ' Y')
C
C      put out the title.
C
      XPOS=XMIN+(2.0E-1*XRANGE)
      YPOS=YMAX+(5.0E-2*YRANGE)
      CALL MOVTO2 (XPOS,YPOS)
      CALL CHAESC ('$')
      CALL CHAHOL ('STARS BRIGHTER THAN MAGNITUDE$.')
      CALL CHAFIX (THRESH,6,2)
C
C      Plot up a circle-size 'v' magnitude scale.
C
C      Find the nearest integers to the extrema of the chosen
C      brightness range & the appropriate increment.
C
      ITHR=NINT(THRESH)
      IBRI=NINT(BRIGHT)
      INCR=((ITHR-IBRI)/5)+1
C
C      Set up X position.
C
      XPOS=XMAX+(4.0E-2*XRANGE)
      XPOS1=XPOS+(5.0E-2*XRANGE)
C
C      Plot scale.
C
      ICOUNT=0
      DO I=IBRI,ITHR,INCR
        ICOUNT=ICOUNT+1
        YPOS=YMIN+(8.0E-1*YRANGE)-((5.0E-2*YRANGE)*FLOAT(ICOUNT-1))
        CALL MOVTO2 (XPOS,YPOS)
        RADIUS=RADTHR+((FLOAT(I)-THRESH)*(RADBRI-RADTHR)/
     :         (BRIGHT-THRESH))
        CALL CIRCLE (XPOS,YPOS,RADIUS)
        CALL MOVTO2 (XPOS1,YPOS)
        CALL CHAHOL (' - $.')
        CALL CHAFIX (FLOAT(I),5,1)
      END DO
      END
