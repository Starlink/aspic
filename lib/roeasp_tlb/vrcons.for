      SUBROUTINE VRCONS (XEXT,YEXT,IMAGE,MAXREG,MAXPAR,REGION,
     :                   NREG,THRESH,INCR,TITLE)
C+
C     VRCONS.
C
C     Subroutine to produce a series of contour maps
C     for a set of predefined regions of an array.
C     The Versatec is automatically
C     selected as the plotting device.
C
C  Given;
C   XEXT   (I)  X extent of the image.
C   YEXT   (I)  Y   "    "   "    "  .
C   IMAGE  (RA) Image to be contoured.
C   MAXREG (I)  Max. permitted no of regions.
C   MAXPAR (I)  Max. permitted no. of parameters for each region
C               (must be greater than 4).
C   REGION (IA) Array holding coords. of regions to be contoured.
C   NREG   (I)  No. of regions to be contoured.
C   THRESH (R)  Faintest contour to be plotted.
C   INCR   (R)  Increment between contours.
C   TITLE  (C)  Title to appear on contour maps.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Graphics:-  GDEVI1, AXIS, STOPLOT.
C   Fings:-     MOVT02, CHAESC, CHAHOL, WINDOL.
C   E2D:-       CONMAP.
C
C  A C Davenhall./ROE/ {Modified from VRPLTS.}               1/2/84.
C-
      IMPLICIT NONE
C
      INTEGER XEXT,YEXT
      REAL IMAGE(XEXT,YEXT)
      INTEGER MAXREG,MAXPAR,NREG
      INTEGER REGION(MAXREG,MAXPAR)
      REAL THRESH,INCR
      CHARACTER TITLE*(*)
C
      INTEGER I,J,K
      INTEGER CNTRS,IBASE,ITOP,JBASE,JTOP
      LOGICAL CTRLC
      REAL XMIN,XMAX,YMIN,YMAX,XTOP,YTOP,RANGE
      REAL XMIN1,YMIN1,XTOP1,YTOP1
      REAL MAXVAL,XPOS,YPOS,THRPSU
      REAL XRANGE,YRANGE
      CHARACTER BUFFER*80
C
C
C    Contour each region in turn.
C
      DO K=1,NREG
C
C    Set up the extent of the current region.
C
        IBASE=REGION(K,1)
        JBASE=REGION(K,2)
        ITOP=REGION(K,3)
        JTOP=REGION(K,4)
C
C    Find the largest pixel in the region.
C
        MAXVAL=IMAGE(IBASE,JBASE)
        DO J=JBASE,JTOP
          DO I=IBASE,ITOP
            IF (IMAGE(I,J).GT.MAXVAL) MAXVAL=IMAGE(I,J)
          END DO
        END DO
C
C    Determine the number of contours.
C
        CNTRS=NINT((MAXVAL-THRESH)/INCR)+1
C
C    Set up for plotting to the versatec using fings.
C
        CALL GDEVI1 (3,.TRUE.)
C
C    Define a square plotting space to force the scales
C    for both axes to be the same.
C
        XMIN=FLOAT(IBASE)
        XMAX=FLOAT(ITOP)
        YMIN=FLOAT(JBASE)
        YMAX=FLOAT(JTOP)
        RANGE=MAX(XMAX-XMIN,YMAX-YMIN)
        XTOP=XMIN+RANGE
        YTOP=YMIN+RANGE
        XRANGE=XTOP-XMIN
        YRANGE=YTOP-YMIN
C
C    Allow space for plotting axes etc.
C
        XMIN1=XMIN-(RANGE*2.0E-1)
        XTOP1=XTOP+(RANGE*2.0E-1)
        YMIN1=YMIN-(RANGE*2.0E-1)
        YTOP1=YTOP+(RANGE*2.0E-1)
C
C    Define the plotting space.
C
        CALL WINDOL (XMIN1,XTOP1,YMIN1,YTOP1)
C
C    Draw the axes.
C
        CALL AXIS (XMIN,XMAX,YMIN,YMAX,'X (PIXELS)','Y')
C
C    Plot the contours.
C
        CTRLC=.FALSE.
        CALL CONMAP (IMAGE,XEXT,YEXT,IBASE,ITOP,JBASE,JTOP,
     :               THRESH,INCR,CNTRS,CTRLC)
C
C    indicate the number of the region if there is more than one.
C
C
C    Print out information at the top of the plot.
C
        XPOS=XMIN+(XRANGE*5.0E-2)
        YPOS=YTOP+(YRANGE*1.5E-1)
        CALL MOVTO2 (XPOS,YPOS)
        CALL CHAESC ('$')
        WRITE(BUFFER,2000) K
 2000   FORMAT(1X,'REGION NUMBER ',I2)
        CALL CHAHOL (%REF(BUFFER//'$.'))
C
        XPOS=XMIN+(XRANGE*3.5E-1)
        CALL MOVTO2 (XPOS,YPOS)
        CALL CHAHOL (%REF(TITLE//'$.'))
C
        XPOS=XMIN+(XRANGE*5.0E-2)
        YPOS=YTOP+(YRANGE*1.0E-1)
        CALL MOVTO2 (XPOS,YPOS)
        WRITE(BUFFER,2001) THRESH,INCR
 2001   FORMAT(1X,'THRESHOLD = ',1PE12.3,2X,'INCREMENT = ',1PE12.3)
        CALL CHAHOL (%REF(BUFFER//'$.'))
C
C    Terminate fings.
C
        CALL STOPLOT
      END DO
      END
