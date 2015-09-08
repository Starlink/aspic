      SUBROUTINE VRPLTS (XEXT,YEXT,IMAGE,MAXREG,MAXPAR,REGION,
     :                   NREG,SKY,THRESH,INCR,TITLE)
C+
C     VRPLTS.
C
C     Subroutine to produce a series of contour maps, in magnitudes
C     per square arcsec for a set of predefined regions of an array
C     held as "pseudo contours" (2.5logI). The Versatec is automatically
C     selected as the plotting device.
C
C  Given;
C   XEXT   (I)  X extent of the image.
C   YEXT   (I)  Y   "    "   "    "  .
C   IMAGE  (RA) Image to be contoured in pseudo magnitudes (2.5logI).
C   MAXREG (I)  Max. permitted no of regions.
C   MAXPAR (I)  Max. permitted no. of parameters for each region
C               (must be greater than 4).
C   REGION (IA) Array holding coords. of regions to be contoured.
C   NREG   (I)  No. of regions to be contoured.
C   SKY    (R)  Sky brightness (mag./sq.arcsec.).
C   THRESH (R)  Faintest contour to be plotted (mag./sq.arcsec).
C   INCR   (R)  Increment between contours (mag./sq.arcsec).
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
C  A C Davenhall./ROE/                                       9/9/82.
C-
      INTEGER XEXT,YEXT
      REAL IMAGE(XEXT,YEXT)
      INTEGER MAXREG,MAXPAR,NREG
      INTEGER REGION(MAXREG,MAXPAR)
      REAL THESH,INCR,SKY
      CHARACTER TITLE*(*)
C
      INTEGER CNTRS,IBASE,ITOP,JBASE,JTOP
      LOGICAL CTRLC
      REAL XMIN,XMAX,YMIN,YMAX,XTOP,YTOP,RANGE
      REAL XMIN1,YMIN1,XTOP1,YTOP1
      REAL MAXVAL,XPOS,YPOS,THRPSU
      REAL XRANGE,YRANGE
      CHARACTER BUFFER*80
C
C
C    Compute the threshold in pseudo magnitudes corresponding
C    to the input threshold in mag./sq.arcsec.
C
      THRPSU=SKY-THRESH
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
        CNTRS=NINT((MAXVAL-THRPSU)/INCR)+1
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
     :               THRPSU,INCR,CNTRS,CTRLC)
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
        WRITE(BUFFER,2001) THRESH,INCR,SKY
 2001   FORMAT(1X,'THRESHOLD = ',0PF6.2,2X,'INCREMENT = ',F5.2,2X,
     :         'SKY LEVEL = ',F6.2)
        CALL CHAHOL (%REF(BUFFER//'$.'))
C
        YPOS=YTOP+(YRANGE*5.0E-2)
        CALL MOVTO2 (XPOS,YPOS)
        WRITE(BUFFER,2002)
 2002   FORMAT(1X,'ALL VALUES IN MAGNITUDES / SQUARE ARCSECOND.')
        CALL CHAHOL (%REF(BUFFER//'$.'))
C
C    Terminate fings.
C
        CALL STOPLOT
      END DO
      END
