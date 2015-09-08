      SUBROUTINE VRSTRP (XEXT,YEXT,IMAGE1,IMAGE2,FLAG,LANE,
     :                   WORKPT,WORK1,WORK2,WORK3)
C+
C     VRSTRP.
C
C     Subroutine to extract strips parallel to either the
C     X or Y axes from two images and plot them
C     superposed. Typically one image will represent a 
C     fit made, in some sense, to the other.
C
C  Given;
C   XEXT   (I)  X extent of images.
C   YEXT   (I)  Y   "    "    "   .
C   IMAGE1 (RA) First image (typically data).
C   IMAGE2 (RA) Second image (typically fit to data).
C   FLAG   (I)  Flag determining whether the strip is
C               to be extracted along a row or column;
C               = 1 - Row (Parallel to X axis).
C               = 2 - Column (Parallel to Y axis).
C   LANE   (I)  Row or Column that is to be extracted.
C   WORKPT (I)  Size of work arrays (below).
C
C  Used;
C   WORK1  (RA) Work array, size = WORKPT.
C   WORK2  (RA)  "     "     "   =   "   .
C   WORK3  (RA)  "     "      "  =   "   .
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Graphics:-  GDEVI1, DRAXIS, TYPLO1, QLOT, STOPLOT.
C   Fings:-     CLICTL, MOVTO2, CHAESC, CHAHOL.
C
C  Structure:-
C   Extract strip from (row or column) of image1.
C      "      "    "   ( "  "    "   ) "  image2.
C   Compute Max. and Min. values for the plotting space.
C   Draw axis.
C   Plot strip from image 1.
C   Plot strip from image 2.
C   Stop plotting.
C
C  A C Davenhall./ROE/                           16/8/82.
C-
      INTEGER XEXT,YEXT,FLAG,LANE,WORKPT
      REAL IMAGE1(XEXT,YEXT),IMAGE2(XEXT,YEXT)
      REAL WORK1(WORKPT),WORK2(WORKPT),
     :     WORK3(WORKPT)
C
      REAL MAXVAL,MINVAL,RANGE
      REAL XPOS,YPOS
      INTEGER NPTS,NOBS,COUNT
      CHARACTER*30 LABEL,TITLE
C
C    Extract points from the required row or column
C    of the image.
C
      IF (FLAG.EQ.2) THEN
C
C    Column; Slice along the Y direction.
C
        DO J=1,YEXT
          WORK1(J)=IMAGE1(LANE,J)
          WORK2(J)=IMAGE2(LANE,J)
        END DO
        NPTS=YEXT
        TITLE='STRIP PARALLEL TO Y AXIS.'
        LABEL='Y (PIXELS)'
      ELSE
C
C    Row; Slice along the X direction.
C
        DO I=1,XEXT
          WORK1(I)=IMAGE1(I,LANE)
          WORK2(I)=IMAGE2(I,LANE)
        END DO
        NPTS=XEXT
        TITLE='STRIP PARALLEL TO X AXIS.'
        LABEL='X (PIXELS)'
      END IF
C
C    Setup array to hold positions of pixels along the
C    slice.
C
      DO I=1,NPTS
        WORK3(I)=FLOAT(I)
      END DO
C
C    Compute the max. and min. values for the plotting
C    space.
C
      MINVAL=WORK1(1)
      DO I=1,NPTS
        IF (WORK1(I).LT.MINVAL) MINVAL=WORK1(I)
        IF (WORK2(I).LT.MINVAL) MINVAL=WORK2(I)
      END DO
      RANGE=(WORK2(NPTS/2)-MINVAL)*2.0E0
      MAXVAL=MINVAL+(RANGE*1.05E0)
      MINVAL=MINVAL-(RANGE*5.0E-2)
C
C    Set up for plotting to the Versatec.
C
      CALL GDEVI1 (3,.TRUE.)
C
C    Enable clipping.
C
      CALL CLICTL (1)
C
C    Define the plotting space and draw a set of
C    axes.
C
      CALL DRAXIS (0.0E0,FLOAT(NPTS),MINVAL,MAXVAL,
     :             LABEL,'INTENSITY')
C
C    Plot the strip from image 2 as a line.
C
      CALL TYPLO1 (2,0)
      CALL QLOT (WORK3,WORK2,WORKPT,NPTS)
C
C    Since the strips are being extracted through the centre of the
C    image they are likely to pass through the galaxy. Bright
C    points corresponding to the galaxy in the observed image
C    will overflow the fit. These will be removed before
C    plotting.
C
      COUNT=0
      NOBS=NPTS
      DO WHILE (COUNT.LE.NOBS) 
        COUNT=COUNT+1
        IF (WORK1(COUNT).GT.MAXVAL) THEN
          DO I=COUNT+1,NOBS
            WORK1(I-1)=WORK1(I)
            WORK3(I-1)=WORK3(I)
          END DO
          NOBS=NOBS-1
        END IF
      END DO
C
C    Now plot the modified strip from image 1 as points.
C
      CALL TYPLO1 (1,6)
      CALL QLOT (WORK3,WORK1,WORKPT,NOBS)
C
C    Put up the title.
C
      XPOS=2.0E-1*FLOAT(NPTS)
      YPOS=MAXVAL+((MAXVAL-MINVAL)*5.0E-2)
      CALL MOVTO2 (XPOS,YPOS)
      CALL CHAESC ('$.')
      CALL CHAHOL (%REF(TITLE)//'$.')
C
C    Disable clipping and stop plotting.
C
      CALL CLICTL (0)
      CALL STOPLOT
      END
