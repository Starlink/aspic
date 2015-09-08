      SUBROUTINE CMPLOT (MAXPTS,NPTS,X,Y,MAXOBS,NOBS,XOBS,YOBS,
     :                   DEVICE,TITLE,XAXIS,YAXIS)
C+
C     CMPLOT.
C
C     Subroutine to plot a graph showing 2 sets of data.
C     One data set will be shown as a line and the other
C     as points.
C
C  Given;
C   MAXPTS  (I)  Size of arrays for dataset 1, below.
C   NPTS    (I)  No. of points in dataset 1.
C   X       (RA) X values for dataset 1.
C   Y       (RA) Y   "     "     "    1.
C   MAXOBS  (I)  Size of arrays for dataset 2, below.
C   NOBS    (I)  No. of points in dataset 2.
C   XOBS    (RA) X values for dataset 2.
C   YOBS    (RA) Y   "     "     "    2.
C   DEVICE  (I)  Graphics device to be used;
C                = 1 - T4010.
C                = 2 - Args.
C                = 3 - Versatec.
C   TITLE   (C)  Title of the graph.
C   XAXIS   (C)  Label for the X axis.
C   YAXIS   (C)  Label for the Y axis.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Fings:-       MOVTO2, CHAESC, CHAHOL.
C   Graphics:-    WINDO, GDEVI1, DRAXIS, TYPLO1, QLOT, STOPLOT.
C
C  Structure:-
C   Generate plotting extrema from the first dataset.
C      "        "        "     "    "  second   "   .
C   Combine the extrema.
C   Initialise graphics.
C   Draw axes.
C   Plot the first dataset as a line.
C    "    "  second   "    "  points.
C   Put up the title.
C   Stop plotting.
C
C  A C Davenhall./ROE/                             2/8/82.
C-
      INTEGER MAXPTS,NPTS,MAXOBS,NOBS,DEVICE
      REAL X(MAXPTS),Y(MAXPTS),XOBS(MAXOBS),YOBS(MAXOBS)
      CHARACTER*(*) TITLE,XAXIS,YAXIS
C
      REAL XMIN,XMAX,YMIN,YMAX,XOBMIN,XOBMAX,YOBMIN,YOBMAX
      LOGICAL CLEAR
      REAL XPOS,YPOS
C
C
C    Generate extrema from the first dataset.
C
      CALL WINDO (X,Y,MAXPTS,NPTS,XMIN,XMAX,YMIN,YMAX,.FALSE.)
C
C    Generate extrema from the second dataset.
C
      CALL WINDO (XOBS,YOBS,MAXOBS,NOBS,XOBMIN,XOBMAX,
     :            YOBMIN,YOBMAX,.FALSE.)
C
C    Combine the extrema to provide sufficient space to
C    plot both profiles.
C
      XMIN=MIN(XMIN,XOBMIN)
      XMAX=MAX(XMAX,XOBMAX)
      YMIN=MIN(YMIN,YOBMAX)
      YMAX=MAX(YMAX,YOBMAX)
C
C    Initialise the graphics.
C
      CALL GDEVI1 (DEVICE,.TRUE.)
C
C    Setup the range for plotting the selected extrema and
C    draw a set of axes.
C
      CALL DRAXIS (XMIN,XMAX,YMIN,YMAX,XAXIS,YAXIS)
C
C    Select line plotting and plot the first dataset.
C
      CALL TYPLO1 (2,0)
      CALL QLOT (X,Y,MAXPTS,NPTS)
C
C    Depending on the number of points plot the second dataset
C    as either points or a "histogram".
C
      IF (NOBS.LE.60) THEN
        CALL TYPLO1 (1,6)
      ELSE
        CALL TYPLO1 (3,0)
      END IF
      CALL QLOT (XOBS,YOBS,MAXOBS,NOBS)
C
C    Put up the title.
C
      XPOS=XMIN+(2.0E-1*(XMAX-XMIN))
      YPOS=YMAX+(5.0E-2*(YMAX-YMIN))
      CALL MOVTO2 (XPOS,YPOS)
      CALL CHAESC ('$')
      CALL CHAHOL (%REF(TITLE)//'$.')
C
C    Terminate plotting.
C
      CALL STOPLOT
      END
