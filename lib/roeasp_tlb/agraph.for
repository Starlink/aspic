      SUBROUTINE AGRAPH (X,Y,SIZE,POINTS,DEVICE,CLEAR,TYPE,CHAR,
     :                   LABELX,LABELY,TITLE)
C+
C      AGRAPH.
C
C      Subroutine to produce a plot of the numbers in one real
C      array against the numbers in another, complete with
C      axes, titles etc. The details of the plot are under
C      programmer control.
C
C  Given;
C  X         (RA)  Array of X values.
C  Y         (RA)    "   "  Y   "   .
C  SIZE      (I)   Size of arrays X & Y.
C  POINTS    (I)   No. of points in arrays X & Y.
C  DEVICE    (I)   Graphics device selected;
C                  = 1 - T4010.
C                  = 2 - Args.
C                  = 3 - Versatec.
C  CLEAR     (B)   Determines whether or not to clear the screen.
C                  = .TRUE. - Clear screen.
C                  = .FALSE. - Don't clear screen.
C  TYPE      (I)   Determines type of plot produced;
C                  = 1 - Plot points as points.
C                  = 2 - Join points with straight lines.
C                  = 3 - Join points with "histograms".
C  CHAR      (I)   If TYPE = 1 Determines the plotting symbol used.
C                  Valid range = 0-8. See Fings manual for details.
C  LABELX    (C)   Label for X axis.
C  LABELY    (C)     "    "  Y  "  .
C  TITLE     (C)   Title for graph.
C
C  Returned; -
C
C  Subroutines called;
C  Graphics: GDEVI1, TYPLO1, WINDO, DRAXIS, QLOT, STOPLOT.
C  Fings:    CHAESC, CHAHOL, MOVTO2.
C
C     A C Davenhall./ROE/                                  25/2/82.
C-
      INTEGER SIZE,POINTS,DEVICE,TYPE,CHAR
      REAL X(SIZE),Y(SIZE)
      LOGICAL CLEAR
      CHARACTER LABELX*(*),LABELY*(*),TITLE*(*)
C
      INTEGER NPTS
      REAL XMIN,XMAX,YMIN,YMAX,XPOS,YPOS
C
C      Force tthe number of points to be less than the array size.
C
      NPTS=MIN(POINTS,SIZE)
C
C      Initialise plotting.
C
      CALL GDEVI1 (DEVICE,CLEAR)
      CALL TYPLO1 (TYPE,CHAR)
C
C      Find extrema & draw axes.
C
      CALL WINDO (X,Y,SIZE,NPTS,
     :            XMIN,XMAX,YMIN,YMAX,.FALSE.)
      CALL DRAXIS (XMIN,XMAX,YMIN,YMAX,LABELX,LABELY)
C
C      Plot points.
C
      CALL QLOT (X,Y,SIZE,NPTS)
C
C      Put up a title.
C
      XPOS=XMIN+(2.0E-1*(XMAX-XMIN))
      YPOS=YMAX+((YMAX-YMIN)*0.05)
      CALL MOVTO2 (XPOS,YPOS)
      CALL CHAESC ('$')
      CALL CHAHOL (%REF(TITLE)//'$.')
C
C      Stop ploting.
C
      CALL STOPLOT
      END
