      SUBROUTINE PGRAPH (MAXPTS,NPTS,X,Y,LABELX,LABELY,
     :                   TITLE)
C+
C     PGRAPH.
C
C     Subroutine to plot a graph from two real arrays, the
C     output device and form of the plot being interactively
C     selected by the user.
C
C  Given;
C   MAXPTS  (I)  Size of arrays to hold points (below).
C   NPTS    (I)  No. of points in datasets.
C   X       (RA) X values for the data.
C   Y       (RA) Y   "     "   "   "  .
C   LABELX  (C)  Label for X axis.
C   LABELY  (C)    "    "  Y  "  .
C   TITLE   (C)  Title for graph.
C
C  Returned;
C   None.
C
C  Subroutines called:-
C   INTERFACES;   MULREP, YESNO, OUTPUT.
C   E2D;          TYPLO2, AGRAPH
C
C  STRUCTURE:-
C   Select graphics device.
C   Select type of plotting.
C   Plot the graph.
C
C  A C Davenhall./ROE/                           6/8/82.
C-
      INTEGER MAXPTS,NPTS
      REAL X(MAXPTS),Y(MAXPTS)
      CHARACTER LABELX*(*),LABELY*(*),TITLE*(*)
C
      INTEGER DEVICE,TYPE,CHAR,PTS
      INTEGER IOSTAT
      LOGICAL CLEAR
      CHARACTER REPLY*15
C
C
C    Force the number of points to be sensible.
C
      PTS=MIN(NPTS,MAXPTS)
C
C    Determine the graphics device to be used.
C
      IOSTAT=0
      CALL MULREP (
     : ' Enter the type of graphics device required;',
     : 'T4010,ARGS,VERSATEC$',REPLY,IOSTAT)
      IF (REPLY.EQ.'T4010')     DEVICE=1
      IF (REPLY.EQ.'ARGS')      DEVICE=2
      IF (REPLY.EQ.'VERSATEC')  DEVICE=3
C
C    Determine whether or not the screen is to be 
C    cleared, unless the Versatec is being used,
C    in which case it automatically will be.
C
      IF (DEVICE.NE.3) THEN
        CALL YESNO (' Is the screen to be cleared?',
     :              'Y',REPLY,IOSTAT)
        IF (REPLY.EQ.'Y') THEN
          CLEAR=.TRUE.
        ELSE
          CLEAR=.FALSE.
        END IF
      ELSE
        CLEAR=.TRUE.
      END IF
C
C    Determine the type of plotting.
C
      CALL TYPLO2 (TYPE,CHAR)
C
C    Plot the graph.
C
      CALL AGRAPH (X,Y,MAXPTS,PTS,DEVICE,CLEAR,
     :             TYPE,CHAR,LABELX,LABELY,TITLE)
      END
