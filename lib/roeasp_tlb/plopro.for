      SUBROUTINE PLOPRO (AXIS,AXISIZ,PTS,PIXSIZ,PLANE,COLOUR,
     :                   WORK1,WORK2)
C+
C     PLOPRO.
C
C     Subroutine to plot one of the four extracted profiles
C     on a user selected device.
C
C  Given;
C   AXIS   (RA)  Array containing all four extracted profiles.
C   AXISIZ (I)   Max. permitted no. of points in any axis.
C   PTS    (IA)  No. of points in each axis.
C   PIXSIZ (R)   Size of each pixel.
C   PLANE  (I)   Overlay plane into which the graphics
C                are to be written if the Args is selected.
C   COLOUR (C)   Colour of the graphics if the Args is selected.
C
C  Used;
C   WORK1  (RA)  Work array.
C   WORK2  (RA)   "     "  .
C
C  Subroutines called;
C   Graphics:-    SELDE1, TYPLO1, WINDO, DRAXIS,
C                 QLOT, STOPLOT.
C   Fings:-       MOVTO2, CHAESC, CHAHOL.
C   Interfaces:-  READI, MULREP.
C
C  A C Davenhall./ROE/                                6/7/82.
C-
      INTEGER AXISIZ,PLANE
      INTEGER PTS(4)
      REAL AXIS(AXISIZ,4),WORK1(AXISIZ),WORK2(AXISIZ)
      CHARACTER COLOUR*1
      REAL PIXSIZ
C
      INTEGER STATUS,NPTS,ARM,DEVICE
      CHARACTER REPLY*15,BUFFER*30
      REAL XMIN,XMAX,YMIN,YMAX,XPOS,YPOS
C
C
C    Determine which of the extracted axes is to be plotted.
C
      STATUS=0
      CALL READI ('ARM',
     :  ' Enter the no. of the axis required;',
     :    1,1,4,ARM,STATUS)
C
C    Copy the points for the selected axis into the work array
C    and compute the corresponding radial distance.
C
      NPTS=PTS(ARM)
      NPTS=MIN(NPTS,AXISIZ)
      DO I=1,NPTS
        WORK1(I)=PIXSIZ*FLOAT(I-1)
        WORK2(I)=AXIS(I,ARM)
      END DO
C
C    Select required output device.
C
      CALL MULREP (' Graphics device for output;',
     :  'T4010,ARGS,VERSATEC$',REPLY,STATUS)
      IF (REPLY.EQ.'T4010')     DEVICE=1
      IF (REPLY.EQ.'ARGS')      DEVICE=2
      IF (REPLY.EQ.'VERSATEC')  DEVICE=3
C
C    Initialise plotting.
C
      CALL SELDE1 (DEVICE,.FALSE.,PLANE,COLOUR)
      CALL TYPLO1 (3,1)
C
C    Find extrema and draw axes.
C
      CALL WINDO (WORK1,WORK2,AXISIZ,NPTS,
     :            XMIN,XMAX,YMIN,YMAX,.FALSE.)
      CALL DRAXIS (XMIN,XMAX,YMIN,YMAX,
     :            'RADIUS','INTENSITY')
C
C    Plot the points.
C
      CALL QLOT (WORK1,WORK2,AXISIZ,NPTS)
C
C    Put up the title.
C
      XPOS=XMIN+(2.0E-1*(XMAX-XMIN))
      YPOS=YMAX+((YMAX-YMIN)*5.0E-2)
      CALL MOVTO2 (XPOS,YPOS)
      CALL CHAESC ('$')
      WRITE (BUFFER,2000) ARM
 2000 FORMAT(1X,'EXTRACTED PROFILE NO.',I2)
      CALL CHAHOL (%REF(BUFFER//'$.'))
C
C    Stop plotting.
C
      CALL STOPLOT
      CALL ARGS_PICWRT
      END
