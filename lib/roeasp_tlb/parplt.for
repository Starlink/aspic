      SUBROUTINE PARPLT (DEVICE,CLEAR,PARMS,MAXCNT,ELLPAR,
     :                   XARRAY,YARRAY)
C+
C     PARPLT.
C
C     Subroutine to allow the user to plot any one
C     of the parameters determined for a series of ellipses
C     fitted to contours extracted from an elliptical galaxy
C     against any other such parameter.
C
C  Given;
C   DEVICE (I)  Graphics device selected.
C   CLEAR  (L)  Flag determining whether or not the graphics
C               device is to be cleared:
C               = .TRUE.  - Clear device.
C               = .FALSE. - Do not clear device.
C   PARMS  (I)  No. of perameters determined for each ellipse
C               (must be .ge. 6).
C   MAXCNT (I)  Max. permitted no. of ellipses.
C   ELLPAR (RA) Array holding parameters for fitted ellipses.
C
C  Used;
C   XARRAY (RA) Work array, size = MAXCNT.
C   YARRAY (RA)  "     "  ,  "   =   "   .
C
C  Returned;
C   None.
C
C  Structure:-
C   Obtain parameter required as X axis from the user.
C     "       "         "     "  Y  "    "    "   "  .
C   Copy the appropriate parameter arrays into the work arrays.
C   Set the appropriate axis labels.
C   plot the graph.
C
C  A C Davenhall./ROE/                               24/9/82.
C-
      INTEGER DEVICE,PARMS,MAXCNT
      REAL ELLPAR(PARMS,MAXCNT)
      REAL XARRAY(MAXCNT),YARRAY(MAXCNT)
      LOGICAL CLEAR
C
      CHARACTER REPLY*20,LABELX*15,LABELY*15,PARAMS(6)*15
      INTEGER IOSTAT
      INTEGER XAXIS,YAXIS
C
C    Plotting symbol to be used - a diamond.
C
      INTEGER CHAR
      PARAMETER (CHAR=6)
C
C
C    Obtain the required X axis from the user.
C
      IOSTAT=0
      CALL MULREP (' Select the required variable for the X axis;',
     : 'LOGI,XCENTRE,YCENTRE,MAJAXIS,ELLIPT,ORIENT$',
     :  REPLY,IOSTAT)
      IF (REPLY.EQ.'LOGI')        XAXIS=1
      IF (REPLY.EQ.'XCENTRE')     XAXIS=2
      IF (REPLY.EQ.'YCENTRE')     XAXIS=3
      IF (REPLY.EQ.'MAJAXIS')     XAXIS=4
      IF (REPLY.EQ.'ELLIPT')      XAXIS=5
      IF (REPLY.EQ.'ORIENT')      XAXIS=6
C
C    Obtain the required Y axis from the user.
C
      CALL MULREP (' Select the required variable for the Y axis;',
     : 'LOGI,XCENTRE,YCENTRE,MAJAXIS,ELLIPT,ORIENT$',
     :  REPLY,IOSTAT)
      IF (REPLY.EQ.'LOGI')        YAXIS=1
      IF (REPLY.EQ.'XCENTRE')     YAXIS=2
      IF (REPLY.EQ.'YCENTRE')     YAXIS=3
      IF (REPLY.EQ.'MAJAXIS')     YAXIS=4
      IF (REPLY.EQ.'ELLIPT')      YAXIS=5
      IF (REPLY.EQ.'ORIENT')      YAXIS=6
C
C    Copy the appropriate parameter arrays into the 
C    work arrays.
C
      DO I=1,MAXCNT
        XARRAY(I)=ELLPAR(XAXIS,I)
        YARRAY(I)=ELLPAR(YAXIS,I)
      END DO
C
C    Set up the varius choices for the axis labels.
C
      PARAMS(1)='LOG I'
      PARAMS(2)='X CENTRE'
      PARAMS(3)='Y CENTRE'
      PARAMS(4)='MAJOR AXIS'
      PARAMS(5)='ELLIPTICITY'
      PARAMS(6)='ORIENTATION'
C
C    Adopt the choices selected for this plot.
C
      LABELX=PARAMS(XAXIS)
      LABELY=PARAMS(YAXIS)
C
C    Plot the selected graph.
C
      CALL AGRAPH (XARRAY,YARRAY,MAXCNT,MAXCNT,DEVICE,CLEAR,
     :             1,CHAR,LABELX,LABELY,'SCATTERGRAM')
      END
