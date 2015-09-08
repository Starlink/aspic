      SUBROUTINE IRONXA (PLANE,COLOUR,IXEXT,IYEXT,XCEN,YCEN,THETA,PHI,
     :                   A,B)
C+
C     IRONXA.
C
C     Subroutine to draw an "iron cross" cursor on an overlay
C     plane in the ARGS.
C
C  Given;
C   PLANE  (I)  Args overlay plane on which graphics are to appear.
C   COLOUR (C)  Colour for graphics.
C   IXEXT  (I)  X Size of frame in which cross is to be plotted.
C   IYEXT  (I)  Y  "   "    "   "    "     "   "  "  "     "   .
C   XCEN   (R)  X Coord. of centre of cross.
C   YCEN   (R)  Y   "  . "    "    "    "  .
C   THETA  (R)  Angle of the clockmost arm of the leading major
C               axis, above the right pointing horizontal (radians).
C   PHI    (R)  Angle between the 2 arms of a bar (radians).
C   A      (R)  Length of the longest arms.
C   B      (R)  Length of the shortest arms.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   E2D:-      IRONX.
C   Fings:-    ARGS, DEVSPE, VUPORT, WINDOL, CLICTL, STOPLOT.
C   Args:-     ARGS_PICWRT, ARGS_OVWRT, ARGS_OVCOL, ARGS_IVGEN,
C              ARGS_OVCLR.
C
C  A C Davenhall./ROE/                                     1/7/82.
C  A C Davenhall./ROE/  {Modified}                         13/2/83.
C-
      REAL XCEN,YCEN,THETA,PHI,A,B
      INTEGER IXEXT,IYEXT,PLANE
      CHARACTER COLOUR*1
C
      INTEGER PLANEU
C
C
C    Setup for plotting to the ARGS using FINGS.
C
      CALL ARGS
      CALL ARGS_PICWRT
      CALL DEVSPE (9600)
      CALL VUPORT (0.0E0,1.0E0,0.0E0,1.0E0)
C
C    Setup a frame for plotting in Args coords - 0 to 511.
C
      CALL WINDOL (0.0E0,5.11E2,0.0E0,5.11E2)
C
C    Force the selection of a valid overlay plane.
C
      IF (PLANE.GE.8.AND.PLANE.LE.15) THEN
        PLANEU=PLANE
      ELSE
        PLANEU=8
      END IF
C
C    Setup for plotting to selected plane.
C
      CALL ARGS_OVWRT (PLANEU)
      CALL ARGS_OVCOL (PLANEU,COLOUR)
      CALL ARGS_OVGEN ('W')
C
C    Clear the picture plane, ready to draw the cursor.
C
      CALL ARGS_OVCLR (PLANEU)
C
C    Enable clipping.
C
      CALL CLICTL (1)
C
C    Draw the iron cross.
C
      CALL IRONX (XCEN,YCEN,THETA,PHI,A,B)
C
C    Disable clipping and finish plotting with FINGS.
C
      CALL CLICTL (0)
      CALL STOPLOT
      END
