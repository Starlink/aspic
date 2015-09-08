      SUBROUTINE MARPOS (IXEXT,IYEXT,PLANE,COLOUR,XPOS,YPOS)
C+
C     MARPOS.
C
C     Subroutine to draw a gunsight around a specified position
C     on the Args.
C
C  Given;
C   IXEXT   (I)  X extent of graphics plane selected.
C   IYEXT   (I)  Y   "    "     "       "      "    .
C   PLANE   (I)  Args overlay plane the graphics are to appear in.
C   COLOUR  (C)  Colour of graphics.
C   XPOS    (R)  X coord. of point to be marked.
C   YPOS    (R)  Y   "  . "    "   "  "    "   .
C
C  Returned;
C   None.
C
C  Subroutines called;
C   E2D:-      SELDE1, GNSGHT.
C   Graphics:- STOPLOT.
C   Fings:-    WINDOL.
C   Args:-     ARGS_NUMIM, ARGS_PTOA.
C
C  A C Davenhall./ROE/                                       7/7/82.
C  A C Davenhall./ROE/ {Modified to use the Args database}  13/2/83.
C-
      INTEGER IXEXT,IYEXT,PLANE
      REAL XPOS,YPOS
      CHARACTER COLOUR*1
C
      REAL RADIUS
      INTEGER IMGNO,PLTSTT,XPIX,YPIX,XARG,YARG
C
C
C    Setup for plotting to the Args.
C
      CALL SELDE1 (2,.FALSE.,PLANE,COLOUR)
      CALL WINDOL (0.0E0,5.11E2,0.0E0,5.11E2)
C
C    Convert from pixel to Args coords.
C
      XPIX=NINT(XPOS)
      YPIX=NINT(YPOS)
      CALL ARGS_NUMIM (IMGNO)
      CALL ARGS_PTOA (IMGNO,XPIX,YPIX,XARG,YARG,PLTSTT)
C
C    Mark the position.
C
      RADIUS=2.0E1
      CALL GNSGHT (FLOAT(XARG),FLOAT(YARG),RADIUS)
C
C    Finish plotting.
C
      CALL STOPLOT
      END
