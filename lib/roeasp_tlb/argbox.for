      SUBROUTINE ARGBOX (IX1,IY1,IX2,IY2)
C+
C     ARGBOX.
C
C     Subroutine to draw a box defined by integer coords.
C     on the Args using FINGS graphics.
C
C
C  Given;
C   IX1  (I)  X Coord. of lower left hand corner of the box.
C   IY1  (I)  Y   "  . "    "    "    "     "    "   "   " .
C   IX2  (I)  X   "  . "  upper right "     "    "   "   " .
C   IY2  (I)  Y   "  . "    "    "    "     "    "   "   " .
C
C  Returned;
C   None.
C
C  Subroutines called;
C   E2D:-       ARGSET.
C   Graphics:-  DRABOX.
C   Fings:-     DEVEND.
C   Args:-      ARGS_OVWRT, ARGS_OVCOL, ARGS_OVGEN.
C
C  A C Davenhall./ROE/    {Original}      6/10/81.
C  A C Davenhall./ROE/    {Modified}      14/7/82.
C  A C Davenhall./ROE/    {   "    }      14/8/82.
C-
      INTEGER IX1,IY1,IX2,IY2
C
      INTEGER PLANE
      PARAMETER (PLANE=8)
      CHARACTER COLOUR*1
      PARAMETER (COLOUR='W')
C
C
      CALL DEVEND
      CALL ARGSET (0)
      CALL ARGS_OVWRT (PLANE)
      CALL ARGS_OVCOL (PLANE,COLOUR)
      CALL ARGS_OVGEN ('W')
      CALL DRABOX (IX1,IY1,IX2,IY2)
      CALL DEVEND
      END
