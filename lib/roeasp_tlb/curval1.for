      SUBROUTINE CURVAL1 (XEXT,YEXT,ARRAY,XPOS,YPOS,VALUE)
C+
C     CURVAL1.
C
C     Return the coordinates and value of a cursor defined
C     pixel from an array.
C
C  Given;
C   XEXT   (I)  X size of the array.
C   YEXT   (I)  Y  "   "   "    "  .
C   ARRAY  (RA) Array.
C
C  Returned;
C   XPOS   (I)  X coord. of selected pixel.
C   YPOS   (I)  Y   "  . "     "       "  .
C   VALUE  (R)  Value    "     "       "  .
C
C  Subroutine called;
C   E2D:-  ARGCUR.
C
C  A C Davenhall./ROE/                                 28/10/82.
C-
      INTEGER XEXT,YEXT,XPOS,YPOS
      REAL ARRAY(XEXT,YEXT)
      REAL VALUE
C
      REAL XPOSR,YPOSR
C
C
      XPOSR=FLOAT(XEXT/2)
      YPOSR=FLOAT(YEXT/2)
C
C    Obtain cursor position.
C
      CALL ARGCUR (XPOSR,YPOSR)
C
C    Find the nearest pixel and obtain its value.
C
      XPOS=NINT(XPOSR)
      YPOS=NINT(YPOSR)
      VALUE=ARRAY(XPOS,YPOS)
C
      END
