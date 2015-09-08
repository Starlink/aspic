      SUBROUTINE HEATCOL1 (NCOL,NLEVEL,COLOUR)
C+
C     HEATCOL1.
C
C     Subroutine to generate a "heat sequence"
C     colour table.
C
C  Given;
C   NCOL   (I)  No. of colour guns.
C   NLEVEL (I)  No. of intensity levels in each gun.
C
C  Returned;
C   COLOUR (IA) Colour table evaluated for all intensity
C               levels of all guns.
C
C  Subroutines called;
C   E2D:-  SETCOL.
C 
C  A C Davenhall./ROE/                          15/2/82.
C  A C Davenhall./ROE/    {Modified}            26/10/82.
C-
      INTEGER NCOL,NLEVEL
      INTEGER COLOUR(NCOL,NLEVEL)
C
C    Setup colour table.
C
C    Black.
C
      CALL SETCOL (1,23,0,0,0,COLOUR)
C
C    Violet/black.
C
      CALL SETCOL (24,47,128,0,128,COLOUR)
C
C    Violet.
C
      CALL SETCOL (48,70,255,0,255,COLOUR)
C
C    Red/violet.
C
      CALL SETCOL (71,93,255,0,128,COLOUR)
C
C    Red.
C
      CALL SETCOL (94,116,255,0,0,COLOUR)
C
C    Red/orange.
C
      CALL SETCOL (117,140,255,64,0,COLOUR)
C
C    Orange.
C
      CALL SETCOL (141,164,255,128,0,COLOUR)
C
C    Orange/yellow.
C
      CALL SETCOL (165,187,255,192,0,COLOUR)
C
C    Yellow.
C
      CALL SETCOL (188,210,255,255,0,COLOUR)
C
C    Yellow/white.
C
      CALL SETCOL (211,233,255,255,128,COLOUR)
C
C    White.
C
      CALL SETCOL (234,256,255,255,255,COLOUR)
C
      END
