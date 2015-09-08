      SUBROUTINE GNSGHT (XPOS,YPOS,RADIUS)
C+
C     GNSGHT.                                   {Gunsight}
C
C      Subroutine to draw a "gunsight" symbol to indicate some
C     "target" on a graphics screen.
C
C  Given;
C   XPOS  (R)  X position of target.
C   YPOS  (R)  Y    "     "    "   .
C   RADIUS (R) Outer radius of "gunsight".
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Fings;  MOVTO2, LINTO2, SYMBOL.
C
C  A C Davenhall./ROE                                   18/3/82.
C-
      REAL XPOS,YPOS,RADIUS
C
C
      REAL X1,X2,Y1,Y2,HLFRAD
C
      IF (RADIUS.GT.1.0E-10) THEN
C
C      Sensible radius.
C
        HLFRAD=RADIUS/2.0E0
        X1=XPOS
        X2=XPOS
        Y1=YPOS-RADIUS
        Y2=YPOS-HLFRAD
        CALL MOVTO2 (X1,Y1)
        CALL LINTO2 (X2,Y2)
        Y1=YPOS+HLFRAD
        Y2=YPOS+RADIUS
        CALL MOVTO2 (X1,Y1)
        CALL LINTO2 (X2,Y2)
        X1=XPOS-RADIUS
        X2=XPOS-HLFRAD
        Y1=YPOS
        Y2=YPOS
        CALL MOVTO2 (X1,Y1)
        CALL LINTO2 (X2,Y2)
        X1=XPOS+HLFRAD
        X2=XPOS+RADIUS
        CALL MOVTO2 (X1,Y1)
        CALL LINTO2 (X2,Y2)
      ELSE
C
C      Radius has a silly value. Plot a symbol instead of
C      drawing a gunsight.
C
        CALL MOVTO2 (XPOS,YPOS)
        CALL SYMBOL (6)
      END IF
      END
