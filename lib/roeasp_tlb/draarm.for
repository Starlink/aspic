      SUBROUTINE DRAARM (XCEN,YCEN,THETAS,PHI,LEN)
C+
C     DRAARM.
C
C     Subroutine to draw one arm of an "iron cross",
C     for use in the "pseudo-cursor" graphics associated
C     with the major and minor axis extraction routine.
C
C  Given;
C   XCEN   (R)  X Coord. of the centre of the cross.
C   YCEN   (R)  Y   "  . "   "    "    "   "    "  .
C   THETAS (R)  Angle of the clockmost arm above the right
C               pointing horizontal (radians).
C   PHI    (R)  Angle between the bars of an arm (radians).
C   LEN    (R)  Length of either arm.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Fings:-    MOVTO2, LINTO2.
C   Args:-     ARGS_NUMIM, ARGS_PTOA.
C
C  A C Davenhall./ROE/                                     1/7/82.
C  A C Davenhall./ROE/ {Modified to use the Args database} 13/2/83.
C-
      REAL XCEN,YCEN,THETAS,PHI,LEN
C
      REAL X1,Y1,X2,Y2
      INTEGER IMGNO,PLTSTT,X1PIX,Y1PIX,X1ARG,Y1ARG,
     :        X2PIX,Y2PIX,X2ARG,Y2ARG,XCPIX,YCPIX,XCARG,YCARG
C
C    Compute the position of the most clockwise arm.
C
      X1=XCEN+(LEN*COS(THETAS))
      Y1=YCEN+(LEN*SIN(THETAS))
C
C    Compute the position of the least clockwise arm.
C
      X2=XCEN+(LEN*COS(THETAS+PHI))
      Y2=YCEN+(LEN*SIN(THETAS+PHI))
C
C    Convert from pixel to Args coords.
C
      CALL ARGS_NUMIM (IMGNO)
      X1PIX=NINT(X1)
      Y1PIX=NINT(Y1)
      X2PIX=NINT(X2)
      Y2PIX=NINT(Y2)
      XCPIX=NINT(XCEN)
      YCPIX=NINT(YCEN)
      CALL ARGS_PTOA (IMGNO,X1PIX,Y1PIX,X1ARG,Y1ARG,PLTSTT)
      CALL ARGS_PTOA (IMGNO,X2PIX,Y2PIX,X2ARG,Y2ARG,PLTSTT)
      CALL ARGS_PTOA (IMGNO,XCPIX,YCPIX,XCARG,YCARG,PLTSTT)
C
C    Draw the sector.
C
      CALL MOVTO2 (FLOAT(XCARG),FLOAT(YCARG))
      CALL LINTO2 (FLOAT(X1ARG),FLOAT(Y1ARG))
      CALL LINTO2 (FLOAT(X2ARG),FLOAT(Y2ARG))
      CALL LINTO2 (FLOAT(XCARG),FLOAT(YCARG))
      END
