      SUBROUTINE IRONX (XCEN,YCEN,THETA,PHI,A,B)
C+
C     IRONX.
C
C     Subroutine to draw an "iron cross" using Fings.
C
C  Given;
C   XCEN   (R)  X Coord. of centre.
C   YCEN   (R)  Y   "  . "    "   .
C   THETA  (R)  Angle of clockmost arm of leading major axis,
C               above the right pointing horizontal (radians).
C   PHI    (R)  Angle between the 2 branches of an arm (radians).
C   A      (R)  Length of the longest arm (major axis).
C   B      (R)    "    "   "  shortest "  (minor  "  ).
C
C  Returned;
C   None.
C
C  Subroutines called;
C   E2D;   DRAARM.
C   Args;  ARGS_NUMIM, ARGS_PTOA.
C
C  A C Davenhall./ROE/                                        1/7/82.
C  A C Davenhall./ROE/ {Modified to use Args database}        13/2/83.
C-
      REAL XCEN,YCEN,THETA,PHI,A,B
C
      REAL RHTGLE,THETAA,LEN
      INTEGER IMGNO,PLTSTT,X1PIX,Y1PIX,X1ARG,Y1ARG,
     :        XCPIX,YCPIX,XCARG,YCARG
C
C    A right angle expressed in radians.
C
      PARAMETER (RHTGLE=1.57080E0)
C
C
C    Draw each arm in turn.
C
      DO I=1,4
C
C    Compute theta for each arm.
C
        THETAA=THETA+(RHTGLE*FLOAT(I-1))
C
C    Choose appropriate length of arm.
C    (corresponds to major or minor axis).
C
        IF (I.EQ.1.OR.I.EQ.3) THEN
          LEN=A
        ELSE
          LEN=B
        END IF
C
C    Draw the arm.
C
        CALL DRAARM (XCEN,YCEN,THETAA,PHI,LEN)
      END DO
C
C    Draw the leading major axis...
C
      X1=XCEN+(A*COS(THETA+(PHI/2.0E0)))
      Y1=YCEN+(A*SIN(THETA+(PHI/2.0E0)))
C
C    ...convert from pixel to Args coords.
C
      CALL ARGS_NUMIM (IMGNO)
      X1PIX=NINT(X1)
      Y1PIX=NINT(Y1)
      XCPIX=NINT(XCEN)
      YCPIX=NINT(YCEN)
      CALL ARGS_PTOA (IMGNO,X1PIX,Y1PIX,X1ARG,Y1ARG,PLTSTT)
      CALL ARGS_PTOA (IMGNO,XCPIX,YCPIX,XCARG,YCARG,PLTSTT)
C
C    ...draw the resultant line.
C
      CALL MOVTO2 (FLOAT(XCARG),FLOAT(YCARG))
      CALL LINTO2 (FLOAT(X1ARG),FLOAT(Y1ARG))
      END
