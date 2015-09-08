      SUBROUTINE RECUR (PLANE1,COL1,PLANE2,COL2,XBASE,YBASE,XTOP,
     :                  YTOP,STATUS)
C+
C     RECUR.
C
C     Subroutine to return the coords. (in pixels) of a rectangular
C     box drawn on the Args. The box is defined using the
C     cross-hair cursor.
C
C  Given;
C   PLANE1 (I)  Args overlay plane to be used by the tentative
C               box. note; any graphics in this plane will
C               be cleared.
C   COL1   (C)  Colour of the tentative box.
C   PLANE2 (I)  Args overlay plane to be used by the accepted
C               box.
C   COL2   (C)  Colour of the accepted box.
C
C  Returned;
C   XBASE  (I)  X coord of lower left hand corner of box.
C   YBASE  (I)  Y   "   "    "    "    "     "    "   " .
C   XTOP   (I)  X   "   "  upper right "     "    "   " .
C   YTOP   (I)  Y   "   "    "    "    "     "    "   " .
C   STATUS (I)  Max(input status, internal status).
C               Internal status=0 for a successful return, otherwise
C               non-zero.
C
C  Subroutines called;
C   Interfaces:- OUTPUT, YESNO.
C   Fings:-      DEVEND, ARGS, ARGS_PICWRT, DEVSPE, VUPORT, WINDOL,
C                MOVTO2, SYMBOL.
C   Args:-       ARGS_OVWRT, ARGS_OVCOL, ARGS_OVGEN, ARGCUR,
C                ARGS_NUMIM, ARGS_ATOP, ARGS_PTOA.
C   E2D:-        ARGREC.
C
C  Structure:-
C   Set up a window to fill the Args in Args units.
C   Put up the cross hair cursor.
C   Put up the cross hair cursor.
C   If valid box
C     Set up for using the tentative overlay plane.
C     Display the tentative box.
C     If box Ok by the user
C       Set return coords.
C       Clear the tentative overlay plane.
C       Set up for using the accepted overlay plane.
C       draw box
C     else
C       Clear the tentative overlay plane.
C       set return status
C     end if
C   else
C     print message saying invalid box.
C     set return status.
C   end if
C
C  A C Davenhall./ROE/                                19/3/82.
C  A C Davenhall./ROE/ {Modified}                     10/7/83.
C-
      IMPLICIT NONE
C
      INTEGER PLANE1,PLANE2,XBASE,YBASE,XTOP,YTOP,STATUS
      CHARACTER COL1*(*),COL2*(*)
C
      REAL X1,Y1,X2,Y2,XPOS,YPOS
      CHARACTER REPLY*1
      INTEGER IOSTAT,INSTAT,DRSTAT,TESTAT,ASTAT
      INTEGER IX1,IY1,IX2,IY2,ARGIMG,IXA,IYA
C
C
      INSTAT=0
      DRSTAT=0
      TESTAT=0
      XBASE=0
      YBASE=0
      XTOP=0
      YTOP=0
C
C    Put up the cross-hair cursor twice to obtain the
C    the two extrema of the required rectangle.
C    The values are returned in pixel coords.
C
      CALL OUTPUT (' Define corners of the required rectangle.',
     :               IOSTAT)
C
C    Give the cursor a starting position in the middle of
C    the screen. This is done by using the Args database
C    to convert the middle of the screen in Args coords.
C    into pixel coords. and giving the result as
C    input Arguments to ARGCUR.
C
      CALL ARGS_NUMIM (ARGIMG)
      CALL ARGS_ATOP (ARGIMG,255,255,IX1,IY1,ASTAT)
      X1=FLOAT(IX1)
      Y1=FLOAT(IY1)
      CALL ARGCUR (X1,Y1)
C
C    Draw a cross indicating to the user where the first corner
C    has been placed.
C    ...Initialise Fings.
C
      CALL DEVEND
      CALL ARGS
      CALL ARGS_PICWRT
      CALL DEVSPE (9600)
      CALL VUPORT (0.0E0,1.0E0,0.0E0,1.0E0)
      CALL WINDOL (0.0E0,5.11E2,0.0E0,5.11E2)
C
C    Set up for using the tentative overlay plane.
C
      CALL ARGS_OVWRT (PLANE1)
      CALL ARGS_OVCOL (PLANE1,COL1)
      CALL ARGS_OVGEN ('W')
      IX1=NINT(X1)
      IY1=NINT(Y1)
      CALL ARGS_PTOA (ARGIMG,IX1,IY1,IXA,IYA,ASTAT)
      XPOS=FLOAT(IXA)
      YPOS=FLOAT(IYA)
      CALL MOVTO2 (XPOS,YPOS)
      CALL SYMBOL (3)
      CALL DEVEND
C
      X2=X1
      Y2=Y1
      CALL ARGCUR (X2,Y2)
C
C    Determine if the box is valid or not.
C
      IF (ABS(X1-X2).GT.1.0E-4.AND.ABS(Y1-Y2).GT.1.0E-4) THEN
C
C    Initialise Fings.
C
        CALL DEVEND
        CALL ARGS
        CALL ARGS_PICWRT
        CALL DEVSPE (9600)
        CALL VUPORT (0.0E0,1.0E0,0.0E0,1.0E0)
        CALL WINDOL (0.0E0,5.11E2,0.0E0,5.11E2)
C
C    Set up for using the tentative overlay plane.
C
        CALL ARGS_OVWRT (PLANE1)
        CALL ARGS_OVCOL (PLANE1,COL1)
        CALL ARGS_OVGEN ('W')
C
C    Display the tentative box.
C
        IX1=IFIX(MIN(X1,X2))
        IY1=IFIX(MIN(Y1,Y2))
        IX2=IFIX(MAX(X1,X2))
        IY2=IFIX(MAX(Y1,Y2))
        CALL ARGREC (IX1,IY1,IX2,IY2,TESTAT)
        CALL DEVEND
C
C    Check whether the user is satisfied.
C
        CALL YESNO (' Is the box acceptable?','Y',REPLY,IOSTAT)
        IF (REPLY.EQ.'Y') THEN
          CALL ARGS_OVCLR (PLANE1)
C
C    Set the return arguments.
C
          XBASE=IX1
          YBASE=IY1
          XTOP=IX2
          YTOP=IY2
C    Initialise fings.
C
          CALL ARGS
          CALL ARGS_PICWRT
          CALL DEVSPE (9600)
          CALL VUPORT (0.0E0,1.0E0,0.0E0,1.0E0)
          CALL WINDOL (0.0E0,5.11E2,0.0E0,5.11E2)
C
C    Set up for drawing in the accepted overlay plane.
C
          CALL ARGS_OVWRT (PLANE2)
          CALL ARGS_OVCOL (PLANE2,COL2)
          CALL ARGS_OVGEN ('W')
C
C    Draw the box.
C
          CALL ARGREC (XBASE,YBASE,XTOP,YTOP,DRSTAT)
          CALL DEVEND
        ELSE
C
C    User not satisfied with the box. Set the return status.
C
          CALL ARGS_OVCLR (PLANE1)
          INSTAT=1
        END IF
      ELSE
C
C    An invalid box.
C
        CALL ARGS_OVCLR (PLANE1)
        CALL OUTPUT (' ***ERROR Invalid box.',IOSTAT)
        INSTAT=2
      END IF
C
C    Set the return status.
C
      STATUS=MAX(STATUS,INSTAT,DRSTAT,TESTAT)
      END
