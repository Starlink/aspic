      SUBROUTINE LABARM (PLANE,COLOUR,IXEXT,IYEXT,XCEN,YCEN,
     :                   THETA,PHI,A,B)
C++
C     LABARM.
C
C     Subroutine to label the arms of an "iron cross"
C     pseudo-cursor on the ARGS.
C
C  Given;
C   PLANE   (I)  Overlay plane to which the numbers are to be
C                written.
C   COLOUR  (C)  The colour in which the numbers are to appear.
C   IXEXT   (I)  X extent of the frame to be plotted.
C   IYEXT   (I)  Y   "    "   "    "   "  "     "   .
C   XCEN    (R)  X coord. of the centre of the cross.
C   YCEN    (R)  Y   "  . "   "    "    "   "    "  .
C   THETA   (R)  Angle of the clockmost arm of the leading
C                major axis, above the right pointing
C                horizontal (radians).
C   PHI     (R)  Angle between the 2 branches of an arm (radians).
C   A       (R)  Length of longest arm (marjor axis).
C   B       (R)    "    "  shortest "  (minor   "  ).
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Fings:-  ARGS, DEVSPE, VUPORT, WINDOL, MOVTO2, CHAINT.
C   Args:-   ARGS_OVWRT, ARGS_OVCOL, ARGS_OVGEN, ARGS_NUMIM,
C            ARGS_PTOA.
C   Graphics:- STOPLOT.
C
C  A C Davenhall./ROE/                                     3/7/82.
C  A C Davenhall./ROE/ {modified to use the Args database} 13/2/83.
C-
      INTEGER PLANE,IXEXT,IYEXT
      REAL XCEN,YCEN,THETA,PHI,A,B
      CHARACTER COLOUR*1
C
      INTEGER PLANEU
      REAL RHTGLE,THETAA,LEN,EXTRA,X1,Y1
      INTEGER IMGNO,PLTSTT,X1PIX,Y1PIX,X1ARG,Y1ARG
C
C    A right angle expressed in radians.
C
      PARAMETER (RHTGLE=1.57080E0)
C
C
C    Set up for plotting to the ARGS using FINGS.
C
      CALL ARGS
      CALL ARGS_PICWRT
      CALL DEVSPE (9600)
      CALL VUPORT (0.0E0,1.0E0,0.0E0,1.0E0)
C
C    Set up for plotting in Args coords - 0 to 511.
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
C    Find the number of the last image plotted on the Args.
C
      CALL ARGS_NUMIM (IMGNO)
C
C    Setup for plotting to the selected plane.
C
      CALL ARGS_OVWRT (PLANEU)
      CALL ARGS_OVCOL (PLANEU,COLOUR)
      CALL ARGS_OVGEN ('W')
C
C    Compute the distance beyond the end of each arm that
C    the number is to be plotted.
C
      EXTRA=A*2.0E-1
C
C    Number each arm.
C
      DO I=1,4
C
C    Compute the value of theta appropriate to each arm.
C
        THETAA=THETA+(RHTGLE*FLOAT(I-1))
C
C    Compute the appropriate distance from the centre for each
C    number, depending on whether the arm is a major
C    or minore axis.
C
        IF (I.EQ.1.OR.I.EQ.3) THEN
          LEN=A
        ELSE
          LEN=B
        END IF
        LEN=LEN+EXTRA
C
C    Compute the coordinates of the arm.
C
        X1=XCEN+(LEN*COS(THETAA+(PHI/2.0E0)))
        Y1=YCEN+(LEN*SIN(THETAA+(PHI/2.0E0)))
C
C    Convert from pixel to Args coords.
C
        X1PIX=NINT(X1)
        Y1PIX=NINT(Y1)
        CALL ARGS_PTOA (IMGNO,X1PIX,Y1PIX,X1ARG,Y1ARG,PLTSTT)
C
C    Draw the number.
C
        CALL MOVTO2 (FLOAT(X1ARG),FLOAT(Y1ARG))
        CALL CHAINT (I,1)
      END DO
C
C    Terminate plotting with FINGS.
C
      CALL STOPLOT
      END
