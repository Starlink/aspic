      SUBROUTINE CURVAL (XMIN,XMAX,YMIN,YMAX,XCURS,YCURS,
     :                   XCURF,YCURF)
C+
C     CURVAL.
C
C     Subroutine to return the X and Y values of a pair
C     of positions defined using the cross hair cursor.
C     Fings graphics are assumed to be active.
C
C  Given;
C   XMIN   (R)  Minimum X coord. of current plotting space.
C   XMAX   (R)  Maximum "   "  . "     "       "       "  .
C   YMIN   (R)  Minimum "  "   . "     "       "       "  .
C   YMAX   (R)  Maximum "   "  . "     "       "       "  .
C
C  Returned;
C   XCURS  (R)  X coord. of starting cursor position.
C   YCURS  (R)  Y   "  . "     "       "       "    .
C   XCURF  (R)  X   "  . "  finishing  "       "    .
C   YCURF  (R)  Y   "  . "     "       "       "    .
C
C  Subroutines called;
C   Fings:-  MOVTO2, CHAESC, CHAHOL, CURDEF, CURSOR.
C
C  A C Davenhall./ROE/                                    9/12/82
C-
      REAL XMIN,XMAX,YMIN,YMAX,XCURS,YCURS,XCURF,YCURF
C
      REAL XX,YY,XPOS,YPOS
      INTEGER ICOM
C
C
C    Print instructional message.
C
      XX=XMIN+(1.5E-1*(XMAX-XMIN))
      YY=YMAX+((YMAX-YMIN)*5.0E-2)
      CALL MOVTO2 (XX,YY)
      CALL CHAESC ('$')
      CALL CHAHOL (
     : ' S - START  F - FINISH  E - EXIT$.')
C
C    Define the allowed user responses.
C
      CALL CURDEF ('SFE$.')
C
C    Continue until acceptable values are obtained.
C
      ICOM=1
      DO WHILE (ICOM.LT.3)
        CALL CURSOR (ICOM,XPOS,YPOS)
        IF (ICOM.EQ.1) THEN
          XCURS=XPOS
          YCURS=YPOS
        END IF
        IF (ICOM.EQ.2) THEN
          XCURF=XPOS
          YCURF=YPOS
        END IF
      END DO
C
C    Leave current position at a sensible place.
C
      CALL MOVTO2 (XMIN,YMAX)
      CALL CHAHOL (3H $.)
      END
