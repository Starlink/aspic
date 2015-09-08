      SUBROUTINE IRONX1 (IXEXT,IYEXT,PLANE,COLOUR,XCEN,YCEN,THETA,PHI,
     :                   A,B)
C+
C     IRONX1.
C
C     Subroutine to produce an "iron cross" pseudo-cursor on the Args
C     constrained to be centred on a particular
C     position.
C
C  Given;
C   IXEXT  (I)  X size of frame in which the cross is to be drawn.
C   IYEXT  (I)  Y  "   "    "   "    "    "    "   "  "  "    "  .
C   PLANE  (I)  Args overlay plane on which the cross
C               is to appear (10-15).
C   COLOUR (C)  Colour of cross and labels.
C   XCEN   (R)  X Coord. of centre of cross.
C   YCEN   (R)  Y   "  . "    "    "    "  .
C   THETA  (R)  Angle of the clockmost arm of the leading major
C               axis, above the right pointing horizontal
C               (radians).
C    PHI   (R)  Angle between the 2 branches of an arm (radians).
C    A     (R)  Length of longest arm (major axis).
C    B     (R)    "    "  shortest "  (minor  "  ).
C
C  Returned;
C   None.
C
C  Subroutines called;
C   E2D:-  IRONXA, IRONX2, LABARM.
C   Args:- BLABELS, ARGS_USRCUR, ARGS_OVCLR,
C          ARGS_PICWRT.
C
C  Note; Overlay planes 8 & 9 are used internally by this
C        routine and the cursor should be restricted to
C        planes 10 - 15.
C
C  A C Davenhall./ROE/                             1/7/82.
C-
      REAL XCEN,YCEN,THETA,PHI,A,B
      INTEGER IXEXT,IYEXT,PLANE
      CHARACTER COLOUR*1
C
      LOGICAL MORE,CHAOPT,ACCEPT
      CHARACTER*10 OPTION(5),THISOP,BUFF
      INTEGER OPT,XPOS,YPOS,XOUT,YOUT,BUTT,MAXOPT,INDEX
C
      INTEGER LABEL,USRCUR
C
C    Overlay plane reserved for ARGS_USRCUR.
C
      PARAMETER (USRCUR=8)
C
C    Overlay plane used by BLABELS.
C
      PARAMETER (LABEL=9)
C
      INTEGER*2 CURSOR(256)
      PARAMETER (MAXOPT=5)
      REAL OPTINC(5),OPTMIN(5),OPTMAX(5),RATE(10)
      REAL INCR,SCALE,WORK
C
C
C    Initilise the cursor array to 0, ie. completely blank.
C
      DO I=1,256
        CURSOR(I)=0
      END DO
C
C    Set up the screen prompts for the options.
C
      OPTION(1)='TILT'
      OPTION(2)='MAJOR AXIS'
      OPTION(3)='MINOR AXIS'
      OPTION(4)='PITCH'
      OPTION(5)='RATE'
C
C    Setup the increments for the various options.
C
      OPTINC(1)=3.14159E-1
      OPTINC(2)=1.0E1
      OPTINC(3)=1.0E1
      OPTINC(4)=6.0E-2
      OPTINC(5)=1.0E0
C
C    Setup the maxima for the various options.
C
      OPTMAX(1)=6.28318E0
      OPTMAX(2)=FLOAT(IXEXT)/2.0E0
      OPTMAX(3)=FLOAT(IYEXT)/2.0E0
      OPTMAX(4)=3.14159E0
      OPTMAX(5)=1.0E1
C
C    Setup the minima for the various options.
C
      OPTMIN(1)=0.0E0
      OPTMIN(2)=1.0E0
      OPTMIN(3)=1.0E0
      OPTMIN(4)=6.0E-3
      OPTMIN(5)=1.0E0
C
C    Setup the various permitted rates for changing the parameters.
C
      RATE(1)=1.0E-2
      RATE(2)=1.0E-1
      RATE(3)=5.0E-1
      RATE(4)=7.5E-1
      RATE(5)=1.0E0
      RATE(6)=1.25E0
      RATE(7)=1.5E0
      RATE(8)=2.0E0
      RATE(9)=5.0E0
      RATE(10)=1.0E1
C
C    Draw the cursor input.
C
      CALL IRONXA (PLANE,COLOUR,IXEXT,IYEXT,XCEN,YCEN,THETA,
     :             PHI,A,B)
C
C    Allow selection of option.
C
      OPT=1
      MORE=.TRUE.
      INDEX=5
      SCALE=RATE(INDEX)
      DO WHILE (MORE)
C
C    Put up the labels.
C
        THISOP=OPTION(OPT)
        CALL BLABELS (LABEL,COLOUR,THISOP,'NEXT',
     :               'LAST','ACCEPT')
C
C    Obtain choice from buttons.
C
        XPOS=NINT(XCEN)
        YPOS=NINT(YCEN)
        CALL ARGS_USRCUR (USRCUR,COLOUR,CURSOR,XPOS,YPOS,XOUT,YOUT,
     :                    BUTT)
C
C    Decode choice.
C
        CHAOPT=.FALSE.
        IF (BUTT.EQ.1) THEN
          CHAOPT=.TRUE.
        ELSE IF (BUTT.EQ.2) THEN
          OPT=OPT+1
          IF (OPT.GT.MAXOPT) OPT=1
        ELSE IF (BUTT.EQ.3) THEN
          OPT=OPT-1
          IF (OPT.LT.1) OPT=MAXOPT
        ELSE IF (BUTT.EQ.4) THEN
          MORE=.FALSE.
        END IF
C
C    If one of the options to change the cursor has been
C    selected then process this option.
C
        IF (CHAOPT) THEN
C
C    Change tilt.
C
          IF (OPT.EQ.1) THEN
            CALL BLABELS (LABEL,COLOUR,'CHNG FUNC','ANTI CLCK',
     :                   'CLCKWSE','   ')
            ACCEPT=.FALSE.
            DO WHILE (.NOT.ACCEPT)
C
C    Change and redraw the cursor.
C
              CALL IRONXA (PLANE,COLOUR,IXEXT,IYEXT,XCEN,YCEN,THETA,
     :                     PHI,A,B)
              INCR=OPTINC(1)*SCALE
              CALL IRONX2 (USRCUR,COLOUR,OPTMIN(1),OPTMAX(1),INCR,
     :                     THETA,ACCEPT)
            END DO
          END IF
C
C    Change the length of the major axis.
C
          IF (OPT.EQ.2) THEN
            CALL BLABELS (LABEL,COLOUR,'CHNG FUNC','INCREASE',
     :                   'DECREASE','  ')
            ACCEPT=.FALSE.
            DO WHILE (.NOT.ACCEPT)
C
C    Change and redraw the cursor.
C
              CALL IRONXA (PLANE,COLOUR,IXEXT,IYEXT,XCEN,YCEN,THETA,
     :                     PHI,A,B)
              INCR=OPTINC(2)*SCALE
              CALL IRONX2 (USRCUR,COLOUR,OPTMIN(2),OPTMAX(2),INCR,
     :                     A,ACCEPT)
            END DO
          END IF
C
C    Change the length of the minor axis.
C
          IF (OPT.EQ.3) THEN
            CALL BLABELS (LABEL,COLOUR,'CHNG FUNC','INCREASE',
     :                   'DECREASE','  ')
            ACCEPT=.FALSE.
            DO WHILE (.NOT.ACCEPT)
C
C    Change and redraw the cursor.
C
              CALL IRONXA (PLANE,COLOUR,IXEXT,IYEXT,XCEN,YCEN,THETA,
     :                     PHI,A,B)
              INCR=OPTINC(3)*SCALE
              CALL IRONX2 (USRCUR,COLOUR,OPTMIN(3),OPTMAX(3),INCR,
     :                     B,ACCEPT)
            END DO
          END IF
C
C    Change the pitch angle of the arms.
C
          IF (OPT.EQ.4) THEN
            CALL BLABELS (LABEL,COLOUR,'CHNG FUNC','WIDER',
     :                   'NARROWER','  ')
            ACCEPT=.FALSE.
            DO WHILE (.NOT.ACCEPT)
C
C    Change and redraw the cursor.
C
              CALL IRONXA (PLANE,COLOUR,IXEXT,IYEXT,XCEN,YCEN,THETA,
     :                     PHI,A,B)
              INCR=OPTINC(4)*SCALE
              CALL IRONX2 (USRCUR,COLOUR,OPTMIN(4),OPTMAX(4),INCR,
     :                     PHI,ACCEPT)
            END DO
          END IF
C
C    Alter the increment by which the other parameters are changed.
C
          IF (OPT.EQ.5) THEN
            ACCEPT=.FALSE.
            DO WHILE (.NOT.ACCEPT)
              WRITE (BUFF,2000) INDEX
 2000         FORMAT (1X,'RATE=',I2)
              CALL BLABELS (LABEL,COLOUR,'CHNG FUNC','INCREASE',
     :                     'DECREASE',BUFF)
              WORK=FLOAT(INDEX)
              CALL IRONX2 (USRCUR,COLOUR,OPTMIN(5),OPTMAX(5),OPTINC(5),
     :                     WORK,ACCEPT)
              INDEX=NINT(WORK)
              SCALE=RATE(INDEX)
            END DO
          END IF
        END IF
      END DO
C
C
C    Label the four arms.
C
      CALL LABARM (PLANE,COLOUR,IXEXT,IYEXT,XCEN,YCEN,THETA,
     :             PHI,A,B)
C
C    Clear the plane used by BLABELS and the one notionally
C    used by ARGS_USRCUR.
C
      CALL ARGS_OVCLR (LABEL)
      CALL ARGS_OVCLR (USRCUR)
C
C    Re-enable the ARGS picture planes.
C
      CALL ARGS_PICWRT
      END
