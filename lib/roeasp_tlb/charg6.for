      SUBROUTINE CHARG6 (PLANE,COLOUR,MODE)
C+
C     CHARG6.
C
C     Subroutine to add an arrow of user specified length,
C     position and orientation to an ARGS image.
C
C  Given;
C   PLANE (I)  Args overlay plane in which the arrow is to be drawn.
C   COLOUR (C) Colour in which the arrow is to appear.
C   MODE  (I)  Determines whether the central position of the
C              arrow is to be input from the cursor or keyboard.
C              = 1 - cursor.
C              = 2 - keyboard.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interfaces: OUTPUT, READR.
C   Args:       SRINIT, ARGCUR, ARGS_PICWRT, ARGS_OVWRT, ARGS_OVCOL,
C               ARGS_OVGEN.
C   Fings:      ARGS, DEVSPE, VUPORT, WINDOL, MOVTO2, LINTO2,
C               DEVEND.
C
C  Structure:-
C   Obtain angle for arrow.
C   Obtain length of arrow.
C   as appropriate
C     Obtain central position for arrow from cursor
C   else
C     Obtain central position for arrow from keyboard.
C   end if
C   if (all ok)
C     set up for plotting
C     compute a horizontal arrow
C     rotate and translate the arrow to the appropriate position
C     draw the arrow
C     stop plotting
C   else
C     print a message saying all not ok.
C   end if.
C
C  A C Davenhall./ROE/                                  8/6/82.
C-
      INTEGER MODE,PLANE
      CHARACTER COLOUR*(1)
C
      REAL THETA,THETAR,ARRLEN,XX,YY,XPOS,YPOS,ARRHD
      REAL X(4),Y(4)
      INTEGER STAT,STAT1
      INTEGER XARG,YARG
C
      REAL PI
      PARAMETER (PI=3.14159265E0)
      REAL HEAD
      PARAMETER (HEAD=1.0E-1)
C
C    Obtain angle for arrow.
C
      CALL OUTPUT (
     : ' Enter angle for arrow, measured anticlockwise from the',
     :   STAT1)
      CALL OUTPUT (' horizontal, in degrees.',STAT1)
      STAT=0
      CALL READR ('THETA',' Enter angle;',
     :             0.0E0,0.0E0,3.60E2,THETA,STAT)
C
C    Obtain the length of the arrow.
C
      CALL READR ('ARRLEN',
     : ' Enter length of arrow (full screen = 512);',
     :   5.0E1,0.0E0,5.12E2,ARRLEN,STAT)
C
C    Obtain the position for the centre of the arrow.
C
      IF (MODE.NE.2) THEN
C
C    Position from cursor.
C
        CALL SRINIT (0,.FALSE.,IFAIL)
        CALL OUTPUT (
     :  ' Place cursor over the arrowhead.',STAT1)
        XARG=255
        YARG=255
        CALL CURARG (XARG,YARG)
        XPOS=FLOAT(XARG)
        YPOS=FLOAT(YARG)
      ELSE
C
C    Position from keyboard.
C
        CALL OUTPUT (
     :  ' Enter coords. for the arrowhead.',STAT1)
        CALL READR ('XPOS',' X coord;',2.56E2,1.0E0,5.12E2,XPOS,STAT)
        CALL READR ('YPOS',' Y coord;',2.56E2,1.0E0,5.12E2,YPOS,STAT)
      END IF
C
C    Proceed to draw arrow of all ok.
C
      IF (STAT.EQ.0) THEN
C
C    Setup for plotting.
C
        CALL SRINIT (0,.FALSE.,IFAIL)
        CALL ARGS
        CALL ARGS_PICWRT
        CALL DEVSPE (9600)
        CALL ARGS_OVWRT (PLANE)
        CALL ARGS_OVCOL (PLANE,COLOUR)
        CALL ARGS_OVGEN ('W')
        CALL VUPORT (0.0E0,1.0E0,0.0E0,1.0E0)
C
C    Setup a 512**2 plotting space.
C
        CALL WINDOL (0.0E0,5.11E2,0.0E0,5.11E2)
C
C    Convert theta to radians.
C
        THETAR=THETA*2.0E0*PI/3.60E2
C
C    Construct a horizontal arrow.
C
        X(1)=0.0E0
        Y(1)=0.0E0
        X(2)=ARRLEN
        Y(2)=0.0E0
        ARRHD=ARRLEN*HEAD
        X(3)=ARRHD
        Y(3)=ARRHD
        X(4)=ARRHD
        Y(4)=-ARRHD
C
C    Rotate and translate the arrow to the required position.
C
        DO I=1,4
          XX=X(I)
          YY=Y(I)
          X(I)=XPOS+(XX*COS(THETAR))-(YY*SIN(THETAR))
          Y(I)=YPOS+(XX*SIN(THETAR))+(YY*COS(THETAR))
        END DO
C
C    Draw the arrow.
C
        CALL MOVTO2 (X(1),Y(1))
        CALL LINTO2 (X(2),Y(2))
        CALL MOVTO2 (X(3),Y(3))
        CALL LINTO2 (X(1),Y(1))
        CALL LINTO2 (X(4),Y(4))
C
C    Stop plotting.
C
        CALL DEVEND
      ELSE
C
C    Unable to get one or more of the parameters. print a message.
C
        CALL OUTPUT (
     :  ' ***ERROR Unable to obtain parameters correctly.',STAT1)
      END IF
      END
