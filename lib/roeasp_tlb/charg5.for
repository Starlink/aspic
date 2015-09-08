      SUBROUTINE CHARG5 (PLANE,COLOUR,MODE)
C+
C     CHARG5.
C
C     Subroutine to put up a horizontal line with a length
C     specified  in "astronomical" units at a user specified
C     position on the ARGS display.
C
C  Given;
C   PLANE (I)  Overlay plane on the Args in which the line is to be
C              drawn.
C   COLOUR (C) Colour in which the line is to appear.
C   MODE  (I)  Determines whether the bar position is to be obtained
C              from the cursor or keyboard.
C              = 1 - Cursor.
C              = 2 - Keyboard.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interfaces: READR, READI, OUTPUT.
C   Args:       SRINIT, ARGCUR, ARGS_PICWRT, ARGS_OVWRT, ARGS_OVCOL,
C               ARGS_OVGEN.
C   Fings:      ARGS, DEVSPE, VUPORT, WINDOL, MOVTO2, LINTO2,
C               DEVEND.
C
C  Structure:-
C   Obtain pixel size in astronomical units.
C   Obtain the length of the bar in astronomical units.
C   As appropriate
C     obtain position from cursor
C   or
C     obtain position from keyboard.
C   end if
C   if all values obtain ok
C     compute the length of the bar in screen coords.
C     set up for using Fings.
C     draw the bar.
C     finish plotting.
C   else
C     say not got all values ok.
C   end if

C
C  A C Davenhall./ROE/                                        7/6/82.
C  A C Davenhall./ROE/  {Modified}                            28/7/82.
C  A C Davenhall./ROE/  {Modified to use the Args database}   18/2/83.
C-
      INTEGER MODE,PLANE
      CHARACTER COLOUR*(1)
C
      REAL PIXSIZ,BARLEN,BARLES,XX,YY,XPOS,YPOS
      INTEGER STAT,STAT1
      INTEGER IMGNO,ARGSTT,LENARG,LENPIX,DUM1,DUM2,XARG,YARG
      INTEGER PIX0,ARG0
C
C
C    Obtain the pixel size in astronomical units.
C
      STAT=0
      CALL READR ('PIXSIZ',
     :  ' Enter pixel size in astronomical units;',
     :    5.0E-1,0.0E0,5.0E5,PIXSIZ,STAT)
C
C    Obtain the length of the bar in astronomical units.
C
      CALL READR ('BARLEN',
     :  ' Enter the bar length in astronomical units;',
     :    1.0E1,0.0E0,5.0E5,BARLEN,STAT)
C
C    Obtain the position of the mid point of the bar.
C
      IF (MODE.NE.2) THEN
C
C    Position from cursor.
C
        CALL SRINIT (0,.FALSE.,IFAIL)
        CALL OUTPUT (
     :  ' Place cursor over middle point of bar.',STAT1)
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
     :  ' Enter coords. of mid. point of bar',STAT1)
        CALL READR ('XPOS','X Coord;',2.56E2,1.0E0,5.12E2,XPOS,STAT)
        CALL READR ('YPOS','Y Coord;',2.56E2,1.0E0,5.12E2,YPOS,STAT)
      END IF
C
C    If all is ok proceed to compute the length of the bar and plot
C    it.
C
      IF (STAT.EQ.0) THEN
C
C    Compute the length of the bar in screen coords.
C
        CALL ARGS_NUMIM (IMGNO)
        LENPIX=NINT(BARLEN/PIXSIZ)
        CALL ARGS_PTOA (IMGNO,LENPIX,DUM1,LENARG,DUM2,ARGSTT)
        PIX0=0
        CALL ARGS_PTOA (IMGNO,PIX0,DUM1,ARG0,DUM2,ARGSTT)
        LENARG=LENARG-ARG0
        BARLES=FLOAT(LENARG)
C
C    Set up for plotting using Fings.
C
        CALL SRINIT (0,.FALSE.,IFAIL)
        CALL ARGS
        CALL ARGS_PICWRT
        CALL DEVSPE (9600)
C
C    Setup for using the selected overlay plane.
C
        CALL ARGS_OVWRT (PLANE)
        CALL ARGS_OVCOL (PLANE,COLOUR)
        CALL ARGS_OVGEN ('W')
        CALL VUPORT (0.0E0,1.0E0,0.0E0,1.0E0)
C
C    Setup a window 512**2.
C
        CALL WINDOL (0.0E0,5.11E2,0.0E0,5.11E2)
C
C    Draw the bar.
C
        XX=XPOS-(BARLES/2.0E0)
        YY=YPOS+1.0E1
        CALL MOVTO2 (XX,YY)
        YY=YPOS-1.0E1
        CALL LINTO2 (XX,YY)
        YY=YPOS
        CALL MOVTO2 (XX,YY)
        XX=XPOS+(BARLES/2.0E0)
        CALL LINTO2 (XX,YY)
        YY=YPOS+1.0E1
        CALL MOVTO2 (XX,YY)
        YY=YPOS-1.0E1
        CALL LINTO2 (XX,YY)
C
C    finish plotting.
C
        CALL DEVEND
      ELSE
C
C    Unable to obtain one or more of the parameters.
C
        CALL OUTPUT (
     :  ' ***ERROR Unable to obtain parameters properly.',STAT1)
      END IF
      END
