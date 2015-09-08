      SUBROUTINE CHARG
C+
C     CHARG.
C
C     Subroutine to provide a command string interpreter for
C     writing character strings to the ARGS using
C     Fings.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   E2D:     CHARGH, CHARG2, CHARG3, CHARG4, CHARG5, CHARG7,
C            ROTPLN, SELCOL.
C   Interfaces:  READC, UPPCAS.
C
C  A C Davenhall./ROE/                                    4/6/82.
C  A C Davenhall./ROE/  {Modified}                       28/7/82.
C-
      CHARACTER BUFF*30
      INTEGER   STAT,MODE,SIZCHA,PLANE,IOSTAT
      LOGICAL   MORE,ROTATE,DRAW
      CHARACTER COLOUR*(1)
C
C    Give values for the minimum and maximum overlay planes
C    to be used.
C
      INTEGER MINVAL,MAXVAL
      PARAMETER (MINVAL=8)
      PARAMETER (MAXVAL=15)
C
      SIZCHA=10
      MODE=1
C
C    Setup the defaults for the cursor colour and the plane
C    to be used for graphics. Also give a value to the
C    flag indicating whether or not graphics are to be
C    rotated through the overlay planes (they are)
C
      COLOUR='Y'
      PLANE=8
      ROTATE=.TRUE.
C
      CALL CHARGH
      MORE=.TRUE.
      DO WHILE (MORE)
        DRAW=.FALSE.
        IOSTAT=0
        CALL READC ('COMMAND',' Enter command:-',
     :              ' ',' ','~',BUFF,IOSTAT)
        CALL UPPCAS (BUFF)
C
C    Obtain and write a character string.
C
        IF (BUFF.EQ.'WRITE'.OR.BUFF.EQ.'W') THEN
          CALL CHARG2 (PLANE,COLOUR,MODE,SIZCHA)
          DRAW=.TRUE.
        END IF
C
C    Display a length scale.
C
        IF (BUFF.EQ.'SCALE'.OR.BUFF.EQ.'S') THEN
          CALL CHARG5 (PLANE,COLOUR,MODE)
          DRAW=.TRUE.
        END IF
C
C    Display an arrow.
C
        IF (BUFF.EQ.'ARROW'.OR.BUFF.EQ.'A') THEN
          CALL CHARG6 (PLANE,COLOUR,MODE)
          DRAW=.TRUE.
        END IF
C
C    Alter the mode in which the routine is used.
C
        IF (BUFF.EQ.'MODE'.OR.BUFF.EQ.'M')
     :    CALL CHARG3 (MODE,SIZCHA)
C
C    Clear a single overlay plane: the last one used if the
C    planes are being rotated. Also decrement the current
C    plane if the palnes are being rotated.
C
        IF (BUFF.EQ.'CLEAR'.OR.BUFF.EQ.'C') THEN
          IF (ROTATE) CALL ROTPLN (MINVAL,MAXVAL,-1,PLANE)
          CALL CHARG4 (PLANE)
        END IF
C
C    Clear all the planes if they are being rotated, otherwise
C    clear the single plane that is being used.
C
        IF (BUFF.EQ.'CLEARALL'.OR.BUFF.EQ.'CA') THEN
          IF (ROTATE) THEN
            DO I=MINVAL,MAXVAL
              CALL CHARG4 (I)
            END DO
            PLANE=MINVAL
          ELSE
            CALL CHARG4 (PLANE)
          END IF
        END IF
C
C    Change the colour of the cursor.
C
        IF (BUFF.EQ.'COLOUR'.OR.BUFF.EQ.'CO') 
     :    CALL SELCOL (COLOUR,IOSTAT)
C
C    Determine whether the graphics are to be rotated through
C    the overlay planes or not.
C
        IF (BUFF.EQ.'ROTATE'.OR.BUFF.EQ.'R')
     :    CALL CHARG7 (MINVAL,MAXVAL,ROTATE,PLANE,IOSTAT)
C
C    List the commands available.
C
        IF (BUFF.EQ.'HELP'.OR.BUFF.EQ.'H')
     :    CALL CHARGH
C
C    Exit.
C
        IF (BUFF.EQ.'EXIT'.OR.BUFF.EQ.'E')
     :    MORE=.FALSE.
C
C    Rotate the plane to the next overlay plane to be used, if this
C    option has been selected and something has been drawn
C    in this pass through the command loop.
C
        IF (ROTATE) THEN
          IF (DRAW)
     :      CALL ROTPLN (MINVAL,MAXVAL,1,PLANE)
        END IF
      END DO
      END
