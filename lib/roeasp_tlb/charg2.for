      SUBROUTINE CHARG2 (PLANE,COLOUR,MODE,SIZCHA)
C+
C     CHARG2.
C
C     Subroutine to obtain a character a string, a position
C     where it is to be located & then to plot it on the
C     ARGS at this position.
C
C  Given;
C   PLANE  (I)  Args overlay plane in which the string is to appear.
C   COLOUR (C)  Colour in which the string is to appear.
C   MODE   (I)  Indicates whether the position is to be obtained
C               from the cursor or the keyboard.
C               = 1 - cursor.
C               = 2 - keyboard.
C   SIZCHA (I)  Character size.
C
C  Subroutines called;
C   E2DEF:    CHARG1.
C   ARGS:     SRINIT, ARGCUR.
C   Interfaces: OUTPUT, READI, READC.
C
C  Structure:-
C   Obtain string.
C   According to mode
C     obtain coords from cursor.
C   else
C     obtain coords. from keyboard.
C   end if
C   plot string on the ARGS.
C
C  A C Davenhall./ROE/                                        4/6/82.
C  A C Davenhall./ROE/  {Modified}                           28/7/82.
C-
      INTEGER MODE,SIZCHA,PLANE
      CHARACTER COLOUR*(1)
C
      INTEGER XPOS,YPOS,STAT
      CHARACTER STRING*80
C
C
C    Obtain character string.
C
      STAT=0
      CALL READC ('STRING',' Enter string to be displayed;',
     :      ' ',' ','~',STRING,STAT)
C
C    Obtain the position for the string from either the cursor
C    or the keyboard.
C
      IF (MODE.NE.2) THEN
C
C    Position from cursor.
C
        CALL SRINIT (0,.FALSE.,IFAIL)
        CALL OUTPUT (
     : ' Place cursor over bottom L.H. Corner of string position.',
     :    STAT)
        XPOS=255
        YPOS=255
        CALL CURARG (XPOS,YPOS)
      ELSE
C
C    Position from keyboard.
C
        CALL OUTPUT (
     : ' Enter coords. of bottom L.H. corner of string position.',
     :    STAT)
        CALL READI ('XPOS',' X coord;',255,0,511,XPOS,STAT)
        CALL READI ('YPOS',' Y voord;',255,0,511,YPOS,STAT)
      END IF
C
C    Put up the string on the ARGS.
C
      IF (STAT.EQ.0)
     :    CALL CHARG1 (PLANE,COLOUR,STRING,XPOS,YPOS,SIZCHA)
      END
