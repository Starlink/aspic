      SUBROUTINE CHARG3 (MODE,SIZCHA)
C+
C     CHARG3.
C
C     Subroutine to control the way in which the Fings character
C     plotting to the ARGS will work.
C
C  Given;
C   None.
C
C  Returned;
C   MODE   (I)  Determines whether the coords. of the string are to 
C               be obtained from the cursor or the keyboard.
C               = 1 - coords. from cursor.
C               = 2 - coords. from keyboard.
C   SIZCHA (I)  Size of characters.
C
C  Subroutines called;
C   Interfaces:  MULREP, READI.
C
C  A C Davenhall./ROE/                                      4/6/82.
C-
      INTEGER MODE,SIZCHA
C
      INTEGER STAT
      CHARACTER REPLY*10
C
C
C    Obtain the mode in which the coords. are to be obtained.
C
      STAT=0
      CALL MULREP (
     : ' String to be obtained from cursor or keyboard?',
     : 'CURSOR,C,KEYBOARD,K$',REPLY,STAT)
      MODE=1
      IF (REPLY.EQ.'KEYBOARD'.OR.REPLY.EQ.'K') MODE=2
C    
C    Obtain the character size.
C
      CALL READI ('SIZCHA',
     : ' Enter the character size (full screen = 512);',
     :   10,1,512,SIZCHA,STAT)
      END
