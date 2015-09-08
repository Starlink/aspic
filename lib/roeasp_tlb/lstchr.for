      SUBROUTINE LSTCHR (STRING,LSTPOS)
C+
C     LSTCHR.
C
C     Subroutine to find the position of the last non-blank
C     character in a character string.
C
C  Given;
C   STRING  (C)  Character string to be analysed.
C
C  Returned;
C   LSTPOS  (I)  Position of the last non-blank character within the
C                string input.
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                                   1/8/82.
C-
      CHARACTER STRING*(*)
      INTEGER LSTPOS
C
C
C    Obtain the length of the string.
C
      LSTPOS=LEN(STRING)
C
C    Decrement the length until a non-zero character is found,
C    or run out of string.
C
      DO WHILE (STRING(LSTPOS:LSTPOS).EQ.' '.AND.LSTPOS.GT.1)
        LSTPOS=LSTPOS-1
      END DO
      END
