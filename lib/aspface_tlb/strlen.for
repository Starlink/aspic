      SUBROUTINE STRLEN (SIZE,STRINGS,LONGSTR)
C+
C     STRLEN.
C 
C     Subroutine to return the length of the longest non-blank
C     string in an array of character strings. Counting starts
C     at the first character of each element.
C
C  Given;
C   SIZE     (I)  No. of elements in the array.
C   STRINGS  (CA) Array holding strings.
C
C  Returned;
C   LONGSTR  (I)  Length of the longest string in the array.
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                                         16/6/83.
C-
      IMPLICIT NONE
C
      INTEGER SIZE,LONGSTR
      CHARACTER STRINGS(SIZE)*(*)
C
      INTEGER STRINLEN,INDEX,INDEX1,THISLEN
C
C
C    Determine the total length (including blanks) of each of the
C    elements of the character array.
C
      STRINLEN=LEN(STRINGS(1))
C
C    Determine length of the longest string.
C
      LONGSTR=0
      DO INDEX=1,SIZE
C
C    Determine the length of each string.
C
        THISLEN=1
        DO INDEX1=1,STRINLEN
          IF (STRINGS(INDEX)(INDEX1:INDEX1).NE.' ') THISLEN=INDEX1
        END DO
C
        LONGSTR=MAX(LONGSTR,THISLEN)
      END DO
C
      END
