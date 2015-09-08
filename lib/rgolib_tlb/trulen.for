      INTEGER FUNCTION TRULEN(STRING)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         FUNCTION TRULEN 
C
C
C         It gives the actual length of a  string,  ignoring  any  trailing 
C         spaces  and  null  characters. It should be noted that the result 
C         returned in TRULEN as the value of this function  is  an  INTEGER 
C         and must be typed as such in the calling segment. 
C
C         STRING      CHAR  In    This is the  text  string,  of  arbitrary 
C                                 length, whose length is to be foun 
C
C
C         K F Hartley              RGO                            11-JUN-82 
C
C-------------------------------------------------------------------------- 

C   Returned:-
C      TRULEN   Integer     The length of the string.
C
C   Written by K F Hartley at RGO on 8/12/81
C
      CHARACTER*(*) STRING
      CHARACTER*1 NULL
      DATA NULL/'0'X/
C
C   It is assumed that STRING is full of a sequence of significant
C   characters, followed by a sequence of spaces and/or null characters
C
      L=LEN(STRING)
C
C   L now holds the total length of STRING
C
  100 CONTINUE
      IF (L.GT.0) THEN
         IF (STRING(L:L).EQ.' '.OR.STRING(L:L).EQ.NULL) THEN
            L=L-1
            GO TO 100
         END IF
         TRULEN=L
      ELSE
         TRULEN=0
      END IF
      END
