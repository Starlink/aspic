      INTEGER FUNCTION NMATCH(STR1,STR2)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO COUNT THE NUMBER OF CHARACTER MATCHES BETWEEN TWO CHARACTER
*	STRINGS
*
*METHOD
*	COMPARE CORRESPONDING CHARACTERS, COUNTING THE MATCHES
*
*ARGUMENTS
*	STR1,STR2 (IN)
*	CHARACTER*(*)
*		THE CHARACTER STRINGS
*	NMATCH (FUNCTION NAME)
*	INTEGER
*		THE NUMBER OF CHARACTER MATCHES
*
*CALLS
*	NONE
*
*WRITTEN BY
*	R.F. WARREN-SMITH
*-----------------------------------------------------------------------
C
C
      CHARACTER STR1*(*),STR2*(*)
      NMATCH=0
C
C FIND LENGTH OF SHORTER STRING
C
      NCHAR=MIN(LEN(STR1),LEN(STR2))
C
C COMPARE CORRESPONDING CHARACTERS, COUNTING MATCHES
C
      DO 1 I=1,NCHAR
       IF(STR1(I:I).EQ.STR2(I:I)) THEN
        NMATCH=NMATCH+1
       ENDIF
    1 CONTINUE
      RETURN
      END