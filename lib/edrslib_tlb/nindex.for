      INTEGER FUNCTION NINDEX(STR1,STR2,N)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO FIND THE N'TH OCCURRENCE OF A SUBSTRING IN A CHARACTER STRING
*
*METHOD
*	SEARCH REPEATEDLY, STARTING EACH SEARCH IMMEDIATELY AFTER THE
*	PREVIOUS OCCURRENCE
*
*ARGUMENTS
*	STR1 (IN)
*	CHARACTER*(*)
*		THE CHARACTER STRING
*	STR2 (IN)
*	CHARACTER*(*)
*		THE SUBSTRING
*	NINDEX (FUNCTION NAME)
*	INTEGER
*		RETURNS LOCATION OF SUBSTRING
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
      INTEGER START,SHIFT 
C
C INITIALLISE RESULT AND FIND LENGTH OF FIRST STRING
C
      NINDEX=0
      LENGTH=LEN(STR1)
C
C SEARCH FOR EACH OCCURRENCE OF STRING 2
C
      DO 1 I=1,N
C
C SET START OF SEARCH AT CHARACTER FOLLOWING LAST OCCURRENCE
C
       START=NINDEX+1
       IF(START.LE.LENGTH) THEN
        SHIFT=INDEX(STR1(START:),STR2)
        IF(SHIFT.EQ.0) THEN
C
C IF NOT FOUND, RESULT =LENGTH+1
C
         NINDEX=LENGTH+1
         GO TO 2
        ELSE
C
C IF FOUND, SET RESULT TO POSITION FOUND
C
         NINDEX=NINDEX+SHIFT
        ENDIF
       ELSE
C
C IF END OF STRING REACHED, OCCURRENCE DOES NOT EXIST..
C
        NINDEX=LENGTH+1
        GO TO 2
       ENDIF
    1 CONTINUE
    2 RETURN
      END
