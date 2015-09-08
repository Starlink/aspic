      SUBROUTINE ADDLST(LIST,NITEM,LSTLEN,ITEM,IBYT1,IBYT2)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO ADD COLUMNS OF ENTRIES TO A LIST
*
*METHOD
*	COPY REQUIRED BYTE COLUMNS INTO THE LIST
*
*ARGUMENTS
*	LIST (IN/OUT)
*	BYTE(4*NITEM,LSTLEN)
*		LIST TO BE UPDATED
*	NITEM (IN)
*	INTEGER
*		NO. OF 4-BYTE ITEMS IN LIST
*	LSTLEN (IN)
*	INTEGER
*		NO. OF ENTRIES (ROWS) IN LIST
*	ITEM (IN)
*	INTEGER
*		ARRAY CONTAINING COLUMNS TO BE INSERTED
*	IBYT1,IBYT2 (IN)
*	INTEGER
*		START AND FINISH BYTE COLUMNS TO BE REPLACED
*
*CALLS
*	NONE
*
*NOTES
*	USES BYTE ARRAYS
*
*WRITTEN BY
*	R.F. WARREN-SMITH
*-----------------------------------------------------------------------
C
C
      BYTE LIST(4*NITEM,LSTLEN),ITEM(IBYT2-IBYT1+1,LSTLEN)
C
C COPY ITEM INTO THE REQUIRED REGION OF LIST
C
      DO 2 J=1,LSTLEN
	DO 1 I=IBYT1,IBYT2
	  LIST(I,J)=ITEM(I-IBYT1+1,J)
    1   CONTINUE
    2 CONTINUE
      RETURN
      END