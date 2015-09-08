      SUBROUTINE XYLTRN(LISTA,NITEM,LSTLEN,C,LISTB)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO APPLY A LINEAR TRANSFORMATION TO A LIST OF X,Y POSITIONS
*
*METHOD
*	APPLY THE TRANSFORMATION TO THE APPROPRIATE ELEMENTS OF AN
*	X,Y LIST
*
*ARGUMENTS
*	LISTA (IN)
*	INTEGER(NITEM,LSTLEN)
*		THE INPUT X,Y LIST
*	NITEM,LSTLEN (IN)
*	INTEGER
*		THE DIMENSIONS OF LISTA
*	C (IN)
*	REAL(6)
*		THE COEFFICIENTS DEFINING THE LINEAR TRANSFORMATION
*	LISTB (OUT)
*	INTEGER(NITEM,LSTLEN)
*		THE TRANSFORMED X,Y LIST
*
*CALLS
*	NONE
*
*WRITTEN BY
*	R.F. WARREN-SMITH
*-----------------------------------------------------------------------
C
C
      INTEGER LISTA(NITEM,LSTLEN),LISTB(NITEM,LSTLEN),IX,IY
      REAL C(6),X,Y
C
C USE EQUIVALENCE TO ACCESS X,Y POSITIONS AS REAL NUMBERS
C
      EQUIVALENCE (IX,X),(IY,Y)
C
C SCAN EACH LIST ENTRY
C
      DO 2 J=1,LSTLEN
C
C COPY INPUT LIST TO OUTPUT LIST
C
	DO 1 I=1,NITEM
	  LISTB(I,J)=LISTA(I,J)
    1   CONTINUE
C
C EXTRACT THE X,Y POSITION FROM EACH ENTRY AND APPLY THE TRANSFORMATION
C
	IX=LISTA(6,J)
	IY=LISTA(7,J)
	XT=C(1)+C(2)*X+C(3)*Y
	YT=C(4)+C(5)*X+C(6)*Y
	X=XT
	Y=YT
C
C PUT TRANSFORMED POSITIONS INTO OUTPUT LIST
C
	LISTB(6,J)=IX
	LISTB(7,J)=IY
    2 CONTINUE
      RETURN
      END
