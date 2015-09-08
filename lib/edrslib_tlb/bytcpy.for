      SUBROUTINE BYTCPY(A,B,N)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO COPY ONE BYTE ARRAY TO ANOTHER OF THE SAME SIZE
*
*METHOD
*	COPY ONE ARRAY TO THE OTHER
*
*ARGUMENTS
*	A (IN)
*	BYTE(N)
*		ARRAY TO BE COPIED
*	B (OUT)
*	BYTE(N)
*		ARRAY TO BE COPIED INTO
*	N (IN)
*	INTEGER
*		DIMENSION OF A AND B
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
      BYTE A(N),B(N)
C
C COPY A TO B
C
      DO 1 I=1,N
	B(I)=A(I)
    1 CONTINUE
      RETURN
      END
