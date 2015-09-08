	SUBROUTINE ZAP (A,IXEXT,IYEXT,IX1,IY1,IX2,IY2,AVRSKY)
C+
C	 Subroutine to replace a previously defined object
C	 with a previously determined constant value
C	 from the surrounding background.
C
C	 A C Davenhall.		1/9/81.
C-
	REAL A(IXEXT,IYEXT),AVRSKY
	INTEGER IXEXT,IYEXT
	DO J=IY1,IY2
	   DO I=IX1,IX2
	      A(I,J)=AVRSKY
	   END DO
	END DO
	END
