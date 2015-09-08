	SUBROUTINE DRABOXR (X1,Y1,X2,Y2)
C+
C	 Subroutine to use FINGS graphics to draw a box
C	 defined by real coords.
C
C	 Arguments:-
C	
C	 X1,Y1 - Coords. of bottom left hand corner of box.
C	 X2,Y2 - Coords. of top right hand corner of box.
C
C	 Subroutines called:-
C
C	 Fings; MOVTO2,LINTO2.
C
C	 A C Davenhall. /ROE/			6/10/81.
C-
	REAL X1,Y1,X2,Y2
	CALL MOVTO2 (X1,Y1)
	CALL LINTO2 (X2,Y1)
	CALL LINTO2 (X2,Y2)
	CALL LINTO2 (X1,Y2)
	CALL LINTO2 (X1,Y1)
	END
