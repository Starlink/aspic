	SUBROUTINE DRABOX (IX1,IY1,IX2,IY2)
C+
C	 Subroutine to use FINGS graphics to draw a box
C	 defined by integer coords.
C
C	 Arguments:-
C	
C	 IX1,IY1 - Coords. of bottom left hand corner of box.
C	 IX2,IY2 - Coords. of top right hand corner of box.
C
C	 Subroutines called:-
C
C	 Fings; MOVTO2,LINTO2.
C
C	 A C Davenhall. /ROE/			6/10/81.
C-
	INTEGER IX1,IY1,IX2,IY2
	REAL X1,Y1,X2,Y2
	X1=FLOAT(IX1)
	X2=FLOAT(IX2)
	Y1=FLOAT(IY1)
	Y2=FLOAT(IY2)
	CALL MOVTO2 (X1,Y1)
	CALL LINTO2 (X2,Y1)
	CALL LINTO2 (X2,Y2)
	CALL LINTO2 (X1,Y2)
	CALL LINTO2 (X1,Y1)
	END
