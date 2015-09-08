	SUBROUTINE CIRINT1 (XCEN,YCEN,RAD,A,IXEXT,IYEXT,TOTLUM)
C+
C	 Subroutine to integrate inside a circular aperture by
C	 simply ignoring overlapping pixels (ie. only counting
C	 pixels totaly inside the aperture).
C
C	 Given;
C	 XCEN - X coord. of centre of aperture.
C	 YCEN - Y   "  . "    "    "     "     ( "  ).
C	 RAD  - Radius of aperture (in pixels) (real).
C	 A - Array to hold image to be integrated (real).
C	 IXEXT - X size of array (real).
C	 IYEXT - Y  "   "    "   ( "  ).
C
C	 Returned;
C	 TOTLUM - Luminusity integrated insid aperture (real).
C
C	 A C Davenhall. /ROE/				15/12/81.
C-
	REAL A(IXEXT,IYEXT)
	INTEGER IXEXT,IYEXT
	REAL XCEN,YCEN,RAD,TOTLUM
	INTEGER X1,X2,Y1,Y2
	REAL RADPT

C
C	 Setup a box surrounding the aperture.
C
	X1=IFIX(XCEN-RAD)-1
	X1=MAX(1,X1)
	Y1=IFIX(YCEN-RAD)-1
	Y1=MAX(1,Y1)
	X2=IFIX(XCEN+RAD)+1
	X2=MIN(IXEXT,X2)
	Y2=IFIX(YCEN+RAD)+1
	Y2=MIN(IYEXT,Y2)
C
C	 Examine all the points inside the box.
C
	TOTLUM=0.0E0
	DO J=Y1,Y2
	  DO I=X1,X2
	    RADPT=SQRT(((FLOAT(I)-XCEN)**2)+((FLOAT(J)-YCEN)**2))
	    IF (RADPT.LT.RAD) THEN
	      TOTLUM=TOTLUM+A(I,J)
	    END IF
	  END DO
	END DO
	END
