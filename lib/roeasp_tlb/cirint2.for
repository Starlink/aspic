	SUBROUTINE CIRINT2 (XCEN,YCEN,RAD,A,IXEXT,IYEXT,TOTLUM)
C+
C	 Subroutine to integrate inside a circular aperture using
C	 the aproximation for pixels overlapping the aperture
C	 boundary given by Jones et.al (1967) p.83.
C	 Pub. Dept. Univ. Texas, Austin, SerII, Vol.1, No.8.
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
	REAL RADPT,PI,PIXSIZ
	DATA PI/3.14159265/
	PIXSIZ=SQRT(1.0E0/PI)
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
C
C	 Overlapping pixels weighted according to the procedure
C	 of Jones et. al. (1967) p.83.
C
	    IF ((RADPT-PIXSIZ).LT.RAD.AND.
     :		 RAD.LT.(RADPT-(4.0E-1*PIXSIZ))) THEN
	      TOTLUM=TOTLUM+(2.5E-1*A(I,J))
	    ELSE IF ((RADPT-(4.0E-1*PIXSIZ)).LE.RAD.AND.
     :		      RAD.LE.(RADPT+(4.0E-1*PIXSIZ))) THEN
	      TOTLUM=TOTLUM+(5.0E-1*A(I,J))
	    ELSE IF ((RADPT+(4.0E-1*PIXSIZ)).LT.RAD.AND.
     :		      RAD.LT.(RADPT+PIXSIZ)) THEN
	      TOTLUM=TOTLUM+(7.5E-1*A(I,J))
	    ELSE IF ((RADPT+PIXSIZ).LE.RAD) THEN
	      TOTLUM=TOTLUM+A(I,J)
	    END IF
	  END DO
	END DO
	END
