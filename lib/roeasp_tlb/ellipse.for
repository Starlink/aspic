	SUBROUTINE ELLIPSE (XPOS,YPOS,A,ECC,THETA)
C+
C	 Subroutine to plot an ellipse of arbitary position,
C	 eccentricity & inclination to the horizontal.
C
C	 If (major axis ~ 0 ) then
C	   Draw a dot.
C	 else
C	 If (eccentricity ~ 1 ) then
C	   Draw a line of length equal to the major axis along
C	   the direction of tilt.
C	 else
C	   Have a real ellipse;
C	   Compute semi-major axis.
C	   Compute smei-minor axis.
C	   For array of points,
C	     Compute an angle phi
C	     Compute radius of the ellipse at this angle.
C	     Compute corresponding X & Y positions.
C	   end do
C	   For array of points
C	     Rotate to desired inclination.
C	   end do
C	   Plot points.
C	 end if
C	 end if
C
C	 Given;
C	 XPOS - X position of the centre of ellipse (real).
C	 YPOS - Y    "     "   "    "    "     "    ( "  ).
C	 A - Major axis (ie. extrema to extrema) (real).
C	 ECC - Eccentricity (real).
C	 THETA - Angle of the major axis above the horizontal
C		 (anticlockwise positive) in radians (real).
C
C	 Returned; -.
C
C	 Subroutines called;
C	 LINE.
C
C	 A C Davenhall. /ROE/				12/11/81.
C-
	REAL XPOS,YPOS,A,ECC,THETA
	REAL PHI,DEPHI,PI,AA,BB,R,XX,YY
	INTEGER NPTS
	REAL X(100),Y(100)
	PARAMETER (PI=3.1415927E0)
	PARAMETER (NPTS=60)
	IF (A.LE.1.0E-7) THEN
C
C	 Take care of the case where the major axis is sensibly
C	 zero. Draw a dot.
C
	  CALL MOVTO2 (XPOS,YPOS)
	  CALL LINTO2 (XPOS,YPOS)
	ELSE
	IF ((1.0E0-ECC).LE.1.0E-7) THEN
C
C	 Take care of the case where the eccentricity is sensibly
C	 1.0. Draw a line.
C
	  X(1)=XPOS-(A*COS(THETA)/2.0E0)
	  Y(1)=YPOS-(A*SIN(THETA)/2.0E0)
	  X(2)=XPOS+(A*COS(THETA)/2.0E0)
	  Y(2)=YPOS+(A*SIN(THETA)/2.0E0)
	  CALL MOVTO2 (X(1),Y(1))
	  CALL LINTO2 (X(2),Y(2))
	ELSE
C
C	 Now do the case where a real ellipse has been input.
C
C
C	 Compute the semi-major axis, AA & the semi-minor
C	 axis, BB.
C
	  AA=A/2.0E0
	  BB=AA*SQRT(1.0E0-(ECC**2))
C
C	 Compute an array of points along a horizontal
C	 Centred on the origin.
C
	  DEPHI=(2.0E0*PI)/FLOAT(NPTS)
	  DO I=1,NPTS+1
	    PHI=DEPHI*FLOAT(I-1)
	    R=SQRT(((AA*BB)**2)/(((AA*SIN(PHI))**2)+
     :	  	  ((BB*COS(PHI))**2)))
	    X(I)=R*COS(PHI)
	    Y(I)=R*SIN(PHI)
	  END DO
C
C	 Now rotate through an angle THETA,
C	 & translate the centre to point XPOS,YPOS.
C
	  NPPTS=NPTS+1
	  DO I=1,NPPTS
	    XX=X(I)
	    YY=Y(I)
	    X(I)=XPOS+(XX*COS(THETA))-(YY*SIN(THETA))
	    Y(I)=YPOS+(XX*SIN(THETA))+(YY*COS(THETA))
	  END DO
C
C	 Plot points,
C
	  CALL LINE (X,Y,60,NPPTS)
	END IF
	END IF
	END
