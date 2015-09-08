	SUBROUTINE CIRCLE (XPOS,YPOS,RAD)
C+
C	 Subroutine to draw a circle of arbitary centre &
C	 radius using Fings.
C
C	 Given;
C	 XPOS - X position of centre (real)
C	 YPOS - Y     "    "    "    ( "  ).
C	 RAD - Radius (real).
C
C	 Returned; -.
C
C	 Subroutine called;
C	 LINE.
C
C	 A C Davenhall /ROE/				12/11/81.
C-
	REAL XPOS,YPOS,RAD
	REAL DETHETA,PI,THETA
	INTEGER NPTS
	REAL X(100),Y(100)
	PARAMETER (NPTS=40)
	PARAMETER (PI=3.1415927E0)
C
C	 Take care of the case where the radius is sensibly zero.
C	 Draw a dot.
C
	IF (RAD.LE.1.0E-7) THEN
	  CALL MOVTO2 (XPOS,YPOS)
	  CALL LINTO2 (XPOS,YPOS)
	ELSE
C
C	 Case where have a real circle.
C
C	 Set up array of points.
C
	  DETHETA=(2.0E0*PI)/FLOAT(NPTS)
	  DO I=1,NPTS+1
	    THETA=DETHETA*(FLOAT(I-1))
	    X(I)=XPOS+(RAD*COS(THETA))
	    Y(I)=YPOS+(RAD*SIN(THETA))
	  END DO
C
C	   Plot points.
C
	  NPPTS=NPTS+1
	  CALL LINE (X,Y,100,NPPTS)
	END IF
	END
