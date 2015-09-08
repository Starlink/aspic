	SUBROUTINE HISTP (X,Y,M,NPTS)
C+
C	 Subroutine to plot a set of points as a "histogram"
C	 ie. joining adjacent points by vertical & horizontal lines.
C
C	 X - Array to hold X coords. of points.
C	 Y - Array to hold Y coords. of points.
C	 M - Size of arrays X & Y.
C	 NPTS - No. of pairs of data points (NPTS.LE.M,necessarily).
C
C	 Note; the routine assumes the X coord. to be sorted into
C	       either increasing or decreasing order.
C
C	 A C Davenhall. ROE.		8/9/81.
C-
	REAL X(M),Y(M)
	REAL X1,X2,Y1,Y2
	INTEGER M,NPTS,NN
	NN=NPTS-1
C
C	 Fudge the first point.
C	
	X1=X(1)
	Y1=Y(1)
	CALL MOVTO2 (X1,Y1)
C
C	 Plot points.
C
	DO I=1,NN
	   II=I+1
	   X2=(X(II)+X(I))/2.0E0
	   Y2=Y(II)
	   CALL LINTO2 (X2,Y1)
	   CALL LINTO2 (X2,Y2)
	   X1=X2
	   Y1=Y2
	END DO
C
C	 Fudge the last point.
C
	X2=X(NPTS)
	CALL LINTO2 (X2,Y2)
	END
