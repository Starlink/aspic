	SUBROUTINE LINE (X,Y,M,NPTS)
C+
C	 Subroutine to plot a set of points by joining them with
C	 straight lines.
C
C	 X - Array to hold X coords. of points.
C	 Y - Array to hold Y coords. of points.
C	 M - Size of arrays X & Y.
C	 NPTS - No. of pairs of data points (NPTS.LE.M, necessarily).
C
C	 Note; the routine assumes the X coord. to be sorted into
C	       either increasing or decreasing order.
C
C	 A C Davenhall. ROE.		8/9/81.
C-
	REAL X(M),Y(M)
	INTEGER M,NPTS
	CALL MOVTO2 (X(1),Y(1))
	DO I=2,NPTS
	   CALL LINTO2 (X(I),Y(I))
	END DO
	END
