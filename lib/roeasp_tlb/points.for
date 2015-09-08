	SUBROUTINE POINTS (X,Y,M,NPTS,ICHAR)
C+
C	 Subroutine to plot a set of points as points.
C
C	 X - Array containing the X coords. of the points.
C	 Y - Array containing the Y coords. of the points.
C	 M - Size of arrays X & Y.
C	 NPTS - No. of pairs of data points (NPTS.LE.M, necessarily).
C	 ICHAR - Point plotting symbol (valid range = 0 - 8).
C
C	 Note; This routine assumes no ordering of the points.
C
C	 A C Davenhall.  ROE.		8/9/81.
C-
	REAL X(M),Y(M)
	INTEGER M,NPTS,ICHAR,IPLOT
C
C	 Check that the plotting symbol is valid, & if not replace
C	 it with 8 (an asterisk).
C
	IF (ICHAR.LT.0.OR.ICHAR.GT.8) THEN
	   IPLOT=8
	ELSE
	   IPLOT=ICHAR
	END IF
C
C	 Plot the points.
C
	DO I=1,NPTS
	   CALL MOVTO2 (X(I),Y(I))
	   CALL SYMBOL (IPLOT)
	END DO
	END
