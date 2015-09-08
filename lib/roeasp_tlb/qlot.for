	SUBROUTINE QLOT (X,Y,M,NPTS)
C+
C	 Subroutine to plota an array of data points using 
C	 one of three predetermined formats for presenting 
C	 the data; plotting individual points as seperate
C	 symbols, joining individual points with a straight
C	 line or joining points with a "histogram" ie.
C	 with vertical & horizontal lines.
C
C	 X - Array to hold X coord. of data points.
C	 Y - Array to hold Y coord. of data points.
C	 M - Size of arrays X & Y.
C	 NPTS - No. of pairs of data points.
C
C	 Common block /PLOTSP/:-
C
C	  ITYPE = 1 - Data plotted as points.
C		= 2 - Data joined with a straight line.
C		= 3 - Data joined with a "histogram".
C
C	  ICHAR - Determines the plotting symbol used if points
C		  are selected. Valid range is 0 - 8.
C
C	 A C Davenhall.  ROE.		9/9/81.
C-
	REAL X(M),Y(M)
	INTEGER M,NPTS,ITYPE,ICHAR
	COMMON /PLOTSP/ ITYPE,ICHAR
	IF (ITYPE.EQ.1) CALL POINTS (X,Y,M,NPTS,ICHAR)
	IF (ITYPE.EQ.2) CALL LINE (X,Y,M,NPTS)
	IF (ITYPE.EQ.3) CALL HISTP (X,Y,M,NPTS)
	END
