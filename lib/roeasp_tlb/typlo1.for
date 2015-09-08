	SUBROUTINE TYPLO1 (TYPE,CHAR)
C+
C	 Subroutine to select the manner of plotting an array of data
C	 points under programmer control; plot the individual points
C	 as seperate symbols, join the points with straight lines
C	 or join the points with "histograms" ie. with vertical and
C	 horizontal lines.
C
C	 Given;
C	 TYPE = 1 - Data plotted as points.
C	      = 2 - Data joined with straight lines.
C	      = 3 - Data joined with a "histogram". (integer).
C
C	 CHAR - Determines the plotting symbol used if points
C	 	are selected. Valid range is 0 - 8. See the Fings
C		manual for the symbol produced by each value. (integer).
C
C	 Returned; -.
C
C	 Subroutines called; None.
C
C	 Common block /PLOTSP/:-
C
C	 ITYPE - as TYPE.
C	 ICHAR - as CHAR.
C
C	 A C Davenhall. /ROE/				17/11/81.
C-
	COMMON /PLOTSP/ ITYPE,ICHAR
	INTEGER ITYPE,ICHAR
	INTEGER TYPE,CHAR
C
C	 Set defaults.
C
	ITYPE=3
	ICHAR=8
C
C	 Accept values if within range.
C
	IF (TYPE.GE.1.AND.TYPE.LE.3) ITYPE=TYPE
	IF (CHAR.GE.0.AND.CHAR.LE.8) ICHAR=CHAR
	END
