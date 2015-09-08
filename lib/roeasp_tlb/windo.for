	SUBROUTINE WINDO (X,Y,M,NPTS,XMIN,XMAX,YMIN,YMAX,WSET)
C+
C	 Subroutine to automatically set up the extrema of the
C	 axes of a plot in mathematical space.
C
C	 X - Array containing the X coords. of the points.
C	 Y - Array containing the Y coords. of the points.
C	 M - Size of arrays X & Y.
C	 NPTS - No. of pairs of data points (NPTS.LE.M, necessarily).
C	 XMIN - Miminum X range of plot.
C	 XMAX - Maximum X range of plot.
C	 YMIN - Minimum Y range of plot.
C	 YMAX - Maximum Y range of plot.
C	 WSET - Determines if WINDOL is to be set by this routine;
C	        = .TRUE. - Set WINDOL.
C	        = .FALSE. - Do not set WINDOL.
C
C	 Note: This routine assumes no particular ordering of the
C	       points.
C
C	 A C Davenhall. ROE.		8/9/81.
C-
	REAL X(M),Y(M)
	REAL XMIN,XMAX,YMIN,YMAX,BIT,XBIT,YBIT
	INTEGER M,NPTS
	LOGICAL WSET
	DATA BIT/5.0E-2/
C
C	 Fine X & Y extrema of points input.
C
	XMIN=X(1)
	XMAX=X(1)
	YMIN=Y(1)
	YMAX=Y(1)
	DO I=2,NPTS
	   XMIN=AMIN1(XMIN,X(I))
	   XMAX=AMAX1(XMAX,X(I))
	   YMIN=AMIN1(YMIN,Y(I))
	   YMAX=AMAX1(YMAX,Y(I))
	END DO
C
C	 Fine the X & Y ranges & add 5% to each  to set up
C	 extrema for plotting.
C
	XRANGE=XMAX-XMIN
	YRANGE=YMAX-YMIN
	XBIT=XRANGE*BIT
	YBIT=YRANGE*BIT
	XMIN=XMIN-XBIT
	XMAX=XMAX+XBIT
	YMIN=YMIN-YBIT
	YMAX=YMAX+YBIT
C
C	 Set WINDOL if required.
C
	IF (WSET) CALL WINDOL (XMIN,XMAX,YMIN,YMAX)
	END
