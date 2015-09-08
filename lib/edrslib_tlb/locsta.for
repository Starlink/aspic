      SUBROUTINE LOCSTA(IA,NPIX,NLINES,INVAL,X0,Y0,ISIZE,X,Y,
     +			SIGMA,IERR)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO ACCURATELY LOCATE THE CENTRE OF A STAR IMAGE OF KNOWN SIZE
*
*METHOD
*	FORM X AND Y MARGINAL PROFILES OF THE STAR IMAGE USING THE
*	DATA WITHIN A SQUARE SEARCH AREA. CALL CLNSTA TO REMOVE THE
*	BACKGROUNDS AND NEIGHBOURING STARS, DIRT, ETC. FROM THESE
*	PROFILES. FIT A GAUSSIAN OF A SPECIFIED WIDTH TO EACH PROFILE
*	USING GAUFIS TO FIND THE CENTRES IN X AND Y. PERFORM 3 SUCH
*	ITERATIONS, CENTRING THE SEARCH SQUARE ON THE PREVIOUS POSITION
*	EACH TIME. IN FORMING THE MARGINAL PROFILES, A WEIGHTING
*	FUNCTION IS USED IN THE ORTHOGONAL DIRECTION TO EXCLUDE
*	NEIGHBOUTING STARS, ETC. AS FAR AS POSSIBLE.
*
*ARGUMENTS
*	IA (IN)
*	INTEGER*2(NPIX,NLINES)
*		THE IMAGE CONTAINING THE STARS
*	NPIX,NLINES (IN)
*	INTEGER
*		THE IMAGE DIMENSIONS
*	INVAL (IN)
*	INTEGER
*		INVALID PIXEL FLAG FOR IMAGE IA
*	X0,Y0 (IN)
*	REAL
*		THE INITIAL APPROXIMATE STAR POSITION
*	ISIZE (IN)
*	INTEGER
*		THE LENGTH OF THE SIDE OF THE SQUARE SEARCH AREA
*	X,Y (OUT)
*	REAL
*		THE ACCURATE CENTRE OF THE STAR
*	SIGMA (IN)
*	REAL
*		AN ESTIMATE OF THE 'SIGMA' OF THE STAR IMAGE
*	IERR (OUT)
*	INTEGER
*		ERROR FLAG: ZERO FOR SUCCESS
*
*CALLS
*	THIS PACKAGE:
*		CLNSTA,GAUFIS
*
*NOTES
*	USES INTEGER*2 ARRAYS
*
*WRITTEN BY
*	R.F. WARREN-SMITH
*-----------------------------------------------------------------------
C
C
C
C SET THE MAXIMUM SEARCH SQUARE SIZE 
C
      PARAMETER (MAXSIZ=101,
     +		 NITER=3,NGAUIT=20,TOLL=0.005)
      INTEGER*2 IA(NPIX,NLINES)
      REAL XDATA(MAXSIZ),YDATA(MAXSIZ),XN(MAXSIZ),YN(MAXSIZ),
     +	   XW(MAXSIZ),YW(MAXSIZ)
C
C MAKE THE SQUARE SIZE ODD AND NOT GREATER THAN MAXSIZ
C
      IX=MIN(ISIZE,MAXSIZ)
      IDX=MAX(1,IX/2)
      X=X0
      Y=Y0
      IERR=0
C
C PERFORM NITER ITERATIONS, EACH TIME CENTERING THE SEARCH SQUARE
C ON THE PREVIOUS ESTIMATE OF THE IMAGE CENTRE
C
      DO 97 ITER=1,NITER
C
C CALCULATE THE EDGES OF THE SEARCH SQUARE
C
	IMIN=MAX(1,NINT(MIN(MAX(-1.0E8,X),1.0E8))-IDX)
	IMAX=MIN(NPIX,NINT(MIN(MAX(-1.0E8,X),1.0E8))+IDX)
	JMIN=MAX(1,NINT(MIN(MAX(-1.0E8,Y),1.0E8))-IDX)
	JMAX=MIN(NLINES,NINT(MIN(MAX(-1.0E8,Y),1.0E8))+IDX)
	NX=IMAX-IMIN+1
	NY=JMAX-JMIN+1
C
C IF THE SEARCH SQUARE LIES COMPLETELY OUTSIDE THE IMAGE, SET IERR=2
C AND QUIT
C
	IF(NX.LT.1.OR.NY.LT.1) THEN
	  IERR=2
	  GO TO 99
	ENDIF
C
C CALCULATE A WEIGHTING FUNCTION TO BE USED IN THE X DIRECTION
C WHEN FORMING THE Y MARGINAL PROFILE
C
	RSIG=1.0/SIGMA
	DX=(IMIN-1)-X
	DO 20 I=1,NX
	  DX=DX+1.0
	  DEV=DX*RSIG
	  DEV2=DEV*DEV
C
C THE WEIGHTING FUNCTION FALLS AT LARGE DISTANCES FROM THE CENTRE
C TO EXCLUDE NEARBY STARS, ETC.
C
	  IF(DEV2.LE.25.0) THEN
	    XW(I)=EXP(-0.5*DEV2)
	  ELSE
	    XW(I)=0.0
	  ENDIF
C
C INITIALLISE ARRAYS FOR FORMING THE X MARGINAL PROFILE
C
	  XDATA(I)=0.0
	  XN(I)=0.0
   20 	CONTINUE
C
C NOW FORM A SIMILAR WEIGHTING FUNCTION FOR THE Y DIRECTION
C
	DY=(JMIN-1)-Y
	DO 21 J=1,NY
	  DY=DY+1.0
	  DEV=DY*RSIG
	  DEV2=DEV*DEV
	  IF(DEV2.LE.25.0) THEN
	    YW(J)=EXP(-0.5*DEV2)
	  ELSE
	    YW(J)=0.0
	  ENDIF
	  YDATA(J)=0.0
	  YN(J)=0.0
   21	CONTINUE
C
C SCAN THROUGH THE SEARCH SQUARE FORMING THE WEIGHTED MARGINAL
C PROFILES OF THE STAR
C
	JJ=0
	DO 81 J=JMIN,JMAX
	  JJ=JJ+1
	  II=0
	  DO 80 I=IMIN,IMAX
	    II=II+1
	    IF(IA(I,J).NE.INVAL) THEN
	      XDATA(II)=XDATA(II)+YW(JJ)*IA(I,J)
	      YDATA(JJ)=YDATA(JJ)+XW(II)*IA(I,J)
	      XN(II)=XN(II)+YW(JJ)
	      YN(JJ)=YN(JJ)+XW(II)
	    ENDIF
   80 	  CONTINUE
   81   CONTINUE
C
C EVALUATE THE X MARGINAL
C
	DO 70 I=1,NX
	  IF(XN(I).GT.0.0) THEN
	    XDATA(I)=XDATA(I)/XN(I)
	  ELSE
	    XDATA(I)=2.0E20
	  ENDIF
   70	CONTINUE
C
C EVALUATE THE Y MARGINAL
C
	DO 71 J=1,NY
	  IF(YN(J).GT.0.0) THEN
	    YDATA(J)=YDATA(J)/YN(J)
	  ELSE
	    YDATA(J)=2.0E20
	  ENDIF
   71   CONTINUE
C
C CALL CLNSTA TO SUBTRACT THE BACKGROUND FROM EACH PROFILE
C AND TO REMOVE CORRUPT DATA AND NEIGHBOURING STARS
C
	CALL CLNSTA(XDATA,IMIN,IMAX,X)
	CALL CLNSTA(YDATA,JMIN,JMAX,Y)
C
C CALL GAUFIS TO FIT A GAUSSIAN PROFILE OF A SPECIFIED SIGMA
C TO EACH PROFILE TO FIND THE STAR CENTRE
C
	CALL GAUFIS(XDATA,IMIN,IMAX,NGAUIT,TOLL,AX,X,SIGMA,IERR)
	IF(IERR.NE.0) GO TO 99
	CALL GAUFIS(YDATA,JMIN,JMAX,NGAUIT,TOLL,AY,Y,SIGMA,IERR)
	IF(IERR.NE.0) GO TO 99
   97 CONTINUE
   99 CONTINUE
      RETURN
      END
