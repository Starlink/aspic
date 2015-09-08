      SUBROUTINE IMGBOX(IA,NPIX,NLINES,INVAL,IX,IY,NMIN,ISTOR,
     +			NSTOR,ILINE,NLINE)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO SMOOTH AN IMAGE USING A RECTANGULAR 'LOCAL MEAN' FILTER
*
*METHOD
*	SMOOTH EACH LINE BY RUNNING A LOCAL MEAN FILTER THROUGH IT.
*	STORE THE RESULT IN WORKSPACE, THEN REPEAT THE PROCESS DOWN
*	THE IMAGE COLUMNS. TAKE ACCOUNT OF INVALID PIXELS.
*
*ARGUMENTS
*	IA (IN)
*	INTEGER*2(NPIX,NLINES)
*		INPUT IMAGE
*	NPIX,NLINES (IN)
*	INTEGER
*		DIMENSIONS OF IMAGE
*	INVAL (IN)
*	INTEGER
*		INVALID PIXEL FLAG FOR IA
*	IX,IY (IN)
*	INTEGER
*		SIZE OF SMOOTHING RECTANGLE IN X,Y
*	NMIN (IN)
*	INTEGER
*		MIN NUMBER OF VALID PIXELS NEEDED IN SMOOTHING AREA
*	ISTOR (WORKSPACE)
*	INTEGER(NPIX,NLINES)
*		INTERMEDIATE STORAGE
*	NSTOR (WORKSPACE)
*	INTEGER*2(NPIX,NLINES)
*		INTERMEDIATE STORAGE
*	ILINE (WORKSPACE)
*	INTEGER(NPIX)
*		INTERMEDIATE STORAGE
*	NLINE (WORKSPACE)
*	INTEGER*2(NPIX)
*		INTERMEDIATE STORAGE
*
*CALLS
*	NONE
*
*NOTES
*	USES INTEGER*2 ARRAYS
*
*WRITTEN BY
*	R.F. WARREN-SMITH
*-----------------------------------------------------------------------
C
C
      INTEGER*2 IA(NPIX,NLINES),NSTOR(NPIX,NLINES),NLINE(NPIX)
      INTEGER ISTOR(NPIX,NLINES),ILINE(NPIX),OLDX,NEWX,OLDY,NEWY,
     +        THRESH
      NPIX2=2*NPIX
      NLINE2=2*NLINES
C
C MAKE BOX SIZES NEXT LARGEST ODD NUMBER AND POSITIVE
C
      IDX=MAX(0,IX/2)
      ISIDE=2*IDX+1
      IDY=MAX(0,IY/2)
      JSIDE=2*IDY+1
C
C SET THRESHOLD FOR NO. OF GOOD PIXELS IN BOX BETWEEN 1 AND MAX 
C POSSIBLE NUMBER
C
      THRESH=MIN(MAX(1,NMIN),ISIDE*JSIDE)
C
C FIRST SMOOTH ALONG EACH LINE
C ----------------------------
C
      DO 101 J=1,NLINES
C
C INITIALLISE RUNNING SUM OF DATA (ISUM) AND NO. OF GOOD PIXELS (NSUM)
C
	ISUM=0
	NSUM=0
C
C START WITH BOX SIZE ISIDE*1 CENTRED ON PIXEL (0,J)
C
	DO 102 II=-IDX,IDX
	  I=II
C
C BOX WILL PROJECT OFF ENDS OF LINES, SO REFLECT TO KEEP IT INSIDE
C
	  IF(I.LT.1) I=2-I
	  IF(I.GT.NPIX) I=NPIX2-I
C
C PROTECT AGAINST EXTREME CASES WHERE BOX IS SO BIG IT GOES OFF
C OPPOSITE END AFTER REFLECTION
C
	  I=MIN(MAX(1,I),NPIX)
C
C IF PIXEL FOUND IS VALID, ADD IT TO SUM OF PIXELS WITHIN BOX
C
	  IF(IA(I,J).NE.INVAL) THEN
	    ISUM=ISUM+IA(I,J)
	    NSUM=NSUM+1
	  ENDIF
  102   CONTINUE
C
C NOW STEP THE BOX ALONG THE LINE
C
	DO 106 I=1,NPIX
C
C FIND POSITION OF OLD PIXEL TO BE REMOVED FROM LEFT AND NEW PIXEL
C TO BE ADDED AT RIGHT
C
	  OLDX=I-IDX-1
	  NEWX=I+IDX
C
C REFLECT AT ENDS OF LINE
C
	  IF(OLDX.LT.1) OLDX=2-OLDX
	  IF(OLDX.GT.NPIX) OLDX=NPIX2-OLDX
	  OLDX=MIN(MAX(1,OLDX),NPIX)
	  IF(NEWX.LT.1) NEWX=2-NEWX
	  IF(NEWX.GT.NPIX) NEWX=NPIX2-NEWX
	  NEWX=MIN(MAX(1,NEWX),NPIX)
C
C IF OLD PIXEL IS VALID, SUBTRACT FROM SUMS
C
	  IF(IA(OLDX,J).NE.INVAL) THEN
	    ISUM=ISUM-IA(OLDX,J)
	    NSUM=NSUM-1
	  ENDIF
C
C IF NEW PIXEL IS VALID, ADD TO SUMS
C
	  IF(IA(NEWX,J).NE.INVAL) THEN
	    ISUM=ISUM+IA(NEWX,J)
	    NSUM=NSUM+1
	  ENDIF
C
C STORE SUMS ALONG LINE IN WORKSPACE
C
	  ISTOR(I,J)=ISUM
	  NSTOR(I,J)=NSUM
  106   CONTINUE
  101 CONTINUE
C
C NOW SMOOTH DOWN COLUMNS
C -----------------------
C
C INITIALLISE SUMS.. THIS TIME PROCESSING A WHOLE LINE AT ONCE
C
      DO 16 I=1,NPIX
	ILINE(I)=0
	NLINE(I)=0
   16 CONTINUE
C
C SUM OVER A BOX OF SIZE 1*JSIDE CENTRED ON PIXEL (I,0), WHERE I
C SCANS ALONG WHOLE LINE
C
      DO 602 JJ=-IDY,IDY
	J=JJ
C
C REFLECT AT TOP AND BOTTOM OF IMAGE
C
	IF(J.LT.1) J=2-J
	IF(J.GT.NLINES) J=NLINE2-J
	J=MIN(MAX(1,J),NLINES)
C
C FORM A SUM FOR EACH PIXEL IN THE LINE FROM THE DATA NOW STORED IN
C THE WORKSPACE
C
	DO 161 I=1,NPIX
	  ILINE(I)=ILINE(I)+ISTOR(I,J)
	  NLINE(I)=NLINE(I)+NSTOR(I,J)
  161   CONTINUE
  602 CONTINUE
C
C NOW STEP DOWN THE IMAGE
C
      DO 606 J=1,NLINES
C
C FIND LOCATION OF OLD LINE TO SUBTRACT AT TOP AND NEW LINE
C TO ADD AT BOTTOM
C
	OLDY=J-IDY-1
	NEWY=J+IDY
C
C REFLECT AT TOP AND BOTTOM OF IMAGE
C
	IF(OLDY.LT.1) OLDY=2-OLDY
	IF(OLDY.GT.NLINES) OLDY=NLINE2-OLDY
	OLDY=MIN(MAX(1,OLDY),NLINES)
	IF(NEWY.LT.1) NEWY=2-NEWY
	IF(NEWY.GT.NLINES) NEWY=NLINE2-NEWY
	NEWY=MIN(MAX(1,NEWY),NLINES)
C
C TAKE OFF OLD LINE
C
	DO 162 I=1,NPIX
	  ILINE(I)=ILINE(I)-ISTOR(I,OLDY)
	  NLINE(I)=NLINE(I)-NSTOR(I,OLDY)
C
C ADD NEW LINE
C
	  ILINE(I)=ILINE(I)+ISTOR(I,NEWY)
	  NLINE(I)=NLINE(I)+NSTOR(I,NEWY)
C
C IF SUFFICIENT PIXELS PRESENT, FORM OUTPUT, OTHERWISE OUTPUT IS
C NOT VALID
C
	  IF(NLINE(I).GE.THRESH) THEN
	    IA(I,J)=NINT(REAL(ILINE(I))/NLINE(I))
	  ELSE
	    IA(I,J)=INVAL
	  ENDIF
  162   CONTINUE
  606 CONTINUE
      RETURN
      END
