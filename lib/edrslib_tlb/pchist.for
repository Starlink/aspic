      SUBROUTINE PCHIST(IA,NPIX,NLINES,INVAL,PC,IPC,NPC,IHIST,MININT,
     +			MAXINT,IERR)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO FIND THE INTEGER VALUE IN AN IMAGE CORRESPONDING TO A
*	SPECIFIC FRACTION OF THE IMAGE HISTOGRAM FROM 0.0 TO 1.0
*
*METHOD
*	FORM A HISTOGRAM OF THE IMAGE INTEGERS AND SCAN UP OR DOWN TO
*	FIND THE APPROPRIATE POINT
*
*ARGUMENTS
*	IA (IN)
*	INTEGER*2(NPIX*NLINES)
*		THE INPUT IMAGE
*	NPIX,NLINES (IN)
*	INTEGER
*		DIMENSIONS OF IA
*	INVAL (IN)
*	INTEGER
*		INVALID PIXEL FLAG FOR IA
*	PC (IN)
*	REAL(NPC)
*		ARRAY OF FRACTIONAL POSITIONS IN THE HISTOGRAM IN THE
*		RANGE 0.0 TO 1.0
*	IPC (OUT)
*	INTEGER(NPC)
*		ARRAY OF INTEGER RESULTS CORRESPONDING TO THE VALUES IN
*		THE ARRAY PC
*	NPC (IN)
*	INTEGER
*		DIMENSION OF PC, IPC
*	IHIST (WORKSPACE)
*	INTEGER(MININT:MAXINT)
*		USED TO HOLD THE IMAGE HISTOGRAM. CONTAINS HISTOGRAM
*		ON EXIT
*	MININT,MAXINT (IN)
*	INTEGER
*		MIN AND MAX INTEGER VALUES POSSIBLE IN IA
*
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
      INTEGER*2 IA(NPIX*NLINES)
      INTEGER IPC(NPC),IHIST(MININT:MAXINT)
      REAL PC(NPC)
C
C INITIALLISE HISTOGRAM
C
      IERR=0
      NN=NPIX*NLINES
      DO 1 I=MININT,MAXINT
	IHIST(I)=0
    1 CONTINUE
C
C FORM A HISTOGRAM OF ALL THE VALID PIXELS
C
      NPTS=0
      DO 2 I=1,NN
	INTGR=IA(I)
	IF(INTGR.NE.INVAL) THEN
	  IHIST(INTGR)=IHIST(INTGR)+1
	  NPTS=NPTS+1
	ENDIF
    2 CONTINUE
C
C IF THERE ARE NO VALID PIXELS, EXIT WITH ERROR FLAG SET
C
      IF(NPTS.EQ.0) THEN
        IERR=1
        GO TO 99
      ENDIF
C
C CONSIDER EACH PERCENTAGE HISTOGRAM POINT
C
      DO 4 I=1,NPC
C
C CALCULATE THE NUMBER OF DATA POINTS CORRESPONDING TO THIS POINT
C COUNTING UP OR DOWN DEPENDING ON WHICH SIDE OF THE MEDIAN
C
	IF(PC(I).LE.0.5) THEN
	  LIMIT=NINT(NPTS*PC(I))
	  ISTART=MININT
	  IEND=MAXINT
	  IDIRN=1
	ELSE
	  LIMIT=NINT(NPTS*(1.0-PC(I)))
	  ISTART=MAXINT
	  IEND=MININT
	  IDIRN=-1
	ENDIF
C
C COUNT THROUGH HISTOGRAM TO FIND THIS POINT
C
	N=0
	DO 3 J=ISTART,IEND,IDIRN
	  N=N+IHIST(J)
	  IF(N.GE.LIMIT) GO TO 8
    3	CONTINUE
    8   IPC(I)=J
    4 CONTINUE
   99 RETURN
      END
