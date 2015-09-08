      SUBROUTINE DSPSCL(IA,NPIX,NLINES,INVAL,SCALE,ZERO,AMIN,AMAX,
     +			IMIN,IMAX,IB)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO MAP A GIVEN DATA RANGE IN ONE IMAGE LINEARLY ON TO ANOTHER 
*	RANGE OF INTEGERS IN A SECOND IMAGE.
*
*METHOD
*	APPLY THE LINEAR TRANSFORMATION TO THE IMAGE INTEGERS, ALLOWING
*	THE OUTPUT DATA VALUES TO SATURATE IF THE OUTPUT RANGE IS
*	EXCEEDED. INVALID PIXELS ARE SET TO THE LOWER LIMIT OF THE
*	OUTPUT RANGE
*
*ARGUMENTS
*	IA (IN)
*	INTEGER*2(NPIX*NLINES)
*		THE INPUT IMAGE
*	NPIX,NLINES (IN)
*	INTEGER
*		THE DIMENSIONS OF IA
*	INVAL (IN)
*	INTEGER
*		INVALID PIXEL FLAG FOR IA
*	SCALE, ZERO (IN)
*	REAL
*		SCALE AND ZERO LEVEL FOR INPUT IMAGE
*	AMIN,AMAX (IN)
*	REAL
*		RANGE OF INPUT DATA VALUES TO BE MAPPED ON TO THE OUTPUT
*		INTEGERS
*	IMIN,IMAX (IN)
*	INTEGER
*		RANGE OF OUTPUT INTEGERS TO BE PRODUCED
*	IB (OUT)
*	INTEGER*2(NPIX*NLINES)
*		THE OUTPUT IMAGE
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
      INTEGER*2 IA(NPIX*NLINES),IB(NPIX*NLINES)
C
C CALCULATE CONVERSION CONSTANTS BETWEEN INPUT AND OUTPUT INTEGERS
C
      BMIN=IMIN
      BMAX=IMAX
      EPS=0.5*SCALE
      DELTAA=AMAX-AMIN
      IF(ABS(DELTAA).LT.EPS) DELTAA=SIGN(EPS,DELTAA)
      CONSTA=SCALE*(IMAX-IMIN)/DELTAA
      CONSTB=((ZERO-AMIN)*(IMAX-IMIN)/DELTAA)+IMIN
C
C SCAN THE IMAGE, CONVERTING THE INTEGERS
C
      DO 33 I=1,NPIX*NLINES
C
C INVALID PIXELS ARE SET TO IMIN
C
	IF(IA(I).EQ.INVAL) THEN
	  IB(I)=IMIN
	ELSE
	  B=MIN(MAX(BMIN,CONSTA*IA(I)+CONSTB),BMAX)
	  IB(I)=NINT(B)
	ENDIF
   33 CONTINUE
      END
