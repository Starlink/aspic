      SUBROUTINE LINFIL(IA,NPIX,NLINES,INVALA,IMETH,NMIN,INVALB,NBAD,
     +			IB)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO EVALUATE AN IMAGE, EACH LINE OF WHICH IS A CONSTANT OR
*	STRAIGHT LINE LEAST-SQUARES FIT TO THE CORRESPONDING INPUT
*	IMAGE LINE
*
*METHOD
*	SCAN EACH INPUT IMAGE LINE, FORMING THE REQUIRED SUMS FOR
*	PERFORMING THE FIT, USING ONLY THE VALID PIXELS. IF SUFFICIENT
*	PIXELS ARE VALID, EVALUATE THE FIT OVER THE OUTPUT LINE. 
*	OTHERWISE SET THE OUTPUT LINE INVALID.
*
*ARGUMENTS
*	IA (IN)
*	INTEGER*2(NPIX,NLINES)
*		THE INPUT IMAGE
*	NPIX,NLINES (IN)
*	INTEGER
*		THE DIMENSIONS OF THE IMAGES IA AND IB
*	INVALA (IN)
*	INTEGER
*		INVALID PIXEL FLAG FOR IA
*	IMETH (IN)
*	INTEGER
*		METHOD OF FITTING THE IMAGE LINES
*			1: CONSTANT
*			2: STRAIGHT LINE
*	NMIN (IN)
*	INTEGER
*		MINIMUM NUMBER OF VALID INPUT PIXELS REQUIRED IN EACH
*		LINE FOR THE OUTPUT LINE TO BE VALID
*	INVALB (IN)
*	INTEGER
*		INVALID PIXEL FLAG FOR IB
*	NBAD (OUT)
*	INTEGER
*		NUMBER OF LINES NOT FITTED DUE TO TOO MANY INVALID
*		PIXELS
*	IB (OUT)
*	INTEGER*2(NPIX,NLINES)
*		OUTPUT IMAGE
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
      INTEGER*2 IA(NPIX,NLINES),IB(NPIX,NLINES)
      DOUBLE PRECISION S0,S1,S2,T0,T1,X
C
C MINIMUM PIXELS PER LINE MUST LIE IN THE RANGE IMETH TO NPIX
C
      MINPIX=MIN(MAX(IMETH,NMIN),NPIX)
      NBAD=0
C
C SCAN THROUGH THE IMAGE LINES
C
      DO 100 J=1,NLINES
C
C FOR FITTING A CONSTANT
C ----------------------
C
	IF(IMETH.EQ.1) THEN
C
C FORM SUMS FOR THE MEAN OF ALL THE VALID PIXELS
C
	  SUM=0.0
	  NPT=0
	  DO 6 I=1,NPIX
	    IF(IA(I,J).NE.INVALA) THEN
	      SUM=SUM+IA(I,J)
	      NPT=NPT+1
	    ENDIF
    6	  CONTINUE
C
C IF THERE ARE SUFFICIENT VALID PIXELS, SET THE OUTPUT LINE TO THE
C MEAN VALUE
C
	  IF(NPT.GE.MINPIX) THEN
	    IBVAL=NINT(SUM/NPT)
	    DO 7 I=1,NPIX
	      IB(I,J)=IBVAL
    7	    CONTINUE
C
C IF THERE ARE INSUFFICIENT VALID PIXELS, THE OUTPUT LINE IS INVALID
C COUNT 1 BAD LINE
C
	  ELSE
	    NBAD=NBAD+1
	    DO 8 I=1,NPIX
	      IB(I,J)=INVALB
    8	    CONTINUE
	  ENDIF
C
C FOR STRAIGHT LINE FIT
C ---------------------
C
	ELSE IF(IMETH.EQ.2) THEN
C
C INITIALLISE SUMS FOR LEAST-SQUARES FIT
C
	  NPT=0
	  S1=0.0D0
	  S2=0.0D0
	  T0=0.0D0
	  T1=0.0D0
C
C FORM THE SUMS OVER THE VALID PIXELS
C
	  DO 16 I=1,NPIX
	    IF(IA(I,J).NE.INVALA) THEN
	      X=I
	      NPT=NPT+1
	      S1=S1+X
	      S2=S2+X*X
	      T0=T0+IA(I,J)
	      T1=T1+IA(I,J)*X
	    ENDIF
   16	  CONTINUE
	  S0=NPT
C
C IF THERE ARE SUFFICIENT VALID PIXELS, CALCULATE THE COEFFICIENTS
C OF THE STRAIGHT LINE FIT
C
	  IF(NPT.GE.MINPIX) THEN
	    A=(S0*T1-S1*T0)/(S0*S2-S1*S1)
	    B=(S2*T0-S1*T1)/(S0*S2-S1*S1)
C
C EVALUATE THE FIT OVER THE OUTPUT LINE
C
	    DO 17 I=1,NPIX
	      VALUE=A*I+B
C
C OUTPUT PIXEL IS INVALID IF OVERFLOW OCCURS
C
	      IF(ABS(VALUE).LE.32767.0) THEN
	        IB(I,J)=NINT(VALUE)
	      ELSE
	        IB(I,J)=INVALB
	      ENDIF
   17	    CONTINUE
C
C IF THERE ARE INSUFFICIENT VALID PIXELS, OUTPUT LINE IS INVALID
C COUNT 1 BAD LINE
C
	  ELSE
	    NBAD=NBAD+1
	    DO 18 I=1,NPIX
	      IB(I,J)=INVALB
   18	    CONTINUE
	  ENDIF
        ENDIF
  100 CONTINUE
      END
