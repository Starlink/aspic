      SUBROUTINE CLNSTA(X,IMIN,IMAX,XCEN)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO 'CLEAN' A MARGINAL STAR PROFILE, REMOVING NEIGHBOURING
*	STARS AND BLEMISHES
*
*METHOD
*	SUBTRACT LOWER QUARTILE DATA POINT AS A BACKGROUND ESTIMATE
*	WORK OUT FROM CENTRE OF STAR, PREVENTING EACH DATA POINT FROM
*	EXCEEDING THE MAXIMUM OF THE TWO PREVIOUS DATA VALUES. THIS
*	ENSURES THAT THE DATA DOES NOT INCREASE WITH INCREASING
*	DISTANCE FROM THE CENTRE.
*
*ARGUMENTS
*	X (IN/OUT)
*	REAL(IMIN:IMAX)
*		DATA ARRAY..INVALID VALUES ARE SET ABOVE 1.0E20
*	IMIN,IMAX (IN)
*	INTEGER
*		FIRST AND LAST COORDINATES OF DATA ARRAY.. ALSO
*		DEFINE THE DIMENSION OF X
*	XCEN (IN)
*	REAL
*		ESTIMATE OF POSITION OF STAR CENTRE
*
*CALLS
*	THIS PACKAGE:
*		NTHMIN
*
*WRITTEN BY
*	R.F. WARREN-SMITH
*-----------------------------------------------------------------------
C
C
      PARAMETER (NMIN=2)
      REAL X(IMIN:IMAX),STAK(25),SMIN(NMIN)
C
C CALCULATE THE NUMBER OF DATA POINTS AND FIND THE LOWER QUARTILE
C POINT AS A BACKGROUND ESTIMATE
C
      NX=IMAX-IMIN+1
      NQUART=MIN(MAX(1,NINT(NX*0.25)),25)
      CALL NTHMIN(X(IMIN),NX,NQUART,STAK,IERR)
C
C SUBTRACT THE BACKGROUND FROM ALL THE VALID DATA POINTS, FINDING
C THE MAXIMUM VALUE
C
      SMAX=-2.0E20
      DO 17 I=IMIN,IMAX
        IF(X(I).LT.1.0E20) THEN
	  X(I)=X(I)-STAK(1)
	  SMAX=MAX(SMAX,X(I))
	ENDIF
   17 CONTINUE
C
C WORK OUT FROM THE CENTRE OF THE STAR IMAGE TOWARDS EACH END
C OF THE DATA ARRAY IN TURN
C
      I0=NINT(XCEN)
      DO 61 IDIRN=-1,1,2
	IF(IDIRN.LT.0) THEN
	  IEND=IMIN
	ELSE
	  IEND=IMAX
	ENDIF
C
C INITIALLISE THE STORE OF THE LAST NMIN DATA VALUES
C
	DO 81 J=1,NMIN
	  SMIN(J)=SMAX
   81	CONTINUE
	MINLOC=1
C
C WORK THROUGH THE DATA ARRAY, AT EACH POINT CALCULATING AN UPPER
C DATA LIMIT AS THE MAXIMUM OF THE LAST NMIN VALUES
C
	DO 60 I=I0,IEND,IDIRN
	  UPLIM=SMIN(1)
	  DO 82 J=2,NMIN
	    UPLIM=MAX(UPLIM,SMIN(J))
   82	  CONTINUE
C
C LIMIT THE DATA TO THE CURRENT UPPER LIMIT
C
	  X(I)=MIN(UPLIM,X(I))
C
C MINLOC CYCLES FROM 1 TO NMIN AND POINTS TO THE STORE FOR
C THIS VALUE IN THE LIST OF THE LAST NMIN VALUES
C
	  MINLOC=MOD(MINLOC,NMIN)+1
	  IF(X(I).GE.0.0) THEN
	    SMIN(MINLOC)=X(I)
	  ELSE
C
C DON'T USE NEGATIVE VALUES...REPLACE WITH THE CURRENT UPPER LIMIT
C
	    SMIN(MINLOC)=UPLIM
	  ENDIF
   60	CONTINUE
   61 CONTINUE
      RETURN
      END