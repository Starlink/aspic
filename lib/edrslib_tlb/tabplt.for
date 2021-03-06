      SUBROUTINE TABPLT(TABLE,NTAB,BOTLIM,TOPLIM,TITLE,IDEV,WORK)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO PLOT A GRAPH OF AN ITF LOOK-UP TABLE USING DRPLOT ROUTINES
*
*METHOD
*	GENERATE ARRAYS TO PLOT, CONVERT TITLE TO A BYTE STRING. CALL
*	DRPLOT ROUTINES TO PLOT GRAPH ON CHOSEN GRAPHICS DEVICE.
*	WAIT FOR USER RESPONSE BEFORE FINISHING AND CLEARING SCREEN
*
*ARGUMENTS
*	TABLE (IN)
*	REAL(NTAB)
*		LOOK UP TABLE
*	NTAB (IN)
*	INTEGER
*		NUMBER OF TABLE ENTRIES
*	BOTLIM,TOPLIM (IN)
*	REAL
*		LOWER AND UPPER LIMITS OF INPUT DATA FOR LOOK UP TABLE
*	TITLE (IN)
*	CHARACTER*(*)
*		MAIN TITLE FOR PLOT (UP TO FIRST 30 CHARACTERS USED)
*	IDEV (IN)
*	INTEGER
*		SELECTS GRAPHICS DEVICE
*		1:ARGS
*		2:TEKTRONIX
*		3:GOC
*	WORK (WORKSPACE)
*	REAL(NTAB)
*		USED TO STORE ARRAY OF X COORDINATES TO PLOT
*
*CALLS
*	STARLINK:
*		RDUSER
*	HIGR GRAPHICS:
*		HIGR_GZBGN,HIGR_DRPLOT,HIGR_GZEND
*
*NOTES
*	USES BYTE ARRAYS AND SUBROUTINE NAMES LONGER THAN 6 CHARACTERS
*
*WRITTEN BY
*	R.F.WARREN-SMITH
*-----------------------------------------------------------------------
C
C
      CHARACTER TITLE*(*),TEXT*80
      BYTE TTLINT(31)
      REAL TABLE(NTAB),WORK(NTAB)
C
C GENERATE THE X COORDINATES FOR EACH TABLE ENTRY
C
      XINTVL=(TOPLIM-BOTLIM)/MAX(1,NTAB-1)
      DO 1 I=1,NTAB
	WORK(I)=BOTLIM+(I-1)*XINTVL
    1 CONTINUE
C
C COPY UP TO 30 CHARACTERS OF TITLE TO A BYTE ARRAY TO PASS TO DRPLOT
C
      DO 2 I=1,MIN(30,LEN(TITLE))
	TTLINT(I)=ICHAR(TITLE(I:I))
    2 CONTINUE
C
C ADD TERMINATION CHARACTER
C
      TTLINT(I)=ICHAR('#')
C
C CALL PLOTTING ROUTINES TO PLOT GRAPH
C
      CALL HIGR_GZBGN(4,0,IDEV,2)
      CALL HIGR_DRPLOT(WORK,TABLE,NTAB,'INPUT DATA VALUE#',
     +'OUTPUT DATA VALUE#',TTLINT,'#')
C
C WAIT FOR USER RESPONSE BEFORE CLOSING DOWN
C
      CALL RDUSER(TEXT,ISTAT)
      CALL HIGR_GZEND(4)
      END
