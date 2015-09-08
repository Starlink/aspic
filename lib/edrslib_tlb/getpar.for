      SUBROUTINE GETPAR(NAME,TYPE,IREPLY,BOTLIM,TOPLIM,
     +                  DFLT,IVAL,RVAL,IERR)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO OBTAIN NUMERICAL PARAMETER VALUES FROM THE ENVIRONMENT, CHECK
*	THEIR VALIDITY AND HANDLE ANY ERRORS WHICH OCCUR
*
*METHOD
*	OBTAIN VALUE (EITHER REAL OF INTEGER) AND CHECK IT IS VALID.
*	IF NOT, GIVE A MESSAGE AND PROMPT FOR A NEW VALUE
*
*ARGUMENTS
*	NAME (IN)
*	CHARACTER*(*)
*		THE PARAMETER NAME
*	TYPE (IN)
*	CHARACTER*(*)
*		'REAL' OR 'INTEGER'... SPECIFIES THE PARAMETER TYPE
*	IREPLY (IN)
*	INTEGER
*		IF SET TO 1 OR MORE, THE ROUTINE ISSUES HELPFUL MESSAGES
*		IF INVALID VALUES ARE GIVEN
*	BOTLIM,TOPLIM (IN)
*	REAL
*		DEFINE THE VALID DATA RANGE. IF BOTLIM.LE.TOPLIM IT IS
*		AN ALLOWED RANGE, OTHERWISE IT IS AN EXCLUDED RANGE.
*	DFLT (IN)
*	LOGICAL
*		IF .TRUE., THE ROUTINE USES THE INPUT PARAMETER VALUES
*		AS RUN-TIME DEFAULTS
*	IVAL (IN/OUT)
*	INTEGER
*		THE INTEGER PARAMETER VALUE, IF USED
*	RVAL (IN/OUT)
*	REAL
*		THE REAL PARAMETER VALUE, IF USED
*	IERR (OUT)
*	INTEGER
*		ERROR FLAG: ZERO FOR SUCCESS
*
*STARLINK PARAMETERS
*	'NAME'
*		THE PARAMETER NAME IS GIVEN IN THE ARGUMENT 'NAME'
*	BADVALUE/ERROR/
*		ACCESSED IF AN INVALID VALUE IS ENTERED
*
*CALLS
*	THIS PACKAGE:
*		RNGERR
*	STARLINK:
*		RDKEYR,WRERR,CNPAR
*
*WRITTEN BY
*	R.F. WARREN-SMITH
*-----------------------------------------------------------------------
C
C
      CHARACTER NAME*(*),TYPE*(*)
      LOGICAL DFLT,GOODPR
      REAL RNUM(1)
      INTEGER INUM(1),CNERR
C
C SET MAX. NUMBER OF WRONG ENTRIES ALLOWED
C
      PARAMETER (MAXWNG=4)
      NWRONG=0
      IERR=0
C
C IF PARAMETER IS REAL...
C --------------------
C
	IF(TYPE.EQ.'REAL') THEN
C
C LOOP WHILE PARAMETER IS NOT OK
C
	  GOODPR=.FALSE.
   19     IF(.NOT.GOODPR) THEN
C
C SET DEFAULT VALUE, THEN OBTAIN A NEW VALUE FROM THE ENVIRONMENT
C
	    RNUM(1)=RVAL
            CALL RDKEYR(NAME,DFLT,1,RNUM,NVAL,ISTAT)
	    GOODPR=.TRUE.
C
C IF NULL WAS ENTERED AND NO DEFAULT WAS INDICATED, SET IERR=1 AND EXIT
C
	    IF(ISTAT.NE.0) THEN
	      IF(.NOT.DFLT) IERR=1
	    ELSE
C
C IF VALUE LIES OUTSIDE THE PERMITTED RANGE, GIVE MESSAGE AND RETURN
C FOR A NEW VALUE
C
	      IF(TOPLIM.GE.BOTLIM.AND.
     +        ((RNUM(1).GT.TOPLIM).OR.(RNUM(1).LT.BOTLIM))
     +        ) THEN
		CALL WRERR('BADVALUE')
		IF(IREPLY.GE.1) CALL RNGERR('***REAL VALUE','REAL',
     +		BOTLIM,TOPLIM)
		GOODPR=.FALSE.
		CALL CNPAR(NAME,CNERR)
C
C IF VALUE LIES INSIDE AN EXCLUDED RANGE, GIVE MESSAGE AND RETURN FOR
C A NEW VALUE
C
	      ELSE IF(TOPLIM.LT.BOTLIM.AND.
     +        ((RNUM(1).GT.TOPLIM).AND.(RNUM(1).LT.BOTLIM))
     +        ) THEN
		CALL WRERR('BADVALUE')
		IF(IREPLY.GE.1) CALL RNGERR('***REAL VALUE','REAL',
     +		BOTLIM,TOPLIM)
                GOODPR=.FALSE.
		CALL CNPAR(NAME,CNERR)
	      ELSE
C
C OTHERWISE VALUE IS OK
C
		RVAL=RNUM(1)
	      ENDIF
	    ENDIF
C
C IF PARAMETER WAS BAD, BUT MAX NUMBER OF BAD VALUES WAS NOT REACHED,
C RETURN FOR A NEW VALUE. OTHERWISE EXIT WITH INPUT VALUE UNCHANGED
C
	    IF(.NOT.GOODPR) NWRONG=NWRONG+1
	    IF(NWRONG.GE.MAXWNG) THEN
              IF(.NOT.DFLT) IERR=1
              GOODPR=.TRUE.
	    ENDIF
	    GO TO 19
	  ENDIF
	ELSE
C
C IF PARAMETER IS INTEGER...
C -----------------------
C REPEAT THE ABOVE WITH INTEGER VALUES
C
	  GOODPR=.FALSE.
   29     IF(.NOT.GOODPR) THEN
	    INUM(1)=IVAL
	    CALL RDKEYI(NAME,DFLT,1,INUM,NVAL,ISTAT)
	    GOODPR=.TRUE.
	    IF(ISTAT.NE.0) THEN
	      IF(.NOT.DFLT) IERR=1
	    ELSE
	      IF(TOPLIM.GE.BOTLIM.AND.
     +        ((INUM(1).GT.NINT(TOPLIM)).OR.(INUM(1).LT.
     +        NINT(BOTLIM)))) THEN
                CALL WRERR('BADVALUE')
		IF(IREPLY.GE.1) CALL RNGERR('***INTEGER VALUE','INTEGER'
     +		,BOTLIM,TOPLIM)
		GOODPR=.FALSE.
		CALL CNPAR(NAME,CNERR)
	      ELSE IF(TOPLIM.LT.BOTLIM.AND.
     +        ((INUM(1).GT.NINT(TOPLIM)).AND.
     +        (INUM(1).LT.NINT(BOTLIM)))) THEN
		CALL WRERR('BADVALUE')
		IF(IREPLY.GE.1) CALL RNGERR('***INTEGER VALUE','INTEGER'
     +		,BOTLIM,TOPLIM)
		GOODPR=.FALSE.
		CALL CNPAR(NAME,CNERR)
	      ELSE
		IVAL=INUM(1)
	      ENDIF
	    ENDIF
	    IF(.NOT.GOODPR) NWRONG=NWRONG+1
            IF(NWRONG.GE.MAXWNG) THEN
	      IF(.NOT.DFLT) IERR=1
	      GOODPR=.TRUE.
	    ENDIF
	    GO TO 29
	  ENDIF
	ENDIF
      RETURN
      END
