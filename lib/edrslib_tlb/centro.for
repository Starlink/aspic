      SUBROUTINE CENTRO
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO FIND THE CENTROIDS OF STAR-LIKE IMAGES GIVEN AN INITIAL
*	ESTIMATE OF THEIR POSITIONS
*
*METHOD
*	OBTAIN INPUT IMAGE AND LIST OF POSITIONS AND EXTRACT REQUIRED
*	DESCRIPTOR ITEMS. OBTAIN WORKSPACE AND EXTRACT POSITIONS AND
*	IDENTIFIERS FROM LIST. OBTAIN REQUIRED PARAMETERS FROM ENVIRON-
*	MENT AND CALL LOCLST TO FIND CENTROIDS. OBTAIN OUTPUT DATASET
*	AND ADD OUTPUT POSITIONS TO IT. FORM OUTPUT DESCRIPTOR.
*
*ARGUMENTS
*	NONE
*
*CALLS
*	THIS PACKAGE:
*		GETPAR,GT2DIR,GTDSCR,GTXYLR,EXTLST,GETCMD,LOCLST,GTXYLW,
*		ADDLST,PTDSCR
*	STARLINK:
*		GETDYN,WRERR,CYDSCR,RDKEYC,FRDATA
*
*STARLINK PARAMETERS
*	ILEVEL
*		INTERACTION LEVEL: CONTROLS PRINTING OF RESULTS
*	IMAGE
*		INPUT IMAGE
*	INPUT
*		INPUT LIST OF INITIAL POSITIONS
*	NOSPACE/ERROR/
*		CALLED IF WORKSPACE CANNOT BE OBTAINED
*	ISIZE
*		SIZE OF SEARCH SQUARE FOR FORMING CENTROID
*	ISIGN
*		INDICATES IF STAR-LIKE FEATURES ARE POSITIVE OR
*		NEGATIVE
*	MAXSHIFT
*		MAXIMUM SHIFT FROM INITIAL POSITION
*	MAXITER
*		MAXIMUM NUMBER OF CENTROIDING ITERATIONS
*	TOLERENC
*		ACCURACY TO WHICH CENTROIDS ARE REQUIRED
*	NOLIST/ERROR/
*		CALLED IF NO CENTROIDS CAN BE FOUND
*	OUTPUT
*		OUTPUT LIST OF POSITIONS
*	TITLE
*		TITLE TO REPLACE INPUT TITLE IN OUTPUT POSITION LIST
*
*NOTES
*	USES VAX %VAL FACILITY
*
*WRITTEN BY
*	R.F. WARREN-SMITH
*-----------------------------------------------------------------------
C
C
      CHARACTER CVAL*1,TITLE(1)*30,IDSIGN*8
C
C OBTAIN INTERACTION LEVEL
C
      ILEVEL=2
      CALL GETPAR('ILEVEL','INTEGER',1,1.0,3.0,.TRUE.,ILEVEL,RVAL,IERR)
C
C OBTAIN INPUT IMAGE FRAME
C
      CALL GT2DIR('IMAGE',102,.FALSE.,NPIX,NLINES,IPIMG,IERRIM)
      IF(IERRIM.EQ.0) THEN
C
C IMAGE OBTAINED SUCCESSFULLY... GET INVALID FLAG FROM DESCRIPTOR
C
	INVAL=-100000
	CALL GTDSCR('IMAGE','INVAL','INTEGER',INVAL,RVAL,CVAL,IERR)
C
C OBTAIN INPUT XY LIST OF INITIAL SEARCH POSITIONS
C
 	CALL GTXYLR('INPUT',.FALSE.,NITEM,LSTLEN,IPIN,IERRXY)
	IF(IERRXY.EQ.0) THEN
C
C XY LIST OBTAINED SUCCESSFULLY... EXTRACT TITLE FROM DESCRIPTOR
C
	  TITLE(1)=' '
	  CALL GTDSCR('INPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE(1),
     +    IERR)
C
C OBTAIN VM WORKSPACE FOR INPUT AND OUTPUT XY LISTS AND IDENTIFIERS
C
	  CALL GETDYN('IDA',104,5*LSTLEN,IPIDA,ISTIDA)
	  CALL GETDYN('IDB',104,5*LSTLEN,IPIDB,ISTIDB)
	  CALL GETDYN('XA',104,LSTLEN,IPXA,ISTXA)
	  CALL GETDYN('YA',104,LSTLEN,IPYA,ISTYA)
	  CALL GETDYN('XB',104,LSTLEN,IPXB,ISTXB)
	  CALL GETDYN('YB',104,LSTLEN,IPYB,ISTYB)
C
C IF SPACE WAS NOT AVAILABLE, GIVE MESSAGE AND ABORT
C
	  IF((ISTIDA.NE.0).OR.(ISTIDB.NE.0).OR.
     +    (ISTXA.NE.0).OR.(ISTYA.NE.0).OR.(ISTXB.NE.0).OR.
     +    (ISTYB.NE.0)) THEN
	    CALL WRERR('NOSPACE')
	    GO TO 99
	  ENDIF
C
C COPY INPUT ID,X AND Y TO WORKSPACE
C
	  CALL EXTLST(%VAL(IPIN),NITEM,LSTLEN,%VAL(IPIDA),1,20)
	  CALL EXTLST(%VAL(IPIN),NITEM,LSTLEN,%VAL(IPXA),21,24)
	  CALL EXTLST(%VAL(IPIN),NITEM,LSTLEN,%VAL(IPYA),25,28)
C
C OBTAIN SEARCH AREA SIZE (ISIZE),SIGN OF IMAGE FEATURES (ISIGN),
C MAX SHIFT FROM INITIAL POSITION (SHFTMX), MAX NO. OF CENTROIDING
C ITERATIONS (MAXIT) AND LOCATION ACCURACY (TOLL) FROM ENVIRONMENT
C
	  ISIZE=15
	  CALL GETPAR('ISIZE','INTEGER',1,3.0,51.0,.TRUE.,ISIZE,RVAL,
     +	  IERR)
	  ISIGN=4
	  CALL GETCMD('ISIGN','NEGATIVE,-,POSITIVE,+.',1,ISIGN,IDSIGN,
     +    LSGN,IERR)
	  ISIGN=ISIGN-3
	  SHFTMX=ISIZE*1.5
	  CALL GETPAR('MAXSHIFT','REAL',1,0.0,1.0E20,.TRUE.,IVAL,SHFTMX,
     +    IERR)
	  MAXIT=3
	  CALL GETPAR('MAXITER','INTEGER',1,1.0,100.0,.TRUE.,MAXIT,RVAL,
     +    IERR)
	  TOLL=0.05
	  CALL GETPAR('TOLERENC','REAL',1,0.0,1.0E20,.TRUE.,IVAL,TOLL,
     +    IERR)
C
C IF IMAGE SCALE FACTOR IS NEGATIVE, THE IMAGE FEATURES WILL BE
C STORED UPSIDE DOWN IN THE INPUT ARRAY, SO INVERT THE SIGN OF
C THE IMAGE FEATURES REQUIRED
C
	  SCALE=1.0
	  CALL GTDSCR('IMAGE','BSCALE','REAL',IVAL,SCALE,CVAL,IERR)
	  IF(SCALE.LT.0.0) ISIGN=-(1+ISIGN)
C
C CALL LOCLST TO FIND THE IMAGE CENTROIDS
C
	  CALL LOCLST(%VAL(IPXA),%VAL(IPYA),%VAL(IPIDA),LSTLEN,
     +    %VAL(IPIMG),NPIX,NLINES,INVAL,ISIZE,ISIGN,SHFTMX,MAXIT,TOLL,
     +    ILEVEL,%VAL(IPXB),%VAL(IPYB),%VAL(IPIDB),NOUT,IERR)
C
C IF THERE ARE NO OUTPUT LOCATIONS, GIVE MESSAGE AND ABORT
C
          IF(NOUT.LE.0) THEN
	    CALL WRERR('NOLIST')
	    GO TO 99
	  ENDIF
C
C OBTAIN OUTPUT DATA FRAME
C
	  CALL GTXYLW('OUTPUT',.FALSE.,7,NOUT,IPOUT,IERRB)
	  IF(IERRB.EQ.0) THEN
C
C OUTPUT FRAME OBTAINED SUCCESSFULLY... COPY OUTPUT DATA INTO LIST
C
	    CALL ADDLST(%VAL(IPOUT),7,NOUT,%VAL(IPIDB),1,20)
	    CALL ADDLST(%VAL(IPOUT),7,NOUT,%VAL(IPXB),21,24)
	    CALL ADDLST(%VAL(IPOUT),7,NOUT,%VAL(IPYB),25,28)
C
C ADD LIST DIMENSIONS AND TITLE TO OUTPUT DESCRIPTOR
C
	    CALL CYDSCR('INPUT','OUTPUT',ISTAT)
	    CALL PTDSCR('OUTPUT','NITEM','INTEGER',7,RVAL,CVAL,IERR)
	    CALL PTDSCR('OUTPUT','LSTLEN','INTEGER',NOUT,RVAL,CVAL,IERR)
	    CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,ISTAT)
	    CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE(1),
     +      IERR)
	  ENDIF
	ENDIF
      ENDIF
C
C FREE DATA AREAS AND RETURN
C
   99 CALL FRDATA(' ',ISTAT)
      RETURN
      END
