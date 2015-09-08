      SUBROUTINE SEGMEN
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO COPY POLYGONAL SEGMENTS FROM ONE IMAGE TO ANOTHER
*
*METHOD
*	OBTAIN INPUT IMAGES. TREAT THE CASE OF 'NULL' INPUT IMAGES 
*	SEPARATELY. EXTRACT INPUT DESCRIPTOR ITEMS AND OBTAIN DUMMY
*	INPUT IMAGE IF NECESSARY. CALCULATE OUTPUT DESCRIPTOR ITEMS
*	ACCORDING TO INPUT IMAGES PRESENT. PROCESS EACH POLYGON IN TURN.
*	OBTAIN INPUT X,Y LIST. OBTAIN WORKSPACE AND EXTRACT X,Y
*	POSITIONS. CALL PLYSMP TO COPY THE POLYGONAL AREA.
*	FINALLY UPDATE OUTPUT DESCRIPTOR ITEMS
*
*ARGUMENTS
*	NONE
*
*STARLINK PARAMETERS
*	AIMAGE
*		THE IMAGE TO BE COPIED FROM
*	BIMAGE
*		THE IMAGE TO BE COPIED INTO
*	NOSPACE/ERROR/
*		ACCESSED IF WORKSPACE CANNOT BE OBTAINED
*	OUTPUT
*		THE OUTPUT IMAGE
*	POLY1,POLY2...POLY20
*		THE INPUT X,Y LISTS SPECIFYING THE POLYGONS
*	2COMPLEX/ERROR/
*		ACCESSED IF A POLYGON HAS TOO MANY SIDES TO HANDLE
*	2FEWSIDE/ERROR/
*		ACCESSED IF A POLYGON DOES NOT HAVE AT LEAST 3 SIDES
*	TITLE
*		A TITLE TO REPLACE THE INPUT TITLE IN THE OUTPUT IMAGE
*
*CALLS
*	THIS PACKAGE:
*		GT2DIR,GT2DIW,IMGSET,GTDSCR,IMGCPY,LBGONE,GTXYLR,EXTLST,
*		PLYSMP,PTDSCR
*	STARLINK:
*		GETDYN,WRERR,FRDATA,CNPAR,RDKEYC,CYDSCR
*
*NOTES
*	USES VAX %VAL FACILITY
*
*WRITTEN BY
*	R.F. WARREN-SMITH
*-----------------------------------------------------------------------
C
C
      CHARACTER CVAL*1,TITLE(1)*30,POLCHR*8
      INTEGER LIMIT(4)
C
C SET MAX NUMBER OF POLYGON INPUT DATA FRAMES TO BE USED
C
      PARAMETER (MAXPOL=20)
C
C OBTAIN INPUT IMAGE 'A'
C
      CALL GT2DIR('AIMAGE',102,.TRUE.,NPIXA,NLINEA,IPA,IERRA)
C
C OBTAIN INPUT IMAGE 'B', ACCEPTING A NULL ENTRY ONLY IF 'A' WAS GIVEN
C
      CALL GT2DIR('BIMAGE',102,(IERRA.EQ.0),NPIXB,NLINEB,IPB,IERRB)
C
C IF AT LEAST 1 INPUT IMAGE WAS GIVEN, CONTINUE
C
      IF((IERRA.EQ.0).OR.(IERRB.EQ.0)) THEN
C
C IF 'A' WAS NOT GIVEN, OBTAIN A DUMMY REPLACEMENT 1 PIXEL IMAGE
C
	IF(IERRA.NE.0) THEN
	  CALL GETDYN('DUMMYA',102,1,IPA,ISTATA)
	  NPIXA=1
	  NLINEA=1
C
C IF SPACE WAS NOT AVAILABLE, GIVE MESSAGE AND ABORT
C
	  IF(ISTATA.NE.0) THEN
	    CALL WRERR('NOSPACE')
	    GO TO 99
	  ENDIF
        ENDIF
C
C IF 'B' WAS GIVEN, OUTPUT IMAGE IS SIZE OF 'B', OTHERWISE IT IS THE
C SIZE OF 'A'
C
	IF(IERRB.EQ.0) THEN
	  NPOUT=NPIXB
	  NLOUT=NLINEB
	ELSE
	  NPOUT=NPIXA
	  NLOUT=NLINEA
	ENDIF
C
C OBTAIN OUTPUT IMAGE FRAME
C
	CALL GT2DIW('OUTPUT',102,.FALSE.,NPOUT,NLOUT,IPOUT,IERROU)
	IF(IERROU.EQ.0) THEN
C
C OUTPUT FRAME OBTAINED SUCCESSFULLY... 
C
	  IF(IERRA.NE.0) THEN
C
C IF 'A' IS ABSENT, SET DEFAULT DESCRIPTOR ITEMS
C ----------------
C
	    INVALA=-32767
	    ASCALE=1.0
	    AZERO=0.0
C
C SET DUMMY 'A' IMAGE PIXEL TO BE INVALID
C
	    CALL IMGSET(%VAL(IPA),NPIXA,NLINEA,-32767)
C
C OBTAIN DESCRIPTOR ITEMS FROM 'B' IMAGE
C
	    TITLE(1)=' '
	    INVALB=-32767
	    BSCALE=1.0
	    BZERO=0.0
	    CALL GTDSCR('BIMAGE','TITLE','CHARACTER',IVAL,RVAL,TITLE(1),
     +      IERR)
	    CALL GTDSCR('BIMAGE','INVAL','INTEGER',INVALB,RVAL,CVAL,
     +	    IERR)
	    CALL GTDSCR('BIMAGE','BSCALE','REAL',IVAL,BSCALE,CVAL,IERR)
	    CALL GTDSCR('BIMAGE','BZERO','REAL',IVAL,BZERO,CVAL,IERR)
C
C COPY CONTENTS OF IMAGE 'B' TO OUTPUT IMAGE
C
	    CALL IMGCPY(%VAL(IPB),NPOUT,NLOUT,%VAL(IPOUT))
	    IF(ABS(INVALB).GT.32767) INVALB=-32767
C
C IF IMAGE 'B' WAS NOT GIVEN, OBTAIN DESCRIPTOR ITEMS FROM 'A'
C --------------------------
C
	  ELSE IF(IERRB.NE.0) THEN
	    TITLE(1)=' '
	    INVALA=-32767
	    ASCALE=1.0
	    AZERO=0.0
	    CALL GTDSCR('AIMAGE','TITLE','CHARACTER',IVAL,RVAL,TITLE(1),
     +      IERR)
	    CALL GTDSCR('AIMAGE','INVAL','INTEGER',INVALA,RVAL,CVAL,
     +	    IERR)
	    CALL GTDSCR('AIMAGE','BSCALE','REAL',IVAL,ASCALE,CVAL,IERR)
	    CALL GTDSCR('AIMAGE','BZERO','REAL',IVAL,AZERO,CVAL,IERR)
C
C OUTPUT DESCRIPTOR ITEMS DEFAULT TO THOSE OF IMAGE 'A'
C
	    INVALB=INVALA
	    IF(ABS(INVALB).GT.32767)INVALB=-32767
	    BSCALE=ASCALE
	    BZERO=AZERO
C
C SET THE OUTPUT IMAGE TO BE ALL INVALID
C
	    CALL IMGSET(%VAL(IPOUT),NPOUT,NLOUT,INVALB)
C
C IF BOTH 'A' AND 'B' IMAGES WERE GIVEN, OBTAIN DESCRIPTOR ITEMS
C -------------------------------------
C FOR IMAGE 'A'
C
	  ELSE
	    INVALA=-100000
	    ASCALE=1.0
	    AZERO=0.0
	    CALL GTDSCR('AIMAGE','INVAL','INTEGER',INVALA,RVAL,CVAL,
     +	    IERR)
	    CALL GTDSCR('AIMAGE','BSCALE','REAL',IVAL,ASCALE,CVAL,IERR)
	    CALL GTDSCR('AIMAGE','BZERO','REAL',IVAL,AZERO,CVAL,IERR)
C
C OBTAIN DESCRIPTOR ITEMS FOR IMAGE 'B', TO APPLY TO OUTPUT IMAGE ALSO
C
	    TITLE(1)=' '
	    INVALB=-32767
	    BSCALE=1.0
	    BZERO=0.0
	    CALL GTDSCR('BIMAGE','TITLE','CHARACTER',IVAL,RVAL,TITLE(1),
     +	    IERR)
	    CALL GTDSCR('BIMAGE','INVAL','INTEGER',INVALB,RVAL,CVAL,
     +	    IERR)
	    CALL GTDSCR('BIMAGE','BSCALE','REAL',IVAL,BSCALE,CVAL,IERR)
	    CALL GTDSCR('BIMAGE','BZERO','REAL',IVAL,BZERO,CVAL,IERR)
C
C COPY IMAGE 'B' TO OUTPUT IMAGE FRAME
C
	    CALL IMGCPY(%VAL(IPB),NPOUT,NLOUT,%VAL(IPOUT))
	    IF(ABS(INVALB).GT.32767) INVALB=-32767
	  ENDIF
C
C COUNT THROUGH THE NUMBER OF POLYGON INPUT DATA FRAMES AND FORM
C THE APPROPRIATE PARAMETER NAME
C
	  DO 616 NPOLY=1,MAXPOL
	    WRITE(POLCHR,1616)NPOLY
 1616	    FORMAT('POLY',I4)
	    CALL LBGONE(POLCHR(5:))
C
C OBTAIN INPUT FRAME, ACCEPTING A NULL RETURN ONLY IF NOT THE FIRST
C POLYGON
C
	    CALL GTXYLR(POLCHR,(NPOLY.GE.2),NITEM,LSTLEN,IPXY,IERRXY)
C
C IF FRAME NOT OBTAINED, EXIT FROM LOOP, OR ABORT IF THIS IS THE FIRST
C POLYGON
C
	    IF(IERRXY.NE.0) THEN
	      IF(NPOLY.GE.2) THEN
		GO TO 617
	      ELSE
	        GO TO 99
	      ENDIF
	    ENDIF
C
C OBTAIN DYNAMIC MEMORY SPACE FOR X,Y POSITIONS
C
	    CALL GETDYN('X',204,LSTLEN,IPX,ISTX)
	    CALL GETDYN('Y',204,LSTLEN,IPY,ISTY)
C
C IF SPACE WAS NOT AVAILABLE, GIVE MESSAGE AND ABORT
C
	    IF((ISTX.NE.0).OR.(ISTY.NE.0)) THEN
	      CALL WRERR('NOSPACE')
	      GO TO 99
	    ENDIF
C
C EXTRACT X,Y POSITIONS FROM INPUT LIST
C
	    CALL EXTLST(%VAL(IPXY),NITEM,LSTLEN,%VAL(IPX),21,24)
	    CALL EXTLST(%VAL(IPXY),NITEM,LSTLEN,%VAL(IPY),25,28)
C
C CALL PLYSMP TO COPY THE POLYGONAL SEGMENT FROM IMAGE A TO IMAGE B
C
	    CALL PLYSMP(%VAL(IPA),NPIXA,NLINEA,INVALA,ASCALE,AZERO,
     +	    %VAL(IPX),%VAL(IPY),LSTLEN,%VAL(IPOUT),NPOUT,NLOUT,INVALB,
     +	    BSCALE,BZERO,LIMIT,IERR)
C
C IF IERR INDICATES THE POLYGON WAS TOO COMPLEX, GIVE MESSAGE AND ABORT
C
	    IF(IERR.EQ.2) THEN
	      CALL WRERR('2COMPLEX')
	      GO TO 99
	    ELSE IF(IERR.EQ.1) THEN
	      CALL WRERR('2FEWSIDE')
	      GO TO 99
	    ENDIF
C
C FREE DYNAMIC MEMORY AND INPUT DATA FRAME BEFORE RETURNING FOR
C NEXT POLYGON
C
	    CALL FRDATA('X',ISTAT)
	    CALL FRDATA('Y',ISTAT)
	    CALL FRDATA(POLCHR,ISTAT)
	    CALL CNPAR(POLCHR,ISTAT)
  616	  CONTINUE
  617     CONTINUE
C
C OBTAIN TITLE FROM ENVIRONMENT
C
	  CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,ISTAT)
C
C COPY THE INPUT IMAGE DESCRIPTOR TO THE OUTPUT
C
	  IF(IERRB.EQ.0) THEN
	    CALL CYDSCR('BIMAGE','OUTPUT',ISTAT)
	  ELSE
	    CALL CYDSCR('AIMAGE','OUTPUT',ISTAT)
	  ENDIF
C
C UPDATE OUTPUT DESCRIPTOR ITEMS
C
	  CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE(1),
     +    IERR)
	  CALL PTDSCR('OUTPUT','INVAL','INTEGER',INVALB,RVAL,CVAL,IERR)
	  CALL PTDSCR('OUTPUT','BSCALE','REAL',IVAL,BSCALE,CVAL,IERR)
	  CALL PTDSCR('OUTPUT','BZERO','REAL',IVAL,BZERO,CVAL,IERR)
	ENDIF
      ENDIF
C
C FREE DATA AREAS AND RETURN
C
   99 CALL FRDATA(' ',ISTAT)
      RETURN
      END