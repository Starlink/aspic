      SUBROUTINE RESCAL
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO ALLOW THE USER TO CHANGE DESCRIPTOR ITEMS IN AN IMAGE SO
*	AS TO RE-SCALE THE INTEGER VALUES STORED
*
*METHOD
*	OBTAIN INPUT/OUTPUT IMAGES AND OBTAIN DESCRIPTOR ITEMS. FIND NEW 
*	VALUES FROM THE ENVIRONMENT. CALL NEWSCL TO ALTER THE IMAGE 
*	INTEGERS THEN UPDATE THE OUTPUT DESCRIPTOR
*
*ARGUMENTS
*	NONE
*
*STARLINK PARAMETERS
*	INPUT
*		THE INPUT IMAGE
*	OUTPUT
*		THE OUTPUT IMAGE
*	TITLE
*		THE NEW TITLE
*	INVAL
*		THE NEW INVALID PIXEL FLAG
*	BSCALE
*		THE NEW IMAGE SCALE FACTOR
*	BZERO
*		THE NEW IMAGE ZERO LEVEL
*
*CALLS
*	THIS PACKAGE:
*		GT2DIR,GT2DIW,GTDSCR,GETPAR,NEWSCL,PTDSCR
*	STARLINK:
*		RDKEYC,CYDSCR,FRDATA
*
*NOTES
*	USES VAX %VAL FACILITY
*
*WRITTEN BY
*	R.F. WARREN-SMITH
*-----------------------------------------------------------------------
C
C
      CHARACTER CVAL*1,TITLE(1)*30
C
C OBTAIN INPUT IMAGE
C
      CALL GT2DIR('INPUT',102,.FALSE.,NPIX,NLINES,IPIN,IERRIN)
      IF(IERRIN.EQ.0) THEN
C
C INPUT IMAGE OBTAINED SUCCESSFULLY...OBTAIN OUTPUT IMAGE
C
	CALL GT2DIW('OUTPUT',102,.FALSE.,NPIX,NLINES,IPOUT,IERROU)
	IF(IERROU.EQ.0) THEN
C
C OUTPUT IMAGE OBTAINED SUCCESSFULLY...EXTRACT DESCRIPTOR ITEMS FROM
C INPUT IMAGE
C
          TITLE(1)=' '
	  INVALA=-100000
	  ASCALE=1.0
	  AZERO=0.0
	  CALL GTDSCR('INPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE(1),
     +	  IERR)
	  CALL GTDSCR('INPUT','INVAL','INTEGER',INVALA,RVAL,CVAL,IERR)
	  CALL GTDSCR('INPUT','BSCALE','REAL',IVAL,ASCALE,CVAL,IERR)
	  CALL GTDSCR('INPUT','BZERO','REAL',IVAL,AZERO,CVAL,IERR)
C
C SET DEFAULT DESCRIPTOR ITEMS FOR OUTPUT
C
	  BSCALE=ASCALE
	  BZERO=AZERO
	  IF(ABS(INVALA).LE.32767) THEN
	    INVALB=INVALA
	  ELSE
	    INVALB=-32767
	  ENDIF
C
C OBTAIN NEW DESCRIPTOR ITEMS FROM ENVIRONMENT
C
	  CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,ISTAT)
	  CALL GETPAR('INVAL','INTEGER',1,-32767.0,+32767.0,.TRUE.,
     +	  INVALB,RVAL,CVAL,IERR)
	  CALL GETPAR('BSCALE','REAL',1,-1.0E20,+1.0E20,.TRUE.,
     +	  IVAL,BSCALE,CVAL,IERR)
	  CALL GETPAR('BZERO','REAL',1,-1.0E20,+1.0E20,.TRUE.,
     +	  IVAL,BZERO,CVAL,IERR)
C
C CALL NEWSCL TO RESCALE THE IMAGE TO SUIT THE NEW DESCRIPTOR ITEMS
C
	  CALL NEWSCL(%VAL(IPIN),NPIX,NLINES,INVALA,ASCALE,AZERO,
     +	  INVALB,BSCALE,BZERO,%VAL(IPOUT))
C
C COPY THE INPUT DESCRIPTOR TO THE OUTPUT AND UPDATE THOSE ITEMS WHICH
C HAVE CHANGED
C
	  CALL CYDSCR('INPUT','OUTPUT',ISTAT)
	  CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE(1),
     +	  IERR)
	  CALL PTDSCR('OUTPUT','INVAL','INTEGER',INVALB,RVAL,CVAL,IERR)
	  CALL PTDSCR('OUTPUT','BSCALE','REAL',IVAL,BSCALE,CVAL,IERR)
	  CALL PTDSCR('OUTPUT','BZERO','REAL',IVAL,BZERO,CVAL,IERR)
	ENDIF
      ENDIF
C
C RELEASE DATA AREAS AND RETURN
C
   99 CALL FRDATA(' ',ISTAT)
      END