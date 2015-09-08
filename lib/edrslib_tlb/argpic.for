      SUBROUTINE ARGPIC
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO RESCALE AN IMAGE TO MAKE IT SUITABLE FOR DISPLAY
*	AND TO PLOT IT ON THE ARGS
*
*METHOD
*	OBTAIN INPUT IMAGE AND EXTRACT DESCRIPTOR ITEMS. OBTAIN 
*	PERCENTAGE HISTOGRAM POINTS FOR AUTOSCALING AND CALL PCHIST
*	TO DETERMINE THE DATA RANGE TO BE DISPLAYED. OBTAIN DATA RANGE
*	FROM ENVIRONMENT AND RESCALE THE IMAGE TO LIE IN THE RANGE 0-255
*	USING DSPSCL. PLOT THE IMAGE ON THE ARGS AND UPDATE THE ARGS
*	DATABASE. IF REQUIRED COPY THE RESCALED IMAGE TO AN OUTPUT
*	IMAGE FRAME.
*
*ARGUMENTS
*	NONE
*
*STARLINK PARAMETERS
*	INPUT
*		THE INPUT IMAGE
*	AUTO
*		LOGICAL PARAMETER SPECIFYING IF AUTOSCALING IS TO BE
*		PERFORMED
*	PCRANGE
*		PERCENTAGE HISTOGRAM POINTS CORRESPONDING TO THE
*		REQUIRED INPUT DATA RANGE TO BE USED FOR AUTOSCALING
*	DRANGE
*		THE INPUT DATA RANGE TO BE DISPLAYED
*	XC
*		THE X COORDINATE OF THE ARGS SCREEN ON WHICH THE IMAGE
*		IS TO BE CENTRED
*	YC
*		THE Y COORDINATE OF THE ARGS SCREEN ON WHICH THE IMAGE
*		IS TO BE CENTRED
*	NONEVAL/ERROR
*		ACCESSED IF THE INPUT IMAGE HAS NO VALID PIXELS
*	NOSPACE/ERROR
*		ACCESSED IF WORKSPACE CANNOT BE OBTAINED
*	NOARGS/ERROR
*		ACCESSED IF THE ARGS IS NOT AVAILABLE
*	NODB/ERROR
*		ACCESSED IF THE ARGS DATABASE CANNOT BE UPDATED
*	OUTPUT
*		THE OUTPUT IMAGE
*	TITLE
*		A TITLE TO REPLACE THE INPUT TITLE IN THE OUTPUT IMAGE
*	BADVALUE/ERROR
*		ACCESSED IF INVALID PERCENTAGE POINTS ARE GIVEN
*
*CALLS
*	THIS PACKAGE:
*		GT2DIR,GTDSCR,GETPAR,GT2DIW,DSPSCL,PTDSCR,RNGERR,
*		PCHIST,IMGCPY
*	STARLINK:
*		CYDSCR,RDKEYC,FRDATA,RDKEYR,RDKEYI,WRERR,CNPAR,
*		GETDYN,RDKEYL
*	ARGS LIBRARY:
*		SRINIT,SRPXI2
*	ARGS DATABASE:
*		ARGS_WRIM
*
*NOTES
*	USES VAX %VAL FACILITY
*	USES ARGS DATABASE
*
*WRITTEN BY
*	R.F. WARREN-SMITH
*-----------------------------------------------------------------------
C
C
      LOGICAL AUTO(1)
      CHARACTER CVAL*1,TITLE(1)*30
      REAL PCRANGE(2),DRANGE(2)
C
C DIMENSION ARRAY FOR HISTOGRAM FOR USE IN AUTOSCALING THE IMAGE
C
      PARAMETER (MININT=-32768,MAXINT=32767)
      INTEGER IHIST(MININT:MAXINT)
      INTEGER ILIM(2)
      INTEGER*2 IDUMMY(1)
C
C OBTAIN INPUT IMAGE
C
      CALL GT2DIR('INPUT',102,.FALSE.,NPIX,NLINES,IPIN,IERRIN)
      IF(IERRIN.EQ.0) THEN
C
C INPUT IMAGE OBTAINED SUCCESSFULLY... EXTRACT REQUIRED DESCRIPTOR
C ITEMS
C
        TITLE(1)=' '
	INVAL=-100000
	SCALE=1.0
	ZERO=0.0
	CALL GTDSCR('INPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE(1),IERR)
	CALL GTDSCR('INPUT','INVAL','INTEGER',INVAL,RVAL,CVAL,IERR)
	CALL GTDSCR('INPUT','BSCALE','REAL',IVAL,SCALE,CVAL,IERR)
	CALL GTDSCR('INPUT','BZERO','REAL',IVAL,ZERO,CVAL,IERR)
C
C DETERMINE IF AUTOSCALING IS REQUIRED
C
        AUTO(1)=.TRUE.
	CALL RDKEYL('AUTO',.TRUE.,1,AUTO,NVAL,ISTAT)
C
C SET DEFAULT INPUT DATA RANGE
C
        DRANGE(1)=SCALE*(-32768.0)+ZERO
	DRANGE(2)=SCALE*32767.0+ZERO
C
C IF AUTOSCALING IS REQUIRED, OBTAIN THE UPPER AND LOWER HISTOGRAM
C POINTS
C
	IF(AUTO(1)) THEN
	  PCRANGE(1)=5.0
	  PCRANGE(2)=95.0
   67	  CALL RDKEYR('PCRANGE',.TRUE.,2,PCRANGE,NVAL,ISTAT)
C
C IF THE VALUES LIE OUTSIDE 0 TO 100 PERCENT, CANCEL THEM,
C GIVE ERROR MESSAGE AND GET NEW VALUES
C
	  IF(MAX(PCRANGE(1),PCRANGE(2)).GT.100.0.OR.
     +	  MIN(PCRANGE(1),PCRANGE(2)).LT.0.0) THEN
	    CALL WRERR('BADVALUE')
	    CALL RNGERR('***REAL VALUES','REAL',0.0,100.0)
	    CALL CNPAR('PCRANGE',ISTAT)
	    GO TO 67
	  ENDIF
	  PCRANGE(1)=PCRANGE(1)*0.01
	  PCRANGE(2)=PCRANGE(2)*0.01
C
C CALL PCHIST TO FIND THE CORRESPONDING INTEGER VALUES
C
	  ILIM(1)=-32768
	  ILIM(2)=32767
	  CALL PCHIST(%VAL(IPIN),NPIX,NLINES,INVAL,PCRANGE,ILIM,2,IHIST,
     +	  MININT,MAXINT,IERR)
	  IF(IERR.NE.0) CALL WRERR('NONEVAL')
C
C CONVERT TO DATA VALUES
C
	  DRANGE(1)=ILIM(1)*SCALE+ZERO
	  DRANGE(2)=ILIM(2)*SCALE+ZERO
	ENDIF
C
C OBTAIN MINIMUM AND MAXIMUM DATA VALUES FOR INPUT IMAGE
C
	CALL RDKEYR('DRANGE',.TRUE.,2,DRANGE,NVAL,ISTAT)
C
C OBTAIN CENTRE COORDINATES ON ARGS SCREEN
C
	IXC=256
	IYC=256
	CALL GETPAR('XC','INTEGER',1,0.0,511.0,.TRUE.,IXC,RVAL,IERR)
	CALL GETPAR('YC','INTEGER',1,0.0,511.0,.TRUE.,IYC,RVAL,IERR)
C
C OBTAIN WORKSPACE FOR ARGS IMAGE
C
	CALL GETDYN('WRK',102,NPIX*NLINES,IPWRK,ISTWRK)
C
C IF SPACE WAS NOT AVAILABLE, ABORT WITH ERROR MESSAGE
C
	IF(ISTWRK.NE.0) THEN
	  CALL WRERR('NOSPACE')
	  GO TO 99
	ENDIF
C
C CALL DSPSCL TO RESCALE THE IMAGE INTO THE RANGE 0-255
C
        CALL DSPSCL(%VAL(IPIN),NPIX,NLINES,INVAL,SCALE,ZERO,DRANGE(1),
     +	DRANGE(2),0,255,%VAL(IPWRK))
C
C INITIALLISE ARGS, QUITTING WITH AN ERROR MESSAGE IF NOT AVAILABLE
C
        CALL SRINIT(0,.FALSE.,ISTAT)
	IF(ISTAT.NE.0) THEN
	  CALL WRERR('NOARGS')
	  GO TO 99
	ENDIF
C
C DETERMINE HOW MUCH IMAGE WILL FIT ON SCREEN
C
	NX=MIN(NPIX,512)
	NY=MIN(NLINES,512)
C
C PLOT IMAGE ON ARGS SCREEN
C
        CALL SRPXI2(%VAL(IPWRK),NPIX,NX,NY,IXC-NX/2,IYC-NY/2,16,.FALSE.,
     +  IDUMMY,1)
C
C UPDATE ARGS DATABASE
C
        CALL ARGS_WRIM(IXC,IYC,NX,NY,NX,NY,ISTAT)
        IF(ISTAT.NE.0) CALL WRERR('NODB')
C
C OBTAIN OUTPUT IMAGE FRAME
C
        CALL GT2DIW('OUTPUT',102,.TRUE.,NPIX,NLINES,IPOUT,IERROU)
C
C IF AN OUTPUT FRAME WAS GIVEN, ADD DESCRIPTOR ITEMS
C
        IF(IERROU.EQ.0) THEN
	  CALL CYDSCR('INPUT','OUTPUT',ISTAT)
	  CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,ISTAT)
	  CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE(1),
     +	  IERR)
	  CALL PTDSCR('OUTPUT','INVAL','INTEGER',-100000,RVAL,CVAL,IERR)
          CALL PTDSCR('OUTPUT','BSCALE','REAL',IVAL,1.0,CVAL,IERR)
          CALL PTDSCR('OUTPUT','BZERO','REAL',IVAL,0.0,CVAL,IERR)
C
C COPY ARGS IMAGE TO OUTPUT
C
	  CALL IMGCPY(%VAL(IPWRK),NPIX,NLINES,%VAL(IPOUT))
	ENDIF
      ENDIF
   99 CALL FRDATA(' ')
      END