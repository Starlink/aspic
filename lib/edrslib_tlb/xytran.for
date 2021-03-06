      SUBROUTINE XYTRAN
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO APPLY A LINEAR TRANSFORMATION TO AN X,Y LIST DATASET
*
*METHOD
*	OBTAIN INPUT DATASET. OBTAIN TRANSFORMATION COEFFICIENTS.
*	CALL XYLTRN TO APPLY THE TRANSFORMATION. UPDATE OUTPUT 
*	DESCRIPTOR ITEMS.
*
*ARGUMENTS
*	NONE
*
*STARLINK PARAMETERS
*	INPUT
*		THE INPUT DATASET
*	OUTPUT
*		THE OUTPUT DATASET
*	TRCOEFFS
*		SIX REAL COEFFICIENTS DEFINING THE TRANSFORMATION
*	TITLE
*		A TITLE TO REPLACE THE INPUT TITLE IN THE OUTPUT DATASET
*
*CALLS
*	THIS PACKAGE:
*		GTXYLR,GTDSCR,GTXYLW,XYLTRN,PTDSCR
*	STARLINK:
*		RDKEYR,CYDSCR,RDKEYC,FRDATA
*
*NOTES
*	USES VAX %VAL FACILITY
*
*WRITTEN BY
*	R.F. WARREN-SMITH
*-----------------------------------------------------------------------
C
C
      CHARACTER TITLE(1)*30
      REAL C(6)
C
C OBTAIN INPUT LIST
C
      CALL GTXYLR('INPUT',.FALSE.,NITEM,LSTLEN,IPIN,IERRIN)
      IF(IERRIN.EQ.0) THEN
C
C INPUT LIST SUCCESSFULLY OBTAINED... EXTRACT TITLE FROM DESCRIPTOR
C
	TITLE(1)=' '
	CALL GTDSCR('INPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE(1),IERR)
C
C OBTAIN OUTPUT LIST DATA FRAME
C
	CALL GTXYLW('OUTPUT',.FALSE.,NITEM,LSTLEN,IPOUT,IERROU)
	IF(IERROU.EQ.0) THEN
C
C OUTPUT FRAME SUCCESSFULLY OBTAINED... GET VALUES OF THE TRANSFORM
C COEFFICIENTS
C
	  C(1)=0.0
	  C(2)=1.0
	  C(3)=0.0
	  C(4)=0.0
	  C(5)=0.0
          C(6)=1.0
	  CALL RDKEYR('TRCOEFFS',.FALSE.,6,C,NVAL,ISTAT)
C
C CALL XYLTRN TO APPLY THE TRANSFORMATION TO THE INPUT LIST, PUTTING
C THE RESULT IN THE OUTPUT LIST
C
	  CALL XYLTRN(%VAL(IPIN),NITEM,LSTLEN,C,%VAL(IPOUT))
C
C COPY INPUT DESCRIPTOR TO OUTPUT AND UPDATE TITLE
C
	  CALL CYDSCR('INPUT','OUTPUT',ISTAT)
	  CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,ISTAT)
	  CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE(1),
     +    IERR)
	ENDIF
      ENDIF
C
C FREE DATA AREAS AND RETURN
C
      CALL FRDATA(' ',ISTAT)
      RETURN
      END
