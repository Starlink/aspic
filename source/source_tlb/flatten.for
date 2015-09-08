*
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*
*
*
*          *******************
*          *                 *
*          * PROGRAM FLATTEN *
*          *                 *
*          *******************
*
*      CALLING SEQUENCE
*            FLATTEN
*
*
*PURPOSE
*     TO DIVIDE AN IMAGE BY A FLAT FIELD WITH AN ARBITRARY ORIENTATION
*     THIS IS FOR EDRS I*2 FORMAT IMAGES AND TAKES THE TRANSFORMATION
*     FACTORS IN THE EDRS FORMAT.
*
*METHOD
*	OBTAIN THE INPUT IMAGE AND TRANSFORM COEFFICIENTS. OBTAIN THE
*	OUTPUT IMAGE. CALL IMDIV TO DIVIDE BY THE FLAT FIELD.
*	UPDATE OUTPUT DESCRIPTOR ITEMS
*
*
*STARLINK PARAMETERS
*	IMAGE
*		THE INPUT IMAGE
*     FLAT
*              THE FLAT FIELD
*	TRCOEFFS
*		6 REAL COEFFICIENTS DEFINING THE TRANSFORMATION
*             FROM AN INPUT IMAGE PSITION TO THE MATCHING
*             POSITION IN THE FLAT IMAGE. THE DEFAULT ASSUMES
*             NO TRANSFORMATION NEEDED.
*	OUTPUT
*		OUTPUT IMAGE
*	TITLE
*		TITLE TO REPLACE THE INPUT TITLE IN THE OUTPUT IMAGE
*
*
*WRITTEN BY
*     A.J. PENNY         RGO                     82-NOV
*-----------------------------------------------------------------------
C
C
C
C   CALLS
C	EDRS PACKAGE:
C		GT2DIR,GETCMD,GETPAR,GT2DIW,GTDSCR,PTDSCR
C	STARLINK:
C		RDKEYR,RDKEYL,CYDSCR,RDKEYC,FRDATA
C
C    NOTES
C	USES VAX %VAL FACILITY
C
C       A J PENNY             RGO                           82-NOV
C ------------------------------------------------------------------
C
C
C
      PROGRAM FLATTEN
C
C
C
      CHARACTER CVAL*1,TITLE(1)*30
      REAL C(6)
C
C OBTAIN INPUT AND FLAT IMAGES
C
      CALL GT2DIR('IMAGE',102,.FALSE.,NPIXA,NLINEA,IPINA,IERRA)
      CALL GT2DIR('FLAT',102,.FALSE.,NPIXB,NLINEB,IPINB,IERRB)
      IF(IERRA.EQ.0.AND.IERRB.EQ.0) THEN
C
C IMAGES SUCESSFULLY OBTAINED...
C OBTAIN POSITION TRANSFORMATION COEFFICIENTS
C
         C(1)=0.0
         C(2)=1.0
         C(3)=0.0
         C(4)=0.0
         C(5)=0.0
         C(6)=1.0
         CALL RDKEYR('TRCOEFFS',.FALSE.,6,C,NVAL,ISTAT)
C
C
C OBTAIN OUTPUT IMAGE
C
         CALL GT2DIW('OUTPUT',102,.FALSE.,NPIXA,NLINEA,IPOUT,IERRC)
         IF(IERRC.EQ.0) THEN
C
C SUCCESSFULLY OBTAINED...
C EXTRACT TITLE, INVALID FLAG, SCALE AND ZERO FROM INPUT DESCRIPTOR
C
        TITLE(1)='Output from FLATTEN'
	  INVALA=-100000
	  SCALEA=1.0
	  ZEROA=0.0
          CALL GTDSCR('IMAGE','TITLE','CHARACTER',IVAL,RVAL,TITLE(1),
     +    IERR)
	  CALL GTDSCR('IMAGE','INVAL','INTEGER',INVALA,RVAL,CVAL,IERR)
	  CALL GTDSCR('IMAGE','BSCALE','REAL',IVAL,SCALEA,CVAL,IERR)
	  CALL GTDSCR('IMAGE','BZERO','REAL',IVAL,ZEROA,CVAL,IERR)
C
C EXTRACT INVALID FLAG, SCALE AND ZERO FROM FLAT DESCRIPTOR
C
	  INVALB=-100000
	  SCALEB=1.0
	  ZEROB=0.0
	  CALL GTDSCR('FLAT','INVAL','INTEGER',INVALB,RVAL,CVAL,IERR)
	  CALL GTDSCR('FLAT','BSCALE','REAL',IVAL,SCALEB,CVAL,IERR)
	  CALL GTDSCR('FLAT','BZERO','REAL',IVAL,ZEROB,CVAL,IERR)
C
C SET OUTPUT INVALID PIXEL FLAG
C
	  IF(ABS(INVALA).LE.32767) THEN
	    INVALC=INVALA
	  ELSE
	    INVALC=-32767
	  ENDIF
C
C DIVIDE INPUT IMAGE BY FLAT FIELD
C
         CALL IMDIV(%VAL(IPINA),NPIXA,NLINEA,INVALA,%VAL(IPINB),NPIXB,
     +             NLINEB,INVALB,ZEROB,SCALEB,C,%VAL(IPOUT),INVALC)
C
C COPY DESCRIPTOR FROM INPUT TO OUTPUT AND UPDATE IMAGE SIZE
C
	  CALL CYDSCR('IMAGE','OUTPUT',ISTAT)
	  CALL PTDSCR('OUTPUT','NAXIS1','INTEGER',NPIXA,RVAL,CVAL,IERR)
	  CALL PTDSCR('OUTPUT','NAXIS2','INTEGER',NLINEA,RVAL,CVAL,IERR)
	  CALL PTDSCR('OUTPUT','INVAL','INTEGER',INVALC,RVAL,CVAL,IERR)
	  CALL PTDSCR('OUTPUT','BSCALE','REAL',IVAL,SCALEA,CVAL,IERR)
	  CALL PTDSCR('OUTPUT','BZERO','REAL',IVAL,ZEROA,CVAL,IERR)
C
C ADD TITLE
C
          CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,ISTAT)
	  CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE(1),
     +    IERR)
	ENDIF
      ENDIF
C
C RELEASE DATA AREAS AND EXIT
C
      CALL FRDATA(' ',ISTAT)
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       CCCCCCCCCCCCC
C       C           C
C       C S/R IMDIV C
C       C           C
C       CCCCCCCCCCCCC
C
C
C
*PURPOSE
*     TO DIVIDE AN IMAGE BY A FLAT FIELD WITH A DIFFERENT ORIENTATION
*
*METHOD
*     SCAN THE INPUT IMAGE, DIVIDING BY THE FLAT IMAGE PIXEL
*     TAKEN FROM THE CORRESPONDING POINT IN THE FLAT IMAGE.
*     THE ROUTINE TAKES THE NEAREST FLAT POINT TO THE CALCULATED
*     FLAT POSITION.
*
*ARGUMENTS
*	IA (IN)
*	INTEGER*2(NPIXA,NLINEA)
*		THE INPUT IMAGE
*	NPIXA,NLINEA (IN)
*	INTEGER
*		THE DIMENSIONS OF IA
*	INVALA (IN)
*	INTEGER
*		INVALID PIXEL FLAG FOR IA
*     IB (IN)
*     INTEGER*2(NPIXB,NLINEB)
*             THE FLAT IMAGE
*     NPIXB,NLINEB (IN)
*     INTEGER
*             THE DIMENSIONS OF IB
*	INVALB (IN)
*	INTEGER
*		INVALID PIXEL FLAG FOR IB
*     ZEROB,SCALEB (IN)
*     REAL
*            SCALE FACTORS OF IB
*	C (IN)
*	REAL(6)
*		COEFFICIENTS GIVING THE TRANSFORMATION FROM INPUT IMAGE
*		POSITIONS TO FLAT POSITIONS
*     IC (IN/OUT)
*	INTEGER*2(NPIXA,NLINEA)
*		OUTPUT IMAGE
*     INVALC (IN)
*     INTEGER
*             INVALID PIXEL FLAG FOR IC
*
*CALLS
*	NONE
*
*NOTES
*	USES INTEGER*2 ARRAYS
*
*WRITTEN BY
*     A.J. PENNY
* ----------------------------------------------------------------------
C
C
C
      SUBROUTINE IMDIV(IA,NPIXA,NLINEA,INVALA,IB,NPIXB,NLINEB,INVALB,
     +                ZEROB,SCALEB,C,IC,INVALC)
C
C
      INTEGER*2 IA(NPIXA,NLINEA),IB(NPIXB,NLINEB),IC(NPIXA,NLINEA)
      INTEGER X,Y,XCEN,YCEN
      REAL C(6)
C
C DIVIDE THE INPUT IMAGE BY THE TRANSLATED FLAT IMAGE
C
C
	DO 12 Y=1,NLINEA
	   XREF=C(1)+C(3)*Y
	   YREF=C(4)+C(6)*Y
	   DO 11 X = 1,NPIXA
	      XDASH=XREF+C(2)*X
	      YDASH=YREF+C(5)*X
C
C FIND NEAREST PIXEL LOCATION
C
	      XCEN=NINT(XDASH)
	      YCEN=NINT(YDASH)
C
C IF NEAREST PIXEL LIES OUTSIDE INPUT IMAGE, OUTPUT PIXEL IS INVALID
C OTHERWISE CONTINUE
C
	      IF((XCEN.LT.1).OR.(XCEN.GT.NPIXB).OR.(YCEN.LT.1).OR.
     +	    (YCEN.GT.NLINEB)) THEN
	         IC(X,Y)=INVALC
	      ELSE
               IF(IA(X,Y).EQ.INVALA.OR.IB(XCEN,YCEN).EQ.INVALB) THEN
                  IC(X,Y) = INVALC
               ELSE
                  DIV = ZEROB+SCALEB*REAL(IB(XCEN,YCEN))
                  IF (DIV.GT.0.01.AND.DIV.LT.100.0) THEN
                     RESULT = REAL(IA(X,Y))/DIV
                     IF (RESULT.GT.-32767.0.AND.RESULT.LT.32767.0) THEN
                        IC(X,Y) = RESULT
                     ELSE
                        IC(X,Y) = INVALC
                     ENDIF
                  ELSE
                     IC(X,Y) = INVALC
                  ENDIF
               ENDIF
            ENDIF
   11    CONTINUE
   12 CONTINUE
C
C
C
      END
C
C
C