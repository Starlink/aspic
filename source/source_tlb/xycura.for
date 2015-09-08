
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ******************** 
C                     *                  * 
C                     * Program   XYCURA * 
C                     *                  * 
C                     ******************** 
C
C
C
C          CALLING SEQUENCE:-   XYCURA  [ILEVEL= MARKER= COLOUR=
C                                        IDENTIFY= MAXENTRY= ]
C
C
C          FUNCTION:- 
C
C             Make a list of X,Y positions by using a cursor to define the
C             positions on an image displayed on the ARGS. The positions can
C             have identifiers given to them.
C
C             Also to display on such an image the X,Y positions of an
C             optional input list.
C             If this is done and the cursor is used to define more
C             positions, these are added to the input list instead of a
C             new list being made.
C             These positions can be displayed as crosses; as ticks
C             displaced alternately by (-2,0) and (2,0); as 1 pixel
C             spots.
C             Of the input list, only those positions inside the area of
C             the image displayed on the ARGS will be put up.
C
C
C          USE:- 
C             First an image must be on the ARGS (by ICDISP,ADISP,etc (qv)).
C               You can clear any existing crosses displayed on the image,
C             and then choose wether to have an input XYlist.
C             If you do, then you choose wether to display the input
C             positions as blue crosses and wether to display their numbers
C             in the list.
C               Then input the new positions by means of the cursor.
C             The positions are then marked with red crosses.
C             If you have chosen to add identifiers you enter them on the
C             keyboard after each use of the cursor. (The choice is made by
C             setting IDENTIFY=TRUE on runnung the program.)
C             To exit input a position at or beyond the bottom left hand
C             corner of the image.
C               The positions are then output to the XYlist.
C
C
C
C         USER PARAMETERS:- 
C
C         OVERCL          NO                  Flag for clearing the crosses
C                                             left  on  the  ARGS  from any
C                                             previous XYCURA  Choices  are
C                                             NO,YES 
C
C         INPUT                               The optional input XY list 
C                                             Return null if there is none.
C
C         FXOUT           YES                  Flag for putting  up  on  the
C                                             ARGS  the  crosses  from  any
C                                             input list. Choices are
C                                             YES,NO.
C
C         NUMINP          NO                  Flag for adding the number of
C                                             the cross of the input list. 
C
C         OUTPUT                              The  Output  file  containing
C                                             the positions 
C
C         TITLE   Output from XYCURA          The Title to be  included  in
C                                             the Output file. 
C
C         IDENTITY                            If inputting star names, this
C                                             is the name  of  the star.
C
C
C         NORMALLY DEFAULTED PARAMETERS:- 
C
C         ILEVEL          2                   Flag for  outputting  on  the
C                                             terminal  the  positions from
C                                             the cursor as you go along.The
C                                             default  (=2) makes it happen
C                                             automatically. Setting it  to
C                                             1   on  running  the  program
C                                             supresses it. 
C
C         MARKER          CROSS               The type of mark made at the
C                                             positions of the input XYlist.
C                                             Choices are crosses; ticks at
C                                             (-2,0) and (2,0) alternately;
C                                             spots.
C                                             Choices are CROSS,ATICK,SPOT
C
C         COLOUR          B                   The colour of the cross put
C                                             on the input list positions.
C                                             Choices are;- White,Red,Blue
C                                             Yellow,Cyan,Magenta,Green.
C                                             (use only 1st letter)
C
C         MAXENTRY        2000                The   maximum    number    of
C                                             positions  that  can be input
C                                             by the cursor.  Defaul  2000.
C                                             You   can  change  this  when
C                                             running the program. 
C
C         IDENTIFY        FALSE               Flag for choosing  wether  to
C                                             put  in via keyboard the name
C                                             of  the  star   at   position
C                                             chosen  via  the  cursor.  To
C                                             have    this    option    put
C                                             IDENTIFY=TRUE    wh   running
C                                             XYCURA.  If  not  chosen  the
C                                             stars    are    labelled   as
C                                             sequential numbe (#n). 
C
C       USE OF TRACKER-BALL BUTTONS
C
C       GREEN 1     Reverts centre to image centre and magnification to 1
C
C       WHITE 2     Decreases magnification by times 2
C
C       WHITE 3     Increases magnification by times 2
C
C       RED   4     Inputs present position of cursor as star position.
C                   If this is to bottom lh of image blh corner, the
C                   program finishes.
C
C
C
C         A J Penny                RGO                             83-JUL-21
C
C
C---------------------------------------------------------------------



C
*CALLS
*	THIS PACKAGE:
*		GETPAR,GTXYLR,EXTLST,XYINCA,GTXYLW,ADDLST,LBGONE,GTDSCR,
*		PTDSCR
*	STARLINK:
*		GETDYN,WRERR,WRUSER,RDKEYC,CYDSCR,FRDATA
*       ARGSLIB:
*               SRINIT
*	ASPIC:
*		ASP_PAN
*
*NOTES
*	USES VAX %VAL FACILITY
*
*WRITTEN BY
*	R.F. WARREN-SMITH
*ADAPTED BY
*     A.J. PENNY
* ----------------------------------------------------------------------
C
C
C
      PROGRAM XYCURA
C
C
C
      CHARACTER CVAL*1,TITLE*30,PRBUF*40
      CHARACTER TEXT*72,TCOL*1
      LOGICAL IDENT
C
C OBTAIN INTERACTION LEVEL
C
      ILEVEL=2
      CALL GETPAR('ILEVEL','INTEGER',1,1.0,3.0,.TRUE.,ILEVEL,RVAL,
     +            IERR)
C
C  Get input XYlist mark option
C
    1 KOPT = 1
      CALL GETCMD('MARKER','CROSS,ATICK,SPOT,HELP,?.',.TRUE.,KOPT,
     +            TEXT,KTEXT,ISTAT)
      IF (KOPT.EQ.4.OR.KOPT.EQ.5) THEN
         CALL WRUSER('Input XYlist mark types',ISTAT)
         CALL WRUSER('Choices are :-',ISTAT)
         CALL WRUSER('CROSS    Cross',ISTAT)
         CALL WRUSER('ATICK    Ticks at (-2:0) and (2:0)'//
     +               ' alternately',ISTAT)
         CALL WRUSER('SPOT     1 pixel spots',ISTAT)
         CALL CNPAR('MARKER',ISTAT)
         GO TO 1
      ENDIF
C
C  Get colour of input spots
C
    2 KCOL = 1
      CALL GETCMD('COLOUR','B,W,R,Y,C,M,G,HELP,?.',.TRUE.,KCOL,
     +            TEXT,KTEXT,ISTAT)
      IF (KCOL.EQ.8.OR.KCOL.EQ.9) THEN
         CALL WRUSER('Colour to paint input list markers',ISTAT)
         CALL WRUSER('Choices are :-',ISTAT)
         CALL WRUSER('White,Red,Blue,Yellow,Cyan,Magenta,Green',
     +               ISTAT)
         CALL WRUSER('(type only the first letter',ISTAT)
         CALL CNPAR('COLOUR',ISTAT)
         GO TO 2
      ENDIF
      IF (KCOL.EQ.1) TCOL = 'B'
      IF (KCOL.EQ.2) TCOL = 'W'
      IF (KCOL.EQ.3) TCOL = 'R'
      IF (KCOL.EQ.4) TCOL = 'Y'
      IF (KCOL.EQ.5) TCOL = 'C'
      IF (KCOL.EQ.6) TCOL = 'M'
      IF (KCOL.EQ.7) TCOL = 'G'
C
C  Get wether to clear all overlay planes before writing
C
    3 KOVCL = 2
      CALL GETCMD('OVERCL','YES,NO,HELP,?.',.TRUE.,KOVCL,
     +            TEXT,KTEXT,ISTAT)
      IF (KOVCL.EQ.3.OR.KOVCL.EQ.4) THEN
         CALL WRUSER('Choices are :-',ISTAT)
         CALL WRUSER('YES      Clear overlay planes',ISTAT)
         CALL WRUSER('NO       Leave planes as they are',ISTAT)
         CALL CNPAR('OVERCL',ISTAT)
         GO TO 3
      ENDIF
C
C OBTAIN AN OPTIONAL INPUT DATA FRAME
C
      CALL GTXYLR('INPUT',.TRUE.,NITEM,LSTLEN,IPIN,IERRI)
C
C IF INPUT NOT OBTAINED, SET DEFAULT VALUES FOR THE LIST DIMENSIONS
C NITEM=NO OF 4-BYTE ITEMS PER LIST RECORD
C LSTLEN=NO OF LIST RECORDS
C
      IF(IERRI.NE.0) THEN
         NITEM=7
         LSTLEN=0
      ENDIF
C
C SET DEFAULT FOR MAX LENGTH OF OUTPUT LIST, THEN OBTAIN VALUE FROM
C ENVIRONMENT
C
      LENOUT=LSTLEN+2000
      CALL GETPAR('MAXENTRY','INTEGER',1,REAL(LSTLEN+1),1.0E6,.TRUE.,
     +LENOUT,RVAL,IERR)
C
C OBTAIN VIRTUAL MEMORY WORKSPACE TO HOLD IDENTIFIERS AND X,Y
C VALUES
C
      CALL GETDYN('ID',104,5*LENOUT,IPID,ISTATI)
      CALL GETDYN('X',104,LENOUT,IPX,ISTATX)
      CALL GETDYN('Y',104,LENOUT,IPY,ISTATY)
C
C IF SPACE NOT AVAILABLE... GIVE ERROR MESSAGE AND ABORT
C
      IF((ISTATI.NE.0).OR.(ISTATX.NE.0).OR.(ISTATY.NE.0)) THEN
	CALL WRERR('NOSPACE')
        GO TO 99
      ENDIF
C
C IF INPUT WAS SUPPLIED, COPY INPUT LIST DATA TO WORKSPACE
C
      IF(IERRI.EQ.0) THEN
	CALL EXTLST(%VAL(IPIN),NITEM,LSTLEN,%VAL(IPID),1,20)
	CALL EXTLST(%VAL(IPIN),NITEM,LSTLEN,%VAL(IPX),21,24)
	CALL EXTLST(%VAL(IPIN),NITEM,LSTLEN,%VAL(IPY),25,28)
      ENDIF
C
C DETERMINE IF IDENTIFIERS ARE TO BE PROMPTED FOR
C
      IDENT=.FALSE.
      CALL RDKEYL('IDENTIFY',.TRUE.,1,IDENT,NVAL,ISTAT)
C
C DETERMINE IF INPUT LIST CROSSES TO BE PUT UP
C
      IF(IERRI.EQ.0) THEN
         CALL WRUSER('DISPLAY INPUT LIST POSITIONS ?',JSTAT)
    4    KFXOUT = 1
         CALL GETCMD('FXOUT','YES,NO,HELP,?.',.TRUE.,KFXOUT,
     +               TEXT,KTEXT,ISTAT)
         IF (KFXOUT.EQ.3.OR.KFXOUT.EQ.4) THEN
            CALL WRUSER('Choices are :-',ISTAT)
            CALL WRUSER('YES     Positions are displayed',ISTAT)
            CALL WRUSER('NO      Positions not displayed',ISTAT)
            CALL CNPAR('FXOUT',ISTAT)
            GO TO 4
         ENDIF
         IF (KFXOUT.EQ.1) THEN
            CALL WRUSER('NUMBER THEM ?',ISTAT)
            NUMINP = 2
            CALL GETCMD('NUMINP','NO,YES.',1,NUMINP,PRBUF,NVAL,
     +                  ISTAT)
         ENDIF
      ENDIF
C
C  Initialise the ARGS and get compression and window data
C
      ISTAT=0
      CALL SRINIT(0,.FALSE.,ISTAT)
      IF (ISTAT.NE.0) THEN
         CALL WRERR('NOARGS')
         GO TO 99
      ELSE
         CALL ARGS_NUMIM(ID)
         CALL ARGS_RDIM(IXPOS,IYPOS,ISX,ISY,I,I,ISTAT)
         CALL ARGS_RDPAR('COMPRE',1,TEXT,NVALS,ISTAT)
         IF (ISTAT.EQ.0) THEN
            READ(TEXT,900)KXB,KXE,KYB,KYE,KCOMP
  900       FORMAT(5I10)
            COMFAC = REAL(KCOMP)
            DX = REAL(KXB)
            DY = REAL(KYB)
         ELSE
            COMFAC = 1.0
            DX = 1.0
            DY = 1.0
         ENDIF
      ENDIF
C
C  Do the input list (if any) display and get the new positions
C
      LENBEG = LSTLEN
      CALL XYINCA(%VAL(IPID),%VAL(IPX),%VAL(IPY),LENOUT,LSTLEN,
     +IDENT,ILEVEL,IERR,KOVCL,KFXOUT,NUMINP,COMFAC,DX,DY,ISX,ISY,
     +            KOPT,TCOL)
C
C IF NO LIST OBTAINED, GIVE ERROR MESSAGE, OTHERWISE
C OBTAIN OUTPUT DATA FRAME
C
      IF(LSTLEN.LE.0.OR.LSTLEN.EQ.LENBEG) THEN
        CALL WRERR('NOLIST')
      ELSE
	CALL GTXYLW('OUTPUT',.FALSE.,7,LSTLEN,IPOUT,IERR2)
	IF(IERR2.EQ.0) THEN
C
C OUTPUT FRAME SUCCESSFULLY OBTAINED. COPY LISTS FROM WORKSPACE TO
C OUTPUT DATA FRAME
C
          CALL ADDLST(%VAL(IPOUT),7,LSTLEN,%VAL(IPID),1,20)
	  CALL ADDLST(%VAL(IPOUT),7,LSTLEN,%VAL(IPX),21,24)
	  CALL ADDLST(%VAL(IPOUT),7,LSTLEN,%VAL(IPY),25,28)
C
C TELL USER HOW MANY ENTRIES IN OUTPUT LIST
C
	  IF(ILEVEL.GE.2) THEN
            WRITE(PRBUF,104)LSTLEN
	    IF(LSTLEN.EQ.1) PRBUF(28:)='ENTRY'
  104       FORMAT('   OUTPUT LIST HAS ',I7,' ENTRIES')
            CALL LBGONE(PRBUF(20:))
	    CALL WRUSER(' ',ISTAT)
            CALL WRUSER(PRBUF,ISTAT)
	    CALL WRUSER(' ',ISTAT)
	  ENDIF
C
C DEFAULT OUTPUT TITLE IS EITHER THE INPUT TITLE, OR BLANK.
C THEN OBTAIN A NEW ONE FROM THE ENVIRONMENT
C
	  TITLE='Output from XYCURA'
	  IF(IERRI.EQ.0) THEN
	     CALL GTDSCR('INPUT','TITLE','CHARACTER',IVAL,RVAL,
     +                 TITLE,IERR)
        ENDIF
        CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,ISTAT)
C
C COPY INPUT DESCRIPTOR TO OUTPUT (IF AVAILABLE) THEN UPDATE 
C DESCRIPTOR ITEMS
C
	  IF(IERRI.EQ.0) CALL CYDSCR('INPUT','OUTPUT',ISTAT)
	  CALL PTDSCR('OUTPUT','NITEM','INTEGER',7,RVAL,CVAL,IERR)
	  CALL PTDSCR('OUTPUT','LSTLEN','INTEGER',LSTLEN,RVAL,CVAL,
     +    IERR)
	  CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE,
     +	  IERR)
	ENDIF
      ENDIF
C
C REPOSITION IMAGE AT CENTRE OF ARGS
C
      CALL ARCEMA(255,255,1,1,1)
C
C FREE ALL DATA AREAS AND EXIT
C
   99 CALL FRDATA(' ',ISTAT)
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R XYINCA *
C      *            *
C      **************
C
C
* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO INTERACTIVELY OBTAIN A SET OF X,Y POSITIONS AND ATTACHED
*	CHARACTER IDENTIFIERS FROM THE ARGS AND INSERT THEM IN
*	A LIST OF POSITIONS
*     ALSO, IF REQUIRED, TO PUT UP AS CROSSES ANY POSITIONS ALREADY
*     IN THE LIST
*
*METHOD
*       IF WANTED, GET ALL POSNS FROM INPUT LIST AND PUT ON ARGS
*       SCREEN USING S/R CROSS TAKEN FROM PRGM SLICE
*       OBTAIN X,Y POSITION FROM THE ARGS SCREEN USING ASP_PAN AND
*       IDENTIFIER (IF REQUIRED) FROM KEYBOARD USING STARLINK 
*	PARAMETER 'IDENTITY'. 
*	IF THE IDENTIFIER IS BLANK, CREATE ONE USING THE CURRENT
*	COUNT OF BLANK IDENTIFIERS ENTERED. IF THE IDENTIFIER IS IN THE
*	FORM #N, RESET THE BLANK COUNTER TO N. OTHERWISE USE THE
*	IDENTIFIER AS IT STANDS AND ADD IT TO THE LIST.
*
*ARGUMENTS
*	ID (IN/OUT)
*	BYTE(20,MAXLEN)
*		A LIST OF 20 BYTE ASCII IDENTIFIERS
*	X,Y (IN/OUT)
*	REAL(MAXLEN)
*		LISTS OF X,Y POSITIONS
*	MAXLEN (IN)
*	INTEGER
*		THE MAXIMUM NUMBER OF ENTRIES WHICH CAN BE HELD IN THE
*		LISTS
*	LEN (IN/OUT)
*	INTEGER
*		ON ENTRY, GIVES THE NUMBER OF ENTRIES ALREADY IN THE
*		LISTS ID,X AND Y. ON EXIT, GIVES THE NUMBER OF ENTRIES
*		IN THE OUTPUT LISTS.
*       IDENT (IN)
*       LOGICAL
*               IF TRUE, IDENTIFIERS ARE PROMPTED FOR
*       ILEVEL (IN)
*       INTEGER
*               INTERACTION LEVEL: CONTROLS PRINTING OF POSITIONS ON
*               SCREEN AS THEY ARE OBTAINED
*	IERR (OUT)
*	INTEGER
*		ERROR FLAG: ZERO FOR SUCCESS
*		1: LEN .GT. MAXLEN ON ENTRY
*
*     KOVCL (IN)
*     INTEGER
*             FLAG FOR CLEARING OVERLAY PLANE ON ENTRY
*     KFXOUT (IN)
*     INTEGER
*             FLAG FOR PUTTING UP CROSSES FROM INPUT LIST (1=YES)
*     NUMINP
*     INTEGER
*             FLAG FOR NUMBERING INPUT LIST CROSSES (1=NO)
*     COMFAC (IN)
*     REAL
*             COMPRESSION FACTOR OF DISPLAY
*     DX     (IN)
*     REAL
*             DISPLAY X START COORDINATE
*     DY     (IN)
*     REAL
*             DISPLAY Y START COORDINATE
*
*    Input
*     ISX     Int       X width of display 
*     ISY     Int       Y width of display
C     KOPT    Int       flag for XYlist input mark type 1=+,2=tick,3=spot
*     TCOL    Char*1    Colour to paint input markers
*
*STARLINK PARAMETERS
*       IDENTITY
*               CHARACTER IDENTIFIER FOR POSITIONS
*
*CALLS
*	EDRS PACKAGE:
*		LBGONE,GETCMD
*	STARLINK:
*		RDKEYC,CNPAR,CTOR,WRERR,CTOI
*     ASPIC:
*             ASP_PAN,ARGS_CLS,ARGS_NUMIM,ARGS_OVCL
*     THIS FILE:
*             CROSS,ATICK,SPOT,ARGS_OVOPN
*
*NOTES
*	USES BYTE ARRAYS
*
*WRITTEN BY
*	R.F. WARREN-SMITH
*ADAPTED BY
*     A.J. PENNY
* ----------------------------------------------------------------------
C
C
C
      SUBROUTINE XYINCA(ID,X,Y,MAXLEN,LEN,IDENT,ILEVEL,IERR,KOVCL,
     +                  KFXOUT,NUMINP,COMFAC,DX,DY,ISX,ISY,KOPT,TCOL)
C
C
C
      CHARACTER IDBUF*20,INBUF*80,PRBUF*80,TCOL*1
      LOGICAL EXIT,IDENT,FINISH
      REAL X(MAXLEN),Y(MAXLEN)
      BYTE ID(20,MAXLEN)
C
C CHECK ARGUMENTS
C
      IF(MAXLEN.LT.LEN) THEN
	IERR=1
      ELSE
	IERR=0
C
C CHECK LENGTH OF LIST IS NOT -VE, INITIALLISE BLANK IDENTIFIER
C COUNT TO AFTER INPUT LIST
C
      LEN=MAX(0,LEN)
      NBLANK= LEN + 1
C
C  Clear ARGS overlay planes if wanted
C
      IF (KOVCL.EQ.1) THEN
         DO L = 8,15
            CALL ARGS_CLS(L)
         ENDDO
      ENDIF
C
C WRITE CROSSES FROM INPUT LIST POSNS IF REQUIRED
C
      IF (KFXOUT.EQ.1.AND.LEN.GT.0) THEN
         IF (TCOL.EQ.'B') NPLANE = 8
         IF (TCOL.EQ.'W') NPLANE = 9
         IF (TCOL.EQ.'R') NPLANE = 10
         IF (TCOL.EQ.'Y') NPLANE = 11
         IF (TCOL.EQ.'C') NPLANE = 12
         IF (TCOL.EQ.'M') NPLANE = 13
         IF (TCOL.EQ.'G') NPLANE = 14
         CALL ARGS_OVOPN(NPLANE,TCOL)
         CALL ARGS_NUMIM(IDARGS)
         DO K = 1,LEN
            KX = INT(X(K))
            KY = INT(Y(K))
            XA = (REAL(KX)-DX+1.0-1.0)/COMFAC
            YA = (REAL(KY)-DY+1.0-1.0)/COMFAC
            IF (XA.GE.1.0.AND.YA.GE.1.0.AND.
     +          XA.LE.REAL(ISX).AND.YA.LE.REAL(ISY)) THEN
               NUM = K
               IF (NUMINP.EQ.1) NUM = -1
               KN = -1
               IF (MOD(K,2).EQ.0) KN = 1
               IF (KOPT.EQ.1) CALL CROSS(IDARGS,XA,YA,NUM,TCOL)
               IF (KOPT.EQ.2) CALL ATICK(IDARGS,XA,YA,KN,NUM,TCOL)
               IF (KOPT.EQ.3) CALL SPOT(IDARGS,XA,YA,NUM,TCOL)
            ENDIF
         ENDDO
      CALL ARGS_OVCL(NPLANE,.FALSE.)
      ENDIF
C
C
C  Get positions from ARGS using cursor and marking positions
C LOOP WHILE EXIT HAS NOT BEEN SET AND LIST HAS NOT OVERFLOWED
C ------------------------------------------------------------
C
      CALL ARGS_OVOPN(10,'R')
      CALL ARGS_NUMIM(IDARGS)
	EXIT=.FALSE.
   67 CONTINUE
      IF((.NOT.EXIT).AND.(LEN.LE.MAXLEN)) THEN
C
C CALL ASP_PAN TO GET COORDINATES FROM ARGS SCREEN
C AND DRAW A CROSS AT THAT POSITION
C
          CALL ASP_PAN(IX,IY,XA,YA)
C
          IF (KOPT.EQ.1.OR.KOPT.EQ.2) CALL CROSS(IDARGS,XA,YA,-1,'R')
          IF (KOPT.EQ.3) CALL SPOT(IDARGS,XA,YA,-1,'R')
          X(LEN+1) = COMFAC*XA + 1.0 + DX - 1.0
          Y(LEN+1) = COMFAC*YA + 1.0 + DY - 1.0
C
C TEST IF USER WANTS TO STOP
C
          FINISH=((X(LEN+1).LE.DX).AND.(Y(LEN+1).LE.DY))
C
C IF NOT, PRINT POSITION IF REQUIRED
C
          IF(.NOT.FINISH) THEN
            IF(ILEVEL.GE.2) THEN
              KN = LEN + 1
              KXN = INT(X(KN)+0.01)
              KYN = INT(Y(KN)+0.01)
              WRITE(PRBUF,64) KN,KXN,KYN
   64         FORMAT(' ',I5,5X,2I7)
              CALL WRUSER(PRBUF,ISTAT)
            ENDIF
C
C IF IDENTIFIER REQUIRED, OBTAIN FROM ENVIRONMENT
C
          INBUF=' '
            IF(IDENT) THEN
	      CALL RDKEYC('IDENTITY',.FALSE.,1,INBUF,NVAL,ISTAT)
              CALL CNPAR('IDENTITY',ISTAT)
              CALL LBGONE(INBUF)
            ENDIF
            IDBUF=INBUF
          ENDIF
C
C IF STOPPING, SET EXIT
C
          IF(FINISH) THEN
            EXIT=.TRUE.
          ELSE
C
C TEST IF LIST OF INPUT HAS OVERFLOWED
C
              IF(LEN.GE.MAXLEN) THEN
		EXIT=.TRUE.
	      ELSE
C
C INCREMENT LIST LENGTH IF IT WILL NOT OVERFLOW
C
		LEN=LEN+1
		EXIT=.FALSE.
C
C TREAT THE SPECIAL CASES OF BLANK IDENTIFIER OR '#N'
C ----------------------------------------------------
C
C REMOVE LEADING BLANKS FROM IDENTIFIER AND TEST IF ALL BLANK
C
		CALL LBGONE(IDBUF)
		IF(IDBUF.EQ.' ') THEN
C
C IF BLANK, GENERATE AN IDENTIFIER FROM THE BLANK COUNT IN THE FORM
C '#N' AND INCREMENT THE BLANK COUNT
C
		  WRITE(IDBUF,'(I20)')NBLANK
		  IDBUF(1:1)='#'
	          CALL LBGONE(IDBUF(2:))
		  NBLANK=NBLANK+1
C
C IF ID STARTS WITH # THEN SEE IF IT IS FOLLOWED BY AN INTEGER
C IF SO, RESET NBLANK AND PUT ID IN #N STANDARD FORM
C RESET NBLANK SO THAT SUBSEQUENT BLANK IDENTIFIERS ARE CONVERTED TO
C SEQUENTIALLY NUMBERED '#N' FORM
C
		ELSE IF(IDBUF(1:1).EQ.'#') THEN
		  CALL CTOI(IDBUF(2:),NB,ISTATB)
		  IF(ISTATB.EQ.0) THEN
		    NBLANK=NB+1
		    WRITE(IDBUF,'(I20)')NB
		    IDBUF(1:1)='#'
		    CALL LBGONE(IDBUF(2:))
		  ENDIF
		ENDIF
C
C PUT ID INTO IDENTIFIER LIST
C
		DO 16 I=1,20
		  ID(I,LEN)=ICHAR(IDBUF(I:I))
   16		CONTINUE
	      ENDIF
	    ENDIF
C
C IF LIST IS FULL, RETURN
C
	    IF(LEN.GE.MAXLEN) THEN
	      EXIT=.TRUE.
	    ENDIF
	    GO TO 67
	  ENDIF
      CALL ARGS_OVCL(10,.FALSE.)
	ENDIF
C
	END
C
C
C



C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE CROSS (ID,X,Y,NUM)
C
C DRAW A CROSS ON ARGS AT POSITION (X,Y) (USER UNITS) OF WIDTH 5
C PIXELS. (ID IS ID OF IMAGE)
C  IF NUM IS GREATER THAN 0, THE CROSS IS NUMBERED AS NUM.
C
      INTEGER PX,PY,STATUS
      CHARACTER*72 TEXT
      REAL X,Y,UX,UY,DX,DY,XVAL(2),YVAL(2)
      CALL ARGS_UTOP (ID,X,Y,PX,PY,STATUS)
      CALL ARGS_PTOU (ID,PX+2,PY+2,UX,UY,STATUS)
      DX = UX - X
      DY = UY - Y
      XVAL(1) = X - DX
      YVAL(1) = Y
      XVAL(2) = X + DX
      YVAL(2) = Y
      CALL ARGS_POLYL (ID,2,XVAL,YVAL,STATUS)
      XVAL(1) = X
      YVAL(1) = Y - DY
      XVAL(2) = X
      YVAL(2) = Y + DY
      CALL ARGS_POLYL (ID,2,XVAL,YVAL,STATUS)
C
      IF (NUM.GT.0) THEN
         DO K = 1,72
            TEXT(K:K) = ' '
         ENDDO
         WRITE(TEXT,900)NUM
  900    FORMAT(I10)
         KSHIFT = 0
         DO K = 1,9
            IF(TEXT(K:K).EQ.' ') KSHIFT = K
         ENDDO
         DO K = 1,10
            J = K + KSHIFT
            TEXT(K:K) = TEXT(J:J)
         ENDDO
         CALL ARGS_UTOP(ID,X,Y,PX,PY,STATUS)
         CALL ARGS_PTOU(ID,PX+3,PY+3,UXA,UYA,STATUS)
         KX = UXA
         KY = UYA
         CALL ARGTXT(TEXT,'B',0,KX,KY,0)
      ENDIF
C
C
C
      END



C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE SPOT (ID,X,Y,NUM)
C
C DRAW A SPOT ON ARGS AT POSITION (X,Y) (USER UNITS) OF WIDTH 1
C PIXELS. (ID IS ID OF IMAGE)
C  IF NUM IS GREATER THAN 0, THE CROSS IS NUMBERED AS NUM.
C
      INTEGER PX,PY,STATUS
      CHARACTER*72 TEXT
      REAL X,Y,UX,UY,DX,DY,XVAL(2),YVAL(2)
      XVAL(1) = INT(X)
      YVAL(1) = INT(Y)
      XVAL(2) = INT(X)
      YVAL(2) = INT(Y)
      CALL ARGS_POLYL (ID,2,XVAL,YVAL,STATUS)
C
      IF (NUM.GT.0) THEN
         DO K = 1,72
            TEXT(K:K) = ' '
         ENDDO
         WRITE(TEXT,900)NUM
  900    FORMAT(I10)
         KSHIFT = 0
         DO K = 1,9
            IF(TEXT(K:K).EQ.' ') KSHIFT = K
         ENDDO
         DO K = 1,10
            J = K + KSHIFT
            TEXT(K:K) = TEXT(J:J)
         ENDDO
         CALL ARGS_UTOP(ID,X,Y,PX,PY,STATUS)
         CALL ARGS_PTOU(ID,PX+3,PY+3,UXA,UYA,STATUS)
         KX = UXA
         KY = UYA
         CALL ARGTXT(TEXT,'B',0,KX,KY,0)
      ENDIF
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE ATICK (ID,X,Y,KN,NUM)
C
C DRAW A TICK ON ARGS AT POSITION (X+KN*2,Y) (USER UNITS)
C  (ID IS ID OF IMAGE) AND NUMBERS IT IF NUM IS +VE.
C
      INTEGER PX,PY,STATUS
      CHARACTER*72 TEXT
      REAL X,Y,UX,UY,DX,DY,XVAL(2),YVAL(2)
      KX = KN*2
      KXA = KN*3
      KY = KN
      CALL ARGS_UTOP (ID,X,Y,PX,PY,STATUS)
      CALL ARGS_PTOU (ID,PX+KX,PY+KY,UX,UY,STATUS)
      DXA = UX - X
      DY = UY - Y
      CALL ARGS_PTOU(ID,PX+KXA,PY,UX,UY,STATUS)
      DXB = UX - X
      XVAL(1) = X + DXA
      YVAL(1) = Y
      XVAL(2) = X + DXB
      YVAL(2) = Y
      CALL ARGS_POLYL (ID,2,XVAL,YVAL,STATUS)
      XVAL(1) = X + DXA
      YVAL(1) = Y
      XVAL(2) = X + DXA
      YVAL(2) = Y + DY
      CALL ARGS_POLYL (ID,2,XVAL,YVAL,STATUS)
C
C
      IF (NUM.GT.0) THEN
         DO K = 1,72
            TEXT(K:K) = ' '
         ENDDO
         WRITE(TEXT,900)NUM
  900    FORMAT(I10)
         KSHIFT = 0
         DO K = 1,9
            IF(TEXT(K:K).EQ.' ') KSHIFT = K
         ENDDO
         DO K = 1,10
            J = K + KSHIFT
            TEXT(K:K) = TEXT(J:J)
         ENDDO
         CALL ARGS_UTOP(ID,X,Y,PX,PY,STATUS)
         CALL ARGS_PTOU(ID,PX+3,PY+3,UXA,UYA,STATUS)
         KX = UXA
         KY = UYA
         CALL ARGTXT(TEXT,'B',0,KX,KY,0)
      ENDIF
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE ARGS_OVOPN(N,COL)
*
*+ PERFORM ALL INITIALISATION TO ALLOW USE OF BIT PLANE 'N'
* FOR OVERLAYS. OVERLAYS WILL BE IN COLOUR 'COL' (SEE 'ARGS_DECOL')

      INTEGER N,MASKN
      CHARACTER COL

* RESET VSR
      CALL ARGS_VSRRST

* DISABLE OVERLAYS TEMPORARILY
      CALL ARGS_OVCG('0')

* ENABLE PLANE 'N' AND SET ZDI
      MASKN = IAND(N,'000F'X)
      CALL ARGS_FLUSH(9)
      CALL ARGS_S1('ZWEI',ISHFT('0001'X,MASKN))
      CALL ARGS_S1('ZDI1',ISHFT('0001'X,MASKN))

* ENABLE OVERLAYS FOR BIT PLANE 'N'
      CALL ARGS_OVCG('W')
      CALL ARGS_OVC(ISHFT('0001'X,IAND(N,'007'X)),COL)

      END


C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R ARCEMA *
C      *            *
C      **************
C
C  ORIG PROGRAM PTW MOD 28/7/81 WFL TO ACCESS ARGS DATABASE
C  MOD BY AJP TO S/R ARCEMA 82 SEP
C
C ------------------------------------------------------



      SUBROUTINE ARCEMA(KX,KY,KXF,KYF,KW)



      CHARACTER VALUE*80


      INCLUDE 'INTERIM(ERRPAR)'       
      INCLUDE 'INTERIM(FMTPAR)'          



*  IF WANTED, USE PRESENT MAGNIFICATION

      IF (KW.EQ.0) THEN
         CALL ARGS_RDPAR('DISPZOOM',1,VALUE,NVALS,ISTAT)
         CALL ASP_DZTOI('ZXF',VALUE,KXF,ISTAT)
         CALL ASP_DZTOI('ZYF',VALUE,KYF,ISTAT)
      ENDIF

*  DO THE ZOOM

      CALL IZOOM(KX,KY,KXF,KYF)
      CALL ARGS_RDPAR('DISPZOOM',1,VALUE,NVALS,JSTAT)
      CALL ASP_ITODZ('ZXC',KX,VALUE,JSTAT)
      CALL ASP_ITODZ('ZYC',KY,VALUE,JSTAT)
      CALL ASP_ITODZ('ZXF',KXF,VALUE,JSTAT)
      CALL ASP_ITODZ('ZYF',KYF,VALUE,JSTAT)
      CALL ARGS_WRPAR('DISPZOOM',VALUE,1,JSTAT)



      END

      SUBROUTINE IZOOM(IX,IY,IFX,IFY)
      INTEGER IX,IY,IFX,IFY



      CALL ARGS_FLUSH(4)
      CALL ARGS_PUT1('C000'X+IX)
      CALL ARGS_PUT1('A000'X+IY)
      CALL ARGS_PUT1('5001'X)
      CALL ARGS_PUT1(256*(IFY-1)+IFX-1)
      CALL SRSEND

      END
