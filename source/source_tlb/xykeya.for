C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C        ******************
C        *                *
C        * PROGRAM XYKEYA *
C        *                *
C        ******************
C
C
C
C          CALLING SEQUENCE:-
C                XYKEYA [ILEVEL=?] [MAXENTRY=?]
C
C
C          FUNCTION:-
C              It inputs data from the keyboard to make an XYlist of
C              entries, each having an identifier and up to 200 parameters.
C
C              Each parameter has an 30 character descriptor, which you
C              input.
C
C              The original purpose of this included having the first
C              two parameters as X,Y positions, but they can be anything,
C              the name 'XYlist' is just a convenient mnemonic.
C
C          USE:-
C
C
C
C
C          USER PARAMETERS
C
C          NUMPAR          2            The number of parameters
C                                       (including any XY coords, which
C                                       are usually the first two) to
C                                       be input in each entry.
C
C          XYDATA                       The entries. These are input in
C                                       the format '[param1,,,],name'
C                                       The name does not have to be there
C                                       If it is not, it is put in as
C                                       '#n', where n = the number of the
C                                       entry in the file. (unless there has
C                                       been an entry of the form '#m',
C                                       when n is numbered as m+x, where x
C                                       is the number of entries from the
C                                       '#m' entry)
C                                       The entries are asked for again
C                                       and again, and the entries are
C                                       terminated by inputing a blank
C                                       line.
C
C          OUTPUT                       The name of the file the data is
C                                       to be stored in.
C
C          TITLE  Output from XYKEYA    A title to be stored in the OUTPUT
C                                       file.
C
C          HEADER                       The up to 30 character descriptor
C                                       to be added to the output file
C                                       for each parameter
C
C
C          NORMALLY DEFAULTED PARAMETERS:-
C
C          ILEVEL           2           This controls the typing of
C                                       informational messages. If 1 ,then
C                                       no typing; if 2, then typing.
C
C          MAXENTRY      1000           The maximum no of entries to be
C                                       input at the keyboard.
C
C
C
C          A.J.Penny and others  RGO                      JUN-82
C
C
C------------------------------------------------------------------



C   PROGRAM PURPOSE
C
C	TO OBTAIN A LIST OF PARAMETERS AND IDENTIFIERS FROM THE
C	KEYBOARD AND INSERT THEM IN AN XY FILE
C
C  METHOD
C    GET DETAILS AND OPEN WORKSPACE.
C	CALL XYINKA TO INTERACT WITH THE
C	KEYBOARD. OBTAIN AN OUTPUT FILE OF THE REQUIRED SIZE AND INSERT
C	THE LISTS INTO IT. INSERT THE OUTPUT DESCRIPTOR ITEMS
C
C  ARGUMENTS
C	NONE
C
C STARLINK PARAMETERS
C	ILEVEL
C		INTERACTION LEVEL: CONTROLS PRINTING OF INFORMATIONAL 
C		MESSAGES
C     NUMPAR
C             NUMBER OF PARAMETERS IN AN ENTRY
C	MAXENTRY
C		MAX NUMBER OF LIST ENTRIES (USED TO ASSIGN WORKSPACE)
C	NOSPACE/ERROR/
C		ACCESSED IF WORKSPACE CANNOT BE OBTAINED
C	NOLIST/ERROR/
C		ACCESSED IF NO ENTRIES WERE GIVEN AT THE KEYBOARD
C	OUTPUT
C		OUTPUT FILE
C	TITLE
C		A TITLE FOR THE OUTPUT FILE
C
C  CALLS
C	EDRS PACKAGE:
C		GETPAR,GTXYLR,GTXYLW,LBGONE,GTDSCR,PTDSCR
C	STARLINK:
C		GETDYN,WRERR,WRUSER,RDKEYC,CYDSCR,FRDATA
C
C  NOTES
C	USES VAX %VAL FACILITY
C
C  WRITTEN BY
C     A.J. PENNY
C ----------------------------------------------------------------------
C
C
      CHARACTER CVAL*1,TITLE(1)*30,PRBUF*40
      CHARACTER TEXT*80,HEADER*30,TEXTH*7
      LOGICAL VALID
C
C  Set continuation flag
C
      VALID = .TRUE.
C
C OBTAIN INTERACTION LEVEL
C
      ILEVEL=2
      CALL GETPAR('ILEVEL','INTEGER',1,1.0,3.0,.TRUE.,ILEVEL,RVAL,IERR)
C
C GET VALUES FOR THE LIST DIMENSIONS
C NUMVAL = NO OF PARAMETERS AFTER THE NAMES IN THE LIST
C NITEM=NO OF 4-BYTE ITEMS PER LIST RECORD
C
      NUMVAL = 2
      CALL GETPAR('NUMPAR','INTEGER',1,1.0,200.0,.TRUE.,NUMVAL,
     +            RVAL,IERR)
      NITEM = 5 + NUMVAL
C
C SET DEFAULT FOR MAX INPUT TO OUTPUT LIST, THEN OBTAIN VALUE FROM
C ENVIRONMENT
C
      MAXENT = 1000
      CALL GETPAR('MAXENTRY','INTEGER',1,1.0,1.0E6,.TRUE.,
     +            MAXENT,RVAL,IERR)
C
C OBTAIN VIRTUAL MEMORY WORKSPACE TO HOLD IDENTIFIERS AND X,Y,DATA
C VALUES
C
      CALL GETDYN('ID',104,5*MAXENT,IPID,ISTAT1)
      CALL GETDYN('DATA',104,NUMVAL*MAXENT,IPDATA,ISTAT2)
C
C IF SPACE NOT AVAILABLE... GIVE ERROR MESSAGE AND ABORT
C
      IF ((ISTAT1.NE.0).OR.(ISTAT2.NE.0)) THEN
         CALL WRERR('NOSPACE')
         VALID = .FALSE.
      ENDIF
C
C CALL XYINKA TO ADD NEW KEYBOARD VALUES TO THE WORKSPACE
C
      IF (VALID) THEN
         CALL XYINKA(%VAL(IPID),%VAL(IPDATA),NUMVAL,MAXENT,
     +               LENOUT,IERR)
         IF (LENOUT.LE.0.OR.IERR.NE.0) THEN
            CALL WRERR('NOLIST')
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C  Obtain Output frame
C
      IF (VALID) THEN
         CALL GTXYLW('OUTPUT',.FALSE.,NITEM,LENOUT,IPOUT,IERR)
         IF (IERR.NE.0) VALID = .FALSE.
      ENDIF
C
C OUTPUT FRAME SUCCESSFULLY OBTAINED. COPY LISTS FROM WORKSPACE TO
C OUTPUT DATA FRAME
C
      IF (VALID) THEN
         CALL EXTLSA(%VAL(IPID),5,MAXENT,1,LENOUT,1,5,
     +               %VAL(IPOUT),NITEM,LENOUT,1,1)
         CALL EXTLSA(%VAL(IPDATA),NUMVAL,MAXENT,1,LENOUT,1,NUMVAL,
     +               %VAL(IPOUT),NITEM,LENOUT,1,6)
C
C TELL USER HOW MANY ENTRIES IN OUTPUT LIST
C
         IF (ILEVEL.GE.2) THEN
            WRITE(PRBUF,104)LENOUT
	      IF (LENOUT.EQ.1) PRBUF(28:)='ENTRY'
  104       FORMAT('   OUTPUT LIST HAS ',I7,' ENTRIES')
            CALL LBGONE(PRBUF(20:))
	      CALL WRUSER(' ',ISTAT)
            CALL WRUSER(PRBUF,ISTAT)
	      CALL WRUSER(' ',ISTAT)
	   ENDIF
C
C  Obtain Title
C
	   TITLE(1)='Output from XYKEYA'
         CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,ISTAT)
C
C  Input descriptor items
C
	   CALL PTDSCR('OUTPUT','NITEM','INTEGER',NITEM,RVAL,CVAL,IERR)
	   CALL PTDSCR('OUTPUT','LSTLEN','INTEGER',LENOUT,RVAL,CVAL,
     +               IERR)
	   CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE(1),
     +	       IERR)
C
C  Input parameter headers
C
         DO K = 1,NUMVAL
            WRITE(TEXT,900)K
  900       FORMAT('INPUT HEADER FOR PARAMETER NO ',I2)
            CALL WRUSER(TEXT,IERR)
            HEADER = ' '
            IF (K.EQ.1) HEADER = '   X    '
            IF (K.EQ.2) HEADER = '   Y    '
            IF (K.EQ.3) HEADER = '  MAG   '
            CALL RDKEYC('HEADER',.TRUE.,1,HEADER,NVAL,ISTAT)
            WRITE(TEXTH,901)K
  901       FORMAT('HEAD',I3.3)
            CALL PTDSCR('OUTPUT',TEXTH,'CHARACTER',IVAL,RVAL,HEADER,
     +                  IERR)
            CALL CNPAR('HEADER',ISTAT)
         ENDDO
	ENDIF
C
C  Free all data areas
C
      CALL FRDATA(' ',ISTAT)
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R XYINKA *
C      *            *
C      **************
C
C
*PURPOSE
*	TO INTERACTIVELY OBTAIN A SET OF XY POSNS AND DATA AND ATTACHED
*	CHARACTER IDENTIFIERS FROM THE KEYBOARD AND INSERT THEM IN
*     A LIST
*
*METHOD
*	OBTAIN XYDATA AND IDENTIFIER FROM THE KEYBOARD AS CHARACTER STRINGS
*	USING STARLINK PARAMETER XYPOSN. IF A NULL IS GIVEN, RETURN WITH
*	THE CURRENT LIST, OTHERWISE CONVERT THE XYPOSNS AND DATA TO
*	REAL NUMBERS, CHECKING AND RE-PROMPTING FOR INPUT IF AN ERROR
*	OCCURS. IF THE IDENTIFIER IS BLANK, CREATE ONE USING THE CURRENT
*	COUNT OF BLANK IDENTIFIERS ENTERED. IF THE IDENTIFIER IS IN THE
*	FORM #N, RESET THE BLANK COUNTER TO N. OTHERWISE USE THE
*	IDENTIFIER AS IT STANDS AND ADD IT TO THE LIST.
*
*ARGUMENTS
*	ID (IN/OUT)
*	BYTE(20,MAXENT)
*		A LIST OF 20 BYTE ASCII IDENTIFIERS
*	DATA (IN/OUT)
*	REAL(NUMVAL,MAXENT)
*		LISTS OF X,Y POSITIONS AND DATA
*     NUMVAL (IN)
*     INTEGER
*             THE NUMBER OF XYPOSN AND DATA WORDS PER ENTRY
*	MAXENT (IN)
*	INTEGER
*		THE MAXIMUM NUMBER OF ENTRIES WHICH CAN BE HELD IN THE
*		LISTS
*     LENOUT
*     INTEGER (OUT)
*             GIVES THE NUMBER OF ID,DATA ENTRIES ON EXIT
*		IN THE OUTPUT LISTS.
*	IERR (OUT)
*	INTEGER
*		ERROR FLAG: ZERO FOR SUCCESS
*		1: LEN .GT. MAXENT ON ENTRY
*
*STARLINK PARAMETERS
*
*CALLS
*     NONE
*
*NOTES
*	USES BYTE ARRAYS
*
*WRITTEN BY
*     A.J. PENNY
* ----------------------------------------------------------------------
C
C
C
      SUBROUTINE XYINKA(ID,DATA,NUMVAL,MAXENT,LENOUT,IERR)
C
C
      CHARACTER IDBUF*20
      REAL DATA(NUMVAL,MAXENT),ADATA(201)
      BYTE ID(20,MAXENT)
C
C  Set number of keyboard parameters (no of values + identifier)
C
      NUMTOT = NUMVAL + 1
C
C  Check that there is space for entries.
C
      IF(MAXENT.GT.0) THEN
         IERR = 0
      ELSE
         IERR = 1
      ENDIF
C
C  Set blank count and number of entries
C
      NBLANK = 0
      LEN = 0
C
C  Loop getting entries from the keyboard
C ------------------------------------------------------------
C
      IF (IERR.EQ.0) THEN
C
C
C
    1    CONTINUE
         LEN = LEN + 1
         NBLANK = NBLANK + 1
C
C  Obtain the identifier character string and the XYposn and data
C
         CALL XYIN(IDBUF,ADATA,NUMVAL,NBLANK,IOERR)
C
C  If a valid entry, store it, else note last entry not to be saved
C
         IF (IOERR.EQ.0) THEN
            DO I = 1,20
               ID(I,LEN) = ICHAR(IDBUF(I:I))
            ENDDO
            DO I = 1,NUMVAL
               DATA(I,LEN) = ADATA(I)
            ENDDO
         ELSE
            LEN = LEN - 1
         ENDIF
C
C  If list is not now full and a non-blank entry made, get another
C  entry
C
         IF (LEN.LT.MAXENT.AND.IOERR.EQ.0) GO TO 1
C
C
C
      ENDIF
C
C ------------------------------------------------------
C
C
C
C  Store the final number of entries containing data
C
      LENOUT = LEN
C
C
C
	END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      ************
C      *          *
C      * S/R XYIN *
C      *          *
C      ************
C
*  PURPOSE
*        To obtain an XYposn,data,and identifier from the keyboard
*        The XYposn and data are converted to real numbers.
*        If a blank identifier is given, it is taken as '#n', where
*        n is one of the input arguments of the s/r. If an entire
*        balnk line is given, this is noted.
*
*  ARGUMENTS
*
*  (IN)
*
*     NUMVAL
*        The number of XYposn and data values expected
*   
*  (IN/OUT)
*
*     NBLANK
*        The value of n in '#n' assigned to a blank identifier. On
*        output it contains the input, unless a '#m' has been input
*        from the keyboard, when it contains m.
*
*  (OUT)
*
*     IDBUF
*        The character string containing the identifier (which may
*        be '#n' if a blank has been input.
*     DATA
*        The XYposn and data
*     IOERR
*        = 0 for input, = 1 for a blank line input
*
* STARLINK PARAMETERS
*
*	XYDATA
*		USED TO PROMP USER TO ENTER XY DATA AND ID (CHARACTER
*		STRINGS) FOR NEXT LIST ENTRY. NULL TERMINATES INPUT
*		SEQUENCE.
*	WHAT/ERROR/
*		ACCESSED IF THE INPUT FROM XYPOSN CANNOT BE CONVERTED
*		TO REAL NUMBER POSITIONS.
*
*  CALLS
*       EDRS PACKAGE
*		LBGONE
*       STARLINK
*		RDKEYC,CNPAR,CTOR,WRERR,CTOI
*
*
*  WRITTEN BY
*       A.J. PENNY
* -----------------------------------------------------------
C
C
C
      SUBROUTINE XYIN(IDBUF,DATA,NUMVAL,NBLANK,IOERR)
C
C
C
      CHARACTER IDBUF*20,INBUF(101)*80
      REAL DATA(201)
C
C
C
      NUMTOT = NUMVAL + 1
    1 DO K = 1,NUMTOT
         INBUF(K) = ' '
      ENDDO
      CALL RDKEYC('XYDATA',.FALSE.,NUMTOT,INBUF,NVAL,IEND)
C
C CANCEL INPUT PARAMETER FOR USE NEXT TIME
C
      CALL CNPAR('XYDATA',ISTAT)
C
C INTERPRET USER INPUT LINE
C
      DO K = 1,NUMTOT
         CALL LBGONE(INBUF(K))
      ENDDO
      IOERR = 0
      DO K = 1,NUMVAL
         CALL CTOR(INBUF(K),DATA(K),IOERR1)
         IF (IOERR1.NE.0) IOERR = 1
      ENDDO
      IDBUF = INBUF(NUMTOT)
C
C IF A NULL ENTRY WAS MADE, SET IOERR TO INDICATE END OF INPUT
C
      IF(IEND.NE.0) IOERR=-1
C
C IF INPUT COULD NOT BE READ, GIVE MESSAGE AND PROMPT FOR NEW INPUT
C
      IF(IOERR.GT.0) THEN
         CALL WRERR('WHAT')
         GO TO 1
      ENDIF
C
C REMOVE LEADING BLANKS FROM IDENTIFIER
C
      CALL LBGONE(IDBUF)
C
C  IF IDENTIFIER BLANK, GENERATE AN IDENTIFIER IN THE FORM '#N'
C
      IF(IDBUF.EQ.' ') THEN
         WRITE(IDBUF,'(I20)')NBLANK
         IDBUF(1:1)='#'
         CALL LBGONE(IDBUF(2:))
      ENDIF
C
C IF ID STARTS WITH # THEN SEE IF IT IS FOLLOWED BY AN INTEGER
C IF SO, RESET NBLANK AND PUT ID IN #N STANDARD FORM
C RESET NBLANK SO THAT SUBSEQUENT BLANK IDENTIFIERS ARE CONVERTED TO
C SEQUENTIALLY NUMBERED '#N' FORM
C
      IF(IDBUF(1:1).EQ.'#') THEN
         CALL CTOI(IDBUF(2:),NB,ISTATB)
         IF(ISTATB.EQ.0) THEN
            NBLANK = NB
            WRITE(IDBUF,'(I20)')NB
            IDBUF(1:1)='#'
            CALL LBGONE(IDBUF(2:))
         ENDIF
      ENDIF
C
C
C
      END



