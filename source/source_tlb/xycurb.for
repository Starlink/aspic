C
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   XYCURB *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-   XYCURB  [ILEVEL= ]
C
C
C          FUNCTION:-
C               This program displays a section of an XY list of positions on
C               the ARGS, numbered in their order in the list. By means of
C               the cursor you can amend the positions of the stars in that
C               section.
C               You can then choose wether to store just the
C               amended section or the whole input list with the amended
C               section.The identifiers of the objects are preserved.
C
C               Optionally,  positions which lie outside the displayed area
C               can not be put up, with their positions left unchanged.
C
C               You can choose wether to keep points which lie
C               outside the displayed area.
C
C               A position can be removed from the list by putting the
C               refined position to the top l.h. corner of the image.
C
C
C          USE:-
C               When the section is put up, the cursor is set to the 1st
C               object. You move the cursor to an amended posn and press the
C               red button. The position is stored and the cursor moves to
C               the next object, until the section is finished.
C               A position to the top l.h. of the image
C               means that that position is to be removed from the list
C               To exit from amending positions, place the cursor
C               at the bottom l.h. corner of the image or below and to  the
C               left of that corner and press the button.  The remaining
C               positions will still be copied over.
C
C
C
C         USER PARAMETERS:-
C
C         INPUT                               The input XY list
C
C         LIMITS          whole               The section of the list to be
C                                             amended.
C
C         OVERCL          N                   Flag for clearing the crosses
C                                             left  on  the  ARGS  from any
C                                             previous program. Choices are
C                                             N,Y
C
C         NUMINP          NO                  Flag for adding the number of
C                                             the cross of the input list.
C
C         OUTPUT                              The  Output  file  containing
C                                             the positions
C
C         TITLE                               The Title to be  included  in
C                                             the Output file.
C
C         ALLINP        YES                   The output list to be the
C                                             input list with the amended
C                                             positions, or not, when only
C                                             the amended posns are stored
C
C         OPTION        INSIDE                Choice to display only those
C                                             inside the area on the ARGS
C                                             or all. Choices are INSIDE,ALL.
C
C         SAVEOUT       NO                    Choice wether to save points
C                                             outside displayed area. Choices
C                                             are YES,NO.
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         ILEVEL          2                   Flag for  outputting  on  the
C                                             terminal  the  positions from
C                                             the cursor as yo  along.  The
C                                             default  (=2) makes it happen
C                                             automatically. Setting it  to
C                                             1   on  running  the  program
C                                             supresses it.
C
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
C         A J Penny                RGO                             3-SEP-82
C
C
C---------------------------------------------------------------------



C
*CALLS
*       THIS FILE
*            XYINCB
*	THIS PACKAGE:
*		GETPAR,GTXYLR,EXTLST,GTXYLW,ADDLST,LBGONE,GTDSCR,
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
      PROGRAM XYCURB
C
C
C
      CHARACTER CVAL*1,TITLE*30,PRBUF*40,KOVC*1,TEXT*72
      INTEGER KVAL(2)
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C OBTAIN INTERACTION LEVEL
C
      ILEVEL=2
      CALL GETPAR('ILEVEL','INTEGER',1,1.0,3.0,.TRUE.,ILEVEL,RVAL,IERR)
C
C DETERMINE IF OVERLAYS TO BE CLEARED BEFORE ENTRY
C
      CALL WRUSER('CLEAR PREVIOUS CROSSES ?',JSTAT)
      KOVC = 'Y'
      CALL RDKEYC('OVERCL',.TRUE.,1,KOVC,NVAL,ISTAT)
      IF (KOVC.NE.'Y') KOVC = 'N'
C
C  Get if numbers to be added to positions
C
      CALL WRUSER('NUMBER THE POINTS ?',ISTAT)
      NUMINP = 2
      CALL GETCMD('NUMINP','NO,YES.',1,NUMINP,PRBUF,NVAL,ISTAT)
C
C OBTAIN THE INPUT DATA FRAME
C
      CALL WRUSER('INPUT XYLIST ?',ISTAT)
      CALL GTXYLR('INPUT',.TRUE.,NITEM,LSTLEN,IPIN,IERR)
      IF (IERR.NE.0) THEN
         CALL WRUSER('INVALID XY LIST',ISTAT)
         GO TO 99
      ENDIF
C
C  GET LIMITS OF INPUT LIST TO BE USED
C
      KVAL(1) = 1
      KVAL(2) = LSTLEN
      CALL RDKEYI('LIMITS',.TRUE.,2,KVAL,I,IERR)
      IF (KVAL(1).LE.1.OR.KVAL(1).GT.LSTLEN) KVAL(1) = 1
      IF (KVAL(2).LT.1.OR.KVAL(2).GT.LSTLEN) KVAL(2) = LSTLEN
      LENST = KVAL(1)
      LENEND = KVAL(2)
C
C  GET IF OUTPUT ALL OR JUST THE SECTION DONE
C
      KALLIN = 1
      CALL GETCMD('ALLINP','YES,NO.',1,KALLIN,PRBUF,KK,IERR)
C
C  Get if only to handle those inside display area
C
    1 KINSID = 1
      CALL GETCMD('INSIDE','INSIDE,ALL,HELP,?.',1,KINSID,PRBUF,KK,IERR)
      IF (KINSID.EQ.3.OR.KINSID.EQ.4) THEN
         CALL WRUSER('Choices are:  INSIDE  Only handle positions'
     +               //' inside displayed area',ISTAT)
         CALL WRUSER('              ALL     Handle all, inside or out',
     +               ISTAT)
         CALL CNPAR('INSIDE',ISTAT)
         GO TO 1
      ENDIF
C
C  Get wether to save points which lie outside displayed area
C
    2 KSAVE = 2
      CALL GETCMD('SAVEOUT','YES,NO,HELP,?.',1,KSAVE,PRBUF,KK,IERR)
      IF (KSAVE.EQ.3.OR.KSAVE.EQ.4) THEN
         CALL WRUSER('Choices are:  YES  Save points outside displayed'
     +               //' area',ISTAT)
         CALL WRUSER('              NO   Do not',ISTAT)
         CALL CNPAR('SAVEOUT',ISTAT)
         GO TO 2
      ENDIF
C
C  COPY XY INPUT LIST TO WORKSPACE
C
      NXY = 3*LSTLEN
      CALL GETDYN('WORK',204,NXY,IPW,ISTAT)
      IF (ISTAT.NE.0) THEN
         CALL WRUSER('CANT GET WORK SPACE',ISTATA)
         GO TO 99
      ENDIF
      CALL EXTLSA (%VAL(IPIN),NITEM,LSTLEN,1,LSTLEN,6,7,
     +             %VAL(IPW),3,LSTLEN,1,1)
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
C CALL XYINCB TO PUT NEW VALUES TO THE WORKSPACE
C
      CALL XYINCB(%VAL(IPW),LSTLEN,
     +      ILEVEL,IERR,KOVC,NUMINP,COMFAC,DX,DY,ISX,ISY,
     +      LENST,LENEND,KLEN,KALLIN,KINSID,KSAVE)
C
C  REPOSITION IMAGE AT CENTRE OF SCREEN
C
      CALL ARCEMA(255,255,1,1,1)
C
C IF NO LIST OBTAINED, EXIT
C
      IF (KLEN.EQ.0) THEN
         CALL WRERR('NOLIST')
         GO TO 99
      ENDIF
C
C TELL USER HOW MANY ENTRIES IN OUTPUT LIST
C
      IF (ILEVEL.GE.2) THEN
         WRITE(PRBUF,104)KLEN
  104    FORMAT('   OUTPUT LIST HAS ',I7,' ENTRIES')
         IF (KLEN.EQ.1) PRBUF(28:)='ENTRY'
         CALL LBGONE(PRBUF(20:))
         CALL WRUSER(' ',ISTAT)
         CALL WRUSER(PRBUF,ISTAT)
      ENDIF
C
C  Get Output frame
C
	CALL GTXYLW('OUTPUT',.FALSE.,NITEM,KLEN,IPOUT,IERR2)
	IF(IERR2.EQ.0) THEN
C
C OUTPUT FRAME SUCCESSFULLY OBTAINED. COPY IDENTIFIERS AND POSNS
C FROM INPUT LIST TO OUTPUT. THEN COPY NEW POSNS FROM WORKSPACE TO
C OUTPUT.
C
         CALL STORE(%VAL(IPIN),NITEM,LSTLEN,%VAL(IPW),%VAL(IPOUT),KLEN)
C
C DEFAULT OUTPUT TITLE IS EITHER THE INPUT TITLE, OR BLANK.
C THEN OBTAIN A NEW ONE FROM THE ENVIRONMENT
C
	    CALL GTDSCR('INPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE,
     +      IERR)
          CALL CHARLN(TITLE,LEN)
          IF (LEN.EQ.0) TITLE = 'Output from XYCURB'
          CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,ISTAT)
C
C COPY INPUT DESCRIPTOR TO OUTPUT (IF AVAILABLE) THEN UPDATE
C DESCRIPTOR ITEMS
C
	  CALL CYDSCR('INPUT','OUTPUT',ISTAT)
	  CALL PTDSCR('OUTPUT','LSTLEN','INTEGER',KLEN,RVAL,CVAL,
     +    IERR)
	  CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE,
     +	  IERR)
      ENDIF
C
C  Exit
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
C      * S/R XYINCB *
C      *            *
C      **************
C
C
* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO INTERACTIVELY OBTAIN A SET OF X,Y POSITIONS FROM AN INPUT
*     XY LIST, USING THE ARGS CURSOR TO MODIFY THE POSITIONS
*
*METHOD
*        GET ALL POSNS FROM INPUT LIST AND PUT ON ARGS
*       SCREEN USING S/R CROSS
*       OBTAIN X,Y POSITION FROM THE ARGS SCREEN USING ASP_PAN
*
*ARGUMENTS
*	X,Y (IN/OUT)
*	REAL(LSTLEN)
*		LISTS OF X,Y POSITIONS
*     LSTLEN (IN)
*          NO OF OBJECTS IN INPUT LIST
*       ILEVEL (IN)
*       INTEGER
*               INTERACTION LEVEL: CONTROLS PRINTING OF POSITIONS ON
*               SCREEN AS THEY ARE OBTAINED
*	IERR (OUT)
*	INTEGER
*		ERROR FLAG: ZERO FOR SUCCESS
*		1: LEN .GT. MAXLEN ON ENTRY
*
*     KOVC (IN)
*     LOGICAL
*             FLAG FOR CLEARING OVERLAY PLANE ON ENTRY
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
*  Input
*     NUMINP Int       If = 2 Put up numbers at crosses, if =1 dont
*     ISX    Int       X width of ARGS display
*     ISY    Int       Y width of ARGS display
*     KALLIN Int       If=1, store all, if = 2 store only the section done
*     KINSID Int       If=1, ignore points outside display, if=2 do all
*     KSAVE  Int       If=1, save points outside displayed area, if=2 not
*  Output
*     KLEN   Int       No of positions to store
*
*STARLINK PARAMETERS
*
*CALLS
*	EDRS PACKAGE:
*		GETCMD
*	STARLINK:
*		RDKEYC,CNPAR,CTOR,WRERR,CTOI
*     ASPIC:
*             ASP_PAN,ARGS_OVOPN,ARGS_OVOP,ARGS_NUMIM,ARGS_OVCL
*     THIS FILE:
*             CROSS,CROSSA
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
      SUBROUTINE XYINCB(XY,LSTLEN,ILEVEL,IERR,KOVC,NUMINP,
     +                  COMFAC,DX,DY,ISX,ISY,LENST,LENEND,
     +                  KLEN,KALLIN,KINSID,KSAVE)
C
C
C
      CHARACTER PRBUF*80,KOVC*1
      LOGICAL EXIT,FINISH
      REAL XY(3,LSTLEN)
C
C  Set up which stars are wanted
C
      DO K = 1,LSTLEN
         XY(3,K) = 1.0
      ENDDO
      IF (KALLIN.EQ.2) THEN
         IF (LENST.NE.1) THEN
            DO K = 1,LENST-1
               XY(3,K) = 0.0
            ENDDO
         ENDIF
         IF (LENEND.NE.LSTLEN) THEN
            DO K = LENEND,LSTLEN
               XY(3,K) = 0.0
            ENDDO
         ENDIF
      ENDIF
C
C OPEN OVERLAY PLANE FOR WRITING CROSSES AND CLEAR IT IF REQUIRED
C GREEN CROSSES FOR INPUT LIST
C
      IF (KOVC.EQ.'N') THEN
         CALL ARGS_OVOPN(8,'B')
      ELSE
         CALL ARGS_OVOP(8,'B')
      ENDIF
C
C WRITE CROSSES FROM INPUT LIST POSNS
C
         CALL ARGS_NUMIM(IDARGS)
         DO K = LENST,LENEND
            XA = (XY(1,K)-DX+1.0-1.0)/COMFAC
            YA = (XY(2,K)-DY+1.0-1.0)/COMFAC
            IF ((XA.GE.1.0.AND.YA.GE.1.0
     +          .AND.XA.LE.REAL(ISX).AND.YA.LE.REAL(ISY))
     +          .OR.(KINSID.EQ.2)) THEN
               IF (NUMINP.EQ.1) THEN
                  CALL CROSS(IDARGS,XA,YA)
               ELSE
                  CALL CROSSA(IDARGS,XA,YA,K)
               ENDIF
            ENDIF
         ENDDO
C
C  CLOSE INPUT LIST OVERLAY
C
      CALL ARGS_OVCL(8,.FALSE.)
      IF (KOVC.EQ.'N') THEN
         CALL ARGS_OVOPN(9,'R')
      ELSE
         CALL ARGS_OVOP(9,'R')
      ENDIF
C
C  Write header for typing output
C
      IF (ILEVEL.EQ.2) THEN
         CALL WRUSER(' ',ISTAT)
         CALL WRUSER('    No     X shift    Y shift',ISTAT)
      ENDIF
C
C LOOP WHILE EXIT HAS NOT BEEN SET AND LIST HAS NOT OVERFLOWED
C ------------------------------------------------------------
C
      CALL ARGS_NUMIM(IDARGS)
      LENA = LENST - 1
	EXIT=.FALSE.
      DO WHILE (.NOT.EXIT)
         LENA = LENA + 1
C
C  PUT CURSOR AT NEXT POSN
C
         XA = (XY(1,LENA)-DX)/COMFAC
         YA = (XY(2,LENA)-DY)/COMFAC
         IF ((XA.GE.1.0.AND.YA.GE.1.0
     +       .AND.XA.LE.REAL(ISX).AND.YA.LE.REAL(ISY))
     +       .OR.(KINSID.EQ.2)) THEN
            CALL ARGS_UTOA(IDARGS,XA,YA,KPX,KPY,ISTAT)
            CALL ARCEMA(KPX,KPY,KFX,KFY,0)
C
C CALL ASP_PAN TO GET COORDINATES FROM ARGS SCREEN
C AND DRAW A CROSS AT THAT POSITION
C
             CALL ASP_PAN(IX,IY,XA,YA)
C
             CALL CROSS(IDARGS,XA,YA)
C
C  Test if this means finish
C
             FINISH = (XA.LE.1.0.AND.YA.LE.1.0)
             IF (.NOT.FINISH) THEN
C
C  Test if remove this point
C
                IF (XA.LE.1.0.AND.YA.GE.REAL(ISY)) THEN
                   XY(3,LENA) = 0.0
                ELSE
C
C  If real position, store, and type out if wanted
C
                   XN = XA*COMFAC + 1.0 + DX - 1.0
                   YN = YA*COMFAC + 1.0 + DY - 1.0
                   KXN = XN - XY(1,LENA)
                   KYN = YN - XY(2,LENA)
                   IF (ILEVEL.GE.2) THEN
                      WRITE(PRBUF,64) LENA,KXN,KYN
   64                 FORMAT(' ',I5,5X,I7,4X,I7)
                      CALL WRUSER(PRBUF,ISTAT)
                   ENDIF
                   XY(1,LENA) = XN
                   XY(2,LENA) = YN
                ENDIF
             ENDIF
          ENDIF
C
C  IF REACHED END OF LIST, OR USER FINISHED, EXIT
C
         IF((LENA.EQ.LENEND).OR.FINISH)EXIT=.TRUE.
      ENDDO
C
C
C
      CALL ARGS_OVCL(9,.FALSE.)
C
C  Reject those outside displayed area if so wanted
C
      IF (KSAVE.EQ.2) THEN
         DO K = 1,LSTLEN
            XA = (XY(1,K)-DX)/COMFAC
            YA = (XY(2,K)-DY)/COMFAC
            IF ((XA.LT.1.0.OR.YA.LT.1.0
     +           .OR.XA.GT.REAL(ISX).OR.YA.GT.REAL(ISY))
     +           .OR.(KINSID.EQ.2)) THEN
               XY(3,K) = 0.0
            ENDIF
         ENDDO
      ENDIF
C
C  Add up how many positions to be stored
C
      KLEN = 0
      DO K = 1,LSTLEN
         IF (XY(3,K).GT.0.5) KLEN = KLEN + 1
      ENDDO
C
C
C
	END
C
C
C



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE CROSS (ID,X,Y)
C
C DRAW A CROSS ON ARGS AT POSITION (X,Y) (USER UNITS) OF WIDTH 5
C PIXELS. (ID IS ID OF IMAGE)
C
      INTEGER PX,PY,STATUS
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
      END



C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE CROSSA (ID,X,Y,NUM)
C
C DRAW A CROSS ON ARGS AT POSITION (X,Y) (USER UNITS) OF WIDTH 5
C PIXELS. (ID IS ID OF IMAGE)
C AND NUMBER IT
C
      INTEGER KPX,KPY,STATUS
      CHARACTER*72 TEXT
      REAL X,Y,UX,UY,DX,DY,XVAL(2),YVAL(2)
      CALL ARGS_UTOP (ID,X,Y,KPX,KPY,STATUS)
      CALL ARGS_PTOU (ID,KPX+2,KPY+2,UX,UY,STATUS)
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
      DO K = 1,72
         TEXT(K:K) = ' '
      ENDDO
      WRITE(TEXT,900)NUM
  900 FORMAT(I10)
      KSHIFT = 0
      DO K = 1,9
         IF(TEXT(K:K).EQ.' ') KSHIFT = K
      ENDDO
      DO K = 1,10
         J = K + KSHIFT
         TEXT(K:K) = TEXT(J:J)
      ENDDO
      CALL ARGS_UTOP(ID,X,Y,KPX,KPY,STATUS)
      CALL ARGS_PTOU(ID,KPX+3,KPY+3,UXA,UYA,STATUS)
      KX = UXA
      KY = UYA
      CALL ARGTXT(TEXT,'B',0,KX,KY,0)
C
C
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



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R STORE  *
C      *            *
C      **************
C
C
C   PURPOSE
C     Copy input file over and new psoitions, ignoring removed points
C
C   ARGUMENTS
C  IN
C    AIN    Real(NX,NY)     input file
C    NX     Int             No of items in an IN record
C    NY     Int             No of records in In
C    ANEW   Real(3,NY)      New positions and remove flags
C  OUT
C    OUT    Real(NX,KLEN)   Output list
C
C   STARLINK PARAMETERS
C     None
C
C
C   CALLS
C     None
C
C   USES
C
C
C   A.J.PENNY                   RGO                    83-6-21
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE STORE(AIN,NX,NY,ANEW,OUT,KLEN)
C
C
C
      REAL AIN(NX,NY),ANEW(3,NY),OUT(NX,KLEN)
C
C
C
      J = 0
      DO K = 1,NY
         IF (ANEW(3,K).GT.0.5) THEN
            J = J + 1
            DO L = 1,NX
               OUT(L,J) = AIN(L,K)
            ENDDO
            DO L = 1,2
               OUT(L+5,J) = ANEW(L,K)
            ENDDO
         ENDIF
      ENDDO
C
C
C
      END



