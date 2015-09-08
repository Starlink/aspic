C
	subroutine crb_xyedit
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program  XYEDIT *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               XYEDIT
C
C
C          FUNCTION:-
C               This is used to ennable EDRS XY lists to be edited
C               easily.
C
C               By choosing the option an XY list can be turned
C               into an ASCII file, which can then be edited
C               normally. By choosing the other option, such an
C               appropriately structured ASCII file can be
C               turned (back perhaps) into an XY list.
C
C               By choosing TOASCII, the XY list is turned into
C               a file of format:-
C                  One header line with Title, No of parameters,
C                    No of entries as :-
C                         7X,A30,8X,I5,8X,I10
C                  A series of entries with the header descriptors as :-
C                         1X,'HEADER ',A30,' END'
C                  A series of the entries with identifier and the
C                  parameter values as
C                         1X,20A1,5F12.4
C                         5F12.4
C                          .....
C                          .....
C
C                By choosing FROMASCII , such an ASCII file is
C                turned into an XY list
C
C
C
C
C         USER PARAMETERS:-
C
C         OPTION     Toascii                  Flag for which way to go
C                                             Choices are TOASCII,
C                                             FROMASCII
C
C         INPUTA    (ASCII.LIS)               This is the name of the
C         INPUTX                              input file. If the input
C                                             is an ASCII file it must
C                                             be a .LIS file, but the
C                                             user does not type the
C                                             .LIS
C
C         OUTPUTX                             This is the output file
C         OUTPUTA    (ASCII.LIS)              If it is an ASCII file
C                                             it will be made a .LIS
C                                             but again do not type the
C                                             .LIS
C
C
C         A J Penny            RGO                                12-SEP-82
C
C
C--------------------------------------------------------------------------



*  STARLINK PARAMETERS
*      OPTION
*        The choice flag
*
*  CALLS
*      This file
*            XYASCI,ASCIXY
*      Edrs
*            GETCMD
*
*  NOTES
*       Uses VAX %VAL facility
*
*  WRITTEN BY:
*       A.J. PENNY                                      82-6-21
* ------------------------------------------------------------



      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
      CHARACTER PRBUF*72
C
C
C
      K = 1
      CALL GETCMD('OPTION','TOASCII,FROMASCII.',1,K,PRBUF,
     +            KTEXT,IERR)
      IF (IERR.EQ.0) THEN
         IF (K.EQ.1) THEN
            CALL XYASCI
         ELSE
            CALL ASCIXY
         ENDIF
      ENDIF
C
C
C
	call cnpar('OPTION',istat)
	call cnpar('INPUTA',istat)
	call cnpar('INPUTX',istat)
	call cnpar('OUTPUTA',istat)
	call cnpar('OUTPUTX',istat)
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R XYASCI *
C      *            *
C      **************
C
C
*  PURPOSE
*        To make a .LIS file of an EDRS XY file
*
*  METHOD
*        Open the  file, open an output .LIS file, get the
*        file Title, output the Title, get and output the headers,
*        get and output the contents, close the file.
*
*
*  ARGUMENTS
*        None
*
*  STARLINK PARAMETERS
*     INPUTX
*            The input EDRS file
*     OUTPUTA
*            The output .LIS file
*
*  CALLS
*      This file
*            PRALLA,STHEAD
*      Grasp
*            GTHEAD,CHRDYN,CHARLN
*      Edrs
*            GTXYLR,GTDSCR
*      STARLINK:
*            RDKEYC,WRUSER,CNPAR,FRDATA,GETDYN
*
*  NOTES
*       Uses VAX %VAL facility
*
*  WRITTEN BY:
*       A.J. PENNY                                      82-6-21
* ------------------------------------------------------------



      SUBROUTINE XYASCI
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
      CHARACTER CVAL*1,TITLE*30,NAME*9,FILE*13,PRBUF*72
C
C OBTAIN INPUT XY LIST
C
      CALL GTXYLR('INPUTX',.FALSE.,NITEM,LSTLEN,IPIN,IERRXY)
      IF(IERRXY.EQ.0) THEN
C
C  Extract Title and Headers
C
         CALL GTDSCR('INPUTX','TITLE','CHARACTER',IVAL,RVAL,TITLE,
     +               IERR)
         CALL CHARLN(TITLE,KLEN)
         IF (KLEN.EQ.0) TITLE ='.'
         NPAR = NITEM - 5
         NN = 2 + 30*NPAR
         CALL GETDYN('HEADER',FMT_SL,NN,IPHEAD,IERR)
         CALL CHRDYN(%VAL(IPHEAD))
         CALL GTHEAD('INPUTX',NPAR,%VAL(IPHEAD),IERR)
C
C  Open output file
C
    1    CONTINUE
         NAME = 'ASCII'
         CALL RDKEYC('OUTPUTA',.TRUE.,1,NAME,LNAME,IERR)
         IF (IERR.EQ.4) THEN
            CALL WRUSER('BAD NAME',ISTAT)
            CALL CNPAR('OUTPUTA',ISTAT)
            GO TO 1
         ENDIF
         FILE=NAME//'.LIS'
         LU=1
         OPEN (UNIT=LU,FILE=FILE,STATUS='NEW',FORM='FORMATTED')
C
C  STORE : TITLE, NO OF PARAMS, AND NO OF ENTRIES
C
         WRITE (LU,900) TITLE,NPAR,LSTLEN
  900    FORMAT (' ','TITLE ',A30,2X,'PARAMS',I5,2X,'LSTLEN',I10)
C
C  Store the headers
C
         CALL STHEAD(%VAL(IPHEAD),NPAR,NPAR,LU)
C
C CALL PRALLA TO STORE THE CONTENTS OF THE LIST
C
         CALL PRALLA(%VAL(IPIN),NITEM,LSTLEN,LU)
C
C FINALLY TYPE AND STORE THE NUMBER OF ENTRIES
C
         IF (LSTLEN.NE.1) THEN
            WRITE(PRBUF,920)LSTLEN
  920       FORMAT(T22,I7,' LIST ENTRIES')
            CALL WRUSER(PRBUF,ISTAT)
  921       FORMAT (' ',22X,I6,' LIST ENTRIES')
         ELSE
            CALL WRUSER('                         1 LIST ENTRY',ISTAT)
  922       FORMAT (' ',22X,'   ONE LIST ENTRY')
         ENDIF
      ENDIF
C
C FREE DATA AREA AND RETURN
C
      CALL FRDATA(' ',ISTAT)
      CLOSE (UNIT=LU)
      END
C
C
C



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R STHEAD *
C      *            *
C      **************
C
C
C
C   PURPOSE
C      This s/r writes the headers to the output unit
C
C   ARGUMENTS
C   IN
C     HEADER  Character*30(NPAR)  The headers
C     NUM     Integer             The size of the header array
C     NPAR    Integer             The number of headers
C     LU      INteger             The logical unit to write to
C
C   CALLS
C      Grasp
C        CHARLN
C
C
C   A.J.PENNY                   RGO                    83-2-11
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE STHEAD(HEADER,NUM,NPAR,LU)
C
C
C
      CHARACTER*30 HEADER(NUM),HEADT
C
C
C
      DO K = 1,NPAR
         HEADT = HEADER(K)
         CALL CHARLN(HEADT,KLEN)
         IF (KLEN.EQ.0) HEADT = '.'
         WRITE(LU,900)(HEADT(J:J),J=1,30)
  900    FORMAT(' ','HEADER ',30A1,' END')
      ENDDO
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      *************
C      *           *
C      * S/R PRALLA *
C      *           *
C      *************
C
C
C
* PURPOSE
*  TO STORE IN ASCII FORM THE LINES OF AN EDRS FORMAT XY LIST
*
* METHOD
*  TRANSFORM AN EDRS FORMAT LINE INTO ASCII. REPEAT THAT FOR ALL LINES
*
* ARGUMENTS
*  LIST (IN)
*  INTEGER(NITEM,LSTLEN)
* 	THE INPUT X,Y LIST
*  NITEM,LSTLEN (IN)
*  INTEGER
* 	DIMENSIONS OF LIST
*  LU
*  INTEGER
*     FILE NO
*
* CALLS
*  STARLINK:
* 	WRUSER
*
* NOTES
*  USES BYTE ARRAYS
*
* WRITTEN BY
*  R.F. WARREN-SMITH AND A J PENNY                      82-9-13
* ----------------------------------------------------------------------



      SUBROUTINE PRALLA(LIST,NITEM,LSTLEN,LU)
C
C
C
      BYTE ASC(20)
      CHARACTER ID*20
      PARAMETER (MAXITM=2000)
      INTEGER*4 LIST(NITEM,LSTLEN),BUF(MAXITM)
      REAL DATA(MAXITM-5)
C
C SET EQUIVALENCE SO THAT FIRST 20 BYTES ARE ACCESSED AS BYTE
C DATA AND SUBSEQUENT DATA IS REAL*4
C
      EQUIVALENCE (BUF(1),ASC(1)),(BUF(6),DATA(1))
C
C
C
      NINFO = MIN(MAXITM,NITEM) - 5
C
C ----------------------------------------------------
C
C SCAN THROUGH LIST ENTRIES
C
      DO J=1,LSTLEN
C
C COPY ENTRY INTO BUFFER TO ALLOW DIFFERENT DATA TYPES
C
         DO I=1,MIN(NITEM,MAXITM)
            BUF(I)=LIST(I,J)
         ENDDO
C
C CONVERT IDENTIFIER BYTES TO CHARACTERS
C
         DO I = 1,20
            NCHAR=ASC(I)
            ID(I:I)=CHAR(NCHAR)
         ENDDO
C
C  STORE A LINE
C
         DO I = 1,NINFO
            IF (ABS(DATA(I)).GT.999999.0) THEN
               DATA(I) = SIGN(999999.0,DATA(I))
            ENDIF
         ENDDO
         IF (NINFO.LE.4) THEN
            WRITE(LU,933)(ID(I:I),I=1,20),(DATA(I),I=1,NINFO)
         ELSE
            WRITE(LU,933)(ID(I:I),I=1,20),(DATA(I),I=1,4)
            MS = 5
            MTOGO = NINFO - 4
            DO L = 1,5
               IF (MTOGO.GT.0) THEN
                  IF (MTOGO.LE.6) THEN
                     WRITE(LU,934)(DATA(I),I=MS,NINFO)
                     MTOGO = 0
                  ELSE
                     ME = MS + 5
                     WRITE(LU,934)(DATA(I),I=MS,ME)
                     MS = MS + 6
                     MTOGO = MTOGO - 6
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
  934       FORMAT(' ',6F12.4)
  933 FORMAT(' ',20A1,25F12.4)
      ENDDO
C
C ---------------------------------------------------
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R ASCIXY *
C      *            *
C      **************
C
C
C
*  PURPOSE
*        To make an EDRS XY list .BDF file from an ASCII file
*
*  METHOD
*
*
*  ARGUMENTS
*        None
*
*  STARLINK PARAMETERS
*     INPUTA
*            The input .LIS file
*     OUTPUTX
*            The output EDRS file
*
*  CALLS
*      This file
*            PRALLB,RDHEAD
*      Grasp
*            PTHEAD,CHRDYN
*      EDRS Package
*            GTXYLR,GTXYLW,GTDSCR,PTDSCR
*      STARLINK:
*            RDKEYC,WRUSER,CNPAR,FRDATA,GETDYN
*
*  NOTES
*       Uses VAX %VAL facility
*
*  WRITTEN BY:
*       A.J. PENNY                                      82-6-21
* ------------------------------------------------------------



      SUBROUTINE ASCIXY
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
      CHARACTER CVAL*1,TITLE*30,NAME*9,FILE*13,PRBUF*72
C
C  Open input ASCII file
C
    1 CONTINUE
      NAME = 'ASCII'
      CALL RDKEYC('INPUTA',.TRUE.,1,NAME,LNAME,IERR)
      IF (IERR.EQ.4) THEN
         CALL WRUSER('BAD NAME',ISTAT)
         CALL CNPAR('INPUTA',ISTAT)
         GO TO 1
      ENDIF
      FILE=NAME//'.LIS'
      LU=1
      OPEN (UNIT=LU,FILE=FILE,STATUS='OLD',FORM='FORMATTED')
C
C  Get Title, No of params and No of entries
C
      READ (LU,900)TITLE,NPAR,LSTLEN
  900 FORMAT(7X,A30,8X,I5,8X,I10)
C
C  Get headers
C
      NN = 2 + 30*NPAR
      CALL GETDYN('HEADER',FMT_SB,NN,IPHEAD,IERR)
      CALL CHRDYN(%VAL(IPHEAD))
      CALL RDHEAD(LU,NPAR,%VAL(IPHEAD))
C
C  Open the XY file
C
      NITEM = NPAR + 5
      CALL GTXYLW('OUTPUTX',.FALSE.,NITEM,LSTLEN,IPOUT,IERR)
      IF (IERR.EQ.0) THEN
C
C  Store the contents of the file
C
         CALL PRALLB(%VAL(IPOUT),NITEM,LSTLEN,LU)
C
C  Put the descriptors in
C
         CALL PTDSCR('OUTPUTX','TITLE','CHARACTER',IVAL,RVAL,TITLE,
     +               IERR)
         CALL PTDSCR('OUTPUTX','NITEM','INTEGER',NITEM,RVAL,CVAL,
     +               IERR)
         CALL PTDSCR('OUTPUTX','LSTLEN','INTEGER',LSTLEN,RVAL,CVAL,
     +               IERR)
         CALL PTHEAD('OUTPUTX',NPAR,%VAL(IPHEAD),IERR)
C
C  Type the number of entries
C
         IF (LSTLEN.NE.1) THEN
            WRITE(PRBUF,920)LSTLEN
  920       FORMAT(T22,I7,' LIST ENTRIES')
            CALL WRUSER(PRBUF,ISTAT)
  921       FORMAT (' ',22X,I6,' LIST ENTRIES')
         ELSE
            CALL WRUSER('                         1 LIST ENTRY',ISTAT)
  922       FORMAT (' ',22X,'   ONE LIST ENTRY')
         ENDIF
      ENDIF
C
C FREE DATA AREA AND RETURN
C
      CALL FRDATA(' ',ISTAT)
      CLOSE (UNIT=LU)
      END
C
C
C



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R RDHEAD *
C      *            *
C      **************
C
C
C
C   PURPOSE
C      This s/r reads headers from an input unit
C
C   ARGUMENTS
C  IN
C    LU       Integer      The logical unit number of the input
C    NPAR     Integer      The number of headers to read
C  OUT
C    HEADER   Character*30(NPAR)   The headers read
C
C   CALLS
C
C
C   A.J.PENNY                   RGO                    83-2-11
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE RDHEAD(LU,NPAR,HEADER)
C
C
C
      CHARACTER*30 HEADER(NPAR)
C
C
C
      DO K = 1,NPAR
         READ(LU,900)HEADER(K)
  900    FORMAT(1X,7X,A30,4X)
      ENDDO
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      *************
C      *           *
C      * S/R PRALLB *
C      *           *
C      *************
C
C
C
* PURPOSE
*  To store in an EDRS file a line from an ASCII file
*
* METHOD
*  TRANSFORM AN ASCII FORMAT LINE INTO EDRS. REPEAT THAT FOR ALL LINES
*
* ARGUMENTS
*  LIST (IN)
*  INTEGER(NITEM,LSTLEN)
* 	THE INPUT X,Y LIST
*  NITEM,LSTLEN (IN)
*  INTEGER
* 	DIMENSIONS OF LIST
*  LU
*  INTEGER
*     FILE NO
*
* CALLS
*  STARLINK:
* 	WRUSER
*
* NOTES
*  USES BYTE ARRAYS
*
* WRITTEN BY
*  R.F. WARREN-SMITH AND A J PENNY                      82-9-13
* ----------------------------------------------------------------------



      SUBROUTINE PRALLB(LIST,NITEM,LSTLEN,LU)
C
C
C
      BYTE ASC(20)
      CHARACTER ID*20
      PARAMETER (MAXITM=2000)
      INTEGER*4 LIST(NITEM,LSTLEN),BUF(MAXITM)
      REAL DATA(MAXITM-5)
C
C SET EQUIVALENCE SO THAT FIRST 20 BYTES ARE ACCESSED AS BYTE
C DATA AND SUBSEQUENT DATA IS REAL*4
C
      EQUIVALENCE (BUF(1),ASC(1)),(BUF(6),DATA(1))
C
C
C
      NPAR = MIN(MAXITM,NITEM) - 5
C
C ----------------------------------------------------
C
C SCAN THROUGH LIST ENTRIES
C
      DO J=1,LSTLEN
C
C  Read a line
C
         IF (NPAR.LE.4) THEN
            READ(LU,933)(ID(I:I),I=1,20),(DATA(I),I=1,NPAR)
         ELSE
            READ(LU,933)(ID(I:I),I=1,20),(DATA(I),I=1,4)
            MS = 5
            MTOGO = NPAR - 4
            DO L = 1,5
               IF (MTOGO.GT.0) THEN
                  IF (MTOGO.LE.6) THEN
                     READ(LU,934)(DATA(I),I=MS,NPAR)
                     MTOGO = 0
                  ELSE
                     ME = MS + 5
                     READ(LU,934)(DATA(I),I=MS,ME)
                     MS = MS + 6
                     MTOGO = MTOGO - 6
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
  934       FORMAT(' ',6F12.4)
  933 FORMAT(' ',20A1,4F12.4)
C
C  Transform Identifier into proper type and place in Equiv area
C
      DO I = 1,20
         ASC(I) = ICHAR(ID(I:I))
      ENDDO
C
C  Transfer line to EDRS file
C
         DO I=1,MIN(NITEM,MAXITM)
            LIST(I,J) = BUF(I)
         ENDDO
      ENDDO
C
C ---------------------------------------------------
C
C
C
      END



