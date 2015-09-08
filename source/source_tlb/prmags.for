C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *********************
C                     *                   *
C                     * Program   PRMAGS *
C                     *                   *
C                     *********************
C
C
C
C          CALLING SEQUENCE:-
C               PRMAGS
C
C
C          FUNCTION:-
C               It makes a neat magnitude listing of an XY list file
C               and can type it out as it goes. The .LIS file can be
C               output on any printing device.
C               It can only list out the first 15 parameters in an
C               entry. It ignores the rest.
C
C
C          USE:-
C               It is complementary to XYLIST and XYLISP, which  are  parts
C               of the EDRS package.
C
C
C
C         USER PARAMETERS:-
C
C         INPUT                               This is the name of the  .BDF
C                                             file which you want to list.
C
C       OUTPUT              PRMAGS.LIS        This is the formatted file to
C                                             which    the    results   are
C                                             written.  The  user  supplies
C                                             the  name  only;  the program
C                                             adds .LIS.
C                                             PRMAGS is the default
C                                             name.
C
C        TYPING           NO                  Flag for typing the summary
C                                             as the program goes. Choices
C                                             are YES,NO
C
C        HEADER                               Text to be written at the
C                                             top of each mag column. Each
C                                             can be up to 3 characters.
C
C        XYPOSN          YES                  Flag for wether 1st two
C                                             columns are the XY position.
C                                             Choices are YES,NO
C
C
C
C
C         A J Penny            RGO                                 3-SEP-82
C
C
C--------------------------------------------------------------------------



*  PURPOSE
*        To type and make a .LIS file of the MAGS output file.
*
*  METHOD
*        Open the  file, open an output .LIS file, get the
*        file Title, output the Title, call PRALL to output the
*        contents, and close the .LIS file.
*
*
*  ARGUMENTS
*        None
*
*  STARLINK PARAMETERS
*     INPUT
*            The input EDRS file
*     OUTPUT
*            The output .LIS file
*
*  CALLS
*      This file
*            PRALL
*      EDRS Package
*            GTXYLR,GTDSCR
*      STARLINK:
*            RDKEYC,WRUSER,CNPAR,FRDATA
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
      CHARACTER CVAL*1,TITLE*30,NAME*9,FILE*13,HVAL(15)*3
C
C OBTAIN INPUT XY LIST
C
      CALL GTXYLR('INPUT',.FALSE.,NITEM,LSTLEN,IPIN,IERRXY)
      IF(IERRXY.EQ.0) THEN
C
C INPUT SUCCESSFULLY OBTAINED... EXTRACT TITLE
C
         TITLE = ' '
         CALL GTDSCR('INPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE,IERR)
C
C  Open output file
C
    1    CONTINUE
         NAME = 'PRMAGS'
         CALL RDKEYC('OUTPUT',.TRUE.,1,NAME,LNAME,IERR)
         IF (IERR.EQ.4) THEN
            CALL WRUSER('BAD NAME',ISTAT)
            CALL CNPAR('OUTPUT',ISTAT)
            GO TO 1
         ENDIF
         FILE=NAME//'.LIS'
         LU=1
         OPEN (UNIT=LU,FILE=FILE,STATUS='NEW',FORM='FORMATTED')
C
C  Get if typing out as well
C
         K = 1
         CALL GETCMD('TYPING','NO,YES.',1,K,PRBUF,NVAL,IERR)
         IF (K.EQ.2) THEN
            KTYPE = 1
         ELSE
            KTYPE = 0
         ENDIF
C
C  Get type of 1st two columns
C
      CALL WRUSER(' ',ISTAT)
      KPOSN = 1
      CALL GETCMD('XYPOSN','YES,NO.',1,KPOSN,PRBUF,NVAL,IERR)
      IF (KPOSN.NE.1) KPOSN = 0
C
C  Get headings
C
      NINFO = MIN(20,NITEM) - 5
      CALL WRUSER(' ',ISTAT)
      CALL WRUSER('INPUT THE HEADERS',ISTAT)
      HVAL(1) = ' X '
      HVAL(2) = ' Y '
      HVAL(3) = ' V '
      HVAL(4) = 'B-V'
      HVAL(5) = 'V-R'
      HVAL(6) = 'R-I'
      IF (KPOSN.EQ.0) THEN
         HVAL(1) = ' V '
         HVAL(2) = 'B-V'
         HVAL(3) = 'V-R'
         HVAL(4) = 'R-I'
      ENDIF
      DO K = 1,NINFO
         CALL CNPAR('HEADER',ISTAT)
         CALL RDKEYC('HEADER',.TRUE.,1,HVAL(K),NVAL,IERR)
      ENDDO
      IF (NINFO.LT.15) THEN
         LINFO = NINFO + 1
         DO K = LINFO,15
            HVAL(K) = '   '
         ENDDO
      ENDIF
C
C  Type Title
C
         IF (KTYPE.EQ.1) THEN
            CALL WRUSER(' ',ISTAT)
            CALL WRUSER(' ',ISTAT)
            CALL WRUSER('                   TITLE: '//TITLE,ISTAT)
            CALL WRUSER(' ',ISTAT)
         ENDIF
C
C  Store Title
C
         WRITE (LU,900) TITLE
  900    FORMAT ( /// ,27X,'TITLE: ',A,//)
C
C CALL PRALL TO TYPE OUT AND PRNT THE CONTENTS OF THE LIST
C
         CALL PRALL(%VAL(IPIN),NITEM,LSTLEN,LU,KTYPE,HVAL,KPOSN)
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
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      *************
C      *           *
C      * S/R PRALL *
C      *           *
C      *************
C
C
C
* PURPOSE
*  TO TYPE OUT AND STORE IN A FILE PRINT THE CONTENTS OF AN
*  FILE PRODUCED BY 'MAGS'
*
* METHOD
*  TYPE TITLE AND HEADINGS, TYPE TABLE OF IDENTIFIERS, X,Y
*  POSITIONS AND THE OTHER NUMBERS . TYPE TOTAL NUMBER OF
*  ENTRIES IN LIST. STORE ALL THAT IN  A FILE.
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
*  R.F. WARREN-SMITH AND A J PENNY                      82-6-21
* ----------------------------------------------------------------------



      SUBROUTINE PRALL(LIST,NITEM,LSTLEN,LU,KTYPE,HVAL,KPOSN)
C
C
C
      BYTE ASC(20)
      CHARACTER ID*20,PRBUF*72,HVAL(15)*3
C
C SET MAX NUMBER OF ITEMS TO BE PRINTED
C
      PARAMETER (MAXITM=30)
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
C
C NINFO IS THE NUMBER OF NON-CHARACTER ITEMS TO BE PRINTED
C
      NINFO=MIN(20,NITEM)-5
      MINFO = MIN(6,NINFO)
C
C TYPE TITLES AND UNDERLINING
C
      IF (KTYPE.EQ.1) THEN
         CALL WRUSER(' ',ISTAT)
         IF (KPOSN.EQ.0) THEN
            WRITE(PRBUF,900)(HVAL(K),K=1,MINFO)
  900       FORMAT(' ','IDENTIFIER',2X,6(3X,A3,1X))
         ELSE
            WRITE(PRBUF,901)(HVAL(K),K=1,MINFO)
  901       FORMAT(' ','IDENTIFIER',2X,1X,(3X,A3,1X),1X,5(3X,A3,1X))
         ENDIF
         CALL WRUSER(PRBUF,ISTAT)
      ENDIF
C
C PUT THEM IN THE FILE
C
      IF (KPOSN.EQ.0) THEN
         WRITE (LU,902)(HVAL(K),K=1,NINFO)
  902    FORMAT(' ','IDENTIFIER',2X,15(3X,A3,1X))
      ELSE
         WRITE(LU,907)(HVAL(K),K=1,NINFO)
  907    FORMAT(' ','IDENTIFIER',2X,1X,(3X,A3,1X),1X,14(3X,A3,1X))
      ENDIF
C
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
C TYPE AND STORE A LINE OF INFORMATION
C
      DO K = 1,NINFO
         IF (K.LE.2.AND.KPOSN.EQ.1) THEN
            IF(ABS(DATA(K)).GT.9999.0) DATA(K) = SIGN(9999.0,DATA(K))
         ELSE
            IF(ABS(DATA(K)).GT.99.0) DATA(K) = SIGN(99.0,DATA(K))
         ENDIF
      ENDDO
      IF (KTYPE.EQ.1) THEN
         IF (KPOSN.EQ.1) THEN
            WRITE(PRBUF,910)(ID(K:K),K=1,10),(DATA(K),K=1,MINFO)
  910       FORMAT(' ',10A1,2X,2F8.1,4F7.2)
         ELSE
            WRITE(PRBUF,911)(ID(K:K),K=1,10),(DATA(K),K=1,MINFO)
  911        FORMAT(' ',10A1,2X,6F7.2)
         ENDIF
         CALL WRUSER(PRBUF,ISTAT)
      ENDIF
C
      IF (KPOSN.EQ.1) THEN
         WRITE(LU,912)(ID(K:K),K=1,10),(DATA(K),K=1,NINFO)
  912    FORMAT(' ',10A1,2X,2F8.1,13F7.2)
      ELSE
         WRITE(LU,913)(ID(K:K),K=1,10),(DATA(K),K=1,NINFO)
  913    FORMAT(' ',10A1,2X,15F7.2)
      ENDIF
C
C
      ENDDO
C
C ---------------------------------------------------
C
C FINALLY TYPE AND STORE THE NUMBER OF ENTRIES
C
      CALL WRUSER(' ',ISTAT)
      IF (LSTLEN.NE.1) THEN
         WRITE(PRBUF,920)LSTLEN
  920    FORMAT(T22,I7,' LIST ENTRIES')
         CALL WRUSER(PRBUF,ISTAT)
         WRITE (LU,921) LSTLEN
  921    FORMAT (' ',//22X,I6,' LIST ENTRIES')
      ELSE
         CALL WRUSER('                            1 LIST ENTRY',ISTAT)
         WRITE (LU,922)
  922    FORMAT (' ',//22X,'   ONE LIST ENTRY')
      ENDIF
C
C
C
      END



