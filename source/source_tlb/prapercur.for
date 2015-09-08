C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ***********************
C                     *                     *
C                     * Program   PRAPERCUR *
C                     *                     *
C                     ***********************
C
C
C
C          CALLING SEQUENCE:-
C               PRAPERCUR
C
C
C          FUNCTION:-
C               It reads the output of APERCUR and types out a convenient
C               summary and makes a .LIS file. The.
C               .LIS file can then be output on any printing device.
C                 The typing out is optional
C
C
C          USE:-
C
C
C
C         USER PARAMETERS:-
C
C         INPUT                               This is the name of the  .BDF
C                                             file which you want to list.
C
C       OUTPUT           PRAPERCUR.LIS        This is the formatted file to
C                                             which    the    results   are
C                                             written.  The  user  supplies
C                                             the  name  only;  the program
C                                             adds .LIS.
C                                             PRAPERCUR is the default
C                                             name.
C
C        TYPING           NO                  Flag for typing the summary
C                                             as the program goes. Choices
C                                             are YES,NO
C
C
C
C         A J Penny            RGO                                17-DEC-82
C
C
C--------------------------------------------------------------------------



*  PURPOSE
*        To type and make a .LIS file of the APERCUR output file.
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
*       A.J. PENNY                                      82-12-17
* ------------------------------------------------------------



      PROGRAM PRAPERCUR
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
         NAME = 'PRAPERCUR'
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
         CALL PRALL(%VAL(IPIN),NITEM,LSTLEN,LU,KTYPE)
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
*  FILE PRODUCED BY 'APERCUR'
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
*  R.F. WARREN-SMITH AND A J PENNY                      82-12-21
* ----------------------------------------------------------------------



      SUBROUTINE PRALL(LIST,NITEM,LSTLEN,LU,KTYPE)
C
C
C
      BYTE ASC(20)
      CHARACTER ID*20,PRBUF*72
C
C SET MAX NUMBER OF ITEMS TO BE PRINTED
C
      PARAMETER (MAXITM=30)
      INTEGER*4 LIST(NITEM,LSTLEN),BUF(MAXITM)
      REAL DATA(MAXITM-5)
      INTEGER NSOUT(7)
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
      NINFO=MIN(MAXITM,NITEM)-5
C
C TYPE TITLES AND UNDERLINING
C
      IF (KTYPE.EQ.1) THEN
         CALL WRUSER(' ',ISTAT)
         WRITE(PRBUF,900)
  900    FORMAT(' ',' STAR','   MAG  ','      FLUX  ','   X  ',
     +       '   Y  ','  RMS ','INVAL  ','    BASE ','  PIX ')
         CALL WRUSER(PRBUF,ISTAT)
      ENDIF
C
C PUT THEM IN THE FILE
C
      WRITE (LU,902)
  902 FORMAT(' ',//,' ',' STAR',15X,'       X  ','       Y  ',
     +       '   MAG  ','      FLUX ','  RMS ','INVAL','    BASE  ',
     +       '  PIX ','  ZERO  ','  AP ','  SKY X    SKY Y ',
     +       '  SKY AP ',' RMSCOR')
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
      AMAG = DATA(3)
      AH = DATA(9)
      AX = DATA(1)
      AY = DATA(2)
      ADX = DATA(4)
      ADY = DATA(5)
      ARMS = DATA(7)
      NINVAL = INT(DATA(8)+0.01)
      BASE = DATA(10)
      KPIX = INT(DATA(11)+0.01)
      KAPDIA = INT(DATA(12)+0.01)
      XSKY = DATA(13)
      YSKY = DATA(14)
      KSKYAP = INT(DATA(15)+0.01)
      ZERO = DATA(16)
      RMSCOR = DATA(17)
      IF (ABS(AH).GT.99999999.0) AH = SIGN(99999999.0,AH)
      IF (ABS(AX).GT.99999.0) AX = SIGN(99999.0,AX)
      IF (ABS(AY).GT.99999.0) AY = SIGN(99999.0,AY)
      IF (ABS(ADX).GT.999.0) ADX = SIGN(999.0,ADX)
      IF (ABS(ADY).GT.999.0) ADY = SIGN(999.0,ADY)
      IF (ARMS.GT.9999.0) ARMS = 9999.0
      IF (ABS(BASE).GT.9999.0) BASE = SIGN(9999.0,BASE)
      IF (ABS(XSKY).GT.99999.0) XSKY = SIGN(99999.0,XSKY)
      IF (ABS(YSKY).GT.99999.0) YSKY = SIGN(99999.0,YSKY)
      KX = AX
      KY = AY
      KXSKY = XSKY
      KYSKY = YSKY
      IF (KTYPE.EQ.1) THEN
            WRITE (PRBUF,910) J,AMAG,AH,KX,KY,
     +                       ARMS,NINVAL,BASE,KPIX
  910    FORMAT (1H ,I5,F7.2,F11.1,2I6,F7.2,I6,F10.1,I6)
         CALL WRUSER(PRBUF,ISTAT)
      ENDIF
C
         WRITE (LU,911) (ID(I:I),I=1,20),KX,KY,AMAG,AH,
     +                    ARMS,NINVAL,BASE,KPIX,ZERO,KAPDIA,KXSKY,
     +                    KYSKY,KSKYAP,RMSCOR
  911 FORMAT (1H ,20A1,2I10,F7.2,F11.1,F7.2,I5,
     +        F10.1,I5,F8.2,I5,2I7,5X,I5,F10.3)
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



