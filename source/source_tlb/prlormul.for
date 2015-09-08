C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *********************
C                     *                   *
C                     * Program   PRLORMUL *
C                     *                   *
C                     *********************
C
C
C
C          CALLING SEQUENCE:-
C               PRLORMUL
C
C
C          FUNCTION:-
C               It reads the output of LORMUL and types out a convenient
C               summary and makes a .LIS file. The.
C               .LIS file can then be output on any printing device.
C                 The typing out is optional
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
C       OUTPUT              PRLORMUL.LIS        This is the formatted file to
C                                             which    the    results   are
C                                             written.  The  user  supplies
C                                             the  name  only;  the program
C                                             adds .LIS.
C                                             PRLORMUL is the default
C                                             name.
C
C        TYPING           NO                  Flag for typing the summary
C                                             as the program goes. Choices
C                                             are YES,NO
C
C
C
C         A J Penny            RGO                                 3-SEP-82
C
C
C--------------------------------------------------------------------------



*  PURPOSE
*        To type and make a .LIS file of the LORMUL output file.
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
      CHARACTER CVAL*1,TITLE*30,NAME*9,FILE*13
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
         NAME = 'PRLORMUL'
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
*  FILE PRODUCED BY 'LORMUL'
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
C  Type Profile and Heading
C
      DO I = 6,10
         BUF(I) = LIST((I+11),1)
      ENDDO
      IF (KTYPE.EQ.1) THEN
         CALL WRUSER(' ',ISTAT)
         WRITE(PRBUF,903)(DATA(I),I=1,5)
  903    FORMAT(' ','PROFILE IS  ',3F10.3,2F10.2)
         CALL WRUSER(PRBUF,ISTAT)
         CALL WRUSER(' ',ISTAT)
         WRITE(PRBUF,900)
  900    FORMAT(' ',' STAR','   MAG  ',' HEIGHT ','  DX  ',
     +       '  DY  ',' RMS ',' ITS ','INVAL','   COMPANION ',
     +          'STARS')
         CALL WRUSER(PRBUF,ISTAT)
      ENDIF
C
C PUT THEM IN THE FILE
C
      WRITE(LU,904)(DATA(I),I=1,5)
  904 FORMAT(' ',//,' ','PROFILE  ',3F10.3,2F10.2)
      WRITE (LU,902)
  902 FORMAT(' ',/,' ',' STAR',15X,'       X  ','       Y  ',
     +       '   MAG  ',' HEIGHT ','  DX  ','  DY  ',' RMS ',' ITS ',
     +       'INVAL',' BASE  ','     COMPANION STARS')
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
      BASE = DATA(10)
      IF (ABS(AMAG).GT.99.0) AMAG = SIGN(99.0,AMAG)
      IF (ABS(AMAG).LT.0.000001) AMAG = 0.0
      IF (ABS(AH).GT.99999.0) AH = SIGN(99999.0,AH)
      IF (ABS(AH).LT.0.000001) AH = 0.0
      IF (ABS(AX).GT.99999.0) AX = SIGN(99999.0,AX)
      IF (ABS(AX).LT.0.000001) AX = 0.0
      IF (ABS(AY).GT.99999.0) AY = SIGN(99999.0,AY)
      IF (ABS(AY).LT.0.000001) AY = 0.0
      IF (ABS(ADX).GT.999.0) ADX = SIGN(999.0,ADX)
      IF (ABS(ADX).LT.0.000001) ADX = 0.0
      IF (ABS(ADY).GT.999.0) ADY = SIGN(999.0,ADY)
      IF (ABS(ADY).LT.0.000001) ADY = 0.0
      IF (ABS(ARMS).GT.9999.0) ARMS = SIGN(9999.0,ARMS)
      IF (ABS(ARMS).LT.0.000001) ARMS = 0.0
      IF (ABS(BASE).GT.9999.0) BASE = SIGN(9999.0,BASE)
      IF (ABS(BASE).LT.0.000001) BASE = 0.0
      TEMP = DATA(6)
      IF (ABS(TEMP).GT.99.0) TEMP = SIGN(99.0,TEMP)
      IF (ABS(TEMP).LT.0.0001) TEMP = 0.0
      ITER = INT(TEMP+0.01)
      TEMP = DATA(8)
      IF (ABS(TEMP).GT.999.0) TEMP = SIGN(999.0,TEMP)
      IF (ABS(TEMP).LT.0.0001) TEMP = 0.0
      NINVAL = INT(TEMP+0.01)
      TEMP = DATA(17)
      IF (ABS(TEMP).GT.9.0) TEMP = SIGN(9.0,TEMP)
      IF (ABS(TEMP).LT.0.0001) TEMP = 0.0
      NSTA = INT(TEMP+0.01)
      DO K = 1,7
         TEMP = DATA(17+K)
         IF (ABS(TEMP).GT.9999.0) TEMP = SIGN(9999.0,TEMP)
         IF (ABS(TEMP).LT.0.0001) TEMP = 0.0
         NSOUT(K) = INT(TEMP+0.01)
      ENDDO
      IF (KTYPE.EQ.1) THEN
         NSTB = NSTA
         IF (NSTB.GE.5) NSTB = 5
         IF (NSTB.EQ.0) THEN
            WRITE (PRBUF,910) J,AMAG,AH,ADX,ADY,
     +                       ARMS,ITER,NINVAL
         ELSE
            WRITE (PRBUF,910) J,AMAG,AH,ADX,ADY,
     +                       ARMS,ITER,NINVAL,
     +                       (NSOUT(K),K=1,NSTB)
         ENDIF
  910    FORMAT (1H ,I5,F7.2,F8.1,2F6.1,F6.1,I3,I5,5I5)
         CALL WRUSER(PRBUF,ISTAT)
      ENDIF
C
      IF (NSTA.EQ.0) THEN
         WRITE (LU,911) (ID(I:I),I=1,20),AX,AY,AMAG,AH,ADX,ADY,
     +                    ARMS,ITER,NINVAL,BASE
      ELSE
         WRITE (LU,911) (ID(I:I),I=1,20),AX,AY,AMAG,AH,ADX,ADY,
     +                    ARMS,ITER,NINVAL,BASE,
     +                    (NSOUT(K),K=1,NSTA)
      ENDIF
  911 FORMAT (1H ,20A1,2F10.2,F7.2,F8.1,2F6.1,F6.1,I3,I5,
     +        F7.1,7I5)
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



