C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *********************
C                     *                   *
C                     * Program   XYPRNT  *
C                     *                   *
C                     *********************
C
C
C
C          CALLING SEQUENCE:-
C               XYPRNT
C
C
C          FUNCTION:-
C
C               It makes a neat listing of an XYlist file into a .LIS file
C               and can type it out as it goes. The .LIS file can be
C               output on any printing device.
C
C               The Identifiers can be listed or not. If they are, you
C               can choose how many characters are output, although it is
C               defaulted to 12.
C
C               The values are typed out as real numbers of the form
C               000.00, where you define the number of characters before
C               and after the decimal point for each parameter. A number
C               that is too large is printed as (eg) 999.99 or -99.99
C               If there are no characters after the decimal point it is
C               supressed.
C
C               Up to 15 parameters can be listed, and up to 6 typed out
C               as it goes.
C
C               A space is inserted between the parameter columns, but
C               this may be filled if the number is one character too
C               big to fit in the apportioned space.
C
C
C
C
C         USER PARAMETERS:-
C
C         INPUT                               This is the name of the  .BDF
C                                             file which you want to list.
C
C         OUTPUT            XYPRNT.LIS        This is the formatted file to
C                                             which    the    results   are
C                                             written.  The  user  supplies
C                                             the  name  only;  the program
C                                             adds .LIS.
C                                             XYPRNT is the default
C                                             name.
C
C        TYPING           NO                  Flag for typing the summary
C                                             as the program goes. Choices
C                                             are YES,NO
C        HEADER                               Text to be written at the
C                                             top of each column. Each
C                                             can be up to 30 characters.
C
C        NUMBEF           4                   Number of characters before
C                                             decimal point wanted for the
C                                             parameter
C
C        NUMAFT           2                   Number of characters after
C                                             the decimal point wanted for
C                                             the paramter
C
C
C    NORMALLY DEFAULTED PARAMETERS
C
C
C        NUMCHAR          12                  The number of characters of
C                                             the Identifiers to be output.
C                                             There are 20 characters in all.
C                                             If you give less than 1, the
C                                             Identifiers are not listed.
C
C
C
C
C
C
C         A J Penny            RGO                                 83-1-19
C
C
C--------------------------------------------------------------------------



*  PURPOSE
*        To type and make a .LIS file of an XYlist .BDF file
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
*     TYPING
*            The typing flag
*     NUMCHAR
*            The number of characters in the Identifer to be output
*     HEADER
*            The text above each column
*     NUMBEF
*            The number of charaecters before the decimal point in a col
*     NUMAFT
*            The number of charaecters after the decimal point in a col
*
*  CALLS
*      This file
*            PRALL
*      EDRS Package
*            GETCMD,GETPAR,GTXYLR,GTDSCR
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
      CHARACTER CVAL*1,TITLE*30,NAME*9,FILE*13,HVAL(15)*30,HVALA*30
      CHARACTER TEXTH*7
      CHARACTER*72 TEXT
      INTEGER NUMBEF(15),NUMAFT(15)
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
         NAME = 'XYPRNT'
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
         CALL GETCMD('TYPING','NO,YES.',1,K,TEXT,NVAL,IERR)
         IF (K.EQ.2) THEN
            KTYPE = 1
         ELSE
            KTYPE = 0
         ENDIF
C
C  Get number of characters of Identifier to output
C
         NUMID = 12
         CALL GETPAR('NUMCHAR','INTEGER',1,-1.0E6,20.0,.TRUE.,
     +               NUMID,RVAL,IERR)
         IF (NUMID.LT.0) NUMID = 0
C
C  Get headings and formats
C
         NINFO = MIN(20,NITEM) - 5
         CALL WRUSER(' ',ISTAT)
         CALL WRUSER('INPUT THE HEADERS AND FORMATS',ISTAT)
         DO K = 1,NINFO
            CALL WRUSER(' ',ISTAT)
            WRITE(TEXT,910)K
  910       FORMAT(' ','PARAMETER NO = ',I3)
            CALL WRUSER(TEXT,ISTAT)
            HVALA = ' '
            WRITE(TEXTH,911)K
  911       FORMAT('HEAD',I3.3)
            CALL GTDSCR('INPUT',TEXTH,'CHARACTER',IVAL,RVAL,HVALA,
     +                  IERR)
            CALL RDKEYC('HEADER',.TRUE.,1,HVALA,NVAL,IERR)
            CALL LBGONE(HVALA)
            HVAL(K) = HVALA
            IVAL = 4
            CALL GETPAR('NUMBEF','INTEGER',1,1.0,40.0,.TRUE.,
     +                  IVAL,RVAL,IERR)
            CALL CNPAR('NUMBEF',ISTAT)
            NUMBEF(K) = IVAL
            IVAL = 2
            CALL GETPAR('NUMAFT','INTEGER',1,0.0,40.0,.TRUE.,
     +                  IVAL,RVAL,IERR)
            CALL CNPAR('NUMAFT',ISTAT)
            NUMAFT(K) = IVAL
            CALL CNPAR('HEADER',ISTAT)
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
         CALL PRALL(%VAL(IPIN),NITEM,LSTLEN,LU,KTYPE,HVAL,
     +              NUMID,NUMBEF,NUMAFT)
C
C
C
      ENDIF
C
C  Free data area and close output file
C
      CALL FRDATA(' ',ISTAT)
      CLOSE (UNIT=LU)
C
C
C
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
*  XYLIST .BDF FILE
*
* METHOD
*  TYPE TITLE AND HEADINGS, TYPE TABLE OF IDENTIFIERS, VALUES
*  TYPE TOTAL NUMBER OF
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
*  KTYPE
*     Flag for Typing as well as making List file (=1 for yes)
*  HVAL  CHARACTER*30 (15)   (IN)
*     Headers for columns
*  NUMID    Integer       (IN)
*     Number of characters in Identifers to be output
*  NUMBEF   Integer (15)  (IN)
*     Number of characters before decimal points
*  NUMAFT   Integer (15)  (IN)
*     Number of characters after decimal points
*
* CALLS
*  This file
*    LNTIDY
*  Grasp
*    CHARLN
*  Starlink
*    WRUSER
*
* NOTES
*  USES BYTE ARRAYS
*
* WRITTEN BY
*   A J PENNY               RGO                         83-1-19
* ----------------------------------------------------------------------



      SUBROUTINE PRALL(LIST,NITEM,LSTLEN,LU,KTYPE,HVAL,
     +                 NUMID,NUMBEF,NUMAFT)
C
C
C
      BYTE ASC(20)
      CHARACTER ID*20,TEXT*72,HVAL(15)*30
      CHARACTER*200 TEXTA,TEXTB
      CHARACTER*7 FORCHA(16),FORCHB(16),FORIN
      CHARACTER*13 FORCHC(18),FORCHD(18),FORINA,FORINB
      INTEGER NUMBEF(15),NUMAFT(15),KCC(15),KCD(15)
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
C ---------------------------------------------------------
C
C
C   NINFO is the no of parameters to be listed
C   MINFO is the no of parameters to be typed
C
      NINFO=MIN(20,NITEM)-5
      MINFO = MIN(6,NINFO)
C
C  Load Format statements
C
      FORCHA(1) = '(20A1,'
      DO K = 1,MINFO
         KA = NUMBEF(K) + NUMAFT(K) + 2
         WRITE(FORIN,973)KA,NUMAFT(K)
  973    FORMAT('F',I2,'.',I2,',')
         FORCHA(K+1) = FORIN
      ENDDO
      FORCHA(MINFO+1)(7:7) = ')'
C
      FORCHB(1) = '(20A1,'
      DO K = 1,NINFO
         KA = NUMBEF(K) + NUMAFT(K) + 2
         WRITE(FORIN,972)KA,NUMAFT(K)
  972    FORMAT('F',I2,'.',I2,',')
         FORCHB(K+1) = FORIN
      ENDDO
      FORCHB(NINFO+1)(7:7) = ')'
C
C  Load Header Formats
C
      FORCHC(1) = '(5HIDENT,'
      FORCHC(2) = '8HIFIER   ,'
      IF (NUMID.LT.12) THEN
         FORCHC(1) = '(5HNAME ,'
         FORCHC(2) = '8H        ,'
      ENDIF
      FORCHC(3) = '7H       ,'
      DO K = 1,MINFO
         KC = NUMBEF(K) + NUMAFT(K) + 1
         IF (NUMAFT(K).EQ.0) KC = KC -1
         CALL CHARLN(HVAL(K),KLEN)
         IF (KLEN.LT.1) KLEN = 1
         IF (KC.GT.KLEN) KC = KLEN
         KCC(K) = KC
         KB = NUMBEF(K) - KC + 1
         IF (KB.LT.1) KB = 1
         KE = NUMBEF(K) + NUMAFT(K) + 2 - KB - KC
         IF (NUMAFT(K).EQ.0) KE = KE - 1
         IF (KE.GT.0) THEN
            WRITE(FORINA,971)KB,KC,KE
  971       FORMAT(I2,'X,',I2,'A1,',I2,'X,')
         ELSE
            WRITE(FORINA,978)KB,KC
  978       FORMAT(I2,'X,',I2,'A1,')
         ENDIF
         FORCHC(K+3) = FORINA
      ENDDO
      KA = MINFO + 3
      FORCHC(KA)(13:13) = ')'
C
      FORCHD(1) = '(5HIDENT,'
      FORCHD(2) = '8HIFIER   ,'
      IF (NUMID.LT.12) THEN
         FORCHD(1) = '(5HNAME ,'
         FORCHD(2) = '8H        ,'
      ENDIF
      FORCHD(3) = '7H       ,'
      DO K = 1,NINFO
         KC = NUMBEF(K) + NUMAFT(K) + 1
         IF (NUMAFT(K).EQ.0) KC = KC - 1
         CALL CHARLN(HVAL(K),KLEN)
         IF (KLEN.LT.1) KLEN = 1
         IF (KC.GT.KLEN) KC = KLEN
         KCD(K) = KC
         KB = NUMBEF(K) - KC + 1
         IF (KB.LT.1) KB = 1
         KE = NUMBEF(K) + NUMAFT(K) + 2 - KB - KC
         IF (NUMAFT(K).EQ.0) KE = KE - 1
         IF (KE.GT.0) THEN
            WRITE(FORINA,970)KB,KC,KE
  970       FORMAT(I2,'X,',I2,'A1,',I2,'X,')
         ELSE
            WRITE(FORINA,976)KB,KC
  976       FORMAT(I2,'X,',I2,'A1,')
         ENDIF
         FORCHD(K+3) = FORINA
      ENDDO
      KA = NINFO + 3
      FORCHD(KA)(13:13) = ')'
C
C  Write Headers
C
      IF (KTYPE.EQ.1) THEN
         CALL WRUSER(' ',ISTAT)
         WRITE(TEXTA,FORCHC)((HVAL(K)(L:L),L=1,KCC(K)),K=1,MINFO)
         CALL LNTIDY(TEXTA,200,NUMID,NUMAFT,0,TEXTB,KLEN)
         WRITE(FORINB,974)KLEN
C 975    FORMAT('(',I3.3,'A1)')
         WRITE(TEXT,FORINB)(TEXTB(JJ:JJ),JJ=1,KLEN)
         CALL WRUSER(TEXT,ISTAT)
      ENDIF
      WRITE (TEXTA,FORCHD)((HVAL(K)(L:L),L=1,KCD(K)),K=1,NINFO)
      CALL LNTIDY(TEXTA,200,NUMID,NUMAFT,0,TEXTB,KLEN)
      WRITE(FORINB,974)KLEN
  974 FORMAT('(1H ,',I3.3,'A1)')
      WRITE(LU,FORINB)(TEXTB(JJ:JJ),JJ=1,KLEN)
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
C  Check for numbers too big to fit in.
C
         DO I = 1,NINFO
            ALIM = 10.0**(REAL(NUMBEF(I)+1))
            AP = REAL(NUMAFT(I))
            IF (DATA(I).GT.ALIM) THEN
               DATA(I) = ALIM - (0.8/(10.0**AP))
            ENDIF
            ALIM = -1.0*ALIM/10.0
            IF (DATA(I).LE.ALIM) THEN
               DATA(I) = ALIM + (1.2/(10.0**AP))
            ENDIF
         ENDDO
C
C TYPE AND STORE A LINE OF INFORMATION
C
         IF (KTYPE.EQ.1) THEN
            WRITE(TEXTA,FORCHA)(ID(K:K),K=1,20),(DATA(K),K=1,MINFO)
            CALL LNTIDY(TEXTA,200,NUMID,NUMAFT,1,TEXTB,KLEN)
            WRITE(FORINB,974)KLEN
            WRITE(TEXT,FORINB)(TEXTB(K:K),K=1,KLEN)
            CALL WRUSER(TEXT,ISTAT)
         ENDIF
C
         WRITE(TEXTA,FORCHB)(ID(K:K),K=1,20),(DATA(K),K=1,NINFO)
         CALL LNTIDY(TEXTA,200,NUMID,NUMAFT,1,TEXTB,KLEN)
         WRITE(FORINB,974)KLEN
         WRITE(LU,FORINB)(TEXTB(K:K),K=1,KLEN)
C
C
      ENDDO
C
C ---------------------------------------------------
C
C  Type the number of entries
C
      CALL WRUSER(' ',ISTAT)
      IF (LSTLEN.NE.1) THEN
         WRITE(TEXT,920)LSTLEN
  920    FORMAT(T22,I7,' LIST ENTRIES')
         CALL WRUSER(TEXT,ISTAT)
      ELSE
         CALL WRUSER('                            1 LIST ENTRY',ISTAT)
      ENDIF
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R LNTIDY *
C      *            *
C      **************
C
C
C   PURPOSE
C    This removes the unwanted Identifier characters and if flagged,
C    removes any unwanted decimal points (which are known by having
C    no numbers after them.
C
C
C   ARGUMENTS
C  IN
C     TEXTA       Character*N  The input text
C     N           Integer      The length of the text
C     NUMID       Integer      The number of characters wanted in Identifier
C     NUMAFT(15)  Integer      The no of chars after each decimal point
C     KW          Integer      Flag for removing decimal points (yes=1;no=0)
C  OUT
C     TEXTB       Character*N  The output text
C     KLEN        Integer      No of chars in Output up to last non-space one
C
C   CALLS
C     Grasp
C       CHARLN
C
C
C
C   A.J.PENNY                   RGO                    83-2-26
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE LNTIDY(TEXTA,N,NUMID,NUMAFT,KW,TEXTB,KLEN)
C
C
C
      CHARACTER*(*) TEXTA,TEXTB
      INTEGER NUMAFT(15),KPOS(15)
C
C
C
      DO J = 1,N
         TEXTB(J:J) = TEXTA(J:J)
      ENDDO
C
C  Strip redundant decimals
C
      IF (KW.EQ.1) THEN
         L = 0
         DO K = 21,N
            IF (L.LT.15.AND.TEXTB(K:K).EQ.'.') THEN
               L = L + 1
               KPOS(L) = K
            ENDIF
         ENDDO
         IF (L.NE.0) THEN
            DO K = L,1,-1
               IF (NUMAFT(K).EQ.0) THEN
                  DO J = KPOS(K),N-1
                     JA = J + 1
                     TEXTB(J:J) = TEXTB(JA:JA)
                  ENDDO
               ENDIF
            ENDDO
         ENDIF
      ENDIF
C
C  Strip unwanted Identifier characters
C
      IF (NUMID.NE.20) THEN
         KK = 20 - NUMID
         DO J = NUMID+1,N-KK
            JA = J + KK
            TEXTB(J:J) = TEXTB(JA:JA)
         ENDDO
         DO J = N-KK+1,N
            TEXTB(J:J) = ' '
         ENDDO
      ENDIF
C
C  Find length
C
      CALL CHARLN(TEXTB,KLEN)
C
C
C
      END



