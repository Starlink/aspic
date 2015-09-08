C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C
C
C                     ********************* 
C                     *                   *
C                     * Program   XYLISTA * 
C                     *                   *
C                     ********************* 
C
C
C
C          CALLING SEQUENCE:- 
C               XYLISTA 
C
C
C          FUNCTION:- 
C               It reads any XY file in the EDRS format and types out the
C               identifiers, XY coords and the first of the any parameters
C               and also makes a .LIS file containing all the data. The
C               .LIS file can then be output on any printing device.
C
C
C          USE:- 
C               It is complementary to XYLIST and XYLISP, which  are  parts 
C               of the EDRS package. It differs from them in printing up to 
C               200 numbers per record.
C
C
C
C         USER PARAMETERS:- 
C
C         INPUT                               This is the name of the  .BDF 
C                                             file which you want to list.
C
C       OUTPUT              XYLISTA.LIS        This is the formatted file to 
C                                             which    the    results   are 
C                                             written.  The  user  supplies 
C                                             the  name  only;  the program 
C                                             adds .LIS. 
C                                             XYLISTA is the default
C                                             name.
C
C
C         K F Hartley + A J Penny  RGO                            21-JUN-82 
C
C
C-------------------------------------------------------------------------- 



*  PURPOSE
*        To type out and make a .LIS file of an EDRS XY file, which has
*        any number of values per entry. The first three values are output
*        in the typing, but all are output in the .LIS file. The .LIS
*        file can then be output as normal for .LIS files.
*
*  METHOD
*        Open the EDRS file, open an output .LIS file, get the EDRS
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
         NAME = 'XYLISTA'
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
C PRINT TITLE
C
         CALL WRUSER(' ',ISTAT)
         CALL WRUSER(' ',ISTAT)
         CALL WRUSER('                      TITLE: '//TITLE,ISTAT)
         CALL WRUSER(' ',ISTAT)
         WRITE (LU,900) TITLE
  900    FORMAT ( /// ,27X,'TITLE: ',A,//)
C
C CALL PRALL TO TYPE OUT AND PRNT THE CONTENTS OF THE LIST
C
         CALL PRALL(%VAL(IPIN),NITEM,LSTLEN,LU)
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
*  EDRS FORMAT XY LIST
*
* METHOD
*  TYPE TITLE AND HEADINGS, TYPE TABLE OF IDENTIFIERS, X,Y
*  POSITIONS AND THE OTHER NUMBERS IF PRESENT. TYPE TOTAL NUMBER OF
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



      SUBROUTINE PRALL(LIST,NITEM,LSTLEN,LU)
C
C
C
      BYTE ASC(20)
      CHARACTER ID*20,TITLE(3)*12,ULINE(3)*12,PRBUF*80
      DATA TITLE/' X COORD.     ',' Y COORD.    ','MAGNITUDE   '/
      DATA ULINE/' --------     ',' --------    ','---------  '/
C
C SET MAX NUMBER OF ITEMS TO BE PRINTED
C
      PARAMETER (MAXITM=200)
      INTEGER*4 LIST(NITEM,LSTLEN),BUF(MAXITM)
      REAL INFO(MAXITM-5)
C
C SET EQUIVALENCE SO THAT FIRST 20 BYTES ARE ACCESSED AS BYTE
C DATA AND SUBSEQUENT DATA IS REAL*4
C
      EQUIVALENCE (BUF(1),ASC(1)),(BUF(6),INFO(1))
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
      WRITE(PRBUF,900)(TITLE(L),L=1,3)
  900 FORMAT('     IDENTIFIER',T25,3A14)
      CALL WRUSER(PRBUF,ISTAT)
      WRITE(PRBUF,901)(ULINE(L),L=1,3)
  901 FORMAT('     ----------',T25,3A14)
      CALL WRUSER(PRBUF,ISTAT)
C
C PUT THEM IN THE FILE
C
      WRITE (LU,902)
  902 FORMAT(' ',//,' ','    IDENTIFIER        ',5X,'X COORD',7X,
     +       'Y COORD',5X,'MAGNITUDE ',/)
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
         KINFO = MIN(3,NINFO)
         WRITE(PRBUF,910)ID,(INFO(L),L=1,KINFO)
  910    FORMAT(' ',A20,T27,3(SS,G13.6,:,1X))
         CALL WRUSER(PRBUF,ISTAT)
         WRITE (LU,911)ID,(INFO(L),L=1,NINFO)
  911    FORMAT(' ',A20,T27,6(SS,G13.6,:,1X))
C
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



