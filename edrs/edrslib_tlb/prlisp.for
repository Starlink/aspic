      subroutine prlisp(list,nitem,lstlen)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO PRINT THE CONTENTS OF AN X,Y LIST DATASET
*
*METHOD
*       PRINT TITLE AND HEADINGS, PRINT TABLE OF IDENTIFIERS, X,Y
*       POSITIONS AND MAGNITUDES IF PRESENT. PRINT TOTAL NUMBER OF
*       ENTRIES IN LIST.
*
*ARGUMENTS
*       LIST (IN)
*       INTEGER(NITEM,LSTLEN)
*               THE INPUT X,Y LIST
*       NITEM,LSTLEN (IN)
*       INTEGER
*               DIMENSIONS OF LIST
*
*CALLS
*       STARLINK:
*               WRUSER
*
*NOTES
*       USES BYTE ARRAYS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
*
* SET MAX NUMBER OF ITEMS TO BE PRINTED
*
      parameter (maxitm=8)
      integer*4 list(nitem,lstlen),buf(maxitm)
      byte asc(20)
      real info(maxitm-5)
 
*
* SET EQUIVALENCE SO THAT FIRST 20 BYTES ARE ACCESSED AS BYTE
* DATA AND SUBSEQUENT DATA IS REAL*4
*
      equivalence (buf(1),asc(1)),(buf(6),info(1))
      character id*20,title(maxitm-5)*14,uline(maxitm-5)*14,prbuf*80
      data title/' X COORD.     ',' Y COORD.     ',' MAGNITUDE    '/
      data uline/' --------     ',' --------     ',' ---------    '/
 
*
* NINFO IS THE NUMBER OF NON-CHARACTER ITEMS TO BE PRINTED
*
      ninfo=min(maxitm,nitem)-5
 
*
* PRINT TITLES AND UNDERLINING
*
      write(prbuf,10)(title(l),l=1,ninfo)
10    format('       IDENTIFIER',t27,20a14)
      write(2,'(A)')prbuf
      write(prbuf,11)(uline(l),l=1,ninfo)
11    format('       ----------',t27,20a14)
      write(2,'(A)')prbuf
 
*
* SCAN THROUGH LIST ENTRIES
*
 
      do 98 j=1,lstlen
 
*
* COPY ENTRY INTO BUFFER TO ALLOW DIFFERENT DATA TYPES
*
 
         do 97 i=1,min(nitem,maxitm)
            buf(i)=list(i,j)
97       continue
 
 
*
* CONVERT IDENTIFIER BYTES TO CHARACTERS
*
 
         do 96 i=1,20
            nchar=asc(i)
            id(i:i)=char(nchar)
96       continue
 
 
*
* PRINT A LINE OF INFORMATION
*
         write(prbuf,12)id,(info(l),l=1,ninfo)
12       format(' ',a20,t27,20(ss,g13.6,:,1x))
         write(2,'(A)')prbuf
98    continue
 
 
*
* FINALLY PRINT THE NUMBER OF ENTRIES
*
      write(2,*)
 
      if(lstlen.ne.1) then
         write(prbuf,13)lstlen
13       format(t22,i7,' LIST ENTRIES')
         write(2,'(A)')prbuf
 
      else
         write(2,6666)
6666     format('                              1 LIST ENTRY')
      endif
 
      return
 
      end
 
 
 
