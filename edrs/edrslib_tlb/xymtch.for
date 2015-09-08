      subroutine xymtch(xa,ya,ida,na,xb,yb,idb,nb,nmtch,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO SORT 2 LISTS OF X,Y POSITIONS AND IDENTIFIERS SO THAT
*       THE ENTRIES WITH MATCHING IDENTIFIERS OCCUR FIRST IN EACH LIST
*       AND IN THE SAME ORDER
*
*METHOD
*       COMPARE EACH ENTRY IN THE FIRST LIST WITH EACH ENTRY IN THE
*       SECOND. WHEN A MATCH IS FOUND, SWAP THE MATCHING ENTRIES TO THE
*       FRONT OF THE LISTS
*
*ARGUMENTS
*       XA,YA (IN/OUT)
*       REAL(NA)
*               THE FIRST LISTS OF X,Y POSITIONS
*       IDA (IN/OUT)
*       BYTE(20,NA)
*               THE FIRST LIST OF ASCII IDENTIFIERS
*       NA (IN)
*       INTEGER
*               THE NUMBER OF ENTRIES IN THE FIRST LIST
*       XB,YB (IN/OUT)
*       REAL(NB)
*               THE SECOND LIST OF X,Y POSITIONS
*       IDB (IN/OUT)
*       BYTE(20,NB)
*               THE SECOND LIST OF ASCII IDENTIFIERS
*       NB (IN)
*       INTEGER
*               THE NUMBER OF ENTRIES IN THE SECOND LIST
*       NMTCH (OUT)
*       INTEGER
*               THE NUMBER OF MATCHES FOUND
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*CALLS
*       THIS PACKAGE:
*               IABORD,BYTCPY
*
*NOTES
*       USES BYTE ARRAYS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      real xa(na),ya(na),xb(nb),yb(nb),tx,ty
      byte ida(20,na),idb(20,nb),tid(20)
      ierr=0
 
*
* CHECK ARGUMENT VALIDITY
*
 
      if(na.lt.1) then
         ierr=1
 
      else if(nb.lt.1) then
         ierr=2
 
      else
 
*
* COUNT THROUGH THE NUMBER OF POSSIBLE ID MATCHES
*
 
         do 99 nmtch=1,min(na,nb)
 
*
* SCAN THE REMAINING ENTRIES IN LIST A AND COMPARE EACH WITH ALL THE
* REMAINING ENTRIES IN LIST B
*
 
            do 98 loca=nmtch,na
 
               do 97 locb=nmtch,nb
 
*
* IF A MATCH IS FOUND, SWAP THE MATCHED ENTRIES WITH THE ENTRY IN
* POSITION NMATCH IN EACH LIST
*
 
                  if(iabord(ida(1,loca),idb(1,locb),20).eq.0) then
 
                     if(loca.ne.nmtch) then
                        tx=xa(loca)
                        ty=ya(loca)
                        call bytcpy(ida(1,loca),tid(1),20)
                        xa(loca)=xa(nmtch)
                        ya(loca)=ya(nmtch)
                        call bytcpy(ida(1,nmtch),ida(1,loca),20)
                        xa(nmtch)=tx
                        ya(nmtch)=ty
                        call bytcpy(tid(1),ida(1,nmtch),20)
                     endif
 
 
                     if(locb.ne.nmtch) then
                        tx=xb(locb)
                        ty=yb(locb)
                        call bytcpy(idb(1,locb),tid(1),20)
                        xb(locb)=xb(nmtch)
                        yb(locb)=yb(nmtch)
                        call bytcpy(idb(1,nmtch),idb(1,locb),20)
                        xb(nmtch)=tx
                        yb(nmtch)=ty
                        call bytcpy(tid(1),idb(1,nmtch),20)
                     endif
 
 
*
* AFTER A MATCH HAS BEEN FOUND, EXIT LOOP AND GO TO NEXT POSSIBLE
* MATCH
*
                     go to 99
 
                  endif
 
97             continue
 
98          continue
 
 
*
* EMERGE HERE IF NO MATCH WAS FOUND.. SO EXIT
*
            go to 100
 
99       continue
 
 
*
* SET THE NUMBER OF MATCHES ACTUALLY FOUND
*
100      nmtch=nmtch-1
      endif
 
      return
 
      end
 
 
 
