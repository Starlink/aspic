      subroutine xyprgg(x,y,id,n,nsave,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO REMOVE DUPLICATE ENTRIES FROM A LIST OF X,Y POSITIONS AND
*       IDENTIFIERS
*
*METHOD
*       COMPARE EACH LIST ENTRY WITH ALL SUBSEQUENT ENTRIES. IF A
*       MATCHING IDENTIFIER IS FOUND, COPY THE MOST RECENT VERSION TO
*       THE OLD LOCATION AND SHIFT SUBSEQUENT ENTRIES DOWN TO FILL
*       THE GAP.
*
*ARGUMENTS
*       X,Y (IN/OUT)
*       REAL(N)
*               THE LISTS OF X,Y POSITIONS
*       ID (IN/OUT)
*       BYTE(20,N)
*               THE LIST OF ASCII IDENTIFIERS
*       N (IN)
*       INTEGER
*               THE NUMBER OF INPUT LIST ENTRIES
*       NSAVE (OUT)
*       INTEGER
*               THE NUMBER OF OUTPUT LIST ENTRIES
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
      real x(n),y(n),xtemp,ytemp
      byte id(20,n),idtemp(20)
      ierr=0
      nsave=n
 
*
* I COUNTS THE NUMBER OF DIFFERENT IDENTIFIERS
*
      i=1
 
*
* CONTINUE IF THERE ARE MORE LIST ENTRIES REMAINING THAN DIFFERENT
* IDENTIFIERS FOUND
*
 
66    if(i.lt.nsave) then
 
*
* SEARCH ALL SUBSEQUENT IDENTIFIERS
*
         ns=nsave
 
         do 18 j=i+1,ns
 
*
* IF TWO IDENTIFIERS ARE THE SAME, REPLACE THE FIRST ENTRY WITH THE
* SECOND
*
 
            if(iabord(id(1,i),id(1,j),20).eq.0) then
               call bytcpy(id(1,j),id(1,i),20)
               x(i)=x(j)
               y(i)=y(j)
 
*
* MOVE REMAINING ENTRIES DOWN TO FILL THE GAP
*
 
               do 17 k=j,ns-1
                  call bytcpy(id(1,k+1),id(1,k),20)
                  x(k)=x(k+1)
                  y(k)=y(k+1)
17             continue
 
 
*
* IF AN ENTRY HAS BEEN ELIMINATED, REDUCE NSAVE BY 1
*
               nsave=nsave-1
               go to 66
 
            endif
 
18       continue
 
 
*
* IF NO DUPLICATE WAS FOUND, TEST THE NEXT ENTRY
*
         i=i+1
         go to 66
 
      endif
 
 
      end
 
 
 
