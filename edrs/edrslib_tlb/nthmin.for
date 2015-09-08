      subroutine nthmin(x,nx,n,stak,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIND THE N'TH SMALLEST NUMBER IN A SET OF DATA VALUES
*
*METHOD
*       MAINTAIN A STACK OF THE SMALLEST VALUES. COMPARE EACH DATA
*       POINT WITH THE TOP OF STACK.. IF SMALLER, INSERT IT IN THE
*       STACK AT AN APPROPRIATE LEVEL TO MAINTAIN NON-INCREASING
*       STACK ORDER. TOP OF STACK IS LOST WHEN DATA IS INSERTED.
*
*ARGUMENTS
*       X (IN)
*       REAL(NX)
*               ARRAY OF DATA VALUES
*       NX (IN)
*       INTEGER
*               NUMBER OF DATA POINTS
*       N (IN)
*       INTEGER
*               SPECIFIES THE N IN 'NTH SMALLEST DATA VALUE'
*       STAK (OUT)
*       REAL(N)
*               STACK OF N SMALLEST VALUES.. N'TH SMALLEST IN STAK(1)
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*CALLS
*       NONE
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      real x(nx),stak(n)
      parameter (extrem=1.0e20)
 
*
* CHECK ARGUMENT VALIDITY
*
 
      if(n.gt.nx) then
         ierr=1
 
      else
         ierr=0
 
*
* INITIALLISE THE STACK OF SMALLEST VALUES
*
 
         do 1 i=1,n
            stak(i)=extrem
1        continue
 
 
*
* COMPARE EACH DATA VALUE WITH THE TOP OF STACK
*
 
         do 6 j=1,nx
 
            if(x(j).lt.stak(1)) then
 
*
* IF LESS THAN TOP OF STACK, IT BELONGS IN THE STACK.. SEARCH DOWN
* THE STACK, MOVING CONTENTS UP.
*
 
               do 2 locn=2,n
 
                  if(x(j).lt.stak(locn)) then
                     stak(locn-1)=stak(locn)
 
                  else
 
*
* WHEN CORRECT LEVEL IS FOUND, INSERT DATA IN STACK
*
                     stak(locn-1)=x(j)
                     go to 61
 
                  endif
 
2              continue
 
 
*
* ARRIVE HERE IF DATA POINT BELONGS ON BOTTOM OF STACK
*
               stak(n)=x(j)
61             continue
            endif
 
6        continue
 
      endif
 
      return
 
      end
 
 
 
