      subroutine maxmin(x,y,len,idir,amax,amin,ierr)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIND THE EXTREMAL VALUES OF AN XY LIST IN EITHER X OR Y
*
*METHOD
*       TREAT X AND Y SEPERATLY
*
*ARGUMENTS
*       X (IN)
*       REAL
*               THE INPUT X LIST
*       Y (IN)
*       REAL
*               THE INPUT Y LIST
*       LEN (IN)
*       INTEGER
*               THE NUMBER OF ENTRIES IN THE X AND Y LISTS
*       IDIR (IN)
*       INTEGER
*               IF IDIR=1 THEN THE EXTREMAL Y VALUES ARE FOUND,IF
*               IDIR=2 THEN THE EXTREMAL X VALUES ARE FOUND.
*       AMAX (OUT)
*       REAL
*               THE MAXIMUM EXTREMAL VALUE
*       AMIN (OUT)
*       REAL
*               THE MINIMUM EXTREMAL VALUE
*       IERR (OUT)
*       INTEGER
*               ERROR RETURN FLAG,ZERO FOR SUCCESS
*
*CALLS
*       NONE
*
*WRITTEN BY
*       D.S.BERRY
*----------------------------------------------------------------------
      real x(len),y(len)
 
*
* SET ERROR RETURN FLAG
*
      ierr=0
 
*
* IF IDIR=1,FIND EXTREMAL Y VALUES
*
 
      if(idir.eq.1) then
         amax=y(1)
         amin=y(1)
 
         do 10 i=1,len
            amax=amax1(amax,y(i))
            amin=amin1(amin,y(i))
10       continue
 
 
*
* IF IDIR=2,FIND EXREMAL X VALUES
*
 
      else if(idir.eq.2) then
         amax=x(1)
         amin=x(1)
 
         do 20 i=1,len
            amax=amax1(amax,x(i))
            amin=amin1(amin,x(i))
20       continue
 
 
*
* IF IDIR IS NIETHER 1 OR 2,STOP
*
 
      else
         ierr=1
 
*
* RETURN
*
      endif
 
      return
 
      end
 
 
 
