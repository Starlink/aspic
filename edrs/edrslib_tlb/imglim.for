      subroutine imglim(ia,npix,nlin,inval,lim,ierr)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIND THE MAXIMUM AND MINIMUM INTEGER IN THE INPUT IMAGE
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX,NLIN)
*              THE INPUT IMAGE
*       NPIX,NLIN (IN)
*       INTEGER
*              THE DIMENSIONS OF IA
*       INVAL (IN)
*       INTEGER
*              THE FLAG FOR INVALID PIXELS
*       LIM (OUT)
*       INTEGER(2)
*              THE ARRAY TO HOLD THE MAXIMUM AND MINIMUM INTEGERS
*       IERR (OUT)
*       INTEGER
*              THE ERROR FLAG
*
*CALLS
*       NONE
*
*NOTES
*       USES INTEGER*2 ARRAYS
*
*WRITTEN BY
*       D.S. BERRY
*----------------------------------------------------------------------
*
*
      integer lim(2)
      integer*2 ia(npix,nlin)
 
*
* INITIALISE ERROR FLAG AND GOOD PIXEL INDICATOR
*
      ierr=0
      ngood=0
 
*
* INITIALISE LIMITS
*
      lim(1)=32767
      lim(2)=-32767
 
*
* SCAN THROUGH INPUT IMAGE TO FIND MAX AND MIN VALUES,IGNORING INVALID
* PIXELS
*
 
      do 20 j=1,nlin
 
         do 10 i=1,npix
            in=ia(i,j)
 
            if(in.ne.inval) then
               lim(1)=jmin0(lim(1),in)
               lim(2)=jmax0(lim(2),in)
               ngood=1
            endif
 
10       continue
 
20    continue
 
 
*
* IF INPUT CONTAINED NO VALID PIXELS SET IERR=1
*
 
      if(ngood.eq.0) ierr=1
      return
 
      end
 
 
 
