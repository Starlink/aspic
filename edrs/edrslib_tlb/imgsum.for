      subroutine imgsum(ia,npix,nlines,inval,sum,npt)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO SUM ALL THE VALID PIXELS IN AN IMAGE
*
*METHOD
*       STRAIGHTFORWARD ADDITION
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX,NLINES)
*               INPUT IMAGE
*       NPIX,NLINES (IN)
*       INTEGER
*               DIMENSIONS OF IMAGE
*       INVAL (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IA
*       SUM (OUT)
*       REAL
*               SUM OF VALID PIXELS
*       NPT (OUT)
*       INTEGER
*               NUMBER OF VALID PIXELS USED
*
*CALLS
*       NONE
*
*NOTES
*       USES INTEGER*2 ARRAYS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      integer*2 ia(npix,nlines)
 
*
* INITIALLISE SUM OF PIXELS AND VALID PIXEL COUNT
*
      sum=0.0
      npt=0
 
*
* SCAN IMAGE
*
 
      do 2 j=1,nlines
 
         do 1 i=1,npix
 
*
* IF PIXEL IS VALID, ADD TO SUMS
*
 
            if(ia(i,j).ne.inval) then
               sum=sum+ia(i,j)
               npt=npt+1
            endif
 
1        continue
 
2     continue
 
      return
 
      end
 
 
 
