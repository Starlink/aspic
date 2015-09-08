      subroutine irconv(ia,npix,nlines,inval,scale,zero,rim)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO CONVERT AN INTEGER*2 IMAGE TO A REAL ONE BY INCLUDING THE
*       IMAGE SCALE AND ZERO. INVALID PIXELS ARE SET TO ZERO.
*
*METHOD
*       THE REAL IMAGE IS ASSIGNED THE VALUE IA*SCALE+ZERO AT EACH
*       PIXEL
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX,NLINES)
*               THE INPUT IMAGE
*       NPIX,NLINES (IN)
*       INTEGER
*               THE DIMENSIONS OF IA
*       INVAL (IN)
*       INTEGER
*               THE INVALID PIXEL FLAG FOR IA
*       SCALE,ZERO (IN)
*       REAL
*               THE SCALE FACTOR AND ZERO LEVEL FOR IA
*       RIM (OUT)
*       REAL(NPIX,NLINES)
*               THE OUTPUT REAL IMAGE
*
*CALLS
*       NONE
*
*NOTES
*       USES INTEGER*2 ARRAYS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*----------------------------------------------------------------------
*
*
      integer*2 ia(npix,nlines)
      real rim(npix,nlines)
 
      do 2 j=1,nlines
 
         do 1 i=1,npix
 
            if(ia(i,j).ne.inval)then
               rim(i,j)=ia(i,j)*scale+zero
 
            else
               rim(i,j)=0.0
            endif
 
1        continue
 
2     continue
 
 
      end
 
 
 
