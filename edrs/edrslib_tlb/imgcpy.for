      subroutine imgcpy(ia,npix,nlines,ib)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO COPY ONE IMAGE ARRAY TO ANOTHER
*
*METHOD
*       STRAIGHTFORWARD ASSIGNMENT
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX,NLINES)
*               INPUT IMAGE ARRAY
*       NPIX,NLINES (IN)
*       INTEGER
*               DIMENSIONS OF IMAGES
*       IB (OUT)
*       INTEGER*2(NPIX,NLINES)
*               OUTPUT IMAGE ARRAY
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
      integer*2 ia(npix,nlines),ib(npix,nlines)
 
*
* SCAN IMAGE, COPYING IA TO IB
*
 
      do 2 j=1,nlines
 
         do 1 i=1,npix
            ib(i,j)=ia(i,j)
1        continue
 
2     continue
 
      return
 
      end
 
 
 
