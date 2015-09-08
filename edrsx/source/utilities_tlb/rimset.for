      subroutine rimset(image,npix,nlines,value)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO SET A REAL IMAGE ARRAY TO A CONSTANT VALUE
*
*SOURCE
*       RIMSET.FOR IN UTILITIES.TLB
*
*METHOD
*       STRAIGHTFORWARD ASSIGNMENT
*
*ARGUMENTS
*       IMAGE (OUT)
*       REAL(NPIX,NLINES)
*               IMAGE ARRAY
*       NPIX,NLINES (IN)
*       INTEGER
*               DIMENSIONS OF IMAGE
*       VALUE (IN)
*       REAL
*               VALUE TO BE PUT IN IMAGE
*
*CALLS
*       NONE
*
*WRITTEN BY
*       D.S. Berry (based on IMGSET in EDRS by R.F. Warren-Smith)
*-----------------------------------------------------------------------
*
*
      real image(npix,nlines),value
*
* SCAN IMAGE, SETTING EACH PIXEL TO VALUE
*

      do 2 j=1,nlines

         do 1 i=1,npix
            image(i,j)=value
1        continue

2     continue

      return

      end



