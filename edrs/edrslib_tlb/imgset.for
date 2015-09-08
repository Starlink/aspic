      subroutine imgset(ia,npix,nlines,ival)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO SET AN IMAGE ARRAY TO A CONSTANT VALUE
*
*METHOD
*       STRAIGHTFORWARD ASSIGNMENT
*
*ARGUMENTS
*       IA (OUT)
*       INTEGER*2(NPIX,NLINES)
*               IMAGE ARRAY
*       NPIX,NLINES (IN)
*       INTEGER
*               DIMENSIONS OF IA
*       IVAL (IN)
*       INTEGER
*               VALUE TO BE PUT IN IA
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
      integer*2 ia(npix,nlines),i2val
 
*
* SCAN IMAGE, SETTING EACH PIXEL TO IVAL
*
      i2val=ival
 
      do 2 j=1,nlines
 
         do 1 i=1,npix
            ia(i,j)=i2val
1        continue
 
2     continue
 
      return
 
      end
 
 
 
