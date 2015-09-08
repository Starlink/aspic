      subroutine imgmsk(ia,ascale,azero,invala,ib,bscale,bzero,invalb
     : ,npix,nlines,cscale,czero,invalc,ic)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*
*METHOD
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX,NLINES)
*               THE FIRST INPUT IMAGE
*       ASCALE,AZERO (IN)
*       REAL
*               SCALE AND ZERO FOR IMAGE IA
*       INVALA (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IA
*       IB (IN)
*       INTEGER*2(NPIX,NLINES)
*               SECOND INPUT IMAGE
*       BSCALE,BZERO (IN)
*       REAL
*               SCALE AND ZERO FOR IB
*       INVALB (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IB
*       NPIX,NLINES (IN)
*       INTEGER
*               DIMENSIONS OF IMAGES
*       CSCALE,CZERO (IN)
*       REAL
*               SCALE AND ZERO FOR OUTPUT IMAGE IC
*       INVALC (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IC
*       IC (OUT)
*       INTEGER*2(NPIX,NLINES)
*               OUTPUT IMAGE
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
      integer*2 ia(npix,nlines),ib(npix,nlines),ic(npix,nlines)
      real minint,maxint
 
*
* SET MINIMUM AND MAXIMUM PERMISSIBLE INTEGER VALUES
*
      parameter (minint=-32767.0,maxint=32767.0)
      rcscal=1.0/sign(max(abs(cscale),1.0e-20),cscale)
 
*
* SCAN IMAGE
*
 
      do 2 j=1,nlines
 
         do 1 i=1,npix
 
            if(ia(i,j).ne.invala)then
               c=ia(i,j)*ascale+azero
               c=(c-czero)*rcscal
 
            else
 
               if(ib(i,j).ne.invalb)then
                  c=ib(i,j)*bscale+bzero
                  c=(c-czero)*rcscal
 
               else
                  c=-100000.0
               endif
 
            endif
 
 
*
* IF ANSWER LIES IN THE VALID RANGE, ADD TO OUTPUT IMAGE
*
 
            if((c.le.maxint).and.(c.ge.minint)) then
               ic(i,j)=nint(c)
 
            else
               ic(i,j)=invalc
            endif
 
1        continue
 
2     continue
 
      return
 
      end
 
 
 
