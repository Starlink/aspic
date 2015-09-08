      subroutine imgalg(ia,ascale,azero,invala,ib,bscale,bzero,invalb
     : ,npix,nlines,nfunct,cscale,czero,invalc,ic)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO IMPLEMENT 4 FUNCTION ARITHMETIC BETWEEN 2 IMAGES OF THE
*       SAME SIZE
*
*METHOD
*       PERFORM REAL ARITHMETIC USING THE FUNCTION REQUIRED, THEN
*       CONVERT THE RESULT TO INTEGERS
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
*       NFUNCT (IN)
*       INTEGER
*               SPECIFIES WHICH FUNCTION IS REQUIRED:+,-,* OR /.
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
 
*
* IF BOTH INPUT PIXELS ARE VALID, CONTINUE WITH ARITHMETIC
*
 
            if((ia(i,j).ne.invala).and.(ib(i,j).ne.invalb)) then
 
*
* CALCULATE TRUE DATA VALUES
*
               a=ia(i,j)*ascale+azero
               b=ib(i,j)*bscale+bzero
 
*
* SELECT AND PERFORM REQUIRED FUNCTION
*
               go to (101,102,103,104) nfunct
 
101            c=((a+b)-czero)*rcscal
               go to 200
 
102            c=((a-b)-czero)*rcscal
               go to 200
 
103            c=((a*b)-czero)*rcscal
               go to 200
 
 
104            if(b.eq.0.0) then
                  c=maxint+1.0
 
               else
                  c=((a/b)-czero)*rcscal
               endif
 
200            continue
 
*
* IF ANSWER LIES IN THE VALID RANGE, ADD TO OUTPUT IMAGE
*
 
               if((c.le.maxint).and.(c.ge.minint)) then
                  ic(i,j)=nint(c)
 
               else
                  ic(i,j)=invalc
               endif
 
 
*
* IF EITHER INPUT PIXEL WAS INVALID, SO IS THE OUTPUT PIXEL
*
 
            else
               ic(i,j)=invalc
            endif
 
1        continue
 
2     continue
 
      return
 
      end
 
 
 
