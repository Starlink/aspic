      subroutine dspscl(ia,npix,nlines,inval,scale,zero,amin,amax,imin
     : ,imax,ib)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO MAP A GIVEN DATA RANGE IN ONE IMAGE LINEARLY ON TO ANOTHER
*       RANGE OF INTEGERS IN A SECOND IMAGE.
*
*METHOD
*       APPLY THE LINEAR TRANSFORMATION TO THE IMAGE INTEGERS, ALLOWING
*       THE OUTPUT DATA VALUES TO SATURATE IF THE OUTPUT RANGE IS
*       EXCEEDED. INVALID PIXELS ARE SET TO THE LOWER LIMIT OF THE
*       OUTPUT RANGE
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX*NLINES)
*               THE INPUT IMAGE
*       NPIX,NLINES (IN)
*       INTEGER
*               THE DIMENSIONS OF IA
*       INVAL (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IA
*       SCALE, ZERO (IN)
*       REAL
*               SCALE AND ZERO LEVEL FOR INPUT IMAGE
*       AMIN,AMAX (IN)
*       REAL
*               RANGE OF INPUT DATA VALUES TO BE MAPPED ON TO THE OUTPUT
*               INTEGERS
*       IMIN,IMAX (IN)
*       INTEGER
*               RANGE OF OUTPUT INTEGERS TO BE PRODUCED
*       IB (OUT)
*       INTEGER*2(NPIX*NLINES)
*               THE OUTPUT IMAGE
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
      integer*2 ia(npix*nlines),ib(npix*nlines)
 
*
* CALCULATE CONVERSION CONSTANTS BETWEEN INPUT AND OUTPUT INTEGERS
*
      bmin=imin
      bmax=imax
      eps=0.5*scale
      deltaa=amax-amin
 
      if(abs(deltaa).lt.eps) deltaa=sign(eps,deltaa)
      consta=scale*(imax-imin)/deltaa
      constb=((zero-amin)*(imax-imin)/deltaa)+imin
 
*
* SCAN THE IMAGE, CONVERTING THE INTEGERS
*
 
      do 33 i=1,npix*nlines
 
*
* INVALID PIXELS ARE SET TO IMIN
*
 
         if(ia(i).eq.inval) then
            ib(i)=imin
 
         else
            b=min(max(bmin,consta*ia(i)+constb),bmax)
            ib(i)=nint(b)
         endif
 
33    continue
 
 
      end
 
 
 
