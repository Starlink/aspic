      subroutine newscl(ia,npix,nlines,invala,ascale,azero,invalb,
     :bscale,bzero,ib)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO ALTER THE INTEGERS IN AN IMAGE TO SUIT A NEW SCALE FACTOR
*       AND ZERO LEVEL AND A NEW INVALID PIXEL FLAG
*
*METHOD
*       ALTER EACH INTEGER SO THAT IT HAS THE SAME DATA VALUE WITH THE
*       NEW ITEMS
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX*NLINES)
*               THE INPUT IMAGE
*       NPIX,NLINES (IN)
*       INTEGER
*               THE DIMENSIONS OF IA,IB
*       INVALA (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IA
*       ASCALE,AZERO (IN)
*       REAL
*               SCALE FACTOR AND ZERO LEVEL FOR IA
*       INVALB (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IB
*       BSCALE,BZERO (IN)
*       REAL
*               SCALE FACTOR AND ZERO LEVEL FOR IB
*       IB (OUT)
*       INTEGER*2(NPIX*NLINES)
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
      integer*2 ia(npix*nlines),ib(npix*nlines)
 
*
* CALCULATE CONVERSION FACTORS FROM INPUT TO OUTPUT
*
      nn=npix*nlines
      bs=bscale
 
      if(abs(bs).lt.1.0e-20) bs=sign(1.0e-20,bs)
      zero=(azero-bzero)/bs
      scale=ascale/bs
 
*
* SCAN IMAGE
*
 
      do 1 i=1,nn
 
*
* IF INPUT PIXEL IS INVALID, SO IS OUTPUT
*
 
         if(ia(i).eq.invala) then
            ib(i)=invalb
 
         else
 
*
* CALCULATE OUTPUT INTEGER
*
            b=ia(i)*scale+zero
 
*
* OUTPUT IS INVALID IF OVERFLOW OCCURS
*
 
            if(abs(b).le.32767.0) then
               ib(i)=nint(b)
 
            else
               ib(i)=invalb
            endif
 
         endif
 
1     continue
 
 
      end
 
 
 
