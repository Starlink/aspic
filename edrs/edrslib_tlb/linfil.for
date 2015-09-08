      subroutine linfil(ia,npix,nlines,invala,imeth,nmin,invalb,nbad
     : ,ib)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO EVALUATE AN IMAGE, EACH LINE OF WHICH IS A CONSTANT OR
*       STRAIGHT LINE LEAST-SQUARES FIT TO THE CORRESPONDING INPUT
*       IMAGE LINE
*
*METHOD
*       SCAN EACH INPUT IMAGE LINE, FORMING THE REQUIRED SUMS FOR
*       PERFORMING THE FIT, USING ONLY THE VALID PIXELS. IF SUFFICIENT
*       PIXELS ARE VALID, EVALUATE THE FIT OVER THE OUTPUT LINE.
*       OTHERWISE SET THE OUTPUT LINE INVALID.
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX,NLINES)
*               THE INPUT IMAGE
*       NPIX,NLINES (IN)
*       INTEGER
*               THE DIMENSIONS OF THE IMAGES IA AND IB
*       INVALA (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IA
*       IMETH (IN)
*       INTEGER
*               METHOD OF FITTING THE IMAGE LINES
*                       1: CONSTANT
*                       2: STRAIGHT LINE
*       NMIN (IN)
*       INTEGER
*               MINIMUM NUMBER OF VALID INPUT PIXELS REQUIRED IN EACH
*               LINE FOR THE OUTPUT LINE TO BE VALID
*       INVALB (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IB
*       NBAD (OUT)
*       INTEGER
*               NUMBER OF LINES NOT FITTED DUE TO TOO MANY INVALID
*               PIXELS
*       IB (OUT)
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
      integer*2 ia(npix,nlines),ib(npix,nlines)
      double precision s0,s1,s2,t0,t1,x
 
*
* MINIMUM PIXELS PER LINE MUST LIE IN THE RANGE IMETH TO NPIX
*
      minpix=min(max(imeth,nmin),npix)
      nbad=0
 
*
* SCAN THROUGH THE IMAGE LINES
*
 
      do 100 j=1,nlines
 
*
* FOR FITTING A CONSTANT
* ----------------------
*
 
         if(imeth.eq.1) then
 
*
* FORM SUMS FOR THE MEAN OF ALL THE VALID PIXELS
*
            sum=0.0
            npt=0
 
            do 6 i=1,npix
 
               if(ia(i,j).ne.invala) then
                  sum=sum+ia(i,j)
                  npt=npt+1
               endif
 
6           continue
 
 
*
* IF THERE ARE SUFFICIENT VALID PIXELS, SET THE OUTPUT LINE TO THE
* MEAN VALUE
*
 
            if(npt.ge.minpix) then
               ibval=nint(sum/npt)
 
               do 7 i=1,npix
                  ib(i,j)=ibval
7              continue
 
 
*
* IF THERE ARE INSUFFICIENT VALID PIXELS, THE OUTPUT LINE IS INVALID
* COUNT 1 BAD LINE
*
 
            else
               nbad=nbad+1
 
               do 8 i=1,npix
                  ib(i,j)=invalb
8              continue
 
            endif
 
 
*
* FOR STRAIGHT LINE FIT
* ---------------------
*
 
         else if(imeth.eq.2) then
 
*
* INITIALLISE SUMS FOR LEAST-SQUARES FIT
*
            npt=0
            s1=0.0d0
            s2=0.0d0
            t0=0.0d0
            t1=0.0d0
 
*
* FORM THE SUMS OVER THE VALID PIXELS
*
 
            do 16 i=1,npix
 
               if(ia(i,j).ne.invala) then
                  x=i
                  npt=npt+1
                  s1=s1+x
                  s2=s2+x*x
                  t0=t0+ia(i,j)
                  t1=t1+ia(i,j)*x
               endif
 
16          continue
 
            s0=npt
 
*
* IF THERE ARE SUFFICIENT VALID PIXELS, CALCULATE THE COEFFICIENTS
* OF THE STRAIGHT LINE FIT
*
 
            if(npt.ge.minpix) then
               a=(s0*t1-s1*t0)/(s0*s2-s1*s1)
               b=(s2*t0-s1*t1)/(s0*s2-s1*s1)
 
*
* EVALUATE THE FIT OVER THE OUTPUT LINE
*
 
               do 17 i=1,npix
                  value=a*i+b
 
*
* OUTPUT PIXEL IS INVALID IF OVERFLOW OCCURS
*
 
                  if(abs(value).le.32767.0) then
                     ib(i,j)=nint(value)
 
                  else
                     ib(i,j)=invalb
                  endif
 
17             continue
 
 
*
* IF THERE ARE INSUFFICIENT VALID PIXELS, OUTPUT LINE IS INVALID
* COUNT 1 BAD LINE
*
 
            else
               nbad=nbad+1
 
               do 18 i=1,npix
                  ib(i,j)=invalb
18             continue
 
            endif
 
         endif
 
100   continue
 
 
      end
 
 
 
