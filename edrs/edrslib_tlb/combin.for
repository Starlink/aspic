      subroutine combin(ia,npix,nimage,weight,meth,scale,zero,inval
     : ,nmin,scalef,zerol,invalb,lineb,ib,work)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO COMBINE A NUMBER OF LINES FROM DIFFERENT IMAGES INTO A
*       SINGLE OUTPUT LINE
*
*METHOD
*       WORK ALONG THE LINE, COMBINING THE VALUES FROM EACH INPUT IMAGE
*       USING EITHER A WEIGHTED MEAN, THE MEDIAN, THE MODE, THE MAXIMUM
*       OR THE MINIMUM, TAKING DUE ACCOUNT OF INVALID PIXELS AND
*       DIFFERING SCALE FACTORS AND ZERO LEVELS IN EACH IMAGE.
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX,NIMAGE)
*               THE INPUT STACK OF IMAGE LINES
*       NPIX (IN)
*       INTEGER
*               THE NUMBER OF PIXELS PER LINE
*       NIMAGE (IN)
*       INTEGER
*               THE NUMBER OF IMAGE LINES IN THE STACK IA
*       WEIGHT (IN)
*       REAL(NIMAGE)
*               WEIGHTS TO BE USED WITH EACH IMAGE IF USING MEAN OR
*               MODE
*       METH (IN)
*       INTEGER
*               METHOD TO BE USED IN THE RANGE 1 TO 5
*       SCALE,ZERO (IN)
*       REAL
*               SCALE FACTORS AND ZERO LEVELS FOR INPUT IMAGE LINES
*       INVAL (IN)
*       INTEGER
*               INVALID PIXEL FLAGS FOR INPUT IMAGE LINES
*       NMIN (IN)
*       INTEGER
*               MINIMUM NUMBER OF IMAGES REQUIRED AT EACH PIXEL BEFORE
*               A VALID OUTPUT VALUE IS GENERATED
*       SCALEF,ZEROL (IN)
*       REAL
*               CONSTANTS TO CONVERT OUTPUT VALUES TO INTEGERS
*               I.E. THE 'INVERSE' OF THE OUTPUT SCALE AND ZERO LEVEL
*       INVALB (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR OUTPUT IMAGE
*       LINEB (IN)
*       INTEGER
*               THE LINE IN THE OUTPUT IMAGE IB INTO WHICH THE RESULT
*               IS TO BE PUT
*       IB (IN/OUT)
*       INTEGER*2(NPIX,*)
*               THE OUTPUT IMAGE INTO WHICH THE RESULTANT LINE IS
*               INSERTED
*       WORK (WORKSPACE)
*       REAL(NIMAGE,2)
*               USED TO STORE INTERMEDIATE VALUES IN THE COMPUTATIONS
*
*CALLS
*       THIS PACKAGE:
*               NTHMIN,WMODE
*
*NOTES
*       USES INTEGER*2 ARRAYS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      integer*2 ia(npix,nimage),ib(npix,*)
      real weight(nimage),scale(nimage),zero(nimage),work(nimage,2)
      integer inval(nimage)
 
*
* SET PROBABILITY OF A PIXEL BEING CORRUPT (USED IN THE ROUTINE WMODE)
* 0.01 IS A SAFE VALUE FOR ELECTRONOGRAPHS
*
      parameter (pbad=0.01)
 
*
* SET ACCURACY CRITERION AND LIMIT THE METHOD TO THE RANGE 1-5
*
      toll=0.5/scalef
      method=min(max(1,meth),5)
 
*
* COUNT THROUGH EACH PIXEL IN THE LINE
*
 
      do 100 i=1,npix
 
*
* INITIALLISE COUNTER FOR GOOD IMAGES AT EACH PIXEL LOCATION
*
         ngood=0
 
*
* IF THE MEAN IS REQUIRED:
* ------------------------
*
 
         if(method.eq.1) then
            sum1=0.0
            sum2=0.0
 
*
* FORM WEIGHTED MEAN OF VALID VALUES
*
 
            do 901 j=1,nimage
 
               if(ia(i,j).ne.inval(j)) then
                  ngood=ngood+1
                  sum1=sum1+weight(j)
                  sum2=sum2+weight(j)*(ia(i,j)*scale(j)+zero(j))
               endif
 
901         continue
 
 
*
* IF INSUFFICIENT IMAGES WERE GOOD, ABORT AND SET OUTPUT INVALID
*
 
            if(ngood.ge.nmin) then
               b=sum2/sum1
 
            else
               go to 999
 
            endif
 
 
*
* IF THE MEDIAN IS REQUIRED:
* --------------------------
*
 
         else if(method.eq.2) then
 
*
* EXTRACT A LIST OF THE VALID VALUES, CORRECTLY SCALED
*
 
            do 902 j=1,nimage
 
               if(ia(i,j).ne.inval(j)) then
                  ngood=ngood+1
                  work(ngood,1)=ia(i,j)*scale(j)+zero(j)
               endif
 
902         continue
 
 
*
* IF SUFFICIENT POINTS ARE PRESENT, CALL NTHMIN TO FIND THE MEDIAN
* VALUE
*
 
            if(ngood.ge.nmin) then
               nmed=ngood/2+1
               call nthmin(work(1,1),ngood,nmed,work(1,2),ierr)
 
*
* MEDIAN LIES BETWEEN TWO VALUES IF THE NUMBER OF VALUES IS EVEN
*
 
               if(mod(ngood,2).eq.0) then
                  b=(work(1,2)+work(2,2))*0.5
 
               else
                  b=work(1,2)
               endif
 
 
            else
               go to 999
 
            endif
 
 
*
* IF THE MODE IS REQUIRED:
* ------------------------
*
 
         else if(method.eq.3) then
 
*
* FORM A LIST OF THE VALID VALUES (SCALED) AND THEIR WEIGHTS
*
 
            do 903 j=1,nimage
 
               if(ia(i,j).ne.inval(j)) then
                  ngood=ngood+1
                  work(ngood,1)=ia(i,j)*scale(j)+zero(j)
                  work(ngood,2)=weight(j)
               endif
 
903         continue
 
 
*
* IF SUFFICIENT VALUES ARE VALID, CALL WMODE TO FIND THE MAXIMUM
* LIKELIHOOD VALUE
*
 
            if(ngood.ge.nmin) then
               call wmode(work(1,1),work(1,2),ngood,pbad,3,toll,b,
     :         sigma)
 
            else
               go to 999
 
            endif
 
 
*
* IF THE MINIMUM IS REQUIRED:
* ---------------------------
*
 
         else if(method.eq.4) then
            b=1.0e20
 
*
* FIND THE MINIMUM OF THE VALID VALUES (SCALED)
*
 
            do 904 j=1,nimage
 
               if(ia(i,j).ne.inval(j)) then
                  ngood=ngood+1
                  b=min(b,ia(i,j)*scale(j)+zero(j))
               endif
 
904         continue
 
 
            if(ngood.lt.nmin) go to 999
 
*
* IF THE MAXIMUM IS REQUIRED:
* ---------------------------
*
 
         else if(method.eq.5) then
            b=-1.0e20
 
*
* FIND THE MAXIMUM OF THE VALID VALUES (SCALED)
*
 
            do 905 j=1,nimage
 
               if(ia(i,j).ne.inval(j)) then
                  ngood=ngood+1
                  b=max(b,ia(i,j)*scale(j)+zero(j))
               endif
 
905         continue
 
 
            if(ngood.lt.nmin) go to 999
         endif
 
 
*
* THE OUTPUT VALUE IS NOW KNOWN... APPLY THE OUTPUT SCALE FACTOR
* TO CONVERT TO AN INTEGER
*
         b=b*scalef+zerol
 
         if(abs(b).gt.32767.0) then
            ib(i,lineb)=invalb
 
         else
            ib(i,lineb)=nint(b)
         endif
 
         go to 100
 
 
*
* PIXELS WITH INSUFFICIENT VALID INPUT IMAGES ARE HERE SET INVALID
* ON OUTPUT:
*
999      ib(i,lineb)=invalb
100   continue
 
      return
 
      end
 
 
 
