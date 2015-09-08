      subroutine wmode(x,w,nx,pbad,niter,toll,xmode,sigma)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIND THE MOST LIKELY MEAN VALUE FOR A SET OF DATA POINTS
*       WITH DIFFERENT NORMALLY DISTRIBUTED ERRORS AND A CONSTANT
*       PROBABILITY OF BEING CORRUPT.
*
*METHOD
*       THIS ROUTINE USES THE STATISTICAL MODEL USED BY MODE TO
*       MINIMISE THE EFFECT OF CORRUPT DATA, BUT INCLUDES DIFFERENT
*       WEIGHTS FOR EACH DATA POINT, TO ALLOW FOR DIFFERENT INTRINSIC
*       ERRORS. SEE MODE FOR A FULLER DESCRIPTION OF THE ALGORITHM.
*
*ARGUMENTS
*       X (IN)
*       REAL(NX)
*               AN ARRAY OF DATA VALUES
*       W (IN)
*       REAL(NX)
*               AN ARRAY OF WEIGHTS FOR EACH DATA POINT IN X. THE
*               WEIGHTS ARE INVERSELY PROPORTIONAL TO THE SQUARE OF
*               THE RELATIVE ERRORS ON EACH DATA POINT.
*       NX (IN)
*       INTEGER
*               THE NUMBER OF DATA POINTS
*       PBAD (IN)
*       REAL
*               AN ESTIMATE OF THE PROBABILITY THAT ANY ONE DATA POINT
*               IS CORRUPT. (THIS VALUE NOT VERY CRITICAL)
*       NITER (IN)
*       INTEGER
*               THE MAXIMUM NUMBER OF ITERATIONS TO BE PERFORMED
*       TOLL (IN)
*       REAL
*               THE ABSOLUTE ACCURACY REQUIRED IN THE MEAN VALUE.
*               ITERATIONS CEASE WHEN TWO SUCCESSIVE ITERATIONS AGREE
*               TO WITHIN THIS VALUE.
*       XMODE (OUT)
*       REAL
*               THE ESTIMATE OF THE UNCORRUPTED MEAN VALUE
*       SIGMA (OUT)
*       REAL
*               AN ESTIMATE OF THE UNCORRUPTED NORMALISED STANDARD
*               DEVIATION OF THE DATA POINTS. AN ESTIMATE OF THE
*               STANDARD DEVIATION OF ANY ONE DATA POINT IS:
*                   SIGMA/SQRT(W) , WHERE W IS ITS WEIGHT.
*
*CALLS
*       NONE
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      real x(nx),w(nx)
      double precision sum1,sum2,sum3
 
*
* FORM SUMS FOR FINDING THE MEAN AND STANDARD DEVIATION OF THE
* DATA
*
      sum1=0.0d0
      sum2=0.0d0
      sum3=0.0d0
 
      do 1 i=1,nx
         data=w(i)
         sum1=sum1+data
         data=data*x(i)
         sum2=sum2+data
         data=data*x(i)
         sum3=sum3+data
1     continue
 
 
*
* CALCULATE THE WEIGHTED MEAN AND VARIANCE
*
      xmode=sum2/sum1
      sig2=(sum3-(sum2*sum2)/sum1)/nx
      sig2=max(0.0,sig2)
 
*
* HAVING FORMED THE INITIAL ESTIMATES, THE MAIN ITERATION LOOP
* STARTS HERE
*
 
      do 3 iter=1,niter
 
*
* CALCULATE THE STANDARD DEVIATION AND NORMALISATION CONSTANTS
*
         sig2=max(1.0e-20,sig2)
         sig1=sqrt(sig2)
         w1=1.0/sig1
         w2=0.5/sig2
         pbnorm=0.707107*pbad
 
*
* SET LIMIT AT WHICH POINTS ARE NEGLIGIBLE AT 10 STAND. DEVS.
*
         devlim=100.0*sig2
 
*
* INITIALLISE SUMS FOR FINDING NEW ESTIMATES
*
         suma=0.0
         sumb=0.0
         sumc=0.0
         sumd=0.0
 
*
* COUNT THROUGH DATA POINTS
*
 
         do 2 i=1,nx
 
*
* FIND DEVIATION FROM MODE, NORMALISED TO THAT EXPECTED FROM
* THE WEIGHTS SUPPLIED
*
            dx=x(i)-xmode
            dx2=dx*dx
            dev2=dx2*w(i)
 
*
* IF POINT IS NOT NEGLIGIBLE, CALCULATE THE FRACTIONAL PROBABILITY
* THAT IT IS GOOD
*
 
            if(dev2.le.devlim) then
               ex=exp(-w2*dev2)
               prob=ex/(ex+pbnorm)
               suma=suma+prob
               sumb=sumb+prob*w(i)
               sumc=sumc+dx*prob*w(i)
               sumd=sumd+dev2*prob
            endif
 
2        continue
 
 
*
* CALCULATE THE NEW ESTIMATES
*
         suma=max(suma,1.0e-20)
         sumb=max(sumb,1.0e-20)
         dxmode=sumc/sumb
         xmode=xmode+dxmode
         sig2=sumd/suma
 
*
* IF REQUIRED ACCURACY HAS BEEN ACHIEVED, EXIT ITERATION LOOP
*
 
         if(abs(dxmode).le.toll) then
            go to 7
 
         endif
 
3     continue
 
7     sigma=sqrt(sig2)
      return
 
      end
 
 
 
