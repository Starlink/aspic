      subroutine mode(x,nx,pbad,niter,toll,xmode,sigma)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO ESTIMATE THE MEAN OF A NUMBER OF NORMALLY DISTRIBUTED DATA
*       VALUES, SOME OF WHICH MAY BE CORRUPT.
*
*METHOD
*       THE ROUTINE IS BASED ON MAXIMISING THE LIKELIHOOD FUNCTION
*       FOR A STATISTICAL MODEL IN WHICH ANY OF THE DATA POINTS HAS
*       A CONSTANT PROBABILITY OF BEING CORRUPT. A WEIGHTED MEAN IS
*       FORMED WITH WEIGHTS CHOSEN ACCORDING TO THE DEVIATION OF EACH
*       DATA POINT FROM THE CURRENT ESTIMATE OF THE MEAN. THE WEIGHTS
*       ARE DERIVED FROM THE RELATIVE PROBABILITIES OF BEING VALID
*       OR CORRUPT. A SEQUENCE OF THESE ITERATIONS CONVERGES TO A
*       STATIONARY POINT IN THE LIKELIHOOD FUNCTION. THE ROUTINE
*       APPROXIMATES TO A K-SIGMA CLIPPING ALGORITHM FOR A LARGE NUMBER
*       OF DATA POINTS AND TO A MODE-ESTIMATING ALGORITHM FOR FEWER
*       DATA POINTS.
*
*ARGUMENTS
*       X (IN)
*       REAL(NX)
*               AN ARRAY OF DATA VALUES
*       NX (IN)
*       INTEGER
*               THE NUMBER OF DATA VALUES
*       PBAD (IN)
*       REAL
*               AN ESTIMATE OF THE PROBABILITY THAT ANY ONE DATA POINT
*               WILL BE CORRUPT (THIS VALUE IS NOT CRITICAL)
*       NITER (IN)
*       INTEGER
*               THE MAXIMUM NUMBER OF ITERATIONS REQUIRED
*       TOLL (IN)
*       REAL
*               THE ABSOLUTE ACCURACY REQUIRED IN THE ESTIMATE OF THE
*               MEAN. ITERATIONS CEASE WHEN TWO SUCCESSIVE ESTIMATES
*               DIFFER BY LESS THAN THIS AMOUNT
*       XMODE (OUT)
*       REAL
*               THE ESTIMATE OF THE UNCORRUPTED MEAN
*       SIGMA (OUT)
*       REAL
*               AN ESTIMATE OF THE UNCORRUPTED STANDARD DEVIATION
*               OF THE DATA POINTS
*
*CALLS
*       NONE
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      real x(nx)
      double precision sum1,sum2,sum3
 
*
* FORM INITIAL ESTIMATE OF MEAN AND SIGMA
*
      sum1=0.0d0
      sum2=0.0d0
      sum3=0.0d0
 
      do 1 i=1,nx
         sum1=sum1+1.0d0
         sum2=sum2+x(i)
         sum3=sum3+x(i)*x(i)
1     continue
 
      xmode=sum2/sum1
      sig2=(sum3-(sum2*sum2)/sum1)/sum1
      sig2=max(sig2,0.0)
 
*
* NOW START THE ITERATION LOOP
*
      pbnorm=pbad*0.707107
 
      do 3 iter=1,niter
         sig2=max(sig2,1.0e-20,1.0e-12*xmode**2)
         w2=0.5/sig2
 
*
* INITIALLISE SUMS FOR FORMING NEW ESTIMATE
*
         suma=0.0
         sumb=0.0
         sumc=0.0
         devlim=100.0*sig2
 
*
* SCAN THROUGH THE DATA POINTS, FORMING WEIGHTED SUMS TO CALCULATE
* NEW MEAN AND VARIANCE
*
 
         do 2 i=1,nx
            dev=x(i)-xmode
            dev2=dev*dev
 
*
* IGNORE POINTS MORE THAN 10 SIGMA FROM MODE
*
 
            if(dev2.le.devlim) then
 
*
* WEIGHTS DEPEND ON THE FRACTIONAL PROBABILITY OF BEING GOOD DATA
*
               ex=exp(-w2*dev2)
               prob=ex/(pbnorm+ex)
               suma=suma+prob
               sumb=sumb+dev*prob
               sumc=sumc+dev2*prob
            endif
 
2        continue
 
 
*
* FORM THE NEW ESTIMATES
*
         suma=max(suma,1.0e-20)
         dxmode=sumb/suma
         xmode=xmode+dxmode
         sig2=sumc/suma
 
*
* IF THE REQUIRED ACCURACY HAS BEEN MET, RETURN
*
 
         if(abs(dxmode).le.toll) go to 7
3     continue
 
7     sigma=sqrt(sig2)
 
      end
 
 
 
