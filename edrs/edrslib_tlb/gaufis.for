      subroutine gaufis(data,imin,imax,niter,toll,amp,x0,sigma,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIT A GAUSSIAN WITH A SPECIFIED WIDTH TO A 1D ARRAY OF DATA
*
*METHOD
*       MAKE AN INITIAL ESTIMATE OF THE CENTRE AND AMPLITUDE BY
*       CENTROIDING FOLLOWED BY LINEAR LEAST SQUARES. THEN REFINE
*       THE ESTIMATES BY REPEATEDLY SOLVING THE LINEARISED NORMAL
*       EQUATIONS FOR THE CENTRE AND AMPLITUDE.
*
*ARGUMENTS
*       DATA (IN)
*       REAL(IMIN:IMAX)
*               THE INPUT DATA ARRAY
*       IMIN,IMAX (IN)
*       INTEGER
*               THE COORDINATES OF THE FIRST AND LAST DATA ELEMENTS
*               THESE ALSO DEFINE THE DIMENSION OF 'DATA'
*       NITER (IN)
*       INTEGER
*               MAXIMUM NUMBER OF REFINING ITERATIONS
*       TOLL (IN)
*       REAL
*               ACCURACY CRITERION: ABSOLUTE FOR THE CENTRE AND
*               RELATIVE FOR THE AMPLITUDE
*       AMP (OUT)
*       REAL
*               GAUSSIAN AMPLITUDE FOUND BY ROUTINE
*       X0 (OUT)
*       REAL
*               CENTRE FOUND BY ROUTINE
*       SIGMA (IN)
*       REAL
*               SIGMA FOR THE GAUSSIAN TO BE FITTED
*       IERR (OUT)
*       INTEGER
*               ERROR INDICATOR: ZERO FOR SUCCESS
*
*CALLS
*       NONE
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      real data(imin:imax),x(2),dx(2),c(2)
      double precision a(2,2),r(2)
 
*
* IERR=1 UNTIL THE PROCESS CONVERGES
*
      ierr=1
      points=imax-imin+1
 
*
* FORM AN INITIAL ESTIMATE OF THE CENTRE AS THE DATA CENTROID
*
      sum1=0.0
      sum2=0.0
 
      do 1 i=imin,imax
 
*
* DO NOT ALLOW NEGATIVE DATA
*
         d=max(data(i),0.0)
         sum1=sum1+d
         sum2=sum2+d*i
1     continue
 
 
*
* IF THERE IS NO POSITIVE DATA, SET IERR=2 AND ABORT
*
 
      if(sum1.le.0.0) then
         ierr=2
         go to 99
 
      endif
 
      x(2)=sum2/sum1
 
*
* INITIALLISE SUMS FOR ESTIMATING THE AMPLITUDE
*
      sum1=0.0
      sum2=0.0
      rsig=1.0/sigma
 
*
* ESTIMATE THE AMPLITUDE USING THE PREVIOUS CENTRE BY LINEAR
* LEAST SQUARES
*
 
      do 2 i=imin,imax
         dev=(i-x(2))*rsig
         dev2=dev*dev
 
         if(dev2.le.25.0) then
 
*
* IGNORE POINTS MORE THAN 5 SIGMA FROM THE CENTRE
*
            ex=exp(-0.5*dev2)
            d=max(data(i),0.0)
            sum1=sum1+ex*ex
            sum2=sum2+d*ex
         endif
 
2     continue
 
 
*
* IF SIGMA IS VERY SMALL THERE MAY BE NO POINTS WITHIN 5 SIGMA
* ...ABORT WITH IERR=3
*
 
      if(sum1.le.0.0) then
         ierr=3
         go to 99
 
      endif
 
      x(1)=sum2/sum1
 
*
* NOW START THE MAIN ITERATION LOOP
* ---------------------------------
*
      rvar=0.5/(sigma*sigma)
      rvar2=2.0*rvar
      devmax=(5.0*sigma)**2
 
      do 88 iter=1,niter
 
*
* INITIALLISE ARRAYS FOR LINEARISED NORMAL EQUATIONS
*
         r(1)=0.0d0
         r(2)=0.0d0
         a(1,1)=0.0d0
         a(2,1)=0.0d0
         a(1,2)=0.0d0
         a(2,2)=0.0d0
 
*
* FORM SUMS OVER THE DATA POINTS
*
 
         do 44 i=imin,imax
            dev=i-x(2)
            dev2=dev*dev
 
*
* IGNORE POINTS BEYOND 5 SIGMA FROM THE CENTRE
*
 
            if(dev2.le.devmax) then
               ex=exp(-rvar*dev2)
 
*
* SET UP SUMS FOR LINEARISED NORMAL EQUATIONS
*
               c(1)=ex
               c(2)=ex*x(1)*dev*rvar2
               delta=x(1)*ex-data(i)
               r(1)=r(1)+delta*c(1)
               r(2)=r(2)+delta*c(2)
               a(1,1)=a(1,1)+c(1)*c(1)
               a(2,1)=a(2,1)+c(2)*c(1)
               a(2,2)=a(2,2)+c(2)*c(2)
            endif
 
44       continue
 
         a(1,2)=a(2,1)
 
*
* IF THE NORMAL EQUATIONS ARE SINGULAR, ABORT WITH IERR=4
*
         det=a(1,1)*a(2,2)-a(1,2)*a(2,1)
 
         if(det.eq.0.0) then
            ierr=4
            go to 99
 
         endif
 
 
*
* SOLVE NORMAL EQUATIONS
*
         dx(1)=(r(1)*a(2,2)-a(1,2)*r(2))/det
         dx(2)=(a(1,1)*r(2)-r(1)*a(2,1))/det
 
*
* APPLY CORRECTIONS TO AMPLITUDE AND CENTRE WITH A DAMPING FACTOR
* FOR LARGE CHANGES
*
         x(1)=x(1)-dx(1)/(1.0+2.0*abs(dx(1)/max(x(1),1.0e-10)))
         x(2)=x(2)-dx(2)/(1.0+2.0*abs(dx(2)/points))
 
*
* IF THE CONVERGENCE CRITERIA ARE SATISFIED, EXIT FROM ITERATION
* LOOP
*
 
         if(abs(dx(1)).le.toll*x(1).and.abs(dx(2)).le.toll) then
            ierr=0
            go to 501
 
         endif
 
88    continue
 
501   amp=x(1)
      x0=x(2)
99    return
 
      end
 
 
 
