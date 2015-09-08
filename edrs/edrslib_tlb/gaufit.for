      subroutine gaufit(data,imin,imax,niter,toll,amp,x0,sigma,back
     : ,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIT A GAUSSIAN TO A 1 DIMENSIONAL ARRAY OF DATA
*
*METHOD
*       FIND INITIAL ESTIMATES OF THE GAUSSIAN AMPLITUDE, MEAN AND
*       WIDTH BY CENTROIDING AND LINEAR LEAST-SQUARES, USING THE
*       LOWER QUARTILE DATA POINT AS AN INITIAL BACKGROUND ESTIMATE.
*       REFINE THE ESTIMATES BY REPEATEDLY SOLVING THE LINEARISED NORMAL
*       EQUATIONS FOR A LEAST-SQUARES FIT.
*
*ARGUMENTS
*       DATA (IN)
*       REAL(IMIN:IMAX)
*               THE DATA ARRAY
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
*               RELATIVE FOR THE AMPLITUDE AND WIDTH
*               ACCURACY OF BACK IS ASSESSED AS A FRACTION OF THE
*               GAUSSIAN AMPLITUDE
*       AMP (OUT)
*       REAL
*               THE GAUSSIAN AMPLITUDE
*       X0 (OUT)
*       REAL
*               THE GAUSSIAN CENTRE
*       SIGMA (OUT)
*       REAL
*               THE 'SIGMA' FOR THE GAUSSIAN
*       BACK (OUT)
*       REAL
*               THE BACKGROUND SIGNAL LEVEL
*       IERR (OUT)
*       INTEGER
*               ERROR INDICATOR: ZERO FOR SUCCESS
*               1: ACCURACY CRITERION NOT MET
*               2: ALL DATA HAS THE SAME VALUE
*               3: NORMAL EQUATIONS ARE SINGULAR
*
*CALLS
*       THIS PACKAGE:
*               NTHMIN
*       NAG LIBRARY:
*               F04ATF
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      real data(imin:imax),x(4),c(4),stak(25)
      double precision sum0,sum1,sum2,sum3,a(4,4),r(4),t1,t2,t3,b,dx(4)
     : ,aa(4,4),wks1(4),wks2(4)
      parameter (varmin=0.3**2)
 
*
* INITIALLISE SUMS FOR FORMING INITIAL ESTIMATE OF THE DATA MEAN
* AND STANDARD DEVIATION
*

      ierr=1
      points=imax-imin+1
      sum1=0.0d0
      sum2=0.0d0
      sum3=0.0d0
 
*
* FIND THE LOWER QUARTILE DATA POINT (OR THE 25'TH SMALLEST IF THERE
* ARE TOO MANY) TO USE AS AN INITIAL BACKGROUND ESTIMATE
*
      nquart=min(max(1,nint(points*0.25)),25)
      call nthmin(data(imin),nint(points),nquart,stak,ierrqt)
      x(4)=stak(1)
 
*
* FORM SUMS FOR MEAN AND STANDARD DEVIATION
*
 
      do 1 i=imin,imax
 
*
* DO NOT COUNT NEGATIVE DATA
*
         d=max(0.0,data(i)-x(4))
         sum1=sum1+d
         d=d*i
         sum2=sum2+d
         d=d*i
         sum3=sum3+d
1     continue
 
 
*
* IF THERE ARE NO POSITIVE DATA POINTS, ABORT WITH IERR=2
*
 
      if(sum1.le.0.0d0) then
         ierr=2
         go to 99
 
      endif
 
 
*
* FORM MEAN AND STANDARD DEVIATION
*
      x(2)=sum2/sum1
      x(3)=sqrt(max(dble(varmin),(sum3-sum2*x(2))/sum1))
 
*
* INITIALLISE SUMS FOR FORMING AN INITIAL ESTIMATE OF THE GAUSSIAN
* AMPLITUDE
*
      sum0=0.0d0
      sum1=0.0d0
      sum2=0.0d0
      sum3=0.0d0
 
*
* CONSIDER THOSE DATA POINTS WITHIN 5 STANDARD DEVIATIONS OF THE MEAN
*
      rvar=0.5/(x(3)**2)
      devmax=(5.0*x(3))**2
 
      do 2 i=imin,imax
         dev=i-x(2)
         dev2=dev*dev
 
         if(dev2.le.devmax) then
 
*
* FORM SUMS
*
            ex=exp(-rvar*dev2)
            d=max(0.0,data(i)-x(4))
            sum0=sum0+ex*ex
            sum1=sum1+d*ex
            sum2=sum2+d*dev*ex
            sum3=sum3+d*dev2*ex
         endif
 
2     continue
 
 
*
* IF THERE WERE NO SATISFACTORY DATA POINTS, ABORT WITH IERR=2
*
 
      if(sum0.le.0.0d0.or.sum1.le.0.0d0) then
         ierr=2
         go to 99
 
      endif
 
 
*
* FORM ESTIMATES OF THE GAUSSIAN HEIGHT AND MEAN
*
      x(1)=sum1/sum0
      x(2)=x(2)+sum2/sum1
 
*
* ESTIMATE THE VARIANCE, THEN MAKE AN APPROXIMATE CORRECTION
* FOR THE FACT THAT THE DATA POINTS WERE WEIGHTED WITH A GAUSSIAN
* USING THE OLD VALUE FOR THE VARIANCE
*
      var=(sum3-(sum2*sum2)/sum1)/sum1
      oldvar=x(3)*x(3)
      var=(oldvar*var)/max(oldvar-var,1.0e-10)
      x(3)=sqrt(max(varmin,var))
 
*
* START THE ITERATION LOOP TO REFINE THE ESTIMATES OF THE GAUSSIAN
* PARAMETERS
*
 
      do 88 iter=1,niter
 
*
* INITIALLISE ARRAYS FOR FORMING THE LINEARISED NORMAL EQUATIONS
*
 
         do 6 i=1,4
            r(i)=0.0d0
 
            do 16 j=1,4
               a(i,j)=0.0d0
16          continue
 
6        continue
 
 
*
* CONSIDER DATA POINTS WITHIN 5 STANDARD DEVIATIONS OF THE MEAN
*
         rvar1=1.0/x(3)
         rvar2=rvar1*rvar1
         rvar=0.5*rvar2
         devmax=(5.0*x(3))**2
 
         do 44 i=imin,imax
            dev=i-x(2)
            dev2=dev*dev
 
            if(dev2.le.devmax) then
 
*
* FORM THE QUANTITIES REQUIRED IN THE NORMAL EQUATIONS
*
               ex=exp(-rvar*dev2)
               c(1)=ex
               c(2)=ex*x(1)*dev*rvar2
               c(3)=c(2)*dev*rvar1
               c(4)=1.0
 
*
* DELTA IS THE DEVIATION OF THE FIT FROM THE DATA
*
               delta=x(1)*ex+x(4)-data(i)
 
*
* FORM SUMS FOR NORMAL EQUATIONS
*
               r(1)=r(1)+delta*c(1)
               r(2)=r(2)+delta*c(2)
               r(3)=r(3)+delta*c(3)
               r(4)=r(4)+delta*c(4)
               a(1,1)=a(1,1)+c(1)*c(1)
               a(2,1)=a(2,1)+c(2)*c(1)
               a(3,1)=a(3,1)+c(3)*c(1)
               a(4,1)=a(4,1)+c(4)*c(1)
               a(2,2)=a(2,2)+c(2)*c(2)
               a(3,2)=a(3,2)+c(3)*c(2)
               a(4,2)=a(4,2)+c(4)*c(2)
               a(3,3)=a(3,3)+c(3)*c(3)
               a(4,3)=a(4,3)+c(4)*c(3)
               a(4,4)=a(4,4)+c(4)*c(4)
            endif
 
44       continue
 
 
*
* FORM THE COMPLETE SYMMETRIC MATRIX OF NORMAL COEFFICIENTS
*
         a(1,2)=a(2,1)
         a(1,3)=a(3,1)
         a(2,3)=a(3,2)
         a(1,4)=a(4,1)
         a(2,4)=a(4,2)
         a(3,4)=a(4,3)
 
*
* CALL NAG ROUTINE F04ATF TO SOLVE THE LINEARISED NORMAL EQUATIONS
*
         ifail=1
         call f04atf(a,4,r,4,dx,aa,4,wks1,wks2,ifail)
 
         if(ifail.ne.0) then
            ierr=3
            go to 99
 
         endif
 
 
*
* APPLY THE CORRECTIONS TO THE GAUSSIAN PARAMETERS WITH DAMPING
* FACTORS FOR LARGE AMPLITUDE CHANGES
*
         x(1)=x(1)-dx(1)/(1.0+2.0*abs(dx(1)/max(x(1),1.0e-20)))
         x(2)=x(2)-dx(2)/(1.0+2.0*abs(dx(2)/points))
         x(3)=x(3)-dx(3)/(1.0+2.0*abs(dx(3)/max(x(3),1.0e-20)))
         x(4)=x(4)-dx(4)/(1.0+2.0*abs(dx(4)/max(x(1),1.0e-20)))
 
*
* IF THE ACCURACY CRITERION IS MET, EXIT FROM THE ITERATION LOOP
*
 
         if((abs(dx(1)).le.toll*x(1)).and.(abs(dx(2)).le.toll).and.
     :   (abs(dx(3)).le.toll*x(3)).and.(abs(dx(4)).le.toll*x(1))) then
            ierr=0
            amp=x(1)
            x0=x(2)
            sigma=x(3)
            back=x(4)
            go to 99
 
         endif
 
88    continue
 
99    return
 
      end
 
 
 
