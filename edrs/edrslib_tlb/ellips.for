      subroutine ellips(sig,sig0,axisr,theta)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO CALCULATE THE AXIS RATIO, INCLINATION AND MINOR AXIS WIDTH
*       OF A STAR IMAGE, GIVEN THE GAUSSIAN WIDTHS OF MARGINAL PROFILES
*       AT 45 DEGREE INTERVALS
*
*METHOD
*       THE RECIPROCAL OF THE SQUARE OF THE WIDTH VARIES APPROXIMATELY
*       LIKE THE LENGTH OF AN ELLIPSE DIAMETER AS THE ANGLE OF
*       PROJECTION VARIES. THE ROUTINE CALCULATES THE REQUIRED
*       PARAMETERS ASSUMING THIS RELATION HOLDS, THEN ITERATES,
*       CALCULATING THE EXPECTED DEVIATION FROM THIS LAW AND SUBTRACTING
*       IT FROM THE DATA BEFORE CALCULATING A NEW ESTIMATE. THE SOLUTION
*       OF THE ELLIPSE EQUATIONS IS ANALOGOUS TO USING THE STOKES
*       PARAMETERS OF LINEAR POLARIZATION TO FIND THE ELLIPSE PARAMETERS
*       THE ROUTINE ASSUMES A GAUSSIAN PROFILE FOR THE STAR
*
*ARGUMENTS
*       SIG (IN)
*       REAL(4)
*               THE GAUSSIAN WIDTHS OF THE MARGINAL PROFILES OF THE STAR
*               IN DIRECTIONS AT 0,45,90 AND 135 DEGREES TO THE X AXIS
*       SIG0 (OUT)
*       REAL
*               THE WIDTH OF THE MINOR AXIS OF THE ELLIPSE
*       AXISR (OUT)
*       REAL
*               THE AXIS RATIO OF THE ELLIPSE.
*       THETA (OUT)
*       REAL
*               THE INCLINATION OF THE MAJOR AXIS TO THE X AXIS IN
*               RADIANS. (X THROUGH Y POSITIVE)
*
*CALLS
*       NONE
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      parameter (niter=10,tolls=1.0e-4,tolla=1.0e-4,tollt=1.0e-2)
      parameter (ang=0.0174533,t45=45.0*ang,t90=90.0*ang,t135=135.0
     : *ang)
      real sig(4),rsig2(4),t(4)
      double precision d(4),delq,delu,amp1,amp2
 
*
* WORK WITH THE RECIPROCALS OF THE WIDTHS SQUARED... THESE VARY
* APPROXIMATELY ELLIPTICALLY WITH INCLINATION ANGLE
*
 
      do 1 i=1,4
         rsig2(i)=(1.0/sig(i))**2
1     continue
 
 
*
* SET INITIAL ESTIMATES OF ELLIPSE PARAMETERS
*
      sig0=1.0
      axisr=1.0
      theta=0.0
 
*
* PERFORM ITERATIONS TO FIND THE ACCURATE PARAMETERS
*
 
      do 13 iter=1,niter
         rsig02=(1.0/sig0)**2
         axisr2=axisr**2
         t(1)=theta
         t(2)=theta-t45
         t(3)=theta-t90
         t(4)=theta-t135
 
*
* MAKE A CORRECTION TO THE DATA VALUES WHICH IS THE AMOUNT BY WHICH
* THEY WOULD DEVIATE FROM A PURE ELLIPTICAL VARIATION GIVEN THE
* CURRENT ELLIPSE PARAMETERS
*
 
         do 12 i=1,4
            c=cos(t(i))
            s=sin(t(i))
            d(i)=rsig02*((c*s*(axisr2-1.0))**2)/(axisr2*((axisr2*c*c)
     :       +(s*s)))
            d(i)=d(i)+rsig2(i)
12       continue
 
 
*
* NOW FIND THE ELLIPSE PARAMETERS ASSUMING THE DATA VARIES ELLIPTICALLY
*
         delq=d(3)-d(1)
         delu=d(4)-d(2)
         amp1=0.5d0*sqrt(delq**2+delu**2)
         amp2=0.25d0*(d(1)+d(2)+d(3)+d(4))
         amp1=min(amp1,0.9999d0*amp2)
 
         if(delq.eq.0.0d0.and.delu.eq.0.0d0) then
            thetan=0.0
 
         else
            thetan=0.5d0*atan2(delu,delq)
         endif
 
         rsig02=amp1+amp2
         sig0n=sqrt(1.0/max(rsig02,1.0e-10))
         axisrn=sqrt((amp1+amp2)/(amp2-amp1))
 
*
* CALCULATE THE CHANGES TO THE PARAMETERS
*
         dsig0=abs(sig0n-sig0)
         daxisr=abs(axisrn-axisr)
         dtheta=abs(thetan-theta)
         sig0=sig0n
         axisr=axisrn
         theta=thetan
 
*
* IF THE ACCURACY CRITERION IS MET, EXIT FROM THE ITERATION LOOP
*
 
         if(dsig0.le.tolls.and.daxisr.le.tolla.and.dtheta.le.tollt)then
            go to 99
 
         endif
 
13    continue
 
99    return
 
      end
 
 
 
