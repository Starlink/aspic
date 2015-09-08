      subroutine interp(xx,x,y,n,r)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO INTERPOLATE BETWEEN VALUES IN AN IRREGULARLY SPACED
*       TABLE OF DATA (FOR USE IN INTERPOLATING FILTER DENSITY DATA
*       TO OTHER WAVELENGTHS)
*
*METHOD
*       THE ROUTINE SEARCHES TO FIND WHICH PAIR OF DATA POINTS THE
*       INTERPOLATION POSITION LIES BETWEEN. IF IT LIES OUTSIDE THE
*       KNOWN RANGE, ZERO IS RETURNED (NO EXTRAPOLATION). A POLYNOMIAL
*       IS FITTED TO THE SURROUNDING DATA POINTS, PASSING EXACTLY
*       THROUGH THE TWO ADJACENT POINTS AND WITH WEIGHTS DECREASING
*       WITH DISTANCE AS MORE REMOTE POINTS ARE CONSIDERED. THE NUMBER
*       OF POINTS AND DEGREE OF THE POLYNOMIAL ARE REDUCED NEAR THE
*       ENDS OF THE DATA TABLE.
*
*ARGUMENTS
*       XX (IN)
*       REAL
*               THE POSITION AT WHICH AN INTERPOLATED Y VALUE IS
*               REQUIRED
*       X,Y (IN)
*       REAL(N)
*               THE TABLE OF X,Y POSITIONS (AT LEAST 2) IN INCREASING
*               X ORDER
*       N (IN)
*       INTEGER
*               NUMBER OF X,Y POSITIONS
*       R (OUT)
*       REAL
*               THE INTERPOLATED RESULT
*
*CALLS
*       NAG LIBRARY:
*               E02ADF,E02AEF
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*----------------------------------------------------------------------
*
*
      real x(n),y(n)
 
*
* DEFINE MAX. NUMBER OF POINTS TO BE USED EACH SIDE OF INTERPOLATION
* POSITION
*
      parameter (mxp=4)
 
*
* DEFINE MAX. NUMBER OF FREE PARAMETERS IN LOCAL CURVE FIT
*
      parameter (mxc=4)
      double precision dx(2*mxp),dy(2*mxp),dw(2*mxp),work1(3,2*mxp)
     : ,work2(2,mxc),a(mxc,mxc),s(mxc),xcap,dr
 
*
* CHECK THE ABSCISSA LIES IN THE TABLE
*
 
      if(xx.lt.x(1).or.xx.gt.x(n))then
         r=0.0
 
      else
 
*
* SEARCH TO FIND WHICH DATA POINTS THE ABSCISSA LIES BETWEEN
*
 
         do 1 i=2,n
 
            if(xx.lt.x(i)) go to 2
1        continue
 
2        i=i-1
 
         if(i.eq.n)i=n-1
 
*
* DETERMINE HOW MANY POINTS CAN BE PLACED SYMMETRICALLY AROUND XX
*
         noffs=min(mxp,i,n-i)
 
*
* DETERMINE THE MAX. NUMBER OF POINTS TO BE USED IN ANY DIRECTION
*
 
         do 5 j=1,noffs
 
            if(j.eq.1)then
               w=1.0e10
 
            else
               w=2.0**(1-j)
            endif
 
            k=noffs-j+1
            dx(k)=x(i-j+1)
            dy(k)=y(i-j+1)
            dw(k)=w
            k=noffs+j
            dx(k)=x(i+j)
            dy(k)=y(i+j)
            dw(k)=w
5        continue
 
 
*
* FIT A POLYNOMIAL
*
         nc=min(noffs+1,mxc)
         ifail=1
         call e02adf(k,nc,mxc,dx,dy,dw,work1,work2,a,s,ifail)
 
*
* EVALUATE THE POLYNOMIAL
*
 
         do 66 l=1,nc
            a(l,1)=a(nc,l)
66       continue
 
         ifail=1
         xcap=((dble(xx)-dx(1))-(dx(2*noffs)-dble(xx)))/(dx(2*noffs)
     :    -dx(1))
         call e02aef(nc,a,xcap,dr,ifail)
         r=dr
      endif
 
 
      end
 
 
 
