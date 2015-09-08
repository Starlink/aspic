      subroutine xyfitr(xa,ya,xb,yb,ok,n,maxit,gamma,ifit,c,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIND A LEAST-SQUARES LINEAR TRANSFORMATION BETWEEN TWO SETS
*       OF X,Y POSITIONS, REJECTING ERRONEOUS DATA.
*
*METHOD
*       CALL LINTRN TO OBTAIN A LEAST-SQUARES FIT. CALCULATE THE RMS
*       MIS-ALIGNMENT AND THE MOST MIS-ALIGNED DATA POINT. REJECT THE
*       WORST POINT IF IT EXCEEDS A THRESHOLD BASED ON THE RMS ERROR.
*       REPEAT FOR A PRESET NUMBER OF ITERATIONS, OR UNTIL NO FURTHER
*       POINTS ARE REJECTED
*
*ARGUMENTS
*       XA,YA (IN)
*       REAL(N)
*               THE FIRST SET OF POSITIONS TO BE TRANSFORMED
*       XB,YB (IN)
*       REAL(N)
*               THE SECOND SET OF REFERENCE POSITIONS
*       OK (IN/OUT)
*       LOGICAL(N)
*               FLAGS TO INDICATE WHICH DATA POINTS HAVE NOT BEEN
*               REJECTED
*       N (IN)
*       INTEGER
*               NUMBER OF DATA POINTS
*       MAXIT (IN)
*       INTEGER
*               MAX. NUMBER OF REJECTION ITERATIONS
*       GAMMA (IN)
*       REAL
*               NUMBER OF STANDARD DEVIATIONS AT WHICH ABERRANT POINTS
*               ARE REJECTED
*       IFIT (IN/OUT)
*       INTEGER
*               FLAG WHICH INDICATES THE TYPE OF LINEAR TRANSFORMATION
*               USED... SERVES THE SAME PURPOSE AS IN LINTRN
*       C (OUT)
*       REAL(6)
*               THE COEFFICIENTS DEFINING THE TRANSFORMATION
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*CALLS
*       THIS PACKAGE:
*               LINTRN
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      real xa(n),ya(n),xb(n),yb(n),c(6)
      logical ok(n),reject
 
*
* INITIALLISE ITERATION COUNT AND LOOP WHILE POINTS WERE REJECTED LAST
* ITERATION
*
      iter=0
      reject=.true.
 
63    if(reject) then
 
*
* CALL LINTRN TO FIND THE LINEAR TRANSFORMATION BETWEEN THE 2 SETS
* OF X,Y POSITIONS
*
         call lintrn(xa,ya,xb,yb,ok,n,c,ifit,ierr)
 
*
* QUIT IF ERROR, OR MAX ITERATIONS EXCEEDED
*
 
         if((ierr.eq.0).and.(iter.lt.maxit)) then
 
*
* COUNT 1 ITERATION AND INITIALLISE REJECTED POINTS FLAG
*
            iter=iter+1
            reject=.false.
 
*
* INITIALLISE COUNTERS FOR FINDING STANDARD DEVIATION AND MAXIMUM
* MIS-ALIGNMENT
*
            varsum=0.0
            ngood=0
            errmax=-1.0
 
*
* SCAN ALL POINTS NOT REJECTED SO FAR
*
 
            do 2 i=1,n
 
               if(ok(i)) then
 
*
* CALCULATE THE MIS-ALIGNMENT
*
                  xd=c(1)+c(2)*xa(i)+c(3)*ya(i)
                  yd=c(4)+c(5)*xa(i)+c(6)*ya(i)
                  errsq=(xb(i)-xd)**2+(yb(i)-yd)**2
                  varsum=varsum+errsq
                  ngood=ngood+1
 
*
* FIND THE MAXIMUM MIS-ALIGNMENT
*
 
                  if(errsq.gt.errmax) then
                     errmax=errsq
                     imax=i
                  endif
 
               endif
 
2           continue
 
 
*
* FIND THE RMS MIS-ALIGNMENT AND REJECT THE WORST POINT IF IT IS MORE
* THAN GAMMA STD. DEVS. OUT
*
 
            if(ngood.gt.0) then
               var=varsum/ngood
 
               if((var.gt.0.0).and.(errmax.gt.var*(gamma**2))) then
                  ok(imax)=.false.
                  reject=.true.
               endif
 
            endif
 
            go to 63
 
         endif
 
      endif
 
      return
 
      end
 
 
 
