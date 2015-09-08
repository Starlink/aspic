      subroutine per_phaze(in,n,x,y,f,yr,nval,ze)
 
*
*   Computes phases of input data and stores the results in X and Y
*
      double precision in(3,n),yr(2),f,ze
      real x(n),y(n)
      yr(1)=in(2,1)
      yr(2)=yr(1)
      k=1
 
      do i=1,n
 
*        IF (IN(3,I).NE.0.0) THEN
 
         if (in(2,i).lt.yr(1)) yr(1)=in(2,i)
 
         if (in(2,i).gt.yr(2)) yr(2)=in(2,i)
         y(k)=in(2,i)
         x(k)=(in(1,i)-ze)*f
         x(k)=x(k)-real(int(x(k)))
 
         if (x(k).lt.0.0) x(k)=x(k)+1.0
 
         if (x(k).gt.1.0) x(k)=x(k)-1.0
         k=k+1
 
*        END IF
      end do
 
      nval=k-1
 
      end
 
 
 
