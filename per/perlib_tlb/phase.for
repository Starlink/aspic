      subroutine per_phase(in,n,x,y,f,yr,nval)
 
*
*   Computes phases of input data and stores the results in X and Y
*
      double precision in(3,n),yr(2),f
      real x(n),y(n)
      yr(1)=in(2,1)
      yr(2)=yr(1)
      k=0
 
      do i=1,n
 
         if (in(3,i).ne.0.0) then
 
            if (in(2,i).lt.yr(1)) yr(1)=in(2,i)
 
            if (in(2,i).gt.yr(2)) yr(2)=in(2,i)
            y(k)=in(2,i)
            x(k)=(in(1,i)-in(1,1))*f
            x(k)=x(k)-real(int(x(k)))
 
            if (x(k).lt.0.0) x(k)=x(k)+1.0
 
            if (x(k).gt.1.0) x(k)=x(k)-1.0
            k=k+1
         end if
 
      end do
 
      nval=k
 
*
*   Now sort into order of increasing phase
*
 
      do i=1,nval-1
 
         do j=i+1,nval
 
            if (x(j).lt.x(i)) then
               temp=x(i)
               x(i)=x(j)
               x(j)=temp
               temp=y(i)
               y(i)=y(j)
               y(j)=temp
            end if
 
         end do
 
      end do
 
 
      end
 
 
 
