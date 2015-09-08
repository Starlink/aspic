      subroutine per_make(a,n,m,e,dt)
 
*
*   Makes up a series of equally spaced values and stores them
*   in array A
*
      double precision a(n,m),e,dt
 
      do i=1,m
         a(1,i) = e + dble(i-1)*dt
      end do
 
 
      end
 
 
 
