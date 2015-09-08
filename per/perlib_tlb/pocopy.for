      subroutine per_pocopy(in,x,y,n,m)
 
*
*   Copies the data parts of IN into X and Y arrays
*
      double precision in(n,m)
      double precision x(m),y(m)
 
      do i=1,m
         x(i)=in(1,i)
         y(i)=in(2,i)
      end do
 
 
      end
 
 
 
