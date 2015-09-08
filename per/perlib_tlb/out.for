      subroutine per_out(x,y,out,ax)
 
*
*   Creates an output dataset from two 1-D arrays
*
      integer ax(2)
      double precision x(ax(2)),y(ax(2)),out(ax(1),ax(2))
 
      do i=1,ax(2)
         out(1,i) = x(i)
         out(2,i) = y(i)
 
         if (ax(1).ge.3) out(3,i) = 1.0
      end do
 
 
      end
 
 
 
