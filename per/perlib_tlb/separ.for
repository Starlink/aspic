      subroutine per_separ(in,x,y,ndim)
 
*
*   The usual splitting routine
*
      integer ndim(2)
      double precision in(ndim(1),ndim(2)),x(ndim(2)),y(ndim(2))
 
      do i=1,ndim(2)
         x(i)=in(1,i)
         y(i)=in(2,i)
      end do
 
 
      end
 
 
 
