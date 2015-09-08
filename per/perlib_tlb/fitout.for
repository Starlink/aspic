      subroutine per_fitout(out,axes,xs,yapr)
 
*
*   Written by K.F.Hartley at RGO on 1-2-84
*
*   This subroutine writes the results into an output dataset.
*
      integer axes(2)
      double precision out(axes(1),axes(2))
      double precision xs(axes(2)),yapr(axes(2))
 
      do i=1,axes(2)
         out(1,i) = (xs(i))
         out(2,i) = (yapr(i))
         out(3,i)=1.0
      end do
 
 
      end
 
 
 
