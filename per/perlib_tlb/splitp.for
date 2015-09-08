      subroutine per_splitp(in,x,y,ndim,range,err)
 
*
*   The usual splitting routine
*   with some re-scaling as recommended by Dworetsky (op.cit.)
*   and setting some parameters needed elsewhere.
*
      integer ndim(2)
      double precision in(ndim(1),ndim(2)),x(ndim(2)),y(ndim(2))
      double precision range,min,max,err
      min=in(2,1)
      max=in(2,1)
 
      do i=1,ndim(2)
         x(i)=in(1,i)
         y(i)=in(2,i)
 
         if (y(i).gt.max) max = y(i)
 
         if (y(i).lt.min) min = y(i)
      end do
 
 
*
*   Now rescale the Y values according to Dworetsky's criterion.
*
 
      do i=1,ndim(2)
         y(i) = (y(i)-min)/(2.0*(max-min)) - 0.25
      end do
 
 
*
*   The input error estimate must also be scaled
*
      err = err/(2.0*(max-min))
 
*
*   The full range of epochs is used elsewhere
*
      range = x(ndim(2))-x(1)
 
      end
 
 
 
