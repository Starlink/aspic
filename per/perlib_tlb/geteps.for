      subroutine per_geteps(y,n,eps)
 
*
*   Finds the range of values in array Y and divides it by
*   1000 to obtain a suggested value for the tolerence to
*   be put on solving the equations involved in filling gaps.
*
*   Written by K.F.Hartley at RGoi on 20-3-84
*
      double precision y(n),ymin,ymax,eps
      ymin=y(1)
      ymax=y(1)
 
      do i=2,n
 
         if (y(i).gt.ymax) ymax=y(i)
 
         if (y(i).lt.ymin) ymin=y(i)
      end do
 
      eps = (ymax-ymin)/1000.0d0
 
      end
 
 
 
