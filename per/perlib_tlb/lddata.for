      subroutine per_lddata(in,axes,y,np)
 
*
*   Simply the values from a 2-d array into a 1-d array
*   ignoring the epochs
*
      integer axes(2)
      double precision in(axes(1),axes(2))
      double precision y(np)
 
      do i=1,axes(2)
         y(i) = dble(in(2,i))
      end do
 
 
      end
 
 
 
