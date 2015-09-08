      subroutine per_copout(out,axes,temp)
 
*
*   Copies TEMP into OUT
*
      integer axes(2)
      double precision out(axes(1),axes(2)),temp(axes(1),axes(2))
 
      do i=1,axes(2)
 
         do j=1,axes(1)
            out(j,i)=temp(j,i)
         end do
 
      end do
 
 
      end
 
 
 
