      subroutine per_out2(in,n,out,m)
 
*
*   This subroutine simply copies IN to OUT
*   retaining only the samples with non-zero weights
*
      double precision in(3,n),out(3,m)
      jo=0
 
      do j=1,n
 
         if (in(3,j).ne.0.0) then
            jo=jo+1
 
            do i=1,3
               out(i,jo)=in(i,j)
            end do
 
         end if
 
      end do
 
 
      end
 
 
 
