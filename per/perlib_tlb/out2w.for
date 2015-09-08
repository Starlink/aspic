      subroutine per_out2w(in,n,out,m)
 
*
*   This subroutine simply copies IN to OUT
*   but only those with non-zero weights
*
      double precision in(3,n),out(3,m)
      k=0
 
      do j=1,n
 
         if (in(3,j).ne.0.0) then
            k=k+1
 
            do i=1,3
               out(i,k)=in(i,j)
            end do
 
         end if
 
      end do
 
 
      end
 
 
 
