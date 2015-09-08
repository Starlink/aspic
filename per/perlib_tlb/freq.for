      subroutine per_freq(f,n,flim)
 
*
*   This subroutine creates an array of frequencies from
*   the two limits and the number of frequencies required.
*
      double precision df,flim(2)
      real f(n)
      df = (flim(2)-flim(1))/dble(n-1)
 
      do i=1, n
         f(i) = flim(1) + dble(i-1)*df
      end do
 
 
      end
 
 
 
