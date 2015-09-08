      subroutine per_phasez(x,ph,n,f,ze)
 
*
*   Computes the phases of each of a set of epochs (X) for
*   a given frequency F, using ZE as the zero point.
*
      double precision x(n),ph(n),f,temp,ze
 
      do i=1,n
         temp = (x(i)-ze)*f
         temp = temp - dble(int(temp))
         ph(i) = temp
 
         if (ph(i).lt.0.0) ph(i)=ph(i)+1.0d0
 
         if (ph(i).gt.1.0d0) ph(i)=ph(i)-1.0d0
      end do
 
 
      end
 
 
 
