      subroutine per_phases(x,ph,n,f)
 
*
*   Computes the phases of each of a set of epochs (X) for
*   a given frequency F
*
      double precision x(n),ph(n),f,temp
 
      do i=1,n
         temp = (x(i)-x(1))*f
         temp = temp - dble(int(temp))
         ph(i) = temp
      end do
 
 
      end
 
 
 
