      subroutine per_mean(ph,y,n,nb,pa,ya,a)
 
*
*   Given a set of N phases and observations it fits a mean
*   curve through the results and plots it on top of a pre-existing
*   phase diagram.
*
*   Written by K.F.Hartley at RGO on 22-2-84
*
      double precision ph(n),y(n)
      real pa(nb),ya(nb),a(nb)
 
*
*   NB is large enough to store an extra value as first and last
*   entry, to help with later interpolation.
*
*   Consequently NBIN is the actual number of phase bins.
*
      nbin = nb-2
 
*
*   First zeroize the arrays
*
 
      do i=1,nb
         pa(i)=0.0
         ya(i)=0.0
         a(i)=0.0
      end do
 
 
*
*   Now form the sums of the values in each phase bin
*
 
      do i=1,n
 
         if (ph(i).ge.1.0) ph(i) = ph(i)-1.0
         k = int(ph(i)*dble(nbin)) + 2
         pa(k) = pa(k) + ph(i)
         ya(k) = ya(k) + y(i)
         a(k)  = a(k)  + 1.0
      end do
 
 
*
*   Now form the averages
*
 
      do i=2,nb-1
 
         if (a(i).ne.0.0) then
            pa(i)=pa(i)/a(i)
            ya(i)=ya(i)/a(i)
 
         else
            pa(i)= real(i-2)/real(nb-3)+0.05
            ya(i)=0.0
         end if
 
      end do
 
 
*
*   Now sort out the ends
*
      pa(1) = pa(nb-1)-1.0
      ya(1) = ya(nb-1)
      pa(nb) = pa(2) + 1.0
      ya(nb) = ya(2)
 
*
*   and plot the complete set of average values
*
      call break
      call pen(3)
 
*
*   First the 'negative' phases
*
 
      do i=2,nb-1
         phase = pa(i)-1.0
 
         if (phase.ge.(-0.5)) then
            call join pt(phase,ya(i))
         end if
 
      end do
 
 
*
*   Then the main curve
*
 
      do i=1,nb
         call join pt(pa(i),ya(i))
      end do
 
 
*
*   Finally the phases greater than 1.0
*
 
      do i=2,nb
         phase=pa(i)+1.0
 
         if (phase.lt.1.5) then
            call join pt(phase,ya(i))
         end if
 
      end do
 
      call pen(1)
 
      end
 
 
 
