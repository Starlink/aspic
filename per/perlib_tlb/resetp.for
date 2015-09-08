      subroutine per_resetp(in,ind,f,p,ifreq,tzero)
 
*
*   This simply resets the phases, which refer to epoch TZ
*   to refer to the mean epoch of this dataset
*
      integer ind(2),ifreq
      double precision in(ind(1),ind(2)),f(20),p(20)
      double precision tmean,delta
 
*
*   First find the mean epoch
*
      tmean = 0.0
 
      do i=1,ind(2)
         tmean = tmean + in(1,i)
      end do
 
      tmean = tmean/dble(ind(2))
 
*
*   Now adjust the phases
*
 
      do i=1,ifreq
 
         if (tmean.gt.tzero) then
            delta = (tmean-tzero)*f(i)
            idel = int(delta)
            delta = delta - idel
            p(i) = p(i)/6.283185d0 + delta
 
            if (p(i).le.0.0) p(i) = p(i) + 1.0
 
            if (p(i).gt.1.0) p(i) = p(i) -1.0
 
         else
            delta = (tzero - tmean)*f(i)
            idel = int(delta)
            delta = delta - idel
            p(i) = p(i)/6.283185d0 - delta
 
            if (p(i).le.0.0) p(i) = p(i) + 1.0
 
            if (p(i).gt.1.0) p(i) = p(i) -1.0
         end if
 
         p(i) = p(i)*6.283185d0
      end do
 
 
      end
 
 
 
