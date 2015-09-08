      subroutine per_spline(ph,y,n,wrkp,wrky,n2,nspl,pa,ya,a,gr)
 
*
*   Given a set of N phases and observations it fits a mean
*   curve through the results and plots it on top of a pre-existing
*   phase diagram.
*
*   The mean curve is found by fitting splines to the raw data.
*
*   Written by K.F.Hartley at RGO on 22-2-84
*
      double precision ph(n),y(n),wrkp(n2),wrky(n2)
      double precision delphi,dphi1,dphi2
      double precision pa(102),ya(102),a(102)
      logical gr
      nb=102
 
*
*      Store phases at which spline is to be evaluated
*
 
      do i=1,102
         pa(i) = dble(i-1)/50.0d0 - 0.5d0
      end do
 
 
*
*   To make it give a smooth fit at the ends we copy two ranges of
*   phases going from -0.5 to 1.5 (and the corresponding Y's)
*
      k=1
 
      do i=1,n
 
         if (ph(i).gt.0.5d0) then
            wrkp(k)=ph(i)-1.0d0
            wrky(k)=y(i)
            k=k+1
         end if
 
      end do
 
 
      do i=1,n
         wrkp(k)=ph(i)
         wrky(k)=y(i)
         k=k+1
      end do
 
 
      do i=1,n
 
         if (ph(i).lt.0.5d0) then
            wrkp(k)=ph(i)+1.0d0
            wrky(k)=y(i)
            k=k+1
         end if
 
      end do
 
      nptsf=k-1
 
*
*     The routine fits NSPL splines, storing 102 computed points
*     at equal phase intervals.
*
      call per_bells(wrkp,wrky,nptsf,-0.5d0,1.5d0,102,nspl,pa,ya,ier)
 
      if (ier.ne.1) then
         call wruser('Spline fitting failed',istat)
         return
      end if
 
 
*
*   and plot the complete set of average values
*
 
      if (gr) then
         call break
         call pen(3)
 
         do i=1,nb
            call join pt(pa(i),ya(i))
         end do
 
         call pen(1)
      end if
 
 
      end
 
 
 
