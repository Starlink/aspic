      subroutine per_med(ph,y,n,wrkp,wrky,n2,bw,bs,pa,ya,a,gr,npt)
 
*
*   Given a set of N phases and observations it fits a mean
*   curve through the results and plots it on top of a pre-existing
*   phase diagram.
*
*   The best curve is found by taking the median of overlapping
*   bins - their width specified by BW and the step between them
*   by BS
*
*   Written by K.F.Hartley at RGO on 22-2-84
*
      double precision ph(n),y(n),wrkp(n2),wrky(n2)
      double precision delphi,dphi1,dphi2
      double precision pa(1000),ya(1000),a(1000)
      logical gr
 
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
*   Form the medians
*
      p1=-0.5
      p2=p1+bw
      k=1
      i1=1
      npos=1
 
      do while (p2.lt.1.5)
 
*
*      This outer loop loops through the various phase bins
*
         nval=0
         avep=0.0
 
         do i=i1,nptsf
 
*
*         This inner loop looks for all the points which might
*         contribute to this phase bin.
*
 
            if (wrkp(i).lt.p1) then
 
*
*            To save time we note the last point which does NOT
*            contribute to this bin and start the next loop at
*            that point in the array.
*
               itemp=i
 
            else if (wrkp(i).ge.p1.and.wrkp(i).lt.p2) then
 
*
*            These points do contribute to this bin
*
               nval=nval+1
 
*
*            The amplitudes are store for sorting
*
               a(nval)=wrky(i)
 
*
*            and the average of their phases is calculated
*
               avep=avep+wrkp(i)
 
            else
 
*
*            Because the points are sorted in phase the third
*            alternative is that there are no more points which
*            can contribute to this bin.
*
               go to 100
 
            end if
 
         end do
 
100      continue
 
*
*      We can only procede if some points have contributed to
*      this bin - otherwise nothing is stored and no gap left.
*
 
         if (nval.gt.0) then
            pa(npos)=avep/real(nval)
 
*
*         The amplitudes are sorted into order
*
            call per_getmed(a,nval)
 
            if (mod(nval,2).eq.0) then
 
*
*            If there were an even number of points there is
*            no true median for a small number of samples
*            so average the two central values.
*
               np=nval/2
               ya(npos)=(a(np)+a(np+1))/2.0
 
            else
 
*
*            If there were an odd number take the central one
*            which is their median.
*
               np=(nval+1)/2
               ya(npos)=a(np)
            end if
 
            npos=npos+1
         end if
 
 
*
*      Increment the phase limits for the next bin.
*
         p1=p1+bs
         p2=p1+bw
 
*
*      If there are no more points it is not worth doing another bin.
*
 
         if (itemp.gt.nptsf) then
            i1=itemp
            go to 200
 
         end if
 
      end do
 
200   continue
 
*
*   Plot the complete set of median values
*   (but only if the graphics option was selected.)
*
      npt=npos-1
 
      if (gr) then
         call break
         call pen(3)
 
         do i=1,npt
            call join pt(pa(i),ya(i))
         end do
 
         call pen(1)
      end if
 
 
      end
 
 
 
