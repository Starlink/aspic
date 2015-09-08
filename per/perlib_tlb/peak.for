      subroutine per_peak(height,n,lims)
 
*
*   Simply finds the min and max of the array height.
*   If the minimum is positive it writes the X value at the
*   maximum; if negative it writes the position of the minimum
*
*   (It is the non-graphic equivalent of PER_FITPAR
*
*   Written by K.F.Hartley at  RGO on 22-2-84
*
      real height(n)
      double precision lims(2)
      character*72 text
 
*
*   First look for location and value of min and max
*
      imin=1
      imax=1
      hmin=height(1)
      hmax=height(1)
 
      do i=2,n
 
         if (height(i).gt.hmax) then
            hmax=height(i)
            imax=i
         end if
 
 
         if (height(i).lt.hmin) then
            hmin=height(i)
            imin=i
         end if
 
      end do
 
 
*
*   If HMIN is positive the mode is L and the intersting point
*   is the location of the MAXIMUM
*
 
      if (hmin.gt.0.0) then
         f=lims(1) + real(imax-1)*(lims(2)-lims(1))/real(n-1)
         p=1.0/f
 
      else
         f=lims(1) + real(imin-1)*(lims(2)-lims(1))/real(n-1)
         p=1.0/f
      end if
 
 
*
*   Write the results to the user and the environment
*
      write (text,900) p,f
900   format ('Period is ',f15.8,' Frequency ',f15.8)
      call wruser(text,istat)
      call wrkeyr('GOODPER',p,1,istat)
      call wrkeyr('GOODFREQ',f,1,istat)
      call cnpar('GOODPER',istat)
      call cnpar('GOODFREQ',istat)
 
      end
 
 
 
