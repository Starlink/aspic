      subroutine per_shove(in,axes,temp,npts)
 
*
*   Stores array IN as part of TEMP
*   NPTS holds the number of samples alrady stored in TEMP
*
      integer axes(2),npts
      double precision in(axes(1),axes(2)),temp(3,100000)
 
      do i=1,axes(2)
         npts = npts + 1
 
         if (npts.gt.100000) then
            call wruser('Work space full - no more please',istat)
            return
         end if
 
         temp(1,npts) = in(1,i)
         temp(2,npts) = in(2,i)
 
         if (axes(1).eq.3) then
            temp(3,npts)=in(3,i)
 
         else
            temp(3,npts)=1.0
         end if
 
      end do
 
 
      end
 
 
 
