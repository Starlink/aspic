      subroutine per_getnum(in,axes,lims,npts)
 
*
*   Counts the number of samples lying between two epoch
*   limits which have non-zero weight
*
*   Written by K.F.Hartley at RGO on 1-2-84
*
      integer axes(2),npts
      double precision    lims(2)
      double precision in(axes(1),axes(2))
      npts = 0
 
      do i=1,axes(2)
 
         if(in(1,i).ge.lims(1).and.in(1,i).le.lims(2))  then
 
*
*         Check if weights are present
*
 
            if (axes(1).eq.3) then
 
*
*            If they are then only count this as a valid
*            point if it has non-zero weight
*
 
               if (in(3,i).ne.0.0) then
                  npts=npts+1
               end if
 
 
            else
 
*
*            If there are no weights - just count it anyway
*
               npts=npts+1
            end if
 
         end if
 
      end do
 
 
      end
 
 
 
