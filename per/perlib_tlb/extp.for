      subroutine per_extp(in,axin,out,axou,gam,m,ifr,isc)
 
*
*   Uses data in IN to extrapolate it to fill OUT
*   The limits on th enew data are defined by IFR to ISC
*   and the extrapolationis done using the coefficients in GAM
*
*   Written by K.F.Hartley at RGO on 17-1-84
*
      integer axin(2),axou(2),m,ifr,isc
      double precision in(axin(1),axin(2)),out(axou(1),axou(2)),gam(m)
 
*
*   First set total number of samples
      nsamp=axou(2)
 
*
*   and the sampling interval
*
      deltat = (in(1,axin(2))-in(1,1)) / dble(axin(2)-1)
 
*
*   There are two cases - when the new data comes before
*   the old or after. IFR helps decide which!
*
 
      if (ifr.eq.1) then
 
*
*      The new data come before the old ones.
*
*      First fill the output with the good values
*
 
         do i=isc+1,nsamp
            out(1,i)=in(1,i-isc)
            out(2,i)=in(2,i-isc)
            out(3,i)=1.0
         end do
 
 
*
*      Now extrapolate to the first part of the output
*
 
         do i=isc,ifr,-1
            out(1,i) = out(1,i+1)-deltat
            out(2,i)=0.0
 
            do k=1,m
               out(2,i)=out(2,i) + gam(k)*out(2,i+k)
            end do
 
            out(3,i)=0.1
         end do
 
 
      else
 
*
*      Handle the case where the new data follows the old
*
*      Again fill the output with the input
*
 
         do i=1,ifr-1
            out(1,i)=in(1,i)
            out(2,i)=in(2,i)
            out(3,i)=1.0
         end do
 
 
*
*      and do the forward extrapolation
*
 
         do i=ifr,isc
            out(1,i) = out(1,i-1) + deltat
            out(2,i) = 0.0
 
            do k=1,m
               out(2,i) = out(2,i) + gam(k)*out(2,i-k)
            end do
 
            out(3,i)=0.1
         end do
 
      end if
 
 
      end
 
 
 
