      subroutine per_plotres(res,numf,freq,xl,yl,err,nobs)
 
*
*   This subroutine plots the statistics obtained for this
*   range of frequencies
*
      double precision freq(2),err
      real res(numf),fre(2),yr(2)
 
*
*   First find the range
*
      yr(1)=res(1)
      yr(2)=res(1)
 
      do i=2,numf
 
         if (res(i).lt.yr(1)) yr(1)=res(i)
 
         if (res(i).gt.yr(2)) yr(2)=res(i)
      end do
 
 
*
*   If in S mode and an error estimate is available
*   compute the highest value of the length which is thought
*   to be significant (See Dworetsky op.cit. p921)
*
 
      if (err.gt.0.0) then
         y1 = 0.34*(err-err*err/2.0)*(real(nobs)-sqrt(10.0/err))
         y1 = 1.6 + 1.2*y1
 
*
*   It has to be scaled into the same units as the output
*
         ave = 0.212*real(nobs)
         sig = real(nobs)/37.5
         y1 = (y1-ave)/sig
 
*
*      If this is smaller than the lowest length found, then
*      adjust the Y scaling to allow it to be seen.
*
 
         if (y1.lt.yr(1)) yr(1) = y1
      end if
 
 
*
*   And the frequency range
*
      fre(1)=freq(1)
      fre(2)=freq(2)
 
*
*   Then plot some axes
*
      call jbaxes(fre,2,xl,'Frequency',9,yr,2,yl,'Statistic',9)
 
*
*   and at long last some data
*
 
      do i=1,numf
         x = fre(1) + real(i-1)*(fre(2)-fre(1))/real(numf-1)
         y=res(i)
         call joinpt(x,y)
      end do
 
      call break
 
*
*   Now plot a line indicating an upper limit for acceptable
*   values for this statistic
*
 
      if (err.gt.0.0) then
         call pen(3)
         call join pt (fre(1),y1)
         call join pt(fre(2),y1)
         call pen(1)
      end if
 
 
      end
 
 
 
