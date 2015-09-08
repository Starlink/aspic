      subroutine per_long(ph,y,n,res,nr,ir)
 
*
*   Processes the light curve represented by the phases PH and
*   values Y, computes a smoothness statistic and stores it as
*   the IR'th result in RES
*
*   This version bins the points into 11 bins of equal width in phase
*   forms the average and then computes the length of the average
*   light curve.
*
*   Written by K.F.Hartley at RGO on 14-2-84
*
      double precision ph(n),y(n)
      real pbin(11),ybin(11),numbin(11)
      real res(nr)
 
*
*   First clear the bins
*
 
      do i=1,11
         pbin(i)=0.0
         ybin(i)=0.0
         numbin(i)=0.0
      end do
 
 
*
*   Then fill them with the data
*
 
      do i=1,n
         k=int(ph(i)*10.0) + 1
         pbin(k) = pbin(k) + ph(i)
         ybin(k) = ybin(k) + y(i)
         numbin(k) = numbin(k) + 1.0
      end do
 
 
*
*   The last bin is different because it represents only
*   points with phase of 1.0 exactly
*   so add those from the first bin as well
*
      numbin(11) = numbin(11) + numbin(1)
 
*
*   The phases have to be increased by 1.0 to make this a sensible
*   thing to do.
*
      pbin(11) = pbin(11) + pbin(1) + numbin(1)
      ybin(11) = ybin(11) + ybin(1)
 
*
*   and form the averages for each bin
*
 
      do i=1,11
 
         if (numbin(i).gt.0.0) then
            pbin(i)=pbin(i)/numbin(i)
            ybin(i)=ybin(i)/numbin(i)
 
         else
 
*
*      If there were no real smaples there put in a zero amplitude
*      at the central phase for that bin.
*
            pbin(i)=real(i-1)*0.1+0.05
            ybin(i)=0.0
         end if
 
      end do
 
 
*
*   Now compute the length of the mean light curve
*
      sum=0.0
 
      do i=1,10
         dx = pbin(i+1)-pbin(i)
         dy = ybin(i+1)-ybin(i)
         sum = sum + sqrt(dx*dx+dy*dy)
      end do
 
 
*
*   Store this result in the array of results
*
      res(ir)=sum
 
      end
 
 
 
