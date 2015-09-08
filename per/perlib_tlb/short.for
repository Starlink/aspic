      subroutine per_short(ph,y,n,res,nr,ir)
 
*
*   Processes the light curve represented by the phases PH and
*   values Y, computes a smoothness statistic and stores it as
*   the IR'th result in RES
*
*   This version computes the shortest piece of string which could
*   be used to join up the points when the are sorted into order
*   of increasing phase.
*
*   Written by K.F.Hartley at RGO on 14-2-84
*
      double precision ph(n),y(n)
      real res(nr)
      sum=0.0
 
      do i=1,n-1
         dx = real(ph(i+1)) - real(ph(i))
         dy=(y(i+1)-y(i))
         sum = sum + sqrt(dx*dx+dy*dy)
      end do
 
 
*
*   For completeness we must handle the last point!
*
      dx = ph(1)+1.0 - ph(n)
      dy = y(1) - y(n)
      sum = sum + sqrt(dx*dx+dy*dy)
 
*
*   The resulting sum is scaled according to Dworetsky's criteria
*   in terms of the expected length and the expected error
*   (See MNRAS 203,917-924,1983) Equation 6)
*   The result is that values are in terms of sigmas away from the
*   expected value if no signal was present.
*
      esum = 0.212*real(n)
      esigsum = real(n)/37.5
      sum = (sum - esum)/esigsum
 
*
*   Store this result in the array of results
*
      res(ir)=sum
 
      end
 
 
 
