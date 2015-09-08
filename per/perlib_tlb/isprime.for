      subroutine per_isprime(n,np)
 
*
*   This subroutine looks to see if N is prime. If it is
*   NP is returned with value N, otherwise with the value 1
*
*   Written by K.F.Hartley at RGO on 13-2-84
*
      nmax=n-1
      ntemp=0
 
*
*   NMAX with value 2 or 3 is of course prime
*
 
      if (nmax.le.2) then
         np=n
         go to 100
 
 
      else
         np=1
      end if
 
 
*
*   Now go through all likely divisors
*
 
      do i=2,nmax
         rat=real(n)/real(i)
         rat=abs(rat-real(int(rat)))
 
         if (rat.le.0.0000001) then
 
*
*      We have a divisor
*
            ntemp=i
         end if
 
      end do
 
 
*
*   If there was a divisor then NTEMP is set to the largest one
*   otherwise it stays at zero
*
 
      if (ntemp.eq.0) then
         np=n
 
      else
         np=1
      end if
 
100   continue
 
      end
 
 
 
