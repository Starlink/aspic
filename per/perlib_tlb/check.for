      subroutine per_check(n,m)
 
*
*   This subroutine checks to see if the number of samples
*   found is acceptable to C06FAF. If not it keeps on increasing
*   it by 1 until it is acceptable.
*
*   The strategy is to find the divisors of N, find out if they
*   are prime and then see whether they are larger than 19, which
*   is the largest prime factor that can be handled.
*   (The other condition - that no factor may be repeated more
*    than 20 times is assumed to be irrelevent, as 2**20 is
*    already much larger than any likely dataset.)
*
      character*70 text
      m=n
100   continue
      npmax=1
      nmax=m
 
*
*   Loop around looking for all divisors of M
*
200   continue
      rat=real(m)/real(nmax)
      rat=abs(rat-real(int(rat)))
 
      if (rat.le.0.0000001) then
 
*
*   We have a divisor
*   Now check to see if it is prime
*
         call per_isprime(nmax,np)
 
*
*   NP is 1 if NMAX is not prime
*
 
         if (np.ne.1) then
 
*
*      Update the largest prime found so far
*
 
            if (np.gt.npmax) npmax=np
         end if
 
      end if
 
 
*
*      Try the next smaller divisor
*
      nmax=nmax-1
 
*
*      If we have not reached the end go back for another try
*
 
      if (nmax.le.1) then
         go to 300
 
 
      else
         go to 200
 
      end if
 
300   continue
 
*
*   Having found the largest prime factor, we must test to see
*   if it is less than or equal to 19 - the condition required
*   by C06FAF
*
 
      if (npmax.gt.19) then
         m=m+1
         write (text,900) m
900      format (1h ,'Dataset extended to ',i5,' samples')
         call wruser(text,istat)
         go to 100
 
      end if
 
 
      end
 
 
 
