      subroutine per_limsort(a,b,nsamp)
 
*
*      Subroutine PER_LIMSORT
*
*      Sorts the array A into increasing order and then
*      sorts array B into the corresponding order, so that
*      pairs of original values remain together.
*      The work arrays are all dimensioned in this subroutine.
*      (cf STORE, where they are dynamically dimensioned)
*
*      The hard work is done by NAG routine M01AJF
*
*      Written by K.F.Hartley at RGO on 23-1-84
*
      double precision a(10000),b(10000),w(10000)
      integer ind(10000),indw(10000)
 
*
*   Use the NAG routine to generate an index array in IND
*   and sort the A's
*
      ifail=0
      call m01ajf(a,w,ind,indw,nsamp,10000,ifail)
 
      if (ifail.ne.0) then
         call wruser('Failed to sort the epochs',istat)
         return
      end if
 
 
*
*   Now sort the B's into the corresponding order
*
 
      do i=1,nsamp
         w(i)=b(i)
      end do
 
 
      do i=1,nsamp
         b(i)=w(ind(i))
      end do
 
 
*
*   Thats all
*
 
      end
 
 
 
