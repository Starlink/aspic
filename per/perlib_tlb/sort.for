      subroutine per_sort(ph,y,w,ind,indw,x,n)
 
*
*   Sorts the phase array PH, storing an index array in IND
*   and uses these indices to sort the Y values in the same order
*
      double precision ph(n),y(n),w(n),x(n)
      integer ind(n),indw(n)
 
*
*   Use NAG routine M01AJF to sort the phases
*
      ifail=0
      call m01ajf(ph,w,ind,indw,n,n,ifail)
 
      if (ifail.ne.0) then
         call wruser('Sorting of phases failed',istat)
      end if
 
 
*
*   Now use the index array to sort the Y values
*
 
      do i=1,n
         w(i)=y(i)
      end do
 
 
      do i=1,n
         y(i) = w(ind(i))
      end do
 
 
*
*   Note that the epochs must be sorted to match the sorted Y values!
*
 
      do i=1,n
         w(i)=x(i)
      end do
 
 
      do i=1,n
         x(i)=w(ind(i))
      end do
 
 
      end
 
 
 
