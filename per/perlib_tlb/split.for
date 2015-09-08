      subroutine per_split(work,n,x,y,ep,yr,nval)
 
*
*   This takes a 2-D (DP) array WORK and splits it into
*   two 1-D (REAL) arrays, only storing the values with
*   non-zero-weights in the range of epochs specified by EP.
*   NVAL stores the number of points actaully stored.
*
      double precision work(3,n),ep(2),yr(2)
      double precision time,weight
      real x(n),y(n)
      k=1
 
      do i=1,n
         time=work(1,i)
         weight=work(3,i)
 
         if (time.ge.ep(1).and.time.le.ep(2).and.weight.ne.0.0) then
            x(k)=work(1,i)
            y(k)=work(2,i)
            k=k+1
         end if
 
      end do
 
      nval=k-1
 
*
*   Now look for the range of Y
*
      yr(1)=y(1)
      yr(2)=yr(1)
 
      do i=1,nval
 
         if (y(i).lt.yr(1)) yr(1)=y(i)
 
         if (y(i).gt.yr(2)) yr(2)=y(i)
      end do
 
 
      end
 
 
 
