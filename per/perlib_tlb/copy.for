      subroutine per_copy(in,work,n,m,ep)
 
*
*   Simply copies a 2-D array from IN to WORK, at the
*   same time ensuring that WORK has weights and
*   storing the range of values in the first column.
*
*   (In the case of period finding datasets this will be
*   the range of epochs)
*
*   Written by K.F.Hartley at RGO on 26-1-84
*
      double precision in(n,m),work(3,m),ep(2)
      ep(1)=in(1,1)
      ep(2)=ep(1)
 
*
*   Now loop through all the entries
*
 
      do i=1,m
         work(1,i)=in(1,i)
         work(2,i)=in(2,i)
 
         if (n.eq.3) then
            work(3,i)=in(3,i)
 
         else
            work(3,i)=1.0
         end if
 
 
         if (work(1,i).lt.ep(1)) ep(1)=work(1,i)
 
         if (work(1,i).gt.ep(2)) ep(2)=work(1,i)
      end do
 
 
      end
 
 
 
