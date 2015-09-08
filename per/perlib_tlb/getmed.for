      subroutine per_getmed(in,n)
 
*
*   Given an input array of N numbers (IN) it
*   soert them into descending order.
*
*   Written by K.F.Hartley at RGO on 8/6/84
*
      double precision in(n),temp
      nw = (n+1)/2
 
      do i=1,n
 
         do j=i+1,n
 
            if (in(j).gt.in(i)) then
               temp=in(i)
               in(i)=in(j)
               in(j)=temp
            end if
 
         end do
 
      end do
 
 
      end
 
 
 
