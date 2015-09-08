      subroutine per_countw(data,n,nz)
 
*
*   Counts the number of non-zero values in the third column
*   of the input array - usually the weights column!
*
*   Written by K.F.Hartley at RGO on 23-2-84
*
      double precision data(3,n)
      nz=0
 
      do i=1,n
 
         if (data(3,i).ne.0.0) nz=nz+1
      end do
 
 
      end
 
 
 
