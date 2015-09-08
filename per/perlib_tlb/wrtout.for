      subroutine per_wrtout(a,n,m,idev)
 
*
*   This subroutine writes two columns of array A to the
*   device specified by IDEV
*
*   Written by K.F.Hartley at RGO on 24-1-84
*
      double precision a(n,m)
 
      do i=1,m
         write (idev,*) a(1,i),a(2,i)
      end do
 
 
      end
 
 
 
