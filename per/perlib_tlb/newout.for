      subroutine per_newout(y,ny,t,dt,out,nout)
 
*
*   Stores the new data in a 2-D array and fills in
*   some epochs and weights.
*
*   Written by K.F.Hartley at RGO on 16-3-84
*
      integer nout(2),ny
      double precision y(ny),out(nout(1),nout(2))
 
      do i=1,ny
         out(1,i) = t + dble(i-1)*dt
         out(2,i) = y(i)
 
         if (nout(1).gt.2) out(3,i)=1.0
      end do
 
 
      end
 
 
 
