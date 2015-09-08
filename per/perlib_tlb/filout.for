      subroutine per_filout(out,nax,yf,npt,np,t,dt)
 
*
*   Fills th eoutput array from the input array, making up
*   suitable epochs as it goes and giving the newly created
*   values a low weight.
*
*   Written by K.F.Hartley at RGO on 2-Mar-1984
*
      integer nax(2)
      double precision out(nax(1),nax(2)),t(4)
      double precision dt
      integer np(4)
      double precision yf(npt)
      out(1,1) = t(1)
      out(2,1) = yf(1)
      out(3,1)=1.0
 
      do i=2,nax(2)
         out(1,i)= out(1,i-1) + dt
         out(2,i)= yf(i)
 
         if (i.gt.np(2).and.i.lt.np(3)) then
            out (3,i)=0.1
 
         else
            out(3,i)=1.0
         end if
 
      end do
 
 
      end
 
 
 
