      subroutine per_credat(x,y,nval,f,c,p,nf,epfl,epoch,ep,freq)
 
*
*   This creates a set of X and Y from fitted parameters stored
*   in arrays F C and P. If required, th eepochs are converted
*   into phases.
*
*   Written by K.F.Hartley at RGO on 1-Mar-1984
*
      real x(nval),y(nval)
      double precision f(20),c(21),p(20),ep(2)
      double precision epoch,freq,delt,t
      logical epfl
 
*
*   First decide on the increment in epoch
*
      delt = (ep(2)-ep(1))/dble(nval-1)
 
      do i=1,nval
 
         if (epfl) then
 
*
*         Create a suitable set of epochs
*
            x(i) = ep(1) + dble(i-1)*delt
            t = (dble(x(i))-epoch)*6.283185d0
 
         else
 
*
*         or a set of phases
*
            x(i) = real(i-1)/real(nval-1)
            t = (dble(x(i))/freq +ep(1) - epoch)*6.283185d0
         end if
 
         y(i) = c(1)
 
         do j = 1,nf
            y(i) = y(i) + c(j+1)*sin(t*f(j)+p(j))
         end do
 
      end do
 
 
      end
 
 
 
