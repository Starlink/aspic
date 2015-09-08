      subroutine per_putfre3(data,af,fknown,c,phi)
 
*
*   Extracts the parameters from an input parameter file and stores
*   them as arrays of known frequencies, amplitudes and phases.
*
      integer af(2)
      double precision data(af(1),af(2)),fknown(20),c(21),phi(20)
      nf=af(2)
 
      do i=1,nf
         fknown(i) = data(1,i)
         c(i+1) = data(2,i)
         phi(i) = data(3,i)
      end do
 
 
      end
 
 
 
