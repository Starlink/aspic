      subroutine per_putfre(data,af,fknown)
 
*
*   Extracts the frequencies from an input parameter file and stores
*   them as an array of known frequencies.
*
      integer af(2)
      double precision data(af(1),af(2)),fknown(20)
      nf=af(2)
 
      do i=1,nf
         fknown(i) = data(1,i)
      end do
 
 
      end
 
 
 
