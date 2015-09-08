      subroutine per_winmm(a,x,y,dim,lx,ly,ncent)
 
*
*   This subroutine rearranges the window function so that it
*   can be conveniently displayed at the same time as a power
*   spectrum.
*
*   In particular it assumes that it was sample at the
*   same frequencies as the power spectrum, resales it to the same
*   maximum as the power spectrum and reflects it about the centre,
*   storing it at the centre of the frequency array.
*   (this means that the frequencies are "wrong", but the result is
*   visible!)
*
*   Written by K.F.Hartley at RGO on 5-2-84
*
      integer dim(2)
      double precision a(dim(1),dim(2))
      real x(dim(2)),y(dim(2))
      real lx(2),ly(2)
 
*
*   First find the maximum value in the window function
*
      amax=a(2,1)
 
      do i=2,dim(2)
 
         if (a(2,i).gt.amax) amax=a(2,i)
      end do
 
 
*
*   Set up a scale factor
*   LY(2) is the peak intensity of the power spectrum, but it
*   has been modified to allow room for a title, so the factor
*   of 1.15 must be used
*
      factor=ly(2)/(1.15*amax)
 
*
*   Then zeroize the Y array
*
 
      do i=1,dim(2)
         y(i)=0.0
      end do
 
 
*
*   Now do the re-organization
*
 
      do i=1,dim(2)-1
         k1=ncent+i
         k2=ncent-i
 
         if (k1.le.dim(2)) then
            y(k1)=a(2,i+1)*factor
         end if
 
 
         if (k2.ge.1) then
            y(k2)=a(2,i+1)*factor
         end if
 
      end do
 
 
*
*   The central value (frequency=0) is not repeated
*
      y(ncent)=a(2,1)*factor
 
      end
 
 
 
