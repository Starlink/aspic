      subroutine per_copyy(in,y,n,m,ny,t)
 
*
*   Copies the intensities from IN into Y
*
      double precision in(n,m)
      double precision y(ny),t
 
      do i=1,m
         y(i)=in(2,i)
      end do
 
 
*
*   If NY is greater than M then fill with zeros
*
 
      do i=m+1,ny
         y(i)=0.0
      end do
 
 
*
*   Need the full range of epochs covered, to find
*   out the frequencies at which the Power Spectrum is calculated
*
      t=in(1,m)-in(1,1)
      t=t*real(ny-1)/real(m-1)
 
      end
 
 
 
