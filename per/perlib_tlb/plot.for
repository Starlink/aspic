      subroutine per_plot(x,y,n,ep,yr,xl,yl)
 
*
*   This plots array Y against X on axes of length
*   XL and YL cms. on an already opend device
*   The limits of the two axes are held in EP (X) and YR (Y)
*
      real x(n),y(n),xlim(2),ylim(2)
      double precision ep(2),yr(2)
 
*
*   First conver the limits into single precision for SIMPLEPLOT
*
 
      do i=1,2
         xlim(i)=ep(i)
         ylim(i)=yr(i)
      end do
 
 
*
*   Now plot the axes
*
      call jbaxes(xlim,2,xl,'EPOCH',5,ylim,2,yl,'VALUE',5)
 
*
*   and the data as crosses
*
 
      do i=1,n
         call mark pt(x(i),y(i),3)
      end do
 
 
      end
 
 
 
