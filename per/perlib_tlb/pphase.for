      subroutine per_pphase(ph,y,n,xl,yl)
 
*
*   This subroutine plots the phase diagram for a given frequency
*
*   Written by K.F.Hartley at RGO on 22-2-84
*
      double precision ph(n),y(n)
      real yr(2),pr(2)
 
*
*   First find full range of Y's
*
      yr(1)=y(1)
      yr(2)=y(1)
 
      do i=2,n
 
         if (y(i).gt.yr(2)) yr(2)=y(i)
 
         if (y(i).lt.yr(1)) yr(1)=y(i)
      end do
 
      call rdkeyr('YLIMITS',.true.,2,yr,i,istat)
 
*
*   Allow a little space at the top for a title.
*
      yr(2)=yr(2)+0.1*(yr(2)-yr(1))
 
*
*   Now the phases (note that we plot phases from -0.5 to +1.5
*
      pr(1) = -0.5
      pr(2) = 1.5
 
*
*   Now plot some axes
*
      call jbaxes(pr,2,xl,'PHASE',5,yr,2,yl,'VALUE',5)
 
*
*   and finally the data
*
 
      do i=n/2,n
         xp=ph(i)-1.0
         yp = y(i)
         call  mark pt(xp,yp,3)
      end do
 
 
      do i=1,n
         xp = ph(i)
         yp = y(i)
         call mark pt(xp,yp,3)
      end do
 
 
      do i=1,n/2
         xp = ph(i) + 1.0
         yp = y(i)
         call mark pt(xp,yp,3)
      end do
 
 
      end
 
 
 
