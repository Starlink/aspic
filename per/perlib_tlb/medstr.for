      subroutine per_medstr(im,nd,p,y,npts)
 
*
*   This creates a 3xN image from a set of phases (P) and
*   amplitudes (Y) by using linear interpolation.
*
      integer nd(2),npts
      double precision im(nd(1),nd(2)),p(npts),y(npts)
      double precision ph
      nout=nd(2)
 
      do i=1,nout
         im(1,i) = dble(i-1)/dble(nout-1)
         im(3,i)=1.0
         ph=im(1,i)
 
         do k=1,npts
 
            if (ph.ge.p(k).and.ph.lt.p(k+1)) then
               k1=k
               k2=k+1
               go to 100
 
            end if
 
         end do
 
100      continue
         im(2,i) = y(k1) + (y(k2)-y(k1))*(ph-p(k1))/(p(k2)-p(k1))
      end do
 
 
      end
 
 
 
