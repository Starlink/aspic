      subroutine per_fillmm(a,x,y,dim,lx,ly)
 
*
*   Subroutine which extracts epochs and magnitudes from a 2-D
*   array, storing them in two 1-D arrays.
*   At the same time it finds limits of both parameters,
*
*   Written by K.F.Hartley at RGO on 13-1-84
*
      integer dim(2)
      double precision a(dim(1),dim(2))
      real x(dim(2)),y(dim(2))
      real lx(2),ly(2)
      lx(1)=a(1,1)
      lx(2)=lx(1)
      ly(1)=a(2,1)
      ly(2)=ly(1)
 
      do i=1,dim(2)
         x(i)=a(1,i)
         y(i)=a(2,i)
 
         if (x(i).gt.lx(2)) lx(2)=x(i)
 
         if (x(i).lt.lx(1)) lx(1)=x(i)
 
         if (y(i).lt.ly(1)) ly(1)=y(i)
 
         if (y(i).gt.ly(2)) ly(2)=y(i)
      end do
 
 
      end
 
 
 
