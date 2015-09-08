      subroutine per_parext(in,ax,f,a,p)
 
*
*   This routine extracts the parameters from an input set
*   and stores them as if they had been typed at the keyboard.
*
*   Written by K.F.Hartley at RGO on 24-2-84
*
      integer ax(2)
      double precision in(ax(1),ax(2))
      real f(ax(2)),a(ax(2)),p(ax(2))
 
      do i=1,ax(2)
         f(i) = in(1,i)
         a(i) = in(2,i)
         p(i) = in(3,i)
      end do
 
 
      end
 
 
 
