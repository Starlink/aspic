      subroutine args_cmapp (a,bx,by,cx,cy)

*+  Apply coordinate transformation 'a' to '(bx,by)' to give 'cx,cy)'.

      real a(2,3),bx,by,cx,cy,ta(3,3),bvec(3),cvec(3)

      call args_cmext (0,a,ta)
      bvec(1) = bx
      bvec(2) = by
      bvec(3) = 1.0
      call args_mmul (3,3,1,ta,bvec,cvec)
      cx = cvec(1)
      cy = cvec(2)

      end
