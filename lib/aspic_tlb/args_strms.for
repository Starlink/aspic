      subroutine args_strms (cx1,cy1,sx1,sy1,cx2,cy2,sx2,sy2,m,status)

*+  Set up 2x3 transformation matrix 'm' which will map the rectangle
*   centre '(cx1,cy1)' size '(sx1,sy1)' on to the rectangle '(cx2,cy2)'
*   size '(sx2,sy2)'.
*
*   Status returns are 0 for success and 8 for "size too small".

      integer cx1,cy1,sx1,sy1,cx2,cy2,sx2,sy2,status
      real m(2,3),x0,y0,dx,dy,phi,fx,fy

      if (sx1.lt.2.or.sy1.lt.2.or.sx2.lt.2.or.sy2.lt.2) then
          status = 8
      else

          x0 = cx1 - sx1 / 2
          y0 = cy1 - sy1 / 2
          dx = (cx2 - cx1) - (sx2 / 2 - sx1 / 2)
          dy = (cy2 - cy1) - (sy2 / 2 - sy1 / 2)
          phi = 0.0
          fx = real (sx2 - 1) / (sx1 - 1)
          fy = real (sy2 - 1) / (sy1 - 1)
          call args_strme (x0,y0,dx,dy,phi,fx,fy,m,status)

      endif

      end
