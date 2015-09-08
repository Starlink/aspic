      subroutine args_strme (x0,y0,dx,dy,phi,fx,fy,m,status)

*+  Set up 2x3 transformation matrix 'm' corresponding to elemental
*   transformations :-
*
*   1  scale with factors '(fx,fy)' about '(x0,y0)'
*   2  rotation of 'phi' radians anticlockwise about '(x0,y0)'
*   3  shift through '(dx,dy)'
*
*   Status return can only be 0 (success).

      integer status
      real x0,y0,dx,dy,phi,fx,fy,m(2,3),c,s,fxc,fxs,fyc,fys

      c = cos(phi)
      s = sin(phi)
      fxc = fx * c
      fxs = fx * s
      fyc = fy * c
      fys = fy * s

      m(1,1) = fxc
      m(1,2) = -fys
      m(1,3) = -x0 * fxc + y0 * fys + x0 + dx
      m(2,1) = fxs
      m(2,2) = fyc
      m(2,3) = -x0 * fxs - y0 * fyc + y0 + dy

      status = 0

      end
