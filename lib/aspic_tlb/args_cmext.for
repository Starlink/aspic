      subroutine args_cmext (flag,a,b)

*+  Extend 2x3 coordinate transformation matrix 'a' to 3x3 by adding
*   a bottom row, which is "0 0 1" (flag = 0) or "1 1 1" (flag <> 0)

      integer flag,i
      real a(2,3),b(3,3)

      do i=1,3
          b(1,i) = a(1,i)
          b(2,i) = a(2,i)
          if (i.lt.3.and.flag.eq.0) then
              b(3,i) = 0.0
          else
              b(3,i) = 1.0
          endif
      enddo

      end
