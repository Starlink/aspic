      subroutine args_cmid (a)

*+  Set identity into coordinate transformation matrix 'a'

      real a(2,3)

      a(1,1) = 1.0
      a(1,2) = 0.0
      a(1,3) = 0.0
      a(2,1) = 0.0
      a(2,2) = 1.0
      a(2,3) = 0.0

      end
