      subroutine args_cmcur (a,b)

*+  Curtail 3x3 matrix 'a' to become a 2x3 coordinate transformation matrix
*   by removing the bottom row.

      integer i
      real a(3,3),b(2,3)

      do i=1,3
          b(1,i) = a(1,i)
          b(2,i) = a(2,i)
      enddo

      end
