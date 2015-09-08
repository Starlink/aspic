      subroutine args_cminv (a,b,status)

*+  Invert coordinate transformation matrix 'a' to give 'b'.
*
*   Status returns are those from 'args_minv3'.

      integer status
      real a(2,3),b(2,3),ta(3,3),tb(3,3)

      call args_cmext (0,a,ta)
      call args_minv3 (ta,tb,status)
      if (status.eq.0) then
          call args_cmcur (tb,b)
      endif

      end
