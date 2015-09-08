      subroutine args_cmmul (a,b,c)

*+  Multiply coordinate transformation matrices 'a' and 'b' to give 'c'.

      real a(2,3),b(2,3),c(2,3),ta(3,3),tb(3,3),tc(3,3)

      call args_cmext (0,a,ta)
      call args_cmext (0,b,tb)
      call args_mmul (3,3,3,ta,tb,tc)
      call args_cmcur (tc,c)

      end
