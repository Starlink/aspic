      subroutine args_curcl

*+  Tidy up after using ARGS cursor (remove cursor and switch off lamps)

      call args_curs('0')
      call args_lamps (0,0,0,0)

      end
