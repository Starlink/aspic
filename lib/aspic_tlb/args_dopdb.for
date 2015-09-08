      subroutine args_dopdb (lognam , status)

*+  Translate 'lognam' and open the ARGS database. Status returns are 0 for
*   success and 2 for failure. If the translation fails then the literal
*   value of 'lognam' is used.

      integer status
      character lognam*(*) , device*11

      call args_trlog (lognam , device , status)
      call args_opdb (device , status)

      end
