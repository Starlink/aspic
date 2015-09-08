      subroutine args_atou (id,ax,ay,ux,uy,status)

*+  For record 'id' convert the ARGS coordinates '(ax,ay)' to user coord-
*   inates '(ux,uy)'.
*
*   Status returns are those from 'args_atoun' and thus 'args_tdb7p'
*   or 'args_qdbf'.

      integer id,ax,ay,status
      real ux,uy

      call args_atoun (id,1,ax,ay,ux,uy,status)

      end
