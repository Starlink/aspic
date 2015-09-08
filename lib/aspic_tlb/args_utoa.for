      subroutine args_utoa (id,ux,uy,ax,ay,status)

*+  For record 'id' convert the ARGS coordinates '(ux,uy)' to user coord-
*   inates '(ax,ay)'.
*
*   Status returns are those from 'args_utoan' and thus 'args_tdb7p',
*   'args_qdbf' or 'args_cminv'.

      integer id,ax,ay,status
      real ux,uy

      call args_utoan (id,1,ux,uy,ax,ay,status)

      end
