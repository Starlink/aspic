      subroutine args_atop (id,ax,ay,px,py,status)

*+  For record 'id' convert the ARGS coordinates '(ax,ay)' to pixel coord-
*   inates '(px,py)'.
*
*   Status returns are those from 'args_atopn' and thus 'args_tdb7p'.

      integer id,ax,ay,px,py,status

      call args_atopn (id,1,ax,ay,px,py,status)

      end
