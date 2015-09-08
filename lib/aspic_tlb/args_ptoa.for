      subroutine args_ptoa (id,px,py,ax,ay,status)

*+  For record 'id' convert the pixel coordinates '(px,py)' to ARGS coord-
*   inates '(ax,ay)'.
*
*   Status returns are those from 'args_ptoan' and thus 'args_tdb7p'.

      integer id,px,py,ax,ay,status

      call args_ptoan (id,1,px,py,ax,ay,status)

      end
