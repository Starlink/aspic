      subroutine args_ptou (id,px,py,ux,uy,status)

*+  For record 'id' convert the pixel coordinates '(px,py)' to user coord-
*   inates '(ux,uy)'.
*
*   Status returns are those from 'args_ptoun' and thus 'args_tdbf'.

      integer id,px,py,status
      real ux,uy

      call args_ptoun (id,1,px,py,ux,uy,status)

      end
