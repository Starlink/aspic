      subroutine args_utop (id,ux,uy,px,py,status)

*+  For record 'id' convert the user coordinates '(ux,uy)' to pixel
*   coordinates '(px,py)'.
*
*   Status returns are those from 'args_utopn' and thus 'args_qdbf'
*   or 'args_cminv'.

      integer id,px,py,status
      real ux,uy

      call args_utopn (id,1,ux,uy,px,py,status)

      end
