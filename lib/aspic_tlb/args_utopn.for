      subroutine args_utopn (id,n,ux,uy,px,py,status)

*+  For record 'id' convert the 'n' user coordinate points '(ux,uy)' to
*   pixel coordinates '(px,py)'.
*
*   Status returns are those from 'args_tdbf' or 'args_cminv'.

      integer id,n,px(n),py(n),status,nvals,i
      real ux(n),uy(n),ptou(2,3),utop(2,3),tux,tuy,tpx,tpy

      call args_qdbf (id,'PTOU',2*3,ptou,nvals,status)
      if (status.eq.0) then

          call args_cminv (ptou,utop,status)
          if (status.eq.0) then

              do i=1,n
                  tux = ux(i)
                  tuy = uy(i)
                  call args_cmapp (utop,tux,tuy,tpx,tpy)
                  px(i) = nint (tpx)
                  py(i) = nint (tpy)
              enddo

          endif

      endif

      end
