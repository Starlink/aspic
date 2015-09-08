      subroutine args_ptoun (id,n,px,py,ux,uy,status)

*+  For record 'id' convert the 'n' pixel coordinates '(px,py)' to user
*   coordinates '(ux,uy)'.
*
*   Status returns are those from 'args_qdbf'.

      integer id,n,px(n),py(n),status,nvals,i
      real ux(n),uy(n),ptou(2,3),tpx,tpy,tux,tuy

      call args_qdbf (id,'PTOU',2*3,ptou,nvals,status)
      if (status.eq.0) then

          do i=1,n
              tpx = px(i)
              tpy = py(i)
              call args_cmapp (ptou,tpx,tpy,tux,tuy)
              ux(i) = tux
              uy(i) = tuy
          enddo

      endif

      end
