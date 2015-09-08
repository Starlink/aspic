      subroutine args_ptoan (id,n,px,py,ax,ay,status)

*+  For record 'id' convert the 'n' pixel coordinates '(px,py)' to ARGS
*   coordinates '(ax,ay)'.
*
*   Status returns are those from 'args_qdb7p' or 'args_strms'.

      integer id,n,px(n),py(n),ax(n),ay(n),acx,acy,asx,asy,psx,psy,
     :    status,i
      real ptoa(2,3),tpx,tpy,tax,tay
      character type

      call args_qdb7p (id,type,acx,acy,asx,asy,psx,psy,status)
      if (status.eq.0) then

          call args_strms (psx/2,psy/2,psx,psy,acx,acy,asx,asy,ptoa,
     :    status)
          if (status.eq.0) then

              do i=1,n
                  tpx = px(i)
                  tpy = py(i)
                  call args_cmapp (ptoa,tpx,tpy,tax,tay)
                  ax(i) = nint (tax)
                  ay(i) = nint (tay)
              enddo

          endif

      endif

      end
