      subroutine args_atopn (id,n,ax,ay,px,py,status)

*+  For record 'id' convert the 'n' ARGS coordinates '(ax,ay)' to pixel
*   coordinates '(px,py)'.
*
*   Status returns are those from 'args_qdb7p' or 'args_strms'.

      integer id,n,ax(n),ay(n),px(n),py(n),acx,acy,asx,asy,psx,psy,
     :    status,i
      real atop(2,3),tax,tay,tpx,tpy
      character type

      call args_qdb7p (id,type,acx,acy,asx,asy,psx,psy,status)
      if (status.eq.0) then

          call args_strms (acx,acy,asx,asy,psx/2,psy/2,psx,psy,atop,
     :    status)
          if (status.eq.0) then

              do i=1,n
                  tax = ax(i)
                  tay = ay(i)
                  call args_cmapp (atop,tax,tay,tpx,tpy)
                  px(i) = nint (tpx)
                  py(i) = nint (tpy)
              enddo

          endif

      endif

      end
