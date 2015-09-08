          subroutine args_utoan (id,n,ux,uy,ax,ay,status)

*+  For record 'id' convert the 'n' user coordinates '(ux,uy)' to ARGS
*   coordinates '(ax,ay)'.
*
*   Status returns are those from 'args_qdb7p', 'args_qdbf', 'args_cminv'
*   or 'args_strms'.

      integer id,n,ax(n),ay(n),acx,acy,asx,asy,psx,psy,status,nvals,i
      real ux(n),uy(n),ptou(2,3),utop(2,3),ptoa(2,3),utoa(2,3),tux,tuy,
     :    tax,tay
      character type

      call args_qdb7p (id,type,acx,acy,asx,asy,psx,psy,status)
      if (status.eq.0) then

          call args_qdbf (id,'PTOU',2*3,ptou,nvals,status)
          if (status.eq.0) then

              call args_cminv (ptou,utop,status)
              if (status.eq.0) then

                  call args_strms (psx/2,psy/2,psx,psy,acx,acy,asx,asy,
     :            ptoa,status)
                  if (status.eq.0) then

                      call args_cmmul (ptoa,utop,utoa)
                      do i=1,n
                          tux = ux(i)
                          tuy = uy(i)
                          call args_cmapp (utoa,tux,tuy,tax,tay)
                          ax(i) = nint (tax)
                          ay(i) = nint (tay)
                      enddo

                  endif

              endif

          endif

      endif

      end
