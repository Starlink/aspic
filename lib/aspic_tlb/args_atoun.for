      subroutine args_atoun (id,n,ax,ay,ux,uy,status)

*+  For record 'id' convert the 'n' ARGS coordinates '(ax,ay)' to user
*   coordinates '(ux,uy)'.
*
*   Status returns are those from 'args_qdb7p', 'args_strms' or 'args_qdbf'.

      integer id,n,ax(n),ay(n),acx,acy,asx,asy,psx,psy,status,nvals,i
      real ux(n),uy(n),atop(2,3),ptou(2,3),atou(2,3),tax,tay,tux,tuy
      character type

      call args_qdb7p (id,type,acx,acy,asx,asy,psx,psy,status)
      if (status.eq.0) then

          call args_strms (acx,acy,asx,asy,psx/2,psy/2,psx,psy,atop,
     :    status)
          if (status.eq.0) then

              call args_qdbf (id,'PTOU',2*3,ptou,nvals,status)
              if (status.eq.0) then
    
                  call args_cmmul (ptou,atop,atou)
    
                  do i=1,n
                      tax = ax(i)
                      tay = ay(i)
                      call args_cmapp (atou,tax,tay,tux,tuy)
                      ux(i) = tux
                      uy(i) = tuy
                  enddo
    
              endif
    
          endif
    
      endif

      end
