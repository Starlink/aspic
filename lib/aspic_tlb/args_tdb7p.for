      subroutine args_tdb7p (id,type,acx,acy,asx,asy,psx,psy,
     :    status)

*+  Put the first 7 parameters to record 'id'. Status returns are those
*   from 'args_tdbx'.

      integer id,acx,acy,asx,asy,psx,psy,status,hstat
      character type*(*)

      call args_tdbc (id,'TYPE',type,1,status)
      if (status.eq.0) then

          call args_tdbi (id,'ACENX',acx,1,hstat)
          status = max(hstat,status)
          call args_tdbi (id,'ACENY',acy,1,hstat)
          status = max(hstat,status)
          call args_tdbi (id,'ASIZX',asx,1,hstat)
          status = max(hstat,status)
          call args_tdbi (id,'ASIZY',asy,1,hstat)
          status = max(hstat,status)
          call args_tdbi (id,'PSIZX',psx,1,hstat)
          statps = max(hstat,status)
          call args_tdbi (id,'PSIZY',psy,1,hstat)
          status = max(hstat,status)

      endif

      end
