      subroutine args_qdb7p (id,type,acx,acy,asx,asy,psx,psy,
     :    status)

*+  Get the first 7 parameters from record 'id'. Status returns are those
*   from 'args_qdbx'.

      integer id,acx,acy,asx,asy,psx,psy,status,i,hstat
      character type*(*)

      call args_qdbc (id,'TYPE',1,type,i,status)
      if (status.ne.0) then

          acx = 0
          acy = 0
          asx = 0
          asy = 0
          psx = 0
          psy = 0
      else

          call args_qdbi (id,'ACENX',1,acx,i,hstat)
          status = max(hstat,status)
          call args_qdbi (id,'ACENY',1,acy,i,hstat)
          status = max(hstat,status)
          call args_qdbi (id,'ASIZX',1,asx,i,hstat)
          status = max(hstat,status)
          call args_qdbi (id,'ASIZY',1,asy,i,hstat)
          status = max(hstat,status)
          call args_qdbi (id,'PSIZX',1,psx,i,hstat)
          statps = max(hstat,status)
          call args_qdbi (id,'PSIZY',1,psy,i,hstat)
          status = max(hstat,status)

      endif

      end
