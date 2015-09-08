      subroutine args_qdbc (id,par,infosz,info,ninfo,status)

*+  Query database value of parameter 'par' for record 'id'. Information
*   is returned in 1st 'ninfo' elements of 'info' in character format.
*   Status returns are those from 'args_getp', ie 0 for success and 1
*   or 2 for failure, plus an extra one of 3 for illegal 'par'.

      include 'ASPIC(dbparms)'

      integer id,infosz,ninfo,status,parid
      character par*(*),info(infosz)*(*)

      call args_chkpr (id,par,parid)
      if (parid.eq.0) then
          ninfo = 0
          status = 3

      else
          if (parid.le.DVPARB) then
              call args_getp (id,parid,info,status)
              if (status.eq.0) then
                  ninfo = 1
              else
                  ninfo = 0
              endif
          else
              call args_getdp (id,parid,infosz,info,ninfo,status)
          endif

      endif

      do i = ninfo+1,infosz
          info(i) = ' '
      enddo

      end
