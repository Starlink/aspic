      subroutine args_getdp (id,parid,infosz,info,ninfo,status)

*+  For derived parameters, given 'parid' determine which standard parameters
*   must be read, read them, and calculate derived values. Status returns are
*   those from 'args_getp', ie 0 for success and 1 or 2 for failure.

      include 'ASPIC(dbparms)'
      include 'ASPIC(dbdvpar)'

      integer id,parid,infosz,ninfo,status,dvid,tparid,i
      character info(infosz)*(*)

      dvid = parid - DVPARB

      if (1.le.dvid.and.dvid.le.NDVPAR) then
          ninfo = dvnstp(dvid)
          do i = 1,min(infosz,ninfo)
              tparid = dvprid(dvid) + i - 1
              info(i) = ' '
              call args_getp (id,tparid,info(i),status)
          enddo

      else
          ninfo = 0
      endif

      end
