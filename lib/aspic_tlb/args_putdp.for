      subroutine args_putdp (id,parid,info,ninfo,status)

*+  For derived parameters, given 'parid' determine which standard parameters
*   must be written, write them, and calculate derived values. Status returns
*   are those from 'args_putp', ie 0 for success and 1 or 2 for failure.

      include 'ASPIC(dbparms)'
      include 'ASPIC(dbdvpar)'

      integer id,parid,ninfo,status,dvid,i
      character info(ninfo)*(*)

      dvid = parid - DVPARB

      if (1.le.dvid.and.dvid.le.NDVPAR) then
          do i = 1,min(ninfo,dvnstp(dvid))
              call args_putp (id,dvprid(dvid)+i-1,info(i),status)
          enddo

      endif

      end
