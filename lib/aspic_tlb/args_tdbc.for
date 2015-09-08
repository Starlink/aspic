      subroutine args_tdbc (id,par,info,ninfo,status)

*+  Tell database values for parameter 'par' of record 'id'. Information
*   is held in 1st 'ninfo' elements of 'info' in character format.
*   Status returns are those from 'args_putp', ie 0 for success, and 1
*   or 2 for failure, plus an extra one of 3 for "no room for extra user
*   parameter".

      include 'ASPIC(dbparms)'

      integer id,ninfo,status,parid,nusp,i
      character par*(*),info(ninfo)*(*),uspars(NUSPAR)*(PARLEN),
     :    uppar*(PARLEN)

      call args_chkpr (id,par,parid)

      if (parid.eq.0) then
          call args_qupar (id,NUSPAR,uspars,nusp,status)
          if (status.eq.0) then
              if (nusp.lt.NUSPAR) then
                  nusp = nusp + 1
                  call str$upcase (uppar,par)
                  uspars(nusp) = uppar
                  call args_tupar (id,uspars,nusp,status)
                  parid = USPARB + nusp
              else
                  status = 3
              endif
          endif
      endif

*   if necessary, new user parameter name has been set up
      if (status.eq.0) then

          if (parid.le.DVPARB) then
              call args_putp (id,parid,info,status)
          else
              call args_putdp (id,parid,info,ninfo,status)
          endif

      endif

      end
