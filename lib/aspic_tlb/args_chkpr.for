      subroutine args_chkpr (id,par,parid)

*+  Check validity of parameter 'par' for record 'id' and return parameter
*   id in 'parid'. 'par' may be :-
*
*       a)   a standard name (eg TYPE, ACENX, ACENY,...)
*       b)   a known derived name (eg ACENT, USIZE,...)
*       c)   a user-defined parameter relevant only to a specific record.
*            There may be up to NUSPAR of these per record.
*
*   Searching is in this order. If 'id' is out of range then no error is
*   signalled, but parameters of type 'c)' cannot, of couse, be searched for.
*   a 'parid' value of 0 indicates an illegal 'par' value.

      include 'ASPIC(dbparms)'
      include 'ASPIC(dbstpar)'
      include 'ASPIC(dbdvpar)'

      integer id,parid,args_loc,idmax,nusp,status
      character par*(*),uppar*(PARLEN),uspars(NUSPAR)*(PARLEN)

*   ensure upper case parameter
      call str$upcase (uppar,par)

*   standard names first
      parid = STPARB + args_loc (stpars,NSTPAR,uppar)
      if (parid.eq.0) then

*       next derived names
          parid = DVPARB + args_loc (dvpars,NDVPAR,uppar)
          if (parid.eq.DVPARB) then

*           finally user-defined names (must have legal 'id' value)
              call args_qidmx (idmax)
              if (1.le.id.and.id.le.idmax) then
                  call args_qupar (id,NUSPAR,uspars,nusp,status)
                  parid = USPARB + args_loc (uspars,nusp,uppar)
                  if (parid.eq.USPARB) then
                      parid = 0
                  endif

*           not found
              else
                  parid = 0
              endif

          endif
      endif
      end
