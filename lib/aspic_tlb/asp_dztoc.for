      subroutine asp_dztoc (par,val,subval,status)

*+  ASP_DZTOC
*
*   Get character information from the requisite part of the special user
*   parameter DISPZOOM.
*
*   Given:
*     PAR      C   "extra" parameter name corresponding to the required entry
*                  in DISPZOOM
*     VAL      C   value of DISPZOOM on entry
*
*   Returned:
*     SUBVAL   C   value corresponding to PAR
*     STATUS   I   Return status (0 is success, 3 is "PAR invalid")
*
*   Called:
*     STR$UPCASE: VMS
*     ARGS_LOC: ARGS DATABASE
*
*   W F Lupton RGO 12th November 1981

      include 'ASPIC(dbparms)'
      include 'ASPIC(aspdbxpar)'

      integer status,parid,args_loc
      character par*(*),val*(*),subval*(*),uppar*(PARLEN)

*   check 'par' is legal
      call str$upcase (uppar,par)
      parid = args_loc (xpars,NXPAR,uppar)
      if (parid.eq.0) then
          status = 3
      else

*       it is, so update relevant info
          subval = val (xoffst(parid):min(len(val),xoffst(parid)+
     :        xleng(parid)-1))
          status = 0

      endif

      end
