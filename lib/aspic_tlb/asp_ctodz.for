      subroutine asp_ctodz (par,subval,val,status)

*+  ASP_CTODZ
*
*   Put character information into the requisite part of the special user
*   parameter DISPZOOM.
*
*   Given:
*     PAR      C   "extra" parameter name corresponding to the required entry
*                  in DISPZOOM
*     SUBVAL   C   value which is to be inserted into the requisite place
*                  in VAL
*     VAL      C   value of DISPZOOM on entry
*
*   Returned:
*     VAL      C   value of DISPZOOM on exit
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
      character par*(*),subval*(*),val*(*),uppar*(PARLEN)

*   check 'par' is legal
      call str$upcase (uppar,par)
      parid = args_loc (xpars,NXPAR,uppar)
      if (parid.eq.0) then
          status = 3
      else

*       it is, so update relevant info
          val (xoffst(parid):min(len(val),xoffst(parid)+xleng(parid)-1))
     :    =subval
          status = 0

      endif

      end
