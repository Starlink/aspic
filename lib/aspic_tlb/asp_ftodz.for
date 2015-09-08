      subroutine asp_ftodz (par,subval,val,status)

*+  ASP_FTODZ
*
*   Put real information into the requisite part of the special user
*   parameter DISPZOOM.
*
*   Given:
*     PAR      C   "extra" parameter name corresponding to the required entry
*                  in DISPZOOM
*     SUBVAL   R   value which is to be inserted into the requisite place
*                  in VAL
*     VAL      C   value of DISPZOOM on entry
*
*   Returned:
*     VAL      C   value of DISPZOOM on exit
*     STATUS   I   Return status (0 is success, 3 is "PAR invalid",
*                  may also have value from Starlink routine RTOC)
*
*   Called:
*     RTOC: STARLINK
*     ASP_CTODZ: ASPIC
*
*   W F Lupton RGO 12th November 1981

      include 'ASPIC(dbparms)'

      integer status
      real subval
      character par*(*),val*(*),cval*(FLTLEN)

      call rtoc (subval,cval,status)
      if (status.eq.0) then
          call asp_ctodz (par,cval,val,status)
      endif

      end
