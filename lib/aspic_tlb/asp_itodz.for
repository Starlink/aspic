      subroutine asp_itodz (par,subval,val,status)

*+  ASP_ITODZ
*
*   Put integer information into the requisite part of the special user
*   parameter DISPZOOM.
*
*   Given:
*     PAR      C   "extra" parameter name corresponding to the required entry
*                  in DISPZOOM
*     SUBVAL   I   value which is to be inserted into the requisite place
*                  in VAL
*     VAL      C   value of DISPZOOM on entry
*
*   Returned:
*     VAL      C   value of DISPZOOM on exit
*     STATUS   I   Return status (0 is success, 3 is "PAR invalid",
*                  may also have value from Starlink routine ITOC)
*
*   Called:
*     ITOC: STARLINK
*     ASP_CTODZ: ASPIC
*
*   W F Lupton RGO 12th November 1981

      include 'ASPIC(dbparms)'

      integer subval,status
      character par*(*),val*(*),cval*(INTLEN)

      call itoc (subval,cval,status)
      if (status.eq.0) then
          call asp_ctodz (par,cval,val,status)
      endif

      end
