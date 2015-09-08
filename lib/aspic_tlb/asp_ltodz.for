      subroutine asp_ltodz (par,subval,val,status)

*+  ASP_LTODZ
*
*   Put logical information into the requisite part of the special user
*   parameter DISPZOOM.
*
*   Given:
*     PAR      C   "extra" parameter name corresponding to the required entry
*                  in DISPZOOM
*     SUBVAL   L   value which is to be inserted into the requisite place
*                  in VAL
*     VAL      C   value of DISPZOOM on entry
*
*   Returned:
*     VAL      C   value of DISPZOOM on exit
*     STATUS   I   Return status (0 is success, 3 is "PAR invalid")
*
*   Called:
*     ASP_CTODZ: ASPIC
*
*   W F Lupton RGO 12th November 1981

      include 'ASPIC(dbparms)'
      include 'ASPIC(aspdbxpar)'

      integer status
      logical subval
      character par*(*),val*(*),cval*(LOGLEN)

      if (subval) then
          cval='T'
      else
          cval='F'
      endif
      call asp_ctodz (par,cval,val,status)

      end
