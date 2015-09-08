      subroutine asp_dztol (par,val,subval,status)

*+  ASP_DZTOL
*
*   Get logical information from the requisite part of the special user
*   parameter DISPZOOM.
*
*   Given:
*     PAR      C   "extra" parameter name corresponding to the required entry
*                  in DISPZOOM
*     VAL      C   value of DISPZOOM on entry
*
*   Returned:
*     SUBVAL   L   value corresponding to PAR
*     STATUS   I   Return status (0 is success, 3 is "PAR invalid")
*
*   Called:
*     ASP_DZTOC: ASPIC
*
*   W F Lupton RGO 12th November 1981

      include 'ASPIC(dbparms)'
      include 'ASPIC(aspdbxpar)'

      integer status
      logical subval
      character par*(*),val*(*),cval*(LOGLEN)

      call asp_dztoc (par,val,cval,status)
      if (status.eq.0) then
          subval = cval.eq.'T'
      endif

      end
