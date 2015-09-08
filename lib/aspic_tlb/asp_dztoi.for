      subroutine asp_dztoi (par,val,subval,status)

*+  ASP_DZTOI
*
*   Get integer information from the requisite part of the special user
*   parameter DISPZOOM.
*
*   Given:
*     PAR      C   "extra" parameter name corresponding to the required entry
*                  in DISPZOOM
*     VAL      C   value of DISPZOOM on entry
*
*   Returned:
*     SUBVAL   I   value corresponding to PAR
*     STATUS   I   Return status (0 is success, 3 is "PAR invalid",
*                  may also have value from Starlink routine CTOI)
*
*   Called:
*     CTOI: STARLINK
*     ASP_DZTOC: ASPIC
*
*   W F Lupton RGO 12th November 1981

      include 'ASPIC(dbparms)'

      integer subval,status
      character par*(*),val*(*),cval*(INTLEN)

      call asp_dztoc (par,val,cval,status)
      if (status.eq.0) then
          call ctoi (cval,subval,status)
      endif

      end
