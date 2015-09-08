      subroutine asp_dztof (par,val,subval,status)

*+  ASP_DZTOF
*
*   Get real information from the requisite part of the special user
*   parameter DISPZOOM.
*
*   Given:
*     PAR      C   "extra" parameter name corresponding to the required entry
*                  in DISPZOOM
*     VAL      C   value of DISPZOOM on entry
*
*   Returned:
*     SUBVAL   R   value corresponding to PAR
*     STATUS   I   Return status (0 is success, 3 is "PAR invalid",
*                  may also have value from Starlink routine CTOR)
*
*   Called:
*     CTOR: STARLINK
*     ASP_DZTOC: ASPIC
*
*   W F Lupton RGO 12th November 1981

      include 'ASPIC(dbparms)'

      integer status
      real subval
      character par*(*),val*(*),cval*(FLTLEN)

      call asp_dztoc (par,val,cval,status)
      if (status.eq.0) then
          call ctor (cval,subval,status)
      endif

      end
