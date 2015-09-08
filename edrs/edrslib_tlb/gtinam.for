      subroutine gtinam(pname,imname,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Returns the name of a BDF file associated with a given
*       frame parameter.
*
*METHOD
*       Uses low-level interim routines. Based on STL_accfrm.
*
*ARGUMENTS
*   INPUTS:
*       pname   character       The 'frame' type parameter name
*   OUTPUTS:
*       imname  character       The name of the .BDF file associated
*                               with parameter given by pname
*       ierr    integer         Error status: 0 - Success
*                                             1 - Parameter not yet used
*                                             2 - Parameter has null value
*                                             3 - Parameter is cancelled
*                                             4 - Parameter not yet used
*                                             5 - Error in stl_getppi
*                                             6 - Parameter not a frame
*
*USED BY
*       CRDDTRACE
*
*SUBROUTINES CALLED
*       EDRS:
*               stl_getppi
*       INTERIM:
*               stl_findpe
*
*STARLINK PARAMETERS
*       pname/read/     Argument pname contains name of parameter
*                       associated with the .BDF file
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 22/1/87
*       (Based on the INTERIM routine STL_accfrm)
*-------------------------------------------------------------------
*
      implicit none
*
* INCLUDE INTERIM COMMON BLOCKS AND PARAMETER DEFINITIONS
*
      include 'interim(pctcom)'
      include 'interim(errpar)'
*
* DECLARE ARGUMENTS
*
      character*(*)     pname,imname
      integer           ierr
*
* DECLARE LOCAL VARIABLES
*
      integer   entry   ! Pointer to entry for given parameter in
                        ! parameter information tables
      integer   ival    ! Dummy integer argument
*
* INITIALISE ERROR STATUS AND FILE NAME
*
      ierr=0
      imname=' '
*
* SEARCH FOR EXISTING ENTRY IN PARAMETER TABLE FOR GIVEN PARAMETER
*
      call stl_findpe(pname,entry)
*
* IF ENTRY DOES NOT EXIST, THEN EXIT WITH IERR=1
*
      if (entry.eq.0) then
         ierr=1
         goto 999
      endif
*
* IF FRAME IS NULL, THEN EXIT WITH IERR=2
*
      if (pct_state(entry).eq.pct_null) then
         ierr=2
         goto 999
*
* ELSE, IF PARAMETER HAS BEEN CANCELLED, EXIT WITH IERR=3
*
      else if(pct_state(entry).eq.pct_cancel) then
         ierr=3
         goto 999
*
* ELSE, IF PARAMETER HAS NOT YET BEEN ASSIGNED A VALUE, EXIT WITH IERR=4
*
      else if(pct_state(entry).eq.pct_ground) then
         ierr=4
         goto 999
*
* ELSE, IF PARAMETER CURRENTLY HAS A NON-NULL VALUE, GET PROGRAM
* PARAMETER INFORMATION FROM THE ENVIRONMENT
*
      else if(pct_state(entry).eq.pct_active) then
         call stl_getppi(entry,.false.,1,imname,ival,ierr)
         if (ierr.ne.err_normal) then
            imname=' '
            ierr=5
            goto 999
         endif
*
* CHECK IF PARAMETER IS BULK DATA FRAME TYPE. IF NOT EXIT WITH IERR=6
*
         if (pct_type(entry)(1:5).ne.'frame'.and.
     :       pct_type(entry)(1:5).ne.'FRAME') then
            imname=' '
            ierr=6
            goto 999
         endif
      endif
*
* FINISH
*
  999 continue

      end
