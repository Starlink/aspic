      subroutine gtoffs(name,offset,dets,ballno,ndets,ndtout,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To aquire a list of y axis trace offsets, one for each
*       displayed trace.
*
*SOURCE
*       GTOFFS.FOR in CRDDTRACE.TLB
*
*METHOD
*       Any number of values can be given at a single prompt, only
*       the first ndtout are used. If ndtout values were not given
*       then the user is prompted for more values until a total of
*       ndtout have been aquired.
*
*ARGUMENTS
*   INPUTS:
*       name    character       The name of the Starlink parameter to
*                               use for aquiring the offset values
*       dets(ndets)    integer  List of detector to be displayed
*       ballno(ndets)  integer  List of detector Ball numbers
*       ndets   integer         The max no. of offsets which could be
*                               required
*       ndtout  integer         The no. of offsets actually required
*   OUTPUTS:
*       offset(ndets)   real    An array to hold the offset values
*       ierr    integer         Error value 0: Success
*
*SUBROUTINES CALLED
*       EDRS:
*               lbgone
*       INTERIM:
*               wruser,cnpar,rdkeyr
*
*STARLINK PARAMETERS
*       'name'/read/    Argument 'name' contains the name of the
*                       parameter to use for aquiring offset values
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*       do while
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 22/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      character name*(*)
      integer   ndets,ndtout,ierr,dets(ndets),ballno(ndets)
      real      offset(ndets)
*
* DECLARE LOCAL VARIABLES
*
      logical   first   ! True on first loop round rdkeyr
      integer   idet    ! No. of offsets aquired so far
      integer   nval    ! The no. of values received from user at last
                        ! prompt
      character prbuf*80! Buffer for information messages
*
* SET CURRENT DETECTOR COUNT TO 0 AND LOOP UNTIL AN OFFSET HAS BEEN
* GIVEN FOR EACH DETECTOR
*
      first=.true.
      idet=0
      do while(idet.lt.ndtout)
*
* GIVE AN INFORMATIVE MESSAGE UNLESS DEALING WITH THE FIRST OFFSET (SO
* THAT NO MESSAGE IS GIVEN IF USER SPECIFIES ALL OFFSETS ON COMMAND
* LINE)
*
         if(.not.first) then
            write(prbuf,10) ndtout-idet,ballno(dets(idet+1))
  10  format(' Give offset values for ',I2,' detectors starting at #',
     :        I2)
            call lbgone(prbuf(51:))
            call lbgone(prbuf(25:))
            call wruser(prbuf,ierr)
            call cnpar(name,ierr)
         endif
         first=.false.
*
* PROMPT USER AND GET A LIST OF OFFSETS
*
         call rdkeyr(name,.false.,ndtout-idet,offset(idet+1),nval,ierr)
*
* UPDATE THE AQUIRED OFFSET COUNT
*
         if(ierr.ne.0) then
            ierr=0
            nval=0
            call wruser('*** Bad offset value. Try again',ierr)
         endif
         idet=idet+nval
      enddo
*
* FINISH
*
      end
