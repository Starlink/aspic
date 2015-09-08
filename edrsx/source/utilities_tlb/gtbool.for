      subroutine gtbool(name,deflt,value,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Gets a Boolean value from the user, allowing user to
*       specify any abreviation of YES NO TRUE or FALSE.
*
*SOURCE
*       GTBOOL.FOR in UTILITIES.TLB
*
*METHOD
*       Calls GTSTRN and then selects .true. or .false. on the basis
*       of the user selected option.
*
*ARGUMENTS
*   INPUTS:
*       name    character       The name of the associated INTERIM
*                               parameter.
*       deflt   logical         If true then the input value is
*                               given to the user as a default
*       value   logical         If deflt is true, this gives the
*                               default value to be passed to the user
*   OUTPUTS:
*       value   logical         Either true or false depending on the
*                               users selection. YES and TRUE gives
*                               .true., NO and FALSE give .false.
*       ierr    integer         Error status: 0 - Success
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gtstrn
*
*STARLINK PARAMETERS
*       'name'/read/    Argument name holds the name of the parameter
*                       to be used to aquire the value from the user
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 27/5/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      character name*(*)
      logical   deflt,value
      integer   ierr
*
* DECLARE LOCAL VARIABLES
*
      character cval*1  ! Dummy character argument
      integer ival      ! Dummy integer argument
      integer ncomm     ! Position of selected option within option list
*
* SET DEFAULT
*
      if(value) then
         ncomm=1
      else
         ncomm=2
      endif
*
* GET VALUE FROM USER
*
      call gtstrn(name,deflt,'YES,NO,TRUE,FALSE.',1,ncomm,cval,ival,
     :            ierr)
      if(ierr.ne.0) goto 999
*
* DECODE THE USERS SELECTION INTO TRUE AND FALSE ANSWERS
*
      if(mod(ncomm,2).eq.0) then
         value=.false.
      else
         value=.true.
      endif
*
* FINISH
*
 999  continue

      end
