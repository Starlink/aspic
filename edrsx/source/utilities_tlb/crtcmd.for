      subroutine crtcmd(name,optlst,xlo,xhi,ylo,yhi,cmd,title,usecur,
     :                  ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To get a command from either the keyboard or the
*       screen.
*
*SOURCE
*       CRTCMD.FOR in UTILITIES.TLB
*
*ARGUMENTS
*   INPUTS:
*       name    character       The parameter name to be used for
*                               aquiring the command
*       optlst  character       The list of the options seperated by
*                               commas and terminated by a full stop.
*       xlo     real            The lower limit in x of the box used
*                               to display the options if the cursor
*                               is used for selection. Specified in
*                               terms of the current SGS world co-
*                               ordinates.
*       xhi     real            The upper x limit (see xlo).
*       ylo     real            The lower y limit (see xlo).
*       yhi     real            The upper y limit (see xlo).
*       title   character       Title to display above option boxes
*       usecur  logical         True if user wants to use the cursor
*       cmd     integer         Position of default within options list
*   OUTPUTS:
*       cmd     integer         The position within the list optlst
*                               of the selected command.
*       ierr    integer         Status return:  0 - Success
*
*USED BY
*       CRDDTRACE
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               gtstrn,curcmd,wrerr
*       SGS:
*               sgs_icuav
*       INTERIM:
*               cnpar
*
*STARLINK PARAMETERS
*       'name'/read/    The argument name contains the parameter name
*                       used to aquire the command string
*       NOCURS/error/   Accessed if the cursor was requested, but no
*                       cursor exists.
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 3/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      character*(*) optlst,name,title
      integer   cmd,ierr
      real      xlo,xhi,ylo,yhi
      logical   usecur
*
* DECLARE LOCAL VARIABLES
*
      logical   cursav  ! True if graphics device has a cursor
      integer   lenopt  ! Length of option selected in gtstrn
      integer   nopt    ! Position of selected option within option list
      character opt*20  ! Option selected in gtstrn
      logical   optgot  ! True when an options has been obtained
*
* IF CURSOR IS TO BE USED, CALL CURCMD TO GET THE REQUIRED OPTION
*
      optgot=.false.
      if(usecur) then
*
* SEE IF CURRENT GRAPHICS DEVICE HAS A CURSOR
*
         call sgs_icuav(cursav)
         if(cursav) then
            call curcmd(optlst,xlo,xhi,ylo,yhi,cmd,title,ierr)
            if(ierr.ne.0) goto 999
            optgot=.true.
         else
            call wrerr('NOCURS')
         endif
      endif
*
* IF AN OPTION WAS NOT THUS OBTAINED, GET ONE FROM KEYBOARD
*
      if(.not.optgot) then
         call cnpar(name,ierr)
         call gtstrn(name,.true.,optlst,1,cmd,opt,lenopt,ierr)
      endif
*
* FINISH
*
 999  continue

      end
