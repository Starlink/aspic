      subroutine getdev(name,device,none,ierr)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To aquire a valid SGS workstation identifier for graphics
*       output.
*
*METHOD
*	The user is prompted for a character string, using the input
*	value of argument device as the default (if device is null
*	the the value INFO is used instead). If the string thus obtained
*	is not a valid sgs workstation identifier, then SGS routine 
*	SGS_wname is called to provide a list of device names
*       valid on the node at which the program is executing. This uses
*       logical names and so does not need to be re-linked every time
*       the list of valid devices changes. If the logical names are not
*       defined at the users node, then accept any string from the user
*       as a device name and hope it works
*          If the user replies 'INFO' then a more complete description
*       of the available devices is given using SGS_wlist. If the
*       argument 'none' is .true., then the user is given the option of
*       replying NONE, in which case no graphics device will be opened.
*
*ARGUMENTS
*   INPUTS:
*       name    character*(*)   The parameter name to be associated with
*                               the string containing the device name
*       none    logical         If true, then NONE is a valid option
*	device	character*(*)	The default device name. If null then
*				INFO is used.
*   OUTPUT:
*       device  character*(*)   The selected workstation identifier
*       ierr    integer         Status return    0 - Success
*
*SUBROUTINES CALLED
*       THIS PACKAGE 
*               getcmd,getdvn,lenstr,wruser
*       INTERIM:
*               cnpar,rdkeyc
*       SGS:
*               sgs_wlist,sgs_wname
*	GNS:
*	        gns_tng
*
*STARLINK PARAMETERS
*       'name'/read/    The argument name contains the parameter name
*                       to use when prompting the user for the device
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 4/9/87
*-------------------------------------------------------------------
*
      implicit none
      external getdvn
*
* DECLARE ARGUMENTS
*
      character*(*)     device,name
      logical           none
      integer           ierr
*
* DECLARE LOCAL VARIABLES
*
      character cmdlst*1000     ! A list of legal device names
      integer	iconid		! Workstation connection identifier
      integer   itype		! Workstation type
      integer   ival            ! A dummy integer argument
      integer   lendev          ! The no. of non-blank characters in 
				! device name
      integer   ndev            ! Posn of selected device  name in list
      integer   lenstr          ! Length of string minus trailing blanks
*
* COMMON BLOCK /GETDEV/ HOLDS THE LIST OF LEGAL DEVICE NAMES FORMED
* IN ROUTINE GETDVN (CALLED BY SGS ROUTINE WNAME)
*
      common /getdev/ cmdlst
*
* GET A FIRST TRY FROM THE USER
*
      if(device.eq.' ') device='INFO'
      call rdkeyc(name,.true.,1,device,ival,ierr)
*
* SEE IF IT IS A VALID WORKSTATION NAME
*
      ierr=0
      call gns_tng(device,itype,iconid,ierr)
*
* IF USER HAS GIVEN A GOOD NAME RETURN WITHOUT FURTHER ADO
*
      if(ierr.eq.0) goto 999
*
* IF USER HAS NOT GIVEN A GOOD NAME, AQUIRE A LIST OF LEGAL DEVICE NAMES
*
      call sgs_wname(getdvn,1,ierr)
      if(ierr.ne.0) goto 999
*
* IF ANY SGS DEVICE NAMES ARE DEFINED, USER MUST SPECIFY ONE OF THEM
*
      if(cmdlst.ne.' ') then
*
* IF NONE IS A VALID OPTION ADD IT TO THE LIST OF OPTIONS
*
         if(none) cmdlst=cmdlst(:lenstr(cmdlst))//'NONE,'
*
* OPTION 'INFO' DISPLAYS LIST OF VALID DEVICE NAMES
*
         cmdlst='INFO,'//cmdlst(:lenstr(cmdlst)-1)//'.'
*
* AQUIRE NAME OF DEVICE FROM USER
*
         ndev=1
  10     call getcmd(name,cmdlst(:lenstr(cmdlst)),1,ndev,device,lendev,
     :               ierr)
         if(ierr.ne.0) goto 999
*
* IF REQUIRED, GIVE LIST OF LEGAL DEVICE NAMES AND GET ANOTHER DEVICE
* FROM USER
*
         if(device.eq.'INFO') then
            call wruser(' ',ierr)
            call wruser('  The following graphics devices are'//
     :                  ' available:',ierr)
            call sgs_wlist(6)
            if(none) call wruser('  NONE        No graphics output '//
     :                           'required',ierr)
            call wruser(' ',ierr)
            call cnpar(name,ierr)
            goto 10
         endif
*
* IF NO SGS DEVICE NAMES ARE DEFINED ALLOW USE TO SPECIFY ANY STRING
*
      else
         call wrerr('NOSGSNAME')
         call rdkeyc('name',.false.,1,device,ival,ierr)
      endif
*
* FINISH
*
 999  continue

      end






      subroutine getdvn(name,commnt,ival,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To get a list of the available SGS device types.
*       This routine is called by SGS routine sgs_wname once for every
*       valid workstation type recognized by the node on which the
*       program is running (See SUN85 p33).
*
*METHOD
*       The list is stored in string cmdlist which is in common block
*       /getdev/. Workstation names are seperated by commas and the
*       list is terminated by a full stop.
*
*ARGUMENTS
*   INPUTS:
*       name    character       Workstation name
*       commnt  character       Descriptive comment string
*       ival    integer         Not used
*   OUTPUTS:
*       ierr    integer         Status value
*
*SUBROUTINES CALLED
*       THIS PACKAGE:
*               lenstr
*
*VAX SPECIFICS
*       implicit none
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 4/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      character*(*)     name,commnt
      integer           ival,ierr
*
* DECLARE LOCAL VARIABLES
*
      character*1000    cmdlst
      common /getdev/   cmdlst
      integer           lenstr
*
* ADD NEW DEVICE NAME TO END OF LIST OF LEGAL DEVICE NAMES
*
      cmdlst=cmdlst(:lenstr(cmdlst))//name(:lenstr(name))//','
*
* FINISH
*
      ierr=0

      end
