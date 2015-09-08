      subroutine getdev(name,device,none,ierr)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To aquire a valid SGS workstation identifier for graphics
*       output.
*
*SOURCE
*       GETDEV.FOR in UTILITIES.TLB
*
*METHOD
*       Uses SGS routine SGS_wname to provide a list of device names
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
*   OUTPUT:
*       device  character*(*)   The selected workstation identifier
*       ierr    integer         Status return    0 - Success
*USED BY
*       CRDDTRACE
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               gtstrn,getdvn,wrerr
*       INTERIM:
*               wruser,cnpar
*       SGS:
*               sgs_wlist,sgs_wname
*
*STARLINK PARAMETERS
*       DEFDEV/read/    A device name specified in the program
*                       connection file to be used as the default
*                       when prompting the user
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
      character cmdlst*2000     ! A list of legal device names
      integer   ival            ! A dummy integer argument
      integer   lendev          ! The no. of non-blank characters in device name
      integer   ndev            ! Posn of selected or default device
                                ! name in list
      integer   ndevlt          ! The value of ndev for default device
      integer   ustrln          ! Length of string minus trailing blanks
*
* COMMON BLOCK /GETDEV/ HOLDS THE LIST OF LEGAL DEVICE NAMES FORMED
* IN ROUTINE GETDVN (CALLED BY SGS ROUTINE WNAME)
*
      common /getdev/ cmdlst
*
* SAVE THE VALUE OF ndev TO USE AS DEFAULT DEVICE NUMBER SHOULD
* THE USER WISH TO CHANGE THE DEVICE
*
      save ndev
*
* AQUIRE A LIST OF LEGAL DEVICE NAMES
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
         if(none) cmdlst=cmdlst(:ustrln(cmdlst))//'NONE,'
*
* OPTION 'INFO' DISPLAYS LIST OF VALID DEVICE NAMES
*
         cmdlst=cmdlst(:ustrln(cmdlst))//'INFO.'
*
* AQUIRE DEFAULT DEVICE (SHOULD BE DEFINED IN CONNECTION FILE)
*
         if(device.eq.' ') then
            ndev=1
            call gtstrn('DEFDEV',.true.,cmdlst(:ustrln(cmdlst)),1,ndev,
     :                  device,lendev,ierr)
            if(ierr.ne.0) goto 999
         endif
*
* AQUIRE NAME OF DEVICE TO ACTUALLY USE FROM USER
*
         ndevlt=ndev
  10     call gtstrn(name,.true.,cmdlst(:ustrln(cmdlst)),1,ndev,device,
     :               lendev,ierr)
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
            ndev=ndevlt
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

