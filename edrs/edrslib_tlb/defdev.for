      subroutine defdev(device)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Returns a default device for image handling on the users
*       node (generally ARGS or IKON devices). The device is returned
*       as an SGS workstation identifier.
*
*METHOD
*       An attempt is made to translate the logical name EDRS_DEVICE.
*       If it is successful, and the returned value is a valid SGS
*       workstation identifier, then this value is returned as the
*       default device. If the logical name does not exist or does
*       not have the value of a valid SGS workstation identifier, 
*       then a search is made through the local table of SGS workstation
*       identifiers until an ARGS device is found (workstation type 
*	160) and this is returned. If no ARGS is found, a search is
*	made for an IKON and if found its identifier is returned. If
*	neither ARGS nor IKON is found, a null string is returned.
*       
*ARGUMENTS       
*   OUTPUTS:
*       device	character	The SGS workstation identifier of the
*       			default image handling device for EDRS
*
*SUBROUTINES CALLED
*       THIS PACKAGE:
*              fnddev
*	SGS:
*		sgs_widen
*	RUN TIME LIBRARY:
*		lib$sys_trnlog
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*       RTL routines
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 13/7/88
*-------------------------------------------------------------------
*
      implicit none
      include '($SSDEF)'
*
* DECLARE ARGUMENTS
*
      character	device*(*)
*
* DECLARE LOCAL VARIABLES
*
      integer	args		! ARGS workstation type
      integer	iconid		! Connection identifier of default device
      integer	ierr		! Error status
      integer	ikon		! IKON workstation type
      integer	itype		! Workstation type of default device
      integer	lib$sys_trnlog	! RTL routine for translating log. names
      logical	ok		! True if good device has been obtained

      parameter	(args=160,ikon=3200)

*
* ATTEMPT TO TRANSLATE THE LOGICAL NAME "EDRS_DEVICE"
*
      ok=.false.
      ierr=lib$sys_trnlog('EDRS_DEVICE',,device,,,)

*
* IF SUCCESFUL, SEE IF IT IS A VALID SGS WORKSTATION IDENTIFIER
*
      if(ierr.eq.SS$_NORMAL) then
         call sgs_widen(device,itype,iconid,ierr)

*
* IF SUCCESSFUL, RETURN WITH THIS VALUE. 
*
         if(ierr.eq.0) then
            ok=.true.
         else
            device=' '
         endif
      endif

*
* IF NO VALID NAME HAS YET BEEN OBTAINED, SEARCH FOR AN ARGS DEVICE
* IN THE LOCAL LIST OF SGS WORKSTATION IDENTIFIERS
*
      if(.not.ok) then
         call fnddev(device,args)
         
*
* IF NOT FOUND, SEARCH FOR AN IKON
*
         if(device.eq.' ') then
            call fnddev(device,ikon)
         endif
      endif

*
* FINISH WITH A NULL DEVICE IF NEITHER ARGS NOR IKON WAS FOUND
*

      end
