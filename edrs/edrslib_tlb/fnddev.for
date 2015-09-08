      subroutine fnddev(device,itype)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Locates the first SGS workstation identifier in the local
*       table of such identifiers, which corresponds to a device
*       with a given workstation type.
*
*METHOD
*       Call sgs_wname to step through the local workstation
*       identifier list, and checks the type of each one until
*       one of the required type is found. If no suitable
*       identifier is found the argument device is returned empty.
*
*ARGUMENTS       
*   INPUTS:
*       itype	integer		The required workstation type
*   OUTPUTS:
*       device	character	The first workstation identifier having
*       			the required workstation type. If none
*				is found, then a null value is returned.
*
*SUBROUTINES CALLED
*       SGS:
*		sgs_wname
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 13/7/88
*-------------------------------------------------------------------
*
      implicit none

*
* DECLARE ARGUMENTS
*
      integer	itype
      character	device*(*)

*
* DECLARE SERVICE ROUTINE FOR SGS_WNAME AS EXTERNAL
*
      external wnamrt

*
* DECLARE LOCAL VARIABLES
*
      character	dname*30! The identifier returned from sgs_wname. Held
			! in common block /FNDDEV/
      integer	ierr	! Status from sgs_wname

*
* COMMON BLOCK /FNDDEV/ IS USED TO COMMUNICATE WITH SGS_WNAME SERVICE
* ROUTINE
*
      common /FNDDEV/ dname

*
* SGS_WNAME CALLS ROUTINE WNAMRT ONCE FOR EACH WORKSTATION IDENTIFIER IN
* THE LOCAL TABLE. WNAMRT CHECKS THE TYPE OF EACH WORKSTATION AND IF
* IT MATCHES THE REQUIRED TYPE, PUTS THE IDENTIFIER INTO COMMON BLOCK
* /FNDDEV/
*
      dname=' '
      call sgs_wname(wnamrt,itype,ierr)

*
* COPY THE IDENTIFIER FROM COMMON TO THE OUTPUT ARGUMENT
*
      device=dname

*
* FINISH
*
      end




      subroutine wnamrt(name,coment,itype,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Called once per workstation identifier by routine sgs_wname.
*       Checks the workstation type of the identifier against the
*       required workstation type. If they match, the identifier is
*       copied into common block /FNDDEV/.
*
*ARGUMENTS       
*   INPUTS:
*       name	character	The next device workstation identifier
*       coment	character	Comment about the next device
*       itype	integer		The required workstation type
*   OUTPUTS:
*       ierr    integer         Status: Always 0 
*
*SUBROUTINES CALLED
*       SGS:
*		sgs_widen
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 13/7/88
*-------------------------------------------------------------------
*
      implicit none

*
* DECLARE ARGUMENTS
*
      character	name*(*),coment*(*)
      integer	itype,ierr
      
*
* DECLARE LOCAL VARIABLES
*
      character	dname*30! Workstation identifier if match is found, null
			! otherwise.
      integer	istat	! Error status from sgs_widen
      integer	jconid	! Connection identifier of next device
      integer	jtype	! Workstation type of next device

*
* COMMON BLOCK /FNDDEV/ IS USED TO RETURN THE MATCHING IDENTIFER TO
* THE ROUTINE WHICH CALLED SGS_WNAME
*
      common /FNDDEV/ dname

*
* FIND THE WORKSTATION TYPE OF THE NEXT WORKSTATION IDENTIFIER
*
      call sgs_widen(name,jtype,jconid,istat)

*
* IF THE WORKSTATION TYPE MATCHES THE REQUIRED TYPE, COPY THE IDENTIFIER
* INTO COMMON.
*
      if(jtype.eq.itype.and.dname.eq.' ') dname=name
      ierr=0

*
* FINISH
*
      end
