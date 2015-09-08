      subroutine gropen(name,device,none,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Opens SGS/GKS graphics packages for output onto any
*       SGS supported device. Puts device info into common block
*       /GRCOM/.
*
*SOURCE
*       GROPEN.FOR in UTILITIES.TLB
*
*ARGUMENTS
*   INPUTS
*       name    character       Name of Starlink parameter to be used
*                               for aquiring device name
*       none    logical         If true, then NONE is a valid device
*                               option.
*   OUTPUTS:
*       device  charcter        SGS device name
*       ierr    integer         Error status: 0 - Success
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               getdev,wrerr
*       SGS:
*               sgs_widen
*       GKS:
*               gopks,gopwk,gacwk
*
*STARLINK PARAMETERS
*       'name'/read/    Argument 'name' contains the parameter name to
*                       be used for getting the SGS device name
*       NODEVICE/error/ Accessed if the device given cannot be
*                       translated into a valid GKS workstation type
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 12/11/87
*-------------------------------------------------------------------
*
      implicit none
*
* INCLUDE COMMON BLOCK TO HOLD INFO ABOUT DEVICE CAPABILITIES
*
      include 'UTILITIES(GR_COM)'
*
* DECLARE ARGUMENTS
*
      integer ierr
      character*(*) device,name
      logical none
*
* DECLARE LOCAL VARIABLES
*
      integer   anycol  ! 1 if colour available on device
      integer   bzone   ! SGS base zone identifier
      integer   ncol    ! No. of different colours available
      integer   npci    ! No. of preset colour indeces
*
* GET SGS DEVICE NAME
*
      GR_dev=' '
      call getdev(name,device,none,ierr)
      if(ierr.ne.0) goto 999
      GR_dev=device
      if(device.eq.'NONE') goto 999
*
* OPEN SGS SENDING ERROR MESSAGES TO THE SCREEN
*
      call sgs_init(6,ierr)
      if(ierr.eq.0) call sgs_opnwk(device,bzone,ierr)
      if(ierr.ne.0) then
         call wrerr('NODEVICE')
         goto 999
      endif
*
* TRANSLATE NAME TO GKS VALUES
*
      call sgs_widen(device,GR_wty,GR_con,ierr)
      if(ierr.ne.0) goto 999
*
* GET GKS WORKSTATION IDENTIFIER
*
      call sgs_icurw(GR_wid)
*
* SEE IF DEVICE HAS COLOUR
*
      call gqcf(GR_wty,ierr,ncol,anycol,npci)
      if(anycol.eq.1) then
         GR_col=.true.
      else
         GR_col=.false.
      endif
*
* FINISH
*
  999 continue

      end
