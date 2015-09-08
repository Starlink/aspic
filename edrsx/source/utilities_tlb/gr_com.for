*      module GR_COM
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Holds information about the most recently opened graphics
*       device.
*
*SOURCE
*       GR_COM.INC in UTILITIES.TLB
*
*METHOD
*       Common block /GRCOM/ holds following variables:
*
*       GR_col  logical         True if device has colour
*       GR_dev  character*30    SGS name of device
*       GR_con  integer         GKS connection identifier
*       GR_wty  integer         GKS workstation type
*       GR_wid  integer         GKS workstation identifier
*
*VAX SPECIFICS
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 13/11/87
*-------------------------------------------------------------------
*
* DECLARE VARIABLES IN COMMON BLOCK /GRCOM/
*
      logical   GR_col  ! True if device has colour
      character GR_dev*30       ! SGS name of device
      integer   GR_con  ! GKS connection identifier
      integer   GR_wty  ! GKS workstation type
      integer   GR_wid  ! GKS workstation identifier
*
* DECLARE COMMON BLOCK /GRCOM/ TO HOLD INFO ABOUT DEVICE
*
      common /grcom/ GR_dev,GR_col,GR_wty,GR_con,GR_wid

