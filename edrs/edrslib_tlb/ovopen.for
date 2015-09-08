      subroutine ovopen(device,iplane,colour)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To open an overlay plane for line graphics in a particular
*       colour on either an ARGS or an IKON.
*
*METHOD
*       Call device specific routines depending on the value of
*       argument 'device'.
*       
*ARGUMENTS       
*   INPUTS:
*       device	character*(*)	Device for graphics. ARGS or IKON
*       iplane  integer		Overlay plane to use (8-15).
*       colour	character*(*)   Colour for overlay: RED,GREEN,BLUE,
*       			CYAN,MAGENTA,YELLOW,WHITE,BLACK.
*
*SUBROUTINES CALLED
*       THIS PACKAGE:
*              IKON_OVOP,IKON_CLS
*       ARGS_:
*	       ARGS_OVOP
*
*VAX SPECIFICS
*       implicit none
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 29/6/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      character*(*) colour,device
      integer	    iplane
*
* INCLUDE COMMON BLOCK HOLDING ARGS DATABASE INFORMATION
*
      include 'ARGSCOM.FOR'

*
* IF DEVICE IS ARGS, USE ARGS DATABASE ROUTINE FOR OPENING OVERLAY
*
      if(device.eq.'ARGS') then
         call args_ovop(iplane,colour)

*
* OTHERWISE IF DEVICE IS IKON, USE ROUTINES FROM THIS PACKAGE WHICH
* ARE BASED MAINLY ON GKS BUT WITH SOME IKON SPECIFIC CODE ASWELL
*
      else if(device.eq.'IKON') then
         call ikon_ovop(iplane,colour)
         call ikon_cls(iplane)
      endif

*
* FINISH
*
      end
