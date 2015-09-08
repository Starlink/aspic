      subroutine ovclos(device,iplane)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To take any required action to clear up an ARGS or IKON
*       after using an overlay plane for graphics.
*
*METHOD
*       If an ARGS is being used, then call args_ovcl to reenable
*       the overlay planes. 
*       If an IKON is being used, then call ikon_ovcl to reenable
*       the overlay planes. 
*       
*ARGUMENTS       
*   INPUTS:
*       device	character*(*)	Device beiong used: ARGS or IKON
*       iplane	integer		Overlay plane number (8-15)
*
*SUBROUTINES CALLED
*	THIS PACKAGE:
*		IKON_OVCL
*       ARGS_:
*		ARGS_OVCL
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
      character*(*) device
      integer	    iplane
*
* IF DEVICE IS ARGS, RE-ENABLE OVERLAYS
*
      if(device.eq.'ARGS') then
         call args_ovcl(iplane,.false.)

*
* IF DEVICE IS IKON, RE-ENABLE OVERLAYS
*
      else if(device.eq.'IKON') then
         call ikon_ovcl(iplane,.false.)

      endif
*
* FINISH
*
      end
