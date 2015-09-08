      subroutine xyscrn(ixd,iyd,ux,uy,exit,device,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To obtain an XY position from the ARGS or IKON screen,
*       using the cursor and pointing device (trackerball or
*       mouse).
*
*METHOD
*       Call XYPAN if device is args. Call XYIKON if device is
*       ikon.
*       
*ARGUMENTS       
*   INPUTS:
*       device		character	Device to use: ARGS or IKON
*   OUTPUTS:
*       ixd,iyd		integers	Selected position in device
*       				co-ords.(ARGS only)
*       ux,uy		real		Selected position in user
*       				co-ords
*       exit		logical		If true, user wants to finish.
*       ierr            integer         Error status 0 - Success
*
*SUBROUTINES CALLED
*       THIS PACKAGE:
*              xypan, xyikon
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
      real	ux,uy
      integer	ixd,iyd,ierr
      logical	exit
      character	device*(*)
*
* IF ARGS IS BEING USED, CALL ROUTINE XYPAN WHICH USES ARGSLIB ETC
*
      if(device.eq.'ARGS') then
         call xypan(ixd,iyd,ux,uy,exit,ierr)
*
* IF IKON IS BEING USED, CALL ROUTINE XYIKON WHICH USES GKS/SGS AND A
* LITTLE IKON NATIVE CODE
*
      else if(device.eq.'IKON') then
         call xyikon(ixd,iyd,ux,uy,exit)
         ierr=0
      endif
*
* FINISH
*
      end
