      subroutine ovcros(x,y,lx,ly,device)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Plot a cross on an ARGS or an IKON overlay plane.
*
*METHOD
*       Call routine across to deal with ARGS, use GKS to deal
*       with IKON.
*       
*ARGUMENTS       
*   INPUTS:
*       x,y	real		Co-ords of cross centre in image 
*                               coordinates (i.e. centre of bottom left
*                               pixel should be given as (1,1) ).
*       lx,ly   integer		Length of cross arms in device units 
*				(pixels)(on an IKON user units are used)
*       device	character*(*)	Name of device: ARGS or IKON
*
*SUBROUTINES CALLED
*       THIS PACKAGE:
*              across
*	GKS:
*	       gpl
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 29/6/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      real	x,y
      integer	lx,ly
      character device*(*)
*
* DECLARE LOCAL VARIABLES
*
      real	dxw	! Size in x of a single pixel.
      real	dyw	! Size in y of a single pixel.
      real	xt(2)	! Array holding x co-ords of arm ends
      real	yt(2)	! Array holding y co-ords of arm ends
*
* INCLUDE COMMON BLOCK HOLDING ARGS DATABASE INFO, WRITTEN BY
* ROUTINE ARGSOP
*
      include 'ARGSCOM.FOR'
*
* IF ARGS, CALL ROUTINE ACROSS TO PLOT THE CROSS
*
      if(device.eq.'ARGS') then
         call across(imlast,x,y,lx,ly)
*
* IF IKON, USE GKS TO DRAW CROSS
*
      else if(device.eq.'IKON') then

*
* GET THE SIZE IN WORLD COORDINATES OF A SINGLE PIXEL.
*
         call sgs_idun( dxw, dyw )

         xt(1)=x-0.5-dxw*real(lx)/2.0
         xt(2)=x-0.5+dxw*real(lx)/2.0
         yt(1)=y-0.5
         yt(2)=y-0.5
         call gpl(2,xt,yt)
         yt(1)=y-0.5-dyw*real(ly)/2.0
         yt(2)=y-0.5+dyw*real(ly)/2.0
         xt(1)=x-0.5
         xt(2)=x-0.5
         call gpl(2,xt,yt)
      endif
*
* FINISH
*
      end
