      subroutine ovpoly(x,y,n,device)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Plot a polyline on an ARGS or IKON overlay plane.
*METHOD
*       Use ARGS database routines for ARGS, GKS for IKON. 
*       ARGS or IKON should have been previously opened with
*       ARGSOP or IKONOP, and overlay planes opened with
*       OVOPEN.
*       
*ARGUMENTS       
*   INPUTS:
*       n	integer		No. of points in polyline
*       x(n)	real		Array of x co-ords
*       y(n)	real		Array of y co-ords
*       device	character	Device to use: ARGS or IKON
*
*SUBROUTINES CALLED
*	ARGS_:
*		ARGS_POLYL
*	GKS:
*		GPL
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
      integer	n
      real	x(n),y(n)
      character	device*(*)
*
* DECLARE LOCAL VARIABLE
*
      integer	ierr	! Error status from args_polyl
      real	x1,x2,y1,y2,xm,ym! Extent of current zone in world
                        ! coordinates.
*
* INCLUDE COMMON BLOCK HOLDING ARGS DATABASE INFO (WRITTEN BY ROUTINE
* ARGSOP)
*
      include 'ARGSCOM.FOR'
*
* IF AN ARGS IS BEING USED, CALL ARGS_POLYL
*
      if(device.eq.'ARGS') then
         call args_polyl(imlast,n,x,y,ierr)
*
* IF AN IKON IS BEING USED, USE GKS ROUTINE GPL
*
      else if(device.eq.'IKON') then
         call sgs_izone( x1, x2, y1, y2, xm, ym )
         call sgs_sw( x1+0.5, x2+0.5, y1+0.5, y2+0.5, ierr)
         call gpl(n,x,y)
         call sgs_sw( x1, x2, y1, y2, ierr)
      endif
*
* FINISH
*
      end
