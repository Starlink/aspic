      subroutine argscl
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Closes the ARGS after use by program XYCUR
*       
*METHOD
*	Reset the ARGS zoom factor to what it was when XYCUR
*	was activated and store the info in the ARGS database.
*
*ARGUMENTS       
*	none
*
*SUBROUTINES CALLED
*       ARGSLIB:
*              SRSEND
*       ASP_:
*              ASP_ITODZ
*       ARGS_:
*              ARGS_FLUSH,ARGS_PUT1,ARGS_WRPAR
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
* DECLARE LOCAL VARIABLES
*
      integer	istat	! Error status
*
* INCLUDE COMMON BLOCK HOLDING ARGS ZOOM STATUS
*
      include 'ARGSCOM.FOR'
*
* RESET THE ARGS ZOOM
*
      call args_flush(4)
      call args_put1('C000'x+ixc)
      call args_put1('A000'x+iyc)
      call args_put1('5001'x)
      call args_put1(256*(iyf-1)+ixf-1)
      call srsend
*
* UPDATE ARGS DATABASE WITH ZOOM STATUS
*
      call asp_itodz('ZXF',ixf,value,istat)
      call asp_itodz('ZYF',iyf,value,istat)
      call asp_itodz('ZXC',ixc,value,istat)
      call asp_itodz('ZYC',iyc,value,istat)
      call args_wrpar('DISPZOOM',value,1,istat)
*
* FINISH
*

      end
