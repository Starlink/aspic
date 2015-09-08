      subroutine ikoncl
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Closes the ikon after use by program XYCUR
*       
*METHOD
*	This routine just calls SGS_CLOSE.
*
*ARGUMENTS       
*	none
*
*SUBROUTINES CALLED
*	SGS:
*		SGS_CLOSE
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
* CLOSE SGS
*
      call sgs_close
*
* FINISH
*
      end
