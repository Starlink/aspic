      subroutine agchax(iflg,iaxs,iprt,vils)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Sets the NCAR pen colour during the drawing of an AUTOGRAPH
*       background.
*
*SOURCE
*       AGCHAX.FOR in UTILITIES.TLB
*
*METHOD
*       This routine is called by AUTOGRAPH several times during the
*       production of a background, just before and just after each
*       of the objects making up an axis is drawn. The arguments are
*       provided by AUTOGRAPH and determine what stage the drawing of
*       the background has reached.
*
*ARGUMENTS
*   INPUTS:
*       iflg    integer         0 if object is about to be drawn, non-
*                               zero if it has just been drawn.
*       iaxs    integer         Axis identification. 1,2,3,4 correspond
*                               to left, right, bottom and top axes.
*       iprt    integer         Part of axis being drawn:
*                                  1 - line of the axis
*                                  2 - a major tick
*                                  3 - a minor tick
*                                  4 - mantissa of numeric label
*                                  5 - exponent of numeric label
*       vils    real            Co-ordinate at which object is being
*                               drawn (X or Y determined by iaxs).
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              ncrcol
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*       tabs
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 22/3/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   iflg,iaxs,iprt
      real      vils
*
* THE LINE AND THE TICK MARKS ARE DRAWN IN GREEN (REVERT TO WHITE AFTER
* THEY HAVE BEEN DRAWN)
*
      if(iprt.le.3) then
         if(iflg.eq.0) then
            call ncrcol('GREEN')
         else
            call ncrcol('WHITE')
         endif
      endif
*
* FINISH
*
      end
