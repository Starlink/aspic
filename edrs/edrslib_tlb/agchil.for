      subroutine agchil(iflg,lbnm,lnno)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Sets the NCAR pen colour during the drawing of AUTOGRAPH
*       labels.
*
*METHOD
*       This routine is called by AUTOGRAPH several times during the
*       production of the labels, just before and just after each
*       of the labels is drawn. The arguments are provided by AUTOGRAPH
*       and determine what stage the drawing of the label has reached.
*          This version causes the X and Y labels to be drawn in cyan,
*       and the top label in white.
*
*ARGUMENTS
*   INPUTS:
*       iflg    integer         0 if label is about to be drawn, non-
*                               zero if it has just been drawn.
*       lbnm    character       The name of the label.
*       lnno    integer         The number of the line being drawn.
*
*SUBROUTINES CALLED
*       THIS PACKAGE:
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
      integer   iflg,lnno
      character lbnm*(*)
*
* DRAW BOTTOM AND TOP LABELS IN CYAN AND THE TOP LABEL IN WHITE
* (REVERT TO WHITE AFTER THEY HAVE BEEN DRAWN)
*
      if(iflg.eq.0) then
         if(lbnm.ne.'T') call ncrcol('CYAN')
      else
         if(lbnm.ne.'T') call ncrcol('WHITE')
      endif
*
* FINISH
*
      end
