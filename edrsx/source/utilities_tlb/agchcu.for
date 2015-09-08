      subroutine agchcu(iflg,kdsh)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Sets the NCAR pen colour during the drawing of an AUTOGRAPH
*       curve.
*
*SOURCE
*       AGCHCU.FOR in UTILITIES.TLB
*
*METHOD
*       This routine is called by AUTOGRAPH just before and just after 
*       each curve is drawn. 
*
*ARGUMENTS
*   INPUTS:
*       iflg    integer         0 if curve is about to be drawn, non-
*                               zero if it has just been drawn.
*       kdsh    integer         1 if curve drawn by EZY or EZXY;
*                               n or -n, where n is the curve number if
*                               drawn by EZMY or EZMXY;
*                               AGCURV argument KDSH if curve drawn by
*                               AGCURV.
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
      integer   iflg,kdsh

*
* INCLUDE NCAR COMMON BLOCKS CONTAINING COLOUT DEFINITIONS.
*
      include 'UTILITIES(NC_COM)'

*
* IF CURVE IS ABOUT TO BE DRAWN...
*
      if(iflg.eq.0) then

*
* SET UP A COLOUR FOR THE CURVE.
*
         call ncrcol(NC_cnm( mod(abs(kdsh)-1,7)+1 ) )

*
* RESET THE COLOUR TO WHIT ONCE THE CURVE HAS BEEN DRAWN.
*
      else
         call ncrcol('WHITE')
      endif

*
* FINISH
*
      end
