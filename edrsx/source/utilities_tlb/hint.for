      integer function hint(x)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Returns the x if x is a whole number, or the most negative
*       integer which more positive than x, if x is not a whole number.
*
*SOURCE
*       HINT.FOR in UTILITIES.TLB
*
*ARGUMENTS
*   INPUTS:
*       x       real    The input real number
*   FUNCTION VALUE:
*       hint    integer The next higher integer
*
*USED BY
*       CONVOLVE
*
*SUBROUTINES CALLED
*       none
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 13/11/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      real x
*
* DO IT
*
      hint=int(x)
      if(x.gt.0.0) hint=hint+1
*
* FINISH
*
      end
