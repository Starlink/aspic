      integer function lint(x)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Returns x if x is a whole number, or the most positive
*       integer which more negative than x, if x is not a whole number.
*
*SOURCE
*       LINT.FOR in UTILITIES.TLB
*
*ARGUMENTS
*   INPUTS:
*       x       real    The input real number
*   FUNCTION VALUE:
*       hint    integer The next lower integer
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
      lint=int(x)
      if(x.lt.0.0) lint=lint-1
*
* FINISH
*
      end
