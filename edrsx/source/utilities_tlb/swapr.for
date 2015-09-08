      subroutine swapr(a,b)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To swap 2 real numbers
*
*SOURCE
*       SWAPR.FOR in UTILITIES.TLB
*
*ARGUMENTS
*   INPUTS:
*       a       real            1st number
*       b       real            2nd number
*   OUTPUTS:
*       a       real            2nd number
*       b       real            1st number
*
*USED BY
*       CRDDTRACE
*
*VAX SPECIFICS
*       end of line comments
*       implicit none
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 14/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      real      a,b
*
* DECLARE LOCAL VARIABLES
*
      real      temp    ! Temporary storage
*
* DO IT
*
      temp=a
      a=b
      b=temp

      end
