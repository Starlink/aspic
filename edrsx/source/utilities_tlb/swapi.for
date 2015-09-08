      subroutine swapi(i,j)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To swap 2 integers
*
*SOURCE
*       SWAPI.FOR in UTILITIES.TLB
*
*ARGUMENTS
*   INPUTS:
*       i       integer         1st integer
*       j       integer         2nd integer
*   OUTPUTS:
*       i       integer         2nd integer
*       j       integer         1st integer
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
      integer   i,j
*
* DECLARE LOCAL VARIABLES
*
      integer   temp    ! Temporary storage
*
* DO IT
*
      temp=i
      i=j
      j=temp

      end
