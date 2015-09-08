      subroutine transp(in,m,n,out)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Transposes a real 2d array.
*
*SOURCE
*       TRANSP.FOR in UTILITIES.TLB
*
*METHOD
*       Do a direct assignment from input to output to save on page
*       faults.
*
*ARGUMENTS
*   INPUTS:
*       in(m,n)         real    Input array
*       m,n             integer size of input array
*   OUTPUTS:
*       out(n,m)        real    The output array
*
*SUBROUTINES CALLED
*       none
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 12/5/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   m,n
      real      in(m,n),out(n,m)
*
* DECLARE LOCAL VARIABLES
*
      integer   i       ! Input pixel and output line count
      integer   j       ! Output pixel and input line count
*
* JUST DO IT
*
      do j=1,n
         do i=1,m
            out(j,i)=in(i,j)
         enddo
      enddo
*
* FINISH
*
      end
