      subroutine getrms( m, h, a, b, rms )
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	
*	
*
*SOURCE
*       GETRMS.FOR in SOURCEFIT.TLB
*
*METHOD
*       
*ARGUMENTS       
*   INPUT:
*
*   OUTPUTS:
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              
*       THIS PACKAGE (MEMCRDD.TLB):
*              
*       EDRS:
*              
*       INTERIM:
*              
*STARLINK PARAMETERS
*
*
*VAX SPECIFICS
*       implicit none
*       %val
*       do while
*       REAL*8
*       Trig functions in degrees
*       enddo
*       end of line comments
*       2 byte integer values
*       RTL routines
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 13/9/91
*-------------------------------------------------------------------
      implicit none

*
* DECLARE ARGUMENTS
*
      integer	m
      real*8	h(m),a,b,rms

*
* DECLARE LOCAL VARIABLES
*
      integer	i
      real*8	sum

*
* FIND THE RMS SCALED RESIDUAL
*
      sum = 0.0

      do i = 1, m
         sum = sum + ( h(i)*b + a )**2
      end do

      rms = sqrt( sum/m )

*
* FINISH
*
  999 continue

      end
