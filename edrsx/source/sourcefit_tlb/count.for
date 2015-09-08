      subroutine count( npix, nlin, image, inval, xlo, xhi, ylo, yhi, 
     :                  nval )
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	
*	
*
*SOURCE
*       COUNT.FOR in SOURCEFIT.TLB
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
*       D.S. Berry (MAVAD::DSB) 12/9/91
*-------------------------------------------------------------------
      implicit none

*
* DECLARE ARGUMENTS
*
      integer	npix, nlin, inval, nval, ylo, yhi, xlo, xhi
      integer*2 image( npix, nlin )

*
* DECLARE LOCAL VARIABLES
*
      integer	lin
      integer	pix

*
* LOOP ROUND ALL PIXELS IN THE REQUIRED REGION, COUNTING THE NUMBER OF 
* GOOD PIXELS.
*
      nval = 0

      do lin = ylo, yhi
         do pix = xlo, xhi
            if( image( pix, lin ) .ne. inval ) nval = nval + 1
         end do
      end do

*
* FINISH
*
      end
