      subroutine srcgen( fit, npix, nlin, x, sum )
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	
*	
*
*SOURCE
*       SRCGEN.FOR in SOURCEFIT.TLB
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
*       D.S. Berry (MAVAD::DSB) 15/9/91
*-------------------------------------------------------------------
      implicit none

*
* DECLARE ARGUMENTS
*
      integer	npix, nlin
      real	fit( npix, nlin ),sum
      real*8    x(7)

*
* DECLARE LOCAL VARIABLES
*
      real*8	arg
      real      fitval
      integer	lin
      integer	pix
      
*
* INITIALIZE THE TOTAL DATA SUM TO ZERO.
*
      sum = 0.0

*
* IF ANY OF THE PARAMETERS ARE INVALID, SET THE WHOLE FIT TO A NEGATIVE
* VALUE TO INDICATE THAT IT IS INVALID.
*
      if( x(2) .le. 0.0 .or. x(4) .le. 0.0 .or. x(7) .le. 0.0 ) then
         do lin = 1, nlin
            do pix = 1, npix
               fit( pix, lin ) = -10000.0
            end do
         end do

*
* OTHERWISE, ASSIGN THE FITTED SOURCE VALUES TO THE OUTPUT ARRAY.
*
      else
         do lin = 1, nlin
            do pix = 1, npix

               arg =   x(2)*(pix-x(3))**2
     :               + x(4)*(lin-x(5))**2
     :               + x(6)*(pix-x(3))*(lin-x(5))

               if( arg .ge. 0.0 ) then
                  fitval = x(1)*exp( -arg**x(7) )
                  sum = sum + fitval
                  fit( pix, lin ) = fitval

               else
                  fit( pix, lin ) =-10000

               end if

            end do
         end do
      end if

*
* FINISH
*
      end
