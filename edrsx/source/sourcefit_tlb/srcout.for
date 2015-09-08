      subroutine srcout( fits, npix, nlin, npeak, peak, out, back, 
     :                   inval )
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	
*	
*
*SOURCE
*       SRCOUT.FOR in SOURCEFIT.TLB
*
*METHOD
*       
*ARGUMENTS       
*   INPUT:
*
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
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
      integer	npix,nlin,npeak,peak,back,inval
      integer*2 out(npix,nlin)
      real	fits(npix,nlin,npeak)

*
* DECLARE LOCAL VARIABLES
*
      real	fitval
      integer	lin
      integer   outval
      integer   pix

*
* LOOP ROUND EACH PIXEL IN THE OUTPUT IMAGE.
*
      do lin = 1, nlin
         do pix = 1, npix      

*
* IF THE FITTED VALUE IS LESS THAN ZERO, SET TEH OUTPUT INVALID.
*
            fitval = fits( pix, lin, peak )

            if( fitval .lt. 0.0 ) then
               out( pix, lin ) = inval

*
* OTHERWISE STORE THE FITTED VALUE WITH ADDED BACKGROUND SO LONG AS IT
* IS WITHIN THE I*2 RANGE.
*
            else
               outval = nint( fitval + back )

               if( outval .ge. -32767 .and. outval .le. 32767 ) then
                  out( pix, lin ) = outval 
               else
                  out( pix, lin ) = inval
               end if

            end if

         end do
      end do

*
* FINISH
*
      end
