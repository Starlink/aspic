      subroutine sumout( fits, npix, nlin, npeak, out, back, inval )
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	
*	
*
*SOURCE
*       SRCSUM.FOR in SOURCEFIT.TLB
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
      integer	npix,nlin,npeak,back,inval
      integer*2 out(npix,nlin)
      real	fits(npix,nlin,npeak)

*
* DECLARE LOCAL VARIABLES
*
      integer	i
      integer   ival
      integer	lin
      integer   pix

*
* INITIALISE THE OUTPUT ARRAY TO THE BACKGROUND VALUE.
*
      if( back .ge. -32767 .and. back .le. 32767 ) then
         ival = back
      else
         ival = inval
      end if

      do lin = 1, nlin
         do pix = 1, npix
            out( pix, lin ) = ival
         end do
      end do

*
* LOOP ROUND EACH PEAK.
*
      do i = 1, npeak

*
* LOOP ROUND EACH VALID PIXEL IN THE OUTPUT IMAGE.
*
         do lin = 1, nlin
            do pix = 1, npix      

               if( out( pix, lin ) .ne. inval ) then

*
* EVALUATE THE INCREMENTED OUTPUT VALUE.
*
                  ival = out( pix, lin ) + nint( fits( pix, lin, i ) )

*
* IF THE PIXEL HAS SATURATED, SET IT INVALID.
*
                  if( ival .le. 32767 .and. ival .ge. -32767 ) then
                     out( pix, lin ) = ival

                  else
                     out( pix, lin ) = inval

                  end if
                     
               end if
            end do
         end do
      end do

*
* FINISH
*
      end
