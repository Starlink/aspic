      subroutine ratout( fits, npix, nlin, npeak, peak, out, inval,
     :                   cutoff, rscale, rzero )
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	
*	
*
*SOURCE
*       RATOUT.FOR in SOURCEFIT.TLB
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
      integer	npix,nlin,npeak,peak,inval
      integer*2 out(npix,nlin)
      real	cutoff,fits(npix,nlin,npeak),rscale,rzero

*
* DECLARE LOCAL VARIABLES
*
      real	fitval
      logical 	good
      integer	i
      integer	lin
      integer   pix
      real	sum

*
* SET UP THE SCALE AND ZERO FACTORS FOR THE OUTPUT IMAGE SUCH THAT 
* THE DATA RANGE [0.0,1.0] IS MAPPED ONTO THE INTEGER RANGE 
* [-29940,29940].
*
      rscale = 1.67E-5
      rzero = 0.5

*
* LOOP ROUND EACH PIXEL IN THE OUTPUT IMAGE.
*
      do lin = 1, nlin
         do pix = 1, npix      

*
* FIND THE SUM OF THE SOURCES AT THIS PIXEL.
*
            sum = 0.0
            good = .true.

            do i = 1, npeak
               fitval = fits( pix, lin, i )
               sum = sum + fitval
               if( fitval .lt. 0.0 ) good =.false.
            end do

*
* IF ALL SOURCES WERE VALID, STORE THE RATIO. OTHERWISE STORE AN
* INVALID FLAG VALUE.
*
            if( good ) then

               fitval = fits( pix, lin, peak )
               if( fitval .gt. cutoff .and. fitval .le. sum ) then
                  out( pix, lin ) = nint( ( (fitval/sum) - 0.5)
     :                                       *59880.23952 )

               else
                  out( pix, lin ) = inval

               end if

            else
               out( pix, lin ) = inval

            end if

         end do
      end do

*
* FINISH
*
      end
