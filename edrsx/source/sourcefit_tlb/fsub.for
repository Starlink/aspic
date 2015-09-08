      subroutine fsub( image, npix, nlin, rms )
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	
*	
*
*SOURCE
*       FSUB.FOR in SOURCEFIT.TLB
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
* INCLUDE LOCAL PARAMETER DECLARATIONS
*
      include '(SF_PAR)'

*
* INCLUDE COMMON BLOCK
*
      include '(SF_COM)'

*
* DECLARE ARGUMENTS
*
      integer	npix,nlin
      integer*2 image( npix, nlin )
      real      rms

*
* DECLARE LOCAL VARIABLES
*
      real	arg
      real	bcksum
      integer	i
      integer   ival
      integer   lin
      integer	pix
      real	source
      real	srcsum
      real	ssr
      real	sw
      real	wgt


*
* INITIALISE THE SUM OF SQUARED RESIDUALS AND THE SUM OF THE WEIGHTS.
*
      SSR = 0.0
      SW = 0.0

*
* LOOP ROUND EACH PIXEL IN THE REQUESTED IMAGE AREA
*
      do lin = SF_ylo, SF_yhi
         do pix = SF_xlo, SF_xhi

*
* PASS ON IF THIS PIXEL IS INVALID.
*
            ival = image(pix,lin) 
            if( ival .ne. SF_inv )then

*
* LOOP ROUND EACH PEAK.
*
               srcsum = 0.0
               bcksum = 0.0
               do i = 1, SF_npk

*
* EVALUE THE CONTRIBUTION FROM THE CURRENT PEAK TO THE CURRENT PIXEL
* VALUE.
*
                  arg = ( (pix-SF_xc(i))**2 + (lin-SF_yc(i))**2 )/
     :                  ( SF_rad(i)**2 )

                  if( arg .eq. 0.0 .and. SF_pwr(i) .lt. 0.0 ) then
                     source = 0.0

                  else
                     source = SF_pk(i)*exp( -( arg**(0.5*SF_pwr(i))))
                  end if

*
* INCREMENT THE SUM OF THE SOURCES AND THE SUM OF THE BACKGROUNDS.
*
                  srcsum = srcsum + source
                  bcksum = bcksum + SF_bck(i)

               end do

*
* EVALUATE THE WEIGHT FOR THIS CONTRIBUTION TO THE SUM OF THE SQUARED
* RESDIUALS, AND INCREMENT THE SUM OF THE WEIGHTS.
*
               wgt = 1.0
               sw = sw + wgt

*
* INCREMENT THE SUM OF SQUARED RESIDUALS.
*
               ssr = ssr + wgt*( ival - (srcsum+bcksum) )**2

            end if

         end do
      end do

*
* EVALUATE THE RMS RESIDUAL.
*
      if( sw .ne. 0.0 ) then
         rms = sqrt( ssr/sw )

      else
         call wrerr('NOVALID')
         rms = -1.0E6

      end if 

*
* FINISH
*
      end
