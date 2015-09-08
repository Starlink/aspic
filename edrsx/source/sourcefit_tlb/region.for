      subroutine region( npeak, x0, y0, npix, nlin, fwhm, xlims, ylims )
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	
*	
*
*SOURCE
*       REGION.FOR in SOURCEFIT.TLB
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
      integer	npeak,nlin,npix
      real      x0(npeak),y0(npeak),fwhm,xlims(2),ylims(2)

*
* DECLARE LOCAL VARIABLES
*
      integer	i

*
* INITIALISE THE BOUNDS OF THE REGION TO BE INCLUDED IN THE FIT.
*
      xlims(1) = 1.0E30
      xlims(2) = -1.0E30
      ylims(1) = 1.0E30
      ylims(2) = -1.0E30

*
* LOOP ROUND EACH PEAK.
*
      do i = 1, npeak

*
* UPDATE THE X LIMITS
*
         xlims(1) = min( xlims(1), x0(i) - 2*fwhm )
         xlims(2) = max( xlims(2), x0(i) + 2*fwhm )

*
* UPDATE THE Y LIMITS
*
         ylims(1) = min( ylims(1), y0(i) - 2*fwhm )
         ylims(2) = max( ylims(2), y0(i) + 2*fwhm )

      end do

*
* LIMIT THE REGION TO THE AREA COVERED BY THE IMAGE.
*
      xlims(1) = max( 1.0, xlims(1) )
      xlims(2) = min( real(npix), xlims(2) )

      ylims(1) = max( 1.0, ylims(1) )
      ylims(2) = min( real(nlin), ylims(2) )

*
* FINISH
*
      end
