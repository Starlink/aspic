      subroutine memcf6(backfl,mask,back,pixsol,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Integrates the background flux within an analysis mask
*
*SOURCE
*       MEMCF6.FOR in MEMCRDD.TLB
*
*ARGUMENTS       
*   INPUT:
*	mask(ME_mj) real	Analysis mask
*	back(ME_mj) real	Background image (Jy/St)
*	pixsol	    real	Solid angle of one pixel (St)
*	ierr	    integer	Inherited status: 0 - "OK so far"
*   OUTPUTS:
*	backfl	    real	Integrated flux (Jy)
*       ierr        integer     Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/ME_COM/
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
*       D.S. Berry (MAVAD::DSB) 26/1/90
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE GLOBAL PARAMETER DECLARATIONS
*
      include '(PR_DEC)'

*
* INCLUDE COMMON BLOCKS HOLDING...

* ... MEMSYS3 INFO
      include '(ME_COM)'

*
* DECLARE ARGUMENTS
*
      real	mask(ME_mj),back(ME_mj),backfl,pixsol
      integer	ierr

*
* DECLARE LOCAL VARIABLES
*
      integer	pixel	! Pixel counter
      real	sum	! Sum of surface brigtness values (Jy/St)

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* DO THE SUMMATION
*
      sum=0.0
      do pixel=1,ME_mj
         sum=sum+mask(pixel)*back(pixel)
      enddo
      backfl=sum*pixsol

*
* FINISH
*
  999 continue

      end
