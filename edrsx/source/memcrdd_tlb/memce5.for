      subroutine memce5(file,null,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Calculates the coverage image.
*	
*SOURCE
*       MEMCE5.FOR in MEMCRDD.TLB
*
*METHOD
*	If the sky was set a constant value of 1 Jy/st, then the 
*	corresponding data samples would have a value in Jy equal to 
*	their solid angles in Steradians. Set up such a data set and
*	apply TROPUS to it. This gives an image in which each pixel 
*	has a value equal to the total solid angle contributing to 
*	the pixel.
*       
*ARGUMENTS       
*   INPUT:
*	null	logical		If true, user may choose not to 
*				generate a BDF holding the coverage image
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*	file(ME_mj) real	The coverage image
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/B6_COM/,/ME_COM/
*   WRITE:
*	/ME_COM/  (files 2 and 23 are used as work space)
*
*SUBROUTINES CALLED
*       THIS PACKAGE (MEMCRDD.TLB):
*              tropus,memca2
*              
*STARLINK PARAMETERS
*	COVER		Name of BDF to receive coverage image
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 18/12/89
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE GLOBAL PARAMETER DECLARATIONS
*
      include '(PR_DEC)'

*
* INCLUDE IRAS MISSION PARAMETERS
*
      include 'UTILITIES(IR_PAR)'

*
* INCLUDE COMMON BLOCKS HOLDING...

* ... POINTER TO START OF COVERAGE IMAGE STORE
      include '(B6_COM)'

* ... MEMSYS3 INFORMATION
      include '(ME_COM)'

*
* DECLARE ARGUMENTS
*
      logical	null
      integer	ierr
      real	file(ME_mj)

*
* DECLARE LOCAL VARIABLES
*
      real	cover	! Value of a pixel from coverage image
      real	covlim	! Lowest reliable coverage value
      real	maxcov	! Maximum coverage value
      integer	offset	! Offset into a data set or image

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* SET FILE 23 TO ALL ONES
*
      do offset=0,ME_mk-1
         ME_st(ME_kb(23)+offset)=1.0
      enddo

*
* APPLY TROPUS TO FILE 23 TO GET COVERAGE IMAGE IN FILE 2
*
      call tropus(23,2)

*
* COVERAGE BELOW 0.00001 OF THE MAXIMUM IS CONSIDERED TO BE ZERO 
* SINCE TROPUS INTRODUCES ROUNDING ERRORS. FIND THE LOWER LIMIT OF
* USABLE COVERAGE VALUES.
*
      maxcov=0.0
      do offset=0,ME_mj-1
         maxcov=max(maxcov,ME_st(ME_kb(2)+offset))
      enddo
      covlim=0.00001*maxcov

*
* COPY THE COVERAGE IMAGE TO THE OUTPUT FILE AND SET ALL VALUES BELOW 
* THE LOWER LIMIT, TO ZERO.
*
      do offset=0,ME_mj-1
         cover=ME_st(ME_kb(2)+offset)
         if(cover.lt.covlim) then
            file(1+offset)=0.0
         else
            file(1+offset)=cover
         endif
      enddo

*
* SEE IF USER WANTS TO OUTPUT COVERAGE IMAGE TO DISK
*
      call memca2('COVER',2,null,ierr)

*
* FINISH
*
  999 continue

      end
