      subroutine memcc5(covage,deflt,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Calculate the low resolution image and optionally
*	write it to disk as an EDRS image.
*
*SOURCE
*       MEMCC5.FOR in MEMCRDD.TLB
*
*METHOD
*	TROPUS produces an image in which each pixel value is the
*	weighted sum of all the data samples that cover the pixel.
*	The weights are related to the PSFs. However the sum of these
*	wieghts is not necessarily unity, and so this image is not a 
*	properly normalised estimate of the sky. The sum of the weights
*	at each pixel can be found by setting all the data samples to 
*	unity, and applying TROPUS again to produce a second image.
*       This is the "coverage" image in which each pixel has the value 
*	of the sum of the weights used to produce the first image. So
*	dividing the first image by the second will produce a correctly
*	normalised estimate of the sky brightness.
*
*ARGUMENTS       
*   INPUT:
*	deflt	logical		True if user may choose not to output
*				lo-res image to disk
*	covage(ME_mj) real	Coverage image
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/B2_COM/,/B6_COM/,/ME_COM/,/ZZ_COM/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              wruser
*       THIS PACKAGE (MEMCRDD.TLB):
*              memca2,tropus
*              
*STARLINK PARAMETERS
*	LORES		The output low resolution image
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 5/10/89
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

* ... INFO ABOUT CRDD FILE
      include '(B2_COM)'

* ... POINTERS TO MEMCRDD FILES HELD IN ARRAY ME_ST
      include '(B6_COM)'

* ... MEMSYS3 INFORMATION
      include '(ME_COM)'

* ... USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr
      logical	deflt
      real	covage(ME_mj)

*
* DECLARE LOCAL VARIABLES
*
      real	covlim  ! Lower limit of acceptable coverage 
                        ! (in steradians).
      real	cover	! Coverage value
      integer	istat	! Temporary status value
      integer	offset	! Offset into an image or data set
      real	omegap	! Solid angle of an image pixel

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* IF REQUIRED TELL USER WHAT IS GOING ON
*
      if(ZZ_ilv.ge.3) then 
         call wruser(' ',istat)
         call wruser('  Calculating Lo-Res image...',istat)
      endif

*
* SET FILE 23 TO SURFACE BRIGHTNESS DATA (FLUX/SOLID ANGLE)
*
      do offset=0,ME_mk-1
         ME_st(ME_kb(23)+offset)=ME_st(ME_kb(21)+offset)
     :                           /ME_st(B6_sol+offset)
      enddo

*
* APPLY TROPUS TO THE SURFACE BRIGHTNESS DATA PUTTING THE ANSWER IN 
* FILE 20
*
      call tropus(23,20)

*
* DIVIDE THE COVERAGE IMAGE INTO FILE 20, TO GET THE PROPERLY NORMALISED
* LO-RES IMAGE. SET PIXEL VALUES TO "INVALID" WHICH ARE LESS THAN THE
* LOWER COVERAGE LIMIT. THE SOLID ANGLE OF AN IMAGE PIXEL IS TAKEN
* AS THE COVERAGE LIMIT
*
      covlim=ZZ_psz*ZZ_psz*0.84616E-7

      do offset=0,ME_mj-1

         cover=covage(1+offset)

         if(cover.gt.covlim) then
            ME_st(ME_kb(20)+offset)=ME_st(ME_kb(20)+offset)/cover
         else
            ME_st(ME_kb(20)+offset)=PR_rin
         endif

      enddo

*
* OUTPUT THE LO_RES IMAGE TO DISK
*
      call memca2('LORES',20,deflt,ierr)

*
* RESET INVALID MODEL PIXELS TO THE VALUE ZERO, TO AVOID MEMSYS3 
* BEING CONFUSED BY THE FLAG VALUES
*
      do offset=0,ME_mj-1

         if(ME_st(ME_kb(20)+offset).eq.PR_rin) then
            ME_st(ME_kb(20)+offset)=0.0
         endif

      enddo               

*
* FINISH
*
  999 continue

      end
