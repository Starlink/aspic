      subroutine memcg4(file,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Smooth the specified internal image file with a Gaussian
*	Intrinsic Correlation Function.
*
*SOURCE
*       MEMCG4.FOR in MEMCRDD.TLB
*
*METHOD
*       Generate an image holding the Gaussian ICF and take it's FFT.
*       Take the FFT of the specified image, and multiply it by the
*       FFT of the ICF. Take the inverse FFT of the product to get
*       the final result.
*
*ARGUMENTS       
*   INPUT:
*       file    integer		MEMSYS3 internal file number.
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/B0_COM/,/B6_COM/,/ME_COM/,/ZZ_COM/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              wruser
*       THIS PACKAGE (MEMCRDD.TLB):
*              memcf7,memcc7,memcc7,memcd0
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 5/9/90
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

* ... IMAGE DIMENSIONS
      include '(B0_COM)'

* ... POINTERS TO INTERNAL FILES
      include '(B6_COM)'

* ... MEMSYS3 INFORMATION
      include '(ME_COM)'

* ... USER PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	file,ierr

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* RETURN IMMEDIATELY IF NO SMOOTHING REQUIRED.
*
      if(ZZ_cfl.le.0.0) goto 999

*
* CHECK THE SPECIFIED FILE IS AN IMAGE AND NOT A DATA SET
*
      if(file.le.0.or.file.ge.21) then
         call wruser(' *** Programming error detected in MEMCG4',ierr)
         ierr=1
         goto 999
      endif

*
* GENERATE THE GAUSSIAN SPOT IN IMAGE "WORK 1"
*
      call memcf7(ZZ_cfl,ME_st(B6_wk1),ierr)

*
* TAKE FFT OF GAUSSIAN SPOT AND STORE IN "WORK 2"
*
      call memcc6(PR_fwd,ME_st(B6_wk1),ME_st(B6_wk2),ME_st(B6_wk1),
     :            B0_nps,B0_nls,ierr)

*
* TAKE FFT OF SPECIFIED IMAGE AND STORE IN "WORK 3"
*
      call memcc6(PR_fwd,ME_st(ME_kb(file)),ME_st(B6_wk3),ME_st(B6_wk1),
     :            B0_nps,B0_nls,ierr)

*
* MULTIPLY THE FFTS AND STORE PRODUCT IN "WORK 1"
*
      call memcd0(ME_st(B6_wk2),ME_st(B6_wk3),ME_st(B6_wk1),B0_nps,
     :            B0_nls)

*
* TAKE INVERSE FFT OF PRODUCT, STORING THE RESULT BACK IN THE SPECIFIED
* FILE
*
      call memcc7(PR_inv,ME_st(B6_wk1),ME_st(ME_kb(file)),ME_st(B6_wk2),
     :            B0_nps,B0_nls,ierr)

*
* FINISH
*
  999 continue

      end
