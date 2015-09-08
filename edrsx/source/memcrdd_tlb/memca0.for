      subroutine memca0(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Sets up the internal structures required by MEMCRDD and 
*	to generate simulated data.
*
*SOURCE
*       MEMCA0.FOR in MEMCRDD.TLB
*
*ARGUMENTS       
*   INPUTS:
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/A9_COM/,/B0_COM/
*   WRITE:
*	/ME_COM/
*		ME_mj		No. of pixels in an image
*
*SUBROUTINES CALLED
*       THIS PACKAGE (MEMCRDD.TLB):
*              memcb5,memcb6,memcb7,memcc1,memcd3,memce0,memce1
*
*VAX SPECIFICS
*       implicit none
*	%val
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 7/11/89
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE GLOBAL PARAMETER DECLARATIONS
*
      include '(PR_DEC)'

*
* INCLUDE COMMON BLOCKS HOLDING...

* ... MEMSYS3 DATA
      include '(ME_COM)'

* ... TRIAL SKY POINTER 
      include '(A9_COM)'

* ... TRIAL SKY FRAME DESCRIPTORS
      include '(B0_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr

*
* IF INHERITED STATUS INDICATES AN ERROR, EXIT IMMEDIATELY
*
      if(ierr.ne.0) goto 999

*
* SET UP NO. OF PIXELS IN THE TRIAL SKY IMAGE
*
      ME_mj=B0_nps*B0_nls

*
* IDENTIFY USABLE SAMPLES, AND STORE DATA RELATING TO SUCH SAMPLES AT
* THE LOW END OF THE MAIN STORAGE ARRAY ME_ST. THIS DATA IS STORED IN
* THE ORDER IN WHICH THE SAMPLES WERE READ FROM THE INPUT CRDD FILES.
* SOME OTHER RELATED VALUES ARE CALCULATED AND STORED IN /B5_COM/.
* NB, ONLY "USABLE" INPUT CRDD SAMPLES ARE SIMULATED, OTHERS ARE SET
* TO THE BLANK VALUE IN THE OUTPUT CRDD FILES.
*
      call memcb5(ierr)

*
* SET UP POINTERS TO THE START OF ALL INTERNAL MEM FILE, AND ALSO TO 
* WORK SPACE REQUIRED BY MEMCRDD
*
      call memcb6(ierr)

*
* REORDER THE DATA STORED IN ME_ST BY ROUTINE MEMCB5, BY GROUP NUMBER.
* THIS RESULTS IN SAMPLES BELONGING TO THE SAME SAMPLE GROUP BEING 
* STORED NEXT TO EACH OTHER. 
*
      call memcb7(ierr)

*
* SET UP THE FOURIER TRANSFORM OF EACH SAMPLE GROUPS PSF
*
      call memcc1(ierr)

*
* COPY THE INPUT TRIAL SKY FROM DISK INTO FILE 1, AND SET UP THE INVALID
* PIXEL MASK IN FILE 2
*
      call memce0(%val(A9_pin),ME_st(ME_kb(1)),ME_st(ME_kb(2)),ierr)

*
* MODIFY THE SAMPLE CENTRE COORDINATES TO INTRODUCE A GAUSSIAN POINTING 
* ERROR WITH SIGMA GIVEN BY THE USER
*
      call memce1(ierr)

*
* FINISH
*
  999 continue

      end
