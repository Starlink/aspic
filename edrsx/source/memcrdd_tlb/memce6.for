      subroutine memce6(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Sets up the internal structures required by MEMCRDD and 
*	to destripe data.
*
*SOURCE
*       MEMCE6.FOR in MEMCRDD.TLB
*
*ARGUMENTS       
*   INPUTS:
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/B6_COM/,/B0_COM/,/ME_COM/
*   WRITE:
*	/ME_COM/
*		ME_mj		No. of pixels in an image
*
*SUBROUTINES CALLED
*       THIS PACKAGE (MEMCRDD.TLB):
*              memcb5,memcb6,memcb7,memcc1
*
*VAX SPECIFICS
*       implicit none
*	%val
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 19/12/89
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

* ... DESCRIPTORS FOR WORK IMAGE
      include '(B0_COM)'

* ... POINTERS TO INTERNAL FILES
      include '(B6_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr

*
* DECLARE LOCAL VARIABLES
*
      integer	offset	! Offset into a data set

*
* IF INHERITED STATUS INDICATES AN ERROR, EXIT IMMEDIATELY
*
      if(ierr.ne.0) goto 999

*
* SET UP NO. OF PIXELS IN THE WORK IMAGE
*
      ME_mj=B0_nps*B0_nls

*
* IDENTIFY USABLE SAMPLES, AND STORE DATA RELATING TO SUCH SAMPLES AT
* THE LOW END OF THE MAIN STORAGE ARRAY ME_ST. THIS DATA IS STORED IN
* THE ORDER IN WHICH THE SAMPLES WERE READ FROM THE INPUT CRDD FILES.
* SOME OTHER RELATED VALUES ARE CALCULATED AND STORED IN /B5_COM/.
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
* CONVERT THE DATA FROM JY TO JY/ST VALUES
*
      do offset=0,ME_mk-1
         ME_st(ME_kb(21)+offset)=ME_st(ME_kb(21)+offset)/
     :                           ME_st(B6_sol+offset)
      enddo
         
*
* FINISH
*
  999 continue

      end
