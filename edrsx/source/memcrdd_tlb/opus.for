      subroutine opus(j,k)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Calculate a data set from a given image.
*
*SOURCE
*       OPUS.FOR in MEMCRDD.TLB
*
*METHOD
*	Call MEMCD1 to do the work.
*       
*ARGUMENTS       
*   INPUT:
*	j	integer		Internal file no. of the input image
*	k	integer		Internal file no. of the output data set
*
*COMMON USAGE
*   READ:
*	/B6_COM/,/ME_COM/,/ZZ_COM/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*	       wruser
*       THIS PACKAGE (MEMCRDD.TLB):
*              memcd1
*	EDRS:
*	       lbgone
*
*VAX SPECIFICS
*       implicit none
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 6/10/89
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

* ... POINTERS TO MEMCRDD INTERNAL FILES
      include '(B6_COM)'

* ... MEMSYS3 COMMON BLOCKS
      include '(ME_COM)'

* ... USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	j,k

*
* DECLARE LOCAL VARIABLES
*
      integer	istat	! Local error status
      character	prbuf*80! Buffer for screen output

*
* IF REQUIRED TELL THE USER WHAT IS GOING ON
*
      if(ZZ_ilv.ge.5) then
         write(prbuf,10) ME_ntr+1
  10     format('    Entering OPUS   (transform no. ',I10,' )')
         call lbgone(prbuf(36:))
         call wruser(prbuf,istat)
      endif

*
* CALL MEMCD1 TO DO THE WORK
*
      call memcd1(ME_st(ME_kb(j)),ME_st(ME_kb(k)),ME_st(B6_wk1),
     :            ME_st(B6_wk2),ME_st(B6_wk3),ME_st(B6_wk4))

*
* FINISH
*
      end
