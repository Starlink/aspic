      subroutine memcf5(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Add the stored background onto the image in file 1 
*
*SOURCE
*       MEMCF5.FOR in MEMCRDD.TLB
*
*ARGUMENTS       
*   INPUT:
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/B2_COM/,/B6_COM/,/ME_COM/,/ZZ_COM/
*   WRITE:
*	/ME_COM/
*
*SUBROUTINES CALLED
*	none
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
* INCLUDE IRAS MISSION PARAMETERS
*
      include 'UTILITIES(IR_PAR)'

*
* INCLUDE COMMON BLOCKS HOLDING...

* ... POINTERS TO INTERNAL FILES
      include '(B6_COM)'

* ... MEMSYS3 INFO
      include '(ME_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr

*
* DECLARE LOCAL VARIABLES
*
      real	back	! Background value
      integer	istat	! Temporary status value
      integer	offset	! Offset into an internal data set of image

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* ADD THE STORED BACKGROUND BACK ON TO THE IMAGE
*
      do offset=0,ME_mj-1
         back=ME_st(B6_bac+offset)
         if(back.ne.PR_rin) then
            ME_st(ME_kb(1)+offset)=ME_st(ME_kb(1)+offset)+back
         else
            ME_st(ME_kb(1)+offset)=PR_rin
         endif
      enddo

*
* FINISH
*
  999 continue

      end
