      subroutine memcd7(output,outsiz,input,insiz,start,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Copies a specified part of an input 1D integer array to an
*	output 1D integer array.
*
*SOURCE
*       MEMCD7.FOR in MEMCRDD.TLB
*
*ARGUMENTS       
*   INPUT:
*	input(insiz) integer	Input array
*	insiz	     integer	Size of input array
*	outsiz	     integer	Size of output array
*	start	     integer	Pointer to first location within the 
*				input array from which to read the output 
*	ierr	     integer	Inherited status: 0 - "OK so far"
*
*   OUTPUTS:
*	output(outsiz) integer  Output array
*	start	     integer	Pointer to next free location in output
*       ierr         integer    Exit status: 0 - success
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
*       D.S. Berry (MAVAD::DSB) 30/10/89
*-------------------------------------------------------------------
      implicit none

*
* DECLARE ARGUMENTS
*
      integer	insiz,outsiz,start,input(insiz),output(outsiz),ierr

*
* DECLARE LOCAL VARIABLES
*
      integer	offset	! Offset into arrays

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* CHECK INPUT ARRAY IS BIG ENOUGH
*
      if(start+outsiz-1.gt.insiz.or.start.lt.1) then
         ierr=1

*
* OTHERWISE, COPY THE DATA AND UPDATE THE POINTER TO THE NEXT UNREAD
* LOCATION IN THE INPUT
*
      else
         do offset=0,outsiz-1
            output(1+offset)=input(start+offset)
         enddo
         start=start+outsiz
      endif

*
* FINISH
*
  999 continue

      end
