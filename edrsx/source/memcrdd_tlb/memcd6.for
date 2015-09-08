      subroutine memcd6(input,insiz,output,outsiz,start,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Copies an input 1D integr array to a specified part of an
*	output 1D integer array.
*
*SOURCE
*       MEMCD6.FOR in MEMCRDD.TLB
*
*ARGUMENTS       
*   INPUT:
*	input(insiz) integer	Input array
*	insiz	     integer	Size of input array
*	outsiz	     integer	Size of output array
*	start	     integer	Pointer to location within the output 
*				array at which the input is to be copied
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
* CHECK OUTPUT ARRAY WILL NOT OVERFLOW
*
      if(start+insiz-1.gt.outsiz.or.start.lt.1) then
         ierr=1

*
* OTHERWISE, COPY THE DATA AND UPDATE THE POINTER TO THE NEXT FREE 
* LOCATION IN THE OUTPUT
*
      else
         do offset=0,insiz-1
            output(start+offset)=input(1+offset)
         enddo
         start=start+insiz
      endif

*
* FINISH
*
  999 continue

      end
