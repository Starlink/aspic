*      module E0_COM
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Declares common block /E0_COM/ holding data written by
*	module MEMCE0.
*
*SOURCE
*       E0_COM.INC in MEMCRDD.TLB
*
*COMMON USAGE
*	/E0_COM/,
*		E0_val	The no. of valid pixels in the trial sky image
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 7/11/89
*-------------------------------------------------------------------
*
* DECLARE PARAMETER HOLDING SIZE OF COMMON BLOCK (IN UNITS OF 4 BYTES)
* AND ALSO AN INTEGER WHICH WILL OVERLAY THE START OF THE COMMON BLOCK
*
      integer	E0_cm,E0_sz
      parameter (E0_sz=1)

*
* DECLARE CONTENTS OF COMMON BLOCK /E0_COM/
*
      real	E0_val	! The no. of valid pixels in the trial sky 
			! image

*
* DECLARE THE COMMON BLOCK
*
      common /E0_COM/ E0_val
      equivalence (E0_val,E0_cm)
