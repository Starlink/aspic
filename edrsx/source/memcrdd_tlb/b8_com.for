*      module B8_COM
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Declares common block /B8_COM/ holding data written by
*	module MEMCB8.
*
*SOURCE
*       B8_COM.INC in MEMCRDD.TLB
*
*COMMON USAGE
*	/B8_COM/,
*		B8_nfa	Factor used to scale background sigmas before
*			running MEMSYS3
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 25/4/90
*-------------------------------------------------------------------
*
* DECLARE PARAMETER HOLDING SIZE OF COMMON BLOCK (IN UNITS OF 4 BYTES)
* AND ALSO AN INTEGER WHICH WILL OVERLAY THE START OF THE COMMON BLOCK
*
      integer	B8_cm,B8_sz
      parameter (B8_sz=1)

*
* DECLARE CONTENTS OF COMMON BLOCK /B8_COM/
*
      real	B8_nfa	! Factor used to scale background sigmas before
			! running MEMSYS3

*
* DECLARE THE COMMON BLOCK
*
      common /B8_COM/      B8_nfa
      equivalence	(B8_nfa,B8_cm)
