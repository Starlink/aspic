*      module B1_COM
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Declares common block /B1_COM/ holding data written by
*	module MEMCB1.
*
*SOURCE
*       B1_COM.INC in MEMCRDD.TLB
*
*COMMON USAGE
*	/B1_COM/,
*		B1_it	Next iteration of MEM3 to perform
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 31/10/89
*-------------------------------------------------------------------
*
* DECLARE PARAMETER HOLDING SIZE OF COMMON BLOCK (IN UNITS OF 4 BYTES)
* AND ALSO AN INTEGER WHICH WILL OVERLAY THE START OF THE COMMON BLOCK
*
      integer	B1_cm,B1_sz
      parameter (B1_sz=1)

*
* DECLARE CONTENTS OF COMMON BLOCK /B1_COM/
*
      integer	B1_it	! Next iteration of MEM3 to perform

*
* DECLARE THE COMMON BLOCK
*
      common /B1_COM/  B1_it
      equivalence (B1_it,B1_cm)
