*      module D3_COM
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Declares common block /D3_COM/ holding data written by
*	module MEMCD3.
*
*SOURCE
*       D3_COM.INC in MEMCRDD.TLB
*
*COMMON USAGE
*	/D3_COM/,
*		D3_off		Surface brightness offset added to data
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 10/10/89
*-------------------------------------------------------------------
*
* DECLARE PARAMETER HOLDING SIZE OF COMMON BLOCK (IN UNITS OF 4 BYTES)
* AND ALSO AN INTEGER WHICH WILL OVERLAY THE START OF THE COMMON BLOCK
*
      integer	D3_cm,D3_sz
      parameter (D3_sz=1)

*
* DECLARE CONTENTS OF COMMON BLOCK /D3_COM/
*
      real	D3_off	! Surface brightness offset added to data

*
* DECLARE THE COMMON BLOCK
*
      common /D3_COM/      D3_off
      equivalence (D3_off,D3_cm)
