*      module A9_COM
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Declares common block /A9_COM/ holding data written by
*	module MEMCA9.
*
*SOURCE
*       A9_COM.INC in MEMCRDD.TLB
*
*COMMON USAGE
*	/A9_COM/,
*		A9_nps	Pixels per line in the inpuyt image
*		A9_nls	Lines in the inpuyt image
*		A9_bsc	Scale factor from input image
*		A9_bze	Zero offset from input image
*		A9_inv	Flag for invalid values in input image
*		A9_pin  Pointer to input image
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 6/11/89
*-------------------------------------------------------------------
*
* DECLARE PARAMETER HOLDING SIZE OF COMMON BLOCK (IN UNITS OF 4 BYTES)
* AND ALSO AN INTEGER WHICH WILL OVERLAY THE START OF THE COMMON BLOCK
*
      integer	A9_cm,A9_sz
      parameter (A9_sz=6)

*
* DECLARE CONTENTS OF COMMON BLOCK /A9_COM/
*
      real	A9_bsc	! Scale factor from input image
      real	A9_bze	! Zero offset from input image
      integer	A9_inv	! Flag for invalid values in input image
      integer	A9_nls	! No. of lines in input image
      integer	A9_nps	! No. of pixels per line in input image
      integer	A9_pin  ! Pointer to input image

*
* DECLARE THE COMMON BLOCK
*
      common /A9_COM/      A9_bsc,A9_bze,A9_inv,A9_pin,A9_nls,A9_nps
      equivalence (A9_bsc,A9_cm)
