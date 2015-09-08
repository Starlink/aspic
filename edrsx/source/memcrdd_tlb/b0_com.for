*      module B0_COM
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Declares common block /B0_COM/ holding data written by
*	module MEMCB0.
*
*SOURCE
*       A6_COM.INC in MEMCRDD.TLB
*
*COMMON USAGE
*	/B0_COM/,
*		B0_fit - FITS descriptors of output image
*		B0_nps - No. of pixels per line in output image
*		B0_nls - No. of lines in output image
*		B0_mar - Width of the blank margin in arcmins
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 28/9/89
*-------------------------------------------------------------------
*
* DECLARE PARAMETER HOLDING SIZE OF COMMON BLOCK (IN UNITS OF 4 BYTES)
* AND ALSO AN INTEGER WHICH WILL OVERLAY THE START OF THE COMMON BLOCK
*
      integer	B0_cm,B0_sz
      parameter (B0_sz=10)

*
* DECLARE CONTENTS OF COMMON BLOCK /B0_COM/
*
      real	B0_fit(7)! FITS descriptors of output image
      integer	B0_nps	! No. of pixels per line in output image
      integer   B0_nls	! No. of lines in output image
      real	B0_mar	! Width of the blank margin in arcmins

*
* DECLARE THE COMMON BLOCK
*
      common /B0_COM/      B0_fit,B0_nps,B0_nls,B0_mar
      equivalence	(B0_fit,B0_cm)
