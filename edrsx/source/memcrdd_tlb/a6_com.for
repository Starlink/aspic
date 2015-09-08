*      module A6_COM
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Declares common block /A6_COM/ holding data written by
*	module MEMCA6.
*
*SOURCE
*       A6_COM.INC in MEMCRDD.TLB
*
*COMMON USAGE
*	/A6_COM/,
*		A6_bnd - IRAS band no. of data
*		A6_frm - Descriptor frame identifiers
*		A6_nam - The names of the BDF disk files
*		A6_ncf - No. of CRDD files given as input
*		A6_pin - Pointers to the start of each CRDD file
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 27/9/89
*-------------------------------------------------------------------
*
* DECLARE PARAMETER HOLDING SIZE OF COMMON BLOCK (IN UNITS OF 4 BYTES)
* AND ALSO AN INTEGER WHICH WILL OVERLAY THE START OF THE COMMON BLOCK
*
      integer	A6_cm,A6_sz
      parameter (A6_sz=2+12*PR_crd)

*
* DECLARE CONTENTS OF COMMON BLOCK /A6_COM/
*
      integer	A6_bnd		! IRAS band no. of data
      integer   A6_frm(PR_crd)	! Descriptor frame identifiers
      character A6_nam(PR_crd)*40! The names of the BDF disk files
      integer	A6_ncf 		! No. of CRDD files given as input
      integer   A6_pin(PR_crd)  ! Pointers to start of each CRDD file

*
* DECLARE THE COMMON BLOCK
*
      common /A6_COM/      A6_bnd,A6_frm,A6_nam,A6_ncf,A6_pin
      equivalence (A6_cm,A6_bnd)
