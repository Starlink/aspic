*      module A5_COM
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Declares common block /A5_COM/ holding data written by
*	module MEMCA5.
*
*SOURCE
*       A5_COM.INC in MEMCRDD.TLB
*
*COMMON USAGE
*	/A5_COM/,
*		A5_bnd - IRAS band no. of data
*		A5_frm - Descriptor frame identifiers
*		A5_nam - The names of the BDF disk files
*		A5_ncf - No. of CRDD files given as input
*		A5_pin - Pointers to start of each CRDD file
*		A5_spd - Scan speeds in arcmins per second
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 27/9/89
*-------------------------------------------------------------------
*
* DECLARE PARAMETER HOLDING SIZE OF COMMON BLOCK (IN UNITS OF 4 BYTES)
* AND ALSO AN INTEGER WHICH WILL OVERLAY THE START OF THE COMMON BLOCK
*
      integer	A5_cm,A5_sz
      parameter (A5_sz=2+13*PR_crd)

*
* DECLARE CONTENTS OF COMMON BLOCK /A5_COM/
*
      integer	A5_bnd		! IRAS band no. of data
      integer   A5_frm(PR_crd)	! Descriptor frame identifiers
      character A5_nam(PR_crd)*40! The names of the BDF disk files
      integer	A5_ncf 		! No. of CRDD files given as input
      integer	A5_pin(PR_crd)  ! Pointers to start of each CRDD file
      real	A5_spd(PR_crd)	! Scan speeds in arcmins per second

*
* DECLARE THE COMMON BLOCK
*
      common /A5_COM/      A5_bnd,A5_frm,A5_nam,A5_ncf,A5_pin,A5_spd
      equivalence (A5_cm,A5_bnd)
