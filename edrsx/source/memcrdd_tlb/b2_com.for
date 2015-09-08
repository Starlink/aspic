*      module B2_COM
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Declares common block /B2_COM/ holding data written by
*	module MEMCB2.
*
*SOURCE
*       B2_COM.INC in MEMCRDD.TLB
*
*COMMON USAGE
*	/B2_COM/,
*		B2_bnd	IRAS band no. (1-4)
*		B2_ncf	No. of CRDD files given as input
*		B2_frm  Descriptor frame identifiers
*		B2_nam	Names of BDF files containing data
*		B2_pin	Pointers to start of files
*		B2_bze	Zero offsets giving data values in Jy
*		B2_bsc	scale factors giving data values in Jy
*		B2_bpx	Flag for blank data values (invalid)
*		B2_nde	No. of detector data streams in each file
*		B2_nys  No. of data samples per detector
*		B2_spd  Scan speeds in arcmins per second
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 29/9/89
*-------------------------------------------------------------------
*
* DECLARE PARAMETER HOLDING SIZE OF COMMON BLOCK (IN UNITS OF 4 BYTES)
* AND ALSO AN INTEGER WHICH WILL OVERLAY THE START OF THE COMMON BLOCK
*
      integer	B2_cm,B2_sz
      parameter (B2_sz=2+18*PR_crd)

*
* DECLARE CONTENTS OF COMMON BLOCK /B2_COM/
*
      integer	B2_bnd	        ! IRAS band no. (1-4)
      integer	B2_ncf	        ! No. of CRDD files given as input
      integer	B2_frm(PR_crd)  ! Descriptor frame identifiers
      character	B2_nam(PR_crd)*40!Names of BDF files containing data
      integer	B2_pin(PR_crd)	! Pointers to start of files
      real	B2_bze(PR_crd)	! Zero offsets giving data values in Jy
      real	B2_bsc(PR_crd)	! scale factors giving data values in Jy
      integer	B2_bpx(PR_crd)	! Flag for blank data values (invalid)
      integer	B2_nde(PR_crd)	! No. of detector data streams in each file
      integer	B2_nys(PR_crd)  ! No. of data samples per detector
      real	B2_spd(PR_crd)	! Scan speeds in arcmins per second

*
* DECLARE THE COMMON BLOCK
*
      common /B2_COM/      B2_bnd,B2_ncf,B2_frm,B2_nam,B2_pin,B2_bze,
     :                     B2_bsc,B2_bpx,B2_nde,B2_nys,B2_spd
      equivalence	(B2_bnd,B2_cm)
