*      module B6_COM
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Declares common block /B6_COM/ holding data written by
*	module MEMCB6.
*
*SOURCE
*       B6_COM.INC in MEMCRDD.TLB
*
*COMMON USAGE
*	/B6_COM/,
*		B6_wk1		Pointer to start of work image 1
*		B6_wk2		Pointer to start of work image 2
*		B6_wk3		Pointer to start of work image 3
*		B6_wk4		Pointer to start of work image 4
*		B6_psf		Pointer to start of PSF FFTs
*		B6_sdc		Pointer to data set used to store
*				SDC identifiers
*		B6_x		Pointer to data set used to store
*				pixel no. of sample centre
*		B6_y		Pointer to data set used to store
*				line no. of sample centre
*		B6_sol		Pointer to start of solid angle data
*		B6_bac		Pointer to start of background image
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 4/10/89
*-------------------------------------------------------------------
*
* DECLARE PARAMETER HOLDING SIZE OF COMMON BLOCK (IN UNITS OF 4 BYTES)
* AND ALSO AN INTEGER WHICH WILL OVERLAY THE START OF THE COMMON BLOCK
*
      integer	B6_cm,B6_sz
      parameter (B6_sz=9+PR_grp)

*
* DECLARE CONTENTS OF COMMON BLOCK /B6_COM/
*
      integer	B6_wk1		! Pointer to start of work image 1
      integer	B6_wk2		! Pointer to start of work image 2
      integer	B6_wk3		! Pointer to start of work image 3
      integer	B6_wk4		! Pointer to start of work image 4
      integer	B6_psf(PR_grp)	! Pointer to start of PSF FFTs
      integer	B6_sdc		! Pointer to data set used to store
				! SDC identifiers
      integer	B6_x		! Pointer to data set used to store
				! pixel no. of sample centre
      integer	B6_y		! Pointer to data set used to store
				! line no. of sample centre
      integer	B6_sol		! Pointer to start of solid angle data
      integer	B6_bac		! Pointer to start of coverage image

*
* DECLARE THE COMMON BLOCK
*
      common /B6_COM/      B6_wk1,B6_wk2,B6_wk3,B6_wk4,B6_psf,B6_sdc,
     :                     B6_x,B6_y,B6_sol,B6_bac
      equivalence	(B6_wk1,B6_cm)
