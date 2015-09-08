*      module A7_COM
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Declares common block /A7_COM/ holding data written by
*	module MEMCA7.
*
*SOURCE
*       A6_COM.INC in MEMCRDD.TLB
*
*COMMON USAGE
*	/A7_COM/,
*		A7_psf		Pointers to the individual PSFs
*		A7_nps		Total no. of pixels in a PSF line
*		A7_nls          Total no. of lines in a PSF
*		A7_nds		No. of detector PSFs in the stack
*		A7_npx		No. of used pixels in a PSF line
*		A7_nln		No. of used lines in a PSF
*		A7_inv		Flag for invalid PSF pixels
*		A7_sca		Scale factor for PSFs
*		A7_zer		Zero offset for PSFs
*		A7_tr		PSF pixel to focal plane transformation
*               A7_mar		Width of image margin in arcmins
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 28/9/89
*-------------------------------------------------------------------
*
* DECLARE PARAMETER HOLDING SIZE OF COMMON BLOCK (IN UNITS OF 4 BYTES)
* AND ALSO AN INTEGER WHICH WILL OVERLAY THE START OF THE COMMON BLOCK
*
      integer	A7_cm,A7_sz
      parameter (A7_sz=4+12*IR_dts)

*
* DECLARE CONTENTS OF COMMON BLOCK /A7_COM/
*
      integer	A7_psf(IR_dts)	! Pointers to the individual PSFs
      real	A7_mar		! Width of image margin in arcmins
      integer	A7_nps		! Total no. of pixels in a PSF line
      integer	A7_nls          ! Total no. of lines in a PSF
      integer	A7_nds		! No. of detector PSFs in the stack
      integer	A7_npx(IR_dts)	! No. of used pixels in a PSF line
      integer	A7_nln(IR_dts)	! No. of used lines in a PSF
      integer	A7_inv(IR_dts)	! Flag for invalid PSF pixels
      real	A7_sca(IR_dts)	! Scale factor for PSFs
      real	A7_zer(IR_dts)	! Zero offset for PSFs
      real	A7_tr(6,IR_dts)	! PSF pixel to focal plane transformation

*
* DECLARE THE COMMON BLOCK
*
      common /A7_COM/      A7_psf,A7_nps,A7_nls,A7_nds,A7_npx,A7_nln,
     :                     A7_inv,A7_sca,A7_zer,A7_tr,A7_mar
      equivalence (A7_psf,A7_cm)
