*      module ZZ_COM
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Declares common block /ZZ_COM/ holding data written by
*	module MEMCZZ.
*
*SOURCE
*       A6_COM.INC in MEMCRDD.TLB
*
*COMMON USAGE
*	/ZZ_COM/,
*		ZZ_aim - Target value of omega for MaxEnt procedure
*		ZZ_cfl - FWHM of CRDD filter (in samples)
*		ZZ_cro - Value for CROTA descriptor (orientation of image)
*		ZZ_deg - Degliching strength required (0.0 - 1.0)
*		ZZ_fld - Noise level in background regions given by user
*		ZZ_ilv - Amount of non-MEMSYS3 information to display
*		ZZ_ins - In scan pointing error in arcmins
*		ZZ_k   - Reciprocal of signal to noise ratio
*		ZZ_lev - Amount of MEMSYS3 diagnostics to display
*		ZZ_mth - MEMSYS3 method; COMBINATION, CLASSIC or HISTORIC
*		ZZ_nit - Max. no. of MEM iterations to perform
*		ZZ_ntr - Max. no.of trials to perform when producing
*                        noise estimates due to  pointing
*		ZZ_psf - Fractional error in the used PSFs
*		ZZ_psz - Pixel size given by user
*		ZZ_rat - Convergence rate limit
*		ZZ_stf - Name of stop file
*		ZZ_tol - Tolerance required of MEMSYS3 calculations
*		ZZ_typ - Data type 'AO' or 'SURVEY'
*		ZZ_xs  - Cross scan pointing error in arcmins
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 27/9/89
*-------------------------------------------------------------------
*
* DECLARE PARAMETER HOLDING SIZE OF COMMON BLOCK (IN UNITS OF 4 BYTES)
* AND ALSO AN INTEGER WHICH WILL OVERLAY THE START OF THE COMMON BLOCK
*
      integer	ZZ_cm,ZZ_sz
      parameter (ZZ_sz=34)

*
* DECLARE CONTENTS OF COMMON BLOCK /ZZ_COM/
*
      real	ZZ_aim	! Target value of omega for MaxEnt procedure
      real	ZZ_cfl 	! FWHM of CRDD filter (in samples)
      real	ZZ_cro 	! Value for CROTA descriptor (orientation of image)
      real	ZZ_def	! Value of flat default model given by user
      real	ZZ_deg 	! Degliching strength required (0.0 - 1.0)
      real	ZZ_fld	! Noise level in background regions given by user
      integer	ZZ_ilv	! Amount of non-MEMSYS3 information to display
      real	ZZ_ins 	! In scan pointing error in arcmins
      real	ZZ_k  	! Reciprocal of signal to noise ratio
      integer	ZZ_lev	! Amount of MEMSYS3 diagnostics to display
      character ZZ_mod*10! Form of default model
      character	ZZ_mth*11! MEMSYS3 method 
      integer	ZZ_nit	! Max. no. of MEM iterations to perform
      integer	ZZ_ntr	! Max. no.of trials to perform when producing
			! noise estimates due to  pointing
      real	ZZ_psf 	! Fractional error in the used PSFs
      real	ZZ_psz	! Pixel size given by user
      real	ZZ_rat	! Convergence rate limit
      character	ZZ_stf*41! Name of stop file
      real	ZZ_tol	! Tolerance required of MEMSYS3 calculations
      character	ZZ_typ*6! Data type 'AO' or 'SURVEY'
      real	ZZ_xs  	! Cross scan pointing error in arcmins

*
* DECLARE THE COMMON BLOCK
*
      common /ZZ_COM/      ZZ_aim,ZZ_def,ZZ_fld,ZZ_ilv,ZZ_k,ZZ_lev,
     :                     ZZ_mod,ZZ_mth,ZZ_nit,ZZ_psz,ZZ_rat,ZZ_tol,
     :                     ZZ_typ,ZZ_stf,ZZ_deg,ZZ_ntr,ZZ_xs,ZZ_ins,
     :                     ZZ_psf,ZZ_cro,ZZ_cfl
      equivalence	(ZZ_aim,ZZ_cm)
