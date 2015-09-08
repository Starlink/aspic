*      module B5_COM
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Declares common block /B5_COM/ holding data written by
*	module MEMCB5.
*
*SOURCE
*       B5_COM.INC in MEMCRDD.TLB
*
*COMMON USAGE
*	/B5_COM/,
*		B5_var		Variance of field noise
*		B5_min		Minimum surface brightness of usable samples
*		B5_max		Maximum surface brightness of usable samples
*		B5_ave		Mean surface brightness of usable samples
*		B5_cfa		cfact. Used for unpacking sdc identifiers
*		B5_fgs		Location of first sample in each group 
*				relative to the start of any data set
*		B5_lgs		Location of last sample in each group 
*				relative to the start of any data set
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 20/10/89
*-------------------------------------------------------------------
*
* DECLARE PARAMETER HOLDING SIZE OF COMMON BLOCK (IN UNITS OF 4 BYTES)
* AND ALSO AN INTEGER WHICH WILL OVERLAY THE START OF THE COMMON BLOCK
*
      integer	B5_cm,B5_sz
      parameter (B5_sz=4+2*PR_grp+IR_dts*PR_crd)

*
* DECLARE CONTENTS OF COMMON BLOCK /B5_COM/
*
      real	B5_var(IR_dts,PR_crd) ! Variance of field noise
      real	B5_min	! Minimum surface brightness of usable samples
      real	B5_max	! Maximum surface brightness of usable samples
      real	B5_ave	! Mean surface brightness of usable samples
      integer	B5_cfa	! Used for unpacking SDC identifiers
      integer	B5_fgs(PR_grp) ! Location of first sample in each group 
			! relative to the start of any data set
      integer	B5_lgs(PR_grp) ! Location of last sample in each group 
			! relative to the start of any data set

*
* DECLARE THE COMMON BLOCK
*
      common /B5_COM/      B5_var,B5_min,B5_max,B5_ave,B5_cfa,B5_fgs,
     :                     B5_lgs
      equivalence	(B5_var,B5_cm)
