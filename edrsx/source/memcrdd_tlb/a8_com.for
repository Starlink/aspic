*      module A8_COM
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Declares common block /A8_COM/ holding data written by
*	module MEMCA8.
*
*SOURCE
*       A6_COM.INC in MEMCRDD.TLB
*
*COMMON USAGE
*	/A8_COM/,
*		A8_dgp	- Detector group number for each detector
*		A8_ndg	- Number of used detector groups
*		A8_nsg	- Number of used scan groups
*		A8_sgp	- Scan group number for each CRDD file
*		A8_ang	- Mean scan angle for each scan group in degrees
*			 (=clockwise angle from south to scan direction)
*		A8_ngp	- Total number of sample groups
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 28/9/89
*-------------------------------------------------------------------
*
* DECLARE PARAMETER HOLDING SIZE OF COMMON BLOCK (IN UNITS OF 4 BYTES)
* AND ALSO AN INTEGER WHICH WILL OVERLAY THE START OF THE COMMON BLOCK
*
      integer	A8_cm,A8_sz
      parameter (A8_sz=3+IR_dts+PR_crd+PR_grp)

*
* DECLARE CONTENTS OF COMMON BLOCK /A8_COM/
*
      integer	A8_dgp(IR_dts)	! Detector group number for each detector
      integer	A8_ndg		! Number of used detector groups
      integer	A8_nsg		! Number of used scan groups
      integer	A8_sgp(PR_crd)	! Scan group number for each CRDD file
      integer   A8_ang(PR_grp)	! Mean scan angle for each scan group 
				! in degrees (=clockwise angle from 
				! south to scan direction)
      integer	A8_ngp		! Total number os sample groups

*
* DECLARE THE COMMON BLOCK
*
      common /A8_COM/      A8_dgp,A8_ndg,A8_nsg,A8_sgp,A8_ang,A8_ngp
      equivalence	(A8_dgp,A8_cm)
