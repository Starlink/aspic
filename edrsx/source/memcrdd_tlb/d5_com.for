*      module D5_COM
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Declares common block /D5_COM/ holding data written by
*	module MEMCD5.
*
*SOURCE
*       D5_COM.INC in MEMCRDD.TLB
*
*COMMON USAGE
*	/D5_COM/,
*		D5_gap	  No. of historic iterations before the next 
*			  classic iteration
*		D5_nxt	  Iteration at which next classic call will be
*			  made
*		D5_oml	  Rescaled termination criterion after previous
*			  classic call 
*		D5_run    MEMSYS3 argument "MEMRUN". Indicates what MEM3
*			  will do when called.
*		D5_sta    Indicates what stage the convergence process
*			  has reached.
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 15/11/89
*-------------------------------------------------------------------
*
* DECLARE PARAMETER HOLDING SIZE OF COMMON BLOCK (IN UNITS OF 4 BYTES)
* AND ALSO AN INTEGER WHICH WILL OVERLAY THE START OF THE COMMON BLOCK
*
      integer	D5_cm,D5_sz
      parameter (D5_sz=7)

*
* DECLARE CONTENTS OF COMMON BLOCK /D5_COM/
*
      integer	D5_gap	  ! No. of historic iterations before the next 
			  ! classic iteration
      integer	D5_nxt	  ! Iteration at which next classic call will be
	  		  ! made
      real	D5_oml	  ! Rescaled termination criterion after 
			  ! previous classic call 
      integer	D5_run    ! MEMSYS3 argument "MEMRUN". Indicates what 
			  ! MEM3 will do when called.
      character D5_sta*12 ! Indicates what stage the convergence process
		  	  ! has reached.

*
* DECLARE THE COMMON BLOCK
*
      common /D5_COM/      D5_gap,D5_nxt,D5_oml,D5_run,D5_sta
      equivalence (D5_gap,D5_cm)
